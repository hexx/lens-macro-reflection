import language.experimental.macros
import language.dynamics

import scala.reflect.ClassTag
import scala.reflect.api.Universe
import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

trait LenserTreeBuiler {
  val universe: Universe
  import universe._

  def mkParam(name: String, tpe: Type) =
    ValDef(Modifiers(Flag.PARAM), newTermName(name), TypeTree(tpe), EmptyTree)

  def mkTuple2(_1: Tree, _2: Tree) =
    Apply(Select(Select(Ident(newTermName("scala")), newTermName("Tuple2")), newTermName("apply")), List(_1, _2))

  def mkGetter(memberName: String, classType: Type) =
    Function(List(mkParam("a$", classType)), Select(Ident(newTermName("a$")), newTermName(memberName)))

  def mkSetter(memberName: String, classType: Type, memberType: Type) =
    Function(List(mkParam("a$", classType), mkParam("x$", memberType)),
             Apply(Select(Ident(newTermName("a$")), newTermName("copy")),
                   List(AssignOrNamedArg(Ident(newTermName(memberName)), Ident(newTermName("x$"))))))

  def mkLens(memberName: String, classType: Type, memberType: Type) =
    mkTuple2(mkGetter(memberName, classType), mkSetter(memberName, classType, memberType))

  def result: Tree
}

class LenserMacro[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro LenserMacro.selectDynamic[T]
  def applyDynamic(propName: String)() = macro LenserMacro.applyDynamic[T]
}

object LenserMacro {
  def lens[T] = new LenserMacro[T]

  def selectDynamic[T: c.AbsTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()

  def applyDynamic[T: c.AbsTypeTag](c: Context)(propName: c.Expr[String])() = {
    val tb = new LenserTreeBuiler {
      val universe = c.universe
      import universe._

      val classType = implicitly[AbsTypeTag[T]].tpe
      val Literal(Constant(memberName: String)) = propName.tree.asInstanceOf[Tree]
      val getterMember = classType.member(newTermName(memberName)) orElse {
        c.abort(c.enclosingPosition, "value " + memberName + " is not a member of " + classType)
      }
      val memberType = getterMember.typeSignatureIn(classType) match {
        case NullaryMethodType(memberType) => memberType
        case _                             => c.abort(c.enclosingPosition, "member %s is not a field".format(memberName))
      }
      val result = mkLens(memberName, classType, memberType)
    }
    c.Expr[Any](c.resetAllAttrs(tb.result.asInstanceOf[c.Tree]))
  }
}

object LenserReflection {
  def lens[T: ru.TypeTag: ClassTag] = new LenserReflection[T]
}

class LenserReflection[T: ru.TypeTag: ClassTag] extends Dynamic {
  def selectDynamic(propName: String)  = applyDynamic(propName)()

  def applyDynamic(propName: String)() = {
    val tb = new LenserTreeBuiler {
      val universe = scala.reflect.runtime.universe
      import universe._

      val classType = implicitly[ru.TypeTag[T]].tpe.asInstanceOf[Type]
      val getterMember = classType.member(newTermName(propName))
      val NullaryMethodType(memberType) = getterMember.typeSignatureIn(classType)
      val result = mkLens(propName, classType, memberType)
    }
    val toolBox = ru.runtimeMirror(implicitly[ClassTag[T]].runtimeClass.getClassLoader).mkToolBox()
    toolBox.runExpr(tb.result.asInstanceOf[ru.Tree])
  }
}
