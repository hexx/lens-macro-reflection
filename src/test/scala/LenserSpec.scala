import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

case class Foo(a: Int)

class StackSpec extends FlatSpec with ShouldMatchers {
  val foo = Foo(3)

  "LenserOriginal" should "make a pair of setter/getter methods" in {
    val (g, s) = Macrocosm.lens[Foo].a
    g(foo) should equal (3)
    s(foo, 5) should equal (Foo(5))
  }

  "LenserMacro" should "make a pair of setter/getter methods" in {
    val (g, s) = LenserMacro.lens[Foo].a
    g(foo) should equal (3)
    s(foo, 5) should equal (Foo(5))
  }

  "LenserReflection" should "make a pair of setter/getter methods" in {
    val (g, s) = LenserReflection.lens[Foo].a.asInstanceOf[(Foo => Int, (Foo, Int) => Foo)]
    g(foo) should equal (3)
    s(foo, 5) should equal (Foo(5))
  }
}
