scalaVersion := "2.10.0-M7"

libraryDependencies <+= scalaVersion {
  "org.scala-lang" % "scala-compiler" % _
}

scalacOptions ++= Seq("-deprecation", "-feature")
