scalaVersion := "2.10.0-M7"

libraryDependencies <+= scalaVersion {
  "org.scala-lang" % "scala-compiler" % _
}

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9-2.10.0-M7-B1" % "test" cross CrossVersion.full

scalacOptions ++= Seq("-deprecation", "-feature")
