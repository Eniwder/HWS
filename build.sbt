name := "HWS"

version := "0.1.1"

scalaVersion := "2.10.4"

mainClass in (Compile,run) := Some("main.scala.HWS")

libraryDependencies += scalaVersion { "org.scala-lang" % "scala-swing" % _ }.value