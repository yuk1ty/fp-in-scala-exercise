lazy val scalaTestVersion = "3.0.4"
lazy val scalacticVersion = "3.0.4"

name := "fp-in-scala-exercise"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % scalacticVersion % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"