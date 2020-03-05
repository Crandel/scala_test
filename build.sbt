name := "Scala test Project"

version := "0.1.0"

scalaVersion := "2.13.0"

scalaVersion in ThisBuild := "2.13.0"

assemblyJarName in assembly := "scala_test.jar"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

resolvers += Resolver.bintrayIvyRepo("scalacenter", "sbt-releases")
