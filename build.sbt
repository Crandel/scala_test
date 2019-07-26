name := "Scala test Project"

version := "0.1.0"

scalaVersion := "2.12.8"

scalaVersion in ThisBuild := "2.12.8"

assemblyJarName in assembly := "scala_test.jar"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

resolvers += Resolver.bintrayIvyRepo("scalacenter", "sbt-releases")
