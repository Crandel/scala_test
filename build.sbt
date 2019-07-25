name := "Scala test Project"

version := "0.01"

scalaVersion := "2.13.0"

scalaVersion in ThisBuild := "2.13.0"

assemblyJarName in assembly := "scala_test.jar"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

resolvers += Resolver.bintrayIvyRepo("scalacenter", "sbt-releases")
