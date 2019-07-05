name := "Scala test Project"

version := "0.01"

scalaVersion := "2.13.0"

scalaVersion in ThisBuild := "2.13.0"

assemblyJarName in assembly := "scala_test.jar"
// libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.16.0"

resolvers += Resolver.bintrayIvyRepo("scalacenter", "sbt-releases")
