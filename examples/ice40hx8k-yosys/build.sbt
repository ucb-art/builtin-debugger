name := "builtin-debugger-example-ice40hx8k"

version := "0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-deprecation", "-feature")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.1-SNAPSHOT"

lazy val chiselDebuggers = RootProject(file("../../"))
lazy val chiselJtag = RootProject(file("../deps/chisel-jtag"))
lazy val externalUtils = RootProject(file("../deps/chisel-jtag/external"))

lazy val main = (project in file(".")).
  dependsOn(chiselDebuggers).
  dependsOn(chiselJtag).
  dependsOn(externalUtils)