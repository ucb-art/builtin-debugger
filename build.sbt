name := "builtin-debugger"

organization := "edu.berkeley.cs"

version := "0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "chisel3" -> "3.1-SNAPSHOT",
  "chisel-iotesters" -> "1.2-SNAPSHOT"
)

libraryDependencies ++= (Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
})

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5"
)
