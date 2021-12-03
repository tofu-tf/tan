ThisBuild / version := "0.1-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

val tapir18 = List(
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.18.3",
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.18.3"
)

val tapir19 = List(
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.19.1",
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.19.1"
)

ThisBuild / libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
ThisBuild / libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

ThisBuild / libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

lazy val core = project.in(file("core"))
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan"
  )

lazy val interop18 = project.in(file("interop-18"))
  .settings(tapir18)
  .dependsOn(core)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan",
    name := "tan-tapir18"
  )

lazy val interop19 = project.in(file("interop-19"))
  .settings(tapir19)
  .dependsOn(core)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-tapir19"
  )

lazy val example18 = project.in(file("example-18"))
  .dependsOn(interop18)

lazy val example19 = project.in(file("example-19"))
  .dependsOn(interop19)