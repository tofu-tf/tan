ThisBuild / version := "0.1-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.18.3"
ThisBuild / libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.18.3"

ThisBuild / libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
ThisBuild / libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

ThisBuild / libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

lazy val core = project.in(file("core"))
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan"
  )

lazy val example = project.in(file("example"))
  .dependsOn(core)