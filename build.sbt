ThisBuild / version := "0.1-SNAPSHOT"

val tapir18 = List(
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.18.3",
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.18.3"
)

val tapir19 = List(
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.19.1",
  libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.19.1"
)

lazy val core = project.in(file("core"))
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan",
    scalaVersion := "3.1.0",
    crossScalaVersions := Seq("3.1.0", "2.13.7")
  )

lazy val core2 = project.in(file("core-2"))
  .dependsOn(core)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-core2",
    scalaVersion := "2.13.7",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.7",
    scalacOptions += "-Ytasty-reader"
  )

lazy val core3 = project.in(file("core-3"))
  .dependsOn(core)
  // .settings(tapir18)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-core3",
    scalaVersion := "3.1.0"
  )

lazy val interop18_2 = project.in(file("interop-18-2"))
  .settings(tapir18)
  .dependsOn(core2)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-tapir18-2",
    scalaVersion := "2.13.7",
    scalacOptions += "-Ytasty-reader"
  )

lazy val interop18_3 = project.in(file("interop-18-3"))
  .dependsOn(core3)
  .settings(tapir18)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-tapir18-3",
    scalaVersion := "3.1.0"
  )

lazy val interop19 = project.in(file("interop-19"))
  .settings(tapir19)
  .dependsOn(core2)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-tapir19",
    scalaVersion := "3.1.0",
    scalacOptions += "-Ytasty-reader"
  )

lazy val example18 = project.in(file("example-18"))
  .dependsOn(interop18_3)
  .settings(
    scalaVersion := "3.1.0",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.18.3",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % "0.18.3",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % "0.18.3",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.18.3",
    libraryDependencies += "org.http4s" %% "http4s-ember-server" % "0.22.7"
  )

// lazy val example19 = project.in(file("example-19"))
//  .dependsOn(interop19)