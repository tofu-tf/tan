ThisBuild / version := "0.1-SNAPSHOT"

val scala2Version = "2.13.7"
val scala3Version = "3.1.0"

// idea hax
val doCross = false

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
    scalaVersion := scala3Version,
    crossScalaVersions := { if (doCross) Seq(scala3Version, scala2Version) else Seq(scala2Version) },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value
        )
        case _ => Seq.empty
      }
    }
  )

lazy val interop18 = project.in(file("interop-18"))
  .settings(tapir18)
  .dependsOn(core)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-tapir18",
    scalaVersion := scala3Version,
    crossScalaVersions := { if (doCross) Seq(scala3Version, scala2Version) else Seq(scala2Version) },
    scalacOptions += "-Ytasty-reader"
  )

lazy val interop19 = project.in(file("interop-19"))
  .dependsOn(core)
  .settings(tapir19)
  .settings(
    idePackagePrefix := Some("tan"),
    name := "tan-tapir19",
    crossScalaVersions := { if (doCross) Seq(scala3Version, scala2Version) else Seq(scala2Version) },
    scalacOptions += "-Ytasty-reader"
  )

lazy val example18 = project.in(file("example-18"))
  .dependsOn(interop18)
  .settings(
    scalaVersion := scala2Version,
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.18.3",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % "0.18.3",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % "0.18.3",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.18.3",
    libraryDependencies += "org.http4s" %% "http4s-ember-server" % "0.22.7",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.32",
    scalacOptions += "-Ycheck:all",
  )

lazy val example19 = project.in(file("example-19"))
  .dependsOn(interop19)
  .settings(
    scalaVersion := scala2Version,
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.19.1",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % "0.19.1",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % "0.19.1",
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.19.1",
    libraryDependencies += "org.http4s" %% "http4s-ember-server" % "0.23.6",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.32",
    scalacOptions += "-Ycheck:all",
  )