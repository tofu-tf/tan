name := "tan"

version := "0.1"

scalaVersion := "2.13.7"

idePackagePrefix := Some("tan")

libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.18.3"
libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.18.3"

libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value