lazy val root = project.enablePlugins(ScalaJSPlugin)
  .in(file("."))
  .settings(
    name := "scala3-example-project",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC1",
    scalaJSUseMainModuleInitializer := true,
    useScala3doc := true,
  )
