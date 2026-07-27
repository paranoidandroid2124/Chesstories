ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "io.chesstory.evaluation"
ThisBuild / version := "0.2.0"

ThisBuild / javacOptions ++= Seq("--release", "21")
ThisBuild / scalacOptions ++= Seq(
  "-indent",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-release:21"
)

lazy val chesstoryRuntime = RootProject(file("../../chesstory-runtime"))

lazy val root = (project in file("."))
  .dependsOn(chesstoryRuntime)
  .settings(
    name := "chesstory-judgment-runtime-adapter",
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.6",
    Compile / mainClass := Some("io.chesstory.evaluation.runtimeadapter.RuntimeAdapterCli"),
    Compile / run / fork := true,
    Compile / run / connectInput := true,
    publish / skip := true
  )
