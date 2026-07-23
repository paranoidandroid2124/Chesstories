ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "io.chesstory"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / javacOptions ++= Seq("--release", "21")
ThisBuild / scalacOptions ++= Seq(
  "-indent",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-release:21",
  "-Wimplausible-patterns"
)

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  "com.github.lichess-org.scalachess" %% "scalachess" % "17.14.2",
  "org.playframework" %% "play-json" % "3.0.6",
  "org.scalameta" %% "munit" % "1.2.1" % Test
)

Test / parallelExecution := false

Compile / mainClass := Some("io.chesstory.runtime.ChesstoryRuntime")
