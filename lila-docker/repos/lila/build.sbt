import com.typesafe.sbt.packager.Keys.{ bashScriptExtraDefines, scriptClasspath }
import play.sbt.PlayCommands
import play.sbt.PlayInternalKeys.playDependencyClasspath
import play.sbt.routes.RoutesKeys

import BuildSettings.*
import Dependencies.*

lazy val root = Project("lila", file("."))
  .enablePlugins(JavaServerAppPackaging, RoutesCompiler)
  .dependsOn(api)
  .aggregate(api)
  .settings(buildSettings)

organization := "org.lichess"
Compile / run / fork := true
javaOptions ++= Seq("-Xms1g", "-Xmx4g", "-Dlogger.file=conf/logger.dev.xml")
javaOptions ++= {
  if (sys.props("java.specification.version").toInt > 21)
    Seq("--enable-native-access=ALL-UNNAMED")
  else
    Seq.empty
}
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")
ThisBuild / usePipelining := false
// Docker Desktop on Windows (bind mount) can break zinc's transactional classfile manager
// with `NoSuchFileException: .../classes.bak/...`, cascading into many compile errors.
ThisBuild / incOptions := incOptions.value.withClassfileManagerType(xsbti.compile.DeleteImmediatelyManagerType.of())
// shorter prod classpath
scriptClasspath := Seq("*")
Compile / resourceDirectory := baseDirectory.value / "conf"
// the following settings come from the PlayScala plugin, which I removed
shellPrompt := PlayCommands.playPrompt
// all dependencies from outside the project (all dependency jars)
playDependencyClasspath := (Runtime / externalDependencyClasspath).value
ivyLoggingLevel := UpdateLogging.DownloadOnly
Compile / mainClass := Some("lila.app.Lila")
// Adds the Play application directory to the command line args passed to Play
bashScriptExtraDefines += "addJava \"-Duser.dir=$(realpath \"$(cd \"${app_home}/..\"; pwd -P)\"  $(is_cygwin && echo \"fix\"))\"\n"
Compile / RoutesKeys.routes / sources ++= {
  val dirs = (Compile / unmanagedResourceDirectories).value
  (dirs * "routes").get ++ (dirs * "*.routes").get
}
Compile / RoutesKeys.generateReverseRouter := false
Compile / RoutesKeys.generateForwardRouter := true
Compile / sourceDirectory := baseDirectory.value / "app"
Compile / scalaSource := baseDirectory.value / "app"
Universal / sourceDirectory := baseDirectory.value / "dist"

// format: off
libraryDependencies ++= akka.bundle ++ playWs.bundle ++ macwire.bundle ++ scalalib.bundle ++ chess.bundle ++ Seq(
  play.json, play.logback, compression, hasher,
  reactivemongo.driver, maxmind, scalatags,
  kamon.core, kamon.metrics,
  scaffeine, caffeine, uaparser, nettyTransport, reactivemongo.shaded, catsMtl
) ++ tests.bundle

lazy val modules = Seq(
  core,
  ui, common, tree,
  db,
  memo,
  user, pref, mailer,
  analyse,
  study,
  beta,
  security,
  web
)

lazy val moduleRefs = modules map projectToRef
lazy val moduleCPDeps = moduleRefs map { sbt.ClasspathDependency(_, None) }

// ============================================================
// Infrastructure Modules
// ============================================================

lazy val core = module("core",
  Seq(),
  Seq(catsMtl, scalatags, galimatias) ++ scalalib.bundle ++ reactivemongo.bundle ++ tests.bundle
)

lazy val common = module("common",
  Seq(core),
  Seq(kamon.core, scaffeine, apacheText, chess.playJson) ++ flexmark.bundle
)

lazy val db = module("db",
  Seq(common),
  Seq(hasher) ++ macwire.bundle
)

lazy val memo = module("memo",
  Seq(db),
  Seq(scaffeine) ++ playWs.bundle
)




lazy val tree = module("tree",
  Seq(core),
  Seq(chess.playJson)
)

lazy val ui = module("ui",
  Seq(core),
  Seq()
).enablePlugins(RoutesCompiler).settings(
  Compile / RoutesKeys.generateForwardRouter := false,
  Compile / RoutesKeys.routes / sources ++= {
    val dirs = baseDirectory.value / ".." / ".." / "conf"
    (dirs * "routes").get ++ (dirs * "*.routes").get
  }
)

// ============================================================
// User & Security Modules
// ============================================================

lazy val user = module("user",
  Seq(db, memo),
  Seq(hasher) ++ tests.bundle ++ playWs.bundle
)

lazy val pref = module("pref",
  Seq(memo, ui),
  Seq()
)

lazy val mailer = module("mailer",
  Seq(memo),
  Seq(hasher, play.mailer)
)

lazy val security = module("security",
  Seq(user, mailer, pref),
  Seq(maxmind, hasher, uaparser) ++ tests.bundle
)

// ============================================================
// Analysis Core Modules
// ============================================================

lazy val analyse = module("analyse",
  Seq(tree, memo, ui),
  tests.bundle
)

lazy val study = module("study",
  Seq(tree, memo, ui),
  tests.bundle ++ Seq(scalacheck, munitCheck, chess.testKit)
).dependsOn(common % "test->test")

lazy val beta = module("beta",
  Seq(db, user),
  tests.bundle
)

// ============================================================
// Web Module
// ============================================================

lazy val web = module("web",
  Seq(ui, memo),
  playWs.bundle ++ tests.bundle ++ Seq(
    play.logback, play.server, play.netty,
  )
)

// ============================================================
// API Module (aggregates all modules)
// ============================================================

lazy val api = module("api",
  moduleCPDeps,
  Seq(play.api, play.json, hasher, kamon.core) ++ tests.bundle ++ flexmark.bundle
).settings(
  Runtime / aggregate := false,
  Test / aggregate := true
) aggregate (moduleRefs: _*)
