package lila.app

import akka.actor.ActorSystem
import com.softwaremill.macwire.*
import play.api.inject.DefaultApplicationLifecycle
import play.api.http.{ FileMimeTypes, HttpRequestHandler }
import play.api.inject.ApplicationLifecycle
import play.api.libs.crypto.DefaultCookieSigner
import play.api.libs.ws.StandaloneWSClient
import play.api.mvc.*
import play.api.mvc.request.*
import play.api.routing.Router
import play.api.{ BuiltInComponents, Configuration, Environment }

// The program entry point.
// To run with bloop:
// /path/to/bloop run lila -m lila.app.Lila -c /path/to/lila/.bloop
object Lila:

  def main(args: Array[String]): Unit =
    lila.web.PlayServer.start(args): env =>
      LilaComponents(
        env,
        DefaultApplicationLifecycle(),
        Configuration.load(env)
      ).application

final class LilaComponents(
    val environment: Environment,
    val applicationLifecycle: ApplicationLifecycle,
    val configuration: Configuration
) extends BuiltInComponents:

  val controllerComponents: ControllerComponents = DefaultControllerComponents(
    defaultActionBuilder,
    playBodyParsers,
    fileMimeTypes,
    executionContext
  )

  given executor: Executor = scala.concurrent.ExecutionContextOpportunistic

  lila
    .log("boot")
    .info:
      val appVersionCommit = ~configuration.getOptional[String]("app.version.commit")
      val appVersionDate = ~configuration.getOptional[String]("app.version.date")
      s"lila version: $appVersionCommit $appVersionDate"

  import _root_.controllers.*

  // we want to use the legacy session cookie baker for existing sessions
  lazy val cookieBaker = LegacySessionCookieBaker(httpConfiguration.session, cookieSigner)

  override lazy val requestFactory: RequestFactory =
    val cookieSigner = DefaultCookieSigner(httpConfiguration.secret)
    DefaultRequestFactory(
      DefaultCookieHeaderEncoding(httpConfiguration.cookies),
      cookieBaker,
      LegacyFlashCookieBaker(httpConfiguration.flash, httpConfiguration.secret, cookieSigner)
    )

  given ActorSystem = actorSystem

  given StandaloneWSClient =
    import play.shaded.ahc.org.asynchttpclient.DefaultAsyncHttpClient
    import play.api.libs.ws.WSConfigParser
    import play.api.libs.ws.ahc.{ AhcConfigBuilder, AhcWSClientConfigParser, StandaloneAhcWSClient }
    new StandaloneAhcWSClient(
      DefaultAsyncHttpClient(
        AhcConfigBuilder(
          AhcWSClientConfigParser(
            WSConfigParser(configuration.underlying, environment.classLoader).parse(),
            configuration.underlying,
            environment.classLoader
          ).parse()
        ).modifyUnderlying(_.setIoThreadsCount(8)).build()
      )
    )

  val env: lila.app.Env =
    lila.log("boot").info(s"Start loading lila modules")
    ProductionConfigValidator.validate(configuration, environment.mode)
    val c = lila.common.Chronometer.sync(wire[lila.app.Env])
    lila.log("boot").info(s"Loaded lila modules in ${c.showDuration}")
    c.result

  val httpFilters = Seq(
    lila.web.HttpFilter(
      env.net,
      env.web.settings.sitewideCoepCredentiallessHeader.get
    )
  )

  override lazy val httpErrorHandler =
    lila.app.http.ErrorHandler(
      environment = environment,
      config = configuration,
      router = router,
      mainC = main
    )

  override lazy val httpRequestHandler: HttpRequestHandler =
    lila.app.http.HttpRequestHandler(
      router,
      httpErrorHandler,
      httpConfiguration,
      httpFilters,
      controllerComponents
    )

  lazy val devAssetsController =
    given FileMimeTypes = fileMimeTypes
    wire[ExternalAssets]

  // Core controllers only
  lazy val account: Account = wire[Account]
  lazy val analyse: Analyse = wire[Analyse]
  lazy val auth: Auth = wire[Auth]
  lazy val betaFeedback: BetaFeedback = wire[BetaFeedback]
  lazy val editor: Editor = wire[Editor]
  lazy val explorerProxy: ExplorerProxy = wire[ExplorerProxy]
  lazy val importer: Importer = wire[Importer]
  lazy val main: Main = wire[Main]
  lazy val pref: Pref = wire[Pref]
  lazy val study: Study = wire[Study]
  lazy val user: User = wire[User]
  lazy val userAnalysis: UserAnalysis = wire[UserAnalysis]
  lazy val api = env.apiC

  val router: Router = wire[_root_.router.router.Routes]

  lila.common.Uptime.startedAt
  UiEnv.setEnv(env)

  if configuration.get[Boolean]("kamon.enabled") then
    lila.log("boot").info("Kamon is enabled")
    kamon.Kamon.init()
