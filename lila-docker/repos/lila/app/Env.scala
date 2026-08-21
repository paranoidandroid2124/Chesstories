package lila.app

import com.softwaremill.macwire.*
import play.api.libs.ws.StandaloneWSClient
import play.api.mvc.{ Call, ControllerComponents }
import play.api.{ Configuration, Environment, Mode }

import lila.core.config.*
import lila.common.config.GetRelativeFile

final class Env(
    val config: Configuration,
    val controllerComponents: ControllerComponents,
    environment: Environment,
    shutdown: akka.actor.CoordinatedShutdown
)(using val system: akka.actor.ActorSystem, val executor: Executor)(using
    StandaloneWSClient,
    akka.stream.Materializer
):
  object webHttp:
    val client: StandaloneWSClient = summon[StandaloneWSClient]
  val net: NetConfig = lila.web.WebConfig.netConfig(config)
  export net.baseUrl
  val routeUrl: Call => Url = call => Url(s"${baseUrl}${call.url}")

  given mode: Mode = environment.mode
  given scheduler: Scheduler = system.scheduler
  given RateLimit = net.rateLimit
  given NetDomain = net.domain
  val getFile: GetRelativeFile = GetRelativeFile(environment.getFile(_))
  val openBetaBindings = wire[OpenBetaBindings]

  val mongo: lila.db.Env = wire[lila.db.Env]
  val memo: lila.memo.Env = wire[lila.memo.Env]
  val user: lila.user.Env = new lila.user.Env(
    db = mongo.mainDb,
    cacheApi = memo.cacheApi
  )
  val mailer: lila.mailer.Env = wire[lila.mailer.Env]
  val security: lila.security.Env = wire[lila.security.Env]
  val pref: lila.pref.Env = wire[lila.pref.Env]
  val analyse: lila.analyse.Env = new lila.analyse.Env(net = net)

  val study: lila.study.Env = new lila.study.Env(
    appConfig = config,
    lightUserApi = user.lightUserApi,
    userApi = user.api,
    annotator = analyse.annotator,
    mongo = mongo,
    net = net,
    cacheApi = memo.cacheApi
  )

  val beta: lila.beta.Env = new lila.beta.Env(
    mongo = mongo,
    userApi = user.api
  )

  val web: lila.web.Env = wire[lila.web.Env]
  val api: lila.api.Env = wire[lila.api.Env]
  lazy val apiC: lila.api.Api = wire[lila.api.Api]

  val announceApi = new lila.web.AnnounceApi(mongo.mainDb(CollName("announce")))

  val pageCache = wire[http.PageCache]

end Env
