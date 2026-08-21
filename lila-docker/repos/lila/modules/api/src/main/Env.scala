package lila.api

import akka.actor.*
import com.softwaremill.macwire.*
import play.api.Mode
import scala.annotation.unused

@Module
final class Env(
    net: lila.core.config.NetConfig,
    @unused securityStore: lila.security.SessionStore,
    @unused mailerEnv: lila.mailer.Env,
    @unused prefApi: lila.pref.PrefApi,
    @unused userEnv: lila.user.Env,
    @unused analyseEnv: lila.analyse.Env,
    @unused cacheApi: lila.memo.CacheApi,
    webConfig: lila.web.WebConfig,
    manifest: lila.web.AssetManifest
)(using scheduler: Scheduler)(using
    Mode,
    Executor
):

  export net.{ baseUrl, domain }

  export webConfig.apiToken

  lazy val cli = wire[Cli]
