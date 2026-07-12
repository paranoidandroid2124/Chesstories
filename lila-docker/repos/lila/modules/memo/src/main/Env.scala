package lila.memo

import com.softwaremill.macwire.*
import play.api.{ ConfigLoader, Configuration }

import lila.common.autoconfig.{ *, given }
import lila.common.config.given
import lila.core.config.*

final class MemoConfig(
    @ConfigName("collection.cache") val cacheColl: CollName,
    @ConfigName("collection.config") val configColl: CollName
)

@Module
final class Env(
    appConfig: Configuration,
    db: lila.db.Db,
    net: NetConfig
)(using Executor, Scheduler, play.api.Mode):

  export net.{ domain, assetDomain }

  val config = appConfig.get[MemoConfig]("memo")(using AutoConfig.loader)

  val cacheApi = wire[CacheApi]

  val settingStore = wire[SettingStore.Builder]

  val mongoCache = wire[MongoCache.Api]

  val mongoRateLimitApi = wire[MongoRateLimitApi]

  val markdown = wire[MarkdownCache]

  lila.common.Cli.handle:
    case "cache" :: "clear" :: name :: Nil =>
      cacheApi.clearByName(name) match
        case Some(nb) => fuccess(s"Cleared $nb entries from cache $name")
        case None => fufail(s"No cache named $name")
