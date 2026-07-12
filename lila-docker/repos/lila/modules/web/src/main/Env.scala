package lila.web

import com.softwaremill.macwire.*
import play.api.libs.ws.StandaloneWSClient

@Module
final class Env(
    appConfig: play.api.Configuration,
    cacheApi: lila.memo.CacheApi,
    settingStore: lila.memo.SettingStore.Builder,
    ws: StandaloneWSClient,
    net: lila.core.config.NetConfig,
    getFile: lila.common.config.GetRelativeFile
)(using mode: play.api.Mode, scheduler: akka.actor.Scheduler)(using scala.concurrent.ExecutionContext):

  val config = WebConfig.loadFrom(appConfig)
  export net.baseUrl

  val analyseEndpoints = WebConfig.analyseEndpoints(appConfig)
  lazy val lilaVersion = WebConfig.lilaVersion(appConfig)

  lazy val mobile = wire[Mobile]

  val manifest = wire[AssetManifest]

  val referrerRedirect = wire[ReferrerRedirect]

  object settings:
    import lila.core.data.{ Strings, UserIds }
    import lila.memo.SettingStore.Strings.given
    import lila.memo.SettingStore.UserIds.given

    val apiTimeline = settingStore[Int](
      "apiTimelineEntries",
      default = 10,
      text = Some("API timeline entries to serve")
    )
    val noDelaySecret = settingStore[Strings](
      "noDelaySecrets",
      default = Strings(Nil),
      text = Some("Secret tokens that allows fetching ongoing games without the delay. Separated by commas.")
    )
    val prizeTournamentMakers = settingStore[UserIds](
      "prizeTournamentMakers",
      default = UserIds(Nil),
      text =
        Some("User IDs who can make prize tournaments (arena & swiss) without a warning. Separated by commas.")
    )
    val apiExplorerGamesPerSecond = settingStore[Int](
      "apiExplorerGamesPerSecond",
      default = 300,
      text = Some("Opening explorer games per second")
    )
    val sitewideCoepCredentiallessHeader = settingStore[Boolean](
      "sitewideCoepCredentiallessHeader",
      default = true,
      text = Some("Enable COEP:credentialless header site-wide in supported browsers (Chromium)")
    )
