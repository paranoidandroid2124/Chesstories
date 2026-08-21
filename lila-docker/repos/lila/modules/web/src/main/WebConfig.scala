package lila.web

import play.api.{ Configuration, ConfigLoader }

import lila.common.config.given
import lila.core.config.*

final class WebConfig(
    val apiToken: Secret
)

object WebConfig:

  private def configuredString(c: Configuration, path: String): Option[String] =
    c.getOptional[String](path).map(_.trim).filter(_.nonEmpty)

  private def configuredOrEnv(c: Configuration, path: String, envName: String): String =
    configuredString(c, path)
      .orElse(sys.env.get(envName).map(_.trim).filter(_.nonEmpty))
      .getOrElse("")

  def loadFrom(c: Configuration) =
    WebConfig(
      c.get[Secret]("api.token")
    )

  def analyseEndpoints(c: Configuration) =
    lila.ui.AnalyseEndpoints(
      explorer = configuredOrEnv(c, "explorer.endpoint", "EXPLORER_API_BASE"),
      tablebase = configuredOrEnv(c, "explorer.tablebase_endpoint", "TABLEBASE_API_BASE")
    )

  def netConfig(c: Configuration) = NetConfig(
    domain = c.get[NetDomain]("net.domain"),
    prodDomain = c.get[NetDomain]("net.prodDomain"),
    baseUrl = c.get[BaseUrl]("net.base_url"),
    assetDomain = c.get[AssetDomain]("net.asset.domain"),
    assetBaseUrl = c.get[AssetBaseUrl]("net.asset.base_url"),
    stageBanner = c.get[Boolean]("net.stage.banner"),
    siteName = c.get[String]("net.site.name"),
    socketDomains = c.getOptional[List[String]]("net.socket.domains").getOrElse(Nil),
    socketAlts = c.getOptional[List[String]]("net.socket.alts").getOrElse(Nil),
    crawlable = c.get[Boolean]("net.crawlable"),
    rateLimit = c.get[RateLimit]("net.ratelimit"),
    email = c.get[EmailAddress]("net.email"),
    logRequests = c.get[Boolean]("net.http.log")
  )

  final class LilaVersion(val date: String, val commit: String, val message: String)

  def lilaVersion(c: Configuration): Option[LilaVersion] = (
    c.getOptional[String]("app.version.date"),
    c.getOptional[String]("app.version.commit"),
    c.getOptional[String]("app.version.message")
  ).mapN(LilaVersion.apply)
