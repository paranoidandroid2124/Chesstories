package lila.common

import play.api.http.HeaderNames
import play.api.mvc.RequestHeader
import play.api.routing.Router

import lila.core.net.*

object HTTPRequest:

  def isXhr(req: RequestHeader): Boolean =
    req.headers.get("X-Requested-With").contains("XMLHttpRequest")

  def isSynchronousHttp(req: RequestHeader) = !isXhr(req)

  def isSafe(req: RequestHeader) = req.method == "GET" || req.method == "HEAD" || req.method == "OPTIONS"
  def isUnsafe(req: RequestHeader) = !isSafe(req)

  def isRedirectable(req: RequestHeader) = isSynchronousHttp(req) && isSafe(req) && !isLichessMobile(req)

  private val nativeAppOrigins = List(
    "capacitor://localhost", // ios
    "ionic://localhost" // ios
  )

  private val devAppOrigins = List(
    "http://localhost" // android/dev/flutter
  )

  def appOrigin(req: RequestHeader, allowDevOrigin: Boolean = true): Option[String] =
    origin(req).filter: reqOrigin =>
      val appOrigins = nativeAppOrigins ::: allowDevOrigin.so(devAppOrigins)
      appOrigins.exists: appOrigin =>
        reqOrigin == appOrigin || reqOrigin.startsWith(s"$appOrigin:")

  def isApi(req: RequestHeader) = req.path.startsWith("/api/")
  def isApiOrApp(req: RequestHeader) = isApi(req) || appOrigin(req).isDefined

  def isAssets(req: RequestHeader) = req.path.startsWith("/assets/")

  def userAgent(req: RequestHeader): UserAgent =
    req.headers.get(HeaderNames.USER_AGENT).fold(UserAgent.zero)(UserAgent(_))

  val isChrome96Plus = UaMatcher("""Chrome/(?:\d{3,}|9[6-9])""")
  val isChrome113Plus = UaMatcher("""Chrome/(?:11[3-9]|1[2-9]\d)""")
  val isFirefox119Plus = UaMatcher("""Firefox/(?:119|1[2-9]\d)""")
  val isMobileBrowser = UaMatcher("""(?i)iphone|ipad|ipod|android.+mobile""")
  def isLichessMobile(ua: UserAgent): Boolean = ua.value.startsWith("Lichess Mobile/")
  def isLichessMobile(req: RequestHeader): Boolean = isLichessMobile(userAgent(req))

  def origin(req: RequestHeader): Option[String] = req.headers.get(HeaderNames.ORIGIN)
  def referer(req: RequestHeader): Option[String] = req.headers.get(HeaderNames.REFERER)

  def ipAddress(req: RequestHeader): IpAddress =
    IpAddress.unchecked(ipAddressStr(req))

  def ipAddressStr(req: RequestHeader): String =
    // chain of trusted proxies, strip scope id
    req.remoteAddress.split(", ").last.split("%").head

  def isCrawler(req: RequestHeader) = Crawler(crawlerMatcher(req))

  private val crawlerMatcher = UaMatcher:
    // spiders/crawlers
    """Qwantbot|Googlebot|GoogleOther|AdsBot|Google-Read-Aloud|bingbot|BingPreview|facebookexternalhit|meta-externalagent|SemrushBot|AhrefsBot|PetalBot|Applebot|YandexBot|YandexAdNet|YandexImages|Twitterbot|Bluesky|Baiduspider|Amazonbot|Bytespider|yacybot|ImagesiftBot|ChatGLM-Spider|YisouSpider|Yeti/|DataForSeoBot|ChatGPT|openai.com|anthropic.com|TikTokSpider""" +
      // apps and servers that load previews
      """|Discordbot|WhatsApp""" +
      // http libs
      """|HeadlessChrome|okhttp|axios|undici|wget|curl|python-requests|aiohttp|commons-httpclient|python-urllib|python-httpx|Nessus|imroc/req"""

  final class UaMatcher(rStr: String):
    private val pattern = rStr.r.pattern
    def apply(req: RequestHeader): Boolean = pattern.matcher(userAgent(req).value).find

  def isHuman(req: RequestHeader) = isCrawler(req).no

  private val fileExtensionRegex = """\.(?<!^\.)[a-zA-Z0-9]{2,4}$""".r

  def hasFileExtension(req: RequestHeader) = fileExtensionRegex.find(req.path)

  def print(req: RequestHeader) = s"${printReq(req)} ${printClient(req)}"

  def printReq(req: RequestHeader) = s"${req.method} ${req.domain}${req.uri}"

  def printClient(req: RequestHeader) =
    s"${ipAddress(req)} origin:${~origin(req)} referer:${~referer(req)} ua:${userAgent(req).value}"

  private val webXhrAccepts = Set("application/web.chesstory+json", "application/web.lichess+json")
  private def startsWithVersionedApiAccept(a: String) = a.startsWith("application/vnd.lichess.v")
  def accepts(req: RequestHeader): Option[String] = req.headers.get(HeaderNames.ACCEPT)
  def acceptsNdJson(req: RequestHeader) = accepts(req) contains "application/x-ndjson"
  def acceptsJson(req: RequestHeader) = accepts(req).exists: a =>
    webXhrAccepts.contains(a) || a.startsWith("application/json") || startsWithVersionedApiAccept(a)
  def isEventSource(req: RequestHeader): Boolean = accepts(req) contains "text/event-stream"
  def isProgrammatic(req: RequestHeader) =
    !isSynchronousHttp(req) || isApi(req) ||
      accepts(req).exists(startsWithVersionedApiAccept)

  def actionName(req: RequestHeader): String =
    req.attrs.get(Router.Attrs.ActionName).getOrElse("NoHandler")

  private val VersionedApiAcceptPattern = """application/vnd\.lichess\.v(\d++)\+json""".r

  def apiVersion(req: RequestHeader): Option[ApiVersion] =
    accepts(req).flatMap:
      case VersionedApiAcceptPattern(v) => ApiVersion.from(v.toIntOption)
      case _ => none

  private def isAppeal(req: RequestHeader) = req.path.startsWith("/appeal")
  private def isGameExport(req: RequestHeader) =
    "^/@/[\\w-]{2,30}/download$".r.matches(req.path) ||
      "^/(api/games/user|games/export)/[\\w-]{2,30}($|/.+)".r.matches(req.path)
  private def isStudyExport(req: RequestHeader) = "^/study/by/[\\w-]{2,30}/export.pgn$".r.matches(req.path)
  private def isAccount(req: RequestHeader) = req.path.startsWith("/account")

  def isClosedLoginPath(req: RequestHeader) =
    isAppeal(req) || isStudyExport(req) || isGameExport(req) || isAccount(req)

  def clientName(req: RequestHeader) =
    if isXhr(req) then apiVersion(req).fold("xhr")(v => s"lichobile/$v")
    else if isLichessMobile(req) then "mobile"
    else if isCrawler(req).yes then "crawler"
    else "browser"

  def queryStringGet(req: RequestHeader, name: String): Option[String] =
    req.queryString.get(name).flatMap(_.headOption).filter(_.nonEmpty)

  // this header is set by our nginx config, based on the nginx whitelist file.
  def nginxWhitelist(req: RequestHeader) =
    req.headers.get("X-Ip-Tier").flatMap(_.toIntOption).exists(_ > 1)
