package lila.web

import play.api.mvc.*

import lila.common.HTTPRequest
import lila.core.config.BaseUrl
import lila.core.pref.Pref
import lila.core.net.UserAgent
import lila.ui.Context

trait CtrlExtensions extends play.api.mvc.ControllerHelpers with ResponseHeaders:

  def baseUrl: BaseUrl

  given (using ctx: Context): RequestHeader = ctx.req
  given (using ctx: Context): Pref = ctx.pref
  given (using req: RequestHeader): UserAgent = HTTPRequest.userAgent(req)


  extension (req: RequestHeader)
    def ipAddress = HTTPRequest.ipAddress(req)
    def userAgent = HTTPRequest.userAgent(req)

  extension (result: Result)
    def toFuccess = Future.successful(result)
    def withCanonical(url: String): Result =
      result.withHeaders(LINK -> s"<${baseUrl}${url}>; rel=\"canonical\"")
    def withCanonical(url: Call): Result = withCanonical(url.url)
    def enforceCrossSiteIsolation(using req: RequestHeader): Result =
      result.withHeaders(crossOriginPolicy.forReq(req)*)
    def noCache: Result = result.withHeaders(
      CACHE_CONTROL -> "no-cache, no-store, must-revalidate",
      EXPIRES -> "0"
    )
    def headerCacheSeconds(seconds: Int) = result.withHeaders(CACHE_CONTROL -> s"max-age=$seconds")
    def hasPersonalData = result.noCache
    def noProxyBuffer = result.withHeaders("X-Accel-Buffering" -> "no")
    def withServiceWorker(using RequestHeader) =
      result.enforceCrossSiteIsolation.withHeaders("Service-Worker-Allowed" -> "/")
    def asAttachment(name: String) = result.withHeaders(CONTENT_DISPOSITION -> s"attachment; filename=$name")
    def asAttachmentStream(name: String) = result.noProxyBuffer.asAttachment(name)
