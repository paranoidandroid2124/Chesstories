package lila.web

import akka.stream.Materializer
import play.api.mvc.*

import lila.common.HTTPRequest
import lila.core.config.NetConfig

final class HttpFilter(
    net: NetConfig,
    sitewideCoepCredentiallessHeader: () => Boolean,
    cookieHeaderEncoding: CookieHeaderEncoding = DefaultCookieHeaderEncoding()
)(using val mat: Materializer)(using Executor)
    extends Filter
    with ResponseHeaders:

  private val logger = lila.log("http")

  def apply(handle: RequestHeader => Fu[Result])(req: RequestHeader): Fu[Result] =
    val response =
      if HTTPRequest.isAssets(req) then serveAssets(handle(req))
      else
        val startTime = nowMillis
        redirectWrongDomain(req)
          .map(fuccess)
          .getOrElse:
            handle(req).map: result =>
              logging(req, startTime)(result)
              addContextualResponseHeaders(req):
                addEmbedderPolicyHeaders(req):
                  result
    response.map(SidOnlyCookiePolicy(_, req, cookieHeaderEncoding))

  private def logging(req: RequestHeader, startTime: Long)(result: Result) =
    if net.logRequests then
      val actionName = HTTPRequest.actionName(req)
      val reqTime = nowMillis - startTime
      val statusCode = result.header.status
      val client = HTTPRequest.clientName(req)
      logger.info(s"$statusCode $client ${req.method} $actionName ${reqTime}ms")
    result

  private def serveAssets(res: Fu[Result]) =
    res.dmap:
      _.withHeaders(assetsHeaders*)

  private def redirectWrongDomain(req: RequestHeader): Option[Result] = {
    req.host != net.domain.value &&
    HTTPRequest.isRedirectable(req) &&
    !HTTPRequest.isProgrammatic(req) &&
    // asset request going through the CDN, don't redirect
    !(req.host == net.assetDomain.value && HTTPRequest.hasFileExtension(req))
  }.option(Results.MovedPermanently(s"http${if req.secure then "s" else ""}://${net.domain}${req.uri}"))

  private def addContextualResponseHeaders(req: RequestHeader)(result: Result) =
    val secured = result.withHeaders(securityHeaders(isHttps(req))*)
    if HTTPRequest.isApiOrApp(req)
    then secured.withHeaders(headersForApiOrApp(allowDevAppOrigin = !net.baseUrl.value.startsWith("https://"))(using req)*)
    else secured

  private def isHttps(req: RequestHeader) =
    req.secure || net.baseUrl.value.startsWith("https://")

  private def addEmbedderPolicyHeaders(req: RequestHeader)(result: Result) =
    if !crossOriginPolicy.isSet(result)
      && crossOriginPolicy.supportsCredentiallessIFrames(req)
      && sitewideCoepCredentiallessHeader()
    then result.withHeaders(crossOriginPolicy.credentialless*)
    else result
