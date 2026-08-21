package lila.web

import play.api.http.HeaderNames.SET_COOKIE
import play.api.mvc.*
import play.api.mvc.request.RequestAttrKey

object SidOnlyCookiePolicy:

  val allowedCookieName = "sid"
  private val legacyCookieNames = Set(
    "PLAY_FLASH",
    "PLAY_SESSION",
    "chesstory",
    "email_confirm",
    "lila-email-confirm",
    "bg",
    "zoom",
    "chesstory_cookie_consent",
    "mBzamRgfXgRBSnXB",
    "rk2"
  )

  def apply(result: Result, req: RequestHeader, encoding: CookieHeaderEncoding): Result =
    val rawCookies = result.header.headers.iterator.collect:
      case (name, value) if name.equalsIgnoreCase(SET_COOKIE) =>
        encoding.decodeSetCookieHeader(value)
    val allowedHeaderCookies = rawCookies.flatten.filter(_.name == allowedCookieName).toSeq
    val headersWithoutCookies = result.header.headers.filterNot((name, _) => name.equalsIgnoreCase(SET_COOKIE))
    val headers =
      if allowedHeaderCookies.isEmpty then headersWithoutCookies
      else headersWithoutCookies.updated(SET_COOKIE, encoding.encodeSetCookieHeader(allowedHeaderCookies))

    result.copy(
      header = result.header.copy(headers = headers),
      newSession = None,
      newFlash = None,
      newCookies =
        result.newCookies.filter(_.name == allowedCookieName) ++
          req.attrs
            .get(RequestAttrKey.Cookies)
            .fold(Seq.empty[String])(_.value.iterator.map(_.name).toSeq)
            .filter(legacyCookieNames)
            .toSet
            .toSeq
            .sorted
            .map(name => DiscardingCookie(name, path = "/").toCookie)
    )
