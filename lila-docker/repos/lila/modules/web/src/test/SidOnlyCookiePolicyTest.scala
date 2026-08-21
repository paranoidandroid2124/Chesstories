package lila.web

import play.api.http.HeaderNames.{ COOKIE, SET_COOKIE }
import play.api.libs.typedmap.TypedMap
import play.api.mvc.*
import play.api.mvc.request.{ DefaultRequestFactory, RemoteConnection, RequestTarget }

class SidOnlyCookiePolicyTest extends munit.FunSuite:

  private val encoding = DefaultCookieHeaderEncoding()
  private val sessionBaker = LegacySessionCookieBaker()
  private val flashBaker = LegacyFlashCookieBaker()
  private val requestFactory = SidOnlyRequestFactory:
    DefaultRequestFactory(encoding, sessionBaker, flashBaker)

  test("outbound policy keeps sid and removes every other cookie channel"):
    val rawHeader = encoding.encodeSetCookieHeader:
      Seq(Cookie("bg", "dark"), Cookie("sid", "from-header"))
    val dirty = Results.Redirect("/")
      .withHeaders(SET_COOKIE -> rawHeader)
      .withCookies(Cookie("zoom", "80"), Cookie("sid", "from-result"))
      .withSession("legacy" -> "value")
      .flashing("success" -> "stored")

    val clean = SidOnlyCookiePolicy(dirty, request(), encoding)
    assertEquals(clean.newSession, None)
    assertEquals(clean.newFlash, None)
    assertEquals(clean.newCookies.map(_.name), Seq("sid"))
    assertEquals(headerCookieNames(clean), Seq("sid"))

    val baked = bake(request(), clean)
    assert(baked.header.headers.get(SET_COOKIE).isDefined, "sid Set-Cookie header is present")
    assert(
      encoding.decodeSetCookieHeader(baked.header.headers(SET_COOKIE)).forall(_.name == "sid"),
      "post-bake output contains only sid"
    )

  test("inbound legacy Play cookies never populate session or flash or trigger a discard"):
    val staleFlash = flashBaker.encodeAsCookie(Flash(Map("success" -> "stale")))
    val staleSession = sessionBaker.encodeAsCookie(Session(Map("legacy" -> "value")))
    val req = request(Seq(Cookie("sid", "session-id"), staleFlash, staleSession))

    assertEquals(req.cookies.get("sid").map(_.value), Some("session-id"))
    assert(req.session.isEmpty, "legacy Play session is ignored")
    assert(req.flash.isEmpty, "legacy Play flash is ignored")

    val baked = bake(req, Results.Ok)
    assertEquals(baked.header.headers.get(SET_COOKIE), Option.empty[String], "no automatic flash discard")

  test("legacy cookies are expired once without allowing new non-sid values"):
    val req = request:
      Seq(Cookie("sid", "session-id"), Cookie("chesstory", "old"), Cookie("email_confirm", "old"))
    val clean = SidOnlyCookiePolicy(Results.Ok.withCookies(Cookie("bg", "dark")), req, encoding)
    val expected: Set[(String, Option[Int])] = Set(
      "chesstory" -> Some(Cookie.DiscardedMaxAge),
      "email_confirm" -> Some(Cookie.DiscardedMaxAge)
    )
    assertEquals(clean.newCookies.map(cookie => cookie.name -> cookie.maxAge).toSet, expected)

    val baked = bake(req, clean)
    val cookies = encoding.decodeSetCookieHeader(baked.header.headers(SET_COOKIE))
    assert(
      cookies.forall(cookie => cookie.name == "sid" || cookie.maxAge.contains(Cookie.DiscardedMaxAge)),
      "legacy cookies are deletion-only"
    )

  private def headerCookieNames(result: Result): Seq[String] =
    result.header.headers.iterator
      .collect:
        case (name, value) if name.equalsIgnoreCase(SET_COOKIE) =>
          encoding.decodeSetCookieHeader(value).map(_.name)
      .flatten
      .toSeq

  private def bake(req: RequestHeader, result: Result): Result =
    result.bakeCookies(encoding, sessionBaker, flashBaker, !req.flash.isEmpty)

  private def request(cookies: Seq[Cookie] = Nil): RequestHeader =
    val headers =
      if cookies.isEmpty then Headers("Host" -> "test.example")
      else Headers("Host" -> "test.example", COOKIE -> encoding.encodeCookieHeader(cookies))
    requestFactory.createRequestHeader(
      RemoteConnection("127.0.0.1", secure = true, clientCertificateChain = None),
      "GET",
      RequestTarget("/", "/", Map.empty),
      "HTTP/1.1",
      headers,
      TypedMap.empty
    )
