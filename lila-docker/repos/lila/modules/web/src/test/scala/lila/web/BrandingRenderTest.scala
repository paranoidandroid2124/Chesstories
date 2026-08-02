package lila.web

import munit.FunSuite
import lila.web.ui.*
import lila.common.*
import lila.common.config.GetRelativeFile
import lila.core.config.*
import lila.ui.Helpers
import play.api.mvc.*
import play.api.mvc.request.*
import play.api.libs.typedmap.TypedMap
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import lila.ui.{ Context, PageContext, AssetHelper }
import lila.core.net.IpAddress
import java.net.URI
import scalatags.Text.all.*

class BrandingRenderTest extends FunSuite {

  test("Render site header and check for Chesstory branding") {
    // Setup manifest file
    val manifestJson = """{
      "js": {},
      "css": {},
      "hashed": {
        "font/chesstory.woff2": "123456",
        "font/chesstory-chess.woff2": "654321"
      }
    }"""
    val manifestFile = Files.createTempFile("manifest", ".json").toFile
    Files.write(manifestFile.toPath, manifestJson.getBytes(StandardCharsets.UTF_8))
    
    // Mock GetRelativeFile (opaque type)
    val mockGetFile = GetRelativeFile { _ => manifestFile }

    val mockNetConfig = NetConfig(
          domain = NetDomain("chesstory.com"),
          prodDomain = NetDomain("chesstory.com"),
          baseUrl = BaseUrl("http://chesstory.com"),
          assetDomain = AssetDomain("chesstory.com"),
          assetBaseUrl = AssetBaseUrl("http://chesstory.com"),
          stageBanner = false,
          siteName = "chesstory.com",
          socketDomains = Nil,
          socketAlts = Nil,
          crawlable = false,
          rateLimit = RateLimit(true),
          email = lila.core.email.EmailAddress("contact@chesstory.com"),
          logRequests = false
      )

    val mockAssetHelper = new AssetFullHelper with AssetHelper {
      override def netConfig = mockNetConfig
      override def manifest = new AssetManifest(mockGetFile)
      override def analyseEndpoints = new lila.ui.AnalyseEndpoints("", "")
      override def assetVersion = "test-version" 

      // Resolve conflicts
      override def assetUrl(path: String) = s"/assets/$path"
      override def staticAssetUrl(path: String) = s"/assets/$path"
      override def staticCompiledUrl(path: String) = s"/assets/compiled/$path"
      override def safeJsonValue(v: play.api.libs.json.JsValue) = lila.core.data.SafeJsonStr(v.toString)
      override def assetBaseUrl = "http://chesstory.com"
      override def netBaseUrl = "http://chesstory.com"
      override def embedJsUnsafe(code: String)(nonce: Option[lila.ui.Nonce]) = script(raw(code))
      override def preload(url: String, as: String, _crossOrigin: Boolean, tpe: Option[String]) = raw("") 
      override def iifeModule(path: String) = script(src := s"/assets/$path")
    }

    // Mock RequestHeader manually
    val mockReq = new RequestHeader {
       override def connection = new RemoteConnection {
         def remoteAddress = java.net.InetAddress.getLoopbackAddress
         def secure = false
         def clientCertificateChain = None
       }
       override def method = "GET"
       override def target = new RequestTarget{
          def uri = new URI("/")
          def path = "/"
          override def queryString = ""
          def uriString = "/"
          def queryMap = Map.empty
       }
       override def version = "HTTP/1.1"
       override def headers = Headers()
       override def attrs = TypedMap.empty
    }

    // 2. Mock Context & Pref
    val mockPref = new lila.core.pref.Pref {
      val id = lila.core.userId.UserId("anon")
      val coords = 0
      val keyboardMove = 0
      val voice = None
      val rookCastle = 0
      val animation = 0
      val destination = true
      val moveEvent = 0
      val highlight = true
      val is3d = false
      val resizeHandle = 0
      val theme = "brown"
      val pieceSet = "cburnett"
      def hasKeyboardMove = false
      def hasVoice = false
      def hasSpeech = false
      def animationMillis = 0
      def pieceNotationIsLetter = false
      def currentBg = "dark"
      def themeColor = "#2e2a24"
      def themeColorClass = Some("dark")
      def board = new lila.core.pref.PrefBoard {
         def opacity = 100
         def brightness = 100
         def hue = 0
      }
      def currentTheme = new lila.core.pref.PrefTheme { def name = "brown"; def file = "brown.css" }
      def currentTheme3d = new lila.core.pref.PrefTheme { def name = "brown"; def file = "brown.css" }
      def realTheme = currentTheme
      def currentPieceSet = new lila.core.pref.PrefPieceSet { def name = "cburnett" }
      def realPieceSet = currentPieceSet
    }
    
    val mockCtx = new Context {
       val req: RequestHeader = mockReq
       def isAuth = false
       def isTokenAuth = false
       def me = None
       def user = None
       def userId = None
       def pref = mockPref
       def ip = lila.core.net.IpV4Address("127.0.0.1")
       def blind = false
    }

    implicit val pageCtx: PageContext = new PageContext {
       override val me = None 
       val impersonatedBy = None
       def nonce = None
       def error = false
       
       val req = mockCtx.req
       def isAuth = mockCtx.isAuth
       def isTokenAuth = mockCtx.isTokenAuth
       def user = mockCtx.user
       def userId = mockCtx.userId
       def pref = mockCtx.pref
       def ip = mockCtx.ip
       def blind = mockCtx.blind
    }

    // 3. Instantiate Layout
    val layout = new lila.web.ui.layout(Helpers, mockAssetHelper)
    val renderedTopNav = lila.web.ui.TopNav(Helpers).apply(using pageCtx)

    // 4. Render
    val headerHtml = layout.siteHeader(
      isAppealUser = false,
      error = false,
      topnav = renderedTopNav
    ).toString

    val fontCss = layout.chesstoryFontFaceCss.toString

    // 5. Verify Header
    assert(headerHtml.toLowerCase.contains("chesstory"), "Header should contain 'chesstory' (case-insensitive)")
    assert(headerHtml.contains("""class="cs-wordmark""""))
    assert(headerHtml.contains("""class="cs-brandbar""""))
    assert(headerHtml.contains("""class="cs-brand""""))
    assert(headerHtml.contains("""class="cs-mark""""))
    assert(headerHtml.contains("""class="cs-nav-toggle fullscreen-toggle""""))
    assert(headerHtml.contains("""class="cs-burger""""))
    assert(headerHtml.contains("""class="cs-burger__in""""))
    assert(headerHtml.contains("""id="cs-nav""""), "Top nav should use cs-nav id")
    assert(headerHtml.contains("""class="cs-nav__main""""), "Top nav should use cs-nav__main class")
    assert(headerHtml.contains("""class="cs-nav__link""""), "Top nav links should use cs-nav__link class")
    assert(!headerHtml.contains("lichess.org"), "Header should NOT contain 'lichess.org'")
    assert(headerHtml.contains("""id="cs-top""""), "Header should use cs-top id")
    assert(headerHtml.contains("""id="cs-nav-toggle""""), "Header should use cs-nav-toggle id")
    assert(!headerHtml.contains("""id="top""""), "Legacy top id should be removed")
    assert(!headerHtml.contains("""id="topnav""""), "Legacy topnav id should be removed")
    assert(!headerHtml.contains("site-title-nav"), "Legacy site-title-nav class should be removed")
    assert(!headerHtml.contains("topnav-toggle"), "Legacy topnav-toggle class should be removed")
    assert(!headerHtml.contains("""class="hbg""""), "Legacy burger class should be removed")

    // 6. Verify Fonts
    assert(fontCss.contains("chesstory.woff2"), "CSS should contain chesstory.woff2")
    assert(fontCss.contains("font-family: 'chesstory'"), "CSS should set font-family to chesstory")
    
    // Cleanup
    manifestFile.delete() 
  }
}
