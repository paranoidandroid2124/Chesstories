package lila.web

import scala.concurrent.{ Await, Future }
import akka.actor.ActorSystem
import akka.stream.Materializer
import ch.qos.logback.classic.{ Level, Logger as LogbackLogger }
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import org.slf4j.LoggerFactory
import play.api.libs.typedmap.TypedMap
import play.api.mvc.{ Headers, RequestHeader, Results }
import play.api.mvc.request.{ RemoteConnection, RequestTarget }
import play.api.routing.Router

import lila.core.config.*
import lila.core.email.EmailAddress

class HttpFilterTest extends munit.FunSuite:

  private val system = ActorSystem("http-filter-test")
  private given Executor = system.dispatcher
  private given Materializer = Materializer(system)

  private val net = NetConfig(
    domain = NetDomain("test.example"),
    prodDomain = NetDomain("test.example"),
    baseUrl = BaseUrl("https://test.example"),
    assetDomain = AssetDomain("assets.test.example"),
    assetBaseUrl = AssetBaseUrl("https://assets.test.example"),
    stageBanner = false,
    siteName = "Test",
    socketDomains = Nil,
    socketAlts = Nil,
    crawlable = false,
    rateLimit = RateLimit.No,
    email = EmailAddress("test@example.com"),
    logRequests = true
  )
  private val filter = HttpFilter(net, () => false)

  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  test("request logging retains metadata without request targets"):
    val tokenValues = List("synthetic-path-token", "synthetic-query-token")
    def tokenUri(path: String) = s"$path/${tokenValues.head}?continue=${tokenValues.last}"
    List(
      ("GET", tokenUri("/auth/password-reset"), "controllers.Auth.passwordResetToken", tokenValues),
      ("GET", tokenUri("/auth/signup-confirm"), "controllers.Auth.signupConfirmToken", tokenValues),
      ("GET", tokenUri("/account/email-confirm"), "controllers.Account.emailConfirm", tokenValues),
      (
        "POST",
        "/login?ref=normal-query-value",
        "controllers.Auth.authenticate",
        List("/login", "normal-query-value")
      )
    ).foreach:
      case (method, uri, action, absent) =>
        val event = loggedEvent(method, uri, action)
        (uri :: absent).foreach: value =>
          assert(!event.contains(value), s"$uri leaked $value in $event")
        assert(event.startsWith(s"200 browser $method $action "))
        assert(event.endsWith("ms"))

  private def loggedEvent(method: String, uri: String, action: String): String =
    val logger = LoggerFactory.getLogger("http").asInstanceOf[LogbackLogger]
    val previousLevel = logger.getLevel
    val appender = new ListAppender[ILoggingEvent]()
    logger.setLevel(Level.INFO)
    appender.start()
    logger.addAppender(appender)
    try
      Await.result(filter(_ => Future.successful(Results.Ok))(request(method, uri, action)), 2.seconds)
      assertEquals(appender.list.size(), 1)
      appender.list.get(0).getFormattedMessage
    finally
      logger.detachAppender(appender)
      appender.stop()
      logger.setLevel(previousLevel)

  private def request(requestMethod: String, url: String, action: String): RequestHeader = new RequestHeader:
    val method = requestMethod
    val target = RequestTarget(url, url.takeWhile(_ != '?'), Map.empty[String, Seq[String]])
    val headers = Headers("Host" -> net.domain.value)
    val attrs = TypedMap.empty.updated(Router.Attrs.ActionName, action)
    val connection = RemoteConnection("127.0.0.1", secure = false, clientCertificateChain = None)
    val version = "HTTP/1.1"
    override def withTarget(target: RequestTarget) = this
    override def withHeaders(headers: Headers) = this
    override def withAttrs(attrs: TypedMap) = this
    override def withConnection(connection: RemoteConnection) = this
