package lila.app

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.Future

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.sun.net.httpserver.HttpServer
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.shaded.ahc.org.asynchttpclient.DefaultAsyncHttpClient

import lila.common.config.GetRelativeFile

class OpenBetaBindingsTest extends munit.FunSuite:

  private val system = ActorSystem("open-beta-bindings-test")
  private given Executor = system.dispatcher
  private given Materializer = Materializer(system)
  private val ws = new StandaloneAhcWSClient(new DefaultAsyncHttpClient())

  override def afterAll(): Unit =
    ws.close()
    scala.concurrent.Await.result(system.terminate(), 5.seconds)

  test("runtime health probe rejects 404, 401, and 5xx responses"):
    Future
      .traverse(List(404, 401, 500)): statusCode =>
        withServer(_ => statusCode -> "{}") { base =>
          runtimeStatus(base).map: status =>
            assertEquals(status.reachable, Some(false))
            assertEquals(status.readyOk, false)
        }
      .map(_ => ())

  test("runtime health probe rejects malformed and mismatched JSON"):
    val invalidResponses = List(
      "not-json",
      """{"schema_version":"chesstory.move-meaning.response.v3"}""",
      """{"ok":false,"schema_version":"chesstory.move-meaning.response.v3"}""",
      """{"ok":true}""",
      """{"ok":true,"schema_version":"chesstory.move-meaning.response.v2"}""",
      """{"ok":true,"schema_version":"chesstory.move-meaning.response.v3","extra":true}"""
    )
    Future
      .traverse(invalidResponses): response =>
        withServer(_ => 200 -> response) { base =>
          runtimeStatus(base).map(status => assertEquals(status.reachable, Some(false)))
        }
      .map(_ => ())

  test("runtime health probe accepts the declared contract with or without a trailing slash"):
    val health = """{"ok":true,"schema_version":"chesstory.move-meaning.response.v3"}"""
    withServer({ path =>
      if path == "/health" then 200 -> health else 404 -> "{}"
    }) { base =>
      Future
        .traverse(List(base, s"$base/"))(runtimeStatus)
        .map: statuses =>
          statuses.foreach: status =>
            assertEquals(status.reachable, Some(true))
            assertEquals(status.readyOk, true)
    }

  test("generic HTTP probes retain their existing non-5xx behavior"):
    withServer(_ => 404 -> "{}") { base =>
      genericHttpStatus(base).map: status =>
        assertEquals(status.reachable, Some(true))
        assertEquals(status.readyOk, true)
    }

  private def runtimeStatus(base: String): Future[OpenBetaBindingStatus] =
    bindingStatus(
      runtimeManifest,
      s"chesstory.runtime.url = \"$base\""
    )

  private def genericHttpStatus(base: String): Future[OpenBetaBindingStatus] =
    bindingStatus(
      genericHttpManifest,
      s"explorer.endpoint = \"$base\""
    )

  private def bindingStatus(manifest: String, config: String): Future[OpenBetaBindingStatus] =
    val manifestFile = Files.createTempFile("openbeta-bindings", ".json")
    Files.writeString(manifestFile, manifest, StandardCharsets.UTF_8)
    val bindings = new OpenBetaBindings(
      Configuration(ConfigFactory.parseString(config)),
      GetRelativeFile(_ => manifestFile.toFile),
      ws
    )
    bindings.snapshot.map(_.head).andThen { case _ => Files.deleteIfExists(manifestFile) }

  private def withServer(
      response: String => (Int, String)
  )(run: String => Future[Unit]): Future[Unit] =
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/", exchange =>
      val (status, body) = response(exchange.getRequestURI.getPath)
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      exchange.getResponseHeaders.set("Content-Type", "application/json")
      exchange.sendResponseHeaders(status, bytes.length)
      exchange.getResponseBody.write(bytes)
      exchange.close()
    )
    server.start()
    run(s"http://127.0.0.1:${server.getAddress.getPort}").andThen { case _ => server.stop(0) }

  private val runtimeManifest = """{
    "bindings": [
      {
        "env": "CHESSTORY_RUNTIME_URL",
        "configPath": "chesstory.runtime.url",
        "kind": "plain",
        "requiredMode": "always",
        "readinessClass": "core",
        "probe": "http_json",
        "probePath": "/health",
        "probeResponse": {
          "ok": true,
          "schema_version": "chesstory.move-meaning.response.v3"
        },
        "notes": "runtime base URL"
      }
    ]
  }"""

  private val genericHttpManifest = """{
    "bindings": [
      {
        "env": "EXPLORER_API_BASE",
        "configPath": "explorer.endpoint",
        "kind": "plain",
        "requiredMode": "always",
        "readinessClass": "core",
        "probe": "http",
        "notes": "explorer endpoint"
      }
    ]
  }"""
