package controllers

import com.typesafe.config.ConfigFactory
import play.api.Configuration
import lila.app.{ OpenBetaBindingSpec, OpenBetaBindingStatus }

class OperationalReadinessTest extends munit.FunSuite:

  test("health readiness fails when a required check fails"):
    val checks = List(
      Main.HealthCheck("mongo", ok = true, required = true, detail = "query_ok"),
      Main.HealthCheck("mailer", ok = false, required = true, detail = "disabled_by_live_setting"),
      Main.HealthCheck("explorer", ok = true, required = false, detail = "disabled")
    )

    assertEquals(Main.isReady(checks), false)

  test("mailer check reports mock mail and missing settings as not ready"):
    val config = Configuration(
      ConfigFactory.parseString("""
        mailer.primary.mock = true
      """)
    )

    val check = Main.mailerCheck(config, canSend = true, required = true)

    assertEquals(check.ok, false)
    assertEquals(check.detail, "mock_enabled")

  test("binding health check treats soft bindings as informational only"):
    val softBinding = OpenBetaBindingStatus(
      spec = OpenBetaBindingSpec(
        env = "SUPPORT_PATREON_URL",
        configPath = "support.links.patreon",
        kind = "plain",
        requiredMode = "soft_optional",
        readinessClass = "soft",
        probe = "http",
        notes = "optional support link"
      ),
      required = false,
      configured = false,
      reachable = None,
      detail = "optional_missing"
    )

    val check = Main.bindingHealthCheck(softBinding, requiredInProd = true).get

    assertEquals(check.required, false)
    assertEquals(check.ok, true)
    assertEquals(check.detail, "optional_missing")

  test("binding health check fails required configured endpoint when unreachable"):
    val requiredBinding = OpenBetaBindingStatus(
      spec = OpenBetaBindingSpec(
        env = "EXPLORER_API_BASE",
        configPath = "explorer.endpoint",
        kind = "plain",
        requiredMode = "dispatch_only",
        readinessClass = "core",
        probe = "http",
        notes = "opening explorer endpoint"
      ),
      required = true,
      configured = true,
      reachable = Some(false),
      detail = "unreachable:https://worker.example.com"
    )

    val check = Main.bindingHealthCheck(requiredBinding, requiredInProd = true).get

    assertEquals(check.required, true)
    assertEquals(check.ok, false)
    assert(check.detail.contains("unreachable"))
