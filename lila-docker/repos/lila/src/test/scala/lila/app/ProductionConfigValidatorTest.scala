package lila.app

import com.typesafe.config.ConfigFactory
import play.api.{ Configuration, Mode }

class ProductionConfigValidatorTest extends munit.FunSuite:

  private val validProdConfig = Configuration(
    ConfigFactory.parseString("""
      net.domain = "chesstory.com"
      net.base_url = "https://chesstory.com"
      net.email = "contact@chesstory.com"
      play.http.secret.key = "play-http-secret"
      user.password.bpass.secret = "bpass-secret"
      security.password_reset.secret = "reset-secret"
      security.email_change.secret = "change-secret"
      security.login_token.secret = "token-secret"
      auth.magicLink.autoCreate = false
      mailer.primary.mock = false
      mailer.primary.host = "smtp.postmarkapp.com"
      mailer.primary.port = 587
      mailer.primary.tls = true
      mailer.primary.user = "smtp-user"
      mailer.primary.password = "smtp-pass"
      mailer.primary.sender = "Chesstory <noreply@chesstory.com>"
      security.hcaptcha.enabled = true
      security.hcaptcha.secret = "captcha-secret"
      security.hcaptcha.public.sitekey = "captcha-sitekey"
    """)
  )

  test("valid production configuration passes validation"):
    ProductionConfigValidator.validate(validProdConfig, Mode.Prod)

  test("production validation keeps move review dark"):
    val runtimeConfig = Configuration(
      ConfigFactory
        .parseString("""chesstory.moveReview.mode = "runtime"""")
        .withFallback(validProdConfig.underlying)
    )
    val error = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(runtimeConfig, Mode.Prod)
    assert(error.getMessage.contains("chesstory.moveReview.mode"))

  test("production validation rejects invalid configurations"):
    List(
      (
        Configuration(
          ConfigFactory.parseString("""
            net.domain = "chesstory.com"
            net.base_url = "https://chesstory.com"
            net.email = "contact@chesstory.com"
            play.http.secret.key = "play-http-secret"
            user.password.bpass.secret = "bpass-secret"
            security.password_reset.secret = "???"
            security.email_change.secret = "???"
            security.login_token.secret = "???"
            mailer.primary.mock = true
            mailer.primary.host = "mailpit"
            mailer.primary.port = 1025
            mailer.primary.tls = false
            mailer.primary.user = "???"
            mailer.primary.password = "???"
            mailer.primary.sender = "lichess.org <noreply@lichess.org>"
          """)
        ),
        List("security.password_reset.secret", "mailer.primary.mock", "mailer.primary.sender")
      ),
      (
        Configuration(
          ConfigFactory.parseString("""
            net.domain = "chesstory.com"
            net.base_url = "https://chesstory.com"
            net.email = "contact@chesstory.com"
            play.http.secret.key = "play-http-secret"
            user.password.bpass.secret = "bpass-secret"
            security.password_reset.secret = "reset-secret"
            security.email_change.secret = "change-secret"
            security.login_token.secret = "token-secret"
            auth.magicLink.autoCreate = false
            mailer.primary.mock = false
            mailer.primary.host = "smtp.postmarkapp.com"
            mailer.primary.port = 587
            mailer.primary.tls = true
            mailer.primary.user = "smtp-user"
            mailer.primary.password = "smtp-pass"
            mailer.primary.sender = "Chesstory <noreply@chesstory.com>"
            security.hcaptcha.enabled = true
            security.hcaptcha.secret = "dummy_secret"
            security.hcaptcha.public.sitekey = "10000000-ffff-ffff-ffff-000000000001"
          """)
        ),
        List("security.hcaptcha.secret", "security.hcaptcha.public.sitekey")
      ),
      (
        Configuration(
          ConfigFactory.parseString("""
            net.domain = "chesstory.com"
            net.base_url = "https://chesstory.com"
            net.email = "contact@chesstory.com"
            play.http.secret.key = "play-http-secret"
            user.password.bpass.secret = "bpass-secret"
            security.password_reset.secret = "reset-secret"
            security.email_change.secret = "change-secret"
            security.login_token.secret = "token-secret"
            auth.magicLink.autoCreate = true
            mailer.primary.mock = false
            mailer.primary.host = "smtp.postmarkapp.com"
            mailer.primary.port = 587
            mailer.primary.tls = true
            mailer.primary.user = "smtp-user"
            mailer.primary.password = "smtp-pass"
            mailer.primary.sender = "Chesstory <noreply@chesstory.com>"
            security.hcaptcha.enabled = false
            security.hcaptcha.secret = "captcha-secret"
            security.hcaptcha.public.sitekey = "captcha-sitekey"
          """)
        ),
        List("security.hcaptcha.enabled")
      ),
      (
        Configuration(
          ConfigFactory.parseString("""
            net.domain = "chesstory.com"
            net.base_url = "https://chesstory.com"
            security.password_reset.secret = "reset-secret"
            security.email_change.secret = "change-secret"
            security.login_token.secret = "token-secret"
            play.http.secret.key = "play-http-secret"
            user.password.bpass.secret = "bpass-secret"
            mailer.primary.mock = false
            mailer.primary.host = "smtp.postmarkapp.com"
            mailer.primary.port = 587
            mailer.primary.tls = true
            mailer.primary.user = "smtp-user"
            mailer.primary.password = "smtp-pass"
            mailer.primary.sender = "Chesstory <noreply@chesstory.com>"
          """)
        ),
        List("net.email")
      ),
      (
        Configuration(
          ConfigFactory.parseString("""
            net.domain = "chesstory.com"
            net.base_url = "https://chesstory.com"
            net.email = "contact@chesstory.com"
            play.http.secret.key = "CiebwjgIM9cHQ;I?Xk:sfqDJ;BhIe:jsL?r=?IPF[saf>s^r0]?0grUq4>q?5mP^"
            user.password.bpass.secret = "9qEYN0ThHer1KWLNekA76Q=="
            security.password_reset.secret = "reset-secret"
            security.email_change.secret = "change-secret"
            security.login_token.secret = "token-secret"
            auth.magicLink.autoCreate = false
            mailer.primary.mock = false
            mailer.primary.host = "smtp.postmarkapp.com"
            mailer.primary.port = 587
            mailer.primary.tls = true
            mailer.primary.user = "smtp-user"
            mailer.primary.password = "smtp-pass"
            mailer.primary.sender = "Chesstory <noreply@chesstory.com>"
            security.hcaptcha.enabled = true
            security.hcaptcha.secret = "captcha-secret"
            security.hcaptcha.public.sitekey = "captcha-sitekey"
          """)
        ),
        List("play.http.secret.key", "user.password.bpass.secret")
      ),
      (
        Configuration(
          ConfigFactory.parseString("""
            net.domain = "chesstory.com"
            net.base_url = "https://chesstory.com"
            net.email = "contact@chesstory.com"
            play.http.secret.key = "play-http-secret"
            user.password.bpass.secret = "bpass-secret"
            security.password_reset.secret = "reset-secret"
            security.email_change.secret = "change-secret"
            security.login_token.secret = "token-secret"
            auth.magicLink.autoCreate = false
            mailer.primary.mock = false
            mailer.primary.host = "smtp.postmarkapp.com"
            mailer.primary.port = 587
            mailer.primary.tls = true
            mailer.primary.user = "smtp-user"
            mailer.primary.password = "smtp-pass"
            mailer.primary.sender = "Chesstory <noreply@chesstory.com>"
            security.hcaptcha.enabled = true
            security.hcaptcha.secret = "captcha-secret"
            security.hcaptcha.public.sitekey = "captcha-sitekey"
            push.web.url = "https://push.example.com"
            push.web.vapid_public_key = "vapid-key"
          """)
        ),
        List("push.web.url", "push.web.vapid_public_key")
      )
    ).foreach { case (config, expectedErrors) =>
      val err = intercept[IllegalStateException]:
        ProductionConfigValidator.validate(config, Mode.Prod)
      expectedErrors.foreach { path => assert(err.getMessage.contains(path)) }
    }

  test("non-production mode skips production validation"):
    val devConfig = Configuration(ConfigFactory.parseString("""net.domain = "localhost:9663""""))
    ProductionConfigValidator.validate(devConfig, Mode.Dev)
