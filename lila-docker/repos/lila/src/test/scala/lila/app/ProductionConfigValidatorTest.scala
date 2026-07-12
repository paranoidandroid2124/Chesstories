package lila.app

import com.typesafe.config.ConfigFactory
import play.api.{ Configuration, Mode }

class ProductionConfigValidatorTest extends munit.FunSuite:

  test("valid production configuration passes validation"):
    ProductionConfigValidator.validate(validProdConfig, Mode.Prod)

  test("production validation rejects placeholder secrets and mock mail"):
    val badConfig = Configuration(
      ConfigFactory.parseString("""
        net.domain = "chesstory.com"
        net.base_url = "https://chesstory.com"
        net.email = "contact@chesstory.com"
        play.http.secret.key = "play-http-secret"
        user.password.bpass.secret = "bpass-secret"
        security.password_reset.secret = "???"
        security.email_confirm.secret = "???"
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
    )

    val err = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(badConfig, Mode.Prod)

    assert(err.getMessage.contains("security.password_reset.secret"))
    assert(err.getMessage.contains("mailer.primary.mock"))
    assert(err.getMessage.contains("mailer.primary.sender"))

  test("production validation rejects hcaptcha test keys when captcha is enabled"):
    val badCaptcha = Configuration(
      ConfigFactory.parseString("""
        net.domain = "chesstory.com"
        net.base_url = "https://chesstory.com"
        net.email = "contact@chesstory.com"
        play.http.secret.key = "play-http-secret"
        user.password.bpass.secret = "bpass-secret"
        security.email_confirm.enabled = true
        security.password_reset.secret = "reset-secret"
        security.email_confirm.secret = "confirm-secret"
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
    )

    val err = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(badCaptcha, Mode.Prod)

    assert(err.getMessage.contains("security.hcaptcha.secret"))
    assert(err.getMessage.contains("security.hcaptcha.public.sitekey"))

  test("production validation rejects disabled signup protections"):
    val weakConfig = Configuration(
      ConfigFactory.parseString("""
        net.domain = "chesstory.com"
        net.base_url = "https://chesstory.com"
        net.email = "contact@chesstory.com"
        play.http.secret.key = "play-http-secret"
        user.password.bpass.secret = "bpass-secret"
        security.email_confirm.enabled = false
        security.password_reset.secret = "reset-secret"
        security.email_confirm.secret = "confirm-secret"
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
    )

    val err = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(weakConfig, Mode.Prod)

    assert(err.getMessage.contains("security.email_confirm.enabled"))
    assert(err.getMessage.contains("security.hcaptcha.enabled"))

  test("non-production mode skips production validation"):
    val devConfig = Configuration(ConfigFactory.parseString("""net.domain = "localhost:9663""""))
    ProductionConfigValidator.validate(devConfig, Mode.Dev)

  test("production validation requires a public contact email"):
    val missingContact = Configuration(
      ConfigFactory.parseString("""
        net.domain = "chesstory.com"
        net.base_url = "https://chesstory.com"
        security.password_reset.secret = "reset-secret"
        security.email_confirm.secret = "confirm-secret"
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
    )

    val err = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(missingContact, Mode.Prod)

    assert(err.getMessage.contains("net.email"))

  test("production validation rejects public framework and password-pepper defaults"):
    val weakSecrets = Configuration(
      ConfigFactory.parseString("""
        net.domain = "chesstory.com"
        net.base_url = "https://chesstory.com"
        net.email = "contact@chesstory.com"
        play.http.secret.key = "CiebwjgIM9cHQ;I?Xk:sfqDJ;BhIe:jsL?r=?IPF[saf>s^r0]?0grUq4>q?5mP^"
        user.password.bpass.secret = "9qEYN0ThHer1KWLNekA76Q=="
        security.email_confirm.enabled = true
        security.password_reset.secret = "reset-secret"
        security.email_confirm.secret = "confirm-secret"
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

    val err = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(weakSecrets, Mode.Prod)

    assert(err.getMessage.contains("play.http.secret.key"))
    assert(err.getMessage.contains("user.password.bpass.secret"))

  test("production validation rejects dormant push bindings"):
    val badBindings = Configuration(
      ConfigFactory.parseString("""
        net.domain = "chesstory.com"
        net.base_url = "https://chesstory.com"
        net.email = "contact@chesstory.com"
        play.http.secret.key = "play-http-secret"
        user.password.bpass.secret = "bpass-secret"
        security.email_confirm.enabled = true
        security.password_reset.secret = "reset-secret"
        security.email_confirm.secret = "confirm-secret"
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
    )

    val err = intercept[IllegalStateException]:
      ProductionConfigValidator.validate(badBindings, Mode.Prod)

    assert(err.getMessage.contains("push.web.url"))
    assert(err.getMessage.contains("push.web.vapid_public_key"))

  private val validProdConfig = Configuration(
    ConfigFactory.parseString("""
      net.domain = "chesstory.com"
      net.base_url = "https://chesstory.com"
      net.email = "contact@chesstory.com"
      play.http.secret.key = "play-http-secret"
      user.password.bpass.secret = "bpass-secret"
      security.email_confirm.enabled = true
      security.password_reset.secret = "reset-secret"
      security.email_confirm.secret = "confirm-secret"
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
