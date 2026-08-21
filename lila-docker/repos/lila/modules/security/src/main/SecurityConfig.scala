package lila.security

import com.softwaremill.macwire.*
import play.api.ConfigLoader

import lila.common.autoconfig.{ *, given }
import lila.common.config.{ *, given }
import lila.core.config.*

import SecurityConfig.*
import scalalib.newtypes.TotalWrapper

opaque type LameNameCheck = Boolean
object LameNameCheck extends TotalWrapper[LameNameCheck, Boolean]

@Module
final private class SecurityConfig(
    val collection: Collection,
    @ConfigName("password_reset.secret") val passwordResetSecret: Secret,
    @ConfigName("email_change.secret") val emailChangeSecret: Secret,
    @ConfigName("login_token.secret") val loginTokenSecret: Secret,
    @ConfigName("hcaptcha") val hcaptcha: Hcaptcha.Config,
    @ConfigName("ip2proxy") val ip2Proxy: Ip2Proxy,
    @ConfigName("lame_name_check") val lameNameCheck: LameNameCheck,
    @ConfigName("pwned.range_url") val pwnedRangeUrl: String,
    @ConfigName("proxyscrape.url") val proxyscrapeUrl: String,
    @ConfigName("password.bpass.secret") val passwordBPassSecret: Secret
):
    def proxyMultiplier(v: Int): Int = v

private object SecurityConfig:

  case class Collection(
      security: CollName,
      firewall: CollName
  )
  given ConfigLoader[Collection] = AutoConfig.loader

  case class Ip2Proxy(
      enabled: Boolean,
      url: String
  )
  given ConfigLoader[Ip2Proxy] = AutoConfig.loader

  given ConfigLoader[LameNameCheck] = boolLoader(LameNameCheck.apply)

  given ConfigLoader[SecurityConfig] = AutoConfig.loader
