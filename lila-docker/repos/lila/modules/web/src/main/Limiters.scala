package lila.web

import lila.core.net.IpAddress
import lila.memo.RateLimit

final class Limiters(using Executor, lila.core.config.RateLimit):

  val passwordLogin = RateLimit[IpAddress](credits = 20, duration = 10.minutes, key = "login.password.ip")
  val signup = RateLimit[IpAddress](credits = 15, duration = 1.hour, key = "signup.web.ip")
  val passwordResetRequest = RateLimit[IpAddress](credits = 10, duration = 1.hour, key = "password.reset.request.ip")
  val moveMeaning = RateLimit[IpAddress](credits = 60, duration = 1.minute, key = "analyse.moveMeaning.ip")
