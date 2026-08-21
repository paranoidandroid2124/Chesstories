package lila.web

import lila.core.net.IpAddress
import lila.memo.RateLimit

final class Limiters(using Executor, lila.core.config.RateLimit):

  val passwordLogin = RateLimit[IpAddress](credits = 20, duration = 10.minutes, key = "login.password.ip", log = false)
  val signup = RateLimit[IpAddress](credits = 15, duration = 1.hour, key = "signup.web.ip", log = false)
  val passwordResetRequest =
    RateLimit[IpAddress](credits = 10, duration = 1.hour, key = "password.reset.request.ip", log = false)
  val moveReviewJobCreation =
    RateLimit[IpAddress](credits = 4, duration = 1.minute, key = "analyse.moveReview.create.ip", log = false)
  val moveReviewJobAccess =
    RateLimit[IpAddress](credits = 240, duration = 1.minute, key = "analyse.moveReview.access.ip", log = false)
