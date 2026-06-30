package lila.mailer

import play.api.i18n.Lang
import scala.annotation.unused

import lila.core.config.BaseUrl

final class AutomaticEmail(
    userApi: lila.core.user.UserApi,
    mailer: Mailer,
    baseUrl: BaseUrl
)(using Executor):

  import Mailer.html.*

  val regards = """Regards,

The Chesstory team"""

  def welcomeEmail(user: User, email: EmailAddress)(using @unused lang: Lang): Funit =
    mailer.canSend.so:
      lila.mon.email.send.welcome.increment()
      val profileUrl = s"$baseUrl/@/${user.username}"
      val editUrl = s"$baseUrl/account/profile"
      mailer.sendOrSkip:
        Mailer.Message(
          to = email,
          subject = s"Welcome to Chesstory, ${user.username}!",
          text = Mailer.txt.addServiceNote(s"Your profile: $profileUrl\nEdit your profile: $editUrl"),
          htmlBody = standardEmail(
            s"Your profile: $profileUrl\nEdit your profile: $editUrl"
          ).some
        )

  def signupConfirm(user: User, email: EmailAddress, url: String): Funit =
    mailer.canSend.so:
      val body =
        s"""Hello ${user.username.value},
           |
           |Confirm your Chesstory account by opening this link:
           |$url
           |
           |If you did not create this account, you can ignore this email.
           |
           |$regards
           |""".stripMargin
      mailer.sendOrSkip:
        Mailer.Message(
          to = email,
          subject = "Confirm your Chesstory account",
          text = Mailer.txt.addServiceNote(body),
          htmlBody = standardEmail(body).some
        )

  def emailChangeConfirm(user: User, email: EmailAddress, url: String): Funit =
    mailer.canSend.so:
      val body =
        s"""Hello ${user.username.value},
           |
           |Confirm your new Chesstory email address by opening this link:
           |$url
           |
           |If you did not request this change, you can ignore this email.
           |
           |$regards
           |""".stripMargin
      mailer.sendOrSkip:
        Mailer.Message(
          to = email,
          subject = "Confirm your new Chesstory email",
          text = Mailer.txt.addServiceNote(body),
          htmlBody = standardEmail(body).some
        )

  def passwordReset(user: User, email: EmailAddress, url: String): Funit =
    mailer.canSend.so:
      val body =
        s"""Hello ${user.username.value},
           |
           |Use this link to reset your Chesstory password:
           |$url
           |
           |If you did not request this, you can ignore this email.
           |
           |$regards
           |""".stripMargin
      mailer.sendOrSkip:
        Mailer.Message(
          to = email,
          subject = "Reset your Chesstory password",
          text = Mailer.txt.addServiceNote(body),
          htmlBody = standardEmail(body).some
        )

  def delete(user: User): Funit =
    val body =
      s"""Hello,

Following your request, the Chesstory account "${user.username}" will be deleted in 7 days from now.

$regards
"""
    userApi.email(user.id).flatMapz { email =>
      mailer.sendOrSkip:
        Mailer.Message(
          to = email,
          subject = "Chesstory account deletion",
          text = Mailer.txt.addServiceNote(body),
          htmlBody = standardEmail(body).some
        )
    }
