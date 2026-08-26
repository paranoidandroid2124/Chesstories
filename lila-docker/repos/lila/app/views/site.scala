package views.site

import lila.app.UiEnv.*

object message:

  private def errorPage(code: String, title: String, msg: String) =
    Page(s"$title - Chesstory")
      .css("error")
      .flag(_.noHeader)
      .wrap: _ =>
        main(cls := "error-page")(
          div(cls := "error-container")(
            div(cls := "error-code")(code),
            h1(cls := "error-title")(title),
            p(cls := "error-message")(msg),
            div(cls := "error-actions")(
              a(href := routes.Main.landing.url, cls := "error-primary")("Back to home"),
              a(href := routes.UserAnalysis.index.url, cls := "error-secondary")("Open review board")
            )
          )
        )

  def notFound(msg: Option[String] = None) =
    errorPage(
      "404",
      "Page Not Found",
      msg.getOrElse("The page you're looking for doesn't exist or has been moved.")
    )

  def serverError(msg: String) =
    errorPage("500", "Something went wrong", s"We encountered an internal error. $msg")

  def rateLimited(msg: Option[String] = None) =
    errorPage(
      "429",
      "Too Many Requests",
      "You've made too many requests in a short time. " + msg.getOrElse("Please slow down.")
    )

  def authFailed =
    errorPage("403", "Access Denied", "You don't have permission to access this page.")
