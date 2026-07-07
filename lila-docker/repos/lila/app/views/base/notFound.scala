package views.base

import lila.app.UiEnv.*

def notFound(msg: Option[String]) =
  Page(msg | "Page not found").css("bits.not-found"):
    main(cls := "not-found page-small box box-pad")(
      header(
        h1("404"),
        div(
          strong("Page not found!"),
          msg.map(em(_)),
          p(cls := "text")(
            "Page not found",
            br,
            a(href := routes.UserAnalysis.index.url)("the homepage"),
            br,
            a(href := routes.Main.contact.url)("Contact")
          )
        )
      )
    )

def notFoundEmbed(msg: Option[String])(using EmbedContext) =
  views.base.embed.site(title = msg | "Page not found", cssKeys = List("bits.not-found"))(
    main(cls := "not-found page-small box box-pad")(
      header(
        h1("404"),
        div(
          strong("Page not found!"),
          msg.map(em(_))
        )
      )
    )
  )
