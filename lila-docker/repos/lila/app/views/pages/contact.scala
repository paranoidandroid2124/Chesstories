package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object contact:
  private def emailLink(address: String) = a(href := s"mailto:$address")(address)

  def apply(contactEmail: Option[String])(using @unused ctx: Context): Page =
    Page("Contact - Chesstory")
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content")(
              header(cls := "legal-header")(
                h1("How can we help?"),
                p(cls := "legal-meta")("Chesstory is currently run as a personal beta project by an individual developer based in the Republic of Korea.")
              ),

              st.section(cls := "legal-section")(
                h2("Get in touch"),
                contactEmail.fold[Frag](
                  frag(
                    p("Use the request form for product questions, problems, and privacy requests. Leave a reply email if you would like us to follow up."),
                    div(cls := "legal-actions")(
                      a(href := routes.BetaFeedback.formPage(surface = "general", feature = "contact", entrypoint = "contact_page", returnTo = routes.Main.contact.url, notify = false).url, cls := "button")("Send a request"),
                      a(href := routes.Main.help.url, cls := "button button-empty")("Browse help")
                    )
                  )
                )(email =>
                  p(
                    "General support, product questions, privacy requests, and legal notices can be sent to ",
                    emailLink(email),
                    "."
                  )
                )
              ),

              st.section(cls := "legal-section")(
                h2("What to include"),
                ul(
                  li("Your Chesstory username, if you have one"),
                  li("The email address connected to your account"),
                  li("A short description of your question or request"),
                  li("If this is a deletion or privacy request, enough detail for us to verify and process it safely")
                )
              ),

              st.section(cls := "legal-section")(
                h2("Questions and requests"),
                ul(
                  li("Account recovery or account security issues"),
                  li("Privacy requests such as access, correction, or deletion"),
                  li("Bug reports, support questions, and feedback"),
                  li("Legal notices or rights-related questions")
                )
              ),

              st.section(cls := "legal-section")(
                h2("Related policies"),
                p(
                  "For more detail about how Chesstory handles personal information, read the ",
                  a(href := routes.Main.privacy.url)("Privacy Policy"),
                  ". For the rules that govern use of the Service, read the ",
                  a(href := routes.Main.terms.url)("Terms of Service"),
                  "."
                )
              ),

              footer(cls := "legal-footer")(
                a(href := homeUrl, cls := "legal-link")("Back to Home")
              )
            )
          )
        )
