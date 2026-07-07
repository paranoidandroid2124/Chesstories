package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object openSource:
  def apply()(using @unused ctx: Context): Page =
    Page("Open Source - Chesstory")
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content")(
              header(cls := "legal-header")(
                h1("Open Source Notice"),
                p(cls := "legal-meta")("Chesstory includes software licensed under GNU AGPL v3.")
              ),
              st.section(cls := "legal-section")(
                h2("Source Code Availability"),
                p(
                  "This service includes software licensed under GNU AGPL v3. ",
                  "If you interact with this network service, you can access the corresponding source code and license details at:"
                ),
                ul(
                  li(
                    a(
                      href := "https://github.com/paranoidandroid2124/chesstory",
                      target := "_blank",
                      rel := "noopener"
                    )("Chesstory Corresponding Source Code")
                  ),
                  li(
                    a(
                      href := "https://www.gnu.org/licenses/agpl-3.0.html#section13",
                      target := "_blank",
                      rel := "noopener"
                    )("GNU AGPL v3 Section 13")
                  ),
                  li(a(href := "https://lichess.org/source", target := "_blank", rel := "noopener")("Upstream Lichess Source & Licensing")),
                  li(
                    a(
                      href := "https://raw.githubusercontent.com/paranoidandroid2124/chesstory/master/lila-docker/repos/lila/COPYING.md",
                      target := "_blank",
                      rel := "noopener"
                    )("Chesstory COPYING (asset licenses and attributions)")
                  )
                )
              ),
              st.section(cls := "legal-section")(
                h2("Branding And Assets"),
                p(
                  "Some third-party or upstream assets may have additional restrictions. ",
                  "See the COPYING file for full details."
                )
              ),
              st.section(cls := "legal-section")(
                h2("Chess Engine And Analysis Components"),
                p(
                  "Chesstory uses browser-side chess engine and analysis components, including Stockfish-related packages and WebAssembly assets listed in the source repository package manifests."
                ),
                ul(
                  li(a(href := "https://stockfishchess.org/", target := "_blank", rel := "noopener")("Stockfish")),
                  li(a(href := "https://github.com/official-stockfish/Stockfish", target := "_blank", rel := "noopener")("Official Stockfish source")),
                  li(a(href := "https://github.com/lichess-org/stockfish-web", target := "_blank", rel := "noopener")("stockfish-web package source"))
                )
              ),
              footer(cls := "legal-footer")(
                a(href := routes.Main.privacy.url, cls := "legal-link")("Privacy Policy"),
                " • ",
                a(href := routes.Main.terms.url, cls := "legal-link")("Terms of Service"),
                " • ",
                a(href := routes.Main.contact.url, cls := "legal-link")("Contact"),
                " • ",
                a(href := homeUrl, cls := "legal-link")("Back to Home")
              )
            )
          )
        )
