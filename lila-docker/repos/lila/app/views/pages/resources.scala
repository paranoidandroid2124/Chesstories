package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

private object resourcePageBits:
  def shell(pageTitle: String, heading: String, meta: String)(body: Frag*)(using @unused ctx: Context): Page =
    Page(pageTitle)
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page legal-page--resource")(
          div(cls := "legal-container")(
            header(cls := "legal-header")(
              h1(heading),
              p(cls := "legal-meta")(meta)
            ),
            div(cls := "legal-content")(body*)
          )
        )

  def linkRows(items: List[(String, String, String)]): Frag =
    div(cls := "legal-link-rows")(
      items.map { (title, copy, url) =>
        a(href := url, cls := "legal-link-row")(
          strong(title),
          span(copy)
        )
      }*
    )

  def actionRow(actions: List[(String, String, String)]): Frag =
    div(cls := "legal-actions")(
      actions.map { (label, url, klass) =>
        a(href := url, cls := klass)(label)
      }*
    )

  def factRows(items: List[(String, Frag)]): Frag =
    dl(cls := "legal-facts")(
      items.map { (label, value) =>
        div(
          dt(label),
          dd(value)
        )
      }*
    )

object help:
  private val questions = List(
    "What can I import?" -> "Paste a PGN, choose a PGN file, or browse a public Lichess or Chess.com account.",
    "What does Stockfish do here?" -> "Evaluation, depth, and candidate lines stay beside the board as evidence for the move you are reviewing.",
    "What is a review study?" -> "A study keeps the board, move tree, notes, and saved sections together so you can reopen or share a specific chapter.",
    "Can I use Chesstory without signing in?" -> "You can open the core board flow without an account. Sign in to manage Studies you explicitly save; ordinary analysis and imports are not retained as history.",
    "Can I delete my data?" -> "Use the contact page for account, study, or privacy requests. The privacy policy describes the current handling of chess material."
  )

  def apply(query: Option[String] = None)(using @unused ctx: Context): Page =
    val term = query.map(_.trim).filter(_.nonEmpty)
    val matched = term.fold(questions): needle =>
      questions.filter { case (title, body) => s"$title $body".toLowerCase.contains(needle.toLowerCase) }
    val matchedItems: List[Frag] = matched.map { case (title, body) =>
      details(
        summary(title),
        p(body)
      )
    }
    resourcePageBits.shell(
      "Help - Chesstory",
      "Help",
      "Guidance for importing games, reviewing positions, studies, and account data."
    )(
      st.form(cls := "legal-search", role := "search", method := "get", action := routes.Main.help.url)(
        label(`for` := "help-query")("Search help"),
        div(
          input(
            id := "help-query",
            tpe := "search",
            name := "q",
            value := term | "",
            placeholder := "Import, engine, study, privacy"
          ),
          button(tpe := "submit", cls := "button button-thin")("Search")
        )
      ),
      term.map(value =>
        p(cls := "legal-status", role := "status", attr("aria-live") := "polite")(s"Results for “$value”")
      ),
      st.section(cls := "legal-section")(
        h2("Start a task"),
        resourcePageBits.linkRows(
          List(
            (
              "Review a game",
              "Paste a PGN and work through why key positions changed.",
              routes.UserAnalysis.index.url
            ),
            (
              "Import a game",
              "Paste PGN, choose a file, or browse a public account.",
              routes.Importer.importGame.url
            ),
            ("Browse studies", "Open public studies or return to your saved work.", "/study")
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Popular questions"),
        div(cls := "legal-questions")(
          if matched.nonEmpty then frag(matchedItems*)
          else p("No help topic matched that search. Try “import”, “engine”, “study”, or “privacy”.")
        )
      ),
      st.section(cls := "legal-section")(
        h2("Contact, service, and policy"),
        resourcePageBits.factRows(
          List(
            "Contact" -> frag(a(href := routes.Main.contact.url)("Ask a question or request help.")),
            "Service" -> frag(
              "The readiness endpoint is available at ",
              code("/healthz"),
              "."
            ),
            "Policies" -> frag(
              a(href := routes.Main.privacy.url)("Privacy"),
              " • ",
              a(href := routes.Main.terms.url)("Terms"),
              " • ",
              a(href := routes.Main.trust.url)("Trust")
            )
          )
        )
      )
    )

object pricing:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Beta Access / Pricing - Chesstory",
      "Beta access and pricing",
      "Chesstory is open beta. There is no active paid subscription or billing flow."
    )(
      st.section(cls := "legal-section")(
        h2("What is available now"),
        p(
          "The beta is for reviewing one game at a time: board, engine evidence, and saved study work when you are signed in."
        ),
        resourcePageBits.actionRow(List(("Start a review", routes.UserAnalysis.index.url, "button")))
      )
    )

object trust:
  def apply(contactEmail: Option[String])(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Trust - Chesstory",
      "Trust",
      "Facts about the engine, judgment path, chess data, and beta limits."
    )(
      st.section(cls := "legal-section")(
        h2("What the product uses"),
        resourcePageBits.factRows(
          List(
            "Engine reference" -> frag(
              "Browser-side Stockfish components provide evaluation and principal variations. The product shows depth and lines; it does not currently expose a fixed engine build identifier."
            ),
            "Judgment path" -> frag(
              "Current move facts and judgment are produced by the Chesstory engine and judgment path. This route does not use an external AI-model provider."
            ),
            "Review inputs" -> frag(
              "Current FEN, played move, engine variations and evaluations, move prefix, opening context, and bounded probe results when available."
            ),
            "External model processing" -> frag(
              "The current explanation route does not send review data to an external AI-model provider."
            ),
            "Your deletion request" -> frag(
              "Use ",
              a(href := routes.Main.contact.url)("contact"),
              " for account, study, or privacy requests; see the ",
              a(href := routes.Main.privacy.url)("privacy policy"),
              " for the current scope."
            ),
            "Limitations" -> frag(
              "Engine output and current judgment facts are study aids, not a coach, tournament arbiter, or guarantee of a correct human plan."
            )
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Service and changes"),
        resourcePageBits.factRows(
          List(
            "Service readiness" -> frag(
              "A health endpoint is available. A public monitored status page is not published yet."
            ),
            "Changes" -> frag(
              "Product changes are recorded in the ",
              a(href := routes.Main.journal.url)("journal"),
              " and planned work is listed on the ",
              a(href := routes.Main.roadmap.url)("roadmap"),
              "."
            ),
            "Source" -> frag(
              "Chesstory includes AGPL-licensed chess server software; see ",
              a(href := routes.Main.source.url)("open source"),
              " for source-availability details."
            ),
            "Contact" -> frag(
              "For privacy, security, or source questions, use ",
              a(href := routes.Main.contact.url)("contact"),
              contactEmail
                .map(email => frag(" or ", a(href := s"mailto:$email")(email)))
                .getOrElse(emptyFrag),
              "."
            )
          )
        )
      )
    )

private[pages] object ReviewExample:
  val fen = "r1bq1rk1/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQ1RK1 w - - 4 6"
  val move = "6. h3"
  val opening = "Italian Game"
  val line = "6. c3 d6 7. d4"
  val explanation =
    "6. h3 prevents ...Bg4, but it does not prepare the central break. " +
      "6. c3 supports d4 immediately; after 6...d6, 7. d4 challenges e5 with the break prepared."
  val lesson = "Prepare the support for a central break before playing the break itself."

object examples:

  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Explained example - Chesstory",
      "A move sequence, explained",
      "See how Chesstory connects the move under review, a reference line, and a reusable lesson."
    )(
      st.section(cls := "legal-section")(
        h2(s"${ReviewExample.opening}: prepare the central break"),
        div(cls := "legal-example-list")(
          st.article(cls := "legal-example")(
            chessgroundMini(ReviewExample.fen, chess.White, None)(
              div(
                cls := "legal-example__board",
                role := "img",
                aria.label := "Italian Game position after 5...O-O"
              )
            ),
            div(cls := "legal-example__copy")(
              h3("Why 6. c3 comes before 7. d4"),
              dl(
                div(dt("Move under review"), dd(ReviewExample.move)),
                div(dt("Reference line"), dd(ReviewExample.line))
              ),
              p(ReviewExample.explanation),
              p(strong("Lesson: "), ReviewExample.lesson),
              resourcePageBits.actionRow(
                List(("Review your own game", routes.UserAnalysis.index.url, "button"))
              )
            )
          )
        )
      )
    )

object roadmap:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Roadmap - Chesstory",
      "Roadmap",
      "A beta roadmap without date promises."
    )(
      st.section(cls := "legal-section")(
        h2("Now"),
        ul(
          li("One-game review flow."),
          li("PGN paste and public game import."),
          li("Stockfish evidence beside plain-language explanation."),
          li("Review studies for saving and sharing work.")
        )
      ),
      st.section(cls := "legal-section")(
        h2("Next"),
        ul(
          li("Clearer import, help, and study-sharing flows."),
          li("Additional public example reviews."),
          li("Iterate on explanation quality with beta feedback and review evidence.")
        )
      ),
      st.section(cls := "legal-section")(
        h2("Later"),
        ul(
          li("Player-level recurring weakness summaries."),
          li("Paid plans after limits and billing terms are ready."),
          li("A monitored public status page.")
        )
      )
    )
