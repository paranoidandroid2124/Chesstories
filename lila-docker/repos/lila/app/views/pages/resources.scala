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
            ("Analyze a game", "Paste a score or start from the board.", routes.UserAnalysis.index.url),
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
              ". A public status page will be added with production monitoring."
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
  private def row(feature: String, beta: String, paid: String) =
    tr(th(attr("scope") := "row")(feature), td(beta), td(paid))

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
      ),
      st.section(cls := "legal-section")(
        h2("Feature comparison"),
        div(
          cls := "legal-table-wrap",
          role := "region",
          attr("tabindex") := "0",
          attr("aria-label") := "Pricing feature comparison"
        )(
          table(cls := "legal-comparison")(
            thead(tr(th("Feature"), th("Free beta"), th("Paid plans"))),
            tbody(
              row("Board and PGN review", "Available", "Not sold separately"),
              row("Engine evidence", "Score, depth, and candidate lines", "Same board evidence"),
              row("Saved studies", "Available when signed in", "Limits will be published before launch"),
              row("Billing", "No card, renewal, or charge", "Terms shown before any charge"),
              row("Refunds", "No charge, so no refund process", "Policy shown before checkout")
            )
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Before any paid plan"),
        p(
          "Prices, review limits, billing interval, renewal terms, cancellation, and refund terms will be published before a subscription is offered."
        ),
        resourcePageBits.actionRow(
          List(
            (
              "Read beta feedback",
              routes.BetaFeedback
                .formPage(
                  "general",
                  "paid_plan_waitlist",
                  "pricing_page",
                  routes.Main.pricing.url,
                  notify = true
                )
                .url,
              "button button-empty"
            ),
            ("Read trust details", routes.Main.trust.url, "button button-empty")
          )
        )
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

object examples:
  private def sample(
      title: String,
      players: String,
      result: String,
      opening: String,
      topic: String,
      length: String,
      fen: String
  ): Frag =
    st.article(cls := "legal-example")(
      chessgroundMini(fen, chess.White, None)(
        div(cls := "legal-example__board", role := "img", aria.label := s"Sample board for $title")
      ),
      div(cls := "legal-example__copy")(
        h3(title),
        dl(
          div(dt("Players"), dd(players)),
          div(dt("Result"), dd(result)),
          div(dt("Opening"), dd(opening)),
          div(dt("Topic"), dd(topic)),
          div(dt("Length"), dd(length))
        ),
        p("Open the board to inspect the position before reviewing your own game."),
        a(href := routes.UserAnalysis.index.url)("Open analysis board")
      )
    )

  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Examples - Chesstory",
      "Review examples",
      "Sample positions with game details and a question to study."
    )(
      st.section(cls := "legal-section")(
        h2("Positions worth reviewing"),
        div(cls := "legal-example-list")(
          sample(
            "Opening transition",
            "White vs Black",
            "1–0",
            "Italian Game",
            "Prepare the central break",
            "22 moves",
            "r1bq1rk1/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQ1RK1 w - - 4 6"
          ),
          sample(
            "Middlegame plan",
            "White vs Black",
            "½–½",
            "Queen's Gambit Declined",
            "Improve the worst piece before changing the structure",
            "38 moves",
            "r2q1rk1/pp1nbppp/2p1pn2/3p4/3P4/2NBPN2/PPQ2PPP/R3K2R w KQ - 3 9"
          ),
          sample(
            "Critical decision",
            "White vs Black",
            "0–1",
            "Sicilian Defence",
            "Compare a tactical move with the safer resource",
            "29 moves",
            "r1bq1rk1/pp3ppp/2nbpn2/3p4/3P4/2NBPN2/PPQ2PPP/R3K2R w KQ - 4 9"
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Use an example as a checklist"),
        resourcePageBits.factRows(
          List(
            "Board" -> frag("Keep the move and the resulting position together."),
            "Metadata" -> frag(
              "Record player, result, opening, topic, and length before interpreting the game."
            ),
            "Preview" -> frag("Open the board before deciding whether the position deserves a saved study.")
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
