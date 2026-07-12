package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

private object resourcePageBits:
  def shell(pageTitle: String, heading: String, meta: String)(body: Frag*)(using @unused ctx: Context): Page =
    Page(pageTitle)
      .css("legal")
      .wrap: _ =>
        val articleBody = List[Frag](
          header(cls := "legal-header")(
            h1(heading),
            p(cls := "legal-meta")(meta)
          )
        ) ++ body.toList ++ List[Frag](
          footer(cls := "legal-footer")(
            a(href := homeUrl, cls := "legal-link")("Back to Home")
          )
        )
        main(cls := "legal-page legal-page--resource")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content")(articleBody*)
          )
        )

  def linkCards(items: List[(String, String, String)]): Frag =
    div(cls := "legal-card-grid")(
      items.map { (title, copy, url) =>
        a(href := url, cls := "legal-card-link")(
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

object help:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Help - Chesstory",
      "Help",
      "A short guide to starting a review, understanding the output, and sharing the result."
    )(
      st.section(cls := "legal-section")(
        h2("Start here"),
        p(
          "Chesstory is built for one serious review at a time. Paste a PGN, import a public game, inspect the board, then turn the work into a review study."
        ),
        resourcePageBits.actionRow(
          List(
            ("Analyze a game", routes.UserAnalysis.index.url, "button"),
            ("Import public game", routes.Importer.importGame.url, "button button-empty")
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Common questions"),
        h3("What can I import?"),
        p("You can paste a PGN directly or bring recent public games from supported public sources such as Lichess and Chess.com."),
        h3("What does Stockfish do here?"),
        p("Stockfish stays visible as objective evidence: evaluation, depth, and candidate lines. Chesstory's job is to explain the strategic logic around that evidence."),
        h3("How is this different from normal engine analysis?"),
        p("Normal engine analysis often stops at best moves and numbers. Chesstory tries to explain the question of the position, the effect of the move, and the plan a human should remember."),
        h3("Does Chesstory replace a coach?"),
        p("No. It is a review tool for self-study. A coach can still see habits, psychology, and tournament context that a single-game review may miss."),
        h3("What rating level is it for?"),
        p("The first beta is aimed at serious improvers who already use engines but struggle to turn engine lines into plans, especially around opening-to-middlegame positions."),
        h3("What is a review study?"),
        p("A review study keeps the board, move tree, notes, and explanation together so you can return to the game or share the exact section link."),
        h3("Can I use Chesstory without signing in?"),
        p("You can try the core flow without a full account, but saving and sharing review studies requires sign-in."),
        h3("Can I delete my data?"),
        p("Account and study data are covered by the privacy policy. For deletion or privacy questions, use the contact page while the beta tools are still being finalized.")
      ),
      st.section(cls := "legal-section")(
        h2("More resources"),
        resourcePageBits.linkCards(
          List(
            ("Examples", "See sample reviews Chesstory is aiming for.", routes.Main.examples.url),
            ("Journal", "Read product notes and thinking behind Chesstory.", routes.Main.journal.url),
            ("Beta Access / Pricing", "Check current beta access before subscriptions exist.", routes.Main.pricing.url),
            ("Trust", "Understand privacy, AI handling, and open-source obligations.", routes.Main.trust.url),
            ("Roadmap", "See what is available now and what comes next.", routes.Main.roadmap.url)
          )
        )
      )
    )

object pricing:
  private def planCard(name: String, badge: String, copy: String, bullets: List[String], featured: Boolean = false): Frag =
    val klass = if featured then "legal-card-link legal-plan-card legal-plan-card--featured" else "legal-card-link legal-plan-card"
    div(cls := klass)(
      span(cls := "legal-plan-card__badge")(badge),
      strong(name),
      span(copy),
      ul(
        bullets.map { bullet =>
          li(bullet)
        }*
      )
    )

  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Beta Access / Pricing - Chesstory",
      "Beta Access / Pricing",
      "Chesstory is currently an open beta. Paid plans are not active yet."
    )(
      st.section(cls := "legal-section")(
        h2("Current access"),
        p(
          "The current beta focuses on making one-game review useful before adding subscriptions. The Stockfish line stays visible; Chesstory explains what the position is asking."
        ),
        ul(
          li("Paste or import one game and review it on the analysis board within current beta limits."),
          li("See the Stockfish score and top candidate moves while reviewing the board."),
          li("Get a plain-language read of what inspected moves are trying to do."),
          li("Use Coach explanations on key moments during the beta."),
          li("Create review studies when signed in."),
          li("No credit card is required for the current beta.")
        ),
        resourcePageBits.actionRow(
          List(
            ("Start a review", routes.UserAnalysis.index.url, "button"),
            ("Join paid-plan waitlist", routes.BetaFeedback.formPage(
              surface = "general",
              feature = "paid_plan_waitlist",
              entrypoint = "pricing_page",
              returnTo = routes.Main.pricing.url,
              notify = true
            ).url, "button button-empty")
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("How plans should differ"),
        p(
          "Free review already explains the position's main question. Paid plans mainly add deeper coach explanations and more room to ask follow-up questions."
        ),
        div(cls := "legal-card-grid legal-plan-grid")(
          planCard(
            "Free beta",
            "Current",
            "A real post-game review before any subscription.",
            List(
              "One serious game review per day during beta",
              "Stockfish score and top candidate moves",
              "Plain-language read for moves you inspect",
              "Coach explanation for key moments within beta limits",
              "Review study creation when signed in"
            ),
            featured = true
          ),
          planCard(
            "Plus",
            "Planned",
            "For players who want coach-style explanations whenever they study.",
            List(
              "Explain this move on more positions",
              "Coach explanations for all key moments in a game",
              "Follow-up questions on the current position",
              "More daily game reviews",
              "Clear saved review studies for sharing"
            )
          ),
          planCard(
            "Pro",
            "Planned",
            "For deeper study, preparation, and repeated review work.",
            List(
              "More room for deep review sessions",
              "Broader commentary across opening and middlegame phases",
              "Multiple-game weakness summaries when available",
              "Richer export and sharing workflow",
              "Priority feedback during beta"
            )
          )
        ),
        p(cls := "legal-note")(
          "Exact prices, review limits, renewal terms, refunds, and billing rules will be published before paid subscriptions are opened."
        )
      ),
      st.section(cls := "legal-section")(
        h2("Before paid plans"),
        p("We will publish pricing, renewal terms, refund terms, and any usage limits before charging for a subscription."),
        p("Current beta limits may change while we learn how much review depth players need. Changes should be announced before they affect normal use.")
      )
    )

object trust:
  def apply(contactEmail: Option[String])(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Trust - Chesstory",
      "Trust",
      "How Chesstory handles privacy, AI data, open-source duties, and beta limits."
    )(
      st.section(cls := "legal-section")(
        h2("Data and AI"),
        p(
          "Chess positions, PGNs, study notes, and prompts may be sent to OpenAI API models when needed to produce AI-assisted explanations."
        ),
        p("Chesstory does not currently opt in to OpenAI training on API inputs or outputs, and it does not currently promise Zero Data Retention unless that control is approved and configured later."),
        p(
          "Chesstory should not be used for private personal data. Treat PGNs, notes, and comments as chess study material."
        ),
        p("Chesstory is not aimed at children under 13. If school, club, or youth-program features are added later, they need their own privacy and safety review first.")
      ),
      st.section(cls := "legal-section")(
        h2("Security and reliability"),
        ul(
          li("Authentication, cookie, CSRF, and deployment headers are security work required before public launch."),
          li("The public health endpoint is available for basic service readiness."),
          li("A dedicated status page will wait until there is real production monitoring to show.")
        )
      ),
      st.section(cls := "legal-section")(
        h2("Open source"),
        p(
          "Chesstory includes AGPL-licensed chess server software. The open-source page explains source-code obligations for the deployed frontend and server shell."
        ),
        resourcePageBits.linkCards(
          List(
            ("Privacy", "Read how account, study, and analysis data are handled.", routes.Main.privacy.url),
            ("Terms", "Read the service rules and beta limitations.", routes.Main.terms.url),
            ("Open Source", "Review AGPL and source-availability notes.", routes.Main.source.url)
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Contact"),
        p(
          "For security, privacy, or source-code availability questions, use the contact page",
          contactEmail.map(email => frag(" or email ", a(href := s"mailto:$email")(email))).getOrElse(emptyFrag),
          "."
        ),
        resourcePageBits.actionRow(List(("Contact", routes.Main.contact.url, "button button-empty")))
      )
    )

object examples:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Examples - Chesstory",
      "Review Shapes",
      "Sample reviews for players who want to see what Chesstory is trying to produce."
    )(
      st.section(cls := "legal-section")(
        h2("What a good review should contain"),
        p(
          "A useful review does not only mark mistakes. It explains the question of the position, the direct effect of the move, the best-move difference, and the lesson to carry forward."
        ),
        div(cls := "legal-example-list")(
          div(cls := "legal-example-card")(
            strong("Opening transition"),
            p("Question: did the quiet move prepare the central break, or only answer a threat that was not urgent yet?"),
            span("Review should show: move effect, best-vs-played, plan, and lesson.")
          ),
          div(cls := "legal-example-card")(
            strong("Middlegame plan"),
            p("Question: should you improve the worst piece, change the pawn structure, or stop the opponent's counterplay first?"),
            span("Review should show: candidate plans with the Stockfish line kept visible.")
          ),
          div(cls := "legal-example-card")(
            strong("Critical mistake"),
            p("Question: was the mistake tactical, strategic, or a plan that failed because of move order?"),
            span("Review should show: the losing idea, the safer alternative, and the transferable lesson.")
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Public sample studies"),
        p("Public sample reviews will be added as real studies once the review output stabilizes. Until then, these examples describe the review shape without pretending there are finished public case studies."),
        resourcePageBits.actionRow(List(("Open the board", routes.UserAnalysis.index.url, "button")))
      )
    )

object roadmap:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Roadmap - Chesstory",
      "Roadmap",
      "A simple beta roadmap without date promises."
    )(
      st.section(cls := "legal-section")(
        h2("Now"),
        ul(
          li("One-game deep review flow."),
          li("PGN paste and public game import."),
          li("Stockfish line plus plain-language explanation panels."),
          li("Review studies for saving and sharing work.")
        )
      ),
      st.section(cls := "legal-section")(
        h2("Next"),
        ul(
          li("Stronger example reviews and public sample studies."),
          li("Clearer help pages around import, Coach explanations, and study sharing."),
          li("Better post-game explanation quality as the review output improves.")
        )
      ),
      st.section(cls := "legal-section")(
        h2("Later"),
        ul(
          li("Player-level recurring weakness summaries."),
          li("Paid plans after usage limits, privacy terms, and billing terms are ready."),
          li("A real status page after production monitoring exists.")
        )
      )
    )
