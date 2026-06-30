package views.pages

import controllers.Main
import lila.analyse.ImportHistory
import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object home:

  private def reviewStudyVisibilityLabel(study: lila.study.Study): String =
    if study.isPublic then "Public review study"
    else if study.isUnlisted then "Link-only review study"
    else "Private review study"

  def apply(data: Main.HomePageData)(using @unused ctx: Context): Page =
    Page("Home - Chesstory")
      .css("home")
      .wrap: _ =>
        main(cls := "home-page")(
          div(cls := "home-shell")(
            st.section(cls := "home-hero")(
              div(cls := "home-hero__copy")(
                p(cls := "home-eyebrow")("Review workspace"),
                h1("Deep review for one game"),
                p(cls := "home-hero__summary")(
                  "Continue a recent analysis or bring in a new game, " +
                    "then keep the board, eval, explanation, and study notes together."
                ),
                renderContinueCard(data.continueCard)
              ),
              div(cls := "home-quickstart")(
                div(cls := "home-section-head")(
                  strong("Start the next review"),
                  span("Choose how this game or position enters your workspace.")
                ),
                div(cls := "home-quickstart__grid")(
                  data.quickActions.map(renderQuickAction)*
                )
              )
            ),
            st.section(cls := "home-section home-section--reviews")(
              div(cls := "home-section-head")(
                strong("Recent game analyses"),
                span("Reopen a saved game and continue from the board, candidate lines, and notes.")
              ),
              if data.recentAnalyses.nonEmpty then
                div(cls := "home-card-grid home-card-grid--analysis")(
                  data.recentAnalyses.map(renderRecentAnalysis)*
                )
              else
                renderEmptyStrip(
                  "No recent games yet",
                  "Start from a pasted game or bring in a public game once and it will show up here."
                )
            ),
            st.section(cls := "home-section")(
              div(cls := "home-section-head")(
                strong("Review studies"),
                span("Use review studies when a game deserves shareable sections, notes, and variations.")
              ),
              if data.recentStudies.nonEmpty then
                div(cls := "home-card-grid home-card-grid--notebooks")(
                  data.recentStudies.map(renderStudy)*
                )
              else
                renderEmptyStrip(
                  "No saved review studies yet",
                  "Create one from a board when the position is worth keeping."
                )
            ),
            st.section(cls := "home-section home-section--accounts")(
              div(cls := "home-section-head")(
                strong("Game sources"),
                span("Return to public accounts when you want another full game to review deeply.")
              ),
              if data.recentAccounts.nonEmpty then
                div(cls := "home-strip-grid")(
                  data.recentAccounts.map(renderAccountLookup)*
                )
              else
                renderEmptyStrip(
                  "No recent public accounts yet",
                  "Search a public Chess.com or Lichess username, " +
                    "or bring in a public account game once and it will show up here."
                )
            )
          )
        )

  private def renderContinueCard(card: Main.HomeContinueCard): Frag =
    card match
      case Main.HomeContinueCard.Analysis(entry) =>
        val supportLine =
          List(entry.opening, entry.variant.filterNot(v => entry.opening.contains(v))).flatten.mkString(" • ")
        st.article(cls := "home-continue-card")(
          p(cls := "home-continue-card__eyebrow")("Saved game"),
          h2(cls := "home-continue-card__title")(entry.title),
          p(cls := "home-continue-card__summary")(recentAnalysisMeta(entry)),
          supportLine.nonEmpty.option(p(cls := "home-continue-card__support")(supportLine)),
          div(cls := "home-continue-card__actions")(
            a(href := routes.UserAnalysis.imported(entry._id).url, cls := "button button-fat")(
              "Continue analysis"
            ),
            a(href := routes.UserAnalysis.imported(entry._id).url, cls := "button button-metal")("Open board")
          )
        )
      case Main.HomeContinueCard.Study(entry) =>
        val notePartCount = entry.chapters.size
        st.article(cls := "home-continue-card")(
          p(cls := "home-continue-card__eyebrow")("Review study"),
          h2(cls := "home-continue-card__title")(entry.study.name.value),
          p(cls := "home-continue-card__summary")(
            s"${reviewStudyVisibilityLabel(entry.study)} • " +
              s"$notePartCount ${if notePartCount == 1 then "section" else "sections"}"
          ),
          p(cls := "home-continue-card__support")(
            "Reopen the saved review study when the board, lines, and notes are already connected."
          ),
          div(cls := "home-continue-card__actions")(
            a(href := routes.Study.show(entry.study.id).url, cls := "button button-fat")("Open review study"),
            a(href := routes.UserAnalysis.index.url, cls := "button button-metal")("Open board")
          )
        )
      case Main.HomeContinueCard.Starter =>
        st.article(cls := "home-continue-card")(
          p(cls := "home-continue-card__eyebrow")("Start analysis"),
          h2(cls := "home-continue-card__title")("No recent game yet"),
          p(cls := "home-continue-card__summary")(
            "Start from a pasted game, bring in a public game, or open the board for free analysis."
          ),
          div(cls := "home-continue-card__actions")(
            a(href := routes.UserAnalysis.index.url, cls := "button button-fat")("Paste a PGN"),
            a(href := routes.UserAnalysis.index.url, cls := "button button-metal")("Open board")
          )
        )

  private def renderQuickAction(action: Main.HomeQuickAction): Frag =
    a(href := action.href, cls := "home-action-card")(
      span(cls := "home-action-card__label")(action.label),
      strong(cls := "home-action-card__title")(action.title),
      p(cls := "home-action-card__copy")(action.copy),
      span(cls := "home-action-card__cta")("Start")
    )

  private def renderRecentAnalysis(entry: ImportHistory.Analysis): Frag =
    val supportLine =
      List(entry.opening, entry.variant.filterNot(v => entry.opening.contains(v))).flatten
        .mkString(" • ")
    a(href := routes.UserAnalysis.imported(entry._id).url, cls := "home-card home-card--analysis")(
      div(cls := "home-card__badges")(
        entry.provider.map(providerBadge),
        span(cls := s"home-pill home-pill--${sourceBadgeTone(entry.sourceType)}")(
          sourceTypeLabel(entry.sourceType)
        )
      ),
      strong(cls := "home-card__title")(entry.title),
      p(cls := "home-card__summary")(recentAnalysisMeta(entry)),
      supportLine.nonEmpty.option(p(cls := "home-card__meta")(supportLine)),
      span(cls := "home-card__cta")("Open analysis")
    )

  private def renderStudy(entry: lila.study.Study.WithChaptersAndLiked): Frag =
    val notePartCount = entry.chapters.size
    val notePartPreview =
      entry.chapters.take(2).map(_.value).mkString(" • ")
    a(href := routes.Study.show(entry.study.id).url, cls := "home-card home-card--notebook")(
      div(cls := "home-card__badges")(
        span(cls := "home-pill home-pill--notebook")(
          reviewStudyVisibilityLabel(entry.study)
        ),
        entry.liked.option(span(cls := "home-pill home-pill--liked")("Liked"))
      ),
      strong(cls := "home-card__title")(entry.study.name.value),
      p(cls := "home-card__summary")(
        s"$notePartCount ${if notePartCount == 1 then "section" else "sections"}"
      ),
      notePartPreview.nonEmpty.option(p(cls := "home-card__meta")(notePartPreview)),
      span(cls := "home-card__cta")("Open review study")
    )

  private def renderAccountLookup(account: ImportHistory.Account): Frag =
    val targetUrl =
      account.provider match
        case ImportHistory.providerChessCom => routes.Importer.importFromChessCom(account.username).url
        case _                              => routes.Importer.importFromLichess(account.username).url
    a(
      href := targetUrl,
      cls := "home-strip-card"
    )(
      div(cls := "home-strip-card__top")(
        strong(s"@${account.username}"),
        providerBadge(account.provider)
      ),
      p(s"${account.analysisCount} saved games"),
      span(cls := "home-card__cta")("Choose game to review")
    )

  private def renderEmptyStrip(title: String, copy: String): Frag =
    div(cls := "home-empty")(
      strong(title),
      p(copy)
    )

  private def recentAnalysisMeta(entry: ImportHistory.Analysis): String =
    val line = List(
      entry.username.map("@" + _),
      entry.playedAtLabel.filterNot(_ == "-"),
      entry.result,
      entry.speed.filterNot(_ == "-")
    ).flatten.mkString(" • ")
    if line.nonEmpty then line else "Saved game ready for review"

  private def providerBadge(provider: String): Frag =
    span(cls := s"home-pill home-pill--provider home-pill--${providerTone(provider)}")(
      providerLabel(provider)
    )

  private def providerTone(provider: String): String =
    provider.trim.toLowerCase match
      case ImportHistory.providerChessCom => ImportHistory.providerChessCom
      case _                              => ImportHistory.providerLichess

  private def providerLabel(provider: String): String =
    provider.trim.toLowerCase match
      case ImportHistory.providerChessCom => "Chess.com"
      case _                              => "Lichess"

  private def sourceTypeLabel(sourceType: String): String =
    sourceType match
      case ImportHistory.sourceManual => "Pasted game"
      case _                          => "Player game"

  private def sourceBadgeTone(sourceType: String): String =
    sourceType match
      case ImportHistory.sourceManual => "manual"
      case _                          => "imported"
