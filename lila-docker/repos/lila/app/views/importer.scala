package views

import controllers.Importer.GameCard
import lila.analyse.ImportHistory
import lila.app.UiEnv.*
import lila.ui.Page

object importer:

  def index(
      error: Option[String] = None,
      provider: String = ImportHistory.providerLichess,
      username: String = "",
      recentAccounts: List[ImportHistory.Account] = Nil,
      recentAnalyses: List[ImportHistory.Analysis] = Nil
  )(using ctx: Context): Page =
    val pageError = error.orElse(ctx.flash("error"))
    Page("Recent Games - Chesstory")
      .css("auth")
      .wrap: _ =>
        main(cls := "auth-page auth-page--importer")(
          div(cls := "landing-container")(
            div(cls := "landing-header")(
              a(href := homeUrl, cls := "logo")("Chesstory")
            ),
            div(cls := "auth-container auth-container--wide")(
              div(cls := "auth-card auth-card--importer")(
                div(cls := "importer-shell")(
                  div(cls := "importer-hero")(
                    div(cls := "importer-hero__eyebrow")("Recent games"),
                    h1(cls := "auth-title importer-hero__title")("Choose a game to review"),
                    p(cls := "auth-subtitle importer-hero__subtitle")(
                      "Choose recent public games from Lichess or Chess.com, then open one on the review board or keep the position study for later."
                    )
                  ),
                  pageError.map(msg => div(cls := "auth-error")(msg)),
                  div(cls := "importer-grid importer-grid--intake")(
                    div(cls := "importer-panel importer-panel--intake")(
                      div(cls := "importer-panel__head")(
                        strong(cls := "importer-panel__title")("Find a player's games"),
                        p(cls := "importer-panel__copy")(
                          "Choose Lichess or Chess.com, enter a public username, and bring those games to the board."
                        )
                      ),
                      form(cls := "auth-form importer-form", method := "post", action := routes.Importer.sendGame.url)(
                        div(cls := "form-group")(
                          label(`for` := "import-provider")("Game site"),
                          st.select(id := "import-provider", name := "provider")(
                            option(
                              value := ImportHistory.providerLichess,
                              if provider == ImportHistory.providerLichess then selected := true else emptyFrag
                            )("Lichess"),
                            option(
                              value := ImportHistory.providerChessCom,
                              if provider == ImportHistory.providerChessCom then selected := true else emptyFrag
                            )("Chess.com")
                          )
                        ),
                        div(cls := "form-group")(
                          label(`for` := "import-username")("Username"),
                          input(
                            id := "import-username",
                            tpe := "text",
                            name := "username",
                            value := username,
                            placeholder := "e.g. DrNykterstein or hikaru",
                            required
                          )
                        ),
                        div(cls := "importer-panel__hint")(
                          span("Public games only"),
                          span("Review starts from one game"),
                          span("Saved review study stays optional"),
                          span("Independent from Lichess and Chess.com")
                        ),
                        button(cls := "auth-submit importer-submit", tpe := "submit")(
                          "Show recent games",
                          span(cls := "arrow")(" ->")
                        )
                      )
                    )
                  ),
                  renderRecentSections(recentAccounts, recentAnalyses),
                  div(cls := "auth-links")(
                    a(href := routes.UserAnalysis.index.url)("Back to board")
                  )
                )
              )
            )
          )
        )

  def gameList(
      provider: String,
      username: String,
      games: List[GameCard],
      notice: Option[String] = None,
      recentAccounts: List[ImportHistory.Account] = Nil,
      recentAnalyses: List[ImportHistory.Analysis] = Nil
  )(using ctx: Context): Page =
    val pageError = ctx.flash("error")
    Page("Recent Games - Chesstory")
      .css("auth")
      .wrap: _ =>
        main(cls := "auth-page auth-page--importer")(
          div(cls := "landing-container")(
            div(cls := "landing-header")(
              a(href := homeUrl, cls := "logo")("Chesstory")
            ),
            div(cls := "auth-container auth-container--wide")(
              div(cls := "auth-card auth-card--importer")(
                div(cls := "importer-shell")(
                  div(cls := "importer-hero importer-hero--results")(
                    div(cls := "importer-hero__eyebrow")(providerShortLabel(provider)),
                    h1(cls := "auth-title importer-hero__title")(s"@$username"),
                    p(cls := "auth-subtitle importer-hero__subtitle")(
                      "Open one game on the review board first. Save a review study only when the position is worth keeping."
                    ),
                    div(cls := "importer-summary-strip")(
                      div(cls := "importer-summary-chip")(
                        strong(games.size.toString),
                        span("Recent public games")
                      ),
                      div(cls := "importer-summary-chip")(
                        strong(providerShortLabel(provider)),
                        span("Game site")
                      ),
                      div(cls := "importer-summary-chip")(
                        strong("Review first"),
                        span("Saved review study stays secondary")
                      )
                    )
                  ),
                  pageError.map(msg => div(cls := "auth-error")(msg)),
                  notice.map(msg => div(cls := "auth-error")(msg)),
                  div(cls := "importer-grid importer-grid--results")(
                    div(cls := "importer-panel importer-panel--results")(
                      div(cls := "importer-panel__head")(
                        strong(cls := "importer-panel__title")("Open a game on the board"),
                        p(cls := "importer-panel__copy")(
                          "Pick the game you want and reopen it on the board. Saved-study actions stay below because they answer a different question."
                        )
                      ),
                      games.nonEmpty.option(
                        div(cls := "importer-results-grid")(
                          games.map(renderGameCard(_, username))*
                        )
                      )
                    ),
                    div(cls := "importer-side")(
                      div(cls := "importer-panel importer-panel--compact")(
                        div(cls := "importer-panel__head")(
                          strong(cls := "importer-panel__title")("Find another player"),
                          p(cls := "importer-panel__copy")(
                            "Stay on the same site and load another player's games without starting over."
                          )
                        ),
                        form(cls := "auth-form importer-form importer-form--compact", method := "post", action := routes.Importer.sendGame.url)(
                          input(tpe := "hidden", name := "provider", value := provider),
                          div(cls := "form-group")(
                            label(`for` := "import-username")("Username"),
                            input(
                              id := "import-username",
                              tpe := "text",
                              name := "username",
                              value := username,
                              required
                            )
                          ),
                          button(cls := "auth-submit importer-submit", tpe := "submit")(
                            "Load games",
                            span(cls := "arrow")(" ->")
                          )
                        )
                      )
                    )
                  ),
                  renderRecentSections(recentAccounts, recentAnalyses),
                  div(cls := "auth-links")(
                    a(href := routes.Importer.importGame.url)("New recent-game search"),
                    a(href := routes.UserAnalysis.index.url)("Back to board")
                  )
                )
              )
            )
          )
        )

  private def renderGameCard(game: GameCard, username: String): Frag =
    div(
      cls := "auth-import-card"
    )(
      div(cls := "auth-import-card__head")(
        div(cls := "auth-history__badges")(
          providerBadge(game.provider),
          span(cls := "auth-badge auth-badge--result")(game.result)
        ),
        strong(cls := "auth-import-card__title")(s"${game.white} vs ${game.black}")
      ),
      div(cls := "auth-import-card__meta")(
        s"${game.playedAt} UTC",
        span(cls := "auth-import-card__dot")("•"),
        game.speed,
        span(cls := "auth-import-card__dot")("•"),
        game.gameId
      ),
      div(cls := "auth-import-card__actions")(
        form(method := "post", action := routes.Importer.sendGame.url, style := "margin: 0;")(
          input(tpe := "hidden", name := "pgn64", value := game.pgn64),
          input(tpe := "hidden", name := "sourceType", value := ImportHistory.sourceAccount),
          input(tpe := "hidden", name := "sourceProvider", value := game.provider),
          input(tpe := "hidden", name := "sourceUsername", value := username),
          input(tpe := "hidden", name := "sourceGameId", value := game.gameId),
          input(tpe := "hidden", name := "sourceWhite", value := game.white),
          input(tpe := "hidden", name := "sourceBlack", value := game.black),
          input(tpe := "hidden", name := "sourceResult", value := game.result),
          input(tpe := "hidden", name := "sourceSpeed", value := game.speed),
          input(tpe := "hidden", name := "sourcePlayedAt", value := game.playedAt),
          game.sourceUrl.map(url => input(tpe := "hidden", name := "sourceUrl", value := url)),
          button(tpe := "submit", cls := "auth-import-card__action")("Open review board")
        ),
        game.sourceUrl.map(url =>
          a(href := url, target := "_blank", rel := "noopener noreferrer", cls := "auth-import-card__source")(
            "Original game"
          )
        )
      )
    )

  private def renderRecentSections(
      recentAccounts: List[ImportHistory.Account],
      recentAnalyses: List[ImportHistory.Analysis]
  )(using ctx: Context): Frag =
    if recentAccounts.isEmpty && recentAnalyses.isEmpty then
      div(cls := "auth-history auth-history--empty")(
        div(cls := "auth-empty-state")(
          span(cls := "auth-empty-state__eyebrow")(
            if ctx.isAuth then "Saved games" else "Cross-device games"
          ),
          strong(if ctx.isAuth then "No saved games yet" else "Sign in to keep recent games"),
          p(
            if ctx.isAuth then
              "Open a pasted game or player game once, and it will appear here for fast reopen on the review board."
            else
              "Guest sessions only keep local drafts. Sign in to save recent players and games across devices."
          )
        )
      )
    else
      div(cls := "auth-history")(
        recentAnalyses.nonEmpty.option:
          div(cls := "auth-history__section")(
            div(cls := "auth-history__section-head")(
              strong("Recent games"),
              span(
                "Reopen saved games, then continue on the analysis board."
              )
            ),
            div(cls := "auth-history__list")(
              recentAnalyses.zipWithIndex.map { case (entry, index) =>
                renderRecentAnalysis(entry, priority = index == 0)
              }*
            )
          )
        ,
        recentAccounts.nonEmpty.option:
          div(cls := "auth-history__section")(
            div(cls := "auth-history__section-head")(
              strong("Recent players"),
              span("Jump back into saved Lichess or Chess.com players without retyping usernames.")
            ),
            div(cls := "auth-history__list")(
              recentAccounts.zipWithIndex.map { case (account, index) =>
                renderRecentAccount(account, priority = index == 0)
              }*
            )
          )
      )

  private def renderRecentAccount(account: ImportHistory.Account, priority: Boolean): Frag =
    val targetUrl =
      account.provider match
        case ImportHistory.providerChessCom => routes.Importer.importFromChessCom(account.username).url
        case _ => routes.Importer.importFromLichess(account.username).url
    val footer =
      if priority then "Fastest way back to your last player game list."
      else "Open this player again without typing the username."
    a(
      href := targetUrl,
      cls := List(
        "auth-history__card",
        "auth-history__card--account",
        if priority then "auth-history__card--priority" else ""
      ).filter(_.nonEmpty).mkString(" ")
    )(
      div(cls := "auth-history__badges")(
        providerBadge(account.provider),
        priority.option(span(cls := "auth-badge auth-badge--priority")("Latest")),
        account.lastAnalysedAt.isDefined.option(span(cls := "auth-badge auth-badge--activity")("Studied"))
      ),
      div(cls := "auth-history__title-row")(
        strong(cls := "auth-history__title")(s"@${account.username}"),
        span(cls := "auth-history__cta")(if priority then "Open latest" else "Open")
      ),
      div(cls := "auth-history__summary")(s"${account.analysisCount} saved games"),
      div(cls := "auth-history__meta")(footer)
    )

  private def renderRecentAnalysis(entry: ImportHistory.Analysis, priority: Boolean): Frag =
    val supportLine =
      List(
        entry.opening,
        entry.variant.filterNot(v => entry.opening.contains(v))
      ).flatten.mkString(" • ")
    a(
      href := routes.UserAnalysis.imported(entry._id).url,
      cls := List(
        "auth-history__card",
        "auth-history__card--analysis",
        if priority then "auth-history__card--priority" else ""
      ).filter(_.nonEmpty).mkString(" ")
    )(
      div(cls := "auth-history__badges")(
        entry.provider.map(providerBadge),
        span(cls := s"auth-badge auth-badge--${sourceBadgeTone(entry.sourceType)}")(
          sourceTypeLabel(entry.sourceType)
        ),
        priority.option(span(cls := "auth-badge auth-badge--priority")("Latest"))
      ),
      div(cls := "auth-history__title-row")(
        strong(cls := "auth-history__title")(entry.title),
        span(cls := "auth-history__cta")(if priority then "Open latest" else "Open")
      ),
      div(cls := "auth-history__summary")(recentAnalysisMeta(entry)),
      supportLine.nonEmpty.option(div(cls := "auth-history__meta")(supportLine))
    )

  private def recentAnalysisMeta(entry: ImportHistory.Analysis): String =
    val line = List(
      entry.username.map("@" + _),
      entry.playedAtLabel.filterNot(_ == "-"),
      entry.result,
      entry.speed.filterNot(_ == "-")
    ).flatten.mkString(" • ")
    if line.nonEmpty then line else "Saved game ready for review"

  private def providerShortLabel(provider: String): String =
    provider.trim.toLowerCase match
      case ImportHistory.providerChessCom => "Chess.com"
      case _ => "Lichess"

  private def providerBadge(provider: String): Frag =
    span(cls := s"auth-badge auth-badge--provider auth-badge--${providerTone(provider)}")(
      providerShortLabel(provider)
    )

  private def providerTone(provider: String): String =
    provider.trim.toLowerCase match
      case ImportHistory.providerChessCom => ImportHistory.providerChessCom
      case _ => ImportHistory.providerLichess

  private def sourceTypeLabel(sourceType: String): String =
    sourceType match
      case ImportHistory.sourceManual => "Pasted game"
      case _ => "Player game"

  private def sourceBadgeTone(sourceType: String): String =
    sourceType match
      case ImportHistory.sourceManual => "manual"
      case _ => "imported"
