package views.pages

import controllers.Main
import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object home:

  private def reviewStudyVisibilityLabel(study: lila.study.Study): String =
    if study.isPublic then "Public"
    else if study.isUnlisted then "Link sharing"
    else "Private"

  def apply(data: Main.HomePageData)(using @unused ctx: Context): Page =
    Page("Home - Chesstory")
      .css("home")
      .wrap: _ =>
        main(cls := "home-page")(
          div(cls := "home-shell")(
            header(cls := "home-heading")(
              h1("Continue a study or explain a game"),
              p("Return to a saved lesson, or compare the move played with the line that explains it.")
            ),
            data.continueStudy.map(renderContinueCard).getOrElse(emptyFrag),
            st.nav(cls := "home-actions", aria.label := "Start a review")(
              data.quickActions.map(renderQuickAction)*
            ),
            renderSection(
              "Studies",
              "Saved positions, variations, explanations, and lessons.",
              data.recentStudies.map(renderStudy),
              renderEmptyStrip(
                "No studies",
                "Save a study when an explanation or variation is worth keeping."
              )
            )
          )
        )

  private def renderSection(title: String, lede: String, rows: Seq[Frag], empty: Frag): Frag =
    st.section(cls := "home-section")(
      div(cls := "home-section__head")(
        h2(title),
        p(lede)
      ),
      if rows.nonEmpty then div(cls := "home-rows")(rows*) else empty
    )

  private def renderContinueCard(entry: lila.study.Study.WithChaptersAndLiked): Frag =
    val sectionCount = entry.chapters.size
    div(cls := "home-continue")(
      div(cls := "home-continue__label")("Continue"),
      div(cls := "home-continue__copy")(
        strong(entry.study.name.value),
        span(
          s"${reviewStudyVisibilityLabel(entry.study)} • " +
            s"$sectionCount ${if sectionCount == 1 then "section" else "sections"}"
        )
      ),
      div(cls := "home-continue__actions")(
        a(href := routes.Study.show(entry.study.id).url, cls := "button button-fat")("Open study")
      )
    )

  private def renderQuickAction(action: Main.HomeQuickAction): Frag =
    a(href := action.href, cls := "home-action")(
      span(cls := "home-action__label")(action.label),
      span(cls := "home-action__copy")(
        strong(action.title),
        span(action.copy)
      ),
      span(cls := "home-action__arrow", aria.hidden := true)("→")
    )

  private def renderStudy(entry: lila.study.Study.WithChaptersAndLiked): Frag =
    val sectionCount = entry.chapters.size
    a(href := routes.Study.show(entry.study.id).url, cls := "home-row")(
      div(cls := "home-row__main")(
        strong(entry.study.name.value),
        span(s"$sectionCount ${if sectionCount == 1 then "section" else "sections"}"),
        span(reviewStudyVisibilityLabel(entry.study))
      ),
      div(cls := "home-row__facts")(
        entry.liked.option(span("Liked")),
        span("Study")
      ),
      span(cls := "home-row__cta")("Open")
    )

  private def renderEmptyStrip(title: String, copy: String): Frag =
    div(cls := "home-empty")(
      strong(title),
      p(copy)
    )
