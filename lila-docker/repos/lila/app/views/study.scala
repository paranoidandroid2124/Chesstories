package views

import play.api.libs.json.{ JsObject, Json }

import lila.app.UiEnv
import lila.app.UiEnv.*
import lila.study.{ Chapter, Study }

object study:

  private lazy val studyBits = lila.study.ui.StudyBits(UiEnv)
  private lazy val listUi = lila.study.ui.ListUi(UiEnv, studyBits)
  private lazy val analyseUi = lila.analyse.ui.AnalyseUi(UiEnv)(UiEnv.analyseEndpoints)

  private def sectionCountLabel(chapters: List[Chapter.IdName]): String =
    val size = chapters.size
    s"$size ${if size == 1 then "section" else "sections"}"

  private def visibilityLabel(study: Study): String =
    if study.isPublic then "Public"
    else if study.isUnlisted then "Shared by link"
    else "Private"

  private def heroMeta(kind: String, label: String, value: Frag) =
    div(cls := "notebook-hero__meta-pill")(
      studyBits.notebookGlyph(kind, "notebook-hero__meta-icon"),
      span(cls := "notebook-hero__meta-copy")(
        span(cls := "notebook-hero__meta-label")(label),
        strong(value)
      )
    )

  private def studyLede(study: Study, chapter: Chapter): String =
    s"Use ${study.name.value} as a review record. Connect the opening structure in " +
      s"${chapter.name.value} to the middlegame plans, decisions, and evidence on the board below."

  private val reviewMapSteps = List(
    "Opening idea" -> "Name the structure, tension, or tabiya this game came from.",
    "Middlegame plan" -> "Track the pawn break, piece route, or target the opening created.",
    "Decision moment" -> "Use the board and engine lines as evidence for the key choice.",
    "Reusable lesson" -> "Save the idea you should recognize next time this structure appears."
  )

  object ui:

    def all(
        pag: scalalib.paginator.Paginator[Study.WithChaptersAndLiked],
        order: lila.core.study.StudyOrder
    )(using Context): Page =
      listUi.all(pag, order)

    def mine(
        pag: scalalib.paginator.Paginator[Study.WithChaptersAndLiked],
        order: lila.core.study.StudyOrder
    )(using Context): Page =
      listUi.mine(pag, order)

    def chapter(
        data: JsObject,
        study: Study,
        chapter: Chapter,
        canWrite: Boolean,
        chapters: List[Chapter.IdName]
    )(using ctx: Context): Page =
      val studyCfgBase =
        Json.obj(
          "id" -> study.id.value,
          "chapterId" -> chapter.id.value,
          "name" -> study.name.value,
          "chapterName" -> chapter.name.value,
          "canWrite" -> canWrite,
          "url" -> routes.Study.chapter(study.id, chapter.id).url,
          "visibility" -> study.visibility.toString,
          "chapters" -> chapters.map(c =>
            Json.obj(
              "id" -> c.id.value,
              "name" -> c.name.value,
              "url" -> routes.Study.chapter(study.id, c.id).url
            )
          )
        )
      val cfg =
        Json
          .obj(
            "data" -> data,
            "study" -> studyCfgBase
          ) ++ analyseUi.explorerAndCevalConfig

      Page(s"${study.name.value} • ${chapter.name.value}")
        .css("analyse.study")
        .csp(analyseUi.bits.csp.compose(_.withExternalAnalysisApis))
        .js(PageModule("analyse.study", Json.obj("cfg" -> cfg)))
        .flag(_.zoom):
          div(cls := "notebook-shell")(
            div(cls := "notebook-hero")(
              div(cls := "notebook-hero__cover-frame")(
                studyBits.coverPreview(
                  study.name.value,
                  chapter.name.value,
                  s"${sectionCountLabel(chapters)} • ${visibilityLabel(study)}",
                  compact = true
                )
              ),
              div(cls := "notebook-hero__body")(
                div(cls := "notebook-hero__eyebrow")(
                  studyBits.notebookGlyph("bookmark", "notebook-hero__eyebrow-icon"),
                  span("Review study"),
                  span(cls := "notebook-hero__eyebrow-sep")("•"),
                  span(study.name.value)
                ),
                h1(cls := "notebook-hero__title")(chapter.name.value),
                p(cls := "notebook-hero__lede")(studyLede(study, chapter)),
                div(cls := "notebook-hero__meta")(
                  heroMeta("notebook", "Review", "Opening to middlegame"),
                  heroMeta("section", "Sections", sectionCountLabel(chapters)),
                  heroMeta("bookmark", "Access", visibilityLabel(study)),
                  heroMeta("page", "Evidence", "Board, lines, notes")
                ),
                div(cls := "notebook-review")(
                  div(cls := "notebook-review__head")(
                    strong("Opening-to-middlegame thread"),
                    span("A review study should explain how the opening became a plan, not just store lines.")
                  ),
                  div(cls := "notebook-review__grid")(
                    reviewMapSteps.map { case (title, body) =>
                      div(cls := "notebook-review__card")(
                        strong(title),
                        span(body)
                      )
                    }
                  )
                ),
                div(cls := "notebook-hero__navigator")(
                  div(cls := "notebook-hero__navigator-head")(
                    studyBits.notebookGlyph("section", "notebook-hero__navigator-icon"),
                    div(
                      strong("Review sections"),
                      span("Use sections for opening setup, critical moments, and lessons.")
                    )
                  ),
                  div(cls := "notebook-hero__navigator-grid")(
                    chapters.map: c =>
                      a(
                        cls := s"notebook-hero__section${if c.id == chapter.id then " is-active" else ""}",
                        href := routes.Study.chapter(study.id, c.id).url
                      )(
                        studyBits.notebookGlyph(
                          if c.id == chapter.id then "page" else "bookmark",
                          "notebook-hero__section-icon"
                        ),
                        span(cls := "notebook-hero__section-copy")(
                          strong(c.name.value),
                          span(if c.id == chapter.id then "Current review" else "Open review")
                        )
                      )
                  )
                )
              )
            ),
            main(cls := "analyse")(
              div(cls := "study-board-fallback")(
                studyBits.notebookGlyph("notebook", "study-board-fallback__icon"),
                strong("Opening the review study board"),
                p(
                  "The board, move list, engine lines, and review notes load here. " +
                    "If this stays visible, refresh the page or check whether scripts are blocked."
                )
              )
            )
          )
