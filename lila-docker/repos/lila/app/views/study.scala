package views

import play.api.libs.json.{ JsObject, Json }

import lila.app.UiEnv
import lila.app.UiEnv.*
import lila.study.{ Chapter, Study }

object study:

  private lazy val studyBits = lila.study.ui.StudyBits(UiEnv)
  private lazy val listUi = lila.study.ui.ListUi(UiEnv, studyBits)
  private lazy val analyseUi = lila.analyse.ui.AnalyseUi(UiEnv)(UiEnv.analyseEndpoints)

  private def moveReviewConfig: JsObject =
    val mode = UiEnv.env.config
      .getOptional[String]("chesstory.moveReview.mode")
      .map(_.trim.toLowerCase)
      .filter(Set("off", "runtime"))
      .getOrElse("off")
    Json.obj("mode" -> mode)

  private def chapterCountLabel(chapters: List[Chapter.IdName]): String =
    val size = chapters.size
    s"$size ${if size == 1 then "chapter" else "chapters"}"

  private def visibilityLabel(study: Study): String =
    if study.isPublic then "Public"
    else if study.isUnlisted then "Link sharing"
    else "Private"

  private def studyFact(label: String, value: Frag) =
    div(dt(label), dd(value))

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
            "study" -> studyCfgBase,
            "moveReview" -> moveReviewConfig
          ) ++ analyseUi.explorerAndCevalConfig

      Page(s"${study.name.value} • ${chapter.name.value}")
        .css("analyse.study")
        .csp(analyseUi.bits.csp.compose(_.withExternalAnalysisApis))
        .js(PageModule("analyse.study", Json.obj("cfg" -> cfg)))
        .flag(_.zoom):
          div(cls := "study-shell")(
            header(cls := "study-head")(
              div(cls := "study-head__title")(
                h1(chapter.name.value),
                span(study.name.value)
              ),
              dl(cls := "study-head__facts")(
                studyFact("Access", visibilityLabel(study)),
                studyFact("Chapters", chapterCountLabel(chapters)),
                studyFact("Editing", if canWrite then "Enabled" else "View only")
              ),
              st.nav(cls := "study-head__chapters", aria.label := "Study chapters")(
                chapters.map: c =>
                  a(
                    cls := (if c.id == chapter.id then "is-active" else ""),
                    href := routes.Study.chapter(study.id, c.id).url,
                    if c.id == chapter.id then attr("aria-current") := "page" else emptyFrag
                  )(c.name.value)
              )
            ),
            main(cls := "analyse")(
              div(cls := "study-board-fallback", role := "status", attr("aria-live") := "polite")(
                strong("Loading study board"),
                p("The board, move list, engine lines, and chapter notes load here.")
              )
            )
          )
