package lila.study
package ui

import scalalib.paginator.Paginator
import lila.core.study.StudyOrder
import lila.study.Study.WithChaptersAndLiked
import lila.ui.*
import ScalatagsTemplate.{ *, given }

final class ListUi(helpers: Helpers, bits: StudyBits):
  import helpers.*

  private def studyAllUrl(order: StudyOrder, page: Int): String =
    routes.Study.all(order, page).url

  private def studyMineUrl(order: StudyOrder, page: Int): String =
    routes.Study.mine(order, page).url

  def all(pag: Paginator[WithChaptersAndLiked], order: StudyOrder)(using Context) =
    page(
      title = "Public review studies",
      active = "all",
      order = order,
      pag = pag,
      url = order => studyAllUrl(order, 1)
    )

  def mine(pag: Paginator[WithChaptersAndLiked], order: StudyOrder)(using Context) =
    page(
      title = "My review studies",
      active = "mine",
      order = order,
      pag = pag,
      url = order => studyMineUrl(order, 1)
    )

  private def page(
      title: String,
      active: String,
      order: StudyOrder,
      pag: Paginator[WithChaptersAndLiked],
      url: StudyOrder => String
  )(using ctx: Context): Page =
    val (eyebrow, lede, railTitle, railBody) =
      if active == "mine" then
        (
          "Your review studies",
          "Create a blank review study or reopen the ones you already use for lines, notes, and reusable sections.",
          "Start from analysis",
          "Use the analysis board when you want to carry a loaded game into a review study."
        )
      else
        (
          "Shared review studies",
          "Browse public review studies built around opening ideas, middlegame plans, and reusable annotated sections.",
          "Section links",
          "Open a review study directly on the saved section you want to review or share."
        )

    Page(title)
      .css("analyse.study.index")
      .js(Esm("analyse.study.index")):
        main(cls := "page-menu")(
          menu(active, order),
          main(cls := "page-menu__content study-index box")(
            div(cls := "box__top notebook-index__top")(
              div(cls := "notebook-index__intro")(
                span(cls := "notebook-index__eyebrow")(eyebrow),
                h1(title),
                p(cls := "notebook-index__lede")(lede)
              ),
              div(cls := "notebook-index__controls")(
                bits.orderSelect(order, active, url),
                ctx.isAuth.option(
                  div(cls := "notebook-index__create")(
                    bits.newForm(),
                    p(cls := "notebook-index__create-copy")(
                      if active == "mine" then
                        "Starts a blank review study immediately. To carry a loaded game, create it from the analysis board."
                      else "Signed in? Start a blank review study here, or create one from the analysis board when a game is already loaded."
                    )
                  )
                )
              )
            ),
            div(cls := "notebook-index__proof")(
              proofCard("Section-first", "Keep each line as a reusable section instead of a disposable analysis tab."),
              proofCard("Shareable", "Send a direct review study link to the exact saved section you want to discuss."),
              proofCard(railTitle, railBody)
            ),
            paginate(pag, active, url(order))
          )
        )

  private def proofCard(title: String, body: String) =
    div(cls := "notebook-index__proof-card")(
      strong(title),
      span(body)
    )

  private def paginate(pager: Paginator[WithChaptersAndLiked], active: String, url: String) =
    if pager.currentPageResults.isEmpty then
      div(cls := "nostudies")(
        bits.notebookGlyph("notebook", "notebook-empty__icon"),
        strong(if active == "mine" then "No review studies yet" else "No public review studies yet"),
        p(
          if active == "mine" then
            "Create one from analysis when a loaded game is worth keeping, or start blank for an outline."
          else "Public review studies will appear here once authors choose to share them."
        ),
        bits.newForm("Create your first review study")
      )
    else
      div(cls := "studies list infinite-scroll")(
        pager.currentPageResults.map { s =>
          div(cls := "study paginated")(bits.widget(s))
        },
        pagerNext(pager, np => addQueryParam(url, "page", np.toString))
      )

  def menu(active: String, order: StudyOrder)(using ctx: Context) =
    val nonMineOrder = if order == StudyOrder.mine then StudyOrder.hot else order
    def activeCls(key: String) = (active == key).option("active")
    lila.ui.bits.pageMenuSubnav(
      a(cls := activeCls("all"), href := studyAllUrl(nonMineOrder, 1))("Browse review studies"),
      ctx.isAuth.option:
        a(cls := activeCls("mine"), href := studyMineUrl(StudyOrder.mine, 1))("My review studies")
    )
