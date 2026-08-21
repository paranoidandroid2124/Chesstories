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
      title = "Public studies",
      active = "all",
      order = order,
      pag = pag,
      url = order => studyAllUrl(order, 1)
    )

  def mine(pag: Paginator[WithChaptersAndLiked], order: StudyOrder)(using Context) =
    page(
      title = "My studies",
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
    val lede =
      if active == "mine" then "Resume a chapter or create a blank study."
      else "Browse public studies by name, owner, chapter, and recent update."
    val notice = ctx.req.getQueryString("notice").flatMap:
      case "deleted"         => Some("flash flash-success" -> "Review study deleted.")
      case "import-disabled" => Some("flash flash-error" -> "Game import is disabled in this deployment.")
      case "create-failed"   => Some("flash flash-error" -> "Could not create that review study.")
      case _                 => None

    Page(title)
      .css("analyse.study.index")
      .js(Esm("analyse.study.index")):
        main(cls := "page-menu")(
          menu(active, order),
          div(cls := "page-menu__content study-index box")(
            header(cls := "box__top study-index__head")(
              div(cls := "study-index__intro")(
                h1(title),
                p(lede)
              ),
              div(cls := "study-index__controls")(
                bits.orderSelect(order, active, url),
                ctx.isAuth.option(bits.newForm())
              )
            ),
            notice.map((cssClass, message) => div(cls := cssClass)(message)),
            paginate(pag, active, url(order))
          )
        )

  private def paginate(pager: Paginator[WithChaptersAndLiked], active: String, url: String) =
    if pager.currentPageResults.isEmpty then
      div(cls := "nostudies")(
        strong(if active == "mine" then "No studies yet" else "No public studies yet"),
        p(
          if active == "mine" then "Create a study from the board or start a blank chapter here."
          else "Public studies will appear here when their owners choose to share them."
        ),
        bits.newForm("Create study")
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
      a(cls := activeCls("all"), href := studyAllUrl(nonMineOrder, 1))("Browse studies"),
      ctx.isAuth.option:
        a(cls := activeCls("mine"), href := studyMineUrl(StudyOrder.mine, 1))("My studies")
    )
