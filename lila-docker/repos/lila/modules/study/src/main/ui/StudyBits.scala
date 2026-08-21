package lila.study
package ui

import lila.core.study.{ StudyOrder, Visibility }
import lila.ui.*
import ScalatagsTemplate.{ *, given }

final class StudyBits(helpers: Helpers):
  import helpers.*

  private def visibilityLabel(study: Study): String =
    if study.isPublic then "Public"
    else if study.isUnlisted then "Link sharing"
    else "Private"

  def orderSelect(order: StudyOrder, active: String, url: StudyOrder => String) =
    val orders =
      if active == "all" then Orders.withoutSelector
      else Orders.withoutMine
    lila.ui.bits.mselect(
      "orders",
      span(Orders.name(order)),
      orders.map: o =>
        a(href := url(o), cls := (order == o).option("current"))(Orders.name(o))
    )

  def newForm(buttonLabel: String = "Create study") =
    val visibilityChoices = List(
      (Visibility.unlisted, "Link sharing", "Anyone with the link can open it."),
      (Visibility.`private`, "Private", "Only you can open it."),
      (Visibility.public, "Public", "Visible in public study lists.")
    )
    postForm(cls := "new-study", action := routes.Study.create.url)(
      fieldset(cls := "new-study__visibility")(
        legend("New study access"),
        div(cls := "new-study__visibility-options")(
          visibilityChoices.map { case (visibility, title, help) =>
            label(cls := "new-study__visibility-option")(
              input(
                tpe := "radio",
                name := "visibility",
                value := visibility.key,
                if visibility == Visibility.unlisted then checked := true else emptyFrag
              ),
              span(cls := "new-study__visibility-copy")(
                strong(title),
                span(help)
              )
            )
          }
        )
      ),
      submitButton(cls := "button button-green", title := "Create a blank study and open it")(
        span(cls := "new-study__label")(buttonLabel)
      )
    )

  def widget(s: Study.WithChaptersAndLiked, tag: Tag = h2) =
    val chapterCount = s.chapters.size
    a(
      cls := "study-row",
      href := routes.Study.show(s.study.id),
      title := s.study.name,
      aria.label := s"Open study: ${s.study.name.value}"
    )(
      div(cls := "study-row__main")(
        tag(cls := "study-name")(s.study.name),
        span(cls := "study-row__owner")(s.study.ownerId.value)
      ),
      div(cls := "study-row__facts")(
        span(visibilityLabel(s.study)),
        span(s"$chapterCount ${if chapterCount == 1 then "chapter" else "chapters"}"),
        span("Updated ", momentFromNow(s.study.updatedAt))
      ),
      span(cls := "study-row__open")("Open")
    )
