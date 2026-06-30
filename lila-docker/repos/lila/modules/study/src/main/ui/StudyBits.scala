package lila.study
package ui

import lila.core.study.{ StudyOrder, Visibility }
import lila.ui.*
import ScalatagsTemplate.{ *, given }

final class StudyBits(helpers: Helpers):
  import helpers.*

  private def visibilityLabel(study: Study): String =
    if study.isPublic then "Public review study"
    else if study.isUnlisted then "Shared by link"
    else "Private review study"

  private def visibilityTone(study: Study): Option[String] =
    if study.isPublic then None
    else Some(if study.isUnlisted then "study__topchip--unlisted" else "study__topchip--private")

  def notebookGlyph(kind: String, extraCls: String = ""): Frag =
    val classes =
      List("notebook-glyph", s"notebook-glyph--$kind", extraCls).filter(_.nonEmpty).mkString(" ")
    val body = kind match
      case "bookmark" =>
        """<path d="M10 6.5h12a2 2 0 0 1 2 2v17l-8-4.8-8 4.8v-17a2 2 0 0 1 2-2Z" />
           |<path d="M12 11h8" />
           |<path d="M12 14.5h8" />""".stripMargin
      case "page" =>
        """<path d="M11 5.5h8.5L25 11v14.5a2 2 0 0 1-2 2H11a2 2 0 0 1-2-2v-18a2 2 0 0 1 2-2Z" />
           |<path d="M19.5 5.5V11H25" />
           |<path d="M12.5 15h9" />
           |<path d="M12.5 18.5h9" />
           |<path d="M12.5 22h6.5" />""".stripMargin
      case "section" =>
        """<path d="M8 9.5a2 2 0 0 1 2-2h9l5 5v11a2 2 0 0 1-2 2H10a2 2 0 0 1-2-2v-14Z" />
           |<path d="M10.5 7.5v-2a2 2 0 0 1 2-2H21l3 3" />
           |<path d="M13 15h7" />
           |<path d="M13 18.5h7" />
           |<path d="M13 22h4.5" />""".stripMargin
      case _ =>
        """<path d="M8.5 7.5a2 2 0 0 1 2-2h10.5a3 3 0 0 1 3 3v15.5a2 2 0 0 1-2 2H11a2.5 2.5 0 0 1-2.5-2.5V7.5Z" />
           |<path d="M12 5.5v21" />
           |<path d="M15 11.5h6" />
           |<path d="M15 15h6" />
           |<path d="M15 18.5h4.5" />
           |<path d="M12 23.5c.7-1 1.7-1.5 3-1.5h9" />""".stripMargin
    raw(
      s"""<span class="$classes" aria-hidden="true">
         |  <svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" stroke-linejoin="round">
         |    $body
         |  </svg>
         |</span>""".stripMargin
    )

  def coverPreview(title: String, subtitle: String, detail: String, compact: Boolean = false): Frag =
    div(cls := s"notebook-cover${if compact then " notebook-cover--compact" else ""}")(
      div(cls := "notebook-cover__spine"),
      div(cls := "notebook-cover__pages")(
        span(cls := "notebook-cover__page-edge"),
        span(cls := "notebook-cover__page-edge notebook-cover__page-edge--mid"),
        span(cls := "notebook-cover__page-edge notebook-cover__page-edge--inner")
      ),
      div(cls := "notebook-cover__face")(
        div(cls := "notebook-cover__seal")(notebookGlyph("notebook", "notebook-cover__glyph")),
        span(cls := "notebook-cover__eyebrow")("Review study"),
        strong(cls := "notebook-cover__title")(title),
        span(cls := "notebook-cover__subtitle")(subtitle),
        span(cls := "notebook-cover__detail")(detail)
      )
    )

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

  def newForm(buttonLabel: String = "Create review study") =
    val visibilityChoices = List(
      (Visibility.unlisted, "Link sharing", "Anyone with the link can review it."),
      (Visibility.`private`, "Private", "Only you can open this review study."),
      (Visibility.public, "Public", "Visible in public review study lists.")
    )
    postForm(cls := "new-study", action := routes.Study.create.url)(
      fieldset(cls := "new-study__visibility")(
        legend("Access"),
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
      submitButton(
        cls := "button button-green",
        title := "Create a blank review study and open it immediately",
        notebookGlyph("notebook", "new-study__glyph"),
        span(cls := "new-study__label")(buttonLabel)
      )
    )

  def widget(s: Study.WithChaptersAndLiked, tag: Tag = h2) =
    val collaboratorPreview =
      s.study.members.members.values
        .take(Study.previewNbMembers)
        .toList
    val sectionCount = s.chapters.size
    val collaboratorCount = s.study.members.members.size

    frag(
      a(
        cls := "overlay",
        href := routes.Study.show(s.study.id),
        title := s.study.name,
        aria.label := s"Open review study: ${s.study.name.value}"
      ),
      div(cls := "top")(
        div(cls := "study__cover")(
          coverPreview(
            s.study.name.value,
            s.chapters.headOption.fold("Opening section")(_.value),
            s"$sectionCount ${if sectionCount == 1 then "section" else "sections"}",
            compact = true
          )
        ),
        div(
          tag(cls := "study-name")(s.study.name),
          span(cls := "study__topline")(
            visibilityTone(s.study).map(tone => span(cls := s"study__topchip $tone")(visibilityLabel(s.study))),
            span(cls := "study__topchip")(s"${s.study.likes.value} likes"),
            span(cls := "study__topsep")("•"),
            titleNameOrId(s.study.ownerId),
            span(cls := "study__topsep")("•"),
            momentFromNow(s.study.createdAt)
          )
        )
      ),
      div(cls := "study__meta")(
        span(cls := "study__badge")(visibilityLabel(s.study)),
        span(cls := "study__meta-item")(s"$sectionCount ${if sectionCount == 1 then "section" else "sections"}"),
        span(cls := "study__meta-item")(
          s"$collaboratorCount ${if collaboratorCount == 1 then "collaborator" else "collaborators"}"
        )
      ),
      div(cls := "body")(
        div(cls := "chapters")(
          h3("Sections"),
          ol(
            s.chapters.map: name =>
              li(cls := "text")(
                notebookGlyph("section", "study__line-glyph"),
                span(name.value)
              )
          )
        ),
        div(cls := "members")(
          h3("Collaborators"),
          if collaboratorPreview.nonEmpty then
            ol(
              collaboratorPreview.map: m =>
                li(cls := "text")(
                  notebookGlyph(if m.canContribute then "page" else "bookmark", "study__line-glyph"),
                  span(titleNameOrId(m.id))
                )
            )
          else p(cls := "study__empty")("Author only")
        )
      ),
      div(cls := "study__footer")(
        span(cls := "study__footnote")(
          notebookGlyph("bookmark", "study__footer-glyph"),
          span("Board, notes, and saved sections together")
        ),
        span(cls := "study__cta")(
          notebookGlyph("page", "study__cta-glyph"),
          span("Open workspace")
        )
      )
    )
