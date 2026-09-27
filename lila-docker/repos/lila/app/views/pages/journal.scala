package views.pages

import scala.annotation.unused

import lila.app.JournalContent
import lila.app.UiEnv.{ *, given }
import lila.ui.{ Esm, Page }

object journal:

  def apply(posts: List[JournalContent.Post], selected: Option[JournalContent.Post])(using
      @unused ctx: Context
  ): Page =
    Page(s"${selected.fold("Journal")(post => post.title)} - Chesstory")
      .css("journal")
      .js(Esm("site.reviewExample"))
      .wrap: _ =>
        main(cls := "journal-page")(
          div(cls := "journal-shell")(
            if selected.isDefined then
              a(href := routes.Main.journal.url, cls := "journal-back")("← Journal index")
            else
              header(cls := "journal-head")(
                h1("Product and study notes"),
                p(cls := "journal-intro")("Release context, engine limits, and practical notes from the board.")
              ),
            selected.isEmpty.option(
              st.section(cls := "journal-feature")(
                div(
                  p(cls := "journal-post-kicker")("A lesson on the board"),
                  h2("Prepare the idea. Then play it."),
                  p(ReviewExample.lesson),
                  a(href := routes.Main.examples.url, cls := "journal-back")("Explore the explained example →")
                ),
                ReviewExample.widget
              )
            ),
            div(cls := (if selected.isDefined then "journal-layout" else "journal-layout journal-layout--index"))(
              st.nav(cls := "journal-archive", aria.label := "Journal archive")(
                h2(if selected.isDefined then "Continue reading" else "All notes"),
                if posts.nonEmpty then
                  ul(
                    (if selected.isDefined then posts.filterNot(post => selected.exists(_.slug == post.slug)).take(2) else posts).map: post =>
                      li(
                        a(
                          href := routes.Main.journalPost(post.slug).url,
                          cls := List(
                            "journal-archive-link" -> true,
                            "is-active" -> selected.exists(_.slug == post.slug)
                          )
                        )(
                          strong(post.title),
                          span(cls := "journal-archive-meta")(s"${post.publishedLabel} · ${post.readTime}"),
                          span(cls := "journal-archive-summary")(post.summary)
                        )
                      )
                  )
                else p(cls := "journal-empty-copy")("No posts published yet.")
              ),
              selected.map: post =>
                st.article(cls := "journal-post")(
                  header(cls := "journal-post-header")(
                    p(cls := "journal-post-kicker")(post.kicker),
                    h1(post.title),
                    p(cls := "journal-post-deck")(post.summary),
                    div(cls := "journal-post-meta")(post.publishedLabel, " · ", post.readTime),
                    post.tags.nonEmpty.option(
                      div(cls := "journal-tag-row")(
                        post.tags.map(tag => span(cls := "journal-tag")(tag))
                      )
                    )
                  ),
                  div(cls := "journal-post-body")(post.body.frag),
                  footer(cls := "journal-post-footer")(
                    a(href := routes.Main.journal.url, cls := "journal-action")("Journal index")
                  )
                )
            )
          )
        )
