package views.pages

import scala.annotation.unused

import lila.app.JournalContent
import lila.app.UiEnv.{ *, given }
import lila.ui.Page

object journal:

  def apply(posts: List[JournalContent.Post], selected: Option[JournalContent.Post])(using
      @unused ctx: Context
  ): Page =
    Page(s"${selected.fold("Journal")(post => post.title)} - Chesstory")
      .css("journal")
      .wrap: _ =>
        main(cls := "journal-page")(
          div(cls := "journal-shell")(
            header(cls := "journal-head")(
              h1("Product and study notes"),
              p(cls := "journal-intro")("Release context, engine limits, and practical notes from the board.")
            ),
            div(cls := "journal-layout")(
              st.nav(cls := "journal-archive", aria.label := "Journal archive")(
                h2("Archive"),
                if posts.nonEmpty then
                  ul(
                    posts.map: post =>
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
              selected.fold(
                posts.headOption.fold(
                  st.article(cls := "journal-post journal-post--empty")(
                    header(cls := "journal-post-header")(
                      p(cls := "journal-post-kicker")("Archive"),
                      h2("No posts published yet"),
                      p(cls := "journal-post-deck")("Published notes will appear here and in the archive.")
                    )
                  )
                ): latest =>
                  st.article(cls := "journal-post journal-post--landing")(
                    header(cls := "journal-post-header")(
                      p(cls := "journal-post-kicker")("Latest note"),
                      h2(latest.title),
                      p(cls := "journal-post-deck")(latest.summary),
                      div(cls := "journal-post-meta")(latest.publishedLabel, " · ", latest.readTime)
                    ),
                    footer(cls := "journal-post-footer")(
                      a(href := routes.Main.journalPost(latest.slug).url, cls := "journal-action")(
                        "Read article"
                      )
                    )
                  )
              ): post =>
                st.article(cls := "journal-post")(
                  header(cls := "journal-post-header")(
                    p(cls := "journal-post-kicker")(post.kicker),
                    h2(post.title),
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
