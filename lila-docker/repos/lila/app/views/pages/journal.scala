package views.pages

import scala.annotation.unused

import lila.app.JournalContent
import lila.app.UiEnv.{ *, given }
import lila.core.data.Html
import lila.ui.Page

object journal:

  def apply(posts: List[JournalContent.Post], selected: Option[JournalContent.Post])(using @unused ctx: Context): Page =
    val featured = selected.orElse(posts.headOption)

    Page(s"${selected.fold("Journal")(post => post.title)} - Chesstory")
      .css("journal")
      .wrap: _ =>
        main(cls := "journal-page")(
          div(cls := "journal-shell")(
            st.section(cls := "journal-hero")(
              div(cls := "journal-hero-copy")(
                p(cls := "journal-kicker")("Chesstory Journal"),
                h1("Notes on why we are building this, what is changing, and what strategy still needs from the board."),
                p(cls := "journal-intro")(
                  "A small publishing space inside the product for founding notes, product updates, and lessons from building strategy-first study."
                )
              ),
              featured.fold(
                div(cls := "journal-hero-card")(
                  p(cls := "journal-card-label")("Latest post"),
                  h2("No journal posts yet"),
                  p("Publish a note and the latest entry can appear here automatically.")
                )
              ): post =>
                div(cls := "journal-hero-card")(
                  p(cls := "journal-card-label")("Latest post"),
                  h2(post.title),
                  p(post.summary),
                  a(href := routes.Main.journalPost(post.slug).url, cls := "journal-hero-link")("Open article")
                )
            ),
            div(cls := "journal-layout")(
              st.aside(cls := "journal-rail")(
                div(cls := "journal-rail-card")(
                  p(cls := "journal-card-label")("About this space"),
                  h2("What will live here"),
                  ul(cls := "journal-rail-list")(
                    li("Why Chesstory exists"),
                    li("Product updates and shipped changes"),
                    li("Working notes on strategy and plans")
                  )
                ),
                st.nav(cls := "journal-archive", aria.label := "Journal archive")(
                  p(cls := "journal-card-label")("Archive"),
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
                            span(cls := "journal-archive-meta")(post.publishedLabel),
                            strong(post.title),
                            span(cls := "journal-archive-summary")(post.summary)
                          )
                        )
                    )
                  else
                    p(cls := "journal-empty-copy")("No posts published yet.")
                ),
                div(cls := "journal-rail-card journal-rail-card--soft")(
                  p(cls := "journal-card-label")("Reading path"),
                  p("Start with the founding note, then read how engine evidence, AI limits, and review studies shape the product.")
                )
              ),
              selected.fold(
                posts.headOption.fold(
                  st.article(cls := "journal-post journal-post--empty")(
                    header(cls := "journal-post-header")(
                      p(cls := "journal-post-kicker")("Journal"),
                      h1("No posts published yet"),
                      p(cls := "journal-post-deck")(
                        "Once the first journal note is published, it can appear here automatically."
                      )
                    )
                  )
                ): latest =>
                  st.article(cls := "journal-post journal-post--landing")(
                    header(cls := "journal-post-header")(
                      p(cls := "journal-post-kicker")("Journal archive"),
                      h1("Browse the archive or start with the latest note."),
                      p(cls := "journal-post-deck")(
                        "The journal root stays as an archive landing so each published note has a single article URL."
                      )
                    ),
                    div(cls := "journal-post-body")(
                      p("Pick any post from the archive rail, or open the latest entry below."),
                      h2(latest.title),
                      p(latest.summary),
                      div(cls := "journal-post-meta")(
                        span(latest.publishedLabel),
                        span(cls := "journal-meta-dot", aria.hidden := "true")("•"),
                        span(latest.readTime)
                      )
                    ),
                    footer(cls := "journal-post-footer")(
                      p("Start with the latest post or open the board."),
                      div(cls := "journal-footer-actions")(
                        a(href := routes.Main.journalPost(latest.slug).url, cls := "journal-action journal-action--secondary")("Read latest post"),
                        a(href := routes.UserAnalysis.index.url, cls := "journal-action journal-action--primary")("Open board")
                      )
                    )
                  )
              ): post =>
                st.article(cls := "journal-post")(
                  header(cls := "journal-post-header")(
                    p(cls := "journal-post-kicker")(post.kicker),
                    h1(post.title),
                    p(cls := "journal-post-deck")(post.summary),
                    div(cls := "journal-post-meta")(
                      span(post.publishedLabel),
                      span(cls := "journal-meta-dot", aria.hidden := "true")("•"),
                      span(post.readTime)
                    ),
                    post.tags.nonEmpty.option(
                      div(cls := "journal-tag-row")(
                        post.tags.map(tag => span(cls := "journal-tag")(tag))
                      )
                    )
                  ),
                  div(cls := "journal-post-body")(post.body.frag),
                  footer(cls := "journal-post-footer")(
                    p("Want to see the product context around these notes?"),
                    div(cls := "journal-footer-actions")(
                      a(href := homeUrl, cls := "journal-action journal-action--secondary")("Back to home"),
                      a(href := routes.UserAnalysis.index.url, cls := "journal-action journal-action--primary")("Open board")
                    )
                  )
                )
            )
          )
        )
