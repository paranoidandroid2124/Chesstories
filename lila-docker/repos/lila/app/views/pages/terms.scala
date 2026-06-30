package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object terms:
  private def emailLink(address: String) = a(href := s"mailto:$address")(address)

  def apply(contactEmail: Option[String])(using @unused ctx: Context): Page =
    Page("Terms of Service - Chesstory")
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content")(
              header(cls := "legal-header")(
                h1("Terms of Service"),
                p(cls := "legal-meta")("Effective Date: March 10, 2026 • Last Updated: June 29, 2026")
              ),

              st.section(cls := "legal-section")(
                h2("1. About These Terms"),
                p(
                  "These Terms of Service govern your use of Chesstory (the \"Service\"). ",
                  "By accessing or using the Service, you agree to these Terms. ",
                  "If you do not agree, do not use Chesstory."
                ),
                p(
                  "Chesstory is currently operated as a personal beta project by an individual developer based in the Republic of Korea."
                )
              ),

              st.section(cls := "legal-section")(
                h2("2. Eligibility and Beta Status"),
                p(
                  "You must be at least 14 years old to use Chesstory. ",
                  "If you are under the age of majority where you live, use the Service only with permission from a parent or legal guardian."
                ),
                p(
                  "Chesstory is offered as a beta service. Features may change, be interrupted, or be removed without notice. ",
                  "The Service is currently offered without a public paid subscription, but some features may still be gated by login, quotas, fair-use controls, or beta access tiers. ",
                  "If public paid plans are introduced later, the applicable pricing and billing terms will be shown before purchase."
                ),
                p(
                  "During beta, Chesstory may ask for voluntary product feedback, including whether certain features feel worth paying for and whether you want an email if paid plans open later. ",
                  "Submitting that feedback does not create a purchase obligation or reserve a plan."
                )
              ),

              st.section(cls := "legal-section")(
                h2("3. Accounts and Security"),
                p(
                  "Some features require an account. You must provide a working email address and keep your account information reasonably accurate."
                ),
                p(
                  "You are responsible for activity that occurs under your account and for keeping your sign-in methods secure. ",
                  "Chesstory may use passwords, email confirmation links, and password reset emails as part of account security."
                ),
                p(
                  "Chesstory may also store settings, analysis drafts, or performance caches in your browser when those features are enabled. ",
                  "Optional browser-side storage is controlled through Cookie Settings and described in the ",
                  a(href := routes.Main.privacy.url)("Privacy Policy"),
                  "."
                )
              ),

              st.section(cls := "legal-section")(
                h2("4. Acceptable Use"),
                p("You agree not to:"),
                ul(
                  li("Use the Service for unlawful, abusive, or fraudulent purposes"),
                  li("Interfere with or attempt to damage the Service, its infrastructure, or other users"),
                  li("Bypass security features or attempt to access non-public systems or data"),
                  li("Use unauthorized scraping, crawling, or automated access against the Service"),
                  li("Impersonate another person or misrepresent your identity"),
                  li("Share your account access with others in a way that undermines account security")
                )
              ),

              st.section(cls := "legal-section")(
                h2("5. Your Content and Service Content"),
                p(
                  "You retain ownership of PGNs, studies, chess inputs, and other content you submit to Chesstory. ",
                  "You grant Chesstory a limited license to host, process, and display that content as needed to operate the Service for you."
                ),
                p(
                  "Study visibility matters. Public studies may be listed publicly, link-shareable studies can be opened by anyone with the link, and private studies are limited to you or approved collaborators."
                ),
                p(
                  "Chesstory includes open-source software licensed under GNU AGPL v3 and other licenses listed in our ",
                  a(href := routes.Main.source.url)("Open Source Notice"),
                  ". Chesstory branding, site design, and non-user content remain protected by applicable intellectual property laws and licenses."
                )
              ),

              st.section(cls := "legal-section")(
                h2("6. Analysis, AI Output, and Third-Party Data"),
                p(
                  "Chesstory uses software and, in some cases, AI-assisted systems to generate analysis and recommendations. ",
                  "Those outputs are provided for educational and informational purposes only."
                ),
                ul(
                  li("Analysis may be incomplete, inaccurate, or unavailable"),
                  li("AI-assisted explanations may be generated from PGNs, FENs, engine lines, candidate moves, notes, comments, and related chess context that you submit for analysis"),
                  li("Do not submit sensitive personal information, confidential third-party material, or private non-chess information inside PGNs, notes, comments, or prompts"),
                  li("You are responsible for how you use any chess recommendations"),
                  li("Some analysis features may require sign-in and may be subject to rate limits, daily quotas, fair-use controls, or plan/tier limits"),
                  li("Some features may send chess positions, move lists, PGNs, comments, notes, engine lines, and related chess context to analysis systems or model providers when needed to return the requested explanation"),
                  li("Some features may fetch public chess data from third-party platforms at your request; do not use Chesstory to import private, restricted, or non-consensual data"),
                  li("Chesstory is not affiliated with or endorsed by Lichess, Chess.com, or other third-party platforms unless explicitly stated")
                )
              ),

              st.section(cls := "legal-section")(
                h2("7. Suspension, Closure, and Termination"),
                p(
                  "We may suspend, limit, or terminate access to Chesstory if we reasonably believe it is necessary for security, abuse prevention, legal compliance, or protection of the Service and its users."
                ),
                p(
                  "You may close your account through the Service. Account closure disables access. ",
                  "If you want data erasure, use the privacy request channel described in our ",
                  a(href := routes.Main.privacy.url)("Privacy Policy"),
                  ". Deletion requests are reviewed and handled manually."
                )
              ),

              st.section(cls := "legal-section")(
                h2("8. Disclaimers and Limitation of Liability"),
                p(
                  "THE SERVICE IS PROVIDED \"AS IS\" AND \"AS AVAILABLE\". TO THE MAXIMUM EXTENT PERMITTED BY LAW, ",
                  "CHESSTORY DISCLAIMS WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, ",
                  "AND UNINTERRUPTED AVAILABILITY."
                ),
                p(
                  "TO THE MAXIMUM EXTENT PERMITTED BY LAW, CHESSTORY WILL NOT BE LIABLE FOR INDIRECT, INCIDENTAL, SPECIAL, CONSEQUENTIAL, ",
                  "OR PUNITIVE DAMAGES. TOTAL LIABILITY FOR ANY CLAIM RELATING TO THE SERVICE IS LIMITED TO THE AMOUNT YOU PAID TO CHESSTORY ",
                  "IN THE 12 MONTHS BEFORE THE CLAIM, WHICH DURING A FREE BETA WILL USUALLY BE ZERO."
                )
              ),

              st.section(cls := "legal-section")(
                h2("9. Changes to These Terms"),
                p(
                  "We may update these Terms from time to time. We will post the updated version on this page and update the Last Updated date."
                ),
                p(
                  "If we make a material change that adds charges or materially expands how personal data is used, we will provide notice before that change applies to you."
                )
              ),

              st.section(cls := "legal-section")(
                h2("10. Governing Law and Contact"),
                p(
                  "These Terms are governed by the laws of the Republic of Korea, without regard to conflict of law rules."
                ),
                contactEmail.fold[Frag](
                  p(
                    "For questions about these Terms, please use the ",
                    a(href := routes.Main.contact.url)("Contact page"),
                    "."
                  )
                )(email =>
                  p(
                    "For questions about these Terms, contact ",
                    emailLink(email),
                    "."
                  )
                )
              ),

              footer(cls := "legal-footer")(
                a(href := routes.Main.privacy.url, cls := "legal-link")("Privacy Policy"),
                span(" • "),
                a(href := "#cookie-consent", cls := "legal-link js-cookie-consent-open")("Cookie settings"),
                span(" • "),
                a(href := routes.Main.contact.url, cls := "legal-link")("Contact"),
                span(" • "),
                a(href := homeUrl, cls := "legal-link")("Back to Home")
              )
            )
          )
        )
