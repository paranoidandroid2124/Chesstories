package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object privacy:
  private def emailLink(address: String) = a(href := s"mailto:$address")(address)

  def apply(contactEmail: Option[String])(using @unused ctx: Context): Page =
    Page("Privacy Policy - Chesstory")
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content")(
              header(cls := "legal-header")(
                h1("Privacy Policy"),
                p(cls := "legal-meta")("Effective Date: March 10, 2026 • Last Updated: July 2, 2026")
              ),

              st.section(cls := "legal-section")(
                h2("1. Overview"),
                p(
                  "This Privacy Policy explains how Chesstory collects, uses, and handles personal information."
                ),
                p(
                  "Chesstory is currently operated as a personal beta project by an individual developer based in the Republic of Korea."
                )
              ),

              st.section(cls := "legal-section")(
                h2("2. Information We Collect"),
                ul(
                  li(strong("Account data: "), "email address, username, and password hash or other authentication-related records needed to run your account"),
                  li(strong("Chess content: "), "games, PGNs, studies, positions, candidate lines, comments, notes, and analysis inputs you submit or save"),
                  li(strong("Study sharing data: "), "review study titles, section names, visibility settings, collaborator records, and shareable links or public listing metadata"),
                  li(strong("Support communications: "), "messages you send through email or the contact page"),
                  li(strong("Beta feedback and waitlist data: "), "payment-intent answers, price-band preferences, optional product notes, and notification email addresses you submit through beta feedback prompts or forms"),
                  li(strong("Technical and security data: "), "IP address, browser and device information, request logs, and abuse-prevention signals"),
                  li(strong("Public chess data: "), "if you request imports or opponent analysis, Chesstory may fetch public game data from third-party chess platforms"),
                  li(strong("Import history for signed-in users: "), "provider, public username, external game ID, source URL, players, result, date or time controls, opening metadata, and PGN for games you choose to save or reopen")
                )
              ),

              st.section(cls := "legal-section")(
                h2("3. How We Use Information"),
                ul(
                  li("Provide, maintain, and secure the Service"),
                  li("Authenticate accounts and send account-related emails such as verification and password reset messages"),
                  li("Generate analysis and product features you request"),
                  li("Prevent abuse, fraud, and unauthorized access"),
                  li("Respond to support, privacy, and account requests"),
                  li("Improve reliability and product quality"),
                  li("Measure open beta interest and contact people who explicitly ask to hear about possible paid plans")
                )
              ),

              st.section(cls := "legal-section")(
                h2("4. Sharing and Service Providers"),
                p("We do not sell or rent your personal information."),
                p("We may share limited information with service providers when reasonably necessary to operate Chesstory, including:"),
                ul(
                  li("Hosting and infrastructure providers"),
                  li("Email delivery providers"),
                  li("OpenAI API when needed to generate AI-assisted analysis features"),
                  li("Anti-abuse or captcha providers such as hCaptcha when enabled to protect the Service"),
                  li("Authorities or counterparties when required by law or reasonably necessary to protect rights, safety, or the Service")
                ),
                h3("AI and model processing"),
                p(
                  "Some Chesstory explanations may be generated with help from OpenAI API models. ",
                  "Chesstory does not currently plan to use multiple AI model providers for public beta explanations."
                ),
                p("When you request an AI-assisted explanation, Chesstory may send only the information needed to produce that explanation, such as:"),
                ul(
                  li("PGN, FEN, move list, selected move, side to move, player color, candidate lines, Stockfish evaluation, MultiPV lines, and opening or middlegame context"),
                  li("notes, comments, Study section text, or follow-up instructions you choose to ask Chesstory to explain"),
                  li("limited technical metadata such as an internal request identifier, feature name, quota state, error state, and timing information needed for reliability, abuse prevention, and debugging")
                ),
                p(
                  "Chesstory does not intentionally include your password, payment information, account email, contact messages, or unrelated account settings in model prompts. ",
                  "Do not place sensitive personal information, confidential third-party material, or private non-chess information inside PGNs, notes, comments, or prompts unless you are comfortable with that information being processed for the requested explanation."
                ),
                p(
                  "Generated explanations may be stored by Chesstory when you save them as part of a review study, note, import history, or analysis record. ",
                  "Temporary request logs may be retained for a limited period for reliability, security, abuse prevention, and debugging. ",
                  "OpenAI may process prompts and outputs under its API terms, privacy terms, data-processing settings, and retention policies. ",
                  "OpenAI states that API data is not used to train or improve OpenAI models by default unless the customer opts in, but API abuse-monitoring logs may retain prompts, responses, and related metadata for up to 30 days unless a different approved retention control applies."
                ),
                p(
                  "Chesstory does not currently promise Zero Data Retention or Modified Abuse Monitoring for OpenAI API requests. ",
                  "If Chesstory receives approval for stricter OpenAI retention controls later, this policy will be updated before those controls are described as active."
                ),
                p(
                  "Chesstory does not use your private PGNs, notes, or saved review studies to train a general-purpose model. ",
                  "We may use aggregated, de-identified, or manually reviewed product feedback to improve analysis quality, but we try to remove account identifiers where practical before internal review."
                )
              ),

              st.section(cls := "legal-section")(
                h2("5. Cookies and Similar Technologies"),
                p("Chesstory uses cookies and similar browser storage to operate the Service and, if you allow it, to remember optional on-device preferences."),
                h3("Essential storage"),
                ul(
                  li(strong("Session and sign-in cookies: "), "keep you signed in and protect your account session"),
                  li(strong("Security cookies or tokens: "), "support email confirmation, password reset, and related account security flows"),
                  li(strong("Consent record cookie: "), "stores your Cookie Settings choice so we can remember it on later visits"),
                  li(strong("Security and request handling signals: "), "support abuse prevention, account protection, and normal service delivery")
                ),
                h3("Optional preference storage"),
                p("If you choose to allow preferences, Chesstory may store optional browser-side data such as:"),
                ul(
                  li(strong("Appearance settings: "), "theme and zoom preferences, especially for signed-out visitors"),
                  li(strong("On-device analysis state: "), "panel choices and local analysis snapshots saved in your browser"),
                  li(strong("Performance caches: "), "browser-side IndexedDB or file storage used to cache engine assets or similar performance-related files"),
                  li(strong("Troubleshooting logs: "), "browser-side diagnostic logs that help surface client-side errors")
                ),
                p(
                  "If you switch to Essential only, Chesstory stops using optional preference storage and performs best-effort cleanup of known preference cookies and browser-side storage on that device."
                ),
                h3("Advertising and tracking"),
                p("Chesstory does not currently use advertising cookies or cross-site tracking cookies."),
                p(
                  "When anti-abuse providers such as captcha or challenge tools are enabled, those providers may process device or browser signals under their own policies."
                ),
                div(cls := "legal-actions")(
                  a(href := "#cookie-consent", cls := "button button-empty js-cookie-consent-open")("Manage Cookie Settings")
                ),
                p(cls := "legal-note")(
                  "Cookie Settings control optional browser-side storage on your device. They do not delete account data stored on Chesstory's servers."
                )
              ),

              st.section(cls := "legal-section")(
                h2("6. Study Visibility and Sharing"),
                p(
                  "Review studies can be private, link-shareable, or public depending on the visibility you choose. ",
                  "Private studies are intended for you and invited collaborators. Link-shareable studies can be opened by anyone who receives the link. Public studies may appear in public lists."
                ),
                p(
                  "If you share a study link, people who receive it may forward it to others. ",
                  "Imported public games may also contain third-party platform usernames and public game metadata."
                )
              ),

              st.section(cls := "legal-section")(
                h2("7. Retention, Closure, and Deletion"),
                p(
                  "We keep account and service data for as long as needed to operate the Service, secure accounts, and handle legitimate support and operational needs."
                ),
                p(
                  "Closing your account disables access, but deletion is handled through a separate request process. ",
                  "If you request erasure, the request is reviewed and processed manually. ",
                  "Some backups, logs, or security records may remain for a limited period before they age out or are overwritten."
                ),
                p(
                  "We may retain limited records where reasonably necessary for security, fraud prevention, or legal compliance."
                )
              ),

              st.section(cls := "legal-section")(
                h2("8. International Processing"),
                p(
                  "Chesstory is operated from the Republic of Korea, and some service providers may process data in other countries. ",
                  "By using the Service, you understand that data may be processed outside your home jurisdiction."
                )
              ),

              st.section(cls := "legal-section")(
                h2("9. Children"),
                p(
                  "Chesstory is not directed to children under 13 and is not intended for users under 14 years old. ",
                  "If you believe a child has provided personal information to Chesstory without appropriate permission, contact us so we can review, restrict, or remove the information as appropriate."
                )
              ),

              st.section(cls := "legal-section")(
                h2("10. Your Choices and Requests"),
                p("You may have rights to request access, correction, or deletion of your information, depending on applicable law."),
                p(
                  "You can also manage some account information directly inside the Service, including email and password settings when available."
                ),
                p(
                  "You can change optional browser storage choices at any time through ",
                  a(href := "#cookie-consent", cls := "js-cookie-consent-open")("Cookie Settings"),
                  "."
                ),
                contactEmail.fold[Frag](
                  p(
                    "For privacy or account requests, please use the ",
                    a(href := routes.Main.contact.url)("Contact page"),
                    "."
                  )
                )(email =>
                  p(
                    "For privacy or account requests, contact ",
                    emailLink(email),
                    ". Please include your Chesstory username, account email address, and a short description of your request."
                  )
                )
              ),

              st.section(cls := "legal-section")(
                h2("11. Changes to This Policy"),
                p(
                  "We may update this Privacy Policy from time to time. We will post the updated version on this page and update the Last Updated date."
                )
              ),

              footer(cls := "legal-footer")(
                a(href := routes.Main.terms.url, cls := "legal-link")("Terms of Service"),
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
