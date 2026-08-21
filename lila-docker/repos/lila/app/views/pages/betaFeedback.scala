package views.pages

import play.api.data.Form

import controllers.BetaFeedback.FormInput
import lila.app.UiEnv.*
import lila.core.email.EmailAddress
import lila.ui.Page
import scala.annotation.unused

object betaFeedback:

  private val surfaces = List(
    "general" -> "Chesstory beta",
    "strategic_puzzle" -> "Position Exercise",
    "game_chronicle" -> "Guided Review",
    "account_intel" -> "My Patterns"
  )

  private val priceBands = List(
    "" -> "Prefer not to say",
    "under_5" -> "Under $5 / month",
    "5_10" -> "$5 to $10 / month",
    "10_20" -> "$10 to $20 / month",
    "20_plus" -> "$20+ / month"
  )

  private def fieldError(form: Form[?], name: String): Frag =
    form(name).errors.headOption.fold(emptyFrag)(err => p(cls := "legal-field-error")(err.message))

  private def checkedValue(form: Form[FormInput], field: String, expected: String): Boolean =
    form(field).value.contains(expected)

  private def boolValue(form: Form[FormInput], field: String): Boolean =
    form(field).value.exists(v => v == "true" || v == "on" || v == "1")

  private def selectedLabel(form: Form[FormInput]): String =
    val current = form("surface").value.getOrElse("general")
    surfaces.find(_._1 == current).map(_._2).getOrElse("Chesstory beta")

  def apply(
      feedbackForm: Form[FormInput],
      accountEmail: Option[EmailAddress],
      success: Option[String] = None
  )(using @unused ctx: Context): Page =
    Page("Open Beta Feedback - Chesstory")
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content beta-feedback-page")(
              header(cls := "legal-header")(
                h1("Open Beta Feedback"),
                p(cls := "legal-meta")(
                  "Tell us which study moments felt valuable after you actually used them. ",
                  "This does not start billing or reserve a paid plan."
                )
              ),
              st.section(cls := "legal-section beta-feedback-intro")(
                h2("What we want to learn"),
                p(
                  "We use this form to understand which review moments, board exercises, and pattern studies feel worth returning to, which price bands feel reasonable, and who wants a launch email if paid plans open later."
                ),
                div(cls := "legal-beta-pills")(
                  span(cls := "legal-beta-pill")(selectedLabel(feedbackForm)),
                  accountEmail.map(email => span(cls := "legal-beta-pill")(s"Signed in as ${email.value}")),
                  boolValue(feedbackForm, "notify").option(span(cls := "legal-beta-pill legal-beta-pill--accent")("Notify me is on"))
                )
              ),
              feedbackForm.globalErrors.headOption.map(err =>
                div(cls := "legal-form-banner legal-form-banner--error")(err.message)
              ),
              success.map(msg =>
                div(cls := "legal-form-banner legal-form-banner--success")(msg)
              ),
              st.form(cls := "legal-form", method := "post", action := routes.BetaFeedback.submitForm.url)(
                input(tpe := "hidden", name := "entrypoint", value := feedbackForm("entrypoint").value.getOrElse("")),
                input(tpe := "hidden", name := "returnTo", value := feedbackForm("returnTo").value.getOrElse("")),
                div(cls := "legal-field")(
                  label(attr("for") := "surface")("Study area"),
                  select(id := "surface", name := "surface")(
                    surfaces.map: (optionValue, labelText) =>
                      option(value := optionValue, if feedbackForm("surface").value.contains(optionValue) then selected := true else emptyFrag)(labelText)
                  ),
                  p(cls := "legal-note")("Choose the part of Chesstory that best matches the board or review you just studied.")
                ),
                div(cls := "legal-field")(
                  label(attr("for") := "feature")("Moment or study path"),
                  input(
                    id := "feature",
                    name := "feature",
                    value := feedbackForm("feature").value.getOrElse(""),
                    placeholder := "Guided review, e-file plan, position exercise, recurring pattern..."
                  ),
                  p(cls := "legal-note")("Optional. Use this when a specific move, plan, or study path made you want to leave feedback.")
                ),
                div(cls := "legal-field")(
                  span(cls := "legal-label")("Would you pay for this if it kept helping your chess study?"),
                  div(cls := "legal-radio-grid")(
                    label(cls := "legal-radio-card")(
                      input(tpe := "radio", name := "willingness", value := "would_pay", if checkedValue(feedbackForm, "willingness", "would_pay") then checked := true else emptyFrag),
                      strong("Would pay"),
                      span("This already feels like a study tool I would return to.")
                    ),
                    label(cls := "legal-radio-card")(
                      input(tpe := "radio", name := "willingness", value := "maybe", if checkedValue(feedbackForm, "willingness", "maybe") then checked := true else emptyFrag),
                      strong("Maybe"),
                      span("Useful, but pricing and polish still matter.")
                    ),
                    label(cls := "legal-radio-card")(
                      input(tpe := "radio", name := "willingness", value := "not_now", if checkedValue(feedbackForm, "willingness", "not_now") then checked := true else emptyFrag),
                      strong("Not for now"),
                      span("Interesting, but not something I'd pay for yet.")
                    )
                  ),
                  fieldError(feedbackForm, "willingness")
                ),
                div(cls := "legal-field")(
                  label(attr("for") := "priceBand")("What price band feels reasonable?"),
                  select(id := "priceBand", name := "priceBand")(
                    priceBands.map: (optionValue, labelText) =>
                      option(value := optionValue, if feedbackForm("priceBand").value.contains(optionValue) then selected := true else emptyFrag)(labelText)
                  ),
                  fieldError(feedbackForm, "priceBand")
                ),
                div(cls := "legal-field")(
                  label(cls := "legal-check")(
                    input(tpe := "checkbox", name := "notify", value := "true", if boolValue(feedbackForm, "notify") then checked := true else emptyFrag),
                    span("Email me if Chesstory opens paid beta plans later.")
                  ),
                  p(cls := "legal-note")(
                    "If you are signed in and already have an account email, you can leave the email field empty and we will use that address."
                  )
                ),
                div(cls := "legal-field")(
                  label(attr("for") := "email")("Notification email"),
                  input(
                    id := "email",
                    name := "email",
                    tpe := "email",
                    value := feedbackForm("email").value.getOrElse(accountEmail.map(_.value).getOrElse("")),
                    placeholder := "you@example.com",
                    autocomplete := "email"
                  ),
                  fieldError(feedbackForm, "email")
                ),
                div(cls := "legal-field")(
                  label(attr("for") := "notes")("Optional note"),
                  textarea(
                    id := "notes",
                    name := "notes",
                    attr("rows") := "5",
                    placeholder := "What would make this clearly worth paying for? What still feels missing?"
                  )(feedbackForm("notes").value.getOrElse("")),
                  fieldError(feedbackForm, "notes")
                ),
                div(cls := "legal-actions")(
                  button(cls := "button", tpe := "submit")("Save beta feedback"),
                  a(href := (feedbackForm("returnTo").value.filter(_.startsWith("/")).getOrElse(routes.Main.support.url)), cls := "legal-link")("Back")
                )
              ),
              footer(cls := "legal-footer")(
                a(href := routes.Main.support.url, cls := "legal-link")("Support"),
                span(" • "),
                a(href := routes.Main.privacy.url, cls := "legal-link")("Privacy Policy"),
                span(" • "),
                a(href := routes.Main.terms.url, cls := "legal-link")("Terms of Service")
              )
            )
          )
        )
