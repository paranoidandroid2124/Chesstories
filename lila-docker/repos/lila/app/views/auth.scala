package views

import lila.app.UiEnv.*
import lila.ui.Page
import play.api.data.Form

object auth:

  val pubOrTor = span("This access is from a public or Tor network.")

  private def fieldId(name: String) = s"auth-$name"
  private def fieldErrorId(name: String) = s"${fieldId(name)}-error"

  private def fieldState(form: Form[?], name: String): Seq[Modifier] =
    if form(name).errors.nonEmpty then
      Seq[Modifier](attr("aria-invalid") := "true", attr("aria-describedby") := fieldErrorId(name))
    else Seq.empty

  private def inputWithState(form: Form[?], name: String)(mods: Modifier*): Tag =
    input((mods ++ fieldState(form, name))*)

  private def fieldError(form: Form[?], name: String): Frag =
    form(name).errors.headOption
      .map(e => p(id := fieldErrorId(name), cls := "auth-field-error", role := "alert")(e.message))
      .getOrElse(emptyFrag)

  private def globalErrors(form: Form[?]): Frag =
    frag(form.globalErrors.map(e => div(cls := "auth-error", role := "alert")(e.message))*)

  private def noticeLine(success: Option[String], error: Option[String]): Frag =
    frag(
      success.map(msg => div(cls := "auth-success", role := "status", attr("aria-live") := "polite")(msg)),
      error.map(msg => div(cls := "auth-error", role := "alert")(msg))
    )

  private def authPage(
      title: String,
      captchaCsp: Boolean = false
  )(content: Context ?=> Frag)(using ctx: Context): Page =
    val base = Page(title).css("auth").flag(_.noHeader)
    val page =
      if captchaCsp then
        base.csp: csp =>
          csp.copy(
            scriptSrc = "https://hcaptcha.com" :: "https://*.hcaptcha.com" :: csp.scriptSrc,
            frameSrc = "https://hcaptcha.com" :: "https://*.hcaptcha.com" :: csp.frameSrc,
            styleSrc = "https://hcaptcha.com" :: "https://*.hcaptcha.com" :: csp.styleSrc,
            connectSrc = "https://hcaptcha.com" :: "https://*.hcaptcha.com" :: csp.connectSrc
          )
      else base
    page.wrap: _ =>
      main(cls := "auth-page")(
        div(cls := "auth-container")(
          a(cls := "auth-home-link", href := homeUrl)("Chesstory home"),
          div(cls := "auth-card")(content)
        )
      )

  def login(formData: Form[?], success: Option[String] = None)(using ctx: Context): Page =
    authPage("Log In - Chesstory"):
      val rememberChecked = formData("remember").value.exists(v => v == "true" || v == "on" || v == "1")
      frag(
        h1(cls := "auth-title")("Welcome back"),
        p(cls := "auth-subtitle")("Log in with username or email and password."),
        noticeLine(success, none),
        globalErrors(formData),
        form(cls := "auth-form", method := "post", action := routes.Auth.authenticate.url)(
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("username"))("Username or email"),
            inputWithState(formData, "username")(
              id := fieldId("username"),
              tpe := "text",
              name := "username",
              placeholder := "username or email",
              value := (formData("username").value | ""),
              autocomplete := "username",
              required,
              autofocus
            ),
            fieldError(formData, "username")
          ),
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("password"))("Password"),
            inputWithState(formData, "password")(
              id := fieldId("password"),
              tpe := "password",
              name := "password",
              placeholder := "password",
              autocomplete := "current-password",
              required
            ),
            fieldError(formData, "password")
          ),
          label(cls := "auth-remember")(
            input(
              tpe := "checkbox",
              name := "remember",
              value := "true",
              if rememberChecked then checked := true else emptyFrag
            ),
            span("Remember me")
          ),
          button(cls := "auth-submit", tpe := "submit")(
            "Log In",
            span(cls := "arrow")(" ->")
          )
        ),
        div(cls := "auth-links")(
          a(href := routes.Auth.passwordReset.url)("Forgot password?"),
          a(href := routes.Auth.signup.url)("Create account")
        )
      )

  def signup(formData: Form[?], captchaEnabled: Boolean, captchaSiteKey: Option[String])(using
      ctx: Context
  ): Page =
    authPage("Sign Up - Chesstory", captchaCsp = captchaEnabled):
      frag(
        h1(cls := "auth-title")("Create your account"),
        p(cls := "auth-subtitle")("Sign up with email and password."),
        globalErrors(formData),
        form(id := "signup-form", cls := "auth-form", method := "post", action := routes.Auth.signupPost.url)(
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("email"))("Email address"),
            inputWithState(formData, "email")(
              id := fieldId("email"),
              tpe := "email",
              name := "email",
              placeholder := "your@email.com",
              value := (formData("email").value | ""),
              autocomplete := "email",
              required,
              autofocus
            ),
            fieldError(formData, "email")
          ),
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("username"))("Username"),
            inputWithState(formData, "username")(
              id := fieldId("username"),
              tpe := "text",
              name := "username",
              placeholder := "username",
              value := (formData("username").value | ""),
              autocomplete := "username",
              required
            ),
            fieldError(formData, "username")
          ),
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("password"))("Password"),
            inputWithState(formData, "password")(
              id := fieldId("password"),
              tpe := "password",
              name := "password",
              placeholder := "password",
              autocomplete := "new-password",
              required
            ),
            fieldError(formData, "password")
          ),
          if captchaEnabled then
            div(cls := "form-group captcha-group")(
              div(
                cls := "h-captcha",
                attr("data-size") := "compact",
                attr("data-sitekey") := (captchaSiteKey | "")
              ),
              fieldError(formData, "h-captcha-response"),
              raw("""<script src="https://hcaptcha.com/1/api.js" async defer></script>""")
            )
          else emptyFrag,
          button(cls := "auth-submit", tpe := "submit")(
            "Create Account",
            span(cls := "arrow")(" ->")
          )
        ),
        div(cls := "auth-links")(
          a(href := routes.Auth.login.url)("Already have an account? Log in")
        )
      )

  def passwordReset(error: Option[String] = None, success: Option[String] = None)(using ctx: Context): Page =
    authPage("Reset Password - Chesstory"):
      frag(
        h1(cls := "auth-title")("Reset your password"),
        p(cls := "auth-subtitle")("Enter your account email and we'll send a reset link."),
        noticeLine(success, error),
        form(cls := "auth-form", method := "post", action := routes.Auth.passwordResetApply.url)(
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("email"))("Email address"),
            input(
              id := fieldId("email"),
              tpe := "email",
              name := "email",
              placeholder := "your@email.com",
              autocomplete := "email",
              required,
              autofocus
            )
          ),
          button(cls := "auth-submit", tpe := "submit")(
            "Send Reset Link",
            span(cls := "arrow")(" ->")
          )
        ),
        div(cls := "auth-links")(
          a(href := routes.Auth.login.url)("Back to login")
        )
      )

  def passwordResetApply(token: String, error: Option[String] = None)(using ctx: Context): Page =
    authPage("Set New Password - Chesstory"):
      frag(
        h1(cls := "auth-title")("Set a new password"),
        p(cls := "auth-subtitle")("Choose a new password for your account."),
        noticeLine(none, error),
        form(cls := "auth-form", method := "post", action := routes.Auth.passwordResetTokenApply(token).url)(
          div(cls := "form-group")(
            label(cls := "auth-label", `for` := fieldId("password"))("New password"),
            input(
              id := fieldId("password"),
              tpe := "password",
              name := "password",
              placeholder := "new password",
              autocomplete := "new-password",
              required,
              autofocus
            )
          ),
          button(cls := "auth-submit", tpe := "submit")(
            "Update Password",
            span(cls := "arrow")(" ->")
          )
        )
      )

  def passwordResetInvalid(message: String)(using ctx: Context): Page =
    authPage("Reset Link Error - Chesstory"):
      div(cls := "text-center")(
        div(cls := "auth-icon auth-error-icon")("x"),
        h1(cls := "auth-title")("Invalid Link"),
        p(cls := "auth-message")(message),
        a(href := routes.Auth.passwordReset.url, cls := "auth-submit")(
          "Request a new reset link",
          span(cls := "arrow")(" ->")
        )
      )

  def checkEmail()(using ctx: Context): Page =
    authPage("Check Your Email - Chesstory"):
      frag(
        div(cls := "auth-icon auth-success-icon")("✓"),
        h1(cls := "auth-title")("Confirm your email"),
        p(cls := "auth-message")(
          "We sent a confirmation link to the address used at sign-up. Open that email and click the link to activate your account."
        ),
        div(cls := "auth-links")(
          a(href := routes.Auth.signup.url)("Used the wrong address? Create the account again")
        )
      )

  def confirmEmail(formAction: String)(using ctx: Context): Page =
    authPage("Confirm Email - Chesstory"):
      div(cls := "text-center")(
        div(cls := "auth-icon auth-success-icon")("✓"),
        h1(cls := "auth-title")("Confirm your email"),
        p(cls := "auth-message")("Press confirm to continue."),
        form(cls := "auth-form", method := "post", action := formAction)(
          button(cls := "auth-submit", tpe := "submit")(
            "Confirm email",
            span(cls := "arrow")(" ->")
          )
        )
      )

  def loginError(message: String)(using ctx: Context): Page =
    authPage("Login Error - Chesstory"):
      div(cls := "text-center")(
        div(cls := "auth-icon auth-error-icon")("x"),
        h1(cls := "auth-title")("Link Error"),
        p(cls := "auth-message")(message),
        a(href := routes.Auth.login.url, cls := "auth-submit")(
          "Back to login",
          span(cls := "arrow")(" ->")
        )
      )
