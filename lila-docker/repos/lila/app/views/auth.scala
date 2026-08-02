package views

import lila.app.UiEnv.*
import lila.ui.Page
import play.api.data.Form
import scala.annotation.unused

object auth:

  val pubOrTor = span("This access is from a public or Tor network.")

  private def themeChoice(using ctx: Context) =
    ctx.pref.currentBg match
      case "light"  => "light"
      case "system" => "system"
      case _        => "dark"

  private def authThemeSwitch(using ctx: Context) =
    val current = themeChoice
    div(cls := "auth-theme-switch", role := "group", aria.label := "Theme")(
      List(
        "light" -> "Light",
        "dark" -> "Dark",
        "system" -> "Auto"
      ).map: (value, label) =>
        button(
          tpe := "button",
          cls := "auth-theme-switch__button js-theme-choice",
          attr("data-theme-choice") := value,
          attr("aria-pressed") := (value == current).toString,
          title := s"Use ${if value == "system" then "device theme" else s"$label theme"}"
        )(label)
    )

  private def authHeader()(using ctx: Context) =
    div(cls := "landing-header")(
      a(href := homeUrl, cls := "logo")("Chesstory"),
      authThemeSwitch
    )

  private def authFooter() =
    div(cls := "landing-footer")(
      div(cls := "footer-links")(
        span("© 2026 Chesstory"),
        a(href := routes.Main.privacy.url, cls := "btn-text")("Privacy"),
        a(href := routes.Main.terms.url, cls := "btn-text")("Terms")
      )
    )

  private def fieldError(form: Form[?], name: String): Frag =
    form(name).errors.headOption
      .map(e => div(cls := "auth-field-error")(e.message))
      .getOrElse(emptyFrag)

  private def globalErrors(form: Form[?]): Frag =
    frag(form.globalErrors.map(e => div(cls := "auth-error")(e.message))*)

  private def flashLine(success: Option[String], error: Option[String]): Frag =
    frag(
      success.map(msg => div(cls := "auth-success")(msg)),
      error.map(msg => div(cls := "auth-error")(msg))
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
        div(cls := "landing-container")(
          authHeader(),
          div(cls := "auth-container")(div(cls := "auth-card")(content)),
          authFooter()
        )
      )

  def login(formData: Form[?], success: Option[String] = None)(using ctx: Context): Page =
    authPage("Log In - Chesstory"):
      val rememberChecked = formData("remember").value.forall(v => v == "true" || v == "on" || v == "1")
      frag(
        h1(cls := "auth-title")("Welcome back"),
        p(cls := "auth-subtitle")("Log in with username or email and password."),
        flashLine(success, none),
        globalErrors(formData),
        form(cls := "auth-form", method := "post", action := routes.Auth.authenticate.url)(
          div(cls := "form-group")(
            input(
              tpe := "text",
              name := "username",
              placeholder := "username or email",
              aria.label := "Username or email",
              value := (formData("username").value | ""),
              autocomplete := "username",
              required,
              autofocus
            ),
            fieldError(formData, "username")
          ),
          div(cls := "form-group")(
            input(
              tpe := "password",
              name := "password",
              placeholder := "password",
              aria.label := "Password",
              autocomplete := "current-password",
              required
            ),
            fieldError(formData, "password")
          ),
          label(cls := "auth-remember")(
            input(tpe := "checkbox", name := "remember", value := "true", if rememberChecked then checked := true else emptyFrag),
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

  def signup(formData: Form[?], captchaEnabled: Boolean, captchaSiteKey: Option[String])(using ctx: Context): Page =
    authPage("Sign Up - Chesstory", captchaCsp = captchaEnabled):
      frag(
        h1(cls := "auth-title")("Create your account"),
        p(cls := "auth-subtitle")("Sign up with email and password."),
        globalErrors(formData),
        form(id := "signup-form", cls := "auth-form", method := "post", action := routes.Auth.signupPost.url)(
          div(cls := "form-group")(
            input(
              tpe := "email",
              name := "email",
              placeholder := "your@email.com",
              aria.label := "Email address",
              value := (formData("email").value | ""),
              autocomplete := "email",
              required,
              autofocus
            ),
            fieldError(formData, "email")
          ),
          div(cls := "form-group")(
            input(
              tpe := "text",
              name := "username",
              placeholder := "username",
              aria.label := "Username",
              value := (formData("username").value | ""),
              autocomplete := "username",
              required
            ),
            fieldError(formData, "username")
          ),
          div(cls := "form-group")(
            input(
              tpe := "password",
              name := "password",
              placeholder := "password",
              aria.label := "Password",
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
        flashLine(success, error),
        form(cls := "auth-form", method := "post", action := routes.Auth.passwordResetApply.url)(
          div(cls := "form-group")(
            input(
              tpe := "email",
              name := "email",
              placeholder := "your@email.com",
              aria.label := "Email address",
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
        flashLine(none, error),
        form(cls := "auth-form", method := "post", action := routes.Auth.passwordResetTokenApply(token).url)(
          div(cls := "form-group")(
            input(
              tpe := "password",
              name := "password",
              placeholder := "new password",
              aria.label := "New password",
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

  def checkEmail(
      concealedEmail: String,
      success: Option[String] = None,
      error: Option[String] = None
  )(using ctx: Context): Page =
    authPage("Check Your Email - Chesstory"):
      frag(
        div(cls := "auth-icon auth-success-icon")("✓"),
        h1(cls := "auth-title")("Confirm your email"),
        p(cls := "auth-message")(
          "We sent a confirmation link to ",
          strong(concealedEmail),
          ".",
          br,
          "Open that email and click the link to activate your account."
        ),
        flashLine(success, error),
        form(cls := "auth-form", method := "post", action := routes.Auth.fixEmail.url)(
          div(cls := "form-group")(
            input(
              tpe := "email",
              name := "email",
              placeholder := "wrong email? enter a new one",
              aria.label := "Email address",
              autocomplete := "email",
              required
            )
          ),
          button(cls := "auth-submit", tpe := "submit")(
            "Resend confirmation",
            span(cls := "arrow")(" ->")
          )
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
