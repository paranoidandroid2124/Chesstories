package controllers

import play.api.data.*
import play.api.data.Forms.*
import play.api.libs.json.*
import play.api.i18n.Lang
import play.api.mvc.*
import play.api.Mode

import lila.app.*
import lila.core.email.EmailAddress
import lila.core.id.SessionId
import lila.core.security.ClearPassword
import lila.core.user.UserEnabled
import lila.security.{ EmailConfirm, IsPwned }

final class Auth(env: Env) extends LilaController(env):

  import env.security.api

  private val loginForm = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText,
      "remember" -> default(play.api.data.Forms.boolean, true)
    )((u, p, r) => (u, p, r))(tuple => Some(tuple))
  )

  private val passwordResetForm = Form(
    single(
      "email" -> nonEmptyText.verifying("invalid email", EmailAddress.isValid)
    )
  )

  private val passwordApplyForm = Form(
    single(
      "password" -> nonEmptyText(minLength = 4, maxLength = 999)
    )
  )

  private val fixEmailForm = Form(
    single(
      "email" -> nonEmptyText.verifying("invalid email", EmailAddress.isValid)
    )
  )

  private val resetAckMessage = "If an account exists for that email, a reset link has been sent."
  private val signupCaptchaEnabled =
    env.mode == Mode.Prod && env.config.getOptional[Boolean]("security.hcaptcha.enabled").getOrElse(false)
  private val signupCaptchaSiteKey =
    env.config.getOptional[String]("security.hcaptcha.public.sitekey").filter(_.nonEmpty)
  private val signupEmailConfirmRequired =
    env.config.getOptional[Boolean]("security.email_confirm.enabled").getOrElse(true)

  private def mobileUserOk(u: UserModel, sessionId: SessionId): Fu[Result] =
    fuccess:
      Ok:
        env.user.jsonView.full(u) ++ Json.obj(
          "nowPlaying" -> JsArray(),
          "sessionId" -> sessionId.value
        )

  private def getReferrer(using Context): String =
    env.web.referrerRedirect.fromReq | routes.Main.home.url

  private val cookieMaxAgeRemember: Int = 60 * 60 * 24 * 30
  private def sidCookie(value: String, remember: Boolean)(using ctx: Context): Cookie =
    Cookie(
      name = api.sessionIdKey,
      value = value,
      maxAge = remember.option(cookieMaxAgeRemember),
      path = "/",
      secure = secureCookie,
      httpOnly = true,
      sameSite = Cookie.SameSite.Lax.some
    )

  private def loginPage(form: Form[?])(using Context) =
    views.auth.login(form, ctx.req.flash.get("success"))

  private def signupPage(form: Form[?])(using Context) =
    views.auth.signup(form, signupCaptchaEnabled, signupCaptchaSiteKey)

  def authenticateUser(
      u: UserModel,
      remember: Boolean,
      pwned: IsPwned = IsPwned(false)
  )(using ctx: Context): Fu[Result] =
    api
      .saveAuthentication(u.id, ctx.mobileApiVersion, pwned)
      .flatMap: sessionId =>
        negotiate(
          Redirect(getReferrer),
          mobileUserOk(u, sessionId)
        ).map: res =>
          res.withCookies(sidCookie(sessionId.value, remember))
      .recover:
        case _ => BadRequest("Auth Error")

  def login = Open:
    Ok.page(loginPage(loginForm))

  def authenticate = OpenBody:
    bindForm(loginForm)(
      err => BadRequest.page(loginPage(err)),
      data =>
        val tooMany = loginForm
          .fill((data._1, "", data._3))
          .withGlobalError("Too many login attempts. Try again later.")
        limit.passwordLogin(ctx.ip, BadRequest.page(loginPage(tooMany))):
          api.authenticate(data._1, data._2).flatMap:
            case Some(authSuccess) =>
              authenticateUser(authSuccess.user, remember = data._3, pwned = authSuccess.pwned)
            case None =>
              val failForm = loginForm
                .fill((data._1, "", data._3))
                .withGlobalError("Invalid username/email or password.")
              BadRequest.page(loginPage(failForm))
    )

  def logout = Open:
    api.reqSessionId(ctx.req).fold(fuccess(Redirect(getReferrer))): sid =>
      api.logout(sid).map: _ =>
        Redirect(getReferrer)
          .withNewSession
          .discardingCookies(DiscardingCookie(api.sessionIdKey))

  def logoutGet = Open:
    MethodNotAllowed("Use POST to log out.").toFuccess

  def signup = Open:
    Ok.page(signupPage(env.security.forms.signup.website(using ctx.req)))

  def signupPost = OpenBody:
    val form = env.security.forms.signup.website(using ctx.req)
    bindForm(form)(
      err => BadRequest.page(signupPage(err)),
      data =>
        val tooMany = form.fill(data).withGlobalError("Too many signup attempts. Try again later.")
        limit.signup(ctx.ip, BadRequest.page(signupPage(tooMany))):
          env.security.signup.website(data, ctx.blind).flatMap:
            case lila.security.Signup.Result.AllSet(user) =>
              if signupEmailConfirmRequired then
                val email = data.normalizedEmail
                val token = env.security.loginToken.generate(email, 30.minutes)
                val url = s"${env.baseUrl}${routes.Auth.signupConfirmToken(token).url}"
                env.mailer.automaticEmail.signupConfirm(user, email, url).inject:
                  Redirect(routes.Auth.checkYourEmail)
                    .withCookies(EmailConfirm.cookie.set(user.id, email))
              else authenticateUser(user, remember = true)
            case lila.security.Signup.Result.MissingCaptcha =>
              val captchaErr = form
                .fill(data)
                .withError("h-captcha-response", "Captcha verification failed.")
              BadRequest.page(signupPage(captchaErr))
            case lila.security.Signup.Result.Bad(err) =>
              BadRequest.page(signupPage(err))
            case lila.security.Signup.Result.RateLimited =>
              BadRequest.page(signupPage(tooMany))
            case lila.security.Signup.Result.ForbiddenNetwork =>
              val blocked = form.fill(data).withGlobalError("Signup is blocked from this network.")
              BadRequest.page(signupPage(blocked))
    )

  def passwordReset = Open:
    Ok.page:
      views.auth.passwordReset(
        error = ctx.req.flash.get("error"),
        success = ctx.req.flash.get("success")
      )

  def passwordResetApply = OpenBody:
    bindForm(passwordResetForm)(
      _ => BadRequest.page(views.auth.passwordReset(error = Some("Invalid email address."))),
      emailStr =>
        val normalized = EmailAddress(EmailAddress(emailStr).normalize.value)
        val ack = Redirect(routes.Auth.passwordReset).flashing("success" -> resetAckMessage)
        limit.passwordResetRequest(ctx.ip, ack.toFuccess):
          env.user.repo.byEmail(normalized).flatMap:
            case Some(user) =>
              user.email.fold(ack.toFuccess): userEmail =>
                val token = env.security.passwordResetToken.generate(user.id, 30.minutes)
                val url = s"${env.baseUrl}${routes.Auth.passwordResetToken(token).url}"
                env.mailer.automaticEmail.passwordReset(user, userEmail, url).inject(ack)
            case None =>
              ack.toFuccess
    )

  def passwordResetToken(token: String) = Open:
    env.security.passwordResetToken.read(token) match
      case Some(_) => Ok.page(views.auth.passwordResetApply(token))
      case None    => Ok.page(views.auth.passwordResetInvalid("Invalid or expired reset link."))

  def passwordResetTokenApply(token: String) = OpenBody:
    env.security.passwordResetToken.read(token) match
      case None =>
        BadRequest.page(views.auth.passwordResetInvalid("Invalid or expired reset link."))
      case Some(userId) =>
        bindForm(passwordApplyForm)(
          _ => BadRequest.page(views.auth.passwordResetApply(token, Some("Password must be at least 4 characters."))),
          newPassword =>
            val clear = ClearPassword(newPassword)
            env.security.pwnedApi.isPwned(clear).flatMap:
              case pwned if pwned.yes =>
                BadRequest.page(
                  views.auth.passwordResetApply(
                    token,
                    Some("This password is known in data breaches. Choose another one.")
                  )
                )
              case _ =>
                env.security.authenticator
                  .setPassword(userId, clear)
                  .flatMap(_ => env.security.sessionStore.closeAllSessionsOf(userId))
                  .inject(Redirect(routes.Auth.login).flashing("success" -> "Password updated. Please log in."))
        )

  def checkYourEmail = Open:
    EmailConfirm.cookie.get(ctx.req).fold(fuccess(Redirect(routes.Auth.signup))): pending =>
      Ok.page:
        views.auth.checkEmail(
          concealedEmail = pending.email.conceal,
          success = ctx.req.flash.get("success"),
          error = ctx.req.flash.get("error")
        )

  def fixEmail = OpenBody:
    EmailConfirm.cookie.get(ctx.req).fold(fuccess(Redirect(routes.Auth.signup))): pending =>
      bindForm(fixEmailForm)(
        _ =>
          BadRequest.page(
            views.auth.checkEmail(
              concealedEmail = pending.email.conceal,
              error = Some("Invalid email address.")
            )
          ),
        emailStr =>
          val email = EmailAddress(EmailAddress(emailStr).normalize.value)
          val tooMany =
            BadRequest.page(views.auth.checkEmail(concealedEmail = pending.email.conceal, error = Some("Too many requests. Try again later.")))
          limit.signup(ctx.ip, tooMany):
            env.user.repo.byEmail(email).flatMap:
              case Some(existing) if existing.id != pending.userId =>
                BadRequest.page(
                  views.auth.checkEmail(
                    concealedEmail = pending.email.conceal,
                    error = Some("Email already in use.")
                  )
                )
              case _ =>
                env.user.repo.updateEmail(pending.userId, email) >> env.user.repo.byId(pending.userId).flatMap:
                  case Some(user) =>
                    val token = env.security.loginToken.generate(email, 30.minutes)
                    val url = s"${env.baseUrl}${routes.Auth.signupConfirmToken(token).url}"
                    env.mailer.automaticEmail.signupConfirm(user, email, url).inject:
                      Redirect(routes.Auth.checkYourEmail)
                        .withCookies(EmailConfirm.cookie.set(user.id, email))
                        .flashing("success" -> "Confirmation email sent.")
                  case None =>
                    Redirect(routes.Auth.signup).toFuccess
      )

  def signupConfirmToken(token: String) = Open:
    env.security.loginToken.read(token).fold(Ok.page(views.auth.loginError("Invalid or expired confirmation link."))): email =>
      val normalized = EmailAddress(email.normalize.value)
      val loginExistingUser = (user: UserModel) =>
        val enableF = if user.enabled.no then env.user.repo.enable(user.id) else funit
        val activeUser = if user.enabled.yes then user else user.copy(enabled = UserEnabled(true))
        val welcomeF =
          if user.enabled.no then env.mailer.automaticEmail.welcomeEmail(activeUser, normalized)(using Lang.defaultLang)
          else funit
        given Context = ctx
        (enableF >> welcomeF >> authenticateUser(activeUser, remember = true)).map:
          _.discardingCookies(EmailConfirm.cookie.clear)

      env.user.repo.byEmail(normalized).flatMap:
        case Some(user) =>
          env.user.deleteRequestRepo.isPending(user.id).flatMap:
            case true =>
              Ok.page(
                views.auth.loginError("This account has a pending deletion request and cannot be reopened online.")
              )
            case false =>
              loginExistingUser(user)
        case None =>
          Ok.page(
            views.auth.loginError("No account exists for this confirmation link. Create an account first.")
          )
