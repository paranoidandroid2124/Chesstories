package controllers

import play.api.data.*
import play.api.data.Forms.*
import play.api.libs.functional.syntax.*
import play.api.libs.json.*
import play.api.mvc.Result

import lila.app.*
import lila.beta.BetaFeedbackApi.{ PriceBand, Submission, SubmitResult, WaitlistState, Willingness }
import lila.core.email.EmailAddress

object BetaFeedback:
  final case class FormInput(
      surface: String,
      feature: String,
      entrypoint: String,
      willingness: String,
      priceBand: Option[String],
      notifyRequested: Boolean,
      email: Option[String],
      notes: Option[String],
      returnTo: String
  )

final class BetaFeedback(env: Env) extends LilaController(env):

  import BetaFeedback.FormInput

  private case class ApiInput(
      surface: String,
      feature: Option[String],
      entrypoint: Option[String],
      willingness: String,
      priceBand: Option[String],
      notifyRequested: Option[Boolean],
      email: Option[String],
      notes: Option[String]
  )

  private given Reads[ApiInput] =
    (
      (__ \ "surface").read[String] and
        (__ \ "feature").readNullable[String] and
        (__ \ "entrypoint").readNullable[String] and
        (__ \ "willingness").read[String] and
        (__ \ "priceBand").readNullable[String] and
        (__ \ "notify").readNullable[Boolean] and
        (__ \ "email").readNullable[String] and
        (__ \ "notes").readNullable[String]
    )(ApiInput.apply)

  private val willingnessKeys = Willingness.keys
  private val priceBandKeys = PriceBand.keys

  private val inputForm = Form(
    mapping(
      "surface" -> default(text, "general"),
      "feature" -> default(text, ""),
      "entrypoint" -> default(text, ""),
      "willingness" -> nonEmptyText.verifying("Choose a response.", willingnessKeys.contains),
      "priceBand" -> optional(text.verifying("Invalid price band.", value => value.trim.isEmpty || priceBandKeys.contains(value.trim.toLowerCase))),
      "notify" -> default(play.api.data.Forms.boolean, false),
      "email" -> optional(text.verifying("Invalid email address.", value => value.trim.isEmpty || EmailAddress.isValid(value.trim))),
      "notes" -> optional(text(maxLength = 800)),
      "returnTo" -> default(text, "")
    )(FormInput.apply)(input =>
      Some(
        (
          input.surface,
          input.feature,
          input.entrypoint,
          input.willingness,
          input.priceBand,
          input.notifyRequested,
          input.email,
          input.notes,
          input.returnTo
        )
      )
    )
  )

  def formPage(
      surface: String,
      feature: String,
      entrypoint: String,
      returnTo: String,
      notify: Boolean
  ) = Open:
    currentAccountEmail.flatMap: accountEmail =>
      val prefilled = inputForm.fill(
        FormInput(
          surface = surface,
          feature = feature,
          entrypoint = entrypoint,
          willingness = Willingness.Maybe.key,
          priceBand = none,
          notifyRequested = notify,
          email = accountEmail.map(_.value),
          notes = none,
          returnTo = sanitizeReturnTo(returnTo).getOrElse("")
        )
      )
      val success = ctx.req.getQueryString("submitted").filter(_ == "1").map(_ => "Thanks. We saved your feedback.")
      Ok.page(renderPage(prefilled, accountEmail, success))

  def submitForm = OpenBody:
    currentAccountEmail.flatMap: accountEmail =>
      bindForm(inputForm)(
        err => BadRequest.page(renderPage(err, accountEmail)),
        input =>
          toSubmission(input).fold(
            err => BadRequest.page(renderPage(inputForm.fill(input).withGlobalError(err), accountEmail)),
            submission =>
              val effectiveEmail = submission.email.orElse(accountEmail)
              if submission.notifyRequested && effectiveEmail.isEmpty then
                BadRequest.page(
                  renderPage(
                    inputForm.fill(input).withError(
                      "email",
                      s"Enter an email address or sign in with an account email to get ${notificationSubject(submission.feature)}."
                    ),
                    accountEmail
                  )
                )
              else
                env.beta.api
                  .submit(ctx.me.map(_.userId), submission.copy(email = effectiveEmail))
                  .map: _ =>
                    htmlRedirect(input)
          )
      )

  def submitApi = OpenBodyOf(parse.json): (bodyCtx: BodyContext[JsValue]) ?=>
    bodyCtx.body.body.validate[ApiInput].fold(
      err => BadRequest(JsError.toJson(err)).toFuccess,
      input =>
        currentAccountEmail.flatMap: accountEmail =>
          toSubmission(input).fold(
            err => BadRequest(Json.obj("ok" -> false, "error" -> err)).toFuccess,
            submission =>
              env.beta.api
                .submit(bodyCtx.me.map(_.userId), submission.copy(email = submission.email.orElse(accountEmail)))
                .map: result =>
                  JsonOk(
                    Json.obj(
                      "ok" -> true,
                      "waitlist" -> result.waitlist.key,
                      "message" -> successMessage(result, submission.feature),
                      "storedEmail" -> result.storedEmail.map(_.value)
                    )
                  )
          )
    )

  private def renderPage(
      form: Form[FormInput],
      accountEmail: Option[EmailAddress],
      success: Option[String] = None
  )(using Context) =
    views.pages.betaFeedback(form, accountEmail, success)

  private def currentAccountEmail(using Context): Fu[Option[EmailAddress]] =
    ctx.me.fold(fuccess(none[EmailAddress]))(me => env.user.api.email(me.userId))

  private def sanitizeReturnTo(returnTo: String): Option[String] =
    Option(returnTo.trim)
      .filter(path => path.startsWith("/") && !path.startsWith("//"))
      .filter(_.nonEmpty)

  private def toSubmission(input: FormInput): Either[String, Submission] =
    for
      willingness <- Willingness.from(input.willingness).toRight("Choose how interested you are in a paid plan.")
      priceBand <- parsePriceBand(input.priceBand)
      email <- parseEmail(input.email)
    yield Submission(
      surface = input.surface,
      feature = input.feature,
      entrypoint = input.entrypoint,
      willingness = willingness,
      priceBand = priceBand,
      notifyRequested = input.notifyRequested,
      email = email,
      notes = input.notes
    )

  private def toSubmission(input: ApiInput): Either[String, Submission] =
    for
      willingness <- Willingness.from(input.willingness).toRight("Invalid willingness value.")
      priceBand <- parsePriceBand(input.priceBand)
      email <- parseEmail(input.email)
    yield Submission(
      surface = input.surface,
      feature = input.feature.getOrElse(""),
      entrypoint = input.entrypoint.getOrElse(""),
      willingness = willingness,
      priceBand = priceBand,
      notifyRequested = input.notifyRequested.getOrElse(false),
      email = email,
      notes = input.notes
    )

  private def parsePriceBand(priceBand: Option[String]): Either[String, Option[PriceBand]] =
    priceBand.map(_.trim).filter(_.nonEmpty) match
      case None => Right(none)
      case Some(value) =>
        PriceBand.from(value).map(_.some).toRight("Invalid price band.")

  private def parseEmail(email: Option[String]): Either[String, Option[EmailAddress]] =
    email.map(_.trim).filter(_.nonEmpty) match
      case None => Right(none)
      case Some(value) =>
        if EmailAddress.isValid(value) then Right(EmailAddress(value).some)
        else Left("Invalid email address.")

  private def htmlRedirect(input: FormInput): Result =
    val target =
      sanitizeReturnTo(input.returnTo)
        .getOrElse(
          routes.BetaFeedback.formPage(
            surface = input.surface,
            feature = input.feature,
            entrypoint = input.entrypoint,
            returnTo = "",
            notify = input.notifyRequested
          ).url
        )
    val separator = if target.contains('?') then "&" else "?"
    Redirect(s"$target${separator}submitted=1")

  private def notificationSubject(feature: String): String =
    if feature.trim.equalsIgnoreCase("launch_notice") then "launch updates"
    else "paid-plan updates"

  private def successMessage(result: SubmitResult, feature: String): String =
    val subject = notificationSubject(feature)
    result.waitlist match
      case WaitlistState.Enrolled =>
        val target = result.storedEmail.map(_.value).getOrElse("your account email")
        s"Thanks. We saved your interest and will notify $target about $subject."
      case WaitlistState.NeedsEmail =>
        s"Thanks. We saved your interest, but we still need an email address to send $subject."
      case WaitlistState.NotRequested =>
        "Thanks. We saved your feedback."
