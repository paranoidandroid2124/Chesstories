package views

import lila.app.UiEnv.{ *, given }
import lila.core.user.UserDelete
import lila.pref.{ PieceSet, Pref, Theme }
import scala.annotation.unused

object account:

  private def flashMessages(using ctx: Context) =
    val flashes = List(
      ctx.flash("success").map(msg => div(cls := "flash flash-success")(msg)),
      ctx.flash("warning").map(msg => div(cls := "flash flash-warning")(msg)),
      ctx.flash("error").map(msg => div(cls := "flash flash-error")(msg)),
      ctx.flash("failure").map(msg => div(cls := "flash flash-error")(msg))
    ).flatten
    frag(flashes*)

  private def settingsPage(title: String, activeTab: String)(content: Context ?=> scalatags.Text.all.Frag)(using ctx: Context) =
    Page(s"$title - Chesstory")
      .css("account")
      .wrap: _ =>
        main(cls := "account-page")(
          div(cls := "account-container")(
            div(cls := "account-sidebar")(
              div(cls := "account-title")("Settings"),
              st.nav(cls := "account-nav")(
                a(href := routes.Account.profile.url, cls := (if activeTab == "profile" then "active" else ""))("Profile"),
                a(href := routes.Account.passwd.url, cls := (if activeTab == "security" then "active" else ""))("Security"),
                a(href := routes.Pref.form("display").url, cls := (if activeTab == "preferences" then "active" else ""))("Preferences"),
                a(href := routes.Account.close.url, cls := (if activeTab == "close" then "active" else ""))("Close Account"),
                a(href := routes.Account.delete.url, cls := (if activeTab == "delete" then "active" else ""))("Data Deletion")
              )
            ),
            div(cls := "account-content")(
              div(cls := "account-card")(
                content
              )
            )
          )
        )

  def profile(user: User, form: play.api.data.Form[lila.core.userId.UserName])(using ctx: Context) =
    settingsPage("Profile Settings", "profile"):
      frag(
        h2("Profile Settings"),
        p(cls := "desc")("Manage your Chesstory identity."),
        hr,
        flashMessages,
        form.globalError.map: e =>
          div(cls := "form-error")(e.message)
        ,
        postForm(action := routes.Account.profileApply, cls := "account-form")(
          div(cls := "form-group")(
            label(attr("for") := "username")("Username"),
            input(
              id := "username",
              name := "username",
              tpe := "text",
              value := form("username").value | user.username.value,
              placeholder := "New username",
              required := true
            ),
            form("username").error.map: e =>
              span(cls := "field-error")(e.message)
            ,
            p(cls := "help")("You can change your username casing or minor formatting. Full name changes are not allowed.")
          ),
          div(cls := "form-group")(
            label("Email Address"),
            input(tpe := "email", value := user.email.map(_.value) | "", disabled := true),
            p(cls := "help")("Email, password, and login options are managed in Security settings.")
          ),
          div(cls := "form-actions")(
            button(tpe := "submit", cls := "btn-primary")("Save Changes")
          )
        )
      )

  private def fieldError(form: play.api.data.Form[?], key: String) =
    form(key).error.map: e =>
      span(cls := "field-error")(e.message)

  def security(
      user: User,
      hasPassword: Boolean,
      emailForm: play.api.data.Form[?],
      passwordForm: play.api.data.Form[?]
  )(using ctx: Context) =
    settingsPage("Security Settings", "security"):
      val passwordTitle = if hasPassword then "Change Password" else "Set Password"
      val passwordSubmit = if hasPassword then "Update Password" else "Set Password"
      frag(
        h2("Security Settings"),
        p(cls := "desc")("Manage your email address, password, and login methods."),
        hr,
        flashMessages,
        div(cls := "account-form")(
          h3("Email Address"),
          p(cls := "help")("Current login email"),
          input(tpe := "email", value := user.email.map(_.value) | "", disabled := true),
          hr,
          h3("Change Email"),
          emailForm.globalError.map: e =>
            div(cls := "form-error")(e.message)
          ,
          postForm(action := routes.Account.emailApply, cls := "account-form")(
            div(cls := "form-group")(
              label(attr("for") := "email")("New Email Address"),
              input(
                id := "email",
                name := "email",
                tpe := "email",
                value := emailForm("email").value | "",
                placeholder := "new@email.com",
                autocomplete := "email",
                required := true
              ),
              fieldError(emailForm, "email"),
              p(cls := "help")("We will send a confirmation link to the new address before changing your login email.")
            ),
            div(cls := "form-actions")(
              button(tpe := "submit", cls := "btn-primary")("Send Confirmation")
            )
          ),
          hr,
          h3(passwordTitle),
          passwordForm.globalError.map: e =>
            div(cls := "form-error")(e.message)
          ,
          postForm(action := routes.Account.passwdApply, cls := "account-form")(
            if hasPassword then
              div(cls := "form-group")(
                label(attr("for") := "currentPassword")("Current Password"),
                input(
                  id := "currentPassword",
                  name := "currentPassword",
                  tpe := "password",
                  autocomplete := "current-password",
                  required := true
                ),
                fieldError(passwordForm, "currentPassword")
              )
            else
              p(cls := "help")("This account does not have a password yet. Set one now to enable password login."),
            div(cls := "form-group")(
              label(attr("for") := "newPassword")("New Password"),
              input(
                id := "newPassword",
                name := "newPassword",
                tpe := "password",
                autocomplete := "new-password",
                required := true
              ),
              fieldError(passwordForm, "newPassword")
            ),
            div(cls := "form-group")(
              label(attr("for") := "confirmPassword")("Confirm New Password"),
              input(
                id := "confirmPassword",
                name := "confirmPassword",
                tpe := "password",
                autocomplete := "new-password",
                required := true
              ),
              fieldError(passwordForm, "confirmPassword")
            ),
            div(cls := "form-actions")(
              button(tpe := "submit", cls := "btn-primary")(passwordSubmit)
            )
          )
        )
      )

  def pref(
      @unused user: User,
      @unused form: play.api.data.Form[lila.pref.Pref]
  )(using ctx: Context) =
    settingsPage("Preferences", "preferences"):
      val pref = ctx.pref
      frag(
        h2("Preferences"),
        p(cls := "desc")("Appearance and board settings for Chesstory."),
        hr,
        flashMessages,
        postForm(action := routes.Pref.formApply, cls := "account-form")(
          div(cls := "form-group")(
            label(attr("for") := "bg")("Site theme"),
            select(id := "bg", name := "bg")(
              Pref.Bg.choices.map: (bg, labelText) =>
                val v = Pref.Bg.asString.getOrElse(bg, "dark")
                option(value := v, if v == pref.currentBg then selected := true else emptyFrag)(labelText)
            ),
            p(cls := "help")("Controls the overall site appearance, including light, dark, and device theme modes.")
          ),
          div(cls := "form-group")(
            label(attr("for") := "theme")("Board theme (2D)"),
            select(id := "theme", name := "theme")(
              Theme.all.map: t =>
                option(value := t.name, if t.name == pref.theme then selected := true else emptyFrag)(t.name)
            )
          ),
          div(cls := "form-group")(
            label(attr("for") := "pieceSet")("Pieces (2D)"),
            select(id := "pieceSet", name := "pieceSet")(
              PieceSet.all.map: ps =>
                option(value := ps.name, if ps.name == pref.pieceSet then selected := true else emptyFrag)(ps.name)
            )
          ),
          div(cls := "form-actions")(
            button(tpe := "submit", cls := "btn-primary")("Save")
          )
        )
      )

  def close(@unused user: User)(using ctx: Context) =
    settingsPage("Close Account", "close"):
      frag(
        h2(cls := "danger")("Close Account"),
        p("Closing your account disables sign-in and hides your profile."),
        div(cls := "warning-box")(
          strong("Warning: "),
          "This disables sign-in. If you need the account reopened later, contact support from the same email address. This page does not erase your saved game reviews."
        ),
        postForm(action := routes.Account.closeConfirm, cls := "mt-2")(
          button(tpe := "submit", cls := "btn-danger")("I understand, close my account")
        )
      )

  def delete(
      @unused user: User,
      pendingRequest: Option[UserDelete],
      form: play.api.data.Form[String]
  )(using ctx: Context) =
    settingsPage("Data Deletion Request", "delete"):
      frag(
        h2(cls := "danger")("Permanent Data Deletion Request"),
        p("Permanent erasure is reviewed before it is completed. Submitting this form will immediately close your account and record your deletion request."),
        pendingRequest.map: request =>
          div(cls := "warning-box")(
            strong("Request already pending: "),
            s"A deletion request was already recorded on ${request.requested}."
          )
        ,
        div(cls := "danger-box")(
          strong("CRITICAL WARNING: "),
          "Some records may remain until the request is processed, including operational logs and backups retained for security or recovery windows."
        ),
        form.globalError.map: e =>
          div(cls := "form-error")(e.message)
        ,
        postForm(action := routes.Account.deleteConfirm, cls := "mt-2")(
          div(cls := "form-group")(
            label(attr("for") := "confirm")("Type \"DELETE\" to confirm:"),
            input(
              id := "confirm",
              name := "confirm",
              tpe := "text",
              value := form("confirm").value | "",
              required := true,
              pattern := "DELETE"
            ),
            fieldError(form, "confirm")
          ),
          button(tpe := "submit", cls := "btn-danger")("Submit deletion request")
        )
      )
