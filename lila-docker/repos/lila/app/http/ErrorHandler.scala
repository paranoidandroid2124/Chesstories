package lila.app
package http

import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.*
import play.api.mvc.Results.*
import play.api.routing.*
import play.api.{ Configuration, Environment, UsefulException }
import scala.annotation.unused

final class ErrorHandler(
    environment: Environment,
    config: Configuration,
    router: => Router,
    mainC: => controllers.Main
)(using @unused executor: Executor)
    extends DefaultHttpErrorHandler(environment, config, router.some)
    with lila.web.ResponseWriter:

  override def onProdServerError(req: RequestHeader, exception: UsefulException) =
    fuccess(InternalServerError(exception.getMessage))

  override def onClientError(req: RequestHeader, statusCode: Int, msg: String): Fu[Result] =
    if statusCode == 404 && req.headers.get("Accept").exists(_.contains("text/html")) then
      mainC.handlerNotFound(Option(msg).filter(_.nonEmpty))(using req)
    else fuccess(Status(statusCode)(msg))
