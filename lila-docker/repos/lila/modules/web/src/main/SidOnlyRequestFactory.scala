package lila.web

import play.api.libs.typedmap.TypedMap
import play.api.mvc.*
import play.api.mvc.request.*

final class SidOnlyRequestFactory(delegate: RequestFactory) extends RequestFactory:

  def createRequestHeader(
      connection: RemoteConnection,
      method: String,
      target: RequestTarget,
      version: String,
      headers: Headers,
      attrs: TypedMap
  ): RequestHeader =
    val request = delegate.createRequestHeader(connection, method, target, version, headers, attrs)
    request.withAttrs:
      request.attrs
        .updated(RequestAttrKey.Session, Cell(Session()))
        .updated(RequestAttrKey.Flash, Cell(Flash()))
