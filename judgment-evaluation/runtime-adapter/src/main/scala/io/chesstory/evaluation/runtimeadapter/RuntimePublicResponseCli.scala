package io.chesstory.evaluation.runtimeadapter

import java.io.{ BufferedReader, InputStreamReader, PrintWriter }
import java.nio.charset.StandardCharsets

import io.chesstory.runtime.RuntimeProtocol
import play.api.libs.json.Json

/** Development-only JSONL transport for RuntimeProtocol's public response. */
object RuntimePublicResponseCli:
  def main(args: Array[String]): Unit =
    if args.nonEmpty then
      throw IllegalArgumentException("RuntimePublicResponseCli does not accept arguments")

    val reader = BufferedReader(InputStreamReader(System.in, StandardCharsets.UTF_8))
    val writer = PrintWriter(System.out, true, StandardCharsets.UTF_8)
    var requestLine = reader.readLine()
    while requestLine != null do
      val result = RuntimeProtocol.evaluate(requestLine.getBytes(StandardCharsets.UTF_8))
      writer.println(Json.stringify(Json.obj(
        "http_status" -> result.httpStatus,
        "body" -> result.body
      )))
      requestLine = reader.readLine()
