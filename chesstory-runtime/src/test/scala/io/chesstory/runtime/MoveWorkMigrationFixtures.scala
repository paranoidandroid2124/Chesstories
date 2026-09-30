package io.chesstory.runtime

import java.nio.file.{ Files, Paths }
import scala.util.Using

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Re-admit existing R1 inputs; response JSON is never used as projection input. */
private[runtime] object MoveWorkMigrationFixtures:
  // The four previously exposed projection regressions travel with the tests.
  // Running this suite must not open a separate research/evaluation corpus.
  private lazy val researchInputs = Using.resource(getClass.getResourceAsStream("/move-work-regressions.json")) {
    stream => Json.parse(stream).as[List[JsObject]]
  }
  private lazy val resourceInputs = Using.resource(getClass.getResourceAsStream("/resource-cases.json")) { stream =>
    (Json.parse(stream) \ "cases").as[List[JsObject]]
  }
  private lazy val certifiedInputs = Json.parse(Files.readString(Paths.get("..", "judgment-evaluation",
    "fixtures", "public-commentary-v6", "certified-observations-produced.json"))).as[List[JsObject]]

  def assemble(history: CanonicalPositionHistory, move: String, paths: List[List[String]] = Nil): JudgmentAssemblyContext =
    MoveReviewAssembler.assembleOccurrences(MoveReviewInputAdmission.admitIndependent(history, move, paths).get)

  def fromFen(fen: String, move: String): JudgmentAssemblyContext =
    assemble(CanonicalPositionHistory.from(fen, Nil, fen).toOption.get, move)

  def example(id: String): JudgmentAssemblyContext =
    val row = researchInputs.find(row => (row \ "id").as[String] == id).get
    val history = CanonicalPositionHistory.from((row \ "initial_fen").as[String],
      (row \ "prefix").as[List[String]], (row \ "fen").as[String]).toOption.get
    assemble(history, (row \ "move").as[String])

  def resource(id: String): JudgmentAssemblyContext =
    val row = resourceInputs.find(row => (row \ "id").as[String] == id).get
    val fen = (row \ "before_fen").as[String]
    val history = CanonicalPositionHistory.from((row \ "initial_fen").asOpt[String].getOrElse(fen),
      (row \ "move_prefix").asOpt[List[String]].getOrElse(Nil),
      (row \ "runtime_fen").asOpt[String].getOrElse(fen)).toOption.get
    assemble(history, (row \ "move").as[String])

  def certified(name: String, move: Option[String] = None): JudgmentAssemblyContext =
    val row = certifiedInputs.find(row => (row \ "name").as[String] == name).get
    fromFen((row \ "fen").as[String], move.getOrElse((row \ "move").as[String]))
