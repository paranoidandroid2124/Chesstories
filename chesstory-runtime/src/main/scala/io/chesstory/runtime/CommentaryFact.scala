package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** A finite, explicitly scoped question about existing facts. No new chess fact authority. */
object CommentaryFact:
  val MaximumPly = 80
  final case class Piece(side: String, role: String, square: String):
    private[runtime] def witness: RelationColoredPieceWitness = RelationColoredPieceWitness(
      EvidenceSquare(square), EvidencePieceRole(role), if side == "white" then chess.White else chess.Black)
    def json: JsObject = Json.obj("side" -> side, "role" -> role, "square" -> square)

  sealed trait PathSource
  final case class Declared(movesUci: List[String]) extends PathSource
  final case class Comparison(requirementId: String, arm: String) extends PathSource
  final case class Premise(inquiryId: String, premiseId: String) extends PathSource
  final case class Path(binding: String, source: PathSource, extend: Boolean = false):
    def json: JsObject = Json.obj("binding" -> binding, "extend" -> extend, "source" -> (source match
      case Declared(moves) => Json.obj("kind" -> "declared", "moves_uci" -> moves)
      case Comparison(id, arm) => Json.obj("kind" -> "comparison", "requirement_id" -> id, "arm" -> arm)
      case Premise(inquiry, premise) => Json.obj("kind" -> "premise", "inquiry_id" -> inquiry, "premise_id" -> premise)))

  sealed trait Point:
    def horizon: Int
    def json: JsObject
  final case class Ply(count: Int) extends Point:
    def horizon = count
    def json = Json.obj("kind" -> "ply", "ply_count" -> count)
  final case class Event(kind: String, ordinal: Int, phase: String, fromPly: Int, throughPly: Int,
      actor: Option[Piece] = None, target: Option[Piece] = None, square: Option[String] = None) extends Point:
    def horizon = throughPly
    def json = Json.obj("kind" -> "event", "event" -> kind, "ordinal" -> ordinal, "phase" -> phase,
      "from_ply" -> fromPly, "through_ply" -> throughPly) ++
      actor.fold(Json.obj())(value => Json.obj("actor" -> value.json)) ++
      target.fold(Json.obj())(value => Json.obj("target" -> value.json)) ++
      square.fold(Json.obj())(value => Json.obj("square" -> value))

  final case class Predicate(kind: String, subject: Option[Piece] = None, target: Option[Piece] = None,
      square: Option[String] = None, side: Option[String] = None, property: Option[String] = None):
    def json: JsObject = Json.obj("kind" -> kind) ++
      subject.fold(Json.obj())(value => Json.obj("subject" -> value.json)) ++
      target.fold(Json.obj())(value => Json.obj("target" -> value.json)) ++
      square.fold(Json.obj())(value => Json.obj("square" -> value)) ++
      side.fold(Json.obj())(value => Json.obj("side" -> value)) ++
      property.fold(Json.obj())(value => Json.obj("property" -> value))

  private val roles = Set("pawn", "knight", "bishop", "rook", "queen", "king")
  private val sides = Set("white", "black")
  private def square(value: String) = value.matches("[a-h][1-8]")
  private def piece(value: Piece) = sides(value.side) && roles(value.role) && square(value.square)
  private def id(value: String) = RuntimeProtocol.acceptsRequestId(value)
  def validPath(value: Path): Boolean = id(value.binding) && (value.source match
    case Declared(moves) => moves.size <= MaximumPly && moves.forall(EngineLineAdmission.acceptsCanonicalLowercaseUci)
    case Comparison(reference, arm) => id(reference) && Set("reference", "alternative")(arm)
    case Premise(inquiry, premise) => inquiry.nonEmpty && inquiry.length <= 512 && premise.nonEmpty && premise.length <= 512)
  def validPoint(value: Point): Boolean = value match
    case Ply(count) => count >= 0 && count <= MaximumPly
    case Event(kind, ordinal, phase, from, through, actor, target, destination) =>
      Set("move", "capture", "promotion", "check", "castling")(kind) && ordinal >= 1 && ordinal <= MaximumPly &&
        Set("before", "after")(phase) && from >= 0 && through > from && through <= MaximumPly &&
        actor.forall(piece) && target.forall(piece) && (target.isEmpty || kind == "capture") &&
        destination.forall(square) && (destination.isEmpty || kind == "move")

  /** These are state values, not utility scores. Removed and not-turn remain distinguishable. */
  def values(predicate: Predicate): Set[String] = predicate.kind match
    case "presence" => Set("present", "removed")
    case "location" => chess.Square.all.map(_.key).toSet + "removed"
    case "role" => roles + "removed"
    case "control" => Set("present", "absent", "actor_removed")
    case "support" => Set("present", "absent", "actor_removed", "target_removed")
    case "legal_move" => Set("legal", "illegal", "not_turn", "actor_removed")
    case "legal_capture" => Set("legal", "illegal", "not_turn", "actor_removed", "target_removed")
    case "check" => Set("present", "absent")
    case "pawn_property" => Set("true", "false", "not_pawn", "actor_removed")
    case "pawn_advance" => Set("present", "absent", "not_pawn", "actor_removed")
    case "file_occupancy" => Set("open", "white_pawns_only", "black_pawns_only", "both_sides")
    case "direct_support_use" => Set("true", "false")
    case _ => Set.empty

  def validPredicate(value: Predicate): Boolean =
    val fields = List(value.subject.map(_ => "subject"), value.target.map(_ => "target"),
      value.square.map(_ => "square"), value.side.map(_ => "side"), value.property.map(_ => "property")).flatten.toSet
    val expected = value.kind match
      case "presence" | "location" | "role" => Set("subject")
      case "control" | "pawn_advance" => Set("subject", "square")
      case "support" | "legal_capture" | "direct_support_use" => Set("subject", "target")
      case "legal_move" => Set("subject", "square") ++ value.property.map(_ => "property")
      case "check" => Set("side")
      case "pawn_property" => Set("subject", "property")
      case "file_occupancy" => Set("property")
      case _ => Set("unsupported")
    fields == expected && value.subject.forall(piece) && value.target.forall(piece) &&
      value.square.forall(square) && value.side.forall(sides) && (value.kind match
        case "support" | "direct_support_use" => value.subject.get.side == value.target.get.side && value.subject != value.target
        case "legal_capture" => value.subject.get.side != value.target.get.side
        case "legal_move" => value.property.forall(Set("queen", "rook", "bishop", "knight"))
        case "pawn_property" => Set("passed", "isolated", "doubled")(value.property.get)
        case "file_occupancy" => value.property.exists(_.matches("[a-h]"))
        case _ => true)

  private[runtime] final case class Sample(id: String, lineId: String, binding: String, moves: List[String],
      fromPly: Int, throughPly: Int,
      value: Option[String], state: String, sourceIds: List[String] = Nil, reason: Option[String] = None):
    def json: JsObject = Json.obj("id" -> id, "line_id" -> lineId, "binding" -> binding,
      "prefix_ply_count" -> moves.size, "from_ply" -> fromPly, "through_ply" -> throughPly, "value" -> value,
      "state" -> state, "source_ids" -> sourceIds, "reason" -> reason)

  private[runtime] final case class Need(kind: String, reason: String, prefixMoves: List[String] = Nil,
      requirementId: Option[String] = None, horizon: Option[Int] = None):
    def json: JsObject = Json.obj("kind" -> kind, "reason" -> reason, "prefix_moves_uci" -> prefixMoves,
      "requirement_id" -> requirementId, "horizon" -> horizon)

  private[runtime] final case class Proof(bindings: Map[String, List[String]], sampleIds: List[String],
      requirementIds: List[String]):
    def json: JsObject = Json.obj("bindings" -> JsObject(bindings.toList.sortBy(_._1).map((id, moves) => id -> Json.toJson(moves))),
      "sample_ids" -> sampleIds.distinct, "requirement_ids" -> requirementIds.distinct)

  private[runtime] final case class Result(state: String, samples: List[Sample] = Nil, needs: List[Need] = Nil,
      proofs: List[Proof] = Nil, reason: Option[String] = None):
    def json: JsObject = Json.obj("state" -> state, "samples" -> samples.map(_.json), "needs" -> needs.distinct.map(_.json),
      "proofs" -> proofs.map(_.json), "reason" -> reason)
