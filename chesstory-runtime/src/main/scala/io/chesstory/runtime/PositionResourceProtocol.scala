package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.PositionResourceView
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Compact evidence catalogues are indexed by historical position, not FEN.
  * The view chooses a square from these records without calculating chess.
  */
private[runtime] object PositionResourceProtocol:
  def json(view: PositionResourceView): JsObject =
    def color(side: chess.Color): String = if side.white then "white" else "black"
    def piece(value: RelationColoredPieceWitness): JsObject = Json.obj(
      "side" -> color(value.side), "role" -> value.role.name, "square" -> value.square.key)
    // A source is resolved inside this exact position's canonical relation index.
    // The pair (position id, semantic id) retains its historical owner without
    // repeating the long occurrence prefix in every board fact reference.
    val records = view.records.collect {
      case EvidenceRecord(_, fact: RelationFactEvidence, _) => (fact.semanticId, fact.detail)
    }
    val controls = records.collect {
      case (ref, RelationWitnessDetail.GeometricControl(side, square, role, target)) =>
        (RelationColoredPieceWitness(square, role, side), target, ref)
    }.groupBy(_._1).toList.sortBy { case (controller, _) => (color(controller.side), controller.square.key) }
      .map { case (controller, entries) =>
        val ordered = entries.sortBy(_._2.key)
        Json.obj("piece" -> piece(controller), "squares" -> ordered.map(_._2.key),
          "source_ids" -> ordered.map(_._3))
      }
    val rays = records.collect {
      case (ref, RelationWitnessDetail.RayBarrier(side, square, role, occupants, geometry)) =>
        Json.obj("piece" -> piece(RelationColoredPieceWitness(square, role, side)),
          "squares" -> geometry.squares.map(_.key), "occupants" -> occupants.map(piece),
          "source_id" -> ref)
    }.sortBy(row => ((row \ "piece" \ "square").as[String], (row \ "squares").as[List[String]].mkString))
    val checking = view.checkingMoves.map(_.map(_.sourceId).toSet)
    val legal = records.collect {
      case (ref, detail: RelationWitnessDetail.LegalMove) =>
        Json.obj("uci" -> detail.moveUci, "destination" -> detail.destinationSquare.key, "source_id" -> ref) ++
          detail.capture.fold(Json.obj())(capture => Json.obj("captured_square" -> capture.capturedSquare.key)) ++
          checking.fold(Json.obj())(sources => Json.obj("gives_check" -> sources(ref)))
    }.sortBy(row => (row \ "uci").as[String])
    val advances = records.collect {
      case (ref, RelationWitnessDetail.PawnAdvanceAffordance(side, square, destination, traversed)) =>
        Json.obj("piece" -> piece(RelationColoredPieceWitness(square, EvidencePieceRole("pawn"), side)),
          "destination" -> destination.key, "traversed_squares" -> traversed.map(_.key),
          "source_id" -> ref)
    }.sortBy(row => ((row \ "piece" \ "square").as[String], (row \ "destination").as[String]))
    val pawns = view.pawnStates
    Json.obj("id" -> view.position.id.get, "fen" -> view.position.fen,
      "premise_id" -> view.occurrence.id,
      "controls" -> Json.obj("complete" -> view.completeKinds(RelationFactKind.GeometricControl), "pieces" -> controls),
      "rays" -> Json.obj("complete" -> view.completeKinds(RelationFactKind.RayBarrier), "lines" -> rays),
      "legal_moves" -> Json.obj("complete" -> view.completeKinds(RelationFactKind.LegalMove), "moves" -> legal),
      "pawn_states" -> Json.obj(
        "complete" -> pawns.forall(pawn => pawn.passed.isDefined && pawn.isolated.isDefined && pawn.doubled.isDefined),
        "pawns" -> pawns.map(pawn => Json.obj("square" -> pawn.square.key,
          "passed" -> pawn.passed, "isolated" -> pawn.isolated, "doubled" -> pawn.doubled,
          "source_ids" -> pawn.sourceIds))),
      "pawn_files" -> Json.obj("complete" -> view.completeKinds(RelationFactKind.PawnFileGroup),
        "files" -> view.pawnFiles.map(file => Json.obj("file" -> file.file.key,
          "occupancy" -> file.occupancy, "source_ids" -> file.sourceIds))),
      "pawn_advances" -> Json.obj("complete" -> view.completeKinds(RelationFactKind.PawnAdvanceAffordance), "paths" -> advances))
