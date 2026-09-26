package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ MoveObservationMeaning, PieceControlPoint, PieceControlState }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation
import play.api.libs.json.*

/** Reads bounded differences in existing matched searches. Neither a search nor an evaluation-cause proof. */
private[runtime] object PrincipalVariationComparison:
  enum MaterialRelation(val wireId: String):
    case Equal extends MaterialRelation("equal")
    case Different extends MaterialRelation("different")
    case Unavailable extends MaterialRelation("unavailable")

  enum Relation(val wireId: String):
    case Same extends Relation("same")
    case Different extends Relation("different")
    case Unavailable extends Relation("unavailable")

  enum QualityBand(val wireId: String):
    case Close extends QualityBand("close")
    case Separated extends QualityBand("separated")

  final class Reading private[PrincipalVariationComparison] (
      val id: String, val answer: CommentaryInvestigation.Answer,
      val materialRelation: MaterialRelation, val eventSequenceRelation: Relation,
      val rootObservationRelation: Relation, val rootDifferenceOccurrences: List[String],
      val participants: List[RelationColoredPieceWitness], val qualityBand: QualityBand,
      private val owner: EvidenceBackedJudgmentPacket,
      private val prepared: ObservedLineProtocol.PreparedPaths,
      private val reference: ObservedLineProtocol.PreparedPath,
      private val alternative: ObservedLineProtocol.PreparedPath
  ):
    def belongsTo(packet: EvidenceBackedJudgmentPacket): Boolean = owner eq packet
    def referenceMove: String = reference.line.ref.rootMove
    def alternativeMove: String = alternative.line.ref.rootMove

    def eligibleQuietFocus: Boolean = materialRelation == MaterialRelation.Equal &&
      qualityBand == QualityBand.Close && rootObservationRelation == Relation.Different

    def candidateJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      require(belongsTo(packet), "a PV reading belongs to its exact packet")
      val root = packet.assembly.playedTransition.get
      val paths = List(reference, alternative)
      val resourceIds = prepared.resourceIds
      def piece(value: RelationColoredPieceWitness): JsObject = Json.obj(
        "position_id" -> root.from.id.get, "side" -> sideName(value.side),
        "role" -> value.role.name, "square" -> value.square.key)
      val text = List(
        Option.when(materialRelation == MaterialRelation.Different)(
          "The provided lines differ in captured or promoted piece-role counts."),
        Option.when(eventSequenceRelation == Relation.Different)(
          "Their observed capture or promotion events differ in timing, pieces, or squares."),
        Option.when(rootObservationRelation == Relation.Different)(
          "Their root moves have different recorded development or control changes.")
      ).flatten match
        case Nil => "The recorded material, event, and direct-mover facets are the same or unavailable."
        case values => values.mkString(" ")
      Json.obj("id" -> id, "kind" -> "comparison", "selection" -> "context",
        "root_occurrence_id" -> root.evidence.id,
        "occurrence_ids" -> (root.evidence.id :: paths.flatMap(_.ids)).distinct,
        "position_ids" -> paths.flatMap(_.positions.flatMap(_.id)).filter(resourceIds).distinct,
        "line_ids" -> paths.map(_.line.ref.id),
        "evidence_ids" -> paths.flatMap(_.certified.map(_._1.ref.id)).distinct,
        "source_refs" -> paths.zip(List(answer.referenceEvaluation, answer.alternativeEvaluation)).map {
          (path, evaluation) => Json.obj("line_id" -> path.line.ref.id, "kind" -> "evaluation",
            "source" -> (evaluation \ "source").as[String], "accepted_ply_count" -> path.moves.size, "stop" -> JsNull)
        },
        "detail" -> Json.obj("type" -> "pv_comparison", "scope" -> "observed_lines",
          "answer" -> CommentaryInvestigation.answerJson(answer),
          "material_relation" -> materialRelation.wireId, "event_sequence_relation" -> eventSequenceRelation.wireId,
          "root_observation_relation" -> rootObservationRelation.wireId,
          "root_difference_occurrence_ids" -> rootDifferenceOccurrences,
          "root_participants" -> participants.map(piece), "quality_band" -> qualityBand.wireId,
          "text" -> (text + " These are conditional observations, not a forced result or the sole cause of an evaluation.")))

  private case class ControlPoint(controller: Option[RelationColoredPieceWitness],
      target: Option[RelationColoredPieceWitness], state: PieceControlState)
  private enum RootFact:
    case Development(actor: RelationColoredPieceWitness, squares: Set[EvidenceSquare])
    case Control(before: ControlPoint, after: ControlPoint)

  private def rootFacts(packet: EvidenceBackedJudgmentPacket, occurrence: String): Set[RootFact] =
    val root = packet.assembly.playedTransition.get.from
    packet.observationsFor(Set(occurrence)).filter(_.transition.from == root)
      .flatMap { value =>
        val original = RelationColoredPieceWitness(value.mover.from, value.mover.beforeRole, value.mover.side)
        def beforeMover(piece: Option[RelationColoredPieceWitness]): Boolean = piece.contains(
          original)
        def afterMover(piece: Option[RelationColoredPieceWitness]): Boolean = piece.contains(
          RelationColoredPieceWitness(value.mover.to, value.mover.afterRole, value.mover.side))
        // Moving the same actor to another square is not itself a different support/control effect.
        // Only that actor's witness is transported back; every other participant stays exact.
        def point(point: PieceControlPoint, after: Boolean): ControlPoint =
          def participant(piece: Option[RelationColoredPieceWitness]) =
            if after && afterMover(piece) then Some(original) else piece
          ControlPoint(participant(point.controller), participant(point.target), point.state)
        value.meaning match
          case MoveObservationMeaning.Development(squares) =>
            Some(RootFact.Development(RelationColoredPieceWitness(value.mover.from,
              value.mover.beforeRole, value.mover.side), squares.toSet))
          case MoveObservationMeaning.PieceControl(before, after)
              if before.state != PieceControlState.Unknown && after.state != PieceControlState.Unknown &&
                (before.state == PieceControlState.Present || after.state == PieceControlState.Present) &&
                (beforeMover(before.controller) || beforeMover(before.target) ||
                  afterMover(after.controller) || afterMover(after.target)) =>
            Some(RootFact.Control(point(before, false), point(after, true)))
          case _ => None
      }.toSet

  private def compare[A](left: Option[A], right: Option[A]): Relation = (left, right) match
    case (Some(a), Some(b)) => if a == b then Relation.Same else Relation.Different
    case _ => Relation.Unavailable

  // Captures/promotions only: different quiet PV moves alone do not create an event difference.
  // Proven recapture identifies the prior capturer; it does not prove a forced or unique response.
  private def events(material: LineMaterialSummary) = material.events.map { event =>
    (event.plyOffset, event.movement,
      event.capture.map(value => (value.side, value.attackerRole, value.capturedRole, value.square)),
      event.promotionGainCp)
  }

  private[runtime] def quality(mover: chess.Color, left: CandidateLineEvaluation, right: CandidateLineEvaluation): Option[QualityBand] =
    def close(value: EvalComparison): Boolean =
      Set(MoveChoiceVerdict.MatchesReference, MoveChoiceVerdict.PlayableLoss)(value.verdict)
    for
      forward <- EvalComparison.fromEvaluations(mover, left, right)
      reverse <- EvalComparison.fromEvaluations(mover, right, left)
    yield if close(forward) && close(reverse) then QualityBand.Close else QualityBand.Separated

  /** Receipts are in issuance order. The latest usable owned receipt wins for each root pair.
    * Score levels are checked by pairedAnswer; this reader never compares scores across works.
    */
  def read(packet: EvidenceBackedJudgmentPacket, referenceMove: String,
      receipts: List[CommentaryInvestigation.EngineCandidates], retainIds: Set[String] = Set.empty,
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None): List[Reading] =
    readAll(packet, List(referenceMove), receipts, retainIds, prepared)

  /** One packet's references share certified paths and occurrence fingerprints. */
  def readAll(packet: EvidenceBackedJudgmentPacket, referenceMoves: List[String],
      receipts: List[CommentaryInvestigation.EngineCandidates], retainIds: Set[String] = Set.empty,
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None): List[Reading] =
    val paths = prepared.getOrElse(ObservedLineProtocol.prepare(packet))
    paths.requireOwner(packet)
    val observations = scala.collection.mutable.Map.empty[String, Set[RootFact]]
    def facts(occurrence: String): Set[RootFact] = observations.getOrElseUpdate(occurrence, rootFacts(packet, occurrence))
    referenceMoves.map(EvidenceRef.normalizeMove).distinct.flatMap(referenceMoveUci =>
      readReference(packet, referenceMoveUci, receipts, retainIds, paths, facts)).distinctBy(_.id)

  private def readReference(packet: EvidenceBackedJudgmentPacket, referenceMoveUci: String,
      receipts: List[CommentaryInvestigation.EngineCandidates], retainIds: Set[String],
      prepared: ObservedLineProtocol.PreparedPaths, facts: String => Set[RootFact]): List[Reading] =
    val ctx = packet.assembly
    val readings = CommentaryInvestigation.preferredReceipts(receipts).flatMap { receipt =>
      receipt.lines.sortBy(line => -line.evaluation.rankingScoreForMover(ctx.input.positionHistory.currentPosition.color))
        .filterNot(_.evaluation.moves.head == referenceMoveUci).flatMap { candidate =>
          for
            answer <- CommentaryInvestigation.pairedAnswer(packet, referenceMoveUci,
              candidate.evaluation.moves.head, receipt.work, receipt.lines, Some(prepared))
            reference <- prepared.path(packet, answer.referenceLineId)
            alternative <- prepared.path(packet, answer.alternativeLineId)
            band <- answer.qualityBand
          yield
            def material(path: ObservedLineProtocol.PreparedPath): Option[LineMaterialSummary] =
              Option.when(path.ids == path.declared && path.positions.forall(position =>
                position.id.exists(prepared.resourceIds)))(path.certified.flatMap(_._2.material)).flatten
            val leftMaterial = material(reference)
            val rightMaterial = material(alternative)
            val materialRelation = compare(leftMaterial.map(CommentarySelection.materialSignature),
              rightMaterial.map(CommentarySelection.materialSignature)) match
                case Relation.Same => MaterialRelation.Equal
                case Relation.Different => MaterialRelation.Different
                case Relation.Unavailable => MaterialRelation.Unavailable
            val leftFacts = facts(answer.referenceOccurrenceId)
            val rightFacts = facts(answer.alternativeOccurrenceId)
            // A missing observation is not a negative fact, nor a claim that the two moves are equivalent.
            val rootRelation = compare(Option.when(leftFacts.nonEmpty)(leftFacts), Option.when(rightFacts.nonEmpty)(rightFacts))
            val differenceIds = Option.when(rootRelation == Relation.Different)(
              List(answer.referenceOccurrenceId, answer.alternativeOccurrenceId)).toList.flatten
            val participants = differenceIds.flatMap { id =>
              ctx.transitions.find(edge => edge.evidence.id == id && ctx.playedTransition.exists(_.from == edge.from))
                .flatMap(edge => packet.observationsFor(Set(id)).find(_.transition == edge).map(_.mover))
                .map(mover => RelationColoredPieceWitness(mover.from, mover.beforeRole, mover.side))
            }.distinct
            new Reading(CommentarySelection.stableId("pv-comparison", List(ctx.playedTransition.get.evidence.id,
              answer.referenceLineId, answer.alternativeLineId, answer.executionKeySha256)), answer,
              materialRelation, compare(leftMaterial.map(events), rightMaterial.map(events)), rootRelation,
              differenceIds, participants, band, packet, prepared, reference, alternative)
        }
    }
    val latestIds = readings.distinctBy(_.alternativeMove).map(_.id).toSet
    readings.filter(reading => latestIds(reading.id) || retainIds(reading.id)).distinctBy(_.id)
      .sortBy(reading => if reading.alternativeMove == ctx.input.playedMoveUci then 0 else 1)

  private def sideName(side: chess.Color): String = if side.white then "white" else "black"
