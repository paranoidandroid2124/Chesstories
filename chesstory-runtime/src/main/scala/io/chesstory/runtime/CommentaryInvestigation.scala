package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.LegalMoveResource
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AdmittedCandidateLine, CandidateLineEvaluation, CanonicalPositionHistory }
import play.api.libs.json.*

/** Root-choice questions over admitted owners, not another move generator or engine client. */
private[runtime] object CommentaryInvestigation:
  enum Kind(val wireId: String):
    case EngineAlternative extends Kind("engine_alternative")
    case CaptureOption extends Kind("capture_option")
    case SameDestination extends Kind("same_destination")
    case SameActorChoice extends Kind("same_actor_choice")
    case TargetRelocation extends Kind("target_relocation")
    case AttackerCapture extends Kind("attacker_capture")
    case TargetSupport extends Kind("target_support")

  case class EngineCandidates(work: IssuedEngineWork, lines: List[AdmittedCandidateLine])

  /** Prefer the strongest owned comparison; recency only breaks equal-depth ties. No scores are merged. */
  def preferredReceipts(receipts: List[EngineCandidates]): List[EngineCandidates] =
    receipts.reverse.sortBy(receipt => -receipt.lines.flatMap(_.evaluation.engineLine.map(_.depth))
      .minOption.getOrElse(receipt.work.requiredDepth))

  /** Same candidate pool, explicit evidence tiers. Legal sibling identity alone does not justify a probe. */
  def investigationTier(question: Question): Int =
    if question.reasons.exists(reason => Set(Kind.TargetRelocation, Kind.AttackerCapture, Kind.TargetSupport)(reason.kind)) then 0
    else if question.orderingCandidateIds.nonEmpty then 1
    else 2

  case class Reason(kind: Kind, evidence: EvidenceRef, sourceId: Option[String], sourceLineId: Option[String], anchorMove: String,
      actor: RelationColoredPieceWitness, capture: Option[RelationColoredPieceWitness],
      promotionRole: Option[String], respondsToCheck: Boolean,
      sourceName: String = "legal_root", target: Option[RelationColoredPieceWitness] = None,
      pressureSourceIds: List[String] = Nil)

  // The question joins these roots' existing observations to the alternative's evaluation/path.
  // Family membership does not assert equal purpose or establish an evaluation cause.
  case class Question(id: String, rootOccurrenceId: String, positionId: String,
      referenceMove: String, alternativeMove: String, reasons: List[Reason], anchorOccurrenceIds: List[String],
      orderingSourceIds: List[String], orderingCandidateIds: List[String] = Nil)

  case class Answer(workId: String, executionKeySha256: String, engineProfile: String, requiredDepth: Int,
      referenceDepth: Option[Int], alternativeDepth: Option[Int], evaluationRelation: String,
      referenceLineId: String, alternativeLineId: String, referenceEvaluation: JsObject, alternativeEvaluation: JsObject,
      referenceOccurrenceId: String, alternativeOccurrenceId: String)

  /** The caller supplies same-work ownership; packet evaluation observations alone do not retain it. */
  def discover(packet: EvidenceBackedJudgmentPacket, referenceMove: String,
      sameWork: Option[EngineCandidates] = None,
      pvComparisons: List[PrincipalVariationComparison.Reading] = Nil,
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None): List[Question] =
    val ctx = packet.assembly
    val paths = prepared.getOrElse(ObservedLineProtocol.prepare(packet))
    paths.requireOwner(packet)
    val reference = EvidenceRef.normalizeMove(referenceMove)
    val actual = ctx.input.playedMoveUci
    val root = ctx.playedTransition
    root.toList.flatMap { edge =>
      packet.resources.find(_.position == edge.from).toList.flatMap { view =>
        val legal = view.legalRows.getOrElse(Nil)
        val byMove = legal.map(value => value.move -> value).toMap
        if !byMove.contains(reference) || !byMove.contains(actual) then Nil
        else
          val anchors = List(actual, reference).distinct.flatMap(byMove.get)
          val checked = ctx.input.positionHistory.currentPosition.check.yes
          def reason(kind: Kind, value: LegalMoveResource, anchor: String, sourceLineId: Option[String] = None): Reason =
            Reason(kind, view.occurrence, Some(value.sourceId), sourceLineId, anchor,
              value.actor, value.capture, value.promotionRole, checked,
              sourceName = if kind == Kind.EngineAlternative then "engine_pv" else "legal_root")
          val observed = sameWork.filter(value => ownsRoot(packet, value.work, value.lines)).toList.flatMap { batch =>
            batch.lines.sortBy(line => -line.evaluation.rankingScoreForMover(ctx.input.positionHistory.currentPosition.color))
              .flatMap { admitted =>
                val move = admitted.evaluation.moves.head
                for
                  value <- byMove.get(move).toList
                  line <- boundLine(packet, admitted, Some(paths)).toList
                  if ctx.input.evaluationObservations.exists(value =>
                    value.evaluation == admitted.evaluation && sourceForWork(value.source, batch.work))
                yield move -> reason(Kind.EngineAlternative, value, reference, Some(line.ref.id))
              }
          }
          val generated = legal.flatMap { value =>
            Option.when(value.capture.nonEmpty)(value.move -> reason(Kind.CaptureOption, value, reference)).toList ++
              anchors.flatMap { anchor =>
                val sameDestination = value.actor.side == anchor.actor.side && value.actor.role == anchor.actor.role &&
                  value.to == anchor.to && value.actor.square != anchor.actor.square
                val sameActor = value.actor == anchor.actor && value.actor.role.name != "pawn" && value.capture.isEmpty
                Option.when(sameDestination)(value.move -> reason(Kind.SameDestination, value, anchor.move)).toList ++
                  Option.when(sameActor)(value.move -> reason(Kind.SameActorChoice, value, anchor.move)).toList
              }
          }
          val defense = view.attackedTargets(ctx.input.positionHistory.currentPosition.color).flatMap { (target, pressures) =>
            val attackerSquares = pressures.flatMap(_.detail match
              case RelationWitnessDetail.GeometricControl(_, square, _, _) => Some(square)
              case _ => None).toSet
            val supportIds = view.supportCandidates(target).map(_.semanticId).toSet
            legal.flatMap { value =>
              val kinds = Option.when(value.actor == target)(Kind.TargetRelocation).toList ++
                Option.when(value.capture.exists(piece => piece.side != target.side && attackerSquares(piece.square)))(Kind.AttackerCapture).toList ++
                Option.when(supportIds(value.sourceId))(Kind.TargetSupport).toList
              kinds.map(kind => value.move -> reason(kind, value, reference).copy(sourceName = "attacked_target",
                target = Some(target), pressureSourceIds = pressures.map(_.semanticId).distinct.sorted))
            }
          }
          (observed ++ generated ++ defense).filterNot { (move, _) => move == actual || move == reference }
            .groupMap(_._1)(_._2).toList.map { (move, triggers) =>
              // An observed functional contrast motivates another legal choice of the same
              // exact root actor. PV membership, a child event, or missing facts cannot do so.
              val linked = pvComparisons.filter(reading => reading.belongsTo(packet) && reading.eligibleQuietFocus &&
                byMove(move).capture.isEmpty && triggers.exists(reason =>
                  Set(Kind.SameActorChoice, Kind.SameDestination)(reason.kind) &&
                    reading.participants.contains(reason.actor))).map(_.id).distinct
              Question(CommentarySelection.stableId("inquiry", List(edge.evidence.id, reference, move)),
                edge.evidence.id, edge.from.id.get, reference, move,
                triggers.distinct.sortBy(value => (value.kind.ordinal, value.anchorMove, value.evidence.id)),
                anchors.flatMap(anchor => ctx.legalLines.filter(_.ref.rootMove == anchor.move)
                  .flatMap(line => ctx.lineTransitionIds.get(line.ref).flatMap(_.headOption))).distinct,
                Nil,
                linked)
            }.sortBy { question =>
              // Extend an observed functional contrast before an unanchored new hypothesis.
              // Victim values and current pawn control cannot establish post-move strength.
              // UCI only makes an otherwise unresolved sampling tie reproducible.
              (if question.orderingCandidateIds.nonEmpty then 0 else 1, question.alternativeMove)
            }
      }
    }

  private def ownsRoot(packet: EvidenceBackedJudgmentPacket, work: IssuedEngineWork,
      lines: List[AdmittedCandidateLine]): Boolean =
    ownsSearch(packet.assembly.input.positionHistory, work, lines)

  def ownsSearch(history: CanonicalPositionHistory, work: IssuedEngineWork,
      lines: List[AdmittedCandidateLine]): Boolean =
    work.enginePositionInitialFen == history.initialFen && work.enginePositionMovesUci == history.movePrefixUci &&
      work.searchFen == history.currentFen && work.commentaryLineOriginFen == history.currentFen &&
      work.commentaryLinePrefixUci.isEmpty &&
      lines.forall(_.continuationAfter(history).nonEmpty) &&
      EngineLineAdmission.acceptsReturnedRoots(work, lines.flatMap(_.evaluation.moves.headOption))

  private def boundLine(packet: EvidenceBackedJudgmentPacket, admitted: AdmittedCandidateLine,
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None): Option[LegalLineNode] =
    prepared.getOrElse(ObservedLineProtocol.prepare(packet)).certifiedPath(packet, admitted.evaluation.moves).map(_.line)

  /** The same evaluation-level guard applies to exploratory and assessment-refresh pairs. */
  def comparisonReady(work: IssuedEngineWork, evaluations: List[AdmittedCandidateLine]): Boolean =
    evaluations.size == 2 && candidatesReady(work, evaluations)

  /** A survey can return one to three lines; comparison callers still require exactly two. */
  def candidatesReady(work: IssuedEngineWork, evaluations: List[AdmittedCandidateLine]): Boolean =
    val depths = evaluations.flatMap(_.evaluation.engineLine.map(_.depth))
    evaluations.nonEmpty && evaluations.size <= work.multiPv && evaluations.forall(_.evaluation.comparisonReady) &&
      depths.forall(_ >= work.requiredDepth) && depths.distinct.size <= 1

  def evaluationRelation(mover: chess.Color, reference: CandidateLineEvaluation,
      alternative: CandidateLineEvaluation): String =
    val comparison = java.lang.Double.compare(alternative.rankingScoreForMover(mover),
      reference.rankingScoreForMover(mover))
    if comparison > 0 then "better" else if comparison < 0 then "worse" else "equal"

  /** Answer only from one owned search. An older/deeper unrelated score is not a matched comparison. */
  def answer(question: Question, work: IssuedEngineWork, lines: List[AdmittedCandidateLine],
      packet: EvidenceBackedJudgmentPacket,
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None): Option[Answer] =
    packet.assembly.playedTransition.filter(root =>
      root.evidence.id == question.rootOccurrenceId && root.from.id.contains(question.positionId)).flatMap { _ =>
      pairedAnswer(packet, question.referenceMove, question.alternativeMove, work, lines, prepared)
    }

  def pairedAnswer(packet: EvidenceBackedJudgmentPacket, referenceMove: String, alternativeMove: String,
      work: IssuedEngineWork, lines: List[AdmittedCandidateLine],
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None): Option[Answer] =
    val ctx = packet.assembly
    lazy val paths = prepared.getOrElse(ObservedLineProtocol.prepare(packet))
    for
      _ <- ctx.playedTransition
      if referenceMove != alternativeMove
      if ownsRoot(packet, work, lines)
      reference <- lines.find(_.evaluation.moves.head == referenceMove)
      alternative <- lines.find(_.evaluation.moves.head == alternativeMove)
      if comparisonReady(work, List(reference, alternative))
      referenceDepth = reference.evaluation.engineLine.map(_.depth)
      alternativeDepth = alternative.evaluation.engineLine.map(_.depth)
      referencePath <- boundLine(packet, reference, Some(paths))
      alternativePath <- boundLine(packet, alternative, Some(paths))
      referenceOccurrence <- ctx.lineTransitionIds.get(referencePath.ref).flatMap(_.headOption)
      alternativeOccurrence <- ctx.lineTransitionIds.get(alternativePath.ref).flatMap(_.headOption)
      referenceSource <- ctx.input.evaluationObservations.find(value =>
        value.evaluation == reference.evaluation && sourceForWork(value.source, work))
      alternativeSource <- ctx.input.evaluationObservations.find(value =>
        value.evaluation == alternative.evaluation && sourceForWork(value.source, work))
    yield
      val mover = ctx.input.positionHistory.currentPosition.color
      Answer(work.workId, work.executionKeySha256, work.engineProfile.wireId, work.requiredDepth,
        referenceDepth, alternativeDepth, evaluationRelation(mover, reference.evaluation, alternative.evaluation),
        referencePath.ref.id, alternativePath.ref.id,
        ObservedLineProtocol.evaluationJson(referenceSource.evaluation, referenceSource.source),
        ObservedLineProtocol.evaluationJson(alternativeSource.evaluation, alternativeSource.source),
        referenceOccurrence, alternativeOccurrence)

  private def sourceForWork(source: EvaluationSource, work: IssuedEngineWork): Boolean =
    ObservedLineProtocol.evaluationSourceId(source) == work.purpose.wireId

  /** A transport projection of an explicit lifecycle, never a scheduler or a new proof owner. */
  def snapshot(question: Question, state: String = "deferred", answer: Option[Answer] = None,
      stopReason: Option[String] = None): JsObject =
    require(Set("deferred", "awaiting_engine", "answered", "stopped")(state), "unknown inquiry lifecycle state")
    require((state == "answered") == answer.nonEmpty, "only an answered inquiry carries its answer")
    def piece(value: RelationColoredPieceWitness): JsObject = Json.obj("position_id" -> question.positionId,
      "side" -> (if value.side.white then "white" else "black"), "role" -> value.role.name, "square" -> value.square.key)
    Json.obj("id" -> question.id, "scope" -> "root_choice", "source" -> "automatic_observation",
      "root_occurrence_id" -> question.rootOccurrenceId, "position_id" -> question.positionId,
      "reference_move" -> question.referenceMove, "alternative_move" -> question.alternativeMove,
      "anchor_occurrence_ids" -> question.anchorOccurrenceIds,
      "ordering_source_ids" -> question.orderingSourceIds,
      "ordering_candidate_ids" -> question.orderingCandidateIds,
      "reasons" -> question.reasons.map { reason => Json.obj("kind" -> reason.kind.wireId,
        "evidence_id" -> reason.evidence.id, "source_id" -> reason.sourceId,
        "source_line_id" -> reason.sourceLineId, "anchor_move" -> reason.anchorMove,
        "actor" -> piece(reason.actor), "capture" -> reason.capture.map(piece),
        "promotion_role" -> reason.promotionRole, "responds_to_check" -> reason.respondsToCheck,
        "source_name" -> reason.sourceName, "target" -> reason.target.map(piece),
        "pressure_source_ids" -> reason.pressureSourceIds) },
      "state" -> state, "stop_reason" -> stopReason,
      "answer" -> answer.map(answerJson))

  def answerJson(value: Answer): JsObject = Json.obj("work_id" -> value.workId,
        "execution_key_sha256" -> value.executionKeySha256, "engine_profile" -> value.engineProfile,
        "required_depth" -> value.requiredDepth, "reference_depth" -> value.referenceDepth,
        "alternative_depth" -> value.alternativeDepth, "evaluation_relation" -> value.evaluationRelation,
        "reference_line_id" -> value.referenceLineId, "alternative_line_id" -> value.alternativeLineId,
        "reference_occurrence_id" -> value.referenceOccurrenceId, "alternative_occurrence_id" -> value.alternativeOccurrenceId,
        "reference_evaluation" -> value.referenceEvaluation, "alternative_evaluation" -> value.alternativeEvaluation)
