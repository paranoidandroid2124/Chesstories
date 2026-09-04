package lila.chessjudgment.model.judgment

import chess.Move
import chess.format.Fen

import lila.chessjudgment.analysis.position.{ PositionAnalysis, PositionAnalyzer }
import lila.chessjudgment.analysis.structure.{
  StructuralDeltaAnalyzer,
  StructuralDeltaContracts,
  TransitionStructuralDelta
}
import lila.chessjudgment.model.line.{ CanonicalPositionHistoryStep, LegalReplayStep, PrincipalVariationEvidence }
import lila.chessjudgment.model.position.BoardMoveEffect

private final class CanonicalReplayTransitionCalculation(
    beforeAnalysis: PositionAnalysis,
    afterAnalysis: PositionAnalysis,
    legalStep: LegalReplayStep
):
  lazy val relationTransition: RelationInventoryTransition =
    afterAnalysis.relationTransition.getOrElse(
      throw IllegalArgumentException("a replay transition destination must own its canonical relation transition")
    )

  lazy val boardFootprint =
    afterAnalysis.transitionFootprint.getOrElse(
      throw IllegalArgumentException("a replay transition destination must own its admitted board footprint")
    )

  lazy val relationDelta: RelationSemanticDelta =
    CanonicalRelationDelta.semanticFrom(legalStep, boardFootprint, relationTransition)

  lazy val controlSetDeltas: ControlSetDeltaResults =
    ControlSetDeltaResults.derive(relationDelta)

  lazy val certifiedControlSetDeltas: ControlSetDeltaResults =
    RelationFactEvidence
      .certifiedControlSetDeltas(
        controlSetDeltas,
        relationDelta,
        legalStep,
        boardFootprint
      )
      .getOrElse(
        throw IllegalArgumentException("control-set deltas failed legal-replay certification")
      )

  lazy val certifiedVertical: ClosedVerticalRelationResults =
    val unverified = ClosedVerticalRelationResults.derive(certifiedControlSetDeltas, relationDelta)
    RelationFactEvidence
      .certifiedVerticalBatch(
        unverified,
        certifiedControlSetDeltas,
        relationDelta,
        legalStep,
        boardFootprint
      )
      .getOrElse(
        throw IllegalArgumentException("a closed vertical relation batch failed exact derivation certification")
      )

  lazy val structuralDelta: TransitionStructuralDelta =
    StructuralDeltaAnalyzer.delta(
      beforeAnalysis,
      afterAnalysis,
      legalStep,
      relationDelta,
      certifiedVertical.resultsFor(RelationFactKind.PawnTopologyTransition)
    )

  lazy val structuralConsequences: List[TransitionConsequence] =
    StructuralDeltaContracts.consequences(structuralDelta)

  def structuralOccurrence(
      step: LineReplayStep,
      resultPremiseOccurrences: List[ReplayVerticalRelationOccurrence]
  ): ReplayStructuralOccurrence =
    ReplayStructuralOccurrence.certified(
      step,
      relationDelta.rootMove,
      structuralConsequences,
      resultPremiseOccurrences
    )

/** Semantic time-axis address of one certified derived relation. The replay step
  * identifies the exact transition occurrence; a graph record later adds the
  * concrete line occurrence without changing this relation meaning.
  */
private[chessjudgment] final case class ReplayVerticalRelationOccurrence private[judgment] (
    step: LineReplayStep,
    relation: RelationFactEvidence
):
  require(
    VerticalRelationContracts.proofOf(relation.detail).nonEmpty,
    "a replay vertical occurrence must retain its vertical derivation"
  )
  require(
    relation.mentionsLineMove(step.moveUci) && (relation.origin match
      case RelationEvidenceOrigin.LegalReplay(beforeFen, afterFen, List(moveUci)) =>
        EvidenceRef.sameMove(moveUci, step.moveUci) &&
          PrincipalVariationEvidence.sameBoardState(beforeFen, step.fenBefore) &&
          PrincipalVariationEvidence.sameBoardState(afterFen, step.fenAfter)
      case _ => false),
    "a replay vertical occurrence must retain its exact certified transition"
  )

  private[chessjudgment] lazy val resultKey: DerivedRelationResultKey =
    DerivedRelationResultKey.from(relation)

  /** Exact occurrence identity issued by the canonical replay inventory. A
    * graph evidence id later binds this occurrence to one admitted line.
    */
  private[chessjudgment] lazy val occurrenceId: String =
    EvidenceIdentity.digest(
      List(
        "replay-vertical-relation-occurrence:v1",
        EvidenceIdentity.stepKey(step),
        RelationFactKind.id(relation.kind),
        resultKey.stableKey,
        certifiedSourcePremiseIds.mkString("[", ",", "]")
      )
    )

  /** Lower-premise identity stays bound to this replay occurrence. */
  private[chessjudgment] lazy val certifiedSourcePremiseIds: List[String] =
    ReplayVerticalRelationOccurrence.certifiedSourcePremiseIds(relation)

  private[chessjudgment] lazy val stableKey: String =
    EvidenceIdentity.digest(List(
      "replay-vertical-relation-occurrence-binding:v1",
      EvidenceIdentity.stepKey(step),
      RelationFactKind.id(resultKey.kind),
      resultKey.stableKey,
      occurrenceId,
      certifiedSourcePremiseIds.mkString("[", ",", "]")
    ))

private[chessjudgment] object ReplayVerticalRelationOccurrence:
  private[chessjudgment] def certifiedSourcePremiseIds(
      relation: RelationFactEvidence
  ): List[String] =
    val proof = VerticalRelationContracts.proofOf(relation.detail).getOrElse(
      throw IllegalStateException("a replay vertical occurrence lost its vertical derivation proof")
    )
    val premiseIds = proof.sourcePremises.map(_.stableKey).sorted
    require(
      premiseIds.nonEmpty && premiseIds.distinct.size == premiseIds.size,
      "a replay vertical occurrence needs exact role-distinct lower premises"
    )
    premiseIds

/** Replay-owned exact consequence inventory for one admitted transition occurrence.
  * Graph records project this calculation without invoking the structural
  * contracts again.
  */
private[chessjudgment] final case class ReplayStructuralOccurrence private[judgment] (
    step: LineReplayStep,
    occurrenceId: String,
    consequences: List[TransitionConsequence],
    resultPremiseOccurrences: List[ReplayVerticalRelationOccurrence],
    sourcePremiseKeys: List[String]
):
  require(occurrenceId.matches("[0-9a-f]{64}"), "a replay structural occurrence needs a canonical id")
  require(
    sourcePremiseKeys.nonEmpty && sourcePremiseKeys == sourcePremiseKeys.distinct.sorted,
    "a replay structural occurrence needs exact canonical lower premise keys"
  )

private[chessjudgment] object ReplayStructuralOccurrence:
  private[judgment] def certified(
      step: LineReplayStep,
      movement: CanonicalRootLegalMove,
      consequences: List[TransitionConsequence],
      resultPremiseOccurrences: List[ReplayVerticalRelationOccurrence]
  ): ReplayStructuralOccurrence =
    val expectedResultKeys = consequences.flatMap(_.resultPremiseKeys).distinct.sortBy(_.stableKey)
    require(
      resultPremiseOccurrences.map(_.resultKey) == expectedResultKeys &&
        resultPremiseOccurrences.map(_.occurrenceId).distinct.size == resultPremiseOccurrences.size &&
        resultPremiseOccurrences.forall(_.step == step),
      "a structural occurrence must retain every consumed derived relation at this exact replay occurrence"
    )
    val relationPremises = consequences.flatMap(consequence =>
      (consequence.subjectBindings ++ consequence.targetBindings).flatMap(binding =>
        binding.relationKeys.map(key => s"relation:${key.stableKey}")
      )
    )
    val resultPremises = resultPremiseOccurrences.flatMap(occurrence =>
      List(
        s"result:${occurrence.resultKey.stableKey}",
        s"result-occurrence:${occurrence.occurrenceId}"
      ) ++ occurrence.certifiedSourcePremiseIds
    )
    val premiseKeys = (
      s"legal-move:${movement.fact.semanticId}" :: (resultPremises ++ relationPremises)
    ).distinct.sorted
    val semanticParts = List(
      "replay-structural-occurrence:v1",
      step.ply.toString,
      EvidenceRef.normalizeMove(step.moveUci),
      PrincipalVariationEvidence.semanticBoardStateFen(step.fenBefore).getOrElse(
        PrincipalVariationEvidence.normalizeFen(step.fenBefore)
      ),
      PrincipalVariationEvidence.semanticBoardStateFen(step.fenAfter).getOrElse(
        PrincipalVariationEvidence.normalizeFen(step.fenAfter)
      ),
      movement.fact.semanticId,
      consequences.map(_.stableKey).sorted.mkString("[", ",", "]"),
      resultPremiseOccurrences.map(_.stableKey).mkString("[", ",", "]"),
      premiseKeys.mkString("[", ",", "]")
    )
    val digest = EvidenceIdentity.digest(semanticParts)
    ReplayStructuralOccurrence(step, digest, consequences, resultPremiseOccurrences, premiseKeys)

private[chessjudgment] final class CanonicalReplayTransition private[judgment] (
    val declared: LineReplayStep,
    val legal: LegalReplayStep,
    val beforeAnalysis: PositionAnalysis,
    val afterAnalysis: PositionAnalysis,
    private val calculation: CanonicalReplayTransitionCalculation
):
  def relationTransition: RelationInventoryTransition = calculation.relationTransition
  def relationDelta: RelationSemanticDelta = calculation.relationDelta
  def boardFootprint: lila.chessjudgment.model.position.BoardTransitionFootprint = calculation.boardFootprint

  private lazy val closedRelationOutput: ClosedRelationTransitionInventory =
    ClosedRelationTransitionInventory.close(
      declared,
      relationDelta,
      calculation.certifiedControlSetDeltas,
      calculation.certifiedVertical
    )

  private[chessjudgment] def verticalRelationsFor(
      kind: RelationFactKind
  ): List[RelationFactEvidence] =
    calculation.certifiedVertical.resultsFor(kind)

  private[chessjudgment] def closedRelationInventory: ClosedRelationTransitionInventory =
    closedRelationOutput

  def structuralDelta: TransitionStructuralDelta = calculation.structuralDelta

  private[chessjudgment] def structuralConsequences: List[TransitionConsequence] =
    calculation.structuralConsequences

  private[chessjudgment] def ownsStructuralDelta(delta: TransitionStructuralDelta): Boolean =
    calculation.structuralDelta.asInstanceOf[AnyRef] eq delta.asInstanceOf[AnyRef]

  private[chessjudgment] lazy val structuralOccurrence: ReplayStructuralOccurrence =
    val expectedKeys = calculation.structuralConsequences.flatMap(_.resultPremiseKeys).toSet
    val expectedKinds = expectedKeys.iterator.map(_.kind).toSet.toList.sortBy(RelationFactKind.id)
    val occurrences = expectedKinds.flatMap { kind =>
      verticalRelationsFor(kind).flatMap { relation =>
        val occurrence = ReplayVerticalRelationOccurrence(declared, relation)
        Option.when(expectedKeys(occurrence.resultKey))(occurrence)
      }
    }.sortBy(_.resultKey.stableKey)
    require(
      occurrences.map(_.resultKey).distinct.size == occurrences.size &&
        occurrences.map(_.resultKey).toSet == expectedKeys,
      "a structural occurrence must resolve every consumed derived relation exactly once"
    )
    calculation.structuralOccurrence(declared, occurrences)

  private[judgment] def certifiesBinding(
      transition: StructuralTransitionBinding
  ): Boolean =
    declared.ply == transition.to.ply &&
      transition.to.ply == transition.from.ply + 1 &&
      legal.ply == declared.ply &&
      EvidenceRef.sameMove(declared.moveUci, transition.moveUci) &&
      EvidenceRef.sameMove(legal.uci, transition.moveUci) &&
      PrincipalVariationEvidence.normalizeFen(declared.fenBefore) ==
        PrincipalVariationEvidence.normalizeFen(transition.from.fen) &&
      PrincipalVariationEvidence.normalizeFen(declared.fenAfter) ==
        PrincipalVariationEvidence.normalizeFen(transition.to.fen) &&
      legal.move.piece.color == transition.perspective &&
      transition.from.sideToMove.forall(_ == transition.perspective)

private object CanonicalReplayTransition:
  def fresh(
      declared: LineReplayStep,
      legal: LegalReplayStep,
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis
  ): CanonicalReplayTransition =
    new CanonicalReplayTransition(
      declared,
      legal,
      beforeAnalysis,
      afterAnalysis,
      new CanonicalReplayTransitionCalculation(beforeAnalysis, afterAnalysis, legal)
    )

/** The admitted board state for one engine line.
  *
  * Public evidence keeps FENs and UCI moves. Board-aware consumers use this
  * value so the legal replay that admitted the line is not performed again.
  */
private[chessjudgment] final class CanonicalLineReplay private (
    val replaySteps: List[LineReplayStep],
    val legalSteps: List[LegalReplayStep],
    suppliedOccurrenceAnalyses: Option[LazyList[PositionAnalysis]] = None,
    suppliedTransitions: Option[LazyList[CanonicalReplayTransition]] = None
):
  require(replaySteps.nonEmpty, "a canonical replay needs at least one transition occurrence")
  require(replaySteps.size == legalSteps.size, "a canonical replay requires one legal step per declared step")

  private val indexByReplayStep: Map[LineReplayStep, Int] =
    replaySteps.zipWithIndex.toMap
  private val indexByTransitionKey: Map[(Int, String, String, String), Int] =
    replaySteps.zipWithIndex.map { case (step, index) =>
      CanonicalLineReplay.transitionKey(
        step.ply,
        step.moveUci,
        step.fenBefore,
        step.fenAfter
      ) -> index
    }.toMap
  require(
    indexByReplayStep.size == replaySteps.size,
    "a canonical replay cannot contain the same time-axis occurrence twice"
  )
  require(
    indexByTransitionKey.size == replaySteps.size,
    "a canonical replay cannot contain the same normalized transition occurrence twice"
  )

  private lazy val occurrenceAnalyses: LazyList[PositionAnalysis] =
    suppliedOccurrenceAnalyses.getOrElse {
      (replaySteps.headOption, legalSteps.headOption) match
        case (Some(firstReplay), Some(firstLegal)) =>
          val initial = PositionAnalyzer.analyze(
            firstLegal.before,
            firstReplay.fenBefore,
            firstLegal.ply - 1
          )
          replaySteps.zip(legalSteps).to(LazyList).scanLeft(initial) {
            case (previous, (declared, legal)) =>
              PositionAnalyzer.analyzeAfter(previous, legal, declared.fenAfter)
          }
        case _ =>
          LazyList.empty
    }

  private lazy val transitionOccurrences: LazyList[CanonicalReplayTransition] =
    suppliedTransitions.getOrElse(
      CanonicalLineReplay.freshTransitions(replaySteps, legalSteps, occurrenceAnalyses)
    )

  def analysisBefore(step: LineReplayStep): Option[PositionAnalysis] =
    indexByReplayStep.get(step).flatMap(occurrenceAnalyses.lift)

  def analysisAfter(step: LineReplayStep): Option[PositionAnalysis] =
    indexByReplayStep.get(step).flatMap(index => occurrenceAnalyses.lift(index + 1))

  def transition(step: LineReplayStep): Option[CanonicalReplayTransition] =
    indexByReplayStep.get(step).flatMap(transitionOccurrences.lift)

  private[chessjudgment] def transitionIndex(
      moveUci: String,
      from: PositionNodeRef,
      to: PositionNodeRef
  ): Option[Int] =
    Option
      .when(to.ply == from.ply + 1)(
        CanonicalLineReplay.transitionKey(to.ply, moveUci, from.fen, to.fen)
      )
      .flatMap(indexByTransitionKey.get)

  /** Reuses every admitted transition calculation while projecting the
    * one-step replays owned by transition edges. The lazy transition stream is
    * traversed once, regardless of line length.
    */
  private[chessjudgment] lazy val singleStepReplays
      : List[(CanonicalReplayTransition, CanonicalLineReplay)] =
    val projected = replaySteps
      .zip(legalSteps)
      .to(LazyList)
      .zip(transitionOccurrences)
      .map { case ((step, legal), transition) =>
        transition -> new CanonicalLineReplay(
          replaySteps = List(step),
          legalSteps = List(legal),
          suppliedOccurrenceAnalyses = Some(LazyList(transition.beforeAnalysis, transition.afterAnalysis)),
          suppliedTransitions = Some(LazyList(transition))
        )
      }
      .toList
    require(
      projected.size == replaySteps.size && projected.map(_._1.declared) == replaySteps,
      "a canonical replay must project every transition occurrence exactly once"
    )
    projected

  private[chessjudgment] def verticalRelationOccurrences(
      step: LineReplayStep,
      kinds: List[RelationFactKind]
  ): List[ReplayVerticalRelationOccurrence] =
    require(
      kinds.distinct.size == kinds.size,
      "one time-axis query cannot repeat a relation kind"
    )
    transition(step).toList.flatMap { exactTransition =>
      kinds.flatMap(kind =>
        exactTransition.verticalRelationsFor(kind).map(relation =>
          ReplayVerticalRelationOccurrence(step, relation)
        )
      )
    }

  /** Closed recapture classification for consumers that must distinguish a
    * certified absence from an unresolved inventory. `None` never means
    * `Excluded`; it means that the canonical occurrence could not certify
    * either result.
    */
  private[chessjudgment] def exactRecaptureStatus(
      captureStep: LineReplayStep,
      recaptureReplay: CanonicalLineReplay,
      recaptureStep: LineReplayStep
  ): Option[LineMaterialRecaptureStatus] =
    exactRecaptureResolution(captureStep, recaptureReplay, recaptureStep).map { resolution =>
      resolution.resource match
        case Some(_) => LineMaterialRecaptureStatus.Proven(
            resolution.occurrence.resultKey
          )
        case None    => LineMaterialRecaptureStatus.Excluded
    }

  private final case class ExactRecaptureResolution(
      occurrence: ReplayVerticalRelationOccurrence,
      resource: Option[RelationLegalMoveResourceWitness]
  )

  private def exactRecaptureResolution(
      captureStep: LineReplayStep,
      recaptureReplay: CanonicalLineReplay,
      recaptureStep: LineReplayStep
  ): Option[ExactRecaptureResolution] =
    for
      _ <- indexByReplayStep.get(captureStep)
      _ <- recaptureReplay.indexByReplayStep.get(recaptureStep)
      captureTransition <- transition(captureStep)
      recaptureTransition <- recaptureReplay.transition(recaptureStep)
      resolution <- exactRecaptureResolution(captureTransition, recaptureTransition)
    yield resolution

  private def exactRecaptureResolution(
      captureTransition: CanonicalReplayTransition,
      recaptureTransition: CanonicalReplayTransition
  ): Option[ExactRecaptureResolution] =
    val captureStep = captureTransition.declared
    val recaptureStep = recaptureTransition.declared
    val captureLegal = captureTransition.legal
    val recaptureLegal = recaptureTransition.legal
    for
      _ <- Option.when(recaptureStep.ply == captureStep.ply + 1)(())
      _ <- Option.when(PrincipalVariationEvidence.sameBoardState(captureStep.fenAfter, recaptureStep.fenBefore))(
        ()
      )
      captureRoot = captureTransition.relationDelta.rootMove
      recaptureRoot = recaptureTransition.relationDelta.rootMove
      if captureRoot.capture.nonEmpty
      inventory <- only(
        captureTransition
          .verticalRelationsFor(RelationFactKind.CaptureRecaptureInventory)
          .flatMap(relation =>
            relation.detail match
              case detail: RelationWitnessDetail.CaptureRecaptureInventory =>
                List(ReplayVerticalRelationOccurrence(
                  captureStep,
                  relation
                ) -> detail)
              case _ => Nil
          )
      )
      (occurrence, detail) = inventory
      if detail.mover == captureRoot.witness && legalMovementMatches(detail.mover, captureLegal)
      if captureRoot.capture.exists(capture =>
        detail.captured.side == capture.capturedSide &&
          detail.captured.role == capture.capturedRole &&
          detail.captured.square == capture.capturedSquare
      )
      resource <- detail.legalRecaptures.filter(resource =>
        EvidenceRef.sameMove(resource.moveUci, recaptureStep.moveUci) &&
          resource.movement == recaptureRoot.witness &&
          resource.capture == recaptureRoot.capture &&
          legalMovementMatches(resource.movement, recaptureLegal)
      ) match
        case exact :: Nil => Some(Some(exact))
        case Nil          => Some(None)
        case _            => None
    yield ExactRecaptureResolution(occurrence, resource)

  private def only[A](values: List[A]): Option[A] =
    values match
      case exact :: Nil => Some(exact)
      case _            => None

  private def legalMovementMatches(
      movement: RelationMoveTransitionWitness,
      legal: LegalReplayStep
  ): Boolean =
    movement.side == legal.move.piece.color &&
      movement.from.key.equalsIgnoreCase(legal.move.orig.key) &&
      movement.to.key.equalsIgnoreCase(legal.move.dest.key) &&
      movement.beforeRole.name.equalsIgnoreCase(legal.move.piece.role.name)

  def onlyTransition: Option[CanonicalReplayTransition] =
    Option.when(replaySteps.size == 1)(transitionOccurrences.head)

  def matches(steps: List[LineReplayStep]): Boolean =
    replaySteps == steps

  def subset(steps: List[LineReplayStep]): Option[CanonicalLineReplay] =
    val indices = steps.flatMap(indexByReplayStep.get)
    Option.when(
      steps.nonEmpty &&
        indices.size == steps.size &&
        indices.sliding(2).forall {
          case List(left, right) => right == left + 1
          case _                 => true
        }
    ) {
      val firstIndex = indices.head
      new CanonicalLineReplay(
        steps,
        indices.map(legalSteps),
        Some(occurrenceAnalyses.slice(firstIndex, firstIndex + steps.size + 1)),
        Some(transitionOccurrences.slice(firstIndex, firstIndex + steps.size))
      )
    }

  /** Attach a previously admitted line to the one registered analysis of its
    * root occurrence. If the line already owns that inventory, retain it. If
    * it was reconstructed independently from FEN, rebuild each legal step from
    * the registered occurrence's actual legal moves and carry that analysis
    * forward. This keeps legal-response and closed-inventory ownership on one
    * path instead of accepting merely board-equivalent analyses.
    */
  private[chessjudgment] def rootedAt(
      rootAnalysis: PositionAnalysis
  ): CanonicalLineReplay =
    val first = replaySteps.head
    val alreadyOwned = analysisBefore(first).exists(existing =>
      existing.occurrence.plyCount == rootAnalysis.occurrence.plyCount &&
        existing.relationInventory.sameOwner(rootAnalysis.relationInventory)
    )
    if alreadyOwned then this
    else
      replaySteps
        .foldLeft(Option((rootAnalysis, List.empty[LegalReplayStep], List(rootAnalysis)))) {
          case (state, declared) =>
            state.flatMap { case (beforeAnalysis, admitted, analyses) =>
              val matchingMoves = beforeAnalysis.actualLegalMoves.filter(move =>
                EvidenceRef.sameMove(move.toUci.uci, declared.moveUci) &&
                  PrincipalVariationEvidence.sameBoardState(Fen.write(move.after).value, declared.fenAfter)
              )
              matchingMoves match
                case move :: Nil
                    if beforeAnalysis.occurrence.plyCount == declared.ply - 1 &&
                      PrincipalVariationEvidence.sameBoardState(
                        beforeAnalysis.occurrence.fen,
                        declared.fenBefore
                      ) =>
                  val legal = LegalReplayStep.fromMove(declared.ply, beforeAnalysis.position, move)
                  val afterAnalysis = PositionAnalyzer.analyzeAfter(beforeAnalysis, legal, declared.fenAfter)
                  Some((afterAnalysis, legal :: admitted, afterAnalysis :: analyses))
                case _ => None
            }
        }
        .map { case (_, admitted, analyses) =>
          new CanonicalLineReplay(
            replaySteps = replaySteps,
            legalSteps = admitted.reverse,
            suppliedOccurrenceAnalyses = Some(analyses.reverse.to(LazyList))
          )
        }
        .getOrElse(
          throw IllegalArgumentException("an admitted line cannot be rooted at another occurrence")
        )

private[chessjudgment] object CanonicalLineReplay:

  private def transitionKey(
      ply: Int,
      moveUci: String,
      fenBefore: String,
      fenAfter: String
  ): (Int, String, String, String) =
    (
      ply,
      EvidenceRef.normalizeMove(moveUci),
      PrincipalVariationEvidence.normalizeFen(fenBefore),
      PrincipalVariationEvidence.normalizeFen(fenAfter)
    )

  private def freshTransitions(
      replaySteps: List[LineReplayStep],
      legalSteps: List[LegalReplayStep],
      analyses: LazyList[PositionAnalysis]
  ): LazyList[CanonicalReplayTransition] =
    replaySteps
      .zip(legalSteps)
      .to(LazyList)
      .zip(analyses.zip(analyses.drop(1)))
      .map { case ((declared, legal), (beforeAnalysis, afterAnalysis)) =>
        CanonicalReplayTransition.fresh(declared, legal, beforeAnalysis, afterAnalysis)
      }

  def fromAnalyzedLegalMove(
      step: LineReplayStep,
      beforeAnalysis: PositionAnalysis,
      move: Move
  ): Option[CanonicalLineReplay] =
    val before = beforeAnalysis.position
    val uci = PrincipalVariationEvidence.normalizeUci(move.toUci.uci)
    val ownedMove = beforeAnalysis.actualLegalMoves.exists(candidate =>
      candidate.asInstanceOf[AnyRef] eq move.asInstanceOf[AnyRef]
    )
    Option.when(
      step.ply > 0 &&
        beforeAnalysis.occurrence.plyCount == step.ply - 1 &&
        PrincipalVariationEvidence.sameBoardState(beforeAnalysis.occurrence.fen, step.fenBefore) &&
        EvidenceRef.sameMove(step.moveUci, uci) &&
        ownedMove && move.before == before &&
        PrincipalVariationEvidence.sameBoardState(step.fenAfter, Fen.write(move.after).value)
    ) {
      val legal = LegalReplayStep.fromMove(step.ply, before, move)
      val afterAnalysis = PositionAnalyzer.analyzeAfter(beforeAnalysis, legal, step.fenAfter)
      new CanonicalLineReplay(
        replaySteps = List(step.copy(moveUci = uci)),
        legalSteps = List(legal),
        suppliedOccurrenceAnalyses = Some(LazyList(beforeAnalysis, afterAnalysis))
      )
    }

  def fromLegalReplay(steps: List[LegalReplayStep]): Option[CanonicalLineReplay] =
    val admitted =
      steps.nonEmpty &&
        steps.forall(step =>
          step.ply > 0 &&
            EvidenceRef.sameMove(step.uci, step.move.toUci.uci) &&
            step.before == step.move.before &&
            step.after == step.move.after
        ) &&
        steps.sliding(2).forall {
          case List(left, right) =>
            right.ply == left.ply + 1 &&
              left.after == right.before
          case _ => true
        }
    Option.when(admitted) {
      val replaySteps = steps.map { step =>
        LineReplayStep(
          ply = step.ply,
          moveUci = PrincipalVariationEvidence.normalizeUci(step.uci),
          fenBefore = Fen.write(step.before).value,
          fenAfter = Fen.write(step.after).value
        )
      }
      new CanonicalLineReplay(replaySteps, steps)
    }

  def fromHistory(steps: List[CanonicalPositionHistoryStep]): Option[CanonicalLineReplay] =
    val admitted =
      steps.nonEmpty &&
        steps.forall(step =>
          step.ply > 0 &&
            EvidenceRef.sameMove(step.uci, step.move.toUci.uci) &&
            step.before == step.move.before &&
            step.move.after == step.after
        ) &&
        steps.sliding(2).forall {
          case List(left, right) =>
            right.ply == left.ply + 1 && left.after == right.before
          case _ => true
        }
    Option.when(admitted) {
      val replay = steps.map { step =>
        LineReplayStep(
          ply = step.ply,
          moveUci = PrincipalVariationEvidence.normalizeUci(step.uci),
          fenBefore = step.beforeFen,
          fenAfter = step.afterFen
        )
      }
      val legal = steps.map { step =>
        LegalReplayStep(
          ply = step.ply,
          uci = PrincipalVariationEvidence.normalizeUci(step.uci),
          before = step.before,
          move = step.move,
          after = step.after,
          capturedRole = step.capturedRole
        )
      }
      new CanonicalLineReplay(replay, legal)
    }

/** One admitted legal move bound to one structural transition occurrence.
  * Consumers compare this certificate with the declared transition instead
  * of replaying the same move again.
  */
private[chessjudgment] final class CanonicalTransitionProof private (
    private[judgment] val transitionBinding: StructuralTransitionBinding,
    private[chessjudgment] val relationDelta: RelationSemanticDelta
):
  def proves(transition: StructuralTransitionBinding): Boolean =
    transition == transitionBinding

  private[chessjudgment] def provesMove(
      moveUci: String,
      from: PositionNodeRef,
      to: PositionNodeRef,
      scope: EvidenceScope,
      line: Option[LineNodeRef]
  ): Boolean =
    transitionBinding.line == line && transitionBinding.role.scope == scope &&
      EvidenceRef.sameMove(transitionBinding.moveUci, moveUci) &&
      transitionBinding.from == from && transitionBinding.to == to &&
      proves(transitionBinding)

  private[chessjudgment] def rootMovement: CanonicalRootLegalMove =
    relationDelta.rootMove

  private[chessjudgment] def moveEffect: BoardMoveEffect =
    relationDelta.transitionFootprint.moveEffect

private[chessjudgment] object CanonicalTransitionProof:
  def from(
      transition: StructuralTransitionBinding,
      replay: CanonicalLineReplay
  ): Option[CanonicalTransitionProof] =
    replay.onlyTransition
      .filter(_.certifiesBinding(transition))
      .map(owner =>
        new CanonicalTransitionProof(
          transition,
          owner.relationDelta
        )
      )

/** Binds the exact structural output inventory to the admitted legal
  * transition that produced it.  A legal move certificate alone cannot
  * certify arbitrary copied or hand-written consequences.
  */
private[chessjudgment] final case class CanonicalTransitionDeltaProof private (
    transitionProof: CanonicalTransitionProof,
    canonicalRelations: CanonicalRelationDelta,
    consequences: List[TransitionConsequence],
    relationChanges: List[CanonicalRelationChange],
    resultPremiseSources: List[TransitionResultPremiseSource]
):
  def proves(delta: StructuralDeltaEvidence): Boolean =
    transitionProof.proves(delta.transition) &&
      consequences == delta.consequences &&
      relationChanges == delta.relationChanges &&
      resultPremiseSources == delta.resultPremiseSources &&
      TransitionConsequenceBindingProof.provesCanonical(
        consequences,
        relationChanges,
        delta.transition,
        canonicalRelations
      )

private[chessjudgment] object CanonicalTransitionDeltaProof:
  def from(
      transitionProof: CanonicalTransitionProof,
      canonicalRelations: CanonicalRelationDelta,
      consequences: List[TransitionConsequence],
      relationChanges: List[CanonicalRelationChange],
      resultPremiseSources: List[TransitionResultPremiseSource]
  ): CanonicalTransitionDeltaProof =
    require(
      canonicalRelations.owns(transitionProof.relationDelta),
      "canonical consequences must retain the admitted transition's exact semantic delta"
    )
    val expectedResultKeys = consequences.flatMap(_.resultPremiseKeys).toSet
    require(
      resultPremiseSources.map(_.key).distinct.size == resultPremiseSources.size &&
        resultPremiseSources.map(_.source).distinct.size == resultPremiseSources.size &&
        resultPremiseSources.map(_.key).toSet == expectedResultKeys,
      "canonical consequences need one exact occurrence source for every consumed transition result"
    )
    require(
      TransitionConsequenceBindingProof.provesCanonical(
        consequences,
        relationChanges,
        transitionProof.transitionBinding,
        canonicalRelations
      ),
      "canonical transition consequences must be bound to their exact relation changes"
    )
    CanonicalTransitionDeltaProof(
      transitionProof,
      canonicalRelations,
      consequences,
      relationChanges,
      resultPremiseSources
    )
