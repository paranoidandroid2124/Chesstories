package lila.chessjudgment.model.judgment

import chess.{ Move, Position, Role }
import chess.format.Fen

import lila.chessjudgment.analysis.position.{ PositionAnalysis, PositionAnalyzer }
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, TransitionStructuralDelta }
import lila.chessjudgment.model.line.{ CanonicalPositionHistoryStep, LegalReplayStep, PrincipalVariationEvidence }

private final class CanonicalReplayTransitionCalculation(
    beforeAnalysis: PositionAnalysis,
    afterAnalysis: PositionAnalysis,
    legalStep: LegalReplayStep
):
  def ownsOccurrence(
      before: PositionAnalysis,
      after: PositionAnalysis,
      step: LegalReplayStep
  ): Boolean =
    step.copy(ply = legalStep.ply) == legalStep &&
      before.position == beforeAnalysis.position && after.position == afterAnalysis.position &&
      before.relationInventory.sameOwner(beforeAnalysis.relationInventory) &&
      after.relationInventory.sameOwner(afterAnalysis.relationInventory) &&
      after.transitionFootprint == afterAnalysis.transitionFootprint

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

  lazy val relationContractBatch: TransitionRelationContractBatch =
    TransitionRelationContractBatch.derive(relationDelta)

  lazy val certifiedCombinations: ClosedRelationCombinationResults =
    RelationFactEvidence
      .certifiedTransitionContractBatch(
        relationContractBatch,
        relationDelta,
        legalStep,
        boardFootprint
      )
      .getOrElse(
        throw IllegalArgumentException("a closed relation batch failed legal-replay certification")
      )

  lazy val certifiedVertical: ClosedVerticalRelationResults =
    val unverified = ClosedVerticalRelationResults.derive(certifiedCombinations, relationDelta)
    RelationFactEvidence
      .certifiedVerticalBatch(
        unverified,
        certifiedCombinations,
        relationDelta,
        legalStep,
        boardFootprint
      )
      .getOrElse(
        throw IllegalArgumentException("a closed vertical relation batch failed typed contract certification")
      )

/** Semantic time-axis address of one certified L1 result. The replay step
  * identifies the exact transition occurrence; graph binding later adds the
  * concrete line occurrence without changing this relation meaning.
  */
private[chessjudgment] final case class ReplayVerticalRelationOccurrence private[judgment] (
    step: LineReplayStep,
    contract: VerticalRelationContractKind,
    relation: RelationFactEvidence
):
  require(
    VerticalRelationContractKind.forDetail(relation.detail).contains(contract),
    "a replay vertical occurrence must retain its producing contract"
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
      calculation.certifiedCombinations,
      calculation.certifiedVertical
    )

  private[chessjudgment] def combinationRelationsFor(
      contract: RelationCombinationContractKind
  ): List[RelationFactEvidence] =
    calculation.certifiedCombinations.resultsFor(contract)

  private[chessjudgment] def verticalRelationsFor(
      contract: VerticalRelationContractKind
  ): List[RelationFactEvidence] =
    calculation.certifiedVertical.resultsFor(contract)

  private[chessjudgment] def closedRelationInventory: ClosedRelationTransitionInventory =
    closedRelationOutput

  private lazy val structuralOutput: TransitionStructuralDelta =
    StructuralDeltaAnalyzer.delta(
      beforeAnalysis,
      afterAnalysis,
      legal,
      relationDelta,
      combinationRelationsFor(RelationCombinationContractKind.GeometricControlSetDelta),
      combinationRelationsFor(RelationCombinationContractKind.NamedRayTransition),
      verticalRelationsFor(VerticalRelationContractKind.PawnTopologyTransition),
      verticalRelationsFor(VerticalRelationContractKind.SliderReachDelta)
    )

  def structuralDelta: TransitionStructuralDelta = structuralOutput

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

  private[judgment] def atOccurrence(
      nextDeclared: LineReplayStep,
      nextLegal: LegalReplayStep,
      nextBeforeAnalysis: PositionAnalysis,
      nextAfterAnalysis: PositionAnalysis
  ): CanonicalReplayTransition =
    require(
      calculation.ownsOccurrence(nextBeforeAnalysis, nextAfterAnalysis, nextLegal) &&
        nextDeclared.ply == nextLegal.ply &&
        EvidenceRef.sameMove(nextDeclared.moveUci, nextLegal.uci) &&
        PrincipalVariationEvidence.sameBoardState(nextDeclared.fenBefore, Fen.write(nextLegal.before).value) &&
        PrincipalVariationEvidence.sameBoardState(nextDeclared.fenAfter, Fen.write(nextLegal.after).value),
      "a canonical transition calculation may only be rebound to the same admitted occurrence"
    )
    if declared == nextDeclared && legal == nextLegal &&
        (beforeAnalysis.asInstanceOf[AnyRef] eq nextBeforeAnalysis.asInstanceOf[AnyRef]) &&
        (afterAnalysis.asInstanceOf[AnyRef] eq nextAfterAnalysis.asInstanceOf[AnyRef])
    then this
    else
      new CanonicalReplayTransition(
        nextDeclared,
        nextLegal,
        nextBeforeAnalysis,
        nextAfterAnalysis,
        calculation
      )

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
  require(replaySteps.size == legalSteps.size, "a canonical replay requires one legal step per declared step")

  private val byReplayStep: Map[LineReplayStep, LegalReplayStep] =
    replaySteps.zip(legalSteps).toMap
  private val indexByReplayStep: Map[LineReplayStep, Int] =
    replaySteps.zipWithIndex.toMap

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

  def legalStep(step: LineReplayStep): Option[LegalReplayStep] =
    byReplayStep.get(step)

  def before(step: LineReplayStep): Option[Position] =
    legalStep(step).map(_.before)

  def after(step: LineReplayStep): Option[Position] =
    legalStep(step).map(_.after)

  def analysisBefore(step: LineReplayStep): Option[PositionAnalysis] =
    indexByReplayStep.get(step).flatMap(occurrenceAnalyses.lift)

  def analysisAfter(step: LineReplayStep): Option[PositionAnalysis] =
    indexByReplayStep.get(step).flatMap(index => occurrenceAnalyses.lift(index + 1))

  def transition(step: LineReplayStep): Option[CanonicalReplayTransition] =
    indexByReplayStep.get(step).flatMap(transitionOccurrences.lift)

  private[chessjudgment] def verticalRelationOccurrences(
      step: LineReplayStep,
      contracts: List[VerticalRelationContractKind]
  ): List[ReplayVerticalRelationOccurrence] =
    require(
      contracts.distinct.size == contracts.size,
      "one time-axis query cannot repeat an L1 contract"
    )
    transition(step).toList.flatMap { exactTransition =>
      contracts.flatMap(contract =>
        exactTransition.verticalRelationsFor(contract).map(relation =>
          ReplayVerticalRelationOccurrence(step, contract, relation)
        )
      )
    }

  def onlyTransition: Option[CanonicalReplayTransition] =
    Option.when(replaySteps.size == 1)(transitionOccurrences.head)

  def capturedRole(step: LineReplayStep): Option[Role] =
    legalStep(step).flatMap(_.capturedRole)

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

  def append(
      step: LineReplayStep,
      before: Position,
      move: Move
  ): Option[CanonicalLineReplay] =
    CanonicalLineReplay.append(this, step, LegalReplayStep.fromMove(step.ply, before, move))

  def rebased(firstPly: Int): Option[CanonicalLineReplay] =
    Option.when(firstPly > 0)(()).flatMap(_ => replaySteps.headOption).map { first =>
      val shift = firstPly - first.ply
      val rebasedReplaySteps = replaySteps.map(step => step.copy(ply = step.ply + shift))
      val rebasedLegalSteps = legalSteps.map(step => step.copy(ply = step.ply + shift))
      val rebasedAnalyses = occurrenceAnalyses.zipWithIndex.map { case (analysis, index) =>
        analysis.atPly(firstPly - 1 + index)
      }
      new CanonicalLineReplay(
        rebasedReplaySteps,
        rebasedLegalSteps,
        Some(rebasedAnalyses),
        Some(
          CanonicalLineReplay.remapTransitions(
            transitionOccurrences,
            rebasedReplaySteps,
            rebasedLegalSteps,
            rebasedAnalyses
          )
        )
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
  ): Option[CanonicalLineReplay] =
    replaySteps.headOption.flatMap { first =>
      val alreadyOwned = analysisBefore(first).exists(existing =>
        existing.features.plyCount == rootAnalysis.features.plyCount &&
          existing.relationInventory.sameOwner(rootAnalysis.relationInventory)
      )
      if alreadyOwned then Some(this)
      else
        val rebuilt = replaySteps.foldLeft(
          Option((rootAnalysis, List.empty[LegalReplayStep], List(rootAnalysis)))
        ) { case (state, declared) =>
          state.flatMap { case (beforeAnalysis, admitted, analyses) =>
            val matchingMoves = beforeAnalysis.actualLegalMoves.filter(move =>
              EvidenceRef.sameMove(move.toUci.uci, declared.moveUci) &&
                PrincipalVariationEvidence.sameBoardState(Fen.write(move.after).value, declared.fenAfter)
            )
            matchingMoves match
              case move :: Nil
                  if beforeAnalysis.features.plyCount == declared.ply - 1 &&
                    PrincipalVariationEvidence.sameBoardState(
                      beforeAnalysis.features.fen,
                      declared.fenBefore
                    ) =>
                val legal = LegalReplayStep.fromMove(declared.ply, beforeAnalysis.position, move)
                val afterAnalysis = PositionAnalyzer.analyzeAfter(beforeAnalysis, legal, declared.fenAfter)
                Some((afterAnalysis, legal :: admitted, afterAnalysis :: analyses))
              case _ => None
          }
        }
        rebuilt.map { case (_, admitted, analyses) =>
          new CanonicalLineReplay(
            replaySteps = replaySteps,
            legalSteps = admitted.reverse,
            suppliedOccurrenceAnalyses = Some(analyses.reverse.to(LazyList))
          )
        }
    }

private[chessjudgment] object CanonicalLineReplay:

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

  private def remapTransitions(
      transitions: LazyList[CanonicalReplayTransition],
      replaySteps: List[LineReplayStep],
      legalSteps: List[LegalReplayStep],
      analyses: LazyList[PositionAnalysis]
  ): LazyList[CanonicalReplayTransition] =
    transitions
      .zip(replaySteps.zip(legalSteps).to(LazyList))
      .zip(analyses.zip(analyses.drop(1)))
      .map { case ((transition, (declared, legal)), (beforeAnalysis, afterAnalysis)) =>
        transition.atOccurrence(declared, legal, beforeAnalysis, afterAnalysis)
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
        beforeAnalysis.features.plyCount == step.ply - 1 &&
        PrincipalVariationEvidence.sameBoardState(beforeAnalysis.features.fen, step.fenBefore) &&
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

  def concatenate(
      prefix: CanonicalLineReplay,
      continuation: CanonicalLineReplay
  ): Option[CanonicalLineReplay] =
    for
      previousDeclared <- prefix.replaySteps.lastOption
      previousLegal <- prefix.legalSteps.lastOption
      previousAnalysis <- prefix.analysisAfter(previousDeclared)
      ownedContinuation <- continuation.rootedAt(previousAnalysis)
      firstDeclared <- ownedContinuation.replaySteps.headOption
      firstLegal <- ownedContinuation.legalSteps.headOption
      continuationRoot <- ownedContinuation.analysisBefore(firstDeclared)
      if firstDeclared.ply == previousDeclared.ply + 1
      if firstLegal.ply == firstDeclared.ply
      if EvidenceRef.sameMove(firstDeclared.moveUci, firstLegal.uci)
      if previousLegal.after == firstLegal.before
      if PrincipalVariationEvidence.sameBoardState(firstDeclared.fenBefore, previousDeclared.fenAfter)
      if PrincipalVariationEvidence.sameBoardState(firstDeclared.fenBefore, Fen.write(firstLegal.before).value)
      if previousAnalysis.position == continuationRoot.position
      if previousAnalysis.features.plyCount == continuationRoot.features.plyCount
      if PrincipalVariationEvidence.sameBoardState(
        previousAnalysis.features.fen,
        continuationRoot.features.fen
      )
    yield
      val continuationAnalyses =
        LazyList(previousAnalysis) ++ ownedContinuation.occurrenceAnalyses.drop(1)
      new CanonicalLineReplay(
        replaySteps = prefix.replaySteps ++ ownedContinuation.replaySteps,
        legalSteps = prefix.legalSteps ++ ownedContinuation.legalSteps,
        suppliedOccurrenceAnalyses = Some(
          prefix.occurrenceAnalyses ++ continuationAnalyses.drop(1)
        ),
        suppliedTransitions = Some(
          prefix.transitionOccurrences ++ remapTransitions(
            ownedContinuation.transitionOccurrences,
            ownedContinuation.replaySteps,
            ownedContinuation.legalSteps,
            continuationAnalyses
          )
        )
      )

  def continueFromHistory(
      prefix: CanonicalLineReplay,
      steps: List[CanonicalPositionHistoryStep]
  ): Option[CanonicalLineReplay] =
    val declared = steps.map { step =>
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
    Option.when(steps.nonEmpty)(()).flatMap { _ =>
      declared.zip(legal).foldLeft(Option(prefix)) {
        case (Some(replay), (nextDeclared, nextLegal)) => append(replay, nextDeclared, nextLegal)
        case (None, _)                                 => None
      }.flatMap(_.subset(declared))
    }

  private def append(
      prefix: CanonicalLineReplay,
      step: LineReplayStep,
      legal: LegalReplayStep
  ): Option[CanonicalLineReplay] =
    for
      previousReplayStep <- prefix.replaySteps.lastOption
      previousLegalStep <- prefix.legalSteps.lastOption
      previousAnalysis <- prefix.analysisAfter(previousReplayStep)
      if step.ply == previousReplayStep.ply + 1
      if legal.ply == step.ply
      if EvidenceRef.sameMove(step.moveUci, legal.uci)
      if previousLegalStep.after == legal.before
      if PrincipalVariationEvidence.sameBoardState(step.fenBefore, Fen.write(previousLegalStep.after).value)
      if PrincipalVariationEvidence.sameBoardState(step.fenBefore, Fen.write(legal.before).value)
      if PrincipalVariationEvidence.sameBoardState(step.fenAfter, Fen.write(legal.after).value)
    yield
      lazy val afterAnalysis = PositionAnalyzer.analyzeAfter(previousAnalysis, legal, step.fenAfter)
      val appendedAnalysis = LazyList(afterAnalysis)
      val appendedTransition = LazyList(
        CanonicalReplayTransition.fresh(step, legal, previousAnalysis, afterAnalysis)
      )
      new CanonicalLineReplay(
        prefix.replaySteps :+ step,
        prefix.legalSteps :+ legal,
        Some(prefix.occurrenceAnalyses ++ appendedAnalysis),
        Some(prefix.transitionOccurrences ++ appendedTransition)
      )

/** One admitted legal move bound to one structural transition occurrence.
  * Consumers compare this certificate with the declared transition instead
  * of replaying the same move again.
  */
private[chessjudgment] final class CanonicalTransitionProof private (
    private[judgment] val transitionBinding: StructuralTransitionBinding,
    legal: LegalReplayStep,
    afterAnalysis: PositionAnalysis,
    private[chessjudgment] val relationDelta: RelationSemanticDelta
):
  private lazy val legalResponses: List[Move] = afterAnalysis.actualLegalMoves
  private lazy val legalResponsesByUci: Map[String, Move] =
    legalResponses.map(move => EvidenceRef.normalizeMove(move.toUci.uci) -> move).toMap

  def proves(transition: StructuralTransitionBinding): Boolean =
    transition == transitionBinding

  private[chessjudgment] def provesMove(
      moveUci: String,
      from: PositionNodeRef,
      to: PositionNodeRef,
      scope: EvidenceScope
  ): Boolean =
    transitionBinding.line.isEmpty && transitionBinding.role.scope == scope &&
      EvidenceRef.sameMove(transitionBinding.moveUci, moveUci) &&
      transitionBinding.from == from && transitionBinding.to == to &&
      proves(transitionBinding)

  private[chessjudgment] def rootStep: LegalReplayStep = legal

  private[chessjudgment] def rootMovement: CanonicalRootLegalMove =
    relationDelta.rootMove

  private[chessjudgment] def legalResponseCount: Int =
    legalResponses.size

  private[chessjudgment] def legalResponseMoves: List[String] =
    legalResponsesByUci.keys.toList.sorted

  private[chessjudgment] def certifiesCompleteLegalResponseSet(
      responses: List[LineReplayStep]
  ): Boolean =
    responses.size == legalResponseCount &&
      responses.forall(response =>
        response.ply == transitionBinding.to.ply + 1 &&
          legalResponsesByUci
            .get(EvidenceRef.normalizeMove(response.moveUci))
            .exists(move =>
              PrincipalVariationEvidence.sameBoardState(response.fenBefore, Fen.write(move.before).value) &&
                PrincipalVariationEvidence.sameBoardState(response.fenAfter, Fen.write(move.after).value)
            )
      ) &&
      responses.map(response => EvidenceRef.normalizeMove(response.moveUci)).toSet == legalResponsesByUci.keySet

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
          owner.legal,
          owner.afterAnalysis,
          owner.relationDelta
        )
      )

/** Binds the exact structural output inventory to the admitted legal
  * transition that produced it.  A legal move certificate alone cannot
  * certify arbitrary copied or hand-written consequences.
  */
private[chessjudgment] final case class CanonicalTransitionDeltaProof private (
    transitionProof: CanonicalTransitionProof,
    signals: List[StructuralSignal],
    consequences: List[TransitionConsequence],
    relationChanges: List[CanonicalRelationChange],
    derivedRelations: List[RelationFactEvidence],
    derivedRelationSources: List[StructuralDerivedRelationSource]
):
  def proves(delta: StructuralDeltaEvidence): Boolean =
    transitionProof.proves(delta.transition) &&
      signals == delta.signals &&
      consequences == delta.consequences &&
      relationChanges == delta.relationChanges &&
      derivedRelationSources == delta.derivedRelationSources &&
      TransitionConsequenceRelationProof.provesCanonical(
        consequences,
        relationChanges,
        delta.transition,
        transitionProof.relationDelta,
        derivedRelations
      )

private[chessjudgment] object CanonicalTransitionDeltaProof:
  def from(
      transitionProof: CanonicalTransitionProof,
      signals: List[StructuralSignal],
      consequences: List[TransitionConsequence],
      relationChanges: List[CanonicalRelationChange],
      derivedRelations: List[RelationFactEvidence],
      derivedRelationSources: List[StructuralDerivedRelationSource]
  ): CanonicalTransitionDeltaProof =
    require(
      derivedRelationSources.map(_.key).distinct.size == derivedRelationSources.size &&
        derivedRelationSources.map(_.source).distinct.size == derivedRelationSources.size &&
        derivedRelationSources.map(_.key).toSet == derivedRelations.map(DerivedRelationResultKey.from).toSet,
      "canonical structural proof needs one occurrence output per derived relation"
    )
    require(
      TransitionConsequenceRelationProof.provesCanonical(
        consequences,
        relationChanges,
        transitionProof.transitionBinding,
        transitionProof.relationDelta,
        derivedRelations
      ),
      "canonical transition consequences must be causally bound to their exact relation changes"
    )
    CanonicalTransitionDeltaProof(
      transitionProof,
      signals,
      consequences,
      relationChanges,
      derivedRelations,
      derivedRelationSources
    )
