package lila.chessjudgment.model.judgment

import chess.{ Color, Move, Position, Role }
import chess.format.Fen

import lila.chessjudgment.analysis.position.{ PositionAnalysis, PositionAnalyzer, PositionRelationExtractor }
import lila.chessjudgment.analysis.structure.{
  StructuralDeltaAnalyzer,
  StructuralDeltaContracts,
  TransitionStructuralDelta
}
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

  lazy val structuralDelta: TransitionStructuralDelta =
    StructuralDeltaAnalyzer.delta(
      beforeAnalysis,
      afterAnalysis,
      legalStep,
      relationDelta,
      certifiedVertical.resultsFor(VerticalRelationContractKind.PawnTopologyTransition)
    )

  lazy val structuralConsequences: List[TransitionConsequence] =
    StructuralDeltaContracts.consequences(structuralDelta)

  def structuralOccurrence(
      step: LineReplayStep,
      resultPremiseOccurrences: List[ReplayVerticalRelationOccurrenceBinding]
  ): ReplayStructuralOccurrence =
    ReplayStructuralOccurrence.certified(
      step,
      relationDelta.rootMove,
      structuralConsequences,
      resultPremiseOccurrences
    )

/** Opaque owner of one replay destination and its already-closed position
  * inventory. It may answer only targeted queries against that exact inventory;
  * no board fact is recomputed or persisted as another graph record.
  */
private[chessjudgment] final class ReplayPositionOccurrence private[judgment] (
    val step: LineReplayStep,
    private val analysis: PositionAnalysis
):
  require(
    analysis.occurrence.plyCount == step.ply &&
      analysis.occurrence.sideToMove == analysis.position.color &&
      PrincipalVariationEvidence.sameBoardState(analysis.occurrence.fen, step.fenAfter),
    "a replay position occurrence must retain its exact after-step analysis"
  )

  val position: PositionNodeRef =
    PositionNodeRef(step.fenAfter, step.ply, Some(analysis.occurrence.sideToMove))

  private val inventory = analysis.relationInventory

  /** Transposed routes share the same semantic occurrence at the same ply.
    * Concrete route ownership remains in the causal branch and LegalLine
    * evidence ids rather than being folded into this position identity.
    */
  private[chessjudgment] lazy val occurrenceId: String =
    val semanticBoard = PrincipalVariationEvidence
      .semanticBoardStateFen(position.fen)
      .getOrElse(throw IllegalArgumentException("a replay position occurrence needs a semantic board state"))
    BoundedCausalIdentity.digest(
      List("replay-position-after-occurrence:v1", semanticBoard, position.ply.toString)
    )

  private[chessjudgment] def closedAbsence(
      query: PositionRelationExtractor.ClosedRelationAbsenceQuery,
      scope: EvidenceScope
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceProof] =
    inventory.certifyAbsence(query).flatMap(certificate =>
      inventory.bindAbsence(certificate, position, scope)
    )

  private[chessjudgment] def closedState(
      query: PositionRelationExtractor.ClosedPositionStateQuery,
      scope: EvidenceScope
  ): Option[PositionRelationExtractor.ClosedPositionStateProof] =
    inventory.certifyState(query).flatMap(certificate =>
      inventory.bindState(certificate, position, scope)
    )

  /** Selects one exact reach already owned by this position inventory, then
    * binds that full state to this replay occurrence. No ray or control is
    * regenerated from the FEN.
    */
  private[chessjudgment] def existingSliderReachState(
      side: Color,
      slider: RelationPieceWitness,
      direction: RelationRayDirection,
      scope: EvidenceScope
  ): Option[PositionRelationExtractor.ClosedPositionStateProof] =
    inventory
      .sliderReachesFrom(slider.square)
      .filter(reach => reach.side == side && reach.slider == slider && reach.direction == direction) match
      case exact :: Nil =>
        closedState(
          PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
            side,
            slider,
            direction,
            Some(exact.witness)
          ),
          scope
        )
      case Nil => None
      case other =>
        throw IllegalStateException(
          s"one replay position produced ${other.size} slider reaches for one side/piece/direction"
        )

  /** Binds one already-owned occupant fact to this exact replay occurrence.
    * The query is answered by the position inventory; callers never infer
    * persistence from an unchanged-square shortcut.
    */
  private[chessjudgment] def existingOccupantState(
      piece: RelationColoredPieceWitness,
      scope: EvidenceScope
  ): Option[PositionRelationExtractor.ClosedPositionStateProof] =
    closedState(
      PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece),
      scope
    )

  /** Selects the exact pawn-topology witness already owned by this position
    * and binds it to the replay occurrence. Requiring the complete witness is
    * deliberately stricter than treating a missing transition as persistence.
    */
  private[chessjudgment] def existingPawnTopologyState(
      side: Color,
      square: EvidenceSquare,
      passed: Boolean,
      scope: EvidenceScope
  ): Option[PositionRelationExtractor.ClosedPositionStateProof] =
    inventory.pawnTopologyView
      .stateWitness(side, square)
      .filter(_.passed == passed)
      .flatMap(exact =>
        closedState(
          PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(exact),
          scope
        )
      )

  private[chessjudgment] def certifies(
      proof: PositionRelationExtractor.ClosedRelationAbsenceProof,
      scope: EvidenceScope
  ): Boolean =
    proof.sameOwner(position, scope, inventory)

  private[chessjudgment] def certifies(
      proof: PositionRelationExtractor.ClosedPositionStateProof,
      scope: EvidenceScope
  ): Boolean =
    proof.sameOwner(position, scope, inventory)

  private[chessjudgment] def sameOwner(other: ReplayPositionOccurrence): Boolean =
    occurrenceId == other.occurrenceId && inventory.sameOwner(other.inventory)

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

  /** Exact occurrence identity issued by the canonical replay inventory. A
    * graph evidence id later binds this occurrence to one admitted line.
    */
  private[chessjudgment] lazy val occurrenceId: String =
    BoundedCausalIdentity.digest(
      List(
        "replay-vertical-relation-occurrence:v1",
        BoundedCausalIdentity.stepKey(step),
        contract.toString.toLowerCase,
        DerivedRelationResultKey.from(relation).stableKey,
        certifiedSourcePremiseIds.mkString("[", ",", "]")
      )
    )

  /** Lower-premise identity is exposed only through this replay-owned
    * occurrence. L2 consumers cannot manufacture a premise manifest from raw
    * result ids or an arbitrary string list.
    */
  private[chessjudgment] lazy val certifiedSourcePremiseIds: List[String] =
    ReplayVerticalRelationOccurrence.certifiedSourcePremiseIds(relation)

private[chessjudgment] object ReplayVerticalRelationOccurrence:
  private[chessjudgment] def certifiedSourcePremiseIds(
      relation: RelationFactEvidence
  ): List[String] =
    val proof = relation.detail match
      case detail: RelationWitnessDetail.CaptureRecaptureInventory => detail.proof
      case detail: RelationWitnessDetail.CreatedCheckResponseInventory => detail.proof
      case detail: RelationWitnessDetail.RootCheckResponse => detail.proof
      case detail: RelationWitnessDetail.SliderReachDelta => detail.proof
      case detail: RelationWitnessDetail.PawnTopologyTransition => detail.proof
      case detail: RelationWitnessDetail.StalemateTransition => detail.proof
      case _ =>
        throw IllegalStateException("a replay vertical occurrence lost its vertical derivation proof")
    proof.sourcePremises.map(_.stableKey).distinct.sorted

/** Minimal transport form for one replay-owned L1 occurrence. It retains the
  * exact time-axis address and certified lower owners without exposing the
  * closed inventory capability carried by the source occurrence.
  */
private[chessjudgment] final case class ReplayVerticalRelationOccurrenceBinding private (
    step: LineReplayStep,
    contract: VerticalRelationContractKind,
    result: DerivedRelationResultKey,
    occurrenceId: String,
    certifiedSourcePremiseIds: List[String]
):
  require(occurrenceId.matches("[0-9a-f]{64}"), "an L1 occurrence binding needs its canonical id")
  require(
    certifiedSourcePremiseIds.nonEmpty &&
      certifiedSourcePremiseIds == certifiedSourcePremiseIds.distinct.sorted,
    "an L1 occurrence binding needs exact canonical lower premise owners"
  )

  private[chessjudgment] def stableKey: String =
    BoundedCausalIdentity.digest(List(
      "replay-vertical-relation-occurrence-binding:v1",
      BoundedCausalIdentity.stepKey(step),
      contract.toString.toLowerCase,
      result.stableKey,
      occurrenceId,
      certifiedSourcePremiseIds.mkString("[", ",", "]")
    ))

private[chessjudgment] object ReplayVerticalRelationOccurrenceBinding:
  private[chessjudgment] def from(
      occurrence: ReplayVerticalRelationOccurrence
  ): ReplayVerticalRelationOccurrenceBinding =
    ReplayVerticalRelationOccurrenceBinding(
      occurrence.step,
      occurrence.contract,
      DerivedRelationResultKey.from(occurrence.relation),
      occurrence.occurrenceId,
      occurrence.certifiedSourcePremiseIds
    )

/** Replay-owned exact consequence inventory for one admitted transition occurrence.
  * Root graph records and bounded L2 consumers project this same calculation;
  * neither is allowed to invoke the structural contracts again.
  */
private[chessjudgment] final case class ReplayStructuralOccurrence private[judgment] (
    step: LineReplayStep,
    occurrenceId: String,
    consequences: List[TransitionConsequence],
    resultPremiseOccurrences: List[ReplayVerticalRelationOccurrenceBinding],
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
      resultPremiseOccurrences: List[ReplayVerticalRelationOccurrenceBinding]
  ): ReplayStructuralOccurrence =
    val expectedResultKeys = consequences.flatMap(_.resultPremiseKeys).distinct.sortBy(_.stableKey)
    require(
      resultPremiseOccurrences.map(_.result) == expectedResultKeys &&
        resultPremiseOccurrences.map(_.occurrenceId).distinct.size == resultPremiseOccurrences.size &&
        resultPremiseOccurrences.forall(_.step == step),
      "a structural occurrence must retain every consumed L1 result at this exact replay occurrence"
    )
    val relationPremises = consequences.flatMap(consequence =>
      (consequence.subjectBindings ++ consequence.targetBindings).flatMap(binding =>
        binding.relationKeys.map(key => s"relation:${key.stableKey}")
      )
    )
    val resultPremises = resultPremiseOccurrences.flatMap(binding =>
      List(
        s"result:${binding.result.stableKey}",
        s"result-occurrence:${binding.occurrenceId}"
      ) ++ binding.certifiedSourcePremiseIds
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
    val digest = BoundedCausalIdentity.digest(semanticParts)
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

  private[chessjudgment] def activatesVerticalRelation(
      contract: VerticalRelationContractKind
  ): Boolean =
    calculation.certifiedVertical.activates(contract)

  private[chessjudgment] def closedRelationInventory: ClosedRelationTransitionInventory =
    closedRelationOutput

  def structuralDelta: TransitionStructuralDelta = calculation.structuralDelta

  private[chessjudgment] def structuralConsequences: List[TransitionConsequence] =
    calculation.structuralConsequences

  private[chessjudgment] def ownsStructuralDelta(delta: TransitionStructuralDelta): Boolean =
    calculation.structuralDelta.asInstanceOf[AnyRef] eq delta.asInstanceOf[AnyRef]

  private[chessjudgment] lazy val structuralOccurrence: ReplayStructuralOccurrence =
    val expectedKeys = calculation.structuralConsequences.flatMap(_.resultPremiseKeys).toSet
    val occurrences = VerticalRelationContractKind.values.toList.flatMap { contract =>
      verticalRelationsFor(contract).flatMap { relation =>
        val key = DerivedRelationResultKey.from(relation)
        Option.when(expectedKeys(key))(
          ReplayVerticalRelationOccurrenceBinding.from(
            ReplayVerticalRelationOccurrence(declared, contract, relation)
          )
        )
      }
    }.sortBy(_.result.stableKey)
    require(
      occurrences.map(_.result).distinct.size == occurrences.size &&
        occurrences.map(_.result).toSet == expectedKeys,
      "a structural occurrence must resolve every consumed L1 result exactly once"
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
  private val positionOccurrenceCache =
    scala.collection.mutable.Map.empty[LineReplayStep, ReplayPositionOccurrence]
  require(
    indexByReplayStep.size == replaySteps.size,
    "a canonical replay cannot contain the same time-axis occurrence twice"
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

  private[chessjudgment] def positionAfter(
      step: LineReplayStep
  ): Option[ReplayPositionOccurrence] =
    indexByReplayStep.get(step).flatMap { index =>
      positionOccurrenceCache.synchronized {
        positionOccurrenceCache.get(step).orElse(
          occurrenceAnalyses.lift(index + 1).map { analysis =>
            val occurrence = new ReplayPositionOccurrence(step, analysis)
            positionOccurrenceCache.update(step, occurrence)
            occurrence
          }
        )
      }
    }

  def transition(step: LineReplayStep): Option[CanonicalReplayTransition] =
    indexByReplayStep.get(step).flatMap(transitionOccurrences.lift)

  private[chessjudgment] def structuralOccurrence(
      step: LineReplayStep
  ): Option[ReplayStructuralOccurrence] =
    transition(step).map(_.structuralOccurrence)

  /** Changed-dependency ownership for callers that must retain the exact
    * time-axis occurrence instead of reducing dispatch to a boolean and
    * rediscovering the same transition later.
    */
  private[chessjudgment] def structuralConsequenceOccurrences(
      kind: TransitionConsequenceKind
  ): List[(LineReplayStep, List[TransitionConsequence])] =
    replaySteps.flatMap(step =>
      transition(step).toList.flatMap { occurrence =>
        val consequences = occurrence.structuralConsequences.filter(_.kind == kind)
        Option.when(consequences.nonEmpty)(step -> consequences)
      }
    )

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

  /** Read-only L1 demand key for one exact transition occurrence. The
    * underlying vertical contract remains lazy until a selected consumer asks
    * for its closed result.
    */
  private[chessjudgment] def activatesVerticalRelation(
      step: LineReplayStep,
      contract: VerticalRelationContractKind
  ): Boolean =
    transition(step).exists(_.activatesVerticalRelation(contract))

  /** Exact L1 membership owned by the capture transition occurrence. Missing
    * or duplicate inventories/resources fail closed instead of creating a
    * second recapture adjudicator in a line consumer.
    */
  private[chessjudgment] def exactRecaptureMembership(
      captureStep: LineReplayStep,
      recaptureStep: LineReplayStep
  ): Option[(DerivedRelationResultKey, RelationLegalMoveResourceWitness)] =
    exactRecaptureMembership(captureStep, this, recaptureStep)

  private[chessjudgment] def exactRecaptureMembership(
      captureStep: LineReplayStep,
      recaptureReplay: CanonicalLineReplay,
      recaptureStep: LineReplayStep
  ): Option[(DerivedRelationResultKey, RelationLegalMoveResourceWitness)] =
    exactRecaptureResolution(captureStep, recaptureReplay, recaptureStep)
      .flatMap(resolution =>
        resolution.resource.map(DerivedRelationResultKey.from(resolution.occurrence.relation) -> _)
      )

  private[chessjudgment] def exactRecaptureOccurrenceMembership(
      captureStep: LineReplayStep,
      recaptureStep: LineReplayStep
  ): Option[(ReplayVerticalRelationOccurrence, RelationLegalMoveResourceWitness)] =
    exactRecaptureResolution(captureStep, this, recaptureStep)
      .flatMap(resolution => resolution.resource.map(resolution.occurrence -> _))

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
            DerivedRelationResultKey.from(resolution.occurrence.relation)
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
      if recaptureStep.ply == captureStep.ply + 1
      if PrincipalVariationEvidence.sameBoardState(captureStep.fenAfter, recaptureStep.fenBefore)
      captureLegal <- legalStep(captureStep)
      recaptureLegal <- recaptureReplay.legalStep(recaptureStep)
      captureTransition <- transition(captureStep)
      recaptureTransition <- recaptureReplay.transition(recaptureStep)
      captureRoot = captureTransition.relationDelta.rootMove
      recaptureRoot = recaptureTransition.relationDelta.rootMove
      if captureRoot.capture.nonEmpty
      inventory <- only(verticalRelationOccurrences(
        captureStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ).flatMap(occurrence =>
        occurrence.relation.detail match
          case detail: RelationWitnessDetail.CaptureRecaptureInventory =>
            List(occurrence -> detail)
          case _ => Nil
      ))
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

  /** Exact L1 membership owned by the check-producing transition occurrence.
    * Only the immediately following canonical response can consume it.
    */
  private[chessjudgment] def exactCheckResponseMembership(
      checkStep: LineReplayStep,
      responseStep: LineReplayStep
  ): Option[(DerivedRelationResultKey, RelationCheckResponseWitness)] =
    exactCheckResponseOccurrenceMembership(checkStep, responseStep).map { case (occurrence, response) =>
      DerivedRelationResultKey.from(occurrence.relation) -> response
    }

  private[chessjudgment] def exactCheckResponseOccurrenceMembership(
      checkStep: LineReplayStep,
      responseStep: LineReplayStep
  ): Option[(ReplayVerticalRelationOccurrence, RelationCheckResponseWitness)] =
    for
      _ <- indexByReplayStep.get(checkStep)
      _ <- indexByReplayStep.get(responseStep)
      if responseStep.ply == checkStep.ply + 1
      if PrincipalVariationEvidence.sameBoardState(checkStep.fenAfter, responseStep.fenBefore)
      checkLegal <- legalStep(checkStep)
      responseLegal <- legalStep(responseStep)
      if checkLegal.after == responseLegal.before
      checkTransition <- transition(checkStep)
      responseTransition <- transition(responseStep)
      checkRoot = checkTransition.relationDelta.rootMove
      responseRoot = responseTransition.relationDelta.rootMove
      inventory <- only(verticalRelationOccurrences(
        checkStep,
        List(VerticalRelationContractKind.CreatedCheckResponseInventory)
      ).flatMap(occurrence =>
        occurrence.relation.detail match
          case detail: RelationWitnessDetail.CreatedCheckResponseInventory =>
            List(occurrence -> detail)
          case _ => Nil
      ))
      (occurrence, detail) = inventory
      if detail.mover == checkRoot.witness && legalMovementMatches(detail.mover, checkLegal)
      if detail.checkedSide == responseRoot.side && detail.terminal == RelationCheckTerminalState.Ongoing
      response <- only(detail.responses.filter(response =>
        EvidenceRef.sameMove(response.resource.moveUci, responseStep.moveUci) &&
          response.resource.movement == responseRoot.witness &&
          response.resource.capture == responseRoot.capture &&
          legalMovementMatches(response.resource.movement, responseLegal)
      ))
    yield occurrence -> response

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
        existing.occurrence.plyCount == rootAnalysis.occurrence.plyCount &&
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
      if previousAnalysis.occurrence.plyCount == continuationRoot.occurrence.plyCount
      if PrincipalVariationEvidence.sameBoardState(
        previousAnalysis.occurrence.fen,
        continuationRoot.occurrence.fen
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
      "canonical transition consequences must be causally bound to their exact relation changes"
    )
    CanonicalTransitionDeltaProof(
      transitionProof,
      canonicalRelations,
      consequences,
      relationChanges,
      resultPremiseSources
    )
