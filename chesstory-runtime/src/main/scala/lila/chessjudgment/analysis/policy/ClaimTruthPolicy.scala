package lila.chessjudgment.analysis.policy

import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.model.judgment.*

enum ClaimAdmissionStatus:
  case Certified
  case Deferred
  case Rejected

final case class ClaimAdmissionDecision(
    claim: JudgmentClaim,
    status: ClaimAdmissionStatus,
    presentLayers: Set[EvidenceLayer],
    missingLayerGroups: List[Set[EvidenceLayer]],
    missingEvidence: List[EvidenceRef]
):
  def certified: Boolean = status == ClaimAdmissionStatus.Certified

object ClaimTruthPolicy:

  def evaluate(claim: JudgmentClaim, graph: TypedEvidenceGraph): ClaimAdmissionDecision =
    val graphIds = graph.byId.keySet
    val missingEvidence = claim.evidence.filterNot(ref => graphIds.contains(ref.id))
    val claimBoundRecords =
      claim.evidence
        .flatMap(ref => graph.byId.get(ref.id))
        .filter(isBoundToClaim(claim, _, graph))
    val proofRecords =
      (claimBoundRecords ++ claimBoundRecords.flatMap(graph.parentClosure)).distinctBy(_.ref.id)
    val claimBoundLayers =
      proofRecords
        .map(_.ref.layer)
        .toSet
    val missingGroups =
      requiredLayerGroups(claim.family).filterNot(group => group.exists(claimBoundLayers.contains))
    val hasFamilyProof =
      familySpecificProof(claim, proofRecords, graph)
    val status =
      if claim.evidence.isEmpty || missingEvidence.nonEmpty then ClaimAdmissionStatus.Rejected
      else if claimBoundRecords.isEmpty then ClaimAdmissionStatus.Rejected
      else if missingGroups.nonEmpty || !hasFamilyProof then ClaimAdmissionStatus.Deferred
      else ClaimAdmissionStatus.Certified
    ClaimAdmissionDecision(
      claim = claim,
      status = status,
      presentLayers = claimBoundLayers,
      missingLayerGroups = missingGroups,
      missingEvidence = missingEvidence
    )

  private def isBoundToClaim(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Boolean =
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    val samePosition = record.ref.position == claim.primaryPosition
    val layerCompatible = claimLayerCompatible(record)
    val sameLine = layerCompatible && claim.primaryLine.exists(line => recordLineMatches(record, line, graph))
    val sameSubjectMove =
      layerCompatible && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine, graph))
    val comparisonLineSupport =
      !claim.family.isLongTerm &&
        comparisonProofLayer(record.ref.layer) &&
        comparisonLinesForClaim(claim, graph).exists(line => recordLineMatches(record, line, graph))
    val transitionDestinationLocal =
      claimEvidenceIds.contains(record.ref.id) &&
        positionLocalLayer(record.ref.layer) &&
        transitionDestinationsForClaim(claim, graph).contains(record.ref.position)
    val samePositionLocal =
      samePosition &&
        claim.subject == ClaimSubject.Position &&
        claim.subjectMove.isEmpty &&
        record.ref.line.isEmpty &&
        positionLocalLayer(record.ref.layer)
    val parentLinked = record.parents.exists(parent => claimEvidenceIds.contains(parent.id))
    val childLinked =
      graph.records.exists(child =>
        claimEvidenceIds.contains(child.ref.id) &&
          child.parents.exists(parent => parent.id == record.ref.id)
      )
    val relativeCauseProofLinked =
      relativeCauseProofSourceIds(claim, graph).contains(record.ref.id)
    val linkedBound =
      (parentLinked || childLinked) &&
        linkedRecordCompatible(claim, record, graph, samePositionLocal)
    sameLine || sameSubjectMove || comparisonLineSupport || transitionDestinationLocal || samePositionLocal || linkedBound ||
      relativeCauseProofLinked

  private def relativeCauseProofSourceIds(claim: JudgmentClaim, graph: TypedEvidenceGraph): Set[String] =
    claim.evidence
      .flatMap(ref => graph.byId.get(ref.id))
      .flatMap {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          relativeCauseProofSourceIds(cause)
        case _ =>
          Nil
      }
      .map(_.id)
      .toSet

  private def relativeCauseProofSourceIds(cause: RelativeCauseFact): List[EvidenceRef] =
    cause.comparisonEvidence :: cause.supportEvidence ++
      cause.proof.toList.flatMap(proof =>
        proof.directProof.sourceRefs ++
          proof.contrastProof.sourceRefs ++
          proof.contextSupport.sourceRefs
      )

  private def linkedRecordCompatible(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      samePositionLocal: Boolean
  ): Boolean =
    val supportLayer = linkedSupportLayer(record.ref.layer)
    (supportLayer && claim.primaryLine.exists(line => recordLineMatches(record, line, graph))) ||
      (supportLayer && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine, graph))) ||
      samePositionLocal ||
      linkedPositionContext(claim, record) ||
      linkedClaimSupport(claim, record, graph)

  private def linkedPositionContext(claim: JudgmentClaim, record: EvidenceRecord): Boolean =
    claim.primaryLine.isEmpty &&
      claim.subjectMove.isEmpty &&
      record.ref.position == claim.primaryPosition &&
      positionContextLayer(claim.family, record.ref.layer)

  private def linkedClaimSupport(claim: JudgmentClaim, record: EvidenceRecord, graph: TypedEvidenceGraph): Boolean =
    val comparisonLineBound =
      comparisonLinesForClaim(claim, graph).exists(line => recordLineMatches(record, line, graph))
    claim.primaryLine.nonEmpty &&
      (record.ref.position == claim.primaryPosition || comparisonLineBound) &&
      linkedSupportLayer(record.ref.layer)

  private def linkedSupportLayer(layer: EvidenceLayer): Boolean =
    claimProofCarrierLayer(layer)

  private def claimProofCarrierLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board => false
      case _                   => true

  private def claimLayerCompatible(record: EvidenceRecord): Boolean =
    linkedSupportLayer(record.ref.layer) ||
      positionLocalLayer(record.ref.layer)

  private def positionContextLayer(family: ClaimFamily, layer: EvidenceLayer): Boolean =
    if family.isLongTerm then positionLocalLayer(layer)
    else
      layer match
        case EvidenceLayer.Board | EvidenceLayer.Line | EvidenceLayer.Eval =>
          true
        case _ =>
          false

  private def comparisonProofLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Line | EvidenceLayer.Eval =>
        true
      case _ =>
        false

  private def comparisonLinesForClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[LineNodeRef] =
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    graph.records
      .filter(record => claimEvidenceIds.contains(record.ref.id))
      .flatMap(record => comparisonLinesForRecord(record, graph))
      .distinct

  private def transitionDestinationsForClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): Set[PositionNodeRef] =
    val claimMoves = claim.subjectMove.toSet ++ claim.primaryLine.map(_.rootMove).toSet
    if claimMoves.isEmpty then Set.empty
    else
      val claimEvidenceIds = claim.evidence.map(_.id).toSet
      graph.records
        .filter(record => claimEvidenceIds.contains(record.ref.id))
        .collect {
          case EvidenceRecord(_, MoveTransitionEvidence(moveUci, _, to), _) if claimMoves.contains(moveUci) =>
            to
        }
        .toSet

  private def comparisonLinesForRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): List[LineNodeRef] =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case RelativeCauseFactEvidence(cause) =>
        graph.relativeCauseBinding(cause).map(_.eventLine).toList
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case _ =>
        Nil

  private def recordLineMatches(
      record: EvidenceRecord,
      line: LineNodeRef,
      graph: TypedEvidenceGraph
  ): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        graph.relativeCauseBinding(cause).exists(_.eventLine == line)
      case _ =>
        record.referencesLine(line)

  private def recordMentionsMove(
      record: EvidenceRecord,
      move: String,
      claimLine: Option[LineNodeRef],
      graph: TypedEvidenceGraph
  ): Boolean =
    record.payload match
      case payload: MoveMotifEvidence =>
        payload.moveUci == move
      case MoveTransitionEvidence(moveUci, _, _) =>
        moveUci == move &&
          claimLine.forall(line =>
            LineNodeRole.fromMoveEvidenceScope(record.ref.scope).contains(line.role) && line.rootMove == move
          )
      case payload: StructuralDeltaEvidence =>
        payload.moveUci == move &&
          claimLine.forall(line => payload.line.contains(line))
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.contains(move) ||
          claimLine.exists(line => payload.line.contains(line))
      case CandidateComparisonEvidence(fact) =>
        claimLine.exists(line => line == fact.referenceLine || line == fact.candidateLine)
      case RelativeCauseFactEvidence(cause) =>
        graph.relativeCauseBinding(cause).exists(binding =>
          claimLine.exists(_ == binding.eventLine) || binding.eventLine.rootMove == move
        )
      case _ =>
        false

  private def positionLocalLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def requiredLayerGroups(family: ClaimFamily): List[Set[EvidenceLayer]] =
    family match
      case ClaimFamily.Tactical =>
        List(
          Set(EvidenceLayer.TacticalMechanism, EvidenceLayer.RelativeCause, EvidenceLayer.Relation, EvidenceLayer.Line),
          Set(EvidenceLayer.Line, EvidenceLayer.Relation, EvidenceLayer.TacticalMechanism, EvidenceLayer.Eval),
          Set(EvidenceLayer.Eval, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.Strategic =>
        List(
          Set(EvidenceLayer.StrategicMechanism, EvidenceLayer.RelativeCause),
          Set(
            EvidenceLayer.Board,
            EvidenceLayer.PawnStructure,
            EvidenceLayer.Strategic,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.FeatureAnchor
          ),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.MoveTransition,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.RelativeCause
          )
        )
      case ClaimFamily.PawnStructure =>
        List(
          Set(EvidenceLayer.StrategicMechanism, EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.PawnStructure, EvidenceLayer.StructuralDelta, EvidenceLayer.Strategic),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.MoveTransition,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.RelativeCause
          )
        )
      case ClaimFamily.Opening =>
        List(
          Set(EvidenceLayer.StrategicMechanism),
          Set(EvidenceLayer.OpeningContext),
          Set(EvidenceLayer.ApplicabilityAssessment),
          Set(EvidenceLayer.FeatureAnchor)
        )
      case ClaimFamily.Plan =>
        List(Set(EvidenceLayer.StrategicMechanism))
      case ClaimFamily.Defensive =>
        List(
          Set(
            EvidenceLayer.ThreatPressure,
            EvidenceLayer.TacticalMechanism,
            EvidenceLayer.RelativeCause
          ),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.TacticalMechanism
          )
        )
      case ClaimFamily.Conversion =>
        List(
          Set(EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.Line, EvidenceLayer.Board, EvidenceLayer.StructuralDelta, EvidenceLayer.Relation),
          Set(EvidenceLayer.Eval, EvidenceLayer.Line, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.Material =>
        List(
          Set(EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.Line),
          Set(EvidenceLayer.Eval, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.Evaluation =>
        List(Set(EvidenceLayer.RelativeAssessment), Set(EvidenceLayer.CandidateComparison))

  private def familySpecificProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    claim.family match
      case ClaimFamily.Tactical =>
        tacticalProof(records, graph)
      case ClaimFamily.Defensive =>
        defensiveProof(claim, records, graph)
      case ClaimFamily.Strategic =>
        strategicProof(claim, records, graph)
      case ClaimFamily.PawnStructure =>
        pawnStructureProof(claim, records, graph)
      case ClaimFamily.Opening =>
        openingProof(records)
      case ClaimFamily.Plan =>
        planProof(records, graph)
      case ClaimFamily.Conversion =>
        conversionProof(records, graph)
      case ClaimFamily.Material =>
        materialProof(records, graph)
      case ClaimFamily.Evaluation =>
        evaluationProof(records, graph)

  private def strategicProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val hasObservation = records.exists(strategicObservation)
    val hasCausalSupport = records.exists(longTermCausalSupport(claim, _, graph))
    val hasStrategicAnchor = records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        graph.comparisonFor(cause).nonEmpty && strategicRelativeCauseHasProof(cause, graph)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.hasActionableContrast && payload.sustainability.hasSustainedPv
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorStrategicClaim
      case _ =>
        false
    }
    hasObservation && hasCausalSupport && hasStrategicAnchor

  private def pawnStructureProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val hasObservation = records.exists(pawnStructureObservation)
    val hasCausalSupport = records.exists(longTermCausalSupport(claim, _, graph))
    val hasPawnAnchor = records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        graph.comparisonFor(cause).nonEmpty &&
          strategicRelativeCauseHasProof(cause, graph) &&
          pawnStructureCause(cause, graph)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.sustainability.hasSustainedPv &&
          payload.actionableComparisons.exists(_.axis.kind == StrategicAxisKind.PawnBreak)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorPawnStructureClaim
      case _ =>
        false
    }
    hasObservation && hasCausalSupport && hasPawnAnchor

  private def openingProof(records: List[EvidenceRecord]): Boolean =
    val certifyingPriorThemes =
      records
        .collect {
          case EvidenceRecord(_, OpeningContextEvidence(_, _, _, Some(selection)), _)
              if selection.canCertifyOpeningClaim =>
            selection.prior.themes
        }
        .flatten
        .toSet
    val certifyingAssessments =
      records.collect {
        case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), _)
            if assessment.canCertifyOpeningClaim =>
          assessment
      }
    val observedApplicableThemes =
      certifyingAssessments
        .flatMap(assessment => assessment.observedThemes.intersect(assessment.supportedThemes))
        .toSet
        .intersect(certifyingPriorThemes)
    val hasCurrentBoardAnchor =
      records.exists {
        case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) =>
          anchor.hasPositiveStrength &&
            anchor.canCorroborateOpeningPrior &&
            observedApplicableThemes.contains(anchor.theme)
        case _ =>
          false
      }
    StrategicMechanismEvidence.openingClaimSupported(records) &&
      certifyingPriorThemes.nonEmpty &&
      observedApplicableThemes.nonEmpty &&
      hasCurrentBoardAnchor

  private def strategicObservation(record: EvidenceRecord): Boolean =
    record.payload match
      case _: BoardFactEvidence | _: PawnStructureFactEvidence =>
        true
      case payload: StrategicFactEvidence =>
        payload.hasTypedSupport
      case payload: StructuralDeltaEvidence =>
        payload.hasTypedOutput
      case FeatureAnchorEvidence(anchor) =>
        anchor.hasPositiveStrength
      case _ =>
        false

  private def pawnStructureObservation(record: EvidenceRecord): Boolean =
    record.payload match
      case _: PawnStructureFactEvidence =>
        true
      case payload: StrategicFactEvidence =>
        payload.kind == StrategicFactKind.Structure && payload.hasTypedSupport
      case payload: StructuralDeltaEvidence =>
        payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure) ||
          payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructureDelta)
      case _ =>
        false

  private def longTermCausalSupport(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        graph.comparisonFor(cause).nonEmpty &&
          strategicRelativeCauseHasProof(cause, graph) &&
          causeBoundToClaim(claim, cause, graph)
      case payload: StrategicMechanismContrastEvidence =>
        payload.hasActionableContrast &&
          payload.sustainability.hasSustainedPv &&
          claim.primaryLine.exists(line =>
            line == payload.referenceLine || line == payload.candidateLine
          )
      case payload: StructuralDeltaEvidence =>
        payload.hasTypedOutput &&
          transitionBoundToClaim(claim, payload.moveUci, payload.line)
      case MoveTransitionEvidence(moveUci, _, _) =>
        transitionBoundToClaim(claim, moveUci, record.ref.line, Some(record.ref.scope))
      case _: LineFactEvidence =>
        claim.primaryLine.nonEmpty && record.ref.line == claim.primaryLine
      case _ =>
        false

  private def causeBoundToClaim(
      claim: JudgmentClaim,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    graph.relativeCauseBinding(cause).exists(binding =>
      claim.primaryLine.contains(binding.eventLine) ||
        claim.subjectMove.exists(move => EvidenceRef.sameMove(move, binding.eventLine.rootMove))
    )

  private def transitionBoundToClaim(
      claim: JudgmentClaim,
      moveUci: String,
      line: Option[LineNodeRef],
      scope: Option[EvidenceScope] = None
  ): Boolean =
    claim.primaryLine.exists(expected =>
      line.contains(expected) || (line.isEmpty && scope.contains(expected.role.scope))
    ) &&
      claim.subjectMove.forall(move => EvidenceRef.sameMove(move, moveUci))

  private def pawnStructureCause(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.kind == RelativeCauseKind.PawnWeaknessTarget ||
      cause.kind == RelativeCauseKind.PawnBreakOpportunity ||
      cause.proof.exists(proof =>
        graph
          .relativeCauseStrategicMechanisms(proof.directProof)
          .exists { case (_, payload) => payload.kind == StrategicMechanismKind.PawnStructure } ||
          graph
            .relativeCauseStrategicContrasts(proof.directProof)
            .exists { case (_, payload) =>
              payload.axisComparisons.exists(_.axis.kind == StrategicAxisKind.PawnBreak)
            } ||
          graph
            .relativeCauseTransitionConsequences(cause.kind, proof.directProof)
            .exists { case (_, _, consequence) => pawnStructureConsequence(consequence.kind) }
      )

  private def pawnStructureConsequence(kind: TransitionConsequenceKind): Boolean =
    kind match
      case TransitionConsequenceKind.OpenFileGain |
          TransitionConsequenceKind.SemiOpenFileGain |
          TransitionConsequenceKind.WeakPawnTargetCreated |
          TransitionConsequenceKind.PawnTensionGain |
          TransitionConsequenceKind.PawnTensionResolution |
          TransitionConsequenceKind.PassedPawnProgress |
          TransitionConsequenceKind.PassedPawnConcession |
          TransitionConsequenceKind.PromotionPressureGain |
          TransitionConsequenceKind.PromotionPressureConcession =>
        true
      case _ =>
        false

  private[chessjudgment] def planClaimApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.claims.exists(_.family == ClaimFamily.Plan) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) =>
          ref.confidence != EvidenceConfidence.Heuristic && payload.canAnchorPlanClaim
        case _ =>
          false
      }

  private def planProof(records: List[EvidenceRecord], graph: TypedEvidenceGraph): Boolean =
    records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        graph.comparisonFor(cause).nonEmpty &&
          strategicRelativeCauseHasProof(cause, graph) &&
          (cause.kind == RelativeCauseKind.PlanImprovement || cause.kind == RelativeCauseKind.PlanContradiction)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.planComparison.exists(_.hasPlanDelta)
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) =>
        ref.confidence != EvidenceConfidence.Heuristic && payload.canAnchorPlanClaim
      case _ =>
        false
    }

  private[chessjudgment] def pawnStructureCanAnchorPlan(payload: PawnStructureFactEvidence): Boolean =
    StrategicMechanismEvidence.pawnStructureCanAnchorPlan(payload)

  private def tacticalProof(records: List[EvidenceRecord], graph: TypedEvidenceGraph): Boolean =
    val hasTacticalAnchor =
      records.exists {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.canAnchorTacticalClaim
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasConcreteRelationProof
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          graph.comparisonFor(cause).nonEmpty &&
            ((tacticalRelativeCause(cause.kind) || materialResultCause(cause.kind)) &&
              relativeCauseHasTacticalProof(cause, graph)) ||
            (graph.comparisonFor(cause).nonEmpty && relativeCauseHasOwnedRelationTacticalProof(cause, graph))
        case _ =>
          false
      }
    val hasConcreteLine =
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          lineHasTacticalProof(payload)
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasLineProof
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.hasLineProof
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
        case _ =>
          false
      }
    val hasEngineProof =
      records.exists {
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
        case record @ EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
          engineBackedComparison(graph, assessment.primaryComparisonEvidence)
            .exists(engineComparisonProvesTactic(_, graph))
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          recordEngineBacked(record) &&
          engineComparisonProvesTactic(fact, graph)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.hasEngineOrForcingProof
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          engineBackedComparison(graph, cause.comparisonEvidence).exists(fact =>
            engineComparisonProvesTactic(fact, graph) &&
              (
              relativeCauseHasOwnedRelationTacticalProof(cause, graph) ||
                fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
                fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
            )
          )
        case _ =>
          false
      }
    hasTacticalAnchor && hasConcreteLine && hasEngineProof

  private def relativeCauseHasOwnedRelationTacticalProof(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    cause.attribution.directProofEligible &&
      cause.proof.exists(proof =>
        (
          graph.relativeCauseRelations(proof.directProof) ++
            graph.relativeCauseRelations(proof.contrastProof)
        ).exists { case (_, payload) =>
          payload.hasConcreteRelationProof && payload.hasLineProof
        }
      )

  private def defensiveProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    records.exists {
      case record @ EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.isProofSignalDefensivePressure &&
          (!branchLocalThreatPressure(record) || claimIsBranchLocal(claim))
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorDefensiveClaim
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        graph.comparisonFor(cause).nonEmpty && defensiveRelativeCauseHasProof(cause, graph)
      case _ =>
        false
    }

  private[chessjudgment] def defensiveRelativeCauseCanSupportClaim(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    defensiveRelativeCauseHasProof(cause, graph)

  private def defensiveRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    defensiveRelativeCause(cause.kind) &&
      cause.hasOwnedTypedDepth(graph)

  private def claimIsBranchLocal(claim: JudgmentClaim): Boolean =
    claim.scope == EvidenceScope.ThreatLine ||
      claim.primaryLine.exists(_.role == LineNodeRole.Threat)

  private def branchLocalThreatPressure(record: EvidenceRecord): Boolean =
    record.ref.scope == EvidenceScope.ThreatLine ||
      record.ref.line.exists(_.role == LineNodeRole.Threat)

  private def materialProof(records: List[EvidenceRecord], graph: TypedEvidenceGraph): Boolean =
    val hasMaterialCause =
      records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          graph.comparisonFor(cause).nonEmpty && materialRelativeCauseHasProof(cause, graph)
        case _ =>
          false
      }
    val hasMaterialLine =
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          materialLineProof(payload)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.kind == TacticalMechanismKind.MaterialGain && payload.canAnchorTacticalClaim
        case _ =>
          false
      }
    val hasComparison =
      records.exists {
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          recordEngineBacked(record)
        case _ =>
          false
      }
    hasMaterialCause && hasMaterialLine && hasComparison

  private def conversionProof(records: List[EvidenceRecord], graph: TypedEvidenceGraph): Boolean =
    val hasConversionCause =
      records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          graph.comparisonFor(cause).nonEmpty && conversionRelativeCauseHasProof(cause, graph)
        case _ =>
          false
      }
    val hasConversionContext = conversionContextCanSupportClaim(records)
    val hasComparison =
      records.exists {
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          recordEngineBacked(record)
        case _ =>
          false
      }
    hasConversionCause && hasConversionContext && hasComparison

  private def evaluationProof(records: List[EvidenceRecord], graph: TypedEvidenceGraph): Boolean =
    val recordIds = records.map(_.ref.id).toSet
    records.exists {
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        graph
          .candidateComparisonRecord(assessment.primaryComparisonEvidence)
          .exists(record =>
            recordIds.contains(record.ref.id) &&
              recordEngineBacked(record) &&
              graph.comparisonFor(assessment).exists(_.kind == CandidateComparisonKind.PlayedVsBest)
          )
      case _ =>
        false
    }

  private[chessjudgment] def conversionContextCanSupportClaim(records: List[EvidenceRecord]): Boolean =
    records.exists(conversionContextRecord)

  private def conversionContextRecord(record: EvidenceRecord): Boolean =
    record match
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasConversionConsequence
      case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
        payload.positionFeatures.exists(_.materialPhase.phase == "endgame")
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        structuralConversionContext(payload)
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.kind == RelationFactKind.BadPieceLiquidation && payload.hasConcreteRelationProof
      case _ =>
        false

  private def structuralConversionContext(payload: StructuralDeltaEvidence): Boolean =
    import TransitionConsequenceKind.*
    payload.hasAnyConsequence(Set(PassedPawnProgress, PromotionPressureGain))

  private def recordEngineBacked(record: EvidenceRecord): Boolean =
    record.ref.confidence == EvidenceConfidence.EngineBacked ||
      recordHasMate(record)

  private def recordHasMate(record: EvidenceRecord): Boolean =
    record.payload match
      case EvalFactEvidence(_, _, mate, _) =>
        mate.nonEmpty
      case _ =>
        false

  private def engineBackedComparison(
      graph: TypedEvidenceGraph,
      ref: EvidenceRef
  ): Option[CandidateComparisonFact] =
    graph
      .candidateComparisonRecord(ref)
      .filter(recordEngineBacked)
      .collect {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact
      }

  private def engineComparisonProvesTactic(
      fact: CandidateComparisonFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
      fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
      graph.candidateSetFor(fact).exists(_.onlyMove)

  private def tacticalRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Tactical)

  private def materialResultCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Material)

  private def materialRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    materialResultCause(cause.kind) &&
      cause.hasOwnedTypedDepth(graph)

  private def materialLineProof(payload: LineFactEvidence): Boolean =
    payload.hasMaterialConsequence ||
      payload.hasRecaptureRecoveryConsequence ||
      payload.hasMaterialRecaptureChain ||
      payload.hasMaterialRecoveryWindow ||
      payload.hasProofSignalMaterialEvent

  private def conversionRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    (
      cause.kind == RelativeCauseKind.ConversionMiss ||
        cause.kind == RelativeCauseKind.ConversionSecured ||
        cause.kind == RelativeCauseKind.RecaptureRecoveryWindow ||
        cause.kind == RelativeCauseKind.MaterialSwing
    ) &&
      cause.hasOwnedTypedDepth(graph)

  private def strategicRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.hasOwnedAdmissibleLongTermProof(graph)

  private def relativeCauseHasTacticalProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.hasOwnedTacticalProof(graph)

  private def lineHasTacticalProof(payload: LineFactEvidence): Boolean =
    payload.rootMove.exists(rootMove =>
      payload.consequencesForRootMove(rootMove).exists(consequence =>
        LineConsequenceKind.tacticalDriver(consequence.kind)
      )
    )

  private def defensiveRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Defensive)
