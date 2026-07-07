package lila.chessjudgment.analysis.policy

import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.model.{ ActivePlans, PlanScoringResult }
import lila.chessjudgment.model.judgment.*

enum ClaimTruthStatus:
  case Certified
  case Deferred
  case Rejected

final case class ClaimTruthDecision(
    claim: ClaimSeed,
    status: ClaimTruthStatus,
    presentLayers: Set[EvidenceLayer],
    missingLayerGroups: List[Set[EvidenceLayer]],
    missingEvidence: List[EvidenceRef]
):
  def certified: Boolean = status == ClaimTruthStatus.Certified

object ClaimTruthPolicy:

  def evaluate(claim: ClaimSeed, graph: TypedEvidenceGraph): ClaimTruthDecision =
    val graphIds = graph.byId.keySet
    val missingEvidence = claim.evidence.filterNot(ref => graphIds.contains(ref.id))
    val claimBoundRecords =
      claim.evidence
        .flatMap(ref => graph.byId.get(ref.id))
        .filter(isBoundToClaim(claim, _, graph))
    val claimBoundLayers =
      claimBoundRecords
        .map(_.ref.layer)
        .toSet
    val missingGroups =
      requiredLayerGroups(claim.family).filterNot(group => group.exists(claimBoundLayers.contains))
    val hasFamilyProof =
      familySpecificProof(claim, claimBoundRecords)
    val status =
      if claim.evidence.isEmpty || missingEvidence.nonEmpty then ClaimTruthStatus.Rejected
      else if claimBoundRecords.isEmpty then ClaimTruthStatus.Rejected
      else if !hasFamilyProof then ClaimTruthStatus.Deferred
      else ClaimTruthStatus.Certified
    ClaimTruthDecision(
      claim = claim,
      status = status,
      presentLayers = claimBoundLayers,
      missingLayerGroups = missingGroups,
      missingEvidence = missingEvidence
    )

  private def isBoundToClaim(
      claim: ClaimSeed,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Boolean =
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    val samePosition = record.ref.position == claim.primaryPosition
    val layerCompatible = claimLayerCompatible(record)
    val sameLine = layerCompatible && claim.primaryLine.exists(line => recordLineMatches(record, line))
    val sameSubjectMove = layerCompatible && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine))
    val comparisonLineSupport =
      !claim.family.isLongTerm &&
        comparisonProofLayer(record.ref.layer) &&
        comparisonLinesForClaim(claim, graph).exists(line => recordLineMatches(record, line))
    val transitionDestinationLocal =
      claimEvidenceIds.contains(record.ref.id) &&
        positionLocalLayer(record.ref.layer) &&
        transitionDestinationsForClaim(claim, graph).contains(record.ref.position)
    val samePositionLocal =
      samePosition &&
        claim.subject == IdeaSubject.Position &&
        claim.subjectMove.isEmpty &&
        record.ref.line.isEmpty &&
        positionLocalLayer(record.ref.layer)
    val parentLinked = record.parents.exists(parent => claimEvidenceIds.contains(parent.id))
    val childLinked =
      graph.records.exists(child =>
        claimEvidenceIds.contains(child.ref.id) &&
          child.parents.exists(parent => parent.id == record.ref.id)
      )
    val linkedBound =
      (parentLinked || childLinked) &&
        linkedRecordCompatible(claim, record, graph, samePositionLocal)
    sameLine || sameSubjectMove || comparisonLineSupport || transitionDestinationLocal || samePositionLocal || linkedBound

  private def linkedRecordCompatible(
      claim: ClaimSeed,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      samePositionLocal: Boolean
  ): Boolean =
    val supportLayer = linkedSupportLayer(record.ref.layer)
    (supportLayer && claim.primaryLine.exists(line => recordLineMatches(record, line))) ||
      (supportLayer && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine))) ||
      samePositionLocal ||
      linkedPositionContext(claim, record) ||
      linkedClaimSupport(claim, record, graph)

  private def linkedPositionContext(claim: ClaimSeed, record: EvidenceRecord): Boolean =
    claim.primaryLine.isEmpty &&
      claim.subjectMove.isEmpty &&
      record.ref.position == claim.primaryPosition &&
      positionContextLayer(claim.family, record.ref.layer)

  private def linkedClaimSupport(claim: ClaimSeed, record: EvidenceRecord, graph: TypedEvidenceGraph): Boolean =
    val comparisonLineBound =
      comparisonLinesForClaim(claim, graph).exists(line => recordLineMatches(record, line))
    claim.primaryLine.nonEmpty &&
      (record.ref.position == claim.primaryPosition || comparisonLineBound) &&
      linkedSupportLayer(record.ref.layer)

  private def linkedSupportLayer(layer: EvidenceLayer): Boolean =
    claimProofCarrierLayer(layer)

  private def claimProofCarrierLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.Claim | EvidenceLayer.ChessIdea => false
      case _                                                                   => true

  private def claimLayerCompatible(record: EvidenceRecord): Boolean =
    linkedSupportLayer(record.ref.layer) ||
      positionLocalLayer(record.ref.layer)

  private def positionContextLayer(family: ClaimFamily, layer: EvidenceLayer): Boolean =
    if family.isLongTerm then positionLocalLayer(layer)
    else
      layer match
        case EvidenceLayer.Board | EvidenceLayer.SinglePosition | EvidenceLayer.Line | EvidenceLayer.Eval =>
          true
        case _ =>
          false

  private def comparisonProofLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Line | EvidenceLayer.Eval =>
        true
      case _ =>
        false

  private def comparisonLinesForClaim(claim: ClaimSeed, graph: TypedEvidenceGraph): List[LineNodeRef] =
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    graph.records
      .filter(record => claimEvidenceIds.contains(record.ref.id))
      .flatMap(comparisonLinesForRecord)
      .distinct

  private def transitionDestinationsForClaim(claim: ClaimSeed, graph: TypedEvidenceGraph): Set[PositionNodeRef] =
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

  private def comparisonLinesForRecord(record: EvidenceRecord): List[LineNodeRef] =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case CounterfactualFactEvidence(referenceLine, candidateLine, _) =>
        List(referenceLine, candidateLine)
      case RelativeCauseFactEvidence(cause) =>
        List(cause.eventLine)
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.map(_.eventLine).distinct
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case _ =>
        Nil

  private def recordLineMatches(record: EvidenceRecord, line: LineNodeRef): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        cause.eventLine == line
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.exists(_.eventLine == line)
      case _ =>
        record.referencesLine(line)

  private def recordMentionsMove(record: EvidenceRecord, move: String, claimLine: Option[LineNodeRef]): Boolean =
    record.payload match
      case payload: MoveMotifEvidence =>
        payload.moveUci == move
      case MoveTransitionEvidence(moveUci, _, _) =>
        moveUci == move &&
          claimLine.forall(line =>
            TransitionEdgeRole.fromScope(record.ref.scope).exists(role =>
              role.lineRole == line.role && line.rootMove == move
            )
          )
      case payload: StructuralDeltaEvidence =>
        payload.moveUci == move &&
          claimLine.forall(line => payload.line.contains(line))
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.contains(move) ||
          claimLine.exists(line => payload.line.contains(line))
      case CandidateComparisonEvidence(fact) =>
        claimLine.exists(line => line == fact.referenceLine || line == fact.candidateLine)
      case CounterfactualFactEvidence(referenceLine, candidateLine, _) =>
        claimLine.exists(line => line == referenceLine || line == candidateLine)
      case RelativeCauseFactEvidence(cause) =>
        claimLine.exists(_ == cause.eventLine) || cause.eventLine.rootMove == move
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.exists(cause =>
          claimLine.exists(_ == cause.eventLine) || cause.eventLine.rootMove == move
        )
      case _ =>
        false

  private def positionLocalLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.SinglePosition | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def requiredLayerGroups(family: ClaimFamily): List[Set[EvidenceLayer]] =
    family match
      case ClaimFamily.Tactical =>
        List(
          Set(
            EvidenceLayer.TacticalMechanism,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.Relation,
            EvidenceLayer.Line,
            EvidenceLayer.MoveVerdictCertification
          ),
          Set(EvidenceLayer.Line, EvidenceLayer.Relation, EvidenceLayer.TacticalMechanism, EvidenceLayer.Eval),
          Set(
            EvidenceLayer.Eval,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.Counterfactual,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.TacticalMechanism,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.MoveVerdictCertification
          )
        )
      case ClaimFamily.Strategic =>
        List(Set(EvidenceLayer.StrategicMechanism))
      case ClaimFamily.PawnStructure =>
        List(Set(EvidenceLayer.StrategicMechanism))
      case ClaimFamily.Opening =>
        List(Set(EvidenceLayer.StrategicMechanism))
      case ClaimFamily.Plan =>
        List(Set(EvidenceLayer.StrategicMechanism))
      case ClaimFamily.Defensive =>
        List(
          Set(
            EvidenceLayer.ThreatPressure,
            EvidenceLayer.TacticalMechanism,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.MoveVerdictCertification
          ),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.Counterfactual,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.MoveVerdictCertification,
            EvidenceLayer.TacticalMechanism
          )
        )
      case ClaimFamily.Conversion =>
        List(
          Set(EvidenceLayer.RelativeCause, EvidenceLayer.MoveVerdictCertification),
          Set(EvidenceLayer.Line, EvidenceLayer.SinglePosition, EvidenceLayer.StructuralDelta, EvidenceLayer.Relation),
          Set(
            EvidenceLayer.Eval,
            EvidenceLayer.Line,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.Counterfactual,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.MoveVerdictCertification
          )
        )
      case ClaimFamily.Material =>
        List(
          Set(EvidenceLayer.RelativeCause, EvidenceLayer.MoveVerdictCertification),
          Set(EvidenceLayer.Line),
          Set(
            EvidenceLayer.Eval,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.Counterfactual,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.MoveVerdictCertification
          )
        )
      case ClaimFamily.Evaluation =>
        List(
          Set(
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.MoveVerdictCertification,
            EvidenceLayer.Counterfactual,
            EvidenceLayer.Eval,
            EvidenceLayer.CandidateComparison
          )
        )

  private def familySpecificProof(claim: ClaimSeed, records: List[EvidenceRecord]): Boolean =
    claim.family match
      case ClaimFamily.Tactical =>
        tacticalProof(records)
      case ClaimFamily.Defensive =>
        defensiveProof(claim, records)
      case ClaimFamily.Strategic =>
        strategicProof(records)
      case ClaimFamily.PawnStructure =>
        pawnStructureProof(records)
      case ClaimFamily.Opening =>
        openingProof(records)
      case ClaimFamily.Plan =>
        planProof(records)
      case ClaimFamily.Conversion =>
        conversionProof(records)
      case ClaimFamily.Material =>
        materialProof(records)
      case ClaimFamily.Evaluation =>
        evaluationProof(records)

  private def strategicProof(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        strategicRelativeCauseHasProof(cause)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(strategicRelativeCauseHasProof)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        StrategicMechanismContrastEvidence.hasActionableContrastOrSameRootCarrier(payload, records)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorStrategicIdea
      case _ =>
        false
    }

  private def pawnStructureProof(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        strategicRelativeCauseHasProof(cause)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(strategicRelativeCauseHasProof)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.actionableComparisons.exists(_.axis.kind == StrategicAxisKind.PawnBreak)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorPawnStructureIdea
      case _ =>
        false
    }

  private def openingProof(records: List[EvidenceRecord]): Boolean =
    StrategicMechanismEvidence.openingClaimSupported(records)

  private[chessjudgment] def planClaimApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.claims.exists(_.family == ClaimFamily.Plan) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          payload.canAnchorPlanIdea
        case _ =>
          false
      }

  private def planProof(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        strategicRelativeCauseHasProof(cause) &&
          (cause.kind == RelativeCauseKind.PlanImprovement || cause.kind == RelativeCauseKind.PlanContradiction)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(cause =>
          strategicRelativeCauseHasProof(cause) &&
            (cause.kind == RelativeCauseKind.PlanImprovement || cause.kind == RelativeCauseKind.PlanContradiction)
        )
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.planComparison.exists(_.hasPlanDelta)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorPlanIdea
      case _ =>
        false
    }

  private[chessjudgment] def pawnStructureCanAnchorPlan(payload: PawnStructureFactEvidence): Boolean =
    StrategicMechanismEvidence.pawnStructureCanAnchorPlan(payload)

  private def tacticalProof(records: List[EvidenceRecord]): Boolean =
    val hasTacticalAnchor =
      records.exists {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.canAnchorTacticalIdea
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasConcreteRelationProof
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          (tacticalRelativeCause(cause.kind) || materialResultCause(cause.kind)) &&
            relativeCauseHasTacticalProof(cause)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(cause =>
            (tacticalRelativeCause(cause.kind) || materialResultCause(cause.kind)) &&
              relativeCauseHasTacticalProof(cause)
          )
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
          recordEngineBacked(record) &&
          engineComparisonProvesTactic(assessment.comparison)
        case record @ EvidenceRecord(_, CounterfactualFactEvidence(_, _, comparison), _) =>
          recordEngineBacked(record) &&
          engineComparisonProvesTactic(comparison)
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          recordEngineBacked(record) &&
          engineComparisonProvesTactic(fact.comparison)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.hasEngineOrForcingProof
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          recordEngineBacked(record) &&
            (
              cause.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
                cause.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
            )
        case record @ EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          recordEngineBacked(record) &&
            certification.causes.exists(cause =>
              cause.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
                cause.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
            )
        case _ =>
          false
      }
    hasTacticalAnchor && hasConcreteLine && hasEngineProof

  private def defensiveProof(claim: ClaimSeed, records: List[EvidenceRecord]): Boolean =
    records.exists {
      case record @ EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.isProofSignalDefensivePressure &&
          (!branchLocalThreatPressure(record) || claimIsBranchLocal(claim))
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorDefensiveIdea
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        defensiveRelativeCauseHasProof(cause)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(defensiveRelativeCauseHasProof)
      case _ =>
        false
    }

  private[chessjudgment] def defensiveRelativeCauseCanSeedIdea(cause: RelativeCauseFact): Boolean =
    defensiveRelativeCauseHasProof(cause)

  private def defensiveRelativeCauseHasProof(cause: RelativeCauseFact): Boolean =
    defensiveRelativeCause(cause.kind) &&
      cause.hasOwnedTypedDepth

  private def claimIsBranchLocal(claim: ClaimSeed): Boolean =
    claim.scope == EvidenceScope.ThreatLine ||
      claim.primaryLine.exists(_.role == LineNodeRole.Threat)

  private def branchLocalThreatPressure(record: EvidenceRecord): Boolean =
    record.ref.scope == EvidenceScope.ThreatLine ||
      record.ref.line.exists(_.role == LineNodeRole.Threat)

  private def materialProof(records: List[EvidenceRecord]): Boolean =
    val hasMaterialCause =
      records.exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          materialRelativeCauseHasProof(cause)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(materialRelativeCauseHasProof)
        case _ =>
          false
      }
    val hasMaterialLine =
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          materialLineProof(payload)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.kind == TacticalMechanismKind.MaterialGain && payload.canAnchorTacticalIdea
        case _ =>
          false
      }
    val hasComparison =
      records.exists {
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
        case record @ EvidenceRecord(_, RelativeAssessmentEvidence(_), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, CounterfactualFactEvidence(_, _, _), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, MoveVerdictCertificationEvidence(_), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) =>
          recordEngineBacked(record)
        case _ =>
          false
      }
    hasMaterialCause && hasMaterialLine && hasComparison

  private def conversionProof(records: List[EvidenceRecord]): Boolean =
    val hasConversionCause =
      records.exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          conversionRelativeCauseHasProof(cause)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(conversionRelativeCauseHasProof)
        case _ =>
          false
      }
    val hasConversionContext = conversionContextCanSeedIdea(records)
    val hasComparison =
      records.exists {
        case record @ EvidenceRecord(_, RelativeAssessmentEvidence(_), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, CounterfactualFactEvidence(_, _, _), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, MoveVerdictCertificationEvidence(_), _) =>
          recordEngineBacked(record)
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) =>
          recordEngineBacked(record)
        case _ =>
          false
      }
    hasConversionCause && hasConversionContext && hasComparison

  private def evaluationProof(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case record @ EvidenceRecord(_, EvalFactEvidence(_, _, _, _), _) =>
        recordEngineBacked(record)
      case record @ EvidenceRecord(_, RelativeAssessmentEvidence(_), _) =>
        recordEngineBacked(record)
      case record @ EvidenceRecord(_, CounterfactualFactEvidence(_, _, _), _) =>
        recordEngineBacked(record)
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
        recordEngineBacked(record)
      case record @ EvidenceRecord(_, MoveVerdictCertificationEvidence(_), _) =>
        recordEngineBacked(record)
      case _ =>
        false
    }

  private[chessjudgment] def conversionContextCanSeedIdea(records: List[EvidenceRecord]): Boolean =
    records.exists(conversionContextRecord)

  private def conversionContextRecord(record: EvidenceRecord): Boolean =
    record match
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasConversionConsequence
      case EvidenceRecord(_, SinglePositionEvidence(assessment), _) =>
        assessment.simplifyBias.shouldSimplify || assessment.gamePhase.isEndgame
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        structuralConversionContext(payload)
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.kind == RelationFactKind.BadPieceLiquidation && payload.hasConcreteRelationProof
      case _ =>
        false

  private def structuralConversionContext(payload: StructuralDeltaEvidence): Boolean =
    import TransitionConsequenceKind.*
    payload.hasAnyConsequence(Set(PassedPawnProgress, PromotionPressureGain))

  private[chessjudgment] def planPressureHasDirectEvidence(scoring: PlanScoringResult, activePlans: ActivePlans): Boolean =
    StrategicMechanismEvidence.planPressureHasDirectEvidence(scoring, activePlans)

  private def recordEngineBacked(record: EvidenceRecord): Boolean =
    record.ref.confidence == EvidenceConfidence.EngineBacked ||
      recordHasMate(record)

  private def recordHasMate(record: EvidenceRecord): Boolean =
    record.payload match
      case EvalFactEvidence(_, _, mate, _) =>
        mate.nonEmpty
      case _ =>
        false

  private def engineComparisonProvesTactic(comparison: EvalComparison): Boolean =
    comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
      comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
      comparison.candidateSet.exists(_.onlyMove)

  private def tacticalRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.TacticalEvent)

  private def materialResultCause(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.MaterialEvent)

  private def materialRelativeCauseHasProof(cause: RelativeCauseFact): Boolean =
    materialResultCause(cause.kind) &&
      cause.hasOwnedTypedDepth

  private def materialLineProof(payload: LineFactEvidence): Boolean =
    payload.hasMaterialConsequence ||
      payload.hasRecaptureRecoveryConsequence ||
      payload.hasMaterialRecaptureChain ||
      payload.hasMaterialRecoveryWindow ||
      payload.hasProofSignalMaterialEvent

  private def conversionRelativeCauseHasProof(cause: RelativeCauseFact): Boolean =
    (
      cause.kind == RelativeCauseKind.ConversionMiss ||
        cause.kind == RelativeCauseKind.ConversionSecured ||
        cause.kind == RelativeCauseKind.RecaptureRecoveryWindow ||
        cause.kind == RelativeCauseKind.MaterialSwing
    ) &&
      cause.hasOwnedTypedDepth

  private def strategicRelativeCauseHasProof(cause: RelativeCauseFact): Boolean =
    cause.hasOwnedAdmissibleLongTermProof

  private def relativeCauseHasTacticalProof(cause: RelativeCauseFact): Boolean =
    cause.hasOwnedTacticalProof

  private def lineHasTacticalProof(payload: LineFactEvidence): Boolean =
    payload.rootMove.exists(rootMove =>
      payload.rootOwnedProofSignalConsequences(rootMove).exists(consequence =>
        LineConsequenceKind.tacticalDriver(consequence.kind)
      )
    )

  private def defensiveRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.DefensiveEvent)
