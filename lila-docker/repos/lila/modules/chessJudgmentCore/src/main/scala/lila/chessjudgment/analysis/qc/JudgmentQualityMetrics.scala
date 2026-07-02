package lila.chessjudgment.analysis.qc

import chess.{ Color, File }
import lila.chessjudgment.analysis.assembly.{
  JudgmentPacketValidationIssueKind,
  JudgmentPacketValidationResult,
  JudgmentPacketValidator,
  RelativeCauseDraftPlanner,
  RelativeCauseSignalProfile
}
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.policy.ClaimTruthPolicy
import lila.chessjudgment.analysis.singlePosition.{ PawnPlayDriver, ThreatDriver, ThreatSeverity }
import lila.chessjudgment.analysis.tactical.TacticalMotifClassifier
import lila.chessjudgment.model.{ Motif, ProbeAdmissionStatus, ProbePurpose, TransitionType }
import lila.chessjudgment.model.structure.AlignmentBand
import lila.chessjudgment.model.judgment.*

enum EvidenceLossExpectation:
  case Expected
  case Secondary
  case Deferred
  case TruthRejected
  case TruthDeferred
  case DedupeDropped
  case ArbitrationSuppressed
  case Unexpected

final case class EvidenceLossClassification(
    diagnostic: EvidenceLossDiagnostic,
    expectation: EvidenceLossExpectation
)

object ExpectedEvidenceLossPolicy:
  private val supportOnlyLayers: Set[EvidenceLayer] =
    Set(
      EvidenceLayer.Board,
      EvidenceLayer.SinglePosition,
      EvidenceLayer.Line,
      EvidenceLayer.Eval,
      EvidenceLayer.MoveMotif,
      EvidenceLayer.MoveTransition,
      EvidenceLayer.Relation,
      EvidenceLayer.ChessIdea,
      EvidenceLayer.Claim
    )

  def classify(report: EvidenceLossReport): List[EvidenceLossClassification] =
    classify(report, None)

  def classify(report: EvidenceLossReport, packet: EvidenceBackedJudgmentPacket): List[EvidenceLossClassification] =
    classify(report, Some(packet))

  private def classify(
      report: EvidenceLossReport,
      packet: Option[EvidenceBackedJudgmentPacket]
  ): List[EvidenceLossClassification] =
    report.diagnostics.map { diagnostic =>
      EvidenceLossClassification(
        diagnostic = diagnostic,
        expectation = expectationFor(diagnostic, packet)
      )
    }

  private def expectationFor(
      diagnostic: EvidenceLossDiagnostic,
      packet: Option[EvidenceBackedJudgmentPacket]
  ): EvidenceLossExpectation =
    diagnostic.reason match
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if diagnostic.layer.exists(supportOnlyLayers.contains) =>
        EvidenceLossExpectation.Expected
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if packet.exists(packet => isNonProofSignalThreatSupport(diagnostic, packet)) =>
        EvidenceLossExpectation.Expected
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if packet.exists(packet => isStrategicSourceConsumedByMechanism(diagnostic, packet)) =>
        EvidenceLossExpectation.Expected
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if packet.exists(packet => isSupportOnlyStrategicMechanism(diagnostic, packet)) =>
        EvidenceLossExpectation.Expected
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if packet.exists(packet => isExpectedCandidateComparisonSupport(diagnostic, packet)) =>
        EvidenceLossExpectation.Expected
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if packet.exists(packet => isSecondaryCandidateComparisonSupport(diagnostic, packet)) =>
        EvidenceLossExpectation.Secondary
      case EvidenceLossReason.EvidenceAvailableWithoutIdea
          if packet.exists(packet => isDeferredCandidateComparisonSupport(diagnostic, packet)) =>
        EvidenceLossExpectation.Deferred
      case EvidenceLossReason.IdeaAvailableWithoutClaim
          if packet.flatMap(packet => ideaWithoutCertifiedClaimExpectation(diagnostic, packet)).nonEmpty =>
        packet.flatMap(packet => ideaWithoutCertifiedClaimExpectation(diagnostic, packet)).get
      case _ =>
        EvidenceLossExpectation.Unexpected

  private def isNonProofSignalThreatSupport(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Boolean =
    diagnostic.layer.contains(EvidenceLayer.ThreatPressure) &&
      diagnostic.evidence
        .flatMap(ref => packet.evidenceGraph.byId.get(ref.id))
        .exists {
          case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
            !payload.isProofSignalDefensivePressure
          case EvidenceRecord(_, ThreatPressureEvidence(_, threats), _) =>
            !threats.isProofSignalDefensivePressure
          case _ =>
            false
        }

  private def isStrategicSourceConsumedByMechanism(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Boolean =
    diagnostic.evidence.exists(ref =>
      strategicMechanismSourceLayer(ref.layer) &&
        packet.evidenceGraph.records.exists {
          case EvidenceRecord(_, _: StrategicMechanismEvidence, parents) =>
            parents.exists(_.id == ref.id) ||
              parents.exists(parent => evidenceHasAncestor(packet.evidenceGraph, parent, ref.id))
          case _ =>
            false
        }
    )

  private def evidenceHasAncestor(
      graph: TypedEvidenceGraph,
      ref: EvidenceRef,
      ancestorId: String
  ): Boolean =
    def loop(next: List[EvidenceRef], seen: Set[String]): Boolean =
      next.exists { current =>
        current.id == ancestorId ||
          (!seen.contains(current.id) &&
            graph.byId.get(current.id).exists(record => loop(record.parents, seen + current.id)))
      }
    loop(List(ref), Set.empty)

  private def strategicMechanismSourceLayer(layer: EvidenceLayer): Boolean =
    layer match
      case layer if StrategicMechanismEvidence.rawStrategicSourceLayer(layer) =>
        true
      case _ =>
        false

  private def isSupportOnlyStrategicMechanism(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Boolean =
    diagnostic.layer.contains(EvidenceLayer.StrategicMechanism) &&
      diagnostic.evidence
        .flatMap(ref => packet.evidenceGraph.byId.get(ref.id))
        .exists {
          case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
            !strategicMechanismCanSeedJudgment(payload)
          case _ =>
            false
        }

  private def strategicMechanismCanSeedJudgment(payload: StrategicMechanismEvidence): Boolean =
    payload.canAnchorStrategicIdea ||
      payload.canAnchorPawnStructureIdea ||
      payload.canAnchorOpeningIdea ||
      payload.canAnchorPlanIdea ||
      payload.canSupportCompensation

  private def isExpectedCandidateComparisonSupport(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Boolean =
    candidateComparisonFor(diagnostic, packet).exists(fact =>
      fact.kind != CandidateComparisonKind.PlayedVsBest &&
        !comparisonRequiresExplanatoryCause(fact)
    )

  private def isDeferredCandidateComparisonSupport(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Boolean =
    candidateComparisonFor(diagnostic, packet).exists(fact =>
      fact.kind != CandidateComparisonKind.PlayedVsBest &&
        !comparisonTouchesPlayedLine(packet, fact) &&
        comparisonRequiresExplanatoryCause(fact)
    )

  private def isSecondaryCandidateComparisonSupport(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Boolean =
    candidateComparisonFor(diagnostic, packet).exists(fact =>
      fact.kind != CandidateComparisonKind.PlayedVsBest &&
        comparisonTouchesPlayedLine(packet, fact) &&
        comparisonRequiresExplanatoryCause(fact)
    )

  private def candidateComparisonFor(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Option[CandidateComparisonFact] =
    diagnostic.evidence
      .filter(_.layer == EvidenceLayer.CandidateComparison)
      .flatMap(ref => packet.evidenceGraph.byId.get(ref.id))
      .collect { case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact }

  private def comparisonTouchesPlayedLine(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact
  ): Boolean =
    val playedLines =
      packet.candidateLines.filter(_.role == LineNodeRole.Played).map(_.ref).toSet
    val playedMoves =
      playedLines.map(line => normalizeMove(line.rootMove)) ++
        packet.playedTransition.map(transition => normalizeMove(transition.moveUci)).toSet
    playedLines.contains(fact.referenceLine) ||
      playedLines.contains(fact.candidateLine) ||
      playedMoves.contains(normalizeMove(fact.referenceLine.rootMove)) ||
      playedMoves.contains(normalizeMove(fact.candidateLine.rootMove))

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def comparisonRequiresExplanatoryCause(fact: CandidateComparisonFact): Boolean =
    comparisonSignificantEnginePreference(fact) && !comparisonPositiveContextAlternative(fact)

  private def comparisonSignificantEnginePreference(fact: CandidateComparisonFact): Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
      fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
      fact.comparison.candidateSet.exists(_.onlyMove)

  private def comparisonPositiveContextAlternative(fact: CandidateComparisonFact): Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsAlternative &&
      fact.comparison.winPercentLossForMover < JudgmentThresholds.INACCURACY_WP &&
      fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP

  private def ideaWithoutCertifiedClaimExpectation(
      diagnostic: EvidenceLossDiagnostic,
      packet: EvidenceBackedJudgmentPacket
  ): Option[EvidenceLossExpectation] =
    val matchingLifecycle =
      packet.claimLifecycle.filter(_.ideaIds.contains(diagnostic.subjectId))
    val stageExpectation =
      List(
        ClaimLifecycleStage.TruthRejected -> EvidenceLossExpectation.TruthRejected,
        ClaimLifecycleStage.TruthDeferred -> EvidenceLossExpectation.TruthDeferred,
        ClaimLifecycleStage.DedupeDropped -> EvidenceLossExpectation.DedupeDropped,
        ClaimLifecycleStage.ArbitrationSuppressed -> EvidenceLossExpectation.ArbitrationSuppressed
      ).collectFirst {
        case (stage, expectation) if matchingLifecycle.exists(_.stages.contains(stage)) =>
          expectation
      }
    stageExpectation.orElse(
      Option.when(
        packet.claims.exists(claim =>
        claim.idea.exists(_.id == diagnostic.subjectId) &&
          claim.supportStatus.exists(_.status == ClaimSupportStatus.Deferred)
        )
      )(EvidenceLossExpectation.TruthDeferred)
    )

final case class GraphLossMetrics(
    totalRegistered: Int,
    promotedToIdea: Int,
    promotedToClaim: Int,
    unpromoted: Int,
    secondaryLoss: Int,
    deferredLoss: Int,
    unexpectedLoss: Int,
    coverage: List[EvidenceLayerCoverage]
)

object GraphLossMetrics:
  def from(report: EvidenceLossReport, classifications: List[EvidenceLossClassification]): GraphLossMetrics =
    val totalRegistered = report.coverage.map(_.registered).sum
    val promotedToIdea = report.coverage.map(_.promotedToIdea).sum
    val promotedToClaim = report.coverage.map(_.promotedToClaim).sum
    val unpromoted =
      classifications.count(_.diagnostic.reason == EvidenceLossReason.EvidenceAvailableWithoutIdea)
    val unexpectedLoss =
      classifications.count(_.expectation == EvidenceLossExpectation.Unexpected)
    val deferredLoss =
      classifications.count(classification => deferredExpectation(classification.expectation))
    val secondaryLoss =
      classifications.count(_.expectation == EvidenceLossExpectation.Secondary)
    GraphLossMetrics(
      totalRegistered = totalRegistered,
      promotedToIdea = promotedToIdea,
      promotedToClaim = promotedToClaim,
      unpromoted = unpromoted,
      secondaryLoss = secondaryLoss,
      deferredLoss = deferredLoss,
      unexpectedLoss = unexpectedLoss,
      coverage = report.coverage
    )

  private def deferredExpectation(expectation: EvidenceLossExpectation): Boolean =
    expectation match
      case EvidenceLossExpectation.Deferred | EvidenceLossExpectation.TruthRejected |
          EvidenceLossExpectation.TruthDeferred | EvidenceLossExpectation.DedupeDropped |
          EvidenceLossExpectation.ArbitrationSuppressed =>
        true
      case _ =>
        false

final case class ClaimPromotionMetrics(
    ideas: Int,
    claims: Int,
    claimsWithEngineComparison: Int,
    claimPromotionRate: Double
)

object ClaimPromotionMetrics:
  def from(packet: EvidenceBackedJudgmentPacket): ClaimPromotionMetrics =
    val ideas = packet.ideas.size
    val claims = packet.claims.size
    ClaimPromotionMetrics(
      ideas = ideas,
      claims = claims,
      claimsWithEngineComparison = packet.claims.count(_.engineComparison.nonEmpty),
      claimPromotionRate = if ideas == 0 then 0d else claims.toDouble / ideas.toDouble
    )

enum CandidateComparisonFailureClass:
  case NoFailure
  case LowSignalEnginePreference
  case SecondaryContextGap
  case MissingEvidence
  case UnboundEvidence
  case LowDepthCause
  case GenericCause
  case FailedCauseTemplate
  case UnknownChessPattern

enum CandidateComparisonFailureReason:
  case LowSignalEnginePreference
  case MissingLineEvidence
  case MissingEvalEvidence
  case NoCauseGenerated
  case GeneratedOnlyUnexplained
  case TacticalEvidenceUnbound
  case DefensiveEvidenceUnbound
  case ConversionEvidenceUnbound
  case StrategicEvidenceUnbound
  case LatentTacticalEvidence
  case LatentMaterialEvidence
  case LatentStrategicEvidence
  case TacticalEvidenceBelowThreshold
  case MaterialEvidenceBelowThreshold
  case StrategicEvidenceBelowThreshold
  case TacticalSignalNeedsWidthDepth
  case StrategicContextBelowSignalFloor
  case StrategicEvidenceBelowCauseThreshold
  case PrimaryStrategicNearThresholdUnderbinding
  case PrimaryStrategicSignalNeedsWidthDepth
  case ContextAlternativeStrategicNearThreshold
  case LowSignalTacticalContext
  case LowSignalMaterialContext
  case LowSignalStrategicContext
  case LowDepthGeneratedCause
  case GenericGeneratedCause
  case UnboundEvidenceWithGeneratedCause
  case ContextCauseProjectionMissing
  case PlayableLossPrimaryOverclaim
  case ContextAlternativeComparison
  case NoSpecificEvidence
  case UnknownPattern

enum CandidateComparisonSignificanceReason:
  case PlayedLoss
  case CandidateImproves
  case OnlyMove

enum CandidateComparisonLowSignalReason:
  case QuietNoDirection
  case BelowSignalFloor
  case BelowBindingThreshold
  case NearThresholdPrimaryPlayed
  case NearThresholdContextAlternative
  case ContextAlternative

enum CandidateComparisonDedupeClass:
  case Unique
  case DuplicateEpisode

enum TacticalLossStage:
  case LineTacticalSignal
  case MoveMotifEvent
  case RelationWitness
  case ThreatEpisode
  case TacticalMechanism
  case RelativeCause
  case TacticalIdea
  case TacticalClaim

final case class TacticalLossStageDiagnostic(
    stage: TacticalLossStage,
    present: Boolean,
    evidenceIds: List[String],
    detailKinds: List[String]
)

final case class TacticalLossTrace(
    applicable: Boolean,
    signalWinPercent: Double,
    blockedAt: Option[TacticalLossStage],
    lastPresentStage: Option[TacticalLossStage],
    stages: List[TacticalLossStageDiagnostic]
)

enum LocalConcreteClaimKind:
  case DefensiveThreat
  case TacticalLine
  case TacticalEngineContext
  case ConversionContext
  case MaterialContext
  case ConcreteContext

final case class LocalThreatEpisodeDiagnostic(
    episodeId: String,
    sourceThreatIndex: Int,
    sideUnderPressure: String,
    kind: String,
    severity: ThreatSeverity,
    driver: ThreatDriver,
    defenseRequired: Boolean,
    prophylaxisNeeded: Boolean,
    onlyDefense: Option[String],
    maxWinPercentLossIfIgnored: Option[Double],
    insufficientData: Boolean
)

final case class LocalConcreteClaimDiagnostic(
    id: String,
    family: ClaimFamily,
    subject: IdeaSubject,
    primaryLine: Option[LineNodeRef],
    subjectMove: Option[String],
    scope: EvidenceScope,
    localKind: LocalConcreteClaimKind,
    evidenceLayers: Set[EvidenceLayer],
    evidenceIds: List[String],
    engineVerdict: Option[MoveChoiceVerdict],
    engineWinPercentLossForMover: Option[Double],
    threatSeverity: Option[ThreatSeverity],
    threatCount: Option[Int],
    threatDefenseRequired: Option[Boolean],
    threatProphylaxisNeeded: Option[Boolean],
    threatOnlyDefense: Option[String],
    threatMaxWinPercentLossIfIgnored: Option[Double],
    threatPrimaryDriver: Option[ThreatDriver],
    threatInsufficientData: Option[Boolean],
    threatEpisodes: List[LocalThreatEpisodeDiagnostic],
    nearbyEventClusterIds: List[String]
)

final case class ContextTacticalOutrankingDiagnostic(
    contextClaimId: String,
    contextRank: Int,
    contextSubjectBinding: SubjectBindingClass,
    contextTier: PlayerFacingClaimTier,
    contextSalienceScore: Option[Int],
    contextComparisonIds: List[String],
    contextComparisonKinds: List[CandidateComparisonKind],
    contextCauseRoles: List[RelativeCauseRole],
    contextCauseSourceSides: List[RelativeCauseSourceSide],
    contextCauseImportances: List[RelativeCauseImportance],
    contextCauseEventLines: List[LineNodeRef],
    contextEventClusterIds: List[String],
    outrankedPrimaryClaimIds: List[String],
    outrankedPrimaryRanks: List[Int],
    outrankedPrimaryFamilies: List[ClaimFamily],
    outrankedPrimarySalienceScores: List[Int],
    outrankedPrimaryEventClusterIds: List[String],
    salienceDeltaToBestPrimary: Option[Int]
)

final case class CandidateLineDiagnostic(
    id: String,
    rootMove: String,
    role: LineNodeRole,
    rank: Int,
    moves: List[String],
    whitePovEvalCp: Option[Int],
    mate: Option[Int],
    depth: Option[Int]
)

final case class EvidenceLayerNeighborhood(
    directLayers: Set[EvidenceLayer],
    directLayerCounts: Map[EvidenceLayer, Int],
    parentLayerCounts: Map[EvidenceLayer, Int]
)

final case class ComparisonEvidenceLayerDiagnostics(
    reference: EvidenceLayerNeighborhood,
    candidate: EvidenceLayerNeighborhood
)

final case class SemanticAxisDiagnostics(
    referenceAxisStrengths: Map[String, Int],
    candidateAxisStrengths: Map[String, Int],
    referenceAxisSourceIds: Map[String, List[String]],
    candidateAxisSourceIds: Map[String, List[String]],
    referenceAxisSourceLayers: Map[String, List[EvidenceLayer]],
    candidateAxisSourceLayers: Map[String, List[EvidenceLayer]],
    referenceAxes: List[String],
    candidateAxes: List[String],
    sharedAxes: List[String],
    referenceOnlyAxes: List[String],
    candidateOnlyAxes: List[String],
    referenceLeadAxes: List[String],
    candidateLeadAxes: List[String]
):
  val referenceLeadAxisCount: Int = referenceLeadAxes.size
  val candidateLeadAxisCount: Int = candidateLeadAxes.size
  val semanticDeltaAxisCount: Int = (referenceLeadAxes ++ candidateLeadAxes).distinct.size

private final case class CandidateComparisonEvidenceNeighborhood(
    referenceRecords: List[EvidenceRecord],
    candidateRecords: List[EvidenceRecord],
    sharedRecords: List[EvidenceRecord]
)

private final case class SemanticAxisEntry(
    key: String,
    strength: Int,
    source: EvidenceRef
)

private final case class SemanticAxisProfile(
    strengths: Map[String, Int],
    sourceIds: Map[String, List[String]],
    sourceLayers: Map[String, List[EvidenceLayer]]
)

final case class CandidateCauseDecisionTrace(
    badLoss: Boolean,
    tacticalLoss: Boolean,
    majorLoss: Boolean,
    candidateBetter: Boolean,
    requiresExplanatoryCause: Boolean,
    positiveContextAlternative: Boolean,
    referenceTacticalMechanismKinds: List[TacticalMechanismKind],
    candidateTacticalMechanismKinds: List[TacticalMechanismKind],
    referenceConcreteLine: Boolean,
    candidateConcreteLine: Boolean,
    candidateTacticalRefutationBridge: Boolean,
    referenceTacticalRisk: Boolean,
    hasOnlyDefense: Boolean,
    hasThreatResource: Boolean,
    referenceProphylacticResource: Boolean,
    referenceKingStepResource: Boolean,
    candidateKingStepResource: Boolean,
    referenceCastlingResource: Boolean,
    candidateCastlingResource: Boolean,
    referencePreventivePawnResource: Boolean,
    candidatePreventivePawnResource: Boolean,
    hasConversionWindow: Boolean,
    referenceConversionWindow: Boolean,
    candidateConversionWindow: Boolean,
    referenceStructuralTargetRelease: Boolean,
    referenceReleasedTargets: List[String],
    candidateReleasedTargets: List[String],
    referenceCreatedTargets: List[String],
    candidateCreatedTargets: List[String],
    referenceStructuralImprovement: Boolean,
    candidateStructuralImprovement: Boolean,
    candidatePawnStructureImprovement: Boolean,
    candidateTargetPressureGain: Boolean,
    candidateCenterControlGain: Boolean,
    candidateDevelopmentActivation: Boolean,
    candidatePieceActivityGain: Boolean,
    sameDestinationCaptureChoice: Boolean,
    referenceStructuralSignals: List[String],
    candidateStructuralSignals: List[String],
    referenceStructuralConsequences: List[TransitionConsequenceKind],
    candidateStructuralConsequences: List[TransitionConsequenceKind],
    candidatePawnStructureSignals: List[String],
    referenceSemanticAxisCount: Int,
    candidateSemanticAxisCount: Int,
    referenceSemanticAxisLead: Boolean,
    semanticAxisDiagnostics: SemanticAxisDiagnostics,
    candidatePlanEvidence: Boolean,
    candidateStrategicEvidence: Boolean,
    candidateStrategicConcessionEvidence: Boolean,
    referencePassedPawnResource: Boolean,
    candidatePassedPawnResource: Boolean,
    referenceEndgameResource: Boolean,
    candidateEndgameResource: Boolean,
    referenceLooseMaterialExploit: Boolean,
    candidateLooseMaterialExploit: Boolean,
    materialSwingEvidence: Boolean,
    referenceMaterialNetCp: Int,
    candidateMaterialNetCp: Int,
    referenceMaterialMaxGainCp: Int,
    candidateMaterialMaxGainCp: Int,
    referenceMaterialPromotionGainCp: Int,
    candidateMaterialPromotionGainCp: Int,
    referenceMaterialRecapture: Boolean,
    candidateMaterialRecapture: Boolean,
    referenceMaterialRecovery: Boolean,
    candidateMaterialRecovery: Boolean,
    referenceMaterialComplete: Boolean,
    candidateMaterialComplete: Boolean,
    referenceRelationKinds: List[RelationFactKind],
    candidateRelationKinds: List[RelationFactKind],
    relationKinds: List[RelationFactKind],
    referenceMotifs: List[String],
    candidateMotifs: List[String]
)

final case class RelativeCauseFlowDiagnostic(
    causeId: String,
    causeKind: RelativeCauseKind,
    causeRole: RelativeCauseRole,
    causeComparisonKind: CandidateComparisonKind,
    causeSourceSide: RelativeCauseSourceSide,
    causeEventLine: LineNodeRef,
    proofStrategicAxisKeys: List[String],
    familyMismatchKinds: Set[ClaimFamilyMismatchKind],
    expectedIdeaFamilies: Set[ChessIdeaFamily],
    actualIdeaFamilies: Set[ChessIdeaFamily],
    expectedClaimFamilies: Set[ClaimFamily],
    claimCandidateFamilies: Set[ClaimFamily],
    finalClaimFamilies: Set[ClaimFamily],
    directProofSourceIds: List[String],
    contrastProofSourceIds: List[String],
    contextSupportSourceIds: List[String],
    directProofKinds: List[String],
    contrastProofKinds: List[String],
    contextSupportKinds: List[String],
    hasOwnedTypedDepth: Boolean,
    hasOwnedAdmissibleLongTermProof: Boolean,
    eventClusterExpected: Boolean,
    ideaIds: List[String],
    claimCandidateIds: List[String],
    claimCandidateStages: Set[ClaimLifecycleStage],
    claimCandidateTruthStatuses: Set[ClaimLifecycleTruthStatus],
    claimCandidateDroppedStages: Set[ClaimLifecycleStage],
    claimIds: List[String],
    eventClusterIds: List[String],
    eventClusterMissingSupportEvidenceIds: List[String],
    causeWithoutIdea: Boolean,
    ideaWithoutClaimCandidate: Boolean,
    ideaWithoutFinalClaim: Boolean,
    claimWithoutEventCluster: Boolean,
    strategicCauseWithoutContrast: Boolean,
    contextSupportUsedAsDirectProof: Boolean,
    contextOnlyAttribution: Boolean,
    unattributedCause: Boolean,
    rootMismatchedAttribution: Boolean,
    supportPromotedToDirectProof: Boolean,
    objectBindingSignatures: List[String],
    relativeCauseWithoutObjectSignature: Boolean,
    objectLostBetweenEvidenceAndCause: Boolean,
    objectLostBetweenCauseAndClaim: Boolean
)

enum ClaimFamilyMismatchKind:
  case ExpectedTacticalIdeaActualOther
  case ExpectedStrategicIdeaActualOther
  case ExpectedPawnStructureIdeaActualOther
  case ExpectedOpeningIdeaActualOther
  case ExpectedDefensiveIdeaActualOther
  case ExpectedMaterialIdeaActualOther
  case ExpectedConversionIdeaActualOther
  case TacticalIdeaClaimFamilyMismatch
  case StrategicSupportUsedAsTacticalProof
  case TacticalEvidenceAbsorbedByLongTermClaim
  case MaterialConversionCauseOnlyEvaluationClaim
  case ExpectedIdeaFamilyMissing
  case ExpectedClaimFamilyMissing

final case class RelativeCauseFamilyMismatchDiagnostic(
    causeId: String,
    comparisonIds: List[String],
    causeKind: RelativeCauseKind,
    causeRole: RelativeCauseRole,
    causeComparisonKind: CandidateComparisonKind,
    causeSourceSide: RelativeCauseSourceSide,
    causeEventLine: LineNodeRef,
    expectedIdeaFamilies: Set[ChessIdeaFamily],
    actualIdeaFamilies: Set[ChessIdeaFamily],
    expectedClaimFamilies: Set[ClaimFamily],
    claimCandidateFamilies: Set[ClaimFamily],
    finalClaimFamilies: Set[ClaimFamily],
    ideaIds: List[String],
    claimCandidateIds: List[String],
    finalClaimIds: List[String],
    lifecycleStages: Set[ClaimLifecycleStage],
    lifecycleTruthStatuses: Set[ClaimLifecycleTruthStatus],
    supportLayers: Set[EvidenceLayer],
    directProofLayers: Set[EvidenceLayer],
    contrastProofLayers: Set[EvidenceLayer],
    contextSupportLayers: Set[EvidenceLayer],
    directProofSourceIds: List[String],
    contrastProofSourceIds: List[String],
    contextSupportSourceIds: List[String],
    directProofKinds: List[String],
    contrastProofKinds: List[String],
    contextSupportKinds: List[String],
    proofSourceIds: List[String],
    mismatchKinds: Set[ClaimFamilyMismatchKind]
)

final case class ComparisonRelativeCauseDiagnostics(
    expectedCauseHints: List[RelativeCauseKind],
    missingExpectedCauseHints: List[RelativeCauseKind],
    producedCauseIds: List[String],
    producedCauseKinds: List[RelativeCauseKind],
    producedCauseRoles: List[RelativeCauseRole],
    producedCauseSourceSides: List[RelativeCauseSourceSide],
    producedCauseEventLines: List[LineNodeRef],
    missingCause: Boolean,
    shallowProofCauseIds: List[String],
    genericCauseIds: List[String],
    ownedTypedDepthCauseIds: List[String],
    nonGenericCauseIds: List[String],
    unboundEvidenceIds: List[String],
    wrongRoleCauseIds: List[String],
    wrongSourceSideCauseIds: List[String],
    wrongEventLineCauseIds: List[String],
    wrongImportanceCauseIds: List[String],
    causeWithoutIdeaIds: List[String],
    ideaWithoutClaimCandidateCauseIds: List[String],
    ideaWithoutFinalClaimCauseIds: List[String],
    ideaWithoutClaimCauseIds: List[String],
    claimWithoutEventClusterCauseIds: List[String],
    eventClusterSupportMissingCauseIds: List[String],
    strategicCauseWithoutContrastIds: List[String],
    strategicClaimWithoutComparativeCauseIds: List[String],
    genericStructuralImprovementWithoutStrategicContrastIds: List[String],
    contextSupportUsedAsDirectProofIds: List[String],
    contextOnlyCauseIds: List[String],
    unattributedCauseIds: List[String],
    rootMismatchedCauseIds: List[String],
    supportPromotedToDirectProofCauseIds: List[String],
    relativeCauseWithoutObjectSignatureIds: List[String],
    objectLostBetweenEvidenceAndCauseIds: List[String],
    objectLostBetweenCauseAndClaimIds: List[String],
    causeFlow: List[RelativeCauseFlowDiagnostic]
)

final case class ComparisonMoveJudgmentViewDiagnostics(
    primaryCauseKinds: List[RelativeCauseKind],
    secondaryCauseKinds: List[RelativeCauseKind],
    contextCauseKinds: List[RelativeCauseKind],
    primaryCauseEvidenceIds: List[String],
    primaryIdeaFamilies: Set[ChessIdeaFamily],
    primaryClaimCandidateFamilies: Set[ClaimFamily],
    primaryFinalClaimFamilies: Set[ClaimFamily],
    primaryFramedCauseKinds: List[RelativeCauseKind],
    primaryUnframedCauseKinds: List[RelativeCauseKind],
    primaryRootCauseKinds: List[RelativeCauseKind],
    primaryTacticalWitnessCauseKinds: List[RelativeCauseKind],
    primaryPunishmentWitnessCauseKinds: List[RelativeCauseKind],
    primaryContextualTacticalWitnessCauseKinds: List[RelativeCauseKind],
    primaryRootCauseEvidenceIds: List[String],
    rootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
    primaryRootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
    primaryRootCauseEvidenceIdTierSignatures: List[String] = Nil,
    primaryTacticalWitnessCauseEvidenceIds: List[String],
    primaryPunishmentWitnessCauseEvidenceIds: List[String],
    primaryContextualTacticalWitnessCauseEvidenceIds: List[String],
    secondaryCauseEvidenceIds: List[String],
    contextCauseEvidenceIds: List[String],
    projectedContextCauseNoViewIds: List[String],
    playableLossPrimaryCauseEvidenceIds: List[String],
    objectlessPrimaryCauseEvidenceIds: List[String],
    objectlessSecondaryCauseEvidenceIds: List[String],
    objectlessContextCauseEvidenceIds: List[String],
    positionPlanTechniqueFrameIds: List[String] = Nil,
    positionPlanTechniqueUnits: List[PositionPlanTechniqueUnit] = Nil,
    positionPlanTechniqueAxisKeys: List[String] = Nil,
    positionPlanTechniqueSemanticDetailUnits: List[PositionPlanTechniqueUnit] = Nil,
    positionPlanTechniqueSemanticDetailAxisKeys: List[String] = Nil,
    positionPlanTechniqueSemanticDetailMechanismKinds: List[StrategicMechanismKind] = Nil,
    positionPlanTechniqueSemanticDetailAnchorKeys: List[String] = Nil,
    positionPlanTechniqueSemanticDetailTokens: List[String] = Nil,
    positionPlanTechniqueSemanticDetailTokenGroups: List[List[String]] = Nil,
    positionPlanTechniqueObjectBindingSignatures: List[String] = Nil,
    positionPlanTechniqueEvidenceIds: List[String] = Nil,
    positionPlanTechniqueRelativeCauseEvidenceIds: List[String] = Nil,
    moveMeaningClaimKinds: List[String] = Nil,
    moveMeaningClaimRoles: List[String] = Nil,
    moveMeaningClaimSupportLevels: List[String] = Nil,
    moveMeaningClaimVisibility: List[String] = Nil,
    moveMeaningClaimSurfaceLanes: List[String] = Nil,
    moveMeaningClaimLaneKeys: List[String] = Nil,
    moveMeaningClaimSurfaceSignatures: List[String] = Nil,
    publicMoveMeaningClaimSurfaceSignatures: List[String] = Nil
):
  val hasPrimaryCause: Boolean = primaryCauseKinds.nonEmpty

final case class CandidateComparisonDiagnostic(
    id: String,
    comparisonFingerprint: String,
    dedupeKey: String,
    dedupeClass: CandidateComparisonDedupeClass,
    subjectBinding: SubjectBindingClass,
    comparisonKind: CandidateComparisonKind,
    referenceLine: CandidateLineDiagnostic,
    candidateLine: CandidateLineDiagnostic,
    verdict: MoveChoiceVerdict,
    mover: String,
    rawCpLossForDiagnostics: Int,
    rawCandidateDeltaCpForDiagnostics: Int,
    winPercentLossForMover: Double,
    candidateWinPercentDeltaForMover: Double,
    causeKinds: List[RelativeCauseKind],
    causeRoles: List[RelativeCauseRole],
    causeSourceSides: List[RelativeCauseSourceSide],
    causeImportances: List[RelativeCauseImportance],
    causeEventLines: List[LineNodeRef],
    causeSupport: List[CandidateCauseSupportDiagnostic],
    relativeCauseDiagnostics: ComparisonRelativeCauseDiagnostics,
    moveJudgmentView: ComparisonMoveJudgmentViewDiagnostics,
    advisoryCauseHints: List[RelativeCauseKind],
    significanceReasons: List[CandidateComparisonSignificanceReason],
    lowSignalReasons: List[CandidateComparisonLowSignalReason],
    comparisonConfidence: EvidenceConfidence,
    causeConfidences: List[EvidenceConfidence],
    hasLowDepthCause: Boolean,
    hasUnexplainedEngineGap: Boolean,
    hasSecondaryContextEngineGap: Boolean,
    evidenceLayers: ComparisonEvidenceLayerDiagnostics,
    decisionTrace: CandidateCauseDecisionTrace,
    tacticalLossTrace: TacticalLossTrace,
    failureClass: CandidateComparisonFailureClass,
    failureReasons: List[CandidateComparisonFailureReason]
)

final case class CandidateCauseSupportDiagnostic(
    id: String,
    kind: RelativeCauseKind,
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    importance: RelativeCauseImportance,
    eventLine: LineNodeRef,
    evidenceLines: List[LineNodeRef],
    supportEvidenceIds: List[String],
    parentEvidenceIds: List[String],
    parentLayers: List[EvidenceLayer],
    parentLayerSignature: String,
    semanticSupportKinds: List[String],
    semanticSupportSignature: String,
    hasOwnedTypedDepth: Boolean,
    hasOwnedTacticalProof: Boolean,
    hasOwnedStrategicContrastDepth: Boolean,
    hasOwnedAdmissibleLongTermProof: Boolean,
    rawProofHasDirectProof: Boolean,
    rawProofHasContrastProof: Boolean,
    rawProofHasContextSupport: Boolean,
    directProofSourceIds: List[String],
    contrastProofSourceIds: List[String],
    contextSupportSourceIds: List[String],
    directProofKinds: List[String],
    contrastProofKinds: List[String],
    contextSupportKinds: List[String],
    proofBoardAnchors: List[BoardAnchorKind],
    proofLineEvents: List[LineEventKind],
    proofLineConsequences: List[LineConsequenceKind],
    proofRelationKinds: List[RelationFactKind],
    proofRelationDetails: List[String],
    proofRelationSourceIds: List[String],
    proofTacticalMechanismKinds: List[TacticalMechanismKind],
    proofTacticalMechanismSourceIds: List[String],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    proofTransitionConsequences: List[TransitionConsequenceProof],
    causeContextLayers: List[EvidenceLayer]
)

final case class OpeningApplicabilityDiagnostic(
    id: String,
    applicability: FeatureApplicability,
    status: ApplicabilityStatus,
    observedThemes: List[OpeningTheme],
    supportedThemes: List[OpeningTheme],
    unverifiedPriorThemes: List[OpeningTheme],
    observedOnlyThemes: List[OpeningTheme],
    priorMatchSources: List[OpeningThemePriorMatchSource],
    certifyingPriorPresent: Boolean,
    anchorSourceLayers: List[EvidenceLayer],
    anchorSignals: List[FeatureAnchorSignal],
    supportedAnchorSourceLayers: List[EvidenceLayer],
    supportedAnchorSignals: List[FeatureAnchorSignal],
    internalAnchorAligned: Boolean
)

final case class OpeningSupportDiagnostic(
    contextIds: List[String],
    assessmentIds: List[String],
    anchorIds: List[String],
    supportedAnchorIds: List[String],
    ideaIds: List[String],
    claimIds: List[String],
    candidateClaimIds: List[String],
    supportClusterIds: List[String],
    identityPresent: Boolean,
    recognitionPresent: Boolean,
    themePriorPresent: Boolean,
    priorLineages: List[String],
    requestedPriorLineages: List[String],
    canonicalPriorLineages: List[String],
    priorMatchSources: List[OpeningThemePriorMatchSource],
    openingSpecificPrior: Boolean,
    certifyingPriorPresent: Boolean,
    certifyingPriorMatchSources: List[OpeningThemePriorMatchSource],
    priorFamilies: List[OpeningFamily],
    typicalPawnStructures: List[String],
    centerBreaks: List[String],
    developmentPriorities: List[String],
    gambitCompensation: Boolean,
    strategicPlanPriors: List[String],
    observedThemes: List[OpeningTheme],
    supportedThemes: List[OpeningTheme],
    unverifiedPriorThemes: List[OpeningTheme],
    observedOnlyThemes: List[OpeningTheme],
    anchorSourceLayers: List[EvidenceLayer],
    supportedAnchorSourceLayers: List[EvidenceLayer],
    applicabilities: List[FeatureApplicability],
    assessmentStatuses: List[ApplicabilityStatus],
    internalAnchorAligned: Boolean
)

object CandidateComparisonDiagnostic:
  private final case class RelativeCauseDiagnosticRecord(
      record: EvidenceRecord,
      cause: RelativeCauseFact,
      id: String
  )

  private def relativeCauseDiagnosticRecords(packet: EvidenceBackedJudgmentPacket): List[RelativeCauseDiagnosticRecord] =
    val standalone =
      packet.evidenceGraph.records.collect {
        case record @ EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
          RelativeCauseDiagnosticRecord(record, cause, ref.id)
      }
    val standaloneCauseKeys = standalone.map(_.cause.identityKey).toSet
    val embedded =
      packet.evidenceGraph.records.flatMap {
        case record @ EvidenceRecord(ref, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.zipWithIndex
            .filterNot { case (cause, _) => standaloneCauseKeys.contains(cause.identityKey) }
            .map { case (cause, index) =>
              RelativeCauseDiagnosticRecord(
                record = record,
                cause = cause,
                id = s"${ref.id}:cause:$index:${cause.kind}"
              )
            }
        case _ =>
          Nil
    }
    (standalone ++ embedded).distinctBy(_.id)

  private def relativeCauseLifecycleMatches(
      diagnostic: ClaimLifecycleRelativeCause,
      record: RelativeCauseDiagnosticRecord
  ): Boolean =
    diagnostic.id == record.id ||
      (
        diagnostic.kind == record.cause.kind &&
          diagnostic.comparisonKind == record.cause.comparisonKind &&
          diagnostic.role == record.cause.role &&
          diagnostic.sourceSide == record.cause.sourceSide &&
          diagnostic.importance == record.cause.importance &&
          diagnostic.referenceLine == record.cause.referenceLine &&
          diagnostic.candidateLine == record.cause.candidateLine &&
          diagnostic.eventLine == record.cause.eventLine &&
          diagnostic.attributionKind == record.cause.attribution.kind &&
          diagnostic.attributionOwnedEvidenceIds.toSet == record.cause.attribution.ownedEvidence.map(_.id).toSet &&
          diagnostic.attributionContrastEvidenceIds.toSet == record.cause.attribution.contrastEvidence.map(_.id).toSet &&
          diagnostic.attributionContextEvidenceIds.toSet == record.cause.attribution.contextEvidence.map(_.id).toSet &&
          diagnostic.attributionRootMoveMatched == record.cause.attribution.rootMoveMatched &&
          diagnostic.attributionDirectProofEligible == record.cause.attribution.directProofEligible &&
          diagnostic.attributionReason == record.cause.attribution.reason &&
          diagnostic.supportEvidenceSourceIds.toSet == record.cause.supportEvidence.map(_.id).toSet &&
          lifecycleProofMatchesCause(diagnostic, record.cause)
      )

  private def lifecycleProofMatchesCause(
      diagnostic: ClaimLifecycleRelativeCause,
      cause: RelativeCauseFact
  ): Boolean =
    val strategicProof = cause.strategicProofIdentity
    cause.proof match
      case Some(proof) =>
        diagnostic.proofDirectSourceIds.toSet == proof.directProof.sourceRefs.map(_.id).toSet &&
          diagnostic.proofContrastSourceIds.toSet == proof.contrastProof.sourceRefs.map(_.id).toSet &&
          diagnostic.proofContextSupportSourceIds.toSet == proof.contextSupport.sourceRefs.map(_.id).toSet &&
          diagnostic.proofStrategicAxisKeys == strategicProof.axisKeys &&
          diagnostic.proofStrategicMechanismKinds == strategicProof.mechanismKinds &&
          diagnostic.proofStrategicMechanismSourceIds == strategicProof.mechanismSourceIds &&
          diagnostic.proofStrategicMechanismSignalSourceIds == strategicProof.signalSourceIds &&
          diagnostic.proofDirectKinds.toSet == proof.directProof.kindLabels.toSet &&
          diagnostic.proofContrastKinds.toSet == proof.contrastProof.kindLabels.toSet &&
          diagnostic.proofContextSupportKinds.toSet == proof.contextSupport.kindLabels.toSet
      case None =>
        diagnostic.proofDirectSourceIds.isEmpty &&
          diagnostic.proofContrastSourceIds.isEmpty &&
          diagnostic.proofContextSupportSourceIds.isEmpty &&
          diagnostic.proofStrategicAxisKeys.isEmpty &&
          diagnostic.proofStrategicMechanismKinds.isEmpty &&
          diagnostic.proofStrategicMechanismSourceIds.isEmpty &&
          diagnostic.proofStrategicMechanismSignalSourceIds.isEmpty &&
          diagnostic.proofDirectKinds.isEmpty &&
          diagnostic.proofContrastKinds.isEmpty &&
          diagnostic.proofContextSupportKinds.isEmpty

  def familyMismatchDiagnostics(packet: EvidenceBackedJudgmentPacket): List[RelativeCauseFamilyMismatchDiagnostic] =
    relativeCauseDiagnosticRecords(packet)
      .map(entry => familyMismatchDiagnosticFor(packet, entry))
      .filter(_.mismatchKinds.nonEmpty)

  private def familyMismatchDiagnosticFor(
      packet: EvidenceBackedJudgmentPacket,
      entry: RelativeCauseDiagnosticRecord
  ): RelativeCauseFamilyMismatchDiagnostic =
    val ideasById = packet.ideas.map(idea => idea.ref.id -> idea).toMap
    val claimsById = packet.claims.map(claim => claim.id -> claim).toMap
    val exactLifecycle =
      packet.claimLifecycle.filter(diagnostic =>
        diagnostic.relativeCauses.exists(relativeCauseLifecycleMatches(_, entry))
      )
    val evidenceIdeaIds =
      packet.ideas.filter(_.evidence.exists(_.id == entry.record.ref.id)).map(_.ref.id)
    val ideaIds =
      (evidenceIdeaIds ++ exactLifecycle.flatMap(_.ideaIds)).distinct.sorted
    val actualIdeaFamilies =
      ideaIds.flatMap(id => ideasById.get(id).map(_.ref.family)).toSet
    val candidateFamilies =
      exactLifecycle.map(_.family).toSet
    val candidateIds =
      exactLifecycle.map(_.candidateId).distinct.sorted
    val lifecycleFinalClaimIds =
      exactLifecycle.flatMap(_.finalClaimId).distinct.sorted
    val finalClaimIds =
      if lifecycleFinalClaimIds.nonEmpty then lifecycleFinalClaimIds
      else
        packet.claims
          .filter(claim =>
            claim.evidence.exists(_.id == entry.record.ref.id) ||
              claim.ideaRefs.exists(idea => ideaIds.contains(idea.id))
          )
          .map(_.id)
          .distinct
          .sorted
    val finalFamilies =
      finalClaimIds.flatMap(id => claimsById.get(id).map(_.family)).toSet
    val supportRecords = familyMismatchSupportRecords(packet.evidenceGraph, entry)
    val depthRecords = familyMismatchDepthRecords(packet.evidenceGraph, entry)
    val directProofRefs = entry.cause.proof.map(_.directProof.sourceRefs).getOrElse(Nil)
    val contrastProofRefs = entry.cause.proof.map(_.contrastProof.sourceRefs).getOrElse(Nil)
    val contextSupportRefs = entry.cause.proof.map(_.contextSupport.sourceRefs).getOrElse(Nil)
    val directProofKinds = entry.cause.proof.map(_.directProof.kindLabels).getOrElse(Nil)
    val contrastProofKinds = entry.cause.proof.map(_.contrastProof.kindLabels).getOrElse(Nil)
    val contextSupportKinds = entry.cause.proof.map(_.contextSupport.kindLabels).getOrElse(Nil)
    val directProofLayers = proofLayers(packet.evidenceGraph, directProofRefs)
    val contrastProofLayers = proofLayers(packet.evidenceGraph, contrastProofRefs)
    val contextSupportLayers = proofLayers(packet.evidenceGraph, contextSupportRefs) ++
      entry.cause.proof.map(_.contextSupport.contextLayers.toSet).getOrElse(Set.empty)
    val expectedIdeaFamilies = expectedIdeaFamiliesFor(entry.cause, supportRecords)
    val expectedClaimFamilies = expectedClaimFamiliesFor(entry.cause, expectedIdeaFamilies)
    val mismatchKinds =
      claimFamilyMismatchKinds(
        expectedIdeaFamilies = expectedIdeaFamilies,
        actualIdeaFamilies = actualIdeaFamilies,
        expectedClaimFamilies = expectedClaimFamilies,
        candidateFamilies = candidateFamilies,
        finalFamilies = finalFamilies,
        hasLongTermDepthProof = depthRecords.exists(longTermSupportRecord),
        hasConcreteTacticalDepthProof = relativeCauseHasTacticalProof(entry.cause)
      )
    RelativeCauseFamilyMismatchDiagnostic(
      causeId = entry.id,
      comparisonIds = comparisonIdsForCause(packet, entry.cause),
      causeKind = entry.cause.kind,
      causeRole = entry.cause.role,
      causeComparisonKind = entry.cause.comparisonKind,
      causeSourceSide = entry.cause.sourceSide,
      causeEventLine = entry.cause.eventLine,
      expectedIdeaFamilies = expectedIdeaFamilies,
      actualIdeaFamilies = actualIdeaFamilies,
      expectedClaimFamilies = expectedClaimFamilies,
      claimCandidateFamilies = candidateFamilies,
      finalClaimFamilies = finalFamilies,
      ideaIds = ideaIds,
      claimCandidateIds = candidateIds,
      finalClaimIds = finalClaimIds,
      lifecycleStages = exactLifecycle.flatMap(_.stages).toSet,
      lifecycleTruthStatuses = exactLifecycle.flatMap(_.truthStatus).toSet,
      supportLayers = supportRecords.map(_.ref.layer).toSet,
      directProofLayers = directProofLayers,
      contrastProofLayers = contrastProofLayers,
      contextSupportLayers = contextSupportLayers,
      directProofSourceIds = directProofRefs.map(_.id).distinct.sorted,
      contrastProofSourceIds = contrastProofRefs.map(_.id).distinct.sorted,
      contextSupportSourceIds = contextSupportRefs.map(_.id).distinct.sorted,
      directProofKinds = directProofKinds.distinct.sorted,
      contrastProofKinds = contrastProofKinds.distinct.sorted,
      contextSupportKinds = contextSupportKinds.distinct.sorted,
      proofSourceIds = supportRecords.map(_.ref.id).distinct.sorted,
      mismatchKinds = mismatchKinds
    )

  private def expectedIdeaFamiliesFor(
      cause: RelativeCauseFact,
      supportRecords: List[EvidenceRecord]
  ): Set[ChessIdeaFamily] =
    val base =
      ClaimEventCluster.kindForCause(cause.kind) match
        case Some(ClaimEventClusterKind.TacticalEvent)   => Set(ChessIdeaFamily.Tactical)
        case Some(ClaimEventClusterKind.DefensiveEvent)  => Set(ChessIdeaFamily.Defensive)
        case Some(ClaimEventClusterKind.ConversionEvent) => Set(ChessIdeaFamily.Conversion)
        case Some(ClaimEventClusterKind.MaterialEvent)   => Set(ChessIdeaFamily.Material)
        case None                                        => Set(ChessIdeaFamily.Strategic)
    val tactical =
        Option.when(relativeCauseHasTacticalProof(cause))(ChessIdeaFamily.Tactical)
    val pawn =
      Option.when(strategicRelativeCause(cause.kind) && supportRecords.exists(pawnStructureSupportRecord))(ChessIdeaFamily.PawnStructure)
    val opening =
      Option.when(strategicRelativeCause(cause.kind) && openingSupportRecordsCanSeedIdea(supportRecords))(ChessIdeaFamily.Opening)
    val conversion =
      Option.when(supportRecords.exists(conversionSupportRecord))(ChessIdeaFamily.Conversion)
    cause.kind match
      case RelativeCauseKind.MaterialSwing =>
        Option.when(cause.hasOwnedTypedDepth)(ChessIdeaFamily.Material).toSet ++
          Option.when(materialSwingHasTacticalProof(cause))(ChessIdeaFamily.Tactical).toSet ++
          Option.when(cause.hasOwnedTypedDepth && conversion.nonEmpty)(ChessIdeaFamily.Conversion).toSet
      case RelativeCauseKind.SacrificeCompensation =>
        Option.when(cause.hasOwnedTypedDepth)(ChessIdeaFamily.Material).toSet ++
          tactical.toSet ++
          Option.when(supportRecords.exists(strategicSupportRecord))(ChessIdeaFamily.Strategic).toSet
      case kind if strategicRelativeCause(kind) =>
        Option.when(supportRecords.exists(strategicSupportRecord))(ChessIdeaFamily.Strategic).toSet ++ pawn.toSet ++ opening.toSet
      case RelativeCauseKind.RecaptureRecoveryWindow =>
        tactical.toSet ++ Option.when(cause.hasOwnedTypedDepth)(conversion).flatten.toSet
      case _ =>
        val baseFamily =
          base.filter(family =>
            (family != ChessIdeaFamily.Tactical || tactical.nonEmpty) &&
              (family != ChessIdeaFamily.Material || cause.hasOwnedTypedDepth) &&
              (family != ChessIdeaFamily.Conversion || cause.hasOwnedTypedDepth) &&
              (family != ChessIdeaFamily.Defensive || ClaimTruthPolicy.defensiveRelativeCauseCanSeedIdea(cause))
          )
        baseFamily ++
          Option.when(
            materialConversionCause(cause.kind) &&
              cause.hasOwnedTypedDepth &&
              conversion.nonEmpty
          )(ChessIdeaFamily.Conversion).toSet

  private def expectedClaimFamiliesFor(
      cause: RelativeCauseFact,
      expectedIdeaFamilies: Set[ChessIdeaFamily]
  ): Set[ClaimFamily] =
    val mapped = expectedIdeaFamilies.map(expectedClaimFamilyForIdea)
    val plan =
      Option.when(
        cause.kind == RelativeCauseKind.PlanImprovement ||
          cause.kind == RelativeCauseKind.PlanContradiction
      )(ClaimFamily.Plan)
    mapped ++ plan.toSet

  private def expectedClaimFamilyForIdea(family: ChessIdeaFamily): ClaimFamily =
    family match
      case ChessIdeaFamily.Tactical      => ClaimFamily.Tactical
      case ChessIdeaFamily.Strategic     => ClaimFamily.Strategic
      case ChessIdeaFamily.PawnStructure => ClaimFamily.PawnStructure
      case ChessIdeaFamily.Opening       => ClaimFamily.Opening
      case ChessIdeaFamily.Defensive     => ClaimFamily.Defensive
      case ChessIdeaFamily.Conversion    => ClaimFamily.Conversion
      case ChessIdeaFamily.Material      => ClaimFamily.Material
      case ChessIdeaFamily.Evaluation    => ClaimFamily.Evaluation

  private def claimFamilyMismatchKinds(
      expectedIdeaFamilies: Set[ChessIdeaFamily],
      actualIdeaFamilies: Set[ChessIdeaFamily],
      expectedClaimFamilies: Set[ClaimFamily],
      candidateFamilies: Set[ClaimFamily],
      finalFamilies: Set[ClaimFamily],
      hasLongTermDepthProof: Boolean,
      hasConcreteTacticalDepthProof: Boolean
  ): Set[ClaimFamilyMismatchKind] =
    val actualClaimFamilies = candidateFamilies ++ finalFamilies
    val longTermClaims = actualClaimFamilies.intersect(Set(ClaimFamily.Strategic, ClaimFamily.PawnStructure, ClaimFamily.Opening, ClaimFamily.Plan))
    val finalFamiliesOnlyEvaluation = finalFamilies.nonEmpty && finalFamilies.subsetOf(Set(ClaimFamily.Evaluation))
    Set(
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Tactical) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.Tactical)
      )(ClaimFamilyMismatchKind.ExpectedTacticalIdeaActualOther),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Strategic) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.Strategic)
      )(ClaimFamilyMismatchKind.ExpectedStrategicIdeaActualOther),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.PawnStructure) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.PawnStructure)
      )(ClaimFamilyMismatchKind.ExpectedPawnStructureIdeaActualOther),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Opening) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.Opening)
      )(ClaimFamilyMismatchKind.ExpectedOpeningIdeaActualOther),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Defensive) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.Defensive)
      )(ClaimFamilyMismatchKind.ExpectedDefensiveIdeaActualOther),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Material) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.Material)
      )(ClaimFamilyMismatchKind.ExpectedMaterialIdeaActualOther),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Conversion) &&
          actualIdeaFamilies.nonEmpty &&
          !actualIdeaFamilies.contains(ChessIdeaFamily.Conversion)
      )(ClaimFamilyMismatchKind.ExpectedConversionIdeaActualOther),
      Option.when(
        actualIdeaFamilies.contains(ChessIdeaFamily.Tactical) &&
          actualClaimFamilies.nonEmpty &&
          !actualClaimFamilies.contains(ClaimFamily.Tactical)
      )(ClaimFamilyMismatchKind.TacticalIdeaClaimFamilyMismatch),
      Option.when(
        actualClaimFamilies.contains(ClaimFamily.Tactical) &&
          hasLongTermDepthProof &&
          !hasConcreteTacticalDepthProof
      )(ClaimFamilyMismatchKind.StrategicSupportUsedAsTacticalProof),
      Option.when(
        expectedIdeaFamilies.contains(ChessIdeaFamily.Tactical) &&
          longTermClaims.nonEmpty &&
          !actualClaimFamilies.contains(ClaimFamily.Tactical)
      )(ClaimFamilyMismatchKind.TacticalEvidenceAbsorbedByLongTermClaim),
      Option.when(
        expectedClaimFamilies.exists(family => family == ClaimFamily.Material || family == ClaimFamily.Conversion) &&
          finalFamiliesOnlyEvaluation
      )(ClaimFamilyMismatchKind.MaterialConversionCauseOnlyEvaluationClaim),
      Option.when(
        actualIdeaFamilies.nonEmpty &&
          expectedIdeaFamilies.intersect(actualIdeaFamilies).isEmpty
      )(ClaimFamilyMismatchKind.ExpectedIdeaFamilyMissing),
      Option.when(
        actualClaimFamilies.nonEmpty &&
          expectedClaimFamilies.intersect(actualClaimFamilies).isEmpty
      )(ClaimFamilyMismatchKind.ExpectedClaimFamilyMissing)
    ).flatten

  private def familyMismatchSupportRecords(
      graph: TypedEvidenceGraph,
      entry: RelativeCauseDiagnosticRecord
  ): List[EvidenceRecord] =
    val proofRefs =
      entry.cause.proof.toList.flatMap(proof =>
        proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs ++ proof.contextSupport.sourceRefs
      )
    val comparisonParents = entry.record.parents.filter(ref =>
      graph.byId.get(ref.id).exists {
        case EvidenceRecord(_, CandidateComparisonEvidence(_), _) => true
        case _                                                    => false
      }
    )
    (comparisonParents ++ entry.cause.supportEvidence ++ proofRefs)
      .distinctBy(_.id)
      .flatMap(ref => graph.byId.get(ref.id))

  private def familyMismatchDepthRecords(
      graph: TypedEvidenceGraph,
      entry: RelativeCauseDiagnosticRecord
  ): List[EvidenceRecord] =
    val depthProofRefs =
      entry.cause.proof.toList.flatMap(proof => proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs)
    depthProofRefs
      .distinctBy(_.id)
      .flatMap(ref => graph.byId.get(ref.id))

  private def comparisonIdsForCause(
      packet: EvidenceBackedJudgmentPacket,
      cause: RelativeCauseFact
  ): List[String] =
    packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) if causeMatchesComparison(cause, fact) =>
        ref.id
    }.distinct.sorted

  private def proofLayers(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Set[EvidenceLayer] =
    refs.flatMap(ref => graph.byId.get(ref.id).map(_.ref.layer)).toSet

  private def relativeCauseHasTacticalProof(cause: RelativeCauseFact): Boolean =
    cause.hasOwnedTacticalProof

  private def materialSwingHasTacticalProof(cause: RelativeCauseFact): Boolean =
    val engineBackedMaterialSwing =
      cause.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
        cause.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
    engineBackedMaterialSwing && relativeCauseHasTacticalProof(cause)

  private def materialConversionCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.RecaptureRecoveryWindow || kind == RelativeCauseKind.MaterialSwing

  private def longTermSupportRecord(record: EvidenceRecord): Boolean =
    record.payload match
      case payload: StrategicMechanismEvidence =>
        payload.canSupportStrategicCause || payload.canAnchorPlanIdea || payload.canAnchorOpeningIdea
      case _ =>
        false

  private def strategicRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).isEmpty

  private def pawnStructureSupportRecord(record: EvidenceRecord): Boolean =
    record.payload match
      case payload: StrategicMechanismEvidence =>
        payload.canAnchorPawnStructureIdea
      case _ =>
        false

  private def strategicSupportRecord(record: EvidenceRecord): Boolean =
    record.payload match
      case payload: StrategicMechanismEvidence =>
        payload.canSupportStrategicCause || payload.canAnchorPlanIdea
      case _ =>
        false

  private def openingSupportRecordsCanSeedIdea(records: List[EvidenceRecord]): Boolean =
    StrategicMechanismEvidence.openingClaimSupported(records)

  private def conversionSupportRecord(record: EvidenceRecord): Boolean =
    record.payload match
      case payload: LineFactEvidence =>
        payload.hasConversionConsequence
      case payload: StructuralDeltaEvidence =>
        payload.consequences.exists(_.kind == TransitionConsequenceKind.PromotionPressureGain)
      case payload: RelationFactEvidence =>
        payload.kind == RelationFactKind.BadPieceLiquidation
      case SinglePositionEvidence(assessment) =>
        assessment.simplifyBias.shouldSimplify || assessment.gamePhase.isEndgame
      case _ =>
        false

  def fromPacket(packet: EvidenceBackedJudgmentPacket): List[CandidateComparisonDiagnostic] =
    fromPacket(packet, JudgmentPacketValidator.validate(packet))

  def fromPacket(
      packet: EvidenceBackedJudgmentPacket,
      validation: JudgmentPacketValidationResult
  ): List[CandidateComparisonDiagnostic] =
    val playedMoves = JudgmentSubjectBinding.packetPlayedMoves(packet)
    val causeRecords = relativeCauseDiagnosticRecords(packet)
    val validationIssueIds = relativeCauseValidationIssueIds(validation)
    val diagnostics = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
        val neighborhood = comparisonNeighborhood(packet, fact)
        val matchingCauses =
          causeRecords.filter(entry => causeMatchesComparison(entry.cause, fact)).map(_.cause)
        val matchingCauseRecords =
          causeRecords.filter(entry => causeMatchesComparison(entry.cause, fact))
        val evidenceLayers =
          ComparisonEvidenceLayerDiagnostics(
            reference = evidenceNeighborhood(packet.evidenceGraph, neighborhood.referenceRecords),
            candidate = evidenceNeighborhood(packet.evidenceGraph, neighborhood.candidateRecords)
          )
        val causeKinds = matchingCauses.map(_.kind).distinct
        val causeRoles = matchingCauses.map(_.role).distinct
        val causeSourceSides = matchingCauses.map(_.sourceSide).distinct
        val causeImportances = matchingCauses.map(_.importance).distinct
        val causeEventLines = matchingCauses.map(_.eventLine).distinct
        val causeSupport = causeSupportDiagnostics(matchingCauseRecords, packet.evidenceGraph)
        val trace = decisionTraceFor(
          fact,
          neighborhood.referenceRecords,
          neighborhood.candidateRecords,
          neighborhood.sharedRecords
        )
        val signalProfile =
          RelativeCauseSignalProfile.from(
            fact = fact,
            referenceRecords = neighborhood.referenceRecords,
            candidateRecords = neighborhood.candidateRecords,
            sharedRecords = neighborhood.sharedRecords
          )
        val advisoryCauseHints = advisoryCauseHintsFor(fact, causeKinds, signalProfile)
        val relativeCauseDiagnostics =
          relativeCauseDiagnosticsFor(
            packet = packet,
            fact = fact,
            signalProfile = signalProfile,
            causeRecords = matchingCauseRecords,
            causeSupport = causeSupport,
            missingExpectedCauseHints = advisoryCauseHints,
            validationIssueIds = validationIssueIds
          )
        val moveJudgmentViewDiagnostics = moveJudgmentViewDiagnosticsFor(packet, fact, ref.position)
        val tacticalLossTrace =
          tacticalLossTraceFor(packet, fact, neighborhood, matchingCauseRecords)
        val secondaryContextGap =
          requiresExplanatoryCause(fact) && causeKinds.isEmpty && contextAlternativeComparison(fact)
        val referenceLine = lineDiagnostic(packet, fact.referenceLine)
        val candidateLine = lineDiagnostic(packet, fact.candidateLine)
        val hasLowDepthCause =
          lowDepthCauseShouldBeAudited(fact, causeKinds) &&
            causeSupport.exists(!_.hasOwnedTypedDepth)
        val hasGenericCause =
          relativeCauseDiagnostics.genericCauseIds.nonEmpty
        val hasUnboundEvidence =
          relativeCauseDiagnostics.unboundEvidenceIds.nonEmpty
        CandidateComparisonDiagnostic(
          id = ref.id,
          comparisonFingerprint = comparisonFingerprint(packet, fact),
          dedupeKey = comparisonDedupeKey(packet, fact),
          dedupeClass = CandidateComparisonDedupeClass.Unique,
          subjectBinding = JudgmentSubjectBinding.recordBinding(record, playedMoves),
          comparisonKind = fact.kind,
          referenceLine = referenceLine,
          candidateLine = candidateLine,
          verdict = fact.comparison.verdict,
          mover = fact.comparison.mover.name,
          rawCpLossForDiagnostics = fact.comparison.rawCpLossForDiagnostics,
          rawCandidateDeltaCpForDiagnostics = fact.comparison.rawCandidateDeltaCpForDiagnostics,
          winPercentLossForMover = fact.comparison.winPercentLossForMover,
          candidateWinPercentDeltaForMover = fact.comparison.candidateWinPercentDeltaForMover,
          causeKinds = causeKinds,
          causeRoles = causeRoles,
          causeSourceSides = causeSourceSides,
          causeImportances = causeImportances,
          causeEventLines = causeEventLines,
          causeSupport = causeSupport,
          relativeCauseDiagnostics = relativeCauseDiagnostics,
          moveJudgmentView = moveJudgmentViewDiagnostics,
          advisoryCauseHints = advisoryCauseHints,
          significanceReasons = significanceReasons(fact),
          lowSignalReasons = lowSignalReasons(fact),
          comparisonConfidence = ref.confidence,
          causeConfidences = matchingCauseRecords.map(_.record.ref.confidence).distinct,
          hasLowDepthCause = hasLowDepthCause,
          hasUnexplainedEngineGap = requiresExplanatoryCause(fact) && causeKinds.isEmpty && !secondaryContextGap,
          hasSecondaryContextEngineGap = secondaryContextGap,
          evidenceLayers = evidenceLayers,
          decisionTrace = trace,
          tacticalLossTrace = tacticalLossTrace,
          failureClass = classifyFailure(
            fact,
            causeKinds,
            evidenceLayers,
            trace,
            moveJudgmentViewDiagnostics,
            hasLowDepthCause,
            hasGenericCause,
            hasUnboundEvidence
          ),
          failureReasons = failureReasonsFor(
            fact,
            causeKinds,
            evidenceLayers,
            trace,
            moveJudgmentViewDiagnostics,
            hasLowDepthCause,
            hasGenericCause,
            hasUnboundEvidence
          )
        )
    }
    val duplicateKeys =
      diagnostics.groupMapReduce(_.dedupeKey)(_ => 1)(_ + _).collect { case (key, count) if count > 1 => key }.toSet
    diagnostics.map(diagnostic =>
      if duplicateKeys.contains(diagnostic.dedupeKey) then
        diagnostic.copy(dedupeClass = CandidateComparisonDedupeClass.DuplicateEpisode)
      else diagnostic
    )

  private def moveJudgmentViewDiagnosticsFor(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      comparisonPosition: PositionNodeRef
  ): ComparisonMoveJudgmentViewDiagnostics =
    val primaryFrames = comparisonCauseFrames(packet, fact, _.causeAudit.primary)
    val secondaryFrames = comparisonCauseFrames(packet, fact, _.causeAudit.secondary)
    val contextFrames = comparisonCauseFrames(packet, fact, _.causeAudit.context)
    val contextCauseEvidenceIds = contextFrames.flatMap(_.causeEvidenceIds).distinct.sorted
    val primaryTacticalWitnessFrames =
      primaryFrames.filter(_.narrativeRole == MoveJudgmentCauseNarrativeRole.TacticalWitness)
    val primaryPunishmentWitnessFrames =
      primaryTacticalWitnessFrames.filter(_.witnessBindingLevel == MoveJudgmentCauseWitnessBindingLevel.Punishment)
    val primaryContextualTacticalWitnessFrames =
      primaryTacticalWitnessFrames.filterNot(_.witnessBindingLevel == MoveJudgmentCauseWitnessBindingLevel.Punishment)
    val planTechniqueFrames = comparisonPositionPlanTechniqueFrames(packet, fact, comparisonPosition)
    val allFrames = primaryFrames ++ secondaryFrames ++ contextFrames
    val primaryRootFrames = primaryFrames.filter(_.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause)
    val moveMeaningClaims =
      comparisonMoveMeaningClaims(packet, fact, planTechniqueFrames.map(_.id).toSet)
    val comparisonMoveMeaningClaimSet = moveMeaningClaims.toSet
    val publicMoveMeaningClaims =
      packet.moveJudgmentView.toList
        .flatMap(MoveMeaningSurface.publicClaimsForSurface)
        .filter(comparisonMoveMeaningClaimSet.contains)
    ComparisonMoveJudgmentViewDiagnostics(
      primaryCauseKinds = primaryFrames.map(_.causeKind).distinct,
      secondaryCauseKinds = secondaryFrames.map(_.causeKind).distinct,
      contextCauseKinds = contextFrames.map(_.causeKind).distinct,
      primaryCauseEvidenceIds = primaryFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      primaryIdeaFamilies = frameIdeaFamilies(packet, primaryFrames),
      primaryClaimCandidateFamilies = frameClaimCandidateFamilies(packet, primaryFrames),
      primaryFinalClaimFamilies = frameFinalClaimFamilies(packet, primaryFrames),
      primaryFramedCauseKinds = primaryFrames.filter(_.framed).map(_.causeKind).distinct,
      primaryUnframedCauseKinds = primaryFrames.filterNot(_.framed).map(_.causeKind).distinct,
      primaryRootCauseKinds =
        primaryFrames.filter(_.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause).map(_.causeKind).distinct,
      primaryTacticalWitnessCauseKinds = primaryTacticalWitnessFrames.map(_.causeKind).distinct,
      primaryPunishmentWitnessCauseKinds = primaryPunishmentWitnessFrames.map(_.causeKind).distinct,
      primaryContextualTacticalWitnessCauseKinds = primaryContextualTacticalWitnessFrames.map(_.causeKind).distinct,
      primaryRootCauseEvidenceIds =
        primaryRootFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      rootArbitrationTiers = allFrames.map(_.rootArbitrationTier).sortBy(_.toString),
      primaryRootArbitrationTiers = primaryRootFrames.map(_.rootArbitrationTier).sortBy(_.toString),
      primaryRootCauseEvidenceIdTierSignatures =
        primaryRootFrames
          .flatMap(frame =>
            frame.causeEvidenceIds.distinct.sorted.map(causeEvidenceId =>
              primaryRootCauseEvidenceIdTierSignature(causeEvidenceId, frame.rootArbitrationTier)
            )
          )
          .distinct
          .sorted,
      primaryTacticalWitnessCauseEvidenceIds = primaryTacticalWitnessFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      primaryPunishmentWitnessCauseEvidenceIds = primaryPunishmentWitnessFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      primaryContextualTacticalWitnessCauseEvidenceIds =
        primaryContextualTacticalWitnessFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      secondaryCauseEvidenceIds = secondaryFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      contextCauseEvidenceIds = contextCauseEvidenceIds,
      projectedContextCauseNoViewIds = projectedContextCauseNoViewIds(packet, fact, contextCauseEvidenceIds.toSet),
      playableLossPrimaryCauseEvidenceIds =
        if fact.comparison.verdict == MoveChoiceVerdict.PlayableLoss then primaryFrames.flatMap(_.causeEvidenceIds).distinct.sorted
        else Nil,
      objectlessPrimaryCauseEvidenceIds = primaryFrames.filterNot(_.concreteObjectReady).flatMap(_.causeEvidenceIds).distinct.sorted,
      objectlessSecondaryCauseEvidenceIds =
        secondaryFrames.filterNot(_.concreteObjectReady).flatMap(_.causeEvidenceIds).distinct.sorted,
      objectlessContextCauseEvidenceIds = contextFrames.filterNot(_.concreteObjectReady).flatMap(_.causeEvidenceIds).distinct.sorted,
      positionPlanTechniqueFrameIds = planTechniqueFrames.map(_.id).distinct.sorted,
      positionPlanTechniqueUnits = planTechniqueFrames.flatMap(_.units).distinct.sortBy(_.toString),
      positionPlanTechniqueAxisKeys = planTechniqueFrames.flatMap(_.strategicAxisKeys).distinct.sorted,
      positionPlanTechniqueSemanticDetailUnits =
        planTechniqueFrames.flatMap(_.semanticDetails.map(_.unit)).distinct.sortBy(_.toString),
      positionPlanTechniqueSemanticDetailAxisKeys =
        planTechniqueFrames.flatMap(_.semanticDetails.flatMap(_.axisKey)).distinct.sorted,
      positionPlanTechniqueSemanticDetailMechanismKinds =
        planTechniqueFrames.flatMap(_.semanticDetails.flatMap(_.mechanismKinds)).distinct.sortBy(_.toString),
      positionPlanTechniqueSemanticDetailAnchorKeys =
        planTechniqueFrames.flatMap(_.semanticDetails.flatMap(_.semanticAnchorKeys)).distinct.sorted,
      positionPlanTechniqueSemanticDetailTokens =
        positionPlanTechniqueSemanticDetailTokens(planTechniqueFrames),
      positionPlanTechniqueSemanticDetailTokenGroups =
        positionPlanTechniqueSemanticDetailTokenGroups(planTechniqueFrames),
      positionPlanTechniqueObjectBindingSignatures =
        positionPlanTechniqueObjectBindingSignatures(planTechniqueFrames),
      positionPlanTechniqueEvidenceIds = planTechniqueFrames.flatMap(_.evidenceIds).distinct.sorted,
      positionPlanTechniqueRelativeCauseEvidenceIds = planTechniqueFrames.flatMap(_.relativeCauseEvidenceIds).distinct.sorted,
      moveMeaningClaimKinds = moveMeaningClaims.map(_.meaningKind).distinct.sorted,
      moveMeaningClaimRoles = moveMeaningClaims.map(_.role).distinct.sorted,
      moveMeaningClaimSupportLevels = moveMeaningClaims.map(_.supportLevel).distinct.sorted,
      moveMeaningClaimVisibility = moveMeaningClaims.map(_.visibility).distinct.sorted,
      moveMeaningClaimSurfaceLanes = moveMeaningClaims.map(_.surfaceLane).distinct.sorted,
      moveMeaningClaimLaneKeys = moveMeaningClaims.map(_.laneKey).distinct.sorted,
      moveMeaningClaimSurfaceSignatures =
        moveMeaningClaims.map(moveMeaningClaimSurfaceSignature).distinct.sorted,
      publicMoveMeaningClaimSurfaceSignatures =
        publicMoveMeaningClaims.map(moveMeaningClaimSurfaceSignature)
          .distinct
          .sorted
    )

  private def comparisonMoveMeaningClaims(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      comparisonFrameIds: Set[String]
  ): List[MoveMeaningClaim] =
    val referenceMove = JudgmentSubjectBinding.normalizeMove(fact.referenceLine.rootMove)
    val candidateMove = JudgmentSubjectBinding.normalizeMove(fact.candidateLine.rootMove)
    packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims).filter { claim =>
      val move = JudgmentSubjectBinding.normalizeMove(claim.moveUci)
      comparisonFrameIds.contains(claim.frameId) &&
        (move == referenceMove || move == candidateMove)
    }

  private def moveMeaningClaimSurfaceSignature(claim: MoveMeaningClaim): String =
    def signatureField(values: List[String]): String =
      values.distinct.sorted match
        case Nil    => "none"
        case tokens => tokens.mkString(",")

    val axis = claim.axisKey.getOrElse("none")
    val proofTokens =
      claim.reasonTokens.filter(_.startsWith("proofRole:")).map(_.stripPrefix("proofRole:")) ++
        claim.objectBindingSignatures.flatMap(signature => EvidenceObjectBinding.signatureTokens(List(signature), "proof="))
    val objectTokens =
      claim.objectBindingSignatures
        .map(_.replace('|', ';'))
        .take(4)
    val publicEvidence = MoveMeaningSurface.evidenceForClaim(claim)
    List(
      s"unit=${claim.unit}",
      s"axis=$axis",
      s"kind=${claim.meaningKind}",
      s"support=${claim.supportLevel}",
      s"lane=${claim.surfaceLane}",
      s"lineRole=${claim.lineRole}",
      s"move=${claim.moveUci}",
      s"causes=${signatureField(claim.causeEvidenceIds)}",
      s"sources=${signatureField(claim.sourceEvidenceIds.take(6))}",
      s"proof=${signatureField(proofTokens)}",
      s"carrier=${publicEvidence.hasCarrier}",
      s"boardCarrier=${publicEvidence.boardCarriers.nonEmpty}",
      s"objects=${signatureField(objectTokens)}"
    ).mkString("|")

  private def primaryRootCauseEvidenceIdTierSignature(
      causeEvidenceId: String,
      tier: MoveJudgmentCauseRootArbitrationTier
  ): String =
    s"$causeEvidenceId|tier=$tier"

  private def comparisonPositionPlanTechniqueFrames(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      comparisonPosition: PositionNodeRef
  ): List[PositionPlanTechniqueFrame] =
    val comparisonLines = Set(fact.referenceLine, fact.candidateLine)
    val comparisonCauseIds =
      relativeCauseDiagnosticRecords(packet)
        .collect { case record if relativeCauseMatchesComparison(record.cause, fact) => record.id }
        .toSet
    packet.moveJudgmentView.toList
      .flatMap(_.positionPlanTechniqueFrames)
      .filter(frame =>
        frameMechanismContrastMatchesComparison(packet, frame, fact) ||
          frame.relativeCauseEvidenceIds.exists(comparisonCauseIds.contains) ||
          (frameMechanismContrastEvidenceIds(packet, frame).isEmpty && frame.line.exists(comparisonLines.contains)) ||
          (
            frame.line.isEmpty &&
              frame.relativeCauseEvidenceIds.isEmpty &&
              frameMechanismContrastEvidenceIds(packet, frame).isEmpty &&
              (frame.scope == EvidenceScope.CurrentPosition || frame.scope == EvidenceScope.BeforePosition) &&
              frame.position == comparisonPosition
          )
      )
      .distinctBy(_.id)
      .sortBy(_.id)

  private def frameMechanismContrastMatchesComparison(
      packet: EvidenceBackedJudgmentPacket,
      frame: PositionPlanTechniqueFrame,
      fact: CandidateComparisonFact
  ): Boolean =
    frameMechanismContrastEvidenceIds(packet, frame).exists(id =>
      packet.evidenceGraph.byId.get(id).exists {
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          payload.comparisonKind == fact.kind &&
            payload.referenceLine == fact.referenceLine &&
            payload.candidateLine == fact.candidateLine
        case _ =>
          false
      }
    )

  private def frameMechanismContrastEvidenceIds(
      packet: EvidenceBackedJudgmentPacket,
      frame: PositionPlanTechniqueFrame
  ): Set[String] =
    (frame.mechanismEvidenceIds ++ frame.evidenceIds).toSet.filter(id =>
      packet.evidenceGraph.byId.get(id).exists {
        case EvidenceRecord(_, _: StrategicMechanismContrastEvidence, _) => true
        case _                                                           => false
      }
    )

  private def positionPlanTechniqueSemanticDetailTokens(frames: List[PositionPlanTechniqueFrame]): List[String] =
    frames.flatMap(_.semanticDetails.flatMap(positionPlanTechniqueSemanticDetailTokens)).distinct.sorted

  private def positionPlanTechniqueSemanticDetailTokenGroups(frames: List[PositionPlanTechniqueFrame]): List[List[String]] =
    frames
      .flatMap(_.semanticDetails.map(positionPlanTechniqueSemanticDetailTokens))
      .map(_.distinct.sorted)
      .filter(_.nonEmpty)
      .distinct
      .sortBy(_.mkString("\u0000"))

  private def positionPlanTechniqueSemanticDetailTokens(detail: PositionPlanTechniqueSemanticDetail): List[String] =
    (
      List(
        Some(s"unit:${detail.unit}"),
        detail.axisKey.map(value => s"axisKey:$value"),
        detail.axisKind.map(value => s"axisKind:$value"),
        detail.axisPolarity.map(value => s"axisPolarity:$value"),
        detail.label.map(value => s"label:$value"),
        detail.contrastOutcome.map(value => s"contrastOutcome:$value"),
        detail.referenceStrength.map(value => s"referenceStrength:$value"),
        detail.candidateStrength.map(value => s"candidateStrength:$value"),
        detail.narrativeHorizon.map(value => s"narrativeHorizon:$value"),
        detail.raceLeadingLineRole.map(value => s"raceLeadingLineRole:$value"),
        detail.raceReferenceRootMove.map(value => s"raceReferenceRootMove:$value"),
        detail.raceCandidateRootMove.map(value => s"raceCandidateRootMove:$value"),
        detail.threatKind.map(value => s"threatKind:$value"),
        detail.threatDriver.map(value => s"threatDriver:$value"),
        detail.threatSeverity.map(value => s"threatSeverity:$value"),
        detail.turnsToImpact.map(value => s"turnsToImpact:$value"),
        detail.defenseMove.map(value => s"defenseMove:$value"),
        detail.prophylaxisNeeded.map(value => s"prophylaxisNeeded:$value"),
        detail.maxWinPercentLossIfIgnored.map(value => s"maxWinPercentLossIfIgnored:$value"),
        detail.pawnBreakReady.map(value => s"pawnBreakReady:$value"),
        detail.breakFile.map(value => s"breakFile:$value"),
        detail.breakImpact.map(value => s"breakImpact:$value"),
        detail.advanceOrCapture.map(value => s"advanceOrCapture:$value"),
        detail.passedPawnUrgency.map(value => s"passedPawnUrgency:$value"),
        detail.passerBlockade.map(value => s"passerBlockade:$value"),
        detail.blockadeSquare.map(value => s"blockadeSquare:$value"),
        detail.blockadeRole.map(value => s"blockadeRole:$value"),
        detail.pusherSupport.map(value => s"pusherSupport:$value"),
        detail.minorityAttack.map(value => s"minorityAttack:$value"),
        detail.counterBreak.map(value => s"counterBreak:$value"),
        detail.tensionPolicy.map(value => s"tensionPolicy:$value"),
        detail.pawnPlayDriver.map(value => s"pawnPlayDriver:$value"),
        detail.planAlignmentScore.map(value => s"planAlignmentScore:$value"),
        detail.planAlignmentBand.map(value => s"planAlignmentBand:$value"),
        detail.openingPriorLineage.map(value => s"openingPriorLineage:$value"),
        detail.openingPriorFamily.map(value => s"openingPriorFamily:$value"),
        detail.openingPriorGambitCompensation.map(value => s"openingPriorGambitCompensation:$value"),
        detail.openingPriorMatchSource.map(value => s"openingPriorMatchSource:$value"),
        detail.openingPriorRequestedLineage.map(value => s"openingPriorRequestedLineage:$value"),
        detail.openingPriorCanonicalLineage.map(value => s"openingPriorCanonicalLineage:$value"),
        detail.openingPriorOpeningSpecific.map(value => s"openingPriorOpeningSpecific:$value"),
        detail.openingPriorCanCertify.map(value => s"openingPriorCanCertify:$value"),
        detail.resourceContestActorSide.map(value => s"resourceContestActorSide:$value"),
        detail.resourceContestTargetSide.map(value => s"resourceContestTargetSide:$value"),
        detail.resourceContestMagnitude.map(value => s"resourceContestMagnitude:$value"),
        detail.structuralRouteMove.map(value => s"structuralRouteMove:$value"),
        detail.structuralRouteRole.map(value => s"structuralRouteRole:$value"),
        detail.structuralRoutePerspective.map(value => s"structuralRoutePerspective:$value"),
        detail.structuralRouteFromPly.map(value => s"structuralRouteFromPly:$value"),
        detail.structuralRouteToPly.map(value => s"structuralRouteToPly:$value"),
        detail.structuralPurposeStrength.map(value => s"structuralPurposeStrength:$value"),
        detail.endgameTechniquePattern.map(value => s"pattern:$value"),
        detail.endgameTechniqueRookPattern.map(value => s"rook-pattern:$value"),
        detail.endgameTechniqueSide.map(value => s"techniqueSide:$value"),
        detail.endgameTechniqueHorizonStatus.map(value => s"horizonStatus:$value"),
        detail.endgameTechniqueTriggerMove.map(value => s"triggerMove:$value"),
        detail.endgameTechniqueEntryPlyOffset.map(value => s"entryPlyOffset:$value"),
        detail.endgameTechniqueTerminalPlyOffset.map(value => s"terminalPlyOffset:$value"),
        detail.endgameTechniqueFailureReason.map(value => s"failureReason:$value"),
        detail.anchorMagnitude.map(value => s"anchorMagnitude:$value"),
        Some(s"specificityTier:${detail.specificityTier}")
      ).flatten ++
        detail.mechanismKinds.map(value => s"mechanismKind:$value") ++
        detail.semanticAnchorKeys.map(value => s"semanticAnchor:$value") ++
        detail.referenceEvidenceIds.map(value => s"referenceEvidenceId:$value") ++
        detail.candidateEvidenceIds.map(value => s"candidateEvidenceId:$value") ++
        detail.referencePlanIds.map(value => s"referencePlanId:$value") ++
        detail.candidatePlanIds.map(value => s"candidatePlanId:$value") ++
        detail.boardAnchorKinds.map(value => s"boardAnchorKind:$value") ++
        detail.boardAnchorSignals.map(value => s"boardAnchorSignal:$value") ++
        detail.requiredSquares.map(value => s"requiredSquare:$value") ++
        detail.maintainedSquares.map(value => s"maintainedSquare:$value") ++
        detail.brokenSquares.map(value => s"brokenSquare:$value") ++
        detail.terminalConsequenceKinds.map(value => s"terminalConsequenceKind:$value") ++
        detail.tensionSquares.map(value => s"tensionSquare:$value") ++
        detail.tensionEdges.map(value => s"tensionEdge:$value") ++
        detail.counterBreakFiles.map(value => s"counterBreakFile:$value") ++
        detail.matchedPlanIds.map(value => s"matchedPlanId:$value") ++
        detail.missingPlanIds.map(value => s"missingPlanId:$value") ++
        detail.planAlignmentReasonCodes.map(value => s"planAlignmentReasonCode:$value") ++
        detail.openingPriorThemes.map(value => s"openingPriorTheme:$value") ++
        detail.openingPriorTypicalPawnStructures.map(value => s"openingPriorTypicalPawnStructure:$value") ++
        detail.openingPriorCenterBreaks.map(value => s"openingPriorCenterBreak:$value") ++
        detail.openingPriorDevelopmentPriorities.map(value => s"openingPriorDevelopmentPriority:$value") ++
        detail.openingPriorStrategicPlanPriors.map(value => s"openingPriorStrategicPlanPrior:$value") ++
        detail.resourceContestKinds.map(value => s"resourceContestKind:$value") ++
        detail.resourceContestSignals.map(value => s"resourceContestSignal:$value") ++
        detail.resourceContestSquares.map(value => s"resourceContestSquare:$value") ++
        detail.resourceContestFiles.map(value => s"resourceContestFile:$value") ++
        detail.resourceContestScopes.map(value => s"resourceContestScope:$value") ++
        positionPlanTechniqueStructuralRouteTokens(detail) ++
        detail.structuralPurposeConsequences.map(value => s"structuralPurposeConsequence:$value") ++
        detail.structuralPurposeSubjects.map(value => s"structuralPurposeSubject:$value") ++
        detail.structuralPurposeCategories.map(value => s"structuralPurposeCategory:$value") ++
        detail.structuralPurposePolarities.map(value => s"structuralPurposePolarity:$value") ++
        detail.structuralMotifTags ++
        detail.structuralMotifTags.map(value => s"structuralMotif:$value") ++
        detail.causeEvidenceIds.map(value => s"causeEvidenceId:$value") ++
        detail.proofRoles.map(value => s"proofRole:$value") ++
        detail.contextCauseEvidenceIds.map(value => s"contextCauseEvidenceId:$value") ++
        detail.contextProofRoles.map(value => s"contextProofRole:$value") ++
        detail.objectBindingSignatures.flatMap(positionPlanTechniqueObjectBindingTokens) ++
        detail.planAlignmentReasonWeights.toList
          .sortBy(_._1)
          .map { case (reason, weight) => s"planAlignmentReasonWeight:$reason=$weight" } ++
        detail.sourceEvidenceIds.flatMap(positionPlanTechniqueSourceEvidenceTokens)
      ).distinct.sorted

  private def positionPlanTechniqueSourceEvidenceTokens(evidenceId: String): List[String] =
    List(s"sourceEvidenceId:$evidenceId") ++
      Option
        .when(evidenceId.startsWith("line:") || evidenceId.contains(":evidence:line:"))("sourceEvidenceId:line:")
        .toList

  private def positionPlanTechniqueObjectBindingSignatures(frames: List[PositionPlanTechniqueFrame]): List[String] =
    (
      frames.flatMap(_.objectBindings.map(_.signature)) ++
        frames.flatMap(_.semanticDetails.flatMap(_.objectBindingSignatures))
    ).distinct.sorted

  private def positionPlanTechniqueStructuralRouteTokens(detail: PositionPlanTechniqueSemanticDetail): List[String] =
    val subjectTokens =
      detail.structuralPurposeSubjects.flatMap(positionPlanTechniqueStructuralSubjectTokens)
    val routeAxisTokens =
      detail.structuralRouteMove.toList.flatMap(positionPlanTechniqueMoveAxisTokens)
    val hasTargetPurpose =
      detail.axisKind.contains(StrategicAxisKind.Target) ||
        detail.structuralPurposeCategories.exists(_.toLowerCase.contains("target"))
    val concreteTargetTokens =
      Option
        .when(hasTargetPurpose)(detail.structuralPurposeSubjects.flatMap(positionPlanTechniqueConcreteTargetTokens))
        .getOrElse(Nil)
    val targetTokens =
      Option
        .when(detail.structuralPurposeCategories.exists(_.toLowerCase.contains("target")))(
          List("target", "routeTarget")
        )
        .getOrElse(Nil)
    (subjectTokens ++ routeAxisTokens ++ concreteTargetTokens ++ targetTokens).distinct.sorted

  private def positionPlanTechniqueStructuralSubjectTokens(subject: String): List[String] =
    val normalized = subject.toLowerCase
    val pieceRoute = """([a-z]+):([a-h][1-8])-([a-h][1-8]).*""".r
    val outpost = """outpost:([a-z]+):([a-h][1-8]).*""".r
    val weakSquare = """weak-square:([a-h][1-8]).*""".r
    val pieceRestriction = """([a-z]+):([a-h][1-8]):diagonal-denial:blocked-by:([a-h][1-8]).*""".r
    val battery = """battery:([a-z]+):([a-h][1-8])-([a-h][1-8])(?::([a-z-]+))?.*""".r
    val rookLift = """rook-lift:([a-h][1-8])-([a-h][1-8]):rank-([0-9]+).*""".r
    normalized match
      case outpost(piece, square) =>
        List(
          "piece",
          s"piece:$piece",
          piece,
          s"structuralPurposeSubject:$piece",
          "route",
          "outpost",
          s"targetSquare:$square",
          s"objectTarget:Square:$square"
        )
      case weakSquare(square) =>
        List(
          "weak-square",
          "target",
          "routeTarget",
          s"targetSquare:$square",
          s"objectTarget:Square:$square"
        )
      case pieceRoute(piece, from, to) =>
        val routeTokens =
          List(
            "piece",
            s"piece:$piece",
            piece,
            "route",
            "reroute",
            s"route:$from-$to"
          )
        routeTokens ++ positionPlanTechniqueMoveAxisTokens(from + to)
      case rookLift(from, to, rank) =>
        List(
          "piece",
          "piece:rook",
          "rook",
          "route",
          "reroute",
          "rook-lift",
          s"route:$from-$to",
          s"routeRank:$rank"
        ) ++ positionPlanTechniqueMoveAxisTokens(from + to)
      case pieceRestriction(piece, square, blocker) =>
        List(
          "piece",
          s"piece:$piece",
          piece,
          s"structuralPurposeSubject:$piece",
          "restriction",
          "diagonal-denial",
          "diagonal",
          "axis:Diagonal",
          s"targetSquare:$square",
          s"blockedSquare:$blocker",
          s"objectTarget:Square:$square",
          s"objectTarget:Square:$blocker"
        )
      case battery(axis, from, to, roles) =>
        val axisTokens =
          axis match
            case "diagonal" => List("diagonal", "axis:Diagonal")
            case "file"     => List("file", "axis:File")
            case "rank"     => List("rank", "axis:Rank")
            case other      => List(other)
        val geometryAxisTokens =
          positionPlanTechniqueMoveAxisTokens(from + to)
        val compatibleGeometryAxisTokens =
          axisTokens.find(_.startsWith("axis:")) match
            case Some(declaredAxis) => geometryAxisTokens.filter(_ == declaredAxis)
            case None               => geometryAxisTokens
        val roleTokens =
          Option(roles).toList
            .flatMap(_.split("-").toList)
            .filter(_.nonEmpty)
            .distinct
            .flatMap(role => List("piece", role, s"piece:$role", s"structuralPurposeSubject:$role"))
        List("battery", s"battery:$axis", s"route:$from-$to", s"targetSquare:$from", s"targetSquare:$to") ++
          axisTokens ++
          roleTokens ++
          compatibleGeometryAxisTokens
      case _ =>
        Nil

  private def positionPlanTechniqueConcreteTargetTokens(subject: String): List[String] =
    val normalized = subject.toLowerCase.trim
    if normalized.matches("[a-h][1-8]") then
      List(s"targetSquare:$normalized", s"objectTarget:Square:$normalized")
    else Nil

  private def positionPlanTechniqueMoveAxisTokens(move: String): List[String] =
    val normalized = move.toLowerCase
    if normalized.matches("[a-h][1-8][a-h][1-8].*") then
      val fromFile = normalized.charAt(0) - 'a'
      val fromRank = normalized.charAt(1) - '1'
      val toFile = normalized.charAt(2) - 'a'
      val toRank = normalized.charAt(3) - '1'
      val fileDelta = (toFile - fromFile).abs
      val rankDelta = (toRank - fromRank).abs
      if fileDelta == rankDelta && fileDelta > 0 then List("diagonal", "axis:Diagonal")
      else if fileDelta == 0 && rankDelta > 0 then List("file", "axis:File")
      else if rankDelta == 0 && fileDelta > 0 then List("rank", "axis:Rank")
      else Nil
    else Nil

  private def positionPlanTechniqueObjectBindingTokens(signature: String): List[String] =
    val parts = signature
      .split("\\|")
      .toList
    (
      parts.collect {
        case part if part.startsWith("actor=")       => s"objectActor:${part.stripPrefix("actor=")}"
        case part if part.startsWith("target=")      => s"objectTarget:${part.stripPrefix("target=")}"
        case part if part.startsWith("mechanism=")   => s"objectMechanism:${part.stripPrefix("mechanism=")}"
        case part if part.startsWith("consequence=") => s"objectConsequence:${part.stripPrefix("consequence=")}"
        case part if part.startsWith("witness=")     => s"objectWitness:${part.stripPrefix("witness=")}"
      } ++ parts.flatMap(positionPlanTechniquePawnAdvanceBreakFileTokens)
    ).distinct

  private def positionPlanTechniquePawnAdvanceBreakFileTokens(part: String): List[String] =
    val normalized = part.toLowerCase
    val pattern = """.*pawnadvance\([^)]*some\(([a-h][1-8][a-h][1-8][a-z]?)\).*""".r
    normalized match
      case pattern(move) =>
        val file = move.take(1)
        List(s"breakFile:$file", s"break-file-$file")
      case _ =>
        Nil

  private def relativeCauseMatchesComparison(cause: RelativeCauseFact, fact: CandidateComparisonFact): Boolean =
    cause.comparisonKind == fact.kind &&
      cause.referenceLine == fact.referenceLine &&
      cause.candidateLine == fact.candidateLine

  private def projectedContextCauseNoViewIds(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      contextCauseEvidenceIds: Set[String]
  ): List[String] =
    relativeCauseDiagnosticRecords(packet)
      .collect { case record if contextCauseProjectionExpected(record.cause, fact) => record.id }
      .filterNot(contextCauseEvidenceIds.contains)
      .distinct
      .sorted

  private def contextCauseProjectionExpected(cause: RelativeCauseFact, fact: CandidateComparisonFact): Boolean =
    cause.comparisonKind == fact.kind &&
      cause.referenceLine == fact.referenceLine &&
      cause.candidateLine == fact.candidateLine &&
      MoveJudgmentView.playedAlternativeLongTermContext(cause)

  private def comparisonCauseFrames(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      frames: MoveJudgmentView => List[MoveJudgmentCauseFrame]
  ): List[MoveJudgmentCauseFrame] =
    packet.moveJudgmentView.toList.flatMap(view => frames(view).filter(frame => frameMatchesComparison(frame, fact)))

  private def frameMatchesComparison(frame: MoveJudgmentCauseFrame, fact: CandidateComparisonFact): Boolean =
    frame.comparisonKind == fact.kind &&
      frame.referenceLine == fact.referenceLine &&
      frame.candidateLine == fact.candidateLine

  private def frameIdeaFamilies(
      packet: EvidenceBackedJudgmentPacket,
      frames: List[MoveJudgmentCauseFrame]
  ): Set[ChessIdeaFamily] =
    val ideasById = packet.ideas.map(idea => idea.ref.id -> idea).toMap
    frames
      .flatMap(frame => frame.ideaIds ++ frame.supportIdeaIds)
      .flatMap(id => ideasById.get(id).map(_.ref.family))
      .toSet

  private def frameClaimCandidateFamilies(
      packet: EvidenceBackedJudgmentPacket,
      frames: List[MoveJudgmentCauseFrame]
  ): Set[ClaimFamily] =
    val lifecycleByCandidateId = packet.claimLifecycle.map(diagnostic => diagnostic.candidateId -> diagnostic).toMap
    frames
      .flatMap(_.claimCandidateIds)
      .flatMap(id => lifecycleByCandidateId.get(id).map(_.family))
      .toSet

  private def frameFinalClaimFamilies(
      packet: EvidenceBackedJudgmentPacket,
      frames: List[MoveJudgmentCauseFrame]
  ): Set[ClaimFamily] =
    val claimsById = packet.claims.map(claim => claim.id -> claim).toMap
    frames
      .flatMap(_.finalClaimIds)
      .flatMap(id => claimsById.get(id).map(_.family))
      .toSet

  private def relativeCauseValidationIssueIds(
      validation: JudgmentPacketValidationResult
  ): Map[JudgmentPacketValidationIssueKind, Set[String]] =
    validation.issues
      .groupMap(_.kind)(_.subjectId)
      .view
      .mapValues(_.toSet)
      .toMap

  private def causeSupportDiagnostics(
      causeRecords: List[RelativeCauseDiagnosticRecord],
      graph: TypedEvidenceGraph
  ): List[CandidateCauseSupportDiagnostic] =
    causeRecords.map { entry =>
      val record = entry.record
      val cause = entry.cause
      val parentRecords = record.parents.flatMap(parent => graph.byId.get(parent.id))
      val parentLayers = parentRecords.map(_.ref.layer).distinctBy(_.ordinal).sortBy(_.ordinal)
      val proof = cause.proof.getOrElse(RelativeCauseProof())
      val depthProof = proof.depthProof
      val strategicProof = depthProof.strategicProofIdentity
      val semanticKinds = semanticSupportKinds(cause.kind, parentRecords, proof)
      CandidateCauseSupportDiagnostic(
        id = entry.id,
        kind = cause.kind,
        role = cause.role,
        sourceSide = cause.sourceSide,
        importance = cause.importance,
        eventLine = cause.eventLine,
        evidenceLines = cause.evidenceLines,
        supportEvidenceIds = cause.supportEvidence.map(_.id).distinct.sorted,
        parentEvidenceIds = record.parents.map(_.id).distinct.sorted,
        parentLayers = parentLayers,
        parentLayerSignature = parentLayers.map(_.toString).mkString("+"),
        semanticSupportKinds = semanticKinds,
        semanticSupportSignature = semanticKinds.mkString("+"),
        hasOwnedTypedDepth = cause.hasOwnedTypedDepth,
        hasOwnedTacticalProof = cause.hasOwnedTacticalProof,
        hasOwnedStrategicContrastDepth = cause.hasOwnedStrategicContrastDepth,
        hasOwnedAdmissibleLongTermProof = cause.hasOwnedAdmissibleLongTermProof,
        rawProofHasDirectProof = proof.hasRawDirectProof,
        rawProofHasContrastProof = proof.hasRawContrastProof,
        rawProofHasContextSupport = proof.hasRawContextSupport,
        directProofSourceIds = proof.directProof.sourceRefs.map(_.id).distinct.sorted,
        contrastProofSourceIds = proof.contrastProof.sourceRefs.map(_.id).distinct.sorted,
        contextSupportSourceIds = proof.contextSupport.sourceRefs.map(_.id).distinct.sorted,
        directProofKinds = proof.directProof.kindLabels,
        contrastProofKinds = proof.contrastProof.kindLabels,
        contextSupportKinds = proof.contextSupport.kindLabels,
        proofBoardAnchors = depthProof.boardAnchors,
        proofLineEvents = depthProof.lineEvents,
        proofLineConsequences = depthProof.lineConsequences,
        proofRelationKinds = depthProof.relationKinds,
        proofRelationDetails = depthProof.relationDetails,
        proofRelationSourceIds = depthProof.relationProofs.map(_.source.id).distinct.sorted,
        proofTacticalMechanismKinds = depthProof.tacticalMechanisms.map(_.kind).distinct,
        proofTacticalMechanismSourceIds = depthProof.tacticalMechanisms.map(_.source.id).distinct.sorted,
        proofStrategicAxisKeys = strategicProof.axisKeys,
        proofStrategicMechanismKinds = strategicProof.mechanismKinds,
        proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
        proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
        proofTransitionConsequences = depthProof.transitionConsequences,
        causeContextLayers = proof.contextLayers
      )
    }

  private def relativeCauseDiagnosticsFor(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      signalProfile: RelativeCauseSignalProfile,
      causeRecords: List[RelativeCauseDiagnosticRecord],
      causeSupport: List[CandidateCauseSupportDiagnostic],
      missingExpectedCauseHints: List[RelativeCauseKind],
      validationIssueIds: Map[JudgmentPacketValidationIssueKind, Set[String]]
  ): ComparisonRelativeCauseDiagnostics =
    val producedCauseIds = causeRecords.map(_.id).distinct.sorted
    val producedCauseKinds = causeRecords.map(_.cause.kind).distinct
    val producedCauseRoles = causeRecords.map(_.cause.role).distinct
    val producedCauseSourceSides = causeRecords.map(_.cause.sourceSide).distinct
    val producedCauseEventLines = causeRecords.map(_.cause.eventLine).distinct
    val expectedCauseHints = (producedCauseKinds ++ missingExpectedCauseHints).distinct
    val shallowProofCauseIds =
      causeSupport.filterNot(_.hasOwnedTypedDepth).map(_.id).distinct.sorted
    val genericCauseIds =
      causeSupport
        .filter(_.semanticSupportKinds.contains("GenericComparisonOnly"))
        .map(_.id)
        .distinct
        .sorted
    val ownedTypedDepthCauseIds =
      causeSupport.filter(_.hasOwnedTypedDepth).map(_.id).distinct.sorted
    val nonGenericCauseIds =
      producedCauseIds.filterNot(genericCauseIds.contains)
    val boundEvidenceIds =
      (
        causeRecords.flatMap(entry => entry.record.parents.map(_.id)) ++
          causeRecords.flatMap(_.cause.supportEvidence.map(_.id)) ++
          producedCauseIds
      ).toSet
    val unboundEvidenceIds =
      RelativeCauseDraftPlanner
        .producedCauseBindableRecords(signalProfile)
        .map(_.ref.id)
        .distinct
        .filterNot(boundEvidenceIds.contains)
        .sorted
    val wrongRoleCauseIds =
      relativeCauseIssueCauseIds(causeRecords, validationIssueIds, JudgmentPacketValidationIssueKind.MismatchedRelativeCauseRole)
    val wrongSourceSideCauseIds =
      relativeCauseIssueCauseIds(causeRecords, validationIssueIds, JudgmentPacketValidationIssueKind.MismatchedRelativeCauseSourceSide)
    val wrongEventLineCauseIds =
      relativeCauseIssueCauseIds(causeRecords, validationIssueIds, JudgmentPacketValidationIssueKind.MismatchedRelativeCauseEventLine)
    val wrongImportanceCauseIds =
      relativeCauseIssueCauseIds(causeRecords, validationIssueIds, JudgmentPacketValidationIssueKind.MismatchedRelativeCauseImportance)
    val causeFlow = relativeCauseFlowDiagnostics(packet, causeRecords)
    val strategicClaimWithoutComparativeCauseIds =
      strategicClaimsWithoutComparativeCauseIds(packet, fact)
    val genericStructuralImprovementWithoutStrategicContrastIds =
      causeSupport
        .filter(support =>
          support.kind == RelativeCauseKind.StructuralImprovement &&
            !support.hasOwnedAdmissibleLongTermProof
        )
        .map(_.id)
        .distinct
        .sorted
    ComparisonRelativeCauseDiagnostics(
      expectedCauseHints = expectedCauseHints,
      missingExpectedCauseHints = missingExpectedCauseHints,
      producedCauseIds = producedCauseIds,
      producedCauseKinds = producedCauseKinds,
      producedCauseRoles = producedCauseRoles,
      producedCauseSourceSides = producedCauseSourceSides,
      producedCauseEventLines = producedCauseEventLines,
      missingCause = requiresExplanatoryCause(fact) && causeRecords.isEmpty,
      shallowProofCauseIds = shallowProofCauseIds,
      genericCauseIds = genericCauseIds,
      ownedTypedDepthCauseIds = ownedTypedDepthCauseIds,
      nonGenericCauseIds = nonGenericCauseIds,
      unboundEvidenceIds = unboundEvidenceIds,
      wrongRoleCauseIds = wrongRoleCauseIds,
      wrongSourceSideCauseIds = wrongSourceSideCauseIds,
      wrongEventLineCauseIds = wrongEventLineCauseIds,
      wrongImportanceCauseIds = wrongImportanceCauseIds,
      causeWithoutIdeaIds = causeFlow.filter(_.causeWithoutIdea).map(_.causeId).distinct.sorted,
      ideaWithoutClaimCandidateCauseIds = causeFlow.filter(_.ideaWithoutClaimCandidate).map(_.causeId).distinct.sorted,
      ideaWithoutFinalClaimCauseIds = causeFlow.filter(_.ideaWithoutFinalClaim).map(_.causeId).distinct.sorted,
      ideaWithoutClaimCauseIds =
        causeFlow.filter(flow => flow.ideaWithoutClaimCandidate || flow.ideaWithoutFinalClaim).map(_.causeId).distinct.sorted,
      claimWithoutEventClusterCauseIds = causeFlow.filter(_.claimWithoutEventCluster).map(_.causeId).distinct.sorted,
      eventClusterSupportMissingCauseIds =
        causeFlow.filter(_.eventClusterMissingSupportEvidenceIds.nonEmpty).map(_.causeId).distinct.sorted,
      strategicCauseWithoutContrastIds =
        causeFlow.filter(_.strategicCauseWithoutContrast).map(_.causeId).distinct.sorted,
      strategicClaimWithoutComparativeCauseIds = strategicClaimWithoutComparativeCauseIds,
      genericStructuralImprovementWithoutStrategicContrastIds = genericStructuralImprovementWithoutStrategicContrastIds,
      contextSupportUsedAsDirectProofIds =
        causeFlow.filter(_.contextSupportUsedAsDirectProof).map(_.causeId).distinct.sorted,
      contextOnlyCauseIds =
        causeFlow.filter(_.contextOnlyAttribution).map(_.causeId).distinct.sorted,
      unattributedCauseIds =
        causeFlow.filter(_.unattributedCause).map(_.causeId).distinct.sorted,
      rootMismatchedCauseIds =
        causeFlow.filter(_.rootMismatchedAttribution).map(_.causeId).distinct.sorted,
      supportPromotedToDirectProofCauseIds =
        causeFlow.filter(_.supportPromotedToDirectProof).map(_.causeId).distinct.sorted,
      relativeCauseWithoutObjectSignatureIds =
        causeFlow.filter(_.relativeCauseWithoutObjectSignature).map(_.causeId).distinct.sorted,
      objectLostBetweenEvidenceAndCauseIds =
        causeFlow.filter(_.objectLostBetweenEvidenceAndCause).map(_.causeId).distinct.sorted,
      objectLostBetweenCauseAndClaimIds =
        causeFlow.filter(_.objectLostBetweenCauseAndClaim).map(_.causeId).distinct.sorted,
      causeFlow = causeFlow
    )

  private def relativeCauseIssueCauseIds(
      causeRecords: List[RelativeCauseDiagnosticRecord],
      validationIssueIds: Map[JudgmentPacketValidationIssueKind, Set[String]],
      kind: JudgmentPacketValidationIssueKind
  ): List[String] =
    val issueIds = validationIssueIds.getOrElse(kind, Set.empty)
    causeRecords.collect {
      case entry if issueIds.contains(entry.id) || issueIds.contains(entry.record.ref.id) =>
        entry.id
    }.distinct.sorted

  private def relativeCauseFlowDiagnostics(
      packet: EvidenceBackedJudgmentPacket,
      causeRecords: List[RelativeCauseDiagnosticRecord]
  ): List[RelativeCauseFlowDiagnostic] =
    causeRecords.map { entry =>
      val ref = entry.record.ref
      val cause = entry.cause
      val eventClusterExpected = ClaimEventCluster.kindForCause(cause.kind).nonEmpty
      val exactLifecycleCandidates =
        packet.claimLifecycle.filter(diagnostic =>
          diagnostic.relativeCauses.exists(relativeCauseLifecycleMatches(_, entry))
        )
      val ideasFromEvidence =
        packet.ideas.filter(idea => idea.evidence.exists(_.id == ref.id))
      val ideaIds =
        (ideasFromEvidence.map(_.ref.id) ++ exactLifecycleCandidates.flatMap(_.ideaIds)).distinct.sorted
      val lifecycleCandidates = exactLifecycleCandidates
      val finalLifecycleClaimIds =
        lifecycleCandidates.filter(_.finalPacketIncluded).map(_.claimId).toSet
      val claims =
        packet.claims.filter(claim =>
          claim.evidence.exists(_.id == ref.id) ||
            claim.ideaRefs.exists(idea => ideaIds.contains(idea.id)) ||
            finalLifecycleClaimIds.contains(claim.id)
        )
      val claimIds = claims.map(_.id).distinct.sorted
      val eventClusters =
        packet.claimEventClusters.filter(cluster => eventClusterMatchesRelativeCause(cluster, cause))
      val eventClusterIds = eventClusters.map(_.id).distinct.sorted
      val missingSupportEvidenceIds =
        eventClusters.flatMap(cluster => eventClusterMissingSupportEvidenceIds(cluster, cause)).distinct.sorted
      val candidateStages = lifecycleCandidates.flatMap(_.stages).toSet
      val candidateDroppedStages =
        candidateStages.intersect(
          Set(
            ClaimLifecycleStage.TruthRejected,
            ClaimLifecycleStage.TruthDeferred,
            ClaimLifecycleStage.DedupeDropped,
            ClaimLifecycleStage.ArbitrationSuppressed
          )
        )
      val familyMismatch = familyMismatchDiagnosticFor(packet, entry)
      val proof = cause.proof.getOrElse(RelativeCauseProof())
      val strategicCauseWithoutContrast =
        cause.strategicCauseKind && !cause.hasOwnedAdmissibleLongTermProof
      val contextSupportUsedAsDirectProof =
        proofContextSupportUsedAsDirectProof(proof)
      val supportPromotedToDirectProof =
        relativeCauseSupportPromotedToDirectProof(cause, proof)
      val causeObjectBindings =
        EvidenceObjectBinding.fromRelativeCause(cause, packet.evidenceGraph)
      val causeObjectSignatures =
        EvidenceObjectBinding.objectSignatures(causeObjectBindings)
      val lowLevelObjectAvailable =
        EvidenceObjectBinding.lowLevelObjectAvailable(cause, packet.evidenceGraph)
      val claimObjectSignatures =
        claims.flatMap(claim => EvidenceObjectBinding.objectSignatures(EvidenceObjectBinding.fromClaim(claim, packet.evidenceGraph)))
      val relativeCauseWithoutObjectSignature =
        causeObjectSignatures.isEmpty
      val objectLostBetweenEvidenceAndCause =
        lowLevelObjectAvailable && relativeCauseWithoutObjectSignature
      val objectLostBetweenCauseAndClaim =
        causeObjectSignatures.nonEmpty && claimIds.nonEmpty && claimObjectSignatures.isEmpty
      RelativeCauseFlowDiagnostic(
        causeId = entry.id,
        causeKind = cause.kind,
        causeRole = cause.role,
        causeComparisonKind = cause.comparisonKind,
        causeSourceSide = cause.sourceSide,
        causeEventLine = cause.eventLine,
        proofStrategicAxisKeys = cause.strategicProofIdentity.axisKeys,
        familyMismatchKinds = familyMismatch.mismatchKinds,
        expectedIdeaFamilies = familyMismatch.expectedIdeaFamilies,
        actualIdeaFamilies = familyMismatch.actualIdeaFamilies,
        expectedClaimFamilies = familyMismatch.expectedClaimFamilies,
        claimCandidateFamilies = familyMismatch.claimCandidateFamilies,
        finalClaimFamilies = familyMismatch.finalClaimFamilies,
        directProofSourceIds = proof.directProof.sourceRefs.map(_.id).distinct.sorted,
        contrastProofSourceIds = proof.contrastProof.sourceRefs.map(_.id).distinct.sorted,
        contextSupportSourceIds = proof.contextSupport.sourceRefs.map(_.id).distinct.sorted,
        directProofKinds = proof.directProof.kindLabels.distinct.sorted,
        contrastProofKinds = proof.contrastProof.kindLabels.distinct.sorted,
        contextSupportKinds = proof.contextSupport.kindLabels.distinct.sorted,
        hasOwnedTypedDepth = cause.hasOwnedTypedDepth,
        hasOwnedAdmissibleLongTermProof = cause.hasOwnedAdmissibleLongTermProof,
        eventClusterExpected = eventClusterExpected,
        ideaIds = ideaIds,
        claimCandidateIds = lifecycleCandidates.map(_.candidateId).distinct.sorted,
        claimCandidateStages = candidateStages,
        claimCandidateTruthStatuses = lifecycleCandidates.flatMap(_.truthStatus).toSet,
        claimCandidateDroppedStages = candidateDroppedStages,
        claimIds = claimIds,
        eventClusterIds = eventClusterIds,
        eventClusterMissingSupportEvidenceIds = missingSupportEvidenceIds,
        causeWithoutIdea = ideaIds.isEmpty,
        ideaWithoutClaimCandidate = ideaIds.nonEmpty && lifecycleCandidates.isEmpty,
        ideaWithoutFinalClaim = ideaIds.nonEmpty && lifecycleCandidates.nonEmpty && claimIds.isEmpty,
        claimWithoutEventCluster = eventClusterExpected && claimIds.nonEmpty && eventClusterIds.isEmpty,
        strategicCauseWithoutContrast = strategicCauseWithoutContrast,
        contextSupportUsedAsDirectProof = contextSupportUsedAsDirectProof,
        contextOnlyAttribution = cause.attribution.contextOnly,
        unattributedCause = cause.attribution.unattributed,
        rootMismatchedAttribution = cause.attribution.rootMismatch,
        supportPromotedToDirectProof = supportPromotedToDirectProof,
        objectBindingSignatures = causeObjectSignatures,
        relativeCauseWithoutObjectSignature = relativeCauseWithoutObjectSignature,
        objectLostBetweenEvidenceAndCause = objectLostBetweenEvidenceAndCause,
        objectLostBetweenCauseAndClaim = objectLostBetweenCauseAndClaim
      )
    }

  private def proofContextSupportUsedAsDirectProof(proof: RelativeCauseProof): Boolean =
    val contextIds = proof.contextSupport.sourceRefs.map(_.id).toSet
    contextIds.nonEmpty &&
      (proof.directProof.sourceRefs.exists(ref => contextIds.contains(ref.id)) ||
        proof.contrastProof.sourceRefs.exists(ref => contextIds.contains(ref.id)))

  private def relativeCauseSupportPromotedToDirectProof(
      cause: RelativeCauseFact,
      proof: RelativeCauseProof
  ): Boolean =
    val directIds = proof.directProof.sourceRefs.map(_.id).toSet
    directIds.nonEmpty && !cause.attribution.directProofEligible

  private def strategicClaimsWithoutComparativeCauseIds(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact
  ): List[String] =
    packet.claims
      .filter(claim => claim.family == ClaimFamily.Strategic || claim.family == ClaimFamily.PawnStructure || claim.family == ClaimFamily.Plan)
      .filter(claim => claimReferencesComparison(packet, claim, fact))
      .filterNot(claim => claimHasStrategicContrastCause(packet, claim))
      .map(_.id)
      .distinct
      .sorted

  private def claimReferencesComparison(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed,
      fact: CandidateComparisonFact
  ): Boolean =
    claim.evidence
      .flatMap(ref => packet.evidenceGraph.byId.get(ref.id))
      .exists {
        case EvidenceRecord(_, CandidateComparisonEvidence(candidateFact), _) =>
          candidateFact.kind == fact.kind &&
            candidateFact.referenceLine == fact.referenceLine &&
            candidateFact.candidateLine == fact.candidateLine
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          causeMatchesComparison(cause, fact)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(cause => causeMatchesComparison(cause, fact))
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          payload.comparisonKind == fact.kind &&
            payload.referenceLine == fact.referenceLine &&
            payload.candidateLine == fact.candidateLine
        case _ =>
          false
      }

  private def claimHasStrategicContrastCause(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): Boolean =
    claim.evidence
      .flatMap(ref => packet.evidenceGraph.byId.get(ref.id))
      .exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          cause.hasOwnedAdmissibleLongTermProof
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(_.hasOwnedAdmissibleLongTermProof)
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          StrategicMechanismContrastEvidence.hasActionableContrastOrSameRootCarrier(payload, packet.evidenceGraph.records)
        case _ =>
          false
      }

  private def eventClusterMatchesRelativeCause(
      cluster: ClaimEventCluster,
      cause: RelativeCauseFact
  ): Boolean =
    cluster.causeKind == cause.kind &&
      cluster.comparisonKind == cause.comparisonKind &&
      cluster.causeRole == cause.role &&
      cluster.causeSourceSide == cause.sourceSide &&
      cluster.causeImportance == cause.importance &&
      cluster.attributionKind == cause.attribution.kind &&
      cluster.attributionRootMoveMatched == cause.attribution.rootMoveMatched &&
      cluster.attributionDirectProofEligible == cause.attribution.directProofEligible &&
      cluster.referenceLine == cause.referenceLine &&
      cluster.candidateLine == cause.candidateLine &&
      cluster.eventLine == cause.eventLine &&
      eventClusterCarriesRelativeCauseProof(cluster, cause)

  private def eventClusterCarriesRelativeCauseProof(
      cluster: ClaimEventCluster,
      cause: RelativeCauseFact
  ): Boolean =
    eventClusterCauseProofsForCause(cluster, cause).nonEmpty

  private def eventClusterMissingSupportEvidenceIds(
      cluster: ClaimEventCluster,
      cause: RelativeCauseFact
  ): List[String] =
    val supportIds = cause.supportEvidence.map(_.id).distinct.sorted
    if supportIds.isEmpty then Nil
    else
      val carriedSupportIds =
        eventClusterCauseProofsForCause(cluster, cause)
          .flatMap(_.supportEvidenceSourceIds)
          .toSet
      supportIds.filterNot(carriedSupportIds.contains)

  private def eventClusterCauseProofsForCause(
      cluster: ClaimEventCluster,
      cause: RelativeCauseFact
  ): List[ClaimEventCauseProof] =
    cluster.causeProofs
      .filter(causeProofMatchesRelativeCauseIdentity(_, cause))
      .filter(causeProofCarriesRelativeCauseProof(_, cause))

  private def causeProofMatchesRelativeCauseIdentity(
      causeProof: ClaimEventCauseProof,
      cause: RelativeCauseFact
  ): Boolean =
    causeProof.causeKind == cause.kind &&
      causeProof.comparisonKind == cause.comparisonKind &&
      causeProof.causeRole == cause.role &&
      causeProof.causeSourceSide == cause.sourceSide &&
      causeProof.causeImportance == cause.importance &&
      causeProof.attributionKind == cause.attribution.kind &&
      causeProof.attributionRootMoveMatched == cause.attribution.rootMoveMatched &&
      causeProof.attributionDirectProofEligible == cause.attribution.directProofEligible &&
      causeProof.referenceLine == cause.referenceLine &&
      causeProof.candidateLine == cause.candidateLine &&
      causeProof.eventLine == cause.eventLine &&
      causeProof.supportEvidenceSourceIds.toSet == cause.supportEvidence.map(_.id).toSet

  private def causeProofCarriesRelativeCauseProof(
      causeProof: ClaimEventCauseProof,
      cause: RelativeCauseFact
  ): Boolean =
    cause.proof match
      case Some(proof) =>
        val strategicProof = proof.strategicProofIdentity
        sourceIdsMatch(proof.directProof.sourceRefs, causeProof.proofDirectSourceIds) &&
          sourceIdsMatch(proof.contrastProof.sourceRefs, causeProof.proofContrastSourceIds) &&
          sourceIdsMatch(proof.contextSupport.sourceRefs, causeProof.proofContextSupportSourceIds) &&
          strategicProof.axisKeys == causeProof.proofStrategicAxisKeys &&
          strategicProof.mechanismKinds == causeProof.proofStrategicMechanismKinds &&
          strategicProof.mechanismSourceIds == causeProof.proofStrategicMechanismSourceIds &&
          strategicProof.signalSourceIds == causeProof.proofStrategicMechanismSignalSourceIds &&
          proof.directProof.kindLabels.toSet == causeProof.proofDirectKinds.toSet &&
          proof.contrastProof.kindLabels.toSet == causeProof.proofContrastKinds.toSet &&
          proof.contextSupport.kindLabels.toSet == causeProof.proofContextSupportKinds.toSet
      case None =>
        causeProof.proofDirectSourceIds.isEmpty &&
          causeProof.proofContrastSourceIds.isEmpty &&
          causeProof.proofContextSupportSourceIds.isEmpty &&
          causeProof.proofStrategicAxisKeys.isEmpty &&
          causeProof.proofStrategicMechanismKinds.isEmpty &&
          causeProof.proofStrategicMechanismSourceIds.isEmpty &&
          causeProof.proofStrategicMechanismSignalSourceIds.isEmpty &&
          causeProof.proofDirectKinds.isEmpty &&
          causeProof.proofContrastKinds.isEmpty &&
          causeProof.proofContextSupportKinds.isEmpty

  private def sourceIdsMatch(sourceRefs: List[EvidenceRef], carriedIds: List[String]): Boolean =
    sourceRefs.map(_.id).toSet == carriedIds.toSet

  private def semanticSupportKinds(
      kind: RelativeCauseKind,
      parentRecords: List[EvidenceRecord],
      proof: RelativeCauseProof
  ): List[String] =
    kind match
      case RelativeCauseKind.OnlyMoveNecessity =>
        val kinds = onlyMoveNecessitySupportKinds(parentRecords) ++ typedProofSupportKinds(proof)
        if kinds.nonEmpty then kinds.distinct.sorted else List("GenericComparisonOnly")
      case RelativeCauseKind.OnlyDefenseNecessity | RelativeCauseKind.DefensiveResource =>
        val kinds = defensiveResourceSupportKinds(parentRecords) ++ typedProofSupportKinds(proof)
        if kinds.nonEmpty then kinds.distinct.sorted else List("GenericComparisonOnly")
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        val kinds = materialSupportKinds(parentRecords) ++ typedProofSupportKinds(proof)
        if kinds.nonEmpty then kinds.distinct.sorted else List("GenericComparisonOnly")
      case causeKind if tacticalCause(causeKind) =>
        val kinds = tacticalSupportKinds(causeKind, parentRecords) ++ typedProofSupportKinds(proof)
        if kinds.nonEmpty then kinds.distinct.sorted else List("GenericComparisonOnly")
      case causeKind if strategicCause(causeKind) =>
        val kinds = strategicSupportKinds(parentRecords) ++ typedProofSupportKinds(proof)
        if kinds.nonEmpty then kinds.distinct.sorted else List("GenericComparisonOnly")
      case _ =>
        Nil

  private def typedProofSupportKinds(proof: RelativeCauseProof): List[String] =
    proof.directProof.kindLabels.map(kind => s"Direct:$kind") ++
      proof.contrastProof.kindLabels.map(kind => s"Contrast:$kind")

  private def onlyMoveNecessitySupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    val comparisonKinds = parentRecords.collect { case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
      List(
        Option.when(fact.kind == CandidateComparisonKind.BestVsSecond)("BestVsSecond"),
        Option.when(fact.comparison.candidateSet.exists(_.onlyMove))("OnlyMoveGap"),
        fact.comparison.candidateSet
          .flatMap(_.bestToSecondWinPercentGapForMover)
          .map(gap => s"BestSecondWinPercentGap:${winPercentGapBand(gap)}"),
        fact.comparison.candidateSet
          .flatMap(_.rawBestToSecondCpGapForDiagnostics)
          .map(gap => s"BestSecondCpGap:${cpGapBand(gap)}"),
        Option.when(fact.comparison.candidateSet.exists(_.candidateCount >= 3))("MultiCandidateSet")
      ).flatten
    }.flatten
    val lineKinds =
      List(
        Option.when(parentRecords.exists { case EvidenceRecord(_, _: LineFactEvidence, _) => true; case _ => false })(
          "LineBacked"
        ),
        Option.when(parentRecords.exists { case EvidenceRecord(_, EvalFactEvidence(_, _, _, depth), _) => depth > 0; case _ => false })(
          "EngineEvalBacked"
        )
      ).flatten
    (comparisonKinds ++ lineKinds).distinct

  private def winPercentGapBand(gap: Double): String =
    if gap >= JudgmentThresholds.BLUNDER_WP then "blunder"
    else if gap >= JudgmentThresholds.ONLY_MOVE_GAP_WP then "only-move"
    else if gap >= JudgmentThresholds.INACCURACY_WP then "inaccuracy"
    else "playable"

  private def cpGapBand(gap: Int): String =
    val abs = gap.abs
    if abs >= 300 then "300+"
    else if abs >= 150 then "150+"
    else if abs >= 75 then "75+"
    else "small"

  private def defensiveResourceSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    List(
      Option.when(parentRecords.exists {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.canAnchorTacticalIdea || payload.canAnchorDefensiveIdea
        case _ =>
          false
      })("MateOrTacticalRisk"),
      Option.when(parentRecords.exists {
        case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
          payload.onlyDefense.nonEmpty
        case _ =>
          false
      })("OnlyDefense"),
      Option.when(parentRecords.exists {
        case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
          !payload.insufficientData && payload.defenseRequired
        case _ =>
          false
      })("ThreatDefenseRequired"),
      Option.when(parentRecords.exists {
        case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
          !payload.insufficientData && payload.maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP)
        case _ =>
          false
      })("MaterialThreatIfIgnored"),
      Option.when(hasProphylacticResource(parentRecords))("Prophylaxis"),
      Option.when(hasKingStepResource(parentRecords))("KingStep"),
      Option.when(hasCastlingResource(parentRecords))("Castling"),
      Option.when(hasPreventivePawnResource(parentRecords))("PreventivePawn")
    ).flatten

  private def materialSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    parentRecords.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
      payload.proofSignalConsequenceKinds.map(kind => s"LineConsequence:$kind") ++
        payload.materialOutcomeProfile.gainSignals.map(signal => s"MaterialGainSignal:$signal").toList ++
        payload.materialOutcomeProfile.lossSignals.map(signal => s"MaterialLossSignal:$signal").toList ++
        Option.when(payload.hasMaterialRecaptureChain)("LineMaterial:RecaptureChain").toList ++
        Option.when(payload.hasMaterialRecoveryWindow)("LineMaterial:RecoveryWindow").toList
    }.flatten.distinct.sorted

  private def strategicSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    val mechanismKinds = parentRecords.collect { case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
      s"StrategicMechanism:${payload.kind}" :: payload.signals.map(signal => s"StrategicMechanismSignal:${signal.kind}:${signal.label}")
    }.flatten
    val contrastKinds = parentRecords.collect { case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
      s"StrategicMechanismContrast:${payload.comparisonKind}" ::
        payload.actionableComparisons.map(axis => s"StrategicAxisContrast:${axis.axisKey}:${axis.outcome}")
    }.flatten
    val structuralKinds = parentRecords.collect { case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
      payload.meaningfulConsequences.map(consequence => s"StructuralMeaningfulConsequence:${consequence.anchorKey}")
    }.flatten
    val boardKinds = parentRecords.collect { case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
      List(
        Option.when(payload.endgameTechniqueAnchors.nonEmpty)("BoardAnchor:EndgameTechnique"),
        Option.when(payload.looseMaterialAnchors.nonEmpty)("BoardAnchor:LooseMaterial")
      ).flatten
    }.flatten
    val pawnStructureKinds = parentRecords.collect { case EvidenceRecord(_, PawnStructureFactEvidence(_, alignment, pawnPlay), _) =>
      List(
        Option.when(alignment.exists(alignment => alignment.band == AlignmentBand.OnBook || alignment.band == AlignmentBand.Playable))(
          "PawnStructureAlignment"
        ),
        Option.when(pawnPlay.exists(_.primaryDriver != PawnPlayDriver.Quiet))("PawnPlayDriver")
      ).flatten
    }.flatten
    val lineKinds =
      List(Option.when(hasDevelopmentChoice(parentRecords) && hasCastlingLine(parentRecords))("CastlingPath")).flatten
    (contrastKinds ++ mechanismKinds ++ structuralKinds ++ boardKinds ++ pawnStructureKinds ++ lineKinds).distinct

  private def hasDevelopmentChoice(parentRecords: List[EvidenceRecord]): Boolean =
    parentRecords.exists {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) => payload.developmentChoices.nonEmpty
      case _                                                    => false
    }

  private def hasCastlingLine(parentRecords: List[EvidenceRecord]): Boolean =
    parentRecords.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasLineEvent(LineEventKind.Castling)
      case _ =>
        false
    }

  private def tacticalSupportKinds(
      kind: RelativeCauseKind,
      parentRecords: List[EvidenceRecord]
  ): List[String] =
    (
      tacticalRelationSupportKinds(parentRecords) ++
        tacticalMechanismSupportKinds(parentRecords) ++
        tacticalMotifSupportKinds(parentRecords) ++
        tacticalLineSupportKinds(parentRecords) ++
        tacticalEvalSupportKinds(parentRecords) ++
        tacticalCauseShapeSupportKinds(kind, parentRecords)
    ).distinct

  private def tacticalRelationSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    parentRecords.collect { case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
      List(
        Some(s"Relation:${payload.kind}"),
        Option.when(payload.hasLineProof)("RelationLineProof"),
        Option.when(payload.hasConcreteRelationProof)("RelationConcreteProof")
      ).flatten
    }.flatten

  private def tacticalMechanismSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    parentRecords.collect { case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
      List(
        Some(s"TacticalMechanism:${payload.kind}"),
        Option.when(payload.hasLineProof)("MechanismLineProof"),
        Option.when(payload.hasThreatProof)("MechanismThreatProof"),
        Option.when(payload.hasEngineOrForcingProof)("MechanismForcingProof")
      ).flatten
    }.flatten

  private def tacticalMotifSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    parentRecords.collect { case EvidenceRecord(ref, payload: MoveMotifEvidence, _) =>
      List(
        Some(s"MoveMotif:${payload.proof.kind}"),
        Option.when(payload.recordLineBound(ref))("LineBoundMoveMotif"),
        TacticalMotifClassifier.rootMotif(payload).map(motif => s"RootMoveMotif:${motifSupportName(motif)}")
      ).flatten
    }.flatten

  private def tacticalLineSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    parentRecords.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
      payload.proofSignalConsequenceKinds.map(kind => s"LineConsequence:$kind")
    }.flatten

  private def tacticalEvalSupportKinds(parentRecords: List[EvidenceRecord]): List[String] =
    parentRecords.collect {
      case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) if mate.nonEmpty =>
        "MateLine"
    }

  private def tacticalCauseShapeSupportKinds(
      kind: RelativeCauseKind,
      parentRecords: List[EvidenceRecord]
  ): List[String] =
    kind match
      case RelativeCauseKind.WrongRecapturer
          if parentRecords.exists {
            case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
              payload.kind == TacticalMechanismKind.RecaptureChoice
            case _ =>
              false
          } =>
        List("RecaptureChoice")
      case RelativeCauseKind.RecaptureRecoveryWindow
          if materialSupportKinds(parentRecords).exists(kind =>
            kind == "LineMaterial:RecaptureChain" ||
              kind == "LineMaterial:RecoveryWindow" ||
              kind == "LineConsequence:RecaptureSequence" ||
              kind == "LineConsequence:RecoveryWindow"
          ) =>
        List("RecoveryWindowShape")
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss
          if parentRecords.exists(moveOrderSupportRecord) =>
        List("MoveOrderOrZwischenzug")
      case _ =>
        Nil

  private def moveOrderSupportRecord(record: EvidenceRecord): Boolean =
    record match
      case EvidenceRecord(_, payload: MoveMotifEvidence, _) =>
        payload.motif match
          case _: Motif.Zwischenzug => true
          case _                    => false
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.kind == RelationFactKind.Zwischenzug && payload.hasConcreteRelationProof
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasProofSignalConsequence(LineConsequenceKind.ImmediateReplyCheck)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.Tempo && payload.canAnchorTacticalIdea
      case _ =>
        false

  private def motifSupportName(motif: Motif): String =
    motif match
      case _: Motif.Check                => "Check"
      case _: Motif.DoubleCheck          => "DoubleCheck"
      case _: Motif.BackRankMate         => "BackRankMate"
      case _: Motif.MateNet              => "MateNet"
      case _: Motif.SmotheredMate        => "SmotheredMate"
      case _: Motif.Capture              => "Capture"
      case _: Motif.Zwischenzug          => "Zwischenzug"
      case _: Motif.Fork                 => "Fork"
      case _: Motif.Pin                  => "Pin"
      case _: Motif.Skewer               => "Skewer"
      case _: Motif.DiscoveredAttack     => "DiscoveredAttack"
      case _: Motif.RemovingTheDefender  => "RemovingTheDefender"
      case _: Motif.Deflection           => "Deflection"
      case _: Motif.Decoy                => "Decoy"
      case _: Motif.XRay                 => "XRay"
      case _: Motif.Overloading          => "Overloading"
      case _: Motif.Interference         => "Interference"
      case _: Motif.Clearance            => "Clearance"
      case _: Motif.TrappedPiece         => "TrappedPiece"
      case _: Motif.PawnPromotion        => "PawnPromotion"
      case _: Motif.PassedPawnPush       => "PassedPawnPush"
      case _: Motif.StalemateThreat      => "StalemateThreat"
      case _: Motif.WeakBackRank         => "WeakBackRank"
      case other                         => other.getClass.getSimpleName.replace("$", "")

  private def causeMatchesComparison(
      cause: RelativeCauseFact,
      fact: CandidateComparisonFact
  ): Boolean =
    cause.comparisonKind == fact.kind &&
      cause.referenceLine == fact.referenceLine &&
      cause.candidateLine == fact.candidateLine

  private def lineDiagnostic(
      packet: EvidenceBackedJudgmentPacket,
      line: LineNodeRef
  ): CandidateLineDiagnostic =
    val node = packet.candidateLines.find(_.ref == line)
    CandidateLineDiagnostic(
      id = line.id,
      rootMove = line.rootMove,
      role = line.role,
      rank = line.rank,
      moves = node.map(_.line.moves).getOrElse(Nil),
      whitePovEvalCp = node.map(_.whitePovEvalCp),
      mate = node.flatMap(_.mate),
      depth = node.map(_.depth)
    )

  private def comparisonFingerprint(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact
  ): String =
    val reference = lineDiagnostic(packet, fact.referenceLine)
    val candidate = lineDiagnostic(packet, fact.candidateLine)
    List(
      packet.positions.find(_.role == PositionNodeRole.Before).map(_.ref.fen).getOrElse(""),
      fact.kind.toString,
      fact.referenceLine.rootMove,
      fact.candidateLine.rootMove,
      reference.whitePovEvalCp.map(_.toString).getOrElse(""),
      reference.depth.map(_.toString).getOrElse(""),
      candidate.whitePovEvalCp.map(_.toString).getOrElse(""),
      candidate.depth.map(_.toString).getOrElse("")
    ).mkString("|")

  private def comparisonDedupeKey(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact
  ): String =
    val reference = lineDiagnostic(packet, fact.referenceLine)
    val candidate = lineDiagnostic(packet, fact.candidateLine)
    List(
      packet.positions.find(_.role == PositionNodeRole.Before).map(_.ref.fen).getOrElse(""),
      fact.referenceLine.rootMove,
      fact.candidateLine.rootMove,
      reference.whitePovEvalCp.map(_.toString).getOrElse(""),
      reference.depth.map(_.toString).getOrElse(""),
      candidate.whitePovEvalCp.map(_.toString).getOrElse(""),
      candidate.depth.map(_.toString).getOrElse("")
    ).mkString("|")

  private def evidenceNeighborhood(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord]
  ): EvidenceLayerNeighborhood =
    val directLayerCounts = layerCounts(records.map(_.ref.layer))
    val parentLayers =
      records
        .flatMap(_.parents)
        .map(parent => graph.byId.get(parent.id).map(_.ref.layer).getOrElse(parent.layer))
        .filter(causeInputLayer)
    EvidenceLayerNeighborhood(
      directLayers = directLayerCounts.keySet,
      directLayerCounts = directLayerCounts,
      parentLayerCounts = layerCounts(parentLayers)
    )

  private def comparisonNeighborhood(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact
  ): CandidateComparisonEvidenceNeighborhood =
    val referenceTransition = transitionForLine(packet, fact.referenceLine)
    val candidateTransition = transitionForLine(packet, fact.candidateLine)
    val referenceRecords =
      (
        recordsForLineEndpoint(packet, fact.referenceLine) ++
          recordsForTransition(packet, referenceTransition) ++
          recordsForAfterPosition(packet, referenceTransition)
      ).distinctBy(_.ref.id)
    val candidateRecords =
      (
        recordsForLineEndpoint(packet, fact.candidateLine) ++
          recordsForTransition(packet, candidateTransition) ++
          recordsForAfterPosition(packet, candidateTransition)
      ).distinctBy(_.ref.id)
    val rootContext =
      packet.positions
        .find(_.role == PositionNodeRole.Before)
        .toList
        .flatMap(position => recordsForRootContext(packet, position.ref))
    val parentRecords =
      (referenceRecords ++ candidateRecords)
        .flatMap(record => record.parents.flatMap(parent => packet.evidenceGraph.byId.get(parent.id)))
        .filter(record => causeInputLayer(record.ref.layer))
    CandidateComparisonEvidenceNeighborhood(
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords,
      sharedRecords = (rootContext ++ parentRecords).distinctBy(_.ref.id)
    )

  private def recordsForLineEndpoint(
      packet: EvidenceBackedJudgmentPacket,
      line: LineNodeRef
  ): List[EvidenceRecord] =
    packet.evidenceGraph.records.filter(record =>
      endpointLayer(record.ref.layer) &&
        record.referencesLine(line)
    )

  private def recordsForRootContext(
      packet: EvidenceBackedJudgmentPacket,
      root: PositionNodeRef
  ): List[EvidenceRecord] =
    packet.evidenceGraph.records.filter(record =>
      rootContextLayer(record.ref.layer) &&
        record.ref.position == root &&
        (record.ref.scope == EvidenceScope.BeforePosition || record.ref.scope == EvidenceScope.CurrentPosition)
    )

  private def recordsForTransition(
      packet: EvidenceBackedJudgmentPacket,
      transition: Option[MoveTransitionEdge]
  ): List[EvidenceRecord] =
    transition.toList.flatMap { edge =>
      packet.evidenceGraph.records.filter(record =>
        transitionLayer(record.ref.layer) &&
          record.ref.position == edge.from &&
          recordMatchesTransition(record, edge)
      )
    }

  private def recordsForAfterPosition(
      packet: EvidenceBackedJudgmentPacket,
      transition: Option[MoveTransitionEdge]
  ): List[EvidenceRecord] =
    transition.toList.flatMap(edge =>
      packet.evidenceGraph.records.filter(record =>
        afterPositionLayer(record.ref.layer) &&
          record.ref.position == edge.to
      )
    )

  private def transitionForLine(
      packet: EvidenceBackedJudgmentPacket,
      line: LineNodeRef
  ): Option[MoveTransitionEdge] =
    packet.transitions.find(edge =>
      edge.moveUci == line.rootMove &&
        line.role == edge.role.lineRole
    )

  private def recordMatchesTransition(record: EvidenceRecord, edge: MoveTransitionEdge): Boolean =
    record.payload match
      case MoveTransitionEvidence(moveUci, from, to) =>
        record.ref.scope == edge.role.scope &&
          moveUci == edge.moveUci &&
          from == edge.from &&
          to == edge.to
      case payload: StructuralDeltaEvidence =>
        payload.role == edge.role &&
          payload.moveUci == edge.moveUci &&
          payload.from == edge.from &&
          payload.to == edge.to
      case _: StrategicMechanismEvidence =>
        record.ref.scope == edge.role.scope &&
          (
            record.ref.line.exists(line => line.rootMove == edge.moveUci && line.role == edge.role.lineRole) ||
              record.parents.exists(_.id == edge.evidence.id)
          )
      case _ =>
        false

  private def endpointLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Line | EvidenceLayer.Eval | EvidenceLayer.MoveMotif | EvidenceLayer.Relation |
          EvidenceLayer.TacticalMechanism | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def rootContextLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.SinglePosition | EvidenceLayer.ThreatPressure |
          EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def transitionLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.MoveTransition | EvidenceLayer.StructuralDelta | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def afterPositionLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.SinglePosition | EvidenceLayer.ThreatPressure |
          EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def causeInputLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.CandidateComparison | EvidenceLayer.Counterfactual | EvidenceLayer.RelativeAssessment |
          EvidenceLayer.RelativeCause | EvidenceLayer.MoveVerdictCertification | EvidenceLayer.ChessIdea | EvidenceLayer.Claim =>
        false
      case _ =>
        true

  private def classifyFailure(
      fact: CandidateComparisonFact,
      causeKinds: List[RelativeCauseKind],
      evidenceLayers: ComparisonEvidenceLayerDiagnostics,
      trace: CandidateCauseDecisionTrace,
      viewDiagnostics: ComparisonMoveJudgmentViewDiagnostics,
      hasLowDepthCause: Boolean,
      hasGenericCause: Boolean,
      hasUnboundEvidence: Boolean
  ): CandidateComparisonFailureClass =
    val significantEnginePreference = CandidateComparisonDiagnostic.significantEnginePreference(fact)
    val requiresCause = CandidateComparisonDiagnostic.requiresExplanatoryCause(fact)
    val hasKnownCause =
      causeKinds.nonEmpty
    if viewDiagnostics.projectedContextCauseNoViewIds.nonEmpty || viewDiagnostics.playableLossPrimaryCauseEvidenceIds.nonEmpty then
      CandidateComparisonFailureClass.UnboundEvidence
    else if !significantEnginePreference && causeKinds.isEmpty && latentComparisonEvidence(trace) then
      CandidateComparisonFailureClass.LowSignalEnginePreference
    else if !significantEnginePreference && causeKinds.isEmpty then
      CandidateComparisonFailureClass.NoFailure
    else if !requiresCause then
      CandidateComparisonFailureClass.NoFailure
    else if hasKnownCause && hasLowDepthCause then
      CandidateComparisonFailureClass.LowDepthCause
    else if hasKnownCause && hasGenericCause then
      CandidateComparisonFailureClass.GenericCause
    else if hasKnownCause && hasUnboundEvidence then
      CandidateComparisonFailureClass.UnboundEvidence
    else if hasKnownCause then
      CandidateComparisonFailureClass.NoFailure
    else if contextAlternativeComparison(fact) then
      CandidateComparisonFailureClass.SecondaryContextGap
    else if missingLineOrEval(evidenceLayers) then
      CandidateComparisonFailureClass.MissingEvidence
    else if hasUnboundEvidence || hasAvailableExplanatoryEvidence(evidenceLayers) then
      CandidateComparisonFailureClass.UnboundEvidence
    else if causeKinds.isEmpty then
      CandidateComparisonFailureClass.FailedCauseTemplate
    else
      CandidateComparisonFailureClass.UnknownChessPattern

  private def missingLineOrEval(evidenceLayers: ComparisonEvidenceLayerDiagnostics): Boolean =
    List(evidenceLayers.reference, evidenceLayers.candidate).exists(neighborhood =>
      !neighborhood.directLayers.contains(EvidenceLayer.Line) ||
        !neighborhood.directLayers.contains(EvidenceLayer.Eval)
    )

  private def hasAvailableExplanatoryEvidence(evidenceLayers: ComparisonEvidenceLayerDiagnostics): Boolean =
    availableLayers(evidenceLayers).exists(explanatoryLayers.contains)

  private def decisionTraceFor(
      fact: CandidateComparisonFact,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): CandidateCauseDecisionTrace =
    val involvedRecords = (referenceRecords ++ candidateRecords).distinctBy(_.ref.id)
    val referenceLooseMaterial = hasLooseMaterialExploit(referenceRecords, sharedRecords)
    val candidateLooseMaterial = hasLooseMaterialExploit(candidateRecords, sharedRecords)
    val referenceStructuralConsequenceKinds = structuralImprovementConsequenceKinds(referenceRecords)
    val candidateStructuralConsequenceKinds = structuralImprovementConsequenceKinds(candidateRecords)
    val semanticAxisDiagnostics =
      semanticAxisDiagnosticsFor(referenceRecords, candidateRecords)
    val referenceSemanticAxisLead =
      semanticAxisDiagnostics.referenceLeadAxisCount > 0
    val referenceSemanticAxisCount =
      semanticAxisDiagnostics.referenceAxes.size
    val candidateSemanticAxisCount =
      semanticAxisDiagnostics.candidateAxes.size
    val materialSwing = hasMaterialSwingEvidence(referenceRecords, candidateRecords)
    val candidateRelations = relationKinds(candidateRecords)
    val referenceMechanisms = tacticalMechanismKinds(referenceRecords)
    val candidateMechanisms = tacticalMechanismKinds(candidateRecords)
    CandidateCauseDecisionTrace(
      badLoss = fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP,
      tacticalLoss = fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP,
      majorLoss = fact.comparison.winPercentLossForMover >= JudgmentThresholds.MATERIAL_THREAT_WP,
      candidateBetter = fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP,
      requiresExplanatoryCause = requiresExplanatoryCause(fact),
      positiveContextAlternative = positiveContextAlternative(fact),
      referenceTacticalMechanismKinds = referenceMechanisms,
      candidateTacticalMechanismKinds = candidateMechanisms,
      referenceConcreteLine = EvidenceRecord.hasConcreteLineSignal(referenceRecords),
      candidateConcreteLine = EvidenceRecord.hasConcreteLineSignal(candidateRecords),
      candidateTacticalRefutationBridge =
        candidateConcreteTacticalBridge(referenceRecords) ||
          candidateConcreteTacticalBridge(candidateRecords),
      referenceTacticalRisk = referenceHasTacticalRisk(referenceRecords),
      hasOnlyDefense = hasOnlyDefense(involvedRecords),
      hasThreatResource = hasThreatResource(involvedRecords),
      referenceProphylacticResource = hasProphylacticResource(referenceRecords),
      referenceKingStepResource = hasKingStepResource(referenceRecords),
      candidateKingStepResource = hasKingStepResource(candidateRecords),
      referenceCastlingResource = hasCastlingResource(referenceRecords),
      candidateCastlingResource = hasCastlingResource(candidateRecords),
      referencePreventivePawnResource = hasPreventivePawnResource(referenceRecords),
      candidatePreventivePawnResource = hasPreventivePawnResource(candidateRecords),
      hasConversionWindow = hasConversionWindow(involvedRecords),
      referenceConversionWindow = hasConversionWindow(referenceRecords),
      candidateConversionWindow = hasConversionWindow(candidateRecords),
      referenceStructuralTargetRelease = hasStructuralTargetRelease(referenceRecords),
      referenceReleasedTargets = releasedTargetPressure(referenceRecords),
      candidateReleasedTargets = releasedTargetPressure(candidateRecords),
      referenceCreatedTargets = createdTargetPressure(referenceRecords),
      candidateCreatedTargets = createdTargetPressure(candidateRecords),
      referenceStructuralImprovement = hasStructuralImprovementEvidence(referenceRecords),
      candidateStructuralImprovement = hasStructuralImprovementEvidence(candidateRecords),
      candidatePawnStructureImprovement = hasPawnStructureImprovementEvidence(candidateRecords),
      candidateTargetPressureGain = hasTargetPressureGainEvidence(candidateRecords),
      candidateCenterControlGain = hasCenterControlGainEvidence(candidateRecords),
      candidateDevelopmentActivation = hasDevelopmentActivationEvidence(candidateRecords),
      candidatePieceActivityGain = hasPieceActivityGainEvidence(candidateRecords),
      sameDestinationCaptureChoice = sameDestinationCaptureChoice(fact, referenceRecords, candidateRecords),
      referenceStructuralSignals = structuralSignalAnchors(referenceRecords),
      candidateStructuralSignals = structuralSignalAnchors(candidateRecords),
      referenceStructuralConsequences = referenceStructuralConsequenceKinds,
      candidateStructuralConsequences = candidateStructuralConsequenceKinds,
      candidatePawnStructureSignals = pawnStructureImprovementSignals(candidateRecords),
      referenceSemanticAxisCount = referenceSemanticAxisCount,
      candidateSemanticAxisCount = candidateSemanticAxisCount,
      referenceSemanticAxisLead = referenceSemanticAxisLead,
      semanticAxisDiagnostics = semanticAxisDiagnostics,
      candidatePlanEvidence = hasPlanCauseEvidence(candidateRecords),
      candidateStrategicEvidence = hasStrategicCauseEvidence(candidateRecords),
      candidateStrategicConcessionEvidence = hasStrategicConcessionEvidence(candidateRecords),
      referencePassedPawnResource = hasPassedPawnResource(referenceRecords),
      candidatePassedPawnResource = hasPassedPawnResource(candidateRecords),
      referenceEndgameResource = hasEndgameResource(referenceRecords),
      candidateEndgameResource = hasEndgameResource(candidateRecords),
      referenceLooseMaterialExploit = referenceLooseMaterial,
      candidateLooseMaterialExploit = candidateLooseMaterial,
      materialSwingEvidence = materialSwing,
      referenceMaterialNetCp = LineFactEvidence.maxMaterialNetCaptureCpForMover(referenceRecords),
      candidateMaterialNetCp = LineFactEvidence.maxMaterialNetCaptureCpForMover(candidateRecords),
      referenceMaterialMaxGainCp = LineFactEvidence.maxMaterialGainCpForMover(referenceRecords),
      candidateMaterialMaxGainCp = LineFactEvidence.maxMaterialGainCpForMover(candidateRecords),
      referenceMaterialPromotionGainCp = LineFactEvidence.maxMaterialPromotionGainCpForMover(referenceRecords),
      candidateMaterialPromotionGainCp = LineFactEvidence.maxMaterialPromotionGainCpForMover(candidateRecords),
      referenceMaterialRecapture = LineFactEvidence.hasMaterialRecaptureChain(referenceRecords),
      candidateMaterialRecapture = LineFactEvidence.hasMaterialRecaptureChain(candidateRecords),
      referenceMaterialRecovery = LineFactEvidence.hasMaterialRecoveryWindow(referenceRecords),
      candidateMaterialRecovery = LineFactEvidence.hasMaterialRecoveryWindow(candidateRecords),
      referenceMaterialComplete = LineFactEvidence.allHaveCompleteMaterialWindow(referenceRecords),
      candidateMaterialComplete = LineFactEvidence.allHaveCompleteMaterialWindow(candidateRecords),
      referenceRelationKinds = relationKinds(referenceRecords),
      candidateRelationKinds = candidateRelations,
      relationKinds = relationKinds(involvedRecords),
      referenceMotifs = motifNames(referenceRecords),
      candidateMotifs = motifNames(candidateRecords)
    )

  private def tacticalLossTraceFor(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      neighborhood: CandidateComparisonEvidenceNeighborhood,
      causeRecords: List[RelativeCauseDiagnosticRecord]
  ): TacticalLossTrace =
    val involvedRecords = (neighborhood.referenceRecords ++ neighborhood.candidateRecords).distinctBy(_.ref.id)
    val evidenceIds = involvedRecords.map(_.ref.id).toSet
    val lineStage = lineTacticalSignalStage(involvedRecords)
    val motifStage = moveMotifStage(involvedRecords)
    val relationStage = relationWitnessStage(involvedRecords)
    val threatStage = threatEpisodeStage(involvedRecords)
    val mechanismStage = tacticalMechanismStage(involvedRecords)
    val causeStage = tacticalRelativeCauseStage(causeRecords)
    val tacticalIdeas = tacticalIdeasForComparison(packet, fact, evidenceIds)
    val ideaStage =
      TacticalLossStageDiagnostic(
        stage = TacticalLossStage.TacticalIdea,
        present = tacticalIdeas.nonEmpty,
        evidenceIds = tacticalIdeas.map(_.ref.id).distinct.sorted,
        detailKinds = tacticalIdeas.map(_.confidence.toString).distinct.sorted
      )
    val tacticalClaims = tacticalClaimsForComparison(packet, fact, evidenceIds, tacticalIdeas)
    val claimStage =
      TacticalLossStageDiagnostic(
        stage = TacticalLossStage.TacticalClaim,
        present = tacticalClaims.nonEmpty,
        evidenceIds = tacticalClaims.map(_.id).distinct.sorted,
        detailKinds = tacticalClaims.flatMap(_.supportStatus.map(_.status.toString)).distinct.sorted
      )
    val stages =
      List(lineStage, motifStage, relationStage, threatStage, mechanismStage, causeStage, ideaStage, claimStage)
    val hasAnyTacticalTrace =
      stages.exists(_.present)
    val applicable =
      hasAnyTacticalTrace ||
        (requiresExplanatoryCause(fact) && comparisonSignal(fact) >= JudgmentThresholds.SIGNIFICANT_THREAT_WP)
    val downstreamStages =
      List(causeStage, ideaStage, claimStage)
    val blockedAt =
      if !applicable || claimStage.present then None
      else downstreamStages.find(!_.present).map(_.stage).orElse(stages.find(!_.present).map(_.stage))
    TacticalLossTrace(
      applicable = applicable,
      signalWinPercent = comparisonSignal(fact),
      blockedAt = blockedAt,
      lastPresentStage = stages.reverse.find(_.present).map(_.stage),
      stages = stages
    )

  private def lineTacticalSignalStage(records: List[EvidenceRecord]): TacticalLossStageDiagnostic =
    val lineRecords =
      records.collect {
        case record @ EvidenceRecord(_, payload: LineFactEvidence, _)
            if payload.hasTacticalLineConsequence || payload.hasProofSignalMaterialEvent =>
          record
      }
    val mateRecords =
      records.collect {
        case record @ EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) if mate.nonEmpty =>
          record
      }
    val detailKinds =
      lineRecords.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.tacticalLineConsequenceKinds.map(kind => s"LineConsequence:$kind") ++
          Option.when(payload.hasProofSignalMaterialEvent)("MaterialEvent").toList
      }.flatten ++ mateRecords.map(_ => "MateEval")
    TacticalLossStageDiagnostic(
      stage = TacticalLossStage.LineTacticalSignal,
      present = lineRecords.nonEmpty || mateRecords.nonEmpty,
      evidenceIds = (lineRecords ++ mateRecords).map(_.ref.id).distinct.sorted,
      detailKinds = detailKinds.distinct.sorted
    )

  private def moveMotifStage(records: List[EvidenceRecord]): TacticalLossStageDiagnostic =
    val motifRecords =
      records.collect {
        case record @ EvidenceRecord(_, payload: MoveMotifEvidence, _) if TacticalMotifClassifier.isRootMoveMotif(payload) =>
          record
      }
    TacticalLossStageDiagnostic(
      stage = TacticalLossStage.MoveMotifEvent,
      present = motifRecords.nonEmpty,
      evidenceIds = motifRecords.map(_.ref.id).distinct.sorted,
      detailKinds = motifRecords.collect { case EvidenceRecord(_, payload: MoveMotifEvidence, _) => payload.proof.kind }.distinct.sorted
    )

  private def relationWitnessStage(records: List[EvidenceRecord]): TacticalLossStageDiagnostic =
    val relationRecords =
      records.collect {
        case record @ EvidenceRecord(_, payload: RelationFactEvidence, _) if payload.hasConcreteRelationProof =>
          record
      }
    TacticalLossStageDiagnostic(
      stage = TacticalLossStage.RelationWitness,
      present = relationRecords.nonEmpty,
      evidenceIds = relationRecords.map(_.ref.id).distinct.sorted,
      detailKinds = relationRecords.collect { case EvidenceRecord(_, payload: RelationFactEvidence, _) => payload.kind.toString }.distinct.sorted
    )

  private def threatEpisodeStage(records: List[EvidenceRecord]): TacticalLossStageDiagnostic =
    val threatRecords =
      records.collect {
        case record @ EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) if payload.isProofSignalDefensivePressure =>
          record
      }
    TacticalLossStageDiagnostic(
      stage = TacticalLossStage.ThreatEpisode,
      present = threatRecords.nonEmpty,
      evidenceIds = threatRecords.map(_.ref.id).distinct.sorted,
      detailKinds = threatRecords.collect {
        case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
          s"${payload.episode.kind}:${payload.episode.driver}:${payload.episode.severity}"
      }.distinct.sorted
    )

  private def tacticalMechanismStage(records: List[EvidenceRecord]): TacticalLossStageDiagnostic =
    val mechanismRecords =
      records.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalIdea =>
          record
      }
    TacticalLossStageDiagnostic(
      stage = TacticalLossStage.TacticalMechanism,
      present = mechanismRecords.nonEmpty,
      evidenceIds = mechanismRecords.map(_.ref.id).distinct.sorted,
      detailKinds = mechanismRecords.collect { case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) => payload.kind.toString }.distinct.sorted
    )

  private def tacticalRelativeCauseStage(causeRecords: List[RelativeCauseDiagnosticRecord]): TacticalLossStageDiagnostic =
    val tacticalCauseRecords =
      causeRecords.filter(entry => tacticalCause(entry.cause.kind) && entry.cause.hasOwnedTypedDepth)
    TacticalLossStageDiagnostic(
      stage = TacticalLossStage.RelativeCause,
      present = tacticalCauseRecords.nonEmpty,
      evidenceIds = tacticalCauseRecords.map(_.id).distinct.sorted,
      detailKinds = tacticalCauseRecords.map(_.cause.kind.toString).distinct.sorted
    )

  private def tacticalIdeasForComparison(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      evidenceIds: Set[String]
  ): List[ChessIdea] =
    packet.ideas.filter(idea =>
      idea.ref.family == ChessIdeaFamily.Tactical &&
        (
          idea.primaryLine.exists(line => comparisonLine(fact, line)) ||
            idea.evidence.exists(ref => evidenceIds.contains(ref.id))
        )
    )

  private def tacticalClaimsForComparison(
      packet: EvidenceBackedJudgmentPacket,
      fact: CandidateComparisonFact,
      evidenceIds: Set[String],
      tacticalIdeas: List[ChessIdea]
  ): List[ClaimSeed] =
    val ideaIds = tacticalIdeas.map(_.ref.id).toSet
    packet.claims.filter(claim =>
      claim.family == ClaimFamily.Tactical &&
        (
          claim.primaryLine.exists(line => comparisonLine(fact, line)) ||
            claim.evidence.exists(ref => evidenceIds.contains(ref.id)) ||
            claim.ideaRefs.exists(idea => ideaIds.contains(idea.id))
        )
    )

  private def comparisonLine(fact: CandidateComparisonFact, line: LineNodeRef): Boolean =
    line == fact.referenceLine || line == fact.candidateLine

  private def advisoryCauseHintsFor(
      fact: CandidateComparisonFact,
      causeKinds: List[RelativeCauseKind],
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseKind] =
    val generated = causeKinds.toSet
    val filteredCandidates =
      RelativeCauseDraftPlanner
        .drafts(profile)
        .map(_.kind)
        .distinct
    if requiresExplanatoryCause(fact) then
      filteredCandidates.filterNot(generated.contains)
    else Nil

  private def failureReasonsFor(
      fact: CandidateComparisonFact,
      causeKinds: List[RelativeCauseKind],
      evidenceLayers: ComparisonEvidenceLayerDiagnostics,
      trace: CandidateCauseDecisionTrace,
      viewDiagnostics: ComparisonMoveJudgmentViewDiagnostics,
      hasLowDepthCause: Boolean,
      hasGenericCause: Boolean,
      hasUnboundEvidence: Boolean
  ): List[CandidateComparisonFailureReason] =
    val missingLine =
      List(evidenceLayers.reference, evidenceLayers.candidate).exists(neighborhood =>
        !neighborhood.directLayers.contains(EvidenceLayer.Line)
      )
    val missingEval =
      List(evidenceLayers.reference, evidenceLayers.candidate).exists(neighborhood =>
        !neighborhood.directLayers.contains(EvidenceLayer.Eval)
      )
    val significantEnginePreference = CandidateComparisonDiagnostic.significantEnginePreference(fact)
    val requiresCause = CandidateComparisonDiagnostic.requiresExplanatoryCause(fact)
    val generatedKnownCause = causeKinds.nonEmpty
    val generatedOnlyUnexplained =
      requiresCause && causeKinds.isEmpty
    val secondaryContextGap =
      generatedOnlyUnexplained && contextAlternativeComparison(fact)
    val viewProjectionReasons =
      List(
        Option.when(viewDiagnostics.projectedContextCauseNoViewIds.nonEmpty)(
          CandidateComparisonFailureReason.ContextCauseProjectionMissing
        ),
        Option.when(viewDiagnostics.playableLossPrimaryCauseEvidenceIds.nonEmpty)(
          CandidateComparisonFailureReason.PlayableLossPrimaryOverclaim
        )
      ).flatten
    if viewProjectionReasons.nonEmpty then
      return viewProjectionReasons
    if !significantEnginePreference && causeKinds.isEmpty && !latentComparisonEvidence(trace) then
      return Nil
    if generatedKnownCause && !generatedOnlyUnexplained then
      return List(
        Option.when(hasLowDepthCause)(CandidateComparisonFailureReason.LowDepthGeneratedCause),
        Option.when(hasGenericCause)(CandidateComparisonFailureReason.GenericGeneratedCause),
        Option.when(hasUnboundEvidence)(CandidateComparisonFailureReason.UnboundEvidenceWithGeneratedCause)
      ).flatten.distinct
    if positiveContextAlternative(fact) && causeKinds.isEmpty then
      return Nil
    if secondaryContextGap then
      return List(CandidateComparisonFailureReason.ContextAlternativeComparison)
    val reasons = List(
      Option.when(hasLowDepthCause)(CandidateComparisonFailureReason.LowDepthGeneratedCause),
      Option.when(hasGenericCause)(CandidateComparisonFailureReason.GenericGeneratedCause),
      Option.when(hasUnboundEvidence)(CandidateComparisonFailureReason.UnboundEvidenceWithGeneratedCause),
      Option.when(
        requiresCause &&
          causeKinds.isEmpty &&
          contextAlternativeComparison(fact)
      )(CandidateComparisonFailureReason.ContextAlternativeComparison),
      Option.when(!significantEnginePreference)(CandidateComparisonFailureReason.LowSignalEnginePreference),
      latentReason(
        !significantEnginePreference && latentTacticalEvidence(trace),
        tacticalLatentReason(fact, trace)
      ),
      latentReason(
        !significantEnginePreference && trace.materialSwingEvidence,
        CandidateComparisonFailureReason.MaterialEvidenceBelowThreshold
      ),
      Option.when(!significantEnginePreference && latentStrategicEvidence(trace))(
        strategicLatentReason(fact, trace)
      ),
      Option.when(missingLine)(CandidateComparisonFailureReason.MissingLineEvidence),
      Option.when(missingEval)(CandidateComparisonFailureReason.MissingEvalEvidence),
      Option.when(requiresCause && causeKinds.isEmpty)(CandidateComparisonFailureReason.NoCauseGenerated),
      Option.when(generatedOnlyUnexplained)(CandidateComparisonFailureReason.GeneratedOnlyUnexplained),
      Option.when(
        (generatedOnlyUnexplained &&
          (trace.referenceTacticalMechanismKinds.nonEmpty ||
            trace.candidateTacticalMechanismKinds.nonEmpty)
        )
      )(
        CandidateComparisonFailureReason.TacticalEvidenceUnbound
      ),
      Option.when(generatedOnlyUnexplained && trace.hasThreatResource)(
        CandidateComparisonFailureReason.DefensiveEvidenceUnbound
      ),
      Option.when(
        generatedOnlyUnexplained && (trace.referenceConversionWindow || trace.candidateConversionWindow)
      )(
        CandidateComparisonFailureReason.ConversionEvidenceUnbound
      ),
      Option.when(
        (generatedOnlyUnexplained &&
          fact.kind == CandidateComparisonKind.PlayedVsBest &&
          (trace.candidateStrategicEvidence ||
            trace.candidatePlanEvidence ||
            trace.referenceStructuralTargetRelease ||
            trace.candidateStructuralImprovement ||
            trace.candidatePawnStructureImprovement ||
            trace.candidateTargetPressureGain ||
            trace.candidateCenterControlGain ||
            trace.candidateDevelopmentActivation ||
            trace.candidatePieceActivityGain ||
            trace.referencePassedPawnResource ||
            trace.candidatePassedPawnResource ||
            trace.referenceEndgameResource ||
            trace.candidateEndgameResource ||
            trace.candidateStrategicConcessionEvidence)
        )
      )(
        CandidateComparisonFailureReason.StrategicEvidenceUnbound
      ),
      Option.when(requiresCause && causeKinds.isEmpty && !hasUnboundEvidence && !hasAvailableExplanatoryEvidence(evidenceLayers))(
        CandidateComparisonFailureReason.NoSpecificEvidence
      )
    ).flatten.distinct
    if reasons.nonEmpty then reasons else List(CandidateComparisonFailureReason.UnknownPattern)

  private def tacticalCause(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.TacticalEvent)

  private def strategicCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def significantEnginePreference(fact: CandidateComparisonFact): Boolean =
    significanceReasons(fact).nonEmpty

  private def significanceReasons(fact: CandidateComparisonFact): List[CandidateComparisonSignificanceReason] =
    List(
      Option.when(fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP)(
        CandidateComparisonSignificanceReason.PlayedLoss
      ),
      Option.when(fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP)(
        CandidateComparisonSignificanceReason.CandidateImproves
      ),
      Option.when(fact.comparison.candidateSet.exists(_.onlyMove))(CandidateComparisonSignificanceReason.OnlyMove)
    ).flatten

  private def requiresExplanatoryCause(fact: CandidateComparisonFact): Boolean =
    significantEnginePreference(fact) && !positiveContextAlternative(fact) && !sameRootMoveComparison(fact)

  private def sameRootMoveComparison(fact: CandidateComparisonFact): Boolean =
    normalizeMove(fact.referenceLine.rootMove) == normalizeMove(fact.candidateLine.rootMove)

  private def contextAlternativeComparison(fact: CandidateComparisonFact): Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsAlternative ||
      fact.kind == CandidateComparisonKind.ReferenceVsAlternative

  private def positiveContextAlternative(fact: CandidateComparisonFact): Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsAlternative &&
      fact.comparison.winPercentLossForMover < JudgmentThresholds.INACCURACY_WP &&
      fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP

  private def comparisonSignal(fact: CandidateComparisonFact): Double =
    math.max(
      fact.comparison.winPercentLossForMover,
      math.abs(fact.comparison.candidateWinPercentDeltaForMover)
    )

  private def lowSignalReasons(fact: CandidateComparisonFact): List[CandidateComparisonLowSignalReason] =
    val signal = comparisonSignal(fact)
    List(
      Option.when(!significantEnginePreference(fact) && signal == 0.0)(CandidateComparisonLowSignalReason.QuietNoDirection),
      Option.when(
        !significantEnginePreference(fact) &&
          signal > 0.0 &&
          signal < JudgmentThresholds.ONLY_DEFENSE_TOLERANCE_WP
      )(CandidateComparisonLowSignalReason.BelowSignalFloor),
      Option.when(
        !significantEnginePreference(fact) &&
          signal >= JudgmentThresholds.ONLY_DEFENSE_TOLERANCE_WP &&
          signal < JudgmentThresholds.SIGNIFICANT_THREAT_WP
      )(CandidateComparisonLowSignalReason.BelowBindingThreshold),
      Option.when(
        !significantEnginePreference(fact) &&
          signal >= JudgmentThresholds.SIGNIFICANT_THREAT_WP &&
          fact.kind == CandidateComparisonKind.PlayedVsBest
      )(CandidateComparisonLowSignalReason.NearThresholdPrimaryPlayed),
      Option.when(
        !significantEnginePreference(fact) &&
          signal >= JudgmentThresholds.SIGNIFICANT_THREAT_WP &&
          fact.kind != CandidateComparisonKind.PlayedVsBest
      )(CandidateComparisonLowSignalReason.NearThresholdContextAlternative),
      Option.when(
        fact.kind == CandidateComparisonKind.PlayedVsAlternative ||
          fact.kind == CandidateComparisonKind.ReferenceVsAlternative
      )(CandidateComparisonLowSignalReason.ContextAlternative)
    ).flatten.distinct

  private def latentReason(
      condition: Boolean,
      reason: CandidateComparisonFailureReason
  ): Option[CandidateComparisonFailureReason] =
    Option.when(condition)(reason)

  private def tacticalLatentReason(
      fact: CandidateComparisonFact,
      trace: CandidateCauseDecisionTrace
  ): CandidateComparisonFailureReason =
    if tacticalSignalNeedsWidthDepth(fact, trace) then CandidateComparisonFailureReason.TacticalSignalNeedsWidthDepth
    else if comparisonSignal(fact) >= JudgmentThresholds.IGNORABLE_THREAT_WP then
      CandidateComparisonFailureReason.TacticalEvidenceBelowThreshold
    else CandidateComparisonFailureReason.LowSignalTacticalContext

  private def tacticalSignalNeedsWidthDepth(
      fact: CandidateComparisonFact,
      trace: CandidateCauseDecisionTrace
  ): Boolean =
    comparisonSignal(fact) >= JudgmentThresholds.SIGNIFICANT_THREAT_WP &&
      trace.relationKinds.isEmpty &&
      trace.referenceTacticalMechanismKinds.isEmpty &&
      trace.candidateTacticalMechanismKinds.isEmpty &&
      !trace.referenceConcreteLine &&
      !trace.candidateConcreteLine

  private def strategicLatentReason(
      fact: CandidateComparisonFact,
      trace: CandidateCauseDecisionTrace
  ): CandidateComparisonFailureReason =
    val signal = comparisonSignal(fact)
    if signal < JudgmentThresholds.ONLY_DEFENSE_TOLERANCE_WP then
      CandidateComparisonFailureReason.StrategicContextBelowSignalFloor
    else if signal < JudgmentThresholds.SIGNIFICANT_THREAT_WP then
      CandidateComparisonFailureReason.StrategicEvidenceBelowCauseThreshold
    else if fact.kind == CandidateComparisonKind.PlayedVsBest &&
        (trace.candidateStrategicConcessionEvidence ||
          trace.materialSwingEvidence ||
          trace.candidatePlanEvidence)
    then
      CandidateComparisonFailureReason.PrimaryStrategicNearThresholdUnderbinding
    else if fact.kind == CandidateComparisonKind.PlayedVsBest then
      CandidateComparisonFailureReason.PrimaryStrategicSignalNeedsWidthDepth
    else CandidateComparisonFailureReason.ContextAlternativeStrategicNearThreshold

  private def latentTacticalEvidence(trace: CandidateCauseDecisionTrace): Boolean =
    trace.referenceTacticalMechanismKinds.nonEmpty ||
      trace.candidateTacticalMechanismKinds.nonEmpty ||
      trace.relationKinds.nonEmpty ||
      trace.referenceTacticalRisk

  private def latentComparisonEvidence(trace: CandidateCauseDecisionTrace): Boolean =
    latentTacticalEvidence(trace) ||
      trace.materialSwingEvidence ||
      latentStrategicEvidence(trace)

  private def latentStrategicEvidence(trace: CandidateCauseDecisionTrace): Boolean =
    trace.candidatePlanEvidence ||
      trace.referenceStructuralTargetRelease ||
      trace.referenceStructuralImprovement ||
      trace.candidateStructuralImprovement ||
      trace.candidatePawnStructureImprovement ||
      trace.candidateTargetPressureGain ||
      trace.candidateCenterControlGain ||
      trace.candidateDevelopmentActivation ||
      trace.candidatePieceActivityGain ||
      trace.referencePassedPawnResource ||
      trace.candidatePassedPawnResource ||
      trace.referenceEndgameResource ||
      trace.candidateEndgameResource ||
      trace.candidateStrategicConcessionEvidence

  private def lowDepthCauseShouldBeAudited(
      fact: CandidateComparisonFact,
      causeKinds: List[RelativeCauseKind]
  ): Boolean =
    requiresExplanatoryCause(fact) ||
      (!positiveContextAlternative(fact) &&
        causeKinds.exists(tacticalCause) &&
        (fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
          fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP))

  private def candidateConcreteTacticalBridge(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalIdea
      case _ =>
        false
    }

  private def hasOnlyDefense(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.onlyDefense.nonEmpty
      case _ => false
    }

  private def referenceHasTacticalRisk(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalIdea
      case _ =>
        false
    }

  private def hasThreatResource(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.onlyDefense.nonEmpty ||
          (!payload.insufficientData &&
            (payload.defenseRequired || payload.maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP)))
      case _ => false
    }

  private def hasProphylacticResource(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        !payload.insufficientData &&
          (payload.prophylaxisNeeded ||
            payload.maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.SIGNIFICANT_THREAT_WP))
      case _ => false
    }

  private def hasCastlingResource(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(ref, payload: MoveMotifEvidence, _) if payload.recordLineBound(ref) =>
        val moveUci = payload.moveUci
        payload.motif match
          case Motif.Castling(_, _, _, move) if motifMoveBound(move, moveUci) => true
          case _                                                             => false
      case _ =>
        false
    }

  private def hasKingStepResource(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(ref, payload: MoveMotifEvidence, _) if payload.recordLineBound(ref) =>
        val moveUci = payload.moveUci
        payload.motif match
          case Motif.KingStep(stepType, _, _, move) if motifMoveBound(move, moveUci) =>
            stepType != Motif.KingStepType.Activation
          case _ => false
      case _ =>
        false
    }

  private def hasPreventivePawnResource(records: List[EvidenceRecord]): Boolean =
    val rootMotifs = rootMoveMotifRecords(records)
    val flankPawnRecords =
      rootMotifs.filter {
        case EvidenceRecord(_, payload: MoveMotifEvidence, _) =>
          val moveUci = payload.moveUci
          payload.motif match
            case Motif.PawnAdvance(file, _, _, color, _, move) if motifMoveBound(move, moveUci) =>
              kingFlankPawnAdvance(records, file, color)
            case _ => false
        case _ => false
      }
    val kingSafetyCueRecords =
      rootMotifs.filter {
        case EvidenceRecord(_, payload: MoveMotifEvidence, _) =>
          payload.motif match
            case _: Motif.Pin | _: Motif.WeakBackRank | _: Motif.BackRankMate | _: Motif.MateNet => true
            case _                                                                                => false
        case _ => false
      }
    flankPawnRecords.exists(flank => kingSafetyCueRecords.exists(sameMoveMotifContext(flank, _)))

  private def sameDestinationCaptureChoice(
      fact: CandidateComparisonFact,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): Boolean =
    val referenceMove = normalizeMove(fact.referenceLine.rootMove)
    val candidateMove = normalizeMove(fact.candidateLine.rootMove)
    EvidenceRef.sameDestinationDifferentOrigin(referenceMove, candidateMove) &&
      EvidenceRecord.hasRootCaptureEvent(referenceRecords, referenceMove) &&
      EvidenceRecord.hasRootCaptureEvent(candidateRecords, candidateMove)

  private def motifMoveBound(move: Option[String], moveUci: String): Boolean =
    move.exists(value => normalizeMove(value) == normalizeMove(moveUci))

  private def rootMoveMotifRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(ref, payload: MoveMotifEvidence, _) if payload.recordLineBound(ref) =>
        record
    }

  private def sameMoveMotifContext(left: EvidenceRecord, right: EvidenceRecord): Boolean =
    (left, right) match
      case (EvidenceRecord(leftRef, leftPayload: MoveMotifEvidence, _), EvidenceRecord(rightRef, rightPayload: MoveMotifEvidence, _)) =>
        leftPayload.sameMotifContext(leftRef, rightRef, rightPayload)
      case _ =>
        false

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def kingFlankPawnAdvance(records: List[EvidenceRecord], pawnFile: File, color: Color): Boolean =
    kingFile(records, color).exists(file => (fileIndex(file) - fileIndex(pawnFile)).abs <= 1)

  private def kingFile(records: List[EvidenceRecord], color: Color): Option[File] =
    records.view.flatMap(record => kingFileFromFen(record.ref.position.fen, color)).headOption

  private def kingFileFromFen(fen: String, color: Color): Option[File] =
    val king = if color.white then 'K' else 'k'
    Option(fen)
      .map(_.takeWhile(_ != ' '))
      .flatMap: board =>
        var file = 0
        var found = Option.empty[File]
        board.iterator.foreach {
          case '/' =>
            file = 0
          case ch if ch.isDigit =>
            file += ch.asDigit
          case ch =>
            val current = file
            file += 1
            if ch == king && found.isEmpty then found = Some(fileFromIndex(current))
        }
        found

  private def fileFromIndex(index: Int): File =
    List(File.A, File.B, File.C, File.D, File.E, File.F, File.G, File.H)(index.max(0).min(7))

  private def fileIndex(file: File): Int =
    file match
      case File.A => 0
      case File.B => 1
      case File.C => 2
      case File.D => 3
      case File.E => 4
      case File.F => 5
      case File.G => 6
      case File.H => 7

  private def hasConversionWindow(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, SinglePositionEvidence(assessment), _) =>
        assessment.simplifyBias.shouldSimplify
      case EvidenceRecord(_, payload: RelationFactEvidence, _) if payload.kind == RelationFactKind.BadPieceLiquidation =>
        true
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.maintainedWinningEndgameTechniqueHorizons.nonEmpty ||
          payload.failedWinningEndgameTechniqueHorizons.nonEmpty
      case _ => false
    }

  private def hasStructuralTargetRelease(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(_.hasTargetPressureReleaseAxis)

  private def hasStructuralImprovementEvidence(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(payload =>
      payload.kind == StrategicMechanismKind.StructuralImprovement && payload.canSupportStrategicCause
    )

  private def hasTargetPressureGainEvidence(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(_.hasTargetPressureGainAxis)

  private def hasCenterControlGainEvidence(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(_.hasCenterControlGainAxis)

  private def hasDevelopmentActivationEvidence(records: List[EvidenceRecord]): Boolean =
    hasActivityGainEvidence(records)

  private def hasPieceActivityGainEvidence(records: List[EvidenceRecord]): Boolean =
    hasActivityGainEvidence(records)

  private def hasActivityGainEvidence(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(_.hasActivityGainAxis)

  private def hasPawnStructureImprovementEvidence(records: List[EvidenceRecord]): Boolean =
    pawnStructureImprovementSignals(records).nonEmpty

  private def semanticAxisDiagnosticsFor(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): SemanticAxisDiagnostics =
    val referenceProfile = semanticAxisProfile(referenceRecords)
    val candidateProfile = semanticAxisProfile(candidateRecords)
    val referenceStrengths = referenceProfile.strengths
    val candidateStrengths = candidateProfile.strengths
    val referenceAxes = referenceStrengths.keySet
    val candidateAxes = candidateStrengths.keySet
    val allAxes = referenceAxes ++ candidateAxes
    SemanticAxisDiagnostics(
      referenceAxisStrengths = referenceStrengths,
      candidateAxisStrengths = candidateStrengths,
      referenceAxisSourceIds = referenceProfile.sourceIds,
      candidateAxisSourceIds = candidateProfile.sourceIds,
      referenceAxisSourceLayers = referenceProfile.sourceLayers,
      candidateAxisSourceLayers = candidateProfile.sourceLayers,
      referenceAxes = referenceAxes.toList.sorted,
      candidateAxes = candidateAxes.toList.sorted,
      sharedAxes = referenceAxes.intersect(candidateAxes).toList.sorted,
      referenceOnlyAxes = referenceAxes.diff(candidateAxes).toList.sorted,
      candidateOnlyAxes = candidateAxes.diff(referenceAxes).toList.sorted,
      referenceLeadAxes =
        allAxes.filter(axis => referenceStrengths.getOrElse(axis, 0) > candidateStrengths.getOrElse(axis, 0)).toList.sorted,
      candidateLeadAxes =
        allAxes.filter(axis => candidateStrengths.getOrElse(axis, 0) > referenceStrengths.getOrElse(axis, 0)).toList.sorted
    )

  private def semanticAxisProfile(records: List[EvidenceRecord]): SemanticAxisProfile =
    val entries = records.flatMap(strategicMechanismAxisEntries)
    val strengths =
      entries.groupMapReduce(_.key)(_.strength)(_ + _)
    val sourceIds =
      entries
        .groupMap(_.key)(_.source.id)
        .view
        .mapValues(_.distinct.sorted)
        .toMap
    val sourceLayers =
      entries
        .groupMap(_.key)(_.source.layer)
        .view
        .mapValues(_.distinctBy(_.ordinal).sortBy(_.ordinal))
        .toMap
    SemanticAxisProfile(strengths = strengths, sourceIds = sourceIds, sourceLayers = sourceLayers)

  private def strategicMechanismAxisEntries(record: EvidenceRecord): List[SemanticAxisEntry] =
    record match
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.signals.flatMap(signal =>
          signal.axisKey.map(axisKey =>
            SemanticAxisEntry(
              key = axisKey,
              strength = signal.strength,
              source = signal.source
            )
          )
        )
      case _ =>
        Nil

  private def structuralImprovementConsequenceKinds(records: List[EvidenceRecord]): List[TransitionConsequenceKind] =
    StructuralDeltaEvidence.structuralImprovementConsequenceKinds(records)

  private def structuralSignalAnchors(records: List[EvidenceRecord]): List[String] =
    records.collect { case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
      payload.signalAnchors
    }.flatten.distinct.sorted

  private def pawnStructureImprovementSignals(records: List[EvidenceRecord]): List[String] =
    records.collect { case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) if payload.canAnchorPawnStructureIdea =>
      payload.signals.map(signal => s"${signal.kind}:${signal.label}")
    }.flatten.distinct.sorted

  private def releasedTargetPressure(records: List[EvidenceRecord]): List[String] =
    records.collect { case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
      payload.releasedTargetPressureSubjects
    }.flatten.distinct.sorted

  private def createdTargetPressure(records: List[EvidenceRecord]): List[String] =
    records.collect { case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
      payload.createdTargetPressureSubjects
    }.flatten.distinct.sorted

  private def hasPlanCauseEvidence(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(_.canAnchorPlanIdea)

  private def hasStrategicCauseEvidence(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canSupportStrategicCause
      case _ =>
        false
    }

  private def hasStrategicConcessionEvidence(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(_.hasStrategicConcessionAxis)

  private def hasMaterialSwingEvidence(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): Boolean =
    typedMaterialConsequenceSwing(referenceRecords, candidateRecords)

  private def hasPassedPawnResource(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.hasPassedPawnResourceSignal
      case EvidenceRecord(_, payload: MoveMotifEvidence, _) =>
        payload.motif match
          case _: Motif.PassedPawnPush | _: Motif.PassedPawn | _: Motif.PawnPromotion => true
          case _                                                                      => false
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasProofSignalConsequence(LineConsequenceKind.PromotionRace) ||
          payload.materialOutcomeProfile.gainSignals.contains(LineMaterialOutcomeSignal.PromotionGain) ||
          payload.materialOutcomeProfile.lossSignals.contains(LineMaterialOutcomeSignal.PromotionLoss)
      case _ =>
        false
    }

  private def hasEndgameResource(records: List[EvidenceRecord]): Boolean =
    strategicMechanismExists(records)(payload =>
      payload.kind == StrategicMechanismKind.Endgame && payload.canSupportStrategicCause
    ) ||
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          payload.endgameTechniqueHorizons.nonEmpty
        case _ =>
          false
      }

  private def strategicMechanismExists(
      records: List[EvidenceRecord]
  )(mechanismPredicate: StrategicMechanismEvidence => Boolean): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        mechanismPredicate(payload)
      case _ =>
        false
    }

  private def hasLooseMaterialExploit(
      records: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): Boolean =
    val lineMaterial =
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          payload.materialOutcomeProfile.gainMagnitude != LineMaterialOutcomeMagnitude.None ||
            payload.materialOutcomeProfile.lossMagnitude != LineMaterialOutcomeMagnitude.None ||
            payload.hasProofSignalConsequence(LineConsequenceKind.MaterialGain) ||
            payload.hasProofSignalConsequence(LineConsequenceKind.MaterialLoss)
        case _ =>
          false
      }
    val looseRelation = records.exists {
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        looseMaterialRelation(payload)
      case _ =>
        false
    }
    (lineMaterial || looseRelation) && looseMaterialContextPresent(records ++ sharedRecords)

  private def looseMaterialContextPresent(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
        payload.looseMaterialAnchors.nonEmpty
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        looseMaterialRelation(payload)
      case _ =>
        false
    }

  private def looseMaterialRelation(payload: RelationFactEvidence): Boolean =
    payload.hasConcreteRelationProof &&
      (
        payload.kind == RelationFactKind.HangingPiece ||
          payload.kind == RelationFactKind.TrappedPiece ||
          payload.kind == RelationFactKind.Domination
      )

  private def typedMaterialConsequenceSwing(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): Boolean =
    materialGainMagnitude(referenceRecords).ordinal > materialGainMagnitude(candidateRecords).ordinal ||
      materialLossMagnitude(candidateRecords).ordinal > materialLossMagnitude(referenceRecords).ordinal

  private def materialGainMagnitude(records: List[EvidenceRecord]): LineMaterialOutcomeMagnitude =
    LineFactEvidence.materialOutcomeProfile(records).gainMagnitude

  private def materialLossMagnitude(records: List[EvidenceRecord]): LineMaterialOutcomeMagnitude =
    LineFactEvidence.materialOutcomeProfile(records).lossMagnitude

  private def motifNames(records: List[EvidenceRecord]): List[String] =
    records.collect {
      case EvidenceRecord(_, payload: MoveMotifEvidence, _) if TacticalMotifClassifier.isRootMoveMotif(payload) =>
        payload.proof.kind
    }.distinct

  private def relationKinds(records: List[EvidenceRecord]): List[RelationFactKind] =
    records.collect { case EvidenceRecord(_, payload: RelationFactEvidence, _) => payload.kind }.distinct

  private def tacticalMechanismKinds(records: List[EvidenceRecord]): List[TacticalMechanismKind] =
    records.collect { case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) => payload.kind }.distinct

  private val explanatoryLayers: Set[EvidenceLayer] =
    Set(
      EvidenceLayer.SinglePosition,
      EvidenceLayer.StrategicMechanism,
      EvidenceLayer.ThreatPressure,
      EvidenceLayer.MoveMotif,
      EvidenceLayer.TacticalMechanism,
      EvidenceLayer.Relation
    )

  private def availableLayers(evidenceLayers: ComparisonEvidenceLayerDiagnostics): Set[EvidenceLayer] =
    List(evidenceLayers.reference, evidenceLayers.candidate).flatMap(neighborhood =>
      neighborhood.directLayers ++ neighborhood.parentLayerCounts.keySet
    ).toSet

  private def layerCounts(layers: List[EvidenceLayer]): Map[EvidenceLayer, Int] =
    layers.groupMapReduce(identity)(_ => 1)(_ + _)

final case class SemanticCoverageMetrics(
    tacticalIdeas: Int,
    strategicIdeas: Int,
    pawnStructureIdeas: Int,
    openingIdeas: Int,
    defensiveIdeas: Int,
    evaluationIdeas: Int,
    conversionIdeas: Int,
    materialIdeas: Int,
    claimFamilies: Set[ClaimFamily],
    claimCandidateFamilies: Set[ClaimFamily],
    claimCandidateFamilyCounts: Map[ClaimFamily, Int],
    claimLifecycleDiagnostics: List[ClaimLifecycleDiagnostic],
    claimLifecycleStageCounts: Map[ClaimLifecycleStage, Int],
    claimLifecycleTruthCounts: Map[ClaimLifecycleTruthStatus, Int],
    claimLifecycleRelativeCauseIdeaDroppedIds: List[String],
    claimLifecycleRelativeCauseDroppedByStage: Map[ClaimLifecycleStage, List[String]],
    relativeCauseFamilyMismatchDiagnostics: List[RelativeCauseFamilyMismatchDiagnostic],
    relativeCauseFamilyMismatchKindCounts: Map[ClaimFamilyMismatchKind, Int],
    hasRelativeCauseFamilyMismatch: Boolean,
    claimSupportClusters: Int,
    clusteredAnchorClaims: Int,
    clusteredSupportingClaims: Int,
    clusteredConstrainingClaims: Int,
    clusteredLongTermSupportIdeas: Int,
    claimSupportClusterFamilies: Set[ClaimFamily],
    claimSupportClusterLayers: Set[EvidenceLayer],
    claimEventClusters: Int,
    clusteredEventClaims: Int,
    clusteredEventCauseClaims: Int,
    clusteredEventEvaluationClaims: Int,
    clusteredEventWitnessClaims: Int,
    clusteredEventIdeas: Int,
    relatedEventSupportClusters: Int,
    claimEventClusterFamilies: Set[ClaimFamily],
    claimEventClusterLayers: Set[EvidenceLayer],
    claimEventClusterKinds: Set[ClaimEventClusterKind],
    claimEventClusterCauses: Set[RelativeCauseKind],
    claimEventClusterRoles: Set[RelativeCauseRole],
    claimEventInteractionKinds: Set[ClaimInteractionKind],
    unclusteredEventClaims: Int,
    unclusteredEventClaimFamilies: Set[ClaimFamily],
    verdictCarriersWithoutEventCauseOwner: Int,
    verdictOnlyEvaluationClaims: Int,
    localConcreteClaims: Int,
    localConcreteClaimFamilies: Set[ClaimFamily],
    localConcreteClaimDiagnostics: List[LocalConcreteClaimDiagnostic],
    boardAnchorFacts: Int,
    boardAttackDefenseFacts: Int,
    lineReplayFacts: Int,
    lineEventFacts: Int,
    lineConsequenceFacts: Int,
    relativeCauseProofs: Int,
    hasRelativeAssessment: Boolean,
    candidateComparisonFacts: Int,
    relativeCauseFacts: Int,
    moveVerdictCertifications: Int,
    playedRelatedComparisonFacts: Int,
    primaryPlayedComparisonFacts: Int,
    playedRelativeCauseFacts: Int,
    planTransitionWithoutSnapshotPairIds: List[String],
    branchReplyProbeRequests: Int,
    branchReplyProbeMoves: List[String],
    branchReplyThreatLines: Int,
    branchReplyThreatPressureRecords: Int,
    branchReplyThreatEpisodeRecords: Int,
    branchReplyProbeAdmittedResults: Int,
    branchReplyProbeRejectedResults: Int,
    branchReplyProbeIgnoredResults: Int,
    branchReplyProbePendingRequests: Int,
    branchReplyProbeRejectReasons: Map[String, Int],
    branchReplyProbeAdmittedMoves: List[String],
    branchReplyProbeRejectedMoves: List[String],
    branchReplyProbePendingMoves: List[String],
    branchReplyProbeLifecycleState: String,
    hasPendingBranchReplyDepth: Boolean,
    hasUnexplainedEngineGap: Boolean,
    hasPlayedUnexplainedEngineGap: Boolean,
    hasContextUnexplainedEngineGap: Boolean,
    hasSecondaryContextEngineGap: Boolean,
    comparisonDiagnostics: List[CandidateComparisonDiagnostic],
    secondaryContextComparisonIds: List[String],
    secondaryContextWithPrimaryCoverageIds: List[String],
    secondaryContextWithoutPrimaryCoverageIds: List[String],
    contextUnexplainedComparisonIds: List[String],
    contextUnexplainedWithPrimaryCoverageIds: List[String],
    contextUnexplainedWithoutPrimaryCoverageIds: List[String],
    openingApplicabilityDiagnostics: List[OpeningApplicabilityDiagnostic],
    openingSupportDiagnostics: List[OpeningSupportDiagnostic],
    tacticalDetectionFailureComparisonIds: List[String],
    tacticalDetectionFailureBlockedStages: Map[TacticalLossStage, Int],
    hasTacticalDetectionFailure: Boolean,
    contextTacticalOutrankingDiagnostics: List[ContextTacticalOutrankingDiagnostic],
    contextTacticalOutranksPrimaryPlayed: Boolean,
    hasVerdict: Boolean,
    hasCandidateSetComparison: Boolean,
    hasOnlyMoveSignal: Boolean,
    hasForcedLineTheme: Boolean
)

object SemanticCoverageMetrics:
  def from(packet: EvidenceBackedJudgmentPacket): SemanticCoverageMetrics =
    val playedComparisonRecords = playedRelatedComparisonRecords(packet)
    val primaryPlayedComparisonRecords = primaryPlayedCandidateComparisonRecords(packet)
    val playedCauseRecords = playedRelativeCauseRecords(packet)
    val comparisonDiagnostics = CandidateComparisonDiagnostic.fromPacket(packet)
    val familyMismatchDiagnostics = CandidateComparisonDiagnostic.familyMismatchDiagnostics(packet)
    val clusteredEventClaimIds = packet.claimEventClusters.flatMap(_.memberClaimIds).toSet
    val unclusteredConcreteClaims =
      packet.claims.filter(claim => claim.family.isEvent && !clusteredEventClaimIds.contains(claim.id))
    val directEventUnclusteredClaims = unclusteredConcreteClaims.filter(hasDirectEventCause(_, packet.evidenceGraph))
    val unclusteredEventClaims = directEventUnclusteredClaims.filterNot(_.family == ClaimFamily.Evaluation)
    val verdictCarriersWithoutEventCauseOwner = directEventUnclusteredClaims.filter(_.family == ClaimFamily.Evaluation)
    val noDirectEventClaims = unclusteredConcreteClaims.filterNot(hasDirectEventCause(_, packet.evidenceGraph))
    val verdictOnlyEvaluationClaims = noDirectEventClaims.filter(_.family == ClaimFamily.Evaluation)
    val localConcreteClaims = noDirectEventClaims.filterNot(_.family == ClaimFamily.Evaluation)
    val localConcreteClaimDiagnostics =
      localConcreteClaims.map(localConcreteClaimDiagnostic(packet, _))
    val primaryPlayedComparisonIds = primaryPlayedComparisonRecords.map(_.ref.id).toSet
    val playedHasUnexplained =
      comparisonDiagnostics.exists(diagnostic =>
        primaryPlayedComparisonIds.contains(diagnostic.id) && diagnostic.hasUnexplainedEngineGap
      )
    val contextUnexplainedIds =
      comparisonDiagnostics
        .filter(diagnostic =>
          !primaryPlayedComparisonIds.contains(diagnostic.id) &&
            (diagnostic.hasUnexplainedEngineGap || diagnostic.hasSecondaryContextEngineGap)
        )
        .map(_.id)
    val secondaryContextIds =
      comparisonDiagnostics
        .filter(diagnostic =>
          !primaryPlayedComparisonIds.contains(diagnostic.id) &&
            diagnostic.hasSecondaryContextEngineGap
        )
        .map(_.id)
    val primaryPlayedCovered =
      comparisonDiagnostics.exists(diagnostic =>
        primaryPlayedComparisonIds.contains(diagnostic.id) &&
          !diagnostic.hasUnexplainedEngineGap &&
          diagnostic.causeSupport.exists(support =>
            support.role == RelativeCauseRole.PrimaryPlayedCause &&
              support.importance == RelativeCauseImportance.Primary
          )
      )
    val contextUnexplainedWithPrimaryCoverageIds =
      if primaryPlayedCovered then contextUnexplainedIds else Nil
    val contextUnexplainedWithoutPrimaryCoverageIds =
      if primaryPlayedCovered then Nil else contextUnexplainedIds
    val secondaryContextWithPrimaryCoverageIds =
      if primaryPlayedCovered then secondaryContextIds else Nil
    val secondaryContextWithoutPrimaryCoverageIds =
      if primaryPlayedCovered then Nil else secondaryContextIds
    val branchReplyProbeRequests =
      packet.probeRequests.filter(request => request.purpose.exists(branchReplyProbePurpose))
    val planTransitionWithoutSnapshotPairIds =
      packet.evidenceGraph.records.collect {
        case EvidenceRecord(ref, PlanTransitionEvidence(transition), _) if transition.transitionType == TransitionType.Opening =>
          ref.id
      }.distinct.sorted
    val branchReplyThreatLines =
      packet.candidateLines.count(_.role == LineNodeRole.Threat)
    val branchReplyThreatPressureRecords =
      packet.evidenceGraph.records.count {
        case EvidenceRecord(ref, _: ThreatPressureEvidence, _) =>
          ref.scope == EvidenceScope.ThreatLine || ref.line.exists(_.role == LineNodeRole.Threat)
        case _ =>
          false
      }
    val branchReplyThreatEpisodeRecords =
      packet.evidenceGraph.records.count {
        case EvidenceRecord(ref, _: ThreatEpisodeEvidence, _) =>
          ref.scope == EvidenceScope.ThreatLine || ref.line.exists(_.role == LineNodeRole.Threat)
        case _ =>
          false
      }
    val branchReplyProbeDiagnostics = packet.probeDiagnostics
    val admittedProbeResults =
      branchReplyProbeDiagnostics.filter(_.status == ProbeAdmissionStatus.Admitted)
    val rejectedProbeResults =
      branchReplyProbeDiagnostics.filter(_.status == ProbeAdmissionStatus.Rejected)
    val ignoredProbeResults =
      branchReplyProbeDiagnostics.filter(_.status == ProbeAdmissionStatus.Ignored)
    val diagnosedProbeIds = branchReplyProbeDiagnostics.map(_.probeId).toSet
    val pendingProbeRequests =
      branchReplyProbeRequests.filterNot(request => diagnosedProbeIds.contains(request.id))
    val branchReplyProbeLifecycleState =
      if branchReplyProbeRequests.isEmpty && branchReplyProbeDiagnostics.isEmpty then "none"
      else if pendingProbeRequests.nonEmpty && branchReplyProbeDiagnostics.isEmpty then "request_only_pending"
      else if pendingProbeRequests.nonEmpty then "partial_pending"
      else if admittedProbeResults.nonEmpty && rejectedProbeResults.isEmpty && ignoredProbeResults.isEmpty then "admitted"
      else if admittedProbeResults.isEmpty && rejectedProbeResults.nonEmpty then "rejected"
      else if admittedProbeResults.isEmpty && ignoredProbeResults.nonEmpty then "ignored"
      else "mixed"
    val tacticalDetectionFailures =
      comparisonDiagnostics.filter(tacticalDetectionFailure)
    val contextOutrankingDiagnostics =
      contextTacticalOutrankingDiagnostics(packet)
    SemanticCoverageMetrics(
      tacticalIdeas = countIdeas(packet, ChessIdeaFamily.Tactical),
      strategicIdeas = countIdeas(packet, ChessIdeaFamily.Strategic),
      pawnStructureIdeas = countIdeas(packet, ChessIdeaFamily.PawnStructure),
      openingIdeas = countIdeas(packet, ChessIdeaFamily.Opening),
      defensiveIdeas = countIdeas(packet, ChessIdeaFamily.Defensive),
      evaluationIdeas = countIdeas(packet, ChessIdeaFamily.Evaluation),
      conversionIdeas = countIdeas(packet, ChessIdeaFamily.Conversion),
      materialIdeas = countIdeas(packet, ChessIdeaFamily.Material),
      claimFamilies = packet.claims.map(_.family).toSet,
      claimCandidateFamilies = packet.claimLifecycle.map(_.family).toSet,
      claimCandidateFamilyCounts = packet.claimLifecycle.groupMapReduce(_.family)(_ => 1)(_ + _),
      claimLifecycleDiagnostics = packet.claimLifecycle,
      claimLifecycleStageCounts = claimLifecycleStageCounts(packet.claimLifecycle),
      claimLifecycleTruthCounts = claimLifecycleTruthCounts(packet.claimLifecycle),
      claimLifecycleRelativeCauseIdeaDroppedIds = claimLifecycleRelativeCauseIdeaDroppedIds(packet.claimLifecycle),
      claimLifecycleRelativeCauseDroppedByStage = claimLifecycleRelativeCauseDroppedByStage(packet.claimLifecycle),
      relativeCauseFamilyMismatchDiagnostics = familyMismatchDiagnostics,
      relativeCauseFamilyMismatchKindCounts =
        familyMismatchDiagnostics.flatMap(_.mismatchKinds).groupMapReduce(identity)(_ => 1)(_ + _),
      hasRelativeCauseFamilyMismatch = familyMismatchDiagnostics.nonEmpty,
      claimSupportClusters = packet.claimSupportClusters.size,
      clusteredAnchorClaims = packet.claimSupportClusters.flatMap(_.anchorClaimIds).distinct.size,
      clusteredSupportingClaims = packet.claimSupportClusters.flatMap(_.supportingClaimIds).distinct.size,
      clusteredConstrainingClaims = packet.claimSupportClusters.flatMap(_.constrainingClaimIds).distinct.size,
      clusteredLongTermSupportIdeas = packet.claimSupportClusters.flatMap(_.ideas.map(_.id)).distinct.size,
      claimSupportClusterFamilies = packet.claimSupportClusters.flatMap(_.families).toSet,
      claimSupportClusterLayers = packet.claimSupportClusters.flatMap(_.presentLayers).toSet,
      claimEventClusters = packet.claimEventClusters.size,
      clusteredEventClaims = packet.claimEventClusters.flatMap(_.memberClaimIds).distinct.size,
      clusteredEventCauseClaims = packet.claimEventClusters.flatMap(_.causeClaimIds).distinct.size,
      clusteredEventEvaluationClaims = packet.claimEventClusters.flatMap(_.evaluationClaimIds).distinct.size,
      clusteredEventWitnessClaims = packet.claimEventClusters.flatMap(_.witnessClaimIds).distinct.size,
      clusteredEventIdeas = packet.claimEventClusters.flatMap(_.ideas.map(_.id)).distinct.size,
      relatedEventSupportClusters = packet.claimEventClusters.flatMap(_.relatedSupportClusterIds).distinct.size,
      claimEventClusterFamilies = packet.claimEventClusters.flatMap(_.families).toSet,
      claimEventClusterLayers = packet.claimEventClusters.flatMap(_.presentLayers).toSet,
      claimEventClusterKinds = packet.claimEventClusters.map(_.kind).toSet,
      claimEventClusterCauses = packet.claimEventClusters.map(_.causeKind).toSet,
      claimEventClusterRoles = packet.claimEventClusters.map(_.causeRole).toSet,
      claimEventInteractionKinds = packet.claimEventClusters.flatMap(_.interactions.map(_.kind)).toSet,
      unclusteredEventClaims = unclusteredEventClaims.size,
      unclusteredEventClaimFamilies = unclusteredEventClaims.map(_.family).toSet,
      verdictCarriersWithoutEventCauseOwner = verdictCarriersWithoutEventCauseOwner.size,
      verdictOnlyEvaluationClaims = verdictOnlyEvaluationClaims.size,
      localConcreteClaims = localConcreteClaims.size,
      localConcreteClaimFamilies = localConcreteClaims.map(_.family).toSet,
      localConcreteClaimDiagnostics = localConcreteClaimDiagnostics,
      boardAnchorFacts = boardAnchorCount(packet),
      boardAttackDefenseFacts = boardAttackDefenseCount(packet),
      lineReplayFacts = lineReplayCount(packet),
      lineEventFacts = lineEventCount(packet),
      lineConsequenceFacts = lineConsequenceCount(packet),
      relativeCauseProofs = relativeCauseProofCount(packet),
      hasRelativeAssessment = packet.relativeAssessments.nonEmpty,
      candidateComparisonFacts = packet.evidenceGraph.records.count(_.ref.layer == EvidenceLayer.CandidateComparison),
      relativeCauseFacts = packet.evidenceGraph.records.count(_.ref.layer == EvidenceLayer.RelativeCause),
      moveVerdictCertifications = packet.evidenceGraph.records.count(_.ref.layer == EvidenceLayer.MoveVerdictCertification),
      playedRelatedComparisonFacts = playedComparisonRecords.size,
      primaryPlayedComparisonFacts = primaryPlayedComparisonRecords.size,
      playedRelativeCauseFacts = playedCauseRecords.size,
      planTransitionWithoutSnapshotPairIds = planTransitionWithoutSnapshotPairIds,
      branchReplyProbeRequests = branchReplyProbeRequests.size,
      branchReplyProbeMoves = branchReplyProbeRequests.flatMap(_.candidateMove).distinct.sorted,
      branchReplyThreatLines = branchReplyThreatLines,
      branchReplyThreatPressureRecords = branchReplyThreatPressureRecords,
      branchReplyThreatEpisodeRecords = branchReplyThreatEpisodeRecords,
      branchReplyProbeAdmittedResults = admittedProbeResults.size,
      branchReplyProbeRejectedResults = rejectedProbeResults.size,
      branchReplyProbeIgnoredResults = ignoredProbeResults.size,
      branchReplyProbePendingRequests = pendingProbeRequests.size,
      branchReplyProbeRejectReasons = rejectedProbeResults.flatMap(_.reasonCodes).groupMapReduce(identity)(_ => 1)(_ + _),
      branchReplyProbeAdmittedMoves = admittedProbeResults.flatMap(_.candidateMove).distinct.sorted,
      branchReplyProbeRejectedMoves = rejectedProbeResults.flatMap(_.candidateMove).distinct.sorted,
      branchReplyProbePendingMoves = pendingProbeRequests.flatMap(_.candidateMove).distinct.sorted,
      branchReplyProbeLifecycleState = branchReplyProbeLifecycleState,
      hasPendingBranchReplyDepth = pendingProbeRequests.nonEmpty,
      hasUnexplainedEngineGap = playedHasUnexplained,
      hasPlayedUnexplainedEngineGap = playedHasUnexplained,
      hasContextUnexplainedEngineGap = contextUnexplainedWithoutPrimaryCoverageIds.nonEmpty,
      hasSecondaryContextEngineGap = secondaryContextIds.nonEmpty,
      comparisonDiagnostics = comparisonDiagnostics,
      secondaryContextComparisonIds = secondaryContextIds,
      secondaryContextWithPrimaryCoverageIds = secondaryContextWithPrimaryCoverageIds,
      secondaryContextWithoutPrimaryCoverageIds = secondaryContextWithoutPrimaryCoverageIds,
      contextUnexplainedComparisonIds = contextUnexplainedIds,
      contextUnexplainedWithPrimaryCoverageIds = contextUnexplainedWithPrimaryCoverageIds,
      contextUnexplainedWithoutPrimaryCoverageIds = contextUnexplainedWithoutPrimaryCoverageIds,
      openingApplicabilityDiagnostics = openingApplicabilityDiagnostics(packet),
      openingSupportDiagnostics = openingSupportDiagnostics(packet),
      tacticalDetectionFailureComparisonIds = tacticalDetectionFailures.map(_.id),
      tacticalDetectionFailureBlockedStages =
        tacticalDetectionFailures.flatMap(_.tacticalLossTrace.blockedAt).groupMapReduce(identity)(_ => 1)(_ + _),
      hasTacticalDetectionFailure = tacticalDetectionFailures.nonEmpty,
      contextTacticalOutrankingDiagnostics = contextOutrankingDiagnostics,
      contextTacticalOutranksPrimaryPlayed = contextOutrankingDiagnostics.nonEmpty,
      hasVerdict = packet.ideaVerdict.flatMap(_.verdict).nonEmpty,
      hasCandidateSetComparison = packet.relativeAssessments.exists(_.comparison.candidateSet.nonEmpty),
      hasOnlyMoveSignal = packet.relativeAssessments.exists(_.comparison.candidateSet.exists(_.onlyMove)),
      hasForcedLineTheme = packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.hasForcedTheme
        case _                                               => false
      }
    )

  private def branchReplyProbePurpose(purpose: ProbePurpose): Boolean =
    purpose match
      case ProbePurpose.ReplyMultipv | ProbePurpose.DefenseReplyMultipv | ProbePurpose.ConvertReplyMultipv |
          ProbePurpose.RecaptureBranches | ProbePurpose.KeepTensionBranches | ProbePurpose.FreeTempoBranches =>
        true

  private def countIdeas(packet: EvidenceBackedJudgmentPacket, family: ChessIdeaFamily): Int =
    packet.ideas.count(_.ref.family == family)

  private def claimLifecycleStageCounts(
      diagnostics: List[ClaimLifecycleDiagnostic]
  ): Map[ClaimLifecycleStage, Int] =
    diagnostics.flatMap(_.stages).groupMapReduce(identity)(_ => 1)(_ + _)

  private def claimLifecycleTruthCounts(
      diagnostics: List[ClaimLifecycleDiagnostic]
  ): Map[ClaimLifecycleTruthStatus, Int] =
    diagnostics.flatMap(_.truthStatus).groupMapReduce(identity)(_ => 1)(_ + _)

  private def claimLifecycleRelativeCauseIdeaDroppedIds(
      diagnostics: List[ClaimLifecycleDiagnostic]
  ): List[String] =
    diagnostics
      .filter(diagnostic =>
        diagnostic.relativeCauses.nonEmpty &&
          diagnostic.ideaIds.nonEmpty &&
          !diagnostic.finalPacketIncluded
      )
      .map(_.candidateId)
      .distinct
      .sorted

  private def claimLifecycleRelativeCauseDroppedByStage(
      diagnostics: List[ClaimLifecycleDiagnostic]
  ): Map[ClaimLifecycleStage, List[String]] =
    val dropStages =
      Set(
        ClaimLifecycleStage.TruthRejected,
        ClaimLifecycleStage.TruthDeferred,
        ClaimLifecycleStage.DedupeDropped,
        ClaimLifecycleStage.ArbitrationSuppressed
      )
    diagnostics
      .filter(diagnostic =>
        diagnostic.relativeCauses.nonEmpty &&
          diagnostic.ideaIds.nonEmpty &&
          !diagnostic.finalPacketIncluded
      )
      .flatMap(diagnostic => diagnostic.stages.filter(dropStages.contains).map(_ -> diagnostic.candidateId))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.distinct.sorted)
      .toMap

  private def boardAnchorCount(packet: EvidenceBackedJudgmentPacket): Int =
    packet.evidenceGraph.records.collect { case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
      payload.boardAnchorCount
    }.sum

  private def boardAttackDefenseCount(packet: EvidenceBackedJudgmentPacket): Int =
    packet.evidenceGraph.records.collect { case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
      payload.attackDefenseCount
    }.sum

  private def lineReplayCount(packet: EvidenceBackedJudgmentPacket): Int =
    packet.evidenceGraph.records.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
      payload.lineReplayCount
    }.sum

  private def lineEventCount(packet: EvidenceBackedJudgmentPacket): Int =
    packet.evidenceGraph.records.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
      payload.lineEventCount
    }.sum

  private def lineConsequenceCount(packet: EvidenceBackedJudgmentPacket): Int =
    packet.evidenceGraph.records.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
      payload.lineConsequenceCount
    }.sum

  private def relativeCauseProofCount(packet: EvidenceBackedJudgmentPacket): Int =
    packet.evidenceGraph.records.map {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        if cause.hasOwnedTypedDepth then 1 else 0
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.count(_.hasOwnedTypedDepth)
      case _ =>
        0
    }.sum

  private def tacticalDetectionFailure(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.tacticalLossTrace.applicable &&
      !diagnostic.tacticalLossTrace.stages.exists(stage =>
        stage.present &&
          (stage.stage == TacticalLossStage.RelationWitness ||
            stage.stage == TacticalLossStage.ThreatEpisode ||
            stage.stage == TacticalLossStage.TacticalMechanism ||
            stage.stage == TacticalLossStage.RelativeCause ||
            stage.stage == TacticalLossStage.TacticalIdea ||
            stage.stage == TacticalLossStage.TacticalClaim)
      )

  private def contextTacticalOutrankingDiagnostics(
      packet: EvidenceBackedJudgmentPacket
  ): List[ContextTacticalOutrankingDiagnostic] =
    val rankedClaims = packet.claims.zipWithIndex
    val primaryPlayedClaims =
      rankedClaims.filter { case (claim, _) =>
        JudgmentSubjectBinding.primaryPlayed(JudgmentSubjectBinding.claimBinding(packet, claim))
      }
    val bestPrimaryPlayedRank = primaryPlayedClaims.map(_._2).minOption
    rankedClaims.flatMap { case (contextClaim, contextIndex) =>
      val contextBinding = JudgmentSubjectBinding.claimBinding(packet, contextClaim)
      Option
        .when(
          contextClaim.family == ClaimFamily.Tactical &&
            contextBinding == SubjectBindingClass.ContextPlayed &&
            bestPrimaryPlayedRank.exists(contextIndex < _)
        ) {
          val outrankedPrimaryClaims =
            primaryPlayedClaims.filter { case (_, primaryIndex) => contextIndex < primaryIndex }
          val contextSalience = contextClaim.salience.map(_.score)
          val primarySaliences = outrankedPrimaryClaims.flatMap(_._1.salience.map(_.score))
          Option.when(outrankedPrimaryClaims.nonEmpty)(
            ContextTacticalOutrankingDiagnostic(
              contextClaimId = contextClaim.id,
              contextRank = contextIndex + 1,
              contextSubjectBinding = contextBinding,
              contextTier = PlayerFacingClaimPolicy.tier(packet, contextClaim),
              contextSalienceScore = contextSalience,
              contextComparisonIds = comparisonIdsForClaim(packet, contextClaim),
              contextComparisonKinds = comparisonKindsForClaim(packet, contextClaim),
              contextCauseRoles = relativeCauseFactsForClaim(packet, contextClaim).map(_.role).distinct,
              contextCauseSourceSides = relativeCauseFactsForClaim(packet, contextClaim).map(_.sourceSide).distinct,
              contextCauseImportances = relativeCauseFactsForClaim(packet, contextClaim).map(_.importance).distinct,
              contextCauseEventLines = relativeCauseFactsForClaim(packet, contextClaim).map(_.eventLine).distinct,
              contextEventClusterIds = eventClusterIdsForClaim(packet, contextClaim),
              outrankedPrimaryClaimIds = outrankedPrimaryClaims.map(_._1.id),
              outrankedPrimaryRanks = outrankedPrimaryClaims.map(_._2 + 1),
              outrankedPrimaryFamilies = outrankedPrimaryClaims.map(_._1.family).distinct,
              outrankedPrimarySalienceScores = primarySaliences,
              outrankedPrimaryEventClusterIds = outrankedPrimaryClaims.flatMap { case (claim, _) =>
                eventClusterIdsForClaim(packet, claim)
              }.distinct.sorted,
              salienceDeltaToBestPrimary =
                contextSalience.zip(primarySaliences.maxOption).map { case (context, primary) => context - primary }
            )
          )
        }
        .flatten
    }

  private def relativeCauseFactsForClaim(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): List[RelativeCauseFact] =
    claimRecords(packet, claim).flatMap {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        List(cause)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes
      case _ =>
        Nil
    }

  private def comparisonKindsForClaim(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): List[CandidateComparisonKind] =
    claimRecords(packet, claim).flatMap {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        List(fact.kind)
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        List(cause.comparisonKind)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        (certification.primaryComparison.kind :: certification.causes.map(_.comparisonKind)).distinct
      case _ =>
        Nil
    }.distinct

  private def comparisonIdsForClaim(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): List[String] =
    claimRecords(packet, claim).flatMap {
      case EvidenceRecord(ref, CandidateComparisonEvidence(_), _) =>
        List(ref.id)
      case EvidenceRecord(_, RelativeCauseFactEvidence(_), parents) =>
        parents.filter(_.layer == EvidenceLayer.CandidateComparison).map(_.id)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(_), parents) =>
        parents.filter(_.layer == EvidenceLayer.CandidateComparison).map(_.id)
      case _ =>
        Nil
    }.distinct.sorted

  private def eventClusterIdsForClaim(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): List[String] =
    packet.claimEventClusters
      .filter(cluster =>
        cluster.memberClaimIds.contains(claim.id) ||
          cluster.causeClaimIds.contains(claim.id) ||
          cluster.evaluationClaimIds.contains(claim.id) ||
          cluster.witnessClaimIds.contains(claim.id)
      )
      .map(_.id)
      .distinct
      .sorted

  private def claimRecords(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): List[EvidenceRecord] =
    claim.evidence.flatMap(ref => packet.evidenceGraph.byId.get(ref.id))

  private def hasDirectEventCause(claim: ClaimSeed, graph: TypedEvidenceGraph): Boolean =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id)).exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        ClaimEventCluster.kindForCause(cause.kind).nonEmpty
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(cause => ClaimEventCluster.kindForCause(cause.kind).nonEmpty)
      case _ =>
        false
    }

  private def localConcreteClaimDiagnostic(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): LocalConcreteClaimDiagnostic =
    val records = claim.evidence.flatMap(ref => packet.evidenceGraph.byId.get(ref.id))
    val layers = records.map(_.ref.layer).toSet
    val threatEpisodes = records.collect { case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
      payload
    }
    val threatEpisode = threatEpisodes.headOption
    val threatEpisodeDiagnostics =
      threatEpisodes.map(payload =>
        LocalThreatEpisodeDiagnostic(
          episodeId = payload.episode.episodeId,
          sourceThreatIndex = payload.episode.sourceThreatIndex,
          sideUnderPressure = payload.sideUnderPressure.name,
          kind = payload.episode.kind.toString,
          severity = payload.episode.severity,
          driver = payload.episode.driver,
          defenseRequired = payload.defenseRequired,
          prophylaxisNeeded = payload.prophylaxisNeeded,
          onlyDefense = payload.onlyDefense,
          maxWinPercentLossIfIgnored = payload.maxWinPercentLossIfIgnored,
          insufficientData = payload.insufficientData
        )
      )
    LocalConcreteClaimDiagnostic(
      id = claim.id,
      family = claim.family,
      subject = claim.subject,
      primaryLine = claim.primaryLine,
      subjectMove = claim.subjectMove,
      scope = claim.scope,
      localKind = localConcreteKind(claim, layers),
      evidenceLayers = layers,
      evidenceIds = claim.evidence.map(_.id),
      engineVerdict = claim.engineComparison.map(_.verdict),
      engineWinPercentLossForMover = claim.engineComparison.map(_.winPercentLossForMover),
      threatSeverity = threatEpisode.map(_.episode.severity),
      threatCount = Option.when(threatEpisodes.nonEmpty)(threatEpisodes.size),
      threatDefenseRequired = threatEpisode.map(_.defenseRequired),
      threatProphylaxisNeeded = threatEpisode.map(_.prophylaxisNeeded),
      threatOnlyDefense = threatEpisode.flatMap(_.onlyDefense),
      threatMaxWinPercentLossIfIgnored = threatEpisode.flatMap(_.maxWinPercentLossIfIgnored),
      threatPrimaryDriver = threatEpisode.map(_.episode.driver),
      threatInsufficientData = threatEpisode.map(_.insufficientData),
      threatEpisodes = threatEpisodeDiagnostics,
      nearbyEventClusterIds = nearbyEventClusters(packet, claim)
    )

  private def localConcreteKind(
      claim: ClaimSeed,
      layers: Set[EvidenceLayer]
  ): LocalConcreteClaimKind =
    claim.family match
      case ClaimFamily.Defensive if layers.contains(EvidenceLayer.ThreatPressure) =>
        LocalConcreteClaimKind.DefensiveThreat
      case ClaimFamily.Tactical
          if layers.contains(EvidenceLayer.TacticalMechanism) || layers.contains(EvidenceLayer.Relation) ||
            layers.contains(EvidenceLayer.MoveMotif) =>
        LocalConcreteClaimKind.TacticalLine
      case ClaimFamily.Tactical =>
        LocalConcreteClaimKind.TacticalEngineContext
      case ClaimFamily.Conversion =>
        LocalConcreteClaimKind.ConversionContext
      case ClaimFamily.Material =>
        LocalConcreteClaimKind.MaterialContext
      case _ =>
        LocalConcreteClaimKind.ConcreteContext

  private def nearbyEventClusters(
      packet: EvidenceBackedJudgmentPacket,
      claim: ClaimSeed
  ): List[String] =
    val lines = (claim.primaryLine.toList ++ claim.evidence.flatMap(_.line)).distinct
    val moves = (claim.subjectMove.toList ++ lines.map(_.rootMove)).map(normalizeMove).toSet
    packet.claimEventClusters
      .filter(cluster =>
        lines.exists(line => line == cluster.eventLine || line == cluster.referenceLine || line == cluster.candidateLine) ||
          moves.contains(normalizeMove(cluster.eventRootMove)) ||
          moves.contains(normalizeMove(cluster.referenceLine.rootMove)) ||
          moves.contains(normalizeMove(cluster.candidateLine.rootMove))
      )
      .map(_.id)
      .distinct
      .sorted

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def openingApplicabilityDiagnostics(
      packet: EvidenceBackedJudgmentPacket
  ): List[OpeningApplicabilityDiagnostic] =
    packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, ApplicabilityAssessmentEvidence(assessment), parents) =>
        val anchors =
          parents
            .flatMap(parent => packet.evidenceGraph.byId.get(parent.id))
            .collect { case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) => anchor }
        val supportedAnchors =
          anchors.filter(anchor =>
            assessment.supportedThemes.contains(anchor.theme) &&
              anchor.canCorroborateOpeningPrior
          )
        OpeningApplicabilityDiagnostic(
          id = ref.id,
          applicability = assessment.applicability,
          status = assessment.status,
          observedThemes = assessment.observedThemes,
          supportedThemes = assessment.supportedThemes,
          unverifiedPriorThemes = assessment.unverifiedPriorThemes,
          observedOnlyThemes = assessment.observedOnlyThemes,
          priorMatchSources = assessment.priorMatchSources,
          certifyingPriorPresent = assessment.hasCertifyingPriorEvidence,
          anchorSourceLayers = anchors.map(_.sourceLayer).distinct,
          anchorSignals = anchors.map(_.signal).distinct,
          supportedAnchorSourceLayers = supportedAnchors.map(_.sourceLayer).distinct,
          supportedAnchorSignals = supportedAnchors.map(_.signal).distinct,
          internalAnchorAligned = assessment.hasInternalAnchorAlignment
        )
    }

  private def openingSupportDiagnostics(
      packet: EvidenceBackedJudgmentPacket
  ): List[OpeningSupportDiagnostic] =
    val contextRecords =
      packet.evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, OpeningContextEvidence(_, _, _, _), _) => record
      }
    val assessmentRecords =
      packet.evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, ApplicabilityAssessmentEvidence(_), _) => record
      }
    val anchorRecords =
      packet.evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, FeatureAnchorEvidence(_), _) => record
      }
    val openingIdeaIds =
      packet.ideas.filter(_.ref.family == ChessIdeaFamily.Opening).map(_.ref.id).distinct.sorted
    val openingClaims =
      packet.claims.filter(_.family == ClaimFamily.Opening)
    val openingClaimIds =
      openingClaims.map(_.id).distinct.sorted
    val openingCandidateClaimIds =
      packet.claimLifecycle.filter(_.family == ClaimFamily.Opening).map(_.candidateId).distinct.sorted
    val openingClaimIdSet = openingClaimIds.toSet
    val supportClusterIds =
      packet.claimSupportClusters
        .filter(cluster =>
          cluster.families.contains(ClaimFamily.Opening) ||
            cluster.ideas.exists(_.family == ChessIdeaFamily.Opening) ||
            (cluster.anchorClaimIds ++ cluster.supportingClaimIds ++ cluster.constrainingClaimIds).exists(openingClaimIdSet)
        )
        .map(_.id)
        .distinct
        .sorted
    val assessments =
      assessmentRecords.collect { case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), _) => assessment }
    val contexts =
      contextRecords.collect { case EvidenceRecord(_, OpeningContextEvidence(identity, _, recognition, themePriorSelection), _) =>
        (identity, recognition, themePriorSelection)
      }
    val priorSelections = contexts.flatMap(_._3).distinct
    val themePriors = priorSelections.map(_.prior).distinct
    val supportedAnchorRecords =
      assessmentRecords.flatMap {
        case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), parents) =>
          parents
            .flatMap(parent => packet.evidenceGraph.byId.get(parent.id))
            .collect {
              case record @ EvidenceRecord(_, FeatureAnchorEvidence(anchor), _)
                  if assessment.supportedThemes.contains(anchor.theme) && anchor.canCorroborateOpeningPrior =>
                record
            }
        case _ => Nil
      }.distinctBy(_.ref.id)
    val anchors = anchorRecords.collect { case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) => anchor }
    val supportedAnchors =
      supportedAnchorRecords.collect { case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) => anchor }
    val openingSpecificAssessment =
      assessments.exists(assessment =>
        assessment.applicability == FeatureApplicability.OpeningRelevant ||
          assessment.supportedThemes.nonEmpty ||
          assessment.unverifiedPriorThemes.nonEmpty
      )
    val hasOpeningEvidence =
      contextRecords.nonEmpty || openingSpecificAssessment || openingIdeaIds.nonEmpty ||
        openingClaimIds.nonEmpty || openingCandidateClaimIds.nonEmpty
    if !hasOpeningEvidence then Nil
    else
      val themePriorPresent = priorSelections.nonEmpty
      val internalAnchorAligned = assessments.exists(_.hasInternalAnchorAlignment)
      List(
        OpeningSupportDiagnostic(
          contextIds = contextRecords.map(_.ref.id).distinct.sorted,
          assessmentIds = assessmentRecords.map(_.ref.id).distinct.sorted,
          anchorIds = anchorRecords.map(_.ref.id).distinct.sorted,
          supportedAnchorIds = supportedAnchorRecords.map(_.ref.id).distinct.sorted,
          ideaIds = openingIdeaIds,
          claimIds = openingClaimIds,
          candidateClaimIds = openingCandidateClaimIds,
          supportClusterIds = supportClusterIds,
          identityPresent = contexts.exists(_._1.nonEmpty),
          recognitionPresent = contexts.exists(_._2.nonEmpty),
          themePriorPresent = themePriorPresent,
          priorLineages = themePriors.flatMap(_.lineage).distinct.sorted,
          requestedPriorLineages = priorSelections.flatMap(_.requestedLineage).distinct.sorted,
          canonicalPriorLineages = priorSelections.flatMap(_.canonicalLineage).distinct.sorted,
          priorMatchSources = priorSelections.map(_.matchSource).distinct,
          openingSpecificPrior = priorSelections.exists(_.openingSpecific),
          certifyingPriorPresent = priorSelections.exists(_.canCertifyOpeningClaim),
          certifyingPriorMatchSources = priorSelections.map(_.matchSource).filter(_.canCertifyOpeningClaim).distinct,
          priorFamilies = themePriors.flatMap(_.family).distinct,
          typicalPawnStructures = themePriors.flatMap(_.typicalPawnStructures).distinct.sorted,
          centerBreaks = themePriors.flatMap(_.centerBreaks).distinct.sorted,
          developmentPriorities = themePriors.flatMap(_.developmentPriorities).distinct.sorted,
          gambitCompensation = themePriors.exists(_.gambitCompensation),
          strategicPlanPriors = themePriors.flatMap(_.strategicPlanPriors).distinct.sorted,
          observedThemes = assessments.flatMap(_.observedThemes).distinct,
          supportedThemes = assessments.flatMap(_.supportedThemes).distinct,
          unverifiedPriorThemes = assessments.flatMap(_.unverifiedPriorThemes).distinct,
          observedOnlyThemes = assessments.flatMap(_.observedOnlyThemes).distinct,
          anchorSourceLayers = anchors.map(_.sourceLayer).distinct,
          supportedAnchorSourceLayers = supportedAnchors.map(_.sourceLayer).distinct,
          applicabilities = assessments.map(_.applicability).distinct,
          assessmentStatuses = assessments.map(_.status).distinct,
          internalAnchorAligned = internalAnchorAligned
        )
      )

  private def playedRelatedComparisonRecords(packet: EvidenceBackedJudgmentPacket): List[EvidenceRecord] =
    val playedMoves = JudgmentSubjectBinding.packetPlayedMoves(packet)
    packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _)
          if JudgmentSubjectBinding.playedRelated(JudgmentSubjectBinding.recordBinding(record, playedMoves)) =>
        record
    }

  private def primaryPlayedCandidateComparisonRecords(packet: EvidenceBackedJudgmentPacket): List[EvidenceRecord] =
    val playedMoves = JudgmentSubjectBinding.packetPlayedMoves(packet)
    packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _)
          if JudgmentSubjectBinding.primaryPlayed(JudgmentSubjectBinding.recordBinding(record, playedMoves)) =>
        record
    }

  private def playedRelativeCauseRecords(packet: EvidenceBackedJudgmentPacket): List[EvidenceRecord] =
    val playedMoves = JudgmentSubjectBinding.packetPlayedMoves(packet)
    packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if JudgmentSubjectBinding.playedRelated(JudgmentSubjectBinding.recordBinding(record, playedMoves)) =>
        record
      case record @ EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _)
          if certification.causes.nonEmpty &&
            JudgmentSubjectBinding.playedRelated(
              JudgmentSubjectBinding.recordBinding(record, playedMoves)
            ) =>
        record
    }.distinctBy(_.ref.id)

enum JudgmentGraphLayer:
  case InputGraph
  case PositionEvidence
  case LineEvaluationEvidence
  case TransitionEvidence
  case StrategicPlanOpeningEvidence
  case RelativeChoiceAssessment
  case IdeaSynthesis
  case ClaimSynthesis
  case PacketBoundary

enum JudgmentGraphSlot:
  case BeforePositionNode
  case AfterPlayedPositionNode
  case AfterReferencePositionNode
  case AfterAlternativePositionNode
  case PlayedLineNode
  case BestReferenceLineNode
  case AlternativeLineNode
  case PlayedTransitionEdge
  case ReferenceTransitionEdge
  case AlternativeTransitionEdge
  case BoardFact
  case BoardAnchorFact
  case SinglePositionFact
  case BeforeSinglePositionFact
  case AfterPlayedSinglePositionFact
  case AfterReferenceSinglePositionFact
  case PawnStructureFact
  case ThreatPressureFact
  case ThreatEpisodeFact
  case LineFact
  case LineReplayFact
  case LineEventFact
  case LineConsequenceFact
  case EvalFact
  case LegalReplayLineFact
  case MoveMotifFact
  case TacticalMechanismFact
  case MoveTransitionFact
  case RelationFact
  case StructuralDeltaFact
  case StructuralSignalFact
  case StructuralConsequenceFact
  case StructuralMeaningfulConsequenceFact
  case StrategicFact
  case StrategicMechanismFact
  case OpeningContextFact
  case FeatureAnchorFact
  case ApplicabilityAssessmentFact
  case PlanPressureFact
  case PlanTransitionFact
  case CandidateComparisonFact
  case RelativeAssessmentFact
  case CounterfactualFact
  case RelativeCauseFact
  case RelativeCauseProof
  case MoveVerdictCertificationFact
  case TacticalIdea
  case StrategicIdea
  case PawnStructureIdea
  case OpeningIdea
  case DefensiveIdea
  case ConversionIdea
  case MaterialIdea
  case EvaluationIdea
  case TacticalClaim
  case StrategicClaim
  case PawnStructureClaim
  case OpeningClaim
  case PlanClaim
  case DefensiveClaim
  case ConversionClaim
  case MaterialClaim
  case EvaluationClaim
  case IdeaVerdictSplit
  case ClaimSupportCluster
  case ClaimEventCluster
  case EvidenceLossDiagnostics

enum JudgmentGraphOwner:
  case NodeLineTransitionAssembler
  case PositionFactNormalizer
  case SinglePositionFactNormalizer
  case LineFactNormalizer
  case EvalFactNormalizer
  case MoveMotifNormalizer
  case TacticalMechanismAssembler
  case MoveTransitionNormalizer
  case RelationFactNormalizer
  case StrategicFactNormalizer
  case StrategicMechanismAssembler
  case OpeningContextFactNormalizer
  case FeatureApplicabilityAssembler
  case TransitionFactNormalizer
  case RelativeAssessmentAssembler
  case ChessIdeaAssembler
  case ClaimSeedAssembler
  case JudgmentPacketBuilder
  case EvidenceLossDiagnostics

final case class JudgmentGraphSlotStatus(
    slot: JudgmentGraphSlot,
    owner: JudgmentGraphOwner,
    present: Boolean,
    applicable: Boolean = true
)

final case class JudgmentLayerGapMetric(
    layer: JudgmentGraphLayer,
    slots: List[JudgmentGraphSlotStatus]
):
  def totalSlots: Int = slots.count(_.applicable)
  def presentSlots: Int = slots.count(slot => slot.applicable && slot.present)
  def missingSlots: List[JudgmentGraphSlotStatus] = slots.filter(slot => slot.applicable && !slot.present)
  def gapPercent: Double =
    if totalSlots == 0 then 0d
    else missingSlots.size.toDouble / totalSlots.toDouble * 100d

final case class JudgmentLayerGapProfile(
    layers: List[JudgmentLayerGapMetric]
):
  def totalSlots: Int = layers.map(_.totalSlots).sum
  def presentSlots: Int = layers.map(_.presentSlots).sum
  def missingSlots: List[JudgmentGraphSlotStatus] = layers.flatMap(_.missingSlots)
  def overallGapPercent: Double =
    if totalSlots == 0 then 0d
    else missingSlots.size.toDouble / totalSlots.toDouble * 100d

object JudgmentLayerGapProfile:
  def fromPacket(packet: EvidenceBackedJudgmentPacket): JudgmentLayerGapProfile =
    JudgmentLayerGapProfile(
      layers = List(
        layer(
          JudgmentGraphLayer.InputGraph,
          slot(
            JudgmentGraphSlot.BeforePositionNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.positions.exists(_.role == PositionNodeRole.Before)
          ),
          slot(
            JudgmentGraphSlot.AfterPlayedPositionNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.positions.exists(_.role == PositionNodeRole.AfterPlayed)
          ),
          slot(
            JudgmentGraphSlot.AfterReferencePositionNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.positions.exists(_.role == PositionNodeRole.AfterReference)
          ),
          slot(
            JudgmentGraphSlot.AfterAlternativePositionNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.positions.exists(_.role == PositionNodeRole.AfterAlternative)
          ),
          slot(
            JudgmentGraphSlot.PlayedLineNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.candidateLines.exists(_.role == LineNodeRole.Played)
          ),
          slot(
            JudgmentGraphSlot.BestReferenceLineNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.candidateLines.exists(_.role == LineNodeRole.BestReference)
          ),
          slot(
            JudgmentGraphSlot.AlternativeLineNode,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.candidateLines.exists(_.role == LineNodeRole.Alternative)
          ),
          slot(
            JudgmentGraphSlot.PlayedTransitionEdge,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.transitions.exists(_.role == TransitionEdgeRole.Played)
          ),
          slot(
            JudgmentGraphSlot.ReferenceTransitionEdge,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.transitions.exists(_.role == TransitionEdgeRole.Reference)
          ),
          slot(
            JudgmentGraphSlot.AlternativeTransitionEdge,
            JudgmentGraphOwner.NodeLineTransitionAssembler,
            packet.transitions.exists(_.role == TransitionEdgeRole.Alternative)
          )
        ),
        layer(
          JudgmentGraphLayer.PositionEvidence,
          globalEvidenceLayerSlot(packet, JudgmentGraphSlot.BoardFact, JudgmentGraphOwner.PositionFactNormalizer, EvidenceLayer.Board),
          slot(
            JudgmentGraphSlot.BoardAnchorFact,
            JudgmentGraphOwner.PositionFactNormalizer,
            hasBoardAnchor(packet),
            hasEvidenceLayer(packet, EvidenceLayer.Board)
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.SinglePositionFact,
            JudgmentGraphOwner.SinglePositionFactNormalizer,
            EvidenceLayer.SinglePosition
          ),
          globalPositionEvidenceSlot(
            packet,
            JudgmentGraphSlot.BeforeSinglePositionFact,
            JudgmentGraphOwner.SinglePositionFactNormalizer,
            EvidenceLayer.SinglePosition,
            PositionNodeRole.Before
          ),
          globalPositionEvidenceSlot(
            packet,
            JudgmentGraphSlot.AfterPlayedSinglePositionFact,
            JudgmentGraphOwner.SinglePositionFactNormalizer,
            EvidenceLayer.SinglePosition,
            PositionNodeRole.AfterPlayed
          ),
          globalPositionEvidenceSlot(
            packet,
            JudgmentGraphSlot.AfterReferenceSinglePositionFact,
            JudgmentGraphOwner.SinglePositionFactNormalizer,
            EvidenceLayer.SinglePosition,
            PositionNodeRole.AfterReference
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.PawnStructureFact,
            JudgmentGraphOwner.StrategicFactNormalizer,
            EvidenceLayer.PawnStructure
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.ThreatPressureFact,
            JudgmentGraphOwner.StrategicFactNormalizer,
            EvidenceLayer.ThreatPressure
          ),
          slot(
            JudgmentGraphSlot.ThreatEpisodeFact,
            JudgmentGraphOwner.StrategicFactNormalizer,
            hasThreatEpisode(packet),
            hasThreatPressureEpisodeSource(packet)
          )
        ),
        layer(
          JudgmentGraphLayer.LineEvaluationEvidence,
          globalEvidenceLayerSlot(packet, JudgmentGraphSlot.LineFact, JudgmentGraphOwner.LineFactNormalizer, EvidenceLayer.Line),
          slot(
            JudgmentGraphSlot.LineReplayFact,
            JudgmentGraphOwner.LineFactNormalizer,
            hasLineReplay(packet),
            hasEvidenceLayer(packet, EvidenceLayer.Line)
          ),
          slot(
            JudgmentGraphSlot.LineEventFact,
            JudgmentGraphOwner.LineFactNormalizer,
            hasLineEvent(packet),
            hasEvidenceLayer(packet, EvidenceLayer.Line)
          ),
          slot(
            JudgmentGraphSlot.LineConsequenceFact,
            JudgmentGraphOwner.LineFactNormalizer,
            hasLineConsequence(packet),
            hasEvidenceLayer(packet, EvidenceLayer.Line)
          ),
          globalEvidenceLayerSlot(packet, JudgmentGraphSlot.EvalFact, JudgmentGraphOwner.EvalFactNormalizer, EvidenceLayer.Eval),
          slot(
            JudgmentGraphSlot.LegalReplayLineFact,
            JudgmentGraphOwner.LineFactNormalizer,
            packet.evidenceGraph.records.exists(record =>
              record.ref.layer == EvidenceLayer.Line &&
                record.ref.confidence == EvidenceConfidence.LegalReplayVerified
            )
          )
        ),
        layer(
          JudgmentGraphLayer.TransitionEvidence,
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.MoveMotifFact,
            JudgmentGraphOwner.MoveMotifNormalizer,
            EvidenceLayer.MoveMotif
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.TacticalMechanismFact,
            JudgmentGraphOwner.TacticalMechanismAssembler,
            EvidenceLayer.TacticalMechanism,
            tacticalMechanismApplicable(packet)
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.MoveTransitionFact,
            JudgmentGraphOwner.MoveTransitionNormalizer,
            EvidenceLayer.MoveTransition
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.RelationFact,
            JudgmentGraphOwner.RelationFactNormalizer,
            EvidenceLayer.Relation,
            relationEvidenceApplicable(packet)
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.StructuralDeltaFact,
            JudgmentGraphOwner.TransitionFactNormalizer,
            EvidenceLayer.StructuralDelta
          ),
          slot(
            JudgmentGraphSlot.StructuralSignalFact,
            JudgmentGraphOwner.TransitionFactNormalizer,
            hasStructuralSignals(packet),
            hasEvidenceLayer(packet, EvidenceLayer.StructuralDelta)
          ),
          slot(
            JudgmentGraphSlot.StructuralConsequenceFact,
            JudgmentGraphOwner.TransitionFactNormalizer,
            hasStructuralConsequences(packet),
            hasEvidenceLayer(packet, EvidenceLayer.StructuralDelta)
          ),
          slot(
            JudgmentGraphSlot.StructuralMeaningfulConsequenceFact,
            JudgmentGraphOwner.TransitionFactNormalizer,
            hasStructuralMeaningfulConsequences(packet),
            hasStructuralConsequences(packet)
          )
        ),
        layer(
          JudgmentGraphLayer.StrategicPlanOpeningEvidence,
          globalEvidenceLayerSlot(packet, JudgmentGraphSlot.StrategicFact, JudgmentGraphOwner.StrategicFactNormalizer, EvidenceLayer.Strategic),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.StrategicMechanismFact,
            JudgmentGraphOwner.StrategicMechanismAssembler,
            EvidenceLayer.StrategicMechanism
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.OpeningContextFact,
            JudgmentGraphOwner.OpeningContextFactNormalizer,
            EvidenceLayer.OpeningContext,
            applicable = false
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.FeatureAnchorFact,
            JudgmentGraphOwner.FeatureApplicabilityAssembler,
            EvidenceLayer.FeatureAnchor,
            openingIdeaApplicable(packet)
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.ApplicabilityAssessmentFact,
            JudgmentGraphOwner.FeatureApplicabilityAssembler,
            EvidenceLayer.ApplicabilityAssessment,
            openingIdeaApplicable(packet)
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.PlanPressureFact,
            JudgmentGraphOwner.StrategicFactNormalizer,
            EvidenceLayer.PlanPressure
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.PlanTransitionFact,
            JudgmentGraphOwner.TransitionFactNormalizer,
            EvidenceLayer.PlanTransition,
            applicable = hasEvidenceLayer(packet, EvidenceLayer.PlanTransition)
          )
        ),
        layer(
          JudgmentGraphLayer.RelativeChoiceAssessment,
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.CandidateComparisonFact,
            JudgmentGraphOwner.RelativeAssessmentAssembler,
            EvidenceLayer.CandidateComparison
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.RelativeAssessmentFact,
            JudgmentGraphOwner.RelativeAssessmentAssembler,
            EvidenceLayer.RelativeAssessment
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.CounterfactualFact,
            JudgmentGraphOwner.RelativeAssessmentAssembler,
            EvidenceLayer.Counterfactual
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.RelativeCauseFact,
            JudgmentGraphOwner.RelativeAssessmentAssembler,
            EvidenceLayer.RelativeCause,
            relativeCauseApplicable(packet)
          ),
          slot(
            JudgmentGraphSlot.RelativeCauseProof,
            JudgmentGraphOwner.RelativeAssessmentAssembler,
            hasRelativeCauseProof(packet),
            relativeCauseApplicable(packet)
          ),
          globalEvidenceLayerSlot(
            packet,
            JudgmentGraphSlot.MoveVerdictCertificationFact,
            JudgmentGraphOwner.RelativeAssessmentAssembler,
            EvidenceLayer.MoveVerdictCertification
          )
        ),
        layer(
          JudgmentGraphLayer.IdeaSynthesis,
          ideaSlot(packet, JudgmentGraphSlot.TacticalIdea, ChessIdeaFamily.Tactical, tacticalIdeaApplicable(packet)),
          ideaSlot(packet, JudgmentGraphSlot.StrategicIdea, ChessIdeaFamily.Strategic),
          ideaSlot(packet, JudgmentGraphSlot.PawnStructureIdea, ChessIdeaFamily.PawnStructure),
          ideaSlot(packet, JudgmentGraphSlot.OpeningIdea, ChessIdeaFamily.Opening, openingIdeaApplicable(packet)),
          ideaSlot(packet, JudgmentGraphSlot.DefensiveIdea, ChessIdeaFamily.Defensive),
          ideaSlot(packet, JudgmentGraphSlot.ConversionIdea, ChessIdeaFamily.Conversion, conversionIdeaApplicable(packet)),
          ideaSlot(packet, JudgmentGraphSlot.MaterialIdea, ChessIdeaFamily.Material, materialIdeaApplicable(packet)),
          ideaSlot(packet, JudgmentGraphSlot.EvaluationIdea, ChessIdeaFamily.Evaluation)
        ),
        layer(
          JudgmentGraphLayer.ClaimSynthesis,
          claimSlot(packet, JudgmentGraphSlot.TacticalClaim, ClaimFamily.Tactical, tacticalIdeaApplicable(packet)),
          claimSlot(packet, JudgmentGraphSlot.StrategicClaim, ClaimFamily.Strategic, strategicClaimApplicable(packet)),
          claimSlot(packet, JudgmentGraphSlot.PawnStructureClaim, ClaimFamily.PawnStructure),
          claimSlot(packet, JudgmentGraphSlot.OpeningClaim, ClaimFamily.Opening, openingIdeaApplicable(packet)),
          claimSlot(packet, JudgmentGraphSlot.PlanClaim, ClaimFamily.Plan, planClaimApplicable(packet)),
          claimSlot(packet, JudgmentGraphSlot.DefensiveClaim, ClaimFamily.Defensive),
          claimSlot(packet, JudgmentGraphSlot.ConversionClaim, ClaimFamily.Conversion, conversionIdeaApplicable(packet)),
          claimSlot(packet, JudgmentGraphSlot.MaterialClaim, ClaimFamily.Material, materialIdeaApplicable(packet)),
          claimSlot(packet, JudgmentGraphSlot.EvaluationClaim, ClaimFamily.Evaluation)
        ),
        layer(
          JudgmentGraphLayer.PacketBoundary,
          slot(
            JudgmentGraphSlot.IdeaVerdictSplit,
            JudgmentGraphOwner.JudgmentPacketBuilder,
            packet.ideaVerdict.nonEmpty
          ),
          slot(
            JudgmentGraphSlot.ClaimSupportCluster,
            JudgmentGraphOwner.JudgmentPacketBuilder,
            packet.claimSupportClusters.nonEmpty,
            longTermSupportClusterApplicable(packet)
          ),
          slot(
            JudgmentGraphSlot.ClaimEventCluster,
            JudgmentGraphOwner.JudgmentPacketBuilder,
            packet.claimEventClusters.nonEmpty,
            claimEventClusterApplicable(packet)
          ),
          slot(
            JudgmentGraphSlot.EvidenceLossDiagnostics,
            JudgmentGraphOwner.EvidenceLossDiagnostics,
            packet.diagnostics.coverage.nonEmpty || packet.evidenceGraph.records.nonEmpty
          )
        )
      )
    )

  private def layer(
      graphLayer: JudgmentGraphLayer,
      slots: JudgmentGraphSlotStatus*
  ): JudgmentLayerGapMetric =
    JudgmentLayerGapMetric(graphLayer, slots.toList)

  private def slot(
      graphSlot: JudgmentGraphSlot,
      owner: JudgmentGraphOwner,
      present: Boolean,
      applicable: Boolean = true
  ): JudgmentGraphSlotStatus =
    JudgmentGraphSlotStatus(graphSlot, owner, present, applicable)

  private def globalEvidenceLayerSlot(
      packet: EvidenceBackedJudgmentPacket,
      graphSlot: JudgmentGraphSlot,
      owner: JudgmentGraphOwner,
      evidenceLayer: EvidenceLayer,
      applicable: Boolean = true
  ): JudgmentGraphSlotStatus =
    slot(graphSlot, owner, packet.evidenceGraph.records.exists(_.ref.layer == evidenceLayer), applicable)

  private def hasEvidenceLayer(packet: EvidenceBackedJudgmentPacket, evidenceLayer: EvidenceLayer): Boolean =
    packet.evidenceGraph.records.exists(_.ref.layer == evidenceLayer)

  private def hasBoardAnchor(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: BoardFactEvidence, _) => payload.hasBoardAnchors
      case _                                                => false
    }

  private def hasThreatEpisode(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, _: ThreatEpisodeEvidence, _) => true
      case _                                             => false
    }

  private def hasThreatPressureEpisodeSource(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, ThreatPressureEvidence(_, threats), _) => threats.threats.nonEmpty
      case _                                                       => false
    }

  private def hasLineReplay(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.hasLineReplay
      case _                                               => false
    }

  private def hasLineEvent(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.hasLineEvents
      case _                                               => false
    }

  private def hasLineConsequence(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.hasLineConsequences
      case _                                               => false
    }

  private def hasStructuralSignals(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) => payload.hasSignals
      case _                                                      => false
    }

  private def hasStructuralConsequences(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) => payload.hasConsequences
      case _                                                      => false
    }

  private def hasStructuralMeaningfulConsequences(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) => payload.hasMeaningfulConsequences
      case _                                                      => false
    }

  private def hasRelativeCauseProof(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause.hasOwnedTypedDepth
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(_.hasOwnedTypedDepth)
      case _                                                      => false
    }

  private def globalPositionEvidenceSlot(
      packet: EvidenceBackedJudgmentPacket,
      graphSlot: JudgmentGraphSlot,
      owner: JudgmentGraphOwner,
      evidenceLayer: EvidenceLayer,
      positionRole: PositionNodeRole
  ): JudgmentGraphSlotStatus =
    val positionRefs = packet.positions.collect {
      case position if position.role == positionRole => position.ref
    }.toSet
    slot(
      graphSlot,
      owner,
      packet.evidenceGraph.records.exists(record =>
        record.ref.layer == evidenceLayer && positionRefs.contains(record.ref.position)
      )
    )

  private def ideaSlot(
      packet: EvidenceBackedJudgmentPacket,
      graphSlot: JudgmentGraphSlot,
      family: ChessIdeaFamily,
      applicable: Boolean = true
  ): JudgmentGraphSlotStatus =
    slot(
      graphSlot,
      JudgmentGraphOwner.ChessIdeaAssembler,
      packet.ideas.exists(_.ref.family == family),
      applicable
    )

  private def claimSlot(
      packet: EvidenceBackedJudgmentPacket,
      graphSlot: JudgmentGraphSlot,
      family: ClaimFamily,
      applicable: Boolean = true
  ): JudgmentGraphSlotStatus =
    slot(
      graphSlot,
      JudgmentGraphOwner.ClaimSeedAssembler,
      packet.claims.exists(_.family == family),
      applicable
    )

  private def relationEvidenceApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists(_.ref.layer == EvidenceLayer.Relation)

  private def tacticalMechanismApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.hasConcreteProof
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.hasConcreteRelationProof
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasTacticalLineConsequence
      case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
        mate.nonEmpty
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.isProofSignalDefensivePressure
      case _ =>
        false
    }

  private def relativeCauseApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.evidenceGraph.records.exists(_.ref.layer == EvidenceLayer.RelativeCause) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
            fact.comparison.candidateSet.exists(_.onlyMove)
        case _ =>
          false
      }

  private def tacticalIdeaApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.ideas.exists(_.ref.family == ChessIdeaFamily.Tactical) ||
      packet.claims.exists(_.family == ClaimFamily.Tactical) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.canAnchorTacticalIdea
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          tacticalCauseApplicable(cause)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(tacticalCauseApplicable)
        case _ =>
          false
      }

  private def strategicClaimApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.claims.exists(_.family == ClaimFamily.Strategic) ||
      packet.ideas.exists(idea => idea.ref.family == ChessIdeaFamily.Strategic && idea.subject != IdeaSubject.Plan)

  private def conversionIdeaApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.ideas.exists(_.ref.family == ChessIdeaFamily.Conversion) ||
      packet.claims.exists(_.family == ClaimFamily.Conversion) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          conversionCauseApplicable(cause)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(conversionCauseApplicable)
        case EvidenceRecord(_, payload: RelationFactEvidence, _) if payload.kind == RelationFactKind.BadPieceLiquidation =>
          true
        case _ =>
          false
      }

  private def materialIdeaApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.ideas.exists(_.ref.family == ChessIdeaFamily.Material) ||
      packet.claims.exists(_.family == ClaimFamily.Material) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          materialResultCause(cause)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(materialResultCause)
        case _ =>
          false
      }

  private def materialResultCause(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).contains(ClaimEventClusterKind.MaterialEvent) &&
      cause.hasOwnedTypedDepth

  private def longTermSupportClusterApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.claims.exists(_.family.isLongTerm)

  private def planClaimApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.ideas.exists(_.subject == IdeaSubject.Plan) ||
      ClaimTruthPolicy.planClaimApplicable(packet)

  private def claimEventClusterApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.claims.exists(_.family.isEvent) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          ClaimEventCluster.kindForCause(cause.kind).nonEmpty
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.causes.exists(cause => ClaimEventCluster.kindForCause(cause.kind).nonEmpty)
        case _ =>
          false
      }

  private def openingIdeaApplicable(packet: EvidenceBackedJudgmentPacket): Boolean =
    packet.ideas.exists(_.ref.family == ChessIdeaFamily.Opening) ||
      packet.claims.exists(_.family == ClaimFamily.Opening) ||
      packet.evidenceGraph.records.exists {
        case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), parents) =>
          assessment.canCertifyOpeningClaim &&
            parents.exists(parent =>
              packet.evidenceGraph.byId.get(parent.id).exists {
                case EvidenceRecord(_, OpeningContextEvidence(_, _, _, Some(selection)), _) =>
                  selection.canCertifyOpeningClaim
                case _ =>
                  false
              }
            )
        case _ =>
          false
      }

  private def tacticalCauseApplicable(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).contains(ClaimEventClusterKind.TacticalEvent) &&
      cause.hasOwnedTacticalProof

  private def conversionCauseApplicable(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).contains(ClaimEventClusterKind.ConversionEvent) &&
      cause.hasOwnedTypedDepth

enum ChessQualityIssueKind:
  case PacketValidationFailed
  case MissingRelativeAssessment
  case MissingReferenceLine
  case MissingPlayedLine
  case MissingLegalReplayLine
  case MissingClaim
  case MissingTacticalOrStrategicCoverage
  case MissingCandidateComparison
  case UnexpectedEvidenceLoss

final case class ChessQualityIssue(
    kind: ChessQualityIssueKind,
    subjectId: String,
    validationKind: Option[JudgmentPacketValidationIssueKind] = None,
    evidence: Option[EvidenceRef] = None
)

final case class ChessQualityAudit(
    issues: List[ChessQualityIssue]
):
  def isClean: Boolean = issues.isEmpty

object ChessQualityAudit:
  def from(
      packet: EvidenceBackedJudgmentPacket,
      validation: JudgmentPacketValidationResult,
      graphLoss: GraphLossMetrics,
      semantic: SemanticCoverageMetrics
  ): ChessQualityAudit =
    val issues =
      List.concat(
        validationIssues(validation),
        requiredRelativeAssessment(packet),
        requiredLines(packet),
        legalReplayCoverage(packet),
        relativeBreadthCoverage(packet, semantic),
        claimCoverage(packet),
        ideaCoverage(semantic),
        unexpectedEvidenceLoss(graphLoss)
      )
    ChessQualityAudit(issues.distinct)

  private def validationIssues(validation: JudgmentPacketValidationResult): List[ChessQualityIssue] =
    validation.issues.map(issue =>
      ChessQualityIssue(
        kind = ChessQualityIssueKind.PacketValidationFailed,
        subjectId = issue.subjectId,
        validationKind = Some(issue.kind),
        evidence = issue.evidence
      )
    )

  private def requiredRelativeAssessment(packet: EvidenceBackedJudgmentPacket): List[ChessQualityIssue] =
    Option
      .when(packet.relativeAssessments.isEmpty)(
        ChessQualityIssue(ChessQualityIssueKind.MissingRelativeAssessment, "relative-assessment")
      )
      .toList

  private def requiredLines(packet: EvidenceBackedJudgmentPacket): List[ChessQualityIssue] =
    List(
      Option.when(!packet.candidateLines.exists(_.role == LineNodeRole.BestReference))(
        ChessQualityIssue(ChessQualityIssueKind.MissingReferenceLine, "best-reference-line")
      ),
      Option.when(!packet.candidateLines.exists(_.role == LineNodeRole.Played))(
        ChessQualityIssue(ChessQualityIssueKind.MissingPlayedLine, "played-line")
      )
    ).flatten

  private def legalReplayCoverage(packet: EvidenceBackedJudgmentPacket): List[ChessQualityIssue] =
    val hasLegalReplayLine =
      packet.evidenceGraph.records.exists(record =>
        record.ref.layer == EvidenceLayer.Line &&
          record.ref.confidence == EvidenceConfidence.LegalReplayVerified
      )
    Option
      .when(packet.candidateLines.nonEmpty && !hasLegalReplayLine)(
        ChessQualityIssue(ChessQualityIssueKind.MissingLegalReplayLine, "legal-replay-line")
      )
      .toList

  private def relativeBreadthCoverage(
      packet: EvidenceBackedJudgmentPacket,
      semantic: SemanticCoverageMetrics
  ): List[ChessQualityIssue] =
    val significantRelative =
      packet.relativeAssessments.exists(assessment =>
        assessment.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
          assessment.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
          assessment.comparison.candidateSet.exists(_.onlyMove)
      )
    List(
      Option.when(significantRelative && semantic.primaryPlayedComparisonFacts == 0)(
        ChessQualityIssue(ChessQualityIssueKind.MissingCandidateComparison, "candidate-comparison")
      )
    ).flatten

  private def claimCoverage(packet: EvidenceBackedJudgmentPacket): List[ChessQualityIssue] =
    Option
      .when(packet.ideas.nonEmpty && packet.claims.isEmpty)(
        ChessQualityIssue(ChessQualityIssueKind.MissingClaim, "claim")
      )
      .toList

  private def ideaCoverage(semantic: SemanticCoverageMetrics): List[ChessQualityIssue] =
    val hasChessIdeaCoverage =
      semantic.tacticalIdeas > 0 ||
        semantic.strategicIdeas > 0 ||
        semantic.pawnStructureIdeas > 0 ||
        semantic.defensiveIdeas > 0 ||
        semantic.conversionIdeas > 0 ||
        semantic.materialIdeas > 0
    Option
      .when(!hasChessIdeaCoverage)(
        ChessQualityIssue(ChessQualityIssueKind.MissingTacticalOrStrategicCoverage, "idea-coverage")
      )
      .toList

  private def unexpectedEvidenceLoss(graphLoss: GraphLossMetrics): List[ChessQualityIssue] =
    Option
      .when(graphLoss.unexpectedLoss > 0)(
        ChessQualityIssue(ChessQualityIssueKind.UnexpectedEvidenceLoss, "evidence-loss")
      )
      .toList

final case class JudgmentQualityReport(
    validation: JudgmentPacketValidationResult,
    graphLoss: GraphLossMetrics,
    claimPromotion: ClaimPromotionMetrics,
    semanticCoverage: SemanticCoverageMetrics,
    layerGaps: JudgmentLayerGapProfile,
    audit: ChessQualityAudit,
    evidenceLoss: List[EvidenceLossClassification]
)

object JudgmentQualityReport:
  def fromPacket(packet: EvidenceBackedJudgmentPacket): JudgmentQualityReport =
    fromPacket(packet, JudgmentPacketValidator.validate(packet))

  def fromPacket(
      packet: EvidenceBackedJudgmentPacket,
      validation: JudgmentPacketValidationResult
  ): JudgmentQualityReport =
    val evidenceLoss = ExpectedEvidenceLossPolicy.classify(packet.diagnostics, packet)
    val graphLoss = GraphLossMetrics.from(packet.diagnostics, evidenceLoss)
    val claimPromotion = ClaimPromotionMetrics.from(packet)
    val semanticCoverage = SemanticCoverageMetrics.from(packet)
    val layerGaps = JudgmentLayerGapProfile.fromPacket(packet)
    val audit =
      ChessQualityAudit.from(
        packet = packet,
        validation = validation,
        graphLoss = graphLoss,
        semantic = semanticCoverage
      )
    JudgmentQualityReport(
      validation = validation,
      graphLoss = graphLoss,
      claimPromotion = claimPromotion,
      semanticCoverage = semanticCoverage,
      layerGaps = layerGaps,
      audit = audit,
      evidenceLoss = evidenceLoss
    )
