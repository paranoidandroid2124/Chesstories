package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import chess.{ Pawn, Square }
import chess.format.Fen
import lila.chessjudgment.model.{ ProbeAdmissionDiagnostic, ProbeRequest }
import play.api.libs.json.*

enum ClaimFamily:
  case Tactical
  case Strategic
  case PawnStructure
  case Opening
  case Plan
  case Defensive
  case Conversion
  case Material
  case Evaluation

  private[chessjudgment] def isLongTerm: Boolean =
    this match
      case Strategic | PawnStructure | Opening | Plan => true
      case Tactical | Defensive | Conversion | Material | Evaluation => false

  private[chessjudgment] def isEvent: Boolean =
    !isLongTerm

enum ClaimSalienceDriver:
  case TacticalRelation
  case ForcingLine
  case DefensiveUrgency
  case PlanPressure
  case PawnStructureAlignment
  case StructuralChange
  case StrategicFeature
  case OpeningContext
  case EndgamePattern
  case EngineSwing
  case CandidateConstraint
  case BoardAnchor

enum ClaimInteractionKind:
  case TacticalConstrainsLongTerm
  case LongTermConstrainedByTactic
  case TacticalRefutesStrategicPlan
  case TacticalSupportsStrategicPlan
  case DefensiveNecessityOverridesPlan
  case StrategicCompensationSupportsSacrifice
  case ConversionSecuresAdvantage
  case BadVerdictPreservesLocalIdea

enum SubjectBindingClass:
  case DirectPlayed
  case PrimaryPlayedCause
  case ContextPlayed
  case Other

enum PlayedSubjectBinding:
  case SubjectMove
  case PlayedLine
  case SubjectMoveAndPlayedLine
  case Unbound

enum ClaimLifecycleStage:
  case CandidateCreated
  case TruthCertified
  case TruthRejected
  case TruthDeferred
  case DedupeDropped
  case ArbitrationSuppressed
  case FinalPacketIncluded

enum ClaimLifecycleTruthStatus:
  case Certified
  case Deferred
  case Rejected

final case class ClaimLifecycleRelativeCause(
    id: String,
    kind: RelativeCauseKind,
    role: RelativeCauseRole,
    comparisonKind: CandidateComparisonKind,
    sourceSide: RelativeCauseSourceSide,
    importance: RelativeCauseImportance,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    supportEvidenceSourceIds: List[String],
    proofDirectKinds: List[String],
    proofContrastKinds: List[String],
    proofContextSupportKinds: List[String],
    attributionKind: CauseAttributionKind,
    attributionOwnedEvidenceIds: List[String],
    attributionContrastEvidenceIds: List[String],
    attributionContextEvidenceIds: List[String],
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean,
    attributionReason: Option[String],
    objectBindingSignatures: List[String] = Nil
)

final case class ClaimStrategicAxisLineage(
    axisKey: String,
    axisKind: StrategicAxisKind,
    axisPolarity: StrategicAxisPolarity,
    axisLabel: String,
    mechanismEvidenceId: String,
    signalSourceEvidenceId: String,
    signalSourceLayer: EvidenceLayer,
    relativeCauseIds: List[String]
)

object ClaimStrategicAxisLineage:
  def fromClaim(claim: ClaimSeed, graph: TypedEvidenceGraph): List[ClaimStrategicAxisLineage] =
    fromRecords(claim.evidence.flatMap(ref => graph.byId.get(ref.id)))

  def fromRecords(records: List[EvidenceRecord]): List[ClaimStrategicAxisLineage] =
    val grouped =
      records
        .flatMap(axisEntries)
        .groupBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))
    grouped.values.toList
      .map { entries =>
        val first = entries.head
        first.copy(relativeCauseIds = entries.flatMap(_.relativeCauseIds).distinct.sorted)
      }
      .sortBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))

  private def axisEntries(record: EvidenceRecord): List[ClaimStrategicAxisLineage] =
    record.payload match
      case payload: StrategicMechanismContrastEvidence =>
        contrastEntries(record.ref.id, payload, None)
      case RelativeCauseFactEvidence(cause) =>
        causeEntries(record.ref.id, cause)
      case _ =>
        Nil

  private def causeEntries(causeId: String, cause: RelativeCauseFact): List[ClaimStrategicAxisLineage] =
    cause.proof.toList.flatMap(proof =>
      proof.strategicMechanisms.flatMap(mechanism => mechanismEntries(mechanism.source.id, mechanism, Some(causeId))) ++
        proof.strategicMechanismContrasts.flatMap(contrast => contrastEntries(contrast.source.id, contrast, Some(causeId)))
    )

  private def mechanismEntries(
      mechanismEvidenceId: String,
      proof: StrategicMechanismProof,
      causeId: Option[String]
  ): List[ClaimStrategicAxisLineage] =
    proof.signals.flatMap(signal => signal.axis.map(axis => lineage(axis, mechanismEvidenceId, signal, causeId)))

  private def contrastEntries(
      mechanismEvidenceId: String,
      payload: StrategicMechanismContrastEvidence,
      causeId: Option[String]
  ): List[ClaimStrategicAxisLineage] =
    payload.axisComparisons.flatMap(axisComparison =>
      axisComparison.sources.map(source => lineage(axisComparison.axis, mechanismEvidenceId, source, causeId))
    )

  private def contrastEntries(
      mechanismEvidenceId: String,
      proof: StrategicMechanismContrastProof,
      causeId: Option[String]
  ): List[ClaimStrategicAxisLineage] =
    proof.axisComparisons.flatMap(axisComparison =>
      axisComparison.sources.map(source => lineage(axisComparison.axis, mechanismEvidenceId, source, causeId))
    )

  private def lineage(
      axis: StrategicAxisDetail,
      mechanismEvidenceId: String,
      signal: StrategicMechanismSignal,
      causeId: Option[String]
  ): ClaimStrategicAxisLineage =
    ClaimStrategicAxisLineage(
      axisKey = axis.stableKey,
      axisKind = axis.kind,
      axisPolarity = axis.polarity,
      axisLabel = axis.label,
      mechanismEvidenceId = mechanismEvidenceId,
      signalSourceEvidenceId = signal.source.id,
      signalSourceLayer = signal.source.layer,
      relativeCauseIds = causeId.toList
    )

  private def lineage(
      axis: StrategicAxisDetail,
      mechanismEvidenceId: String,
      source: EvidenceRef,
      causeId: Option[String]
  ): ClaimStrategicAxisLineage =
    ClaimStrategicAxisLineage(
      axisKey = axis.stableKey,
      axisKind = axis.kind,
      axisPolarity = axis.polarity,
      axisLabel = axis.label,
      mechanismEvidenceId = mechanismEvidenceId,
      signalSourceEvidenceId = source.id,
      signalSourceLayer = source.layer,
      relativeCauseIds = causeId.toList
    )

final case class ClaimLifecycleDiagnostic(
    candidateId: String,
    claimId: String,
    finalClaimId: Option[String],
    sourceCandidateIds: List[String],
    family: ClaimFamily,
    subject: IdeaSubject,
    subjectBinding: SubjectBindingClass,
    primaryLine: Option[LineNodeRef],
    subjectMove: Option[String],
    ideaIds: List[String],
    finalIdeaIds: List[String],
    evidenceIds: List[String],
    finalEvidenceIds: List[String],
    relativeCauses: List[ClaimLifecycleRelativeCause],
    strategicAxisLineage: List[ClaimStrategicAxisLineage],
    truthStatus: Option[ClaimLifecycleTruthStatus],
    presentLayers: Set[EvidenceLayer],
    missingLayerGroups: List[Set[EvidenceLayer]],
    missingEvidenceIds: List[String],
    stages: List[ClaimLifecycleStage],
    dedupeWinnerId: Option[String],
    arbitrationRank: Option[Int],
    finalPacketIncluded: Boolean
)

object JudgmentSubjectBinding:

  def claimBinding(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): SubjectBindingClass =
    claimBinding(claim, packet.evidenceGraph, packetPlayedMoves(packet))

  def claimBinding(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    if playedMoves.isEmpty then SubjectBindingClass.Other
    else
      val evidenceBinding =
        strongest(
          claim.evidence
            .flatMap(ref => graph.byId.get(ref.id))
            .map(recordBinding(_, playedMoves))
        )
      evidenceBinding match
        case SubjectBindingClass.PrimaryPlayedCause | SubjectBindingClass.ContextPlayed =>
          evidenceBinding
        case SubjectBindingClass.DirectPlayed =>
          SubjectBindingClass.DirectPlayed
        case SubjectBindingClass.Other =>
          if directPlayedClaim(claim, playedMoves) && hasDirectPlayedEvidence(claim, graph, playedMoves) then
            SubjectBindingClass.DirectPlayed
          else SubjectBindingClass.Other

  def recordBinding(record: EvidenceRecord, playedMoves: Set[String]): SubjectBindingClass =
    record.payload match
      case RelativeAssessmentEvidence(assessment) if playedMoves.contains(normalizeMove(assessment.played.moveUci)) =>
        if badVerdict(assessment.comparison.verdict) then SubjectBindingClass.PrimaryPlayedCause
        else SubjectBindingClass.ContextPlayed
      case CandidateComparisonEvidence(fact) =>
        comparisonBinding(fact, playedMoves)
      case RelativeCauseFactEvidence(cause) =>
        relativeCauseBinding(cause, playedMoves)
      case MoveVerdictCertificationEvidence(certification) =>
        strongest(certification.causes.map(relativeCauseBinding(_, playedMoves)))
      case _ =>
        SubjectBindingClass.Other

  def comparisonBinding(
      fact: CandidateComparisonFact,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    val candidateIsPlayed = playedMoves.contains(normalizeMove(fact.candidateLine.rootMove))
    val referenceIsPlayed = playedMoves.contains(normalizeMove(fact.referenceLine.rootMove))
    fact.kind match
      case CandidateComparisonKind.PlayedVsBest if candidateIsPlayed && badVerdict(fact.comparison.verdict) =>
        SubjectBindingClass.PrimaryPlayedCause
      case CandidateComparisonKind.PlayedVsBest if candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case CandidateComparisonKind.PlayedVsAlternative if candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case CandidateComparisonKind.BestVsSecond if referenceIsPlayed || candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case CandidateComparisonKind.ReferenceVsAlternative if referenceIsPlayed || candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case _ =>
        SubjectBindingClass.Other

  def relativeCauseBinding(
      cause: RelativeCauseFact,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    val candidateIsPlayed = playedMoves.contains(normalizeMove(cause.candidateLine.rootMove))
    val referenceIsPlayed = playedMoves.contains(normalizeMove(cause.referenceLine.rootMove))
    cause.role match
      case RelativeCauseRole.PrimaryPlayedCause
          if candidateIsPlayed && cause.importance == RelativeCauseImportance.Primary && badVerdict(cause.verdict) =>
        SubjectBindingClass.PrimaryPlayedCause
      case RelativeCauseRole.PrimaryPlayedCause if candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case RelativeCauseRole.PlayedAlternativeContext if candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case RelativeCauseRole.CandidateSetConstraint | RelativeCauseRole.AlternativeDiagnostic
          if referenceIsPlayed || candidateIsPlayed =>
        SubjectBindingClass.ContextPlayed
      case _ =>
        SubjectBindingClass.Other

  def packetPlayedMoves(packet: EvidenceBackedJudgmentPacket): Set[String] =
    (
      packet.playedTransition.map(_.moveUci).toList ++
        packet.relativeAssessments.map(_.played.moveUci) ++
        packet.candidateLines.filter(_.role == LineNodeRole.Played).map(_.ref.rootMove)
    ).map(normalizeMove).filter(_.nonEmpty).toSet

  def directPlayedClaim(claim: ClaimSeed, playedMoves: Set[String]): Boolean =
    directPlayedSubject(claim.subjectMove, claim.primaryLine, playedMoves)

  def directPlayedSubject(
      subjectMove: Option[String],
      primaryLine: Option[LineNodeRef],
      playedMoves: Set[String]
  ): Boolean =
    playedSubjectBinding(subjectMove, primaryLine, playedMoves) != PlayedSubjectBinding.Unbound

  def playedSubjectBinding(
      subjectMove: Option[String],
      primaryLine: Option[LineNodeRef],
      playedMoves: Set[String]
  ): PlayedSubjectBinding =
    if playedMoves.isEmpty then PlayedSubjectBinding.Unbound
    else
      val subjectMoveBound = subjectMove.exists(move => playedMoves.contains(normalizeMove(move)))
      val playedLineBound =
        primaryLine.exists(line =>
          line.role == LineNodeRole.Played && playedMoves.contains(normalizeMove(line.rootMove))
        )
      (subjectMoveBound, playedLineBound) match
        case (true, true)   => PlayedSubjectBinding.SubjectMoveAndPlayedLine
        case (true, false)  => PlayedSubjectBinding.SubjectMove
        case (false, true)  => PlayedSubjectBinding.PlayedLine
        case (false, false) => PlayedSubjectBinding.Unbound

  def hasDirectPlayedEvidence(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id)).exists { record =>
      val localLineBinding =
        record.ref.line.exists(line =>
          line.role == LineNodeRole.Played && playedMoves.contains(normalizeMove(line.rootMove))
        )
      val playedTransitionBinding =
        record.ref.scope == EvidenceScope.PlayedTransition &&
          claim.subjectMove.exists(move => playedMoves.contains(normalizeMove(move)) && recordMentionsMove(record, move))
      (localLineBinding || playedTransitionBinding) && directEvidencePayload(record.payload)
    }

  def recordMentionsMove(record: EvidenceRecord, move: String): Boolean =
    record.payload match
      case payload: MoveMotifEvidence =>
        sameMove(payload.moveUci, move)
      case MoveTransitionEvidence(moveUci, _, _) =>
        sameMove(moveUci, move)
      case payload: StructuralDeltaEvidence =>
        sameMove(payload.moveUci, move)
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(sameMove(_, move)) ||
          payload.line.exists(line => sameMove(line.rootMove, move)) ||
          record.ref.scope == EvidenceScope.PlayedTransition
      case payload: RelationFactEvidence =>
        payload.mentionsLineMove(move) || record.ref.scope == EvidenceScope.PlayedTransition
      case _ =>
        record.ref.line.exists(line => sameMove(line.rootMove, move))

  private def sameMove(left: String, right: String): Boolean =
    normalizeMove(left) == normalizeMove(right)

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict match
      case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder => true
      case _                                                                                   => false

  private def directEvidencePayload(payload: EvidencePayload): Boolean =
    payload match
      case CandidateComparisonEvidence(_) | CounterfactualFactEvidence(_, _, _) | RelativeAssessmentEvidence(_) |
          RelativeCauseFactEvidence(_) | MoveVerdictCertificationEvidence(_) =>
        false
      case _ =>
        true

  def strongest(bindings: Iterable[SubjectBindingClass]): SubjectBindingClass =
    bindings.foldLeft(SubjectBindingClass.Other) { (best, next) =>
      if bindingScore(next) > bindingScore(best) then next else best
    }

  def bindingScore(binding: SubjectBindingClass): Int =
    binding match
      case SubjectBindingClass.DirectPlayed       => 3
      case SubjectBindingClass.PrimaryPlayedCause => 2
      case SubjectBindingClass.ContextPlayed      => 1
      case SubjectBindingClass.Other              => 0

  def primaryPlayed(binding: SubjectBindingClass): Boolean =
    binding == SubjectBindingClass.DirectPlayed || binding == SubjectBindingClass.PrimaryPlayedCause

  def playedRelated(binding: SubjectBindingClass): Boolean =
    binding != SubjectBindingClass.Other

  def sourceRecordOwnsCurrentPlayedMove(record: EvidenceRecord, move: String): Boolean =
    recordMentionsMove(record, move) &&
      (
        record.ref.scope == EvidenceScope.PlayedTransition ||
          record.ref.line.exists(line => line.role == LineNodeRole.Played && sameMove(line.rootMove, move))
      )

  def sourceRecordOwnsPawnBreakMove(record: EvidenceRecord, move: String): Boolean =
    recordMentionsMove(record, move) &&
      (
        record.ref.scope == EvidenceScope.PlayedTransition ||
          record.ref.scope == EvidenceScope.ReferenceTransition ||
          record.ref.line.exists(line =>
            (line.role == LineNodeRole.Played || line.role == LineNodeRole.BestReference) &&
              sameMove(line.rootMove, move)
          )
      )

  def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

enum PlayerFacingClaimTier:
  case Primary
  case Secondary
  case Context
  case Diagnostic

object PlayerFacingClaimPolicy:

  def tier(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): PlayerFacingClaimTier =
    tier(claim, packet.evidenceGraph, JudgmentSubjectBinding.packetPlayedMoves(packet))

  def tier(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingClaimTier =
    if playedMoves.isEmpty then PlayerFacingClaimTier.Diagnostic
    else
      val evidenceRecords =
        claim.evidence
          .flatMap(ref => graph.byId.get(ref.id))
      val baseTier =
        if claim.family == ClaimFamily.Evaluation &&
          evidenceRecords.exists(record => evaluationVerdictCarrierRecord(record, playedMoves))
        then
          PlayerFacingClaimTier.Secondary
        else if evidenceRecords.exists(record => primaryEvidenceRecord(record, playedMoves)) then
          PlayerFacingClaimTier.Primary
        else if evidenceRecords.exists(record => contextEvidenceRecord(record, playedMoves)) then
          PlayerFacingClaimTier.Context
        else if JudgmentSubjectBinding.hasDirectPlayedEvidence(claim, graph, playedMoves) then
          PlayerFacingClaimTier.Secondary
        else
          PlayerFacingClaimTier.Diagnostic
      if promotedTier(baseTier) && requiresConcreteObject(claim.family) &&
        !EvidenceObjectBinding.playerFacingReady(EvidenceObjectBinding.fromClaim(claim, graph))
      then PlayerFacingClaimTier.Diagnostic
      else baseTier

  def requiresConcreteObject(family: ClaimFamily): Boolean =
    family != ClaimFamily.Evaluation

  private def promotedTier(tier: PlayerFacingClaimTier): Boolean =
    tier == PlayerFacingClaimTier.Primary || tier == PlayerFacingClaimTier.Secondary

  private def primaryEvidenceRecord(
      record: EvidenceRecord,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        JudgmentSubjectBinding.comparisonBinding(fact, playedMoves) == SubjectBindingClass.PrimaryPlayedCause
      case RelativeCauseFactEvidence(cause) =>
        JudgmentSubjectBinding.relativeCauseBinding(cause, playedMoves) == SubjectBindingClass.PrimaryPlayedCause
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.exists(cause =>
          JudgmentSubjectBinding.relativeCauseBinding(cause, playedMoves) == SubjectBindingClass.PrimaryPlayedCause
        )
      case _ =>
        false

  private def evaluationVerdictCarrierRecord(
      record: EvidenceRecord,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case RelativeAssessmentEvidence(assessment) =>
        playedMoves.contains(JudgmentSubjectBinding.normalizeMove(assessment.played.moveUci))
      case MoveVerdictCertificationEvidence(certification) =>
        playedMoves.contains(JudgmentSubjectBinding.normalizeMove(certification.playedMove))
      case _ =>
        false

  private def contextEvidenceRecord(
      record: EvidenceRecord,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        JudgmentSubjectBinding.comparisonBinding(fact, playedMoves) == SubjectBindingClass.ContextPlayed
      case RelativeCauseFactEvidence(cause) =>
        JudgmentSubjectBinding.relativeCauseBinding(cause, playedMoves) == SubjectBindingClass.ContextPlayed
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.exists(cause =>
          JudgmentSubjectBinding.relativeCauseBinding(cause, playedMoves) == SubjectBindingClass.ContextPlayed
        )
      case _ =>
        false

  def rankPriority(tier: PlayerFacingClaimTier): Int =
    tier match
      case PlayerFacingClaimTier.Primary    => 3
      case PlayerFacingClaimTier.Secondary  => 2
      case PlayerFacingClaimTier.Context    => 1
      case PlayerFacingClaimTier.Diagnostic => 0

case class ClaimInteraction(
    kind: ClaimInteractionKind,
    relatedClaimId: String,
    strength: Int,
    interactionEvidence: List[EvidenceRef],
    basis: List[ClaimInteractionBasis] = Nil
)

case class ClaimInteractionBasis(
    causeKind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    causeRole: RelativeCauseRole,
    causeSourceSide: RelativeCauseSourceSide,
    causeImportance: RelativeCauseImportance,
    attributionKind: CauseAttributionKind,
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    supportEvidenceSourceIds: List[String]
)

object ClaimInteractionBasis:
  def stableKey(basis: ClaimInteractionBasis): String =
    List(
      basis.causeKind.toString,
      basis.comparisonKind.toString,
      basis.causeRole.toString,
      basis.causeSourceSide.toString,
      basis.causeImportance.toString,
      basis.attributionKind.toString,
      basis.attributionRootMoveMatched.toString,
      basis.attributionDirectProofEligible.toString,
      basis.referenceLine.id,
      basis.candidateLine.id,
      basis.eventLine.id,
      basis.proofDirectSourceIds.mkString(","),
      basis.proofContrastSourceIds.mkString(","),
      basis.proofContextSupportSourceIds.mkString(","),
      basis.supportEvidenceSourceIds.mkString(",")
    ).mkString("|")

case class ClaimSalience(
    score: Int,
    drivers: List[ClaimSalienceDriver],
    interactions: List[ClaimInteraction] = Nil
)

enum ClaimSupportStatus:
  case Certified
  case Deferred

case class ClaimSupportCheck(
    status: ClaimSupportStatus,
    presentLayers: Set[EvidenceLayer],
    missingLayerGroups: List[Set[EvidenceLayer]],
    missingEvidence: List[EvidenceRef]
)

enum ClaimSupportClusterKind:
  case LongTermSupport

case class ClaimSupportClusterInteraction(
    kind: ClaimInteractionKind,
    sourceClaimId: String,
    targetClaimId: String,
    strength: Int,
    evidence: List[EvidenceRef],
    basis: List[ClaimInteractionBasis]
)

case class ClaimSupportCluster(
    id: String,
    kind: ClaimSupportClusterKind,
    families: List[ClaimFamily],
    subject: IdeaSubject,
    primaryPosition: PositionNodeRef,
    primaryLine: Option[LineNodeRef],
    subjectMove: Option[String],
    scope: Option[EvidenceScope],
    anchorClaimIds: List[String],
    supportingClaimIds: List[String],
    constrainingClaimIds: List[String],
    ideas: List[ChessIdeaRef],
    evidence: List[EvidenceRef],
    presentLayers: Set[EvidenceLayer],
    confidence: EvidenceConfidence,
    salienceDrivers: List[ClaimSalienceDriver],
    interactions: List[ClaimSupportClusterInteraction]
)

object ClaimSupportCluster:

  def fromClaims(claims: List[ClaimSeed]): List[ClaimSupportCluster] =
    fromClaims(claims, TypedEvidenceGraph(Nil))

  def fromClaims(claims: List[ClaimSeed], graph: TypedEvidenceGraph): List[ClaimSupportCluster] =
    val anchors =
      claims
      .filter(claim => longTermSupportEligible(claim, graph))
      .groupBy(claim => clusterKey(claim, graph))
      .toList
    anchors
      .flatMap((key, groupedClaims) => clusterFor(key, groupedClaims, claims))
      .sortBy(cluster => (cluster.primaryPosition.ply, cluster.subject.toString, cluster.id))

  private final case class ClusterKey(
      subject: IdeaSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      subjectMove: Option[String],
      scope: Option[EvidenceScope],
      semanticAnchors: List[EvidenceSemanticAnchor]
  )

  private val causeBoundLayers: Set[EvidenceLayer] =
    Set(
      EvidenceLayer.Relation,
      EvidenceLayer.MoveMotif,
      EvidenceLayer.TacticalMechanism,
      EvidenceLayer.RelativeCause,
      EvidenceLayer.RelativeAssessment,
      EvidenceLayer.CandidateComparison,
      EvidenceLayer.Counterfactual,
      EvidenceLayer.MoveVerdictCertification
    )

  private val longTermExcludedLayers: Set[EvidenceLayer] =
    causeBoundLayers ++ Set(
      EvidenceLayer.Line,
      EvidenceLayer.Eval,
      EvidenceLayer.ThreatPressure
    )

  private[chessjudgment] def causeBoundLayer(layer: EvidenceLayer): Boolean =
    causeBoundLayers.contains(layer)

  private[chessjudgment] def longTermSupportExcludedLayer(layer: EvidenceLayer): Boolean =
    longTermExcludedLayers.contains(layer) ||
      StrategicMechanismEvidence.rawStrategicSourceLayer(layer)

  private def longTermSupportEligible(claim: ClaimSeed, graph: TypedEvidenceGraph): Boolean =
    claim.family.isLongTerm &&
      claim.engineComparison.isEmpty &&
      claim.evidence.nonEmpty &&
      !claim.evidence.exists(ref => longTermSupportExcludedLayer(ref.layer)) &&
      semanticAnchors(claim, graph).nonEmpty

  private def clusterKey(claim: ClaimSeed, graph: TypedEvidenceGraph): ClusterKey =
    ClusterKey(
      subject = claim.subject,
      primaryPosition = claim.primaryPosition,
      primaryLine = claim.primaryLine,
      subjectMove = claim.subjectMove,
      scope = Option.when(claim.primaryLine.nonEmpty || claim.subjectMove.nonEmpty)(claim.scope),
      semanticAnchors = semanticAnchors(claim, graph)
    )

  def semanticAnchors(claim: ClaimSeed, graph: TypedEvidenceGraph): List[EvidenceSemanticAnchor] =
      claim.evidence
        .flatMap(ref => graph.byId.get(ref.id))
        .flatMap(semanticAnchorsForRecord)
        .distinctBy(_.stableKey)
        .sortBy(_.stableKey)

  private def semanticAnchorsForRecord(record: EvidenceRecord): List[EvidenceSemanticAnchor] =
    import EvidenceSemanticAnchorKind.*
    record.payload match
      case payload: StrategicMechanismEvidence =>
        payload.semanticGroupingAnchors
      case CandidateComparisonEvidence(fact) =>
        List(
          Option.when(fact.kind == CandidateComparisonKind.BestVsSecond)(
            EvidenceSemanticAnchor.of(CandidateComparison, "best-vs-second")
          ),
          Option.when(fact.comparison.candidateSet.exists(_.onlyMove))(
            EvidenceSemanticAnchor.of(CandidateComparison, "only-move")
          ),
          fact.comparison.candidateSet
            .flatMap(_.bestToSecondWinPercentGapForMover)
            .map(gap => EvidenceSemanticAnchor.of(CandidateComparison, "best-second-wp", winPercentGapAnchor(gap)))
        ).flatten
      case payload: LineFactEvidence =>
        payload.semanticGroupingAnchors
      case payload: BoardFactEvidence =>
        payload.semanticGroupingAnchors
      case _ =>
        Nil

  private def winPercentGapAnchor(gap: Double): String =
    if gap >= 20.0 then "20+"
    else if gap >= 10.0 then "10+"
    else if gap >= 5.0 then "5+"
    else "small"

  private def clusterFor(
      key: ClusterKey,
      claims: List[ClaimSeed],
      allClaims: List[ClaimSeed]
  ): Option[ClaimSupportCluster] =
    val members = claims.sortBy(_.id)
    val anchorIds = members.map(_.id).toSet
    val evidence = members.flatMap(_.evidence).distinctBy(_.id)
    val interactions = clusterInteractions(anchorIds, allClaims)
    Option.when(members.size >= 2 || interactions.nonEmpty) {
      val supportingClaimIds = relatedClusterClaimIds(interactions, supportingInteraction, anchorIds)
      val constrainingClaimIds = relatedClusterClaimIds(interactions, constrainingInteraction, anchorIds)
      ClaimSupportCluster(
        id = s"claim-support:long-term:${members.head.id}",
        kind = ClaimSupportClusterKind.LongTermSupport,
        families = members.map(_.family).distinct.sortBy(_.toString),
        subject = key.subject,
        primaryPosition = key.primaryPosition,
        primaryLine = key.primaryLine,
        subjectMove = key.subjectMove,
        scope = key.scope,
        anchorClaimIds = members.map(_.id),
        supportingClaimIds = supportingClaimIds,
        constrainingClaimIds = constrainingClaimIds,
        ideas = members.flatMap(_.ideaRefs).distinctBy(_.id),
        evidence = evidence,
        presentLayers = evidence.map(_.layer).toSet,
        confidence = members.map(_.confidence).maxBy(confidenceScore),
        salienceDrivers = members.flatMap(_.salience.toList.flatMap(_.drivers)).distinct,
        interactions = interactions
      )
    }

  private def clusterInteractions(
      anchorIds: Set[String],
      claims: List[ClaimSeed]
  ): List[ClaimSupportClusterInteraction] =
    claims.flatMap { source =>
      source.salience.toList.flatMap(_.interactions).flatMap { interaction =>
        val sourceIsAnchor = anchorIds.contains(source.id)
        val targetIsAnchor = anchorIds.contains(interaction.relatedClaimId)
        Option.when(sourceIsAnchor != targetIsAnchor)(
          ClaimSupportClusterInteraction(
            kind = interaction.kind,
            sourceClaimId = source.id,
            targetClaimId = interaction.relatedClaimId,
            strength = interaction.strength,
            evidence = interaction.interactionEvidence,
            basis = interaction.basis
          )
        )
      }
    }.distinctBy(interaction =>
      (interaction.kind, interaction.sourceClaimId, interaction.targetClaimId, interaction.basis.map(ClaimInteractionBasis.stableKey).sorted)
    )

  private def relatedClusterClaimIds(
      interactions: List[ClaimSupportClusterInteraction],
      predicate: ClaimInteractionKind => Boolean,
      anchorIds: Set[String]
  ): List[String] =
    interactions
      .filter(interaction => predicate(interaction.kind))
      .flatMap(interaction => List(interaction.sourceClaimId, interaction.targetClaimId))
      .filterNot(anchorIds.contains)
      .distinct
      .sorted

  private def supportingInteraction(kind: ClaimInteractionKind): Boolean =
    kind match
      case ClaimInteractionKind.StrategicCompensationSupportsSacrifice | ClaimInteractionKind.ConversionSecuresAdvantage =>
        true
      case _ =>
        false

  private def constrainingInteraction(kind: ClaimInteractionKind): Boolean =
    kind match
      case ClaimInteractionKind.TacticalConstrainsLongTerm | ClaimInteractionKind.LongTermConstrainedByTactic |
          ClaimInteractionKind.TacticalRefutesStrategicPlan | ClaimInteractionKind.DefensiveNecessityOverridesPlan |
          ClaimInteractionKind.BadVerdictPreservesLocalIdea =>
        true
      case _ =>
        false

  private def confidenceScore(confidence: EvidenceConfidence): Int =
    confidence match
      case EvidenceConfidence.LegalReplayVerified => 5
      case EvidenceConfidence.EngineBacked        => 4
      case EvidenceConfidence.BoardDerived        => 3
      case EvidenceConfidence.Mixed               => 2
      case EvidenceConfidence.Heuristic           => 1

enum ClaimEventClusterKind:
  case TacticalEvent
  case DefensiveEvent
  case ConversionEvent
  case MaterialEvent

case class ClaimEventClusterInteraction(
    kind: ClaimInteractionKind,
    sourceClaimId: String,
    targetClaimId: String,
    strength: Int,
    evidence: List[EvidenceRef],
    basis: List[ClaimInteractionBasis]
)

enum ClaimEventMemberRole:
  case CauseOwner
  case VerdictCarrier
  case Witness

case class ClaimEventCauseProof(
    claimId: String,
    family: ClaimFamily,
    memberRole: ClaimEventMemberRole,
    causeKind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    causeRole: RelativeCauseRole,
    causeSourceSide: RelativeCauseSourceSide,
    causeImportance: RelativeCauseImportance,
    attributionKind: CauseAttributionKind,
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    supportEvidenceSourceIds: List[String],
    proofDirectKinds: List[String],
    proofContrastKinds: List[String],
    proofContextSupportKinds: List[String]
)

case class ClaimEventCluster(
    id: String,
    kind: ClaimEventClusterKind,
    causeKind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    causeRole: RelativeCauseRole,
    causeSourceSide: RelativeCauseSourceSide,
    causeImportance: RelativeCauseImportance,
    attributionKind: CauseAttributionKind,
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    eventRootMove: String,
    verdict: MoveChoiceVerdict,
    winPercentLossForMover: Double,
    candidateWinPercentDeltaForMover: Double,
    families: List[ClaimFamily],
    primaryPosition: PositionNodeRef,
    scope: EvidenceScope,
    memberClaimIds: List[String],
    causeClaimIds: List[String],
    evaluationClaimIds: List[String],
    witnessClaimIds: List[String],
    relatedSupportClusterIds: List[String],
    ideas: List[ChessIdeaRef],
    evidence: List[EvidenceRef],
    presentLayers: Set[EvidenceLayer],
    proofBoardAnchors: List[BoardAnchorKind],
    proofLineEvents: List[LineEventKind],
    proofLineConsequences: List[LineConsequenceKind],
    proofRelationKinds: List[RelationFactKind],
    proofRelationDetails: List[String],
    proofTacticalMechanisms: List[TacticalMechanismProof],
    proofStrategicMechanisms: List[StrategicMechanismProof],
    proofTransitionConsequences: List[TransitionConsequenceProof],
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofDirectKinds: List[String],
    proofContrastKinds: List[String],
    proofContextSupportKinds: List[String],
    causeProofs: List[ClaimEventCauseProof],
    causeContextLayers: Set[EvidenceLayer],
    confidence: EvidenceConfidence,
    salienceDrivers: List[ClaimSalienceDriver],
    interactions: List[ClaimEventClusterInteraction],
    objectBindingSignatures: List[String] = Nil
)

object ClaimEventCluster:

  def fromClaims(
      claims: List[ClaimSeed],
      graph: TypedEvidenceGraph,
      supportClusters: List[ClaimSupportCluster] = Nil
  ): List[ClaimEventCluster] =
    val keyedBindings =
      claims
        .filter(eventClaimEligible)
        .flatMap(claim => eventBindingsForClaim(claim, graph))
    keyedBindings
      .groupBy(_.key)
      .toList
      .flatMap { case (key, bindings) =>
        clusterFor(
          key = key,
          bindings = bindings,
          allClaims = claims,
          supportClusters = supportClusters,
          graph = graph
        )
      }
      .sortBy(cluster =>
        (
          cluster.primaryPosition.ply,
          cluster.comparisonKind.toString,
          cluster.causeRole.toString,
          cluster.causeSourceSide.toString,
          cluster.causeImportance.toString,
          cluster.causeKind.toString,
          cluster.eventRootMove,
          cluster.id
        )
      )

  private final case class EventClusterKey(
      primaryPosition: PositionNodeRef,
      scope: EvidenceScope,
      causeKind: RelativeCauseKind,
      comparisonKind: CandidateComparisonKind,
      causeRole: RelativeCauseRole,
      causeSourceSide: RelativeCauseSourceSide,
      causeImportance: RelativeCauseImportance,
      attributionKind: CauseAttributionKind,
      attributionRootMoveMatched: Boolean,
      attributionDirectProofEligible: Boolean,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      eventLine: LineNodeRef,
      verdict: MoveChoiceVerdict,
      winPercentLossForMover: Double,
      candidateWinPercentDeltaForMover: Double
  ):
    def eventRootMove: String = eventLine.rootMove

  private final case class EventMemberBinding(
      key: EventClusterKey,
      claim: ClaimSeed,
      role: ClaimEventMemberRole,
      cause: Option[RelativeCauseFact] = None,
      proof: Option[RelativeCauseProof] = None
  )

  private val eventEvidenceLayers: Set[EvidenceLayer] =
    Set(
      EvidenceLayer.SinglePosition,
      EvidenceLayer.Line,
      EvidenceLayer.Eval,
      EvidenceLayer.ThreatPressure,
      EvidenceLayer.MoveMotif,
      EvidenceLayer.TacticalMechanism,
      EvidenceLayer.MoveTransition,
      EvidenceLayer.Relation,
      EvidenceLayer.RelativeAssessment,
      EvidenceLayer.CandidateComparison,
      EvidenceLayer.Counterfactual,
      EvidenceLayer.RelativeCause,
      EvidenceLayer.MoveVerdictCertification
    )

  private def eventClaimEligible(claim: ClaimSeed): Boolean =
    claim.family.isEvent && claim.evidence.nonEmpty

  private val eventInteractionEvidenceLayers: Set[EvidenceLayer] =
    Set(
      EvidenceLayer.Line,
      EvidenceLayer.Eval,
      EvidenceLayer.ThreatPressure,
      EvidenceLayer.MoveMotif,
      EvidenceLayer.TacticalMechanism,
      EvidenceLayer.MoveTransition,
      EvidenceLayer.Relation,
      EvidenceLayer.RelativeAssessment,
      EvidenceLayer.CandidateComparison,
      EvidenceLayer.Counterfactual,
      EvidenceLayer.RelativeCause,
      EvidenceLayer.MoveVerdictCertification
    )

  private def eventBindingsForClaim(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph
  ): List[EventMemberBinding] =
    val records = recordsFor(claim.evidence, graph)
    val direct = records.flatMap(eventBindingsForRecord(claim, _)).distinct
    if direct.nonEmpty then direct
    else if indirectWitnessEligible(claim) then
      val evidenceIds = claim.evidence.map(_.id).toSet
      val parentRecords = recordsFor(records.flatMap(_.parents), graph)
      val childRecords =
        graph.records.filter(record => record.parents.exists(parent => evidenceIds.contains(parent.id)))
      val linked = (parentRecords ++ childRecords).flatMap(eventKeysForRecord).distinct
      lineAwareWitnessKeys(claim, linked).map(key =>
        EventMemberBinding(key = key, claim = claim, role = ClaimEventMemberRole.Witness)
      )
    else Nil

  private def recordsFor(refs: List[EvidenceRef], graph: TypedEvidenceGraph): List[EvidenceRecord] =
    refs.flatMap(ref => graph.byId.get(ref.id))

  private def eventBindingsForRecord(claim: ClaimSeed, record: EvidenceRecord): List[EventMemberBinding] =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        eventKey(record.ref, cause).toList.map { key =>
          val role =
            if claim.family == ClaimFamily.Evaluation then ClaimEventMemberRole.Witness
            else ClaimEventMemberRole.CauseOwner
          EventMemberBinding(key = key, claim = claim, role = role, cause = Some(cause), proof = cause.proof)
        }
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.flatMap { cause =>
          eventKey(record.ref, cause).map { key =>
            val role =
              if claim.family == ClaimFamily.Evaluation then ClaimEventMemberRole.VerdictCarrier
              else ClaimEventMemberRole.CauseOwner
            EventMemberBinding(key = key, claim = claim, role = role, cause = Some(cause), proof = cause.proof)
          }
        }
      case _ =>
        Nil

  private def eventKeysForRecord(record: EvidenceRecord): List[EventClusterKey] =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        eventKey(record.ref, cause).toList
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.flatMap(cause => eventKey(record.ref, cause))
      case _ =>
        Nil

  private def eventKey(ref: EvidenceRef, cause: RelativeCauseFact): Option[EventClusterKey] =
    Option.when(kindForCause(cause.kind).nonEmpty)(
      EventClusterKey(
        primaryPosition = ref.position,
        scope = ref.scope,
        causeKind = cause.kind,
        comparisonKind = cause.comparisonKind,
        causeRole = cause.role,
        causeSourceSide = cause.sourceSide,
        causeImportance = cause.importance,
        attributionKind = cause.attribution.kind,
        attributionRootMoveMatched = cause.attribution.rootMoveMatched,
        attributionDirectProofEligible = cause.attribution.directProofEligible,
        referenceLine = cause.referenceLine,
        candidateLine = cause.candidateLine,
        eventLine = cause.eventLine,
        verdict = cause.verdict,
        winPercentLossForMover = cause.winPercentLossForMover,
        candidateWinPercentDeltaForMover = cause.candidateWinPercentDeltaForMover
      )
    )

  private def indirectWitnessEligible(claim: ClaimSeed): Boolean =
    claim.family match
      case ClaimFamily.Tactical | ClaimFamily.Conversion =>
        claim.evidence.exists(ref =>
          ref.layer == EvidenceLayer.TacticalMechanism ||
            ref.layer == EvidenceLayer.Relation ||
            ref.layer == EvidenceLayer.MoveMotif
        )
      case ClaimFamily.Material =>
        claim.evidence.exists(ref => ref.layer == EvidenceLayer.Line)
      case _ =>
        false

  private def lineAwareWitnessKeys(
      claim: ClaimSeed,
      keys: List[EventClusterKey]
  ): List[EventClusterKey] =
    val unique = keys.distinct
    val claimLines = (claim.primaryLine.toList ++ claim.evidence.flatMap(_.line)).distinct
    val claimMoves = (claim.subjectMove.toList ++ claimLines.map(_.rootMove)).map(normalizeMove).toSet
    val eventLineMatched =
      unique.filter(key => claimLines.contains(key.eventLine))
    val eventMoveMatched =
      unique.filter(key => claimMoves.contains(normalizeMove(key.eventRootMove)))
    List(eventLineMatched, eventMoveMatched)
      .find(_.nonEmpty)
      .getOrElse(Nil)
      .distinct

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def clusterFor(
      key: EventClusterKey,
      bindings: List[EventMemberBinding],
      allClaims: List[ClaimSeed],
      supportClusters: List[ClaimSupportCluster],
      graph: TypedEvidenceGraph
  ): Option[ClaimEventCluster] =
    val members = bindings.map(_.claim).distinctBy(_.id).sortBy(_.id)
    val memberIds = members.map(_.id).toSet
    val causeClaimIds =
      bindings
        .filter(_.role == ClaimEventMemberRole.CauseOwner)
        .map(_.claim)
        .filterNot(_.family == ClaimFamily.Evaluation)
        .distinctBy(_.id)
        .sortBy(_.id)
        .map(_.id)
    val evaluationClaimIds =
      bindings
        .filter(_.role == ClaimEventMemberRole.VerdictCarrier)
        .map(_.claim)
        .filter(_.family == ClaimFamily.Evaluation)
        .distinctBy(_.id)
        .sortBy(_.id)
        .map(_.id)
    val witnessClaimIds =
      bindings
        .filter(_.role == ClaimEventMemberRole.Witness)
        .map(_.claim)
        .filterNot(claim => causeClaimIds.contains(claim.id) || evaluationClaimIds.contains(claim.id))
        .distinctBy(_.id)
        .sortBy(_.id)
        .map(_.id)
    val evidence =
      members.flatMap(_.evidence).filter(ref => eventEvidenceLayers.contains(ref.layer)).distinctBy(_.id)
    val proof =
      mergedProof(bindings.filter(_.role == ClaimEventMemberRole.CauseOwner).flatMap(_.proof))
    val claimProof = proof.depthProof
    val causeProofs = bindings.flatMap(causeProofForBinding).distinctBy(proof =>
      (
        proof.claimId,
        proof.memberRole.toString,
        proof.causeKind.toString,
        proof.comparisonKind.toString,
        proof.causeRole.toString,
        proof.causeSourceSide.toString,
        proof.causeImportance.toString,
        proof.attributionKind.toString,
        proof.attributionRootMoveMatched.toString,
        proof.attributionDirectProofEligible.toString,
        proof.eventLine.id,
        proof.proofDirectSourceIds.mkString(","),
        proof.proofContrastSourceIds.mkString(","),
        proof.proofContextSupportSourceIds.mkString(","),
        proof.proofStrategicAxisKeys.mkString(","),
        proof.proofStrategicMechanismSourceIds.mkString(","),
        proof.proofStrategicMechanismSignalSourceIds.mkString(","),
        proof.supportEvidenceSourceIds.mkString(",")
      )
    )
    val interactions = clusterInteractions(memberIds, allClaims)
    val relatedSupportClusterIds = supportClustersRelatedTo(memberIds, interactions, supportClusters)
    Option.when(causeClaimIds.nonEmpty && evidence.nonEmpty) {
      ClaimEventCluster(
        id = eventMembershipClusterId(key, members),
        kind = clusterKind(key.causeKind),
        causeKind = key.causeKind,
        comparisonKind = key.comparisonKind,
        causeRole = key.causeRole,
        causeSourceSide = key.causeSourceSide,
        causeImportance = key.causeImportance,
        attributionKind = key.attributionKind,
        attributionRootMoveMatched = key.attributionRootMoveMatched,
        attributionDirectProofEligible = key.attributionDirectProofEligible,
        referenceLine = key.referenceLine,
        candidateLine = key.candidateLine,
        eventLine = key.eventLine,
        eventRootMove = key.eventRootMove,
        verdict = key.verdict,
        winPercentLossForMover = key.winPercentLossForMover,
        candidateWinPercentDeltaForMover = key.candidateWinPercentDeltaForMover,
        families = members.map(_.family).distinct.sortBy(_.toString),
        primaryPosition = key.primaryPosition,
        scope = key.scope,
        memberClaimIds = members.map(_.id),
        causeClaimIds = causeClaimIds,
        evaluationClaimIds = evaluationClaimIds,
        witnessClaimIds = witnessClaimIds,
        relatedSupportClusterIds = relatedSupportClusterIds,
        ideas = members.flatMap(_.ideaRefs).distinctBy(_.id),
        evidence = evidence,
        presentLayers = evidence.map(_.layer).toSet,
        proofBoardAnchors = claimProof.boardAnchors,
        proofLineEvents = claimProof.lineEvents,
        proofLineConsequences = claimProof.lineConsequences,
        proofRelationKinds = claimProof.relationKinds,
        proofRelationDetails = claimProof.relationDetails,
        proofTacticalMechanisms = claimProof.tacticalMechanisms,
        proofStrategicMechanisms = claimProof.strategicMechanisms,
        proofTransitionConsequences = claimProof.transitionConsequences,
        proofDirectSourceIds = proof.directProof.sourceRefs.map(_.id).distinct.sorted,
        proofContrastSourceIds = proof.contrastProof.sourceRefs.map(_.id).distinct.sorted,
        proofContextSupportSourceIds = proof.contextSupport.sourceRefs.map(_.id).distinct.sorted,
        proofDirectKinds = proof.directProof.kindLabels.distinct.sorted,
        proofContrastKinds = proof.contrastProof.kindLabels.distinct.sorted,
        proofContextSupportKinds = proof.contextSupport.kindLabels.distinct.sorted,
        causeProofs = causeProofs,
        causeContextLayers = proof.contextLayers.toSet,
        confidence = members.map(_.confidence).maxBy(confidenceScore),
        salienceDrivers = members.flatMap(_.salience.toList.flatMap(_.drivers)).distinct,
        interactions = interactions,
        objectBindingSignatures = clusterObjectBindingSignatures(bindings, graph)
      )
    }

  private def clusterObjectBindingSignatures(
      bindings: List[EventMemberBinding],
      graph: TypedEvidenceGraph
  ): List[String] =
    EvidenceObjectBinding.objectSignatures(
      bindings.flatMap(binding =>
        binding.cause.toList.flatMap(cause => EvidenceObjectBinding.fromRelativeCause(cause, graph)) ++
          EvidenceObjectBinding.fromClaim(binding.claim, graph)
      )
    )

  private def mergedProof(proofs: List[RelativeCauseProof]): RelativeCauseProof =
    RelativeCauseProof.merge(proofs)

  private def causeProofForBinding(binding: EventMemberBinding): Option[ClaimEventCauseProof] =
    binding.cause.map { cause =>
      val proof = binding.proof.getOrElse(RelativeCauseProof())
      val strategicProof = proof.strategicProofIdentity
      ClaimEventCauseProof(
        claimId = binding.claim.id,
        family = binding.claim.family,
        memberRole = binding.role,
        causeKind = binding.key.causeKind,
        comparisonKind = binding.key.comparisonKind,
        causeRole = binding.key.causeRole,
        causeSourceSide = binding.key.causeSourceSide,
        causeImportance = binding.key.causeImportance,
        attributionKind = cause.attribution.kind,
        attributionRootMoveMatched = cause.attribution.rootMoveMatched,
        attributionDirectProofEligible = cause.attribution.directProofEligible,
        referenceLine = binding.key.referenceLine,
        candidateLine = binding.key.candidateLine,
        eventLine = binding.key.eventLine,
        proofDirectSourceIds = proof.directProof.sourceRefs.map(_.id).distinct.sorted,
        proofContrastSourceIds = proof.contrastProof.sourceRefs.map(_.id).distinct.sorted,
        proofContextSupportSourceIds = proof.contextSupport.sourceRefs.map(_.id).distinct.sorted,
        proofStrategicAxisKeys = strategicProof.axisKeys,
        proofStrategicMechanismKinds = strategicProof.mechanismKinds,
        proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
        proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
        supportEvidenceSourceIds = cause.supportEvidence.map(_.id).distinct.sorted,
        proofDirectKinds = proof.directProof.kindLabels.distinct.sorted,
        proofContrastKinds = proof.contrastProof.kindLabels.distinct.sorted,
        proofContextSupportKinds = proof.contextSupport.kindLabels.distinct.sorted
      )
    }

  private def eventMembershipClusterId(key: EventClusterKey, members: List[ClaimSeed]): String =
    val lineIdentity =
      stableIdHash(List(key.referenceLine.id, key.candidateLine.id, key.eventLine.id).mkString("||"))
    val memberIdentity =
      stableIdHash(members.map(_.id).sorted.mkString("||"))
    List(
      "claim-event-membership",
      key.comparisonKind.toString,
      key.causeRole.toString,
      key.causeSourceSide.toString,
      key.causeImportance.toString,
      key.attributionKind.toString,
      key.attributionRootMoveMatched.toString,
      key.attributionDirectProofEligible.toString,
      key.causeKind.toString,
      lineIdentity,
      key.eventRootMove,
      memberIdentity
    ).map(idPart).mkString(":")

  private def stableIdHash(raw: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
      .take(16)

  private def idPart(raw: String): String =
    raw.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  private def clusterKind(causeKind: RelativeCauseKind): ClaimEventClusterKind =
    kindForCause(causeKind).getOrElse(ClaimEventClusterKind.TacticalEvent)

  def kindForCause(kind: RelativeCauseKind): Option[ClaimEventClusterKind] =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability |
          RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
          RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
        Some(ClaimEventClusterKind.TacticalEvent)
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity |
          RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        Some(ClaimEventClusterKind.DefensiveEvent)
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        Some(ClaimEventClusterKind.ConversionEvent)
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        Some(ClaimEventClusterKind.MaterialEvent)
      case _ =>
        None

  private def clusterInteractions(
      memberIds: Set[String],
      claims: List[ClaimSeed]
  ): List[ClaimEventClusterInteraction] =
    val claimsById = claims.map(claim => claim.id -> claim).toMap
    claims.flatMap { source =>
      source.salience.toList.flatMap(_.interactions).flatMap { interaction =>
        val sourceIsMember = memberIds.contains(source.id)
        val targetIsMember = memberIds.contains(interaction.relatedClaimId)
        Option.when(sourceIsMember || targetIsMember)(
          ClaimEventClusterInteraction(
            kind = interaction.kind,
            sourceClaimId = source.id,
            targetClaimId = interaction.relatedClaimId,
            strength = interaction.strength,
            evidence =
              if crossesLongTerm(source, interaction.relatedClaimId, memberIds, claimsById) then Nil
              else interaction.interactionEvidence.filter(ref => eventInteractionEvidenceLayers.contains(ref.layer)),
            basis = interaction.basis
          )
        )
      }
    }.distinctBy(interaction =>
      (interaction.kind, interaction.sourceClaimId, interaction.targetClaimId, interaction.basis.map(ClaimInteractionBasis.stableKey).sorted)
    )

  private def crossesLongTerm(
      source: ClaimSeed,
      targetClaimId: String,
      memberIds: Set[String],
      claimsById: Map[String, ClaimSeed]
  ): Boolean =
    val sourceExternalLongTerm = !memberIds.contains(source.id) && source.family.isLongTerm
    val targetExternalLongTerm = !memberIds.contains(targetClaimId) && claimsById.get(targetClaimId).exists(_.family.isLongTerm)
    sourceExternalLongTerm || targetExternalLongTerm

  private def supportClustersRelatedTo(
      memberIds: Set[String],
      interactions: List[ClaimEventClusterInteraction],
      supportClusters: List[ClaimSupportCluster]
  ): List[String] =
    val externallyRelatedClaims =
      interactions
        .flatMap(interaction => List(interaction.sourceClaimId, interaction.targetClaimId))
        .filterNot(memberIds.contains)
        .toSet
    supportClusters
      .filter { cluster =>
        val supportIds =
          (cluster.anchorClaimIds ++ cluster.supportingClaimIds ++ cluster.constrainingClaimIds).toSet
        supportIds.exists(memberIds.contains) || supportIds.exists(externallyRelatedClaims.contains)
      }
      .map(_.id)
      .distinct
      .sorted

  private def confidenceScore(confidence: EvidenceConfidence): Int =
    confidence match
      case EvidenceConfidence.LegalReplayVerified => 5
      case EvidenceConfidence.EngineBacked        => 4
      case EvidenceConfidence.BoardDerived        => 3
      case EvidenceConfidence.Mixed               => 2
      case EvidenceConfidence.Heuristic           => 1

case class ClaimSeed(
    id: String,
    family: ClaimFamily,
    idea: Option[ChessIdeaRef],
    subject: IdeaSubject,
    primaryPosition: PositionNodeRef,
    primaryLine: Option[LineNodeRef],
    subjectMove: Option[String],
    evidence: List[EvidenceRef],
    engineComparison: Option[EvalComparison],
    scope: EvidenceScope,
    confidence: EvidenceConfidence,
    supportStatus: Option[ClaimSupportCheck] = None,
    salience: Option[ClaimSalience] = None,
    relatedIdeas: List[ChessIdeaRef] = Nil
):
  def ideaRefs: List[ChessIdeaRef] =
    (idea.toList ++ relatedIdeas).distinctBy(_.id)

case class IdeaVerdictSplit(
    ideas: List[ChessIdeaRef],
    ideaClaims: List[ClaimSeed],
    verdict: Option[EvalComparison],
    bindings: List[IdeaVerdictBinding]
)

object IdeaVerdictSplit:
  def from(
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      assessments: List[RelativeMoveAssessment]
  ): Option[IdeaVerdictSplit] =
    val ideaRefs = ideas.map(_.ref)
    val ideaClaims = claims.filter(claim => claim.ideaRefs.exists(ideaRefs.contains))
    val assessment = assessments.headOption
    val verdict = assessment.map(_.comparison)
    val bindings =
      assessments.flatMap { relative =>
        ideas.filter(ideaBindsToRelative(_, relative)).map { idea =>
          IdeaVerdictBinding(
            idea = idea.ref,
            verdict = relative.comparison,
            relation = relationFor(idea, relative.comparison),
            evidence =
              (idea.evidence ++
                (relative.evidence ::
                  (relative.counterfactualEvidence ++
                    relative.candidateComparisonEvidence ++
                    relative.relativeCauseEvidence ++
                    relative.verdictCertificationEvidence.toList))).distinctBy(_.id)
          )
        }
      }
    Option.when(ideaRefs.nonEmpty || ideaClaims.nonEmpty || verdict.nonEmpty)(
      IdeaVerdictSplit(
        ideas = ideaRefs,
        ideaClaims = ideaClaims,
        verdict = verdict,
        bindings = bindings
      )
    )

  private def relationFor(idea: ChessIdea, comparison: EvalComparison): IdeaVerdictRelation =
    comparison.verdict match
      case MoveChoiceVerdict.ImprovesOnReference | MoveChoiceVerdict.MatchesReference | MoveChoiceVerdict.PlayableLoss =>
        if idea.ref.family == ChessIdeaFamily.Defensive then IdeaVerdictRelation.DefensiveNecessity
        else IdeaVerdictRelation.SupportsVerdict
      case MoveChoiceVerdict.Blunder
          if idea.subject == IdeaSubject.PlayedMove || idea.moveUci.nonEmpty =>
        IdeaVerdictRelation.RefutesIdea
      case MoveChoiceVerdict.Mistake
          if idea.ref.family == ChessIdeaFamily.Tactical && (idea.subject == IdeaSubject.PlayedMove || idea.moveUci.nonEmpty) =>
        IdeaVerdictRelation.RefutesIdea
      case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder =>
        IdeaVerdictRelation.ExplainsIdeaDespiteBadVerdict

  private def ideaBindsToRelative(idea: ChessIdea, relative: RelativeMoveAssessment): Boolean =
    val relativeLines = Set(relative.reference.ref, relative.candidate.ref)
    val relativeEvidence =
      (relative.evidence ::
        (relative.counterfactualEvidence ++
          relative.candidateComparisonEvidence ++
          relative.relativeCauseEvidence ++
          relative.verdictCertificationEvidence.toList)).map(_.id).toSet
    idea.ref.family == ChessIdeaFamily.Evaluation ||
      idea.primaryLine.exists(relativeLines.contains) ||
      idea.moveUci.contains(relative.played.moveUci) ||
      idea.evidence.exists(ref =>
        relativeEvidence.contains(ref.id) ||
          ref.line.exists(relativeLines.contains)
      )

enum MoveJudgmentCauseFrameRole:
  case PrimaryCause
  case SecondaryCause
  case ContextCause

enum MoveJudgmentCauseNarrativeRole:
  case RootCause
  case TacticalWitness
  case SupportingCause
  case ContextCause

enum MoveJudgmentCauseWitnessBindingLevel:
  case NotWitness
  case SameComparisonOnly
  case LineContext
  case ObjectContext
  case Punishment

enum MoveJudgmentCauseRootArbitrationTier:
  case ExactOwnedRoot
  case ConcreteOwnedRoot
  case BroadOwnedRoot
  case FallbackRoot
  case ContextOnly

enum MoveJudgmentCauseWitnessBindingSignal:
  case SameComparison
  case SharedExactObjectSignature
  case SharedActor
  case SharedTarget
  case SharedMechanism
  case SharedConsequence
  case SharedWitness
  case SharedDirectObjectSignature
  case SharedDirectActor
  case SharedDirectTarget
  case SharedDirectMechanism
  case SharedDirectConsequence
  case SameEventLine
  case DirectProofSourceOverlap

case class MoveJudgmentVerdictFrame(
    verdict: MoveChoiceVerdict,
    winPercentLossForMover: Double,
    candidateWinPercentDeltaForMover: Double,
    relativeAssessmentEvidenceId: String,
    verdictCertificationEvidenceId: Option[String],
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    candidateSet: Option[CandidateSetComparison] = None
)

case class MoveJudgmentClaimFrame(
    claimId: String,
    family: ClaimFamily,
    tier: PlayerFacingClaimTier,
    subjectBinding: SubjectBindingClass,
    ideaIds: List[String],
    evidenceIds: List[String],
    strategicAxisLineage: List[ClaimStrategicAxisLineage],
    strategicAxisKeys: List[String],
    strategicAxisMechanismEvidenceIds: List[String],
    strategicAxisSourceEvidenceIds: List[String],
    strategicAxisRelativeCauseIds: List[String],
    objectBindingSignatures: List[String],
    concreteObjectReady: Boolean
)

case class MoveJudgmentCauseFrame(
    role: MoveJudgmentCauseFrameRole,
    clusterId: Option[String],
    framed: Boolean,
    causeEvidenceIds: List[String],
    causeKind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    causeRole: RelativeCauseRole,
    causeSourceSide: RelativeCauseSourceSide,
    causeImportance: RelativeCauseImportance,
    attributionKind: CauseAttributionKind,
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    eventRootMove: String,
    causeClaimIds: List[String],
    evaluationClaimIds: List[String],
    witnessClaimIds: List[String],
    ideaIds: List[String],
    supportIdeaIds: List[String],
    claimCandidateIds: List[String],
    finalClaimIds: List[String],
    relatedSupportClusterIds: List[String],
    evidenceIds: List[String],
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofStrategicAxisLineage: List[StrategicAxisProofLineage],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    supportEvidenceSourceIds: List[String],
    proofLineConsequences: List[LineConsequenceKind] = Nil,
    objectBindingSignatures: List[String],
    concreteObjectReady: Boolean,
    hasOwnedAdmissibleLongTermProof: Boolean = false,
    hasOwnedTacticalProof: Boolean = false,
    narrativeRole: MoveJudgmentCauseNarrativeRole = MoveJudgmentCauseNarrativeRole.RootCause,
    rootArbitrationTier: MoveJudgmentCauseRootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ContextOnly,
    tacticalWitnessCauseEvidenceIds: List[String] = Nil,
    tacticalWitnessCauseKinds: List[RelativeCauseKind] = Nil,
    punishmentWitnessCauseEvidenceIds: List[String] = Nil,
    punishmentWitnessCauseKinds: List[RelativeCauseKind] = Nil,
    contextualTacticalWitnessCauseEvidenceIds: List[String] = Nil,
    contextualTacticalWitnessCauseKinds: List[RelativeCauseKind] = Nil,
    witnessBindingLevel: MoveJudgmentCauseWitnessBindingLevel = MoveJudgmentCauseWitnessBindingLevel.NotWitness,
    witnessBindingSignals: List[MoveJudgmentCauseWitnessBindingSignal] = Nil,
    witnessBindingRootCauseEvidenceIds: List[String] = Nil
)

case class MoveJudgmentCauseAudit(
    primary: List[MoveJudgmentCauseFrame] = Nil,
    secondary: List[MoveJudgmentCauseFrame] = Nil,
    context: List[MoveJudgmentCauseFrame] = Nil
):
  def all: List[MoveJudgmentCauseFrame] =
    primary ++ secondary ++ context

case class MoveMeaningClaim(
    meaningKind: String,
    role: String,
    laneKey: String,
    conflictKey: Option[String],
    supportLevel: String,
    visibility: String,
    surfaceLane: String,
    lineRole: String,
    moveUci: String,
    frameId: String,
    unit: PositionPlanTechniqueUnit,
    axisKey: Option[String],
    axisKind: Option[StrategicAxisKind],
    axisPolarity: Option[StrategicAxisPolarity],
    label: Option[String],
    causeKinds: List[RelativeCauseKind],
    causeSourceSides: List[RelativeCauseSourceSide],
    causeEvidenceIds: List[String],
    sourceEvidenceIds: List[String],
    objectBindingSignatures: List[String],
    reasonTokens: List[String] = Nil,
    comparisonLossSides: List[String] = Nil,
    comparisonLossKinds: List[String] = Nil,
    objectCarrierReady: Boolean = false,
    boardCarriers: List[MoveMeaningSurfaceBoardCarrier] = Nil,
    comparisonMoveRefs: List[MoveMeaningSurfaceMoveRef] = Nil,
    targetSquares: List[String] = Nil,
    targetFiles: List[String] = Nil,
    targetPieces: List[String] = Nil,
    routeIdentityParts: List[String] = Nil,
    breakIdentityParts: List[String] = Nil,
    breakFiles: List[String] = Nil,
    structuralMotifTags: List[String] = Nil,
    specificityTier: PositionPlanTechniqueSpecificityTier = PositionPlanTechniqueSpecificityTier.ContextOnly,
    terminalConsequenceKinds: List[String] = Nil,
    endgameTechniquePattern: Option[String] = None,
    endgameTechniqueRookPattern: Option[String] = None,
    endgameTechniqueSide: Option[String] = None,
    endgameTechniqueHorizonStatus: Option[String] = None,
    endgameTechniqueTriggerMove: Option[String] = None,
    endgameTechniqueEntryPlyOffset: Option[Int] = None,
    endgameTechniqueTerminalPlyOffset: Option[Int] = None,
    endgameTechniqueFailureReason: Option[String] = None,
    requiredSquares: List[String] = Nil,
    maintainedSquares: List[String] = Nil,
    brokenSquares: List[String] = Nil,
    publicIdeaType: Option[String] = None,
    publicHasCarrier: Boolean = false,
    publicProofLevel: String = "none",
    publicTargetBound: Boolean = false
)

case class MoveMeaningSurfaceTarget(
    squares: List[String] = Nil,
    files: List[String] = Nil,
    pieces: List[String] = Nil
)

object MoveMeaningSurfaceTarget:
  def fromClaim(claim: MoveMeaningClaim): MoveMeaningSurfaceTarget =
    MoveMeaningSurfaceTarget(
      squares = claim.targetSquares.flatMap(cleanSquare).distinct.sorted,
      files = claim.targetFiles.flatMap(cleanFile).distinct.sorted,
      pieces = (claim.targetPieces ++ routePieces(claim.routeIdentityParts)).flatMap(cleanPiece).distinct.sorted
    )

  private[judgment] def fromDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      boardCarriers: List[MoveMeaningSurfaceBoardCarrier]
  ): MoveMeaningSurfaceTarget =
    val targetCarriers = boardCarriers.filter(_.role == "target")
    val pawnTargets = targetCarriers.filter(_.kind == "Pawn").map(_.value)
    MoveMeaningSurfaceTarget(
      squares =
        (
          targetCarriers.filter(_.kind == "Square").map(_.value) ++
            pawnTargets.flatMap(squareFromText) ++
            detail.tensionSquares ++
            detail.blockadeSquare.toList ++
            detail.resourceContestSquares ++
            detail.requiredSquares ++
            detail.maintainedSquares ++
            detail.brokenSquares
        ).flatMap(cleanSquare).distinct.sorted,
      files =
        (
          targetCarriers.filter(_.kind == "File").map(_.value) ++
            detail.breakFile.toList ++
            detail.counterBreakFiles ++
            detail.resourceContestFiles
        ).flatMap(cleanFile).distinct.sorted,
      pieces =
        (
          targetCarriers.filter(_.kind == "Piece").map(_.value).flatMap(cleanPiece) ++
            EvidenceObjectBinding
              .signatureValues(detail.objectBindingSignatures, "target", "Piece")
              .flatMap(cleanPiece) ++
            Option.when(pawnTargets.nonEmpty)("pawn").toList
        ).distinct.sorted
    )

  private def cleanSquare(value: String): Option[String] =
    val cleaned = cleanTheme(value)
    Option.when(cleaned.matches("[a-h][1-8]"))(cleaned)

  private def cleanFile(value: String): Option[String] =
    val cleaned = cleanTheme(value)
    Option.when(cleaned.matches("[a-h]"))(cleaned)

  private def cleanPiece(value: String): Option[String] =
    val cleaned = cleanTheme(value).split("_").headOption.getOrElse("")
    Option.when(
      cleaned == "king" ||
        cleaned == "queen" ||
        cleaned == "rook" ||
        cleaned == "bishop" ||
        cleaned == "knight" ||
        cleaned == "pawn"
    )(cleaned)

  private def routePieces(parts: List[String]): List[String] =
    parts.collect { case part if part.startsWith("piece:") => part.stripPrefix("piece:") }

  private def squareFromText(value: String): List[String] =
    "[a-h][1-8]".r.findAllIn(cleanTheme(value)).toList.distinct

  private def cleanTheme(value: String): String =
    value.trim
      .replaceAll("([a-z])([A-Z])", "$1_$2")
      .replaceAll("[^A-Za-z0-9]+", "_")
      .replaceAll("_+", "_")
      .stripPrefix("_")
      .stripSuffix("_")
      .toLowerCase

case class MoveMeaningSurfaceVerdict(
    verdictCode: String,
    moveQuality: String,
    playedMove: String,
    referenceMove: String
)

case class MoveMeaningSurfaceCode(
    code: String,
    label: String
)

case class MoveMeaningSurfaceMoveRef(
    role: String,
    uci: String
)

case class MoveMeaningSurfaceComparisonLoss(
    side: String,
    code: String,
    label: String
)

case class MoveMeaningSurfaceAssessment(
    moveQuality: MoveMeaningSurfaceCode,
    ideaQuality: MoveMeaningSurfaceCode,
    priority: MoveMeaningSurfaceCode,
    verdictReason: Boolean,
    localIdea: Boolean,
    failureFamily: Option[MoveMeaningSurfaceCode] = None,
    problem: Option[MoveMeaningSurfaceCode] = None
)

case class MoveMeaningSurfaceEndgameTechnique(
    pattern: Option[String] = None,
    patternLabel: Option[String] = None,
    rookPattern: Option[String] = None,
    rookPatternLabel: Option[String] = None,
    side: Option[String] = None,
    horizonStatus: Option[String] = None,
    statusLabel: Option[String] = None,
    triggerMove: Option[String] = None,
    entryPlyOffset: Option[Int] = None,
    terminalPlyOffset: Option[Int] = None,
    requiredSquares: List[String] = Nil,
    maintainedSquares: List[String] = Nil,
    brokenSquares: List[String] = Nil,
    terminalConsequences: List[MoveMeaningSurfaceCode] = Nil,
    failureReason: Option[MoveMeaningSurfaceCode] = None
)

case class MoveMeaningSurfaceComparison(
    kind: String,
    relation: String,
    referenceMove: String,
    candidateMove: String,
    secondMove: Option[String] = None,
    moves: List[MoveMeaningSurfaceMoveRef] = Nil,
    lostIdeas: List[MoveMeaningSurfaceComparisonLoss] = Nil
)

case class MoveMeaningSurfaceEvidence(
    hasCarrier: Boolean = false,
    proofLevel: String = "none",
    targetBound: Boolean = false,
    causeIds: List[String] = Nil,
    sourceIds: List[String] = Nil,
    boardCarriers: List[MoveMeaningSurfaceBoardCarrier] = Nil
)

case class MoveMeaningSurfaceBoardCarrier(
    role: String,
    kind: String,
    value: String,
    from: Option[String] = None,
    to: Option[String] = None
)

case class MoveMeaningSurface(
    moveUci: String,
    subject: String,
    lineRole: String,
    moveQuality: String,
    ideaType: String,
    idea: MoveMeaningSurfaceCode,
    ideaQuality: String,
    assessment: MoveMeaningSurfaceAssessment,
    failureFamily: Option[String],
    problem: Option[String],
    target: MoveMeaningSurfaceTarget,
    priority: String,
    comparisonLossSides: List[String] = Nil,
    comparisonLosses: List[String] = Nil,
    comparisonLostIdeas: List[MoveMeaningSurfaceComparisonLoss] = Nil,
    endgameTechnique: Option[MoveMeaningSurfaceEndgameTechnique] = None,
    comparison: Option[MoveMeaningSurfaceComparison] = None,
    terminalConsequences: List[MoveMeaningSurfaceCode] = Nil,
    structureContext: List[String] = Nil,
    evidence: MoveMeaningSurfaceEvidence = MoveMeaningSurfaceEvidence()
)

object MoveMeaningSurface:
  def verdict(frame: MoveJudgmentVerdictFrame): MoveMeaningSurfaceVerdict =
    MoveMeaningSurfaceVerdict(
      verdictCode = verdictCode(frame.verdict),
      moveQuality = moveQuality(frame.verdict),
      playedMove = frame.candidateLine.rootMove,
      referenceMove = frame.referenceLine.rootMove
    )

  def from(view: MoveJudgmentView): List[MoveMeaningSurface] =
    publicClaimsWithEvidenceForSurface(view)
      .map((claim, evidence) => fromClaim(view.verdict, claim, evidence))

  def publicPayloadJson(view: MoveJudgmentView): JsObject =
    val verdict = view.verdict.map(frame => MoveMeaningSurface.verdict(frame))
    val surfaces = MoveMeaningSurface.from(view)
    Json.obj(
      "verdict" -> verdict.map(publicVerdictJson),
      "idea_chains" -> verdict.toList.flatMap(publicIdeaChains(_, surfaces))
    )

  private def publicVerdictJson(verdict: MoveMeaningSurfaceVerdict): JsObject =
    Json.obj(
      "verdict_code" -> verdict.verdictCode,
      "move_quality" -> verdict.moveQuality,
      "played_move" -> verdict.playedMove,
      "reference_move" -> verdict.referenceMove
    )

  private def publicAssessmentJson(assessment: MoveMeaningSurfaceAssessment): JsObject =
    Json.obj(
      "move_quality" -> publicCodeJson(assessment.moveQuality),
      "idea_quality" -> publicCodeJson(assessment.ideaQuality),
      "priority" -> publicCodeJson(assessment.priority),
      "is_verdict_reason" -> assessment.verdictReason,
      "is_local_idea" -> assessment.localIdea,
      "failure_family" -> assessment.failureFamily.map(publicCodeJson),
      "problem" -> assessment.problem.map(publicCodeJson)
    )

  private def publicCodeJson(code: MoveMeaningSurfaceCode): JsObject =
    Json.obj(
      "code" -> code.code,
      "label" -> code.label
    )

  private def publicBoardCarrierJson(carrier: MoveMeaningSurfaceBoardCarrier, surface: MoveMeaningSurface): JsObject =
    Json.obj(
      "subject" -> surface.subject,
      "line_role" -> surface.lineRole,
      "move_uci" -> surface.moveUci,
      "role" -> carrier.role,
      "kind" -> carrier.kind,
      "value" -> carrier.value,
      "from" -> carrier.from,
      "to" -> carrier.to
    )

  private def publicIdeaChains(verdict: MoveMeaningSurfaceVerdict, surfaces: List[MoveMeaningSurface]): List[JsObject] =
    val allowedSubjects = publicIdeaChainSubjects(verdict, surfaces).toSet
    val problemMove = verdict.moveQuality == "bad" || verdict.verdictCode == "playable_loss"
    val subjectSpecs =
      if problemMove then List(("reference_move", verdict.referenceMove, "best_pv_"), ("played_move", verdict.playedMove, "played_pv_"))
      else List(("played_move", verdict.playedMove, "played_pv_"))
    subjectSpecs.flatMap { (subject, subjectMove, pvRolePrefix) =>
      val evidenceSurfaces = surfaces.filter(surface => surface.subject == subject && publicIdeaChainSurfaceHasCarrier(surface))
      val rootMoveRole = if subject == "reference_move" then "best_move" else "played_move"
      val pv = surfaces
        .filter(_.subject == subject)
        .flatMap(_.comparison.toList.flatMap(_.moves))
        .filter(move => move.uci.nonEmpty && (move.role == rootMoveRole || move.role.startsWith(pvRolePrefix)))
        .map(_.uci)
        .distinct
      val terminal = evidenceSurfaces.flatMap(_.terminalConsequences).distinct
      val technique = evidenceSurfaces.flatMap(_.endgameTechnique).distinct
      val strongChainProof =
        terminal.nonEmpty ||
          technique.nonEmpty ||
          evidenceSurfaces.exists(surface => surface.evidence.proofLevel == "terminal_proof" || surface.evidence.proofLevel == "owned_cause")
      val chainSurfaces = evidenceSurfaces.filter(surface => publicIdeaChainSemanticAllowed(surface, strongChainProof))
      val carrierPairs = publicIdeaChainCarrierPairs(chainSurfaces)
      val allConsequenceCarriers = carrierPairs.filter((carrier, _) =>
        carrier.role == "target" && publicIdeaChainConsequenceCarrier(carrier)
      )
      if evidenceSurfaces.isEmpty || !allowedSubjects.contains(subject) then Nil
      else
        val publicSemantics = chainSurfaces
          .sortBy(publicIdeaChainSemanticSortKey)
          .distinctBy(surface =>
            (surface.moveUci, surface.subject, surface.lineRole, surface.idea.code, surface.evidence.proofLevel)
          )
        val semanticTargetCarriers =
          publicSemantics.flatMap(surface =>
            surface.evidence.boardCarriers
              .filter(carrier => carrier.role == "target" && publicIdeaChainConsequenceCarrier(carrier))
              .sortBy(publicIdeaChainConsequenceCarrierSortKey)
              .take(1)
              .map(carrier => carrier -> surface)
          )
        val consequenceCarriers =
          (semanticTargetCarriers ++ allConsequenceCarriers)
            .sortBy((carrier, _) => publicIdeaChainConsequenceCarrierSortKey(carrier))
            .distinctBy((carrier, surface) =>
              (surface.subject, surface.lineRole, surface.moveUci, carrier.role, carrier.kind, carrier.value, carrier.from, carrier.to)
            )
            .take(6)
        val subjectFrom = Option.when(subjectMove.length >= 4)(subjectMove.take(2))
        val semanticTargetPieces =
          publicSemantics.flatMap(_.target.pieces).map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
        val actorPieceHint =
          if subjectMove.length > 4 then Some("pawn")
          else semanticTargetPieces match
            case piece :: Nil => Some(piece)
            case _            => None
        val currentMoveCarriers = carrierPairs.filter((carrier, _) =>
            carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove
          ).take(1)
        val currentMoveActorCarrierCandidates = carrierPairs.filter((carrier, surface) =>
          carrier.role == "actor" &&
            (carrier.kind == "Piece" ||
              subjectFrom.exists(from => carrier.kind == "Square" && carrier.value == from)) &&
            surface.evidence.boardCarriers.exists(carrier =>
              carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove
            ) &&
            subjectFrom.exists(from =>
              surface.evidence.boardCarriers.exists(carrier =>
                carrier.role == "actor" && carrier.kind == "Square" && carrier.value == from
              )
            )
        )
        val actorPieceValues =
          currentMoveActorCarrierCandidates
            .collect { case (carrier, _) if carrier.kind == "Piece" => carrier.value.trim.toLowerCase }
            .distinct
        val currentMoveActorCarriers = currentMoveCarriers ++ currentMoveActorCarrierCandidates.filter((carrier, _) =>
          carrier.kind != "Piece" ||
            actorPieceValues.size <= 1 ||
            actorPieceHint.contains(carrier.value.trim.toLowerCase)
        ).take(2)
        val carriers = currentMoveActorCarriers ++ consequenceCarriers
        List(
          Json.obj(
            "key" -> "current-move-chain",
            "current_move" -> verdict.playedMove,
            "reference_move" -> verdict.referenceMove,
            "move_quality" -> verdict.moveQuality,
            "subject" -> subject,
            "move_semantics" -> publicSemantics.map(publicIdeaChainMoveSemanticJson),
            "proof_levels" -> chainSurfaces.map(_.evidence.proofLevel).distinct,
            "carriers" -> carriers.map((carrier, surface) => publicBoardCarrierJson(carrier, surface)),
            "pv" -> pv,
            "consequence_carriers" -> consequenceCarriers.map((carrier, surface) => publicBoardCarrierJson(carrier, surface)),
            "terminal_consequences" -> terminal.map(publicCodeJson),
            "technique" -> technique.map(publicEndgameTechniqueJson),
            "player_facing_reason_allowed" -> true
          )
        )
    }

  private def publicIdeaChainSubjects(verdict: MoveMeaningSurfaceVerdict, surfaces: List[MoveMeaningSurface]): List[String] =
    val problemMove = verdict.moveQuality == "bad" || verdict.verdictCode == "playable_loss"
    val subjectSpecs =
      if problemMove then List(("reference_move", verdict.referenceMove, "best_pv_"), ("played_move", verdict.playedMove, "played_pv_"))
      else List(("played_move", verdict.playedMove, "played_pv_"))
    subjectSpecs.collect {
      case (subject, subjectMove, pvRolePrefix) if publicIdeaChainAllowed(subject, subjectMove, pvRolePrefix, surfaces) =>
        subject
    }

  private def publicIdeaChainAllowed(
      subject: String,
      subjectMove: String,
      pvRolePrefix: String,
      surfaces: List[MoveMeaningSurface]
  ): Boolean =
    val evidenceSurfaces = surfaces.filter(surface => surface.subject == subject && publicIdeaChainSurfaceHasCarrier(surface))
    val rootMoveRole = if subject == "reference_move" then "best_move" else "played_move"
    val pv = surfaces
      .filter(_.subject == subject)
      .flatMap(_.comparison.toList.flatMap(_.moves))
      .filter(move => move.uci.nonEmpty && (move.role == rootMoveRole || move.role.startsWith(pvRolePrefix)))
      .map(_.uci)
      .distinct
    val terminal = evidenceSurfaces.flatMap(_.terminalConsequences).distinct
    val technique = evidenceSurfaces.flatMap(_.endgameTechnique).distinct
    val carrierPairs = publicIdeaChainCarrierPairs(evidenceSurfaces)
    val allConsequenceCarriers = carrierPairs.filter((carrier, _) =>
      carrier.role == "target" && publicIdeaChainConsequenceCarrier(carrier)
    )
    val materialTacticalCarrier = allConsequenceCarriers.exists((carrier, _) => materialEventPlanSubjectCarrier(carrier))
    val strongProofSurface =
      terminal.nonEmpty ||
        technique.nonEmpty ||
        evidenceSurfaces.exists(surface => surface.evidence.proofLevel != "surface_evidence" || surface.evidence.causeIds.nonEmpty)
    val concreteConsequence = allConsequenceCarriers.exists((carrier, _) =>
      carrier.kind == "Pawn" ||
        (carrier.kind == "PlanSubject" && !materialEventPlanSubjectCarrier(carrier))
    )
    val ownedRouteCarrier = evidenceSurfaces.exists(surface =>
      surface.evidence.proofLevel == "owned_cause" &&
        surface.evidence.boardCarriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove)
    )
    val normalizedSubjectMove = JudgmentSubjectBinding.normalizeMove(subjectMove).toLowerCase
    val directPieceRouteCarrier = subject == "played_move" && evidenceSurfaces.exists { surface =>
      surface.idea.code == "piece_route" &&
        surface.evidence.proofLevel == "surface_evidence" &&
        (
          surface.evidence.sourceIds.exists(_.contains("structural-delta"))
        ) &&
        surface.evidence.boardCarriers.exists(carrier =>
          carrier.role == "actor" &&
            carrier.kind == "Move" &&
            JudgmentSubjectBinding.normalizeMove(carrier.value).toLowerCase == normalizedSubjectMove
        ) &&
        surface.evidence.boardCarriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Piece") &&
        surface.evidence.boardCarriers.exists(carrier => carrier.role == "target" && carrier.kind == "Square")
    }
    val directStructuralCarrier = evidenceSurfaces.exists { surface =>
      val carriers = surface.evidence.boardCarriers
      surface.evidence.sourceIds.exists(_.contains("structural-delta")) &&
        carriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove) &&
        carriers.exists(carrier =>
          carrier.role == "target" &&
            (carrier.kind == "Pawn" || (carrier.kind == "PlanSubject" && carrier.value.startsWith("passed-pawn")))
        )
    }
    evidenceSurfaces.nonEmpty &&
      (!materialTacticalCarrier || strongProofSurface) &&
      (terminal.nonEmpty || technique.nonEmpty || directStructuralCarrier || ownedRouteCarrier || directPieceRouteCarrier || (pv.nonEmpty && concreteConsequence))

  private def publicIdeaChainSurfaceHasCarrier(surface: MoveMeaningSurface): Boolean =
    surface.evidence.proofLevel != "none" &&
      surface.evidence.hasCarrier &&
      (surface.evidence.boardCarriers.nonEmpty || surface.terminalConsequences.nonEmpty || surface.endgameTechnique.nonEmpty) &&
      !surfaceOnlyStrategicMaterialEvent(surface)

  private def publicIdeaChainSemanticAllowed(surface: MoveMeaningSurface, strongChainProof: Boolean): Boolean =
    !strongChainProof ||
      surface.evidence.proofLevel != "surface_evidence" ||
      surface.evidence.causeIds.nonEmpty ||
      surface.terminalConsequences.nonEmpty ||
      surface.endgameTechnique.nonEmpty

  private def publicIdeaChainSemanticSortKey(surface: MoveMeaningSurface): (Int, Int, Int, Int, String, String) =
    (
      terminalIdeaRank(surface.idea.code),
      publicIdeaChainProofRank(surface.evidence.proofLevel),
      publicIdeaChainIdeaRank(surface),
      surface.priority match
        case "main"       => 0
        case "supporting" => 1
        case "comparison" => 2
        case "context"    => 3
        case _            => 4,
      surface.idea.code,
      surface.moveUci
    )

  private def publicIdeaChainProofRank(level: String): Int =
    level match
      case "terminal_proof" => 0
      case "owned_cause"    => 1
      case "cause_linked"   => 2
      case "technique"      => 3
      case _                => 4

  private def publicIdeaChainIdeaRank(surface: MoveMeaningSurface): Int =
    surface.idea.code match
      case "terminal_mate" | "promotion_race" | "promotion" | "draw_resource" | "material_gain" | "material_loss" =>
        0
      case "defensive_resource"    => 1
      case "passed_pawn_advance"   => 2
      case "pawn_break_timing"     => 3
      case "counterplay_race"      => 4
      case "counterplay_control" if counterplayControlConcreteConsequence(surface) =>
        5
      case "outpost_attempt"       => 5
      case "compensation"          => 6
      case "long_diagonal_pressure" => 7
      case "target_pressure"
          if surface.target.files.isEmpty &&
            surface.target.squares.nonEmpty &&
            moveDestination(surface.moveUci).exists(to => surface.target.squares.forall(_.equalsIgnoreCase(to))) =>
        10
      case "target_pressure" if surface.idea.label == "target pressure" || surface.idea.label == "central pressure" =>
        10
      case "target_pressure"        => 8
      case "piece_route"            => 9
      case "piece_activity"         => 10
      case "plan_continuity"        => 11
      case _                        => 12

  private def counterplayControlConcreteConsequence(surface: MoveMeaningSurface): Boolean =
    surface.evidence.proofLevel == "owned_cause" &&
      surface.evidence.boardCarriers.exists(carrier =>
        carrier.role == "target" &&
          carrier.kind == "PlanSubject" &&
          (
            carrier.value.startsWith("material-capture:") ||
              carrier.value.startsWith("material-recapture:") ||
              carrier.value.startsWith("passed-pawn:")
          )
      )

  private def moveDestination(move: String): Option[String] =
    val normalized = JudgmentSubjectBinding.normalizeMove(move).toLowerCase
    Option.when(normalized.matches("[a-h][1-8][a-h][1-8].*"))(normalized.slice(2, 4))

  private def surfaceOnlyStrategicMaterialEvent(surface: MoveMeaningSurface): Boolean =
    surface.evidence.proofLevel == "surface_evidence" &&
      surface.evidence.causeIds.isEmpty &&
      surface.ideaType == "strategic" &&
      surface.evidence.boardCarriers.exists(carrier =>
        carrier.kind == "PlanSubject" && materialEventPlanSubjectCarrier(carrier)
      )

  private def materialEventPlanSubjectCarrier(carrier: MoveMeaningSurfaceBoardCarrier): Boolean =
    carrier.kind == "PlanSubject" &&
      (
        carrier.value.startsWith("material-capture:") ||
          carrier.value.startsWith("material-recapture:")
      )

  private def publicIdeaChainMoveSemanticJson(surface: MoveMeaningSurface): JsObject =
    Json.obj(
      "move_uci" -> surface.moveUci,
      "subject" -> surface.subject,
      "line_role" -> surface.lineRole,
      "move_quality" -> surface.moveQuality,
      "idea_type" -> surface.ideaType,
      "idea" -> publicCodeJson(surface.idea),
      "idea_quality" -> surface.ideaQuality,
      "assessment" -> publicAssessmentJson(surface.assessment),
      "failure_family" -> surface.failureFamily,
      "problem" -> surface.problem,
      "target" -> publicTargetJson(surface.target),
      "priority" -> surface.priority,
      "comparison_loss" -> surface.comparisonLostIdeas.map(publicComparisonLossJson),
      "terminal_consequences" -> surface.terminalConsequences.map(publicCodeJson),
      "endgame_technique" -> surface.endgameTechnique.map(publicEndgameTechniqueJson),
      "structure_context" -> surface.structureContext,
      "evidence" -> Json.obj(
        "has_carrier" -> surface.evidence.hasCarrier,
        "proof_level" -> surface.evidence.proofLevel,
        "target_bound" -> surface.evidence.targetBound,
        "cause_ids" -> surface.evidence.causeIds,
        "source_ids" -> surface.evidence.sourceIds
      )
    )

  private def publicIdeaChainConsequenceCarrier(carrier: MoveMeaningSurfaceBoardCarrier): Boolean =
    carrier.kind match
      case "PlanSubject" => !carrier.value.contains(",")
      case "Pawn" | "Square" | "File" => true
      case _                           => false

  private def publicIdeaChainConsequenceCarrierSortKey(carrier: MoveMeaningSurfaceBoardCarrier): (Int, String, String) =
    val value = carrier.value.toLowerCase
    val rank =
      if carrier.kind == "PlanSubject" &&
          (
            value.startsWith("material-capture:") ||
              value.startsWith("material-recapture:") ||
              value.startsWith("defender-move:") ||
              value.startsWith("passed-pawn:")
          )
      then 0
      else if carrier.kind == "Pawn" then 1
      else if carrier.kind == "Square" || carrier.kind == "File" then 2
      else if carrier.kind == "PlanSubject" then 3
      else 4
    (rank, carrier.kind, carrier.value)

  private def publicIdeaChainCarrierPairs(
      surfaces: List[MoveMeaningSurface]
  ): List[(MoveMeaningSurfaceBoardCarrier, MoveMeaningSurface)] =
    surfaces
      .flatMap(surface => surface.evidence.boardCarriers.map(carrier => carrier -> surface))
      .distinctBy((carrier, surface) => (surface.subject, surface.lineRole, surface.moveUci, carrier.role, carrier.kind, carrier.value, carrier.from, carrier.to))

  private def publicEndgameTechniqueJson(technique: MoveMeaningSurfaceEndgameTechnique): JsObject =
    Json.obj(
      "pattern" -> technique.pattern,
      "pattern_label" -> technique.patternLabel,
      "pattern_info" -> technique.pattern.zip(technique.patternLabel).headOption.map { case (code, label) =>
        publicCodeJson(MoveMeaningSurfaceCode(code, label))
      },
      "rook_pattern" -> technique.rookPattern,
      "rook_pattern_label" -> technique.rookPatternLabel,
      "rook_geometry" -> technique.rookPattern.zip(technique.rookPatternLabel).headOption.map { case (code, label) =>
        publicCodeJson(MoveMeaningSurfaceCode(code, label))
      },
      "side" -> technique.side,
      "status" -> technique.horizonStatus,
      "status_label" -> technique.statusLabel,
      "trigger_move" -> technique.triggerMove,
      "status_move" -> technique.triggerMove.map(move => Json.obj("uci" -> move)),
      "entry_ply_offset" -> technique.entryPlyOffset,
      "terminal_ply_offset" -> technique.terminalPlyOffset,
      "required_squares" -> technique.requiredSquares,
      "maintained_squares" -> technique.maintainedSquares,
      "broken_squares" -> technique.brokenSquares,
      "squares" -> Json.obj(
        "required" -> technique.requiredSquares,
        "maintained" -> technique.maintainedSquares,
        "broken" -> technique.brokenSquares
      ),
      "terminal_consequences" -> technique.terminalConsequences.map(publicCodeJson),
      "failure_reason" -> technique.failureReason.map(publicCodeJson)
    )

  private def publicComparisonLossJson(loss: MoveMeaningSurfaceComparisonLoss): JsObject =
    Json.obj(
      "side" -> loss.side,
      "code" -> loss.code,
      "label" -> loss.label
    )

  private def publicTargetJson(target: MoveMeaningSurfaceTarget): JsObject =
    Json.obj(
      "squares" -> target.squares,
      "files" -> target.files,
      "pieces" -> target.pieces
    )

  private[chessjudgment] def publicClaimsWithEvidenceForSurface(
      view: MoveJudgmentView
  ): List[(MoveMeaningClaim, MoveMeaningSurfaceEvidence)] =
    val candidates = publicClaimEvidenceCandidates(view)
    view.verdict match
      case None => candidates
      case Some(frame) =>
        val verdict = MoveMeaningSurface.verdict(frame)
        val surfaces = candidates.map((claim, evidence) => (claim, evidence, fromClaim(view.verdict, claim, evidence)))
        val allowedSubjects = publicIdeaChainSubjects(verdict, surfaces.map(_._3)).toSet
        surfaces.collect {
          case (claim, evidence, surface) if allowedSubjects.contains(surface.subject) => claim -> evidence
        }

  private def publicClaimEvidenceCandidates(
      view: MoveJudgmentView
  ): List[(MoveMeaningClaim, MoveMeaningSurfaceEvidence)] =
    view.moveMeaningClaims
      .flatMap { claim =>
        val evidence = evidenceForClaim(claim)
        val badPlayedMove =
          view.verdict.exists(frame => badVerdict(frame.verdict)) &&
            subject(claim) == "played_move"
        val strongProof =
          evidence.proofLevel == "owned_cause" || evidence.proofLevel == "terminal_proof"
        Option.when(
          evidence.hasCarrier &&
            (strongProof || (evidence.proofLevel != "none" && !badPlayedMove))
        )(claim -> evidence)
      }
      .sortBy((claim, _) => claimSurfaceSortKey(claim))
      .take(12)

  private[chessjudgment] def evidenceForClaim(claim: MoveMeaningClaim): MoveMeaningSurfaceEvidence =
    publicEvidence(claim)

  private def claimSurfaceSortKey(claim: MoveMeaningClaim): (Int, Int, Int, String, String) =
    val claimSubject = subject(claim)
    val idea = ideaType(claim)
    (
      terminalIdeaRank(idea),
      priority(claim, claimSubject) match
        case "main"       => 0
        case "supporting" => 1
        case "comparison" => 2
        case "context"    => 3
        case _            => 4,
      publicSpecificityRank(claim),
      idea,
      claim.moveUci
    )

  private def publicSpecificityRank(claim: MoveMeaningClaim): Int =
    val directTerminal =
      terminalIdeaType(claim).nonEmpty
    val directPawnBreak =
      claim.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
        claim.publicHasCarrier &&
        (claim.breakFiles.nonEmpty || claim.breakIdentityParts.nonEmpty)
    val directBreakPlan =
      MoveMeaningClaim.directBreakPlanClaim(claim)
    val directPassedPawnAdvance =
      concretePassedPawnAdvanceClaim(claim)
    val directCheckingPressure =
      claim.meaningKind == "TargetPressure" &&
        claim.publicHasCarrier &&
        !currentMoveLikelyPawnAdvance(claim) &&
        checkingPressureClaim(claim)
    val concreteRoute =
      claim.meaningKind == "PieceRoute" &&
        (claim.routeIdentityParts.nonEmpty || claim.structuralMotifTags.exists(tag => tag == "route" || tag == "reroute"))
    val concreteOutpostRoute =
      concreteRoute &&
        (claim.publicIdeaType.contains("outpost_attempt") || claim.structuralMotifTags.exists(_ == "outpost"))
    val concreteRace =
      claim.unit == PositionPlanTechniqueUnit.CounterplayRace &&
        claim.causeEvidenceIds.nonEmpty &&
        (claim.breakFiles.nonEmpty || claim.targetFiles.nonEmpty || claim.targetSquares.nonEmpty)
    val directStructure =
      claim.sourceEvidenceIds.exists(_.contains("structural-delta")) &&
        claim.structuralMotifTags.nonEmpty
    val compactTarget =
      (claim.targetSquares.size + claim.targetFiles.size + claim.targetPieces.size) <= 2
    if directTerminal then 0
    else if directPawnBreak then 0
    else if directBreakPlan then 0
    else if directPassedPawnAdvance then 0
    else if directCheckingPressure then 0
    else if concreteOutpostRoute then 0
    else if concreteRace then 1
    else if concreteRoute then 1
    else if directStructure then 2
    else if claim.publicTargetBound && compactTarget then 3
    else 4

  private def fromClaim(
      verdict: Option[MoveJudgmentVerdictFrame],
      claim: MoveMeaningClaim,
      evidence: MoveMeaningSurfaceEvidence
  ): MoveMeaningSurface =
    val claimSubject = subject(claim)
    val played = claimSubject == "played_move"
    val badPlayedMove = played && verdict.exists(frame => badVerdict(frame.verdict))
    val publicFailureClaim =
      badPlayedMove &&
        claim.surfaceLane == "current_move_owned" &&
        claim.supportLevel == "owned_cause_linked"
    val quality = if played then verdict.map(frame => moveQuality(frame.verdict)).getOrElse("unknown") else "not_applicable"
    val idea = ideaType(claim)
    val ideaLabel =
      (idea, claim.label.map(_.trim).getOrElse("")) match
        case ("target_pressure", "TargetFixation") => "target fixation"
        case ("target_pressure", "weak-pawn-target") => "weak pawn target"
        case ("target_pressure", "king-safety-pressure") => "king safety pressure"
        case ("target_pressure", _) if checkingPressureClaim(claim) => "checking pressure"
        case ("target_pressure", _) if claim.causeKinds.contains(RelativeCauseKind.PawnWeaknessTarget) => "weak pawn target"
        case ("target_pressure", _)
            if claim.causeKinds.contains(RelativeCauseKind.TargetPressureGain) &&
              claim.targetFiles.isEmpty &&
              claim.targetSquares.nonEmpty &&
              claim.targetSquares.forall(square => Set("d4", "d5", "e4", "e5")(square.toLowerCase)) =>
          "central pressure"
        case ("counterplay_control", label) if label.startsWith("defensive-counter-break-") => "counter-break control"
        case ("pawn_break_timing", label) if ownedTensionBreakClaim(claim) && label.contains("created-tension") => "creates pawn tension"
        case ("pawn_break_timing", label) if ownedTensionBreakClaim(claim) && label.contains("resolved-tension") => "resolves pawn tension"
        case ("pawn_break_timing", label) if ownedTensionBreakClaim(claim) && label.contains("release-") => "releases pawn tension"
        case ("pawn_break_timing", label) if breakPreparationPlanClaim(claim, label) => "break preparation"
        case ("long_diagonal_pressure", _) if lineUnlockClaim(claim) => "line unlock"
        case ("compensation", _) if materialSacrificeCompensationClaim(claim) => "sacrifice compensation"
        case ("piece_route", _) if routeLineUnlockClaim(claim) => "line unlock"
        case ("piece_route", label) if routeManeuverClaim(claim, label) => "piece maneuver"
        case ("piece_route", label) if routeDevelopmentLabel(label) => "piece development"
        case ("piece_activity", label) if routeDevelopmentLabel(label) => "piece activation"
        case _ => ideaLabels.getOrElse(idea, "")
    val qualityOfIdea = ideaQuality(claim, claimSubject, badPlayedMove)
    val surfacePriority = priority(claim, claimSubject)
    val publicFailureFamily = Option.when(publicFailureClaim)(failureFamily(claim)).flatten
    val publicProblem = Option.when(publicFailureClaim)(problem(claim)).flatten
    val comparisonLossDetails = comparisonLostIdeas(verdict, claim)
    val terminal = terminalConsequences(claim)
    val target = MoveMeaningSurfaceTarget.fromClaim(claim)
    val technique = endgameTechnique(claim)
    MoveMeaningSurface(
      moveUci = claim.moveUci,
      subject = claimSubject,
      lineRole = claim.lineRole,
      moveQuality = quality,
      ideaType = idea,
      idea = MoveMeaningSurfaceCode(idea, ideaLabel),
      ideaQuality = qualityOfIdea,
      assessment = MoveMeaningSurfaceAssessment(
        moveQuality = publicCode(quality, moveQualityLabels),
        ideaQuality = publicCode(qualityOfIdea, ideaQualityLabels),
        priority = publicCode(surfacePriority, priorityLabels),
        verdictReason = publicFailureClaim,
        localIdea =
          claimSubject == "played_move" &&
            claim.surfaceLane == "current_move_function" &&
            !badPlayedMove &&
            !MoveMeaningClaim.negativeCurrentMoveMeaning(claim),
        failureFamily = publicFailureFamily.map(publicCode(_, failureFamilyLabels)),
        problem = publicProblem.map(publicCode(_, problemLabels))
      ),
      failureFamily = publicFailureFamily,
      problem = publicProblem,
      target = target,
      priority = surfacePriority,
      comparisonLossSides = comparisonLossSides(claim),
      comparisonLosses = comparisonLosses(claim),
      comparisonLostIdeas = comparisonLossDetails,
      endgameTechnique = technique,
      comparison =
        Option
          .when(claimSubject == "played_move" || claimSubject == "reference_move")(
            comparison(verdict, comparisonLossDetails, claim)
          )
          .flatten,
      terminalConsequences = terminal,
      structureContext = claim.structuralMotifTags,
      evidence = evidence
    )

  private def publicEvidence(claim: MoveMeaningClaim): MoveMeaningSurfaceEvidence =
    MoveMeaningSurfaceEvidence(
      hasCarrier = claim.publicHasCarrier,
      proofLevel = claim.publicProofLevel,
      targetBound = claim.publicTargetBound,
      causeIds = claim.causeEvidenceIds.take(6),
      sourceIds = claim.sourceEvidenceIds.take(6),
      boardCarriers = claim.boardCarriers
    )

  private def subject(claim: MoveMeaningClaim): String =
    claim.surfaceLane match
      case "current_move_owned" | "current_move_function" => "played_move"
      case "reference_or_opponent_resource" =>
        if claim.lineRole == "reference" then "reference_move" else "opponent_resource"
      case "inherited_context" => "background"
      case _                   => "line_variation"

  private def verdictCode(verdict: MoveChoiceVerdict): String =
    verdict match
      case MoveChoiceVerdict.ImprovesOnReference => "improves_on_reference"
      case MoveChoiceVerdict.MatchesReference    => "matches_reference"
      case MoveChoiceVerdict.PlayableLoss        => "playable_loss"
      case MoveChoiceVerdict.Inaccuracy          => "inaccuracy"
      case MoveChoiceVerdict.Mistake             => "mistake"
      case MoveChoiceVerdict.Blunder             => "blunder"

  private def moveQuality(verdict: MoveChoiceVerdict): String =
    verdict match
      case MoveChoiceVerdict.ImprovesOnReference | MoveChoiceVerdict.MatchesReference => "good"
      case MoveChoiceVerdict.PlayableLoss                                             => "playable"
      case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder =>
        "bad"

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict == MoveChoiceVerdict.Inaccuracy ||
      verdict == MoveChoiceVerdict.Mistake ||
      verdict == MoveChoiceVerdict.Blunder

  private def ideaType(claim: MoveMeaningClaim): String =
    terminalIdeaType(claim)
      .orElse(Option.when(concretePassedPawnAdvanceClaim(claim))("passed_pawn_advance"))
      .orElse(Option.when(claim.causeKinds.contains(RelativeCauseKind.DefensiveResource))("defensive_resource"))
      .orElse(
        Option.when(
          (claim.causeKinds.contains(RelativeCauseKind.RecaptureRecoveryWindow) ||
            claim.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe) &&
            claim.boardCarriers.exists(carrier =>
              carrier.role == "target" &&
                carrier.kind == "PlanSubject" &&
                carrier.value.startsWith("defender-move:")
            )
        )("defensive_resource")
      )
      .orElse(Option.when(ownedDefenderMoveResourceClaim(claim))("defensive_resource"))
      .orElse(Option.when(claim.unit == PositionPlanTechniqueUnit.CounterplayRace)("counterplay_race"))
      .orElse(Option.when(claim.meaningKind == "PieceActivity")("piece_activity"))
      .orElse(Option.when(claim.unit == PositionPlanTechniqueUnit.PieceRerouteRoute && longDiagonalPressureClaim(claim))("long_diagonal_pressure"))
      .orElse(
        Option.when(
          claim.unit != PositionPlanTechniqueUnit.SpacePreventionResourceDenial &&
            claim.unit != PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
            claim.unit != PositionPlanTechniqueUnit.CounterplayRace &&
            !MoveMeaningClaim.directBreakPlanClaim(claim) &&
            materialSacrificeCompensationClaim(claim) &&
            claim.causeKinds.exists(kind =>
              kind == RelativeCauseKind.ActivityGain ||
                kind == RelativeCauseKind.TargetPressureGain ||
                kind == RelativeCauseKind.PawnWeaknessTarget ||
                kind == RelativeCauseKind.PawnBreakOpportunity ||
                kind == RelativeCauseKind.PlanImprovement
            )
        )("compensation")
      )
      .getOrElse(claim.unit match
        case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
          "pawn_break_timing"
        case PositionPlanTechniqueUnit.CounterplayRace =>
          "counterplay_race"
        case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
          claim.publicIdeaType.orElse(Option.when(claim.meaningKind == "CenterControl")("center_control")).getOrElse("counterplay_control")
        case PositionPlanTechniqueUnit.PieceRerouteRoute if claim.meaningKind == "PieceRoute" =>
          claim.publicIdeaType.orElse(Option.when(checkingRouteTargetPressureClaim(claim))("target_pressure")).getOrElse("piece_route")
        case PositionPlanTechniqueUnit.PieceRerouteRoute =>
          "piece_activity"
        case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
          "endgame_technique"
        case PositionPlanTechniqueUnit.CompensationSource =>
          "compensation"
        case PositionPlanTechniqueUnit.StructuralTransformation =>
          axisIdeaType(claim).getOrElse("structure_shift")
        case PositionPlanTechniqueUnit.PlanOptionSet =>
          planOptionIdeaType(claim)
      )

  private def terminalIdeaType(claim: MoveMeaningClaim): Option[String] =
    val codes = terminalConsequenceCodes(claim).toSet
    if codes.contains("mate") then Some("terminal_mate")
    else if codes.contains("promotion_race") then Some("promotion_race")
    else if codes.contains("promotion") then Some("promotion")
    else if codes.contains("draw_resource") then Some("draw_resource")
    else if codes.contains("material_gain") then Some("material_gain")
    else if codes.contains("material_loss") then Some("material_loss")
    else None

  private def ownedDefenderMoveResourceClaim(claim: MoveMeaningClaim): Boolean =
    claim.supportLevel == "owned_cause_linked" &&
      claim.surfaceLane == "current_move_owned" &&
      claim.causeEvidenceIds.nonEmpty &&
      claim.publicHasCarrier &&
      claim.boardCarriers.exists(carrier =>
        carrier.role == "target" &&
          carrier.kind == "PlanSubject" &&
          carrier.value.startsWith("defender-move:")
      ) &&
      claim.boardCarriers.exists(carrier =>
        carrier.role == "target" &&
          carrier.kind == "PlanSubject" &&
          carrier.value.startsWith("check:")
      )

  private def checkingRouteTargetPressureClaim(claim: MoveMeaningClaim): Boolean =
    claim.boardCarriers.exists(carrier =>
      carrier.role == "target" &&
        carrier.kind == "PlanSubject" &&
        carrier.value.startsWith("check:")
    ) ||
      claim.objectBindingSignatures.exists(signature =>
        val normalized = signature.toLowerCase
        normalized.contains("mechanism=mechanism:check") ||
          normalized.contains("consequence=consequence:check")
      )

  private def checkingPressureClaim(claim: MoveMeaningClaim): Boolean =
    claim.boardCarriers.exists(carrier =>
      carrier.role == "target" &&
        carrier.kind == "PlanSubject" &&
        carrier.value.startsWith("check:")
    ) ||
      claim.objectBindingSignatures.exists(signature =>
        val normalized = signature.toLowerCase
        normalized.contains("mechanism=mechanism:check") ||
          normalized.contains("consequence=consequence:check")
      )

  private def longDiagonalPressureClaim(claim: MoveMeaningClaim): Boolean =
    lineUnlockClaim(claim) ||
      claim.objectBindingSignatures.exists(signature =>
        val normalized = signature.toLowerCase
        normalized.contains("actor=piece:bishop") &&
          (
            normalized.contains("mechanism=mechanism:bishop-long-diagonal") ||
              normalized.contains("consequence=consequence:diagonalpressure")
          )
      )

  private def lineUnlockClaim(claim: MoveMeaningClaim): Boolean =
    claim.routeIdentityParts.exists(_.equalsIgnoreCase("piece:bishop")) &&
      routeLineUnlockClaim(claim)

  private def routeLineUnlockClaim(claim: MoveMeaningClaim): Boolean =
    claim.routeIdentityParts.exists(part => part.toLowerCase.contains(":line-unlock:by:"))

  private def routeManeuverClaim(claim: MoveMeaningClaim, label: String): Boolean =
    val normalizedLabel = label.toLowerCase
    normalizedLabel == "activity-loss" ||
      normalizedLabel == "mobility-loss" ||
      (normalizedLabel.isEmpty && claim.routeIdentityParts.exists(part => part.toLowerCase.contains(":maneuver")))

  private def routeDevelopmentLabel(label: String): Boolean =
    val normalized = label.toLowerCase
    normalized == "activity-gain" || normalized == "mobility-gain" || normalized == "activity"

  private def ownedTensionBreakClaim(claim: MoveMeaningClaim): Boolean =
    claim.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
      claim.publicProofLevel == "owned_cause"

  private def breakPreparationPlanClaim(claim: MoveMeaningClaim, label: String): Boolean =
    claim.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
      (
        label.contains("PawnBreakPreparation") ||
          (
            claim.role == "PreparesBreakOption" &&
              (label.contains("OpeningDevelopment") || label.contains("PieceActivation")) &&
              (claim.breakFiles.nonEmpty || claim.breakIdentityParts.exists(_.startsWith("breakFile:")))
          )
      )

  private def materialSacrificeCompensationClaim(claim: MoveMeaningClaim): Boolean =
    claim.boardCarriers.exists(carrier =>
      carrier.role == "target" &&
        carrier.kind == "PlanSubject" &&
        carrier.value.startsWith("material-sacrifice:")
    )

  private def planOptionIdeaType(claim: MoveMeaningClaim): String =
    if passedPawnAdvanceClaim(claim) then "passed_pawn_advance"
    else if MoveMeaningClaim.directBreakPlanClaim(claim) then "pawn_break_timing"
    else if planContinuityTargetPressureCarrier(claim) then "target_pressure"
    else claim.role match
      case "DevelopsPieceForPlan" => "piece_activity"
      case _                      => "plan_continuity"

  private def planContinuityTargetPressureCarrier(claim: MoveMeaningClaim): Boolean =
    claim.meaningKind == "PlanContinuity" &&
      claim.sourceEvidenceIds.exists(_.contains(":strategic-mechanism:target-pressure:")) &&
      (claim.targetSquares.nonEmpty || claim.targetFiles.nonEmpty || claim.targetPieces.nonEmpty) &&
      claim.objectCarrierReady

  private def passedPawnAdvanceClaim(claim: MoveMeaningClaim): Boolean =
    currentMoveActorPiece(claim, "pawn") &&
      (
        claim.routeIdentityParts.exists(_.startsWith("subject:passed-pawn-advanced:")) ||
          claim.boardCarriers.exists(carrier =>
            carrier.kind == "PlanSubject" &&
              (carrier.value.startsWith("passed-pawn:") || carrier.value.startsWith("passed-pawn-advanced:"))
          )
      )

  private def concretePassedPawnAdvanceClaim(claim: MoveMeaningClaim): Boolean =
    claim.publicHasCarrier &&
      claim.unit != PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
      claim.unit != PositionPlanTechniqueUnit.CounterplayRace &&
      passedPawnAdvanceClaim(claim)

  private def currentMoveLikelyPawnAdvance(claim: MoveMeaningClaim): Boolean =
    val actorPieces =
      claim.boardCarriers.collect {
        case carrier if carrier.role == "actor" && carrier.kind == "Piece" => carrier.value.toLowerCase
      }
    val sameFileMove =
      claim.moveUci.length >= 4 && claim.moveUci.take(1) == claim.moveUci.slice(2, 3)
    actorPieces.contains("pawn") ||
      claim.routeIdentityParts.exists(_.equalsIgnoreCase("piece:pawn")) ||
      (actorPieces.isEmpty && sameFileMove)

  private def currentMoveActorPiece(claim: MoveMeaningClaim, piece: String): Boolean =
    (claim.moveUci.length > 4 && piece == "pawn") ||
      claim.boardCarriers.exists(carrier =>
        carrier.role == "actor" &&
          carrier.kind == "Piece" &&
          carrier.value.equalsIgnoreCase(piece)
      ) ||
      claim.routeIdentityParts.exists(_.equalsIgnoreCase(s"piece:$piece"))

  private def axisIdeaType(claim: MoveMeaningClaim): Option[String] =
    claim.axisKind.map {
      case StrategicAxisKind.Target        => "target_pressure"
      case StrategicAxisKind.SpaceCenter   => "center_control"
      case StrategicAxisKind.PawnBreak     => "pawn_break_timing"
      case StrategicAxisKind.Counterplay   => "counterplay_control"
      case StrategicAxisKind.Activity      => "piece_activity"
      case StrategicAxisKind.PlanCoherence => "plan_continuity"
    }

  private def ideaQuality(claim: MoveMeaningClaim, claimSubject: String, badPlayedMove: Boolean): String =
    if claimSubject == "background" || claim.supportLevel == "contextual" then "background"
    else if badPlayedMove && claim.surfaceLane == "current_move_owned" && claim.supportLevel == "owned_cause_linked" then "failed"
    else if badPlayedMove && claim.surfaceLane == "current_move_function" then "weak"
    else if claim.surfaceLane == "current_move_owned" then "real"
    else if claim.supportLevel == "owned_cause_linked" then "real"
    else if claim.surfaceLane == "current_move_function" then "supported"
    else "weak"

  private def priority(claim: MoveMeaningClaim, claimSubject: String): String =
    if claimSubject == "played_move" && terminalIdeaType(claim).nonEmpty then "main"
    else if claimSubject == "played_move" && claim.surfaceLane == "current_move_owned" then "main"
    else if claimSubject == "played_move" && claim.surfaceLane == "current_move_function" then "supporting"
    else if claimSubject == "reference_move" then "comparison"
    else if claimSubject == "opponent_resource" then "context"
    else if claimSubject == "background" then "context"
    else "variation"

  private def failureFamily(claim: MoveMeaningClaim): Option[String] =
    claim.causeKinds.flatMap(causeEventFamily).headOption
      .orElse(motifFailureFamily(claim))

  private def motifFailureFamily(claim: MoveMeaningClaim): Option[String] =
    if claim.causeKinds.contains(RelativeCauseKind.PawnBreakOpportunity) || claim.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute
    then Some("pawn_break_timing")
    else if claim.causeKinds.contains(RelativeCauseKind.OpponentRestriction) || claim.unit == PositionPlanTechniqueUnit.CounterplayRace
    then Some("counterplay")
    else if claim.causeKinds.exists(strategicCause) then Some("strategic")
    else if claim.causeKinds.contains(RelativeCauseKind.ActivityLoss) then Some("piece_activity")
    else if claim.causeKinds.contains(RelativeCauseKind.TargetPressureRelease) then Some("target_pressure")
    else if claim.causeKinds.contains(RelativeCauseKind.KingSafetyConcession) then Some("king_safety")
    else None

  private def causeEventFamily(kind: RelativeCauseKind): Option[String] =
    ClaimEventCluster.kindForCause(kind).map {
      case ClaimEventClusterKind.TacticalEvent   => "tactical"
      case ClaimEventClusterKind.DefensiveEvent  => "defensive_resource"
      case ClaimEventClusterKind.ConversionEvent => "conversion"
      case ClaimEventClusterKind.MaterialEvent   => "material"
    }

  private def strategicCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.StrategicConcession ||
      kind == RelativeCauseKind.MissedStrategicImprovement ||
      kind == RelativeCauseKind.PlanContradiction ||
      kind == RelativeCauseKind.PlanImprovement

  private def problem(claim: MoveMeaningClaim): Option[String] =
    if claim.causeKinds.contains(RelativeCauseKind.TacticalRefutationOfPlayed) ||
      claim.causeKinds.contains(RelativeCauseKind.CandidateTacticalLiability)
    then Some("tactical_flaw")
    else if claim.causeKinds.contains(RelativeCauseKind.MissedTacticalResource) then Some("missed_tactical_resource")
    else if claim.causeKinds.contains(RelativeCauseKind.WrongMoveOrder) then Some("wrong_move_order")
    else if claim.causeKinds.contains(RelativeCauseKind.WrongRecapturer) then Some("wrong_recapturer")
    else if claim.causeKinds.contains(RelativeCauseKind.OnlyDefenseNecessity) ||
      claim.causeKinds.contains(RelativeCauseKind.DefensiveResource)
    then Some("missed_defense")
    else if claim.causeKinds.contains(RelativeCauseKind.OnlyMoveNecessity) then Some("missed_only_move")
    else if claim.causeKinds.contains(RelativeCauseKind.TempoLoss) then Some("too_slow")
    else if claim.role == "AllowsOpponentCounterplayRace" then Some("opponent_counterplay_arrives_first")
    else if claim.unit == PositionPlanTechniqueUnit.CounterplayRace && claim.targetFiles.nonEmpty then Some("loses_race")
    else if claim.role == "ReleasesPawnTension" then Some("tension_released_early")
    else if claim.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute then Some("break_timing")
    else if claim.role == "ConcedesOutpost" then Some("outpost_conceded")
    else if claim.role == "ConcedesPieceRoute" || claim.role == "LosesPieceActivity" then Some("piece_activity_lost")
    else if claim.causeKinds.contains(RelativeCauseKind.PlanContradiction) then Some("plan_conflict")
    else if claim.causeKinds.contains(RelativeCauseKind.StrategicConcession) then Some("strategic_concession")
    else if claim.causeKinds.contains(RelativeCauseKind.TargetPressureRelease) then Some("pressure_released")
    else if claim.causeKinds.contains(RelativeCauseKind.KingSafetyConcession) then Some("king_safety")
    else if claim.causeKinds.contains(RelativeCauseKind.ConversionMiss) then Some("missed_conversion")
    else None

  private def comparisonLosses(claim: MoveMeaningClaim): List[String] =
    claim.comparisonLossKinds
      .flatMap(publicComparisonLossCode)
      .distinct
      .sorted

  private def comparisonLossSides(claim: MoveMeaningClaim): List[String] =
    claim.comparisonLossSides
      .filter(side => side == "candidate" || side == "reference")
      .distinct
      .sorted

  private def comparisonLostIdeas(
      verdict: Option[MoveJudgmentVerdictFrame],
      claim: MoveMeaningClaim
  ): List[MoveMeaningSurfaceComparisonLoss] =
    verdict.toList.flatMap { frame =>
      val sides = comparisonLossSides(claim)
      val losses = comparisonLosses(claim).map(publicCode(_, comparisonLossLabels))
      for
        side <- sides
        loss <- losses
      yield MoveMeaningSurfaceComparisonLoss(
        side = publicComparisonSideRole(frame.comparisonKind, side),
        code = loss.code,
        label = loss.label
      )
    }.distinct.sortBy(loss => (loss.side, loss.code))

  private def endgameTechnique(claim: MoveMeaningClaim): Option[MoveMeaningSurfaceEndgameTechnique] =
    if claim.unit != PositionPlanTechniqueUnit.EndgameTechniqueRecipe then None
    else
      val publicStatus = claim.endgameTechniqueHorizonStatus.flatMap(publicEndgameHorizonStatus)
      val pattern = claim.endgameTechniquePattern.flatMap(publicEndgamePatternCode)
      val rookPattern = claim.endgameTechniqueRookPattern.flatMap(publicRookPatternCode)
      val technique = MoveMeaningSurfaceEndgameTechnique(
        pattern = pattern.map(_.code),
        patternLabel = pattern.map(_.label),
        rookPattern = rookPattern.map(_.code),
        rookPatternLabel = rookPattern.map(_.label),
        side = claim.endgameTechniqueSide.flatMap(publicSideCode),
        horizonStatus = publicStatus,
        statusLabel = publicStatus.map(publicCode(_, endgameStatusLabels).label),
        triggerMove = claim.endgameTechniqueTriggerMove.flatMap(publicUciMove),
        entryPlyOffset = claim.endgameTechniqueEntryPlyOffset,
        terminalPlyOffset = claim.endgameTechniqueTerminalPlyOffset,
        requiredSquares = claim.requiredSquares.flatMap(publicSquare),
        maintainedSquares = claim.maintainedSquares.flatMap(publicSquare),
        brokenSquares = claim.brokenSquares.flatMap(publicSquare),
        terminalConsequences = claim.terminalConsequenceKinds.flatMap(publicTerminalConsequenceCode),
        failureReason = claim.endgameTechniqueFailureReason.flatMap(publicEndgameFailureCode)
      )
      Option.when(
        technique.horizonStatus.nonEmpty &&
          (
            technique.requiredSquares.nonEmpty ||
              technique.maintainedSquares.nonEmpty ||
              technique.brokenSquares.nonEmpty
          )
      )(technique)

  private def terminalConsequences(claim: MoveMeaningClaim): List[MoveMeaningSurfaceCode] =
    terminalConsequenceCodes(claim).map(publicCode(_, terminalConsequenceLabels))

  private def terminalConsequenceCodes(claim: MoveMeaningClaim): List[String] =
    claim.terminalConsequenceKinds.flatMap(publicTerminalConsequenceCode).map(_.code).distinct

  private def publicEndgameHorizonStatus(status: String): Option[String] =
    status match
      case "Active"                      => Some("holding")
      case "Transitioned"                => Some("converted")
      case "Failed"                      => Some("broken")
      case "SupersededByTactic"          => Some("forcing_result_first")
      case "ContradictedByTerminalProof" => Some("line_refutes_technique")
      case _                             => None

  private def publicEndgamePatternCode(pattern: String): Option[MoveMeaningSurfaceCode] =
    pattern match
      case "Lucena"                => Some(MoveMeaningSurfaceCode("lucena", "Lucena"))
      case "PhilidorDefense"      => Some(MoveMeaningSurfaceCode("philidor_defense", "Philidor defense"))
      case "VancuraDefense"       => Some(MoveMeaningSurfaceCode("vancura_defense", "Vancura defense"))
      case "ShortSideDefense"     => Some(MoveMeaningSurfaceCode("short_side_defense", "short-side defense"))
      case "TarraschDefenseActive" => Some(MoveMeaningSurfaceCode("active_tarrasch_defense", "active Tarrasch defense"))
      case "PassiveRookDefense"   => Some(MoveMeaningSurfaceCode("passive_rook_defense", "passive rook defense"))
      case _                       => None

  private def publicRookPatternCode(pattern: String): Option[MoveMeaningSurfaceCode] =
    pattern match
      case "RookBehindPassedPawn"  => Some(MoveMeaningSurfaceCode("rook_behind_passed_pawn", "rook behind passed pawn"))
      case "KingCutOff"            => Some(MoveMeaningSurfaceCode("king_cut_off", "king cut off"))
      case "TarraschDefenseActive" => Some(MoveMeaningSurfaceCode("active_tarrasch_defense", "active Tarrasch defense"))
      case "PassiveRookDefense"    => Some(MoveMeaningSurfaceCode("passive_rook_defense", "passive rook defense"))
      case _                       => None

  private def publicTerminalConsequenceCode(kind: String): Option[MoveMeaningSurfaceCode] =
    kind match
      case "Mate"          => Some(MoveMeaningSurfaceCode("mate", "mate"))
      case "MaterialGain"  => Some(MoveMeaningSurfaceCode("material_gain", "material gain"))
      case "MaterialLoss"  => Some(MoveMeaningSurfaceCode("material_loss", "material loss"))
      case "PromotionRace" => Some(MoveMeaningSurfaceCode("promotion_race", "promotion race"))
      case "DrawResource"  => Some(MoveMeaningSurfaceCode("draw_resource", "draw resource"))
      case _               => None

  private def publicEndgameFailureCode(reason: String): Option[MoveMeaningSurfaceCode] =
    reason match
      case "terminal-proof-supersedes-technique" =>
        Some(MoveMeaningSurfaceCode("forcing_result_first", "forcing result comes first"))
      case "terminal-proof-contradicts-technique" =>
        Some(MoveMeaningSurfaceCode("line_refutes_technique", "forcing line shows the technique fails"))
      case _ => None

  private def publicSideCode(side: String): Option[String] =
    Option.when(side == "white" || side == "black")(side)

  private def publicSquare(square: String): Option[String] =
    Option.when(square.matches("[a-h][1-8]"))(square)

  private[judgment] def publicUciMove(move: String): Option[String] =
    val normalized = JudgmentSubjectBinding.normalizeMove(move).toLowerCase
    Option.when(normalized.matches("[a-h][1-8][a-h][1-8][nbrq]?"))(normalized)

  private def comparison(
      verdict: Option[MoveJudgmentVerdictFrame],
      lostIdeas: List[MoveMeaningSurfaceComparisonLoss],
      claim: MoveMeaningClaim
  ): Option[MoveMeaningSurfaceComparison] =
    verdict.map { frame =>
      MoveMeaningSurfaceComparison(
        kind = comparisonKindCode(frame.comparisonKind),
        relation = comparisonRelation(frame.comparisonKind),
        referenceMove = frame.referenceLine.rootMove,
        candidateMove = frame.candidateLine.rootMove,
        secondMove = frame.candidateSet.flatMap(_.secondLine.map(_.rootMove)),
        moves = comparisonMoves(frame, claim),
        lostIdeas = lostIdeas
      )
    }

  private def comparisonMoves(frame: MoveJudgmentVerdictFrame, claim: MoveMeaningClaim): List[MoveMeaningSurfaceMoveRef] =
    val roleMoves =
      frame.comparisonKind match
        case CandidateComparisonKind.PlayedVsBest =>
          List(
            MoveMeaningSurfaceMoveRef("played_move", frame.candidateLine.rootMove),
            MoveMeaningSurfaceMoveRef("best_move", frame.referenceLine.rootMove)
          ) ++ frame.candidateSet.flatMap(_.secondLine.map(line => MoveMeaningSurfaceMoveRef("next_candidate_move", line.rootMove))).toList
        case CandidateComparisonKind.BestVsSecond =>
          List(
            MoveMeaningSurfaceMoveRef("best_move", frame.referenceLine.rootMove),
            MoveMeaningSurfaceMoveRef("next_candidate_move", frame.candidateLine.rootMove)
          )
        case CandidateComparisonKind.PlayedVsAlternative =>
          List(
            MoveMeaningSurfaceMoveRef("played_move", frame.candidateLine.rootMove),
            MoveMeaningSurfaceMoveRef("alternative_move", frame.referenceLine.rootMove)
          )
        case CandidateComparisonKind.ReferenceVsAlternative =>
          List(
            MoveMeaningSurfaceMoveRef("best_move", frame.referenceLine.rootMove),
            MoveMeaningSurfaceMoveRef("alternative_move", frame.candidateLine.rootMove)
          )
    (roleMoves ++ claim.comparisonMoveRefs).filter(_.uci.nonEmpty).distinct

  private def comparisonKindCode(kind: CandidateComparisonKind): String =
    kind match
      case CandidateComparisonKind.PlayedVsBest           => "played_vs_reference"
      case CandidateComparisonKind.BestVsSecond           => "reference_vs_next_candidate"
      case CandidateComparisonKind.PlayedVsAlternative    => "played_vs_alternative"
      case CandidateComparisonKind.ReferenceVsAlternative => "reference_vs_alternative"

  private def publicComparisonSideRole(kind: CandidateComparisonKind, side: String): String =
    (kind, side) match
      case (CandidateComparisonKind.PlayedVsBest, "candidate")           => "played_move"
      case (CandidateComparisonKind.PlayedVsBest, "reference")           => "best_move"
      case (CandidateComparisonKind.BestVsSecond, "candidate")           => "next_candidate_move"
      case (CandidateComparisonKind.BestVsSecond, "reference")           => "best_move"
      case (CandidateComparisonKind.PlayedVsAlternative, "candidate")    => "played_move"
      case (CandidateComparisonKind.PlayedVsAlternative, "reference")    => "alternative_move"
      case (CandidateComparisonKind.ReferenceVsAlternative, "candidate") => "alternative_move"
      case (CandidateComparisonKind.ReferenceVsAlternative, "reference") => "best_move"
      case (_, value)                                                    => value

  private[judgment] def comparisonRelation(kind: CandidateComparisonKind): String =
    kind match
      case CandidateComparisonKind.PlayedVsBest           => "best_vs_played"
      case CandidateComparisonKind.BestVsSecond           => "best_vs_second"
      case CandidateComparisonKind.PlayedVsAlternative    => "alternative_vs_played"
      case CandidateComparisonKind.ReferenceVsAlternative => "best_vs_alternative"

  private def publicComparisonLossCode(value: String): Option[String] =
    Option.when(comparisonLossLabels.contains(value))(value)

  private def publicCode(code: String, labels: Map[String, String]): MoveMeaningSurfaceCode =
    MoveMeaningSurfaceCode(code, labels.getOrElse(code, ""))

  private val ideaLabels: Map[String, String] = Map(
    "terminal_mate" -> "mate",
    "promotion_race" -> "promotion",
    "promotion" -> "promotion",
    "draw_resource" -> "draw resource",
    "defensive_resource" -> "defensive resource",
    "material_gain" -> "material gain",
    "material_loss" -> "material loss",
    "pawn_break_timing" -> "pawn break timing",
    "counterplay_race" -> "counterplay race",
    "ray_denial" -> "diagonal denial",
    "counterplay_control" -> "counterplay control",
    "outpost_attempt" -> "outpost attempt",
    "long_diagonal_pressure" -> "long diagonal pressure",
    "piece_route" -> "piece route",
    "piece_activity" -> "piece activity",
    "passed_pawn_advance" -> "passed pawn advance",
    "endgame_technique" -> "endgame technique",
    "compensation" -> "compensation",
    "structure_shift" -> "structure shift",
    "target_pressure" -> "target pressure",
    "center_control" -> "center control",
    "plan_continuity" -> "plan continuity"
  )

  private val terminalConsequenceLabels: Map[String, String] = Map(
    "mate" -> "mate",
    "material_gain" -> "material gain",
    "material_loss" -> "material loss",
    "promotion_race" -> "promotion race",
    "promotion" -> "promotion",
    "draw_resource" -> "draw resource"
  )

  private val moveQualityLabels: Map[String, String] = Map(
    "good" -> "good move",
    "playable" -> "playable move",
    "bad" -> "bad move",
    "unknown" -> "unknown move quality",
    "not_applicable" -> "not applicable"
  )

  private val ideaQualityLabels: Map[String, String] = Map(
    "real" -> "owned reason",
    "supported" -> "supporting idea",
    "weak" -> "weak local idea",
    "background" -> "background context",
    "failed" -> "failed idea"
  )

  private val priorityLabels: Map[String, String] = Map(
    "main" -> "main reason",
    "supporting" -> "supporting idea",
    "comparison" -> "comparison idea",
    "context" -> "context",
    "variation" -> "line variation"
  )

  private val failureFamilyLabels: Map[String, String] = Map(
    "pawn_break_timing" -> "pawn break timing",
    "counterplay" -> "counterplay",
    "strategic" -> "strategic concession",
    "piece_activity" -> "piece activity",
    "target_pressure" -> "target pressure",
    "king_safety" -> "king safety",
    "tactical" -> "tactical issue",
    "defensive_resource" -> "defensive resource",
    "conversion" -> "conversion",
    "material" -> "material"
  )

  private val problemLabels: Map[String, String] = Map(
    "tactical_flaw" -> "tactical flaw",
    "missed_tactical_resource" -> "missed tactical resource",
    "wrong_move_order" -> "wrong move order",
    "wrong_recapturer" -> "wrong recapturer",
    "missed_defense" -> "missed defense",
    "missed_only_move" -> "missed only move",
    "too_slow" -> "too slow",
    "opponent_counterplay_arrives_first" -> "opponent counterplay arrives first",
    "loses_race" -> "loses the race",
    "tension_released_early" -> "tension released too early",
    "break_timing" -> "break timing",
    "outpost_conceded" -> "outpost conceded",
    "piece_activity_lost" -> "piece activity lost",
    "plan_conflict" -> "plan conflict",
    "strategic_concession" -> "strategic concession",
    "pressure_released" -> "pressure released",
    "king_safety" -> "king safety",
    "missed_conversion" -> "missed conversion"
  )

  private val comparisonLossLabels: Map[String, String] = Map(
    "break_option_missed" -> "missed pawn break option",
    "counter_break_allowed" -> "allowed counter-break",
    "outpost_route_missed" -> "missed outpost route",
    "diagonal_pressure_lost" -> "lost diagonal pressure",
    "move_order_too_slow" -> "move order too slow",
    "tension_released_early" -> "released tension too early"
  )

  private val endgameStatusLabels: Map[String, String] = Map(
    "holding" -> "technique held",
    "converted" -> "technique reached",
    "broken" -> "technique broken",
    "forcing_result_first" -> "forcing result comes first",
    "line_refutes_technique" -> "forcing line shows the technique fails"
  )

  private def terminalIdeaRank(ideaType: String): Int =
    ideaType match
      case "terminal_mate"  => 0
      case "promotion_race" => 1
      case "promotion"      => 1
      case "draw_resource"  => 1
      case "material_gain"  => 2
      case "material_loss"  => 2
      case _                => 9

object MoveMeaningClaim:
  def from(
      evidenceGraph: TypedEvidenceGraph,
      view: MoveJudgmentView,
      causeFrames: List[MoveJudgmentCauseFrame]
  ): List[MoveMeaningClaim] =
    val claims =
      view.verdict.toList
        .flatMap(verdict =>
          val causeFramesById =
            causeFrames
              .flatMap(frame => frame.causeEvidenceIds.map(_ -> frame))
              .groupMap(_._1)(_._2)
          view.positionPlanTechniqueFrames
            .filter(frame => frameMatches(evidenceGraph, frame, verdict))
            .flatMap(frame =>
              frame.semanticDetails.flatMap(detail =>
                fromDetail(evidenceGraph, frame, detail, verdict, causeFramesById)
              )
            )
        )
        .groupBy(claim => (claim.laneKey, claim.role, claim.lineRole, claim.moveUci, provenanceKey(claim)))
        .values
        .flatMap(mergeMeaningClaims)
        .toList
        .groupBy(claim => duplicateMeaningKey(claim))
        .values
        .flatMap(mergeMeaningClaims)
        .toList
    val unshadowedClaims =
      suppressShadowedPlanContinuity(
        suppressShadowedRouteFunctionClaims(suppressShadowedCounterplayRacePawnBreak(claims))
      )
    withPublicSurfaceEligibility(view.verdict, unshadowedClaims)
      .sortBy(claim =>
        (claim.meaningKind, claim.role, claim.lineRole, claim.laneKey, claim.frameId)
      )

  private def withPublicSurfaceEligibility(
      verdict: Option[MoveJudgmentVerdictFrame],
      claims: List[MoveMeaningClaim]
  ): List[MoveMeaningClaim] =
    claims.map { claim =>
      if publicSurfaceCarrierAllowed(verdict, claims, claim) then claim
      else claim.copy(publicHasCarrier = false, publicProofLevel = "none", publicTargetBound = false)
    }

  private def publicSurfaceCarrierAllowed(
      verdict: Option[MoveJudgmentVerdictFrame],
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    publicSurfaceLaneAllowed(claim) &&
      !sameRootReferenceSurface(verdict, claim) &&
      !terminalOverriddenEndgameTechniqueShadowed(claims, claim) &&
      !routeActivityShadowedByOwnedRoute(claims, claim) &&
      !planContinuityShadowedByOwnedRoute(claims, claim) &&
      !planContinuityShadowedByConcreteCurrentClaim(claims, claim) &&
      !planContinuityBreakOptionShadowedByOwnedBreak(claims, claim) &&
      !genericActivityOrPlanShadowedByOwnedTargetPressure(claims, claim) &&
      publicSpecificPlanContinuityClaim(claims, claim) &&
      !badMoveSuppressesCurrentMoveSurface(verdict, claim)

  private def sameRootReferenceSurface(
      verdict: Option[MoveJudgmentVerdictFrame],
      claim: MoveMeaningClaim
  ): Boolean =
    claim.surfaceLane == "reference_or_opponent_resource" &&
      claim.lineRole == "reference" &&
      verdict.exists(frame =>
        sameMove(frame.candidateLine.rootMove, frame.referenceLine.rootMove) &&
          sameMove(claim.moveUci, frame.candidateLine.rootMove)
      )

  private def publicSurfaceLaneAllowed(claim: MoveMeaningClaim): Boolean =
    terminalOverriddenEndgameTechniqueClaim(claim) ||
      (
        claim.supportLevel != "contextual" &&
          claim.surfaceLane != "inherited_context" &&
          claim.surfaceLane != "pv_or_line_witness"
      )

  private def terminalOverriddenEndgameTechniqueClaim(claim: MoveMeaningClaim): Boolean =
    claim.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      claim.endgameTechniqueHorizonStatus.exists(status =>
        status == "SupersededByTactic" ||
          status == "ContradictedByTerminalProof"
      )

  private def terminalOverriddenEndgameTechniqueShadowed(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    val terminalKinds = terminalConsequenceKinds(claim)
    terminalOverriddenEndgameTechniqueClaim(claim) &&
      terminalKinds.nonEmpty &&
      claims.exists(other =>
        other != claim &&
          other.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
          other.supportLevel != "contextual" &&
          (other.surfaceLane == "current_move_owned" || other.surfaceLane == "current_move_function") &&
          other.moveUci == claim.moveUci &&
          terminalConsequenceKinds(other).intersect(terminalKinds).nonEmpty
      )

  private def terminalConsequenceKinds(claim: MoveMeaningClaim): Set[String] =
    claim.terminalConsequenceKinds.filter(LineConsequenceKind.terminalResultProofName).toSet

  private def publicSpecificPlanContinuityClaim(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    if claim.meaningKind != "PlanContinuity" then true
    else if claim.surfaceLane == "current_move_function" then
      currentMovePlanContinuityFunctionReady(claims, claim)
    else if claim.surfaceLane == "current_move_owned" &&
        claim.supportLevel == "owned_cause_linked" &&
        claim.causeKinds.contains(RelativeCauseKind.PlanImprovement) &&
        claim.causeEvidenceIds.nonEmpty &&
        claim.objectCarrierReady &&
        claim.boardCarriers.exists(carrier => carrier.role == "target" && carrier.kind == "PlanSubject")
    then true
    else
      (claim.specificityTier == PositionPlanTechniqueSpecificityTier.ExactObjectAxis ||
        claim.specificityTier == PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis) &&
        claim.objectCarrierReady

  private def badMoveSuppressesCurrentMoveSurface(
      verdict: Option[MoveJudgmentVerdictFrame],
      claim: MoveMeaningClaim
  ): Boolean =
    verdict.exists(frame =>
      badVerdict(frame.verdict) &&
        currentMoveClaim(frame, claim) &&
        (
          (claim.surfaceLane == "current_move_owned" && positiveCurrentMoveMotif(claim)) ||
            (claim.surfaceLane == "current_move_function" && negativeCurrentMoveMeaning(claim))
        )
    )

  private def positiveCurrentMoveMotif(claim: MoveMeaningClaim): Boolean =
    positiveMeaningRole(claim.role) || positiveCurrentMoveMeaning(claim)

  private def positiveCurrentMoveMeaning(claim: MoveMeaningClaim): Boolean =
    claim.role == "ExplainsMoveFunction" &&
      !negativeCurrentMoveMeaning(claim)

  private[judgment] def negativeCurrentMoveMeaning(claim: MoveMeaningClaim): Boolean =
    claim.axisPolarity.exists(negativePolarity) ||
      claim.role.startsWith("Concedes") ||
      claim.role.startsWith("Loses") ||
      claim.role.startsWith("Allows") ||
      claim.role.startsWith("Releases") ||
      claim.role.startsWith("Breaks") ||
      claim.causeKinds.exists(negativeCauseKind)

  private def currentMoveClaim(verdict: MoveJudgmentVerdictFrame, claim: MoveMeaningClaim): Boolean =
    (claim.surfaceLane == "current_move_owned" || claim.surfaceLane == "current_move_function") &&
      claim.lineRole == "candidate" &&
      JudgmentSubjectBinding.normalizeMove(claim.moveUci) ==
      JudgmentSubjectBinding.normalizeMove(verdict.candidateLine.rootMove)

  private def suppressShadowedRouteFunctionClaims(claims: List[MoveMeaningClaim]): List[MoveMeaningClaim] =
    val ownedRouteKeys =
      claims
        .filter(claim =>
          claim.meaningKind == "PieceRoute" &&
            claim.surfaceLane == "current_move_owned" &&
            claim.supportLevel == "owned_cause_linked"
        )
        .flatMap(routeIdentityKey)
        .toSet
    if ownedRouteKeys.isEmpty then claims
    else
      claims.filterNot(claim =>
        claim.meaningKind == "PieceRoute" &&
          claim.surfaceLane == "current_move_function" &&
          claim.causeEvidenceIds.isEmpty &&
          routeIdentityKey(claim).exists(ownedRouteKeys.contains)
      )

  private def routeIdentityKey(claim: MoveMeaningClaim): Option[(String, String, List[String])] =
    Option.when(claim.routeIdentityParts.nonEmpty)((claim.lineRole, claim.moveUci, claim.routeIdentityParts))

  private def routeActivityShadowedByOwnedRoute(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    claim.meaningKind == "PieceActivity" &&
      (
        claim.role == "ImprovesPieceActivity" ||
          claim.role == "LosesPieceActivity" && claim.causeKinds.nonEmpty && claim.causeKinds.forall(kind => !negativeCauseKind(kind))
      ) &&
      currentMoveSurfaceLane(claim) &&
      (
        claim.routeIdentityParts.nonEmpty && routeClaimWithSameIdentity(claims, claim) ||
          claim.causeEvidenceIds.nonEmpty &&
            claims.exists(other =>
              other != claim &&
                other.meaningKind == "PieceRoute" &&
                currentMoveSurfaceLane(other) &&
                other.supportLevel == "owned_cause_linked" &&
                other.publicHasCarrier &&
                other.moveUci == claim.moveUci &&
                other.lineRole == claim.lineRole &&
                other.causeEvidenceIds.intersect(claim.causeEvidenceIds).nonEmpty
            )
      )

  private def planContinuityShadowedByOwnedRoute(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    claim.meaningKind == "PlanContinuity" &&
      claim.role == "DevelopsPieceForPlan" &&
      claim.surfaceLane == "current_move_function" &&
      claim.causeEvidenceIds.isEmpty &&
      routeClaimWithSameIdentity(claims, claim)

  private def planContinuityBreakOptionShadowedByOwnedBreak(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    claim.meaningKind == "PlanContinuity" &&
      claim.role == "PreparesBreakOption" &&
      claim.surfaceLane == "current_move_function" &&
      claim.breakFiles.nonEmpty &&
      claims.exists(other =>
        other != claim &&
          (other.meaningKind == "PawnBreakTiming" || other.meaningKind == "CounterplayRace") &&
          other.supportLevel == "owned_cause_linked" &&
          other.publicHasCarrier &&
          other.moveUci == claim.moveUci &&
          other.lineRole == claim.lineRole &&
          currentMoveSurfaceLane(other) &&
          other.breakFiles.toSet.intersect(claim.breakFiles.toSet).nonEmpty
      )

  private def genericActivityOrPlanShadowedByOwnedTargetPressure(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    def concreteTargetPressurePlan(other: MoveMeaningClaim): Boolean =
      other.meaningKind == "PlanContinuity" &&
        other.sourceEvidenceIds.exists(_.contains(":strategic-mechanism:target-pressure:")) &&
        (other.targetSquares.nonEmpty || other.targetFiles.nonEmpty || other.targetPieces.nonEmpty) &&
        other.objectCarrierReady
    (
      claim.meaningKind == "PieceActivity" && claim.role == "ImprovesPieceActivity" ||
        claim.meaningKind == "PlanContinuity" && claim.role == "ReferencePreservesPlan" ||
        claim.meaningKind == "PlanContinuity" && claim.role == "PreparesBreakOption" && claim.breakFiles.nonEmpty &&
          claim.targetSquares.isEmpty && !directBreakPlanClaim(claim)
    ) &&
      currentMoveSurfaceLane(claim) &&
      claims.exists(other =>
        other != claim &&
          other.supportLevel == "owned_cause_linked" &&
          other.publicHasCarrier &&
          other.moveUci == claim.moveUci &&
          other.lineRole == claim.lineRole &&
          currentMoveSurfaceLane(other) &&
          (other.meaningKind == "TargetPressure" || concreteTargetPressurePlan(other)) &&
          (
            other.causeEvidenceIds.intersect(claim.causeEvidenceIds).nonEmpty ||
              targetOverlap(other, claim) ||
              (claim.role == "PreparesBreakOption" && concreteTargetPressurePlan(other))
          )
      )

  private def targetOverlap(left: MoveMeaningClaim, right: MoveMeaningClaim): Boolean =
    left.targetSquares.intersect(right.targetSquares).nonEmpty ||
      left.targetPieces.intersect(right.targetPieces).nonEmpty ||
      left.targetFiles.intersect(right.targetFiles).nonEmpty

  private def routeClaimWithSameIdentity(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    val exact = routeIdentityKey(claim).exists(key =>
      claims.exists(other =>
        other != claim &&
        other.meaningKind == "PieceRoute" &&
          currentMoveSurfaceLane(other) &&
          other.supportLevel != "contextual" &&
          other.publicHasCarrier &&
          routeIdentityKey(other).contains(key)
      )
    )
    exact ||
      routeCoreIdentity(claim).exists(key =>
        claims.exists(other =>
          other != claim &&
          other.meaningKind == "PieceRoute" &&
            currentMoveSurfaceLane(other) &&
            other.supportLevel != "contextual" &&
            other.publicHasCarrier &&
            routeCoreIdentity(other).contains(key)
        )
      )

  private def currentMoveSurfaceLane(claim: MoveMeaningClaim): Boolean =
    claim.surfaceLane == "current_move_owned" || claim.surfaceLane == "current_move_function"

  private def routeCoreIdentity(claim: MoveMeaningClaim): Option[(String, String, String, Option[String], Option[String], Option[String])] =
    val piece = routePart(claim, "piece:")
    val from = routePart(claim, "from:")
    val to = routePart(claim, "to:")
    val target = routePart(claim, "target:")
    piece.flatMap(p =>
      Option.when(from.nonEmpty || to.nonEmpty || target.nonEmpty)(
        (claim.lineRole, claim.moveUci, p, from, to, target)
      )
    )

  private def routePart(claim: MoveMeaningClaim, prefix: String): Option[String] =
    claim.routeIdentityParts.collectFirst { case part if part.startsWith(prefix) => part.stripPrefix(prefix) }

  private def suppressShadowedCounterplayRacePawnBreak(claims: List[MoveMeaningClaim]): List[MoveMeaningClaim] =
    val raceClaims =
      claims.filter(claim =>
          claim.meaningKind == "CounterplayRace" &&
          claim.unit == PositionPlanTechniqueUnit.CounterplayRace &&
          claim.supportLevel != "contextual" &&
          claim.breakFiles.nonEmpty
      )
    if raceClaims.isEmpty then claims
    else
      claims.filterNot(claim =>
        claim.meaningKind == "PawnBreakTiming" &&
          claim.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
          claim.supportLevel != "owned_cause_linked" &&
          claim.causeEvidenceIds.isEmpty &&
          raceClaims.exists(counterplayRaceShadowsPawnBreak(_, claim))
      )

  private def counterplayRaceShadowsPawnBreak(
      raceClaim: MoveMeaningClaim,
      pawnBreakClaim: MoveMeaningClaim
  ): Boolean =
    raceClaim.moveUci == pawnBreakClaim.moveUci &&
      raceClaim.lineRole == pawnBreakClaim.lineRole &&
      raceClaim.surfaceLane == pawnBreakClaim.surfaceLane &&
      strengthRank(raceClaim.supportLevel) >= strengthRank(pawnBreakClaim.supportLevel) &&
      raceClaim.breakFiles.toSet.intersect(pawnBreakClaim.breakFiles.toSet).nonEmpty &&
      (
        raceClaim.sourceEvidenceIds.intersect(pawnBreakClaim.sourceEvidenceIds).nonEmpty ||
          raceClaim.causeEvidenceIds.intersect(pawnBreakClaim.causeEvidenceIds).nonEmpty
      )

  private def suppressShadowedPlanContinuity(claims: List[MoveMeaningClaim]): List[MoveMeaningClaim] =
    val concreteCurrentClaims =
      claims
        .filter(claim =>
          claim.meaningKind != "PlanContinuity" &&
            (claim.surfaceLane == "current_move_owned" || claim.surfaceLane == "current_move_function")
        )
    claims.filterNot(claim =>
      claim.meaningKind == "PlanContinuity" &&
        claim.surfaceLane == "current_move_function" &&
        !currentMovePlanContinuityFunctionReady(claims, claim) &&
        concreteCurrentClaims.exists(concreteClaimShadowsPlanContinuity(claim, _))
    )

  private def concreteClaimShadowsPlanContinuity(
      planClaim: MoveMeaningClaim,
      concreteClaim: MoveMeaningClaim
  ): Boolean =
    planClaim.moveUci == concreteClaim.moveUci &&
      (
        planClaim.role match
          case "PreparesBreakOption" =>
            if directBreakPlanClaim(planClaim) then concreteClaim.meaningKind == "PawnBreakTiming"
            else concreteClaim.meaningKind != "PlanContinuity"
          case "DevelopsPieceForPlan" =>
            concreteClaim.meaningKind == "PieceRoute" ||
              concreteClaim.meaningKind == "PieceActivity"
          case _ =>
            true
      )

  private def planContinuityShadowedByConcreteCurrentClaim(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    claim.meaningKind == "PlanContinuity" &&
      claim.surfaceLane == "current_move_owned" &&
      !directBreakPlanClaim(claim) &&
      claims.exists(other =>
        other != claim &&
          other.meaningKind != "PlanContinuity" &&
          other.unit != PositionPlanTechniqueUnit.PlanOptionSet &&
          currentMoveSurfaceLane(other) &&
          other.publicHasCarrier &&
          other.sourceEvidenceIds.nonEmpty &&
          concreteClaimShadowsPlanContinuity(claim, other)
      )

  private[judgment] def directBreakPlanClaim(claim: MoveMeaningClaim): Boolean =
    claim.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
      claim.role == "PreparesBreakOption" &&
      claim.publicHasCarrier &&
      claim.causeEvidenceIds.nonEmpty &&
      (claim.breakFiles.nonEmpty || claim.breakIdentityParts.nonEmpty) &&
      moveEndpoints(claim.moveUci).exists { case (from, to) =>
        from.take(1) == to.take(1) &&
          (for
            fromRank <- from.drop(1).toIntOption
            toRank <- to.drop(1).toIntOption
          yield fromRank != toRank && (fromRank - toRank).abs <= 2).getOrElse(false)
      }

  private def planContinuityClaimHasCurrentMovePlanSubject(claim: MoveMeaningClaim): Boolean =
    val normalizedMove = JudgmentSubjectBinding.normalizeMove(claim.moveUci).toLowerCase
    claim.meaningKind == "PlanContinuity" &&
      claim.surfaceLane == "current_move_function" &&
      claim.boardCarriers.exists(boardCarrierMove(_, normalizedMove)) &&
      claim.boardCarriers.exists(carrier => carrier.role == "target" && carrier.kind == "PlanSubject")

  def currentMovePlanContinuityFunctionReady(claims: List[MoveMeaningClaim], claim: MoveMeaningClaim): Boolean =
    val hasProofCarrier = claim.sourceEvidenceIds.nonEmpty && claim.objectCarrierReady
    claim.supportLevel == "view_surfaced" &&
      hasProofCarrier &&
      (
        planContinuityClaimHasCurrentMovePlanSubject(claim) ||
          planContinuityClaimHasSeparateCurrentMoveCarrier(claims, claim)
      )

  private def planContinuityClaimHasSeparateCurrentMoveCarrier(
      claims: List[MoveMeaningClaim],
      claim: MoveMeaningClaim
  ): Boolean =
    val normalizedMove = JudgmentSubjectBinding.normalizeMove(claim.moveUci).toLowerCase
    claims.exists(carrier =>
      carrier != claim &&
        carrier.meaningKind != "PlanContinuity" &&
        carrier.unit != PositionPlanTechniqueUnit.PlanOptionSet &&
        (carrier.surfaceLane == "current_move_owned" || carrier.surfaceLane == "current_move_function") &&
        JudgmentSubjectBinding.normalizeMove(carrier.moveUci).toLowerCase == normalizedMove &&
        carrier.sourceEvidenceIds.nonEmpty &&
        carrier.boardCarriers.exists(boardCarrierMove(_, normalizedMove)) &&
        carrier.boardCarriers.exists(nonPlanSubjectBoardCarrier)
    )

  private def boardCarrierMove(carrier: MoveMeaningSurfaceBoardCarrier, normalizedMove: String): Boolean =
    (carrier.role == "actor" || carrier.role == "witness") &&
      carrier.kind == "Move" &&
      JudgmentSubjectBinding.normalizeMove(carrier.value).toLowerCase == normalizedMove

  private def nonPlanSubjectBoardCarrier(carrier: MoveMeaningSurfaceBoardCarrier): Boolean =
    carrier.role == "target" &&
      carrier.kind != "PlanSubject" &&
      (carrier.kind == "Square" || carrier.kind == "File" || carrier.kind == "Piece" || carrier.kind == "Pawn")

  private def mergeMeaningClaims(claims: Iterable[MoveMeaningClaim]): Option[MoveMeaningClaim] =
    val list = claims.toList
    list.sortBy(sortKey).lastOption.map(best =>
      val mergedCauseEvidenceIds = list.flatMap(_.causeEvidenceIds).distinct.sorted
      val mergedSourceEvidenceIds = list.flatMap(_.sourceEvidenceIds).distinct.sorted
      val mergedObjectBindingSignatures = list.flatMap(_.objectBindingSignatures).distinct.sorted
      val mergedObjectCarrierReady = list.exists(_.objectCarrierReady)
      val mergedBreakFileCarriers =
        list
          .flatMap(_.breakFiles)
          .distinct
          .sorted
          .map(file => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"break-file:$file"))
      val mergedCarrierPool = list.flatMap(_.boardCarriers)
      val mergedBoardCarriers =
        (
          mergedCarrierPool.filter(carrier => carrier.role == "actor" && carrier.kind == "Move") ++
            mergedBreakFileCarriers ++
            mergedCarrierPool.distinct.sortBy(boardCarrierSortKey)
        ).distinct.take(12)
      val mergedPublicDrawableCarrier =
        mergedBoardCarriers.nonEmpty ||
          best.terminalConsequenceKinds.nonEmpty ||
          best.endgameTechniquePattern.nonEmpty ||
          best.endgameTechniqueRookPattern.nonEmpty
      val mergedPublicHasCarrier =
        mergedPublicDrawableCarrier &&
          mergedObjectCarrierReady &&
          (mergedCauseEvidenceIds.nonEmpty || mergedSourceEvidenceIds.nonEmpty) &&
          publicCarrierAllowed(best.unit, list.flatMap(_.causeKinds), mergedBoardCarriers)
      val mergedPublicProofLevel = list.map(_.publicProofLevel).sortBy(publicProofLevelRank).lastOption.getOrElse("none")
      best.copy(
        causeKinds = list.flatMap(_.causeKinds).distinct.sortBy(_.toString),
        causeSourceSides = list.flatMap(_.causeSourceSides).distinct.sortBy(_.toString),
        causeEvidenceIds = mergedCauseEvidenceIds,
        sourceEvidenceIds = mergedSourceEvidenceIds,
        objectBindingSignatures = mergedObjectBindingSignatures,
        reasonTokens = Nil,
        comparisonLossSides = list.flatMap(_.comparisonLossSides).distinct.sorted,
        comparisonLossKinds = list.flatMap(_.comparisonLossKinds).distinct.sorted,
        objectCarrierReady = mergedObjectCarrierReady,
        boardCarriers = mergedBoardCarriers,
        comparisonMoveRefs = list.flatMap(_.comparisonMoveRefs).distinct.sortBy(ref => (ref.role, ref.uci)).take(12),
        targetSquares = list.flatMap(_.targetSquares).distinct.sorted,
        targetFiles = list.flatMap(_.targetFiles).distinct.sorted,
        targetPieces = list.flatMap(_.targetPieces).distinct.sorted,
        routeIdentityParts = list.flatMap(_.routeIdentityParts).distinct.sorted,
        breakIdentityParts = list.flatMap(_.breakIdentityParts).distinct.sorted,
        breakFiles = list.flatMap(_.breakFiles).distinct.sorted,
        structuralMotifTags = list.flatMap(_.structuralMotifTags).distinct.sorted,
        publicHasCarrier = mergedPublicHasCarrier,
        publicProofLevel =
          if mergedPublicHasCarrier && publicProofLevelRank(mergedPublicProofLevel) == 0 then "surface_evidence"
          else mergedPublicProofLevel,
        publicTargetBound = list.exists(_.publicTargetBound) || mergedBoardCarriers.exists(_.role == "target")
      )
    )

  private def boardCarrierSortKey(carrier: MoveMeaningSurfaceBoardCarrier): (String, String, String, String, String) =
    (carrier.role, carrier.kind, carrier.value, carrier.from.getOrElse(""), carrier.to.getOrElse(""))

  private def publicProofLevelRank(level: String): Int =
    level match
      case "terminal_proof"   => 5
      case "owned_cause"      => 4
      case "cause_linked"     => 3
      case "surface_evidence" => 2
      case "technique"        => 1
      case _                  => 0

  private def publicCarrierAllowed(
      unit: PositionPlanTechniqueUnit,
      causeKinds: List[RelativeCauseKind],
      boardCarriers: List[MoveMeaningSurfaceBoardCarrier]
  ): Boolean =
    unit != PositionPlanTechniqueUnit.CompensationSource ||
      (
        causeKinds.contains(RelativeCauseKind.SacrificeCompensation) &&
          boardCarriers.exists(carrier =>
            carrier.role == "target" &&
              carrier.kind == "PlanSubject" &&
              carrier.value.toLowerCase.startsWith("material-sacrifice:")
          )
      )

  private def duplicateMeaningKey(claim: MoveMeaningClaim): (String, String, String, String, String) =
    val objectKey =
      if claim.meaningKind == "PawnBreakTiming" then
        claim.breakIdentityParts.mkString("|")
      else if claim.meaningKind == "PieceRoute" then
        routeCoreIdentity(claim)
          .map((lineRole, moveUci, piece, from, to, target) =>
            List(lineRole, moveUci, piece, from.getOrElse(""), to.getOrElse(""), target.getOrElse("")).mkString("|")
          )
          .getOrElse(claim.laneKey)
      else if claim.meaningKind == "PlanContinuity" && claim.routeIdentityParts.nonEmpty then
        (claim.routeIdentityParts ++ claim.breakIdentityParts).mkString("|")
      else if claim.meaningKind == "TargetPressure" && targetIdentityParts(claim).nonEmpty then
        targetIdentityParts(claim).mkString("|")
      else claim.laneKey
    (claim.meaningKind, claim.role, claim.surfaceLane, claim.moveUci, objectKey)

  private def targetIdentityParts(claim: MoveMeaningClaim): List[String] =
    (
      claim.targetSquares.map(square => s"square:$square") ++
        claim.targetFiles.map(file => s"file:$file") ++
        claim.targetPieces.map(piece => s"piece:$piece")
    ).distinct.sorted

  private def provenanceKey(claim: MoveMeaningClaim): String =
    if claim.meaningKind == "PawnBreakTiming" then
      (
        claim.axisKey.toList ++
          claim.causeEvidenceIds.map(id => s"cause=$id") ++
          claim.sourceEvidenceIds.map(id => s"source=$id")
      ).sorted.mkString("|")
    else ""

  private def causeFrameMatches(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame
  ): Boolean =
    frame.comparisonKind == verdict.comparisonKind &&
      frame.referenceLine == verdict.referenceLine &&
      frame.candidateLine == verdict.candidateLine

  private def frameMatches(
      graph: TypedEvidenceGraph,
      frame: PositionPlanTechniqueFrame,
      verdict: MoveJudgmentVerdictFrame
  ): Boolean =
    val candidateMove = JudgmentSubjectBinding.normalizeMove(verdict.candidateLine.rootMove)
    val referenceMove = JudgmentSubjectBinding.normalizeMove(verdict.referenceLine.rootMove)
    val frameMove = frame.moveUci.map(JudgmentSubjectBinding.normalizeMove)
    val candidateLineMoveMatches = frame.line.contains(verdict.candidateLine) && frameMove.contains(candidateMove)
    val referenceLineMoveMatches = frame.line.contains(verdict.referenceLine) && frameMove.contains(referenceMove)
    val routeScopeMatches =
      frame.semanticDetails.exists(detail =>
        frame.scope == EvidenceScope.AfterPlayedPosition &&
          detail.structuralRouteMove.exists(move => sameMove(move, verdict.candidateLine.rootMove)) ||
          frame.scope == EvidenceScope.AfterReferencePosition &&
            detail.structuralRouteMove.exists(move => sameMove(move, verdict.referenceLine.rootMove))
      )
    val contrastMatches =
      (frame.mechanismEvidenceIds ++ frame.evidenceIds).exists(id =>
        graph.byId.get(id).exists {
          case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
            payload.comparisonKind == verdict.comparisonKind &&
              payload.referenceLine == verdict.referenceLine &&
              payload.candidateLine == verdict.candidateLine
          case _ =>
            false
        }
      )
    val causeMatches =
      frame.semanticDetails.exists(detail =>
        detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
          detail.causeEvidenceIds.exists(id =>
            graph.byId.get(id).exists {
              case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
                cause.kind == RelativeCauseKind.ConversionSecured &&
                  cause.hasOwnedTypedDepth &&
                  cause.comparisonKind == verdict.comparisonKind &&
                  cause.referenceLine == verdict.referenceLine &&
                  cause.candidateLine == verdict.candidateLine &&
                  endgameTechniqueRootDefenderMoveShape(detail, detail.objectBindingSignatures, cause.eventRootMove)
              case _ =>
                false
            }
          )
      )
    candidateLineMoveMatches || referenceLineMoveMatches || routeScopeMatches || contrastMatches || causeMatches

  private def fromDetail(
      evidenceGraph: TypedEvidenceGraph,
      frame: PositionPlanTechniqueFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      verdict: MoveJudgmentVerdictFrame,
      causeFramesById: Map[String, List[MoveJudgmentCauseFrame]]
  ): List[MoveMeaningClaim] =
    val objectSignatures = detail.objectBindingSignatures.distinct.sorted
    val directLinkedCauseFrames =
      detail.causeEvidenceIds.distinct.sorted
        .flatMap(id =>
          causeFramesById.getOrElse(id, Nil).filter(frame =>
            causeFrameExplainsMeaningDetail(frame, verdict, detail, objectSignatures)
          )
        )
        .distinctBy(_.causeEvidenceIds)
    val bridgedRouteCauseFrames =
      causeFramesById.values.flatten.toList
        .filter(frame => sameRootRouteOwnedCauseBridgeCandidate(frame, verdict, detail, objectSignatures))
        .distinctBy(_.causeEvidenceIds)
    val bridgedPlanCauseFrames =
      causeFramesById.values.flatten.toList
        .filter(frame => sameRootPlanOwnedCauseBridgeCandidate(frame, verdict, detail))
        .distinctBy(_.causeEvidenceIds)
    val linkedCauseFrames = (directLinkedCauseFrames ++ bridgedRouteCauseFrames ++ bridgedPlanCauseFrames).distinctBy(_.causeEvidenceIds)
    val bridgedCauseIds = (bridgedRouteCauseFrames ++ bridgedPlanCauseFrames).flatMap(_.causeEvidenceIds).toSet
    kind(detail, objectSignatures, None).toList
      .filter(_ => detailMatchesLine(frame, detail, verdict, linkedCauseFrames))
      .flatMap { baseMeaningKind =>
        val currentRouteLineRole =
          Option.when(
            currentMoveRouteLineRole(detail, objectSignatures, verdict)
          )("candidate")
        val lineRoleOptions =
          (currentRouteLineRole.toList ++ lineRoles(frame, detail, verdict, linkedCauseFrames)).distinct
        val claimOptions =
          lineRoleOptions.map { optionLineRole =>
            val optionMove = moveUci(verdict, optionLineRole)
            val optionObjectSignatures = surfaceObjectBindingSignatures(detail, objectSignatures, optionMove)
            val optionMeaningKind = kind(detail, optionObjectSignatures, Some(optionMove)).getOrElse(baseMeaningKind)
            val optionClaimRole = role(optionMeaningKind, detail, optionMove, frame.position.fen, optionLineRole)
            val optionRoleCompatibleCauseFrames =
              linkedCauseFrames.filter(linkedFrame =>
                causeFrameOwnsMeaningClaim(
                  linkedFrame,
                  verdict,
                  detail,
                  objectSignatures,
                  optionLineRole,
                  optionMove,
                  optionClaimRole
                ) ||
                  sameRootRouteOwnedCauseBridge(
                    linkedFrame,
                    verdict,
                    detail,
                    objectSignatures,
                    optionLineRole,
                    optionMove,
                    optionClaimRole
                  ) ||
                  sameRootPlanOwnedCauseBridge(
                    linkedFrame,
                    verdict,
                    detail,
                    optionLineRole,
                    optionMove,
                    optionClaimRole
                  )
              )
            (optionLineRole, optionMove, optionObjectSignatures, optionMeaningKind, optionClaimRole, optionRoleCompatibleCauseFrames)
          }
        val boardCarriers = publicBoardCarriers(detail)
        claimOptions.flatMap {
          case (claimLineRole, claimMove, surfaceObjectSignatures, meaningKind, claimRole, roleCompatibleCauseFrames) =>
            supportLevel(
              detail,
              meaningKind,
              linkedCauseFrames,
              roleCompatibleCauseFrames,
              surfaceObjectSignatures,
              verdict,
              claimLineRole,
              claimMove,
              frame.position.fen,
              claimRole,
              evidenceGraph
            ).map { support =>
              val linkedCauseIds =
                roleCompatibleCauseFrames
                  .flatMap(_.causeEvidenceIds)
                  .filter(id => detail.causeEvidenceIds.contains(id) || bridgedCauseIds.contains(id))
                  .distinct
                  .sorted
              val sourceEvidenceIds =
                moveMeaningClaimSourceEvidenceIds(
                  evidenceGraph,
                  detail,
                  surfaceObjectSignatures,
                  verdict,
                  claimLineRole,
                  claimMove
                )
              val comparisonMoveRefs = comparisonMoveRefsFromLineEvidence(evidenceGraph, sourceEvidenceIds)
              val claimOwnedBoardCarriers =
                boardCarriers.filterNot(carrier =>
                  carrier.role == "actor" &&
                    carrier.kind == "Move" &&
                    !sameMove(carrier.value, claimMove)
                )
              val baseClaimBoardCarriers =
                (claimOwnedBoardCarriers ++ lineEventBoardCarriersFromLineEvidence(evidenceGraph, sourceEvidenceIds, claimMove))
                  .distinct
                  .sortBy(boardCarrierSortKey)
              val currentPawnBreakFiles =
                currentPawnAdvanceBreakFiles(detail, claimMove, sourceEvidenceIds)
              val claimBreakFiles = (detail.breakFile.toList.flatMap(claimFile) ++ currentPawnBreakFiles).distinct.sorted
              val breakFileIdentityCarriers =
                claimBreakFiles.map(file => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"break-file:$file"))
              val pressureIdentityCarriers =
                mechanismSquareIdentityCarriers(detail, "battery-pressure", batteryPressureSignature) ++
                  mechanismSquareIdentityCarriers(detail, "pin-pressure", pinPressureSignature)
              val checkIdentityCarriers =
                if pressureIdentityCarriers.nonEmpty then Nil
                else mechanismSquareIdentityCarriers(detail, "check", checkSignature)
              val spareIdentityCarriers =
                lineUnlockIdentityCarriers(detail) ++
                  pressureIdentityCarriers ++
                  checkIdentityCarriers ++
                  defenderMoveIdentityCarriersFromLineEvidence(evidenceGraph, sourceEvidenceIds, claimMove) ++
                  passedPawnIdentityCarriersFromLineEvidence(evidenceGraph, sourceEvidenceIds) ++
                  materialCaptureIdentityCarriersFromLineEvidence(evidenceGraph, sourceEvidenceIds, claimMove, detail, verdict) ++
                  breakFileIdentityCarriers
              val claimBoardCarriers =
                (
                  baseClaimBoardCarriers.filter(carrier => carrier.role == "actor" && carrier.kind == "Move") ++
                    breakFileIdentityCarriers ++
                    (baseClaimBoardCarriers ++ spareIdentityCarriers).distinct.sortBy(boardCarrierSortKey)
                ).distinct
                  .take(12)
              val surfaceTarget = MoveMeaningSurfaceTarget.fromDetail(detail, claimBoardCarriers)
              val objectCarrierReady = publicObjectCarrierReady(evidenceGraph, detail, roleCompatibleCauseFrames)
              val publicDrawableCarrier =
                claimBoardCarriers.nonEmpty ||
                  detail.terminalConsequenceKinds.nonEmpty ||
                  detail.endgameTechniquePattern.nonEmpty ||
                  detail.endgameTechniqueRookPattern.nonEmpty
              val publicHasCarrier =
                publicDrawableCarrier &&
                  objectCarrierReady &&
                  (linkedCauseIds.nonEmpty || sourceEvidenceIds.nonEmpty) &&
                  publicCarrierAllowed(detail.unit, roleCompatibleCauseFrames.map(_.causeKind), claimBoardCarriers)
              val publicProofLevel =
                if !publicHasCarrier then "none"
                else if detail.terminalConsequenceKinds.exists(terminalProofConsequenceKind) then "terminal_proof"
                else if linkedCauseIds.nonEmpty && support == "owned_cause_linked" then "owned_cause"
                else if linkedCauseIds.nonEmpty then "cause_linked"
                else "surface_evidence"
              MoveMeaningClaim(
                meaningKind = meaningKind,
                role = claimRole,
                laneKey = laneKey(meaningKind, detail, boardCarriers),
                conflictKey = conflictKey(meaningKind, detail, boardCarriers),
                supportLevel = support,
                visibility = visibility(support),
                surfaceLane =
                  surfaceLane(
                    detail,
                    verdict,
                    claimLineRole,
                    claimMove,
                    support,
                    surfaceObjectSignatures,
                    roleCompatibleCauseFrames
                  ),
                lineRole = claimLineRole,
                moveUci = moveUci(verdict, claimLineRole),
                frameId = frame.id,
                unit = detail.unit,
                axisKey = detail.axisKey,
                axisKind = detail.axisKind,
                axisPolarity = detail.axisPolarity,
                label = detail.label,
                causeKinds = roleCompatibleCauseFrames.map(_.causeKind).distinct.sortBy(_.toString),
                causeSourceSides = roleCompatibleCauseFrames.map(_.causeSourceSide).distinct.sortBy(_.toString),
                causeEvidenceIds = linkedCauseIds,
                sourceEvidenceIds = sourceEvidenceIds,
                objectBindingSignatures = surfaceObjectSignatures,
                reasonTokens = Nil,
                comparisonLossSides =
                  PositionPlanTechniqueSemanticDetail
                    .comparisonLossSides(detail)
                    .filter(side => side == "candidate" || side == "reference")
                    .distinct
                    .sorted,
                comparisonLossKinds = PositionPlanTechniqueSemanticDetail.comparisonLossKinds(detail).distinct.sorted,
                objectCarrierReady = objectCarrierReady,
                boardCarriers = claimBoardCarriers,
                comparisonMoveRefs = comparisonMoveRefs,
                targetSquares = surfaceTarget.squares,
                targetFiles = surfaceTarget.files,
                targetPieces = surfaceTarget.pieces,
                routeIdentityParts = routeIdentityParts(detail, claimMove),
                breakIdentityParts = breakIdentityParts(detail, claimBreakFiles),
                breakFiles = claimBreakFiles,
                structuralMotifTags = detail.structuralMotifTags.distinct.sorted,
                specificityTier = detail.specificityTier,
                terminalConsequenceKinds = detail.terminalConsequenceKinds.distinct.sorted,
                endgameTechniquePattern = detail.endgameTechniquePattern,
                endgameTechniqueRookPattern = detail.endgameTechniqueRookPattern,
                endgameTechniqueSide = detail.endgameTechniqueSide,
                endgameTechniqueHorizonStatus = detail.endgameTechniqueHorizonStatus,
                endgameTechniqueTriggerMove = detail.endgameTechniqueTriggerMove,
                endgameTechniqueEntryPlyOffset = detail.endgameTechniqueEntryPlyOffset,
                endgameTechniqueTerminalPlyOffset = detail.endgameTechniqueTerminalPlyOffset,
                endgameTechniqueFailureReason = detail.endgameTechniqueFailureReason,
                requiredSquares = detail.requiredSquares.distinct.sorted,
                maintainedSquares = detail.maintainedSquares.distinct.sorted,
                brokenSquares = detail.brokenSquares.distinct.sorted,
                publicIdeaType = publicIdeaType(detail, meaningKind, surfaceObjectSignatures),
                publicHasCarrier = publicHasCarrier,
                publicProofLevel = publicProofLevel,
                publicTargetBound = claimBoardCarriers.exists(_.role == "target")
              )
            }
        }
      }

  private def comparisonMoveRefsFromLineEvidence(
      evidenceGraph: TypedEvidenceGraph,
      sourceEvidenceIds: List[String]
  ): List[MoveMeaningSurfaceMoveRef] =
    sourceEvidenceIds
      .flatMap(id => evidenceGraph.byId.get(id))
      .collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }
      .flatMap(line =>
        val prefix =
          line.line.role match
            case LineNodeRole.Played        => Some("played_pv")
            case LineNodeRole.BestReference => Some("best_pv")
            case LineNodeRole.Alternative   => Some("alternative_pv")
            case LineNodeRole.Threat        => None
        prefix.toList.flatMap(role =>
          line.lineReplayContinuationMoves
            .flatMap(MoveMeaningSurface.publicUciMove)
            .take(4)
            .zipWithIndex
            .map((move, index) => MoveMeaningSurfaceMoveRef(s"${role}_${index + 1}", move))
        )
      )
      .distinct

  private def lineEventBoardCarriersFromLineEvidence(
      evidenceGraph: TypedEvidenceGraph,
      sourceEvidenceIds: List[String],
      claimMove: String
  ): List[MoveMeaningSurfaceBoardCarrier] =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    sourceEvidenceIds
      .flatMap(id => evidenceGraph.byId.get(id))
      .collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }
      .flatMap(line =>
        val carryLineCapturePieces = line.hasTacticalLineConsequence || line.hasProofSignalMaterialEvent
        line.lineEventsOf(LineEventKind.PassedPawn)
          .flatMap(event => event.square.toList.flatMap(square => publicSquareCarrier("target", square.key))) ++
          lineForkTargetCarriers(line) ++
          line.lineEvents
            .filter(event =>
              (event.kind == LineEventKind.Capture || event.kind == LineEventKind.Recapture) &&
                (carryLineCapturePieces || event.plyOffset == 0 ||
                  JudgmentSubjectBinding.normalizeMove(event.moveUci).toLowerCase == normalizedClaimMove)
            )
            .flatMap(event => event.targetRole.toList.flatMap(role => publicPieceCarrier("target", role.name)))
      )
      .distinct

  private def lineForkTargetCarriers(line: LineFactEvidence): List[MoveMeaningSurfaceBoardCarrier] =
    val hasCheck = line.hasLineEvent(LineEventKind.Check)
    val hasCapture = line.hasLineEvent(LineEventKind.Capture)
    if !hasCheck || !hasCapture then Nil
    else
      line.lineEvents
        .filter(event => event.kind == LineEventKind.Check || event.kind == LineEventKind.Capture)
        .flatMap(event => event.square.toList.flatMap(square => publicSquareCarrier("target", square.key)))

  private def defenderMoveIdentityCarriersFromLineEvidence(
      evidenceGraph: TypedEvidenceGraph,
      sourceEvidenceIds: List[String],
      claimMove: String
  ): List[MoveMeaningSurfaceBoardCarrier] =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    sourceEvidenceIds
      .flatMap(id => evidenceGraph.byId.get(id))
      .collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }
      .flatMap(_.lineEvents)
      .filter(event =>
        event.kind == LineEventKind.DefenderMove &&
          (event.plyOffset == 0 || JudgmentSubjectBinding.normalizeMove(event.moveUci).toLowerCase == normalizedClaimMove)
      )
      .flatMap(_.square.toList)
      .map(square => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"defender-move:${square.key}"))
      .distinct

  private def passedPawnIdentityCarriersFromLineEvidence(
      evidenceGraph: TypedEvidenceGraph,
      sourceEvidenceIds: List[String]
  ): List[MoveMeaningSurfaceBoardCarrier] =
    sourceEvidenceIds
      .flatMap(id => evidenceGraph.byId.get(id))
      .collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }
      .flatMap(_.lineEventsOf(LineEventKind.PassedPawn))
      .flatMap(_.square.toList)
      .map(square => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"passed-pawn:${square.key}"))
      .distinct

  private def currentPawnAdvanceBreakFiles(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      sourceEvidenceIds: List[String]
  ): List[String] =
    if !pawnBreakClaimDetail(detail) || !sourceEvidenceIds.exists(_.contains(":evidence:structural-delta:")) then Nil
    else
      moveEndpoints(claimMove).toList.flatMap { case (from, to) =>
        val file = from.take(1)
        Option
          .when(
            from.take(1) == to.take(1) &&
              pawnAdvanceRanks(from, to)
          )(file)
          .toList
      }.distinct

  private def pawnBreakClaimDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.breakFile.nonEmpty ||
      detail.axisKey.exists(_.toLowerCase.contains("pawnbreak")) ||
      detail.label.exists(_.toLowerCase.contains("pawnbreak"))

  private def pawnAdvanceRanks(from: String, to: String): Boolean =
    (for
      fromRank <- from.drop(1).toIntOption
      toRank <- to.drop(1).toIntOption
    yield fromRank != toRank && (fromRank - toRank).abs <= 2).getOrElse(false)

  private def materialCaptureIdentityCarriersFromLineEvidence(
      evidenceGraph: TypedEvidenceGraph,
      sourceEvidenceIds: List[String],
      claimMove: String,
      detail: PositionPlanTechniqueSemanticDetail,
      verdict: MoveJudgmentVerdictFrame
  ): List[MoveMeaningSurfaceBoardCarrier] =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val claimDestination = moveEndpoints(claimMove).map(_._2)
    val currentMoveSacrificeAllowed =
      detail.mechanismKinds.contains(StrategicMechanismKind.StrategicConcession) ||
        verdict.verdict == MoveChoiceVerdict.ImprovesOnReference ||
        verdict.verdict == MoveChoiceVerdict.MatchesReference
    sourceEvidenceIds
      .flatMap(id => evidenceGraph.byId.get(id))
      .collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }
      .flatMap(line =>
        val currentCaptureCarriers =
          line.materialCaptures
            .filter(capture =>
              capture.plyOffset == 0 || JudgmentSubjectBinding.normalizeMove(capture.moveUci).toLowerCase == normalizedClaimMove
            )
            .flatMap(capture =>
              val prefix = if capture.recapture then "material-recapture" else "material-capture"
              val base = MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"$prefix:${capture.square.key}")
              val sacrifice =
                Option.when(line.materialSacrificeCapture(capture))(
                  MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"material-sacrifice:${capture.square.key}")
                )
              base :: sacrifice.toList
            )
        val replyCapturesMovedPiece =
          if !currentMoveSacrificeAllowed then Nil
          else
            claimDestination.toList.flatMap(destination =>
              line.materialCaptures
                .filter(capture =>
                  capture.plyOffset == 1 &&
                    !capture.recapture &&
                    capture.square.key == destination &&
                    capture.capturedRole.name.equalsIgnoreCase("pawn")
                )
                .map(capture => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"material-sacrifice:${capture.square.key}"))
            )
        currentCaptureCarriers ++ replyCapturesMovedPiece
      )
      .distinct

  private def publicBoardCarriers(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[MoveMeaningSurfaceBoardCarrier] =
    val terminalTargetCarriers =
      if detail.terminalConsequenceKinds.exists(terminalProofConsequenceKind) then
        EvidenceObjectBinding
          .signatureTokens(detail.objectBindingSignatures, "target=Square:")
          .flatMap(token => publicSquareCarrier("target", token.stripPrefix("target=Square:")))
      else Nil
    (
        detail.structuralRouteMove.toList.map(move => publicMoveCarrier("actor", move)) ++
        terminalTargetCarriers ++
        structuralRouteFileCarriers(detail) ++
        flankPawnAdvanceDestinationCarriers(detail) ++
        detail.structuralPurposeSubjects.flatMap(publicStructuralSubjectCarriers) ++
        publicPlanSubjectCarriers(detail) ++
        detail.breakFile.toList.flatMap(file => publicFileCarrier("target", file)) ++
        detail.counterBreakFiles.flatMap(file => publicFileCarrier("target", file)) ++
        detail.resourceContestFiles.flatMap(file => publicFileCarrier("target", file)) ++
        (
          detail.tensionSquares ++
            detail.blockadeSquare.toList ++
            detail.resourceContestSquares ++
            detail.requiredSquares ++
            detail.maintainedSquares ++
            detail.brokenSquares
        ).flatMap(square => publicSquareCarrier("target", square))
    ).distinct.sortBy(boardCarrierSortKey).take(12)

  private def structuralRouteFileCarriers(detail: PositionPlanTechniqueSemanticDetail): List[MoveMeaningSurfaceBoardCarrier] =
    if !lineUnlockDetail(detail) then Nil
    else detail.structuralRouteMove.toList.flatMap(sameFileMoveFile).flatMap(file => publicFileCarrier("target", file))

  private def flankPawnAdvanceDestinationCarriers(detail: PositionPlanTechniqueSemanticDetail): List[MoveMeaningSurfaceBoardCarrier] =
    detail.structuralRouteMove.toList.flatMap(flankPawnAdvanceDestination).flatMap(square => publicSquareCarrier("target", square))

  private def flankPawnAdvanceDestination(move: String): Option[String] =
    moveEndpoints(move).collect {
      case (from, to)
          if from.take(1) == to.take(1) &&
            from.headOption.exists(flankFile) &&
            pawnAdvanceRankShape(from, to) =>
        to
    }

  private def flankFile(file: Char): Boolean =
    file == 'a' || file == 'b' || file == 'g' || file == 'h'

  private def pawnAdvanceRankShape(from: String, to: String): Boolean =
    val fromRank = from.drop(1).headOption.filter(_.isDigit).map(_.asDigit)
    val toRank = to.drop(1).headOption.filter(_.isDigit).map(_.asDigit)
    fromRank.zip(toRank).exists { case (fromValue, toValue) =>
      val rankDelta = (toValue - fromValue).abs
      rankDelta >= 1 && rankDelta <= 2 &&
        ((toValue > fromValue && toValue >= 4) || (toValue < fromValue && toValue <= 5))
    }

  private def lineUnlockIdentityCarriers(detail: PositionPlanTechniqueSemanticDetail): List[MoveMeaningSurfaceBoardCarrier] =
    detail.objectBindingSignatures
      .filter(lineUnlockSignature)
      .flatMap(signature => EvidenceObjectBinding.signatureTokens(List(signature), "target=Square:"))
      .toList
      .flatMap(token => claimSquare(token.stripPrefix("target=Square:")))
      .distinct
      .map(square => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"line-unlock:$square"))

  private def mechanismSquareIdentityCarriers(
      detail: PositionPlanTechniqueSemanticDetail,
      carrierPrefix: String,
      signatureMatches: String => Boolean
  ): List[MoveMeaningSurfaceBoardCarrier] =
    detail.objectBindingSignatures
      .filter(signatureMatches)
      .flatMap { signature =>
        val squares =
          EvidenceObjectBinding
            .signatureTokens(List(signature), "target=Square:")
            .toList
            .flatMap(token => claimSquare(token.stripPrefix("target=Square:")))
            .distinct
            .sorted
        Option.when(squares.nonEmpty)(
          MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", s"$carrierPrefix:${squares.mkString("-")}")
        )
      }
      .distinct

  private def lineUnlockDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.objectBindingSignatures.exists(signature => signature.toLowerCase.contains("lineunlock") || signature.toLowerCase.contains("line-unlock"))

  private def lineUnlockSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("mechanism=mechanism:lineunlock") ||
      normalized.contains("mechanism=mechanism:line-unlock") ||
      normalized.contains("consequence=consequence:lineunlock")

  private def batteryPressureSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("mechanism=mechanism:batterypressure") ||
      normalized.contains("consequence=consequence:batterypressure")

  private def pinPressureSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("mechanism=mechanism:pinpressure") ||
      normalized.contains("consequence=consequence:pinpressure")

  private def checkSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("mechanism=mechanism:check") ||
      normalized.contains("consequence=consequence:check")

  private def sameFileMoveFile(move: String): Option[String] =
    moveEndpoints(move).collect { case (from, to) if from.take(1) == to.take(1) => from.take(1) }

  private def publicStructuralSubjectCarriers(subject: String): List[MoveMeaningSurfaceBoardCarrier] =
    val identityCarriers =
      StructuralPurposeSubject.structuralIdentity(subject).toList.map { value =>
        MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", value)
      }
    val weakPawnSquare = StructuralPurposeSubject.weakPawnSquare(subject)
    val fileSquareTarget = StructuralPurposeSubject.fileSquareTarget(subject)
    val carriers = StructuralPurposeSubject.parse(subject) match
      case Some(StructuralPurposeSubject.PieceRoute(piece, from, to)) =>
        publicPieceCarrier("actor", piece) ++ publicSquareCarrier("actor", from) ++ publicSquareCarrier("target", to)
      case Some(StructuralPurposeSubject.Outpost(piece, square)) =>
        publicPieceCarrier("actor", piece) ++ publicSquareCarrier("target", square)
      case Some(StructuralPurposeSubject.Battery(_, from, to, roles)) =>
        roles.flatMap(role => publicPieceCarrier("actor", role)) ++
          publicSquareCarrier("target", from) ++ publicSquareCarrier("target", to)
      case Some(StructuralPurposeSubject.PieceRestriction(piece, square, blocker)) =>
        publicPieceCarrier("actor", piece) ++ publicSquareCarrier("target", square) ++ publicSquareCarrier("target", blocker)
      case Some(StructuralPurposeSubject.PieceSquare(piece, square)) =>
        publicPieceCarrier("actor", piece) ++ publicSquareCarrier("actor", square) ++ publicSquareCarrier("target", square)
      case Some(StructuralPurposeSubject.TensionEdge(from, to)) =>
        publicSquareCarrier("target", from) ++ publicSquareCarrier("target", to)
      case _ if weakPawnSquare.nonEmpty =>
        weakPawnSquare.toList.flatMap(square =>
          MoveMeaningSurfaceBoardCarrier("target", "Pawn", s"weak-pawn:$square") :: publicSquareCarrier("target", square)
        )
      case _ if fileSquareTarget.nonEmpty =>
        fileSquareTarget.toList.flatMap { case (file, square) =>
          publicFileCarrier("target", file) ++ publicSquareCarrier("target", square)
        }
      case _ =>
        val carrierSubject = StructuralPurposeSubject.carrierToken(subject)
        val carriers = StructuralPurposeSubject.parse(carrierSubject) match
          case Some(StructuralPurposeSubject.TensionEdge(from, to)) =>
            publicSquareCarrier("target", from) ++ publicSquareCarrier("target", to)
          case _ =>
            publicSquareCarrier("target", carrierSubject) match
              case Nil if carrierSubject.matches("[a-h]") => publicFileCarrier("target", carrierSubject)
              case Nil                                   => Nil
              case carriers => carriers
        carriers match
          case Nil      => Nil
          case carriers => carriers
    (identityCarriers ++ carriers).distinct

  private def publicPlanSubjectCarriers(detail: PositionPlanTechniqueSemanticDetail): List[MoveMeaningSurfaceBoardCarrier] =
    if detail.unit != PositionPlanTechniqueUnit.PlanOptionSet then Nil
    else
      (detail.matchedPlanIds ++ detail.referencePlanIds ++ detail.candidatePlanIds)
        .flatMap(_.split(",").toList)
        .map(_.trim.toLowerCase)
        .filter(_.nonEmpty)
        .distinct
        .map(value => MoveMeaningSurfaceBoardCarrier("target", "PlanSubject", value))

  private def publicObjectCarrierReady(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      roleCompatibleCauseFrames: List[MoveJudgmentCauseFrame]
  ): Boolean =
    roleCompatibleCauseFrames.exists(_.concreteObjectReady) ||
      EvidenceObjectBinding.playerFacingReady(
        EvidenceObjectBinding.fromEvidenceRefs(
          evidenceGraph,
          (
            detail.sourceEvidenceIds ++
              detail.referenceEvidenceIds ++
              detail.candidateEvidenceIds
          ).distinct.flatMap(id => evidenceGraph.byId.get(id).map(_.ref))
        )
      )

  private def publicMoveCarrier(role: String, move: String): MoveMeaningSurfaceBoardCarrier =
    val normalized = JudgmentSubjectBinding.normalizeMove(move).toLowerCase
    val (from, to) =
      if normalized.length >= 4 then (Some(normalized.take(2)), Some(normalized.slice(2, 4)))
      else (None, None)
    MoveMeaningSurfaceBoardCarrier(role, "Move", normalized, from, to)

  private def publicSquareCarrier(role: String, square: String): List[MoveMeaningSurfaceBoardCarrier] =
    claimSquare(square).map(value => MoveMeaningSurfaceBoardCarrier(role, "Square", value)).toList

  private def publicFileCarrier(role: String, file: String): List[MoveMeaningSurfaceBoardCarrier] =
    claimFile(file).map(value => MoveMeaningSurfaceBoardCarrier(role, "File", value)).toList

  private def publicPieceCarrier(role: String, piece: String): List[MoveMeaningSurfaceBoardCarrier] =
    val normalized = piece.trim.toLowerCase
    Option.when(normalized.nonEmpty)(MoveMeaningSurfaceBoardCarrier(role, "Piece", normalized)).toList

  private def routeIdentityParts(detail: PositionPlanTechniqueSemanticDetail, claimMove: String): List[String] =
    (
      detail.structuralPurposeSubjects.flatMap { subject =>
        val normalizedSubject = subject.trim.toLowerCase
        StructuralPurposeSubject.parse(subject) match
          case Some(StructuralPurposeSubject.PieceRoute(piece, from, to)) =>
            List(s"piece:$piece", s"from:$from", s"to:$to", s"subject:$normalizedSubject")
          case Some(StructuralPurposeSubject.Outpost(piece, square)) =>
            List(s"piece:$piece", s"target:$square", s"subject:$normalizedSubject")
          case Some(StructuralPurposeSubject.Battery(axis, from, to, roles)) =>
            List(s"axis:${axis.toLowerCase}", s"from:$from", s"to:$to", s"subject:$normalizedSubject") ++ roles.map(role => s"piece:$role")
          case Some(StructuralPurposeSubject.PieceRestriction(piece, square, blocker)) =>
            List(s"piece:$piece", s"target:$square", s"blocker:$blocker", s"subject:$normalizedSubject")
          case Some(StructuralPurposeSubject.PieceSquare(piece, square)) if lineUnlockRouteToken(normalizedSubject) =>
            List(s"piece:$piece", s"target:$square", s"subject:$normalizedSubject")
          case _ =>
            Nil
      } ++ lineEventRouteIdentityParts(detail, claimMove)
    ).distinct.sorted

  private def lineEventRouteIdentityParts(detail: PositionPlanTechniqueSemanticDetail, claimMove: String): List[String] =
    if detail.unit != PositionPlanTechniqueUnit.PieceRerouteRoute then Nil
    else
      val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
      detail.objectBindingSignatures.flatMap { signature =>
        val signatureList = List(signature)
        if !lineEventRouteSignature(signature) then Nil
        else
          val pieces =
            EvidenceObjectBinding
              .signatureTokens(signatureList, "actor=Piece:")
              .toList
              .map(_.stripPrefix("actor=Piece:").trim.toLowerCase)
              .filter(piece => piece.nonEmpty && piece != "king" && piece != "pawn")
          val moves =
            EvidenceObjectBinding
              .signatureTokens(signatureList, "actor=Move:")
              .toList
              .map(token => JudgmentSubjectBinding.normalizeMove(token.stripPrefix("actor=Move:")).toLowerCase)
              .filter(_ == normalizedClaimMove)
              .flatMap(moveEndpoints)
          for
            piece <- pieces
            (from, to) <- moves
            if EvidenceObjectBinding.signatureTokens(signatureList, "actor=Square:").exists(_.stripPrefix("actor=Square:").equalsIgnoreCase(from))
          yield List(s"piece:$piece", s"from:$from", s"to:$to", s"subject:$piece:$from-$to:line-event")
      }.flatten

  private def lineEventRouteSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("horizon=ply:") &&
      (
        normalized.contains("mechanism=mechanism:check") ||
          normalized.contains("mechanism=mechanism:tempo") ||
          normalized.contains("mechanism=mechanism:capture") ||
          normalized.contains("mechanism=mechanism:materialcapture") ||
          normalized.contains("mechanism=mechanism:materialrecapture") ||
          normalized.contains("mechanism=mechanism:defendermove")
      )

  private def breakIdentityParts(detail: PositionPlanTechniqueSemanticDetail, breakFiles: List[String]): List[String] =
    (
      breakFiles.map(file => s"breakFile:$file") ++
        detail.tensionSquares.flatMap(claimSquare).map(square => s"tensionSquare:$square") ++
        detail.tensionEdges.flatMap(edge =>
          val normalized = edge.trim.toLowerCase
          Option.when(normalized.nonEmpty)(s"tensionEdge:$normalized")
        )
    ).distinct.sorted

  private def claimFile(value: String): Option[String] =
    Option(value).map(_.trim.toLowerCase.take(1)).filter(_.matches("[a-h]"))

  private def claimSquare(value: String): Option[String] =
    Option(value).map(_.trim.toLowerCase).filter(_.matches("[a-h][1-8]"))

  private def publicIdeaType(
      detail: PositionPlanTechniqueSemanticDetail,
      meaningKind: String,
      objectSignatures: List[String]
  ): Option[String] =
    detail.unit match
      case PositionPlanTechniqueUnit.SpacePreventionResourceDenial if rayDenialDetail(detail) =>
        Some("ray_denial")
      case PositionPlanTechniqueUnit.PieceRerouteRoute if meaningKind == "PieceRoute" && outpostRouteDetail(detail) =>
        Some("outpost_attempt")
      case PositionPlanTechniqueUnit.PieceRerouteRoute if meaningKind == "PieceRoute" && longDiagonalRouteDetail(detail) =>
        Some("long_diagonal_pressure")
      case PositionPlanTechniqueUnit.PieceRerouteRoute if meaningKind == "PieceRoute" && checkingRouteTargetPressureDetail(objectSignatures) =>
        Some("target_pressure")
      case _ =>
        None

  private def outpostRouteDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.structuralPurposeSubjects.exists(subject =>
      StructuralPurposeSubject.parse(subject).exists(_.isInstanceOf[StructuralPurposeSubject.Outpost])
    )

  private def longDiagonalRouteDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.structuralPurposeSubjects.exists(subject =>
      StructuralPurposeSubject.parse(subject) match
        case Some(StructuralPurposeSubject.Battery(axis, _, _, _)) => axis.equalsIgnoreCase("diagonal")
        case _                                                     => false
    )

  private def checkingRouteTargetPressureDetail(objectSignatures: List[String]): Boolean =
    objectSignatures.exists(checkSignature)

  private def rayDenialDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.structuralPurposeSubjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)

  private def currentMoveRouteLineRole(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      verdict: MoveJudgmentVerdictFrame
  ): Boolean =
    terminalProofDetailOwnsClaimMove(detail, objectSignatures, verdict.candidateLine.rootMove) ||
      detail.structuralRouteMove.exists(move => sameMove(move, verdict.candidateLine.rootMove)) &&
      (
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
          structuralCurrentMoveCarrier(detail) ||
          detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
            (
              pieceRouteQualifiedCarrierForMove(detail, objectSignatures, verdict.candidateLine.rootMove) ||
                structuralOpenCenterDevelopmentRoute(detail)
            )
      )

  private def supportLevel(
      detail: PositionPlanTechniqueSemanticDetail,
      meaningKind: String,
      allLinkedCauseFrames: List[MoveJudgmentCauseFrame],
      roleCompatibleCauseFrames: List[MoveJudgmentCauseFrame],
      objectSignatures: List[String],
      verdict: MoveJudgmentVerdictFrame,
      claimLineRole: String,
      claimMove: String,
      positionFen: String,
      claimRole: String,
      evidenceGraph: TypedEvidenceGraph
  ): Option[String] =
    val hasConcreteObject = detailHasConcreteSurfaceObject(detail)
    val specificObjectAxis = detailHasSpecificObjectAxis(detail)
    val hasDetailEvidence = detailHasEvidenceLink(detail)
    val currentMoveClaim = currentMoveMeaningClaim(verdict, claimLineRole, claimMove)
    val directCurrentMoveCarrier =
      !currentMoveClaim ||
        currentMoveDirectCarrier(evidenceGraph, detail, objectSignatures, claimMove)
    lazy val currentMoveFunctionalProof =
      directCurrentMoveCarrier &&
        currentMoveFunctionalDetailProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
    lazy val currentMoveSurfaceProof =
      directCurrentMoveCarrier &&
        currentMoveSurfaceReady(evidenceGraph, meaningKind, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
    val reasonGradeCauseFrames =
      roleCompatibleCauseFrames.filter(frame => reasonGradeCauseFrame(frame) || planFallbackReasonFrame(detail, frame, Some(meaningKind)))
    val ownedCause =
      reasonGradeCauseFrames.exists(frame => frame.hasOwnedAdmissibleLongTermProof || frame.attributionDirectProofEligible)
    val ownedCandidateCauseOwnsCurrentMove =
      currentMoveClaim &&
        reasonGradeCauseFrames.exists(ownedCandidateCauseFrameOwnsClaimMove(_, claimMove))
    val planOptionCurrentFunctionOnly =
      currentMoveClaim &&
        detail.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
        !ownedCandidateCauseOwnsCurrentMove
    val rejectedPositiveCause =
      allLinkedCauseFrames.nonEmpty &&
        roleCompatibleCauseFrames.isEmpty &&
        positiveMeaningRole(claimRole) &&
        allLinkedCauseFrames.exists(frame => !causeFramePolarityCompatibleWithMeaning(frame, verdict, detail, claimRole))
    val badCurrentMovePositiveMeaning =
      currentMoveClaim &&
        badVerdict(verdict.verdict) &&
        positiveCurrentMoveReasonRole(claimRole, detail)
    val broadPlanContinuityCurrentMove =
      currentMoveClaim &&
        meaningKind == "PlanContinuity" &&
        !ownedCandidateCauseOwnsCurrentMove &&
        !planContinuityCurrentMoveFunctionReady(evidenceGraph, detail, objectSignatures, claimMove, positionFen)
    val ownedMeaningReady =
      detail.unit match
        case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
          pawnBreakOwnedCauseReady(detail, claimMove, positionFen)
        case PositionPlanTechniqueUnit.CounterplayRace =>
          counterplayRaceMeaningReady(detail, objectSignatures, claimMove, positionFen)
        case PositionPlanTechniqueUnit.PieceRerouteRoute if meaningKind == "PieceRoute" =>
          pieceRouteOwnedCauseReady(detail, objectSignatures, claimMove)
        case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
          endgameTechniqueOwnedShape(detail, objectSignatures, claimMove)
        case _ =>
          true
    lazy val viewMeaningReady =
      detail.unit match
        case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
          currentMoveSurfaceProof
        case PositionPlanTechniqueUnit.CounterplayRace =>
          currentMoveSurfaceProof
        case PositionPlanTechniqueUnit.PieceRerouteRoute =>
          currentMoveSurfaceProof
        case PositionPlanTechniqueUnit.SpacePreventionResourceDenial | PositionPlanTechniqueUnit.StructuralTransformation =>
          if currentMoveClaim then currentMoveSurfaceProof
          else true
        case _ =>
          true
    val endgameTechniqueViewProof = endgameTechniqueHorizonViewShape(detail)
    lazy val laneOwnershipReady =
      !currentMoveClaim ||
        currentMoveSurfaceProof
    val ownedLaneOwnershipReady =
      !currentMoveClaim ||
        ownedCandidateCauseOwnsCurrentMove ||
        currentMoveSurfaceProof
    if terminalOverriddenEndgameTechniqueDetail(detail) && endgameTechniqueViewProof then
      Some("contextual")
    else if reasonGradeCauseFrames.nonEmpty && ownedCause && ownedLaneOwnershipReady && ownedMeaningReady &&
        !planOptionCurrentFunctionOnly && !badCurrentMovePositiveMeaning && !broadPlanContinuityCurrentMove
    then
      Some("owned_cause_linked")
    else if laneOwnershipReady && viewMeaningReady && hasConcreteObject &&
        (specificObjectAxis || currentMoveFunctionalProof || endgameTechniqueViewProof) && hasDetailEvidence &&
        (detailHasAnyProofLink(detail) || currentMoveFunctionalProof || endgameTechniqueViewProof) &&
        !rejectedPositiveCause && !broadPlanContinuityCurrentMove
    then
      Some("view_surfaced")
    else if hasDetailEvidence && contextualMeaningDetail(detail) then
      Some("contextual")
    else None

  private def currentMoveFunctionalDetailProof(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String,
      positionFen: String,
      currentMoveClaim: Boolean
  ): Boolean =
    if terminalProofDetailOwnsClaimMove(detail, objectSignatures, claimMove) then true
    else
      val moveOwnedSource =
        currentMoveCarrierSourceOwnsClaimMove(evidenceGraph, detail, claimMove)
      detail.unit match
        case PositionPlanTechniqueUnit.PieceRerouteRoute =>
          moveOwnedSource &&
            detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) &&
            (pieceRouteDetailReady(detail) || structuralOpenCenterDevelopmentRoute(detail)) &&
            pieceRouteOwnsClaimMove(detail, claimMove)
        case PositionPlanTechniqueUnit.StructuralTransformation =>
          moveOwnedSource &&
            detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) &&
            generalDetailOwnsClaimMove(detail, objectSignatures, claimMove) &&
            (
              !currentMoveNegativeStructuralHook(detail, objectSignatures, claimMove) ||
                positiveStructuralCurrentMoveCarrier(detail)
            ) &&
            (
              structuralCurrentMoveCarrier(detail)
            )
        case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
          moveOwnedSource &&
            pawnBreakEvidenceOwnsClaimMove(evidenceGraph, detail, objectSignatures, claimMove) &&
            pawnMoveFromPawn(positionFen, claimMove) &&
            pawnBreakOwnsClaimMove(detail, claimMove) &&
            pawnBreakCurrentMoveFunctionalCarrier(detail)
        case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
          moveOwnedSource &&
            resourceDetailOwnsClaimMove(detail, objectSignatures, claimMove) &&
            resourceDetailHasConcreteCarrier(detail)
        case PositionPlanTechniqueUnit.CounterplayRace =>
          moveOwnedSource &&
            counterplayRaceMeaningReady(detail, objectSignatures, claimMove, positionFen)
        case PositionPlanTechniqueUnit.PlanOptionSet =>
          planContinuityCurrentMoveFunctionalProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
        case _ =>
          false

  private def terminalProofDetailOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    detail.terminalConsequenceKinds.exists(terminalProofConsequenceKind) &&
      detail.structuralRouteMove.nonEmpty &&
      objectSignatures.exists(terminalProofSignatureOwnsMoveAndTarget(_, normalizedClaimMove))

  private def terminalProofConsequenceKind(kind: String): Boolean =
    LineConsequenceKind.terminalResultProofName(kind)

  private def terminalProofSignatureOwnsMoveAndTarget(signature: String, normalizedClaimMove: String): Boolean =
    val signatureList = List(signature)
    (
      EvidenceObjectBinding.signatureTokens(signatureList, "actor=Move:") ++
        EvidenceObjectBinding.signatureTokens(signatureList, "witness=Move:")
    )
      .map(part =>
        JudgmentSubjectBinding
          .normalizeMove(part.stripPrefix("actor=Move:").stripPrefix("witness=Move:"))
          .toLowerCase
      )
      .contains(normalizedClaimMove) &&
      EvidenceObjectBinding.signatureTokens(signatureList, "target=").exists(EvidenceObjectBinding.concreteTargetToken)

  private def currentMoveDirectCarrier(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    if terminalProofDetailOwnsClaimMove(detail, objectSignatures, claimMove) then true
    else
      val sourceOwnsCurrentMove =
        currentMoveCarrierSourceOwnsClaimMove(evidenceGraph, detail, claimMove)
      val lineHorizonOwnsCurrentMove =
        detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
          endgameTechniqueHorizonViewShape(detail) &&
          detail.endgameTechniqueTriggerMove.exists(move => sameMove(move, claimMove))
      sourceOwnsCurrentMove ||
        lineHorizonOwnsCurrentMove

  private def moveMeaningClaimSourceEvidenceIds(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      verdict: MoveJudgmentVerdictFrame,
      claimLineRole: String,
      claimMove: String
  ): List[String] =
    if currentMoveMeaningClaim(verdict, claimLineRole, claimMove) then
      val terminalProofSources =
        Option
          .when(
            terminalProofDetailOwnsClaimMove(detail, objectSignatures, claimMove)
          )(detail.sourceEvidenceIds)
          .getOrElse(Nil)
      val signatureLineIds =
        (
          EvidenceObjectBinding.signatureTokens(objectSignatures, "line=").map(_.stripPrefix("line=")) ++
            EvidenceObjectBinding.signatureTokens(objectSignatures, "witness=Line:").map(_.stripPrefix("witness=Line:"))
        ).filter(_.nonEmpty).toSet
      val signatureLineRole =
        claimLineRole match
          case "candidate"   => Some(LineNodeRole.Played)
          case "reference"   => Some(LineNodeRole.BestReference)
          case "alternative" => Some(LineNodeRole.Alternative)
          case _             => None
      val lineWitnessSources =
        if signatureLineIds.isEmpty then Nil
        else
          evidenceGraph.records.collect {
            case EvidenceRecord(ref, _: LineFactEvidence, _)
                if ref.line.exists(line => signatureLineIds.contains(line.id) && signatureLineRole.forall(_ == line.role)) =>
              ref.id
          }
      (
        currentMoveCarrierSourceEvidenceIds(evidenceGraph, detail, claimMove) ++
          terminalProofSources ++
          currentMoveStructureContextSourceEvidenceIds(evidenceGraph, detail) ++
          lineWitnessSources
      ).distinct.sorted
    else detail.sourceEvidenceIds.distinct.sorted

  private def currentMoveCarrierSourceEvidenceIds(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): List[String] =
    val graphOwnedSources =
      (detail.sourceEvidenceIds ++ detail.candidateEvidenceIds)
      .distinct
      .sorted
      .filter(id => evidenceGraph.byId.get(id).exists(JudgmentSubjectBinding.sourceRecordOwnsCurrentPlayedMove(_, claimMove)))
    if graphOwnedSources.nonEmpty then graphOwnedSources
    else if detail.unit == PositionPlanTechniqueUnit.StructuralTransformation && structuralCurrentMoveCarrier(detail) then
      detail.sourceEvidenceIds.distinct.sorted.filter(currentMovePlayedSourceId(_, claimMove))
    else Nil

  private def currentMovePlayedSourceId(id: String, claimMove: String): Boolean =
    val normalizedMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val normalizedId = id.toLowerCase
    normalizedId.contains(s":played:$normalizedMove") ||
      normalizedId.contains(s":$normalizedMove:played-transition")

  private def currentMoveStructureContextSourceEvidenceIds(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    if structuralOpenCenterDevelopmentRoute(detail) then
      detail.sourceEvidenceIds.filter(id => evidenceGraph.byId.get(id).exists(currentMoveStructureContextRecord))
    else Nil

  private def currentMoveStructureContextRecord(record: EvidenceRecord): Boolean =
    record.ref.producer == EvidenceProducer.PawnStructureProducer ||
      record.payload.isInstanceOf[PawnStructureFactEvidence]

  private def currentMoveCarrierSourceOwnsClaimMove(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    currentMoveCarrierSourceEvidenceIds(evidenceGraph, detail, claimMove).nonEmpty

  private def reasonGradeCauseFrame(frame: MoveJudgmentCauseFrame): Boolean =
    (
      frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot ||
        frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
    ) &&
      reasonGradeFrameProofReady(frame)

  private def planFallbackReasonFrame(
      detail: PositionPlanTechniqueSemanticDetail,
      frame: MoveJudgmentCauseFrame,
      meaningKind: Option[String] = None
  ): Boolean =
    val concretePlanCarrier =
      meaningKind.forall(planFallbackMeaningKindAllowed) &&
        planImprovementConcreteCarrierDetail(detail, frame) &&
        planCauseFrameOverlapsDetail(frame, detail)
    frame.causeKind == RelativeCauseKind.PlanImprovement &&
      frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.FallbackRoot &&
      frame.hasOwnedAdmissibleLongTermProof &&
      frame.attributionDirectProofEligible &&
      frame.attributionRootMoveMatched &&
      reasonGradeFrameProofReady(frame) &&
      (
        detail.unit == PositionPlanTechniqueUnit.PlanOptionSet ||
          counterplayRestraintCarrierDetail(detail) ||
          concretePlanCarrier
      )

  private def planFallbackMeaningKindAllowed(meaningKind: String): Boolean =
    meaningKind != "StructureShift"

  private def planImprovementConcreteCarrierDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      frame: MoveJudgmentCauseFrame
  ): Boolean =
    (
      detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
        detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial
    ) &&
      detail.structuralRouteMove.exists(move => sameMove(move, frame.eventRootMove)) &&
      !detail.axisKind.contains(StrategicAxisKind.PlanCoherence) &&
      !negativeStrategicDetail(detail) &&
      !planImprovementFallbackFrameHasNegativeAxis(frame) &&
      detailHasConcreteSurfaceObject(detail) &&
      detailHasSpecificObjectAxis(detail) &&
      detailHasDirectOrContrastProof(detail) &&
      !detail.objectBindingSignatures.exists(lineEventRouteSignature) &&
      (
        causeFrameObjectOverlapsDetail(frame, detail.objectBindingSignatures) ||
          frame.proofStrategicAxisKeys.exists(axisKey => detail.axisKey.exists(_.equalsIgnoreCase(axisKey)))
      )

  private def planImprovementFallbackFrameHasNegativeAxis(frame: MoveJudgmentCauseFrame): Boolean =
    frame.proofStrategicAxisKeys.exists(axisKey =>
      val normalized = axisKey.toLowerCase
      normalized.contains(":concede:") ||
        normalized.contains(":loss:") ||
        normalized.contains("concession") ||
        normalized.contains("liability")
    )

  private def ownedCandidateCauseFrameOwnsClaimMove(frame: MoveJudgmentCauseFrame, claimMove: String): Boolean =
    frame.causeSourceSide == RelativeCauseSourceSide.Candidate &&
      frame.attributionDirectProofEligible &&
      frame.attributionRootMoveMatched &&
      sameMove(frame.eventRootMove, claimMove)

  private def reasonGradeFrameProofReady(frame: MoveJudgmentCauseFrame): Boolean =
    frame.concreteObjectReady ||
      EvidenceObjectBinding.directProofSpecificTargetReady(frame.objectBindingSignatures) ||
      frame.proofLineConsequences.exists(LineConsequenceKind.terminalResultProof)

  private def positiveCurrentMoveReasonRole(
      claimRole: String,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    positiveMeaningRole(claimRole) ||
      (claimRole == "ExplainsMoveFunction" && !negativeStrategicDetail(detail))

  private def planContinuityCurrentMoveFunctionReady(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String,
      positionFen: String
  ): Boolean =
    detailHasConcreteSurfaceObject(detail) &&
      planContinuityCurrentMoveFunctionalProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim = true)

  private def currentMoveMeaningClaim(
      verdict: MoveJudgmentVerdictFrame,
      claimLineRole: String,
      claimMove: String
  ): Boolean =
    val candidateMove = JudgmentSubjectBinding.normalizeMove(verdict.candidateLine.rootMove)
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove)
    (
      verdict.comparisonKind == CandidateComparisonKind.PlayedVsBest ||
        verdict.comparisonKind == CandidateComparisonKind.PlayedVsAlternative
    ) &&
      claimLineRole == "candidate" &&
      normalizedClaimMove == candidateMove

  private def currentMoveSurfaceReady(
      evidenceGraph: TypedEvidenceGraph,
      meaningKind: String,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String,
      positionFen: String,
      currentMoveClaim: Boolean
  ): Boolean =
    detail.unit match
      case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
        currentMoveFunctionalDetailProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
      case PositionPlanTechniqueUnit.CounterplayRace =>
        currentMoveFunctionalDetailProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
      case PositionPlanTechniqueUnit.PieceRerouteRoute if meaningKind == "PieceRoute" =>
        pieceRouteViewReady(detail, objectSignatures, claimMove) &&
          detailOwnsClaimMove(detail, objectSignatures, claimMove) &&
          (!currentMoveNegativeStructuralHook(detail, objectSignatures, claimMove) || explicitNegativeRouteCarrier(detail))
      case PositionPlanTechniqueUnit.PieceRerouteRoute =>
        generalDetailOwnsClaimMove(detail, objectSignatures, claimMove) &&
          (!currentMoveNegativeStructuralHook(detail, objectSignatures, claimMove) || explicitNegativeActivityCarrier(detail))
      case PositionPlanTechniqueUnit.StructuralTransformation if meaningKind == "PieceActivity" =>
        generalDetailOwnsClaimMove(detail, objectSignatures, claimMove) &&
          (!currentMoveNegativeStructuralHook(detail, objectSignatures, claimMove) || explicitNegativeActivityCarrier(detail))
      case PositionPlanTechniqueUnit.SpacePreventionResourceDenial | PositionPlanTechniqueUnit.StructuralTransformation =>
        currentMoveFunctionalDetailProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
      case PositionPlanTechniqueUnit.PlanOptionSet =>
        planContinuityCurrentMoveFunctionalProof(evidenceGraph, detail, objectSignatures, claimMove, positionFen, currentMoveClaim)
      case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
        endgameTechniqueViewShape(detail, objectSignatures, claimMove) &&
          detailOwnsClaimMove(detail, objectSignatures, claimMove)
      case _ =>
        detailOwnsClaimMove(detail, objectSignatures, claimMove)

  private def planContinuityCurrentMoveFunctionalProof(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String,
      positionFen: String,
      currentMoveClaim: Boolean
  ): Boolean =
    val ownsMove =
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) ||
        detail.defenseMove.exists(move => sameMove(move, claimMove))
    val ownsCurrentMoveSource =
      currentMoveClaim &&
        currentMoveCarrierSourceOwnsClaimMove(evidenceGraph, detail, claimMove)
    val planSignal =
      detail.axisKind.contains(StrategicAxisKind.PlanCoherence) ||
        detail.matchedPlanIds.nonEmpty ||
        detail.referencePlanIds.nonEmpty ||
        detail.candidatePlanIds.nonEmpty ||
        detail.semanticAnchorKeys.exists(anchor =>
          anchor.startsWith("Plan:") ||
            anchor.startsWith("PlanPressure:")
        )
    val concretePlanHook =
      (
        detail.structuralPurposeSubjects.exists(concreteSubject) &&
          detail.structuralRouteMove.exists(move => sameMove(move, claimMove))
      ) ||
        planContinuityCurrentMoveBreakOption(detail, claimMove, positionFen) ||
        planContinuityCurrentMoveDevelopmentOption(detail, claimMove)
    ownsMove && ownsCurrentMoveSource && planSignal && concretePlanHook && !currentMoveNegativeStructuralHook(detail, objectSignatures, claimMove)

  private def planContinuityCurrentMoveBreakOption(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    planContinuityBreakOptionDetail(detail) &&
      pawnMoveFromPawn(positionFen, claimMove) &&
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove))

  private def planContinuityCurrentMoveDevelopmentOption(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    planContinuityDevelopmentOptionDetail(detail) &&
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove))

  private def planContinuityBreakOptionDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.breakFile.exists(_.trim.nonEmpty) &&
      planContinuityTextTokens(detail).exists(token =>
        token.contains("pawnbreakpreparation") ||
          token.contains("pawn-break-preparation") ||
          token.contains("breakpreparation") ||
          token.contains("centerbreak") ||
          token.contains("center-break")
      )

  private def planContinuityDevelopmentOptionDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.structuralRouteMove.nonEmpty &&
      detail.structuralPurposeSubjects.exists(subject =>
        val normalized = subject.toLowerCase
        normalized.contains("bishop") ||
          normalized.contains("knight") ||
          normalized.contains("rook") ||
          normalized.contains("queen")
      )

  private def planContinuityTextTokens(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    (
      detail.semanticAnchorKeys ++
        detail.referencePlanIds ++
        detail.candidatePlanIds ++
        detail.matchedPlanIds ++
        detail.planAlignmentReasonCodes ++
        detail.structuralPurposeSubjects ++
        detail.structuralPurposeConsequences ++
        detail.structuralPurposeCategories
    ).map(_.toLowerCase)

  private def currentMoveNegativeStructuralHook(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val currentMoveSignatures =
      objectSignatures.filter(signature => moveTokens(List(signature)).contains(normalizedClaimMove))
    val routeSignatures =
      if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
          pieceRouteQualifiedCarrierForMove(detail, objectSignatures, claimMove)
      then currentMoveSignatures.filter(pieceRouteObjectSignature)
      else Nil
    val signatures =
      if routeSignatures.nonEmpty then routeSignatures
      else if currentMoveSignatures.nonEmpty then currentMoveSignatures
      else objectSignatures
    detail.axisPolarity.exists(negativePolarity) ||
      detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession) ||
      detail.structuralPurposePolarities.exists(negativeStructuralToken) ||
      detail.structuralPurposeConsequences.exists(negativeStructuralToken) ||
      signatures.exists(signature =>
        EvidenceObjectBinding.signatureTokens(List(signature), "consequence=").exists(negativeStructuralToken) ||
          EvidenceObjectBinding.signatureTokens(List(signature), "mechanism=").exists(negativeStructuralToken)
      )

  private def negativeStructuralToken(token: String): Boolean =
    val normalized = token.toLowerCase
    normalized.contains("loss") ||
      normalized.contains("concede") ||
      normalized.contains("concession") ||
      normalized.contains("failed") ||
      normalized.contains("developmentunsafeplacement") ||
      normalized.contains("development-unsafe") ||
      normalized.contains("unsafeplacement") ||
      normalized.contains("unsafe-placement") ||
      normalized.contains("developmentpieceretreated") ||
      normalized.contains("development-piece-retreated") ||
      normalized.contains("retreated") ||
      normalized.contains("targetpressurerelease") ||
      normalized.contains("target-pressure-release") ||
      normalized.contains("centercontrolloss") ||
      normalized.contains("center-control-loss")

  private def causeFrameOwnsMeaningClaim(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimLineRole: String,
      claimMove: String,
      claimRole: String
  ): Boolean =
    causeFrameLineOwnsClaimMove(frame, verdict, claimLineRole, claimMove) &&
      causeFrameMatchesMeaningDetail(frame, detail, objectSignatures) &&
      detailOwnsClaimMove(detail, objectSignatures, claimMove) &&
      causeFramePolarityCompatibleWithMeaning(frame, verdict, detail, claimRole)

  private def sameRootRouteOwnedCauseBridgeCandidate(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String]
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
      currentMoveRouteLineRole(detail, objectSignatures, verdict) &&
      nonLossMeaningVerdict(verdict.verdict) &&
      crossComparisonPositiveCause(frame, detail) &&
      detailCanOwnCrossComparisonCause(detail) &&
      reasonGradeCauseFrame(frame) &&
      crossComparisonOwnedRootTier(frame) &&
      causeFrameObjectOverlapsDetail(frame, objectSignatures)

  private def sameRootPlanOwnedCauseBridgeCandidate(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
      nonLossMeaningVerdict(verdict.verdict) &&
      planFallbackReasonFrame(detail, frame) &&
      planCauseFrameOverlapsDetail(frame, detail)

  private def sameRootRouteOwnedCauseBridge(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimLineRole: String,
      claimMove: String,
      claimRole: String
  ): Boolean =
    sameRootRouteOwnedCauseBridgeCandidate(frame, verdict, detail, objectSignatures) &&
      causeFrameLineOwnsClaimMove(frame, verdict, claimLineRole, claimMove) &&
      detailOwnsClaimMove(detail, objectSignatures, claimMove) &&
      causeFramePolarityCompatibleWithMeaning(frame, verdict, detail, claimRole)

  private def sameRootPlanOwnedCauseBridge(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      claimLineRole: String,
      claimMove: String,
      claimRole: String
  ): Boolean =
    sameRootPlanOwnedCauseBridgeCandidate(frame, verdict, detail) &&
      causeFrameLineOwnsClaimMove(frame, verdict, claimLineRole, claimMove) &&
      causeFramePolarityCompatibleWithMeaning(frame, verdict, detail, claimRole)

  private def planCauseFrameOverlapsDetail(
      frame: MoveJudgmentCauseFrame,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    val detailTokens = planMeaningTokens(
      detail.matchedPlanIds ++
        detail.referencePlanIds ++
        detail.candidatePlanIds ++
        detail.label.toList ++
        detail.axisKey.toList ++
        detail.semanticAnchorKeys
    )
    val frameTokens = planMeaningTokens(frame.proofStrategicAxisKeys ++ frame.objectBindingSignatures)
    detailTokens.intersect(frameTokens).nonEmpty

  private def planMeaningTokens(values: List[String]): Set[String] =
    values
      .flatMap(_.split("[^A-Za-z0-9]+").toList)
      .map(_.trim.toLowerCase)
      .filter(token => token.length > 3)
      .filterNot(token =>
        token == "plan" ||
          token == "support" ||
          token == "plancoherence" ||
          token == "planpressure" ||
          token == "strategicaxis"
      )
      .toSet

  private def causeFrameLineOwnsClaimMove(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      claimLineRole: String,
      claimMove: String
  ): Boolean =
    val candidateMove = JudgmentSubjectBinding.normalizeMove(verdict.candidateLine.rootMove)
    val referenceMove = JudgmentSubjectBinding.normalizeMove(verdict.referenceLine.rootMove)
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove)
    val frameRootMatches = sameMove(frame.eventRootMove, claimMove)
    val exactSameMove = candidateMove == referenceMove && normalizedClaimMove == candidateMove
    if exactSameMove then
      claimLineRole match
        case "candidate" =>
          frame.causeSourceSide == RelativeCauseSourceSide.Candidate &&
            (frame.eventLine == verdict.candidateLine || frame.eventLine == verdict.referenceLine) &&
            frameRootMatches
        case "reference" =>
          frame.causeSourceSide == RelativeCauseSourceSide.Reference &&
            (frame.eventLine == verdict.referenceLine || frame.eventLine == verdict.candidateLine) &&
            frameRootMatches
        case "contrast" =>
          frameRootMatches &&
            (
              (frame.causeSourceSide == RelativeCauseSourceSide.Candidate && frame.eventLine == verdict.candidateLine) ||
                (frame.causeSourceSide == RelativeCauseSourceSide.Reference && frame.eventLine == verdict.referenceLine)
            )
        case _ =>
          false
    else
      claimLineRole match
        case "candidate" =>
          frame.causeSourceSide == RelativeCauseSourceSide.Candidate &&
            frame.eventLine == verdict.candidateLine &&
            frameRootMatches
        case "reference" =>
          frame.causeSourceSide == RelativeCauseSourceSide.Reference &&
            frame.eventLine == verdict.referenceLine &&
            frameRootMatches
        case "contrast" =>
          frameRootMatches &&
            (
              (frame.causeSourceSide == RelativeCauseSourceSide.Candidate && frame.eventLine == verdict.candidateLine) ||
                (frame.causeSourceSide == RelativeCauseSourceSide.Reference && frame.eventLine == verdict.referenceLine)
            )
        case _ =>
          false

  private def causeFrameMatchesMeaningDetail(
      frame: MoveJudgmentCauseFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String]
  ): Boolean =
    detail.causeEvidenceIds.exists(frame.causeEvidenceIds.contains) &&
      sameComparisonCauseMatchesDetail(frame, detail) &&
      (
        causeFrameObjectOverlapsDetail(frame, objectSignatures) ||
          sameRootCounterBreakCauseMatchesDetail(frame, detail)
      )

  private def sameRootCounterBreakCauseMatchesDetail(
      frame: MoveJudgmentCauseFrame,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    frame.causeKind == RelativeCauseKind.OpponentRestriction &&
      detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial &&
      detail.axisKey.exists(counterBreakAxisKey) &&
      frame.proofStrategicAxisKeys.exists(axisKey => detail.axisKey.exists(_.equalsIgnoreCase(axisKey))) &&
      detail.counterBreakFiles.exists(_.trim.nonEmpty) &&
      detail.structuralRouteMove.exists(move => sameMove(move, frame.eventRootMove)) &&
      (
        detail.breakFile.exists(_.trim.nonEmpty) ||
          detail.tensionEdges.nonEmpty ||
          detail.tensionSquares.nonEmpty ||
          detail.structuralPurposeSubjects.exists(pawnBreakTensionSubject)
      )

  private def opponentRestrictionSpaceDetailCanOwn(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.axisKey.exists(counterBreakAxisKey) ||
      detail.structuralPurposeSubjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject) ||
      counterplayRestraintCarrierDetail(detail)

  private def counterplayRestraintCarrierDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial &&
      detail.axisKind.contains(StrategicAxisKind.Counterplay) &&
      detail.axisPolarity.contains(StrategicAxisPolarity.Restrain) &&
      detail.structuralRouteMove.nonEmpty &&
      resourceDetailHasConcreteCarrier(detail)

  private def counterBreakAxisKey(axisKey: String): Boolean =
    val normalized = axisKey.toLowerCase
    normalized.equals("counterplay:restrain:defensive-counter-break-c") ||
      normalized.contains("counterplay:restrain:defensive-counter-break-")

  private def sameComparisonCauseMatchesDetail(
      frame: MoveJudgmentCauseFrame,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    frame.causeKind match
      case RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss =>
        (
          detail.axisKind.contains(StrategicAxisKind.Activity) &&
            detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute
        ) ||
          (
            detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
              structuralOpenCenterDevelopmentRoute(detail)
          )
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease | RelativeCauseKind.PawnWeaknessTarget =>
        detail.axisKind.contains(StrategicAxisKind.Target) &&
          (
            detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
              detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
              detail.unit == PositionPlanTechniqueUnit.CompensationSource
          )
      case RelativeCauseKind.CenterControlGain =>
        detail.axisKind.contains(StrategicAxisKind.SpaceCenter) &&
          (
            detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
              detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
              detail.unit == PositionPlanTechniqueUnit.StructuralTransformation
          )
      case RelativeCauseKind.PawnBreakOpportunity =>
        detail.axisKind.contains(StrategicAxisKind.PawnBreak) &&
          (
            detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
              counterplayRacePawnBreakDetail(detail) ||
              detail.unit == PositionPlanTechniqueUnit.StructuralTransformation
          )
      case RelativeCauseKind.OpponentRestriction =>
        detail.axisKind.contains(StrategicAxisKind.Counterplay) &&
          (
            (detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial && opponentRestrictionSpaceDetailCanOwn(detail)) ||
              detail.unit == PositionPlanTechniqueUnit.CounterplayRace
          )
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        detail.axisKind.contains(StrategicAxisKind.PlanCoherence) ||
          detail.unit == PositionPlanTechniqueUnit.PlanOptionSet ||
          (frame.causeKind == RelativeCauseKind.PlanImprovement &&
            (counterplayRestraintCarrierDetail(detail) || planImprovementConcreteCarrierDetail(detail, frame)))
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.StrategicConcession =>
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
          detail.unit == PositionPlanTechniqueUnit.CompensationSource
      case _ =>
        ClaimEventCluster.kindForCause(frame.causeKind).nonEmpty

  private def detailOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val actorMoves = moveTokens(objectSignatures)
    if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute then
      pieceRouteOwnsClaimMove(detail, claimMove)
    else if detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute then
      pawnBreakOwnsClaimMove(detail, claimMove)
    else if actorMoves.nonEmpty && !actorMoves.contains(normalizedClaimMove) then false
    else
      detail.unit match
        case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
          resourceDetailOwnsClaimMove(detail, objectSignatures, claimMove)
        case PositionPlanTechniqueUnit.CounterplayRace =>
          counterplayRaceOwnsClaimMove(detail, objectSignatures, claimMove)
        case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
          detail.endgameTechniqueTriggerMove.exists(move => sameMove(move, claimMove)) ||
            generalDetailOwnsClaimMove(detail, objectSignatures, claimMove)
        case _ =>
          generalDetailOwnsClaimMove(detail, objectSignatures, claimMove)

  private def pieceRouteOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val routeObjectSignatures = detail.objectBindingSignatures.filter(pieceRouteObjectSignature)
    val routeActorMoves = moveTokens(routeObjectSignatures)
    val routeObjectForClaimMove =
      routeObjectSignatures.exists(signature => moveTokens(List(signature)).contains(normalizedClaimMove))
    val routeContinuationForClaimMove =
      routeObjectSignatures.exists(routeContinuationSignatureOwnsClaimMove(_, claimMove))
    val routeObjectWithoutMoveActor =
      routeObjectSignatures.exists(signature => moveTokens(List(signature)).isEmpty)
    val routeSubjectForClaimMove =
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) &&
        detail.structuralPurposeSubjects.exists(qualifiedRouteSubjectToken)
    routeObjectForClaimMove ||
      routeContinuationForClaimMove ||
      routeSubjectForClaimMove ||
      (
        detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) &&
          routeActorMoves.forall(_ == normalizedClaimMove) &&
          routeObjectWithoutMoveActor
      )

  private def pieceRouteViewReady(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val routeObjectSignatures = objectSignatures.filter(pieceRouteObjectSignature)
    val routeActorMoves = moveTokens(routeObjectSignatures)
    val detailMoveOwnsClaim = detail.structuralRouteMove.exists(move => sameMove(move, claimMove))
    val currentMoveActor = routeActorMoves.contains(normalizedClaimMove)
    val currentMoveContinuation =
      routeObjectSignatures.exists(routeContinuationSignatureOwnsClaimMove(_, claimMove))
    val noBorrowedActor = routeActorMoves.isEmpty || currentMoveActor
    currentMoveContinuation || (noBorrowedActor && (currentMoveActor || detailMoveOwnsClaim))

  private def pieceRouteOwnedCauseReady(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    pieceRouteQualifiedCarrierForMove(detail, objectSignatures, claimMove) &&
      pieceRouteViewReady(detail, objectSignatures, claimMove) &&
      pieceRouteOwnsClaimMove(detail, claimMove)

  private def pawnBreakOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    val hasTensionCarrier = pawnBreakTensionCarrier(detail)
    val tensionTouchesMove =
      moveTouchesSquares(claimMove, detail.tensionSquares) ||
        moveTouchesSquares(claimMove, detail.tensionEdges) ||
        moveTouchesSquares(claimMove, detail.structuralPurposeSubjects.filter(pawnBreakTensionSubject))
    if hasTensionCarrier then tensionTouchesMove && pawnBreakFileOwnsClaimMove(detail, claimMove)
    else
      detail.breakFile match
        case Some(_) =>
          moveTouchesBreakFile(detail, claimMove)
        case None =>
          false

  private def pawnBreakFileOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    detail.breakFile.forall(file =>
      moveEndpoints(claimMove).exists { case (from, to) =>
        val normalizedFile = file.trim.toLowerCase
        from.take(1).toLowerCase == normalizedFile ||
          to.take(1).toLowerCase == normalizedFile
      }
    )

  private def pawnBreakOwnedCauseReady(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    pawnMoveFromPawn(positionFen, claimMove) &&
      pawnBreakTensionCarrier(detail) &&
      pawnBreakOwnsClaimMove(detail, claimMove) &&
      pawnBreakTensionPolicyOwnsMove(detail, claimMove)

  private def pawnBreakTensionCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.tensionSquares.nonEmpty ||
      detail.tensionEdges.nonEmpty ||
      detail.structuralPurposeSubjects.exists(pawnBreakTensionSubject)

  private def pawnBreakCurrentMoveFunctionalCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    pawnBreakConcreteTransitionCarrier(detail)

  private def pawnBreakConcreteTransitionCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.structuralPurposeSubjects.exists(pawnBreakTensionSubject) ||
      detail.tensionSquares.nonEmpty ||
      detail.tensionEdges.nonEmpty ||
      (detail.axisKey.toList ++ detail.label.toList).exists(text =>
        val normalized = text.toLowerCase
        normalized.contains("created-tension") || normalized.contains("resolved-tension")
      )

  private def pawnBreakEvidenceOwnsClaimMove(
      evidenceGraph: TypedEvidenceGraph,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val sourceOwnsMove =
      currentMoveCarrierSourceOwnsClaimMove(evidenceGraph, detail, claimMove) ||
        detail.sourceEvidenceIds.exists(id =>
          evidenceGraph.byId.get(id).exists(JudgmentSubjectBinding.sourceRecordOwnsPawnBreakMove(_, claimMove))
        )
    sourceOwnsMove ||
      moveTokens(objectSignatures).contains(normalizedMove) ||
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove))

  private def pawnMoveFromPawn(positionFen: String, claimMove: String): Boolean =
    moveEndpoints(claimMove).exists { case (from, to) =>
      val fileDelta = (to.charAt(0) - from.charAt(0)).abs
      val rankDelta = (to.charAt(1) - from.charAt(1)).abs
      val pawnLikeMove =
        (fileDelta == 0 && (rankDelta == 1 || rankDelta == 2)) ||
        (fileDelta == 1 && rankDelta == 1)
      pawnLikeMove &&
        Fen
          .read(chess.variant.Standard, Fen.Full(positionFen))
          .exists(position =>
            Square.fromKey(from).exists(square =>
              position.board.pieceAt(square).exists(_.role == Pawn)
            )
          )
    }

  private def pawnBreakTensionSubject(subject: String): Boolean =
    val normalized = subject.toLowerCase
    normalized.contains("created-tension:") ||
      normalized.contains("resolved-tension:")

  private def pawnBreakTensionPolicyOwnsMove(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    val policy = detail.tensionPolicy.map(_.toLowerCase).getOrElse("")
    if !(policy.contains("maintain") || policy.contains("preserve")) then true
    else
      moveEndpoints(claimMove).exists { case (from, to) =>
        val edges = detail.tensionEdges.map(_.toLowerCase)
        val squares = detail.tensionSquares.map(_.toLowerCase)
        val destinationStillTense =
          edges.exists(_.contains(to)) ||
            squares.contains(to)
        val movedThroughSameEdge =
          edges.exists(edge => edge.contains(from) && edge.contains(to))
        val movedAwayFromTrackedSquare =
          squares.contains(from) && !squares.contains(to)
        destinationStillTense && !movedThroughSameEdge && !movedAwayFromTrackedSquare
      }

  private def role(
      meaningKind: String,
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String,
      claimLineRole: String
  ): String =
    meaningKind match
      case "PawnBreakTiming" =>
        if pawnBreakMoveResolvesTrackedTension(detail, claimMove, positionFen) then "ReleasesPawnTension"
        else if pawnBreakMovePreservesTrackedTension(detail, claimMove, positionFen) then "PreservesTension"
        else if detail.axisPolarity.exists(negativePolarity) ||
            detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession)
        then "ReleasesPawnTension"
        else "PreparesBreak"
      case "CounterplayRace" =>
        counterplayRaceRole(detail, claimMove, positionFen, claimLineRole)
      case _ =>
        role(meaningKind, detail)

  private def counterplayRaceRole(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String,
      claimLineRole: String
  ): String =
    if claimLineRole == "reference" then "ReferenceStartsCounterplayRace"
    else if counterplayRaceNegative(detail) then "AllowsOpponentCounterplayRace"
    else if counterplayRaceDelayDetail(detail, claimMove, positionFen) then "DelaysOpponentCounterplayRace"
    else if counterplayRaceOwnRaceLead(detail, claimMove) then "StartsOwnCounterplayRace"
    else "StartsCounterplayRace"

  private def counterplayRaceNegative(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.axisPolarity.exists(negativePolarity) ||
      detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession) ||
      (
        PositionPlanTechniqueSemanticDetail.comparisonLossSides(detail).contains("candidate") &&
          PositionPlanTechniqueSemanticDetail
            .comparisonLossKinds(detail)
            .exists(kind => kind == "counter_break_allowed" || kind == "break_option_missed")
      )

  private def counterplayRaceDelayDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
      detail.resourceContestKinds.exists(_.equalsIgnoreCase(BoardAnchorKind.CounterplayRestraint.toString)) &&
      counterplayRaceConcreteCarrier(detail) &&
      counterplayRaceResourceTargetsOpponent(detail, claimMove, positionFen) &&
      counterplayRaceOwnRaceLead(detail, claimMove)

  private def counterplayRaceResourceTargetsOpponent(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    moveSide(positionFen, claimMove).exists { moverSide =>
      detail.resourceContestActorSide.exists(_.equalsIgnoreCase(moverSide)) &&
        detail.resourceContestTargetSide.exists(target => target.nonEmpty && !target.equalsIgnoreCase(moverSide))
    }

  private def counterplayRaceOwnRaceLead(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove)
    (
      counterplayRacePawnBreakDetail(detail) &&
        !Set(LineNodeRole.BestReference, LineNodeRole.Alternative).exists(detail.raceLeadingLineRole.contains) &&
        counterplayRacePawnBreakCarrierOwnsClaimMove(detail, normalizedClaimMove)
    ) ||
      (
        detail.raceLeadingLineRole.contains(LineNodeRole.Played) &&
          detail.raceCandidateRootMove.exists(move => sameMove(move, normalizedClaimMove))
      )

  private def pawnBreakMoveResolvesTrackedTension(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    moveEndpoints(claimMove).exists { case (from, to) =>
      detail.tensionEdges.exists(edge =>
        val edgeSquares = squareTokens(edge)
        val edgeEndpoints = edgeSquares.take(2).toSet
        edgeSquares.lengthCompare(2) >= 0 &&
          pawnTensionEdgeExists(positionFen, edgeSquares.head, edgeSquares(1)) &&
          (
            edgeEndpoints == Set(from, to) ||
              (edgeEndpoints.contains(from) && !edgeEndpoints.contains(to))
          )
      )
    }

  private def pawnBreakMovePreservesTrackedTension(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    detail.tensionPolicy.exists(policy => policy.toLowerCase.contains("maintain") || policy.toLowerCase.contains("preserve")) &&
      detail.tensionEdges.exists(edge =>
        val edgeSquares = squareTokens(edge)
        edgeSquares.lengthCompare(2) >= 0 &&
          pawnTensionEdgeExists(positionFen, edgeSquares.head, edgeSquares(1))
      ) &&
      pawnBreakTensionPolicyOwnsMove(detail, claimMove)

  private def pawnTensionEdgeExists(positionFen: String, first: String, second: String): Boolean =
    Fen
      .read(chess.variant.Standard, Fen.Full(positionFen))
      .flatMap(position =>
        for
          firstSquare <- Square.fromKey(first)
          secondSquare <- Square.fromKey(second)
          firstPiece <- position.board.pieceAt(firstSquare)
          secondPiece <- position.board.pieceAt(secondSquare)
        yield firstPiece.role == Pawn && secondPiece.role == Pawn && firstPiece.color != secondPiece.color
      )
      .contains(true)

  private def squareTokens(value: String): List[String] =
    "[a-h][1-8]".r.findAllIn(value.toLowerCase).toList

  private def resourceDetailOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val directlyOwnsMove =
      moveTokens(objectSignatures).contains(normalizedClaimMove) ||
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) ||
      detail.defenseMove.exists(move => sameMove(move, claimMove))
    if detail.prophylaxisNeeded.contains(true) || detail.threatKind.nonEmpty then directlyOwnsMove
    else
      directlyOwnsMove ||
        moveTouchesSquares(claimMove, detail.resourceContestSquares) ||
        moveTouchesFiles(claimMove, detail.resourceContestFiles)

  private def resourceDetailHasConcreteCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.resourceContestSquares.nonEmpty ||
      detail.resourceContestFiles.nonEmpty ||
      detail.structuralPurposeSubjects.exists(concreteSubject)

  private def counterplayRaceMeaningReady(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String,
      positionFen: String
  ): Boolean =
    counterplayRaceClaimReady(detail, claimMove, positionFen) &&
      counterplayRaceOrderProof(detail) &&
      counterplayRaceOwnsClaimMove(detail, objectSignatures, claimMove)

  private def counterplayRaceShapeReady(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    counterplayRaceSemanticProof(detail) &&
      counterplayRaceConcreteCarrier(detail)

  private def counterplayRaceClaimReady(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    counterplayRaceShapeReady(detail) &&
      (
        !counterplayRacePawnBreakDetail(detail) ||
          counterplayRacePawnBreakReady(detail, claimMove, positionFen)
      )

  private def counterplayRaceSemanticProof(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
      !counterplayRaceGenericRestraint(detail) &&
      (counterplayRaceDynamicThreat(detail) || counterplayRaceLineProof(detail))

  private def counterplayRaceDynamicThreat(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    counterplayRaceText(detail).contains("dynamic-counterplay-race") &&
      detail.threatKind.nonEmpty &&
      detail.turnsToImpact.nonEmpty

  private def counterplayRaceLineProof(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    !counterplayRaceText(detail).contains("king-safety-concession") &&
      (
        detail.raceLeadingLineRole.nonEmpty ||
          (detail.raceCandidateRootMove.nonEmpty && detail.raceReferenceRootMove.nonEmpty) ||
          (
            detail.axisKind.contains(StrategicAxisKind.Counterplay) &&
              detail.contrastOutcome.nonEmpty &&
              (detail.candidateEvidenceIds.nonEmpty || detail.referenceEvidenceIds.nonEmpty) &&
              (detail.raceCandidateRootMove.nonEmpty || detail.raceReferenceRootMove.nonEmpty)
          )
      )

  private def counterplayRaceOrderProof(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    counterplayRaceDynamicThreat(detail) ||
      detail.raceLeadingLineRole.nonEmpty ||
      (detail.raceCandidateRootMove.nonEmpty && detail.raceReferenceRootMove.nonEmpty)

  private def counterplayRaceGenericRestraint(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.threatKind.isEmpty &&
      !counterplayRacePawnBreakDetail(detail) &&
      detail.resourceContestKinds.exists(_.equalsIgnoreCase(BoardAnchorKind.CounterplayRestraint.toString)) &&
      !counterplayRaceText(detail).contains("race")

  private def counterplayRaceText(detail: PositionPlanTechniqueSemanticDetail): String =
    (
      detail.axisKey.toList ++
        detail.label.toList ++
        detail.semanticAnchorKeys ++
        detail.structuralMotifTags
    ).mkString(" ").toLowerCase

  private def counterplayRaceConcreteCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.resourceContestSquares.nonEmpty ||
      detail.resourceContestFiles.nonEmpty ||
      detail.breakFile.nonEmpty ||
      detail.structuralPurposeSubjects.exists(concreteSubject) ||
      (counterplayRaceDynamicThreat(detail) && detail.defenseMove.nonEmpty)

  private def counterplayRaceOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    if counterplayRacePawnBreakDetail(detail) then
      counterplayRacePawnBreakCarrierOwnsClaimMove(detail, claimMove)
    else
      counterplayRaceLineLeadOwnsClaimMove(detail, claimMove) &&
        (
          resourceDetailOwnsClaimMove(detail, objectSignatures, claimMove) ||
            detail.raceCandidateRootMove.exists(move => sameMove(move, claimMove)) ||
            detail.raceReferenceRootMove.exists(move => sameMove(move, claimMove))
        )

  private def counterplayRacePawnBreakDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
      detail.breakFile.exists(_.trim.nonEmpty) &&
      detail.counterBreakFiles.exists(_.trim.nonEmpty)

  private def counterplayRacePawnBreakReady(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String,
      positionFen: String
  ): Boolean =
    pawnMoveFromPawn(positionFen, claimMove) &&
      pawnBreakConcreteTransitionCarrier(detail) &&
      counterplayRaceDistinctBreakFiles(detail)

  private def counterplayRaceDistinctBreakFiles(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    val breakFile = detail.breakFile.map(counterplayRaceFileToken)
    breakFile.exists(file =>
      detail.counterBreakFiles.map(counterplayRaceFileToken).exists(counterFile => counterFile.nonEmpty && counterFile != file)
    )

  private def counterplayRaceFileToken(value: String): String =
    value.trim.toLowerCase.take(1)

  private def counterplayRacePawnBreakCarrierOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    moveEndpoints(claimMove).exists { case (_, to) =>
      val raceFiles = (detail.breakFile.toList ++ detail.counterBreakFiles).map(counterplayRaceFileToken).filter(_.nonEmpty)
      raceFiles.exists(file =>
        detail.tensionEdges.exists(edge =>
          val edgeSquares = squareTokens(edge)
          edgeSquares.contains(to) &&
            to.take(1) == file
        ) ||
          detail.tensionSquares.exists(square =>
            square.toLowerCase == to &&
              square.toLowerCase.take(1) == file
          )
      )
    }

  private def counterplayRaceLineLeadOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    detail.raceLeadingLineRole match
      case Some(LineNodeRole.Played) =>
        detail.raceCandidateRootMove.exists(move => sameMove(move, claimMove))
      case Some(LineNodeRole.BestReference) | Some(LineNodeRole.Alternative) =>
        detail.raceReferenceRootMove.exists(move => sameMove(move, claimMove))
      case Some(LineNodeRole.Threat) =>
        false
      case None =>
        false

  private def generalDetailOwnsClaimMove(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    moveTokens(objectSignatures).contains(normalizedClaimMove) ||
      detail.structuralRouteMove.exists(move => sameMove(move, claimMove)) ||
      detail.raceCandidateRootMove.exists(move => sameMove(move, claimMove)) ||
      detail.raceReferenceRootMove.exists(move => sameMove(move, claimMove))

  private def moveTokens(signatures: List[String]): Set[String] =
    EvidenceObjectBinding.signatureTokens(signatures, "actor=Move:")
      .map(part => JudgmentSubjectBinding.normalizeMove(part.stripPrefix("actor=Move:")).toLowerCase)

  private def witnessMoveTokens(signatures: List[String]): Set[String] =
    EvidenceObjectBinding.signatureTokens(signatures, "witness=Move:")
      .map(part => JudgmentSubjectBinding.normalizeMove(part.stripPrefix("witness=Move:")).toLowerCase)

  private def moveTouchesBreakFile(
      detail: PositionPlanTechniqueSemanticDetail,
      claimMove: String
  ): Boolean =
    detail.breakFile.exists(file => moveTouchesFiles(claimMove, List(file)))

  private def moveTouchesSquares(
      claimMove: String,
      squaresOrEdges: List[String]
  ): Boolean =
    moveEndpoints(claimMove).exists { case (from, to) =>
      val normalized = squaresOrEdges.map(_.toLowerCase)
      normalized.exists(token => token.contains(from) || token.contains(to))
    }

  private def moveTouchesFiles(
      claimMove: String,
      files: List[String]
  ): Boolean =
    moveEndpoints(claimMove).exists { case (from, to) =>
      val moveFiles = Set(from.take(1), to.take(1))
      files.map(_.toLowerCase.trim).exists(file => moveFiles.contains(file.take(1)))
    }

  private def moveEndpoints(move: String): Option[(String, String)] =
    val normalized = JudgmentSubjectBinding.normalizeMove(move).toLowerCase
    Option.when(normalized.matches("[a-h][1-8][a-h][1-8].*"))(
      normalized.take(2) -> normalized.slice(2, 4)
    )

  private def moveSide(positionFen: String, move: String): Option[String] =
    moveEndpoints(move).flatMap { case (from, _) =>
      Fen
        .read(chess.variant.Standard, Fen.Full(positionFen))
        .flatMap(position =>
          Square
            .fromKey(from)
            .flatMap(square =>
              position.board
                .pieceAt(square)
                .map(piece => if piece.color.white then "white" else "black")
            )
        )
    }

  private def surfaceObjectBindingSignatures(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): List[String] =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val currentMoveSignatures =
      objectSignatures.filter(signature => moveTokens(List(signature)).contains(normalizedClaimMove))
    val noMoveActorSignatures =
      objectSignatures.filter(signature => moveTokens(List(signature)).isEmpty)
    val surface =
      detail.unit match
        case PositionPlanTechniqueUnit.PieceRerouteRoute =>
          val detailMoveOwnsClaim = detail.structuralRouteMove.exists(move => sameMove(move, claimMove))
          val routeSignatures =
            objectSignatures.filter(signature =>
              pieceRouteObjectSignature(signature) &&
                (
                  moveTokens(List(signature)).contains(normalizedClaimMove) ||
                    (moveTokens(List(signature)).isEmpty && detailMoveOwnsClaim)
                )
            )
          if routeSignatures.nonEmpty then routeSignatures
          else if currentMoveSignatures.nonEmpty then currentMoveSignatures
          else Nil
        case PositionPlanTechniqueUnit.CounterplayRace =>
          val raceSignatures =
            objectSignatures.filter(signature =>
              moveTokens(List(signature)).contains(normalizedClaimMove) ||
                moveTokens(List(signature)).isEmpty
            )
          if raceSignatures.nonEmpty then raceSignatures
          else if currentMoveSignatures.nonEmpty then currentMoveSignatures
          else Nil
        case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
          val pawnSignatures =
            objectSignatures.filter(signature =>
              moveTokens(List(signature)).contains(normalizedClaimMove)
            )
          if pawnSignatures.nonEmpty then pawnSignatures else noMoveActorSignatures
        case _ =>
          if currentMoveSignatures.nonEmpty then currentMoveSignatures
          else noMoveActorSignatures
    if surface.nonEmpty then surface.distinct.sorted
    else if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
        detail.unit == PositionPlanTechniqueUnit.CounterplayRace
    then Nil
    else objectSignatures

  private def causeFramePolarityCompatibleWithMeaning(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      claimRole: String
  ): Boolean =
    val positiveRole = positiveMeaningRole(claimRole)
    val decisiveBadMoveEvent =
      lossVerdict(verdict.verdict) &&
        decisiveBadMoveCauseKind(frame.causeKind)
    val negativeDetail =
      detail.axisPolarity.exists(negativePolarity) ||
        detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession) ||
        (
          detail.axisPolarity.isEmpty &&
            detail.structuralPurposePolarities.exists(token =>
              val normalized = token.toLowerCase
              normalized.contains("loss") || normalized.contains("concede") || normalized.contains("release")
            )
        )
    !(positiveRole && (negativeCauseKind(frame.causeKind) || decisiveBadMoveEvent || negativeDetail))

  private[judgment] def positiveMeaningRole(role: String): Boolean =
    role == "ImprovesPieceRoute" ||
      role == "ImprovesPieceActivity" ||
      role == "PreparesBreak" ||
      role == "PreservesTension" ||
      role == "PreventsCounterplay" ||
      role == "StartsCounterplayRace" ||
      role == "StartsOwnCounterplayRace" ||
      role == "DelaysOpponentCounterplayRace" ||
      role == "MaintainsTechnique" ||
      role == "ReachesTechnique" ||
      role == "SupportsCurrentPlan" ||
      role == "KeepsAlternativeAvailable" ||
      role == "ReferencePreservesPlan" ||
      role == "SharedCompatiblePlan" ||
      role == "PreparesBreakOption" ||
      role == "DevelopsPieceForPlan"

  private[judgment] def negativeCauseKind(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.ActivityLoss ||
      kind == RelativeCauseKind.TargetPressureRelease ||
      kind == RelativeCauseKind.StrategicConcession ||
      kind == RelativeCauseKind.MissedStrategicImprovement ||
      kind == RelativeCauseKind.PlanContradiction ||
      kind == RelativeCauseKind.KingSafetyConcession ||
      kind == RelativeCauseKind.CandidateTacticalLiability ||
      kind == RelativeCauseKind.TacticalRefutationOfPlayed ||
      kind == RelativeCauseKind.MissedTacticalResource ||
      kind == RelativeCauseKind.WrongRecapturer ||
      kind == RelativeCauseKind.WrongMoveOrder ||
      kind == RelativeCauseKind.TempoLoss

  private[judgment] def decisiveBadMoveCauseKind(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).nonEmpty

  private def detailHasSpecificObjectAxis(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.specificityTier == PositionPlanTechniqueSpecificityTier.ExactObjectAxis ||
      detail.specificityTier == PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis

  private def detailHasDirectOrContrastProof(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.proofRoles.exists(role => role == RelativeCauseProofRole.DirectProof || role == RelativeCauseProofRole.ContrastProof)

  private def detailHasAnyProofLink(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detailHasDirectOrContrastProof(detail) ||
      detail.contextProofRoles.nonEmpty ||
      detail.contrastOutcome.nonEmpty

  private def detailHasEvidenceLink(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.sourceEvidenceIds.nonEmpty ||
      detail.referenceEvidenceIds.nonEmpty ||
      detail.candidateEvidenceIds.nonEmpty ||
      detail.causeEvidenceIds.nonEmpty ||
      detail.contextCauseEvidenceIds.nonEmpty

  private def causeFrameExplainsMeaningDetail(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String]
  ): Boolean =
    causeFrameMatches(frame, verdict) ||
      eligibleCrossComparisonSupport(frame, verdict, detail, objectSignatures)

  private def eligibleCrossComparisonSupport(
      frame: MoveJudgmentCauseFrame,
      verdict: MoveJudgmentVerdictFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String]
  ): Boolean =
    nonLossMeaningVerdict(verdict.verdict) &&
      crossComparisonPositiveCause(frame, detail) &&
      detailCanOwnCrossComparisonCause(detail) &&
      detail.causeEvidenceIds.exists(frame.causeEvidenceIds.contains) &&
      frame.hasOwnedAdmissibleLongTermProof &&
      frame.concreteObjectReady &&
      crossComparisonOwnedRootTier(frame) &&
      causeFrameObjectOverlapsDetail(frame, objectSignatures)

  private def crossComparisonOwnedRootTier(frame: MoveJudgmentCauseFrame): Boolean =
    frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot ||
      frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot

  private def nonLossMeaningVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict == MoveChoiceVerdict.MatchesReference ||
      verdict == MoveChoiceVerdict.ImprovesOnReference

  private def crossComparisonPositiveCause(
      frame: MoveJudgmentCauseFrame,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    ClaimEventCluster.kindForCause(frame.causeKind).isEmpty &&
      crossComparisonCauseMatchesDetail(frame.causeKind, detail)

  private def crossComparisonCauseMatchesDetail(
      causeKind: RelativeCauseKind,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    causeKind match
      case RelativeCauseKind.ActivityGain =>
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
          (
            detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
              structuralOpenCenterDevelopmentRoute(detail)
          )
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.PawnWeaknessTarget =>
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
          detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
          detail.unit == PositionPlanTechniqueUnit.CompensationSource
      case RelativeCauseKind.CenterControlGain =>
        detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
          detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
          detail.unit == PositionPlanTechniqueUnit.StructuralTransformation
      case RelativeCauseKind.PawnBreakOpportunity =>
        detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
          counterplayRacePawnBreakDetail(detail) ||
          detail.unit == PositionPlanTechniqueUnit.StructuralTransformation
      case RelativeCauseKind.OpponentRestriction =>
        detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial ||
          detail.unit == PositionPlanTechniqueUnit.CounterplayRace
      case _ =>
        false

  private def detailCanOwnCrossComparisonCause(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit != PositionPlanTechniqueUnit.PlanOptionSet &&
      !detail.axisKind.contains(StrategicAxisKind.PlanCoherence) &&
      detailHasSpecificObjectAxis(detail) &&
      detailHasDirectOrContrastProof(detail) &&
      crossComparisonDetailHasOwnedShape(detail)

  private def crossComparisonDetailHasOwnedShape(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit match
      case PositionPlanTechniqueUnit.PieceRerouteRoute =>
        pieceRouteDetailReady(detail)
      case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
        detail.breakFile.nonEmpty ||
          detail.tensionSquares.nonEmpty ||
          detail.tensionEdges.nonEmpty ||
          detail.counterBreakFiles.nonEmpty
      case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
        resourceDetailHasConcreteCarrier(detail)
      case PositionPlanTechniqueUnit.CounterplayRace =>
        counterplayRaceShapeReady(detail)
      case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
        endgameTechniqueHorizonOwnedShape(detail)
      case PositionPlanTechniqueUnit.StructuralTransformation | PositionPlanTechniqueUnit.CompensationSource =>
        detail.structuralPurposeSubjects.exists(concreteSubject) ||
          detail.structuralMotifTags.nonEmpty ||
          detail.boardAnchorSignals.nonEmpty ||
          detail.breakFile.nonEmpty ||
          detail.resourceContestSquares.nonEmpty ||
          detail.resourceContestFiles.nonEmpty
      case PositionPlanTechniqueUnit.PlanOptionSet =>
        false

  private def pieceRouteDetailReady(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    val hasRouteIntent = pieceRouteQualifiedCarrier(detail, detail.objectBindingSignatures)
    hasRouteIntent &&
      (
        detail.objectBindingSignatures.exists(pieceRouteObjectSignature) ||
          detail.structuralPurposeSubjects.exists(qualifiedRouteSubjectToken)
      )

  private def endgameTechniqueHorizonOwnedShape(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      detail.endgameTechniquePattern.nonEmpty &&
      detail.endgameTechniqueRookPattern.nonEmpty &&
      detail.endgameTechniqueHorizonStatus.exists(endgameTechniqueStatusOwnable) &&
      detail.requiredSquares.nonEmpty &&
      (detail.maintainedSquares.nonEmpty || detail.brokenSquares.nonEmpty) &&
      endgameTechniqueHasLineWitness(detail)

  private def endgameTechniqueHorizonViewShape(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      detail.endgameTechniquePattern.nonEmpty &&
      detail.endgameTechniqueRookPattern.nonEmpty &&
      detail.endgameTechniqueHorizonStatus.nonEmpty &&
      detail.requiredSquares.nonEmpty &&
      (detail.maintainedSquares.nonEmpty || detail.brokenSquares.nonEmpty) &&
      endgameTechniqueHasLineWitness(detail)

  private def endgameTechniqueHasLineWitness(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.endgameTechniqueTriggerMove.nonEmpty ||
      detail.endgameTechniqueEntryPlyOffset.nonEmpty ||
      detail.endgameTechniqueTerminalPlyOffset.nonEmpty

  private def endgameTechniqueOwnedShape(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    endgameTechniqueHorizonOwnedShape(detail) ||
      (
        detail.proofRoles.exists(_ == RelativeCauseProofRole.DirectProof) &&
          endgameTechniqueRootDefenderMoveShape(detail, objectSignatures, claimMove)
      )

  private def endgameTechniqueViewShape(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    endgameTechniqueHorizonViewShape(detail) ||
      endgameTechniqueRootDefenderMoveShape(detail, objectSignatures, claimMove)

  private def endgameTechniqueRootDefenderMoveShape(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      detail.causeEvidenceIds.nonEmpty &&
      objectSignatures.exists(signature =>
        val signatureList = List(signature)
        signature.toLowerCase.contains("mechanism=mechanism:defendermove") &&
          moveTokens(signatureList).contains(normalizedClaimMove) &&
          EvidenceObjectBinding.signatureTokens(signatureList, "target=").exists(EvidenceObjectBinding.concreteTargetToken)
      )

  private def endgameTechniqueStatusOwnable(status: String): Boolean =
    status != "SupersededByTactic" && status != "ContradictedByTerminalProof"

  private def terminalOverriddenEndgameTechniqueDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      detail.endgameTechniqueHorizonStatus.exists(status =>
        status == "SupersededByTactic" || status == "ContradictedByTerminalProof"
      )

  private def pieceRouteQualifiedCarrier(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String]
  ): Boolean =
    detail.structuralPurposeSubjects.exists(qualifiedRouteSubjectToken) ||
      objectSignatures.exists(qualifiedRouteObjectSignature)

  private def pieceRouteQualifiedCarrierForMove(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: String
  ): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    val detailMoveOwnsClaim = detail.structuralRouteMove.exists(move => sameMove(move, claimMove))
    val moveOwnedObjectSignatures =
      objectSignatures.filter(signature =>
        val moves = moveTokens(List(signature))
        moves.contains(normalizedClaimMove) || (moves.isEmpty && detailMoveOwnsClaim)
      )
    val routeTaggedPieceSubject =
      detail.structuralMotifTags.exists(tag => tag.equalsIgnoreCase("route") || tag.equalsIgnoreCase("reroute")) &&
        detail.structuralPurposeSubjects.exists(subject =>
          StructuralPurposeSubject.parse(subject).exists(_.isInstanceOf[StructuralPurposeSubject.PieceRoute])
        )
    moveOwnedObjectSignatures.exists(qualifiedRouteObjectSignature) ||
      objectSignatures.exists(routeContinuationSignatureOwnsClaimMove(_, claimMove)) ||
      (
        detailMoveOwnsClaim &&
          (detail.structuralPurposeSubjects.exists(qualifiedRouteSubjectToken) || routeTaggedPieceSubject)
      )

  private def routeContinuationSignatureOwnsClaimMove(signature: String, claimMove: String): Boolean =
    val normalizedClaimMove = JudgmentSubjectBinding.normalizeMove(claimMove).toLowerCase
    moveEndpoints(claimMove).exists { case (_, to) =>
      val normalized = signature.toLowerCase
      witnessMoveTokens(List(signature)).contains(normalizedClaimMove) &&
        normalized.contains(s"actor=square:$to") &&
        normalized.contains("actor=piece:") &&
        EvidenceObjectBinding.signatureTokens(List(signature), "target=").exists(EvidenceObjectBinding.concreteTargetToken)
    }

  private def qualifiedRouteSubjectToken(subject: String): Boolean =
    val normalized = subject.toLowerCase
    StructuralPurposeSubject.parse(normalized) match
      case Some(StructuralPurposeSubject.Outpost(_, _))       => true
      case Some(StructuralPurposeSubject.Battery(_, _, _, _)) => true
      case Some(StructuralPurposeSubject.PieceRestriction(_, _, _)) => strategicRayRestrictionToken(normalized)
      case Some(StructuralPurposeSubject.PieceRoute(_, _, _)) => qualifiedRouteToken(normalized)
      case Some(StructuralPurposeSubject.PieceSquare(_, _))   => lineUnlockRouteToken(normalized)
      case _                                                  => false

  private def qualifiedRouteObjectSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("actor=piece:") &&
      (
        normalized.contains("mechanism=mechanism:outpost") ||
        normalized.contains("mechanism=mechanism:battery") ||
          normalized.contains("mechanism=motif:kingstep") ||
          normalized.contains("mechanism=mechanism:bishop-long-diagonal") ||
          normalized.contains("mechanism=mechanism:rerouting") ||
          normalized.contains("mechanism=mechanism:improvingscope") ||
          normalized.contains("mechanism=mechanism:maneuver") ||
          (normalized.contains("mechanism=mechanism:diagonal-denial") && strategicRayRestrictionToken(normalized)) ||
          normalized.contains("mechanism=mechanism:filecontrol") ||
          normalized.contains("mechanism=mechanism:file-control") ||
          normalized.contains("mechanism=mechanism:fileaccess") ||
          normalized.contains("mechanism=mechanism:file-access") ||
          normalized.contains("mechanism=mechanism:fileoccupation") ||
          normalized.contains("mechanism=mechanism:file-occupation") ||
          normalized.contains("mechanism=mechanism:line-unlock") ||
          routeWeakSquareObjectSignature(normalized) ||
          normalized.contains("consequence=consequence:outpost") ||
          normalized.contains("consequence=consequence:batteryline") ||
          normalized.contains("consequence=consequence:lineunlockgain") ||
          (normalized.contains("consequence=consequence:mobilityloss") && strategicRayRestrictionToken(normalized)) ||
          normalized.contains("consequence=consequence:diagonalpressure")
      )

  private def routeWeakSquareObjectSignature(normalized: String): Boolean =
    (
      normalized.contains("mechanism=mechanism:weaksquare") ||
        normalized.contains("mechanism=mechanism:weak-square") ||
        normalized.contains("mechanism=mechanism:weaksquaretargetcreated") ||
        normalized.contains("mechanism=mechanism:weak-square-target-created") ||
        normalized.contains("consequence=consequence:weaksquare") ||
        normalized.contains("consequence=consequence:weak-square") ||
        normalized.contains("consequence=consequence:weaksquaretargetcreated") ||
        normalized.contains("consequence=consequence:weak-square-target-created")
    ) &&
      normalized.contains("target=square:")

  private def qualifiedRouteToken(token: String): Boolean =
    val normalized = token.toLowerCase
    normalized.contains("outpost") ||
      normalized.contains("battery") ||
      strategicRayRestrictionToken(normalized) ||
      normalized.contains("maneuver") ||
      normalized.contains("filecontrol") ||
      normalized.contains("file-control") ||
      normalized.contains("fileaccess") ||
      normalized.contains("file-access") ||
      normalized.contains("fileoccupation") ||
      normalized.contains("file-occupation") ||
      normalized.contains("weak-square") ||
      normalized.contains("weaksquare") ||
      normalized.contains("rook-lift")

  private def pieceRouteObjectSignature(signature: String): Boolean =
    val normalized = signature.toLowerCase
    val concretePieceDestination =
      normalized.contains("actor=piece:") &&
        (normalized.contains("target=square:") || normalized.matches(".*target=[^|]*[a-h][1-8].*"))
    concretePieceDestination ||
      (
        normalized.contains("actor=piece:") &&
          routeMechanismToken(signature)
      )

  private def routeMechanismToken(signature: String): Boolean =
    val normalized = signature.toLowerCase
    normalized.contains("rerouting") ||
      normalized.contains("maneuver") ||
      normalized.contains("mechanism=motif:kingstep") ||
      normalized.contains("battery") ||
      normalized.contains("bishop-long-diagonal") ||
      strategicRayRestrictionToken(normalized) ||
      normalized.contains("diagonalpressure") ||
      normalized.contains("line-unlock") ||
      (normalized.contains("mobilityloss") && strategicRayRestrictionToken(normalized)) ||
      routeWeakSquareObjectSignature(normalized)

  private def lineUnlockRouteToken(normalized: String): Boolean =
    normalized.contains(":line-unlock:by:") && normalized.contains(":mobility+")

  private def strategicRayRestrictionToken(normalized: String): Boolean =
    normalized.matches(".*bishop:(g7|b7|g2|b2):diagonal-denial:blocked-by:[c-f][45]:locked-center:mobility-[0-9]+-to-[0-9]+.*")

  private def sameMove(left: String, right: String): Boolean =
    JudgmentSubjectBinding.normalizeMove(left) == JudgmentSubjectBinding.normalizeMove(right)

  private def causeFrameObjectOverlapsDetail(
      frame: MoveJudgmentCauseFrame,
      objectSignatures: List[String]
  ): Boolean =
    val frameTargets =
      EvidenceObjectBinding.signatureTokens(frame.objectBindingSignatures, "target=").filter(EvidenceObjectBinding.concreteTargetToken)
    val detailTargets =
      EvidenceObjectBinding.signatureTokens(objectSignatures, "target=").filter(EvidenceObjectBinding.concreteTargetToken)
    val sharedTargets = frameTargets.intersect(detailTargets)
    if sharedTargets.nonEmpty then true
    else
      val frameHasSpecificTarget = frameTargets.nonEmpty || EvidenceObjectBinding.playerFacingReadySignatures(frame.objectBindingSignatures)
      val detailHasSpecificTarget = detailTargets.nonEmpty || EvidenceObjectBinding.playerFacingReadySignatures(objectSignatures)
      val frameActors =
        EvidenceObjectBinding.signatureTokens(frame.objectBindingSignatures, "actor=").filter(EvidenceObjectBinding.specificActorToken)
      val detailActors =
        EvidenceObjectBinding.signatureTokens(objectSignatures, "actor=").filter(EvidenceObjectBinding.specificActorToken)
      val sharedActors = frameActors.intersect(detailActors)
      val sharedMechanisms =
        EvidenceObjectBinding.signatureTokens(frame.objectBindingSignatures, "mechanism=")
          .intersect(EvidenceObjectBinding.signatureTokens(objectSignatures, "mechanism="))
      val sharedConsequences =
        EvidenceObjectBinding.signatureTokens(frame.objectBindingSignatures, "consequence=")
          .intersect(EvidenceObjectBinding.signatureTokens(objectSignatures, "consequence="))
      !frameHasSpecificTarget && !detailHasSpecificTarget && sharedActors.nonEmpty && (sharedMechanisms.nonEmpty || sharedConsequences.nonEmpty)

  private def contextualMeaningDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.PlanOptionSet ||
      detail.axisKind.contains(StrategicAxisKind.PlanCoherence)

  private def detailHasConcreteSurfaceObject(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.requiredSquares.nonEmpty ||
      detail.maintainedSquares.nonEmpty ||
      detail.resourceContestSquares.nonEmpty ||
      detail.resourceContestFiles.nonEmpty ||
      detail.tensionSquares.nonEmpty ||
      detail.tensionEdges.nonEmpty ||
      detail.breakFile.nonEmpty ||
      detail.structuralPurposeSubjects.exists(concreteSubject) ||
      EvidenceObjectBinding.playerFacingReadySignatures(detail.objectBindingSignatures)

  private def concreteSubject(subject: String): Boolean =
    val normalized = subject.toLowerCase.trim
    normalized.matches(".*[a-h][1-8].*") ||
      normalized.contains("file") ||
      normalized.contains("diagonal") ||
      normalized.contains("pawn") ||
      normalized.contains("bishop") ||
      normalized.contains("knight") ||
      normalized.contains("rook") ||
      normalized.contains("queen")

  private def detailMatchesLine(
      frame: PositionPlanTechniqueFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      verdict: MoveJudgmentVerdictFrame,
      linkedCauseFrames: List[MoveJudgmentCauseFrame]
  ): Boolean =
    val candidateMove = JudgmentSubjectBinding.normalizeMove(verdict.candidateLine.rootMove)
    val referenceMove = JudgmentSubjectBinding.normalizeMove(verdict.referenceLine.rootMove)
    val frameMove = frame.moveUci.map(JudgmentSubjectBinding.normalizeMove)
    val candidateMoveMatches = frame.line.contains(verdict.candidateLine) && frameMove.contains(candidateMove)
    val referenceMoveMatches = frame.line.contains(verdict.referenceLine) && frameMove.contains(referenceMove)
    val candidateRootMatches =
      detail.raceCandidateRootMove.exists(move => JudgmentSubjectBinding.normalizeMove(move) == candidateMove) ||
        detail.structuralRouteMove.exists(move => JudgmentSubjectBinding.normalizeMove(move) == candidateMove)
    val referenceRootMatches =
      detail.raceReferenceRootMove.exists(move => JudgmentSubjectBinding.normalizeMove(move) == referenceMove) ||
        detail.structuralRouteMove.exists(move => JudgmentSubjectBinding.normalizeMove(move) == referenceMove)
    val contrastDetail =
      detail.contrastOutcome.nonEmpty &&
        (detail.candidateEvidenceIds.nonEmpty || detail.referenceEvidenceIds.nonEmpty)
    val causeRootMatches =
      linkedCauseFrames.exists(causeFrame =>
        detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
          causeFrame.causeKind == RelativeCauseKind.ConversionSecured &&
          causeFrame.attributionDirectProofEligible &&
          detail.proofRoles.exists(_ == RelativeCauseProofRole.DirectProof) &&
          endgameTechniqueRootDefenderMoveShape(detail, detail.objectBindingSignatures, causeFrame.eventRootMove) &&
          causeFrameMatches(causeFrame, verdict) &&
          (
            JudgmentSubjectBinding.normalizeMove(causeFrame.eventRootMove) == candidateMove ||
              JudgmentSubjectBinding.normalizeMove(causeFrame.eventRootMove) == referenceMove
          )
      )
    candidateMoveMatches || referenceMoveMatches || candidateRootMatches || referenceRootMatches || contrastDetail || causeRootMatches

  private def lineRoles(
      frame: PositionPlanTechniqueFrame,
      detail: PositionPlanTechniqueSemanticDetail,
      verdict: MoveJudgmentVerdictFrame,
      linkedCauseFrames: List[MoveJudgmentCauseFrame]
  ): List[String] =
    val graphRoles =
      linkedCauseFrames.flatMap(frame => causeFrameLineRole(frame, verdict)).distinct
    if graphRoles.nonEmpty then graphRoles
    else if frame.line.contains(verdict.candidateLine) then List("candidate")
    else if frame.line.contains(verdict.referenceLine) then List("reference")
    else if frame.scope == EvidenceScope.AfterPlayedPosition &&
        detail.structuralRouteMove.exists(move => sameMove(move, verdict.candidateLine.rootMove))
    then List("candidate")
    else if frame.scope == EvidenceScope.AfterReferencePosition &&
        detail.structuralRouteMove.exists(move => sameMove(move, verdict.referenceLine.rootMove))
    then List("reference")
    else if detail.candidateEvidenceIds.nonEmpty && detail.referenceEvidenceIds.nonEmpty then List("contrast")
    else if detail.referenceEvidenceIds.nonEmpty then List("reference")
    else if detail.candidateEvidenceIds.nonEmpty then List("candidate")
    else List("contrast")

  private def causeFrameLineRole(frame: MoveJudgmentCauseFrame, verdict: MoveJudgmentVerdictFrame): Option[String] =
    frame.causeSourceSide match
      case RelativeCauseSourceSide.Candidate =>
        Some("candidate")
      case RelativeCauseSourceSide.Reference =>
        Some("reference")
      case RelativeCauseSourceSide.Mixed | RelativeCauseSourceSide.Shared =>
        if frame.eventLine == verdict.candidateLine then Some("candidate")
        else if frame.eventLine == verdict.referenceLine then Some("reference")
        else Some("contrast")

  private def moveUci(
      verdict: MoveJudgmentVerdictFrame,
      lineRole: String
  ): String =
    if lineRole == "reference" then verdict.referenceLine.rootMove
    else if lineRole == "candidate" then verdict.candidateLine.rootMove
    else if sameMove(verdict.referenceLine.rootMove, verdict.candidateLine.rootMove) then verdict.candidateLine.rootMove
    else ""

  private[judgment] def negativePolarity(polarity: StrategicAxisPolarity): Boolean =
    polarity == StrategicAxisPolarity.Loss ||
      polarity == StrategicAxisPolarity.Release ||
      polarity == StrategicAxisPolarity.Concede

  private def kind(
      detail: PositionPlanTechniqueSemanticDetail,
      objectSignatures: List[String],
      claimMove: Option[String]
  ): Option[String] =
    val base =
      detail.unit match
        case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
          Some("PawnBreakTiming")
        case PositionPlanTechniqueUnit.PlanOptionSet =>
          Some("PlanContinuity")
        case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
          Some("TechniqueConversion")
        case PositionPlanTechniqueUnit.CounterplayRace =>
          Option.when(counterplayRaceSemanticProof(detail))("CounterplayRace")
        case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
          detail.axisKind match
            case Some(StrategicAxisKind.SpaceCenter) => Some("CenterControl")
            case _                                   => Some("CounterplayControl")
        case PositionPlanTechniqueUnit.PieceRerouteRoute =>
          val hasRouteCarrier =
            claimMove match
              case Some(move) => pieceRouteQualifiedCarrierForMove(detail, objectSignatures, move)
              case None       => pieceRouteQualifiedCarrier(detail, objectSignatures)
          if hasRouteCarrier then Some("PieceRoute")
          else Some("PieceActivity")
        case PositionPlanTechniqueUnit.StructuralTransformation =>
          if detail.terminalConsequenceKinds.exists(terminalProofConsequenceKind) then Some("TerminalProof")
          else detail.axisKind match
            case Some(StrategicAxisKind.Target)        => Some("TargetPressure")
            case _ if structuralOpenCenterDevelopmentRoute(detail) => Some("PieceActivity")
            case Some(StrategicAxisKind.SpaceCenter)   => Some("CenterControl")
            case Some(StrategicAxisKind.PawnBreak)     => Some("PawnBreakTiming")
            case Some(StrategicAxisKind.Counterplay)   => Option.when(counterplayRaceSemanticProof(detail))("CounterplayRace")
            case Some(StrategicAxisKind.Activity)      => Some("PieceActivity")
            case Some(StrategicAxisKind.PlanCoherence) => Some("PlanContinuity")
            case None                                  => Some("StructureShift")
        case PositionPlanTechniqueUnit.CompensationSource =>
          Some("Compensation")
    base

  private def role(
      meaningKind: String,
      detail: PositionPlanTechniqueSemanticDetail
  ): String =
    meaningKind match
      case "PlanContinuity" =>
        if planContinuityBreakOptionDetail(detail) then "PreparesBreakOption"
        else if planContinuityDevelopmentOptionDetail(detail) then "DevelopsPieceForPlan"
        else
          detail.contrastOutcome match
            case Some(StrategicAxisComparisonOutcome.SharedSustained) =>
              "SharedCompatiblePlan"
            case Some(StrategicAxisComparisonOutcome.ReferenceOnly | StrategicAxisComparisonOutcome.ReferenceStronger |
                StrategicAxisComparisonOutcome.ReferencePreservesPlan) =>
              "ReferencePreservesPlan"
            case _ if detail.missingPlanIds.nonEmpty =>
              "KeepsAlternativeAvailable"
            case _ =>
              "SupportsCurrentPlan"
      case "PawnBreakTiming" =>
        if pawnBreakResolutionDetail(detail)
        then "ReleasesPawnTension"
        else if detail.tensionPolicy.exists(policy => policy.toLowerCase.contains("maintain") || policy.toLowerCase.contains("preserve"))
        then "PreservesTension"
        else "PreparesBreak"
      case "CounterplayControl" =>
        if detail.axisPolarity.exists(negativePolarity) || detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession)
        then "ConcedesCounterplay"
        else "PreventsCounterplay"
      case "CounterplayRace" =>
        if detail.axisPolarity.exists(negativePolarity) || detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession)
        then "AllowsOpponentCounterplayRace"
        else "StartsCounterplayRace"
      case "PieceRoute" =>
        if outpostConcessionDetail(detail) then "ConcedesOutpost"
        else if pieceRouteConcessionDetail(detail) then "ConcedesPieceRoute"
        else "ImprovesPieceRoute"
      case "PieceActivity" =>
        if negativeStrategicDetail(detail) then "LosesPieceActivity" else "ImprovesPieceActivity"
      case "TerminalProof" =>
        terminalProofRole(detail)
      case "TechniqueConversion" =>
        endgameTechniqueRole(detail)
      case _ =>
        "ExplainsMoveFunction"

  private def endgameTechniqueRole(detail: PositionPlanTechniqueSemanticDetail): String =
    detail.endgameTechniqueHorizonStatus match
      case Some("Transitioned")                 => "ReachesTechnique"
      case Some("Failed")                       => "BreaksTechnique"
      case Some("SupersededByTactic")           => "TacticOverridesTechnique"
      case Some("ContradictedByTerminalProof")  => "TerminalProofContradictsTechnique"
      case Some("Active") | None                => "MaintainsTechnique"
      case Some(_)                              => "MaintainsTechnique"

  private def terminalProofRole(detail: PositionPlanTechniqueSemanticDetail): String =
    if detail.terminalConsequenceKinds.contains("Mate") then "ProvesMate"
    else if detail.terminalConsequenceKinds.contains("PromotionRace") then "ProvesPromotionRace"
    else if detail.terminalConsequenceKinds.contains("DrawResource") then "ProvesDrawResource"
    else if detail.terminalConsequenceKinds.contains("MaterialGain") then "ProvesMaterialGain"
    else if detail.terminalConsequenceKinds.contains("MaterialLoss") then "ProvesMaterialLoss"
    else "ProvesTerminalResult"

  private def pawnBreakResolutionDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.axisPolarity.exists(negativePolarity) ||
      detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession) ||
      detail.tensionPolicy.exists(_.toLowerCase.contains("release")) ||
      detail.label.exists(_.toLowerCase.contains("resolved-tension")) ||
      detail.axisKey.exists(_.toLowerCase.contains("resolved-tension")) ||
      detail.structuralPurposeSubjects.exists(_.toLowerCase.contains("resolved-tension"))

  private def outpostConcessionDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    explicitConsequence(detail, TransitionConsequenceKind.OutpostConcession)

  private def explicitNegativeRouteCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    outpostConcessionDetail(detail) ||
      List(
        TransitionConsequenceKind.MobilityLoss,
        TransitionConsequenceKind.FileAccessLoss,
        TransitionConsequenceKind.CenterControlLoss,
        TransitionConsequenceKind.DevelopmentMobilityLoss,
        TransitionConsequenceKind.DevelopmentCenterControlLoss,
        TransitionConsequenceKind.DevelopmentPieceRetreated
      ).exists(explicitConsequence(detail, _))

  private def explicitNegativeActivityCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    List(
      TransitionConsequenceKind.OutpostConcession,
      TransitionConsequenceKind.MobilityLoss,
      TransitionConsequenceKind.FileAccessLoss,
      TransitionConsequenceKind.TargetPressureRelease,
      TransitionConsequenceKind.CenterControlLoss
    ).exists(explicitConsequence(detail, _))

  private def explicitConsequence(
      detail: PositionPlanTechniqueSemanticDetail,
      consequence: TransitionConsequenceKind
  ): Boolean =
    detail.structuralPurposeConsequences.exists(_.equalsIgnoreCase(consequence.toString))

  private def structuralOpenCenterDevelopmentRoute(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.structuralRouteMove.nonEmpty &&
      structuralOpenCenterContext(detail) &&
      structuralDevelopmentRoute(detail)

  private def structuralCurrentMoveCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    structuralOpenCenterDevelopmentRoute(detail) ||
      (
        detail.structuralRouteMove.nonEmpty &&
          detailHasSpecificObjectAxis(detail) &&
          detail.axisKind.exists(kind =>
            kind == StrategicAxisKind.Target ||
              kind == StrategicAxisKind.SpaceCenter
          ) &&
          detail.structuralPurposeSubjects.exists(concreteSubject)
      )

  private def positiveStructuralCurrentMoveCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
      structuralCurrentMoveCarrier(detail) &&
      detail.axisPolarity.forall(polarity => !negativePolarity(polarity)) &&
      !detail.structuralPurposePolarities.exists(negativeStructuralToken) &&
      !detail.structuralPurposeConsequences.exists(negativeStructuralToken)

  private def structuralOpenCenterContext(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    val anchors = detail.semanticAnchorKeys.map(_.toLowerCase)
    val motifs = detail.structuralMotifTags.map(_.toLowerCase)
    motifs.exists(tag => tag == "iqp" || tag == "open") ||
      anchors.exists(anchor =>
        anchor == "pawnstructure:opencenter" ||
          anchor == "pawnplay:center-break" ||
          anchor.contains("openinganchor:pawnstructure:pawnbreakobserved") ||
          anchor.contains("pawnbreakpreparation")
      )

  private def structuralDevelopmentRoute(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.structuralPurposeSubjects.exists(subject =>
      StructuralPurposeSubject.parse(subject).exists {
        case _: StructuralPurposeSubject.PieceRoute => true
        case _                                     => false
      } || subject.toLowerCase.matches(".*(bishop|knight|rook|queen|king):[a-h][1-8]-[a-h][1-8].*")
    ) ||
      detail.structuralPurposeConsequences.exists(value =>
        val normalized = value.toLowerCase
        normalized.contains("development") || normalized.contains("mobilitygain")
      ) ||
      detail.structuralPurposeCategories.exists(value =>
        val normalized = value.toLowerCase
        normalized.contains("development") || normalized.contains("pieceactivity")
      )

  private def negativeStrategicDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.axisPolarity.exists(negativePolarity) ||
      detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession) ||
      detail.structuralPurposePolarities.exists(negativeStructuralToken) ||
      detail.structuralPurposeConsequences.exists(negativeStructuralToken)

  private def pieceRouteConcessionDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.axisPolarity.exists(negativePolarity) ||
      detail.contrastOutcome.contains(StrategicAxisComparisonOutcome.CandidateConcession) ||
      (
        detail.axisPolarity.forall(routeAxisAllowsPurposeConcession) &&
          (detail.structuralPurposePolarities.exists(negativeStructuralToken) ||
            detail.structuralPurposeConsequences.exists(negativeStructuralToken))
      )

  private def routeAxisAllowsPurposeConcession(polarity: StrategicAxisPolarity): Boolean =
    polarity != StrategicAxisPolarity.Gain &&
      polarity != StrategicAxisPolarity.Support &&
      polarity != StrategicAxisPolarity.Preserve &&
      polarity != StrategicAxisPolarity.Restrain

  private def laneKey(
      meaningKind: String,
      detail: PositionPlanTechniqueSemanticDetail,
      boardCarriers: List[MoveMeaningSurfaceBoardCarrier]
  ): String =
    List(
      s"kind=$meaningKind",
      detail.axisKey.map(value => s"axis=$value").getOrElse(s"axis=${detail.axisKind.map(_.toString).getOrElse("none")}"),
      s"object=${semanticObjectKey(boardCarriers)}"
    ).mkString("|")

  private def conflictKey(
      meaningKind: String,
      detail: PositionPlanTechniqueSemanticDetail,
      boardCarriers: List[MoveMeaningSurfaceBoardCarrier]
  ): Option[String] =
    Option.when(meaningKind == "PlanContinuity" || meaningKind == "PawnBreakTiming")(
      List(
        s"kind=$meaningKind",
        detail.axisKey.map(value => s"axis=$value").getOrElse(s"axis=${detail.axisKind.map(_.toString).getOrElse("none")}"),
        s"object=${semanticObjectKey(boardCarriers)}"
      ).mkString("|")
    )

  private def semanticObjectKey(
      boardCarriers: List[MoveMeaningSurfaceBoardCarrier]
  ): String =
    boardCarriers.map(carrier => s"${carrier.role}=${carrier.kind}:${carrier.value}").distinct.sorted.take(6).mkString(";") match
      case ""    => "none"
      case value => value

  private def visibility(supportLevel: String): String =
    supportLevel match
      case "owned_cause_linked" => "reason_grade"
      case "view_surfaced"      => "functional_explanation"
      case _                    => "soft_context"

  private def surfaceLane(
      detail: PositionPlanTechniqueSemanticDetail,
      verdict: MoveJudgmentVerdictFrame,
      claimLineRole: String,
      claimMove: String,
      supportLevel: String,
      objectSignatures: List[String],
      linkedCauseFrames: List[MoveJudgmentCauseFrame]
  ): String =
    val currentMove = currentMoveMeaningClaim(verdict, claimLineRole, claimMove)
    val candidateOwnedCause = linkedCauseFrames.exists(_.causeSourceSide == RelativeCauseSourceSide.Candidate)
    val terminalProofOwned = terminalProofDetailOwnsClaimMove(detail, objectSignatures, claimMove)
    if claimLineRole == "reference" then
      "reference_or_opponent_resource"
    else if supportLevel == "owned_cause_linked" && currentMove && (candidateOwnedCause || terminalProofOwned) then
      "current_move_owned"
    else if supportLevel == "view_surfaced" && currentMove && claimLineRole == "candidate" then
      "current_move_function"
    else if supportLevel == "contextual" || contextualMeaningDetail(detail) then
      "inherited_context"
    else if claimLineRole == "contrast" then
      "pv_or_line_witness"
    else
      "pv_or_line_witness"

  private def lossVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict == MoveChoiceVerdict.PlayableLoss ||
      verdict == MoveChoiceVerdict.Inaccuracy ||
      verdict == MoveChoiceVerdict.Mistake ||
      verdict == MoveChoiceVerdict.Blunder

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict == MoveChoiceVerdict.Inaccuracy ||
      verdict == MoveChoiceVerdict.Mistake ||
      verdict == MoveChoiceVerdict.Blunder

  private def sortKey(claim: MoveMeaningClaim): (Int, Int, Int, String) =
    (
      strengthRank(claim.supportLevel),
      if claim.specificityTier == PositionPlanTechniqueSpecificityTier.ExactObjectAxis then 1 else 0,
      kindRank(claim.meaningKind),
      claim.frameId
    )

  private def strengthRank(strength: String): Int =
    strength match
      case "owned_cause_linked" => 3
      case "view_surfaced"      => 2
      case "contextual"         => 1
      case _                    => 0

  private def kindRank(kind: String): Int =
    kind match
      case "TerminalProof"       => 10
      case "TechniqueConversion" => 9
      case "PawnBreakTiming"     => 8
      case "CounterplayRace"     => 7
      case "CounterplayControl"  => 7
      case "TargetPressure"      => 6
      case "CenterControl"       => 5
      case "PieceActivity"       => 4
      case "PieceRoute"          => 4
      case "Compensation"        => 3
      case "StructureShift"      => 3
      case "PlanContinuity"      => 1
      case _                     => 0

case class MoveJudgmentLocalIdeaFrame(
    ideaId: String,
    relation: IdeaVerdictRelation,
    claimIds: List[String],
    evidenceIds: List[String]
)

case class MoveJudgmentView(
    verdict: Option[MoveJudgmentVerdictFrame],
    verdictCarriers: List[MoveJudgmentClaimFrame],
    causeAudit: MoveJudgmentCauseAudit = MoveJudgmentCauseAudit(),
    positionPlanTechniqueFrames: List[PositionPlanTechniqueFrame] = Nil,
    supportContextClusterIds: List[String],
    overriddenLocalIdeas: List[MoveJudgmentLocalIdeaFrame],
    preservedLocalIdeas: List[MoveJudgmentLocalIdeaFrame],
    moveMeaningClaims: List[MoveMeaningClaim] = Nil
)

object MoveJudgmentView:
  def from(
      relativeAssessments: List[RelativeMoveAssessment],
      evidenceGraph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      claimLifecycle: List[ClaimLifecycleDiagnostic],
      ideaVerdict: Option[IdeaVerdictSplit],
      claimSupportClusters: List[ClaimSupportCluster],
      claimEventClusters: List[ClaimEventCluster]
  ): Option[MoveJudgmentView] =
    val playedMoves = relativeAssessments.map(assessment => JudgmentSubjectBinding.normalizeMove(assessment.played.moveUci)).toSet
    val claimsById = claims.map(claim => claim.id -> claim).toMap
    val clusterFrames =
      claimEventClusters.map(clusterFrame(_, evidenceGraph, claimLifecycle))
    val unframedFrames =
      unframedCauseFrames(evidenceGraph, ideas, claimLifecycle, claimEventClusters, claimSupportClusters)
    val causeFrames =
      (clusterFrames ++ unframedFrames)
        .distinctBy(frame =>
          (
            frame.clusterId,
            frame.causeEvidenceIds.mkString(","),
            frame.causeKind,
            frame.comparisonKind,
            frame.causeRole,
            frame.causeSourceSide,
            frame.causeImportance,
            frame.eventLine.id
          )
        )
    val narratedCauseFrames = MoveJudgmentCauseNarrativeProjection.withNarrativeRoles(evidenceGraph, causeFrames)
    val verdictCarrierIds =
      (claimEventClusters.flatMap(_.evaluationClaimIds) ++
        claims.filter(claim => claim.family == ClaimFamily.Evaluation && claimCarriesVerdict(claim, evidenceGraph, playedMoves)).map(_.id))
        .distinct
        .sorted
    val verdictCarriers =
      verdictCarrierIds.flatMap(claimsById.get).map(claimFrame(_, evidenceGraph, playedMoves))
    val supportContextClusterIds =
      (narratedCauseFrames.flatMap(_.relatedSupportClusterIds) ++ supportClustersRelatedTo(claimSupportClusters, narratedCauseFrames))
        .distinct
        .sorted
    val planTechniqueFrames =
      PositionPlanTechniqueProjection.frames(evidenceGraph, ideas, claims, ideaVerdict)
    val causeAudit = causeAuditBuckets(narratedCauseFrames)
    val baseView =
      MoveJudgmentView(
        verdict = relativeAssessments.headOption.map(verdictFrame),
        verdictCarriers = verdictCarriers,
        causeAudit = causeAudit,
        positionPlanTechniqueFrames = planTechniqueFrames,
        supportContextClusterIds = supportContextClusterIds,
        overriddenLocalIdeas = overriddenLocalIdeas(ideaVerdict, claims),
        preservedLocalIdeas = preservedLocalIdeas(claims, evidenceGraph, playedMoves)
      )
    val moveMeaningClaims = MoveMeaningClaim.from(evidenceGraph, baseView, narratedCauseFrames)
    val view =
      baseView.copy(
        moveMeaningClaims = moveMeaningClaims
      )
    Option.when(
      view.verdict.nonEmpty ||
        view.verdictCarriers.nonEmpty ||
        view.causeAudit.all.nonEmpty ||
        view.positionPlanTechniqueFrames.nonEmpty ||
        view.moveMeaningClaims.nonEmpty ||
        view.overriddenLocalIdeas.nonEmpty ||
        view.preservedLocalIdeas.nonEmpty
    )(view)

  private[judgment] def causeAuditBuckets(
      frames: List[MoveJudgmentCauseFrame]
  ): MoveJudgmentCauseAudit =
    val concretePeerKeys =
      frames
        .filter(causeAuditConcretePeer)
        .map(moveJudgmentCauseComparisonKey)
        .toSet
    val primaryCandidates = frames.filter(_.role == MoveJudgmentCauseFrameRole.PrimaryCause)
    val secondaryCandidates = frames.filter(_.role == MoveJudgmentCauseFrameRole.SecondaryCause)
    val weakDemoted =
      (primaryCandidates ++ secondaryCandidates)
        .filter(frame => moveJudgmentCauseDemoteWeakFrame(frame, concretePeerKeys))
        .map(moveJudgmentCauseContextFrame)
    val weakDemotedIds = weakDemoted.map(moveJudgmentCauseFrameIdentity).toSet
    val primaryAfterWeakDemotion =
      primaryCandidates.filterNot(frame => weakDemotedIds.contains(moveJudgmentCauseFrameIdentity(frame)))
    val secondaryAfterWeakDemotion =
      secondaryCandidates.filterNot(frame => weakDemotedIds.contains(moveJudgmentCauseFrameIdentity(frame)))
    val compacted = compactCauseAuditBuckets(primaryAfterWeakDemotion, secondaryAfterWeakDemotion)
    MoveJudgmentCauseAudit(
      primary = compacted.primary,
      secondary = compacted.secondary,
      context =
        (
          frames.filter(_.role == MoveJudgmentCauseFrameRole.ContextCause) ++ weakDemoted ++ compacted.context
        ).distinctBy(moveJudgmentCauseFrameIdentity)
    )

  private def compactCauseAuditBuckets(
      primaryCandidates: List[MoveJudgmentCauseFrame],
      secondaryCandidates: List[MoveJudgmentCauseFrame]
  ): MoveJudgmentCauseAudit =
    val selectedPrimaryIds =
      primaryCandidates
        .filter(_.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause)
        .groupBy(moveJudgmentCauseComparisonKey)
        .values
        .flatMap(selectCauseAuditPrimaryRoots)
        .map(moveJudgmentCauseFrameIdentity)
        .toSet
    val selectedPrimary =
      primaryCandidates.filter(frame => selectedPrimaryIds.contains(moveJudgmentCauseFrameIdentity(frame)))
    val primaryOverflow =
      primaryCandidates.filterNot(frame => selectedPrimaryIds.contains(moveJudgmentCauseFrameIdentity(frame)))
    val secondaryPool =
      secondaryCandidates ++ primaryOverflow.map(moveJudgmentCauseSecondaryFrame)
    val selectedSecondaryIds =
      selectCauseAuditSecondaryFrames(secondaryPool.filterNot(_.narrativeRole == MoveJudgmentCauseNarrativeRole.ContextCause))
        .map(moveJudgmentCauseFrameIdentity)
        .toSet
    val selectedSecondary =
      secondaryPool.filter(frame => selectedSecondaryIds.contains(moveJudgmentCauseFrameIdentity(frame)))
    val secondaryOverflow =
      secondaryPool.filterNot(frame => selectedSecondaryIds.contains(moveJudgmentCauseFrameIdentity(frame)))
    MoveJudgmentCauseAudit(
      primary = selectedPrimary,
      secondary = selectedSecondary,
      context = secondaryOverflow.map(moveJudgmentCauseContextFrame)
    )

  private def selectCauseAuditPrimaryRoots(frames: List[MoveJudgmentCauseFrame]): List[MoveJudgmentCauseFrame] =
    val representatives = frames
      .groupBy(causeAuditPrimaryCauseFamily)
      .values
      .flatMap(group => group.sortBy(causeAuditPrimaryCauseSortKey).lastOption)
      .toList
    representatives
      .filterNot(frame => causeAuditGenericRootDominated(frame, representatives))
      .sortBy(causeAuditPrimaryCauseSortKey)

  private def selectCauseAuditSecondaryFrames(frames: List[MoveJudgmentCauseFrame]): List[MoveJudgmentCauseFrame] =
    frames
      .groupBy(causeAuditSecondaryCauseFamily)
      .values
      .flatMap(group => group.sortBy(causeAuditSecondaryCauseSortKey).lastOption)
      .toList
      .sortBy(causeAuditSecondaryCauseSortKey)

  private def causeAuditPrimaryCauseSortKey(frame: MoveJudgmentCauseFrame): (Int, Int, Int, Int, Int, String) =
    (
      causeAuditCauseKindRank(frame.causeKind),
      causeAuditRootTierRank(frame.rootArbitrationTier),
      boolRank(frame.causeSourceSide == RelativeCauseSourceSide.Candidate),
      boolRank(frame.concreteObjectReady),
      frame.proofDirectSourceIds.distinct.size,
      frame.causeKind.toString
    )

  private def causeAuditSecondaryCauseSortKey(frame: MoveJudgmentCauseFrame): (Int, Int, Int, Int, Int, Int, Int, String) =
    (
      causeAuditNarrativeRoleRank(frame.narrativeRole),
      causeAuditWitnessBindingRank(frame.witnessBindingLevel),
      causeAuditComparisonRank(frame.comparisonKind),
      causeAuditSecondaryCauseKindRank(frame.causeKind),
      causeAuditRootTierRank(frame.rootArbitrationTier),
      boolRank(frame.concreteObjectReady),
      frame.proofDirectSourceIds.distinct.size,
      frame.causeKind.toString
    )

  private def causeAuditGenericRootDominated(
      frame: MoveJudgmentCauseFrame,
      roots: List[MoveJudgmentCauseFrame]
  ): Boolean =
    causeAuditGenericRoot(frame.causeKind) &&
      roots.exists(root =>
        moveJudgmentCauseFrameIdentity(root) != moveJudgmentCauseFrameIdentity(frame) &&
          causeAuditSpecificRoot(root.causeKind)
      )

  private def causeAuditGenericRoot(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.ActivityGain ||
      kind == RelativeCauseKind.ActivityLoss ||
      kind == RelativeCauseKind.CenterControlGain ||
      kind == RelativeCauseKind.StructuralImprovement

  private def causeAuditSpecificRoot(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PawnBreakOpportunity ||
      kind == RelativeCauseKind.PawnWeaknessTarget ||
      kind == RelativeCauseKind.TargetPressureGain ||
      kind == RelativeCauseKind.TargetPressureRelease ||
      kind == RelativeCauseKind.OpponentRestriction ||
      kind == RelativeCauseKind.KingSafetyConcession

  private def causeAuditPrimaryCauseFamily(frame: MoveJudgmentCauseFrame): String =
    frame.causeKind match
      case RelativeCauseKind.PawnBreakOpportunity =>
        "pawn-break"
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease | RelativeCauseKind.PawnWeaknessTarget =>
        "target"
      case RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss =>
        "activity"
      case RelativeCauseKind.CenterControlGain =>
        "center"
      case RelativeCauseKind.OpponentRestriction | RelativeCauseKind.KingSafetyConcession =>
        "restriction"
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.StrategicConcession =>
        "structure"
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        "plan"
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability |
          RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.WrongRecapturer |
          RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
        "tactical"
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity |
          RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        "defense"
      case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.MaterialSwing |
          RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured | RelativeCauseKind.SacrificeCompensation =>
        "conversion"

  private def causeAuditSecondaryCauseFamily(frame: MoveJudgmentCauseFrame): String =
    frame.causeKind match
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability |
          RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.WrongRecapturer |
          RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
        "tactical"
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity |
          RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        "defense"
      case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.MaterialSwing |
          RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured | RelativeCauseKind.SacrificeCompensation =>
        "conversion"
      case RelativeCauseKind.PawnBreakOpportunity =>
        "pawn-break"
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease | RelativeCauseKind.PawnWeaknessTarget =>
        "target"
      case RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss | RelativeCauseKind.CenterControlGain |
          RelativeCauseKind.StructuralImprovement | RelativeCauseKind.OpponentRestriction |
          RelativeCauseKind.KingSafetyConcession | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.StrategicConcession =>
        "structure"
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        "plan"

  private def causeAuditCauseKindRank(kind: RelativeCauseKind): Int =
    kind match
      case RelativeCauseKind.WrongRecapturer =>
        120
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        115
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability =>
        110
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.KingForcing =>
        105
      case RelativeCauseKind.TargetPressureRelease =>
        100
      case RelativeCauseKind.ActivityLoss =>
        98
      case RelativeCauseKind.PawnBreakOpportunity =>
        96
      case RelativeCauseKind.PawnWeaknessTarget =>
        94
      case RelativeCauseKind.OpponentRestriction | RelativeCauseKind.KingSafetyConcession =>
        92
      case RelativeCauseKind.TargetPressureGain =>
        90
      case RelativeCauseKind.ActivityGain =>
        88
      case RelativeCauseKind.CenterControlGain =>
        86
      case RelativeCauseKind.StructuralImprovement =>
        84
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        80
      case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.MaterialSwing =>
        76
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity =>
        70
      case RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        60
      case RelativeCauseKind.SacrificeCompensation =>
        55
      case RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.StrategicConcession =>
        50
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        40

  private def causeAuditSecondaryCauseKindRank(kind: RelativeCauseKind): Int =
    kind match
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability =>
        120
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.KingForcing =>
        115
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        110
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity =>
        100
      case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.MaterialSwing =>
        95
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        90
      case RelativeCauseKind.PawnBreakOpportunity =>
        86
      case RelativeCauseKind.TargetPressureRelease | RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.TargetPressureGain =>
        84
      case RelativeCauseKind.OpponentRestriction | RelativeCauseKind.KingSafetyConcession =>
        82
      case RelativeCauseKind.ActivityLoss | RelativeCauseKind.ActivityGain | RelativeCauseKind.CenterControlGain |
          RelativeCauseKind.StructuralImprovement =>
        78
      case RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        70
      case RelativeCauseKind.SacrificeCompensation =>
        65
      case RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.StrategicConcession =>
        55
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        50

  private def causeAuditRootTierRank(tier: MoveJudgmentCauseRootArbitrationTier): Int =
    tier match
      case MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot    => 4
      case MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot => 3
      case MoveJudgmentCauseRootArbitrationTier.FallbackRoot      => 2
      case MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot    => 1
      case MoveJudgmentCauseRootArbitrationTier.ContextOnly       => 0

  private def causeAuditNarrativeRoleRank(role: MoveJudgmentCauseNarrativeRole): Int =
    role match
      case MoveJudgmentCauseNarrativeRole.TacticalWitness => 4
      case MoveJudgmentCauseNarrativeRole.RootCause       => 3
      case MoveJudgmentCauseNarrativeRole.SupportingCause => 2
      case MoveJudgmentCauseNarrativeRole.ContextCause    => 0

  private def causeAuditWitnessBindingRank(level: MoveJudgmentCauseWitnessBindingLevel): Int =
    level match
      case MoveJudgmentCauseWitnessBindingLevel.Punishment         => 4
      case MoveJudgmentCauseWitnessBindingLevel.ObjectContext      => 3
      case MoveJudgmentCauseWitnessBindingLevel.LineContext        => 2
      case MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly => 1
      case MoveJudgmentCauseWitnessBindingLevel.NotWitness         => 0

  private def causeAuditComparisonRank(kind: CandidateComparisonKind): Int =
    kind match
      case CandidateComparisonKind.PlayedVsBest          => 4
      case CandidateComparisonKind.PlayedVsAlternative   => 3
      case CandidateComparisonKind.BestVsSecond          => 2
      case CandidateComparisonKind.ReferenceVsAlternative => 1

  private def boolRank(value: Boolean): Int =
    if value then 1 else 0

  private def causeAuditConcretePeer(frame: MoveJudgmentCauseFrame): Boolean =
    frame.concreteObjectReady &&
      (
        frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot ||
          frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
      )

  private def moveJudgmentCauseDemoteWeakFrame(
      frame: MoveJudgmentCauseFrame,
      concretePeerKeys: Set[(CandidateComparisonKind, LineNodeRef, LineNodeRef)]
  ): Boolean =
    concretePeerKeys.contains(moveJudgmentCauseComparisonKey(frame)) &&
      (
        frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ContextOnly ||
          frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.FallbackRoot ||
          frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot
      )

  private def moveJudgmentCauseContextFrame(frame: MoveJudgmentCauseFrame): MoveJudgmentCauseFrame =
    frame.copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      narrativeRole = MoveJudgmentCauseNarrativeRole.ContextCause
    )

  private def moveJudgmentCauseSecondaryFrame(frame: MoveJudgmentCauseFrame): MoveJudgmentCauseFrame =
    frame.copy(role = MoveJudgmentCauseFrameRole.SecondaryCause)

  private def moveJudgmentCauseComparisonKey(
      frame: MoveJudgmentCauseFrame
  ): (CandidateComparisonKind, LineNodeRef, LineNodeRef) =
    (frame.comparisonKind, frame.referenceLine, frame.candidateLine)

  private def moveJudgmentCauseFrameIdentity(
      frame: MoveJudgmentCauseFrame
  ): (List[String], RelativeCauseKind, CandidateComparisonKind, LineNodeRef, LineNodeRef) =
    (frame.causeEvidenceIds, frame.causeKind, frame.comparisonKind, frame.referenceLine, frame.candidateLine)

  private final case class CauseEvidenceEntry(
      id: String,
      cause: RelativeCauseFact,
      evidence: List[EvidenceRef]
  )

  private def verdictFrame(assessment: RelativeMoveAssessment): MoveJudgmentVerdictFrame =
    MoveJudgmentVerdictFrame(
      verdict = assessment.comparison.verdict,
      winPercentLossForMover = assessment.comparison.winPercentLossForMover,
      candidateWinPercentDeltaForMover = assessment.comparison.candidateWinPercentDeltaForMover,
      relativeAssessmentEvidenceId = assessment.evidence.id,
      verdictCertificationEvidenceId = assessment.verdictCertificationEvidence.map(_.id),
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = assessment.reference.ref,
      candidateLine = assessment.candidate.ref,
      candidateSet = assessment.comparison.candidateSet
    )

  private def claimFrame(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): MoveJudgmentClaimFrame =
    val strategicAxisLineage = ClaimStrategicAxisLineage.fromClaim(claim, graph)
    val objectBindings = EvidenceObjectBinding.fromClaim(claim, graph)
    MoveJudgmentClaimFrame(
      claimId = claim.id,
      family = claim.family,
      tier = PlayerFacingClaimPolicy.tier(claim, graph, playedMoves),
      subjectBinding = JudgmentSubjectBinding.claimBinding(claim, graph, playedMoves),
      ideaIds = claim.ideaRefs.map(_.id).distinct.sorted,
      evidenceIds = judgmentVisibleEvidenceIds(claim.evidence),
      strategicAxisLineage = strategicAxisLineage,
      strategicAxisKeys = strategicAxisLineage.map(_.axisKey).distinct.sorted,
      strategicAxisMechanismEvidenceIds = strategicAxisLineage.map(_.mechanismEvidenceId).distinct.sorted,
      strategicAxisSourceEvidenceIds = strategicAxisLineage.map(_.signalSourceEvidenceId).distinct.sorted,
      strategicAxisRelativeCauseIds = strategicAxisLineage.flatMap(_.relativeCauseIds).distinct.sorted,
      objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
      concreteObjectReady = !PlayerFacingClaimPolicy.requiresConcreteObject(claim.family) ||
        EvidenceObjectBinding.playerFacingReady(objectBindings)
    )

  private def judgmentVisibleEvidenceRefs(refs: List[EvidenceRef]): List[EvidenceRef] =
    refs
      .filterNot(ref => StrategicMechanismEvidence.rawStrategicSourceLayer(ref.layer))
      .distinctBy(_.id)

  private def judgmentVisibleEvidenceIds(refs: List[EvidenceRef]): List[String] =
    judgmentVisibleEvidenceRefs(refs).map(_.id).distinct.sorted

  private def judgmentVisibleEvidenceIds(graph: TypedEvidenceGraph, ids: List[String]): List[String] =
    ids
      .distinct
      .filter(id =>
        graph.byId
          .get(id)
          .forall(record => !StrategicMechanismEvidence.rawStrategicSourceLayer(record.ref.layer))
      )
      .sorted

  private def claimCarriesVerdict(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id)).exists {
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        playedMoves.contains(JudgmentSubjectBinding.normalizeMove(assessment.played.moveUci))
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        playedMoves.contains(JudgmentSubjectBinding.normalizeMove(certification.playedMove))
      case _ =>
        false
    }

  private def clusterFrame(
      cluster: ClaimEventCluster,
      graph: TypedEvidenceGraph,
      claimLifecycle: List[ClaimLifecycleDiagnostic]
  ): MoveJudgmentCauseFrame =
    val lifecycleMatches =
      claimLifecycle.filter(diagnostic => diagnostic.relativeCauses.exists(causeMatchesCluster(_, cluster)))
    val strategicProof = RelativeCauseStrategicProofIdentity.fromMechanisms(cluster.proofStrategicMechanisms)
    MoveJudgmentCauseFrame(
      role = frameRoleFor(cluster.causeRole, cluster.comparisonKind, cluster.causeImportance, cluster.verdict),
      clusterId = Some(cluster.id),
      framed = true,
      causeEvidenceIds = causeEvidenceIdsFor(cluster, graph),
      causeKind = cluster.causeKind,
      comparisonKind = cluster.comparisonKind,
      causeRole = cluster.causeRole,
      causeSourceSide = cluster.causeSourceSide,
      causeImportance = cluster.causeImportance,
      attributionKind = cluster.attributionKind,
      attributionRootMoveMatched = cluster.attributionRootMoveMatched,
      attributionDirectProofEligible = cluster.attributionDirectProofEligible,
      referenceLine = cluster.referenceLine,
      candidateLine = cluster.candidateLine,
      eventLine = cluster.eventLine,
      eventRootMove = cluster.eventRootMove,
      causeClaimIds = cluster.causeClaimIds,
      evaluationClaimIds = cluster.evaluationClaimIds,
      witnessClaimIds = cluster.witnessClaimIds,
      ideaIds = cluster.ideas.map(_.id).distinct.sorted,
      supportIdeaIds = Nil,
      claimCandidateIds = lifecycleMatches.map(_.candidateId).distinct.sorted,
      finalClaimIds = (cluster.memberClaimIds ++ lifecycleMatches.flatMap(_.finalClaimId)).distinct.sorted,
      relatedSupportClusterIds = cluster.relatedSupportClusterIds,
      evidenceIds = judgmentVisibleEvidenceIds(graph, cluster.evidence.map(_.id)),
      proofDirectSourceIds = judgmentVisibleEvidenceIds(graph, cluster.proofDirectSourceIds),
      proofContrastSourceIds = judgmentVisibleEvidenceIds(graph, cluster.proofContrastSourceIds),
      proofContextSupportSourceIds = judgmentVisibleEvidenceIds(graph, cluster.proofContextSupportSourceIds),
      proofStrategicAxisLineage = StrategicAxisProofLineage.fromMechanisms(cluster.proofStrategicMechanisms),
      proofStrategicAxisKeys = strategicProof.axisKeys,
      proofStrategicMechanismKinds = strategicProof.mechanismKinds,
      proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
      proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
      supportEvidenceSourceIds = judgmentVisibleEvidenceIds(graph, cluster.causeProofs.flatMap(_.supportEvidenceSourceIds)),
      proofLineConsequences = cluster.proofLineConsequences.distinct.sortBy(_.toString),
      objectBindingSignatures = cluster.objectBindingSignatures,
      concreteObjectReady = EvidenceObjectBinding.playerFacingReadySignatures(cluster.objectBindingSignatures),
      hasOwnedTacticalProof = clusterHasOwnedTacticalProof(cluster),
      narrativeRole = MoveJudgmentCauseNarrativeProjection.defaultNarrativeRole(frameRoleFor(cluster.causeRole, cluster.comparisonKind, cluster.causeImportance, cluster.verdict))
    )

  private def unframedCauseFrames(
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claimLifecycle: List[ClaimLifecycleDiagnostic],
      clusters: List[ClaimEventCluster],
      supportClusters: List[ClaimSupportCluster]
  ): List[MoveJudgmentCauseFrame] =
    causeEvidenceEntries(graph)
      .filter(entry => unframedCauseEligible(entry.cause))
      .filterNot(entry => clusters.exists(clusterMatchesCause(_, entry.cause)))
      .map(entry => unframedCauseFrame(entry, graph, ideas, claimLifecycle, supportClusters))

  private def unframedCauseEligible(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).nonEmpty ||
      primaryLongTermRelativeCause(cause) ||
      exactCurrentMoveStrategicSupport(cause) ||
      playedAlternativeLongTermContext(cause)

  private def primaryLongTermRelativeCause(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).isEmpty &&
      cause.hasOwnedAdmissibleLongTermProof &&
      cause.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      cause.role == RelativeCauseRole.PrimaryPlayedCause &&
      (cause.sourceSide == RelativeCauseSourceSide.Reference || cause.sourceSide == RelativeCauseSourceSide.Candidate) &&
      cause.importance == RelativeCauseImportance.Primary &&
      lossVerdict(cause.verdict)

  private[chessjudgment] def playedAlternativeLongTermContext(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).isEmpty &&
      cause.hasOwnedAdmissibleLongTermProof &&
      cause.comparisonKind == CandidateComparisonKind.PlayedVsAlternative &&
      cause.role == RelativeCauseRole.PlayedAlternativeContext &&
      cause.importance == RelativeCauseImportance.Context &&
      cause.eventLine == cause.candidateLine

  private def exactCurrentMoveStrategicSupport(cause: RelativeCauseFact): Boolean =
    ClaimEventCluster.kindForCause(cause.kind).isEmpty &&
      exactCurrentMoveStrategicSupportKind(cause.kind) &&
      cause.hasOwnedAdmissibleLongTermProof &&
      cause.attribution.directProofEligible &&
      cause.attribution.rootMoveMatched &&
      cause.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      cause.verdict == MoveChoiceVerdict.MatchesReference &&
      cause.role == RelativeCauseRole.PrimaryPlayedCause &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      cause.importance == RelativeCauseImportance.Primary &&
      cause.eventLine == cause.candidateLine

  private def exactCurrentMoveStrategicSupportKind(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.ActivityGain ||
      kind == RelativeCauseKind.TargetPressureGain ||
      kind == RelativeCauseKind.PawnWeaknessTarget ||
      kind == RelativeCauseKind.PawnBreakOpportunity ||
      kind == RelativeCauseKind.OpponentRestriction ||
      kind == RelativeCauseKind.PlanImprovement

  private def lossVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict match
      case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder =>
        true
      case _ =>
        false

  private def unframedCauseFrame(
      entry: CauseEvidenceEntry,
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claimLifecycle: List[ClaimLifecycleDiagnostic],
      supportClusters: List[ClaimSupportCluster]
  ): MoveJudgmentCauseFrame =
    val cause = entry.cause
    val lifecycleMatches =
      claimLifecycle.filter(diagnostic => diagnostic.relativeCauses.exists(causeMatchesLifecycle(_, cause)))
    val directIdeaIds =
      ideas
        .filter(idea => idea.evidence.exists(_.id == entry.id))
        .map(_.ref.id)
        .distinct
        .sorted
    val boundIdeaIds =
      (directIdeaIds ++ lifecycleMatches.flatMap(_.ideaIds)).distinct.sorted
    val supportIdeaIds =
      supportIdeaIdsForUnframedCause(ideas, entry.evidence, boundIdeaIds)
    val relatedSupportClusterIds =
      supportClusterIdsForUnframedCause(supportClusters, boundIdeaIds ++ supportIdeaIds, lifecycleMatches, entry.evidence)
    val proof = cause.proof.getOrElse(RelativeCauseProof())
    val strategicProof = proof.strategicProofIdentity
    val objectBindings = EvidenceObjectBinding.fromRelativeCause(cause, graph)
    MoveJudgmentCauseFrame(
      role = frameRoleFor(cause.role, cause.comparisonKind, cause.importance, cause.verdict),
      clusterId = None,
      framed = false,
      causeEvidenceIds = List(entry.id),
      causeKind = cause.kind,
      comparisonKind = cause.comparisonKind,
      causeRole = cause.role,
      causeSourceSide = cause.sourceSide,
      causeImportance = cause.importance,
      attributionKind = cause.attribution.kind,
      attributionRootMoveMatched = cause.attribution.rootMoveMatched,
      attributionDirectProofEligible = cause.attribution.directProofEligible,
      referenceLine = cause.referenceLine,
      candidateLine = cause.candidateLine,
      eventLine = cause.eventLine,
      eventRootMove = cause.eventRootMove,
      causeClaimIds = Nil,
      evaluationClaimIds = Nil,
      witnessClaimIds = Nil,
      ideaIds = boundIdeaIds,
      supportIdeaIds = supportIdeaIds,
      claimCandidateIds = lifecycleMatches.map(_.candidateId).distinct.sorted,
      finalClaimIds = lifecycleMatches.flatMap(_.finalClaimId).distinct.sorted,
      relatedSupportClusterIds = relatedSupportClusterIds,
      evidenceIds = judgmentVisibleCauseEvidenceIds(entry.evidence, cause),
      proofDirectSourceIds = judgmentVisibleCauseEvidenceIds(proof.directProof.sourceRefs, cause),
      proofContrastSourceIds = judgmentVisibleEvidenceIds(proof.contrastProof.sourceRefs),
      proofContextSupportSourceIds = judgmentVisibleEvidenceIds(proof.contextSupport.sourceRefs),
      proofStrategicAxisLineage = proof.strategicAxisLineage,
      proofStrategicAxisKeys = strategicProof.axisKeys,
      proofStrategicMechanismKinds = strategicProof.mechanismKinds,
      proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
      proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
      supportEvidenceSourceIds = judgmentVisibleCauseEvidenceIds(cause.supportEvidence, cause),
      proofLineConsequences = proof.lineConsequences.distinct.sortBy(_.toString),
      objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
      concreteObjectReady = EvidenceObjectBinding.playerFacingReady(objectBindings),
      hasOwnedAdmissibleLongTermProof = cause.hasOwnedAdmissibleLongTermProof,
      hasOwnedTacticalProof = cause.hasOwnedTacticalProof,
      narrativeRole = MoveJudgmentCauseNarrativeProjection.defaultNarrativeRole(frameRoleFor(cause.role, cause.comparisonKind, cause.importance, cause.verdict))
    )

  private def clusterHasOwnedTacticalProof(cluster: ClaimEventCluster): Boolean =
    cluster.attributionDirectProofEligible &&
      (
        cluster.proofTacticalMechanisms.exists(_.hasConcreteProof) ||
          cluster.proofLineConsequences.exists(LineConsequenceKind.tacticalDriver) ||
          cluster.proofRelationKinds.nonEmpty
      )

  private def supportIdeaIdsForUnframedCause(
      ideas: List[ChessIdea],
      evidence: List[EvidenceRef],
      boundIdeaIds: List[String]
  ): List[String] =
    val evidenceIds = evidence.map(_.id).toSet
    val bound = boundIdeaIds.toSet
    ideas
      .filter(idea => idea.evidence.exists(ref => evidenceIds.contains(ref.id)))
      .map(_.ref.id)
      .filterNot(bound.contains)
      .distinct
      .sorted

  private def supportClusterIdsForUnframedCause(
      supportClusters: List[ClaimSupportCluster],
      ideaIds: List[String],
      lifecycleMatches: List[ClaimLifecycleDiagnostic],
      evidence: List[EvidenceRef]
  ): List[String] =
    val ideaIdSet = ideaIds.toSet
    val evidenceIds = evidence.map(_.id).toSet
    val lifecycleClaimIds =
      lifecycleMatches.flatMap(diagnostic =>
        diagnostic.finalClaimId.toList ++
          diagnostic.sourceCandidateIds ++
          List(diagnostic.claimId, diagnostic.candidateId)
      ).toSet
    supportClusters
      .filter { cluster =>
        cluster.ideas.exists(idea => ideaIdSet.contains(idea.id)) ||
          cluster.evidence.exists(ref => evidenceIds.contains(ref.id)) ||
          (cluster.anchorClaimIds ++ cluster.supportingClaimIds ++ cluster.constrainingClaimIds).exists(lifecycleClaimIds.contains)
      }
      .map(_.id)
      .distinct
      .sorted

  private def frameRoleFor(
      causeRole: RelativeCauseRole,
      comparisonKind: CandidateComparisonKind,
      importance: RelativeCauseImportance,
      verdict: MoveChoiceVerdict
  ): MoveJudgmentCauseFrameRole =
    if causeRole == RelativeCauseRole.PrimaryPlayedCause &&
      comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      importance == RelativeCauseImportance.Primary &&
      lossVerdict(verdict)
    then
      MoveJudgmentCauseFrameRole.PrimaryCause
    else if importance == RelativeCauseImportance.Context ||
      causeRole == RelativeCauseRole.PlayedAlternativeContext ||
      causeRole == RelativeCauseRole.AlternativeDiagnostic
    then MoveJudgmentCauseFrameRole.ContextCause
    else MoveJudgmentCauseFrameRole.SecondaryCause

  private def causeEvidenceEntries(graph: TypedEvidenceGraph): List[CauseEvidenceEntry] =
    val standalone =
      graph.records.collect { case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), parents) =>
        CauseEvidenceEntry(ref.id, cause, relativeCauseEvidence(ref, parents, cause))
      }
    val standaloneKeys = standalone.map(_.cause.identityKey).toSet
    val embedded =
      graph.records.collect { case EvidenceRecord(ref, MoveVerdictCertificationEvidence(certification), parents) =>
        certification.causes.zipWithIndex
          .filterNot { case (cause, _) => standaloneKeys.contains(cause.identityKey) }
          .map { case (cause, index) =>
            CauseEvidenceEntry(s"${ref.id}:cause:$index:${cause.kind}", cause, relativeCauseEvidence(ref, parents, cause))
          }
      }.flatten
    (standalone ++ embedded).distinctBy(_.cause.identityKey)

  private def relativeCauseEvidence(
      ref: EvidenceRef,
      parents: List[EvidenceRef],
      cause: RelativeCauseFact
  ): List[EvidenceRef] =
    val proofRefs =
      cause.proof.toList.flatMap(proof =>
        proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs ++ proof.contextSupport.sourceRefs
      )
    (ref :: (parents ++ cause.supportEvidence ++ proofRefs)).distinctBy(_.id)

  private def judgmentVisibleCauseEvidenceIds(refs: List[EvidenceRef], cause: RelativeCauseFact): List[String] =
    val directProofSourceIds =
      cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).toSet
    refs
      .filter(ref => directProofSourceIds.contains(ref.id) || !StrategicMechanismEvidence.rawStrategicSourceLayer(ref.layer))
      .map(_.id)
      .distinct
      .sorted

  private def causeEvidenceIdsFor(
      cluster: ClaimEventCluster,
      graph: TypedEvidenceGraph
  ): List[String] =
    graph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) if clusterMatchesCause(cluster, cause) =>
        ref.id
    }.distinct.sorted

  private def clusterMatchesCause(cluster: ClaimEventCluster, cause: RelativeCauseFact): Boolean =
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
      cluster.eventLine == cause.eventLine

  private def causeMatchesCluster(cause: ClaimLifecycleRelativeCause, cluster: ClaimEventCluster): Boolean =
    val clusterStrategicProof = RelativeCauseStrategicProofIdentity.fromMechanisms(cluster.proofStrategicMechanisms)
    cause.kind == cluster.causeKind &&
      cause.comparisonKind == cluster.comparisonKind &&
      cause.role == cluster.causeRole &&
      cause.sourceSide == cluster.causeSourceSide &&
      cause.importance == cluster.causeImportance &&
      cause.attributionKind == cluster.attributionKind &&
      cause.attributionRootMoveMatched == cluster.attributionRootMoveMatched &&
      cause.attributionDirectProofEligible == cluster.attributionDirectProofEligible &&
      cause.referenceLine == cluster.referenceLine &&
      cause.candidateLine == cluster.candidateLine &&
      cause.eventLine == cluster.eventLine &&
      strategicProofCovers(clusterStrategicProof, cause)

  private def causeMatchesLifecycle(
      lifecycleCause: ClaimLifecycleRelativeCause,
      cause: RelativeCauseFact
  ): Boolean =
    val proof = cause.proof
    val strategicProof = cause.strategicProofIdentity
    lifecycleCause.kind == cause.kind &&
      lifecycleCause.comparisonKind == cause.comparisonKind &&
      lifecycleCause.role == cause.role &&
      lifecycleCause.sourceSide == cause.sourceSide &&
      lifecycleCause.importance == cause.importance &&
      lifecycleCause.referenceLine == cause.referenceLine &&
      lifecycleCause.candidateLine == cause.candidateLine &&
      lifecycleCause.eventLine == cause.eventLine &&
      lifecycleCause.attributionKind == cause.attribution.kind &&
      lifecycleCause.attributionOwnedEvidenceIds.toSet == cause.attribution.ownedEvidence.map(_.id).toSet &&
      lifecycleCause.attributionContrastEvidenceIds.toSet == cause.attribution.contrastEvidence.map(_.id).toSet &&
      lifecycleCause.attributionContextEvidenceIds.toSet == cause.attribution.contextEvidence.map(_.id).toSet &&
      lifecycleCause.attributionRootMoveMatched == cause.attribution.rootMoveMatched &&
      lifecycleCause.attributionDirectProofEligible == cause.attribution.directProofEligible &&
      lifecycleCause.attributionReason == cause.attribution.reason &&
      lifecycleCause.supportEvidenceSourceIds.toSet == cause.supportEvidence.map(_.id).toSet &&
      lifecycleCause.proofDirectSourceIds.toSet == proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).toSet &&
      lifecycleCause.proofContrastSourceIds.toSet == proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).toSet &&
      lifecycleCause.proofContextSupportSourceIds.toSet == proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).toSet &&
      lifecycleCause.proofStrategicAxisKeys == strategicProof.axisKeys &&
      lifecycleCause.proofStrategicMechanismKinds == strategicProof.mechanismKinds &&
      lifecycleCause.proofStrategicMechanismSourceIds == strategicProof.mechanismSourceIds &&
      lifecycleCause.proofStrategicMechanismSignalSourceIds == strategicProof.signalSourceIds &&
      lifecycleCause.proofDirectKinds.toSet == proof.toList.flatMap(_.directProof.kindLabels).toSet &&
      lifecycleCause.proofContrastKinds.toSet == proof.toList.flatMap(_.contrastProof.kindLabels).toSet &&
      lifecycleCause.proofContextSupportKinds.toSet == proof.toList.flatMap(_.contextSupport.kindLabels).toSet

  private def strategicProofCovers(
      cluster: RelativeCauseStrategicProofIdentity,
      cause: ClaimLifecycleRelativeCause
  ): Boolean =
    containsAll(cluster.axisKeys, cause.proofStrategicAxisKeys) &&
      containsAll(cluster.mechanismKinds, cause.proofStrategicMechanismKinds) &&
      containsAll(cluster.mechanismSourceIds, cause.proofStrategicMechanismSourceIds) &&
      containsAll(cluster.signalSourceIds, cause.proofStrategicMechanismSignalSourceIds)

  private def containsAll[A](available: List[A], required: List[A]): Boolean =
    val availableSet = available.toSet
    required.forall(availableSet.contains)

  private def supportClustersRelatedTo(
      supportClusters: List[ClaimSupportCluster],
      frames: List[MoveJudgmentCauseFrame]
  ): List[String] =
    val frameClaimIds =
      frames.flatMap(frame => frame.causeClaimIds ++ frame.evaluationClaimIds ++ frame.witnessClaimIds ++ frame.finalClaimIds).toSet
    supportClusters
      .filter(cluster =>
        (cluster.anchorClaimIds ++ cluster.supportingClaimIds ++ cluster.constrainingClaimIds).exists(frameClaimIds.contains)
      )
      .map(_.id)

  private def overriddenLocalIdeas(
      ideaVerdict: Option[IdeaVerdictSplit],
      claims: List[ClaimSeed]
  ): List[MoveJudgmentLocalIdeaFrame] =
    val claimIdsByIdea =
      claims
        .flatMap(claim => claim.ideaRefs.map(idea => idea.id -> claim.id))
        .groupMap(_._1)(_._2)
    ideaVerdict.toList.flatMap(_.bindings).collect {
      case binding if binding.relation == IdeaVerdictRelation.RefutesIdea =>
        MoveJudgmentLocalIdeaFrame(
          ideaId = binding.idea.id,
          relation = binding.relation,
          claimIds = claimIdsByIdea.getOrElse(binding.idea.id, Nil).distinct.sorted,
          evidenceIds = judgmentVisibleEvidenceIds(binding.evidence)
        )
    }.distinctBy(frame => (frame.ideaId, frame.relation))

  private def preservedLocalIdeas(
      claims: List[ClaimSeed],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[MoveJudgmentLocalIdeaFrame] =
    val claimsById = claims.map(claim => claim.id -> claim).toMap
    claims.filter(preservedLocalIdeaClaimReady(_, graph, playedMoves)).flatMap { claim =>
      claim.salience.toList.flatMap(_.interactions).filter(_.kind == ClaimInteractionKind.BadVerdictPreservesLocalIdea).flatMap {
        interaction =>
          claim.ideaRefs.map { idea =>
            MoveJudgmentLocalIdeaFrame(
              ideaId = idea.id,
              relation = IdeaVerdictRelation.ExplainsIdeaDespiteBadVerdict,
              claimIds = List(claim.id, interaction.relatedClaimId).filter(claimsById.contains).distinct.sorted,
              evidenceIds = judgmentVisibleEvidenceIds(interaction.interactionEvidence)
            )
          }
      }
    }.distinctBy(frame => (frame.ideaId, frame.claimIds.mkString(","), frame.evidenceIds.mkString(",")))

  private def preservedLocalIdeaClaimReady(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    val binding = JudgmentSubjectBinding.claimBinding(claim, graph, playedMoves)
    val playedMoveBound =
      claim.subjectMove.exists(move => playedMoves.contains(JudgmentSubjectBinding.normalizeMove(move)))
    playedMoveBound &&
      (
        binding == SubjectBindingClass.DirectPlayed ||
          binding == SubjectBindingClass.PrimaryPlayedCause
      )

case class EvidenceBackedJudgmentPacket(
    root: PositionNodeRef,
    positions: List[PositionNode],
    candidateLines: List[CandidateLineNode],
    transitions: List[MoveTransitionEdge],
    relativeAssessments: List[RelativeMoveAssessment],
    evidenceGraph: TypedEvidenceGraph,
    ideas: List[ChessIdea],
    claims: List[ClaimSeed],
    claimLifecycle: List[ClaimLifecycleDiagnostic] = Nil,
    ideaVerdict: Option[IdeaVerdictSplit],
    claimSupportClusters: List[ClaimSupportCluster] = Nil,
    claimEventClusters: List[ClaimEventCluster] = Nil,
    moveJudgmentView: Option[MoveJudgmentView] = None,
    diagnostics: EvidenceLossReport = EvidenceLossReport.empty,
    probeRequests: List[ProbeRequest] = Nil,
    probeDiagnostics: List[ProbeAdmissionDiagnostic] = Nil
):
  def playedTransition: Option[MoveTransitionEdge] =
    transitions.find(_.role == TransitionEdgeRole.Played)

  def referenceTransition: Option[MoveTransitionEdge] =
    transitions.find(_.role == TransitionEdgeRole.Reference)
