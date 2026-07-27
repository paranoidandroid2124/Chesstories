package lila.chessjudgment.model.judgment

import chess.{ Pawn, Square }
import chess.format.Fen
import lila.chessjudgment.model.{ Motif, PlanId, ProbeAdmissionDiagnostic, ProbeRequest, TransitionType }
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanTheme

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

object ClaimFamily:
  def fromCause(kind: RelativeCauseKind): Option[ClaimFamily] =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability |
          RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
          RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
        Some(ClaimFamily.Tactical)
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity |
          RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        Some(ClaimFamily.Defensive)
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        Some(ClaimFamily.Conversion)
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        Some(ClaimFamily.Material)
      case _ =>
        None

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
  case BadVerdictPreservesLocalClaim

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
  def fromClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[ClaimStrategicAxisLineage] =
    val records = claim.evidence.flatMap(ref => graph.record(ref))
    val causeRecords = records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => record
    }
    if causeRecords.isEmpty then fromRecords(records, graph)
    else
      val causeOwnedEvidenceIds = causeRecords.flatMap {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          cause.supportEvidence ++ cause.proof.toList.flatMap(_.sections.flatMap(_.sourceRefs))
        case _ =>
          Nil
      }.map(_.id).toSet
      fromRecords(
        causeRecords ++ records.filterNot(record =>
          causeRecords.exists(_.ref.id == record.ref.id) || causeOwnedEvidenceIds.contains(record.ref.id)
        ),
        graph
      )

  def fromRecords(records: List[EvidenceRecord], graph: TypedEvidenceGraph): List[ClaimStrategicAxisLineage] =
    val grouped =
      records
        .flatMap(record => axisEntries(record, graph))
        .groupBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))
    grouped.values.toList
      .map { entries =>
        val first = entries.head
        first.copy(relativeCauseIds = entries.flatMap(_.relativeCauseIds).distinct.sorted)
      }
      .sortBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))

  private def axisEntries(record: EvidenceRecord, graph: TypedEvidenceGraph): List[ClaimStrategicAxisLineage] =
    record.payload match
      case payload: StrategicMechanismContrastEvidence =>
        contrastEntries(record.ref.id, payload, None)
      case RelativeCauseFactEvidence(cause) =>
        causeEntries(record.ref.id, cause, graph)
      case _ =>
        Nil

  private def causeEntries(
      causeId: String,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[ClaimStrategicAxisLineage] =
    cause.proof.toList.flatMap(proof =>
      proof.sections.flatMap(section =>
        graph.relativeCauseStrategicMechanisms(section).flatMap { case (source, payload) =>
          mechanismEntries(source.id, payload, Some(causeId))
        } ++
          graph.relativeCauseStrategicAxisComparisons(cause, section).flatMap { case (source, comparison) =>
            graph
              .strategicComparisonSourceRefs(comparison, cause.sourceSide)
              .map(sourceRef => lineage(comparison.axis, source.id, sourceRef, Some(causeId)))
          }
      )
    )

  private def mechanismEntries(
      mechanismEvidenceId: String,
      payload: StrategicMechanismEvidence,
      causeId: Option[String]
  ): List[ClaimStrategicAxisLineage] =
    payload.signals.flatMap(signal => signal.axis.map(axis => lineage(axis, mechanismEvidenceId, signal, causeId)))

  private def contrastEntries(
      mechanismEvidenceId: String,
      payload: StrategicMechanismContrastEvidence,
      causeId: Option[String]
  ): List[ClaimStrategicAxisLineage] =
    payload.axisComparisons.flatMap(axisComparison =>
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

object JudgmentSubjectBinding:

  def claimBinding(packet: EvidenceBackedJudgmentPacket, claim: JudgmentClaim): SubjectBindingClass =
    claimBinding(claim, packet.evidenceGraph, packetPlayedMoves(packet))

  def claimBinding(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    if playedMoves.isEmpty then SubjectBindingClass.Other
    else
      val evidenceBinding =
        strongest(
          claim.evidence
            .flatMap(ref => graph.byId.get(ref.id))
            .map(recordBinding(_, graph, playedMoves))
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

  def recordBinding(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    record.payload match
      case RelativeAssessmentEvidence(assessment) if playedMoves.contains(normalizeMove(assessment.played.moveUci)) =>
        graph.comparisonFor(assessment).map(comparisonBinding(_, playedMoves)).getOrElse(SubjectBindingClass.Other)
      case CandidateComparisonEvidence(fact) =>
        comparisonBinding(fact, playedMoves)
      case RelativeCauseFactEvidence(cause) =>
        relativeCauseBinding(cause, graph, playedMoves)
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
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    graph.comparisonFor(cause).map { fact =>
      val binding = RelativeCauseFact.binding(cause, fact)
      val candidateIsPlayed = playedMoves.contains(normalizeMove(fact.candidateLine.rootMove))
      val referenceIsPlayed = playedMoves.contains(normalizeMove(fact.referenceLine.rootMove))
      binding.role match
        case RelativeCauseRole.PrimaryPlayedCause
            if candidateIsPlayed &&
              binding.importance == RelativeCauseImportance.Primary &&
              badVerdict(fact.comparison.verdict) =>
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
    }.getOrElse(SubjectBindingClass.Other)

  def packetPlayedMoves(packet: EvidenceBackedJudgmentPacket): Set[String] =
    (
      packet.playedTransition.map(_.moveUci).toList ++
        packet.relativeAssessments.map(_.played.moveUci) ++
        packet.candidateLines.filter(_.role == LineNodeRole.Played).map(_.ref.rootMove)
    ).map(normalizeMove).filter(_.nonEmpty).toSet

  def directPlayedClaim(claim: JudgmentClaim, playedMoves: Set[String]): Boolean =
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
      claim: JudgmentClaim,
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
      case CandidateComparisonEvidence(_) | RelativeAssessmentEvidence(_) | RelativeCauseFactEvidence(_) =>
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

  def sourceRecordCoversCurrentPlayedMove(record: EvidenceRecord, move: String): Boolean =
    recordMentionsMove(record, move) &&
      (
        record.ref.scope == EvidenceScope.PlayedTransition ||
          record.ref.line.exists(line => line.role == LineNodeRole.Played && sameMove(line.rootMove, move))
      )

  def sourceRecordCoversPawnBreakMove(record: EvidenceRecord, move: String): Boolean =
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

final case class PlayerFacingClaimDecision(
    claimId: String,
    tier: PlayerFacingClaimTier,
    causeEvidenceIds: List[String]
)

object PlayerFacingClaimPolicy:

  def tier(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingClaimTier =
    if playedMoves.isEmpty then PlayerFacingClaimTier.Diagnostic
    else
      val evidenceRecords =
        claim.evidence
          .flatMap(ref => graph.byId.get(ref.id))
      val hasContextEvidence =
        evidenceRecords.exists(record => contextEvidenceRecord(record, graph, playedMoves))
      val hasDirectPlayedEvidence =
        JudgmentSubjectBinding.hasDirectPlayedEvidence(claim, graph, playedMoves)
      val hasCertifiedContextCause =
        evidenceRecords.exists(record =>
          certifiedContextCause(claim, record, graph, playedMoves)
        )
      val relativeCauseRecords = evidenceRecords.collect {
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => record -> cause
      }
      val playerFacingCauses =
        eligibleRelativeCauses(claim, evidenceRecords, graph, playedMoves)
      val hasPrimaryPlayerFacingCause =
        playerFacingCauses.exists { case (cause, _) =>
          val binding = graph.requiredRelativeCauseBinding(cause)
          binding.role == RelativeCauseRole.PrimaryPlayedCause &&
            binding.importance == RelativeCauseImportance.Primary
        }
      val playerFacingCauseBindings =
        playerFacingCauses.flatMap { case (cause, _) =>
          EvidenceObjectBinding.fromRelativeCauseForProjection(cause, graph)
        }
      val objectBindings =
        if relativeCauseRecords.nonEmpty then playerFacingCauseBindings
        else EvidenceObjectBinding.fromClaim(claim, graph)
      val baseTier =
        if claim.family == ClaimFamily.Evaluation &&
          evidenceRecords.exists(record => evaluationVerdictCarrierRecord(record, graph, playedMoves))
        then
          PlayerFacingClaimTier.Context
        else if hasPrimaryPlayerFacingCause then
          PlayerFacingClaimTier.Primary
        else if hasCertifiedContextCause then
          PlayerFacingClaimTier.Secondary
        else if !requiresRelativeCause(claim.family) && hasContextEvidence then
          PlayerFacingClaimTier.Context
        else if !requiresRelativeCause(claim.family) && relativeCauseRecords.isEmpty && hasDirectPlayedEvidence then
          PlayerFacingClaimTier.Secondary
        else
          PlayerFacingClaimTier.Diagnostic
      if promotedTier(baseTier) && requiresConcreteObject(claim.family) &&
        !EvidenceObjectBinding.specificTargetMechanismReady(objectBindings)
      then PlayerFacingClaimTier.Diagnostic
      else baseTier

  def certifiedPlayedValueCause(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        val claimEvidenceIds = claim.evidence.map(_.id).toSet
        val directProofRefs =
          cause.proof.toList.flatMap(_.directProof.sourceRefs).distinctBy(_.id)
        graph.relativeCauseBinding(cause).exists(binding =>
          claimEvidenceIds.contains(record.ref.id) &&
            binding.role == RelativeCauseRole.PlayedAlternativeContext &&
            binding.importance == RelativeCauseImportance.Context &&
            cause.sourceSide == RelativeCauseSourceSide.Candidate &&
            cause.attribution.kind == CauseAttributionKind.CandidateCreatesValue &&
            playedMoves.contains(JudgmentSubjectBinding.normalizeMove(binding.eventLine.rootMove)) &&
            JudgmentSubjectBinding.relativeCauseBinding(cause, graph, playedMoves) ==
              SubjectBindingClass.ContextPlayed &&
            cause.hasOwnedAdmissibleLongTermProof(graph) &&
            directProofRefs.nonEmpty &&
            directProofRefs.exists(ref =>
              claimEvidenceIds.contains(ref.id) &&
                graph.byId.get(ref.id).exists(source =>
                playedMoves.exists(move => JudgmentSubjectBinding.recordMentionsMove(source, move))
              )
            )
        )
      case _ => false

  def certifiedAlternativeResourceCause(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        val claimEvidenceIds = claim.evidence.map(_.id).toSet
        val directProofRefs =
          cause.proof.toList.flatMap(_.directProof.sourceRefs).distinctBy(_.id)
        val hasOwnedDepth =
          if cause.strategicCauseKind then cause.hasOwnedAdmissibleLongTermProof(graph)
          else cause.hasOwnedTypedDepth(graph)
        graph.comparisonFor(cause).exists(comparison =>
          comparison.kind == CandidateComparisonKind.PlayedVsAlternative &&
            playedMoves.contains(JudgmentSubjectBinding.normalizeMove(comparison.candidateLine.rootMove)) &&
            graph.relativeCauseBinding(cause).exists(binding =>
              claimEvidenceIds.contains(record.ref.id) &&
                binding.role == RelativeCauseRole.PlayedAlternativeContext &&
                binding.importance == RelativeCauseImportance.Context &&
                binding.eventLine == comparison.referenceLine &&
                cause.sourceSide == RelativeCauseSourceSide.Reference &&
                cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
                JudgmentSubjectBinding.relativeCauseBinding(cause, graph, playedMoves) ==
                  SubjectBindingClass.ContextPlayed &&
                hasOwnedDepth &&
                directProofRefs.nonEmpty &&
                directProofRefs.exists(ref =>
                  claimEvidenceIds.contains(ref.id) &&
                    graph.byId.get(ref.id).exists(source =>
                      JudgmentSubjectBinding.recordMentionsMove(source, binding.eventLine.rootMove)
                    )
                )
            )
        )
      case _ => false

  def certifiedContextCause(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    certifiedPlayedValueCause(claim, record, graph, playedMoves) ||
      certifiedAlternativeResourceCause(claim, record, graph, playedMoves)

  def eligibleRelativeCauses(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[(RelativeCauseFact, EvidenceRef)] =
    records
      .collect {
        case record @ EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
          (record, ref, cause, graph.requiredRelativeCauseBinding(cause))
      }
      .filter { case (record, ref, cause, binding) =>
        val directClaimEvidence = claim.evidence.exists(_.id == ref.id)
        val subjectBinding = JudgmentSubjectBinding.relativeCauseBinding(cause, graph, playedMoves)
        val primaryCause =
          directClaimEvidence &&
            binding.role == RelativeCauseRole.PrimaryPlayedCause &&
            binding.importance == RelativeCauseImportance.Primary &&
            subjectBinding == SubjectBindingClass.PrimaryPlayedCause
        val authorized = primaryCause || certifiedContextCause(claim, record, graph, playedMoves)
        val objectReady =
          !requiresConcreteObject(claim.family) ||
            EvidenceObjectBinding.specificTargetMechanismReady(
              EvidenceObjectBinding.fromRelativeCauseForProjection(cause, graph)
            )
        authorized && objectReady
      }
      .map { case (_, ref, cause, _) => cause -> ref }
      .distinctBy { case (cause, _) => RelativeCauseSemanticKey.from(cause, graph) }
      .sortBy { case (cause, ref) => (cause.kind.toString, ref.id) }

  def requiresConcreteObject(family: ClaimFamily): Boolean =
    family != ClaimFamily.Evaluation

  private def requiresRelativeCause(family: ClaimFamily): Boolean =
    family != ClaimFamily.Evaluation && family != ClaimFamily.Opening

  private def promotedTier(tier: PlayerFacingClaimTier): Boolean =
    tier == PlayerFacingClaimTier.Primary || tier == PlayerFacingClaimTier.Secondary

  private def evaluationVerdictCarrierRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case RelativeAssessmentEvidence(assessment) =>
        playedMoves.contains(JudgmentSubjectBinding.normalizeMove(assessment.played.moveUci)) &&
          graph.comparisonFor(assessment).exists(_.kind == CandidateComparisonKind.PlayedVsBest)
      case _ =>
        false

  private def contextEvidenceRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        JudgmentSubjectBinding.comparisonBinding(fact, playedMoves) == SubjectBindingClass.ContextPlayed
      case RelativeCauseFactEvidence(cause) =>
        JudgmentSubjectBinding.relativeCauseBinding(cause, graph, playedMoves) == SubjectBindingClass.ContextPlayed
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
    causeEvidence: List[EvidenceRef] = Nil
)

case class ClaimSalience(
    score: Int,
    drivers: List[ClaimSalienceDriver],
    interactions: List[ClaimInteraction] = Nil
)

object ClaimEvidenceSemantics:

  private val causeBoundLayers: Set[EvidenceLayer] =
    Set(
      EvidenceLayer.Relation,
      EvidenceLayer.MoveMotif,
      EvidenceLayer.TacticalMechanism,
      EvidenceLayer.RelativeCause,
      EvidenceLayer.RelativeAssessment,
      EvidenceLayer.CandidateComparison
    )

  private val longTermExcludedLayers: Set[EvidenceLayer] =
    causeBoundLayers ++ Set(
      EvidenceLayer.Line,
      EvidenceLayer.Eval,
      EvidenceLayer.ThreatPressure
    )

  private[chessjudgment] def longTermSupportExcludedLayer(layer: EvidenceLayer): Boolean =
    longTermExcludedLayers.contains(layer) ||
      StrategicMechanismEvidence.rawStrategicSourceLayer(layer)

  def semanticAnchors(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceSemanticAnchor] =
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
          Option.when(
            fact.kind == CandidateComparisonKind.BestVsSecond || fact.candidateSet.nonEmpty
          )(
            EvidenceSemanticAnchor.of(CandidateComparison, "best-vs-second")
          ),
          Option.when(fact.candidateSet.exists(_.onlyMove))(
            EvidenceSemanticAnchor.of(CandidateComparison, "only-move")
          ),
          Option
            .when(
              fact.kind == CandidateComparisonKind.BestVsSecond || fact.candidateSet.nonEmpty
            )(
              EvidenceSemanticAnchor.of(
                CandidateComparison,
                "best-second-wp",
                winPercentGapAnchor(fact.comparison.winPercentLossForMover)
              )
            )
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

case class JudgmentClaim(
    id: String,
    family: ClaimFamily,
    subject: ClaimSubject,
    primaryPosition: PositionNodeRef,
    primaryLine: Option[LineNodeRef],
    subjectMove: Option[String],
    evidence: List[EvidenceRef],
    scope: EvidenceScope,
    confidence: EvidenceConfidence,
    salience: Option[ClaimSalience] = None
)

final class EvidenceBackedJudgmentPacket private (
    val assembly: JudgmentAssemblyContext,
    val probeRequests: List[ProbeRequest],
    val playerFacingClaimDecisions: List[PlayerFacingClaimDecision]
):
  private val playerFacingClaimDecisionsById: Map[String, PlayerFacingClaimDecision] =
    playerFacingClaimDecisions.map(decision => decision.claimId -> decision).toMap

  def root: PositionNodeRef =
    assembly.root.get

  def positions: List[PositionNode] = assembly.positions

  def candidateLines: List[CandidateLineNode] = assembly.lines

  def candidateLine(role: LineNodeRole): Option[CandidateLineNode] =
    exactlyOne(candidateLines.filter(_.role == role))

  def transitions: List[MoveTransitionEdge] = assembly.transitions

  def relativeAssessments: List[RelativeMoveAssessment] = assembly.relativeAssessments

  def evidenceGraph: TypedEvidenceGraph = assembly.evidenceGraph

  def claims: List[JudgmentClaim] = assembly.claims

  def playerFacingDecision(claim: JudgmentClaim): Option[PlayerFacingClaimDecision] =
    playerFacingClaimDecisionsById.get(claim.id)

  def playerFacingTier(claim: JudgmentClaim): PlayerFacingClaimTier =
    playerFacingDecision(claim).map(_.tier).getOrElse(PlayerFacingClaimTier.Diagnostic)

  def playerFacingRelativeCauses(claim: JudgmentClaim): List[(RelativeCauseFact, EvidenceRef)] =
    val directClaimEvidenceIds = claim.evidence.map(_.id).toSet
    playerFacingDecision(claim).toList
      .flatMap(_.causeEvidenceIds)
      .filter(directClaimEvidenceIds)
      .flatMap(id =>
        evidenceGraph.byId.get(id).collect {
          case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) => cause -> ref
        }
      )
      .distinctBy(_._2.id)

  def probeDiagnostics: List[ProbeAdmissionDiagnostic] = assembly.probeDiagnostics

  def playedTransition: Option[MoveTransitionEdge] =
    exactlyOne(transitions.filter(_.role == TransitionEdgeRole.Played))

  def referenceTransition: Option[MoveTransitionEdge] =
    exactlyOne(transitions.filter(_.role == TransitionEdgeRole.Reference))

  def primaryRelativeAssessment: Option[RelativeMoveAssessment] =
    (
      playedTransition,
      referenceTransition,
      candidateLine(LineNodeRole.Played),
      candidateLine(LineNodeRole.BestReference)
    ) match
      case (Some(played), Some(referenceTransition), Some(playedLine), Some(referenceLine)) =>
        exactlyOne(
          relativeAssessments.filter(assessment =>
            assessment.played == played &&
              assessment.referenceTransition.contains(referenceTransition) &&
              assessment.candidate == playedLine &&
              assessment.reference == referenceLine
          )
        )
      case _ =>
        None

  private def exactlyOne[A](items: List[A]): Option[A] =
    items match
      case item :: Nil => Some(item)
      case _           => None

object EvidenceBackedJudgmentPacket:
  private[chessjudgment] def fromAssembly(
      assembly: JudgmentAssemblyContext,
      probeRequests: List[ProbeRequest],
      playerFacingClaimDecisions: List[PlayerFacingClaimDecision]
  ): Option[EvidenceBackedJudgmentPacket] =
    Option.when(
      structurallyClosed(assembly) &&
        probesClosed(assembly, probeRequests) &&
        playerFacingClaimsClosed(assembly, playerFacingClaimDecisions)
    )(
      new EvidenceBackedJudgmentPacket(assembly, probeRequests, playerFacingClaimDecisions)
    )

  private def playerFacingClaimsClosed(
      assembly: JudgmentAssemblyContext,
      decisions: List[PlayerFacingClaimDecision]
  ): Boolean =
    val claimsById = assembly.claims.map(claim => claim.id -> claim).toMap
    val decisionIds = decisions.map(_.claimId)
    decisionIds.distinct.size == decisionIds.size &&
      decisionIds.toSet == claimsById.keySet &&
      decisions.forall { decision =>
        val causeIds = decision.causeEvidenceIds
        claimsById.get(decision.claimId).exists { claim =>
          val directClaimEvidenceIds = claim.evidence.map(_.id).toSet
          causeIds.distinct.size == causeIds.size &&
            causeIds.forall(id =>
              directClaimEvidenceIds(id) &&
                assembly.evidenceGraph.byId.get(id).exists {
                  case EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => true
                  case _                                                  => false
                }
            )
        }
      }

  private def probesClosed(
      assembly: JudgmentAssemblyContext,
      requests: List[ProbeRequest]
  ): Boolean =
    def canonicalFen(raw: String): Option[String] =
      Fen
        .read(chess.variant.Standard, Fen.Full(raw))
        .map(position => Fen.write(position).value)
    val knownFens =
      (
        assembly.positions.map(_.ref.fen) ++
          assembly.evidenceGraph.records.flatMap {
            case EvidenceRecord(_, facts: LineFactEvidence, _) =>
              facts.lineReplaySteps.flatMap(step => List(step.fenBefore, step.fenAfter))
            case _ =>
              Nil
          }
      ).flatMap(canonicalFen).toSet
    val root = assembly.root
    val ids = requests.map(_.id.trim)
    ids.forall(_.nonEmpty) &&
      ids.distinct.size == ids.size &&
      requests.forall { request =>
        val requestFen = canonicalFen(request.fen)
        val rootMoveBound =
          request.candidateMove.forall(move =>
            root.exists(rootPosition =>
              assembly.lines.exists(line => EvidenceRef.sameMove(line.ref.rootMove, move)) &&
                PrincipalVariationEvidence
                  .legalFenAfter(rootPosition.fen, move)
                  .flatMap(canonicalFen)
                  .contains(requestFen.getOrElse(""))
            )
          )
        request.purpose.nonEmpty &&
          request.requiredSignals.nonEmpty &&
          requestFen.exists(knownFens) &&
          request.comparisonFen.forall(fen => canonicalFen(fen).exists(knownFens)) &&
          request.moves.forall(move =>
            PrincipalVariationEvidence.legalFenAfter(request.fen, move).nonEmpty
          ) &&
          request.baselineMove.forall(baseline =>
            request.candidateMove.exists(EvidenceRef.sameMove(baseline, _))
          ) &&
          rootMoveBound
      }

  private def structurallyClosed(assembly: JudgmentAssemblyContext): Boolean =
    val rootPresent = assembly.root.nonEmpty
    val graphRecords = assembly.evidenceGraph.records
    val graphIds = graphRecords.map(_.ref.id)
    val uniqueGraphIds = graphIds.distinct.size == graphIds.size
    val registeredById = graphRecords.map(record => record.ref.id -> record).toMap
    def registered(ref: EvidenceRef): Boolean =
      registeredById.get(ref.id).exists(_.ref == ref)
    val positionRefs = assembly.positions.map(_.ref).toSet
    val lineRefs = assembly.lines.map(_.ref).toSet
    val uniquePositionNodes = positionRefs.size == assembly.positions.size
    val uniqueLineNodes = lineRefs.size == assembly.lines.size
    val uniqueTransitionEvidence =
      assembly.transitions.map(_.evidence.id).distinct.size == assembly.transitions.size
    val uniqueClaimIds =
      assembly.claims.map(_.id).distinct.size == assembly.claims.size
    def exactlyOneRole[A, R](items: List[A], role: R)(itemRole: A => R): Boolean =
      items.count(item => itemRole(item) == role) == 1
    val primaryRolesUnique =
      exactlyOneRole(assembly.positions, PositionNodeRole.Before)(_.role) &&
        exactlyOneRole(assembly.lines, LineNodeRole.Played)(_.role) &&
        exactlyOneRole(assembly.lines, LineNodeRole.BestReference)(_.role) &&
        exactlyOneRole(assembly.transitions, TransitionEdgeRole.Played)(_.role) &&
        exactlyOneRole(assembly.transitions, TransitionEdgeRole.Reference)(_.role)
    val graphClosed =
      graphRecords.forall(record =>
        positionRefs.contains(record.ref.position) &&
          record.ref.line.forall(lineRefs.contains) &&
          record.payloadLineRefs.forall(lineRefs.contains) &&
          record.parents.forall(registered)
      )
    def normalizedMoves(moves: List[String]): List[String] =
      moves.map(EvidenceRef.normalizeMove)
    def exactLineReplay(
        line: CandidateLineNode,
        record: EvidenceRecord,
        payload: LineFactEvidence
    ): Boolean =
      val expectedReplay =
        PrincipalVariationEvidence
          .legalReplay(record.ref.position.fen, line.line.moves, record.ref.position.ply)
          .map(_.zipWithIndex.map { case ((fenBefore, move), index) =>
            LineReplayStep(
              ply = move.ply,
              moveUci = EvidenceRef.normalizeMove(move.uci),
              fenBefore = if index == 0 then record.ref.position.fen else fenBefore,
              fenAfter = move.fenAfter
            )
          })
      val actualReplay =
        payload.lineReplaySteps.map(step =>
          step.copy(moveUci = EvidenceRef.normalizeMove(step.moveUci))
        )
      val lineMatches = payload.line == line.ref
      val movesMatch =
        normalizedMoves(payload.lineReplayMoves) == normalizedMoves(line.line.moves)
      val replayMatches = expectedReplay.contains(actualReplay)
      val exactProofsValid =
        payload.endgameTechniqueHorizons.forall(horizon =>
          horizon.zugzwangProof.forall(_.validFor(horizon, payload.lineReplaySteps))
        )
      lineMatches && movesMatch && replayMatches && exactProofsValid
    def exactLineEvidence(line: CandidateLineNode): Boolean =
      val rootMoveMatches =
        line.line.moves.headOption.exists(move =>
          EvidenceRef.sameMove(move, line.ref.rootMove)
        )
      registeredById.get(line.evidence.id).exists {
        case record @ EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          ref == line.evidence &&
            ref.producer == EvidenceProducer.LegalLineProducer &&
            ref.layer == EvidenceLayer.Line &&
            ref.line.contains(line.ref) &&
            ref.scope == line.role.scope &&
            rootMoveMatches &&
            exactLineReplay(line, record, payload)
        case _ =>
          false
      }
    def exactEvalEvidence(line: CandidateLineNode): Boolean =
      graphRecords.collect {
        case record @ EvidenceRecord(_, EvalFactEvidence(payloadLine, _, _, _), _)
            if payloadLine == line.ref =>
          record
      } match
        case List(EvidenceRecord(ref, EvalFactEvidence(_, whitePovEvalCp, mate, depth), parents)) =>
          ref.producer == EvidenceProducer.EngineEvalProducer &&
            ref.layer == EvidenceLayer.Eval &&
            ref.position == line.evidence.position &&
            ref.line.contains(line.ref) &&
            ref.scope == line.role.scope &&
            whitePovEvalCp == line.whitePovEvalCp &&
            mate == line.mate &&
            depth == line.depth &&
            parents == List(line.evidence)
        case _ =>
          false
    def exactTransitionEvidence(transition: MoveTransitionEdge): Boolean =
      positionRefs.contains(transition.from) &&
        positionRefs.contains(transition.to) &&
        registeredById.get(transition.evidence.id).exists {
          case record @ EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to), parents) =>
            ref == transition.evidence &&
              ref.producer == EvidenceProducer.MoveTransitionProducer &&
              ref.layer == EvidenceLayer.MoveTransition &&
              ref.position == transition.from &&
              ref.line.isEmpty &&
              ref.scope == transition.role.scope &&
              moveUci == transition.moveUci &&
              from == transition.from &&
              to == transition.to &&
              parents.isEmpty &&
              transition.matches(record)
          case _ =>
            false
        }
    val nodeAndTransitionEvidenceClosed =
      assembly.lines.forall(line => exactLineEvidence(line) && exactEvalEvidence(line)) &&
        assembly.transitions.forall(exactTransitionEvidence)
    val planCausalEvidenceClosed =
      graphRecords.forall {
        case EvidenceRecord(ref, event: PlanCausalEventEvidence, _) =>
          ref.line.contains(event.rootLine) &&
            event.rootTransition.line.contains(event.rootLine) &&
            event.opponentResourceDeterrence.forall(_ =>
              event.opponentResourceDeterrenceProofReady(assembly.lines, assembly.evidenceGraph)
            )
        case _ =>
          true
      }
    val claimsClosed =
      assembly.claims.forall(claim =>
        positionRefs.contains(claim.primaryPosition) &&
          claim.primaryLine.forall(lineRefs.contains) &&
          claim.evidence.nonEmpty &&
          claim.evidence.forall(registered) &&
          claim.salience.toList
            .flatMap(_.interactions)
            .forall(interaction =>
              interaction.interactionEvidence.forall(registered) &&
                interaction.causeEvidence.forall(ref =>
                  ref.layer == EvidenceLayer.RelativeCause && registered(ref)
                )
            )
      )
    val assessmentRecords =
      graphRecords.collect {
        case EvidenceRecord(ref, RelativeAssessmentEvidence(assessment), parents) =>
          (ref, assessment, parents)
      }
    val comparisonRecords =
      graphRecords.collect {
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          record -> fact
      }
    val comparisonPairsUnique =
      comparisonRecords
        .map { case (record, fact) =>
          (record.ref.position, fact.referenceLine, fact.candidateLine)
        }
        .distinct
        .size == comparisonRecords.size
    val comparisonLinesByRef =
      assembly.lines.map(line => line.ref -> line).toMap
    val candidateSetOwners =
      comparisonRecords.filter(_._2.candidateSet.nonEmpty)
    def canonicalLineParents(line: CandidateLineNode): List[EvidenceRef] =
      (
        line.evidence ::
          graphRecords.collect {
            case EvidenceRecord(evalRef, EvalFactEvidence(payloadLine, _, _, _), _)
                if payloadLine == line.ref =>
              evalRef
          }
      ).distinctBy(_.id)
    val expectedCandidateSet =
      for
        root <- assembly.root
        mover <- root.sideToMove.orElse(assembly.input.sideToMove)
        reference <- assembly.line(LineNodeRole.BestReference)
        descriptor <- CandidateSetDescriptor.fromLines(mover, assembly.lines, reference)
        second <- CandidateSetDescriptor
          .uniqueLines(assembly.lines)
          .find(line => !EvidenceRef.sameMove(line.ref.rootMove, reference.ref.rootMove))
        played <- assembly.line(LineNodeRole.Played)
      yield
        val ownerCandidate =
          if EvidenceRef.sameMove(second.ref.rootMove, played.ref.rootMove) then played
          else second
        (mover, reference, ownerCandidate, descriptor)
    val candidateSetClosed =
      expectedCandidateSet match
        case None =>
          candidateSetOwners.isEmpty
        case Some((_, reference, ownerCandidate, descriptor)) =>
          candidateSetOwners match
            case List((ownerRecord, ownerFact)) =>
              val topCandidateParents =
                CandidateSetDescriptor
                  .uniqueLines(assembly.lines)
                  .take(3)
                  .flatMap(canonicalLineParents)
                  .distinctBy(_.id)
              val primaryRecord =
                comparisonRecords.collectFirst {
                  case (record, fact)
                      if fact.kind == CandidateComparisonKind.PlayedVsBest &&
                        fact.referenceLine == reference.ref =>
                    record
                }
              ownerFact.referenceLine == reference.ref &&
              ownerFact.candidateLine == ownerCandidate.ref &&
              ownerFact.candidateSet.contains(descriptor) &&
              topCandidateParents.forall(ownerRecord.parents.contains) &&
              primaryRecord.forall(record =>
                record.ref == ownerRecord.ref || record.parents.contains(ownerRecord.ref)
              )
            case _ =>
              false
    val comparisonParentsCanonical =
      comparisonRecords.forall { case (record, fact) =>
        val comparedLineParents =
          List(fact.referenceLine, fact.candidateLine)
            .flatMap(comparisonLinesByRef.get)
            .flatMap(canonicalLineParents)
        val descriptorParents =
          Option
            .when(fact.candidateSet.nonEmpty)(
              CandidateSetDescriptor
                .uniqueLines(assembly.lines)
                .take(3)
                .flatMap(canonicalLineParents)
            )
            .toList
            .flatten
        val descriptorOwnerParent =
          Option
            .when(
              fact.kind == CandidateComparisonKind.PlayedVsBest &&
                fact.candidateSet.isEmpty
            )(
              candidateSetOwners.collectFirst {
                case (ownerRecord, _) => ownerRecord.ref
              }
            )
            .flatten
            .toList
        val expectedParents =
          (comparedLineParents ++ descriptorParents ++ descriptorOwnerParent)
            .distinctBy(_.id)
        record.parents.size == expectedParents.size &&
          record.parents.toSet == expectedParents.toSet
      }
    def descriptorForComparison(
        record: EvidenceRecord,
        fact: CandidateComparisonFact
    ): Option[CandidateSetDescriptor] =
      fact.candidateSet.orElse(
        Option
          .when(fact.kind == CandidateComparisonKind.PlayedVsBest)(
            candidateSetOwners.collectFirst {
              case (ownerRecord, ownerFact) if record.parents.contains(ownerRecord.ref) =>
                ownerFact.candidateSet
            }.flatten
          )
          .flatten
      )
    val comparisonValuesCanonical =
      comparisonRecords.forall { case (record, fact) =>
        (for
          root <- assembly.root
          mover <- root.sideToMove.orElse(assembly.input.sideToMove)
          reference <- comparisonLinesByRef.get(fact.referenceLine)
          candidate <- comparisonLinesByRef.get(fact.candidateLine)
        yield
          val descriptor = descriptorForComparison(record, fact)
          fact.comparison ==
            EvalComparison.fromLines(
              mover,
              reference,
              candidate,
              descriptor.map(_.candidateSetType)
            )).contains(true)
      }
    def comparisonFactBound(ref: EvidenceRef): Boolean =
      assembly.evidenceGraph.candidateComparisonRecord(ref).exists {
        case EvidenceRecord(recordRef, CandidateComparisonEvidence(fact), _) =>
          recordRef.producer == EvidenceProducer.RelativeMoveProducer &&
            recordRef.layer == EvidenceLayer.CandidateComparison &&
            recordRef.line.contains(fact.candidateLine) &&
            lineRefs.contains(fact.referenceLine) &&
            lineRefs.contains(fact.candidateLine)
        case _ =>
          false
      }
    val causesClosed =
      graphRecords.forall {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), parents) =>
          assembly.evidenceGraph.comparisonFor(cause).exists(fact =>
            val binding = RelativeCauseFact.binding(cause, fact)
            comparisonFactBound(cause.comparisonEvidence) &&
              parents.contains(cause.comparisonEvidence) &&
              lineRefs.contains(binding.eventLine) &&
              (cause.comparisonEvidence :: cause.supportEvidence ++
                cause.proof.toList.flatMap(proof =>
                  proof.directProof.sourceRefs ++
                    proof.contrastProof.sourceRefs ++
                    proof.contextSupport.sourceRefs
                )).forall(registered)
          )
        case _ =>
          true
      }
    val assessmentsClosed =
      assessmentRecords.forall { case (recordRef, assessment, parents) =>
        val comparisonRefs =
          assessment.primaryComparisonEvidence :: assessment.relatedComparisonEvidence
        val refs =
          assessment.evidence :: (comparisonRefs ++ assessment.relativeCauseEvidence)
        val expectedParents =
          (
            List(
              assessment.played.evidence,
              assessment.reference.evidence,
              assessment.candidate.evidence
            ) ++
              assessment.referenceTransition.toList.map(_.evidence) ++
              comparisonRefs ++
              assessment.relativeCauseEvidence
          ).distinctBy(_.id)
        val objectsBound =
          assembly.lines.contains(assessment.reference) &&
            assembly.lines.contains(assessment.candidate) &&
            assembly.transitions.contains(assessment.played) &&
            assessment.referenceTransition.forall(assembly.transitions.contains)
        val primaryComparisonBound =
          assembly.evidenceGraph
            .candidateComparisonRecord(assessment.primaryComparisonEvidence)
            .exists {
              case EvidenceRecord(primaryRef, CandidateComparisonEvidence(fact), _) =>
                comparisonFactBound(primaryRef) &&
                  fact.kind == CandidateComparisonKind.PlayedVsBest &&
                  fact.referenceLine == assessment.reference.ref &&
                  fact.candidateLine == assessment.candidate.ref &&
                  primaryRef.confidence == recordRef.confidence
              case _ =>
                false
            }
        val relatedComparisonsBound =
          assessment.relatedComparisonEvidence
            .forall(comparisonFactBound) &&
            assessment.relatedComparisonEvidence.forall(_.id != assessment.primaryComparisonEvidence.id) &&
            comparisonRefs.map(_.id).distinct.size == comparisonRefs.size
        val relativeCausesBound =
          assessment.relativeCauseEvidence.forall(ref =>
            registeredById.get(ref.id).exists {
              case EvidenceRecord(recordCauseRef, RelativeCauseFactEvidence(cause), causeParents) =>
                recordCauseRef == ref &&
                  causeParents.contains(cause.comparisonEvidence) &&
                  comparisonRefs.contains(cause.comparisonEvidence) &&
                  assembly.evidenceGraph.comparisonFor(cause).nonEmpty
              case _ =>
                false
            }
          )
        recordRef == assessment.evidence &&
        recordRef.producer == EvidenceProducer.RelativeMoveProducer &&
        recordRef.layer == EvidenceLayer.RelativeAssessment &&
        recordRef.position == assessment.played.from &&
        recordRef.line.contains(assessment.candidate.ref) &&
        recordRef.scope == EvidenceScope.Counterfactual &&
        objectsBound &&
        refs.forall(registered) &&
        parents == expectedParents &&
          primaryComparisonBound &&
          relatedComparisonsBound &&
          relativeCausesBound
      }
    val primaryAssessmentPresent =
      (
        assembly.playedTransition,
        assembly.referenceTransition,
        assembly.line(LineNodeRole.Played),
        assembly.line(LineNodeRole.BestReference)
      ) match
        case (Some(playedTransition), Some(referenceTransition), Some(playedLine), Some(referenceLine)) =>
          assembly.relativeAssessments.count(assessment =>
            assessment.played == playedTransition &&
              assessment.referenceTransition.contains(referenceTransition) &&
              assessment.candidate == playedLine &&
              assessment.reference == referenceLine &&
              assembly.evidenceGraph.comparisonFor(assessment).exists(fact =>
                fact.kind == CandidateComparisonKind.PlayedVsBest &&
                  fact.candidateLine == playedLine.ref &&
                  fact.referenceLine == referenceLine.ref
              )
          ) == 1
        case _ =>
          false
    val visiting = scala.collection.mutable.Set.empty[String]
    val visited = scala.collection.mutable.Set.empty[String]
    def visit(id: String): Boolean =
      if visited.contains(id) then true
      else if visiting.contains(id) then false
      else
        visiting += id
        val closed =
          registeredById.get(id).exists(record =>
            record.parents.forall(parent => registered(parent) && visit(parent.id))
          )
        visiting -= id
        if closed then visited += id
        closed
    val parentDagAcyclic =
      uniqueGraphIds && graphRecords.forall(record => visit(record.ref.id))
    rootPresent &&
      assembly.positions.nonEmpty &&
      assembly.lines.nonEmpty &&
      graphRecords.nonEmpty &&
      uniquePositionNodes &&
      uniqueLineNodes &&
      uniqueTransitionEvidence &&
      uniqueClaimIds &&
      primaryRolesUnique &&
      uniqueGraphIds &&
      graphClosed &&
      nodeAndTransitionEvidenceClosed &&
      planCausalEvidenceClosed &&
      claimsClosed &&
      causesClosed &&
      assessmentsClosed &&
      primaryAssessmentPresent &&
      comparisonPairsUnique &&
      candidateSetClosed &&
      comparisonParentsCanonical &&
      comparisonValuesCanonical &&
      parentDagAcyclic
