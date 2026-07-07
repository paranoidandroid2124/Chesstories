package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.singlePosition.{ ThreatDriver, ThreatKind, ThreatSeverity }
import lila.chessjudgment.model.CollapseAnalysis

case class CandidateSetComparison(
    secondLine: Option[LineNodeRef],
    rawBestToSecondCpGapForDiagnostics: Option[Int],
    bestToSecondWinPercentGapForMover: Option[Double],
    candidateCount: Int,
    onlyMove: Boolean
)

enum CandidateComparisonKind:
  case PlayedVsBest
  case BestVsSecond
  case PlayedVsAlternative
  case ReferenceVsAlternative

enum RelativeCauseRole:
  case PrimaryPlayedCause
  case PlayedAlternativeContext
  case CandidateSetConstraint
  case AlternativeDiagnostic

  def defaultEventLine(referenceLine: LineNodeRef, candidateLine: LineNodeRef): LineNodeRef =
    this match
      case RelativeCauseRole.PrimaryPlayedCause | RelativeCauseRole.PlayedAlternativeContext =>
        candidateLine
      case RelativeCauseRole.CandidateSetConstraint =>
        referenceLine
      case RelativeCauseRole.AlternativeDiagnostic =>
        candidateLine

object RelativeCauseRole:
  def fromComparisonKind(kind: CandidateComparisonKind): RelativeCauseRole =
    kind match
      case CandidateComparisonKind.PlayedVsBest =>
        RelativeCauseRole.PrimaryPlayedCause
      case CandidateComparisonKind.PlayedVsAlternative =>
        RelativeCauseRole.PlayedAlternativeContext
      case CandidateComparisonKind.BestVsSecond =>
        RelativeCauseRole.CandidateSetConstraint
      case CandidateComparisonKind.ReferenceVsAlternative =>
        RelativeCauseRole.AlternativeDiagnostic

enum RelativeCauseSourceSide:
  case Reference
  case Candidate
  case Shared
  case Mixed

object RelativeCauseSourceSide:
  def eventLine(
      sourceSide: RelativeCauseSourceSide,
      role: RelativeCauseRole,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): LineNodeRef =
    sourceSide match
      case RelativeCauseSourceSide.Reference =>
        referenceLine
      case RelativeCauseSourceSide.Candidate =>
        candidateLine
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
        role.defaultEventLine(referenceLine, candidateLine)

  def fromSupportEvidence(
      kind: RelativeCauseKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef]
  ): Option[RelativeCauseSourceSide] =
    if supportEvidence.isEmpty then
      Some(
        if kind == RelativeCauseKind.OnlyMoveNecessity then RelativeCauseSourceSide.Reference
        else RelativeCauseSourceSide.Shared
      )
    else
      combine(
        supportEvidence.flatMap(ref => evidenceRefSourceSide(ref, referenceLine, candidateLine))
      )

  private def evidenceRefSourceSide(
      ref: EvidenceRef,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): Option[RelativeCauseSourceSide] =
    ref.line match
      case Some(line) if line == referenceLine =>
        Some(RelativeCauseSourceSide.Reference)
      case Some(line) if line == candidateLine =>
        Some(RelativeCauseSourceSide.Candidate)
      case _ =>
        ref.scope match
          case EvidenceScope.BestLine | EvidenceScope.ReferenceTransition | EvidenceScope.AfterReferencePosition =>
            lineRoleSourceSide(LineNodeRole.BestReference, referenceLine, candidateLine)
          case EvidenceScope.PlayedLine | EvidenceScope.PlayedTransition | EvidenceScope.AfterPlayedPosition =>
            lineRoleSourceSide(LineNodeRole.Played, referenceLine, candidateLine)
          case EvidenceScope.CandidateLine | EvidenceScope.AlternativeTransition =>
            lineRoleSourceSide(LineNodeRole.Alternative, referenceLine, candidateLine)
          case EvidenceScope.BeforePosition | EvidenceScope.CurrentPosition =>
            Some(RelativeCauseSourceSide.Shared)
          case EvidenceScope.ThreatLine | EvidenceScope.Counterfactual =>
            None

  private def lineRoleSourceSide(
      role: LineNodeRole,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): Option[RelativeCauseSourceSide] =
    if referenceLine.role == role then Some(RelativeCauseSourceSide.Reference)
    else if candidateLine.role == role then Some(RelativeCauseSourceSide.Candidate)
    else None

  private def combine(sides: List[RelativeCauseSourceSide]): Option[RelativeCauseSourceSide] =
    if sides.isEmpty then None
    else
      val concreteSides = sides.filter(side =>
        side == RelativeCauseSourceSide.Reference || side == RelativeCauseSourceSide.Candidate
      ).distinct
      val hasShared = sides.contains(RelativeCauseSourceSide.Shared)
      if concreteSides.size > 1 || sides.contains(RelativeCauseSourceSide.Mixed) || (concreteSides.nonEmpty && hasShared) then
        Some(RelativeCauseSourceSide.Mixed)
      else concreteSides.headOption.orElse(Option.when(hasShared)(RelativeCauseSourceSide.Shared))

enum RelativeCauseImportance:
  case Primary
  case Supporting
  case Context

enum CauseAttributionKind:
  case ReferenceCreatesResource
  case CandidateAllowsLiability
  case CandidateCreatesValue
  case SharedContext
  case ContextOnly
  case Unattributed

case class CauseAttribution(
    kind: CauseAttributionKind,
    ownedEvidence: List[EvidenceRef] = Nil,
    contrastEvidence: List[EvidenceRef] = Nil,
    contextEvidence: List[EvidenceRef] = Nil,
    rootMoveMatched: Boolean = false,
    directProofEligible: Boolean = false,
    reason: Option[String] = None
):
  def contextOnly: Boolean =
    kind == CauseAttributionKind.ContextOnly
  def unattributed: Boolean =
    kind == CauseAttributionKind.Unattributed
  def rootMismatch: Boolean =
    reason.contains("root-mismatch") ||
      (!rootMoveMatched && ownedEvidence.nonEmpty)

object CauseAttribution:
  val unattributed: CauseAttribution =
    CauseAttribution(CauseAttributionKind.Unattributed, reason = Some("no-cause-attribution"))

object RelativeCauseImportance:
  def from(
      role: RelativeCauseRole,
      sourceSide: RelativeCauseSourceSide,
      kind: RelativeCauseKind
  ): RelativeCauseImportance =
    role match
      case RelativeCauseRole.PrimaryPlayedCause
          if sourceSide == RelativeCauseSourceSide.Shared && kind != RelativeCauseKind.OnlyMoveNecessity =>
        RelativeCauseImportance.Supporting
      case RelativeCauseRole.PrimaryPlayedCause =>
        RelativeCauseImportance.Primary
      case RelativeCauseRole.CandidateSetConstraint if kind == RelativeCauseKind.OnlyMoveNecessity =>
        RelativeCauseImportance.Primary
      case RelativeCauseRole.CandidateSetConstraint =>
        RelativeCauseImportance.Supporting
      case RelativeCauseRole.PlayedAlternativeContext | RelativeCauseRole.AlternativeDiagnostic =>
        RelativeCauseImportance.Context

case class EvalComparison(
    mover: Color,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    rawCandidateDeltaCpForDiagnostics: Int,
    candidateWinPercentDeltaForMover: Double,
    rawCpLossForDiagnostics: Int,
    winPercentLossForMover: Double,
    verdict: MoveChoiceVerdict,
    candidateSet: Option[CandidateSetComparison] = None
)

case class CandidateComparisonFact(
    kind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    comparison: EvalComparison
)

enum RelativeCauseKind:
  case MissedTacticalResource
  case TacticalRefutationOfPlayed
  case CandidateTacticalLiability
  case WrongRecapturer
  case RecaptureRecoveryWindow
  case WrongMoveOrder
  case OnlyMoveNecessity
  case OnlyDefenseNecessity
  case TempoLoss
  case ConversionMiss
  case ConversionSecured
  case SacrificeCompensation
  case StructuralImprovement
  case TargetPressureGain
  case TargetPressureRelease
  case CenterControlGain
  case KingSafetyConcession
  case PawnWeaknessTarget
  case PawnBreakOpportunity
  case ActivityGain
  case ActivityLoss
  case OpponentRestriction
  case StrategicConcession
  case MissedStrategicImprovement
  case PlanImprovement
  case PlanContradiction
  case DefensiveResource
  case DrawResource
  case KingForcing
  case MaterialSwing

case class RelativeCauseFact(
    kind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    verdict: MoveChoiceVerdict,
    winPercentLossForMover: Double,
    candidateWinPercentDeltaForMover: Double,
    supportEvidence: List[EvidenceRef],
    evidenceLines: List[LineNodeRef],
    role: RelativeCauseRole,
    eventLine: LineNodeRef,
    sourceSide: RelativeCauseSourceSide,
    importance: RelativeCauseImportance,
  attribution: CauseAttribution = CauseAttribution.unattributed
)(val proof: Option[RelativeCauseProof] = None):
  def eventRootMove: String = eventLine.rootMove
  def hasRawTypedDepth: Boolean = proof.exists(_.hasRawTypedDepth)
  def hasOwnedTypedDepth: Boolean =
    attribution.directProofEligible && (hasRawTypedDepth || hasOwnedRootDefenderMoveProof)
  def strategicCauseKind: Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)
  def hasStrategicContrastDepth: Boolean =
    strategicCauseKind && proof.exists(_.hasStrategicContrastDepth)
  def hasOwnedStrategicContrastDepth: Boolean =
    attribution.directProofEligible && hasStrategicContrastDepth
  def hasOwnedAdmissibleLongTermProof: Boolean =
    attribution.directProofEligible &&
      strategicCauseKind &&
      proof.exists(proof =>
        proof.directProof.strategicMechanismContrasts.exists(_.hasConcreteProof) ||
          proof.directProof.transitionConsequences.nonEmpty
      )
  def hasOwnedTacticalProof: Boolean =
    attribution.directProofEligible &&
      proof.exists(proof => proof.directProof.hasTacticalProof || proof.contrastProof.hasTacticalProof)
  private def hasOwnedRootDefenderMoveProof: Boolean =
    kind == RelativeCauseKind.ConversionSecured &&
      proof.exists(_.directProof.lineEvents.exists(event =>
        event.kind == LineEventKind.DefenderMove &&
          event.rootMoveMatched(eventRootMove)
      ))
  def strategicProofIdentity: RelativeCauseStrategicProofIdentity =
    RelativeCauseStrategicProofIdentity.fromCause(this)
  def identityKey: RelativeCauseIdentityKey = RelativeCauseIdentityKey.from(this)

object RelativeCauseKind:
  def strategicContrastBacked(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.TargetPressureGain |
          RelativeCauseKind.TargetPressureRelease |
          RelativeCauseKind.CenterControlGain | RelativeCauseKind.KingSafetyConcession |
          RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
          RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss | RelativeCauseKind.OpponentRestriction |
          RelativeCauseKind.StrategicConcession | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        true
      case _ =>
        false

case class RelativeCauseIdentityKey(
    kind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    importance: RelativeCauseImportance,
    verdict: MoveChoiceVerdict,
    winPercentLossForMover: Double,
    candidateWinPercentDeltaForMover: Double,
    supportEvidenceIds: List[String],
    evidenceLineIds: List[String],
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    proofDirectKinds: List[String],
    proofContrastKinds: List[String],
    proofContextSupportKinds: List[String],
    attributionKind: CauseAttributionKind,
    attributionOwnedEvidenceIds: List[String],
    attributionContrastEvidenceIds: List[String],
    attributionContextEvidenceIds: List[String],
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean
)

case class RelativeCauseBinding(
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    eventLine: LineNodeRef,
    evidenceLines: List[LineNodeRef],
    importance: RelativeCauseImportance
)

object RelativeCauseIdentityKey:
  def from(cause: RelativeCauseFact): RelativeCauseIdentityKey =
    RelativeCauseIdentityKey(
      kind = cause.kind,
      comparisonKind = cause.comparisonKind,
      referenceLine = cause.referenceLine,
      candidateLine = cause.candidateLine,
      eventLine = cause.eventLine,
      role = cause.role,
      sourceSide = cause.sourceSide,
      importance = cause.importance,
      verdict = cause.verdict,
      winPercentLossForMover = cause.winPercentLossForMover,
      candidateWinPercentDeltaForMover = cause.candidateWinPercentDeltaForMover,
      supportEvidenceIds = cause.supportEvidence.map(_.id).distinct.sorted,
      evidenceLineIds = cause.evidenceLines.map(_.id).distinct.sorted,
      proofDirectSourceIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContrastSourceIds = cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContextSupportSourceIds = cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).distinct.sorted,
      proofStrategicAxisKeys = cause.strategicProofIdentity.axisKeys,
      proofStrategicMechanismKinds = cause.strategicProofIdentity.mechanismKinds,
      proofStrategicMechanismSourceIds = cause.strategicProofIdentity.mechanismSourceIds,
      proofStrategicMechanismSignalSourceIds = cause.strategicProofIdentity.signalSourceIds,
      proofDirectKinds = cause.proof.toList.flatMap(_.directProof.kindLabels).distinct.sorted,
      proofContrastKinds = cause.proof.toList.flatMap(_.contrastProof.kindLabels).distinct.sorted,
      proofContextSupportKinds = cause.proof.toList.flatMap(_.contextSupport.kindLabels).distinct.sorted,
      attributionKind = cause.attribution.kind,
      attributionOwnedEvidenceIds = cause.attribution.ownedEvidence.map(_.id).distinct.sorted,
      attributionContrastEvidenceIds = cause.attribution.contrastEvidence.map(_.id).distinct.sorted,
      attributionContextEvidenceIds = cause.attribution.contextEvidence.map(_.id).distinct.sorted,
      attributionRootMoveMatched = cause.attribution.rootMoveMatched,
      attributionDirectProofEligible = cause.attribution.directProofEligible
    )

object RelativeCauseFact:
  def binding(
      kind: RelativeCauseKind,
      comparisonKind: CandidateComparisonKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef],
      explicitSourceSide: Option[RelativeCauseSourceSide]
  ): RelativeCauseBinding =
    val role = RelativeCauseRole.fromComparisonKind(comparisonKind)
    val sourceSide =
      explicitSourceSide
        .orElse(semanticSourceSide(kind, referenceLine, candidateLine, supportEvidence))
        .orElse(RelativeCauseSourceSide.fromSupportEvidence(kind, referenceLine, candidateLine, supportEvidence))
        .getOrElse(defaultSourceSide(kind))
    val eventLine = RelativeCauseSourceSide.eventLine(sourceSide, role, referenceLine, candidateLine)
    RelativeCauseBinding(
      role = role,
      sourceSide = sourceSide,
      eventLine = eventLine,
      evidenceLines = evidenceLinesFor(referenceLine, candidateLine, supportEvidence, sourceSide),
      importance = RelativeCauseImportance.from(role, sourceSide, kind)
    )

  def defaultSourceSide(kind: RelativeCauseKind): RelativeCauseSourceSide =
    kind match
      case RelativeCauseKind.OnlyMoveNecessity =>
        RelativeCauseSourceSide.Reference
      case _ =>
        RelativeCauseSourceSide.Shared

  private def semanticSourceSide(
      kind: RelativeCauseKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef]
  ): Option[RelativeCauseSourceSide] =
    kind match
      case RelativeCauseKind.WrongRecapturer
          if EvidenceRef.sameDestinationDifferentOrigin(referenceLine.rootMove, candidateLine.rootMove) &&
            supportReferencesLine(supportEvidence, referenceLine) &&
            supportReferencesLine(supportEvidence, candidateLine) =>
        Some(RelativeCauseSourceSide.Candidate)
      case _ =>
        None

  private def supportReferencesLine(supportEvidence: List[EvidenceRef], line: LineNodeRef): Boolean =
    supportEvidence.exists(_.line.contains(line))

  private def evidenceLinesFor(
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef],
      sourceSide: RelativeCauseSourceSide
  ): List[LineNodeRef] =
    val supportLines =
      supportEvidence.flatMap(_.line).filter(line => line == referenceLine || line == candidateLine)
    val sideLines =
      sourceSide match
        case RelativeCauseSourceSide.Reference =>
          List(referenceLine)
        case RelativeCauseSourceSide.Candidate =>
          List(candidateLine)
        case RelativeCauseSourceSide.Mixed | RelativeCauseSourceSide.Shared =>
          List(referenceLine, candidateLine)
    (supportLines ++ sideLines).distinct

  def apply(
      kind: RelativeCauseKind,
      comparisonKind: CandidateComparisonKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      verdict: MoveChoiceVerdict,
      winPercentLossForMover: Double,
      candidateWinPercentDeltaForMover: Double,
      supportEvidence: List[EvidenceRef],
      evidenceLines: List[LineNodeRef],
      role: RelativeCauseRole,
      eventLine: LineNodeRef,
      sourceSide: RelativeCauseSourceSide,
      importance: RelativeCauseImportance,
      attribution: CauseAttribution
  ): RelativeCauseFact =
    new RelativeCauseFact(
      kind = kind,
      comparisonKind = comparisonKind,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      verdict = verdict,
      winPercentLossForMover = winPercentLossForMover,
      candidateWinPercentDeltaForMover = candidateWinPercentDeltaForMover,
      supportEvidence = supportEvidence,
      evidenceLines = evidenceLines,
      role = role,
      eventLine = eventLine,
      sourceSide = sourceSide,
      importance = importance,
      attribution = attribution
    )()

enum RelativeCauseProofRole:
  case DirectProof
  case ContrastProof
  case ContextSupport

enum RelativeCauseProofStrength:
  case Primary
  case Supporting
  case WeakHint

case class BoardAnchorProof(
    source: EvidenceRef,
    kind: BoardAnchorKind
)

case class LineEventProof(
    source: EvidenceRef,
    kind: LineEventKind,
    moveUci: Option[String] = None,
    plyOffset: Option[Int] = None,
    side: Option[Color] = None,
    square: Option[EvidenceSquare] = None
):
  def rootMoveMatched(rootMove: String): Boolean =
    moveUci.exists(move => LineEventProof.normalizeUci(move) == LineEventProof.normalizeUci(rootMove)) ||
      plyOffset.contains(0)

object LineEventProof:
  private def normalizeUci(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

case class LineConsequenceProof(
    source: EvidenceRef,
    kind: LineConsequenceKind,
    eventMove: Option[String] = None,
    lineMoves: List[String] = Nil
):
  def rootMoveMatched(rootMove: String): Boolean =
    eventMove.exists(move => EvidenceRef.sameMove(move, rootMove)) ||
      lineMoves.exists(move => EvidenceRef.sameMove(move, rootMove))

case class ThreatEpisodeCauseProof(
    source: EvidenceRef,
    driver: ThreatDriver,
    kind: ThreatKind,
    severity: ThreatSeverity
)

case class StrategicMechanismProof(
    source: EvidenceRef,
    kind: StrategicMechanismKind,
    signals: List[StrategicMechanismSignal]
):
  def hasConcreteProof: Boolean =
    signals.nonEmpty

case class StrategicMechanismContrastProof(
    source: EvidenceRef,
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    axisComparisons: List[StrategicAxisComparison],
    sustainability: StrategicSustainabilityAssessment
):
  def hasConcreteProof: Boolean =
    axisComparisons.exists(_.hasContrast)

case class StrategicAxisProofLineage(
    axisKey: String,
    axisKind: StrategicAxisKind,
    axisPolarity: StrategicAxisPolarity,
    axisLabel: String,
    mechanismEvidenceId: String,
    signalSourceEvidenceId: String,
    signalSourceLayer: EvidenceLayer
)

object StrategicAxisProofLineage:
  def fromMechanisms(mechanisms: List[StrategicMechanismProof]): List[StrategicAxisProofLineage] =
    mechanisms
      .flatMap(mechanism =>
        mechanism.signals.flatMap(signal =>
          signal.axis.map(axis =>
            StrategicAxisProofLineage(
              axisKey = axis.stableKey,
              axisKind = axis.kind,
              axisPolarity = axis.polarity,
              axisLabel = axis.label,
              mechanismEvidenceId = mechanism.source.id,
              signalSourceEvidenceId = signal.source.id,
              signalSourceLayer = signal.source.layer
            )
          )
        )
      )
      .distinctBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))
      .sortBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))

  def fromContrasts(contrasts: List[StrategicMechanismContrastProof]): List[StrategicAxisProofLineage] =
    contrasts
      .flatMap(contrast =>
        contrast.axisComparisons.flatMap(axisComparison =>
          axisComparison.sources.map(source =>
            StrategicAxisProofLineage(
              axisKey = axisComparison.axisKey,
              axisKind = axisComparison.axis.kind,
              axisPolarity = axisComparison.axis.polarity,
              axisLabel = axisComparison.axis.label,
              mechanismEvidenceId = contrast.source.id,
              signalSourceEvidenceId = source.id,
              signalSourceLayer = source.layer
            )
          )
        )
      )
      .distinctBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))
      .sortBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))

  def fromStrategicProofs(
      mechanisms: List[StrategicMechanismProof],
      contrasts: List[StrategicMechanismContrastProof]
  ): List[StrategicAxisProofLineage] =
    (fromMechanisms(mechanisms) ++ fromContrasts(contrasts))
      .distinctBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))
      .sortBy(entry => (entry.axisKey, entry.mechanismEvidenceId, entry.signalSourceEvidenceId))

case class RelativeCauseStrategicProofIdentity(
    axisKeys: List[String],
    mechanismKinds: List[StrategicMechanismKind],
    mechanismSourceIds: List[String],
    signalSourceIds: List[String]
):
  def isEmpty: Boolean =
    axisKeys.isEmpty && mechanismKinds.isEmpty && mechanismSourceIds.isEmpty && signalSourceIds.isEmpty

object RelativeCauseStrategicProofIdentity:
  val empty: RelativeCauseStrategicProofIdentity =
    RelativeCauseStrategicProofIdentity(Nil, Nil, Nil, Nil)

  def fromCause(cause: RelativeCauseFact): RelativeCauseStrategicProofIdentity =
    cause.proof.map(fromProof).getOrElse(empty)

  def fromProof(proof: RelativeCauseProof): RelativeCauseStrategicProofIdentity =
    fromStrategicProofs(proof.strategicMechanisms, proof.strategicMechanismContrasts)

  def fromMechanisms(mechanisms: List[StrategicMechanismProof]): RelativeCauseStrategicProofIdentity =
    fromStrategicProofs(mechanisms, Nil)

  def fromStrategicProofs(
      mechanisms: List[StrategicMechanismProof],
      contrasts: List[StrategicMechanismContrastProof]
  ): RelativeCauseStrategicProofIdentity =
    RelativeCauseStrategicProofIdentity(
      axisKeys = StrategicAxisProofLineage.fromStrategicProofs(mechanisms, contrasts).map(_.axisKey).distinct.sorted,
      mechanismKinds = mechanisms.map(_.kind).distinct.sortBy(_.toString),
      mechanismSourceIds = (mechanisms.map(_.source.id) ++ contrasts.map(_.source.id)).distinct.sorted,
      signalSourceIds = (
        mechanisms.flatMap(_.signals.map(_.source.id)) ++
          contrasts.flatMap(_.axisComparisons.flatMap(_.sources.map(_.id)))
      ).distinct.sorted
    )

case class RelativeCauseProofSection(
    role: RelativeCauseProofRole,
    strength: RelativeCauseProofStrength,
    boardAnchors: List[BoardAnchorProof] = Nil,
    lineEvents: List[LineEventProof] = Nil,
    lineConsequences: List[LineConsequenceProof] = Nil,
    relationProofs: List[RelationCauseProof] = Nil,
    tacticalMechanisms: List[TacticalMechanismProof] = Nil,
    strategicMechanisms: List[StrategicMechanismProof] = Nil,
    strategicMechanismContrasts: List[StrategicMechanismContrastProof] = Nil,
    threatEpisodes: List[ThreatEpisodeCauseProof] = Nil,
    transitionConsequences: List[TransitionConsequenceProof] = Nil,
    contextLayers: List[EvidenceLayer] = Nil
):
  def sourceRefs: List[EvidenceRef] =
    (
      boardAnchors.map(_.source) ++
        lineEvents.map(_.source) ++
        lineConsequences.map(_.source) ++
        relationProofs.map(_.source) ++
        tacticalMechanisms.map(_.source) ++
        strategicMechanisms.map(_.source) ++
        strategicMechanismContrasts.map(_.source) ++
        threatEpisodes.map(_.source) ++
        transitionConsequences.map(_.source)
    ).distinctBy(_.id)

  def hasConcreteProof: Boolean =
    boardAnchors.nonEmpty ||
      lineConsequences.nonEmpty ||
      relationProofs.exists(_.hasConcreteProof) ||
      tacticalMechanisms.exists(_.hasConcreteProof) ||
      strategicMechanisms.exists(_.hasConcreteProof) ||
      strategicMechanismContrasts.exists(_.hasConcreteProof) ||
      threatEpisodes.nonEmpty ||
      transitionConsequences.nonEmpty

  def hasAnyEvidence: Boolean =
    hasConcreteProof || lineEvents.nonEmpty || contextLayers.nonEmpty

  def hasTacticalProof: Boolean =
    tacticalMechanisms.exists(_.hasConcreteProof) ||
      relationProofs.exists(proof => proof.hasConcreteProof && proof.hasLineProof) ||
      lineConsequences.exists(proof => LineConsequenceKind.tacticalDriver(proof.kind)) ||
      threatEpisodes.nonEmpty

  def kindLabels: List[String] =
    boardAnchors.map(proof => s"BoardAnchor:${proof.kind}") ++
      lineEvents.map(proof => s"LineEvent:${proof.kind}") ++
      lineConsequences.map(proof => s"LineConsequence:${proof.kind}") ++
      relationProofs.map(proof => s"Relation:${proof.kind}:${proof.detailName}") ++
      tacticalMechanisms.map(proof => s"TacticalMechanism:${proof.kind}") ++
      strategicMechanisms.flatMap(proof =>
        s"StrategicMechanism:${proof.kind}" ::
          proof.signals.flatMap(_.axisKey.map(axis => s"StrategicAxis:$axis"))
      ) ++
      strategicMechanismContrasts.flatMap(proof =>
        s"StrategicMechanismContrast:${proof.comparisonKind}" ::
          proof.axisComparisons.map(axis => s"StrategicAxisContrast:${axis.axisKey}:${axis.outcome}")
      ) ++
      threatEpisodes.map(proof => s"ThreatEpisode:${proof.driver}:${proof.kind}:${proof.severity}") ++
      transitionConsequences.map(proof => s"TransitionConsequence:${proof.anchorKey}") ++
      contextLayers.map(layer => s"ContextLayer:$layer")

object RelativeCauseProofSection:
  def merge(
      role: RelativeCauseProofRole,
      strength: RelativeCauseProofStrength,
      sections: List[RelativeCauseProofSection]
  ): RelativeCauseProofSection =
    RelativeCauseProofSection(
      role = role,
      strength = strength,
      boardAnchors = sections.flatMap(_.boardAnchors).distinct,
      lineEvents = sections.flatMap(_.lineEvents).distinct,
      lineConsequences = sections.flatMap(_.lineConsequences).distinct,
      relationProofs = sections.flatMap(_.relationProofs).distinct,
      tacticalMechanisms = sections.flatMap(_.tacticalMechanisms).distinct,
      strategicMechanisms = sections.flatMap(_.strategicMechanisms).distinct,
      strategicMechanismContrasts = sections.flatMap(_.strategicMechanismContrasts).distinct,
      threatEpisodes = sections.flatMap(_.threatEpisodes).distinct,
      transitionConsequences = sections.flatMap(_.transitionConsequences).distinct,
      contextLayers = sections.flatMap(_.contextLayers).distinct
    )

case class RelativeCauseProof(
    directProof: RelativeCauseProofSection = RelativeCauseProofSection(
      role = RelativeCauseProofRole.DirectProof,
      strength = RelativeCauseProofStrength.Primary
    ),
    contrastProof: RelativeCauseProofSection = RelativeCauseProofSection(
      role = RelativeCauseProofRole.ContrastProof,
      strength = RelativeCauseProofStrength.Supporting
    ),
    contextSupport: RelativeCauseProofSection = RelativeCauseProofSection(
      role = RelativeCauseProofRole.ContextSupport,
      strength = RelativeCauseProofStrength.WeakHint
    )
):
  def sections: List[RelativeCauseProofSection] =
    List(directProof, contrastProof, contextSupport)

  def proofSections: List[RelativeCauseProofSection] =
    sections.filter(_.hasAnyEvidence)

  def boardAnchorProofs: List[BoardAnchorProof] =
    proofSections.flatMap(_.boardAnchors).distinct
  def lineEventProofs: List[LineEventProof] =
    proofSections.flatMap(_.lineEvents).distinct
  def lineConsequenceProofs: List[LineConsequenceProof] =
    proofSections.flatMap(_.lineConsequences).distinct
  def boardAnchors: List[BoardAnchorKind] =
    boardAnchorProofs.map(_.kind).distinct
  def lineEvents: List[LineEventKind] =
    lineEventProofs.map(_.kind).distinct
  def lineConsequences: List[LineConsequenceKind] =
    lineConsequenceProofs.map(_.kind).distinct
  def relationProofs: List[RelationCauseProof] =
    proofSections.flatMap(_.relationProofs).distinct
  def tacticalMechanisms: List[TacticalMechanismProof] =
    proofSections.flatMap(_.tacticalMechanisms).distinct
  def strategicMechanisms: List[StrategicMechanismProof] =
    proofSections.flatMap(_.strategicMechanisms).distinct
  def strategicMechanismContrasts: List[StrategicMechanismContrastProof] =
    proofSections.flatMap(_.strategicMechanismContrasts).distinct
  def strategicProofIdentity: RelativeCauseStrategicProofIdentity =
    RelativeCauseStrategicProofIdentity.fromProof(this)
  def strategicAxisLineage: List[StrategicAxisProofLineage] =
    StrategicAxisProofLineage.fromStrategicProofs(strategicMechanisms, strategicMechanismContrasts)
  def threatEpisodes: List[ThreatEpisodeCauseProof] =
    proofSections.flatMap(_.threatEpisodes).distinct
  def transitionConsequences: List[TransitionConsequenceProof] =
    proofSections.flatMap(_.transitionConsequences).distinct
  def contextLayers: List[EvidenceLayer] =
    contextSupport.contextLayers.distinct
  def relationKinds: List[RelationFactKind] =
    relationProofs.map(_.kind).distinct
  def relationDetails: List[String] =
    relationProofs.map(_.detailSummary).distinct
  def hasAnyEvidence: Boolean =
    sections.exists(_.hasAnyEvidence)
  def hasRawTypedDepth: Boolean =
    directProof.hasConcreteProof || contrastProof.hasConcreteProof
  def hasRawDirectProof: Boolean =
    directProof.hasConcreteProof
  def hasRawContrastProof: Boolean =
    contrastProof.hasConcreteProof
  def hasStrategicContrastDepth: Boolean =
    directProof.strategicMechanismContrasts.exists(_.hasConcreteProof) ||
      contrastProof.strategicMechanismContrasts.exists(_.hasConcreteProof)
  def hasRawContextSupport: Boolean =
    contextSupport.hasAnyEvidence
  def depthProof: RelativeCauseProof =
    RelativeCauseProof(
      directProof = directProof,
      contrastProof = contrastProof
    )

object RelativeCauseProof:
  def merge(proofs: List[RelativeCauseProof]): RelativeCauseProof =
    RelativeCauseProof(
      directProof = RelativeCauseProofSection.merge(
        RelativeCauseProofRole.DirectProof,
        RelativeCauseProofStrength.Primary,
        proofs.map(_.directProof)
      ),
      contrastProof = RelativeCauseProofSection.merge(
        RelativeCauseProofRole.ContrastProof,
        RelativeCauseProofStrength.Supporting,
        proofs.map(_.contrastProof)
      ),
      contextSupport = RelativeCauseProofSection.merge(
        RelativeCauseProofRole.ContextSupport,
        RelativeCauseProofStrength.WeakHint,
        proofs.map(_.contextSupport)
      )
    )

case class TacticalMechanismProof(
    source: EvidenceRef,
    kind: TacticalMechanismKind,
    signals: List[TacticalMechanismSignal]
):
  def hasConcreteProof: Boolean =
    signals.nonEmpty
  def signalLabels: List[String] =
    signals.map(signal => s"${signal.kind}:${signal.label}").distinct

case class RelationCauseProof(
    source: EvidenceRef,
    kind: RelationFactKind,
    proof: RelationWitnessProof
):
  def detailName: String =
    proof.detailName
  def detailSummary: String =
    proof.detailSummary
  def hasLineProof: Boolean =
    proof.hasLineProof
  def hasConcreteProof: Boolean =
    proof.hasTypedDetail && proof.proofAtoms.nonEmpty

case class TransitionConsequenceProof(
    source: EvidenceRef,
    transition: StructuralTransitionBinding,
    consequence: TransitionConsequence
):
  def anchorKey: String =
    consequence.anchorKey

case class MoveVerdictCertification(
    playedMove: String,
    verdict: MoveChoiceVerdict,
    primaryComparison: CandidateComparisonFact,
    causes: List[RelativeCauseFact]
)

case class RelativeMoveAssessment(
    played: MoveTransitionEdge,
    referenceTransition: Option[MoveTransitionEdge],
    reference: CandidateLineNode,
    candidate: CandidateLineNode,
    comparison: EvalComparison,
    collapse: Option[CollapseAnalysis],
    confidence: EvidenceConfidence,
    evidence: EvidenceRef,
    counterfactualEvidence: List[EvidenceRef],
    candidateComparisonEvidence: List[EvidenceRef] = Nil,
    relativeCauseEvidence: List[EvidenceRef] = Nil,
    verdictCertificationEvidence: Option[EvidenceRef] = None
)
