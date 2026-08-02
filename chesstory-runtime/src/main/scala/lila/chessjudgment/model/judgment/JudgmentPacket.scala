package lila.chessjudgment.model.judgment

import chess.{ Pawn, Square }
import chess.format.Fen
import lila.chessjudgment.model.{ Motif, ProbeAdmissionDiagnostic, ProbeRequest, TransitionType }
import lila.chessjudgment.model.line.PrincipalVariationEvidence

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

object ClaimFamily:
  def fromCause(kind: RelativeCauseKind): Option[ClaimFamily] =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability |
          RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
          RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
        Some(ClaimFamily.Tactical)
      case RelativeCauseKind.OnlyDefenseNecessity | RelativeCauseKind.DefensiveResource |
          RelativeCauseKind.DrawResource =>
        Some(ClaimFamily.Defensive)
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        Some(ClaimFamily.Conversion)
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        Some(ClaimFamily.Material)
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        Some(ClaimFamily.Plan)
      case _ =>
        None

enum SubjectBindingClass:
  case PrimaryPlayedCause
  case ContextPlayed
  case Other

object JudgmentSubjectBinding:

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
      case CandidateComparisonKind.PlayedVsBest
          if candidateIsPlayed && fact.comparison.verdict.isActionableLoss =>
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

  private[chessjudgment] def uniquePlayedEndpoint(
      fact: CandidateComparisonFact,
      playedMoves: Set[String]
  ): Option[LineNodeRef] =
    List(fact.referenceLine, fact.candidateLine)
      .filter(line => playedMoves.contains(normalizeMove(line.rootMove)))
      .distinct match
      case line :: Nil => Some(line)
      case _           => None

  def relativeCauseBinding(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): SubjectBindingClass =
    graph.comparisonFor(cause).map { fact =>
      val binding = graph.requiredRelativeCauseBinding(cause)
      val candidateIsPlayed = playedMoves.contains(normalizeMove(fact.candidateLine.rootMove))
      val referenceIsPlayed = playedMoves.contains(normalizeMove(fact.referenceLine.rootMove))
      binding.role match
        case RelativeCauseRole.PrimaryPlayedCause
            if candidateIsPlayed &&
              binding.bindingTier == RelativeCauseBindingTier.Primary &&
              fact.comparison.verdict.isActionableLoss =>
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

  private def directEvidencePayload(payload: EvidencePayload): Boolean =
    payload match
      case CandidateComparisonEvidence(_) | RelativeAssessmentEvidence(_) | RelativeCauseFactEvidence(_) =>
        false
      case _ =>
        true

  def primaryPlayed(binding: SubjectBindingClass): Boolean =
    binding == SubjectBindingClass.PrimaryPlayedCause

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
    causeSelections: List[PlayerFacingCauseSelection]
)

object PlayerFacingClaimDecision:

  def tierFor(
      claim: JudgmentClaim,
      exposure: PlayerFacingCauseExposureResolution,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingClaimTier =
    val selections = exposure.selectionsForClaim(claim.id)
    if selections.exists(_.exposure == PlayerFacingCauseExposureTier.Primary) then
      PlayerFacingClaimTier.Primary
    else if selections.exists(_.exposure == PlayerFacingCauseExposureTier.Complementary) then
      PlayerFacingClaimTier.Secondary
    else PlayerFacingClaimPolicy.causeFreeTier(claim, graph, playedMoves)

  def fromExposure(
      claims: List[JudgmentClaim],
      exposure: PlayerFacingCauseExposureResolution,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[PlayerFacingClaimDecision] =
    claims.map(claim =>
      PlayerFacingClaimDecision(
        claimId = claim.id,
        tier = tierFor(claim, exposure, graph, playedMoves),
        causeSelections = exposure.selectionsForClaim(claim.id).sortBy(_.selectionOrder)
      )
    )

object PlayerFacingClaimPolicy:

  def causeFreeTier(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingClaimTier =
    if playedMoves.isEmpty then PlayerFacingClaimTier.Diagnostic
    else
      val evidenceRecords = claim.evidence.flatMap(graph.record)
      val hasRelativeCause = evidenceRecords.exists(_.payload.isInstanceOf[RelativeCauseFactEvidence])
      if hasRelativeCause || (claim.family != ClaimFamily.Evaluation && claim.family != ClaimFamily.Opening) then
        PlayerFacingClaimTier.Diagnostic
      else
        val hasContextEvidence =
          evidenceRecords.exists(record => contextEvidenceRecord(record, playedMoves))
        val hasDirectPlayedEvidence =
          JudgmentSubjectBinding.hasDirectPlayedEvidence(claim, graph, playedMoves)
        val baseTier =
          if claim.family == ClaimFamily.Evaluation &&
            evidenceRecords.exists(record => evaluationVerdictCarrierRecord(record, graph, playedMoves))
          then PlayerFacingClaimTier.Context
          else if hasContextEvidence then PlayerFacingClaimTier.Context
          else if hasDirectPlayedEvidence then PlayerFacingClaimTier.Secondary
          else PlayerFacingClaimTier.Diagnostic
        if promotedTier(baseTier) && requiresConcreteObject(claim.family) &&
          !EvidenceObjectBinding.specificTargetMechanismReady(
            EvidenceObjectBinding.fromClaim(claim, graph)
          )
        then PlayerFacingClaimTier.Diagnostic
        else baseTier

  def requiresConcreteObject(family: ClaimFamily): Boolean =
    family != ClaimFamily.Evaluation

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
      playedMoves: Set[String]
  ): Boolean =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        JudgmentSubjectBinding.comparisonBinding(fact, playedMoves) == SubjectBindingClass.ContextPlayed
      case _ =>
        false

  def rankPriority(tier: PlayerFacingClaimTier): Int =
    tier match
      case PlayerFacingClaimTier.Primary    => 3
      case PlayerFacingClaimTier.Secondary  => 2
      case PlayerFacingClaimTier.Context    => 1
      case PlayerFacingClaimTier.Diagnostic => 0

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

/** Closed, typed explanatory content candidate owned by one JudgmentClaim. The graph
  * remains the canonical source for every carrier and its payload; this value
  * only names the exact F carrier and, for comparisons, the evaluated relation
  * it must still resolve to.
  */
enum JudgmentClaimContent:
  case CandidateComparison(
      carrier: EvidenceRef,
      comparison: CandidateComparisonSemanticKey
  )
  case StrategicMechanism(carrier: EvidenceRef)

  def carrierRef: EvidenceRef =
    this match
      case CandidateComparison(value, _)       => value
      case StrategicMechanism(value)  => value

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
    content: Option[JudgmentClaimContent] = None
)

final class EvidenceBackedJudgmentPacket private (
    val assembly: JudgmentAssemblyContext,
    val probeRequests: List[ProbeRequest],
    val playerFacingClaimDecisions: List[PlayerFacingClaimDecision],
    val onlyMoveConstraintResolutions: List[OnlyMoveConstraintResolution],
    val causeExposureResolution: PlayerFacingCauseExposureResolution,
    val causeDispositionLedger: CauseDispositionLedger,
    val selectedContentClaimIds: List[String]
):
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

  /** Typed importance interpretation of the already selected R Causes. The
    * packet carries the exact canonical exposure result produced by R; this is
    * a read-only interpretation of that result, not a second selection pass.
    */
  def directCauseImportanceResolution: DirectCauseImportanceResolution =
    causeExposureResolution.importanceResolution

  def directCauseChannel(
      selection: PlayerFacingCauseSelection,
      selectedChannel: PlayerFacingCauseChannelSelection
  ): Option[DirectCauseChannel] =
    evidenceGraph.record(selection.causeEvidence).toList.flatMap {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        RelativeCauseConstructionAdmission
          .admittedDirectChannels(cause, evidenceGraph)
          .filter(channel =>
            channel.binding.source == selectedChannel.carrierEvidence &&
              channel.causalSignature == selectedChannel.causalSignature &&
              channel.directChange == selectedChannel.directChange
          )
      case _ => Nil
    } match
      case channel :: Nil => Some(channel)
      case _              => None

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
      playerFacingClaimDecisions: List[PlayerFacingClaimDecision],
      onlyMoveConstraintResolutions: List[OnlyMoveConstraintResolution],
      causeExposureResolution: PlayerFacingCauseExposureResolution,
      causeDispositionLedger: CauseDispositionLedger,
      selectedContentClaimIds: List[String]
  ): Option[EvidenceBackedJudgmentPacket] =
    Option.when(
      structurallyClosed(assembly) &&
        probesClosed(assembly, probeRequests) &&
        playerFacingClaimsClosed(
          assembly,
          playerFacingClaimDecisions,
          onlyMoveConstraintResolutions,
          causeExposureResolution,
          causeDispositionLedger
        ) &&
        contentClaimsClosed(assembly, selectedContentClaimIds)
    )(
      new EvidenceBackedJudgmentPacket(
        assembly,
        probeRequests,
        playerFacingClaimDecisions,
        onlyMoveConstraintResolutions,
        causeExposureResolution,
        causeDispositionLedger,
        selectedContentClaimIds
      )
    )

  private def playerFacingClaimsClosed(
      assembly: JudgmentAssemblyContext,
      decisions: List[PlayerFacingClaimDecision],
      onlyMoveResolutions: List[OnlyMoveConstraintResolution],
      exposure: PlayerFacingCauseExposureResolution,
      dispositionLedger: CauseDispositionLedger
  ): Boolean =
    val playedMoves = assembly.relativeAssessments
      .map(assessment => EvidenceRef.normalizeMove(assessment.played.moveUci))
      .filter(_.nonEmpty)
      .toSet
    val expectedExposure = PlayerFacingCauseExposurePipeline.resolve(
      assembly.claims,
      assembly.evidenceGraph,
      playedMoves
    )
    val expectedDecisions = PlayerFacingClaimDecision.fromExposure(
      assembly.claims,
      expectedExposure,
      assembly.evidenceGraph,
      playedMoves
    )
    exposure == expectedExposure &&
      decisions == expectedDecisions &&
      onlyMoveResolutions == expectedExposure.onlyMoveConstraintResolutions &&
      dispositionLedger.closedFor(
        assembly.evidenceGraph,
        expectedExposure,
        assembly.claims.map(_.id).toSet
      )

  /** P keeps R's selection opaque: it only validates each selected claim's
    * registered typed content and carrier.
    */
  private def contentClaimsClosed(
      assembly: JudgmentAssemblyContext,
      claimIds: List[String]
  ): Boolean =
    val graph = assembly.evidenceGraph
    val claimsById = assembly.claims.map(claim => claim.id -> claim).toMap

    def registeredCarrier(content: JudgmentClaimContent): Boolean =
      content match
        case JudgmentClaimContent.CandidateComparison(carrier, _) =>
          graph.record(carrier).exists {
            case EvidenceRecord(_, CandidateComparisonEvidence(_), _) => true
            case _                                                     => false
          }
        case JudgmentClaimContent.StrategicMechanism(carrier) =>
          graph.record(carrier).exists {
            case EvidenceRecord(_, _: StrategicMechanismEvidence, _) => true
            case _                                                   => false
          }

    claimIds.distinct.size == claimIds.size &&
      claimIds.forall(claimId =>
        claimsById.get(claimId).exists { claim =>
          claim.content.exists(content =>
            claim.evidence.contains(content.carrierRef) && registeredCarrier(content)
          )
        }
      )

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
          claim.evidence.forall(registered)
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
          assembly.evidenceGraph.comparisonFor(cause).exists(_ =>
            val binding = assembly.evidenceGraph.requiredRelativeCauseBinding(cause)
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
