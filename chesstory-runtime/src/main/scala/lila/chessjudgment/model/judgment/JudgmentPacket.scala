package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  ProbeContractValidator,
  ProbeKind,
  ProbeRequest,
  ProbeVariant
}
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction, PrincipalVariationEvidence }

enum ClaimFamily:
  case Tactical
  case PawnStructure
  case PassedPawnResult
  case Defensive
  case Conversion
  case Material
  case Evaluation

  private[chessjudgment] def isLongTerm: Boolean =
    this match
      case PawnStructure => true
      case Tactical | PassedPawnResult | Defensive | Conversion | Material | Evaluation => false

object ClaimFamily:
  def fromCause(kind: RelativeCauseKind): ClaimFamily =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability |
          RelativeCauseKind.RecaptureRecoveryWindow |
          RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.KingForcing =>
        ClaimFamily.Tactical
      case RelativeCauseKind.DrawResource =>
        ClaimFamily.Defensive
      case RelativeCauseKind.MaterialSwing =>
        ClaimFamily.Material
      case RelativeCauseKind.PassedPawnResult =>
        ClaimFamily.PassedPawnResult

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
      case RelativeAssessmentEvidence(assessment) if playedMoves.contains(EvidenceRef.normalizeMove(assessment.played.moveUci)) =>
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
    val candidateIsPlayed = playedMoves.contains(EvidenceRef.normalizeMove(fact.candidateLine.rootMove))
    val referenceIsPlayed = playedMoves.contains(EvidenceRef.normalizeMove(fact.referenceLine.rootMove))
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
      .filter(line => playedMoves.contains(EvidenceRef.normalizeMove(line.rootMove)))
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
      val candidateIsPlayed = playedMoves.contains(EvidenceRef.normalizeMove(fact.candidateLine.rootMove))
      val referenceIsPlayed = playedMoves.contains(EvidenceRef.normalizeMove(fact.referenceLine.rootMove))
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
          line.role == LineNodeRole.Played && playedMoves.contains(EvidenceRef.normalizeMove(line.rootMove))
        )
      val playedTransitionBinding =
        record.ref.scope == EvidenceScope.PlayedTransition &&
          claim.subjectMove.exists(move => playedMoves.contains(EvidenceRef.normalizeMove(move)) && recordMentionsMove(record, move))
      (localLineBinding || playedTransitionBinding) && directEvidencePayload(record.payload)
    }

  def recordMentionsMove(record: EvidenceRecord, move: String): Boolean =
    record.payload match
      case MoveTransitionEvidence(moveUci, _, _, _) =>
        EvidenceRef.sameMove(moveUci, move)
      case payload: StructuralDeltaEvidence =>
        EvidenceRef.sameMove(payload.moveUci, move)
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(EvidenceRef.sameMove(_, move)) ||
          payload.line.exists(line => EvidenceRef.sameMove(line.rootMove, move)) ||
          record.ref.scope == EvidenceScope.PlayedTransition
      case payload: RelationFactEvidence =>
        payload.mentionsLineMove(move) || record.ref.scope == EvidenceScope.PlayedTransition
      case _ =>
        record.ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, move))

  private def directEvidencePayload(payload: EvidencePayload): Boolean =
    payload match
      case CandidateComparisonEvidence(_) | RelativeAssessmentEvidence(_) | RelativeCauseFactEvidence(_) =>
        false
      case _ =>
        true

  def primaryPlayed(binding: SubjectBindingClass): Boolean =
    binding == SubjectBindingClass.PrimaryPlayedCause

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
    else PlayerFacingClaimPolicy.tierForNonCauseClaim(claim, graph, playedMoves)

object PlayerFacingClaimPolicy:

  def tierForNonCauseClaim(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingClaimTier =
    if playedMoves.isEmpty then PlayerFacingClaimTier.Diagnostic
    else
      val evidenceRecords = claim.evidence.flatMap(graph.record)
      val hasRelativeCause = evidenceRecords.exists(_.payload.isInstanceOf[RelativeCauseFactEvidence])
      if hasRelativeCause || claim.family != ClaimFamily.Evaluation then
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
        playedMoves.contains(EvidenceRef.normalizeMove(assessment.played.moveUci)) &&
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
      EvidenceLayer.TacticalMechanism,
      EvidenceLayer.RelativeCause,
      EvidenceLayer.RelativeAssessment,
      EvidenceLayer.CandidateComparison
    )

  private val longTermExcludedLayers: Set[EvidenceLayer] =
    causeBoundLayers ++ Set(
      EvidenceLayer.Line,
      EvidenceLayer.Eval
    )

  private[chessjudgment] def longTermSupportExcludedLayer(layer: EvidenceLayer): Boolean =
    longTermExcludedLayers.contains(layer)

  def semanticAnchors(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceSemanticAnchor] =
    claim.evidence
      .flatMap(ref => graph.byId.get(ref.id))
      .flatMap(semanticAnchorsForRecord)
      .distinctBy(_.stableKey)
      .sortBy(_.stableKey)

  private def semanticAnchorsForRecord(record: EvidenceRecord): List[EvidenceSemanticAnchor] =
    import EvidenceSemanticAnchorKind.*
    record.payload match
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
      case payload: RelationFactEvidence =>
        payload.semanticGroupingAnchors
      case payload: StructuralDeltaEvidence =>
        payload.consequenceAnchors.map(anchor =>
          EvidenceSemanticAnchor.of(StructuralDelta, s"consequence:$anchor")
        ).distinctBy(_.stableKey)
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

  def carrierRef: EvidenceRef =
    this match
      case CandidateComparison(value, _) => value

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
):
  require(
    evidence.map(_.id).distinct.size == evidence.size,
    "a claim must not repeat or collide evidence owners"
  )

enum PlayerFacingPositionAction:
  case AutomaticTerminal(value: lila.chessjudgment.model.line.AutomaticTerminal)
  case ForcedSingleMove(
      moveUci: String,
      mover: chess.Color,
      supportingEvaluation: CandidateLineEvaluation,
      drawClaims: List[DrawClaimAction]
  )
  case DominantDrawClaim(claims: List[DrawClaimAction])

enum PlayerFacingPrimary:
  case MoveVerdict(comparisonEvidence: EvidenceRef)
  case BestChoice(comparisonEvidence: EvidenceRef)

final class EvidenceBackedJudgmentPacket private (
    private[chessjudgment] val assembly: JudgmentAssemblyContext,
    val primary: PlayerFacingPrimary,
    val probeRequests: List[ProbeRequest],
    val playerFacingClaimDecisions: List[PlayerFacingClaimDecision],
    val causeExposureResolution: PlayerFacingCauseExposureResolution,
    val causeDispositionLedger: CauseDispositionLedger
):
  def candidateLines: List[CandidateLineNode] = assembly.lines

  def evidenceGraph: TypedEvidenceGraph = assembly.evidenceGraph

object EvidenceBackedJudgmentPacket:
  private[chessjudgment] def fromAssembly(
      assembly: JudgmentAssemblyContext,
      primary: PlayerFacingPrimary,
      probeRequests: List[ProbeRequest],
      playerFacingClaimDecisions: List[PlayerFacingClaimDecision],
      causeExposureResolution: PlayerFacingCauseExposureResolution,
      causeDispositionLedger: CauseDispositionLedger
  ): Option[EvidenceBackedJudgmentPacket] =
    Option.when(
      structurallyClosed(assembly) &&
        primaryClosed(assembly, primary) &&
        probesClosed(assembly, probeRequests) &&
        playerFacingClaimsClosed(
          assembly,
          playerFacingClaimDecisions,
          causeExposureResolution,
          causeDispositionLedger
        )
    )(
      new EvidenceBackedJudgmentPacket(
        assembly,
        primary,
        probeRequests,
        playerFacingClaimDecisions,
        causeExposureResolution,
        causeDispositionLedger
      )
    )

  private def primaryClosed(
      assembly: JudgmentAssemblyContext,
      primary: PlayerFacingPrimary
  ): Boolean =
    assembly.relativeAssessments match
      case assessment :: Nil =>
        val comparisonEvidence = primary match
          case PlayerFacingPrimary.MoveVerdict(ref) => ref
          case PlayerFacingPrimary.BestChoice(ref)  => ref
        assembly.evidenceGraph.candidateComparisonRecord(comparisonEvidence).exists {
          case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
            primary match
              case PlayerFacingPrimary.MoveVerdict(_) =>
                fact.kind == CandidateComparisonKind.PlayedVsBest &&
                  fact.referenceLine == assessment.reference.ref &&
                  fact.candidateLine == assessment.candidate.ref &&
                  fact.hasDistinctRootMoves
              case PlayerFacingPrimary.BestChoice(_) =>
                fact.kind == CandidateComparisonKind.BestVsSecond &&
                  EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove) &&
                  EvidenceRef.sameMove(assessment.candidate.ref.rootMove, assessment.reference.ref.rootMove) &&
                  fact.referenceLine == assessment.reference.ref &&
                  fact.candidateLine.role == LineNodeRole.Alternative &&
                  fact.candidateLine.rank == 2 &&
                  fact.hasDistinctRootMoves &&
                  fact.candidateSet.nonEmpty
          case _ => false
        }
      case _ => false

  private def playerFacingClaimsClosed(
      assembly: JudgmentAssemblyContext,
      decisions: List[PlayerFacingClaimDecision],
      exposure: PlayerFacingCauseExposureResolution,
      dispositionLedger: CauseDispositionLedger
  ): Boolean =
    val claimIds = assembly.claims.map(_.id).toSet
    val decisionsClosed = decisions.map(_.claimId).distinct.size == decisions.size &&
      decisions.forall(decision =>
        claimIds(decision.claimId) && decision.causeSelections.forall(selection =>
          assembly.evidenceGraph.record(selection.causeEvidence).exists {
            case EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => true
            case _                                                   => false
          }
        )
      )
    decisionsClosed &&
      dispositionLedger.closedFor(
        assembly.evidenceGraph,
        exposure,
        claimIds
      )

  private def probesClosed(
      assembly: JudgmentAssemblyContext,
      requests: List[ProbeRequest]
  ): Boolean =
    val ids = requests.map(_.id.trim)
    ids.forall(_.nonEmpty) &&
      ids.distinct.size == ids.size &&
      requests.forall { request =>
        ProbeContractValidator.validateRequest(request).isValid &&
          assembly.root.exists { root =>
            request.variant match
              case ProbeVariant.BranchReply(replyMove, requiredHorizonPlyOffset) =>
                assembly.lines.exists { line =>
                  EvidenceRef.sameMove(line.ref.rootMove, request.candidateMove) &&
                    line.evaluation.engineLine.exists { engineLine =>
                      assembly.lineReplay(line.ref).flatMap(_.replaySteps.headOption).exists { rootStep =>
                        val expectedHash = BranchReplyProbeBinding.variationHash(
                          rootFen = root.fen,
                          role = line.ref.role,
                          rootMove = line.ref.rootMove,
                          whitePovEvalCp = engineLine.scoreCp,
                          mate = engineLine.mate,
                          depth = engineLine.depth,
                          moves = engineLine.moves,
                          replyMove = replyMove,
                          certifiedHorizonPlyOffset = requiredHorizonPlyOffset
                        )
                        PrincipalVariationEvidence.sameBoardState(rootStep.fenBefore, root.fen) &&
                          PrincipalVariationEvidence.sameBoardState(rootStep.fenAfter, request.fen) &&
                          assembly.evidenceGraph
                            .certifiedRootResponseMovesFor(line.ref)
                            .exists(_.exists(EvidenceRef.sameMove(_, replyMove))) &&
                          request.variationHash == expectedHash &&
                          request.id == ProbeRequest.transportId(ProbeKind.BranchReply, expectedHash)
                      }
                    }
                }
          }
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
        payload: LineFactEvidence
    ): Boolean =
      val expectedReplay = assembly.lineReplay(line.ref).map(_.replaySteps)
      val actualReplay =
        payload.lineReplaySteps.map(step =>
          step.copy(moveUci = EvidenceRef.normalizeMove(step.moveUci))
        )
      val lineMatches = payload.line == line.ref
      val movesMatch =
        normalizedMoves(payload.lineReplayMoves) == normalizedMoves(line.evaluation.moves)
      val replayMatches = expectedReplay.contains(actualReplay)
      lineMatches && movesMatch && replayMatches
    def exactLineEvidence(line: CandidateLineNode): Boolean =
      val rootMoveMatches =
        line.evaluation.moves.headOption.exists(move =>
          EvidenceRef.sameMove(move, line.ref.rootMove)
        )
      registeredById.get(line.evidence.id).exists {
        case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          ref == line.evidence &&
            ref.producer == EvidenceProducer.LegalLineProducer &&
            ref.layer == EvidenceLayer.Line &&
            ref.line.contains(line.ref) &&
            ref.scope == line.role.scope &&
            rootMoveMatches &&
            exactLineReplay(line, payload)
        case _ =>
          false
      }
    def exactEvaluationEvidence(line: CandidateLineNode): Boolean =
      graphRecords.collect {
        case record @ EvidenceRecord(_, CandidateLineEvaluationEvidence(payloadLine, _), _)
            if payloadLine == line.ref =>
          record
      } match
        case List(EvidenceRecord(ref, CandidateLineEvaluationEvidence(_, evaluation), parents)) =>
          val exactAuthority = evaluation match
            case CandidateLineEvaluation.EngineSearch(_) =>
              ref.producer == EvidenceProducer.EngineEvalProducer &&
                ref.confidence == EvidenceConfidence.EngineBacked
            case CandidateLineEvaluation.ExactAutomaticTerminal(_, _) =>
              ref.producer == EvidenceProducer.LegalLineProducer &&
                ref.confidence == EvidenceConfidence.LegalReplayVerified
          exactAuthority &&
            ref.layer == EvidenceLayer.Eval &&
            ref.position == line.evidence.position &&
            ref.line.contains(line.ref) &&
            ref.scope == line.role.scope &&
            evaluation == line.evaluation &&
            parents == List(line.evidence)
        case _ =>
          false
    def exactTransitionEvidence(transition: MoveTransitionEdge): Boolean =
      positionRefs.contains(transition.from) &&
        positionRefs.contains(transition.to) &&
        registeredById.get(transition.evidence.id).exists {
          case record @ EvidenceRecord(ref, payload @ MoveTransitionEvidence(moveUci, from, to, _), parents) =>
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
              payload.canonicalTransitionProof.exists(
                _.provesMove(moveUci, from, to, ref.scope)
              ) &&
              transition.matches(record)
          case _ =>
            false
        }
    val nodeAndTransitionEvidenceClosed =
      assembly.lines.forall(line => exactLineEvidence(line) && exactEvaluationEvidence(line)) &&
        assembly.transitions.forall(exactTransitionEvidence)
    val passedPawnResultEvidenceClosed =
      graphRecords.forall {
        case EvidenceRecord(ref, event: PassedPawnResultEventEvidence, _) =>
          ref.line.contains(event.rootLine) &&
            event.rootTransition.line.contains(event.rootLine)
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
            case EvidenceRecord(evalRef, CandidateLineEvaluationEvidence(payloadLine, _), _)
                if payloadLine == line.ref =>
              evalRef
          }
      ).distinctBy(_.id)
    val expectedCandidateSet =
      for
        complete <- assembly.input.completeCandidateSet
        reference <- assembly.line(LineNodeRole.BestReference)
        if EvidenceRef.sameMove(reference.ref.rootMove, complete.bestMoveUci)
        second <- assembly.lines.find(line => EvidenceRef.sameMove(line.ref.rootMove, complete.secondMoveUci))
        played <- assembly.line(LineNodeRole.Played)
      yield
        val ownerCandidate =
          if EvidenceRef.sameMove(second.ref.rootMove, played.ref.rootMove) then played
          else second
        (reference, ownerCandidate, complete.descriptor)
    val candidateSetClosed =
      expectedCandidateSet match
        case None =>
          candidateSetOwners.isEmpty
        case Some((reference, ownerCandidate, descriptor)) =>
          candidateSetOwners match
            case List((ownerRecord, ownerFact)) =>
              val candidateSetParents =
                List(reference, ownerCandidate)
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
              candidateSetParents.forall(ownerRecord.parents.contains) &&
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
              expectedCandidateSet.toList
                .flatMap { case (reference, ownerCandidate, _) =>
                  List(reference, ownerCandidate)
                }
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
                  (fact.kind match
                    case CandidateComparisonKind.PlayedVsBest =>
                      fact.referenceLine == assessment.reference.ref &&
                        fact.candidateLine == assessment.candidate.ref &&
                        fact.hasDistinctRootMoves
                    case CandidateComparisonKind.BestVsSecond =>
                      EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove) &&
                        EvidenceRef.sameMove(assessment.candidate.ref.rootMove, assessment.reference.ref.rootMove) &&
                        fact.referenceLine == assessment.reference.ref &&
                        fact.candidateLine.role == LineNodeRole.Alternative &&
                        fact.candidateLine.rank == 2 &&
                        fact.hasDistinctRootMoves &&
                        fact.candidateSet.nonEmpty
                    case CandidateComparisonKind.PlayedVsAlternative |
                        CandidateComparisonKind.ReferenceVsAlternative =>
                      false)
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
      passedPawnResultEvidenceClosed &&
      claimsClosed &&
      causesClosed &&
      assessmentsClosed &&
      comparisonPairsUnique &&
      candidateSetClosed &&
      comparisonParentsCanonical &&
      parentDagAcyclic
