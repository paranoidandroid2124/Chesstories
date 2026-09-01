package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction }

enum ClaimFamily:
  case BoundedCausal
  case PawnStructure
  case PassedPawnProgress
  case Evaluation

  private[chessjudgment] def isLongTerm: Boolean =
    this match
      case PawnStructure => true
      case BoundedCausal | PassedPawnProgress | Evaluation => false

object ClaimFamily:
  def fromCause(kind: RelativeCauseKind): ClaimFamily =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.MissedSquareRelease |
          RelativeCauseKind.WrongMoveOrder =>
        ClaimFamily.BoundedCausal
      case RelativeCauseKind.PassedPawnProgress =>
        ClaimFamily.PassedPawnProgress

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
    graph.comparisonFor(cause).filter(ActionablePlayedVsBestCausalProofDemand.accepts).map { fact =>
      val binding = graph.requiredRelativeCauseBinding(cause)
      val candidateIsPlayed = playedMoves.contains(EvidenceRef.normalizeMove(fact.candidateLine.rootMove))
      val referenceIsPlayed = playedMoves.contains(EvidenceRef.normalizeMove(fact.referenceLine.rootMove))
      if
        candidateIsPlayed && !referenceIsPlayed &&
          RelativeCauseConstructionAdmission.initiallyReady(cause, graph) &&
          binding.evidenceLines == List(binding.eventLine)
      then SubjectBindingClass.PrimaryPlayedCause
      else SubjectBindingClass.Other
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
        baseTier

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
        Option
          .when(fact.kind == CandidateComparisonKind.BestVsSecond)(
            EvidenceSemanticAnchor.of(CandidateComparison, "best-vs-second")
          )
          .toList
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
      playerFacingClaimDecisions: List[PlayerFacingClaimDecision],
      causeExposureResolution: PlayerFacingCauseExposureResolution,
      causeDispositionLedger: CauseDispositionLedger
  ): Option[EvidenceBackedJudgmentPacket] =
    Option.when(
      structurallyClosed(assembly) &&
        primaryClosed(assembly, primary) &&
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
                  fact.hasDistinctRootMoves
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
    def canonicalLineParents(line: CandidateLineNode): List[EvidenceRef] =
      line.evidence ::
        graphRecords.collect {
          case EvidenceRecord(evalRef, CandidateLineEvaluationEvidence(payloadLine, _), _)
              if payloadLine == line.ref =>
            evalRef
        }
    val comparisonParentsCanonical =
      comparisonRecords.forall { case (record, fact) =>
        val comparedLineParents =
          List(fact.referenceLine, fact.candidateLine)
            .flatMap(comparisonLinesByRef.get)
            .flatMap(canonicalLineParents)
        val parentIds = comparedLineParents.map(_.id)
        parentIds.distinct.size == parentIds.size &&
          record.parents.size == comparedLineParents.size &&
          record.parents.toSet == comparedLineParents.toSet
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
          assembly.evidenceGraph.comparisonFor(cause).exists(comparison =>
            val binding = assembly.evidenceGraph.requiredRelativeCauseBinding(cause)
            ActionablePlayedVsBestCausalProofDemand.accepts(comparison) &&
              RelativeCauseConstructionAdmission.initiallyReady(cause, assembly.evidenceGraph) &&
              binding.evidenceLines == List(binding.eventLine) &&
              comparisonFactBound(cause.comparisonEvidence) &&
              parents.contains(cause.comparisonEvidence) &&
              lineRefs.contains(binding.eventLine) &&
              (cause.comparisonEvidence :: cause.proofSources).forall(registered)
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
          List(
            assessment.played.evidence,
            assessment.reference.evidence,
            assessment.candidate.evidence
          ) ++
            assessment.referenceTransition.toList.map(_.evidence) ++
            comparisonRefs ++
            assessment.relativeCauseEvidence
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
                        fact.hasDistinctRootMoves
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
        expectedParents.map(_.id).distinct.size == expectedParents.size &&
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
      comparisonParentsCanonical &&
      parentDagAcyclic
