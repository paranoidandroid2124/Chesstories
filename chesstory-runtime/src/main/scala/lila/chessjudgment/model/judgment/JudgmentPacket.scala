package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction }

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
    val primary: PlayerFacingPrimary
):
  def candidateLines: List[CandidateLineNode] = assembly.lines

  def evidenceGraph: TypedEvidenceGraph = assembly.evidenceGraph

object EvidenceBackedJudgmentPacket:
  private[chessjudgment] def fromAssembly(
      assembly: JudgmentAssemblyContext
  ): Option[EvidenceBackedJudgmentPacket] =
    selectPrimary(assembly).map(new EvidenceBackedJudgmentPacket(assembly, _))

  /** Projects only the selected Assessment chain. Unrelated occurrence records
    * retain their own graph-local proof eligibility.
    */
  private def selectPrimary(
      assembly: JudgmentAssemblyContext
  ): Option[PlayerFacingPrimary] =
    val graph = assembly.evidenceGraph
    graph.records.flatMap {
      case assessmentRecord @ EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        primaryFrom(assembly, assessment, assessmentRecord)
      case _ => None
    } match
      case primary :: Nil => Some(primary)
      case _ => None

  private def primaryFrom(
      assembly: JudgmentAssemblyContext,
      assessment: RelativeMoveAssessment,
      assessmentRecord: EvidenceRecord
  ): Option[PlayerFacingPrimary] =
    val graph = assembly.evidenceGraph
    if !exactAssessmentRecord(assembly, assessment, assessmentRecord) ||
        !graph.proofEligible(assessmentRecord)
    then None
    else
      for
        comparisonRecord <- graph.candidateComparisonRecord(
          assessment.primaryComparisonEvidence
        )
        if graph.proofEligible(comparisonRecord)
        fact <- comparisonRecord.payload match
          case CandidateComparisonEvidence(value) => Some(value)
          case _                                  => None
        reference <- ownedEndpoint(assembly, fact.referenceLine)
        candidate <- ownedEndpoint(assembly, fact.candidateLine)
        if CandidateComparisonFact
          .fromLines(fact.kind, fact.comparison.mover, reference, candidate)
          .contains(fact)
        primary <- playerFacingPrimary(
          assessment,
          fact,
          reference,
          candidate,
          comparisonRecord.ref
        )
      yield primary

  private def exactAssessmentRecord(
      assembly: JudgmentAssemblyContext,
      assessment: RelativeMoveAssessment,
      record: EvidenceRecord
  ): Boolean =
    record match
      case EvidenceRecord(ref, RelativeAssessmentEvidence(value), parents) =>
        ref == assessment.evidence &&
          value == assessment &&
          ref.position == assessment.played.from &&
          ref.scope == EvidenceScope.Counterfactual &&
          assessment.canonicalParents.contains(parents) &&
          assembly.lines.contains(assessment.reference) &&
          assembly.lines.contains(assessment.candidate) &&
          assessmentMovesBound(assessment) &&
          exactTransition(assembly, assessment.played) &&
          assessment.referenceTransition.forall(exactTransition(assembly, _))
      case _ => false

  private def assessmentMovesBound(assessment: RelativeMoveAssessment): Boolean =
    EvidenceRef.sameMove(assessment.played.moveUci, assessment.candidate.ref.rootMove) &&
      (assessment.referenceTransition match
        case Some(reference) =>
          reference.from == assessment.played.from &&
            EvidenceRef.sameMove(reference.moveUci, assessment.reference.ref.rootMove)
        case None =>
          EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove))

  private def exactTransition(
      assembly: JudgmentAssemblyContext,
      edge: MoveTransitionEdge
  ): Boolean =
    assembly.transitions.contains(edge) &&
      assembly.evidenceGraph.record(edge.evidence).exists(record =>
        edge.matches(record) && assembly.evidenceGraph.proofEligible(record)
      )

  private def ownedEndpoint(
      assembly: JudgmentAssemblyContext,
      ref: LineNodeRef
  ): Option[CandidateLineNode] =
    assembly.lines.filter(_.ref == ref) match
      case endpoint :: Nil =>
        assembly.legalLines.filter(_.ref == ref) match
          case owner :: Nil if owner.evidence == endpoint.lineEvidence => Some(endpoint)
          case _                                                       => None
      case _ => None

  private def playerFacingPrimary(
      assessment: RelativeMoveAssessment,
      fact: CandidateComparisonFact,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      comparisonEvidence: EvidenceRef
  ): Option[PlayerFacingPrimary] =
    fact.kind match
      case CandidateComparisonKind.PlayedVsBest
          if assessment.reference == reference &&
            assessment.candidate == candidate &&
            reference.role == LineNodeRole.BestReference &&
            candidate.role == LineNodeRole.Played &&
            fact.hasDistinctRootMoves =>
        Some(PlayerFacingPrimary.MoveVerdict(comparisonEvidence))
      case CandidateComparisonKind.BestVsSecond
          if assessment.reference == reference &&
            assessment.candidate == reference &&
            EvidenceRef.sameMove(assessment.played.moveUci, reference.ref.rootMove) &&
            candidate.role == LineNodeRole.Alternative &&
            candidate.rank == 2 &&
            fact.hasDistinctRootMoves =>
        Some(PlayerFacingPrimary.BestChoice(comparisonEvidence))
      case _ => None
