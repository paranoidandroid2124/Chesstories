package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.ProbeAdmissionDiagnostic

final case class JudgmentAssemblyContext(
    positions: List[PositionNode],
    lines: List[CandidateLineNode],
    transitions: List[MoveTransitionEdge],
    relativeAssessments: List[RelativeMoveAssessment],
    evidenceGraph: TypedEvidenceGraph,
    ideas: List[ChessIdea],
    claims: List[ClaimSeed],
    claimLifecycle: List[ClaimLifecycleDiagnostic] = Nil,
    probeDiagnostics: List[ProbeAdmissionDiagnostic] = Nil
):
  def position(role: PositionNodeRole): Option[PositionNode] =
    positions.find(_.role == role)

  def line(role: LineNodeRole): Option[CandidateLineNode] =
    lines.find(_.role == role)

  def transition(role: TransitionEdgeRole): Option[MoveTransitionEdge] =
    transitions.find(_.role == role)

  def playedTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Played)

  def referenceTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Reference)

  def root: Option[PositionNodeRef] =
    position(PositionNodeRole.Before).map(_.ref)

  def withPosition(node: PositionNode): JudgmentAssemblyContext =
    copy(positions = replaceBy(positions, node)(_.ref))

  def withLine(line: CandidateLineNode): JudgmentAssemblyContext =
    copy(lines = replaceBy(lines, line)(_.ref))

  def withTransition(edge: MoveTransitionEdge): JudgmentAssemblyContext =
    copy(transitions = replaceBy(transitions, edge)(_.evidence.id))

  def withRelativeAssessment(assessment: RelativeMoveAssessment): JudgmentAssemblyContext =
    copy(relativeAssessments = replaceBy(relativeAssessments, assessment)(_.evidence.id))

  def withEvidence(record: EvidenceRecord): JudgmentAssemblyContext =
    copy(evidenceGraph = evidenceGraph.add(record))

  def withEvidence(records: List[EvidenceRecord]): JudgmentAssemblyContext =
    copy(evidenceGraph = records.foldLeft(evidenceGraph)((graph, record) => graph.add(record)))

  def withIdea(idea: ChessIdea): JudgmentAssemblyContext =
    copy(ideas = replaceBy(ideas, idea)(_.ref))

  def withClaim(claim: ClaimSeed): JudgmentAssemblyContext =
    copy(claims = replaceBy(claims, claim)(_.id))

  def withClaimLifecycle(lifecycle: List[ClaimLifecycleDiagnostic]): JudgmentAssemblyContext =
    copy(claimLifecycle = lifecycle)

  private def replaceBy[A, K](items: List[A], item: A)(key: A => K): List[A] =
    items.filterNot(existing => key(existing) == key(item)) :+ item

object JudgmentAssemblyContext:
  def empty(evidenceGraph: TypedEvidenceGraph = TypedEvidenceGraph.empty): JudgmentAssemblyContext =
    JudgmentAssemblyContext(
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      relativeAssessments = Nil,
      evidenceGraph = evidenceGraph,
      ideas = Nil,
      claims = Nil,
      claimLifecycle = Nil,
      probeDiagnostics = Nil
    )
