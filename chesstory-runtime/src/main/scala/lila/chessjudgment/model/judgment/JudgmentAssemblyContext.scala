package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.model.{ ProbeAdmissionDiagnostic, ProbeResult }
import lila.chessjudgment.model.strategic.EngineLine

final case class NormalizedCandidateLine(
    role: LineNodeRole,
    rank: Int,
    line: EngineLine
):
  def rootMove: Option[String] =
    line.moves.headOption.map(_.trim.toLowerCase)

final case class NormalizedThreatBranch(
    sourceProbeId: String,
    probedMoveUci: String,
    branchFen: String,
    branchPly: Int,
    opponentResourceMove: Option[String],
    certifiedHorizonPlyOffset: Option[Int],
    lines: List[NormalizedCandidateLine]
):
  def rankedUniqueLines: List[NormalizedCandidateLine] =
    lines.sortBy(_.rank).distinctBy(_.rootMove)

final case class NormalizedMoveReviewInput(
    beforeFen: String,
    playedMoveUci: String,
    beforePly: Int,
    sideToMove: Option[Color],
    afterPlayedFen: String,
    afterReferenceFen: Option[String],
    lines: List[NormalizedCandidateLine],
    opening: Option[OpeningIdentity],
    movePrefixUci: List[String] = Nil,
    openingRecognition: Option[OpeningRecognition] = None,
    openingThemePriorSelection: Option[OpeningThemePriorSelection] = None,
    openingSignals: List[OpeningContextSignal] = Nil,
    threatBranches: List[NormalizedThreatBranch] = Nil,
    endgameTablebaseResults: List[ProbeResult] = Nil,
    probeDiagnostics: List[ProbeAdmissionDiagnostic] = Nil
):
  def playedLine: Option[NormalizedCandidateLine] =
    lines.find(_.role == LineNodeRole.Played)

  def referenceLine: Option[NormalizedCandidateLine] =
    lines.find(_.role == LineNodeRole.BestReference)

  def rankedUniqueLines: List[NormalizedCandidateLine] =
    lines.sortBy(_.rank).distinctBy(_.rootMove)

final case class JudgmentAssemblyContext(
    input: NormalizedMoveReviewInput,
    positions: List[PositionNode],
    lines: List[CandidateLineNode],
    transitions: List[MoveTransitionEdge],
    evidenceGraph: TypedEvidenceGraph,
    claims: List[JudgmentClaim],
    probeDiagnostics: List[ProbeAdmissionDiagnostic] = Nil
):
  def position(role: PositionNodeRole): Option[PositionNode] =
    exactlyOne(positions.filter(_.role == role))

  def line(role: LineNodeRole): Option[CandidateLineNode] =
    exactlyOne(lines.filter(_.role == role))

  def transition(role: TransitionEdgeRole): Option[MoveTransitionEdge] =
    exactlyOne(transitions.filter(_.role == role))

  def playedTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Played)

  def referenceTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Reference)

  def relativeAssessments: List[RelativeMoveAssessment] =
    evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) => assessment
    }

  def root: Option[PositionNodeRef] =
    position(PositionNodeRole.Before).map(_.ref)

  def withPosition(node: PositionNode): JudgmentAssemblyContext =
    appendWithoutCollision(
      items = positions,
      item = node,
      sameIdentity = (left, right) => left.ref == right.ref,
      sameExclusiveRole = (left, right) =>
        left.role == right.role && Set(
          PositionNodeRole.Before,
          PositionNodeRole.AfterPlayed,
          PositionNodeRole.AfterReference
        )(left.role),
      identity = s"position '${node.ref.id.getOrElse(node.ref.fen)}'",
      role = s"position role '${node.role}'"
    ).map(updated => copy(positions = updated)).getOrElse(this)

  def withLine(line: CandidateLineNode): JudgmentAssemblyContext =
    appendWithoutCollision(
      items = lines,
      item = line,
      sameIdentity = (left, right) => left.ref.id == right.ref.id,
      sameExclusiveRole = (left, right) =>
        left.role == right.role && Set(LineNodeRole.Played, LineNodeRole.BestReference)(left.role),
      identity = s"line '${line.ref.id}'",
      role = s"line role '${line.role}'"
    ).map(updated => copy(lines = updated)).getOrElse(this)

  def withTransition(edge: MoveTransitionEdge): JudgmentAssemblyContext =
    appendWithoutCollision(
      items = transitions,
      item = edge,
      sameIdentity = (left, right) => left.evidence.id == right.evidence.id,
      sameExclusiveRole = (left, right) =>
        left.role == right.role && Set(TransitionEdgeRole.Played, TransitionEdgeRole.Reference)(left.role),
      identity = s"transition evidence '${edge.evidence.id}'",
      role = s"transition role '${edge.role}'"
    ).map(updated => copy(transitions = updated)).getOrElse(this)

  def withEvidence(record: EvidenceRecord): JudgmentAssemblyContext =
    copy(evidenceGraph = evidenceGraph.add(record))

  def withEvidence(records: List[EvidenceRecord]): JudgmentAssemblyContext =
    copy(evidenceGraph = records.foldLeft(evidenceGraph)((graph, record) => graph.add(record)))

  def withClaim(claim: JudgmentClaim): JudgmentAssemblyContext =
    claims.find(_.id == claim.id) match
      case None =>
        copy(claims = claims :+ claim)
      case Some(existing) if existing == claim =>
        this
      case Some(_) =>
        throw IllegalArgumentException(
          s"claim id collision for '${claim.id}': existing claim differs from the attempted addition"
        )

  private def exactlyOne[A](items: List[A]): Option[A] =
    items match
      case item :: Nil => Some(item)
      case _           => None

  private def appendWithoutCollision[A](
      items: List[A],
      item: A,
      sameIdentity: (A, A) => Boolean,
      sameExclusiveRole: (A, A) => Boolean,
      identity: String,
      role: String
  ): Option[List[A]] =
    items.find(existing => sameIdentity(existing, item) || sameExclusiveRole(existing, item)) match
      case None =>
        Some(items :+ item)
      case Some(existing) if existing == item =>
        None
      case Some(existing) if sameIdentity(existing, item) =>
        throw IllegalArgumentException(s"$identity collision: existing value differs from the attempted addition")
      case Some(_) =>
        throw IllegalArgumentException(s"$role collision: the role already has a different authoritative value")

object JudgmentAssemblyContext:
  def empty(
      input: NormalizedMoveReviewInput,
      evidenceGraph: TypedEvidenceGraph = TypedEvidenceGraph.empty
  ): JudgmentAssemblyContext =
    JudgmentAssemblyContext(
      input = input,
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      evidenceGraph = evidenceGraph,
      claims = Nil,
      probeDiagnostics = input.probeDiagnostics
    )
