package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.position.PositionAnalysis
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }

final case class AdmittedLegalLine(
    rootMove: String,
    replay: CanonicalLineReplay
):
  require(rootMove.nonEmpty, "an admitted LegalLine needs its canonical root move")
  require(
    rootMove == EvidenceRef.normalizeMove(rootMove),
    "an admitted LegalLine root move must be canonical"
  )
  require(
    replay.replaySteps.headOption.exists(step => EvidenceRef.sameMove(step.moveUci, rootMove)),
    "an admitted LegalLine replay must begin with its declared root move"
  )

final case class AdmittedAssessmentCandidate(
    lineRootMove: String,
    role: LineNodeRole,
    rank: Int,
    evaluation: CandidateLineEvaluation
):
  require(rank >= 1, "an assessment candidate rank must be positive")
  require(lineRootMove.nonEmpty, "an assessment candidate needs its LegalLine root move")
  require(
    lineRootMove == EvidenceRef.normalizeMove(lineRootMove),
    "an assessment candidate LegalLine root move must be canonical"
  )

  def rootMove: Option[String] =
    evaluation.moves.headOption.map(_.trim.toLowerCase)

  require(
    rootMove.exists(EvidenceRef.sameMove(_, lineRootMove)),
    "an assessment candidate must reference the LegalLine with the same root move"
  )

final case class LineRootOccurrence(
    start: PositionNodeRef,
    destination: PositionNodeRef,
    transitionEvidenceId: String
):
  require(transitionEvidenceId.trim.nonEmpty, "a line-root occurrence requires its exact transition")

final case class AdmittedMoveReviewInput(
    beforeFen: String,
    playedMoveUci: String,
    beforePly: Int,
    sideToMove: Option[Color],
    afterPlayedFen: String,
    afterReferenceFen: Option[String],
    legalLines: List[AdmittedLegalLine],
    assessmentCandidates: List[AdmittedAssessmentCandidate],
    admittedRootRankingPair: Option[AdmittedRootRankingPair],
    positionHistory: CanonicalPositionHistory
):
  val legalRootMoves: List[String] = positionHistory.currentLegalMoveUcis
  val admittedRootMoves: List[String] = legalLines.map(line => EvidenceRef.normalizeMove(line.rootMove))
  val rootMovesWithoutAdmittedLine: List[String] =
    legalRootMoves.filterNot(admittedRootMoves.contains)

  require(
    admittedRootMoves.size == legalLines.size && admittedRootMoves.distinct.size == admittedRootMoves.size,
    "one root move must have exactly one admitted LegalLine owner"
  )
  require(
    admittedRootMoves.forall(legalRootMoves.contains),
    "every admitted LegalLine root must belong to the legal root move universe"
  )
  require(
    legalRootMoves.contains(EvidenceRef.normalizeMove(playedMoveUci)),
    "the observed move must belong to the legal root move universe"
  )
  require(
    assessmentCandidates.map(candidate => EvidenceRef.normalizeMove(candidate.lineRootMove)).distinct.size ==
      assessmentCandidates.size,
    "one admitted LegalLine may have at most one assessment projection"
  )
  require(
    assessmentCandidates.forall { candidate =>
      legalLines.filter(line => EvidenceRef.sameMove(line.rootMove, candidate.lineRootMove)) match
        case line :: Nil =>
          line.replay.replaySteps.map(step => EvidenceRef.normalizeMove(step.moveUci)) ==
            candidate.evaluation.moves.map(EvidenceRef.normalizeMove)
        case _ => false
    },
    "every assessment candidate must bind the full replay of one admitted LegalLine"
  )
  require(
    assessmentCandidates.count(candidate => EvidenceRef.sameMove(candidate.lineRootMove, playedMoveUci)) == 1,
    "the observed move needs exactly one assessment projection"
  )
  require(
    assessmentCandidates.count(_.role == LineNodeRole.BestReference) == 1,
    "an admitted review needs exactly one assessment reference projection"
  )

  def playedRootLineOwner: Option[AdmittedLegalLine] =
    legalLines.filter(line => EvidenceRef.sameMove(line.rootMove, playedMoveUci)) match
      case line :: Nil => Some(line)
      case _           => None

  def referenceCandidate: Option[AdmittedAssessmentCandidate] =
    assessmentCandidates.filter(_.role == LineNodeRole.BestReference) match
      case line :: Nil => Some(line)
      case _           => None

  def legalLineFor(candidate: AdmittedAssessmentCandidate): Option[AdmittedLegalLine] =
    legalLines.filter(line => EvidenceRef.sameMove(line.rootMove, candidate.lineRootMove)) match
      case line :: Nil => Some(line)
      case _           => None

final case class JudgmentAssemblyContext(
    input: AdmittedMoveReviewInput,
    positions: List[PositionNode],
    positionAnalyses: Map[PositionNodeRef, PositionAnalysis] = Map.empty,
    legalLines: List[LegalLineNode],
    lines: List[CandidateLineNode],
    transitions: List[MoveTransitionEdge],
    lineReplays: Map[LineNodeRef, CanonicalLineReplay] = Map.empty,
    transitionReplays: Map[String, CanonicalLineReplay] = Map.empty,
    lineRootOccurrences: Map[LineNodeRef, LineRootOccurrence] = Map.empty,
    evidenceGraph: TypedEvidenceGraph
):
  def position(role: PositionNodeRole): Option[PositionNode] =
    exactlyOne(positions.filter(_.role == role))

  def line(role: LineNodeRole): Option[CandidateLineNode] =
    exactlyOne(lines.filter(_.role == role))

  def lineForRootMove(moveUci: String): Option[CandidateLineNode] =
    exactlyOne(lines.filter(line => EvidenceRef.sameMove(line.ref.rootMove, moveUci)))

  private[chessjudgment] def lineReplay(line: LineNodeRef): Option[CanonicalLineReplay] =
    lineReplays.get(line)

  private[chessjudgment] def positionAnalysis(position: PositionNodeRef): Option[PositionAnalysis] =
    positionAnalyses.get(position)

  private[chessjudgment] def transitionReplay(edge: MoveTransitionEdge): Option[CanonicalLineReplay] =
    transitionReplays.get(edge.evidence.id)

  private[chessjudgment] def lineRootOccurrence(
      line: LineNodeRef
  ): Option[LineRootOccurrence] =
    lineRootOccurrences.get(line)

  private[chessjudgment] def lineForRootTransition(
      edge: MoveTransitionEdge
  ): Option[LegalLineNode] =
    lineRootOccurrences.collect {
      case (line, occurrence) if occurrence.transitionEvidenceId == edge.evidence.id => line
    }.toList match
      case lineRef :: Nil => legalLines.find(_.ref == lineRef)
      case Nil            => None
      case _ =>
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' is owned by more than one line-root occurrence"
        )

  private[chessjudgment] def lineForTransition(
      edge: MoveTransitionEdge
  ): Option[LegalLineNode] =
    edge.evidence.line match
      case Some(line) =>
        legalLines.filter(_.ref == line) match
          case exact :: Nil => Some(exact)
          case Nil         => None
          case _ =>
            throw IllegalArgumentException(
              s"transition '${edge.evidence.id}' has more than one LegalLine owner"
            )
      case None => lineForRootTransition(edge)

  def transition(role: TransitionEdgeRole): Option[MoveTransitionEdge] =
    exactlyOne(transitions.filter(_.role == role))

  def playedTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Played)

  def referenceTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Reference)

  def root: Option[PositionNodeRef] =
    position(PositionNodeRole.Before).map(_.ref)

  /** Appends a known-new occurrence batch after one linear collision check. */
  private[chessjudgment] def withNewPositions(
      additions: List[(PositionNode, PositionAnalysis)]
  ): JudgmentAssemblyContext =
    if additions.isEmpty then this
    else
      val nodes = additions.map(_._1)
      val refs = nodes.map(_.ref)
      val existingRefs = positions.iterator.map(_.ref).toSet
      val exclusiveRoles = Set(
        PositionNodeRole.Before,
        PositionNodeRole.AfterPlayed,
        PositionNodeRole.AfterReference
      )
      val claimedExclusiveRoles = (positions ++ nodes).collect {
        case node if exclusiveRoles(node.role) => node.role
      }
      require(
        additions.forall((node, analysis) => positionAnalysisMatches(node, analysis)),
        "every batched position analysis must describe its registered occurrence"
      )
      require(
        refs.distinct.size == refs.size && refs.forall(ref => !existingRefs(ref)) &&
          refs.forall(ref => !positionAnalyses.contains(ref)),
        "a batched position occurrence must be new and uniquely owned"
      )
      require(
        claimedExclusiveRoles.distinct.size == claimedExclusiveRoles.size,
        "a batched position cannot collide with an exclusive position role"
      )
      copy(
        positions = positions ++ nodes,
        positionAnalyses = positionAnalyses ++ additions.map { case (node, analysis) => node.ref -> analysis }
      )

  def withLegalLine(line: LegalLineNode, replay: CanonicalLineReplay): JudgmentAssemblyContext =
    require(
      replay.replaySteps.headOption.exists(step => EvidenceRef.sameMove(step.moveUci, line.ref.rootMove)),
      "a LegalLine replay must begin with its registered root move"
    )
    val updated = appendWithoutCollision(
      items = legalLines,
      item = line,
      sameIdentity = (left, right) => left.ref.id == right.ref.id,
      sameExclusiveRole = (_, _) => false,
      identity = s"line '${line.ref.id}'",
      role = "LegalLine owner"
    )
    updated match
      case None =>
        lineReplays.get(line.ref) match
          case Some(existing) if existing.asInstanceOf[AnyRef] eq replay.asInstanceOf[AnyRef] => this
          case Some(_) =>
            throw IllegalArgumentException(s"line '${line.ref.id}' has more than one replay owner")
          case None =>
            throw IllegalArgumentException(s"line '${line.ref.id}' is registered without its replay")
      case Some(accepted) =>
        lineReplays.get(line.ref) match
          case Some(_) => throw IllegalArgumentException(s"line '${line.ref.id}' replay collision")
          case None => copy(legalLines = accepted, lineReplays = lineReplays.updated(line.ref, replay))

  def withCandidateLine(line: CandidateLineNode): JudgmentAssemblyContext =
    require(
      legalLines.exists(owner => owner.ref == line.ref && owner.evidence == line.lineEvidence),
      s"assessment candidate '${line.ref.id}' must reference its registered LegalLine owner"
    )
    val updated = appendWithoutCollision(
      items = lines,
      item = line,
      sameIdentity = (left, right) => left.ref == right.ref,
      sameExclusiveRole = (left, right) =>
        left.role == right.role && Set(LineNodeRole.Played, LineNodeRole.BestReference)(left.role),
      identity = s"assessment line '${line.ref.id}'",
      role = s"assessment line role '${line.role}'"
    )
    updated match
      case None           => this
      case Some(accepted) => copy(lines = accepted)

  /** Appends a known-new transition batch after one linear collision check. */
  private[chessjudgment] def withNewTransitions(
      additions: List[(MoveTransitionEdge, CanonicalLineReplay)]
  ): JudgmentAssemblyContext =
    if additions.isEmpty then this
    else
      val edges = additions.map(_._1)
      val ids = edges.map(_.evidence.id)
      val existingIds = transitions.iterator.map(_.evidence.id).toSet
      val exclusiveRoles = Set(TransitionEdgeRole.Played, TransitionEdgeRole.Reference)
      val claimedExclusiveRoles = (transitions ++ edges).collect {
        case edge if exclusiveRoles(edge.role) => edge.role
      }
      require(
        additions.forall(exactTransitionReplay),
        "every batched transition must reuse its exact admitted line step"
      )
      require(
        ids.distinct.size == ids.size && ids.forall(id => !existingIds(id)) &&
          ids.forall(id => !transitionReplays.contains(id)),
        "a batched transition occurrence must be new and uniquely owned"
      )
      require(
        claimedExclusiveRoles.distinct.size == claimedExclusiveRoles.size,
        "a batched transition cannot collide with an exclusive transition role"
      )
      copy(
        transitions = transitions ++ edges,
        transitionReplays = transitionReplays ++ additions.map { case (edge, replay) => edge.evidence.id -> replay }
      )

  private[chessjudgment] def withLineRootOccurrence(
      line: LineNodeRef,
      edge: MoveTransitionEdge
  ): JudgmentAssemblyContext =
    val exactLine = legalLines.find(_.ref == line)
    val exactEdge = transitions.find(_.evidence.id == edge.evidence.id)
    val exactStep = lineReplays.get(line).flatMap(_.replaySteps.headOption)
    require(
      exactLine.exists(owner =>
        owner.evidence.position == edge.from &&
          EvidenceRef.sameMove(owner.ref.rootMove, edge.moveUci)
      ) &&
        exactEdge.contains(edge) &&
        positions.exists(_.ref == edge.from) &&
        positions.exists(_.ref == edge.to) &&
        transitionReplays.contains(edge.evidence.id) &&
        exactStep.exists(step =>
          step.ply == edge.to.ply &&
            edge.to.ply == edge.from.ply + 1 &&
            EvidenceRef.sameMove(step.moveUci, edge.moveUci) &&
            PrincipalVariationEvidence.sameBoardState(step.fenBefore, edge.from.fen) &&
            PrincipalVariationEvidence.sameBoardState(step.fenAfter, edge.to.fen)
        ),
      s"line '${line.id}' must own its exact root transition occurrence"
    )
    require(
      !lineRootOccurrences.exists { case (otherLine, occurrence) =>
        otherLine != line && occurrence.transitionEvidenceId == edge.evidence.id
      },
      s"transition '${edge.evidence.id}' cannot be shared by line-root occurrences"
    )
    val occurrence = LineRootOccurrence(edge.from, edge.to, edge.evidence.id)
    lineRootOccurrences.get(line) match
      case None => copy(lineRootOccurrences = lineRootOccurrences.updated(line, occurrence))
      case Some(existing) if existing == occurrence => this
      case Some(_) =>
        throw IllegalArgumentException(s"line '${line.id}' has more than one root occurrence")

  def withEvidence(record: EvidenceRecord): JudgmentAssemblyContext =
    copy(evidenceGraph = evidenceGraph.add(record))

  def withEvidence(records: List[EvidenceRecord]): JudgmentAssemblyContext =
    copy(evidenceGraph = evidenceGraph.addAll(records))

  private def exactlyOne[A](items: List[A]): Option[A] =
    items match
      case item :: Nil => Some(item)
      case _           => None

  private def positionAnalysisMatches(
      node: PositionNode,
      analysis: PositionAnalysis
  ): Boolean =
    analysis.occurrence.plyCount == node.ref.ply &&
      analysis.position.color == node.ref.sideToMove.getOrElse(analysis.position.color) &&
      PrincipalVariationEvidence.sameBoardState(analysis.occurrence.fen, node.ref.fen)

  private def exactTransitionReplay(
      edge: MoveTransitionEdge,
      admittedReplay: CanonicalLineReplay
  ): Boolean =
    admittedReplay.replaySteps match
      case step :: Nil =>
        step.ply == edge.to.ply && edge.to.ply == edge.from.ply + 1 &&
          EvidenceRef.sameMove(step.moveUci, edge.moveUci) &&
          PrincipalVariationEvidence.sameBoardState(step.fenBefore, edge.from.fen) &&
          PrincipalVariationEvidence.sameBoardState(step.fenAfter, edge.to.fen)
      case _ => false

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
      input: AdmittedMoveReviewInput,
      evidenceGraph: TypedEvidenceGraph = TypedEvidenceGraph.empty
  ): JudgmentAssemblyContext =
    JudgmentAssemblyContext(
      input = input,
      positions = Nil,
      positionAnalyses = Map.empty,
      legalLines = Nil,
      lines = Nil,
      transitions = Nil,
      lineReplays = Map.empty,
      transitionReplays = Map.empty,
      lineRootOccurrences = Map.empty,
      evidenceGraph = evidenceGraph
    )
