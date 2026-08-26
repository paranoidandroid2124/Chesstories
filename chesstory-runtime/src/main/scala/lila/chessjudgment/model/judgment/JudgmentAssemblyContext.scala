package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.position.PositionAnalysis
import lila.chessjudgment.model.{ ProbeAdmissionDiagnostic, ProbeObjective }
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }

final case class NormalizedCandidateLine(
    role: LineNodeRole,
    rank: Int,
    evaluation: CandidateLineEvaluation,
    replay: CanonicalLineReplay,
    predecessorReplay: Option[CanonicalLineReplay] = None
):
  def rootMove: Option[String] =
    evaluation.moves.headOption.map(_.trim.toLowerCase)

final case class NormalizedThreatBranch(
    sourceProbeId: String,
    objective: ProbeObjective,
    probedMoveUci: String,
    branchFen: String,
    branchPly: Int,
    opponentResourceMove: Option[String],
    certifiedHorizonPlyOffset: Option[Int],
    lines: List[NormalizedCandidateLine],
    continuationMoves: List[String] = Nil
):
  require(
    objective match
      case ProbeObjective.BranchReplyMultiPv =>
        opponentResourceMove.isEmpty && continuationMoves.isEmpty && certifiedHorizonPlyOffset.nonEmpty
      case ProbeObjective.CausalContinuation =>
        opponentResourceMove.isEmpty && continuationMoves.size == 2 && certifiedHorizonPlyOffset.isEmpty
      case ProbeObjective.CounterResource =>
        opponentResourceMove.nonEmpty && continuationMoves.isEmpty && certifiedHorizonPlyOffset.isEmpty,
    "a normalized threat branch must have exactly one objective-shaped proof contract"
  )
  require(
    lines.groupBy(_.rootMove).values.forall(group => group.map(line => line.evaluation -> line.replay).distinct.size == 1),
    "one threat-branch root move cannot carry conflicting evaluations"
  )
  def rankedUniqueLines: List[NormalizedCandidateLine] =
    lines.sortBy(_.rank).distinctBy(_.rootMove)

final case class ThreatLineOccurrenceOwner(
    sourceProbeId: String,
    objective: ProbeObjective,
    probedMoveUci: String,
    branchPosition: PositionNodeRef
):
  require(sourceProbeId.trim.nonEmpty, "a threat-line occurrence requires its source probe")
  require(EvidenceRef.normalizeMove(probedMoveUci).nonEmpty, "a threat-line occurrence requires its probed move")

final case class LineRootOccurrence(
    start: PositionNodeRef,
    destination: PositionNodeRef,
    transitionEvidenceId: String
):
  require(transitionEvidenceId.trim.nonEmpty, "a line-root occurrence requires its exact transition")

final case class NormalizedMoveReviewInput(
    beforeFen: String,
    playedMoveUci: String,
    beforePly: Int,
    sideToMove: Option[Color],
    afterPlayedFen: String,
    afterReferenceFen: Option[String],
    lines: List[NormalizedCandidateLine],
    completeCandidateSet: Option[CompleteCandidateSet],
    positionHistory: CanonicalPositionHistory,
    openingContext: OpeningContextEvidence,
    threatBranches: List[NormalizedThreatBranch] = Nil,
    probeDiagnostics: List[ProbeAdmissionDiagnostic] = Nil
):
  private[chessjudgment] lazy val historyReplay: Option[CanonicalLineReplay] =
    CanonicalLineReplay.fromHistory(positionHistory.segmentReplaySteps)

  require(
    lines.groupBy(_.rootMove).values.forall(group => group.map(line => line.evaluation -> line.replay).distinct.size == 1),
    "one root move cannot carry conflicting normalized evaluations"
  )
  def playedLine: Option[NormalizedCandidateLine] =
    lines.filter(_.role == LineNodeRole.Played) match
      case line :: Nil => Some(line)
      case _           => None

  def referenceLine: Option[NormalizedCandidateLine] =
    lines.filter(_.role == LineNodeRole.BestReference) match
      case line :: Nil => Some(line)
      case _           => None

  def rankedUniqueLines: List[NormalizedCandidateLine] =
    lines.sortBy(_.rank).distinctBy(_.rootMove)

final case class JudgmentAssemblyContext(
    input: NormalizedMoveReviewInput,
    positions: List[PositionNode],
    positionAnalyses: Map[PositionNodeRef, PositionAnalysis] = Map.empty,
    lines: List[CandidateLineNode],
    transitions: List[MoveTransitionEdge],
    lineReplays: Map[LineNodeRef, CanonicalLineReplay] = Map.empty,
    transitionReplays: Map[String, CanonicalLineReplay] = Map.empty,
    threatLineOwners: Map[LineNodeRef, ThreatLineOccurrenceOwner] = Map.empty,
    lineRootOccurrences: Map[LineNodeRef, LineRootOccurrence] = Map.empty,
    evidenceGraph: TypedEvidenceGraph,
    claims: List[JudgmentClaim],
    probeDiagnostics: List[ProbeAdmissionDiagnostic] = Nil
):
  def position(role: PositionNodeRole): Option[PositionNode] =
    exactlyOne(positions.filter(_.role == role))

  def line(role: LineNodeRole): Option[CandidateLineNode] =
    exactlyOne(lines.filter(_.role == role))

  private[chessjudgment] def lineReplay(line: LineNodeRef): Option[CanonicalLineReplay] =
    lineReplays.get(line)

  private[chessjudgment] def positionAnalysis(position: PositionNodeRef): Option[PositionAnalysis] =
    positionAnalyses.get(position)

  private[chessjudgment] def transitionReplay(edge: MoveTransitionEdge): Option[CanonicalLineReplay] =
    transitionReplays.get(edge.evidence.id)

  private[chessjudgment] def threatLineOwner(
      line: LineNodeRef
  ): Option[ThreatLineOccurrenceOwner] =
    threatLineOwners.get(line)

  private[chessjudgment] def lineRootOccurrence(
      line: LineNodeRef
  ): Option[LineRootOccurrence] =
    lineRootOccurrences.get(line)

  private[chessjudgment] def lineForRootTransition(
      edge: MoveTransitionEdge
  ): Option[CandidateLineNode] =
    lineRootOccurrences.collect {
      case (line, occurrence) if occurrence.transitionEvidenceId == edge.evidence.id => line
    }.toList match
      case lineRef :: Nil => lines.find(_.ref == lineRef)
      case Nil            => None
      case _ =>
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' is owned by more than one line-root occurrence"
        )

  /** Joins the already admitted root transition to a continuation line that
    * starts at the transition result. No move is replayed here.
    */
  private[chessjudgment] def continuationReplay(
      transition: StructuralTransitionBinding,
      continuationLine: LineNodeRef
  ): Option[CanonicalLineReplay] =
    for
      edge <- exactlyOne(transitions.filter(edge =>
        edge.role == transition.role &&
          edge.from == transition.from &&
          edge.to == transition.to &&
          EvidenceRef.sameMove(edge.moveUci, transition.moveUci)
      ))
      root <- transitionReplay(edge)
      continuation <- lineReplay(continuationLine)
      first <- continuation.replaySteps.headOption
      if PrincipalVariationEvidence.sameBoardState(first.fenBefore, transition.to.fen)
      rebased <- continuation.rebased(transition.to.ply + 1)
      combined <- CanonicalLineReplay.concatenate(root, rebased)
    yield combined

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

  def withPosition(node: PositionNode, analysis: PositionAnalysis): JudgmentAssemblyContext =
    require(
      analysis.features.plyCount == node.ref.ply &&
        analysis.position.color == node.ref.sideToMove.getOrElse(analysis.position.color) &&
        PrincipalVariationEvidence.sameBoardState(analysis.features.fen, node.ref.fen),
      "a position analysis must describe its registered position occurrence"
    )
    val updated = appendWithoutCollision(
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
    )
    updated match
      case None =>
        positionAnalyses.get(node.ref) match
          case Some(existing) if existing.asInstanceOf[AnyRef] eq analysis.asInstanceOf[AnyRef] => this
          case Some(_) =>
            throw IllegalArgumentException(
              s"position '${node.ref.id.getOrElse(node.ref.fen)}' has more than one analysis owner"
            )
          case None =>
            throw IllegalArgumentException(
              s"position '${node.ref.id.getOrElse(node.ref.fen)}' is registered without its analysis"
            )
      case Some(accepted) =>
        positionAnalyses.get(node.ref) match
          case Some(_) =>
            throw IllegalArgumentException(s"position '${node.ref.id.getOrElse(node.ref.fen)}' analysis collision")
          case None =>
            copy(positions = accepted, positionAnalyses = positionAnalyses.updated(node.ref, analysis))

  def withLine(line: CandidateLineNode, replay: CanonicalLineReplay): JudgmentAssemblyContext =
    require(
      replay.replaySteps.headOption.exists(step => EvidenceRef.sameMove(step.moveUci, line.ref.rootMove)),
      "a candidate line replay must begin with its registered root move"
    )
    val updated = appendWithoutCollision(
      items = lines,
      item = line,
      sameIdentity = (left, right) => left.ref.id == right.ref.id,
      sameExclusiveRole = (left, right) =>
        left.role == right.role && Set(LineNodeRole.Played, LineNodeRole.BestReference)(left.role),
      identity = s"line '${line.ref.id}'",
      role = s"line role '${line.role}'"
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
          case None => copy(lines = accepted, lineReplays = lineReplays.updated(line.ref, replay))

  def withTransition(
      edge: MoveTransitionEdge,
      admittedReplay: CanonicalLineReplay
  ): JudgmentAssemblyContext =
    val exactTransition = admittedReplay.replaySteps match
      case step :: Nil =>
        step.ply == edge.to.ply &&
          edge.to.ply == edge.from.ply + 1 &&
          EvidenceRef.sameMove(step.moveUci, edge.moveUci) &&
          PrincipalVariationEvidence.sameBoardState(step.fenBefore, edge.from.fen) &&
          PrincipalVariationEvidence.sameBoardState(step.fenAfter, edge.to.fen)
      case _ => false
    require(
      exactTransition,
      s"transition '${edge.evidence.id}' must reuse its one admitted line step"
    )
    val updated = appendWithoutCollision(
      items = transitions,
      item = edge,
      sameIdentity = (left, right) => left.evidence.id == right.evidence.id,
      sameExclusiveRole = (left, right) =>
        left.role == right.role && Set(TransitionEdgeRole.Played, TransitionEdgeRole.Reference)(left.role),
      identity = s"transition evidence '${edge.evidence.id}'",
      role = s"transition role '${edge.role}'"
    )
    updated match
      case None =>
        transitionReplays.get(edge.evidence.id) match
          case Some(existing) if existing.asInstanceOf[AnyRef] eq admittedReplay.asInstanceOf[AnyRef] => this
          case Some(_) =>
            throw IllegalArgumentException(
              s"transition '${edge.evidence.id}' has more than one replay owner"
            )
          case None =>
            throw IllegalArgumentException(
              s"transition '${edge.evidence.id}' is registered without its replay"
            )
      case Some(accepted) =>
        transitionReplays.get(edge.evidence.id) match
          case Some(_) =>
            throw IllegalArgumentException(s"transition '${edge.evidence.id}' replay collision")
          case None =>
            copy(
              transitions = accepted,
              transitionReplays = transitionReplays.updated(edge.evidence.id, admittedReplay)
            )

  private[chessjudgment] def withThreatLineOwner(
      line: LineNodeRef,
      owner: ThreatLineOccurrenceOwner
  ): JudgmentAssemblyContext =
    val exactLine = lines.exists(_.ref == line)
    val exactStart = lineReplays.get(line).exists(_.replaySteps.headOption.exists(step =>
      step.ply == owner.branchPosition.ply + 1 &&
        PrincipalVariationEvidence.sameBoardState(step.fenBefore, owner.branchPosition.fen)
    ))
    require(
      line.role == LineNodeRole.Threat && exactLine && exactStart,
      s"threat line '${line.id}' must be bound to its exact branch occurrence"
    )
    threatLineOwners.get(line) match
      case None => copy(threatLineOwners = threatLineOwners.updated(line, owner))
      case Some(existing) if existing == owner => this
      case Some(_) =>
        throw IllegalArgumentException(s"threat line '${line.id}' has more than one occurrence owner")

  private[chessjudgment] def withLineRootOccurrence(
      line: LineNodeRef,
      edge: MoveTransitionEdge
  ): JudgmentAssemblyContext =
    val exactLine = lines.find(_.ref == line)
    val exactEdge = transitions.find(_.evidence.id == edge.evidence.id)
    val exactStep = lineReplays.get(line).flatMap(_.replaySteps.headOption)
    require(
      exactLine.exists(_.role == edge.role.lineRole) &&
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
      positionAnalyses = Map.empty,
      lines = Nil,
      transitions = Nil,
      lineReplays = Map.empty,
      transitionReplays = Map.empty,
      threatLineOwners = Map.empty,
      lineRootOccurrences = Map.empty,
      evidenceGraph = evidenceGraph,
      claims = Nil,
      probeDiagnostics = input.probeDiagnostics
    )
