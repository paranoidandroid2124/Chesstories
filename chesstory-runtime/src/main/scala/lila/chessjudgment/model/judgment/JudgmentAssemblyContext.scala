package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionAnalysis
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }

final case class AdmittedLegalLine private[chessjudgment] (
    replay: CanonicalLineReplay,
    discoveryOnly: Boolean = false,
    automaticTerminal: Option[AutomaticTerminal] = None
):
  val moves: List[String] = replay.replaySteps.map(step => EvidenceRef.normalizeMove(step.moveUci))
  val rootMove: String = moves.head

object AdmittedLegalLine:
  /** A local discovery probe proves its board facts, but does not become a selected plan.
    * A later independent observation can qualify that same exact path without recalculation.
    */
  def merge(lines: List[AdmittedLegalLine]): List[AdmittedLegalLine] =
    val qualified = lines.iterator.filterNot(_.discoveryOnly).map(_.moves).toSet
    val terminals = lines.iterator.flatMap(line => line.automaticTerminal.map(line.moves -> _)).toMap
    lines.distinctBy(_.moves).map { line =>
      val discoveryOnly = line.discoveryOnly && !qualified(line.moves)
      val terminal = terminals.get(line.moves)
      if discoveryOnly == line.discoveryOnly && terminal == line.automaticTerminal then line
      else line.copy(discoveryOnly = discoveryOnly, automaticTerminal = terminal)
    }

enum EvaluationSource:
  case DirectInput, RootSearch, FocusComparison, InvestigationComparison, ResponseComparison

/** Describes the submitted document, never certifies that a game happened. */
enum CommentaryLineSource(val wireId: String):
  case PgnRecord extends CommentaryLineSource("pgn_record")
  case Analysis extends CommentaryLineSource("analysis")
  case Study extends CommentaryLineSource("study")

final case class SuppliedCommentaryLine(source: CommentaryLineSource, moves: List[String])

enum CommentaryLineStop(val wireId: String):
  case InvalidContinuation extends CommentaryLineStop("invalid_continuation")
  case AutomaticTerminal extends CommentaryLineStop("automatic_terminal")
  case InputLimit extends CommentaryLineStop("input_limit")

final case class CommentaryLineAdmission private[chessjudgment] (
    input: SuppliedCommentaryLine,
    acceptedPlyCount: Int,
    stop: Option[CommentaryLineStop]
):
  def acceptedMoves: List[String] = input.moves.take(acceptedPlyCount)

final case class AdmittedEvaluationObservation private[chessjudgment] (
    evaluation: CandidateLineEvaluation,
    source: EvaluationSource
)

final case class AdmittedAssessmentCandidate private[chessjudgment] (
    role: LineNodeRole,
    rank: Int,
    evaluation: CandidateLineEvaluation
):
  require(rank >= 1, "an assessment candidate rank must be positive")
  require(
    evaluation.moves.nonEmpty && evaluation.moves == evaluation.moves.map(EvidenceRef.normalizeMove),
    "an assessment candidate needs a canonical replay path"
  )

  def rootMove: Option[String] =
    evaluation.moves.headOption.map(_.trim.toLowerCase)

final case class AdmittedMoveReviewInput private[chessjudgment] (
    playedMoveUci: String,
    legalLines: List[AdmittedLegalLine],
    assessmentCandidates: List[AdmittedAssessmentCandidate],
    admittedRootRankingPair: Option[AdmittedRootRankingPair],
    positionHistory: CanonicalPositionHistory,
    evaluationObservations: List[AdmittedEvaluationObservation] = Nil,
    suppliedLines: List[CommentaryLineAdmission] = Nil
):
  private val legalRootMoves: List[String] = positionHistory.currentLegalMoveUcis
  private val admittedRootMoves: List[String] = legalLines.map(line => EvidenceRef.normalizeMove(line.rootMove))
  private val playedLineOwners = legalLines.filter(line => EvidenceRef.sameMove(line.rootMove, playedMoveUci))

  require(
    playedMoveUci == EvidenceRef.normalizeMove(playedMoveUci),
    "the observed move must be canonical"
  )
  require(
    legalLines.map(_.moves).distinct.size == legalLines.size,
    "an admitted replay path must have one LegalLine owner"
  )
  require(
    admittedRootMoves.forall(legalRootMoves.contains),
    "every admitted LegalLine root must belong to the legal root move universe"
  )
  require(
    legalLines.forall { line =>
      val first = line.replay.replaySteps.head
      first.ply == positionHistory.currentPly + 1 &&
      PrincipalVariationEvidence.normalizeFen(first.fenBefore) == positionHistory.currentFen
    },
    "every admitted LegalLine must start at the exact reviewed occurrence"
  )
  require(
    playedLineOwners.nonEmpty,
    "the observed move needs an admitted legal transition"
  )
  require(
    assessmentCandidates.map(_.rootMove).distinct.size ==
      assessmentCandidates.size,
    "one admitted LegalLine may have at most one assessment projection"
  )
  require(
    assessmentCandidates.forall { candidate =>
      legalLines.exists(_.moves == candidate.evaluation.moves)
    },
    "every assessment candidate must bind the full replay of one admitted LegalLine"
  )
  require(
    evaluationObservations.forall(observation => legalLines.exists(_.moves == observation.evaluation.moves)),
    "each evaluation observation must retain its exact admitted path"
  )
  require(
    assessmentCandidates.count(_.role == LineNodeRole.Played) <= 1,
    "an admitted review may have at most one played assessment projection"
  )
  require(
    assessmentCandidates.count(_.role == LineNodeRole.BestReference) <= 1,
    "an admitted review may have at most one assessment reference projection"
  )

  def referenceCandidate: Option[AdmittedAssessmentCandidate] =
    assessmentCandidates.find(_.role == LineNodeRole.BestReference)

final case class JudgmentAssemblyContext(
    input: AdmittedMoveReviewInput,
    positions: List[PositionNode],
    positionAnalyses: Map[PositionNodeRef, PositionAnalysis] = Map.empty,
    legalLines: List[LegalLineNode],
    lines: List[CandidateLineNode],
    transitions: List[MoveTransitionEdge],
    lineReplays: Map[LineNodeRef, CanonicalLineReplay] = Map.empty,
    transitionReplays: Map[String, CanonicalLineReplay] = Map.empty,
    lineTransitionIds: Map[LineNodeRef, List[String]] = Map.empty,
    private[chessjudgment] val predecessorReplay: Option[CanonicalLineReplay] = None,
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
  ): Option[MoveTransitionEdge] =
    lineTransitionIds.get(line).flatMap(_.headOption).flatMap(id => transitions.find(_.evidence.id == id))

  def transition(role: TransitionEdgeRole): Option[MoveTransitionEdge] =
    exactlyOne(transitions.filter(_.role == role))

  def playedTransition: Option[MoveTransitionEdge] =
    transition(TransitionEdgeRole.Played)

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
        PositionNodeRole.AfterPlayed
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
    withNewLegalLines(List(line -> replay))

  /** Checks a line batch once and preserves the exact replay owner on repeated admission. */
  private[chessjudgment] def withNewLegalLines(
      additions: List[(LegalLineNode, CanonicalLineReplay)]
  ): JudgmentAssemblyContext =
    if additions.isEmpty then this
    else
      var registered = legalLines.iterator.map(line => line.ref.id -> line).toMap
      var replays = lineReplays
      val accepted = List.newBuilder[LegalLineNode]
      additions.foreach { (line, replay) =>
        require(
          replay.replaySteps.headOption.exists(step => EvidenceRef.sameMove(step.moveUci, line.ref.rootMove)),
          "a LegalLine replay must begin with its registered root move"
        )
        registered.get(line.ref.id) match
          case Some(existing) if existing == line =>
            replays.get(line.ref) match
              case Some(owner) if owner.asInstanceOf[AnyRef] eq replay.asInstanceOf[AnyRef] => ()
              case Some(_) =>
                throw IllegalArgumentException(s"line '${line.ref.id}' has more than one replay owner")
              case None =>
                throw IllegalArgumentException(s"line '${line.ref.id}' is registered without its replay")
          case Some(_) =>
            throw IllegalArgumentException(s"line '${line.ref.id}' collision: existing value differs from the attempted addition")
          case None =>
            if replays.contains(line.ref) then
              throw IllegalArgumentException(s"line '${line.ref.id}' replay collision")
            registered = registered.updated(line.ref.id, line)
            replays = replays.updated(line.ref, replay)
            accepted += line
      }
      val newLines = accepted.result()
      if newLines.isEmpty then this
      else copy(legalLines = legalLines ++ newLines, lineReplays = replays)

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
      val exclusiveRoles = Set(TransitionEdgeRole.Played)
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

  private[chessjudgment] def withLinePaths(
      paths: List[(LineNodeRef, List[MoveTransitionEdge])]
  ): JudgmentAssemblyContext =
    val registered = transitions.iterator.map(edge => edge.evidence.id -> edge).toMap
    val owners = legalLines.iterator.map(line => line.ref -> line.evidence.position).toMap
    val additions = paths.map { (line, path) =>
      val steps = lineReplays.get(line).map(_.replaySteps).getOrElse(Nil)
      require(
        path.nonEmpty && steps.size == path.size && owners.get(line).contains(path.head.from) &&
          path.zip(steps).forall { (edge, step) =>
            registered.get(edge.evidence.id).contains(edge) && edge.to.ply == step.ply &&
              edge.from.fen == step.fenBefore && edge.to.fen == step.fenAfter &&
              EvidenceRef.sameMove(edge.moveUci, step.moveUci)
          } && path.zip(path.drop(1)).forall((left, right) => left.to == right.from),
        s"line '${line.id}' must retain its complete historical transition path"
      )
      val ids = path.map(_.evidence.id)
      require(lineTransitionIds.get(line).forall(_ == ids), "a line cannot change its historical path")
      line -> ids
    }
    require(additions.map(_._1).distinct.size == additions.size, "a path batch cannot repeat a line")
    copy(lineTransitionIds = lineTransitionIds ++ additions)

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
      lineTransitionIds = Map.empty,
      evidenceGraph = evidenceGraph
    )
