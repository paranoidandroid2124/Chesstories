package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.position.PositionAnalysis
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }

final case class AdmittedLegalLine(
    rootMove: String,
    replay: CanonicalLineReplay,
    predecessorReplay: Option[CanonicalLineReplay] = None
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

final case class ExplanationSubjectOccurrence private[chessjudgment] (
    occurrenceId: String,
    line: LineNodeRef,
    lineOwnerEvidenceId: String,
    transitionEvidenceId: String,
    moveUci: String,
    start: PositionNodeRef,
    destination: PositionNodeRef,
    rootProvenance: CausalRootProvenance
):
  require(occurrenceId == transitionEvidenceId, "the subject occurrence id must be its exact transition owner")
  require(lineOwnerEvidenceId.nonEmpty, "an explanation subject needs its LegalLine owner")
  require(
    rootProvenance == CausalRootProvenance.ObservedGameRoot,
    "an explanation subject must be the history-observed root occurrence"
  )

  private[chessjudgment] val stableKey: String =
    BoundedCausalIdentity.digest(List(
      "explanation-subject-occurrence:v1",
      occurrenceId,
      BoundedCausalIdentity.lineKey(line),
      lineOwnerEvidenceId,
      transitionEvidenceId,
      EvidenceRef.normalizeMove(moveUci),
      PrincipalVariationEvidence.normalizeFen(start.fen),
      start.ply.toString,
      PrincipalVariationEvidence.normalizeFen(destination.fen),
      destination.ply.toString,
      rootProvenance.toString
    ))

/** One graph-owned line root with its exact LegalLine replay and transition.
  * Selection roles and engine ranks do not decide whether the root was
  * observed. That provenance is closed against the admitted history move.
  */
private[chessjudgment] final case class CertifiedRootOccurrence private[chessjudgment] (
    line: LineNodeRef,
    lineOwner: EvidenceRecord,
    replay: CanonicalLineReplay,
    occurrence: LineRootOccurrence,
    transition: MoveTransitionEdge,
    transitionOwner: EvidenceRecord,
    rootProvenance: CausalRootProvenance
):
  val rootStep: LineReplayStep = replay.replaySteps.head

  def isObserved: Boolean =
    rootProvenance == CausalRootProvenance.ObservedGameRoot

  def parentSources: List[EvidenceRef] =
    List(lineOwner.ref, transitionOwner.ref).sortBy(_.id)

  def publicOccurrence: ExplanationSubjectOccurrence =
    ExplanationSubjectOccurrence(
      occurrence.transitionEvidenceId,
      line,
      lineOwner.ref.id,
      transitionOwner.ref.id,
      EvidenceRef.normalizeMove(transition.moveUci),
      transition.from,
      transition.to,
      rootProvenance
    )

  def remainsCertified: Boolean =
    CertifiedLineReplayRecord.from(lineOwner).exists(authority =>
      authority.line == line && authority.replay == replay
    ) &&
      transitionOwner.ref == transition.evidence &&
      (transitionOwner.payload match
        case payload @ MoveTransitionEvidence(moveUci, from, to, _) =>
          moveUci == transition.moveUci && from == transition.from && to == transition.to &&
            payload.canonicalTransitionProof.exists(
              _.provesMove(moveUci, from, to, transitionOwner.ref.scope)
            )
        case _ => false) &&
      occurrence.start == transition.from && occurrence.destination == transition.to &&
      occurrence.transitionEvidenceId == transition.evidence.id &&
      replay.replaySteps.headOption.contains(rootStep)

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
  private val admittedRootMoves = legalLines.map(line => EvidenceRef.normalizeMove(line.rootMove))
  require(
    admittedRootMoves.size == legalLines.size && admittedRootMoves.distinct.size == admittedRootMoves.size,
    "one root move must have exactly one admitted LegalLine owner"
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

  /** Sole role-neutral authority for the root occurrence of one admitted
    * line. It joins already registered owners only; no board fact is replayed
    * or recomputed here.
    */
  private[chessjudgment] def certifiedRootOccurrence(
      line: LineNodeRef
  ): Option[CertifiedRootOccurrence] =
    for
      registeredLine <- legalLines.find(_.ref == line)
      lineOwner <- evidenceGraph.uniqueProofEligibleLineFactRecordFor(line).map(_._1)
      replay <- lineReplay(line)
      occurrence <- lineRootOccurrence(line)
      transition <- transitions.filter(_.evidence.id == occurrence.transitionEvidenceId) match
        case exact :: Nil => Some(exact)
        case _            => None
      transitionOwner <- evidenceGraph.record(transition.evidence)
      if evidenceGraph.proofEligible(transitionOwner)
      transitionReplay <- transitionReplay(transition)
      rootStep <- replay.replaySteps.headOption
      exactTransitionStep <- transitionReplay.replaySteps match
        case exact :: Nil => Some(exact)
        case _            => None
      if registeredLine.evidence == lineOwner.ref
      if lineOwner.ref.line.contains(line)
      if occurrence.start == transition.from && occurrence.destination == transition.to
      if rootStep == exactTransitionStep
      if EvidenceRef.sameMove(rootStep.moveUci, transition.moveUci)
      if PrincipalVariationEvidence.sameBoardState(rootStep.fenBefore, transition.from.fen)
      if PrincipalVariationEvidence.sameBoardState(rootStep.fenAfter, transition.to.fen)
    yield
      val provenance =
        if isAdmittedObservedRoot(transition, rootStep) then CausalRootProvenance.ObservedGameRoot
        else CausalRootProvenance.CounterfactualAnalyzedRoot
      CertifiedRootOccurrence(
        line,
        lineOwner,
        replay,
        occurrence,
        transition,
        transitionOwner,
        provenance
      )

  private[chessjudgment] def certifiedRootOccurrences: Option[List[CertifiedRootOccurrence]] =
    val certified = legalLines.map(line => certifiedRootOccurrence(line.ref))
    Option.when(certified.forall(_.nonEmpty)) {
      val exact = certified.flatten
      require(
        exact.count(_.isObserved) == 1,
        "an admitted review must have exactly one history-observed root occurrence"
      )
      exact
    }

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
      analysis.occurrence.plyCount == node.ref.ply &&
        analysis.position.color == node.ref.sideToMove.getOrElse(analysis.position.color) &&
        PrincipalVariationEvidence.sameBoardState(analysis.occurrence.fen, node.ref.fen),
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

  private def isAdmittedObservedRoot(
      edge: MoveTransitionEdge,
      rootStep: LineReplayStep
  ): Boolean =
    input.positionHistory.currentPly == input.beforePly &&
      PrincipalVariationEvidence.sameBoardState(input.positionHistory.currentFen, input.beforeFen) &&
      edge.from.ply == input.beforePly &&
      PrincipalVariationEvidence.sameBoardState(edge.from.fen, input.beforeFen) &&
      EvidenceRef.sameMove(edge.moveUci, input.playedMoveUci) &&
      EvidenceRef.sameMove(rootStep.moveUci, input.playedMoveUci) &&
      PrincipalVariationEvidence.sameBoardState(edge.to.fen, input.afterPlayedFen) &&
      PrincipalVariationEvidence.sameBoardState(rootStep.fenAfter, input.afterPlayedFen)

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
