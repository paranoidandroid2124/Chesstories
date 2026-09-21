package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.evaluation.EvalFactNormalizer
import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.{
  CanonicalPositionHistory,
  CandidateLineEvaluation,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.judgment.*

private[assembly] final case class PositionNodeAssembly(
    node: PositionNode,
    evidence: List[EvidenceRecord],
    analysis: lila.chessjudgment.analysis.position.PositionAnalysis
)

private[assembly] final case class LegalLineAssembly(
    node: LegalLineNode,
    evidence: List[EvidenceRecord],
    replay: CanonicalLineReplay
)

private[assembly] final case class CandidateLineAssembly(
    node: CandidateLineNode,
    evidence: List[EvidenceRecord]
)

private[assembly] final case class TransitionEdgeAssembly(
    edge: MoveTransitionEdge,
    evidence: List[EvidenceRecord],
    replay: CanonicalLineReplay
)

private[assembly] object PositionNodeAssembler:
  def fromOccurrence(
      node: PositionNode,
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      allocator: JudgmentProvenanceAllocator,
      incomingTransition: Option[EvidenceRef]
  ): PositionNodeAssembly =
    val ref = node.ref
    require(
      analysis.occurrence.fen == ref.fen && analysis.occurrence.plyCount == ref.ply &&
        ref.sideToMove.contains(analysis.position.color),
      "a position node must use the analysis of its exact occurrence"
    )
    val nodeKey = ref.id.getOrElse(throw IllegalArgumentException("an occurrence needs its historical id"))
    val occurrenceRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = allocator.evidenceId(s"position-occurrence:$nodeKey"),
        producer = EvidenceProducer.PositionOccurrenceProducer,
        layer = EvidenceLayer.PositionOccurrence,
        position = ref,
        line = None,
        scope = node.role.scope,
        confidence = EvidenceConfidence.BoardDerived
      ),
      payload = PositionOccurrenceEvidence(analysis.occurrence),
      parents = incomingTransition.toList
    )
    val relations = PositionRelationExtractor.records(
      analysis.boardRelations,
      ref,
      node.role.scope,
      relation => allocator.evidenceId(
        s"relation:position:$nodeKey:${RelationFactKind.id(relation.kind)}:${relation.semanticId}"
      )
    )
    PositionNodeAssembly(node, occurrenceRecord :: relations, analysis)

private[assembly] object LegalLineAssembler:

  def fromLine(
      line: AdmittedLegalLine,
      root: PositionNodeRef,
      rootAnalysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      positionHistory: CanonicalPositionHistory,
      predecessorReplay: Option[CanonicalLineReplay],
      allocator: JudgmentProvenanceAllocator
  ): LegalLineAssembly =
    val scope = EvidenceScope.LegalLine
    val occurrenceKey = allocator.lineOccurrenceKey(line, root)
    val ref = allocator.lineRef(line, occurrenceKey)
    val replay = line.replay.rootedAt(rootAnalysis)
    val lineEvidence =
      allocator.evidenceRef(
        suffix = s"line:$occurrenceKey",
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = root,
        line = Some(ref),
        scope = scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      )
    val node = LegalLineNode(ref = ref, evidence = lineEvidence)
    val lineRecord =
      LineFactNormalizer.fromValidatedLine(
        id = lineEvidence.id,
        lineRef = ref,
        replay = replay,
        position = root,
        scope = scope,
        materialSummary = Some(LineMaterialSummary(replay, positionHistory, predecessorReplay))
      )
    LegalLineAssembly(node, List(lineRecord), replay)

  private[assembly] def lineMaterialSummary(
      positionHistory: CanonicalPositionHistory,
      lineHistory: CanonicalPositionHistory
  ): Option[LineMaterialSummary] =
    val prefixSize = positionHistory.segmentReplaySteps.size
    val replaySteps = lineHistory.segmentReplaySteps.drop(prefixSize)
    val belongsToPositionHistory =
      lineHistory.segmentReplaySteps.take(prefixSize) == positionHistory.segmentReplaySteps
    val predecessorReplay =
      CanonicalLineReplay.fromHistory(positionHistory.segmentReplaySteps.takeRight(1))
    Option.when(belongsToPositionHistory)(replaySteps)
      .flatMap(CanonicalLineReplay.fromHistory)
      .map(replay => LineMaterialSummary(replay, positionHistory, predecessorReplay))

private[assembly] object CandidateLineAssembler:
  def fromProjection(
      candidate: AdmittedAssessmentCandidate,
      legalLine: LegalLineAssembly,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): Option[CandidateLineAssembly] =
    val replayMoves = legalLine.replay.replaySteps.map(step => EvidenceRef.normalizeMove(step.moveUci))
    val evaluationMoves = candidate.evaluation.moves.map(EvidenceRef.normalizeMove)
    Option.when(
      candidate.evaluation.comparisonReady &&
        replayMoves == evaluationMoves
    ) {
      val node = CandidateLineNode(
        ref = legalLine.node.ref,
        role = candidate.role,
        rank = candidate.rank,
        evaluation = candidate.evaluation,
        lineEvidence = legalLine.node.evidence
      )
      val evaluationKey = EvidenceIdentity.digest(List(
        legalLine.node.ref.id,
        candidate.role.toString,
        candidate.rank.toString,
        evaluationIdentity(candidate.evaluation)
      ))
      val evalRecord = EvalFactNormalizer.fromCandidateLine(
        id = allocator.evidenceId(s"eval:$evaluationKey"),
        line = node,
        position = root,
        scope = candidate.role.scope,
        parents = List(legalLine.node.evidence)
      )
      CandidateLineAssembly(node, List(evalRecord))
    }

  private def evaluationIdentity(evaluation: CandidateLineEvaluation): String =
    evaluation match
      case CandidateLineEvaluation.EngineSearch(line) =>
        List(
          "engine",
          line.moves.map(EvidenceRef.normalizeMove).mkString("[", ",", "]"),
          line.scoreCp.toString,
          line.mate.map(_.toString).getOrElse("none"),
          line.depth.toString
        ).mkString("|")
      case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
        List(
          "automatic-terminal",
          moves.map(EvidenceRef.normalizeMove).mkString("[", ",", "]"),
          terminal.toString
        ).mkString("|")

private[assembly] object TransitionEdgeAssembler:
  def fromMove(
      role: TransitionEdgeRole,
      from: PositionNodeAssembly,
      to: PositionNode,
      replay: CanonicalLineReplay,
      allocator: JudgmentProvenanceAllocator
  ): TransitionEdgeAssembly =
    val moveUci = replay.replaySteps.head.moveUci
    val occurrenceKey = allocator.transitionOccurrenceKey(from.node.ref, moveUci, to.ref)
    val evidence = allocator.evidenceRef(
      suffix = s"transition:$occurrenceKey",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = from.node.ref,
      line = None,
      scope = role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val edge = MoveTransitionEdge(role, from.node.ref, moveUci, to.ref, evidence)
    TransitionEdgeAssembly(
      edge,
      List(TransitionFactNormalizer.fromMoveTransition(edge, replay, from.evidence.head.ref)),
      replay
    )

private[assembly] object NodeLineTransitionAssembler:
  def assemble(input: AdmittedMoveReviewInput, retained: Option[JudgmentAssemblyContext] = None): JudgmentAssemblyContext =
    require(retained.forall(ctx => ctx.input.positionHistory == input.positionHistory && ctx.input.playedMoveUci == input.playedMoveUci),
      "retained calculations must belong to the same reviewed history and move")
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val history = input.positionHistory
    val beforeAnalysis = retained.flatMap(ctx => ctx.root.flatMap(ctx.positionAnalysis)).getOrElse(lila.chessjudgment.analysis.position.PositionAnalyzer.analyze(
      history.currentPosition, history.currentFen, history.currentPly
    ))
    val beforeNode = PositionNode(
      PositionNodeRole.Before,
      allocator.positionRef(PositionNodeRole.Before, history.currentFen, history.currentPly, Some(beforeAnalysis.position.color))
    )
    val before = PositionNodeAssembler.fromOccurrence(beforeNode, beforeAnalysis, allocator, None)
    val predecessorReplay = retained.map(_.predecessorReplay).getOrElse(CanonicalLineReplay.fromHistory(history.segmentReplaySteps.takeRight(1)))
    val replays = CanonicalLineReplay.sharePrefixes(input.legalLines.map(_.replay), beforeAnalysis,
      retained.toList.flatMap(_.lineReplays.values))
    val knownLines = retained.toList.flatMap(ctx => ctx.legalLines.map { node =>
      val replay = ctx.lineReplay(node.ref).get
      replay.replaySteps.map(_.moveUci) -> LegalLineAssembly(node, Nil, replay)
    }).toMap
    val rootLines = replays.map(replay => knownLines.getOrElse(replay.replaySteps.map(_.moveUci),
      LegalLineAssembler.fromLine(
        AdmittedLegalLine(replay), beforeNode.ref, beforeAnalysis, history, predecessorReplay, allocator
      )))
    val occurrences = scala.collection.mutable.LinkedHashMap.empty[
      CanonicalReplayTransition, (PositionNodeAssembly, TransitionEdgeAssembly)
    ]
    retained.foreach { ctx =>
      require(ctx.root.contains(beforeNode.ref), "retained evidence needs the same historical root")
      val positions = ctx.positions.map(node => node.ref -> node).toMap
      ctx.transitions.foreach { edge =>
        val replay = ctx.transitionReplay(edge).get
        val node = positions(edge.to)
        val occurrence = ctx.evidenceGraph.byId(allocator.evidenceId(s"position-occurrence:${node.ref.id.get}"))
        occurrences.update(replay.onlyTransition.get,
          PositionNodeAssembly(node, List(occurrence), ctx.positionAnalysis(node.ref).get) ->
            TransitionEdgeAssembly(edge, Nil, replay))
      }
    }
    val existing = occurrences.keySet.toSet
    val paths = rootLines.map { line =>
      var predecessor = before
      val edges = line.replay.singleStepReplays.zipWithIndex.map { case ((canonical, replay), offset) =>
        val (position, transition) = occurrences.getOrElseUpdate(canonical, {
          val played = offset == 0 && EvidenceRef.sameMove(canonical.declared.moveUci, input.playedMoveUci)
          val positionRole =
            if offset > 0 then PositionNodeRole.LineOccurrence
            else if played then PositionNodeRole.AfterPlayed
            else PositionNodeRole.AfterAlternative
          val transitionRole =
            if offset > 0 then TransitionEdgeRole.LineContinuation
            else if played then TransitionEdgeRole.Played
            else TransitionEdgeRole.Alternative
          val destination = PositionNode(
            positionRole,
            allocator.successorPositionRef(predecessor.node.ref, canonical.declared, canonical.afterAnalysis.position.color)
          )
          val transition = TransitionEdgeAssembler.fromMove(transitionRole, predecessor, destination, replay, allocator)
          val position = PositionNodeAssembler.fromOccurrence(
            destination, canonical.afterAnalysis, allocator, Some(transition.edge.evidence)
          )
          position -> transition
        })
        require(transition.edge.from == predecessor.node.ref, "a shared transition must have the same historical predecessor")
        predecessor = position
        transition.edge
      }
      line.node.ref -> edges
    }
    val produced = occurrences.iterator.filterNot((canonical, _) => existing(canonical)).map(_._2).toList
    val positions = Option.when(retained.isEmpty)(before).toList ++ produced.map(_._1)
    val base = retained.fold(JudgmentAssemblyContext.empty(input))(ctx =>
      ctx.copy(input = input, lines = Nil, evidenceGraph = ctx.evidenceGraph.withoutAssessment))
    val context = base
      .copy(predecessorReplay = predecessorReplay)
      .withNewPositions(positions.map(position => position.node -> position.analysis))
      .withNewTransitions(produced.map { case (_, transition) => transition.edge -> transition.replay })
      .withEvidence(positions.flatMap(_.evidence) ++ produced.flatMap(_._2.evidence))
    val withLines = context
      .withNewLegalLines(rootLines.map(line => line.node -> line.replay))
      .withEvidence(rootLines.flatMap(_.evidence))
    withCandidates(withLines.withLinePaths(paths))

  def withCandidates(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    val allocator = JudgmentProvenanceAllocator.forInput(context.input)
    val root = context.root.getOrElse(throw IllegalArgumentException("assessment needs its reviewed root"))
    val linesByMoves = context.legalLines.map { node =>
      val replay = context.lineReplay(node.ref).getOrElse(
        throw IllegalArgumentException("assessment needs its registered replay"))
      replay.replaySteps.map(_.moveUci) -> LegalLineAssembly(node, Nil, replay)
    }.toMap
    context.input.assessmentCandidates.foldLeft(context) { (ctx, candidate) =>
      linesByMoves.get(candidate.evaluation.moves)
        .flatMap(CandidateLineAssembler.fromProjection(candidate, _, root, allocator))
        .map(produced => ctx.withCandidateLine(produced.node).withEvidence(produced.evidence))
        .getOrElse(ctx)
    }
