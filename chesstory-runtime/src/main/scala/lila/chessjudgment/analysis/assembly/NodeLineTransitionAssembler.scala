package lila.chessjudgment.analysis.assembly

import chess.Pawn
import lila.chessjudgment.analysis.evaluation.EvalFactNormalizer
import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.{
  CanonicalPositionHistory,
  CandidateLineEvaluation,
  PreInitialHistoryKnowledge,
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
  private[assembly] def fromAnalysis(
      role: PositionNodeRole,
      fen: String,
      ply: Int,
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      allocator: JudgmentProvenanceAllocator,
      scope: EvidenceScope,
      lineRootOwner: Option[LineNodeRef] = None
  ): PositionNodeAssembly =
    val position = analysis.position
    require(
      analysis.occurrence.fen == fen && analysis.occurrence.plyCount == ply,
      "a position node must use the analysis of its exact occurrence"
    )
    val ref = lineRootOwner
      .map(allocator.lineRootPositionRef(role, fen, ply, Some(position.color), _))
      .getOrElse(allocator.positionRef(role, fen, ply, Some(position.color)))
    val nodeKey = lineRootOwner
      .map(allocator.lineRootPositionKey(role, fen, ply, _))
      .getOrElse(allocator.positionKey(role, fen, ply))
    assemble(
      role,
      ref,
      nodeKey,
      analysis,
      allocator,
      scope,
      line = None,
      parents = Nil
    )

  private[assembly] def fromLineOccurrence(
      node: PositionNode,
      lineOwner: LegalLineNode,
      plyOffset: Int,
      predecessor: PositionNodeRef,
      step: LineReplayStep,
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      allocator: JudgmentProvenanceAllocator,
      incomingTransition: EvidenceRef
  ): PositionNodeAssembly =
    require(
      incomingTransition.producer == EvidenceProducer.MoveTransitionProducer &&
        incomingTransition.layer == EvidenceLayer.MoveTransition &&
        incomingTransition.position == predecessor && incomingTransition.line.contains(lineOwner.ref) &&
        incomingTransition.scope == EvidenceScope.LegalLine &&
        incomingTransition.confidence == EvidenceConfidence.LegalReplayVerified,
      "a line position occurrence needs its exact incoming continuation authority"
    )
    require(
      node.role == PositionNodeRole.LineOccurrence && node.ref.fen == step.fenAfter && node.ref.ply == step.ply,
      "a line position assembly must use its preallocated occurrence node"
    )
    val nodeKey = allocator.lineOccurrencePositionKey(
      lineOwner.ref,
      plyOffset,
      predecessor,
      step.fenAfter,
      step.ply
    )
    assemble(
      PositionNodeRole.LineOccurrence,
      node.ref,
      nodeKey,
      analysis,
      allocator,
      EvidenceScope.LegalLine,
      line = Some(lineOwner.ref),
      parents = List(incomingTransition)
    )

  private[assembly] def lineOccurrenceNode(
      line: LineNodeRef,
      plyOffset: Int,
      predecessor: PositionNodeRef,
      step: LineReplayStep,
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      allocator: JudgmentProvenanceAllocator
  ): PositionNode =
    PositionNode(
      PositionNodeRole.LineOccurrence,
      allocator.lineOccurrencePositionRef(
        line,
        plyOffset,
        predecessor,
        step.fenAfter,
        step.ply,
        Some(analysis.position.color)
      )
    )

  private def assemble(
      role: PositionNodeRole,
      ref: PositionNodeRef,
      nodeKey: String,
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      allocator: JudgmentProvenanceAllocator,
      scope: EvidenceScope,
      line: Option[LineNodeRef],
      parents: List[EvidenceRef]
  ): PositionNodeAssembly =
    val occurrenceRecord =
      EvidenceRecord(
        ref = EvidenceRef(
          id = allocator.evidenceId(s"position-occurrence:$nodeKey"),
          producer = EvidenceProducer.PositionOccurrenceProducer,
          layer = EvidenceLayer.PositionOccurrence,
          position = ref,
          line = line,
          scope = scope,
          confidence = EvidenceConfidence.BoardDerived
        ),
        payload = PositionOccurrenceEvidence(analysis.occurrence),
        parents = parents
      )
    val relationRecords = PositionRelationExtractor.records(
      analysis.boardRelations,
      ref,
      scope,
      relation =>
        allocator.evidenceId(
          s"relation:position:$nodeKey:${RelationFactKind.id(relation.kind)}:${relation.semanticId}"
        )
    )
    val node = PositionNode(role = role, ref = ref)
    PositionNodeAssembly(node, occurrenceRecord :: relationRecords, analysis)

private[assembly] object LegalLineAssembler:

  private final case class ReplayedLineFacts(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      materialSummary: Option[LineMaterialSummary]
  )

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
    val replayed = replayFacts(
      line.replay.rootedAt(rootAnalysis),
      positionHistory,
      predecessorReplay
    )
    val facts = replayed.facts
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
        facts = facts,
        replay = replayed.replay,
        position = root,
        scope = scope,
        materialSummary = replayed.materialSummary
      )
    LegalLineAssembly(node, List(lineRecord), replayed.replay)

  private def replayFacts(
      replay: CanonicalLineReplay,
      positionHistory: CanonicalPositionHistory,
      predecessorReplay: Option[CanonicalLineReplay]
  ): ReplayedLineFacts =
    val replaySteps = replay.replaySteps
    val moves = replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moves.head
    ReplayedLineFacts(
      facts = PrincipalVariationEvidence.LineFacts(
        line = PrincipalVariationEvidence.LineVariationRef(moves),
        first = first,
        reply = moves.lift(1),
        continuation = moves.lift(2),
        continuationTail = moves.drop(3)
      ),
      replay = replay,
      materialSummary = lineMaterialSummary(positionHistory, replay, predecessorReplay)
    )

  private[assembly] def lineMaterialSummary(
      positionHistory: CanonicalPositionHistory,
      lineHistory: CanonicalPositionHistory
  ): Option[LineMaterialSummary] =
    val prefixSize = positionHistory.segmentReplaySteps.size
    val replaySteps = lineHistory.segmentReplaySteps.drop(prefixSize)
    val belongsToPositionHistory =
      lineHistory.segmentReplaySteps.take(prefixSize) == positionHistory.segmentReplaySteps
    val predecessorReplay =
      CanonicalLineReplay.fromHistory(positionHistory.segmentReplaySteps)
    Option.when(belongsToPositionHistory)(replaySteps)
      .flatMap(CanonicalLineReplay.fromHistory)
      .flatMap(lineMaterialSummary(positionHistory, _, predecessorReplay))

  private[assembly] def lineMaterialSummary(
      positionHistory: CanonicalPositionHistory,
      replay: CanonicalLineReplay,
      predecessorReplay: Option[CanonicalLineReplay] = None
  ): Option[LineMaterialSummary] =
    val singleStepReplays = replay.singleStepReplays
    val events = singleStepReplays
      .zip(None :: singleStepReplays.map(Option(_)))
      .zipWithIndex
      .flatMap { case (((transition, transitionReplay), priorOccurrence), plyOffset) =>
        val declared = transition.declared
        val root = transition.relationDelta.rootMove
        val primaryMovements = transition.boardFootprint.pieceTransitions.filter(movement =>
          movement.side == root.side &&
            EvidenceSquare(movement.from.key) == root.from &&
            EvidenceSquare(movement.to.key) == root.to &&
            EvidencePieceRole(movement.beforeRole.name) == root.beforeRole &&
            EvidencePieceRole(movement.afterRole.name) == root.afterRole
        )
        require(
          primaryMovements.size == 1,
          s"'${declared.moveUci}' needs one canonical primary movement in its material ledger"
        )
        val primary = primaryMovements.head
        val promotionGain =
          if primary.beforeRole == Pawn && primary.afterRole != Pawn then
            MaterialValue.materialValueCp(primary.afterRole) - MaterialValue.materialValueCp(Pawn)
          else 0
        val capture = root.capture.map { captured =>
          val capturedRole = transition.legal.capturedRole.getOrElse(
            throw IllegalArgumentException(s"'${declared.moveUci}' lost its canonical captured role")
          )
          require(
            capturedRole.name.equalsIgnoreCase(captured.capturedRole.name),
            s"'${declared.moveUci}' capture disagrees with its closed legal-move relation"
          )
          LineMaterialCapture(
            moveUci = EvidenceRef.normalizeMove(declared.moveUci),
            plyOffset = plyOffset,
            side = root.side,
            attackerRole = root.beforeRole,
            capturedRole = captured.capturedRole,
            square = captured.capturedSquare,
            valueCp = MaterialValue.materialValueCp(capturedRole),
            recaptureStatus = recaptureStatus(
              positionHistory,
              predecessorReplay,
              priorOccurrence,
              transitionReplay,
              declared,
              plyOffset
            )
          )
        }
        Option.when(capture.nonEmpty || promotionGain > 0)(
          ObservedLineMaterialEvent(
            moveUci = EvidenceRef.normalizeMove(declared.moveUci),
            plyOffset = plyOffset,
            movement = root.witness,
            legalMoveSemanticId = root.fact.semanticId,
            capture = capture,
            promotionGainCp = promotionGain
          )
        )
      }
    val hasClosedRecaptureKnowledge =
      events.forall(_.capture.forall(
        _.recaptureStatus != LineMaterialRecaptureStatus.Unknown
      ))
    Option.when(events.nonEmpty && hasClosedRecaptureKnowledge)(
      LineMaterialSummary(
        sideToMove = replay.legalSteps.head.move.piece.color,
        events = events,
        closedOutcome = singleStepReplays.lastOption.flatMap { case (_, terminalReplay) =>
          closedMaterialOutcome(terminalReplay, singleStepReplays.size - 1)
        }
      )
    )

  private def recaptureStatus(
      positionHistory: CanonicalPositionHistory,
      predecessorReplay: Option[CanonicalLineReplay],
      priorOccurrence: Option[(CanonicalReplayTransition, CanonicalLineReplay)],
      replay: CanonicalLineReplay,
      declared: LineReplayStep,
      plyOffset: Int
  ): LineMaterialRecaptureStatus =
    val predecessorOccurrence =
      if plyOffset == 0 then
        predecessorReplay
          .map(_.singleStepReplays)
          .flatMap(_.lastOption)
          .filter { case (previousTransition, _) =>
            val previousStep = previousTransition.declared
            previousStep.ply + 1 == declared.ply &&
              PrincipalVariationEvidence.sameBoardState(previousStep.fenAfter, declared.fenBefore)
          }
      else
        None
    priorOccurrence.orElse(predecessorOccurrence) match
      case None =>
        if plyOffset == 0 &&
            positionHistory.segmentReplaySteps.isEmpty &&
            positionHistory.preInitialHistoryKnowledge == PreInitialHistoryKnowledge.KnownEmpty
        then LineMaterialRecaptureStatus.Excluded
        else LineMaterialRecaptureStatus.Unknown
      case Some((priorTransition, priorReplay)) =>
        if priorTransition.relationDelta.rootMove.capture.isEmpty then
          LineMaterialRecaptureStatus.Excluded
        else
          priorReplay
            .exactRecaptureStatus(priorTransition.declared, replay, declared)
            .getOrElse(LineMaterialRecaptureStatus.Unknown)

  private def closedMaterialOutcome(
      replay: CanonicalLineReplay,
      terminalPlyOffset: Int
  ): Option[ClosedLineMaterialOutcome] =
    replay.replaySteps.lastOption.flatMap { terminalStep =>
      val terminals = replay.verticalRelationOccurrences(
        terminalStep,
        List(
          RelationFactKind.CreatedCheckResponseInventory,
          RelationFactKind.StalemateTransition
        )
      ).flatMap(occurrence =>
        occurrence.relation.detail match
          case RelationWitnessDetail.CreatedCheckResponseInventory(
                _,
                _,
                _,
                _,
                _,
                _,
                RelationCheckTerminalState.Checkmate,
                _
              ) =>
            Some(ClosedLineMaterialTerminal.Checkmate)
          case RelationWitnessDetail.StalemateTransition(_, _, _, _) =>
            Some(ClosedLineMaterialTerminal.Stalemate)
          case _ =>
            None
      ).distinct
      terminals match
        case terminal :: Nil =>
          Some(ClosedLineMaterialOutcome(terminal, terminalPlyOffset))
        case Nil =>
          None
        case _ =>
          throw IllegalArgumentException("one canonical line cannot terminate as both mate and stalemate")
    }

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
        EvidenceRef.sameMove(candidate.lineRootMove, legalLine.node.ref.rootMove) &&
        replayMoves == evaluationMoves
    ) {
      val node = CandidateLineNode(
        ref = legalLine.node.ref,
        role = candidate.role,
        rank = candidate.rank,
        evaluation = candidate.evaluation,
        lineEvidence = legalLine.node.evidence
      )
      val evaluationKey = allocator.exactKey(List(
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
      from: PositionNode,
      moveUci: String,
      to: PositionNode,
      replay: CanonicalLineReplay,
      allocator: JudgmentProvenanceAllocator
  ): TransitionEdgeAssembly =
    require(
      role != TransitionEdgeRole.LineContinuation,
      "a root move cannot use the line-continuation role"
    )
    assemble(role, from, moveUci, to, replay, allocator, None)

  def fromLineContinuation(
      from: PositionNode,
      moveUci: String,
      to: PositionNode,
      replay: CanonicalLineReplay,
      allocator: JudgmentProvenanceAllocator,
      lineOwner: LegalLineNode,
      linePlyOffset: Int
  ): TransitionEdgeAssembly =
    assemble(
      TransitionEdgeRole.LineContinuation,
      from,
      moveUci,
      to,
      replay,
      allocator,
      Some(lineOwner -> linePlyOffset)
    )

  private def assemble(
      role: TransitionEdgeRole,
      from: PositionNode,
      moveUci: String,
      to: PositionNode,
      replay: CanonicalLineReplay,
      allocator: JudgmentProvenanceAllocator,
      lineOwnership: Option[(LegalLineNode, Int)]
  ): TransitionEdgeAssembly =
    val scope = role.scope
    val occurrenceKey = lineOwnership match
      case Some((line, plyOffset)) =>
        allocator.lineTransitionOccurrenceKey(line.ref, plyOffset, from.ref, moveUci, to.ref)
      case None => allocator.transitionOccurrenceKey(from.ref, moveUci, to.ref)
    val lineOwner = lineOwnership.map(_._1)
    val transitionEvidence =
      allocator.evidenceRef(
        suffix = s"transition:$occurrenceKey",
        producer = EvidenceProducer.MoveTransitionProducer,
        layer = EvidenceLayer.MoveTransition,
        position = from.ref,
        line = lineOwner.map(_.ref),
        scope = scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      )
    val edge =
      MoveTransitionEdge(
        role = role,
        from = from.ref,
        moveUci = EvidenceRef.normalizeMove(moveUci),
        to = to.ref,
        evidence = transitionEvidence
      )
    TransitionEdgeAssembly(
      edge,
      List(
        lineOwner match
          case Some(line) => TransitionFactNormalizer.fromLineContinuation(edge, replay, line.evidence)
          case None       => TransitionFactNormalizer.fromMoveTransition(edge, replay)
      ),
      replay
    )

private[assembly] object NodeLineTransitionAssembler:

  /** The sole node/line/transition assembly implementation for admitted review input. */
  def assemble(input: AdmittedMoveReviewInput): JudgmentAssemblyContext =
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val predecessorReplay = CanonicalLineReplay.fromHistory(input.positionHistory.segmentReplaySteps)
    val admittedPlayedLine = input.playedRootLineOwner
    val beforeAnalysis = admittedPlayedLine.replay.singleStepReplays.head._1.beforeAnalysis
    val before = PositionNodeAssembler.fromAnalysis(
      role = PositionNodeRole.Before,
      fen = input.positionHistory.currentFen,
      ply = input.positionHistory.currentPly,
      analysis = beforeAnalysis,
      allocator = allocator,
      scope = EvidenceScope.BeforePosition
    )
    val rootLines = input.legalLines.map(
      LegalLineAssembler.fromLine(
        _,
        before.node.ref,
        beforeAnalysis,
        input.positionHistory,
        predecessorReplay,
        allocator
      )
    )
    val candidateLines = input.assessmentCandidates.flatMap { candidate =>
      rootLines
        .find(line => EvidenceRef.sameMove(line.node.ref.rootMove, candidate.lineRootMove))
        .flatMap(CandidateLineAssembler.fromProjection(candidate, _, before.node.ref, allocator))
    }
    val rootOccurrences = rootLines.map { line =>
      val (canonical, transitionReplay) = line.replay.singleStepReplays.head
      val played = EvidenceRef.sameMove(line.node.ref.rootMove, input.playedMoveUci)
      val position = PositionNodeAssembler.fromAnalysis(
        role = if played then PositionNodeRole.AfterPlayed else PositionNodeRole.AfterAlternative,
        fen = canonical.declared.fenAfter,
        ply = canonical.declared.ply,
        analysis = canonical.afterAnalysis,
        allocator = allocator,
        scope = if played then EvidenceScope.AfterPlayedPosition else EvidenceScope.AlternativeTransition,
        lineRootOwner = Option.when(!played)(line.node.ref)
      )
      val transition = TransitionEdgeAssembler.fromMove(
        role = if played then TransitionEdgeRole.Played else TransitionEdgeRole.Alternative,
        from = before.node,
        moveUci = line.node.ref.rootMove,
        to = position.node,
        replay = transitionReplay,
        allocator = allocator
      )
      (line, position, transition)
    }
    val (playedOccurrences, alternativeOccurrences) = rootOccurrences.partition { case (_, _, transition) =>
      transition.edge.role == TransitionEdgeRole.Played
    }
    val orderedRootOccurrences = playedOccurrences.head :: alternativeOccurrences
    val rootPositions = before :: orderedRootOccurrences.map(_._2)
    val context = JudgmentAssemblyContext
      .empty(input)
      .withNewPositions(rootPositions.map(assembly => assembly.node -> assembly.analysis))
      .withEvidence(rootPositions.flatMap(_.evidence))
    val withLegalLines =
      rootLines.foldLeft(context) { (ctx, assembly) =>
        ctx.withLegalLine(assembly.node, assembly.replay).withEvidence(assembly.evidence)
      }
    val withCandidates =
      candidateLines.foldLeft(withLegalLines) { (ctx, assembly) =>
        ctx.withCandidateLine(assembly.node).withEvidence(assembly.evidence)
      }
    val rootTransitions = orderedRootOccurrences.map(_._3)
    val withTransitions = withCandidates
      .withNewTransitions(rootTransitions.map(assembly => assembly.edge -> assembly.replay))
      .withEvidence(rootTransitions.flatMap(_.evidence))
    val rootContext = orderedRootOccurrences.foldLeft(withTransitions) {
      case (ctx, (line, _, transition)) =>
        ctx.withLineRootOccurrence(line.node.ref, transition.edge)
    }
    materializeLineContinuations(
      rootContext,
      rootOccurrences.map { case (line, position, _) => line -> position.node },
      allocator
    )

  private def materializeLineContinuations(
      context: JudgmentAssemblyContext,
      roots: List[(LegalLineAssembly, PositionNode)],
      allocator: JudgmentProvenanceAllocator
  ): JudgmentAssemblyContext =
    val assemblies = roots.flatMap { case (line, root) =>
      lineContinuationAssemblies(root, line, allocator)
    }
    val withOccurrences = context
      .withNewPositions(assemblies.map { case (position, _) => position.node -> position.analysis })
      .withNewTransitions(assemblies.map { case (_, transition) => transition.edge -> transition.replay })
    withOccurrences.withEvidence(
      assemblies.flatMap { case (position, transition) => transition.evidence ++ position.evidence }
    )

  private def lineContinuationAssemblies(
      root: PositionNode,
      line: LegalLineAssembly,
      allocator: JudgmentProvenanceAllocator
  ): List[(PositionNodeAssembly, TransitionEdgeAssembly)] =
    line.replay.singleStepReplays.zipWithIndex.drop(1).foldLeft(
      root -> List.empty[(PositionNodeAssembly, TransitionEdgeAssembly)]
    ) { case ((predecessor, reversed), ((canonical, transitionReplay), plyOffset)) =>
      val step = canonical.declared
      val destination = PositionNodeAssembler.lineOccurrenceNode(
        line.node.ref,
        plyOffset,
        predecessor.ref,
        step,
        canonical.afterAnalysis,
        allocator
      )
      val transition = TransitionEdgeAssembler.fromLineContinuation(
        from = predecessor,
        moveUci = step.moveUci,
        to = destination,
        replay = transitionReplay,
        allocator = allocator,
        lineOwner = line.node,
        linePlyOffset = plyOffset
      )
      val position = PositionNodeAssembler.fromLineOccurrence(
        destination,
        line.node,
        plyOffset,
        predecessor.ref,
        step,
        canonical.afterAnalysis,
        allocator,
        transition.edge.evidence
      )
      destination -> ((position -> transition) :: reversed)
    }._2.reverse
