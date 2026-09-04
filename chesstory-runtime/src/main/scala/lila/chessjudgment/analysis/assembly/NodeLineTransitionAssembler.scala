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

final case class PositionNodeAssembly(
    node: PositionNode,
    evidence: List[EvidenceRecord],
    analysis: lila.chessjudgment.analysis.position.PositionAnalysis
)

final case class LegalLineAssembly(
    node: LegalLineNode,
    evidence: List[EvidenceRecord],
    replay: CanonicalLineReplay
)

final case class CandidateLineAssembly(
    node: CandidateLineNode,
    evidence: List[EvidenceRecord]
)

final case class TransitionEdgeAssembly(
    edge: MoveTransitionEdge,
    evidence: List[EvidenceRecord],
    replay: CanonicalLineReplay
)

object PositionNodeAssembler:
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

object LegalLineAssembler:

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
  ): Option[LegalLineAssembly] =
    val scope = EvidenceScope.LegalLine
    val occurrenceKey = allocator.lineOccurrenceKey(line, root)
    val ref = allocator.lineRef(line, occurrenceKey)
    line.replay
      .rootedAt(rootAnalysis)
      .flatMap(replay => replayFacts(replay, root, positionHistory, predecessorReplay))
      .map { replayed =>
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
    }

  private def replayFacts(
      replay: CanonicalLineReplay,
      root: PositionNodeRef,
      positionHistory: CanonicalPositionHistory,
      predecessorReplay: Option[CanonicalLineReplay]
  ): Option[ReplayedLineFacts] =
    val replaySteps = replay.replaySteps
    Option.when(
      positionHistory.currentFen == root.fen &&
        positionHistory.currentPly == root.ply &&
        replaySteps.nonEmpty &&
        replaySteps.head.ply == root.ply + 1 &&
        PrincipalVariationEvidence.sameBoardState(replaySteps.head.fenBefore, root.fen)
    ) {
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
    }

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
    val projection = replay.singleStepReplays
    val singleStepReplays = projection.getOrElse(Nil)
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
    projection.flatMap { _ =>
      Option.when(events.nonEmpty && hasClosedRecaptureKnowledge)(
        LineMaterialSummary(
          sideToMove = replay.legalSteps.head.move.piece.color,
          events = events,
          closedOutcome = singleStepReplays.lastOption.flatMap { case (_, terminalReplay) =>
            closedMaterialOutcome(terminalReplay, singleStepReplays.size - 1)
          }
        )
      )
    }

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
          .flatMap(_.singleStepReplays)
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
          VerticalRelationContractKind.CreatedCheckResponseInventory,
          VerticalRelationContractKind.StalemateTransition
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

object CandidateLineAssembler:
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

object TransitionEdgeAssembler:

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

object NodeLineTransitionAssembler:

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    MoveReviewInputAdmission.admit(raw).flatMap(assemble)

  /** The sole node/line/transition assembly implementation for admitted review input. */
  def assemble(input: AdmittedMoveReviewInput): Option[JudgmentAssemblyContext] =
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val predecessorReplay = CanonicalLineReplay.fromHistory(input.positionHistory.segmentReplaySteps)
    for
      playedLine <- input.playedRootLineOwner
      playedReplayStep <- playedLine.replay.replaySteps.headOption
      beforeAnalysis <- playedLine.replay.analysisBefore(playedReplayStep)
      afterPlayedAnalysis <- playedLine.replay.analysisAfter(playedReplayStep)
      if beforeAnalysis.occurrence.plyCount == input.beforePly
      if PrincipalVariationEvidence.sameBoardState(beforeAnalysis.occurrence.fen, input.beforeFen)
      if PrincipalVariationEvidence.sameBoardState(playedReplayStep.fenAfter, input.afterPlayedFen)
      before = PositionNodeAssembler.fromAnalysis(
        role = PositionNodeRole.Before,
        fen = input.beforeFen,
        ply = input.beforePly,
        analysis = beforeAnalysis,
        allocator = allocator,
        scope = EvidenceScope.BeforePosition
      )
      afterPlayed = PositionNodeAssembler.fromAnalysis(
        role = PositionNodeRole.AfterPlayed,
        fen = input.afterPlayedFen,
        ply = input.beforePly + 1,
        analysis = afterPlayedAnalysis,
        allocator = allocator,
        scope = EvidenceScope.AfterPlayedPosition
      )
      rootLineOptions = input.legalLines.map(
          LegalLineAssembler.fromLine(
            _,
            before.node.ref,
            beforeAnalysis,
            input.positionHistory,
            predecessorReplay,
            allocator
          )
        )
      rootLines <- Option.when(rootLineOptions.forall(_.nonEmpty))(rootLineOptions.flatten)
      candidateLineOptions = input.assessmentCandidates.map(candidate =>
        rootLines.filter(line => EvidenceRef.sameMove(line.node.ref.rootMove, candidate.lineRootMove)) match
          case legalLine :: Nil =>
            CandidateLineAssembler.fromProjection(candidate, legalLine, before.node.ref, allocator)
          case _ => None
      )
      candidateLines <- Option.when(candidateLineOptions.forall(_.nonEmpty))(candidateLineOptions.flatten)
      afterReferenceOption = input.referenceCandidate match
        case Some(reference)
            if !EvidenceRef.sameMove(reference.lineRootMove, input.playedMoveUci) =>
          (for
            fen <- input.afterReferenceFen
            referenceAssembly <- rootLines.filter(assembly =>
              EvidenceRef.sameMove(reference.lineRootMove, assembly.node.ref.rootMove)
            ) match
              case assembly :: Nil => Some(assembly)
              case _               => None
            replayStep <- referenceAssembly.replay.replaySteps.headOption
            analysis <- referenceAssembly.replay.analysisAfter(replayStep)
            if PrincipalVariationEvidence.sameBoardState(replayStep.fenAfter, fen)
            transitionReplay <- admittedTransition(
              referenceAssembly.replay,
              input.beforeFen,
              reference.lineRootMove,
              fen
            )
          yield
            Some(reference -> (PositionNodeAssembler.fromAnalysis(
              role = PositionNodeRole.AfterReference,
              fen = fen,
              ply = input.beforePly + 1,
              analysis = analysis,
              allocator = allocator,
              scope = EvidenceScope.AfterReferencePosition
            ) -> transitionReplay)))
        case Some(_) => Some(None)
        case None    => None
      afterReference <- afterReferenceOption
      alternativeRootLines = rootLines.filterNot(assembly =>
        EvidenceRef.sameMove(assembly.node.ref.rootMove, input.playedMoveUci) ||
          input.referenceCandidate.exists(reference =>
            EvidenceRef.sameMove(assembly.node.ref.rootMove, reference.lineRootMove)
          )
      )
      afterAlternativeOptions = alternativeRootLines.map { assembly =>
        assembly.replay.replaySteps.headOption.flatMap { first =>
          assembly.replay.analysisAfter(first).map { analysis =>
            (
              assembly,
              PositionNodeAssembler.fromAnalysis(
                role = PositionNodeRole.AfterAlternative,
                fen = first.fenAfter,
                ply = input.beforePly + 1,
                analysis = analysis,
                allocator = allocator,
                scope = EvidenceScope.AlternativeTransition,
                lineRootOwner = Some(assembly.node.ref)
              ),
              assembly.replay
            )
          }
        }
      }
      afterAlternatives <- Option.when(afterAlternativeOptions.forall(_.nonEmpty))(
        afterAlternativeOptions.flatten
      )
      playedTransition <- admittedTransition(
        playedLine.replay,
        input.beforeFen,
        input.playedMoveUci,
        input.afterPlayedFen
      ).map(replay =>
        TransitionEdgeAssembler.fromMove(
          role = TransitionEdgeRole.Played,
          from = before.node,
          moveUci = input.playedMoveUci,
          to = afterPlayed.node,
          replay = replay,
          allocator = allocator
        )
      )
      referenceTransition = afterReference.map { case (reference, (referencePosition, replay)) =>
        TransitionEdgeAssembler.fromMove(
          role = TransitionEdgeRole.Reference,
          from = before.node,
          moveUci = reference.lineRootMove,
          to = referencePosition.node,
          replay = replay,
          allocator = allocator
        )
      }
      alternativeTransitionOptions = afterAlternatives.map { case (line, position, replay) =>
        val move = line.node.ref.rootMove
        admittedTransition(replay, input.beforeFen, move, position.node.ref.fen).map { transitionReplay =>
            TransitionEdgeAssembler.fromMove(
              role = TransitionEdgeRole.Alternative,
              from = before.node,
              moveUci = move,
              to = position.node,
              replay = transitionReplay,
              allocator = allocator
            )
        }
      }
      alternativeTransitions <- Option.when(alternativeTransitionOptions.forall(_.nonEmpty))(
        alternativeTransitionOptions.flatten
      )
      transitionAssemblies = playedTransition :: (referenceTransition.toList ++ alternativeTransitions)
      if transitionAssemblies.size == rootLines.size
      if transitionAssemblies.map(_.edge.evidence.id).distinct.size == transitionAssemblies.size
      rootLineBindingOptions = transitionAssemblies.map(transition =>
        uniqueLineForTransition(rootLines, transition.edge).map(_ -> transition.edge)
      )
      rootLineBindings <- Option.when(rootLineBindingOptions.forall(_.nonEmpty))(
        rootLineBindingOptions.flatten
      )
      if rootLineBindings.map(_._1).toSet == rootLines.map(_.node.ref).toSet
      rootContext =
        val rootPositions = List(before, afterPlayed) ++
          afterReference.toList.map(_._2._1) ++
          afterAlternatives.map(_._2)
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
        val withTransitions = withCandidates
          .withNewTransitions(transitionAssemblies.map(assembly => assembly.edge -> assembly.replay))
          .withEvidence(transitionAssemblies.flatMap(_.evidence))
        rootLineBindings.foldLeft(withTransitions) { case (ctx, (line, edge)) =>
          ctx.withLineRootOccurrence(line, edge)
        }
      fullContext <- materializeLineContinuations(rootContext, rootLines, allocator)
    yield fullContext

  private def materializeLineContinuations(
      context: JudgmentAssemblyContext,
      lines: List[LegalLineAssembly],
      allocator: JudgmentProvenanceAllocator
  ): Option[JudgmentAssemblyContext] =
    val continuations = lines.map(line => lineContinuationAssemblies(context, line, allocator))
    Option.when(continuations.forall(_.nonEmpty))(continuations.flatten.flatten).map { assemblies =>
      val withOccurrences = context
        .withNewPositions(assemblies.map { case (position, _) => position.node -> position.analysis })
        .withNewTransitions(assemblies.map { case (_, transition) => transition.edge -> transition.replay })
      withOccurrences.withEvidence(
        assemblies.flatMap { case (position, transition) => transition.evidence ++ position.evidence }
      )
    }

  private def lineContinuationAssemblies(
      context: JudgmentAssemblyContext,
      line: LegalLineAssembly,
      allocator: JudgmentProvenanceAllocator
  ): Option[List[(PositionNodeAssembly, TransitionEdgeAssembly)]] =
    for
      rootOccurrence <- context.lineRootOccurrence(line.node.ref)
      rootDestination <- context.positions.find(_.ref == rootOccurrence.destination)
      singleStepReplays <- line.replay.singleStepReplays
    yield
      singleStepReplays.zipWithIndex.drop(1).foldLeft(
        rootDestination -> List.empty[(PositionNodeAssembly, TransitionEdgeAssembly)]
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

  private def uniqueLineForTransition(
      lines: List[LegalLineAssembly],
      edge: MoveTransitionEdge
  ): Option[LineNodeRef] =
    lines.filter(assembly =>
      assembly.node.evidence.position == edge.from &&
        EvidenceRef.sameMove(assembly.node.ref.rootMove, edge.moveUci)
    ) match
      case line :: Nil => Some(line.node.ref)
      case _           => None

  private def admittedTransition(
      replay: CanonicalLineReplay,
      fenBefore: String,
      moveUci: String,
      fenAfter: String
  ): Option[CanonicalLineReplay] =
    replay.replaySteps match
      case step :: _ if
        EvidenceRef.sameMove(step.moveUci, moveUci) &&
          PrincipalVariationEvidence.sameBoardState(step.fenBefore, fenBefore) &&
          PrincipalVariationEvidence.sameBoardState(step.fenAfter, fenAfter) =>
        replay.subset(List(step))
      case _ => None
