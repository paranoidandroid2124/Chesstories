package lila.chessjudgment.analysis.assembly

import chess.Pawn
import lila.chessjudgment.analysis.evaluation.EvalFactNormalizer
import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.{
  CanonicalPositionHistory,
  PreInitialHistoryKnowledge,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.judgment.*

final case class PositionNodeAssembly(
    node: PositionNode,
    evidence: List[EvidenceRecord],
    analysis: lila.chessjudgment.analysis.position.PositionAnalysis
)

final case class CandidateLineAssembly(
    node: CandidateLineNode,
    evidence: List[EvidenceRecord],
    replay: CanonicalLineReplay
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
    val occurrenceRecord =
      EvidenceRecord(
        ref = EvidenceRef(
          id = allocator.evidenceId(s"position-occurrence:$nodeKey"),
          producer = EvidenceProducer.PositionOccurrenceProducer,
          layer = EvidenceLayer.PositionOccurrence,
          position = ref,
          line = None,
          scope = scope,
          confidence = EvidenceConfidence.BoardDerived
        ),
        payload = PositionOccurrenceEvidence(analysis.occurrence)
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

object CandidateLineAssembler:

  private final case class ReplayedLineFacts(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      materialSummary: Option[LineMaterialSummary]
  )

  def fromLine(
      line: AdmittedReviewLine,
      root: PositionNodeRef,
      rootAnalysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      positionHistory: CanonicalPositionHistory,
      allocator: JudgmentProvenanceAllocator
  ): Option[CandidateLineAssembly] =
    val scope = line.role.scope
    val occurrenceKey = allocator.lineOccurrenceKey(line, root)
    val ref = allocator.lineRef(line, occurrenceKey)
    Option.when(line.evaluation.comparisonReady)(line)
      .flatMap(line => line.replay.rootedAt(rootAnalysis).map(line -> _))
      .flatMap { case (line, replay) => replayFacts(line, replay, root, positionHistory) }
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
      val node =
        CandidateLineNode(
          ref = ref,
          evaluation = line.evaluation,
          evidence = lineEvidence
        )
      val lineRecord =
        LineFactNormalizer.fromValidatedLine(
          id = lineEvidence.id,
          lineRef = ref,
          facts = facts,
          replay = replayed.replay,
          position = root,
          scope = scope,
          materialSummary = replayed.materialSummary,
          predecessorReplay = line.predecessorReplay,
          whitePovMate = line.evaluation.engineLine.flatMap(_.mate)
        )
      val evalRecord =
        EvalFactNormalizer.fromCandidateLine(
          id = allocator.evidenceId(s"eval:$occurrenceKey"),
          line = node,
          position = root,
          scope = scope,
          parents = List(lineRecord.ref)
        )
      CandidateLineAssembly(node, List(lineRecord, evalRecord), replayed.replay)
    }

  private def replayFacts(
      line: AdmittedReviewLine,
      replay: CanonicalLineReplay,
      root: PositionNodeRef,
      positionHistory: CanonicalPositionHistory
  ): Option[ReplayedLineFacts] =
    val replaySteps = replay.replaySteps
    val expectedMoves = line.evaluation.moves.map(PrincipalVariationEvidence.normalizeUci)
    Option.when(
      positionHistory.currentFen == root.fen &&
        positionHistory.currentPly == root.ply &&
        replaySteps.nonEmpty &&
        replaySteps.head.ply == root.ply + 1 &&
        PrincipalVariationEvidence.sameBoardState(replaySteps.head.fenBefore, root.fen) &&
        replaySteps.map(_.moveUci) == expectedMoves
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
        materialSummary = lineMaterialSummary(positionHistory, replay, line.predecessorReplay)
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

    val events = replay.replaySteps.zipWithIndex.flatMap { case (declared, plyOffset) =>
      replay.transition(declared).flatMap { transition =>
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
              replay,
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
    }
    val hasClosedRecaptureKnowledge =
      events.forall(_.capture.forall(
        _.recaptureStatus != LineMaterialRecaptureStatus.Unknown
      ))
    Option.when(events.nonEmpty && hasClosedRecaptureKnowledge)(
      LineMaterialSummary(
        sideToMove = replay.legalSteps.head.move.piece.color,
        events = events,
        closedOutcome = closedMaterialOutcome(replay)
      )
    )

  private def recaptureStatus(
      positionHistory: CanonicalPositionHistory,
      predecessorReplay: Option[CanonicalLineReplay],
      replay: CanonicalLineReplay,
      declared: LineReplayStep,
      plyOffset: Int
  ): LineMaterialRecaptureStatus =
    val priorOccurrence =
      if plyOffset > 0 then
        replay.replaySteps.lift(plyOffset - 1).map(replay -> _)
      else
        predecessorReplay.flatMap(previous =>
          for
            previousStep <- previous.replaySteps.lastOption
            if previousStep.ply + 1 == declared.ply
            if PrincipalVariationEvidence.sameBoardState(previousStep.fenAfter, declared.fenBefore)
          yield previous -> previousStep
        )
    priorOccurrence match
      case None =>
        if plyOffset == 0 &&
            positionHistory.segmentReplaySteps.isEmpty &&
            positionHistory.preInitialHistoryKnowledge == PreInitialHistoryKnowledge.KnownEmpty
        then LineMaterialRecaptureStatus.Excluded
        else LineMaterialRecaptureStatus.Unknown
      case Some((priorReplay, priorStep)) =>
        val priorTransition = priorReplay.transition(priorStep).getOrElse(
          throw IllegalArgumentException("a predecessor replay step lost its canonical transition")
        )
        if priorTransition.relationDelta.rootMove.capture.isEmpty then
          LineMaterialRecaptureStatus.Excluded
        else
          priorReplay
            .exactRecaptureStatus(priorStep, replay, declared)
            .getOrElse(LineMaterialRecaptureStatus.Unknown)

  private def closedMaterialOutcome(
      replay: CanonicalLineReplay
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
          Some(ClosedLineMaterialOutcome(terminal, replay.replaySteps.size - 1))
        case Nil =>
          None
        case _ =>
          throw IllegalArgumentException("one canonical line cannot terminate as both mate and stalemate")
    }

object TransitionEdgeAssembler:

  def fromMove(
      role: TransitionEdgeRole,
      from: PositionNode,
      moveUci: String,
      to: PositionNode,
      replay: CanonicalLineReplay,
      allocator: JudgmentProvenanceAllocator
  ): TransitionEdgeAssembly =
    val scope = role.scope
    val occurrenceKey = allocator.transitionOccurrenceKey(role, from.ref, moveUci, to.ref)
    val transitionEvidence =
      allocator.evidenceRef(
        suffix = s"transition:$occurrenceKey",
        producer = EvidenceProducer.MoveTransitionProducer,
        layer = EvidenceLayer.MoveTransition,
        position = from.ref,
        line = None,
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
    TransitionEdgeAssembly(edge, List(TransitionFactNormalizer.fromMoveTransition(edge, replay)), replay)

object NodeLineTransitionAssembler:

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    MoveReviewInputAdmission.admit(raw).flatMap(assemble)

  /** The sole node/line/transition assembly implementation for admitted review input. */
  def assemble(input: AdmittedMoveReviewInput): Option[JudgmentAssemblyContext] =
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    for
      playedLine <- input.playedRootLineOwner
      playedReplayStep <- playedLine.replay.replaySteps.headOption
      beforeAnalysis <- playedLine.replay.analysisBefore(playedReplayStep)
      afterPlayedAnalysis <- playedLine.replay.analysisAfter(playedReplayStep)
      if beforeAnalysis.occurrence.plyCount == input.beforePly
      if PrincipalVariationEvidence.sameBoardState(beforeAnalysis.occurrence.fen, input.beforeFen)
      if PrincipalVariationEvidence.sameBoardState(playedReplayStep.fenAfter, input.afterPlayedFen)
    yield
      val before = PositionNodeAssembler.fromAnalysis(
        role = PositionNodeRole.Before,
        fen = input.beforeFen,
        ply = input.beforePly,
        analysis = beforeAnalysis,
        allocator = allocator,
        scope = EvidenceScope.BeforePosition
      )
      val afterPlayed = PositionNodeAssembler.fromAnalysis(
        role = PositionNodeRole.AfterPlayed,
        fen = input.afterPlayedFen,
        ply = input.beforePly + 1,
        analysis = afterPlayedAnalysis,
        allocator = allocator,
        scope = EvidenceScope.AfterPlayedPosition
      )
      val rootLines = input.lines
        .flatMap(
          CandidateLineAssembler.fromLine(
            _,
            before.node.ref,
            beforeAnalysis,
            input.positionHistory,
            allocator
          )
        )
      val afterReference =
        for
          reference <- input.referenceLine
          if !reference.rootMove.exists(EvidenceRef.sameMove(_, input.playedMoveUci))
          fen <- input.afterReferenceFen
          referenceAssembly <- rootLines.filter(assembly =>
            assembly.node.ref.role == reference.role &&
              assembly.node.ref.rank == reference.rank &&
              reference.rootMove.exists(EvidenceRef.sameMove(_, assembly.node.ref.rootMove))
          ) match
            case assembly :: Nil => Some(assembly)
            case _               => None
          replayStep <- referenceAssembly.replay.replaySteps.headOption
          analysis <- referenceAssembly.replay.analysisAfter(replayStep)
          if PrincipalVariationEvidence.sameBoardState(replayStep.fenAfter, fen)
          transitionReplay <- admittedTransition(
            referenceAssembly.replay,
            input.beforeFen,
            reference.rootMove.getOrElse(""),
            fen
          )
        yield
          reference -> (PositionNodeAssembler.fromAnalysis(
            role = PositionNodeRole.AfterReference,
            fen = fen,
            ply = input.beforePly + 1,
            analysis = analysis,
            allocator = allocator,
            scope = EvidenceScope.AfterReferencePosition
          ) -> transitionReplay)
      val afterAlternatives =
        input.lines.filter(_.role == LineNodeRole.Alternative).flatMap { line =>
          rootLines.filter(assembly =>
            assembly.node.ref.role == line.role &&
              assembly.node.ref.rank == line.rank &&
              line.rootMove.exists(EvidenceRef.sameMove(_, assembly.node.ref.rootMove))
          ) match
            case assembly :: Nil =>
              assembly.replay.replaySteps.headOption.flatMap { first =>
                assembly.replay.analysisAfter(first).map { analysis =>
                  (
                    line,
                    PositionNodeAssembler.fromAnalysis(
                      role = PositionNodeRole.AfterAlternative,
                      fen = first.fenAfter,
                      ply = input.beforePly + 1,
                      analysis = analysis,
                      allocator = allocator,
                      scope = EvidenceScope.AlternativeTransition
                    ),
                    assembly.replay
                  )
                }
              }
            case _ => None
        }
      val lines = rootLines
      val playedTransition = admittedTransition(
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
      val referenceTransition =
        afterReference.map { case (reference, (referencePosition, replay)) =>
          TransitionEdgeAssembler.fromMove(
            role = TransitionEdgeRole.Reference,
            from = before.node,
            moveUci = reference.rootMove.getOrElse(""),
            to = referencePosition.node,
            replay = replay,
            allocator = allocator
          )
        }
      val alternativeTransitions =
        afterAlternatives.flatMap { case (line, position, replay) =>
          line.rootMove.flatMap { move =>
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
        }
      val context =
        (List(before, afterPlayed) ++
          afterReference.toList.map(_._2._1) ++
          afterAlternatives.map(_._2))
          .foldLeft(JudgmentAssemblyContext.empty(input)) { (ctx, assembly) =>
            ctx.withPosition(assembly.node, assembly.analysis).withEvidence(assembly.evidence)
          }
      val withLines =
        lines.foldLeft(context) { (ctx, assembly) =>
          ctx.withLine(assembly.node, assembly.replay).withEvidence(assembly.evidence)
        }
      val withTransitions =
        (playedTransition.toList ++
          referenceTransition.toList ++
          alternativeTransitions).foldLeft(withLines) { (ctx, assembly) =>
          ctx.withTransition(assembly.edge, assembly.replay).withEvidence(assembly.evidence)
        }
      val rootLineBindings =
        playedTransition.toList.map(transition =>
          uniqueLineForTransition(rootLines, transition.edge) -> transition.edge
        ) ++
          referenceTransition.toList.map(transition =>
            uniqueLineForTransition(rootLines, transition.edge) -> transition.edge
          ) ++
          alternativeTransitions.map(transition =>
            uniqueLineForTransition(rootLines, transition.edge) -> transition.edge
          )
      rootLineBindings.foldLeft(withTransitions) { case (ctx, (line, edge)) =>
        ctx.withLineRootOccurrence(line, edge)
      }

  private def uniqueLineForTransition(
      lines: List[CandidateLineAssembly],
      edge: MoveTransitionEdge
  ): LineNodeRef =
    lines.filter(assembly =>
      edge.role.acceptsLineOwnerRole(assembly.node.role) &&
        assembly.node.evidence.position == edge.from &&
        EvidenceRef.sameMove(assembly.node.ref.rootMove, edge.moveUci)
    ) match
      case line :: Nil => line.node.ref
      case _ =>
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' has no unique candidate-line root owner"
        )

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
