package lila.chessjudgment.analysis.assembly

import chess.Pawn
import chess.format.Fen
import lila.chessjudgment.analysis.evaluation.EvalFactNormalizer
import lila.chessjudgment.analysis.line.{ ForcedLineTruth, LineFactNormalizer }
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
      analysis.features.fen == fen && analysis.features.plyCount == ply,
      "a position node must use the analysis of its exact occurrence"
    )
    val ref = lineRootOwner
      .map(allocator.lineRootPositionRef(role, fen, ply, Some(position.color), _))
      .getOrElse(allocator.positionRef(role, fen, ply, Some(position.color)))
    val nodeKey = lineRootOwner
      .map(allocator.lineRootPositionKey(role, fen, ply, _))
      .getOrElse(allocator.positionKey(role, fen, ply))
    val featureRecord =
      EvidenceRecord(
        ref = EvidenceRef(
          id = allocator.evidenceId(s"position-feature:$nodeKey"),
          producer = EvidenceProducer.PositionFeatureProducer,
          layer = EvidenceLayer.PositionFeature,
          position = ref,
          line = None,
          scope = scope,
          confidence = EvidenceConfidence.BoardDerived
        ),
        payload = PositionFeatureEvidence(analysis.features)
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
    PositionNodeAssembly(node, featureRecord :: relationRecords, analysis)

object CandidateLineAssembler:

  private final case class ReplayedLineFacts(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      materialSummary: Option[LineMaterialSummary]
  )

  def fromLine(
      line: NormalizedCandidateLine,
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
      val forcedTheme =
        line.evaluation.engineLine.flatMap(engineLine =>
          ForcedLineTruth
            .detect(replayed.replay, List(engineLine))
            .map(LineFactNormalizer.fromForcedTheme)
        )
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
          forcedTheme = forcedTheme,
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
      line: NormalizedCandidateLine,
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
    Option.when(belongsToPositionHistory)(replaySteps)
      .flatMap(CanonicalLineReplay.fromHistory)
      .flatMap(lineMaterialSummary(positionHistory, _))

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
              plyOffset,
              root
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
    Option.when(events.nonEmpty)(
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
      plyOffset: Int,
      root: CanonicalRootLegalMove
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
          val inventories = priorReplay.verticalRelationOccurrences(
            priorStep,
            List(VerticalRelationContractKind.CaptureRecaptureInventory)
          )
          require(
            inventories.size == 1,
            "a canonical capture must own exactly one closed recapture inventory"
          )
          val occurrence = inventories.head
          val matching = occurrence.relation.detail match
            case RelationWitnessDetail.CaptureRecaptureInventory(_, _, _, legalRecaptures, _) =>
              legalRecaptures.filter(resource =>
                EvidenceRef.sameMove(resource.moveUci, declared.moveUci) &&
                  resource.movement == root.witness &&
                  resource.capture == root.capture
              )
            case _ =>
              throw IllegalArgumentException("a recapture contract changed relation kind")
          matching match
            case _ :: Nil =>
              LineMaterialRecaptureStatus.Proven(DerivedRelationResultKey.from(occurrence.relation))
            case Nil =>
              LineMaterialRecaptureStatus.Excluded
            case _ =>
              throw IllegalArgumentException("one legal move cannot occur twice in a closed recapture inventory")

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
  private final case class LineRootTransitionAssembly(
      line: LineNodeRef,
      destination: PositionNodeAssembly,
      transition: TransitionEdgeAssembly
  )

  private final case class AdmittedBranchPredecessor(
      replay: CanonicalLineReplay,
      history: CanonicalPositionHistory
  )

  private final case class ThreatRootOccurrence(
      moveUci: String,
      fenBefore: String,
      fenAfter: String,
      ply: Int
  ):
    def stableKey: String =
      List(ply.toString, moveUci, fenBefore, fenAfter).mkString("|")

  private object ThreatRootOccurrence:
    def from(input: NormalizedMoveReviewInput, branch: NormalizedThreatBranch): ThreatRootOccurrence =
      ThreatRootOccurrence(
        moveUci = EvidenceRef.normalizeMove(branch.probedMoveUci),
        fenBefore = PrincipalVariationEvidence.normalizeFen(input.beforeFen),
        fenAfter = PrincipalVariationEvidence.normalizeFen(branch.branchFen),
        ply = branch.branchPly
      )

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    MoveReviewInputNormalizer.normalize(raw).flatMap(assemble)

  /** The sole node/line/transition assembly implementation for normalized input. */
  def assemble(input: NormalizedMoveReviewInput): Option[JudgmentAssemblyContext] =
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    for
      playedLine <- input.playedLine
      playedReplayStep <- playedLine.replay.replaySteps.headOption
      beforeAnalysis <- playedLine.replay.analysisBefore(playedReplayStep)
      afterPlayedAnalysis <- playedLine.replay.analysisAfter(playedReplayStep)
      if beforeAnalysis.features.plyCount == input.beforePly
      if PrincipalVariationEvidence.sameBoardState(beforeAnalysis.features.fen, input.beforeFen)
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
      val admittedThreats =
        input.threatBranches.flatMap { branch =>
          admittedBranchPredecessor(input, branch)
            .map(predecessor => branch -> predecessor)
        }
      val threatOccurrences = admittedThreats
        .map { case (branch, _) => ThreatRootOccurrence.from(input, branch) }
        .distinct
        .sortBy(_.stableKey)
      val conflictingThreatMoves = threatOccurrences
        .groupBy(_.moveUci)
        .collect { case (move, occurrences) if occurrences.size > 1 => move }
        .toList
        .sorted
      require(
        conflictingThreatMoves.isEmpty,
        s"one threat root move cannot own multiple transition occurrences: ${conflictingThreatMoves.mkString(",")}"
      )
      val threatTransitionReplays = threatOccurrences.flatMap { occurrence =>
        val legalMoves = beforeAnalysis.actualLegalMoves.filter(move =>
          EvidenceRef.sameMove(move.toUci.uci, occurrence.moveUci) &&
            PrincipalVariationEvidence.sameBoardState(
              Fen.write(move.after).value,
              occurrence.fenAfter
            )
        )
        legalMoves match
          case move :: Nil =>
            val step = LineReplayStep(
              ply = occurrence.ply,
              moveUci = occurrence.moveUci,
              fenBefore = occurrence.fenBefore,
              fenAfter = occurrence.fenAfter
            )
            CanonicalLineReplay
              .fromAnalyzedLegalMove(step, beforeAnalysis, move)
              .map(occurrence -> _)
          case _ => None
      }.toMap
      require(
        threatTransitionReplays.size == threatOccurrences.size,
        "every threat transition occurrence must have exactly one root-owned legal replay"
      )
      val threatPositionAssemblies = threatOccurrences.map { occurrence =>
        val replay = threatTransitionReplays(occurrence)
        val step = replay.replaySteps.head
        val analysis = replay.analysisAfter(step).getOrElse(
          throw IllegalArgumentException("a threat transition replay must own its destination analysis")
        )
        occurrence -> PositionNodeAssembler.fromAnalysis(
          role = PositionNodeRole.AfterThreat,
          fen = occurrence.fenAfter,
          ply = occurrence.ply,
          analysis = analysis,
          allocator = allocator,
          scope = EvidenceScope.ThreatLine
        )
      }
      val threatPositionsByOccurrence = threatPositionAssemblies.toMap
      val afterThreats = admittedThreats.map { case (branch, predecessor) =>
        val occurrence = ThreatRootOccurrence.from(input, branch)
        (branch, predecessor) -> threatPositionsByOccurrence(occurrence)
      }
      val threatLineAssemblies =
        afterThreats.flatMap { case ((branch, predecessor), position) =>
          val owner = ThreatLineOccurrenceOwner(
            probedMoveUci = EvidenceRef.normalizeMove(branch.probedMoveUci),
            branchPosition = position.node.ref
          )
          branch.lines.flatMap(line =>
            CandidateLineAssembler
              .fromLine(
                line,
                position.node.ref,
                position.analysis,
                predecessor.history,
                allocator
              )
              .map(owner -> _)
          )
        }
      val threatLines = threatLineAssemblies.map(_._2)
      val lines = rootLines ++ threatLines
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
      val threatTransitions =
        threatPositionAssemblies.map { case (occurrence, position) =>
          TransitionEdgeAssembler.fromMove(
            role = TransitionEdgeRole.Threat,
            from = before.node,
            moveUci = occurrence.moveUci,
            to = position.node,
            replay = threatTransitionReplays(occurrence),
            allocator = allocator
          )
        }
      val threatContinuationRoots = threatLineAssemblies.map { case (owner, lineAssembly) =>
        val start = threatPositionAssemblies.collect {
          case (_, position) if position.node.ref == owner.branchPosition => position
        } match
          case position :: Nil => position
          case _ =>
            throw IllegalArgumentException(
              s"threat line '${lineAssembly.node.ref.id}' has no unique registered start occurrence"
            )
        val rootStep = lineAssembly.replay.replaySteps.headOption.getOrElse(
          throw IllegalArgumentException(s"threat line '${lineAssembly.node.ref.id}' has no root step")
        )
        val rootReplay = lineAssembly.replay.subset(List(rootStep)).getOrElse(
          throw IllegalArgumentException(
            s"threat line '${lineAssembly.node.ref.id}' root step is not an admitted replay subset"
          )
        )
        val afterRootAnalysis = lineAssembly.replay.analysisAfter(rootStep).getOrElse(
          throw IllegalArgumentException(
            s"threat line '${lineAssembly.node.ref.id}' root step has no destination analysis"
          )
        )
        val destination = PositionNodeAssembler.fromAnalysis(
          role = PositionNodeRole.AfterThreatContinuation,
          fen = rootStep.fenAfter,
          ply = rootStep.ply,
          analysis = afterRootAnalysis,
          allocator = allocator,
          scope = EvidenceScope.ThreatLine,
          lineRootOwner = Some(lineAssembly.node.ref)
        )
        val transition = TransitionEdgeAssembler.fromMove(
          role = TransitionEdgeRole.ThreatContinuation,
          from = start.node,
          moveUci = rootStep.moveUci,
          to = destination.node,
          replay = rootReplay,
          allocator = allocator
        )
        LineRootTransitionAssembly(lineAssembly.node.ref, destination, transition)
      }
      val context =
        (List(before, afterPlayed) ++
          afterReference.toList.map(_._2._1) ++
          afterAlternatives.map(_._2) ++
          threatPositionAssemblies.map(_._2) ++
          threatContinuationRoots.map(_.destination))
          .foldLeft(JudgmentAssemblyContext.empty(input)) { (ctx, assembly) =>
            ctx.withPosition(assembly.node, assembly.analysis).withEvidence(assembly.evidence)
          }
      val withLines =
        lines.foldLeft(context) { (ctx, assembly) =>
          ctx.withLine(assembly.node, assembly.replay).withEvidence(assembly.evidence)
        }
      val withThreatLineOwners =
        threatLineAssemblies.foldLeft(withLines) { case (ctx, (owner, assembly)) =>
          ctx.withThreatLineOwner(assembly.node.ref, owner)
        }
      val withTransitions =
        (playedTransition.toList ++
          referenceTransition.toList ++
          alternativeTransitions ++
          threatTransitions ++
          threatContinuationRoots.map(_.transition)).foldLeft(withThreatLineOwners) { (ctx, assembly) =>
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
          ) ++
          threatContinuationRoots.map(root => root.line -> root.transition.edge)
      rootLineBindings.foldLeft(withTransitions) { case (ctx, (line, edge)) =>
        ctx.withLineRootOccurrence(line, edge)
      }

  private def uniqueLineForTransition(
      lines: List[CandidateLineAssembly],
      edge: MoveTransitionEdge
  ): LineNodeRef =
    lines.filter(assembly =>
      assembly.node.role == edge.role.lineRole &&
        assembly.node.evidence.position == edge.from &&
        EvidenceRef.sameMove(assembly.node.ref.rootMove, edge.moveUci)
    ) match
      case line :: Nil => line.node.ref
      case _ =>
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' has no unique candidate-line root owner"
        )

  private def admittedBranchPredecessor(
      input: NormalizedMoveReviewInput,
      branch: NormalizedThreatBranch
  ): Option[AdmittedBranchPredecessor] =
    val candidates = branch.lines.flatMap(_.predecessorReplay).flatMap { replay =>
      for
        first <- replay.replaySteps.headOption
        last <- replay.replaySteps.lastOption
        if replay.replaySteps.size == 1 && replay.legalSteps.size == 1
        if branch.branchPly == input.beforePly + 1
        if EvidenceRef.sameMove(first.moveUci, branch.probedMoveUci)
        if PrincipalVariationEvidence.sameBoardState(first.fenBefore, input.beforeFen)
        if last.ply == branch.branchPly
        if PrincipalVariationEvidence.sameBoardState(last.fenAfter, branch.branchFen)
        history <- input.positionHistory.extendAdmitted(replay.legalSteps).toOption
        if history.currentPly == branch.branchPly
        if PrincipalVariationEvidence.sameBoardState(history.currentFen, branch.branchFen)
        transition <- admittedTransition(replay, input.beforeFen, branch.probedMoveUci, branch.branchFen)
        transitionStep <- transition.replaySteps.headOption
        if transitionStep.ply == input.beforePly + 1
      yield AdmittedBranchPredecessor(replay, history)
    }
    uniqueSemanticPredecessor(candidates.map(_.replay))
      .flatMap(selected => candidates.find(candidate => candidate.replay eq selected))

  private[assembly] def uniqueSemanticPredecessor(
      candidates: List[CanonicalLineReplay]
  ): Option[CanonicalLineReplay] =
    val keyed = candidates.flatMap(replay => replaySemanticKey(replay).map(_ -> replay))
    keyed.groupMap(_._1)(_._2).values.toList match
      case equivalent :: Nil => equivalent.sortBy(candidateOrderKey).headOption
      case _                 => None

  private def replaySemanticKey(
      replay: CanonicalLineReplay
  ): Option[List[(Int, String, String, String)]] =
    val steps = replay.replaySteps.flatMap { step =>
      for
        before <- PrincipalVariationEvidence.semanticBoardStateFen(step.fenBefore)
        after <- PrincipalVariationEvidence.semanticBoardStateFen(step.fenAfter)
      yield (step.ply, EvidenceRef.normalizeMove(step.moveUci), before, after)
    }
    Option.when(steps.size == replay.replaySteps.size)(steps)

  private def candidateOrderKey(replay: CanonicalLineReplay): String =
    replay.replaySteps
      .map(step =>
        List(
          step.ply.toString,
          EvidenceRef.normalizeMove(step.moveUci),
          PrincipalVariationEvidence.normalizeFen(step.fenBefore),
          PrincipalVariationEvidence.normalizeFen(step.fenAfter)
        ).mkString("\u0001")
      )
      .mkString("\u0000")

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
