package lila.chessjudgment.analysis.assembly

import chess.{ Bishop, Knight, Pawn, Queen, Rook }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.evaluation.EvalFactNormalizer
import lila.chessjudgment.analysis.line.{ ForcedLineTruth, LineFactNormalizer }
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.analysis.position.{ FactExtractor, PositionAnalyzer, PositionFactNormalizer }
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.FactScope
import lila.chessjudgment.model.line.{
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  CanonicalPositionHistoryStep,
  PreInitialHistoryKnowledge,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.judgment.*

final case class PositionNodeAssembly(
    node: PositionNode,
    evidence: List[EvidenceRecord]
)

final case class CandidateLineAssembly(
    node: CandidateLineNode,
    evidence: List[EvidenceRecord]
)

final case class TransitionEdgeAssembly(
    edge: MoveTransitionEdge,
    evidence: List[EvidenceRecord]
)

object PositionNodeAssembler:

  def fromFen(
      role: PositionNodeRole,
      fen: String,
      ply: Int,
      allocator: JudgmentProvenanceAllocator,
      scope: EvidenceScope
  ): Option[PositionNodeAssembly] =
    Fen.read(Standard, Fen.Full(fen)).map { position =>
      val ref = allocator.positionRef(role, fen, ply, Some(position.color))
      val nodeKey = allocator.positionKey(role, fen, ply)
      val features = PositionAnalyzer.extractFeatures(fen, ply)
      val stateMotifFacts =
        FactExtractor.fromMotifs(position.board, MoveAnalyzer.detectStateMotifs(position, ply), FactScope.Now)
      val facts =
        (
          stateMotifFacts ++
            List(position.color, !position.color).flatMap { side =>
              val endgameFeature = EndgamePatternOracle.analyze(position.board, side)
              FactExtractor.extractStaticFacts(position, side) ++
                FactExtractor.extractEndgameFacts(position.board, side, endgameFeature)
            }
        ).distinct
      val boardRecord =
        PositionFactNormalizer.fromBoardFacts(
          id = allocator.evidenceId(s"board:$nodeKey"),
          facts = facts,
          features = features,
          position = ref,
          scope = scope
        )
      val node =
        PositionNode(
          role = role,
          ref = ref
        )
      PositionNodeAssembly(node, List(boardRecord))
    }

object CandidateLineAssembler:

  private final case class ReplayedLineFacts(
      facts: PrincipalVariationEvidence.LineFacts,
      materialSummary: Option[LineMaterialSummary]
  )

  def fromLine(
      line: NormalizedCandidateLine,
      root: PositionNodeRef,
      positionHistory: CanonicalPositionHistory,
      allocator: JudgmentProvenanceAllocator
  ): Option[CandidateLineAssembly] =
    val scope = line.role.scope
    val ref = allocator.lineRef(line)
    Option.when(line.evaluation.comparisonReady)(line).flatMap(replayFacts(_, root, positionHistory)).map { replayed =>
      val facts = replayed.facts
      val forcedTheme =
        line.evaluation.engineLine.flatMap(engineLine =>
          ForcedLineTruth
            .detect(root.fen, ref.rootMove, List(engineLine))
            .map(LineFactNormalizer.fromForcedTheme)
        )
      val lineEvidence =
        allocator.evidenceRef(
          suffix = s"line:${allocator.key(line.role)}:${line.rank}",
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
          position = root,
          scope = scope,
          forcedTheme = forcedTheme,
          materialSummary = replayed.materialSummary,
          whitePovMate = line.evaluation.engineLine.flatMap(_.mate)
        )
      val evalRecord =
        EvalFactNormalizer.fromCandidateLine(
          id = allocator.evidenceId(s"eval:${allocator.key(line.role)}:${line.rank}"),
          line = node,
          position = root,
          scope = scope,
          parents = List(lineRecord.ref)
        )
      CandidateLineAssembly(node, List(lineRecord, evalRecord))
    }

  private def replayFacts(
      line: NormalizedCandidateLine,
      root: PositionNodeRef,
      positionHistory: CanonicalPositionHistory
  ): Option[ReplayedLineFacts] =
    positionHistory.extend(line.evaluation.moves).toOption.flatMap { lineHistory =>
      val prefixSize = positionHistory.segmentReplaySteps.size
      val replaySteps = lineHistory.segmentReplaySteps.drop(prefixSize)
      val expectedMoves = line.evaluation.moves.map(PrincipalVariationEvidence.normalizeUci)
      Option
        .when(
          lineHistory.segmentReplaySteps.take(prefixSize) == positionHistory.segmentReplaySteps &&
            positionHistory.currentFen == root.fen &&
            positionHistory.currentPly == root.ply &&
            replaySteps.nonEmpty &&
            replaySteps.map(_.uci) == expectedMoves
        )(replaySteps)
        .map { steps =>
          val moves = steps.map(step => PrincipalVariationEvidence.LineMoveRef(step.ply, step.uci, step.afterFen))
          val first = moves.head
          ReplayedLineFacts(
            facts = PrincipalVariationEvidence.LineFacts(
              line = PrincipalVariationEvidence.LineVariationRef(moves),
              first = first,
              reply = moves.lift(1),
              continuation = moves.lift(2),
              continuationTail = moves.drop(3).take(3)
            ),
            materialSummary = lineMaterialSummary(positionHistory, lineHistory)
          )
        }
    }

  private[assembly] def lineMaterialSummary(
      positionHistory: CanonicalPositionHistory,
      lineHistory: CanonicalPositionHistory
  ): Option[LineMaterialSummary] =
    val prefixSize = positionHistory.segmentReplaySteps.size
    val replay = lineHistory.segmentReplaySteps.drop(prefixSize)
    val belongsToPositionHistory =
      lineHistory.segmentReplaySteps.take(prefixSize) == positionHistory.segmentReplaySteps
    if !belongsToPositionHistory || replay.isEmpty ||
      (replay.head.capturedRole.nonEmpty &&
        positionHistory.segmentReplaySteps.isEmpty &&
        positionHistory.preInitialHistoryKnowledge == PreInitialHistoryKnowledge.Unknown)
    then None
    else
      val sideToMove = replay.headOption.map(_.move.piece.color)
      sideToMove.flatMap { mover =>
        val captures = replay.zipWithIndex.flatMap { case (step, index) =>
          if step.castle.nonEmpty then None
          else step.capturedRole.map { captured =>
            val previous =
              if index == 0 then positionHistory.segmentReplaySteps.lastOption
              else replay.lift(index - 1)
            LineMaterialCapture(
              moveUci = step.uci,
              plyOffset = index,
              side = step.move.piece.color,
              attackerRole = EvidencePieceRole(step.move.piece.role.toString),
              capturedRole = EvidencePieceRole(captured.toString),
              square = EvidenceSquare(step.move.dest.key),
              valueCp = MaterialValue.materialValueCp(captured),
              recapture =
                previous.exists(prev =>
                  prev.capturedRole.nonEmpty &&
                    prev.move.dest == step.move.dest &&
                    prev.move.piece.color != step.move.piece.color
                )
            )
          }
        }
        val promotionGain =
          replay.map { step =>
            val gain = promotionGainCp(step)
            if step.move.piece.color == mover then gain else -gain
          }.sum
        val signedValues =
          captures.map(capture => if capture.side == mover then capture.valueCp else -capture.valueCp) ++
            Option.when(promotionGain != 0)(promotionGain).toList
        val running = signedValues.scanLeft(0)(_ + _).tail
        val net = running.lastOption.getOrElse(0)
        Option.when(captures.nonEmpty || promotionGain != 0)(
          LineMaterialSummary(
            sideToMove = mover,
            captures = captures,
            netCaptureCpForMover = net,
            maxGainCpForMover = (0 :: running).max,
            maxLossCpForMover = (0 :: running).min,
            hasRecaptureChain = captures.exists(_.recapture),
            hasRecoveryWindow = running.exists(_ < 0) && running.exists(_ >= 0) && net >= 0,
            promotionGainCpForMover = promotionGain,
            materialWindowComplete = replay.size == lineHistory.segmentReplaySteps.size - prefixSize
          )
        )
      }

  private def promotionGainCp(step: CanonicalPositionHistoryStep): Int =
    Option.when(step.uci.length == 5) {
      val promotedValue = step.uci.last.toLower match
        case 'q' => MaterialValue.materialValueCp(Queen)
        case 'r' => MaterialValue.materialValueCp(Rook)
        case 'b' => MaterialValue.materialValueCp(Bishop)
        case 'n' => MaterialValue.materialValueCp(Knight)
        case _   => MaterialValue.materialValueCp(Queen)
      promotedValue - MaterialValue.materialValueCp(Pawn)
    }.getOrElse(0)

object TransitionEdgeAssembler:

  def fromMove(
      role: TransitionEdgeRole,
      from: PositionNode,
      moveUci: String,
      to: PositionNode,
      allocator: JudgmentProvenanceAllocator
  ): TransitionEdgeAssembly =
    val scope = role.scope
    val transitionEvidence =
      allocator.evidenceRef(
        suffix = s"transition:${allocator.key(role)}:${MoveReviewInputNormalizer.normalizeUci(moveUci)}",
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
        moveUci = MoveReviewInputNormalizer.normalizeUci(moveUci),
        to = to.ref,
        evidence = transitionEvidence
      )
    TransitionEdgeAssembly(edge, List(TransitionFactNormalizer.fromMoveTransition(edge)))

object NodeLineTransitionAssembler:
  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    MoveReviewInputNormalizer.normalize(raw).flatMap(assemble)

  /** The sole node/line/transition assembly implementation for normalized input. */
  def assemble(input: NormalizedMoveReviewInput): Option[JudgmentAssemblyContext] =
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    for
      before <- PositionNodeAssembler.fromFen(
        role = PositionNodeRole.Before,
        fen = input.beforeFen,
        ply = input.beforePly,
        allocator = allocator,
        scope = EvidenceScope.BeforePosition
      )
      afterPlayed <- PositionNodeAssembler.fromFen(
        role = PositionNodeRole.AfterPlayed,
        fen = input.afterPlayedFen,
        ply = input.beforePly + 1,
        allocator = allocator,
        scope = EvidenceScope.AfterPlayedPosition
      )
    yield
      val afterReference =
        input.afterReferenceFen.flatMap { fen =>
          PositionNodeAssembler.fromFen(
            role = PositionNodeRole.AfterReference,
            fen = fen,
            ply = input.beforePly + 1,
            allocator = allocator,
            scope = EvidenceScope.AfterReferencePosition
          )
        }
      val afterAlternatives =
        input.lines.filter(_.role == LineNodeRole.Alternative).flatMap { line =>
          line.rootMove
            .flatMap(PrincipalVariationEvidence.legalFenAfter(input.beforeFen, _))
            .flatMap { fen =>
              PositionNodeAssembler.fromFen(
                role = PositionNodeRole.AfterAlternative,
                fen = fen,
                ply = input.beforePly + 1,
                allocator = allocator,
                scope = EvidenceScope.AlternativeTransition
              ).map(line -> _)
            }
        }
      val afterThreats =
        input.threatBranches.flatMap { branch =>
          PositionNodeAssembler.fromFen(
            role = PositionNodeRole.AfterThreat,
            fen = branch.branchFen,
            ply = branch.branchPly,
            allocator = allocator,
            scope = EvidenceScope.ThreatLine
          ).map(branch -> _)
        }
      val rootLines = input.lines
        .flatMap(CandidateLineAssembler.fromLine(_, before.node.ref, input.positionHistory, allocator))
      val threatLines =
        afterThreats.flatMap { case (branch, position) =>
          input.positionHistory
            .extend(List(branch.probedMoveUci))
            .toOption
            .filter(history => history.currentFen == branch.branchFen && history.currentPly == branch.branchPly)
            .toList
            .flatMap(history =>
              branch.lines.flatMap(CandidateLineAssembler.fromLine(_, position.node.ref, history, allocator))
            )
        }
      val lines = rootLines ++ threatLines
      val playedTransition =
        TransitionEdgeAssembler.fromMove(
          role = TransitionEdgeRole.Played,
          from = before.node,
          moveUci = input.playedMoveUci,
          to = afterPlayed.node,
          allocator = allocator
        )
      val referenceTransition =
        for
          referencePosition <- afterReference
          referenceMove <- input.referenceLine.flatMap(_.rootMove)
        yield
          TransitionEdgeAssembler.fromMove(
            role = TransitionEdgeRole.Reference,
            from = before.node,
            moveUci = referenceMove,
            to = referencePosition.node,
            allocator = allocator
          )
      val alternativeTransitions =
        afterAlternatives.flatMap { case (line, position) =>
          line.rootMove.map { move =>
            TransitionEdgeAssembler.fromMove(
              role = TransitionEdgeRole.Alternative,
              from = before.node,
              moveUci = move,
              to = position.node,
              allocator = allocator
            )
          }
        }
      val threatTransitions =
        afterThreats.map { case (branch, position) =>
          TransitionEdgeAssembler.fromMove(
            role = TransitionEdgeRole.Threat,
            from = before.node,
            moveUci = branch.probedMoveUci,
            to = position.node,
            allocator = allocator
          )
        }
      val context =
        (List(before, afterPlayed) ++ afterReference.toList ++ afterAlternatives.map(_._2) ++ afterThreats.map(_._2))
          .foldLeft(JudgmentAssemblyContext.empty(input)) { (ctx, assembly) =>
            ctx.withPosition(assembly.node).withEvidence(assembly.evidence)
          }
      val withLines =
        lines.foldLeft(context) { (ctx, assembly) =>
          ctx.withLine(assembly.node).withEvidence(assembly.evidence)
        }
      val withTransitions =
        (List(playedTransition) ++ referenceTransition.toList ++ alternativeTransitions ++ threatTransitions).foldLeft(withLines) { (ctx, assembly) =>
          ctx.withTransition(assembly.edge).withEvidence(assembly.evidence)
        }
      withTransitions
