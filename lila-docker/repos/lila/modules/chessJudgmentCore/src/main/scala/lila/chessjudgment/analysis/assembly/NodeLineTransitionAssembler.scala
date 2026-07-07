package lila.chessjudgment.analysis.assembly

import chess.{ Bishop, Knight, Pawn, Queen, Rook }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.evaluation.{ EvalFactNormalizer, EvaluationPerspectivePolicy }
import lila.chessjudgment.analysis.line.{ ForcedLineTruth, LineFactNormalizer, PrincipalVariationEvidence }
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.analysis.position.{ FactExtractor, PositionAnalyzer, PositionFactNormalizer }
import lila.chessjudgment.analysis.singlePosition.{ SinglePositionAssessor, SinglePositionFactNormalizer }
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.analysis.tactical.{ BoundedReplayStep, TacticalRelationEvidence }
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.{ Fact, FactScope }
import lila.chessjudgment.model.strategic.VariationLine
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

final case class NodeLineTransitionAssembly(
    input: NormalizedMoveReviewInput,
    context: JudgmentAssemblyContext
)

object PositionNodeAssembler:

  def fromFen(
      input: NormalizedMoveReviewInput,
      role: PositionNodeRole,
      fen: String,
      ply: Int,
      allocator: JudgmentProvenanceAllocator,
      scope: EvidenceScope,
      includeAssessment: Boolean,
      assessmentSourceLines: Option[List[VariationLine]] = None,
      assessmentWhitePovEvalCp: Option[Int] = None
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
              FactExtractor.extractStaticFacts(position.board, side) ++
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
      val assessmentRecord =
        Option
          .when(includeAssessment)(features)
          .flatten
          .flatMap { positionFeatures =>
            val assessmentSide = ref.sideToMove.getOrElse(position.color)
            assessmentWhitePovEval(input, role, assessmentWhitePovEvalCp).map { currentWhitePovEvalCp =>
              val assessment =
                SinglePositionAssessor.classify(
                  features = positionFeatures,
                  multiPv = assessmentLines(input, role, assessmentSide, assessmentSourceLines),
                  currentWhitePovEvalCp = currentWhitePovEvalCp,
                  sideToMove = assessmentSide
                )
              SinglePositionFactNormalizer.fromAssessment(
                id = allocator.evidenceId(s"single-position:$nodeKey"),
                assessment = assessment,
                position = ref,
                scope = scope,
                parents = List(boardRecord.ref)
              )
            }
          }
      val records = boardRecord :: assessmentRecord.toList
      val node =
        PositionNodeBuilder.fromAnalysis(
          role = role,
          ref = ref,
          facts = facts,
          features = features,
          assessment = assessmentRecord.collect { case EvidenceRecord(_, SinglePositionEvidence(assessment), _) => assessment },
          evidence = records.map(_.ref)
        )
      PositionNodeAssembly(node, records)
    }

  private def assessmentLines(
      input: NormalizedMoveReviewInput,
      role: PositionNodeRole,
      sideToMove: chess.Color,
      sourceLines: Option[List[VariationLine]]
  ): List[lila.chessjudgment.analysis.singlePosition.PvLine] =
    val lines =
      sourceLines.getOrElse {
        role match
          case PositionNodeRole.Before =>
            input.lines.map(_.line)
          case PositionNodeRole.AfterPlayed =>
            input.playedLine.map(line => line.line.copy(moves = line.line.moves.drop(1))).toList
          case PositionNodeRole.AfterReference =>
            input.referenceLine.map(line => line.line.copy(moves = line.line.moves.drop(1))).toList
          case PositionNodeRole.AfterAlternative =>
            input.lines
              .filter(_.role == LineNodeRole.Alternative)
              .map(line => line.line.copy(moves = line.line.moves.drop(1)))
          case PositionNodeRole.AfterThreat =>
            Nil
      }
    EvaluationPerspectivePolicy.sideToMovePvLines(sideToMove, lines)

  private def assessmentWhitePovEval(
      input: NormalizedMoveReviewInput,
      role: PositionNodeRole,
      overrideWhitePovEval: Option[Int]
  ): Option[Int] =
    overrideWhitePovEval.orElse {
      role match
        case PositionNodeRole.Before =>
          Some(input.currentWhitePovEvalCp)
        case PositionNodeRole.AfterPlayed =>
          input.playedLine.map(_.line.scoreCp)
        case PositionNodeRole.AfterReference =>
          input.referenceLine.map(_.line.scoreCp)
        case PositionNodeRole.AfterAlternative =>
          input.lines.find(_.role == LineNodeRole.Alternative).map(_.line.scoreCp)
        case PositionNodeRole.AfterThreat =>
          Some(input.currentWhitePovEvalCp)
    }

object CandidateLineAssembler:

  private final case class ReplayedLineFacts(
      facts: PrincipalVariationEvidence.LineFacts,
      materialSummary: Option[LineMaterialSummary]
  )

  def fromLine(
      line: NormalizedCandidateLine,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): Option[CandidateLineAssembly] =
    val scope = line.role.scope
    val ref = allocator.lineRef(line)
    replayFacts(line, root).map { replayed =>
      val facts = replayed.facts
      val forcedTheme =
        ForcedLineTruth
          .detect(root.fen, ref.rootMove, List(line.line))
          .map(LineFactNormalizer.fromForcedTheme)
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
        CandidateLineNodeBuilder.fromEngineLine(
          role = line.role,
          ref = ref,
          line = line.line,
          whitePovEvalCp = line.line.scoreCp,
          mate = line.line.mate,
          depth = line.line.depth,
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
          materialSummary = replayed.materialSummary
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
      root: PositionNodeRef
  ): Option[ReplayedLineFacts] =
    val refs = scala.collection.mutable.ListBuffer.empty[PrincipalVariationEvidence.LineMoveRef]
    var currentFen = root.fen
    var currentPly = root.ply
    val iterator = line.line.moves.iterator
    var legal = true
    while iterator.hasNext && legal do
      val move = PrincipalVariationEvidence.normalizeUci(iterator.next())
      PrincipalVariationEvidence.legalFenAfter(currentFen, move) match
        case Some(afterFen) =>
          currentPly += 1
          refs += PrincipalVariationEvidence.LineMoveRef(currentPly, move, afterFen)
          currentFen = afterFen
        case None =>
          legal = false
    val replayed = refs.toList
    val replayedMoves = replayed.map(_.uci)
    Option
      .when(replayed.nonEmpty)(PrincipalVariationEvidence.LineVariationRef(replayed))
      .flatMap(PrincipalVariationEvidence.validatedLineFromStart(root.fen, _))
      .flatMap { validated =>
        validated.first.map { first =>
          ReplayedLineFacts(
            facts = PrincipalVariationEvidence.LineFacts(
              line = validated.line,
              first = first,
              reply = validated.reply,
              continuation = validated.continuation,
              continuationTail = validated.moves.drop(3).take(3)
            ),
            materialSummary = lineMaterialSummary(replayedMoves, root.fen)
          )
        }
      }

  private def lineMaterialSummary(
      moves: List[String],
      rootFen: String
  ): Option[LineMaterialSummary] =
    TacticalRelationEvidence.boundedReplay(rootFen, moves, maxPlies = moves.size).flatMap { replay =>
      val sideToMove = replay.headOption.map(_.move.piece.color)
      sideToMove.flatMap { mover =>
        val captures = replay.zipWithIndex.flatMap { case (step, index) =>
          if LineFactNormalizer.castlingSide(PrincipalVariationEvidence.normalizeUci(step.uci)).nonEmpty then None
          else step.capturedRole.map { captured =>
            val previous = replay.lift(index - 1)
            LineMaterialCapture(
              moveUci = step.uci,
              plyOffset = index,
              side = step.move.piece.color,
              attackerRole = EvidencePieceRole(step.move.piece.role.toString),
              capturedRole = EvidencePieceRole(captured.toString),
              square = EvidenceSquare(step.move.dest.key),
              valueCp = MaterialValue.materialValueCp(captured),
              recapture = previous.exists(prev =>
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
            materialWindowComplete = replay.size == moves.size
          )
        )
      }
    }

  private def promotionGainCp(step: BoundedReplayStep): Int =
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
      MoveTransitionEdgeBuilder.fromMove(
        role = role,
        id = allocator.transitionId(role, moveUci),
        from = from.ref,
        moveUci = MoveReviewInputNormalizer.normalizeUci(moveUci),
        to = to.ref,
        changedFacts = changedFacts(from.facts, to.facts),
        planTransition = None,
        evidence = transitionEvidence
      )
    TransitionEdgeAssembly(edge, List(TransitionFactNormalizer.fromMoveTransition(edge)))

  private def changedFacts(before: List[Fact], after: List[Fact]): List[Fact] =
    after.filterNot(before.contains)

object NodeLineTransitionAssembler:

  def assemble(raw: RawMoveReviewInput): Option[NodeLineTransitionAssembly] =
    MoveReviewInputNormalizer.normalize(raw).flatMap { input =>
      val allocator = JudgmentProvenanceAllocator.forInput(input)
      for
        before <- PositionNodeAssembler.fromFen(
          input = input,
          role = PositionNodeRole.Before,
          fen = input.beforeFen,
          ply = input.beforePly,
          allocator = allocator,
          scope = EvidenceScope.BeforePosition,
          includeAssessment = true
        )
        afterPlayed <- PositionNodeAssembler.fromFen(
          input = input,
          role = PositionNodeRole.AfterPlayed,
          fen = input.afterPlayedFen,
          ply = input.beforePly + 1,
          allocator = allocator,
          scope = EvidenceScope.AfterPlayedPosition,
          includeAssessment = true
        )
      yield
        val afterReference =
          input.afterReferenceFen.flatMap { fen =>
            PositionNodeAssembler.fromFen(
              input = input,
              role = PositionNodeRole.AfterReference,
              fen = fen,
              ply = input.beforePly + 1,
              allocator = allocator,
              scope = EvidenceScope.AfterReferencePosition,
              includeAssessment = true
            )
          }
        val afterAlternatives =
          input.lines.filter(_.role == LineNodeRole.Alternative).flatMap { line =>
            line.rootMove
              .flatMap(PrincipalVariationEvidence.legalFenAfter(input.beforeFen, _))
              .flatMap { fen =>
                PositionNodeAssembler.fromFen(
                  input = input,
                  role = PositionNodeRole.AfterAlternative,
                  fen = fen,
                  ply = input.beforePly + 1,
                  allocator = allocator,
                  scope = EvidenceScope.AlternativeTransition,
                  includeAssessment = true,
                  assessmentSourceLines = Some(List(line.line.copy(moves = line.line.moves.drop(1)))),
                  assessmentWhitePovEvalCp = Some(line.line.scoreCp)
                ).map(line -> _)
              }
          }
        val afterThreats =
          input.threatBranches.flatMap { branch =>
            PositionNodeAssembler.fromFen(
              input = input,
              role = PositionNodeRole.AfterThreat,
              fen = branch.branchFen,
              ply = branch.branchPly,
              allocator = allocator,
              scope = EvidenceScope.ThreatLine,
              includeAssessment = true,
              assessmentSourceLines = Some(branch.lines.map(_.line)),
              assessmentWhitePovEvalCp = branch.lines.headOption.map(_.line.scoreCp)
            ).map(branch -> _)
          }
        val rootLines = input.lines.flatMap(CandidateLineAssembler.fromLine(_, before.node.ref, allocator))
        val threatLines =
          afterThreats.flatMap { case (branch, position) =>
            branch.lines.flatMap(CandidateLineAssembler.fromLine(_, position.node.ref, allocator))
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
            .foldLeft(JudgmentAssemblyContext.empty().copy(probeDiagnostics = input.probeDiagnostics)) { (ctx, assembly) =>
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
        NodeLineTransitionAssembly(input, withTransitions)
    }
