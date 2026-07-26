package lila.chessjudgment.analysis.assembly

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import chess.{ Bishop, Knight, Pawn, Queen, Rook, Square }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.evaluation.EvalFactNormalizer
import lila.chessjudgment.analysis.line.{ ForcedLineTruth, LineFactNormalizer }
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.analysis.position.{ FactExtractor, PositionAnalyzer, PositionFactNormalizer }
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.analysis.tactical.TacticalRelationEvidence
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.{ FactScope, ProbeContractValidator, ProbePurpose, ProbeRequest, ProbeResult }
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
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
      allocator: JudgmentProvenanceAllocator,
      previousCaptureSquare: Option[Square] = None
  ): Option[CandidateLineAssembly] =
    val scope = line.role.scope
    val ref = allocator.lineRef(line)
    replayFacts(line, root, previousCaptureSquare).map { replayed =>
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
        CandidateLineNode(
          ref = ref,
          line = line.line,
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
          whitePovMate = line.line.mate
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
      previousCaptureSquare: Option[Square]
  ): Option[ReplayedLineFacts] =
    PrincipalVariationEvidence
      .legalReplay(root.fen, line.line.moves, root.ply)
      .map(_.map(_._2))
      .filter(_.nonEmpty)
      .map(PrincipalVariationEvidence.LineVariationRef.apply)
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
            materialSummary = lineMaterialSummary(validated.moves.map(_.uci), root.fen, previousCaptureSquare)
          )
        }
      }

  private[assembly] def lineMaterialSummary(
      moves: List[String],
      rootFen: String,
      previousCaptureSquare: Option[Square]
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
              recapture =
                (index == 0 && previousCaptureSquare.contains(step.move.dest)) ||
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
            materialWindowComplete = replay.size == moves.size
          )
        )
      }
    }

  private def promotionGainCp(step: LegalReplayStep): Int =
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

  private def previousCaptureSquare(input: NormalizedMoveReviewInput): Option[Square] =
    Option
      .when(input.movePrefixUci.nonEmpty && input.movePrefixUci.size == input.beforePly)(
        PrincipalVariationEvidence.legalMoveReplay(Standard.initialFen.value, input.movePrefixUci, 0)
      )
      .flatten
      .filter(_.lastOption.exists(step =>
        PrincipalVariationEvidence.sameBoardState(Fen.write(step.after).value, input.beforeFen)
      ))
      .flatMap(_.lastOption)
      .filter(_.capturedRole.nonEmpty)
      .map(_.move.dest)

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    MoveReviewInputNormalizer.normalize(raw).flatMap { input =>
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
        val rootPreviousCaptureSquare = previousCaptureSquare(input)
        val rootLines = input.lines
          .flatMap(CandidateLineAssembler.fromLine(_, before.node.ref, allocator, rootPreviousCaptureSquare))
          .map { assembly =>
            val evidence = assembly.evidence.map {
              case record @ EvidenceRecord(_, lineFacts: LineFactEvidence, _) =>
                record.copy(
                  payload = EndgameTablebaseProbeBinding.admit(
                    assembly.node,
                    lineFacts,
                    input.endgameTablebaseResults
                  )
                )
              case record =>
                record
            }
            assembly.copy(evidence = evidence)
          }
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
    }

private[assembly] object EndgameTablebaseProbeBinding:
  val Objective = "endgame_tablebase"
  val RequiredSignals: List[String] = List("tablebase", "purpose", "variationHash")

  def requestFor(
      line: CandidateLineNode,
      lineFacts: LineFactEvidence,
      horizon: LineEndgameTechniqueHorizon
  ): Option[ProbeRequest] =
    val replay = lineFacts.lineReplaySteps
    for
      trigger <- horizon.triggerMove.map(EvidenceRef.normalizeMove)
      if horizon.pattern == "Triangulation"
      if horizon.status == LineEndgameTechniqueHorizonStatus.Completed
      if horizon.zugzwangProof.isEmpty
      if horizon.entryPlyOffset == 0 && horizon.terminalPlyOffset == 4
      if EvidenceRef.sameMove(trigger, line.ref.rootMove)
      entry <- replay.lift(horizon.entryPlyOffset)
      terminal <- replay.lift(horizon.terminalPlyOffset)
      comparisonPosition <- position(entry.fenBefore)
      terminalPosition <- position(terminal.fenAfter)
      if comparisonPosition.color == horizon.techniqueSide
      if terminalPosition.color == !horizon.techniqueSide
      if sameBoardState(entry.fenBefore, terminal.fenAfter)
      if terminalPosition.board.occupied.count <= 7
      legalMoves = terminalPosition.legalMoves.map(_.toUci.uci).distinct.sorted
      if legalMoves.nonEmpty
      bindingHash = positionBindingHash(terminal.fenAfter, entry.fenBefore, legalMoves)
    yield ProbeRequest(
      id = s"endgame-tablebase:${bindingHash.take(24)}",
      fen = terminal.fenAfter,
      moves = legalMoves,
      depth = 0,
      purpose = Some(ProbePurpose.EndgameTablebase),
      objective = Some(Objective),
      requiredSignals = RequiredSignals,
      variationHash = Some(bindingHash),
      comparisonFen = Some(entry.fenBefore)
    )

  def admit(
      line: CandidateLineNode,
      lineFacts: LineFactEvidence,
      results: List[ProbeResult]
  ): LineFactEvidence =
    lineFacts.endgameTechniqueHorizons.foldLeft(lineFacts) { (current, horizon) =>
      val proofs = results.flatMap(exactProof(line, lineFacts, horizon, _)).distinct
      proofs match
        case proof :: Nil =>
          current.withEndgameZugzwangProof(horizon.entryPlyOffset, horizon.terminalPlyOffset, proof)
        case _ =>
          current
    }

  private def exactProof(
      line: CandidateLineNode,
      lineFacts: LineFactEvidence,
      horizon: LineEndgameTechniqueHorizon,
      result: ProbeResult
  ): Option[EndgameZugzwangProof] =
    for
      request <- requestFor(line, lineFacts, horizon)
      if result.id == request.id
      if result.purpose.contains(ProbePurpose.EndgameTablebase)
      if result.variationHash == request.variationHash
      if ProbeContractValidator.validateAgainstRequest(request, result).isValid
      tablebase <- result.tablebase
      comparisonFen <- request.comparisonFen
      if normalizeFen(tablebase.terminal.fen) == normalizeFen(request.fen)
      if normalizeFen(tablebase.comparison.fen) == normalizeFen(comparisonFen)
      terminalPosition <- position(request.fen)
      comparisonPosition <- position(comparisonFen)
      if terminalPosition.color == !horizon.techniqueSide
      if comparisonPosition.color == horizon.techniqueSide
      if tablebase.terminal.wdl == -2 && tablebase.comparison.wdl == 2
      terminalDtm <- tablebase.terminal.dtm.filter(_ < 0)
      comparisonDtm <- tablebase.comparison.dtm.filter(_ > 0)
      replies = tablebase.legalMoves.map(reply => reply.copy(moveUci = EvidenceRef.normalizeMove(reply.moveUci)))
      expectedMoves = request.moves.map(EvidenceRef.normalizeMove).distinct.sorted
      if replies.nonEmpty && replies.map(_.moveUci).distinct.size == replies.size
      if replies.map(_.moveUci).sorted == expectedMoves
      if replies.forall(_.resultingWdl == 2)
      replyDtms <- Option.when(replies.forall(_.resultingDtm.exists(_ > 0)))(replies.flatMap(_.resultingDtm))
      maxReplyDtm <- replyDtms.maxOption
      if terminalDtm.toLong.abs == maxReplyDtm.toLong + 1L
      if comparisonDtm > maxReplyDtm
      proof <- EndgameZugzwangProof.verified(
        constrainedSide = terminalPosition.color,
        terminalFen = request.fen,
        comparisonFen = comparisonFen,
        terminalDtm = terminalDtm,
        comparisonDtm = comparisonDtm,
        legalReplies =
          replies
            .zip(replyDtms)
            .map((reply, dtm) => EndgameZugzwangReplyProof(reply.moveUci, dtm))
            .sortBy(_.moveUci),
        horizon = horizon,
        replay = lineFacts.lineReplaySteps
      )
    yield proof

  private def positionBindingHash(terminalFen: String, comparisonFen: String, legalMoves: List[String]): String =
    val raw = List(normalizeFen(terminalFen), normalizeFen(comparisonFen), legalMoves.sorted.mkString(",")).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def position(fen: String): Option[_root_.chess.Position] =
    _root_.chess.format.Fen.read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(fen))

  private def sameBoardState(left: String, right: String): Boolean =
    def state(fen: String): List[String] =
      normalizeFen(fen).split("\\s+").toList.zipWithIndex.collect { case (field, index) if index == 0 || index == 2 || index == 3 => field }
    state(left) == state(right)

  private def normalizeFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).mkString(" ")
