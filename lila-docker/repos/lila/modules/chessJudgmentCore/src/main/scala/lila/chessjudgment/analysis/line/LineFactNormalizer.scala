package lila.chessjudgment.analysis.line

import chess.{ Color, King, Pawn, Position }
import chess.format.Fen
import chess.variant.Standard

import lila.chessjudgment.analysis.position.PositionAnalyzer
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.model.judgment.*

object LineFactNormalizer:

  def fromValidatedLine(
      id: String,
      lineRef: LineNodeRef,
      facts: PrincipalVariationEvidence.LineFacts,
      position: PositionNodeRef,
      scope: EvidenceScope,
      forcedTheme: Option[ForcedLineThemeEvidence] = None,
      materialSummary: Option[LineMaterialSummary] = None,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val replay = replaySteps(position.fen, facts)
    val events = lineEvents(position.fen, lineRef, facts, replay, forcedTheme, materialSummary)
    val baseConsequences = lineConsequences(facts, forcedTheme, materialSummary)
    val endgameHorizons = endgameTechniqueHorizons(position.fen, replay, baseConsequences)
    val consequences = (baseConsequences ++ endgameTechniqueConsequences(facts, endgameHorizons)).distinct
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = position,
        line = Some(lineRef),
        scope = scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      )
    EvidenceRecord(
      ref = ref,
      payload = LineFactEvidence(
        line = lineRef,
        firstMove = Some(facts.first.uci),
        replyMove = facts.reply.map(_.uci),
        continuationMoves = facts.continuation.toList.map(_.uci) ++ facts.continuationTail.map(_.uci),
        forcedTheme = forcedTheme,
        material = materialSummary
      )(
        replay = replay,
        events = events,
        consequences = consequences,
        endgameHorizons = endgameHorizons
      ),
      parents = parents
    )

  def fromForcedTheme(theme: ForcedLineTruth.VerifiedTheme): ForcedLineThemeEvidence =
    ForcedLineThemeEvidence(
      id = theme.id,
      lineMoves = theme.lineMoves
    )

  private def replaySteps(startFen: String, facts: PrincipalVariationEvidence.LineFacts): List[LineReplayStep] =
    facts.line.moves.foldLeft((startFen, List.empty[LineReplayStep])) { case ((fenBefore, acc), move) =>
      move.fenAfter -> (LineReplayStep(
        ply = move.ply,
        moveUci = move.uci,
        fenBefore = fenBefore,
        fenAfter = move.fenAfter
      ) :: acc)
    }._2.reverse

  private def lineEvents(
      startFen: String,
      lineRef: LineNodeRef,
      facts: PrincipalVariationEvidence.LineFacts,
      replay: List[LineReplayStep],
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary]
  ): List[LineMoveEvent] =
    val replayEvents =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        val checkDefense =
          replay.lift(index).toList.flatMap(step =>
            positionAfter(step.fenBefore).toList.filter(_.check.yes).map { position =>
              LineMoveEvent(
                kind = LineEventKind.DefenderMove,
                moveUci = normalized,
                plyOffset = index,
                side = Some(position.color),
                targetRole = Some(EvidencePieceRole(King.name)),
                square = position.board.kingPosOf(position.color).map(square => EvidenceSquare(square.key))
              )
            }
          )
        val stateEvents =
          positionAfter(move.fenAfter).toList.flatMap { position =>
            val kingSquare = position.board.kingPosOf(position.color).map(square => EvidenceSquare(square.key))
            val checkLike =
              Option.when(position.checkMate)(
                List(
                  LineMoveEvent(
                    kind = LineEventKind.Mate,
                    moveUci = normalized,
                    plyOffset = index,
                    side = Some(!position.color),
                    pieceRole = Some(EvidencePieceRole(King.name)),
                    square = kingSquare
                  ),
                  LineMoveEvent(
                    kind = LineEventKind.Tempo,
                    moveUci = normalized,
                    plyOffset = index,
                    side = Some(!position.color),
                    pieceRole = Some(EvidencePieceRole(King.name)),
                    square = kingSquare
                  )
                )
              ).orElse(
                Option.when(position.check.yes)(
                  List(
                    LineMoveEvent(
                      kind = LineEventKind.Check,
                      moveUci = normalized,
                      plyOffset = index,
                      side = Some(!position.color),
                      pieceRole = Some(EvidencePieceRole(King.name)),
                      square = kingSquare
                    ),
                    LineMoveEvent(
                      kind = LineEventKind.Tempo,
                      moveUci = normalized,
                      plyOffset = index,
                      side = Some(!position.color),
                      pieceRole = Some(EvidencePieceRole(King.name)),
                      square = kingSquare
                    )
                  )
                )
              ).getOrElse(Nil)
            checkLike ++ List(
              Option.when(position.staleMate)(
                LineMoveEvent(
                  kind = LineEventKind.Stalemate,
                  moveUci = normalized,
                  plyOffset = index,
                  side = Some(!position.color),
                  pieceRole = Some(EvidencePieceRole(King.name)),
                  square = position.board.kingPosOf(position.color).map(square => EvidenceSquare(square.key))
                )
              )
            ).flatten
          }
        castlingEvent(normalized, index).toList ++ checkDefense ++ stateEvents
      }
    val roleEvents =
      Option
        .when(lineRef.role == LineNodeRole.Threat)(
          LineMoveEvent(
            kind = LineEventKind.Threat,
            moveUci = PrincipalVariationEvidence.normalizeUci(facts.first.uci),
            plyOffset = 0
          )
        )
        .toList
    val forcedEvents =
      forcedTheme.toList.flatMap(theme =>
        theme.lineMoves.headOption.toList.flatMap(move =>
          List(
            LineMoveEvent(
              kind = LineEventKind.ForcedTheme,
              moveUci = move,
              plyOffset = 0
            ),
            LineMoveEvent(
              kind = LineEventKind.Threat,
              moveUci = move,
              plyOffset = 0
            )
          ) ++ Option.when(theme.id == ForcedLineTruth.ImmediateReplyCheckId)(
            LineMoveEvent(
              kind = LineEventKind.Tempo,
              moveUci = move,
              plyOffset = 0
            )
          ).toList
        )
      )
    val materialEvents =
      materialSummary.toList.flatMap { summary =>
        val promotions = promotionMoves(facts)
        val captureEvents =
          summary.captures.flatMap { capture =>
            val captureEvent =
              LineMoveEvent(
                kind = if capture.recapture then LineEventKind.Recapture else LineEventKind.Capture,
                moveUci = capture.moveUci,
                plyOffset = capture.plyOffset,
                side = Some(capture.side),
                pieceRole = Some(capture.attackerRole),
                targetRole = Some(capture.capturedRole),
                square = Some(capture.square)
              )
            captureEvent :: Option
              .when(capture.recapture)(
                LineMoveEvent(
                  kind = LineEventKind.DefenderMove,
                  moveUci = capture.moveUci,
                  plyOffset = capture.plyOffset,
                  side = Some(capture.side),
                  pieceRole = Some(capture.attackerRole),
                  targetRole = Some(capture.capturedRole),
                  square = Some(capture.square)
                )
              )
              .toList
          }
        val promotionEvents =
          Option.when(summary.hasPromotion)(
            LineMoveEvent(
              kind = LineEventKind.Promotion,
              moveUci = promotions.lastOption.map(_._2).getOrElse(summary.captures.lastOption.map(_.moveUci).getOrElse("")),
              plyOffset = promotions.lastOption.map(_._1).getOrElse(summary.captures.lastOption.map(_.plyOffset).getOrElse(0))
            )
          ).toList
        captureEvents ++ promotionEvents
      }
    val carriedDefense =
      facts.line.moves.headOption.toList.flatMap { root =>
        val rootMove = PrincipalVariationEvidence.normalizeUci(root.uci)
        val rootTo = Option.when(rootMove.length >= 4)(rootMove.slice(2, 4))
        val rootSide = replay.headOption.flatMap(step => positionAfter(step.fenBefore).map(_.color))
        val laterDefense = replayEvents.collectFirst {
          case event
              if event.kind == LineEventKind.DefenderMove &&
                event.plyOffset > 0 &&
                PrincipalVariationEvidence.normalizeUci(event.moveUci).take(2) == rootTo.getOrElse("") =>
            event
        }
        laterDefense.map(event =>
          event.copy(
            moveUci = rootMove,
            plyOffset = 0,
            side = rootSide.orElse(event.side),
            pieceRole = None,
            square = rootTo.map(EvidenceSquare.apply)
          )
        )
      }
    val passedPawnEvents = linePassedPawnEvents(startFen, facts)
    (replayEvents ++ roleEvents ++ forcedEvents ++ materialEvents ++ passedPawnEvents ++ carriedDefense).distinct

  private def linePassedPawnEvents(startFen: String, facts: PrincipalVariationEvidence.LineFacts): List[LineMoveEvent] =
    val eventMove = facts.line.moves.lastOption.map(move => PrincipalVariationEvidence.normalizeUci(move.uci))
    val plyOffset = facts.line.moves.size - 1
    val endPosition = facts.line.moves.lastOption.map(_.fenAfter).flatMap(positionAfter)
    val beforeAfter =
      for
        before <- positionAfter(startFen).toList
        after <- endPosition.toList
      yield (before, after)
    beforeAfter.flatMap { case (before, after) =>
      List(Color.White, Color.Black).flatMap { color =>
        val beforePassed = passedPawnSquares(before, color)
        val gained = passedPawnSquares(after, color).diff(beforePassed)
        eventMove.toList.flatMap(move =>
          gained.toList.sorted.map(square =>
            LineMoveEvent(
              kind = LineEventKind.PassedPawn,
              moveUci = move,
              plyOffset = plyOffset,
              side = Some(color),
              pieceRole = Some(EvidencePieceRole(Pawn.name)),
              targetRole = Some(EvidencePieceRole(Pawn.name)),
              square = Some(EvidenceSquare(square))
            )
          )
        )
      }
    }

  private def passedPawnSquares(position: Position, color: Color): Set[String] =
    PositionAnalyzer
      .passedPawns(color, position.board.byPiece(color, Pawn), position.board.byPiece(!color, Pawn))
      .map(_.key)
      .toSet

  private def lineConsequences(
      facts: PrincipalVariationEvidence.LineFacts,
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary]
  ): List[LineConsequence] =
    val outcome =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        positionAfter(move.fenAfter).toList.flatMap { position =>
          val prefix = facts.line.moves.take(index + 1).map(_.uci)
          List(
            Option.when(position.checkMate)(
              LineConsequence(LineConsequenceKind.Mate, prefix, proofSignal = true, eventMove = Some(normalized))
            ),
            Option.when(position.staleMate)(
              LineConsequence(
                LineConsequenceKind.DrawResource,
                prefix,
                proofSignal = true,
                eventMove = Some(normalized)
              )
            )
          ).flatten
        }
      }
    val forced =
      forcedTheme.toList.map(theme =>
        LineConsequence(
          kind =
            if theme.id == ForcedLineTruth.ImmediateReplyCheckId then LineConsequenceKind.ImmediateReplyCheck
            else LineConsequenceKind.ForcedTheme,
          lineMoves = theme.lineMoves,
          proofSignal = ForcedLineTruth.isProofSignalThemeId(theme.id),
          eventMove = theme.lineMoves.headOption
        )
      )
    val material =
      materialSummary.toList.flatMap { summary =>
        val promotionMoveUcis = promotionMoves(facts).map(_._2)
        val proofMoves = (summary.captures.map(_.moveUci) ++ promotionMoveUcis).distinct
        val promotionEventMove = promotionMoveUcis.lastOption
        List(
          Option.when(summary.hasProofSignalMaterialGain || summary.hasUnrecoveredPawnGainForMover)(
            LineConsequence(
              LineConsequenceKind.MaterialGain,
              proofMoves,
              proofSignal = summary.hasProofSignalMaterialGain,
              eventMove = promotionEventMove.filter(_ => summary.hasPromotionGainForMover)
            )
          ),
          Option.when(summary.hasProofSignalMaterialLoss || summary.hasUnrecoveredPawnLossForMover)(
            LineConsequence(
              LineConsequenceKind.MaterialLoss,
              proofMoves,
              proofSignal = summary.hasProofSignalMaterialLoss,
              eventMove = promotionEventMove.filter(_ => summary.hasPromotionLossForMover)
            )
          ),
          Option.when(summary.hasResolvedMaterialSequence)(
            LineConsequence(LineConsequenceKind.RecaptureSequence, summary.captures.map(_.moveUci), proofSignal = true)
          ),
          Option.when(summary.hasRecoveryWindow)(
            LineConsequence(LineConsequenceKind.RecoveryWindow, summary.captures.map(_.moveUci), proofSignal = true)
          ),
          Option.when(summary.hasSacrificeMaterialEvent)(
            LineConsequence(LineConsequenceKind.Sacrifice, summary.captures.map(_.moveUci), proofSignal = true)
          ),
          Option.when(summary.hasPromotion)(
            LineConsequence(LineConsequenceKind.PromotionRace, proofMoves, proofSignal = true, eventMove = promotionEventMove)
          )
        ).flatten
      }
    (outcome ++ forced ++ material).distinct

  private def promotionMoves(facts: PrincipalVariationEvidence.LineFacts): List[(Int, String)] =
    facts.line.moves.zipWithIndex.collect {
      case (move, index) if PrincipalVariationEvidence.normalizeUci(move.uci).length == 5 =>
        index -> PrincipalVariationEvidence.normalizeUci(move.uci)
    }

  private def endgameTechniqueConsequences(
      facts: PrincipalVariationEvidence.LineFacts,
      horizons: List[LineEndgameTechniqueHorizon]
  ): List[LineConsequence] =
    val lineMoves = facts.line.moves.map(move => PrincipalVariationEvidence.normalizeUci(move.uci))
    val rootMove = lineMoves.headOption
    horizons
      .filter(horizon =>
        LineEndgameTechniqueHorizon.defensivePattern(horizon.pattern) &&
          LineEndgameTechniqueHorizon.maintained(horizon.status)
      )
      .map(horizon =>
        LineConsequence(
          kind = LineConsequenceKind.DrawResource,
          lineMoves = lineMoves,
          proofSignal = true,
          eventMove = horizon.triggerMove.orElse(rootMove)
        )
      )
      .distinct

  private def endgameTechniqueHorizons(
      startFen: String,
      replay: List[LineReplayStep],
      consequences: List[LineConsequence]
  ): List[LineEndgameTechniqueHorizon] =
    val terminalKinds = consequences.filter(_.proofSignal).map(_.kind).distinct.sortBy(_.toString)
    EndgamePatternOracle.techniqueHorizons(startFen, replay, terminalKinds)

  private def castlingEvent(moveUci: String, plyOffset: Int): Option[LineMoveEvent] =
    castlingSide(moveUci).map(side =>
      LineMoveEvent(
        kind = LineEventKind.Castling,
        moveUci = moveUci,
        plyOffset = plyOffset,
        side = Some(side),
        pieceRole = Some(EvidencePieceRole(King.name)),
        square = destinationSquare(moveUci)
      )
    )

  private def castlingSide(moveUci: String): Option[Color] =
    moveUci match
      case "e1g1" | "e1c1" => Some(Color.White)
      case "e8g8" | "e8c8" => Some(Color.Black)
      case _               => None

  private def destinationSquare(moveUci: String): Option[EvidenceSquare] =
    Option.when(moveUci.length >= 4)(EvidenceSquare(moveUci.slice(2, 4)))

  private def positionAfter(fen: String): Option[Position] =
    Fen.read(Standard, Fen.Full(fen))
