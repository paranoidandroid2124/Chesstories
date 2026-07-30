package lila.chessjudgment.analysis.line

import chess.{ Color, King, Pawn, Position }
import chess.format.Fen
import chess.variant.Standard

import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.model.position.PawnTopology
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence

object LineFactNormalizer:

  def fromValidatedLine(
      id: String,
      lineRef: LineNodeRef,
      facts: PrincipalVariationEvidence.LineFacts,
      position: PositionNodeRef,
      scope: EvidenceScope,
      forcedTheme: Option[ForcedLineThemeEvidence] = None,
      materialSummary: Option[LineMaterialSummary] = None,
      whitePovMate: Option[Int] = None,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val replay = replaySteps(position.fen, facts)
    val events = lineEvents(lineRef, facts, replay, forcedTheme, materialSummary)
    val baseConsequences = lineConsequences(facts, replay, forcedTheme, materialSummary, whitePovMate)
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
        forcedTheme = forcedTheme,
        material = materialSummary,
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
      lineRef: LineNodeRef,
      facts: PrincipalVariationEvidence.LineFacts,
      replay: List[LineReplayStep],
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary]
  ): List[LineMoveEvent] =
    val replayEvents =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        val moverRole = replay.lift(index).flatMap(step => movingPieceRole(step.fenBefore, normalized))
        val checkDefense =
          replay.lift(index).toList.flatMap(step =>
            positionAfter(step.fenBefore).toList.filter(_.check.yes).map { position =>
              LineMoveEvent(
                kind = LineEventKind.DefenderMove,
                moveUci = normalized,
                plyOffset = index,
                side = Some(position.color),
                pieceRole = moverRole,
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
                    pieceRole = moverRole,
                    targetRole = Some(EvidencePieceRole(King.name)),
                    square = kingSquare
                  ),
                  LineMoveEvent(
                    kind = LineEventKind.Tempo,
                    moveUci = normalized,
                    plyOffset = index,
                    side = Some(!position.color),
                    pieceRole = moverRole,
                    targetRole = Some(EvidencePieceRole(King.name)),
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
                      pieceRole = moverRole,
                      targetRole = Some(EvidencePieceRole(King.name)),
                      square = kingSquare
                    ),
                    LineMoveEvent(
                      kind = LineEventKind.Tempo,
                      moveUci = normalized,
                      plyOffset = index,
                      side = Some(!position.color),
                      pieceRole = moverRole,
                      targetRole = Some(EvidencePieceRole(King.name)),
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
                  pieceRole = moverRole,
                  targetRole = Some(EvidencePieceRole(King.name)),
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
    val captureEvents =
      materialSummary.toList.flatMap(_.captures).flatMap { capture =>
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
      promotionMoves(facts).flatMap { case (plyOffset, moveUci) =>
        replay.lift(plyOffset).flatMap { step =>
          for
            side <- movingPieceColor(step.fenBefore, moveUci)
            square <- chess.Square.fromKey(moveUci.slice(2, 4))
          yield LineMoveEvent(
            kind = LineEventKind.Promotion,
            moveUci = moveUci,
            plyOffset = plyOffset,
            side = Some(side),
            pieceRole = Some(EvidencePieceRole(Pawn.name)),
            targetRole = positionAfter(step.fenAfter).flatMap(_.board.pieceAt(square)).map(piece => EvidencePieceRole(piece.role.name)),
            square = Some(EvidenceSquare(square.key))
          )
        }.toList
      }
    val materialEvents = captureEvents ++ promotionEvents
    val carriedDefense =
      facts.line.moves.headOption.toList.flatMap { root =>
        val rootMove = PrincipalVariationEvidence.normalizeUci(root.uci)
        val rootTo = Option.when(rootMove.length >= 4)(rootMove.slice(2, 4))
        val rootSide = replay.headOption.flatMap(step => positionAfter(step.fenBefore).map(_.color))
        val rootPieceRole = replay.headOption.flatMap(step => movingPieceRole(step.fenBefore, rootMove))
        val laterDefense = (replayEvents ++ materialEvents).collectFirst {
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
            pieceRole = rootPieceRole.orElse(event.pieceRole)
          )
        )
      }
    val passedPawnEvents = linePassedPawnEvents(replay)
    (replayEvents ++ roleEvents ++ forcedEvents ++ materialEvents ++ passedPawnEvents ++ carriedDefense).distinct

  private def linePassedPawnEvents(replay: List[LineReplayStep]): List[LineMoveEvent] =
    replay.zipWithIndex.flatMap { case (step, index) =>
      val beforeAfter =
        for
          before <- positionAfter(step.fenBefore).toList
          after <- positionAfter(step.fenAfter).toList
        yield (before, after)
      beforeAfter.flatMap { case (before, after) =>
        val move = PrincipalVariationEvidence.normalizeUci(step.moveUci)
        val moverRole = movingPieceRole(step.fenBefore, move)
        List(Color.White, Color.Black).flatMap { color =>
          val beforePassed = passedPawnSquares(before, color)
          val gained = passedPawnSquares(after, color).diff(beforePassed)
          gained.toList.sorted.map(square =>
            LineMoveEvent(
              kind = LineEventKind.PassedPawn,
              moveUci = move,
              plyOffset = index,
              side = Some(color),
              pieceRole = moverRole,
              targetRole = Some(EvidencePieceRole(Pawn.name)),
              square = Some(EvidenceSquare(square))
            )
          )
        }
      }
    }

  private def passedPawnSquares(position: Position, color: Color): Set[String] =
    PawnTopology
      .passedPawns(color, position.board.byPiece(color, Pawn), position.board.byPiece(!color, Pawn))
      .map(_.key)
      .toSet

  private def lineConsequences(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: List[LineReplayStep],
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary],
      whitePovMate: Option[Int]
  ): List[LineConsequence] =
    val rootMove = facts.line.moves.headOption.map(move => PrincipalVariationEvidence.normalizeUci(move.uci))
    val rootSide = replay.headOption.flatMap(step => movingPieceColor(step.fenBefore, step.moveUci))
    val outcome =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        positionAfter(move.fenAfter).toList.flatMap { position =>
          val prefix = facts.line.moves.take(index + 1).map(_.uci)
          List(
            Option.when(position.checkMate && mateResultMatches(whitePovMate, !position.color))(
              LineConsequence(
                LineConsequenceKind.Mate,
                prefix,
                proofSignal = true,
                eventMove = Some(normalized),
                rootMove = rootMove,
                rootSide = rootSide,
                beneficiary = Some(!position.color)
              )
            ),
            Option.when(position.staleMate)(
              LineConsequence(
                LineConsequenceKind.DrawResource,
                prefix,
                proofSignal = true,
                eventMove = Some(normalized),
                rootMove = rootMove,
                rootSide = rootSide
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
          proofSignal = false,
          eventMove = theme.lineMoves.headOption,
          rootMove = rootMove,
          rootSide = rootSide
        )
      )
    val material =
      materialSummary.toList.flatMap { summary =>
        val indexedPromotionMoves = promotionMoves(facts)
        val indexedProofMoves = (
          summary.captures.map(capture => capture.plyOffset -> capture.moveUci) ++ indexedPromotionMoves
        ).sortBy(_._1)
        val proofMoves = indexedProofMoves.map(_._2).distinct
        val rootCaptureAcceptance = rootCaptureAcceptanceEvidence(replay, summary, rootMove)
        val rootCaptureSacrificeOfferMove =
          rootCaptureAcceptance.filter(_.sacrificeOffer).map(_.moveUci)
        val offeredSacrifices = offeredSacrificeEvidence(replay, summary, rootMove)
        val rootMovedOffer = rootMove.flatMap(root =>
          offeredSacrifices.collectFirst {
            case (Some(offerMove), capture) if EvidenceRef.sameMove(offerMove, root) => offerMove -> capture
          }
        )
        val materialEvents = materialOutcomeEvents(replay, summary)
        val lastingMaterialOutcome = lastingMaterialOutcomeFor(materialEvents, summary)
        val materialOutcomeEvent = lastingMaterialOutcome.map(_.event)
        val materialGainEvent = materialOutcomeEvent.filter(_.side == summary.sideToMove)
        val materialLossEvent = materialOutcomeEvent.filter(_.side != summary.sideToMove)
        val materialGainOutcome = lastingMaterialOutcome
          .filter(_.event.side == summary.sideToMove)
          .flatMap(_.toRootOwnedOutcome)
        val materialLossOutcome = lastingMaterialOutcome
          .filter(_.event.side != summary.sideToMove)
          .flatMap(_.toRootOwnedOutcome)
        val promotionEvent = Option
          .when(
            (summary.hasPromotionGainForMover && summary.netCaptureCpForMover > 0) ||
              (summary.hasPromotionLossForMover && summary.netCaptureCpForMover < 0)
          )(beneficiaryPromotionEvent(materialEvents, summary))
          .flatten
        val materialGainEventMove = materialGainEvent.map(_.moveUci)
        val materialLossEventMove = materialLossEvent.map(_.moveUci)
        val selectedRootMovedOffer = rootMovedOffer.filter { case (_, capture) =>
          materialLossEvent.exists(event =>
            event.plyOffset == capture.plyOffset && EvidenceRef.sameMove(event.moveUci, capture.moveUci)
          )
        }
        val materialGainRootMove =
          rootMove.filter(_ => materialGainEvent.exists(_.plyOffset == 0))
        val materialLossRootMove =
          rootMove.filter(_ => materialLossEvent.exists(_.plyOffset == 0) || selectedRootMovedOffer.nonEmpty)
        def proofMovesThrough(plyOffset: Int): List[String] =
          indexedProofMoves.takeWhile(_._1 <= plyOffset).map(_._2).distinct
        def proofMovesFrom(event: Option[MaterialOutcomeEvent]): List[String] =
          event
            .map(materialEvent => proofMovesThrough(materialEvent.plyOffset))
            .getOrElse(Nil)
        val materialGainProofMoves = proofMovesFrom(materialGainEvent)
        val materialLossProofMoves = materialLossEvent
          .map(event => (selectedRootMovedOffer.map(_._1).toList ++ proofMovesFrom(Some(event))).distinct)
          .getOrElse(Nil)
        val materialResultConsequences = List(
          Option.when(summary.hasProofSignalMaterialGain || summary.hasUnrecoveredPawnGainForMover)(
            LineConsequence(
              LineConsequenceKind.MaterialGain,
              materialGainProofMoves,
              proofSignal = summary.hasProofSignalMaterialGain,
              eventMove = materialGainEventMove,
              rootMove = materialGainRootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(summary.sideToMove),
              materialOutcome = materialGainOutcome
            )
          ),
          Option.when(summary.hasProofSignalMaterialLoss || summary.hasUnrecoveredPawnLossForMover)(
            LineConsequence(
              LineConsequenceKind.MaterialLoss,
              materialLossProofMoves,
              proofSignal = summary.hasProofSignalMaterialLoss,
              eventMove = materialLossEventMove,
              rootMove = materialLossRootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(!summary.sideToMove),
              materialOutcome = materialLossOutcome
            )
          )
        ).flatten
        val recaptureConsequences =
          Option
            .when(summary.materialWindowComplete)(summary.captures.filter(_.recapture))
            .toList
            .flatten
            .map(capture =>
              LineConsequence(
                LineConsequenceKind.RecaptureSequence,
                proofMovesThrough(capture.plyOffset),
                proofSignal = true,
                eventMove = Some(capture.moveUci),
                rootMove = rootMove,
                rootSide = Some(summary.sideToMove),
                beneficiary = Some(capture.side)
              )
            )
        val remainingMaterialConsequences = List(
          summary.durableRecoveryCaptureForMover.map(capture =>
            LineConsequence(
              LineConsequenceKind.RecoveryWindow,
              proofMovesThrough(capture.plyOffset),
              proofSignal = true,
              eventMove = Some(capture.moveUci),
              rootMove = rootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(summary.sideToMove)
            )
          ),
          Option.when(summary.hasSacrificeMaterialEventFor(rootMove) || rootCaptureSacrificeOfferMove.nonEmpty)(
            LineConsequence(
              LineConsequenceKind.Sacrifice,
              summary.captures.map(_.moveUci),
              proofSignal = true,
              eventMove = rootCaptureSacrificeOfferMove.orElse(offeredSacrifices.flatMap(_._1).headOption),
              rootMove = rootMove,
              rootSide = Some(summary.sideToMove),
              stationarySacrificeCaptures = offeredSacrifices.collect { case (None, capture) => capture }
            )
          ),
          promotionEvent.map(event =>
            LineConsequence(
              if materialEvents.filter(_.promotion).map(_.side).distinct.size > 1 then LineConsequenceKind.PromotionRace
              else LineConsequenceKind.Promotion,
              proofMoves,
              proofSignal = true,
              eventMove = Some(event.moveUci),
              rootMove = rootMove.filter(_ => event.plyOffset == 0),
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(event.side)
            )
          )
        ).flatten
        materialResultConsequences ++ recaptureConsequences ++ remainingMaterialConsequences
      }
    (outcome ++ forced ++ material).distinct

  private def promotionMoves(facts: PrincipalVariationEvidence.LineFacts): List[(Int, String)] =
    facts.line.moves.zipWithIndex.collect {
      case (move, index) if PrincipalVariationEvidence.normalizeUci(move.uci).length == 5 =>
        index -> PrincipalVariationEvidence.normalizeUci(move.uci)
    }

  private final case class MaterialOutcomeEvent(
      moveUci: String,
      plyOffset: Int,
      side: Color,
      balanceAfterForMover: Int,
      immediateAcceptanceBalancesForMover: List[Int],
      promotion: Boolean,
      recapture: Boolean,
      capture: Option[LineMaterialCapture]
  )

  private final case class LastingMaterialOutcome(
      event: MaterialOutcomeEvent,
      durableNetCp: Int
  ):
    def toRootOwnedOutcome: Option[RootOwnedMaterialOutcome] =
      for
        capture <- event.capture
        if capture.side == event.side
        salience <- RootOwnedMaterialEventSalience.from(capture)
      yield RootOwnedMaterialOutcome(
        event = salience,
        beneficiary = event.side,
        durableNetCp = durableNetCp
      )

  private def materialOutcomeEvents(
      replay: List[LineReplayStep],
      summary: LineMaterialSummary
  ): List[MaterialOutcomeEvent] =
    replay.headOption.flatMap(step => positionAfter(step.fenBefore)).toList.flatMap { start =>
      val initialBalance = materialBalanceCp(start, summary.sideToMove)
      replay.zipWithIndex.flatMap { case (step, plyOffset) =>
        val moveUci = PrincipalVariationEvidence.normalizeUci(step.moveUci)
        for
          before <- positionAfter(step.fenBefore).toList
          after <- positionAfter(step.fenAfter).toList
          origin <- chess.Square.fromKey(moveUci.take(2)).toList
          destination <- chess.Square.fromKey(moveUci.slice(2, 4)).toList
          actor <- before.board.pieceAt(origin).toList
          beforeBalance = materialBalanceCp(before, summary.sideToMove) - initialBalance
          afterBalance = materialBalanceCp(after, summary.sideToMove) - initialBalance
          signedDelta = afterBalance - beforeBalance
          if signedDelta != 0
          if (actor.color == summary.sideToMove) == (signedDelta > 0)
        yield
          val exactCaptures = summary.captures.filter(capture =>
            capture.plyOffset == plyOffset && EvidenceRef.sameMove(capture.moveUci, moveUci)
          )
          MaterialOutcomeEvent(
            moveUci = moveUci,
            plyOffset = plyOffset,
            side = actor.color,
            balanceAfterForMover = afterBalance,
            immediateAcceptanceBalancesForMover = after.legalMoves
              .filter(reply => reply.captures && reply.dest == destination)
              .map(reply => materialBalanceCp(reply.after, summary.sideToMove) - initialBalance),
            promotion = moveUci.length == 5,
            recapture = exactCaptures.exists(_.recapture),
            capture = exactCaptures match
              case capture :: Nil => Some(capture)
              case _              => None
          )
      }
    }

  private def lastingMaterialOutcomeFor(
      events: List[MaterialOutcomeEvent],
      summary: LineMaterialSummary
  ): Option[LastingMaterialOutcome] =
    val finalSign = Integer.signum(summary.netCaptureCpForMover)
    val observedNet = events.lastOption.map(_.balanceAfterForMover).getOrElse(0)
    Option.when(
      summary.materialWindowComplete &&
        finalSign != 0 &&
        observedNet == summary.netCaptureCpForMover
    ) {
      val beneficiary = if finalSign > 0 then summary.sideToMove else !summary.sideToMove
      def keepsFinalSign(balance: Int): Boolean = finalSign * balance > 0
      events.zipWithIndex.collectFirst {
        case (event, index)
            if event.side == beneficiary &&
              keepsFinalSign(event.balanceAfterForMover) &&
              events.drop(index).forall(next => keepsFinalSign(next.balanceAfterForMover)) &&
              event.immediateAcceptanceBalancesForMover.forall(keepsFinalSign) &&
              !(event.plyOffset == 0 && event.recapture && !event.promotion) =>
          val beneficiaryBalances =
            (events.drop(index).map(_.balanceAfterForMover) ++
              event.immediateAcceptanceBalancesForMover).map(finalSign * _)
          beneficiaryBalances.minOption
            .filter(_ > 0)
            .map(durableNetCp => LastingMaterialOutcome(event, durableNetCp))
      }
    }.flatten.flatten

  private def beneficiaryPromotionEvent(
      events: List[MaterialOutcomeEvent],
      summary: LineMaterialSummary
  ): Option[MaterialOutcomeEvent] =
    val finalSign = Integer.signum(summary.netCaptureCpForMover)
    val observedNet = events.lastOption.map(_.balanceAfterForMover).getOrElse(0)
    Option.when(finalSign != 0 && observedNet == summary.netCaptureCpForMover) {
      val beneficiary = if finalSign > 0 then summary.sideToMove else !summary.sideToMove
      events.reverse.find(event =>
        event.promotion &&
          event.side == beneficiary &&
          event.immediateAcceptanceBalancesForMover.forall(balance => finalSign * balance > 0)
      )
    }.flatten

  private def materialBalanceCp(position: Position, side: Color): Int =
    MaterialValue.sideMaterialCp(position.board, side) - MaterialValue.sideMaterialCp(position.board, !side)

  private final case class RootCaptureAcceptance(
      moveUci: String,
      capturedValueCp: Int,
      actorValueCp: Int,
      legallyAcceptable: Boolean
  ):
    def sacrificeOffer: Boolean =
      legallyAcceptable && actorValueCp > capturedValueCp

  private def rootCaptureAcceptanceEvidence(
      replay: List[LineReplayStep],
      summary: LineMaterialSummary,
      rootMove: Option[String]
  ): Option[RootCaptureAcceptance] =
    (for
      move <- rootMove
      capture <- summary.captureForMove(move).filter(_.plyOffset == 0)
      rootStep <- replay.headOption.filter(step => EvidenceRef.sameMove(step.moveUci, move))
      origin <- chess.Square.fromKey(PrincipalVariationEvidence.normalizeUci(move).take(2))
      destination <- chess.Square.fromKey(PrincipalVariationEvidence.normalizeUci(move).slice(2, 4))
      before <- positionAfter(rootStep.fenBefore)
      actor <- before.board.pieceAt(origin)
      after <- positionAfter(rootStep.fenAfter)
    yield
      val acceptingCaptures = after.legalMoves.filter(reply => reply.captures && reply.dest == destination)
      RootCaptureAcceptance(
        moveUci = PrincipalVariationEvidence.normalizeUci(move),
        capturedValueCp = capture.valueCp,
        actorValueCp = MaterialValue.materialValueCp(actor.role),
        legallyAcceptable = acceptingCaptures.nonEmpty
      )
    )

  private def offeredSacrificeEvidence(
      replay: List[LineReplayStep],
      summary: LineMaterialSummary,
      rootMove: Option[String]
  ): List[(Option[String], LineMaterialCapture)] =
    val rootCaptureSacrificeResponse =
      for
        move <- rootMove
        capture <- summary.captureForMove(move)
        response <- summary.sacrificeResponseFor(capture)
      yield response
    summary.captures
      .filter(capture =>
        capture.side != summary.sideToMove &&
          (!capture.recapture || rootCaptureSacrificeResponse.contains(capture))
      )
      .sortBy(_.plyOffset)
      .flatMap { capture =>
        replay.lift(capture.plyOffset).toList.flatMap { captureStep =>
          val movedOffer = replay.zipWithIndex.take(capture.plyOffset).reverse.flatMap { case (offerStep, _) =>
            val move = PrincipalVariationEvidence.normalizeUci(offerStep.moveUci)
            (for
              origin <- chess.Square.fromKey(move.take(2))
              destination <- chess.Square.fromKey(move.slice(2, 4))
              if destination.key == capture.square.key
              beforeOffer <- positionAfter(offerStep.fenBefore)
              offeredPiece <- beforeOffer.board.pieceAt(origin)
              if offeredPiece.color == summary.sideToMove
              if capture.capturedRole.name.equalsIgnoreCase(offeredPiece.role.toString)
              afterOffer <- positionAfter(offerStep.fenAfter)
              if afterOffer.board.pieceAt(destination).contains(offeredPiece)
              beforeCapture <- positionAfter(captureStep.fenBefore)
              afterCapture <- positionAfter(captureStep.fenAfter)
              if beforeCapture.board.pieceAt(destination).contains(offeredPiece)
              if !afterCapture.board.pieceAt(destination).contains(offeredPiece)
            yield move).toList
          }.headOption
          movedOffer
            .map(move => Some(move) -> capture)
            .orElse(Option.when(stationarySacrifice(replay, capture))(None -> capture))
        }
      }
      .distinctBy(_._2)

  private def stationarySacrifice(
      replay: List[LineReplayStep],
      capture: LineMaterialCapture
  ): Boolean =
    (for
      rootStep <- replay.headOption
      captureStep <- replay.lift(capture.plyOffset)
      square <- chess.Square.fromKey(capture.square.key)
      beforeRoot <- positionAfter(rootStep.fenBefore)
      offeredPiece <- beforeRoot.board.pieceAt(square)
      if offeredPiece.color != capture.side
      if capture.capturedRole.name.equalsIgnoreCase(offeredPiece.role.toString)
      if replay.take(capture.plyOffset).forall { step =>
        val move = PrincipalVariationEvidence.normalizeUci(step.moveUci)
        move.take(2) != square.key && move.slice(2, 4) != square.key
      }
      beforeCapture <- positionAfter(captureStep.fenBefore)
      afterCapture <- positionAfter(captureStep.fenAfter)
      if beforeCapture.board.pieceAt(square).contains(offeredPiece)
      if !afterCapture.board.pieceAt(square).contains(offeredPiece)
    yield true).getOrElse(false)

  private def endgameTechniqueConsequences(
      facts: PrincipalVariationEvidence.LineFacts,
      horizons: List[LineEndgameTechniqueHorizon]
  ): List[LineConsequence] =
    val lineMoves = facts.line.moves.map(move => PrincipalVariationEvidence.normalizeUci(move.uci))
    horizons
      .filter(horizon =>
        LineEndgameTechniqueHorizon.defensivePattern(horizon.pattern) &&
          LineEndgameTechniqueHorizon.maintained(horizon.status) &&
          horizon.triggerMove.nonEmpty
      )
      .map(horizon =>
        LineConsequence(
          kind = LineConsequenceKind.DrawResource,
          lineMoves = lineMoves,
          proofSignal = false,
          eventMove = horizon.triggerMove,
          rootMove = lineMoves.headOption
        )
      )
      .distinct

  private def endgameTechniqueHorizons(
      startFen: String,
      replay: List[LineReplayStep],
      consequences: List[LineConsequence]
  ): List[LineEndgameTechniqueHorizon] =
    EndgamePatternOracle.techniqueHorizons(startFen, replay, consequences.filter(_.proofSignal))

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

  private[chessjudgment] def castlingSide(moveUci: String): Option[Color] =
    moveUci match
      case "e1g1" | "e1h1" | "e1c1" | "e1a1" => Some(Color.White)
      case "e8g8" | "e8h8" | "e8c8" | "e8a8" => Some(Color.Black)
      case _               => None

  private def destinationSquare(moveUci: String): Option[EvidenceSquare] =
    moveUci match
      case "e1h1" => Some(EvidenceSquare("g1"))
      case "e1a1" => Some(EvidenceSquare("c1"))
      case "e8h8" => Some(EvidenceSquare("g8"))
      case "e8a8" => Some(EvidenceSquare("c8"))
      case _      => Option.when(moveUci.length >= 4)(EvidenceSquare(moveUci.slice(2, 4)))

  private def movingPieceRole(fenBefore: String, moveUci: String): Option[EvidencePieceRole] =
    for
      position <- positionAfter(fenBefore)
      from <- _root_.chess.Square.all.find(_.key == moveUci.take(2))
      piece <- position.board.pieceAt(from)
    yield EvidencePieceRole(piece.role.name)

  private def movingPieceColor(fenBefore: String, moveUci: String): Option[Color] =
    for
      position <- positionAfter(fenBefore)
      from <- _root_.chess.Square.all.find(_.key == moveUci.take(2))
      piece <- position.board.pieceAt(from)
    yield piece.color

  private def mateResultMatches(whitePovMate: Option[Int], winner: Color): Boolean =
    whitePovMate.exists(mate => mate != 0 && (mate > 0) == winner.white)

  private def positionAfter(fen: String): Option[Position] =
    Fen.read(Standard, Fen.Full(fen))
