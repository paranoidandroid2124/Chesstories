package lila.chessjudgment.analysis.line

import chess.{ Color, King, Pawn, Position }

import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence

object LineFactNormalizer:

  def fromValidatedLine(
      id: String,
      lineRef: LineNodeRef,
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      position: PositionNodeRef,
      scope: EvidenceScope,
      forcedTheme: Option[ForcedLineThemeEvidence] = None,
      materialSummary: Option[LineMaterialSummary] = None,
      predecessorReplay: Option[CanonicalLineReplay] = None,
      whitePovMate: Option[Int] = None,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val events = lineEvents(facts, replay, forcedTheme, materialSummary)
    val consequences = lineConsequences(facts, replay, forcedTheme, materialSummary, whitePovMate)
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
    val payload = LineFactEvidence.fromCertifiedReplay(
      line = lineRef,
      forcedTheme = forcedTheme,
      material = materialSummary,
      replay = replay,
      events = events,
      consequences = consequences,
      predecessorReplay = predecessorReplay
    )
    EvidenceRecord(
      ref = ref,
      payload = payload,
      parents = parents
    )

  def fromForcedTheme(theme: ForcedLineTruth.VerifiedTheme): ForcedLineThemeEvidence =
    ForcedLineThemeEvidence(
      id = theme.id,
      lineMoves = theme.lineMoves
    )

  private def lineEvents(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary]
  ): List[LineMoveEvent] =
    val replayEvents =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        val moverRole = replay.legalSteps.lift(index).map(step => EvidencePieceRole(step.move.piece.role.name))
        val checkDefense =
          replay.legalSteps.lift(index).toList.flatMap(step =>
            Option.when(step.before.check.yes)(step.before).toList.map { position =>
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
          replay.legalSteps.lift(index).toList.flatMap { step =>
            val position = step.after
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
    val forcedEvents =
      forcedTheme.toList.flatMap(theme =>
        theme.lineMoves.headOption.toList.flatMap(move =>
          List(
            LineMoveEvent(
              kind = LineEventKind.ForcedTheme,
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
        replay.legalSteps.lift(plyOffset).flatMap { step =>
          for
            square <- chess.Square.fromKey(moveUci.slice(2, 4))
          yield LineMoveEvent(
            kind = LineEventKind.Promotion,
            moveUci = moveUci,
            plyOffset = plyOffset,
            side = Some(step.move.piece.color),
            pieceRole = Some(EvidencePieceRole(Pawn.name)),
            targetRole = step.after.board.pieceAt(square).map(piece => EvidencePieceRole(piece.role.name)),
            square = Some(EvidenceSquare(square.key))
          )
        }.toList
      }
    val materialEvents = captureEvents ++ promotionEvents
    (replayEvents ++ forcedEvents ++ materialEvents).distinct

  private def lineConsequences(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary],
      whitePovMate: Option[Int]
  ): List[LineConsequence] =
    val rootMove = facts.line.moves.headOption.map(move => PrincipalVariationEvidence.normalizeUci(move.uci))
    val rootSide = replay.legalSteps.headOption.map(_.move.piece.color)
    val outcome =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        replay.legalSteps.lift(index).toList.flatMap { step =>
          val position = step.after
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
        val sacrificeOccurrences = offeredSacrificeOccurrences(replay, summary, rootMove)
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
        val materialGainRootMove =
          rootMove.filter(_ => materialGainEvent.exists(_.plyOffset == 0))
        val materialLossRootMove =
          rootMove.filter(_ => materialLossEvent.exists(_.plyOffset == 0))
        def proofMovesThrough(plyOffset: Int): List[String] =
          indexedProofMoves.takeWhile(_._1 <= plyOffset).map(_._2).distinct
        def proofMovesFrom(event: Option[MaterialOutcomeEvent]): List[String] =
          event
            .map(materialEvent => proofMovesThrough(materialEvent.plyOffset))
            .getOrElse(Nil)
        val materialGainProofMoves = proofMovesFrom(materialGainEvent)
        val materialLossProofMoves = proofMovesFrom(materialLossEvent)
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
        val sacrificeConsequences =
          sacrificeOccurrences.map { occurrence =>
            val acceptance = occurrence.acceptance
            LineConsequence(
              LineConsequenceKind.Sacrifice,
              replay.replaySteps.take(acceptance.plyOffset + 1).map(_.moveUci),
              proofSignal = true,
              eventMove = Some(acceptance.moveUci),
              rootMove = rootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(acceptance.side),
              sacrificeOccurrence = Some(occurrence)
            )
          }
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
        materialResultConsequences ++ recaptureConsequences ++ sacrificeConsequences ++ remainingMaterialConsequences
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
      replay: CanonicalLineReplay,
      summary: LineMaterialSummary
  ): List[MaterialOutcomeEvent] =
    replay.legalSteps.headOption.toList.flatMap { first =>
      val start = first.before
      val initialBalance = materialBalanceCp(start, summary.sideToMove)
      replay.legalSteps.zipWithIndex.flatMap { case (step, plyOffset) =>
        val moveUci = PrincipalVariationEvidence.normalizeUci(step.uci)
        for
          positionRef <- replay.replaySteps.lift(plyOffset).toList
          afterAnalysis <- replay.analysisAfter(positionRef).toList
          origin <- chess.Square.fromKey(moveUci.take(2)).toList
          destination <- chess.Square.fromKey(moveUci.slice(2, 4)).toList
          actor <- step.before.board.pieceAt(origin).toList
          beforeBalance = materialBalanceCp(step.before, summary.sideToMove) - initialBalance
          afterBalance = materialBalanceCp(step.after, summary.sideToMove) - initialBalance
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
            immediateAcceptanceBalancesForMover = afterAnalysis.actualLegalMoves
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

  private def offeredSacrificeOccurrences(
      replay: CanonicalLineReplay,
      summary: LineMaterialSummary,
      rootMove: Option[String]
  ): List[LineSacrificeOccurrence] =
    summary.exactCaptures.toList.flatMap { captures =>
      val rootCaptureResponses = rootMove.toList.flatMap(move =>
        summary.exactCaptureAt(0, move).toList.flatMap(summary.sacrificeResponsesFor)
      )
      val acceptedMaterialLosses =
        Option
          .when(summary.materialWindowComplete && summary.netCaptureCpForMover < 0)(
            captures.filter(capture => capture.side != summary.sideToMove && !capture.recapture)
          )
          .toList
          .flatten
      (acceptedMaterialLosses ++ rootCaptureResponses)
        .distinct
        .sortBy(capture => (
          capture.plyOffset,
          EvidenceRef.normalizeMove(capture.moveUci),
          capture.square.key.toLowerCase
        ))
        .flatMap(capture =>
          LineSacrificeOccurrence.fromCanonicalReplay(replay, summary.sideToMove, capture)
        )
    }

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

  private def mateResultMatches(whitePovMate: Option[Int], winner: Color): Boolean =
    whitePovMate.exists(mate => mate != 0 && (mate > 0) == winner.white)
