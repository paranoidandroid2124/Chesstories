package lila.chessjudgment.analysis.line

import chess.{ Color, King, Pawn }

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
        val relationDetails = replay.replaySteps.lift(index).toList.flatMap(step =>
          replay.verticalRelationOccurrences(
            step,
            List(
              VerticalRelationContractKind.RootCheckResponse,
              VerticalRelationContractKind.CreatedCheckResponseInventory,
              VerticalRelationContractKind.StalemateTransition
            )
          ).map(_.relation.detail)
        )
        val checkDefense = relationDetails.collect {
          case RelationWitnessDetail.RootCheckResponse(_, respondingSide, kingSquare, _, _, _) =>
              LineMoveEvent(
                kind = LineEventKind.CheckEvasion,
                moveUci = normalized,
                plyOffset = index,
                side = Some(respondingSide),
                pieceRole = moverRole,
                targetRole = Some(EvidencePieceRole(King.name)),
                square = Some(kingSquare)
              )
        }
        val stateEvents = relationDetails.flatMap {
          case RelationWitnessDetail.CreatedCheckResponseInventory(
                mover,
                _,
                kingSquare,
                _,
                _,
                _,
                terminal,
                _
              ) =>
            terminal match
              case RelationCheckTerminalState.Checkmate =>
                List(LineMoveEvent(
                  kind = LineEventKind.Mate,
                  moveUci = normalized,
                  plyOffset = index,
                  side = Some(mover.side),
                  pieceRole = moverRole,
                  targetRole = Some(EvidencePieceRole(King.name)),
                  square = Some(kingSquare)
                ))
              case RelationCheckTerminalState.Ongoing =>
                List(LineMoveEvent(
                  kind = LineEventKind.Check,
                  moveUci = normalized,
                  plyOffset = index,
                  side = Some(mover.side),
                  pieceRole = moverRole,
                  targetRole = Some(EvidencePieceRole(King.name)),
                  square = Some(kingSquare)
                ))
          case RelationWitnessDetail.StalemateTransition(mover, _, kingSquare, _) =>
            List(LineMoveEvent(
              kind = LineEventKind.Stalemate,
              moveUci = normalized,
              plyOffset = index,
              side = Some(mover.side),
              pieceRole = moverRole,
              targetRole = Some(EvidencePieceRole(King.name)),
              square = Some(kingSquare)
            ))
          case _ => Nil
        }
        val castling = replay.replaySteps.lift(index).flatMap(replay.transition).flatMap { transition =>
          Option.when(transition.legal.move.castle.nonEmpty)(
            LineMoveEvent(
              kind = LineEventKind.Castling,
              moveUci = normalized,
              plyOffset = index,
              side = Some(transition.relationDelta.rootMove.side),
              pieceRole = Some(EvidencePieceRole(King.name)),
              square = Some(transition.relationDelta.rootMove.to)
            )
          )
        }
        castling.toList ++ checkDefense ++ stateEvents
      }
    val forcedEvents =
      forcedTheme.toList.flatMap(theme =>
        theme.lineMoves.headOption.toList.flatMap(move =>
          List(LineMoveEvent(
            kind = LineEventKind.ForcedTheme,
            moveUci = move,
            plyOffset = 0
          ))
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
        captureEvent :: Nil
      }
    val promotionEvents =
      materialSummary.toList.flatMap(_.events).filter(_.promotionGainCp > 0).map { event =>
          LineMoveEvent(
            kind = LineEventKind.Promotion,
            moveUci = event.moveUci,
            plyOffset = event.plyOffset,
            side = Some(event.movement.side),
            pieceRole = Some(EvidencePieceRole(Pawn.name)),
            targetRole = Some(event.movement.afterRole),
            square = Some(event.movement.to)
          )
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
        val prefix = facts.line.moves.take(index + 1).map(_.uci)
        replay.replaySteps.lift(index).toList.flatMap { step =>
          replay.verticalRelationOccurrences(
            step,
            List(
              VerticalRelationContractKind.CreatedCheckResponseInventory,
              VerticalRelationContractKind.StalemateTransition
            )
          ).flatMap(_.relation.detail match
            case RelationWitnessDetail.CreatedCheckResponseInventory(
                  mover,
                  _,
                  _,
                  _,
                  _,
                  _,
                  RelationCheckTerminalState.Checkmate,
                  _
                ) if mateResultMatches(whitePovMate, mover.side) =>
              Some(
              LineConsequence(
                LineConsequenceKind.Mate,
                prefix,
                directCauseProjectionEligible = true,
                eventMove = Some(normalized),
                rootMove = rootMove,
                rootSide = rootSide,
                beneficiary = Some(mover.side)
              )
              )
            case RelationWitnessDetail.StalemateTransition(_, _, _, _) =>
              Some(
              LineConsequence(
                LineConsequenceKind.DrawResource,
                prefix,
                directCauseProjectionEligible = true,
                eventMove = Some(normalized),
                rootMove = rootMove,
                rootSide = rootSide
              )
              )
            case _ => None
          )
        }
      }
    val forced =
      forcedTheme.toList.map(theme =>
        LineConsequence(
          kind =
            if theme.id == ForcedLineTruth.ImmediateReplyCheckId then LineConsequenceKind.ImmediateReplyCheck
            else LineConsequenceKind.ForcedTheme,
          lineMoves = theme.lineMoves,
          directCauseProjectionEligible = false,
          eventMove = theme.lineMoves.headOption,
          rootMove = rootMove,
          rootSide = rootSide
        )
      )
    val material =
      materialSummary.toList.flatMap { summary =>
        val indexedPromotionMoves = summary.events
          .filter(_.promotionGainCp > 0)
          .map(event => event.plyOffset -> event.moveUci)
        val indexedProofMoves = (
          summary.captures.map(capture => capture.plyOffset -> capture.moveUci) ++ indexedPromotionMoves
        ).sortBy(_._1)
        val proofMoves = indexedProofMoves.map(_._2).distinct
        val sacrificeOccurrences = offeredSacrificeOccurrences(replay, summary, rootMove)
        val materialEvents = materialOutcomeEvents(summary)
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
          Option.when(summary.hasDirectCauseProjectionEligibleMaterialGain || summary.hasUnrecoveredPawnGainForMover)(
            LineConsequence(
              LineConsequenceKind.MaterialGain,
              materialGainProofMoves,
              directCauseProjectionEligible = summary.hasDirectCauseProjectionEligibleMaterialGain,
              eventMove = materialGainEventMove,
              rootMove = materialGainRootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(summary.sideToMove),
              materialOutcome = materialGainOutcome
            )
          ),
          Option.when(summary.hasDirectCauseProjectionEligibleMaterialLoss || summary.hasUnrecoveredPawnLossForMover)(
            LineConsequence(
              LineConsequenceKind.MaterialLoss,
              materialLossProofMoves,
              directCauseProjectionEligible = summary.hasDirectCauseProjectionEligibleMaterialLoss,
              eventMove = materialLossEventMove,
              rootMove = materialLossRootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(!summary.sideToMove),
              materialOutcome = materialLossOutcome
            )
          )
        ).flatten
        val recaptureConsequences =
          summary.captures.filter(_.recapture).map(capture =>
              LineConsequence(
                LineConsequenceKind.RecaptureSequence,
                proofMovesThrough(capture.plyOffset),
                directCauseProjectionEligible = true,
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
              directCauseProjectionEligible = true,
              eventMove = Some(acceptance.moveUci),
              rootMove = rootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(acceptance.side),
              sacrificeOccurrence = Some(occurrence)
            )
          }
        val recoveryConsequences = summary.durableRecoveryCaptureForMover.toList.map(capture =>
            LineConsequence(
              LineConsequenceKind.RecoveryWindow,
              proofMovesThrough(capture.plyOffset),
              directCauseProjectionEligible = true,
              eventMove = Some(capture.moveUci),
              rootMove = rootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(summary.sideToMove)
            )
          )
        val promotionEvents = materialEvents.filter(_.promotion)
        val promotionRace = promotionEvents.map(_.side).distinct.size > 1
        val promotionConsequences = promotionEvents.map(event =>
            LineConsequence(
              if promotionRace then LineConsequenceKind.PromotionRace
              else LineConsequenceKind.Promotion,
              proofMovesThrough(event.plyOffset),
              directCauseProjectionEligible = true,
              eventMove = Some(event.moveUci),
              rootMove = rootMove.filter(_ => event.plyOffset == 0),
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(event.side)
            )
          )
        materialResultConsequences ++ recaptureConsequences ++ sacrificeConsequences ++
          recoveryConsequences ++ promotionConsequences
      }
    (outcome ++ forced ++ material).distinct


  private final case class MaterialOutcomeEvent(
      moveUci: String,
      plyOffset: Int,
      side: Color,
      balanceAfterForMover: Int,
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
      summary: LineMaterialSummary
  ): List[MaterialOutcomeEvent] =
    summary.events.foldLeft((0, List.empty[MaterialOutcomeEvent])) {
      case ((runningBalance, events), observed) =>
        val captureDelta = observed.capture.map(_.valueCp).getOrElse(0)
        val unsignedDelta = captureDelta + observed.promotionGainCp
        val signedDelta =
          if observed.movement.side == summary.sideToMove then unsignedDelta else -unsignedDelta
        val afterBalance = runningBalance + signedDelta
        val event = Option.when(signedDelta != 0)(
          MaterialOutcomeEvent(
            moveUci = observed.moveUci,
            plyOffset = observed.plyOffset,
            side = observed.movement.side,
            balanceAfterForMover = afterBalance,
            promotion = observed.promotionGainCp > 0,
            recapture = observed.capture.exists(_.recapture),
            capture = observed.capture
          )
        )
        afterBalance -> (events ++ event)
    }._2

  private def lastingMaterialOutcomeFor(
      events: List[MaterialOutcomeEvent],
      summary: LineMaterialSummary
  ): Option[LastingMaterialOutcome] =
    summary.closedNetCpForMover.flatMap { closedNet =>
      val finalSign = Integer.signum(closedNet)
      val observedNet = events.lastOption.map(_.balanceAfterForMover).getOrElse(0)
      Option.when(finalSign != 0 && observedNet == closedNet) {
        val beneficiary = if finalSign > 0 then summary.sideToMove else !summary.sideToMove
        def keepsFinalSign(balance: Int): Boolean = finalSign * balance > 0
        events.zipWithIndex.collectFirst {
          case (event, index)
              if event.side == beneficiary &&
                keepsFinalSign(event.balanceAfterForMover) &&
                events.drop(index).forall(next => keepsFinalSign(next.balanceAfterForMover)) &&
                !(event.plyOffset == 0 && event.recapture && !event.promotion) =>
            events
              .drop(index)
              .map(next => finalSign * next.balanceAfterForMover)
              .minOption
              .filter(_ > 0)
              .map(durableNetCp => LastingMaterialOutcome(event, durableNetCp))
        }.flatten
      }.flatten
    }

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
          .when(summary.closedNetCpForMover.exists(_ < 0))(
            captures.filter(capture =>
              capture.side != summary.sideToMove && capture.recaptureExcluded
            )
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

  private def mateResultMatches(whitePovMate: Option[Int], winner: Color): Boolean =
    whitePovMate.exists(mate => mate != 0 && (mate > 0) == winner.white)
