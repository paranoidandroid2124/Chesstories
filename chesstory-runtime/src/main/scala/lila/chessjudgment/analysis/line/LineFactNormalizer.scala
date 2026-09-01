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
      materialSummary: Option[LineMaterialSummary] = None,
      predecessorReplay: Option[CanonicalLineReplay] = None,
      whitePovMate: Option[Int] = None,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val events = lineEvents(facts, replay, materialSummary)
    val consequences = lineConsequences(facts, replay, events, materialSummary, whitePovMate)
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

  private def lineEvents(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
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
          Option.when(transition.relationDelta.rootMove.isCastling)(
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
    (replayEvents ++ materialEvents).distinct

  private def lineConsequences(
      facts: PrincipalVariationEvidence.LineFacts,
      replay: CanonicalLineReplay,
      events: List[LineMoveEvent],
      materialSummary: Option[LineMaterialSummary],
      whitePovMate: Option[Int]
  ): List[LineConsequence] =
    val rootMove = facts.line.moves.headOption.map(move => PrincipalVariationEvidence.normalizeUci(move.uci))
    val rootSide = replay.legalSteps.headOption.map(_.move.piece.color)
    val outcome =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
        val prefix = facts.line.moves.take(index + 1).zipWithIndex.map { case (proofMove, plyOffset) =>
          LineMoveOccurrence(PrincipalVariationEvidence.normalizeUci(proofMove.uci), plyOffset)
        }
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
                eventOccurrence = Some(LineMoveOccurrence(normalized, index)),
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
                eventOccurrence = Some(LineMoveOccurrence(normalized, index)),
                rootMove = rootMove,
                rootSide = rootSide
              )
              )
            case _ => None
          )
        }
      }
    val immediateReplyChecks =
      for
        normalizedRoot <- rootMove.toList
        replyCheck <- events.filter(event => event.kind == LineEventKind.Check && event.plyOffset == 1)
        rootStep <- replay.replaySteps.headOption.toList
        replyStep <- replay.replaySteps.lift(1).toList
        if EvidenceRef.sameMove(rootStep.moveUci, normalizedRoot)
        if EvidenceRef.sameMove(replyStep.moveUci, replyCheck.moveUci)
        checker <- replyCheck.side.toList
        if rootSide.exists(_ != checker)
      yield LineConsequence(
        kind = LineConsequenceKind.ImmediateReplyCheck,
        proofOccurrences = List(
          LineMoveOccurrence(EvidenceRef.normalizeMove(rootStep.moveUci), 0),
          LineMoveOccurrence(EvidenceRef.normalizeMove(replyStep.moveUci), 1)
        ),
        eventOccurrence = Some(LineMoveOccurrence(EvidenceRef.normalizeMove(replyStep.moveUci), 1)),
        rootMove = Some(normalizedRoot),
        rootSide = rootSide,
        beneficiary = Some(checker)
      )
    val material =
      materialSummary.toList.flatMap { summary =>
        val indexedProofOccurrences = summary.events
          .filter(event => event.capture.nonEmpty || event.promotionGainCp > 0)
          .map(event =>
            LineMoveOccurrence(EvidenceRef.normalizeMove(event.moveUci), event.plyOffset)
          )
        val materialEvents = materialOutcomeEvents(summary)
        val materialOutcomeEvent = lastingMaterialOutcomeFor(materialEvents, summary)
        val materialGainEvent = materialOutcomeEvent.filter(_.side == summary.sideToMove)
        val materialLossEvent = materialOutcomeEvent.filter(_.side != summary.sideToMove)
        val materialGainEventOccurrence = materialGainEvent.map(event =>
          LineMoveOccurrence(EvidenceRef.normalizeMove(event.moveUci), event.plyOffset)
        )
        val materialLossEventOccurrence = materialLossEvent.map(event =>
          LineMoveOccurrence(EvidenceRef.normalizeMove(event.moveUci), event.plyOffset)
        )
        val materialGainRootMove =
          rootMove.filter(_ => materialGainEvent.exists(_.plyOffset == 0))
        val materialLossRootMove =
          rootMove.filter(_ => materialLossEvent.exists(_.plyOffset == 0))
        def proofOccurrencesThrough(plyOffset: Int): List[LineMoveOccurrence] =
          indexedProofOccurrences.takeWhile(_.plyOffset <= plyOffset)
        def proofOccurrencesFrom(event: Option[MaterialOutcomeEvent]): List[LineMoveOccurrence] =
          event
            .map(materialEvent => proofOccurrencesThrough(materialEvent.plyOffset))
            .getOrElse(Nil)
        val materialGainProofOccurrences = proofOccurrencesFrom(materialGainEvent)
        val materialLossProofOccurrences = proofOccurrencesFrom(materialLossEvent)
        val materialResultConsequences = List(
          materialGainEvent.map(_ =>
            LineConsequence(
              LineConsequenceKind.MaterialGain,
              materialGainProofOccurrences,
              eventOccurrence = materialGainEventOccurrence,
              rootMove = materialGainRootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(summary.sideToMove)
            )
          ),
          materialLossEvent.map(_ =>
            LineConsequence(
              LineConsequenceKind.MaterialLoss,
              materialLossProofOccurrences,
              eventOccurrence = materialLossEventOccurrence,
              rootMove = materialLossRootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(!summary.sideToMove)
            )
          )
        ).flatten
        val recaptureConsequences =
          summary.captures.filter(_.recapture).map(capture =>
              LineConsequence(
                LineConsequenceKind.RecaptureSequence,
                proofOccurrencesThrough(capture.plyOffset),
                eventOccurrence = Some(LineMoveOccurrence(EvidenceRef.normalizeMove(capture.moveUci), capture.plyOffset)),
                rootMove = rootMove,
                rootSide = Some(summary.sideToMove),
                beneficiary = Some(capture.side)
              )
          )
        val recoveryConsequences = summary.durableRecoveryCaptureForMover.toList.map(capture =>
            LineConsequence(
              LineConsequenceKind.RecoveryWindow,
              proofOccurrencesThrough(capture.plyOffset),
              eventOccurrence = Some(LineMoveOccurrence(EvidenceRef.normalizeMove(capture.moveUci), capture.plyOffset)),
              rootMove = rootMove,
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(summary.sideToMove)
            )
        )
        val promotionEvents = materialEvents.filter(_.promotion)
        val promotionConsequences = promotionEvents.map(event =>
            LineConsequence(
              LineConsequenceKind.Promotion,
              proofOccurrencesThrough(event.plyOffset),
              eventOccurrence = Some(LineMoveOccurrence(EvidenceRef.normalizeMove(event.moveUci), event.plyOffset)),
              rootMove = rootMove.filter(_ => event.plyOffset == 0),
              rootSide = Some(summary.sideToMove),
              beneficiary = Some(event.side)
            )
          )
        materialResultConsequences ++ recaptureConsequences ++
          recoveryConsequences ++ promotionConsequences
      }
    (outcome ++ immediateReplyChecks ++ material).distinct


  private final case class MaterialOutcomeEvent(
      moveUci: String,
      plyOffset: Int,
      side: Color,
      balanceAfterForMover: Int,
      promotion: Boolean,
      recapture: Boolean,
      capture: Option[LineMaterialCapture]
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
  ): Option[MaterialOutcomeEvent] =
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
              .map(_ => event)
        }.flatten
      }.flatten
    }

  private def mateResultMatches(whitePovMate: Option[Int], winner: Color): Boolean =
    whitePovMate.exists(mate => mate != 0 && (mate > 0) == winner.white)
