package lila.chessjudgment.analysis.line

import chess.{ King, Pawn }

import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PreInitialHistoryKnowledge, PrincipalVariationEvidence }

object LineFactNormalizer:
  def fromValidatedLine(
      id: String,
      lineRef: LineNodeRef,
      replay: CanonicalLineReplay,
      position: PositionNodeRef,
      scope: EvidenceScope,
      materialSummary: Option[LineMaterialSummary] = None,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref = EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = position,
      line = Some(lineRef),
      scope = scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    EvidenceRecord(ref, LineFactEvidence.fromCertifiedReplay(lineRef, replay, materialSummary), parents)

  /** One projection of the already admitted canonical transitions and their exact predecessor. */
  private[chessjudgment] def materialEvents(
      positionHistory: CanonicalPositionHistory,
      replay: CanonicalLineReplay,
      predecessorReplay: Option[CanonicalLineReplay]
  ): List[ObservedLineMaterialEvent] =
    val singleStepReplays = replay.singleStepReplays
    singleStepReplays
      .zip(None :: singleStepReplays.map(Option(_)))
      .zipWithIndex
      .flatMap { case (((transition, transitionReplay), priorOccurrence), plyOffset) =>
        val declared = transition.declared
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
              priorOccurrence,
              transitionReplay,
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

  private[chessjudgment] def recaptureStatus(
      positionHistory: CanonicalPositionHistory,
      predecessorReplay: Option[CanonicalLineReplay],
      priorOccurrence: Option[(CanonicalReplayTransition, CanonicalLineReplay)],
      replay: CanonicalLineReplay,
      declared: LineReplayStep,
      plyOffset: Int
  ): LineMaterialRecaptureStatus =
    val predecessorOccurrence =
      if plyOffset == 0 then
        predecessorReplay
          .map(_.singleStepReplays)
          .flatMap(_.lastOption)
          .filter { case (previousTransition, _) =>
            val previousStep = previousTransition.declared
            previousStep.ply + 1 == declared.ply &&
              PrincipalVariationEvidence.sameBoardState(previousStep.fenAfter, declared.fenBefore)
          }
      else
        None
    priorOccurrence.orElse(predecessorOccurrence) match
      case None =>
        if plyOffset == 0 &&
            positionHistory.segmentReplaySteps.isEmpty &&
            positionHistory.preInitialHistoryKnowledge == PreInitialHistoryKnowledge.KnownEmpty
        then LineMaterialRecaptureStatus.Excluded
        else LineMaterialRecaptureStatus.Unknown
      case Some((priorTransition, priorReplay)) =>
        if priorTransition.relationDelta.rootMove.capture.isEmpty then
          LineMaterialRecaptureStatus.Excluded
        else
          priorReplay
            .exactRecaptureStatus(priorTransition.declared, replay, declared)
            .getOrElse(LineMaterialRecaptureStatus.Unknown)

  private[chessjudgment] def lineEvents(
      replay: CanonicalLineReplay,
      materialSummary: Option[LineMaterialSummary]
  ): List[LineMoveEvent] =
    val singleStepReplays = replay.singleStepReplays
    val replayEvents =
      singleStepReplays.zipWithIndex.flatMap { case ((transition, transitionReplay), index) =>
        val normalized = EvidenceRef.normalizeMove(transition.declared.moveUci)
        val moverRole = Some(EvidencePieceRole(transition.legal.move.piece.role.name))
        val relationDetails = transitionReplay.verticalRelationOccurrences(
          transition.declared,
          List(
            RelationFactKind.RootCheckResponse,
            RelationFactKind.CreatedCheckResponseInventory,
            RelationFactKind.StalemateTransition
          )
        ).map(_.relation.detail)
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
        val castling = Option.when(transition.relationDelta.rootMove.isCastling)(
          LineMoveEvent(
            kind = LineEventKind.Castling,
            moveUci = normalized,
            plyOffset = index,
            side = Some(transition.relationDelta.rootMove.side),
            pieceRole = Some(EvidencePieceRole(King.name)),
            square = Some(transition.relationDelta.rootMove.to)
          )
        )
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
