package lila.chessjudgment.analysis.line

import chess.{ Color, King, Position }
import chess.format.Fen
import chess.variant.Standard

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
    val events = lineEvents(lineRef, facts, forcedTheme, materialSummary)
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
      lineRef: LineNodeRef,
      facts: PrincipalVariationEvidence.LineFacts,
      forcedTheme: Option[ForcedLineThemeEvidence],
      materialSummary: Option[LineMaterialSummary]
  ): List[LineMoveEvent] =
    val replayEvents =
      facts.line.moves.zipWithIndex.flatMap { case (move, index) =>
        val normalized = PrincipalVariationEvidence.normalizeUci(move.uci)
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
        castlingEvent(normalized, index).toList ++ stateEvents
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
    (replayEvents ++ roleEvents ++ forcedEvents ++ materialEvents).distinct

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

  private final case class EndgameTechniqueSnapshot(
      plyOffset: Int,
      moveUci: Option[String],
      pattern: String,
      rookPattern: String,
      techniqueSide: Color,
      requiredSquares: List[String]
  ):
    def key: (String, Color) = pattern -> techniqueSide

  private def endgameTechniqueHorizons(
      startFen: String,
      replay: List[LineReplayStep],
      consequences: List[LineConsequence]
  ): List[LineEndgameTechniqueHorizon] =
    val snapshots =
      (techniqueSnapshots(startFen, -1, None) ++
        replay.zipWithIndex.flatMap { case (step, index) =>
          techniqueSnapshots(step.fenAfter, index, Some(PrincipalVariationEvidence.normalizeUci(step.moveUci)))
        }).sortBy(_.plyOffset)
    val finalPlyOffset = if replay.nonEmpty then replay.size - 1 else -1
    val terminalKinds = consequences.filter(_.proofSignal).map(_.kind).distinct.sortBy(_.toString)
    val terminalOverrideKinds = terminalKinds.filter(terminalTechniqueOverride)
    val replayMovesByPly =
      replay.zipWithIndex.map { case (step, index) =>
        index -> PrincipalVariationEvidence.normalizeUci(step.moveUci)
      }.toMap
    snapshots
      .groupBy(_.key)
      .values
      .toList
      .flatMap(group =>
        contiguousTechniqueEpisodes(group.sortBy(_.plyOffset)).map(episode =>
          lineEndgameTechniqueHorizon(
            episode,
            snapshots,
            replayMovesByPly,
            finalPlyOffset,
            terminalKinds,
            terminalOverrideKinds
          )
        )
      )
      .sortBy(horizon => (horizon.pattern, horizon.techniqueSideKey, horizon.entryPlyOffset))

  private def contiguousTechniqueEpisodes(group: List[EndgameTechniqueSnapshot]): List[List[EndgameTechniqueSnapshot]] =
    group.foldLeft(List.empty[List[EndgameTechniqueSnapshot]]) { case (episodes, snapshot) =>
      episodes.lastOption match
        case None =>
          List(List(snapshot))
        case Some(current) if snapshot.plyOffset <= current.last.plyOffset + 1 =>
          episodes.dropRight(1) :+ (current :+ snapshot)
        case Some(_) =>
          episodes :+ List(snapshot)
    }

  private def techniqueSnapshots(
      fen: String,
      plyOffset: Int,
      moveUci: Option[String]
  ): List[EndgameTechniqueSnapshot] =
    positionAfter(fen).toList.flatMap { position =>
      List(Color.White, Color.Black).flatMap { side =>
        EndgamePatternOracle
          .analyze(position.board, side)
          .toList
          .flatMap(feature =>
            for
              pattern <- feature.primaryPattern
              geometry <- feature.rookEndgameGeometry
              if feature.rookEndgamePattern != lila.chessjudgment.model.strategic.RookEndgamePattern.None
            yield
              EndgameTechniqueSnapshot(
                plyOffset = plyOffset,
                moveUci = moveUci,
                pattern = pattern,
                rookPattern = feature.rookEndgamePattern.toString,
                techniqueSide = geometry.techniqueSide,
                requiredSquares = geometry.anchorSquares.map(_.key).distinct.sorted
              )
          )
      }
    }.distinctBy(snapshot => (snapshot.plyOffset, snapshot.pattern, snapshot.techniqueSide, snapshot.requiredSquares.mkString(",")))

  private def lineEndgameTechniqueHorizon(
      group: List[EndgameTechniqueSnapshot],
      allSnapshots: List[EndgameTechniqueSnapshot],
      replayMovesByPly: Map[Int, String],
      finalPlyOffset: Int,
      terminalKinds: List[LineConsequenceKind],
      terminalOverrideKinds: List[LineConsequenceKind]
  ): LineEndgameTechniqueHorizon =
    val first = group.head
    val last = group.last
    val requiredSquares = first.requiredSquares
    val presentAtFinal = group.exists(_.plyOffset == finalPlyOffset)
    val maintainedSquares =
      if presentAtFinal then requiredSquares.intersect(last.requiredSquares).distinct.sorted
      else Nil
    val baseStatus =
      if terminalOverrideKinds.nonEmpty && defensiveTechnique(first.pattern) then
        LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof
      else if terminalOverrideKinds.nonEmpty then
        LineEndgameTechniqueHorizonStatus.SupersededByTactic
      else if first.plyOffset > -1 && presentAtFinal then
        LineEndgameTechniqueHorizonStatus.Transitioned
      else if presentAtFinal then
        LineEndgameTechniqueHorizonStatus.Active
      else
        LineEndgameTechniqueHorizonStatus.Failed
    val missingAfterLastMove =
      replayMovesByPly.get(last.plyOffset + 1).orElse(
        allSnapshots
          .filter(snapshot => snapshot.plyOffset > last.plyOffset)
          .sortBy(_.plyOffset)
          .find(snapshot => snapshot.key != first.key)
          .flatMap(_.moveUci)
      )
    val triggerMove =
      if baseStatus == LineEndgameTechniqueHorizonStatus.Failed then missingAfterLastMove.orElse(first.moveUci)
      else if first.plyOffset > -1 then first.moveUci
      else None
    val brokenSquares = requiredSquares.diff(maintainedSquares).distinct.sorted
    LineEndgameTechniqueHorizon(
      pattern = first.pattern,
      rookPattern = first.rookPattern,
      techniqueSide = first.techniqueSide,
      entryPlyOffset = first.plyOffset,
      terminalPlyOffset = if presentAtFinal then finalPlyOffset else last.plyOffset,
      status = baseStatus,
      triggerMove = triggerMove,
      requiredSquares = requiredSquares,
      maintainedSquares = maintainedSquares,
      brokenSquares = brokenSquares,
      terminalConsequenceKinds = terminalKinds,
      failureReason =
        Option.when(baseStatus == LineEndgameTechniqueHorizonStatus.Failed)("technique-geometry-not-maintained")
          .orElse(Option.when(baseStatus == LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof)("terminal-proof-overrides-technique"))
          .orElse(Option.when(baseStatus == LineEndgameTechniqueHorizonStatus.SupersededByTactic)("terminal-proof-supersedes-technique"))
    )

  private def terminalTechniqueOverride(kind: LineConsequenceKind): Boolean =
    LineEndgameTechniqueHorizon.terminalProofOverrides(kind)

  private def defensiveTechnique(pattern: String): Boolean =
    LineEndgameTechniqueHorizon.defensivePattern(pattern)

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
