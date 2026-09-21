package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.judgment.{
  AdmittedLegalLine,
  AdmittedMoveReviewInput
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence

final case class JudgmentProvenanceAllocator(prefix: String):

  def positionRef(
      role: PositionNodeRole,
      fen: String,
      ply: Int,
      sideToMove: Option[Color]
  ): PositionNodeRef =
    PositionNodeRef(
      fen = fen,
      ply = ply,
      sideToMove = sideToMove,
      id = Some(EvidenceIdentity.digest(List(prefix, "position", role.toString, ply.toString, occurrenceFen(fen))))
    )

  def lineOccurrenceKey(
      line: AdmittedLegalLine,
      root: PositionNodeRef
  ): String =
    EvidenceIdentity.digest(List(
      root.id.getOrElse(""),
      root.ply.toString,
      root.sideToMove.map(_.toString).getOrElse(""),
      occurrenceFen(root.fen),
      line.moves.mkString(" ")
    ))

  def lineRef(
      line: AdmittedLegalLine,
      occurrenceKey: String
  ): LineNodeRef =
    LineNodeRef(
      id = s"$prefix:line:$occurrenceKey",
      rootMove = line.rootMove
    )

  def transitionOccurrenceKey(
      from: PositionNodeRef,
      moveUci: String,
      to: PositionNodeRef
  ): String =
    EvidenceIdentity.digest(List(
      from.id.getOrElse(""),
      from.ply.toString,
      occurrenceFen(from.fen),
      EvidenceRef.normalizeMove(moveUci),
      to.id.getOrElse(""),
      to.ply.toString,
      occurrenceFen(to.fen)
    ))

  def evidenceId(suffix: String): String =
    s"$prefix:evidence:$suffix"

  def successorPositionRef(predecessor: PositionNodeRef, step: LineReplayStep, side: Color): PositionNodeRef =
    val occurrence = EvidenceIdentity.digest(List(
      prefix,
      predecessor.id.getOrElse(""),
      step.ply.toString,
      EvidenceRef.normalizeMove(step.moveUci),
      occurrenceFen(step.fenBefore),
      occurrenceFen(step.fenAfter)
    ))
    PositionNodeRef(step.fenAfter, step.ply, Some(side), Some(occurrence))

  def evidenceRef(
      suffix: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(
      id = evidenceId(suffix),
      producer = producer,
      layer = layer,
      position = position,
      line = line,
      scope = scope,
      confidence = confidence
    )

  private def occurrenceFen(fen: String): String =
    PrincipalVariationEvidence.normalizeFen(fen)

object JudgmentProvenanceAllocator:

  def forInput(input: AdmittedMoveReviewInput): JudgmentProvenanceAllocator =
    val history = input.positionHistory
    val historyOccurrence = EvidenceIdentity.digest(
      List(
        "canonical-history-occurrence:v1",
        history.initialFen,
        history.preInitialHistoryKnowledge.toString,
        history.segmentReplaySteps
          .map(step =>
            List(
              step.ply.toString,
              PrincipalVariationEvidence.normalizeUci(step.uci),
              step.beforeFen,
              step.afterFen
            ).mkString(":")
          )
          .mkString("[", ",", "]"),
        history.currentFen,
        history.currentPly.toString
      )
    )
    JudgmentProvenanceAllocator(
      s"move-review:$historyOccurrence:${history.currentPly}:${EvidenceRef.normalizeMove(input.playedMoveUci)}"
    )
