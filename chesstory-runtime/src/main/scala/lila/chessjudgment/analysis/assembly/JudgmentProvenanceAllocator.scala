package lila.chessjudgment.analysis.assembly

import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.Locale

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
      id = Some(s"$prefix:position:${positionKey(role, fen, ply)}")
    )

  def lineRootPositionRef(
      role: PositionNodeRole,
      fen: String,
      ply: Int,
      sideToMove: Option[Color],
      line: LineNodeRef
  ): PositionNodeRef =
    PositionNodeRef(
      fen = fen,
      ply = ply,
      sideToMove = sideToMove,
      id = Some(s"$prefix:position:${lineRootPositionKey(role, fen, ply, line)}")
    )

  def lineOccurrenceKey(
      line: AdmittedLegalLine,
      root: PositionNodeRef
  ): String =
    exactKey(List(
      root.id.getOrElse(""),
      root.ply.toString,
      root.sideToMove.map(_.toString).getOrElse(""),
      occurrenceFen(root.fen),
      line.rootMove
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
    exactKey(List(
      from.ply.toString,
      occurrenceFen(from.fen),
      EvidenceRef.normalizeMove(moveUci),
      to.ply.toString,
      occurrenceFen(to.fen)
    ))

  def evidenceId(suffix: String): String =
    s"$prefix:evidence:$suffix"

  def positionKey(role: PositionNodeRole, fen: String, ply: Int): String =
    exactKey(List(key(role), ply.toString, occurrenceFen(fen)))

  def lineRootPositionKey(
      role: PositionNodeRole,
      fen: String,
      ply: Int,
      line: LineNodeRef
  ): String =
    exactKey(List(key(role), ply.toString, occurrenceFen(fen), line.id))

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

  def key(value: Any): String =
    Option(value)
      .map(_.toString.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT))
      .getOrElse("none")

  private def occurrenceFen(fen: String): String =
    PrincipalVariationEvidence.normalizeFen(fen)

  private[assembly] def exactKey(parts: Iterable[String]): String =
    val raw = parts.iterator.map(value => s"${value.length}:$value").mkString("|")
    Base64.getUrlEncoder.withoutPadding.encodeToString(raw.getBytes(StandardCharsets.UTF_8))

object JudgmentProvenanceAllocator:

  def forInput(input: AdmittedMoveReviewInput): JudgmentProvenanceAllocator =
    val history = input.positionHistory
    val historyOccurrence = BoundedCausalIdentity.digest(
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
      s"move-review:$historyOccurrence:${input.beforePly}:${EvidenceRef.normalizeMove(input.playedMoveUci)}"
    )
