package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] object EvidenceIdentity:
  def digest(parts: Iterable[String]): String =
    val exact = parts.iterator.map(value => s"${value.length}:$value").mkString("|")
    HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
      .digest(exact.getBytes(StandardCharsets.UTF_8)))

  def lineKey(line: LineNodeRef): String =
    List(
      line.id,
      EvidenceRef.normalizeMove(line.rootMove)
    ).mkString(":")

  def stepKey(step: LineReplayStep): String =
    List(
      step.ply.toString,
      EvidenceRef.normalizeMove(step.moveUci),
      PrincipalVariationEvidence.normalizeFen(step.fenBefore),
      PrincipalVariationEvidence.normalizeFen(step.fenAfter)
    ).mkString(":")
