package lila.chessjudgment.analysis.opening

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Locale

import chess.opening.OpeningDb

import lila.chessjudgment.model.judgment.*

object OpeningIndexKeys:

  def movePrefixHash(moves: List[String]): String =
    hash(moves.map(normalizeUci).filter(_.nonEmpty).mkString(" "))

  def positionKey(fen: String): String =
    hash(boardStateFen(fen))

  def normalizeUci(uci: String): String =
    Option(uci).getOrElse("").trim.toLowerCase(Locale.ROOT)

  def boardStateFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).take(4).mkString(" ")

  private def hash(value: String): String =
    val digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8))
    digest.iterator.map(byte => f"${byte & 0xff}%02x").mkString.take(16)

final class OpeningRecognitionIndex private (
    entries: List[OpeningRecognitionIndex.Entry]
):

  private val byPrefixAndPosition: Map[(String, String), List[OpeningRecognitionIndex.Entry]] =
    entries.groupBy(entry => entry.movePrefixHash -> entry.positionKey)

  private val byPosition: Map[String, List[OpeningRecognitionIndex.Entry]] =
    entries.groupBy(_.positionKey)

  def allEntries: List[OpeningRecognitionIndex.Entry] =
    entries

  def recognize(movePrefixUci: List[String], fen: String, ply: Int): Option[OpeningRecognition] =
    val movePrefixHash = OpeningIndexKeys.movePrefixHash(movePrefixUci)
    val positionKey = OpeningIndexKeys.positionKey(fen)
    val exact =
      if movePrefixUci.nonEmpty then byPrefixAndPosition.getOrElse(movePrefixHash -> positionKey, Nil)
      else Nil
    if exact.nonEmpty then
      select(
        candidates = exact,
        movePrefixHash = movePrefixHash,
        positionKey = positionKey,
        ply = ply,
        matchedBy = OpeningRecognitionMatchKind.ExactPrefixAndPosition
      )
    else
      select(
        candidates = byPosition.getOrElse(positionKey, Nil),
        movePrefixHash = movePrefixHash,
        positionKey = positionKey,
        ply = ply,
        matchedBy = OpeningRecognitionMatchKind.PositionTransposition
      )

  private def select(
      candidates: List[OpeningRecognitionIndex.Entry],
      movePrefixHash: String,
      positionKey: String,
      ply: Int,
      matchedBy: OpeningRecognitionMatchKind
  ): Option[OpeningRecognition] =
    val eligible =
      candidates.filter(entry => ply <= 0 || entry.matchedPly <= ply)
    val bestPly = eligible.map(_.matchedPly).maxOption
    bestPly.flatMap { matchedPly =>
      val ranked =
        eligible
          .filter(_.matchedPly == matchedPly)
          .sortBy(entry => (-entry.candidate.confidence, -entry.candidate.frequency, -entry.candidate.sampleCount))
      ranked.headOption.map { best =>
        OpeningRecognition(
          movePrefixHash = movePrefixHash,
          positionKey = positionKey,
          matchedBy = matchedBy,
          candidates = ranked.map(_.candidate).distinctBy(candidate => candidate.identity -> candidate.lineage).take(3),
          matchedPly = matchedPly,
          frequency = best.candidate.frequency,
          sampleCount = best.candidate.sampleCount,
          confidence = best.candidate.confidence
        )
      }
    }

object OpeningRecognitionIndex:

  final case class Entry(
      movePrefixHash: String,
      positionKey: String,
      matchedPly: Int,
      candidate: OpeningCandidate
  )

  lazy val default: OpeningRecognitionIndex =
    fromEntries:
      OpeningDb.all.iterator.map: opening =>
        val moves = opening.uci.value.split("\\s+").toList.filter(_.nonEmpty)
        val eco = opening.eco.value
        val name = opening.name.value
        Entry(
          movePrefixHash = OpeningIndexKeys.movePrefixHash(moves),
          positionKey = OpeningIndexKeys.positionKey(opening.fen.value),
          matchedPly = moves.size,
          candidate = OpeningCandidate(
            identity = OpeningIdentity(
              eco = Some(eco),
              name = Some(name),
              family = OpeningFamily.fromEco(eco)
            ),
            lineage = Some(OpeningThemePriorIndex.lineageForOpeningName(name)),
            frequency = 1,
            sampleCount = 1,
            confidence = math.min(0.98, 0.55 + moves.size.toDouble / 20.0)
          )
        )

  val empty: OpeningRecognitionIndex =
    fromEntries(Nil)

  def fromEntries(entries: IterableOnce[Entry]): OpeningRecognitionIndex =
    OpeningRecognitionIndex(entries.iterator.toList)

  private[opening] def cleanText(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty)

  private[opening] def normalizeLineage(raw: String): String =
    raw.trim.toLowerCase(Locale.ROOT)
