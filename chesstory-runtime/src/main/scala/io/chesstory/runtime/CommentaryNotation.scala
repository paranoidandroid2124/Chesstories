package io.chesstory.runtime

import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket

/** Display only: exact prefixes own SAN; protocol moves and evidence identities stay UCI. */
private[runtime] final class CommentaryNotation(packet: EvidenceBackedJudgmentPacket):
  private val root = packet.assembly.input.positionHistory.currentPosition
  private lazy val admitted = packet.assembly.lineReplays.valuesIterator.flatMap { replay =>
    replay.legalSteps.inits.filter(_.nonEmpty).map(steps => steps.map(_.uci) -> steps.last.move)
  }.toMap
  private val labels = scala.collection.mutable.Map.empty[List[String], String]

  def line(moves: List[String]): List[String] =
    moves.indices.map(index => move(moves.take(index), moves(index))).toList

  def move(prefix: List[String], uci: String): String =
    val moves = prefix :+ uci
    labels.synchronized {
      labels.getOrElseUpdate(moves, resolve(moves).map(_.toSanStr.value).getOrElse("?"))
    }

  private def resolve(moves: List[String]): Option[chess.Move] =
    admitted.get(moves).orElse {
      // A named legal alternative can lack a returned PV. Use its exact parent position.
      val before = if moves.size == 1 then Some(root) else resolve(moves.init).map(_.after)
      before.flatMap(_.legalMoves.find(move => move.toUci.uci == moves.last &&
        move.castle.forall(castle => move.dest == castle.kingTo)))
    }
