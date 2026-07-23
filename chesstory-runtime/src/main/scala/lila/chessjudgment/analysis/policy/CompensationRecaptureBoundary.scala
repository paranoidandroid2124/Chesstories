package lila.chessjudgment.analysis.policy

import chess.{ Board, Color }
import chess.format.{ Fen, Uci }
import lila.chessjudgment.analysis.material.MaterialValue

private[chessjudgment] object CompensationRecaptureBoundary:

  private val MaterialToleranceCp = 75

  /**
   * Immediate parity-restoring captures should not be promoted into
   * durable after-move compensation.
   *
   * We do not have the previous ply's SAN/UCI here, so the gate uses the
   * current board plus the played move:
   * - the move must be a capture
   * - the mover must currently be down material by roughly the value of the captured piece
   * - the capture must restore near-parity for the mover
   */
  def suppressAfterCompensation(
      fenBefore: String,
      playedMoveUci: String,
      investedMaterial: Int
  ): Boolean =
    if investedMaterial <= 0 then false
    else
      Fen.read(chess.variant.Standard, Fen.Full(fenBefore))
        .flatMap { pos =>
          Uci(playedMoveUci)
            .collect { case m: Uci.Move => m }
            .flatMap(pos.move(_).toOption)
            .map { move =>
              if !move.captures then false
              else
                val capturedRole =
                  move.capture
                    .flatMap(pos.board.roleAt)
                    .orElse(pos.board.roleAt(move.dest))

                capturedRole.exists { role =>
                  val capturedValue = MaterialValue.materialValueCp(role)
                  val mover = pos.color
                  val beforeDeficit = sideMaterialDeficitCp(pos.board, mover)
                  val afterDeficit = sideMaterialDeficitCp(move.after.board, mover)
                  val recovered = beforeDeficit - afterDeficit

                  beforeDeficit >= capturedValue - MaterialToleranceCp &&
                  afterDeficit <= MaterialToleranceCp &&
                  recovered >= capturedValue - MaterialToleranceCp
                }
            }
        }
        .getOrElse(false)

  private def sideMaterialDeficitCp(board: Board, side: Color): Int =
    val diff = MaterialValue.materialBalanceCp(board)
    if side.white then math.max(0, -diff) else math.max(0, diff)
