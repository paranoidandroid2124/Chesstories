package lila.chessjudgment.analysis.tactical

import chess.*
import chess.format.Uci

private[chessjudgment] object TacticalPatternDetectors:

  val ordered: List[TacticalPatternDetector] =
    List(
      SmotheredMateDetector,
      BackRankMateDetector,
      ArabianMateDetector,
      BodensMateDetector,
      AnastasiaMateDetector,
      HookMateDetector,
      CornerMateDetector,
      StalemateTrapDetector,
      GreekGiftDetector
    )

  private object SmotheredMateDetector extends TacticalPatternDetector:
    val id = "smothered_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.board.kingPosOf(after.color).exists { king =>
        after.board.attackers(king, !after.color).forall(sq => after.board.roleAt(sq).contains(Knight)) &&
          king.kingAttacks.forall(sq => after.board.pieceAt(sq).exists(_.color == after.color))
      }

  private object BackRankMateDetector extends TacticalPatternDetector:
    val id = "back_rank_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      val loser = after.color
      after.board.kingPosOf(loser).exists { king =>
        val backRank = if loser.white then Rank.First else Rank.Eighth
        if king.rank != backRank then false
        else
          val checkedByMajor =
            after.board.attackers(king, !loser).exists(sq => after.board.roleAt(sq).exists(r => r == Rook || r == Queen))
          checkedByMajor && {
            val forward = if loser.white then Rank.Second else Rank.Seventh
            king.kingAttacks.filter(_.rank == forward).forall { sq =>
              after.board.pieceAt(sq).exists(_.color == loser) || after.board.attackers(sq, !loser).nonEmpty
            }
          }
      }

  private object ArabianMateDetector extends TacticalPatternDetector:
    val id = "arabian_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.board.kingPosOf(after.color).exists { king =>
        val winner = !after.color
        isCornerRegion(king) && {
          val checkers = after.board.attackers(king, winner)
          checkers.exists(sq =>
            after.board.roleAt(sq).contains(Rook) &&
              after.board.attackers(sq, winner).exists(k => after.board.roleAt(k).contains(Knight))
          )
        }
      }

  private object BodensMateDetector extends TacticalPatternDetector:
    val id = "bodens_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.board.kingPosOf(after.color).exists { king =>
        val winner = !after.color
        val checkers = after.board.attackers(king, winner)
        checkers.exists(sq => after.board.roleAt(sq).contains(Bishop)) &&
          (after.board.bishops & after.board.byColor(winner)).count >= 2
      }

  private object AnastasiaMateDetector extends TacticalPatternDetector:
    val id = "anastasia_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.board.kingPosOf(after.color).exists { king =>
        if king.file != File.H && king.file != File.A then false
        else
          val winner = !after.color
          val checkers = after.board.attackers(king, winner)
          val fileCheck =
            checkers.exists(sq =>
              (after.board.roleAt(sq).contains(Rook) || after.board.roleAt(sq).contains(Queen)) && sq.file == king.file
            )
          fileCheck &&
            (after.board.knights & after.board.byColor(winner)).exists { sq =>
              sq.file != king.file && (sq.rank.value - king.rank.value).abs <= 2
            }
      }

  private object HookMateDetector extends TacticalPatternDetector:
    val id = "hook_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.board.kingPosOf(after.color).exists { king =>
        val winner = !after.color
        val checkers = after.board.attackers(king, winner)
        val rookCheck = checkers.find(sq => after.board.roleAt(sq).contains(Rook))
        rookCheck.exists { rSq =>
          after.board.attackers(rSq, winner).exists(sq => after.board.roleAt(sq).contains(Knight))
        }
      }

  private object CornerMateDetector extends TacticalPatternDetector:
    val id = "corner_mate"
    val requiresMate = true

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.board.kingPosOf(after.color).exists { king =>
        isCornerRegion(king) &&
          after.board.attackers(king, !after.color).count >= 1 &&
          king.kingAttacks.forall(sq => after.board.pieceAt(sq).isDefined)
      }

  private object StalemateTrapDetector extends TacticalPatternDetector:
    val id = "stalemate_trap"
    val requiresMate = false

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      after.staleMate

  private object GreekGiftDetector extends TacticalPatternDetector:
    val id = "greek_gift"
    val requiresMate = false

    def matches(before: Option[Position], after: Position, lastUci: String): Boolean =
      matchesWithContinuations(before, after, lastUci, Nil)

    override def matchesWithContinuations(
        before: Option[Position],
        after: Position,
        lastUci: String,
        continuations: List[List[String]]
    ): Boolean =
      before.exists { beforePos =>
        Uci(lastUci).collect { case m: Uci.Move => m }.exists { move =>
          val mover = beforePos.color
          val targetKey = if mover.white then "h7" else "h2"
          Square.fromKey(targetKey).exists { target =>
            move.dest == target &&
              after.check.yes &&
              beforePos.board.pieceAt(target).exists(piece => piece.color == !mover && piece.role == Pawn) &&
              after.board.pieceAt(target).exists(piece => piece.color == mover && piece.role == Bishop) &&
              (
                greekGiftKingsideSupport(after.board, mover, target) ||
                  continuationBuildsGreekGiftSupport(after, mover, target, lastUci, continuations)
              )
          }
        }
      }

  private def continuationBuildsGreekGiftSupport(
      after: Position,
      mover: Color,
      target: Square,
      lastUci: String,
      continuations: List[List[String]]
  ): Boolean =
    continuations.exists { rawLine =>
      val line = stripPlayedPrefix(rawLine.map(normalizeUci).filter(isUciMove), normalizeUci(lastUci)).take(8)
      var pos = Option(after)
      var supported = false
      val iterator = line.iterator
      while iterator.hasNext && pos.nonEmpty && !supported do
        val uci = iterator.next()
        pos = pos.flatMap(applyUci(_, uci))
        supported = pos.exists(next => greekGiftKingsideSupport(next.board, mover, target))
      supported
    }

  private def stripPlayedPrefix(line: List[String], played: String): List[String] =
    if line.headOption.contains(played) then line.tail else line

  private def greekGiftKingsideSupport(board: Board, mover: Color, target: Square): Boolean =
    board.byPiece(mover, Knight).squares.exists(_.knightAttacks.contains(target)) ||
      board.byPiece(mover, Queen).squares.exists { queen =>
        queen.queenAttacks(board.occupied).contains(target)
      }

  private def applyUci(position: Position, raw: String): Option[Position] =
    Uci(raw).collect { case move: Uci.Move => move }.flatMap(position.move(_).toOption.map(_.after))

  private def normalizeUci(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def isUciMove(raw: String): Boolean =
    raw.matches("""[a-h][1-8][a-h][1-8][qrbn]?""")

  private def isCornerRegion(sq: Square): Boolean =
    (sq.file == File.A || sq.file == File.B || sq.file == File.G || sq.file == File.H) &&
      (sq.rank == Rank.First || sq.rank == Rank.Second || sq.rank == Rank.Seventh || sq.rank == Rank.Eighth)
