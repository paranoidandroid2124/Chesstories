package lila.chessjudgment.analysis.position

import chess.{ Board, Square, Role, Color, Position }
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.model._
import lila.chessjudgment.model.strategic.{
  EndgameFeature,
  EndgameOppositionType,
  RookEndgamePattern as OracleRookPattern,
  RuleOfSquareStatus,
  TheoreticalOutcomeHint
}

/**
 * FactExtractor
 * 
 * Maps board-backed motifs into verified facts with coordinate validation.
 */
object FactExtractor {

  private def strictNow(scope: FactScope): Boolean = scope == FactScope.Now

  /**
   * Generates a list of verified Facts from raw Motifs and Board state.
   */
  def fromMotifs(
      board: Board,
      motifs: List[Motif],
      scope: FactScope
  ): List[Fact] = {
    motifs.flatMap { m =>
      mapMotifToFact(m, board, scope)
    }.distinct
  }

  /**
   * Extracts static positional facts (Hanging pieces, Weak squares) from a board state.
   */
  def extractStaticFacts(position: Position, color: Color): List[Fact] = {
    val board = position.board
    val ourPieces = board.byColor(color)
    val scope = FactScope.Now

      val hanging = ourPieces.squares.flatMap { sq =>
        board.roleAt(sq).flatMap { role =>
        if (role == chess.King) None // King is never "hanging", it's in check or mate
        else {
          val attacks = legalCapturesOn(position, !color, sq)
          val attackers = attacks.map(_.orig).distinct
          val defenders = attacks.flatMap(legalRecapturesOn(_, sq)).distinct

          val pressureGap = attackers.size - defenders.size
          val isHanging =
            attackers.nonEmpty &&
              (
                (role != chess.Pawn && defenders.isEmpty) ||
                  (role != chess.Pawn && MaterialValue.materialValueUnit(role) > 1 && pressureGap >= 1) ||
                  (role == chess.Pawn && defenders.isEmpty && attackers.size >= 2)
              )

          if (isHanging) Some(Fact.HangingPiece(color, sq, role, attackers, defenders, scope))
          else None
        }
        }
      }

    val loose = ourPieces.squares.flatMap { sq =>
      board.roleAt(sq).flatMap { role =>
        val attacks = legalCapturesOn(position, !color, sq)
        val attackers = attacks.map(_.orig).distinct
        val defenders = attacks.flatMap(legalRecapturesOn(_, sq)).distinct

        if (attackers.nonEmpty && !hanging.exists(_.participants.contains(sq))) {
          Some(Fact.TargetPiece(color, sq, role, attackers, defenders, scope))
        } else None
      }
    }

    hanging ++ loose
  }

  private def legalCapturesOn(position: Position, side: Color, target: Square) =
    val active = if position.color == side then position else position.withColor(side)
    active.legalMoves.filter(move => move.captures && move.dest == target)

  private def legalRecapturesOn(capture: chess.Move, target: Square): List[Square] =
    capture.after.legalMoves
      .filter(move => move.captures && move.dest == target)
      .map(_.orig)
      .toList

  /**
   * Endgame specialized fact extraction.
   */
  def extractEndgameFacts(board: Board, color: Color): List[Fact] =
    extractEndgameFacts(board, color, None)

  /**
   * Endgame specialized fact extraction.
   * If oracle output is present, it is treated as the single source of truth.
   */
  def extractEndgameFacts(
      board: Board,
      color: Color,
      oracle: Option[EndgameFeature]
  ): List[Fact] = {
    oracle match {
      case Some(endgame) => extractEndgameFactsFromOracle(board, color, endgame)
      case None          => extractEndgameFactsHeuristic(board, color)
    }
  }

  private def extractEndgameFactsHeuristic(board: Board, color: Color): List[Fact] = {
    // (Approximated here by low total material or absence of major pieces)
    val majorPieces = (board.queens | board.rooks)
    val totalMaterial = board.occupied.count
    val isEndgame = majorPieces.count <= 2 || totalMaterial <= 15

    if (!isEndgame) return Nil

    val myKing = board.kingPosOf(color)
    val scope = FactScope.Now
    val facts = scala.collection.mutable.ListBuffer[Fact]()

    myKing.foreach { mk =>
      // King Activity: Correct mobility counts squares king can actually move to
      val mobility = (mk.kingAttacks & ~board.byColor(color)).count
      val dist = Math.max((mk.file.value - 4).abs, (mk.rank.value - 4).abs)
      facts += Fact.KingActivity(color, mk, mobility.toInt, dist, scope)
    }

    facts.toList
  }

  private def extractEndgameFactsFromOracle(
      board: Board,
      color: Color,
      endgame: EndgameFeature
  ): List[Fact] = {
    val myKing = board.kingPosOf(color)
    val scope = FactScope.Now
    val facts = scala.collection.mutable.ListBuffer[Fact]()

    myKing.foreach { mk =>
      val mobility = (mk.kingAttacks & ~board.byColor(color)).count
      val dist = Math.max((mk.file.value - 4).abs, (mk.rank.value - 4).abs)
      facts += Fact.KingActivity(color, mk, mobility.toInt, dist, scope)
    }

    if (endgame.isZugzwang || endgame.zugzwangLikelihood >= 0.65) {
      facts += Fact.Zugzwang(color, scope)
    }

    if (endgame.rookEndgamePattern != OracleRookPattern.None) {
      val anchorSquares =
        if endgame.rookEndgameAnchorSquares.nonEmpty then endgame.rookEndgameAnchorSquares
        else rookEndgameAnchorSquares(board)
      facts += Fact.RookEndgamePattern(
        endgame.rookEndgamePattern,
        scope,
        endgame.primaryPattern,
        anchorSquares,
        endgame.rookEndgameGeometry
      )
    }

    if (endgame.ruleOfSquare != RuleOfSquareStatus.NA) {
      val enemyPassers = board.byPiece(!color, chess.Pawn).squares.filter(isPassedPawn(board, _, !color))
      val targetPawnOpt = enemyPassers.sortBy(p => if ((!color).white) -p.rank.value else p.rank.value).headOption
      targetPawnOpt.foreach { pawnSq =>
        val promoRank = if ((!color).white) 7 else 0
        val promoSqOpt = Square.at(pawnSq.file.value, promoRank)
        for
          mk <- myKing
          promoSq <- promoSqOpt
        do
          facts += Fact.RuleOfSquare(
            defenderKing = mk,
            targetPawn = pawnSq,
            promotionSquare = promoSq,
            status = endgame.ruleOfSquare,
            scope = scope
          )
      }
    }

    if (endgame.theoreticalOutcomeHint != TheoreticalOutcomeHint.Unclear) {
      facts += Fact.EndgameOutcome(endgame.theoreticalOutcomeHint, endgame.confidence, scope)
    }

    facts.toList.distinct
  }

  private def rookEndgameAnchorSquares(board: Board): List[Square] = {
    val kings =
      List(Color.White, Color.Black).flatMap(color => board.kingPosOf(color))
    val rooks =
      List(Color.White, Color.Black).flatMap(color => board.byPiece(color, chess.Rook).squares.toList)
    val passedPawnsWithColor =
      List(Color.White, Color.Black).flatMap(color =>
        board.byPiece(color, chess.Pawn).squares.filter(pawn => isPassedPawn(board, pawn, color)).toList.map(_ -> color)
      )
    val promotionSquares =
      passedPawnsWithColor.flatMap { case (pawn, color) =>
        Square.at(pawn.file.value, if (color.white) 7 else 0)
      }
    (kings ++ rooks ++ passedPawnsWithColor.map(_._1) ++ promotionSquares).distinct
  }

  private def isPassedPawn(board: Board, pawnSq: Square, color: Color): Boolean = {
    val oppPawnsByFile = board.byPiece(!color, chess.Pawn).squares.groupBy(_.file)
    val fileValue = pawnSq.file.value
    val filesToCheck = List(fileValue - 1, fileValue, fileValue + 1).filter(f => f >= 0 && f <= 7)
    filesToCheck.forall { idx =>
      chess.File.all.lift(idx).forall { f =>
        oppPawnsByFile.get(f).forall { pawns =>
          pawns.forall { oppPawn =>
            if (color.white) oppPawn.rank.value <= pawnSq.rank.value
            else oppPawn.rank.value >= pawnSq.rank.value
          }
        }
      }
    }
  }

  private def mapMotifToFact(motif: Motif, board: Board, scope: FactScope): Option[Fact] = {
    motif match {
      case m: Motif.Pin =>
        for {
          a <- m.pinningSq
          p <- m.pinnedSq
          b <- m.behindSq
          aRole <- roleForScope(board, a, m.pinningPiece, scope)
          pRole <- roleForScope(board, p, m.pinnedPiece, scope)
          bRole <- roleForScope(board, b, m.targetBehind, scope)
        } yield Fact.Pin(m.color, a, aRole, p, pRole, b, bRole, m.isAbsolutePin, scope)

      case m: Motif.Skewer =>
        for {
          a <- m.attackingSq
          f <- m.frontSq
          b <- m.backSq
          aRole <- roleForScope(board, a, m.attackingPiece, scope)
          fRole <- roleForScope(board, f, m.frontPiece, scope)
          bRole <- roleForScope(board, b, m.backPiece, scope)
        } yield Fact.Skewer(m.color, a, aRole, f, fRole, b, bRole, scope)

      case m: Motif.Fork =>
        val attackerRoleOpt =
          if strictNow(scope) then board.roleAt(m.square)
          else Some(m.attackingPiece)
        val verifiedTargets =
          if strictNow(scope) then
            m.targetSquares.flatMap(ts => board.roleAt(ts).map(r => (ts, r)))
          else
            m.targetSquares.zip(m.targets)
        attackerRoleOpt.flatMap { attackerRole =>
          Option.when(verifiedTargets.nonEmpty)(Fact.Fork(m.color, m.square, attackerRole, verifiedTargets, scope))
        }

      case m: Motif.XRay =>
        xrayGeometry(board, m.color, m.piece, m.target, m.square).map { case (attacker, blocker, blockerRole) =>
          Fact.XRay(m.color, attacker, m.piece, blocker, blockerRole, m.square, m.target, scope)
        }

      case m: Motif.Battery =>
        for
          front <- m.frontSq
          back <- m.backSq
        yield Fact.Battery(
          m.color,
          front,
          m.front,
          back,
          m.back,
          batteryAxis(m.axis),
          scope,
          m.targetSq,
          m.targetRole
        )

      case m: Motif.OpenFileControl =>
        Some(Fact.FileControl(m.file, m.color, open = true, scope))

      case m: Motif.SemiOpenFileControl =>
        Some(Fact.FileControl(m.file, m.color, open = false, scope))

      case m: Motif.SpaceAdvantage =>
        Some(Fact.SpaceAdvantage(m.color, m.pawnDelta, scope))

      case m: Motif.Outpost =>
        Some(Fact.Outpost(m.square, m.piece, scope))

      case m: Motif.Centralization =>
        Some(Fact.ActivatesPiece(m.piece, m.square, m.square, false, scope)) // Simplified mapping

      case m: Motif.Opposition =>
        val fileDiff = (m.ownKingSquare.file.value - m.opponentKingSquare.file.value).abs
        val rankDiff = (m.ownKingSquare.rank.value - m.opponentKingSquare.rank.value).abs
        Some(
          Fact.Opposition(
            color = m.color,
            king = m.ownKingSquare,
            enemyKing = m.opponentKingSquare,
            distance = Math.max(fileDiff, rankDiff),
            isDirect = m.oppType == Motif.OppositionType.Direct,
            oppositionType = m.oppType match
              case Motif.OppositionType.Direct => EndgameOppositionType.Direct
              case Motif.OppositionType.Distant => EndgameOppositionType.Distant
              case Motif.OppositionType.Diagonal => EndgameOppositionType.Diagonal,
            scope = scope
          )
        )

      case m: Motif.Zugzwang =>
        Some(Fact.Zugzwang(m.color, scope))

      case m: Motif.Check =>
        if (m.checkType == Motif.CheckType.Double) Some(Fact.DoubleCheck(List(m.targetSquare), scope)) // Simplified
        else Some(Fact.TargetPiece(!m.color, m.targetSquare, chess.King, List(m.targetSquare), Nil, scope))

      case m: Motif.PawnPromotion =>
        Square.at(m.file.value, if (m.color.white) 7 else 0)
          .map(Fact.PawnPromotion(_, Some(m.promotedTo), scope))

      case m: Motif.PassedPawnPush =>
        Square.at(m.file.value, m.toRank - 1)
          .map(Fact.PawnPromotion(_, None, scope))

      /*
      case m: Motif.StalemateThreat =>
        board.kingPosOf(m.color).map(k => Fact.StalemateThreat(k, scope))
      */

      case _: Motif.DoubleCheck =>
        // Need to find the checking squares - in Motif it's just a category
        // But we can approximate or leave as TargetPiece for now if data is missing
        Some(Fact.DoubleCheck(Nil, scope))

      case _ => None
    }
  }

  private def roleForScope(board: Board, square: Square, motifRole: Role, scope: FactScope): Option[Role] =
    if strictNow(scope) then board.roleAt(square)
    else board.roleAt(square).orElse(Some(motifRole))

  private def xrayGeometry(
      board: Board,
      attackerColor: Color,
      attackerRole: Role,
      targetRole: Role,
      target: Square
  ): Option[(Square, Square, Role)] =
    board.pieceAt(target).filter(piece => piece.color != attackerColor && piece.role == targetRole).flatMap { _ =>
      board.byPiece(attackerColor, attackerRole).squares.toList.flatMap { attacker =>
        rayStep(attacker, target).toList.flatMap { step =>
          val blockers = squaresBetween(attacker, target, step).flatMap(square => board.roleAt(square).map(square -> _))
          blockers match
            case List((blocker, blockerRole)) => Some((attacker, blocker, blockerRole))
            case _                            => None
        }
      }.headOption
    }

  private def rayStep(from: Square, to: Square): Option[(Int, Int)] =
    val fileDiff = to.file.value - from.file.value
    val rankDiff = to.rank.value - from.rank.value
    Option.when(fileDiff == 0 || rankDiff == 0 || fileDiff.abs == rankDiff.abs) {
      (Integer.signum(fileDiff), Integer.signum(rankDiff))
    }.filter(_ != (0, 0))

  private def squaresBetween(from: Square, to: Square, step: (Int, Int)): List[Square] =
    def loop(file: Int, rank: Int, acc: List[Square]): List[Square] =
      Square.at(file, rank) match
        case Some(square) if square == to => acc.reverse
        case Some(square)                 => loop(file + step._1, rank + step._2, square :: acc)
        case None                         => acc.reverse
    loop(from.file.value + step._1, from.rank.value + step._2, Nil)

  private def batteryAxis(axis: Motif.BatteryAxis): Fact.RayAxis =
    axis match
      case Motif.BatteryAxis.File     => Fact.RayAxis.File
      case Motif.BatteryAxis.Rank     => Fact.RayAxis.Rank
      case Motif.BatteryAxis.Diagonal => Fact.RayAxis.Diagonal

}
