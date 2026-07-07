package lila.chessjudgment.analysis.move

import lila.chessjudgment.model.*
import lila.chessjudgment.model.Motif.*
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.position.PositionAnalyzer

import _root_.chess.*
import _root_.chess.format.{ Fen, Uci }

/**
 * Extracts typed motifs from legal move replay.
 */
object MoveAnalyzer:

  private def replayMotifs(initialFen: String, pv: List[String]): Option[List[Motif]] =
    Fen.read(chess.variant.Standard, Fen.Full(initialFen)).flatMap { initialPos =>
      pv.zipWithIndex.foldLeft(Option((initialPos, Option.empty[Move], List.empty[Motif]))) {
        case (Some((pos, lastMv, acc)), (uciStr, index)) =>
          Uci(uciStr).collect { case m: Uci.Move => m }.flatMap(pos.move(_).toOption).map { mv =>
            val moveMotifs = detectMoveMotifs(mv, pos, index, lastMv)
            val stateMotifs = detectStateMotifs(mv.after, index)
            (mv.after, Some(mv), acc ++ moveMotifs ++ stateMotifs)
          }
        case (None, _) => None
      }.map(_._3)
    }

  /**
   * Translates a PV (list of UCI moves) into a list of Motifs.
   */
  def tokenizePv(initialFen: String, pv: List[String]): Option[List[Motif]] =
    replayMotifs(initialFen, pv)

  def detectMoveMotifs(mv: Move, pos: Position, plyIndex: Int, prevMove: Option[Move] = None): List[Motif] =
    val color = pos.color
    val moveUci = mv.toUci.uci
    val nextPos = mv.after

    List.concat(
      detectPawnMotifs(mv, pos, color, moveUci, plyIndex),
      detectPieceMotifs(mv, nextPos, color, moveUci, plyIndex),
      detectKingMotifs(mv, color, moveUci, plyIndex),
      detectTacticalMotifs(mv, pos, nextPos, color, moveUci, plyIndex, prevMove),
      detectPositionalMotifs(mv, pos, nextPos, color, moveUci, plyIndex) 
    )

  private def detectPawnMotifs(mv: Move, _pos: Position, color: Color, moveUci: String, plyIndex: Int): List[Motif] =
    if mv.piece.role != Pawn then return Nil
    
    var motifs = List.empty[Motif]

    // Pawn Advance (non-capture)
    if !mv.captures then
      motifs = motifs :+ PawnAdvance(
        file = mv.dest.file,
        fromRank = mv.orig.rank.value + 1,
        toRank = mv.dest.rank.value + 1,
        color = color,
        plyIndex = plyIndex,
        move = Some(moveUci)
      )

    val beforeBoard = _pos.board
    val wPassed = PositionAnalyzer.passedPawns(Color.White, beforeBoard.pawns & beforeBoard.white, beforeBoard.pawns & beforeBoard.black)
    val bPassed = PositionAnalyzer.passedPawns(Color.Black, beforeBoard.pawns & beforeBoard.black, beforeBoard.pawns & beforeBoard.white)
    val passedSquares = if color == Color.White then wPassed else bPassed

    if (passedSquares.contains(mv.orig)) {
       motifs = motifs :+ PassedPawnPush(
         file = mv.dest.file,
         toRank = mv.dest.rank.value + 1,
         color = color,
         plyIndex = plyIndex,
         move = Some(moveUci)
       )
    }

    // Pawn Break (capture)
    if mv.captures then
      motifs = motifs :+ PawnBreak(
        file = mv.orig.file,
        targetFile = mv.dest.file,
        color = color,
        plyIndex = plyIndex,
        move = Some(moveUci)
      )

    // Pawn Promotion
    mv.promotion.foreach { promotedTo =>
      motifs = motifs :+ PawnPromotion(
        file = mv.dest.file,
        promotedTo = promotedTo,
        color = color,
        plyIndex = plyIndex,
        move = Some(moveUci)
      )
    }

    motifs

  private def detectPieceMotifs(mv: Move, nextPos: Position, color: Color, moveUci: String, plyIndex: Int): List[Motif] =
    var motifs = List.empty[Motif]
    val role = mv.piece.role

    // Rook Lift
    if role == Rook then
      val backRankValue = if color.white then 0 else 7
      if mv.orig.rank.value == backRankValue && mv.dest.rank.value != backRankValue then
        motifs = motifs :+ RookLift(
          file = mv.dest.file,
          fromRank = mv.orig.rank.value + 1,
          toRank = mv.dest.rank.value + 1,
          color = color,
          plyIndex = plyIndex,
          move = Some(moveUci)
        )

    // Fianchetto
    if role == Bishop then
      val fianchettoSquares = if color.white then List(Square.G2, Square.B2) else List(Square.G7, Square.B7)
      if fianchettoSquares.contains(mv.dest) then
        val side = if mv.dest.file == File.G then FianchettoSide.Kingside else FianchettoSide.Queenside
        motifs = motifs :+ Fianchetto(side, color, plyIndex, Some(moveUci))

    // Centralization
    val centralSquares = List(Square.D4, Square.D5, Square.E4, Square.E5)
    if (role == Knight || role == Bishop) && centralSquares.contains(mv.dest) then
      motifs = motifs :+ Centralization(role, mv.dest, color, plyIndex, Some(moveUci))

    // Outpost
    if role == Knight then
      val isInEnemyTerritory = if color.white then mv.dest.rank.value >= 4 else mv.dest.rank.value <= 3
      if isInEnemyTerritory then
        val board = nextPos.board
        val attackedByEnemyPawn = board.attackers(mv.dest, !color).exists(sq => board.roleAt(sq).contains(Pawn))
        val supportedByFriendlyPawn = board.attackers(mv.dest, color).exists(sq => board.roleAt(sq).contains(Pawn))
        if (!attackedByEnemyPawn && supportedByFriendlyPawn) {
          motifs = motifs :+ Outpost(role, mv.dest, color, plyIndex, Some(moveUci))
        }

    motifs

  private def detectKingMotifs(mv: Move, color: Color, moveUci: String, plyIndex: Int): List[Motif] =
    if mv.piece.role != King then return Nil
    var motifs = List.empty[Motif]
    if mv.castle.isDefined then
      val side = if mv.dest.file == File.G then CastlingSide.Kingside else CastlingSide.Queenside
      motifs = motifs :+ Castling(side, color, plyIndex, Some(moveUci))
    else
      val stepType = determineKingStepType(mv, color)
      motifs = motifs :+ KingStep(stepType, color, plyIndex, Some(moveUci))
    motifs

  private def determineKingStepType(mv: Move, color: Color): KingStepType =
    val relFrom = Motif.relativeRank(mv.orig.rank.value + 1, color)
    val relTo = Motif.relativeRank(mv.dest.rank.value + 1, color)
    if relFrom == 1 && relTo > 1 then KingStepType.OffBackRank
    else if relTo >= 4 then KingStepType.Activation
    else if mv.dest.file == File.A || mv.dest.file == File.H then KingStepType.ToCorner
    else KingStepType.Other

  def detectTacticalMotifs(
      mv: Move, 
      pos: Position, 
      nextPos: Position,
      color: Color, 
      moveUci: String, 
      plyIndex: Int,
      prevMove: Option[Move] = None
  ): List[Motif] = {
    var motifs = List.empty[Motif]
    
    // Check
    if nextPos.check.yes then
      val checkers = nextPos.checkers
                                                                                               
      val isMate = nextPos.checkMate
      val isDouble = checkers.count > 1
      val isDiscovered = checkers.exists(sq => sq != mv.dest)
      
      val checkType = if (isMate) {
        if (isSmotheredPattern(nextPos, color)) CheckType.Smothered 
        else CheckType.Mate
      } else if (isDouble) CheckType.Double
      else if (isDiscovered) CheckType.Discovered
      else CheckType.Normal
      
      nextPos.board.kingPosOf(!color).foreach { kingSq =>
        motifs = motifs :+ Motif.Check(mv.piece.role, kingSq, checkType, color, plyIndex, Some(moveUci))
      }
    
    // Capture (enhanced with Exchange Sacrifice detection and Recapture logic)
    if mv.captures then
      mv.capture.flatMap(pos.board.roleAt).orElse(pos.board.roleAt(mv.dest)).foreach { capturedRole =>
        val isRecapture = prevMove.exists(pm => pm.captures && pm.dest == mv.dest)
        val captureType = if (isRecapture) CaptureType.Recapture else determineCaptureType(mv.piece.role, capturedRole)

        // Detect Positional Sacrifice with ROI (any meaningful material sacrifice)
        // Cases: Rook for minor piece, Minor piece for pawn, Queen sacrifice
        val isMaterialSacrifice = (
          // Rook for minor piece (classic exchange sacrifice)
          (mv.piece.role == Rook && (capturedRole == Knight || capturedRole == Bishop)) ||
          // Minor piece for pawn (positional sacrifice)
          ((mv.piece.role == Knight || mv.piece.role == Bishop) && capturedRole == Pawn) ||
          // Queen sacrifice for compensation
          (mv.piece.role == Queen && capturedRole != Queen)
        )

        val sacrificeROI = if (isMaterialSacrifice) {
          detectExchangeSacrificeROI(mv, nextPos, color)
        } else None

        val finalCaptureType = if (sacrificeROI.isDefined) CaptureType.ExchangeSacrifice else captureType
        motifs = motifs :+ Motif.Capture(mv.piece.role, capturedRole, mv.dest, finalCaptureType, color, plyIndex, Some(moveUci), sacrificeROI)
      }
    
    // Fork detection
    val forkTargets = detectForkTargets(mv, nextPos, color)
    val forkRoles = forkTargets.map(_._2)
    
    def valueOf(r: Role) = r match {
      case Queen => 9
      case Rook => 5
      case Bishop | Knight => 3
      case Pawn => 1
      case King => 100
    }
    
    val attackerVal = valueOf(mv.piece.role)
    val enemyAttackers = nextPos.board.attackers(mv.dest, !color)
    val minEnemyAttackerVal = enemyAttackers.squares.flatMap(sq => nextPos.board.roleAt(sq)).map(valueOf).minOption.getOrElse(100)
    val isSupported = nextPos.board.attackers(mv.dest, color).nonEmpty
    
    val isRealFork = if (enemyAttackers.isEmpty) {
      forkRoles.size >= 2 && forkRoles.exists(_ != Pawn)
    } else if (isSupported) {
      forkRoles.size >= 2 && minEnemyAttackerVal >= attackerVal && forkRoles.exists(r => valueOf(r) > attackerVal)
    } else {
      false
    }
    
    if isRealFork then
      motifs = motifs :+ Motif.Fork(mv.piece.role, forkRoles, mv.dest, forkTargets.map(_._1), color, plyIndex, Some(moveUci))
    detectRemovingTheDefender(mv, pos, nextPos, color, plyIndex, Some(moveUci)).foreach { m =>
      motifs = motifs :+ m
    }
    detectMateNet(nextPos, color, plyIndex, Some(moveUci)).foreach { m =>
      motifs = motifs :+ m
    }
      
    // Pin detection
    detectPin(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }
    
    // Skewer detection  
    detectSkewer(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }
    
    // XRay detection
    detectXRay(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }
    
    // Battery detection (Tactical formation by move)
    // If the move creates a battery, we should flag it here too?
    // Currently Battery is detected as a Structural/Positional motif in detectStateMotifs.
    // We'll leave it there for now to avoid duplication, or check for "Battery Formation".
    
    // Pin / Skewer detection
    detectLineTactics(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Discovered Attack detection
    detectDiscoveredTactics(mv, pos, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Overloading detection
    detectOverloading(nextPos.board, !color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Interference detection
    detectInterference(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Deflection detection
    detectDeflection(mv, pos, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Decoy detection
    detectDecoy(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Clearance detection
    detectClearance(mv, pos, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Trapped Piece detection
    detectTrappedPiece(nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }
    
    // Blockade detection
    detectBlockade(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }

    // Smothered Mate detection
    if (nextPos.check.yes) { 
      detectSmotheredMate(mv, nextPos, color, plyIndex, Some(moveUci)).foreach { m => motifs = motifs :+ m }
    }

    motifs
  }

  private def detectPositionalMotifs(
      mv: Move,
      pos: Position,
      nextPos: Position,
      color: Color,
      moveUci: String,
      plyIndex: Int
  ): List[Motif] = {
    var motifs = List.empty[Motif]
    val role = mv.piece.role

    // 1. Maneuver (Piece Improvement)
    if (!mv.captures && !nextPos.check.yes && role != Pawn && role != King) {
      val mobilityBefore = calculateMobility(pos.board, mv.orig, role)
      val mobilityAfter = calculateMobility(nextPos.board, mv.dest, role)
      
      if (mobilityAfter > mobilityBefore + 2) {
        motifs = motifs :+ Motif.Maneuver(role, ManeuverPurpose.ImprovingScope, color, plyIndex, Some(moveUci))
      } else {
        val isBackRank = if (color.white) mv.orig.rank == chess.Rank.First else mv.orig.rank == chess.Rank.Eighth
        if (role == Knight && !isBackRank && mv.dest.rank != mv.orig.rank && mv.dest.file != mv.orig.file) {
          motifs = motifs :+ Motif.Maneuver(role, ManeuverPurpose.Rerouting, color, plyIndex, Some(moveUci))
        }
      }
    }

    // 2. Domination (Restricting enemy pieces)
    val board = nextPos.board
    val attacks = role match {
      case Knight => mv.dest.knightAttacks
      case Bishop => mv.dest.bishopAttacks(board.occupied)
      case Rook => mv.dest.rookAttacks(board.occupied)
      case Queen => mv.dest.queenAttacks(board.occupied)
      case _ => Bitboard.empty
    }

    val enemyPieces = board.byColor(!color)
    val attackedEnemies = attacks & enemyPieces
    
    attackedEnemies.squares.foreach { targetSq =>
      board.roleAt(targetSq).foreach { targetRole =>
      if (targetRole != Pawn && targetRole != King) {
        val targetMobility = calculateMobility(board, targetSq, targetRole)
        if (targetMobility <= 1) {
          motifs = motifs :+ Motif.Domination(role, targetRole, targetSq, color, plyIndex, Some(moveUci))
        }
      }
      }
    }

    // 3. Rook Behind Passed Pawn (Tarrasch Rule)
    if (role == Rook) {
      val ourPawnsBitboard = nextPos.board.byPiece(color, Pawn)
      val oppPawnsByFile = nextPos.board.byPiece(!color, Pawn).squares.groupBy(_.file)
       
      ourPawnsBitboard.squares.filter(pSq => pSq.file == mv.dest.file).foreach { pSq =>
        if (isPassed(pSq, color, oppPawnsByFile)) {
          val isBehind = if (color.white) mv.dest.rank.value < pSq.rank.value else mv.dest.rank.value > pSq.rank.value
          if (isBehind) {
            motifs = motifs :+ Motif.RookBehindPassedPawn(mv.dest.file, color, plyIndex, Some(moveUci))
          }
        }
      }
    }

    // 4. King Cut-Off (Endgame)
    if (nextPos.board.occupied.count <= 12 && (role == Rook || role == Queen)) {
      nextPos.board.kingPosOf(!color).foreach { kSq =>
        val isCutOffRank = (
          (color.white && mv.dest.rank.value > kSq.rank.value && mv.dest.rank.value <= 4) ||
          (color.black && mv.dest.rank.value < kSq.rank.value && mv.dest.rank.value >= 3)
        )
        val isCutOffFile = (
          (mv.dest.file.value < kSq.file.value && mv.dest.file.value >= 2) ||
          (mv.dest.file.value > kSq.file.value && mv.dest.file.value <= 5)
        )
        
        if (isCutOffRank) {
          motifs = motifs :+ Motif.KingCutOff(KingCutOffAxis.Rank, mv.dest.rank.value + 1, color, plyIndex, Some(moveUci))
        } else if (isCutOffFile) {
          motifs = motifs :+ Motif.KingCutOff(KingCutOffAxis.File, mv.dest.file.value, color, plyIndex, Some(moveUci))
        }
      }
    }

    // 5. Seventh Rank Invasion
    if (role == Rook) {
      val seventhRank = if (color.white) Rank.Seventh else Rank.Second
      if (mv.dest.rank == seventhRank && mv.orig.rank != seventhRank) {
        motifs = motifs :+ SeventhRankInvasion(color, plyIndex, Some(moveUci))
      }
    }

    // 6. Semi-open File Control
    if (role == Rook || role == Queen) {
      val file = mv.dest.file
      val ourPawnsOnFile = nextPos.board.pawns & nextPos.board.byColor(color) & Bitboard.file(file)
      val enemyPawnsOnFile = nextPos.board.pawns & nextPos.board.byColor(!color) & Bitboard.file(file)
      
      if (ourPawnsOnFile.isEmpty && enemyPawnsOnFile.nonEmpty) {
        motifs = motifs :+ SemiOpenFileControl(file, color, plyIndex, Some(moveUci))
      }
    }
    detectInitiative(mv, pos, nextPos, color, plyIndex, Some(moveUci)).foreach { m =>
      motifs = motifs :+ m
    }

    motifs
  }

  // Helper for mobility calculation (simple count of pseudo-legal moves)
  private def calculateMobility(board: Board, square: Square, role: Role): Int = {
    val occupied = board.occupied
    val targets = role match {
      case Knight => square.knightAttacks
      case Bishop => square.bishopAttacks(occupied)
      case Rook => square.rookAttacks(occupied)
      case Queen => square.queenAttacks(occupied)
      case _ => Bitboard.empty
    }
    // Exclude friendly pieces (cannot capture own)
    val friends = board.colorAt(square).map(color => board.byColor(color)).getOrElse(Bitboard.empty)
    (targets & ~friends).count
  }

  private def determineCaptureType(attacker: Role, victim: Role): CaptureType =
    val val1 = MaterialValue.tacticalValueUnit(attacker)
    val val2 = MaterialValue.tacticalValueUnit(victim)
    if val2 > val1 then CaptureType.Winning
    else if val2 == val1 then CaptureType.Exchange
    else CaptureType.Normal // Default to Normal for BxP, Sacrifice requires ROI/context

  private def detectForkTargets(mv: Move, nextPos: Position, color: Color): List[(Square, Role)] =
    val sq = mv.dest
    val role = mv.piece.role
    val board = nextPos.board
    val attacks: Bitboard = role match
      case Pawn   => sq.pawnAttacks(color)
      case Knight => sq.knightAttacks
      case Bishop => sq.bishopAttacks(board.occupied)
      case Rook   => sq.rookAttacks(board.occupied)
      case Queen  => sq.queenAttacks(board.occupied)
      case King   => sq.kingAttacks
    
    val enemyPieces = board.byColor(!color)
    val targets = attacks & enemyPieces
    // A target is meaningful if it is the King, higher value than attacker, or undefended.
    targets.squares.flatMap { targetSq =>
      board.roleAt(targetSq).flatMap { targetRole =>
      val isKing = targetRole == King
      
      // Simple value check
      val attackerVal = if (role == Queen) 9 else if (role == Rook) 5 else if (role == Bishop || role == Knight) 3 else 1
      val targetVal = if (targetRole == Queen) 9 else if (targetRole == Rook) 5 else if (targetRole == Bishop || targetRole == Knight) 3 else 1
      val isHigherValue = targetVal > attackerVal
      
      // Undefended check: Does the opponent (!color) have any pieces defending targetSq?
      val isUndefended = board.attackers(targetSq, !color).isEmpty
      
      if (isKing || isHigherValue || isUndefended) Some((targetSq, targetRole)) else None
      }
    }

  private def detectLineTactics(
      mv: Move,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): List[Motif] =
    val sq = mv.dest
    val role = mv.piece.role
    if (!List(Bishop, Rook, Queen).contains(role)) return Nil

    val board = nextPos.board
    val occupied = board.occupied
    val enemyPieces = board.byColor(!color)
    val targets = role match
      case Bishop => sq.bishopAttacks(occupied) & enemyPieces
      case Rook   => sq.rookAttacks(occupied) & enemyPieces
      case Queen  => sq.queenAttacks(occupied) & enemyPieces
      case _      => Bitboard.empty

    targets.squares.flatMap { targetSq =>
      board.roleAt(targetSq).flatMap { targetRole =>
      val ray = Bitboard.ray(sq, targetSq)
      val occupiedBehind = occupied & ray & ~Bitboard(targetSq) & ~Bitboard(sq)
      
      def chebyshevDist(a: Square, b: Square): Int =
        Math.max((a.file.value - b.file.value).abs, (a.rank.value - b.rank.value).abs)
      val attackerIsSafe = nextPos.board.attackers(sq, !color).isEmpty
      
      if (attackerIsSafe) {
        occupiedBehind.squares.filter(s => Bitboard.between(sq, s).contains(targetSq))
          .minByOption(s => chebyshevDist(sq, s))
          .flatMap { behindSq =>
            board.roleAt(behindSq).flatMap { behindRole =>
              if (board.colorAt(behindSq).contains(!color)) {
                // Trivial pins (pawn to knight etc) shouldn't be reported as tactical 'shots'
                val isSignificantPin = (MaterialValue.tacticalValueUnit(targetRole) < MaterialValue.tacticalValueUnit(behindRole)) && {
                  if (targetRole == Pawn) List(Rook, Queen, King).contains(behindRole)
                  else (MaterialValue.tacticalValueUnit(behindRole) - MaterialValue.tacticalValueUnit(targetRole) >= 2) || behindRole == King
                }
                
                val isSignificantSkewer = (MaterialValue.tacticalValueUnit(targetRole) > MaterialValue.tacticalValueUnit(behindRole)) && {
                  behindRole != Pawn // Skewering a piece to a pawn is rarely a 'skewer' worth mentioning
                }

                if (isSignificantPin)
                  Some(Motif.Pin(role, targetRole, behindRole, color, plyIndex, moveUci, Some(sq), Some(targetSq), Some(behindSq)))
                else if (isSignificantSkewer)
                  Some(Motif.Skewer(role, targetRole, behindRole, color, plyIndex, moveUci, Some(sq), Some(targetSq), Some(behindSq)))
                else None
              } else None
            }
          }
      } else None
      }
    }

  private def detectDiscoveredTactics(
      mv: Move,
      pos: Position,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): List[Motif] =
    val movedPiece = mv.piece.role
    val longRangePieces = pos.board.byColor(color) & (pos.board.bishops | pos.board.rooks | pos.board.queens)
    
    longRangePieces.squares.flatMap { pieceSq =>
      pos.board.roleAt(pieceSq).toList.flatMap { role =>
      val attacksBefore = role match
        case Bishop => pieceSq.bishopAttacks(pos.board.occupied)
        case Rook   => pieceSq.rookAttacks(pos.board.occupied)
        case Queen  => pieceSq.queenAttacks(pos.board.occupied)
        case _      => Bitboard.empty
        
      val attacksAfter = role match
        case Bishop => pieceSq.bishopAttacks(nextPos.board.occupied)
        case Rook   => pieceSq.rookAttacks(nextPos.board.occupied)
        case Queen  => pieceSq.queenAttacks(nextPos.board.occupied)
        case _      => Bitboard.empty
      
      val newTargets = (attacksAfter & ~attacksBefore) & nextPos.board.byColor(!color)
      
      newTargets.squares.flatMap { targetSq =>
        nextPos.board.roleAt(targetSq).flatMap { targetRole =>
          val isKing = targetRole == King
          val attackerVal = MaterialValue.tacticalValueUnit(role)
          val targetVal = MaterialValue.tacticalValueUnit(targetRole)
          val isHigherValue = targetVal > attackerVal
          val isUndefended = nextPos.board.attackers(targetSq, !color).isEmpty
          val attackerIsSafe = nextPos.board.attackers(pieceSq, !color).isEmpty

          if (isKing || ((isHigherValue || isUndefended) && attackerIsSafe)) {
            Some(Motif.DiscoveredAttack(movedPiece, role, targetRole, color, plyIndex, moveUci, Some(mv.dest), Some(pieceSq), Some(targetSq)))
          } else None
        }
      }
      }
    }

  private def detectOverloading(
      board: Board, 
      color: Color, 
      plyIndex: Int, 
      moveUci: Option[String]
  ): List[Motif] =
    val criticalSquares = (board.byColor(color) & ~board.kings).squares.filter { sq =>
      board.roleAt(sq).exists { targetRole =>
        board.attackers(sq, !color).squares.exists { attackerSq =>
          board.roleAt(attackerSq).exists { attackerRole =>
            val isUndefended = board.attackers(sq, color).isEmpty
            val isHigherValue = MaterialValue.tacticalValueUnit(targetRole) > MaterialValue.tacticalValueUnit(attackerRole)
            val attackerIsSafe = board.attackers(attackerSq, color).isEmpty

            isHigherValue || (isUndefended && attackerIsSafe)
          }
        }
      }
    }

    val duties = scala.collection.mutable.Map[Square, List[Square]]()

    criticalSquares.foreach { defendedSq =>
      val defenders = board.attackers(defendedSq, color)
      defenders.squares.foreach { defenderSq =>
        if defenderSq != defendedSq then
          duties.update(defenderSq, defendedSq :: duties.getOrElse(defenderSq, Nil))
      }
    }

    duties.flatMap { case (defenderSq, tasks) =>
      if tasks.size >= 2 then
        board.roleAt(defenderSq).map(role => Motif.Overloading(role, defenderSq, tasks, color, plyIndex, moveUci))
      else None
    }.toList

  private def detectInterference(
      mv: Move, 
      nextPos: Position, 
      color: Color, 
      plyIndex: Int, 
      moveUci: Option[String]
  ): List[Motif] =
    val blockerSq = mv.dest
    val board = nextPos.board
    val oppColor = !color
    
    val oppDefenders = board.byColor(oppColor) & (board.rooks | board.bishops | board.queens)
    val blockedDefenses = List.newBuilder[Motif]
    
    oppDefenders.squares.foreach { defenderSq =>
      board.roleAt(defenderSq).foreach { role =>
      val opponentPieces = board.byColor(oppColor) & ~Bitboard(defenderSq)
      val attackerIsSafe = nextPos.board.attackers(blockerSq, !color).isEmpty
      
      if (attackerIsSafe) {
        opponentPieces.squares.foreach { targetSq =>
          if (Bitboard.between(defenderSq, targetSq).contains(blockerSq)) {
            board.roleAt(targetSq).foreach { targetRole =>
              blockedDefenses += Motif.Interference(
                 interferingPiece = mv.piece.role,
                 interferingSquare = blockerSq,
                 blockedPiece1 = role,
                 blockedPiece2 = targetRole,
                 color = color,
                 plyIndex = plyIndex,
                 move = moveUci
              )
            }
          }
        }
      }
      }
    }
    
    blockedDefenses.result()

  private def detectTrappedPiece(
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): List[Motif] =
    val board = nextPos.board
    val enemyPieces = board.byColor(!color)
    val trapped = List.newBuilder[Motif]
    
    val targetSquares = (enemyPieces & (board.queens | board.rooks | board.bishops | board.knights)).squares
    
    targetSquares.foreach { sq =>
      board.roleAt(sq).foreach { role =>
      // Must be attacked by us
      if (board.attackers(sq, color).nonEmpty) {
        val mobility = calculateMobility(board, sq, role)
        if (mobility == 0) {
           // Trapped!
           trapped += Motif.TrappedPiece(role, sq, !color, plyIndex, moveUci)
        }
      }
      }
    }
    trapped.result()

  private def detectBlockade(mv: Move, nextPos: Position, color: Color, plyIndex: Int, moveUci: Option[String]): Option[Motif] =
    val board = nextPos.board
    // 1. Must be a piece blocking an ENEMY pawn
    val rankVal = mv.dest.rank.value
    val enemyPawnRankVal = if (color.white) rankVal + 1 else rankVal - 1
    
    if (enemyPawnRankVal < 0 || enemyPawnRankVal > 7) return None

    val enemyPawnSqOpt = Square.at(mv.dest.file.value, enemyPawnRankVal)
    
    enemyPawnSqOpt.flatMap { pSq =>
       val pieceBehind = board.roleAt(pSq)
       val colorBehind = board.colorAt(pSq)
       
       if (pieceBehind.contains(Pawn) && colorBehind.contains(!color)) {
         if (Set(Knight, Bishop, Rook, King).contains(mv.piece.role)) {
            Some(Motif.Blockade(mv.piece.role, mv.dest, pSq, color, plyIndex, moveUci))
         } else None
       } else None
    }

  private def detectSmotheredMate(mv: Move, nextPos: Position, color: Color, plyIndex: Int, moveUci: Option[String]): Option[Motif] =
    // 1. Must be a Knight delivering mate
    if (nextPos.checkMate && mv.piece.role == Knight) {
      val board = nextPos.board
      val enemyKing = board.kingPosOf(!color)
      
      enemyKing.flatMap { kSq =>
        // 2. Check King's valid moves (0, because it's mate)
        // 3. Check if squares around King are occupied by FRIENDS
        val neighbors = kSq.kingAttacks
        
        // Smothered if majority of escape squares are blocked by friends
        // Heuristic: If King has no moves (mate) and > 0 neighbors are blocked by friends, and knight checks...
        // Strictly: Smothered means ALL flight squares are blocked by own pieces.
        // But in corner it might be fewer.
        // Let's check if all accessible neighbors are occupied by own pieces.
        val onBoardNeighbors = neighbors
        val allBlockedByFriends = (onBoardNeighbors & ~board.byColor(!color)).isEmpty 
        // Also check if Knight is safe from capture (simplified)
        val isKnightSafe = board.attackers(mv.dest, !color).isEmpty

        if (allBlockedByFriends && isKnightSafe) Some(Motif.SmotheredMate(color, kSq, plyIndex, moveUci))
        else None
      }
    } else None

  def detectStateMotifs(pos: Position, plyIndex: Int): List[Motif] =
    val board = pos.board
    List.concat(
      detectPawnStructure(board, plyIndex),
      detectPins(board, plyIndex),
      detectOpposition(board, pos.color, plyIndex),
      detectOpenFiles(board, plyIndex),
      detectWeakBackRank(pos, plyIndex),
      detectMateInOneThreats(pos, plyIndex),
      detectSpaceAdvantage(board, plyIndex),
      detectBattery(board, plyIndex),
      detectImbalance(board, plyIndex) // Knight vs Bishop
    )

  private def detectMateInOneThreats(pos: Position, plyIndex: Int): List[Motif] =
    if pos.checkMate then Nil
    else
      List(White, Black).flatMap { color =>
        val activePosition = if pos.color == color then pos else pos.withColor(color)
        if activePosition.check.yes then None
        else
          activePosition.legalMoves.find(_.after.checkMate).flatMap { move =>
            move.after.board.kingPosOf(!color).map { kingSq =>
              Motif.Check(move.piece.role, kingSq, CheckType.Mate, color, plyIndex, Some(move.toUci.uci))
            }
          }
      }

  private def detectPins(board: Board, plyIndex: Int): List[Motif] =
    // Static absolute pins (to king) for bishops, rooks, and queens.
    // Keep conservative to avoid noise: only detect pins to the king.
    val motifs = List.newBuilder[Motif]

    def addRayPins(from: Square, role: Role, attacker: Color, deltas: List[(Int, Int)]): Unit =
      deltas.foreach { (df, dr) =>
        var f = from.file.value + df
        var r = from.rank.value + dr
        var pinned: Option[(Square, Role)] = None

        def stopRay(): Unit =
          f = 99
          r = 99

        while (f >= 0 && f <= 7 && r >= 0 && r <= 7) do
          Square.at(f, r) match
            case None => stopRay()
            case Some(sq) =>
              (board.colorAt(sq), board.roleAt(sq)) match
                case (None, _) =>
                  f += df; r += dr
                case (Some(hitColor), _) if hitColor == attacker =>
                  stopRay()
                case (Some(_), Some(hitRole)) =>
                  pinned match
                    case None =>
                      pinned = Some((sq, hitRole))
                      f += df; r += dr
                    case Some((pinnedSq, pinnedRole)) =>
                      if (hitRole == King) then
                        motifs += Motif.Pin(
                          pinningPiece = role,
                          pinnedPiece = pinnedRole,
                          targetBehind = King,
                          color = attacker,
                          plyIndex = plyIndex,
                          move = None,
                          pinningSq = Some(from),
                          pinnedSq = Some(pinnedSq),
                          behindSq = Some(sq)
                        )
                      stopRay()
                case _ => stopRay()
      }

    for attacker <- List(White, Black) do
      (board.bishops & board.byColor(attacker)).squares.foreach { from =>
        addRayPins(from, Bishop, attacker, List((1, 1), (1, -1), (-1, 1), (-1, -1)))
      }
      (board.rooks & board.byColor(attacker)).squares.foreach { from =>
        addRayPins(from, Rook, attacker, List((1, 0), (-1, 0), (0, 1), (0, -1)))
      }
      (board.queens & board.byColor(attacker)).squares.foreach { from =>
        addRayPins(from, Queen, attacker, List((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)))
      }

    motifs.result().distinct
  private def detectOpenFiles(board: Board, plyIndex: Int): List[Motif] =
    var motifs = List.empty[Motif]
    for file <- File.all do
      val whitePawns = board.byPiece(White, Pawn) & Bitboard.file(file)
      val blackPawns = board.byPiece(Black, Pawn) & Bitboard.file(file)
      
      if whitePawns.isEmpty && blackPawns.isEmpty then
        // Open file - check who controls it
        val whiteRooks = (board.byPiece(White, Rook) | board.byPiece(White, Queen)) & Bitboard.file(file)
        val blackRooks = (board.byPiece(Black, Rook) | board.byPiece(Black, Queen)) & Bitboard.file(file)
        
        if whiteRooks.nonEmpty && blackRooks.isEmpty then
          motifs = motifs :+ OpenFileControl(file, White, plyIndex)
        else if blackRooks.nonEmpty && whiteRooks.isEmpty then
          motifs = motifs :+ OpenFileControl(file, Black, plyIndex)
    motifs

  private def detectImbalance(board: Board, plyIndex: Int): List[Motif] =
    var motifs = List.empty[Motif]
    
    // Check for Knight vs Bishop specific imbalance (e.g. White has Knight, Black has Bishop)
    // We only care if roles differ significantly
    val wKnights = board.byPiece(White, Knight).count
    val wBishops = board.byPiece(White, Bishop).count
    val bKnights = board.byPiece(Black, Knight).count
    val bBishops = board.byPiece(Black, Bishop).count
    
    // Scenario 1: White has Knight, Black has Bishop (and no Knight vs no Bishop or similar)
    // Simplified: Look for cases where one side has a Knight and NO Bishop, and other has Bishop and NO Knight (1 vs 1 minor piece endgame/middlegame)
    val whiteHasKnightOnly = wKnights > 0 && wBishops == 0
    val blackHasBishopOnly = bBishops > 0 && bKnights == 0
    
    val blackHasKnightOnly = bKnights > 0 && bBishops == 0
    val whiteHasBishopOnly = wBishops > 0 && wKnights == 0
    
    if ((whiteHasKnightOnly && blackHasBishopOnly) || (blackHasKnightOnly && whiteHasBishopOnly)) {
       // Determine openness
       val pawnCount = board.pawns.count
       val isClosed = pawnCount >= 10 // Heuristic
       
       if (whiteHasKnightOnly) {
         val isKnightBetter = isClosed
         motifs = motifs :+ Motif.KnightVsBishop(White, isKnightBetter, plyIndex)
       } else {
         val isKnightBetter = isClosed
         motifs = motifs :+ Motif.KnightVsBishop(Black, isKnightBetter, plyIndex)
       }
    }
    
    motifs


  private def detectWeakBackRank(pos: Position, plyIndex: Int): List[Motif] =
    var motifs = List.empty[Motif]
    val board = pos.board
    
    for color <- List(White, Black) do
      val backRank = if color.white then Rank.First else Rank.Eighth
      board.kingPosOf(color).foreach { kingSq =>
        if kingSq.rank == backRank then
          // Check if king is trapped on back rank (no escape squares)
          val escapeRank = if color.white then Rank.Second else Rank.Seventh
          val escapeSquares = kingSq.kingAttacks & Bitboard.rank(escapeRank)
          val blockedByOwn = escapeSquares & board.byColor(color)
          
          if escapeSquares.count > 0 && blockedByOwn.count == escapeSquares.count then
            motifs = motifs :+ WeakBackRank(color, plyIndex)
      }
    motifs

  private def detectSpaceAdvantage(board: Board, plyIndex: Int): List[Motif] =
    // Count pawns on rank 4/5 for White, rank 5/4 for Black
    val whiteCentralPawns = (board.byPiece(White, Pawn) & (Bitboard.rank(Rank.Fourth) | Bitboard.rank(Rank.Fifth))).count
    val blackCentralPawns = (board.byPiece(Black, Pawn) & (Bitboard.rank(Rank.Fourth) | Bitboard.rank(Rank.Fifth))).count
    
    val diff = whiteCentralPawns - blackCentralPawns
    if diff >= 2 then List(SpaceAdvantage(White, diff, plyIndex))
    else if diff <= -2 then List(SpaceAdvantage(Black, -diff, plyIndex))
    else Nil

  private def detectPawnStructure(board: Board, plyIndex: Int): List[Motif] =
    var motifs = List.empty[Motif]
    for color <- List(White, Black) do
      val pawns = board.byPiece(color, Pawn)
      val oppPawns = board.byPiece(!color, Pawn)
      val pawnsByFile = pawns.squares.groupBy(_.file)
      val oppPawnsByFile = oppPawns.squares.groupBy(_.file)
      pawns.foreach { pawnSq =>
        if isIsolated(pawnSq, pawnsByFile) then 
          motifs = motifs :+ IsolatedPawn(pawnSq.file, pawnSq.rank.value + 1, color, plyIndex)
        if isPassed(pawnSq, color, oppPawnsByFile) then 
          motifs = motifs :+ PassedPawn(pawnSq.file, pawnSq.rank.value + 1, color, isProtected = false, plyIndex)
        if isBackward(pawnSq, color, pawnsByFile, oppPawnsByFile) then
          motifs = motifs :+ BackwardPawn(pawnSq.file, pawnSq.rank.value + 1, color, plyIndex)
      }

      pawnsByFile.foreach { (file, sqs) =>
        if sqs.size >= 2 then motifs = motifs :+ Motif.DoubledPawns(file, color, plyIndex)
      }

      val chainPairs = scala.collection.mutable.Set[(File, File)]()
      pawns.squares.foreach { baseSq =>
        val supported = baseSq.pawnAttacks(color) & pawns
        supported.squares.foreach { tipSq =>
          val (base, tip) =
            if (color.white && baseSq.rank.value <= tipSq.rank.value) (baseSq, tipSq)
            else if (!color.white && baseSq.rank.value >= tipSq.rank.value) (baseSq, tipSq)
            else (tipSq, baseSq)
          chainPairs += ((base.file, tip.file))
        }
      }
      chainPairs.foreach { case (baseFile, tipFile) =>
        motifs = motifs :+ Motif.PawnChain(baseFile, tipFile, color, plyIndex)
      }
    motifs

  private def isIsolated(pawnSq: Square, pawnsByFile: Map[File, List[Square]]): Boolean =
    val fileValue = pawnSq.file.value
    val adjacentFiles = List(fileValue - 1, fileValue + 1).filter(f => f >= 0 && f <= 7)
    !adjacentFiles.exists { adjFileIdx => 
      File.all.lift(adjFileIdx).exists(pawnsByFile.contains)
    }

  private def isPassed(pawnSq: Square, color: Color, oppPawnsByFile: Map[File, List[Square]]): Boolean =
    val fileValue = pawnSq.file.value
    val filesToCheck = List(fileValue - 1, fileValue, fileValue + 1).filter(f => f >= 0 && f <= 7)
    filesToCheck.forall { fIdx =>
      File.all.lift(fIdx).forall { f =>
        oppPawnsByFile.get(f).forall { oppPawns =>
          oppPawns.forall { oppPawn =>
            if color.white then oppPawn.rank.value <= pawnSq.rank.value
            else oppPawn.rank.value >= pawnSq.rank.value
          }
        }
      }
    }

  private def isBackward(
      pawnSq: Square,
      color: Color,
      pawnsByFile: Map[File, List[Square]],
      oppPawnsByFile: Map[File, List[Square]]
  ): Boolean =
    // Minimal heuristic:
    // - no adjacent friendly pawn can support later
    // - and the pawn is blocked from advancing by enemy pawn control on its advance square
    val rank = pawnSq.rank.value
    val fileValue = pawnSq.file.value
    val adjacentFiles = List(fileValue - 1, fileValue + 1).filter(f => f >= 0 && f <= 7)

    val canBeSupportedLater = adjacentFiles.exists { adjFileIdx =>
      File.all.lift(adjFileIdx).exists { adjFile =>
        pawnsByFile.get(adjFile).exists(_.exists { supportSq =>
          if (color.white) supportSq.rank.value <= rank else supportSq.rank.value >= rank
        })
      }
    }

    if (canBeSupportedLater) false
    else
      val advanceSqOpt = Square.at(pawnSq.file.value, if (color.white) pawnSq.rank.value + 1 else pawnSq.rank.value - 1)
      advanceSqOpt.exists { advanceSq =>
        // If enemy pawns control the advance square, it's a more realistic backward pawn.
        val enemyPawnAttacks = oppPawnsByFile.values.flatten.foldLeft(Bitboard.empty) { (bb, oppPawnSq) =>
          bb | oppPawnSq.pawnAttacks(!color)
        }
        enemyPawnAttacks.contains(advanceSq)
      }

  private def detectOpposition(board: Board, color: Color, plyIndex: Int): List[Motif] =
    (board.kingPosOf(White), board.kingPosOf(Black)) match
      case (Some(wk), Some(bk)) =>
        val fDiff = (wk.file.value - bk.file.value).abs
        val rDiff = (wk.rank.value - bk.rank.value).abs
        // Side with opposition is the one NOT whose turn it is.
        // If it's White's turn (color == White) and opposition exists, Black has the opposition.
        val sideWithOpposition = !color
        if fDiff == 0 && rDiff == 2 then List(Opposition(bk, wk, OppositionType.Direct, sideWithOpposition, plyIndex))
        else if rDiff == 0 && fDiff == 2 then List(Opposition(bk, wk, OppositionType.Direct, sideWithOpposition, plyIndex))
        else if (fDiff == 0 && (rDiff == 4 || rDiff == 6)) || (rDiff == 0 && (fDiff == 4 || fDiff == 6)) then
          List(Opposition(bk, wk, OppositionType.Distant, sideWithOpposition, plyIndex))
        else if fDiff == 2 && rDiff == 2 then
          List(Opposition(bk, wk, OppositionType.Diagonal, sideWithOpposition, plyIndex))
        else Nil
      case _ => Nil

  private def isSmotheredPattern(pos: Position, color: Color): Boolean =
    pos.board.kingPosOf(!color).exists { kingSq =>
      val attackers = pos.board.attackers(kingSq, color)
      val isKnightCheck = attackers.exists(sq => pos.board.roleAt(sq).contains(Knight))
      val adjacent = kingSq.kingAttacks
      val blocked = adjacent & pos.board.byColor(!color)
      isKnightCheck && (blocked == adjacent)
    }

  /**
   * Detect compensation for Exchange Sacrifice (Rook for minor piece).
   * Returns SacrificeROI if positional compensation exists.
   */
  private def detectExchangeSacrificeROI(mv: Move, nextPos: Position, color: Color): Option[SacrificeROI] = {
    val board = nextPos.board
    var reasons = List.empty[SacrificeReason]
    var totalValue = 0
    
    // 1. Open file created for remaining rook?
    val capturedFile = mv.dest.file
    val ownPawnsOnFile = (board.pawns & board.byColor(color)).squares.count(_.file == capturedFile)
    val enemyPawnsOnFile = (board.pawns & board.byColor(!color)).squares.count(_.file == capturedFile)
    if (ownPawnsOnFile == 0) {
      val hasRookOnFile = (board.rooks & board.byColor(color)).squares.exists(_.file == capturedFile)
      if (hasRookOnFile || enemyPawnsOnFile == 0) {
        reasons = reasons :+ SacrificeReason.OpenFile
        totalValue += 150
      }
    }
    
    // 2. King exposed (weakened shelter)?
    val enemyKingPos = board.kingPosOf(!color)
    enemyKingPos.foreach { kingSq =>
      // Check if king is on back rank with reduced pawn cover
      val kingFile = kingSq.file
      val kingFileIdx = kingFile.value
      val adjacentFileIndices = List(kingFileIdx - 1, kingFileIdx, kingFileIdx + 1).filter(i => i >= 0 && i <= 7)
      val adjacentFiles = adjacentFileIndices.flatMap(i => File.all.lift(i))
      val pawnShieldCount = adjacentFiles.count { f =>
        (board.pawns & board.byColor(!color)).squares.exists(sq => 
          sq.file == f && (if (!color).white then sq.rank.value >= kingSq.rank.value else sq.rank.value <= kingSq.rank.value)
        )
      }
      if (pawnShieldCount <= 1) {
        reasons = reasons :+ SacrificeReason.KingExposed
        totalValue += 200
      }
    }
    
    // 3. Piece activity gain (bishop pair, dominant piece, battery)?
    val ownBishops = (board.bishops & board.byColor(color)).count
    if (ownBishops >= 2) {
      reasons = reasons :+ SacrificeReason.BishopPair
      totalValue += 50
    }
    
    val hasBattery = detectBattery(board, 0).exists(b => b.color == color && (b.frontSq.exists(_ == mv.dest) || b.backSq.exists(_ == mv.dest)))
    if (hasBattery) {
      reasons = reasons :+ SacrificeReason.PieceActivity
      totalValue += 100
    }

    // 4. Passed pawn created or supported?
    val ourPawns = board.byPiece(color, Pawn).squares
    val enemyPawnsByFile = board.byPiece(!color, Pawn).squares.groupBy(_.file)
    val hasPassedPawn = ourPawns.exists(p => isPassed(p, color, enemyPawnsByFile))
    if (hasPassedPawn) {
      reasons = reasons :+ SacrificeReason.PassedPawn
      totalValue += 150
    }
    
    // Only return ROI if there's meaningful compensation (tightened threshold)
    if totalValue >= 200 || (reasons.size >= 2 && totalValue >= 100) then 
      Some(SacrificeROI(reasons, totalValue))
    else None
  }

  /**
   * Detect Deflection - forcing a defending piece away from its duties.
   * Example: Attacking a piece that defends the king or a valuable target.
   */
  private def detectDeflection(
      mv: Move,
      pos: Position,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): Option[Motif.Deflection] = {
    val opponent = !color
    val board = pos.board
    
    // The moved piece attacks enemy pieces in next position
    val attackedSquares = mv.dest match {
      case dest =>
        val role = mv.piece.role
        role match {
          case Pawn   => dest.pawnAttacks(color)
          case Knight => dest.knightAttacks
          case Bishop => dest.bishopAttacks(nextPos.board.occupied)
          case Rook   => dest.rookAttacks(nextPos.board.occupied)
          case Queen  => dest.queenAttacks(nextPos.board.occupied)
          case King   => dest.kingAttacks
        }
    }
    val attackedEnemies = attackedSquares & board.byColor(opponent)
    
    // For each attacked enemy piece, check if it's defending something important
    attackedEnemies.squares.flatMap { defenderSq =>
      board.roleAt(defenderSq).flatMap { defenderRole =>
        // What does this defender protect?
        val defenderAttacks = defenderRole match {
          case Pawn   => defenderSq.pawnAttacks(opponent)
          case Knight => defenderSq.knightAttacks
          case Bishop => defenderSq.bishopAttacks(board.occupied)
          case Rook   => defenderSq.rookAttacks(board.occupied)
          case Queen  => defenderSq.queenAttacks(board.occupied)
          case King   => defenderSq.kingAttacks
        }
        val protectedFriendlies = (defenderAttacks & board.byColor(opponent)).squares
        
        // Is it protecting something more valuable than itself?
        val protectsValuable = protectedFriendlies.exists { sq =>
          board.roleAt(sq).exists(r => MaterialValue.tacticalValueUnit(r) >= MaterialValue.tacticalValueUnit(defenderRole))
        }
        
        if (protectsValuable) {
          // Wait, the deflection attack comes from mv.dest. We want to know if the piece at mv.dest is safe.
          val destIsSafe = nextPos.board.attackers(mv.dest, !color).isEmpty

          if (destIsSafe) {
            Some(Motif.Deflection(defenderRole, defenderSq, opponent, plyIndex, moveUci))
          } else None
        } else None
      }
    }.headOption
  }

  /**
   * Detect Decoy - luring a piece to a vulnerable square.
   * Example: Sacrifice to force king/piece to a fork or check square.
   */
  private def detectDecoy(
      mv: Move,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): Option[Motif.Decoy] = {
    // For decoy, we typically sacrifice to lure a piece
    // Check if we're offering our piece (capture available for opponent)
    val isOffering = nextPos.legalMoves.exists { oppMv =>
      oppMv.captures && oppMv.dest == mv.dest
    }
    
    if (!isOffering) return None
    
    // Simulate opponent capture on our piece
    val captureScenarios = nextPos.legalMoves.filter(m => m.captures && m.dest == mv.dest)
    
    captureScenarios.flatMap { oppCapture =>
      val afterCaptureOpt = nextPos.move(oppCapture.toUci.asInstanceOf[Uci.Move]).toOption
      
      afterCaptureOpt.flatMap { afterCapture =>
        val afterPos = afterCapture.after
        val hasExecution = afterPos.legalMoves.exists { ourResponse =>
          val givesMate = ourResponse.after.checkMate
          val isWinningCapture =
            ourResponse.captures &&
              afterPos.board.roleAt(ourResponse.dest).exists(victim =>
                MaterialValue.tacticalValueUnit(victim) > MaterialValue.tacticalValueUnit(ourResponse.piece.role)
              )
          val forkTargets = detectForkTargets(ourResponse, ourResponse.after, color)
          val forkRoles = forkTargets.map(_._2)
          val hasRealFork = (forkRoles.contains(King) && forkRoles.size >= 2) || (forkRoles.filter(_ != Pawn).size >= 2)

          givesMate || isWinningCapture || hasRealFork
        }
        
        if (hasExecution) {
          Some(Motif.Decoy(oppCapture.piece.role, mv.dest, !color, plyIndex, moveUci))
        } else None
      }
    }.headOption
  }

  private type Dir = Square => Option[Square]
  private def getSquare(f: Int, r: Int): Option[Square] = 
    if (f < 0 || f > 7 || r < 0 || r > 7) None
    else Square.fromKey(s"${('a' + f).toChar}${('1' + r).toChar}")

  private val bishopDirs: List[Dir] = List(
    s => getSquare(s.file.value - 1, s.rank.value + 1),
    s => getSquare(s.file.value + 1, s.rank.value + 1),
    s => getSquare(s.file.value - 1, s.rank.value - 1),
    s => getSquare(s.file.value + 1, s.rank.value - 1)
  )
  private val rookDirs: List[Dir] = List(
    s => getSquare(s.file.value, s.rank.value + 1),
    s => getSquare(s.file.value, s.rank.value - 1),
    s => getSquare(s.file.value - 1, s.rank.value),
    s => getSquare(s.file.value + 1, s.rank.value)
  )
  private val queenDirs: List[Dir] = bishopDirs ++ rookDirs

  private def detectPin(mv: Move, nextPos: Position, color: Color, plyIndex: Int, moveUci: Option[String]): List[Motif.Pin] = {
    val board = nextPos.board
    val moverRole = mv.piece.role
    if (!List(Bishop, Rook, Queen).contains(moverRole)) return Nil

    val directions = moverRole match {
      case Bishop => bishopDirs
      case Rook => rookDirs
      case Queen => queenDirs
      case _ => Nil
    }

    directions.flatMap { dir =>
      // Ray casting: Find first piece, then second piece
      // 1. First piece must be Enemy
      // 2. Second piece must be Enemy (and valuable)
      
      def trace(sq: Square): Option[(Square, Role, Square, Role)] = {
        var current = sq
        var first: Option[(Square, Role)] = None
        
        while (true) {
          dir(current) match {
            case Some(next) =>
              current = next
              board.roleAt(current) match {
                case Some(role) =>
                  val roleColor = board.colorAt(current).get
                  if (first.isEmpty) {
                    if (roleColor != color) first = Some((current, role))
                    else return None // Blocked by friendly
                  } else {
                    // We found the second piece
                    if (roleColor != color) {
                      return Some((first.get._1, first.get._2, current, role))
                    } else return None // Blocked by friendly behind
                  }
                case None => // Empty square, continue
              }
            case None => return None // End of board
          }
        }
        None
      }
      
      trace(mv.dest).flatMap { case (pinnedSq, pinnedRole, behindSq, behindRole) =>
        if (MaterialValue.tacticalValueUnit(behindRole) > MaterialValue.tacticalValueUnit(pinnedRole) || behindRole == King) {
          Some(Motif.Pin(
            pinningPiece = moverRole,
            pinnedPiece = pinnedRole,
            targetBehind = behindRole,
            color = color,
            plyIndex = plyIndex,
            move = moveUci,
            pinningSq = Some(mv.dest),
            pinnedSq = Some(pinnedSq),
            behindSq = Some(behindSq)
          ))
        } else None
      }
    }
  }

  private def detectSkewer(mv: Move, nextPos: Position, color: Color, plyIndex: Int, moveUci: Option[String]): List[Motif.Skewer] = {
    val board = nextPos.board
    val moverRole = mv.piece.role
    if (!List(Bishop, Rook, Queen).contains(moverRole)) return Nil

    // Same ray casting logic as Pin, but value check is inverted
    val directions = moverRole match {
      case Bishop => bishopDirs
      case Rook => rookDirs
      case Queen => queenDirs
      case _ => Nil
    }
    
    directions.flatMap { dir =>
      def trace(sq: Square): Option[(Square, Role, Square, Role)] = {
        var current = sq
        var first: Option[(Square, Role)] = None
        while(true) {
          dir(current) match {
            case Some(next) =>
              current = next
              board.roleAt(current) match {
                case Some(role) =>
                  val roleColor = board.colorAt(current).get
                  if (first.isEmpty) {
                    if (roleColor != color) first = Some((current, role))
                    else return None 
                  } else {
                    if (roleColor != color) return Some((first.get._1, first.get._2, current, role))
                    else return None
                  }
                case None => 
              }
            case None => return None
          }
        }
        None
      }
      
      trace(mv.dest).flatMap { case (frontSq, frontRole, backSq, backRole) =>
        // Skewer: Front piece is MORE valuable than Back piece (or is King)
        // And Front piece must be able to move (escape)
        if (MaterialValue.tacticalValueUnit(frontRole) > MaterialValue.tacticalValueUnit(backRole) || frontRole == King) {
           Some(Motif.Skewer(
             attackingPiece = moverRole,
             frontPiece = frontRole,
             backPiece = backRole,
             color = color,
             plyIndex = plyIndex,
             move = moveUci,
             attackingSq = Some(mv.dest),
             frontSq = Some(frontSq),
             backSq = Some(backSq)
           ))
        } else None
      }
    }
  }

  private def detectXRay(mv: Move, nextPos: Position, color: Color, plyIndex: Int, moveUci: Option[String]): List[Motif.XRay] = {
    // X-Ray Attack: Attacking a square/piece THROUGH another piece.
    // Common case: Rooks supporting each other, or attacking enemy through enemy.
    // We'll focus on X-Ray ATTACK: Aiming at a piece behind a blocker on the same line.
    // This is similar to Pin/Skewer but implies the "presence" of the attack on the rear piece.
    
    // Implementation: Same ray cast. If aligned, it's an X-Ray.
    // Difference from Pin: Pin focuses on the immobility of the middle piece.
    // Difference from Skewer: Skewer forces the front piece to move.
    // X-Ray is the generic "looking through".
    // We'll detect it if we see two pieces on a line, regardless of values.
    // But to avoid noise, let's limit to:
    // 1. Attacking through FRIENDLY piece (Support)
    // 2. Attacking through ENEMY piece (Pressure)
    
    val board = nextPos.board
    val moverRole = mv.piece.role
    if (!List(Bishop, Rook, Queen).contains(moverRole)) return Nil

    val directions = moverRole match {
      case Bishop => bishopDirs
      case Rook => rookDirs
      case Queen => queenDirs
      case _ => Nil
    }
    
    directions.flatMap { dir =>
      def trace(sq: Square): Option[(Square, Role, Square, Role, Color)] = {
        var current = sq
        var first: Option[(Square, Role, Color)] = None
        while(true) {
          dir(current) match {
            case Some(next) =>
              current = next
              board.roleAt(current) match {
                case Some(role) =>
                  val roleColor = board.colorAt(current).get
                  if (first.isEmpty) {
                    first = Some((current, role, roleColor))
                  } else {
                    // Second piece found
                    return Some((first.get._1, first.get._2, current, role, first.get._3))
                  }
                case None =>
              }
            case None => return None
          }
        }
        None
      }
      
      trace(mv.dest).flatMap { case (_, _, rearSq, rearRole, midColor) =>
        // Only interesting if targeting enemy or supporting friend
        val rearColor = board.colorAt(rearSq).get
        
        // Case A: Attacking enemy through enemy (Pressure/Pin/Skewer latent)
        // Case B: Attacking enemy through friend (Discovery prep)
        // Case C: Defending friend through friend (Battery/Support)
        // Case D: Defending friend through enemy (X-Ray Defense)
        
        // Let's filter for "X-Ray Attack" -> Rear is Enemy. Mid is Enemy
        if (rearColor != color && midColor != color) {
           Some(Motif.XRay(moverRole, rearRole, rearSq, color, plyIndex, moveUci))
        } else None
      }
    }
  }

  /**
   * Detect Clearance - moving a piece to clear a line/square for another piece.
   * Example: Moving a rook so that the queen can use the file.
   */
  private def detectClearance(
      mv: Move,
      pos: Position,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): Option[Motif.Clearance] = {
    val board = pos.board
    val nextBoard = nextPos.board
    val origSq = mv.orig
    val destSq = mv.dest
    val movedRole = mv.piece.role
    
    // Only detect for long-range pieces clearing lines
    if (!List(Rook, Queen, Bishop).contains(movedRole)) return None
    
    // Check if a friendly piece now has access to a line that was blocked
    val friendlyLongRange = (nextBoard.byColor(color) & 
      (nextBoard.rooks | nextBoard.bishops | nextBoard.queens)) &
      ~Bitboard(destSq)
    
    friendlyLongRange.squares.flatMap { friendlySq =>
      nextBoard.roleAt(friendlySq).toList.flatMap { friendlyRole =>
      
      // Check if this piece can now attack through where the moved piece was
      val attacksAfter = friendlyRole match {
        case Bishop => friendlySq.bishopAttacks(nextBoard.occupied)
        case Rook   => friendlySq.rookAttacks(nextBoard.occupied)
        case Queen  => friendlySq.queenAttacks(nextBoard.occupied)
        case _      => Bitboard.empty
      }
      
      val attacksBefore = friendlyRole match {
        case Bishop => friendlySq.bishopAttacks(board.occupied)
        case Rook   => friendlySq.rookAttacks(board.occupied)
        case Queen  => friendlySq.queenAttacks(board.occupied)
        case _      => Bitboard.empty
      }
      
      // New attack squares = squares accessible after but not before
      val newAttacks = attacksAfter & ~attacksBefore
      val ray = Bitboard.ray(friendlySq, origSq)
      val targets = newAttacks & nextBoard.byColor(!color)
      val meaningfulTarget = targets.squares.exists { sq =>
        nextBoard.roleAt(sq).exists { targetRole =>
          targetRole == King || MaterialValue.tacticalValueUnit(targetRole) > MaterialValue.tacticalValueUnit(friendlyRole) || nextBoard.attackers(sq, !color).isEmpty
        }
      }
      val clearedRay = ray.nonEmpty && meaningfulTarget
      
      if (clearedRay) {
        val clearanceType = 
          if (friendlySq.file == origSq.file) Motif.ClearanceType.File
          else if (friendlySq.rank == origSq.rank) Motif.ClearanceType.Rank
          else Motif.ClearanceType.Diagonal
          
        Some(Motif.Clearance(
          clearingPiece = movedRole,
          clearingFrom = origSq,
          clearedLine = clearanceType,
          beneficiary = friendlyRole,
          color = color,
          plyIndex = plyIndex,
          move = moveUci
        ))
      } else None
      }
    }.headOption
  }

  /**
   * Detect Battery - two pieces aligned on the same line (Q+B on diagonal, R+R on file).
   * Used for detectStateMotifs at end of PV.
   */
  private def detectBattery(board: Board, plyIndex: Int): List[Motif.Battery] = {
    val batteries = List.newBuilder[Motif.Battery]
    
    for (color <- List(White, Black)) {
      val queens = board.byPiece(color, Queen)
      val rooks = board.byPiece(color, Rook)
      val bishops = board.byPiece(color, Bishop)
      val enemyPieces = board.byColor(!color)
      
      // Helper: Check if there's an enemy piece between or beyond two squares on a line
      def hasTargetOnLine(sq1: Square, sq2: Square, axis: Motif.BatteryAxis): Boolean = {
        val ray = axis match {
          case Motif.BatteryAxis.File => Bitboard.file(sq1.file)
          case Motif.BatteryAxis.Rank => Bitboard.rank(sq1.rank)
          case Motif.BatteryAxis.Diagonal => Bitboard.ray(sq1, sq2)
        }
        // Check if any enemy piece is on this line
        (ray & enemyPieces).nonEmpty
      }
      
      // Queen + Rook on same file/rank - ONLY if enemy target exists on line
      queens.foreach { queenSq =>
        rooks.foreach { rookSq =>
          if (queenSq.file == rookSq.file && queenSq != rookSq) {
            if (hasTargetOnLine(queenSq, rookSq, Motif.BatteryAxis.File)) {
              batteries += Motif.Battery(Queen, Rook, Motif.BatteryAxis.File, color, plyIndex, None, Some(queenSq), Some(rookSq))
            }
          } else if (queenSq.rank == rookSq.rank && queenSq != rookSq) {
            if (hasTargetOnLine(queenSq, rookSq, Motif.BatteryAxis.Rank)) {
              batteries += Motif.Battery(Queen, Rook, Motif.BatteryAxis.Rank, color, plyIndex, None, Some(queenSq), Some(rookSq))
            }
          }
        }
      }
      
      // Queen + Bishop on same diagonal - ONLY if enemy target exists
      queens.foreach { queenSq =>
        bishops.foreach { bishopSq =>
          if (queenSq != bishopSq) {
            val fileDiff = (queenSq.file.value - bishopSq.file.value).abs
            val rankDiff = (queenSq.rank.value - bishopSq.rank.value).abs
            if (fileDiff == rankDiff && fileDiff > 0) {
              if (hasTargetOnLine(queenSq, bishopSq, Motif.BatteryAxis.Diagonal)) {
                batteries += Motif.Battery(Queen, Bishop, Motif.BatteryAxis.Diagonal, color, plyIndex, None, Some(queenSq), Some(bishopSq))
              }
            }
          }
        }
      }
      
      // Rook + Rook on same file/rank - ONLY if enemy target exists
      val rookList = rooks.squares.toList
      if (rookList.size >= 2) {
        val r1 = rookList(0)
        val r2 = rookList(1)
        if (r1.file == r2.file) {
          if (hasTargetOnLine(r1, r2, Motif.BatteryAxis.File)) {
            batteries += Motif.Battery(Rook, Rook, Motif.BatteryAxis.File, color, plyIndex, None, Some(r1), Some(r2))
          }
        } else if (r1.rank == r2.rank) {
          if (hasTargetOnLine(r1, r2, Motif.BatteryAxis.Rank)) {
            batteries += Motif.Battery(Rook, Rook, Motif.BatteryAxis.Rank, color, plyIndex, None, Some(r1), Some(r2))
          }
        }
      }
    }
    
    batteries.result()
  }

  /**
   * Detect Removing the Defender - capturing a piece that protected another.
   */
  private def detectRemovingTheDefender(
      mv: Move,
      pos: Position,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): Option[Motif.RemovingTheDefender] = {
    if (!mv.captures) return None
    
    val board = pos.board
    val nextBoard = nextPos.board
    val capturedSq = mv.dest
    val oppColor = !color

    board.roleAt(capturedSq).flatMap { capturedRole =>
      // 1. What did the captured piece defend in the BEFORE position?
      val defenderAttacks = capturedRole match {
        case Pawn   => capturedSq.pawnAttacks(oppColor)
        case Knight => capturedSq.knightAttacks
        case Bishop => capturedSq.bishopAttacks(board.occupied)
        case Rook   => capturedSq.rookAttacks(board.occupied)
        case Queen  => capturedSq.queenAttacks(board.occupied)
        case King   => capturedSq.kingAttacks
      }
      val protectedSqs = (defenderAttacks & board.byColor(oppColor)).squares

      // 2. For each protected piece, was it the SOLE defender?
      protectedSqs.flatMap { targetSq =>
        val otherDefenders = board.attackers(targetSq, oppColor) & ~Bitboard(capturedSq)
        if (otherDefenders.isEmpty) {
          // It was the sole defender. Is the target now under attack by US in nextPos?
          val currentAttackers = nextBoard.attackers(targetSq, color)
          if (currentAttackers.nonEmpty) {
            nextBoard.roleAt(targetSq).flatMap { targetRole =>
              if (MaterialValue.tacticalValueUnit(targetRole) > 1 || targetRole == King) { // Only interesting if not just a pawn
                 Some(Motif.RemovingTheDefender(mv.piece.role, capturedRole, targetRole, capturedSq, color, plyIndex, moveUci))
              } else None
            }
          } else None
        } else None
      }.headOption
    }
  }

  /**
   * Detect Mate Net - King's mobility restricted by multiple attackers.
   */
  private def detectMateNet(
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): Option[Motif.MateNet] = {
    val board = nextPos.board
    val oppColor = !color
    val kingSqOpt = board.kingPosOf(oppColor)

    Option.when(nextPos.checkMate)(()).flatMap { _ =>
      kingSqOpt.flatMap { kingSq =>
      val adjacent = kingSq.kingAttacks
      val safeSquares = (adjacent & ~board.occupied).squares.filter { sq =>
        board.attackers(sq, color).isEmpty
      }
      
      // Heuristic: King has 0 or 1 safe escape squares
      if (safeSquares.size <= 1) {
        val attackers = board.attackers(kingSq, color).squares.flatMap(board.roleAt).toList
        val nearbyAttackers = (kingSq.kingAttacks & board.byColor(color)).count
        
        // At least two pieces coordinating or one very close/powerful (Queen)
        if (nearbyAttackers >= 2 || attackers.contains(Queen)) {
          Some(Motif.MateNet(kingSq, attackers, color, plyIndex, moveUci))
        } else None
      } else None
      }
    }
  }

  /**
   * Detect Initiative - maintaining pressure and active flow.
   */
  private def detectInitiative(
      mv: Move,
      pos: Position,
      nextPos: Position,
      color: Color,
      plyIndex: Int,
      moveUci: Option[String]
  ): Option[Motif.Initiative] = {
    // Basic Initiative heuristics:
    // 1. Move generates a significant new threat or activity
    val board = nextPos.board

    val hasNewThreat = nextPos.check.yes || detectForkTargets(mv, nextPos, color).nonEmpty
    val mobilityBefore = calculateMobility(pos.board, mv.orig, mv.piece.role)
    val mobilityAfter = calculateMobility(board, mv.dest, mv.piece.role)
    val activityGain = mobilityAfter > mobilityBefore + 3

    if (hasNewThreat || activityGain) {
      val score = (if (hasNewThreat) 10 else 0) + (if (activityGain) 10 else 0)
      Some(Motif.Initiative(color, score, plyIndex, moveUci))
    } else None
  }
