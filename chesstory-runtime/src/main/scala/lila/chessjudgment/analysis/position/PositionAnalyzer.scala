package lila.chessjudgment.analysis.position

import chess._
import chess.format.Fen
import lila.chessjudgment.model.position.*

object PositionAnalyzer:

  def extractFeatures(fen: String, plyCount: Int): Option[PositionFeatures] =
    cached(positionFeaturesCache, fen -> plyCount) {
      computeFeatures(fen, plyCount)
    }

  private val CacheMaxEntries = 4096
  private val positionFeaturesCache =
    boundedCache[(String, Int), Option[PositionFeatures]]

  private def boundedCache[K, V]: java.util.LinkedHashMap[K, V] =
    new java.util.LinkedHashMap[K, V](CacheMaxEntries, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean =
        size() > CacheMaxEntries

  private def cached[K, V](cache: java.util.LinkedHashMap[K, V], key: K)(compute: => V): V =
    val existing =
      cache.synchronized {
        Option.when(cache.containsKey(key))(cache.get(key))
      }
    existing.getOrElse {
      val value = compute
      cache.synchronized {
        if cache.containsKey(key) then cache.get(key)
        else
          cache.put(key, value)
          value
      }
    }

  private def computeFeatures(fen: String, plyCount: Int): Option[PositionFeatures] =
    Fen.read(chess.variant.Standard, Fen.Full(fen)).map { position =>
      val board = position.board
      val imbalance = computeImbalance(board)

      PositionFeatures(
        fen = fen,
        sideToMove = position.color,
        plyCount = plyCount,
        pawns = computePawnStructure(board),
        activity = computeActivity(position),
        kingSafety = computeKingSafety(board, position),
        materialPhase = computeMaterialPhase(board),
        lineControl = computeLineControl(board),
        imbalance = imbalance,
        centralSpace = computeCentralSpace(board),
        strategicState = computeStrategicState(board)
      )
    }

  private def computeStrategicState(board: Board): StrategicStateFeatures =
    StrategicStateFeatures(
      whiteEntrenchedPieces = entrenchedPieceCount(board, Color.White),
      blackEntrenchedPieces = entrenchedPieceCount(board, Color.Black),
      whiteRookPawnMarchReady = rookPawnMarchReady(board, Color.White),
      blackRookPawnMarchReady = rookPawnMarchReady(board, Color.Black),
      whiteHookCreationChance = hookCreationChance(board, Color.White),
      blackHookCreationChance = hookCreationChance(board, Color.Black),
      whiteColorComplexClamp = colorComplexClamp(board, Color.White),
      blackColorComplexClamp = colorComplexClamp(board, Color.Black)
    )

  // --- Internal Calculation Logic ---

  private def computePawnStructure(board: Board): PawnStructureFeatures =
    val wPawns = board.pawns & board.white
    val bPawns = board.pawns & board.black
    val wByFile = countsByFile(wPawns)
    val bByFile = countsByFile(bPawns)

    val wIso = isolatedPawns(wPawns)
    val bIso = isolatedPawns(bPawns) // Added missing bIso
    val wDbl = doubledPawns(wPawns)
    val bDbl = doubledPawns(bPawns) // Added missing bDbl
    val wPassed = PawnTopology.passedPawns(Color.White, wPawns, bPawns)
    val bPassed = PawnTopology.passedPawns(Color.Black, bPawns, wPawns)

    // IQP (Isolated Queen Pawn on d-file) with practical support-window handling.
    // We allow distant c/e pawns that are too far to immediately support d-pawn advances.
    val wIQP = iqpOnDFile(Color.White, wPawns, wByFile)
    val bIQP = iqpOnDFile(Color.Black, bPawns, bByFile)

    // Hanging Pawns
    val wHanging = hangingPawns(Color.White, wPawns)
    val bHanging = hangingPawns(Color.Black, bPawns)

    // Backward pawns
    val wBackward = backwardPawns(Color.White, wPawns, board)
    val bBackward = backwardPawns(Color.Black, bPawns, board)

    // Pawn islands.
    val wIslands = pawnIslands(wPawns)
    val bIslands = pawnIslands(bPawns)

    // Connected pawns.
    val wConnected = connectedPawns(Color.White, wPawns)
    val bConnected = connectedPawns(Color.Black, bPawns)

    // Passed pawn quality.
    val (wPassedRank, wProtectedPassed) = analyzePassedPawns(Color.White, wPassed, wPawns)
    val (bPassedRank, bProtectedPassed) = analyzePassedPawns(Color.Black, bPassed, bPawns)

    PawnStructureFeatures(
      whitePawnCount = wPawns.count,
      blackPawnCount = bPawns.count,
      whiteIsolatedPawns = wIso.size,
      blackIsolatedPawns = bIso.size,
      whiteDoubledPawns = wDbl.size,
      blackDoubledPawns = bDbl.size,
      whitePassedPawns = wPassed.size,
      blackPassedPawns = bPassed.size,
      whiteIQP = wIQP,
      blackIQP = bIQP,
      whiteHangingPawns = wHanging.nonEmpty,
      blackHangingPawns = bHanging.nonEmpty,
      whiteBackwardPawns = wBackward.size,
      blackBackwardPawns = bBackward.size,
      whitePawnIslands = wIslands,
      blackPawnIslands = bIslands,
      whiteConnectedPawns = wConnected.size,
      blackConnectedPawns = bConnected.size,
      whitePassedPawnRank = wPassedRank,
      blackPassedPawnRank = bPassedRank,
      whiteProtectedPassedPawns = wProtectedPassed,
      blackProtectedPassedPawns = bProtectedPassed
    )

  private def computeActivity(position: Position): ActivityFeatures =
    val board = position.board
    
    val wPos = if position.color == Color.White then position else position.withColor(Color.White)
    val bPos = if position.color == Color.Black then position else position.withColor(Color.Black)
    
    val wMoves = wPos.legalMoves
    val bMoves = bPos.legalMoves
    
    val wMoveMap = wMoves.groupBy(_.orig)
    val bMoveMap = bMoves.groupBy(_.orig)
    
    def countMobility(moves: Map[Square, List[Move]], roles: Set[Role]): Int =
      moves.values.flatten.count(m => roles.contains(m.piece.role))

    val wMinors = countMobility(wMoveMap, Set(Knight, Bishop))
    val bMinors = countMobility(bMoveMap, Set(Knight, Bishop))

    // Board feature input: Pseudo-legal mobility aggregates
    def pieceMobility(color: Color): (Int, Int, List[String]) = {
      val pieces = (board.knights | board.bishops | board.rooks | board.queens) & board.byColor(color)
      val occupied = board.occupied
      val enemyPawns = board.pawns & board.byColor(!color)
      val enemyPawnAttacks = enemyPawns.squares.foldLeft(Bitboard.empty)((bb, pSq) => bb | pSq.pawnAttacks(!color))
      
      var totalMobility = 0
      var lowMobilityCount = 0
      var lowMobilitySquares = List.empty[String]
      
      pieces.squares.foreach { sq =>
        board.pieceAt(sq).foreach { piece =>
          val attacks = piece.role match {
            case Knight => sq.knightAttacks
            case Bishop => sq.bishopAttacks(occupied)
            case Rook => sq.rookAttacks(occupied)
            case Queen => sq.queenAttacks(occupied)
            case King => sq.kingAttacks
            case Pawn => Bitboard.empty
          }
          val safeMoves = attacks & ~board.byColor(color) & ~enemyPawnAttacks
          val moves = safeMoves.count
          totalMobility += moves
          if (moves <= 2) {
            lowMobilityCount += 1
            lowMobilitySquares ::= sq.key
          }
        }
      }
      (totalMobility, lowMobilityCount, lowMobilitySquares.distinct.sorted)
    }
    
    val (wPseudoMob, wLowMob, wLowMobilitySquares) = pieceMobility(White)
    val (bPseudoMob, bLowMob, bLowMobilitySquares) = pieceMobility(Black)
    
    // Board feature input: Attacked pieces count
    def attackedPieces(color: Color): Int = {
      val pieces = (board.knights | board.bishops | board.rooks | board.queens) & board.byColor(color)
      pieces.squares.count { sq =>
        board.attackers(sq, !color).nonEmpty
      }
    }
    
    val wAttacked = attackedPieces(White)
    val bAttacked = attackedPieces(Black)

    // Development lag: minor pieces still on the back rank.
    def developmentLag(color: Color): Int =
      val backRank = if color == Color.White then Rank.First else Rank.Eighth
      val minors = (board.knights | board.bishops) & board.byColor(color)
      minors.squares.count(_.rank == backRank)

    val wDevLag = developmentLag(White)
    val bDevLag = developmentLag(Black)

    ActivityFeatures(
      whiteLegalMoves = wMoves.size,
      blackLegalMoves = bMoves.size,
      whiteMinorPieceMobility = wMinors,
      blackMinorPieceMobility = bMinors,
      whitePseudoMobility = wPseudoMob,
      blackPseudoMobility = bPseudoMob,
      whiteLowMobilityPieces = wLowMob,
      blackLowMobilityPieces = bLowMob,
      whiteAttackedPieces = wAttacked,
      blackAttackedPieces = bAttacked,
      whiteDevelopmentLag = wDevLag,
      blackDevelopmentLag = bDevLag,
      whiteLowMobilitySquares = wLowMobilitySquares,
      blackLowMobilitySquares = bLowMobilitySquares
    )

  private def computeKingSafety(board: Board, position: Position): KingSafetyFeatures =
    val castles = position.history.castles
    def castleRights(color: Color): String =
      val k = castles.can(color, Side.KingSide)
      val q = castles.can(color, Side.QueenSide)
      if k && q then "can_castle_both"
      else if k then "can_castle_short"
      else if q then "can_castle_long"
      else "none"

    def castledSide(color: Color): String =
      board.kingPosOf(color) match {
        case Some(sq) if sq.file == File.G => "short"
        case Some(sq) if sq.file == File.C => "long"
        case _ => "none"
      }
    
    def shieldPawns(color: Color): Int =
      board.kingPosOf(color).map { kSq =>
        val rank = kSq.rank
        val file = kSq.file
        // Shield = pawns on same/adjacent files, one rank ahead
        val shieldRank = if color == Color.White then rank.value + 1 else rank.value - 1
        if shieldRank < 0 || shieldRank > 7 then 0
        else
           (-1 to 1).count { fOffset =>
             val fVal = file.value + fOffset
             if fVal >= 0 && fVal <= 7 then
               val sq = Square.at(fVal, shieldRank)
               sq.exists(s => board.pieceAt(s).contains(Piece(color, Pawn)))
             else false
           }
      }.getOrElse(0)

    val wCastlesRights = castleRights(Color.White)
    val bCastlesRights = castleRights(Color.Black)
    val wCastled = castledSide(Color.White)
    val bCastled = castledSide(Color.Black)
    val wShield = shieldPawns(Color.White)
    val bShield = shieldPawns(Color.Black)

    // Exposed files
    def exposedFiles(color: Color): Int =
      board.kingPosOf(color).map { kSq =>
        val file = kSq.file
        val kingRank = kSq.rank.value
        (-1 to 1).count { fOffset =>
          val fVal = file.value + fOffset
          if fVal >= 0 && fVal <= 7 then
             // Open, semi-open, or only advanced pawns
             val f = File.all(fVal)
             val fBb = Bitboard.file(f)
             val friendlyPawns = board.pawns & fBb & board.byColor(color)
             
             if friendlyPawns.isEmpty then true
             else
               // Check if closest friendly pawn is too far (e.g. > 2 ranks away)
               val closestRank = if color == Color.White then
                 friendlyPawns.squares.map(_.rank.value).min
               else
                 friendlyPawns.squares.map(_.rank.value).max
                 
               (closestRank - kingRank).abs > 2
          else false
        }
      }.getOrElse(0)

    val wExposed = exposedFiles(Color.White)
    val bExposed = exposedFiles(Color.Black)
    
    val wBackRank = board.kingPosOf(Color.White).exists(_.rank == Rank.First) && wShield == 0
    val bBackRank = board.kingPosOf(Color.Black).exists(_.rank == Rank.Eighth) && bShield == 0

    // Weighted attackers around the king zone.
    def attackersCount(color: Color): Int =
      board.kingPosOf(color).map { kSq =>
        val kingZone = kSq.kingAttacks
        val enemyColor = !color
        // Pieces of enemyColor attacking any square in kingZone
        val attackingPieces = kingZone.squares.flatMap { sq =>
          board.attackers(sq, enemyColor).squares
        }.toSet
        
        attackingPieces.toList.map { attackerSq =>
          board.roleAt(attackerSq) match {
            case Some(Queen) => 4
            case Some(Rook) => 2
            case Some(Bishop) | Some(Knight) => 2
            case Some(Pawn) => 1
            case _ => 1
          }
        }.sum
      }.getOrElse(0)

    // Safe squares for the king to flee.
    def escapeSquares(color: Color): Int =
       board.kingPosOf(color).map { kSq =>
         val kingMoves = kSq.kingAttacks
         val ownPieces = board.byColor(color)
         val enemyColor = !color
         
         // Safe escape = not occupied by own piece and not attacked by enemy
         (kingMoves & ~ownPieces).squares.count { sq =>
           !board.attackers(sq, enemyColor).nonEmpty
         }
       }.getOrElse(0)

    // Count of attacked squares around the king.
    def kingRingAttacked(color: Color): Int =
       board.kingPosOf(color).map { kSq =>
         val kingZone = kSq.kingAttacks
         val enemyColor = !color
         
         kingZone.squares.count { sq =>
           board.attackers(sq, enemyColor).nonEmpty
         }
       }.getOrElse(0)

    KingSafetyFeatures(
      whiteCastlingRights = wCastlesRights,
      blackCastlingRights = bCastlesRights,
      whiteCastledSide = wCastled,
      blackCastledSide = bCastled,
      whiteKingShield = wShield,
      blackKingShield = bShield,
      whiteKingExposedFiles = wExposed,
      blackKingExposedFiles = bExposed,
      whiteBackRankWeakness = wBackRank,
      blackBackRankWeakness = bBackRank,
      whiteAttackersCount = attackersCount(Color.White),
      blackAttackersCount = attackersCount(Color.Black),
      whiteEscapeSquares = escapeSquares(Color.White),
      blackEscapeSquares = escapeSquares(Color.Black),
      whiteKingRingAttacked = kingRingAttacked(Color.White),
      blackKingRingAttacked = kingRingAttacked(Color.Black)
    )

  private def computeMaterialPhase(board: Board): MaterialPhaseFeatures =
    def mat(color: Color) = 
      board.byColor(color).squares.map { sq =>
        board.pieceAt(sq).fold(0) { p =>
          p.role match
            case Queen => 9
            case Rook => 5
            case Bishop => 3
            case Knight => 3
            case Pawn => 1
            case King => 0
        }
      }.sum

    val wMat = mat(Color.White)
    val bMat = mat(Color.Black)
    
    val wMaj = board.count(Color.White, Queen) + board.count(Color.White, Rook)
    val bMaj = board.count(Color.Black, Queen) + board.count(Color.Black, Rook)
    val wMin = board.count(Color.White, Bishop) + board.count(Color.White, Knight)
    val bMin = board.count(Color.Black, Bishop) + board.count(Color.Black, Knight)

    val totalMat = wMat + bMat
    val isEndgameByPieces = wMaj + bMaj == 0 && wMin + bMin <= 2
    val phase = 
      if isEndgameByPieces || totalMat <= 40 then "endgame"
      else if totalMat >= 70 then "opening"
      else "middlegame"

    MaterialPhaseFeatures(
      whiteMaterial = wMat,
      blackMaterial = bMat,
      materialDiff = wMat - bMat,
      phase = phase
    )

  // Line control features: open files and rook placement.
  private def computeLineControl(board: Board): LineControlFeatures =
    val wPawns = board.pawns & board.white
    val bPawns = board.pawns & board.black
    
    // Count open files (no pawns at all)
    val openFiles = File.all.count { file =>
      val fileBb = Bitboard.file(file)
      (board.pawns & fileBb).isEmpty
    }
    
    // Count semi-open files (only enemy pawns)
    val whiteSemiOpen = File.all.count { file =>
      val fileBb = Bitboard.file(file)
      (wPawns & fileBb).isEmpty && (bPawns & fileBb).nonEmpty
    }
    
    val blackSemiOpen = File.all.count { file =>
      val fileBb = Bitboard.file(file)
      (bPawns & fileBb).isEmpty && (wPawns & fileBb).nonEmpty
    }
    
    // Rook on 7th (White) or 2nd (Black)
    val whiteRookOn7th = (board.rooks & board.white & Rank.Seventh.bb).nonEmpty
    val blackRookOn7th = (board.rooks & board.black & Rank.Second.bb).nonEmpty
    
    LineControlFeatures(
      openFilesCount = openFiles,
      whiteSemiOpenFiles = whiteSemiOpen,
      blackSemiOpenFiles = blackSemiOpen,
      whiteRookOn7th = whiteRookOn7th,
      blackRookOn7th = blackRookOn7th
    )

  // Material imbalance: piece counts and bishop-pair detection.
  private def computeImbalance(board: Board): MaterialImbalanceFeatures =
    val wKnights = (board.knights & board.white).count
    val bKnights = (board.knights & board.black).count
    val wBishops = (board.bishops & board.white).count
    val bBishops = (board.bishops & board.black).count
    val wRooks = (board.rooks & board.white).count
    val bRooks = (board.rooks & board.black).count
    val wQueens = (board.queens & board.white).count
    val bQueens = (board.queens & board.black).count
    
    // Bishop pair = has 2+ bishops
    val wBishopPair = wBishops >= 2
    val bBishopPair = bBishops >= 2
    
    MaterialImbalanceFeatures(
      whiteKnights = wKnights,
      blackKnights = bKnights,
      whiteBishops = wBishops,
      blackBishops = bBishops,
      whiteRooks = wRooks,
      blackRooks = bRooks,
      whiteQueens = wQueens,
      blackQueens = bQueens,
      whiteBishopPair = wBishopPair,
      blackBishopPair = bBishopPair
    )

  // Unified central space computation - consolidates pawn structure, tension, and piece control
  private def computeCentralSpace(board: Board): CentralSpaceFeatures =
    val wPawns = board.pawns & board.white
    val bPawns = board.pawns & board.black
    
    // Central pawns (d, e files) - from PawnStructureFeatures
    val wByFile = countsByFile(wPawns)
    val bByFile = countsByFile(bPawns)
    val wCentralPawns = wByFile.getOrElse(3, 0) + wByFile.getOrElse(4, 0)
    val bCentralPawns = bByFile.getOrElse(3, 0) + bByFile.getOrElse(4, 0)
    
    // Center control (d4, e4, d5, e5) - from ActivityFeatures
    val dFile = File.D
    val eFile = File.E
    val center = (dFile.bb & Rank.Fourth.bb) | (eFile.bb & Rank.Fourth.bb) | 
                 (dFile.bb & Rank.Fifth.bb) | (eFile.bb & Rank.Fifth.bb)
    
    def countControl(color: Color) = 
      center.squares.count(sq => board.attackers(sq, color).nonEmpty)

    val wCenterControl = countControl(Color.White)
    val bCenterControl = countControl(Color.Black)
    
    // Space diff: count pawns beyond 4th rank for white, below 5th for black
    val wSpace = wPawns.squares.count(_.rank.value >= 3) // rank 4+
    val bSpace = bPawns.squares.count(_.rank.value <= 4) // rank 5-
    val spaceDiff = wSpace - bSpace
    
    // Pawn tension: pawns that can capture each other
    val tensionCount = wPawns.squares.count { wSq =>
      val wAttacks = wSq.pawnAttacks(Color.White)
      (wAttacks & bPawns).nonEmpty
    } + bPawns.squares.count { bSq =>
      val bAttacks = bSq.pawnAttacks(Color.Black)
      (bAttacks & wPawns).nonEmpty
    }
    
    // Locked center: e4 blocked by e5, d4 blocked by d5
    val wOnD4 = wPawns.squares.exists(s => s.file == dFile && s.rank == Rank.Fourth)
    val bOnD5 = bPawns.squares.exists(s => s.file == dFile && s.rank == Rank.Fifth)
    val wOnE4 = wPawns.squares.exists(s => s.file == eFile && s.rank == Rank.Fourth)
    val bOnE5 = bPawns.squares.exists(s => s.file == eFile && s.rank == Rank.Fifth)
    val lockedCenter = (wOnD4 && bOnD5) || (wOnE4 && bOnE5)
    
    // Open center: no pawns on d or e files for either side
    val dFileBb = Bitboard.file(dFile)
    val eFileBb = Bitboard.file(eFile)
    val centerPawns = (board.pawns & (dFileBb | eFileBb)).count
    val openCenter = centerPawns == 0
    
    CentralSpaceFeatures(
      whiteCentralPawns = wCentralPawns,
      blackCentralPawns = bCentralPawns,
      whiteCenterControl = wCenterControl,
      blackCenterControl = bCenterControl,
      spaceDiff = spaceDiff,
      pawnTensionCount = tensionCount,
      lockedCenter = lockedCenter,
      openCenter = openCenter
    )

  // --- Helpers (Exposed for Verification) ---

  def countsByFile(pawns: Bitboard): Map[Int, Int] =
    pawns.squares.groupBy(_.file.value).view.mapValues(_.size).toMap

  def isolatedPawns(pawns: Bitboard): List[Square] =
    pawns.squares.filter { sq =>
      val file = sq.file
      val f = file.value
      val adjacentMask = (if f > 0 then Bitboard.file(File.all(f - 1)) else Bitboard.empty) | 
                         (if f < 7 then Bitboard.file(File.all(f + 1)) else Bitboard.empty)
      (pawns & adjacentMask).isEmpty
    }.toList

  def doubledPawns(pawns: Bitboard): List[Square] =
    pawns.squares.groupBy(_.file).filter(_._2.size > 1).values.flatten.toList

  def analyzePassedPawns(color: Color, passed: List[Square], pawns: Bitboard): (Int, Int) =
    if passed.isEmpty then (0, 0)
    else
      val maxRank = if color == Color.White then passed.map(_.rank.value).max
                    else 7 - passed.map(_.rank.value).min
      val protectedCount = passed.count { p => (p.pawnAttacks(!color) & pawns).nonEmpty }
      (maxRank, protectedCount)

  private def iqpOnDFile(color: Color, pawns: Bitboard, byFile: Map[Int, Int]): Boolean =
    val dFileIdx = File.D.value
    if byFile.getOrElse(dFileIdx, 0) != 1 then false
    else
      val dPawnOpt = pawns.squares.find(_.file == File.D)
      dPawnOpt.exists { dPawn =>
        val adjacent = pawns.squares.filter(s => s.file == File.C || s.file == File.E)
        // Immediate support window:
        // White d4 is no longer "isolated" if c3/e3 or more advanced adjacent pawn exists.
        // Black d5 is no longer "isolated" if c6/e6 or more advanced adjacent pawn exists.
        val hasImmediateSupport = adjacent.exists { s =>
          if color == Color.White then s.rank.value >= (dPawn.rank.value - 1).max(Rank.First.value)
          else s.rank.value <= (dPawn.rank.value + 1).min(Rank.Eighth.value)
        }
        !hasImmediateSupport
      }

  def pawnIslands(pawns: Bitboard): Int =
    val files = pawns.squares.map(_.file.value).distinct.sorted
    if files.isEmpty then 0
    else
      files.foldLeft((0, -2)) { case ((count, prev), curr) =>
        if curr > prev + 1 then (count + 1, curr) else (count, curr)
      }._1

  def connectedPawns(color: Color, pawns: Bitboard): List[Square] =
    pawns.squares.filter { p => (p.pawnAttacks(!color) & pawns).nonEmpty }.toList

  def backwardPawns(color: Color, pawns: Bitboard, board: Board): List[Square] =
     pawns.squares.filter { pawn =>
       val pawnFile = pawn.file.value
       val pawnRank = pawn.rank.value
       
       val adjacentMask = (if pawnFile > 0 then Bitboard.file(File.all(pawnFile - 1)) else Bitboard.empty) | 
                          (if pawnFile < 7 then Bitboard.file(File.all(pawnFile + 1)) else Bitboard.empty)
       val adjFriendly = pawns & adjacentMask
       
       // 1. Must have friendly pawns on adjacent files
       val hasAdjacentFriendly = adjFriendly.nonEmpty
       
       // 2. Must be behind all adjacent friendly pawns
       val isBehindAllAdj = hasAdjacentFriendly && adjFriendly.squares.forall { adj =>
          if color == Color.White then adj.rank.value > pawnRank else adj.rank.value < pawnRank
       }
       
       // 3. No friendly pawns ahead on the same file
       val friendlyAhead = pawns.squares.exists(p => p.file == pawn.file && (if color == White then p.rank > pawn.rank else p.rank < pawn.rank))
       
       // 4. Stop square controlled by enemy or blocked
       val stopRank = if color == White then pawnRank + 1 else pawnRank - 1
       val stopSq = Square.at(pawnFile, stopRank)
       val isBackward = stopSq.exists { sq =>
          val controlledByEnemy = board.attackers(sq, !color).nonEmpty
          val blockadedByEnemy = board.pieceAt(sq).exists(_.color != color)
          val occupiedByFriendly = board.pieceAt(sq).exists(_.color == color)
          
          (controlledByEnemy || blockadedByEnemy) && !occupiedByFriendly
       }
       
       hasAdjacentFriendly && isBehindAllAdj && !friendlyAhead && isBackward
     }.toList
  
  def hangingPawns(color: Color, pawns: Bitboard): List[Square] =
    val cAndD = pawns.squares.filter(s => s.file == File.C || s.file == File.D).toList
    val hasC = cAndD.exists(_.file == File.C)
    val hasD = cAndD.exists(_.file == File.D)
    if !hasC || !hasD then Nil
    else
      // Practical criterion:
      // 1) C/D pair exists and is at least minimally advanced.
      // 2) Adjacent B/E pawns are allowed when they are distant (e.g. b2/e2),
      //    but not when they are immediately supporting the pair (b3/e3+ for white, b6/e6- for black).
      val advancedThreshold = if color == White then Rank.Fourth.value else Rank.Fifth.value
      val advanced = cAndD.exists(s => if color == White then s.rank.value >= advancedThreshold else s.rank.value <= advancedThreshold)
      val cPawnRanks = cAndD.filter(_.file == File.C).map(_.rank.value)
      val dPawnRanks = cAndD.filter(_.file == File.D).map(_.rank.value)
      val sameRankPair = cPawnRanks.exists(cr => dPawnRanks.contains(cr))

      def hasImmediateSupport(file: File): Boolean =
        pawns.squares.exists { s =>
          s.file == file &&
          (if color == White then s.rank.value >= Rank.Third.value else s.rank.value <= Rank.Sixth.value)
        }

      val bSupport = hasImmediateSupport(File.B)
      val eSupport = hasImmediateSupport(File.E)
      if advanced && sameRankPair && !bSupport && !eSupport then cAndD else Nil

  private def entrenchedPieceCount(board: Board, color: Color): Int =
    val pieces = ((board.knights | board.bishops) & board.byColor(color)).squares
    pieces.count { sq =>
      val enemyPawnAttack = (board.pawns & board.byColor(!color)).squares.exists { ep =>
        (ep.pawnAttacks(!color) & sq.bb).nonEmpty
      }
      val defenders = board.attackers(sq, color).count
      val centralOrAdvanced =
        if color.white then sq.rank.value >= Rank.Fourth.value
        else sq.rank.value <= Rank.Fifth.value
      !enemyPawnAttack && defenders >= 2 && centralOrAdvanced
    }

  private def rookPawnMarchReady(board: Board, color: Color): Boolean =
    val aFile = if color.white then List(Square.A2, Square.A3, Square.A4, Square.A5) else List(Square.A7, Square.A6, Square.A5, Square.A4)
    val hFile = if color.white then List(Square.H2, Square.H3, Square.H4, Square.H5) else List(Square.H7, Square.H6, Square.H5, Square.H4)
    val hasA = aFile.exists(sq => board.pieceAt(sq).contains(Piece(color, Pawn)))
    val hasH = hFile.exists(sq => board.pieceAt(sq).contains(Piece(color, Pawn)))
    val targetKingFlankAligned =
      board.kingPosOf(!color).exists { k =>
        (hasH && k.file.value >= File.F.value) ||
          (hasA && k.file.value <= File.C.value)
      }
    (hasA || hasH) && targetKingFlankAligned

  private def hookCreationChance(board: Board, color: Color): Boolean =
    val enemy = !color
    val flankPairs =
      if color.white then
        List((Square.G4, Square.G5), (Square.H4, Square.H5), (Square.A4, Square.A5), (Square.B4, Square.B5))
      else
        List((Square.G5, Square.G4), (Square.H5, Square.H4), (Square.A5, Square.A4), (Square.B5, Square.B4))
    flankPairs.exists { case (ourSq, theirSq) =>
      board.pieceAt(ourSq).contains(Piece(color, Pawn)) &&
      board.pieceAt(theirSq).contains(Piece(enemy, Pawn))
    }

  private def colorComplexClamp(board: Board, color: Color): Boolean =
    val enemy = !color
    val enemyKing = board.kingPosOf(enemy)
    enemyKing.exists { kSq =>
      val ring = kSq.kingAttacks.squares
      val attacked = ring.count(sq => board.attackers(sq, color).nonEmpty)
      val sameColorBias =
        val darkSquares = ring.count(!_.isLight)
        val lightSquares = ring.count(_.isLight)
        Math.abs(darkSquares - lightSquares) <= 3
      attacked >= 5 && sameColorBias
    }
