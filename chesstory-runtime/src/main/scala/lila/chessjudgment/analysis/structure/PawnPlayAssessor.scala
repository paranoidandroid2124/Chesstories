package lila.chessjudgment.analysis.structure

import chess._
import chess.format.Fen
import chess.variant.Standard
import chess.Color
import lila.chessjudgment.model.position.{ PawnTopology, PositionFeatures }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*

/**
 * Break & Pawn Play Analyzer
 *
 * Extracts board-backed break, tension, blockade, minority-attack, and
 * counter-break facts. Internal scores are used only to rank candidates.
 */
object PawnPlayAssessor:

  private enum PassedPawnUrgency:
    case Critical, Important, Background, Blocked

  private val HIGH_TENSION_THRESHOLD = 2      // Tension squares to force resolution

  private case class BreakCandidate(
    file: String,
    impact: Int,
    priority: Int
  )

  /**
   * Analyze pawn play concepts from position features.
   *
   * @param features board feature inputs
   * @param motifs tactical motif inputs (for PawnBreak detection)
   * @param sideToMove side to move
   * @return Complete pawn play analysis with 10 concepts
   */
  def analyze(
    features: PositionFeatures,
    motifs: List[Motif],
    sideToMove: Color
  ): Option[PawnPlayAnalysis] =
    val isWhite = sideToMove.white
    Fen.read(Standard, Fen.Full(features.fen)).map { position =>
      val board = position.board
      val breakFile = analyzeBreaks(features, motifs, board, isWhite)
      val breakReady = breakFile.nonEmpty
      val advanceOrCapture = checkTensionResolution(features)
      val (urgency, blockadeSq, blockadeRole) = analyzePassedPawns(features, board, isWhite)
      val minorityAttack = checkMinorityAttack(board, isWhite)
      val counterBreakFiles = extractCounterBreakFiles(features, motifs, board, isWhite)
      val counterBreak = counterBreakFiles.nonEmpty
      val tensionPolicy = computeTensionPolicy(features, breakReady, advanceOrCapture)
      val tensionEdges = extractTensionEdges(board)
      val driver =
        computePrimaryDriver(breakReady, urgency, advanceOrCapture, features.centralSpace.pawnTensionCount, counterBreak)

      PawnPlayAnalysis(
        breakFile = breakFile,
        advanceOrCapture = advanceOrCapture,
        blockadeSquare = blockadeSq,
        blockadeRole = blockadeRole,
        minorityAttack = minorityAttack,
        tensionPolicy = tensionPolicy,
        primaryDriver = driver,
        tensionEdges = tensionEdges,
        counterBreakFiles = counterBreakFiles
      )
    }
  // CONCEPT 1-3: BREAK ANALYSIS

  private def analyzeBreaks(
    features: PositionFeatures,
    motifs: List[Motif],
    board: Board,
    isWhite: Boolean
  ): Option[String] =

    // Check for existing PawnBreak motifs from tactical motif inputs
    val pawnBreakMotifs = motifs.collect {
      case m: Motif.PawnBreak if colorMatches(m.color, isWhite) => m
    }

    if pawnBreakMotifs.nonEmpty then
      val primaryBreak = pawnBreakMotifs.head
      // `chess.File.toString` is numeric in some contexts; use `.char` for file letter.
      val file = primaryBreak.file.char.toString.toLowerCase
      Some(file)
    else
      // Detect potential breaks from pawn structure
      detectPotentialBreak(features, board, isWhite)

  private def detectPotentialBreak(
    features: PositionFeatures,
    board: Board,
    isWhite: Boolean
  ): Option[String] =
    detectBoardBreak(features, board, isWhite).map(_.file)

  private def detectBoardBreak(
    features: PositionFeatures,
    board: Board,
    isWhite: Boolean
  ): Option[BreakCandidate] =
    detectBoardBreakCandidates(features, board, isWhite).headOption

  private def detectBoardBreakCandidates(
    features: PositionFeatures,
    board: Board,
    isWhite: Boolean
  ): List[BreakCandidate] =
    val color = if isWhite then Color.White else Color.Black
    val enemy = !color
    val myPawns = board.byPiece(color, Pawn)
    val enemyPawns = board.byPiece(enemy, Pawn)

    val captureLevers =
      myPawns.squares
      .flatMap { pawnSq =>
        (pawnSq.pawnAttacks(color) & enemyPawns).squares.map { targetSq =>
          val file = pawnSq.file.char.toString.toLowerCase
          val centralFile = isCentralFile(pawnSq.file)
          val wingLever = !centralFile && features.centralSpace.lockedCenter
          val impact =
            estimateBreakImpact(features, board, file, color) +
              fileOpennessBonus(board, pawnSq.file) +
              (if centralFile then 15 else 10) +
              (if wingLever then 20 else 0)
          val priority =
            impact +
              relativeRank(pawnSq, color) * 4 +
              (if board.attackers(targetSq, color).count >= board.attackers(targetSq, enemy).count then 12 else 0)
          BreakCandidate(
            file = file,
            impact = impact,
            priority = priority
          )
        }
      }

    val centralAdvanceLevers =
      myPawns.squares.flatMap(pawnSq => centralAdvanceBreak(features, board, pawnSq, color))

    (captureLevers ++ centralAdvanceLevers).toList
      .sortBy(candidate => -candidate.priority)

  private def centralAdvanceBreak(
    features: PositionFeatures,
    board: Board,
    pawnSq: Square,
    color: Color
  ): Option[BreakCandidate] =
    val forwardRank = pawnSq.rank.value + (if color.white then 1 else -1)
    Square
      .at(pawnSq.file.value, forwardRank)
      .filter(_ => pawnHasAdvanced(pawnSq, color))
      .filter(targetSq => board.pieceAt(targetSq).isEmpty)
      .filter(isCentralBreakSquare)
      .filter(targetSq => centralAdvanceCreatesContact(features, board, targetSq, color))
      .map { targetSq =>
        val file = pawnSq.file.char.toString.toLowerCase
        val attackedTargets = (targetSq.pawnAttacks(color) & board.byColor(!color)).count
        val impact =
          estimateBreakImpact(features, board, file, color) +
            fileOpennessBonus(board, pawnSq.file) +
            20 +
            attackedTargets * 10
        val priority =
          impact +
            relativeRank(targetSq, color) * 4 +
            (if board.attackers(targetSq, color).count >= board.attackers(targetSq, !color).count then 8 else 0)
        BreakCandidate(
          file = file,
          impact = impact,
          priority = priority
        )
      }

  private def centralAdvanceCreatesContact(
    features: PositionFeatures,
    board: Board,
    targetSq: Square,
    color: Color
  ): Boolean =
    val directTarget = (targetSq.pawnAttacks(color) & board.byColor(!color)).nonEmpty
    val contestedCenter = !features.centralSpace.openCenter && board.attackers(targetSq, !color).nonEmpty
    directTarget || contestedCenter

  private def pawnHasAdvanced(pawnSq: Square, color: Color): Boolean =
    if color.white then pawnSq.rank != Rank.Second else pawnSq.rank != Rank.Seventh

  private def isCentralBreakSquare(square: Square): Boolean =
    val centralRank = square.rank == Rank.Fourth || square.rank == Rank.Fifth
    val centralFile = square.file == File.C || square.file == File.D || square.file == File.E || square.file == File.F
    centralRank && centralFile

  private def estimateBreakImpact(features: PositionFeatures, board: Board, file: String, color: Color): Int =
    // Base impact from opening a file
    val baseImpact = 80

    // Bonus if we have rooks to use the file
    val rookBonus = if color.white then
      if features.imbalance.whiteRooks > 0 then 50 else 0
    else
      if features.imbalance.blackRooks > 0 then 50 else 0

    val fileIndex = file.headOption.map(ch => ch.toLower - 'a')
    val kingAttackBonus =
      (for
        f <- fileIndex
        king <- board.kingPosOf(!color)
      yield
        val distance = (f - king.file.value).abs
        if distance <= 1 then 30 else if distance == 2 then 15 else 0
      ).getOrElse(0)

    baseImpact + rookBonus + kingAttackBonus

  private def checkTensionResolution(features: PositionFeatures): Boolean =
    val tension = features.centralSpace.pawnTensionCount
    tension >= HIGH_TENSION_THRESHOLD && !staticPosition(features)
  // CONCEPT 5-7: PASSED PAWN ANALYSIS

  private def analyzePassedPawns(
    features: PositionFeatures,
    board: Board,
    isWhite: Boolean
  ): (PassedPawnUrgency, Option[Square], Option[Role]) = {
    val pawns = features.pawns

    val passedCount = if isWhite then pawns.whitePassedPawns else pawns.blackPassedPawns
    val passedRank = if isWhite then pawns.whitePassedPawnRank else pawns.blackPassedPawnRank
    val protectedPassed = if isWhite then pawns.whiteProtectedPassedPawns else pawns.blackProtectedPassedPawns

    if passedCount == 0 then
      (PassedPawnUrgency.Background, None, None)
    else
      // Read passed-pawn squares from the canonical pawn-topology primitive.
      val color = if isWhite then Color.White else Color.Black
      val myPawns = board.byPiece(color, Pawn)
      val oppPawns = board.byPiece(!color, Pawn)
      val actualPassedPawns = PawnTopology.passedPawns(color, myPawns, oppPawns)

      // Sort by advancement: most advanced first (White: highest rank, Black: lowest rank)
      val sortedPassers = if isWhite then actualPassedPawns.sortBy(-_.rank.value)
                          else actualPassedPawns.sortBy(_.rank.value)

      // Find first blockaded passed pawn (prioritizing most advanced)
      val blockadeInfo = sortedPassers.view.flatMap { pSq =>
        val aheadRank = pSq.rank.value + (if isWhite then 1 else -1)
        Square.at(pSq.file.value, aheadRank).flatMap { sq =>
          board.pieceAt(sq).filter(_.color != color).map(p => (sq, p.role))
        }
      }.headOption

      val isBlocked = blockadeInfo.isDefined

      // Concept 7: Support detection
      val hasRookSupport = if isWhite then features.lineControl.whiteRookOn7th
                          else features.lineControl.blackRookOn7th
      val detailedSupport = checkRookSupport(board, sortedPassers, color)
      val kingSupport = checkKingSupport(board, sortedPassers, color)
      val support = hasRookSupport || detailedSupport || kingSupport || protectedPassed > 0
      val urgency = adjustPasserUrgency(
        computePasserUrgency(passedRank),
        passedRank,
        isBlocked,
        blockadeInfo.map(_._2),
        support
      )

      (urgency, blockadeInfo.map(_._1), blockadeInfo.map(_._2))
    }

  private def checkRookSupport(board: Board, passedPawns: List[Square], color: Color): Boolean =
    // Check for rook behind any passed pawn
    val myRooks = board.byPiece(color, Rook)
    passedPawns.exists { pSq =>
      myRooks.squares.exists { rSq =>
        rSq.file == pSq.file &&
        (if color.white then rSq.rank.value < pSq.rank.value else rSq.rank.value > pSq.rank.value)
      }
    }

  private def checkKingSupport(board: Board, passedPawns: List[Square], color: Color): Boolean =
    board.kingPosOf(color).exists { kingSq =>
      passedPawns.exists { pawnSq =>
        val nextRank = pawnSq.rank.value + (if color.white then 1 else -1)
        val advanceSquare = Square.at(pawnSq.file.value, nextRank)
        (pawnSq :: advanceSquare.toList).exists { sq =>
          (kingSq.file.value - sq.file.value).abs <= 1 &&
          (kingSq.rank.value - sq.rank.value).abs <= 2
        }
      }
    }

  private def computePasserUrgency(rank: Int): PassedPawnUrgency =
    // CRITICAL DEPENDENCY WARNING:
    // This value 'rank' must be normalized by board feature extraction (PositionAnalyzer) to represent "advancement".
    // Contract: 0 = Promotion Square (Not possible for pawn), 1 = Start Rank, 7 = Promotion Rank.
    // Board feature extraction verified:
    //   White: rank.value.max (Index 1 to 7)
    //   Black: 7 - rank.value.min (Index 6->1 becomes 1; Index 1->6 becomes 6)
    // Thus, 'rank' is always 1 (start) to 7 (promotion).

    rank match
      case r if r >= 6 => PassedPawnUrgency.Critical   // Rank 7 or 8 (Index 6/7)
      case r if r >= 4 => PassedPawnUrgency.Important  // Rank 5 or 6 (Index 4/5)
      case r if r >= 2 => PassedPawnUrgency.Background // Rank 3 or 4 (Index 2/3)
      case _ => PassedPawnUrgency.Blocked              // Rank 2 (Index 1)

  private def adjustPasserUrgency(
    base: PassedPawnUrgency,
    passedRank: Int,
    isBlocked: Boolean,
    blockadeRole: Option[Role],
    support: Boolean
  ): PassedPawnUrgency =
    if passedRank >= 6 then
      if isBlocked && !support then PassedPawnUrgency.Important
      else PassedPawnUrgency.Critical
    else if passedRank >= 4 then
      if isBlocked && blockadeRole.contains(King) && !support then PassedPawnUrgency.Background
      else if support || !isBlocked then PassedPawnUrgency.Important
      else PassedPawnUrgency.Background
    else if isBlocked && !support then PassedPawnUrgency.Blocked
    else base
  // CONCEPT 8-9: STRATEGIC POSTURE

  private def checkMinorityAttack(board: Board, isWhite: Boolean): Boolean =
    val whitePawns = board.byPiece(Color.White, Pawn)
    val blackPawns = board.byPiece(Color.Black, Pawn)

    val myPawns = if isWhite then whitePawns else blackPawns
    val oppPawns = if isWhite then blackPawns else whitePawns

    // Queenside files: a, b, c
    val qSideMask = Bitboard.file(File.A) | Bitboard.file(File.B) | Bitboard.file(File.C)

    val myQSide = (myPawns & qSideMask).count
    val oppQSide = (oppPawns & qSideMask).count

    // Classic minority: we have fewer pawns (but > 0) than opponent on queenside
    // e.g. 2 vs 3
    myQSide > 0 && myQSide < oppQSide

  private def extractCounterBreakFiles(
    features: PositionFeatures,
    motifs: List[Motif],
    board: Board,
    isWhite: Boolean
  ): List[String] =
    val motifFiles = motifs.collect {
      case m: Motif.PawnBreak if !colorMatches(m.color, isWhite) =>
        m.file.char.toString.toLowerCase
    }
    val potentialFiles =
      detectBoardBreakCandidates(features, board, !isWhite).map(_.file)
    (motifFiles ++ potentialFiles).distinct.sorted

  private def computeTensionPolicy(
    features: PositionFeatures,
    breakReady: Boolean,
    advanceOrCapture: Boolean
  ): TensionPolicy =
    val tension = features.centralSpace.pawnTensionCount
    if tension == 0 then
      TensionPolicy.Ignore
    else if advanceOrCapture || (breakReady && !staticPosition(features)) then
      TensionPolicy.Release
    else
      TensionPolicy.Maintain

  private def staticPosition(features: PositionFeatures): Boolean =
    features.centralSpace.lockedCenter &&
      features.centralSpace.pawnTensionCount <= 3 &&
      features.lineControl.openFilesCount <= 2

  private def extractTensionEdges(board: Board): List[String] =
    val whitePawns = board.byPiece(Color.White, Pawn)
    val blackPawns = board.byPiece(Color.Black, Pawn)
    val whiteEdges =
      whitePawns.squares.flatMap { pawnSq =>
        (pawnSq.pawnAttacks(Color.White) & blackPawns).squares.map(targetSq => s"${pawnSq.key}-${targetSq.key}")
      }
    val blackEdges =
      blackPawns.squares.flatMap { pawnSq =>
        (pawnSq.pawnAttacks(Color.Black) & whitePawns).squares.map(targetSq => s"${pawnSq.key}-${targetSq.key}")
      }
    (whiteEdges ++ blackEdges).toList.distinct.sorted

  private def computePrimaryDriver(
    breakReady: Boolean,
    urgency: PassedPawnUrgency,
    advanceOrCapture: Boolean,
    tensionCount: Int,
    counterBreak: Boolean
  ): PawnPlayDriver =
    if urgency == PassedPawnUrgency.Critical then PawnPlayDriver.PassedPawn
    else if advanceOrCapture then PawnPlayDriver.TensionCritical
    else if breakReady then PawnPlayDriver.BreakReady
    else if tensionCount > 0 then PawnPlayDriver.TensionActive
    else if counterBreak then PawnPlayDriver.Defensive
    else PawnPlayDriver.Quiet

  private def colorMatches(motifColor: Color, isWhite: Boolean): Boolean =
    (isWhite && motifColor == Color.White) || (!isWhite && motifColor == Color.Black)

  private def isCentralFile(file: File): Boolean =
    file == File.C || file == File.D || file == File.E || file == File.F

  private def relativeRank(square: Square, color: Color): Int =
    if color.white then square.rank.value + 1 else 8 - square.rank.value

  private def fileOpennessBonus(board: Board, file: File): Int =
    val fileMask = Bitboard.file(file)
    val pawnsOnFile = (board.pawns & fileMask).count
    if pawnsOnFile == 0 then 25
    else if pawnsOnFile == 1 then 12
    else 0
