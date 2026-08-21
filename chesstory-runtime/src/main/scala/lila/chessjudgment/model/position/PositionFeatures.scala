package lila.chessjudgment.model.position

import chess.*

final case class PawnStructureFeatures(
    whitePawnCount: Int,
    blackPawnCount: Int,
    whiteIsolatedPawns: Int,
    blackIsolatedPawns: Int,
    whiteDoubledPawns: Int,
    blackDoubledPawns: Int,
    whitePassedPawns: Int,
    blackPassedPawns: Int,
    whiteIQP: Boolean,
    blackIQP: Boolean,
    whiteHangingPawns: Boolean,
    blackHangingPawns: Boolean,
    whiteBackwardPawns: Int,
    blackBackwardPawns: Int,
    whitePawnIslands: Int,
    blackPawnIslands: Int,
    whiteConnectedPawns: Int,
    blackConnectedPawns: Int,
    whitePassedPawnRank: Int,
    blackPassedPawnRank: Int,
    whiteProtectedPassedPawns: Int,
    blackProtectedPassedPawns: Int
)

final case class ActivityFeatures(
    whiteLegalMoves: Int,
    blackLegalMoves: Int,
    whiteMinorPieceMobility: Int,
    blackMinorPieceMobility: Int,
    whitePseudoMobility: Int,
    blackPseudoMobility: Int,
    whiteLowMobilityPieces: Int,
    blackLowMobilityPieces: Int,
    whiteAttackedPieces: Int,
    blackAttackedPieces: Int,
    whiteDevelopmentLag: Int,
    blackDevelopmentLag: Int,
    whiteLowMobilitySquares: List[String] = Nil,
    blackLowMobilitySquares: List[String] = Nil
)

final case class KingSafetyFeatures(
    whiteCastlingRights: String,
    blackCastlingRights: String,
    whiteKingShield: Int,
    blackKingShield: Int,
    whiteKingExposedFiles: Int,
    blackKingExposedFiles: Int,
    whiteBackRankWeakness: Boolean,
    blackBackRankWeakness: Boolean,
    whiteAttackersCount: Int,
    blackAttackersCount: Int,
    whiteEscapeSquares: Int,
    blackEscapeSquares: Int,
    whiteKingRingAttacked: Int,
    blackKingRingAttacked: Int
)

final case class MaterialPhaseFeatures(
    whiteMaterial: Int,
    blackMaterial: Int,
    materialDiff: Int,
    phase: String
)

final case class LineControlFeatures(
    openFilesCount: Int,
    whiteSemiOpenFiles: Int,
    blackSemiOpenFiles: Int,
    whiteRookOn7th: Boolean,
    blackRookOn7th: Boolean
)

final case class MaterialImbalanceFeatures(
    whiteKnights: Int,
    blackKnights: Int,
    whiteBishops: Int,
    blackBishops: Int,
    whiteRooks: Int,
    blackRooks: Int,
    whiteQueens: Int,
    blackQueens: Int,
    whiteBishopPair: Boolean,
    blackBishopPair: Boolean
)

final case class CentralSpaceFeatures(
    whiteCentralPawns: Int,
    blackCentralPawns: Int,
    whiteCenterControl: Int,
    blackCenterControl: Int,
    spaceDiff: Int,
    pawnTensionCount: Int,
    lockedCenter: Boolean,
    openCenter: Boolean
)

final case class PositionFeatures(
    fen: String,
    sideToMove: Color,
    plyCount: Int,
    pawns: PawnStructureFeatures,
    activity: ActivityFeatures,
    kingSafety: KingSafetyFeatures,
    materialPhase: MaterialPhaseFeatures,
    lineControl: LineControlFeatures,
    imbalance: MaterialImbalanceFeatures,
    centralSpace: CentralSpaceFeatures,
    strategicState: StrategicStateFeatures
)

final case class StrategicStateFeatures(
    whiteEntrenchedPieces: Int,
    blackEntrenchedPieces: Int,
    whiteRookPawnMarchReady: Boolean,
    blackRookPawnMarchReady: Boolean,
    whiteHookCreationChance: Boolean,
    blackHookCreationChance: Boolean,
    whiteColorComplexClamp: Boolean,
    blackColorComplexClamp: Boolean
)

object PositionFeatures:
  def empty: PositionFeatures = PositionFeatures(
    fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    sideToMove = Color.White,
    plyCount = 0,
    pawns = PawnStructureFeatures(0, 0, 0, 0, 0, 0, 0, 0, false, false, false, false, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    activity = ActivityFeatures(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    kingSafety = KingSafetyFeatures("", "", 0, 0, 0, 0, false, false, 0, 0, 0, 0, 0, 0),
    materialPhase = MaterialPhaseFeatures(0, 0, 0, "opening"),
    lineControl = LineControlFeatures(0, 0, 0, false, false),
    imbalance = MaterialImbalanceFeatures(0, 0, 0, 0, 0, 0, 0, 0, false, false),
    centralSpace = CentralSpaceFeatures(0, 0, 0, 0, 0, 0, false, false),
    strategicState = StrategicStateFeatures.empty
  )

object StrategicStateFeatures:
  val empty: StrategicStateFeatures = StrategicStateFeatures(
    whiteEntrenchedPieces = 0,
    blackEntrenchedPieces = 0,
    whiteRookPawnMarchReady = false,
    blackRookPawnMarchReady = false,
    whiteHookCreationChance = false,
    blackHookCreationChance = false,
    whiteColorComplexClamp = false,
    blackColorComplexClamp = false
  )

/** Shared board-value primitive for passed-pawn topology. */
private[chessjudgment] object PawnTopology:
  def passedPawns(color: Color, pawns: Bitboard, opponentPawns: Bitboard): List[Square] =
    pawns.squares.filter { pawn =>
      val files = List(pawn.file.value - 1, pawn.file.value, pawn.file.value + 1).filter(index => index >= 0 && index <= 7)
      !opponentPawns.squares.exists { opponent =>
        files.contains(opponent.file.value) &&
          (if color == Color.White then opponent.rank.value > pawn.rank.value
           else opponent.rank.value < pawn.rank.value)
      }
    }.toList
