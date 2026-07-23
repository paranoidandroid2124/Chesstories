package lila.chessjudgment.analysis.tactical

import _root_.chess.{ Move, Position, Role }

final case class BoundedReplayStep(
    uci: String,
    before: Position,
    move: Move,
    after: Position,
    capturedRole: Option[Role]
)

final case class DefenderTradeBranch(
    defenderSquare: String,
    exchangeSquare: String,
    targetSquare: String,
    lineMoves: List[String]
)

final case class BadPieceLiquidationBranch(
    badPieceSquare: String,
    exchangeSquare: String,
    lineMoves: List[String]
)

final case class RelationWitness(
    kind: String,
    focusSquares: List[String],
    lineMoves: List[String],
    targetSquare: Option[String] = None,
    details: RelationDetails = RelationDetails.Empty
)

enum RelationThreatType:
  case MateCheck
  case Check

enum RelationAxis:
  case File
  case Rank
  case Diagonal

sealed trait RelationDetails
object RelationDetails:
  case object Empty extends RelationDetails
  final case class DefenderTrade(
      defenderSquare: String,
      exchangeSquare: String,
      targetSquare: String
  ) extends RelationDetails
  final case class BadPieceLiquidation(
      badPieceSquare: String,
      exchangeSquare: String
  ) extends RelationDetails
  final case class Overload(
      defenderSquare: String,
      targetSquares: List[String],
      attackerSquare: String
  ) extends RelationDetails
  final case class Deflection(
      defenderSquare: String,
      targetSquare: String,
      attackerSquare: String
  ) extends RelationDetails
  final case class DiscoveredAttack(
      attackerSquare: String,
      clearedSquare: String,
      targetSquare: String,
      attackerRole: String
  ) extends RelationDetails
  final case class DoubleCheck(
      kingSquare: String,
      checkerSquares: List[String],
      moverSquare: String,
      moverRole: String
  ) extends RelationDetails
  final case class MatePattern(
      relationKind: String,
      kingSquare: String,
      checkerSquares: List[String],
      matingMove: String,
      patternId: Option[String]
  ) extends RelationDetails
  final case class GreekGift(
      bishopSquare: String,
      targetSquare: String,
      entryMove: String,
      patternId: String
  ) extends RelationDetails
  final case class TargetPiece(square: String, role: String)
  final case class Fork(
      attackerSquare: String,
      attackerRole: String,
      targets: List[TargetPiece]
  ) extends RelationDetails
  final case class HangingPiece(
      attackerSquare: String,
      targetSquare: String,
      attackerRole: String,
      targetRole: String
  ) extends RelationDetails
  final case class TrappedPiece(
      attackerSquare: String,
      targetSquare: String,
      attackerRole: String,
      targetRole: String
  ) extends RelationDetails
  final case class Domination(
      attackerSquare: String,
      targetSquare: String,
      attackerRole: String,
      targetRole: String,
      controlledEscapeSquares: List[String]
  ) extends RelationDetails
  final case class Zwischenzug(
      intermediateMove: String,
      expectedRecaptureSquare: String,
      checkingPieceSquare: String,
      checkingPieceRole: String,
      checkedKingSquare: String,
      threatType: RelationThreatType
  ) extends RelationDetails
  final case class Decoy(
      baitFromSquare: String,
      baitSquare: String,
      luredFromSquare: String,
      executionFromSquare: String,
      executionToSquare: String,
      baitRole: String,
      luredRole: String
  ) extends RelationDetails
  final case class XRay(
      attackerSquare: String,
      blockerSquare: String,
      targetSquare: String,
      attackerRole: String,
      blockerRole: String,
      targetRole: String
  ) extends RelationDetails
  final case class Clearance(
      beneficiarySquare: String,
      clearedSquare: String,
      targetSquare: String,
      beneficiaryRole: String,
      clearingTo: String
  ) extends RelationDetails
  final case class Battery(
      frontSquare: String,
      backSquare: String,
      targetSquare: String,
      frontRole: String,
      backRole: String,
      axis: RelationAxis
  ) extends RelationDetails
  final case class Interference(
      blockerSquare: String,
      defenderSquare: String,
      targetSquare: String,
      blockerRole: String,
      defenderRole: String,
      targetRole: String
  ) extends RelationDetails
  final case class Pin(
      attackerSquare: String,
      pinnedSquare: String,
      behindSquare: String,
      targetSquare: String,
      attackerRole: String,
      pinnedRole: String,
      behindRole: String,
      absolute: Boolean
  ) extends RelationDetails
  final case class Skewer(
      attackerSquare: String,
      frontSquare: String,
      backSquare: String,
      targetSquare: String,
      attackerRole: String,
      frontRole: String,
      backRole: String
  ) extends RelationDetails
  final case class StalemateTrap(
      stalematedKingSquare: String,
      resourceSquare: String,
      entryMove: String,
      terminalMove: String,
      scoreCp: Option[Int]
  ) extends RelationDetails
  final case class PerpetualCheck(
      checkedKingSquare: String,
      checkerSquares: List[String],
      checkingSide: String,
      entryMove: String,
      cycleStartMove: String,
      cycleReturnMove: String,
      repeatedPositionKey: String,
      scoreCp: Option[Int]
  ) extends RelationDetails

object RelationKind:
  val DefenderTrade = "defender_trade"
  val BadPieceLiquidation = "bad_piece_liquidation"
  val Overload = "overload"
  val Deflection = "deflection"
  val DiscoveredAttack = "discovered_attack"
  val DoubleCheck = "double_check"
  val BackRankMate = "back_rank_mate"
  val MateNet = "mate_net"
  val Fork = "fork"
  val HangingPiece = "hanging_piece"
  val Decoy = "decoy"
  val Interference = "interference"
  val Clearance = "clearance"
  val XRay = "xray"
  val Battery = "battery"
  val Pin = "pin"
  val Skewer = "skewer"
  val Zwischenzug = "zwischenzug"
  val Domination = "domination"
  val TrappedPiece = "trapped_piece"
  val GreekGift = "greek_gift"
  val StalemateTrap = "stalemate_trap"
  val PerpetualCheck = "perpetual_check"
