package lila.chessjudgment.analysis.tactical

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

final class RelationWitness private (
    val details: RelationDetails,
    val lineMoves: List[String]
):
  def kind: String =
    RelationDetails.kind(details).get
  def focusSquares: List[String] =
    RelationDetails.focusSquares(details)
  def targetSquare: Option[String] =
    RelationDetails.targetSquare(details)
  def copy(
      details: RelationDetails = this.details,
      lineMoves: List[String] = this.lineMoves
  ): RelationWitness =
    RelationWitness
      .fromDetails(details, lineMoves)
      .getOrElse(throw new IllegalArgumentException("relation witness requires canonical non-empty details"))
  override def equals(other: Any): Boolean =
    other match
      case that: RelationWitness =>
        details == that.details && lineMoves == that.lineMoves
      case _ =>
        false
  override def hashCode: Int =
    31 * details.hashCode + lineMoves.hashCode

object RelationWitness:
  def apply(
      kind: String,
      focusSquares: List[String],
      lineMoves: List[String],
      targetSquare: Option[String] = None,
      details: RelationDetails = RelationDetails.Empty
  ): RelationWitness =
    val witness =
      fromDetails(details, lineMoves)
        .getOrElse(throw new IllegalArgumentException("relation witness requires canonical non-empty details"))
    require(
      RelationDetails.normalizeKind(kind) == witness.kind,
      s"relation witness kind '$kind' contradicts canonical detail kind '${witness.kind}'"
    )
    val declaredTarget = targetSquare.map(RelationDetails.normalizeSquare).filter(_.nonEmpty)
    require(
      declaredTarget.forall(witness.targetSquare.contains),
      s"relation witness target '${declaredTarget.getOrElse("")}' contradicts canonical detail target '${witness.targetSquare.getOrElse("")}'"
    )
    val _ = focusSquares
    witness

  def fromDetails(
      details: RelationDetails,
      lineMoves: List[String]
  ): Option[RelationWitness] =
    RelationDetails
      .kind(details)
      .map(_ => new RelationWitness(details = details, lineMoves = lineMoves))

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
      terminalMove: String
  ) extends RelationDetails
  final case class PerpetualCheck(
      checkedKingSquare: String,
      checkerSquares: List[String],
      checkingSide: String,
      entryMove: String,
      cycleStartMove: String,
      cycleReturnMove: String,
      repeatedPositionKey: String
  ) extends RelationDetails

  def kind(details: RelationDetails): Option[String] =
    details match
      case Empty                  => None
      case _: DefenderTrade       => Some(RelationKind.DefenderTrade)
      case _: BadPieceLiquidation => Some(RelationKind.BadPieceLiquidation)
      case _: Overload            => Some(RelationKind.Overload)
      case _: Deflection          => Some(RelationKind.Deflection)
      case _: DiscoveredAttack    => Some(RelationKind.DiscoveredAttack)
      case _: DoubleCheck         => Some(RelationKind.DoubleCheck)
      case MatePattern(relationKind, _, _, _, _) =>
        Option
          .when(Set(RelationKind.BackRankMate, RelationKind.MateNet)(normalizeKind(relationKind)))(
            normalizeKind(relationKind)
          )
      case _: GreekGift      => Some(RelationKind.GreekGift)
      case _: Fork           => Some(RelationKind.Fork)
      case _: HangingPiece   => Some(RelationKind.HangingPiece)
      case _: Decoy          => Some(RelationKind.Decoy)
      case _: Interference   => Some(RelationKind.Interference)
      case _: Clearance      => Some(RelationKind.Clearance)
      case _: XRay           => Some(RelationKind.XRay)
      case _: Battery        => Some(RelationKind.Battery)
      case _: Pin            => Some(RelationKind.Pin)
      case _: Skewer         => Some(RelationKind.Skewer)
      case _: Zwischenzug    => Some(RelationKind.Zwischenzug)
      case _: Domination     => Some(RelationKind.Domination)
      case _: TrappedPiece   => Some(RelationKind.TrappedPiece)
      case _: StalemateTrap  => Some(RelationKind.StalemateTrap)
      case _: PerpetualCheck => Some(RelationKind.PerpetualCheck)

  def focusSquares(details: RelationDetails): List[String] =
    val squares =
      details match
        case Empty =>
          Nil
        case DefenderTrade(_, exchangeSquare, targetSquare) =>
          List(targetSquare, exchangeSquare)
        case BadPieceLiquidation(badPieceSquare, exchangeSquare) =>
          List(badPieceSquare, exchangeSquare)
        case Overload(defenderSquare, targetSquares, _) =>
          defenderSquare :: targetSquares
        case Deflection(defenderSquare, targetSquare, attackerSquare) =>
          List(targetSquare, defenderSquare, attackerSquare)
        case DiscoveredAttack(attackerSquare, clearedSquare, targetSquare, _) =>
          List(attackerSquare, clearedSquare, targetSquare)
        case DoubleCheck(kingSquare, checkerSquares, _, _) =>
          kingSquare :: checkerSquares
        case MatePattern(_, kingSquare, checkerSquares, _, _) =>
          kingSquare :: checkerSquares
        case GreekGift(bishopSquare, targetSquare, _, _) =>
          List(bishopSquare, targetSquare)
        case Fork(attackerSquare, _, targets) =>
          attackerSquare :: targets.map(_.square)
        case HangingPiece(attackerSquare, targetSquare, _, _) =>
          List(attackerSquare, targetSquare)
        case TrappedPiece(attackerSquare, targetSquare, _, _) =>
          List(attackerSquare, targetSquare)
        case Domination(attackerSquare, targetSquare, _, _, controlledEscapeSquares) =>
          attackerSquare :: targetSquare :: controlledEscapeSquares
        case Zwischenzug(_, expectedRecaptureSquare, checkingPieceSquare, _, checkedKingSquare, _) =>
          List(checkingPieceSquare, expectedRecaptureSquare, checkedKingSquare)
        case Decoy(baitFromSquare, baitSquare, luredFromSquare, _, _, _, _) =>
          List(baitFromSquare, baitSquare, luredFromSquare)
        case XRay(attackerSquare, blockerSquare, targetSquare, _, _, _) =>
          List(attackerSquare, blockerSquare, targetSquare)
        case Clearance(beneficiarySquare, clearedSquare, targetSquare, _, _) =>
          List(beneficiarySquare, clearedSquare, targetSquare)
        case Battery(frontSquare, backSquare, targetSquare, _, _, _) =>
          List(frontSquare, backSquare, targetSquare)
        case Interference(blockerSquare, defenderSquare, targetSquare, _, _, _) =>
          List(blockerSquare, defenderSquare, targetSquare)
        case Pin(attackerSquare, pinnedSquare, behindSquare, _, _, _, _, _) =>
          List(attackerSquare, pinnedSquare, behindSquare)
        case Skewer(attackerSquare, frontSquare, backSquare, _, _, _, _) =>
          List(attackerSquare, frontSquare, backSquare)
        case StalemateTrap(stalematedKingSquare, resourceSquare, _, _) =>
          List(stalematedKingSquare, resourceSquare)
        case PerpetualCheck(checkedKingSquare, checkerSquares, _, _, _, _, _) =>
          checkedKingSquare :: checkerSquares
    squares.map(normalizeSquare).filter(_.nonEmpty).distinct

  def targetSquare(details: RelationDetails): Option[String] =
    val target =
      details match
        case Empty =>
          None
        case DefenderTrade(_, _, targetSquare) =>
          Some(targetSquare)
        case BadPieceLiquidation(_, exchangeSquare) =>
          Some(exchangeSquare)
        case Overload(_, targetSquares, _) =>
          targetSquares.headOption
        case Deflection(_, targetSquare, _) =>
          Some(targetSquare)
        case DiscoveredAttack(_, _, targetSquare, _) =>
          Some(targetSquare)
        case DoubleCheck(kingSquare, _, _, _) =>
          Some(kingSquare)
        case MatePattern(_, kingSquare, _, _, _) =>
          Some(kingSquare)
        case GreekGift(_, targetSquare, _, _) =>
          Some(targetSquare)
        case Fork(_, _, targets) =>
          targets.headOption.map(_.square)
        case HangingPiece(_, targetSquare, _, _) =>
          Some(targetSquare)
        case TrappedPiece(_, targetSquare, _, _) =>
          Some(targetSquare)
        case Domination(_, targetSquare, _, _, _) =>
          Some(targetSquare)
        case Zwischenzug(_, expectedRecaptureSquare, _, _, _, _) =>
          Some(expectedRecaptureSquare)
        case Decoy(_, baitSquare, _, _, _, _, _) =>
          Some(baitSquare)
        case XRay(_, _, targetSquare, _, _, _) =>
          Some(targetSquare)
        case Clearance(_, _, targetSquare, _, _) =>
          Some(targetSquare)
        case Battery(_, _, targetSquare, _, _, _) =>
          Some(targetSquare)
        case Interference(_, _, targetSquare, _, _, _) =>
          Some(targetSquare)
        case Pin(_, _, _, targetSquare, _, _, _, _) =>
          Some(targetSquare)
        case Skewer(_, _, _, targetSquare, _, _, _) =>
          Some(targetSquare)
        case StalemateTrap(stalematedKingSquare, _, _, _) =>
          Some(stalematedKingSquare)
        case PerpetualCheck(checkedKingSquare, _, _, _, _, _, _) =>
          Some(checkedKingSquare)
    target.map(normalizeSquare).filter(_.nonEmpty)

  private[tactical] def normalizeKind(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

  private[tactical] def normalizeSquare(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

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
