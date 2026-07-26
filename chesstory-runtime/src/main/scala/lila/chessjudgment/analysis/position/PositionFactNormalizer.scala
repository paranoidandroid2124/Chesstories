package lila.chessjudgment.analysis.position

import chess.{ Color, File, Role, Square }
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.model.Fact
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.position.PositionFeatures
import lila.chessjudgment.model.strategic.RookEndgameGeometry

object PositionFactNormalizer:

  def fromBoardFacts(
      id: String,
      facts: List[Fact],
      features: Option[PositionFeatures],
      position: PositionNodeRef,
      scope: EvidenceScope
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.BoardFactProducer,
        layer = EvidenceLayer.Board,
        position = position,
        line = None,
        scope = scope,
        confidence = EvidenceConfidence.BoardDerived
      )
    EvidenceRecord(
      ref = ref,
      payload = BoardFactEvidence(
        facts = facts,
        features = features,
        anchors = factAnchors(facts, features.map(_.sideToMove)) ++ features.toList.flatMap(featureAnchors),
        attackDefense = attackDefenseEntries(facts)
      )
    )

  private def attackDefenseEntries(facts: List[Fact]): List[BoardAttackDefenseEntry] =
    facts.flatMap {
      case Fact.HangingPiece(owner, square, role, attackers, defenders, _) =>
        Some(attackDefenseEntry(owner, square, role, attackers, defenders, loose = true))
      case Fact.TargetPiece(owner, square, role, attackers, defenders, _) =>
        Some(attackDefenseEntry(owner, square, role, attackers, defenders, loose = false))
      case _ =>
        None
    }.distinct

  private def attackDefenseEntry(
      owner: Color,
      square: Square,
      role: Role,
      attackers: List[Square],
      defenders: List[Square],
      loose: Boolean
  ): BoardAttackDefenseEntry =
    val attackCount = attackers.distinct.size
    val defenseCount = defenders.distinct.size
    val materialValue = MaterialValue.materialValueCp(role)
    BoardAttackDefenseEntry(
      square = evidenceSquare(square),
      occupantColor = owner,
      occupantRole = evidenceRole(role),
      attackerColor = !owner,
      attackerSquares = evidenceSquares(attackers),
      defenderSquares = evidenceSquares(defenders),
      attackCount = attackCount,
      defenseCount = defenseCount,
      pressureDelta = attackCount - defenseCount,
      materialValueCp = materialValue,
      isLoose = loose,
      isUnderdefended = !loose && attackCount > defenseCount
    )

  private def factAnchors(facts: List[Fact], perspective: Option[Color]): List[BoardAnchor] =
    val side = perspective.getOrElse(Color.White)
    facts.flatMap {
      case fact @ Fact.HangingPiece(owner, _, pieceRole, _, _, _) =>
        val focus = fact.squareFocus
        val value = MaterialValue.materialValueCp(pieceRole)
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.LooseMaterial,
            side = !owner,
            signal = BoardAnchorSignal.HangingPiece,
            magnitude = value,
            confidence = 0.84,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(owner),
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                subjectRole = Some(evidenceRole(pieceRole)),
                attackerColor = Some(!owner),
                attackerSquares = evidenceSquares(focus.attackerSquares),
                defenderSquares = evidenceSquares(focus.defenderSquares),
                axis = attackRayAxis(focus.attackerSquares, focus.subjectSquares.headOption),
                materialLossCp = Some(value)
              )
            )
          )
        )
      case fact @ Fact.TargetPiece(owner, _, pieceRole, _, _, _) =>
        val focus = fact.squareFocus
        val pressure = (focus.attackerSquares.size - focus.defenderSquares.size).max(1)
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.LooseMaterial,
            side = !owner,
            signal = BoardAnchorSignal.AttackedTarget,
            magnitude = pressure,
            confidence = if focus.defenderSquares.isEmpty then 0.78 else 0.64,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(owner),
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                subjectRole = Some(evidenceRole(pieceRole)),
                attackerColor = Some(!owner),
                attackerSquares = evidenceSquares(focus.attackerSquares),
                defenderSquares = evidenceSquares(focus.defenderSquares),
                axis = attackRayAxis(focus.attackerSquares, focus.subjectSquares.headOption),
                materialLossCp = Some(MaterialValue.materialValueCp(pieceRole))
              )
            )
          )
        )
      case fact @ Fact.Pin(attackerColor, attacker, attackerRole, _, pinnedRole, _, behindRole, isAbsolute, _) =>
        val focus = fact.squareFocus
        val valueGap = (MaterialValue.materialValueCp(behindRole) - MaterialValue.materialValueCp(pinnedRole)).max(1)
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.PinPressure,
            side = attackerColor,
            signal = if isAbsolute then BoardAnchorSignal.AbsolutePin else BoardAnchorSignal.RelativePin,
            magnitude = valueGap,
            confidence = if isAbsolute then 0.88 else 0.74,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(!attackerColor),
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                subjectRole = Some(evidenceRole(pinnedRole)),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                targetRole = Some(evidenceRole(behindRole)),
                attackerColor = Some(attackerColor),
                attackerSquare = Some(evidenceSquare(attacker)),
                attackerRole = Some(evidenceRole(attackerRole)),
                relatedSquares = evidenceSquares(focus.relatedSquares),
                isAbsolute = Some(isAbsolute)
              )
            )
          )
        )
      case fact @ Fact.Skewer(attackerColor, attacker, attackerRole, _, frontRole, _, backRole, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.SkewerPressure,
            side = attackerColor,
            signal = BoardAnchorSignal.SkewerLine,
            magnitude = (MaterialValue.materialValueCp(backRole) + MaterialValue.materialValueCp(frontRole)).max(1),
            confidence = 0.76,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(!attackerColor),
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                subjectRole = Some(evidenceRole(frontRole)),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                targetRole = Some(evidenceRole(backRole)),
                attackerColor = Some(attackerColor),
                attackerSquare = Some(evidenceSquare(attacker)),
                attackerRole = Some(evidenceRole(attackerRole)),
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case fact @ Fact.Fork(attackerColor, attacker, attackerRole, targets, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.ForkPressure,
            side = attackerColor,
            signal = BoardAnchorSignal.ForkTargets,
            magnitude = targets.map { case (_, role) => MaterialValue.materialValueCp(role) }.sum.max(targets.size),
            confidence = if targets.size >= 2 then 0.82 else 0.60,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(!attackerColor),
                attackerColor = Some(attackerColor),
                attackerSquare = Some(evidenceSquare(attacker)),
                attackerRole = Some(evidenceRole(attackerRole)),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                targetRole = targets.headOption.map { case (_, role) => evidenceRole(role) },
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case fact @ Fact.XRay(attackerColor, attacker, attackerRole, blocker, blockerRole, target, targetRole, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.XRayPressure,
            side = attackerColor,
            signal = BoardAnchorSignal.XRayLine,
            magnitude = MaterialValue.materialValueCp(targetRole).max(1),
            confidence = 0.72,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(!attackerColor),
                subjectSquare = Some(evidenceSquare(blocker)),
                subjectRole = Some(evidenceRole(blockerRole)),
                targetSquare = Some(evidenceSquare(target)),
                targetRole = Some(evidenceRole(targetRole)),
                attackerColor = Some(attackerColor),
                attackerSquare = Some(evidenceSquare(attacker)),
                attackerRole = Some(evidenceRole(attackerRole)),
                relatedSquares = evidenceSquares(focus.relatedSquares),
                axis = rayAxis(attacker, target)
              )
            )
          )
        )
      case fact @ Fact.Battery(attackerColor, front, frontRole, back, backRole, axis, _, target, targetRole) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.BatteryPressure,
            side = attackerColor,
            signal = BoardAnchorSignal.BatteryLine,
            magnitude = MaterialValue.materialValueCp(frontRole).max(MaterialValue.materialValueCp(backRole)).max(1),
            confidence = if axis == Fact.RayAxis.Diagonal then 0.72 else 0.70,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(if target.nonEmpty then !attackerColor else attackerColor),
                subjectSquare = target.map(evidenceSquare).orElse(Some(evidenceSquare(front))),
                subjectRole = targetRole.map(evidenceRole).orElse(Some(evidenceRole(frontRole))),
                attackerColor = Some(attackerColor),
                attackerSquare = Some(evidenceSquare(front)),
                attackerRole = Some(evidenceRole(frontRole)),
                relatedSquares = evidenceSquares(focus.relatedSquares),
                axis = Some(boardAnchorAxis(axis))
              )
            )
          )
        )
      case Fact.FileControl(file, color, open, _) =>
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.FileControl,
            side = color,
            signal = if open then BoardAnchorSignal.OpenFileAccess else BoardAnchorSignal.SemiOpenFileAccess,
            magnitude = 1,
            confidence = if open then 0.78 else 0.68,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(color),
                file = Some(evidenceFile(file)),
                axis = Some(BoardAnchorAxis.File)
              )
            )
          )
        )
      case Fact.SpaceAdvantage(color, pawnDelta, _) =>
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.Space,
            side = color,
            signal = BoardAnchorSignal.SpaceEdge,
            magnitude = pawnDelta,
            confidence = 0.72,
            detail = Some(BoardAnchorDetail(subjectColor = Some(color)))
          )
        )
      case fact @ Fact.WeakSquare(_, _, _, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.WeakSquare,
            side = side,
            signal = BoardAnchorSignal.WeakSquareHole,
            magnitude = 1,
            confidence = 0.68,
            detail = Some(
              BoardAnchorDetail(
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                targetSquare = focus.subjectSquares.headOption.map(evidenceSquare)
              )
            )
          )
        )
      case fact @ Fact.Outpost(_, pieceRole, _) =>
        val focus = fact.squareFocus
        val outpostSquare = focus.subjectSquares.headOption.map(evidenceSquare)
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.Outpost,
            side = side,
            signal = BoardAnchorSignal.OutpostSquare,
            magnitude = MaterialValue.materialValueCp(pieceRole).max(1),
            confidence = 0.72,
            detail = Some(
              BoardAnchorDetail(
                subjectSquare = outpostSquare,
                targetSquare = outpostSquare,
                subjectRole = Some(evidenceRole(pieceRole))
              )
            )
          )
        )
      case fact @ Fact.KingActivity(color, _, mobility, proximityToCenter, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = color,
            signal = BoardAnchorSignal.EndgameKingActivity,
            magnitude = mobility.max(proximityToCenter).max(1),
            confidence = 0.68,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(color),
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case fact @ Fact.Opposition(color, _, _, distance, isDirect, _, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = color,
            signal = BoardAnchorSignal.EndgameOpposition,
            magnitude = if isDirect then 3 else distance.max(1),
            confidence = if isDirect then 0.76 else 0.70,
            detail = Some(
              BoardAnchorDetail(
                subjectColor = Some(color),
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case fact @ Fact.RuleOfSquare(_, _, _, _, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = side,
            signal = BoardAnchorSignal.EndgameRuleOfSquare,
            magnitude = 2,
            confidence = 0.72,
            detail = Some(
              BoardAnchorDetail(
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case Fact.RookEndgamePattern(pattern, _, primaryPattern, anchorSquares, geometry) =>
        val anchors =
          if anchorSquares.nonEmpty then anchorSquares
          else geometry.toList.flatMap(_.anchorSquares)
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = geometry.map(_.techniqueSide).getOrElse(side),
            signal = BoardAnchorSignal.EndgameRookPattern,
            magnitude = 2,
            confidence = 0.74,
            detail = Some(rookEndgameAnchorDetail(pattern.toString, primaryPattern, anchors, geometry))
          )
        )
      case Fact.EndgameOutcome(_, confidence, _) =>
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = side,
            signal = BoardAnchorSignal.EndgameOutcomeHint,
            magnitude = (confidence * 10).round.toInt.max(1),
            confidence = math.max(0.0, math.min(confidence, 0.90))
          )
        )
      case Fact.Zugzwang(color, _) =>
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = color,
            signal = BoardAnchorSignal.EndgameZugzwang,
            magnitude = 3,
            confidence = 0.76,
            detail = Some(BoardAnchorDetail(subjectColor = Some(color)))
          )
        )
      case fact @ Fact.PawnPromotion(_, promotedTo, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = side,
            signal = BoardAnchorSignal.EndgamePromotion,
            magnitude = promotedTo.map(MaterialValue.materialValueCp).getOrElse(1).max(1),
            confidence = 0.72,
            detail = Some(
              BoardAnchorDetail(
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                targetRole = promotedTo.map(evidenceRole),
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case fact @ Fact.StalemateThreat(_, _) =>
        val focus = fact.squareFocus
        Some(
          BoardAnchor(
            kind = BoardAnchorKind.EndgameTechnique,
            side = side,
            signal = BoardAnchorSignal.EndgameStalemateResource,
            magnitude = 2,
            confidence = 0.72,
            detail = Some(
              BoardAnchorDetail(
                subjectSquare = focus.subjectSquares.headOption.map(evidenceSquare),
                targetSquare = focus.targetSquares.headOption.map(evidenceSquare),
                relatedSquares = evidenceSquares(focus.relatedSquares)
              )
            )
          )
        )
      case _ =>
        None
    }

  private def rookEndgameAnchorDetail(
      pattern: String,
      primaryPattern: Option[String],
      anchorSquares: List[Square],
      geometry: Option[RookEndgameGeometry]
  ): BoardAnchorDetail =
    val geometryTags =
      geometry.toList.flatMap { g =>
        List(
          Some(s"technique-side:${colorKey(g.techniqueSide)}"),
          Some(s"strong-side:${colorKey(g.strongSide)}"),
          Some(s"defending-side:${colorKey(g.defendingSide)}"),
          Some(s"passed-pawn:${g.passedPawn.key}"),
          Some(s"promotion-square:${g.promotionSquare.key}"),
          Some(s"pawn-file:${fileKey(g.passedPawn.file)}"),
          g.attackingKing.map(square => s"attacking-king:${square.key}"),
          g.defendingKing.map(square => s"defending-king:${square.key}"),
          g.attackingRook.map(square => s"attacking-rook:${square.key}"),
          g.defendingRook.map(square => s"defending-rook:${square.key}"),
          g.barrierRank.map(rank => s"barrier-rank:${rank.value + 1}")
        ).flatten
      }
    BoardAnchorDetail(
      subjectColor = geometry.map(_.techniqueSide),
      subjectSquare = geometry.flatMap(_.techniqueKing).map(evidenceSquare),
      targetSquare = geometry.map(_.promotionSquare).map(evidenceSquare),
      attackerColor = geometry.map(_.strongSide),
      attackerSquare = geometry.flatMap(_.attackingRook).map(evidenceSquare),
      attackerRole = geometry.flatMap(g => Option.when(g.attackingRook.nonEmpty)(evidenceRole(chess.Rook))),
      defenderSquares = evidenceSquares(geometry.toList.flatMap(g => g.defendingKing.toList ++ g.defendingRook.toList)),
      relatedSquares = evidenceSquares(anchorSquares),
      file = geometry.map(g => evidenceFile(g.passedPawn.file)),
      axis = geometry.map(_ => BoardAnchorAxis.File),
      tags = (List(s"rook-pattern:$pattern") ++ primaryPattern.map(pattern => s"pattern:$pattern") ++ geometryTags).distinct
    )

  private def featureAnchors(features: PositionFeatures): List[BoardAnchor] =
    val side = features.sideToMove
    val opponent = !side
    val centerEdge = features.centralSpace.whiteCenterControl - features.centralSpace.blackCenterControl
    val sideCenterEdge = if side.white then centerEdge else -centerEdge
    val spaceEdge = if side.white then features.centralSpace.spaceDiff else -features.centralSpace.spaceDiff
    val developmentEdge = developmentLead(features, side)
    val mobilityEdge = mobilityLead(features, side)
    val opponentLowMobility = lowMobility(features, opponent)
    val opponentLowMobilitySquares = lowMobilitySquares(features, opponent).map(EvidenceSquare(_))
    val sideSemiOpenFiles = semiOpenFiles(features, side)
    val sideRookOnSeventh = rookOnSeventh(features, side)
    val opponentExposure = kingExposure(features, opponent)
    val opponentKingPressure = kingPressure(features, opponent)
    List(
      Option.when(sideCenterEdge >= 2)(
        BoardAnchor(BoardAnchorKind.CenterControl, side, BoardAnchorSignal.CenterControlEdge, sideCenterEdge, 0.72)
      ),
      Option.when(spaceEdge >= 2)(
        BoardAnchor(BoardAnchorKind.Space, side, BoardAnchorSignal.SpaceEdge, spaceEdge, 0.72)
      ),
      Option.when(developmentEdge >= 2)(
        BoardAnchor(BoardAnchorKind.Development, side, BoardAnchorSignal.DevelopmentLead, developmentEdge, 0.74)
      ),
      Option.when(sideSemiOpenFiles > 0)(
        BoardAnchor(BoardAnchorKind.FileControl, side, BoardAnchorSignal.SemiOpenFileAccess, sideSemiOpenFiles, 0.68)
      ),
      Option.when(sideRookOnSeventh)(
        BoardAnchor(BoardAnchorKind.FileControl, side, BoardAnchorSignal.RookOnSeventh, 1, 0.82)
      ),
      Option.when(mobilityEdge >= 5)(
        BoardAnchor(BoardAnchorKind.Activity, side, BoardAnchorSignal.MobilityEdge, mobilityEdge, 0.70)
      ),
      Option.when(opponentLowMobility >= 2)(
        BoardAnchor(
          BoardAnchorKind.CounterplayRestraint,
          side,
          BoardAnchorSignal.OpponentLowMobility,
          opponentLowMobility,
          0.72,
          detail = Some(BoardAnchorDetail(subjectColor = Some(opponent), relatedSquares = opponentLowMobilitySquares))
        )
      ),
      Option.when(opponentExposure >= 2)(
        BoardAnchor(BoardAnchorKind.KingSafety, side, BoardAnchorSignal.KingExposure, opponentExposure, 0.72)
      ),
      Option.when(opponentKingPressure >= 4)(
        BoardAnchor(BoardAnchorKind.KingSafety, side, BoardAnchorSignal.KingPressure, opponentKingPressure, 0.74)
      ),
      Option.when(features.pawns.whiteIQP || features.pawns.blackIQP || features.pawns.whiteHangingPawns || features.pawns.blackHangingPawns)(
        BoardAnchor(BoardAnchorKind.PawnStructure, side, BoardAnchorSignal.PawnStructureShape, 1, 0.68)
      )
    ).flatten

  private def developmentLead(features: PositionFeatures, side: chess.Color): Int =
    if side.white then features.activity.blackDevelopmentLag - features.activity.whiteDevelopmentLag
    else features.activity.whiteDevelopmentLag - features.activity.blackDevelopmentLag

  private def mobilityLead(features: PositionFeatures, side: chess.Color): Int =
    if side.white then features.activity.whitePseudoMobility - features.activity.blackPseudoMobility
    else features.activity.blackPseudoMobility - features.activity.whitePseudoMobility

  private def lowMobility(features: PositionFeatures, side: chess.Color): Int =
    if side.white then features.activity.whiteLowMobilityPieces else features.activity.blackLowMobilityPieces

  private def lowMobilitySquares(features: PositionFeatures, side: chess.Color): List[String] =
    if side.white then features.activity.whiteLowMobilitySquares else features.activity.blackLowMobilitySquares

  private def semiOpenFiles(features: PositionFeatures, side: chess.Color): Int =
    if side.white then features.lineControl.whiteSemiOpenFiles else features.lineControl.blackSemiOpenFiles

  private def rookOnSeventh(features: PositionFeatures, side: chess.Color): Boolean =
    if side.white then features.lineControl.whiteRookOn7th else features.lineControl.blackRookOn7th

  private def kingExposure(features: PositionFeatures, side: chess.Color): Int =
    if side.white then features.kingSafety.whiteKingExposedFiles else features.kingSafety.blackKingExposedFiles

  private def kingPressure(features: PositionFeatures, side: chess.Color): Int =
    if side.white then features.kingSafety.whiteAttackersCount else features.kingSafety.blackAttackersCount

  private def evidenceSquare(square: Square): EvidenceSquare =
    EvidenceSquare(square.key)

  private def evidenceFile(file: File): EvidenceFile =
    EvidenceFile(fileKey(file))

  private def evidenceSquares(squares: List[Square]): List[EvidenceSquare] =
    squares.distinct.map(evidenceSquare)

  private def evidenceRole(role: Role): EvidencePieceRole =
    EvidencePieceRole(role.name)

  private def boardAnchorAxis(axis: Fact.RayAxis): BoardAnchorAxis =
    axis match
      case Fact.RayAxis.File     => BoardAnchorAxis.File
      case Fact.RayAxis.Rank     => BoardAnchorAxis.Rank
      case Fact.RayAxis.Diagonal => BoardAnchorAxis.Diagonal

  private def rayAxis(from: Square, to: Square): Option[BoardAnchorAxis] =
    val fileDiff = to.file.value - from.file.value
    val rankDiff = to.rank.value - from.rank.value
    if fileDiff == 0 then Some(BoardAnchorAxis.File)
    else if rankDiff == 0 then Some(BoardAnchorAxis.Rank)
    else if fileDiff.abs == rankDiff.abs then Some(BoardAnchorAxis.Diagonal)
    else None

  private def attackRayAxis(attackers: List[Square], target: Option[Square]): Option[BoardAnchorAxis] =
    target.flatMap { square =>
      val attackerAxes = attackers.distinct.map(attacker => rayAxis(attacker, square))
      val axes = attackerAxes.flatten.distinct
      if attackerAxes.nonEmpty && attackerAxes.forall(_.nonEmpty) then
        axes match
          case axis :: Nil => Some(axis)
          case _           => None
      else None
    }

  private def colorKey(color: Color): String =
    if color.white then "white" else "black"

  private def fileKey(file: File): String =
    file match
      case File.A => "a"
      case File.B => "b"
      case File.C => "c"
      case File.D => "d"
      case File.E => "e"
      case File.F => "f"
      case File.G => "g"
      case File.H => "h"
