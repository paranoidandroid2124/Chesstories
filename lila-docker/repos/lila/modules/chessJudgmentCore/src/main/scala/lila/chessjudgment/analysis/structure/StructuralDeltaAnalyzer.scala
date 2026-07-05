package lila.chessjudgment.analysis.structure

import _root_.chess.{ Bishop, Board, Color, King, Knight, Pawn, Queen, Role, Rook, Square }
import _root_.chess.format.{ Fen, Uci }

import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.analysis.position.{ PositionAnalyzer, PositionFeatures }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.{
  StructuralDevelopmentChoice,
  StructuralSignal,
  StructuralSignalKind,
  StructuralSignalPolarity,
  TransitionConsequence,
  TransitionConsequenceKind
}

private[chessjudgment] final case class TransitionStructuralDelta(
    openedFiles: List[String] = Nil,
    semiOpenedFiles: List[String] = Nil,
    newWeakPawns: List[String] = Nil,
    newWeakSquares: List[String] = Nil,
    createdTension: List[String] = Nil,
    resolvedTension: List[String] = Nil,
    pawnTensionBefore: Int = 0,
    pawnTensionAfter: Int = 0,
    targetPressureDelta: Int = 0,
    fileAccessDelta: Int = 0,
    kingShelterDelta: Int = 0,
    kingShelterFiles: List[String] = Nil,
    kingTargetSubjects: List[String] = Nil,
    mobilityDelta: Int = 0,
    developmentDelta: Int = 0,
    centerControlDelta: Int = 0,
    centerControlSquares: List[String] = Nil,
    lineUnlockDelta: Int = 0,
    lineUnlocks: List[String] = Nil,
    developmentMoves: List[DevelopmentMoveDelta] = Nil,
    fileOccupation: List[String] = Nil,
    releasedTargetPressure: List[String] = Nil,
    createdTargetPressure: List[String] = Nil,
    passedPawnCreated: List[String] = Nil,
    passedPawnAdvanced: List[String] = Nil,
    passedPawnLost: List[String] = Nil,
    promotionPressureDelta: Int = 0,
    outpostCreated: List[String] = Nil,
    outpostRemoved: List[String] = Nil,
    rookLiftCreated: List[String] = Nil,
    batteryCreated: List[String] = Nil,
    blockedOwnRays: List[String] = Nil,
    opponentMobilityRestrictions: List[String] = Nil,
    kingRingPressureDelta: Int = 0
):
  def pawnTensionDelta: Int = pawnTensionAfter - pawnTensionBefore
  def targetPressureRelease: Int = releasedTargetPressure.size
  def targetPressureGain: Int = createdTargetPressure.size

private[chessjudgment] final case class DevelopmentMoveDelta(
    role: String,
    from: String,
    to: String,
    fromBackRank: Boolean,
    toBackRank: Boolean,
    destinationCenterDistance: Int,
    mobilityDelta: Int,
    centerControlDelta: Int,
    defendedAfter: Boolean,
    enemyAttackersAfter: Int
)

private[chessjudgment] object StructuralDeltaContracts:
  import StructuralSignalKind.*
  import StructuralSignalPolarity.*
  import TransitionConsequenceKind.*

  def signals(delta: TransitionStructuralDelta): List[StructuralSignal] =
    val developedPieces = developedPieceMoves(delta)
    List(
      signal(FileOpened, Gain, delta.openedFiles.size, delta.openedFiles),
      signal(SemiOpenFileCreated, Gain, delta.semiOpenedFiles.size, delta.semiOpenedFiles),
      signedSignal(FileAccessChanged, delta.fileAccessDelta, fileAccessSubjects(delta)),
      signal(FileOccupied, Gain, delta.fileOccupation.size, delta.fileOccupation),
      signal(WeakPawnCreated, Gain, delta.newWeakPawns.size, delta.newWeakPawns),
      signal(WeakSquareCreated, Gain, delta.newWeakSquares.size, delta.newWeakSquares),
      signal(PawnTensionCreated, Gain, delta.createdTension.size, delta.createdTension),
      signal(PawnTensionResolved, Gain, delta.resolvedTension.size, delta.resolvedTension),
      signedSignal(PawnTensionChanged, delta.pawnTensionDelta, Nil),
      signal(TargetPressureCreated, Gain, delta.createdTargetPressure.size, delta.createdTargetPressure),
      signal(TargetPressureReleased, Loss, delta.releasedTargetPressure.size, delta.releasedTargetPressure),
      signedSignal(TargetPressureChanged, delta.targetPressureDelta, Nil),
      signedSignal(CenterControlChanged, delta.centerControlDelta, centerControlSubjects(delta)),
      signedSignal(DevelopmentChanged, delta.developmentDelta, Nil),
      signal(
        DevelopmentChoice,
        Gain,
        developedPieces.size,
        developedPieces.map(developmentMoveLabel)
      ),
      signedSignal(MobilityChanged, delta.mobilityDelta, Nil),
      signedSignal(KingSafetyChanged, delta.kingShelterDelta, kingShelterSubjects(delta)),
      signal(LineUnlocked, Gain, delta.lineUnlockDelta, delta.lineUnlocks),
      signal(PassedPawnCreated, Gain, delta.passedPawnCreated.size, delta.passedPawnCreated),
      signal(PassedPawnAdvanced, Gain, delta.passedPawnAdvanced.size, delta.passedPawnAdvanced),
      signedSignal(PromotionPressureChanged, delta.promotionPressureDelta, promotionPressureSubjects(delta)),
      signal(OutpostCreated, Gain, delta.outpostCreated.size, delta.outpostCreated),
      signal(OutpostRemoved, Loss, delta.outpostRemoved.size, delta.outpostRemoved),
      signal(RookLiftCreated, Gain, delta.rookLiftCreated.size, delta.rookLiftCreated),
      signal(BatteryCreated, Gain, delta.batteryCreated.size, delta.batteryCreated),
      signedSignal(KingRingPressureChanged, delta.kingRingPressureDelta, kingTargetSubjects(delta))
    ).flatten

  def consequences(delta: TransitionStructuralDelta): List[TransitionConsequence] =
    val developedPieces = developedPieceMoves(delta)
    val developmentMobilityGain = developedPieces.map(_.mobilityDelta.max(0)).sum
    val developmentMobilityLoss = developedPieces.map(move => (-move.mobilityDelta).max(0)).sum
    val developmentCenterGain = developedPieces.map(_.centerControlDelta.max(0)).sum
    val developmentCenterLoss = developedPieces.map(move => (-move.centerControlDelta).max(0)).sum
    val safeDevelopmentMoves =
      developedPieces.filter(move => move.defendedAfter && move.enemyAttackersAfter == 0)
    val retreatedDevelopmentMoves =
      developedPieces.filter(move => !move.fromBackRank && move.toBackRank)
    val unsafeDevelopmentMoves =
      developedPieces.filter(move => !move.defendedAfter && move.enemyAttackersAfter > 0)
    val baseConsequences =
      List(
        Option.when(delta.openedFiles.nonEmpty)(
          TransitionConsequence(OpenFileGain, Gain, delta.openedFiles.size, delta.openedFiles.map(file => s"open-file:$file"))
        ),
        Option.when(delta.semiOpenedFiles.nonEmpty)(
          TransitionConsequence(
            SemiOpenFileGain,
            Gain,
            delta.semiOpenedFiles.size,
            delta.semiOpenedFiles.map(file => s"semi-open-file:$file")
          )
        ),
        Option.when(delta.newWeakPawns.nonEmpty)(
          TransitionConsequence(WeakPawnTargetCreated, Gain, delta.newWeakPawns.size, delta.newWeakPawns.map(square => s"weak-pawn:$square"))
        ),
        Option.when(delta.newWeakSquares.nonEmpty)(
          TransitionConsequence(
            WeakSquareTargetCreated,
            Gain,
            delta.newWeakSquares.size,
            delta.newWeakSquares.map(square => s"weak-square:$square")
          )
        ),
        Option.when(delta.createdTension.nonEmpty || delta.pawnTensionDelta > 0)(
          TransitionConsequence(
            PawnTensionGain,
            Gain,
            delta.createdTension.size + delta.pawnTensionDelta.max(0),
            pawnTensionSubjects("created", delta.createdTension)
          )
        ),
        Option.when(delta.resolvedTension.nonEmpty || delta.pawnTensionDelta < 0)(
          TransitionConsequence(
            PawnTensionResolution,
            Neutral,
            delta.resolvedTension.size + (-delta.pawnTensionDelta).max(0),
            pawnTensionSubjects("resolved", delta.resolvedTension)
          )
        ),
        Option.when(delta.targetPressureGain > delta.targetPressureRelease || delta.targetPressureDelta > 0)(
          TransitionConsequence(
            TargetPressureGain,
            Gain,
            (delta.targetPressureGain - delta.targetPressureRelease).max(0) + delta.targetPressureDelta.max(0),
            delta.createdTargetPressure
          )
        ),
        Option.when(delta.targetPressureRelease > delta.targetPressureGain || delta.targetPressureDelta < 0)(
          TransitionConsequence(
            TargetPressureRelease,
            Loss,
            (delta.targetPressureRelease - delta.targetPressureGain).max(0) + (-delta.targetPressureDelta).max(0),
            delta.releasedTargetPressure
          )
        ),
        Option.when(delta.centerControlDelta > 0)(
          TransitionConsequence(CenterControlGain, Gain, delta.centerControlDelta, centerControlSubjects(delta))
        ),
        Option.when(delta.centerControlDelta < 0)(
          TransitionConsequence(CenterControlLoss, Loss, -delta.centerControlDelta, centerControlSubjects(delta))
        ),
        Option.when(delta.developmentDelta > 0)(
          TransitionConsequence(DevelopmentLagReduced, Gain, delta.developmentDelta, Nil)
        ),
        Option.when(developedPieces.nonEmpty)(
          TransitionConsequence(DevelopmentPieceActivated, Gain, developedPieces.size, developedPieces.map(developmentMoveLabel))
        ),
        Option.when(developmentMobilityGain > 0)(
          TransitionConsequence(DevelopmentMobilityGain, Gain, developmentMobilityGain, developmentMoveSubjectsWithMobilityGain(developedPieces))
        ),
        Option.when(developmentCenterGain > 0)(
          TransitionConsequence(
            DevelopmentCenterControlGain,
            Gain,
            developmentCenterGain,
            developmentMoveSubjectsWithCenterGain(developedPieces)
          )
        ),
        Option.when(safeDevelopmentMoves.nonEmpty)(
          TransitionConsequence(
            DevelopmentSafePlacement,
            Gain,
            safeDevelopmentMoves.size,
            safeDevelopmentMoves.map(developmentMoveLabel)
          )
        ),
        Option.when(delta.developmentDelta < 0)(
          TransitionConsequence(DevelopmentLagIncreased, Loss, -delta.developmentDelta, Nil)
        ),
        Option.when(retreatedDevelopmentMoves.nonEmpty)(
          TransitionConsequence(
            DevelopmentPieceRetreated,
            Loss,
            retreatedDevelopmentMoves.size,
            retreatedDevelopmentMoves.map(developmentMoveLabel)
          )
        ),
        Option.when(developmentMobilityLoss > 0)(
          TransitionConsequence(DevelopmentMobilityLoss, Loss, developmentMobilityLoss, developmentMoveSubjectsWithMobilityLoss(developedPieces))
        ),
        Option.when(developmentCenterLoss > 0)(
          TransitionConsequence(
            DevelopmentCenterControlLoss,
            Loss,
            developmentCenterLoss,
            developmentMoveSubjectsWithCenterLoss(developedPieces)
          )
        ),
        Option.when(unsafeDevelopmentMoves.nonEmpty)(
          TransitionConsequence(
            DevelopmentUnsafePlacement,
            Loss,
            unsafeDevelopmentMoves.size,
            unsafeDevelopmentMoves.map(developmentMoveLabel)
          )
        ),
        Option.when(delta.mobilityDelta > 0)(
          TransitionConsequence(MobilityGain, Gain, delta.mobilityDelta, developmentMoveSubjectsWithMobilityGain(delta.developmentMoves))
        ),
        Option.when(delta.lineUnlockDelta > 0)(
          TransitionConsequence(LineUnlockGain, Gain, delta.lineUnlockDelta, delta.lineUnlocks)
        ),
        Option.when(delta.mobilityDelta < 0)(
          TransitionConsequence(
            MobilityLoss,
            Loss,
            -delta.mobilityDelta,
            (developmentMoveSubjectsWithMobilityLoss(delta.developmentMoves) ++ delta.blockedOwnRays).distinct.sorted
          )
        ),
        Option.when(delta.fileOccupation.nonEmpty)(
          TransitionConsequence(
            FileOccupationGain,
            Gain,
            delta.fileOccupation.size,
            delta.fileOccupation
          )
        ),
        Option.when(delta.fileAccessDelta > 0)(
          TransitionConsequence(FileAccessGain, Gain, delta.fileAccessDelta, fileAccessSubjects(delta))
        ),
        Option.when(delta.fileAccessDelta < 0)(
          TransitionConsequence(FileAccessLoss, Loss, -delta.fileAccessDelta, fileAccessSubjects(delta))
        ),
        Option.when(delta.kingShelterDelta > 0)(
          TransitionConsequence(KingSafetyPressure, Gain, delta.kingShelterDelta, kingShelterSubjects(delta))
        ),
        Option.when(delta.kingShelterDelta < 0)(
          TransitionConsequence(KingSafetyConcession, Loss, -delta.kingShelterDelta, kingShelterSubjects(delta))
        ),
        Option.when(delta.passedPawnCreated.nonEmpty || delta.passedPawnAdvanced.nonEmpty)(
          TransitionConsequence(
            PassedPawnProgress,
            Gain,
            delta.passedPawnCreated.size + delta.passedPawnAdvanced.size,
            (delta.passedPawnCreated ++ delta.passedPawnAdvanced).distinct
          )
        ),
        Option.when(delta.passedPawnLost.nonEmpty)(
          TransitionConsequence(PassedPawnConcession, Loss, delta.passedPawnLost.size, delta.passedPawnLost)
        ),
        Option.when(delta.promotionPressureDelta > 0)(
          TransitionConsequence(PromotionPressureGain, Gain, delta.promotionPressureDelta, promotionPressureSubjects(delta))
        ),
        Option.when(delta.promotionPressureDelta < 0)(
          TransitionConsequence(PromotionPressureConcession, Loss, -delta.promotionPressureDelta, promotionPressureSubjects(delta))
        ),
        Option.when(delta.outpostCreated.nonEmpty)(
          TransitionConsequence(OutpostGain, Gain, delta.outpostCreated.size, delta.outpostCreated)
        ),
        Option.when(delta.outpostRemoved.nonEmpty)(
          TransitionConsequence(OutpostConcession, Loss, delta.outpostRemoved.size, delta.outpostRemoved)
        ),
        Option.when(delta.rookLiftCreated.nonEmpty)(
          TransitionConsequence(RookLiftActivation, Gain, delta.rookLiftCreated.size, delta.rookLiftCreated)
        ),
        Option.when(delta.batteryCreated.nonEmpty)(
          TransitionConsequence(BatteryPressureGain, Gain, delta.batteryCreated.size, delta.batteryCreated)
        ),
        Option.when(delta.opponentMobilityRestrictions.nonEmpty)(
          TransitionConsequence(
            OpponentMobilityRestriction,
            Gain,
            delta.opponentMobilityRestrictions.size,
            delta.opponentMobilityRestrictions
          )
        ),
        Option.when(delta.kingRingPressureDelta > 0)(
          TransitionConsequence(KingRingPressureGain, Gain, delta.kingRingPressureDelta, kingTargetSubjects(delta))
        ),
        Option.when(delta.kingRingPressureDelta < 0)(
          TransitionConsequence(KingRingPressureConcession, Loss, -delta.kingRingPressureDelta, kingTargetSubjects(delta))
        )
      ).flatten.distinctBy(consequence => (consequence.kind, consequence.polarity, consequence.strength, consequence.subjects.sorted.mkString(",")))
    baseConsequences

  def developmentChoices(delta: TransitionStructuralDelta): List[StructuralDevelopmentChoice] =
    developedPieceMoves(delta)
      .map(move => StructuralDevelopmentChoice(move.role, move.from, move.to))

  private def developedPieceMoves(delta: TransitionStructuralDelta): List[DevelopmentMoveDelta] =
    delta.developmentMoves.filter(move => move.fromBackRank && !move.toBackRank)

  private def developmentMoveLabel(move: DevelopmentMoveDelta): String =
    s"${move.role}:${move.from}-${move.to}"

  private def developmentMoveSubjectsWithMobilityGain(moves: List[DevelopmentMoveDelta]): List[String] =
    moves.filter(_.mobilityDelta > 0).map(move => s"${developmentMoveLabel(move)}:mobility+${move.mobilityDelta}")

  private def developmentMoveSubjectsWithMobilityLoss(moves: List[DevelopmentMoveDelta]): List[String] =
    moves.filter(_.mobilityDelta < 0).map(move => s"${developmentMoveLabel(move)}:mobility${move.mobilityDelta}")

  private def developmentMoveSubjectsWithCenterGain(moves: List[DevelopmentMoveDelta]): List[String] =
    moves.filter(_.centerControlDelta > 0).map(move => s"${developmentMoveLabel(move)}:center+${move.centerControlDelta}")

  private def developmentMoveSubjectsWithCenterLoss(moves: List[DevelopmentMoveDelta]): List[String] =
    moves.filter(_.centerControlDelta < 0).map(move => s"${developmentMoveLabel(move)}:center${move.centerControlDelta}")

  private def centerControlSubjects(delta: TransitionStructuralDelta): List[String] =
    if delta.centerControlDelta > 0 then (developmentMoveSubjectsWithCenterGain(delta.developmentMoves) ++ delta.centerControlSquares).distinct.sorted
    else if delta.centerControlDelta < 0 then (developmentMoveSubjectsWithCenterLoss(delta.developmentMoves) ++ delta.centerControlSquares).distinct.sorted
    else Nil

  private def fileAccessSubjects(delta: TransitionStructuralDelta): List[String] =
    (
      delta.openedFiles.map(file => s"open-file:$file") ++
        delta.semiOpenedFiles.map(file => s"semi-open-file:$file") ++
        delta.fileOccupation ++
        delta.lineUnlocks
    ).distinct.sorted

  private def kingShelterSubjects(delta: TransitionStructuralDelta): List[String] =
    (delta.kingTargetSubjects ++ delta.kingShelterFiles.map(file => s"file:$file")).distinct.sorted

  private def kingTargetSubjects(delta: TransitionStructuralDelta): List[String] =
    delta.kingTargetSubjects.distinct.sorted

  private def promotionPressureSubjects(delta: TransitionStructuralDelta): List[String] =
    if delta.promotionPressureDelta > 0 then (delta.passedPawnCreated ++ delta.passedPawnAdvanced).distinct.sorted
    else if delta.promotionPressureDelta < 0 then delta.passedPawnLost.distinct.sorted
    else Nil

  private def pawnTensionSubjects(prefix: String, edges: List[String]): List[String] =
    edges.flatMap { edge =>
      val breakFile = edge.takeWhile(_ != '-').headOption.map(file => s"break-file:$file")
      List(s"$prefix-tension:$edge") ++ breakFile
    }.distinct

  private def signal(
      kind: StructuralSignalKind,
      polarity: StructuralSignalPolarity,
      magnitude: Int,
      subjects: List[String]
  ): Option[StructuralSignal] =
    Option.when(magnitude > 0)(StructuralSignal(kind, polarity, magnitude, subjects.distinct))

  private def signedSignal(kind: StructuralSignalKind, value: Int, subjects: List[String]): Option[StructuralSignal] =
    if value > 0 then Some(StructuralSignal(kind, Gain, value, subjects.distinct))
    else if value < 0 then Some(StructuralSignal(kind, Loss, -value, subjects.distinct))
    else None

private[chessjudgment] object StructuralDeltaAnalyzer:

  private final case class StructureSnapshot(
      features: Option[PositionFeatures],
      openFiles: Set[String],
      semiOpenFilesForSide: Set[String],
      weakPawnsForEnemy: Set[String],
      weakSquaresForEnemy: Set[String],
      pawnTensions: Set[String],
      targetPressure: Map[String, Int],
      fileAccess: Int
  )

  def delta(
      beforeFen: String,
      beforeBoard: Board,
      afterFen: String,
      afterBoard: Board,
      side: Color,
      files: List[Char],
      targets: List[String],
      createdTensionFrom: Option[String] = None,
      moveUci: Option[String] = None
  ): Option[TransitionStructuralDelta] =
    val normalizedFiles = files.distinct
    val normalizedTargets =
      if targets.nonEmpty then targets.distinct
      else dynamicTargets(beforeBoard, afterBoard, side)
    val before = structureSnapshot(beforeFen, beforeBoard, side, normalizedFiles, normalizedTargets)
    val after = structureSnapshot(afterFen, afterBoard, side, normalizedFiles, normalizedTargets)
    val beforeAttacks = normalizedTargets.filter(target => sidePawnAttacksTarget(beforeBoard, side, target))
    val afterAttacks = normalizedTargets.filter(target => sidePawnAttacksTarget(afterBoard, side, target))
    val newTargets = afterAttacks.diff(beforeAttacks).distinct
    val createdTension =
      createdTensionFrom.toList.flatMap { origin =>
        normalizedTargets
          .filter(target => pawnAttacks(origin, side).contains(target) && !beforeAttacks.contains(target))
          .map(target => s"$origin-$target")
      }.distinct
    val targetPressureDelta =
      normalizedTargets.map(target => after.targetPressure.getOrElse(target, 0) - before.targetPressure.getOrElse(target, 0)).sum
    val pieceTargetPressure = moveUci
      .map(uci => pieceTargetPressureDelta(beforeBoard, afterBoard, side, uci))
      .getOrElse(PieceTargetPressureDelta(Nil, Nil))
    val passedPawnDelta = passedPawnDeltaFor(beforeBoard, afterBoard, side, moveUci)
    val moveMotifs = moveUci.toList.flatMap(uci => transitionMoveMotifs(beforeFen, uci))
    val outpostDelta = outpostDeltaFor(beforeFen, afterFen, moveMotifs, side)
    val mobilityDeltaValue = mobilityDelta(before.features, after.features, side)
    val developmentDeltaValue = developmentDelta(before.features, after.features, side)
    val centerControlDeltaValue = centerControlDelta(before.features, after.features, side)
    val centerControlSquaresValue =
      if centerControlDeltaValue != 0 then centerControlChangedSquares(beforeBoard, afterBoard, side) else Nil
    val shelterFiles = moveUci.map(moveFiles).getOrElse(Nil)
    val targetKingSubjects = kingSubjects(afterBoard, !side)
    val lineUnlock: (Int, List[String]) =
      moveUci.map(uci => lineUnlockDeltaAndSubjects(beforeBoard, afterBoard, side, uci)).getOrElse(0 -> Nil)
    Some(
      TransitionStructuralDelta(
        openedFiles = after.openFiles.diff(before.openFiles).toList.sorted,
        semiOpenedFiles = after.semiOpenFilesForSide.diff(before.semiOpenFilesForSide).toList.sorted,
        newWeakPawns = (after.weakPawnsForEnemy.diff(before.weakPawnsForEnemy) ++ newTargets).toList.sorted.distinct,
        newWeakSquares = (after.weakSquaresForEnemy.diff(before.weakSquaresForEnemy) ++ newTargets).toList.sorted.distinct,
        createdTension = createdTension,
        resolvedTension = before.pawnTensions.diff(after.pawnTensions).toList.sorted,
        pawnTensionBefore = beforeAttacks.size,
        pawnTensionAfter = afterAttacks.size,
        targetPressureDelta = targetPressureDelta,
        fileAccessDelta = after.fileAccess - before.fileAccess,
        kingShelterDelta = kingShelterDelta(
          before.features,
          after.features,
          side,
          shelterFiles,
          afterBoard.kingPosOf(!side).map(_.file.value)
        ),
        kingShelterFiles = shelterFiles.map(_.toString),
        kingTargetSubjects = targetKingSubjects,
        mobilityDelta = mobilityDeltaValue,
        developmentDelta = developmentDeltaValue,
        centerControlDelta = centerControlDeltaValue,
        centerControlSquares = centerControlSquaresValue,
        lineUnlockDelta = lineUnlock._1,
        lineUnlocks = lineUnlock._2,
        developmentMoves = moveUci.flatMap(uci => developmentMoveDelta(beforeBoard, afterBoard, side, uci)).toList,
        fileOccupation = moveUci.map(uci => fileOccupationGain(beforeBoard, afterBoard, after, side, uci)).getOrElse(Nil),
        releasedTargetPressure = pieceTargetPressure.released,
        createdTargetPressure = pieceTargetPressure.created,
        passedPawnCreated = passedPawnDelta.created,
        passedPawnAdvanced = passedPawnDelta.advanced,
        passedPawnLost = passedPawnDelta.lost,
        promotionPressureDelta = passedPawnDelta.promotionPressureDelta,
        outpostCreated = outpostDelta.created,
        outpostRemoved = outpostDelta.removed,
        rookLiftCreated = rookLiftLabels(moveMotifs, side),
        batteryCreated = batteryLines(afterFen, side).diff(batteryLines(beforeFen, side)).toList.sorted,
        blockedOwnRays = diagonalRestrictions(beforeBoard, afterBoard, after.features, side, side, moveUci),
        opponentMobilityRestrictions = opponentDiagonalRestrictions(beforeBoard, afterBoard, after.features, side, moveUci),
        kingRingPressureDelta = kingRingPressureDelta(before.features, after.features, side)
      )
    )

  private def developmentMoveDelta(
      beforeBoard: Board,
      afterBoard: Board,
      side: Color,
      moveUci: String
  ): Option[DevelopmentMoveDelta] =
    val origin = squareAt(moveUci.take(2))
    val dest = squareAt(moveUci.drop(2).take(2))
    val movedPiece = origin.flatMap(beforeBoard.pieceAt)
    (origin, dest, movedPiece) match
      case (Some(from), Some(to), Some(piece))
          if piece.color == side && Set(Knight, Bishop).contains(piece.role) && from != to =>
        val beforeMobility = pieceMobility(beforeBoard, from, piece.role, side)
        val afterMobility = pieceMobility(afterBoard, to, piece.role, side)
        val beforeCenter = centerControlByPiece(beforeBoard, from, piece.role, side)
        val afterCenter = centerControlByPiece(afterBoard, to, piece.role, side)
        Some(
          DevelopmentMoveDelta(
            role = piece.role.name,
            from = from.key,
            to = to.key,
            fromBackRank = backRank(from, side),
            toBackRank = backRank(to, side),
            destinationCenterDistance = centerDistance(to),
            mobilityDelta = afterMobility - beforeMobility,
            centerControlDelta = afterCenter - beforeCenter,
            defendedAfter = afterBoard.attackers(to, side).nonEmpty,
            enemyAttackersAfter = afterBoard.attackers(to, !side).count
          )
        )
      case _ =>
        None

  private final case class PassedPawnTransitionDelta(
      created: List[String],
      advanced: List[String],
      lost: List[String],
      promotionPressureDelta: Int
  )

  private def passedPawnDeltaFor(
      beforeBoard: Board,
      afterBoard: Board,
      side: Color,
      moveUci: Option[String]
  ): PassedPawnTransitionDelta =
    val beforePassers = passedPawnSquares(beforeBoard, side)
    val afterPassers = passedPawnSquares(afterBoard, side)
    val beforeOpponentPassers = passedPawnSquares(beforeBoard, !side)
    val afterOpponentPassers = passedPawnSquares(afterBoard, !side)
    val beforeKeys = beforePassers.map(_.key).toSet
    val afterKeys = afterPassers.map(_.key).toSet
    val advanced = moveUci.flatMap(uci => passedPawnAdvance(beforeBoard, afterBoard, side, uci, beforeKeys, afterKeys))
    val advancedFrom = advanced.map(_.from).toSet
    val advancedTo = advanced.map(_.to).toSet
    val rawPromotionPressureDelta =
      promotionPressureBalance(afterPassers, afterOpponentPassers, side) -
        promotionPressureBalance(beforePassers, beforeOpponentPassers, side)
    val promotionPressureDelta =
      if advanced.exists(_.promoted) && rawPromotionPressureDelta < 0 then 0 else rawPromotionPressureDelta
    PassedPawnTransitionDelta(
      created = afterKeys.diff(beforeKeys).diff(advancedTo).toList.sorted.map(square => s"passed-pawn-created:$square"),
      advanced = advanced.toList.map(_.label),
      lost = beforeKeys.diff(afterKeys).diff(advancedFrom).toList.sorted.map(square => s"passed-pawn-lost:$square"),
      promotionPressureDelta = promotionPressureDelta
    )

  private def passedPawnSquares(board: Board, side: Color): List[Square] =
    val ownPawns = if side.white then board.pawns & board.white else board.pawns & board.black
    val enemyPawns = if side.white then board.pawns & board.black else board.pawns & board.white
    PositionAnalyzer.passedPawns(side, ownPawns, enemyPawns)

  private final case class PassedPawnAdvance(
      from: String,
      to: String,
      label: String,
      promoted: Boolean
  )

  private def passedPawnAdvance(
      beforeBoard: Board,
      afterBoard: Board,
      side: Color,
      moveUci: String,
      beforePassers: Set[String],
      afterPassers: Set[String]
  ): Option[PassedPawnAdvance] =
    val origin = squareAt(moveUci.take(2))
    val dest = squareAt(moveUci.drop(2).take(2))
    val movedPiece = origin.flatMap(beforeBoard.pieceAt)
    (origin, dest, movedPiece) match
      case (Some(from), Some(to), Some(piece))
          if piece.color == side && piece.role == Pawn &&
            beforePassers.contains(from.key) &&
            afterBoard.pieceAt(to).exists(afterPiece => afterPiece.color == side && afterPiece.role != Pawn) =>
        Some(PassedPawnAdvance(from.key, to.key, s"passed-pawn-promoted:${from.key}-${to.key}", promoted = true))
      case (Some(from), Some(to), Some(piece))
          if piece.color == side && piece.role == Pawn &&
            afterBoard.pieceAt(to).exists(afterPiece => afterPiece.color == side && afterPiece.role == Pawn) &&
            afterPassers.contains(to.key) &&
            relativeRank(to, side) > relativeRank(from, side) =>
        val prefix = if beforePassers.contains(from.key) then "passed-pawn-advanced" else "passed-pawn-breakthrough"
        Some(PassedPawnAdvance(from.key, to.key, s"$prefix:${from.key}-${to.key}:rank-${relativeRank(to, side)}", promoted = false))
      case _ =>
        None

  private def promotionPressureScore(passers: List[Square], side: Color): Int =
    passers.map(square => (relativeRank(square, side) - 5).max(0)).sum

  private def promotionPressureBalance(ownPassers: List[Square], opponentPassers: List[Square], side: Color): Int =
    promotionPressureScore(ownPassers, side) - promotionPressureScore(opponentPassers, !side)

  private final case class OutpostTransitionDelta(
      created: List[String],
      removed: List[String]
  )

  private def outpostDeltaFor(beforeFen: String, afterFen: String, motifs: List[Motif], side: Color): OutpostTransitionDelta =
    val beforeOutposts = outpostLabels(beforeFen, side)
    val afterOutposts = outpostLabels(afterFen, side)
    OutpostTransitionDelta(
      created =
        (
          motifs.collect {
            case Motif.Outpost(role, square, color, _, _) if color == side =>
              s"outpost:${role.name}:${square.key}"
          } ++ afterOutposts.diff(beforeOutposts)
        ).distinct.sorted,
      removed = beforeOutposts.diff(afterOutposts).toList.sorted
    )

  private def outpostLabels(fen: String, side: Color): Set[String] =
    Fen.read(_root_.chess.variant.Standard, Fen.Full(fen)).toList.flatMap { position =>
      MoveAnalyzer.detectStateMotifs(position, 0).collect {
        case Motif.Outpost(role, square, color, _, _) if color == side =>
          s"outpost:${role.name}:${square.key}"
      }
    }.toSet

  private def transitionMoveMotifs(beforeFen: String, moveUci: String): List[Motif] =
    Fen.read(_root_.chess.variant.Standard, Fen.Full(beforeFen)).toList.flatMap { position =>
      Uci(moveUci).collect { case move: Uci.Move => move }
        .flatMap(position.move(_).toOption)
        .map(move => MoveAnalyzer.detectMoveMotifs(move, position, 0))
        .getOrElse(Nil)
    }

  private def rookLiftLabels(motifs: List[Motif], side: Color): List[String] =
    motifs.collect {
      case Motif.RookLift(_, _, _, color, _, Some(moveUci)) if color == side =>
        val dest = squareAt(moveUci.drop(2).take(2))
        s"rook-lift:${moveUci.take(2)}-${moveUci.drop(2).take(2)}:rank-${dest.map(relativeRank(_, side)).getOrElse(0)}"
    }.distinct.sorted

  private def batteryLines(fen: String, side: Color): Set[String] =
    Fen.read(_root_.chess.variant.Standard, Fen.Full(fen)).toList.flatMap { position =>
      MoveAnalyzer.detectStateMotifs(position, 0).collect {
        case Motif.Battery(frontRole, backRole, axis, color, _, _, Some(front), Some(back)) if color == side =>
          val endpoints =
            List(front.key -> frontRole.name.toLowerCase, back.key -> backRole.name.toLowerCase).sortBy(_._1)
          s"battery:${axis.toString.toLowerCase}:${endpoints.map(_._1).mkString("-")}:${endpoints.map(_._2).mkString("-")}"
      }
    }.toSet

  private def opponentDiagonalRestrictions(
      beforeBoard: Board,
      afterBoard: Board,
      afterFeatures: Option[PositionFeatures],
      side: Color,
      moveUci: Option[String]
  ): List[String] =
    diagonalRestrictions(beforeBoard, afterBoard, afterFeatures, blockerSide = side, restrictedSide = !side, moveUci = moveUci)

  private def diagonalRestrictions(
      beforeBoard: Board,
      afterBoard: Board,
      afterFeatures: Option[PositionFeatures],
      blockerSide: Color,
      restrictedSide: Color,
      moveUci: Option[String]
  ): List[String] =
    moveUci.toList.flatMap { uci =>
      val origin = squareAt(uci.take(2))
      val dest = squareAt(uci.drop(2).take(2))
      val movedPiece = origin.flatMap(beforeBoard.pieceAt)
      (origin, dest, movedPiece) match
        case (Some(_), Some(to), Some(piece))
            if piece.color == blockerSide &&
              piece.role == Pawn &&
              afterBoard.pieceAt(to).exists(afterPiece => afterPiece.color == blockerSide && afterPiece.role == Pawn) =>
          val candidateBishops =
            if blockerSide == restrictedSide then bishopSquares(beforeBoard, restrictedSide)
            else fianchettoBishopSquares(restrictedSide).filter(square => beforeBoard.pieceAt(square).exists(piece => piece.color == restrictedSide && piece.role == Bishop))
          candidateBishops.flatMap { bishopSquare =>
            val bishopBefore = beforeBoard.pieceAt(bishopSquare).exists(piece => piece.color == restrictedSide && piece.role == Bishop)
            val bishopAfter = afterBoard.pieceAt(bishopSquare).exists(piece => piece.color == restrictedSide && piece.role == Bishop)
            val beforeMobility = if bishopBefore then slidingMobility(beforeBoard, bishopSquare, Bishop, restrictedSide) else 0
            val afterMobility = if bishopAfter then slidingMobility(afterBoard, bishopSquare, Bishop, restrictedSide) else 0
            val strategicOpponentRestriction =
              blockerSide != restrictedSide &&
                afterFeatures.exists(_.centralSpace.lockedCenter) &&
                centralDiagonalBlocker(to) &&
                fianchettoBishopSquares(restrictedSide).contains(bishopSquare)
            val restrictionScope =
              if blockerSide == restrictedSide then "self-block"
              else "locked-center"
            Option.when(
              bishopBefore &&
                bishopAfter &&
                (blockerSide == restrictedSide || strategicOpponentRestriction) &&
                diagonalAligned(bishopSquare, to) &&
                firstOccupiedSquareOnRay(afterBoard, bishopSquare, to).contains(to) &&
                beforeMobility > afterMobility
            )(
              s"bishop:${bishopSquare.key}:diagonal-denial:blocked-by:${to.key}:$restrictionScope:mobility-$beforeMobility-to-$afterMobility"
            )
          }
        case _ =>
          Nil
    }.distinct.sorted

  private def bishopSquares(board: Board, side: Color): List[Square] =
    board.byPiece(side, Bishop).squares.toList

  private def fianchettoBishopSquares(side: Color): List[Square] =
    if side.white then List(Square.G2, Square.B2) else List(Square.G7, Square.B7)

  private def centralDiagonalBlocker(square: Square): Boolean =
    Set("c", "d", "e", "f").contains(square.key.take(1)) &&
      Set(4, 5).contains(rankOf(square.key).getOrElse(0))

  private def diagonalAligned(left: Square, right: Square): Boolean =
    (left.file.value - right.file.value).abs == (left.rank.value - right.rank.value).abs

  private def firstOccupiedSquareOnRay(board: Board, from: Square, through: Square): Option[Square] =
    if !diagonalAligned(from, through) then None
    else
      val fileStep = Integer.compare(through.file.value, from.file.value)
      val rankStep = Integer.compare(through.rank.value, from.rank.value)
      def loop(file: Int, rank: Int): Option[Square] =
        if file < 0 || file > 7 || rank < 0 || rank > 7 then None
        else
          squareAt(s"${('a' + file).toChar}${rank + 1}") match
            case Some(square) if board.pieceAt(square).nonEmpty => Some(square)
            case Some(_)                                        => loop(file + fileStep, rank + rankStep)
            case None                                           => None
      loop(from.file.value + fileStep, from.rank.value + rankStep)

  private final case class PieceTargetPressureDelta(
      released: List[String],
      created: List[String]
  )

  private def pieceTargetPressureDelta(
      beforeBoard: Board,
      afterBoard: Board,
      side: Color,
      moveUci: String
  ): PieceTargetPressureDelta =
    val origin = squareAt(moveUci.take(2))
    val dest = squareAt(moveUci.drop(2).take(2))
    val movedPiece = origin.flatMap(beforeBoard.pieceAt)
    (origin, dest, movedPiece) match
      case (Some(from), Some(to), Some(piece)) if piece.color == side && piece.role == Pawn =>
        val beforeTargets = enemyOccupiedSquares(beforeBoard, side).filter(square =>
          pawnAttacks(from.key, side).contains(square.key)
        )
        val afterTargets = enemyOccupiedSquares(afterBoard, side).filter(square =>
          pawnAttacks(to.key, side).contains(square.key)
        )
        PieceTargetPressureDelta(
          released = beforeTargets.diff(afterTargets).map(_.key).distinct.sorted,
          created = afterTargets.diff(beforeTargets).map(_.key).distinct.sorted
        )
      case (Some(from), Some(to), Some(piece)) if piece.color == side && piece.role != Pawn =>
        val beforeTargets = enemyOccupiedSquares(beforeBoard, side).filter(square =>
          beforeBoard.attackers(square, side).squares.contains(from)
        )
        val afterTargets = enemyOccupiedSquares(afterBoard, side).filter(square =>
          afterBoard.attackers(square, side).squares.contains(to)
        )
        val released = beforeTargets.filter(square =>
          afterBoard.attackers(square, side).count < beforeBoard.attackers(square, side).count
        )
        val created = afterTargets.filter(square =>
          beforeBoard.attackers(square, side).count < afterBoard.attackers(square, side).count
        )
        PieceTargetPressureDelta(
          released = released.map(_.key).distinct.sorted,
          created = created.map(_.key).distinct.sorted
        )
      case _ =>
        PieceTargetPressureDelta(Nil, Nil)

  private def fileOccupationGain(
      beforeBoard: Board,
      afterBoard: Board,
      after: StructureSnapshot,
      side: Color,
      moveUci: String
  ): List[String] =
    val origin = squareAt(moveUci.take(2))
    val dest = squareAt(moveUci.drop(2).take(2))
    val movedPiece = origin.flatMap(beforeBoard.pieceAt)
    (origin, dest, movedPiece) match
      case (Some(from), Some(to), Some(piece))
          if piece.color == side && Set(Rook, Queen).contains(piece.role) && from != to =>
        val file = to.key.take(1)
        val fileAccessible = after.openFiles.contains(file) || after.semiOpenFilesForSide.contains(file)
        val movedPieceArrived = afterBoard.pieceAt(to).exists(afterPiece => afterPiece.color == side && afterPiece.role == piece.role)
        val squareWasOccupied = beforeBoard.pieceAt(to).exists(_.color == side)
        if fileAccessible && movedPieceArrived && !squareWasOccupied then List(s"$file:${to.key}") else Nil
      case _ =>
        Nil

  private def lineUnlockDeltaAndSubjects(
      beforeBoard: Board,
      afterBoard: Board,
      side: Color,
      moveUci: String
  ): (Int, List[String]) =
    val origin = squareAt(moveUci.take(2))
    val movedPiece = origin.flatMap(beforeBoard.pieceAt)
    movedPiece match
      case Some(piece) if piece.color == side && piece.role == Pawn =>
        val unlocked =
          ownSlidingPieces(beforeBoard, side).flatMap { case (square, role) =>
            val beforeMobility = slidingMobility(beforeBoard, square, role, side)
            val afterMobility =
              if afterBoard.pieceAt(square).exists(afterPiece => afterPiece.color == side && afterPiece.role == role)
              then slidingMobility(afterBoard, square, role, side)
              else 0
            val gain = (afterMobility - beforeMobility).max(0)
            Option.when(gain > 0)(
              gain -> s"${role.name}:${square.key}:line-unlock:by:${moveUci.toLowerCase}:mobility+$gain"
            )
          }
        unlocked.map(_._1).sum -> unlocked.map(_._2).distinct.sorted
      case _ =>
        0 -> Nil

  private def ownSlidingPieces(board: Board, side: Color): List[(Square, Role)] =
    Square.all.toList.flatMap { square =>
      board.pieceAt(square).collect {
        case piece if piece.color == side && Set(Bishop, Rook, Queen).contains(piece.role) =>
          square -> piece.role
      }
    }

  private def slidingMobility(board: Board, square: Square, role: Role, side: Color): Int =
    val targets =
      role match
        case Bishop => square.bishopAttacks(board.occupied)
        case Rook   => square.rookAttacks(board.occupied)
        case Queen  => square.queenAttacks(board.occupied)
        case _      => _root_.chess.Bitboard.empty
    (targets & ~board.byColor(side)).count

  private def pieceMobility(board: Board, square: Square, role: Role, side: Color): Int =
    val targets =
      role match
        case Knight => square.knightAttacks
        case Bishop => square.bishopAttacks(board.occupied)
        case Rook   => square.rookAttacks(board.occupied)
        case Queen  => square.queenAttacks(board.occupied)
        case King   => square.kingAttacks
        case Pawn   => square.pawnAttacks(side)
    (targets & ~board.byColor(side)).count

  private def centerControlByPiece(board: Board, square: Square, role: Role, side: Color): Int =
    val targets =
      role match
        case Knight => square.knightAttacks
        case Bishop => square.bishopAttacks(board.occupied)
        case Rook   => square.rookAttacks(board.occupied)
        case Queen  => square.queenAttacks(board.occupied)
        case King   => square.kingAttacks
        case Pawn   => square.pawnAttacks(side)
    (targets & centerSquares).count

  private def centerControlChangedSquares(beforeBoard: Board, afterBoard: Board, side: Color): List[String] =
    val before = controlledCenterSquares(beforeBoard, side)
    val after = controlledCenterSquares(afterBoard, side)
    (before.diff(after) ++ after.diff(before)).toList.map(_.key).sorted

  private def controlledCenterSquares(board: Board, side: Color): Set[Square] =
    centerSquareList.filter(square => board.attackers(square, side).nonEmpty).toSet

  private val centerSquares =
    Square.D4.bb | Square.E4.bb | Square.D5.bb | Square.E5.bb

  private val centerSquareList =
    List(Square.D4, Square.E4, Square.D5, Square.E5)

  private def backRank(square: Square, side: Color): Boolean =
    if side.white then square.rank.value == 0 else square.rank.value == 7

  private def moveFiles(moveUci: String): List[Char] =
    List(moveUci.take(2), moveUci.drop(2).take(2))
      .flatMap(square => square.headOption.filter(file => file >= 'a' && file <= 'h'))
      .distinct

  private def centerDistance(square: Square): Int =
    List(Square.D4, Square.E4, Square.D5, Square.E5)
      .map(center => (square.file.value - center.file.value).abs + (square.rank.value - center.rank.value).abs)
      .minOption
      .getOrElse(0)

  private def relativeRank(square: Square, side: Color): Int =
    if side.white then square.rank.value + 1 else 8 - square.rank.value

  private def enemyOccupiedSquares(board: Board, side: Color): List[Square] =
    Square.all.toList.filter(square => board.pieceAt(square).exists(_.color != side))

  private def kingSubjects(board: Board, side: Color): List[String] =
    board.kingPosOf(side).map(square => s"king:${square.key}").toList

  private def structureSnapshot(
      fen: String,
      board: Board,
      side: Color,
      files: List[Char],
      targets: List[String]
  ): StructureSnapshot =
    val features = if fen.trim.nonEmpty then PositionAnalyzer.extractFeatures(fen, 1) else None
    val openFiles = files.filter(file => pawnsOnFile(board, file).isEmpty).map(_.toString).toSet
    val semiOpenFiles =
      files
        .filter(file =>
          val pawns = pawnsOnFile(board, file)
          pawns.nonEmpty && pawns.forall(square => board.pieceAt(square).exists(_.color != side))
        )
        .map(_.toString)
        .toSet
    val weakPawns =
      WeaknessTargetProfile
        .targetsForPressure(board, side)
        .map(_.targetSquare)
        .filter(square => targets.contains(square) || square.headOption.exists(files.contains))
        .toSet
    val weakSquares =
      weakPawns.flatMap(square => adjacentSquares(square) :+ square).filter(square =>
        square.headOption.exists(files.contains)
      )
    StructureSnapshot(
      features = features,
      openFiles = openFiles,
      semiOpenFilesForSide = semiOpenFiles,
      weakPawnsForEnemy = weakPawns,
      weakSquaresForEnemy = weakSquares,
      pawnTensions = pawnTensionEdges(board, side, files),
      targetPressure = targets.map(target => target -> targetPressure(board, side, target)).toMap,
      fileAccess = openFiles.size * 2 + semiOpenFiles.size
    )

  private def dynamicTargets(beforeBoard: Board, afterBoard: Board, side: Color): List[String] =
    (
      WeaknessTargetProfile.targetsForPressure(beforeBoard, side) ++
        WeaknessTargetProfile.targetsForPressure(afterBoard, side)
    ).map(_.targetSquare).distinct ++
      (pawnAttackedEnemyTargets(beforeBoard, side) ++ pawnAttackedEnemyTargets(afterBoard, side)).distinct

  private def pawnAttackedEnemyTargets(board: Board, side: Color): List[String] =
    val enemyPawns = board.byPiece(!side, Pawn).squares.map(_.key).toSet
    board
      .byPiece(side, Pawn)
      .squares
      .flatMap(square => pawnAttacks(square.key, side).filter(enemyPawns.contains))
      .toList
      .distinct

  private def pawnTensionEdges(board: Board, side: Color, files: List[Char]): Set[String] =
    val sidePawns = board.byPiece(side, Pawn).squares
    val enemyPawns = board.byPiece(!side, Pawn).squares.map(_.key).toSet
    sidePawns
      .filter(square => square.key.headOption.exists(files.contains))
      .flatMap(square =>
        pawnAttacks(square.key, side)
          .filter(enemyPawns.contains)
          .map(target => s"${square.key}-$target")
      )
      .toSet

  private def targetPressure(board: Board, side: Color, target: String): Int =
    squareAt(target).map(square => board.attackers(square, side).count).getOrElse(0)

  private def kingShelterDelta(
      before: Option[PositionFeatures],
      after: Option[PositionFeatures],
      side: Color,
      files: List[Char],
      targetKingFile: Option[Int]
  ): Int =
    val nearTargetKing =
      targetKingFile.exists(kingFile =>
        files.exists(file => (file.toLower - 'a' - kingFile).abs <= 2)
      )
    if !nearTargetKing then 0
    else
      before.zip(after).map { case (beforeFeatures, afterFeatures) =>
        if side.white then beforeFeatures.kingSafety.blackKingShield - afterFeatures.kingSafety.blackKingShield
        else beforeFeatures.kingSafety.whiteKingShield - afterFeatures.kingSafety.whiteKingShield
      }.getOrElse(0)

  private def kingRingPressureDelta(
      before: Option[PositionFeatures],
      after: Option[PositionFeatures],
      side: Color
  ): Int =
    before.zip(after).map { case (beforeFeatures, afterFeatures) =>
      if side.white then
        (afterFeatures.kingSafety.blackKingRingAttacked - beforeFeatures.kingSafety.blackKingRingAttacked) +
          (afterFeatures.kingSafety.blackAttackersCount - beforeFeatures.kingSafety.blackAttackersCount)
      else
        (afterFeatures.kingSafety.whiteKingRingAttacked - beforeFeatures.kingSafety.whiteKingRingAttacked) +
          (afterFeatures.kingSafety.whiteAttackersCount - beforeFeatures.kingSafety.whiteAttackersCount)
    }.getOrElse(0)

  private def mobilityDelta(
      before: Option[PositionFeatures],
      after: Option[PositionFeatures],
      side: Color
  ): Int =
    before.zip(after).map { case (beforeFeatures, afterFeatures) =>
      if side.white then afterFeatures.activity.whitePseudoMobility - beforeFeatures.activity.whitePseudoMobility
      else afterFeatures.activity.blackPseudoMobility - beforeFeatures.activity.blackPseudoMobility
    }.getOrElse(0)

  private def developmentDelta(
      before: Option[PositionFeatures],
      after: Option[PositionFeatures],
      side: Color
  ): Int =
    before.zip(after).map { case (beforeFeatures, afterFeatures) =>
      if side.white then beforeFeatures.activity.whiteDevelopmentLag - afterFeatures.activity.whiteDevelopmentLag
      else beforeFeatures.activity.blackDevelopmentLag - afterFeatures.activity.blackDevelopmentLag
    }.getOrElse(0)

  private def centerControlDelta(
      before: Option[PositionFeatures],
      after: Option[PositionFeatures],
      side: Color
  ): Int =
    before.zip(after).map { case (beforeFeatures, afterFeatures) =>
      if side.white then afterFeatures.centralSpace.whiteCenterControl - beforeFeatures.centralSpace.whiteCenterControl
      else afterFeatures.centralSpace.blackCenterControl - beforeFeatures.centralSpace.blackCenterControl
    }.getOrElse(0)

  private def pawnsOnFile(board: Board, file: Char): List[Square] =
    Square.all.toList.filter(square =>
      square.key.headOption.contains(file) &&
        board.pieceAt(square).exists(_.role == Pawn)
    )

  private def sidePawnAttacksTarget(
      board: Board,
      side: Color,
      target: String
  ): Boolean =
    Square.all.exists { square =>
      board.pieceAt(square).exists(piece => piece.color == side && piece.role == Pawn) &&
        pawnAttacks(square.key, side).contains(target)
    }

  private def pawnAttacks(square: String, side: Color): List[String] =
    for
      file <- fileOf(square).toList
      rank <- rankOf(square).toList
      targetRank = rank + forwardDirection(side)
      targetFile <- List((file - 1).toChar, (file + 1).toChar)
      if targetFile >= 'a' && targetFile <= 'h' && targetRank >= 1 && targetRank <= 8
    yield s"$targetFile$targetRank"

  private def adjacentSquares(square: String): List[String] =
    for
      file <- fileOf(square).toList
      rank <- rankOf(square).toList
      df <- List(-1, 0, 1)
      dr <- List(-1, 0, 1)
      if df != 0 || dr != 0
      nextFile = (file + df).toChar
      nextRank = rank + dr
      if nextFile >= 'a' && nextFile <= 'h' && nextRank >= 1 && nextRank <= 8
    yield s"$nextFile$nextRank"

  private def squareAt(key: String): Option[Square] =
    Square.all.find(_.key == key)

  private def fileOf(square: String): Option[Char] =
    square.headOption.filter(file => file >= 'a' && file <= 'h')

  private def rankOf(square: String): Option[Int] =
    square.lift(1).flatMap(char => Option.when(char >= '1' && char <= '8')(char.asDigit))

  private def forwardDirection(side: Color): Int =
    if side.white then 1 else -1
