package lila.chessjudgment.analysis.structure

import chess.*
import lila.chessjudgment.analysis.position.PositionFeatures
import lila.chessjudgment.model.structure.{ CenterState, StructureId, StructureProfile }

/**
 * Assesses pawn structure from board features and rule evidence.
 */
object PawnStructureAssessor:

  private case class Rule(flag: Boolean, code: String)
  private case class Candidate(
      id: StructureId,
      score: Double,
      requiredOk: Boolean,
      blockerHit: Boolean,
      evidence: List[String]
  )

  private val centerOnlyIds = Set(
    StructureId.OpenCenter,
    StructureId.LockedCenter,
    StructureId.FluidCenter,
    StructureId.SymmetricCenter
  )

  def assess(
      features: PositionFeatures,
      board: Board,
      minConfidence: Double = 0.72,
      minMargin: Double = 0.10
  ): StructureProfile =
    val centerState = centerStateOf(features)
    val rankedAll = detect(features, board).filter(_.score > 0.0).sortBy(c => -c.score)
    val rankedSpecific = rankedAll.filterNot(c => centerOnlyIds.contains(c.id))
    val ranked = if rankedSpecific.nonEmpty then rankedSpecific else rankedAll

    if ranked.isEmpty then
      return StructureProfile(
        primary = StructureId.Unknown,
        confidence = 0.0,
        alternatives = Nil,
        centerState = centerState,
        evidenceCodes = List("NO_SIGNAL", "LOW_CONF", "LOW_MARGIN")
      )

    val topRaw = ranked.head
    val top = resolveSymmetricOwner(topRaw, ranked.drop(1), features)
    val others = ranked.filterNot(_.id == top.id)
    val second = others.headOption

    val scoreWindow = (top :: others).take(3)
    val sumTop = scoreWindow.map(_.score).sum.max(1e-6)
    val topConfidence = top.score / sumTop
    val secondConfidence = second.map(_.score / sumTop).getOrElse(0.0)
    val margin = topConfidence - secondConfidence
    val alternatives = rankedAll.filterNot(_.id == top.id).take(2).map(_.id)

    val symmetricTie =
      second.exists(s => isSymmetricPair(top.id, s.id)) &&
        second.exists(s => math.abs(top.score - s.score) < 0.25)
    val strongTopSignal = top.score >= 5.2 && top.requiredOk && !top.blockerHit
    val strongGap = second.forall(s => (top.score - s.score) >= 0.8)

    val effectiveConfidence =
      if symmetricTie || strongTopSignal then math.max(topConfidence, minConfidence)
      else topConfidence
    val effectiveMargin =
      if symmetricTie || (strongTopSignal && strongGap) then math.max(margin, minMargin)
      else margin

    val lowConf = effectiveConfidence < minConfidence
    val lowMargin = effectiveMargin < minMargin
    val requiredMiss = !top.requiredOk
    val blockerConflict = top.blockerHit
    val mustUnknown = lowConf || lowMargin || requiredMiss || blockerConflict

    if mustUnknown then
      val reasons =
        List(
          Option.when(lowConf)("LOW_CONF"),
          Option.when(lowMargin)("LOW_MARGIN"),
          Option.when(requiredMiss)("REQ_MISS"),
          Option.when(blockerConflict)("BLK_CONFLICT")
        ).flatten
      StructureProfile(
        primary = StructureId.Unknown,
        confidence = effectiveConfidence,
        alternatives = (top.id :: alternatives).distinct.take(2),
        centerState = centerState,
        evidenceCodes = (top.evidence ++ reasons).distinct
      )
    else
      StructureProfile(
        primary = top.id,
        confidence = effectiveConfidence,
        alternatives = alternatives,
        centerState = centerState,
        evidenceCodes = top.evidence
      )

  private def centerStateOf(features: PositionFeatures): CenterState =
    val c = features.centralSpace
    if c.openCenter then CenterState.Open
    else if c.lockedCenter then CenterState.Locked
    else if c.whiteCentralPawns == c.blackCentralPawns && math.abs(c.spaceDiff) <= 1 then CenterState.Symmetric
    else CenterState.Fluid

  private def detect(features: PositionFeatures, board: Board): List[Candidate] =
    val p = features.pawns
    val c = features.centralSpace

    val whitePawns = board.byPiece(Color.White, Pawn).squares
    val blackPawns = board.byPiece(Color.Black, Pawn).squares
    val whiteByFile = whitePawns.groupBy(_.file).view.mapValues(_.size).toMap
    val blackByFile = blackPawns.groupBy(_.file).view.mapValues(_.size).toMap

    def hasPawn(color: Color, sq: Square): Boolean = board.pieceAt(sq).contains(Piece(color, Pawn))
    def fileCount(color: Color, file: File): Int =
      if color == Color.White then whiteByFile.getOrElse(file, 0) else blackByFile.getOrElse(file, 0)
    def fileHas(color: Color, file: File): Boolean = fileCount(color, file) > 0

    val hasWc4 = hasPawn(Color.White, Square.C4)
    val hasWd4 = hasPawn(Color.White, Square.D4)
    val hasWe4 = hasPawn(Color.White, Square.E4)
    val hasWe3 = hasPawn(Color.White, Square.E3)
    val hasWe5 = hasPawn(Color.White, Square.E5)
    val hasBc5 = hasPawn(Color.Black, Square.C5)
    val hasBc6 = hasPawn(Color.Black, Square.C6)
    val hasBd6 = hasPawn(Color.Black, Square.D6)
    val hasBd5 = hasPawn(Color.Black, Square.D5)
    val hasBe5 = hasPawn(Color.Black, Square.E5)
    val hasBe6 = hasPawn(Color.Black, Square.E6)
    val hasBe7 = hasPawn(Color.Black, Square.E7)
    val cPawnAsymmetry = fileHas(Color.White, File.C) != fileHas(Color.Black, File.C)

    val fianchettoSides = List(Color.White, Color.Black).flatMap { color =>
      val pawnSq = if color == Color.White then Square.G3 else Square.G6
      val bishopSq = if color == Color.White then Square.G2 else Square.G7
      Option.when(
        board.pieceAt(pawnSq).contains(Piece(color, Pawn)) &&
          board.pieceAt(bishopSq).contains(Piece(color, Bishop))
      )(color)
    }

    val whiteStonewall =
      hasPawn(Color.White, Square.F4) &&
        hasWe3 &&
        hasWd4 &&
        fileHas(Color.White, File.C)
    val blackStonewall =
      hasPawn(Color.Black, Square.F5) &&
        hasBe6 &&
        hasBd5 &&
        fileHas(Color.Black, File.C)

    val whiteHangingPair = hasWc4 && hasWd4 && !fileHas(Color.White, File.E)
    val blackHangingPair = hasBc5 && hasBd5 && !fileHas(Color.Black, File.E)

    List(
      buildCandidate(
        id = StructureId.Carlsbad,
        required = List(
          Rule(hasWd4 && hasBd5, "D_PAWN_TENSION"),
          Rule(cPawnAsymmetry, "C_FILE_ASYMMETRY")
        ),
        support = List(
          Rule(hasBe6 || hasBe7, "BLACK_E_SUPPORT"),
          Rule(!p.whiteIQP && !p.blackIQP, "NOT_IQP")
        ),
        blockers = List(
          Rule(hasWe5, "FRENCH_ADVANCE_SHAPE"),
          Rule(hasBc5, "SICILIAN_OR_BENONI_C5")
        ),
        baseScore = 4.3
      ),
      buildCandidate(
        id = StructureId.IQPWhite,
        required = List(
          Rule(p.whiteIQP || (fileCount(Color.White, File.D) == 1 && !fileHas(Color.White, File.C) && !fileHas(Color.White, File.E)), "WHITE_ISOLATED_D")
        ),
        support = List(
          Rule(fileCount(Color.White, File.D) == 1, "SINGLE_D_PAWN"),
          Rule(!whiteHangingPair, "NO_C_D_PAIR")
        ),
        blockers = List(
          Rule(whiteHangingPair, "HANGING_PAIR"),
          Rule(hasWc4 && hasWd4, "CONNECTED_CENTER"),
          Rule(hasBd5 && (hasBe6 || hasBe7) && !fileHas(Color.White, File.C) && !fileHas(Color.Black, File.C), "QGD_EXCHANGE_COMPETING")
        ),
        baseScore = 4.0
      ),
      buildCandidate(
        id = StructureId.IQPBlack,
        required = List(
          Rule(p.blackIQP || (fileCount(Color.Black, File.D) == 1 && !fileHas(Color.Black, File.C) && !fileHas(Color.Black, File.E)), "BLACK_ISOLATED_D")
        ),
        support = List(
          Rule(fileCount(Color.Black, File.D) == 1, "SINGLE_D_PAWN"),
          Rule(!blackHangingPair, "NO_C_D_PAIR")
        ),
        blockers = List(
          Rule(blackHangingPair, "HANGING_PAIR"),
          Rule(hasBc5 && hasBd5, "CONNECTED_CENTER"),
          Rule(hasWd4 && (hasPawn(Color.White, Square.E3) || hasWe4) && !fileHas(Color.White, File.C) && !fileHas(Color.Black, File.C), "QGD_EXCHANGE_COMPETING")
        ),
        baseScore = 4.0
      ),
      buildCandidate(
        id = StructureId.HangingPawnsWhite,
        required = List(
          Rule(p.whiteHangingPawns || whiteHangingPair, "WHITE_C_D_PAIR")
        ),
        support = List(
          Rule(hasWc4 && hasWd4, "ADVANCED_PAIR"),
          Rule(!fileHas(Color.White, File.E), "NO_E_SUPPORT")
        ),
        blockers = List(
          Rule(hasBd5 && hasBe6 && !hasBc5, "CARLSBAD_COMPETING"),
          Rule(p.whiteIQP, "IQP_COMPETING")
        ),
        baseScore = 3.9
      ),
      buildCandidate(
        id = StructureId.HangingPawnsBlack,
        required = List(
          Rule(p.blackHangingPawns || blackHangingPair, "BLACK_C_D_PAIR")
        ),
        support = List(
          Rule(hasBc5 && hasBd5, "ADVANCED_PAIR"),
          Rule(!fileHas(Color.Black, File.E), "NO_E_SUPPORT")
        ),
        blockers = List(
          Rule(hasWd4 && (hasPawn(Color.White, Square.E3) || hasWe4) && !hasWc4, "CARLSBAD_COMPETING"),
          Rule(p.blackIQP, "IQP_COMPETING")
        ),
        baseScore = 3.9
      ),
      buildCandidate(
        id = StructureId.FrenchAdvanceChain,
        required = List(
          Rule(hasBe6 && hasBd5 && hasWe5, "E6_D5_VS_E5")
        ),
        support = List(
          Rule(c.lockedCenter || hasWd4, "LOCKED_OR_FIXED_CENTER"),
          Rule(!hasBc5, "NO_SICILIAN_C5")
        ),
        blockers = List(
          Rule(!hasWe5, "NO_ADVANCE_PAWN")
        ),
        baseScore = 4.1
      ),
      buildCandidate(
        id = StructureId.NajdorfScheveningenCenter,
        required = List(
          Rule(hasBd6 && hasBe6 && hasWe4, "SCHEVENINGEN_CORE")
        ),
        support = List(
          Rule(hasBc5 || hasBc6 || fileHas(Color.Black, File.C), "C_FILE_PRESSURE"),
          Rule(!hasPawn(Color.White, Square.D5), "NO_WHITE_D5"),
          Rule(!c.lockedCenter, "NOT_LOCKED")
        ),
        blockers = List(
          Rule(hasWc4 && hasWe4 && hasBd6 && !hasBe6, "MAROCZY_COMPETING"),
          Rule(hasPawn(Color.White, Square.D5), "BENONI_COMPETING"),
          Rule(hasBd6 && hasBe5 && hasWd4 && hasWe4, "KID_COMPETING")
        ),
        baseScore = 4.0
      ),
      buildCandidate(
        id = StructureId.BenoniCenter,
        required = List(
          Rule(hasBc5 && hasBd6 && hasPawn(Color.White, Square.D5), "BENONI_CORE")
        ),
        support = List(
          Rule(hasWc4, "WHITE_SPACE_ON_C4"),
          Rule(!hasBe6, "NO_E6_CHAIN")
        ),
        blockers = List(
          Rule(hasBe6 && hasWe4, "SCHEVENINGEN_COMPETING")
        ),
        baseScore = 3.9
      ),
      buildCandidate(
        id = StructureId.KIDLockedCenter,
        required = List(
          Rule(
            hasBd6 &&
              hasBe5 &&
              hasPawn(Color.Black, Square.G6) &&
              hasWd4 && hasWe4,
            "KID_PAWN_SHELL"
          )
        ),
        support = List(
          Rule(c.lockedCenter || hasBc5, "CENTER_LOCKED_OR_C5_PRESENT"),
          Rule(fileHas(Color.Black, File.F), "F_FILE_ANCHOR")
        ),
        blockers = List(
          Rule(hasBe6 && hasBd5 && (hasBc5 || hasBc6), "TRIANGLE_COMPETING")
        ),
        baseScore = 4.1
      ),
      buildCandidate(
        id = StructureId.SlavCaroTriangle,
        required = List(
          Rule(
            (hasBc6 || hasBc5) &&
              hasBd5 &&
              hasBe6,
            "C6_OR_C5_D5_E6_TRIANGLE"
          )
        ),
        support = List(
          Rule(hasWd4 || hasWe3, "WHITE_CENTER_CONTACT"),
          Rule(!hasWe5, "NO_WHITE_E5")
        ),
        blockers = List(
          Rule(hasWe5, "FRENCH_ADVANCE_COMPETING"),
          Rule(hasBd6 && hasBe5 && hasWd4 && hasWe4, "KID_COMPETING")
        ),
        baseScore = 4.0
      ),
      buildCandidate(
        id = StructureId.MaroczyBind,
        required = List(
          Rule(hasWc4 && hasWe4 && hasBd6 && !fileHas(Color.White, File.D), "MAROCZY_CLAMP")
        ),
        support = List(
          Rule(!hasPawn(Color.White, Square.D5), "NO_WHITE_D5"),
          Rule(!hasBe6 && !hasBe5, "NO_BLACK_E_CHAIN")
        ),
        blockers = List(
          Rule(hasBe6 && fileHas(Color.Black, File.C), "SCHEVENINGEN_COMPETING"),
          Rule(hasPawn(Color.White, Square.D5), "BENONI_COMPETING")
        ),
        baseScore = 4.2
      ),
      buildCandidate(
        id = StructureId.Hedgehog,
        required = List(
          Rule(
            (
              hasPawn(Color.Black, Square.A6) &&
                hasPawn(Color.Black, Square.B6) &&
                hasBd6 &&
                (hasBe6 || hasPawn(Color.Black, Square.G6))
            ) || (
              hasBc6 &&
                hasBd6 &&
                hasWc4 &&
                (hasPawn(Color.White, Square.B3) || hasWe3)
            ),
            "HEDGEHOG_NET"
          )
        ),
        support = List(
          Rule(hasBc6 || hasPawn(Color.Black, Square.A6), "QUEENSIDE_ANCHOR"),
          Rule(!hasPawn(Color.White, Square.D5), "NO_WHITE_D5_WEDGE")
        ),
        blockers = List(
          Rule(hasWc4 && hasWe4 && !fileHas(Color.White, File.D), "MAROCZY_SPACE_BIND"),
          Rule(hasBd5 && hasBe6 && (hasBc5 || hasBc6), "TRIANGLE_COMPETING")
        ),
        baseScore = 4.2
      ),
      buildCandidate(
        id = StructureId.FianchettoShell,
        required = List(
          Rule(fianchettoSides.nonEmpty, "G_PAWN_AND_BISHOP_SHELL")
        ),
        support = List(
          Rule(fianchettoSides.size == 2, "DOUBLE_FIANCHETTO")
        ),
        blockers = List(
          Rule(hasWc4 && hasWd4, "CENTER_PAIR_PRESENT"),
          Rule(hasBc5 && hasBd5, "CENTER_COUNTERPAIR_PRESENT"),
          Rule(hasPawn(Color.Black, Square.A6) && hasPawn(Color.Black, Square.B6) && hasBd6 && hasBe6, "HEDGEHOG_COMPETING"),
          Rule(hasWc4 && hasWe4 && hasBd6 && !fileHas(Color.White, File.D), "MAROCZY_COMPETING"),
          Rule(hasWd4 && hasWe4 && hasBd6 && hasBe5, "KID_STRUCTURE_COMPETING"),
          Rule(hasWd4 && hasWe4 && hasBd6 && !hasBe5, "FLUID_STRUCTURE_COMPETING"),
          Rule(hasWd4 && hasBd5 && (hasWe3 || hasWe4) && (hasBe6 || hasBe7), "QGD_STRUCTURE_COMPETING"),
          Rule((hasBc5 || hasBc6) && hasBd6 && hasBe6, "SICILIAN_STRUCTURE_COMPETING"),
          Rule(whiteStonewall || blackStonewall, "STONEWALL_COMPETING")
        ),
        baseScore = 2.2
      ),
      buildCandidate(
        id = StructureId.Stonewall,
        required = List(
          Rule(whiteStonewall || blackStonewall, "STONEWALL_CHAIN")
        ),
        support = List(
          Rule(c.lockedCenter || c.pawnTensionCount > 0, "CENTER_FRICTION"),
          Rule(fileHas(Color.White, File.F) || fileHas(Color.Black, File.F), "F_FILE_ANCHOR")
        ),
        blockers = List(
          Rule(!whiteStonewall && !blackStonewall, "CHAIN_NOT_COMPLETE")
        ),
        baseScore = 4.1
      ),
      buildCandidate(
        id = StructureId.OpenCenter,
        required = List(
          Rule(
            c.openCenter ||
              (hasWd4 && hasBd5 && hasWe3 &&
                !fileHas(Color.Black, File.E) &&
                !fileHas(Color.White, File.C) &&
                !fileHas(Color.Black, File.C)),
            "OPEN_CENTER"
          )
        ),
        support = List(
          Rule(!hasWe4 && !hasBe5, "NO_E_PAWN_LOCK"),
          Rule(c.pawnTensionCount <= 1, "LOW_CENTER_FRICTION")
        ),
        blockers = List(
          Rule(hasBe6 && (hasWe3 || hasWe4), "E_FILE_LOCK_COMPETING"),
          Rule((hasWc4 || hasBc5) && !c.openCenter, "WING_CENTER_TENSION")
        ),
        baseScore = 3.1
      ),
      buildCandidate(
        id = StructureId.LockedCenter,
        required = List(
          Rule(
            c.lockedCenter &&
              (
                (hasWe4 && hasBe5) ||
                (hasWe3 && hasBe6)
              ),
            "LOCKED_CENTER"
          )
        ),
        support = List(
          Rule(hasWd4 && hasBd5, "D_PAWN_LOCK"),
          Rule(hasWe3 && hasBe6, "E3_E6_LOCK_SHELL"),
          Rule(fileHas(Color.White, File.F) || fileHas(Color.Black, File.C), "FLANK_ANCHOR")
        ),
        blockers = List(
          Rule(c.openCenter, "OPEN_CONFLICT"),
          Rule(
            c.whiteCentralPawns == c.blackCentralPawns &&
              math.abs(c.spaceDiff) <= 1 &&
              hasWe4 && hasBe5,
            "SYMMETRIC_COMPETING"
          )
        ),
        baseScore = 3.9
      ),
      buildCandidate(
        id = StructureId.FluidCenter,
        required = List(
          Rule(
            !c.openCenter && !c.lockedCenter &&
              (c.pawnTensionCount > 0 || (hasWd4 && hasBd6 && hasWe4)),
            "FLUID_WITH_TENSION"
          )
        ),
        support = List(
          Rule(math.abs(c.spaceDiff) <= 2, "BALANCED_SPACE"),
          Rule(c.whiteCentralPawns + c.blackCentralPawns >= 2, "CENTER_PAWN_PRESENCE"),
          Rule(hasWd4 && hasWe4, "WHITE_DUAL_CENTER")
        ),
        blockers = List(
          Rule(c.openCenter || c.lockedCenter, "NON_FLUID_CENTER"),
          Rule(hasWd4 && hasBd5 && (hasWe3 || hasWe4) && hasBe6, "QGD_LOCK_COMPETING")
        ),
        baseScore = 3.3
      ),
      buildCandidate(
        id = StructureId.SymmetricCenter,
        required = List(
          Rule(
            c.whiteCentralPawns == c.blackCentralPawns &&
              math.abs(c.spaceDiff) <= 1 &&
              math.abs(c.whiteCenterControl - c.blackCenterControl) <= 1 &&
              (
                (hasWd4 && hasBd5) ||
                (hasWe4 && hasBe5) ||
                (hasWe4 && hasBe6 && !hasWd4 && !hasBd5)
              ),
            "CENTER_SYMMETRY"
          )
        ),
        support = List(
          Rule((hasWd4 && hasBd5) || (hasWe4 && hasBe5), "MIRRORED_CENTER_PAWNS"),
          Rule(c.pawnTensionCount <= 2, "CONTROLLED_TENSION")
        ),
        blockers = List(
          Rule(math.abs(c.spaceDiff) > 1, "ASYMMETRIC_SPACE"),
          Rule((hasWe3 ^ hasBe6) && (hasWd4 || hasBd5), "ASYMMETRIC_E_STRUCTURE"),
          Rule(hasWe3 && hasBe6 && hasWd4 && hasBd5, "LOCKED_SHELL_COMPETING")
        ),
        baseScore = 3.4
      )
    ).groupBy(_.id).values.map(_.maxBy(_.score)).toList

  private def resolveSymmetricOwner(
      topRaw: Candidate,
      rest: List[Candidate],
      features: PositionFeatures
  ): Candidate =
    rest.headOption match
      case Some(second) if isSymmetricPair(topRaw.id, second.id) && math.abs(topRaw.score - second.score) < 0.25 =>
        ownerPreferred(topRaw.id, second.id, features).flatMap { preferred =>
          (topRaw :: second :: Nil).find(_.id == preferred)
        }.getOrElse(topRaw)
      case _ =>
        topRaw

  private def isSymmetricPair(a: StructureId, b: StructureId): Boolean =
    Set(a, b) == Set(StructureId.IQPWhite, StructureId.IQPBlack) ||
      Set(a, b) == Set(StructureId.HangingPawnsWhite, StructureId.HangingPawnsBlack)

  private def ownerPreferred(
      a: StructureId,
      b: StructureId,
      features: PositionFeatures
  ): Option[StructureId] =
    Set(a, b) match
      case pair if pair == Set(StructureId.IQPWhite, StructureId.IQPBlack) =>
        if features.pawns.whiteIQP && !features.pawns.blackIQP then Some(StructureId.IQPWhite)
        else if features.pawns.blackIQP && !features.pawns.whiteIQP then Some(StructureId.IQPBlack)
        else None
      case pair if pair == Set(StructureId.HangingPawnsWhite, StructureId.HangingPawnsBlack) =>
        None
      case _ =>
        None

  private def buildCandidate(
      id: StructureId,
      required: List[Rule],
      support: List[Rule],
      blockers: List[Rule],
      baseScore: Double
  ): Candidate =
    val supportHits = support.count(_.flag)
    val blockerHits = blockers.count(_.flag)
    val requiredOk = required.forall(_.flag)
    val blockerHit = blockerHits > 0

    val reqPart =
      if required.isEmpty then baseScore
      else if requiredOk then baseScore
      else 0.0

    val score =
      if required.nonEmpty && !requiredOk then 0.0
      else if blockerHit then 0.0
      else (reqPart + supportHits * 0.85 - blockerHits * 1.25).max(0.0)
    val evidence =
      required.collect { case Rule(true, code) => s"REQ_$code" } :::
        support.collect { case Rule(true, code) => s"SUP_$code" } :::
        blockers.collect { case Rule(true, code) => s"BLK_$code" }

    Candidate(
      id = id,
      score = score,
      requiredOk = requiredOk,
      blockerHit = blockerHit,
      evidence = evidence.distinct
    )
