package lila.chessjudgment.analysis.strategic

import chess.*
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.model.position.PawnTopology
import lila.chessjudgment.model.judgment.{
  LineConsequence,
  LineEndgameTechniqueHorizon,
  LineEndgameTechniqueHorizonStatus,
  LineReplayStep
}
import lila.chessjudgment.model.strategic.*

object EndgamePatternOracle:

  final case class PatternSignalOverrides(
      oppositionType: Option[EndgameOppositionType] = None,
      ruleOfSquare: Option[RuleOfSquareStatus] = None,
      kingActivityDelta: Option[Int] = None,
      rookEndgamePattern: Option[RookEndgamePattern] = None,
      zugzwangLikelihood: Option[Double] = None,
      theoreticalOutcomeHint: Option[TheoreticalOutcomeHint] = None,
      rookEndgameAnchorSquares: List[Square] = Nil,
      rookEndgameGeometry: Option[RookEndgameGeometry] = None
  )

  final case class PatternMatch(
      id: String,
      outcomeOverride: Option[TheoreticalOutcomeHint] = None,
      confidenceFloor: Double,
      signalOverrides: PatternSignalOverrides = PatternSignalOverrides(),
      ambiguityPenalty: Double = 0.0
  )

  private type Detector = (Board, Color, EndgameFeature) => Option[PatternMatch]

  private val PatternConfidenceThreshold = 0.70
  private val ZugzwangThreshold = 0.65

  private val orderedDetectors: List[Detector] = List(
    detectWrongRookPawnWrongBishopFortress,
    detectVancuraDefense,
    detectPhilidorDefense,
    detectLucena,
    detectKeySquaresOppositionBreakthrough,
    detectConnectedPassers,
    detectOutsidePasserDecoy,
    detectBreakthroughSacrifice,
    detectShouldering,
    detectRetiManeuver,
    detectShortSideDefense,
    detectOppositeColoredBishopsDraw,
    detectGoodBishopRookPawnConversion,
    detectKnightBlockadeRookPawnDraw,
    detectQueenVsAdvancedPawn,
    detectTarraschDefense,
    detectPassiveRookDefense,
    detectRookAndBishopVsRookDraw,
    detectSameColoredBishopsBlockade
  )

  def detect(board: Board, color: Color, coreFeature: EndgameFeature): Option[PatternMatch] =
    orderedDetectors.iterator.flatMap(det => det(board, color, coreFeature)).take(1).toList.headOption

  def analyze(board: Board, color: Color): Option[EndgameFeature] =
    Option.when(isEndgameCandidate(board)) {
      val core = baseFeature(board, color)
      detect(board, color, core).map(applyPattern(core, _)).getOrElse(core)
    }

  def techniqueHorizons(
      startFen: String,
      replay: List[LineReplayStep],
      terminalConsequences: List[LineConsequence]
  ): List[LineEndgameTechniqueHorizon] =
    val snapshots =
      (techniqueSnapshots(startFen, -1, None) ++
        replay.zipWithIndex.flatMap { case (step, index) =>
          techniqueSnapshots(step.fenAfter, index, Some(normalizeUci(step.moveUci)))
        }).sortBy(_.plyOffset)
    val finalPlyOffset = if replay.nonEmpty then replay.size - 1 else -1
    val terminalOverrideConsequences =
      terminalConsequences.filter(consequence => LineEndgameTechniqueHorizon.terminalProofOverrides(consequence.kind))
    val replayMovesByPly =
      replay.zipWithIndex.map { case (step, index) =>
        index -> normalizeUci(step.moveUci)
      }.toMap
    val rookHorizons = snapshots
      .groupBy(_.key)
      .values
      .toList
      .flatMap(group =>
        contiguousTechniqueEpisodes(group.sortBy(_.plyOffset)).map(episode =>
          lineEndgameTechniqueHorizon(
            episode,
            snapshots,
            replayMovesByPly,
            finalPlyOffset,
            terminalOverrideConsequences
          )
        )
      )
    (rookHorizons ++ triangulationHorizons(replay))
      .distinct
      .sortBy(horizon => (horizon.pattern, horizon.techniqueSideKey, horizon.entryPlyOffset))

  def applyPattern(core: EndgameFeature, pattern: PatternMatch): EndgameFeature =
    val overrides = pattern.signalOverrides
    val updatedZugLikelihood = overrides.zugzwangLikelihood.getOrElse(core.zugzwangLikelihood)
    val updatedOutcome = overrides.theoreticalOutcomeHint
      .orElse(pattern.outcomeOverride)
      .getOrElse(core.theoreticalOutcomeHint)
    val adjustedConfidence = clamp01(math.max(core.confidence, pattern.confidenceFloor) - pattern.ambiguityPenalty)
    if adjustedConfidence < PatternConfidenceThreshold then core
    else
      core.copy(
        oppositionType = overrides.oppositionType.getOrElse(core.oppositionType),
        ruleOfSquare = overrides.ruleOfSquare.getOrElse(core.ruleOfSquare),
        kingActivityDelta = overrides.kingActivityDelta.getOrElse(core.kingActivityDelta),
        rookEndgamePattern = overrides.rookEndgamePattern.getOrElse(core.rookEndgamePattern),
        zugzwangLikelihood = updatedZugLikelihood,
        isZugzwang = updatedZugLikelihood >= ZugzwangThreshold || core.isZugzwang,
        theoreticalOutcomeHint = updatedOutcome,
        confidence = adjustedConfidence,
        primaryPattern = Some(pattern.id),
        rookEndgameAnchorSquares =
          overrides.rookEndgameGeometry.map(_.anchorSquares).filter(_.nonEmpty)
            .orElse(Option.when(overrides.rookEndgameAnchorSquares.nonEmpty)(overrides.rookEndgameAnchorSquares.distinct))
            .getOrElse(core.rookEndgameAnchorSquares),
        rookEndgameGeometry = overrides.rookEndgameGeometry.orElse(core.rookEndgameGeometry)
      )

  private def isEndgameCandidate(board: Board): Boolean =
    val matWhite = material(board, Color.White)
    val matBlack = material(board, Color.Black)
    val majorPieces = matWhite.rooks + matWhite.queens + matBlack.rooks + matBlack.queens
    val minorPieces = matWhite.knights + matWhite.bishops + matBlack.knights + matBlack.bishops
    board.occupied.count <= 15 || majorPieces <= 2 && minorPieces <= 2

  private final case class EndgameTechniqueSnapshot(
      plyOffset: Int,
      moveUci: Option[String],
      pattern: String,
      rookPattern: String,
      techniqueSide: Color,
      requiredSquares: List[String]
  ):
    def key: (String, Color) = pattern -> techniqueSide

  private def contiguousTechniqueEpisodes(group: List[EndgameTechniqueSnapshot]): List[List[EndgameTechniqueSnapshot]] =
    group.foldLeft(List.empty[List[EndgameTechniqueSnapshot]]) { case (episodes, snapshot) =>
      episodes.lastOption match
        case None =>
          List(List(snapshot))
        case Some(current) if snapshot.plyOffset <= current.last.plyOffset + 1 =>
          episodes.dropRight(1) :+ (current :+ snapshot)
        case Some(_) =>
          episodes :+ List(snapshot)
    }

  private def techniqueSnapshots(
      fen: String,
      plyOffset: Int,
      moveUci: Option[String]
  ): List[EndgameTechniqueSnapshot] =
    positionAfter(fen).toList.flatMap { position =>
      List(Color.White, Color.Black).flatMap { side =>
        analyze(position.board, side).toList.flatMap(feature =>
          for
            pattern <- feature.primaryPattern
            geometry <- feature.rookEndgameGeometry
            if feature.rookEndgamePattern != RookEndgamePattern.None
          yield
          EndgameTechniqueSnapshot(
            plyOffset = plyOffset,
            moveUci = moveUci,
            pattern = pattern,
            rookPattern = feature.rookEndgamePattern.toString,
            techniqueSide = geometry.techniqueSide,
            requiredSquares = geometry.anchorSquares.map(_.key).distinct.sorted
          )
        )
      }
    }.distinctBy(snapshot => (snapshot.plyOffset, snapshot.pattern, snapshot.techniqueSide, snapshot.requiredSquares.mkString(",")))

  private def lineEndgameTechniqueHorizon(
      group: List[EndgameTechniqueSnapshot],
      allSnapshots: List[EndgameTechniqueSnapshot],
      replayMovesByPly: Map[Int, String],
      finalPlyOffset: Int,
      terminalOverrideConsequences: List[LineConsequence]
  ): LineEndgameTechniqueHorizon =
    val first = group.head
    val last = group.last
    val requiredSquares = first.requiredSquares
    val presentAtFinal = group.exists(_.plyOffset == finalPlyOffset)
    val maintainedSquares =
      if presentAtFinal then requiredSquares.intersect(last.requiredSquares).distinct.sorted
      else Nil
    val missingAfterLastMove =
      replayMovesByPly.get(last.plyOffset + 1).orElse(
        allSnapshots
          .filter(snapshot => snapshot.plyOffset > last.plyOffset)
          .sortBy(_.plyOffset)
          .find(snapshot => snapshot.key != first.key)
          .flatMap(_.moveUci)
      )
    val horizonTerminalKinds = terminalOverrideConsequences
      .filter(consequence =>
        (first.moveUci.toList ++ missingAfterLastMove.toList)
          .exists(move => consequence.eventMove.exists(eventMove => normalizeUci(eventMove) == normalizeUci(move)))
      )
      .map(_.kind)
      .distinct
      .sortBy(_.toString)
    val baseStatus =
      if horizonTerminalKinds.nonEmpty && LineEndgameTechniqueHorizon.defensivePattern(first.pattern) then
        LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof
      else if horizonTerminalKinds.nonEmpty then
        LineEndgameTechniqueHorizonStatus.SupersededByTactic
      else if first.plyOffset > -1 && presentAtFinal then
        LineEndgameTechniqueHorizonStatus.Transitioned
      else if presentAtFinal then
        LineEndgameTechniqueHorizonStatus.Active
      else
        LineEndgameTechniqueHorizonStatus.Failed
    val triggerMove =
      if baseStatus == LineEndgameTechniqueHorizonStatus.Failed then missingAfterLastMove.orElse(first.moveUci)
      else if first.plyOffset > -1 then first.moveUci
      else None
    val brokenSquares = requiredSquares.diff(maintainedSquares).distinct.sorted
    LineEndgameTechniqueHorizon(
      pattern = first.pattern,
      rookPattern = Some(first.rookPattern),
      techniqueSide = first.techniqueSide,
      entryPlyOffset = first.plyOffset,
      terminalPlyOffset = if presentAtFinal then finalPlyOffset else last.plyOffset,
      status = baseStatus,
      triggerMove = triggerMove,
      requiredSquares = requiredSquares,
      maintainedSquares = maintainedSquares,
      brokenSquares = brokenSquares,
      terminalConsequenceKinds = horizonTerminalKinds,
      failureReason =
        Option.when(baseStatus == LineEndgameTechniqueHorizonStatus.Failed)("technique-geometry-not-maintained")
          .orElse(Option.when(baseStatus == LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof)("terminal-proof-overrides-technique"))
          .orElse(Option.when(baseStatus == LineEndgameTechniqueHorizonStatus.SupersededByTactic)("terminal-proof-supersedes-technique"))
    )

  private final case class KingRouteMove(side: Color, from: Square, to: Square)

  private def triangulationHorizons(replay: List[LineReplayStep]): List[LineEndgameTechniqueHorizon] =
    replay
      .sliding(5)
      .zipWithIndex
      .flatMap { case (window, entryPlyOffset) =>
        if window.size < 5 then None
        else
          for
            first <- kingRouteMove(window(0))
            second <- kingRouteMove(window(2))
            third <- kingRouteMove(window(4))
            if first.side == second.side && second.side == third.side
            if first.to == second.from && second.to == third.from && third.to == first.from
            routeSquares = List(first.from, first.to, second.to).distinct
            if routeSquares.size == 3
            startState <- fenState(window.head.fenBefore)
            terminalState <- fenState(window.last.fenAfter)
            if startState == terminalState
            startPosition <- positionAfter(window.head.fenBefore)
            terminalPosition <- positionAfter(window.last.fenAfter)
            if startPosition.color == first.side && terminalPosition.color == !first.side
          yield LineEndgameTechniqueHorizon(
            pattern = "Triangulation",
            rookPattern = None,
            techniqueSide = first.side,
            entryPlyOffset = entryPlyOffset,
            terminalPlyOffset = entryPlyOffset + 4,
            status = LineEndgameTechniqueHorizonStatus.Completed,
            triggerMove = Some(normalizeUci(window.head.moveUci)),
            requiredSquares = routeSquares.map(_.key).sorted
          )
      }
      .toList
      .distinct

  private def kingRouteMove(step: LineReplayStep): Option[KingRouteMove] =
    val move = normalizeUci(step.moveUci)
    for
      position <- positionAfter(step.fenBefore)
      from <- Square.fromKey(move.take(2))
      to <- Square.fromKey(move.slice(2, 4))
      piece <- position.board.pieceAt(from)
      if piece.role == King && piece.color == position.color
    yield KingRouteMove(piece.color, from, to)

  private def fenState(fen: String): Option[(String, String, String)] =
    val fields = Option(fen).getOrElse("").trim.split("\\s+").toList
    Option.when(fields.size >= 4)((fields(0), fields(2), fields(3)))

  private def positionAfter(fen: String): Option[Position] =
    Fen.read(Standard, Fen.Full(fen))

  private def normalizeUci(uci: String): String =
    Option(uci).getOrElse("").trim.toLowerCase

  private def baseFeature(board: Board, color: Color): EndgameFeature =
    val (hasOpposition, oppositionType) = opposition(board, color)
    EndgameFeature(
      hasOpposition = hasOpposition,
      isZugzwang = false,
      keySquaresControlled = Nil,
      oppositionType = oppositionType,
      ruleOfSquare = ruleOfSquareStatus(board, color),
      kingActivityDelta = kingActivity(board, color) - kingActivity(board, !color),
      rookEndgamePattern = RookEndgamePattern.None,
      theoreticalOutcomeHint = theoreticalOutcome(board),
      confidence = 0.68
    )

  private def opposition(board: Board, color: Color): (Boolean, EndgameOppositionType) =
    (board.kingPosOf(color), board.kingPosOf(!color)) match
      case (Some(ourKing), Some(theirKing)) =>
        val fileDiff = (ourKing.file.value - theirKing.file.value).abs
        val rankDiff = (ourKing.rank.value - theirKing.rank.value).abs
        val isDirect = (fileDiff == 0 && rankDiff == 2) || (fileDiff == 2 && rankDiff == 0)
        val isDistant = (fileDiff == 0 && (rankDiff == 4 || rankDiff == 6)) || (rankDiff == 0 && (fileDiff == 4 || fileDiff == 6))
        val isDiagonal = fileDiff == 2 && rankDiff == 2
        if isDirect then true -> EndgameOppositionType.Direct
        else if isDistant then true -> EndgameOppositionType.Distant
        else if isDiagonal then true -> EndgameOppositionType.Diagonal
        else false -> EndgameOppositionType.None
      case _ =>
        false -> EndgameOppositionType.None

  private def ruleOfSquareStatus(board: Board, defender: Color): RuleOfSquareStatus =
    val enemy = !defender
    val enemyPasser =
      passedPawns(board, enemy)
        .sortBy(pawn => -relativeRank(pawn, enemy))
        .headOption
    (enemyPasser, board.kingPosOf(defender)) match
      case (Some(pawn), Some(king)) =>
        promotionSquare(pawn, enemy) match
          case Some(promotion) =>
            val pawnMovesToPromote = 8 - relativeRank(pawn, enemy)
            val kingDistance = chebyshev(king, promotion)
            if kingDistance <= pawnMovesToPromote then RuleOfSquareStatus.Holds else RuleOfSquareStatus.Fails
          case None =>
            RuleOfSquareStatus.NA
      case _ =>
        RuleOfSquareStatus.NA

  private def kingActivity(board: Board, color: Color): Int =
    board.kingPosOf(color).map { king =>
      val mobility = (king.kingAttacks & ~board.byColor(color)).count
      val centerDistance = math.min(
        chebyshev(king, Square.D4),
        math.min(chebyshev(king, Square.E4), math.min(chebyshev(king, Square.D5), chebyshev(king, Square.E5)))
      )
      mobility.toInt * 2 - centerDistance
    }.getOrElse(0)

  private def theoreticalOutcome(board: Board): TheoreticalOutcomeHint =
    val white = material(board, Color.White)
    val black = material(board, Color.Black)
    val noPawns = white.pawns == 0 && black.pawns == 0
    val noMajors = white.rooks + white.queens + black.rooks + black.queens == 0
    val minorCount = white.knights + white.bishops + black.knights + black.bishops
    if noPawns && noMajors && minorCount <= 1 then TheoreticalOutcomeHint.Draw
    else TheoreticalOutcomeHint.Unclear

  private def detectWrongRookPawnWrongBishopFortress(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List(color, !color).collectFirst(Function.unlift { attacker =>
      val defender = !attacker
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.bishops != 1 || matA.rooks > 0 || matA.queens > 0 || matA.knights > 0 then None
      else if matD.rooks > 0 || matD.queens > 0 || matD.bishops > 0 || matD.knights > 0 then None
      else
        val bishopSq = board.byPiece(attacker, Bishop).squares.headOption
        val pawnOpt = board.byPiece(attacker, Pawn).squares
          .filter(isRookPawn)
          .sortBy(p => -relativeRank(p, attacker))
          .headOption
        for
          bishop <- bishopSq
          pawn <- pawnOpt
          promo <- promotionSquare(pawn, attacker)
          dKing <- board.kingPosOf(defender)
          if chebyshev(dKing, promo) <= 1
          if relativeRank(pawn, attacker) >= 7
          if
            val classicalWrongBishop = bishop.isLight != promo.isLight
            val distantFortress = bishop.isLight == promo.isLight &&
              chebyshev(bishop, pawn) >= 3 &&
              fileDistance(bishop.file, pawn.file) >= 3
            classicalWrongBishop || distantFortress
        yield
          val classicalWrongBishop = bishop.isLight != promo.isLight
          PatternMatch(
            id = "WrongRookPawnWrongBishopFortress",
            outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
            confidenceFloor = if classicalWrongBishop then 0.90 else 0.82,
            ambiguityPenalty = if classicalWrongBishop && core.kingActivityDelta.abs > 3 then 0.06 else 0.0
          )
    })

  private def detectVancuraDefense(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (defender, attacker) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.rooks != 1 || matD.rooks != 1 then None
      else if matA.queens > 0 || matD.queens > 0 || matA.knights + matA.bishops + matD.knights + matD.bishops > 0 then None
      else
        val rookPawn = passedPawns(board, attacker)
          .filter(p => isFlankPawn(p) && relativeRank(p, attacker) >= 5 && relativeRank(p, attacker) <= 6)
          .sortBy(p => -relativeRank(p, attacker))
          .headOption
        for
          pawn <- rookPawn
          dKing <- board.kingPosOf(defender)
          dRook <- board.byPiece(defender, Rook).squares.headOption
          aRook <- board.byPiece(attacker, Rook).squares.headOption
          relative = relativeRank(pawn, attacker)
          sideCheckRank = if attacker.white then Rank.Third else Rank.Sixth
          if (
            relative == 6 &&
              dRook.rank == pawn.rank &&
              fileDistance(dRook.file, pawn.file) >= 1
          ) || (
            relative == 5 &&
              dRook.rank == sideCheckRank &&
              fileDistance(dRook.file, pawn.file) >= 2 &&
              chebyshev(dKing, pawn) <= 2
          )
          if !isRookBehindPawn(board, attacker, pawn)
        yield
          val geometry = rookTechniqueGeometry(board, defender, attacker, defender, pawn, Some(aRook), Some(dRook))
          PatternMatch(
            id = "VancuraDefense",
            outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
            confidenceFloor = 0.76,
            signalOverrides = PatternSignalOverrides(
              rookEndgamePattern = Some(RookEndgamePattern.RookBehindPassedPawn),
              rookEndgameAnchorSquares = geometry.anchorSquares,
              rookEndgameGeometry = Some(geometry)
            ),
            ambiguityPenalty = if core.kingActivityDelta > 3 then 0.06 else 0.01
          )
    })

  private def detectPhilidorDefense(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (defender, attacker) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.rooks != 1 || matD.rooks != 1 then None
      else if matA.queens > 0 || matD.queens > 0 || matA.knights + matA.bishops + matD.knights + matD.bishops > 0 then None
      else
        val candidatePawn =
          passedPawns(board, attacker)
            .filterNot(isRookPawn)
            .sortBy(p => -relativeRank(p, attacker))
            .headOption
        val barrierRank = if attacker.white then Rank.Sixth else Rank.Third
        val defenderRooks = board.byPiece(defender, Rook).squares
        for
          pawn <- candidatePawn
          promo <- promotionSquare(pawn, attacker)
          dKing <- board.kingPosOf(defender)
          aKing <- board.kingPosOf(attacker)
          aRook <- board.byPiece(attacker, Rook).squares.headOption
          dRook <- defenderRooks.find { rook =>
            val sixthRankBarrier =
              rook.rank == barrierRank &&
                dKing.file == pawn.file &&
                (if attacker.white then dKing.rank.value > pawn.rank.value else dKing.rank.value < pawn.rank.value) &&
                relativeRank(aKing, attacker) <= 5 &&
                fileDistance(rook.file, pawn.file) >= 1
            val checkingResourceAfterAdvance =
              !isFlankPawn(pawn) &&
                rook.rank == pawn.rank &&
                fileDistance(rook.file, pawn.file) >= 2 &&
                chebyshev(dKing, promo) <= 2 &&
                chebyshev(aKing, pawn) <= 1 &&
                relativeRank(aKing, attacker) <= 5
            sixthRankBarrier || checkingResourceAfterAdvance
          }
          if relativeRank(pawn, attacker) == 5
        yield
          val patternBarrierRank = Option.when(dRook.rank == barrierRank)(barrierRank)
          val geometry =
            rookTechniqueGeometry(board, defender, attacker, defender, pawn, Some(aRook), Some(dRook), patternBarrierRank)
          PatternMatch(
            id = "PhilidorDefense",
            outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
            confidenceFloor = 0.78,
            signalOverrides = PatternSignalOverrides(
              rookEndgamePattern = Some(RookEndgamePattern.KingCutOff),
              rookEndgameAnchorSquares = geometry.anchorSquares,
              rookEndgameGeometry = Some(geometry)
            ),
            ambiguityPenalty = if core.kingActivityDelta > 2 then 0.06 else 0.01
          )
    })

  private def detectLucena(board: Board, color: Color, core: EndgameFeature): Option[PatternMatch] =
    val matUs = material(board, color)
    val matThem = material(board, !color)
    if matUs.rooks != 1 || matThem.rooks != 1 then None
    else if matUs.queens > 0 || matThem.queens > 0 || matUs.knights + matUs.bishops + matThem.knights + matThem.bishops > 0 then None
    else
      val passerOpt = passedPawns(board, color)
        .filter(p => !isRookPawn(p) && relativeRank(p, color) == 7)
        .sortBy(p => -relativeRank(p, color))
        .headOption
      for
        pawn <- passerOpt
        promo <- promotionSquare(pawn, color)
        ourKing <- board.kingPosOf(color)
        theirKing <- board.kingPosOf(!color)
        ourRook <- board.byPiece(color, Rook).squares.headOption
        theirRook <- board.byPiece(!color, Rook).squares.headOption
        if chebyshev(ourKing, promo) <= 1
        if chebyshev(theirKing, promo) >= 2
        if fileDistance(theirKing.file, pawn.file) >= 1
        if
          val bridgeRank = if color.white then Rank.Fourth else Rank.Fifth
          val sameFileFourthRankBridge = ourRook.rank == bridgeRank && ourRook.file == pawn.file
          sameFileFourthRankBridge || (ourRook.file != pawn.file && fileDistance(ourRook.file, pawn.file) >= 2)
      yield
        val geometry = rookTechniqueGeometry(board, color, color, !color, pawn, Some(ourRook), Some(theirRook))
        PatternMatch(
          id = "Lucena",
          outcomeOverride = Some(TheoreticalOutcomeHint.Win),
          confidenceFloor = 0.82,
          signalOverrides = PatternSignalOverrides(
            rookEndgamePattern = Some(RookEndgamePattern.RookBehindPassedPawn),
            rookEndgameAnchorSquares = geometry.anchorSquares,
            rookEndgameGeometry = Some(geometry)
          ),
          ambiguityPenalty = if core.ruleOfSquare == RuleOfSquareStatus.Fails then 0.08 else 0.0
        )

  private def detectKeySquaresOppositionBreakthrough(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    if !hasOnlyKingsAndPawns(board) then None
    else if core.oppositionType == EndgameOppositionType.None then None
    else
      val passers = passedPawns(board, color).filterNot(isRookPawn)
      if passers.size != 1 then None
      else
        val pawn = passers.head
        (board.kingPosOf(color), board.kingPosOf(!color)) match
          case (Some(ourKing), Some(theirKing)) =>
            val kingNearPawn = chebyshev(ourKing, pawn) <= 1
            val kingContest = chebyshev(theirKing, pawn) <= 3
            if !kingNearPawn || !kingContest then None
            else
              Some(
                PatternMatch(
                  id = "KeySquaresOppositionBreakthrough",
                  outcomeOverride = Some(TheoreticalOutcomeHint.Win),
                  confidenceFloor = 0.82,
                  signalOverrides = PatternSignalOverrides(
                    zugzwangLikelihood = Some(math.max(core.zugzwangLikelihood, 0.65))
                  ),
                  ambiguityPenalty = if core.ruleOfSquare == RuleOfSquareStatus.Fails then 0.10 else 0.02
                )
              )
          case _ => None

  private def detectConnectedPassers(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    if !hasOnlyKingsAndPawns(board) then None
    else if passedPawns(board, !color).nonEmpty then None
    else
      val pairs = connectedPassedPairs(board, color).filter { case (a, b) =>
        relativeRank(a, color) >= 5 &&
        relativeRank(b, color) >= 5 &&
        (relativeRank(a, color) - relativeRank(b, color)).abs <= 1
      }
      if pairs.size != 1 then None
      else
        val (first, second) = pairs.head
        val kingSupport = board.kingPosOf(color).exists { ourKing =>
          chebyshev(ourKing, first) <= 2 || chebyshev(ourKing, second) <= 2
        }
        val blockedByKing = board.kingPosOf(!color).exists { enemyKing =>
          chebyshev(enemyKing, first) <= 1 && chebyshev(enemyKing, second) <= 1
        }
        if !kingSupport || blockedByKing then None
        else
          Some(
            PatternMatch(
              id = "ConnectedPassers",
              outcomeOverride = Some(TheoreticalOutcomeHint.Win),
              confidenceFloor = 0.78,
              ambiguityPenalty = if core.kingActivityDelta <= 0 then 0.07 else 0.0
            )
          )

  private def detectOutsidePasserDecoy(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    if !hasOnlyKingsAndPawns(board) then None
    else
      val outsideCandidates = passedPawns(board, color)
        .filter(isFlankPawn)
        .sortBy(p => -relativeRank(p, color))
      (outsideCandidates.headOption, board.kingPosOf(color), board.kingPosOf(!color)) match
        case (Some(outsidePawn), Some(ourKing), Some(enemyKing)) =>
          val outsideRank = relativeRank(outsidePawn, color)
          val supportPawnOpt = board.byPiece(color, Pawn).squares
            .filter(p => p != outsidePawn && fileDistance(p.file, outsidePawn.file) >= 3)
            .sortBy(p => -relativeRank(p, color))
            .headOption
          val enemyCounterPasser = passedPawns(board, !color).exists { p =>
            relativeRank(p, !color) >= math.max(1, outsideRank - 1)
          }
          val enemyFrontBlock =
            enemyKing.file == outsidePawn.file &&
            (if color.white then enemyKing.rank.value > outsidePawn.rank.value else enemyKing.rank.value < outsidePawn.rank.value)
          val directSupportAccess = supportPawnOpt.exists(p => chebyshev(ourKing, p) <= 3)
          val indirectSupportAccess =
            chebyshev(ourKing, outsidePawn) <= 2 &&
              supportPawnOpt.exists(p => relativeRank(p, color) >= math.max(4, outsideRank - 1))
          if outsideCandidates.size != 1 || outsideRank < 4 || (!directSupportAccess && !indirectSupportAccess) || core.kingActivityDelta < 0 || enemyCounterPasser || enemyFrontBlock then None
          else
            Some(
              PatternMatch(
                id = "OutsidePasserDecoy",
                outcomeOverride = Some(TheoreticalOutcomeHint.Win),
                confidenceFloor = 0.76,
                ambiguityPenalty = if directSupportAccess then 0.01 else 0.03
              )
            )
        case _ => None

  private def detectBreakthroughSacrifice(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    if !hasOnlyKingsAndPawns(board) then None
    else
      val ourPawns = board.byPiece(color, Pawn).squares.toSet
      val enemyPawns = board.byPiece(!color, Pawn).squares.toSet
      val targetRank = if color.white then Rank.Fifth else Rank.Fourth
      val frontRank = if color.white then Rank.Sixth else Rank.Third
      val tripletStartOpt = (0 to 5).find { start =>
        val files = List(start, start + 1, start + 2)
        val ourAligned = files.forall(f => Square.at(f, targetRank.value).exists(ourPawns.contains))
        val enemyAligned = files.forall(f => Square.at(f, frontRank.value).exists(enemyPawns.contains))
        ourAligned && enemyAligned
      }
      tripletStartOpt.flatMap { start =>
        for
          middle <- Square.at(start + 1, targetRank.value)
          ourKing <- board.kingPosOf(color)
          enemyKing <- board.kingPosOf(!color)
          if chebyshev(ourKing, middle) <= 3
          if chebyshev(enemyKing, middle) >= 2
          if passedPawns(board, !color).isEmpty
        yield
          PatternMatch(
            id = "BreakthroughSacrifice",
            outcomeOverride = Some(TheoreticalOutcomeHint.Win),
            confidenceFloor = 0.74,
            signalOverrides = PatternSignalOverrides(
              zugzwangLikelihood = Some(math.max(core.zugzwangLikelihood, 0.70))
            ),
            ambiguityPenalty = 0.0
          )
      }

  private def detectShouldering(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    if !hasOnlyKingsAndPawns(board) then None
    else
      val ourPawns = board.byPiece(color, Pawn).squares
      if ourPawns.size != 1 then None
      else
        val pawn = ourPawns.head
        (board.kingPosOf(color), board.kingPosOf(!color)) match
          case (Some(ourKing), Some(theirKing)) =>
            val pawnIsPassed = passedPawns(board, color).contains(pawn)
            val advanced = relativeRank(pawn, color) >= 4
            val enemyFrontBlock =
              theirKing.file == pawn.file &&
              (if color.white then theirKing.rank.value > pawn.rank.value else theirKing.rank.value < pawn.rank.value)
            val kingClose = chebyshev(ourKing, theirKing) <= 2
            val ourNearPawn = chebyshev(ourKing, pawn) <= 1
            val enemyNearPawn = chebyshev(theirKing, pawn) <= 2
            val oppositeFlanks =
              (ourKing.file.value - pawn.file.value) * (theirKing.file.value - pawn.file.value) < 0
            val shoulderWidth = fileDistance(ourKing.file, theirKing.file) == 2
            val progressShield =
              if color.white then ourKing.rank.value >= pawn.rank.value
              else ourKing.rank.value <= pawn.rank.value
            if !pawnIsPassed || !advanced || enemyFrontBlock || !kingClose || !ourNearPawn || !enemyNearPawn || !oppositeFlanks || !shoulderWidth || !progressShield then None
            else
              Some(
                PatternMatch(
                  id = "Shouldering",
                  confidenceFloor = 0.74,
                  signalOverrides = PatternSignalOverrides(
                    zugzwangLikelihood = Some(math.max(core.zugzwangLikelihood, 0.65))
                  ),
                  ambiguityPenalty = 0.0
                )
              )
          case _ => None

  private def detectRetiManeuver(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    val _ = core
    if !hasOnlyKingsAndPawns(board) then None
    else
      val ourAllPawns = board.byPiece(color, Pawn).squares
      if ourAllPawns.count(p => !isRookPawn(p)) > 0 then None
      else
        val ourPassers = passedPawns(board, color).filter(isRookPawn).sortBy(p => -relativeRank(p, color))
        val enemyPassers = passedPawns(board, !color).filterNot(isRookPawn).sortBy(p => -relativeRank(p, !color))
        (board.kingPosOf(color), board.kingPosOf(!color), ourPassers.headOption, enemyPassers.headOption) match
          case (Some(ourKing), Some(enemyKing), Some(ourPawn), Some(enemyPawn)) =>
            val ourAdvanced = relativeRank(ourPawn, color) >= 6
            val enemyAdvanced = relativeRank(enemyPawn, !color) >= 4
            val kingNotAlreadyGuardingPawn = chebyshev(ourKing, ourPawn) >= 2
            val flankSplit = fileDistance(ourPawn.file, enemyPawn.file) >= 5
            val cornerTarget = promotionSquare(ourPawn, color)
            val kingOnCorner = cornerTarget.contains(ourKing)
            val enemyFarFromCorner = cornerTarget.forall(pr => chebyshev(enemyKing, pr) > 2)
            if ourPassers.size != 1 || enemyPassers.size != 1 || !ourAdvanced || !enemyAdvanced || !kingNotAlreadyGuardingPawn || !flankSplit || !kingOnCorner || !enemyFarFromCorner then None
            else
              Some(
                PatternMatch(
                  id = "RetiManeuver",
                  confidenceFloor = 0.74,
                  ambiguityPenalty = 0.0
                )
              )
          case _ => None

  private def detectShortSideDefense(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (defender, attacker) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.rooks != 1 || matD.rooks != 1 then None
      else if matA.queens > 0 || matD.queens > 0 || matA.knights + matA.bishops + matD.knights + matD.bishops > 0 then None
      else
        val pawnOpt = board.byPiece(attacker, Pawn).squares
          .filter(p => isFlankPawn(p) && relativeRank(p, attacker) >= 5)
          .sortBy(p => -relativeRank(p, attacker))
          .headOption
        for
          pawn <- pawnOpt
          dKing <- board.kingPosOf(defender)
          dRook <- board.byPiece(defender, Rook).squares.headOption
          aRook <- board.byPiece(attacker, Rook).squares.headOption
          if isShortSideCorner(dKing, pawn, attacker)
          if dRook.rank == pawn.rank
          if fileDistance(dRook.file, pawn.file) >= 2
        yield
          val geometry = rookTechniqueGeometry(board, defender, attacker, defender, pawn, Some(aRook), Some(dRook))
          PatternMatch(
            id = "ShortSideDefense",
            outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
            confidenceFloor = 0.76,
            signalOverrides = PatternSignalOverrides(
              rookEndgamePattern = Some(RookEndgamePattern.KingCutOff),
              rookEndgameAnchorSquares = geometry.anchorSquares,
              rookEndgameGeometry = Some(geometry)
            ),
            ambiguityPenalty = if core.kingActivityDelta > 2 then 0.05 else 0.01
          )
    })

  private def detectOppositeColoredBishopsDraw(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    val _ = color
    val wMat = material(board, Color.White)
    val bMat = material(board, Color.Black)
    if wMat.bishops != 1 || bMat.bishops != 1 then None
    else if wMat.rooks + wMat.queens + wMat.knights + bMat.rooks + bMat.queens + bMat.knights > 0 then None
    else
      val wBishop = board.byPiece(Color.White, Bishop).squares.headOption
      val bBishop = board.byPiece(Color.Black, Bishop).squares.headOption
      (wBishop, bBishop) match
        case (Some(wb), Some(bb)) =>
          val passers = passedPawns(board, Color.White).map(Color.White -> _) ++ passedPawns(board, Color.Black).map(Color.Black -> _)
          val advancedPassers = passers.filter { case (attacker, pawn) =>
            relativeRank(pawn, attacker) >= 5
          }
          val pawnDiff = (wMat.pawns - bMat.pawns).abs
          val totalPassers = passers.size
          val advancedPasserStillDrawish =
            advancedPassers.isEmpty ||
              (advancedPassers.size == 1 && {
                val (attacker, pawn) = advancedPassers.head
                val defenderBishop = if attacker.white then bb else wb
                board.kingPosOf(!attacker).exists { defenderKing =>
                  isOppositeColoredBishopBlockade(board, pawn, attacker, defenderBishop, defenderKing)
                }
              })
          val maxPawnDiff = if advancedPassers.nonEmpty then 1 else 2
          if wb.isLight == bb.isLight || pawnDiff > maxPawnDiff || totalPassers > 1 || !advancedPasserStillDrawish then None
          else
            Some(
              PatternMatch(
                id = "OppositeColoredBishopsDraw",
                outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
                confidenceFloor = if advancedPassers.nonEmpty then 0.78 else 0.80,
                ambiguityPenalty =
                  if advancedPassers.nonEmpty then 0.04
                  else if core.kingActivityDelta.abs > 3 then 0.07
                  else 0.02
              )
            )
        case _ => None

  private def detectGoodBishopRookPawnConversion(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    val matUs = material(board, color)
    val matThem = material(board, !color)
    if matUs.bishops < 1 then None
    else if matUs.rooks + matUs.queens + matUs.knights + matThem.rooks + matThem.queens + matThem.knights + matThem.bishops > 0 then None
    else
      val bishopSq = board.byPiece(color, Bishop).squares.headOption
      val pawnOpt = passedPawns(board, color)
        .filter(p => isRookPawn(p) && relativeRank(p, color) >= 6)
        .sortBy(p => -relativeRank(p, color))
        .headOption
      for
        bishop <- bishopSq
        pawn <- pawnOpt
        promo <- promotionSquare(pawn, color)
        ourKing <- board.kingPosOf(color)
        if bishop.isLight == promo.isLight
        if chebyshev(bishop, pawn) <= 2
        if chebyshev(ourKing, pawn) <= 2
      yield
        PatternMatch(
          id = "GoodBishopRookPawnConversion",
          outcomeOverride = Some(TheoreticalOutcomeHint.Win),
          confidenceFloor = 0.80,
          ambiguityPenalty = if core.ruleOfSquare == RuleOfSquareStatus.Fails then 0.05 else 0.0
        )

  private def detectKnightBlockadeRookPawnDraw(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (defender, attacker) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matD.knights < 1 then None
      else if matA.rooks + matA.queens + matA.knights + matA.bishops + matD.rooks + matD.queens + matD.bishops > 0 then None
      else
        val pawnOpt = passedPawns(board, attacker)
          .filter(p => isRookPawn(p) && relativeRank(p, attacker) >= 5)
          .sortBy(p => -relativeRank(p, attacker))
          .headOption
        for
          pawn <- pawnOpt
          promo <- promotionSquare(pawn, attacker)
          dKing <- board.kingPosOf(defender)
          if chebyshev(dKing, promo) <= 2
          if board.byPiece(defender, Knight).squares.exists { n =>
            n.knightAttacks.contains(promo) || chebyshev(n, promo) <= 1
          }
        yield
          PatternMatch(
            id = "KnightBlockadeRookPawnDraw",
            outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
            confidenceFloor = 0.72,
            ambiguityPenalty = if core.kingActivityDelta > 2 then 0.08 else 0.02
          )
    })

  private def detectQueenVsAdvancedPawn(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (attacker, defender) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.queens != 1 || matA.rooks + matA.bishops + matA.knights + matA.pawns > 0 then None
      else if matD.queens + matD.rooks + matD.bishops + matD.knights > 0 || matD.pawns != 1 then None
      else
        val pawnOpt = board.byPiece(defender, Pawn).squares.filter(p => relativeRank(p, defender) >= 7).headOption
        for
          pawn <- pawnOpt
          promo <- promotionSquare(pawn, defender)
          aKing <- board.kingPosOf(attacker)
        yield
          val isDrawingPawn = pawn.file == File.A || pawn.file == File.C || pawn.file == File.F || pawn.file == File.H
          val attackerKingTooFar = chebyshev(aKing, promo) > 3
          val isTheoreticalDraw = isDrawingPawn && attackerKingTooFar
          PatternMatch(
            id = "QueenVsAdvancedPawn",
            outcomeOverride = Some(if isTheoreticalDraw then TheoreticalOutcomeHint.Draw else TheoreticalOutcomeHint.Win),
            confidenceFloor = 0.85,
            ambiguityPenalty = if core.kingActivityDelta.abs > 2 then 0.05 else 0.0
          )
    })

  private def detectTarraschDefense(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (attacker, defender) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.rooks != 1 || matD.rooks != 1 then None
      else if matA.queens + matA.bishops + matA.knights + matD.queens + matD.bishops + matD.knights > 0 then None
      else
        val pawnOpt = passedPawns(board, attacker)
          .filter(p => relativeRank(p, attacker) >= 5)
          .sortBy(p => -relativeRank(p, attacker))
          .headOption
        for
          pawn <- pawnOpt
          dRook <- board.byPiece(defender, Rook).squares.headOption
          dKing <- board.kingPosOf(defender)
          aRook <- board.byPiece(attacker, Rook).squares.headOption
          if dRook.file == pawn.file
          // Defender rook must truly be behind the passed pawn, not in front.
          if (if attacker.white then dRook.rank.value < pawn.rank.value else dRook.rank.value > pawn.rank.value)
          if chebyshev(dKing, pawn) >= 3
        yield
          val rankGap = math.abs(dRook.rank.value - pawn.rank.value)
          val geometry = rookTechniqueGeometry(board, defender, attacker, defender, pawn, Some(aRook), Some(dRook))
          PatternMatch(
            id = "TarraschDefenseActive",
            confidenceFloor = 0.78,
            signalOverrides = PatternSignalOverrides(
              rookEndgamePattern = Some(RookEndgamePattern.TarraschDefenseActive),
              rookEndgameAnchorSquares = geometry.anchorSquares,
              rookEndgameGeometry = Some(geometry)
            ),
            ambiguityPenalty = if rankGap < 2 then 0.06 else if core.kingActivityDelta < 0 then 0.03 else 0.0
          )
    })

  private def detectPassiveRookDefense(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (defender, _) =>
      val matD = material(board, defender)
      if matD.rooks != 1 then None
      else if board.queens.count > 0 || board.bishops.count > 0 || board.knights.count > 0 then None
      else
        val ownPassers = passedPawns(board, defender).filter(p => relativeRank(p, defender) >= 5)
        board.byPiece(defender, Rook).squares.headOption.flatMap { rook =>
          ownPassers.find { pawn =>
            rook.file == pawn.file &&
            // Rook sits in front of its own passed pawn and is tied to passive duty.
            (if defender.white then rook.rank.value > pawn.rank.value else rook.rank.value < pawn.rank.value) &&
            math.abs(rook.rank.value - pawn.rank.value) <= 2
          }.map { pawn =>
            val geometry =
              rookTechniqueGeometry(
                board,
                defender,
                defender,
                !defender,
                pawn,
                Some(rook),
                board.byPiece(!defender, Rook).squares.headOption
              )
            PatternMatch(
              id = "PassiveRookDefense",
              confidenceFloor = 0.75,
              signalOverrides = PatternSignalOverrides(
                rookEndgamePattern = Some(RookEndgamePattern.PassiveRookDefense),
                rookEndgameAnchorSquares = geometry.anchorSquares,
                rookEndgameGeometry = Some(geometry)
              ),
              ambiguityPenalty = if defender == color && core.kingActivityDelta < 0 then 0.05 else 0.0
            )
          }
        }
    })

  private def detectRookAndBishopVsRookDraw(
      board: Board,
      color: Color,
      _core: EndgameFeature
  ): Option[PatternMatch] =
    val _ = _core
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (attacker, defender) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.rooks != 1 || matA.bishops != 1 || matD.rooks != 1 || matD.bishops != 0 then None
      else if matA.queens > 0 || matD.queens > 0 || matA.knights > 0 || matD.knights > 0 || matA.pawns > 0 || matD.pawns > 0 then None
      else
        for
          bishop <- board.byPiece(attacker, Bishop).squares.headOption
          dKing <- board.kingPosOf(defender)
          if isCornerRegion(dKing)
          if dKing.isLight != bishop.isLight // Safe corner is opposite color of bishop
        yield
          PatternMatch(
            id = "RookAndBishopVsRookDraw",
            outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
            confidenceFloor = 0.85
          )
    })

  private def detectSameColoredBishopsBlockade(
      board: Board,
      color: Color,
      core: EndgameFeature
  ): Option[PatternMatch] =
    List((color, !color), (!color, color)).collectFirst(Function.unlift { (attacker, defender) =>
      val matA = material(board, attacker)
      val matD = material(board, defender)
      if matA.bishops != 1 || matD.bishops != 1 then None
      else if matA.rooks + matA.queens + matA.knights > 0 || matD.rooks + matD.queens + matD.knights > 0 then None
      else if matA.pawns > 2 || matD.pawns > 2 then None
      else
        (for
          aBishop <- board.byPiece(attacker, Bishop).squares.headOption
          dBishop <- board.byPiece(defender, Bishop).squares.headOption
          if aBishop.isLight == dBishop.isLight
          dKing <- board.kingPosOf(defender)
        yield (dBishop, dKing)).flatMap { (dBishop, dKing) =>
          val attackerPawns = board.byPiece(attacker, Pawn).squares.toList
          val advancedAttackerPawns = attackerPawns.filter(p => relativeRank(p, attacker) >= 4)
          val defenderPawns = board.byPiece(defender, Pawn).squares.toList
          val blockingPawnsOnBishopColor = defenderPawns.filter(_.isLight == dBishop.isLight)
          val isBlockading =
            advancedAttackerPawns.nonEmpty &&
              blockingPawnsOnBishopColor.nonEmpty &&
              advancedAttackerPawns.forall { pawn =>
                blockingPawnsOnBishopColor.exists { blocker =>
                  blocker.file == pawn.file &&
                  (if attacker.white then blocker.rank.value >= pawn.rank.value else blocker.rank.value <= pawn.rank.value) &&
                  math.abs(blocker.rank.value - pawn.rank.value) <= 2
                } || chebyshev(dKing, pawn) <= 2
              }
          if !isBlockading then None
          else
            Some(
              PatternMatch(
                id = "SameColoredBishopsBlockade",
                outcomeOverride = Some(TheoreticalOutcomeHint.Draw),
                confidenceFloor = 0.78,
                ambiguityPenalty = if core.kingActivityDelta > 2 then 0.06 else 0.0
              )
            )
        }
    })

  private final case class MaterialCounts(
      pawns: Int,
      knights: Int,
      bishops: Int,
      rooks: Int,
      queens: Int
  )

  private def material(board: Board, color: Color): MaterialCounts =
    MaterialCounts(
      pawns = board.byPiece(color, Pawn).count,
      knights = board.byPiece(color, Knight).count,
      bishops = board.byPiece(color, Bishop).count,
      rooks = board.byPiece(color, Rook).count,
      queens = board.byPiece(color, Queen).count
    )

  private def hasOnlyKingsAndPawns(board: Board): Boolean =
    board.queens.count == 0 && board.rooks.count == 0 && board.bishops.count == 0 && board.knights.count == 0

  private def isRookPawn(square: Square): Boolean =
    square.file == File.A || square.file == File.H

  private def isFlankPawn(square: Square): Boolean =
    square.file == File.A || square.file == File.B || square.file == File.G || square.file == File.H

  private def isCornerRegion(square: Square): Boolean =
    (square.file.value <= File.B.value || square.file.value >= File.G.value) &&
    (square.rank.value <= Rank.Second.value || square.rank.value >= Rank.Seventh.value)

  private def isShortSideCorner(defenderKing: Square, pawn: Square, attacker: Color): Boolean =
    val targetRank = if attacker.white then Rank.Eighth else Rank.First
    val targetFile =
      if pawn.file.value <= File.B.value then File.A
      else if pawn.file.value >= File.G.value then File.H
      else pawn.file
    defenderKing.rank == targetRank && defenderKing.file == targetFile

  private def connectedPassedPairs(board: Board, color: Color): List[(Square, Square)] =
    val passers = passedPawns(board, color).sortBy(_.file.value)
    passers
      .combinations(2)
      .collect {
        case List(a, b) if fileDistance(a.file, b.file) == 1 => (a, b)
      }
      .toList

  private def passedPawns(board: Board, color: Color): List[Square] =
    PawnTopology.passedPawns(color, board.byPiece(color, Pawn), board.byPiece(!color, Pawn))

  private def relativeRank(square: Square, color: Color): Int =
    if color.white then square.rank.value + 1 else 8 - square.rank.value

  private def promotionSquare(pawnSq: Square, color: Color): Option[Square] =
    val promoRank = if color.white then 7 else 0
    Square.at(pawnSq.file.value, promoRank)

  private def rookTechniqueGeometry(
      board: Board,
      techniqueSide: Color,
      attacker: Color,
      defender: Color,
      pawn: Square,
      attackerRook: Option[Square],
      defenderRook: Option[Square],
      barrierRank: Option[Rank] = None
  ): RookEndgameGeometry =
    RookEndgameGeometry(
      techniqueSide = techniqueSide,
      strongSide = attacker,
      defendingSide = defender,
      passedPawn = pawn,
      promotionSquare = promotionSquare(pawn, attacker).getOrElse(pawn),
      attackingKing = board.kingPosOf(attacker),
      defendingKing = board.kingPosOf(defender),
      attackingRook = attackerRook,
      defendingRook = defenderRook,
      barrierRank = barrierRank
    )

  private def advanceSquare(square: Square, color: Color): Option[Square] =
    Square.at(square.file.value, square.rank.value + (if color.white then 1 else -1))

  private def isOppositeColoredBishopBlockade(
      board: Board,
      pawn: Square,
      attacker: Color,
      defenderBishop: Square,
      defenderKing: Square
  ): Boolean =
    val stopSquare = advanceSquare(pawn, attacker).orElse(promotionSquare(pawn, attacker))
    val promotion = promotionSquare(pawn, attacker)
    val kingBlockade =
      stopSquare.exists(sq => chebyshev(defenderKing, sq) <= 1) ||
        promotion.exists(sq => chebyshev(defenderKing, sq) <= 1)
    val bishopNet =
      stopSquare.exists(sq => bishopControlsSquare(board, defenderBishop, sq)) ||
        promotion.exists(sq => bishopControlsSquare(board, defenderBishop, sq))
    kingBlockade && bishopNet

  private def bishopControlsSquare(board: Board, bishop: Square, target: Square): Boolean =
    val fileDelta = target.file.value - bishop.file.value
    val rankDelta = target.rank.value - bishop.rank.value
    if fileDelta == 0 || fileDelta.abs != rankDelta.abs then false
    else
      val fileStep = if fileDelta > 0 then 1 else -1
      val rankStep = if rankDelta > 0 then 1 else -1
      var file = bishop.file.value + fileStep
      var rank = bishop.rank.value + rankStep
      var clear = true
      while clear && (file != target.file.value || rank != target.rank.value) do
        clear = Square.at(file, rank).forall(sq => !board.occupied.contains(sq))
        file += fileStep
        rank += rankStep
      clear

  private def isRookBehindPawn(board: Board, color: Color, pawnSq: Square): Boolean =
    board.byPiece(color, Rook).squares.exists { rookSq =>
      rookSq.file == pawnSq.file &&
      (if color.white then rookSq.rank.value < pawnSq.rank.value else rookSq.rank.value > pawnSq.rank.value)
    }

  private def chebyshev(a: Square, b: Square): Int =
    math.max((a.file.value - b.file.value).abs, (a.rank.value - b.rank.value).abs)

  private def fileDistance(a: File, b: File): Int =
    (a.value - b.value).abs

  private def clamp01(v: Double): Double =
    math.max(0.0, math.min(1.0, v))
