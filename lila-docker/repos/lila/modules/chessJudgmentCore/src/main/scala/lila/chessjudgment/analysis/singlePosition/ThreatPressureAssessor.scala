package lila.chessjudgment.analysis.singlePosition

import chess.{ Color, File }
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.tactical.{ BoundedReplayStep, TacticalRelationEvidence }
import lila.chessjudgment.model.Motif

/**
 * Threat Analyzer
 * 
 * Analyzes threats from the opponent's perspective and assesses
 * defensive requirements for the side to move.
 * 
 * Uses Hybrid approach:
 * 1. Tactical motif inputs -> base threat detection (filtered by side)
 * 2. MultiPV delta -> loss correction (signed, not absolute)
 * 3. Position assessment -> threshold adjustment
 * 
 * IMPORTANT: Eval POV Assumption
 * This code assumes that `PvLine.sideRelativeEvalCp` and `PvLine.mate` are already normalized
 * to the side-to-move's perspective (positive = good for side to move).
 * If your engine provides eval from White's POV, you must flip the sign
 * for Black before passing to this analyzer.
 */
object ThreatPressureAssessor:

  private case class ThreatProfile(
    kind: ThreatKind,
    turnsToImpact: Int,
    baseLossCp: Int
  )

  /**
   * Analyze threats in the position from the opponent's perspective.
   * 
   * @param motifs tactical motif inputs detected in the position
   * @param multiPv MultiPV lines from engine analysis
   * @param positionAssessment position assessment for threshold adjustment
   * @param sideToMove side to move in the analyzed position
   * @return Complete threat analysis
   */
  def analyze(
    fen: String,
    motifs: List[Motif],
    multiPv: List[PvLine],
    positionAssessment: SinglePositionAssessment,
    sideToMove: Color
  ): ThreatAnalysis =
    val isWhiteToMove = sideToMove.white
    val suppressStaticRelationThreats =
      positionAssessment.gamePhase.isEndgame ||
        positionAssessment.drawBias.insufficientMaterial ||
        (
          positionAssessment.simplifyBias.isEndgameNear &&
            !positionAssessment.gamePhase.queensOnBoard &&
            positionAssessment.gamePhase.minorPiecesCount <= 2
        )
    
    val opponentThreats = extractOpponentThreats(motifs, isWhiteToMove, suppressStaticRelationThreats)
    val correctedThreats = correctWithMultiPv(opponentThreats, multiPv, fen)
    val withDefenses = populateDefenseEvidence(correctedThreats, multiPv)
    val activeCounterThreat = activeCounterThreatAvailable(fen, motifs, multiPv, sideToMove)
    computeAggregates(withDefenses, multiPv, positionAssessment, activeCounterThreat)

  /**
   * Extract opponent threats from tactical motif inputs.
   * We keep conservative base timing/loss estimates so positional pressure survives
   * even when MultiPV evidence is thin.
   */
  private def extractOpponentThreats(
      motifs: List[Motif],
      isWhiteToMove: Boolean,
      suppressStaticRelationThreats: Boolean
  ): List[Threat] =
    val opponentColor = if isWhiteToMove then Color.Black else Color.White

    motifs.flatMap { motif =>
      if threateningColor(motif).contains(opponentColor) then
        threatProfileFor(motif, suppressStaticRelationThreats).map { profile =>
          Threat(
            kind = profile.kind,
            lossIfIgnoredCp = profile.baseLossCp,
            lossIfIgnoredWinPercent = None,
            turnsToImpact = profile.turnsToImpact,
            evidenceSource = ThreatEvidenceSource.MotifPattern,
            motifs = List(motif),
            attackSquares = extractAttackSquares(motif),
            targetPieces = extractTargetPieces(motif),
            bestDefense = None,
            defenseCount = 0
          )
        }
      else None
    }

  private def threatProfileFor(motif: Motif, suppressStaticRelationThreats: Boolean): Option[ThreatProfile] =
    motif match
      case _: Motif.BackRankMate | _: Motif.MateNet | _: Motif.SmotheredMate =>
        Some(ThreatProfile(ThreatKind.Mate, 1, 0))
      case m: Motif.Check if m.checkType == Motif.CheckType.Mate || m.checkType == Motif.CheckType.Smothered =>
        Some(ThreatProfile(ThreatKind.Mate, 1, 0))
      case _: Motif.Check | _: Motif.DoubleCheck =>
        Some(ThreatProfile(ThreatKind.Material, 1, 180))
      case m: Motif.Capture =>
        val baseLoss = m.captureType match
          case Motif.CaptureType.Winning => 320
          case Motif.CaptureType.Exchange | Motif.CaptureType.Recapture => 220
          case Motif.CaptureType.ExchangeSacrifice => 180
          case _ => 150
        Some(ThreatProfile(ThreatKind.Material, 1, baseLoss))
      case (_: Motif.Pin | _: Motif.XRay) if suppressStaticRelationThreats =>
        None
      case _: Motif.Fork | _: Motif.Pin | _: Motif.Skewer | _: Motif.DiscoveredAttack |
          _: Motif.Deflection | _: Motif.Decoy | _: Motif.Overloading | _: Motif.Interference |
          _: Motif.Clearance | _: Motif.Zwischenzug | _: Motif.RemovingTheDefender | _: Motif.XRay =>
        Some(ThreatProfile(ThreatKind.Material, 1, 180))
      case m: Motif.TrappedPiece =>
        Some(ThreatProfile(ThreatKind.Material, if m.isValuableTrap then 1 else 2, if m.isValuableTrap then 240 else 160))
      case _: Motif.PawnPromotion =>
        Some(ThreatProfile(ThreatKind.Material, 1, 900))
      case m: Motif.PassedPawnPush =>
        val rel = m.relativeTo
        Some(
          ThreatProfile(
            kind = if rel >= 6 then ThreatKind.Material else ThreatKind.Positional,
            turnsToImpact = if rel >= 6 then 1 else 2,
            baseLossCp = if rel >= 6 then 260 else 140
          )
        )
      case m: Motif.PassedPawn =>
        val rel = Motif.relativeRank(m.rank, m.color)
        Some(
          ThreatProfile(
            kind = if rel >= 6 then ThreatKind.Material else ThreatKind.Positional,
            turnsToImpact = if rel >= 6 then 2 else if rel >= 4 then 3 else 4,
            baseLossCp = if rel >= 6 then 220 else if m.isProtected then 130 else 100
          )
        )
      case _: Motif.WeakBackRank =>
        Some(ThreatProfile(ThreatKind.Positional, 2, 120))
      case _: Motif.RookBehindPassedPawn | _: Motif.SeventhRankInvasion | _: Motif.KingCutOff =>
        Some(ThreatProfile(ThreatKind.Positional, 2, 110))
      case _: Motif.OpenFileControl | _: Motif.SemiOpenFileControl | _: Motif.RookLift |
          _: Motif.Battery | _: Motif.SpaceAdvantage | _: Motif.Initiative |
          _: Motif.Domination | _: Motif.Blockade =>
        Some(ThreatProfile(ThreatKind.Positional, 3, 100))
      case _ => None

  /**
   * Maps motifs to the side actually posing the threat.
   * Some motifs store the vulnerable side (`WeakBackRank`, `TrappedPiece`) rather than the attacker.
   */
  private def threateningColor(motif: Motif): Option[Color] =
    motif match
      case m: Motif.Fork => Some(m.color)
      case m: Motif.Pin => Some(m.color)
      case m: Motif.Skewer => Some(m.color)
      case m: Motif.DiscoveredAttack => Some(m.color)
      case m: Motif.Deflection => Some(m.color)
      case m: Motif.Decoy => Some(m.color)
      case m: Motif.Overloading => Some(m.color)
      case m: Motif.DoubleCheck => Some(m.color)
      case m: Motif.BackRankMate => Some(m.color)
      case m: Motif.Interference => Some(m.color)
      case m: Motif.Clearance => Some(m.color)
      case m: Motif.Check => Some(m.color)
      case m: Motif.Capture => Some(m.color)
      case m: Motif.Zwischenzug => Some(m.color)
      case m: Motif.RemovingTheDefender => Some(m.color)
      case m: Motif.XRay => Some(m.color)
      case m: Motif.MateNet => Some(m.color)
      case m: Motif.SmotheredMate => Some(m.color)
      case m: Motif.TrappedPiece => Some(!m.color)
      case m: Motif.PawnAdvance => Some(m.color)
      case m: Motif.PawnBreak => Some(m.color)
      case m: Motif.PawnPromotion => Some(m.color)
      case m: Motif.PassedPawnPush => Some(m.color)
      case m: Motif.RookLift => Some(m.color)
      case m: Motif.Battery => Some(m.color)
      case m: Motif.PassedPawn => Some(m.color)
      case m: Motif.OpenFileControl => Some(m.color)
      case m: Motif.SemiOpenFileControl => Some(m.color)
      case m: Motif.SeventhRankInvasion => Some(m.color)
      case m: Motif.RookBehindPassedPawn => Some(m.color)
      case m: Motif.KingCutOff => Some(m.color)
      case m: Motif.WeakBackRank => Some(!m.color)
      case m: Motif.SpaceAdvantage => Some(m.color)
      case m: Motif.Initiative => Some(m.color)
      case m: Motif.Domination => Some(m.color)
      case m: Motif.Blockade => Some(m.color)
      case _ => None

  private def extractAttackSquares(motif: Motif): List[String] =
    motif match
      case m: Motif.Fork => List(m.square.key)
      case m: Motif.Check => List(m.targetSquare.key)
      case m: Motif.Capture => List(m.square.key)
      case m: Motif.Deflection => List(m.fromSquare.key)
      case m: Motif.Decoy => List(m.toSquare.key)
      case m: Motif.Pin => m.pinnedSq.map(_.key).toList
      case m: Motif.Skewer => m.frontSq.map(_.key).toList
      case m: Motif.DiscoveredAttack => m.targetSq.map(_.key).toList
      case m: Motif.TrappedPiece => List(m.trappedSquare.key)
      case m: Motif.PawnPromotion => List(s"${m.file.char.toString.toLowerCase}${if m.color.white then 8 else 1}")
      case m: Motif.PassedPawnPush => List(s"${m.file.char.toString.toLowerCase}${m.toRank}")
      case m: Motif.PassedPawn => List(s"${m.file.char.toString.toLowerCase}${m.rank}")
      case _ => Nil

  private def extractTargetPieces(motif: Motif): List[String] =
    motif match
      case m: Motif.Fork => m.targets.map(_.name)
      case m: Motif.Pin => List(m.pinnedPiece.name, m.targetBehind.name)
      case m: Motif.Skewer => List(m.frontPiece.name, m.backPiece.name)
      case m: Motif.DiscoveredAttack => List(m.target.name)
      case m: Motif.Deflection => List(m.piece.name)
      case m: Motif.Decoy => List(m.piece.name)
      case _: Motif.Check => List("King")
      case m: Motif.Capture => List(m.captured.name)
      case m: Motif.TrappedPiece => List(m.trappedRole.name)
      case _: Motif.WeakBackRank | _: Motif.BackRankMate | _: Motif.MateNet => List("King")
      case _ => Nil

  /**
   * Correct MultiPV detection:
   * - Use UCI pattern matching (not SAN)
   * - Use side-to-move-normalized eval deltas
   * - Classify threats correctly based on mate signals
   */
  private def correctWithMultiPv(
    threats: List[Threat],
    multiPv: List[PvLine],
    fen: String
  ): List[Threat] =
    if multiPv.size < 2 then
      // Insufficient data - return base threats
      threats
    else
      val bestLine = multiPv.head
      val secondLine = multiPv(1)
      val evalLoss =
        if bestLine.mate.isEmpty && secondLine.mate.isEmpty then (bestLine.sideRelativeEvalCp - secondLine.sideRelativeEvalCp).max(0)
        else 0
      val winPercentLoss = bestLine.winPercentLossTo(secondLine)
      val bestLineFirstStep = firstLegalStep(fen, bestLine)
      val secondLineFirstStep = firstLegalStep(fen, secondLine)
      val secondLineIsCapture = secondLineFirstStep.exists(_.move.captures)
      val secondLineIsMate = secondLine.mate.exists(_ < 0)  // Negative mate = opponent mates us
      
      // Detect threat if: mate, board-verified capture, or significant win-percent loss.
      val hasSignificantThreat =
        secondLineIsMate ||
          secondLineIsCapture ||
          winPercentLoss >= JudgmentThresholds.MATERIAL_THREAT_WP
      
      // Suppress only quiet implied threats in undeveloped positions.
      val totalPieces = chess.format.Fen.read(chess.variant.Standard, chess.format.Fen.Full(fen)).map(_.board.occupied.count)
      val isQuietOpeningImpliedThreat =
        totalPieces.exists(_ >= 31) &&
          !secondLineIsMate &&
          !secondLineIsCapture &&
          winPercentLoss < JudgmentThresholds.URGENT_THREAT_WP
      
      if hasSignificantThreat &&
          (secondLineIsMate || winPercentLoss >= JudgmentThresholds.SIGNIFICANT_THREAT_WP) &&
          !isQuietOpeningImpliedThreat
      then
        val kind = if secondLineIsMate then ThreatKind.Mate
                   else if winPercentLoss >= JudgmentThresholds.MATERIAL_THREAT_WP
                   then ThreatKind.Material
                   else ThreatKind.Positional
        
        // Extract attack square from legal replay only.
        val secondLineDestSquare = secondLineFirstStep.map(_.move.dest.key)
        val bestLineDestSquare = bestLineFirstStep.map(_.move.dest.key)
        val attackSquaresList = secondLineDestSquare.toList ++ bestLineDestSquare.toList
        
        val impliedThreat = Threat(
          kind = kind,
          lossIfIgnoredCp = if secondLineIsMate then 0 else evalLoss,
          lossIfIgnoredWinPercent = Some(if secondLineIsMate then 100.0 else winPercentLoss),
          turnsToImpact = 1,
          evidenceSource = ThreatEvidenceSource.CandidateLineValueDelta,
          motifs = Nil,
          attackSquares = attackSquaresList.distinct.take(1), // Keep only first valid square
          targetPieces = Nil,
          bestDefense = bestLineFirstStep.map(_.uci),
          defenseCount = 1
        )
        
        // Preserve existing base threats alongside the MultiPV-implied threat.
        impliedThreat :: threats
      else if winPercentLoss > 0.0 then
        // Boost existing threats with eval evidence
        threats.map { t =>
          val boosted =
            winPercentLoss > t.lossIfIgnoredWinPercent.getOrElse(0.0)
          t.copy(
            lossIfIgnoredCp = t.lossIfIgnoredCp.max(evalLoss),
            lossIfIgnoredWinPercent = Some(t.lossIfIgnoredWinPercent.getOrElse(0.0).max(winPercentLoss)),
            evidenceSource = if boosted then ThreatEvidenceSource.MotifAndLineValueDelta else t.evidenceSource
          )
        }
      else
        threats

  private def firstLegalStep(
    fen: String,
    pv: PvLine
  ): Option[BoundedReplayStep] =
    TacticalRelationEvidence
      .boundedReplay(fen, pv.moves, maxPlies = 1)
      .flatMap(_.headOption)

  private def activeCounterThreatAvailable(
    fen: String,
    motifs: List[Motif],
    multiPv: List[PvLine],
    sideToMove: Color
  ): Boolean =
    if multiPv.size < 2 then false
    else
      val bestLine = multiPv.head
      val secondLine = multiPv(1)
      val avgDepth = multiPv.map(_.depth).sum.toDouble / multiPv.size
      val bestRoot = bestLine.moves.headOption.map(normalizedMove)
      avgDepth >= JudgmentThresholds.THREAT_RELIABLE_DEPTH &&
        bestLine.winPercentLossTo(secondLine) >= JudgmentThresholds.SIGNIFICANT_THREAT_WP &&
        bestRoot.exists(root =>
          firstLegalStep(fen, bestLine).exists(activeForcingRoot) ||
            motifs.exists(activeCounterThreatMotif(_, sideToMove, root))
        )

  private def activeForcingRoot(step: BoundedReplayStep): Boolean =
    step.move.after.check.yes ||
      step.move.promotion.isDefined

  private def activeCounterThreatMotif(motif: Motif, sideToMove: Color, rootMove: String): Boolean =
    threateningColor(motif).contains(sideToMove) &&
      motifMove(motif).exists(move => normalizedMove(move) == rootMove) &&
      (
        motif match
          case _: Motif.PawnBreak | _: Motif.PassedPawnPush | _: Motif.PawnPromotion |
              _: Motif.RookLift | _: Motif.OpenFileControl | _: Motif.SemiOpenFileControl |
              _: Motif.SpaceAdvantage | _: Motif.Initiative | _: Motif.Battery =>
            true
          case m: Motif.PawnAdvance =>
            centralFile(m.file) || m.relativeTo > m.relativeFrom
          case m: Motif.Capture =>
            m.captureType == Motif.CaptureType.Winning ||
              m.captureType == Motif.CaptureType.ExchangeSacrifice
          case _: Motif.Check | _: Motif.Zwischenzug =>
            true
          case _ =>
            false
      )

  private def motifMove(motif: Motif): Option[String] =
    motif match
      case m: Motif.PawnAdvance          => m.move
      case m: Motif.PawnBreak            => m.move
      case m: Motif.PawnPromotion        => m.move
      case m: Motif.PassedPawnPush       => m.move
      case m: Motif.RookLift             => m.move
      case m: Motif.Check                => m.move
      case m: Motif.Capture              => m.move
      case m: Motif.Zwischenzug          => m.move
      case m: Motif.Battery              => m.move
      case m: Motif.OpenFileControl      => m.move
      case m: Motif.SemiOpenFileControl  => m.move
      case m: Motif.SpaceAdvantage       => m.move
      case m: Motif.Initiative           => m.move
      case _                             => None

  private def normalizedMove(move: String): String =
    move.trim.toLowerCase

  private def centralFile(file: File): Boolean =
    file == File.C || file == File.D || file == File.E || file == File.F

  case class ThresholdConfig(
    ignorableThresholdWinPercent: Double,
    immediateThresholdWinPercent: Double,
    overrideDefenseRequired: Boolean,
    depthReliable: Boolean
  )

  private def adjustThresholds(positionAssessment: SinglePositionAssessment, multiPv: List[PvLine]): ThresholdConfig =
    val avgDepth = if multiPv.isEmpty then 0 else multiPv.map(_.depth).sum / multiPv.size
    
    ThresholdConfig(
      ignorableThresholdWinPercent =
        if positionAssessment.simplifyBias.isSimplificationWindow then JudgmentThresholds.IGNORABLE_THREAT_WP + 0.75
        else JudgmentThresholds.IGNORABLE_THREAT_WP,
      
      immediateThresholdWinPercent =
        if positionAssessment.riskProfile.isHighRisk then JudgmentThresholds.IGNORABLE_THREAT_WP
        else JudgmentThresholds.SIGNIFICANT_THREAT_WP,
      
      overrideDefenseRequired = positionAssessment.criticality.isForced,
      depthReliable = avgDepth >= JudgmentThresholds.THREAT_RELIABLE_DEPTH
    )

  private def populateDefenseEvidence(
    threats: List[Threat],
    multiPv: List[PvLine]
  ): List[Threat] =
    if threats.isEmpty then threats
    else if multiPv.isEmpty then threats
    else
      // Count defenses that keep eval within the central defense tolerance.
      val bestLine = multiPv.head
      val adequateDefenses = multiPv.filter { pv =>
        bestLine.winPercentLossTo(pv) <= JudgmentThresholds.ONLY_DEFENSE_TOLERANCE_WP
      }
      val defenseCount = adequateDefenses.size
      
      // Damp threat loss when the average MultiPV depth is below the reliability floor.
      val avgDepth = multiPv.map(_.depth).sum.toDouble / multiPv.size.max(1)
      val reliabilityFactor = if avgDepth >= JudgmentThresholds.THREAT_RELIABLE_DEPTH then 1.0 else 0.8
      
      threats.map { t =>
        val lineBacked = t.evidenceSource != ThreatEvidenceSource.MotifPattern
        t.copy(
          defenseCount = if lineBacked then defenseCount else t.defenseCount,
          bestDefense = if lineBacked then multiPv.headOption.flatMap(_.moves.headOption) else t.bestDefense,
          lossIfIgnoredCp = (t.lossIfIgnoredCp * reliabilityFactor).toInt,
          lossIfIgnoredWinPercent = t.lossIfIgnoredWinPercent.map(_ * reliabilityFactor)
        )
      }

  private def computeAggregates(
    threats: List[Threat],
    multiPv: List[PvLine],
    positionAssessment: SinglePositionAssessment,
    activeCounterThreat: Boolean
  ): ThreatAnalysis =
    val maxLoss = threats.map(_.lossIfIgnoredCp).maxOption.getOrElse(0)
    val maxWinPercentLoss = threats.flatMap(_.lossIfIgnoredWinPercent).maxOption
    val hasMate = threats.exists(_.kind == ThreatKind.Mate)
    val hasImmediate = threats.exists(_.isImmediate)
    val hasStrategic = threats.exists(_.isStrategic)
    val thresholds = adjustThresholds(positionAssessment, multiPv)
    val hasUrgentImmediate = threats.exists { t =>
      t.isImmediate &&
        t.lossIfIgnoredWinPercent.exists(_ >= thresholds.immediateThresholdWinPercent)
    }
    
    val severity = 
      if hasMate ||
          maxWinPercentLoss.exists(_ >= JudgmentThresholds.URGENT_THREAT_WP)
      then ThreatSeverity.Urgent
      else if maxWinPercentLoss.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP)
      then ThreatSeverity.Important
      else ThreatSeverity.Low
    
    val defenseRequired = thresholds.overrideDefenseRequired ||
                          hasMate ||
                          maxWinPercentLoss.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP)
    
    val threatIgnorable = !defenseRequired &&
                          maxWinPercentLoss.forall(_ < thresholds.ignorableThresholdWinPercent) &&
                          positionAssessment.riskProfile.isLowRisk
    
    val counterThreatBetter =
      activeCounterThreat &&
        !hasMate &&
        maxWinPercentLoss.exists(_ >= JudgmentThresholds.SIGNIFICANT_THREAT_WP) &&
        threats.nonEmpty
    
    val prophylaxisNeeded =
      !hasImmediate && hasStrategic &&
        maxWinPercentLoss.exists(_ >= JudgmentThresholds.SIGNIFICANT_THREAT_WP)
    val totalDefenses = threats.map(_.defenseCount).sum
    val resourceAvailable = threats.isEmpty || totalDefenses > 0
    
    val primaryDriver =
      if hasMate then ThreatDriver.MateThreat
      else if maxWinPercentLoss.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP)
      then ThreatDriver.MaterialThreat
      else if threats.nonEmpty then ThreatDriver.PositionalThreat
      else ThreatDriver.NoThreat
    val alternatives = multiPv.take(3).flatMap(_.moves.headOption).distinct
    
    val defense = DefenseAssessment(
      necessity = severity,
      onlyDefense = if totalDefenses == 1 then threats.headOption.flatMap(_.bestDefense) else None,
      alternatives = alternatives,
      counterIsBetter = counterThreatBetter,
      prophylaxisNeeded = prophylaxisNeeded,
      resourceCoverageScore = if resourceAvailable then 80 else 20
    )
    
    ThreatAnalysis(
      threats = threats,
      defense = defense,
      threatSeverity = severity,
      immediateThreat = hasImmediate || hasUrgentImmediate,
      strategicThreat = hasStrategic,
      threatIgnorable = threatIgnorable,
      defenseRequired = defenseRequired,
      counterThreatBetter = counterThreatBetter,
      prophylaxisNeeded = prophylaxisNeeded,
      resourceAvailable = resourceAvailable,
      maxLossIfIgnored = maxLoss,
      maxWinPercentLossIfIgnored = maxWinPercentLoss,
      primaryDriver = primaryDriver,
      insufficientData = multiPv.size < 2
    )
  def noThreat: ThreatAnalysis = ThreatAnalysis(
    threats = Nil,
    defense = DefenseAssessment(
      necessity = ThreatSeverity.Low,
      onlyDefense = None,
      alternatives = Nil,
      counterIsBetter = false,
      prophylaxisNeeded = false,
      resourceCoverageScore = 100
    ),
    threatSeverity = ThreatSeverity.Low,
    immediateThreat = false,
    strategicThreat = false,
    threatIgnorable = true,
    defenseRequired = false,
    counterThreatBetter = false,
    prophylaxisNeeded = false,
    resourceAvailable = true,
    maxLossIfIgnored = 0,
    maxWinPercentLossIfIgnored = None,
    primaryDriver = ThreatDriver.NoThreat,
    insufficientData = false
  )
