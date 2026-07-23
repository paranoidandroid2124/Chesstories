package lila.chessjudgment.analysis.singlePosition

import chess.Color
import lila.chessjudgment.analysis.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.analysis.position.PositionFeatures

/**
 * Single Position Assessor
 * 
 * Classifies a position into 8 fundamental categories using only:
 * - Board feature inputs (PositionFeatures)
 * - MultiPV evaluation data
 * - Current white POV eval
 * 
 * No dependency on tactical motif inputs or downstream analysis.
 */
object SinglePositionAssessor:

  def classify(
    features: PositionFeatures,
    multiPv: List[PvLine],
    currentWhitePovEvalCp: Int,
    sideToMove: Color,
    tacticalMotifsCount: Int = 0  // Optional: from tactical motif inputs if available
  ): SinglePositionAssessment =
    val nature = classifyNature(features)
    val criticality = classifyCriticality(multiPv)
    val candidateSet = assessCandidateSet(multiPv)
    val gamePhase = classifyGamePhase(features)
    val simplifyBias = classifySimplifyBias(features, currentWhitePovEvalCp, sideToMove)
    val drawBias = classifyDrawBias(features)
    val riskProfile = classifyRiskProfile(features, tacticalMotifsCount)
    val judgmentFocus = deriveJudgmentFocus(nature, criticality, simplifyBias, riskProfile)

    SinglePositionAssessment(
      nature = nature,
      criticality = criticality,
      candidateSet = candidateSet,
      gamePhase = gamePhase,
      simplifyBias = simplifyBias,
      drawBias = drawBias,
      riskProfile = riskProfile,
      judgmentFocus = judgmentFocus
    )
  // 1. NATURE CLASSIFICATION

  private def classifyNature(features: PositionFeatures): NatureResult =
    val central = features.centralSpace
    val line = features.lineControl
    val activity = features.activity

    val tensionScore = central.pawnTensionCount
    val openFilesCount = line.openFilesCount
    val mobilityDiff = activity.whitePseudoMobility - activity.blackPseudoMobility
    val lockedCenter = central.lockedCenter

    // Decision logic
    // Locked center strongly favors Static, even with some tension
    val natureType =
      if lockedCenter && tensionScore <= 3 && openFilesCount <= 2 then
        NatureType.Static
      else if tensionScore >= 4 || openFilesCount >= 4 then
        NatureType.Chaos
      else if tensionScore >= 2 || openFilesCount >= 2 then
        NatureType.Dynamic
      else
        NatureType.Static

    NatureResult(
      natureType = natureType,
      tensionScore = tensionScore,
      openFilesCount = openFilesCount,
      mobilityDiff = mobilityDiff,
      lockedCenter = lockedCenter
    )
  // 2. CRITICALITY CLASSIFICATION

  private def classifyCriticality(multiPv: List[PvLine]): CriticalityResult =
    val bestLine = multiPv.headOption
    val secondLine = multiPv.lift(1)

    val mateDistance = bestLine.flatMap(_.mate)
    val evalDelta = for
      best <- bestLine
      second <- secondLine
      if best.mate.isEmpty && second.mate.isEmpty
    yield best.sideRelativeEvalCp - second.sideRelativeEvalCp
    val evalDeltaWinPercent = for
      best <- bestLine
      second <- secondLine
    yield best.winPercentLossTo(second)
    // PV length alone is not a valid proxy for forcing sequences
    val hasMateInLine = mateDistance.isDefined
    val hasLargeEvalJump = evalDeltaWinPercent.exists(_ >= JudgmentThresholds.FORCING_CANDIDATE_GAP_WP)
    val forcingMovesInPv = if hasMateInLine then 5 else if hasLargeEvalJump then 3 else 0

    // Decision logic
    val criticalityType =
      if mateDistance.exists(_.abs <= 5) then
        CriticalityType.ForcedSequence
      else if evalDeltaWinPercent.exists(_ >= JudgmentThresholds.CRITICAL_CANDIDATE_GAP_WP) then
        CriticalityType.CriticalMoment
      else if evalDeltaWinPercent.isEmpty then
        CriticalityType.InsufficientData
      else
        CriticalityType.Normal

    CriticalityResult(
      criticalityType = criticalityType,
      rawSideRelativeEvalDeltaCpForDiagnostics = evalDelta,
      evalDeltaWinPercent = evalDeltaWinPercent,
      mateDistance = mateDistance,
      forcingMovesInPv = forcingMovesInPv
    )
  private def assessCandidateSet(multiPv: List[PvLine]): CandidateSetTopology =
    if multiPv.size < 2 then
      return CandidateSetTopology(
        candidateSetType = CandidateSetType.NarrowChoice, // Unknown, not OnlyMove
        bestLineSideRelativeEvalCp = multiPv.headOption.map(_.sideRelativeEvalCp),
        secondLineSideRelativeEvalCp = None,
        thirdLineSideRelativeEvalCp = None,
        gapBestToSecondWp = None,
        spreadTop3Wp = None,
        secondCandidateFailure = Some(CandidateFailureMode.InsufficientData)
      )

    val bestLine = multiPv.head
    val secondLine = multiPv(1)
    val thirdLine = multiPv.lift(2)
    val bestLineSideRelativeEvalCp = bestLine.sideRelativeEvalCp
    val secondLineSideRelativeEvalCp = secondLine.sideRelativeEvalCp
    val thirdLineSideRelativeEvalCp = thirdLine.map(_.sideRelativeEvalCp)

    val bestLineWinPercent = bestLine.winPercentForSideToMove
    val secondLineWinPercent = secondLine.winPercentForSideToMove
    val thirdLineWinPercent = thirdLine.map(_.winPercentForSideToMove)

    val gapBestToSecondWp = (bestLineWinPercent - secondLineWinPercent).abs
    val spreadTop3Wp = thirdLineWinPercent.map(third => (bestLineWinPercent - third).abs).getOrElse(gapBestToSecondWp)
    val topCandidates = multiPv.take(3)
    val reliableCandidateSet =
      topCandidates.forall(line => JudgmentThresholds.engineBackedByDepth(line.depth, line.mate))
    val onlyMove =
      topCandidates.size >= 3 && reliableCandidateSet && gapBestToSecondWp >= JudgmentThresholds.ONLY_MOVE_GAP_WP

    // Determine why the second candidate fails if the best line is mandatory.
    val secondCandidateFailure: Option[CandidateFailureMode] =
      if onlyMove then
        Some(categorizeCandidateFailure(gapBestToSecondWp, secondLineWinPercent))
      else None

    // Decision logic with WinPercent thresholds
    val candidateSetType =
      if onlyMove then
        CandidateSetType.OnlyMove
      else if reliableCandidateSet && spreadTop3Wp <= JudgmentThresholds.STYLE_CHOICE_SPREAD_WP then
        CandidateSetType.StyleChoice
      else
        CandidateSetType.NarrowChoice

    CandidateSetTopology(
      candidateSetType = candidateSetType,
      bestLineSideRelativeEvalCp = Some(bestLineSideRelativeEvalCp),
      secondLineSideRelativeEvalCp = Some(secondLineSideRelativeEvalCp),
      thirdLineSideRelativeEvalCp = thirdLineSideRelativeEvalCp,
      gapBestToSecondWp = Some(gapBestToSecondWp),
      spreadTop3Wp = Some(spreadTop3Wp),
      secondCandidateFailure = secondCandidateFailure
    )

  private def categorizeCandidateFailure(gapWp: Double, secondCandidateWinPercent: Double): CandidateFailureMode =
    if gapWp > JudgmentThresholds.DECISIVE_CANDIDATE_FAILURE_WP then CandidateFailureMode.DecisiveEvaluationLoss
    else if gapWp > JudgmentThresholds.MATERIAL_CANDIDATE_FAILURE_WP then CandidateFailureMode.LosesMaterial
    else if secondCandidateWinPercent < JudgmentThresholds.POSITION_COLLAPSE_WIN_PERCENT then CandidateFailureMode.PositionCollapses
    else CandidateFailureMode.SignificantDisadvantage

  // 4. GAME PHASE CLASSIFICATION

  private def classifyGamePhase(features: PositionFeatures): GamePhaseResult =
    val mat = features.materialPhase
    val imb = features.imbalance

    val totalMaterial = mat.whiteMaterial + mat.blackMaterial
    val queensOnBoard = imb.whiteQueens > 0 || imb.blackQueens > 0
    val minorPiecesCount = imb.whiteKnights + imb.blackKnights + imb.whiteBishops + imb.blackBishops

    // Use existing phase from board feature extraction, but enrich with evidence
    val phaseType = mat.phase match
      case "opening" => GamePhaseType.Opening
      case "endgame" => GamePhaseType.Endgame
      case _ => GamePhaseType.Middlegame

    GamePhaseResult(
      phaseType = phaseType,
      totalMaterial = totalMaterial,
      queensOnBoard = queensOnBoard,
      minorPiecesCount = minorPiecesCount
    )
  // 5. SIMPLIFY BIAS CLASSIFICATION

  private def classifySimplifyBias(features: PositionFeatures, currentWhitePovEvalCp: Int, sideToMove: Color): SimplifyBiasResult =
    val isEndgameNear = features.materialPhase.phase == "endgame" ||
                        features.materialPhase.whiteMaterial + features.materialPhase.blackMaterial <= 50
    // Positive eval = White advantage, sideToMove determines who benefits
    // For simplification window, the WINNING side should want to simplify
    val evalFromSideToMove = if sideToMove.white then currentWhitePovEvalCp else -currentWhitePovEvalCp
    val evalAdvantageWinPercent = PerspectiveMath.winPercentAdvantageFor(sideToMove, currentWhitePovEvalCp)
    val isWinning = evalAdvantageWinPercent >= JudgmentThresholds.CONVERSION_EDGE_WP
    val rawEvalAdvantageCp = evalFromSideToMove.max(0)
    
    // Check if major piece exchanges are "natural" (rooks/queens on same file, etc.)
    val exchangeAvailable = features.imbalance.whiteQueens > 0 && features.imbalance.blackQueens > 0

    // Decision: Simplification window ONLY if side-to-move is winning AND near endgame
    val isSimplificationWindow =
      isWinning && (isEndgameNear || evalAdvantageWinPercent >= JudgmentThresholds.DECISIVE_EDGE_WP)

    SimplifyBiasResult(
      isSimplificationWindow = isSimplificationWindow,
      rawEvalAdvantageCpForDiagnostics = rawEvalAdvantageCp,
      evalAdvantageWinPercent = evalAdvantageWinPercent,
      isEndgameNear = isEndgameNear,
      exchangeAvailable = exchangeAvailable
    )
  // 6. DRAW BIAS CLASSIFICATION

  private def classifyDrawBias(features: PositionFeatures): DrawBiasResult =
    val mat = features.materialPhase
    val imb = features.imbalance

    val materialSymmetry = mat.materialDiff.abs <= 1
    // K+N vs K, K+B vs K, K vs K are all theoretical draws
    val totalPawns = features.pawns.whitePawnCount + features.pawns.blackPawnCount
    val whiteMinors = imb.whiteKnights + imb.whiteBishops
    val blackMinors = imb.blackKnights + imb.blackBishops
    val hasMajorPieces = imb.whiteRooks > 0 || imb.blackRooks > 0 || imb.whiteQueens > 0 || imb.blackQueens > 0
    
    // Insufficient material: no pawns, no major pieces, and at most 1 minor piece total
    val insufficientMaterial = totalPawns == 0 && !hasMajorPieces && (whiteMinors + blackMinors) <= 1
    // OCB endgames are drawish even WITH rooks (R+B vs R+B OCB is very drawish)
    val oppositeColorBishops = imb.whiteBishops == 1 && imb.blackBishops == 1 &&
                               imb.whiteQueens == 0 && imb.blackQueens == 0

    // Fortress: blocked center with symmetric pawns
    val fortressLikely = features.centralSpace.lockedCenter && materialSymmetry
    val isDrawish = insufficientMaterial || 
                    (materialSymmetry && (oppositeColorBishops || fortressLikely))

    DrawBiasResult(
      isDrawish = isDrawish,
      materialSymmetry = materialSymmetry,
      oppositeColorBishops = oppositeColorBishops,
      fortressLikely = fortressLikely,
      insufficientMaterial = insufficientMaterial
    )
  // 7. RISK PROFILE CLASSIFICATION

  private def classifyRiskProfile(features: PositionFeatures, tacticalMotifsCount: Int): RiskProfileResult =
    val ks = features.kingSafety
    val kingExposureSum = ks.whiteKingExposedFiles + ks.blackKingExposedFiles +
                          ks.whiteKingRingAttacked + ks.blackKingRingAttacked

    // Eval volatility is hard to measure without multi-depth data; use proxy
    val evalVolatility = tacticalMotifsCount * 20 // Rough proxy

    // Decision logic
    val riskLevel =
      if tacticalMotifsCount >= 4 || kingExposureSum >= 6 then
        RiskLevel.High
      else if tacticalMotifsCount >= 2 || kingExposureSum >= 3 then
        RiskLevel.Medium
      else
        RiskLevel.Low

    RiskProfileResult(
      riskLevel = riskLevel,
      evalVolatility = evalVolatility,
      tacticalMotifsCount = tacticalMotifsCount,
      kingExposureSum = kingExposureSum
    )
  // 8. JUDGMENT FOCUS DERIVATION

  private def deriveJudgmentFocus(
    nature: NatureResult,
    criticality: CriticalityResult,
    simplifyBias: SimplifyBiasResult,
    riskProfile: RiskProfileResult
  ): JudgmentFocusResult =
    val (focus, driver) =
      if criticality.isForced then
        (JudgmentFocusType.Tactics, JudgmentDriver.ForcedSequence)
      else if simplifyBias.isSimplificationWindow then
        (JudgmentFocusType.Conversion, JudgmentDriver.ConversionOpportunity)
      else if riskProfile.isHighRisk then
        (JudgmentFocusType.Tactics, JudgmentDriver.TacticalComplexity)
      else if nature.isChaos then
        (JudgmentFocusType.Tactics, JudgmentDriver.ChaoticPosition)
      else if nature.isStatic then
        (JudgmentFocusType.Plan, JudgmentDriver.StrategicStructure)
      else
        (JudgmentFocusType.Plan, JudgmentDriver.DynamicPosition)

    JudgmentFocusResult(
      focus = focus,
      primaryDriver = driver
    )
  // EXTENSION: Int to Boolean conversion
  extension (i: Int)
    def toBoolean: Boolean = i > 0
