package lila.chessjudgment.analysis.plan

import chess.*
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.judgment.{
  PawnPlayAnalysis,
  ThreatKind,
  ThreatSeverity,
  TensionPolicy
}
import lila.chessjudgment.model.*
import lila.chessjudgment.model.judgment.ThreatEpisode
import lila.chessjudgment.model.position.PositionFeatures
import lila.chessjudgment.model.Motif.*
import lila.chessjudgment.model.structure.{ StructureId, StructureProfile }
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanSignal, PlanTheme, SubplanCatalog }

case class PlanInteractionContext(
    whitePovEvalCp: Int,
    positionFeatures: Option[PositionFeatures] = None,
    candidateLineAvailable: Boolean = false,
    pawnAnalysis: Option[PawnPlayAnalysis] = None,
    threatEpisodesToUs: List[ThreatEpisode] = Nil,
    threatEpisodesToThem: List[ThreatEpisode] = Nil,
    initialPos: Option[Position] = None,
    rootMove: Option[String] = None,
    structureProfile: Option[StructureProfile] = None
):
  def winPercentFor(color: Color): Double =
    PerspectiveMath.winPercentForMover(color, whitePovEvalCp)
  def winPercentAdvantageFor(color: Color): Double =
    PerspectiveMath.winPercentAdvantageFor(color, whitePovEvalCp)
  def phase: String = phaseEnumOpt match
    case Some(value) => value
    case None        => "unclassified"
  def phaseEnumOpt: Option[String] =
    positionFeatures.map(_.materialPhase.phase)
  def simplificationWindowFor(side: Color): Boolean =
    positionFeatures.exists { features =>
      val material = features.materialPhase
      val endgameNear =
        material.phase == "endgame" ||
          material.whiteMaterial + material.blackMaterial <= 50
      endgameNear && winPercentAdvantageFor(side) >= JudgmentThresholds.CONVERSION_EDGE_WP
    }
  private def materialThreat(episode: ThreatEpisode): Boolean =
    episode.kind == ThreatKind.Mate || episode.kind == ThreatKind.Material
  private def significantThreat(episode: ThreatEpisode): Boolean =
    episode.severity != ThreatSeverity.Low
  def tacticalThreatToUs: Boolean =
    threatEpisodesToUs.exists(t => t.turnsToImpact <= 2 && materialThreat(t))
  def strategicThreatToUs: Boolean =
    !tacticalThreatToUs &&
      threatEpisodesToUs.exists(t => t.strategic && t.turnsToImpact <= 5 && significantThreat(t))
  def tacticalThreatToThem: Boolean =
    threatEpisodesToThem.exists(t => t.turnsToImpact <= 2 && materialThreat(t))
  def strategicThreatToThem: Boolean =
    !tacticalThreatToThem &&
      threatEpisodesToThem.exists(t => t.strategic && t.turnsToImpact <= 5 && significantThreat(t))
  def underDefensivePressure: Boolean = strategicThreatToUs

object PlanMatcher:
  private case class SideSnapshot(
      lockedCenter: Boolean,
      openCenter: Boolean,
      space: Int,
      devLag: Int,
      lowMobility: Int,
      kingExposure: Int,
      oppWeakness: Int,
      ourPassers: Int,
      oppPassers: Int,
      entrenched: Int,
      rookPawnReady: Boolean,
      hookChance: Boolean,
      clamp: Boolean
  )

  def matchPlans(motifs: List[Motif], ctx: PlanInteractionContext, side: Color): PlanScoringResult =
    (for
      features <- ctx.positionFeatures
      phase <- ctx.phaseEnumOpt
    yield
      val s = snapshot(features, side)
      val openingRaw =
        Option.when(phase == "opening")(
          openingDevelopment(motifs, side, features, s)
        ).toList
      val raw =
        openingRaw ++ List(
          restriction(motifs, ctx, side, s),
          redeployment(motifs, ctx, side, s),
          spaceClamp(motifs, ctx, side, s),
          weaknessFixation(motifs, ctx, side, s),
          breakPrep(motifs, ctx, side, features),
          favorableExchange(motifs, ctx, side),
          wingPlay(motifs, ctx, side, s),
          advantageTransformation(motifs, ctx, side, s)
      )
      val (compatible, events) = applyCompatWithEvents(raw, ctx, side)
      val themePolicyScores = computePlanThemePolicyScores(compatible)
      val availableSignals = availablePlanSignals(ctx, motifs)
      val signalGated =
        compatible
          .map(pm => applySignalGate(pm, availableSignals))
          .filter(_.missingSignals.isEmpty)
      val annotated = signalGated.map(pm => annotateWithPlanThemeScore(pm, themePolicyScores.getOrElse(themeOf(pm), 0.0)))
      val top = annotated.sortBy(p => -p.score).filter(_.score >= 0.18)
      PlanScoringResult(top, ctx.phase, events)
    ).getOrElse(PlanScoringResult(Nil, ctx.phase, Nil))

  def toActivePlans(sortedPlans: List[PlanMatch], events: List[CompatibilityEvent] = Nil): Option[ActivePlans] =
    Option.when(sortedPlans.nonEmpty)(ActivePlans(sortedPlans, events))

  def applyCompatWithEvents(
      plans: List[PlanMatch],
      ctx: PlanInteractionContext,
      side: Color
  ): (List[PlanMatch], List[CompatibilityEvent]) =
    import scala.collection.mutable.ListBuffer
    val events = ListBuffer.empty[CompatibilityEvent]

    def adjust(
        list: List[PlanMatch],
        t: PlanTheme,
        factor: Double,
        adjustment: CompatibilityAdjustment
    ): List[PlanMatch] =
      list.map { p =>
        if p.plan.theme != t then p
        else
          val before = p.score
          val after = clamp(before * factor)
          if math.abs(after - before) > 1e-6 then
            events += CompatibilityEvent(
              adjustment = adjustment
            )
          p.copy(score = after)
      }

    var out = plans
    if ctx.underDefensivePressure then
      out = adjust(out, PlanTheme.RestrictionProphylaxis, 1.15, CompatibilityAdjustment.DefensivePressure)
      out = adjust(out, PlanTheme.WingPlay, 0.80, CompatibilityAdjustment.DefensivePressure)
      out = adjust(out, PlanTheme.PawnBreakPreparation, 0.82, CompatibilityAdjustment.DefensivePressure)
    if ctx.simplificationWindowFor(side)
    then
      out = adjust(out, PlanTheme.FavorableExchange, 1.15, CompatibilityAdjustment.ConversionWindow)
      out = adjust(out, PlanTheme.AdvantageTransformation, 1.12, CompatibilityAdjustment.ConversionWindow)
    if ctx.positionFeatures.exists(_.centralSpace.openCenter) && kingExposure(ctx.positionFeatures, side) >= 2 then
      out = adjust(out, PlanTheme.WingPlay, 0.72, CompatibilityAdjustment.OpenCenterFlankRisk)
    if ctx.phaseEnumOpt.contains("opening") then
      out = adjust(out, PlanTheme.WingPlay, 0.78, CompatibilityAdjustment.OpeningPhase)
      out = adjust(out, PlanTheme.AdvantageTransformation, 0.78, CompatibilityAdjustment.OpeningPhase)
    (out, events.toList)

  private def openingDevelopment(
      m: List[Motif],
      side: Color,
      features: PositionFeatures,
      s: SideSnapshot
  ): PlanMatch =
    val ev = evidence(m, 0.18) {
      case Centralization(piece, _, c, _, _) if c == side && (piece == Knight || piece == Bishop) => true
      case Castling(_, c, _, _) if c == side => true
      case Fianchetto(_, c, _, _) if c == side => true
      case Maneuver(_, ManeuverPurpose.ImprovingScope, c, _, _) if c == side => true
    }
    val centerControlDiff =
      if side.white then features.centralSpace.whiteCenterControl - features.centralSpace.blackCenterControl
      else features.centralSpace.blackCenterControl - features.centralSpace.whiteCenterControl
    val ourCenterPawns =
      if side.white then features.centralSpace.whiteCentralPawns else features.centralSpace.blackCentralPawns
    val hasCastled = m.exists { case Castling(_, c, _, _) if c == side => true; case _ => false }
    val developmentNeed = s.devLag.max(0)
    val score =
      0.26 +
        math.min(0.12, developmentNeed * 0.04) +
        math.min(0.08, centerControlDiff.max(0) * 0.015) +
        math.min(0.06, ourCenterPawns.max(0) * 0.03) +
        (if hasCastled then 0.05 else 0.0) +
        math.min(0.18, ev.size * 0.05) -
        (if developmentNeed == 0 && !hasCastled && ev.isEmpty then 0.08 else 0.0)
    matched(PlanKind.OpeningDevelopment, side, score, ev)

  private def restriction(m: List[Motif], ctx: PlanInteractionContext, side: Color, s: SideSnapshot): PlanMatch =
    val ev = evidence(m, 0.18) {
      case Domination(_, _, _, c, _, _) if c == side => true
      case Blockade(_, _, _, c, _, _) if c == side => true
      case OpenFileControl(_, c, _, _) if c == side => true
      case PawnAdvance(file, _, _, c, _, _) if c == side && isFlank(file) &&
          (ctx.underDefensivePressure || ctx.pawnAnalysis.exists(_.counterBreak) || opposedFlankAdvance(ctx, side, file)) => true
    }
    val score =
      0.28 +
        (if ctx.strategicThreatToUs then 0.12 else 0.0) +
        (if s.clamp then 0.14 else 0.0) +
        (if s.lockedCenter then 0.06 else 0.0) +
        math.min(0.16, ev.size * 0.05) -
        (if ctx.strategicThreatToThem && !ctx.strategicThreatToUs then 0.08 else 0.0)
    matched(PlanKind.ProphylaxisRestraint, side, score, ev)

  private def redeployment(m: List[Motif], ctx: PlanInteractionContext, side: Color, s: SideSnapshot): PlanMatch =
    val ev = evidence(m, 0.17) {
      case Outpost(_, _, c, _, _) if c == side => true
      case Centralization(_, _, c, _, _) if c == side => true
      case Maneuver(_, _, c, _, _) if c == side => true
      case RookLift(_, _, _, c, _, _) if c == side => true
      case OpenFileControl(_, c, _, _) if c == side => true
      case SemiOpenFileControl(_, c, _, _) if c == side => true
      case SeventhRankInvasion(c, _, _) if c == side => true
      case RookBehindPassedPawn(_, c, _, _) if c == side => true
    }
    val prefersOutpost = s.entrenched > 0 || m.exists { case Outpost(_, _, c, _, _) if c == side => true; case _ => false }
    val rootMoveIsRook =
      for
        position <- ctx.initialPos
        move <- ctx.rootMove
        origin <- Square.fromKey(move.trim.toLowerCase.take(2))
      yield position.board.roleAt(origin).contains(Rook)
    val prefersRookFileTransfer =
      rootMoveIsRook.contains(true) && m.exists {
        case RookLift(_, _, _, c, _, _) if c == side => true
        case OpenFileControl(_, c, _, _) if c == side => true
        case SemiOpenFileControl(_, c, _, _) if c == side => true
        case SeventhRankInvasion(c, _, _) if c == side => true
        case RookBehindPassedPawn(_, c, _, _) if c == side => true
        case _ => false
      }
    val subplanId =
      if prefersRookFileTransfer then PlanKind.RookFileTransfer
      else if prefersOutpost then PlanKind.OutpostEntrenchment
      else PlanKind.WorstPieceImprovement
    val score =
      0.28 +
        math.min(0.18, s.devLag * 0.05) +
        math.min(0.14, s.lowMobility * 0.04) +
        math.min(0.14, s.entrenched * 0.06) +
        math.min(0.14, ev.size * 0.04) -
        (if ctx.strategicThreatToUs then 0.07 else 0.0)
    matched(subplanId, side, score, ev)

  private def spaceClamp(m: List[Motif], ctx: PlanInteractionContext, side: Color, s: SideSnapshot): PlanMatch =
    val ev = evidence(m, 0.16) {
      case SpaceAdvantage(c, _, _, _) if c == side => true
      case advance: PawnAdvance if advance.color == side && isFlank(advance.file) &&
          advance.relativeTo >= 4 && !opposedFlankAdvance(ctx, side, advance.file) => true
    }
    val score =
      0.24 +
        (if s.space > 0 then math.min(0.20, s.space * 0.05) else -math.min(0.10, s.space.abs * 0.03)) +
        (if s.clamp then 0.16 else 0.0) +
        (if s.lockedCenter then 0.06 else 0.0) +
        math.min(0.14, ev.size * 0.04) -
        (if s.openCenter && s.kingExposure >= 2 then 0.10 else 0.0)
    matched(PlanKind.FlankClamp, side, score, ev)

  private def weaknessFixation(m: List[Motif], ctx: PlanInteractionContext, side: Color, s: SideSnapshot): PlanMatch =
    val opp = !side
    val targetFiles =
      m.collect {
        case IsolatedPawn(file, _, c, _, _) if c == opp => file
        case BackwardPawn(file, _, c, _, _) if c == opp => file
        case DoubledPawns(file, c, _, _) if c == opp => file
      }.distinct
    val targetPawns =
      ctx.initialPos.toList.flatMap(position =>
        (position.board.pawns & position.board.byColor(opp)).squares.filter(square => targetFiles.contains(square.file))
      )
    val rootTargetingManeuver =
      rootManeuverEvidence(ctx, side, 0.17)(destination =>
        targetFiles.exists(target => (target.value - destination.file.value).abs <= 2)
      )
    val ev = (rootTargetingManeuver ++ evidence(m, 0.17) {
      case IsolatedPawn(_, _, c, _, _) if c == opp => true
      case BackwardPawn(_, _, c, _, _) if c == opp => true
      case DoubledPawns(_, c, _, _) if c == opp => true
      case Blockade(_, _, _, c, _, _) if c == side => true
      case advance: PawnAdvance if advance.color == side &&
          targetPawns.exists(target =>
            (target.file.value - advance.file.value).abs <= 1 && {
              val distance =
                if side.white then target.rank.value + 1 - advance.toRank
                else advance.toRank - target.rank.value - 1
              distance >= 1 && distance <= 2
            }
          ) && !opposedFlankAdvance(ctx, side, advance.file) => true
      case Maneuver(_, _, c, _, _) if c == side && targetFiles.nonEmpty => true
      case Centralization(_, _, c, _, _) if c == side && targetFiles.nonEmpty => true
    }).distinctBy(_.motif).take(4)
    val score =
      0.23 +
        math.min(0.24, s.oppWeakness * 0.05) +
        (if s.hookChance then 0.10 else 0.0) +
        math.min(0.16, ev.size * 0.05) -
        (if s.oppWeakness == 0 && ev.isEmpty then 0.05 else 0.0)
    matched(weaknessFixationSubplan(m, ctx, side, s), side, score, ev)

  private def breakPrep(m: List[Motif], ctx: PlanInteractionContext, side: Color, features: PositionFeatures): PlanMatch =
    val ev = evidence(m, 0.18) {
      case PawnBreak(_, _, c, _, _) if c == side => true
      case PawnAdvance(file, _, _, c, _, _) if c == side && centralFile(file) => true
    }
    val pa = ctx.pawnAnalysis
    val breakFile = pa.flatMap(_.breakFile)
    val subplanId =
      if pa.exists(_.tensionPolicy == TensionPolicy.Maintain) then PlanKind.TensionMaintenance
      else if breakFile.exists(isWingBreakFile) then PlanKind.WingBreakTiming
      else PlanKind.CentralBreakTiming
    val score =
      0.26 +
        (if pa.exists(_.pawnBreakReady) then 0.24 else 0.0) +
        (if pa.exists(_.tensionPolicy == TensionPolicy.Maintain) then 0.06 else 0.0) +
        (if pa.exists(_.tensionPolicy == TensionPolicy.Release) then 0.10 else 0.0) +
        math.min(0.12, features.centralSpace.pawnTensionCount * 0.03) +
        math.min(0.16, ev.size * 0.05) -
        (if ctx.strategicThreatToUs then 0.08 else 0.0)
    matched(subplanId, side, score, ev)

  private def favorableExchange(m: List[Motif], ctx: PlanInteractionContext, side: Color): PlanMatch =
    val ev = evidence(m, 0.17) {
      case Capture(_, _, _, t, c, _, _, _) if c == side &&
          (t == CaptureType.Exchange || t == CaptureType.Recapture || t == CaptureType.Winning) => true
      case RemovingTheDefender(_, _, _, _, c, _, _) if c == side => true
    }
    val advantageEdge = ctx.winPercentAdvantageFor(side)
    val opponentAdvantageEdge = ctx.winPercentAdvantageFor(!side)
    val simplifyWindow = ctx.simplificationWindowFor(side)
    val score =
      0.20 +
        (if simplifyWindow then 0.20 else 0.0) +
        (if advantageEdge >= JudgmentThresholds.CONVERSION_EDGE_WP then 0.10
         else if opponentAdvantageEdge >= JudgmentThresholds.CONVERSION_EDGE_WP then -0.08
         else 0.0) +
        math.min(0.15, ev.size * 0.05)
    matched(PlanKind.SimplificationWindow, side, score, ev)

  private def wingPlay(m: List[Motif], ctx: PlanInteractionContext, side: Color, s: SideSnapshot): PlanMatch =
    val existingAdvancedFlankFiles = advancedFlankFiles(ctx, side)
    val advancedPawnFiles =
      m.collect {
        case advance: PawnAdvance if advance.color == side && isFlank(advance.file) && advance.relativeTo >= 4 =>
          advance.file
      }
    val opponentAdvancedFlankFiles = advancedFlankFiles(ctx, !side)
    val opponentKingsidePressure = opponentAdvancedFlankFiles.exists(file => file == File.G || file == File.H)
    val opponentQueensidePressure = opponentAdvancedFlankFiles.exists(file => file == File.A || file == File.B)
    val kingsideAttack =
      existingAdvancedFlankFiles.exists(file => file == File.G || file == File.H) ||
        (!opponentKingsidePressure && advancedPawnFiles.exists(file => file == File.G || file == File.H))
    val queensideAttack =
      existingAdvancedFlankFiles.exists(file => file == File.A || file == File.B) ||
        (!opponentQueensidePressure && advancedPawnFiles.exists(file => file == File.A || file == File.B))
    val ev = evidence(m, 0.19) {
      case advance: PawnAdvance if advance.color == side && isFlank(advance.file) &&
          (advance.relativeTo >= 4 || existingAdvancedFlankFiles.exists(sameFlank(_, advance.file))) &&
          !opposedFlankAdvance(ctx, side, advance.file) => true
      case RookLift(_, _, _, c, _, _) if c == side => true
      case PawnChain(_, _, c, _, _) if c == side => true
      case Outpost(_, square, c, _, _) if c == side &&
          ((kingsideAttack && (square.file == File.G || square.file == File.H)) ||
            (queensideAttack && (square.file == File.A || square.file == File.B))) => true
    }.distinctBy(_.motif).take(4)
    val hasRookLiftSignal = m.exists { case RookLift(_, _, _, c, _, _) if c == side => true; case _ => false }
    val hasHookSignal =
      s.hookChance ||
        m.exists {
          case PawnChain(_, _, c, _, _) if c == side => true
          case PawnAdvance(file, _, _, c, _, _) if c == side && (file == File.B || file == File.G) => true
          case _ => false
        }
    val subplanId =
      if hasRookLiftSignal then PlanKind.RookLiftScaffold
      else if hasHookSignal && (!s.rookPawnReady || ev.size <= 1) then PlanKind.HookCreation
      else if kingsideAttack && !queensideAttack then PlanKind.KingsideWingExpansion
      else if queensideAttack && !kingsideAttack then PlanKind.QueensideWingExpansion
      else PlanKind.WingExpansion
    val score =
      0.16 +
        (if kingsideAttack || queensideAttack then 0.20 else 0.0) +
        (if s.rookPawnReady then 0.12 else 0.0) +
        (if s.hookChance then 0.08 else 0.0) +
        (if existingAdvancedFlankFiles.nonEmpty then 0.08 else 0.0) +
        math.min(0.25, ev.size * 0.10) -
        (if s.openCenter && s.kingExposure >= 2 then 0.12 else 0.0) -
        (if ctx.strategicThreatToUs then 0.08 else 0.0)
    matched(subplanId, side, score, ev)

  private def advantageTransformation(m: List[Motif], ctx: PlanInteractionContext, side: Color, s: SideSnapshot): PlanMatch =
    val ev = evidence(m, 0.17) {
      case PassedPawnPush(_, _, c, _, _) if c == side => true
      case RookBehindPassedPawn(_, c, _, _) if c == side => true
      case SeventhRankInvasion(c, _, _) if c == side => true
    }
    val advantageEdge = ctx.winPercentAdvantageFor(side)
    val score =
      0.22 +
        (if advantageEdge >= JudgmentThresholds.CRITICAL_CANDIDATE_GAP_WP then 0.14 else 0.0) +
        (if s.ourPassers > s.oppPassers then 0.10 else 0.0) +
        (if ctx.simplificationWindowFor(side) then 0.10 else 0.0) +
        math.min(0.16, ev.size * 0.05)
    matched(PlanKind.SimplificationConversion, side, score, ev)

  private def matched(
      kind: PlanKind,
      side: Color,
      score: Double,
      evidence: List[EvidenceAtom]
  ): PlanMatch =
    PlanMatch(
      plan = Plan(kind, side),
      score = clamp(score),
      evidence = evidence.take(4)
    )

  private def themeOf(pm: PlanMatch): PlanTheme =
    pm.plan.theme

  private def availablePlanSignals(ctx: PlanInteractionContext, motifs: List[Motif]): Set[PlanSignal] =
    import PlanSignal.*
    val threatMotifs =
      (ctx.threatEpisodesToUs ++ ctx.threatEpisodesToThem).flatMap(_.motifs)
    Set(
      Option.when((motifs ++ threatMotifs).exists(planMotif))(MoveMotifSignal),
      Option.when(ctx.positionFeatures.nonEmpty)(StrategicSnapshotSignal),
      Option.when(ctx.candidateLineAvailable)(CandidateLineSignal),
      Option.when(ctx.structureProfile.nonEmpty || ctx.pawnAnalysis.nonEmpty)(StructuralSignal)
    ).flatten

  private def applySignalGate(pm: PlanMatch, availableSignals: Set[PlanSignal]): PlanMatch =
    val missing =
      SubplanCatalog.specs
        .get(pm.plan.kind)
        .map(_.requiredSignals.filterNot(availableSignals.contains))
        .getOrElse(Nil)
    pm.copy(missingSignals = missing)

  private def computePlanThemePolicyScores(plans: List[PlanMatch]): Map[PlanTheme, Double] =
    val nonNegative = plans.map(p => p -> p.score.max(0.0))
    val total = nonNegative.map(_._2).sum
    if total <= 1e-9 then Map.empty
    else
      nonNegative
        .groupBy((p, _) => themeOf(p))
        .view
        .mapValues(v => v.map(_._2).sum / total)
        .toMap

  private def annotateWithPlanThemeScore(pm: PlanMatch, themePolicyScore: Double): PlanMatch =
    pm.copy(themePolicyScore = Some(themePolicyScore.max(0.0).min(1.0)))

  private def evidence(
      motifs: List[Motif],
      weight: Double
  )(pf: PartialFunction[Motif, Boolean]): List[EvidenceAtom] =
    motifs.collect { case m if planMotif(m) && pf.isDefinedAt(m) && pf(m) => EvidenceAtom(m, weight) }.take(4)

  private def planMotif(motif: Motif): Boolean =
    motif.category != MotifCategory.Tactical

  private def snapshot(features: PositionFeatures, side: Color): SideSnapshot =
    val opponent = !side
    val strategic = features.strategicState
    SideSnapshot(
      lockedCenter = features.centralSpace.lockedCenter,
      openCenter = features.centralSpace.openCenter,
      space = if side.white then features.centralSpace.spaceDiff else -features.centralSpace.spaceDiff,
      devLag =
        if side.white then features.activity.whiteDevelopmentLag
        else features.activity.blackDevelopmentLag,
      lowMobility =
        if side.white then features.activity.whiteLowMobilityPieces
        else features.activity.blackLowMobilityPieces,
      kingExposure =
        if side.white then features.kingSafety.whiteKingExposedFiles
        else features.kingSafety.blackKingExposedFiles,
      oppWeakness =
        pawnWeaknesses(features, opponent),
      ourPassers =
        if side.white then features.pawns.whitePassedPawns else features.pawns.blackPassedPawns,
      oppPassers =
        if side.white then features.pawns.blackPassedPawns else features.pawns.whitePassedPawns,
      entrenched =
        if side.white then strategic.whiteEntrenchedPieces else strategic.blackEntrenchedPieces,
      rookPawnReady =
        if side.white then strategic.whiteRookPawnMarchReady else strategic.blackRookPawnMarchReady,
      hookChance =
        if side.white then strategic.whiteHookCreationChance else strategic.blackHookCreationChance,
      clamp =
        if side.white then strategic.whiteColorComplexClamp else strategic.blackColorComplexClamp
    )

  private def pawnWeaknesses(features: PositionFeatures, side: Color): Int =
    if side.white then
      features.pawns.whiteIsolatedPawns +
        features.pawns.whiteBackwardPawns +
        features.pawns.whiteDoubledPawns
    else
      features.pawns.blackIsolatedPawns +
        features.pawns.blackBackwardPawns +
        features.pawns.blackDoubledPawns

  private def weaknessFixationSubplan(
      m: List[Motif],
      ctx: PlanInteractionContext,
      side: Color,
      s: SideSnapshot
  ): PlanKind =
    val opp = !side
    val backwardPawnTarget = m.exists { case BackwardPawn(_, _, c, _, _) if c == opp => true; case _ => false }
    val isolatedPawnTarget = m.exists { case IsolatedPawn(_, _, c, _, _) if c == opp => true; case _ => false }
    val minorityAttackStructure = structureMatches(ctx, StructureId.Carlsbad)
    val iqpTargetStructure = if side.white then structureMatches(ctx, StructureId.IQPBlack) else structureMatches(ctx, StructureId.IQPWhite)
    if minorityAttackStructure && (s.hookChance || s.oppWeakness > 0) then PlanKind.MinorityAttackFixation
    else if backwardPawnTarget then PlanKind.BackwardPawnTargeting
    else if iqpTargetStructure && isolatedPawnTarget then PlanKind.IQPInducement
    else PlanKind.StaticWeaknessFixation

  private def structureMatches(ctx: PlanInteractionContext, id: StructureId): Boolean =
    ctx.structureProfile.exists(_.primary == id)

  private def advancedFlankFiles(ctx: PlanInteractionContext, side: Color): List[File] =
    ctx.initialPos.toList.flatMap(position =>
      (position.board.pawns & position.board.byColor(side)).squares.collect {
        case square if isFlank(square.file) && Motif.relativeRank(square.rank.value + 1, side) >= 4 => square.file
      }
    )

  private def rootManeuverEvidence(
      ctx: PlanInteractionContext,
      side: Color,
      weight: Double
  )(supports: Square => Boolean): List[EvidenceAtom] =
    (for
      position <- ctx.initialPos
      move <- ctx.rootMove.map(_.trim.toLowerCase)
      origin <- Square.fromKey(move.take(2))
      destination <- Square.fromKey(move.slice(2, 4))
      role <- position.board.roleAt(origin)
      if role != Pawn && role != King
      if position.board.roleAt(destination).isEmpty && supports(destination)
    yield EvidenceAtom(Maneuver(role, ManeuverPurpose.ImprovingScope, side, 0, Some(move)), weight)).toList

  private def opposedFlankAdvance(ctx: PlanInteractionContext, side: Color, file: File): Boolean =
    advancedFlankFiles(ctx, !side).exists(sameFlank(_, file))

  private def sameFlank(left: File, right: File): Boolean =
    left.value <= File.B.value && right.value <= File.B.value ||
      left.value >= File.G.value && right.value >= File.G.value

  private def kingExposure(features: Option[PositionFeatures], side: Color): Int =
    features
      .map(position =>
        if side.white then position.kingSafety.whiteKingExposedFiles
        else position.kingSafety.blackKingExposedFiles
      )
      .getOrElse(0)

  private def centralFile(file: File): Boolean =
    file == File.C || file == File.D || file == File.E || file == File.F

  private def isFlank(file: File): Boolean =
    file == File.A || file == File.B || file == File.G || file == File.H

  private def isWingBreakFile(file: String): Boolean =
    val low = Option(file).getOrElse("").trim.toLowerCase
    low == "a" || low == "b" || low == "g" || low == "h"

  private def clamp(score: Double): Double =
    math.max(0.0, math.min(1.0, score))
