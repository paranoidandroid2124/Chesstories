package lila.chessjudgment.analysis.structure

import chess.Color
import lila.chessjudgment.analysis.singlePosition.PawnPlayAnalysis
import lila.chessjudgment.model.{ Motif, PlanMatch }
import lila.chessjudgment.model.structure.{
  AlignmentBand,
  CenterState,
  PlanAlignment,
  StructureId,
  StructurePrecondition,
  StructureProfile,
  StructuralPlaybookEntry
}

object PlanAlignmentScorer:

  private val PlanMatchWeight = 55.0
  private val PreconditionWeight = 25.0
  private val AntiPlanPenalty = 20
  private val ConfidenceFloor = 0.72
  private val PrimaryPlanConfidenceFloor = 0.55

  def score(
      structureProfile: StructureProfile,
      playbookEntry: StructuralPlaybookEntry,
      topPlans: List[PlanMatch],
      motifs: List[Motif],
      pawnAnalysis: Option[PawnPlayAnalysis],
      sideToMove: Color
  ): PlanAlignment =
    if structureProfile.primary == StructureId.Unknown then return unknown(topPlans)

    val expected = StructuralPlaybook.expectedPlans(playbookEntry, sideToMove)
    val observed = topPlans.take(3).map(_.plan.id).distinct
    val matched = expected.filter(observed.contains)
    val missing = expected.filterNot(matched.contains)

    val planScore =
      if expected.isEmpty then 0
      else if observed.headOption.exists(expected.contains) then PlanMatchWeight.toInt
      else math.round(PlanMatchWeight * matched.size.toDouble / expected.size.toDouble).toInt

    val preconds = playbookEntry.preconditions
    val precondHits = preconds.count(p => isPreconditionSatisfied(p, structureProfile, pawnAnalysis, motifs))
    val precondScore =
      if preconds.isEmpty then PreconditionWeight.toInt
      else math.round(PreconditionWeight * precondHits.toDouble / preconds.size.toDouble).toInt

    val primaryObserved = observed.headOption
    val topPlanTrusted = topPlans.headOption.exists(_.score >= PrimaryPlanConfidenceFloor)
    val antiPlanByCounter = topPlanTrusted && primaryObserved.exists(id => playbookEntry.counterPlans.contains(id))
    val antiPlanByMiss = topPlanTrusted && primaryObserved.exists(id => !expected.contains(id) && matched.isEmpty)
    val antiPlan = antiPlanByCounter || antiPlanByMiss
    val antiPenalty = if antiPlan then AntiPlanPenalty else 0

    val raw = (planScore + precondScore - antiPenalty).max(0).min(100)
    val lowConf = structureProfile.confidence < ConfidenceFloor
    val band =
      if lowConf then AlignmentBand.Unknown
      else if raw >= 70 then AlignmentBand.OnBook
      else if raw >= 45 then AlignmentBand.Playable
      else AlignmentBand.OffPlan

    val reasons =
      (List.newBuilder[String]
        .addAll(if matched.nonEmpty then List("PA_MATCH") else Nil)
        .addAll(if preconds.nonEmpty && precondHits < preconds.size then List("PRECOND_MISS") else Nil)
        .addAll(if antiPlan then List("ANTI_PLAN") else Nil)
        .addAll(if lowConf then List("LOW_CONF") else Nil)
        .result()).distinct

    val reasonWeights = Map(
      "PA_MATCH" -> (if matched.nonEmpty then planScore.toDouble / 100.0 else 0.0),
      "PRECOND_MISS" -> (if preconds.nonEmpty && precondHits < preconds.size then (preconds.size - precondHits).toDouble / preconds.size.toDouble else 0.0),
      "ANTI_PLAN" -> (if antiPlan then antiPenalty.toDouble / 100.0 else 0.0),
      "LOW_CONF" -> (if lowConf then (ConfidenceFloor - structureProfile.confidence).max(0.0) else 0.0)
    ).filter(_._2 > 0.0)

    PlanAlignment(
      score = raw,
      band = band,
      matchedPlanIds = matched.map(_.toString),
      missingPlanIds = missing.map(_.toString),
      reasonCodes = reasons,
      reasonWeights = reasonWeights
    )

  def unknown(topPlans: List[PlanMatch]): PlanAlignment =
    PlanAlignment(
      score = 0,
      band = AlignmentBand.Unknown,
      matchedPlanIds = Nil,
      missingPlanIds = Nil,
      reasonCodes = List("LOW_CONF") ++ topPlans.headOption.map(p => s"TOP_${p.plan.id.toString}").toList,
      reasonWeights = Map("LOW_CONF" -> 1.0)
    )

  private def isPreconditionSatisfied(
      precondition: StructurePrecondition,
      structureProfile: StructureProfile,
      pawnAnalysis: Option[PawnPlayAnalysis],
      motifs: List[Motif]
  ): Boolean =
    import StructurePrecondition.*
    precondition match
      case TensionOrBreak =>
        pawnAnalysis.exists(p => p.pawnBreakReady || p.advanceOrCapture || p.counterBreak)
      case MinorityReady =>
        pawnAnalysis.exists(_.minorityAttack)
      case PieceActivity =>
        motifs.exists {
          case _: Motif.Outpost | _: Motif.Centralization | _: Motif.Maneuver => true
          case _ => false
        }
      case LockedCenter =>
        structureProfile.centerState == CenterState.Locked
      case OpenCenter =>
        structureProfile.centerState == CenterState.Open
      case SolidCenter =>
        structureProfile.centerState == CenterState.Locked || structureProfile.centerState == CenterState.Symmetric
      case CounterBreakWatch =>
        pawnAnalysis.exists(_.counterBreak)
      case KingFlankFocus =>
        val flankPawnPush = pawnAnalysis.exists(_.breakFile.exists(isOuterFlankFile)) ||
          motifs.exists {
            case m: Motif.PawnAdvance => isOuterFlankFile(m.file.char)
            case _ => false
          }
        val flankPieceActivity = motifs.exists {
          case m: Motif.Outpost => isOuterFlankFile(m.square.file.char)
          case m: Motif.Maneuver => maneuverTouchesOuterFlank(m.move)
          case _ => false
        }
        flankPawnPush && flankPieceActivity

  private def maneuverTouchesOuterFlank(move: Option[String]): Boolean =
    move.exists { raw =>
      val normalized = Option(raw).getOrElse("").trim.toLowerCase
      val originFile = normalized.lift(0)
      val targetFile = normalized.lift(2)
      (originFile.toList ++ targetFile.toList).exists(isOuterFlankFile)
    }

  private def isOuterFlankFile(raw: String): Boolean =
    raw.length == 1 && isOuterFlankFile(raw.head)

  private def isOuterFlankFile(file: Char): Boolean =
    file.toLower == 'a' || file.toLower == 'b' || file.toLower == 'g' || file.toLower == 'h'
