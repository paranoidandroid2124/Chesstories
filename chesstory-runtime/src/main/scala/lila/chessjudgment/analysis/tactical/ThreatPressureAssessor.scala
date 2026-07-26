package lila.chessjudgment.analysis.tactical

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.LegalReplayStep
import lila.chessjudgment.model.position.PositionFeatures
import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.analysis.tactical.TacticalRelationEvidence
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine

/**
 * Threat Analyzer
 *
 * Analyzes threats from the opposing actor and assesses pressure on an
 * explicitly selected side. Defense candidates are attached only when that
 * pressured side is actually to move in the analyzed FEN.
 *
 * Uses board-backed motifs as the threat authority. Engine lines may identify
 * a concrete defense to an already observed mate threat, but never create or
 * rescore a threat from candidate spread.
 *
 * Engine lines retain their canonical White perspective. All side-relative
 * comparisons use the canonical `PerspectiveMath.compareForMover`.
 */
object ThreatPressureAssessor:

  private case class ThreatProfile(
    kind: ThreatKind,
    turnsToImpact: Int
  )

  /**
   * Analyze threats in the position from the opponent's perspective.
   *
   * @param motifs tactical motif inputs detected in the position
   * @param multiPv MultiPV lines from engine analysis
   * @param features board-derived context for threshold adjustment
   * @param sideUnderPressure side whose defensive requirements are being assessed
   * @return Complete threat analysis
   */
  def analyze(
    fen: String,
    motifs: List[Motif],
    multiPv: List[EngineLine],
    features: PositionFeatures,
    sideUnderPressure: Color
  ): List[Threat] =
    val material = features.materialPhase
    val imbalance = features.imbalance
    val minorPieces =
      imbalance.whiteKnights + imbalance.blackKnights +
        imbalance.whiteBishops + imbalance.blackBishops
    val endgameNear =
      material.phase == "endgame" || material.whiteMaterial + material.blackMaterial <= 50
    val suppressStaticRelationThreats =
      material.phase == "endgame" ||
        insufficientMaterial(features) ||
        (
          endgameNear &&
            imbalance.whiteQueens == 0 &&
            imbalance.blackQueens == 0 &&
            minorPieces <= 2
        )

    val threatActor = !sideUnderPressure
    val opponentThreats = extractOpponentThreats(motifs, threatActor, suppressStaticRelationThreats)
    val pressureSideToMove =
      Fen.read(Standard, Fen.Full(fen)).exists(_.color == sideUnderPressure)
    val withDefenses =
      if pressureSideToMove then
        populateDefenseEvidence(opponentThreats, multiPv, fen, sideUnderPressure)
      else opponentThreats
    withDefenses

  /**
   * Extract opponent threats from tactical motif inputs.
   * We keep conservative base timing/loss estimates so positional pressure survives
   * even when MultiPV evidence is thin.
   */
  private def extractOpponentThreats(
      motifs: List[Motif],
      threatActor: Color,
      suppressStaticRelationThreats: Boolean
  ): List[Threat] =
    motifs.flatMap { motif =>
      if threateningColor(motif).contains(threatActor) then
        threatProfileFor(motif, suppressStaticRelationThreats).map { profile =>
          Threat(
            threatActor = threatActor,
            kind = profile.kind,
            turnsToImpact = profile.turnsToImpact,
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
        Some(ThreatProfile(ThreatKind.Mate, 1))
      case m: Motif.Check if m.checkType == Motif.CheckType.Mate || m.checkType == Motif.CheckType.Smothered =>
        Some(ThreatProfile(ThreatKind.Mate, 1))
      case _: Motif.Check | _: Motif.DoubleCheck =>
        Some(ThreatProfile(ThreatKind.Material, 1))
      case _: Motif.Capture =>
        Some(ThreatProfile(ThreatKind.Material, 1))
      case (_: Motif.Pin | _: Motif.XRay) if suppressStaticRelationThreats =>
        None
      case _: Motif.Fork | _: Motif.Pin | _: Motif.Skewer | _: Motif.DiscoveredAttack |
          _: Motif.Deflection | _: Motif.Decoy | _: Motif.Overloading | _: Motif.Interference |
          _: Motif.Clearance | _: Motif.Zwischenzug | _: Motif.RemovingTheDefender | _: Motif.XRay =>
        Some(ThreatProfile(ThreatKind.Material, 1))
      case m: Motif.TrappedPiece =>
        Some(ThreatProfile(ThreatKind.Material, if m.isValuableTrap then 1 else 2))
      case _: Motif.PawnPromotion =>
        Some(ThreatProfile(ThreatKind.Material, 1))
      case m: Motif.PassedPawnPush =>
        val rel = m.relativeTo
        Some(
          ThreatProfile(
            kind = if rel >= 6 then ThreatKind.Material else ThreatKind.Positional,
            turnsToImpact = if rel >= 6 then 1 else 2
          )
        )
      case m: Motif.PassedPawn =>
        val rel = Motif.relativeRank(m.rank, m.color)
        Some(
          ThreatProfile(
            kind = if rel >= 6 then ThreatKind.Material else ThreatKind.Positional,
            turnsToImpact = if rel >= 6 then 2 else if rel >= 4 then 3 else 4
          )
        )
      case _: Motif.WeakBackRank =>
        Some(ThreatProfile(ThreatKind.Positional, 2))
      case _: Motif.RookBehindPassedPawn | _: Motif.SeventhRankInvasion | _: Motif.KingCutOff =>
        Some(ThreatProfile(ThreatKind.Positional, 2))
      case _: Motif.OpenFileControl | _: Motif.SemiOpenFileControl | _: Motif.RookLift |
          _: Motif.Battery | _: Motif.SpaceAdvantage | _: Motif.Initiative |
          _: Motif.Domination | _: Motif.Blockade =>
        Some(ThreatProfile(ThreatKind.Positional, 3))
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
      case m: Motif.Check => m.move.filter(_.length >= 4).map(_.slice(2, 4)).toList :+ m.targetSquare.key
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

  private def firstLegalStep(
    fen: String,
    pv: EngineLine
  ): Option[LegalReplayStep] =
    TacticalRelationEvidence
      .boundedReplay(fen, pv.moves, maxPlies = 1)
      .flatMap(_.headOption)

  private def activeForcingRoot(step: LegalReplayStep): Boolean =
    step.move.after.check.yes ||
      step.move.promotion.isDefined

  private def normalizedMove(move: String): String =
    move.trim.toLowerCase

  private def populateDefenseEvidence(
    threats: List[Threat],
    multiPv: List[EngineLine],
    fen: String,
    sideUnderPressure: Color
  ): List[Threat] =
    if threats.isEmpty then threats
    else if multiPv.isEmpty then threats
    else
      // Count defenses that keep eval within the central defense tolerance.
      val bestLine = multiPv.head
      val depthReliable = multiPv.forall(line => JudgmentThresholds.engineBackedByDepth(line.depth, line.mate))
      val adequateDefenses = multiPv.filter { pv =>
        PerspectiveMath
          .compareForMover(
            sideUnderPressure,
            PerspectiveMath.EvalPoint(bestLine.scoreCp, bestLine.mate),
            PerspectiveMath.EvalPoint(pv.scoreCp, pv.mate)
          )
          .winPercentLossForMover <= JudgmentThresholds.ONLY_DEFENSE_TOLERANCE_WP
      }
      val distinctDefenseRoots = adequateDefenses.flatMap(_.moves.headOption).map(normalizedMove).distinct
      val defenseCount =
        if depthReliable && multiPv.size >= 3 then distinctDefenseRoots.size.max(adequateDefenses.size.min(1))
        else 0

      threats.map { t =>
        val urgentMateMotif =
          t.kind == ThreatKind.Mate &&
            bestLineDefusesMateThreat(fen, bestLine, t)
        t.copy(
          defenseCount = if urgentMateMotif then defenseCount else t.defenseCount,
          bestDefense = if urgentMateMotif then multiPv.headOption.flatMap(_.moves.headOption) else t.bestDefense
        )
      }

  private def bestLineDefusesMateThreat(
      fen: String,
      bestLine: EngineLine,
      threat: Threat
  ): Boolean =
    firstLegalStep(fen, bestLine).exists { step =>
      val afterMotifs = MoveAnalyzer.detectStateMotifs(step.move.after, 1)
      activeForcingRoot(step) &&
        !afterMotifs.exists(motif =>
          threateningColor(motif).contains(threat.threatActor) && mateThreatMotif(motif)
        )
    }

  private def mateThreatMotif(motif: Motif): Boolean =
    motif match
      case m: Motif.Check =>
        m.checkType == Motif.CheckType.Mate || m.checkType == Motif.CheckType.Smothered
      case _: Motif.BackRankMate | _: Motif.MateNet | _: Motif.SmotheredMate =>
        true
      case _ =>
        false

  private def insufficientMaterial(features: PositionFeatures): Boolean =
    val imbalance = features.imbalance
    val pawns = features.pawns.whitePawnCount + features.pawns.blackPawnCount
    val minors =
      imbalance.whiteKnights + imbalance.blackKnights +
        imbalance.whiteBishops + imbalance.blackBishops
    val majors =
      imbalance.whiteRooks + imbalance.blackRooks +
        imbalance.whiteQueens + imbalance.blackQueens
    pawns == 0 && majors == 0 && minors <= 1
