package lila.chessjudgment.model.strategic

import lila.chessjudgment.model.PlanCategory

object PlanTaxonomy:

  enum PlanTheme(val id: String):
    case OpeningPrinciples extends PlanTheme("opening_principles")
    case RestrictionProphylaxis extends PlanTheme("restriction_prophylaxis")
    case PieceRedeployment extends PlanTheme("piece_redeployment")
    case SpaceClamp extends PlanTheme("space_clamp")
    case WeaknessFixation extends PlanTheme("weakness_fixation")
    case PawnBreakPreparation extends PlanTheme("pawn_break_preparation")
    case FavorableExchange extends PlanTheme("favorable_exchange")
    case WingPlay extends PlanTheme("wing_play")
    case AdvantageTransformation extends PlanTheme("advantage_transformation")
    case Unknown extends PlanTheme("unknown")

  object PlanTheme:
    val ranked: List[PlanTheme] = List(
      OpeningPrinciples,
      RestrictionProphylaxis,
      PieceRedeployment,
      SpaceClamp,
      WeaknessFixation,
      PawnBreakPreparation,
      FavorableExchange,
      WingPlay,
      AdvantageTransformation
    )
    private val byId: Map[String, PlanTheme] =
      PlanTheme.values.toList.map(t => t.id -> t).toMap
    def fromId(raw: String): Option[PlanTheme] =
      byId.get(normalize(raw))

  enum PlanKind(val id: String, val theme: PlanTheme):
    case OpeningDevelopment extends PlanKind("opening_development", PlanTheme.OpeningPrinciples)
    case ProphylaxisRestraint extends PlanKind("prophylaxis_restraint", PlanTheme.RestrictionProphylaxis)
    case BreakPrevention extends PlanKind("break_prevention", PlanTheme.RestrictionProphylaxis)
    case KeySquareDenial extends PlanKind("key_square_denial", PlanTheme.RestrictionProphylaxis)

    case OutpostEntrenchment extends PlanKind("outpost_entrenchment", PlanTheme.PieceRedeployment)
    case WorstPieceImprovement extends PlanKind("worst_piece_improvement", PlanTheme.PieceRedeployment)
    case RookFileTransfer extends PlanKind("rook_file_transfer", PlanTheme.PieceRedeployment)
    case BishopReanchor extends PlanKind("bishop_reanchor", PlanTheme.PieceRedeployment)
    case OpenFilePressure extends PlanKind("open_file_pressure", PlanTheme.PieceRedeployment)

    case FlankClamp extends PlanKind("flank_clamp", PlanTheme.SpaceClamp)
    case CentralSpaceBind extends PlanKind("central_space_bind", PlanTheme.SpaceClamp)
    case MobilitySuppression extends PlanKind("mobility_suppression", PlanTheme.SpaceClamp)

    case StaticWeaknessFixation extends PlanKind("static_weakness_fixation", PlanTheme.WeaknessFixation)
    case MinorityAttackFixation extends PlanKind("minority_attack_fixation", PlanTheme.WeaknessFixation)
    case BackwardPawnTargeting extends PlanKind("backward_pawn_targeting", PlanTheme.WeaknessFixation)
    case IQPInducement extends PlanKind("iqp_inducement", PlanTheme.WeaknessFixation)

    case CentralBreakTiming extends PlanKind("central_break_timing", PlanTheme.PawnBreakPreparation)
    case WingBreakTiming extends PlanKind("wing_break_timing", PlanTheme.PawnBreakPreparation)
    case TensionMaintenance extends PlanKind("tension_maintenance", PlanTheme.PawnBreakPreparation)

    case SimplificationWindow extends PlanKind("simplification_window", PlanTheme.FavorableExchange)
    case DefenderTrade extends PlanKind("defender_trade", PlanTheme.FavorableExchange)
    case QueenTradeShield extends PlanKind("queen_trade_shield", PlanTheme.FavorableExchange)
    case BadPieceLiquidation extends PlanKind("bad_piece_liquidation", PlanTheme.FavorableExchange)

    case WingExpansion extends PlanKind("wing_expansion", PlanTheme.WingPlay)
    case HookCreation extends PlanKind("hook_creation", PlanTheme.WingPlay)
    case RookLiftScaffold extends PlanKind("rook_lift_scaffold", PlanTheme.WingPlay)
    case KingsideWingExpansion extends PlanKind("kingside_wing_expansion", PlanTheme.WingPlay)
    case QueensideWingExpansion extends PlanKind("queenside_wing_expansion", PlanTheme.WingPlay)

    case SimplificationConversion extends PlanKind("simplification_conversion", PlanTheme.AdvantageTransformation)
    case PasserConversion extends PlanKind("passer_conversion", PlanTheme.AdvantageTransformation)
    case PassedPawnManufacture extends PlanKind("passed_pawn_manufacture", PlanTheme.AdvantageTransformation)
    case InvasionTransition extends PlanKind("invasion_transition", PlanTheme.AdvantageTransformation)
    case OppositeBishopsConversion extends PlanKind("opposite_bishops_conversion", PlanTheme.AdvantageTransformation)

    def category: PlanCategory =
      theme match
        case PlanTheme.OpeningPrinciples       => PlanCategory.Opening
        case PlanTheme.RestrictionProphylaxis  => PlanCategory.Defensive
        case PlanTheme.PieceRedeployment       => PlanCategory.Positional
        case PlanTheme.SpaceClamp              => PlanCategory.Positional
        case PlanTheme.WeaknessFixation        => PlanCategory.Structural
        case PlanTheme.PawnBreakPreparation    => PlanCategory.Structural
        case PlanTheme.FavorableExchange       => PlanCategory.Transition
        case PlanTheme.WingPlay                => PlanCategory.Attack
        case PlanTheme.AdvantageTransformation => PlanCategory.Transition
        case PlanTheme.Unknown                 => PlanCategory.Positional

    /**
     * Presentation-only label. Internal identities, keys and graph anchors must
     * use the injective `id` value.
     */
    def publicLabel: String =
      this match
        case OpeningDevelopment                        => "OpeningDevelopment"
        case ProphylaxisRestraint | BreakPrevention |
            KeySquareDenial | MobilitySuppression     => "Prophylaxis"
        case OutpostEntrenchment | WorstPieceImprovement |
            InvasionTransition                        => "PieceActivation"
        case RookFileTransfer                          => "RookActivation"
        case BishopReanchor                            => "MinorPieceManeuver"
        case OpenFilePressure                          => "FileControl"
        case FlankClamp                                => "SpaceAdvantage"
        case CentralSpaceBind                          => "CentralControl"
        case StaticWeaknessFixation | BackwardPawnTargeting |
            IQPInducement                              => "WeakPawnAttack"
        case MinorityAttackFixation                    => "MinorityAttack"
        case CentralBreakTiming | WingBreakTiming |
            TensionMaintenance                         => "PawnBreakPreparation"
        case SimplificationWindow | DefenderTrade |
            BadPieceLiquidation                        => "Exchange"
        case QueenTradeShield                          => "QueenTrade"
        case WingExpansion                             => "WingExpansion"
        case HookCreation                              => "PawnStorm"
        case RookLiftScaffold                          => "RookActivation"
        case KingsideWingExpansion                     => "KingsideAttack"
        case QueensideWingExpansion                    => "QueensideAttack"
        case SimplificationConversion |
            OppositeBishopsConversion                  => "Simplification"
        case PasserConversion                          => "PassedPawnPush"
        case PassedPawnManufacture                     => "PassedPawnCreation"

  object PlanKind:
    private val byId: Map[String, PlanKind] =
      PlanKind.values.toList.map(s => s.id -> s).toMap

    def fromId(raw: String): Option[PlanKind] =
      byId.get(normalize(raw))

  enum PlanSignal:
    case MoveMotifSignal
    case StrategicSnapshotSignal
    case CandidateLineSignal
    case StructuralSignal

  enum PlanHorizon:
    case Short
    case Medium
    case Long

  final case class SubplanSpec(
      requiredSignals: List[PlanSignal],
      horizon: PlanHorizon
  )

  object SubplanCatalog:
    import PlanHorizon.*
    import PlanSignal.*

    val specs: Map[PlanKind, SubplanSpec] = Map(
      PlanKind.OpeningDevelopment -> SubplanSpec(
        requiredSignals = List(MoveMotifSignal, StrategicSnapshotSignal),
        horizon = Short
      ),
      PlanKind.ProphylaxisRestraint -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Long
      ),
      PlanKind.BreakPrevention -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StructuralSignal),
        horizon = Medium
      ),
      PlanKind.KeySquareDenial -> SubplanSpec(
        requiredSignals = List(MoveMotifSignal),
        horizon = Medium
      ),
      PlanKind.OutpostEntrenchment -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Long
      ),
      PlanKind.WorstPieceImprovement -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal),
        horizon = Medium
      ),
      PlanKind.RookFileTransfer -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.BishopReanchor -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.OpenFilePressure -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.FlankClamp -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Long
      ),
      PlanKind.CentralSpaceBind -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal),
        horizon = Medium
      ),
      PlanKind.MobilitySuppression -> SubplanSpec(
        requiredSignals = List(MoveMotifSignal),
        horizon = Medium
      ),
      PlanKind.StaticWeaknessFixation -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Long
      ),
      PlanKind.MinorityAttackFixation -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Long
      ),
      PlanKind.BackwardPawnTargeting -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal),
        horizon = Long
      ),
      PlanKind.IQPInducement -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StructuralSignal),
        horizon = Medium
      ),
      PlanKind.CentralBreakTiming -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StructuralSignal),
        horizon = Medium
      ),
      PlanKind.WingBreakTiming -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StructuralSignal),
        horizon = Medium
      ),
      PlanKind.TensionMaintenance -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal),
        horizon = Medium
      ),
      PlanKind.SimplificationWindow -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StructuralSignal),
        horizon = Medium
      ),
      PlanKind.DefenderTrade -> SubplanSpec(
        requiredSignals = List(MoveMotifSignal),
        horizon = Short
      ),
      PlanKind.QueenTradeShield -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Short
      ),
      PlanKind.BadPieceLiquidation -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Short
      ),
      PlanKind.WingExpansion -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Medium
      ),
      PlanKind.HookCreation -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.RookLiftScaffold -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.KingsideWingExpansion -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Medium
      ),
      PlanKind.QueensideWingExpansion -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Medium
      ),
      PlanKind.SimplificationConversion -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.PasserConversion -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, MoveMotifSignal),
        horizon = Long
      ),
      PlanKind.PassedPawnManufacture -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal, StructuralSignal),
        horizon = Long
      ),
      PlanKind.InvasionTransition -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal),
        horizon = Medium
      ),
      PlanKind.OppositeBishopsConversion -> SubplanSpec(
        requiredSignals = List(CandidateLineSignal, StrategicSnapshotSignal, MoveMotifSignal),
        horizon = Long
      )
    )

  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase
