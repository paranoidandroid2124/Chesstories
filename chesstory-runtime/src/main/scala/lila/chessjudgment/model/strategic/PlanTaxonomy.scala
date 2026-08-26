package lila.chessjudgment.model.strategic

object PlanTaxonomy:

  enum PlanTheme(val id: String):
    case OpeningPrinciples extends PlanTheme("opening_principles")
    case RestrictionProphylaxis extends PlanTheme("restriction_prophylaxis")
    case PieceRedeployment extends PlanTheme("piece_redeployment")
    case PawnBreakPreparation extends PlanTheme("pawn_break_preparation")
    case AdvantageTransformation extends PlanTheme("advantage_transformation")

  object PlanTheme:
    private val byId: Map[String, PlanTheme] =
      PlanTheme.values.toList.map(theme => theme.id -> theme).toMap

    def fromId(raw: String): Option[PlanTheme] =
      byId.get(normalize(raw))

  enum PlanKind(val id: String, val theme: PlanTheme):
    case OpeningDevelopment extends PlanKind("opening_development", PlanTheme.OpeningPrinciples)
    case ProphylaxisRestraint extends PlanKind("prophylaxis_restraint", PlanTheme.RestrictionProphylaxis)

    case RookFileTransfer extends PlanKind("rook_file_transfer", PlanTheme.PieceRedeployment)

    case PawnAdvancePreparation extends PlanKind("pawn_advance_preparation", PlanTheme.PawnBreakPreparation)

    case PasserConversion extends PlanKind("passer_conversion", PlanTheme.AdvantageTransformation)
    case PassedPawnManufacture extends PlanKind("passed_pawn_manufacture", PlanTheme.AdvantageTransformation)
    case InvasionTransition extends PlanKind("invasion_transition", PlanTheme.AdvantageTransformation)

  object PlanKind:
    private val byId: Map[String, PlanKind] =
      PlanKind.values.toList.map(s => s.id -> s).toMap

    def fromId(raw: String): Option[PlanKind] =
      byId.get(normalize(raw))


  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase
