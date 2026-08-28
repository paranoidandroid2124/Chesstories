package lila.chessjudgment.model.strategic

object PlanTaxonomy:

  enum PlanTheme(val id: String):
    case AdvantageTransformation extends PlanTheme("advantage_transformation")

  object PlanTheme:
    private val byId: Map[String, PlanTheme] =
      PlanTheme.values.toList.map(theme => theme.id -> theme).toMap

    def fromId(raw: String): Option[PlanTheme] =
      byId.get(normalize(raw))

  enum PlanKind(val id: String, val theme: PlanTheme):
    case PasserConversion extends PlanKind("passer_conversion", PlanTheme.AdvantageTransformation)
    case PassedPawnManufacture extends PlanKind("passed_pawn_manufacture", PlanTheme.AdvantageTransformation)

  object PlanKind:
    private val byId: Map[String, PlanKind] =
      PlanKind.values.toList.map(s => s.id -> s).toMap

    def fromId(raw: String): Option[PlanKind] =
      byId.get(normalize(raw))


  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase
