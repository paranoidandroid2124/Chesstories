package lila.chessjudgment.model

import chess.Color
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }

/**
 * A plan persists exactly one classification value; theme is derived from it.
 */
final case class Plan(kind: PlanKind, color: Color):
  def theme: PlanTheme = kind.theme
