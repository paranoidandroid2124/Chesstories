package lila.chessjudgment.model

enum NatureType:
  case Static, Dynamic, Transition, Chaos

case class PositionNature(
    natureType: NatureType,
    tension: Double,   // 0.0 to 1.0 (0.0 = dead draw/dry, 1.0 = chaos)
    stability: Double // 0.0 to 1.0 (0.0 = wild swings, 1.0 = solid)
)
