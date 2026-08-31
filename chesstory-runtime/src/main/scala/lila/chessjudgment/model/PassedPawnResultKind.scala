package lila.chessjudgment.model

enum PassedPawnResultKind(val id: String):
  case AdvanceOrPromote extends PassedPawnResultKind("passed_pawn_advance_or_promotion")
  case Creation extends PassedPawnResultKind("passed_pawn_creation")
