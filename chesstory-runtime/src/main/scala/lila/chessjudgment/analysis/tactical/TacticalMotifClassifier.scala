package lila.chessjudgment.analysis.tactical

import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.MoveMotifEvidence
import lila.chessjudgment.model.judgment.RelationFactKind

object TacticalMotifClassifier:

  def isTactical(motif: Motif): Boolean =
    motif match
      case _: Motif.Check | _: Motif.DoubleCheck | _: Motif.BackRankMate | _: Motif.MateNet | _: Motif.SmotheredMate |
          _: Motif.Capture | _: Motif.Zwischenzug | _: Motif.Fork | _: Motif.Pin | _: Motif.Skewer |
          _: Motif.DiscoveredAttack | _: Motif.RemovingTheDefender | _: Motif.Deflection | _: Motif.Decoy |
          _: Motif.XRay | _: Motif.Overloading | _: Motif.Interference | _: Motif.Clearance |
          _: Motif.TrappedPiece | _: Motif.PawnPromotion | _: Motif.PassedPawnPush | _: Motif.StalemateThreat |
          _: Motif.WeakBackRank =>
        true
      case _ =>
        false

  def isForcing(motif: Motif): Boolean =
    motif match
      case _: Motif.Check | _: Motif.DoubleCheck | _: Motif.BackRankMate | _: Motif.MateNet | _: Motif.SmotheredMate |
          _: Motif.Zwischenzug | _: Motif.PawnPromotion =>
        true
      case _ =>
        false

  def isCauseEligible(motif: Motif): Boolean =
    isForcing(motif) ||
      (motif match
        case m: Motif.Capture =>
          m.captureType match
            case Motif.CaptureType.Winning | Motif.CaptureType.Exchange | Motif.CaptureType.Recapture |
                Motif.CaptureType.ExchangeSacrifice =>
              true
            case _ =>
              false
        case _: Motif.Fork | _: Motif.Skewer | _: Motif.DiscoveredAttack | _: Motif.RemovingTheDefender |
            _: Motif.TrappedPiece =>
          true
        case _ =>
          false
      )

  def isRootMoveMotif(rootMove: String, motif: Motif): Boolean =
    motif.move.contains(rootMove) || (motif.move.isEmpty && motif.plyIndex == 0)

  def isRootMoveMotif(payload: MoveMotifEvidence): Boolean =
    payload.isRootEvent

  def rootMotif(payload: MoveMotifEvidence): Option[Motif] =
    Option.when(isRootMoveMotif(payload))(payload.motif)

  def isRootForcing(payload: MoveMotifEvidence): Boolean =
    isRootMoveMotif(payload) && isForcing(payload.motif)

  def isRootCauseEligible(payload: MoveMotifEvidence): Boolean =
    isRootMoveMotif(payload) && isCauseEligible(payload.motif)

  def isRootTactical(payload: MoveMotifEvidence): Boolean =
    isRootMoveMotif(payload) && isTactical(payload.motif)

  def isRiskRelation(kind: RelationFactKind): Boolean =
    kind match
      case RelationFactKind.DefenderTrade | RelationFactKind.Zwischenzug |
          RelationFactKind.DoubleCheck | RelationFactKind.BackRankMate | RelationFactKind.MateNet |
          RelationFactKind.GreekGift | RelationFactKind.Fork | RelationFactKind.Pin |
          RelationFactKind.Skewer | RelationFactKind.Overload | RelationFactKind.Deflection |
          RelationFactKind.DiscoveredAttack | RelationFactKind.Decoy | RelationFactKind.Interference |
          RelationFactKind.Clearance | RelationFactKind.XRay | RelationFactKind.Battery |
          RelationFactKind.HangingPiece | RelationFactKind.TrappedPiece | RelationFactKind.Domination =>
        true
      case _ =>
        false
