package lila.chessjudgment.model.structure

import lila.chessjudgment.model.PlanId
import play.api.libs.json.*

enum StructureId:
  case Carlsbad
  case IQPWhite
  case IQPBlack
  case HangingPawnsWhite
  case HangingPawnsBlack
  case FrenchAdvanceChain
  case NajdorfScheveningenCenter
  case BenoniCenter
  case KIDLockedCenter
  case SlavCaroTriangle
  case MaroczyBind
  case Hedgehog
  case FianchettoShell
  case Stonewall
  case OpenCenter
  case LockedCenter
  case FluidCenter
  case SymmetricCenter
  case Unknown

object StructureId:
  val taxonomy: List[StructureId] = List(
    StructureId.Carlsbad,
    StructureId.IQPWhite,
    StructureId.IQPBlack,
    StructureId.HangingPawnsWhite,
    StructureId.HangingPawnsBlack,
    StructureId.FrenchAdvanceChain,
    StructureId.NajdorfScheveningenCenter,
    StructureId.BenoniCenter,
    StructureId.KIDLockedCenter,
    StructureId.SlavCaroTriangle,
    StructureId.MaroczyBind,
    StructureId.Hedgehog,
    StructureId.FianchettoShell,
    StructureId.Stonewall,
    StructureId.OpenCenter,
    StructureId.LockedCenter,
    StructureId.FluidCenter,
    StructureId.SymmetricCenter
  )

  given Writes[StructureId] = Writes(id => JsString(id.toString))
  given Reads[StructureId] = Reads {
    case JsString(raw) =>
      StructureId.values.find(_.toString == raw).map(JsSuccess(_)).getOrElse(JsError(s"Unknown StructureId: $raw"))
    case _ => JsError("Expected StructureId string")
  }

enum CenterState:
  case Open
  case Locked
  case Fluid
  case Symmetric

object CenterState:
  given Writes[CenterState] = Writes(v => JsString(v.toString))
  given Reads[CenterState] = Reads {
    case JsString(raw) =>
      CenterState.values.find(_.toString == raw).map(JsSuccess(_)).getOrElse(JsError(s"Unknown CenterState: $raw"))
    case _ => JsError("Expected CenterState string")
  }

enum AlignmentBand:
  case OnBook
  case Playable
  case OffPlan
  case Unknown

object AlignmentBand:
  given Writes[AlignmentBand] = Writes(v => JsString(v.toString))
  given Reads[AlignmentBand] = Reads {
    case JsString(raw) =>
      AlignmentBand.values.find(_.toString == raw).map(JsSuccess(_)).getOrElse(JsError(s"Unknown AlignmentBand: $raw"))
    case _ => JsError("Expected AlignmentBand string")
  }

enum StructurePrecondition:
  case TensionOrBreak
  case MinorityReady
  case PieceActivity
  case LockedCenter
  case OpenCenter
  case SolidCenter
  case CounterBreakWatch
  case KingFlankFocus

object StructurePrecondition:
  given Writes[StructurePrecondition] = Writes(v => JsString(v.toString))
  given Reads[StructurePrecondition] = Reads {
    case JsString(raw) =>
      StructurePrecondition.values.find(_.toString == raw).map(JsSuccess(_)).getOrElse(JsError(s"Unknown StructurePrecondition: $raw"))
    case _ => JsError("Expected StructurePrecondition string")
  }

case class StructureProfile(
    primary: StructureId,
    confidence: Double,
    alternatives: List[StructureId],
    centerState: CenterState,
    evidenceCodes: List[String]
)

object StructureProfile:
  given OFormat[StructureProfile] = Json.format[StructureProfile]

case class StructuralPlaybookEntry(
    structureId: StructureId,
    whitePlans: List[PlanId],
    blackPlans: List[PlanId],
    counterPlans: List[PlanId],
    preconditions: List[StructurePrecondition] = Nil
)

case class PlanAlignment(
    score: Int,
    band: AlignmentBand,
    matchedPlanIds: List[String],
    missingPlanIds: List[String],
    reasonCodes: List[String],
    reasonWeights: Map[String, Double] = Map.empty
)

object PlanAlignment:
  given OFormat[PlanAlignment] = Json.format[PlanAlignment]
