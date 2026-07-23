package lila.chessjudgment.analysis.structure

import chess.Color
import lila.chessjudgment.model.PlanId
import lila.chessjudgment.model.structure.{ StructureId, StructurePrecondition, StructuralPlaybookEntry }

object StructuralPlaybook:
  import StructurePrecondition.*

  val entries: Map[StructureId, StructuralPlaybookEntry] = List(
    StructuralPlaybookEntry(
      structureId = StructureId.Carlsbad,
      whitePlans = List(PlanId.MinorityAttack, PlanId.FileControl, PlanId.PawnBreakPreparation),
      blackPlans = List(PlanId.KingsideAttack, PlanId.PawnBreakPreparation, PlanId.CentralControl),
      counterPlans = List(PlanId.PawnStorm),
      preconditions = List(MinorityReady, TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.IQPWhite,
      whitePlans = List(PlanId.PieceActivation, PlanId.KingsideAttack, PlanId.PawnBreakPreparation),
      blackPlans = List(PlanId.WeakPawnAttack, PlanId.Blockade, PlanId.Simplification),
      counterPlans = List(PlanId.PawnChain),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.IQPBlack,
      whitePlans = List(PlanId.WeakPawnAttack, PlanId.Blockade, PlanId.Simplification),
      blackPlans = List(PlanId.PieceActivation, PlanId.KingsideAttack, PlanId.PawnBreakPreparation),
      counterPlans = List(PlanId.PawnChain),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.HangingPawnsWhite,
      whitePlans = List(PlanId.PawnBreakPreparation, PlanId.SpaceAdvantage, PlanId.PieceActivation),
      blackPlans = List(PlanId.Blockade, PlanId.WeakPawnAttack, PlanId.Prophylaxis),
      counterPlans = List(PlanId.QueenTrade),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.HangingPawnsBlack,
      whitePlans = List(PlanId.Blockade, PlanId.WeakPawnAttack, PlanId.Prophylaxis),
      blackPlans = List(PlanId.PawnBreakPreparation, PlanId.SpaceAdvantage, PlanId.PieceActivation),
      counterPlans = List(PlanId.QueenTrade),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.FrenchAdvanceChain,
      whitePlans = List(PlanId.KingsideAttack, PlanId.SpaceAdvantage, PlanId.PawnStorm),
      blackPlans = List(PlanId.PawnBreakPreparation, PlanId.Counterplay, PlanId.WeakPawnAttack),
      counterPlans = List(PlanId.Simplification),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.NajdorfScheveningenCenter,
      whitePlans = List(PlanId.KingsideAttack, PlanId.CentralControl, PlanId.PieceActivation),
      blackPlans = List(PlanId.PawnBreakPreparation, PlanId.QueensideAttack, PlanId.Counterplay),
      counterPlans = List(PlanId.QueenTrade),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.BenoniCenter,
      whitePlans = List(PlanId.QueensideAttack, PlanId.SpaceAdvantage, PlanId.CentralControl),
      blackPlans = List(PlanId.KingsideAttack, PlanId.Counterplay, PlanId.PawnBreakPreparation),
      counterPlans = List(PlanId.Simplification),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.KIDLockedCenter,
      whitePlans = List(PlanId.QueensideAttack, PlanId.SpaceAdvantage, PlanId.Prophylaxis),
      blackPlans = List(PlanId.KingsideAttack, PlanId.PawnStorm, PlanId.Counterplay),
      counterPlans = List(PlanId.QueenTrade),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.SlavCaroTriangle,
      whitePlans = List(PlanId.CentralControl, PlanId.PieceActivation, PlanId.FileControl),
      blackPlans = List(PlanId.CentralControl, PlanId.PawnBreakPreparation, PlanId.Simplification),
      counterPlans = List(PlanId.Sacrifice),
      preconditions = List(SolidCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.MaroczyBind,
      whitePlans = List(PlanId.SpaceAdvantage, PlanId.PieceActivation, PlanId.Prophylaxis),
      blackPlans = List(PlanId.PawnBreakPreparation, PlanId.Counterplay, PlanId.QueensideAttack),
      counterPlans = List(PlanId.Simplification),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.Hedgehog,
      whitePlans = List(PlanId.SpaceAdvantage, PlanId.PieceActivation, PlanId.Prophylaxis),
      blackPlans = List(PlanId.Counterplay, PlanId.PawnBreakPreparation, PlanId.Prophylaxis),
      counterPlans = List(PlanId.PawnStorm),
      preconditions = List(CounterBreakWatch)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.FianchettoShell,
      whitePlans = List(PlanId.KingsideAttack, PlanId.PawnStorm, PlanId.PieceActivation),
      blackPlans = List(PlanId.KingsideAttack, PlanId.PawnStorm, PlanId.PieceActivation),
      counterPlans = List(PlanId.QueenTrade),
      preconditions = List(KingFlankFocus)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.Stonewall,
      whitePlans = List(PlanId.KingsideAttack, PlanId.PieceActivation, PlanId.SpaceAdvantage),
      blackPlans = List(PlanId.KingsideAttack, PlanId.PieceActivation, PlanId.SpaceAdvantage),
      counterPlans = List(PlanId.WeakPawnAttack),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.OpenCenter,
      whitePlans = List(PlanId.PieceActivation, PlanId.CentralControl, PlanId.FileControl),
      blackPlans = List(PlanId.PieceActivation, PlanId.CentralControl, PlanId.FileControl),
      counterPlans = List(PlanId.PawnChain),
      preconditions = List(OpenCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.LockedCenter,
      whitePlans = List(PlanId.PawnStorm, PlanId.QueensideAttack, PlanId.KingsideAttack),
      blackPlans = List(PlanId.PawnStorm, PlanId.QueensideAttack, PlanId.KingsideAttack),
      counterPlans = List(PlanId.Simplification),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.FluidCenter,
      whitePlans = List(PlanId.PawnBreakPreparation, PlanId.CentralControl, PlanId.PieceActivation),
      blackPlans = List(PlanId.PawnBreakPreparation, PlanId.CentralControl, PlanId.PieceActivation),
      counterPlans = List(PlanId.PawnChain),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.SymmetricCenter,
      whitePlans = List(PlanId.PieceActivation, PlanId.CentralControl, PlanId.Prophylaxis),
      blackPlans = List(PlanId.PieceActivation, PlanId.CentralControl, PlanId.Prophylaxis),
      counterPlans = List(PlanId.Sacrifice),
      preconditions = List(SolidCenter)
    )
  ).map(e => e.structureId -> e).toMap

  def lookup(id: StructureId): Option[StructuralPlaybookEntry] =
    entries.get(id)

  def expectedPlans(entry: StructuralPlaybookEntry, sideToMove: Color): List[PlanId] =
    if sideToMove == Color.White then entry.whitePlans else entry.blackPlans
