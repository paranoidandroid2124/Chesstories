package lila.chessjudgment.analysis.structure

import chess.Color
import lila.chessjudgment.model.structure.{ StructureId, StructurePrecondition, StructuralPlaybookEntry }
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

object StructuralPlaybook:
  import StructurePrecondition.*
  import PlanKind.*

  val entries: Map[StructureId, StructuralPlaybookEntry] = List(
    StructuralPlaybookEntry(
      structureId = StructureId.Carlsbad,
      whitePlans = List(MinorityAttackFixation, OpenFilePressure, CentralBreakTiming),
      blackPlans = List(KingsideWingExpansion, CentralBreakTiming, CentralSpaceBind),
      counterPlans = List(WingExpansion),
      preconditions = List(MinorityReady, TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.IQPWhite,
      whitePlans = List(WorstPieceImprovement, KingsideWingExpansion, CentralBreakTiming),
      blackPlans = List(StaticWeaknessFixation, KeySquareDenial, SimplificationWindow),
      counterPlans = List(TensionMaintenance),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.IQPBlack,
      whitePlans = List(StaticWeaknessFixation, KeySquareDenial, SimplificationWindow),
      blackPlans = List(WorstPieceImprovement, KingsideWingExpansion, CentralBreakTiming),
      counterPlans = List(TensionMaintenance),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.HangingPawnsWhite,
      whitePlans = List(CentralBreakTiming, FlankClamp, WorstPieceImprovement),
      blackPlans = List(KeySquareDenial, StaticWeaknessFixation, ProphylaxisRestraint),
      counterPlans = List(QueenTradeShield),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.HangingPawnsBlack,
      whitePlans = List(KeySquareDenial, StaticWeaknessFixation, ProphylaxisRestraint),
      blackPlans = List(CentralBreakTiming, FlankClamp, WorstPieceImprovement),
      counterPlans = List(QueenTradeShield),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.FrenchAdvanceChain,
      whitePlans = List(KingsideWingExpansion, FlankClamp, WingExpansion),
      blackPlans = List(CentralBreakTiming, WingBreakTiming, StaticWeaknessFixation),
      counterPlans = List(SimplificationWindow),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.NajdorfScheveningenCenter,
      whitePlans = List(KingsideWingExpansion, CentralSpaceBind, WorstPieceImprovement),
      blackPlans = List(CentralBreakTiming, QueensideWingExpansion, WingBreakTiming),
      counterPlans = List(QueenTradeShield),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.BenoniCenter,
      whitePlans = List(QueensideWingExpansion, FlankClamp, CentralSpaceBind),
      blackPlans = List(KingsideWingExpansion, WingBreakTiming, CentralBreakTiming),
      counterPlans = List(SimplificationWindow),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.KIDLockedCenter,
      whitePlans = List(QueensideWingExpansion, FlankClamp, ProphylaxisRestraint),
      blackPlans = List(KingsideWingExpansion, WingExpansion, WingBreakTiming),
      counterPlans = List(QueenTradeShield),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.SlavCaroTriangle,
      whitePlans = List(CentralSpaceBind, WorstPieceImprovement, OpenFilePressure),
      blackPlans = List(CentralSpaceBind, CentralBreakTiming, SimplificationWindow),
      counterPlans = Nil,
      preconditions = List(SolidCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.MaroczyBind,
      whitePlans = List(FlankClamp, WorstPieceImprovement, ProphylaxisRestraint),
      blackPlans = List(CentralBreakTiming, WingBreakTiming, QueensideWingExpansion),
      counterPlans = List(SimplificationWindow),
      preconditions = List(PieceActivity)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.Hedgehog,
      whitePlans = List(FlankClamp, WorstPieceImprovement, ProphylaxisRestraint),
      blackPlans = List(WingBreakTiming, CentralBreakTiming, ProphylaxisRestraint),
      counterPlans = List(WingExpansion),
      preconditions = List(CounterBreakWatch)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.FianchettoShell,
      whitePlans = List(KingsideWingExpansion, WingExpansion, WorstPieceImprovement),
      blackPlans = List(KingsideWingExpansion, WingExpansion, WorstPieceImprovement),
      counterPlans = List(QueenTradeShield),
      preconditions = List(KingFlankFocus)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.Stonewall,
      whitePlans = List(KingsideWingExpansion, WorstPieceImprovement, FlankClamp),
      blackPlans = List(KingsideWingExpansion, WorstPieceImprovement, FlankClamp),
      counterPlans = List(StaticWeaknessFixation),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.OpenCenter,
      whitePlans = List(WorstPieceImprovement, CentralSpaceBind, OpenFilePressure),
      blackPlans = List(WorstPieceImprovement, CentralSpaceBind, OpenFilePressure),
      counterPlans = List(TensionMaintenance),
      preconditions = List(OpenCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.LockedCenter,
      whitePlans = List(WingExpansion, QueensideWingExpansion, KingsideWingExpansion),
      blackPlans = List(WingExpansion, QueensideWingExpansion, KingsideWingExpansion),
      counterPlans = List(SimplificationWindow),
      preconditions = List(LockedCenter)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.FluidCenter,
      whitePlans = List(CentralBreakTiming, CentralSpaceBind, WorstPieceImprovement),
      blackPlans = List(CentralBreakTiming, CentralSpaceBind, WorstPieceImprovement),
      counterPlans = List(TensionMaintenance),
      preconditions = List(TensionOrBreak)
    ),
    StructuralPlaybookEntry(
      structureId = StructureId.SymmetricCenter,
      whitePlans = List(WorstPieceImprovement, CentralSpaceBind, ProphylaxisRestraint),
      blackPlans = List(WorstPieceImprovement, CentralSpaceBind, ProphylaxisRestraint),
      counterPlans = Nil,
      preconditions = List(SolidCenter)
    )
  ).map(e => e.structureId -> e).toMap

  def lookup(id: StructureId): Option[StructuralPlaybookEntry] =
    entries.get(id)

  def expectedPlans(entry: StructuralPlaybookEntry, sideToMove: Color): List[PlanKind] =
    if sideToMove == Color.White then entry.whitePlans else entry.blackPlans
