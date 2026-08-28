package lila.chessjudgment.analysis.assembly

import chess.White

import lila.chessjudgment.model.Plan
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory }
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class BranchReplyHorizonContractTest extends munit.FunSuite:

  test("a deeper branch supplies only the exact required horizon prefix"):
    val fen = "4k3/p7/3p4/8/4P3/8/8/4K1N1 w - - 0 1"
    val rootMove = "g1f3"
    val branchMoves = List("a7a6", "e4e5", "d6e5")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.getOrElse(
      fail("expected the starting history")
    )
    val fullReplay = replayFrom(history, rootMove :: branchMoves)
    val rootReplay = fullReplay.subset(List(fullReplay.replaySteps.head)).getOrElse(
      fail("expected the root replay")
    )
    val rootStep = rootReplay.replaySteps.head
    val branchReplay = replayFrom(
      CanonicalPositionHistory.from(rootStep.fenAfter, Nil, rootStep.fenAfter).toOption.getOrElse(
        fail("expected the branch history")
      ),
      branchMoves
    )
    val rootLine = LineNodeRef("root", rootMove, 1, LineNodeRole.Played)
    val transition = StructuralTransitionBinding(
      moveUci = rootMove,
      role = TransitionEdgeRole.Played,
      from = PositionNodeRef(fen, 0, Some(White)),
      to = PositionNodeRef(rootStep.fenAfter, 1, Some(rootReplay.legalSteps.head.after.color)),
      line = Some(rootLine),
      perspective = White,
      actorRole = Some(EvidencePieceRole("knight"))
    )
    val input = NormalizedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = rootMove,
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = rootStep.fenAfter,
      afterReferenceFen = None,
      lines = List(
        NormalizedCandidateLine(
          LineNodeRole.Played,
          1,
          CandidateLineEvaluation.EngineSearch(EngineLine(rootMove :: branchMoves, 0, depth = 20)),
          fullReplay
        )
      ),
      completeCandidateSet = None,
      positionHistory = history,
      openingContext = OpeningContextEvidence(None, Nil)
    )
    val branchEvaluation = CandidateLineEvaluation.EngineSearch(
      EngineLine(branchMoves, scoreCp = 0, depth = 20)
    )
    def branch(sourceProbeId: String, horizon: Int, rank: Int) =
      NormalizedThreatBranch(
        sourceProbeId = sourceProbeId,
        probedMoveUci = rootMove,
        branchFen = rootStep.fenAfter,
        branchPly = 1,
        certifiedHorizonPlyOffset = horizon,
        lines = List(NormalizedCandidateLine(LineNodeRole.Threat, rank, branchEvaluation, branchReplay))
      )
    val exact = branch("exact-horizon", 2, 1)
    val deeper = branch("deeper-horizon", 3, 2)

    assertEquals(
      PlanCausalEventAssembler
        .replyBranchLines(input.copy(threatBranches = List(deeper)), transition, 2)
        .map(_._1.sourceProbeId),
      List("deeper-horizon")
    )
    assertEquals(
      PlanCausalEventAssembler
        .replyBranchLines(input.copy(threatBranches = List(deeper, exact)), transition, 2)
        .map(_._1.sourceProbeId),
      List("exact-horizon")
    )

    val rootTransition = rootReplay.onlyTransition.getOrElse(fail("expected the root transition"))
    val plan = Plan(PlanKind.PasserConversion, White)
    val expectedEpisode = PlanCausalEpisode(
      root = PlanCausalEventNode(
        identity = PlanEventIdentityBuilder.from(rootMove, rootTransition.relationDelta.rootMove, plan),
        step = rootStep,
        perspective = White,
        structuralConsequences = Nil,
        canonicalStep = Some(rootReplay.legalSteps.head),
        canonicalMovement = Some(rootTransition.relationDelta.rootMove)
      ),
      continuations = Nil,
      dependencies = Nil,
      responses = Nil
    )
    val threatLine = LineNodeRef("deeper-prefix", branchMoves.head, 2, LineNodeRole.Threat)
    val witness = PlanCausalEventProof.branchWitness(
      sourceProbeId = deeper.sourceProbeId,
      line = threatLine,
      linePayload = LineFactEvidence.fromCertifiedReplay(threatLine, branchReplay),
      rootLine = rootLine,
      rootTransition = transition,
      plan = plan,
      expectedEpisode = expectedEpisode,
      requiredHorizonPlyOffset = 2,
      evaluation = branchEvaluation,
      admittedReplay = Some(fullReplay)
    )

    assertEquals(witness.certifiedHorizonPlyOffset, 2)
    assertEquals(witness.observedThroughPlyOffset, 2)
    assertEquals(witness.canonicalReplay.map(_.replaySteps), Some(branchReplay.replaySteps.take(2)))

  private def replayFrom(
      history: CanonicalPositionHistory,
      moves: List[String]
  ): CanonicalLineReplay =
    val extended = history.extend(moves).toOption.getOrElse(fail("expected a legal replay"))
    CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected a canonical replay"))
