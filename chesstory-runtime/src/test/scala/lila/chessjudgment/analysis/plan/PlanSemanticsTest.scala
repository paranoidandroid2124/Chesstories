package lila.chessjudgment.analysis.plan

import chess.{ Black, White }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class PlanSemanticsTest extends munit.FunSuite:

  test("transformation kinds cannot borrow a sibling result"):
    val pawnTransition = transition(
      "4k3/8/P7/8/8/8/8/4K3 w - - 0 1",
      "a6a7"
    )
    val passerProgress = consequence(
      TransitionConsequenceKind.PassedPawnProgress,
      List(StructuralSubject.PassedPawnAdvanced(White, EvidenceSquare("a6"), EvidenceSquare("a7"), 7))
    )
    val passerCreation = consequence(
      TransitionConsequenceKind.PassedPawnProgress,
      List(StructuralSubject.PassedPawnCreated(White, EvidenceSquare("a7")))
    )
    assert(proves(PlanKind.PasserConversion, pawnTransition, passerProgress))
    assert(
      !proves(
        PlanKind.PasserConversion,
        pawnTransition,
        consequence(
          TransitionConsequenceKind.PassedPawnProgress,
          List(StructuralSubject.PassedPawnAdvanced(White, EvidenceSquare("a6"), EvidenceSquare("a7"), 6))
        )
      )
    )
    assert(!proves(PlanKind.PasserConversion, pawnTransition, passerCreation))
    assert(proves(PlanKind.PassedPawnManufacture, pawnTransition, passerCreation))

    val promotionTransition = transition(
      "4k3/P7/8/8/8/8/8/4K3 w - - 0 1",
      "a7a8q"
    )
    assert(
      proves(
        PlanKind.PasserConversion,
        promotionTransition,
        consequence(
          TransitionConsequenceKind.PassedPawnProgress,
          List(StructuralSubject.PassedPawnPromoted(White, EvidenceSquare("a7"), EvidenceSquare("a8")))
        )
      )
    )

    val invasionTransition = transition(
      "7k/8/8/8/8/8/8/R6K w - - 0 1",
      "a1a7"
    )
    val invasion = consequence(
      TransitionConsequenceKind.FileOccupationEstablished,
      List(
        StructuralSubject.FileOccupation(
          EvidenceFile("a"),
          EvidenceSquare("a7"),
          EvidencePieceRole("rook")
        )
      )
    )
    assert(proves(PlanKind.InvasionTransition, invasionTransition, invasion))

  test("pawn-advance preparation is board-zone agnostic"):
    val centralTransition = transition(
      "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1",
      "e2e4"
    )
    val centralTension = consequence(
      TransitionConsequenceKind.PawnTensionCreated,
      List(
        StructuralSubject.PawnTensionCreated(EvidenceSquare("e4"), EvidenceSquare("d5")),
        StructuralSubject.BreakFile(EvidenceFile("e"))
      )
    )
    assert(proves(PlanKind.PawnAdvancePreparation, centralTransition, centralTension))

    val flankTransition = transition(
      "4k3/8/8/8/8/8/7P/4K3 w - - 0 1",
      "h2h4"
    )
    val flankTension = consequence(
      TransitionConsequenceKind.PawnTensionCreated,
      List(
        StructuralSubject.PawnTensionCreated(EvidenceSquare("h4"), EvidenceSquare("g5")),
        StructuralSubject.BreakFile(EvidenceFile("h"))
      )
    )
    assert(proves(PlanKind.PawnAdvancePreparation, flankTransition, flankTension))

  test("file transfer requires its exact actor-result pair"):
    val queenTransfer = transition("4k3/8/8/8/8/8/8/Q3K3 w - - 0 1", "a1a7")
    val fileOccupation = consequence(
      TransitionConsequenceKind.FileOccupationEstablished,
      List(
        StructuralSubject.FileOccupation(
          EvidenceFile("a"),
          EvidenceSquare("a7"),
          EvidencePieceRole("rook")
        )
      )
    )
    assert(!proves(PlanKind.RookFileTransfer, queenTransfer, fileOccupation))

    val rookTransfer = transition("7k/8/8/8/8/8/8/R6K w - - 0 1", "a1a7")
    assert(proves(PlanKind.RookFileTransfer, rookTransfer, fileOccupation))
    assert(
      !proves(
        PlanKind.RookFileTransfer,
        rookTransfer,
        consequence(
          TransitionConsequenceKind.FileOccupationEstablished,
          List(
            StructuralSubject.FileOccupation(
              EvidenceFile("b"),
              EvidenceSquare("b7"),
              EvidencePieceRole("rook")
            )
          )
        )
      )
    )
  test("causal target extraction separates battery witnesses from its exact target"):
    val battery = TransitionConsequence(
      kind = TransitionConsequenceKind.BatteryFormation,
      strength = 1,
      subjectBindings = List(
        StructuralSubjectBinding.unbound(
          StructuralSubject.Battery(
            RelationWitnessDetail.RayBarrier(
              White,
              EvidenceSquare("c1"),
              EvidencePieceRole("bishop"),
              List(
                RelationColoredPieceWitness(
                  EvidenceSquare("d2"),
                  EvidencePieceRole("queen"),
                  White
                ),
                RelationColoredPieceWitness(
                  EvidenceSquare("e3"),
                  EvidencePieceRole("rook"),
                  Black
                )
              ),
              RelationAxisSignal.Diagonal
            )
          )
        )
      ),
      targetBindings = List(
        StructuralSubjectBinding.unbound(
          StructuralSubject.PieceAt(EvidencePieceRole("rook"), EvidenceSquare("e3"))
        )
      )
    )
    assertEquals(
      PlanCausalEpisode.consequenceSquares(battery).map(_.key).toSet,
      Set("c1", "d2", "e3")
    )
    assertEquals(
      PlanCausalEpisode.consequenceTargetSquares(battery).map(_.key),
      List("e3")
    )

  private def proves(
      kind: PlanKind,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    PlanCausalGoalProof.proves(kind.theme, Some(kind), transition, consequence)

  private def consequence(
      kind: TransitionConsequenceKind,
      subjects: List[StructuralSubject]
  ): TransitionConsequence =
    TransitionConsequence(
      kind = kind,
      strength = 1,
      subjectBindings = subjects.map(StructuralSubjectBinding.unbound)
    )

  private def transition(fen: String, move: String): StructuralTransitionBinding =
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .getOrElse(fail("expected a canonical test root"))
    val extended = history
      .extend(List(move))
      .getOrElse(fail(s"expected legal test move $move"))
    val step = extended.segmentReplaySteps.lastOption.getOrElse(fail("expected one legal step"))
    StructuralTransitionBinding(
      moveUci = move,
      role = TransitionEdgeRole.Reference,
      from = PositionNodeRef(fen, 0, Some(White)),
      to = PositionNodeRef(step.afterFen, 1, Some(chess.Black)),
      line = None,
      perspective = White,
      actorRole = Some(EvidencePieceRole(step.move.piece.role.name))
    )
