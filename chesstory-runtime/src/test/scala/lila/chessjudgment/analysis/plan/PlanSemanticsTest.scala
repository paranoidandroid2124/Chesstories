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

  test("causal target extraction separates battery witnesses from its exact target"):
    val battery = TransitionConsequence(
      kind = TransitionConsequenceKind.BatteryFormation,
      strength = 1,
      subjectBindings = List(
        StructuralSubjectBinding.unbound(
          StructuralSubject.Battery(
            RelationBatteryFormationWitness(
              White,
              RelationColoredPieceWitness(
                EvidenceSquare("c1"),
                EvidencePieceRole("bishop"),
                White
              ),
              RelationColoredPieceWitness(
                EvidenceSquare("d2"),
                EvidencePieceRole("queen"),
                White
              ),
              RelationAxisSignal.Diagonal
            )
          )
        )
      ),
      targetBindings = List(
        StructuralSubjectBinding.unbound(
          StructuralSubject.PieceAt(Black, EvidencePieceRole("rook"), EvidenceSquare("e3"))
        )
      )
    )
    assertEquals(
      PlanCausalEpisode.consequenceSquares(battery).map(_.key).toSet,
      Set("c1", "d2")
    )
    assertEquals(
      PlanCausalEpisode.consequenceTargetSquares(battery).map(_.key),
      List("e3")
    )

  private def proves(
      kind: PlanKind,
      transition: CertifiedPlanTransition,
      consequence: TransitionConsequence
  ): Boolean =
    PlanCausalGoalProof.proves(
      kind.theme,
      Some(kind),
      transition.binding,
      consequence,
      transition.movement
    )

  private def consequence(
      kind: TransitionConsequenceKind,
      subjects: List[StructuralSubject]
  ): TransitionConsequence =
    TransitionConsequence(
      kind = kind,
      strength = 1,
      subjectBindings = subjects.map(StructuralSubjectBinding.unbound)
    )

  private final case class CertifiedPlanTransition(
      binding: StructuralTransitionBinding,
      movement: CanonicalRootLegalMove
  )

  private def transition(fen: String, move: String): CertifiedPlanTransition =
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .getOrElse(fail("expected a canonical test root"))
    val extended = history
      .extend(List(move))
      .getOrElse(fail(s"expected legal test move $move"))
    val step = extended.segmentReplaySteps.lastOption.getOrElse(fail("expected one legal step"))
    val replay = CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one canonical plan transition"))
    val movement = replay.onlyTransition
      .map(_.relationDelta.rootMove)
      .getOrElse(fail("expected one canonical root movement"))
    CertifiedPlanTransition(
      StructuralTransitionBinding(
        moveUci = move,
        role = TransitionEdgeRole.Reference,
        from = PositionNodeRef(fen, 0, Some(White)),
        to = PositionNodeRef(step.afterFen, 1, Some(chess.Black)),
        line = None,
        perspective = White,
        actorRole = Some(EvidencePieceRole(step.move.piece.role.name))
      ),
      movement
    )
