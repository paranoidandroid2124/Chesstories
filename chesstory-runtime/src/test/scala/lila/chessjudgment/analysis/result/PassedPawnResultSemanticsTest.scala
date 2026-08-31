package lila.chessjudgment.analysis.result

import chess.White
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.PassedPawnResultKind

class PassedPawnResultSemanticsTest extends munit.FunSuite:

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
    assert(proves(PassedPawnResultKind.AdvanceOrPromote, pawnTransition, passerProgress))
    assert(
      !proves(
        PassedPawnResultKind.AdvanceOrPromote,
        pawnTransition,
        consequence(
          TransitionConsequenceKind.PassedPawnProgress,
          List(StructuralSubject.PassedPawnAdvanced(White, EvidenceSquare("a6"), EvidenceSquare("a7"), 6))
        )
      )
    )
    assert(!proves(PassedPawnResultKind.AdvanceOrPromote, pawnTransition, passerCreation))
    assert(!proves(PassedPawnResultKind.Creation, pawnTransition, passerCreation))

    val manufactureTransition = transition(
      "4k3/8/1p6/P7/8/8/8/4K3 w - - 0 1",
      "a5a6"
    )
    val passedStatusCreation = consequence(
      TransitionConsequenceKind.PassedPawnProgress,
      List(
        StructuralSubject.PassedStatusCreated(
          White,
          EvidenceSquare("a5"),
          EvidenceSquare("a6"),
          6
        )
      )
    )
    assert(!proves(PassedPawnResultKind.AdvanceOrPromote, manufactureTransition, passedStatusCreation))
    assert(proves(PassedPawnResultKind.Creation, manufactureTransition, passedStatusCreation))

    val promotionTransition = transition(
      "4k3/P7/8/8/8/8/8/4K3 w - - 0 1",
      "a7a8q"
    )
    assert(
      proves(
        PassedPawnResultKind.AdvanceOrPromote,
        promotionTransition,
        consequence(
          TransitionConsequenceKind.PassedPawnProgress,
          List(StructuralSubject.PassedPawnPromoted(White, EvidenceSquare("a7"), EvidenceSquare("a8")))
        )
      )
    )

  private def proves(
      kind: PassedPawnResultKind,
      transition: CertifiedPassedPawnResultTransition,
      consequence: TransitionConsequence
  ): Boolean =
    val exactConsequence = transition.occurrence.consequences
      .find(candidate =>
        candidate.kind == consequence.kind && candidate.subjectFacts == consequence.subjectFacts
      )
      .getOrElse(consequence)
    PassedPawnResultTransitionProof.proves(
      kind,
      transition.owner,
      transition.occurrence,
      transition.binding,
      exactConsequence,
      transition.movement
    )

  private def consequence(
      kind: TransitionConsequenceKind,
      subjects: List[StructuralSubject]
  ): TransitionConsequence =
    TransitionConsequence(
      kind = kind,
      subjectBindings = subjects.map(StructuralSubjectBinding(_, Nil))
    )

  private final case class CertifiedPassedPawnResultTransition(
      binding: StructuralTransitionBinding,
      movement: CanonicalRootLegalMove,
      owner: EvidenceRef,
      occurrence: ReplayStructuralOccurrence
  )

  private def transition(fen: String, move: String): CertifiedPassedPawnResultTransition =
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .getOrElse(fail("expected a canonical test root"))
    val extended = history
      .extend(List(move))
      .getOrElse(fail(s"expected legal test move $move"))
    val step = extended.segmentReplaySteps.lastOption.getOrElse(fail("expected one legal step"))
    val replay = CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one canonical passed-pawn-result transition"))
    val movement = replay.onlyTransition
      .map(_.relationDelta.rootMove)
      .getOrElse(fail("expected one canonical root movement"))
    val line = LineNodeRef(
      s"passed-pawn-result-semantics:$move",
      move,
      1,
      LineNodeRole.BestReference
    )
    val binding = StructuralTransitionBinding(
      moveUci = move,
      role = TransitionEdgeRole.Reference,
      from = PositionNodeRef(fen, 0, Some(White)),
      to = PositionNodeRef(step.afterFen, 1, Some(chess.Black)),
      line = Some(line),
      perspective = White,
      actorRole = Some(EvidencePieceRole(step.move.piece.role.name))
    )
    val occurrence = replay
      .structuralOccurrence(replay.replaySteps.head)
      .getOrElse(fail("expected one replay-owned structural occurrence"))
    CertifiedPassedPawnResultTransition(
      binding,
      movement,
      EvidenceRef(
        s"passed-pawn-result-semantics-owner:$move",
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        binding.from,
        Some(line),
        line.role.scope,
        EvidenceConfidence.LegalReplayVerified
      ),
      occurrence
    )
