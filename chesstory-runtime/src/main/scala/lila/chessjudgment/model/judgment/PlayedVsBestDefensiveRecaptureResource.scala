package lila.chessjudgment.model.judgment

import chess.{ Color, Queen, Role, Rook, Square }
import chess.format.Fen
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Comparison-owned proof that the reference root creates one exact legal
  * recapture option which preserves the important defender used by the played
  * continuation.
  *
  * The candidate continuation is used only to establish relevance. This fact
  * does not claim that the reference prevents or forces that continuation, nor
  * that the resource alone explains the evaluation difference.
  */
final case class PlayedVsBestDefensiveRecaptureResource(
    target: EvidenceSquare,
    removedOccupantRole: EvidencePieceRole,
    referenceDefenderRole: EvidencePieceRole,
    preservedDefenderRole: EvidencePieceRole,
    referenceRootMove: String,
    opponentCaptureMove: String,
    referenceRecaptureMove: String
):
  require(
    List(referenceRootMove, opponentCaptureMove, referenceRecaptureMove).forall(_.nonEmpty),
    "a defensive recapture resource needs an exact reference move chain"
  )

  def referenceProofMoves: List[String] =
    List(referenceRootMove, opponentCaptureMove, referenceRecaptureMove)

object PlayedVsBestDefensiveRecaptureResource:
  private val ImportantDefenderRoles: Set[Role] = Set(Rook, Queen)

  def derive(
      comparison: CandidateComparisonFact,
      root: PositionNodeRef,
      candidateLine: LineFactEvidence
  ): Option[PlayedVsBestDefensiveRecaptureResource] =
    val steps = candidateLine.lineReplaySteps.take(5)
    for
      _ <- Option.when(
        comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          comparison.comparison.verdict.isActionableLoss &&
          comparison.hasDistinctRootMoves &&
          candidateLine.line == comparison.candidateLine &&
          steps.size == 5 &&
          EvidenceRef.sameMove(steps.head.moveUci, comparison.candidateLine.rootMove) &&
          exactContinuousLegalReplay(root, steps)
      )(() )
      rootBefore <- position(root.fen)
      mover = comparison.comparison.mover
      if rootBefore.color == mover
      referenceAfterFen <- PrincipalVariationEvidence.legalFenAfter(
        root.fen,
        comparison.referenceLine.rootMove
      )
      referenceAfter <- position(referenceAfterFen)
      candidateAfter <- position(steps(0).fenAfter)
      referenceFrom <- squareFrom(comparison.referenceLine.rootMove)
      referenceTo <- squareTo(comparison.referenceLine.rootMove)
      referenceDefender <- rootBefore.board.pieceAt(referenceFrom)
      if referenceDefender.color == mover
      if referenceAfter.board.pieceAt(referenceTo).contains(referenceDefender)
      captureFrom <- squareFrom(steps(1).moveUci)
      target <- squareTo(steps(1).moveUci)
      removedOccupant <- candidateAfter.board.pieceAt(target)
      if removedOccupant.color == mover
      if rootBefore.board.pieceAt(target).contains(removedOccupant)
      if referenceAfter.board.pieceAt(target).contains(removedOccupant)
      opponentCapturer <- candidateAfter.board.pieceAt(captureFrom)
      if opponentCapturer.color == !mover
      afterCapture <- position(steps(1).fenAfter)
      if afterCapture.board.pieceAt(target).contains(opponentCapturer)
      candidateRecaptureFrom <- squareFrom(steps(2).moveUci)
      candidateRecaptureTo <- squareTo(steps(2).moveUci)
      if candidateRecaptureTo == target && candidateRecaptureFrom != target
      candidateRecapturer <- afterCapture.board.pieceAt(candidateRecaptureFrom)
      if candidateRecapturer.color == mover && ImportantDefenderRoles(candidateRecapturer.role)
      if candidateAfter.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer)
      if candidateAfter.board.attackers(target, mover).squares.contains(candidateRecaptureFrom)
      if referenceUniquelyAddsRecapture(
        rootBefore,
        referenceAfterFen,
        referenceAfter,
        candidateAfter,
        referenceFrom,
        referenceTo,
        referenceDefender,
        target,
        steps(1).moveUci,
        candidateRecaptureFrom,
        candidateRecapturer
      )
      afterCandidateRecapture <- position(steps(2).fenAfter)
      if afterCandidateRecapture.board.pieceAt(target).contains(candidateRecapturer)
      quietFrom <- squareFrom(steps(3).moveUci)
      quietTo <- squareTo(steps(3).moveUci)
      quietAttacker <- afterCandidateRecapture.board.pieceAt(quietFrom)
      if quietAttacker.color == !mover
      if afterCandidateRecapture.board.pieceAt(quietTo).isEmpty
      afterQuiet <- position(steps(3).fenAfter)
      if !afterQuiet.check.yes
      if afterQuiet.board.pieceAt(quietTo).contains(quietAttacker)
      if afterQuiet.board.pieceAt(target).contains(candidateRecapturer)
      if !afterCandidateRecapture.board.attackers(target, !mover).squares.contains(quietFrom)
      if afterQuiet.board.attackers(target, !mover).squares.contains(quietTo)
      displacedFrom <- squareFrom(steps(4).moveUci)
      displacedTo <- squareTo(steps(4).moveUci)
      if displacedFrom == target && displacedTo != target
      if afterQuiet.board.pieceAt(displacedFrom).contains(candidateRecapturer)
      afterDisplacement <- position(steps(4).fenAfter)
      if afterDisplacement.board.pieceAt(displacedTo).contains(candidateRecapturer)
      if afterDisplacement.board.pieceAt(target).isEmpty
    yield PlayedVsBestDefensiveRecaptureResource(
      target = EvidenceSquare(target.key),
      removedOccupantRole = EvidencePieceRole(removedOccupant.role.name),
      referenceDefenderRole = EvidencePieceRole(referenceDefender.role.name),
      preservedDefenderRole = EvidencePieceRole(candidateRecapturer.role.name),
      referenceRootMove = EvidenceRef.normalizeMove(comparison.referenceLine.rootMove),
      opponentCaptureMove = EvidenceRef.normalizeMove(steps(1).moveUci),
      referenceRecaptureMove = s"${referenceTo.key}${target.key}"
    )

  def proves(
      comparison: CandidateComparisonFact,
      root: PositionNodeRef,
      candidateLine: LineFactEvidence,
      resource: PlayedVsBestDefensiveRecaptureResource
  ): Boolean =
    comparison.defensiveRecaptureResource.contains(resource) &&
      derive(comparison.copy(defensiveRecaptureResource = None), root, candidateLine).contains(resource)

  private def referenceUniquelyAddsRecapture(
      rootBefore: chess.Position,
      referenceAfterFen: String,
      referenceAfter: chess.Position,
      candidateAfter: chess.Position,
      referenceFrom: Square,
      referenceTo: Square,
      referenceDefender: chess.Piece,
      target: Square,
      opponentCaptureMove: String,
      candidateRecaptureFrom: Square,
      candidateRecapturer: chess.Piece
  ): Boolean =
    val mover = referenceDefender.color
    val beforeDefenders = rootBefore.board.attackers(target, mover).squares
    val referenceDefenders = referenceAfter.board.attackers(target, mover).squares
    val candidateDefenders = candidateAfter.board.attackers(target, mover).squares
    val referenceRecapture = s"${referenceTo.key}${target.key}"
    val exactReferenceResource =
      !beforeDefenders.contains(referenceFrom) &&
        referenceDefenders.contains(referenceTo) &&
        candidateAfter.board.pieceAt(referenceFrom).contains(referenceDefender) &&
        !candidateDefenders.contains(referenceFrom) &&
        !candidateDefenders.contains(referenceTo) &&
        referenceFrom != candidateRecaptureFrom &&
        rootBefore.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer) &&
        referenceAfter.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer)
    val legalReferenceRecapture =
      PrincipalVariationEvidence
        .legalFenAfter(referenceAfterFen, opponentCaptureMove)
        .flatMap(captureFen =>
          position(captureFen).filter(
            _.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer)
          ).flatMap(_ =>
            PrincipalVariationEvidence
              .legalFenAfter(captureFen, referenceRecapture)
              .flatMap(position)
          )
        )
        .exists(afterReferenceRecapture =>
          afterReferenceRecapture.board.pieceAt(target).contains(referenceDefender) &&
            afterReferenceRecapture.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer)
        )
    exactReferenceResource && legalReferenceRecapture

  private def exactContinuousLegalReplay(
      root: PositionNodeRef,
      steps: List[LineReplayStep]
  ): Boolean =
    val exactRoot = steps.headOption.exists(step =>
      PrincipalVariationEvidence.sameBoardState(step.fenBefore, root.fen)
    )
    val continuous = steps.sliding(2).forall {
      case List(left, right) =>
        right.ply == left.ply + 1 &&
          PrincipalVariationEvidence.sameBoardState(left.fenAfter, right.fenBefore)
      case _ => true
    }
    val legal = steps.forall(step =>
      PrincipalVariationEvidence
        .legalFenAfter(step.fenBefore, step.moveUci)
        .exists(PrincipalVariationEvidence.sameBoardState(_, step.fenAfter))
    )
    exactRoot && continuous && legal

  private def position(fen: String): Option[chess.Position] =
    Fen.read(chess.variant.Standard, Fen.Full(fen))

  private def squareFrom(move: String): Option[Square] =
    Square.fromKey(EvidenceRef.normalizeMove(move).take(2))

  private def squareTo(move: String): Option[Square] =
    Square.fromKey(EvidenceRef.normalizeMove(move).slice(2, 4))
