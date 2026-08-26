package lila.chessjudgment.model.judgment

import chess.Square
import lila.chessjudgment.analysis.position.PositionAnalysis
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Comparison-owned proof that the reference root creates one exact legal
  * recapture option which preserves the alternative recapturer used by the played
  * continuation.
  *
  * The candidate continuation is used only to establish relevance. This fact
  * does not claim that the reference prevents or forces that continuation, nor
  * that the resource alone explains the evaluation difference.
  */
final case class PlayedVsBestDefensiveRecaptureResource private[chessjudgment] (
    target: EvidenceSquare,
    removedOccupantRole: EvidencePieceRole,
    referenceRecapturerRole: EvidencePieceRole,
    preservedAlternativeRecapturerRole: EvidencePieceRole,
    referenceRootMove: String,
    opponentCaptureMove: String,
    referenceRecaptureMove: String,
    private[chessjudgment] val playedDisplacementContact: ReplayRelationChangeWitness
):
  require(
    List(referenceRootMove, opponentCaptureMove, referenceRecaptureMove).forall(_.nonEmpty),
    "a defensive recapture resource needs an exact reference move chain"
  )

  def referenceProofMoves: List[String] =
    List(referenceRootMove, opponentCaptureMove, referenceRecaptureMove)

object PlayedVsBestDefensiveRecaptureResource:
  def derive(
      comparison: CandidateComparisonFact,
      root: PositionNodeRef,
      candidateLine: LineFactEvidence,
      referenceLine: LineFactEvidence
  ): Option[PlayedVsBestDefensiveRecaptureResource] =
    val steps = candidateLine.lineReplaySteps.take(5)
    for
      candidateReplay <- candidateLine.certifiedReplay
      referenceReplay <- referenceLine.certifiedReplay
      candidateLegal = candidateReplay.legalSteps.take(5)
      referenceReplayStep <- referenceReplay.replaySteps.headOption
      referenceRoot <- referenceReplay.legalSteps.headOption
      _ <- Option.when(
        comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          comparison.comparison.verdict.isActionableLoss &&
          comparison.hasDistinctRootMoves &&
          candidateLine.line == comparison.candidateLine &&
          referenceLine.line == comparison.referenceLine &&
          steps.size == 5 &&
          candidateLegal.size == steps.size &&
          EvidenceRef.sameMove(steps.head.moveUci, comparison.candidateLine.rootMove) &&
          EvidenceRef.sameMove(referenceRoot.uci, comparison.referenceLine.rootMove) &&
          candidateReplay.matches(candidateLine.lineReplaySteps) &&
          referenceReplay.matches(referenceLine.lineReplaySteps) &&
          PrincipalVariationEvidence.sameBoardState(steps.head.fenBefore, root.fen) &&
          PrincipalVariationEvidence.sameBoardState(
            _root_.chess.format.Fen.write(referenceRoot.before).value,
            root.fen
          )
      )(() )
      rootBeforeAnalysis <- candidateReplay.analysisBefore(steps.head)
      referenceAfterAnalysis <- referenceReplay.analysisAfter(referenceReplayStep)
      candidateAfterAnalysis <- candidateReplay.analysisAfter(steps.head)
      rootBefore = rootBeforeAnalysis.position
      mover = comparison.comparison.mover
      if rootBefore.color == mover
      referenceAfter = referenceAfterAnalysis.position
      candidateAfter = candidateAfterAnalysis.position
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
      afterCapture = candidateLegal(1).after
      if afterCapture.board.pieceAt(target).contains(opponentCapturer)
      candidateRecaptureFrom <- squareFrom(steps(2).moveUci)
      candidateRecaptureTo <- squareTo(steps(2).moveUci)
      if candidateRecaptureTo == target && candidateRecaptureFrom != target
      if candidateLegal(2).move.captures
      candidateRecapturer <- afterCapture.board.pieceAt(candidateRecaptureFrom)
      if candidateRecapturer.color == mover
      if candidateAfter.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer)
      if referenceUniquelyAddsRecapture(
        rootBeforeAnalysis,
        referenceAfterAnalysis,
        candidateAfterAnalysis,
        referenceFrom,
        referenceTo,
        referenceDefender,
        target,
        steps(1).moveUci,
        candidateLegal(1).move,
        candidateLegal(2).move,
        candidateRecaptureFrom,
        candidateRecapturer
      )
      afterCandidateRecaptureAnalysis <- candidateReplay.analysisAfter(steps(2))
      afterCandidateRecapture = afterCandidateRecaptureAnalysis.position
      if afterCandidateRecapture.board.pieceAt(target).contains(candidateRecapturer)
      quietFrom <- squareFrom(steps(3).moveUci)
      quietTo <- squareTo(steps(3).moveUci)
      quietAttacker <- afterCandidateRecapture.board.pieceAt(quietFrom)
      if quietAttacker.color == !mover
      if afterCandidateRecapture.board.pieceAt(quietTo).isEmpty
      afterQuietAnalysis <- candidateReplay.analysisAfter(steps(3))
      afterQuiet = afterQuietAnalysis.position
      if !afterQuiet.check.yes
      if afterQuiet.board.pieceAt(quietTo).contains(quietAttacker)
      if afterQuiet.board.pieceAt(target).contains(candidateRecapturer)
      displacementContact <- candidateReplay.transition(steps(3)).toList
        .flatMap(_.relationDelta.established)
        .find(_.detail match
          case RelationWitnessDetail.GeometricControl(
                owner,
                attacker,
                attackerRole,
                contacted,
                RelationControlTarget.Enemy(contactedRole)
              ) =>
            owner == !mover &&
              attacker.key.equalsIgnoreCase(quietTo.key) &&
              attackerRole == EvidencePieceRole(quietAttacker.role.name) &&
              contacted.key.equalsIgnoreCase(target.key) &&
              contactedRole == EvidencePieceRole(candidateRecapturer.role.name)
          case _ => false
        )
      displacementContactWitness <- ReplayRelationChangeWitness.certify(
        candidateReplay,
        steps(3),
        displacementContact
      )
      displacedFrom <- squareFrom(steps(4).moveUci)
      displacedTo <- squareTo(steps(4).moveUci)
      if displacedFrom == target && displacedTo != target
      if afterQuiet.board.pieceAt(displacedFrom).contains(candidateRecapturer)
      afterDisplacement = candidateLegal(4).after
      if afterDisplacement.board.pieceAt(displacedTo).contains(candidateRecapturer)
      if afterDisplacement.board.pieceAt(target).isEmpty
    yield PlayedVsBestDefensiveRecaptureResource(
      target = EvidenceSquare(target.key),
      removedOccupantRole = EvidencePieceRole(removedOccupant.role.name),
      referenceRecapturerRole = EvidencePieceRole(referenceDefender.role.name),
      preservedAlternativeRecapturerRole = EvidencePieceRole(candidateRecapturer.role.name),
      referenceRootMove = EvidenceRef.normalizeMove(comparison.referenceLine.rootMove),
      opponentCaptureMove = EvidenceRef.normalizeMove(steps(1).moveUci),
      referenceRecaptureMove = s"${referenceTo.key}${target.key}",
      playedDisplacementContact = displacementContactWitness
    )

  def proves(
      comparison: CandidateComparisonFact,
      root: PositionNodeRef,
      candidateLine: LineFactEvidence,
      referenceLine: LineFactEvidence,
      resource: PlayedVsBestDefensiveRecaptureResource
  ): Boolean =
    comparison.defensiveRecaptureResource.contains(resource) &&
      derive(
        comparison.copy(defensiveRecaptureResource = None),
        root,
        candidateLine,
        referenceLine
      ).contains(resource)

  private def referenceUniquelyAddsRecapture(
      rootBefore: PositionAnalysis,
      referenceAfter: PositionAnalysis,
      candidateAfter: PositionAnalysis,
      referenceFrom: Square,
      referenceTo: Square,
      referenceDefender: chess.Piece,
      target: Square,
      opponentCaptureMove: String,
      candidateCapture: chess.Move,
      candidateRecapture: chess.Move,
      candidateRecaptureFrom: Square,
      candidateRecapturer: chess.Piece
  ): Boolean =
    val referenceRecapture = s"${referenceTo.key}${target.key}"
    val exactReferenceResource =
      rootBefore.position.board.pieceAt(referenceFrom).contains(referenceDefender) &&
        !rootBefore.position.board.pieceAt(referenceTo).contains(referenceDefender) &&
        referenceAfter.position.board.pieceAt(referenceTo).contains(referenceDefender) &&
        candidateAfter.position.board.pieceAt(referenceFrom).contains(referenceDefender) &&
        !candidateAfter.position.board.pieceAt(referenceTo).contains(referenceDefender) &&
        referenceFrom != candidateRecaptureFrom &&
        rootBefore.position.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer) &&
        referenceAfter.position.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer) &&
        EvidenceRef.sameMove(candidateCapture.toUci.uci, opponentCaptureMove) &&
        candidateCapture.before == candidateAfter.position &&
        candidateRecapture.before == candidateCapture.after &&
        candidateRecapture.orig == candidateRecaptureFrom &&
        candidateRecapture.dest == target &&
        candidateRecapture.captures
    val candidateDoesNotHaveReferenceRecapture =
      PrincipalVariationEvidence.actualLegalMoves(candidateCapture.after)
        .forall(move => !EvidenceRef.sameMove(move.toUci.uci, referenceRecapture))
    val legalReferenceRecapture =
      referenceAfter.actualLegalMoves
        .find(move => EvidenceRef.sameMove(move.toUci.uci, opponentCaptureMove))
        .filter(_.after.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer))
        .flatMap(capture =>
          PrincipalVariationEvidence.actualLegalMoves(capture.after).find(move =>
            EvidenceRef.sameMove(move.toUci.uci, referenceRecapture)
          )
        )
        .exists(recapture =>
          val afterReferenceRecapture = recapture.after
          afterReferenceRecapture.board.pieceAt(target).contains(referenceDefender) &&
          afterReferenceRecapture.board.pieceAt(candidateRecaptureFrom).contains(candidateRecapturer)
        )
    exactReferenceResource && candidateDoesNotHaveReferenceRecapture && legalReferenceRecapture

  private def squareFrom(move: String): Option[Square] =
    Square.fromKey(EvidenceRef.normalizeMove(move).take(2))

  private def squareTo(move: String): Option[Square] =
    Square.fromKey(EvidenceRef.normalizeMove(move).slice(2, 4))
