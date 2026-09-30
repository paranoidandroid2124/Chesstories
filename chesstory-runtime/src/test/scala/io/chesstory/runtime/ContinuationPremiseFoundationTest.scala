package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Supplied legal continuations test requirement identity and partial evidence,
  * not engine discovery, move strength, or forced support use.
  */
class ContinuationPremiseFoundationTest extends munit.FunSuite:
  test("a stopped merged support inquiry retains each origin and partial witness across rediscovery"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val prefix = List("g1h1", "d5e4")
    val donor = prefix :+ "d2e4"
    val peer = prefix :+ "f3e4"
    val firstContact = List("h8g7", "h1g2", "g7g6", "g2h2")
    val observation = donor ++ firstContact ++ List("g6f5", "h2h1")
    val peerWitness = peer ++ firstContact

    def requirements(extra: List[List[String]]) =
      val lines = List(donor, observation, peer) ++ extra
      val input = MoveReviewInputAdmission.admitIndependent(history, prefix.head,
        suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
      assertEquals(input.suppliedLines.map(_.acceptedMoves), lines)
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
      val prepared = ObservedLineProtocol.prepare(packet)
      def path(moves: List[String]) = prepared.continuationReadings.find(_.path.moves == moves).get.path
      val source = LineContinuationInvestigation.ComparisonSource("supplied-support-comparison",
        path(donor).line.ref.id, path(peer).line.ref.id, "support_application", Some(path(observation).line.ref.id))
      val values = LineContinuationInvestigation.continuationRequirements(packet, peer, List(source), prepared).get
      val root = packet.assembly.playedTransition.get
      val peerPath = path(peer)
      val position = peerPath.positions.last.id.get
      val legal = prepared.resourceViews(packet)(position).legalRows.get
      val question = CommentaryContinuationQuestion(
        CommentarySelection.stableId("line-continuation-question", root.evidence.id :: peerPath.ids),
        root.evidence.id, root.from.id.get, position, peer, peerPath.ids,
        Nil, List(peerPath.line.ref.id), legal.map(_.move), legal.map(row => row.move -> row.sourceId).toMap,
        values.map(value => CommentaryContinuationPremise(LineContinuationInvestigation.ChoicePremise(
          "support_application_not_observed", value.triggerIds, List(source), Some(value.id)), List(peerPath.line.ref.id))))
      (packet, prepared, source, values, path(observation), question)

    val (beforePacket, beforePrepared, beforeSource, before, _, beforeQuestion) = requirements(Nil)
    val (packet, prepared, afterSource, after, observedPath, afterQuestion) = requirements(List(peerWitness))
    assertEquals(before.size, 2)
    assertEquals(before.map(_.id).distinct.size, 2)
    assert(before.forall(_.witnessLineIds.isEmpty))
    assertEquals(afterSource, beforeSource)
    assertEquals(after.map(value => value.id -> value.triggerIds), before.map(value => value.id -> value.triggerIds))

    val witnessed = after.filter(_.witnessLineIds.nonEmpty)
    val missing = after.filter(_.witnessLineIds.isEmpty)
    assertEquals(witnessed.size, 1)
    assertEquals(missing.size, 1)
    assert(witnessed.head.triggerIds.contains(observedPath.ids(donor.size + firstContact.size - 1)))
    assert(missing.head.triggerIds.contains(observedPath.ids.last))
    val witnessId = prepared.continuationReadings.find(_.path.moves == peerWitness).get.path.line.ref.id
    assertEquals(witnessed.head.witnessLineIds, List(witnessId))
    assertEquals(LineContinuationInvestigation.observedContinuations(packet, peer, List(afterSource), prepared), None)

    val stopped = CommentaryJobReducer.refreshContinuationInquiries(beforePacket, beforePrepared,
      List(beforeQuestion), Nil, Nil).head.copy(state = "stopped", stopReason = Some("cancelled"))
    assert(stopped.premiseObservations.forall(_.observationState == "unobserved"))
    // Discovery now reports only the still-missing requirement. The old observed
    // origin must survive that merge; its new witness is not a discovery source.
    val rediscovered = afterQuestion.copy(premises = afterQuestion.premises.filter(_.origin.requirementId.contains(missing.head.id)))
    val refreshed = CommentaryJobReducer.refreshContinuationInquiries(packet, prepared,
      List(rediscovered), List(stopped), Nil).head
    assertEquals(refreshed.state, "stopped")
    assertEquals(refreshed.stopReason, Some("cancelled"))
    assertEquals(refreshed.question.premises.map(_.id(refreshed.question.id)).toSet,
      stopped.question.premises.map(_.id(stopped.question.id)).toSet)
    assertEquals(refreshed.premiseObservations.map(value => value.premise.origin.requirementId.get -> value.observationState).toMap,
      Map(witnessed.head.id -> "observed", missing.head.id -> "unobserved"))
    assert(refreshed.question.sourceLineIds.contains(witnessId))
    assert(refreshed.question.premises.forall(_.sourceLineIds == List(afterSource.peerLineId)))
    val retained = refreshed.premiseObservations.find(_.premise.origin.requirementId.contains(witnessed.head.id)).get
    assertEquals(retained.witnessLineIds, List(witnessId))
    assertEquals(retained.unresolvedSourceLineIds, Nil)
    assert(refreshed.premiseObservations.find(_.premise.origin.requirementId.contains(missing.head.id)).get
      .unresolvedSourceLineIds.contains(afterSource.peerLineId))
