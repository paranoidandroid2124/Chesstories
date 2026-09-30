package io.chesstory.runtime {

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Canonical legal replay and invented reports test observation and scheduling,
  * not move quality, engine discovery, or the best response to a check.
  */
class CheckResponseInvestigationTest extends munit.FunSuite:
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val ruyPrefix = List("f3e5", "d8d4", "e5f3", "d4e4")
  private def ruyHistory =
    val initial = chess.variant.Standard.initialFen.value
    CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get

  private def packet(history: CanonicalPositionHistory, lines: List[List[String]]) =
    val input = MoveReviewInputAdmission.admitIndependent(history, lines.head.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), lines)
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get

  private def inquiry(source: EvidenceBackedJudgmentPacket, prefix: List[String]) =
    val prepared = ObservedLineProtocol.prepare(source)
    val points = LineContinuationInvestigation.checkResponsePoints(source, prepared)
      .filter(_._1.moves == prefix)
    assert(points.nonEmpty)
    val (path, point) = points.head
    val root = source.assembly.playedTransition.get
    val legal = prepared.resourceViews(source)(point.positionId).legalRows.get
    val question = CommentaryContinuationQuestion(
      CommentarySelection.stableId("line-continuation-question", root.evidence.id :: path.ids),
      root.evidence.id, root.from.id.get, point.positionId, prefix, path.ids,
      Nil, points.map(_._1.line.ref.id).distinct.sorted,
      legal.map(_.move), legal.map(row => row.move -> row.sourceId).toMap,
      points.flatMap((originPath, choice) => choice.premises.map(premise =>
        CommentaryContinuationPremise(premise, List(originPath.line.ref.id)))))
    CommentaryJobReducer.refreshContinuationInquiries(source, prepared, List(question), Nil, Nil).head

  private def start(history: CanonicalPositionHistory, actual: String): AwaitingEngineWork =
    CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen, actual,
      history.extend(List(actual)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get
      .asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile,
      state.issuedWork.workId, state.issuedWork.executionKeySha256,
      rows.map((moves, cp) => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16))), 1L)
      .toOption.get

  test("a quiet one-ply check opens a response inquiry without a material profile"):
    val fen = "4k3/8/8/8/8/8/R7/4K3 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val line = List("a2e2")
    val source = packet(history, List(line))
    val prepared = ObservedLineProtocol.prepare(source)
    assert(prepared.continuationReadings.isEmpty)
    val question = inquiry(source, line)
    assertEquals(question.state, "deferred")
    assertEquals(question.question.kinds, List("check_response_not_observed"))
    assertEquals(question.question.legalMoves.toSet,
      history.extend(line).toOption.get.currentLegalMoveUcis.toSet)
    assert(question.question.legalMoves.size > 1)
    assertEquals(question.unresolvedSourceLineIds.toSet, question.question.sourceLineIds.toSet)
    val next = submit(start(history, line.head), List(line -> 0, List("a2a3") -> -10, List("a2a4") -> -20))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, line)
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(next.continuationInquiries.exists(_.question.kinds.contains("check_response_not_observed")))

  test("zero material balance does not answer the Ruy queen check"):
    val history = ruyHistory
    val source = packet(history, List(ruyPrefix))
    val prepared = ObservedLineProtocol.prepare(source)
    val reading = prepared.continuationReadings.find(_.path.moves == ruyPrefix).get
    assertEquals(reading.openLossEventIds, Set.empty[String])
    assertEquals(history.extend(ruyPrefix).toOption.get.currentLegalMoveUcis.toSet,
      Set("d1e2", "e1f1")) // The f1 bishop was exchanged on c6; Be2 is unavailable.
    val question = inquiry(source, ruyPrefix)
    assertEquals(question.state, "deferred")
    assertEquals(question.question.observedMoves, Nil)
    assert(question.unresolvedSourceLineIds.contains(reading.path.line.ref.id))
    val next = submit(start(history, ruyPrefix.head), List(ruyPrefix -> 0,
      List("e1g1") -> -10, List("d2d4") -> -20)).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, ruyPrefix)

  test("one or two exact longer observations answer an old question without choosing a best reply"):
    val history = ruyHistory
    val before = inquiry(packet(history, List(ruyPrefix)), ruyPrefix)
    for replies <- List(List("d1e2"), List("d1e2", "e1f1")) do
      val source = packet(history, ruyPrefix :: replies.map(ruyPrefix :+ _))
      val prepared = ObservedLineProtocol.prepare(source)
      // No rediscovery is supplied: refreshing the retained question itself
      // must prevent its old deferred/stopped state from being reissued.
      val observed = CommentaryJobReducer.refreshContinuationInquiries(source, prepared, Nil,
        List(before.copy(state = "stopped", stopReason = Some("investigation_limit"))), Nil).head
      assertEquals(observed.state, "observed")
      assertEquals(observed.question.observedMoves.toSet, replies.toSet)
      assertEquals(observed.searchWorkIds, Nil)
      assertEquals(observed.stopReason, None)
      assertEquals(observed.unresolvedSourceLineIds, Nil)
      val extra = before.question.premises.head
      val mixed = before.copy(question = before.question.copy(premises = before.question.premises :+
        extra.copy(origin = extra.origin.copy(kind = "delayed_capturer_removal"))))
      val partial = CommentaryJobReducer.refreshContinuationInquiries(source, prepared, Nil, List(mixed), Nil).head
      assertEquals(partial.state, "deferred")
      assertEquals(partial.premiseObservations.find(_.premise.origin.kind == "check_response_not_observed")
        .get.observationState, "observed")
      assertEquals(partial.premiseObservations.find(_.premise.origin.kind == "delayed_capturer_removal")
        .get.observationState, "not_applicable")
      assertEquals(partial.question.premises.find(_.origin.kind == "check_response_not_observed").get,
        before.question.premises.head)
      assert(partial.premiseObservations.filter(_.observationState == "observed").flatMap(_.witnessLineIds).nonEmpty)

  test("the same moves on another exact history cannot answer the retained check"):
    val history = ruyHistory
    val before = inquiry(packet(history, List(ruyPrefix)), ruyPrefix)
    val otherHistory = CanonicalPositionHistory.from(history.currentFen, Nil, history.currentFen).toOption.get
    val other = packet(otherHistory, List(ruyPrefix, ruyPrefix :+ "d1e2"))
    val prepared = ObservedLineProtocol.prepare(other)
    val path = prepared.paths.find(_.moves == ruyPrefix).get
    assert(path.ids != before.question.prefixOccurrenceIds)
    val untouched = CommentaryJobReducer.refreshContinuationInquiries(other, prepared, Nil, List(before), Nil).head
    assertEquals(untouched.state, "deferred")
    assertEquals(untouched.question.observedMoves, Nil)

  test("missing material evidence cannot suppress an independently witnessed current check"):
    val original = packet(ruyHistory, List(ruyPrefix))
    val source = lila.chessjudgment.testing.CheckResponseEvidenceFixture.withoutMaterial(original)
    val prepared = ObservedLineProtocol.prepare(source)
    assert(prepared.continuationReadings.isEmpty)
    assertEquals(inquiry(source, ruyPrefix).question.legalMoves.toSet, Set("d1e2", "e1f1"))

  test("a legal checking candidate is not a currently checked king"):
    val prefix = ruyPrefix.take(3)
    val source = packet(ruyHistory, List(prefix))
    val prepared = ObservedLineProtocol.prepare(source)
    val path = prepared.paths.find(_.moves == prefix).get
    val view = prepared.resourceViews(source)(path.positions.last.id.get)
    assert(view.checkingMoves.get.exists(_.move == "d4e4"))
    assertEquals(view.currentCheckSources(chess.Color.Black), Nil)
    assert(!LineContinuationInvestigation.checkResponsePoints(source, prepared).exists(_._1.moves == prefix))

  test("the unique reply to the observed queen exchange is replayed without another engine receipt"):
    val history = ruyHistory
    val checked = ruyPrefix ++ List("d1e2", "e4e2")
    assertEquals(history.extend(checked).toOption.get.currentLegalMoveUcis, List("e1e2"))
    val next = submit(start(history, checked.head), List(checked -> 0,
      List("e1g1") -> -10, List("d2d4") -> -20))
    val question = next.continuationInquiries.find(_.question.prefixMoves == checked).get
    assertEquals(question.question.observedMoves, List("e1e2"))
    assertEquals(question.state, "observed")
    assertEquals(question.searchWorkIds, Nil)
    assertEquals(question.unresolvedSourceLineIds, Nil)
    val source = next.observationsPacket.get
    assert(ObservedLineProtocol.prepare(source).paths.exists(_.moves == (checked :+ "e1e2")))
    assertEquals(next.progress.physicalReportsAccepted, 1)
    assert(source.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))

  test("an automatic draw with check and remaining legal moves needs no further response"):
    val fen = "4k3/8/8/8/8/8/R7/4K3 w - - 149 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val line = List("a2e2")
    val end = history.admitToFirstAutomaticTerminal(line).toOption.get
    assertEquals(end.automaticTerminal,
      Some(lila.chessjudgment.model.line.AutomaticTerminal.SeventyFiveMoveRule))
    assert(end.resultingHistory.currentPosition.check.yes)
    assert(end.resultingHistory.currentLegalMoveUcis.nonEmpty)
    val source = packet(history, List(line))
    val prepared = ObservedLineProtocol.prepare(source)
    assertEquals(LineContinuationInvestigation.checkResponsePoints(source, prepared), Nil)

  test("a fifth checked occurrence is already terminal under its exact retained history"):
    val fen = "4k3/8/8/8/8/8/1R6/4K3 w - - 0 1"
    val cycle = List("e8d8", "e2a2", "d8e8", "a2e2")
    val prefix = List("b2e2") ++ List.fill(3)(cycle).flatten ++ cycle.take(3)
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get.extend(prefix).toOption.get
    val line = List("a2e2")
    val end = history.admitToFirstAutomaticTerminal(line).toOption.get
    assertEquals(end.automaticTerminal,
      Some(lila.chessjudgment.model.line.AutomaticTerminal.FivefoldRepetition))
    assert(end.resultingHistory.currentPosition.check.yes)
    assert(end.resultingHistory.currentLegalMoveUcis.nonEmpty)
    val source = packet(history, List(line))
    val prepared = ObservedLineProtocol.prepare(source)
    assertEquals(LineContinuationInvestigation.checkResponsePoints(source, prepared), Nil)
}

package lila.chessjudgment.testing {
  import lila.chessjudgment.model.judgment.*

  /** Remove only the optional ledger; the exact canonical replay stays intact. */
  object CheckResponseEvidenceFixture:
    def withoutMaterial(original: EvidenceBackedJudgmentPacket, lineId: Option[String] = None): EvidenceBackedJudgmentPacket =
      val records = original.evidenceGraph.records.map { record => record.payload match
        case fact: LineFactEvidence if lineId.forall(_ == fact.line.id) => record.copy(payload = fact.copy(material = None))
        case _ => record
      }
      EvidenceBackedJudgmentPacket.fromAssembly(original.assembly.copy(
        evidenceGraph = TypedEvidenceGraph.empty.addAll(records))).get
}
