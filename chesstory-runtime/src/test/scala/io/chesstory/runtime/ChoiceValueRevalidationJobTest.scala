package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Legal composed continuations and invented values isolate the reducer's
  * conflict agenda. These are not an engine study or a claim about the book
  * endgame's value, best defense, or the original move's full explanation.
  */
class ChoiceValueRevalidationJobTest extends munit.FunSuite:
  import CommentaryProposal.*

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val spend = "g3g4"
  private val keep = "f3e3"
  private val at = List(keep, "e5e6")
  private val plan = "e3f2"
  private val left = "e3f2 e6f6 f2e3 f6e5 e3f3 e5e6".split(" ").toList
  private val right = "e3f2 e6f6 f2e3 f6e5 e3f3 e5f6".split(" ").toList
  private val divergence = at ++ left.take(5)

  private def waiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case active: AwaitingEngineWork => active
    case other => fail(s"expected an independently owned next work, got ${other.getClass.getSimpleName}")

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth)
      }), state.progress.physicalReportsAccepted.toLong + 1L).toOption.get

  private def start(): AwaitingEngineWork =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    waiting(CommentaryJobReducer.startJob(fen, Nil, fen, spend,
      history.extend(List(spend)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 180000L, profile, investigateChoices = true,
        previewDepth = Some(8)), 0L).toOption.get)

  private val roots = List((at ++ left) -> 221,
    List(spend, "f7f6", "f3e3") -> 0, List("f3e2") -> -500)

  private def reachOwnPair(): AwaitingEngineWork =
    var state: CommentaryJobState = submit(start(), roots)
    state = submit(waiting(state), roots)
    var steps = 0
    while waiting(state).issuedWork.searchAnchor.map(_.prefixMoves) != Some(at) && steps < 12 do
      val active = waiting(state)
      val prefix = active.issuedWork.searchAnchor.get.prefixMoves
      val rows = if prefix == List(spend) then
        List(List("f7f6") -> 0, List("e5e6") -> 400, List("e5d6") -> 500)
      else if prefix == List(keep) then
        List(("e5e6" :: left) -> 221, List("f7f6") -> 500, List("e5d6") -> 600)
      else fail(s"the current root defense precedes optional work: $prefix")
      val admitted = active.issuedWork.rootRestriction match
        case EngineRootRestriction.Unrestricted => rows
        case EngineRootRestriction.Restricted(moves) => moves.map(move =>
          rows.find(_._1.headOption.contains(move)).getOrElse(fail(s"missing owned root defense: $move")))
      state = submit(active, admitted)
      steps += 1
    val own = waiting(state)
    assertEquals(own.issuedWork.searchAnchor.map(_.prefixMoves), Some(at))
    assertEquals(own.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    waiting(submit(own, List(left -> 541, List("e3e2") -> 100, List("e3f4") -> 90)))

  private def valueHeads(state: CommentaryJobState) = state.proposalEntries.flatMap(entry =>
    entry.request.requirements.collect {
      case value: CompareMoves if value.id.startsWith(AnchoredChoiceReading.ValuePrefix) &&
        value.prefixMovesUci == at && value.referenceMove == spend && value.alternativeMove == plan =>
          (entry, value, entry.readings.find(_.requirementId == value.id).get)
    })

  test("a live pool/pair disagreement precedes optional discovery and retains the new child's issued owner"):
    val ownPair = reachOwnPair()
    assertEquals(ownPair.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List(spend, plan)))
    val before = ownPair.observationsPacket.get.assembly.input.assessmentCandidates
    val discovery = ownPair.discoveryState
    val child = waiting(submit(ownPair, List(List(spend) -> 0, right -> 315)))
    assertEquals(child.issuedWork.searchAnchor.map(_.prefixMoves), Some(divergence))
    assertEquals(child.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e5e6", "e5f6")))
    assert(child.issuedWork.requiredDepth >= ownPair.issuedWork.requiredDepth)
    assertEquals(child.observationsPacket.get.assembly.input.assessmentCandidates, before,
      "contradictory local values never replace the original numerical assessment")
    assertEquals(child.discoveryState, discovery,
      "optional discovery cannot consume the current dependency's remaining slots")
    assert(valueHeads(child).nonEmpty)
    assert(valueHeads(child).forall(_._3.answer.isEmpty), "an unresolved value is not a Checked answer")
    val required = child.proposalEntries.filter(_.request.origin.question.producer == "C-comparison-conflict")
      .flatMap(entry => entry.readings.map(entry -> _)).filter((_, reading) =>
        reading.state == "awaiting_engine" && reading.workId.contains(child.issuedWork.workId))
    assert(required.nonEmpty)
    assert(required.forall((entry, reading) => child.issuedWork.proposalRefs.contains(entry.request.id -> reading.requirementId)))
    assert(child.proposalEntries.map(_.request.requirements.size).sum < CommentaryProposal.MaximumRequirements)

  test("an answered divergent child renews the full parent pool and an omitted resource still needs a fresh pair"):
    val ownPair = reachOwnPair()
    val rootOwner = ownPair.observationsPacket.get.assembly.input.assessmentCandidates
    val child = waiting(submit(ownPair, List(List(spend) -> 0, right -> 315)))
    // Black's stronger divergent reply is the one already in the original
    // root forecast. This isolates parent revalidation from the independent
    // root child-challenge verification, which has its own existing duties.
    val renewed = waiting(submit(child, List(List("e5e6") -> 315, List("e5f6") -> 541)))
    assertEquals(renewed.issuedWork.searchAnchor.map(_.prefixMoves), Some(at))
    assertEquals(renewed.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(renewed.issuedWork.requiredDepth, ownPair.issuedWork.requiredDepth + 1,
      "a contradictory parent needs a deeper full measurement after its divergent child")
    assertEquals(renewed.observationsPacket.get.assembly.input.assessmentCandidates, rootOwner)
    val fresh = waiting(submit(renewed, List(right -> 315, List("e3e2") -> 0, List("e3f4") -> -10)))
    assertEquals(fresh.issuedWork.searchAnchor.map(_.prefixMoves), Some(at),
      s"restriction=${fresh.issuedWork.rootRestriction}; purpose=${fresh.issuedWork.purpose}; " +
        s"heads=${valueHeads(fresh).map((_, question, reading) => (question.id, reading.state, reading.stopReason, reading.workId))}; " +
        s"verification=${fresh.verification}")
    assertEquals(fresh.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List(spend, plan)))
    assertEquals(fresh.issuedWork.requiredDepth, renewed.issuedWork.requiredDepth)
    assertNotEquals(fresh.issuedWork.executionKeySha256, ownPair.issuedWork.executionKeySha256)
    val current = valueHeads(fresh).filter(_._3.state == "awaiting_engine")
    assert(current.nonEmpty)
    assert(current.forall((_, _, reading) => reading.answer.isEmpty &&
      reading.workId.contains(fresh.issuedWork.workId)))
    assert(valueHeads(fresh).filterNot(current.contains).forall((_, _, reading) =>
      reading.state == "stopped" && reading.stopReason.contains("not_required_for_decision") && reading.answer.isEmpty),
      "old exact questions remain historical; their lower-depth pairs cannot own the new value")
    val after = submit(fresh, List(List(spend) -> 0, right -> 315))
    val heads = valueHeads(after)
    assert(heads.exists((_, _, reading) => reading.answer.exists(answer =>
      answer.workId == fresh.issuedWork.workId && answer.executionKeySha256 == fresh.issuedWork.executionKeySha256)))
    assertEquals(after.observationsPacket.get.assembly.input.assessmentCandidates, rootOwner)
    assert(after.responseSearches.exists(_.work == ownPair.issuedWork), "old receipts remain immutable observations")

  test("a repeated same-parent conflict advances depth again instead of looping at the old horizon"):
    val ownPair = reachOwnPair()
    val rootOwner = ownPair.observationsPacket.get.assembly.input.assessmentCandidates
    val child = waiting(submit(ownPair, List(List(spend) -> 0, right -> 315)))
    val firstRenewal = waiting(submit(child, List(List("e5e6") -> 315, List("e5f6") -> 541)))
    val fresh = waiting(submit(firstRenewal, List(left -> 600, List("e3e2") -> 100, List("e3f4") -> 90)))
    val secondChild = waiting(submit(fresh, List(List(spend) -> 0, right -> 315)))
    assertEquals(secondChild.issuedWork.searchAnchor.map(_.prefixMoves), Some(divergence))
    assertEquals(secondChild.issuedWork.requiredDepth, firstRenewal.issuedWork.requiredDepth)
    assertNotEquals(secondChild.issuedWork.executionKeySha256, child.issuedWork.executionKeySha256)
    val secondRenewal = waiting(submit(secondChild, List(List("e5e6") -> 315, List("e5f6") -> 600)))
    assertEquals(secondRenewal.issuedWork.searchAnchor.map(_.prefixMoves), Some(at))
    assertEquals(secondRenewal.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(secondRenewal.issuedWork.requiredDepth, firstRenewal.issuedWork.requiredDepth + 1)
    assertEquals(secondRenewal.observationsPacket.get.assembly.input.assessmentCandidates, rootOwner)
    assert(List(ownPair.issuedWork, child.issuedWork, firstRenewal.issuedWork, fresh.issuedWork,
      secondChild.issuedWork).forall(work => secondRenewal.responseSearches.exists(_.work == work)),
      "progress in depth cannot overwrite any contradictory or verification receipt")

  test("a still inconsistent depth18 parent stays unresolved instead of issuing depth19"):
    val ownPair = reachOwnPair()
    val originalAssessment = ownPair.observationsPacket.get.assembly.input.assessmentCandidates
    val child16 = waiting(submit(ownPair, List(List(spend) -> 0, right -> 315)))
    val pool17 = waiting(submit(child16, List(List("e5e6") -> 315, List("e5f6") -> 541)))
    val pair17 = waiting(submit(pool17, List(left -> 600, List("e3e2") -> 100, List("e3f4") -> 90)))
    val child17 = waiting(submit(pair17, List(List(spend) -> 0, right -> 315)))
    val pool18 = waiting(submit(child17, List(List("e5e6") -> 315, List("e5f6") -> 600)))
    assertEquals(pool18.issuedWork.requiredDepth, 18)
    val pair18 = waiting(submit(pool18, List(left -> 600, List("e3e2") -> 100, List("e3f4") -> 90)))
    val child18 = waiting(submit(pair18, List(List(spend) -> 0, right -> 315)))
    assertEquals(child18.issuedWork.requiredDepth, 18)
    val finished = submit(child18, List(List("e5e6") -> 315, List("e5f6") -> 600))
    assert(finished.isInstanceOf[CompletedCommentaryJob],
      "the bounded investigation must expose partial completion, rather than hide another deeper request")
    assert(valueHeads(finished).exists((_, _, reading) => reading.state == "stopped" &&
      reading.stopReason.contains("depth_limit") && reading.answer.isEmpty && reading.workId.isEmpty))
    assertEquals(finished.observationsPacket.get.assembly.input.assessmentCandidates, originalAssessment)
    assert(List(ownPair.issuedWork, child16.issuedWork, pool17.issuedWork, pair17.issuedWork,
      child17.issuedWork, pool18.issuedWork, pair18.issuedWork, child18.issuedWork)
      .forall(work => work.requiredDepth <= 18 && finished.responseSearches.exists(_.work == work)))
    assert(valueHeads(finished).forall(_._3.answer.isEmpty),
      "an unresolved pair cannot acquire a complete value merely because its job stopped at the depth bound")

  test("normal commentary starts at depth18 but rejects a deeper root request"):
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    def begin(depth: Int) = CommentaryJobReducer.startJob(fen, Nil, fen, spend,
      history.extend(List(spend)).toOption.get.currentFen,
      CommentaryJobPolicy(depth, 180000L, profile, investigateChoices = true), 0L)
    assertEquals(waiting(begin(18).toOption.get).issuedWork.requiredDepth, 18)
    assertEquals(begin(19).left.toOption, Some(CommentaryJobStartRejection.RootSearchDepthInvalid))
