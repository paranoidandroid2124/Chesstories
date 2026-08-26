package lila.chessjudgment.model

import lila.chessjudgment.model.line.CandidateLineEvaluation
import lila.chessjudgment.model.strategic.EngineLine

class ProbeContractValidatorTest extends munit.FunSuite:

  test("probe transport identity is bounded and keeps objective plus exact search hash"):
    val firstHash = "a" * 64
    val secondHash = "b" * 64
    val branch = ProbeRequest.transportId(ProbeObjective.BranchReplyMultiPv, firstHash)
    val continuation = ProbeRequest.transportId(ProbeObjective.CausalContinuation, firstHash)
    val changedSearch = ProbeRequest.transportId(ProbeObjective.BranchReplyMultiPv, secondHash)

    assert(branch.length <= 256)
    assertNotEquals(branch, continuation)
    assertNotEquals(branch, changedSearch)
    assert(branch.endsWith(firstHash))

  test("MultiPV replies reject duplicate root moves instead of choosing one evaluation"):
    val request = ProbeRequest(
      id = "duplicate-root-probe",
      fen = "4k3/8/8/8/8/8/8/4K3 b - - 0 1",
      depth = 18,
      multiPv = 2,
      candidateMove = "e8d7",
      depthFloor = 16,
      variationHash = "duplicate-root",
      variant = ProbeVariant.BranchReply(requiredHorizonPlyOffset = 2)
    )
    val result = ProbeResult(
      id = request.id,
      resolution = ProbeResolution.EngineSearch(
        evaluations = List(
          CandidateLineEvaluation.EngineSearch(EngineLine(List("e8d7", "e1d1"), 10, depth = 18)),
          CandidateLineEvaluation.EngineSearch(EngineLine(List("e8d7", "e1f1"), -10, depth = 18))
        ),
        depth = 18
      )
    )

    val validation = ProbeContractValidator.validateAgainstRequest(request, result)
    assert(!validation.isValid)
    assert(validation.reasonCodes.contains("REPLY_ROOT_MOVE_CONFLICT"))
