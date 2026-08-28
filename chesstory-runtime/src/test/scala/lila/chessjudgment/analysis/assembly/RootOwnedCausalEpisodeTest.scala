package lila.chessjudgment.analysis.assembly

import chess.Black

import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }

class RootOwnedCausalEpisodeTest extends munit.FunSuite:

  test("an immediate recapture is owned through the canonical relation inventory"):
    val initialFen = "r1b2rk1/5ppp/1qp2n2/p2p4/1b6/N3P1P1/PPQB1PBP/R1R3K1 w - - 0 13"
    val rootFen = "r1b2rk1/5ppp/1qp2n2/p2p4/1b6/4P1P1/PPQB1PBP/RNR3K1 b - - 1 13"
    val moves = List("b4d2", "b1d2")
    val history = CanonicalPositionHistory
      .from(initialFen, List("a3b1"), rootFen)
      .toOption
      .getOrElse(fail("expected the exact predecessor history"))
    val lineHistory = history
      .extend(moves)
      .toOption
      .getOrElse(fail("expected the exact recapture line"))
    val replay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one canonical replay"))
    val moveRefs = replay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.head
    val line = LineNodeRef("exact-recapture", first.uci, 1, LineNodeRole.BestReference)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = None,
      continuationTail = Nil
    )
    val material = CandidateLineAssembler
      .lineMaterialSummary(history, lineHistory)
      .getOrElse(fail("expected the canonical material ledger"))
    val payload = LineFactNormalizer
      .fromValidatedLine(
        id = "exact-recapture-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(rootFen, 0, Some(Black)),
        scope = line.role.scope,
        materialSummary = Some(material)
      )
      .payload match
      case value: LineFactEvidence => value
      case _                       => fail("expected normalized line evidence")

    val recapture = payload.materialCaptures
      .find(capture => EvidenceRef.sameMove(capture.moveUci, "b1d2"))
      .getOrElse(fail("expected the canonical recapture occurrence"))
    assert(recapture.recaptureStatus.isInstanceOf[LineMaterialRecaptureStatus.Proven])

    val episode = payload
      .rootOwnedCausalEpisodes(line.rootMove)
      .find(_.consequence.kind == LineConsequenceKind.RecaptureSequence)
      .getOrElse(fail("expected the exact root-owned recapture sequence"))
    assertEquals(episode.actor.moveUci, "b4d2")
    assertEquals(episode.eventPlyOffset, 1)
    assert(episode.links.exists(_.kind == RootCausalLinkKind.RootActorCaptured))
    assert(RootOwnedEffectPolicy.admitsLineEpisode(payload, episode))
