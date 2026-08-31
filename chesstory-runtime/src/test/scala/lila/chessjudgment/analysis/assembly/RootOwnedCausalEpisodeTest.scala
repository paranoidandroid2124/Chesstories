package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }

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
    assertEquals(episode.eventOccurrence.plyOffset, 1)
    assert(episode.links.exists(_.kind == RootCausalLinkKind.RootActorCaptured))
    assert(RootOwnedEffectPolicy.admitsLineEpisode(payload, episode))

  test("repeated promotion UCI retains two exact consequence occurrences"):
    val rootFen = "8/P7/P6k/8/8/8/8/7K w - - 0 1"
    val moves = List("a7a8q", "h6h5", "a8b8", "h5h4", "a6a7", "h4h3", "a7a8q")
    val (payload, line) = normalizedMaterialLine(
      id = "repeated-promotion",
      initialFen = rootFen,
      historyMoves = Nil,
      rootFen = rootFen,
      moves = moves
    )

    val promotions = payload.lineConsequences.filter(_.kind == LineConsequenceKind.Promotion)
    assertEquals(promotions.flatMap(_.eventOccurrence.map(_.plyOffset)), List(0, 6))
    assertEquals(
      promotions.map(_.proofOccurrences.map(occurrence => occurrence.plyOffset -> occurrence.moveUci)),
      List(
        List(0 -> "a7a8q"),
        List(0 -> "a7a8q", 6 -> "a7a8q")
      )
    )
    assertEquals(
      payload
        .rootOwnedCausalEpisodes(line.rootMove)
        .filter(_.consequence.kind == LineConsequenceKind.Promotion)
        .map(_.eventOccurrence.plyOffset),
      List(0)
    )

  test("lasting material gain does not absorb a later repeated UCI capture"):
    val initialFen = "7k/8/5KQ1/8/1r6/8/r7/R7 b - - 0 1"
    val rootFen = "7k/8/5KQ1/8/8/1r6/r7/R7 w - - 1 2"
    val (payload, _) = normalizedMaterialLine(
      id = "repeated-material-capture",
      initialFen = initialFen,
      historyMoves = List("b4b3"),
      rootFen = rootFen,
      moves = List("a1a2", "b3b2", "a2a1", "b2a2", "a1a2")
    )

    assertEquals(payload.materialGainCapturesFor(White).map(_.plyOffset), List(0))

  private def normalizedMaterialLine(
      id: String,
      initialFen: String,
      historyMoves: List[String],
      rootFen: String,
      moves: List[String]
  ): (LineFactEvidence, LineNodeRef) =
    val history = CanonicalPositionHistory
      .from(initialFen, historyMoves, rootFen)
      .toOption
      .getOrElse(fail(s"expected canonical history for $id"))
    val lineHistory = history
      .extend(moves)
      .toOption
      .getOrElse(fail(s"expected canonical line for $id"))
    val replay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail(s"expected canonical replay for $id"))
    val moveRefs = replay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.headOption.getOrElse(fail(s"expected a root move for $id"))
    val line = LineNodeRef(id, first.uci, 1, LineNodeRole.BestReference)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    val material = CandidateLineAssembler
      .lineMaterialSummary(history, lineHistory)
      .getOrElse(fail(s"expected a canonical material ledger for $id"))
    val payload = LineFactNormalizer
      .fromValidatedLine(
        id = s"$id-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(rootFen, 0, replay.legalSteps.headOption.map(_.move.piece.color)),
        scope = line.role.scope,
        materialSummary = Some(material)
      )
      .payload match
      case value: LineFactEvidence => value
      case _                       => fail(s"expected normalized line evidence for $id")
    payload -> line
