package lila.chessjudgment.analysis.line

import chess.{ Bishop, Knight, Pawn, White }

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }

class SacrificeOccurrenceCardinalityTest extends munit.FunSuite:

  private val fen = "7k/8/4p3/3B2p1/5p2/8/4N3/K7 w - - 0 1"
  private val moves = List("e2f4", "g5f4", "a1b1", "e6d5")

  private val rootCaptureOffer = LineMaterialCapture(
    moveUci = "e2f4",
    plyOffset = 0,
    side = White,
    attackerRole = EvidencePieceRole(Knight.name),
    capturedRole = EvidencePieceRole(Pawn.name),
    square = EvidenceSquare("f4"),
    valueCp = 100,
    recapture = false
  )
  private val rootAcceptance = LineMaterialCapture(
    moveUci = "g5f4",
    plyOffset = 1,
    side = !White,
    attackerRole = EvidencePieceRole(Pawn.name),
    capturedRole = EvidencePieceRole(Knight.name),
    square = EvidenceSquare("f4"),
    valueCp = 300,
    recapture = true
  )
  private val stationaryAcceptance = LineMaterialCapture(
    moveUci = "e6d5",
    plyOffset = 3,
    side = !White,
    attackerRole = EvidencePieceRole(Pawn.name),
    capturedRole = EvidencePieceRole(Bishop.name),
    square = EvidenceSquare("d5"),
    valueCp = 300,
    recapture = false
  )
  private val captures = List(rootCaptureOffer, rootAcceptance, stationaryAcceptance)

  test("root-capture and stationary sacrifice occurrences coexist without fallback loss"):
    val line = normalizedLine("two-sacrifice-occurrences", captures)
    val occurrences = line.sacrificeOccurrencesForRootMove(moves.head)

    assertEquals(
      identities(occurrences),
      List(
        Some("e2f4" -> 0) -> ("g5f4", 1, "f4"),
        None -> ("e6d5", 3, "d5")
      )
    )
    assertEquals(
      line.lineConsequences
        .filter(_.kind == LineConsequenceKind.Sacrifice)
        .flatMap(_.sacrificeOccurrence),
      occurrences
    )
    assertEquals(
      line.principalSacrificeCostSequenceForRootMove(moves.head),
      List(rootAcceptance, stationaryAcceptance)
    )

  test("sacrifice occurrence inventory is order-invariant and merges exact duplicates"):
    val forward = normalizedLine("ordered-sacrifices", captures)
    val reversed = normalizedLine("reversed-sacrifices", captures.reverse)
    val duplicated = normalizedLine(
      "duplicated-sacrifices",
      List(stationaryAcceptance, rootCaptureOffer, rootAcceptance, rootAcceptance, stationaryAcceptance)
    )

    val expected = identities(forward.sacrificeOccurrencesForRootMove(moves.head))
    assertEquals(identities(reversed.sacrificeOccurrencesForRootMove(moves.head)), expected)
    assertEquals(identities(duplicated.sacrificeOccurrencesForRootMove(moves.head)), expected)

  test("conflicting capture payloads fail the exact sacrifice inventory closed"):
    val conflict = rootAcceptance.copy(valueCp = rootAcceptance.valueCp + 1)
    val line = normalizedLine("conflicting-sacrifice", captures :+ conflict)

    assertEquals(line.materialCaptures, Nil)
    assertEquals(line.sacrificeOccurrencesForRootMove(moves.head), Nil)
    assertEquals(
      line.lineConsequences.filter(_.kind == LineConsequenceKind.Sacrifice),
      Nil
    )

  private def identities(
      occurrences: List[LineSacrificeOccurrence]
  ): List[(Option[(String, Int)], (String, Int, String))] =
    occurrences.map { occurrence =>
      occurrence.offer.map(offer => offer.moveUci -> offer.plyOffset) -> (
        occurrence.acceptance.moveUci,
        occurrence.acceptance.plyOffset,
        occurrence.acceptance.square.key
      )
    }

  private def normalizedLine(
      id: String,
      materialCaptures: List[LineMaterialCapture]
  ): LineFactEvidence =
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .flatMap(_.extend(moves))
      .getOrElse(fail("expected a legal sacrifice replay"))
    val replay = CanonicalLineReplay
      .fromHistory(history.segmentReplaySteps)
      .getOrElse(fail("expected a canonical sacrifice replay"))
    val moveRefs = replay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.headOption.getOrElse(fail("expected a non-empty sacrifice line"))
    val line = LineNodeRef(id, first.uci, 1, LineNodeRole.BestReference)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    val material = LineMaterialSummary(
      sideToMove = White,
      captures = materialCaptures,
      netCaptureCpForMover = -500,
      maxGainCpForMover = 100,
      maxLossCpForMover = 500,
      hasRecaptureChain = true,
      hasRecoveryWindow = false,
      promotionGainCpForMover = 0,
      materialWindowComplete = true
    )

    LineFactNormalizer
      .fromValidatedLine(
        id = s"$id-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(fen, 0, Some(White)),
        scope = line.role.scope,
        materialSummary = Some(material)
      )
      .payload match
      case payload: LineFactEvidence => payload
      case _                         => fail("expected normalized line evidence")
