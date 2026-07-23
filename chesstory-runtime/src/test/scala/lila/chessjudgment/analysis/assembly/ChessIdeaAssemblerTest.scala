package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.{
  EndgameTablebaseEvidence,
  Plan,
  PlanEventIdentity,
  PlanId,
  PlanMatch,
  PlanSupport,
  ProbeAdmissionStatus,
  ProbePurpose,
  ProbeResult,
  TablebaseMoveEvidence,
  TablebasePositionEvidence,
  TransitionType
}
import lila.chessjudgment.model.strategic.VariationLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.analysis.line.PrincipalVariationEvidence
import lila.chessjudgment.analysis.singlePosition.CandidateSetType
import play.api.libs.json.JsObject
import scala.concurrent.duration.*

class ChessIdeaAssemblerTest extends munit.FunSuite:

  override val munitTimeout = 60.seconds

  test("an engine-backed style choice keeps observed triangulation separate from forced results"):
    def assess(depth: Int, probeResults: List[ProbeResult] = Nil) = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1",
          playedMoveUci = "d5e5",
          variations = List(
            VariationLine(
              List("d5d4", "d7d8", "d4e5", "d8d7", "e5d5", "d7e7", "c5c6", "e7f7", "c6b7", "f7f6", "b7b8q", "f6g5"),
              scoreCp = 1241,
              depth = depth
            ),
            VariationLine(
              List("d5e4", "d7c6", "e4d4", "c6d7", "d4d5", "d7e7", "c5c6", "e7f7", "c6b7", "f7f6", "b7b8q", "f6e7"),
              scoreCp = 1128,
              depth = depth
            ),
            VariationLine(
              List("d5e5", "d7c6", "e5d4", "c6d7", "d4d5", "d7e7", "c5c6", "b7c6", "d5c6", "e7e6", "b6b7", "e6e7"),
              scoreCp = 897,
              depth = depth
            )
          ),
          currentEvalCp = Some(897),
          ply = Some(0),
          probeResults = probeResults
        )
      )
      .getOrElse(fail("expected judgment result"))

    val result = assess(depth = 18)
    assertEquals(result.packet.probeRequests.count(_.purpose.contains(ProbePurpose.EndgameTablebase)), 1)
    val tablebaseRequest = result.packet.probeRequests
      .find(_.purpose.contains(ProbePurpose.EndgameTablebase))
      .getOrElse(fail("expected exact endgame request"))
    assertEquals(tablebaseRequest.candidateMove, None)
    assertEquals(tablebaseRequest.comparisonFen, Some("8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"))
    assert(tablebaseRequest.fen.startsWith("8/1p1k4/1P6/2PK4/8/8/8/8 b - - "), tablebaseRequest.fen)
    assertEquals(tablebaseRequest.moves, List("d7c8", "d7d8", "d7e7", "d7e8"))
    val comparison = result.packet.relativeAssessments.head.comparison
    val shallowComparison = assess(depth = 7).packet.relativeAssessments.head.comparison

    assertEquals(comparison.candidateSet.flatMap(_.candidateSetType), Some(CandidateSetType.StyleChoice))
    assertEquals(comparison.verdict, MoveChoiceVerdict.PlayableLoss)
    assertEquals(shallowComparison.candidateSet.flatMap(_.candidateSetType), Some(CandidateSetType.NarrowChoice))
    assertEquals(shallowComparison.verdict, MoveChoiceVerdict.Inaccuracy)

    val publicJson = MoveMeaningSurface.publicPayloadJson(result.packet.moveJudgmentView.getOrElse(fail("expected public view")))
    val playedTechnique = (publicJson \ "explanations")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .find(explanation => (explanation \ "move").asOpt[String].contains("d5e5"))
      .flatMap(explanation =>
        (explanation \ "techniques")
          .asOpt[List[play.api.libs.json.JsObject]]
          .getOrElse(Nil)
          .find(technique => (technique \ "pattern").asOpt[String].contains("triangulation"))
      )
      .getOrElse(fail("expected public triangulation technique"))
    val kingRoute = (playedTechnique \ "tempo_transfer" \ "king_route")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .flatMap(move => (move \ "uci").asOpt[String])

    assertEquals(kingRoute, List("d5e5", "e5d4", "d4d5"))
    assertEquals((playedTechnique \ "tempo_transfer" \ "tempo_passes_to").as[String], "black")
    assertEquals((playedTechnique \ "observed_continuation" \ "opponent_reply" \ "uci").as[String], "d7e7")
    assertEquals((playedTechnique \ "observed_continuation" \ "pawn_move" \ "uci").as[String], "c5c6")
    assertEquals((playedTechnique \ "observed_continuation" \ "reply_to_pawn_move" \ "uci").as[String], "b7c6")
    assertEquals((playedTechnique \ "observed_continuation" \ "new_passed_pawns").as[List[String]], List("b6"))
    assertEquals((playedTechnique \ "forced_results").as[List[String]], Nil)
    val publicTechniqueText = play.api.libs.json.Json.stringify(playedTechnique).toLowerCase
    assert(!List("zugzwang", "corresponding", "breakthrough").exists(publicTechniqueText.contains), publicTechniqueText)

    val exactEvidence = EndgameTablebaseEvidence(
      terminal = TablebasePositionEvidence(tablebaseRequest.fen, wdl = -2, dtm = Some(-22)),
      comparison = TablebasePositionEvidence(tablebaseRequest.comparisonFen.get, wdl = 2, dtm = Some(27)),
      legalMoves = List(
        TablebaseMoveEvidence("d7e7", resultingWdl = 2, resultingDtm = Some(21)),
        TablebaseMoveEvidence("d7e8", resultingWdl = 2, resultingDtm = Some(19)),
        TablebaseMoveEvidence("d7c8", resultingWdl = 2, resultingDtm = Some(17)),
        TablebaseMoveEvidence("d7d8", resultingWdl = 2, resultingDtm = Some(15))
      )
    )
    val exactProbe = ProbeResult(
      id = tablebaseRequest.id,
      fen = Some(tablebaseRequest.fen),
      evalCp = 0,
      deltaVsBaseline = 0,
      purpose = tablebaseRequest.purpose,
      variationHash = tablebaseRequest.variationHash,
      tablebase = Some(exactEvidence)
    )
    val proven = assess(depth = 18, probeResults = List(exactProbe))
    assert(!proven.packet.probeRequests.exists(_.id == tablebaseRequest.id))
    val provenTechnique = (MoveMeaningSurface.publicPayloadJson(proven.packet.moveJudgmentView.get) \ "explanations")
      .as[List[play.api.libs.json.JsObject]]
      .flatMap(explanation => (explanation \ "techniques").asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil))
      .find(technique =>
        (technique \ "pattern").asOpt[String].contains("triangulation") &&
          (technique \ "exact_proof").asOpt[play.api.libs.json.JsObject].nonEmpty
      )
      .getOrElse(fail("expected proven triangulation"))
    val exactProof = (provenTechnique \ "exact_proof").as[play.api.libs.json.JsObject]
    assertEquals((exactProof \ "kind").as[String], "zugzwang")
    assertEquals((exactProof \ "side_to_move").as[String], "black")
    assert((exactProof \ "all_legal_moves_lose").as[Boolean])
    assertEquals(
      (exactProof \ "legal_replies").as[List[play.api.libs.json.JsObject]].flatMap(reply => (reply \ "uci").asOpt[String]).sorted,
      tablebaseRequest.moves
    )
    assertEquals((provenTechnique \ "forced_results").as[List[String]], List("zugzwang"))
    assert(!play.api.libs.json.Json.stringify(provenTechnique).toLowerCase.contains("corresponding"))

    val incompleteProbe = exactProbe.copy(
      tablebase = Some(exactEvidence.copy(legalMoves = exactEvidence.legalMoves.filterNot(_.moveUci == "d7e8")))
    )
    val incomplete = assess(depth = 18, probeResults = List(incompleteProbe))
    assert(incomplete.packet.probeRequests.exists(_.id == tablebaseRequest.id))
    val incompletePublic = play.api.libs.json.Json.stringify(
      MoveMeaningSurface.publicPayloadJson(incomplete.packet.moveJudgmentView.get)
    ).toLowerCase
    assert(!incompletePublic.contains("zugzwang"), incompletePublic)

  test("assembled plan pressure and pawn structure evidence stays acyclic"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = chess.variant.Standard.initialFen.value,
          playedMoveUci = "d2d4",
          variations = List(
            VariationLine(List("e2e4", "e7e5", "g1f3"), scoreCp = 30, depth = 16),
            VariationLine(List("d2d4", "d7d5", "g1f3"), scoreCp = 20, depth = 16)
          ),
          currentEvalCp = Some(20),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph

    assert(graph.records.exists(_.payload.isInstanceOf[PlanPressureEvidence]))
    assert(graph.records.exists(_.payload.isInstanceOf[PawnStructureFactEvidence]))
    val playedPressure = graph.records.collectFirst {
      case EvidenceRecord(ref, payload: PlanPressureEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => payload
    }.getOrElse(fail("expected played plan pressure"))
    assert(!playedPressure.activePlanIds(Some("d2d4")).contains(PlanId.OpeningDevelopment))
    assert(!graph.records.exists {
      case EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _) =>
        mechanism.signals.exists(signal =>
          signal.kind == StrategicMechanismSignalKind.PlanPressure &&
            signal.source.layer == EvidenceLayer.PlanPressure
        )
      case _ => false
    })
    assertEquals(
      graph.records.filter(record => graph.parentClosure(record).exists(_.ref.id == record.ref.id)).map(_.ref.id),
      Nil
    )

  test("castling activity keeps its root line without borrowing plan pressure"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "rnb1k2r/pp2ppbp/5np1/q1Pp4/2P2B2/2N1P3/PP3PPP/2RQKBNR b Kkq - 2 7",
          playedMoveUci = "e8h8",
          variations = List(
            VariationLine(
              List("e8g8", "c4d5", "b8d7", "f1c4", "d7c5", "a2a3"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(13)
        )
      )
      .getOrElse(fail("expected castling evidence graph"))
      .context
      .evidenceGraph
    val activity = graph.records.collectFirst {
      case EvidenceRecord(ref, mechanism: StrategicMechanismEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) &&
            mechanism.kind == StrategicMechanismKind.Activity &&
            mechanism.signals.exists(_.label == "current-move-route") =>
        mechanism
    }.getOrElse(fail("expected line-owned castling activity"))

    assert(activity.canAnchorStrategicIdea)

  test("minor-piece development to a flank square does not invent wing play"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbq1rk1/p3bppp/4p3/2Pn4/PpN5/6P1/1PQ1PPBP/RNB2RK1 b - - 0 11",
          playedMoveUci = "c8a6",
          variations = List(
            VariationLine(
              List("e7c5", "e2e4", "d5b6", "c4d2", "b8d7", "a4a5", "c8a6", "a5b6"),
              scoreCp = 62,
              depth = 18
            ),
            VariationLine(
              List("c8a6", "c4e3", "b8d7", "c5c6", "d7c5", "e3d5", "e6d5", "c1f4"),
              scoreCp = 86,
              depth = 18
            )
          ),
          currentEvalCp = Some(62),
          ply = Some(21)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val rootPlans = result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) && EvidenceRef.sameMove(event.rootMove, "c8a6") =>
        event
    }

    assert(!rootPlans.exists(event =>
      event.planId == PlanId.QueensideAttack &&
        event.identity.goalKind.contains(PlanKind.WingExpansion)
    ), rootPlans)
    assert(result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces).exists(surface =>
      surface.moveUci == "c8a6" && surface.idea.code == "piece_route"
    ))

  test("a mixed route comparison cannot lend the reference actor to the played move"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "3rrnk1/pp3ppp/1npqb3/3p4/1P1P4/2NBP2P/P1QN1PP1/1R3RK1 w - - 3 17",
          playedMoveUci = "c3e2",
          variations = List(
            VariationLine(
              List("a2a4", "d6e7", "c3e2", "a7a6", "e2f4", "g7g6", "b4b5", "c6b5"),
              scoreCp = 77,
              depth = 18
            ),
            VariationLine(
              List("f1c1", "a7a6", "a2a4", "d8a8", "c3e2", "f8g6", "b1a1", "d6e7"),
              scoreCp = 73,
              depth = 18
            ),
            VariationLine(
              List("c3e2", "a7a6", "d2b3", "e6c8", "b3a5", "g7g6", "a2a4", "f8e6"),
              scoreCp = 71,
              depth = 18
            )
          ),
          currentEvalCp = Some(77),
          ply = Some(32)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val playedSurfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(surface => surface.lineRole == "candidate" && surface.moveUci == "c3e2")

    assert(!playedSurfaces.exists(surface =>
      surface.evidence.boardCarriers.exists(carrier =>
        carrier.value == "c1" || carrier.value == "c" || carrier.value == "rook"
      )
    ), playedSurfaces)
    assert(playedSurfaces.filter(_.evidence.boardCarriers.exists(_.value == "queen")).forall(surface =>
      surface.evidence.boardCarriers.exists(carrier => carrier.kind == "Move" && EvidenceRef.sameMove(carrier.value, "c3e2")) &&
        surface.evidence.boardCarriers.exists(carrier => carrier.kind == "Square" && carrier.value == "c2")
    ), playedSurfaces)

  test("a root-owned rook-behind-passer motif survives immediate mobility loss"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "6k1/6pp/8/8/1P6/8/2r3PP/4R2K w - - 0 1",
          playedMoveUci = "e1b1",
          variations = List(
            VariationLine(
              List("e1b1", "g8f7", "b4b5", "c2c8", "h2h3", "f7e6", "b5b6", "e6d6"),
              scoreCp = 156,
              depth = 18
            ),
            VariationLine(
              List("h1g1", "c2b2", "e1e8", "g8f7", "e8b8", "g7g5", "b4b5", "f7g7"),
              scoreCp = 19,
              depth = 18
            )
          ),
          currentEvalCp = Some(156),
          ply = Some(0)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val surface = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .find(item => item.moveUci == "e1b1" && item.idea.code == "rook_behind_passed_pawn")
      .getOrElse(fail("expected rook-behind-passer surface"))

    assert(surface.evidence.boardCarriers.exists(carrier => carrier.role == "actor" && carrier.value == "rook"))
    assert(surface.evidence.boardCarriers.exists(carrier => carrier.role == "target" && carrier.value == "pawn"))
    assert(surface.evidence.boardCarriers.exists(carrier =>
      carrier.role == "target" && carrier.kind == "File" && carrier.value == "b"
    ), surface.evidence.boardCarriers)

  test("a forcing move carries the absolute pin that disables its apparent defender"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "7k/2p2p1p/p7/1p1p4/PP2pr2/B1P3qP/4N1B1/R1Qn2K1 b - - 0 36",
          playedMoveUci = "f4f1",
          variations = List(
            VariationLine(List("f4f1", "g1f1", "g3f2"), scoreCp = -9998, depth = 18, mate = Some(-2)),
            VariationLine(
              List("g3f2", "g1h2", "f4g4", "h3g4", "f2h4", "g2h3", "d1f2", "h2g2"),
              scoreCp = 249,
              depth = 18
            )
          ),
          currentEvalCp = Some(-9998),
          ply = Some(71)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val pin = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.Pin && relation.mentionsLineMove("f4f1") =>
        relation
    }.getOrElse(fail("expected root-owned pin relation"))
    val mateSurface = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .find(surface => surface.moveUci == "f4f1" && surface.idea.code == "terminal_mate")
      .getOrElse(fail("expected terminal mate surface"))

    assert(pin.hasConcreteRelationProof)
    assert(mateSurface.evidence.proofRelationKinds.contains(RelationFactKind.Pin), mateSurface.evidence)
    assert(mateSurface.evidence.boardCarriers.exists(carrier =>
      carrier.role == "blocker" && carrier.value == "g2"
    ), mateSurface.evidence.boardCarriers)

  test("a losing offered queen does not manufacture sacrifice compensation"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "7k/q1p5/8/1P6/N7/8/8/7K b - - 0 1",
          playedMoveUci = "a7b6",
          variations = List(
            VariationLine(List("a7b7", "h1g1"), scoreCp = 0, depth = 18),
            VariationLine(List("a7b6", "a4b6", "c7b6"), scoreCp = 600, depth = 18)
          ),
          currentEvalCp = Some(0),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val surfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val materialLoss = surfaces
      .find(surface => surface.moveUci == "a7b6" && surface.idea.code == "material_loss")
      .getOrElse(fail("expected root-owned material-loss terminal proof"))
    val publicPayload = result.packet.moveJudgmentView
      .map(MoveMeaningSurface.publicPayloadJson)
      .getOrElse(fail("expected public payload"))
    val publicIdeas = (publicPayload \\ "ideas")
      .flatMap(_.asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil))
    val publicIdeaKeys = publicIdeas.map(idea =>
      (
        (idea \ "kind").asOpt[String],
        (idea \ "name").asOpt[String],
        (idea \ "target").toOption
      )
    )
    assertEquals(result.packet.moveJudgmentView.flatMap(_.verdict).map(_.verdict), Some(MoveChoiceVerdict.Blunder))
    assert(materialLoss.assessment.verdictReason, materialLoss)
    assertEquals(publicIdeaKeys.distinct.size, publicIdeaKeys.size)
    assert(!surfaces.exists(surface => surface.moveUci == "a7b6" && surface.idea.code == "compensation"), surfaces)
    assert(!surfaces.exists(surface => surface.moveUci == "a7b6" && surface.idea.code == "material_gain"), surfaces)

  test("positional sacrifice compensation keeps both the sacrifice and its functional result"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "r1bq1rk1/2ppbppp/p1n2n2/1p2p3/4P3/1BP2N2/PP1P1PPP/RNBQR1K1 b - - 0 8",
          playedMoveUci = "d7d5",
          variations = List(
            VariationLine(List("d7d6", "h2h3", "f8e8", "f3g5", "e8f8", "d2d4"), 42, depth = 18),
            VariationLine(List("c6a5", "b3c2", "d7d5", "e4d5", "e5e4", "c2e4"), 43, depth = 18),
            VariationLine(List("d7d5", "e4d5", "f6d5", "f3e5", "c6e5", "e1e5", "c8b7"), 46, depth = 18)
          ),
          currentEvalCp = Some(42),
          ply = Some(15)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val compensation = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .find(surface => surface.moveUci == "d7d5" && surface.idea.code == "compensation")
      .getOrElse(fail("expected positional sacrifice compensation"))
    val signatures = result.packet.moveJudgmentView.toList
      .flatMap(_.moveMeaningClaims)
      .find(claim =>
        claim.publicSurfaceAdmitted &&
          claim.unit == PositionPlanTechniqueUnit.CompensationSource &&
          EvidenceRef.sameMove(claim.moveUci, "d7d5")
      )
      .map(_.objectBindingSignatures)
      .getOrElse(fail("expected public compensation claim"))

    assert(signatures.exists(_.toLowerCase.contains("mechanism=mechanism:sacrifice")), signatures)
    assert(signatures.exists(signature =>
      val normalized = signature.toLowerCase
      normalized.contains("mechanism=mechanism:lineunlockgain") &&
        (normalized.contains("target=piece:bishop") || normalized.contains("target=piece:queen"))
    ), signatures)
    assert(compensation.evidence.boardCarriers.exists(carrier =>
      carrier.role == "target" &&
        carrier.kind == "PlanSubject" &&
        carrier.value.startsWith("material-sacrifice:")
    ), compensation.evidence)

  test("actor-bound functions expire while a completed like-piece exchange remains"):
    val cases = List(
      RawMoveReviewInput(
        fen = "3qk3/8/8/3r4/8/8/8/3RK3 w - - 0 1",
        playedMoveUci = "d1d5",
        variations = List(VariationLine(List("d1d5", "d8d5"), scoreCp = 0, depth = 18)),
        currentEvalCp = Some(0),
        ply = Some(0)
      ),
      RawMoveReviewInput(
        fen = "7r/P6k/8/8/8/8/8/4K3 w - - 0 1",
        playedMoveUci = "a7a8q",
        variations = List(VariationLine(List("a7a8q", "h8a8"), scoreCp = 0, depth = 18)),
        currentEvalCp = Some(0),
        ply = Some(0)
      )
    )
    val actorBoundIdeas = Set(
      "piece_route",
      "piece_activity",
      "flank_pawn_pressure",
      "pawn_break_timing",
      "center_control"
    )

    cases.foreach { input =>
      val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected judgment result"))
      val playedLine = result.context.evidenceGraph.records.collectFirst {
        case EvidenceRecord(ref, payload: LineFactEvidence, _)
            if ref.line.exists(_.role == LineNodeRole.Played) => payload
      }.getOrElse(fail("expected played line fact"))
      val playedSurfaces = result.packet.moveJudgmentView.toList
        .flatMap(MoveMeaningSurface.publicSurfaces)
        .filter(_.subject == "played_move")

      assertEquals(playedLine.rootActorSurvivesLine, Some(false))
      assert(!playedSurfaces.exists(surface => actorBoundIdeas(surface.idea.code)), playedSurfaces)
      val resolved = playedSurfaces.flatMap(_.resolvedPlanEvent).distinct
      if EvidenceRef.sameMove(input.playedMoveUci, "d1d5") then
        assert(playedSurfaces.exists(_.idea.code == "piece_exchange"), playedSurfaces)
        assert(resolved.nonEmpty, playedSurfaces)
        assert(resolved.forall(_.goalTheme == PlanTheme.FavorableExchange.id), resolved)
        assertEquals(
          resolved.flatMap(_.results).map(_.kind).toSet,
          Set(TransitionConsequenceKind.PieceExchangeCompleted)
        )
      else
        assertEquals(resolved, Nil)
    }

  test("root-owned immediate target pressure survives the attacker's later capture"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "b1qrr1k1/p6p/P4ppP/Npp1p2n/3n4/2QP1NP1/1PP2PB1/R3R1K1 w - - 2 26",
          playedMoveUci = "a5b7",
          variations = List(
            VariationLine(
              List("a5b7", "d4e6", "c3b3", "d8d7", "a1a5", "a8b7", "a6b7"),
              scoreCp = 227,
              depth = 18
            ),
            VariationLine(
              List("e1e3", "b5b4", "c3d2", "d4f5", "e3e1", "e5e4", "f3h2", "e4e3", "f2e3"),
              scoreCp = -36,
              depth = 18
            ),
            VariationLine(
              List("f3h2", "a8g2", "g1g2", "d8d5", "a5b3", "d4f5", "b3d2", "f5h6"),
              scoreCp = -97,
              depth = 18
            )
          ),
          currentEvalCp = Some(227),
          ply = Some(50)
        )
      )
      .getOrElse(fail("expected target-pressure judgment"))
    val playedLine = result.context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => payload
    }.getOrElse(fail("expected played line fact"))
    val pressure = result.packet.moveJudgmentView.toList
      .flatMap(_.moveMeaningClaims)
      .find(claim =>
        EvidenceRef.sameMove(claim.moveUci, "a5b7") &&
          claim.meaningKind == "TargetPressure" &&
          claim.supportLevel == "owned_cause_linked" &&
          claim.surfaceLane == "current_move_owned" &&
          claim.causeKinds.contains(RelativeCauseKind.TargetPressureGain) &&
          claim.targetSquares.toSet == Set("c5", "d8")
      )
      .getOrElse(fail("expected root-owned double attack"))

    assertEquals(playedLine.rootActorSurvivesLine, Some(false))
    assert(pressure.publicSurfaceAdmitted, pressure)
    assertEquals(pressure.targetPieces.toSet, Set("pawn", "rook"))
    assert(pressure.boardCarriers.exists(carrier =>
      carrier.role == "target" &&
        carrier.kind == "Piece" &&
        carrier.semanticRole.contains(MoveMeaningSurfaceBoardCarrier.AttackedPieceRole)
    ), pressure.boardCarriers)
    val publicIdeas = (MoveMeaningSurface.publicPayloadJson(result.packet.moveJudgmentView.get) \ "explanations")
      .as[List[JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains("a5b7"))
      .toList
      .flatMap(explanation => (explanation \ "ideas").as[List[JsObject]])
    val publicPressure = publicIdeas
      .find(idea =>
        (idea \ "kind").asOpt[String].contains("target_pressure") &&
          (idea \ "target" \ "squares").as[List[String]].toSet == Set("c5", "d8")
      )
      .getOrElse(fail("expected the double attack in the public top three"))
    assertEquals((publicPressure \ "target" \ "pieces").as[List[String]].toSet, Set("pawn", "rook"))
    assert(!(publicPressure \ "name").as[String].toLowerCase.contains("king"), publicPressure)

  test("exact target pressure stays public for a single target and a pawn fork"):
    val cases = List(
      (
        RawMoveReviewInput(
          fen = "r1bq1rk1/ppp2ppp/2nbpn2/1B6/3P4/PQN1PN2/1P3PPP/R1B1K2R b KQ - 2 9",
          playedMoveUci = "e6e5",
          variations = List(
            VariationLine(List("e6e5", "b5c6", "e5d4", "e3d4", "b7c6", "e1g1", "h7h6"), -17, depth = 18),
            VariationLine(List("h7h6", "b5c6", "b7c6", "e3e4", "f6d7", "e4e5", "d6e7"), -4, depth = 18),
            VariationLine(List("a7a5", "e1g1", "e6e5", "d4d5", "c6e7", "e3e4", "h7h6"), 28, depth = 18),
            VariationLine(List("a7a6", "b5c6", "b7c6", "e3e4", "f6d7", "c1g5", "d8e8"), 29, depth = 18)
          ),
          currentEvalCp = Some(-17),
          ply = Some(17)
        ),
        Set("d4"),
        Set("pawn")
      ),
      (
        RawMoveReviewInput(
          fen = "rn2qrk1/b1pb2pp/p2p1n2/1P1Ppp2/N7/1Q2PPP1/PP1BN1BP/R3K2R w KQ - 3 15",
          playedMoveUci = "b5b6",
          variations = List(
            VariationLine(List("b5b6", "a7b6", "a4b6", "c7b6", "b3b6", "f6d5", "b6d6"), 120, depth = 18),
            VariationLine(List("e1g1", "a6b5", "a4c3", "b8a6", "a2a3", "f5f4", "g3f4"), -192, depth = 18),
            VariationLine(List("a2a3", "a6b5", "a4c3", "b8a6", "e1g1", "f5f4", "g3f4"), -211, depth = 18),
            VariationLine(List("a4c3", "a6b5", "a2a4", "a7c5", "e1g1", "b5a4", "b3c2"), -212, depth = 18)
          ),
          currentEvalCp = Some(120),
          ply = Some(28)
        ),
        Set("a7", "c7"),
        Set("bishop", "pawn")
      )
    )

    cases.foreach { (input, targetSquares, targetPieces) =>
      val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected target-pressure judgment"))
      val view = result.packet.moveJudgmentView.getOrElse(fail("expected move-judgment view"))
      val pressure = view.moveMeaningClaims
        .find(claim =>
          EvidenceRef.sameMove(claim.moveUci, input.playedMoveUci) &&
            claim.meaningKind == "TargetPressure" &&
            claim.supportLevel == "owned_cause_linked" &&
            claim.surfaceLane == "current_move_owned" &&
            claim.causeKinds.contains(RelativeCauseKind.TargetPressureGain) &&
            claim.targetSquares.toSet == targetSquares &&
            claim.targetPieces.toSet == targetPieces
        )
        .getOrElse(fail(s"expected exact pressure on ${targetSquares.toList.sorted.mkString("/")}"))
      assert(pressure.publicSurfaceAdmitted, pressure)

      val publicIdeas = (MoveMeaningSurface.publicPayloadJson(view) \ "explanations")
        .as[List[JsObject]]
        .find(explanation => (explanation \ "move").asOpt[String].contains(input.playedMoveUci))
        .toList
        .flatMap(explanation => (explanation \ "ideas").as[List[JsObject]])
      val publicPressure = publicIdeas
        .find(idea =>
          (idea \ "kind").asOpt[String].contains("target_pressure") &&
            (idea \ "target" \ "squares").as[List[String]].toSet == targetSquares &&
            (idea \ "target" \ "pieces").as[List[String]].toSet == targetPieces
        )
        .getOrElse(fail(s"expected exact pressure in public top three: $publicIdeas"))
      assert(!(publicPressure \ "name").as[String].toLowerCase.contains("king"), publicPressure)
    }

  test("downstream threat squares do not become current-move counterplay control"):
    val base = RawMoveReviewInput(
      fen = "8/8/4K2k/8/4P3/4Q3/8/7q b - - 4 82",
      playedMoveUci = "h6h5",
      variations = List(
        VariationLine(List("h6h7", "e3a7", "h7h6", "a7d4", "h6h7", "d4d7", "h7h6"), 24, depth = 18),
        VariationLine(List("h6g6", "e3g3", "g6h5", "g3e5", "h5g4", "e5f5", "g4g3"), 415, depth = 18),
        VariationLine(List("h6h5", "e3c5", "h5h4", "c5f2", "h4h3", "f2f5", "h3g2"), 420, depth = 18),
        VariationLine(List("h6g7", "e3g5", "g7h7", "e6f7", "h1f1", "g5f5", "f1f5"), 512, depth = 18)
      ),
      currentEvalCp = Some(420),
      ply = Some(163)
    )
    val input = withReplyProbeFor(
      base,
      "h6h5",
      List(
        VariationLine(List("e3c5", "h5h4", "c5f2", "h4h3", "f2f5", "h3g3", "e4e5", "h1c6"), 405, depth = 16),
        VariationLine(List("e6f7", "h1f1", "f7e7", "f1b1", "e3h3", "h5g5", "h3f5", "g5h6", "f5f4", "h6g7"), 62, depth = 16),
        VariationLine(List("e3e2", "h5h6", "e6f7", "h1g1", "e2d2", "h6h7", "d2d7", "g1g3"), 28, depth = 16)
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected endgame judgment"))
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move-judgment view"))
    val falseDefensiveClaim = view.moveMeaningClaims.find(claim =>
      EvidenceRef.sameMove(claim.moveUci, "h6h5") &&
        claim.label.contains("defensive-resource") &&
        claim.meaningKind == "CounterplayControl" &&
        claim.publicSurfaceAdmitted
    )
    val playedIdeas = (MoveMeaningSurface.publicPayloadJson(view) \ "explanations")
      .as[List[JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains("h6h5"))
      .toList
      .flatMap(explanation => (explanation \ "ideas").as[List[JsObject]])

    assertEquals(falseDefensiveClaim, None)
    assert(!playedIdeas.exists(idea =>
      (idea \ "kind").asOpt[String].contains("counterplay_control") &&
        (idea \ "target" \ "squares").asOpt[List[String]].exists(_.exists(Set("b1", "e7")))
    ), playedIdeas)

  test("a root capture inherits recapture status from verified game history"):
    val assembly = NodeLineTransitionAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "rnbqkb1r/ppp1pppp/8/3n4/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 4",
          playedMoveUci = "c3d5",
          variations = List(VariationLine(List("c3d5", "d8d5"), scoreCp = 0, depth = 12)),
          currentEvalCp = Some(0),
          ply = Some(6),
          movePrefixUci = List("e2e4", "d7d5", "e4d5", "g8f6", "b1c3", "f6d5")
        )
      )
      .getOrElse(fail("expected node-line assembly"))
    val playedLine = assembly.context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => payload
    }.getOrElse(fail("expected played line facts"))
    val rootCapture = playedLine.materialCaptures.find(capture =>
      capture.plyOffset == 0 && EvidenceRef.sameMove(capture.moveUci, "c3d5")
    )

    assertEquals(rootCapture.map(_.recapture), Some(true))
    assert(playedLine.rootIsRecapture("c3d5"))
    assert(!playedLine.rootIsCaptureSacrifice("c3d5"))
    assert(playedLine.lineEvents.exists(event =>
      event.kind == LineEventKind.Recapture &&
        event.plyOffset == 0 &&
        EvidenceRef.sameMove(event.moveUci, "c3d5")
    ), playedLine.lineEvents)
    assert(playedLine.hasProofSignalConsequence(LineConsequenceKind.RecaptureSequence))

  test("a root capture sacrifice is not mistaken for a root recapture"):
    val assembly = NodeLineTransitionAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "8/8/8/8/6k1/7p/6B1/K7 w - - 0 1",
          playedMoveUci = "g2h3",
          variations = List(VariationLine(List("g2h3", "g4h3"), scoreCp = 0, depth = 12)),
          currentEvalCp = Some(0),
          ply = Some(0)
        )
      )
      .getOrElse(fail("expected node-line assembly"))
    val playedLine = assembly.context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => payload
    }.getOrElse(fail("expected played line facts"))

    assert(!playedLine.rootIsRecapture("g2h3"))
    assert(playedLine.rootIsCaptureSacrifice("g2h3"))
    assert(playedLine.hasProofSignalConsequence(LineConsequenceKind.RecaptureSequence))

  test("completed pawn tension remains explainable after the initiating pawn is exchanged"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/p2ppppp/5n2/1PpP4/8/8/PP2PPPP/RNBQKBNR b KQkq - 0 4",
          playedMoveUci = "a7a6",
          variations = List(
            VariationLine(
              List("a7a6", "b5a6", "g7g6", "b1c3", "f8g7", "e2e4", "e8g8", "g1f3"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(7)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val playedLine = result.context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => payload
    }.getOrElse(fail("expected played line fact"))
    val playedSurfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(_.subject == "played_move")

    assertEquals(playedLine.rootActorSurvivesLine, Some(false))
    assert(playedSurfaces.exists(_.idea.code == "pawn_tension_creation"), playedSurfaces)

  test("distinct same-root plans expose only the matching chess result"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/pp1ppppp/5n2/2pP4/2P5/8/PP2PPPP/RNBQKBNR b KQkq - 0 3",
          playedMoveUci = "b7b5",
          variations = List(
            VariationLine(
              List("b7b5", "c4b5", "a7a6", "b5a6", "g7g6", "b1c3", "f8g7", "e2e4", "e8g8", "g1f3", "d7d6", "f1e2"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(5),
          openingContext = Some(RawOpeningContext(name = Some("Benko Gambit / queenside file pressure"))),
          movePrefixUci = List("d2d4", "g8f6", "c2c4", "c7c5", "d4d5")
        )
      )
      .getOrElse(fail("expected judgment result"))
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val queensideSurface = MoveMeaningSurface
      .publicSurfaces(view)
      .find(_.displayPlanId.contains(PlanId.QueensideAttack))
      .getOrElse(fail("expected queenside surface"))
    assertEquals(queensideSurface.target.squares, List("c4"))
    assert(!queensideSurface.target.pieces.contains("bishop"), queensideSurface.target)
    val selectedEvent = queensideSurface.resolvedPlanEvent.getOrElse(fail("expected queenside plan event"))
    val publicPlan = (MoveMeaningSurface.publicPayloadJson(view) \ "explanations")
      .as[List[play.api.libs.json.JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains("b7b5"))
      .flatMap(explanation => (explanation \ "plan").asOpt[play.api.libs.json.JsObject])
      .getOrElse(fail("expected one representative same-root plan"))
    assertEquals(
      (publicPlan \ "goal" \ "theme").as[String],
      selectedEvent.goalTheme.replaceAll("[_-]+", " ").toLowerCase
    )
    assertEquals(
      (publicPlan \ "goal" \ "kind").asOpt[String],
      selectedEvent.goalKind.map(_.replaceAll("[_-]+", " ").toLowerCase)
    )
    assertEquals((publicPlan \ "goal" \ "theme").as[String], "wing play")
    assertEquals((publicPlan \ "goal" \ "kind").as[String], "hook creation")

  test("resolved plan event exposes only its own board objects"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/p2ppppp/5n2/2pP4/2p1P3/8/PP1N1PPP/R1BQKBNR b KQkq - 0 5",
          playedMoveUci = "e7e6",
          variations = List(
            VariationLine(
              List("e7e6", "f1c4", "e6d5", "e4d5", "d7d6", "g1f3", "g7g6", "e1g1", "f8g7", "f1e1"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(9),
          openingContext = Some(RawOpeningContext(name = Some("Benko Gambit / ...bxc4 and ...e6 counterplay"))),
          movePrefixUci = List("d2d4", "g8f6", "c2c4", "c7c5", "d4d5", "b7b5", "b1d2", "b5c4", "e2e4")
        )
      )
      .getOrElse(fail("expected judgment result"))
    val claims =
      result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims).filter(_.resolvedPlanEvent.nonEmpty)
    val publicSurfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(_.resolvedPlanEvent.nonEmpty)
    assert(claims.nonEmpty)
    assert(publicSurfaces.nonEmpty)
    assert(result.packet.moveJudgmentView.exists(view =>
      val public = MoveMeaningSurface.publicPayloadJson(view)
      !public.toString.contains("positive_functional_proof_ids") &&
        (public \\ "verification").exists(_.asOpt[List[String]].exists(_.contains("verified move function")))
    ))
    claims.foreach { claim =>
      val event = claim.resolvedPlanEvent.getOrElse(fail("expected resolved plan event"))
      assertEquals(claim.displayPlanId, Some(PlanId.PawnBreakPreparation))
      assertEquals(event.rootMove, "e7e6")
      assertEquals(event.results.map(_.kind), List(TransitionConsequenceKind.PawnTensionGain))
      assert(claim.boardCarriers.exists(carrier => carrier.kind == "Move" && carrier.value == "e7e6"))
      assertEquals(claim.boardCarriers.filter(_.kind == "File").map(_.value).distinct, List("e"))
      assertEquals(claim.boardCarriers.filter(_.kind == "Piece").map(_.value).distinct, List("pawn"))
      assert(!claim.boardCarriers.exists(_.semanticRole.contains("counter_break_file")), claim.boardCarriers)
      assert(!claim.boardCarriers.exists(carrier => carrier.kind == "Square" && Set("c4", "d8", "f8")(carrier.value)), claim.boardCarriers)
    }

  test("specific weak-pawn event outranks generic activity without borrowing plan authority"):
    val result = MoveReviewJudgmentOrchestrator
      .build(weakPawnInput())
      .getOrElse(fail("expected judgment result"))
    val events = result.packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) && EvidenceRef.sameMove(event.rootMove, "c6a5") =>
        record -> event
    }
    val weakPawnEvent =
      events.find(_._2.planId == PlanId.WeakPawnAttack).getOrElse(fail("expected c4 plan event"))
    val activityEvent =
      events.find(_._2.planId == PlanId.PieceActivation).getOrElse(fail("expected activity alternative"))
    val publicClaims =
      result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims).filter(_.publicSurfaceAdmitted)

    assertEquals(
      weakPawnEvent._2.structuralConsequences.map(_.kind),
      List(TransitionConsequenceKind.TargetPressureGain)
    )
    assertEquals(weakPawnEvent._2.structuralConsequences.flatMap(_.subjects), List("c4"))
    assert(
      publicClaims.exists(claim =>
        claim.displayPlanId.contains(PlanId.WeakPawnAttack) &&
          claim.positiveFunctionalProofEvidenceIds.contains(weakPawnEvent._1.ref.id) &&
          claim.targetSquares.contains("c4")
      ),
      publicClaims
    )
    assert(
      !publicClaims.exists(claim =>
        claim.meaningKind == "TargetPressure" &&
          claim.positiveFunctionalProofEvidenceIds.isEmpty &&
          claim.sourceEvidenceIds.exists(_.contains("structural-delta:played:c6a5"))
      ),
      publicClaims
    )

  test("a quiet retreat activates the existing redeployment plan through its later piece route"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "1r3rk1/1b2bpp1/pq1ppn2/1p4B1/nP1NP1PP/P1N4R/2P1Q1B1/1K1R4 w - - 1 19",
          playedMoveUci = "g5c1",
          variations = List(
            VariationLine(
              List("g5c1", "f6d7", "c3a4", "b5a4", "c1b2", "d7e5", "h4h5", "e7g5"),
              scoreCp = 108,
              depth = 18
            )
          ),
          currentEvalCp = Some(108),
          ply = Some(36)
        )
      )
      .getOrElse(fail("expected quiet-route judgment"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation && EvidenceRef.sameMove(payload.rootMove, "g5c1") =>
        payload
    }.getOrElse(fail("expected continuation-backed redeployment event"))
    val episode = event.episode.getOrElse(fail("expected typed bishop route"))

    assert(episode.dependencies.exists {
      case PlanCausalEventDependency(from, to, PlanCausalDependencyKind.ObjectStatePrecondition, _, _) =>
        EvidenceRef.sameMove(from.moveUci, "g5c1") && EvidenceRef.sameMove(to.moveUci, "c1b2")
      case _ => false
    }, episode.dependencies)
    assert(episode.continuations.exists(node =>
      EvidenceRef.sameMove(node.moveUci, "c1b2") && node.developmentChoices.exists(choice =>
        choice.role.equalsIgnoreCase("bishop") && choice.from == "c1" && choice.to == "b2"
      )
    ), episode.continuations)

  test("a quiet clearance connects the enabled piece to its later route"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "5r1k/1pprb1pp/2n4q/5p2/1P1Pp3/4P3/2QB1PPP/R2R1NK1 b - - 2 22",
          playedMoveUci = "e7d6",
          variations = List(
            VariationLine(
              List(
                "e7d6", "b4b5", "c6e7", "a1a7", "b7b6", "d2c1", "e7d5", "c1a3",
                "d6a3", "a7a3", "d7e7", "d1e1", "e7e8", "c2b3", "h6d6", "f1d2"
              ),
              scoreCp = -16,
              depth = 18
            )
          ),
          currentEvalCp = Some(-16),
          ply = Some(43)
        )
      )
      .getOrElse(fail("expected clearance-route judgment"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation && EvidenceRef.sameMove(payload.rootMove, "e7d6") =>
        payload
    }.getOrElse(fail("expected continuation-backed piece route"))
    val episode = event.episode.getOrElse(fail("expected continuation-backed piece route"))
    val enabledRoute = episode.continuations.find(node => EvidenceRef.sameMove(node.moveUci, "c6e7"))
      .getOrElse(fail("expected enabled knight route"))
    val laterPressure = episode.continuations.find(node => EvidenceRef.sameMove(node.moveUci, "e7d5"))
      .getOrElse(fail("expected later knight pressure"))

    assert(episode.dependencies.exists {
      case PlanCausalEventDependency(from, to, PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(trajectory), _) =>
        EvidenceRef.sameMove(from.moveUci, "e7d6") &&
          EvidenceRef.sameMove(to.moveUci, "c6e7") &&
          trajectory.enabledPieceRole.name.equalsIgnoreCase("knight")
      case _ => false
    }, episode.dependencies)
    assert(episode.vacatedSquareLineAccessTo(enabledRoute).exists(
      _.enabledPieceRole.name.equalsIgnoreCase("knight")
    ))
    assertEquals(episode.vacatedSquareLineAccessTo(laterPressure), Nil)
    assert(episode.dependencies.exists {
      case PlanCausalEventDependency(from, to, PlanCausalDependencyKind.ObjectStatePrecondition, _, _) =>
        EvidenceRef.sameMove(from.moveUci, "c6e7") && EvidenceRef.sameMove(to.moveUci, "e7d5")
      case _ => false
    }, episode.dependencies)
    assert(episode.continuations.exists(node =>
      EvidenceRef.sameMove(node.moveUci, "e7d5") &&
        node.structuralConsequences.exists(consequence =>
          consequence.kind == TransitionConsequenceKind.TargetPressureGain &&
            consequence.positive &&
            consequence.goalSubjects.nonEmpty
        )
    ), episode.continuations)
    assert(event.futureMove.exists(EvidenceRef.sameMove(_, "e7d5")), event)
    val (expectedEvent, expectedConsequence) = episode.representativeResult
      .getOrElse(fail("expected typed representative result"))
    assert(PlanCausalFunctionalMatch.causallyEquivalent(
      episode,
      expectedEvent,
      List(expectedConsequence),
      episode,
      expectedEvent,
      List(expectedConsequence)
    ))
    assert(!PlanCausalFunctionalMatch.causallyEquivalent(
      episode,
      expectedEvent,
      List(expectedConsequence),
      episode.copy(dependencies = Nil),
      expectedEvent,
      List(expectedConsequence)
    ))

  test("a rook setup owns the later clearance pressure and keeps the intervening sacrifice line public"):
    val input = withReplyProbeFor(
      RawMoveReviewInput(
        fen = "r1bq1rk1/pppnn1bp/3p4/3Pp1p1/2P1Pp2/2N2P2/PP2BBPP/R2QNRK1 w - - 0 13",
        playedMoveUci = "a1c1",
        variations = List(
          VariationLine(List("e1d3", "e7g6", "a2a4", "d7f6", "a4a5", "h7h5", "a5a6", "b7a6"), 72, depth = 18),
          VariationLine(List("b2b4", "d7f6", "c4c5", "e7g6", "c5c6", "h7h5", "a2a4", "g8h8"), 60, depth = 18),
          VariationLine(List("a1c1", "e7g6", "c4c5", "d7c5", "b2b4", "c5a6", "c3b5", "h7h5"), 52, depth = 18)
        ),
        currentEvalCp = Some(72),
        ply = Some(24)
      ),
      "a1c1",
      List(
        VariationLine(List("e7g6", "c4c5", "d7c5", "b2b4", "c5a6", "c3b5", "h7h5"), 56, depth = 16),
        VariationLine(List("f8f7", "c4c5", "d7c5", "b2b4", "c5a6", "c3b5", "c8d7"), 58, depth = 16),
        VariationLine(List("b7b6", "b2b4", "e7g6", "c4c5", "d7f6", "a2a4", "h7h5", "c3b5"), 76, depth = 16)
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation &&
            EvidenceRef.sameMove(payload.rootMove, "a1c1") =>
        payload
    }.getOrElse(fail("expected rook-file causal event"))
    val trajectory = event.episode.toList.flatMap(_.dependencies).collectFirst {
      case PlanCausalEventDependency(_, _, PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(value), _) if value.placesPieceBeforeClearance => value
    }.getOrElse(fail("expected delayed line clearance"))
    val sacrifice = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, line: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) =>
        line.proofSignalConsequences.collectFirst {
          case value @ LineConsequence(LineConsequenceKind.Sacrifice, _, _, Some(eventMove), Some(rootMove), _, _, _)
              if EvidenceRef.sameMove(eventMove, "c4c5") && EvidenceRef.sameMove(rootMove, "a1c1") => value
        }
    }.flatten.getOrElse(fail("expected root-owned c5 sacrifice"))
    val playedSurfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(item => item.moveUci == "a1c1")
    val surface = playedSurfaces
      .find(item => item.idea.code == "plan_continuity" && item.displayPlanId.contains(PlanId.PieceActivation))
      .getOrElse(fail("expected public rook-plan continuity"))
    val publicPayload = result.packet.moveJudgmentView
      .map(MoveMeaningSurface.publicPayloadJson)
      .getOrElse(fail("expected public payload"))
    val publicExplanation = (publicPayload \ "explanations").as[List[JsObject]]
      .find(explanation => (explanation \ "move").as[String] == "a1c1")
      .getOrElse(fail("expected played-move explanation"))

    assertEquals(event.identity.goalKind, Some(PlanKind.RookFileTransfer))
    assertEquals((trajectory.enabledFrom, trajectory.vacatedSquare, trajectory.enabledTo),
      (EvidenceSquare("c1"), EvidenceSquare("c3"), EvidenceSquare("c7")))
    assert(sacrifice.proofSignal, sacrifice)
    assert(!playedSurfaces.exists(_.idea.code == "target_pressure"), playedSurfaces)
    assert(surface.target.squares.contains("c7"), surface)
    assertEquals(surface.resolvedPlanEvent.flatMap(_.testedContinuation.map(_.futureMove)), Some("c3b5"))
    assertEquals(
      (publicExplanation \ "line").as[List[String]],
      List("a1c1", "e7g6", "c4c5", "d7c5", "b2b4", "c5a6", "c3b5")
    )
    assert((publicExplanation \ "plan" \ "opponent_replies").as[List[JsObject]].nonEmpty)

  test("a pawn move that newly supports the next flank advance reaches the public plan"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "r3k2r/pbpqbppp/np6/3p4/3P4/2Q2NP1/PP1BPPBP/R3K2R w KQkq - 1 12",
          playedMoveUci = "a2a3",
          variations = List(
            VariationLine(List("f3e5", "d7e6", "e5d3", "c7c5", "d3f4", "e6d7", "d4c5", "e8g8"), 51, depth = 18),
            VariationLine(List("a2a3", "e8g8", "b2b4", "c7c5", "b4c5", "b6c5", "f3e5", "d7a4"), 32, depth = 18)
          ),
          currentEvalCp = Some(32),
          ply = Some(22)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.SpaceAdvantage && EvidenceRef.sameMove(payload.rootMove, "a2a3") =>
        payload
    }.getOrElse(fail("expected flank-preparation event"))
    val dependency = event.episode.toList.flatMap(_.dependencies).collectFirst {
      case dependency
          if dependency.kind == PlanCausalDependencyKind.PawnAdvanceSupport &&
            EvidenceRef.sameMove(dependency.from.moveUci, "a2a3") &&
            EvidenceRef.sameMove(dependency.to.moveUci, "b2b4") =>
        dependency
    }.getOrElse(fail("expected supported b-pawn advance"))
    val resolved = ResolvedPlanEvent.from(event)
    val publicExplanation = (MoveMeaningSurface.publicPayloadJson(
      result.packet.moveJudgmentView.getOrElse(fail("expected public move view"))
    ) \ "explanations")
      .as[List[play.api.libs.json.JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains("a2a3"))
      .getOrElse(fail("expected public explanation for 12.a3"))
    val publicIdeas = (publicExplanation \ "ideas").as[List[play.api.libs.json.JsObject]]
    val publicSequence = (publicExplanation \ "plan" \ "method" \ "sequence").as[List[play.api.libs.json.JsObject]]

    assert(dependency.planConnectionProven)
    assertEquals(dependency.proofSquares.map(_.key), List("a3", "b2", "b4"))
    assertEquals(resolved.preparedPawnAdvanceFiles, List("b"))
    assert(event.principalExplanationSortKey.nonEmpty)
    assert(publicSequence.exists(step => (step \ "move" \ "uci").asOpt[String].contains("b2b4")), publicSequence)
    assert(!publicIdeas.exists(idea => (idea \ "kind").asOpt[String].contains("outpost")), publicIdeas)

  test("an exact main-line pawn preparation is authoritative through its causal event"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rn2k2r/pbpnqppp/1p1pp3/6PP/2PP4/2P2N2/P3PP2/R1BQKB1R w KQkq - 1 10",
          playedMoveUci = "d1c2",
          variations = List(
            VariationLine(
              List("d1c2", "b8c6", "e2e4", "e8c8", "c1e3", "c8b8", "e1c1", "c6a5"),
              scoreCp = 0,
              depth = 18
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(18)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val dependencyEvents = result.packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if event.rootLine.role == LineNodeRole.Played &&
            EvidenceRef.sameMove(event.rootMove, "d1c2") &&
            event.goalDependencyProofReady =>
        record -> event
    }
    val publicExplanation = (MoveMeaningSurface.publicPayloadJson(
      result.packet.moveJudgmentView.getOrElse(fail("expected public move view"))
    ) \ "explanations")
      .as[List[JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains("d1c2"))
      .getOrElse(fail("expected public explanation for 10.Qc2"))
    val publicPlan = (publicExplanation \ "plan").as[JsObject]
    val publicSequence = (publicPlan \ "method" \ "sequence").as[List[JsObject]]

    assert(dependencyEvents.nonEmpty, dependencyEvents)
    assert(publicSequence.exists(step =>
      (step \ "move" \ "uci").asOpt[String].contains("d1c2") &&
        (step \ "move" \ "notation").asOpt[String].contains("10.Qc2")
    ), publicSequence)
    assert(publicSequence.exists(step =>
      (step \ "move" \ "uci").asOpt[String].contains("e2e4") &&
        (step \ "move" \ "notation").asOpt[String].contains("11.e4")
    ), publicSequence)
    assertEquals((publicPlan \ "continuation").toOption, Some(play.api.libs.json.JsNull))

  test("direct target pressure reaches the public surface with the attacked piece"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "r5k1/pp1b1pbp/3p2p1/3Pp2n/1qr1P3/2N1B1PP/PP2QPB1/1R3RK1 w - - 7 18",
          playedMoveUci = "a2a3",
          variations = List(
            VariationLine(
              List("a2a3", "b4b3", "f1c1", "c4c8", "g2f3", "h5f6", "g1g2"),
              scoreCp = 112,
              depth = 18
            )
          ),
          currentEvalCp = Some(112),
          ply = Some(34)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected public move view"))
    val pressureClaim = view.moveMeaningClaims
      .find(claim =>
        claim.publicSurfaceAdmitted &&
          claim.meaningKind == "TargetPressure" &&
          claim.targetSquares.contains("b4")
      )
      .getOrElse(fail("expected direct target-pressure claim"))
    val publicExplanation = (MoveMeaningSurface.publicPayloadJson(
      view
    ) \ "explanations")
      .as[List[JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains("a2a3"))
      .getOrElse(fail("expected public explanation for 18.a3"))
    val pressureTarget = (publicExplanation \ "ideas")
      .as[List[JsObject]]
      .find(idea => (idea \ "kind").asOpt[String].contains("target_pressure"))
      .flatMap(idea => (idea \ "target").asOpt[JsObject])
      .getOrElse(fail("expected direct target pressure"))

    assert(pressureClaim.boardCarriers.exists(carrier =>
      carrier.kind == "Piece" &&
        carrier.value == "queen" &&
          carrier.semanticRole.contains(MoveMeaningSurfaceBoardCarrier.AttackedPieceRole)
    ), pressureClaim.boardCarriers)
    assert((pressureTarget \ "squares").as[List[String]].contains("b4"), pressureTarget)
    assert((pressureTarget \ "pieces").as[List[String]].contains("queen"), pressureTarget)

  test("a pawn advance owns the induced capture and non-pawn recapture"):
    val advance = LineReplayStep(
      ply = 1,
      moveUci = "b4b5",
      fenBefore = "4k3/8/2p5/8/1P6/8/8/1R2K3 w - - 0 1",
      fenAfter = "4k3/8/2p5/1P6/8/8/8/1R2K3 b - - 0 1"
    )
    val reply = LineReplayStep(
      ply = 2,
      moveUci = "c6b5",
      fenBefore = advance.fenAfter,
      fenAfter = "4k3/8/8/1p6/8/8/8/1R2K3 w - - 0 2"
    )
    val recapture = LineReplayStep(
      ply = 3,
      moveUci = "b1b5",
      fenBefore = reply.fenAfter,
      fenAfter = "4k3/8/8/1R6/8/8/8/4K3 b - - 0 2"
    )
    val trajectory = CaptureResponseFollowUpTrajectory.find(advance, reply, recapture, List(reply))

    assertEquals(trajectory.map(_.triggerRole), Some(EvidencePieceRole("Pawn")))
    assertEquals(trajectory.map(_.followUpRole), Some(EvidencePieceRole("Rook")))
    assert(trajectory.exists(CaptureResponseFollowUpTrajectory.proves))

  test("a checking move owns the plan follow-up after the forced king reply"):
    val pawnCheck = LineReplayStep(
      ply = 3,
      moveUci = "e5e6",
      fenBefore = "R7/5kp1/3K3p/4P3/8/8/8/7r w - - 2 63",
      fenAfter = "R7/5kp1/3KP2p/8/8/8/8/7r b - - 0 63"
    )
    val kingReply = LineReplayStep(
      ply = 4,
      moveUci = "f7f6",
      fenBefore = pawnCheck.fenAfter,
      fenAfter = "R7/6p1/3KPk1p/8/8/8/8/7r w - - 1 64"
    )
    val rookCheck = LineReplayStep(
      ply = 5,
      moveUci = "a8f8",
      fenBefore = kingReply.fenAfter,
      fenAfter = "5R2/6p1/3KPk1p/8/8/8/8/7r b - - 2 64"
    )
    val secondKingReply = LineReplayStep(
      ply = 6,
      moveUci = "f6g5",
      fenBefore = rookCheck.fenAfter,
      fenAfter = "5R2/6p1/3KP2p/6k1/8/8/8/7r w - - 3 65"
    )
    val pawnFollowUp = LineReplayStep(
      ply = 7,
      moveUci = "e6e7",
      fenBefore = secondKingReply.fenAfter,
      fenAfter = "5R2/4P1p1/3K3p/6k1/8/8/8/7r b - - 0 65"
    )
    val first = CheckResponseFollowUpTrajectory.find(pawnCheck, kingReply, rookCheck, List(kingReply))
    val second = CheckResponseFollowUpTrajectory.find(rookCheck, secondKingReply, pawnFollowUp, List(secondKingReply))

    assertEquals(first.map(_.involvedRoles.map(_.name)), Some(List("Pawn", "King", "Rook")))
    assertEquals(second.map(_.involvedRoles.map(_.name)), Some(List("Rook", "King", "Pawn")))
    assert(first.exists(CheckResponseFollowUpTrajectory.proves))
    assert(second.exists(CheckResponseFollowUpTrajectory.proves))

  test("reply testing exposes the verified result at the end of one causal path"):
    val base = RawMoveReviewInput(
      fen = "R7/5kp1/3K3p/7r/4P3/8/8/8 w - - 0 62",
      playedMoveUci = "e4e5",
      variations = List(VariationLine(
        List("e4e5", "h5h1", "e5e6", "f7g6", "e6e7", "h1d1", "d6c5"),
        scoreCp = 379,
        depth = 18
      )),
      currentEvalCp = Some(379),
      ply = Some(122)
    )
    val pendingResult = MoveReviewJudgmentOrchestrator.build(base).getOrElse(fail("expected pending result"))
    val pending = causalEvent(pendingResult, "e4e5", "e6e7")

    assertEquals(pending.publicTailExpectedResult.map(_._1.moveUci), Some("e6e7"))
    assertEquals(pending.canonicalPublicTailAssessment, None)
    assertEquals(ResolvedPlanEvent.from(pending).testedContinuation, None)
    assertEquals(
      branchReplyRequests(pendingResult)
        .find(_.candidateMove.exists(EvidenceRef.sameMove(_, "e4e5")))
        .flatMap(_.horizon),
      Some(BranchReplyProbeBinding.horizon(4))
    )
    val replyLines = List(
      VariationLine(
        List("h5h4", "e5e6", "f7f6", "a8f8", "f6g5", "e6e7", "h4e4", "e7e8q", "e4e8", "f8e8"),
        447,
        depth = 18
      ),
      VariationLine(
        List("h5h2", "e5e6", "f7f6", "a8f8", "f6g5", "e6e7", "h2e2", "e7e8q", "e2e8", "f8e8"),
        456,
        depth = 18
      ),
      VariationLine(
        List(
          "h5h1", "e5e6", "f7f6", "a8f8", "f6g5", "e6e7", "h1d1", "d6c5", "d1c1", "c5d4",
          "c1d1", "d4c3", "d1e1", "e7e8q", "e1e8", "f8e8"
        ),
        466,
        depth = 18
      )
    )
    val firstProbeInput = withReplyProbeFor(base, "e4e5", replyLines)
    val firstProbeResult = MoveReviewJudgmentOrchestrator.build(firstProbeInput).getOrElse(fail("expected wider tail probe"))
    val firstProbeEvent = causalEvent(firstProbeResult, "e4e5", "e6e7")
    assertEquals(firstProbeEvent.canonicalPublicTailAssessment.map(_.sourceEvent.moveUci), Some("e5e6"))
    assertEquals(
      firstProbeEvent.canonicalPublicTailAssessment.map(_.robustness),
      Some(PlanCausalRobustness.Robust)
    )
    assertEquals(
      firstProbeEvent.publicTailExpectedResultAssessment.map(_.robustness),
      Some(PlanCausalRobustness.Deferred)
    )
    assertEquals(firstProbeEvent.requiredHorizonPlyOffset, 6)
    assert(firstProbeEvent.branchWitnesses.forall(witness =>
      witness.outcome == PlanCausalBranchOutcome.Deferred &&
        witness.requiredPlyOffset == 6 &&
        witness.observedThroughPlyOffset == 4
    ), firstProbeEvent.branchWitnesses)
    assert(!firstProbeEvent.publicTailCoverageComplete)
    assert(!firstProbeEvent.episodePublicProofReady)
    assertEquals(ResolvedPlanEvent.from(firstProbeEvent).testedContinuation, None)
    val widerRequest = branchReplyRequests(firstProbeResult)
      .find(_.candidateMove.exists(EvidenceRef.sameMove(_, "e4e5")))
      .getOrElse(fail("expected wider reply request"))
    assertEquals(widerRequest.horizon, Some(BranchReplyProbeBinding.horizon(6)))
    val input = withReplyProbeFor(firstProbeInput, "e4e5", replyLines)
    val skippedRound = MoveReviewJudgmentOrchestrator
      .build(base.copy(probeResults = List(input.probeResults.last)))
      .getOrElse(fail("expected fail-closed skipped-round result"))
    assert(skippedRound.packet.probeDiagnostics.exists(_.reasonCodes.contains("PROBE_REQUEST_UNISSUED")))
    assertEquals(causalEvent(skippedRound, "e4e5", "e6e7").branchWitnesses, Nil)
    assertEquals(
      branchReplyRequests(skippedRound)
        .find(_.candidateMove.exists(EvidenceRef.sameMove(_, "e4e5")))
        .flatMap(_.horizon),
      Some(BranchReplyProbeBinding.horizon(4))
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "e4e5", "e6e7")
    val episode = event.episode.getOrElse(fail("expected causal episode"))
    val tail = event.canonicalPublicTailAssessment.map(_.sourceEvent).getOrElse(fail("expected verified tail"))
    val path = episode.enablingPathTo(tail).getOrElse(fail("expected path to verified tail"))
    val dependencies = episode.enablingDependenciesTo(tail)
    val publicEvent = ResolvedPlanEvent.from(event)

    assertEquals(event.publicTailExpectedResult.map(_._1.moveUci), Some("e6e7"))
    assertEquals(event.requiredHorizonPlyOffset, 6)
    assertEquals(path.map(_.moveUci), List("e4e5", "e5e6", "a8f8", "e6e7"))
    assertEquals(
      dependencies.map(_.kind),
      List(
        PlanCausalDependencyKind.ObjectStatePrecondition,
        PlanCausalDependencyKind.ResponseContinuationPrecondition,
        PlanCausalDependencyKind.ResponseContinuationPrecondition
      )
    )
    assertEquals(
      dependencies.collect {
        case PlanCausalEventDependency(
              _,
              _,
              PlanCausalDependencyKind.ResponseContinuationPrecondition,
              PlanCausalDependencyProof.ResponseContinuation(trajectory),
              _
            ) => trajectory.replyStep.moveUci
      },
      List("f7f6", "f6g5")
    )
    assertEquals(event.branchWitnesses.size, event.expectedReplyCount)
    assert(event.branchWitnesses.forall(witness =>
      witness.outcome == PlanCausalBranchOutcome.Realized &&
        witness.requiredPlyOffset == 6 &&
        witness.observedThroughPlyOffset == 6
    ), event.branchWitnesses)
    assertEquals(event.branchWitnesses.map(_.sourceProbeId).distinct, List(widerRequest.id))
    assert(event.causalResultAssessments.forall(_.sourcePlyOffset <= 6), event.causalResultAssessments)
    assertEquals(branchReplyRequests(result), Nil)
    assertEquals(
      publicEvent.testedContinuation.toList.flatMap(_.sequence.map(_.move)),
      List("e4e5", "e5e6", "a8f8", "e6e7")
    )
    assertEquals(publicEvent.representativeResult.flatMap(_.source).map(_.uci), Some("e6e7"))
    assert(publicEvent.responses.exists(response =>
      EvidenceRef.sameMove(response.move, "f7f6") && response.realizationMove.exists(EvidenceRef.sameMove(_, "a8f8"))
    ), (episode.responses, dependencies, publicEvent.responses))
    assert(publicEvent.responses.exists(response =>
      EvidenceRef.sameMove(response.move, "f6g5") && response.realizationMove.exists(EvidenceRef.sameMove(_, "e6e7"))
    ), publicEvent.responses)

  test("a central pawn challenge is not a king-shelter concession"):
    val triggerStep = LineReplayStep(
      ply = 1,
      moveUci = "d7d5",
      fenBefore = "4k3/3p4/8/8/8/8/4P3/4K3 b - - 0 1",
      fenAfter = "4k3/8/8/3p4/8/8/4P3/4K3 w - - 0 2"
    )
    val responseStep = LineReplayStep(
      ply = 2,
      moveUci = "e2e4",
      fenBefore = triggerStep.fenAfter,
      fenAfter = "4k3/8/8/3p4/4P3/8/8/4K3 b - - 0 2"
    )
    val trigger = PlanCausalEventNode(
      identity = PlanEventIdentity(
        rootMove = "d7d5",
        goalTheme = PlanTheme.PawnBreakPreparation,
        goalKind = None,
        actorRole = Some("pawn"),
        actorFrom = Some("d7"),
        actorTo = Some("d5"),
        targets = List("d5"),
        results = Nil
      ),
      step = triggerStep,
      perspective = Color.Black,
      structuralConsequences = Nil,
      developmentChoices = Nil
    )
    val response = PlanCausalResponse(trigger, responseStep, plyOffset = 1)

    assert(response.attacksPlanPiece, response)
    assert(!response.weakensKingShelter, response)

  test("a newly controlled retreat square binds a later pawn advance as an internal dependency"):
    val result = MoveReviewJudgmentOrchestrator
      .build(supportedPawnAdvanceInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation && EvidenceRef.sameMove(payload.rootMove, "e2g3") =>
        payload
    }.getOrElse(fail("expected knight-support event"))
    val episode = event.episode.getOrElse(fail("expected pawn-advance episode"))
    val (dependency, trajectory) = episode.dependencies.collectFirst {
      case dependency @ PlanCausalEventDependency(
            _,
            _,
            PlanCausalDependencyKind.RetreatControlPrecondition,
            PlanCausalDependencyProof.RetreatControl(trajectory),
            _
          ) if EvidenceRef.sameMove(dependency.from.moveUci, "e2g3") &&
            EvidenceRef.sameMove(dependency.to.moveUci, "g4g5") =>
        dependency -> trajectory
    }.getOrElse(fail("expected retreat-control dependency"))
    val publicEvent = ResolvedPlanEvent.from(event)

    assert(dependency.planConnectionProven)
    assert(dependency.enablesContinuation)
    assertEquals(trajectory.pressuredSquare, EvidenceSquare("f6"))
    assertEquals(trajectory.controlledRetreatSquare, EvidenceSquare("h5"))
    assertEquals(dependency.proofSquares, List(EvidenceSquare("f6"), EvidenceSquare("h5")))
    assertEquals(dependency.proofPieceRoles.map(_.name.toLowerCase), List("knight", "knight"))
    assert(episode.rootEnabledSteps.exists(event => EvidenceRef.sameMove(event.moveUci, "g4g5")))
    assert(!episode.rootEnabledSteps.exists(event => EvidenceRef.sameMove(event.moveUci, "b2b3")))
    assertEquals(event.requiredHorizonPlyOffset, 2)
    assert(event.causalResultAssessments.exists(assessment =>
      assessment.consequence.subjects.exists(_.toLowerCase.contains("f6"))
    ), s"goal=${event.identity.goalKind} direct=${event.structuralConsequences} episode=${episode.events.map(event => event.moveUci -> event.structuralConsequences)} causal=${event.causalResultAssessments}")
    assert(!event.episodePublicProofReady)
    assert(event.responseGoalResults.nonEmpty)
    assertEquals(event.planVerifiedResponseGoalResults, Nil)
    assertEquals(publicEvent.results.filterNot(_.stage == "direct"), Nil)
    assertEquals(publicEvent.responses, Nil)
    assertEquals(publicEvent.testedContinuation, None)
    assert(result.packet.probeRequests.exists(_.candidateMove.exists(EvidenceRef.sameMove(_, "e2g3"))))

  test("tested knight support owns the next pawn advance without reversing event order"):
    val input = withReplyProbeFor(
      supportedPawnAdvanceInput(),
      "e2g3",
      List(
        VariationLine(List("d7b6", "g4g5"), 0, depth = 16),
        VariationLine(List("a7a5", "g4g5"), 0, depth = 16),
        VariationLine(List("f8e8", "g4g5"), 0, depth = 16)
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.KingsideAttack &&
            EvidenceRef.sameMove(payload.rootMove, "e2g3") =>
        payload
    }.getOrElse(fail("expected tested kingside event"))
    val pawnAdvance = event.episode.toList.flatMap(_.rootEnabledSteps).find(node =>
      EvidenceRef.sameMove(node.moveUci, "g4g5")
    ).getOrElse(fail("expected enabled pawn advance"))
    val publicEvent = ResolvedPlanEvent.from(event)
    val continuation = publicEvent.testedContinuation.getOrElse(fail("expected tested continuation"))
    val sequenceSummary = event.planSequenceSummary.getOrElse(fail("expected proven plan sequence"))

    assert(pawnAdvance.structuralConsequences.exists(event.resultAdvancesGoal(pawnAdvance, _)))
    assert(event.episodePublicProofReady)
    assertEquals(publicEvent.goalTheme, PlanTheme.WingPlay.id)
    assertEquals(publicEvent.transitionType, Some(TransitionType.Opening))
    assertEquals(publicEvent.moveRole, Some(PlanMoveRole.Preparation))
    assertEquals(sequenceSummary.currentEvent, Some(event.identity))
    assertEquals(sequenceSummary.previousEvent, None)
    assertEquals(sequenceSummary.continuity.flatMap(_.startingEvent), Some(event.identity))
    assert(sequenceSummary.continuity.exists(_.supportingEvents.exists(future =>
      EvidenceRef.sameMove(future.rootMove, "g4g5")
    )))
    assertEquals(continuation.futureMove, "g4g5")
    assertEquals(continuation.dependencyKind, PlanCausalDependencyKind.RetreatControlPrecondition)
    assertEquals(continuation.sequence.take(2).map(_.move), List("e2g3", "g4g5"))

  test("a completed advance keeps its latest active preparation instead of ambient control"):
    val result = MoveReviewJudgmentOrchestrator
      .build(preparedKingsideAdvanceInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "g4g5") &&
            payload.episode.exists(_.historySequenceProven) =>
        payload
    }.getOrElse(fail("expected completed kingside episode"))
    val episode = event.episode.getOrElse(fail("expected historical episode"))

    assertEquals(episode.historicalSequence.map(_.moveUci), List("e2g3", "g4g5"))
    assertEquals(event.planSequenceSummary.map(_.transitionType), Some(TransitionType.Completion))
    assertEquals(
      ResolvedPlanEvent.from(event).historySequence.flatMap(_.moveReference.map(_.notation)),
      List("12.Ng3", "13.g5")
    )

  test("a historical plan move uses its completed replay ply when the FEN clock is stale"):
    val step = LineReplayStep(
      ply = 21,
      moveUci = "c3e4",
      fenBefore = "r1bqr1k1/pp1n1ppp/2pbp3/8/2PPn3/2N3P1/PP3PBP/R1BQ1RK1 w - - 0 1",
      fenAfter = "r1bqr1k1/pp1n1ppp/2pbp3/8/2PPN3/6P1/PP3PBP/R1BQ1RK1 b - - 0 1"
    )

    assertEquals(NumberedChessMove.fromHistoricalStep(step).map(_.notation), Some("11.Nxe4"))

  test("flank episode preserves the inducing move, ...h6 reply, and knight route"):
    val result = MoveReviewJudgmentOrchestrator
      .build(syntheticFlankEpisodeInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.KingsideAttack && EvidenceRef.sameMove(payload.rootMove, "h2h4") =>
        payload
    }.getOrElse(fail("expected kingside causal event"))
    val episode = event.episode.getOrElse(fail("expected flank episode"))
    val kingPressure = event.directGoalConsequences
      .find(_.kind == TransitionConsequenceKind.KingSafetyPressure)
      .getOrElse(fail(s"expected current h-pawn pressure to prove the wing goal; event=$event"))
    val publicEvent = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .flatMap(_.resolvedPlanEvent)
      .find(resolved =>
        EvidenceRef.sameMove(resolved.rootMove, "h2h4") &&
          resolved.results.exists(_.kind == TransitionConsequenceKind.KingSafetyPressure)
      )
      .getOrElse(fail("expected current h-pawn pressure in the public plan"))

    assert(episode.planSequenceProven, episode)
    assert(kingPressure.subjects.exists(_.startsWith("king:g8")), kingPressure)
    assert(kingPressure.subjects.contains("file:h"), kingPressure)
    assert(publicEvent.results.exists(_.kind == TransitionConsequenceKind.KingSafetyPressure), publicEvent)
    val episodeMoves = episode.events.map(_.moveUci)
    val h4Index = episodeMoves.indexWhere(EvidenceRef.sameMove(_, "h2h4"))
    val ng5Index = episodeMoves.indexWhere(EvidenceRef.sameMove(_, "f3g5"))
    val nh3Index = episodeMoves.indexWhere(EvidenceRef.sameMove(_, "g5h3"))
    val dependencySummary = episode.dependencies.map(dependency =>
      s"${dependency.kind}:${dependency.from.moveUci}->${dependency.to.moveUci}:${dependency.proof}"
    )
    assert(h4Index >= 0 && h4Index < ng5Index && ng5Index < nh3Index, episodeMoves)
    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.FlankAdvanceCoordination &&
        EvidenceRef.sameMove(dependency.from.moveUci, "h2h4") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "f3g5")
    ), dependencySummary)
    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.ObjectStatePrecondition &&
        EvidenceRef.sameMove(dependency.from.moveUci, "f3g5") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "g5h3")
    ), episode.dependencies)
    val inducedPawnMove = episode.responses.find(response =>
      EvidenceRef.sameMove(response.trigger.moveUci, "f3g5") &&
        EvidenceRef.sameMove(response.step.moveUci, "h7h6")
    ).getOrElse(fail(s"expected ...h6 to answer Ng5; responses=${episode.responses}"))
    val inducedWeakness = inducedPawnMove.structuralConsequences
      .find(_.kind == TransitionConsequenceKind.KingSafetyPressure)
      .getOrElse(fail(s"expected ...h6 to leave the black king shelter; response=$inducedPawnMove"))
    val publicInducedWeakness = publicEvent.results.find(result =>
      result.kind == TransitionConsequenceKind.KingSafetyPressure &&
        result.source.exists(source => EvidenceRef.sameMove(source.uci, "h7h6"))
    ).getOrElse(fail(s"expected the ...h6 consequence in the public plan; event=$publicEvent"))
    assert(inducedPawnMove.attacksPlanPiece, inducedPawnMove)
    assert(inducedPawnMove.weakensKingShelter, inducedPawnMove)
    assert(inducedPawnMove.proven, inducedPawnMove)
    assert(inducedWeakness.subjects.contains("king:g8"), inducedWeakness)
    assert(publicInducedWeakness.conditions.exists(_.move == "h7h6"), publicInducedWeakness)

  test("a causal plan event ranks its proved chess function before a weaker incidental restraint"):
    val root = PositionNodeRef("8/8/8/8/2B5/2N5/8/4K2k w - - 0 1", 1, Some(Color.White))
    val after = PositionNodeRef("8/8/8/1B6/8/4N3/8/4K2k b - - 1 1", 2, Some(Color.Black))
    def event(
        move: String,
        planId: PlanId,
        theme: PlanTheme,
        kind: PlanKind,
        role: String,
        consequence: TransitionConsequence
    ) =
      val line = LineNodeRef(s"played-$move", move, 1, LineNodeRole.Played)
      PlanCausalEventEvidence(
        planId = planId,
        identity = PlanEventIdentity(
          rootMove = move,
          goalTheme = theme,
          goalKind = Some(kind),
          actorRole = Some(role),
          actorFrom = Some(move.take(2)),
          actorTo = Some(move.slice(2, 4)),
          targets = consequence.goalSubjects,
          results = List(s"kind:${consequence.kind.toString.toLowerCase}")
        ),
        rootLine = line,
        rootTransition = StructuralTransitionBinding(
          moveUci = move,
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(line),
          perspective = Color.White
        ),
        structuralConsequences = List(consequence),
        developmentChoices = Nil,
        branchWitnesses = Nil
      )

    val restriction = event(
      "c3e2",
      PlanId.Prophylaxis,
      PlanTheme.RestrictionProphylaxis,
      PlanKind.ProphylaxisRestraint,
      "knight",
      TransitionConsequence(
        TransitionConsequenceKind.OpponentMobilityRestriction,
        StructuralSignalPolarity.Gain,
        1,
        List("pawn:f6-f5:advance-restricted")
      )
    )
    val pieceImprovement = event(
      "c3e2",
      PlanId.PieceActivation,
      PlanTheme.PieceRedeployment,
      PlanKind.WorstPieceImprovement,
      "knight",
      TransitionConsequence(
        TransitionConsequenceKind.MobilityGain,
        StructuralSignalPolarity.Gain,
        2,
        List("piece:knight:e2")
      )
    )
    val exchange = event(
      "d7b5",
      PlanId.Exchange,
      PlanTheme.FavorableExchange,
      PlanKind.SimplificationWindow,
      "bishop",
      TransitionConsequence(
        TransitionConsequenceKind.PieceExchangeAvailable,
        StructuralSignalPolarity.Gain,
        1,
        List("piece:bishop:d3")
      )
    )
    val pawnAdvancePreparation = event(
      "c4b5",
      PlanId.PawnBreakPreparation,
      PlanTheme.PawnBreakPreparation,
      PlanKind.CentralBreakTiming,
      "bishop",
      TransitionConsequence(
        TransitionConsequenceKind.LineUnlockGain,
        StructuralSignalPolarity.Gain,
        1,
        List("pawn:c3-c4:line-unlock:by:c4b5:mobility+1")
      )
    )
    val ordering = summon[Ordering[(Int, Int, Int, Int)]]
    val restrictionKey = restriction.principalExplanationSortKey.getOrElse(fail("expected restriction priority"))
    assert(ordering.gt(
      pieceImprovement.principalExplanationSortKey.getOrElse(fail("expected piece-improvement priority")),
      restrictionKey
    ))
    assert(ordering.gt(
      exchange.principalExplanationSortKey.getOrElse(fail("expected exchange priority")),
      restrictionKey
    ))
    assert(ordering.gt(
      pawnAdvancePreparation.principalExplanationSortKey.getOrElse(fail("expected pawn-advance priority")),
      restrictionKey
    ))

  test("resolved plan event targets come from its results rather than its method"):
    val root = PositionNodeRef("4k3/4p3/8/1P6/8/8/8/3RK3 w - - 0 1", 1, Some(Color.White))
    val after = PositionNodeRef("4k3/4p3/8/8/1P6/8/8/3RK3 b - - 1 1", 2, Some(Color.Black))

    def event(
        move: String,
        from: String,
        to: String,
        targets: List[String],
        consequences: List[TransitionConsequence],
        planId: PlanId,
        goalTheme: PlanTheme,
        goalKind: Option[PlanKind],
        actorRole: String
    ): PlanCausalEventEvidence =
      val line = LineNodeRef(s"played-$move", move, 1, LineNodeRole.Played)
      PlanCausalEventEvidence(
        planId = planId,
        identity = PlanEventIdentity(
          rootMove = move,
          goalTheme = goalTheme,
          goalKind = goalKind,
          actorRole = Some(actorRole),
          actorFrom = Some(from),
          actorTo = Some(to),
          targets = targets,
          results = consequences.map(consequence => s"kind:${consequence.kind.toString.toLowerCase}")
        ),
        rootLine = line,
        rootTransition = StructuralTransitionBinding(
          moveUci = move,
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(line),
          perspective = Color.White
        ),
        structuralConsequences = consequences,
        developmentChoices = Nil,
        branchWitnesses = Nil
      )

    val pressure = event(
      move = "b5b4",
      from = "b5",
      to = "b4",
      targets = List("a3", "b4", "c3", "created-tension:b4-a3", "square:a3", "square:b4", "square:c3"),
      consequences = List(
        TransitionConsequence(TransitionConsequenceKind.TargetPressureGain, StructuralSignalPolarity.Gain, 2, List("a3", "c3")),
        TransitionConsequence(
          TransitionConsequenceKind.PawnTensionGain,
          StructuralSignalPolarity.Gain,
          1,
          List("created-tension:b4-a3", "break-file:b")
        )
      ),
      planId = PlanId.PawnStorm,
      goalTheme = PlanTheme.WingPlay,
      goalKind = Some(PlanKind.WingExpansion),
      actorRole = "pawn"
    )
    val tension = event(
      move = "e7e6",
      from = "e7",
      to = "e6",
      targets = List("break-file:e", "created-tension:e6-d5", "square:d5", "square:e6"),
      consequences = List(
        TransitionConsequence(
          TransitionConsequenceKind.PawnTensionGain,
          StructuralSignalPolarity.Gain,
          1,
          List("created-tension:e6-d5", "break-file:e")
        )
      ),
      planId = PlanId.PawnBreakPreparation,
      goalTheme = PlanTheme.PawnBreakPreparation,
      goalKind = Some(PlanKind.CentralBreakTiming),
      actorRole = "pawn"
    )
    val battery = event(
      move = "d1d4",
      from = "d1",
      to = "d4",
      targets = List("battery:file:d4-d5:rook-rook", "d:d4", "rook-lift:d1-d4:rank-4", "square:d4", "square:d5"),
      consequences = List(
        TransitionConsequence(
          TransitionConsequenceKind.BatteryPressureGain,
          StructuralSignalPolarity.Gain,
          1,
          List("battery:file:d4-d5:rook-rook"),
          targetSubjects = List("pawn:d6")
        ),
        TransitionConsequence(TransitionConsequenceKind.FileOccupationGain, StructuralSignalPolarity.Gain, 1, List("d:d4")),
        TransitionConsequence(
          TransitionConsequenceKind.RookLiftActivation,
          StructuralSignalPolarity.Gain,
          1,
          List("rook-lift:d1-d4:rank-4")
        )
      ),
      planId = PlanId.RookActivation,
      goalTheme = PlanTheme.PieceRedeployment,
      goalKind = Some(PlanKind.RookFileTransfer),
      actorRole = "rook"
    )

    assertEquals(
      ResolvedPlanEvent.from(pressure).targets,
      List("a3", "break-file:b", "c3", "created-tension:b4-a3", "square:a3", "square:c3")
    )
    assertEquals(
      ResolvedPlanEvent.from(tension).targets,
      List("break-file:e", "created-tension:e6-d5", "square:d5")
    )
    assertEquals(
      ResolvedPlanEvent.from(battery).targets,
      List("file:d", "pawn:d6", "square:d6")
    )
    assertEquals(
      ResolvedPlanEvent.from(battery).results.find(_.kind == TransitionConsequenceKind.BatteryPressureGain).map(_.subjects),
      Some(List("pawn:d6"))
    )

  test("line trajectory does not transfer identity to a same-role replacement"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "4k3/8/8/8/6b1/8/3N4/4K1N1 w - - 0 1",
          playedMoveUci = "g1f3",
          variations = List(
            VariationLine(List("g1f3", "g4f3", "d2f3", "e8e7", "f3e5"), scoreCp = 0, depth = 12)
          ),
          currentEvalCp = Some(0),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph
    val trajectory = graph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) =>
        payload.futureRootObjectMove(payload.lineReplayCount)
    }.flatten

    assertEquals(trajectory, None)

  test("an untested future plan event does not become reply-tested proof"):
    val result = MoveReviewJudgmentOrchestrator
      .build(syntheticQueensideEpisodeInput())
      .getOrElse(fail("expected judgment result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "b7b5") && payload.episode.exists(
            _.events.exists(event => EvidenceRef.sameMove(event.moveUci, "b5b4"))
          ) =>
        record -> payload
    }.getOrElse(fail("expected causal event"))

    assert(eventRecord._2.counterfactualContinuationProven)
    assertEquals(eventRecord._2.robustness, PlanCausalRobustness.Untested)
    assert(!eventRecord._2.episodePublicProofReady)
    val publicEvent = ResolvedPlanEvent.from(eventRecord._2)
    assertEquals(eventRecord._2.planSequenceSummary, None)
    assertEquals(publicEvent.transitionType, None)
    assertEquals(publicEvent.moveRole, Some(PlanMoveRole.Preparation))
    assert(publicEvent.results.forall(_.stage == "direct"), publicEvent.results)
    assertEquals(publicEvent.responses, Nil)
    assertEquals(publicEvent.testedContinuation, None)
    assertEquals(branchReplyRequests(result).flatMap(_.candidateMove), List("b7b5"))
    assertEquals(
      result.packet.probeRequests.flatMap(_.horizon),
      List(BranchReplyProbeBinding.horizon(eventRecord._2.requiredHorizonPlyOffset))
    )
    assert(!EvidenceObjectBinding.fromEvidenceRefs(result.packet.evidenceGraph, List(eventRecord._1.ref)).exists(binding =>
      binding.witness.exists(obj => obj.kind == EvidenceObjectKind.Move && obj.key == "b5b4")
    ))

  test("reply probe certifies future trajectory under event-owned plan authority"):
    val input = syntheticQueensideComparisonInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assert(event.branchWitnesses.forall(_.outcome == PlanCausalBranchOutcome.Realized), event.branchWitnesses)
    assertEquals(event.robustness, PlanCausalRobustness.Robust)
    assert(event.episodePublicProofReady)
    val publicEvent = ResolvedPlanEvent.from(event)
    val causalProof = publicEvent.testedContinuation.getOrElse(fail("expected public causal episode"))
    assertEquals(causalProof.futureMove, "b5b4")
    assertEquals(publicEvent.rootMoveReference.map(_.notation), Some("10...b5"))
    assert(!publicEvent.results.exists(_.stage == "direct"), publicEvent.results)
    assertEquals(causalProof.representativeResult.flatMap(_.source).map(_.notation), Some("14...b4"))
    assertEquals(causalProof.sequence.map(_.move), List("b7b5", "b5b4"))
    assertEquals(publicEvent.results.map(_.stage).distinct, List("future"))
    assert(!result.packet.probeRequests.exists(_.objective.contains(BranchReplyProbeBinding.Objective)))
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val omittedMoves = event.episode.toList.flatMap(_.planSteps.map(_.moveUci)).filterNot(move =>
      causalProof.sequence.exists(step => EvidenceRef.sameMove(step.move, move))
    )
    assert(omittedMoves.nonEmpty)
    val eventClaim = view.moveMeaningClaims.find(_.resolvedPlanEvent.contains(publicEvent)).getOrElse(
      fail("expected public claim owned by the causal event")
    )
    assert(!eventClaim.boardCarriers.exists(carrier =>
      carrier.role == "witness" && omittedMoves.exists(EvidenceRef.sameMove(_, carrier.value))
    ), eventClaim.boardCarriers)
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.episode.exists(_.events.exists(event => EvidenceRef.sameMove(event.moveUci, "b5b4"))) => record
    }.getOrElse(fail("expected internal causal event"))
    val eventPayload = eventRecord.payload.asInstanceOf[PlanCausalEventEvidence]
    val firstWitness = eventPayload.branchWitnesses.headOption.getOrElse(fail("expected branch witness"))
    val invalidBranchRecord = eventRecord.copy(payload = eventPayload.copy(
      branchWitnesses = firstWitness.copy(requiredPlyOffset = firstWitness.requiredPlyOffset + 1) ::
        eventPayload.branchWitnesses.tail
    ))
    val invalidBranchGraph = result.packet.evidenceGraph.copy(
      records = result.packet.evidenceGraph.records.map(record =>
        if record.ref.id == invalidBranchRecord.ref.id then invalidBranchRecord else record
      )
    )
    val invalidBranchValidation = JudgmentPacketValidator.validate(
      result.packet.copy(evidenceGraph = invalidBranchGraph)
    )
    assert(
      invalidBranchValidation.issues.exists(issue =>
        issue.subjectId == eventRecord.ref.id &&
          issue.kind == JudgmentPacketValidationIssueKind.InvalidPlanCausalBranchProof
      ),
      invalidBranchValidation.issues
    )
    val publicJson = MoveMeaningSurface.publicPayloadJson(view)
    assert((publicJson \\ "next_move").exists(value => (value \ "uci").asOpt[String].contains("b5b4")), publicJson)
    assert((publicJson \\ "notation").exists(_.asOpt[String].contains("14...b4")), publicJson)
    assert((publicJson \\ "ply_offset").isEmpty, publicJson)
    val planEventJson = (publicJson \\ "plan").collectFirst {
      case value: play.api.libs.json.JsObject
          if (value \ "continuation" \ "next_move" \ "uci").asOpt[String].contains("b5b4") =>
        value
    }.getOrElse(fail("expected public plan"))
    val sequenceMoves = (planEventJson \ "method" \ "sequence")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .flatMap(step => (step \ "move" \ "uci").asOpt[String])
    assert(sequenceMoves.contains("b7b5") && sequenceMoves.contains("b5b4"), sequenceMoves)
    val futureResultJson = (planEventJson \ "results")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .find(result =>
        (result \ "tested_reply_status").asOpt[String].contains("the continuation occurs against the tested replies")
      )
      .getOrElse(fail("expected robust future result"))
    val publicReplies = (planEventJson \ "opponent_replies")
      .asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil)
    assert(publicReplies.exists(reply =>
      (reply \ "continuation" \ "notation").asOpt[String].contains("14...b4")
    ), publicReplies)
    assert(publicReplies.forall(reply =>
      (reply \ "effect_on_plan").asOpt[String].contains("the planned continuation occurs")
    ), publicReplies)
    assert((play.api.libs.json.JsArray(publicReplies) \\ "ply_offset").isEmpty, publicReplies)
    val semanticCodes = (publicJson \\ "ideas")
      .flatMap(_.asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil))
      .flatMap(idea => (idea \ "kind").asOpt[String])
    val planIndex = semanticCodes.indexOf("plan_continuity")
    val specificIndex = semanticCodes.indexWhere(_ != "plan_continuity")
    assert(specificIndex >= 0 && (planIndex < 0 || specificIndex < planIndex), semanticCodes)

  test("a quiet played move discovers a typed plan without lending authority to its seed"):
    val initial = MoveReviewJudgmentOrchestrator
      .build(stonewallRb1Input())
      .getOrElse(fail("expected discovery request"))
    val request = branchReplyRequests(initial)
      .find(_.candidateMove.exists(EvidenceRef.sameMove(_, "a1b1")))
      .getOrElse(fail("expected Rb1 discovery request"))
    assertEquals(request.horizon, Some(BranchReplyProbeBinding.horizon(6)))
    assert(!initial.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces).exists(surface =>
      surface.subject == "played_move" && surface.idea.code == "pawn_break_timing"
    ))
    val input = withReplyProbeFor(
      stonewallRb1Input(),
      "a1b1",
      List(
        VariationLine(List("d6c7", "b2b3", "a7a5", "c1f4", "c7f4", "g3f4", "b8d7", "b1c1"), 61, depth = 16),
        VariationLine(List("b8d7", "b2b4", "b7b6", "a2a4", "c8b7", "f3d2", "e4d2", "c1d2"), 61, depth = 16),
        VariationLine(List("b7b6", "b2b4", "b8d7", "a2a4", "c8b7", "f3d2", "d8e8", "c3e4"), 68, depth = 16)
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected discovered result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(event.rootMove, "a1b1") &&
            event.identity.goalTheme == PlanTheme.PawnBreakPreparation &&
            event.futureMove.exists(EvidenceRef.sameMove(_, "b2b4")) =>
        record -> event
    }.getOrElse(fail("expected branch-observed b-pawn plan"))

    assert(eventRecord._2.continuationSourceLine.exists(_.role == LineNodeRole.Threat))
    assert(eventRecord._2.episode.exists(_.dependencies.exists(dependency =>
      dependency.from == eventRecord._2.episode.get.root && dependency.enablesContinuation
    )))
    assert(!result.packet.probeRequests.exists(_.id == request.id))

  test("an admitted off-main pawn plan seeds the causal event but stays private until reply coverage"):
    val seeded = withOpponentResourceProbes(
      stonewallRb1Input(),
      List(ResourceProbeOutcome(
        rootMove = "a1b1",
        resourceMove = "b7b6",
        whitePovEvalCp = 63,
        moves = List("b7b6", "b2b4", "b8d7", "a2a4", "c8b7", "f3d2")
      ))
    )
    val seededResult = MoveReviewJudgmentOrchestrator.build(seeded).getOrElse(fail("expected seeded result"))
    val seededRecord = seededResult.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(event.rootMove, "a1b1") &&
            event.identity.goalTheme == PlanTheme.PawnBreakPreparation &&
            event.futureMove.exists(EvidenceRef.sameMove(_, "b2b4")) =>
        record -> event
    }.getOrElse(fail("expected branch-observed b-pawn plan"))

    assert(
      !seededResult.validation.issues.exists(_.subjectId == seededRecord._1.ref.id),
      seededResult.validation.issues
    )
    assert(seededRecord._2.counterfactualContinuationProven)
    assert(seededRecord._2.continuationSourceLine.exists(_.role == LineNodeRole.Threat))
    assert(!seededRecord._2.branchCoverageComplete)
    assert(!seededRecord._2.publicGoalProofReady)
    assertEquals(
      branchReplyRequests(seededResult).flatMap(_.candidateMove).filter(EvidenceRef.sameMove(_, "a1b1")),
      List("a1b1")
    )
    assert(!seededResult.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces).exists(surface =>
      surface.subject == "played_move" && surface.idea.code == "pawn_break_timing"
    ))

    val closed = withReplyProbeFor(
      seeded,
      "a1b1",
      List(
        VariationLine(List("d6c7", "b2b3", "a7a5", "c1f4", "c7f4", "g3f4", "b8d7", "b1c1"), 61, depth = 16),
        VariationLine(List("b8d7", "b2b4", "b7b6", "a2a4", "c8b7", "f3d2", "e4d2", "c1d2"), 61, depth = 16),
        VariationLine(List("b7b6", "b2b4", "b8d7", "a2a4", "c8b7", "f3d2", "d8e8", "c3e4"), 68, depth = 16)
      )
    )
    val closedResult = MoveReviewJudgmentOrchestrator.build(closed).getOrElse(fail("expected closed result"))
    val closedRecord = closedResult.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(event.rootMove, "a1b1") &&
            event.identity.goalTheme == PlanTheme.PawnBreakPreparation &&
            event.futureMove.exists(EvidenceRef.sameMove(_, "b2b4")) =>
        record -> event
    }.getOrElse(fail("expected tested b-pawn plan"))
    val closedEvent = closedRecord._2

    assert(
      !closedResult.validation.issues.exists(_.subjectId == closedRecord._1.ref.id),
      closedResult.validation.issues
    )
    assert(closedEvent.branchCoverageComplete)
    assertEquals(closedEvent.robustness, PlanCausalRobustness.Conditional)
    assert(closedEvent.publicGoalProofReady)
    assert(closedResult.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces).exists(surface =>
      surface.subject == "played_move" && surface.idea.code == "pawn_break_timing"
    ))

    val rejected = seeded.copy(probeResults = seeded.probeResults.map(_.copy(variationHash = Some("wrong-binding"))))
    val rejectedResult = MoveReviewJudgmentOrchestrator.build(rejected).getOrElse(fail("expected rejected result"))
    assert(!rejectedResult.packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
        EvidenceRef.sameMove(event.rootMove, "a1b1") && event.futureMove.exists(EvidenceRef.sameMove(_, "b2b4"))
      case _ => false
    })
    assert(rejectedResult.packet.probeDiagnostics.exists(diagnostic =>
      diagnostic.status == ProbeAdmissionStatus.Rejected && diagnostic.reasonCodes.contains("VARIATION_HASH_MISMATCH")
    ))

  test("off-main line access prefers occupying the freed square over merely crossing it"):
    val seeded = withOpponentResourceProbes(
      reengineeringRe2Input(),
      List(
        ResourceProbeOutcome(
          rootMove = "e1e2",
          resourceMove = "d5d4",
          whitePovEvalCp = 61,
          moves = List("d5d4", "c3c4", "c6e7", "c4c5", "b6c6", "d3e4", "d6d5", "c2d3", "c6d7", "a1h1")
        ),
        ResourceProbeOutcome(
          rootMove = "e1e2",
          resourceMove = "h4h3",
          whitePovEvalCp = 186,
          moves = List("h4h3", "g2h2", "d6f6", "a1e1", "g6f8", "h2h3")
        )
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(seeded).getOrElse(fail("expected line-access result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(event.rootMove, "e1e2") && event.planId == PlanId.RookActivation =>
        record -> event
    }.getOrElse(fail("expected rook activation event"))
    val event = eventRecord._2
    val access = event.episode.toList.flatMap(_.dependencies).collectFirst {
      case PlanCausalEventDependency(
            _,
            _,
            PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(trajectory),
            _
          ) => trajectory
    }.getOrElse(fail("expected line-access dependency"))

    assert(
      !result.validation.issues.exists(_.subjectId == eventRecord._1.ref.id),
      result.validation.issues
    )
    assertEquals(event.futureMove, Some("a1e1"))
    assertEquals(access.vacatedSquare, EvidenceSquare("e1"))
    assertEquals(access.enabledTo, EvidenceSquare("e1"))
    assert(event.continuationSourceLine.exists(_.role == LineNodeRole.Threat))
    assert(!event.episodePublicProofReady)
    assert(!result.packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, candidate: PlanCausalEventEvidence, _) =>
        EvidenceRef.sameMove(candidate.rootMove, "e1e2") &&
          candidate.planId == PlanId.Prophylaxis &&
          candidate.counterfactualContinuationProven
      case _ => false
    })

  test("line access waits for the last blocker on the enabled path"):
    val startFen = "2r5/1br1kp2/p2pn1p1/1p2p3/4P1P1/P1N1KP2/1PPR4/2NR4 b - - 16 35"
    val moves = List("c7c3", "c1d3", "c3c2", "d2c2", "c8c2", "d1c1", "c2c1", "d3c1")
    val steps = PrincipalVariationEvidence
      .legalReplay(startFen, moves, startPly = 0)
      .getOrElse(fail("expected legal blocker-clearance replay"))
      .map { case (fenBefore, move) =>
        LineReplayStep(move.ply, move.uci, fenBefore, move.fenAfter)
      }
    val root = steps.head
    val lastClearance = steps(2)
    val enabled = steps(4)

    assertEquals(LineAccessTrajectory.find(root, enabled, steps.slice(1, 4)), None)
    val access = LineAccessTrajectory
      .find(lastClearance, enabled, List(steps(3)))
      .getOrElse(fail("expected access after the last path blocker moves"))
    val response = CaptureResponseFollowUpTrajectory
      .find(lastClearance, steps(3), enabled, List(steps(3)))
      .getOrElse(fail("expected induced capture and recapture"))
    assertEquals(access.vacatedSquare, EvidenceSquare("c3"))
    assertEquals(access.enabledFrom, EvidenceSquare("c8"))
    assertEquals(access.enabledTo, EvidenceSquare("c2"))
    assert(CaptureResponseFollowUpTrajectory.proves(response))

    val plan = PlanMatch(
      plan = Plan.Simplification(Color.Black),
      score = 1.0,
      evidence = Nil,
      support = List(
        PlanSupport.Theme(PlanTheme.FavorableExchange),
        PlanSupport.Subplan(PlanKind.SimplificationWindow)
      )
    )
    val rootLine = LineNodeRef("played-c7c3", "c7c3", 1, LineNodeRole.Played)
    val rootNode = PlanCausalEventNode(
      identity = PlanEventIdentityBuilder.from("c7c3", startFen, plan, Nil, Nil),
      step = root,
      perspective = Color.Black,
      structuralConsequences = Nil,
      developmentChoices = Nil
    )
    val episode = PlanCausalEpisodeBuilder.fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = TransitionEdgeRole.Played,
      root = rootNode,
      continuation = steps.tail,
      observedResultMove = Some("c2c1"),
      observedReplyBranch = true
    )
    val resultEvent = episode.continuations
      .find(event => EvidenceRef.sameMove(event.moveUci, "c2c1"))
      .getOrElse(fail("expected exchange result event"))
    val nestedResponse = episode.enablingDependenciesTo(resultEvent).collectFirst {
      case dependency @ PlanCausalEventDependency(
            from,
            to,
            PlanCausalDependencyKind.ResponseContinuationPrecondition,
            PlanCausalDependencyProof.ResponseContinuation(trajectory: CaptureResponseFollowUpTrajectory),
            _
          ) if EvidenceRef.sameMove(from.moveUci, "c3c2") && EvidenceRef.sameMove(to.moveUci, "c8c2") =>
        dependency -> trajectory
    }.getOrElse(fail("expected the induced recapture on the selected causal path"))
    assert(nestedResponse._1.planConnectionProven)
    assertEquals(nestedResponse._2.replyStep.moveUci, "d2c2")
    assert(episode.responses.exists(response =>
      EvidenceRef.sameMove(response.trigger.moveUci, "c3c2") &&
        EvidenceRef.sameMove(response.step.moveUci, "d2c2") &&
        response.proven
    ))
    val exchangeAvailable = resultEvent.structuralConsequences
      .find(_.kind == TransitionConsequenceKind.PieceExchangeAvailable)
      .getOrElse(fail("expected exchange availability after the induced recapture"))
    val rootTransition = StructuralTransitionBinding(
      moveUci = root.moveUci,
      role = TransitionEdgeRole.Played,
      from = PositionNodeRef(root.fenBefore, root.ply, Some(Color.Black)),
      to = PositionNodeRef(root.fenAfter, root.ply + 1, Some(Color.White)),
      line = Some(rootLine),
      perspective = Color.Black
    )
    val event = PlanCausalEventEvidence(
      planId = PlanId.Simplification,
      identity = rootNode.identity,
      rootLine = rootLine,
      rootTransition = rootTransition,
      structuralConsequences = Nil,
      developmentChoices = Nil,
      branchWitnesses = Nil,
      episode = Some(episode)
    )
    assert(event.resultAdvancesGoal(resultEvent, exchangeAvailable))

  test("line access stays closed when an intervening move reblocks the path"):
    val steps = PrincipalVariationEvidence
      .legalReplay(
        "4k3/8/8/1r6/8/8/N6P/R3K3 w - - 0 1",
        List("a2b4", "b5a5", "h2h3", "a5b5", "a1a8"),
        startPly = 0
      )
      .getOrElse(fail("expected legal transient-blocker replay"))
      .map { case (fenBefore, move) =>
        LineReplayStep(move.ply, move.uci, fenBefore, move.fenAfter)
      }

    assertEquals(LineAccessTrajectory.find(steps.head, steps.last, steps.slice(1, 4)), None)

  test("rook file transfer keeps the rook entering the vacated file ahead of later same-rook travel"):
    val seeded = withOpponentResourceProbes(
      structuresRd7Input(),
      List(ResourceProbeOutcome(
        rootMove = "d8d7",
        resourceMove = "d4d5",
        whitePovEvalCp = -76,
        moves = List(
          "d4d5", "c8d8", "g2g3", "f4h6", "b3b4", "c5b4", "a2a3",
          "b4b3", "d3b3", "e6d5", "c4d5", "f7f6", "g1g2", "d7d5"
        )
      ))
    )
    val result = MoveReviewJudgmentOrchestrator.build(seeded).getOrElse(fail("expected rook-transfer result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) &&
            EvidenceRef.sameMove(event.rootMove, "d8d7") &&
            event.planId == PlanId.RookActivation =>
        record -> event
    }.getOrElse(fail("expected played rook-transfer event"))
    val event = eventRecord._2
    val transfer = event.episode.toList.flatMap(_.dependencies).collectFirst {
      case dependency @ PlanCausalEventDependency(
            from,
            to,
            PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(trajectory),
            _
          ) if EvidenceRef.sameMove(from.moveUci, "d8d7") && EvidenceRef.sameMove(to.moveUci, "c8d8") =>
        dependency -> trajectory
    }.getOrElse(fail("expected root-enabled rook transfer"))

    assert(
      !result.validation.issues.exists(_.subjectId == eventRecord._1.ref.id),
      result.validation.issues
    )
    assertEquals(event.identity.goalKind, Some(PlanKind.RookFileTransfer))
    assertEquals(event.futureMove, Some("c8d8"))
    assertEquals(
      event.representativeResult.map(_._2.kind),
      Some(TransitionConsequenceKind.FileOccupationGain)
    )
    assert(transfer._1.enablesContinuation)
    assertEquals(transfer._2.vacatedSquare, EvidenceSquare("d8"))
    assertEquals(transfer._2.enabledTo, EvidenceSquare("d8"))
    assert(!event.episodePublicProofReady)

  test("future realization accepts a different move only when typed function and subject agree"):
    val expected = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      2,
      List("weak-pawn:c4")
    )
    val equivalent = expected.copy(strength = 1)
    val differentTarget = expected.copy(subjects = List("weak-pawn:f4"))
    val taggedRoute = expected.copy(
      kind = TransitionConsequenceKind.MobilityGain,
      subjects = List("knight:c6-e7:vacated-route")
    )
    val canonicalRoute = taggedRoute.copy(subjects = List("knight:c6-e7"))
    val differentRoute = taggedRoute.copy(subjects = List("knight:c6-e8"))
    val unknownCoordinateShape = expected.copy(subjects = List("foo:e4"))
    val expectedProgress = expected.copy(
      kind = TransitionConsequenceKind.PassedPawnProgress,
      subjects = List("passed-pawn-advanced:e6-e7:rank-7")
    )
    val earlierProgress = expectedProgress.copy(subjects = List("passed-pawn-advanced:e5-e6:rank-6"))
    val expectedPromotionPressure = expectedProgress.copy(kind = TransitionConsequenceKind.PromotionPressureGain)
    val earlierPromotionPressure = earlierProgress.copy(kind = TransitionConsequenceKind.PromotionPressureGain)

    assert(PlanCausalFunctionalMatch.functionallyEquivalent(List(expected), List(equivalent)))
    assert(!PlanCausalFunctionalMatch.functionallyEquivalent(List(expected), List(differentTarget)))
    assert(!PlanCausalFunctionalMatch.functionallyEquivalent(List(expected.copy(subjects = Nil)), List(equivalent)))
    assert(PlanCausalFunctionalMatch.functionallyEquivalent(List(taggedRoute), List(canonicalRoute)))
    assert(!PlanCausalFunctionalMatch.functionallyEquivalent(List(taggedRoute), List(differentRoute)))
    assert(!PlanCausalFunctionalMatch.functionallyEquivalent(List(expectedProgress), List(earlierProgress)))
    assert(!PlanCausalFunctionalMatch.functionallyEquivalent(
      List(expectedPromotionPressure),
      List(earlierPromotionPressure)
    ))
    assert(!PlanCausalFunctionalMatch.functionallyEquivalent(
      List(unknownCoordinateShape),
      List(unknownCoordinateShape)
    ))

  test("one realized reply and two full-horizon refutations stay explicitly conditional"):
    val refuted = List("h2h3", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6")
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(syntheticQueensideComparisonInput(), List(
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(refuted, 0, depth = 16),
        VariationLine(refuted.updated(0, "g1h3"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")
    val assessment = event.representativeResultAssessment.getOrElse(fail("expected representative result"))
    val publicResult = ResolvedPlanEvent.from(event).testedContinuation
      .flatMap(_.representativeResult)
      .getOrElse(fail("expected conditional public result"))

    assertEquals(assessment.robustness, PlanCausalRobustness.Conditional)
    assertEquals(assessment.observations.count(_.outcome == PlanCausalBranchOutcome.Realized), 1)
    assertEquals(assessment.observations.count(_.outcome == PlanCausalBranchOutcome.Refuted), 2)
    assert(event.branchCoverageComplete)
    assertEquals(branchReplyRequests(result), Nil)
    assertEquals(publicResult.conditions.size, 1)
    assertEquals(publicResult.refutations.size, 2)

  test("plan event results must support the typed plan goal"):
    val weaknessPlan = PlanMatch(
      plan = Plan.WeakPawnAttack(Color.White),
      score = 0.8,
      evidence = Nil,
      support = List(
        PlanSupport.Theme(PlanTheme.WeaknessFixation),
        PlanSupport.Subplan(PlanKind.StaticWeaknessFixation)
      )
    )
    val mobility = TransitionConsequence(
      TransitionConsequenceKind.MobilityGain,
      StructuralSignalPolarity.Gain,
      2,
      List("bishop:c8-a6:mobility+2")
    )
    assert(!PlanCausalEventProof.developmentSupportsPlan(weaknessPlan))

    val before = PositionNodeRef(
      "r1bq1bk1/p1n3r1/p2p2n1/3Pp1pp/1N2Pp2/2N2P2/1P2BBPP/R2Q1R1K w - - 2 21",
      40,
      Some(Color.White)
    )
    val targetBundle = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      6,
      List("a7", "d8", "e5")
    )
    val transition = StructuralTransitionBinding(
      moveUci = "b4c6",
      role = TransitionEdgeRole.Played,
      from = before,
      to = before.copy(ply = 41, sideToMove = Some(Color.Black)),
      line = Some(LineNodeRef("played-b4c6", "b4c6", 1, LineNodeRole.Played)),
      perspective = Color.White
    )
    val matching = PlanCausalEventProof
      .consequenceForPlan(weaknessPlan, targetBundle, transition)
      .getOrElse(fail("expected matching weak-pawn targets"))

    assertEquals(PlanCausalEventProof.candidateConsequenceForPlan(weaknessPlan, mobility, transition), None)
    assertEquals(PlanCausalEventProof.candidateConsequenceForPlan(weaknessPlan, targetBundle, transition), Some(matching))
    assertEquals(PlanCausalEventProof.goalConsequenceForPlan(weaknessPlan, targetBundle, transition), Some(matching))
    assertEquals(matching.subjects, List("a7", "e5"))
    assertEquals(matching.strength, 2)

  test("wing-plan evidence stays on the named side of the board"):
    def event(
        move: String,
        target: String,
        fen: String = chess.variant.Standard.initialFen.value
    ): PlanCausalEventEvidence =
      val before = PositionNodeRef(fen, 1, Some(Color.White))
      val line = LineNodeRef(s"played-$move", move, 1, LineNodeRole.Played)
      val consequence = TransitionConsequence(
        TransitionConsequenceKind.TargetPressureGain,
        StructuralSignalPolarity.Gain,
        1,
        List(target)
      )
      PlanCausalEventEvidence(
        planId = PlanId.PawnStorm,
        identity = PlanEventIdentity(
          rootMove = move,
          goalTheme = PlanTheme.WingPlay,
          goalKind = Some(PlanKind.WingExpansion),
          actorRole = Some("pawn"),
          actorFrom = Some(move.take(2)),
          actorTo = Some(move.slice(2, 4)),
          targets = List(target),
          results = List("kind:targetpressuregain")
        ),
        rootLine = line,
        rootTransition = StructuralTransitionBinding(
          moveUci = move,
          role = TransitionEdgeRole.Played,
          from = before,
          to = before.copy(ply = 2, sideToMove = Some(Color.Black)),
          line = Some(line),
          perspective = Color.White
        ),
        structuralConsequences = List(consequence),
        developmentChoices = Nil,
        branchWitnesses = Nil
      )

    assert(PlanCausalEventProof.decisiveGoalProof(event("h2h4", "g6")))
    assert(!PlanCausalEventProof.decisiveGoalProof(event("h2h4", "c4")))
    assert(!PlanCausalEventProof.decisiveGoalProof(event("d2d4", "f3")))
    assert(!PlanCausalEventProof.decisiveGoalProof(event("a1d1", "e6")))
    assert(!PlanCausalEventProof.decisiveGoalProof(event(
      "f3h2",
      "f6",
      "r4rk1/pp1qbppp/2n1pn2/2pp4/3P4/2P1PN2/PP1NBPPP/R2Q1RK1 w - - 0 12"
    )))

    def attackPlan(plan: Plan): PlanMatch =
      PlanMatch(
        plan = plan,
        score = 1.0,
        evidence = Nil,
        support = List(
          PlanSupport.Theme(PlanTheme.WingPlay),
          PlanSupport.Subplan(PlanKind.WingExpansion)
        )
      )

    val kingsideResult = event("h2h4", "g6")
    val queensideResult = event("b2b4", "c6")
    val kingsidePlan = attackPlan(Plan.KingsideAttack(Color.White))
    val queensidePlan = attackPlan(Plan.QueensideAttack(Color.White))

    assert(PlanCausalEventProof
      .consequenceForPlan(kingsidePlan, kingsideResult.structuralConsequences.head, kingsideResult.rootTransition)
      .nonEmpty)
    assertEquals(
      PlanCausalEventProof
        .consequenceForPlan(queensidePlan, kingsideResult.structuralConsequences.head, kingsideResult.rootTransition),
      None
    )
    assert(PlanCausalEventProof
      .consequenceForPlan(queensidePlan, queensideResult.structuralConsequences.head, queensideResult.rootTransition)
      .nonEmpty)
    assertEquals(
      PlanCausalEventProof
        .consequenceForPlan(kingsidePlan, queensideResult.structuralConsequences.head, queensideResult.rootTransition),
      None
    )

    val knightMove = event(
      "f3h2",
      "f6",
      "r4rk1/pp1qbppp/2n1pn2/2pp4/3P4/2P1PN2/PP1NBPPP/R2Q1RK1 w - - 0 12"
    ).rootTransition
    val unlockedPawnAdvance = TransitionConsequence(
      TransitionConsequenceKind.LineUnlockGain,
      StructuralSignalPolarity.Gain,
      1,
      List("pawn:f2-f4:line-unlock:by:f3h2:mobility+1")
    )
    assert(PlanCausalGoalProof.proves(
      PlanTheme.PawnBreakPreparation,
      Some(PlanKind.CentralBreakTiming),
      knightMove,
      unlockedPawnAdvance
    ))
    assert(!PlanCausalGoalProof.provesAfterInducedResponse(
      PlanEventIdentity(
        rootMove = "f3h2",
        goalTheme = PlanTheme.WeaknessFixation,
        goalKind = Some(PlanKind.StaticWeaknessFixation),
        actorRole = Some("knight"),
        actorFrom = Some("f3"),
        actorTo = Some("h2"),
        targets = Nil,
        results = Nil
      ),
      knightMove,
      TransitionConsequence(
        TransitionConsequenceKind.WeakSquareTargetCreated,
        StructuralSignalPolarity.Gain,
        1,
        List("weak-square:f6")
      )
    ))

  test("a root structural pawn unlock owns the existing break plan"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "1q3rk1/1prnbpp1/p2p2np/3Qp3/P3P1P1/1NP1BP1P/1PR5/1K1R1B2 w - - 3 20",
          playedMoveUci = "b3c1",
          variations = List(
            VariationLine(
              List(
                "b3c1", "d7c5", "a4a5", "f8d8", "b2b4", "g6f4", "d5a2", "c5e6", "e3b6",
                "e6g5", "b6c7", "b8c7", "a2c4", "c7b8", "c2b2", "d8c8", "c4b3", "c8c7"
              ),
              scoreCp = 67,
              depth = 18
            ),
            VariationLine(List("a4a5", "c7c6", "b3c1", "e7g5", "e3f2", "g5c1", "d1c1"), scoreCp = 37, depth = 18),
            VariationLine(List("c2d2", "f8c8", "a4a5", "g6f4", "e3f4", "e5f4", "d2d4"), scoreCp = 36, depth = 18)
          ),
          currentEvalCp = Some(67),
          ply = Some(38)
        )
      )
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PawnBreakPreparation &&
            EvidenceRef.sameMove(payload.rootMove, "b3c1") =>
        payload
    }.getOrElse(fail("expected root-owned pawn-break preparation"))
    val resolved = ResolvedPlanEvent.from(event)
    val publicSurfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val otherPublicEvents = publicSurfaces
      .flatMap(_.resolvedPlanEvent)
      .filter(publicEvent =>
        EvidenceRef.sameMove(publicEvent.rootMove, "b3c1") &&
          publicEvent.goalTheme != PlanTheme.PawnBreakPreparation.id
      )
    val planClaims = result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims).filter(_.unit == PositionPlanTechniqueUnit.PlanOptionSet)
    val publicBreak = publicSurfaces
      .find(surface =>
        surface.displayPlanId.contains(PlanId.PawnBreakPreparation) &&
          surface.idea.code == "pawn_break_timing"
      )
      .getOrElse(fail(s"expected public b-pawn break preparation; surfaces=${publicSurfaces.map(surface =>
        (surface.idea.code, surface.displayPlanId, surface.resolvedPlanEvent.map(event =>
          (event.goalTheme, event.results.map(result => (result.stage, result.kind, result.subjects)))
        ), surface.evidence.publicSurfaceAdmitted)
      )}; planClaims=${planClaims.map(claim =>
        (claim.role, claim.displayPlanId, claim.resolvedPlanEvent.map(event =>
          (event.goalTheme, event.preparedPawnAdvanceFiles, event.results.map(result => (result.stage, result.kind, result.subjects)))
        ), claim.supportLevel, claim.surfaceLane, claim.publicSurfaceAdmitted, claim.publicProofLevel,
          claim.specificityTier, claim.breakFiles, claim.routeIdentityParts)
      )}"))
    val publicPlan = result.packet.moveJudgmentView
      .flatMap(view =>
        (MoveMeaningSurface.publicPayloadJson(view) \ "explanations")
          .as[List[play.api.libs.json.JsObject]]
          .find(explanation => (explanation \ "move").asOpt[String].contains("b3c1"))
          .flatMap(explanation => (explanation \ "plan").asOpt[play.api.libs.json.JsObject])
      )
      .getOrElse(fail("expected the leading public idea's plan event"))

    assert(event.directGoalConsequences.exists(consequence =>
      consequence.kind == TransitionConsequenceKind.LineUnlockGain &&
        consequence.subjects.exists(_.contains("pawn:b2-b4:line-unlock:by:b3c1"))
    ))
    assertEquals(resolved.preparedPawnAdvanceFiles, List("b"))
    assert(otherPublicEvents.forall(_.preparedPawnAdvanceFiles.isEmpty), otherPublicEvents)
    assertEquals(publicBreak.resolvedPlanEvent.map(_.goalTheme), Some(PlanTheme.PawnBreakPreparation.id))
    assert(publicBreak.representativePlanEvent)
    assert(publicSurfaces.exists(_.displayPlanId.contains(PlanId.PieceActivation)), publicSurfaces)
    assert(publicSurfaces.exists(_.displayPlanId.contains(PlanId.Prophylaxis)), publicSurfaces)
    assertEquals((publicPlan \ "goal" \ "theme").as[String], "pawn break preparation")
    assert(publicBreak.evidence.boardCarriers.exists(carrier =>
      carrier.role == "target" && carrier.kind == "PlanSubject" && carrier.value == "break-file:b"
    ))

  test("reply probe keeps an unfinished branch deferred and requests the same horizon again"):
    val input = syntheticQueensideComparisonInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "a7a6"), 0, depth = 16),
        VariationLine(List("g1h3", "e7e6"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assertEquals(event.branchWitnesses.count(_.outcome == PlanCausalBranchOutcome.Realized), 1)
    assertEquals(event.branchWitnesses.count(_.outcome == PlanCausalBranchOutcome.Deferred), 2)
    assertEquals(event.robustness, PlanCausalRobustness.Deferred)
    assert(!event.branchCoverageComplete)
    assert(!event.episodePublicProofReady)
    assert(ResolvedPlanEvent.from(event).results.forall(_.stage == "direct"))
    assertEquals(
      result.packet.probeRequests.flatMap(_.horizon),
      List(BranchReplyProbeBinding.horizon(event.requiredHorizonPlyOffset))
    )

  test("reply probe refutes an unrealized episode after its representative horizon"):
    val input = syntheticQueensideComparisonInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("f1e2", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6"), 0, depth = 16),
        VariationLine(List("h2h3", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6"), 0, depth = 16),
        VariationLine(List("g1h3", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assertEquals(event.branchWitnesses.map(_.outcome).distinct, List(PlanCausalBranchOutcome.Refuted))
    assert(event.causalResultAssessments.nonEmpty)
    assert(event.allCausalResultsRefuted)
    assertEquals(event.robustness, PlanCausalRobustness.Refuted)
    assert(!event.episodePublicProofReady)
    val publicEvent = ResolvedPlanEvent.from(event)
    assertEquals(event.planSequenceSummary, None)
    assertEquals(publicEvent.transitionType, None)
    assertEquals(publicEvent.moveRole, Some(PlanMoveRole.Preparation))
    val refutedResults = publicEvent.results.filter(_.robustness.contains(PlanCausalRobustness.Refuted))
    assert(refutedResults.nonEmpty)
    assert(refutedResults.forall(_.refutations.size == 3), refutedResults)
    assertEquals(publicEvent.testedContinuation, None)
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val publicClaim = view.moveMeaningClaims.find(claim =>
      claim.resolvedPlanEvent.exists(_.results.exists(_.robustness.contains(PlanCausalRobustness.Refuted)))
    ).getOrElse(fail("expected refuted resolved plan event in the public claim"))
    assertEquals(publicClaim.testedContinuation, None)
    assert(!publicClaim.boardCarriers.exists(carrier =>
      carrier.role == "witness" && EvidenceRef.sameMove(carrier.value, "b5b4")
    ), publicClaim.boardCarriers)
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _) if payload == event => record
    }.getOrElse(fail("expected refuted causal event record"))
    assert(!EvidenceObjectBinding.fromEvidenceRefs(result.packet.evidenceGraph, List(eventRecord.ref)).exists(binding =>
      binding.horizon.nonEmpty &&
        binding.witness.exists(obj => obj.kind == EvidenceObjectKind.Move && EvidenceRef.sameMove(obj.key, "b5b4"))
    ))
    val publicJson = MoveMeaningSurface.publicPayloadJson(view)
    val testedPlanLimits = (publicJson \ "tested_plan_limits")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
    assert(testedPlanLimits.exists(assessment =>
      (assessment \ "move" \ "uci").asOpt[String].exists(EvidenceRef.sameMove(_, "b7b5"))
    ), testedPlanLimits)
    val explanationPlans = (publicJson \ "explanations")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .flatMap(explanation => (explanation \ "plan").asOpt[play.api.libs.json.JsObject])
    assert(!explanationPlans.exists(plan =>
      (plan \ "method" \ "starting_move" \ "uci").asOpt[String].exists(EvidenceRef.sameMove(_, "b7b5")) &&
        (plan \ "results").asOpt[List[play.api.libs.json.JsObject]].exists(_.exists(result =>
          (result \ "tested_reply_status").asOpt[String].contains("does not hold against the tested replies")
        ))
    ), explanationPlans)
    val refutedEventJson = (publicJson \\ "plan")
      .collect { case plan: play.api.libs.json.JsObject => plan }
      .find(plan =>
        (plan \ "method" \ "starting_move" \ "uci").asOpt[String].exists(EvidenceRef.sameMove(_, "b7b5")) &&
          (plan \ "results").asOpt[List[play.api.libs.json.JsObject]].exists(_.exists(result =>
            (result \ "tested_reply_status").asOpt[String].contains("does not hold against the tested replies")
          ))
      )
      .getOrElse(fail("expected serialized refuted plan"))
    val serializedRefuted = (refutedEventJson \ "results")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .filter(result =>
        (result \ "tested_reply_status").asOpt[String].contains("does not hold against the tested replies")
      )
    assert(serializedRefuted.nonEmpty)
    val publicReplies = (refutedEventJson \ "opponent_replies")
      .asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil)
    assertEquals(publicReplies.size, 3)
    assert(publicReplies.forall(reply =>
      (reply \ "effect_on_plan").asOpt[String].contains("the tested line does not confirm the planned result")
    ), publicReplies)
    assertEquals((refutedEventJson \ "continuation").asOpt[play.api.libs.json.JsObject], None)
    val planSignals = planCoherenceSignals(result, "b7b5")
    assertEquals(planSignals.map(_.source.layer).distinct, List(EvidenceLayer.PlanCausalEvent))
    assertEquals(
      planSignals.flatMap(_.axis.map(_.polarity)).toSet,
      Set(StrategicAxisPolarity.Support, StrategicAxisPolarity.Concede)
    )
    assert(planRelativeCauses(result, "b7b5").contains(RelativeCauseKind.PlanContradiction))

  test("a realization is certified only when it falls inside the observed reply horizon"):
    val tail = List("a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6", "f1e2", "b5b4")
    val probed = withReplyProbe(syntheticQueensideComparisonInput(), List(
        VariationLine("f1e2" :: tail.updated(7, "h2h3"), 0, depth = 16),
        VariationLine("h2h3" :: tail, 0, depth = 16),
        VariationLine("g1h3" :: tail, 0, depth = 16)
      ))
    val bindingPacket = MoveReviewJudgmentOrchestrator
      .build(syntheticQueensideComparisonInput())
      .getOrElse(fail("expected binding packet"))
      .packet
    val bindingLine = bindingPacket.candidateLines
      .find(_.ref.rootMove == "b7b5")
      .getOrElse(fail("expected bound b7-b5 line"))
    val idOnlyResult = MoveReviewJudgmentOrchestrator.build(
      probed.copy(probeResults = probed.probeResults.map(probe => probe.copy(id = s"${probe.id}:opaque-ply-10")))
    ).getOrElse(fail("expected opaque-id judgment result"))
    val idOnlyEvent = causalEvent(idOnlyResult, "b7b5", "b5b4")
    assertEquals(idOnlyEvent.branchWitnesses, Nil)
    assert(idOnlyResult.packet.probeDiagnostics.exists(_.reasonCodes.contains("PROBE_REQUEST_UNISSUED")))

    val missingHorizon = MoveReviewJudgmentOrchestrator.build(
      probed.copy(probeResults = probed.probeResults.map(_.copy(horizon = None)))
    ).getOrElse(fail("expected missing-horizon judgment result"))
    assert(missingHorizon.packet.probeDiagnostics.exists(_.reasonCodes.contains("HORIZON_UNVERIFIED")))

    val hashMismatch = MoveReviewJudgmentOrchestrator.build(
      probed.copy(probeResults = probed.probeResults.map(
        _.copy(horizon = Some(BranchReplyProbeBinding.horizon(10)))
      ))
    ).getOrElse(fail("expected hash-mismatch judgment result"))
    assert(hashMismatch.packet.probeDiagnostics.exists(_.reasonCodes.contains("HORIZON_MISMATCH")))

    val shallowResult = MoveReviewJudgmentOrchestrator.build(probed).getOrElse(fail("expected issued-horizon result"))
    val shallowEvent = causalEvent(shallowResult, "b7b5", "b5b4")
    assertEquals(shallowEvent.requiredHorizonPlyOffset, 8)
    assertEquals(shallowEvent.branchWitnesses.map(_.outcome).distinct, List(PlanCausalBranchOutcome.Refuted))
    assertEquals(shallowEvent.branchWitnesses.map(_.observedPlyOffset).distinct, List(8))
    assertEquals(shallowEvent.branchWitnesses.map(_.observedThroughPlyOffset).distinct, List(8))
    assertEquals(shallowEvent.branchWitnesses.map(_.certifiedHorizonPlyOffset).distinct, List(8))
    assert(shallowEvent.branchWitnesses.forall(_.realizationMove.isEmpty))

    val jointlyTampered = MoveReviewJudgmentOrchestrator.build(
      probed.copy(probeResults = probed.probeResults.map(probe => probe.copy(
        horizon = Some(BranchReplyProbeBinding.horizon(10)),
        variationHash = Some(BranchReplyProbeBinding.variationHash(bindingPacket.root, bindingLine, 10))
      )))
    ).getOrElse(fail("expected jointly-tampered judgment result"))
    assertEquals(causalEvent(jointlyTampered, "b7b5", "b5b4").branchWitnesses, Nil)
    assert(jointlyTampered.packet.probeDiagnostics.exists(diagnostic =>
      diagnostic.reasonCodes.contains("ISSUED_REQUEST_MISMATCH") &&
        diagnostic.reasonCodes.contains("HORIZON_MISMATCH") &&
        diagnostic.reasonCodes.contains("VARIATION_HASH_MISMATCH")
    ), jointlyTampered.packet.probeDiagnostics)

    val shortResult = MoveReviewJudgmentOrchestrator.build(withReplyProbe(
      syntheticQueensideComparisonInput(),
      List(
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16),
        VariationLine(List("g1h3", "b5b4"), 0, depth = 16)
      )
    )).getOrElse(fail("expected short issued-horizon result"))
    val shortEvent = causalEvent(shortResult, "b7b5", "b5b4")
    val eventRecordId = shortResult.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: PlanCausalEventEvidence, _) if payload == shortEvent => ref.id
    }.getOrElse(fail("expected causal event record"))
    val validation = JudgmentPacketValidator.validate(shortResult.packet)
    assert(!validation.issues.exists(issue =>
      issue.subjectId == eventRecordId &&
        issue.kind == JudgmentPacketValidationIssueKind.InvalidPlanCausalBranchProof
    ), validation.issues)
    val tamperedPacket = shortResult.packet.copy(
      evidenceGraph = shortResult.packet.evidenceGraph.copy(
        records = shortResult.packet.evidenceGraph.records.map {
          case record @ EvidenceRecord(ref, payload: PlanCausalEventEvidence, _) if ref.id == eventRecordId =>
            record.copy(payload = payload.copy(
              branchWitnesses = payload.branchWitnesses.map(_.copy(observedPlyOffset = 1))
            ))
          case record => record
        }
      )
    )
    val tamperedValidation = JudgmentPacketValidator.validate(tamperedPacket)
    assert(tamperedValidation.issues.exists(issue =>
      issue.subjectId == eventRecordId &&
        issue.kind == JudgmentPacketValidationIssueKind.InvalidPlanCausalBranchProof
    ), tamperedValidation.issues)
    val horizonTamperedPacket = shortResult.packet.copy(
      evidenceGraph = shortResult.packet.evidenceGraph.copy(
        records = shortResult.packet.evidenceGraph.records.map {
          case record @ EvidenceRecord(ref, payload: PlanCausalEventEvidence, _) if ref.id == eventRecordId =>
            record.copy(payload = payload.copy(
              branchWitnesses = payload.branchWitnesses.map(_.copy(certifiedHorizonPlyOffset = 9))
            ))
          case record => record
        }
      )
    )
    val horizonTamperedValidation = JudgmentPacketValidator.validate(horizonTamperedPacket)
    assert(horizonTamperedValidation.issues.exists(issue =>
      issue.subjectId == eventRecordId &&
        issue.kind == JudgmentPacketValidationIssueKind.InvalidPlanCausalBranchProof
    ), horizonTamperedValidation.issues)

  test("terminal horizon distinguishes victory defeat and draw"):
    val mate = "7k/6Q1/6K1/8/8/8/8/8 b - - 0 1"
    val stalemate = "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"

    assertEquals(PlanCausalEventProof.terminalOutcomeAt(mate, Color.White), Some(PlanCausalTerminalOutcome.Victory))
    assertEquals(PlanCausalEventProof.terminalOutcomeAt(mate, Color.Black), Some(PlanCausalTerminalOutcome.Defeat))
    assertEquals(PlanCausalEventProof.terminalOutcomeAt(stalemate, Color.White), Some(PlanCausalTerminalOutcome.Draw))

  test("reply admission does not backfill a shallow top reply from lower ranks"):
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(syntheticQueensideComparisonInput(), List(
        VariationLine(List("f1e2", "b5b4"), 0, depth = 11),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16),
        VariationLine(List("g1h3", "b5b4"), 0, depth = 16),
        VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assertEquals(event.branchWitnesses, Nil)
    assert(result.packet.probeDiagnostics.exists(_.reasonCodes.contains("REPLY_LINE_DEPTH_FLOOR_UNMET")))
    assertEquals(branchReplyRequests(result).flatMap(_.candidateMove), List("b7b5"))

  test("reply admission does not backfill a terminal-score-inconsistent top reply"):
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(syntheticQueensideComparisonInput(), List(
        VariationLine(List("f4d2", "e7e6", "g1e2", "c5d3"), 0, mate = Some(1), depth = 16),
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16),
        VariationLine(List("g1h3", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assertEquals(event.branchWitnesses, Nil)
    assert(result.packet.probeDiagnostics.exists(
      _.reasonCodes.contains("REPLY_LINE_TERMINAL_SCORE_INCONSISTENT")
    ))
    assertEquals(branchReplyRequests(result).flatMap(_.candidateMove), List("b7b5"))

  test("resolved mobility event compresses its route without turning the destination into a target"):
    val result = MoveReviewJudgmentOrchestrator
      .build(marDelPlataKnightRouteInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation && EvidenceRef.sameMove(payload.rootMove, "d3b4") =>
        payload
    }.getOrElse(fail("expected piece-activation event"))
    val proof = ResolvedPlanEvent.from(event)
    val surfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val route = surfaces.filter(surface => surface.subject == "played_move" && surface.ideaType == "piece_route") match
      case value :: Nil => value
      case values       => fail(s"expected one compressed route, got $values")

    assertEquals(proof.actorFrom -> proof.actorTo, Some("d3") -> Some("b4"))
    assertEquals(proof.targets, Nil)
    assert(proof.results.exists(_.kind == TransitionConsequenceKind.MobilityGain), proof.results)
    assert(!proof.results.exists(_.kind == TransitionConsequenceKind.LineUnlockGain), proof.results)
    assertEquals(route.displayPlanId, Some(PlanId.PieceActivation))
    assertEquals(route.target.squares, Nil)
    assert(!surfaces.exists(surface => surface.displayPlanId.contains(PlanId.PieceActivation) && surface.ideaType == "target_pressure"))
    assert(surfaces.exists(surface => surface.displayPlanId.contains(PlanId.WeakPawnAttack) && surface.target.squares == List("a6")))

  test("a goal-owned route keeps the moved actor without promoting an incidental line beneficiary"):
    val result = MoveReviewJudgmentOrchestrator
      .build(carlsbadKnightRouteInput())
      .getOrElse(fail("expected judgment result"))
    val events = result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "c3e2") =>
        payload
    }
    val event = events.find(_.planId == PlanId.PieceActivation)
      .getOrElse(fail(s"expected knight route; events=$events"))
    val surfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val route = surfaces.find(surface =>
      surface.subject == "played_move" &&
        surface.ideaType == "piece_route" &&
        surface.displayPlanId.contains(PlanId.PieceActivation)
    ).getOrElse(fail(s"expected public knight route; events=$events surfaces=$surfaces"))

    assertEquals(event.identity.actorRole, Some("knight"))
    assertEquals(event.structuralConsequences.map(_.kind).distinct, List(TransitionConsequenceKind.MobilityGain))
    assert(!event.episode.exists(_.historySequenceProven), event.episode)
    assertEquals(event.planSequenceSummary, None)
    assert(route.evidence.boardCarriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Piece" && carrier.value == "knight"))
    assert(!route.evidence.boardCarriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Piece" && carrier.value == "queen"))
    assert(!surfaces.exists(surface => surface.subject == "played_move" && surface.ideaType == "piece_activity"))

  test("a dominant line unlock can prove piece improvement without inheriting every vacated route"):
    val result = MoveReviewJudgmentOrchestrator
      .build(doknjasBishopUnlockInput())
      .getOrElse(fail("expected judgment result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(event.rootMove, "f6h5") &&
            event.identity.goalKind.contains(PlanKind.WorstPieceImprovement) =>
        record -> event
    }.getOrElse(fail("expected proven piece-improvement event"))
    val publicEvents = result.packet.moveJudgmentView.toList
      .flatMap(_.moveMeaningClaims)
      .filter(_.publicSurfaceAdmitted)
      .flatMap(_.resolvedPlanEvent)
      .filter(event => EvidenceRef.sameMove(event.rootMove, "f6h5"))
    val publicSurfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(_.subject == "played_move")
    val secondaryExchanges = publicSurfaces.filter(surface =>
      surface.idea.code == "piece_exchange" && !surface.representativePlanEvent
    )

    assertEquals(eventRecord._2.structuralConsequences.map(_.kind), List(TransitionConsequenceKind.LineUnlockGain))
    assert(eventRecord._2.structuralConsequences.flatMap(_.subjects).exists(_.startsWith("bishop:g7:")))
    assert(publicEvents.exists(event =>
      event.goalKind.contains(PlanKind.WorstPieceImprovement.id) &&
        event.results.flatMap(_.subjects).exists(_.contains("bishop:g7"))
    ), publicEvents)
    assert(!publicEvents.flatMap(_.results).flatMap(_.subjects).exists(subject =>
      subject.contains("pawn:f7") || subject.contains("queen:b2")
    ), publicEvents)
    assert(!publicSurfaces.filterNot(_.idea.code == "target_pressure").flatMap(_.evidence.boardCarriers).exists(
        _.semanticRole.contains(MoveMeaningSurfaceBoardCarrier.AttackedPieceRole)
    ), publicSurfaces)
    assert(publicSurfaces.exists(_.idea.label == "clears bishop diagonal from g7"), publicSurfaces)
    assert(!publicSurfaces.exists(_.idea.label.contains("knight line")), publicSurfaces)
    assert(secondaryExchanges.forall(_.priority == "supporting"), secondaryExchanges)

  test("vacated lines stay heuristic when they do not prove the proposed outpost goal"):
    List(gamechangerKnightRetreatInput(), bosboomKnightSacrificeInput()).foreach { input =>
      val result = MoveReviewJudgmentOrchestrator
        .build(input)
        .getOrElse(fail(s"expected judgment result for ${input.playedMoveUci}"))
      val candidates = result.packet.evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
            if EvidenceRef.sameMove(event.rootMove, input.playedMoveUci) &&
              event.identity.goalKind.contains(PlanKind.OutpostEntrenchment) =>
          record -> event
      }
      val publicOutpostEvents = result.packet.moveJudgmentView.toList
        .flatMap(_.moveMeaningClaims)
        .filter(_.publicSurfaceAdmitted)
        .flatMap(_.resolvedPlanEvent)
        .filter(event =>
          EvidenceRef.sameMove(event.rootMove, input.playedMoveUci) &&
            event.goalKind.contains(PlanKind.OutpostEntrenchment.id)
        )

      assert(candidates.nonEmpty, s"expected internal outpost hypothesis for ${input.playedMoveUci}")
      assert(candidates.flatMap(_._2.structuralConsequences).forall(
        _.kind == TransitionConsequenceKind.LineUnlockGain
      ), candidates)
      assertEquals(publicOutpostEvents, Nil)
    }

  test("castling keeps its own route after unrelated battery evidence is removed"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnb1k2r/pp2ppbp/5np1/q1Pp4/2P2B2/2N1P3/PP3PPP/2RQKBNR b Kkq - 2 7",
          playedMoveUci = "e8h8",
          variations = List(
            VariationLine(
              List("e8g8", "c4d5", "b8d7", "f1c4", "d7c5", "a2a3", "b7b5", "c4a2", "b5b4", "a3b4", "a5b4", "g1e2"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(13)
        )
      )
      .getOrElse(fail("expected castling judgment"))
    val publicRoutes = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(surface => surface.subject == "played_move" && surface.idea.code == "piece_route")

    assertEquals(publicRoutes.map(_.moveUci), List("e8g8"))

  test("a king move that opens a tested rook route keeps one typed public plan event"):
    val input = withReplyProbeFor(
      RawMoveReviewInput(
        fen = "r2qk2r/ppp2ppp/2pb4/4p3/4P1b1/3PBN2/PPP2P2/RN1Q1RK1 w kq - 2 11",
        playedMoveUci = "g1h1",
        variations = List(
          VariationLine(List("g1h1", "d8d7", "f1g1", "f7f6", "b1d2", "h7h5"), 0, depth = 16)
        ),
        currentEvalCp = Some(0),
        ply = Some(20)
      ),
      "g1h1",
      List(
        VariationLine(List("d8d7", "f1g1"), 0, depth = 16),
        VariationLine(List("f7f6", "f1g1"), 0, depth = 16),
        VariationLine(List("h7h5", "f1g1"), 0, depth = 16)
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation &&
            payload.identity.goalKind.contains(PlanKind.RookFileTransfer) &&
            EvidenceRef.sameMove(payload.rootMove, "g1h1") =>
        payload
    }.getOrElse(fail("expected tested rook-transfer event"))
    val episode = event.episode.getOrElse(fail("expected typed plan episode"))
    assert(
      event.episodePublicProofReady,
      s"branchComplete=${event.branchCoverageComplete} assessments=${event.causalResultAssessments} episode=$episode"
    )
    val publicEvent = result.packet.moveJudgmentView.toList
      .flatMap(_.moveMeaningClaims)
      .filter(_.publicSurfaceAdmitted)
      .flatMap(_.resolvedPlanEvent)
      .find(resolved => EvidenceRef.sameMove(resolved.rootMove, "g1h1"))
      .getOrElse(fail("expected public rook-transfer event"))

    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition &&
        EvidenceRef.sameMove(dependency.from.moveUci, "g1h1") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "f1g1") &&
        dependency.proofPieceRoles.exists(_.name.equalsIgnoreCase("rook"))
    ), episode.dependencies)
    assertEquals(publicEvent.goalKind, Some(PlanKind.RookFileTransfer.id))
    assertEquals(publicEvent.testedContinuation.map(_.futureMove), Some("f1g1"))
    assert(publicEvent.results.exists(result =>
      result.subjects.exists(subject => subject.contains("g4") || subject.startsWith("rook:f1-g1"))
    ), publicEvent.results)
    assert(!result.packet.moveJudgmentView.toList
      .flatMap(_.moveMeaningClaims)
      .filter(_.publicSurfaceAdmitted)
      .flatMap(_.resolvedPlanEvent)
      .exists(resolved =>
        EvidenceRef.sameMove(resolved.rootMove, "g1h1") &&
          resolved.goalTheme == PlanTheme.AdvantageTransformation.id
      ))

  test("an induced recapture carries only the newly doubled pawn into the public plan"):
    val result = MoveReviewJudgmentOrchestrator
      .build(nimzoLeningradExchangeInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.WeakPawnAttack &&
            EvidenceRef.sameMove(payload.rootMove, "b4c3") => payload
    }.getOrElse(fail("expected response-created weakness event"))
    val responseResult = event.responseGoalResults.find { case (response, consequence) =>
      EvidenceRef.sameMove(response.step.moveUci, "b2c3") &&
      consequence.kind == TransitionConsequenceKind.WeakPawnTargetCreated
    }.getOrElse(fail("expected c3 weakness after the recapture"))
    val publicSurface = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .find(_.resolvedPlanEvent.exists(resolved => EvidenceRef.sameMove(resolved.rootMove, "b4c3")))
      .getOrElse(fail("expected public weakness plan"))
    val resolved = publicSurface.resolvedPlanEvent.getOrElse(fail("expected resolved plan event"))
    val weakPawn = resolved.results.find(_.kind == TransitionConsequenceKind.WeakPawnTargetCreated)
      .getOrElse(fail("expected public weak-pawn result"))
    val publicMoves = publicSurface.evidence.boardCarriers.filter(_.kind == "Move").map(_.value).toSet

    assertEquals(responseResult._2.subjects, List("weak-pawn:c3"))
    assertEquals(resolved.rootMoveReference.map(_.notation), Some("6...Bxc3+"))
    assertEquals(weakPawn.stage, "response")
    assertEquals(weakPawn.source.map(_.notation), Some("7.bxc3"))
    assertEquals(weakPawn.subjects, List("weak-pawn:c3"))
    assertEquals(publicSurface.idea.code, "target_pressure")
    assert(publicSurface.target.squares.contains("c3"), publicSurface.target)
    assert(Set("b4c3", "b2c3").subsetOf(publicMoves), publicSurface.evidence.boardCarriers)
    assert(!weakPawn.subjects.exists(_.contains("a2")), weakPawn)

  test("a tested future exchange carries its recapture-created weakness to the public plan"):
    val result = MoveReviewJudgmentOrchestrator
      .build(carlsbadBishopExchangeInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: PlanCausalEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) &&
            payload.planId == PlanId.WeakPawnAttack &&
            EvidenceRef.sameMove(payload.rootMove, "d3b5") =>
        payload
    }.getOrElse(fail("expected tested weakness event"))
    val (response, consequence) = event.responseGoalResults.find { case (response, consequence) =>
      EvidenceRef.sameMove(response.trigger.moveUci, "b5c6") &&
      EvidenceRef.sameMove(response.step.moveUci, "b7c6") &&
      consequence.kind == TransitionConsequenceKind.WeakPawnTargetCreated
    }.getOrElse(fail("expected c6 weakness after the future exchange"))
    val weakPawnOwners = result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: PlanCausalEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) &&
            payload.responseGoalResults.exists(_._2.kind == TransitionConsequenceKind.WeakPawnTargetCreated) =>
        payload.identity.goalTheme
    }.toSet
    val resolved = ResolvedPlanEvent.from(event)
    val weakPawn = resolved.results
      .find(result =>
        result.kind == TransitionConsequenceKind.WeakPawnTargetCreated &&
          result.source.exists(source => EvidenceRef.sameMove(source.uci, "b7c6"))
      )
      .getOrElse(fail("expected public c6 weakness"))
    val publicJson = MoveMeaningSurface.publicPayloadJson(
      result.packet.moveJudgmentView.getOrElse(fail("expected public move view"))
    )
    val publicText = play.api.libs.json.Json.stringify(publicJson)
    assertEquals(consequence.subjects, List("weak-pawn:c6"))
    assertEquals(weakPawnOwners, Set(PlanTheme.WeaknessFixation))
    assertEquals(event.responseStepDistanceFromPlanStart(response), 3)
    assertEquals(weakPawn.stage, "response")
    assertEquals(weakPawn.source.map(_.notation), Some("23...bxc6"))
    assert(weakPawn.conditions.exists(_.moveReference.exists(_.notation == "23...bxc6")), weakPawn)
    assert(resolved.targets.contains("weak-pawn:c6"), resolved.targets)
    assert(publicText.contains("23...bxc6"), publicText)
    assert(!publicText.contains("22...bxc6"), publicText)

  test("opening development does not probe a later capture as the same plan episode"):
    val result = MoveReviewJudgmentOrchestrator
      .build(morphyBishopDevelopmentInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.OpeningDevelopment && EvidenceRef.sameMove(payload.rootMove, "c8g4") =>
        payload
    }.getOrElse(fail("expected opening-development event"))
    val proof = ResolvedPlanEvent.from(event)
    val surfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val route = surfaces.filter(surface => surface.subject == "played_move" && surface.ideaType == "piece_route") match
      case value :: Nil => value
      case values       => fail(s"expected one ordinary development route, got $values")

    assertEquals(proof.targets, Nil)
    assertEquals(
      proof.results.map(_.kind).toSet,
      Set(
        TransitionConsequenceKind.DevelopmentMobilityGain,
        TransitionConsequenceKind.DevelopmentLagReduced,
        TransitionConsequenceKind.DevelopmentPieceActivated
      )
    )
    assertEquals(proof.testedContinuation, None)
    assert(!result.packet.probeRequests.exists(_.candidateMove.exists(EvidenceRef.sameMove(_, "c8g4"))))
    assert(!route.displayPlanId.contains(PlanId.OpeningDevelopment))
    assertEquals(route.target.squares, Nil)

  test("owned weak-pawn event absorbs the same-target surface fallback"):
    val result = MoveReviewJudgmentOrchestrator
      .build(benoniC4FixationInput())
      .getOrElse(fail("expected judgment result"))
    val surfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(_.subject == "played_move")
    val weakB2 = surfaces.filter(surface => surface.ideaType == "target_pressure" && surface.target.squares == List("b2"))

    assertEquals(weakB2.size, 1)
    assertEquals(weakB2.head.displayPlanId, Some(PlanId.WeakPawnAttack))
    assert(!weakB2.head.target.squares.contains("c4"))

  private def normalizedInput(
      root: PositionNodeRef,
      afterPlayed: PositionNodeRef
  ): NormalizedMoveReviewInput =
    NormalizedMoveReviewInput(
      beforeFen = root.fen,
      playedMoveUci = "d2d4",
      beforePly = root.ply,
      sideToMove = Some(Color.White),
      afterPlayedFen = afterPlayed.fen,
      afterReferenceFen = None,
      lines = Nil,
      currentWhitePovEvalCp = 0,
      opening = None
    )

  private def supportedPawnAdvanceInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r4rk1/pbqn1pbp/2pp1np1/1p2p3/3PP1P1/P1N1B2P/1PP1NPB1/R2Q1RK1 w - - 3 12",
      playedMoveUci = "e2g3",
      variations = List(
        VariationLine(
          List("e2g3", "d7b6", "g4g5", "f6e8", "b2b3", "e5d4", "e3d4", "a8d8", "f2f4", "f7f6"),
          scoreCp = 0,
          depth = 16
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(22),
      openingContext = Some(RawOpeningContext(name = Some("piece support for a kingside pawn advance")))
    )

  private def preparedKingsideAdvanceInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r4rk1/pbq2pbp/1npp1np1/1p2p3/3PP1P1/P1N1B1NP/1PP2PB1/R2Q1RK1 w - - 5 13",
      playedMoveUci = "g4g5",
      variations = List(
        VariationLine(
          List("g4g5", "f6e8", "b2b3", "a7a5", "h3h4", "e5d4", "e3d4", "f7f6"),
          scoreCp = 30,
          depth = 18
        )
      ),
      currentEvalCp = Some(30),
      ply = Some(24),
      openingContext = Some(RawOpeningContext(name = Some("prepared kingside space gain"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "g2g3", "d7d6", "f1g2", "g7g6", "e2e4", "f8g7", "b1c3", "e8g8",
        "g1e2", "e7e5", "h2h3", "c7c6", "e1g1", "b7b5", "a2a3", "d8c7", "g3g4", "b8d7",
        "c1e3", "c8b7", "e2g3", "d7b6"
      )
    )

  private def carlsbadKnightRouteInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "3rrnk1/pp3ppp/1npqb3/3p4/1P1P4/2NBP2P/P1QN1PP1/1R3RK1 w - - 3 17",
      playedMoveUci = "c3e2",
      variations = List(
        VariationLine(List("f1c1", "a7a6", "a2a4", "d6e7", "c3e2", "d8c8", "e2f4", "e6d7", "b1a1", "f8g6", "f4e2", "c8b8"), scoreCp = 76, depth = 18),
        VariationLine(List("c3e2", "d8c8", "a2a4", "b6d7", "a4a5", "d7f6", "d2f3", "f8d7", "a5a6", "b7b6", "f1c1", "g7g6"), scoreCp = 76, depth = 18),
        VariationLine(List("a2a4", "b6c4", "d2c4", "d5c4", "d3f5", "f8g6", "f1d1", "e6d5", "e3e4", "d5e6", "f5g6", "h7g6"), scoreCp = 69, depth = 18)
      ),
      currentEvalCp = Some(76),
      ply = Some(32),
      openingContext = Some(RawOpeningContext(name = Some("Milos-Narciso, Mexico City 2010"))),
      movePrefixUci = List(
        "d2d4", "d7d5", "c2c4", "e7e6", "b1c3", "g8f6", "c4d5", "e6d5", "c1g5", "c7c6",
        "e2e3", "f8e7", "f1d3", "b8d7", "h2h3", "e8g8", "d1c2", "f8e8", "g1f3", "d7f8",
        "g5f4", "e7d6", "f4d6", "d8d6", "e1g1", "c8e6", "a1b1", "f6d7", "b2b4", "d7b6",
        "f3d2", "a8d8"
      )
    )

  private def stonewallRb1Input(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "rnbq1rk1/pp4pp/2pbp3/3p1p2/2PPn3/2N2NP1/PPQ1PPBP/R1B2RK1 w - - 6 9",
      playedMoveUci = "a1b1",
      variations = List(
        VariationLine(List("a1b1", "a7a5", "c4c5", "d6c7", "c3a4", "b8d7", "c1f4", "c7f4", "g3f4", "f8f6", "e2e3", "f6h6"), 63, depth = 18),
        VariationLine(List("f3e5", "b8d7", "e5d3", "d8e7", "f2f3", "e4c3", "c2c3", "e6e5", "c4d5", "c6d5", "d4e5", "d7e5"), 43, depth = 18),
        VariationLine(List("a2a3", "b8d7", "b2b4", "d6e7", "c3a4", "b7b5", "c4b5", "c6b5", "a4c5", "d7b6", "c1f4", "g7g5"), 43, depth = 18)
      ),
      currentEvalCp = Some(63),
      ply = Some(16),
      movePrefixUci = List(
        "d2d4", "e7e6", "c2c4", "f7f5", "g2g3", "g8f6", "f1g2", "c7c6",
        "g1f3", "d7d5", "e1g1", "f8d6", "d1c2", "e8g8", "b1c3", "f6e4"
      )
    )

  private def reengineeringRe2Input(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "3r2k1/pp3pp1/1qnr2n1/3p4/5P1p/PNPB2P1/1PQ2PK1/R3R3 w - - 0 22",
      playedMoveUci = "e1e2",
      variations = List(
        VariationLine(List("e1e2", "h4g3", "f2g3", "g6f8", "b3d2", "d6e6", "d2f3", "e6e2", "c2e2", "d8d7", "b2b4", "d7c7", "a1d1"), 44, depth = 18),
        VariationLine(List("b3d2", "c6a5", "e1e2", "d5d4", "d2e4", "d6c6", "a1d1", "h4g3", "f2g3", "d4c3"), 46, depth = 18),
        VariationLine(List("a1d1", "g6f8", "e1h1", "d6h6", "a3a4", "d8d6", "b3d2", "h4g3"), 32, depth = 18),
        VariationLine(List("e1h1", "h4g3", "f2g3", "d8e8", "a1d1", "d6f6", "d1e1", "e8e1"), 29, depth = 18),
        VariationLine(List("a1c1", "d5d4", "c3c4", "d6f6", "c1d1", "b6c7", "c2e2", "h4g3"), 27, depth = 18)
      ),
      currentEvalCp = Some(44),
      ply = Some(42),
      movePrefixUci = List(
        "e2e4", "e7e6", "d2d4", "d7d5", "b1d2", "c7c5", "e4d5", "e6d5", "g1f3", "b8c6",
        "f1b5", "f8d6", "d4c5", "d6c5", "e1g1", "g8e7", "d2b3", "c5d6", "c1g5", "e8g8",
        "g5h4", "d8c7", "h4g3", "d6g3", "h2g3", "c8g4", "f1e1", "a8d8", "c2c3", "c7b6",
        "b5d3", "e7g6", "d1c2", "g4f3", "g2f3", "d8d6", "f3f4", "f8d8", "a2a3", "h7h5",
        "g1g2", "h5h4"
      )
    )

  private def structuresRd7Input(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "2rr2k1/p4ppp/1p2p3/2p5/2PP1b2/1P1R4/PBR2PPP/6K1 b - - 0 22",
      playedMoveUci = "d8d7",
      variations = List(
        VariationLine(List("d8d7", "c2e2", "f4g5", "d4d5", "e6d5", "c4d5", "g5e7", "g2g3", "f7f6", "e2c2"), -21, depth = 18),
        VariationLine(List("c8c7", "c2e2", "f4g5", "d4d5", "c7d7", "g2g3", "e6d5", "c4d5", "g8f8"), -21, depth = 18),
        VariationLine(List("f4h6", "c2e2", "g7g6", "d4d5", "e6d5", "d3d5", "d8d5", "c4d5", "h6g7"), -16, depth = 18),
        VariationLine(List("c5d4", "c2e2", "f4g5", "d3d4", "d8d4", "b2d4", "c8d8", "d4c3"), -15, depth = 18),
        VariationLine(List("f4g5", "d4d5", "e6d5", "c4d5", "d8d7", "g2g3", "c8d8", "f2f4"), -12, depth = 18)
      ),
      currentEvalCp = Some(-21),
      ply = Some(43),
      movePrefixUci = List(
        "c2c4", "c7c6", "g1f3", "d7d5", "e2e3", "g8f6", "b1c3", "e7e6", "b2b3", "b8d7",
        "d1c2", "f8d6", "c1b2", "e8g8", "f1e2", "b7b6", "e1g1", "c8b7", "d2d4", "a8c8",
        "e3e4", "f6e4", "c3e4", "d5e4", "c2e4", "d7f6", "e4h4", "c6c5", "a1d1", "f6e4",
        "h4d8", "f8d8", "f3d2", "e4d2", "d1d2", "d6e5", "f1d1", "e5f4", "d2c2", "b7e4",
        "e2d3", "e4d3", "d1d3"
      )
    )

  private def syntheticQueensideEpisodeInput(probeResults: List[ProbeResult] = Nil): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1b2rk1/pp2ppbp/5np1/q1nP4/4PB2/2N2P2/PP4PP/2RQKBNR b K - 0 10",
      playedMoveUci = "b7b5",
      variations = List(
        VariationLine(
          List("b7b5", "b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4", "c3b5", "a3a5", "c2c5"),
          scoreCp = 0,
          depth = 16
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(19),
      openingContext = Some(RawOpeningContext(name = Some("Queenside b5-b4 expansion"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5", "c1f4", "f8g7", "e2e3", "c7c5",
        "d4c5", "d8a5", "a1c1", "e8h8", "c4d5", "b8d7", "f2f3", "d7c5", "e3e4"
      ),
      probeResults = probeResults
    )

  private def syntheticQueensideComparisonInput(): RawMoveReviewInput =
    val base = syntheticQueensideEpisodeInput()
    base.copy(variations =
      VariationLine(List("e7e6", "h2h3"), scoreCp = 0, depth = 16) :: base.variations
    )

  private def weakPawnInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1bqk2r/p4ppp/1pnppn2/2p5/2PPP3/P1PBBP2/6PP/R2QK1NR b KQkq - 1 9",
      playedMoveUci = "c6a5",
      variations = List(
        VariationLine(
          List(
            "c6a5",
            "g1h3",
            "c8a6",
            "d1e2",
            "d8d7",
            "e4e5",
            "d6e5",
            "d4e5",
            "f6g8",
            "e1g1",
            "g8e7",
            "a1d1"
          ),
          scoreCp = 0,
          depth = 12
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(17),
      openingContext = Some(RawOpeningContext(name = Some("Nimzo-Indian Saemisch / c4-pawn pressure"))),
      movePrefixUci = List(
        "d2d4",
        "g8f6",
        "c2c4",
        "e7e6",
        "b1c3",
        "f8b4",
        "a2a3",
        "b4c3",
        "b2c3",
        "c7c5",
        "f2f3",
        "b8c6",
        "e2e4",
        "d7d6",
        "c1e3",
        "b7b6",
        "f1d3"
      )
    )

  private def syntheticFlankEpisodeInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r2q1rk1/p3bppp/bpn1p3/3pP3/3P1B2/2P2NP1/P4PBP/R2QR1K1 w - - 3 15",
      playedMoveUci = "h2h4",
      variations = List(
        VariationLine(
          List("h2h4", "a8c8", "e1e3", "c8c7", "f3g5", "h7h6", "g5h3"),
          scoreCp = 0,
          depth = 12
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(28),
      openingContext = Some(RawOpeningContext(name = Some("Kingside h4 expansion and induced h6 concession"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "g1f3", "e7e6", "c2c4", "b7b6", "g2g3", "c8b7", "f1g2", "f8b4",
        "c1d2", "b4e7", "b1c3", "c7c6", "d2f4", "e8h8", "e2e4", "d7d5", "e4e5", "f6e4",
        "c4d5", "c6d5", "e1h1", "e4c3", "b2c3", "b7a6", "f1e1", "b8c6"
      )
    )

  private def marDelPlataKnightRouteInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1bqnbk1/p5r1/p2p2n1/3Pp1pp/4Pp2/2NN1P2/1P2BBPP/R2Q1R1K w - - 0 20",
      playedMoveUci = "d3b4",
      variations = List(VariationLine(List("d3b4", "e8c7", "b4c6", "d8f6"), scoreCp = 0, depth = 12)),
      currentEvalCp = Some(0),
      ply = Some(38),
      openingContext = Some(RawOpeningContext(name = Some("Kings Indian Mar del Plata / opposite-wing breakthrough race"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "f8g7", "e2e4", "d7d6", "g1f3", "e8h8",
        "f1e2", "e7e5", "e1h1", "b8c6", "d4d5", "c6e7", "f3e1", "f6e8", "c1e3", "f7f5",
        "f2f3", "f5f4", "e3f2", "g6g5", "c4c5", "e7g6", "a2a4", "f8f7", "e1d3", "g7f8",
        "a4a5", "f7g7", "a5a6", "b7a6", "c5d6", "c7d6", "g1h1", "h7h5"
      )
    )

  private def morphyBishopDevelopmentInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "rnbqr1k1/ppp2ppp/5n2/3P4/5P2/2PP4/P1PBB1PP/R2QK1NR b KQ - 2 10",
      playedMoveUci = "c8g4",
      variations = List(VariationLine(
        List("c8g4", "c3c4", "c7c6", "d5c6", "b8c6", "e1f1", "e8e2", "g1e2", "c6d4", "d1b1", "g4e2", "f1f2"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(19),
      openingContext = Some(RawOpeningContext(name = Some("Kings Gambit / positional pawn sacrifice to open files"))),
      movePrefixUci = List(
        "e2e4", "e7e5", "f2f4", "d7d5", "e4d5", "e5e4", "b1c3", "g8f6", "d2d3", "f8b4",
        "c1d2", "e4e3", "d2e3", "e8h8", "e3d2", "b4c3", "b2c3", "f8e8", "f1e2"
      )
    )

  private def benoniC4FixationInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r2qr1k1/1p1n1pbp/p2p1np1/2pP4/P3PP2/2N2Q2/1P1N2PP/R1B2RK1 b - - 2 14",
      playedMoveUci = "c5c4",
      variations = List(VariationLine(
        List("c5c4", "d2c4", "a8c8", "c4d6", "d8b6", "g1h1", "b6d6", "e4e5", "d7e5", "f4e5", "d6e5", "c1f4"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(27),
      openingContext = Some(RawOpeningContext(name = Some("Modern Benoni / ...c4 pawn sacrifice"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "c7c5", "d4d5", "e7e6", "b1c3", "e6d5", "c4d5", "d7d6",
        "e2e4", "g7g6", "g1f3", "f8g7", "f1e2", "e8h8", "e1h1", "a7a6", "a2a4", "c8g4",
        "f3d2", "g4e2", "d1e2", "b8d7", "f2f4", "f8e8", "e2f3"
      )
    )

  private def doknjasBishopUnlockInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r4rk1/p3ppbp/5np1/3P1b2/5N2/4P3/Bq2NPPP/R2Q1RK1 b - - 1 17",
      playedMoveUci = "f6h5",
      variations = List(VariationLine(
        List("f6h5", "f4h5", "b2a1", "d1a4", "a1b2", "h5g7", "f5d3", "d5d6", "b2e2", "f1a1", "e7e5"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(33),
      openingContext = Some(RawOpeningContext(name = Some("Grunfeld / queenside initiative")))
    )

  private def gamechangerKnightRetreatInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "3q1rk1/p1r1bpp1/bpn1p2p/3pP1N1/3P1B1P/2P1R1P1/P4PB1/R2Q2K1 w - - 0 18",
      playedMoveUci = "g5h3",
      variations = List(VariationLine(List("g5h3"), scoreCp = 0, depth = 12)),
      currentEvalCp = Some(0),
      ply = Some(34),
      openingContext = Some(RawOpeningContext(name = Some("Queenside Indian / kingside play")))
    )

  private def bosboomKnightSacrificeInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "rn2k2r/1bqp1pp1/pp2pn2/2b4p/P3P3/1NNB3P/1PP1QPP1/R1B2RK1 b kq - 2 11",
      playedMoveUci = "f6g4",
      variations = List(VariationLine(
        List("f6g4", "h3g4", "h5g4", "e4e5", "b7f3", "g2f3", "c7d8"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(21),
      openingContext = Some(RawOpeningContext(name = Some("Sicilian Kan / h-file attack")))
    )

  private def nimzoLeningradExchangeInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "rnbqk2r/pp3ppp/3ppn2/2pP2B1/1bP5/2N1P3/PP3PPP/R2QKBNR b KQkq - 0 6",
      playedMoveUci = "b4c3",
      variations = List(VariationLine(
        List("b4c3", "b2c3", "h7h6", "g5h4", "e6e5", "f1d3", "e5e4", "d3c2", "g7g5", "h4g3", "d8e7", "g1e2"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(11),
      openingContext = Some(RawOpeningContext(name = Some("Nimzo-Indian Leningrad / dark-square bind and pawn wedge"))),
      movePrefixUci = List("d2d4", "g8f6", "c2c4", "e7e6", "b1c3", "f8b4", "c1g5", "c7c5", "d4d5", "d7d6", "e2e3")
    )

  private def carlsbadBishopExchangeInput(): RawMoveReviewInput =
    val base = RawMoveReviewInput(
      fen = "2nrrnk1/pp3ppp/2bq4/R2p4/3P1N2/3BP2P/P1QN1PP1/5RK1 w - - 5 22",
      playedMoveUci = "d3b5",
      variations = List(VariationLine(
        List("d3b5", "d6b4", "b5c6", "b4a5", "c6e8", "d8e8", "d2b3", "a5b5"),
        scoreCp = 151,
        depth = 18
      )),
      currentEvalCp = Some(151),
      ply = Some(42)
    )
    withReplyProbeFor(
      base,
      "d3b5",
      List(
        VariationLine(List("d6b4", "b5c6", "b4a5", "c6e8", "d8e8", "d2b3"), 154, depth = 16),
        VariationLine(List("c6b5", "a5b5", "c8b6", "a2a4", "f8g6", "f4g6"), 155, depth = 16),
        VariationLine(List("d8d7", "b5c6", "b7c6", "f1c1", "c8e7", "f4d3"), 159, depth = 16)
      )
    )

  private def withReplyProbe(input: RawMoveReviewInput, replyLines: List[VariationLine]): RawMoveReviewInput =
    withReplyProbeFor(input, "b7b5", replyLines)

  private def branchReplyRequests(result: MoveReviewJudgmentResult) =
    result.packet.probeRequests.filter(_.objective.contains(BranchReplyProbeBinding.Objective))

  private def withReplyProbeFor(
      input: RawMoveReviewInput,
      candidateMove: String,
      replyLines: List[VariationLine]
  ): RawMoveReviewInput =
    val request = MoveReviewJudgmentOrchestrator
      .build(input)
      .getOrElse(fail("expected initial probe request"))
      .packet
      .probeRequests
      .find(request =>
        request.objective.contains(BranchReplyProbeBinding.Objective) &&
          request.candidateMove.exists(EvidenceRef.sameMove(_, candidateMove))
      )
      .getOrElse(fail(s"expected $candidateMove reply probe request"))
    input.copy(probeResults = input.probeResults :+
      ProbeResult(
        id = request.id,
        fen = Some(request.fen),
        evalCp = 0,
        replyLines = Some(replyLines),
        deltaVsBaseline = 0,
        purpose = request.purpose,
        probedMove = request.candidateMove,
        depth = Some(request.depth),
        objective = request.objective,
        requiredSignals = request.requiredSignals,
        horizon = request.horizon,
        candidateMove = request.candidateMove,
        depthFloor = request.depthFloor,
        variationHash = request.variationHash
      )
    )

  private def causalEvent(
      result: MoveReviewJudgmentResult,
      rootMove: String,
      futureMove: String
  ): PlanCausalEventEvidence =
    result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, rootMove) && payload.episode.exists(
            _.events.exists(event => EvidenceRef.sameMove(event.moveUci, futureMove))
          ) =>
        payload
    }.getOrElse(fail("expected causal event"))

  private def planCoherenceSignals(
      result: MoveReviewJudgmentResult,
      rootMove: String
  ): List[StrategicMechanismSignal] =
    result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _)
          if ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, rootMove)) &&
            payload.kind == StrategicMechanismKind.PlanPressure =>
        payload.signals.filter(_.axis.exists(_.kind == StrategicAxisKind.PlanCoherence))
    }.flatten

  private def planRelativeCauses(
      result: MoveReviewJudgmentResult,
      rootMove: String
  ): List[RelativeCauseKind] =
    result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if EvidenceRef.sameMove(cause.candidateLine.rootMove, rootMove) =>
        cause.kind
    }.distinct

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = producer,
      layer = layer,
      position = position,
      line = line,
      scope = scope,
      confidence = EvidenceConfidence.EngineBacked
    )

  test("stationary material costs stay with their collector and survive plan compression"):
    val singleCost = MoveReviewJudgmentOrchestrator
      .build(RawMoveReviewInput(
        fen = "r4rkb/4np1p/p1b1p1pP/1p2P3/3P4/3BBN2/P2K1PP1/1R5R w - - 1 20",
        playedMoveUci = "f3g5",
        variations = List(
          VariationLine(List("f3g5", "c6d5", "h1c1", "d5a2", "b1a1", "a2d5", "c1c7", "e7f5", "g2g4", "f5h6", "f2f3", "f8c8", "c7c8", "a8c8"), 77, depth = 18),
          VariationLine(List("f3h4", "f7f6", "f2f4", "c6d5", "h1c1", "d5a2", "b1a1", "a2d5", "c1c7", "f8f7", "g2g4", "g6g5", "e5f6", "g5f4"), 23, depth = 18),
          VariationLine(List("d3c2", "f7f6", "c2b3", "g8f7", "e5f6", "h8f6", "f3g5", "f6g5", "e3g5", "c6d5", "h1e1", "f8d8", "b1c1", "a8a7"), 21, depth = 18)
        ),
        currentEvalCp = Some(77),
        ply = Some(38)
      ))
      .getOrElse(fail("expected single stationary-cost result"))
    val consecutiveCosts = MoveReviewJudgmentOrchestrator
      .build(RawMoveReviewInput(
        fen = "r2q1r1k/bpp2p1p/p1np3p/3Bp2b/P3P3/2PP1N1P/1P1N1PP1/R2Q1RK1 b - - 3 13",
        playedMoveUci = "c6e7",
        variations = List(
          VariationLine(List("c6e7", "d5b7", "a8b8", "b7a6", "f7f5", "a6c4", "f5e4", "d3e4", "e7g6", "g1h2", "b8b2", "d1c1", "b2d2", "f3d2"), 0, depth = 18),
          VariationLine(List("d8c8", "d5c6", "b7c6", "g2g4", "f8g8", "g1h1", "h5g6", "d1e2", "a8b8", "b2b3", "h6h5", "f1g1", "h5g4", "h3g4"), 5, depth = 18),
          VariationLine(List("a8b8", "g1h1", "c6e7", "d5a2", "f7f5", "e4f5", "h5f3", "d2f3", "e7f5", "d1d2", "f5h4", "f3h4", "d8h4", "f2f4"), 6, depth = 18)
        ),
        currentEvalCp = Some(0),
        ply = Some(25)
      ))
      .getOrElse(fail("expected consecutive stationary-cost result"))

    def playedEvent(result: MoveReviewJudgmentResult, planId: PlanId): PlanCausalEventEvidence =
      result.packet.evidenceGraph.records.collectFirst {
        case EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
            if ref.line.exists(_.role == LineNodeRole.Played) && event.planId == planId => event
      }.getOrElse(fail(s"expected played $planId event"))

    def compensationClaim(result: MoveReviewJudgmentResult): MoveMeaningClaim =
      result.packet.moveJudgmentView.toList
        .flatMap(_.moveMeaningClaims)
        .find(claim =>
          claim.unit == PositionPlanTechniqueUnit.CompensationSource &&
            claim.publicSurfaceAdmitted &&
            claim.publicProofLevel == "owned_function"
        )
        .getOrElse(fail("expected public owned material compensation"))

    def sacrificeSquares(claim: MoveMeaningClaim): List[String] =
      claim.boardCarriers.collect {
        case carrier
            if carrier.role == "target" &&
              carrier.kind == "PlanSubject" &&
              carrier.value.startsWith("material-sacrifice:") =>
          carrier.value.stripPrefix("material-sacrifice:")
      }

    def publicPlayedPlan(result: MoveReviewJudgmentResult): play.api.libs.json.JsObject =
      val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
      (MoveMeaningSurface.publicPayloadJson(view) \ "explanations")
        .as[List[play.api.libs.json.JsObject]]
        .find(explanation => (explanation \ "role").asOpt[String].contains("played move"))
        .flatMap(explanation => (explanation \ "plan").asOpt[play.api.libs.json.JsObject])
        .getOrElse(fail("expected played public plan"))

    val singleEvent = playedEvent(singleCost, PlanId.WeakPawnAttack)
    assertEquals(singleEvent.observedMaterialCosts.map(_.capture.square.key), List("a2"))
    assertEquals(sacrificeSquares(compensationClaim(singleCost)), List("a2"))
    val singlePlan = publicPlayedPlan(singleCost)
    val singleReplies = (singlePlan \ "opponent_replies")
      .as[List[play.api.libs.json.JsObject]]
      .flatMap(reply => (reply \ "reply" \ "uci").asOpt[String])
    assertEquals(singleReplies, Nil)

    val consecutiveEvent = playedEvent(consecutiveCosts, PlanId.PieceActivation)
    assertEquals(consecutiveEvent.observedMaterialCosts.map(_.capture.square.key), List("b7", "a6"))
    assert(!consecutiveEvent.materialTradeoffs.exists(_._3.kind == TransitionConsequenceKind.PawnTensionGain))
    val consecutiveBreak = playedEvent(consecutiveCosts, PlanId.PawnBreakPreparation)
    assertEquals(consecutiveBreak.observedMaterialCosts.map(_.capture.square.key), List("b7", "a6"))
    assert(consecutiveBreak.materialTradeoffs.exists(_._3.kind == TransitionConsequenceKind.PawnTensionGain))
    assertEquals(ResolvedPlanEvent.from(consecutiveBreak).materialCostSquares, List("b7", "a6"))
    val branchSourcedBreak = consecutiveBreak.copy(continuationSourceLine = Some(
      consecutiveBreak.rootLine.copy(id = s"${consecutiveBreak.rootLine.id}:threat", role = LineNodeRole.Threat)
    ))
    assertEquals(branchSourcedBreak.sourceOwnedMaterialCosts, Nil)
    assertEquals(branchSourcedBreak.materialTradeoffs, Nil)
    assertEquals(ResolvedPlanEvent.from(branchSourcedBreak).materialCostSquares, Nil)
    assertEquals(sacrificeSquares(compensationClaim(consecutiveCosts)), List("b7", "a6"))
    val consecutivePlan = publicPlayedPlan(consecutiveCosts)
    val consecutiveReplies = (consecutivePlan \ "opponent_replies")
      .as[List[play.api.libs.json.JsObject]]
      .flatMap(reply => (reply \ "reply" \ "uci").asOpt[String])
    assertEquals(consecutiveReplies.toSet, Set("d5b7", "b7a6"))
    val publicResultKinds = (consecutivePlan \ "results")
      .as[List[play.api.libs.json.JsObject]]
      .flatMap(result => (result \ "change").asOpt[String])
    assert(publicResultKinds.contains("pawn tension gain"), publicResultKinds)

  test("a pawn-route blockade needs reply persistence or an owned pawn restraint plan"):
    val cases = List(
      RawMoveReviewInput(
        fen = "2r5/p3bk1p/1p2p1p1/1P1n4/P1p1N3/4P3/3BKPPP/2R5 w - - 1 25",
        playedMoveUci = "d2c3",
        variations = List(VariationLine(List("d2c3", "g6g5", "e4d2", "e7f6"), 50, depth = 18)),
        currentEvalCp = Some(50),
        ply = Some(48)
      ) -> true,
      RawMoveReviewInput(
        fen = "1r2k2r/pbppnppp/1bn5/4P2q/Q3N3/B1PB1N2/P4PPP/R3R1K1 w k - 1 17",
        playedMoveUci = "e4f6",
        variations = List(VariationLine(List("e4f6", "g7f6", "e5f6", "h8g8"), 0, depth = 12)),
        currentEvalCp = Some(0),
        ply = Some(32)
      ) -> false,
      RawMoveReviewInput(
        fen = "r1b2rk1/p3ppbp/5np1/qpnP4/4PB2/2N2P2/PP4PP/2RQKBNR w K - 0 11",
        playedMoveUci = "b2b4",
        variations = List(VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2"), 0, depth = 12)),
        currentEvalCp = Some(0),
        ply = Some(20)
      ) -> true
    )

    cases.foreach { case (input, expectedPublicRestriction) =>
      val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected judgment result"))
      val structural = result.packet.evidenceGraph.records.collectFirst {
        case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
            if ref.line.exists(_.role == LineNodeRole.Played) && EvidenceRef.sameMove(payload.moveUci, input.playedMoveUci) =>
          payload
      }.getOrElse(fail("expected played structural delta"))
      val publicRestriction = result.packet.moveJudgmentView.toList
        .flatMap(MoveMeaningSurface.publicSurfaces)
        .exists(surface =>
          surface.subject == "played_move" &&
            surface.moveUci == input.playedMoveUci &&
            surface.idea.code == "counterplay_control"
        )

      assert(structural.hasOpponentMobilityRestriction, structural)
      assert(!structural.hasReplyIndependentOpponentMobilityRestriction, structural)
      assertEquals(publicRestriction, expectedPublicRestriction, input.playedMoveUci)
    }

  private final case class ResourceProbeOutcome(
      rootMove: String,
      resourceMove: String,
      whitePovEvalCp: Int,
      moves: List[String]
  )

  private def withOpponentResourceProbes(
      input: RawMoveReviewInput,
      outcomes: List[ResourceProbeOutcome]
  ): RawMoveReviewInput =
    val requests = MoveReviewJudgmentOrchestrator
      .build(input)
      .getOrElse(fail("expected opponent-resource probe requests"))
      .packet
      .probeRequests
    val results = outcomes.map { outcome =>
      val request = requests.find(request =>
        request.objective.contains(CounterResourceProbeBinding.Objective) &&
          request.candidateMove.exists(EvidenceRef.sameMove(_, outcome.rootMove)) &&
          request.opponentResourceMove.exists(EvidenceRef.sameMove(_, outcome.resourceMove))
      ).getOrElse(fail(s"expected ${outcome.rootMove} ${outcome.resourceMove} resource probe"))
      ProbeResult(
        id = request.id,
        fen = Some(request.fen),
        evalCp = outcome.whitePovEvalCp,
        replyLines = Some(List(VariationLine(outcome.moves, outcome.whitePovEvalCp, depth = 16))),
        deltaVsBaseline = outcome.whitePovEvalCp - request.baselineEvalCp.getOrElse(0),
        purpose = request.purpose,
        probedMove = request.candidateMove,
        depth = Some(16),
        objective = request.objective,
        requiredSignals = request.requiredSignals,
        horizon = request.horizon,
        candidateMove = request.candidateMove,
        opponentResourceMove = request.opponentResourceMove,
        depthFloor = request.depthFloor,
        variationHash = request.variationHash
      )
    }
    input.copy(probeResults = results)

  private def opponentResourceDeterrenceEvent(
      result: MoveReviewJudgmentResult,
      rootMove: String
  ): Option[(EvidenceRecord, PlanCausalEventEvidence)] =
    result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(event.rootMove, rootMove) && event.opponentResourceDeterrenceProofReady =>
        record -> event
    }

  test("shared opponent-resource probes bind indirect break deterrence to one causal event"):
    val rios = RawMoveReviewInput(
      fen = "1rr5/pp1nkp1p/2p1pn2/N5p1/1PNP4/P5P1/4PPKP/2RR4 w - - 0 22",
      playedMoveUci = "c4b2",
      variations = List(
        VariationLine(List("d1e1", "f6e8", "e2e4", "h7h6", "h2h3", "c8c7", "a5b3", "e8d6", "c4d6", "e7d6", "g2f3", "b7b6"), 67, depth = 18),
        VariationLine(List("c4b2", "h7h5", "d1h1", "c8c7", "h2h4", "g5g4", "c1c2", "f6e4", "b2d3", "e4d6", "a5b3", "d6e4", "a3a4", "b8d8"), 59, depth = 18),
        VariationLine(List("c4d2", "f6e8", "e2e4", "f7f6", "d2c4", "d7b6", "h2h4", "g5g4", "c4e3", "h7h5", "c1c5", "e8g7", "a5b3", "c8d8"), 67, depth = 18)
      ),
      currentEvalCp = Some(67),
      ply = Some(42)
    )
    val gameChanger = RawMoveReviewInput(
      fen = "r4rkb/4np1p/p1b1p1pP/1p2P3/3P4/3BBN2/P2K1PP1/1R5R w - - 1 20",
      playedMoveUci = "f3g5",
      variations = List(
        VariationLine(List("f3g5", "c6d5", "h1c1", "d5a2", "b1a1", "a2d5", "c1c7", "e7f5", "g2g4", "f5h6", "f2f3", "f8c8", "c7c8", "a8c8"), 77, depth = 18),
        VariationLine(List("f3h4", "f7f6", "f2f4", "c6d5", "h1c1", "d5a2", "b1a1", "a2d5", "c1c7", "f8f7", "g2g4", "g6g5", "e5f6", "g5f4"), 23, depth = 18),
        VariationLine(List("d3c2", "f7f6", "c2b3", "g8f7", "e5f6", "h8f6", "f3g5", "f6g5", "e3g5", "c6d5", "h1e1", "f8d8", "b1c1", "a8a7"), 21, depth = 18)
      ),
      currentEvalCp = Some(77),
      ply = Some(38)
    )
    val classics = RawMoveReviewInput(
      fen = "3r2k1/pp3ppp/1qnr2n1/3p4/5P2/1NPB2P1/PPQ2P2/R3R1K1 w - - 1 20",
      playedMoveUci = "b3d2",
      variations = List(
        VariationLine(List("b3d2", "g6f8", "b2b4", "g7g6", "a2a3", "a7a5", "a1d1", "a5b4", "a3b4", "d6e6", "d2f3", "e6e1", "d1e1", "f8d7"), 90, depth = 18),
        VariationLine(List("a1d1", "d5d4", "d3e4", "d4c3", "d1d6", "d8d6", "c2c3", "h7h5", "c3c5", "b6b4", "c5b4", "c6b4", "a2a3", "b4c6"), 88, depth = 18),
        VariationLine(List("a2a4", "h7h5", "b3d2", "h5h4", "g1g2", "b6c7", "d2f3", "h4g3", "f2g3", "d5d4", "c3d4", "c7b6", "d3g6", "d6g6"), 65, depth = 18)
      ),
      currentEvalCp = Some(90),
      ply = Some(38)
    )
    val cases = List(
      (
        withOpponentResourceProbes(rios, List(
          ResourceProbeOutcome("c4b2", "b7b6", 457, List("b7b6", "a5c6", "c8c6", "c1c6")),
          ResourceProbeOutcome("d1e1", "b7b6", 78, List("b7b6", "a5b3"))
        )),
        "c4b2",
        "d1e1",
        "b7b6",
        "a5c6"
      ),
      (
        withOpponentResourceProbes(gameChanger, List(
          ResourceProbeOutcome("f3g5", "f7f6", 193, List("f7f6", "g5e6")),
          ResourceProbeOutcome("f3h4", "f7f6", 21, List("f7f6", "f2f4"))
        )),
        "f3g5",
        "f3h4",
        "f7f6",
        "g5e6"
      ),
      (
        withOpponentResourceProbes(classics, List(
          ResourceProbeOutcome("b3d2", "d5d4", 382, List("d5d4", "d2c4", "b6c5", "c4d6", "c5d6")),
          ResourceProbeOutcome("a1d1", "d5d4", 84, List("d5d4", "b3d2"))
        )),
        "b3d2",
        "a1d1",
        "d5d4",
        "d2c4"
      )
    )

    val evaluatedCases = cases.map { case entry @ (input, _, _, _, _) =>
      entry -> MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected deterrence result"))
    }
    evaluatedCases.foreach { case ((_, rootMove, controlMove, resourceMove, responseMove), result) =>
      val branchRequests = result.packet.probeRequests.filter(_.purpose.exists(ProbePurpose.isBranchReply))
      val resolvedProbeIds = result.packet.probeDiagnostics
        .filter(_.purpose.exists(ProbePurpose.isBranchReply))
        .map(_.probeId)
        .toSet
      assertEquals(
        result.quality.semanticCoverage.branchReplyProbePendingRequests,
        branchRequests.count(request => !resolvedProbeIds(request.id)),
        rootMove
      )
      val (record, event) = opponentResourceDeterrenceEvent(result, rootMove).getOrElse(
        fail(
          s"expected $rootMove opponent-resource deterrence; diagnostics=${result.packet.probeDiagnostics}; " +
            s"plans=${result.packet.evidenceGraph.records.collect { case EvidenceRecord(_, candidate: PlanCausalEventEvidence, _) if EvidenceRef.sameMove(candidate.rootMove, rootMove) => candidate.planId }}"
        )
      )
      val proof = event.opponentResourceDeterrence.getOrElse(fail("expected owned deterrence proof"))
      val publicEvent = ResolvedPlanEvent.from(event)
      val sequenceSummary = event.planSequenceSummary.getOrElse(fail("expected deterrence plan sequence"))
      if proof.responseStep != proof.materialResultStep then
        assert(!event.materialCounterplayPreventionProofReady, proof)
      assert(
        !result.validation.issues.exists(_.subjectId == record.ref.id),
        result.validation.issues.filter(_.subjectId == record.ref.id)
      )
      assertEquals(event.identity.goalTheme, PlanTheme.RestrictionProphylaxis)
      assertEquals(event.identity.goalKind, Some(PlanKind.ProphylaxisRestraint))
      assert(EvidenceRef.sameMove(proof.resourceMove, resourceMove), proof)
      assert(EvidenceRef.sameMove(proof.responseStep.moveUci, responseMove), proof)
      assert(proof.comparisons.forall(_.resourceLine != proof.resourceLine), proof)
      assert(event.directGoalConsequences.exists(_.subjects.contains(
        s"pawn:${resourceMove.take(2)}-${resourceMove.slice(2, 4)}:advance-restricted"
      )), event.directGoalConsequences)
      assertEquals(sequenceSummary.transitionType, TransitionType.Opening)
      assert(sequenceSummary.continuity.exists(_.supportingEvents.exists(step =>
        EvidenceRef.sameMove(step.rootMove, proof.materialGain.moveUci)
      )), sequenceSummary)
      assertEquals(publicEvent.transitionType, Some(TransitionType.Opening))
      val resourceResponse = publicEvent.responses.find(response =>
        EvidenceRef.sameMove(response.move, resourceMove) && response.realizationMove.exists(EvidenceRef.sameMove(_, responseMove))
      ).getOrElse(fail(s"expected public $resourceMove response: ${publicEvent.responses}"))
      assertEquals(resourceResponse.line.map(_.uci), proof.resourceSequence.map(_.moveUci))
      assertEquals(resourceResponse.line.headOption.map(_.turn.side), Some("black"))
      assertEquals(
        resourceResponse.line.headOption.map(_.turn.moveNumber),
        publicEvent.rootMoveReference.map(_.turn.moveNumber)
      )
      assert(publicEvent.testedContinuation.exists(_.sequence.exists(step =>
        EvidenceRef.sameMove(step.move, proof.materialGain.moveUci)
      )), publicEvent)
      assert(!opponentResourceDeterrenceEvent(result, controlMove).isDefined, controlMove)
      val publicClaims = result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims)
      assert(publicClaims.exists(claim =>
        EvidenceRef.sameMove(claim.moveUci, rootMove) && claim.resolvedPlanEvent.exists(_.responses.exists(response =>
          EvidenceRef.sameMove(response.move, resourceMove)
        ))
      ), publicClaims.map(claim => claim.moveUci -> claim.meaningKind))
      val publicJson = MoveMeaningSurface.publicPayloadJson(result.packet.moveJudgmentView.get)
      val resourceLabel = s"${resourceMove.take(2)}-${resourceMove.slice(2, 4)}"
      val responseLabel = resourceResponse.realizationReference.map(_.san).getOrElse(responseMove)
      val expectedIdeaName =
        if event.materialCounterplayPreventionProofReady then
          s"tests $resourceLabel with $responseLabel; material gain appears in that line"
        else s"answers $resourceLabel with $responseLabel in a tested line"
      assert((publicJson \\ "ideas").flatMap(_.asOpt[List[JsObject]].getOrElse(Nil)).exists(idea =>
        (idea \ "kind").asOpt[String].contains("counterplay_control") &&
          (idea \ "name").asOpt[String].contains(expectedIdeaName)
      ), publicJson)
      assert((publicJson \\ "ply_offset").isEmpty, publicJson)
    }
    val deterrence = opponentResourceDeterrenceEvent(evaluatedCases.head._2, cases.head._2)
      .map(_._2)
      .getOrElse(fail("expected representative deterrence fixture"))
    val directMobility = TransitionConsequence(
      TransitionConsequenceKind.MobilityGain,
      StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("piece:knight:b2")
    )
    val directFunction = deterrence.copy(
      planId = PlanId.PieceActivation,
      identity = deterrence.identity.copy(
        goalTheme = PlanTheme.PieceRedeployment,
        goalKind = Some(PlanKind.WorstPieceImprovement),
        targets = directMobility.goalSubjects,
        results = List("kind:mobilitygain")
      ),
      structuralConsequences = List(directMobility),
      developmentChoices = Nil,
      branchWitnesses = Nil,
      episode = None,
      observedMaterialCosts = Nil,
      opponentResourceDeterrence = None,
      continuationSourceLine = None
    )
    val deterrenceKey = deterrence.principalExplanationSortKey.getOrElse(fail("expected deterrence key"))
    val directFunctionKey = directFunction.principalExplanationSortKey.getOrElse(fail("expected direct-function key"))
    assert(deterrenceKey._1 > directFunctionKey._1, deterrenceKey -> directFunctionKey)
    assert(!deterrence.principalCurrentMoveFunctionProofReady, deterrence)
    assert(directFunction.principalCurrentMoveFunctionProofReady, directFunction)
    assertEquals(
      PositionPlanTechniqueProjection
        .principalCausalSelectionPool(
          List(deterrence -> deterrenceKey, directFunction -> directFunctionKey),
          _.principalCurrentMoveFunctionProofReady
        )
        .map(_._1.planId),
      List(PlanId.PieceActivation)
    )
    assertEquals(
      PositionPlanTechniqueProjection
        .principalCausalSelectionPool(List(deterrence -> deterrenceKey), _.principalCurrentMoveFunctionProofReady)
        .map(_._1.planId),
      List(deterrence.planId)
    )
    val directRestriction = deterrence.copy(
      branchWitnesses = Nil,
      episode = None,
      observedMaterialCosts = Nil,
      opponentResourceDeterrence = None,
      continuationSourceLine = None
    )
    assert(directFunction.directPieceRoleProofReady, directFunction)
    assert(directRestriction.directGoalConsequences.nonEmpty, directRestriction)
    assert(
      PositionPlanTechniqueProjection.principalCausalOwnershipTier(directFunction, false) <
        PositionPlanTechniqueProjection.principalCausalOwnershipTier(directRestriction, false)
    )
    val strippedClientCarrier = cases.head._1.copy(
      probeResults = cases.head._1.probeResults.map(_.copy(objective = None, requiredSignals = Nil))
    )
    val strippedResult = MoveReviewJudgmentOrchestrator.build(strippedClientCarrier).getOrElse(
      fail("expected public client resource carrier to close the probe contract")
    )
    assert(opponentResourceDeterrenceEvent(strippedResult, "c4b2").nonEmpty)
    val missingResourceCarrier = cases.head._1.copy(
      probeResults = cases.head._1.probeResults.map(_.copy(opponentResourceMove = None))
    )
    val rejected = MoveReviewJudgmentOrchestrator.build(missingResourceCarrier).getOrElse(
      fail("expected fail-closed resource result")
    )
    assert(rejected.packet.probeDiagnostics.exists(diagnostic =>
      diagnostic.status == ProbeAdmissionStatus.Rejected &&
        diagnostic.reasonCodes.contains("OPPONENT_RESOURCE_MISSING")
    ), rejected.packet.probeDiagnostics)
    val rejectedBranchRequests = rejected.packet.probeRequests.filter(_.purpose.exists(ProbePurpose.isBranchReply))
    val rejectedResolvedProbeIds = rejected.packet.probeDiagnostics
      .filter(_.purpose.exists(ProbePurpose.isBranchReply))
      .map(_.probeId)
      .toSet
    assertEquals(
      rejected.quality.semanticCoverage.branchReplyProbePendingRequests,
      rejectedBranchRequests.count(request => !rejectedResolvedProbeIds(request.id))
    )
    assertEquals(opponentResourceDeterrenceEvent(rejected, "c4b2"), None)

  test("a shared resource without root-specific contrast does not become deterrence"):
    val input = RawMoveReviewInput(
      fen = "r4rkb/2R2p1p/p3p1pP/1p1bPnN1/3P4/3BB3/P2K1P2/1R6 w - - 4 23",
      playedMoveUci = "g5e4",
      variations = List(
        VariationLine(List("g5e4", "d5a2", "b1a1", "a2c4", "d3c4", "b5c4", "e4c5", "a8c8", "c7c8", "f8c8", "c5e4", "c4c3", "d2c2", "f5e7"), 16, depth = 18),
        VariationLine(List("d3f5", "e6f5", "b1c1", "d5c4", "a2a4", "a8c8", "c7a7", "c8a8", "a7b7", "a8b8", "b7b8", "f8b8", "f2f4", "f7f6"), 7, depth = 18),
        VariationLine(List("g5h3", "d5a2", "b1a1", "a2c4", "d3f5", "e6f5", "e3g5", "a6a5", "g5e7", "f8b8", "h3g5", "a5a4", "f2f4", "c4b3"), 2, depth = 18)
      ),
      currentEvalCp = Some(16),
      ply = Some(44)
    )
    val probed = withOpponentResourceProbes(input, List(
      ResourceProbeOutcome("g5e4", "f7f6", 40, List("f7f6", "e4f6", "g8f7")),
      ResourceProbeOutcome("d3f5", "f7f6", 66, List("f7f6", "f5e6", "g8f7"))
    ))
    val result = MoveReviewJudgmentOrchestrator.build(probed).getOrElse(fail("expected no-contrast result"))

    assertEquals(opponentResourceDeterrenceEvent(result, "g5e4"), None)

  test("a same-knight route exposes its later restriction after reply testing"):
    val input = RawMoveReviewInput(
      fen = "1rb2rk1/5pp1/2p2n2/p2p3p/1q6/1P2P1P1/P1Q2PBP/RNR3K1 w - - 0 16",
      playedMoveUci = "b1c3",
      variations = List(
        VariationLine(List("b1d2", "c8d7", "a2a3", "b4b6", "c2c5", "b6c7", "c5d4", "d7e6", "c1c3", "f6d7", "e3e4", "d5e4", "d4e4", "f8d8", "c3c6", "c7e5"), 16, depth = 18),
        VariationLine(List("h2h4", "c8g4", "b1d2", "f8c8", "d2f3", "f6d7", "a1b1", "g4f3", "g2f3", "g7g6", "c2b2", "a5a4", "f3g2", "a4b3", "b2b3", "b4b3"), 2, depth = 18),
        VariationLine(List("b1c3", "h5h4", "c3e2", "c8d7", "c2c5", "f8c8", "c5b4", "b8b4", "e2f4", "a5a4", "f4d3", "a4b3", "g3h4", "b4b8", "a2b3", "b8b3"), -19, depth = 18)
      ),
      currentEvalCp = Some(16),
      ply = Some(30)
    )
    def routeEvent(result: MoveReviewJudgmentResult) =
      result.packet.evidenceGraph.records.collectFirst {
        case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
            if EvidenceRef.sameMove(payload.rootMove, "b1c3") &&
              payload.planId == PlanId.PieceActivation &&
              payload.identity.goalTheme == PlanTheme.PieceRedeployment &&
              payload.identity.actorRole.exists(_.equalsIgnoreCase("knight")) =>
          payload
      }.getOrElse(fail(s"expected same-knight route event; records=${result.packet.evidenceGraph.records.collect {
        case EvidenceRecord(_, payload: PlanCausalEventEvidence, _) if EvidenceRef.sameMove(payload.rootMove, "b1c3") =>
          payload.identity -> payload.episode
      }}"))

    val unprobed = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected move review"))
    val unprobedEvent = routeEvent(unprobed)
    assertEquals(unprobedEvent.robustness, PlanCausalRobustness.Untested)
    assert(!unprobedEvent.episodePublicProofReady)
    assert(branchReplyRequests(unprobed).flatMap(_.candidateMove).exists(EvidenceRef.sameMove(_, "b1c3")))
    val unprobedIdeas = (MoveMeaningSurface.publicPayloadJson(
      unprobed.packet.moveJudgmentView.getOrElse(fail("expected unprobed public view"))
    ) \ "explanations")
      .asOpt[List[JsObject]]
      .getOrElse(Nil)
      .find(explanation => (explanation \ "move").asOpt[String].exists(EvidenceRef.sameMove(_, "b1c3")))
      .flatMap(explanation => (explanation \ "ideas").asOpt[List[JsObject]])
      .getOrElse(Nil)
    assert(!unprobedIdeas.exists(idea => (idea \ "kind").asOpt[String].contains("counterplay_control")))

    val probedInput = withReplyProbeFor(
      input,
      "b1c3",
      List(
        VariationLine(List("h5h4", "c3e2", "c8d7", "e2f4", "f8e8", "g3h4", "f6e4", "c1d1"), 0, depth = 16),
        VariationLine(List("f8e8", "c3e2", "c8d7", "e2d4", "b8c8", "c2c5", "h5h4", "c1c2"), 0, depth = 16),
        VariationLine(List("c8e6", "c3e2", "f8c8", "e2f4", "c6c5", "a1b1", "h5h4", "g3h4"), 0, depth = 16)
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(probedInput).getOrElse(fail("expected tested move review"))
    val event = routeEvent(result)

    val restrictionResult = event.resolvedGoalResultAssessments
      .find(_.consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction)
      .getOrElse(
        fail(s"expected tested restriction; episode=${event.episode.map(_.continuations.map(node => node.moveUci -> node.structuralConsequences))}")
      )
    assertEquals(
      restrictionResult.sourceEvent.moveUci,
      "e2f4",
      event.episode.toList.flatMap(_.continuations).flatMap(node =>
        node.structuralConsequences
          .filter(_.kind == TransitionConsequenceKind.OpponentMobilityRestriction)
          .map(consequence => node.moveUci -> consequence.subjects)
      )
    )
    assert(!event.resolvedGoalResultAssessments.exists(
      _.consequence.kind == TransitionConsequenceKind.BatteryPressureGain
    ))
    assertEquals(event.futureMove, Some("e2f4"))
    assertEquals(
      event.robustness,
      PlanCausalRobustness.Conditional,
      event.branchCoverageComplete -> event.branchWitnesses.map(witness =>
        (
          witness.line.rootMove,
          witness.outcome,
          witness.requiredPlyOffset,
          witness.observedPlyOffset,
          witness.observedThroughPlyOffset,
          witness.realizationMove
        )
      )
    )
    assert(event.episodePublicProofReady)
    assert(!event.episode.toList.flatMap(_.continuations).exists(node =>
      EvidenceRef.sameMove(node.moveUci, "f4d3") &&
        node.structuralConsequences.exists(_.kind == TransitionConsequenceKind.OpponentMobilityRestriction)
    ))
    val publicView = result.packet.moveJudgmentView.getOrElse(fail("expected public view"))
    val eventClaims = publicView.moveMeaningClaims.filter(_.resolvedPlanEvent.exists(plan =>
      EvidenceRef.sameMove(plan.rootMove, "b1c3") && plan.goalTheme == PlanTheme.PieceRedeployment.id
    ))
    assert(eventClaims.nonEmpty, publicView.moveMeaningClaims.map(claim =>
      (
        claim.moveUci,
        claim.label,
        claim.publicSurfaceAdmitted,
        claim.resolvedPlanEvent.map(plan => plan.goalTheme -> plan.rootMove)
      )
    ))
    assert(eventClaims.exists(_.publicSurfaceAdmitted), eventClaims.map(claim =>
      (
        claim.meaningKind,
        claim.unit,
        claim.axisPolarity,
        claim.label,
        claim.supportLevel,
        claim.visibility,
        claim.surfaceLane,
        claim.publicSurfaceAdmitted,
        claim.publicIdeaType,
        claim.representativePlanEvent,
        claim.displayPlanId,
        claim.positiveFunctionalProofEvidenceIds,
        claim.testedContinuation,
        claim.objectCarrierReady
      )
    ))
    val publicJson = MoveMeaningSurface.publicPayloadJson(publicView)
    val playedExplanation = (publicJson \ "explanations")
      .asOpt[List[JsObject]]
      .getOrElse(Nil)
      .find(explanation => (explanation \ "move").asOpt[String].exists(EvidenceRef.sameMove(_, "b1c3")))
      .getOrElse(fail("expected played-move explanation"))
    val playedIdeas = (playedExplanation \ "ideas").asOpt[List[JsObject]].getOrElse(Nil)
    val continuity = playedIdeas.find(idea =>
      (idea \ "kind").asOpt[String].contains("plan_continuity")
    ).getOrElse(fail(s"expected representative plan continuity; ideas=$playedIdeas"))
    assert(!playedIdeas.exists(idea => (idea \ "kind").asOpt[String].contains("target_pressure")), playedIdeas)
    val representativeSquares = event.representativeResult.toList
      .flatMap(_._2.goalSubjects)
      .filter(_.matches("[a-h][1-8]"))
      .distinct
      .sorted
    assertEquals((continuity \ "target" \ "squares").asOpt[List[String]], Some(representativeSquares))
    val publicPlan = (playedExplanation \ "plan").as[JsObject]
    val publicResultChanges = (publicPlan \ "results").as[List[JsObject]]
      .flatMap(result => (result \ "change").asOpt[String])
    assert(publicResultChanges.contains("opponent mobility restriction"), publicResultChanges)
    val sequence = (publicPlan \ "method" \ "sequence").as[List[JsObject]]
    assertEquals(sequence.flatMap(step => (step \ "move" \ "uci").asOpt[String]), List("b1c3", "c3e2", "e2f4"))
    assertEquals(sequence.flatMap(step => (step \ "move" \ "notation").asOpt[String]), List("16.Nc3", "17.Ne2", "18.Nf4"))
    assertEquals(
      (publicPlan \ "continuation" \ "tested_reply_status").asOpt[String],
      Some("the continuation occurs against some tested replies")
    )

  test("a latent opponent route reaches public output only through its causal plan event"):
    val input = RawMoveReviewInput(
      fen = "3rq1k1/4rppp/2n3b1/pp2P3/2pP1QB1/P1P1R3/1B4PP/5RK1 w - - 2 25",
      playedMoveUci = "g4f3",
      variations = List(
        VariationLine(List("g4f3", "e8d7", "f1e1", "d7c7", "b2c1", "d8e8", "h2h4", "h7h6", "c1d2", "b5b4", "a3b4", "a5b4", "c3b4", "c7b6", "h4h5", "g6d3"), 144, depth = 18),
        VariationLine(List("h2h4", "f7f6", "e5e6", "h7h6", "f4f3", "b5b4", "a3b4", "a5b4", "f1d1", "h6h5", "g4h3", "g6d3", "d4d5", "c6e5", "e3e5"), 121, depth = 18),
        VariationLine(List("e3g3", "g6d3", "f1e1", "f7f5", "g4f5", "e8f7", "f4h6", "d3f5", "h6c6", "f7d5", "c6b6", "d8a8", "b6h6", "a8f8", "h2h3", "f5e6"), 110, depth = 18)
      ),
      currentEvalCp = Some(144),
      ply = Some(48),
      openingContext = Some(RawOpeningContext(name = Some("Reshevsky-Petrosian, Zurich Candidates 1953"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "e7e6", "b1c3", "f8b4", "e2e3", "e8g8",
        "f1d3", "d7d5", "g1f3", "c7c5", "e1g1", "b8c6", "a2a3", "b4c3",
        "b2c3", "b7b6", "c4d5", "e6d5", "c1b2", "c5c4", "d3c2", "c8g4",
        "d1e1", "f6e4", "f3d2", "e4d2", "e1d2", "g4h5", "f2f3", "h5g6",
        "e3e4", "d8d7", "a1e1", "d5e4", "f3e4", "f8e8", "d2f4", "b6b5",
        "c2d1", "e8e7", "d1g4", "d7e8", "e4e5", "a7a5", "e1e3", "a8d8"
      )
    )
    val result = MoveReviewJudgmentOrchestrator.build(input).getOrElse(fail("expected move review"))
    val publicView = result.packet.moveJudgmentView.getOrElse(fail("expected public view"))
    val publicClaims = publicView.moveMeaningClaims.filter(claim =>
      claim.publicSurfaceAdmitted && EvidenceRef.sameMove(claim.moveUci, "g4f3")
    )
    val eventClaims = publicClaims.filter(_.displayPlanId.contains(PlanId.Prophylaxis))

    assert(eventClaims.nonEmpty, publicView.moveMeaningClaims.map(claim =>
      (
        claim.meaningKind,
        claim.label,
        claim.publicSurfaceAdmitted,
        claim.displayPlanId,
        claim.resolvedPlanEvent,
        claim.positiveFunctionalProofEvidenceIds
      )
    ))
    assert(eventClaims.forall(claim =>
      claim.boardCarriers.exists(carrier =>
        carrier.role == "target" &&
          carrier.kind == "PlanSubject" &&
          StructuralDeltaEvidence.restrictedOpponentRoute(carrier.value).contains(List("c6", "e7", "d5"))
      ) &&
        claim.positiveFunctionalProofEvidenceIds.exists(id =>
          result.packet.evidenceGraph.byId.get(id).exists {
            case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
              EvidenceRef.sameMove(event.rootMove, "g4f3") &&
                event.identity.goalTheme == PlanTheme.RestrictionProphylaxis
            case _ => false
          }
        )
    ), eventClaims)
    assert(!publicClaims.exists(_.meaningKind == "CounterplayControl"), publicClaims)

    val eventSurfaces = MoveMeaningSurface.publicSurfaces(publicView).filter(surface =>
      EvidenceRef.sameMove(surface.moveUci, "g4f3") && surface.displayPlanId.contains(PlanId.Prophylaxis)
    )
    assert(eventSurfaces.exists(_.mainExplanationCandidate), eventSurfaces.map(surface =>
      (
        surface.idea.code,
        surface.idea.label,
        surface.mainExplanationCandidate,
        surface.representativePlanEvent,
        surface.evidence.proofLevel
      )
    ))

    val playedIdeas = (MoveMeaningSurface.publicPayloadJson(publicView) \ "explanations")
      .asOpt[List[JsObject]]
      .getOrElse(Nil)
      .find(explanation => (explanation \ "move").asOpt[String].exists(EvidenceRef.sameMove(_, "g4f3")))
      .flatMap(explanation => (explanation \ "ideas").asOpt[List[JsObject]])
      .getOrElse(Nil)
    assert(playedIdeas.exists(idea =>
      (idea \ "kind").asOpt[String].contains("counterplay_control") &&
        (idea \ "name").asOpt[String].contains("restrains c6-e7-d5")
    ), playedIdeas)

  test("a bad non-promoting move says it allows the opponent promotion"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "4R3/8/7p/p4k2/P5p1/3pK2n/8/8 w - - 0 62",
          playedMoveUci = "e3d3",
          variations = List(
            VariationLine(
              List("e8d8", "g4g3", "e3f3", "h3g1", "f3g3", "f5e4", "g3f2", "g1e2", "f2e1", "e4e3", "d8e8", "e3d4", "e1d2", "e2c3", "e8d8", "c3d5", "d8a8"),
              scoreCp = 0,
              depth = 18
            ),
            VariationLine(
              List("e3d3", "g4g3", "e8f8", "f5e6", "f8f3", "g3g2", "f3h3", "g2g1q", "h3h6", "e6f7"),
              scoreCp = -466,
              depth = 18
            )
          ),
          currentEvalCp = Some(-466),
          ply = Some(122)
        )
      )
      .getOrElse(fail("expected move review"))
    val playedPromotion = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .find(surface => EvidenceRef.sameMove(surface.moveUci, "e3d3") && surface.idea.code == "promotion")
      .getOrElse(fail("expected played promotion consequence"))
    val promotionConsequence = result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, line: LineFactEvidence, _)
          if ref.line.exists(node =>
            node.role == LineNodeRole.Played && EvidenceRef.sameMove(node.rootMove, "e3d3")
          ) => line.proofSignalConsequences
    }.flatten.find(_.kind == LineConsequenceKind.Promotion)
      .getOrElse(fail("expected played-line promotion consequence"))
    val (liabilityCauseRef, liabilityCause) = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.TacticalRefutationOfPlayed &&
            EvidenceRef.sameMove(cause.eventRootMove, "e3d3") => ref -> cause
    }.getOrElse(fail("expected typed promotion-liability cause"))
    val promotionClaims = result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims)
      .filter(claim =>
        EvidenceRef.sameMove(claim.moveUci, "e3d3") &&
          claim.terminalConsequenceKinds.contains(LineConsequenceKind.Promotion.toString)
      )

    assertEquals(promotionConsequence.rootMove, None)
    assertEquals(liabilityCause.sourceSide, RelativeCauseSourceSide.Candidate)
    assertEquals(liabilityCause.attribution.kind, CauseAttributionKind.CandidateAllowsLiability)
    assert(liabilityCause.attribution.rootMoveMatched && liabilityCause.attribution.directProofEligible, liabilityCause.attribution)
    assert(liabilityCause.proof.exists(_.directProof.lineConsequences.exists(consequence =>
      consequence.kind == LineConsequenceKind.Promotion &&
        consequence.rootMove.isEmpty &&
        consequence.source.line.contains(liabilityCause.eventLine)
    )), liabilityCause.proof)
    assert(promotionClaims.exists(_.role == "AllowsPromotion"), promotionClaims)
    assert(!promotionClaims.exists(_.role == "ProvesPromotion"), promotionClaims)
    assert(promotionClaims.exists(_.axisPolarity.contains(StrategicAxisPolarity.Loss)), promotionClaims)
    assertEquals(playedPromotion.idea.label, "allows promotion")
    assertEquals(playedPromotion.target.squares, List("g1"))
    assertEquals(playedPromotion.evidence.proofLevel, "terminal_proof")
    assert(playedPromotion.evidence.causeIds.contains(liabilityCauseRef.id), playedPromotion.evidence)

  test("a developing-looking bishop move is not labelled as lost activity without positive proof"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "6k1/5p2/6p1/b1p1P3/P1P4P/1p3NP1/5P2/1nB2K2 w - - 2 36",
          playedMoveUci = "c1b2",
          variations = List(
            VariationLine(
              List("c1b2", "b1c3", "f3d2", "c3a4", "d2b3", "a5b4", "b2c1", "a4b6", "b3d2", "b4c3", "f2f4", "c3d2", "c1d2", "b6c4", "f1e2", "c4a3"),
              scoreCp = 258,
              depth = 18
            ),
            VariationLine(
              List("g3g4", "g8f8", "c1b2", "b1c3", "f3d2", "c3a4", "d2b3", "a4b2", "b3a5", "b2d3", "e5e6", "f7f5", "g4f5", "g6f5", "a5c6", "d3f4"),
              scoreCp = 205,
              depth = 18
            ),
            VariationLine(
              List("f1g2", "a5c3", "g3g4", "g8g7", "g2f1", "b3b2", "c1b2", "c3b2", "a4a5", "b1a3", "a5a6", "a3c4", "a6a7", "c4b6"),
              scoreCp = 120,
              depth = 18
            ),
            VariationLine(
              List("e5e6", "f7e6", "c1b2", "b1c3", "f3d2", "c3a4", "d2b3", "a5b4", "b2e5", "a4b6", "e5d6", "b6c4", "d6c5", "b4c3"),
              scoreCp = 119,
              depth = 18
            )
          ),
          currentEvalCp = Some(258),
          ply = Some(70)
        )
      )
      .getOrElse(fail("expected move review"))
    val playedActivity = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(surface => EvidenceRef.sameMove(surface.moveUci, "c1b2") && surface.idea.code == "piece_activity")

    assert(!playedActivity.exists(_.idea.label == "piece activity lost"), playedActivity)

  test("a rook move that supports a future pawn break reaches the public plan"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/1p2pppp/p2p1n2/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6",
          playedMoveUci = "h1g1",
          variations = List(
            VariationLine(
              List("f2f3", "e7e5", "d4b3", "c8e6", "c1e3", "b8c6", "d1e2", "b7b5", "e1c1", "c6a5", "e2f2", "a8b8", "g2g4", "a5c4", "f1c4", "b5c4", "b3c5", "d8a5", "c5e6", "f7e6", "g4g5", "f6h5", "f2h4", "h5f4", "e3f4", "e5f4", "h4g4"),
              scoreCp = 48,
              depth = 18
            ),
            VariationLine(
              List("h2h3", "e7e5", "d4b3", "c8e6", "f2f4", "d6d5", "f4e5", "f6e4", "c3e4", "d5e4", "d1d8", "e8d8", "c1e3", "b8c6"),
              scoreCp = 44,
              depth = 18
            ),
            VariationLine(
              List("c1e3", "e7e5", "d4b3", "c8e6", "d1d2", "f8e7", "f2f3", "h7h5", "f1e2", "b8d7", "c3d5", "e6d5", "e4d5", "d8c7", "a1c1", "e8g8", "e1g1", "b7b5"),
              scoreCp = 41,
              depth = 18
            ),
            VariationLine(
              List("f1e2", "e7e5", "d4b3", "c8e6", "c1e3", "f8e7", "c3d5", "b8d7", "d1d3", "e6d5", "e4d5", "a8c8", "c2c4", "e8g8", "e1g1", "g7g6", "a1c1", "f6e8", "f2f4", "e5f4"),
              scoreCp = 40,
              depth = 18
            ),
            VariationLine(
              List("h1g1", "e7e5", "d4b3", "c8e6", "g2g4", "d6d5", "e4d5", "f6d5", "c3d5", "d8d5", "d1d5", "e6d5", "c1e3", "b8c6", "c2c3", "e8c8", "b3d2", "f8d6", "h2h4"),
              scoreCp = 33,
              depth = 18
            )
          ),
          currentEvalCp = Some(33),
          ply = Some(10),
          openingContext = Some(RawOpeningContext(name = Some("FIDE Candidates 2024"))),
          movePrefixUci = List("e2e4", "c7c5", "g1f3", "d7d6", "d2d4", "c5d4", "f3d4", "g8f6", "b1c3", "a7a6")
        )
      )
      .getOrElse(fail("expected move review"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "h1g1") &&
            payload.identity.goalTheme == PlanTheme.PawnBreakPreparation &&
            payload.preparedPawnAdvanceFiles.contains("g") =>
        payload
    }.getOrElse(fail("expected Rg1 to prepare the g-pawn advance"))
    val publicExplanation = (MoveMeaningSurface.publicPayloadJson(
      result.packet.moveJudgmentView.getOrElse(fail("expected public move view"))
    ) \ "explanations")
      .as[List[JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].exists(EvidenceRef.sameMove(_, "h1g1")))
      .getOrElse(fail("expected public explanation for 6.Rg1"))
    val publicPlan = (publicExplanation \ "plan").as[JsObject]
    val sequence = (publicPlan \ "method" \ "sequence").as[List[JsObject]]

    assert(event.goalDependencyProofReady)
    assert(event.rootEnablingDependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.PawnAdvanceSupport &&
        EvidenceRef.sameMove(dependency.to.moveUci, "g2g4")
    ), event.rootEnablingDependencies)
    assertEquals((publicPlan \ "goal" \ "theme").asOpt[String], Some("pawn break preparation"))
    assert(sequence.exists(step =>
      (step \ "move" \ "uci").asOpt[String].contains("g2g4") &&
        (step \ "connections").as[List[String]].contains("backs the pawn advance")
    ), sequence)

  test("the verified root recapturer reaches the public explanation but a capture sacrifice does not"):
    val recapture = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "2kr1bnr/ppp2ppp/2np4/q7/3PPB2/P1N1Kb2/1PP1B1PP/R2Q3R w - - 0 11",
          playedMoveUci = "e3f3",
          variations = List(
            VariationLine(
              List("e3f3", "d6d5", "c3d5", "f7f5", "c2c4", "f5e4", "f3f2", "d8d5", "b2b4", "d5d4", "b4a5", "d4d1", "h1d1", "a7a6", "g2g3", "g8f6", "f2g2", "h7h5", "a3a4"),
              scoreCp = 107,
              depth = 18
            ),
            VariationLine(
              List("e2f3", "g7g5", "f4g3", "f8g7", "d1d3", "h7h5", "b2b4", "a5b6", "e3d2", "b6d4", "a1f1", "h5h4", "g3f2", "d4c3", "d3c3", "g7c3", "d2c3", "g8f6", "f2e3", "d8g8", "c3b2", "h4h3", "g2g3", "c6e5", "e3d4", "g8e8", "f3e2"),
              scoreCp = -112,
              depth = 18
            ),
            VariationLine(
              List("g2f3", "g7g5", "f4g3", "f8g7", "d4d5", "a5b6", "e3d2", "b6b2", "c3b5", "b2a1", "d1a1", "g7a1", "h1a1", "c6e5", "b5a7", "c8b8", "a7b5", "g8e7"),
              scoreCp = -123,
              depth = 18
            ),
            VariationLine(
              List("b2b4", "f3e2", "d1e2", "a5b6", "h1d1", "g8f6", "b4b5", "c6e7", "e3f2", "d6d5", "e4e5", "f6e4", "c3e4", "d5e4", "c2c4", "e7f5", "f4g5"),
              scoreCp = -325,
              depth = 18
            )
          ),
          currentEvalCp = Some(107),
          ply = Some(20),
          movePrefixUci = List("e2e4", "e7e5", "b1c3", "b8c6", "f2f4", "e5f4", "d2d4", "d8h4", "e1e2", "d7d6", "g1f3", "c8g4", "c1f4", "e8a8", "e2e3", "h4h5", "f1e2", "h5a5", "a2a3", "g4f3")
        )
      )
      .getOrElse(fail("expected recapture move review"))
    val recaptureView = recapture.packet.moveJudgmentView.getOrElse(fail("expected recapture move view"))
    val recaptureClaim = recaptureView.moveMeaningClaims.find(claim =>
      EvidenceRef.sameMove(claim.moveUci, "e3f3") &&
        claim.publicSurfaceAdmitted &&
        claim.causeKinds.contains(RelativeCauseKind.RecaptureRecoveryWindow)
    ).getOrElse(fail("expected public root-recapture claim"))
    val recaptureSurface = MoveMeaningSurface.publicSurfaces(recaptureView).find(surface =>
      EvidenceRef.sameMove(surface.moveUci, "e3f3") &&
        surface.idea.code == "tactical_pressure" &&
        surface.idea.label == "recapture recovery"
    )

    assert(recaptureClaim.proofLineConsequences.contains(LineConsequenceKind.RecaptureSequence), recaptureClaim)
    assert(recaptureClaim.proofLineConsequences.contains(LineConsequenceKind.RecoveryWindow), recaptureClaim)
    assert(recaptureSurface.nonEmpty, MoveMeaningSurface.publicSurfaces(recaptureView))

    val sacrifice = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "r1bq1rk1/pp2nppp/1bp2n2/4N3/4P3/2PB3P/PP3PP1/RNBQ1RK1 b - - 0 11",
          playedMoveUci = "c8h3",
          variations = List(
            VariationLine(
              List("c8h3", "g2h3", "d8b8", "c1f4", "b6c7", "f4h2", "c7e5", "f2f4", "e5c7", "d1f3", "e7g6", "g1h1", "b8d8", "d3c2", "f8e8", "f1g1", "g6h4", "f3g3", "h4g6", "g3g2", "f6h5", "e4e5", "d8h4", "g2g4"),
              scoreCp = 12,
              depth = 18
            ),
            VariationLine(
              List("c8e6", "b1d2", "f6d7", "e5d7", "d8d7", "d3c2", "a8d8", "d1e2", "e7g6", "d2f3", "d7c7", "f3g5", "c7e5", "g1h1", "g6f4", "c1f4", "e5f4", "e2h5", "h7h6", "g5e6", "f7e6", "e4e5", "d8d2"),
              scoreCp = 88,
              depth = 18
            ),
            VariationLine(
              List("b6c7", "f2f4", "e7g6", "e5g6", "h7g6", "b1a3", "c7b6", "g1h2", "f6g4", "h2g3", "g4h6", "a3c4", "b6c7", "a2a4", "c8e6", "g3h2", "g6g5", "d1e2", "g5g4"),
              scoreCp = 99,
              depth = 18
            ),
            VariationLine(
              List("e7g6", "e5g6", "h7g6", "d3c2", "c8e6", "d1d8", "f8d8", "b1d2", "g6g5", "e4e5", "f6d7", "f1e1", "d7c5", "d2b3", "g5g4", "h3g4"),
              scoreCp = 108,
              depth = 18
            )
          ),
          currentEvalCp = Some(12),
          ply = Some(21),
          movePrefixUci = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "g8f6", "d2d3", "f8c5", "c2c3", "e8h8", "e1h1", "d7d6", "h2h3", "c6e7", "d3d4", "c7c6", "b5d3", "c5b6", "d4e5", "d6e5", "f3e5")
        )
      )
      .getOrElse(fail("expected capture-sacrifice move review"))
    val sacrificeView = sacrifice.packet.moveJudgmentView.getOrElse(fail("expected capture-sacrifice move view"))
    val sacrificeLine = sacrifice.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(line => line.role == LineNodeRole.Played && EvidenceRef.sameMove(line.rootMove, "c8h3")) =>
        payload
    }.getOrElse(fail("expected capture-sacrifice line facts"))

    assert(!sacrificeLine.rootIsRecapture("c8h3"), sacrificeLine)
    assert(sacrificeLine.rootIsCaptureSacrifice("c8h3"), sacrificeLine)
    assert(sacrificeLine.hasProofSignalConsequence(LineConsequenceKind.Sacrifice), sacrificeLine)
    assert(sacrificeView.moveMeaningClaims.exists(claim =>
      EvidenceRef.sameMove(claim.moveUci, "c8h3") &&
        claim.publicSurfaceAdmitted &&
        claim.causeKinds.contains(RelativeCauseKind.SacrificeCompensation)
    ), sacrificeView.moveMeaningClaims)
    assert(!sacrificeView.moveMeaningClaims.exists(claim =>
      EvidenceRef.sameMove(claim.moveUci, "c8h3") &&
        claim.publicSurfaceAdmitted &&
        claim.causeKinds.contains(RelativeCauseKind.RecaptureRecoveryWindow)
    ), sacrificeView.moveMeaningClaims)
    assert(!MoveMeaningSurface.publicSurfaces(sacrificeView).exists(_.idea.label == "recapture recovery"))
