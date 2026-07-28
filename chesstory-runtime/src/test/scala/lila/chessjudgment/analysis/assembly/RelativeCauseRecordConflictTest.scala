package lila.chessjudgment.analysis.assembly

import chess.White
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

class RelativeCauseRecordConflictTest extends munit.FunSuite:

  private val fen = "4k3/7b/8/8/1r2r2r/8/4QNB1/K7 w - - 0 1"
  private val position = PositionNodeRef(fen, 0, Some(White))
  private val reference = LineNodeRef("reference", "e2e4", 1, LineNodeRole.BestReference)
  private val played = LineNodeRef("played", "a1a2", 9, LineNodeRole.Played)
  private val comparisonRef = ref(
    "comparison",
    EvidenceProducer.RelativeMoveProducer,
    EvidenceLayer.CandidateComparison,
    Some(played),
    EvidenceConfidence.EngineBacked
  )
  private val comparison = CandidateComparisonFact(
    CandidateComparisonKind.PlayedVsBest,
    reference,
    played,
    EvalComparison.fromLines(
      White,
      CandidateLineNode(
        reference,
        EngineLine(List(reference.rootMove), scoreCp = 900, depth = 20),
        ref(
          "reference-eval",
          EvidenceProducer.EngineEvalProducer,
          EvidenceLayer.Eval,
          Some(reference),
          EvidenceConfidence.EngineBacked
        )
      ),
      CandidateLineNode(
        played,
        EngineLine(List(played.rootMove), scoreCp = -900, depth = 20),
        ref(
          "played-eval",
          EvidenceProducer.EngineEvalProducer,
          EvidenceLayer.Eval,
          Some(played),
          EvidenceConfidence.EngineBacked
        )
      )
    )
  )

  test("mate completeness failure does not poison material or qualitative families"):
    val material = ComparisonEndpointEffectInventory.Complete(Set.empty)
    val qualitative = ComparisonEndpointEffectInventory.Complete(Set.empty)
    val inventories = ComparisonEndpointLineEffectInventories(
      material = material,
      mate = ComparisonEndpointEffectInventory.Complete(Set.empty),
      qualitative = qualitative
    )
    val noMateLine = LineFactEvidence(
      line = reference,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = Nil,
        netCaptureCpForMover = 0,
        maxGainCpForMover = 0,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = Nil
    )
    val engineClaimsMate = EvalFactEvidence(reference, 900, mate = Some(1), depth = 20)

    val isolated = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      noMateLine,
      Some(engineClaimsMate)
    )

    assertEquals(isolated.material, material)
    assertEquals(isolated.qualitative, qualitative)
    assertEquals(isolated.mate, ComparisonEndpointEffectInventory.Incomplete)

  test("material completeness failure does not poison mate or qualitative families"):
    val mate = ComparisonEndpointEffectInventory.Complete(Set.empty)
    val qualitative = ComparisonEndpointEffectInventory.Complete(Set.empty)
    val inventories = ComparisonEndpointLineEffectInventories(
      material = ComparisonEndpointEffectInventory.Complete(Set.empty),
      mate = mate,
      qualitative = qualitative
    )
    val missingMaterial = LineFactEvidence(line = reference, replay = Nil)
    val exactNoMate = EvalFactEvidence(reference, 0, mate = None, depth = 20)

    val isolated = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      missingMaterial,
      Some(exactNoMate)
    )

    assertEquals(isolated.material, ComparisonEndpointEffectInventory.Incomplete)
    assertEquals(isolated.mate, mate)
    assertEquals(isolated.qualitative, qualitative)

  test("qualitative incompleteness does not poison material or mate families"):
    val material = ComparisonEndpointEffectInventory.Complete(Set.empty)
    val mate = ComparisonEndpointEffectInventory.Complete(Set.empty)
    val inventories = ComparisonEndpointLineEffectInventories(
      material = material,
      mate = mate,
      qualitative = ComparisonEndpointEffectInventory.Incomplete
    )
    val completeMaterial = LineFactEvidence(
      line = reference,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = Nil,
        netCaptureCpForMover = 0,
        maxGainCpForMover = 0,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = Nil
    )
    val exactNoMate = EvalFactEvidence(reference, 0, mate = None, depth = 20)

    val isolated = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      completeMaterial,
      Some(exactNoMate)
    )

    assertEquals(isolated.material, material)
    assertEquals(isolated.mate, mate)
    assertEquals(isolated.qualitative, ComparisonEndpointEffectInventory.Incomplete)

  test("semantic duplicate carriers canonicalize while conflicting payloads fail closed"):
    val exact = EvalFactEvidence(reference, whitePovEvalCp = 120, mate = None, depth = 20)
    val duplicate = RelativeAssessmentAssembler.canonicalSemanticCarrier(List(
      ref(
        "z-duplicate",
        EvidenceProducer.EngineEvalProducer,
        EvidenceLayer.Eval,
        Some(reference),
        EvidenceConfidence.EngineBacked
      ) -> exact,
      ref(
        "a-canonical",
        EvidenceProducer.EngineEvalProducer,
        EvidenceLayer.Eval,
        Some(reference),
        EvidenceConfidence.EngineBacked
      ) -> exact
    ))

    assertEquals(duplicate.map(_._1.id), Some("a-canonical"))
    assertEquals(
      RelativeAssessmentAssembler.canonicalSemanticCarrier(List(
        ref(
          "first",
          EvidenceProducer.EngineEvalProducer,
          EvidenceLayer.Eval,
          Some(reference),
          EvidenceConfidence.EngineBacked
        ) -> exact,
        ref(
          "conflict",
          EvidenceProducer.EngineEvalProducer,
          EvidenceLayer.Eval,
          Some(reference),
          EvidenceConfidence.EngineBacked
        ) -> exact.copy(whitePovEvalCp = 121)
      )),
      None
    )

  test("exact endpoint carriers must belong to the comparison root occurrence"):
    val sameBoardDifferentClock = position.copy(
      fen = position.fen.replace("0 1", "7 9")
    )
    val repeatedAtAnotherPly = position.copy(ply = position.ply + 2)
    val wrongBoard = position.copy(fen = "7k/8/8/8/8/8/8/7K w - - 0 1")
    val base = ref(
      "root-carrier",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(reference),
      EvidenceConfidence.EngineBacked
    )

    assert(RelativeAssessmentAssembler.carrierAtComparisonRoot(base, sameBoardDifferentClock))
    assert(!RelativeAssessmentAssembler.carrierAtComparisonRoot(
      base.copy(position = repeatedAtAnotherPly),
      position
    ))
    assert(!RelativeAssessmentAssembler.carrierAtComparisonRoot(
      base.copy(position = wrongBoard),
      position
    ))

  test("mate completeness requires engine winner polarity to match the owned beneficiary"):
    val mateFen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val matePosition = PositionNodeRef(mateFen, 0, Some(White))
    val mateLine = LineNodeRef("mate-polarity", "g6g7", 1, LineNodeRole.BestReference)
    val mateAfter = PrincipalVariationEvidence
      .legalFenAfter(mateFen, mateLine.rootMove)
      .getOrElse(fail("expected legal mate"))
    val payload = LineFactEvidence(
      line = mateLine,
      replay = List(LineReplayStep(0, mateLine.rootMove, mateFen, mateAfter)),
      events = List(LineMoveEvent(
        kind = LineEventKind.Mate,
        moveUci = mateLine.rootMove,
        plyOffset = 0,
        side = Some(White),
        pieceRole = Some(EvidencePieceRole("queen")),
        targetRole = Some(EvidencePieceRole("king")),
        square = Some(EvidenceSquare("h8"))
      )),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Mate,
        lineMoves = List(mateLine.rootMove),
        proofSignal = true,
        eventMove = Some(mateLine.rootMove),
        rootMove = Some(mateLine.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    val source = EvidenceRef(
      "mate-polarity-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      matePosition,
      Some(mateLine),
      EvidenceScope.BestLine,
      EvidenceConfidence.LegalReplayVerified
    )
    val inventories = EvidenceObjectBinding.comparisonEndpointLineObservations(
      source,
      payload,
      matePosition
    )

    assertEquals(inventories.qualitative, ComparisonEndpointEffectInventory.Complete(Set.empty))

    val correct = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      payload,
      Some(EvalFactEvidence(mateLine, 900, mate = Some(1), depth = 20))
    )
    val reversed = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      payload,
      Some(EvalFactEvidence(mateLine, 900, mate = Some(-1), depth = 20))
    )

    assert(correct.mate.isInstanceOf[ComparisonEndpointEffectInventory.Complete])
    assertEquals(reversed.mate, ComparisonEndpointEffectInventory.Incomplete)

  test("C merges overlap components before carrier choice and preserves descriptor conflict"):
    val weakRoot = materialLine("z-weak-root", List("e2e4"), 100)
    val strongRoot = materialLine("a-strong-root", List("e2e4"), 500)
    val forcedResponse = materialLine("forced-response", List("e2e4", "b4e4", "g2e4"), 100)
    val longResponse = materialLine(
      "long-response",
      List("e2e4", "b4e4", "g2e4", "h7e4", "f2e4"),
      100
    )
    val graph = graphOf(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      weakRoot,
      strongRoot,
      forcedResponse,
      longResponse
    )
    val first = admit(causeRecord("z-first-carrier", List(weakRoot.ref), List(weakRoot.ref)), graph)
    val superset = admit(
      causeRecord(
        "a-more-support-is-not-authority",
        List(strongRoot.ref, forcedResponse.ref),
        List(strongRoot.ref, forcedResponse.ref)
      ),
      graph
    )
    val disjoint = admit(causeRecord("m-disjoint", List(longResponse.ref), List(longResponse.ref)), graph)

    val canonical = RelativeAssessmentAssembler.canonicalizeRelativeCauseRecords(
      List(first, superset, disjoint),
      graph
    )

    assertEquals(canonical.map(_.ref.id), List("z-first-carrier", "m-disjoint"))
    val mergedCause = causeOf(canonical.head)
    assertEquals(
      mergedCause.supportEvidence.map(_.id),
      List("z-weak-root", "a-strong-root", "forced-response")
    )
    assertEquals(canonical.head.parents.map(_.id), List("z-weak-root", "a-strong-root", "forced-response"))
    val rawChannels = EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(mergedCause, graph)
    val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(mergedCause, graph)
    assertEquals(rawChannels.size, 2)
    assertEquals(rawChannels.count(_.importanceDescriptorAmbiguous), 1)
    assertEquals(channels.size, 1)
    assert(channels.forall(channel => !channel.importanceDescriptorAmbiguous))
    assertEquals(causeOf(canonical(1)).supportEvidence.map(_.id), List("long-response"))

  test("C keeps proof-segment disagreement on one exact channel as null"):
    val firstLine = materialLine("segment-b4", List("e2e4", "b4e4", "g2e4"), 100)
    val secondLine = materialLine("segment-h4", List("e2e4", "h4e4", "g2e4"), 100)
    val graph = graphOf(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      firstLine,
      secondLine
    )
    val first = admit(causeRecord("segment-first", List(firstLine.ref), List(firstLine.ref)), graph)
    val second = admit(causeRecord("segment-second", List(secondLine.ref), List(secondLine.ref)), graph)

    val canonical = RelativeAssessmentAssembler.canonicalizeRelativeCauseRecords(
      List(first, second),
      graph
    )

    assertEquals(canonical.size, 1)
    val mergedCause = causeOf(canonical.head)
    assertEquals(mergedCause.supportEvidence.map(_.id), List("segment-b4", "segment-h4"))
    val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(mergedCause, graph)
    assertEquals(channels.size, 1)
    assert(!channels.head.importanceDescriptorAmbiguous)
    assert(channels.head.proofSegmentAmbiguous)
    assertEquals(channels.head.proofSegment, None)
    assert(RelativeCauseConstructionAdmission.initiallyReady(mergedCause, graph))

  test("descriptor-only conflict loses public admission while segment conflict does not"):
    val weak = materialLine("isolated-weak", List("e2e4"), 100)
    val strong = materialLine("isolated-strong", List("e2e4"), 500)
    val graph = graphOf(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      weak,
      strong
    )
    val canonical = RelativeAssessmentAssembler.canonicalizeRelativeCauseRecords(
      List(
        admit(causeRecord("isolated-a", List(weak.ref), List(weak.ref)), graph),
        admit(causeRecord("isolated-b", List(strong.ref), List(strong.ref)), graph)
      ),
      graph
    )
    assertEquals(canonical.size, 1)
    val merged = causeOf(canonical.head)
    val rawChannels = EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(merged, graph)
    assertEquals(rawChannels.size, 1)
    assertEquals(
      rawChannels.head.rootOwnedEffectDescriptor.map(_.magnitude),
      Some(DirectEffectMagnitudeKnowledge.Ambiguous)
    )
    assertEquals(
      rawChannels.head.rootOwnedEffectDescriptor.flatMap(_.materialEventSalience),
      None
    )
    assertEquals(RelativeCauseConstructionAdmission.admittedDirectChannels(merged, graph), Nil)
    assert(!RelativeCauseConstructionAdmission.initiallyReady(merged, graph))

  private def causeRecord(
      id: String,
      directRefs: List[EvidenceRef],
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.MaterialSwing,
      comparisonEvidence = comparisonRef,
      supportEvidence = directRefs,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          directRefs
        )
      ))
    )
    EvidenceRecord(
      ref(
        id,
        EvidenceProducer.RelativeMoveProducer,
        EvidenceLayer.RelativeCause,
        Some(reference),
        EvidenceConfidence.EngineBacked
      ),
      RelativeCauseFactEvidence(cause),
      parents
    )

  private def admit(record: EvidenceRecord, graph: TypedEvidenceGraph): EvidenceRecord =
    val cause = causeOf(record)
    val signatures = EvidenceObjectBinding
      .rawDirectSentenceChannelsForProjection(cause, graph)
      .map(_.causalSignature)
      .toSet
    record.copy(
      payload = RelativeCauseFactEvidence(
        cause.copy(directEffectAdmission = DirectEffectAdmission.Restricted(signatures))
      )
    )

  private def materialLine(
      id: String,
      moves: List[String],
      durableNetCp: Int
  ): EvidenceRecord =
    val replay = legalReplay(fen, moves)
    val eventMove = moves.last
    val eventOffset = moves.size - 1
    val captures = moves.zipWithIndex.map { case (move, plyOffset) =>
      val (side, attacker, captured, defaultValue) = move match
        case "e2e4" => (White, "queen", "rook", 500)
        case "b4e4" | "h4e4" => (!White, "rook", "queen", 900)
        case "g2e4" => (White, "bishop", "rook", 500)
        case "h7e4" => (!White, "bishop", "bishop", 300)
        case "f2e4" => (White, "knight", "bishop", 300)
        case other => fail(s"unexpected material-sequence move $other")
      LineMaterialCapture(
        moveUci = move,
        plyOffset = plyOffset,
        side = side,
        attackerRole = EvidencePieceRole(attacker),
        capturedRole = EvidencePieceRole(captured),
        square = EvidenceSquare(move.slice(2, 4)),
        valueCp = if moves.size == 1 then durableNetCp else defaultValue,
        recapture = plyOffset > 0
      )
    }
    val eventCapture = captures.last
    val consequence = LineConsequence(
      kind = LineConsequenceKind.MaterialGain,
      lineMoves = moves,
      proofSignal = true,
      eventMove = Some(eventMove),
      rootMove = Some(reference.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = eventCapture.moveUci,
          plyOffset = eventCapture.plyOffset,
          capturedRole = eventCapture.capturedRole,
          square = eventCapture.square,
          targetValueCp = eventCapture.valueCp
        ),
        beneficiary = White,
        durableNetCp = durableNetCp
      ))
    )
    EvidenceRecord(
      ref(
        id,
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        Some(reference),
        EvidenceConfidence.LegalReplayVerified
      ),
      LineFactEvidence(
        line = reference,
        material = Some(LineMaterialSummary(
          sideToMove = White,
          captures = captures,
          netCaptureCpForMover = durableNetCp,
          maxGainCpForMover = captures.filter(_.side == White).map(_.valueCp).sum,
          maxLossCpForMover = captures.filter(_.side != White).map(_.valueCp).sum,
          hasRecaptureChain = captures.size > 1,
          hasRecoveryWindow = false,
          promotionGainCpForMover = 0,
          materialWindowComplete = true
        )),
        replay = replay,
        consequences = List(consequence)
      )
    )

  private def legalReplay(startFen: String, moves: List[String]): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(startFen -> List.empty[LineReplayStep]) {
      case ((before, steps), (move, index)) =>
        val after = PrincipalVariationEvidence
          .legalFenAfter(before, move)
          .getOrElse(fail(s"expected legal test move $move after $before"))
        after -> (steps :+ LineReplayStep(index, move, before, after))
    }._2

  private def causeOf(record: EvidenceRecord): RelativeCauseFact =
    record.payload match
      case RelativeCauseFactEvidence(cause) => cause
      case other                            => fail(s"expected a Cause record, got $other")

  private def ref(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      line: Option[LineNodeRef],
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(
      id,
      producer,
      layer,
      position,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      confidence
    )

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    records.foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
