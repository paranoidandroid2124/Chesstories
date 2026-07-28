package lila.chessjudgment.model.judgment

import chess.White

class PlayerFacingCauseSelectionContractTest extends munit.FunSuite:

  private val position = PositionNodeRef(
    "1r2k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val line = LineNodeRef("reference", "e2b5", 1, LineNodeRole.BestReference)
  private val played = LineNodeRef("played", "e2e3", 9, LineNodeRole.Played)

  test("one selected Cause keeps every compatible causal channel in one selection"):
    val first = channel("first-carrier", "e8")
    val second = channel("second-carrier", "h4")
    val selection = PlayerFacingCauseSelectionPolicy.build(
      causeEvidence = ref("cause", EvidenceLayer.RelativeCause),
      status = CrossComparisonExposureStatus.SelectedComplementary,
      effectMode = PlayerFacingCauseEffectMode.AlternativeResource,
      channels = List(second, first),
      onlyMoveQualifier = None
    )

    assert(selection.nonEmpty)
    assertEquals(selection.toList.flatMap(_.channels).size, 2)
    assertEquals(
      selection.toList.flatMap(_.channels).map(_.playedChange).distinct,
      List(PlayerFacingCausalChange.Missed)
    )
    assertEquals(
      selection.toList.flatMap(_.channels).map(_.causalSignature),
      List(first.causalSignature, second.causalSignature).sorted
    )

  test("representation-equivalent carriers collapse inside the Cause without losing another channel"):
    val first = channel("z-carrier", "e8")
    val duplicate = first.copy(binding = first.binding.copy(source = ref("a-carrier", EvidenceLayer.MoveMotif)))
    val independent = channel("independent-carrier", "h4")
    val selection = PlayerFacingCauseSelectionPolicy.build(
      causeEvidence = ref("cause", EvidenceLayer.RelativeCause),
      status = CrossComparisonExposureStatus.SelectedPrimary,
      effectMode = PlayerFacingCauseEffectMode.PlayedValue,
      channels = List(first, duplicate, independent),
      onlyMoveQualifier = None
    ).getOrElse(fail("expected one selected Cause"))

    assertEquals(selection.channels.size, 2)
    assertEquals(
      selection.channels.find(_.causalSignature == first.causalSignature).map(_.carrierEvidence.id),
      Some("a-carrier")
    )
    assert(selection.channels.exists(_.causalSignature == independent.causalSignature))

  test("independent Causes tied by owned evidence share comparison exposure rank but keep stable serialization order"):
    val comparisonRef = ref("comparison", EvidenceLayer.CandidateComparison, Some(played))
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      line,
      played,
      EvalComparison(White, -12.0, MoveChoiceVerdict.Blunder)
    )
    val relationProof = zwischenzugRecord("zwischenzug-proof", line)
    val moveOrder = causeRecord(
      "move-order-cause",
      RelativeCauseKind.WrongMoveOrder,
      comparisonRef,
      relationProof.ref
    )
    val recapturer = causeRecord(
      "recapturer-cause",
      RelativeCauseKind.WrongRecapturer,
      comparisonRef,
      relationProof.ref
    )
    val graph = graphWithAdmittedCauses(
      nonCauseRecords = List(
        EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
        relationProof
      ),
      causeRecords = List(moveOrder, recapturer)
    )
    val claim = JudgmentClaim(
      "shared-host",
      ClaimFamily.Tactical,
      ClaimSubject.ReferenceMove,
      position,
      Some(line),
      Some(line.rootMove),
      List(moveOrder.ref, recapturer.ref),
      EvidenceScope.BestLine,
      EvidenceConfidence.EngineBacked
    )
    val exposure = PlayerFacingCauseExposurePipeline
      .resolve(List(claim), graph, Set(played.rootMove))
    val selections = exposure.selections

    assertEquals(selections.map(_.causeEvidence.id).toSet, Set(moveOrder.ref.id, recapturer.ref.id))
    assertEquals(selections.map(_.comparisonExposureRank).distinct.size, 1)
    assertEquals(selections.map(_.selectionOrder).distinct.size, 2)
    assertEquals(selections.map(_.causeEvidence.id), List(moveOrder.ref.id, recapturer.ref.id).sorted)
    assertEquals(
      exposure.importanceResolution.selectedCauseEvidenceIds,
      selections.map(_.causeEvidence.id)
    )
    assertEquals(
      exposure.importanceResolution.frontierCauseEvidenceIds,
      selections.map(_.causeEvidence.id)
    )
    assertEquals(exposure.importanceResolution.dominatedWithinDomainCauseEvidenceIds, Nil)
    assertEquals(exposure.importanceResolution.uniqueTopCauseEvidenceId, None)

  test("serialization follows comparison exposure authority before stable Cause id"):
    val lowerLine = LineNodeRef("lower-reference", "e2e7", 2, LineNodeRole.Alternative)
    val primaryComparisonRef = ref("primary-comparison", EvidenceLayer.CandidateComparison, Some(played))
    val secondaryComparisonRef = ref("secondary-comparison", EvidenceLayer.CandidateComparison, Some(played))
    val primaryComparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      line,
      played,
      EvalComparison(White, -12.0, MoveChoiceVerdict.Blunder)
    )
    val secondaryComparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsAlternative,
      lowerLine,
      played,
      EvalComparison(White, -8.0, MoveChoiceVerdict.Inaccuracy)
    )
    val primaryProof = zwischenzugRecord("z-primary-proof", line)
    val secondaryProof = zwischenzugRecord(
      "a-secondary-proof",
      lowerLine,
      expectedRecaptureSquare = "c4"
    )
    val primary = causeRecord(
      "z-primary-cause",
      RelativeCauseKind.WrongMoveOrder,
      primaryComparisonRef,
      primaryProof.ref,
      line
    )
    val secondary = causeRecord(
      "a-secondary-cause",
      RelativeCauseKind.WrongMoveOrder,
      secondaryComparisonRef,
      secondaryProof.ref,
      lowerLine
    )
    val graph = graphWithAdmittedCauses(
      nonCauseRecords = List(
        EvidenceRecord(primaryComparisonRef, CandidateComparisonEvidence(primaryComparison)),
        EvidenceRecord(secondaryComparisonRef, CandidateComparisonEvidence(secondaryComparison)),
        primaryProof,
        secondaryProof
      ),
      causeRecords = List(primary, secondary)
    )
    val claim = JudgmentClaim(
      "ordered-host",
      ClaimFamily.Tactical,
      ClaimSubject.ReferenceMove,
      position,
      Some(line),
      Some(line.rootMove),
      List(primary.ref, secondary.ref),
      EvidenceScope.BestLine,
      EvidenceConfidence.EngineBacked
    )
    val selections = PlayerFacingCauseExposurePipeline
      .resolve(List(claim), graph, Set(played.rootMove))
      .selections

    assertEquals(
      selections.map(selection =>
        (selection.causeEvidence.id, selection.comparisonExposureRank, selection.selectionOrder)
      ),
      List(
        (primary.ref.id, 0, 0),
        (secondary.ref.id, 1, 1)
      )
    )

  private def channel(id: String, target: String): DirectCauseChannel =
    DirectCauseChannel(
      EvidenceObjectBinding(
        source = ref(id, EvidenceLayer.MoveMotif),
        actor = List(
          ConcreteChessObject(EvidenceObjectKind.Move, line.rootMove),
          ConcreteChessObject(EvidenceObjectKind.Side, "white"),
          ConcreteChessObject(EvidenceObjectKind.Piece, "queen"),
          ConcreteChessObject(EvidenceObjectKind.Square, "e2"),
          ConcreteChessObject(EvidenceObjectKind.Square, "b5")
        ),
        target = List(ConcreteChessObject(EvidenceObjectKind.Square, target)),
        mechanism = List(ConcreteChessObject(EvidenceObjectKind.Mechanism, "check")),
        consequence = List(ConcreteChessObject(EvidenceObjectKind.Consequence, "forcing-resource")),
        line = Some(line),
        proofRole = Some(RelativeCauseProofRole.DirectProof)
      ),
      DirectCausalChange.Occurred
    )

  private def causeRecord(
      id: String,
      kind: RelativeCauseKind,
      comparisonRef: EvidenceRef,
      proofRef: EvidenceRef,
      eventLine: LineNodeRef = line
  ): EvidenceRecord =
    val cause = RelativeCauseFact(
      kind,
      comparisonRef,
      List(proofRef),
      RelativeCauseSourceSide.Reference,
      CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(proofRef)
        )
      ))
    )
    EvidenceRecord(
      ref(id, EvidenceLayer.RelativeCause, Some(eventLine)),
      RelativeCauseFactEvidence(cause)
    )

  private def zwischenzugRecord(
      id: String,
      eventLine: LineNodeRef,
      expectedRecaptureSquare: String = "d5"
  ): EvidenceRecord =
    val detail = RelationWitnessDetail.Zwischenzug(
      intermediateMove = eventLine.rootMove,
      expectedRecaptureSquare = EvidenceSquare(expectedRecaptureSquare),
      checkingPieceSquare = EvidenceSquare(eventLine.rootMove.slice(2, 4)),
      checkingPieceRole = EvidencePieceRole("queen"),
      checkedKingSquare = EvidenceSquare("e8"),
      threatType = RelationThreatSignal.Check
    )
    EvidenceRecord(
      ref(id, EvidenceLayer.Relation, Some(eventLine)),
      RelationFactEvidence
        .from(detail, List(eventLine.rootMove))
        .getOrElse(fail("expected a typed zwischenzug relation"))
    )

  private def ref(
      id: String,
      layer: EvidenceLayer,
      boundLine: Option[LineNodeRef] = Some(line)
  ): EvidenceRef =
    EvidenceRef(
      id,
      layer match
        case EvidenceLayer.RelativeCause | EvidenceLayer.CandidateComparison =>
          EvidenceProducer.RelativeMoveProducer
        case EvidenceLayer.Relation =>
          EvidenceProducer.TacticalRelationProducer
        case _ =>
          EvidenceProducer.MoveMotifProducer,
      layer,
      position,
      boundLine,
      boundLine.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      if layer == EvidenceLayer.RelativeCause || layer == EvidenceLayer.CandidateComparison then
        EvidenceConfidence.EngineBacked
      else EvidenceConfidence.LegalReplayVerified
    )

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    records.foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))

  private def graphWithAdmittedCauses(
      nonCauseRecords: List[EvidenceRecord],
      causeRecords: List[EvidenceRecord]
  ): TypedEvidenceGraph =
    val provisional = graphOf((nonCauseRecords ++ causeRecords)*)
    val admitted = causeRecords.map { record =>
      val cause = record.payload match
        case RelativeCauseFactEvidence(value) => value
        case _ => fail(s"expected a RelativeCause fixture at '${record.ref.id}'")
      val signatures = EvidenceObjectBinding
        .rawDirectSentenceChannelsForProjection(cause, provisional)
        .map(_.causalSignature)
        .toSet
      assert(signatures.nonEmpty, s"expected raw direct channels for '${record.ref.id}'")
      record.copy(payload = RelativeCauseFactEvidence(cause.copy(
        directEffectAdmission = DirectEffectAdmission.Restricted(signatures)
      )))
    }
    graphOf((nonCauseRecords ++ admitted)*)
