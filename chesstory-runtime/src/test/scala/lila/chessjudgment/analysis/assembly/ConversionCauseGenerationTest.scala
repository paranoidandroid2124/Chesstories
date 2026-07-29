package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

class ConversionCauseGenerationTest extends munit.FunSuite:

  test("reference conversion and promotion resources produce resource-owned ConversionSecured drafts"):
    val promotionFen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
    val position = PositionNodeRef(promotionFen, 0, Some(White))
    val reference = LineNodeRef("promotion-reference", "a7a8q", 1, LineNodeRole.BestReference)
    val candidate = LineNodeRef("promotion-played", "e1f1", 2, LineNodeRole.Played)
    val promotion = lineRecord(
      "reference-promotion",
      position,
      reference,
      legalReplay(promotionFen, List(reference.rootMove)),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Promotion,
        lineMoves = List(reference.rootMove),
        proofSignal = true,
        eventMove = Some(reference.rootMove),
        rootMove = Some(reference.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    val fact = comparison(position, reference, candidate)
    val profile = RelativeCauseSignalProfile.from(
      fact = fact,
      referenceRecords = List(promotion),
      candidateRecords = Nil,
      sharedRecords = Nil
    )
    val comparisonRecord =
      ExplicitCauseAdmissionTestSupport.comparisonRecord("promotion-comparison", position, fact)
    val drafts = RelativeCauseDraftPlanner.drafts(profile, comparisonRecord)
    val referenceConversions = drafts.filter(draft =>
      draft.kind == RelativeCauseKind.ConversionSecured &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Reference)
    )

    assert(referenceConversions.exists(draft =>
      draft.attributionKind == CauseAttributionKind.ReferenceCreatesResource &&
        draft.support.map(_.ref.id) == List(promotion.ref.id)
    ))
    assert(!drafts.exists(draft =>
      draft.kind == RelativeCauseKind.ConversionMiss &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Reference)
    ))

  test("completed winning conversion requires an exact root, actor side, replay span, and concrete squares"):
    val fen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"
    val position = PositionNodeRef(fen, 0, Some(White))
    val line = LineNodeRef("completed-conversion", "d5d4", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List("d5d4", "d7d8", "d4e5", "d8d7", "e5d5"))
    val completed = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = Some("Lucena"),
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Completed,
      triggerMove = Some(line.rootMove),
      requiredSquares = List("d4", "d5", "e5")
    )
    val exact = completedRecord(
      "exact-completed-winning",
      position,
      line,
      replay,
      completed
    )
    val wrongSide = completedRecord("wrong-side", position, line, replay, completed.copy(techniqueSide = Black))
    val noSquares = completedRecord("no-required-squares", position, line, replay, completed.copy(requiredSquares = Nil))
    val wrongRoot = completedRecord("wrong-root", position, line, replay, completed.copy(triggerMove = Some("d5e5")))
    val incompleteReplay = completedRecord("incomplete-replay", position, line, replay.take(1), completed)
    val selected = RelativeCauseSignalProfile
      .conversionWindowRecords(
        List(exact, wrongSide, noSquares, wrongRoot, incompleteReplay),
        line,
        White
      )
      .map(_.ref.id)
      .toSet

    assertEquals(selected, Set(exact.ref.id))

  test("completed triangulation requires its verified zugzwang proof"):
    val fen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"
    val position = PositionNodeRef(fen, 0, Some(White))
    val line = LineNodeRef("triangulation", "d5d4", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List("d5d4", "d7d8", "d4e5", "d8d7", "e5d5"))
    val horizon = LineEndgameTechniqueHorizon(
      pattern = "Triangulation",
      rookPattern = None,
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Completed,
      triggerMove = Some(line.rootMove),
      requiredSquares = List("d4", "d5", "e5")
    )
    val proof = EndgameZugzwangProof
      .verified(
        constrainedSide = Black,
        terminalFen = replay.last.fenAfter,
        comparisonFen = replay.head.fenBefore,
        terminalDtm = -22,
        comparisonDtm = 27,
        legalReplies = List(
          EndgameZugzwangReplyProof("d7e7", 21),
          EndgameZugzwangReplyProof("d7e8", 19),
          EndgameZugzwangReplyProof("d7c8", 17),
          EndgameZugzwangReplyProof("d7d8", 15)
        ),
        horizon = horizon,
        replay = replay
      )
      .getOrElse(fail("expected a verified triangulation proof"))
    val unverified = completedRecord("unverified-triangulation", position, line, replay, horizon)
    val verified = lineRecord(
      "verified-triangulation",
      position,
      line,
      replay,
      horizons = List(horizon)
    ).copy(
      payload = LineFactEvidence(line = line, replay = replay, endgameHorizons = List(horizon))
        .withEndgameZugzwangProof(0, 4, proof)
    )

    assertEquals(
      RelativeCauseSignalProfile
        .conversionWindowRecords(List(unverified, verified), line, White)
        .map(_.ref.id),
      List(verified.ref.id)
    )

  test("Cause-neutral draft planning preserves candidate-owned conversion miss and secured"):
    val endgameFen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"
    val endgamePosition = PositionNodeRef(endgameFen, 0, Some(White))
    val missedLine = LineNodeRef("missed-played", "d5d4", 2, LineNodeRole.Played)
    val missedReference = LineNodeRef("missed-reference", "d5e5", 1, LineNodeRole.BestReference)
    val missedReplay = legalReplay(endgameFen, List("d5d4", "d7d8", "d4e5"))
    val failed = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = Some("Lucena"),
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 2,
      status = LineEndgameTechniqueHorizonStatus.Failed,
      triggerMove = Some(missedLine.rootMove),
      requiredSquares = List("d4", "e5")
    )
    val missedRecord = lineRecord(
      "candidate-conversion-miss",
      endgamePosition,
      missedLine,
      missedReplay,
      horizons = List(failed)
    )
    val missedFact = comparison(endgamePosition, missedReference, missedLine)
    val missedComparisonRecord = ExplicitCauseAdmissionTestSupport.comparisonRecord(
      "candidate-miss-comparison",
      endgamePosition,
      missedFact
    )
    val missedDrafts = RelativeCauseDraftPlanner.drafts(
      RelativeCauseSignalProfile.from(missedFact, Nil, List(missedRecord), Nil),
      missedComparisonRecord
    )
    val missedDraft = missedDrafts.find(draft =>
      draft.kind == RelativeCauseKind.ConversionMiss &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Candidate)
    ).getOrElse(fail("expected candidate ConversionMiss"))
    assertCandidateNeutralWitness(
      missedDraft,
      missedFact,
      missedComparisonRecord,
      missedRecord,
      endgamePosition
    )

    val promotionFen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
    val promotionPosition = PositionNodeRef(promotionFen, 0, Some(White))
    val securedLine = LineNodeRef("secured-played", "a7a8q", 1, LineNodeRole.Played)
    val securedReference = LineNodeRef("secured-reference", "e1f1", 2, LineNodeRole.BestReference)
    val securedRecord = lineRecord(
      "candidate-conversion-secured",
      promotionPosition,
      securedLine,
      legalReplay(promotionFen, List(securedLine.rootMove)),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Promotion,
        lineMoves = List(securedLine.rootMove),
        proofSignal = true,
        eventMove = Some(securedLine.rootMove),
        rootMove = Some(securedLine.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    val securedFact = comparison(
      promotionPosition,
      securedReference,
      securedLine,
      referenceScore = 0,
      candidateScore = 900
    )
    val securedComparisonRecord = ExplicitCauseAdmissionTestSupport.comparisonRecord(
      "candidate-secured-comparison",
      promotionPosition,
      securedFact
    )
    val securedDrafts = RelativeCauseDraftPlanner.drafts(
      RelativeCauseSignalProfile.from(securedFact, Nil, List(securedRecord), Nil),
      securedComparisonRecord
    )
    val securedDraft = securedDrafts.find(draft =>
      draft.kind == RelativeCauseKind.ConversionSecured &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Candidate)
    ).getOrElse(fail("expected candidate ConversionSecured"))
    assertCandidateNeutralWitness(
      securedDraft,
      securedFact,
      securedComparisonRecord,
      securedRecord,
      promotionPosition
    )

  private def assertCandidateNeutralWitness(
      draft: RelativeCauseDraft,
      fact: CandidateComparisonFact,
      comparisonRecord: EvidenceRecord,
      support: EvidenceRecord,
      position: PositionNodeRef
  ): Unit =
    val cause = RelativeCauseFact(
      kind = draft.kind,
      comparisonEvidence = comparisonRecord.ref,
      supportEvidence = List(support.ref),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(
        draft.attributionKind,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(support.ref)
        )
      ))
    )
    val graph = ExplicitCauseAdmissionTestSupport.graph(List(comparisonRecord, support))
    val witnesses = EvidenceObjectBinding.comparisonEndpointEvidenceWitnesses(
      RelativeCauseSourceSide.Candidate,
      fact.candidateLine,
      position,
      comparisonRecord.ref,
      fact,
      List(support),
      List(support),
      graph
    )
    val snapshot = ComparisonEndpointEvidenceSnapshot(
      comparisonRecord.ref,
      fact,
      ComparisonEndpointEvidenceSideSnapshot(
        RelativeCauseSourceSide.Reference,
        fact.referenceLine,
        Nil
      ),
      ComparisonEndpointEvidenceSideSnapshot(
        RelativeCauseSourceSide.Candidate,
        fact.candidateLine,
        witnesses
      )
    )
    val channels = EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(cause, graph)
    assert(channels.nonEmpty, s"expected a direct channel for ${draft.kind}")
    assert(channels.exists(channel =>
      ComparisonEndpointEffectObservationPolicy
        .uniqueNeutralWitnessFor(
          snapshot,
          RelativeCauseSourceSide.Candidate,
          channel,
          graph
        )
        .nonEmpty
    ), s"expected a neutral admission witness for ${draft.kind}")

  private def completedRecord(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      replay: List[LineReplayStep],
      horizon: LineEndgameTechniqueHorizon
  ): EvidenceRecord =
    lineRecord(id, position, line, replay, horizons = List(horizon))

  private def lineRecord(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      replay: List[LineReplayStep],
      consequences: List[LineConsequence] = Nil,
      horizons: List[LineEndgameTechniqueHorizon] = Nil
  ): EvidenceRecord =
    EvidenceRecord(
      evidenceRef(id, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, position, line),
      LineFactEvidence(
        line = line,
        replay = replay,
        consequences = consequences,
        endgameHorizons = horizons
      )
    )

  private def comparison(
      position: PositionNodeRef,
      reference: LineNodeRef,
      candidate: LineNodeRef,
      referenceScore: Int = 900,
      candidateScore: Int = 0
  ): CandidateComparisonFact =
    CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      reference,
      candidate,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          reference,
          EngineLine(List(reference.rootMove), scoreCp = referenceScore, depth = 20),
          evidenceRef("reference-eval", EvidenceProducer.EngineEvalProducer, EvidenceLayer.Eval, position, reference)
        ),
        CandidateLineNode(
          candidate,
          EngineLine(List(candidate.rootMove), scoreCp = candidateScore, depth = 20),
          evidenceRef("candidate-eval", EvidenceProducer.EngineEvalProducer, EvidenceLayer.Eval, position, candidate)
        )
      )
    )

  private def legalReplay(fen: String, moves: List[String]): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(fen -> List.empty[LineReplayStep]) {
      case ((fenBefore, steps), (move, index)) =>
        val fenAfter = PrincipalVariationEvidence
          .legalFenAfter(fenBefore, move)
          .getOrElse(fail(s"expected legal test move $move after $fenBefore"))
        fenAfter -> (steps :+ LineReplayStep(index, move, fenBefore, fenAfter))
    }._2

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: LineNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = producer,
      layer = layer,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence =
        if layer == EvidenceLayer.Eval then EvidenceConfidence.EngineBacked
        else EvidenceConfidence.LegalReplayVerified
    )
