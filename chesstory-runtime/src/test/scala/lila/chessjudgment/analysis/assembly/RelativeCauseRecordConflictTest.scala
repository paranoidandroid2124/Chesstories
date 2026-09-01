package lila.chessjudgment.analysis.assembly

import chess.White
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class RelativeCauseRecordConflictTest extends munit.FunSuite:

  test("one exact claim projection cannot hide duplicate producers"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/K6k w - - 0 1", 0, Some(White))
    def carrier(id: String): EvidenceRef =
      EvidenceRef(
        id,
        EvidenceProducer.PositionOccurrenceProducer,
        EvidenceLayer.PositionOccurrence,
        position,
        None,
        EvidenceScope.CurrentPosition,
        EvidenceConfidence.BoardDerived
      )
    def claim(id: String, evidence: List[EvidenceRef] = Nil): JudgmentClaim =
      JudgmentClaim(
        id,
        ClaimFamily.Tactical,
        ClaimSubject.PlayedMove,
        position,
        None,
        None,
        evidence,
        EvidenceScope.CurrentPosition,
        EvidenceConfidence.BoardDerived
      )

    intercept[IllegalArgumentException] {
      ClaimProducerOwnership.validate(List(claim("same-id"), claim("same-id")), TypedEvidenceGraph.empty)
    }
    intercept[IllegalArgumentException] {
      claim("duplicate-evidence", List(carrier("same-carrier"), carrier("same-carrier")))
    }
    intercept[IllegalArgumentException] {
      RelativeCauseFact(
        RelativeCauseKind.MissedTacticalResource,
        carrier("comparison"),
        RelativeCauseSourceSide.Reference,
        List(carrier("same-source"), carrier("same-source"))
      )
    }
    intercept[IllegalArgumentException] {
      ClaimProducerOwnership.validate(List(claim("owner-a"), claim("owner-b")), TypedEvidenceGraph.empty)
    }
    ClaimProducerOwnership.validate(
      List(
        claim("independent-a", List(carrier("carrier-a"))),
        claim("independent-b", List(carrier("carrier-b")))
      ),
      TypedEvidenceGraph.empty
    )

  test("one ready relative Cause cannot hide duplicate producers"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
          playedMoveUci = "d1d8",
          variations = List(
            EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an exact relative Cause fixture"))
    val graph = packet.evidenceGraph
    val owner = graph.records.collectFirst {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        RelativeCauseSemanticKey.from(cause, record.ref.id, graph)
        record
    }.getOrElse(fail("expected one proof-admitted relative Cause"))
    val duplicate = owner.copy(ref = owner.ref.copy(id = s"${owner.ref.id}:duplicate-owner"))

    val conflict = intercept[IllegalArgumentException] {
      RelativeAssessmentAssembler.validateRelativeCauseOwnership(
        List(owner, duplicate),
        graph
      )
    }
    assert(conflict.getMessage.contains("multiple evidence owners"))

  test("comparison and Cause owner ids are stable across admitted input order"):
    val lines = List(
      EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
      EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
    )
    def ownerIds(variations: List[EngineLine]): (List[String], List[String]) =
      val packet = MoveReviewJudgmentOrchestrator
        .execute(
          RawMoveReviewInput(
            fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
            playedMoveUci = "d1d8",
            variations = variations
          )
        )
        .getOrElse(fail("expected an exact relative Cause fixture"))
      val comparisons = packet.evidenceGraph.records.collect {
        case EvidenceRecord(ref, CandidateComparisonEvidence(_), _) => ref.id
      }.sorted
      val causes = packet.evidenceGraph.records.collect {
        case EvidenceRecord(ref, RelativeCauseFactEvidence(_), _) => ref.id
      }.sorted
      comparisons -> causes

    val forward = ownerIds(lines)
    val reversed = ownerIds(lines.reverse)
    assertEquals(reversed, forward)
    assert(forward._1.forall(!_.matches(".*:\\d+$")))
    assert(forward._2.forall(!_.matches(".*:\\d+$")))

  test("independent WrongMoveOrder proof records remain independent Cause drafts"):
    val uniquePacket = resolved(
      RawMoveReviewInput(
        fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
        playedMoveUci = "d1d8",
        variations = List(
          EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
          EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
        )
      )
    )
    val solePacket = resolved(
      RawMoveReviewInput(
        fen = "8/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1",
        playedMoveUci = "d1d5",
        variations = List(
          EngineLine(List("g5f6", "e7f6", "d1d5"), scoreCp = 600, depth = 24),
          EngineLine(List("d1d5", "f6d5"), scoreCp = 0, depth = 24)
        )
      )
    )
    val proofRecords = List(
      uniquePacket.evidenceGraph.records.collectFirst {
        case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
      }.getOrElse(fail("expected the unique-check WrongMoveOrder proof")),
      solePacket.evidenceGraph.records.collectFirst {
        case record @ EvidenceRecord(_, _: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _) => record
      }.getOrElse(fail("expected the sole-recapturer WrongMoveOrder proof"))
    )

    val drafts = RelativeCauseDraftPlanner.onePerExactProofRecord(proofRecords)
    assertEquals(drafts.map(_.kind), List.fill(2)(RelativeCauseKind.WrongMoveOrder))
    assertEquals(drafts.map(_.support.map(_.ref.id)), proofRecords.map(record => List(record.ref.id)))

    val causes = List(uniquePacket, solePacket).flatMap(_.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => cause
    })
    assertEquals(causes.size, 2)
    assertEquals(
      causes.map(_.proofSources.map(_.id)).toSet,
      proofRecords.map(record => List(record.ref.id)).toSet
    )

  test("multiple proof paths in one proposition remain one Cause draft"):
    val packet = resolved(
      RawMoveReviewInput(
        fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
        playedMoveUci = "d1d8",
        variations = List(
          EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
          EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
        )
      )
    )
    val proofRecord = packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) =>
        record -> payload
    }.getOrElse(fail("expected one exact WrongMoveOrder proof"))
    val (record, payload) = proofRecord
    val proofSet = payload.occurrence.proofSet
    val originalPath = proofSet.paths.head
    val independentManifest = new BoundedCausalContractManifest:
      val contractKind = originalPath.manifest.contractKind
      val premiseUses = originalPath.manifest.premiseUses
      val absenceBindings = originalPath.manifest.absenceBindings
      override val stateBindings = originalPath.manifest.stateBindings
      override val supplementalPremiseUses = originalPath.manifest.supplementalPremiseUses
      override val supplementalClosureBindings = originalPath.manifest.supplementalClosureBindings
      val stableKey = s"${originalPath.manifest.stableKey}|independent-path"
    val secondPath = CausalProofPathOccurrence.from(proofSet.proposition, independentManifest)
    val multiPathSet = BoundedCausalProofSet.from(
      proofSet.proposition,
      proofSet.occurrence,
      proofSet.paths :+ secondPath
    )
    val multiPathPayload = payload.copy(
      occurrence = payload.occurrence.copy(proofSet = multiPathSet)
    )
    val multiPathRecord = record.copy(payload = multiPathPayload)

    assertEquals(multiPathPayload.proofPaths.size, 2)
    val drafts = RelativeCauseDraftPlanner.onePerExactProofRecord(List(multiPathRecord))
    assertEquals(drafts.size, 1)
    assertEquals(drafts.head.support, List(multiPathRecord))

  private def resolved(input: RawMoveReviewInput): EvidenceBackedJudgmentPacket =
    MoveReviewJudgmentOrchestrator
      .execute(input)
      .getOrElse(fail("expected an exact relative Cause fixture"))
