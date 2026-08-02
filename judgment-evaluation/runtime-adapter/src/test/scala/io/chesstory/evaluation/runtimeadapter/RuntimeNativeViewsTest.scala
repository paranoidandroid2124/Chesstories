package io.chesstory.evaluation.runtimeadapter

import io.chesstory.runtime.RuntimeProtocol
import lila.chessjudgment.model.ProbeRequest
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

class RuntimeNativeViewsTest extends munit.FunSuite:

  test("packet audit fields remain native-tree and hash sensitive"):
    val packet = packetFixture()
    val firstClaim = packet.assembly.claims.headOption.getOrElse(
      fail("fixture must produce an assembly claim")
    )
    val firstDecision = packet.playerFacingClaimDecisions.headOption.getOrElse(
      fail("fixture must produce a player-facing claim decision")
    )
    assert(packet.onlyMoveConstraintResolutions.nonEmpty, "fixture must produce an OnlyMove resolution")

    val changedTier =
      if firstDecision.tier == PlayerFacingClaimTier.Primary then PlayerFacingClaimTier.Secondary
      else PlayerFacingClaimTier.Primary
    val changedExposure = packet.causeExposureResolution.copy(
      importanceResolution = DirectCauseImportanceResolution(
        selectedCauseEvidenceIds = List("synthetic-cause"),
        profiles = Nil,
        relations = Nil,
        decisions = List(
          DirectCauseImportanceDecision(
            causeEvidenceId = "synthetic-cause",
            measuredChannelSignatures = Nil,
            unmeasuredChannelSignatures = List("synthetic-channel"),
            dominatingCauseEvidenceIds = Nil,
            dominatedWithinDomain = false
          )
        )
      )
    )
    val syntheticDisposition = CauseDisposition(
      causeEvidence = EvidenceRef(
        id = "synthetic-relative-cause",
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.RelativeCause,
        position = packet.root,
        line = None,
        scope = EvidenceScope.CurrentPosition,
        confidence = EvidenceConfidence.BoardDerived
      ),
      status = CauseDispositionStatus.Unproposed,
      reason = CauseDispositionReason.NoClaimProposal,
      proposedClaimIds = Nil,
      certifiedClaimIds = Nil,
      rankEligibleClaimIds = Nil,
      selectedOwnerClaimId = None,
      relatedCauseEvidenceIds = Nil,
      relatedClaimIds = Nil
    )
    val changedLedger =
      if packet.causeDispositionLedger.dispositions.nonEmpty then CauseDispositionLedger.empty
      else CauseDispositionLedger(List(syntheticDisposition))
    val variants = List(
      "assembly" -> packetWith(
        packet,
        "assembly",
        packet.assembly.copy(claims = firstClaim.copy(id = s"${firstClaim.id}-native") :: packet.assembly.claims.tail)
      ),
      "probeRequests" -> packetWith(
        packet,
        "probeRequests",
        ProbeRequest("native-view-probe", chess.variant.Standard.initialFen.value, List("e2e4"), 1) :: packet.probeRequests
      ),
      "playerFacingClaimDecisions" -> packetWith(
        packet,
        "playerFacingClaimDecisions",
        firstDecision.copy(tier = changedTier) :: packet.playerFacingClaimDecisions.tail
      ),
      "onlyMoveConstraintResolutions" -> packetWith(packet, "onlyMoveConstraintResolutions", Nil),
      "causeExposureResolution" -> packetWith(packet, "causeExposureResolution", changedExposure),
      "causeDispositionLedger" -> packetWith(packet, "causeDispositionLedger", changedLedger),
      "selectedContentClaimIds" -> packetWith(
        packet,
        "selectedContentClaimIds",
        "synthetic-content-claim" :: packet.selectedContentClaimIds
      )
    )
    val encoded = encode(packet)
    val canonicalHash = hash(packet)

    variants.foreach { case (field, changed) =>
      assert(
        nativeField(encode(changed), field) != nativeField(encoded, field),
        s"$field must remain in the native tree"
      )
      assert(hash(changed) != canonicalHash, s"$field must influence the canonical hash")
    }

  private def packetFixture(): EvidenceBackedJudgmentPacket =
    val request = Json.obj(
      "schema_version" -> RuntimeProtocol.RequestSchema,
      "request_id" -> "native-packet-view-test",
      "input" -> Json.obj(
        "fen" -> chess.variant.Standard.initialFen.value,
        "playedMoveUci" -> "d2d4",
        "variations" -> Json.arr(
          Json.obj("moves" -> Json.arr("e2e4", "e7e5", "g1f3"), "scoreCp" -> 900, "depth" -> 18),
          Json.obj("moves" -> Json.arr("d2d4", "d7d5", "g1f3"), "scoreCp" -> 0, "depth" -> 18),
          Json.obj("moves" -> Json.arr("g1f3", "d7d5", "d2d4"), "scoreCp" -> -900, "depth" -> 18)
        ),
        "ply" -> 0,
        "movePrefixUci" -> Json.arr()
      )
    )
    RuntimeProtocol
      .evaluateWithBoundary(request, RuntimeProtocol.RuntimeBoundaryIntervention.identity)
      .boundaryExecution
      .flatMap(_.packet)
      .getOrElse(fail("fixture request must produce a packet"))

  private def encode(packet: EvidenceBackedJudgmentPacket): JsObject =
    NativeTreeEncoder.encode(packet, RuntimeNativeViews.adapters)

  private def hash(packet: EvidenceBackedJudgmentPacket): String =
    NativeTreeEncoder.sha256Canonical(encode(packet))

  private def nativeField(tree: JsObject, name: String): JsValue =
    (tree \ "fields")
      .as[List[JsObject]]
      .find(field => (field \ "name").as[String] == name)
      .flatMap(field => (field \ "value").toOption)
      .getOrElse(fail(s"native tree has no $name field"))

  /** The packet factory is intentionally inaccessible from this adapter package.
    * Reflection is limited to serialization sensitivity; runtime fixtures still use the factory.
    */
  private def packetWith(
      packet: EvidenceBackedJudgmentPacket,
      field: String,
      replacement: Any
  ): EvidenceBackedJudgmentPacket =
    val fields = List(
      "assembly" -> packet.assembly,
      "probeRequests" -> packet.probeRequests,
      "playerFacingClaimDecisions" -> packet.playerFacingClaimDecisions,
      "onlyMoveConstraintResolutions" -> packet.onlyMoveConstraintResolutions,
      "causeExposureResolution" -> packet.causeExposureResolution,
      "causeDispositionLedger" -> packet.causeDispositionLedger,
      "selectedContentClaimIds" -> packet.selectedContentClaimIds
    )
    val constructor = classOf[EvidenceBackedJudgmentPacket].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(fields.map { case (name, value) => if name == field then replacement else value }.map(_.asInstanceOf[AnyRef])* )
      .asInstanceOf[EvidenceBackedJudgmentPacket]
