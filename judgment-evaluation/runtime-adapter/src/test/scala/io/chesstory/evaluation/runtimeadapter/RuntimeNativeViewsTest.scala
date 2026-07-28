package io.chesstory.evaluation.runtimeadapter

import io.chesstory.runtime.RuntimeProtocol
import lila.chessjudgment.model.judgment.{
  EvidenceBackedJudgmentPacket,
  CauseDisposition,
  CauseDispositionLedger,
  CauseDispositionReason,
  CauseDispositionStatus,
  DirectCauseImportanceDecision,
  DirectCauseImportanceResolution,
  EvidenceConfidence,
  EvidenceLayer,
  EvidenceProducer,
  EvidenceRef,
  EvidenceScope,
  OnlyMoveConstraintResolution,
  PlayerFacingCauseExposureResolution,
  PlayerFacingClaimDecision,
  PlayerFacingClaimTier
}
import play.api.libs.json.*

class RuntimeNativeViewsTest extends munit.FunSuite:

  test("packet structural view exposes every constructor field"):
    val encoded = encode(packetFixture())
    assertEquals(
      fieldNames(encoded),
      List(
        "assembly",
        "probeRequests",
        "playerFacingClaimDecisions",
        "onlyMoveConstraintResolutions",
        "causeExposureResolution",
        "causeDispositionLedger"
      )
    )
    assertEquals((encoded \ "arity").as[Int], 6)

  test("packet canonical hash changes when only player-facing decisions change"):
    val original = packetFixture()
    val first = original.playerFacingClaimDecisions.headOption.getOrElse(
      fail("fixture must produce a player-facing claim decision")
    )
    val replacementTier =
      if first.tier == PlayerFacingClaimTier.Primary then PlayerFacingClaimTier.Secondary
      else PlayerFacingClaimTier.Primary
    val changed = serializationOnlyCopy(
      original,
      decisions = Some(first.copy(tier = replacementTier) :: original.playerFacingClaimDecisions.tail)
    )

    assertEquals(changed.assembly, original.assembly)
    assertEquals(changed.probeRequests, original.probeRequests)
    assertEquals(changed.onlyMoveConstraintResolutions, original.onlyMoveConstraintResolutions)
    assertNotEquals(changed.playerFacingClaimDecisions, original.playerFacingClaimDecisions)
    assertNotEquals(hash(changed), hash(original))

  test("packet canonical hash changes when only OnlyMove resolutions change"):
    val original = packetFixture()
    assert(original.onlyMoveConstraintResolutions.nonEmpty, "fixture must produce an OnlyMove resolution")
    assert(
      original.onlyMoveConstraintResolutions.forall(_.qualifiers.isEmpty),
      "fixture needs diagnostic-only resolutions so the legitimate factory can omit them"
    )
    val changed = serializationOnlyCopy(
      original,
      onlyMoveResolutions = Some(Nil)
    )

    assertEquals(changed.assembly, original.assembly)
    assertEquals(changed.probeRequests, original.probeRequests)
    assertEquals(changed.playerFacingClaimDecisions, original.playerFacingClaimDecisions)
    assertNotEquals(changed.onlyMoveConstraintResolutions, original.onlyMoveConstraintResolutions)
    assertNotEquals(hash(changed), hash(original))

  test("packet canonical hash changes when only typed importance authority changes"):
    val original = packetFixture()
    val changedExposure = original.causeExposureResolution.copy(
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
    val changed = serializationOnlyCopy(original, exposure = Some(changedExposure))

    assertEquals(changed.assembly, original.assembly)
    assertEquals(changed.probeRequests, original.probeRequests)
    assertEquals(changed.playerFacingClaimDecisions, original.playerFacingClaimDecisions)
    assertEquals(changed.onlyMoveConstraintResolutions, original.onlyMoveConstraintResolutions)
    assertNotEquals(changed.causeExposureResolution, original.causeExposureResolution)
    assertNotEquals(hash(changed), hash(original))

  test("packet canonical hash changes when only the Cause disposition ledger changes"):
    val original = packetFixture()
    val syntheticDisposition = CauseDisposition(
      causeEvidence = EvidenceRef(
        id = "synthetic-relative-cause",
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.RelativeCause,
        position = original.root,
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
    val replacement =
      if original.causeDispositionLedger.dispositions.nonEmpty then CauseDispositionLedger.empty
      else CauseDispositionLedger(List(syntheticDisposition))
    val changed = serializationOnlyCopy(
      original,
      dispositionLedger = Some(replacement)
    )

    assertEquals(changed.assembly, original.assembly)
    assertEquals(changed.causeExposureResolution, original.causeExposureResolution)
    assertNotEquals(changed.causeDispositionLedger, original.causeDispositionLedger)
    assertNotEquals(hash(changed), hash(original))

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

  private def fieldNames(encoded: JsObject): List[String] =
    (encoded \ "fields")
      .as[List[JsObject]]
      .flatMap(field => (field \ "name").asOpt[String])

  /** Constructs no production packet. This deliberately bypasses packet closure
    * only to prove that the structural serializer is sensitive to each private
    * constructor field in isolation. Runtime behavior tests must use the factory.
    */
  private def serializationOnlyCopy(
      packet: EvidenceBackedJudgmentPacket,
      decisions: Option[List[PlayerFacingClaimDecision]] = None,
      onlyMoveResolutions: Option[List[OnlyMoveConstraintResolution]] = None,
      exposure: Option[PlayerFacingCauseExposureResolution] = None,
      dispositionLedger: Option[CauseDispositionLedger] = None
  ): EvidenceBackedJudgmentPacket =
    val constructor = classOf[EvidenceBackedJudgmentPacket].getDeclaredConstructors
      .find(_.getParameterCount == 6)
      .getOrElse(fail("expected the six-field packet constructor"))
    constructor.setAccessible(true)
    constructor
      .newInstance(
        packet.assembly,
        packet.probeRequests,
        decisions.getOrElse(packet.playerFacingClaimDecisions),
        onlyMoveResolutions.getOrElse(packet.onlyMoveConstraintResolutions),
        exposure.getOrElse(packet.causeExposureResolution),
        dispositionLedger.getOrElse(packet.causeDispositionLedger)
      )
      .asInstanceOf[EvidenceBackedJudgmentPacket]
