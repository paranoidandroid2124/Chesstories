package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.RelativeCauseDraftPlanner

class CauseChannelMultiplicityTest extends munit.FunSuite:

  test("endpoint-positive tactical records have one Cause-meaning owner"):
    val exactOwners = List(
      TacticalMechanismKind.MaterialGain -> RelativeCauseKind.MaterialSwing,
      TacticalMechanismKind.RecaptureChoice -> RelativeCauseKind.RecaptureRecoveryWindow,
      TacticalMechanismKind.PawnPromotion -> RelativeCauseKind.ConversionSecured,
      TacticalMechanismKind.KingForcing -> RelativeCauseKind.KingForcing
    )
    assertEquals(
      exactOwners.map { case (mechanism, _) =>
        mechanism -> RelativeCauseDraftPlanner.endpointPositiveMechanismCauseKind(mechanism)
      },
      exactOwners.map { case (mechanism, cause) => mechanism -> Some(cause) }
    )

  private val position = PositionNodeRef(
    "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1",
    ply = 0
  )

  private val causeRef = EvidenceRef(
    id = "cause:coexisting-relations",
    producer = EvidenceProducer.RelativeMoveProducer,
    layer = EvidenceLayer.RelativeCause,
    position = position,
    line = None,
    scope = EvidenceScope.PlayedTransition,
    confidence = EvidenceConfidence.LegalReplayVerified
  )

  private val relationRef = causeRef.copy(
    id = "relation:coexisting-relations",
    producer = EvidenceProducer.RelationProducer,
    layer = EvidenceLayer.Relation
  )

  private val rootActor = RootCausalActor(
    moveUci = "e2b5",
    role = EvidencePieceRole("bishop"),
    color = White,
    from = EvidenceSquare("e2"),
    to = EvidenceSquare("b5")
  )

  private def channel(mechanism: String): DirectCauseChannel =
    DirectCauseChannel(
      binding = EvidenceObjectBinding(
        source = relationRef,
        actor = List(ConcreteChessObject(EvidenceObjectKind.Piece, "bishop@b5")),
        target = List(ConcreteChessObject(EvidenceObjectKind.Square, "e8")),
        mechanism = List(ConcreteChessObject(EvidenceObjectKind.Relation, mechanism)),
        consequence = List(ConcreteChessObject(EvidenceObjectKind.Consequence, "king-forcing"))
      ),
      rootActor = rootActor,
      directChange = DirectCausalChange.Occurred
    )

  test("public Cause transport rejects an upstream exact duplicate and retains coexisting relation ideas"):
    val doubleCheck = channel("double-check")
    val checkingMultiControl = channel("checking-multi-control")

    val duplicateFailure = intercept[IllegalArgumentException] {
      PlayerFacingCauseSelectionPolicy.build(
        causeEvidence = causeRef,
        status = CrossComparisonExposureStatus.SelectedPrimary,
        effectMode = PlayerFacingCauseEffectMode.PlayedValue,
        channels = List(doubleCheck, doubleCheck, checkingMultiControl)
      )
    }
    assertEquals(
      duplicateFailure.getMessage,
      "requirement failed: player-facing Cause channels must be canonical before public selection"
    )

    val selection = PlayerFacingCauseSelectionPolicy
      .build(
        causeEvidence = causeRef,
        status = CrossComparisonExposureStatus.SelectedPrimary,
        effectMode = PlayerFacingCauseEffectMode.PlayedValue,
        channels = List(doubleCheck, checkingMultiControl)
      )
      .getOrElse(fail("expected a player-facing Cause selection"))

    assertEquals(selection.channels.size, 2)
    assertEquals(selection.channels.map(_.causalSignature).distinct.size, 2)
    assertEquals(selection.channels.map(_.exactOccurrence).distinct.size, 2)
