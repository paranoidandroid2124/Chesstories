package lila.chessjudgment.analysis.assembly

import chess.{ Bishop, Black, Color, King, Knight, Pawn, Queen, Rook, Square, White }
import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.strategic.EngineLine

class RootOwnedCausalEpisodeTest extends munit.FunSuite:

  test("a later same-actor capture is not a root-owned material result"):
    val line = continuationOnlyMaterialLine()
    val episodes = RootOwnedCausalEpisode
      .from(line, line.line.rootMove)
      .filter(_.consequence.kind == LineConsequenceKind.MaterialGain)
    val episode = episodes match
      case exact :: Nil => exact
      case other        => fail(s"expected one raw continuation episode, got ${other.size}")
    val record = lineRecord("continuation-only-proof", line)

    assertEquals(episode.links.map(_.kind).toSet, Set(RootCausalLinkKind.RootActorContinuation))
    assert(!RootOwnedEffectPolicy.admitsLineEpisode(line, episode))
    assertEquals(line.rootOwnedCausalEpisodes(line.line.rootMove), Nil)
    assert(!episode.forcingTacticalResource(line))
    assertEquals(RelativeCauseSignalProfile.materialGainRecords(List(record)), Nil)
    assertEquals(
      EvidenceObjectBinding
        .comparisonEndpointLineObservations(record.ref, line, record.ref.position)
        .material,
      ComparisonEndpointEffectInventory.Complete(Set.empty)
    )
    assertEquals(projectionChannels(line, RelativeCauseKind.MaterialSwing, Black), Nil)

  test("same-actor continuation alone cannot own delayed terminal results"):
    val rows = List(
      (
        "mate",
        continuationOnlyTerminalLine(
          id = "continuation-only-mate",
          fen = "7k/8/5K2/4Q3/8/8/8/8 w - - 0 1",
          moves = List("e5g5", "h8h7", "g5g7"),
          kind = LineConsequenceKind.Mate
        ),
        LineConsequenceKind.Mate,
        RelativeCauseKind.KingForcing
      ),
      (
        "promotion",
        continuationOnlyTerminalLine(
          id = "continuation-only-promotion",
          fen = "7k/8/4P3/8/8/8/8/K7 w - - 0 1",
          moves = List("e6e7", "h8g8", "e7e8q"),
          kind = LineConsequenceKind.Promotion
        ),
        LineConsequenceKind.Promotion,
        RelativeCauseKind.ConversionSecured
      )
    )

    rows.foreach { case (label, line, kind, causeKind) =>
      val raw = RootOwnedCausalEpisode
        .from(line, line.line.rootMove)
        .filter(_.consequence.kind == kind)
      val episode = raw match
        case exact :: Nil => exact
        case other        => fail(s"expected one raw $label continuation episode, got ${other.size}")
      assertEquals(
        episode.links.map(_.kind).toSet,
        Set(RootCausalLinkKind.RootActorContinuation),
        label
      )
      assert(!RootOwnedEffectPolicy.admitsLineEpisode(line, episode), label)
      assertEquals(line.rootOwnedCausalEpisodes(line.line.rootMove), Nil, label)
      val record = lineRecord(s"$label-continuation-source", line, White)
      val inventories = EvidenceObjectBinding.comparisonEndpointLineObservations(
        record.ref,
        line,
        record.ref.position
      )
      if kind == LineConsequenceKind.Mate then
        assertEquals(inventories.mate, ComparisonEndpointEffectInventory.Complete(Set.empty), label)
      else
        assertEquals(inventories.qualitative, ComparisonEndpointEffectInventory.Complete(Set.empty), label)
      if kind == LineConsequenceKind.Promotion then
        assertEquals(RelativeCauseSignalProfile.promotionRaceRecords(List(record)), Nil, label)
        assertEquals(conversionDrafts(line, White), Nil, label)
      assertEquals(projectionChannels(line, causeKind, White), Nil, label)
    }

  test("a forced capture response preserves a root-triggered material sequence"):
    val line = rootTriggeredMaterialSequenceLine()
    val episode = line
      .rootOwnedCausalEpisodes(line.line.rootMove)
      .find(_.consequence.kind == LineConsequenceKind.MaterialGain)
      .getOrElse(fail("expected a root-triggered material episode"))
    val record = lineRecord("root-triggered-material-proof", line, White)

    assert(episode.links.exists(_.kind == RootCausalLinkKind.ForcedCaptureResponse))
    assert(RootOwnedEffectPolicy.admitsLineEpisode(line, episode))
    assert(episode.forcingTacticalResource(line))
    assertEquals(RelativeCauseSignalProfile.materialGainRecords(List(record)), List(record))
    EvidenceObjectBinding
      .comparisonEndpointLineObservations(record.ref, line, record.ref.position)
      .material match
      case ComparisonEndpointEffectInventory.Complete(observations) =>
        assert(observations.nonEmpty)
      case ComparisonEndpointEffectInventory.Incomplete =>
        fail("a root-triggered exact material sequence must keep a complete inventory")
    assert(
      projectionChannels(line, RelativeCauseKind.MaterialSwing, White)
        .exists(_.rootOwnedProof.exists {
          case RootOwnedEffectProof.LineEpisode(_, _, owned) => owned == episode
          case _                                             => false
        })
    )

  test("strong intermediate root bridges preserve longer exact material transports"):
    val rows = List(
      (
        "capture response",
        longForcedCaptureMaterialSequenceLine(),
        RootCausalLinkKind.ForcedCaptureResponse
      ),
      (
        "check response",
        longForcedCheckMaterialSequenceLine(),
        RootCausalLinkKind.ForcedCheckResponse
      ),
      (
        "immediate root actor loss",
        longRootActorLossMaterialSequenceLine(),
        RootCausalLinkKind.RootActorCaptured
      )
    )

    rows.foreach { case (label, line, seedKind) =>
      val episode = line
        .rootOwnedCausalEpisodes(line.line.rootMove)
        .find(_.consequence.kind == LineConsequenceKind.MaterialGain)
        .getOrElse(fail(s"$label should preserve a material-gain episode"))
      val record = lineRecord(s"long-${label.replace(' ', '-')}", line, White)

      assert(episode.links.exists(_.kind == seedKind), label)
      assert(
        episode.links.exists(link =>
          link.kind == RootCausalLinkKind.MaterialActorContinuation ||
            link.kind == RootCausalLinkKind.MaterialCaptureResponse
        ),
        s"$label should transport the strong seed to the final event"
      )
      assert(RootOwnedEffectPolicy.admitsLineEpisode(line, episode), label)
      assert(episode.forcingTacticalResource(line), label)
      assertEquals(RelativeCauseSignalProfile.materialGainRecords(List(record)), List(record), label)
      EvidenceObjectBinding
        .comparisonEndpointLineObservations(record.ref, line, record.ref.position)
        .material match
        case ComparisonEndpointEffectInventory.Complete(observations) =>
          assert(observations.nonEmpty, label)
        case ComparisonEndpointEffectInventory.Incomplete =>
          fail(s"$label should keep a complete material inventory")
      assert(
        projectionChannels(line, RelativeCauseKind.MaterialSwing, White)
          .exists(_.rootOwnedProof.exists {
            case RootOwnedEffectProof.LineEpisode(_, _, owned) => owned == episode
            case _                                             => false
          }),
        label
      )
    }

  test("broad continuation geometry cannot seed a root-owned material result"):
    val unrelatedCheck = unrelatedCheckFollowUpMaterialLine()
    val checkSteps = unrelatedCheck.lineReplaySteps
    val broadCheck = CheckResponseFollowUpTrajectory.find(
      checkSteps.head,
      checkSteps(1),
      checkSteps(2),
      List(checkSteps(1))
    )
    assert(broadCheck.nonEmpty)
    assertEquals(unrelatedCheck.rootOwnedCausalEpisodes(unrelatedCheck.line.rootMove), Nil)
    assertEquals(
      projectionChannels(unrelatedCheck, RelativeCauseKind.MaterialSwing, White),
      Nil
    )

    val placementOnly = placementBeforeClearanceMaterialLine()
    val placementSteps = placementOnly.lineReplaySteps
    assert(
      LineAccessTrajectory
        .find(placementSteps.head, placementSteps(2), List(placementSteps(1)))
        .nonEmpty
    )
    assertEquals(
      LineAccessTrajectory.findRootClearanceBeforeUse(
        placementSteps.head,
        placementSteps(2),
        List(placementSteps(1))
      ),
      None
    )
    assertEquals(placementOnly.rootOwnedCausalEpisodes(placementOnly.line.rootMove), Nil)
    assertEquals(
      projectionChannels(placementOnly, RelativeCauseKind.MaterialSwing, White),
      Nil
    )

  test("delayed root-owned causal episode truth table"):
    val clearance = clearanceLine()
    val recoveryChain = recoveryChainLine()
    val immediateRecapture = immediateRecaptureLine()
    val unrelated = unrelatedFollowUpLine()
    val pawnLoss = actorLossLine(
      id = "pawn-loss",
      fen = "4k3/1b6/8/8/8/8/4P3/4K3 w - - 0 1",
      moves = List("e2e4", "b7e4"),
      actorRole = Pawn.name
    )
    val knightLoss = actorLossLine(
      id = "knight-loss",
      fen = "4k3/2b5/8/8/8/5N2/8/4K3 w - - 0 1",
      moves = List("f3e5", "c7e5"),
      actorRole = Knight.name
    )

    val rows = List(
      ("continuous clearance", clearance, true, Some(RootCausalLinkKind.ContinuousLineAccess)),
      ("continuous material recovery chain", recoveryChain, true, Some(RootCausalLinkKind.MaterialCaptureResponse)),
      ("immediate typed recapture", immediateRecapture, true, Some(RootCausalLinkKind.RootActorCaptured)),
      ("unrelated later tactic", unrelated, false, None),
      ("pawn root actor captured", pawnLoss, true, Some(RootCausalLinkKind.RootActorCaptured)),
      ("knight root actor captured", knightLoss, true, Some(RootCausalLinkKind.RootActorCaptured))
    )

    rows.foreach { case (label, line, expectedOwned, expectedLink) =>
      val episodes = line.rootOwnedCausalEpisodes(line.line.rootMove)
      assertEquals(episodes.nonEmpty, expectedOwned, label)
      expectedLink.foreach(link =>
        assert(episodes.exists(_.links.exists(_.kind == link)), s"$label should contain $link")
      )
    }
    assert(clearance.rootOwnedCausalEpisodes(clearance.line.rootMove).exists(_.forcingTacticalResource(clearance)))
    val recoveryEpisodes = recoveryChain.rootOwnedCausalEpisodes(recoveryChain.line.rootMove)
    assert(recoveryEpisodes.exists(_.links.exists(_.kind == RootCausalLinkKind.ContinuousLineAccess)))
    assert(recoveryEpisodes.exists(_.links.exists(_.kind == RootCausalLinkKind.MaterialActorContinuation)))
    val recoveryConsequence = recoveryChain.lineConsequences
      .find(_.kind == LineConsequenceKind.RecoveryWindow)
      .getOrElse(fail("expected the durable recovery event"))
    assertEquals(recoveryChain.durableRecoveryCaptureForMover.map(_.moveUci), Some("a7b6"))
    assertEquals(recoveryConsequence.eventMove, Some("a7b6"))
    assertEquals(recoveryConsequence.lineMoves.lastOption, Some("a7b6"))
    val recaptureEpisode = immediateRecapture
      .rootOwnedCausalEpisodes(immediateRecapture.line.rootMove)
      .find(_.consequence.kind == LineConsequenceKind.RecaptureSequence)
      .getOrElse(fail("expected the exact b1d2 recapture"))
    assertEquals(recaptureEpisode.consequence.eventMove, Some("b1d2"))
    assertEquals(recaptureEpisode.consequence.beneficiary, Some(White))
    assertEquals(recaptureEpisode.eventPlyOffset, 1)
    assert(RootOwnedEffectPolicy.admitsLineEpisode(immediateRecapture, recaptureEpisode))
    val recaptureRecord = lineRecord("cs07-immediate-recapture", immediateRecapture)
    EvidenceObjectBinding
      .comparisonEndpointLineObservations(
        recaptureRecord.ref,
        immediateRecapture,
        recaptureRecord.ref.position
      )
      .qualitative match
      case ComparisonEndpointEffectInventory.Complete(observations) =>
        assert(observations.nonEmpty)
      case ComparisonEndpointEffectInventory.Incomplete =>
        fail("an exact admitted recapture must keep a complete qualitative inventory")
    assertEquals(pawnLoss.rootOwnedCausalConsequences(pawnLoss.line.rootMove).map(_.kind), List(LineConsequenceKind.MaterialLoss))
    assertEquals(knightLoss.rootOwnedCausalConsequences(knightLoss.line.rootMove).map(_.kind), List(LineConsequenceKind.MaterialLoss))
    List("pawn" -> pawnLoss, "knight" -> knightLoss).foreach { case (label, line) =>
      val owned = line.ownedLineConsequences(
        eventLine = line.line,
        rootMoveUci = line.line.rootMove,
        expectedRootSide = White,
        expectedBeneficiary = Black
      )
      assertEquals(owned.map(_.kind), List(LineConsequenceKind.MaterialLoss), label)
    }

  test("public Cause projection keeps the root actor and consequence-aware material target"):
    val clearance = clearanceLine()
    val bindings = projectionBindings(clearance, RelativeCauseKind.MissedTacticalResource, Black)
      .filter(_.specificTargetMechanismReady)
    val material = bindings.find(_.consequence.exists(_.key.equalsIgnoreCase("MaterialGain")))
      .getOrElse(fail("expected a root-owned material projection"))

    assert(objectKeys(material.actor, EvidenceObjectKind.Move)("f6d5"))
    assert(objectKeys(material.actor, EvidenceObjectKind.Side)("black"))
    assert(objectKeys(material.actor, EvidenceObjectKind.Piece)("knight"))
    assert(Set("f6", "d5").subsetOf(objectKeys(material.actor, EvidenceObjectKind.Square)))
    assert(!objectKeys(material.actor, EvidenceObjectKind.Move)("e7h4"))
    assert(!objectKeys(material.actor, EvidenceObjectKind.Piece)("bishop"))
    assert(objectKeys(material.target, EvidenceObjectKind.Square)("h4"))
    assert(objectKeys(material.target, EvidenceObjectKind.Piece)("queen"))
    assert(material.mechanism.exists(_.key.equalsIgnoreCase("ContinuousLineAccess")))
    assert(material.witness.exists(obj => obj.kind == EvidenceObjectKind.Move && obj.key == "e7h4"))
    assertEquals(clearance.rootOwnedCausalEpisodes(clearance.line.rootMove).map(_.target.key), List("h4"))

    val unrelated = unrelatedFollowUpLine()
    assertEquals(
      projectionBindings(unrelated, RelativeCauseKind.MissedTacticalResource, Black)
        .filter(_.specificTargetMechanismReady),
      Nil
    )

  test("root-local public projection preserves admitted events with verified targets"):
    val checkingFen = "4k3/8/8/8/8/8/4Q3/4K3 w - - 0 1"
    val check = rootEventLine(
      id = "root-check",
      fen = checkingFen,
      move = "e2e7",
      eventKind = LineEventKind.Check,
      pieceRole = Queen.name,
      targetRole = King.name,
      targetSquare = "e8"
    )
    val tempo = rootEventLine(
      id = "root-tempo",
      fen = checkingFen,
      move = "e2e7",
      eventKind = LineEventKind.Tempo,
      pieceRole = Queen.name,
      targetRole = King.name,
      targetSquare = "e8"
    )
    val mateConsequence = LineConsequence(
      kind = LineConsequenceKind.Mate,
      lineMoves = List("g6g7"),
      proofSignal = true,
      eventMove = Some("g6g7"),
      rootMove = Some("g6g7"),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val mate = rootEventLine(
      id = "root-mate",
      fen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1",
      move = "g6g7",
      eventKind = LineEventKind.Mate,
      pieceRole = Queen.name,
      targetRole = King.name,
      targetSquare = "h8",
      consequences = List(mateConsequence)
    )
    val rootCapture = capture("e2d2", 0, White, Queen.name, "Rook", "d2", 500)
    val captureLine = rootEventLine(
      id = "root-capture",
      fen = "4k3/8/8/8/8/8/3rQ3/4K3 w - - 0 1",
      move = "e2d2",
      eventKind = LineEventKind.Capture,
      pieceRole = Queen.name,
      targetRole = "Rook",
      targetSquare = "d2",
      materialCapture = Some(rootCapture)
    )
    val promotion = rootEventLine(
      id = "root-promotion",
      fen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1",
      move = "a7a8q",
      eventKind = LineEventKind.Promotion,
      pieceRole = Pawn.name,
      targetRole = Queen.name,
      targetSquare = "a8"
    )
    val defender = rootEventLine(
      id = "root-defender",
      fen = "4r1k1/8/8/8/8/8/8/4K3 w - - 0 1",
      move = "e1f1",
      eventKind = LineEventKind.DefenderMove,
      pieceRole = King.name,
      targetRole = King.name,
      targetSquare = "e1"
    )

    val rows = List(
      ("check", check, RelativeCauseKind.KingForcing, LineEventKind.Check, "e8", King.name),
      ("capture", captureLine, RelativeCauseKind.MissedTacticalResource, LineEventKind.Capture, "d2", "Rook"),
      ("promotion", promotion, RelativeCauseKind.MissedTacticalResource, LineEventKind.Promotion, "a8", Queen.name)
    )
    rows.foreach { case (label, line, causeKind, eventKind, targetSquare, targetRole) =>
      val binding = projectionBindings(line, causeKind, White)
        .filter(_.specificTargetMechanismReady)
        .find(_.mechanism.exists(_.key.equalsIgnoreCase(eventKind.toString)))
        .getOrElse(fail(s"expected ready $label root-event projection"))
      assert(objectKeys(binding.actor, EvidenceObjectKind.Move)(line.line.rootMove), label)
      assert(objectKeys(binding.target, EvidenceObjectKind.Square)(targetSquare), label)
      assert(objectKeys(binding.target, EvidenceObjectKind.Piece)(targetRole.toLowerCase), label)
    }
    assertEquals(
      projectionChannels(defender, RelativeCauseKind.ConversionSecured, White),
      Nil,
      "a root DefenderMove alone must not admit conversion secured"
    )
    val mateChannels = projectionChannels(mate, RelativeCauseKind.KingForcing, White)
      .filter(_.binding.specificTargetMechanismReady)
    assertEquals(mateChannels.size, 1)
    val mateChannel = mateChannels.head
    assert(mateChannel.rootOwnedProof.exists {
      case _: RootOwnedEffectProof.LineEpisode => true
      case _                                   => false
    })
    assert(!mateChannels.exists(_.rootOwnedProof.exists {
      case _: RootOwnedEffectProof.RootLineEvent => true
      case _                                     => false
    }))
    assert(objectKeys(mateChannel.binding.actor, EvidenceObjectKind.Move)(mate.line.rootMove))
    assert(objectKeys(mateChannel.binding.target, EvidenceObjectKind.Square)("h8"))
    assert(objectKeys(mateChannel.binding.target, EvidenceObjectKind.Piece)(King.name.toLowerCase))
    assert(mateChannel.binding.consequence.exists(_.key.equalsIgnoreCase(LineConsequenceKind.Mate.toString)))
    val tempoBinding = projectionBindings(tempo, RelativeCauseKind.WrongMoveOrder, White, liability = true)
      .filter(_.specificTargetMechanismReady)
      .find(_.mechanism.exists(_.key.equalsIgnoreCase(LineEventKind.Tempo.toString)))
      .getOrElse(fail("expected candidate-owned tempo liability projection"))
    assert(objectKeys(tempoBinding.actor, EvidenceObjectKind.Move)(tempo.line.rootMove))
    assert(!projectionBindings(tempo, RelativeCauseKind.WrongMoveOrder, White)
      .exists(_.mechanism.exists(_.key.equalsIgnoreCase(LineEventKind.Tempo.toString))))
    assert(!projectionBindings(check, RelativeCauseKind.WrongMoveOrder, White)
      .exists(_.mechanism.exists(_.key.equalsIgnoreCase(LineEventKind.Check.toString))))
    assertEquals(mate.rootOwnedCausalEpisodes(mate.line.rootMove).map(_.target.key), List("h8"))

    val nonStalemateMove = "e2e3"
    val nonStalemateLine = LineNodeRef("unverified-draw", nonStalemateMove, 1, LineNodeRole.BestReference)
    val unverifiedDraw = LineFactEvidence(
      line = nonStalemateLine,
      replay = legalReplay(checkingFen, List(nonStalemateMove)),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.DrawResource,
        lineMoves = List(nonStalemateMove),
        proofSignal = true,
        eventMove = Some(nonStalemateMove),
        rootMove = Some(nonStalemateMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    assertEquals(
      projectionBindings(unverifiedDraw, RelativeCauseKind.DrawResource, White)
        .filter(_.specificTargetMechanismReady),
      Nil,
      "a draw consequence without a verified stalemated king must not become public-ready"
    )

  test("root-owned promotion and root-triggered endgames retain conversion evidence"):
    val promotion = forcedPromotionLine()
    val promotionRecord = lineRecord("forced-promotion-proof", promotion, White)
    val promotionEpisode = promotion
      .rootOwnedCausalEpisodes(promotion.line.rootMove)
      .find(_.consequence.kind == LineConsequenceKind.Promotion)
      .getOrElse(fail("expected a root-owned forced promotion"))
    val promotionChannels = projectionChannels(promotion, RelativeCauseKind.ConversionSecured, White)

    assert(promotionEpisode.links.exists(_.kind == RootCausalLinkKind.ForcedCheckResponse))
    assertEquals(RelativeCauseSignalProfile.promotionRaceRecords(List(promotionRecord)), List(promotionRecord))
    assertEquals(conversionDrafts(promotion, White).size, 1)
    assertEquals(promotionChannels.size, 1)
    assert(promotionChannels.head.rootOwnedProof.exists {
      case RootOwnedEffectProof.LineEpisode(_, _, episode) => episode == promotionEpisode
      case _                                               => false
    })
    assert(promotionChannels.head.binding.consequence.exists(_.key.equalsIgnoreCase("Promotion")))

    List(
      "maintained" -> rootTriggeredEndgameConversionLine(LineEndgameTechniqueHorizonStatus.Active),
      "completed" -> rootTriggeredEndgameConversionLine(LineEndgameTechniqueHorizonStatus.Completed)
    ).foreach { case (label, line) =>
      val channels = projectionChannels(line, RelativeCauseKind.ConversionSecured, White)
      assert(line.endgameTechniquesTriggeredByRootMove(line.line.rootMove, RelativeCauseKind.ConversionSecured).nonEmpty, label)
      assert(channels.exists(_.rootOwnedProof.exists {
        case RootOwnedEffectProof.EndgameHorizon(_, _, _) => true
        case _                                            => false
      }), label)
    }

  test("missed tactical resource admission truth table"):
    val clearance = clearanceLine()
    val unrelated = unrelatedFollowUpLine()
    val exactMotifLine = LineNodeRef("motif-reference", "d1h5", 1, LineNodeRole.BestReference)
    val siblingMotifLine = LineNodeRef("motif-sibling", "d1h5", 9, LineNodeRole.Threat)

    val rows = List(
      mtrRow(
        "verified enabling episode",
        clearance.line,
        Black,
        List(lineRecord("clearance-proof", clearance)),
        expected = true
      ),
      mtrRow(
        "unrelated follow-up tactic",
        unrelated.line,
        Black,
        List(lineRecord("unrelated-proof", unrelated)),
        expected = false
      ),
      mtrRow(
        "independent root forcing motif",
        exactMotifLine,
        White,
        List(motifRecord("exact-motif", exactMotifLine, exactMotifLine)),
        expected = true
      ),
      mtrRow(
        "sibling forcing motif",
        exactMotifLine,
        White,
        List(motifRecord("sibling-motif", siblingMotifLine, exactMotifLine)),
        expected = false
      ),
      mtrRow(
        "evaluation delta only",
        exactMotifLine,
        White,
        List(evalRecord("eval-only", exactMotifLine)),
        expected = false
      )
    )

    rows.foreach { case (label, actual, expected) =>
      assertEquals(actual, expected, label)
    }

  private def mtrRow(
      label: String,
      referenceLine: LineNodeRef,
      mover: Color,
      records: List[EvidenceRecord],
      expected: Boolean
  ): (String, Boolean, Boolean) =
    val candidateLine = LineNodeRef(s"${referenceLine.id}-candidate", "a2a3", 2, LineNodeRole.Played)
    val position = PositionNodeRef(
      fen = "4k3/8/8/8/8/8/8/4K3 w - - 0 1",
      ply = 0,
      sideToMove = Some(mover)
    )
    val fact = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparison = EvalComparison.fromLines(
        mover,
        CandidateLineNode(
          referenceLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(referenceLine.rootMove), scoreCp = 300, depth = 18)),
          lineRef("reference-eval", referenceLine, position)
        ),
        CandidateLineNode(
          candidateLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(candidateLine.rootMove), scoreCp = -300, depth = 18)),
          lineRef("candidate-eval", candidateLine, position)
        )
      ).get,
      verdictConfidence = VerdictConfidence.EngineBacked
    )
    val binding = RelativeCauseBinding(
      role = RelativeCauseRole.PrimaryPlayedCause,
      sourceSide = RelativeCauseSourceSide.Reference,
      eventLine = referenceLine,
      evidenceLines = List(referenceLine, candidateLine),
      bindingTier = RelativeCauseBindingTier.Primary
    )
    val graph = records.foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    (
      label,
      RootOwnedCausePolicy.directProofRecords(
        graph,
        fact,
        RelativeCauseKind.MissedTacticalResource,
        binding,
        CauseAttributionKind.ReferenceCreatesResource,
        records
      ).nonEmpty,
      expected
    )

  private def clearanceLine(): LineFactEvidence =
    val fen = "r1b1k2r/p1q1bpp1/1pp1pn1p/8/3P1B1Q/3B1N2/PPP2PPP/R4RK1 b kq - 3 14"
    val moves = List("f6d5", "f4c7", "e7h4")
    val captures = List(
      capture("f4c7", 1, White, Bishop.name, Queen.name, "c7", 900),
      capture("e7h4", 2, Black, Bishop.name, Queen.name, "h4", 900)
    )
    materialLine(
      id = "clearance-reference",
      fen = fen,
      moves = moves,
      sideToMove = Black,
      captures = captures,
      consequence = LineConsequence(
        kind = LineConsequenceKind.MaterialGain,
        lineMoves = List("e7h4"),
        proofSignal = true,
        eventMove = Some("e7h4"),
        rootMove = None,
        rootSide = Some(Black),
        beneficiary = Some(Black)
      )
    )

  private def unrelatedFollowUpLine(): LineFactEvidence =
    val fen = "r1b1k2r/p1q1bpp1/1pp1pn1p/8/3P1B1Q/3B1N2/PPP2PPP/R4RK1 b kq - 3 14"
    val moves = List("a7a6", "f4c7", "f6d5", "c2c3", "e7h4")
    val captures = List(
      capture("f4c7", 1, White, Bishop.name, Queen.name, "c7", 900),
      capture("e7h4", 4, Black, Bishop.name, Queen.name, "h4", 900)
    )
    materialLine(
      id = "unrelated-reference",
      fen = fen,
      moves = moves,
      sideToMove = Black,
      captures = captures,
      consequence = LineConsequence(
        kind = LineConsequenceKind.MaterialGain,
        lineMoves = List("e7h4"),
        proofSignal = true,
        eventMove = Some("e7h4"),
        rootMove = Some("a7a6"),
        rootSide = Some(Black),
        beneficiary = Some(Black)
      )
    )

  private def recoveryChainLine(): LineFactEvidence =
    val fen = "r1b1k2r/p1q1bpp1/1pp1pn1p/8/3P1B1Q/3B1N2/PPP2PPP/R4RK1 b kq - 3 14"
    val positionHistory = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .toOption
      .getOrElse(fail("expected explicit recovery-chain root history"))
    val moves = List(
      "f6d5", "f4c7", "e7h4", "c7b6", "h4f2", "f1f2", "a7b6"
    )
    normalizedMaterialLine(
      id = "recovery-reference",
      fen = fen,
      positionHistory = positionHistory,
      moves = moves,
      sideToMove = Black,
      role = LineNodeRole.BestReference,
      rank = 1
    )

  private def immediateRecaptureLine(): LineFactEvidence =
    val initialFen = "r1b2rk1/5ppp/1qp2n2/p2p4/1b6/N3P1P1/PPQB1PBP/R1R3K1 w - - 0 13"
    val fen = "r1b2rk1/5ppp/1qp2n2/p2p4/1b6/4P1P1/PPQB1PBP/RNR3K1 b - - 1 13"
    val positionHistory = CanonicalPositionHistory
      .from(initialFen, List("a3b1"), fen)
      .toOption
      .getOrElse(fail("expected explicit immediate-recapture predecessor history"))
    normalizedMaterialLine(
      id = "cs07-played-recapture",
      fen = fen,
      positionHistory = positionHistory,
      moves = List("b4d2", "b1d2"),
      sideToMove = Black,
      role = LineNodeRole.Played,
      rank = 2
    )

  private def normalizedMaterialLine(
      id: String,
      fen: String,
      positionHistory: CanonicalPositionHistory,
      moves: List[String],
      sideToMove: Color,
      role: LineNodeRole,
      rank: Int
  ): LineFactEvidence =
    val replay = legalReplay(fen, moves)
    val moveRefs = replay.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.head
    val line = LineNodeRef(id, first.uci, rank, role)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    val lineHistory = positionHistory
      .extend(moves)
      .toOption
      .getOrElse(fail("expected canonical material line history"))
    val materialSummary = CandidateLineAssembler
      .lineMaterialSummary(positionHistory, lineHistory)
      .getOrElse(fail("expected a complete material summary"))
    LineFactNormalizer
      .fromValidatedLine(
        id = s"$id-evidence",
        lineRef = line,
        facts = facts,
        position = PositionNodeRef(fen, 0, Some(sideToMove)),
        scope = role.scope,
        materialSummary = Some(materialSummary)
      )
      .payload match
      case payload: LineFactEvidence => payload
      case _                         => fail("expected normalized line evidence")

  private def actorLossLine(
      id: String,
      fen: String,
      moves: List[String],
      actorRole: String
  ): LineFactEvidence =
    val destination = moves.head.slice(2, 4)
    materialLine(
      id = id,
      fen = fen,
      moves = moves,
      sideToMove = White,
      captures = List(capture(moves(1), 1, Black, Bishop.name, actorRole, destination, 100)),
      consequence = LineConsequence(
        kind = LineConsequenceKind.MaterialLoss,
        lineMoves = List(moves(1)),
        proofSignal = false,
        eventMove = Some(moves(1)),
        rootMove = None,
        rootSide = Some(White),
        beneficiary = Some(Black)
      )
    )

  private def continuationOnlyMaterialLine(): LineFactEvidence =
    val fen = "8/8/8/8/2k1P3/8/8/K7 b - - 0 1"
    val moves = List("c4d4", "a1b1", "d4e4")
    val gain = capture("d4e4", 2, Black, King.name, Pawn.name, "e4", 100)
    val line = LineNodeRef("continuation-only-material", moves.head, 1, LineNodeRole.BestReference)
    LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = Black,
        captures = List(gain),
        netCaptureCpForMover = 100,
        maxGainCpForMover = 100,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = legalReplay(fen, moves),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.MaterialGain,
        lineMoves = List(gain.moveUci),
        proofSignal = true,
        eventMove = Some(gain.moveUci),
        rootMove = Some(moves.head),
        rootSide = Some(Black),
        beneficiary = Some(Black),
        materialOutcome = RootOwnedMaterialEventSalience.from(gain).map(event =>
          RootOwnedMaterialOutcome(event, Black, durableNetCp = 100)
        )
      ))
    )

  private def rootTriggeredMaterialSequenceLine(): LineFactEvidence =
    val fen = "4k3/8/8/8/1r2r3/8/4Q1B1/K7 w - - 0 1"
    val moves = List("e2e4", "b4e4", "g2e4")
    val trigger = capture("e2e4", 0, White, Queen.name, "Rook", "e4", 500)
    val response = capture("b4e4", 1, Black, "Rook", Queen.name, "e4", 900, recapture = true)
    val payoff = capture("g2e4", 2, White, Bishop.name, "Rook", "e4", 500, recapture = true)
    val line = LineNodeRef("root-triggered-material", moves.head, 1, LineNodeRole.BestReference)
    LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = List(trigger, response, payoff),
        netCaptureCpForMover = 100,
        maxGainCpForMover = 1000,
        maxLossCpForMover = 900,
        hasRecaptureChain = true,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = legalReplay(fen, moves),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.MaterialGain,
        lineMoves = moves,
        proofSignal = true,
        eventMove = Some(payoff.moveUci),
        rootMove = Some(moves.head),
        rootSide = Some(White),
        beneficiary = Some(White),
        materialOutcome = RootOwnedMaterialEventSalience.from(payoff).map(event =>
          RootOwnedMaterialOutcome(event, White, durableNetCp = 100)
        )
      ))
    )

  private def longForcedCaptureMaterialSequenceLine(): LineFactEvidence =
    val moves = List("e2e4", "b4e4", "g2e4", "h7e4", "f2e4")
    ownedMaterialGainLine(
      id = "long-forced-capture-material",
      fen = "k7/7b/8/8/1r2r3/8/4QNB1/K7 w - - 0 1",
      moves = moves,
      captures = List(
        capture(moves(0), 0, White, Queen.name, Rook.name, "e4", 500),
        capture(moves(1), 1, Black, Rook.name, Queen.name, "e4", 900, recapture = true),
        capture(moves(2), 2, White, Bishop.name, Rook.name, "e4", 500, recapture = true),
        capture(moves(3), 3, Black, Bishop.name, Bishop.name, "e4", 300, recapture = true),
        capture(moves(4), 4, White, Knight.name, Bishop.name, "e4", 300, recapture = true)
      ),
      durableNetCp = 100
    )

  private def longForcedCheckMaterialSequenceLine(): LineFactEvidence =
    val moves = List("d1a4", "e8d8", "a4a7", "a8a7", "g1a7")
    ownedMaterialGainLine(
      id = "long-forced-check-material",
      fen = "q3k3/p7/8/8/8/8/8/3QK1B w - - 0 1",
      moves = moves,
      captures = List(
        capture(moves(2), 2, White, Queen.name, Pawn.name, "a7", 100),
        capture(moves(3), 3, Black, Queen.name, Queen.name, "a7", 900, recapture = true),
        capture(moves(4), 4, White, Bishop.name, Queen.name, "a7", 900, recapture = true)
      ),
      durableNetCp = 100
    )

  private def longRootActorLossMaterialSequenceLine(): LineFactEvidence =
    val moves = List("e2e4", "b4e4", "g2e4", "h7e4", "f2e4")
    ownedMaterialGainLine(
      id = "long-root-actor-loss-material",
      fen = "k7/7b/8/8/1q6/8/4PNB1/K7 w - - 0 1",
      moves = moves,
      captures = List(
        capture(moves(1), 1, Black, Queen.name, Pawn.name, "e4", 100),
        capture(moves(2), 2, White, Bishop.name, Queen.name, "e4", 900, recapture = true),
        capture(moves(3), 3, Black, Bishop.name, Bishop.name, "e4", 300, recapture = true),
        capture(moves(4), 4, White, Knight.name, Bishop.name, "e4", 300, recapture = true)
      ),
      durableNetCp = 800
    )

  private def unrelatedCheckFollowUpMaterialLine(): LineFactEvidence =
    val moves = List("d1a4", "e8d8", "g2b7")
    ownedMaterialGainLine(
      id = "unrelated-check-follow-up",
      fen = "4k3/1p6/8/8/8/8/6B1/K2Q4 w - - 0 1",
      moves = moves,
      captures = List(capture(moves(2), 2, White, Bishop.name, Pawn.name, "b7", 100)),
      durableNetCp = 100
    )

  private def placementBeforeClearanceMaterialLine(): LineFactEvidence =
    val moves = List("b2a2", "h8g8", "a4b5")
    ownedMaterialGainLine(
      id = "placement-before-clearance-material",
      fen = "7k/8/8/1p6/P7/8/1R6/7K w - - 0 1",
      moves = moves,
      captures = List(capture(moves(2), 2, White, Pawn.name, Pawn.name, "b5", 100)),
      durableNetCp = 100
    )

  private def ownedMaterialGainLine(
      id: String,
      fen: String,
      moves: List[String],
      captures: List[LineMaterialCapture],
      durableNetCp: Int
  ): LineFactEvidence =
    val payoff = captures.lastOption.getOrElse(fail("a material-gain fixture needs a payoff capture"))
    val line = LineNodeRef(id, moves.head, 1, LineNodeRole.BestReference)
    LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = captures,
        netCaptureCpForMover = durableNetCp,
        maxGainCpForMover = captures.filter(_.side == White).map(_.valueCp).sum,
        maxLossCpForMover = captures.filter(_.side == Black).map(_.valueCp).sum,
        hasRecaptureChain = captures.exists(_.recapture),
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = legalReplay(fen, moves),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.MaterialGain,
        lineMoves = moves,
        proofSignal = true,
        eventMove = Some(payoff.moveUci),
        rootMove = Some(moves.head),
        rootSide = Some(White),
        beneficiary = Some(White),
        materialOutcome = RootOwnedMaterialEventSalience.from(payoff).map(event =>
          RootOwnedMaterialOutcome(event, White, durableNetCp)
        )
      ))
    )

  private def continuationOnlyTerminalLine(
      id: String,
      fen: String,
      moves: List[String],
      kind: LineConsequenceKind
  ): LineFactEvidence =
    val line = LineNodeRef(id, moves.head, 1, LineNodeRole.BestReference)
    LineFactEvidence(
      line = line,
      replay = legalReplay(fen, moves),
      consequences = List(LineConsequence(
        kind = kind,
        lineMoves = List(moves.last),
        proofSignal = true,
        eventMove = Some(moves.last),
        rootMove = Some(moves.head),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )

  private def forcedPromotionLine(): LineFactEvidence =
    val moves = List("g6g7", "h8h7", "g7g8q")
    LineFactEvidence(
      line = LineNodeRef("forced-promotion", moves.head, 1, LineNodeRole.BestReference),
      replay = legalReplay("7k/8/5KP1/8/8/8/8/8 w - - 0 1", moves),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Promotion,
        lineMoves = List(moves.last),
        proofSignal = true,
        eventMove = Some(moves.last),
        rootMove = None,
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )

  private def rootTriggeredEndgameConversionLine(
      status: LineEndgameTechniqueHorizonStatus
  ): LineFactEvidence =
    val move = "a7a8q"
    LineFactEvidence(
      line = LineNodeRef(s"root-triggered-${status.toString.toLowerCase}", move, 1, LineNodeRole.BestReference),
      replay = legalReplay("4k3/P7/8/8/8/8/8/4K3 w - - 0 1", List(move)),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Promotion,
        lineMoves = List(move),
        proofSignal = true,
        eventMove = Some(move),
        rootMove = Some(move),
        rootSide = Some(White),
        beneficiary = Some(White)
      )),
      endgameHorizons = List(LineEndgameTechniqueHorizon(
        pattern = "Lucena",
        rookPattern = None,
        techniqueSide = White,
        entryPlyOffset = 0,
        terminalPlyOffset = 0,
        status = status,
        triggerMove = Some(move),
        requiredSquares = List("a8")
      ))
    )

  private def materialLine(
      id: String,
      fen: String,
      moves: List[String],
      sideToMove: Color,
      captures: List[LineMaterialCapture],
      consequence: LineConsequence
  ): LineFactEvidence =
    val line = LineNodeRef(id, moves.head, 1, LineNodeRole.BestReference)
    LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = sideToMove,
        captures = captures,
        netCaptureCpForMover = 0,
        maxGainCpForMover = 900,
        maxLossCpForMover = 900,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = legalReplay(fen, moves),
      consequences = List(consequence)
    )

  private def rootEventLine(
      id: String,
      fen: String,
      move: String,
      eventKind: LineEventKind,
      pieceRole: String,
      targetRole: String,
      targetSquare: String,
      materialCapture: Option[LineMaterialCapture] = None,
      consequences: List[LineConsequence] = Nil
  ): LineFactEvidence =
    val line = LineNodeRef(id, move, 1, LineNodeRole.BestReference)
    val material = materialCapture.map(capture =>
      LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = capture.valueCp,
        maxGainCpForMover = capture.valueCp,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )
    LineFactEvidence(
      line = line,
      material = material,
      replay = legalReplay(fen, List(move)),
      events = List(LineMoveEvent(
        kind = eventKind,
        moveUci = move,
        plyOffset = 0,
        side = Some(White),
        pieceRole = Some(EvidencePieceRole(pieceRole)),
        targetRole = Some(EvidencePieceRole(targetRole)),
        square = Some(EvidenceSquare(targetSquare))
      )),
      consequences = consequences
    )

  private def projectionBindings(
      line: LineFactEvidence,
      causeKind: RelativeCauseKind,
      mover: Color,
      liability: Boolean = false
  ): List[EvidenceObjectBinding] =
    projectionChannels(line, causeKind, mover, liability).map(_.binding)

  private def projectionChannels(
      line: LineFactEvidence,
      causeKind: RelativeCauseKind,
      mover: Color,
      liability: Boolean = false
  ): List[DirectCauseChannel] =
    val position = PositionNodeRef(line.lineReplaySteps.head.fenBefore, 0, Some(mover))
    val projectedLine =
      if liability then line.copy(line = line.line.copy(id = s"${line.line.id}-candidate", rank = 2, role = LineNodeRole.Played))
      else line
    val referenceLine =
      if liability then
        val referenceRoot = if line.line.rootMove == "e2e3" then "e2e4" else "e2e3"
        LineNodeRef(s"${line.line.id}-reference", referenceRoot, 1, LineNodeRole.BestReference)
      else line.line
    val candidateRoot =
      if liability then projectedLine.line.rootMove
      else if line.line.rootMove == "a2a3" then "h2h3" else "a2a3"
    val candidateLine =
      if liability then projectedLine.line
      else LineNodeRef(s"${line.line.id}-candidate", candidateRoot, 2, LineNodeRole.Played)
    val comparisonRef = EvidenceRef(
      id = s"${line.line.id}-comparison",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      position = position,
      line = Some(candidateLine),
      scope = candidateLine.role.scope,
      confidence = EvidenceConfidence.EngineBacked
    )
    val referenceScore = if mover.white then 300 else -300
    val comparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparison = EvalComparison.fromLines(
        mover,
        CandidateLineNode(
          referenceLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(referenceLine.rootMove), referenceScore, depth = 18)),
          lineRef(s"${line.line.id}-reference-eval", referenceLine, position)
        ),
        CandidateLineNode(
          candidateLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(candidateLine.rootMove), -referenceScore, depth = 18)),
          lineRef(s"${line.line.id}-candidate-eval", candidateLine, position)
        )
      ).get,
      verdictConfidence = VerdictConfidence.EngineBacked
    )
    val proofRef = lineRef(s"${line.line.id}-proof", projectedLine.line, position)
    val cause = RelativeCauseFact(
      kind = causeKind,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proofRef),
      sourceSide = if liability then RelativeCauseSourceSide.Candidate else RelativeCauseSourceSide.Reference,
      attribution = CauseAttribution(
        if liability then CauseAttributionKind.CandidateAllowsLiability
        else CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(proofRef)
        )
      ))
    )
    val eventLine = if liability then candidateLine else referenceLine
    val causeRef = EvidenceRef(
      id = s"${line.line.id}-cause",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = position,
      line = Some(eventLine),
      scope = eventLine.role.scope,
      confidence = EvidenceConfidence.EngineBacked
    )
    val graph = ExplicitCauseAdmissionTestSupport.graph(List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(proofRef, projectedLine),
      EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
    ))
    val registeredCause = ExplicitCauseAdmissionTestSupport
      .registeredCause(causeRef, graph)
      .getOrElse(fail(s"expected registered Cause ${causeRef.id}"))
    RelativeCauseConstructionAdmission.admittedDirectChannels(registeredCause, graph)

  private def conversionDrafts(
      line: LineFactEvidence,
      mover: Color
  ): List[RelativeCauseDraft] =
    val position = PositionNodeRef(line.lineReplaySteps.head.fenBefore, 0, Some(mover))
    val candidateLine = LineNodeRef(s"${line.line.id}-candidate-draft", "a2a3", 2, LineNodeRole.Played)
    val comparisonRef = EvidenceRef(
      id = s"${line.line.id}-draft-comparison",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      position = position,
      line = Some(candidateLine),
      scope = candidateLine.role.scope,
      confidence = EvidenceConfidence.EngineBacked
    )
    val referenceScore = if mover.white then 300 else -300
    val comparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = line.line,
      candidateLine = candidateLine,
      comparison = EvalComparison.fromLines(
        mover,
        CandidateLineNode(
          line.line,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(line.line.rootMove), referenceScore, depth = 18)),
          lineRef(s"${line.line.id}-draft-reference-eval", line.line, position)
        ),
        CandidateLineNode(
          candidateLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(candidateLine.rootMove), -referenceScore, depth = 18)),
          lineRef(s"${line.line.id}-draft-candidate-eval", candidateLine, position)
        )
      ).get,
      verdictConfidence = VerdictConfidence.EngineBacked
    )
    RelativeCauseDraftPlanner
      .drafts(
        RelativeCauseSignalProfile.from(
          comparison,
          List(lineRecord(s"${line.line.id}-draft-proof", line, mover)),
          Nil,
          Nil
        ),
        EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison))
      )
      .filter(_.kind == RelativeCauseKind.ConversionSecured)

  private def objectKeys(
      objects: List[ConcreteChessObject],
      kind: EvidenceObjectKind
  ): Set[String] =
    objects.collect { case obj if obj.kind == kind => obj.key.toLowerCase }.toSet

  private def legalReplay(fen: String, moves: List[String]): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(fen -> List.empty[LineReplayStep]) {
      case ((fenBefore, steps), (move, index)) =>
        val fenAfter = PrincipalVariationEvidence
          .legalFenAfter(fenBefore, move)
          .getOrElse(fail(s"expected legal test move $move after $fenBefore"))
        fenAfter -> (steps :+ LineReplayStep(index, move, fenBefore, fenAfter))
    }._2

  private def capture(
      move: String,
      ply: Int,
      side: Color,
      attacker: String,
      captured: String,
      square: String,
      valueCp: Int,
      recapture: Boolean = false
  ): LineMaterialCapture =
    LineMaterialCapture(
      moveUci = move,
      plyOffset = ply,
      side = side,
      attackerRole = EvidencePieceRole(attacker),
      capturedRole = EvidencePieceRole(captured),
      square = EvidenceSquare(square),
      valueCp = valueCp,
      recapture = recapture
    )

  private def lineRecord(
      id: String,
      line: LineFactEvidence,
      sideToMove: Color = Black
  ): EvidenceRecord =
    val position = PositionNodeRef(line.lineReplaySteps.head.fenBefore, 0, Some(sideToMove))
    EvidenceRecord(lineRef(id, line.line, position), line)

  private def motifRecord(
      id: String,
      recordLine: LineNodeRef,
      eventLine: LineNodeRef
  ): EvidenceRecord =
    val position = PositionNodeRef("4k3/8/8/8/8/8/8/3QK3 w - - 0 1", 0, Some(White))
    val target = Square.fromKey("e8").getOrElse(fail("expected e8"))
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.MoveMotifProducer,
        layer = EvidenceLayer.MoveMotif,
        position = position,
        line = Some(recordLine),
        scope = recordLine.role.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      MoveMotifEvidence(MoveMotifEvent.fromMotif(
        eventLine.rootMove,
        Motif.Check(Queen, target, Motif.CheckType.Normal, White, 0, Some(eventLine.rootMove))
      ))
    )

  private def evalRecord(id: String, line: LineNodeRef): EvidenceRecord =
    val position = PositionNodeRef("4k3/8/8/8/8/8/8/4K3 w - - 0 1", 0, Some(White))
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.EngineEvalProducer,
        layer = EvidenceLayer.Eval,
        position = position,
        line = Some(line),
        scope = line.role.scope,
        confidence = EvidenceConfidence.EngineBacked
      ),
      CandidateLineEvaluationEvidence(
        line,
        lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(
          EngineLine(List(line.rootMove), scoreCp = 900, mate = None, depth = 18)
        )
      )
    )

  private def lineRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
