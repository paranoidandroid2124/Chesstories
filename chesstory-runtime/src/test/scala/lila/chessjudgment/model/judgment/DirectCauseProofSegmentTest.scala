package lila.chessjudgment.model.judgment

import chess.{ Black, White }
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class DirectCauseProofSegmentTest extends munit.FunSuite:

  private val position = PositionNodeRef(
    "4k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val lineRef = LineNodeRef("reference", "e2e4", 1, LineNodeRole.BestReference)
  private val source = EvidenceRef(
    "line-proof",
    EvidenceProducer.LegalLineProducer,
    EvidenceLayer.Line,
    position,
    Some(lineRef),
    EvidenceScope.BestLine,
    EvidenceConfidence.LegalReplayVerified
  )

  test("line episode exposes exactly its owned replay prefix with root-relative offsets"):
    val moves = List("e2e4", "e8e7", "e4e7")
    val line = LineFactEvidence(
      lineRef,
      replay = moves.zipWithIndex.map { case (move, ply) =>
        LineReplayStep(ply, move, s"before-$ply", s"after-$ply")
      }
    )
    val episode = RootOwnedCausalEpisode(
      line = lineRef,
      actor = RootCausalActor(
        lineRef.rootMove,
        EvidencePieceRole("queen"),
        White,
        EvidenceSquare("e2"),
        EvidenceSquare("e4")
      ),
      target = EvidenceSquare("e7"),
      links = List(RootCausalLink(
        RootCausalLinkKind.ContinuousLineAccess,
        lineRef.rootMove,
        moves.last,
        EvidenceSquare("e7")
      )),
      consequence = LineConsequence(
        LineConsequenceKind.MaterialGain,
        moves,
        proofSignal = true,
        eventMove = Some(moves.last),
        rootMove = Some(lineRef.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ),
      eventPlyOffset = 2,
      chainMoves = moves
    )

    val segment = DirectCauseProofSegment
      .from(RootOwnedEffectProof.LineEpisode(source, line, episode))
      .getOrElse(fail("verified line episode must expose its exact prefix"))

    assertEquals(segment.terminalRelation, DirectCauseProofTerminalRelation.ProducesLineConsequence)
    assertEquals(segment.steps.map(_.plyOffset), List(0, 1, 2))
    assertEquals(segment.steps.map(_.moveUci), moves)
    assertEquals(
      segment.steps.map(_.role),
      List(
        DirectCauseProofStepRole.RootAction,
        DirectCauseProofStepRole.CausalLink,
        DirectCauseProofStepRole.TerminalEvent
      )
    )

    List(
      episode.copy(chainMoves = moves.take(2)),
      episode.copy(chainMoves = moves :+ "e7e8")
    ).foreach: malformed =>
      assertEquals(
        DirectCauseProofSegment.from(RootOwnedEffectProof.LineEpisode(source, line, malformed)),
        None
      )

    val distortedReplay = line.copy(replay = line.lineReplaySteps.updated(
      1,
      line.lineReplaySteps(1).copy(ply = 4)
    ))
    assertEquals(
      DirectCauseProofSegment.from(RootOwnedEffectProof.LineEpisode(source, distortedReplay, episode)),
      None
    )

  test("relation proof exposes only its owned root move and never its witness continuation"):
    val relation = RelationFactEvidence
      .from(
        RelationWitnessDetail.Zwischenzug(
          intermediateMove = lineRef.rootMove,
          expectedRecaptureSquare = EvidenceSquare("d5"),
          checkingPieceSquare = EvidenceSquare("e4"),
          checkingPieceRole = EvidencePieceRole("queen"),
          checkedKingSquare = EvidenceSquare("e8"),
          threatType = RelationThreatSignal.Check
        ),
        List(lineRef.rootMove, "e8e7", "e4d5")
      )
      .getOrElse(fail("relation fixture must be typed"))

    val segment = DirectCauseProofSegment
      .from(RootOwnedEffectProof.RootRelation(source, relation))
      .getOrElse(fail("root-owned relation must expose its root action"))

    assertEquals(segment.terminalRelation, DirectCauseProofTerminalRelation.InstantiatesRelation)
    assertEquals(segment.steps.map(_.moveUci), List(lineRef.rootMove))

  test("unsafe line mismatch yields no segment without inventing a replacement step"):
    val line = LineFactEvidence(
      lineRef,
      replay = List(LineReplayStep(0, lineRef.rootMove, "before", "after"))
    )
    val episode = RootOwnedCausalEpisode(
      line = lineRef,
      actor = RootCausalActor(
        lineRef.rootMove,
        EvidencePieceRole("queen"),
        White,
        EvidenceSquare("e2"),
        EvidenceSquare("e4")
      ),
      target = EvidenceSquare("e8"),
      links = List(RootCausalLink(
        RootCausalLinkKind.ImmediateRootAction,
        lineRef.rootMove,
        lineRef.rootMove,
        EvidenceSquare("e8")
      )),
      consequence = LineConsequence(
        LineConsequenceKind.ImmediateReplyCheck,
        List(lineRef.rootMove),
        proofSignal = true,
        eventMove = Some("d1h5")
      ),
      eventPlyOffset = 0,
      chainMoves = List(lineRef.rootMove)
    )

    assertEquals(
      DirectCauseProofSegment.from(RootOwnedEffectProof.LineEpisode(source, line, episode)),
      None
    )

  test("canonical public channels collapse exact root capture mate and promotion views into their episodes"):
    val material = immediateMaterialPair()
    val mate = immediateMateChannels(includeCheck = false)
    val promotion = immediatePromotionPair()

    List(material, mate, promotion).foreach { pair =>
      assertNotEquals(pair.eventChannel.causalSignature, pair.episodeChannel.causalSignature)
      val canonical = EvidenceObjectBinding.canonicalCauseChannels(
        List(pair.eventChannel, pair.episodeChannel)
      )

      assertEquals(canonical.size, 1)
      assert(canonical.head.rootOwnedProof.exists {
        case _: RootOwnedEffectProof.LineEpisode => true
        case _                                   => false
      })
    }
    assertEquals(DirectCauseMeasuredEffect.fromChannel(material.eventChannel), None)
    assert(DirectCauseMeasuredEffect.fromChannel(material.episodeChannel).nonEmpty)
    assertEquals(DirectCauseMeasuredEffect.fromChannel(mate.eventChannel), None)
    assert(DirectCauseMeasuredEffect.fromChannel(mate.episodeChannel).nonEmpty)

  test("actual Cause projection exposes one complete episode for root material mate and promotion effects"):
    val materialChannels = projectedChannels(
      immediateMaterialPair(),
      RelativeCauseKind.MissedTacticalResource
    )
    val mateChannels = projectedChannels(
      immediateMateChannels(includeCheck = false),
      RelativeCauseKind.KingForcing
    )
    val promotionChannels = projectedChannels(
      immediatePromotionPair(),
      RelativeCauseKind.MissedTacticalResource
    )

    List(materialChannels, mateChannels, promotionChannels).foreach { channels =>
      assertEquals(channels.size, 1)
      assert(channels.head.rootOwnedProof.exists {
        case _: RootOwnedEffectProof.LineEpisode => true
        case _                                   => false
      })
    }
    assert(DirectCauseMeasuredEffect.fromChannel(materialChannels.head).nonEmpty)
    assert(DirectCauseMeasuredEffect.fromChannel(mateChannels.head).nonEmpty)
    assertEquals(DirectCauseMeasuredEffect.fromChannel(promotionChannels.head), None)

  test("canonical public channels preserve delayed recovery distinct results and enriched mechanisms"):
    val delayed = delayedMaterialPair()
    assertEquals(
      EvidenceObjectBinding
        .canonicalCauseChannels(List(delayed.eventChannel, delayed.episodeChannel))
        .size,
      2
    )

    List(
      LineConsequenceKind.RecaptureSequence,
      LineConsequenceKind.RecoveryWindow
    ).foreach { consequenceKind =>
      val recovery = recapturePair(consequenceKind)
      assertEquals(
        EvidenceObjectBinding
          .canonicalCauseChannels(List(recovery.eventChannel, recovery.episodeChannel))
          .size,
        2
      )
    }

    val mate = immediateMateChannels(includeCheck = true)
    val withDifferentResult = EvidenceObjectBinding.canonicalCauseChannels(
      mate.checkChannel.toList ++ List(mate.eventChannel, mate.episodeChannel)
    )
    assertEquals(withDifferentResult.size, 2)
    assert(withDifferentResult.exists(_.rootOwnedProof.exists {
      case RootOwnedEffectProof.RootLineEvent(_, _, event) => event.kind == LineEventKind.Check
      case _                                               => false
    }))
    assert(withDifferentResult.exists(_.rootOwnedProof.exists {
      case _: RootOwnedEffectProof.LineEpisode => true
      case _                                   => false
    }))

    val material = immediateMaterialPair()
    val enrichedEvent = material.eventChannel.copy(
      binding = material.eventChannel.binding.copy(
        mechanism = material.eventChannel.binding.mechanism :+
          ConcreteChessObject(EvidenceObjectKind.Mechanism, "fork")
      )
    )
    val enrichedCanonical = EvidenceObjectBinding.canonicalCauseChannels(
      List(enrichedEvent, material.episodeChannel)
    )
    assertEquals(enrichedCanonical.size, 2)
    assert(enrichedCanonical.exists(_.causalSignature == enrichedEvent.causalSignature))

  private final case class LineEffectPair(
      eventChannel: DirectCauseChannel,
      episodeChannel: DirectCauseChannel,
      checkChannel: Option[DirectCauseChannel] = None
  )

  private def immediateMaterialPair(): LineEffectPair =
    val fen = "4k3/8/8/8/8/8/4Q2r/4K3 w - - 0 1"
    val line = LineNodeRef("material-root", "e2h2", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List(line.rootMove))
    val capture = LineMaterialCapture(
      moveUci = line.rootMove,
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("queen"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("h2"),
      valueCp = 500,
      recapture = false
    )
    val event = LineMoveEvent(
      LineEventKind.Capture,
      line.rootMove,
      plyOffset = 0,
      side = Some(White),
      pieceRole = Some(EvidencePieceRole("queen")),
      targetRole = Some(EvidencePieceRole("rook")),
      square = Some(EvidenceSquare("h2"))
    )
    val consequence = LineConsequence(
      LineConsequenceKind.MaterialGain,
      List(line.rootMove),
      proofSignal = true,
      eventMove = Some(line.rootMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = capture.moveUci,
          plyOffset = capture.plyOffset,
          capturedRole = capture.capturedRole,
          square = capture.square,
          targetValueCp = capture.valueCp
        ),
        beneficiary = White,
        durableNetCp = 500
      ))
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(materialSummary(List(capture), netCp = 500)),
      replay = replay,
      events = List(event),
      consequences = List(consequence)
    )
    effectPair(fen, "material-source", payload, event, consequence)

  private def delayedMaterialPair(): LineEffectPair =
    val fen = "k7/7b/8/8/1r2r3/8/4QNB1/K7 w - - 0 1"
    val moves = List("e2e4", "b4e4", "g2e4", "h7e4", "f2e4")
    val line = LineNodeRef("delayed-material", moves.head, 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, moves)
    val rootCapture = LineMaterialCapture(
      moves.head,
      0,
      White,
      EvidencePieceRole("queen"),
      EvidencePieceRole("rook"),
      EvidenceSquare("e4"),
      500,
      recapture = false
    )
    val response = LineMaterialCapture(
      moves(1),
      1,
      Black,
      EvidencePieceRole("rook"),
      EvidencePieceRole("queen"),
      EvidenceSquare("e4"),
      900,
      recapture = true
    )
    val forcedRecapture = LineMaterialCapture(
      moves(2),
      2,
      White,
      EvidencePieceRole("bishop"),
      EvidencePieceRole("rook"),
      EvidenceSquare("e4"),
      500,
      recapture = true
    )
    val continuationCapture = LineMaterialCapture(
      moves(3),
      3,
      Black,
      EvidencePieceRole("bishop"),
      EvidencePieceRole("bishop"),
      EvidenceSquare("e4"),
      300,
      recapture = true
    )
    val delayedCapture = LineMaterialCapture(
      moves.last,
      4,
      White,
      EvidencePieceRole("knight"),
      EvidencePieceRole("bishop"),
      EvidenceSquare("e4"),
      300,
      recapture = true
    )
    val event = LineMoveEvent(
      LineEventKind.Capture,
      moves.head,
      plyOffset = 0,
      side = Some(White),
      pieceRole = Some(EvidencePieceRole("queen")),
      targetRole = Some(EvidencePieceRole("rook")),
      square = Some(EvidenceSquare("e4"))
    )
    val consequence = LineConsequence(
      LineConsequenceKind.MaterialGain,
      moves,
      proofSignal = true,
      eventMove = Some(moves.last),
      rootMove = Some(moves.head),
      rootSide = Some(White),
      beneficiary = Some(White),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = delayedCapture.moveUci,
          plyOffset = delayedCapture.plyOffset,
          capturedRole = delayedCapture.capturedRole,
          square = delayedCapture.square,
          targetValueCp = delayedCapture.valueCp
        ),
        beneficiary = White,
        durableNetCp = 100
      ))
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(materialSummary(
        List(rootCapture, response, forcedRecapture, continuationCapture, delayedCapture),
        netCp = 100,
        hasRecaptureChain = true
      )),
      replay = replay,
      events = List(event),
      consequences = List(consequence)
    )
    effectPair(fen, "delayed-material-source", payload, event, consequence)

  private def recapturePair(consequenceKind: LineConsequenceKind): LineEffectPair =
    val fen = "4k3/8/8/8/8/8/4Q2r/4K3 w - - 0 1"
    val line = LineNodeRef(
      s"${consequenceKind.toString.toLowerCase}-root",
      "e2h2",
      1,
      LineNodeRole.BestReference
    )
    val replay = legalReplay(fen, List(line.rootMove))
    val capture = LineMaterialCapture(
      line.rootMove,
      0,
      White,
      EvidencePieceRole("queen"),
      EvidencePieceRole("rook"),
      EvidenceSquare("h2"),
      500,
      recapture = true
    )
    val event = LineMoveEvent(
      LineEventKind.Recapture,
      line.rootMove,
      plyOffset = 0,
      side = Some(White),
      pieceRole = Some(EvidencePieceRole("queen")),
      targetRole = Some(EvidencePieceRole("rook")),
      square = Some(EvidenceSquare("h2"))
    )
    val consequence = LineConsequence(
      consequenceKind,
      List(line.rootMove),
      proofSignal = true,
      eventMove = Some(line.rootMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(materialSummary(
        List(capture),
        netCp = 500,
        hasRecaptureChain = true,
        hasRecoveryWindow = consequenceKind == LineConsequenceKind.RecoveryWindow
      )),
      replay = replay,
      events = List(event),
      consequences = List(consequence)
    )
    effectPair(fen, s"${line.id}-source", payload, event, consequence)

  private def immediateMateChannels(includeCheck: Boolean): LineEffectPair =
    val fen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val line = LineNodeRef("mate-root", "g6g7", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List(line.rootMove))
    val mateEvent = LineMoveEvent(
      LineEventKind.Mate,
      line.rootMove,
      plyOffset = 0,
      side = Some(White),
      pieceRole = Some(EvidencePieceRole("queen")),
      targetRole = Some(EvidencePieceRole("king")),
      square = Some(EvidenceSquare("h8"))
    )
    val checkEvent = mateEvent.copy(kind = LineEventKind.Check)
    val consequence = LineConsequence(
      LineConsequenceKind.Mate,
      List(line.rootMove),
      proofSignal = true,
      eventMove = Some(line.rootMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(materialSummary(Nil, netCp = 0)),
      replay = replay,
      events = mateEvent :: Option.when(includeCheck)(checkEvent).toList,
      consequences = List(consequence)
    )
    val pair = effectPair(fen, "mate-source", payload, mateEvent, consequence)
    if !includeCheck then pair
    else
      val source = proofSource("mate-source", fen, line)
      val actor = RootCausalActor
        .fromLineFact(payload, line.rootMove)
        .getOrElse(fail("expected the mate root actor"))
      pair.copy(checkChannel = Some(rootEventChannel(source, payload, actor, checkEvent)))

  private def immediatePromotionPair(): LineEffectPair =
    val fen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
    val line = LineNodeRef("promotion-root", "a7a8q", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List(line.rootMove))
    val event = LineMoveEvent(
      LineEventKind.Promotion,
      line.rootMove,
      plyOffset = 0,
      side = Some(White),
      pieceRole = Some(EvidencePieceRole("pawn")),
      targetRole = Some(EvidencePieceRole("queen")),
      square = Some(EvidenceSquare("a8"))
    )
    val consequence = LineConsequence(
      LineConsequenceKind.Promotion,
      List(line.rootMove),
      proofSignal = true,
      eventMove = Some(line.rootMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(materialSummary(Nil, netCp = 0).copy(promotionGainCpForMover = 800)),
      replay = replay,
      events = List(event),
      consequences = List(consequence)
    )
    effectPair(fen, "promotion-source", payload, event, consequence)

  private def effectPair(
      fen: String,
      sourceId: String,
      payload: LineFactEvidence,
      event: LineMoveEvent,
      consequence: LineConsequence
  ): LineEffectPair =
    val source = proofSource(sourceId, fen, payload.line)
    val actor = RootCausalActor
      .fromLineFact(payload, payload.line.rootMove)
      .getOrElse(fail("expected a legal root actor"))
    val episode = payload
      .rootOwnedCausalEpisodes(payload.line.rootMove)
      .find(_.consequence == consequence)
      .getOrElse(fail(s"expected an exact ${consequence.kind} episode"))
    LineEffectPair(
      eventChannel = rootEventChannel(source, payload, actor, event),
      episodeChannel = lineEpisodeChannel(source, payload, episode)
    )

  private def projectedChannels(
      pair: LineEffectPair,
      kind: RelativeCauseKind
  ): List[DirectCauseChannel] =
    val (source, payload) = pair.episodeChannel.rootOwnedProof match
      case Some(RootOwnedEffectProof.LineEpisode(exactSource, exactPayload, _)) =>
        exactSource -> exactPayload
      case _ =>
        fail("expected a line-episode projection fixture")
    val played = LineNodeRef(
      s"${payload.line.id}-played",
      payload.line.rootMove,
      payload.line.rank + 1,
      LineNodeRole.Played
    )
    val comparisonRef = EvidenceRef(
      s"${source.id}-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      source.position,
      Some(played),
      EvidenceScope.Counterfactual,
      EvidenceConfidence.EngineBacked
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      payload.line,
      played,
      EvalComparison(White, -12.0, MoveChoiceVerdict.Blunder)
    )
    val cause = RelativeCauseFact(
      kind,
      comparisonRef,
      List(source),
      RelativeCauseSourceSide.Reference,
      CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(source)
        )
      ))
    )
    val graph = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(source, payload)
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(cause, graph)

  private def rootEventChannel(
      source: EvidenceRef,
      payload: LineFactEvidence,
      actor: RootCausalActor,
      event: LineMoveEvent
  ): DirectCauseChannel =
    DirectCauseChannel(
      binding = EvidenceObjectBinding(
        source = source,
        actor = actorObjects(actor),
        target = effectTargetObjects(payload, event.plyOffset, event.kind),
        mechanism = List(ConcreteChessObject(EvidenceObjectKind.Mechanism, event.kind.toString)),
        consequence = List(ConcreteChessObject(EvidenceObjectKind.Consequence, event.kind.toString)),
        line = Some(payload.line),
        horizon = Some(s"ply:${event.plyOffset}")
      ),
      directChange = DirectCausalChange.Occurred,
      rootOwnedProof = Some(RootOwnedEffectProof.RootLineEvent(source, payload, event))
    )

  private def lineEpisodeChannel(
      source: EvidenceRef,
      payload: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): DirectCauseChannel =
    DirectCauseChannel(
      binding = EvidenceObjectBinding(
        source = source,
        actor = actorObjects(episode.actor),
        target = effectTargetObjects(payload, episode.eventPlyOffset, episode.consequence.kind),
        mechanism = episode.links.map(link =>
          ConcreteChessObject(EvidenceObjectKind.Mechanism, link.kind.toString)
        ).distinctBy(_.signaturePart),
        consequence = List(ConcreteChessObject(
          EvidenceObjectKind.Consequence,
          episode.consequence.kind.toString
        )),
        line = Some(payload.line),
        horizon = Some(s"ply:${episode.eventPlyOffset}")
      ),
      directChange = DirectCausalChange.Occurred,
      rootOwnedProof = Some(RootOwnedEffectProof.LineEpisode(source, payload, episode))
    )

  private def effectTargetObjects(
      payload: LineFactEvidence,
      plyOffset: Int,
      kind: LineEventKind | LineConsequenceKind
  ): List[ConcreteChessObject] =
    val captureTarget = payload.materialCaptures
      .find(_.plyOffset == plyOffset)
      .toList
      .flatMap(capture => List(
        ConcreteChessObject(EvidenceObjectKind.Square, capture.square.key),
        ConcreteChessObject(EvidenceObjectKind.Piece, capture.capturedRole.name)
      ))
    val promotionTarget = payload.lineEvents
      .find(event => event.plyOffset == plyOffset && event.kind == LineEventKind.Promotion)
      .toList
      .flatMap(event =>
        event.square.toList.map(square =>
          ConcreteChessObject(EvidenceObjectKind.Square, square.key)
        ) ++ event.targetRole.toList.map(role =>
          ConcreteChessObject(EvidenceObjectKind.Piece, role.name)
        )
      )
    kind match
      case LineEventKind.Capture | LineEventKind.Recapture |
          LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        captureTarget
      case LineEventKind.Check | LineEventKind.Mate | LineConsequenceKind.Mate =>
        List(
          ConcreteChessObject(EvidenceObjectKind.Square, "h8"),
          ConcreteChessObject(EvidenceObjectKind.Piece, "king")
        )
      case LineEventKind.Promotion | LineConsequenceKind.Promotion =>
        promotionTarget
      case _ =>
        Nil

  private def actorObjects(actor: RootCausalActor): List[ConcreteChessObject] =
    List(
      ConcreteChessObject(EvidenceObjectKind.Move, actor.moveUci),
      ConcreteChessObject(EvidenceObjectKind.Side, if actor.color == White then "white" else "black"),
      ConcreteChessObject(EvidenceObjectKind.Piece, actor.role.name),
      ConcreteChessObject(EvidenceObjectKind.Square, actor.from.key),
      ConcreteChessObject(EvidenceObjectKind.Square, actor.to.key)
    ).distinctBy(_.signaturePart)

  private def materialSummary(
      captures: List[LineMaterialCapture],
      netCp: Int,
      hasRecaptureChain: Boolean = false,
      hasRecoveryWindow: Boolean = false
  ): LineMaterialSummary =
    LineMaterialSummary(
      sideToMove = White,
      captures = captures,
      netCaptureCpForMover = netCp,
      maxGainCpForMover = captures.filter(_.side == White).map(_.valueCp).maxOption.getOrElse(0),
      maxLossCpForMover = captures.filter(_.side == Black).map(_.valueCp).maxOption.getOrElse(0),
      hasRecaptureChain = hasRecaptureChain,
      hasRecoveryWindow = hasRecoveryWindow,
      promotionGainCpForMover = 0,
      materialWindowComplete = true
    )

  private def proofSource(
      id: String,
      fen: String,
      line: LineNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id,
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      PositionNodeRef(fen, 0, Some(White)),
      Some(line),
      line.role.scope,
      EvidenceConfidence.LegalReplayVerified
    )

  private def legalReplay(
      fen: String,
      moves: List[String]
  ): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(fen -> List.empty[LineReplayStep]) {
      case ((before, steps), (move, ply)) =>
        val after = PrincipalVariationEvidence
          .legalFenAfter(before, move)
          .getOrElse(fail(s"expected legal move $move after $before"))
        after -> (steps :+ LineReplayStep(ply, move, before, after))
    }._2
