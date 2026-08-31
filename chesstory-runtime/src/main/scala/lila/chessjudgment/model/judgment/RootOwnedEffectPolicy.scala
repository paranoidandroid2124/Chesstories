package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.model.line.PrincipalVariationEvidence
private[judgment] object RootOwnedEffectDescriptorPolicy:

  private final case class IdentityParts(
      primitiveKind: RootOwnedEffectPrimitiveKind,
      passedPawnResultKindIds: List[String] = Nil,
      passedPawnResult: Option[PassedPawnResultSemanticIdentity] = None,
      causalProofId: Option[String] = None
  )

  def describe(
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof
  ): RootOwnedEffectDescriptor =
    val parts = identityParts(proof)
    val materialOutcome = rootOwnedMaterialOutcome(proof)
    RootOwnedEffectDescriptor(
      identity = RootOwnedEffectIdentity(
        primitiveKind = parts.primitiveKind,
        targetSignatures = binding.target.map(_.signaturePart).distinct.sorted,
        passedPawnResultKindIds = parts.passedPawnResultKindIds.map(normalize).filter(_.nonEmpty).distinct.sorted,
        passedPawnResult = parts.passedPawnResult,
        causalProofId = parts.causalProofId
      ),
      magnitude = magnitudeKnowledge(proof, materialOutcome),
      materialEventSalience = materialOutcome.map(_.event)
    )

  private def identityParts(proof: RootOwnedEffectProof): IdentityParts =
    proof match
      case _: RootOwnedEffectProof.LineEpisode =>
        IdentityParts(RootOwnedEffectPrimitiveKind.LineEpisode)
      case _: RootOwnedEffectProof.RootLineEvent =>
        IdentityParts(RootOwnedEffectPrimitiveKind.RootLineEvent)
      case _: RootOwnedEffectProof.RootRelation =>
        IdentityParts(RootOwnedEffectPrimitiveKind.RootRelation)
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, result) =>
        IdentityParts(
          RootOwnedEffectPrimitiveKind.ForcedReplyResourceDifferential,
          causalProofId = Some(result.semanticId)
        )
      case RootOwnedEffectProof.DefenseObligationChange(_, result) =>
        IdentityParts(
          RootOwnedEffectPrimitiveKind.DefenseObligationChange,
          causalProofId = Some(result.semanticId)
        )
      case RootOwnedEffectProof.PassedPawnResult(_, result) =>
        IdentityParts(
          RootOwnedEffectPrimitiveKind.PassedPawnResult,
          passedPawnResultKindIds = List(result.event.passedPawnResultKind.id),
          passedPawnResult = Some(result.semanticIdentity),
          causalProofId = Some(result.semanticId)
        )

  private def magnitudeKnowledge(
      proof: RootOwnedEffectProof,
      materialOutcome: Option[RootOwnedMaterialOutcome]
  ): DirectEffectMagnitudeKnowledge =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, line, episode) =>
        episode.consequence.kind match
          case LineConsequenceKind.Mate =>
            Option
              .when(
                episode.consequence.beneficiary.nonEmpty
              )(DirectCauseImportanceMeasure.MateArrival(episode.eventOccurrence.plyOffset))
              .map(DirectEffectMagnitudeKnowledge.Exact.apply)
              .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)
          case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
            (for
              beneficiary <- episode.consequence.beneficiary
              outcome <- materialOutcome
              if outcome.beneficiary == beneficiary
              capture <- line.uniqueMaterialCaptureFor(episode)
              if capture.side == beneficiary
              if outcome.event.matches(capture)
              if outcome.durableNetCp > 0
            yield DirectCauseImportanceMeasure.MaterialOutcome(
              outcome.durableNetCp,
              outcome.event.plyOffset
            )).map(DirectEffectMagnitudeKnowledge.Exact.apply)
              .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)
          case _ => DirectEffectMagnitudeKnowledge.NotApplicable
      case _: RootOwnedEffectProof.PassedPawnResult | _: RootOwnedEffectProof.RootLineEvent |
          _: RootOwnedEffectProof.RootRelation |
          _: RootOwnedEffectProof.ForcedReplyResourceDifferential |
          _: RootOwnedEffectProof.DefenseObligationChange =>
        DirectEffectMagnitudeKnowledge.NotApplicable

  private def rootOwnedMaterialOutcome(
      proof: RootOwnedEffectProof
  ): Option[RootOwnedMaterialOutcome] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, episode) =>
        episode.consequence.materialOutcome
      case _ => None

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

private[chessjudgment] object RootOwnedEffectPolicy:
  /** A causal root is one board occurrence, not merely a board shape that may
    * recur later in a line. The semantic FEN comparison includes side to move;
    * ply disambiguates repetitions of that same semantic board.
    */
  private[chessjudgment] def sameCausalRootOccurrence(
      left: PositionNodeRef,
      right: PositionNodeRef
  ): Boolean =
    left.ply == right.ply &&
      PrincipalVariationEvidence.sameBoardState(left.fen, right.fen)

  /** A replayed consequence belongs to the root move only when its causal
    * chain starts with a typed root bridge. Moving the same piece again later
    * is a witness of identity, not proof that the root move caused the later
    * result. Material continuation links may transport an already-rooted
    * episode, but cannot seed one.
    */
  private[chessjudgment] def admitsLineEpisode(
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): Boolean =
    val rootMove = EvidenceRef.normalizeMove(line.line.rootMove)
    val replay = line.lineReplaySteps.zipWithIndex.map { case (step, plyOffset) =>
      LineMoveOccurrence(EvidenceRef.normalizeMove(step.moveUci), plyOffset)
    }
    val event = episode.eventOccurrence
    val exactReplayPrefix =
      episode.line == line.line &&
        EvidenceRef.sameMove(episode.actor.moveUci, rootMove) &&
        episode.chain.nonEmpty &&
        episode.chain == replay.take(episode.chain.size) &&
        event.plyOffset == episode.chain.size - 1 &&
        line.lineReplaySteps
          .lift(event.plyOffset)
          .exists(step => event.sameOccurrence(event.plyOffset, step.moveUci))
    exactReplayPrefix &&
      causalPathStartsAtRoot(episode)

  private def causalPathStartsAtRoot(
      episode: RootOwnedCausalEpisode
  ): Boolean =
    val root = episode.chain.head
    val event = episode.eventOccurrence
    val transports = episode.links.filter(link =>
      link.kind == RootCausalLinkKind.MaterialActorContinuation ||
        link.kind == RootCausalLinkKind.MaterialCaptureResponse
    )
    def reachesEvent(
        current: LineMoveOccurrence,
        visited: Set[LineMoveOccurrence]
    ): Boolean =
      current == event ||
        transports.exists(link =>
          link.cause == current &&
            !visited(link.effect) &&
            reachesEvent(
              link.effect,
              visited + link.effect
            )
        )

    episode.links.exists { link =>
      val rootSeed = link.cause == root
      link.kind match
        case RootCausalLinkKind.ImmediateRootAction =>
          episode.chain.size == 1 && rootSeed && link.effect == root && event == root
        case RootCausalLinkKind.ContinuousLineAccess |
            RootCausalLinkKind.ForcedCaptureResponse |
            RootCausalLinkKind.RootActorCaptured =>
          rootSeed &&
            reachesEvent(
              link.effect,
              Set(link.effect)
            )
        case RootCausalLinkKind.RootActorContinuation |
            RootCausalLinkKind.MaterialActorContinuation |
            RootCausalLinkKind.MaterialCaptureResponse =>
          false
    }

  def passedPawnResultProofs(
      cause: RelativeCauseFact,
      source: EvidenceRef,
      result: PassedPawnResultProofEvidence
  ): List[(PassedPawnResultReplyAssessment, RootOwnedEffectProof)] =
    Option
      .when(
        result.comparisonDemand == cause.comparisonEvidence &&
          RelativeCauseKind.passedPawnResultProofCanProveCause(cause.kind, result)
      )(
        result.assessment -> RootOwnedEffectProof.PassedPawnResult(source, result)
      )
      .toList

  def certify(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      primitiveCausalSignature: Option[String] = None,
      selectedProofSegment: Option[DirectCauseProofSegment] = None
  ): List[RootOwnedEffect] =
    val rootActors = for
      causeBinding <- graph.relativeCauseBinding(cause).toList
      actor <- graph.certifiedRootActorFor(causeBinding.eventLine).toList
    yield actor
    expectedDirectChange(cause, proof).toList.flatMap { change =>
      val availableSegments = DirectCauseProofSegment.allFrom(proof)
      val exactOccurrences: List[Option[DirectCauseProofSegment]] =
        selectedProofSegment match
          case Some(selected) if availableSegments.contains(selected) => List(Some(selected))
          case Some(_)                                                => Nil
          case None if availableSegments.nonEmpty                     => availableSegments.map(Some(_))
          case None                                                   => List(None)
      rootActors.flatMap(rootActor => exactOccurrences
        .map(segment =>
          DirectCauseChannel(
            binding = binding,
            rootActor = rootActor,
            directChange = change,
            primitiveCausalSignature = primitiveCausalSignature,
            rootOwnedProof = Some(proof),
            proofSegmentOccurrence = segment
          )
        )
        .filter(admits(cause, graph, _))
      )
    }

  def admits(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      effect: RootOwnedEffect
  ): Boolean =
    effect.rootOwnedProof.exists { proof =>
      graph.relativeCauseBinding(cause).exists { causeBinding =>
        val eventLine = causeBinding.eventLine
        graph
          .certifiedRootActorFor(eventLine)
          .exists { actor =>
            RelativeCauseKind.sourceAttributionCompatible(
              cause.kind,
              cause.sourceSide,
              cause.attribution.kind
            ) &&
              requiredExactFamilyAuthority(cause, graph, proof) &&
              sameCausalRootBoard(cause, effect.binding, proof) &&
              expectedDirectChange(cause, proof).contains(effect.directChange) &&
              effect.binding.line.contains(eventLine) &&
              exactRootActor(effect.binding, actor) &&
              specificTargetReady(effect.binding.target) &&
              effect.binding.mechanism.nonEmpty &&
              effect.binding.consequence.nonEmpty &&
              proofSourceCarried(effect.binding, proof.primitiveSource) &&
              primitiveSourceRegistered(graph, proof) &&
              proofOwnsEventRoot(proof, eventLine, actor, cause, graph, effect.binding) &&
              attributionOwnsEffect(cause, proof, actor)
          }
      }
    }

  /** Extracts one exact passed-pawn-result occurrence. */
  private[chessjudgment] def exactPassedPawnResultPrimitive(
      proof: RootOwnedEffectProof
  ): Option[(EvidenceRef, PassedPawnResultProofEvidence)] =
    proof match
      case RootOwnedEffectProof.PassedPawnResult(source, result) =>
        Some((source, result))
      case _ =>
        None

  private def requiredExactFamilyAuthority(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      proof: RootOwnedEffectProof
  ): Boolean =
    proof match
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(source, result) =>
        cause.kind == RelativeCauseKind.WrongMoveOrder &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          graph.comparisonFor(cause).exists(comparison =>
            result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine
          ) &&
          graph.record(source).exists(record =>
            record.payload == result && graph.proofEligible(record)
          )
      case RootOwnedEffectProof.DefenseObligationChange(source, result) =>
        cause.kind == RelativeCauseKind.WrongMoveOrder &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          graph.comparisonFor(cause).exists(comparison =>
            result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine
          ) &&
          graph.record(source).exists(record =>
            record.payload == result && graph.proofEligible(record)
          )
      case _ if cause.kind == RelativeCauseKind.WrongMoveOrder =>
        false
      case RootOwnedEffectProof.PassedPawnResult(source, result) =>
        result.comparisonDemand == cause.comparisonEvidence &&
          RelativeCauseKind.passedPawnResultProofCanProveCause(cause.kind, result) &&
          graph.record(source).exists(record =>
            record.payload == result && graph.proofEligible(record)
          )
      case _ =>
        !RelativeCauseKind.requiresExactPassedPawnResult(cause.kind)

  /** Common root-ownership predicates shared by Cause generation and public
    * projection. Callers may add kind-specific semantics, but may not weaken
    * these legal-root and exact-line requirements.
    */
  def lineRecordOwnsEventRoot(
      source: EvidenceRef,
      line: LineFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      line.line == eventLine &&
      RootCausalActor.fromLineFact(line, eventLine.rootMove).nonEmpty

  def relationRecordOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      relation: RelationFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      graph.certifiedRootActorFor(eventLine).nonEmpty &&
      graph.record(source).exists(record => record.payload == relation && graph.relationProofEligible(record)) &&
      relation.mentionsLineMove(eventLine.rootMove) &&
      relation.rootGeometryConnected(eventLine.rootMove)

  def tacticalCarrierOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      mechanism: TacticalMechanismEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      graph.certifiedRootActorFor(eventLine).nonEmpty &&
      mechanism.line.contains(eventLine) &&
      mechanism.moveUci.exists(EvidenceRef.sameMove(_, eventLine.rootMove))

  /** A MaterialGain carrier may only summarize an admitted primitive that
    * owns the same event root. A carrier cannot turn a continuation-only line
    * result into root causality; an independently root-owned motif or relation
    * remains sufficient.
    */
  private[chessjudgment] def materialGainPrimitiveOwnsEventRoot(
      signal: TacticalMechanismSignal,
      record: EvidenceRecord,
      eventLine: LineNodeRef
  ): Boolean =
    record.payload match
      case line: LineFactEvidence =>
        signal.kind == TacticalMechanismSignalKind.LineConsequence &&
          signal.label.equalsIgnoreCase(LineConsequenceKind.MaterialGain.toString) &&
          RootOwnedEffectPolicy.lineRecordOwnsEventRoot(record.ref, line, eventLine) &&
          line
            .rootOwnedCausalEpisodes(eventLine.rootMove)
            .exists(_.consequence.kind == LineConsequenceKind.MaterialGain)
      case _ =>
        false

  private def sameCausalRootBoard(
      cause: RelativeCauseFact,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof
  ): Boolean =
    val rootPosition = cause.comparisonEvidence.position
    (binding.source :: (binding.provenance :+ proof.primitiveSource)).forall(ref =>
      sameCausalRootOccurrence(ref.position, rootPosition)
    )

  private def attributionOwnsEffect(
      cause: RelativeCauseFact,
      proof: RootOwnedEffectProof,
      actor: RootCausalActor
  ): Boolean =
    val required = cause.attribution.kind match
      case CauseAttributionKind.ReferenceCreatesResource | CauseAttributionKind.CandidateCreatesValue =>
        Some(RootOwnedEffectStake.ActorValue)
      case CauseAttributionKind.CandidateAllowsLiability =>
        Some(RootOwnedEffectStake.ActorLiability)
      case CauseAttributionKind.SharedContext | CauseAttributionKind.ContextOnly |
          CauseAttributionKind.Unattributed =>
        None
    required.exists(stake => effectStake(proof, actor).contains(stake))

  private[chessjudgment] def effectStake(
      proof: RootOwnedEffectProof,
      actor: RootCausalActor
  ): Option[RootOwnedEffectStake] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, episode) =>
        episode.consequence.beneficiary.map(beneficiary =>
          if beneficiary == actor.color then RootOwnedEffectStake.ActorValue
          else RootOwnedEffectStake.ActorLiability
        )
      case RootOwnedEffectProof.RootLineEvent(_, _, _) =>
        Some(RootOwnedEffectStake.ActorValue)
      case RootOwnedEffectProof.RootRelation(_, _) =>
        Some(RootOwnedEffectStake.ActorValue)
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, _) =>
        Some(RootOwnedEffectStake.ActorValue)
      case RootOwnedEffectProof.DefenseObligationChange(_, _) =>
        Some(RootOwnedEffectStake.ActorValue)
      case RootOwnedEffectProof.PassedPawnResult(_, result) =>
        result.assessment.robustness match
          case PassedPawnResultReplyCoverage.AllLegalRepliesRealize | PassedPawnResultReplyCoverage.SomeRepliesRealize =>
            Some(RootOwnedEffectStake.ActorValue)
          case PassedPawnResultReplyCoverage.NoReplyWitnesses | PassedPawnResultReplyCoverage.IncompleteReplyCoverage |
              PassedPawnResultReplyCoverage.AllRepliesDiverted =>
            None

  private def proofSourceCarried(binding: EvidenceObjectBinding, source: EvidenceRef): Boolean =
    binding.source == source || binding.provenance.contains(source)

  private def primitiveSourceRegistered(
      graph: TypedEvidenceGraph,
      proof: RootOwnedEffectProof
  ): Boolean =
    def eligiblePayload(source: EvidenceRef, payload: EvidencePayload): Boolean =
      graph.record(source).exists(record => graph.proofEligible(record) && record.payload == payload)

    proof match
      case RootOwnedEffectProof.LineEpisode(source, line, _) =>
        eligiblePayload(source, line)
      case RootOwnedEffectProof.RootLineEvent(source, line, _) =>
        eligiblePayload(source, line)
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        eligiblePayload(source, relation)
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(source, result) =>
        eligiblePayload(source, result)
      case RootOwnedEffectProof.DefenseObligationChange(source, result) =>
        eligiblePayload(source, result)
      case RootOwnedEffectProof.PassedPawnResult(source, result) =>
        eligiblePayload(source, result)

  private def proofOwnsEventRoot(
      proof: RootOwnedEffectProof,
      eventLine: LineNodeRef,
      actor: RootCausalActor,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      binding: EvidenceObjectBinding
  ): Boolean =
    proof match
      case RootOwnedEffectProof.LineEpisode(source, line, episode) =>
        lineRecordOwnsEventRoot(source, line, eventLine) &&
          episode.line == eventLine &&
          episode.actor == actor &&
          admitsLineEpisode(line, episode) &&
          line.rootOwnedCausalEpisodes(eventLine.rootMove).contains(episode)
      case RootOwnedEffectProof.RootLineEvent(source, line, event) =>
        lineRecordOwnsEventRoot(source, line, eventLine) &&
          line.eventsForRootMove(eventLine.rootMove).contains(event) &&
          event.side.forall(_ == actor.color)
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        relationRecordOwnsEventRoot(graph, source, relation, eventLine)
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(source, result) =>
        forcedReplyResourceOwnsEventRoot(graph, source, result, eventLine, actor)
      case RootOwnedEffectProof.DefenseObligationChange(source, result) =>
        defenseObligationChangeOwnsEventRoot(graph, source, result, eventLine, actor)
      case RootOwnedEffectProof.PassedPawnResult(source, result) =>
        passedPawnResultOwnsEventRoot(graph, source, result, eventLine, actor)

  private[chessjudgment] def passedPawnResultOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      result: PassedPawnResultProofEvidence,
      eventLine: LineNodeRef,
      actor: RootCausalActor
  ): Boolean =
    source.line.contains(eventLine) &&
      result.rootLine == eventLine &&
      EvidenceRef.sameMove(result.rootMove, eventLine.rootMove) &&
      RootCausalActor.fromPassedPawnResultEvent(result.event).contains(actor) &&
      sameCausalRootOccurrence(source.position, result.event.rootTransition.from) &&
      graph.record(source).exists(record =>
        record.payload == result && graph.proofEligible(record)
      )

  private def expectedDirectChange(
      cause: RelativeCauseFact,
      proof: RootOwnedEffectProof
  ): Option[DirectCausalChange] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, line, episode) =>
        Option
          .when(
            admitsLineEpisode(line, episode) &&
              RelativeCauseKind.acceptsDirectLineConsequence(
                cause.kind,
                line,
                line.line.rootMove,
                episode.consequence
              ) && lineBeneficiaryCompatible(cause, episode.consequence, episode.actor.color)
          )(episode)
          .flatMap(owned => lineConsequenceChange(owned.consequence, owned.actor.color))
      case RootOwnedEffectProof.RootLineEvent(_, _, event) =>
        Option
          .when(rootLocalEventAccepted(cause.kind, event.kind))(event.kind)
          .flatMap(lineEventChange)
      case RootOwnedEffectProof.RootRelation(_, relation) =>
        Option
          .when(relationDirectlyProvesCause(relation, cause.kind))(DirectCausalChange.Occurred)
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, result) =>
        Option.when(
          cause.kind == RelativeCauseKind.WrongMoveOrder && result.hasCompleteProofPaths
        )(DirectCausalChange.Occurred)
      case RootOwnedEffectProof.DefenseObligationChange(_, result) =>
        Option.when(
          cause.kind == RelativeCauseKind.WrongMoveOrder && result.hasCompleteProofPaths
        )(DirectCausalChange.Occurred)
      case RootOwnedEffectProof.PassedPawnResult(_, result) =>
        Option
          .when(RelativeCauseKind.passedPawnResultProofCanProveCause(cause.kind, result))(
            transitionConsequenceChange(result.assessment.consequence)
          )

  private def forcedReplyResourceOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      result: ForcedReplyResourceDifferentialEvidence,
      eventLine: LineNodeRef,
      actor: RootCausalActor
  ): Boolean =
    source.line.contains(eventLine) &&
      result.occurrence.referenceLine == eventLine &&
      EvidenceRef.sameMove(result.occurrence.triggerStep.moveUci, eventLine.rootMove) &&
      result.semantic.trigger.side == actor.color &&
      result.semantic.trigger.beforeRole == actor.role &&
      result.semantic.trigger.from == actor.from &&
      result.semantic.trigger.to == actor.to &&
      graph.record(source).exists(record =>
        record.payload == result && graph.proofEligible(record)
      )

  private def defenseObligationChangeOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      result: DefenseObligationChangeEvidence,
      eventLine: LineNodeRef,
      actor: RootCausalActor
  ): Boolean =
    source.line.contains(eventLine) &&
      result.occurrence.referenceLine == eventLine &&
      EvidenceRef.sameMove(result.occurrence.removalStep.moveUci, eventLine.rootMove) &&
      result.semantic.remover.side == actor.color &&
      result.semantic.remover.beforeRole == actor.role &&
      result.semantic.remover.from == actor.from &&
      result.semantic.remover.to == actor.to &&
      graph.record(source).exists(record =>
        record.payload == result && graph.proofEligible(record)
      )

  private def exactRootActor(binding: EvidenceObjectBinding, actor: RootCausalActor): Boolean =
    val expected = Set(
      EvidenceObjectKind.Move -> actor.moveUci,
      EvidenceObjectKind.Side -> (if actor.color.white then "white" else "black"),
      EvidenceObjectKind.Piece -> actor.role.name,
      EvidenceObjectKind.Square -> actor.from.key,
      EvidenceObjectKind.Square -> actor.to.key
    ).map { case (kind, key) => s"$kind:${key.trim.toLowerCase}" }
    binding.actor.map(_.signaturePart).toSet == expected

  private def specificTargetReady(targets: List[ConcreteChessObject]): Boolean =
    targets.exists(target =>
      target.key.trim.nonEmpty &&
        Set(EvidenceObjectKind.Square, EvidenceObjectKind.File)(target.kind)
    )

  private def lineBeneficiaryCompatible(
      cause: RelativeCauseFact,
      consequence: LineConsequence,
      actor: Color
  ): Boolean =
    val expected = cause.attribution.kind match
      case CauseAttributionKind.ReferenceCreatesResource | CauseAttributionKind.CandidateCreatesValue => Some(actor)
      case CauseAttributionKind.CandidateAllowsLiability => Some(!actor)
      case CauseAttributionKind.SharedContext | CauseAttributionKind.ContextOnly |
          CauseAttributionKind.Unattributed => None
    expected.exists(expectedBeneficiary => consequence.beneficiary.contains(expectedBeneficiary))

  private[chessjudgment] def relationDirectlyProvesCause(
      payload: RelationFactEvidence,
      kind: RelativeCauseKind
  ): Boolean =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.KingForcing =>
        RelationWitnessDetail.createdCheckResponse(payload.detail).exists(_.checkers.size == 2)
      case _ => false

  private def rootLocalEventAccepted(
      causeKind: RelativeCauseKind,
      eventKind: LineEventKind
  ): Boolean =
    causeKind match
      case RelativeCauseKind.WrongMoveOrder => false
      case RelativeCauseKind.KingForcing =>
        eventKind == LineEventKind.Check || eventKind == LineEventKind.Mate
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        eventKind == LineEventKind.Capture ||
          eventKind == LineEventKind.Recapture ||
          eventKind == LineEventKind.Check ||
          eventKind == LineEventKind.Mate ||
          eventKind == LineEventKind.Promotion
      case _ => false

  private def lineConsequenceChange(
      consequence: LineConsequence,
      actor: Color
  ): Option[DirectCausalChange] =
    val raw = consequence.kind match
      case LineConsequenceKind.MaterialLoss => Some(DirectCausalChange.Lost)
      case LineConsequenceKind.MaterialGain =>
        consequence.beneficiary.map(beneficiary =>
          if beneficiary == actor then DirectCausalChange.Occurred else DirectCausalChange.Lost
        )
      case LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.Promotion =>
        Some(DirectCausalChange.Occurred)
      case LineConsequenceKind.DrawResource => Some(DirectCausalChange.Maintained)
    raw

  private def lineEventChange(
      kind: LineEventKind
  ): Option[DirectCausalChange] =
    val raw = kind match
      case LineEventKind.Capture | LineEventKind.Recapture | LineEventKind.Check |
          LineEventKind.Mate | LineEventKind.Promotion | LineEventKind.CheckEvasion =>
        Some(DirectCausalChange.Occurred)
      case _ => None
    raw

  private def transitionConsequenceChange(
      consequence: TransitionConsequence
  ): DirectCausalChange =
    val raw = consequence.kind match
      case TransitionConsequenceKind.PawnTensionResolution => DirectCausalChange.Occurred
      case _ if consequence.removesState => DirectCausalChange.Lost
      case _ => DirectCausalChange.Occurred
    raw
