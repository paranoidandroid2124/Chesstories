package lila.chessjudgment.analysis.assembly

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import lila.chessjudgment.model.line.CandidateLineEvaluation
import lila.chessjudgment.analysis.opening.OpeningContextFactNormalizer
import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.tactical.TacticalRelationEvidence
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

object EvidenceFactAssembler:

  final private case class PlanTransitionProductionOccurrence(
      eventRecord: EvidenceRef,
      proof: PlanSequenceProof
  ):
    def idKey: String =
      val exact = exactIdentityKey(List(eventRecord.id, proof.stableKey))
      MessageDigest
        .getInstance("SHA-256")
        .digest(exact.getBytes(StandardCharsets.UTF_8))
        .iterator
        .map(byte => f"${byte & 0xff}%02x")
        .mkString

  final private case class TacticalMechanismCandidate(
      kind: TacticalMechanismKind,
      records: List[EvidenceRecord],
      signals: List[TacticalMechanismSignal],
      semanticAnchors: List[EvidenceSemanticAnchor]
  ):
    require(records.map(_.ref.id).distinct.size == records.size, "a tactical mechanism cannot repeat one source")
    require(signals.distinct.size == signals.size, "a tactical mechanism cannot repeat one signal")
    require(
      semanticAnchors.map(_.stableKey).distinct.size == semanticAnchors.size,
      "a tactical mechanism cannot repeat one semantic anchor"
    )

    def exactKey: (TacticalMechanismKind, List[String], List[String], List[String]) =
      (
        kind,
        records.map(_.ref.id).sorted,
        signals.map(tacticalSignalKey).sorted,
        semanticAnchors.map(_.stableKey).sorted
      )

    def idKey: String =
      exactIdentityKey(
        kind.toString :: exactKey._2.map(id => s"carrier:$id") ++
          exactKey._3.map(signal => s"signal:$signal") ++
          exactKey._4.map(anchor => s"anchor:$anchor")
      )

    def withRecords(additional: List[EvidenceRecord]): TacticalMechanismCandidate =
      copy(records = (records ++ additional).sortBy(_.ref.id))

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    NodeLineTransitionAssembler.assemble(raw).map(enrich)

  def enrich(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    val input = context.input
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val closedRelationsByOccurrence = closedPositionRelations(context)
    val canonicalRelationDeltas = canonicalRelationDeltasFor(
      context,
      closedRelationsByOccurrence
    )
    val baseRecords =
      List.concat(
        relationRecords(context, allocator, closedRelationsByOccurrence, canonicalRelationDeltas),
        structuralDeltaRecords(context, allocator, canonicalRelationDeltas)
      )
    val baseContext = context.withEvidence(baseRecords)
    val mechanismRecords = tacticalMechanismRecords(baseContext, allocator)
    val strategicContext = baseContext.withEvidence(mechanismRecords)
    val planCausalEventRecords = PlanCausalEventAssembler.fromAssembly(input, strategicContext, allocator)
    val causalPlanContext = strategicContext.withEvidence(planCausalEventRecords)
    val transitionRecords = planTransitionRecords(causalPlanContext, allocator)
    val planEventContext = causalPlanContext.withEvidence(transitionRecords)
    val openingRecords = featureApplicabilityRecords(input, planEventContext, allocator)
    val openingContext = planEventContext.withEvidence(openingRecords)
    val strategicMechanismOutput = strategicMechanismRecords(openingContext, allocator)
    openingContext.withEvidence(strategicMechanismOutput)

  private[analysis] def exactStrategicMechanisms(
      context: JudgmentAssemblyContext
  ): Option[List[EvidenceRecord]] =
    val actual = context.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: StrategicMechanismEvidence, _) => record
    }
    Option.when(
      actual.forall(context.evidenceGraph.proofEligible) &&
        sourcesPrecedeMechanisms(context, actual)
    )(actual)

  private def sourcesPrecedeMechanisms(
      context: JudgmentAssemblyContext,
      expected: List[EvidenceRecord]
  ): Boolean =
    expected
      .flatMap(_.parents)
      .distinctBy(_.id)
      .forall(source =>
        context.evidenceGraph
          .record(source)
          .exists(record =>
            context.evidenceGraph.parentClosure(record).forall { parent =>
              !parent.payload.isInstanceOf[StrategicMechanismEvidence] &&
              !parent.payload.isInstanceOf[StrategicMechanismContrastEvidence]
            }
          )
      )

  private def closedPositionRelations(
      context: JudgmentAssemblyContext
  ): Map[PositionNodeRef, CanonicalPositionRelationSnapshot] =
    val positionScopes = context.positions.map(node => node.ref -> node.role.scope).toMap
    require(
      positionScopes.size == context.positions.size,
      "every registered relation occurrence must own exactly one evidence scope"
    )
    context.transitions
      .flatMap(edge => List(edge.from, edge.to))
      .distinct
      .map { position =>
        val scope = positionScopes.getOrElse(
          position,
          throw IllegalArgumentException(
            s"relation occurrence '${position.id.getOrElse(position.fen)}' has no registered scope"
          )
        )
        val analysis = context.positionAnalysis(position).getOrElse(
          throw IllegalArgumentException(
            s"relation occurrence '${position.id.getOrElse(position.fen)}' has no position analysis"
          )
        )
        position -> context.evidenceGraph.relationGraph.closedPositionRelationSnapshot(
          position,
          scope,
          analysis.relationInventory
        )
      }
      .toMap

  /** One occurrence binding per admitted transition. Closed relation outputs
    * and structural deltas consume this same value instead of resolving the
    * identical changed relation nodes twice.
    */
  private def canonicalRelationDeltasFor(
      context: JudgmentAssemblyContext,
      closedRelations: Map[PositionNodeRef, CanonicalPositionRelationSnapshot]
  ): Map[String, CanonicalRelationDelta] =
    val deltas = context.transitions.map { edge =>
      val transition = context
        .transitionReplay(edge)
        .flatMap(_.onlyTransition)
        .getOrElse(
          throw IllegalArgumentException(
            s"transition '${edge.evidence.id}' has no exact canonical replay transition"
          )
        )
      edge.evidence.id -> CanonicalRelationDelta.bind(
        transition.declared,
        transition.relationDelta,
        closedRelations(edge.from),
        closedRelations(edge.to)
      )
    }
    require(
      deltas.map(_._1).distinct.size == deltas.size,
      "one transition evidence occurrence must own exactly one canonical relation delta"
    )
    deltas.toMap

  private def relationRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      closedRelationsByOccurrence: Map[PositionNodeRef, CanonicalPositionRelationSnapshot],
      canonicalRelationDeltas: Map[String, CanonicalRelationDelta]
  ): List[EvidenceRecord] =
    context.transitions.flatMap { edge =>
      val lineOwner = lineForTransition(context, edge)
      val replay = context.transitionReplay(edge).getOrElse(
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' has no exact canonical replay"
        )
      )
      val production = TacticalRelationEvidence.relationProduction(replay, edge.moveUci)
      val closedRelations = production.bindClosedOutput(
        closedRelationsByOccurrence(edge.from),
        closedRelationsByOccurrence(edge.to),
        canonicalRelationDeltas(edge.evidence.id)
      )
      val lineEvidence = lineOwner.map(line => requiredLineFactRecord(context, line.ref).ref)
      val provisional = production.relations.map { relation =>
        val occurrenceOwner = lineOwner.map(_.ref.id).getOrElse(edge.evidence.id)
        RelationFactEvidence.record(
          id = allocator.evidenceId(
            s"relation:transition:${exactIdentityKey(List(occurrenceOwner, RelationFactKind.id(relation.kind), relation.semanticId))}"
          ),
          payload = relation,
          position = edge.from,
          line = lineOwner.map(_.ref),
          scope = edge.role.scope,
          confidence = EvidenceConfidence.LegalReplayVerified
        )
      }
      val sourceNodesByOutputId = provisional.map { record =>
        val relation = record.payload match
          case exact: RelationFactEvidence => exact
          case _ => throw IllegalArgumentException("a tactical relation production emitted a non-relation payload")
        record.ref.id -> relationSourceNodes(closedRelations, relation)
      }.toMap
      require(
        sourceNodesByOutputId.size == provisional.size,
        "one transition occurrence cannot repeat a materialized relation output"
      )
      closedRelations.materializeOccurrence(
        occurrenceId = allocator.evidenceId(s"closed-relation-occurrence:${edge.evidence.id}"),
        edge = edge,
        lineOwner = lineOwner.map(_.ref),
        lineEvidence = lineEvidence,
        outputRecords = provisional,
        sourceNodesByOutputId = sourceNodesByOutputId
      )
    }

  private def tacticalMechanismRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val lineMechanisms =
      context.lines.flatMap(line => tacticalMechanismRecordsForLine(context, allocator, line))
    val unlinedTransitionMechanisms =
      context.transitions
        .filter(edge => lineForTransition(context, edge).isEmpty)
        .flatMap(edge => tacticalMechanismRecordsForTransition(context, allocator, edge))
    val mechanisms = lineMechanisms ++ unlinedTransitionMechanisms
    val duplicateIds = mechanisms.groupBy(_.ref.id).collect {
      case (id, records) if records.size > 1 => id
    }.toList.sorted
    require(
      duplicateIds.isEmpty,
      s"tactical mechanism producer emitted duplicate evidence ids: ${duplicateIds.mkString(",")}"
    )
    mechanisms

  private def tacticalMechanismRecordsForLine(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      line: CandidateLineNode
  ): List[EvidenceRecord] =
    val lineRecords = context.evidenceGraph.recordsFor(line.ref)
    val positions = lineRecords.map(_.ref.position).distinct
    val position = positions match
      case exact :: Nil => Some(exact)
      case Nil          => context.root
      case _ =>
        throw IllegalArgumentException(
          s"line '${line.ref.id}' carries tactical inputs from conflicting position occurrences"
        )
    val records = lineRecords
    position.toList.flatMap { nodeRef =>
      tacticalMechanismCandidates(context.evidenceGraph, records, line.ref.rootMove).flatMap { candidate =>
        tacticalMechanismRecord(
          id = allocator.evidenceId(
            s"tactical-mechanism:line:${allocator.key(line.role)}:${line.ref.rank}:${line.ref.rootMove}:${allocator.key(candidate.kind)}:${candidate.idKey}"
          ),
          kind = candidate.kind,
          position = nodeRef,
          line = Some(line.ref),
          moveUci = Some(line.ref.rootMove),
          scope = line.role.scope,
          records = candidate.records,
          signals = candidate.signals
        )
      }
    }

  private def tacticalMechanismRecordsForTransition(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      edge: MoveTransitionEdge
  ): List[EvidenceRecord] =
    val records =
      context.evidenceGraph.records.filter(record =>
          record.ref.scope == edge.role.scope &&
          record.ref.position == edge.from &&
          record.mentionsMove(edge.moveUci)
      )
    tacticalMechanismCandidates(context.evidenceGraph, records, edge.moveUci).flatMap { candidate =>
      val routedCandidate =
        candidate.withRecords(context.evidenceGraph.byId.get(edge.evidence.id).toList)
      tacticalMechanismRecord(
        id = allocator.evidenceId(
          s"tactical-mechanism:transition:${allocator.key(edge.role)}:${edge.moveUci}:${allocator.key(candidate.kind)}:${routedCandidate.idKey}"
        ),
        kind = routedCandidate.kind,
        position = edge.from,
        line = None,
        moveUci = Some(edge.moveUci),
        scope = edge.role.scope,
        records = routedCandidate.records,
        signals = routedCandidate.signals
      )
    }

  private def tacticalMechanismCandidates(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      rootMove: String
  ): List[TacticalMechanismCandidate] =
    val relationNodes = graph.proofEligibleRelationNodesByEvidenceIds(records.map(_.ref.id).toSet)
    val nonRelationEntries =
      records.flatMap(record =>
        record.payload match
          case payload: LineFactEvidence =>
            (
              payload.rootOwnedCausalConsequences(rootMove) ++
                payload.immediateReplyCheckLiabilitiesForRootMove(rootMove)
            ).distinct
              .flatMap(consequence =>
                TacticalMechanismKind
                  .fromLineConsequence(consequence.kind, payload.rootIsRecapture(rootMove))
                  .map(mechanismKind =>
                    TacticalMechanismCandidate(
                      mechanismKind,
                      List(record),
                      List(
                        TacticalMechanismSignal(
                          TacticalMechanismSignalKind.LineConsequence,
                          consequence.kind.toString,
                          EvidenceLayer.Line,
                          Some(record.ref)
                        )
                      ),
                      List(lineConsequenceAnchor(consequence))
                    )
                  )
              )
          case CandidateLineEvaluationEvidence(_, CandidateLineEvaluation.EngineSearch(line)) =>
            line.mate.toList.map(mate =>
              TacticalMechanismCandidate(
                TacticalMechanismKind.KingForcing,
                List(record),
                List(
                  TacticalMechanismSignal(
                    TacticalMechanismSignalKind.MateBranch,
                    mate.toString,
                    EvidenceLayer.Eval,
                    Some(record.ref)
                  )
                ),
                List(
                  EvidenceSemanticAnchor.of(
                    EvidenceSemanticAnchorKind.LineConsequence,
                    LineConsequenceKind.Mate.toString,
                    s"mate:$mate",
                    s"moves:${line.moves.map(EvidenceRef.normalizeMove).mkString(",")}"
                  )
                )
              )
            )
          case _ =>
            Nil
      )
    val relationEntries = relationNodes.flatMap(node =>
      TacticalRelationLineContract.bindings(graph, node, rootMove).map(binding =>
        TacticalMechanismCandidate(
          binding.kind,
          List(node.record, binding.lineRecord),
          binding.signals,
          node.relation.semanticGroupingAnchors :+ lineEventAnchor(binding.event)
        )
      )
    )
    val defensiveResourceEntries =
      records.collect { case record @ EvidenceRecord(_, payload: LineFactEvidence, _) =>
        val declaredRootMove = payload.rootMove.map(EvidenceRef.normalizeMove)
        val rootDefenderEvents =
          payload
            .lineEventsOf(LineEventKind.DefenderMove)
            .filter(event =>
              event.plyOffset == 0 &&
                declaredRootMove.exists(root => EvidenceRef.sameMove(root, event.moveUci))
            )
        val rootMoveRecoveryConsequences =
          declaredRootMove
            .map(payload.consequencesForRootMove)
            .getOrElse(Nil)
            .filter(consequence =>
              consequence.kind == LineConsequenceKind.RecaptureSequence ||
                consequence.kind == LineConsequenceKind.RecoveryWindow
            )
        val recoveryEntries =
          if rootDefenderEvents.isEmpty then Nil
          else
            rootMoveRecoveryConsequences.map(consequence =>
              TacticalMechanismCandidate(
                TacticalMechanismKind.DefensiveResource,
                List(record),
                List(
                  TacticalMechanismSignal(
                    TacticalMechanismSignalKind.LineConsequence,
                    consequence.kind.toString,
                    EvidenceLayer.Line,
                    Some(record.ref)
                  )
                ),
                List(lineConsequenceAnchor(consequence))
              )
            )
        val checkDefenseEntries =
          rootDefenderEvents
            .filter(_.targetRole.exists(_.name.equalsIgnoreCase("king")))
            .map(event =>
              TacticalMechanismCandidate(
                TacticalMechanismKind.DefensiveResource,
                List(record),
                List(
                  TacticalMechanismSignal(
                    TacticalMechanismSignalKind.LineEvent,
                    LineEventKind.DefenderMove.toString,
                    EvidenceLayer.Line,
                    Some(record.ref)
                  )
                ),
                List(lineEventAnchor(event))
              )
            )
        recoveryEntries ++ checkDefenseEntries
      }.flatten
    val candidates = nonRelationEntries ++ defensiveResourceEntries ++ relationEntries
    require(
      candidates.map(_.exactKey).distinct.size == candidates.size,
      "tactical mechanism producers emitted one exact candidate twice"
    )
    candidates
      .sortBy(_.idKey)
      .filter { candidate =>
        val payload = TacticalMechanismEvidence(candidate.kind, None, None, candidate.signals)
        payload.canAnchorTacticalClaim || payload.canAnchorDefensiveClaim
      }

  private def lineConsequenceAnchor(consequence: LineConsequence): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.LineConsequence,
      consequence.kind.toString,
      s"proof:${consequence.proofSignal}",
      s"root:${consequence.rootMove.map(EvidenceRef.normalizeMove).getOrElse("none")}",
      s"event:${consequence.eventMove.map(EvidenceRef.normalizeMove).getOrElse("none")}",
      s"moves:${consequence.lineMoves.map(EvidenceRef.normalizeMove).mkString(",")}",
      s"root-side:${consequence.rootSide.map(_.toString).getOrElse("none")}",
      s"beneficiary:${consequence.beneficiary.map(_.toString).getOrElse("none")}",
      s"captures:${consequence.stationarySacrificeCaptures.mkString(",")}",
      s"material:${consequence.materialOutcome.map(_.toString).getOrElse("none")}"
    )

  private def lineEventAnchor(event: LineMoveEvent): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.LineEvent,
      event.kind.toString,
      s"move:${EvidenceRef.normalizeMove(event.moveUci)}",
      s"ply:${event.plyOffset}",
      s"side:${event.side.map(_.toString).getOrElse("none")}",
      s"piece:${event.pieceRole.map(_.name).getOrElse("none")}",
      s"target:${event.targetRole.map(_.name).getOrElse("none")}",
      s"square:${event.square.map(_.key).getOrElse("none")}"
    )

  private def tacticalSignalKey(signal: TacticalMechanismSignal): String =
    exactIdentityKey(
      List(
        signal.kind.toString,
        signal.label,
        signal.sourceLayer.toString,
        signal.source.map(_.id).getOrElse("none"),
        signal.relationKind.map(RelationFactKind.id).getOrElse("none")
      )
    )

  private def exactIdentityKey(parts: Iterable[String]): String =
    parts.iterator.map(value => s"${value.length}:$value").mkString("|")

  private def tacticalMechanismRecord(
      id: String,
      kind: TacticalMechanismKind,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      moveUci: Option[String],
      scope: EvidenceScope,
      records: List[EvidenceRecord],
      signals: List[TacticalMechanismSignal]
  ): Option[EvidenceRecord] =
    val payload = TacticalMechanismEvidence(kind, moveUci, line, signals)
    Option.when(payload.hasConcreteProof) {
      val confidence =
        if signals.exists(_.kind == TacticalMechanismSignalKind.MateBranch) then
          EvidenceConfidence.EngineBacked
        else if signals.exists(signal =>
            signal.kind == TacticalMechanismSignalKind.Relation ||
              signal.kind == TacticalMechanismSignalKind.LineConsequence ||
              signal.kind == TacticalMechanismSignalKind.LineEvent
          )
        then EvidenceConfidence.LegalReplayVerified
        else EvidenceConfidence.Mixed
      EvidenceRecord(
        ref = EvidenceRef(
          id = id,
          producer = EvidenceProducer.TacticalMechanismProducer,
          layer = EvidenceLayer.TacticalMechanism,
          position = position,
          line = line,
          scope = scope,
          confidence = confidence
        ),
        payload = payload,
        parents = records.map(_.ref)
      )
    }

  private def structuralDeltaRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      canonicalRelationDeltas: Map[String, CanonicalRelationDelta]
  ): List[EvidenceRecord] =
    context.transitions.flatMap { edge =>
      for
        transitionReplay <- context.transitionReplay(edge)
        replayTransition <- transitionReplay.onlyTransition
        legalStep = replayTransition.legal
        side = legalStep.move.piece.color
        line = lineForTransition(context, edge)
        delta = StructuralDeltaAnalyzer.bind(
          transition = replayTransition,
          canonicalRelations = canonicalRelationDeltas(edge.evidence.id)
        )
        record = TransitionFactNormalizer.fromStructuralDelta(
          id = allocator.evidenceId(s"structural-delta:${edge.evidence.id}"),
          delta = delta,
          transition = edge,
          replay = transitionReplay,
          line = line.map(_.ref),
          perspective = side,
          parents = (
            List(edge.evidence) ++
              line.toList.flatMap(lineParents(context, _)) ++
              evidenceRefs(context, EvidenceLayer.PositionFeature, Some(edge.from), None) ++
              evidenceRefs(context, EvidenceLayer.PositionFeature, Some(edge.to), None) ++
              delta.canonicalRelations.sourceRefs
          ).distinctBy(_.id)
        )
        if record.payload match
          case payload: StructuralDeltaEvidence =>
            payload.hasTypedOutput
          case _ => false
      yield record
    }

  private def requiredLineFactRecord(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): EvidenceRecord =
    val matches = context.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, payload: LineFactEvidence, _)
          if record.carriesLinePayload(line, EvidenceLayer.Line) =>
        record
    }
    matches match
      case exact :: Nil => exact
      case Nil =>
        throw IllegalArgumentException(
          s"line '${line.id}' has no canonical line-fact carrier"
        )
      case _ =>
        throw IllegalArgumentException(
          s"line '${line.id}' has more than one canonical line-fact carrier"
        )

  private def featureApplicabilityRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val root = context.position(PositionNodeRole.Before)
    root.toList.flatMap { rootNode =>
      val openingContext = input.openingContext
      val canAssessOpening =
        openingContext.identity.nonEmpty ||
          openingContext.recognition.nonEmpty ||
          openingContext.themePriorSelection.nonEmpty
      val contextRecord = Option.when(canAssessOpening) {
        OpeningContextFactNormalizer.fromContext(
          id = allocator.evidenceId("opening-context:before"),
          identity = openingContext.identity,
          signals = openingContext.signals,
          recognition = openingContext.recognition,
          themePriorSelection = openingContext.themePriorSelection,
          position = rootNode.ref,
          line = None,
          scope = EvidenceScope.BeforePosition,
          confidence = EvidenceConfidence.Mixed,
          parents = openingContextParents(context, rootNode.ref)
        )
      }
      val anchorRecords = featureAnchorRecords(context, rootNode, allocator)
        .filter(record => StrategicMechanismEvidence.sourceMechanisms(record).nonEmpty)
      val anchors = anchorRecords.collect { case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) =>
        anchor
      }
      val openingContextEvidence =
        contextRecord.collect { case EvidenceRecord(_, payload: OpeningContextEvidence, _) => payload }
      val applicabilityRecord =
        for
          assessment <- assessApplicability(openingContextEvidence, anchors)
          if assessment.canCertifyOpeningClaim
        yield applicabilityAssessmentRecord(
          id = allocator.evidenceId("applicability:before"),
          assessment = assessment,
          position = rootNode.ref,
          line = None,
          scope = EvidenceScope.BeforePosition,
          confidence = EvidenceConfidence.Mixed,
          parents = (contextRecord.map(_.ref).toList ++ anchorRecords.map(_.ref)).distinctBy(_.id)
        )
      contextRecord
        .filter(_ => applicabilityRecord.nonEmpty)
        .toList ++ anchorRecords ++ applicabilityRecord.toList
    }

  private def strategicMechanismRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val candidates =
      context.evidenceGraph.records.filter(context.evidenceGraph.proofEligible).flatMap { record =>
        StrategicMechanismEvidence.sourceMechanisms(record).map { case (kind, signal) =>
          StrategicMechanismCandidate(
            kind,
            record.ref.position,
            record.ref.line,
            record.ref.scope,
            signal,
            record
          )
        }
      }
    candidates
      .distinctBy(_.exactKey)
      .sortBy(_.idKey)
      .flatMap { candidate =>
        val transitionCarriers =
          if candidate.kind != StrategicMechanismKind.PlanPressure ||
            !candidate.source.payload.isInstanceOf[PlanCausalEventEvidence]
          then Nil
          else
            context.evidenceGraph.records.collect {
              case record @ EvidenceRecord(ref, _: PlanTransitionEvidence, parents)
                  if ref.line == candidate.line &&
                    ref.scope == candidate.scope &&
                    parents.exists(_.id == candidate.source.ref.id) &&
                    context.evidenceGraph.proofEligible(record) =>
                record
            }
        val routes =
          if transitionCarriers.isEmpty then List(List(candidate.source))
          else transitionCarriers.sortBy(_.ref.id).map(carrier => List(candidate.source, carrier))
        routes.map { route =>
          val sourceRecords = route.distinctBy(_.ref.id).sortBy(_.ref.id)
          val signals = List(candidate.signal)
          val semanticAnchors =
            (
              EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicMechanism, candidate.kind.toString) ::
                sourceRecords.flatMap(StrategicMechanismEvidence.sourceSemanticAnchors)
            ).distinctBy(_.stableKey).sortBy(_.stableKey)
          val positionKey =
            candidate.position.id
              .map(allocator.key)
              .getOrElse(exactIdentityKey(List(candidate.position.ply.toString, candidate.position.fen)))
          val routeKey = exactIdentityKey(
            candidate.kind.toString ::
              sourceRecords.map(record => s"carrier:${record.ref.id}") ++
              signals.map(signal => s"signal:${strategicSignalKey(signal)}") ++
              semanticAnchors.map(anchor => s"anchor:${anchor.stableKey}")
          )
          val ref = EvidenceRef(
            id = allocator.evidenceId(
              s"strategic-mechanism:${allocator.key(candidate.kind)}:$positionKey:${candidate.line.map(line => allocator.key(line.rootMove)).getOrElse("position")}:${allocator.key(candidate.scope)}:$routeKey"
            ),
            producer = EvidenceProducer.StrategicMechanismProducer,
            layer = EvidenceLayer.StrategicMechanism,
            position = candidate.position,
            line = candidate.line,
            scope = candidate.scope,
            confidence = strategicMechanismConfidence(sourceRecords)
          )
          val parents = sourceRecords.map(_.ref)
          val barePayload = StrategicMechanismEvidence(candidate.kind, signals, semanticAnchors)
          val payload = barePayload.copy(
            assemblyProof = Some(StrategicMechanismAssemblyProof.from(ref, parents, barePayload))
          )
          EvidenceRecord(
            ref = ref,
            payload = payload,
            parents = parents
          )
        }
      }

  final private case class StrategicMechanismCandidate(
      kind: StrategicMechanismKind,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      signal: StrategicMechanismSignal,
      source: EvidenceRecord
  ):
    def exactKey
        : (StrategicMechanismKind, PositionNodeRef, Option[LineNodeRef], EvidenceScope, String, List[String]) =
      (
        kind,
        position,
        line,
        scope,
        strategicSignalKey(signal),
        source.ref.id :: StrategicMechanismEvidence
          .sourceSemanticAnchors(source)
          .map(_.stableKey)
          .distinct
          .sorted
      )

    def idKey: String =
      exactIdentityKey(
        List(
          kind.toString,
          position.id.getOrElse(position.fen),
          position.ply.toString,
          line.map(_.id).getOrElse("position"),
          scope.toString,
          strategicSignalKey(signal),
          source.ref.id
        ) ++ exactKey._6
      )

  private def strategicSignalKey(signal: StrategicMechanismSignal): String =
    exactIdentityKey(
      List(
        signal.kind.toString,
        signal.label,
        signal.source.id,
        signal.axisKey.getOrElse("none"),
        signal.planResultAssessment
          .map(_.resultRoute.stableKey)
          .getOrElse("no-plan-result-route")
      )
    )

  private def strategicMechanismConfidence(
      records: List[EvidenceRecord]
  ): EvidenceConfidence =
    if records.exists(_.ref.confidence == EvidenceConfidence.EngineBacked) then EvidenceConfidence.Mixed
    else if records.forall(_.ref.confidence == EvidenceConfidence.BoardDerived) then
      EvidenceConfidence.BoardDerived
    else EvidenceConfidence.Mixed

  private def confidenceForParents(
      context: JudgmentAssemblyContext,
      parents: List[EvidenceRef]
  ): EvidenceConfidence =
    val confidences = parents
      .flatMap(context.evidenceGraph.record)
      .flatMap(record => record :: context.evidenceGraph.parentClosure(record))
      .map(_.ref.confidence)
      .distinct
    confidences match
      case confidence :: Nil => confidence
      case _                 => EvidenceConfidence.Mixed

  private def openingContextParents(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef
  ): List[EvidenceRef] =
    val base =
      List(
        EvidenceLayer.PositionFeature,
        EvidenceLayer.Relation
      ).flatMap(layer => evidenceRefs(context, layer, Some(position), None))
    base.distinctBy(_.id)

  private def featureAnchorRecords(
      context: JudgmentAssemblyContext,
      node: PositionNode,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val evidenceAnchors =
      context.evidenceGraph.records
        .filter(record => record.ref.position == node.ref && context.evidenceGraph.proofEligible(record))
        .flatMap(evidenceFeatureAnchors)
    evidenceAnchors
      .distinctBy { case (anchor, parents) =>
        (anchor.theme, anchor.signal, anchor.sourceLayer, parents.map(_.id).sorted.mkString("|"))
      }
      .map { case (anchor, parents) =>
        val parentIds = parents.map(_.id).distinct.sorted
        val parentKey =
          if parentIds.isEmpty then "root"
          else exactIdentityKey(parentIds.map(id => s"parent:$id"))
        featureAnchorRecord(
          id = allocator.evidenceId(
            s"feature-anchor:${allocator.key(anchor.theme)}:${allocator.key(anchor.sourceLayer)}:${allocator.key(anchor.signal)}:$parentKey"
          ),
          anchor = anchor,
          position = node.ref,
          line = None,
          scope = EvidenceScope.BeforePosition,
          confidence = confidenceForParents(context, parents),
          parents = parents.distinctBy(_.id)
        )
      }

  private def featureAnchorRecord(
      id: String,
      anchor: FeatureAnchor,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.FeatureAnchorProducer,
        layer = EvidenceLayer.FeatureAnchor,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = FeatureAnchorEvidence(anchor),
      parents = parents
    )

  private def applicabilityAssessmentRecord(
      id: String,
      assessment: ApplicabilityAssessment,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.ApplicabilityAssessmentProducer,
        layer = EvidenceLayer.ApplicabilityAssessment,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = ApplicabilityAssessmentEvidence(assessment),
      parents = parents
    )

  private def assessApplicability(
      contextEvidence: Option[OpeningContextEvidence],
      anchors: List[FeatureAnchor]
  ): Option[ApplicabilityAssessment] =
    val observedThemes = anchors.map(_.theme).distinct
    Option.when(observedThemes.nonEmpty) {
      val priorSelections = contextEvidence.flatMap(_.themePriorSelection).toList
      val priorThemes = priorSelections.flatMap(_.prior.themes).distinct
      val supported = priorThemes.filter(theme =>
        anchors.exists(_.theme == theme)
      )
      val unverified = priorThemes.filterNot(supported.contains)
      val observedOnly = observedThemes.filterNot(priorThemes.contains)
      val ambiguousRecognition = contextEvidence.flatMap(_.recognition).exists(_.candidates.drop(1).nonEmpty)
      val applicability =
        featureApplicability(contextEvidence, supported)
      val status =
        if priorThemes.isEmpty then ApplicabilityStatus.InternalOnly
        else if ambiguousRecognition && supported.isEmpty then ApplicabilityStatus.Ambiguous
        else if supported.nonEmpty && unverified.nonEmpty then ApplicabilityStatus.PartiallySupported
        else if supported.nonEmpty then ApplicabilityStatus.Supported
        else ApplicabilityStatus.Unverified
      ApplicabilityAssessment(
        applicability = applicability,
        status = status,
        observedThemes = observedThemes,
        supportedThemes = supported,
        unverifiedPriorThemes = unverified,
        observedOnlyThemes = observedOnly,
        priorMatchSources = priorSelections.map(_.matchSource).distinct
      )
    }

  private def featureApplicability(
      contextEvidence: Option[OpeningContextEvidence],
      supportedThemes: List[OpeningTheme]
  ): FeatureApplicability =
    val hasOpeningContext = contextEvidence.exists(opening =>
      opening.identity.nonEmpty || opening.recognition.nonEmpty || opening.themePriorSelection.nonEmpty
    )
    if hasOpeningContext && supportedThemes.nonEmpty then FeatureApplicability.OpeningRelevant
    else FeatureApplicability.ObservedOnly


  private def evidenceFeatureAnchors(record: EvidenceRecord): List[(FeatureAnchor, List[EvidenceRef])] =
    record.payload match
      case payload: StructuralDeltaEvidence if payload.hasTypedOutput =>
        val hasPawnStructureAnchor =
          payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure) ||
            payload.hasConsequence(TransitionConsequenceKind.PawnTensionResolution)
        val pawnStructure =
          Option.when(hasPawnStructureAnchor)(
            FeatureAnchor(
              OpeningTheme.PawnStructure,
              FeatureAnchorSignal.PawnStructureObserved,
              EvidenceLayer.StructuralDelta
            )
          )
        val pawnTension =
          Option.when(
            payload.hasAnyConsequence(
              Set(
                TransitionConsequenceKind.PawnTensionCreated,
                TransitionConsequenceKind.PawnTensionResolution
              )
            )
          )(
            FeatureAnchor(
              OpeningTheme.PawnStructure,
              FeatureAnchorSignal.PawnTensionObserved,
              EvidenceLayer.StructuralDelta
            )
          )
        List(pawnStructure, pawnTension).flatten.map(
          _ -> (record.ref :: record.parents)
        )
      case _ => Nil

  private def planTransitionRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val graph = context.evidenceGraph
    val authoritativeEvents = graph.records.collect {
      case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
          if graph.proofEligible(record) =>
        record -> event
    }
    val produced = authoritativeEvents
      .flatMap { case (eventRecord, event) =>
        event.planSequenceProofs.map { proof =>
          val occurrence = PlanTransitionProductionOccurrence(
            eventRecord = eventRecord.ref,
            proof = proof
          )
          TransitionFactNormalizer.fromPlanTransition(
            id = allocator.evidenceId(
              s"plan-transition:${occurrence.idKey}"
            ),
            proof = proof,
            position = event.rootTransition.from,
            line = Some(event.rootLine),
            scope = eventRecord.ref.scope,
            confidence = eventRecord.ref.confidence,
            parents = List(eventRecord.ref)
          )
        }
      }
    val duplicateIds = produced
      .groupMapReduce(_.ref.id)(_ => 1)(_ + _)
      .collect { case (id, count) if count > 1 => id }
      .toList
      .sorted
    require(
      duplicateIds.isEmpty,
      s"plan transition producer emitted duplicate exact evidence ids: ${duplicateIds.mkString(", ")}"
    )
    produced.sortBy(_.ref.id)

  private def relationSourceNodes(
      closedRelations: CanonicalRelationTransitionInventory,
      relation: RelationFactEvidence
  ): List[CanonicalRelationNode] =
    closedRelations.sourceNodesFor(relation).getOrElse(
      throw IllegalArgumentException(
        s"closed relation '${relation.semanticId}' has no exact occurrence binding"
      )
    )

  private def lineParents(
      context: JudgmentAssemblyContext,
      line: CandidateLineNode
  ): List[EvidenceRef] =
    evidenceRefs(context, EvidenceLayer.Line, None, Some(line.ref)) ++
      evidenceRefs(context, EvidenceLayer.Eval, None, Some(line.ref))

  private def evidenceRefs(
      context: JudgmentAssemblyContext,
      layer: EvidenceLayer,
      position: Option[PositionNodeRef],
      line: Option[LineNodeRef]
  ): List[EvidenceRef] =
    context.evidenceGraph.records.collect {
      case record
          if record.ref.layer == layer &&
            position.forall(_ == record.ref.position) &&
            line.forall(record.ref.line.contains) =>
        record.ref
    }

  private def lineForTransition(
      context: JudgmentAssemblyContext,
      edge: MoveTransitionEdge
  ): Option[CandidateLineNode] =
    context.lineForRootTransition(edge)
