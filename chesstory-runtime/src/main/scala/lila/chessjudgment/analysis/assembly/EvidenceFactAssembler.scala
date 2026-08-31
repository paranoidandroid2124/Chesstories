package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.line.CandidateLineEvaluation
import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

object EvidenceFactAssembler:

  final private case class TacticalMechanismCandidate(
      kind: TacticalMechanismKind,
      records: List[EvidenceRecord],
      signals: List[TacticalMechanismSignal],
      semanticAnchors: List[EvidenceSemanticAnchor],
      relationLineProof: Option[TacticalRelationLineProof] = None
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
          exactKey._4.map(anchor => s"anchor:$anchor") ++
          relationLineProof.toList.map(proof => s"relation-line:${proof.relation.id}:${proof.occurrence.id}:${proof.line.id}")
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
    val closedRelationRecords = relationRecords(
      context,
      allocator,
      closedRelationsByOccurrence,
      canonicalRelationDeltas
    )
    val baseRecords =
      List.concat(
        closedRelationRecords,
        structuralDeltaRecords(context, allocator, canonicalRelationDeltas, closedRelationRecords)
      )
    val baseContext = context.withEvidence(baseRecords)
    val mechanismRecords = tacticalMechanismRecords(baseContext, allocator)
    baseContext.withEvidence(mechanismRecords)

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
      val production = ClosedRelationEvidence.relationProduction(replay, edge.moveUci)
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
      closedRelations.materializeOccurrence(
        occurrenceId = allocator.evidenceId(s"closed-relation-occurrence:${edge.evidence.id}"),
        edge = edge,
        lineOwner = lineOwner.map(_.ref),
        lineEvidence = lineEvidence,
        outputRecords = provisional
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
          signals = candidate.signals,
          relationLineProof = candidate.relationLineProof
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
        signals = routedCandidate.signals,
        relationLineProof = routedCandidate.relationLineProof
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
          node.relation.semanticGroupingAnchors :+ lineEventAnchor(binding.event),
          Some(binding.proof)
        )
      )
    )
    val defensiveResourceEntries =
      records.collect { case record @ EvidenceRecord(_, payload: LineFactEvidence, _) =>
        val declaredRootMove = payload.rootMove.map(EvidenceRef.normalizeMove)
        val rootCheckEvasionEvents =
          payload
            .lineEventsOf(LineEventKind.CheckEvasion)
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
          if !declaredRootMove.exists(payload.rootIsRecapture) then Nil
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
          rootCheckEvasionEvents
            .filter(_.targetRole.exists(_.name.equalsIgnoreCase("king")))
            .map(event =>
              TacticalMechanismCandidate(
                TacticalMechanismKind.DefensiveResource,
                List(record),
                List(
                  TacticalMechanismSignal(
                    TacticalMechanismSignalKind.LineEvent,
                    LineEventKind.CheckEvasion.toString,
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
        val payload = TacticalMechanismEvidence(
          candidate.kind,
          None,
          None,
          candidate.signals,
          candidate.relationLineProof
        )
        payload.canAnchorTacticalClaim || payload.canAnchorDefensiveClaim
      }

  private def lineConsequenceAnchor(consequence: LineConsequence): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.LineConsequence,
      consequence.kind.toString,
      s"proof:${consequence.directCauseProjectionEligible}",
      s"root:${consequence.rootMove.map(EvidenceRef.normalizeMove).getOrElse("none")}",
      s"event:${consequence.eventOccurrence.map(_.stableKey).getOrElse("none")}",
      s"occurrences:${consequence.proofOccurrences.map(_.stableKey).mkString(",")}",
      s"root-side:${consequence.rootSide.map(_.toString).getOrElse("none")}",
      s"beneficiary:${consequence.beneficiary.map(_.toString).getOrElse("none")}",
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
      signals: List[TacticalMechanismSignal],
      relationLineProof: Option[TacticalRelationLineProof]
  ): Option[EvidenceRecord] =
    val payload = TacticalMechanismEvidence(kind, moveUci, line, signals, relationLineProof)
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
      canonicalRelationDeltas: Map[String, CanonicalRelationDelta],
      relationRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val occurrencesById = relationRecords.collect {
      case EvidenceRecord(ref, occurrence: ClosedRelationOccurrenceEvidence, _) => ref.id -> occurrence
    }.toMap
    val resultOutputsByTransitionId = relationRecords.flatMap {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, parents)
          if relation.proofStage != RelationProofStage.PositionFact =>
        parents.flatMap(parent =>
          occurrencesById.get(parent.id).flatMap(occurrence =>
            occurrence.outputFor(record.ref).map(_ =>
              occurrence.edge.evidence.id -> (DerivedRelationResultKey.from(relation) -> record)
            )
          )
        )
      case _ => Nil
    }.groupMap(_._1)(_._2)
    context.transitions.flatMap { edge =>
      for
        transitionReplay <- context.transitionReplay(edge)
        replayTransition <- transitionReplay.onlyTransition
        line = lineForTransition(context, edge)
        consequences = replayTransition.structuralConsequences
        if consequences.nonEmpty
        delta = StructuralDeltaAnalyzer.bind(
          transition = replayTransition,
          canonicalRelations = canonicalRelationDeltas(edge.evidence.id)
        )
        consequenceKeys = consequences.flatMap(_.relationKeys).toSet
        relationSources = delta.canonicalRelations.changes
          .filter(change => consequenceKeys(change.key))
          .map(_.source)
          .distinctBy(_.id)
        expectedResultKeys = consequences.flatMap(_.resultPremiseKeys).toSet
        resultOutputRecords = resultOutputsByTransitionId
          .getOrElse(edge.evidence.id, Nil)
          .filter { case (key, _) => expectedResultKeys(key) }
        _ = require(
          resultOutputRecords.map(_._1).distinct.size == resultOutputRecords.size &&
            resultOutputRecords.map(_._1).toSet == expectedResultKeys,
          "a structural consequence must bind every consumed transition result to one exact occurrence output"
        )
        resultPremiseSources = resultOutputRecords.sortBy(_._1.stableKey).map { case (key, output) =>
          TransitionResultPremiseSource(key, output.ref)
        }
        record = TransitionFactNormalizer.fromStructuralDelta(
          id = allocator.evidenceId(s"structural-delta:${edge.evidence.id}"),
          delta = delta,
          transition = edge,
          replay = transitionReplay,
          line = line.map(_.ref),
          resultPremiseSources = resultPremiseSources,
          parents = (
            List(edge.evidence) ++
              line.toList.flatMap(lineParents(context, _)) ++
              evidenceRefs(context, EvidenceLayer.PositionOccurrence, Some(edge.from), None) ++
              evidenceRefs(context, EvidenceLayer.PositionOccurrence, Some(edge.to), None) ++
              relationSources ++
              resultPremiseSources.map(_.source)
          ).distinctBy(_.id)
        )
      yield record
    }

  private def requiredLineFactRecord(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): EvidenceRecord =
    context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(line) match
      case Some((record, _)) => record
      case None =>
        throw IllegalArgumentException(
          s"line '${line.id}' has no unique proof-eligible line-fact carrier"
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
