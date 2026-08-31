package lila.chessjudgment.analysis.policy

import lila.chessjudgment.analysis.assembly.{ EvidenceFactAssembler, RelativeAssessmentAssembler }
import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.model.line.CandidateLineEvaluation
import lila.chessjudgment.model.judgment.*

enum ClaimAdmissionStatus:
  case Certified
  case Deferred
  case Rejected

final case class ClaimAdmissionDecision(
    claim: JudgmentClaim,
    status: ClaimAdmissionStatus,
    presentLayers: Set[EvidenceLayer],
    missingLayerGroups: List[Set[EvidenceLayer]],
    missingEvidence: List[EvidenceRef]
)

object ClaimTruthPolicy:

  def evaluate(claim: JudgmentClaim, context: JudgmentAssemblyContext): ClaimAdmissionDecision =
    val graph = context.evidenceGraph
    val contentCarrierRefs = claim.content.toList.map(_.carrierRef)
    val missingEvidence =
      (claim.evidence ++ contentCarrierRefs).distinct.filter(ref => graph.record(ref).isEmpty)
    val claimBoundRecords =
      claim.evidence
        .flatMap(ref => graph.record(ref))
        .filter(graph.proofEligible)
        .filter(isBoundToClaim(claim, _, graph))
    val directlyNamedCauses =
      directlyHostedRelativeCauseRecords(claim, graph)
    val directlyHostedCauses =
      directlyNamedCauses.filter(record => graph.proofEligible(record) && isBoundToClaim(claim, record, graph))
    val allNamedCausesBound =
      directlyNamedCauses.size == directlyHostedCauses.size
    val causeHostProofGroups =
      directlyHostedCauses.map(record => directCauseHostProofRecords(record, graph))
    val useCauseHostProof =
      claim.content.isEmpty && causeHostProofGroups.nonEmpty
    val proofGroups =
      if useCauseHostProof then
        causeHostProofGroups.map(_.filter(graph.proofEligible).distinctBy(_.ref.id))
      else
        claimBoundRecords.map(record =>
          (record :: graph.parentClosure(record))
            .filter(graph.proofEligible)
            .distinctBy(_.ref.id)
        )
    val proofRecords = proofGroups.flatten.distinctBy(_.ref.id)
    val claimBoundLayers =
      proofRecords
        .map(_.ref.layer)
        .toSet
    val requiredGroups = contentRequiredLayerGroups(claim).getOrElse(requiredLayerGroups(claim.family))
    def missingFor(records: List[EvidenceRecord]): List[Set[EvidenceLayer]] =
      val layers = records.map(_.ref.layer).toSet
      requiredGroups.filterNot(group => group.exists(layers.contains))
    def routeCertified(records: List[EvidenceRecord]): Boolean =
      missingFor(records).isEmpty && familySpecificProof(claim, records, graph)
    val certifiedRouteGroups = proofGroups.filter(routeCertified)
    val missingGroups =
      if certifiedRouteGroups.nonEmpty then Nil
      else
        proofGroups
          .map(missingFor)
          .minByOption(_.size)
          .getOrElse(requiredGroups)
    val hasFamilyProof =
      allNamedCausesBound &&
        (
          if useCauseHostProof then
            proofGroups.nonEmpty && proofGroups.forall(routeCertified)
          else certifiedRouteGroups.nonEmpty
        )
    val baselineStatus =
      if claim.evidence.isEmpty || missingEvidence.nonEmpty then ClaimAdmissionStatus.Rejected
      else if !allNamedCausesBound then ClaimAdmissionStatus.Rejected
      else if claimBoundRecords.isEmpty then ClaimAdmissionStatus.Rejected
      else if missingGroups.nonEmpty || !hasFamilyProof then ClaimAdmissionStatus.Deferred
      else ClaimAdmissionStatus.Certified
    val contentProofClosed = claim.content.isEmpty || exactContentCertified(claim, context)
    val status =
      if claim.content.nonEmpty && baselineStatus == ClaimAdmissionStatus.Certified && !contentProofClosed then
        ClaimAdmissionStatus.Deferred
      else baselineStatus
    ClaimAdmissionDecision(
      claim = claim,
      status = status,
      presentLayers = claimBoundLayers,
      missingLayerGroups = missingGroups,
      missingEvidence = missingEvidence
    )

  /** Content uses the same Ja authority as the surrounding claim, but carries
    * a narrower F proof obligation. Nothing is copied out of the graph: each
    * branch reopens the carrier named by the claim and checks the exact closure
    * that Jp was allowed to attach.
    */
  private def exactContentCertified(claim: JudgmentClaim, context: JudgmentAssemblyContext): Boolean =
    val graph = context.evidenceGraph
    claim.content.exists {
      case JudgmentClaimContent.CandidateComparison(carrier, identity) =>
        exactCandidateComparisonContent(claim, carrier, identity, graph)
    }

  private def contentRequiredLayerGroups(claim: JudgmentClaim): Option[List[Set[EvidenceLayer]]] =
    claim.content.map {
      case JudgmentClaimContent.CandidateComparison(_, _) =>
        List(Set(EvidenceLayer.CandidateComparison))
    }

  private def exactCandidateComparisonContent(
      claim: JudgmentClaim,
      carrier: EvidenceRef,
      identity: CandidateComparisonSemanticKey,
      graph: TypedEvidenceGraph
  ): Boolean =
    graph.record(carrier).exists {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        exactPlayedTransition(graph, claim.primaryPosition).exists { case (_, transition) =>
          val playedMove = EvidenceRef.normalizeMove(transition.moveUci)
          val playedMoves = Set(playedMove).filter(_.nonEmpty)
          JudgmentSubjectBinding.uniquePlayedEndpoint(fact, playedMoves).exists { line =>
                carrier.producer == EvidenceProducer.RelativeMoveProducer &&
                carrier.layer == EvidenceLayer.CandidateComparison &&
                carrier.scope == EvidenceScope.Counterfactual &&
                carrier.line == Some(fact.candidateLine) &&
                CandidateComparisonSemanticKey.from(fact) == identity &&
                claim.family == ClaimFamily.Evaluation &&
                claim.subject == ClaimSubject.PlayedMove &&
                claim.subjectMove.exists(move => EvidenceRef.sameMove(move, transition.moveUci)) &&
                claim.primaryLine.contains(line) &&
                claim.primaryPosition == transition.from &&
                carrier.position == transition.from &&
                claim.scope == carrier.scope &&
                claim.confidence == carrier.confidence &&
                playedMoves.nonEmpty &&
                JudgmentSubjectBinding.comparisonBinding(fact, playedMoves) != SubjectBindingClass.Other &&
                exactRefs(claim.evidence, List(carrier))
          }
        }
      case _ => false
    }

  private def exactPlayedTransition(
      graph: TypedEvidenceGraph,
      position: PositionNodeRef
  ): Option[(EvidenceRef, MoveTransitionEvidence)] =
    graph.recordsFor(position).collect {
      case record @ EvidenceRecord(ref, payload @ MoveTransitionEvidence(_, from, _, _), _)
          if ref.producer == EvidenceProducer.MoveTransitionProducer &&
            ref.layer == EvidenceLayer.MoveTransition &&
            ref.scope == EvidenceScope.PlayedTransition &&
            ref.position == from &&
            ref.line.isEmpty &&
            ref.confidence == EvidenceConfidence.LegalReplayVerified && graph.proofEligible(record) =>
        ref -> payload
    } match
      case transition :: Nil => Some(transition)
      case _                 => None

  private def exactRefs(actual: List[EvidenceRef], expected: List[EvidenceRef]): Boolean =
    actual.map(_.id).distinct.size == actual.size &&
      expected.map(_.id).distinct.size == expected.size &&
      actual.size == expected.size &&
      actual.forall(ref => expected.contains(ref)) &&
      expected.forall(ref => actual.contains(ref))

  /** A RelativeCause directly named by a claim is a Cause host, not a license
    * to traverse every parent attached to the Cause record. Cause parents also
    * contain contrast and context evidence, which may diagnose the comparison
    * but cannot replace the Cause's own direct proof at Ja.
    */
  private def directlyHostedRelativeCauseRecords(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    claim.evidence
      .distinctBy(_.id)
      .flatMap(ref => graph.record(ref))
      .collect {
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) =>
          record
      }

  /** Ja may inspect the Cause carrier, its exact comparison, and the records
    * named by C's admitted direct channels. Contrast/context sections and
    * generic parent closure remain diagnostic-only.
    */
  private def directCauseHostProofRecords(
      causeRecord: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    causeRecord match
      case causeRecord @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        val comparisonRecord = graph.record(cause.comparisonEvidence).toList
        val certifiedDirectRecords =
          RelativeCauseConstructionAdmission
            .admittedDirectChannels(cause, graph)
            .flatMap(channel => channel.binding.source :: channel.binding.provenance)
            .distinctBy(_.id)
            .flatMap(ref => graph.record(ref).toList)
        (causeRecord :: (comparisonRecord ++ certifiedDirectRecords))
          .distinctBy(_.ref.id)
      case _ =>
        Nil

  private def isBoundToClaim(
      claim: JudgmentClaim,
      record: EvidenceRecord,
    graph: TypedEvidenceGraph
  ): Boolean =
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    val layerCompatible = claimLayerCompatible(record)
    val sameLine = layerCompatible && claim.primaryLine.exists(line => recordLineMatches(record, line, graph))
    val sameSubjectMove =
      layerCompatible && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine, graph))
    val comparisonLineSupport =
      !claim.family.isLongTerm &&
        comparisonProofLayer(record.ref.layer) &&
        comparisonLinesForClaim(claim, graph).exists(line => recordLineMatches(record, line, graph))
    val transitionDestinationLocal =
      claimEvidenceIds.contains(record.ref.id) &&
        positionLocalLayer(record.ref.layer) &&
        transitionDestinationsForClaim(claim, graph).contains(record.ref.position)
    val parentLinked = record.parents.exists(parent => claimEvidenceIds.contains(parent.id))
    val childLinked =
      claim.evidence.flatMap(graph.record).exists(child =>
        child.parents.exists(parent => parent.id == record.ref.id)
      )
    val relativeCauseProofLinked =
      relativeCauseProofSourceIds(claim, graph).contains(record.ref.id)
    val linkedBound =
      (parentLinked || childLinked) &&
        linkedRecordCompatible(claim, record, graph)
    sameLine || sameSubjectMove || comparisonLineSupport || transitionDestinationLocal || linkedBound ||
      relativeCauseProofLinked

  private def relativeCauseProofSourceIds(claim: JudgmentClaim, graph: TypedEvidenceGraph): Set[String] =
    claim.evidence
      .flatMap(ref => graph.byId.get(ref.id))
      .flatMap {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          relativeCauseProofSourceIds(cause)
        case _ =>
          Nil
      }
      .map(_.id)
      .toSet

  private def relativeCauseProofSourceIds(cause: RelativeCauseFact): List[EvidenceRef] =
    cause.comparisonEvidence :: cause.supportEvidence ++
      cause.proof.toList.flatMap(proof =>
        proof.directProof.sourceRefs ++
          proof.contrastProof.sourceRefs ++
          proof.contextSupport.sourceRefs
      )

  private def linkedRecordCompatible(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Boolean =
    val supportLayer = linkedSupportLayer(record.ref.layer)
    (supportLayer && claim.primaryLine.exists(line => recordLineMatches(record, line, graph))) ||
      (supportLayer && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine, graph))) ||
      linkedPositionContext(claim, record) ||
      linkedClaimSupport(claim, record, graph)

  private def linkedPositionContext(claim: JudgmentClaim, record: EvidenceRecord): Boolean =
    claim.primaryLine.isEmpty &&
      claim.subjectMove.isEmpty &&
      record.ref.position == claim.primaryPosition &&
      positionContextLayer(claim.family, record.ref.layer)

  private def linkedClaimSupport(claim: JudgmentClaim, record: EvidenceRecord, graph: TypedEvidenceGraph): Boolean =
    val comparisonLineBound =
      comparisonLinesForClaim(claim, graph).exists(line => recordLineMatches(record, line, graph))
    claim.primaryLine.nonEmpty &&
      (record.ref.position == claim.primaryPosition || comparisonLineBound) &&
      linkedSupportLayer(record.ref.layer)

  private def linkedSupportLayer(layer: EvidenceLayer): Boolean =
    claimProofCarrierLayer(layer)

  private def claimProofCarrierLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.PositionOccurrence => false
      case _                   => true

  private def claimLayerCompatible(record: EvidenceRecord): Boolean =
    linkedSupportLayer(record.ref.layer) ||
      positionLocalLayer(record.ref.layer)

  private def positionContextLayer(family: ClaimFamily, layer: EvidenceLayer): Boolean =
    if family.isLongTerm then positionLocalLayer(layer)
    else
      layer match
        case EvidenceLayer.PositionOccurrence | EvidenceLayer.Relation | EvidenceLayer.Line | EvidenceLayer.Eval =>
          true
        case _ =>
          false

  private def comparisonProofLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Line | EvidenceLayer.Eval =>
        true
      case _ =>
        false

  private def comparisonLinesForClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[LineNodeRef] =
    claim.evidence
      .flatMap(graph.record)
      .flatMap(record => comparisonLinesForRecord(record, graph))
      .distinct

  private def transitionDestinationsForClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): Set[PositionNodeRef] =
    val claimMoves = claim.subjectMove.toSet ++ claim.primaryLine.map(_.rootMove).toSet
    if claimMoves.isEmpty then Set.empty
    else
      claim.evidence
        .flatMap(graph.record)
        .collect {
          case EvidenceRecord(_, MoveTransitionEvidence(moveUci, _, to, _), _) if claimMoves.contains(moveUci) =>
            to
        }
        .toSet

  private def comparisonLinesForRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): List[LineNodeRef] =
    record.payload match
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case RelativeCauseFactEvidence(cause) =>
        graph.relativeCauseBinding(cause).map(_.eventLine).toList
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case _ =>
        Nil

  private def recordLineMatches(
      record: EvidenceRecord,
      line: LineNodeRef,
      graph: TypedEvidenceGraph
  ): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        graph.relativeCauseBinding(cause).exists(_.eventLine == line)
      case _ =>
        record.referencesLine(line)

  private def recordMentionsMove(
      record: EvidenceRecord,
      move: String,
      claimLine: Option[LineNodeRef],
      graph: TypedEvidenceGraph
  ): Boolean =
    record.payload match
      case MoveTransitionEvidence(moveUci, _, _, _) =>
        moveUci == move &&
          claimLine.forall(line =>
            LineNodeRole.fromMoveEvidenceScope(record.ref.scope).contains(line.role) && line.rootMove == move
          )
      case payload: StructuralDeltaEvidence =>
        payload.moveUci == move &&
          claimLine.forall(line => payload.line.contains(line))
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.contains(move) ||
          claimLine.exists(line => payload.line.contains(line))
      case CandidateComparisonEvidence(fact) =>
        claimLine.exists(line => line == fact.referenceLine || line == fact.candidateLine)
      case RelativeCauseFactEvidence(cause) =>
        graph.relativeCauseBinding(cause).exists(binding =>
          claimLine match
            case Some(line) => line == binding.eventLine
            case None       => EvidenceRef.sameMove(binding.eventLine.rootMove, move)
        )
      case _ =>
        false

  private def positionLocalLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.PositionOccurrence | EvidenceLayer.Relation =>
        true
      case _ =>
        false

  private def requiredLayerGroups(family: ClaimFamily): List[Set[EvidenceLayer]] =
    family match
      case ClaimFamily.Tactical =>
        List(
          Set(EvidenceLayer.TacticalMechanism, EvidenceLayer.RelativeCause, EvidenceLayer.Relation, EvidenceLayer.Line),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.Relation,
            EvidenceLayer.TacticalMechanism,
            EvidenceLayer.PassedPawnResultEvent,
            EvidenceLayer.Eval
          ),
          Set(EvidenceLayer.Eval, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.PawnStructure =>
        List(
          Set(EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.StructuralDelta),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.MoveTransition,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.RelativeCause
          )
        )
      case ClaimFamily.PassedPawnResult =>
        List(Set(EvidenceLayer.PassedPawnResultEvent))
      case ClaimFamily.Defensive =>
        List(
          Set(
            EvidenceLayer.TacticalMechanism,
            EvidenceLayer.RelativeCause
          ),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeCause,
            EvidenceLayer.TacticalMechanism
          )
        )
      case ClaimFamily.Conversion =>
        List(
          Set(EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.Line, EvidenceLayer.StructuralDelta, EvidenceLayer.Relation),
          Set(EvidenceLayer.Eval, EvidenceLayer.Line, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.Material =>
        List(
          Set(EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.Line),
          Set(EvidenceLayer.Eval, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.Evaluation =>
        List(Set(EvidenceLayer.RelativeAssessment), Set(EvidenceLayer.CandidateComparison))

  private def familySpecificProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    claim.family match
      case ClaimFamily.Tactical =>
        tacticalProof(claim, records, graph)
      case ClaimFamily.Defensive =>
        defensiveProof(records, graph)
      case ClaimFamily.PawnStructure =>
        pawnStructureProof(claim, records, graph)
      case ClaimFamily.PassedPawnResult =>
        passedPawnResultProof(claim, records, graph)
      case ClaimFamily.Conversion =>
        conversionProof(claim, records, graph)
      case ClaimFamily.Material =>
        materialProof(records, graph)
      case ClaimFamily.Evaluation =>
        evaluationProof(claim, records, graph)

  private def pawnStructureProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    records.exists {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        graph.proofEligible(record) &&
          (
            payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure) ||
              payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructureDelta)
          ) && transitionBoundToClaim(
            claim,
            payload.moveUci,
            payload.line,
            Some(record.ref.scope)
          )
      case _ => false
    }

  private def causeBoundToClaim(
      claim: JudgmentClaim,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    graph.relativeCauseBinding(cause).exists(binding =>
      claim.primaryLine.contains(binding.eventLine) ||
        claim.subjectMove.exists(move => EvidenceRef.sameMove(move, binding.eventLine.rootMove))
    )

  private def transitionBoundToClaim(
      claim: JudgmentClaim,
      moveUci: String,
      line: Option[LineNodeRef],
      scope: Option[EvidenceScope] = None
  ): Boolean =
    claim.primaryLine.exists(expected =>
      line.contains(expected) || (line.isEmpty && scope.contains(expected.role.scope))
    ) &&
      claim.subjectMove.forall(move => EvidenceRef.sameMove(move, moveUci))

  private def passedPawnResultProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val relativeCauses = relativeCausesBoundToClaimEvidence(claim, records)
    relativeCauses.exists(cause =>
      graph.comparisonFor(cause).nonEmpty &&
        passedPawnResultRelativeCauseHasProof(cause, graph) &&
        RelativeCauseKind.requiresExactPassedPawnResult(cause.kind)
    )

  private def relativeCausesBoundToClaimEvidence(
      claim: JudgmentClaim,
      records: List[EvidenceRecord]
  ): List[RelativeCauseFact] =
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if claimEvidenceIds(ref.id) =>
        cause
    }.distinct

  private def tacticalProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val relativeCauses = relativeCausesBoundToClaimEvidence(claim, records)
    // The exact actual/counterfactual L2 proof is both the WrongMoveOrder
    // tactical anchor and its concrete tactical line.
    val exactMoveOrderTacticalCauses =
      relativeCauses.filter(exactMoveOrderCausalProof(_, graph))
    val hasMaterialSwingCauseCarrier =
      relativeCauses.exists(_.kind == RelativeCauseKind.MaterialSwing)
    val ownedCauseProof =
      relativeCauses.exists { cause =>
        graph.comparisonFor(cause).exists(fact =>
          engineComparisonProvesTactic(fact) &&
            tacticalRelativeCause(cause.kind) &&
            (cause.hasOwnedTacticalProof(graph) || exactMoveOrderTacticalCauses.contains(cause))
        )
      }
    val selfContainedMechanismProof =
      records.exists {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.canAnchorTacticalClaim && payload.hasLineProof && payload.hasEngineOrForcingProof
        case _ =>
          false
      }
    !hasMaterialSwingCauseCarrier &&
      (ownedCauseProof || selfContainedMechanismProof || exactMoveOrderTacticalCauses.nonEmpty)

  private def defensiveProof(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorDefensiveClaim
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        graph.comparisonFor(cause).nonEmpty && defensiveRelativeCauseHasProof(cause, graph)
      case _ =>
        false
    }

  private def defensiveRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    defensiveRelativeCause(cause.kind) &&
      cause.hasOwnedTypedDepth(graph)

  private def materialProof(records: List[EvidenceRecord], graph: TypedEvidenceGraph): Boolean =
    val hasMaterialCause =
      records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          graph.comparisonFor(cause).nonEmpty && materialRelativeCauseHasProof(cause, graph)
        case _ =>
          false
      }
    val hasMaterialLine =
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          materialLineProof(payload)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.kind == TacticalMechanismKind.MaterialGain && payload.canAnchorTacticalClaim
        case _ =>
          false
      }
    val hasComparison =
      records.exists {
        case EvidenceRecord(_, CandidateLineEvaluationEvidence(_, CandidateLineEvaluation.EngineSearch(line)), _) =>
          line.mate.nonEmpty
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          recordEngineBacked(record)
        case _ =>
          false
      }
    hasMaterialCause && hasMaterialLine && hasComparison

  private def conversionProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val conversionCauses = relativeCausesBoundToClaimEvidence(claim, records).filter(cause =>
      graph.comparisonFor(cause).nonEmpty && conversionRelativeCauseHasProof(cause, graph)
    )
    val hasConversionCause = conversionCauses.nonEmpty
    val hasConversionContext = conversionCauses.exists(cause => ConversionContextPolicy.supports(records, cause.kind))
    val hasComparison =
      records.exists {
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          recordEngineBacked(record)
        case _ =>
          false
      }
    hasConversionCause && hasConversionContext && hasComparison

  private def evaluationProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val recordIds = records.map(_.ref.id).toSet
    records.exists {
      case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
        claim.content.exists {
          case JudgmentClaimContent.CandidateComparison(carrier, identity) =>
            carrier == ref &&
              CandidateComparisonSemanticKey.from(fact) == identity &&
              recordIds(ref.id)
        }
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        graph
          .candidateComparisonRecord(assessment.primaryComparisonEvidence)
          .exists {
            case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
              recordIds.contains(record.ref.id) &&
                (fact.kind match
                  case CandidateComparisonKind.PlayedVsBest =>
                    fact.referenceLine == assessment.reference.ref &&
                      fact.candidateLine == assessment.candidate.ref &&
                      fact.hasDistinctRootMoves
                  case CandidateComparisonKind.BestVsSecond =>
                    EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove) &&
                      EvidenceRef.sameMove(assessment.candidate.ref.rootMove, assessment.reference.ref.rootMove) &&
                      fact.referenceLine == assessment.reference.ref &&
                      fact.candidateLine.role == LineNodeRole.Alternative &&
                      fact.candidateLine.rank == 2 &&
                      fact.hasDistinctRootMoves &&
                      fact.candidateSet.nonEmpty
                  case CandidateComparisonKind.PlayedVsAlternative |
                      CandidateComparisonKind.ReferenceVsAlternative =>
                    false)
            case _ => false
          }
      case _ =>
        false
    }

  private def recordEngineBacked(record: EvidenceRecord): Boolean =
    record.ref.confidence == EvidenceConfidence.EngineBacked ||
      recordHasMate(record)

  private def recordHasMate(record: EvidenceRecord): Boolean =
    record.payload match
      case CandidateLineEvaluationEvidence(_, CandidateLineEvaluation.EngineSearch(line)) =>
        line.mate.nonEmpty
      case _ =>
        false

  private def engineComparisonProvesTactic(
      fact: CandidateComparisonFact
  ): Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.TACTICAL_IMPACT_WP ||
      fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP

  private def tacticalRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind) == ClaimFamily.Tactical

  private def materialResultCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind) == ClaimFamily.Material

  private def materialRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    materialResultCause(cause.kind) &&
      cause.hasOwnedTypedDepth(graph)

  private def materialLineProof(payload: LineFactEvidence): Boolean =
    payload.hasMaterialConsequence ||
      payload.hasRecaptureRecoveryConsequence ||
      payload.hasMaterialRecaptureChain ||
      payload.hasClosedMaterialRecovery ||
      payload.hasDirectCauseProjectionEligibleMaterialEvent

  private def conversionRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    (
      cause.kind == RelativeCauseKind.ConversionSecured ||
        cause.kind == RelativeCauseKind.RecaptureRecoveryWindow
    ) &&
      cause.hasOwnedTypedDepth(graph)

  private def passedPawnResultRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.hasOwnedPassedPawnResultProof(graph)

  /** WrongMoveOrder is public only through the direct, occurrence-bound L2
    * resource differential. No passed-pawn result label or endpoint evaluation can replace
    * its certified actual/counterfactual branch proof.
    */
  private def exactMoveOrderCausalProof(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    val comparison = graph.comparisonFor(cause)
    val directSection = cause.proof.map(_.directProof)
    val admitted = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
    (comparison, directSection) match
      case (Some(fact), Some(section)) =>
        cause.kind == RelativeCauseKind.WrongMoveOrder &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
          cause.attribution.directProofEligible && cause.attribution.rootMoveMatched &&
          section.role == RelativeCauseProofRole.DirectProof &&
          section.strength == RelativeCauseProofStrength.Primary &&
          ForcedReplyResourceDifferentialDemand.accepts(fact) &&
          admitted.exists {
            case channel @ DirectCauseChannel(_, _, _, _, Some(
                  RootOwnedEffectProof.ForcedReplyResourceDifferential(source, result)
                ), _, _, _) =>
              section.sourceRefs.contains(source) && cause.supportEvidence.contains(source) &&
                result.occurrence.referenceLine == fact.referenceLine &&
                result.occurrence.playedLine == fact.candidateLine &&
                result.hasCompleteProofPaths &&
                channel.binding.proofRole.contains(RelativeCauseProofRole.DirectProof) &&
                RootOwnedEffectPolicy.admits(cause, graph, channel)
            case _ => false
          }
      case _ => false

  private def lineHasTacticalProof(payload: LineFactEvidence): Boolean =
    payload.rootMove.exists(rootMove =>
      payload.consequencesForRootMove(rootMove).exists(consequence =>
        LineConsequenceKind.tacticalDriver(consequence.kind)
      )
    )

  private def defensiveRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind) == ClaimFamily.Defensive
