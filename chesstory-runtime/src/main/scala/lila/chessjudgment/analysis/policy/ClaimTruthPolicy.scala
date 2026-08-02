package lila.chessjudgment.analysis.policy

import lila.chessjudgment.analysis.assembly.{ EvidenceFactAssembler, RelativeAssessmentAssembler }
import lila.chessjudgment.model.evaluation.JudgmentThresholds
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
        .filter(isBoundToClaim(claim, _, graph))
    val directlyNamedCauses =
      directlyHostedRelativeCauseRecords(claim, graph)
    val directlyHostedCauses =
      directlyNamedCauses.filter(record => isBoundToClaim(claim, record, graph))
    val allNamedCausesBound =
      directlyNamedCauses.size == directlyHostedCauses.size
    val causeHostProofGroups =
      directlyHostedCauses.map(record => directCauseHostProofRecords(record, graph))
    val useCauseHostProof =
      claim.content.isEmpty && causeHostProofGroups.nonEmpty
    val proofRecords =
      if useCauseHostProof then
        causeHostProofGroups.flatten.distinctBy(_.ref.id)
      else
        val directRecords =
          if claim.content.nonEmpty then claimBoundRecords
          else claimBoundRecords ++ claimBoundRecords.flatMap(graph.parentClosure)
        directRecords.distinctBy(_.ref.id)
    val claimBoundLayers =
      proofRecords
        .map(_.ref.layer)
        .toSet
    val requiredGroups = contentRequiredLayerGroups(claim).getOrElse(requiredLayerGroups(claim.family))
    val missingGroups =
      if useCauseHostProof then
        causeHostProofGroups
          .flatMap(records =>
            val layers = records.map(_.ref.layer).toSet
            requiredGroups.filterNot(group => group.exists(layers.contains))
          )
          .distinct
      else
        requiredGroups.filterNot(group => group.exists(claimBoundLayers.contains))
    val hasFamilyProof =
      allNamedCausesBound &&
        (
          if useCauseHostProof then
            causeHostProofGroups.forall(records => familySpecificProof(claim, records, graph))
          else familySpecificProof(claim, proofRecords, graph)
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
      case JudgmentClaimContent.StrategicMechanism(carrier) =>
        exactStrategicMechanismContent(claim, carrier, context)
    }

  private def contentRequiredLayerGroups(claim: JudgmentClaim): Option[List[Set[EvidenceLayer]]] =
    claim.content.map {
      case JudgmentClaimContent.CandidateComparison(_, _) =>
        List(Set(EvidenceLayer.CandidateComparison))
      case JudgmentClaimContent.StrategicMechanism(_) =>
        List(
          Set(EvidenceLayer.StrategicMechanism),
          Set(EvidenceLayer.Line),
          Set(EvidenceLayer.Eval)
        )
    }

  private def exactCandidateComparisonContent(
      claim: JudgmentClaim,
      carrier: EvidenceRef,
      identity: CandidateComparisonSemanticKey,
      graph: TypedEvidenceGraph
  ): Boolean =
    graph.record(carrier).exists {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        exactPlayedTransition(graph).exists { case (_, transition) =>
          val playedMove = JudgmentSubjectBinding.normalizeMove(transition.moveUci)
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

  private def exactStrategicMechanismContent(
      claim: JudgmentClaim,
      carrier: EvidenceRef,
      context: JudgmentAssemblyContext
  ): Boolean =
    val graph = context.evidenceGraph
    EvidenceFactAssembler.exactStrategicMechanisms(context).exists { wrappers =>
      graph.record(carrier).exists {
        case record @ EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents) =>
          exactPlayedTransition(graph).exists { case (_, transition) =>
            claim.primaryLine.exists { line =>
              val lineRecords = graph.records.collect {
                case lineRecord @ EvidenceRecord(lineRef, lineFacts: LineFactEvidence, _)
                    if lineRef.producer == EvidenceProducer.LegalLineProducer &&
                      lineRef.layer == EvidenceLayer.Line &&
                      lineRef.position == transition.from &&
                      lineRef.scope == line.role.scope &&
                      lineRef.line.contains(line) &&
                      lineFacts.line == line =>
                  lineRecord
              }
              lineRecords match
                case EvidenceRecord(lineRef, _, _) :: Nil =>
                  val evalRecords = graph.records.collect {
                    case evalRecord @ EvidenceRecord(evalRef, EvalFactEvidence(payloadLine, _, _, _), evalParents)
                        if evalRef.producer == EvidenceProducer.EngineEvalProducer &&
                          evalRef.layer == EvidenceLayer.Eval &&
                          evalRef.position == lineRef.position &&
                          evalRef.scope == line.role.scope &&
                          evalRef.line.contains(line) &&
                          payloadLine == line &&
                          evalParents == List(lineRef) =>
                      evalRecord
                  }
                  val expectedEvidence =
                    evalRecords match
                      case EvidenceRecord(evalRef, _, _) :: Nil =>
                        (ref :: parents ++ payload.signals.map(_.source) ++ List(lineRef, evalRef)).distinct
                      case _ => Nil
                  wrappers.contains(record) &&
                    ref.layer == EvidenceLayer.StrategicMechanism &&
                    ref.line.contains(line) &&
                    line.role == LineNodeRole.Played &&
                    EvidenceRef.sameMove(line.rootMove, transition.moveUci) &&
                    claim.primaryPosition == ref.position &&
                    claim.scope == ref.scope &&
                    claim.confidence == ref.confidence &&
                    claim.subjectMove.exists(move => EvidenceRef.sameMove(move, transition.moveUci)) &&
                    (claim.subject == line.role.subject || claim.subject == ClaimSubject.Plan) &&
                    exactRefs(claim.evidence, expectedEvidence)
                case _ => false
            }
          }
        case _ => false
      }
    }

  private def exactPlayedTransition(
      graph: TypedEvidenceGraph
  ): Option[(EvidenceRef, MoveTransitionEvidence)] =
    graph.records.collect {
      case EvidenceRecord(ref, payload @ MoveTransitionEvidence(_, from, _), _)
          if ref.producer == EvidenceProducer.MoveTransitionProducer &&
            ref.layer == EvidenceLayer.MoveTransition &&
            ref.scope == EvidenceScope.PlayedTransition &&
            ref.position == from &&
            ref.line.isEmpty &&
            ref.confidence == EvidenceConfidence.LegalReplayVerified =>
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
    val samePosition = record.ref.position == claim.primaryPosition
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
    val samePositionLocal =
      samePosition &&
        claim.subject == ClaimSubject.Position &&
        claim.subjectMove.isEmpty &&
        record.ref.line.isEmpty &&
        positionLocalLayer(record.ref.layer)
    val parentLinked = record.parents.exists(parent => claimEvidenceIds.contains(parent.id))
    val childLinked =
      graph.records.exists(child =>
        claimEvidenceIds.contains(child.ref.id) &&
          child.parents.exists(parent => parent.id == record.ref.id)
      )
    val relativeCauseProofLinked =
      relativeCauseProofSourceIds(claim, graph).contains(record.ref.id)
    val linkedBound =
      (parentLinked || childLinked) &&
        linkedRecordCompatible(claim, record, graph, samePositionLocal)
    sameLine || sameSubjectMove || comparisonLineSupport || transitionDestinationLocal || samePositionLocal || linkedBound ||
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
      graph: TypedEvidenceGraph,
      samePositionLocal: Boolean
  ): Boolean =
    val supportLayer = linkedSupportLayer(record.ref.layer)
    (supportLayer && claim.primaryLine.exists(line => recordLineMatches(record, line, graph))) ||
      (supportLayer && claim.subjectMove.exists(move => recordMentionsMove(record, move, claim.primaryLine, graph))) ||
      samePositionLocal ||
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
      case EvidenceLayer.Board => false
      case _                   => true

  private def claimLayerCompatible(record: EvidenceRecord): Boolean =
    linkedSupportLayer(record.ref.layer) ||
      positionLocalLayer(record.ref.layer)

  private def positionContextLayer(family: ClaimFamily, layer: EvidenceLayer): Boolean =
    if family.isLongTerm then positionLocalLayer(layer)
    else
      layer match
        case EvidenceLayer.Board | EvidenceLayer.Line | EvidenceLayer.Eval =>
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
    val claimEvidenceIds = claim.evidence.map(_.id).toSet
    graph.records
      .filter(record => claimEvidenceIds.contains(record.ref.id))
      .flatMap(record => comparisonLinesForRecord(record, graph))
      .distinct

  private def transitionDestinationsForClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): Set[PositionNodeRef] =
    val claimMoves = claim.subjectMove.toSet ++ claim.primaryLine.map(_.rootMove).toSet
    if claimMoves.isEmpty then Set.empty
    else
      val claimEvidenceIds = claim.evidence.map(_.id).toSet
      graph.records
        .filter(record => claimEvidenceIds.contains(record.ref.id))
        .collect {
          case EvidenceRecord(_, MoveTransitionEvidence(moveUci, _, to), _) if claimMoves.contains(moveUci) =>
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
      case payload: MoveMotifEvidence =>
        payload.moveUci == move
      case MoveTransitionEvidence(moveUci, _, _) =>
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
      case EvidenceLayer.Board | EvidenceLayer.StrategicMechanism =>
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
            EvidenceLayer.PlanCausalEvent,
            EvidenceLayer.Eval
          ),
          Set(EvidenceLayer.Eval, EvidenceLayer.CandidateComparison)
        )
      case ClaimFamily.Strategic =>
        List(
          Set(EvidenceLayer.StrategicMechanism, EvidenceLayer.RelativeCause),
          Set(
            EvidenceLayer.Board,
            EvidenceLayer.PawnStructure,
            EvidenceLayer.Strategic,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.FeatureAnchor
          ),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.MoveTransition,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.RelativeCause
          )
        )
      case ClaimFamily.PawnStructure =>
        List(
          Set(EvidenceLayer.StrategicMechanism, EvidenceLayer.RelativeCause),
          Set(EvidenceLayer.PawnStructure, EvidenceLayer.StructuralDelta, EvidenceLayer.Strategic),
          Set(
            EvidenceLayer.Line,
            EvidenceLayer.MoveTransition,
            EvidenceLayer.StructuralDelta,
            EvidenceLayer.CandidateComparison,
            EvidenceLayer.RelativeAssessment,
            EvidenceLayer.RelativeCause
          )
        )
      case ClaimFamily.Opening =>
        List(
          Set(EvidenceLayer.StrategicMechanism),
          Set(EvidenceLayer.OpeningContext),
          Set(EvidenceLayer.ApplicabilityAssessment),
          Set(EvidenceLayer.FeatureAnchor)
        )
      case ClaimFamily.Plan =>
        List(Set(EvidenceLayer.StrategicMechanism, EvidenceLayer.PlanCausalEvent))
      case ClaimFamily.Defensive =>
        List(
          Set(
            EvidenceLayer.ThreatPressure,
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
          Set(EvidenceLayer.Line, EvidenceLayer.Board, EvidenceLayer.StructuralDelta, EvidenceLayer.Relation),
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
        defensiveProof(claim, records, graph)
      case ClaimFamily.Strategic =>
        strategicProof(claim, records, graph)
      case ClaimFamily.PawnStructure =>
        pawnStructureProof(claim, records, graph)
      case ClaimFamily.Opening =>
        openingProof(records)
      case ClaimFamily.Plan =>
        planProof(claim, records, graph)
      case ClaimFamily.Conversion =>
        conversionProof(claim, records, graph)
      case ClaimFamily.Material =>
        materialProof(records, graph)
      case ClaimFamily.Evaluation =>
        evaluationProof(claim, records, graph)

  private def strategicProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val relativeCauses = relativeCausesBoundToClaimEvidence(claim, records)
    val hasObservation =
      records.exists(strategicObservation) ||
        claim.content.exists {
          case JudgmentClaimContent.StrategicMechanism(_) =>
            records.exists {
              case EvidenceRecord(_, _: PlanCausalEventEvidence, _) => true
              case _                                                => false
            }
          case _ => false
        }
    val hasCausalSupport = records.exists(longTermCausalSupport(claim, _, graph))
    val hasStrategicAnchor = records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        relativeCauses.contains(cause) &&
          graph.comparisonFor(cause).nonEmpty && strategicRelativeCauseHasProof(cause, graph)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        relativeCauses.isEmpty && payload.hasSustainedActionableContrast
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        relativeCauses.isEmpty && payload.canAnchorStrategicClaim
      case _ =>
        false
    }
    hasObservation && hasCausalSupport && hasStrategicAnchor

  private def pawnStructureProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val relativeCauses = relativeCausesBoundToClaimEvidence(claim, records)
    val hasObservation = records.exists(pawnStructureObservation)
    val hasCausalSupport = records.exists(longTermCausalSupport(claim, _, graph))
    val hasPawnAnchor = records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        relativeCauses.contains(cause) &&
          graph.comparisonFor(cause).nonEmpty &&
          strategicRelativeCauseHasProof(cause, graph) &&
          pawnStructureCause(cause, graph)
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        relativeCauses.isEmpty &&
          payload.sustainedActionableComparisons.exists(_.axis.kind == StrategicAxisKind.PawnBreak)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        relativeCauses.isEmpty && payload.canAnchorPawnStructureClaim
      case _ =>
        false
    }
    hasObservation && hasCausalSupport && hasPawnAnchor

  private def openingProof(records: List[EvidenceRecord]): Boolean =
    val certifyingPriorThemes =
      records
        .collect {
          case EvidenceRecord(_, OpeningContextEvidence(_, _, _, Some(selection)), _)
              if selection.canCertifyOpeningClaim =>
            selection.prior.themes
        }
        .flatten
        .toSet
    val certifyingAssessments =
      records.collect {
        case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), _)
            if assessment.canCertifyOpeningClaim =>
          assessment
      }
    val observedApplicableThemes =
      certifyingAssessments
        .flatMap(assessment => assessment.observedThemes.intersect(assessment.supportedThemes))
        .toSet
        .intersect(certifyingPriorThemes)
    val hasCurrentBoardAnchor =
      records.exists {
        case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) =>
          anchor.hasPositiveStrength &&
            anchor.canCorroborateOpeningPrior &&
            observedApplicableThemes.contains(anchor.theme)
        case _ =>
          false
      }
    StrategicMechanismEvidence.openingClaimSupported(records) &&
      certifyingPriorThemes.nonEmpty &&
      observedApplicableThemes.nonEmpty &&
      hasCurrentBoardAnchor

  private def strategicObservation(record: EvidenceRecord): Boolean =
    record.payload match
      case _: BoardFactEvidence | _: PawnStructureFactEvidence =>
        true
      case payload: StrategicFactEvidence =>
        payload.hasTypedSupport
      case payload: StructuralDeltaEvidence =>
        payload.hasTypedOutput
      case FeatureAnchorEvidence(anchor) =>
        anchor.hasPositiveStrength
      case _ =>
        false

  private def pawnStructureObservation(record: EvidenceRecord): Boolean =
    record.payload match
      case _: PawnStructureFactEvidence =>
        true
      case payload: StrategicFactEvidence =>
        payload.kind == StrategicFactKind.Structure && payload.hasTypedSupport
      case payload: StructuralDeltaEvidence =>
        payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure) ||
          payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructureDelta)
      case _ =>
        false

  private def longTermCausalSupport(
      claim: JudgmentClaim,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Boolean =
    record.payload match
      case RelativeCauseFactEvidence(cause) =>
        graph.comparisonFor(cause).nonEmpty &&
          strategicRelativeCauseHasProof(cause, graph) &&
          causeBoundToClaim(claim, cause, graph)
      case payload: StrategicMechanismContrastEvidence =>
        payload.hasActionableContrast &&
          payload.sustainability.hasSustainedPv &&
          claim.primaryLine.exists(line =>
            line == payload.referenceLine || line == payload.candidateLine
          )
      case payload: StructuralDeltaEvidence =>
        payload.hasTypedOutput &&
          transitionBoundToClaim(claim, payload.moveUci, payload.line)
      case MoveTransitionEvidence(moveUci, _, _) =>
        transitionBoundToClaim(claim, moveUci, record.ref.line, Some(record.ref.scope))
      case _: LineFactEvidence =>
        claim.primaryLine.nonEmpty && record.ref.line == claim.primaryLine
      case _ =>
        false

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

  private def pawnStructureCause(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.kind == RelativeCauseKind.PawnWeaknessTarget ||
      cause.kind == RelativeCauseKind.PawnBreakOpportunity ||
      cause.proof.exists(proof =>
          graph
            .relativeCauseStrategicMechanisms(proof.directProof)
            .exists { case (_, payload) =>
              payload.signals.exists(signal =>
                signal.axis.exists(axis =>
                  axis.kind == StrategicAxisKind.PawnBreak &&
                    RelativeCauseKind.strategicAxisCanProveCause(cause.kind, axis, cause.sourceSide)
                )
              )
            } ||
          graph
            .relativeCauseStrategicContrasts(proof.directProof)
            .exists { case (_, payload) =>
              payload
                .sustainedCauseComparisons(cause.kind, cause.sourceSide)
                .exists(_.axis.kind == StrategicAxisKind.PawnBreak)
            } ||
          graph
            .relativeCauseTransitionConsequences(cause.kind, proof.directProof)
            .exists { case (_, _, consequence) => pawnStructureConsequence(consequence.kind) }
      )

  private def pawnStructureConsequence(kind: TransitionConsequenceKind): Boolean =
    kind match
      case TransitionConsequenceKind.OpenFileGain |
          TransitionConsequenceKind.SemiOpenFileGain |
          TransitionConsequenceKind.WeakPawnTargetCreated |
          TransitionConsequenceKind.PawnTensionGain |
          TransitionConsequenceKind.PawnTensionResolution |
          TransitionConsequenceKind.PassedPawnProgress |
          TransitionConsequenceKind.PassedPawnConcession |
          TransitionConsequenceKind.PromotionPressureGain |
          TransitionConsequenceKind.PromotionPressureConcession =>
        true
      case _ =>
        false

  private def planProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    val relativeCauses = relativeCausesBoundToClaimEvidence(claim, records)
    if relativeCauses.nonEmpty then
      relativeCauses.exists(cause =>
        graph.comparisonFor(cause).nonEmpty &&
          strategicRelativeCauseHasProof(cause, graph) &&
          RelativeCauseKind.requiresExactPlanResult(cause.kind)
      )
    else
      records.exists {
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          payload.planComparison.exists(_.hasPlanDelta)
        case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) =>
          ref.confidence != EvidenceConfidence.Heuristic && payload.canAnchorPlanClaim
        case _ =>
          false
      }

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
    // The exact root -> induced response -> PlanResult sequence is deliberately
    // both the WrongMoveOrder tactical anchor and its concrete tactical line.
    val exactMoveOrderTacticalCauses =
      relativeCauses.filter(exactMoveOrderPlanResultTacticalProof(_, graph))
    val hasMaterialSwingCauseCarrier =
      relativeCauses.exists(_.kind == RelativeCauseKind.MaterialSwing)
    val hasTacticalAnchor =
      records.exists {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.canAnchorTacticalClaim
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasConcreteRelationProof
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          graph.comparisonFor(cause).nonEmpty &&
            tacticalRelativeCause(cause.kind) &&
            (cause.hasOwnedTacticalProof(graph) || exactMoveOrderTacticalCauses.contains(cause))
        case _ =>
          false
      }
    val hasConcreteLine =
      records.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          lineHasTacticalProof(payload)
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasLineProof
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.hasLineProof
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
        case _ =>
          false
      } || exactMoveOrderTacticalCauses.nonEmpty
    val hasEngineProof =
      records.exists {
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
        case record @ EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
          engineBackedComparison(graph, assessment.primaryComparisonEvidence)
            .exists(engineComparisonProvesTactic)
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          recordEngineBacked(record) &&
          engineComparisonProvesTactic(fact)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.hasEngineOrForcingProof
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          engineBackedComparison(graph, cause.comparisonEvidence).exists(fact =>
            engineComparisonProvesTactic(fact) &&
              (
              cause.hasOwnedTacticalProof(graph) ||
                exactMoveOrderTacticalCauses.contains(cause) ||
                fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
                fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
            )
          )
        case _ =>
          false
      }
    !hasMaterialSwingCauseCarrier && hasTacticalAnchor && hasConcreteLine && hasEngineProof

  private def defensiveProof(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    records.exists {
      case record @ EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.isProofSignalDefensivePressure &&
          (!branchLocalThreatPressure(record) || claimIsBranchLocal(claim))
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

  private def claimIsBranchLocal(claim: JudgmentClaim): Boolean =
    claim.scope == EvidenceScope.ThreatLine ||
      claim.primaryLine.exists(_.role == LineNodeRole.Threat)

  private def branchLocalThreatPressure(record: EvidenceRecord): Boolean =
    record.ref.scope == EvidenceScope.ThreatLine ||
      record.ref.line.exists(_.role == LineNodeRole.Threat)

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
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
          mate.nonEmpty
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
      case record @ EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
        claim.content.exists {
          case JudgmentClaimContent.CandidateComparison(carrier, identity) =>
            carrier == ref &&
              CandidateComparisonSemanticKey.from(fact) == identity &&
              recordIds(ref.id) &&
              recordEngineBacked(record)
          case _ => false
        }
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        graph
          .candidateComparisonRecord(assessment.primaryComparisonEvidence)
          .exists(record =>
            recordIds.contains(record.ref.id) &&
              recordEngineBacked(record) &&
              graph.comparisonFor(assessment).exists(_.kind == CandidateComparisonKind.PlayedVsBest)
          )
      case _ =>
        false
    }

  private def recordEngineBacked(record: EvidenceRecord): Boolean =
    record.ref.confidence == EvidenceConfidence.EngineBacked ||
      recordHasMate(record)

  private def recordHasMate(record: EvidenceRecord): Boolean =
    record.payload match
      case EvalFactEvidence(_, _, mate, _) =>
        mate.nonEmpty
      case _ =>
        false

  private def engineBackedComparison(
      graph: TypedEvidenceGraph,
      ref: EvidenceRef
  ): Option[CandidateComparisonFact] =
    graph
      .candidateComparisonRecord(ref)
      .filter(recordEngineBacked)
      .collect {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact
      }

  private def engineComparisonProvesTactic(
      fact: CandidateComparisonFact
  ): Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
      fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP

  private def tacticalRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Tactical)

  private def materialResultCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Material)

  private def materialRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    materialResultCause(cause.kind) &&
      cause.hasOwnedTypedDepth(graph)

  private def materialLineProof(payload: LineFactEvidence): Boolean =
    payload.hasMaterialConsequence ||
      payload.hasRecaptureRecoveryConsequence ||
      payload.hasMaterialRecaptureChain ||
      payload.hasMaterialRecoveryWindow ||
      payload.hasProofSignalMaterialEvent

  private def conversionRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    (
      cause.kind == RelativeCauseKind.ConversionMiss ||
        cause.kind == RelativeCauseKind.ConversionSecured ||
        cause.kind == RelativeCauseKind.RecaptureRecoveryWindow
    ) &&
      cause.hasOwnedTypedDepth(graph)

  private def strategicRelativeCauseHasProof(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.hasOwnedAdmissibleLongTermProof(graph)

  /** The only PlanResult-only Tactical path replays the direct WrongMoveOrder
    * evidence from F. C selects public carriers but is not truth-sufficient:
    * every declared direct source is rebuilt from graph facts before an
    * admitted carrier's endpoint differential can prove the claim.
    */
  private def exactMoveOrderPlanResultTacticalProof(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    val directSection = cause.proof.map(_.directProof)
    val comparisonAndBinding = for
      section <- directSection
      _ <- Option.when(
        cause.kind == RelativeCauseKind.WrongMoveOrder &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
          cause.attribution.directProofEligible &&
          cause.attribution.rootMoveMatched &&
          section.role == RelativeCauseProofRole.DirectProof &&
          section.strength == RelativeCauseProofStrength.Primary &&
          RelativeCauseKind.sourceAttributionCompatible(
            cause.kind,
            cause.sourceSide,
            cause.attribution.kind
          )
      )(())
      comparisonRecord <- graph.candidateComparisonRecord(cause.comparisonEvidence)
      comparison <- comparisonRecord.payload match
        case CandidateComparisonEvidence(value) => Some(value)
        case _                                  => None
      if comparisonRecord.ref == cause.comparisonEvidence &&
        comparisonRecord.ref.confidence == EvidenceConfidence.EngineBacked &&
        comparison.hasDistinctRootMoves &&
        comparison.comparison.verdict.isActionableLoss &&
        comparison.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP
      binding <- graph.relativeCauseBinding(cause)
      if binding.eventLine == comparison.referenceLine
    yield (section, comparison, binding)

    comparisonAndBinding.exists { case (section, comparison, causeBinding) =>
      val inventories = RelativeAssessmentAssembler.planResultEndpointInventories(
        graph,
        comparison,
        cause.comparisonEvidence.position
      )
      inventories.forSide(cause.sourceSide).exists { case (sourceInventory, counterpartInventory) =>
        val sources = section.sourceRefs
        val sourceRefsUnique =
          sources.nonEmpty &&
            sources.distinct.size == sources.size &&
            sources.map(_.id).distinct.size == sources.size
        val sourceObservations = Option.when(sourceRefsUnique)(sources.map { source =>
          graph.record(source) match
            case Some(EvidenceRecord(ref, event: PlanCausalEventEvidence, _))
                if ref == source =>
              for
                _ <- Option.when(
                  cause.supportEvidence.contains(source) &&
                    RootOwnedEffectPolicy.sameCausalRootOccurrence(
                      source.position,
                      cause.comparisonEvidence.position
                    ) &&
                    RootOwnedEffectPolicy.planEventOwnsRoot(
                      source,
                      event,
                      causeBinding.eventLine,
                      comparison.comparison.mover
                    )
                )(())
                (assessment, response) <-
                  ComparisonEndpointEffectObservationPolicy
                    .exactInducedResponseMoveOrder(
                      comparison,
                      cause.sourceSide,
                      source,
                      event,
                      graph
                    )
                actor <- RootCausalActor.fromPosition(
                  cause.comparisonEvidence.position,
                  causeBinding.eventLine.rootMove
                )
                exactBinding <- EvidenceObjectBinding.inducedResponseMoveOrderBinding(
                  comparison,
                  cause.sourceSide,
                  source,
                  event,
                  actor,
                  assessment,
                  response,
                  causeBinding.eventLine
                )
                directBinding = exactBinding.copy(proofRole = Some(RelativeCauseProofRole.DirectProof))
                if directBinding.source == source &&
                  directBinding.provenance.isEmpty &&
                  directBinding.proofRole.contains(RelativeCauseProofRole.DirectProof)
                observation <- ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                  cause.comparisonEvidence.position,
                  causeBinding.eventLine,
                  source,
                  event,
                  assessment,
                  Some(directBinding),
                  selectedInducedResponse = Some(response)
                )
                inventoryObservation <- sourceInventory.exactInducedResponseObservationFor(source)
                if inventoryObservation == observation
              yield source -> observation
            case _ => None
        }).getOrElse(Nil)
        val resolved = sourceObservations.flatten
        val admittedSources = RelativeCauseConstructionAdmission
          .admittedDirectChannels(cause, graph)
          .map(_.binding.source)
          .toSet
        val sourceIdentityPreserved =
          sourceRefsUnique &&
            resolved.size == sources.size &&
            resolved.map(_._1).distinct.size == sources.size
        sourceIdentityPreserved &&
          resolved.filter { case (source, _) => admittedSources(source) }.exists { case (source, observation) =>
            RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
              sourceInventory,
              counterpartInventory,
              source,
              observation
            )
          }
      }
    }

  private def lineHasTacticalProof(payload: LineFactEvidence): Boolean =
    payload.rootMove.exists(rootMove =>
      payload.consequencesForRootMove(rootMove).exists(consequence =>
        LineConsequenceKind.tacticalDriver(consequence.kind)
      )
    )

  private def defensiveRelativeCause(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Defensive)
