package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction }

enum PlayerFacingPositionAction:
  case AutomaticTerminal(value: lila.chessjudgment.model.line.AutomaticTerminal)
  case ForcedSingleMove(
      moveUci: String,
      mover: chess.Color,
      supportingEvaluation: CandidateLineEvaluation,
      drawClaims: List[DrawClaimAction]
  )
  case DominantDrawClaim(claims: List[DrawClaimAction])

enum PlayerFacingPrimary:
  case MoveVerdict(comparisonEvidence: EvidenceRef)
  case BestChoice(comparisonEvidence: EvidenceRef)

final class EvidenceBackedJudgmentPacket private (
    private[chessjudgment] val assembly: JudgmentAssemblyContext,
    val primary: PlayerFacingPrimary,
    val occurrenceExplanations: List[CertifiedOccurrenceExplanation]
):
  def candidateLines: List[CandidateLineNode] = assembly.lines

  def evidenceGraph: TypedEvidenceGraph = assembly.evidenceGraph

object EvidenceBackedJudgmentPacket:
  private[chessjudgment] def fromAssembly(
      assembly: JudgmentAssemblyContext
  ): Option[EvidenceBackedJudgmentPacket] =
    for
      _ <- Option.when(structurallyClosed(assembly))((): Unit)
      primary <- selectPrimary(
        assembly.relativeAssessments,
        assembly.evidenceGraph
      )
      explanations <- certifiedOccurrenceExplanations(assembly)
    yield new EvidenceBackedJudgmentPacket(
      assembly,
      primary,
      explanations
    )

  /** Sole projection from quantitative Assessment to the public verdict shape.
    * Neither comparison kind can identify or admit an Explanation proof.
    */
  private def selectPrimary(
      relativeAssessments: List[RelativeMoveAssessment],
      graph: TypedEvidenceGraph
  ): Option[PlayerFacingPrimary] =
    relativeAssessments match
      case assessment :: Nil =>
        val ref = assessment.primaryComparisonEvidence
        graph.candidateComparisonRecord(ref).flatMap {
          case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
              if fact.kind == CandidateComparisonKind.PlayedVsBest &&
                fact.referenceLine == assessment.reference.ref &&
                fact.candidateLine == assessment.candidate.ref &&
                fact.hasDistinctRootMoves =>
            Some(PlayerFacingPrimary.MoveVerdict(ref))
          case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
              if fact.kind == CandidateComparisonKind.BestVsSecond &&
                EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove) &&
                EvidenceRef.sameMove(assessment.candidate.ref.rootMove, assessment.reference.ref.rootMove) &&
                fact.referenceLine == assessment.reference.ref &&
                fact.candidateSelection.role == LineNodeRole.Alternative &&
                fact.candidateSelection.rank == 2 &&
                fact.hasDistinctRootMoves =>
            Some(PlayerFacingPrimary.BestChoice(ref))
          case _ => None
        }
      case _ => None

  private def certifiedOccurrenceExplanations(
      assembly: JudgmentAssemblyContext
  ): Option[List[CertifiedOccurrenceExplanation]] =
    val causeRecords = assembly.evidenceGraph.records.filter(
      _.payload.isInstanceOf[OccurrenceExplanationCauseEvidence]
    )
    val certified = causeRecords.map(
      CertifiedOccurrenceExplanation.fromRecord(_, assembly.evidenceGraph)
    )
    Option.when(certified.forall(_.nonEmpty)) {
      val exact = certified.flatten.sortBy(_.causeEvidence.id)
      require(
        exact.map(_.causeEvidence.id).distinct.size == exact.size &&
          exact.map(value =>
            value.subject.occurrenceId -> value.rootOwnedProof.occurrenceIdentity.producerIndependentKey
          ).distinct.size == exact.size,
        "each requested proof occurrence needs one packet Cause owner"
      )
      exact
    }

  private def structurallyClosed(assembly: JudgmentAssemblyContext): Boolean =
    val rootPresent = assembly.root.nonEmpty
    val graphRecords = assembly.evidenceGraph.records
    val graphIds = graphRecords.map(_.ref.id)
    val uniqueGraphIds = graphIds.distinct.size == graphIds.size
    val registeredById = graphRecords.map(record => record.ref.id -> record).toMap
    def registered(ref: EvidenceRef): Boolean =
      registeredById.get(ref.id).exists(_.ref == ref)
    val positionRefs = assembly.positions.map(_.ref).toSet
    val legalLineRefs = assembly.legalLines.map(_.ref).toSet
    val assessmentLineRefs = assembly.lines.map(_.ref).toSet
    val uniquePositionNodes = positionRefs.size == assembly.positions.size
    val uniqueLegalLineNodes = legalLineRefs.size == assembly.legalLines.size
    val uniqueAssessmentProjections = assessmentLineRefs.size == assembly.lines.size
    val uniqueLegalLineOwnerEvidence =
      assembly.legalLines.map(_.evidence.id).distinct.size == assembly.legalLines.size
    val uniqueTransitionEvidence =
      assembly.transitions.map(_.evidence.id).distinct.size == assembly.transitions.size
    def exactlyOneRole[A, R](items: List[A], role: R)(itemRole: A => R): Boolean =
      items.count(item => itemRole(item) == role) == 1
    val playedTransition = assembly.transitions.filter(_.role == TransitionEdgeRole.Played) match
      case edge :: Nil => Some(edge)
      case _           => None
    val referenceLine = assembly.lines.filter(_.role == LineNodeRole.BestReference) match
      case line :: Nil => Some(line)
      case _           => None
    val playedUsesReferenceOwner =
      for
        edge <- playedTransition
        line <- referenceLine
      yield EvidenceRef.sameMove(edge.moveUci, line.ref.rootMove)
    val primaryRolesUnique =
      exactlyOneRole(assembly.positions, PositionNodeRole.Before)(_.role) &&
        exactlyOneRole(assembly.lines, LineNodeRole.BestReference)(_.role) &&
        exactlyOneRole(assembly.transitions, TransitionEdgeRole.Played)(_.role) &&
        playedUsesReferenceOwner.exists { sharedOwner =>
          if sharedOwner then
            !assembly.lines.exists(_.role == LineNodeRole.Played) &&
              !assembly.transitions.exists(_.role == TransitionEdgeRole.Reference)
          else
            exactlyOneRole(assembly.lines, LineNodeRole.Played)(_.role) &&
              exactlyOneRole(assembly.transitions, TransitionEdgeRole.Reference)(_.role)
        }
    val graphClosed =
      graphRecords.forall(record =>
        positionRefs.contains(record.ref.position) &&
          record.ref.line.forall(legalLineRefs.contains) &&
          record.payloadLineRefs.forall(legalLineRefs.contains) &&
          record.parents.forall(registered)
      )
    def exactLineReplay(
        line: LegalLineNode,
        payload: LineFactEvidence
    ): Boolean =
      val expectedReplay = assembly.lineReplay(line.ref).map(_.replaySteps)
      val actualReplay =
        payload.lineReplaySteps.map(step =>
          step.copy(moveUci = EvidenceRef.normalizeMove(step.moveUci))
        )
      val lineMatches = payload.line == line.ref
      val rootMoveMatches = payload.lineReplayMoves.headOption.exists(move =>
        EvidenceRef.sameMove(move, line.ref.rootMove)
      )
      val replayMatches = expectedReplay.contains(actualReplay)
      lineMatches && rootMoveMatches && replayMatches
    def exactLineEvidence(line: LegalLineNode): Boolean =
      registeredById.get(line.evidence.id).exists {
        case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          ref == line.evidence &&
            ref.producer == EvidenceProducer.LegalLineProducer &&
            ref.layer == EvidenceLayer.Line &&
            ref.line.contains(line.ref) &&
            ref.scope == EvidenceScope.LegalLine &&
            exactLineReplay(line, payload)
        case _ =>
          false
      }
    def exactEvaluationEvidence(line: CandidateLineNode): Boolean =
      graphRecords.collect {
        case record @ EvidenceRecord(_, CandidateLineEvaluationEvidence(payloadLine, _), _)
            if payloadLine == line.ref =>
          record
      } match
        case List(EvidenceRecord(ref, CandidateLineEvaluationEvidence(_, evaluation), parents)) =>
          val exactAuthority = evaluation match
            case CandidateLineEvaluation.EngineSearch(_) =>
              ref.producer == EvidenceProducer.EngineEvalProducer &&
                ref.confidence == EvidenceConfidence.EngineBacked
            case CandidateLineEvaluation.ExactAutomaticTerminal(_, _) =>
              ref.producer == EvidenceProducer.LegalLineProducer &&
                ref.confidence == EvidenceConfidence.LegalReplayVerified
          exactAuthority &&
            ref.layer == EvidenceLayer.Eval &&
            ref.position == line.lineEvidence.position &&
            ref.line.contains(line.ref) &&
            ref.scope == line.role.scope &&
            evaluation == line.evaluation &&
            parents == List(line.lineEvidence)
        case _ =>
          false
    def exactTransitionEvidence(transition: MoveTransitionEdge): Boolean =
      positionRefs.contains(transition.from) &&
        positionRefs.contains(transition.to) &&
        registeredById.get(transition.evidence.id).exists {
          case record @ EvidenceRecord(ref, payload @ MoveTransitionEvidence(moveUci, from, to, _), parents) =>
            ref == transition.evidence &&
              ref.producer == EvidenceProducer.MoveTransitionProducer &&
              ref.layer == EvidenceLayer.MoveTransition &&
              ref.position == transition.from &&
              ref.line.isEmpty &&
              ref.scope == transition.role.scope &&
              moveUci == transition.moveUci &&
              from == transition.from &&
              to == transition.to &&
              parents.isEmpty &&
              payload.canonicalTransitionProof.exists(
                _.provesMove(moveUci, from, to, ref.scope)
              ) &&
              transition.matches(record)
          case _ =>
            false
        }
    val legalLineOwnerRecords = graphRecords.filter(record =>
      record.ref.producer == EvidenceProducer.LegalLineProducer &&
        record.ref.layer == EvidenceLayer.Line &&
        record.ref.scope == EvidenceScope.LegalLine
    )
    val exactLegalLineOwnerSet =
      legalLineOwnerRecords.size == assembly.legalLines.size &&
        legalLineOwnerRecords.map(_.ref).toSet == assembly.legalLines.map(_.evidence).toSet
    val assessmentProjectionClosed =
      assembly.lines.forall(candidate =>
        assembly.legalLines.filter(_.ref == candidate.ref) match
          case owner :: Nil => owner.evidence == candidate.lineEvidence
          case _            => false
      )
    val transitionEvidenceIds = assembly.transitions.map(_.evidence.id).toSet
    val exactLegalLineOccurrenceClosure =
      assembly.lineReplays.keySet == legalLineRefs &&
        assembly.lineRootOccurrences.keySet == legalLineRefs &&
        assembly.transitionReplays.keySet == transitionEvidenceIds &&
        assembly.transitions.size == assembly.legalLines.size &&
        assembly.lineRootOccurrences.values.map(_.transitionEvidenceId).toSet == transitionEvidenceIds &&
        assembly.lineRootOccurrences.values.map(_.transitionEvidenceId).toSet.size ==
          assembly.lineRootOccurrences.size &&
        assembly.legalLines.map { line =>
          for
            replay <- assembly.lineReplay(line.ref)
            rootStep <- replay.replaySteps.headOption
            occurrence <- assembly.lineRootOccurrence(line.ref)
            transition <- assembly.transitions.filter(_.evidence.id == occurrence.transitionEvidenceId) match
              case exact :: Nil => Some(exact)
              case _            => None
            transitionReplay <- assembly.transitionReplay(transition)
            exactTransitionStep <- transitionReplay.replaySteps match
              case exact :: Nil => Some(exact)
              case _            => None
          yield
            occurrence.start == transition.from &&
              occurrence.destination == transition.to &&
              rootStep == exactTransitionStep &&
              EvidenceRef.sameMove(line.ref.rootMove, transition.moveUci)
        }.forall(_.contains(true))
    val nodeAndTransitionEvidenceClosed =
      assembly.legalLines.forall(exactLineEvidence) &&
        assembly.lines.forall(exactEvaluationEvidence) &&
        assembly.transitions.forall(exactTransitionEvidence)
    val passedPawnResultEvidenceClosed =
      graphRecords.forall {
        case EvidenceRecord(ref, event: PassedPawnResultEventEvidence, _) =>
          ref.line.contains(event.rootLine) &&
            event.rootTransition.line.contains(event.rootLine)
        case _ =>
          true
      }
    val assessmentRecords =
      graphRecords.collect {
        case EvidenceRecord(ref, RelativeAssessmentEvidence(assessment), parents) =>
          (ref, assessment, parents)
      }
    val comparisonRecords =
      graphRecords.collect {
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          record -> fact
      }
    val soleAssessmentComparison =
      (assessmentRecords, comparisonRecords) match
        case (List((_, assessment, _)), List((record, _))) =>
          record.ref == assessment.primaryComparisonEvidence
        case _ => false
    val comparisonLinesByRef =
      assembly.lines.map(line => line.ref -> line).toMap
    def canonicalLineParents(line: CandidateLineNode): List[EvidenceRef] =
      line.lineEvidence ::
        graphRecords.collect {
          case EvidenceRecord(evalRef, CandidateLineEvaluationEvidence(payloadLine, _), _)
              if payloadLine == line.ref =>
            evalRef
        }
    val comparisonParentsCanonical =
      comparisonRecords.forall { case (record, fact) =>
        val comparedLineParents =
          List(fact.referenceLine, fact.candidateLine)
            .flatMap(comparisonLinesByRef.get)
            .flatMap(canonicalLineParents)
        val parentIds = comparedLineParents.map(_.id)
        parentIds.distinct.size == parentIds.size &&
          record.parents.size == comparedLineParents.size &&
          record.parents.toSet == comparedLineParents.toSet
      }
    def comparisonFactBound(ref: EvidenceRef): Boolean =
      assembly.evidenceGraph.candidateComparisonRecord(ref).exists {
        case EvidenceRecord(recordRef, CandidateComparisonEvidence(fact), _) =>
          recordRef.producer == EvidenceProducer.RelativeMoveProducer &&
            recordRef.layer == EvidenceLayer.CandidateComparison &&
            recordRef.line.contains(fact.candidateLine) &&
            assessmentLineRefs.contains(fact.referenceLine) &&
            assessmentLineRefs.contains(fact.candidateLine)
        case _ =>
          false
      }
    val assessmentsClosed =
      assessmentRecords.forall { case (recordRef, assessment, parents) =>
        val refs = List(assessment.evidence, assessment.primaryComparisonEvidence)
        val expectedParents = assessment.canonicalParents
        val objectsBound =
          assembly.lines.contains(assessment.reference) &&
            assembly.lines.contains(assessment.candidate) &&
            assembly.transitions.contains(assessment.played) &&
            assessment.referenceTransition.forall(assembly.transitions.contains)
        val primaryComparisonBound =
          comparisonFactBound(assessment.primaryComparisonEvidence)
        recordRef == assessment.evidence &&
        recordRef.producer == EvidenceProducer.RelativeMoveProducer &&
        recordRef.layer == EvidenceLayer.RelativeAssessment &&
        recordRef.position == assessment.played.from &&
        recordRef.line.contains(assessment.candidate.ref) &&
        recordRef.scope == EvidenceScope.Counterfactual &&
        objectsBound &&
        refs.forall(registered) &&
        expectedParents.contains(parents) &&
        primaryComparisonBound
      }
    val visiting = scala.collection.mutable.Set.empty[String]
    val visited = scala.collection.mutable.Set.empty[String]
    def visit(id: String): Boolean =
      if visited.contains(id) then true
      else if visiting.contains(id) then false
      else
        visiting += id
        val closed =
          registeredById.get(id).exists(record =>
            record.parents.forall(parent => registered(parent) && visit(parent.id))
          )
        visiting -= id
        if closed then visited += id
        closed
    val parentDagAcyclic =
      uniqueGraphIds && graphRecords.forall(record => visit(record.ref.id))
    rootPresent &&
      assembly.positions.nonEmpty &&
      assembly.legalLines.nonEmpty &&
      assembly.lines.nonEmpty &&
      graphRecords.nonEmpty &&
      uniquePositionNodes &&
      uniqueLegalLineNodes &&
      uniqueAssessmentProjections &&
      uniqueLegalLineOwnerEvidence &&
      uniqueTransitionEvidence &&
      primaryRolesUnique &&
      uniqueGraphIds &&
      graphClosed &&
      exactLegalLineOwnerSet &&
      assessmentProjectionClosed &&
      exactLegalLineOccurrenceClosure &&
      nodeAndTransitionEvidenceClosed &&
      passedPawnResultEvidenceClosed &&
      assessmentsClosed &&
      soleAssessmentComparison &&
      comparisonParentsCanonical &&
      parentDagAcyclic
