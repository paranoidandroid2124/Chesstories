package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

object JudgmentClaimAssembler:

  private[assembly] def propose(context: JudgmentAssemblyContext): List[JudgmentClaim] =
    val allocator = JudgmentProvenanceAllocator.forInput(context.input)
    requireUniqueClaimProducers(
      List.concat(
        pawnStructureClaims(context, allocator),
        relativeCauseClaims(context, allocator),
        evaluationClaims(context, allocator)
      )
    )

  private def requireUniqueClaimProducers(claims: List[JudgmentClaim]): List[JudgmentClaim] =
    val duplicateIds = claims.groupBy(_.id).collect {
      case (id, owners) if owners.size > 1 => id
    }.toList.sorted
    require(
      duplicateIds.isEmpty,
      s"claim ids have multiple producers: ${duplicateIds.mkString(",")}"
    )
    claims

  private def judgmentClaimFromEvidence(
      id: String,
      family: ClaimFamily,
      subject: ClaimSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      moveUci: Option[String],
      evidence: List[EvidenceRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      content: Option[JudgmentClaimContent] = None
  ): JudgmentClaim =
    JudgmentClaim(
      id = id,
      family = family,
      subject = subject,
      primaryPosition = primaryPosition,
      primaryLine = primaryLine,
      subjectMove = moveUci,
      evidence = evidence,
      scope = scope,
      confidence = confidence,
      content = content
    )

  private def pawnStructureClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.evidenceGraph.records.flatMap {
      case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, parents)
          if context.evidenceGraph.proofEligible(record) &&
            ref.line.exists(_.role == LineNodeRole.Played) &&
            (
              payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure) ||
                payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructureDelta)
            ) =>
        val primaryLine = ref.line
        val evidence = longTermClaimEvidence(ref :: parents)
        Option.when(evidence.nonEmpty) {
          judgmentClaimFromEvidence(
            id = allocator.evidenceId(s"claim:pawn-structure:${allocator.key(ref.id)}"),
            family = ClaimFamily.PawnStructure,
            subject = ClaimSubject.PlayedMove,
            primaryPosition = ref.position,
            primaryLine = primaryLine,
            moveUci = primaryLine.map(_.rootMove),
            evidence = evidence,
            scope = ref.scope,
            confidence = ref.confidence
          )
        }
      case _ =>
        None
      }

  private def evaluationClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.playedTransition.toList.flatMap { played =>
      val playedMove = EvidenceRef.normalizeMove(played.moveUci)
      val playedMoves = Set(playedMove).filter(_.nonEmpty)
      context.evidenceGraph.records.flatMap {
        case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _)
            if ref.producer == EvidenceProducer.RelativeMoveProducer &&
              ref.layer == EvidenceLayer.CandidateComparison &&
              ref.position == played.from &&
              playedMoves.nonEmpty &&
              JudgmentSubjectBinding.comparisonBinding(fact, playedMoves) != SubjectBindingClass.Other =>
          JudgmentSubjectBinding.uniquePlayedEndpoint(fact, playedMoves).map { endpoint =>
            val identity = CandidateComparisonSemanticKey.from(fact)
            judgmentClaimFromEvidence(
              id = allocator.evidenceId(
                s"claim:evaluation:comparison:${allocator.key(ref.id)}"
              ),
              family = ClaimFamily.Evaluation,
              subject = ClaimSubject.PlayedMove,
              primaryPosition = ref.position,
              primaryLine = Some(endpoint),
              moveUci = Some(played.moveUci),
              evidence = List(ref),
              scope = ref.scope,
              confidence = ref.confidence,
              content = Some(JudgmentClaimContent.CandidateComparison(ref, identity))
            )
          }
        case _ =>
          None
      }
    }

  private def relativeCauseClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if RelativeCauseConstructionAdmission.initiallyReady(cause, context.evidenceGraph) =>
        relativeCauseClaimsFromRecord(context, allocator, ref, cause)
      case _ =>
        Nil
    }

  private def relativeCauseClaimsFromRecord(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      ref: EvidenceRef,
      cause: RelativeCauseFact
  ): List[JudgmentClaim] =
    val subjectLine = context.evidenceGraph.requiredRelativeCauseBinding(cause).eventLine
    val family = ClaimFamily.fromCause(cause.kind)
    val evidence = ref :: cause.proofSources
    require(
      evidence.map(_.id).distinct.size == evidence.size,
      s"relative Cause '${ref.id}' claim evidence repeats an exact owner"
    )
    List(
      judgmentClaimFromEvidence(
        id = relativeCauseClaimId(context.evidenceGraph, allocator, family, cause, subjectLine, ref),
        family = family,
        subject = subjectForRelativeCause(subjectLine),
        primaryPosition = ref.position,
        primaryLine = Some(subjectLine),
        moveUci = Some(subjectLine.rootMove),
        evidence = evidence,
        scope = ref.scope,
        confidence = ref.confidence
      )
    )

  private def relativeCauseClaimId(
      graph: TypedEvidenceGraph,
      allocator: JudgmentProvenanceAllocator,
      family: ClaimFamily,
      cause: RelativeCauseFact,
      subjectLine: LineNodeRef,
    ref: EvidenceRef
  ): String =
    val key = RelativeCauseSemanticKey.from(cause, ref.id, graph)
    allocator.evidenceId(
      s"claim:${allocator.key(family)}:relative-cause:${allocator.key(key.kind)}:${allocator.key(key.comparisonKind)}:${allocator.key(key.sourceSide)}:${allocator.key(subjectLine.rootMove)}:${allocator.key(ref.id)}"
    )

  private def subjectForRelativeCause(line: LineNodeRef): ClaimSubject =
    line.role.claimSubject.getOrElse(
      throw IllegalStateException("a non-claim line carrier cannot own a relative cause")
    )

  private def longTermClaimEvidence(refs: List[EvidenceRef]): List[EvidenceRef] =
    refs
      .filterNot(ref => ClaimEvidenceSemantics.longTermSupportExcludedLayer(ref.layer))
      .distinctBy(_.id)
