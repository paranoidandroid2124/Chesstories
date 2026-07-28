package lila.chessjudgment.model.judgment

enum OnlyMoveConstraintDisposition:
  case DiagnosticOnly
  case ConcreteCauseQualifier

enum OnlyMoveMechanismRelation:
  case SameChannelAssociation

  def licensesCausalBecause: Boolean =
    false

final case class OnlyMoveCauseQualifier(
    comparisonEvidence: EvidenceRef,
    causeEvidence: EvidenceRef,
    referenceLine: LineNodeRef,
    relation: OnlyMoveMechanismRelation
):
  def licensesCausalBecause: Boolean = relation.licensesCausalBecause

final case class OnlyMoveConstraintResolution(
    comparisonEvidence: EvidenceRef,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    disposition: OnlyMoveConstraintDisposition,
    qualifiers: List[OnlyMoveCauseQualifier]
)

/** Single authority for interpreting an only-move candidate-set result.
  *
  * Candidate comparison evidence establishes uniqueness but is never an
  * independent Cause. A qualifier is admitted only for a selected, direct-
  * ready reference resource on an exact played-vs-best relation. When the
  * played move is itself best, BestVsSecond owns that relation.
  */
object OnlyMoveConstraintPolicy:

  def constraintFact(
      comparisonEvidence: EvidenceRef,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Option[CandidateComparisonFact] =
    val normalizedPlayedMoves = playedMoves.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    graph
      .candidateComparison(comparisonEvidence)
      .filter(fact =>
        fact.hasDistinctRootMoves &&
          graph.candidateSetFor(fact).exists(_.onlyMove) &&
          exactPlayedOrientation(fact, normalizedPlayedMoves)
      )

  def isConstraint(
      comparisonEvidence: EvidenceRef,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    constraintFact(comparisonEvidence, graph, playedMoves).nonEmpty

  def qualifiesConcreteCause(
      causeEvidence: EvidenceRef,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    constraintFact(cause.comparisonEvidence, graph, playedMoves).exists { fact =>
      val binding = graph.requiredRelativeCauseBinding(cause)
      PlayerFacingCauseReadinessPolicy.ready(cause, causeEvidence, graph) &&
        cause.sourceSide == RelativeCauseSourceSide.Reference &&
        cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
        binding.sourceSide == RelativeCauseSourceSide.Reference &&
        binding.eventLine == fact.referenceLine &&
        EvidenceRef.sameMove(binding.eventLine.rootMove, fact.referenceLine.rootMove)
    }

  def qualifier(
      causeEvidence: EvidenceRef,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Option[OnlyMoveCauseQualifier] =
    Option.when(qualifiesConcreteCause(causeEvidence, cause, graph, playedMoves)) {
      val fact = constraintFact(cause.comparisonEvidence, graph, playedMoves).get
      OnlyMoveCauseQualifier(
        comparisonEvidence = cause.comparisonEvidence,
        causeEvidence = causeEvidence,
        referenceLine = fact.referenceLine,
        relation = OnlyMoveMechanismRelation.SameChannelAssociation
      )
    }

  def resolve(
      comparisonEvidence: EvidenceRef,
      causes: Iterable[(RelativeCauseFact, EvidenceRef)],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Option[OnlyMoveConstraintResolution] =
    constraintFact(comparisonEvidence, graph, playedMoves).map { fact =>
      val qualifiers = causes.toList
        .filter { case (cause, _) => cause.comparisonEvidence.id == comparisonEvidence.id }
        .flatMap { case (cause, causeRef) =>
          qualifier(causeRef, cause, graph, playedMoves).toList
        }
        .distinctBy(_.causeEvidence.id)
        .sortBy(_.causeEvidence.id)
      OnlyMoveConstraintResolution(
        comparisonEvidence = comparisonEvidence,
        referenceLine = fact.referenceLine,
        candidateLine = fact.candidateLine,
        disposition =
          if qualifiers.nonEmpty then OnlyMoveConstraintDisposition.ConcreteCauseQualifier
          else OnlyMoveConstraintDisposition.DiagnosticOnly,
        qualifiers = qualifiers
      )
    }

  def resolveAll(
      graph: TypedEvidenceGraph,
      causes: Iterable[(RelativeCauseFact, EvidenceRef)],
      playedMoves: Set[String]
  ): List[OnlyMoveConstraintResolution] =
    graph.records
      .collect {
        case EvidenceRecord(ref, CandidateComparisonEvidence(_), _)
            if isConstraint(ref, graph, playedMoves) =>
          ref
      }
      .distinctBy(_.id)
      .flatMap(resolve(_, causes, graph, playedMoves))
      .sortBy(_.comparisonEvidence.id)

  private def exactPlayedOrientation(
      fact: CandidateComparisonFact,
      playedMoves: Set[String]
  ): Boolean =
    val candidatePlayed = lineIsPlayed(fact.candidateLine, playedMoves)
    val referencePlayed = lineIsPlayed(fact.referenceLine, playedMoves)
    fact.kind match
      case CandidateComparisonKind.PlayedVsBest =>
        candidatePlayed && !referencePlayed
      case CandidateComparisonKind.BestVsSecond =>
        referencePlayed && !candidatePlayed
      case CandidateComparisonKind.PlayedVsAlternative | CandidateComparisonKind.ReferenceVsAlternative =>
        false

  private def lineIsPlayed(line: LineNodeRef, playedMoves: Set[String]): Boolean =
    playedMoves(EvidenceRef.normalizeMove(line.rootMove))
