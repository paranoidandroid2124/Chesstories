package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Reuses one deterministic causal-family result set without recomputing a
  * complete cache hit or mistaking a partial graph for a complete result.
  */
private[assembly] object ExactCausalProofOwnerReuse:

  /** Family-neutral record identity for one certified compared-line proof.
    * The family proof object still owns chess certification, while the common
    * producer owns payload construction; this value only centralizes graph/cache
    * ownership shared by every family.
    */
  final case class ComparedLineProofRecord(
      semanticId: String,
      occurrenceId: String,
      dependencyId: String,
      referenceLine: LineNodeRef,
      parentSources: List[EvidenceRef],
      proofPathOccurrenceIds: List[String]
  ):
    require(semanticId.matches("[0-9a-f]{64}"), "a compared-line proof needs one semantic id")
    require(occurrenceId.matches("[0-9a-f]{64}"), "a compared-line proof needs one occurrence id")
    require(dependencyId.matches("[0-9a-f]{64}"), "a compared-line proof needs one dependency id")
    require(
      parentSources.nonEmpty && parentSources == parentSources.sortBy(_.id) &&
        parentSources.map(_.id).distinct.size == parentSources.size,
      "a compared-line proof needs canonical distinct parents"
    )
    require(
      proofPathOccurrenceIds.nonEmpty &&
        proofPathOccurrenceIds == proofPathOccurrenceIds.sorted &&
        proofPathOccurrenceIds.distinct.size == proofPathOccurrenceIds.size,
      "a compared-line proof needs canonical distinct proof paths"
    )

    val referencePosition = parentSources
      .find(_.line.contains(referenceLine))
      .map(_.position)
      .getOrElse(
        throw IllegalArgumentException("a compared-line proof needs its reference-line parent")
      )

    def evidenceIdInput(family: String): String =
      s"causal-proof:$family:$semanticId:$occurrenceId:$dependencyId:${proofPathOccurrenceIds.mkString(":")}"

  /** Shared graph/cache kernel for exact compared-line L2 contracts. It does
    * not derive a chess fact, reinterpret a payload, or act as another
    * producer; the caller supplies a typed family certificate and payload constructor.
    */
  def comparedLineRecords[A, S](
      family: String,
      context: JudgmentAssemblyContext,
      input: ExactPlayedVsBestCausalInput,
      existingPayload: EvidencePayload => Option[(String, ExactCausalProofResultSet)]
  )(
      derive: => List[A]
  )(
      semantic: A => S,
      record: A => ComparedLineProofRecord,
      create: (A, ComparedLineProofRecord, ExactCausalProofResultSet) => EvidenceRecord
  ): List[EvidenceRecord] =
    val existing = context.evidenceGraph.recordsFor(input.comparison.referenceLine).flatMap { owner =>
      if context.evidenceGraph.proofEligible(owner) then
        existingPayload(owner.payload).map { case (dependencyId, resultSet) =>
          (dependencyId, resultSet, owner)
        }
      else None
    }
    lazy val derivedDependencies =
      val certified = derive
      certified.groupBy(exact => record(exact).semanticId).foreach { case (semanticId, occurrences) =>
        require(
          occurrences.map(semantic).distinct.size == 1,
          s"$family semantic id '$semanticId' resolved to conflicting proofs"
        )
      }
      require(
        certified.map(exact => record(exact).occurrenceId).distinct.size == certified.size,
        s"one exact $family occurrence may be produced only once"
      )
      certified.map(exact => record(exact).dependencyId -> exact)

    resolve(
      family = family,
      existing = existing
    )(
      derivedDependencies
    ) { (exact, resultSet) =>
      val exactRecord = record(exact)
      require(
        exactRecord.referenceLine == input.comparison.referenceLine,
        s"a $family proof must remain on its demanded reference line"
      )
      create(exact, exactRecord, resultSet)
    }.sortBy(_.ref.id)

  def resolve[A, B](
      family: String,
      existing: List[(String, ExactCausalProofResultSet, B)]
  )(
      derive: => List[(String, A)]
  )(
      create: (A, ExactCausalProofResultSet) => B
  ): List[B] =
    completeHit(family, existing).getOrElse {
      complete(family, derive, existing)(create)
    }

  private def completeHit[B](
      family: String,
      existing: List[(String, ExactCausalProofResultSet, B)]
  ): Option[List[B]] =
    validatedExistingResultSet(family, existing).flatMap { resultSet =>
      Option.when(resultSet.exactlyOwns(existing.map(_._1)))(existing.map(_._3))
    }

  private def complete[A, B](
      family: String,
      derived: List[(String, A)],
      existing: List[(String, ExactCausalProofResultSet, B)]
  )(
      create: (A, ExactCausalProofResultSet) => B
  ): List[B] =
    ensure(
      derived.map(_._1).distinct.size == derived.size,
      s"one exact $family dependency may be derived only once"
    )
    if derived.isEmpty then
      ensure(existing.isEmpty, s"a cached $family proof no longer follows from its exact dependencies")
      Nil
    else
      val resultSet = ExactCausalProofResultSet.from(derived.map(_._1))
      val existingResultSet = validatedExistingResultSet(family, existing)
      ensure(
        existingResultSet.forall(_ == resultSet),
        s"a cached $family result set conflicts with its exact dependencies"
      )
      val derivedIds = derived.map(_._1).toSet
      val existingByDependency = existing.map { case (dependencyId, _, value) =>
        dependencyId -> value
      }.toMap
      ensure(
        existingByDependency.keySet.subsetOf(derivedIds),
        s"a cached $family proof no longer follows from its exact dependencies"
      )
      derived.map { case (dependencyId, exact) =>
        existingByDependency.getOrElse(dependencyId, create(exact, resultSet))
      }

  private def validatedExistingResultSet[B](
      family: String,
      existing: List[(String, ExactCausalProofResultSet, B)]
  ): Option[ExactCausalProofResultSet] =
    ensure(
      existing.map(_._1).distinct.size == existing.size,
      s"one exact $family dependency has multiple graph owners"
    )
    val resultSets = existing.map(_._2).distinct
    ensure(
      resultSets.size <= 1,
      s"cached $family owners disagree about their complete result set"
    )
    resultSets.headOption.map { resultSet =>
      ensure(
        existing.forall { case (dependencyId, _, _) => resultSet.contains(dependencyId) },
        s"a cached $family owner is outside its declared result set"
      )
      resultSet
    }

  private def ensure(condition: Boolean, message: => String): Unit =
    if !condition then throw IllegalStateException(message)
