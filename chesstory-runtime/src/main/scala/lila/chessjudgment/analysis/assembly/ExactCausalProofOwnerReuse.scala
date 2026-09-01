package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.ExactCausalProofResultSet

/** Reuses one deterministic causal-family result set without recomputing a
  * complete cache hit or mistaking a partial graph for a complete result.
  */
private[assembly] object ExactCausalProofOwnerReuse:

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
