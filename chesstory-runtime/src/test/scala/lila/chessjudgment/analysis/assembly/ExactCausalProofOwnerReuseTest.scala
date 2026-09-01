package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.ExactCausalProofResultSet

class ExactCausalProofOwnerReuseTest extends munit.FunSuite:

  test("a partial exact-owner cache retains its owner and creates every missing occurrence"):
    val dependencies = List(
      "a" * 64,
      "b" * 64,
      "c" * 64
    )
    val resultSet = ExactCausalProofResultSet.from(dependencies)
    val created = collection.mutable.ListBuffer.empty[String]
    val result = ExactCausalProofOwnerReuse.resolve(
      family = "test-causal-family",
      existing = List((dependencies(1), resultSet, "cached-b"))
    )(
      dependencies.zip(List("a", "b", "c"))
    ) { (value, exactResultSet) =>
      assertEquals(exactResultSet, resultSet)
      created += value
      s"created-$value"
    }

    assertEquals(result, List("created-a", "cached-b", "created-c"))
    assertEquals(created.toList, List("a", "c"))

  test("a complete exact-owner cache does not execute the derivation"):
    val dependencies = List("a" * 64, "b" * 64)
    val resultSet = ExactCausalProofResultSet.from(dependencies)
    var derivations = 0
    val result = ExactCausalProofOwnerReuse.resolve(
      family = "test-causal-family",
      existing = List(
        (dependencies.head, resultSet, "cached-a"),
        (dependencies(1), resultSet, "cached-b")
      )
    )(
      {
        derivations += 1
        dependencies.zip(List("a", "b"))
      }
    ) { (value, _) => s"created-$value" }

    assertEquals(result, List("cached-a", "cached-b"))
    assertEquals(derivations, 0)

  test("duplicate, stale, and conflicting exact owners fail instead of hiding graph corruption"):
    val dependency = "a" * 64
    val resultSet = ExactCausalProofResultSet.from(List(dependency))
    intercept[IllegalStateException] {
      ExactCausalProofOwnerReuse.resolve(
        family = "test-causal-family",
        existing = List(
          (dependency, resultSet, "first"),
          (dependency, resultSet, "second")
        )
      )(List(dependency -> "a"))((value, _) => value)
    }
    intercept[IllegalStateException] {
      val incompleteSet = ExactCausalProofResultSet.from(List(dependency, "b" * 64))
      ExactCausalProofOwnerReuse.resolve(
        family = "test-causal-family",
        existing = List((dependency, incompleteSet, "stale"))
      )(Nil)((value: String, _) => value)
    }

    val otherDependency = "b" * 64
    val conflictingSet = ExactCausalProofResultSet.from(List(otherDependency))
    intercept[IllegalStateException] {
      ExactCausalProofOwnerReuse.resolve(
        family = "test-causal-family",
        existing = List(
          (dependency, resultSet, "first"),
          (otherDependency, conflictingSet, "second")
        )
      )(List(dependency -> "a", otherDependency -> "b"))((value, _) => value)
    }
