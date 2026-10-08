package io.chesstory.runtime

import munit.FunSuite

final class CommentaryIdeaIdentityTest extends FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val source = Source("user", "idea-identity-test")
  private val origin = Origin(source, source, source, source)
  private val knight = Piece("white", "knight", "g1")
  private val predicate = Predicate("support", subject = Some(knight),
    target = Some(Piece("white", "pawn", "d4")))
  private val key = CommentaryIdea.Key(Nil, "g1f3", List("g1f3"),
    Predicate("legal_move", subject = Some(knight), square = Some("f3")), loss = false)
  private val left = Fact("left", Path("reference", Comparison("choice", "reference")), predicate, Ply(1))
  private val right = Fact("right", Path("alternative", Comparison("choice", "alternative")), predicate, Ply(1))

  // Supply already-read values directly; this test neither loads a position nor runs analysis.
  private def member(id: String, a: Fact = left, b: Fact = right,
      leftValue: String = "present", rightValue: String = "absent"): CommentaryIdea.Member =
    val comparisons = List(a.path, b.path).collect {
      case Path(_, Comparison(choice, _), _) => CompareMoves(choice, Nil, "g1f3", "b1c3")
    }.distinct
    val difference = FactComparison("difference", a.id, b.id, "different")
    val request = Request(id, origin, comparisons ++ List(a, b, difference))
    assert(CommentaryProposal.validRequests(List(request)))
    def reading(fact: Fact, value: String): Reading = Reading(fact.id, "observed", result = Some(
      Result("known", samples = List(Sample(fact.id + "-sample", "line", fact.path.binding,
        Nil, 0, fact.at.horizon, Some(value), "known")))))
    val readings = comparisons.map(value => Reading(value.id)) ++ List(
      reading(a, leftValue), reading(b, rightValue), Reading(difference.id, "supported"))
    CommentaryIdea.Member(Entry(request, readings), key, a)

  private def text(members: CommentaryIdea.Member*): String = CommentaryIdea.Idea(key, members.toList).text
  private def effects(value: String): Int = value.split("The named branches differ in", -1).length - 1

  test("equal wording retains different target conditions") {
    val other = predicate.copy(target = Some(Piece("white", "pawn", "e4")))
    val first = member("first")
    val second = member("second", left.copy(predicate = other), right.copy(predicate = other))
    assertEquals(text(first), text(second))
    assertEquals(effects(text(first, second)), 2)
  }

  test("equal wording retains different observation times") {
    val first = member("first")
    val second = member("second", left.copy(at = Ply(2)), right.copy(at = Ply(2)))
    assertEquals(text(first), text(second))
    assertEquals(effects(text(first, second)), 2)
  }

  test("equal wording retains different named paths") {
    val first = member("first")
    val second = member("second",
      left.copy(path = left.path.copy(source = Comparison("other-choice", "reference"))),
      right.copy(path = right.path.copy(source = Comparison("other-choice", "alternative"))))
    assertEquals(text(first), text(second))
    assertEquals(effects(text(first, second)), 2)
  }

  test("the same facts and values deduplicate before the existing two-effect limit") {
    val first = member("first")
    val duplicate = member("duplicate")
    val later = member("later", left.copy(at = Ply(2)), right.copy(at = Ply(2)))
    assertEquals(effects(text(first, duplicate)), 1)
    assertEquals(effects(text(first, duplicate, later)), 2)
    val last = member("last", left.copy(at = Ply(3)), right.copy(at = Ply(3)))
    assertEquals(effects(text(first, later, last)), 2)
  }

  test("the same facts with different observed values remain distinct and ordered") {
    val first = member("first")
    val changed = member("changed", leftValue = "absent", rightValue = "present")
    val result = text(first, changed)
    assertEquals(effects(result), 2)
    assert(result.indexOf("present versus absent") < result.indexOf("absent versus present"))
  }
