package lila.analyse

import chess.{ ByColor, Color }
import chess.eval.Eval.Cp
import scalalib.Maths.isCloseTo

class AccuracyPercentTest extends munit.FunSuite:

  import AccuracyPercent.*
  type AccMap = ByColor[AccuracyPercent]

  def compute(cps: List[Int]): Option[AccMap] =
    gameAccuracy(Color.white, cps.map(Cp(_)))

  test("calculates white-first game accuracy"):
    val cases: List[(String, List[Int], Option[(Double, Double, Double, Double)])] = List(
      ("empty game", Nil, None),
      ("single move", List(15), None),
      ("two good moves", List(15, 15), Some((100d, 1d, 100d, 1d))),
      ("white blunders on first move", List(-900, -900), Some((10d, 5d, 100d, 1d))),
      ("black blunders on first move", List(15, 900), Some((100d, 1d, 10d, 5d))),
      ("both blunder on first move", List(-900, 0), Some((10d, 5d, 10d, 5d))),
      ("20 perfect moves", List.fill(20)(15), Some((100d, 1d, 100d, 1d))),
      ("20 perfect moves and a white blunder", List.fill(20)(15) :+ -900, Some((50d, 5d, 100d, 1d))),
      ("21 perfect moves and a black blunder", List.fill(21)(15) :+ 900, Some((100d, 1d, 50d, 5d))),
      ("5 average moves on each side", List.fill(5)(List(-50, 15)).flatten, Some((76d, 8d, 76d, 8d))),
      ("50 average moves on each side", List.fill(50)(List(-50, 15)).flatten, Some((76d, 8d, 76d, 8d))),
      ("50 mediocre moves on each side", List.fill(50)(List(-135, 15)).flatten, Some((54d, 8d, 54d, 8d))),
      ("50 terrible moves on each side", List.fill(50)(List(-435, 15)).flatten, Some((20d, 8d, 20d, 8d)))
    )
    cases.foreach:
      case (name, cps, None) => assert(compute(cps).isEmpty, name)
      case (name, cps, Some((white, whiteTolerance, black, blackTolerance))) =>
        val accuracy = compute(cps).get
        assert(isCloseTo(accuracy.white.value, white, whiteTolerance), name)
        assert(isCloseTo(accuracy.black.value, black, blackTolerance), name)

  def computeBlack(cps: List[Int]) = gameAccuracy(Color.black, cps.map(Cp(_)))

  test("calculates black-first game accuracy"):
    val cases: List[(String, List[Int], Option[(Double, Double, Double, Double)])] = List(
      ("empty game", Nil, None),
      ("single move", List(15), None),
      ("two good moves", List(15, 15), Some((100d, 1d, 100d, 1d))),
      ("black blunders on first move", List(900, 900), Some((100d, 1d, 10d, 5d))),
      ("white blunders on first move", List(15, -900), Some((10d, 5d, 100d, 1d))),
      ("both blunder on first move", List(900, 0), Some((10d, 5d, 10d, 5d)))
    )
    cases.foreach:
      case (name, cps, None) => assert(computeBlack(cps).isEmpty, name)
      case (name, cps, Some((white, whiteTolerance, black, blackTolerance))) =>
        val accuracy = computeBlack(cps).get
        assert(isCloseTo(accuracy.white.value, white, whiteTolerance), name)
        assert(isCloseTo(accuracy.black.value, black, blackTolerance), name)
