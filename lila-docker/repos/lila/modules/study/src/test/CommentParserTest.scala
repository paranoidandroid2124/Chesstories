package lila.study

import chess.Centis

class CommentParserTest extends LilaTest:

  import chess.format.pgn.Comment
  import scala.language.implicitConversions
  given Conversion[String, Comment] = Comment(_)

  val C = CommentParser

  test("parses comments"):
    List(
      "" -> "",
      "Hello there" -> "Hello there",
      "[%clk 10:40:33] Hello there" -> "Hello there",
      "Hello there [%clk 10:40:33]" -> "Hello there",
      "Hello there [%clk 10:40:33][%clk 10:40:33]" -> "Hello there",
      "Hello there [%clk\n10:40:33]" -> "Hello there"
    ).foreach { case (input, expected) => assertEquals(C(input).comment, Comment(expected)) }

  test("parses clocks and elapsed move times"):
    List(
      "" -> None,
      "Hello there" -> None,
      "[%clk 10:40:33]" -> Some(Centis(3843300)),
      "[%clk 00:00:33]" -> Some(Centis(3300)),
      "[%clk 1:40:33]" -> Some(Centis(603300)),
      "[%clk 10:40:33] Hello there" -> Some(Centis(3843300)),
      "Hello there [%clk 10:40:33]" -> Some(Centis(3843300)),
      "Hello there [%clk 10:40:33] something else" -> Some(Centis(3843300)),
      "Hello there [%clk 10:40:33][%clk 10:40:33]" -> Some(Centis(3843300)),
      "Hello there [%clk\n10:40:33]" -> Some(Centis(3843300)),
      "Hello there [%clk 2:10] something else" -> Some(Centis(780000)),
      "Hello there [%clk 2:10.33] something else" -> Some(Centis(783300)),
      "d=29, pd=Bb7, mt=00:01:01, tl=00:37:47, s=27938 kN/s, n=1701874274, pv=e4 Bb7 exf5 exf5 d5 Qf6 Rab1 Rae8 g3 Bc8 Kg2 Rxe1 Rxe1, tb=0, R50=49, wv=0.45," -> Some(
        Centis(100 * 47 + 37 * 60 * 100)
      ),
      "d=31, sd=101, mt=225513, tl=3380487, s=55170, n=13264996, pv=d5 Nb4, tb=45, h=0.0, ph=0.0, wv=0.83, R50=50, Rd=-9, Rr=-1000, mb=+0+0+0+0+0," -> Some(
        Centis(338049)
      ),
      "Hello there [%clk 10:40:33.55] something else" -> Some(Centis(3843355)),
      "Hello there [%clk 10:40:33.556] something else" -> Some(Centis(3843356)),
      "Hello there [%clk 2:.:13] something else" -> None,
      "Hello there [%clk 2..:30:0] something else" -> None,
      "Hello there [%clk :30] something else" -> None
    ).foreach { case (input, expected) => assertEquals(C(input).clock, expected) }
    List(
      "Hello there [%emt 2:10] something else" -> Some(Centis(780000)),
      "Hello there [%emt 2:10.33] something else" -> Some(Centis(783300))
    ).foreach { case (input, expected) => assertEquals(C(input).emt, expected) }

  test("parses shapes"):
    List(
      ("", "", 0, None, None),
      ("Hello there", "Hello there", 0, None, None),
      ("[%csl Gb4,Yd5,Rf6] Hello there", "Hello there", 3, None, None),
      ("Hello there [%csl Gb4,Yd5,Rf6]", "Hello there", 3, None, None),
      ("Hello there [%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]", "Hello there", 6, None, None),
      ("Hello there [%csl\nGb4,Yd5,Rf6]", "Hello there", 3, None, None),
      ("Hello there [%csl\nGb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]", "Hello there", 6, None, None),
      ("Hello there [%csl Gb4,Yd5,Rf6][%cal\nGe2e4,Ye2d4,Re2g4]", "Hello there", 6, None, None),
      ("Hello there [%csl \n\n Gb4,Yd5,Rf6][%cal\nGe2e4,Ye2d4,Re2g4]", "Hello there", 6, None, None),
      (
        "Hello there [%clk 10:40:33][%csl \n\n Gb4,Yd5,Rf6][%cal\nGe2e4,Ye2d4,Re2g4]",
        "Hello there",
        6,
        Some(Centis(3843300)),
        None
      ),
      (
        "Hello there [%emt 10:40:33][%clk 10:40:33][%csl \n\n Gb4,Yd5,Rf6][%cal\nGe2e4,Ye2d4,Re2g4]",
        "Hello there",
        6,
        Some(Centis(3843300)),
        Some(Centis(3843300))
      )
    ).foreach { case (input, comment, shapeCount, clock, emt) =>
      val parsed = C(input)
      assertEquals(parsed.comment, Comment(comment))
      assertEquals(parsed.shapes.value.size, shapeCount)
      assertEquals(parsed.clock, clock)
      assertEquals(parsed.emt, emt)
    }
