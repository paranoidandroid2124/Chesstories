package lila.study

import chess.format.pgn.PgnStr

import scala.language.implicitConversions

import lila.db.BSON
import lila.db.BSON.{ Reader, Writer }
import lila.db.dsl.Bdoc
import lila.study.BSONHandlers.given
import lila.tree.Root

// in lila.study to have access to PgnImport
class BsonHandlersTest extends munit.FunSuite:

  given Conversion[String, PgnStr] = PgnStr(_)
  given Conversion[PgnStr, String] = _.value
  given Conversion[Bdoc, Reader] = Reader(_)

  import Helpers.*

  val treeBson = summon[BSON[Root]]
  val w = new Writer

  test("Tree writes.reads == identity"):
    List(PgnFixtures.pgn8).foreach: pgn =>
      val x = StudyPgnImport.result(pgn, Nil).toOption.get.root
      val y = treeBson.reads(treeBson.writes(w, x))
      assertEquals(y, x.withoutClockTrust)
