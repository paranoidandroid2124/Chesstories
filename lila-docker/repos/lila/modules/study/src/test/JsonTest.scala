package lila.study

import chess.variant.Variant

import lila.core.LightUser
import lila.db.BSON
import lila.db.BSON.{ Reader, Writer }
import lila.db.dsl.Bdoc
import lila.study.Helpers.*
import lila.tree.{ Node, Root }

import BSONHandlers.given

class JsonTest extends munit.FunSuite:

  val user = LightUser.fallback(UserName("nt9"))

  test("Json writes"):
    PgnFixtures.roundTrip
      .zip(JsonFixtures.all)
      .foreach: (pgn, expected) =>
        val result = StudyPgnImport.result(pgn, List(user)).toOption.get
        val imported = result.root.cleanCommentIds
        val json = writeTree(imported, result.variant)
        assertEquals(json, expected)

  given Conversion[Bdoc, Reader] = Reader(_)
  val treeBson = summon[BSON[Root]]
  val w = new Writer

  test("Json writes with BSONHandlers"):
    PgnFixtures.roundTrip
      .zip(JsonFixtures.all)
      .foreach: (pgn, expected) =>
        val result = StudyPgnImport.result(pgn, List(user)).toOption.get
        val imported = result.root.cleanCommentIds
        val afterBson = treeBson.reads(treeBson.writes(w, imported))
        val json = writeTree(afterBson, result.variant)
        assertEquals(json, expected)

  extension (root: Root)
    def cleanCommentIds: Root =
      root.toNewRoot.cleanup.toRoot

  def writeTree(tree: Root, variant: Variant): String = Node.partitionTreeJsonWriter
    .writes(lila.study.TreeBuilder(tree, variant))
    .toString
