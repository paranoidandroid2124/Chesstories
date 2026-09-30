package io.chesstory.runtime

import play.api.libs.json.*

/** Public assertions exercise real admitted positions, not hand-made certified witnesses. */
class CertifiedObservationDeliveryTest extends munit.FunSuite:
  private def statements(fen: String, move: String): List[JsObject] =
    val result = RuntimeProtocol.evaluate(Json.obj(
      "schema_version" -> RuntimeProtocol.RequestSchema,
      "input" -> Json.obj("fen" -> fen, "playedMoveUci" -> move)))
    assertEquals(result.httpStatus, 200, Json.stringify(result.body))
    val commentary = (result.body \ "move_commentary").as[JsObject]
    assertEquals((commentary \ "primary").toOption, None)
    (commentary \ "observations" \ "statements").as[List[JsObject]]

  private def only(facts: List[JsObject], kind: String): JsObject =
    val selected = facts.filter(fact => (fact \ "kind").as[String] == kind)
    assertEquals(selected.size, 1, s"expected one $kind: ${Json.stringify(JsArray(facts))}")
    selected.head

  test("a pinned recapturer exposes geometric availability, legal absence and the exact king-exposure path"):
    val facts = statements("4k3/4n3/8/3p4/4P3/8/8/4R1K1 w - - 0 1", "e4d5")
    val inventory = only(facts, "recapture_resources")
    assertEquals((inventory \ "phase").as[String], "after")
    assertEquals((inventory \ "captured").as[JsObject],
      Json.obj("side" -> "black", "role" -> "pawn", "square" -> "d5"))
    assertEquals((inventory \ "geometric_recapturers").as[List[JsObject]],
      List(Json.obj("role" -> "knight", "square" -> "e7")))
    assertEquals((inventory \ "legal_recaptures").as[List[JsObject]], Nil)
    val restricted = (inventory \ "restricted_recaptures").as[List[JsObject]]
    assertEquals(restricted.size, 1)
    assertEquals((restricted.head \ "resource" \ "destination").as[String], "d5")
    assertEquals((restricted.head \ "resource" \ "mode").as[String], "controlled_destination")
    assertEquals((restricted.head \ "king_square").as[String], "e8")
    assertEquals((restricted.head \ "post_move_controllers").as[List[JsObject]],
      List(Json.obj("role" -> "rook", "square" -> "e1")))
    val pin = (restricted.head \ "absolute_pin_paths").as[List[JsObject]].head
    assertEquals((pin \ "pinner" \ "square").as[String], "e1")
    assertEquals((pin \ "pinned" \ "square").as[String], "e7")
    assertEquals((pin \ "geometry" \ "direction").as[JsObject],
      Json.obj("file_step" -> 0, "rank_step" -> 1))
    assert((pin \ "geometry" \ "squares").as[List[String]].contains("e8"))
    // The current capture's unknown history is a separate fact from zero legal replies.
    assertEquals((only(facts, "capture") \ "recapture").as[String], "unknown")
    assertEquals((inventory \ "premise_ids").as[List[String]].size, 2)

  test("double check retains both checkers and post-move control of illegal king destinations"):
    val inventory = only(statements("4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1", "e2b5"), "check_responses")
    assertEquals((inventory \ "checked_side").as[String], "black")
    assertEquals((inventory \ "checkers").as[List[JsObject]].map(p => (p \ "square").as[String]).toSet,
      Set("b5", "e1"))
    val responses = (inventory \ "responses").as[List[JsObject]]
    assert(responses.nonEmpty)
    assert(responses.forall(r => (r \ "modes").as[List[String]].contains("king_move")))
    assert(responses.forall(r => (r \ "resource" \ "movement" \ "side").as[String] == "black"))
    val e7 = (inventory \ "controlled_king_destinations").as[List[JsObject]]
      .find(d => (d \ "resource" \ "destination").as[String] == "e7").get
    assert((e7 \ "controllers").as[List[JsObject]].exists(p => (p \ "square").as[String] == "e1"))
    assertEquals((inventory \ "terminal").as[String], "ongoing")

  test("a selected check response uses the before board and preserves simultaneous response modes"):
    val interpose = only(statements("4r2k/8/8/8/8/8/3B4/4K3 w - - 0 1", "d2e3"), "check_response")
    assertEquals((interpose \ "phase").as[String], "before")
    assertEquals((interpose \ "king_square").as[String], "e1")
    assertEquals((interpose \ "response" \ "modes").as[List[String]], List("interpose"))
    assertEquals((interpose \ "response" \ "resource" \ "move_uci").as[String], "d2e3")
    val kingCapture = only(statements("7k/8/8/8/8/8/4r3/4K3 w - - 0 1", "e1e2"), "check_response")
    assertEquals((kingCapture \ "response" \ "modes").as[List[String]].toSet,
      Set("king_move", "capture_checker"))
    assertEquals((kingCapture \ "response" \ "resource" \ "capture").as[JsObject],
      Json.obj("side" -> "black", "role" -> "rook", "square" -> "e2"))

  test("pawn topology includes the unmoved supported pawn and distinguishes support direction from phalanx"):
    val facts = statements("k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "c4c5")
      .filter(f => (f \ "kind").as[String] == "pawn_structure")
    val moved = facts.find(f => (f \ "before" \ "square").as[String] == "c4").get
    val unmoved = facts.find(f => (f \ "before" \ "square").as[String] == "d5").get
    assertEquals((moved \ "after" \ "square").as[String], "c5")
    assert((moved \ "before" \ "connections").as[List[JsObject]].contains(
      Json.obj("peer" -> "d5", "kind" -> "geometric_support", "direction" -> "supports")))
    assert((unmoved \ "before" \ "connections").as[List[JsObject]].contains(
      Json.obj("peer" -> "c4", "kind" -> "geometric_support", "direction" -> "supported_by")))
    assert((moved \ "after" \ "connections").as[List[JsObject]].contains(
      Json.obj("peer" -> "d5", "kind" -> "phalanx", "direction" -> "peer")))
    assert((unmoved \ "changed_facets").as[List[String]].contains("connections"))
    assertEquals((unmoved \ "before" \ "geometrically_protected_passer").as[Boolean], true)
    assertEquals((unmoved \ "after" \ "geometrically_protected_passer").as[Boolean], false)
    assertEquals((moved \ "before" \ "front_square").as[String], "c5")
    assertEquals((moved \ "after" \ "front_square").as[String], "c6")
    assertEquals((moved \ "after" \ "front_occupant").toOption, Some(JsNull))

  test("mate and stalemate remain distinct certified endpoints without engine evaluation"):
    val fen = "7k/5K2/8/6Q1/8/8/8/8 w - - 0 1"
    val mate = only(statements(fen, "g5g7"), "check_responses")
    assertEquals((mate \ "terminal").as[String], "checkmate")
    assertEquals((mate \ "responses").as[List[JsObject]], Nil)
    val stalledFacts = statements(fen, "g5g6")
    val stalemate = only(stalledFacts, "stalemate")
    assertEquals((stalemate \ "phase").as[String], "after")
    assertEquals((stalemate \ "stalled_side").as[String], "black")
    assertEquals((stalemate \ "king_square").as[String], "h8")
    assert(!stalledFacts.exists(f => (f \ "kind").as[String] == "check_responses"))

  test("structural output keeps coexisting subjects and destination-bound loss of passed status"):
    val tensions = only(statements("4k3/8/8/3p1p2/8/8/4P3/4K3 w - - 0 1", "e2e4"), "structural_change")
    val created = (tensions \ "changes").as[List[JsObject]]
      .find(c => (c \ "kind").as[String] == "pawn_tension_created").get
    assertEquals((created \ "subjects").as[List[JsObject]].map(s =>
      ((s \ "from").as[String], (s \ "to").as[String])).toSet, Set("e4" -> "d5", "e4" -> "f5"))
    assertEquals((created \ "targets").as[List[JsObject]], Nil)
    val loss = only(statements("k7/5p2/4n3/3P4/8/8/8/7K w - - 0 1", "d5e6"), "structural_change")
    val removed = (loss \ "changes").as[List[JsObject]]
      .find(c => (c \ "kind").as[String] == "passed_pawn_status_removed").get
    assertEquals((removed \ "subjects").as[List[JsObject]],
      List(Json.obj("kind" -> "passed_pawn_lost", "side" -> "white", "square" -> "e6")))

  test("castling retains its rook movement and capture underpromotion retains physical roles and pawn removal"):
    val castle = only(statements("4k3/8/8/8/8/8/8/4K2R w K - 0 1", "e1g1"), "castling")
    assertEquals((castle \ "rook").as[JsObject], Json.obj("from" -> "h1", "to" -> "f1"))
    val promoted = statements("1r5k/P7/8/8/8/8/8/7K w - - 0 1", "a7b8n")
    assertEquals((only(promoted, "promotion") \ "promoted_role").as[String], "knight")
    assertEquals((only(promoted, "promotion") \ "role").as[String], "pawn")
    assertEquals((only(promoted, "capture") \ "captured_role").as[String], "rook")
    assert(promoted.exists(f => (f \ "kind").as[String] == "pawn_structure" &&
      (f \ "before" \ "square").asOpt[String].contains("a7") &&
      (f \ "after").toOption.contains(JsNull) &&
      (f \ "changed_facets").as[List[String]].contains("removed")))

  test("moving target control points retain both identities instead of relabeling the move's actor"):
    val facts = statements("k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "c4c5")
    val control = facts.find(f => (f \ "kind").as[String] == "piece_control" &&
      (f \ "before" \ "controller" \ "square").asOpt[String].contains("c4") &&
      (f \ "before" \ "target" \ "square").asOpt[String].contains("d5")).get
    assertEquals((control \ "relation").as[String], "support")
    assertEquals((control \ "before" \ "state").as[String], "present")
    assertEquals((control \ "after" \ "state").as[String], "absent")
    assertEquals((control \ "after" \ "controller" \ "square").as[String], "c5")
    assertEquals((control \ "after" \ "target" \ "square").as[String], "d5")
    assert((control \ "before" \ "premise_ids").as[List[String]].nonEmpty)
    assert((control \ "after" \ "premise_ids").as[List[String]].nonEmpty)
    val targetMoved = statements("k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "d5d6")
      .find(f => (f \ "kind").as[String] == "piece_control" &&
        (f \ "before" \ "controller" \ "square").asOpt[String].contains("c4") &&
        (f \ "before" \ "target" \ "square").asOpt[String].contains("d5")).get
    assertEquals((targetMoved \ "from").as[String], "d5")
    assertEquals((targetMoved \ "after" \ "controller" \ "square").as[String], "c4")
    assertEquals((targetMoved \ "after" \ "target" \ "square").as[String], "d6")
    assertEquals((targetMoved \ "after" \ "state").as[String], "absent")
