package lila.chessjudgment.analysis.structure

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*
import munit.FunSuite

class StructuralDeltaAnalyzerTest extends FunSuite:

  test("an induced pawn advance records the king shelter it leaves"):
    val beforeFen = "6k1/7p/8/6N1/8/8/8/4K3 b - - 0 1"
    val afterFen = "6k1/8/7p/6N1/8/8/8/4K3 w - - 0 2"
    val pressure = consequences(beforeFen, afterFen, Color.White, "h7h6")
      .find(_.kind == TransitionConsequenceKind.KingSafetyPressure)
      .getOrElse(fail("expected ...h6 to leave the king shelter"))

    assert(pressure.subjects.contains("king:g8"), pressure)
    assert(pressure.subjects.contains("file:h"), pressure)

  test("a remote pawn advance is not a king-shelter result"):
    val beforeFen = "6k1/p7/8/6N1/8/8/8/4K3 b - - 0 1"
    val afterFen = "6k1/8/p7/6N1/8/8/8/4K3 w - - 0 2"
    val pressure = consequences(beforeFen, afterFen, Color.White, "a7a6")
      .filter(_.kind == TransitionConsequenceKind.KingSafetyPressure)

    assertEquals(pressure, Nil)

  test("target pressure keeps the attacked piece with its square"):
    val beforeFen = "7k/8/8/8/1q6/8/P7/7K w - - 0 1"
    val afterFen = "7k/8/8/8/1q6/P7/8/7K b - - 1 1"
    val binding = pressureBinding(
      beforeFen,
      afterFen,
      "a2a3",
      TransitionConsequenceKind.TargetPressureGain
    )

    assert(binding.target.exists(obj => obj.kind == EvidenceObjectKind.Square && obj.key == "b4"), binding)
    assert(binding.target.exists(obj => obj.kind == EvidenceObjectKind.Piece && obj.key == "queen"), binding)
    assert(!binding.actor.exists(obj => obj.kind == EvidenceObjectKind.Piece && obj.key == "queen"), binding)

  test("released pressure keeps the relieved piece with its square"):
    val beforeFen = "7k/8/8/8/1q6/P7/8/7K w - - 0 1"
    val afterFen = "7k/8/8/8/Pq6/8/8/7K b - - 1 1"
    val binding = pressureBinding(
      beforeFen,
      afterFen,
      "a3a4",
      TransitionConsequenceKind.TargetPressureRelease
    )

    assert(binding.target.exists(obj => obj.kind == EvidenceObjectKind.Square && obj.key == "b4"), binding)
    assert(binding.target.exists(obj => obj.kind == EvidenceObjectKind.Piece && obj.key == "queen"), binding)

  private def consequences(
      beforeFen: String,
      afterFen: String,
      side: Color,
      moveUci: String
  ): List[TransitionConsequence] =
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("before position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("after position"))
    StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = side,
        files = ('a' to 'h').toList,
        targets = Nil,
        moveUci = Some(moveUci)
      )
      .toList
      .flatMap(StructuralDeltaContracts.consequences)

  private def pressureBinding(
      beforeFen: String,
      afterFen: String,
      moveUci: String,
      kind: TransitionConsequenceKind
  ): EvidenceObjectBinding =
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("before pressure position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("after pressure position"))
    val delta = StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = Color.White,
        files = ('a' to 'h').toList,
        targets = Nil,
        moveUci = Some(moveUci)
      )
      .getOrElse(fail("pressure delta"))
    val from = PositionNodeRef(beforeFen, 0, Some(Color.White))
    val to = PositionNodeRef(afterFen, 1, Some(Color.Black))
    val ref = EvidenceRef(
      id = s"structural-delta:$moveUci",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = from,
      line = None,
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.BoardDerived
    )
    val record = EvidenceRecord(
      ref,
      StructuralDeltaEvidence(
        StructuralTransitionBinding(moveUci, TransitionEdgeRole.Played, from, to, None, Color.White),
        StructuralDeltaContracts.signals(delta),
        StructuralDeltaContracts.consequences(delta)
      )
    )
    EvidenceObjectBinding
      .fromEvidenceRefs(TypedEvidenceGraph(List(record)), List(ref))
      .find(_.mechanism.exists(_.key.equalsIgnoreCase(kind.toString)))
      .getOrElse(fail(s"$kind binding"))

  test("a bishop move records a concrete exchange only when the opponent can legally accept it"):
    val offeredBeforeFen = "r1b2rk1/ppq1nppp/2nb4/1B1p4/7B/1N3N2/PPP2PPP/R2Q1RK1 w - - 8 12"
    val offeredAfterFen = "r1b2rk1/ppq1nppp/2nb4/1B1p4/8/1N4BN/PPP2PPP/R2Q1RK1 b - - 9 12"
    val offered = exchangeConsequences(offeredBeforeFen, offeredAfterFen, Color.White, "h4g3")

    assertEquals(offered.size, 1)
    assertEquals(offered.head.subjects.toSet, Set("bishop:h4-g3", "bishop:d6", "bishop:d6-g3"))

    val routeBeforeFen = "r1bqk2r/pp2nppp/2nb4/1B1p4/8/1N3N2/PPP2PPP/R1BQ1RK1 w kq - 4 10"
    val routeAfterFen = "r1bqk2r/pp2nppp/2nb4/1B1p2B1/8/1N3N2/PPP2PPP/R2Q1RK1 b kq - 5 10"
    val prematureExchange = exchangeConsequences(routeBeforeFen, routeAfterFen, Color.White, "c1g5")

    assertEquals(prematureExchange, Nil)

  test("an equal piece capture records the accepting recapture without calling the capture a win"):
    val beforeFen = "3qk3/8/3b4/8/8/8/7B/4K3 w - - 0 1"
    val afterFen = "3qk3/8/3B4/8/8/8/8/4K3 b - - 0 1"
    val exchanges = exchangeConsequences(beforeFen, afterFen, Color.White, "h2d6")

    assertEquals(exchanges.size, 1)
    assertEquals(exchanges.head.subjects.toSet, Set("bishop:h2-d6", "bishop:d6", "queen:d8-d6"))

  test("capturing another role does not turn a later mutual attack into an exchange plan"):
    val beforeFen = "2b4k/8/P7/8/8/8/4B3/7K b - - 0 1"
    val afterFen = "7k/8/b7/8/8/8/4B3/7K w - - 0 2"
    val exchanges = exchangeConsequences(beforeFen, afterFen, Color.Black, "c8a6")

    assertEquals(exchanges, Nil)

  private def exchangeConsequences(
      beforeFen: String,
      afterFen: String,
      side: Color,
      moveUci: String
  ): List[TransitionConsequence] =
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("before exchange position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("after exchange position"))
    StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = side,
        files = ('a' to 'h').toList,
        targets = Nil,
        moveUci = Some(moveUci)
      )
      .toList
      .flatMap(StructuralDeltaContracts.consequences)
      .filter(_.kind == TransitionConsequenceKind.PieceExchangeAvailable)

  test("a vacating piece records the concrete lines and pawn advances it opens"):
    val beforeFen = "4k3/8/8/8/8/1N6/BP6/4K3 w - - 0 1"
    val afterFen = "4k3/8/8/8/8/8/BP1N4/4K3 b - - 1 1"
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("before position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("after position"))

    val delta = StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = Color.White,
        files = ('a' to 'h').toList,
        targets = Nil,
        moveUci = Some("b3d2")
      )
      .getOrElse(fail("structural delta"))
    val unlocked = StructuralDeltaContracts.consequences(delta)
      .filter(_.kind == TransitionConsequenceKind.LineUnlockGain)

    assert(unlocked.exists(_.subjects.exists(_.startsWith("bishop:a2:line-unlock:by:b3d2:"))), unlocked)
    assert(unlocked.exists(_.subjects.contains("pawn:b2-b4:line-unlock:by:b3d2:mobility+1")), unlocked)
    assert(unlocked.forall(_.subjects.size == 1), unlocked)

    val from = PositionNodeRef(beforeFen, 0, Some(Color.White))
    val to = PositionNodeRef(afterFen, 1, Some(Color.Black))
    val ref = EvidenceRef(
      id = "structural-delta:b3d2",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = from,
      line = None,
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.BoardDerived
    )
    val record = EvidenceRecord(
      ref,
      StructuralDeltaEvidence(
        StructuralTransitionBinding("b3d2", TransitionEdgeRole.Played, from, to, None, Color.White),
        StructuralDeltaContracts.signals(delta),
        StructuralDeltaContracts.consequences(delta)
      )
    )
    val bindings = EvidenceObjectBinding
      .fromEvidenceRefs(TypedEvidenceGraph(List(record)), List(ref))
      .filter(_.mechanism.exists(_.key.equalsIgnoreCase("LineUnlockGain")))

    assert(bindings.nonEmpty, bindings)
    assert(bindings.forall(_.actor.exists(obj => obj.kind == EvidenceObjectKind.Piece && obj.key == "knight")), bindings)
    assert(bindings.forall(!_.actor.exists(obj => obj.kind == EvidenceObjectKind.Piece && obj.key != "knight")), bindings)
    assert(bindings.forall(_.target.exists(_.kind == EvidenceObjectKind.Piece)), bindings)

  test("a battery keeps its pieces as witnesses and its reachable enemy as the target"):
    val beforeFen = "rn3r2/p3b2p/2p3k1/1p3bp1/7Q/q1P1B1P1/P4PB1/3RR1K1 w - - 2 26"
    val afterFen = "rn3r2/p3b2p/2p3k1/1p3bp1/8/q1P1B1P1/P4PB1/3RR1KQ b - - 3 26"
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("before battery position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("after battery position"))
    val delta = StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = Color.White,
        files = ('a' to 'h').toList,
        targets = Nil,
        moveUci = Some("h4h1")
      )
      .getOrElse(fail("battery delta"))
    val battery = StructuralDeltaContracts
      .consequences(delta)
      .find(_.kind == TransitionConsequenceKind.BatteryPressureGain)
      .getOrElse(fail("battery consequence"))

    assertEquals(battery.targetSubjects, List("pawn:c6"))

    val from = PositionNodeRef(beforeFen, 50, Some(Color.White))
    val to = PositionNodeRef(afterFen, 51, Some(Color.Black))
    val batteryMotif = MoveAnalyzer
      .detectStateMotifs(after, 0)
      .collectFirst { case battery: Motif.Battery if battery.targetSq.exists(_.key == "c6") => battery }
      .getOrElse(fail("battery motif"))
    val motifProof = MoveMotifEvent
      .fromMotif("h4h1", batteryMotif, to, None, EvidenceScope.PlayedTransition)
      .proof

    assertEquals(motifProof.targetSquares.map(_.key), List("c6"))
    assertEquals(motifProof.roles.map(_.name.toLowerCase).toSet, Set("bishop", "queen"))

    val ref = EvidenceRef(
      id = "structural-delta:h4h1",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = from,
      line = None,
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.BoardDerived
    )
    val record = EvidenceRecord(
      ref,
      StructuralDeltaEvidence(
        StructuralTransitionBinding("h4h1", TransitionEdgeRole.Played, from, to, None, Color.White),
        StructuralDeltaContracts.signals(delta),
        StructuralDeltaContracts.consequences(delta)
      )
    )
    val binding = EvidenceObjectBinding
      .fromEvidenceRefs(TypedEvidenceGraph(List(record)), List(ref))
      .find(_.mechanism.exists(_.key.equalsIgnoreCase("BatteryPressureGain")))
      .getOrElse(fail("battery binding"))

    assert(binding.target.exists(obj => obj.kind == EvidenceObjectKind.Square && obj.key == "c6"), binding)
    assert(binding.target.exists(obj => obj.kind == EvidenceObjectKind.Piece && obj.key == "pawn"), binding)
    assert(!binding.target.exists(obj => Set("g2", "h1")(obj.key)), binding)
    assert(binding.witness.exists(obj => obj.kind == EvidenceObjectKind.Square && obj.key == "g2"), binding)
    assert(binding.witness.exists(obj => obj.kind == EvidenceObjectKind.Square && obj.key == "h1"), binding)

  test("aligned pieces are not a battery through another friendly blocker"):
    val fen = "k7/8/2p5/8/8/5B2/6N1/K6Q w - - 0 1"
    val position = Fen.read(Standard, Fen.Full(fen)).getOrElse(fail("blocked battery position"))
    val batteries = MoveAnalyzer.detectStateMotifs(position, 0).collect { case battery: Motif.Battery => battery }

    assertEquals(batteries, Nil)
