package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** Supplied current receipts separate a complete finite choice from a later
  * unclosed price. No engine values, root cause or automatic utility are claimed.
  */
class AnchoredChoiceOptionalPriceTest extends munit.FunSuite:
  import AnchoredChoiceTestFixture.{AnchoredFixture, root, keepRoot, keeping, rootPacket}
  import CommentaryFact.*
  import CommentaryProposal.*
  import CommentaryAnswer.*
  import ObservedLineProtocol.PreparedPath
  import RootQuestionSummary.MoveRef

  private val at = AnchoredChoiceFamilyFixture.blockade ++ List(keeping, "e5e6")
  private val unsafe = at ++ List(root, "e6d6", "f3f4", "d6e6", "g4g5", "f6g5")

  private def fixture(): AnchoredFixture =
    val lines = (AnchoredChoiceFamilyFixture.lines :+ unsafe).distinct
    val (packet, paths, receipt) = rootPacket(lines = lines)
    val batch = CommentaryDiscovery.discover(packet, paths,
      List(CommentaryDiscovery.Anchor(Nil, keepRoot)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "optional-price-test"))),
      Nil, pvGuided = true)
    val generated = batch.additions.find(entry =>
      entry.request.origin.question.producer == "C-reserve-use" &&
        entry.request.requirements.exists {
          case value: CompareMoves => value.id == "reserve_root" &&
            value.referenceMove == root && value.alternativeMove == keepRoot
          case _ => false
        }).get
    val answer = CommentaryInvestigation.pairedAnswer(packet, root, keepRoot,
      receipt.work, receipt.lines, Some(paths)).get
    val answered = generated.copy(readings = generated.readings.map(row =>
      if row.requirementId == "reserve_root" then pairReading(
        generated.request.requirements.collectFirst {
          case value: CompareMoves if value.id == "reserve_root" => value
        }.get, answer) else row))
    AnchoredFixture(packet, paths, CommentaryFactReader.read(packet, paths, List(answered)).head, receipt)

  private def roots(f: AnchoredFixture, family: AnchoredChoiceReading.Family): List[PreparedPath] =
    List(family.owner.answer.referenceLineId, family.owner.answer.alternativeLineId)
      .map(f.paths.path(f.packet, _).get)

  private def previous(f: AnchoredFixture, pair: List[PreparedPath]): Answer =
    val seed = Continuation(pair.map(MoveRef(_, 0)))
    RootQuestionSummary.completePairAnswer(f.packet, f.paths, pair, "selected_comparison",
      List(Section(List(seed)))).get.copy(completionSources = Some(List(seed)))

  test("an unselected later loss ending in check cannot withdraw a closed leading plan or enter detail"):
    val f = fixture()
    val after = at :+ root
    val safeSearches = AnchoredChoiceFamilyFixture.completeSearches(f, longest = true)
      .filterNot(_.anchor.prefixMoves == after)
    val unsafeResponse = AnchoredChoiceFamilyFixture.supplied(f, "optional-unclosed-price", after,
      List("e6d6" -> 0, "b4c3" -> 10, "b4a3" -> 20), longest = true)
    val family = AnchoredChoiceFamilyFixture.read(f, safeSearches :+ unsafeResponse).get
    assert(family.complete, "the finite own values, both response pools and capture comparisons are supplied")
    val optional = family.followupStages.find(_.stage.prefix == at).get
    assert(optional.complete && optional.use.get.refuted)
    val source = optional.branches.find(_.ownMove == root).get.resistance.value.get
    val path = source.reference
    assertEquals(path.moves, unsafe)
    assert(source.belongsTo(f.packet, f.paths))
    val pawn = family.pawn.witness
    val events = f.paths.continuationReadingFor(path).get.events
    assert(events.exists(event => event.plyOffset == 9 && event.originalVictim.contains(pawn)))
    assertEquals(f.paths.trackedPieceAfter(path, pawn, unsafe.size).flatten, None)
    assert(f.paths.positionView(path, unsafe.size).get.currentCheckSources(chess.White).nonEmpty)
    assertEquals(path.moves.size, 10, "there is no admitted White reply to the resource's loss with check")

    val pair = roots(f, family)
    val before = previous(f, pair)
    val positions = f.packet.resources.map(view =>
      view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    def protect(answer: Answer, original: List[Section]) =
      MoveWorkProjection.protectRootUses(f.packet, f.paths, pair, positions, notes, answer, original)
    val delivered = AnchoredChoiceAnswer.withFamily(f.packet, f.paths, pair,
      List(family), before, protect)
    assertEquals(delivered.question, "functional_response",
      "an optional unclosed price cannot invalidate independently closed current core accounts")
    val core = delivered.core.flatMap(_.statements).collect { case AnchoredDecision(value) => value }
    val detail = delivered.details.flatMap(_.statements).collect { case AnchoredDecision(value) => value }
    val initial = family.initialStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.blockade).get
    val safeLater = family.followupStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.quietPrefix).get
    assertEquals(core.map(_.task.stage.id).toSet, Set(initial.stage.id, safeLater.stage.id))
    assert((core ++ detail).forall(_.task.stage.id != optional.stage.id),
      "the entire unsafe account must remain outside detail rather than displaying its fact without its price")
    assert(!(delivered.core ++ delivered.details).flatMap(_.references).exists(_.path eq path))
    assertEquals(delivered.comparison, before.comparison)
    assert(before.completionSources.get.forall(delivered.completionSources.get.contains))
    assertEquals(family.stages.size, 8, "omitting the unsafe account from display must not erase its completed task")
    assert(RootQuestionSummary.realize(f.paths, pair, delivered).nonEmpty)
    val selected = AnchoredChoiceAnswer.withAnswer(f.packet, f.paths, pair, List(optional), before, protect)
    assert(selected eq before,
      "the same unresolved original-pawn loss is mandatory when that account is selected as core")
