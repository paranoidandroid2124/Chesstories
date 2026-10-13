package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.LegalMoveResource
import lila.chessjudgment.model.judgment.*

/** A current owned decision to spend a reserve advance or choose another plan.
  * The exact choice and its immediate resistance are valued separately from
  * any later king cycle or entry. This reader never supplies prose or scores.
  */
private[runtime] object AnchoredChoiceReading:
  import CommentaryFact.*
  import CommentaryProposal.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import TargetResponseInvestigation.Search

  val ValuePrefix = "anchored_value_"
  private val SourceVersion = "2"

  case class Obligation(comparison: CompareMoves, facts: List[Fact], context: FactGroup,
      phase: Int, sourceIds: List[String], owner: CommentaryIdea.AnsweredComparison):
    def requirements: List[Requirement] = facts ++ List(context, comparison)

  final class Stage private[AnchoredChoiceReading] (
      private val owner: EvidenceBackedJudgmentPacket, private val prepared: PreparedPaths,
      val rootOwner: CommentaryIdea.AnsweredComparison,
      val proof: Option[CommentaryIdea.ReserveTiming], val anchor: PreparedPath,
      val prefix: List[String], val referenceMove: String,
      val pawn: Piece, val currentPawn: RelationColoredPieceWitness,
      val stateFacts: List[Fact], val stateContext: FactGroup,
      val source: Option[CommentaryChoiceValue.Checked] = None,
      val parentStageId: Option[String] = None, val level: Int = 0):
    val id: String = stateContext.id
    // A declared resource need not be the move selected by this source PV.
    // Its value and subsequent resistance still require their own receipts.
    def witnessedAdvance: Boolean = anchor.moves.lift(prefix.size).contains(referenceMove)
    def evidence: List[String] = (List(rootOwner.proposalId, rootOwner.answer.workId,
      rootOwner.answer.executionKeySha256) ++ source.toList.flatMap(value =>
      List(value.question.id, value.survey.work.workId, value.survey.work.executionKeySha256,
        value.answer.workId, value.answer.executionKeySha256)) ++
      parentStageId.toList ++ anchor.ids.take(prefix.size)).distinct
    def belongsTo(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): Boolean =
      (owner eq packet) && (prepared eq paths)
    private[AnchoredChoiceReading] def owns(value: CommentaryChoiceValue.Checked): Boolean =
      value.belongsTo(owner, prepared)
    private[AnchoredChoiceReading] def current(path: PreparedPath, count: Int): Option[RelationColoredPieceWitness] =
      prepared.trackedPieceAfter(path, pawn.witness, count).flatten

  case class CaptureTest(move: String, question: CompareMoves,
      value: Option[CommentaryChoiceValue.Checked])
  case class ReplyTest(move: String, question: CompareMoves,
      value: Option[CommentaryChoiceValue.Checked])
  case class Resistance(pool: Option[Search], leaderMove: Option[String],
      question: Option[CompareMoves], value: Option[CommentaryChoiceValue.Checked],
      captures: List[CaptureTest], requiredCaptureMoves: Set[String],
      replies: List[ReplyTest] = Nil, requiredReplyMoves: Set[String] = Set.empty):
    def complete: Boolean = pool.nonEmpty && leaderMove.nonEmpty && value.nonEmpty &&
      captures.map(_.move).toSet == requiredCaptureMoves && captures.forall(_.value.nonEmpty) &&
      replies.map(_.move).toSet == requiredReplyMoves && replies.forall(_.value.nonEmpty)

  case class Branch(ownMove: String, path: PreparedPath, afterChoicePrefix: List[String],
      pawnAfter: RelationColoredPieceWitness, spendsAdvance: Boolean, keepsPawnHere: Boolean,
      resistance: Resistance)
  case class Task(stage: Stage, ownQuestion: CompareMoves,
      use: Option[CommentaryChoiceValue.Checked], branches: List[Branch], obligations: List[Obligation]):
    val id: String = CommentarySelection.stableId("anchored-choice-task", List(stage.id, ownQuestion.id))
    def complete: Boolean = use.exists(value => stage.owns(value) &&
      value.question == ownQuestion && (value.useful || value.refuted) &&
      branches.map(_.ownMove).toSet == Set(ownQuestion.referenceMove, ownQuestion.alternativeMove) &&
      branches.size == 2 && branches.forall(branch =>
        (branch.path eq (if branch.ownMove == ownQuestion.referenceMove then value.reference else value.alternative)) &&
        branch.afterChoicePrefix == stage.prefix :+ branch.ownMove &&
        stage.current(branch.path, branch.afterChoicePrefix.size).contains(branch.pawnAfter) &&
        branch.spendsAdvance == (branch.ownMove == stage.referenceMove) &&
        branch.keepsPawnHere == (branch.pawnAfter == stage.currentPawn) &&
        branch.resistance.complete && branch.resistance.value.exists(response =>
          stage.owns(response) && response.question.prefixMovesUci == branch.afterChoicePrefix &&
            branch.resistance.pool.contains(response.survey) &&
            branch.resistance.leaderMove.contains(response.question.referenceMove) &&
            response.survey.lines.headOption.exists(line =>
              branch.resistance.leaderMove.contains(line.evaluation.moves.head)) &&
            samePrefix(branch.path, response.reference, branch.afterChoicePrefix.size)) &&
        branch.resistance.captures.forall(capture => capture.value.exists(response =>
          stage.owns(response) && response.question == capture.question &&
            response.question.prefixMovesUci == branch.afterChoicePrefix &&
            response.question.referenceMove == capture.move &&
            branch.resistance.leaderMove.contains(response.question.alternativeMove) &&
            branch.resistance.pool.contains(response.survey) &&
            samePrefix(branch.path, response.reference, branch.afterChoicePrefix.size))) &&
        branch.resistance.replies.forall(reply => reply.value.exists(response =>
          stage.owns(response) && response.question == reply.question &&
            response.question.prefixMovesUci == branch.afterChoicePrefix &&
            response.question.referenceMove == reply.move &&
            branch.resistance.pool.contains(response.survey) &&
            samePrefix(branch.path, response.reference, branch.afterChoicePrefix.size)))))

  case class RootResistance(rootMove: String, path: PreparedPath,
      pawnAfter: RelationColoredPieceWitness, resistance: Resistance):
    def replies: List[ReplyTest] = resistance.replies
    def requiredReplies: Set[String] = resistance.requiredReplyMoves

  case class FollowupBoundary(prefix: List[String], reason: String,
      source: Option[CommentaryChoiceValue.Checked] = None, parentStageId: Option[String] = None)

  /** One original pawn/root question may have different current realizations.
    * Completion closes only these finite questions. It is not a conversion
    * theorem or an attribution of the whole root evaluation difference.
    */
  final class Family private[AnchoredChoiceReading] (
      private val packet: EvidenceBackedJudgmentPacket, private val prepared: PreparedPaths,
      val owner: CommentaryIdea.AnsweredComparison, val pawn: Piece,
      val roots: List[RootResistance], val stages: List[Task],
      val expectedStagePrefixes: Set[List[String]], val initialExpectedPrefixes: Set[List[String]],
      val unavailableStagePrefixes: Set[List[String]],
      val followupBoundary: List[FollowupBoundary], val obligations: List[Obligation]):
    val id: String = CommentarySelection.stableId("anchored-choice-family",
      List(owner.answer.workId, owner.answer.executionKeySha256,
        owner.comparison.referenceMove, owner.comparison.alternativeMove, pawn.json.toString))
    def belongsTo(current: EvidenceBackedJudgmentPacket, paths: PreparedPaths): Boolean =
      (packet eq current) && (prepared eq paths)
    def initialStages: List[Task] = stages.filter(_.stage.level == 0)
    def followupStages: List[Task] = stages.filter(_.stage.level == 1)
    private def responseOwned(root: RootResistance): Boolean =
      val count = 1
      def valid(value: CommentaryChoiceValue.Checked, question: CompareMoves, move: String): Boolean =
        value.belongsTo(packet, prepared) && value.question == question &&
          value.question.prefixMovesUci == List(root.rootMove) && value.question.referenceMove == move &&
          root.resistance.pool.contains(value.survey) && samePrefix(root.path, value.reference, count)
      root.resistance.complete && root.resistance.value.exists(value =>
        root.resistance.question.exists(valid(value, _, root.resistance.leaderMove.get))) &&
        root.replies.forall(reply => reply.value.exists(valid(_, reply.question, reply.move))) &&
        root.resistance.captures.forall(capture => capture.value.exists(valid(_, capture.question, capture.move)))
    def initialComplete: Boolean = roots.size == 2 &&
      roots.map(_.rootMove).toSet == Set(owner.comparison.referenceMove, owner.comparison.alternativeMove) &&
      roots.forall(responseOwned) && initialExpectedPrefixes.nonEmpty &&
      unavailableStagePrefixes.intersect(initialExpectedPrefixes).isEmpty &&
      initialStages.map(_.stage.prefix).toSet == initialExpectedPrefixes && initialStages.forall(_.complete)
    def complete: Boolean = initialComplete && unavailableStagePrefixes.isEmpty &&
      stages.map(_.stage.prefix).toSet == expectedStagePrefixes && followupStages.forall(_.complete)
    def followupComplete: Boolean = complete && followupStages.nonEmpty && followupBoundary.isEmpty

  private def owned(paths: PreparedPaths, path: PreparedPath): Boolean =
    paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves

  private def samePrefix(left: PreparedPath, right: PreparedPath, count: Int): Boolean =
    left.moves.size >= count && right.moves.size >= count &&
      left.moves.take(count) == right.moves.take(count) &&
      left.ids.take(count) == right.ids.take(count) &&
      left.positions.take(count + 1) == right.positions.take(count + 1)

  private def piece(value: RelationColoredPieceWitness): Piece =
    Piece(if value.side.white then "white" else "black", value.role.name, value.square.key)

  private def rows(paths: PreparedPaths, path: PreparedPath, count: Int): List[LegalMoveResource] =
    paths.positionView(path, count).flatMap(_.legalRows).toList.flatten

  private def original(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      path: PreparedPath, count: Int, current: RelationColoredPieceWitness): Option[Piece] =
    paths.identitiesAfter(packet, path, count).collectFirst {
      case (actor, Some(Some(now))) if now == current => piece(actor)
    }

  private def stateFacts(ownerId: String, prefix: List[String], pawn: Piece,
      current: RelationColoredPieceWitness, row: LegalMoveResource, actor: Piece)
      : (List[Fact], FactGroup) =
    val key = List(SourceVersion, ownerId, prefix.mkString(" "), pawn.json.toString,
      current.square.key, row.move)
    def id(role: String) = CommentarySelection.stableId(ValuePrefix + role, key)
    val path = Path("anchored-choice-position", Declared(prefix))
    val facts = List(
      Fact(id("present"), path, Predicate("presence", Some(pawn)), Ply(prefix.size), expected = Some("present")),
      Fact(id("location"), path, Predicate("location", Some(pawn)), Ply(prefix.size), expected = Some(current.square.key)),
      Fact(id("legal"), path, Predicate("legal_move", Some(actor), square = Some(row.to.key)),
        Ply(prefix.size), expected = Some("legal")))
    facts -> FactGroup(id("context"), "all", facts.map(_.id))

  private def receipt(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      value: CommentaryIdea.AnsweredComparison, profile: String, minimumDepth: Int): Boolean =
    val answer = value.answer
    answer.engineProfile == profile && answer.referenceDepth == answer.alternativeDepth &&
      answer.referenceDepth.exists(_ >= minimumDepth) &&
      List((answer.referenceLineId, value.comparison.referenceMove, answer.referenceOccurrenceId),
        (answer.alternativeLineId, value.comparison.alternativeMove, answer.alternativeOccurrenceId))
        .forall { (id, move, occurrence) =>
          paths.path(packet, id).exists(path => owned(paths, path) &&
            path.moves.startsWith(value.comparison.prefixMovesUci :+ move) &&
            path.ids.lift(value.comparison.prefixMovesUci.size).contains(occurrence))
        }

  /** Start from the current exact root/context, not a completed king cycle or
    * a future entry. Distinct decisions cannot be selected by UCI or detail size.
    */
  private def legacyStages(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], roots: Set[String], profile: String, minimumDepth: Int): List[Stage] =
    CommentaryIdea.reserveTimings(entries).flatMap { proof =>
      val root = proof.owner.comparison
      val prefix = proof.test.prefixMovesUci
      for
        _ <- Option.when(roots == Set(root.referenceMove, root.alternativeMove) &&
          root.prefixMovesUci.isEmpty && receipt(packet, paths, proof.owner, profile, minimumDepth) && prefix.nonEmpty)(())
        actual <- paths.path(packet, proof.owner.answer.referenceLineId)
        alternative <- paths.path(packet, proof.owner.answer.alternativeLineId)
        rootRow <- rows(paths, actual, 0).find(row => row.move == root.referenceMove &&
          row.actor.role.name == "pawn" && row.capture.isEmpty && row.promotionRole.isEmpty)
        pawn = piece(rootRow.actor)
        anchor <- CommentaryProposal.observedPrefix(packet, paths, prefix) if owned(paths, anchor)
        if anchor.moves.startsWith(prefix) && samePrefix(anchor, alternative, 1)
        source <- (proof.source match
          case None => Some(alternative)
          case Some(value) if value.proposalId == proof.owner.proposalId &&
              receipt(packet, paths, value, profile, minimumDepth) &&
              value.comparison.prefixMovesUci.nonEmpty =>
            paths.path(packet, if proof.sourceArm == "reference" then value.answer.referenceLineId else value.answer.alternativeLineId)
              .filter(path => samePrefix(path, alternative, value.comparison.prefixMovesUci.size) &&
                CommentaryIdea.comparableReserveArm(value.answer, proof.sourceArm))
          case _ => None)
        if owned(paths, source) && samePrefix(anchor, source, prefix.size)
        current <- paths.trackedPieceAfter(anchor, pawn.witness, prefix.size).flatten
        resource <- rows(paths, anchor, prefix.size).find(row =>
          row.move == proof.test.referenceMove && row.actor == current &&
            row.capture.isEmpty && row.promotionRole.isEmpty)
        facts = stateFacts(proof.owner.proposalId, prefix, pawn, current, resource, pawn)
      yield new Stage(packet, paths, proof.owner, Some(proof), anchor, prefix,
        proof.test.referenceMove, pawn, current, facts._1, facts._2)
    }

  private def stage(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], roots: Set[String], profile: String, minimumDepth: Int): Option[Stage] =
    val candidates = legacyStages(packet, paths, entries, roots, profile, minimumDepth)
    val decisions = candidates.groupBy(value => (value.prefix, value.referenceMove, value.pawn))
    if decisions.size != 1 then None
    else candidates.sortBy(_.rootOwner.proposalId).headOption

  private final class Choices(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      searches: List[Search], profile: String, minimumDepth: Int):
    val obligations = scala.collection.mutable.ListBuffer.empty[Obligation]
    def pool(prefix: List[String]): Option[Search] =
      CommentaryChoiceValue.survey(packet, prefix, searches, profile, minimumDepth)
    def read(question: CompareMoves): Option[CommentaryChoiceValue.Checked] =
      CommentaryChoiceValue.read(packet, paths, question, searches, profile, minimumDepth)
    def choice(owner: CommentaryIdea.AnsweredComparison, pawn: Piece, key: String,
        role: String, prefix: List[String], reference: String, path: PreparedPath,
        currentPawn: RelationColoredPieceWitness, phase: Int,
        seedOther: Option[String] = None): Option[CompareMoves] =
      if prefix.size >= MaximumPly || !owned(paths, path) || !path.moves.startsWith(prefix) then return None
      val full = pool(prefix)
      val alternative = full.flatMap(CommentaryChoiceValue.strongestOther(packet, paths, _, reference))
        .orElse(Option.when(full.isEmpty)(seedOther.filter(move => move != reference &&
          rows(paths, path, prefix.size).exists(_.move == move))
            .orElse(rows(paths, path, prefix.size).map(_.move).sorted.find(_ != reference))).flatten)
      for
        other <- alternative
        row <- rows(paths, path, prefix.size).find(_.move == reference)
        actor <- original(packet, paths, path, prefix.size, row.actor)
      yield
        val question = CompareMoves(CommentarySelection.stableId(ValuePrefix + role,
          List(SourceVersion, key, prefix.mkString(" "), reference, other, profile,
            CommentaryChoiceValue.pairDepth(packet, prefix, searches, profile, minimumDepth).toString)),
          prefix, reference, other)
        val facts = stateFacts(owner.proposalId, prefix, pawn, currentPawn, row, actor)
        obligations += Obligation(question, facts._1, facts._2, phase,
          full.toList.flatMap(value => List(value.work.workId, value.work.executionKeySha256)), owner)
        question
    def resistance(owner: CommentaryIdea.AnsweredComparison, pawn: Piece, key: String,
        prefix: List[String], path: PreparedPath, currentPawn: RelationColoredPieceWitness,
        phase: Int): Resistance =
      val full = pool(prefix)
      val legal = rows(paths, path, prefix.size)
      val leader = full.flatMap(_.lines.headOption.map(_.evaluation.moves.head))
      val seed = leader.orElse(path.moves.lift(prefix.size).filter(move => legal.exists(_.move == move)))
        .orElse(legal.map(_.move).sorted.headOption)
      val question = seed.flatMap(reply =>
        choice(owner, pawn, key, "response", prefix, reply, path, currentPawn, phase))
      val value = question.flatMap(read)
      val adequate = full.toList.flatMap(CommentaryChoiceValue.adequateReplies(packet, _)).distinct
      val replies = adequate.flatMap { reply =>
        val q = if leader.contains(reply) then question else
          choice(owner, pawn, key, "reply", prefix, reply, path, currentPawn, phase)
        q.map(value => ReplyTest(reply, value, read(value)))
      }
      // A legal capture already witnessed at this exact current prefix is a
      // separate resistance duty even when the finite pool did not return it.
      val captureMoves = paths.paths.filter(other => owned(paths, other) && samePrefix(other, path, prefix.size))
        .flatMap(other => other.moves.lift(prefix.size)).distinct.sorted.filter(reply =>
          !leader.contains(reply) && legal.exists(row => row.move == reply && row.capture.nonEmpty))
      val captures = captureMoves.flatMap { reply =>
        choice(owner, pawn, key, "capture", prefix, reply, path, currentPawn, phase)
          .map(q => CaptureTest(reply, q, read(q)))
      }
      Resistance(full, leader, question, value, captures, captureMoves.toSet, replies, adequate.toSet)

  private def localTask(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      current: Stage, choices: Choices): Option[Task] =
    val phase = if current.level == 0 then 0 else 2
    val before = choices.obligations.size
    choices.choice(current.rootOwner, current.pawn, current.id, "own", current.prefix,
      current.referenceMove, current.anchor, current.currentPawn, phase,
      current.proof.map(_.test.alternativeMove)).map { question =>
      val use = choices.read(question)
      val branches = use.toList.flatMap { value =>
        List(value.question.referenceMove -> value.reference, value.question.alternativeMove -> value.alternative)
          .flatMap { (move, path) =>
            val after = current.prefix :+ move
            paths.trackedPieceAfter(path, current.pawn.witness, after.size).flatten.toList.map { now =>
              Branch(move, path, after, now, move == current.referenceMove, now == current.currentPawn,
                choices.resistance(current.rootOwner, current.pawn, current.id, after, path, now, phase + 1))
            }
          }
      }
      Task(current, question, use, branches, choices.obligations.drop(before).toList)
    }

  def task(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], searches: List[Search], roots: Set[String], profile: String,
      minimumDepth: Int): Option[Task] =
    if !paths.belongsTo(packet) then return None
    val choices = new Choices(packet, paths, searches, profile, minimumDepth)
    stage(packet, paths, entries, roots, profile, minimumDepth).flatMap(localTask(packet, paths, _, choices))

  private case class RootSeed(owner: CommentaryIdea.AnsweredComparison, pawn: Piece,
      before: RelationColoredPieceWitness, spent: PreparedPath, retained: PreparedPath)

  private def rootSeed(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], roots: Set[String], profile: String, minimumDepth: Int): Option[RootSeed] =
    val seeds = CommentaryIdea.reserveChoices(entries).flatMap { proof =>
      val owner = proof.owner
      val root = owner.comparison
      for
        _ <- Option.when(root.prefixMovesUci.isEmpty && roots == Set(root.referenceMove, root.alternativeMove) &&
          receipt(packet, paths, owner, profile, minimumDepth)) (())
        spent <- paths.path(packet, owner.answer.referenceLineId)
        retained <- paths.path(packet, owner.answer.alternativeLineId)
        if samePrefix(spent, retained, 0)
        advance <- rows(paths, spent, 0).find(row => row.move == root.referenceMove &&
          row.actor.role.name == "pawn" && row.capture.isEmpty && row.promotionRole.isEmpty)
        other <- rows(paths, retained, 0).find(row => row.move == root.alternativeMove &&
          row.actor != advance.actor && row.capture.isEmpty && row.promotionRole.isEmpty)
        pawn = piece(advance.actor)
        actual <- paths.trackedPieceAfter(spent, pawn.witness, 1).flatten
        kept <- paths.trackedPieceAfter(retained, pawn.witness, 1).flatten
        if actual.side == advance.actor.side && actual.role == advance.actor.role &&
          actual.square == advance.to && kept == advance.actor
      yield RootSeed(owner, pawn, advance.actor, spent, retained)
    }
    val families = seeds.groupBy(seed =>
      (seed.owner.comparison.referenceMove, seed.owner.comparison.alternativeMove, seed.pawn))
    // A shared functional question does not make distinct numerical receipts
    // interchangeable. The primary consumer must still match this exact one.
    if families.size != 1 || seeds.map(_.owner.answer).distinct.size != 1 then None
    else seeds.sortBy(_.owner.proposalId).headOption

  private def derivedStage(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      seed: RootSeed, anchor: PreparedPath, prefix: List[String],
      source: CommentaryChoiceValue.Checked, parent: Option[Stage]): Option[Stage] =
    for
      _ <- Option.when(source.belongsTo(packet, paths) && owned(paths, anchor) &&
        (source.reference eq anchor) && prefix.startsWith(source.question.prefixMovesUci :+ source.question.referenceMove) &&
        anchor.moves.startsWith(prefix) && samePrefix(anchor, seed.retained, 1)) (())
      current <- paths.trackedPieceAfter(anchor, seed.pawn.witness, prefix.size).flatten
      if current == seed.before
      row <- rows(paths, anchor, prefix.size).find(row => row.move == seed.owner.comparison.referenceMove &&
        row.actor == current && row.capture.isEmpty && row.promotionRole.isEmpty)
      facts = stateFacts(seed.owner.proposalId, prefix, seed.pawn, current, row, seed.pawn)
    yield new Stage(packet, paths, seed.owner, None, anchor, prefix, row.move,
      seed.pawn, current, facts._1, facts._2, Some(source), parent.map(_.id),
      if parent.isEmpty then 0 else 1)

  /** The first later action by this original pawn is a witnessed resource use,
    * not a reason to require the intervening King route. Only this finite
    * owned continuation is inspected; stages created here do not recurse.
    */
  private def firstAdvance(paths: PreparedPaths,
      seed: RootSeed, source: CommentaryChoiceValue.Checked): Option[(Int, String)] =
    val path = source.reference
    val from = source.question.prefixMovesUci.size + 1
    path.moves.indices.iterator.drop(from).takeWhile(_ < MaximumPly).flatMap { count =>
      paths.trackedPieceAfter(path, seed.pawn.witness, count).flatten.flatMap { actor =>
        rows(paths, path, count).find(row => row.move == path.moves(count) && row.actor == actor)
          .map(row => count -> row.move)
      }
    }.nextOption()

  def family(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], searches: List[Search], roots: Set[String], profile: String,
      minimumDepth: Int): Option[Family] =
    if !paths.belongsTo(packet) then return None
    rootSeed(packet, paths, entries, roots, profile, minimumDepth).map { seed =>
      val choices = new Choices(packet, paths, searches, profile, minimumDepth)
      val key = CommentarySelection.stableId("anchored-root-choice", List(seed.owner.proposalId,
        seed.owner.comparison.referenceMove, seed.owner.comparison.alternativeMove, seed.pawn.json.toString))
      val rootReadings = List(seed.owner.comparison.referenceMove -> seed.spent,
        seed.owner.comparison.alternativeMove -> seed.retained).flatMap { (move, path) =>
        paths.trackedPieceAfter(path, seed.pawn.witness, 1).flatten.map { current =>
          RootResistance(move, path, current,
            choices.resistance(seed.owner, seed.pawn, key, List(move), path, current, -2))
        }
      }
      val initialExpected = scala.collection.mutable.Set.empty[List[String]]
      val expected = scala.collection.mutable.Set.empty[List[String]]
      val unavailable = scala.collection.mutable.Set.empty[List[String]]
      val boundaries = scala.collection.mutable.ListBuffer.empty[FollowupBoundary]
      val stages = scala.collection.mutable.ListBuffer.empty[Task]
      rootReadings.find(_.rootMove == seed.owner.comparison.alternativeMove).foreach { retained =>
        retained.requiredReplies.toList.sorted.foreach { reply =>
          val prefix = List(retained.rootMove, reply)
          initialExpected += prefix
          expected += prefix
          val source = retained.replies.find(_.move == reply).flatMap(_.value)
          source.foreach { value =>
            derivedStage(packet, paths, seed, value.reference, prefix, value, None) match
              case Some(stage) => localTask(packet, paths, stage, choices).foreach(stages += _)
              case None => unavailable += prefix
          }
        }
      }
      // Each returned adequate resistance owns its own continuation. Different
      // replies may use the pawn at different times or not show a use at all.
      // Preserve those boundaries instead of requiring a common PV event.
      stages.toList.filter(_.stage.level == 0).foreach { initial =>
        initial.branches.filter(_.keepsPawnHere).foreach { branch =>
          if branch.resistance.pool.isEmpty then boundaries +=
            FollowupBoundary(branch.afterChoicePrefix, "reply_pool_pending", None, Some(initial.stage.id))
          branch.resistance.requiredReplyMoves.toList.sorted.foreach { reply =>
            val prefix = branch.afterChoicePrefix :+ reply
            val source = branch.resistance.replies.find(_.move == reply).flatMap(_.value)
            source match
              case None => boundaries += FollowupBoundary(prefix, "reply_value_pending", None, Some(initial.stage.id))
              case Some(value) if !branch.resistance.complete =>
                boundaries += FollowupBoundary(prefix, "resistance_value_pending", Some(value), Some(initial.stage.id))
              case Some(value) if paths.trackedPieceAfter(value.reference, seed.pawn.witness, prefix.size).contains(None) =>
                boundaries += FollowupBoundary(prefix, "resource_removed_by_reply", Some(value), Some(initial.stage.id))
              case Some(value) =>
                firstAdvance(paths, seed, value) match
                  case None =>
                    // Absence from this PV is not absence of a useful option.
                    // Examine the unchanged original resource at the current
                    // defended position, with the same full survey/pair and
                    // both resistance duties as a witnessed later advance.
                    // This is one finite level; the new task does not recurse.
                    derivedStage(packet, paths, seed, value.reference, prefix, value, Some(initial.stage)) match
                      case Some(stage) =>
                        expected += prefix
                        localTask(packet, paths, stage, choices).foreach(stages += _)
                      case None =>
                        boundaries += FollowupBoundary(prefix, "no_owned_later_advance", Some(value), Some(initial.stage.id))
                  case Some((_, move)) if move != seed.owner.comparison.referenceMove =>
                    boundaries += FollowupBoundary(prefix, "original_pawn_used_differently", Some(value), Some(initial.stage.id))
                  case Some((count, _)) if count + 2 > MaximumPly =>
                    boundaries += FollowupBoundary(value.reference.moves.take(count),
                      "witness_outside_comparison_bound", Some(value), Some(initial.stage.id))
                  case Some((count, _)) =>
                    val at = value.reference.moves.take(count)
                    expected += at
                    derivedStage(packet, paths, seed, value.reference, at, value, Some(initial.stage)) match
                      case Some(stage) => localTask(packet, paths, stage, choices).foreach(stages += _)
                      case None =>
                        unavailable += at
                        boundaries += FollowupBoundary(at, "resource_unavailable", Some(value), Some(initial.stage.id))
          }
        }
      }
      // Old timing aliases keep their immutable duties at a current stage.
      // They are not extra functional votes or extra displayed realizations.
      val prefixes = stages.toList.map(stage => stage.stage.prefix -> stage.stage.referenceMove).toSet
      legacyStages(packet, paths, entries, roots, profile, minimumDepth).filter(stage =>
        stage.rootOwner.answer == seed.owner.answer && stage.pawn == seed.pawn &&
          prefixes(stage.prefix -> stage.referenceMove)).foreach { alias =>
        localTask(packet, paths, alias, choices)
      }
      new Family(packet, paths, seed.owner, seed.pawn, rootReadings,
        stages.toList.groupBy(task => (task.stage.prefix, task.stage.referenceMove, task.stage.pawn))
          .values.map(_.head).toList.sortBy(task => (task.stage.level, task.stage.prefix.mkString(" "))),
        expected.toSet, initialExpected.toSet, unavailable.toSet, boundaries.toList.distinct,
        choices.obligations.toList.distinct)
    }

  /** Keep the original timing question/context/answer intact. Each newly
    * selected comparator is another immutable head at this exact stage.
    */
  def withQuestions(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], searches: List[Search], roots: Set[String], profile: String,
      minimumDepth: Int): List[Entry] =
    val currentFamily = family(packet, paths, entries, searches, roots, profile, minimumDepth)
    val oldTask = Option.when(currentFamily.isEmpty)(
      task(packet, paths, entries, searches, roots, profile, minimumDepth)).flatten
    val obligations = currentFamily.map(_.obligations).orElse(oldTask.map(_.obligations)).getOrElse(Nil)
    if obligations.isEmpty then entries
    else
      val byOwner = obligations.groupBy(_.owner.proposalId)
      var room = (MaximumRequirements - entries.map(_.request.requirements.size).sum).max(0)
      entries.map { entry =>
        if !byOwner.contains(entry.request.id) then entry
        else
          val ownedObligations = byOwner(entry.request.id)
          // Compute the whole family's current union before retiring any
          // pending alias. A sibling stage cannot stop another live question.
          val currentIds = ownedObligations.map(_.comparison.id).toSet
          var current = entry.withCurrentDecisions(ValuePrefix, currentIds)
          ownedObligations.sortBy(value => (value.phase, value.comparison.prefixMovesUci.mkString(" "), value.comparison.id))
            .foreach { obligation =>
              val requirements = obligation.requirements
              val known = current.request.requirements.map(value => value.id -> value).toMap
              require(requirements.forall(value => known.get(value.id).forall(_ == value)),
                "an anchored decision cannot rewrite an earlier exact question")
              val added = requirements.filterNot(value => known.contains(value.id))
              if added.size <= room then
                room -= added.size
                val stageEvidence = currentFamily.toList.flatMap(_.stages.map(_.stage)) ++ oldTask.toList.map(_.stage)
                val refs = (List(obligation.owner.proposalId, obligation.owner.answer.workId,
                  obligation.owner.answer.executionKeySha256) ++ obligation.sourceIds ++
                  stageEvidence.filter(_.rootOwner.proposalId == entry.request.id).flatMap(_.evidence)).distinct
                  .filter(value => value.nonEmpty && value.length <= 256).take(64)
                val source = Source("generator", "anchored-choice-value", Some(SourceVersion), refs)
                val heads = List(obligation.owner.comparison.id, obligation.context.id, obligation.comparison.id)
                val members = (current.request.requirements.map(_.id) ++ added.map(_.id)).distinct
                val arm = GeneratedArm(CommentarySelection.stableId("anchored-value-arm",
                  List(current.request.id, obligation.comparison.id, refs.mkString(" "))), Nil,
                  obligation.owner.comparison.referenceMove, Some(obligation.owner.comparison.alternativeMove),
                  heads, members, source, refs)
                current = current.copy(request = current.request.copy(requirements = current.request.requirements ++ added),
                  readings = current.readings ++ added.map(value => Reading(value.id)),
                  generatedArms = if current.generatedArms.exists(_.id == arm.id) then current.generatedArms
                    else current.generatedArms :+ arm)
            }
          current
      }
