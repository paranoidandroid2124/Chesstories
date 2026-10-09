package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{LegalMoveResource, PositionResourceView}
import lila.chessjudgment.model.judgment.*

/** A later observed use may have a different supporter. This reader certifies
  * access and a closed, conditional capture inventory, never usefulness or the
  * cause of an earlier numerical judgment. Missing local traces are ordinary
  * ObserveMove requests; a probe is not a public proof.
  */
private[runtime] object FunctionalRealizationReading:
  import CommentaryProposal.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import FunctionalReplyReading.Resource
  import RootQuestionSummary.MoveRef

  val Producer = "C-function-realization"
  val MaximumStages = 4
  val MaximumPriceLeaves = 8
  val MaximumLocalWaves = 4
  private val MaximumCaptures = 3

  final case class Provider(original: RelationColoredPieceWitness, current: RelationColoredPieceWitness)
  final case class Stage(resource: Resource, path: PreparedPath, offset: Int,
      before: RelationColoredPieceWitness, arrived: RelationColoredPieceWitness,
      providers: List[Provider], id: String):
    def prefix: List[String] = path.moves.take(offset)
    def arrival: List[String] = path.moves.take(offset + 1)

  final case class Price(path: PreparedPath, through: Int):
    def events: List[ObservedLineMaterialEvent] = path.certified.flatMap(_._2.material).toList
      .flatMap(_.events).filter(event => event.plyOffset <= through)

  final class Checked private[FunctionalRealizationReading] (
      private[FunctionalRealizationReading] val packet: EvidenceBackedJudgmentPacket,
      private[FunctionalRealizationReading] val prepared: PreparedPaths,
      val stage: Stage, val capturers: List[RelationColoredPieceWitness],
      val prices: List[Price], val evidence: List[String]):
    def stageReferences: List[MoveRef] = (List(MoveRef(stage.path, 0)) ++
      Option.when(stage.offset > 0)(MoveRef(stage.path, stage.offset - 1)).toList ++
      List(MoveRef(stage.path, stage.offset))).distinct
    def priceReferences: List[MoveRef] = prices.flatMap(price =>
      (stage.offset + 1 to price.through).map(MoveRef(price.path, _))).distinct
    def references: List[MoveRef] = (stageReferences ++ priceReferences).distinct
    def covered: List[MoveRef] = prices.flatMap(price => price.events.filter(event =>
      event.plyOffset > stage.offset && event.capture.nonEmpty).map(event => MoveRef(price.path, event.plyOffset))).distinct

  final case class Reading(stage: Stage, checked: Option[Checked], missing: List[List[String]],
      reason: Option[String])

  private def owned(paths: PreparedPaths, path: PreparedPath): Boolean =
    paths.paths.exists(_ eq path) && path.certified.nonEmpty && path.ids == path.declared &&
      path.moves == path.sourceMoves && path.positions.size == path.moves.size + 1

  private def viewSources(packet: EvidenceBackedJudgmentPacket, view: PositionResourceView,
      kinds: Set[RelationFactKind]): Option[List[String]] =
    if !kinds.subsetOf(view.completeKinds) then None else
      val records = view.records.filter(record => record.payload match
        case fact: RelationFactEvidence => kinds(fact.kind)
        case _ => false)
      Option.when(records.forall(record => packet.evidenceGraph.record(record.ref).contains(record) &&
        packet.evidenceGraph.proofEligible(record)))(records.map(_.ref.id).distinct.sorted)

  private def original(paths: PreparedPaths, packet: EvidenceBackedJudgmentPacket,
      path: PreparedPath, at: Int, current: RelationColoredPieceWitness): Option[RelationColoredPieceWitness] =
    paths.identitiesAfter(packet, path, at).collect {
      case (value, Some(Some(found))) if found == current => value
    }.distinct match
      case value :: Nil => Some(value)
      case _ => None

  /** The key ends at the actual arrival. An unrelated forecast tail is neither
    * a necessary condition nor another realization of this same observation. */
  def stage(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, resource: Resource,
      path: PreparedPath, offset: Int): Option[Stage] =
    paths.requireOwner(packet)
    val root = resource.owner.comparison
    for
      _ <- Option.when(owned(paths, path) && offset >= 3 && offset < path.moves.size &&
        root.prefixMovesUci.isEmpty && Set(root.referenceMove, root.alternativeMove)(path.moves.head) &&
        resource.condition.kind == "control" && resource.condition.square.contains(resource.square) &&
        resource.condition.subject.exists(provider => provider.side == resource.actor.side && provider != resource.actor)) (())
      before <- paths.trackedPieceAfter(path, resource.actor.witness, offset).flatten
      arrived <- paths.trackedPieceAfter(path, resource.actor.witness, offset + 1).flatten
      if before.side == arrived.side && before.role == arrived.role &&
        arrived.role == resource.actor.witness.role && arrived.square.key == resource.square
      prior <- paths.positionView(path, offset)
      after <- paths.positionView(path, offset + 1)
      if prior.position.sideToMove.contains(before.side) && after.position.sideToMove.contains(!before.side)
      _ <- viewSources(packet, prior, Set(RelationFactKind.LegalMove, RelationFactKind.GeometricControl))
      _ <- viewSources(packet, after, Set(RelationFactKind.LegalMove, RelationFactKind.GeometricControl))
      rows <- prior.legalRows
      row <- rows.filter(row => row.move == path.moves(offset) && row.actor == before &&
        row.to == arrived.square && row.capture.isEmpty && row.promotionRole.isEmpty) match
          case value :: Nil => Some(value)
          case _ => None
      provider <- resource.condition.subject
      old <- paths.trackedPieceAfter(path, provider.witness, offset + 1)
      position = new MoveWorkProjection.Position(after)
      current = position.controllers(resource.square, arrived.side).filterNot(_ == arrived)
      if current.nonEmpty && !old.exists(current.contains)
      replacements = current.map(piece => original(paths, packet, path, offset + 1, piece).map(Provider(_, piece)))
      if replacements.forall(_.nonEmpty) && replacements.flatten.forall(_.original != provider.witness)
    yield Stage(resource, path, offset, before, arrived, replacements.flatten,
      CommentarySelection.stableId("functional-realization", List(resource.owner.proposalId,
        root.id, resource.contextId, resource.condition.json.toString, resource.actor.json.toString,
        resource.square, resource.move) ++ path.moves.take(offset + 1)))

  def stages(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      resources: List[Resource]): List[Stage] =
    paths.requireOwner(packet)
    resources.distinct.flatMap(resource => paths.investigationPaths.flatMap(path =>
      path.moves.indices.drop(3).flatMap(offset => stage(packet, paths, resource, path, offset))))
      .groupBy(_.id).values.map(_.minBy(value => (value.path.moves.size, value.path.line.ref.id)))
      .toList.sortBy(value => (value.offset, value.id))

  /** Enumerate all currently legal captures, recaptures and second recaptures.
    * A further trade or an unresolved checking leaf remains partial. Nominal
    * material prices cannot establish that a sacrifice or an alternative plan
    * is bad, and this reader deliberately has no evaluation field.
    */
  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, value: Stage): Reading =
    paths.requireOwner(packet)
    val missing = scala.collection.mutable.ListBuffer.empty[List[String]]
    val leaves = scala.collection.mutable.ListBuffer.empty[Price]
    val evidence = scala.collection.mutable.ListBuffer.empty[String]
    var failure: Option[String] = None
    def fail(reason: String): Unit = if failure.isEmpty then failure = Some(reason)
    val canonical = stage(packet, paths, value.resource, value.path, value.offset)
    if !canonical.contains(value) then return Reading(value, None, Nil, Some("foreign_or_stale_realization"))
    for count <- List(value.offset, value.offset + 1) do
      paths.positionView(value.path, count).flatMap(view => viewSources(packet, view,
        Set(RelationFactKind.LegalMove, RelationFactKind.GeometricControl))).foreach(evidence ++= _)
    var firstCapturers: List[RelationColoredPieceWitness] = Nil
    def walk(prefix: List[String], target: RelationColoredPieceWitness, depth: Int): Unit =
      CommentaryProposal.observedPrefix(packet, paths, prefix) match
        case None => missing += prefix
        case Some(path) if !owned(paths, path) => fail("uncertified_capture_trace")
        case Some(path) =>
          paths.positionView(path, prefix.size) match
            case None => fail("missing_current_capture_inventory")
            case Some(view) =>
              val sources = viewSources(packet, view, Set(RelationFactKind.LegalMove, RelationFactKind.GeometricControl))
              if sources.isEmpty then fail("incomplete_or_ineligible_capture_inventory")
              else
                evidence ++= sources.get
                view.legalRows match
                  case None => fail("incomplete_capture_inventory")
                  case Some(rows) =>
                    val captures = rows.filter(_.capture.contains(target))
                    if depth == 0 then firstCapturers = captures.map(_.actor).distinct
                    if depth == 0 && view.position.sideToMove.exists(side =>
                        view.currentCheckSources(side).nonEmpty) then fail("checking_arrival_requires_response")
                    if depth >= MaximumCaptures && captures.nonEmpty then fail("capture_horizon_exceeded")
                    else if captures.isEmpty then
                      if view.position.sideToMove.exists(side => view.currentCheckSources(side).nonEmpty) &&
                          !(path.automaticTerminal.nonEmpty && path.moves.size == prefix.size) then
                        fail("checking_leaf_requires_response")
                      else if depth > 0 then leaves += Price(path, prefix.size - 1)
                    else captures.foreach { row =>
                      val child = prefix :+ row.move
                      if child.size > CommentaryFact.MaximumPly then fail("capture_horizon_exceeded")
                      else CommentaryProposal.observedPrefix(packet, paths, child) match
                        case None => missing += child
                        case Some(next) =>
                          val actor = original(paths, packet, path, prefix.size, row.actor)
                          val victim = original(paths, packet, path, prefix.size, target)
                          val arrived = actor.flatMap(original => paths.trackedPieceAfter(next, original, child.size).flatten)
                          val removed = victim.flatMap(original => paths.trackedPieceAfter(next, original, child.size))
                          val event = next.certified.flatMap(_._2.material).toList.flatMap(_.events)
                            .find(_.plyOffset == prefix.size)
                          if !owned(paths, next) || next.ids.take(prefix.size) != path.ids.take(prefix.size) ||
                              next.positions.take(prefix.size + 1) != path.positions.take(prefix.size + 1) ||
                              arrived.isEmpty || !removed.contains(None) || !event.exists(event =>
                                event.moveUci == row.move && event.legalMoveSemanticId == row.sourceId &&
                                  event.movement.beforeRole == row.actor.role && event.movement.side == row.actor.side &&
                                  event.movement.from == row.actor.square && event.movement.to == row.to &&
                                  event.capture.exists(capture => capture.square == target.square &&
                                    capture.capturedRole == target.role && capture.side == row.actor.side)) then
                            fail("capture_identity_or_material_mismatch")
                          else walk(child, arrived.get, depth + 1)
                    }
    walk(value.arrival, value.arrived, 0)
    if leaves.size + missing.distinct.size > MaximumPriceLeaves then fail("capture_inventory_over_limit")
    val complete = failure.isEmpty && missing.isEmpty
    val checked = Option.when(complete)(new Checked(packet, paths, value, firstCapturers,
      leaves.toList.distinct.sortBy(price => price.path.moves.take(price.through + 1).mkString(" ")),
      (evidence.toList ++ value.path.ids.take(value.offset + 1) ++
        leaves.flatMap(price => price.path.ids.take(price.through + 1))).distinct))
    Reading(value, checked, missing.toList.distinct,
      failure.orElse(Option.when(missing.nonEmpty)("capture_trace_not_admitted")))

  def valid(paths: PreparedPaths, value: Checked): Boolean =
    (value.prepared eq paths) && read(value.packet, paths, value.stage).checked.exists(checked =>
      checked.stage == value.stage && checked.capturers == value.capturers &&
        checked.prices == value.prices && checked.evidence == value.evidence)

  def readings(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      resources: List[Resource]): List[Reading] =
    val found = stages(packet, paths, resources)
    if found.size > MaximumStages then found.map(value =>
      Reading(value, None, Nil, Some("realization_inventory_over_limit")))
    else found.map(read(packet, paths, _))

  /** Candidate drafting names compact owners, not long public proof record IDs.
    * The public Checked above retains every exact eligible proof record instead.
    */
  def withQuestions(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], resources: List[Resource]): List[Entry] =
    readings(packet, paths, resources).foldLeft(entries) { (current, reading) =>
      val value = reading.stage
      if reading.reason.exists(reason => reason != "capture_trace_not_admitted") || reading.missing.isEmpty then current
      else
        val prior = current.find(_.request.id == value.id)
        val observations = reading.missing.map(moves => ObserveMove(
          CommentarySelection.stableId("realization-observe", value.id :: moves), moves.init, moves.last))
        val requirements = (prior.toList.flatMap(_.request.requirements) ++ observations).distinct
        val source = Source("generator", Producer, Some("1"),
          List(value.resource.owner.proposalId, value.resource.contextId, value.id))
        val request = Request(value.id, Origin(source, source, source, source), requirements)
        if current.filterNot(_.request.id == value.id).map(_.request.requirements.size).sum + requirements.size >
            MaximumRequirements || !validRequests(List(request)) then current
        else
          val readings = prior.toList.flatMap(_.readings)
          val next = Entry(request, requirements.map(requirement =>
            readings.find(_.requirementId == requirement.id).getOrElse(CommentaryProposal.Reading(requirement.id))),
            admission = "generated", questionText = Some("Which current supporter and complete conditional captures accompany this later recorded use?"))
          current.filterNot(_.request.id == value.id) :+ next
    }

  /** One conditional option can already contain its reference's complete
    * prefix. Reuse that exact available recovery witness in DETAIL only;
    * retain the proof, physical events, and numerical owner unchanged. This
    * is not a FEN match or permission to transfer a forecast's lifetime/price.
    */
  private[runtime] def normalizeDetail(paths: PreparedPaths,
      sections: List[CommentaryAnswer.Section]): Option[List[CommentaryAnswer.Section]] =
    import CommentaryAnswer.*
    val options = sections.flatMap(_.statements.collect { case value: LegalReplyOption => value })
    val possible = options.filter(value => (value.reference ne value.available) &&
      owned(paths, value.reference) && owned(paths, value.available) &&
      value.available.moves.size > value.reference.moves.size &&
      value.available.moves.startsWith(value.reference.moves) &&
      value.available.ids.take(value.reference.ids.size) == value.reference.ids &&
      value.available.positions.take(value.reference.positions.size) == value.reference.positions &&
      FunctionalReplyReading.valid(paths, value) &&
      FunctionalReplyReading.valid(paths, value.copy(reference = value.available)))
    val aliases = possible.groupBy(_.reference).flatMap { (reference, values) =>
      values.map(_.available).distinct match
        case available :: Nil => Some(reference -> available)
        case _ => None
    }
    def alias(move: MoveRef): MoveRef = aliases.get(move.path).fold(move)(path => MoveRef(path, move.offset))
    def eventsSame(path: PreparedPath, replacement: PreparedPath,
        events: List[ObservedLineMaterialEvent]): Boolean =
      path.certified.flatMap(_._2.material).exists(old =>
        replacement.certified.flatMap(_._2.material).exists(current => events.forall(event =>
          old.events.contains(event) && current.events.contains(event) &&
            path.ids.lift(event.plyOffset) == replacement.ids.lift(event.plyOffset) &&
            path.positions.lift(event.plyOffset) == replacement.positions.lift(event.plyOffset) &&
            path.positions.lift(event.plyOffset + 1) == replacement.positions.lift(event.plyOffset + 1))))
    val unsupported = sections.exists(section => section.statements.exists { statement =>
      if !statement.references.exists(move => aliases.contains(move.path)) then false
      else statement match
        case value: LegalReplyOption => aliases.get(value.reference).exists(path =>
          !FunctionalReplyReading.valid(paths, value.copy(reference = path))) ||
          statement.references.exists(move => aliases.contains(move.path) && (move.path ne value.reference))
        case value: MaterialEvents => aliases.get(value.path).exists(path => !eventsSame(value.path, path, value.events))
        case _ => true
    } || section.focus.exists(focus => aliases.contains(focus.move.path) &&
      (focus.note.edge.evidence.id != focus.move.path.ids(focus.move.offset) ||
        focus.note.edge.from != focus.move.path.positions(focus.move.offset) ||
        focus.note.edge.to != focus.move.path.positions(focus.move.offset + 1))))
    if unsupported then None else
      val result = sections.map(section => section.copy(statements = section.statements.map {
        case value: LegalReplyOption => aliases.get(value.reference).fold(value)(path => value.copy(reference = path))
        case value: MaterialEvents => aliases.get(value.path).fold(value)(path => value.copy(path = path))
        case value => value
      }, required = section.required.map(alias), focus = section.focus.map(focus => focus.copy(move = alias(focus.move)))))
      def physical(move: MoveRef) = (move.path.ids(move.offset), move.path.positions(move.offset),
        move.path.positions(move.offset + 1), move.path.moves(move.offset))
      val oldCovered = sections.flatMap(_.covered).map(physical)
      val newCovered = result.flatMap(_.covered).map(physical)
      val oldRequired = sections.flatMap(_.required).map(physical)
      val newRequired = result.flatMap(_.required).map(physical)
      Option.when(oldCovered == newCovered && oldRequired == newRequired)(result)

  /** Optional factual detail has no vote in selecting the root answer. Failure
    * of its existing delivery bounds leaves that entire answer untouched.
    */
  def withDetail(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      tasks: List[FunctionalChoiceReading.Task], previous: CommentaryAnswer.Answer): CommentaryAnswer.Answer =
    import CommentaryAnswer.*
    paths.requireOwner(packet)
    val resources = tasks.map(_.resource).distinct.filter(resource =>
      resource.owner.comparison.prefixMovesUci.isEmpty && pair.size == 2 &&
        pair.map(_.moves.headOption).flatten.toSet == Set(resource.owner.comparison.referenceMove,
          resource.owner.comparison.alternativeMove) && resource.owner.answer.forall(answer =>
          Set(answer.referenceLineId, answer.alternativeLineId) == pair.map(_.line.ref.id).toSet))
    val existing = previous.details.flatMap(_.statements.collect { case FunctionRealization(value) => value })
    val additions = readings(packet, paths, resources).flatMap(_.checked).filterNot(value =>
      existing.exists(prior => prior.stage.id == value.stage.id && valid(paths, prior))).map(value =>
        Section(List(FunctionRealization(value))))
    if additions.isEmpty then previous else
      val addedEvidence = additions.flatMap(_.statements.collect { case FunctionRealization(value) => value.evidence }).flatten
      normalizeDetail(paths, previous.details).fold(previous) { normalized =>
        // A checked later fact belongs beside the completed local decision
        // whose exact alternative witness already contains it. This groups
        // complete sections, without transferring that earlier score to the
        // later action or deleting another section's prices.
        def attachment(sections: List[Section], value: Checked): Int =
          sections.indexWhere(_.statements.exists {
            case ResourceDecision(decision) => decision.other.exists(move => (move.path eq value.stage.path) &&
              value.stage.prefix.startsWith(decision.branch.resourceQuestion.prefixMovesUci))
            case _ => false
          })
        val ordered = additions.sortBy { section =>
          val value = section.statements.head.asInstanceOf[FunctionRealization].value
          (if attachment(normalized, value) >= 0 then 0 else 1, value.stage.offset, value.stage.id)
        }
        val (result, admitted) = ordered.foldLeft(previous.copy(details = normalized) -> false) {
          case ((retained, alreadyAdmitted), section) =>
          val value = section.statements.head.asInstanceOf[FunctionRealization].value
          val index = attachment(retained.details, value)
          val grouped = if index < 0 then retained.details :+ section
            else retained.details.take(index + 1) ++ List(section) ++ retained.details.drop(index + 1)
          val candidate = retained.copy(details = grouped,
            evidence = (retained.evidence ++ value.evidence).distinct)
          if RootQuestionSummary.realize(paths, pair, candidate).nonEmpty then candidate -> true
          else retained -> alreadyAdmitted
        }
        if admitted then result else previous
      }
