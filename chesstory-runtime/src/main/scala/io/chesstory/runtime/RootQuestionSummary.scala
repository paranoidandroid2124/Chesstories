package io.chesstory.runtime

import scala.collection.mutable
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** A root question owns complete reading units and exact move occurrences.
  * Its material ledger, physical identities and position inventories are the
  * existing admitted resources; this reader neither searches nor evaluates.
  */
private[runtime] object RootQuestionSummary:
  import MoveWorkProjection.{ Piece, Position }
  import ObservedLineProtocol.{ PreparedPath, PreparedPaths }
  import LineContinuationInvestigation.{ Event, Reading, DirectSupportUse }

  private def remainingDetails(selected: List[CommentaryAnswer.Section], core: List[CommentaryAnswer.Section]) =
    val included = core.flatMap(_.statements).toSet
    selected.map { section =>
      val remaining = section.statements.filterNot(included)
      val refs = remaining.flatMap(_.references).toSet
      CommentaryAnswer.Section(remaining, section.required.filter(refs), section.focus.filter(f => refs(f.move)))
    }.filter(_.statements.nonEmpty)

  case class MoveRef(path: PreparedPath, offset: Int):
    def json: JsObject = Json.obj("line_id" -> path.line.ref.id, "occurrence_id" -> path.ids(offset))
  case class Unit(role: String, en: List[String | MoveRef], ko: List[String | MoveRef],
      covered: List[MoveRef] = Nil, required: List[MoveRef] = Nil,
      focus: List[CommentaryAnswer.Focus] = Nil, englishOnly: Boolean = false,
      recordedEnds: List[MoveRef] = Nil, expressed: List[CommentaryAnswer.Statement] = Nil):
    def contentReferences: List[MoveRef] = expressed.flatMap(_.references).distinct
    def proofReferences: List[MoveRef] = (refs ++ contentReferences).distinct
    def actionReferences: List[MoveRef] =
      if expressed.isEmpty then refs else expressed.flatMap(_.actionReferences).distinct
    def json: JsObject =
      def parts(values: List[String | MoveRef]): JsArray = JsArray(values.map {
        case value: String => JsString(value)
        case value: MoveRef => value.json
      })
      Json.obj("role" -> role, "en" -> parts(en), "ko" -> parts(ko),
        "covered_moves" -> covered.map(_.json)) ++
        Option.when(expressed.nonEmpty)(Json.obj("proof_moves" -> proofReferences.map(_.json),
          "action_moves" -> actionReferences.map(_.json))).getOrElse(Json.obj()) ++
        Option.when(required.nonEmpty)(Json.obj("required_moves" -> required.distinct.map(_.json))).getOrElse(Json.obj()) ++
        Option.when(recordedEnds.nonEmpty)(Json.obj("recorded_end_moves" -> recordedEnds.distinct.map(_.json))).getOrElse(Json.obj()) ++
        Option.when(englishOnly)(Json.obj("english_only" -> true)).getOrElse(Json.obj()) ++
        Option.when(focus.nonEmpty)(Json.obj("focus" -> focus.map { value =>
          val frame = (value.note.highlight \ "frames").as[List[JsObject]].find(frame =>
            (frame \ "phase").as[String] == value.phase).get
          value.move.json ++ frame
        })).getOrElse(Json.obj())
    def refs: List[MoveRef] = en.collect { case move: MoveRef => move }

  case class Result(text: String, json: JsObject, evidence: List[String])

  private def immediateTrades(material: LineMaterialSummary, moves: List[String]): Map[Int, List[Int]] =
    val captures = material.captures.map(capture => capture.plyOffset -> capture).toMap
    captures.toList.flatMap { (offset, next) =>
      captures.get(offset - 1).filter(first => offset > 0 && next.recapture && next.side != first.side &&
        next.square.key == moves(offset - 1).slice(2, 4)).toList.flatMap(_ =>
        List((offset - 1) -> offset, offset -> (offset - 1)))
    }.groupMap(_._1)(_._2)

  /** Keep the chosen answer intact at the delivery boundary. Wording cannot
    * substitute another occurrence, language or uncertified continuation.
    */
  def fromUnits(paths: PreparedPaths, pair: List[PreparedPath], question: String,
      units: List[Unit], evidence: List[String], detail: List[MoveRef] = Nil,
      detailUnits: List[Unit] = Nil): Option[Result] =
    val mentioned = units.flatMap(_.refs)
    val allUnits = units ++ detailUnits
    val functionOwners = allUnits.flatMap(_.expressed).collect {
      case CommentaryAnswer.FunctionPurpose(value) if value.belongsTo(paths) => value
      case CommentaryAnswer.FunctionReply(value, _) if value.belongsTo(paths) => value
      case CommentaryAnswer.ReferenceExchangeReply(_, value, _) if value.belongsTo(paths) => value
    }.distinct
    def referencesFor(layer: List[Unit], extra: List[MoveRef] = Nil): List[MoveRef] =
      layer.flatMap(u => u.proofReferences ++ u.ko.collect { case move: MoveRef => move } ++
        u.covered ++ u.required ++ u.recordedEnds ++ u.focus.map(_.move)) ++ extra
    val references = referencesFor(units) ++ referencesFor(detailUnits, detail)
    val used = references.map(_.path).distinct
    val witnesses = used.filterNot(p => pair.exists(_ eq p))
    // The selected answer and the supplementary record each own a bounded
    // proof. A detail-only conditional example cannot evict a complete core
    // comparison, and neither layer can borrow the other's unused capacity.
    def boundedWitnesses(layer: List[Unit], extra: List[MoveRef] = Nil): Boolean =
      referencesFor(layer, extra).map(_.path).distinct.count(p => !pair.exists(_ eq p)) <= 5
    // A named conditional example can contrast another legal root choice.
    // It remains a separate proof, never the current evaluation pair. Only
    // its fully reread board condition and exact legal paths admit that root.
    val conditionalUnits = if question == "functional_response" then allUnits else detailUnits
    val retainedChoice = units.flatMap(_.expressed).collect {
      case CommentaryAnswer.RetainedExchangeFunction(value) if question == "exchange_function" &&
          value.belongsTo(paths) => value.kept.toList.map(_.path)
    }.flatten.filterNot(path => pair.exists(primary => primary.ids.head == path.ids.head)).distinct
    val purposeChoices = units.flatMap(_.expressed).collect {
      case CommentaryAnswer.ChoicePurpose(value) if question == "move_purpose" && value.belongsTo(paths) =>
        value.arms.map(_.path)
    }.flatten
    val conditionalPaths = (retainedChoice ++ purposeChoices ++ conditionalUnits.flatMap(_.expressed).collect {
      case value: CommentaryAnswer.LegalReplyOption if FunctionalReplyReading.valid(paths, value) &&
          FunctionalReplyReading.relevant(paths, pair, value) =>
        List(value.reference, value.alternative, value.available) ++ value.price.toList
    }.flatten).distinct
    def valid(ref: MoveRef): Boolean = used.exists(_ eq ref.path) && ref.offset >= 0 &&
      ref.offset < ref.path.moves.size && ref.path.ids == ref.path.declared &&
      ref.path.moves == ref.path.sourceMoves && ref.path.certified.nonEmpty &&
      paths.paths.exists(_ eq ref.path)
    // The selected content owns its proof, including captures summarized by a
    // material account. Inline notation is only the displayed subset of it.
    // A final presence anchor does not activate every preceding capture.
    def visibleAccount(layer: List[Unit], extra: List[MoveRef] = Nil): Boolean =
      val shown = layer.flatMap(_.actionReferences) ++ extra
      val owned = layer.flatMap(u => u.proofReferences ++ u.covered ++ u.recordedEnds) ++ extra
      owned.map(_.path).distinct.forall { path =>
        val visible = shown.filter(_.path eq path).map(_.offset).toSet
        val covered = layer.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet
        val recordedEnds = layer.flatMap(_.recordedEnds).filter(_.path eq path).map(_.offset).toSet
        path.certified.exists { (_, fact) => fact.material.exists { material =>
          val captures = material.captures.map(event => event.plyOffset -> event).toMap
          val trades = immediateTrades(material, path.moves)
          val checks = fact.lineEvents.filter(_.kind == LineEventKind.Check).map(_.plyOffset).toSet
          covered.forall(n => visible(n) && captures.contains(n)) && visible.forall { n =>
            val ended = n == path.moves.size - 1 &&
              (path.automaticTerminal.nonEmpty || recordedEnds(n))
            (!captures.contains(n) || covered(n)) &&
              (!checks(n) || visible(n + 1) || ended) && trades.getOrElse(n, Nil).forall(visible)
          }
        }}
      }
    Option.when(pair.size == 2 && units.nonEmpty && units.size <= 8 && functionOwners.size <= 1 &&
      boundedWitnesses(units) && boundedWitnesses(detailUnits, detail) &&
      pair.forall(path => mentioned.contains(MoveRef(path, 0))) && references.forall(valid) &&
      visibleAccount(units, pair.map(MoveRef(_, 0))) && visibleAccount(detailUnits, detail) &&
      witnesses.forall(path => pair.exists(primary => path.positions.headOption == primary.positions.headOption &&
        (path.ids.headOption == primary.ids.headOption && path.moves.headOption == primary.moves.headOption ||
          conditionalPaths.exists(_ eq path)))) &&
      detailUnits.size <= 8 && allUnits.forall(u => Set("condition", "reply", "cost")(u.role) && u.refs.nonEmpty &&
        ((u.role == "cost") == u.covered.nonEmpty) &&
        u.en.size <= 80 && u.ko.size <= 80 &&
        u.refs.map(_.json.toString).sorted == u.ko.collect { case r: MoveRef => r.json.toString }.sorted &&
        u.covered.distinct.size == u.covered.size && u.covered.forall(u.proofReferences.contains) &&
        u.required.forall(u.proofReferences.contains) &&
        (u.expressed.isEmpty || (u.covered.toSet == u.expressed.flatMap(_.covered).toSet &&
          u.required.forall(u.contentReferences.contains) && u.refs.forall(ref =>
            u.contentReferences.contains(ref) || ref.offset == 0 && u.contentReferences.exists(_.path eq ref.path)))) &&
        u.recordedEnds.forall(ref => u.proofReferences.contains(ref) && ref.offset == ref.path.moves.size - 1 &&
          ref.path.automaticTerminal.isEmpty) && (!u.englishOnly || u.en == u.ko) &&
        u.focus.map(_.move).distinct.size == u.focus.size && u.focus.forall(f =>
          u.proofReferences.contains(f.move) && Set("before", "after")(f.phase) &&
          f.move.path.ids(f.move.offset) == f.note.edge.evidence.id &&
          f.move.path.positions(f.move.offset) == f.note.edge.from &&
          f.move.path.positions(f.move.offset + 1) == f.note.edge.to))) {
      val text = units.map(_.en.map {
        case value: String => value
        case move: MoveRef => paths.notation.line(move.path.moves)(move.offset)
      }.mkString).mkString(" ")
      Result(text, Json.obj("schema_version" -> 2, "question" -> question, "units" -> units.map(_.json),
        "detail_moves" -> detail.distinct.map(_.json)) ++
        functionOwners.headOption.map(value => Json.obj("move_purpose" -> value.ownershipJson)).getOrElse(Json.obj()) ++
        Option.when(retainedChoice.size == 1)(Json.obj("retained_choice_line_id" ->
          retainedChoice.head.line.ref.id)).getOrElse(Json.obj()) ++
        Option.when(detailUnits.nonEmpty)(Json.obj("detail_units" -> detailUnits.map(_.json))).getOrElse(Json.obj()) ++
        Option.when(witnesses.nonEmpty)(Json.obj("witness_line_ids" -> witnesses.map(_.line.ref.id))).getOrElse(Json.obj()),
        (evidence ++ used.flatMap(p => p.certified.map(_._1.ref.id).toList ++ p.ids)).distinct)
    }.filter(_.text.length <= 4000)
  private val names = Map("pawn" -> "폰", "knight" -> "나이트", "bishop" -> "비숍",
    "rook" -> "룩", "queen" -> "퀸", "king" -> "킹")
  // These six fixed role names need no general Korean prose parser.
  private def subject(p: Piece): String = names(p.role.name) + (if p.role.name == "knight" then "가" else "이")
  private def obj(p: Piece): String = names(p.role.name) + (if p.role.name == "knight" then "를" else "을")
  case class Account(events: List[Event], offsets: List[Int], dutyCosts: List[(Event, Event)], complete: Boolean)

  def prepare(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position]): Option[List[Arm]] =
    paths.requireOwner(packet)
    if pair.size != 2 || pair.head.positions.headOption != pair.last.positions.headOption ||
        pair.map(_.moves.headOption).distinct.size != 2 || pair.exists(p =>
          p.certified.flatMap(_._2.material).isEmpty || p.ids != p.declared || p.moves != p.sourceMoves || p.moves.isEmpty ||
          p.automaticTerminal.nonEmpty)
    then return None
    val readings = paths.continuationReadings.map(r => r.path.line.ref.id -> r).toMap
    if pair.exists(p => !readings.get(p.line.ref.id).exists(r => (r.path eq p) && r.complete)) then return None

    Some(pair.map(p => new Arm(packet, paths, p, positions, readings(p.line.ref.id))))

  /** A bounded fallback question follows an already admitted contact in both
    * arms. Ordering remains the caller's explanation order, not a new score.
    */
  def readSelected(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position], selected: List[(PreparedPath, MoveWorkProjection.LocalCandidate)]): Option[Result] =
    selectedAnswer(packet, paths, pair, positions, selected).flatMap(answer => realize(paths, pair, answer))

  /** Delivery of an existing selected comparison. The complete ledger stays
    * in detail; a selected answer owns its actual moves and their mandatory
    * replies, without claiming a total for unrelated later exchanges.
    */
  def completePairAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], question: String, lead: List[CommentaryAnswer.Section],
      details: List[CommentaryAnswer.Section] = Nil): Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    paths.requireOwner(packet)
    if pair.size != 2 || pair.head.positions.headOption != pair.last.positions.headOption ||
        pair.map(_.moves.headOption).distinct.size != 2 || pair.exists(path =>
          !paths.paths.exists(_ eq path) || path.moves.isEmpty || path.moves != path.sourceMoves ||
          path.ids != path.declared || path.certified.flatMap(_._2.material).isEmpty) then return None
    val accounts = pair.map { path =>
      val material = path.certified.flatMap(_._2.material).get
      val checks = path.certified.toList.flatMap(_._2.lineEvents)
        .filter(_.kind == LineEventKind.Check).map(_.plyOffset).distinct
      val offsets = (List(0) ++ material.events.map(_.plyOffset) ++ checks ++
        checks.filter(_ + 1 < path.moves.size).map(_ + 1)).distinct.sorted
      val cost = CommentaryMaterialAccount.calculate(material, material.events.map(_.plyOffset),
        offsets, path.moves.size)
      val root = MoveRef(path, 0)
      val end = MoveRef(path, path.moves.size - 1)
      val content: Statement = if material.events.isEmpty then NoMaterialChange(root, end)
        else cost.fold[Statement](MaterialEvents(path, material.events))(Material(path, _))
      // A terminal result owns its exact last transition, never a finite PV
      // horizon. Keep the preceding cost visible alongside that result.
      val result = path.automaticTerminal.map(Terminal(end, _)).toList ++
        Option.when(path.automaticTerminal.isEmpty &&
          (path.moves.size == 1 || checks.contains(path.moves.size - 1)))(RecordedEnd(end)).toList
      val missing = offsets.map(MoveRef(path, _)).filterNot(content.references.contains)
      val continuation = Option.when(missing.nonEmpty)(Continuation(missing)).toList
      val core = Section(continuation ++ List(content) ++ result, offsets.map(MoveRef(path, _)))
      val detail = Some(Section(
        continuation ++ List(content) ++
          Option.when(material.events.nonEmpty)(MaterialEvents(path, material.events)).toList ++ result,
        offsets.map(MoveRef(path, _))))
      core -> detail
    }
    val alreadyCovered = lead.flatMap(_.covered).toSet
    // A selected capture owns its connected exchange in the same paragraph.
    // This follows certified immediate trades in either direction, never a
    // quiet intervening move or an unrelated later capture.
    def connectedCaptures(path: PreparedPath, material: LineMaterialSummary, seeds: List[Int]): Set[Int] =
      val trades = immediateTrades(material, path.moves)
      val connected = mutable.Set.empty[Int]
      var pending = seeds
      while pending.nonEmpty do
        val adjacent = pending.flatMap(offset => trades.getOrElse(offset, Nil))
          .filterNot(offset => connected(offset) || alreadyCovered(MoveRef(path, offset))).distinct
        connected ++= adjacent
        pending = adjacent
      connected.toSet
    val completedLead = lead.map(section => section.copy(statements = section.statements.map {
      case value: Material =>
        value.path.certified.flatMap(_._2.material).fold(value) { material =>
          val connected = connectedCaptures(value.path, material, value.cost.events.map(_.plyOffset))
          val events = (value.cost.events.map(_.plyOffset) ++ material.events
            .filter(event => connected(event.plyOffset)).map(_.plyOffset)).distinct.sorted
          CommentaryMaterialAccount.calculate(material, events, (value.cost.offsets ++ connected).distinct.sorted, value.path.moves.size)
            .fold(value)(cost => value.copy(cost = cost))
        }
      case value: MaterialEvents =>
        value.path.certified.flatMap(_._2.material).fold(value) { material =>
          val connected = connectedCaptures(value.path, material, value.events.map(_.plyOffset))
          value.copy(events = (value.events ++ material.events.filter(event => connected(event.plyOffset)))
            .distinct.sortBy(_.plyOffset))
        }
      case statement => statement
    }))
    val core = if completedLead.isEmpty then accounts.map(_._1) else
      val references = completedLead.flatMap(section => section.references ++ section.required)
      val actionReferences = completedLead.flatMap(section => section.actionReferences ++
        section.required.filterNot(section.references.contains))
      val covered = completedLead.flatMap(_.covered).toSet
      // Capture coverage and stated role changes are different duties. An
      // already selected promotion must not be re-added as an uncovered capture.
      val statedMaterial = covered ++ completedLead.flatMap(_.statements).collect {
        case value: MaterialEvents => value.references
      }.flatten
      val statedResults: Set[Statement] = completedLead.flatMap(_.statements).collect {
        case value: Terminal => value
        case value: RecordedEnd => value
      }.toSet
      // A selected comparison may name a tested child continuation. Its
      // captures and checks have the same closure duty as the two root PVs.
      // Merely available or detail-only branches do not enter this set.
      val witnessed = (pair ++ actionReferences.map(_.path)).distinct.filter(path =>
        paths.paths.exists(_ eq path) && path.certified.flatMap(_._2.material).nonEmpty)
      completedLead ++ witnessed.flatMap { path =>
        val material = path.certified.flatMap(_._2.material).get
        val checks = path.certified.toList.flatMap(_._2.lineEvents)
          .filter(_.kind == LineEventKind.Check).map(_.plyOffset).toSet
        val trades = immediateTrades(material, path.moves)
        val end = MoveRef(path, path.moves.size - 1)
        val result = (path.automaticTerminal.map(Terminal(end, _)).toList ++
          Option.when(path.automaticTerminal.isEmpty &&
            (path.moves.size == 1 || actionReferences.contains(end) && checks(end.offset)))(RecordedEnd(end)).toList)
          .filterNot(statedResults)
        val shown = actionReferences.filter(_.path eq path).map(_.offset).toSet
        val offsets = mutable.Set.from(shown ++ Option.when(pair.exists(_ eq path))(0) ++ result.flatMap(_.references.map(_.offset)))
        // Follow only obligations of displayed actual moves. A last-position
        // witness is not an interval containing every preceding exchange.
        var changed = true
        while changed do
          val replies = offsets.toList.flatMap(n => trades.getOrElse(n, Nil) ++
            Option.when(n + 1 < path.moves.size && checks(n))(n + 1)).filterNot(offsets)
          changed = replies.nonEmpty
          offsets ++= replies
        val events = material.events.filter(event => offsets(event.plyOffset) &&
          !statedMaterial(MoveRef(path, event.plyOffset)))
        val additions = Option.when(events.nonEmpty)(MaterialEvents(path, events)).toList ++ result
        val included = actionReferences.toSet ++ additions.flatMap(_.actionReferences)
        val missing = offsets.toList.sorted.map(MoveRef(path, _)).filterNot(included)
        val statements = Option.when(missing.nonEmpty)(Continuation(missing)).toList ++ additions
        Option.when(statements.nonEmpty)(Section(statements, statements.flatMap(_.references).distinct))
      }
    Some(Answer(question, core, details ++ accounts.flatMap(_._2),
      pair.flatMap(path => path.ids ++ path.certified.map(_._1.ref.id))))

  /** A quiet root vacates the square used by an ally's actual first evasion.
    * The alternative retains that original blocker and the same ally retreats
    * elsewhere after its own observed attack. Select this two-arm use before
    * unrelated histories; it establishes neither safety nor forced play.
    */
  def vacancyResponseAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position], selected: List[CommentaryAnswer.Section])
      : Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    val arms = prepare(packet, paths, pair, positions) match
      case Some(value) => value
      case _ => return None
    if arms.exists(_.material.events.exists(_.promotionGainCp > 0)) then return None
    val statements = selected.flatMap(_.statements).distinct
    def response(arm: Arm, target: Piece, offset: Int): Option[List[Statement]] = for
      attacker <- arm.evaded(offset, target)
      if arm.route(target).headOption.contains(offset) && arm.quiet(offset) && arm.covers(offset, attacker, target)
      incoming <- arm.at(attacker, offset)
      before <- arm.at(target, offset)
      after <- arm.at(target, offset + 1)
    yield List(Action(arm.ref(offset - 1), MoveWorkProjection.Detail.Attack(incoming, before, true)),
      Action(arm.ref(offset), MoveWorkProjection.Detail.Evades(after, incoming)))
    val candidates = statements.collect { case route: SquareReoccupation if route.move.offset == 0 => route }
      .flatMap { route => for
        open <- arms.find(_.path eq route.move.path).toList
        peer <- arms.filter(_.path ne open.path)
        if (route.use.path eq open.path) && open.root.contains(route.originalActor) &&
          route.originalActor != route.originalEntry && route.originalActor.side == route.originalEntry.side &&
          open.at(route.originalActor, 0).exists(_.square.key == route.square) &&
          open.at(route.originalActor, 1).contains(route.actor) &&
          open.at(route.originalEntry, route.use.offset + 1).contains(route.entry) && route.entry.square.key == route.square &&
          (1 to route.use.offset).forall(n => open.position(n).exists(_.empty(route.square)))
        use <- response(open, route.originalEntry, route.use.offset).toList
        alternative <- peer.route(route.originalEntry).headOption.toList if alternative > 0
        otherUse <- response(peer, route.originalEntry, alternative).toList
        blocker <- peer.at(route.originalActor, alternative + 1).toList
        if blocker.square.key == route.square && (0 to alternative + 1).forall(n =>
          peer.position(n).nonEmpty && peer.at(route.originalActor, n).contains(blocker)) &&
          peer.at(route.originalEntry, alternative + 1).exists(_.square.key != route.square)
      yield (route, open, peer, alternative, blocker, use, otherUse) }
    val chosen = candidates match
      case value :: Nil => value
      case _ => return None
    val (route, open, peer, alternative, blocker, use, otherUse) = chosen
    val selectedUse = route.use
    // Another exact root/use mechanism is still a competing question, even
    // if this quiet response is easier to express. Do not erase its price.
    if statements.exists(statement => conditionUses(statement).exists((condition, action) =>
        condition.offset == 0 && pair.exists(_ eq condition.path) && action != selectedUse)) then return None
    val core = pair.map { path =>
      val content: List[Statement] = if path eq open.path then route :: use else
        Continuation(List(peer.ref(0))) :: (otherUse :+
          PiecePresence(peer.ref(alternative), route.originalActor, blocker))
      val refs = content.flatMap(_.references).toSet
      Section(content, content.flatMap(_.references).distinct,
        selected.flatMap(_.focus).filter(f => refs(f.move)).distinctBy(_.move))
    }
    val details = remainingDetails(selected, core)
    completePairAnswer(packet, paths, pair, "observed_route_use", core, details)
      .map(answer => answer.copy(evidence = (answer.evidence ++ arms.flatMap(_.proofs)).distinct))

  /** Both roots use the same physical provider for the same supported piece.
    * A different captured role or a verified root obstruction supplies the
    * comparison. Keep that root condition and its actual use
    * together; independent histories remain available in detail. Another
    * selected root/use mechanism or actual attack cannot be displaced here.
    */
  def pairedSupportAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position], selected: List[CommentaryAnswer.Section])
      : Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    val arms = prepare(packet, paths, pair, positions) match
      case Some(value) => value
      case _ => return None
    if arms.exists(_.material.events.exists(_.promotionGainCp > 0)) then return None
    val statements = selected.flatMap(_.statements).distinct
    val uses = statements.collect { case use: SupportedCapture if use.move.offset == 0 => use }.filter { use =>
      arms.exists(arm => (arm.path eq use.move.path) && arm.root == use.use.recapture.originalActor &&
        arm.root.exists(provider => LineContinuationInvestigation.directSupportUse(packet, arm.path, paths,
          use.use.recapture.plyOffset + 1, provider, use.use.originalTarget).uses.contains(use.use)))
    }
    val candidates = uses.groupBy(use => use.use.recapture.originalActor -> use.use.originalTarget).values.filter(group =>
      group.size == 2 && pair.forall(path => group.count(_.move.path eq path) == 1)).toList
    val chosen = candidates match
      case group :: Nil => group
      case _ => return None
    val active = chosen.flatMap(_.references).toSet
    if statements.exists(statement => conditionUses(statement).exists((condition, _) =>
        condition.offset == 0 && pair.exists(_ eq condition.path)) && !chosen.contains(statement)) then return None
    val otherAttacks = statements.collect {
      case Action(move, MoveWorkProjection.Detail.Attack(actor, target, _)) if move.offset == 0 => (move, actor, target)
    }.exists { (move, actor, target) =>
      arms.filter(_.path eq move.path).exists { arm =>
        val originalActor = arm.originals.find(arm.at(_, 1).contains(actor))
        val originalTarget = arm.originals.find(arm.at(_, 1).contains(target))
        arm.captures.exists(event => event.plyOffset > 0 && event.originalActor == originalActor &&
          event.originalVictim == originalTarget && !active(arm.ref(event.plyOffset)))
      }
    }
    if otherAttacks then return None
    val contrasts = statements.collect { case value: ControlContrast if value.move.offset == 0 &&
      value.alternative.offset == 0 && value.present != value.alternativePresent => value }.flatMap { contrast =>
      val (absent, actor, target, present, otherActor, otherTarget) = if !contrast.present then
        (contrast.move, contrast.actor, contrast.target, contrast.alternative, contrast.alternativeActor, contrast.alternativeTarget)
      else (contrast.alternative, contrast.alternativeActor, contrast.alternativeTarget,
        contrast.move, contrast.actor, contrast.target)
      for
        blocked <- arms.find(_.path eq absent.path).toList
        open <- arms.find(_.path eq present.path).toList if blocked ne open
        use <- chosen.find(_.move.path eq blocked.path).toList
        otherUse <- chosen.find(_.move.path eq open.path).toList
        originalActor <- blocked.originals.find(blocked.at(_, 1).contains(actor)).toList
        originalTarget <- target.toList.flatMap(t => blocked.originals.find(blocked.at(_, 1).contains(t)))
        if actor.side == use.provider.side && target.exists(_.side == actor.side) &&
          open.at(originalActor, 1).contains(otherActor) && open.at(originalTarget, 1) == otherTarget &&
          blocked.covers(0, originalActor, originalTarget) &&
          (1 to use.use.recapture.plyOffset).forall(blocked.control(_, originalActor, originalTarget).contains(false)) &&
          blocked.covers(use.use.recapture.plyOffset + 1, originalActor, originalTarget) &&
          (1 to otherUse.use.recapture.plyOffset + 1).forall(open.covers(_, originalActor, originalTarget))
        position <- blocked.position(1).toList
        ray <- target.toList.flatMap(position.blockedRay(actor, _)) if ray._2 == List(use.provider)
        restoredActor <- blocked.at(originalActor, use.use.recapture.plyOffset + 1).toList
        restoredTarget <- blocked.at(originalTarget, use.use.recapture.plyOffset + 1).toList
      yield
        blocked.proofs ++= position.sourceRefs(List(ray._1))
        contrast -> ControlState(blocked.ref(use.use.recapture.plyOffset), restoredActor,
          Some(restoredTarget), restoredTarget.square.key, present = true)
    }
    if chosen.map(_.use.originalCapturer.role).distinct.size != 2 && contrasts.isEmpty then return None
    val related = statements.collect {
      case action @ Action(move, MoveWorkProjection.Detail.Support(provider, target, _, _)) if chosen.exists(use =>
        use.move == move && use.provider == provider && use.target == target) => action
    }
    val core = pair.map { path =>
      val use = chosen.find(_.move.path eq path).get
      val comparison: List[Statement] = Option.when(path eq pair.head)(contrasts.map(_._1)).toList.flatten
      val restoration = contrasts.map(_._2).filter(_.move.path eq path)
      val content: List[Statement] = comparison ++ related.filter(_.move.path eq path) ++ List(use) ++ restoration
      val refs = content.flatMap(_.references).toSet
      Section(content, selected.flatMap(_.required).filter(refs).distinct,
        selected.flatMap(_.focus).filter(f => refs(f.move)).distinctBy(_.move))
    }
    val details = remainingDetails(selected, core)
    completePairAnswer(packet, paths, pair, "paired_support_use", core, details)
      .map(answer => answer.copy(evidence = (answer.evidence ++ arms.flatMap(_.proofs)).distinct))

  /** The actual root capture supplies a physical subject. Compare that subject
    * in both provided arms before language generation; unrelated later history
    * is not a price seed and a sparse account cannot stand in for a whole price.
    */
  def captureTargetAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position], selected: List[CommentaryAnswer.Section])
      : Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    val arms = prepare(packet, paths, pair, positions) match
      case Some(value) => value
      case _ => return None
    if arms.exists(_.material.events.exists(_.promotionGainCp > 0)) then return None
    val targets = arms.flatMap(_.captures.filter(_.plyOffset == 0).flatMap(_.originalVictim)).distinct
    val target = targets match
      case value :: Nil => value
      case _ => return None
    val built = arms.map { arm =>
      val loss = arm.captures.find(_.originalVictim.contains(target))
      val present = paths.pieceAfter(packet, arm.path, target, arm.path.moves.size)
      val gains = arm.captures.filter(event => event.originalActor.contains(target) &&
        event.plyOffset < loss.fold(arm.path.moves.size)(_.plyOffset))
      val accounts = (loss.toList ++ gains).distinct.map(arm.targetAccount(_, target))
      val events = accounts.flatMap(_.events).distinct.sortBy(_.plyOffset)
      val offsets = (List(0) ++ accounts.flatMap(_.offsets)).distinct.sorted
      val cost = Option.when(events.nonEmpty)(CommentaryMaterialAccount.calculate(arm.material,
        events.map(_.plyOffset), offsets, arm.path.moves.size)).flatten
      val knownOutcome = loss.exists(event => paths.pieceAfter(packet, arm.path, target,
        event.plyOffset + 1).contains(None)) || loss.isEmpty && present.exists(_.nonEmpty)
      Option.when(knownOutcome && accounts.forall(account => account.complete && account.dutyCosts.isEmpty) &&
        (events.isEmpty || cost.nonEmpty)) {
        val retained = Option.when(loss.isEmpty)(present.flatten).flatten.map(current =>
          PiecePresence(arm.ref(arm.path.moves.size - 1), target, current))
        val departure = Option.when(loss.isEmpty)(arm.route(target).find(n => arm.mover(n).contains(target)))
          .flatten.map(arm.ref)
        val outcome = TargetOutcome(arm.ref(0), target, loss.map(event => arm.pieceExchange(List(event))),
          departure, retained)
        // Keep compound prices in the common flat material representation.
        // A sole target loss is already conveyed by its exact removal statement.
        val price = cost.filterNot(value => value.events.map(_.plyOffset).toSet ==
          outcome.covered.map(_.offset).toSet && value.offsets.forall(n => n == 0 || outcome.references.exists(_.offset == n)))
          .map(Material(arm.path, _)).toList
        val content: List[Statement] = outcome :: price
        val result = loss.nonEmpty -> cost.map(value =>
          (value.commonExchanges, value.whiteAdditionalLosses, value.blackAdditionalLosses))
        Section(content, content.flatMap(_.references).distinct) -> result
      }
    }
    // Equal target outcomes and equal prices do not explain this choice. Their
    // timing alone must not displace a different, already selected question.
    if !built.forall(_.nonEmpty) || built.flatten.map(_._2).distinct.size != 2 then None
    else
      val selectedCore = built.flatten.map(_._1)
      val active = selectedCore.flatMap(_.actionReferences).toSet
      val proof = selectedCore.flatMap(_.references).toSet
      val covered = selectedCore.flatMap(_.covered).toSet
      val mechanisms = selected.flatMap(_.statements).distinct.filter(statement =>
        conditionUses(statement).exists((condition, use) => condition.offset == 0 &&
          pair.exists(_ eq condition.path) && use.offset > 0))
      val mechanismPrices = mechanisms.flatMap(conditionUses).map(_._2).distinct.flatMap { use =>
        arms.filter(_.path eq use.path).flatMap { arm =>
          arm.captures.filter(_.plyOffset == use.offset).flatMap { capture =>
            capture.originalActor.map(actor => arm -> arm.account(capture, actor))
          }
        }
      }
      // A different closed root/use question has not become less important
      // merely because a target outcome is also available. Preserve the old
      // scope unless the new account actually owns the same use and its proof.
      if mechanisms.exists(statement => !conditionUses(statement).forall((_, use) => active(use)) ||
          !statement.references.forall(proof) || !statement.covered.forall(covered)) ||
          mechanismPrices.exists((arm, price) => !price.complete || price.dutyCosts.nonEmpty ||
            !price.events.forall(event => covered(arm.ref(event.plyOffset)))) then return None
      val core = selectedCore.map { section =>
        val related = mechanisms.filter(statement => conditionUses(statement).exists((_, use) =>
          section.actionReferences.contains(use)))
        val enriched = section.copy(statements = (related ++ section.statements).distinct)
        val refs = enriched.references.toSet
        enriched.copy(required = (enriched.required ++ selected.flatMap(_.required).filter(refs)).distinct,
          focus = selected.flatMap(_.focus).filter(f => refs(f.move)).distinctBy(_.move))
      }
      val coreRefs = core.flatMap(_.references).toSet
      val details = selected.map { section =>
        val remaining = section.statements.filterNot {
          case value: MaterialEvents => value.covered.nonEmpty && value.covered.forall(covered) &&
            section.required.forall(coreRefs)
          case value: Material => value.covered.nonEmpty && value.covered.forall(covered) &&
            section.required.forall(coreRefs)
          case _ => false
        }
        val refs = remaining.flatMap(_.references).toSet
        Section(remaining, section.required.filter(refs), section.focus.filter(f => refs(f.move)))
      }.filter(_.statements.nonEmpty)
      completePairAnswer(packet, paths, pair, "observed_capture_target", core, details)
        .map(answer => answer.copy(evidence = (answer.evidence ++ arms.flatMap(_.proofs)).distinct))

  def selectedAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position], selected: List[(PreparedPath, MoveWorkProjection.LocalCandidate)])
      : Option[CommentaryAnswer.Answer] =
    val arms = prepare(packet, paths, pair, positions) match
      case Some(value) if value.forall(a => a.root.nonEmpty && !a.material.events.exists(_.promotionGainCp > 0)) => value
      case _ => return None
    case class Contact(arm: Arm, offset: Int, actor: Piece, target: Piece, note: MoveWorkProjection.LocalCandidate)
    val contacts = selected.flatMap { (path, note) =>
      for
        arm <- arms.find(_.path eq path).toList
        offset = path.ids.indexOf(note.edge.evidence.id)
        if offset >= 0
        (actor, target) <- (note.note.detail match
          case MoveWorkProjection.Detail.Attack(a, t, _) => List(a -> t)
          case _ => Nil)
        originalActor <- arm.originals.find(p => arm.at(p, offset + 1).contains(actor)).toList
        originalTarget <- arm.originals.find(p => arm.at(p, offset + 1).contains(target)).toList
      yield Contact(arm, offset, originalActor, originalTarget, note)
    }
    val answers = contacts.flatMap { contact =>
      contacts.find(peer => (peer.arm ne contact.arm) && peer.target == contact.target &&
        (peer.actor == contact.actor || (contact.offset == 0 && peer.offset == 0 &&
          contact.arm.root.contains(contact.actor) && peer.arm.root.contains(peer.actor))))
        .map(peer => arms.map(a => if a eq contact.arm then contact else peer))
    }.distinct
    answers.iterator.flatMap { answer =>
      // A later capture alone does not make an unrelated common target the
      // root's question. Both root movers may attack the same physical target.
      val rooted = answer.exists(c => c.arm.root.contains(c.actor) || c.arm.root.contains(c.target))
      if !rooted then None else
        val built = answer.map { contact =>
          val a = contact.arm
          val actor = contact.actor
          val target = contact.target
          val start = contact.offset
          val beforeTarget = a.at(target, start + 1).get
          val escape = a.route(target).find(n => n > start && a.control(n, actor, target).contains(true) &&
            a.control(n + 1, actor, target).contains(false))
          val loss = a.captures.find(e => e.plyOffset > start && e.originalVictim.contains(target))
          val removal = a.captures.find(e => e.plyOffset > start && e.originalVictim.contains(actor))
          // A displayed capture owns its actual recovery/counterplay. The root
          // mover's gain or loss cannot disappear behind another piece's story.
          val seeds = (loss.toList ++ removal.toList ++ a.captures.filter(e =>
            e.plyOffset == 0 || e.plyOffset == start || e.originalVictim == a.root)).distinct
          val accounts = seeds.map(e => a.account(e,
            if e.originalVictim == a.root || e.plyOffset == 0 then a.root.get else target))
          val events = accounts.flatMap(_.events).distinct.sortBy(_.plyOffset)
          val required = (accounts.flatMap(_.offsets) ++ List(0, start) ++ escape.toList).distinct.sorted
          val checks = required.filter(a.checks)
          val offsets = (required ++ checks.map(_ + 1)).distinct.sorted
          val explainedEnd = offsets.last
          val quietReply = escape.filterNot(n => events.exists(_.plyOffset == n))
          val unseenCheck = (0 to explainedEnd).filter(a.checks).filterNot(offsets.contains)
          val knownEnd = a.at(target, a.path.moves.size)
          val closed = accounts.forall(c => c.complete && c.dutyCosts.isEmpty) && unseenCheck.isEmpty &&
            offsets.forall(_ < a.path.moves.size) && (loss.nonEmpty || escape.nonEmpty || removal.nonEmpty) &&
            // A later return to the same attack needs its own episode wording.
            escape.forall(n => (n + 1 to a.path.moves.size).forall(count =>
              a.at(actor, count).isEmpty || a.at(target, count).isEmpty || a.control(count, actor, target).contains(false)))
          val costOffsets = offsets.filterNot(n => (n == 0 || n == start || quietReply.contains(n)) &&
             !events.exists(_.plyOffset == n) && !checks.contains(n) && !checks.contains(n - 1))
          val cost = Option.when(events.nonEmpty)(CommentaryMaterialAccount.calculate(a.material,
            events.map(_.plyOffset), costOffsets, a.path.moves.size)).flatten
          Option.when(closed && (events.isEmpty || cost.nonEmpty)) {
            val end = a.ref(a.path.moves.size - 1)
            // Survival answers the target question. Its quiet final coordinate
            // is a proof/detail, not another move the basic answer must explain.
            val survivalWitness = Option.when(loss.isEmpty && knownEnd.nonEmpty && escape.nonEmpty &&
              !a.material.events.exists(_.plyOffset == end.offset) && !a.checks(end.offset) &&
              !offsets.contains(end.offset) &&
              knownEnd.get.role == beforeTarget.role)(
              CommentaryAnswer.PiecePresence(end, target, knownEnd.get))
            val content = CommentaryAnswer.TargetResponse(a.path, start, beforeTarget, actor, target, quietReply,
              cost.map(CommentaryAnswer.Material(a.path, _)), loss.isEmpty && knownEnd.nonEmpty && escape.nonEmpty,
              targetCapture = loss.map(event => a.pieceExchange(List(event))),
              attackerRemoval = removal.map(event => a.pieceExchange(List(event))),
              targetEnd = Option.when(survivalWitness.isEmpty)(knownEnd)
                .flatten.map(current => CommentaryAnswer.PiecePresence(end, target, current)))
            // The exact end position proves survival. Only an already selected
            // final check needs a recorded boundary; unrelated end captures do
            // not become part of this target question.
            val ending: List[CommentaryAnswer.Statement] =
              Option.when(offsets.contains(end.offset) && a.checks(end.offset))(
                CommentaryAnswer.RecordedEnd(end)).toList
            CommentaryAnswer.Section(content :: ending, (offsets.map(a.ref) ++ content.references).distinct,
              Option.when(contact.note.selection == CommentarySelection.Display.Selected)(
                CommentaryAnswer.Focus(a.ref(start), contact.note)).toList) ->
              survivalWitness.map(presence => CommentaryAnswer.Section(List(presence))).toList
          }
        }
        Option.when(built.forall(_.nonEmpty))(CommentaryAnswer.Answer("selected_target_response",
          built.flatten.map(_._1), built.flatten.flatMap(_._2),
          arms.flatMap(_.proofs.toList.sorted) ++ answer.flatMap(_.note.evidence)))
    }.nextOption()

  /** Realization keeps the selected content and its complete proof. Not every
    * proof move must be spoken; displayed moves still own their actual costs.
      */
  def realize(paths: PreparedPaths, pair: List[PreparedPath], answer: CommentaryAnswer.Answer): Option[Result] =
    realizeComplete(paths, pair, answer).orElse {
      // The basic account still has to prove every selected action, capture,
      // check response and required condition. Optional old detail may exceed
      // its own display budget without vetoing that complete basic answer.
      Option.when(answer.details.nonEmpty)(answer.copy(details = Nil))
        .flatMap(realizeComplete(paths, pair, _))
    }

  private def realizeComplete(paths: PreparedPaths, pair: List[PreparedPath], answer: CommentaryAnswer.Answer): Option[Result] =
    def validContent(statement: CommentaryAnswer.Statement): Boolean = statement match
      case CommentaryAnswer.PreparedIdea(value) => value.belongsTo(paths) && value.pair.toSet == pair.toSet
      case CommentaryAnswer.PreparedReply(value) => value.owner.belongsTo(paths) && value.owner.pair.toSet == pair.toSet &&
        (pair.exists(_ eq value.path) ||
          (value.owner.choices.toList ++ value.owner.replies.toList).exists(_.pathsByMove.values.exists(_ eq value.path)))
      case CommentaryAnswer.ReferencePurpose(value) => value.belongsTo(paths) && value.arms.map(_.path).toSet == pair.toSet &&
        value.conditionalPurposes.forall(option => FunctionalReplyReading.valid(paths, option) &&
          FunctionalReplyReading.relevant(paths, pair, option) && answer.core.exists(_.statements.contains(option)))
      case CommentaryAnswer.ActivityUse(value, shared) => value.belongsTo(paths) &&
        pair.exists(path => CommentarySequenceText.sameRoot(path, value.path)) && shared.forall(other =>
          other.belongsTo(paths) && value.sameActivity(other) && answer.core.flatMap(_.statements).exists {
            case CommentaryAnswer.ActivityUse(previous, _) => previous eq other
            case _ => false
          })
      case CommentaryAnswer.ReferencePosition(value, arm) => value.belongsTo(paths) && value.arms.contains(arm) &&
        value.arms.map(_.path).toSet == pair.toSet && arm.counterplay.forall(counter =>
          counter.activity.belongsTo(paths) && (counter.activity.path eq arm.path) &&
            counter.control.forall(_.path eq arm.path))
      case CommentaryAnswer.ReferenceContrast(value) => value.belongsTo(paths) && value.contrasts.nonEmpty &&
        value.arms.map(_.path).toSet == pair.toSet
      case CommentaryAnswer.ReferenceExchangeReply(comparison, value, branch) => comparison.belongsTo(paths) &&
        comparison.arms.map(_.path).toSet == pair.toSet && value.belongsTo(paths) && value.branches.contains(branch) &&
        value.question.activity.nonEmpty && branch.activity.nonEmpty &&
        value.question.rootMove == comparison.played.path.moves.head &&
        value.question.path.positions.head == comparison.played.path.positions.head &&
        branch.path.moves(1) != comparison.played.path.moves(1) &&
        branch.activity.exists(_.mover.nonEmpty != comparison.played.retainedMover.nonEmpty)
      case CommentaryAnswer.FunctionPurpose(value) => value.belongsTo(paths)
      case CommentaryAnswer.FunctionReply(value, branch) => value.belongsTo(paths) && value.branches.contains(branch)
      case CommentaryAnswer.ChoicePurpose(value) => value.belongsTo(paths) &&
        (answer.question != "move_purpose" || value.arms.map(_.path.moves.head).toSet == pair.map(_.moves.head).toSet)
      case CommentaryAnswer.ChoiceReply(value, arm) => value.belongsTo(paths) && value.arms.contains(arm)
      case CommentaryAnswer.ChoiceNeutralization(value) => value.owner.belongsTo(paths) &&
        PurposeChoice.neutralization(value.owner, List(value.reply), paths).contains(value)
      case CommentaryAnswer.ExchangeFunction(value) => value.belongsTo(paths)
      case CommentaryAnswer.RetainedExchangeFunction(value) => value.belongsTo(paths) && value.kept.nonEmpty
      case value: CommentaryAnswer.LegalReplyOption => FunctionalReplyReading.valid(paths, value)
      case value: CommentaryAnswer.FunctionRealization => FunctionalRealizationReading.valid(paths, value.value)
      case value: CommentaryAnswer.ReserveBreak => RootSubjectComparison.validReserveBreak(paths, value)
      case value: CommentaryAnswer.ReserveHold => RootSubjectComparison.validReserveHold(paths, value)
      case value: CommentaryAnswer.ReserveTempo => RootSubjectComparison.validReserveTempo(paths, value)
      case value: CommentaryAnswer.ReserveEntry => RootSubjectComparison.validReserveEntry(paths, value)
      case value: CommentaryAnswer.DirectReserveEntry => RootSubjectComparison.validDirectReserveEntry(paths, value)
      case value: CommentaryAnswer.ReplyConstraint => RootSubjectComparison.validReplyConstraint(paths, value)
      case value: CommentaryAnswer.TargetDefense => value.move.offset == 0 &&
        paths.positionView(value.move.path, 1).exists { view =>
          val position = new Position(view)
          position.complete(RelationFactKind.GeometricControl) && position.contains(value.target) &&
            value.attackers.nonEmpty &&
            position.controllers(value.target.square.key, !value.target.side) == value.attackers &&
            position.controllers(value.target.square.key, value.target.side) == value.defenders
        }
      case value: CommentaryAnswer.ComparisonSubject => value.roots.size == 2 &&
        value.roots.map(_.path).distinct.size == 2 && value.roots.forall(move => move.offset == 0 &&
          value.originals.nonEmpty && value.originals.distinct.size == value.originals.size &&
          value.originals.forall(piece => paths.trackedPieceAfter(move.path, piece, 0).contains(Some(piece))))
      case value: CommentaryAnswer.Material => value.path.certified.flatMap(_._2.material).exists(material =>
        CommentaryMaterialAccount.calculate(material, value.cost.events.map(_.plyOffset),
          value.cost.offsets, value.path.moves.size).contains(value.cost))
      case value: CommentaryAnswer.MaterialBalance => value.path.moves.size >= 2 &&
        value.path.certified.flatMap(_._2.material).exists(material => value.cost match
          case None => material.events.isEmpty && value.through.isEmpty
          case Some(cost) => value.through.fold(material.events.toSet == cost.events.toSet)(end =>
            cost.events.nonEmpty && cost.events.map(_.plyOffset).min == 0 &&
              cost.events.map(_.plyOffset).max == end && cost.offsets.forall(_ <= end) &&
              material.events.filter(_.plyOffset <= end).toSet == cost.events.toSet &&
              answer.core.flatMap(_.covered).filter(_.path eq value.path).forall(move =>
                cost.events.exists(_.plyOffset == move.offset))) &&
            validContent(CommentaryAnswer.Material(value.path, cost)) && cost.events.forall(event =>
              answer.core.flatMap(_.covered).contains(MoveRef(value.path, event.plyOffset))))
      // An empty event group contributes no price receipt. Other selected
      // statements and obligations still have to survive realization.
      case value: CommentaryAnswer.MaterialEvents =>
        value.events.distinct.size == value.events.size && value.path.certified.flatMap(_._2.material)
          .exists(material => value.events.forall(material.events.contains))
      case value: CommentaryAnswer.PieceExchange => value.events.nonEmpty &&
        paths.continuationReadings.exists(reading => (reading.path eq value.path) && reading.complete &&
          value.events.forall(reading.events.contains))
      case value: CommentaryAnswer.TargetResponse => value.material.forall(validContent) &&
        value.targetCapture.forall(validContent) && value.attackerRemoval.forall(validContent) &&
        value.targetEnd.forall(validContent)
      case value: CommentaryAnswer.TargetOutcome => value.root.offset == 0 &&
        value.removal.nonEmpty != value.retained.nonEmpty &&
        value.removal.forall(removed => (removed.path eq value.root.path) &&
          removed.events.size == 1 && removed.events.head.originalVictim.contains(value.original) && validContent(removed) &&
          paths.trackedPieceAfter(removed.path, value.original, removed.events.head.plyOffset + 1).contains(None)) &&
        value.retained.forall(presence => (presence.after.path eq value.root.path) &&
          presence.original == value.original && presence.after.offset == value.root.path.moves.size - 1 &&
          paths.trackedPieceAfter(presence.after.path, value.original,
            presence.after.offset + 1).contains(Some(presence.current))) &&
        value.departure.forall(move => (move.path eq value.root.path) && value.retained.nonEmpty &&
          (for
            before <- paths.trackedPieceAfter(move.path, value.original, move.offset).flatten
            after <- paths.trackedPieceAfter(move.path, value.original, move.offset + 1).flatten
            uci <- move.path.moves.lift(move.offset)
          yield before != after && before.square.key == uci.take(2) && after.square.key == uci.slice(2, 4)).contains(true))
      case CommentaryAnswer.NoMaterialChange(root, end) => (root.path eq end.path) &&
        root.path.certified.flatMap(_._2.material).exists(_.events.isEmpty)
      case value: CommentaryAnswer.PiecePresence => paths.trackedPieceAfter(value.after.path,
        value.original, value.after.offset + 1).contains(Some(value.current))
      case value: CommentaryAnswer.PieceAbsence => paths.trackedPieceAfter(value.after.path,
        value.original, value.after.offset + 1).contains(None)
      case CommentaryAnswer.Terminal(move, result) => move.offset == move.path.moves.size - 1 &&
        move.path.automaticTerminal.contains(result)
      case _ => true
    if !(answer.core ++ answer.details).flatMap(_.statements).forall(validContent) then return None
    def targetResponseSection(section: CommentaryAnswer.Section): Boolean = section.statements match
      case (_: CommentaryAnswer.TargetResponse) :: tail => tail.forall {
        case CommentaryAnswer.MaterialEvents(_, events) => events.forall(event =>
          event.capture.nonEmpty && event.promotionGainCp == 0)
        case _: CommentaryAnswer.RecordedEnd => true
        case _ => false
      }
      case _ => false
    def referenceSection(section: CommentaryAnswer.Section): Boolean =
      section.statements.exists {
        case _: CommentaryAnswer.ReferencePurpose | _: CommentaryAnswer.ReferencePosition |
            _: CommentaryAnswer.ReferenceContrast | _: CommentaryAnswer.ReferenceExchangeReply |
            _: CommentaryAnswer.FirstReplyChoice | _: CommentaryAnswer.PreparedIdea | _: CommentaryAnswer.PreparedReply |
            _: CommentaryAnswer.FunctionReply | _: CommentaryAnswer.FunctionPurpose | _: CommentaryAnswer.ActivityUse |
            _: CommentaryAnswer.LegalReplyOption => true
        case _ => false
      } && section.statements.forall {
        case _: CommentaryAnswer.ReferencePurpose | _: CommentaryAnswer.ReferencePosition |
            _: CommentaryAnswer.ReferenceContrast | _: CommentaryAnswer.ReferenceExchangeReply | _: CommentaryAnswer.Continuation |
            _: CommentaryAnswer.Material | _: CommentaryAnswer.RecordedEnd | _: CommentaryAnswer.CheckSequence => true
        case value => ReferencePurposeText.condition(value).nonEmpty
      }
    def korean(section: CommentaryAnswer.Section, announced: Set[CommentaryAnswer.Statement]): List[String | MoveRef] = section.statements match
      case (value: CommentaryAnswer.LegalReplyOption) :: Nil => ReferencePurposeText.legalOption(value, ko = true)
      case _ if referenceSection(section) =>
        var preceding: Option[PreparedPath] = None
        val grouped = ReferencePurposeText.groupedPurpose(section, ko = true)
        val aftermath = ReferencePurposeText.groupedPosition(section, ko = true,
          announced.collect { case CommentaryAnswer.ReferencePurpose(reading) => reading })
        val prepared = MoveIntent.groupedReplies(section, ko = true, announced.flatMap {
          case CommentaryAnswer.ReferencePurpose(reading) => reading.preparation
          case CommentaryAnswer.PreparedIdea(reading) => Some(reading)
          case _ => None
        })
        val sharedCosts = ReferencePurposeText.groupedCosts(section, ko = true)
        val pursuits = section.statements.collect { case action: CommentaryAnswer.Action => action }.flatMap(action =>
          CommentarySequenceText.pursuit(section.statements, action, ko = true).map(action -> _)).toMap
        val relations = section.statements.collect { case action: CommentaryAnswer.Action => action }.flatMap(action =>
          CommentarySequenceText.relations(section.statements, action, ko = true).filter(group =>
            !group.expressed.collect { case peer: CommentaryAnswer.Action => peer }.exists(pursuits.contains))
            .map(action -> _)).toMap
        val evasions = section.statements.collect { case action: CommentaryAnswer.Action => action }.flatMap(action =>
          CommentarySequenceText.evasions(section.statements, action, ko = true).filter(group =>
            !group.expressed.exists(peer => pursuits.values.exists(_.expressed.contains(peer))))
            .map(action -> _)).toMap
        val replies = (pursuits.values ++ relations.values ++ evasions.values).flatMap(_.expressed.drop(1)).toSet
        val joined = prepared.toList ++ sharedCosts.toList
        section.statements.zipWithIndex.filterNot((statement, _) => replies(statement) ||
          joined.exists(group => group.expressed.drop(1).contains(statement)) ||
          grouped.exists(g => g.expressed.contains(statement) && !statement.isInstanceOf[CommentaryAnswer.ReferencePurpose]) ||
          aftermath.exists(g => g.expressed.contains(statement) && !statement.isInstanceOf[CommentaryAnswer.ReferencePosition]))
          .flatMap { (statement, index) =>
          val words: List[String | MoveRef] = statement match
            case action: CommentaryAnswer.Action if pursuits.contains(action) => pursuits(action).parts
            case action: CommentaryAnswer.Action if relations.contains(action) => relations(action).parts
            case action: CommentaryAnswer.Action if evasions.contains(action) => evasions(action).parts
            case _: CommentaryAnswer.ReferencePurpose if grouped.nonEmpty => grouped.get.parts
            case CommentaryAnswer.ReferencePurpose(value) => ReferencePurposeText.purpose(value, ko = true)
            case value: CommentaryAnswer.ActivityUse => ReferencePurposeText.condition(value).get
            case CommentaryAnswer.FunctionReply(value, branch) if value.question.activity.isEmpty =>
              ReferencePurposeText.functionalReply(value, branch, ko = true,
                announced.collect { case CommentaryAnswer.ReferencePurpose(reading) => reading },
                announced.collect { case CommentaryAnswer.FunctionPurpose(reading) => reading })
            case _: CommentaryAnswer.ReferencePosition if aftermath.nonEmpty => aftermath.get.parts
            case _: CommentaryAnswer.PreparedReply if prepared.nonEmpty => prepared.get.parts
            case _: CommentaryAnswer.Material if sharedCosts.nonEmpty => sharedCosts.get.parts
            case value: CommentaryAnswer.Material if prepared.exists(_.expressed.contains(value)) => Nil
            case CommentaryAnswer.ReferencePosition(value, arm) => ReferencePurposeText.position(value, arm, ko = true)
            case CommentaryAnswer.ReferenceContrast(value) => ReferencePurposeText.contrast(value, ko = true)
            case value: CommentaryAnswer.ReferenceExchangeReply => ReferencePurposeText.exchangeReply(value, ko = true)
            case value: CommentaryAnswer.CheckSequence => CommentarySequenceText.checkSequence(value, ko = true)
            case CommentaryAnswer.Continuation(moves) =>
              List("제시된 수순은 ") ++ moves.zipWithIndex.flatMap((move, i) =>
                (if i == 0 then Nil else List(if moves(i - 1).offset + 1 == move.offset then " " else " … ")) :+ move) ++ List("입니다.")
            case value: CommentaryAnswer.Material => renderMaterialKorean(value, brief = true)
            case value if ReferencePurposeText.condition(value).nonEmpty => ReferencePurposeText.condition(value).get
            case CommentaryAnswer.RecordedEnd(move) =>
              List("기록된 수순은 ", move, " 이후에 끝나며, 그다음 응수는 기록되어 있지 않습니다.")
            case _ => Nil
          val refs = words.collect { case ref: MoveRef => ref }
          val owner = CommentarySequenceText.rootOwner(refs)
          val header = owner.filter(path => !preceding.exists(CommentarySequenceText.sameRoot(_, path)) && !refs.exists(_.offset == 0))
            .toList.flatMap(path => List(MoveRef(path, 0), " 수순에서: "))
          preceding = owner
          (if index == 0 then Nil else List(" ")) ++ header ++ words
        }
      case (value: CommentaryAnswer.TargetResponse) :: tail if targetResponseSection(section) =>
        val lead: List[String | MoveRef] = if value.start == 0 then List(MoveRef(value.path, 0))
          else if value.start == 2 then List(MoveRef(value.path, 0), " ", MoveRef(value.path, 1), " 이후 ", MoveRef(value.path, 2))
          else List(MoveRef(value.path, 0), " 수순의 ", MoveRef(value.path, value.start))
        def original(piece: Piece): String =
          s"원래 ${piece.square.key}에 있던 ${if piece.side.white then "백" else "흑"} "
        val outcomes: List[String | MoveRef] =
          (value.targetCapture.toList ++ value.attackerRemoval.toList).sortBy(_.events.head.plyOffset)
          .flatMap(outcome => outcome.events.flatMap { event =>
            val actor = event.originalActor.fold("움직인 기물이")(piece => original(piece) + subject(piece))
            val victim = event.originalVictim.fold("상대 기물을")(piece => original(piece) + obj(piece))
            List(" ", MoveRef(outcome.path, event.plyOffset), s" 수로 $actor $victim " +
              (if outcome.recaptureOffsets(event.plyOffset) then "되잡습니다." else "잡습니다."))
          })
        lead ++ List(s" 수로 ${value.target.square.key} ${obj(value.target)} 공격합니다.") ++
          value.escape.toList.flatMap(n => List(" ", MoveRef(value.path, n), s" 수로 그 ${subject(value.target)} 공격을 피합니다.")) ++
          outcomes ++
          value.material.toList.flatMap(material => List(" ") ++ renderMaterialKorean(material)) ++
          value.targetEnd.toList.flatMap(presence => List(" ", presence.after,
            s" 이후 ${original(presence.original)}${subject(presence.original)} ${presence.current.square.key}에 남아 있습니다.")) ++
          Option.when(value.survives && value.targetEnd.isEmpty)(s" 그 ${subject(value.target)} 제시된 수순 끝까지 남습니다.").toList ++
          tail.flatMap {
            case CommentaryAnswer.MaterialEvents(path, events) => events.flatMap { event =>
              event.capture.toList.flatMap(capture => List(" ", MoveRef(path, event.plyOffset),
                s" 수로 ${if capture.side.white then "백" else "흑"} ${names(capture.attackerRole.name)}" +
                  (if capture.attackerRole.name == "knight" then "가 " else "이 ") +
                  s"${capture.square.key}의 ${if capture.side.white then "흑" else "백"} ${names(capture.capturedRole.name)}" +
                  (if capture.capturedRole.name == "knight" then "를 " else "을 ") +
                  (if capture.recapture then "되잡습니다." else "잡습니다.")))
            }
            case CommentaryAnswer.RecordedEnd(move) =>
              List(" 기록된 수순은 ", move, " 이후에 끝나며, 그다음 응수는 기록되어 있지 않습니다.")
            case _ => Nil
          }
      case _ => CommentaryEnglish.section(section)
    def unit(section: CommentaryAnswer.Section, announced: Set[CommentaryAnswer.Statement]): Option[Unit] =
      val rendered = CommentaryEnglish.renderedSection(section, announced)
      val en = rendered.parts
      val translated = korean(section, announced)
      val refs = en.collect { case ref: MoveRef => ref }
      val translatedRefs = translated.collect { case ref: MoveRef => ref }
      val required = (section.required ++ section.references).distinct
      val englishOnly = !(targetResponseSection(section) || referenceSection(section)) || refs != translatedRefs
      val ko = if englishOnly then en else translated
      val owned = rendered.expressed.flatMap(_.references).distinct
      val covered = rendered.expressed.flatMap(_.covered).distinct
      val recordedEnds = section.statements.collect { case CommentaryAnswer.RecordedEnd(move) => move }.distinct
      Option.when(required.forall(owned.contains) && section.covered.toSet == covered.toSet)(
        Unit(if covered.nonEmpty then "cost" else "reply", en, ko, covered,
          required, section.focus, englishOnly, recordedEnds, rendered.expressed))
    // Split only the delivery container, between whole selected statements.
    // Its field bound cannot drop a statement or select another chess answer.
    def unitsFor(section: CommentaryAnswer.Section, announced: Set[CommentaryAnswer.Statement] = Set.empty): List[Option[Unit]] =
      def subset(statements: List[CommentaryAnswer.Statement]) =
        val refs = statements.flatMap(_.references).toSet
        CommentaryAnswer.Section(statements, section.required.filter(refs), section.focus.filter(f => refs(f.move)))
      unit(section, announced) match
        case None => List(None)
        case Some(value) if value.en.size <= 80 && value.ko.size <= 80 => List(Some(value))
        case _ =>
          section.statements.foldLeft(List.empty[List[CommentaryAnswer.Statement]]) { (groups, next) =>
            val current = groups.lastOption.getOrElse(Nil)
            val merged = current :+ next
            if current.nonEmpty && unit(subset(merged), announced).exists(u => u.en.size > 80 || u.ko.size > 80) then
              groups :+ List(next)
            else groups.dropRight(1) :+ merged
          }.map(statements => unit(subset(statements), announced))
    // Check the original obligations before splitting a delivery container;
    // filtering per-container references must not erase a missing quiet reply.
    val coreContent = answer.core.flatMap( section => CommentaryEnglish.renderedSection(section).expressed)
    if !answer.required.forall(coreContent.flatMap(_.references).contains) then return None
    if !AnchoredChoiceAnswer.pricesExpressed(coreContent) ||
        !AnchoredChoiceAnswer.pricesExpressed(answer.details.flatMap(section =>
          CommentaryEnglish.renderedSection(section).expressed)) then return None
    // Physical episodes remain separate through selection and price closure.
    // At delivery, the same complete statement only needs one owner; sharing
    // that statement does not join the episodes or discard another obligation.
    val spoken = mutable.Set.empty[CommentaryAnswer.Statement]
    val selectedCore = answer.core.flatMap { section =>
      val statements = section.statements.filter(spoken.add)
      val refs = statements.flatMap(_.references).toSet
      Option.when(statements.nonEmpty)(section.copy(statements = statements,
        required = section.required.filter(refs), focus = section.focus.filter(f => refs(f.move))))
    }
    val deliveryCore = ReferencePurposeText.coalesce(selectedCore)
    val deliveryContent = deliveryCore.flatMap(section => CommentaryEnglish.renderedSection(section).expressed)
    if !answer.required.forall(deliveryContent.flatMap(_.references).contains) then return None
    // Only a preceding, owned purpose licenses a short reference to its
    // realization. Standalone/detail replies retain the full functional text.
    var announced = Set.empty[CommentaryAnswer.Statement]
    val units = deliveryCore.flatMap { section =>
      val result = unitsFor(section, announced)
      announced ++= section.statements.collect {
        case value: CommentaryAnswer.ReferencePurpose => value
        case value: CommentaryAnswer.PreparedIdea => value
        case value: CommentaryAnswer.FunctionPurpose => value
      }
      result
    }
    // Repeated completion may retain the same whole price account again.
    // Share only an exactly identical section, including its required moves
    // and focus. Different physical witnesses or price contexts stay separate.
    val detailUnits = answer.details.distinct.flatMap(section => unitsFor(section))
    // A physical episode owns its price before delivery. Several complete
    // episodes can share a bounded display container without sharing that
    // ownership or omitting content. Keep their order and incompatible focus
    // frames, language modes and roles separate.
    def packed(values: List[Unit]): List[Unit] =
      if values.size <= 8 then values else values.foldLeft(List.empty[Unit]) { (groups, next) =>
        groups.lastOption match
          case Some(current) if current.role == next.role && current.englishOnly == next.englishOnly =>
            val focus = current.focus ++ next.focus
            val merged = current.copy(en = current.en ++ List(" ") ++ next.en,
              ko = current.ko ++ List(" ") ++ next.ko,
              covered = (current.covered ++ next.covered).distinct,
              required = (current.required ++ next.required).distinct,
              focus = focus.distinct,
              recordedEnds = (current.recordedEnds ++ next.recordedEnds).distinct,
              expressed = (current.expressed ++ next.expressed).distinct)
            if merged.en.size <= 80 && merged.ko.size <= 80 && merged.covered.size <= 80 &&
                merged.required.size <= 80 && merged.recordedEnds.size <= 80 && merged.focus.size <= 80 &&
                focus.groupBy(_.move).values.forall(_.distinct.size == 1) then
              groups.dropRight(1) :+ merged
            else groups :+ next
          case _ => groups :+ next
      }
    Option.when((units ++ detailUnits).forall(_.nonEmpty))(units.flatten).flatMap(values =>
      fromUnits(paths, pair, answer.question, packed(values),
        answer.evidence ++ (answer.core ++ answer.details).flatMap(_.focus.flatMap(_.note.evidence)),
        detailUnits = packed(detailUnits.flatten)))

  private def renderMaterialKorean(value: CommentaryAnswer.Material, brief: Boolean = false): List[String | MoveRef] =
    val cost = value.cost
    val numbers = Map(1 -> "한", 2 -> "두", 3 -> "세", 4 -> "네", 5 -> "다섯", 6 -> "여섯", 7 -> "일곱", 8 -> "여덟")
    def counted(xs: List[(EvidencePieceRole, Int)]) = xs.map((role, n) =>
      s"${names(role.name)} ${numbers.getOrElse(n, n.toString)} 개").mkString("와 ")
    val common = Option.when(cost.commonExchanges.nonEmpty)(s"양쪽이 ${counted(cost.commonExchanges)}씩 교환합니다")
    val extra = if cost.whiteAdditionalLosses.nonEmpty && cost.blackAdditionalLosses.nonEmpty then
      Some(s"백은 ${counted(cost.whiteAdditionalLosses)}를 내주고 ${counted(cost.blackAdditionalLosses)}를 얻습니다")
    else if cost.whiteAdditionalLosses.nonEmpty then
      Some(s"백은 ${counted(cost.whiteAdditionalLosses)}를 ${if common.nonEmpty then "더 " else ""}내줍니다")
    else if cost.blackAdditionalLosses.nonEmpty then
      Some(s"흑은 ${counted(cost.blackAdditionalLosses)}를 ${if common.nonEmpty then "더 " else ""}내줍니다")
    else None
    val selected = if brief then List(MoveRef(value.path, 0)) else value.references
    val moves: List[String | MoveRef] = selected.zipWithIndex.flatMap { (ref, index) =>
      (if index == 0 then Nil else List(if value.references(index - 1).offset + 1 == ref.offset then " " else " … ")) :+ ref
    }
    val end = cost.offsets.max
    val later = brief && value.path.certified.flatMap(_._2.material).exists(_.events.exists(_.plyOffset > end))
    List("제시된 ") ++ moves ++
      (if later then List(" 수순의 ", MoveRef(value.path, end), "까지는 ") else List(" 수순에서 ")) ++
      List(s"${(common.toList ++ extra).mkString(". ")}.")

  class Arm(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, val path: PreparedPath,
      positions: Map[PositionNodeRef, Position], val reading: Reading):
    val captures: List[Event] = reading.events.filter(_.originalVictim.nonEmpty)
    val material = path.certified.flatMap(_._2.material).get
    private val checking = path.certified.toList.flatMap(_._2.lineEvents)
      .filter(e => Set(LineEventKind.Check, LineEventKind.Mate)(e.kind)).map(_.plyOffset).toSet
    val proofs = mutable.Set.empty[String]
    val originals: List[Piece] = paths.identitiesAfter(packet, path, 0).map(_._1)
    def ref(offset: Int): MoveRef = MoveRef(path, offset)
    def at(piece: Piece, count: Int): Option[Piece] = paths.pieceAfter(packet, path, piece, count).flatten
    def position(count: Int): Option[Position] = path.positions.lift(count).flatMap(positions.get).map { p =>
      proofs += p.view.occurrence.id
      p
    }
    def mover(offset: Int): Option[Piece] = for
      from <- path.moves.lift(offset).map(_.take(2))
      current <- position(offset).flatMap(_.occupant(from))
      original <- originals.find(p => at(p, offset).contains(current))
    yield original
    val root: Option[Piece] = mover(0)
    def route(piece: Piece): List[Int] = path.moves.indices.filter(n =>
      at(piece, n).nonEmpty && at(piece, n + 1).nonEmpty && at(piece, n) != at(piece, n + 1)).toList
    def control(count: Int, actor: Piece, target: Piece): Option[Boolean] =
      at(target, count).flatMap(t => controlSquare(count, actor, t.square.key))
    def controlSquare(count: Int, actor: Piece, square: String): Option[Boolean] = for
      p <- position(count)
      state <- paths.pieceAfter(packet, path, actor, count)
      result <- state match
        // The outer absence is unknown tracking; this inner absence is the
        // original piece's certified removal, which cannot control a square.
        case None => Some(false)
        case Some(a) =>
          if p.controlledBy(a)(square) then
            proofs ++= p.sourceRefs(p.sources(a, List(square)))
            Some(true)
          else Option.when(p.complete(RelationFactKind.GeometricControl))(false)
    yield result
    def unpinned(count: Int, piece: Piece): Boolean = (for
      actor <- at(piece, count)
      p <- position(count)
      if p.complete(RelationFactKind.RayBarrier)
    yield !p.rays.exists { (_, ray) => ray.occupants.take(2) match
      case List(front, rear) => front.square == actor.square && front.side == actor.side &&
        rear.side == actor.side && rear.role.name == "king" && ray.side != actor.side
      case _ => false
    }).contains(true)
    def covers(count: Int, actor: Piece, target: Piece): Boolean =
      control(count, actor, target).contains(true) && unpinned(count, actor)
    def stationary(piece: Piece, end: Int): Boolean = (0 to end + 1).forall(n => at(piece, n).contains(piece))
    def quiet(end: Int): Boolean = end < path.moves.size &&
      !captures.exists(_.plyOffset <= end) && !checking.exists(_ <= end) &&
      position(0).exists(p => path.positions.head.sideToMove.exists(side =>
        p.view.currentCheckSources(side).isEmpty && p.complete(RelationFactKind.GeometricControl)))
    def gained(target: Piece): Boolean = root.exists(actor =>
      control(0, actor, target).contains(false) && covers(1, actor, target))
    def evaded(offset: Int, target: Piece): Option[Piece] = for
      actor <- mover(offset - 1).filter(p => offset > 0 && p.side != target.side)
      if mover(offset).contains(target) && control(offset - 1, actor, target).contains(false) &&
        control(offset, actor, target).contains(true) && control(offset + 1, actor, target).contains(false)
    yield actor
    def supportUse(target: Piece): List[DirectSupportUse] = root.toList.flatMap { provider =>
      LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size, provider, target).uses
        .filter(use => gained(target) && (1 to use.targetCapture.plyOffset).forall(n => covers(n, provider, target)) &&
          position(use.recapture.plyOffset).flatMap(_.view.legalRows).exists(_.exists(row =>
            row.move == path.moves(use.recapture.plyOffset) && row.capture.nonEmpty)))
    }
    def supportSources(use: DirectSupportUse): List[String] =
      position(use.supportCount).toList.flatMap(_.sourceRefs(use.support.sourceIds)) ++
        List(use.targetCapture, use.recapture).flatMap(event =>
          position(event.plyOffset).toList.flatMap(_.sourceRefs(List(event.sourceId))))
    def checks(offset: Int): Boolean = checking(offset)
    def recovery(event: Event): Option[Event] = reading.captureLinks.find(_.lossEventId == event.id)
      .flatMap(link => captures.find(_.id == link.captureEventId))
    /** A physical removal belongs to this capture only while the actual
      * recovering piece keeps attacking that same original capturer. */
    def continuousRecovery(event: Event): Option[Event] = recovery(event).filter { removal =>
      (for
        capturer <- event.originalActor
        recovering <- removal.originalActor
        if removal.originalVictim.contains(capturer) && removal.plyOffset > event.plyOffset
      yield (event.plyOffset + 1 to removal.plyOffset).forall { count =>
        covers(count, recovering, capturer) || (for
          p <- position(count)
          actor <- at(recovering, count)
          target <- at(capturer, count)
          capture <- p.view.legalRows.flatMap(_.find(row => row.actor == actor && row.captures(target)))
        yield
          // A pin can still allow capture along its ray. Use the admitted
          // exact legal action, rather than relaxing geometric pin checks.
          proofs ++= p.sourceRefs(List(capture.sourceId))
          true).contains(true)
      }).contains(true)
    }

    private lazy val observed = packet.observations.groupBy(_.transition.evidence.id)
    def checkEvents(offsets: List[Int]): List[CommentaryAnswer.CheckEvent] =
      val events = MoveWorkProjection.checkSteps(path, offsets, None, observed)
      proofs ++= events.flatMap(_._3)
      events.map(_._2)
    def balanced(events: List[Event]): Boolean =
      val rows = material.captures.filter(row => events.exists(_.plyOffset == row.plyOffset))
      rows.size == events.size && rows.groupMapReduce(_.capturedRole.name)(row => if row.side.white then 1 else -1)(_ + _).values.forall(_ == 0)
    def identityRoles(events: List[Event]): Boolean = events.forall(event =>
      path.ids.lift(event.plyOffset).contains(event.occurrenceId) &&
        material.events.find(row => row.plyOffset == event.plyOffset &&
          EvidenceRef.sameMove(row.moveUci, path.moves(event.plyOffset)))
        .flatMap(_.capture).exists(capture =>
          event.originalActor.exists(_.role == capture.attackerRole) &&
            event.originalVictim.exists(_.role == capture.capturedRole)))
    def pieceExchange(events: List[Event]): CommentaryAnswer.PieceExchange =
      require(events.forall(captures.contains))
      val recaptures = material.captures.filter(_.recapture).map(_.plyOffset).toSet
      CommentaryAnswer.PieceExchange(path, events, events.map(_.plyOffset).toSet.intersect(recaptures))
    def retained(piece: Piece): Boolean = at(piece, path.moves.size).nonEmpty &&
      position(path.moves.size).exists(_.complete(RelationFactKind.GeometricControl)) &&
      !originals.filter(_.side != piece.side).exists(p => control(path.moves.size, p, piece).contains(true) &&
        !control(0, p, piece).contains(true))
    def singleEvasion(actor: Piece, target: Piece, start: Int = 0): Option[Int] =
      route(target).find(_ > start).filter { escape =>
        (start + 1 to escape).forall(n => covers(n, actor, target)) &&
          (escape + 1 to path.moves.size).forall { n =>
            (paths.pieceAfter(packet, path, actor, n), paths.pieceAfter(packet, path, target, n)) match
              case (Some(None), Some(_)) | (Some(_), Some(None)) => true
              case (Some(Some(_)), Some(Some(_))) => control(n, actor, target).contains(false)
              case _ => false
          }
      }

    /** Existing selected exchange/recovery questions can own a later actual
      * recovery. A target-outcome question has a narrower dependent episode. */
    def account(seed: Event, focus: Piece): Account =
      account(seed, focus, targetOutcome = false, comparison = false)

    def targetAccount(seed: Event, focus: Piece): Account =
      account(seed, focus, targetOutcome = true, comparison = false)

    /** An observed comparison owns connected exchanges, not every check or
      * compensating capture between their dates. Existing mechanism accounts
      * retain their broader, separately selected obligations. */
    def comparisonAccount(seed: Event, focus: Piece): Account =
      account(seed, focus, targetOutcome = true, comparison = true)

    /** The selected quiet user is actually lost. Own that loss and its
      * capturer's recorded recovery, while dependent losses still require a
      * connected recovery. Those dependencies are not new lifetime stories. */
    def usedPieceLossAccount(seed: Event, focus: Piece, counterTargets: Set[Piece] = Set.empty): Account =
      require(seed.originalVictim.contains(focus))
      require(counterTargets.subsetOf(originals.toSet))
      account(seed, focus, targetOutcome = true, comparison = true, ownedLoss = true,
        counterTargets = counterTargets)

    private def account(seed: Event, focus: Piece, targetOutcome: Boolean, comparison: Boolean,
        ownedLoss: Boolean = false, counterTargets: Set[Piece] = Set.empty): Account =
      require(captures.contains(seed) && originals.contains(focus))
      val selected = mutable.ListBuffer.empty[Event]
      val offsets = mutable.Set.empty[Int]
      val context = mutable.Set.empty[Int]
      val duties = mutable.ListBuffer.empty[(Event, Event)]
      def add(event: Event): scala.Unit =
        if !selected.contains(event) then selected += event
        offsets += event.plyOffset
        // A capture answering check carries that preceding check as context.
        if checks(event.plyOffset - 1) then offsets += event.plyOffset - 1
      def span(start: Event, followsFocus: Boolean, ownsLoss: Boolean = false): List[Event] =
        // A dependent capturer's later removal is not this episode's price
        // unless the actual recovering attack remains connected to it.
        val recapture = if followsFocus || ownsLoss || !targetOutcome then recovery(start) else continuousRecovery(start)
        val end = recapture.map(_.plyOffset).getOrElse(if followsFocus then path.moves.size - 1 else start.plyOffset)
        captures.filter(e => e.plyOffset >= start.plyOffset && e.plyOffset <= end &&
          (e.originalActor == start.originalActor || recapture.contains(e)))
      span(seed, seed.originalActor.contains(focus), ownedLoss).foreach(add)
      // A quiet selected attack can be answered by taking its user while an
      // ally takes the originally threatened piece on another square. Own
      // that immediate counter-capture and its connected recovery; a later
      // unrelated gain is not compensation for this selected episode.
      if ownedLoss && counterTargets.nonEmpty then captures.find(event =>
        event.plyOffset == seed.plyOffset + 1 && event.originalActor.exists(_.side == focus.side) &&
          event.originalVictim.exists(counterTargets)).foreach(event => span(event, false).foreach(add))
      seed.originalActor.flatMap(p => route(p).filter(_ < seed.plyOffset).lastOption).filter(checks)
        .flatMap(n => captures.find(_.plyOffset == n)).foreach(e => span(e, false).foreach(add))
      if !comparison && recovery(seed).isEmpty && seed.originalActor.contains(focus) then
        selected.filter(_.originalActor.contains(focus)).lastOption.foreach { last =>
          captures.find(e => e.plyOffset == last.plyOffset + 1 && e.originalActor.exists(_.side != focus.side))
            .foreach(e => span(e, false).foreach(add))
        }
      for
        reply <- captures.find(_.plyOffset == 1)
        counter <- captures.find(e => e.plyOffset == 2 && e.originalActor == root)
        if selected.contains(reply) || selected.contains(counter)
      // An already admitted recapture closes its parent's exchange; it does
      // not start a new account of the recapturing piece's later lifetime.
      do List(reply, counter).filterNot(selected.contains).foreach(e => span(e, false).foreach(add))
      if checks(0) && !captures.exists(_.plyOffset == 0) then context += 0

      def checkReplies(): scala.Unit =
        var end = offsets.max
        while checks(end + 1) do
          offsets += end + 1
          if end + 2 < path.moves.size then offsets += end + 2
          end = offsets.max
        val selectedChecks = if comparison then offsets.toList.filter(checks) else (offsets.min to offsets.max).filter(checks)
        selectedChecks.foreach { n =>
          offsets += n
          if n + 1 < path.moves.size then offsets += n + 1
          captures.find(e => e.plyOffset == n + 2 && e.originalActor == mover(n)).foreach(add)
        }
        context.toList.filter(checks).foreach(n => if n + 1 < path.moves.size then context += n + 1)
        captures.filter(e => offsets(e.plyOffset) || context(e.plyOffset)).foreach(add)
      def lostDuty(loss: Event): List[Event] = loss.originalVictim.toList.flatMap { defender =>
        captures.filter(_.plyOffset > loss.plyOffset).filter { later => (for
          target <- later.originalVictim.filter(_.side == defender.side)
          capturer <- later.originalActor
          d <- at(defender, loss.plyOffset)
          t <- at(target, loss.plyOffset)
          c <- at(capturer, loss.plyOffset)
          start <- position(loss.plyOffset).filter(_.complete(RelationFactKind.RayBarrier))
          if start.controllers(t.square.key, target.side) == List(d)
          if !start.rays.exists { (_, ray) => ray.occupants.take(2) match
            case List(front, rear) => front.square == d.square && front.side == d.side && rear.side == d.side &&
              rear.role.name == "king" && ray.side != d.side &&
              !(ray.attackerSquare == c.square && ray.attackerRole == c.role && ray.side == c.side) &&
              !ray.geometry.squares.takeWhile(_ != rear.square).contains(t.square)
            case _ => false
          }
        yield (loss.plyOffset to later.plyOffset).forall { n =>
          at(target, n).contains(t) && at(capturer, n).contains(c) && control(n, capturer, target).contains(true) &&
            position(n).exists(p => p.complete(RelationFactKind.GeometricControl) &&
              (n == loss.plyOffset || p.controllers(t.square.key, t.side).isEmpty))
        }).contains(true) }
      }
      var processed = 0
      def ownsRecovery(link: LineContinuationInvestigation.CaptureLink): Boolean =
        !targetOutcome || link.elapsedPlies == 1 || ownedLoss && link.lossEventId == seed.id || (for
          loss <- captures.find(_.id == link.lossEventId)
          removal <- captures.find(_.id == link.captureEventId)
        yield loss.originalActor.contains(focus) || continuousRecovery(loss).contains(removal)).contains(true)
      def finish(): scala.Unit =
        checkReplies()
        while processed < selected.size do
          val event = selected(processed)
          processed += 1
          lostDuty(event).filterNot(selected.contains).foreach { cost =>
            duties += event -> cost
            span(cost, false).foreach(add)
          }
          reading.captureLinks.filter { link =>
            ownsRecovery(link) && (link.lossEventId == event.id || link.captureEventId == event.id) &&
              (link.elapsedPlies == 1 || captures.find(_.id == link.lossEventId).exists(loss =>
                // Keep the root mover's delayed loss on its recapture square;
                // another recapturer does not become the question's subject.
                (link.captureEventId == event.id || loss.originalActor == root) &&
                  loss.originalActor.exists(p => !route(p).exists(n =>
                    n > loss.plyOffset && n < loss.plyOffset + link.elapsedPlies))))
          }.foreach { link =>
            captures.find(_.id == (if link.lossEventId == event.id then link.captureEventId else link.lossEventId)).foreach(add)
          }
          checkReplies()
      finish()
      val ledger = path.certified.flatMap(_._2.material).get.captures.map(e => e.plyOffset -> e).toMap
      def delta(e: Event): Int = ledger(e.plyOffset).valueCp * (if e.originalVictim.get.side == root.get.side then -1 else 1)
      val admitted = selected.map(_.id).toSet
      Option.when(!comparison)(reading.otherTargetGains).toList.flatten.foreach { link =>
        for
          loss <- captures.find(_.id == link.lossEventId)
          gain <- captures.find(_.id == link.gainEventId)
          if admitted(loss.id) != admitted(gain.id) && gain.plyOffset == loss.plyOffset + 1
          missing = if admitted(loss.id) then gain else loss
          if !selected.contains(missing) && !reading.captureLinks.exists(l => l.lossEventId == missing.id || l.captureEventId == missing.id)
          if selected.map(delta).sum * delta(missing) < 0
        do add(missing)
      }
      finish()
      def vector(events: List[Event]): List[Int] = events.groupMapReduce(e => ledger(e.plyOffset).capturedRole.name)(
        e => if e.originalVictim.get.side == root.get.side then -1 else 1)(_ + _).values.filter(_ != 0).toList
      val initial = vector(selected.toList)
      if !comparison && initial.nonEmpty && initial.forall(n => math.signum(n) == math.signum(initial.head)) then
        val groups = mutable.ListBuffer.from(captures.map(List(_)))
        def join(a: Event, b: Event): scala.Unit =
          val left = groups.indexWhere(_.contains(a))
          val right = groups.indexWhere(_.contains(b))
          if left != right then
            groups(left) = groups(left) ++ groups(right)
            groups.remove(right)
        reading.captureLinks.foreach { link =>
          for
            a <- captures.find(_.id == link.lossEventId)
            b <- captures.find(_.id == link.captureEventId)
            if ownsRecovery(link)
            if link.elapsedPlies == 1 || a.originalActor.exists(p => !route(p).exists(n => n > a.plyOffset && n < b.plyOffset))
          do join(a, b)
        }
        captures.sliding(2).foreach { case List(a, b) if b.plyOffset == a.plyOffset + 1 => join(a, b); case _ => () }
        groups.toList.sortBy(_.map(_.plyOffset).min).filter(_.exists(selected.contains)).foreach { group =>
          val missing = group.filterNot(selected.contains)
          val change = vector(missing)
          if change.nonEmpty && change.forall(n => math.signum(n) != math.signum(initial.head)) then
            missing.foreach(add)
            finish()
        }
      // A widened capturer story must retain the preceding gain of its side,
      // even when that capturer is only a dependency of the original question.
      Option.when(!comparison)(selected.toList).toList.flatten.foreach { loss =>
        val prior = selected.filter(e => e.originalActor == loss.originalVictim && e.plyOffset < loss.plyOffset).sortBy(_.plyOffset).lastOption
        if prior.exists(e => loss.plyOffset > e.plyOffset + 1) then
          captures.find(e => e.plyOffset == loss.plyOffset - 1 && e.originalActor.map(_.side) == loss.originalVictim.map(_.side))
            .foreach(e => span(e, false).foreach(add))
      }
      finish()
      val refs = (offsets ++ context).toList.sorted
      Account(selected.toList.sortBy(_.plyOffset), refs, duties.toList,
        refs.forall(n => !checks(n) || refs.contains(n + 1)))

  /** Facts are selected before any language is realized. Failure to deliver
    * them cannot erase the question or select a different chess explanation.
    */
  case class SelectedQuestion(confirmedFacts: CommentaryAnswer.Answer):
    def question: String = confirmedFacts.question
    def evidence: List[String] = confirmedFacts.evidence
    def answer: Option[CommentaryAnswer.Answer] = Some(confirmedFacts)
    def realize(paths: PreparedPaths, pair: List[PreparedPath]): Option[Result] =
      RootQuestionSummary.realize(paths, pair, confirmedFacts)

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position]): Option[Result] =
    choose(packet, paths, pair, positions).flatMap(_.realize(paths, pair))

  def choose(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position]): Option[SelectedQuestion] =
    import CommentaryAnswer.{Answer, Section, Statement, Action, Material, MaterialEvents, TargetResponse,
      SupportRecapture, Continuation, Convergence, PieceRoute, PiecePresence, ControlState, CheckStatus, RecordedEnd,
      BlockedAttack}
    val arms = prepare(packet, paths, pair, positions) match
      case Some(value) => value
      case None => return None
    if arms.exists(_.root.isEmpty) then return None
    val choices = mutable.ListBuffer.empty[SelectedQuestion]
    def validRef(ref: MoveRef): Boolean = pair.exists(_ eq ref.path) &&
      ref.offset >= 0 && ref.offset < ref.path.moves.size
    def proofs: List[String] = arms.flatMap(_.proofs.toList.sorted).distinct
    def offerAnswer(question: String, core: List[Section], details: List[Section] = Nil,
        evidence: List[String] = Nil): scala.Unit =
      // Expansion uses exactly the same account as the core, not a second
      // selection or a recomputed final-position material total.
      val accounts = core.flatMap(_.statements.flatMap {
        case value: Material => List(value)
        case value: TargetResponse => value.material.toList
        case _ => Nil
      }).distinct
      val expanded = (details ++ accounts.map(value => Section(
        List(Continuation(value.references), MaterialEvents(value.path, value.cost.events)), value.references))).distinct
      val refs = (core ++ expanded).flatMap(section => section.required ++ section.references ++ section.covered)
      if core.nonEmpty && refs.forall(validRef) && pair.forall(path => core.exists(_.references.contains(MoveRef(path, 0)))) then
        val owned = refs.flatMap(ref => List(ref.path.ids(ref.offset)) ++ ref.path.certified.map(_._1.ref.id))
        val answer = Answer(question, core, expanded, (proofs ++ evidence ++ owned).distinct)
        choices += SelectedQuestion(answer)
    def continuation(refs: List[MoveRef]): List[Section] = Option.when(refs.nonEmpty)(
      Section(List(Continuation(refs.distinct)), refs.distinct)).toList
    def material(a: Arm, events: List[Event], offsets: List[Int]): Option[Material] =
      CommentaryMaterialAccount.calculate(a.material, events.map(_.plyOffset), offsets, a.path.moves.size)
        .map(Material(a.path, _))
    def selected(): Option[SelectedQuestion] = choices.toList.distinctBy(value =>
      (value.question, pair.map(path => path.line.ref -> path.ids), value.evidence.toSet,
        value.confirmedFacts.core, value.confirmedFacts.details)) match
      case value :: Nil => Some(value)
      case _ => None

    // The promotion reader is isolated: all older readers still see only
    // promotion-free records. Original pawn identity and its promoted role
    // come from the existing replay and material ledger.
    val promotions = arms.flatMap(a => a.material.events.filter(_.promotionGainCp > 0).map(a -> _))
    if promotions.nonEmpty then
      if promotions.size == 1 then
        val (later, promotion) = promotions.head
        val now = arms.find(_ ne later).get
        val promotedAt = promotion.plyOffset
        for
          pawn <- later.mover(promotedAt).filter(p => p.role.name == "pawn" && now.root.contains(p)).toList
          gain <- later.captures.find(e => e.plyOffset == promotedAt && e.originalActor.contains(pawn)).toList
          target <- gain.originalVictim.toList
          advance <- later.route(pawn).find(i => i > 0 && i < promotedAt && later.path.moves(i) == now.path.moves.head).toList
          removal <- later.recovery(gain).filter(e => e.plyOffset == promotedAt + 1 && e.originalVictim.contains(pawn)).toList
          escape <- now.singleEvasion(pawn, target).filter(_ > 0).toList
          loss <- now.captures.find(e => e.plyOffset > escape && e.originalActor.contains(target) && e.originalVictim.contains(pawn)).toList
          if now.gained(target) && now.quiet(escape) && later.quiet(advance) &&
            later.control(advance, pawn, target).contains(false) &&
            (advance + 1 to promotedAt).forall(i => later.covers(i, pawn, target)) &&
            now.balanced(now.captures) && !now.path.moves.indices.exists(now.checks) &&
            later.at(pawn, promotedAt + 1).exists(_.role == promotion.movement.afterRole) &&
            paths.pieceAfter(packet, later.path, pawn, removal.plyOffset + 1).contains(None)
          intervening = later.captures.filter(e => e.plyOffset > advance && e.plyOffset < promotedAt)
          if intervening.size == 1 && intervening.head.plyOffset == advance + 1 && promotedAt == advance + 2 &&
            intervening.head.originalActor.exists(_.side != pawn.side) &&
            intervening.head.originalVictim.exists(p => p.side == pawn.side && p != pawn)
          counter = later.captures.find(_.plyOffset == removal.plyOffset + 1).toList
          if counter.forall(e => e.originalActor.exists(_.side == pawn.side) && later.recovery(e).isEmpty)
          end = counter.lastOption.map(_.plyOffset).getOrElse(removal.plyOffset)
          named = intervening ++ List(gain, removal) ++ counter
          if later.captures.filter(_.plyOffset <= end) == named &&
            !(0 to end).filter(_ != promotedAt).exists(later.checks)
        do
          val nowOffsets = (List(0, escape) ++ now.captures.map(_.plyOffset)).distinct.sorted
          material(now, now.captures, nowOffsets).foreach { cost =>
            val laterOffsets = ((0 to advance).toList ++ named.map(_.plyOffset)).distinct.sorted
            val laterEvents = later.material.events.filter(_.plyOffset <= end)
            val nowLoss = now.material.events.filter(_.plyOffset == loss.plyOffset)
            offerAnswer("deferred_pawn_attack", List(
              Section(List(TargetResponse(now.path, 0, now.at(target, 1).get, pawn, target,
                Some(escape), Some(cost), survives = false), MaterialEvents(now.path, nowLoss)), nowOffsets.map(now.ref)),
              Section(List(Continuation(laterOffsets.map(later.ref)), PieceRoute(later.path, pawn, List(advance, promotedAt)),
                MaterialEvents(later.path, laterEvents)), laterOffsets.map(later.ref))),
              List(Section(List(Continuation(laterOffsets.map(later.ref)), MaterialEvents(later.path, laterEvents)),
                laterOffsets.map(later.ref))))
          }
      return selected()

    // A support question is admitted only after an actual transfer or reply
    // closes it. Two isolated support facts do not constitute the answer.
    for target <- arms.head.originals if target.side == arms.head.root.get.side &&
        !arms.exists(_.root.contains(target)) && arms.forall(_.stationary(target, 0)) do
      for (a, n) <- arms.zipWithIndex do
        val b = arms(1 - n)
        val provider = a.root.get
        val other = b.root.get
        if provider != other && a.gained(target) && b.gained(target) && arms.forall(_.quiet(0)) then
          val attackers = a.originals.filter(p => p.side != target.side && a.covers(0, p, target))
          val arrivals = a.route(other).filter(i => i > 0 && a.path.moves(i) == b.path.moves.head)
          for
            arrival <- arrivals.take(1)
            departure <- a.route(provider).find(i => i > arrival && a.control(i, provider, target).contains(true) &&
              a.control(i + 1, provider, target).contains(false)).toList
            if a.quiet(departure) && a.stationary(target, departure) &&
              (1 to departure).forall(i => a.covers(i, provider, target)) &&
              (arrival + 1 to departure + 1).forall(i => a.covers(i, other, target)) &&
              attackers.exists(p => (0 to departure + 1).forall(i => a.covers(i, p, target)))
          do
            val attack = a.evaded(departure, provider)
            val roots = arms.map { arm =>
              val current = arm.at(arm.root.get, 1).get
              Section(List(Action(arm.ref(0), MoveWorkProjection.Detail.Support(current, target, Nil, gained = true))))
            }
            val handoff = List[Statement](
              ControlState(a.ref(arrival), a.at(other, arrival + 1).get, Some(target), target.square.key, present = true),
              ControlState(a.ref(departure), a.at(provider, departure + 1).get, Some(target), target.square.key, present = false),
              ControlState(a.ref(departure), a.at(other, departure + 1).get, Some(target), target.square.key, present = true))
            val response = attack.toList.map(attacker => TargetResponse(a.path, departure - 1,
              a.at(provider, departure).get, attacker, provider, Some(departure), None, survives = false))
            val ongoingAttack = attackers.filter(p => (0 to departure + 1).forall(i => a.covers(i, p, target)))
              .map(attacker => ControlState(a.ref(0), a.at(attacker, 1).get, Some(target), target.square.key, present = true))
            offerAnswer("provider_transfer", roots :+ Section(ongoingAttack ++ handoff ++ response))

        if provider != other && a.gained(target) && b.control(0, other, target).contains(true) &&
            b.control(1, other, target).contains(false) && a.quiet(0) then
          for
            attacker <- b.mover(1).filter(_.side != target.side).toList
            if b.control(1, attacker, target).contains(false) && b.control(2, attacker, target).contains(true)
            blocker <- b.mover(2).filter(_.side == target.side).toList
            if b.control(3, attacker, target).contains(false) && b.at(attacker, 2) == b.at(attacker, 3)
            before <- b.position(2).toList
            afterBlock <- b.at(blocker, 3).toList
            if before.rays.exists { (_, ray) => b.at(attacker, 2).exists(p => ray.attackerSquare == p.square &&
              ray.side == p.side && ray.attackerRole == p.role) &&
              ray.geometry.squares.takeWhile(_ != target.square).contains(afterBlock.square) }
            remaining <- b.position(1).filter(_.complete(RelationFactKind.GeometricControl)).toList
            defenders = remaining.controllers(target.square.key, target.side).flatMap(p =>
              b.originals.find(original => b.at(original, 1).contains(p)))
            if defenders.forall(b.unpinned(1, _))
            counter <- b.control(3, blocker, attacker).toList
            if !counter || (b.evaded(3, attacker).contains(blocker) && b.unpinned(3, blocker))
            end = if counter then 3 else 2
            if b.quiet(end) && b.stationary(target, end) && b.at(blocker, end + 1).nonEmpty
          do
            val closed = MoveWorkProjection.Detail.ChangedLines(
              List((b.at(attacker, 2).get, target, Nil)), Nil, Nil)
            val response = Option.when(counter)(TargetResponse(b.path, 2, b.at(attacker, 3).get,
              blocker, attacker, Some(3), None, survives = false)).toList
            offerAnswer("support_and_interposition", List(
              Section(List(Action(a.ref(0), MoveWorkProjection.Detail.Support(a.at(provider, 1).get, target, Nil, gained = true)))),
              Section(List(
                ControlState(b.ref(0), b.at(other, 1).get, Some(target), target.square.key, present = false,
                  remaining = Some(defenders.flatMap(b.at(_, 1)))),
                ControlState(b.ref(1), b.at(attacker, 2).get, Some(target), target.square.key, present = true),
                Action(b.ref(2), closed)) ++ response, (0 to end).map(b.ref).toList)))

    // Both actual root recapturers answer one paired question. Preserve all
    // interleaved exchanges and the loss of either recapturer in the same unit.
    val uses = arms.map { a =>
      // Start with the actual root recapturer, as the existing support-use
      // projection does; do not run negative full-path queries for every piece.
      val targets = a.reading.captureLinks.flatMap(link => for
        recovery <- a.captures.find(e => e.id == link.captureEventId && e.originalActor == a.root)
        loss <- a.captures.find(_.id == link.lossEventId)
        target <- loss.originalVictim.filter(p => p.side == a.root.get.side && !a.root.contains(p))
      yield target).distinct
      targets.flatMap(a.supportUse).distinctBy(u => (u.originalTarget, u.targetCapture.id, u.recapture.id))
    }
    if uses.forall(_.size == 1) && uses.head.head.originalTarget != uses.last.head.originalTarget then
      val plans = arms.zip(uses.map(_.head)).map { (a, use) =>
        val provider = a.root.get
        val lost = a.captures.find(_.originalVictim.contains(provider))
        val end = lost.map(_.plyOffset).getOrElse(use.recapture.plyOffset)
        val events = a.captures.filter(_.plyOffset <= end)
        val gap = events.filter(e => e.plyOffset > use.targetCapture.plyOffset && e.plyOffset < use.recapture.plyOffset)
        val simpleGap = gap.isEmpty || (gap.size == 2 && gap.last.plyOffset == gap.head.plyOffset + 1 &&
          gap.last.originalVictim == gap.head.originalActor && gap.head.originalVictim.map(_.role) == gap.last.originalVictim.map(_.role))
        val continuous = lost.exists(e => e.plyOffset == use.recapture.plyOffset + 1 &&
          events.size == e.plyOffset - events.head.plyOffset + 1)
        def tally(side: chess.Color) = events.flatMap(_.originalVictim).filter(_.side == side).groupMapReduce(_.role.name)(_ => 1)(_ + _)
        val own = tally(provider.side)
        val laterDutyLoss = events.exists { loss => loss.originalVictim.exists { defender =>
          a.captures.filter(_.plyOffset > end).exists { later => later.originalVictim.exists { target =>
            target.side == defender.side && (for
              d <- a.at(defender, loss.plyOffset)
              t <- a.at(target, loss.plyOffset)
              before <- a.position(loss.plyOffset).filter(_.complete(RelationFactKind.GeometricControl))
              after <- a.position(loss.plyOffset + 1).filter(_.complete(RelationFactKind.GeometricControl))
            yield before.controllers(t.square.key, defender.side) == List(d) &&
              before.controllers(t.square.key, !defender.side).nonEmpty &&
              after.controllers(t.square.key, defender.side).isEmpty &&
              (loss.plyOffset to later.plyOffset).forall(count => a.at(target, count).contains(t))).contains(true)
          }}
        }}
        // ponytail: only a balanced, fully observed exchange interval is concise
        // here. Mixed exchanges need the general transaction policy before admission.
        val valid = !laterDutyLoss && own == tally(!provider.side) && !(0 to end).exists(a.checks) &&
          (if lost.nonEmpty then continuous else simpleGap && events.size == gap.size + 2 && a.at(provider, a.path.moves.size).nonEmpty) &&
          !a.reading.captureLinks.exists(link => events.exists(_.id == link.lossEventId) && !events.exists(_.id == link.captureEventId))
        (a, use, lost, events, gap, own, valid)
      }
      if plans.forall(_._7) then
        val sections = plans.map { (a, use, lost, events, _, _, _) =>
          val arrivals = events.flatMap(event => (event.originalActor.toList ++ event.originalVictim).flatMap(piece =>
            a.route(piece).filter(_ < event.plyOffset).lastOption))
          val offsets = (List(0) ++ events.map(_.plyOffset) ++ arrivals).distinct.sorted
          material(a, events, offsets).map { cost =>
            // The original ledger supplies the intervening captures as well
            // as the recapture. Keep the supporting piece's own later loss.
            val laterLoss = lost.toList.flatMap(e => a.material.events.find(_.plyOffset == e.plyOffset))
            Section(List(
              Action(a.ref(0), MoveWorkProjection.Detail.Support(a.at(a.root.get, 1).get,
                a.at(use.originalTarget, 1).get, Nil, gained = true)),
              SupportRecapture(a.ref(0), a.path, use), cost) ++
              Option.when(laterLoss.nonEmpty)(MaterialEvents(a.path, laterLoss)), offsets.map(a.ref))
          }
        }
        if sections.forall(_.nonEmpty) then
          val details = plans.flatMap { (a, use, lost, _, _, _, _) =>
            continuation((0 to lost.map(_.plyOffset).getOrElse(use.recapture.plyOffset)).map(a.ref).toList)
          }
          offerAnswer("paired_support_use", sections.flatten, details, evidence = plans.flatMap {
            case (a, use, _, _, _, _, _) => a.supportSources(use)
          })

    def fits(account: Account, events: List[Event], offsets: List[Int]): Boolean =
      account.complete && account.dutyCosts.isEmpty && account.events.toSet == events.toSet &&
        account.offsets.forall(offsets.contains)
    val rootNotInCheck = arms.head.position(0).exists(p =>
      p.complete(RelationFactKind.GeometricControl) && p.view.currentCheckSources(arms.head.root.get.side).isEmpty)
    // Retreat or use the other root mover for the actual recapture; capture
    // now or after attacking that same physical recapturer. Neither answer is
    // established by a material total or another piece's unrelated later fate.
    for (keep, n) <- arms.zipWithIndex if rootNotInCheck do
      val trade = arms(1 - n)
      val focus = keep.root.get
      val rootActor = trade.root.get
      val losses = trade.captures.filter(_.originalVictim.contains(focus))
      if keep.at(focus, keep.path.moves.size).nonEmpty then
        for
          loss <- losses
          attacker <- loss.originalActor.toList
        do
          val beforeGain = trade.captures.find(e => e.plyOffset < loss.plyOffset && e.originalActor.contains(focus))
          val cost = trade.account(beforeGain.getOrElse(loss), focus)
          val keepGains = keep.captures.filter(_.originalActor.contains(focus))
          val detailMoves = (keep.route(focus).filter(_ > 0) :+ (keep.path.moves.size - 1)).distinct.map(keep.ref)
          trade.recovery(loss).filter(e => e.plyOffset == loss.plyOffset + 1 && e.originalActor.contains(rootActor) &&
            e.originalVictim.contains(attacker) && focus.role == attacker.role).foreach { recapture =>
            val approach = trade.route(rootActor).filter(_ < recapture.plyOffset).lastOption
            for
              arrival <- approach
              if keep.retained(focus) && keepGains.isEmpty && keep.covers(0, attacker, focus) && keep.control(1, attacker, focus).contains(false) &&
                !keep.captures.exists(_.plyOffset <= 1) && !keep.checks(0) && !keep.checks(1) &&
                !trade.captures.exists(_.plyOffset == arrival)
              required = (List(0, arrival, loss.plyOffset, recapture.plyOffset) ++ Option.when(trade.checks(0))(1)).distinct
              if fits(cost, List(loss, recapture), required) &&
                trade.captures.filter(_.plyOffset <= recapture.plyOffset).toSet == cost.events.toSet &&
                !required.filter(_ != 0).exists(trade.checks)
            do
              material(trade, cost.events, required).foreach { exchange =>
                val check = Option.when(trade.checks(0))(CheckStatus(trade.ref(0),
                  trade.originals.find(p => p.role.name == "king" && p.side != rootActor.side).get,
                  checking = true, reply = Some(trade.ref(1)))).toList
                // The retained piece's route reaches the last supplied
                // position; this is survival within that record, not safety.
                val route = (List(0) ++ keep.route(focus).filter(_ > 0)).distinct
                offerAnswer("escape_or_recapture", List(
                  Section(List(Action(keep.ref(0), MoveWorkProjection.Detail.Evades(keep.at(focus, 1).get, attacker)),
                    PieceRoute(keep.path, focus, route), PiecePresence(keep.ref(keep.path.moves.size - 1),
                      focus, keep.at(focus, keep.path.moves.size).get)), List(keep.ref(0))),
                  Section(check ++ List(exchange, MaterialEvents(trade.path, exchange.cost.events)), required.map(trade.ref))),
                  continuation(detailMoves))
              }
          }
          for
            gain <- keepGains.filter(_.plyOffset == 0)
            victim <- gain.originalVictim.filter(_.role == focus.role).toList
            delayed <- beforeGain.filter(e => e.originalVictim.contains(victim) && loss.plyOffset == e.plyOffset + 1).toList
            escape <- trade.singleEvasion(rootActor, attacker).filter(_ < delayed.plyOffset).toList
            if trade.route(attacker).filter(_ < loss.plyOffset).lastOption.contains(escape) &&
              trade.recovery(delayed).contains(loss) &&
              keep.captures == List(gain) && trade.captures == List(delayed, loss) &&
              fits(keep.account(gain, focus), List(gain), List(0)) &&
              fits(cost, List(delayed, loss), List(0, escape, delayed.plyOffset, loss.plyOffset)) &&
              !arms.exists(a => a.path.moves.indices.exists(a.checks))
          do
            val required = List(0, escape, delayed.plyOffset, loss.plyOffset).distinct
            for
              now <- material(keep, List(gain), List(0))
              later <- material(trade, cost.events, required)
            do offerAnswer("capture_now_or_after_attack", List(
              Section(List(now), List(keep.ref(0))),
              Section(List(TargetResponse(trade.path, 0, trade.at(attacker, 1).get, rootActor, attacker,
                Some(escape), Some(later), survives = false)), required.map(trade.ref))), continuation(detailMoves))

    // The same target can be attacked or captured immediately. Follow the
    // actual evasion and its exchange/counterattack before naming the contrast.
    for (take, n) <- arms.zipWithIndex if rootNotInCheck do
      val attack = arms(1 - n)
      for
        gain <- take.captures.find(_.plyOffset == 0).toList
        target <- gain.originalVictim.toList
        recapture <- take.recovery(gain).filter(e => e.plyOffset == 1 && e.originalVictim.map(_.role).contains(target.role)).toList
        if fits(take.account(gain, target), List(gain, recapture), List(0, 1)) && !take.checks(0) && !take.checks(1)
        attacker <- attack.root.toList
        escape <- attack.singleEvasion(attacker, target).toList
      do
        val rootCapture = attack.captures.find(_.plyOffset == 0)
        val rootCost = rootCapture.map(attack.account(_, attacker))
        val response = attack.captures.find(_.plyOffset == escape)
        val recovery = response.flatMap(attack.recovery)
        val counterEscape = attack.singleEvasion(target, attacker, escape).filter(_ == escape + 1)
        val exchange = response.zip(recovery).find { (loss, recovery) =>
          recovery.plyOffset == escape + 1 && loss.originalVictim.map(_.role) == recovery.originalVictim.map(_.role)
        }.flatMap { (loss, recovery) =>
          val cost = attack.account(loss, target)
          Option.when(fits(cost, List(loss, recovery) ++ rootCapture, List(0, escape, recovery.plyOffset)))(cost)
        }
        val counter = Option.when(response.isEmpty)(counterEscape).flatten
        val end = escape + 1
        if (exchange.nonEmpty || counter.nonEmpty) && rootCost.forall(c => fits(c, rootCapture.toList, List(0))) &&
            attack.captures.filter(_.plyOffset <= end).toSet == (rootCapture.toList ++ response ++ recovery).toSet &&
            !(0 to end).exists(attack.checks) &&
            !take.captures.exists(e => e.plyOffset <= 1 && e != gain && e != recapture)
        then
          val events = exchange.map(_.events).getOrElse(rootCapture.toList)
          val required = List(0, escape, end).distinct
          val cost = Option.when(events.nonEmpty)(material(attack, events, required)).flatten
          if events.isEmpty || cost.nonEmpty then
            material(take, List(gain, recapture), List(0, 1)).foreach { immediate =>
              val counterattack = counter.toList.map(offset => TargetResponse(attack.path, escape,
                attack.at(attacker, escape + 1).get, target, attacker, Some(offset), None, survives = false))
              offerAnswer("attack_or_immediate_exchange", List(
                Section(List(TargetResponse(attack.path, 0, attack.at(target, 1).get, attacker, target,
                  Some(escape), cost, survives = false)) ++ counterattack, required.map(attack.ref)),
                Section(List(immediate), List(take.ref(0), take.ref(1)))))
            }

    // Both choices lose the same threatened piece and capture the same other
    // target. The actual countercapturer is not the original capturer.
    for (attack, n) <- arms.zipWithIndex if rootNotInCheck do
      val escape = arms(1 - n)
      val focus = escape.root.get
      val actor = attack.root.get
      for
        loss <- attack.captures.filter(_.originalVictim.contains(focus))
        otherLoss <- escape.captures.filter(_.originalVictim.contains(focus))
        captor <- loss.originalActor.toList
        counter <- attack.captures.find(e => e.plyOffset == loss.plyOffset + 1 && e.originalActor.contains(actor)).toList
        target <- counter.originalVictim.filter(p => p.side != focus.side && p != captor).toList
        otherCounter <- escape.captures.find(e => e.plyOffset == otherLoss.plyOffset + 1 && e.originalVictim.contains(target) &&
          e.originalActor.exists(_.side == focus.side) && !otherLoss.originalActor.contains(target)).toList
        counterActor <- otherCounter.originalActor.toList
        if actor != focus && attack.covers(0, captor, focus) && attack.covers(1, captor, focus) &&
          escape.control(1, captor, focus).contains(false) &&
          (0 to loss.plyOffset).forall(count => attack.at(focus, count).contains(focus)) &&
          attack.gained(target) && attack.mover(1).contains(target) && attack.evaded(2, actor).contains(target) &&
          attack.covers(2, target, actor) && attack.quiet(2) &&
          (3 to counter.plyOffset).forall(count => attack.covers(count, actor, target))
        approach <- escape.route(counterActor).filter(_ < otherCounter.plyOffset).lastOption.toList
        focusApproach <- escape.route(focus).filter(_ < otherLoss.plyOffset).lastOption.toList
        if approach > 0 && focusApproach > 0 && !escape.captures.exists(e =>
          e.plyOffset == approach || e.plyOffset == focusApproach || e.originalActor.contains(focus))
        reply = otherCounter.plyOffset + 1
        retreat = reply + 1
        responder <- escape.evaded(retreat, counterActor).toList
        if escape.covers(retreat, responder, counterActor) && escape.route(counterActor).contains(retreat)
      do
        val first = attack.account(loss, focus)
        val second = escape.account(otherLoss, focus)
        val events = first.events
        // ponytail: this realization handles the observed countercapture,
        // capturer's further gain, intervening trade and eventual removal.
        // Other mixed accounts stay detailed until their prose is verified.
        if events.size == 6 && events.head == loss && events(1) == counter then
          val List(_, _, advance, laterTake, laterLoss, removal) = events: @unchecked
          val required = List(0, 1, 2) ++ events.map(_.plyOffset)
          val otherRequired = List(0, approach, focusApproach, otherLoss.plyOffset, otherCounter.plyOffset, reply, retreat)
          val detailed = first.offsets.filterNot(required.contains)
          val checkPairs = detailed.forall(offset =>
            if attack.checks(offset) then detailed.contains(offset + 1)
            else detailed.contains(offset - 1) && attack.checks(offset - 1))
          val valid = advance.originalActor.contains(captor) && advance.plyOffset == counter.plyOffset + 1 &&
            removal.originalVictim.contains(captor) && attack.recovery(loss).contains(removal) &&
            laterLoss.plyOffset == laterTake.plyOffset + 1 && laterLoss.originalVictim == laterTake.originalActor &&
            attack.recovery(laterTake).contains(laterLoss) &&
            !required.exists(attack.checks) && !otherRequired.exists(escape.checks) && checkPairs &&
            fits(first, events, required ++ detailed) && fits(second, List(otherLoss, otherCounter), otherRequired) &&
            attack.captures.filter(_.plyOffset <= required.max).toSet == events.toSet &&
            escape.captures.filter(_.plyOffset <= otherRequired.max) == List(otherLoss, otherCounter)
          if valid then
            val firstOffsets = (required ++ detailed).distinct.sorted
            for
              attackCost <- material(attack, first.events, firstOffsets)
              escapeCost <- material(escape, second.events, otherRequired)
            do offerAnswer("counter_capture_comparison", List(
              Section(List(
                ControlState(attack.ref(0), attack.at(captor, 1).get, Some(focus), focus.square.key, present = true),
                Action(attack.ref(0), MoveWorkProjection.Detail.Attack(attack.at(actor, 1).get, attack.at(target, 1).get, gained = true)),
                TargetResponse(attack.path, 1, attack.at(actor, 2).get, target, actor, Some(2), None, survives = false),
                ControlState(attack.ref(2), attack.at(actor, 3).get, attack.at(target, 3),
                  attack.at(target, 3).get.square.key, present = true))),
              Section(List(attackCost, attack.pieceExchange(first.events)), firstOffsets.map(attack.ref)),
              Section(List(Action(escape.ref(0), MoveWorkProjection.Detail.Evades(escape.at(focus, 1).get, captor)),
                PieceRoute(escape.path, counterActor, List(approach)), PieceRoute(escape.path, focus, List(0, focusApproach)),
                TargetResponse(escape.path, reply, escape.at(counterActor, reply + 1).get, responder, counterActor,
                  Some(retreat), None, survives = false))),
              Section(List(escapeCost, escape.pieceExchange(second.events)), otherRequired.map(escape.ref))))

    // Compare an actually blocked pawn advance with its observed advance and
    // the other root pawn's recapture. Follow that pawn through its later loss.
    for (blocked, n) <- arms.zipWithIndex if rootNotInCheck do
      val open = arms(1 - n)
      val blocker = blocked.root.get
      val recapturer = open.root.get
      val square = blocked.path.moves.head.slice(2, 4)
      if blocker.role.name == "pawn" && recapturer.role.name == "pawn" && arms.forall(_.quiet(0)) then
        for
          pawn <- blocked.originals.filter(p => p.role.name == "pawn" && p.side != blocker.side &&
            p.square.key.head == square.head && square.last - p.square.key.last == (if p.side.white then 1 else -1))
          if (0 to blocked.path.moves.size).forall(count => blocked.at(pawn, count).contains(pawn) &&
            blocked.at(blocker, count).exists(_.square.key == (if count == 0 then blocker.square.key else square)))
          advance <- open.route(pawn).headOption.filter(i => i > 0 && open.path.moves(i) == pawn.square.key + square).toList
          if open.position(1).flatMap(_.view.legalRows).exists(_.exists(_.move == pawn.square.key + square)) &&
            blocked.position(1).flatMap(_.view.legalRows).exists(!_.exists(_.move == pawn.square.key + square)) &&
            (1 to advance).forall(count => open.at(pawn, count).contains(pawn) && open.position(count).exists(_.empty(square)))
          taken <- open.captures.find(e => e.plyOffset == advance + 1 && e.originalVictim.contains(pawn)).toList
          traded <- open.recovery(taken).filter(_.plyOffset == taken.plyOffset + 1).toList
          regained <- open.recovery(traded).filter(e => e.plyOffset == traded.plyOffset + 1 && e.originalActor.contains(recapturer)).toList
          if taken.originalActor.map(_.role) == traded.originalActor.map(_.role)
          lost <- open.recovery(regained).filter(e => e.plyOffset > regained.plyOffset + 1 && e.originalVictim.contains(recapturer)).toList
          gain <- open.captures.find(e => e.plyOffset == lost.plyOffset - 1 && e.originalActor.exists(_.side == recapturer.side) &&
            e.originalVictim.exists(_.role.name == "pawn")).toList
        do
          val chain = List(taken, traded, regained)
          val account = open.account(regained, recapturer)
          val named = chain ++ List(gain, lost)
          val background = open.captures.filterNot(named.contains)
          val neutral = background.size % 2 == 0 && background.grouped(2).forall {
            case List(a, b) => b.plyOffset == a.plyOffset + 1 && open.recovery(a).contains(b) &&
              a.originalVictim.map(_.role) == b.originalVictim.map(_.role)
            case _ => false
          }
          val ledger = open.path.certified.flatMap(_._2.material).get.captures
          val balance = ledger.groupMapReduce(_.capturedRole.name)(e => if e.side == recapturer.side then 1 else -1)(_ + _)
          val required = List(0, advance) ++ named.map(_.plyOffset)
          if neutral && fits(account, named, required) && !open.path.moves.indices.exists(open.checks) &&
              balance.getOrElse("pawn", 0) == 1 && balance.forall((role, count) => role == "pawn" || count == 0) then
            val offsets = (required ++ background.map(_.plyOffset)).distinct.sorted
            // Neutral intervening exchanges are already proved above. Include
            // them in the interval, retaining the named recapturing pawn's loss.
            material(open, open.captures, offsets).foreach { cost =>
              offerAnswer("occupied_pawn_destination", List(
                Section(List(Action(blocked.ref(0), MoveWorkProjection.Detail.PawnBlock(square, pawn, square)))),
                Section(List(PieceRoute(open.path, pawn, List(advance)), cost,
                  MaterialEvents(open.path, cost.cost.events.filter(row => named.exists(_.plyOffset == row.plyOffset)))),
                  offsets.map(open.ref))))
            }

    // The same opposing piece uses different first-reply squares. Check the
    // other landing square without pretending its move is illegal or forced.
    for (changed, n) <- arms.zipWithIndex if rootNotInCheck && arms.forall(_.quiet(1)) do
      val peer = arms(1 - n)
      val controller = changed.root.get
      for
        reply <- changed.mover(1).filter(_.role.name != "king").toList
        if peer.mover(1).contains(reply)
        square = peer.path.moves(1).slice(2, 4)
        actualSquare = changed.path.moves(1).slice(2, 4)
        if square != actualSquare && arms.forall(_.position(1).exists(_.empty(square))) &&
          changed.controlSquare(0, controller, square).contains(false) && changed.controlSquare(1, controller, square).contains(true) &&
          peer.controlSquare(1, controller, square).contains(false) && changed.controlSquare(1, controller, actualSquare).contains(false)
        legal <- changed.position(1).flatMap(_.view.legalRows).toList.flatten.find(_.move == peer.path.moves(1)).toList
        afterRoot <- packet.assembly.lineReplays.get(changed.path.line.ref).flatMap(_.legalSteps.headOption).map(_.after).toList
        alternative <- afterRoot.legalMoves.find(_.toUci.uci == legal.move).toList
        current <- changed.at(controller, 1).toList
        if alternative.after.legalMoves.exists(move => move.orig.key == current.square.key && move.dest.key == square && move.capture.nonEmpty)
      do
        changed.proofs ++= changed.position(1).toList.flatMap(_.sourceRefs(List(legal.sourceId)))
        offerAnswer("controlled_destination", List(
          Section(List(ControlState(changed.ref(0), current, None, square, present = true),
            PieceRoute(changed.path, reply, List(1))), List(changed.ref(0), changed.ref(1))),
          Section(List(ControlState(peer.ref(0), peer.at(controller, 1).get, None, square, present = false),
            PieceRoute(peer.path, reply, List(1))), List(peer.ref(0), peer.ref(1)))))

    // Answer an actual root attack and the opponent's first-reply attack on
    // the same role in the other line. Neither root is claimed to cause the
    // entire continuation; an intervening checking exchange stays with it.
    for (opening, n) <- arms.zipWithIndex if rootNotInCheck && arms.forall(_.quiet(0)) do
      val peer = arms(1 - n)
      for
        target <- opening.originals.filter(p => p.side != opening.root.get.side && opening.gained(p))
        ownEvasion <- opening.singleEvasion(opening.root.get, target).toList
        reply <- peer.mover(1).filter(_.side != peer.root.get.side).toList
        otherTarget <- peer.originals.filter(p => p.side == peer.root.get.side && p.role == target.role &&
          peer.control(1, reply, p).contains(false) && peer.covers(2, reply, p))
        otherEvasion <- peer.singleEvasion(reply, otherTarget, 1).toList
      do
        val plans = List((opening, 0, target, ownEvasion), (peer, 1, otherTarget, otherEvasion)).map { (a, start, target, end) =>
          val events = a.captures.filter(_.plyOffset <= end)
          val required = (List(0, start) ++ events.map(_.plyOffset) :+ end).distinct
          val material = events match
            case Nil => true
            case List(gain, loss) => gain.plyOffset > start && loss.plyOffset < end &&
              loss.plyOffset == gain.plyOffset + 1 && a.recovery(gain).contains(loss) &&
              gain.originalVictim.map(_.role) == loss.originalVictim.map(_.role) &&
              gain.originalActor.exists(actor => fits(a.account(gain, actor), events, required))
            case _ => false
          val checksClosed = (0 to end).filter(a.checks).forall(i => required.contains(i) && required.contains(i + 1))
          val valid = material && checksClosed && !a.checks(start)
          (a, start, target, end, events, required, valid)
        }
        if plans.forall(_._7) then
          val sections = plans.map { (a, start, target, end, events, required, _) =>
            val cost = Option.when(events.nonEmpty)(material(a, events, required)).flatten
            Option.when(events.isEmpty || cost.nonEmpty)(Section(List(TargetResponse(a.path, start,
              a.at(target, start + 1).get, a.mover(start).get, target, Some(end), cost, survives = false)), required.map(a.ref)))
          }
          if sections.forall(_.nonEmpty) then offerAnswer("opening_counterplay", sections.flatten)

    // The same physical move checks the old king square, but not the square
    // reached by moving the king first. Neutral intervening trades stay owned.
    for (checked, n) <- arms.zipWithIndex if rootNotInCheck do
      val quiet = arms(1 - n)
      for
        king <- quiet.root.filter(_.role.name == "king").toList
        actor <- checked.originals.filter(p => p.side != king.side && checked.gained(p))
        checking <- checked.singleEvasion(checked.root.get, actor).filter(checked.checks).toList
        if checked.mover(checking + 1).contains(king) && checked.quiet(checking - 1)
        later <- quiet.route(actor).find(i => i > 0 && quiet.path.moves(i) == checked.path.moves(checking)).toList
        if !quiet.checks(later) && quiet.quiet(0) && quiet.stationary(actor, later - 1) &&
          (1 to later + 1).forall(i => quiet.at(king, i) == quiet.at(king, 1)) &&
          checked.controlSquare(checking + 1, actor, king.square.key).contains(true) &&
          quiet.controlSquare(later + 1, actor, king.square.key).contains(true) &&
          quiet.control(later + 1, actor, king).contains(false)
        gain <- checked.captures.find(e => e.plyOffset > checking + 1 && e.originalActor.contains(actor)).toList
        victim <- gain.originalVictim.toList
        if checked.control(checking, actor, victim).contains(false) &&
          (checking + 1 to gain.plyOffset).forall(i => checked.covers(i, actor, victim)) &&
          checked.captures.filter(_.plyOffset <= gain.plyOffset) == List(gain) &&
          fits(checked.account(gain, actor), List(gain), List(0, checking, checking + 1, gain.plyOffset)) &&
          !(0 to gain.plyOffset).filter(_ != checking).exists(checked.checks)
      do
        val background = quiet.captures.filter(_.plyOffset <= later)
        val neutral = background.size % 2 == 0 && background.grouped(2).forall {
          case List(a, b) => b.plyOffset == a.plyOffset + 1 && quiet.recovery(a).contains(b) &&
            a.originalVictim.map(_.role) == b.originalVictim.map(_.role)
          case _ => false
        }
        val closedChecks = (0 to later).filter(quiet.checks).forall(i => background.exists(_.plyOffset == i) && background.exists(_.plyOffset == i + 1))
        if neutral && closedChecks then
          val checkedOffsets = List(0, checking, checking + 1, gain.plyOffset)
          val quietOffsets = (List(0, later) ++ background.map(_.plyOffset)).distinct.sorted
          val quietCost = Option.when(background.nonEmpty)(material(quiet, background, quietOffsets)).flatten
          if background.isEmpty || quietCost.nonEmpty then material(checked, List(gain), checkedOffsets).foreach { cost =>
            val ending = Option.when(later == quiet.path.moves.size - 1)(RecordedEnd(quiet.ref(later))).toList
            offerAnswer("check_status_comparison", List(
              Section(List(TargetResponse(checked.path, 0, checked.at(actor, 1).get, checked.root.get, actor,
                Some(checking), Some(cost), survives = false),
                CheckStatus(checked.ref(checking), checked.at(king, checking + 1).get, checking = true,
                  reply = Some(checked.ref(checking + 1))), MaterialEvents(checked.path, cost.cost.events)), checkedOffsets.map(checked.ref)),
              Section(List(PieceRoute(quiet.path, king, List(0)), PieceRoute(quiet.path, actor, List(later)),
                CheckStatus(quiet.ref(later), quiet.at(king, later + 1).get, checking = false)) ++ quietCost ++ ending,
                quietOffsets.map(quiet.ref))))
          }

    // Compare the same physical root actions and the recorded replies after
    // both have been played. No claim that their order forces those replies.
    val delayed = arms.zipWithIndex.map { (a, n) =>
      val peer = arms(1 - n)
      a.route(peer.root.get).find(i => i > 0 && a.path.moves(i) == peer.path.moves.head)
    }
    // A later meeting can erase the early arrangement difference, even when
    // its captures need a richer explanation than the quiet meeting below.
    val meetings = (for
      left <- arms.head.path.positions.indices.drop(1)
      right <- arms.last.path.positions.indices.drop(1)
      first = arms.head.path.positions(left).fen
      second = arms.last.path.positions(right).fen
      if first.split(' ').take(4).sameElements(second.split(' ').take(4))
    yield List(left - 1, right - 1)).toList
    // Earliest common board when both records are read to the same horizon,
    // then earliest occurrence within it. Input row order cannot choose it.
    val earliest = meetings.sortBy(ends => (ends.max, ends.sum)).headOption.toList.flatMap { first =>
      meetings.filter(ends => ends.max == first.max && ends.sum == first.sum)
    }
    val meeting = earliest match
      case only :: Nil => Some(only)
      case _ => None
    if delayed.forall(_.nonEmpty) && meetings.isEmpty then
      val otherAt = delayed.flatten
      val ends = otherAt.map(_ + 1)
      val sharedAttack = arms.zipWithIndex.exists { (a, n) =>
        val peer = arms(1 - n)
        val start = otherAt(1 - n)
        val actor = a.root.get
        a.originals.exists(target => target.side != actor.side && a.gained(target) &&
          a.singleEvasion(actor, target).exists(a.quiet) &&
          peer.control(start, actor, target).contains(false) && peer.covers(start + 1, actor, target) &&
          peer.singleEvasion(actor, target, start).exists(peer.quiet))
      }
      val differentRook = arms.exists(_.path.certified.exists(_._2.lineEvents.exists(e =>
        e.plyOffset == 0 && e.kind == LineEventKind.Castling))) && arms.head.originals.exists { piece =>
        piece.role.name == "rook" && piece.side == arms.head.root.get.side &&
          arms.head.at(piece, arms.head.path.moves.size).exists { current =>
            arms.last.originals.exists(other => other != piece && other.side == piece.side && other.role == piece.role &&
              arms.last.at(other, arms.last.path.moves.size).contains(current))
          }
      }
      val held = arms.zip(ends).forall { (a, end) =>
        a.quiet(end) && arms.forall(root => a.at(root.root.get, end + 1) == root.at(root.root.get, 1))
      }
      // These already-observed mechanisms compete with a bare route contrast.
      // Their dedicated readings remain detailed until their wording is verified.
      if held && !sharedAttack && !differentRook then
        // The same delayed mover may be under an existing attack. State the
        // verified root difference before its routes, and retain the actual
        // attacker's departure if it releases that piece before the late move.
        // This compares recorded positions; it does not certify later safety.
        val escapes = arms.zipWithIndex.flatMap { (early, n) =>
          val late = arms(1 - n)
          val target = early.root.get
          for
            attacker <- early.originals.filter(_.side != target.side)
            if early.covers(0, attacker, target) && early.control(1, attacker, target).contains(false) &&
              late.covers(1, attacker, target) && late.route(target).headOption.contains(otherAt(1 - n))
            actor <- early.at(attacker, 1).toList
            moved <- early.at(target, 1).toList
            otherActor <- late.at(attacker, 1).toList
            waiting <- late.at(target, 1).toList
          yield
            val contrast = CommentaryAnswer.ControlContrast(early.ref(0), actor, Some(moved), moved.square.key, false,
              late.ref(0), otherActor, Some(waiting), waiting.square.key, true)
            val departure = late.route(attacker).filter(at => at > 0 && at < otherAt(1 - n) &&
              (1 to at).forall(late.covers(_, attacker, target)) &&
              late.control(at + 1, attacker, target).contains(false)).headOption.toList.flatMap { at =>
              for
                before <- late.at(attacker, at).toList
                after <- late.at(attacker, at + 1).toList
                subject <- late.at(target, at).toList
              yield CommentaryAnswer.AttackWithdrawal(late.ref(at), before, after, subject)
            }
            (n, contrast, departure)
        }
        val routes = arms.head.originals.flatMap { piece =>
          val steps = arms.zip(ends).map((a, end) => a.route(piece).filter(_ <= end))
          Option.when(steps.zip(arms).map((moves, a) => moves.map(a.path.moves)).distinct.size == 2)(piece -> steps)
        }
        val opponents = routes.filter(_._1.side != arms.head.root.get.side)
        val comparable = opponents.filter { (piece, steps) =>
          steps.forall(_.nonEmpty) && arms.zip(ends).map((a, end) => a.at(piece, end + 1)).distinct.size == 2
        }
        val chosen = if comparable.nonEmpty then comparable else opponents
        val sections = arms.zipWithIndex.map { (a, n) =>
          val used = mutable.Set.empty[Int]
          val replies = chosen.flatMap { (piece, steps) =>
            val moves = steps(n).filterNot(used)
            used ++= moves
            Option.when(moves.nonEmpty)(piece -> moves)
          }
          val setup = routes.filter((piece, _) => piece.side == a.root.get.side && !arms.exists(_.root.contains(piece)))
            .flatMap { (piece, steps) =>
              val moves = steps(n).filter(_ < otherAt(n))
              Option.when(moves.nonEmpty)(piece -> moves)
            }
          val orderedSetup = setup.isEmpty || (setup.size == 1 && replies.size == 1 &&
            setup.head._2.last < replies.head._2.head && replies.head._2.last < otherAt(n))
          Option.when(replies.nonEmpty && orderedSetup) {
            val refs = (0 to ends(n)).map(a.ref).toList
            val moves = List(a.root.get -> List(0), arms(1 - n).root.get -> List(otherAt(n))) ++ setup ++ replies
            val conditions: List[Statement] = escapes.flatMap { (owner, contrast, departure) =>
              if owner == n then List(contrast) else departure
            }
            Section(conditions ++ (Continuation(refs) :: moves.map((piece, steps) => PieceRoute(a.path, piece, steps))), refs)
          }
        }
        if sections.forall(_.nonEmpty) then offerAnswer("observed_move_order", sections.flatten)

    // Compare admitted FEN board fields; clocks and repetition history
    // remain local to each path. A shared placement is not a shared result.
    meeting.filter(ends => arms.zip(ends).forall((a, end) => a.quiet(end))).foreach { ends =>
      arms.zip(ends).foreach((a, end) => a.position(end + 1))
      val sequences = arms.zip(ends).map((a, end) => (0 to end).map(a.ref).toList)
      offerAnswer("observed_arrangement_convergence", List(Section(
        List(Convergence(sequences.head, sequences.last, fullState = false)), sequences.flatten)))
    }

    // A common destination is only the subject of the comparison. Read the
    // actual interaction along each route, including a capturing response.
    case class RouteAnswer(arm: Arm, arrival: Int, steps: List[Int],
        targets: Map[Int, List[Piece]], replies: List[(Int, Piece, Int)],
        incoming: Map[Int, Piece], use: Option[Event], end: Int, costs: List[Event],
        offsets: List[Int], trades: List[List[Event]], complete: Boolean)
    val beforeRoutes = arms.map(_.proofs.toSet)
    for focus <- arms.flatMap(_.root).distinct if rootNotInCheck do
      val arrivals = arms.map(a => a.route(focus).flatMap(n => a.at(focus, n + 1).map(p => n -> p.square.key)))
      val intersections = for (left, square) <- arrivals.head; (right, other) <- arrivals.last if square == other
        yield (left, right, square)
      val first = intersections.filterNot((left, right, _) => intersections.exists((a, b, _) =>
        a <= left && b <= right && (a < left || b < right)))
      if first.size == 1 then
        val (left, right, square) = first.head
        val answers = arms.zip(List(left, right)).map { (a, arrival) =>
          val steps = a.route(focus).filter(_ <= arrival)
          val targets = steps.map(n => n -> a.originals.filter(p => p.side != focus.side &&
            a.control(n, focus, p).contains(false) && a.covers(n + 1, focus, p))).toMap
          val replies = steps.flatMap(n => targets(n).filter(p => a.evaded(n + 1, p).contains(focus)).map(p => (n, p, n + 1)))
          val incoming = steps.flatMap(n => a.evaded(n, focus).map(n -> _)).toMap
          val use = a.route(focus).find(_ > arrival).flatMap(n => a.captures.find(e => e.plyOffset == n &&
            e.originalActor.contains(focus) && e.originalVictim.exists(target =>
              (arrival + 1 to n).forall(a.covers(_, focus, target)))))
          val needsReply = targets(arrival).nonEmpty || a.checks(arrival) || a.captures.exists(_.plyOffset == arrival + 1)
          var end = math.max(math.min(a.path.moves.size - 1, arrival + (if needsReply then 1 else 0)), use.map(_.plyOffset).getOrElse(arrival))
          // A piece still on its arrival square may be captured without making
          // another move. Its observed loss qualifies this route's endpoint.
          a.captures.find(e => e.plyOffset > arrival && e.originalVictim.contains(focus) &&
            !a.route(focus).exists(n => n > arrival && n < e.plyOffset))
            .foreach(e => end = math.max(end, e.plyOffset))
          var complete = true
          var pending = true
          val offsets = mutable.Set(0, arrival)
          offsets ++= steps
          offsets ++= replies.map(_._3)
          offsets ++= incoming.keys.flatMap(n => List(n - 1, n))
          if arrival < end then offsets += arrival + 1
          while pending do
            val old = end
            a.captures.filter(_.plyOffset <= end).foreach { seed =>
              val cost = a.account(seed, focus)
              complete &&= cost.complete
              offsets ++= cost.offsets
              end = math.max(end, cost.offsets.max)
            }
            pending = old != end
          val costs = a.captures.filter(_.plyOffset <= end)
          val trades = mutable.ListBuffer.empty[List[Event]]
          costs.foreach { loss =>
            val prior = trades.indexWhere(chain => a.recovery(chain.last).contains(loss) &&
              chain.last.originalActor.exists(p => !a.route(p).exists(n =>
                n > chain.last.plyOffset && n < loss.plyOffset)))
            if prior < 0 then trades += List(loss) else trades(prior) = trades(prior) :+ loss
          }
          complete &&= !(0 to end).exists(a.checks)
          // An account can extend beyond the initial route endpoint. Its
          // actual first reply remains attached even when other quiet moves
          // belong only to the detailed prefix.
          if arrival < end then offsets += arrival + 1
          RouteAnswer(a, arrival, steps, targets, replies, incoming, use, end, costs,
            offsets.toList.sorted, trades.toList, complete)
        }
        val chainAccounts = answers.forall(_.trades.forall(_.size >= 2))
        // Group actual pawn gains around the same physical piece exchange.
        // The existing account has already closed every capture in the prefix,
        // including a countergain after the focal piece is removed.
        val exchanges = answers.map { r =>
          val a = r.arm
          for
            gain <- r.costs.find(e => e.plyOffset == r.arrival && e.originalActor.contains(focus) &&
              e.originalVictim.exists(_.role == focus.role))
            loss <- a.recovery(gain).filter(e => e.plyOffset == gain.plyOffset + 1 && e.originalVictim.contains(focus))
          yield (gain, loss)
        }
        val pawnAccounts = !chainAccounts && exchanges.forall(_.nonEmpty) &&
          exchanges.flatten.map(_._1.originalVictim).distinct.size == 1 && answers.zip(exchanges).forall { (r, exchange) =>
            val a = r.arm
            val (gain, loss) = exchange.get
            val enemy = gain.originalVictim.get
            val nextUse = a.route(focus).find(_ > 0).flatMap(n => r.costs.find(e => e.plyOffset == n &&
              e.originalVictim.exists(p => p.role.name == "pawn" && a.gained(p) && (1 to n).forall(a.covers(_, focus, p)))))
            a.root.contains(focus) && nextUse.nonEmpty && a.balanced(r.costs) && r.end <= loss.plyOffset + 1 &&
              r.costs.filterNot(e => e == gain || e == loss).forall(e => e.originalVictim.exists(_.role.name == "pawn") &&
                (if e.plyOffset < gain.plyOffset then e.originalActor.exists(p => p == focus || p == enemy)
                else e.plyOffset == loss.plyOffset + 1 && e.originalActor.exists(_.side == focus.side)))
          }
        val routesDiffer = answers.map(r => r.steps.map(r.arm.path.moves)).distinct.size == 2
        val workDiffers = answers.map(r => (
          r.replies.map((n, p, reply) => (p, r.arm.path.moves(n), r.arm.path.moves(reply))),
          r.incoming.toList.sortBy(_._1).map((n, p) => (p, r.arm.path.moves(n - 1))),
          r.costs.map(e => (e.originalActor, e.originalVictim)))).distinct.size == 2
        if routesDiffer && workDiffers && answers.forall(_.complete) && answers.exists(_.replies.nonEmpty) &&
            (chainAccounts || pawnAccounts) then
          val escaped = Option.when(pawnAccounts)(arms.head.originals.filter(p => p.side != focus.side &&
            arms.forall(a => a.covers(0, p, focus) && a.control(1, p, focus).contains(false)))).getOrElse(Nil)
          val sections = answers.map { answer =>
            val a = answer.arm
            val offsets = answer.offsets
            val cost = Option.when(answer.costs.nonEmpty)(material(a, answer.costs, offsets)).flatten
            Option.when(answer.costs.isEmpty || cost.nonEmpty) {
              val arrivals = List[Statement](PieceRoute(a.path, focus, answer.steps))
              val evasions = answer.incoming.toList.sortBy(_._1).map { (offset, attacker) =>
                TargetResponse(a.path, offset - 1, a.at(focus, offset).get, attacker, focus,
                  Some(offset), None, survives = false)
              }
              val attacks = answer.targets.toList.sortBy(_._1).flatMap { (offset, targets) =>
                targets.map { target =>
                  val escape = answer.replies.find((start, piece, _) => start == offset && piece == target).map(_._3)
                  TargetResponse(a.path, offset, a.at(target, offset + 1).get, focus, target, escape, None, survives = false)
                }
              }
              val initial = Option.when(escaped.size == 1)(Action(a.ref(0),
                MoveWorkProjection.Detail.Evades(a.at(focus, 1).get, escaped.head))).toList
              val ending = Option.when(answer.arrival == a.path.moves.size - 1)(RecordedEnd(a.ref(answer.arrival))).toList
              // Every capture in this prefix has already passed account().
              // Keep the whole interval, including independent exchanges and
              // a later loss of the focal piece; no wording decides omissions.
              val account: List[Statement] = cost.toList ++ cost.toList.map(value =>
                MaterialEvents(a.path, value.cost.events.filter(row => answer.costs.exists(event =>
                  event.plyOffset == row.plyOffset && (event.originalActor.contains(focus) || event.originalVictim.contains(focus))))))
              val content = if account.nonEmpty then account else List(Continuation(offsets.map(a.ref)))
              List(Section(arrivals ++ initial ++ evasions ++ attacks ++ ending),
                Section(content, offsets.map(a.ref)))
            }
          }
          if sections.forall(_.nonEmpty) then offerAnswer("observed_route_use", sections.flatten.flatten,
            answers.flatMap(answer => continuation((0 to answer.end).map(answer.arm.ref).toList)))

    // Unselected route attempts must not alter another question's evidence.
    arms.zip(beforeRoutes).foreach { (a, prior) => a.proofs.clear(); a.proofs ++= prior }

    // First answer how the same opposed pieces are handled in each line.
    // The answers may be an exchange, an evasion or a used defense; their
    // shared result is then stated once, with the actual replies kept together.
    case class PairAnswer(arm: Arm, events: List[Event], evasion: Option[Int],
        supportChange: Option[(Piece, Option[Int])] = None, offsets: List[Int] = Nil)
    val opposed = arms.flatMap(a => a.captures.sliding(2).collect {
      case List(first, second) if second.plyOffset == first.plyOffset + 1 &&
          second.originalVictim == first.originalActor =>
        Set(first.originalActor.get, first.originalVictim.get)
    }.toList).distinct
    for pieces <- opposed if rootNotInCheck do
      val own = pieces.find(_.side == arms.head.root.get.side).get
      val enemy = pieces.find(_.side != own.side).get
      // Compare the actual recapturers in both lines. The defender that no
      // longer recaptures can supply the missing condition in the other line.
      val providers = arms.flatMap(_.captures.filter(_.originalVictim.contains(enemy))
        .flatMap(_.originalActor)).filter(p => p != own && p.side == own.side).distinct
      val withdrawn = (for
        (a, n) <- arms.zipWithIndex
        provider <- providers
        loss <- a.captures.find(_.originalVictim.contains(own)).toList
        offset <- a.route(provider).filter(_ < loss.plyOffset)
        if a.covers(0, provider, own) && a.covers(offset, provider, own) &&
          a.control(offset + 1, provider, own).contains(false) && arms(1 - n).covers(1, provider, own)
      yield (provider, a, offset)) match
        case only :: Nil => Some(only)
        case _ => None
      val plans = arms.map { a =>
        val root = a.root.get
        val trades = a.captures.sliding(2).collect {
          case events @ List(first, second) if second.plyOffset == first.plyOffset + 1 &&
              second.originalVictim == first.originalActor && events.flatMap(_.originalVictim).toSet == pieces => events
        }.toList.flatMap { events =>
          val first = events.head
          val last = events.last
          val direct = root == own && first.plyOffset == 0 && first.originalActor.contains(root)
          val attacking = root == own && a.gained(enemy) && (1 to first.plyOffset).forall(a.covers(_, root, enemy))
          val confronted = root == own && a.covers(0, enemy, own) && (1 to first.plyOffset).forall(a.covers(_, enemy, own))
          val support = a.supportUse(own).exists(use => use.targetCapture.id == first.id && use.recapture.id == last.id) &&
            a.covers(0, enemy, own)
          // The shared subject and its recapturer need not move at the root.
          // Use the same admitted attack, support-use and closed account proof.
          val initialAttack = arms.exists(keep => keep.at(own, keep.path.moves.size).nonEmpty && keep.quiet(0) &&
            keep.singleEvasion(enemy, own, -1).exists(end => end + 1 < keep.path.moves.size && keep.quiet(end))) &&
            first.originalVictim.contains(own) && a.stationary(own, first.plyOffset - 1) &&
            (0 to first.plyOffset).forall(a.covers(_, enemy, own)) && last.originalActor.exists { provider =>
              LineContinuationInvestigation.directSupportUse(packet, a.path, paths, last.plyOffset + 1, provider, own)
                .uses.exists(use => use.targetCapture.id == first.id && use.recapture.id == last.id)
            }
          val changedSupport = withdrawn.flatMap { (provider, departing, offset) =>
            val leaves = a eq departing
            Option.when(first.originalVictim.contains(own) && a.stationary(own, first.plyOffset - 1) &&
              last.originalActor.exists { recapturer =>
                (if leaves then recapturer != provider && offset < first.plyOffset &&
                  (0 to offset).forall(a.covers(_, provider, own)) &&
                  (offset + 1 to first.plyOffset).forall(a.control(_, provider, own).contains(false))
                else recapturer == provider) &&
                  (0 to first.plyOffset).forall(a.covers(_, recapturer, own)) &&
                  LineContinuationInvestigation.directSupportUse(packet, a.path, paths, last.plyOffset + 1, recapturer, own)
                    .uses.exists(use => use.targetCapture.id == first.id && use.recapture.id == last.id)
              })(provider -> Option.when(leaves)(offset))
          }
          val cost = a.account(first, own)
          // ponytail: only a closed two-capture account is verbalized here.
          // Longer claim-changing chains keep their existing detailed account.
          Option.when((direct || attacking || confronted || support || initialAttack || changedSupport.nonEmpty) &&
            fits(cost, events, (0 to last.plyOffset).toList) &&
            a.captures.filter(_.plyOffset <= last.plyOffset) == events &&
            !(0 until first.plyOffset).exists(a.checks) && !a.checks(last.plyOffset)) {
            val required = (List(0) ++ cost.offsets ++ changedSupport.flatMap(_._2) ++
              Option.when(attacking && first.plyOffset > 1)(1)).distinct.sorted
            PairAnswer(a, events, None, changedSupport, required)
          }
        }
        val escape = Option.when(a.path.moves.size > 1 && a.quiet(0) && a.at(own, a.path.moves.size).nonEmpty)(
          a.singleEvasion(enemy, own, -1).filter(end => end + 1 < a.path.moves.size && a.quiet(end))
            .map(end => PairAnswer(a, Nil, Some(end)))).flatten
        val evasion = Option.when(root == own && a.gained(enemy))(a.singleEvasion(root, enemy).filter(a.quiet)).flatten
          .map(end => PairAnswer(a, Nil, Some(end)))
        (trades ++ escape ++ evasion) match
          case only :: Nil => Some(only)
          case _ => None
      }
      if plans.forall(_.nonEmpty) && plans.flatten.exists(_.events.nonEmpty) then
        val answers = plans.flatten
        val sections = answers.map { answer =>
          val a = answer.arm
          answer.evasion match
            case Some(end) if a.mover(end).contains(own) =>
              val refs = List(0, end, a.path.moves.size - 1).distinct.map(a.ref)
              Some(Section(List(Continuation(List(a.ref(0), a.ref(end)).distinct),
                Action(a.ref(end), MoveWorkProjection.Detail.Evades(a.at(own, end + 1).get, a.at(enemy, end).get)),
                PiecePresence(a.ref(a.path.moves.size - 1), own, a.at(own, a.path.moves.size).get)), refs))
            case Some(end) =>
              Some(Section(List(TargetResponse(a.path, 0, a.at(enemy, 1).get, own, enemy,
                Some(end), None, survives = false))))
            case None =>
              val first = answer.events.head
              val last = answer.events.last
              val offsets = (answer.offsets ++ Option.when(first.plyOffset > 1 && a.gained(enemy))(1)).distinct.sorted
              material(a, answer.events, offsets).map { cost =>
                val change = answer.supportChange.toList.map { (provider, departure) =>
                  val at = departure.getOrElse(0)
                  ControlState(a.ref(at), a.at(provider, at + 1).get, a.at(own, at + 1),
                    a.at(own, at + 1).get.square.key, present = departure.isEmpty)
                }
                val support = Option.when(answer.supportChange.isEmpty)(a.supportUse(own).find(use =>
                  use.targetCapture.id == first.id && use.recapture.id == last.id)).flatten
                val use: List[Statement] = support.toList.flatMap(value => List(
                  Action(a.ref(0), MoveWorkProjection.Detail.Support(a.at(a.root.get, 1).get,
                    a.at(own, 1).get, Nil, gained = true)), SupportRecapture(a.ref(0), a.path, value)))
                val attack = Option.when(first.plyOffset > 0 && change.isEmpty && support.isEmpty && a.gained(enemy))(
                  TargetResponse(a.path, 0, a.at(enemy, 1).get, a.root.get, enemy, None, None, survives = false)).toList
                Section(change ++ use ++ attack ++ List(cost, a.pieceExchange(answer.events)), offsets.map(a.ref))
              }
        }
        if sections.forall(_.nonEmpty) then offerAnswer("contested_pair_response", sections.flatten,
          answers.filter(_.events.nonEmpty).flatMap(answer =>
            continuation((0 to answer.events.last.plyOffset).map(answer.arm.ref).toList)))


    lazy val sharedExchanges = opposed.flatMap { pieces =>
      val found = arms.map(a => a.captures.sliding(2).collect {
        case es @ List(first, last) if last.plyOffset == first.plyOffset + 1 &&
            last.originalVictim == first.originalActor && es.flatMap(_.originalVictim).toSet == pieces => es
      }.toList)
      Option.when(found.forall(_.size == 1))(pieces -> found.map(_.head))
    }
    if choices.isEmpty && rootNotInCheck then locally {
      // Reuse a common exchange subject only when no more specific question
      // already closes. Each line owns its own conditions and material account.
      // ponytail: a unique complete comparison is required; importance among
      // multiple complete questions remains unresolved and keeps the fallback.
      val exchanges = sharedExchanges
      val accounts = exchanges.map { (pieces, es) =>
        val own = pieces.find(_.side == arms.head.root.get.side).get
        arms.zip(es).map((a, events) => a.account(events.head, own))
      }
      val groups = exchanges.indices.map { seed =>
        val selected = mutable.Set(seed)
        var changed = true
        while changed do
          val more = exchanges.indices.filterNot(selected).filter(i => selected.exists(j =>
            arms.indices.exists(n => exchanges(i)._2(n).exists(accounts(j)(n).events.contains))))
          changed = more.nonEmpty
          selected ++= more
        selected.toList.sorted
      }.toList.distinct
      for group <- groups if rootNotInCheck do
        val ordered = group.sortBy(i => exchanges(i)._2.head.head.plyOffset)
        val pairs = arms.indices.map(n => ordered.map(i => exchanges(i)._2(n))).toList
        val costs = arms.indices.map(n => ordered.map(i => accounts(i)(n))).toList
        val events = pairs.map(_.flatten)
        val ends = costs.map(_.flatMap(_.offsets).max)
        val subjects = ordered.flatMap(i => exchanges(i)._1).toSet
        val contrasted = ordered.exists(i => arms.zip(exchanges(i)._2).map((a, es) =>
          (es.head.originalActor, es.last.originalActor, a.checks(es.head.plyOffset), a.checks(es.last.plyOffset))).distinct.size > 1)
        val closed = arms.indices.forall(n => costs(n).forall(c => c.complete && c.dutyCosts.isEmpty) &&
          costs(n).flatMap(_.events).toSet == events(n).toSet && events(n).distinct.size == events(n).size &&
          arms(n).captures.filter(_.plyOffset <= ends(n)) == events(n) &&
          events(n).map(_.plyOffset) == events(n).map(_.plyOffset).sorted)
        val connected = arms.indices.map { n =>
          val a = arms(n)
          val root = a.root.get
          val people = subjects ++ pairs(n).flatMap(_.last.originalActor)
          people(root) || people.exists(p => p != root &&
            a.control(0, root, p).nonEmpty && a.control(1, root, p).nonEmpty &&
            a.control(0, root, p) != a.control(1, root, p)) ||
            pairs(n).exists(es => es.head.plyOffset == 2 && a.mover(1).exists(p =>
              es.head.originalActor.exists(target => !a.control(1, p, target).contains(true) && a.covers(2, p, target))))
        }.toList
        val conditions = arms.map(_ => mutable.ListBuffer.empty[Statement])
        val conditionOffsets = arms.map(_ => mutable.Set.empty[Int])
        val explained = arms.indices.map(n => subjects(arms(n).root.get) ||
          pairs(n).exists(_.last.originalActor == arms(n).root)).toArray
        val sameRoot = arms.flatMap(_.root).distinct
        val evasion = sameRoot match
          case root :: Nil => arms.head.originals.filter(p => p.side != root.side && arms.forall(a =>
            a.covers(0, p, root) && a.control(1, p, root).contains(false)))
          case _ => Nil
        if evasion.size == 1 then
          val root = sameRoot.head
          val attacker = evasion.head
          arms.zipWithIndex.foreach { (a, n) =>
            conditions(n) += Action(a.ref(0), MoveWorkProjection.Detail.Evades(a.at(root, 1).get, attacker))
            conditionOffsets(n) += 0
          }
          if pairs.flatten.exists(_.last.originalActor.contains(root)) then arms.indices.foreach(n => explained(n) = true)
        for (i, stage) <- ordered.zipWithIndex; (a, n) <- arms.zipWithIndex do
          val pieces = exchanges(i)._1
          val es = exchanges(i)._2(n)
          val first = es.head
          val recapturer = es.last.originalActor.get
          val arrivals = (pieces + recapturer).toList.flatMap(p =>
            a.route(p).filter(_ < first.plyOffset).lastOption.map(offset => p -> offset))
          val incoming = (0 until first.plyOffset).flatMap(offset => for
            actor <- a.mover(offset)
            target <- first.originalActor
            if actor.side != target.side && a.control(offset, actor, target).contains(false) &&
              (offset + 1 to first.plyOffset).forall(a.covers(_, actor, target))
          yield (offset, actor, target)).toList
          val rootChanges = pieces.toList.filter(p => p != a.root.get && p.side == a.root.get.side &&
            a.control(0, a.root.get, p).nonEmpty && a.control(1, a.root.get, p).nonEmpty &&
            a.control(0, a.root.get, p) != a.control(1, a.root.get, p))
          val before = mutable.ListBuffer.empty[(Int, Statement)]
          if stage == 0 && !conditionOffsets(n)(0) && rootChanges.size == 1 then
            val target = rootChanges.head
            val gained = a.covers(1, a.root.get, target)
            if gained || a.covers(0, a.root.get, target) then
              explained(n) = true
              before += 0 -> ControlState(a.ref(0), a.at(a.root.get, 1).get, a.at(target, 1),
                a.at(target, 1).get.square.key, present = gained)
          incoming.filterNot(t => before.exists(_._1 == t._1) || conditionOffsets(n)(t._1) ||
            events(n).exists(_.plyOffset == t._1)).foreach { (offset, actor, target) =>
            if offset == 0 || offset == 1 && first.plyOffset == 2 then explained(n) = true
            before += offset -> TargetResponse(a.path, offset, a.at(target, offset + 1).get,
              actor, target, None, None, survives = false)
          }
          arrivals.distinct.sortBy(_._2).filterNot((_, offset) => offset == 0 || before.exists(_._1 == offset) ||
            conditionOffsets(n)(offset) || events(n).exists(_.plyOffset == offset)).foreach { (piece, offset) =>
            before += offset -> PieceRoute(a.path, piece, List(offset))
          }
          before.toList.sortBy(_._1).foreach { (offset, statement) =>
            conditionOffsets(n) += offset
            conditions(n) += statement
          }
          if stage == 0 || first.plyOffset == 0 then conditionOffsets(n) += 0
        val closedReplies = arms.indices.forall(n =>
          costs(n).flatMap(_.offsets).forall(offset => offset >= 0 && offset <= ends(n)) &&
            (0 to ends(n)).filter(arms(n).checks).forall(_ + 1 <= ends(n)))
        if contrasted && closed && connected.forall(identity) && explained.forall(identity) && closedReplies then
          val sections = arms.zipWithIndex.map { (a, n) =>
            val checks = (0 to ends(n)).filter(a.checks).flatMap(offset => List(offset, offset + 1))
            val offsets = (List(0) ++ costs(n).flatMap(_.offsets) ++ checks).distinct.sorted
            material(a, events(n), offsets).map { cost =>
              val context = Option.when(conditions(n).nonEmpty)(Section(conditions(n).toList)).toList
              context :+ Section(List(cost, a.pieceExchange(events(n))), offsets.map(a.ref))
            }
          }
          if sections.forall(_.nonEmpty) then offerAnswer("contested_pair_response", sections.flatten.flatten,
            arms.zipWithIndex.flatMap((a, n) => continuation((0 to ends(n)).map(a.ref).toList)))
    }

    if choices.isEmpty && rootNotInCheck then
      // A shared subject brings its required earlier exchanges with it.
      // ponytail: adjacent exchanges only; extend when non-pair accounts can
      // be expressed and validated without losing their compensation.
      val shared = sharedExchanges
      val cached = mutable.Map.empty[(Int, String), Account]
      def account(n: Int, es: List[Event]): Account = cached.getOrElseUpdate((n, es.head.id),
        arms(n).account(es.head, es.flatMap(_.originalVictim).find(_.side == arms(n).root.get.side).get))
      val built = mutable.Set.empty[List[List[Int]]]
      for seed <- shared.indices do
        val selected = mutable.Set(seed)
        val ends = arms.indices.map(n => account(n, shared(seed)._2(n)).offsets.max).toArray
        var pairs = List.empty[List[List[Event]]]
        var accounts = List.empty[List[Account]]
        var valid = true
        var growing = true
        while valid && growing do
          growing = false
          pairs = arms.indices.map(n => arms(n).captures.filter(_.plyOffset <= ends(n)).grouped(2).toList).toList
          valid = pairs.forall(_.forall {
            case List(first, last) => last.plyOffset == first.plyOffset + 1 &&
              last.originalVictim == first.originalActor &&
              first.originalVictim.exists(_.side != first.originalActor.get.side)
            case _ => false
          })
          if valid then
            accounts = arms.indices.map(n => pairs(n).map(es => account(n, es))).toList
            for n <- arms.indices do
              val next = accounts(n).flatMap(_.offsets).max
              if next > ends(n) then
                ends(n) = next
                growing = true
            for i <- shared.indices if !selected(i) && arms.indices.exists(n =>
                shared(i)._2(n).head.plyOffset <= ends(n)) do
              selected += i
              for n <- arms.indices do ends(n) = ends(n).max(account(n, shared(i)._2(n)).offsets.max)
              growing = true
        if valid then
          val key = pairs.map(_.flatten.map(_.plyOffset))
          val contextual = arms.indices.exists(n => key(n) != shared(seed)._2(n).map(_.plyOffset))
          val complete = arms.indices.forall(n => accounts(n).forall(c => c.complete && c.dutyCosts.isEmpty) &&
            accounts(n).flatMap(_.events).toSet == pairs(n).flatten.toSet)
          val rooted = arms.indices.map(n => pairs(n).head.exists(_.originalVictim == arms(n).root)).toList
          val signatures = pairs.map(_.map(es => (es.map(_.originalVictim).toSet, es.last.originalActor)))
          val contrasted = signatures.map(_.head._1).distinct.size > 1
          val closedReplies = arms.indices.forall(n => accounts(n).flatMap(_.offsets).forall(offset =>
            offset >= 0 && offset <= ends(n)) && (0 to ends(n)).filter(arms(n).checks).forall(_ + 1 <= ends(n)))
          if contextual && complete && rooted.forall(identity) && contrasted && closedReplies && !built(key) then
            val sections = arms.zipWithIndex.map { (a, n) =>
              val root = a.root.get
              val evasion = a.originals.filter(p => p.side != root.side && a.covers(0, p, root) &&
                a.control(1, p, root).contains(false))
              val firstAtRoot = pairs(n).head.head.plyOffset == 0
              val initial = Option.when(!firstAtRoot && evasion.size == 1)(Action(a.ref(0),
                MoveWorkProjection.Detail.Evades(a.at(root, 1).get, evasion.head))).toList
              val conditions = pairs(n).flatMap { es =>
                val first = es.head
                val last = es.last
                val recapturer = last.originalActor.get
                val participants = (es.flatMap(_.originalVictim) ++ last.originalActor.toList).distinct
                val arrivals = participants.flatMap(p => a.route(p).filter(_ < first.plyOffset).lastOption
                  .map(offset => PieceRoute(a.path, p, List(offset))))
                val response = for
                  next <- Option.when(last.plyOffset + 1 <= ends(n))(last.plyOffset + 1)
                  target <- a.mover(next)
                  if target.side != recapturer.side && a.control(last.plyOffset, recapturer, target).contains(false) &&
                    a.covers(last.plyOffset + 1, recapturer, target) && a.control(next + 1, recapturer, target).contains(false)
                yield TargetResponse(a.path, last.plyOffset, a.at(target, last.plyOffset + 1).get,
                  recapturer, target, Some(next), None, survives = false)
                arrivals ++ response
              }
              val checks = (0 to ends(n)).filter(a.checks).flatMap(offset => List(offset, offset + 1))
              val offsets = (List(0) ++ accounts(n).flatMap(_.offsets) ++ checks).distinct.sorted
              material(a, pairs(n).flatten, offsets).map { cost =>
                val context = (initial ++ conditions).distinct
                Option.when(context.nonEmpty)(Section(context)).toList :+
                  Section(List(cost, a.pieceExchange(pairs(n).flatten)), offsets.map(a.ref))
              }
            }
            if sections.forall(_.nonEmpty) then
              built += key
              offerAnswer("contested_pair_response", sections.flatten.flatten,
                arms.zipWithIndex.flatMap((a, n) => continuation((0 to ends(n)).map(a.ref).toList)))

    if choices.isEmpty && rootNotInCheck then
      // Compare the same attack before and after a recorded blocker arrives.
      // ponytail: quiet episodes only; add a material account before handling
      // capture or checking responses with this reader.
      def blocks(a: Arm, count: Int, actor: Piece, target: Piece, blocker: Piece): Boolean =
        (for
          current <- a.at(actor, count)
          victim <- a.at(target, count)
          front <- a.at(blocker, count)
          position <- a.position(count).filter(_.complete(RelationFactKind.RayBarrier))
        yield
          val ray = position.blockedRay(current, victim).filter(_._2 == List(front))
          a.proofs ++= position.sourceRefs(ray.toList.map(_._1))
          ray.nonEmpty
        ).contains(true)
      for (early, n) <- arms.zipWithIndex if early.quiet(1) do
        val later = arms(1 - n)
        val actor = early.root.get
        for
          arrival <- later.route(actor).find(i => i > 0 && later.path.moves(i) == early.path.moves.head).toList
          if later.quiet(arrival)
          target <- early.originals.filter(p => p.side != actor.side && early.gained(p))
          blocker <- early.mover(1).filter(_.side == target.side).toList
          blockAt <- later.route(blocker).filter(i => i > 0 && i < arrival).lastOption.toList
          if early.stationary(target, 1) && later.stationary(target, arrival) &&
            early.at(actor, 1) == early.at(actor, 2) &&
            blocks(early, 2, actor, target, blocker) && blocks(later, arrival + 1, actor, target, blocker)
        do
          val plans = List((early, 1), (later, blockAt)).map { (a, start) =>
            a.originals.filter(p => p.side == actor.side && a.control(start, blocker, p).contains(false) && a.covers(start + 1, blocker, p))
              .map { p =>
                // This says the recorded move leaves the attack, not that it never returns.
                val escape = a.route(p).find(_ > start).filter(i =>
                  (start + 1 to i).forall(a.covers(_, blocker, p)) && a.control(i + 1, blocker, p).contains(false))
                p -> escape
              }.sortBy((p, escape) => (escape.isEmpty, p.square.key))
          }
          val ends = plans.zip(List(1, arrival)).map((responses, start) => (responses.flatMap(_._2) :+ start).max)
          val complete = List(early, later).zipWithIndex.forall { (a, index) =>
              val end = ends(index)
              a.quiet(end) && a.stationary(target, end) &&
                a.at(actor, end + 1) == early.at(actor, 1) && blocks(a, end + 1, actor, target, blocker) &&
                plans(index).forall { (p, escape) => escape.nonEmpty ||
                  a.gained(p) && a.stationary(p, end) && (1 to end + 1).forall(a.covers(_, a.root.get, p))
                }
            }
          if complete then
            val sections = List((early, 1), (later, blockAt)).zipWithIndex.map { case ((a, start), index) =>
              val reply = plans(index).flatMap { (piece, escape) =>
                val attack = TargetResponse(a.path, start, a.at(piece, start + 1).get, blocker, piece,
                  escape, None, survives = false)
                val support = Option.when(escape.isEmpty)(ControlState(a.ref(ends(index)),
                  a.at(a.root.get, ends(index) + 1).get, a.at(piece, ends(index) + 1),
                  a.at(piece, ends(index) + 1).get.square.key, present = true)).toList
                attack :: support
              }
              val at = if index == 0 then 1 else arrival
              val lead: List[Statement] = if index == 0 then List(
                TargetResponse(a.path, 0, a.at(target, 1).get, actor, target, None, None, survives = false))
              else List(PieceRoute(a.path, blocker, List(blockAt)), PieceRoute(a.path, actor, List(arrival)))
              val offsets = (0 to ends(index)).toList
              Section(lead ++ List(Continuation(offsets.map(a.ref)),
                BlockedAttack(a.ref(at), a.at(actor, at + 1).get, a.at(target, at + 1).get,
                  a.at(blocker, at + 1).get)) ++ reply, offsets.map(a.ref))
            }
            offerAnswer("opening_counterplay", sections)

    // A shared advance can have a different recovery even when neither root
    // mover is its subject. Keep the actual support use, the provider's earlier
    // retreat, and the shared counterattack's order with the material account.
    for (lost, n) <- arms.zipWithIndex if choices.isEmpty && rootNotInCheck do
      val regained = arms(1 - n)
      for
        loss <- lost.captures.filter(_.plyOffset > 1)
        focus <- loss.originalVictim.filter(_.side == lost.root.get.side).toList
        captor <- loss.originalActor.toList
        start = loss.plyOffset - 1
        if lost.mover(start).contains(focus) && !lost.captures.exists(_.plyOffset == start) && lost.recovery(loss).isEmpty
        otherLoss <- regained.captures.filter(e => e.plyOffset > 1 && e.originalVictim.contains(focus) &&
          e.originalActor.contains(captor) && regained.mover(e.plyOffset - 1).contains(focus) &&
          regained.path.moves(e.plyOffset - 1) == lost.path.moves(start))
        recapture <- regained.recovery(otherLoss).filter(_.plyOffset == otherLoss.plyOffset + 1).toList
        provider <- recapture.originalActor.toList
        retreat <- lost.route(provider).filter(_ < start).lastOption.filter(_ > 0).toList
        incoming <- lost.evaded(retreat, provider).toList
        if lost.covers(retreat, incoming, provider) && regained.stationary(provider, recapture.plyOffset - 1) &&
          (retreat + 1 to loss.plyOffset + 1).forall(count => lost.at(provider, count) == lost.at(provider, retreat + 1)) &&
          lost.control(loss.plyOffset + 1, provider, captor).contains(false) &&
          regained.balanced(List(otherLoss, recapture))
        attack = loss.plyOffset + 1
        actor <- lost.mover(attack).filter(_.side == focus.side).toList
        target <- lost.mover(attack + 1).filter(p => p.side != focus.side && p.role.name != "king").toList
        if lost.evaded(attack + 1, target).contains(actor) && lost.covers(attack + 1, actor, target)
        common = (2 until otherLoss.plyOffset - 1 by 2).filter(i =>
          regained.path.moves(i) == lost.path.moves(attack) && regained.mover(i).contains(actor) &&
          regained.evaded(i + 1, target).contains(actor) && regained.covers(i + 1, actor, target)).toList
        if common.size == 1
        earlier = common.head
      do
        val first = lost.account(loss, focus)
        val second = regained.account(otherLoss, focus)
        val firstRefs = List(0, retreat - 1, retreat, start, loss.plyOffset, attack, attack + 1).distinct
        val secondRefs = List(0, earlier, earlier + 1, otherLoss.plyOffset - 1, otherLoss.plyOffset, recapture.plyOffset).distinct
        val support = LineContinuationInvestigation.directSupportUse(packet, regained.path, paths,
          recapture.plyOffset + 1, provider, focus).uses.find(use =>
            use.targetCapture.id == otherLoss.id && use.recapture.id == recapture.id)
        val valid = fits(first, List(loss), firstRefs) && fits(second, List(otherLoss, recapture), secondRefs) &&
          lost.captures.filter(_.plyOffset <= firstRefs.max) == first.events &&
          regained.captures.filter(_.plyOffset <= secondRefs.max) == second.events &&
          !(0 to firstRefs.max).exists(lost.checks) && !(0 to secondRefs.max).exists(regained.checks) &&
          support.nonEmpty
        // ponytail: earlier captures, mixed material and checking episodes stay
        // with their existing accounts until a complete realization is proved.
        if valid then
          for
            lostCost <- material(lost, first.events, firstRefs)
            regainedCost <- material(regained, second.events, secondRefs)
            use <- support
          do offerAnswer("opening_counterplay", List(
            Section(List(TargetResponse(lost.path, retreat - 1, lost.at(provider, retreat).get,
              incoming, provider, Some(retreat), None, survives = false),
              ControlState(lost.ref(loss.plyOffset), lost.at(provider, loss.plyOffset + 1).get,
                lost.at(captor, loss.plyOffset + 1), lost.at(captor, loss.plyOffset + 1).get.square.key, present = false))),
            Section(List(PieceRoute(lost.path, focus, List(start)), lostCost, lost.pieceExchange(first.events),
              TargetResponse(lost.path, attack, lost.at(target, attack + 1).get, actor, target,
                Some(attack + 1), None, survives = false)), firstRefs.map(lost.ref)),
            Section(List(TargetResponse(regained.path, earlier, regained.at(target, earlier + 1).get, actor, target,
              Some(earlier + 1), None, survives = false), PieceRoute(regained.path, focus, List(otherLoss.plyOffset - 1)),
              SupportRecapture(regained.ref(0), regained.path, use), regainedCost,
              regained.pieceExchange(second.events)), secondRefs.map(regained.ref))),
            evidence = regained.supportSources(use))

    // Select from exact evidence and ownership without invoking realization.
    // The caller retains this question if its eventual delivery cannot fit.
    selected()
