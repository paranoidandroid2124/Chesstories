package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** Keep the root-owned condition separate from its conditional realization.
  * A different forecast cannot falsify a still-legal, independently replayed
  * response. Conversely a legal option supplies no evaluation/utility proof. */
private[runtime] object FunctionalReplyReading:
  import CommentaryFact.*
  import CommentaryProposal.*
  import CommentaryIdea.AnsweredComparison
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  final case class RootOwner(proposalId: String, comparison: CompareMoves,
      answer: Option[CommentaryInvestigation.Answer] = None)

  final case class Proof(owner: RootOwner, condition: Predicate,
      reference: List[String], alternative: List[String], option: List[String],
      effect: Predicate, action: Predicate, evidence: List[String], price: Option[List[String]] = None)

  /** A task and a conditional example have independent prerequisites. Keep
    * the original resource actor/destination; a new defense need not perform
    * the old capture before this task can receive its own value question.
    */
  final case class Resource(owner: RootOwner, condition: Predicate, actor: Piece,
      square: String, move: String, witnessedPrefix: List[String], contextId: String,
      expectedReference: String, expectedAlternative: String, evidence: List[String])

  def resources(entries: List[Entry]): List[Resource] = entries.flatMap { entry =>
    if entry.admission != "generated" || entry.request.origin.question.producer != "C-reply-function" then Nil
    else
      val facts = entry.request.requirements.collect { case fact: Fact => fact.id -> fact }.toMap
      val readings = entry.readings.map(reading => reading.requirementId -> reading).toMap
      for
        root <- entry.request.requirements.collect { case pair: CompareMoves if pair.id == "function_root" => pair }
        if root.prefixMovesUci.isEmpty
        context <- entry.request.requirements.collect {
          case group: FactGroup if group.id == "function_task_context" && group.mode == "all" => group
        }
        if context.members == List("function_before", "function_root_reference", "function_root_alternative",
          "function_owned", "function_changed", "function_live", "function_resource") &&
          (context.id :: context.members).forall(id => readings.get(id).exists(_.state == "supported"))
        live <- facts.get("function_live").toList
        before <- facts.get("function_before").toList
        left <- facts.get("function_root_reference").toList
        right <- facts.get("function_root_alternative").toList
        use <- facts.get("function_resource").toList
        if live.path.source == Declared(List(root.referenceMove)) && live.at == Ply(1) &&
          live.predicate == before.predicate && before.at == Ply(0) &&
          live.predicate == left.predicate && left.predicate == right.predicate &&
          left.at == Ply(1) && right.at == Ply(1) &&
          List(before, left).forall(fact => fact.path.source match
            case Declared(moves) => moves.headOption.contains(root.referenceMove)
            case _ => false) && (right.path.source match
            case Declared(moves) => moves.headOption.contains(root.alternativeMove)
            case _ => false) &&
          entry.request.requirements.exists {
            case FactComparison("function_owned", a, b, relation) => a == before.id && b == left.id &&
              Set("same", "different")(relation) && before.expected.nonEmpty && left.expected.nonEmpty &&
              (before.expected == left.expected) == (relation == "same")
            case _ => false
          } &&
          entry.request.requirements.contains(FactComparison("function_changed", left.id, right.id, "different")) &&
          entry.generatedArms.exists(arm => arm.prefixMoves.isEmpty && arm.referenceMove == root.referenceMove &&
            arm.alternativeMove.contains(root.alternativeMove) && arm.headRequirementIds == List(context.id)) &&
          live.predicate.kind == "control" && use.predicate.kind == "legal_move" && use.predicate.property.isEmpty
        actor <- use.predicate.subject.toList
        square <- use.predicate.square.toList
        if live.predicate.square.contains(square) && live.predicate.subject.exists(provider =>
          provider.side == actor.side && provider != actor)
        referenceValue <- left.expected.toList
        alternativeValue <- right.expected.toList
        if referenceValue != alternativeValue && use.expected.contains("legal")
        moves <- (use.path.source match
          case Declared(moves) => Some(moves)
          case _ => None).toList
        if moves.size >= 3 && moves.head == root.referenceMove && use.at == Ply(moves.size - 1)
      yield Resource(RootOwner(entry.request.id, root, readings.get(root.id).flatMap(_.answer)),
        live.predicate, actor, square, moves.last, moves.init, context.id, referenceValue, alternativeValue,
        context.members.flatMap(id => readings(id).result.toList.flatMap(_.samples.flatMap(_.sourceIds))).distinct)
  }.distinct

  object Proof:
    def apply(owner: AnsweredComparison, condition: Predicate, reference: List[String],
        alternative: List[String], option: List[String], effect: Predicate, action: Predicate,
        evidence: List[String]): Proof = apply(owner, condition, reference, alternative, option,
      effect, action, evidence, None)
    def apply(owner: AnsweredComparison, condition: Predicate, reference: List[String],
        alternative: List[String], option: List[String], effect: Predicate, action: Predicate,
        evidence: List[String], price: Option[List[String]]): Proof = new Proof(
      RootOwner(owner.proposalId, owner.comparison, Some(owner.answer)), condition, reference,
      alternative, option, effect, action, evidence, price)

  def read(entries: List[Entry]): List[Proof] = entries.flatMap { entry =>
    if entry.admission != "generated" || entry.request.origin.question.producer != "C-reply-function" then Nil
    else
      val facts = entry.request.requirements.collect { case fact: Fact => fact.id -> fact }.toMap
      val readings = entry.readings.map(reading => reading.requirementId -> reading).toMap
      def supported(id: String) = readings.get(id).exists(_.state == "supported")
      def declared(fact: Fact) = fact.path.source match
        case Declared(moves) => Some(moves)
        case _ => None
      for
        root <- entry.request.requirements.collect { case pair: CompareMoves if pair.id == "function_root" => pair }
        if root.prefixMovesUci.isEmpty
        context <- entry.request.requirements.collect { case group: FactGroup if group.id == "function_context" => group }
        if context.mode == "all" && (context.id :: context.members).forall(supported)
        live <- facts.get("function_live").toList
        if live.path.source == Declared(List(root.referenceMove)) && live.at == Ply(1)
        left <- facts.get("function_effect_reference").toList
        right <- facts.get("function_effect_alternative").toList
        option <- facts.get("function_option").toList
        reference <- declared(left).toList
        alternative <- declared(right).toList
        available <- declared(option).toList
        price = facts.get("function_price").flatMap(declared)
        if reference.size >= 2 && reference.size == alternative.size &&
          reference.head == root.referenceMove && alternative.head == root.alternativeMove &&
          reference.tail == alternative.tail && left.at == Ply(reference.size) && right.at == left.at &&
          left.predicate == right.predicate && option.expected.contains("legal") &&
          (price match
            case None => available.init == reference && option.at == Ply(reference.size)
            case Some(moves) => available == reference && option.at == Ply(reference.size - 1) &&
              moves.init == alternative && facts.get("function_price").exists(fact =>
                fact.predicate == left.predicate && fact.at == Ply(reference.size) && fact.expected.contains("legal"))) &&
          entry.generatedArms.exists(arm => arm.prefixMoves.isEmpty && arm.referenceMove == root.referenceMove &&
            arm.alternativeMove.contains(root.alternativeMove) && arm.headRequirementIds == List(context.id))
      yield Proof(RootOwner(entry.request.id, root, readings.get(root.id).flatMap(_.answer)), live.predicate, reference,
        alternative, available, left.predicate, option.predicate,
        entry.readings.flatMap(_.result.toList.flatMap(_.samples.flatMap(_.sourceIds))).distinct, price)
  }.distinct

  private[runtime] def conditionAt(paths: PreparedPaths, path: PreparedPath, at: Int, predicate: Predicate): Option[String] =
      paths.positionView(path, at).flatMap { view =>
        def piece(original: Piece) = paths.trackedPieceAfter(path, original.witness, at).flatten
        predicate.kind match
          case "location" => predicate.subject.flatMap(piece).map(_.square.key)
          case "control" =>
            for
              original <- predicate.subject
              actor <- paths.trackedPieceAfter(path, original.witness, at)
              square <- predicate.square
              if view.completeKinds(RelationFactKind.GeometricControl)
            yield if actor.exists(new MoveWorkProjection.Position(view).controlledBy(_)(square)) then "present" else "absent"
          case "check" => predicate.side.filter(_ => view.completeKinds(RelationFactKind.GeometricControl)).map(side =>
            if view.currentCheckSources(if side == "white" then chess.White else chess.Black).nonEmpty then "present" else "absent")
          case "legal_capture" =>
            for
              actor <- predicate.subject.flatMap(piece)
              target <- predicate.target.flatMap(piece)
              rows <- view.legalRows
              if view.position.sideToMove.contains(actor.side)
            yield if rows.exists(row => row.actor == actor && row.capture.contains(target)) then "legal" else "illegal"
          case "legal_move" =>
            for
              actor <- predicate.subject.flatMap(piece)
              square <- predicate.square
              rows <- view.legalRows
              if view.position.sideToMove.contains(actor.side)
            yield if rows.exists(row => row.actor == actor && row.to.key == square && row.promotionRole == predicate.property)
              then "legal" else "illegal"
          case _ => None
      }
  private def lostGuard(proof: Proof): Boolean = proof.price.nonEmpty && proof.reference.size == 2 &&
    proof.condition.kind == "location" && proof.condition.subject == proof.effect.subject &&
    proof.effect.kind == "legal_capture" && proof.effect.target == proof.action.subject &&
    proof.action.kind == "legal_move" && proof.action.property.isEmpty &&
    proof.action.subject.exists(actor => actor.role != "king" &&
      proof.condition.subject.exists(_.side != actor.side))

  private def remainingCapturers(paths: PreparedPaths, value: LegalReplyOption)
      : Option[List[RelationColoredPieceWitness]] =
    val at = value.proof.reference.size
    for
      target <- value.proof.effect.target
      current <- paths.trackedPieceAfter(value.reference, target.witness, at).flatten
      guard <- value.proof.effect.subject
      provider <- paths.trackedPieceAfter(value.reference, guard.witness, at).flatten
      rows <- paths.positionView(value.reference, at).flatMap(_.legalRows)
    yield rows.filter(row => row.capture.contains(current) && row.actor != provider).map(_.actor)
      .distinct.sortBy(actor => (if actor.side.white then 0 else 1, actor.role.name, actor.square.key))

  /** A complete legal inventory qualifies the specific lost capture. It is
    * not an evaluated price, a safe-square proof or another forecast action. */
  def withRemainingCaptures(paths: PreparedPaths, value: LegalReplyOption): LegalReplyOption =
    if lostGuard(value.proof) then value.copy(remainingCapturers = remainingCapturers(paths, value)) else value

  def valid(paths: PreparedPaths, value: LegalReplyOption): Boolean =
    val proof = value.proof
    val count = proof.reference.size
    def owned(path: PreparedPath, moves: List[String]) = paths.paths.exists(_ eq path) &&
      path.certified.nonEmpty && path.ids == path.declared && path.moves.startsWith(moves) && path.sourceMoves == path.moves
    def read(path: PreparedPath, at: Int, predicate: Predicate) = conditionAt(paths, path, at, predicate)
    val root = proof.owner.comparison
    // A present receipt is still evidence with an exact owner. An unfinished
    // numeric comparison supplies no receipt and does not invalidate a board
    // fact independently reread from this canonical root and legal history.
    val receiptOwned = proof.owner.answer.forall { answer =>
      def receiptPath(id: String, move: String, occurrence: String) = paths.paths.find(_.line.ref.id == id)
        .exists(path => owned(path, List(move)) && path.ids.headOption.contains(occurrence) &&
          path.positions.headOption == value.reference.positions.headOption)
      receiptPath(answer.referenceLineId, root.referenceMove, answer.referenceOccurrenceId) &&
        receiptPath(answer.alternativeLineId, root.alternativeMove, answer.alternativeOccurrenceId) &&
        value.reference.ids.headOption.contains(answer.referenceOccurrenceId) &&
        value.alternative.ids.headOption.contains(answer.alternativeOccurrenceId)
    }
    def actionAt(path: PreparedPath, at: Int, move: String, predicate: Predicate): Boolean =
      paths.positionView(path, at).flatMap(_.legalRows).exists(_.exists(row =>
        row.move == move && predicate.subject.exists(original =>
          paths.trackedPieceAfter(path, original.witness, at).flatten.contains(row.actor)) &&
        (predicate.kind match
          case "legal_capture" => predicate.target.exists(original =>
            paths.trackedPieceAfter(path, original.witness, at).flatten.exists(row.capture.contains))
          case "legal_move" => predicate.square.contains(row.to.key) && row.promotionRole == predicate.property
          case _ => false)))
    def capturedFriendly(path: PreparedPath) = paths.continuationReadingFor(path)
      .flatMap(_.events.find(_.plyOffset == count - 1)).filter(event =>
        event.originalActor == proof.action.target.map(_.witness) &&
          event.originalVictim.exists(victim => proof.action.subject.exists(_.witness.side == victim.side)))
      .flatMap(_.originalVictim)
    val sameRecapture = capturedFriendly(value.reference).nonEmpty &&
      capturedFriendly(value.reference) == capturedFriendly(value.alternative)
    val quietArrival = count == 2 && proof.action.target.exists(target => target.role != "king" &&
      proof.action.subject.exists(_.side != target.side) && List(value.reference, value.alternative).forall(path =>
        paths.positionView(path, 1).flatMap(_.legalRows).exists(_.exists(row =>
          row.move == proof.reference.last && row.capture.isEmpty && row.promotionRole.isEmpty &&
            paths.trackedPieceAfter(path, target.witness, 1).flatten.contains(row.actor)))))
    val exposedPin = count == 2 && proof.action.kind == "legal_capture" && (for
      actor <- proof.action.subject
      victim <- proof.action.target
      before <- paths.purposePosition(value.reference, 0)
      front <- before.relativeAlignments(actor.witness).collectFirst {
        case (_, front, rear) if rear == victim.witness => front
      }
      if List(value.reference, value.alternative).forall(path =>
        paths.positionView(path, 1).flatMap(_.legalRows).exists(_.exists(row =>
          row.move == proof.reference.last && paths.trackedPieceAfter(path, front, 1).flatten.contains(row.actor))) &&
          (0 to count).forall(at => paths.trackedPieceAfter(path, actor.witness, at).contains(Some(actor.witness)) &&
            paths.trackedPieceAfter(path, victim.witness, at).contains(Some(victim.witness))))
    yield true).contains(true)
    val shared = owned(value.reference, proof.reference) && owned(value.alternative, proof.alternative) &&
      owned(value.available, proof.option) && count >= 2 && count == proof.alternative.size &&
      proof.reference.head == root.referenceMove && proof.alternative.head == root.alternativeMove &&
      proof.reference.tail == proof.alternative.tail &&
      root.prefixMovesUci.isEmpty &&
      receiptOwned && value.reference.positions.headOption == value.alternative.positions.headOption &&
      value.available.ids.take(count) == value.reference.ids.take(count) &&
      read(value.reference, 0, proof.condition).nonEmpty &&
      read(value.reference, 0, proof.condition) != read(value.reference, 1, proof.condition) &&
      read(value.reference, 1, proof.condition).nonEmpty &&
      read(value.reference, 1, proof.condition) != read(value.alternative, 1, proof.condition)
    if proof.price.nonEmpty then shared && proof.price.exists { moves =>
      proof.option == proof.reference && moves.init == proof.alternative &&
      (proof.condition.kind == "location" && proof.condition.subject == proof.effect.target &&
        proof.effect.kind == "legal_capture" && proof.action.kind == "legal_capture" &&
        proof.action.subject.exists(actor => proof.condition.subject.exists(_.side == actor.side)) ||
        lostGuard(proof) && value.remainingCapturers.nonEmpty &&
          value.remainingCapturers == remainingCapturers(paths, value) &&
          List(value.reference, value.alternative).forall(path =>
            paths.positionView(path, count - 1).flatMap(_.legalRows).exists(_.exists(row =>
              row.move == proof.reference.last && row.capture.isEmpty && row.promotionRole.isEmpty)) &&
            proof.effect.subject.exists(guard => paths.trackedPieceAfter(path, guard.witness, count)
              .flatten.exists(_.role == guard.witness.role)))) &&
      List(value.reference, value.alternative).forall(path =>
        actionAt(path, count - 1, proof.reference.last, proof.action) &&
          (1 to count).forall(at => read(path, at, proof.condition) == read(path, 1, proof.condition))) &&
      read(value.reference, count, proof.effect).contains("illegal") &&
      read(value.alternative, count, proof.effect).contains("legal") &&
      value.price.exists(path => owned(path, moves) &&
        path.ids.headOption == value.alternative.ids.headOption &&
        actionAt(path, count, moves.last, proof.effect) && proof.effect.target.exists(target =>
          paths.trackedPieceAfter(path, target.witness, count + 1).contains(None)))
    }
    else shared && value.price.isEmpty && proof.option.init == proof.reference &&
      read(value.available, count, proof.action).contains("legal") &&
      read(value.alternative, count, proof.action).contains("illegal") &&
      actionAt(value.available, count, proof.option.last, proof.action) && (proof.effect.kind match
      case "legal_capture" => proof.action == proof.effect && proof.condition.kind == "control" &&
        proof.condition.subject == proof.action.subject &&
        (1 until count).forall(at => read(value.reference, at, proof.condition).contains("present") &&
          read(value.alternative, at, proof.condition).contains("absent")) &&
        read(value.reference, count, proof.effect).contains("legal") &&
        read(value.alternative, count, proof.effect).contains("illegal") &&
        proof.effect.target.exists(target => paths.trackedPieceAfter(value.reference, target.witness, count).flatten
          .exists(piece => proof.condition.square.contains(piece.square.key))) &&
        (sameRecapture || quietArrival ||
          List(value.reference, value.alternative).forall(path => paths.positionView(path, count).exists(view =>
            view.currentCheckSources(proof.action.subject.get.witness.side).nonEmpty)))
      case "check" => proof.condition.kind == "location" && proof.condition.subject.exists(_.role == "king") &&
        read(value.reference, count, proof.effect).contains("absent") &&
        read(value.alternative, count, proof.effect).contains("present") &&
        proof.action.subject.exists(piece => proof.effect.side.contains(piece.side)) &&
        proof.action.subject.exists(piece =>
          paths.trackedPieceAfter(value.reference, piece.witness, count).flatten.nonEmpty &&
            paths.trackedPieceAfter(value.reference, piece.witness, count) ==
              paths.trackedPieceAfter(value.alternative, piece.witness, count)) &&
        (read(value.reference, 0, proof.action).contains("legal") || exposedPin)
      case _ => false)

  def relevant(paths: PreparedPaths, pair: List[PreparedPath], value: LegalReplyOption): Boolean =
    pair.find(_.moves.headOption.contains(value.proof.owner.comparison.referenceMove)).exists { owner =>
      pair.find(_ ne owner).exists { peer =>
        value.reference.ids.headOption == owner.ids.headOption &&
          value.reference.positions.headOption == owner.positions.headOption &&
          conditionAt(paths, peer, 1, value.proof.condition).nonEmpty &&
          conditionAt(paths, peer, 1, value.proof.condition) == conditionAt(paths, value.alternative, 1, value.proof.condition)
      }
    }

  /** A current evaluated continuation can also own the same mechanical
    * prefix. Reread every represented board; never move its numerical owner.
    */
  def rebase(paths: PreparedPaths, pair: List[PreparedPath], value: LegalReplyOption,
      current: List[PreparedPath]): Option[LegalReplyOption] =
    if !valid(paths, value) || !relevant(paths, pair, value) then return None
    def replacements(original: PreparedPath, prefix: List[String]) = current.distinct.filter(path =>
      paths.paths.exists(_ eq path) && path.moves.startsWith(prefix) &&
        path.ids.take(prefix.size) == original.ids.take(prefix.size) &&
        path.positions.take(prefix.size + 1) == original.positions.take(prefix.size + 1))
    val proof = value.proof
    (for
      reference <- replacements(value.reference, proof.reference)
      alternative <- replacements(value.alternative, proof.alternative)
      available <- replacements(value.available, proof.option)
      price <- proof.price.fold(List(Option.empty[PreparedPath]))(prefix =>
        value.price.toList.flatMap(original => replacements(original, prefix)).map(Some(_)))
      rebased = withRemainingCaptures(paths, value.copy(reference = reference,
        alternative = alternative, available = available, price = price))
      if valid(paths, rebased) && relevant(paths, pair, rebased)
    yield rebased).minByOption(value => value.references.map(_.path).distinct.size)

  /** The actual line owns the setup. After it diverges at an opponent capture,
    * only consecutive recaptures of that same transaction may complete a
    * conditional example. A later quiet plan cannot borrow this ownership. */
  def ownsSetup(paths: PreparedPaths, actual: PreparedPath, pair: List[PreparedPath],
      value: LegalReplyOption): Boolean =
    val proof = value.proof
    if actual.moves.startsWith(proof.reference.init) then true
    else if proof.price.nonEmpty || !pair.exists(_.moves.startsWith(proof.alternative)) then false
    else
      val common = actual.moves.zip(proof.reference).takeWhile((a, b) => a == b).size
      val end = proof.option.size
      common >= 1 && common % 2 == 1 && common < actual.moves.size && common < proof.reference.size &&
        (common until end).forall { offset =>
          paths.positionView(value.available, offset).flatMap(_.legalRows).exists(_.exists(row =>
            row.move == proof.option(offset) && row.capture.nonEmpty && row.promotionRole.isEmpty &&
              (offset == common || paths.positionView(value.available, offset - 1).flatMap(_.legalRows)
                .flatMap(_.find(_.move == proof.option(offset - 1))).exists(previous =>
                  row.capture.exists(victim => victim.side == previous.actor.side &&
                    victim.role == previous.actor.role && victim.square == previous.to)))))
        }

  def withAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[Proof], previous: Answer,
      protect: (Answer, List[Section]) => Answer = (answer, sections) => RootSubjectComparison.protect(answer, sections)): Answer =
    paths.requireOwner(packet)
    if pair.size != 2 || pair.exists(_.automaticTerminal.nonEmpty) then return previous
    val candidates = proofs.flatMap { proof =>
      for
        played <- packet.assembly.playedTransition.toList
        if Set(proof.owner.comparison.referenceMove, proof.owner.comparison.alternativeMove)(played.moveUci)
        reference <- paths.paths.filter(_.moves.startsWith(proof.reference))
        alternative <- paths.paths.filter(_.moves.startsWith(proof.alternative))
        available <- paths.paths.filter(_.moves.startsWith(proof.option))
        price <- proof.price.fold(List(Option.empty[PreparedPath]))(moves =>
          paths.paths.filter(_.moves.startsWith(moves)).map(Some(_)))
        value = withRemainingCaptures(paths, LegalReplyOption(reference, alternative, available, proof, price))
        if valid(paths, value) && relevant(paths, pair, value)
      yield value
    }.distinct.sortBy(value => (if pair.exists(_.moves.head == value.proof.owner.comparison.alternativeMove) then 0 else 1,
      value.proof.reference.size,
      if value.proof.effect.kind == "check" && value.proof.action.kind == "legal_capture" then 0 else 1,
      value.proof.effect.json.toString,
      value.proof.action.json.toString, value.proof.reference.mkString(" "),
      value.reference.moves.size + value.alternative.moves.size + value.available.moves.size))
    // The new guard orientation is a specific factual difference. Neither an
    // empty inventory nor the loss of one capture proves a useful choice.
    val facts = candidates.filter(value => lostGuard(value.proof))
      .distinctBy(value => (value.proof.condition, value.proof.effect, value.proof.action))
    val values = candidates.filterNot(value => lostGuard(value.proof)).filter { value =>
      val proof = value.proof
      proof.effect.kind != "legal_capture" || proof.effect.target.exists(target =>
        val protectedPath = if proof.price.nonEmpty then value.reference else value.alternative
        paths.trackedPieceAfter(protectedPath, target.witness, proof.alternative.size).flatten.exists(piece =>
          paths.positionView(protectedPath, proof.alternative.size).flatMap(_.legalRows)
            .exists(rows => !rows.exists(_.capture.contains(piece)))))
    }
    // Equivalent examples can share a purpose, but independent conditions do
    // not disappear behind a shorter one. Keep them available when choosing a
    // single representative cannot faithfully answer the selected question.
    val conditions = values.groupBy(value => (value.proof.condition, value.proof.effect.kind))
    // Each actual root owns its missing purpose. An unrelated condition for
    // the other root cannot veto an independently certified conditional idea.
    val previousPurpose = previous.core.flatMap(_.statements).collectFirst {
      case ReferencePurpose(reading) => reading
    }
    val supplementedPurpose = previousPurpose.flatMap { reading =>
      val selected = reading.arms.flatMap { arm =>
        val options = values.filter(value => value.proof.owner.comparison.referenceMove == arm.path.moves.head &&
          (!reading.explanation(arm).answered || reading.rootUses.exists {
            case Action(move, MoveWorkProjection.Detail.OpenedRay(_, user, end)) =>
              (move.path eq arm.path) && move.offset == 0 && value.proof.condition.kind == "control" &&
                value.proof.condition.subject.exists(_.witness == user) && value.proof.condition.square.contains(end)
            case _ => false
          }) && ownsSetup(paths, arm.path, pair, value))
        val groups = options.groupBy(value => (value.proof.condition, value.proof.effect.kind))
        Option.when(groups.size == 1)(options.head).map { value =>
          val shared = value.copy(reference = value.available)
          if valid(paths, shared) && relevant(paths, pair, shared) then shared else value
        }
      }
      Option.when(selected.nonEmpty)(selected).flatMap { options =>
        val enriched = reading.withConditionalPurposes(options)
        def replace(statement: Statement): Statement = statement match
          case ReferencePurpose(value) if value eq reading => ReferencePurpose(enriched)
          case ReferencePosition(value, arm) if value eq reading => ReferencePosition(enriched, arm)
          case ReferenceContrast(value) if value eq reading => ReferenceContrast(enriched)
          case ReferenceExchangeReply(value, purpose, branch) if value eq reading =>
            ReferenceExchangeReply(enriched, purpose, branch)
          case value => value
        val core = previous.core.map(section => section.copy(statements = section.statements.map(replace)))
        val added = options.map { value =>
          // The illustrative branch owns its own complete local price; it
          // neither borrows the main PV's exchanges nor changes its forecast.
          val end = value.proof.option.size - 1
          val prices = value.available.certified.flatMap(_._2.material).toList.flatMap { material =>
            val events = material.events.filter(_.plyOffset <= end)
            Option.when(events.nonEmpty)(CommentaryMaterialAccount.calculate(material,
              events.map(_.plyOffset), (0 to end).toList, value.available.moves.size)
              .fold[Statement](MaterialEvents(value.available, events))(Material(value.available, _))).toList
          }
          Section(value :: prices)
        }
        val lead = core.take(1) ++ added ++ core.drop(1)
        RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response", lead, previous.details)
          .map(answer => answer.copy(evidence = (previous.evidence ++ answer.evidence ++
            options.flatMap(_.proof.evidence)).distinct,
            completionSources = Some(lead.flatMap(_.statements))))
          .filter(answer => RootQuestionSummary.realize(paths, pair, answer).nonEmpty)
      }
    }
    val central = supplementedPurpose.getOrElse(if previousPurpose.nonEmpty || conditions.size != 1 then previous else
      val value = values.head
      val recorded = previous.core ++ previous.details
      val detail = Section(recorded.flatMap(_.statements).distinct,
        recorded.flatMap(_.required).distinct, recorded.flatMap(_.focus).distinctBy(_.move))
      val immediate = pair.filter(path => path.certified.exists { (_, fact) =>
        fact.material.exists(_.events.exists(_.plyOffset == 1)) ||
          fact.lineEvents.exists(event => event.plyOffset == 1 && event.kind == LineEventKind.Check)
      }).map(MoveRef(_, 1))
      RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response",
        Section(List(value)) :: Option.when(immediate.nonEmpty)(Section(List(Continuation(immediate)))).toList,
        List(detail)).map(answer => protect(answer.copy(
          evidence = (answer.evidence ++ previous.evidence ++ value.proof.evidence).distinct), previous.core))
        .getOrElse(previous))
    // Supplement an existing judgment with a checked conditional example.
    // If its complete proof cannot fit the unchanged delivery contract, it
    // remains internal rather than suppressing the already deliverable core.
    facts.foldLeft(central) { (answer, value) =>
      val section = Section(List(value))
      if answer.details.exists(_.statements.contains(value)) then answer else
        val supplemented = answer.copy(details = answer.details :+ section,
          evidence = (answer.evidence ++ value.proof.evidence).distinct)
        RootQuestionSummary.realize(paths, pair, supplemented).fold(answer)(_ => supplemented)
    }
