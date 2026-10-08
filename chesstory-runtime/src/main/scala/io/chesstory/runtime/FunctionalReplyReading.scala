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

  private def read(paths: PreparedPaths, path: PreparedPath, at: Int, predicate: Predicate): Option[String] =
      paths.positionView(path, at).flatMap { view =>
        def piece(original: Piece) = paths.trackedPieceAfter(path, original.witness, at).flatten
        predicate.kind match
          case "location" => predicate.subject.flatMap(piece).map(_.square.key)
          case "control" =>
            for
              actor <- predicate.subject.flatMap(piece)
              square <- predicate.square
              if view.completeKinds(RelationFactKind.GeometricControl)
            yield if new MoveWorkProjection.Position(view).controlledBy(actor)(square) then "present" else "absent"
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
    def read(path: PreparedPath, at: Int, predicate: Predicate) = FunctionalReplyReading.read(paths, path, at, predicate)
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
        (sameRecapture ||
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
        read(value.reference, 0, proof.action).contains("legal")
      case _ => false)

  def relevant(paths: PreparedPaths, pair: List[PreparedPath], value: LegalReplyOption): Boolean =
    pair.find(_.moves.headOption.contains(value.proof.owner.comparison.referenceMove)).exists { owner =>
      pair.find(_ ne owner).exists { peer =>
        value.reference.ids.headOption == owner.ids.headOption &&
          value.reference.positions.headOption == owner.positions.headOption &&
          read(paths, peer, 1, value.proof.condition).nonEmpty &&
          read(paths, peer, 1, value.proof.condition) == read(paths, value.alternative, 1, value.proof.condition)
      }
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
    }.distinct.sortBy(value => (value.proof.reference.size, value.proof.effect.json.toString,
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
    val central = if conditions.size != 1 then previous else
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
        .getOrElse(previous)
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
