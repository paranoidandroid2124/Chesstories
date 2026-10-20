package io.chesstory.runtime

import lila.chessjudgment.model.judgment.{EvidencePieceRole, EvidenceRef, LineEventKind, LineMaterialCapture, LineMaterialRecaptureStatus,
  RelationCheckResponseMode, RelationColoredPieceWitness}
import lila.chessjudgment.model.line.AutomaticTerminal

/** Realization of selected chess content. Wording cannot select another event
  * or remove a reply required by the answer.
  */
private[runtime] object CommentaryEnglish:
  import CommentaryAnswer.*
  import MoveWorkProjection.*
  import MoveWorkProjection.Detail.*
  import RootQuestionSummary.MoveRef
  type Part = String | MoveRef
  case class Rendered(parts: List[Part], expressed: List[Statement])

  def openingExchange(captures: List[LineMaterialCapture], moves: List[String]): String =
    val actions = captures.map { capture =>
      s"${if capture.side.white then "White" else "Black"} ${capture.attackerRole.name} " +
        s"${if capture.recapture then "recaptures" else "captures"} the ${if capture.side.white then "black" else "white"} " +
        s"${capture.capturedRole.name} on ${capture.square.key} with ${moves(capture.plyOffset)}"
    }
    s"the opening exchange is: ${actions.mkString("; ")}. This is the observed sequence, not a forced line."

  extension (p: Piece)
    def at: String = p.square.key
    def roleName: String = p.role.name
    def sideName: String = if p.side.white then "white" else "black"
    def name: String = s"${p.sideName.capitalize} ${p.roleName} on ${p.at}"
    def possessiveName: String = s"${p.sideName.capitalize}'s ${p.roleName} on ${p.at}"

  private def pieceName(piece: Piece, natural: Boolean): String =
    if natural then piece.possessiveName else piece.name

  def description(value: MoveWorkProjection.Detail, natural: Boolean = false): String = value match
    case Development(role, from, to, squares) => s"Develops the $role from $from to $to" +
      (if squares.isEmpty then "." else s", controlling ${squares.mkString(" and ")}.")
    case Castle(from, to) => s"Castles, bringing the rook from $from to $to."
    case Capture(role, at, recapture) =>
      s"${if recapture.isInstanceOf[LineMaterialRecaptureStatus.Proven] then "Recaptures" else "Captures"} the $role on $at."
    case Evades(target, attacker) => s"Moves the ${target.roleName} out of the attack from ${attacker.possessiveName}."
    case Attack(actor, target, gained) => s"${actor.possessiveName} ${if gained then "now targets" else "keeps targeting"} ${target.possessiveName}."
    case Support(actor, target, opposition, gained) =>
      s"${actor.possessiveName} ${if gained then "adds support to" else "keeps supporting"} ${target.possessiveName}" +
        (if opposition.isEmpty then "." else s", which is under attack from ${opposition.map(_.possessiveName).mkString(", ")}.")
    case RookConnection(actor, target) => s"Connects the rooks on ${actor.at} and ${target.at}."
    case PassedPawn(to, Some(true)) => s"Advances the passed pawn to $to."
    case PassedPawn(to, Some(false)) => s"Creates a passed pawn on $to."
    case PassedPawn(to, None) => s"The pawn on $to is a passed pawn."
    case PawnStructure(pawns, doubled) =>
      if doubled then s"Leaves doubled ${pawns.head.sideName} pawns on ${pawns.map(_.at).mkString(" and ")}."
      else s"The ${pawns.head.sideName} pawn on ${pawns.head.at} is no longer doubled."
    case KingEntry(actor, squares) =>
      s"${actor.possessiveName} now controls ${squares.mkString(", ")} next to the opposing king; the king cannot enter ${if squares.size > 1 then "these squares" else "this square"}."
    case PawnBlock(to, pawn, destination) => s"Occupies $to, blocking ${pawn.possessiveName} from advancing to $destination."
    case OpenedRay(from, actor, end) => s"Clears $from, opening a line for ${actor.possessiveName} toward $end."
    case ChangedLines(closed, opened, blockedSquares) =>
      (distinctClosedLines(closed, blockedSquares).map { (actor, target, remaining) =>
        s"Blocks the line from ${pieceName(actor, natural)} to ${pieceName(target, natural)}" +
          (if remaining.isEmpty then "." else
            s"; ${remaining.map(pieceName(_, natural)).mkString(" and ")} still ${if actor.side == target.side then "support" else "attack"}${if remaining.size == 1 then "s" else ""} that target.")
      } ++ opened.map { (actor, target) => s"Opens the line from ${pieceName(actor, natural)} to ${pieceName(target, natural)}." } ++
        blockedSquares.map { (actor, square, _) => s"Blocks the line from ${pieceName(actor, natural)} toward $square." }).mkString(" ")
    case RookFile(actor, fileType, Some((square, helpers))) =>
      s"Uses the $fileType ${actor.at.head}-file: ${pieceName(actor, natural)} controls $square, also supported by ${helpers.map(pieceName(_, natural)).mkString(", ")}."
    case RookFile(actor, fileType, None) => s"Places the rook on the $fileType ${actor.at.head}-file."

  def after(value: MoveWorkProjection.Detail, move: String, description: String): String = value match
    case Attack(_, _, _) | Support(_, _, _, _) | KingEntry(_, _) | PawnStructure(_, false) | PassedPawn(_, None) =>
      s"After $move, ${description.stripPrefix("The ") match
        case rest if description.startsWith("The ") => "the " + rest
        case _ => description}"
    // Several independent clauses can occur here; a move label keeps their subjects explicit.
    case ChangedLines(_, _, _) => s"$move: $description"
    case _ => s"$move ${description.take(1).toLowerCase(java.util.Locale.ROOT)}${description.drop(1)}"

  // A reply's empty square represents the same obstruction to a farther target
  // on this certified ray. Keep the raw relations, but do not count one cost twice.
  private def distinctClosedLines(closed: List[(Piece, Piece, List[Piece])],
    blockedSquares: List[(Piece, String, List[String])]): List[(Piece, Piece, List[Piece])] =
    closed.filterNot((actor, target, _) => blockedSquares.exists((provider, _, ray) =>
    provider == actor && ray.contains(target.at)))

  def currentFunction(details: List[Detail], moves: List[String], later: Option[(String, Piece)]): String =
    val description = details.head match
      case PawnBlock(to, pawn, _) =>
        val destinations = details.collect { case PawnBlock(_, _, destination) => destination }.distinct
        s"Occupies $to, blocking ${pawn.possessiveName} from advancing to ${destinations.mkString(" or ")}."
      case value => CommentaryEnglish.description(value)
    if moves.size > 1 then
      val Attack(_, target, _) = details.head: @unchecked
      s"Both ${moves.mkString(" and ")} attack ${target.possessiveName}."
    else later.fold(after(details.head, moves.head, description)) { (root, actor) =>
      s"Later in the $root line, ${moves.head} (${actor.possessiveName}): $description"
    }

  def materialDescription(signature: CommentarySelection.MaterialSignature): String =
    val (losses, promotions) = signature
    val rows = List("white", "black").flatMap { side =>
      val pieces = losses.collect { case (`side`, role, count) =>
        if count == 1 then s"one $role" else s"$count ${role}s"
      }
      val listed = if pieces.size < 3 then pieces.mkString(" and ")
        else pieces.init.mkString(", ") + ", and " + pieces.last
      Option.when(pieces.nonEmpty)(s"${side.capitalize} loses $listed")
    } ++ promotions.map { (side, before, after, count) =>
      s"${side.capitalize} promotes $count ${if count == 1 then before else s"${before}s"} to ${if count == 1 then s"a $after" else s"${after}s"}"
    }
    if rows.isEmpty then "neither side loses material or promotes" else rows.mkString("; ")

  private val numbers = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five",
    6 -> "six", 7 -> "seven", 8 -> "eight")

  private def counted(roles: List[(EvidencePieceRole, Int)]): String = roles.map { (role, count) =>
    numbers.getOrElse(count, count.toString) + " " + role.name + (if count == 1 then "" else "s")
  }.mkString(" and ")

  private def sequence(refs: List[MoveRef]): List[Part] = refs.zipWithIndex.flatMap { (ref, index) =>
    (if index == 0 then Nil else List(if refs(index - 1).offset + 1 == ref.offset then " " else " … ")) :+ ref
  }

  private def controlRelation(actor: Piece, target: Option[Piece], square: String, present: Boolean): String =
    val verb = target.fold("control")(piece => if actor.side == piece.side then "support" else "attack")
    actor.possessiveName + " " + (if present then verb + "s" else "does not " + verb) + " " + target.fold(square)(_.possessiveName)

  private def originalName(piece: Piece): String =
    "the " + piece.sideName + " " + piece.roleName + " originally on " + piece.at

  private def presenceState(original: Piece, current: Piece): String =
    (if original.role == current.role then "is still present"
      else "has promoted to a " + current.roleName + " and is still present") + " on " + current.at

  private def supportedCapture(value: SupportedCapture, introduced: Boolean = false): List[Part] =
    val SupportedCapture(move, provider, target, use, fromStart, arrival, arrivalCapture) = value
    val start: List[Part] = if fromStart then
      val support: List[Part] = if introduced then List("That support continues")
        else List("In the ", move, " line, " + provider.possessiveName + " keeps supporting " + target.possessiveName)
      if target == use.target then support
      else support ++ List(" as ") ++ arrival.toList ++ List(arrivalCapture.fold(
        " moves that " + target.roleName + " to " + use.target.at)(detail =>
        " " + description(detail).stripSuffix(".").toLowerCase(java.util.Locale.ROOT)))
    else List("In the ", move, " line, " + provider.possessiveName + " controls the empty " + use.target.at +
      " square; ") ++ arrival.toList ++ List(" brings " + use.target.possessiveName + " there under its support")
    val stationaryTarget = fromStart && target == use.target
    start ++ List(if stationaryTarget then " until " else ". After ",
      MoveRef(move.path, use.targetCapture.plyOffset), " captures the supported " +
      use.target.roleName + (if stationaryTarget then "; " else ", "), MoveRef(move.path, use.recapture.plyOffset),
      " recaptures its capturer with the supporting " + use.provider.roleName + ".")

  def clearedRouteUse(value: ClearedRoute, squareIntroduced: Boolean = false): List[Part] =
    val action = if value.passage then "passes through " +
      (if squareIntroduced then "that square" else "the vacated " + value.before.at + " square")
    else "uses the cleared line to target " + value.targets.map(_.possessiveName).mkString(" and ")
    List(value.use, ": " + value.user.possessiveName + " " + action + " in this line.")

  def checkDetail(value: CheckEvent, natural: Boolean = false): String = value match
    case CheckingMove(_, detail, opened) =>
      val king = s"${if detail.checkedSide.white then "White" else "Black"} king on ${detail.kingSquare.key}"
      val checkers = detail.checkers.map(piece => RelationColoredPieceWitness(piece.square, piece.role, detail.mover.side))
      if opened.nonEmpty then
        s"opens ${opened.map(pieceName(_, natural)).mkString(" and ")}'s line, giving check to the $king" +
          (if checkers.toSet == opened.toSet then "" else
            s"; ${checkers.filterNot(opened.contains).map(pieceName(_, natural)).mkString(" and ")} also gives check")
      else s"gives check to the $king with ${checkers.map(pieceName(_, natural)).mkString(" and ")}"
    case CheckReply(_, detail) =>
      val resource = detail.response.resource
      val modes = detail.response.modes
      List(
        Option.when(modes.contains(RelationCheckResponseMode.KingMove))(s"moves the king to ${resource.movement.to.key}"),
        Option.when(modes.contains(RelationCheckResponseMode.CaptureChecker))(resource.capture.map(capture =>
          s"captures the checking ${capture.capturedRole.name} on ${capture.capturedSquare.key}")).flatten,
        Option.when(modes.contains(RelationCheckResponseMode.Interpose))(s"blocks the check on ${resource.movement.to.key}")
      ).flatten.mkString(" and ")

  def material(value: Material, boundLaterChanges: Boolean = true): List[Part] =
    val cost = value.cost
    val common = Option.when(cost.commonExchanges.nonEmpty)("both sides exchange " + counted(cost.commonExchanges))
    val extra = if cost.whiteAdditionalLosses.nonEmpty && cost.blackAdditionalLosses.nonEmpty then
      Some("White gives up " + counted(cost.whiteAdditionalLosses) + " for " + counted(cost.blackAdditionalLosses))
    else if cost.whiteAdditionalLosses.nonEmpty then
      Some("White " + (if common.nonEmpty then "also " else "") + "loses " + counted(cost.whiteAdditionalLosses))
    else if cost.blackAdditionalLosses.nonEmpty then
      Some("Black " + (if common.nonEmpty then "also " else "") + "loses " + counted(cost.blackAdditionalLosses))
    else None
    val description = common.toList ++ extra
    if description.isEmpty then Nil else
      val end = cost.offsets.max
      val later = boundLaterChanges && value.path.certified.flatMap(_._2.material).exists(_.events.exists(_.plyOffset > end))
      List("In the ", MoveRef(value.path, 0)) ++
        (if later then List(" continuation through ", MoveRef(value.path, end), ", ") else List(" line, ")) ++
        List(description.mkString("; ") + ".")

  private def checkingCapture(material: MaterialEvents, next: Statement): Option[List[Part]] =
    val response = next match
      case reply: CheckReply => Some(reply -> Option.empty[MoveRef])
      case CheckSequence(anchor, List(reply: CheckReply)) if anchor.path eq material.path => Some(reply -> Some(anchor))
      case _ => None
    for
      event <- material.events.headOption.filter(_ => material.events.size == 1)
      if event.promotionGainCp == 0 && event.movement.beforeRole == event.movement.afterRole
      capture <- event.capture
      (reply, anchor) <- response
      if (reply.move.path eq material.path) && reply.move.offset == event.plyOffset
      if reply.detail.response.modes == List(RelationCheckResponseMode.CaptureChecker)
      resource = reply.detail.response.resource
      if resource.movement == event.movement && EvidenceRef.sameMove(resource.moveUci, event.moveUci)
      if material.path.moves.lift(event.plyOffset).exists(EvidenceRef.sameMove(_, event.moveUci))
      checkedCapture <- resource.capture
      if checkedCapture.capturedSide != capture.side && checkedCapture.capturedRole == capture.capturedRole &&
        checkedCapture.capturedSquare == capture.square
    yield
      anchor.toList.flatMap(ref => List("In the provided ", ref, " line: ")) ++
        List(reply.move, (if capture.recapture then " recaptures" else " captures") + " the checking " +
          (if checkedCapture.capturedSide.white then "white" else "black") + " " + capture.capturedRole.name +
          " on " + capture.square.key + ".")

  private def materialEvents(value: MaterialEvents, attacking: Set[Int] = Set.empty): List[Part] =
    CommentarySequenceText.groups(value.events)(CommentarySequenceText.linked).flatMap { events =>
      val summary = Option.when(!events.exists(e => attacking(e.plyOffset)))(
        CommentarySequenceText.exchange(value.path, events, ko = false)).flatten
      summary.getOrElse(detailedMaterialEvents(value.copy(events = events), attacking)) :+ " "
    }.dropRight(1)

  private def detailedMaterialEvents(value: MaterialEvents, attacking: Set[Int]): List[Part] =
    value.events.zipWithIndex.flatMap { (event, index) =>
      val capture = event.capture.map(c =>
        s"${if c.recapture then "recaptures" else "captures"} the ${if attacking(event.plyOffset) then "attacking " else ""}${if c.side.white then "black" else "white"} " +
          s"${c.capturedRole.name} on ${c.square.key}")
      val promotion = Option.when(event.promotionGainCp > 0)("promotes to a " + event.movement.afterRole.name)
      (if index == 0 then Nil else List(" ")) ++ List(MoveRef(value.path, event.plyOffset), " " +
        (capture.toList ++ promotion).mkString(" and ") + ".")
    }

  private def pieceExchange(value: PieceExchange, attacking: Set[Int] = Set.empty): List[Part] =
    val ledger = value.path.certified.toList.flatMap(_._2.material).flatMap(_.events).map(e => e.plyOffset -> e).toMap
    CommentarySequenceText.groups(value.events)((a, b) =>
      a.originalActor.nonEmpty && a.originalActor == b.originalVictim &&
        ledger.get(a.plyOffset).exists(left => ledger.get(b.plyOffset).exists(CommentarySequenceText.linked(left, _))))
      .flatMap { events =>
        val summary = Option.when(!events.exists(e => attacking(e.plyOffset)))(
          CommentarySequenceText.exchange(value.path, events.flatMap(e => ledger.get(e.plyOffset)),
            ko = false, physical = Some(events))).flatten
        summary.getOrElse(detailedPieceExchange(value.copy(events = events), attacking)) :+ " "
      }.dropRight(1)

  private def detailedPieceExchange(value: PieceExchange, attacking: Set[Int]): List[Part] =
    val PieceExchange(path, events, recaptureOffsets) = value
    events.zipWithIndex.flatMap { (event, index) =>
      val actor = event.originalActor.fold("the moving piece")(originalName)
      // An immediately preceding capturer has one certified physical identity;
      // its current role can differ from its original role after promotion.
      val takesPrecedingActor = index > 0 && event.originalVictim.nonEmpty &&
        event.originalVictim == events(index - 1).originalActor
      val modifier = if attacking(event.plyOffset) then "attacking " else ""
      val victim = if takesPrecedingActor then "that " + modifier + path.certified.flatMap(_._2.material)
        .flatMap(_.captures.find(_.plyOffset == event.plyOffset)).fold("piece")(_.capturedRole.name)
        else event.originalVictim.fold("the opposing piece")(piece =>
          "the " + modifier + originalName(piece).stripPrefix("the "))
      (if index == 0 then List("With ") else if takesPrecedingActor then List("; with ") else List(". With ")) ++
        List(MoveRef(path, event.plyOffset), ", " + actor +
          (if recaptureOffsets(event.plyOffset) then " recaptures " else " captures ") + victim)
    } ++ Option.when(events.nonEmpty)(".").toList

  /** A capture can express an evasion only when it actually removes that
    * exact attacker, with the same moving piece and occurrence. Other attacks
    * on the mover, and actions in another path, remain independent claims. */
  private def capturesAttacker(value: Statement, evasion: Action): Boolean =
    val captures = value match
      case value: MaterialEvents => Some(value.path -> value.events)
      case value: PieceExchange => value.path.certified.flatMap(_._2.material).map(material =>
        value.path -> material.events.filter(event => value.events.exists(owned =>
          owned.plyOffset == event.plyOffset && owned.originalVictim.nonEmpty)))
      case _ => None
    (captures, evasion.detail) match
      case (Some((path, events)), Evades(target, attacker)) if evasion.move.path eq path =>
        events.exists(event => event.plyOffset == evasion.move.offset && event.promotionGainCp == 0 &&
          path.moves.lift(event.plyOffset).exists(EvidenceRef.sameMove(_, event.moveUci)) &&
          event.movement.side == target.side && event.movement.afterRole == target.role &&
          event.movement.to == target.square && event.capture.exists(capture =>
            capture.side != attacker.side && capture.capturedRole == attacker.role && capture.square == attacker.square))
      case _ => false

  private def exchangeActivity(q: MovePurpose.Question, state: ExchangeActivity.State): String =
    val task = q.activity.get
    val successor = if state.mover.isEmpty then state.successor.fold(
      " The original " + q.actor.role.name + " has been exchanged; its old square is no longer occupied by a friendly piece.")(piece =>
      " " + piece.possessiveName + " has taken over " + q.square + " after the " + q.actor.role.name + " exchange.")
    else " The original " + q.actor.role.name + " remains in play."
    val file = state.file.map(work =>
      " " + work.actor.possessiveName + " uses the open " + work.file + "-file" +
        work.opponent.fold(".")(p => " to confront " + p.possessiveName + "; the file is still contested.")).getOrElse("")
    val deployment = if state.retreats.nonEmpty && state.central.nonEmpty then
      " " + state.central.map(_.possessiveName).mkString(" and ") +
        (if state.central.size == 1 then " is" else " are") + " already active in the centre, while " +
        state.retreats.map(r => r.current.possessiveName + " has returned from " + r.original.at +
          (if r.exits.isEmpty then "" else " and needs another move to come forward, for example toward " + r.exits.take(2).mkString(" or ")) +
          (if r.contested.isEmpty then "" else "; " + r.contested.flatMap(_._2).distinct.map(_.possessiveName).mkString(" and ") +
            (if r.contested.flatMap(_._2).distinct.size == 1 then " controls " else " control ") +
            r.contested.map(_._1).mkString(" and "))).mkString("; ") +
        ". This is the deployment cost left by this continuation."
    else ""
    val pawn = state.pawn.map(work => " The pawn advance to " + work.actor.at + " brings it into contact with " +
      work.targets.map(_.possessiveName).mkString(" and ") +
      (if work.actor.side == q.actor.side then ", beginning the pawn confrontation." else
        ", so the opponent has started the pawn confrontation first.") +
      (if work.continuing && work.actor.side == q.actor.side && work.guards.nonEmpty then " " +
        work.guards.map(_.possessiveName).mkString(" and ") + " control " + work.next +
        " for a possible further advance; those pieces retain a role in the pawn plan." else "")).getOrElse("")
    val queens = if task.openFile.nonEmpty && q.actor.role.name == "queen" then
      if state.queensGone then " Both queens are gone: the opponent's queen can no longer use the open file or attack when the pawns open lines. The remaining pieces and pawns carry the plan."
      else " Queens remain, so the opponent still has queen counterplay when pawn exchanges open lines; this is not the same result as exchanging queens."
    else ""
    val changed = state.targets.collect { case (old, Some(now)) if old.at != now.at =>
      " The original " + old.at + " " + old.role.name + " has moved to " + now.at + "; pressure on its former square is no longer the same attack."
    }.mkString
    successor + file + deployment + pawn + queens + changed

  def statement(value: Statement): List[Part] = value match
    case PreparedIdea(value) => MoveIntent.purpose(value, ko = false)
    case PreparedReply(value) => MoveIntent.reply(value, ko = false)
    case ReferencePurpose(value) => ReferencePurposeText.purpose(value, ko = false)
    case ActivityUse(value, shared) => ReferencePurposeText.activityUse(value, ko = false, shared)
    case ReferencePosition(value, arm) => ReferencePurposeText.position(value, arm, ko = false)
    case ReferenceContrast(value) => ReferencePurposeText.contrast(value, ko = false)
    case value: ReferenceExchangeReply => ReferencePurposeText.exchangeReply(value, ko = false)
    case ChoicePurpose(value) =>
      val q = value.question
      List[Part](MoveRef(value.arms.head.path, 0), " and ", MoveRef(value.arms.last.path, 0),
        " both occupy " + q.square + "; the difference is what remains after the opponent exchanges or reorganizes.") ++
        Option.when(value.escaped.nonEmpty)(List[Part](" ", MoveRef(value.arms.head.path, 0), " also moves the " +
          q.actor.role.name + " out of the attack from " + value.escaped.map(_.possessiveName).mkString(" and ") + ".")).toList.flatten
    case ChoiceNeutralization(value) =>
      List[Part]("In the recorded ", MoveRef(value.alternative.path, 0), " line, exchanging the " + value.guard.at + " " +
        value.guard.role.name + " removes the guard of " + value.post + ": the opposing " + value.target.role.name +
        " reaches it without any " + value.guard.sideName + " piece controlling it. Against ", MoveRef(value.reply.path, 0),
        ", the ", MoveRef(value.reply.path, 1), " continuation keeps " + value.retainedGuard.possessiveName +
        " controlling " + value.post + "; that opposing " +
        value.target.role.name + " instead ends on " + value.opponent.at + " after the exchange.")
    case ChoiceReply(value, arm) =>
      val q = value.question
      val alternative = arm.actor != q.actor
      val blocked = if arm.blocksRoute && arm.constrained.nonEmpty then " Moving the " + arm.actor.role.name + " there leaves the original " +
        q.actor.role.name + " on " + q.actor.at + " and occupies its route to " + q.square + "." else ""
      val retained = if !alternative then "" else arm.retained.current.fold(
        " In this continuation that original " + q.actor.role.name + " is subsequently captured.")(p =>
        " The original " + q.actor.role.name + " remains on " + p.at +
          (if arm.retained.open.isEmpty then "; its unoccupied or enemy-occupied destinations are all controlled by the opponent." else "."))
      val activeTargets = arm.state.targets.collect {
        case (original, Some(current)) if original != current && Set("bishop", "knight")(current.role.name) &&
            Set('c', 'd', 'e', 'f')(current.at.head) &&
            (if current.side.white then Set('4', '5', '6')(current.at.last) else Set('3', '4', '5')(current.at.last)) =>
          " The opposing " + original.at + " " + original.role.name + " reaches " + current.at +
            " and retains a central post in this line."
      }.mkString
      val entries = arm.entry.map(entry => " After these exchanges, the earlier double attack via " + entry.square +
        " no longer has both targets: " + (if entry.remaining.isEmpty then "neither remains" else
          "only " + entry.remaining.map(_.possessiveName).mkString(" and ") + " remains") +
        ". Legal entry alone does not prove a gain.").mkString
      List[Part]("For ", MoveRef(arm.path, 0), ":" + blocked + retained +
        exchangeActivity(q.copy(path = arm.path, actor = arm.actor), arm.state) + activeTargets + entries) ++
        arm.constrained.toList.flatMap { (offset, _) => List[Part](" Before ", MoveRef(arm.path, offset),
          ", its route to " + q.square + " is occupied by its own piece, and every available destination of that " +
          q.actor.role.name + " is controlled by the opponent. The recorded continuation pays for this restricted deployment.") }
    case FunctionPurpose(value) if value.witnessedPlan.nonEmpty =>
      ReferencePurposeText.functionalPurpose(value, ko = false)
    case FunctionPurpose(value) if value.question.activity.nonEmpty =>
      val q = value.question
      val task = q.activity.get
      val purpose = task.openFile.fold(
        " challenges " + task.targets.map(_.possessiveName).mkString(" and ") +
          " while allowing an exchange on " + q.square +
          ". The point is to preserve active pieces after simplification; the successor and the opponent's remaining deployment decide whether that works.")(
        file => " contests the open " + file + "-file against " + task.targets.map(_.possessiveName).mkString(" and ") +
          ", offering a " + q.actor.role.name + " exchange. Exchanging removes that opposing piece's counterplay; retreating keeps it for the later pawn struggle.")
      val continuity = value.activityContinuity.filter(_ => value.choice.isEmpty).toList.map {
        case ExchangeActivity.Continuity.Deployment(opponent) =>
          " Both replies below leave the original " + opponent.at + " " + opponent.role.name +
            " on the back ranks while " + q.actor.sideName.capitalize + " already has a minor piece in the centre." +
            " They preserve that deployment idea through different exchanges, not the original " + q.actor.role.name + "'s exact attack."
        case ExchangeActivity.Continuity.PawnPressure(square, targets, next) =>
          " Both replies below retain the pawn plan: advance to " + square + " against " +
            targets.map(_.possessiveName).mkString(" and ") + ", with pieces controlling " + next +
            "." + (if value.branches.flatMap(_.activity).map(_.queensGone).distinct.size == 2 then
              " The pawn plan matches, while queen counterplay differs." else "")
      }
      List[Part](MoveRef(q.path, 0), purpose) ++ continuity
    case FunctionReply(value, branch) if value.question.activity.nonEmpty =>
      List[Part]("After ", MoveRef(branch.path, 1), ", at the end of this continuation:" +
        exchangeActivity(value.question, branch.activity.get))
    case FunctionReply(value, branch) =>
      ReferencePurposeText.functionalReply(value, branch, ko = false)
    case FunctionPurpose(value) if value.question.support.nonEmpty =>
      val q = value.question
      val task = q.support.get
      val pending = task.post != q.rootMove.slice(2, 4)
      val release = task.release
      List[Part](MoveRef(q.path, 0),
        (if pending then " brings the " + q.actor.role.name + " closer to " + task.post +
          ", where it could help guard " else " adds a guard to ") + task.asset.possessiveName +
          ". The plan is to keep this passed pawn supported while a rook becomes active elsewhere. " +
          task.blockader.possessiveName + (if task.asset.at.last == (if task.asset.side.white then '7' else '2') then
            " blocks its promotion; keeping the pawn keeps that " + task.blockader.role.name + " occupied with the blockade. " else
            " blocks its advance; keeping the pawn maintains pressure on that blockade. ") +
          task.guards.map(_.possessiveName).mkString(" and ") +
          (if task.guards.size == 1 then " currently supports the pawn." else " currently support the pawn.") +
          (if pending then " The king's takeover is still pending." else "") +
          " A possible rook transfer is " + release.rook.at + "–" + release.move.slice(2, 4) +
          ", aiming down the " + release.entry.head + "-file toward " + release.entry +
          ". That move was already legal; the added support concerns who can keep guarding the pawn." +
          (if release.clearance.isEmpty then "" else " Continuing down that file requires " +
            release.clearance.map(_.possessiveName).mkString(" and ") + " to clear the way.") +
          " This is a conditional plan, not a forced continuation.")
    case FunctionPurpose(value) if value.question.plan.nonEmpty =>
      val q = value.question
      val plan = q.plan.get
      val meeting: List[Part] = if plan.path == q.path then Nil else
        List(" The move orders ") ++ sequence((0 until plan.join).map(MoveRef(q.path, _)).toList) ++
          List(" and ") ++ sequence((0 until plan.join).map(MoveRef(plan.path, _)).toList) ++
          List(" reach the same arrangement; the second line shows the longer preparation.")
      val cleared = (plan.start + 1 until plan.use).filter(offset => offset % 2 == 0 &&
        !plan.route.contains(offset) && plan.route.exists(later => later > offset &&
          plan.path.moves(later).slice(2, 4) == plan.path.moves(offset).take(2))).toList
      val preparation = cleared.flatMap(offset => List[Part](" ", MoveRef(plan.path, offset),
        " first clears " + plan.path.moves(offset).take(2) + " for that route.")) ++
        plan.clearance.map(piece => " This requires " + piece.possessiveName + " to leave " + piece.at + " first.")
      val retained = value.retained.flatMap(duty => List[Part](" This leaves " + duty.actor.possessiveName +
        " attacking " + duty.targets.map(_.possessiveName).mkString(" and ") +
        (if duty.actor.role.name == "rook" && duty.targets.exists(_.at.head == duty.actor.at.head) then
          s" along the ${duty.actor.at.head}-file" else "") +
        (if duty.guards.isEmpty then "." else ", while guarding " + duty.guards.map(_.possessiveName).mkString(" and ") + ".")))
      val guards = value.released.flatMap(guard =>
        List[Part](" The rearrangement gives up the original " + q.actor.role.name + "'s guard of " +
          guard.target.possessiveName +
          (if guard.remaining.isEmpty then ", leaving it without geometric support." else
            "; " + guard.remaining.map(_.possessiveName).mkString(" and ") +
              (if guard.remaining.size == 1 then " still provides support." else " still provide support."))) ++
          guard.restored.toList.flatMap(offset => List[Part](" ", MoveRef(plan.path, offset), " restores that guard.")))
          ++ value.released.filter(_.restored.isEmpty).flatMap(guard => guard.option.map(square =>
            " A knight on " + square + " would restore the guard of " + guard.target.possessiveName + "."))
      val route = if plan.potential then
        List[Part](" A possible regrouping route is " + plan.squares.mkString("–") +
          "; this is a plan whose execution depends on the replies.")
      else List[Part](" The recorded route is ") ++ sequence(plan.route.map(MoveRef(plan.path, _))) ++ List(".")
      val exchange = Option.when(q.exchangeTarget.nonEmpty)(
        " From " + q.square + ", the knights would attack one another, offering an exchange.").toList
      List[Part](MoveRef(q.path, 0), (if plan.potential then " opens a regrouping idea for the " else " starts redeploying the ") +
        q.actor.role.name + " from " + q.actor.at +
        (if plan.targets.forall(_.side == q.actor.side) then " to restore support of " else " to work against ") +
        plan.targets.map(_.possessiveName).mkString(" and ") + ".") ++
        retained ++ meeting ++ route ++ preparation ++ exchange ++ guards
    case FunctionPurpose(value) =>
      ReferencePurposeText.functionalPurpose(value, ko = false) ++ value.pawn.toList.map { (pawn, blockader) =>
        s" ${pawn.possessiveName} remains one step from promotion, in front of ${blockader.possessiveName}; preserving this pawn keeps that restriction while the other piece becomes active."
      }
    case ExchangeFunction(value) =>
      value.closed.toList.flatMap { loss =>
        List[Part]("The exchange leaves " + loss.blocker.possessiveName + " blocking the line from " +
          loss.slider.possessiveName + " to " + loss.target.possessiveName +
          ". The advanced piece has gone, but the recapturing pawn keeps that pressure closed.")
      } ++ value.access.toList.flatMap { access =>
        List[Part]("The exchange removes " + access.removed.possessiveName +
          ", which controlled " + access.square + ". After ", access.after,
          ", the recapturing rook can use " + access.square + " to attack " + access.target.possessiveName +
          "; no opposing piece currently controls that entry square. This transfer is an available option.")
      } ++ value.pawnFile.toList.flatMap { file =>
        file.restrictedAdvance.toList.flatMap(pawn => List[Part](MoveRef(value.capture, 0),
          " blocks the forward advance of " + pawn.possessiveName + ". ")) ++
        List[Part]("The advance makes the pawn structure a choice: taking and recapturing opens the " +
          file.file + "-file by removing its pawns. With ", MoveRef(file.decline, 1),
          ", the opponent keeps that file closed: " + file.retained.possessiveName + " is blocked by " +
          file.blocked.possessiveName + " and remains a target of " +
          file.attackers.map(_.possessiveName).mkString(" and ") +
          ". The exchange gives up that pawn target in return for the open file; the closed branch keeps it.")
      }
    case RetainedExchangeFunction(value) =>
      value.kept.toList.flatMap { kept =>
        List[Part](MoveRef(kept.path, 0), " preserves the original " + kept.retained.square.key + " " +
          kept.retained.role.name + " while another can be offered on " + value.capture.moves.head.slice(2, 4) + ". Here ",
          MoveRef(kept.path, kept.exchange), " uses the " + kept.exchanged.role.name + " originally on " +
          kept.exchanged.square.key + ", leaving the " + kept.retained.role.name + " from " +
          kept.retained.square.key + " for ", MoveRef(kept.path, kept.activation), ". After ",
          MoveRef(kept.path, kept.use), ", from " +
          kept.arrived.square.key + " it attacks " +
          (kept.target :: kept.otherTargets).map(_.possessiveName).mkString(" and ") +
          ", retaining another source of pressure on the pawn.") ++ kept.restored.toList.flatMap { (actor, target) =>
          List[Part](" " + actor.possessiveName + " has restored pressure on " + target.possessiveName +
            ", while the retained piece attacks its connected pawn.")
        } ++ List[Part](" The immediate ", MoveRef(value.capture, 0),
          " exchange gives up that piece and this way of continuing the pressure.")
      }
    case FunctionRealization(value) =>
      val stage = value.stage
      val root = MoveRef(stage.path, 0)
      val arrived = MoveRef(stage.path, stage.offset)
      val context: List[Part] = List("Later in the recorded ", root, " continuation, ") ++
        Option.when(stage.offset > 0)(List[Part]("after ", MoveRef(stage.path, stage.offset - 1), ", ")).toList.flatten
      val supporters = stage.providers.map(provider =>
        provider.current.possessiveName + " (originally on " + provider.original.square.key + ")").mkString(" and ")
      val access: List[Part] = List(arrived, " brings the original " + stage.resource.actor.role +
        " from " + stage.resource.actor.square + " to " + stage.resource.square + ". " + supporters +
        " provide" + (if stage.providers.size == 1 then "s" else "") +
        " the current geometric support of that square; the original supporter is not providing it here. ")
      val captures: List[Part] = if value.capturers.isEmpty then
        List("The complete current legal inventory has no immediate capture of this piece.")
      else List("The currently legal capturers are " + value.capturers.map(_.possessiveName).mkString(" and ") + ". ") ++
        value.prices.flatMap { price =>
          val events = price.events.filter(_.plyOffset > stage.offset)
          List[Part]("If this available capture sequence is played: ") ++
            materialEvents(MaterialEvents(price.path, events)) ++
            List(" No immediate legal recapture follows at its recorded endpoint. ")
        }
      context ++ access ++ captures
    case AnchoredDecision(value) =>
      val pawn = value.task.stage.pawn
      val decision: List[Part] = if value.task.use.get.refuted then
        List("prefer ", value.alternative, " to an immediate ", value.choice, ". ")
      else List(value.choice, " remains competitive alongside ", value.alternative, ". ")
      List(value.rootAdvance, " advances the " + pawn.role + " from " + pawn.square +
        "; ", value.rootDeferred, " defers that advance. After ") ++ sequence(value.prefix) ++
        List(", ") ++ decision ++ List("A leading tested reply to ", value.choice,
          " is ", value.replies.head, "; after ", value.alternative, " it is ", value.replies(1), ". ") ++
        value.states.flatMap(value => statement(value) ++ List(" ")) ++
        Option.when(value.laterAdvanceUnestablished)(
          "A later advance of this pawn is not established by the examined continuation of this reply. ").toList
    case FirstReplyChoice(root, preferred, alternative, quality) =>
      val comparison: List[Part] = if quality == PrincipalVariationComparison.QualityBand.Close then
        List(preferred, " and ", alternative, " are competitive replies.")
      else List(preferred, " is the stronger tested reply than ", alternative, ".")
      List("After ", root, ", ") ++ comparison
    case ResourceDecision(value) =>
      val introduction: List[Part] = List("After ", value.sequence.head, " ", value.sequence(1), ", ")
      val condition: List[Part] = value.conditions.flatMap {
        case ControlState(_, actor, _, square, present, _) =>
          List(actor.possessiveName + (if present then " controls " else " does not control ") + square + ". ")
        case claim => statement(claim) ++ List(" ")
      }
      val choice: List[Part] = if value.use.refuted then
        List("Prefer ", value.other.get, " to an immediate ", value.sequence(2), ". ")
      else List(value.sequence(2), " remains competitive") ++ value.other.toList.flatMap(move =>
        List(" alongside ", move)) ++ List(" here. ")
      introduction ++ condition ++ choice ++ value.useRoles.flatMap(claim => statement(claim) ++ List(" ")) ++
        List("A leading tested reply to ", value.sequence(2),
        " is ", value.sequence(3), ".") ++ value.replyRoles.flatMap(value => List(" ") ++ statement(value))
    case value: LegalReplyOption =>
      ReferencePurposeText.legalOption(value, ko = false)
    case value: ReserveBreak =>
      val square = value.actual.moves(value.offset).slice(2, 4)
      val advanced = value.actual.moves.head.slice(2, 4)
      val sequence: List[Part] = value.sequence.flatMap(move => List(" ", move)).drop(1)
      val retained: List[Part] = value.retainedSequence.flatMap(move => List(" ", move)).drop(1)
      List("After ", MoveRef(value.actual, 0), ", the recorded ") ++ sequence ++
        List(s" continuation gives ${if value.passer.side.white then "White" else "Black"} a passed pawn on $square. " +
          s"Advancing the pawn to $advanced has removed its control of $square. ", MoveRef(value.alternative, 0),
          s" keeps that pawn on ${value.guard.at}: after the same ") ++ retained ++
        List(s", it still guards $square. After ", MoveRef(value.alternative, 0), ", the tested ",
          MoveRef(value.held, 1), " evaluates below ", MoveRef(value.otherReply, 1), ".")
    case value: ReserveTempo =>
      val sequence: List[Part] = value.sequence.flatMap(move => List(" ", move)).drop(1)
      List(MoveRef(value.actual, 0), " spends a pawn move that ", MoveRef(value.alternative, 0),
        " keeps available. After ", MoveRef(value.alternative, 0), " ", MoveRef(value.route, 1),
        ", the tested ", MoveRef(value.kept, 2), " evaluates above an immediate ", MoveRef(value.spent, 2),
        ". In the recorded king manoeuvre, ") ++ sequence ++
        List(s" restores the same piece placement with ${if value.pawn.side.white then "Black" else "White"} to move, while the pawn stays on ${value.pawn.at}. " +
          "The king manoeuvre hands over the move without spending the pawn advance.")
    case value: ReserveHold =>
      val offset = value.proof.offset
      val kingSquare = value.retreat.moves(offset).take(2)
      List(MoveRef(value.actual, 0), " spends a pawn move that ", MoveRef(value.alternative, 0),
        " keeps in reserve. In the tested continuation, ", MoveRef(value.waited, offset),
        s" keeps the king on $kingSquare and ${value.square} guarded. The tested king retreat ",
        MoveRef(value.retreat, offset), " gives up that control, allowing ",
        MoveRef(value.retreat, offset + 1), " to enter. In that position, the retained pawn move evaluates above the king retreat.")
    case value: ReserveEntry =>
      val proof = value.proof
      val king = value.entry.moves(proof.last + 2).take(2)
      val sequence: List[Part] = value.sequence.flatMap(move => List(" ", move)).drop(1)
      val counter: List[Part] = value.reply match
        case ReserveReply.PassedPawn(pawn, supported) =>
          List("Against ", MoveRef(value.counter, proof.last + 1),
            ", which keeps that square guarded, ", MoveRef(value.counter, proof.last + 2),
            " leaves a passed pawn on " + pawn.at +
              (if supported then ", supported by the pawn kept in reserve." else "."))
        case ReserveReply.LastPawnWait(offset, square, exchange) =>
          List(MoveRef(value.counter, proof.last + 1), " keeps that square guarded. ") ++
            (if exchange then List(MoveRef(value.counter, proof.last + 2), " recaptures that pawn. ") else Nil) ++
          List("Later in that tested line, ",
            MoveRef(value.counter, offset), s" holds the king's position. The opponent has no legal pawn move left. " +
              "Every legal king reply, including ", MoveRef(value.counter, offset + 1),
              s", gives up $square; the king can enter that square.")
      val entry: List[Part] = if value.available.nonEmpty then
        List("the king can enter with ", MoveRef(value.entry, proof.last + 2), ". ")
      else List(MoveRef(value.entry, proof.last + 2), " enters that square. ")
      List(MoveRef(value.actual, 0), " spends a pawn move that ", MoveRef(value.alternative, 0),
        " keeps in reserve. Later in the analysed ", MoveRef(value.alternative, 0), " continuation, ") ++ sequence ++
        List(s" keeps the king on $king. After ", MoveRef(value.waited, proof.last + 1),
          " gives up control of " + proof.square + ", ") ++ entry ++ counter
    case value: DirectReserveEntry =>
      val proof = value.proof
      val relation = if proof.testAnswer.exists(answer => answer.evaluationRelation == "worse" &&
        answer.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated)) then "evaluates above"
        else "remains comparable to"
      List(MoveRef(value.actual, 0), " spends a pawn move that ", MoveRef(value.alternative, 0),
        " keeps in reserve. Later in the analysed ", MoveRef(value.alternative, 0), " continuation, ", MoveRef(value.entry, proof.offset),
        " leaves the king in place. The opponent has no legal pawn move left: every legal king reply, including ",
        MoveRef(value.entry, proof.offset + 1), ", gives up " + proof.square + ". In that tested position, ",
        MoveRef(value.entry, proof.offset + 2), " " + relation + " ", MoveRef(value.otherwise, proof.offset + 2),
        ".")
    case value: ReplyConstraint =>
      val target = value.proof.challenged.witness
      List(MoveRef(value.actual, 0), s" occupies ${value.proof.square}, taking that retreat square away from " +
        target.possessiveName + ". After ", MoveRef(value.actual, 1), " attacks the " + target.roleName +
        ", the recorded reply is ", MoveRef(value.actual, 2), ". ", MoveRef(value.alternative, 0),
        s" leaves ${value.proof.square} available: against the same ", MoveRef(value.available, 1),
        ", " + (if target.side.white then "White" else "Black") + " can play ", MoveRef(value.available, 2),
        ". In that tested position, ", MoveRef(value.available, 2), " evaluates above ",
        MoveRef(value.otherwise, 2), ".")
    case TargetDefense(move, target, attackers, defenders) =>
      List("After ", move, ", " + target.possessiveName + " is under attack from " +
        attackers.map(_.possessiveName).mkString(" and ") +
        (if defenders.isEmpty then " and has no defender."
         else "; it is supported by " + defenders.map(_.possessiveName).mkString(" and ") + "."))
    case ComparisonSubject(roots, originals) =>
      List("Compare what happens to " + originals.map(originalName).mkString(" and ") + " in the recorded ",
        roots.head, " and ", roots.last, " lines.")
    case Action(move, detail) =>
      val text = description(detail, natural = true)
      detail match
        case Attack(_, _, _) | Support(_, _, _, _) | KingEntry(_, _) | PawnStructure(_, false) | PassedPawn(_, None) =>
          List("After ", move, ", " + (if text.startsWith("The ") then "the " + text.drop(4) else text))
        case ChangedLines(_, _, _) => List(move, ": " + text)
        case _ => List(move, " " + text.take(1).toLowerCase(java.util.Locale.ROOT) + text.drop(1))
    case EnPassantCapture(advance, capture, pawn, capturer, captured, destination) =>
      List(advance, " advances " + originalName(pawn) + " two squares. ", capture,
        " replies en passant: " + originalName(capturer) + " captures that pawn on " + captured.at +
          " and lands on " + destination + ".")
    case value: Material => material(value)
    case MaterialBalance(path, cost, through) => cost match
      case Some(value) => material(Material(path, value), boundLaterChanges = false) match
        case "In the " :: root :: tail if through.nonEmpty =>
          List("Through ", MoveRef(path, through.get), " in the recorded ", root) ++ tail
        case "In the " :: tail => "In the recorded " :: tail
        case parts => parts
      case None => List("In the recorded ", MoveRef(path, 0), " line, no capture or promotion is recorded.")
    case value: TargetOutcome =>
      val target = value.original.sideName.toLowerCase(java.util.Locale.ROOT) + " " + value.original.roleName
      val removal = value.removal.toList.flatMap(_.references).headOption
      val outcome: List[Part] = removal match
        case Some(move) if move == value.root => List(value.root, " captures the " + target + ".")
        case Some(move) => List("In the ", value.root, " line, ", move, " captures that same " + target + ".")
        case None => value.departure match
          case Some(move) => List("In the ", value.root, " line, ", move,
            " moves that same " + target + "; it remains on the board at the end of the recorded line.")
          case None => List("In the ", value.root, " line, that same " + target +
            " remains on the board at the end of the recorded line.")
      outcome
    case value: MaterialEvents => materialEvents(value)
    case value: PieceExchange => pieceExchange(value)
    case RecordedContext(moves) =>
      List("Separately, in the recorded ") ++ sequence(moves) ++ List(" continuation:")
    case value: TargetResponse =>
      val opening: List[Part] = if value.start == 0 then List(MoveRef(value.path, 0))
        else if value.start == 2 then List("After ", MoveRef(value.path, 0), " ", MoveRef(value.path, 1), ", ", MoveRef(value.path, 2))
        else List("In the ", MoveRef(value.path, 0), " line, ", MoveRef(value.path, value.start))
      opening ++ List(" attacks the " + value.target.square.key + "-" + value.target.role.name + ".") ++
        value.escape.toList.flatMap(offset => List(" ", MoveRef(value.path, offset),
          " moves that " + value.target.role.name + " out of the attack.")) ++
        (value.targetCapture.toList ++ value.attackerRemoval.toList).sortBy(_.events.head.plyOffset)
          .flatMap(outcome => List(" ") ++ statement(outcome)) ++
        value.material.toList.flatMap(cost => List(" ") ++ material(cost)) ++
        value.targetEnd.toList.flatMap { presence =>
          if presence.original == value.originalTarget && presence.current.role == value.target.role then
            List(" After ", presence.after, ", the targeted " + value.target.sideName + " " + value.target.roleName +
              " is still present on " + presence.current.at + ".")
          else List(" ") ++ statement(presence)
        } ++
        Option.when(value.survives && value.targetEnd.isEmpty)(
          " That " + value.target.role.name + " remains at the end of the line.").toList
    case RouteUse(move, use, role, opening) =>
      val action = opening match
        case Some(Development(piece, _, to, _)) => " develops the " + piece + " to " + to + ", opening a route"
        case _ => " opens a route"
      List("In this line, ", move, action + " for the " + role + " maneuver ", use, ".")
    case AttackerRemovalOrSupport(removal, supportMove, attacker, target, provider) =>
      List(removal, " captures the opposing " + attacker.square.key + "-" + attacker.role.name + ". ",
        supportMove, " instead adds " + provider.role.name + " support to the attacked " +
          target.square.key + "-" + target.role.name + ".")
    case SupportRecapture(after, witness, use) =>
      List("In a recorded legal continuation after ", after, ", ",
        MoveRef(witness, use.targetCapture.plyOffset), " captures the " + use.originalTarget.role.name + "; ",
        MoveRef(witness, use.recapture.plyOffset), " recaptures that " + use.originalCapturer.role.name + ".")
    case SupportTransfer(move, provider, target, oldProvider, _, departure, use) =>
      List("In the ", move, " line, " + provider.possessiveName + " keeps supporting " + target.possessiveName +
        " after ", departure, " ends the support from " + oldProvider.possessiveName +
        ". When ", MoveRef(move.path, use.targetCapture.plyOffset), " captures that " + target.roleName + ", ",
        MoveRef(move.path, use.recapture.plyOffset), " recaptures its capturer with the supporting " + use.provider.roleName + ".")
    case value: SupportedCapture => supportedCapture(value)
    case SupportRetention(move, _, provider, _, target, _, oldProvider, departure, currentTarget) =>
      val identity = if currentTarget == target then target.possessiveName
        else "the " + target.sideName + " " + target.roleName + " from " + target.at + ", now on " + currentTarget.at
      List("In the ", move, " line, " + provider.possessiveName + " keeps supporting " + identity +
        " even after ", departure, " ends the support from " + oldProvider.possessiveName + ".")
    case BlockedLineTransfer(move, _, blocker, _, slider, departure, arrival, entrant) =>
      List(move, " keeps the line from " + slider.possessiveName + " toward " + entrant.at + " blocked: after ", departure,
        ", " + blocker.possessiveName + " remains in that line as its sole blocker. In the same provided line, ", arrival,
        " brings " + entrant.possessiveName + " there while that line is still blocked.")
    case BlockedAttack(after, attacker, target, blocker) =>
      List("After ", after, ", " + blocker.possessiveName + " blocks the line from " + attacker.possessiveName + " to " + target.possessiveName + ".")
    case value @ ClearedRoute(move, before, mover, _, _, _, _, _, development, preparations) =>
      val developed = development.nonEmpty || Set("bishop", "knight")(mover.roleName) &&
        Set('1', '8')(before.at.last) && mover.at.last != before.at.last
      val opening = if development.nonEmpty then " develops the " + mover.roleName + " to " + mover.at
        else if developed then " brings the " + mover.roleName + " to " + mover.at
        else " clears " + before.at
      (if preparations.nonEmpty then ReferencePurposeText.preparedRoute(value, ko = false)
        else List[Part](move, opening + "; later ")) ++ clearedRouteUse(value, squareIntroduced = !developed)
    case SquareReoccupation(move, _, _, square, use, _, _) =>
      List(move, " clears " + square + "; ", use, " later occupies that square in this line.")
    case PawnAdvanceOption(move, pawn, alternative, advancedPawn, square, use, supported) =>
      List(move, " keeps the " + pawn.at + "-pawn on its starting square with the route to " + square + " clear; ",
        alternative, " commits that same pawn to " + advancedPawn.at + ", from where a one-square advance to " +
          square + " also has a clear route.") ++ use.toList.flatMap { ref =>
        List(" In the provided ", move, " line, ", ref, " uses that two-square advance") ++ supported.toList.flatMap { support =>
          List(", supporting the same pawn from " + support.original.at + ", now on " + support.current.at) ++
            (if support.route.isEmpty then Nil else List(" after ") ++ sequence(support.route))
        } ++ List(".")
      }
    case PawnCaptureChoice(move, capturer, target, pawn, arrived, opposition, alternative) =>
      List(move, " captures the " + target.roleName + " on " + target.at + " with the " + capturer.roleName +
        ", keeping the pawn on " + pawn.at + " to support it on " + arrived.at +
        (if opposition.isEmpty then ". " else ", under attack from " + opposition.map(_.possessiveName).mkString(" and ") + ". "),
        alternative, " captures that same " + target.roleName + " with the " + pawn.at + "-pawn instead.")
    case PawnAdvanceReply(after, pawn, advance) =>
      List("After ", after, ", that same " + pawn.at + "-pawn advances with ", advance, ".")
    case RelativeAlignment(move, actor, front, rear, retained, ended) =>
      List(move, " lines up " + actor.possessiveName + " with " + rear.possessiveName + " through " + front.possessiveName + "; the " +
        front.roleName + " screens the " + rear.roleName + ".") ++
        (if retained.isEmpty then Nil else List(" The alignment remains after ") ++ sequence(retained) ++ List(".")) ++
        ended.toList.flatMap(ref => List(" ", ref, " breaks that alignment in this line."))
    case ReleasedCapture(move, _, pinner, actor, target, reply) =>
      List(move, " moves the king off the line of " + pinner.possessiveName + ", unpinning the " + actor.at + "-" + actor.roleName +
        ". " + actor.possessiveName + " could now capture " + target.possessiveName + " without exposing its king, but the opponent moves first.") ++
        reply.toList.flatMap(ref => List(" The provided reply is ", ref, "."))
    case event: CheckEvent => List(event.move, " " + checkDetail(event, natural = true) + ".")
    case value: CheckSequence => CommentarySequenceText.checkSequence(value, ko = false)
    case Continuation(moves) if moves.size > 1 && moves.forall(_.offset == 0) &&
        moves.map(_.path.positions.head).distinct.size == 1 &&
        moves.map(_.path.moves.head).distinct.size == moves.size =>
      List("The compared root choices are ") ++ moves.zipWithIndex.flatMap { (move, index) =>
        (if index == 0 then Nil else List(if index == moves.size - 1 then " and " else ", ")) :+ move
      } ++ List(".")
    case Continuation(moves) => List("The recorded continuation is ") ++ sequence(moves) ++ List(".")
    case NoMaterialChange(root, end) =>
      if root == end then List("The recorded ", root, " line contains no captures or promotions.")
      else List("In the ", root, " line through ", end, ", neither side loses material or promotes.")
    case value: PieceRoute => value.references match
      case List(first, next) if first.offset == 0 => List("In the provided ", first, " line, the same " + value.original.roleName +
        " next moves with ", next, " to " + next.path.moves(next.offset).slice(2, 4) + ".")
      case _ => List("The same " + value.original.sideName + " " + value.original.roleName + " originally on " +
        value.original.at + " follows ") ++ sequence(value.references) ++ List(" in the recorded line.")
    case ControlState(move, actor, target, square, present, remaining) =>
      val verb = target.fold("control")(piece => if actor.side == piece.side then "support" else "attack")
      val objectName = target.fold(square)(_.possessiveName)
      List("After ", move, ", " + controlRelation(actor, target, square, present) + ".") ++ remaining.toList.map {
        case Nil => " No other " + actor.sideName + " piece " + verb + "s " + objectName + "."
        case pieces => " " + pieces.map(_.possessiveName).mkString(" and ") + " still " + verb +
          (if pieces.size == 1 then "s" else "") + " " + objectName + "."
      }
    case CheckStatus(move, king, checking, reply) =>
      List("After ", move, ", " + king.possessiveName + (if checking then " is in check." else " is not in check.")) ++
        reply.toList.flatMap(ref => List(" The recorded reply is ", ref, "."))
    case RecordedEnd(move) =>
      List("The recorded line stops after ", move, "; no following reply is recorded.")
    case PiecePresence(after, original, current) =>
      List("After ", after, ", " + originalName(original) + " " + presenceState(original, current) + ".")
    case PieceAbsence(after, original) =>
      List("After ", after, ", " + originalName(original) + " is no longer on the board.")
    case AttackWithdrawal(move, before, after, target) =>
      List(move, " moves the " + before.roleName + " from " + before.at + " to " + after.at +
        ", ending its attack on " + target.possessiveName + ".")
    case contrast: ControlContrast =>
      val sameRelation = contrast.actor == contrast.alternativeActor && contrast.target == contrast.alternativeTarget &&
        contrast.square == contrast.alternativeSquare
      val comparison: List[Part] = if sameRelation && contrast.present != contrast.alternativePresent then
        List("After ", contrast.move, ", " + controlRelation(contrast.actor, contrast.target, contrast.square, contrast.present) +
          (if contrast.present then ", but not after " else ", whereas the " + contrast.actor.at + "-" +
            contrast.actor.roleName + " does after "), contrast.alternative, ".")
      else List("After ", contrast.move, ", " + controlRelation(contrast.actor, contrast.target, contrast.square, contrast.present) +
          "; after ", contrast.alternative, ", " + controlRelation(contrast.alternativeActor, contrast.alternativeTarget,
            contrast.alternativeSquare, contrast.alternativePresent) + ".")
      comparison ++
        Option.when(contrast.remaining.nonEmpty)(" " + contrast.remaining.map(_.possessiveName).mkString(" and ") +
          " still " + (if contrast.actor.side == contrast.target.get.side then "support" else "attack") +
          (if contrast.remaining.size == 1 then "s" else "") + " that target.").toList ++
        contrast.reply.toList.flatMap(move => List(" The recorded reply is ", move, "."))
    case Terminal(move, result) =>
      val ending = result match
        case AutomaticTerminal.Checkmate(winner) => (if winner.white then "White" else "Black") + " wins by checkmate"
        case AutomaticTerminal.Stalemate => "the game is drawn by stalemate"
        case AutomaticTerminal.InsufficientMaterial => "the game is drawn because neither side can give checkmate"
        case AutomaticTerminal.FivefoldRepetition => "the game is drawn by fivefold repetition"
        case AutomaticTerminal.SeventyFiveMoveRule => "the game is drawn under the seventy-five-move rule"
      List("After ", move, ", " + ending + ".")
    case Convergence(left, right, fullState) =>
      sequence(left) ++ List(" and ") ++ sequence(right) ++ List(if fullState then
        " reach the same recorded position." else " reach the same arrangement with the same side to move.")
    case ExchangeRetention(trade, retained, role, recapturer) =>
      List("The ") ++ sequence(trade) ++ List(" sequence exchanges " + role + "s; the " + recapturer +
        " recaptures. In the ") ++ sequence(retained) ++ List(" line, both " + role + "s remain.")
    case Divergence(leftRoot, rightRoot, left, right) => (left, right) match
      case (Some(a), Some(b)) => List("Later, the ", leftRoot, " line continues with ", a,
        "; the ", rightRoot, " line instead continues with ", b, ".")
      case (Some(a), None) => List("The ", rightRoot, " line ends there; the ", leftRoot, " line also records ", a, ".")
      case (None, Some(b)) => List("The ", leftRoot, " line ends there; the ", rightRoot, " line also records ", b, ".")
      case _ => Nil

  def section(value: Section): List[Part] = renderedSection(value).parts

  def renderedSection(value: Section, announced: Set[Statement] = Set.empty): Rendered =
    val reference = ReferencePurposeText.groupedPurpose(value, ko = false)
    val aftermath = ReferencePurposeText.groupedPosition(value, ko = false,
      announced.collect { case ReferencePurpose(reading) => reading })
    val replies = MoveIntent.groupedReplies(value, ko = false, announced.flatMap {
      case ReferencePurpose(reading) => reading.preparation
      case PreparedIdea(reading) => Some(reading)
      case _ => None
    })
    val sharedCosts = ReferencePurposeText.groupedCosts(value, ko = false)
    val captures = value.statements.filter {
      case _: MaterialEvents | _: PieceExchange => true
      case _ => false
    }
    val joinedEvasions = value.statements.collect { case action: Action => action }.flatMap(action =>
      captures.find(capturesAttacker(_, action)).map(_ -> action)).groupMap(_._1)(_._2)
    val deferred: Set[Statement] = joinedEvasions.values.flatten.toSet
    val statements = value.statements.filterNot(deferred).filterNot(claim => aftermath.exists(group =>
      group.expressed.contains(claim) && !claim.isInstanceOf[ReferencePosition])).filterNot {
      case route: PieceRoute => value.statements.exists { use =>
        val actor = use match
          case support: SupportedCapture => support.use.recapture.originalActor
          case support: SupportTransfer => support.use.recapture.originalActor
          case _ => None
        // The actual support-use receipt owns these same proof references;
        // the filtered route is not itself credited as expressed.
        actor.contains(route.original) && route.references.forall(use.references.contains)
      }
      case _ => false
    }
    val expressed = scala.collection.mutable.Set.empty[Statement]
    val receipts = scala.collection.mutable.ListBuffer.empty[Statement]
    var precedingPath: Option[ObservedLineProtocol.PreparedPath] = None
    val rendered = statements.flatMap { claim =>
      if expressed(claim) then Nil else
        val consumed = scala.collection.mutable.ListBuffer(claim)
        val parts = claim match
          case _: ReferencePurpose if reference.nonEmpty =>
            consumed ++= reference.get.expressed
            reference.get.parts
          case _: ReferencePosition if aftermath.nonEmpty =>
            consumed ++= aftermath.get.expressed
            aftermath.get.parts
          case FunctionReply(value, branch) if value.question.activity.isEmpty =>
            ReferencePurposeText.functionalReply(value, branch, ko = false,
              announced.collect { case ReferencePurpose(reading) => reading })
          case _: PreparedReply if replies.nonEmpty =>
            consumed ++= replies.get.expressed
            replies.get.parts
          case _: Material if sharedCosts.nonEmpty =>
            consumed ++= sharedCosts.get.expressed
            sharedCosts.get.parts
          case material: MaterialEvents =>
            val evasions = joinedEvasions.getOrElse(claim, Nil)
            consumed ++= evasions
            val index = statements.indexWhere(_ eq claim)
            val joined = statements.lift(index + 1).flatMap(next =>
              checkingCapture(material, next).map(next -> _))
            joined match
              case Some((next, words)) =>
                consumed += next
                words
              case None => materialEvents(material, evasions.map(_.move.offset).toSet)
          case exchange: PieceExchange =>
            val evasions = joinedEvasions.getOrElse(claim, Nil)
            consumed ++= evasions
            pieceExchange(exchange, evasions.map(_.move.offset).toSet)
          case action: Action if CommentarySequenceText.pursuit(statements, action, ko = false).nonEmpty =>
            val joined = CommentarySequenceText.pursuit(statements, action, ko = false).get
            consumed ++= joined.expressed
            joined.parts
          case action @ Action(move, detail) if CommentarySequenceText.relationActor(detail).nonEmpty =>
            val actor = CommentarySequenceText.relationActor(detail).get
            val group = CommentarySequenceText.relations(statements, action, ko = false).get
            val peers = group.expressed.collect { case peer: Action => peer }
            consumed ++= peers
            val introduction = group.parts
            val uses = statements.collect {
              case use: SupportedCapture if !expressed(use) && (use.move.path eq move.path) && use.move.offset == move.offset &&
                  use.fromStart && use.provider == actor &&
                  peers.exists(_.detail match
                    case Support(_, target, _, _) => target == use.target
                    case _ => false) => use
            }
            if uses.size != 1 || peers.count(_.detail.isInstanceOf[Support]) != 1 then introduction else
              consumed += uses.head
              introduction ++ List(" ") ++ supportedCapture(uses.head, introduced = true)
          case action @ Action(_, _: Evades) =>
            val group = CommentarySequenceText.evasions(statements, action, ko = false).get
            consumed ++= group.expressed
            group.parts
          case presence: PiecePresence =>
            val peers = statements.collect {
              case same: PiecePresence if (same.after.path eq presence.after.path) &&
                  same.original == presence.original && same.current == presence.current => same
            }.sortBy(_.after.offset)
            consumed ++= peers
            if peers.size == 1 then statement(claim) else
              List(originalName(presence.original).capitalize + " " +
                presenceState(presence.original, presence.current) + " after ") ++
                peers.zipWithIndex.flatMap((same, index) =>
                  (if index == 0 then Nil else List(" and after ")) :+ same.after) ++ List(".")
          case _ => statement(claim)
        if parts.isEmpty then Nil else
          consumed.distinct.foreach { statement =>
            if expressed.add(statement) then receipts += statement
          }
          val refs = parts.collect { case ref: MoveRef => ref }
          val owner = CommentarySequenceText.rootOwner(refs)
          val header = owner.filter(path => !precedingPath.exists(CommentarySequenceText.sameRoot(_, path)) &&
            !refs.exists(_.offset == 0)).toList.flatMap(path =>
              List("In the ", MoveRef(path, 0), " line: "))
          precedingPath = owner
          List(" ") ++ header ++ parts
    }.drop(1).foldLeft(List.empty[Part]) {
      case ((words: String) :: rest, next: String) => (words + next) :: rest
      case (all, next) => next :: all
    }.reverse
    Rendered(rendered, receipts.toList)
