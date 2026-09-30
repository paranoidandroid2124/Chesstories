package io.chesstory.runtime

import lila.chessjudgment.model.judgment.{EvidencePieceRole, LineMaterialCapture, LineMaterialRecaptureStatus,
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

  def material(value: Material): List[Part] =
    val cost = value.cost
    val common = Option.when(cost.commonExchanges.nonEmpty)("both sides exchange " + counted(cost.commonExchanges))
    val extra = if cost.whiteAdditionalLosses.nonEmpty && cost.blackAdditionalLosses.nonEmpty then
      Some("White gives up " + counted(cost.whiteAdditionalLosses) + " for " + counted(cost.blackAdditionalLosses))
    else if cost.whiteAdditionalLosses.nonEmpty then
      Some("White " + (if common.nonEmpty then "also " else "") + "loses " + counted(cost.whiteAdditionalLosses))
    else if cost.blackAdditionalLosses.nonEmpty then
      Some("Black " + (if common.nonEmpty then "also " else "") + "loses " + counted(cost.blackAdditionalLosses))
    else None
    List("In the ") ++ sequence(value.references) ++
      List(" sequence, " + (common.toList ++ extra).mkString("; ") + ".")

  def statement(value: Statement): List[Part] = value match
    case Action(move, detail) =>
      val text = description(detail, natural = true)
      detail match
        case Attack(_, _, _) | Support(_, _, _, _) | KingEntry(_, _) | PawnStructure(_, false) | PassedPawn(_, None) =>
          List("After ", move, ", " + (if text.startsWith("The ") then "the " + text.drop(4) else text))
        case ChangedLines(_, _, _) => List(move, ": " + text)
        case _ => List(move, " " + text.take(1).toLowerCase(java.util.Locale.ROOT) + text.drop(1))
    case value: Material => material(value)
    case MaterialEvents(path, events) => events.zipWithIndex.flatMap { (event, index) =>
      val capture = event.capture.map(c =>
        s"${if c.recapture then "recaptures" else "captures"} the ${if c.side.white then "black" else "white"} " +
          s"${c.capturedRole.name} on ${c.square.key}")
      val promotion = Option.when(event.promotionGainCp > 0)("promotes to a " + event.movement.afterRole.name)
      (if index == 0 then Nil else List(" ")) ++ List(MoveRef(path, event.plyOffset), " " +
        (capture.toList ++ promotion).mkString(" and ") + ".")
    }
    case PieceExchange(path, events, recaptureOffsets) => events.zipWithIndex.flatMap { (event, index) =>
      def original(piece: Piece) = "the " + piece.sideName + " " + piece.roleName + " originally on " + piece.at
      val actor = event.originalActor.fold("the moving piece")(original)
      val victim = event.originalVictim.fold("the opposing piece")(original)
      (if index == 0 then Nil else List(" ")) ++ List("With ", MoveRef(path, event.plyOffset), ", " + actor +
        (if recaptureOffsets(event.plyOffset) then " recaptures " else " captures ") + victim + ".")
    }
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
        value.targetEnd.toList.flatMap(presence => List(" ") ++ statement(presence)) ++
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
      List("In the provided ", move, " line, " + provider.possessiveName + " maintains support of " + target.possessiveName +
        " after ", departure, " ends the control of that target's square by the " + oldProvider.sideName +
        " " + oldProvider.roleName + " originally on " + oldProvider.at + "; following ",
        MoveRef(move.path, use.targetCapture.plyOffset), ", ", MoveRef(move.path, use.recapture.plyOffset),
        " actually recaptures the piece that took that target.")
    case SupportedCapture(move, provider, target, use, fromStart, arrival, arrivalCapture) =>
      val start: List[Part] = if fromStart then
        if target == use.target then List("In the provided ", move, " line, that support is maintained")
        else List("In the provided ", move, " line, the supported " + target.roleName + " from " + target.at + " " +
          arrivalCapture.fold("reaches " + use.target.at)(detail => description(detail).stripSuffix(".").toLowerCase(java.util.Locale.ROOT)) +
          " with ") ++ arrival.toList ++ List(" while that support is maintained")
      else List("In the provided line, ", move, " gives " + provider.possessiveName + " control of the empty " + use.target.at +
        " square; ") ++ arrival.toList ++ List(" later brings " + use.target.possessiveName + " there under that support")
      start ++ List(". After ", MoveRef(move.path, use.targetCapture.plyOffset), " captures the supported " +
        use.target.roleName + " on " + use.target.at + ", ", MoveRef(move.path, use.recapture.plyOffset),
        " by the supporting " + provider.roleName + " on " + provider.at + " actually recaptures its capturer.")
    case SupportRetention(move, _, provider, _, target, _, oldProvider, departure, currentTarget) =>
      val identity = if currentTarget == target then "that same " + target.roleName + " on " + target.at
        else "the same " + target.roleName + " from " + target.at + ", now on " + currentTarget.at
      List("In the provided ", move, " line, ", departure, " ends the support of " + identity + " by the " +
        oldProvider.sideName + " " + oldProvider.roleName + " originally on " + oldProvider.at + "; the " +
        provider.roleName + " from " + provider.at + " keeps covering it through that move.")
    case BlockedLineTransfer(move, _, blocker, _, slider, departure, arrival, entrant) =>
      List(move, " keeps the line from " + slider.possessiveName + " toward " + entrant.at + " blocked: after ", departure,
        ", " + blocker.possessiveName + " remains in that line as its sole blocker. In the same provided line, ", arrival,
        " brings " + entrant.possessiveName + " there while that line is still blocked.")
    case BlockedAttack(after, attacker, target, blocker) =>
      List("After ", after, ", " + blocker.possessiveName + " blocks the line from " + attacker.possessiveName + " to " + target.possessiveName + ".")
    case ClearedRoute(move, before, mover, _, use, _, targets, passage, development) =>
      val developed = development.nonEmpty || Set("bishop", "knight")(mover.roleName) &&
        Set('1', '8')(before.at.last) && mover.at.last != before.at.last
      val opening = if development.nonEmpty then " develops the " + mover.roleName + " to " + mover.at
        else if developed then " brings the " + mover.roleName + " to " + mover.at
        else " clears " + before.at
      val action = if passage then "passes through " + (if developed then "the vacated " + before.at + " square" else "that square")
        else "uses the cleared line to target " + targets.map(_.possessiveName).mkString(" and ")
      List(move, opening + "; later ", use, " " + action + " in this line.")
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
    case CheckSequence(move, events) =>
      List("In the provided ", move, " line: ") ++ events.groupBy(_.move).toList.sortBy(_._1.offset)
        .zipWithIndex.flatMap { case ((ref, steps), index) =>
          (if index == 0 then Nil else List("; ")) ++ List(ref, " " + steps.map(checkDetail(_, natural = true)).mkString(" and "))
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
      List("After ", after, ", the " + original.sideName + " " + original.roleName + " originally on " +
        original.at + " is still present on " + current.at + ".")
    case AttackWithdrawal(move, before, after, target) =>
      List(move, " moves the " + before.roleName + " from " + before.at + " to " + after.at +
        ", ending its attack on " + target.possessiveName + ".")
    case contrast: ControlContrast =>
      List("After ", contrast.move, ", " + controlRelation(contrast.actor, contrast.target, contrast.square, contrast.present) +
        "; after ", contrast.alternative, ", " + controlRelation(contrast.alternativeActor, contrast.alternativeTarget,
          contrast.alternativeSquare, contrast.alternativePresent) + ".") ++
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

  def section(value: Section): List[Part] =
    val statements = value.statements.filterNot {
      case route: PieceRoute => value.statements.exists { use =>
        val actor = use match
          case support: SupportedCapture => support.use.recapture.originalActor
          case support: SupportTransfer => support.use.recapture.originalActor
          case _ => None
        actor.contains(route.original) && route.references.forall(use.references.contains)
      }
      case _ => false
    }
    val expressed = scala.collection.mutable.Set.empty[Statement]
    var precedingPath: Option[ObservedLineProtocol.PreparedPath] = None
    statements.flatMap { claim =>
      if expressed(claim) then Nil else
        expressed += claim
        val parts = claim match
          case Action(move, Attack(actor, _, gained)) =>
            val peers = statements.collect {
              case action @ Action(other, Attack(same, target, sameGain))
                  if other == move && same == actor && sameGain == gained => action -> target
            }
            expressed ++= peers.map(_._1)
            if peers.size == 1 then statement(claim) else
              List("After ", move, ", " + actor.possessiveName +
                (if gained then " now targets " else " attacks ") +
                peers.map(_._2.possessiveName).distinct.mkString(" and ") + ".")
          case presence: PiecePresence =>
            val peers = statements.collect {
              case same: PiecePresence if (same.after.path eq presence.after.path) &&
                  same.original == presence.original && same.current == presence.current => same
            }.sortBy(_.after.offset)
            expressed ++= peers
            if peers.size == 1 then statement(claim) else
              List("The " + presence.original.sideName + " " + presence.original.roleName + " originally on " +
                presence.original.at + " is still present on " + presence.current.at + " after ") ++
                peers.zipWithIndex.flatMap((same, index) =>
                  (if index == 0 then Nil else List(" and after ")) :+ same.after) ++ List(".")
          case _ => statement(claim)
        val refs = parts.collect { case ref: MoveRef => ref }
        val owner = refs.map(_.path).distinct match
          case List(path) => Some(path)
          case _ => None
        val header = owner.filter(path => !precedingPath.exists(_ eq path) &&
          !refs.exists(_.offset == 0)).toList.flatMap(path =>
            List("In the ", MoveRef(path, 0), " line: "))
        precedingPath = owner
        List(" ") ++ header ++ parts
    }.drop(1).foldLeft(List.empty[Part]) {
      case ((words: String) :: rest, next: String) => (words + next) :: rest
      case (all, next) => next :: all
    }.reverse
