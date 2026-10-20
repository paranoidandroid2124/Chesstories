package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import CommentaryAnswer.*
import CommentaryEnglish.{Part, Rendered, at, possessiveName}
import ObservedLineProtocol.PreparedPath
import RootQuestionSummary.MoveRef

/** Summarize an observed local transaction; never declare a PV tactically quiet. */
private[runtime] object CommentarySequenceText:
  private val roles = Map("pawn" -> "폰", "knight" -> "나이트", "bishop" -> "비숍",
    "rook" -> "룩", "queen" -> "퀸", "king" -> "킹")

  /** A shared root heading does not merge the replies or their receipts. */
  def sameRoot(left: PreparedPath, right: PreparedPath): Boolean =
    left.moves.headOption.nonEmpty && left.moves.headOption == right.moves.headOption &&
      left.ids.headOption == right.ids.headOption && left.positions.take(2) == right.positions.take(2)

  def rootOwner(refs: List[MoveRef]): Option[PreparedPath] = refs.headOption.map(_.path)
    .filter(first => refs.forall(ref => sameRoot(first, ref.path)))

  def relationActor(detail: MoveWorkProjection.Detail): Option[MoveWorkProjection.Piece] = detail match
    case MoveWorkProjection.Detail.Attack(actor, _, _) => Some(actor)
    case MoveWorkProjection.Detail.Support(actor, _, _, _) => Some(actor)
    case _ => None

  /** One move evading several attackers owns one anchor in both languages. */
  def evasions(statements: List[Statement], action: Action, ko: Boolean): Option[Rendered] =
    import MoveWorkProjection.Detail.*
    action.detail match
      case Evades(target, _) =>
        val peers = statements.collect {
          case same @ Action(move, Evades(piece, _)) if move == action.move && piece == target => same
        }
        val attackers = peers.collect { case Action(_, Evades(_, actor)) => actor }.distinct
        val words = if ko then
          val names = attackers.map(p => s"${if p.side.white then "백" else "흑"} ${p.at} ${roles(p.role.name)}")
          s" 수로 ${roles(target.role.name)}${if target.role.name == "knight" then "가" else "이"} ${names.mkString("·")}의 공격을 피합니다."
        else s" moves the ${target.role.name} out of the ${if attackers.size == 1 then "attack" else "attacks"} from ${attackers.map(_.possessiveName).mkString(" and ")}."
        Some(Rendered(List(action.move, words), peers))
      case _ => None

  /** Both languages group the same actor at the same move occurrence. Keeping
    * these receipts together prevents translation from repeating an anchor
    * and silently falling back to the longer English paragraph. */
  def relations(statements: List[Statement], action: Action, ko: Boolean): Option[Rendered] =
    import MoveWorkProjection.Detail.*
    relationActor(action.detail).map { actor =>
      val peers = statements.collect { case same: Action if same.move == action.move &&
        relationActor(same.detail).contains(actor) => same }
      def name(p: MoveWorkProjection.Piece) = if ko then
        s"${if p.side.white then "백" else "흑"} ${p.at} ${roles(p.role.name)}" else p.possessiveName
      def obj(p: MoveWorkProjection.Piece) = if p.role.name == "knight" then "를" else "을"
      val gains = scala.collection.mutable.Set.empty[Boolean]
      val clauses = peers.map(_.detail).distinct.flatMap {
        case Attack(_, _, gained) if gains.add(gained) =>
          val targets = peers.collect { case Action(_, Attack(_, target, `gained`)) => target }.distinct
          Some(if ko then targets.map(name).mkString("·") + obj(targets.last) +
            (if gained then " 새로 공격합니다" else " 계속 공격합니다")
          else (if gained then "now targets " else "keeps targeting ") + targets.map(name).mkString(" and "))
        case Support(_, target, opposition, gained) => Some(if ko then
          (if opposition.isEmpty then "" else opposition.map(name).mkString("·") + "에게 공격받는 ") +
            name(target) + obj(target) + (if gained then " 새로 지킵니다" else " 계속 지킵니다")
          else (if gained then "adds support to " else "keeps supporting ") + name(target) +
            (if opposition.isEmpty then "" else ", which is under attack from " + opposition.map(name).mkString(" and ")))
        case _ => None
      }
      val parts = if peers.size == 1 then
        if ko then ReferencePurposeText.condition(action).get else CommentaryEnglish.statement(action)
      else if ko then List[Part](action.move, s" 뒤 ${name(actor)}${if actor.role.name == "knight" then "가" else "이"} " +
        clauses.mkString("; ") + ".")
      else List[Part]("After ", action.move, ", " + name(actor) + " " + clauses.mkString(", and ") + ".")
      Rendered(parts, peers)
    }

  /** The exact physical losses were already matched by the caller. Equal
    * role counts can be spoken as trades without losing their proof identities. */
  def balancedExchange(white: List[EvidencePieceRole], black: List[EvidencePieceRole], ko: Boolean): Option[String] =
    val left = white.groupMapReduce(_.name)(_ => 1)(_ + _)
    val right = black.groupMapReduce(_.name)(_ => 1)(_ + _)
    Option.when(left.nonEmpty && left == right) {
      val counts = Map(1 -> "한", 2 -> "두", 3 -> "세", 4 -> "네")
      val items = white.map(_.name).distinct.map { role =>
        val count = left(role)
        if ko then s"${roles(role)} ${counts.getOrElse(count, count.toString)} 쌍"
        else s"$count pair${if count == 1 then "" else "s"} of ${role}s"
      }
      if ko then items.mkString("·") + "을 교환합니다."
      else items.mkString(" and ") + (if left.values.sum == 1 then " is exchanged." else " are exchanged.")
    }

  def pursuit(statements: List[Statement], action: Action, ko: Boolean): Option[Rendered] =
    convergingPursuit(statements, action, ko).orElse(singlePursuit(statements, action, ko).map { own =>
      val peers = statements.collect { case peer: Action if peer.detail == action.detail &&
        peer.move.path.moves(peer.move.offset) == action.move.path.moves(action.move.offset) => peer }
        .flatMap(peer => singlePursuit(statements, peer, ko).map(peer -> _))
      val shared = peers match
        case List((left, first), (right, second)) if (left.move.path ne right.move.path) &&
            first.expressed.last.isInstanceOf[Action] && second.expressed.last.isInstanceOf[Action] =>
          val escape = first.expressed.last.asInstanceOf[Action].move
          val other = second.expressed.last.asInstanceOf[Action].move
          val MoveWorkProjection.Detail.Attack(attacker, target, _) = action.detail: @unchecked
          Option.when(escape.path.moves(escape.offset) == other.path.moves(other.offset) &&
            statements.collect { case ReferencePurpose(value) => value }.exists(_.sameOriginalOccupants(
              left.move, right.move, List(attacker.at, target.at, left.move.path.moves(left.move.offset).slice(2, 4))))) {
            def name(piece: MoveWorkProjection.Piece) = if ko then
              s"${if piece.side.white then "백" else "흑"} ${piece.at} ${roles(piece.role.name)}" else piece.possessiveName
            val roots: List[Part] = List(MoveRef(left.move.path, 0), if ko then "·" else " and ", MoveRef(right.move.path, 0))
            val parts = roots ++ List[Part](if ko then " 수순 모두 " else " both include ", left.move,
              if ko then s" 뒤 ${name(attacker)}${if attacker.role.name == "knight" then "가" else "이"} ${name(target)}${if target.role.name == "knight" then "를" else "을"} 공격하며, "
              else s", leaving ${name(attacker)} attacking ${name(target)}; ", escape,
              if ko then "로 그 기물이 피합니다." else " moves that piece away.")
            Rendered(parts, first.expressed ++ second.expressed)
          }
        case _ => None
      shared.getOrElse(own)
    })

  /** Several observed attacks on the same physical piece can have one escape.
    * A quiet intervening attack is named, never silently treated as an exchange. */
  private def convergingPursuit(statements: List[Statement], action: Action, ko: Boolean): Option[Rendered] =
    import MoveWorkProjection.Detail.*
    action.detail match
      case Attack(attacker, target, _) =>
        val path = action.move.path
        val escapes = statements.collect { case a @ Action(move, Evades(moved, from)) if
          (move.path eq path) && move.offset > action.move.offset && from == attacker &&
            moved.side == target.side && moved.role == target.role &&
            path.moves(move.offset) == target.at + moved.at => a }
        escapes.iterator.flatMap { escape =>
          val peers = statements.collect { case a @ Action(move, Evades(moved, _)) if
            move == escape.move && moved.at == path.moves(move.offset).slice(2, 4) &&
              moved.side == target.side && moved.role == target.role => a }
          val pairs = peers.flatMap { reply =>
            val Evades(_, source) = reply.detail: @unchecked
            statements.collect { case attack @ Action(move, Attack(actor, victim, _)) if
              (move.path eq path) && move.offset < reply.move.offset && actor == source && victim == target => attack }
              .sortBy(_.move.offset).lastOption.map(_ -> reply)
          }.distinct.sortBy(_._1.move.offset)
          val paid = statements.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet
          val ledger = path.certified.toList.flatMap(_._2.material).flatMap(_.events)
          val safe = pairs.size >= 2 && pairs.map(_._1.move.offset).distinct.size == pairs.size &&
            pairs.exists(_._1 == action) && pairs.forall { (attack, reply) =>
            val Attack(source, _, _) = attack.detail: @unchecked
            val between = (attack.move.offset + 1 until reply.move.offset).toList
            between.forall { n =>
              val event = ledger.find(_.plyOffset == n)
              val stated = event match
                case Some(e) if e.capture.nonEmpty => paid(n) && e.promotionGainCp == 0
                case Some(_) => false
                case None => pairs.exists(_._1.move.offset == n)
              stated && !Set(source.at, target.at).exists(square =>
                path.moves(n).take(2) == square || path.moves(n).slice(2, 4) == square)
            } && !path.certified.toList.flatMap(_._2.lineEvents).exists(e => between.contains(e.plyOffset) &&
              (e.kind == LineEventKind.Check || e.kind == LineEventKind.Mate))
          }
          Option.when(safe) {
            def name(p: MoveWorkProjection.Piece) = if ko then
              s"${if p.side.white then "백" else "흑"} ${p.at} ${roles(p.role.name)}" else p.possessiveName
            val attacks = pairs.zipWithIndex.flatMap { case ((attack, _), index) =>
              val Attack(source, _, _) = attack.detail: @unchecked
              if index == 0 then
                if ko then List[Part](attack.move, s" 뒤 ${name(source)}${if source.role.name == "knight" then "가" else "이"} ${name(target)}${if target.role.name == "knight" then "를" else "을"} 공격합니다.")
                else List[Part]("After ", attack.move, s", ${name(source)} attacks ${name(target)}.")
              else List[Part](if ko then " 이어 " else " Then ", attack.move,
                if ko then s" 뒤 ${name(source)}의 공격도 더해집니다." else s" adds an attack from ${name(source)}.")
            }
            Rendered(attacks ++ List[Part](" ", escape.move,
              if ko then " 수로 그 기물이 이 공격들을 피합니다." else " moves that piece out of these attacks."),
              pairs.flatMap((attack, reply) => List(attack, reply)))
          }
        }.nextOption()
      case _ => None

  private def singlePursuit(statements: List[Statement], action: Action, ko: Boolean): Option[Rendered] = action.detail match
    case MoveWorkProjection.Detail.Attack(attacker, target, _) =>
      val path = action.move.path
      val paid = statements.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet
      def local(reply: MoveRef): Boolean =
        val between = (action.move.offset + 1 until reply.offset).toList
        val ledger = path.certified.toList.flatMap(_._2.material).flatMap(_.events)
        // Do not fold a quiet intermezzo or a different physical occupant
        // into this attack/escape. Every intervening capture keeps its price.
        between.forall(n => paid(n) && ledger.exists(e => e.plyOffset == n &&
          e.capture.nonEmpty && e.promotionGainCp == 0) &&
          !Set(attacker.at, target.at).exists(square =>
            path.moves(n).take(2) == square || path.moves(n).slice(2, 4) == square)) &&
          !path.certified.toList.flatMap(_._2.lineEvents).exists(e => between.contains(e.plyOffset) &&
            (e.kind == LineEventKind.Check || e.kind == LineEventKind.Mate))
      statements.drop(statements.indexOf(action) + 1).collectFirst {
        case reply @ Action(move, MoveWorkProjection.Detail.Evades(moved, from)) if
            (move.path eq path) && move.offset > action.move.offset && from == attacker && local(move) &&
            moved.side == target.side && moved.role == target.role &&
            move.path.moves.lift(move.offset).contains(target.at + moved.at) =>
          val victim = if ko then s"${if target.side.white then "백" else "흑"} ${target.at} ${roles(target.role.name)}"
            else target.possessiveName
          val source = if ko then s"${if attacker.side.white then "백" else "흑"} ${attacker.at} ${roles(attacker.role.name)}"
            else attacker.possessiveName
          val direct = path.moves(action.move.offset).slice(2, 4) == attacker.at
          Rendered(List[Part](action.move,
            if ko then (if direct then s"의 $victim 공격을 " else s" 뒤 $source${if attacker.role.name == "knight" then "가" else "이"} $victim${if target.role.name == "knight" then "를" else "을"} 공격하지만, ")
            else (if direct then s" attacks $victim; " else s" leaves $source attacking $victim; "), move,
            if ko then " 수로 피합니다." else " moves that piece out of the attack."), List(action, reply))
      }
    case _ => None

  def checkDetail(event: CheckEvent, ko: Boolean): String =
    if !ko then CommentaryEnglish.checkDetail(event, natural = true) else event match
      case CheckingMove(_, detail, opened) =>
        val checkers = detail.checkers.map(p => s"${p.square.key} ${roles(p.role.name)}").mkString("·")
        val opening = if opened.isEmpty then "" else
          opened.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·") + "의 길을 열어, "
        s"${opening}${checkers}으로 ${if detail.checkedSide.white then "백" else "흑"} 킹을 체크합니다"
      case CheckReply(_, detail) =>
        val move = detail.response.resource
        val modes = detail.response.modes
        val capture = move.capture.filter(_ => modes.contains(RelationCheckResponseMode.CaptureChecker))
          .map(c => s"${c.capturedSquare.key}의 체크 기물(${roles(c.capturedRole.name)})을 잡습니다")
        if capture.nonEmpty && modes.contains(RelationCheckResponseMode.KingMove) then
          "킹으로 " + capture.get
        else
          (Option.when(modes.contains(RelationCheckResponseMode.KingMove))(
            s"킹이 ${move.movement.to.key}로 피합니다").toList ++ capture.toList ++
            Option.when(modes.contains(RelationCheckResponseMode.Interpose))(
              s"${move.movement.to.key}에서 ${roles(move.movement.afterRole.name)}으로 체크를 막습니다").toList).mkString("; ")

  def checkSequence(value: CheckSequence, ko: Boolean): List[Part] = checks(value, ko).getOrElse(
    List[Part](if ko then "제시된 " else "In the provided ", value.move,
      if ko then " 수순에서: " else " line: ") ++
      value.events.groupBy(_.move).toList.sortBy(_._1.offset).zipWithIndex.flatMap {
        case ((ref, steps), index) => (if index == 0 then Nil else List("; ")) ++
          List(ref, " " + steps.map(checkDetail(_, ko)).mkString(if ko then "; " else " and "))
      } ++ List("."))

  private def consecutive(offsets: List[Int]) = offsets.sliding(2).forall {
    case List(a, b) => b == a + 1
    case _ => true
  }

  def linked(a: ObservedLineMaterialEvent, b: ObservedLineMaterialEvent): Boolean =
    b.plyOffset == a.plyOffset + 1 && b.movement.side != a.movement.side &&
      b.capture.exists(c => c.recapture && c.square == a.movement.to && c.capturedRole == a.movement.afterRole)

  def groups[A](values: List[A])(linked: (A, A) => Boolean): List[List[A]] =
    values.foldLeft(List.empty[List[A]]) { (groups, next) =>
      if groups.lastOption.exists(group => linked(group.last, next)) then
        groups.init :+ (groups.last :+ next)
      else groups :+ List(next)
    }

  def exchange(path: PreparedPath, events: List[ObservedLineMaterialEvent], ko: Boolean,
      physical: Option[List[LineContinuationInvestigation.Event]] = None): Option[List[Part]] =
    val offsets = events.map(_.plyOffset)
    val ledger = path.certified.flatMap(_._2.material)
    val checks = path.certified.toList.flatMap(_._2.lineEvents).filter(e =>
      e.kind == LineEventKind.Check || e.kind == LineEventKind.Mate)
    if events.size < 2 || !consecutive(offsets) ||
        !ledger.exists(m => events.forall(m.events.contains)) ||
        events.exists(e => e.capture.isEmpty || e.promotionGainCp != 0 ||
          e.movement.beforeRole != e.movement.afterRole || e.capture.exists(_.square != e.movement.to)) ||
        !events.sliding(2).forall { case List(a, b) => linked(a, b); case _ => true } ||
        checks.exists(e => offsets.contains(e.plyOffset)) ||
        ledger.exists(_.events.exists(e => !offsets.contains(e.plyOffset) &&
          (linked(e, events.head) || linked(events.last, e)))) then return None
    // A promoted piece or a same-role replacement must not inherit an original identity.
    if physical.exists(rows => rows.map(_.plyOffset) != offsets || rows.zip(events).exists { (p, e) =>
        !path.ids.lift(p.plyOffset).contains(p.occurrenceId) ||
          !p.originalActor.exists(a => a.side == e.movement.side && a.role == e.movement.beforeRole) ||
          !p.originalVictim.exists(v => v.side != e.movement.side && v.role == e.capture.get.capturedRole)
      } || !rows.sliding(2).forall {
        case List(a, b) => a.originalActor == b.originalVictim
        case _ => true
      }) then return None
    def lost(white: Boolean): String = events.zipWithIndex.collect {
      case (e, i) if e.capture.get.side.white != white =>
        val victim = e.capture.get
        val square = physical.flatMap(_.lift(i)).flatMap(_.originalVictim).fold(victim.square.key)(_.at)
        if ko then s"${if physical.nonEmpty then "원래 " else ""}$square ${roles(victim.capturedRole.name)}"
        else s"${if physical.nonEmpty then "original " else ""}$square ${victim.capturedRole.name}"
    }.mkString(if ko then "·" else " and ")
    val last = events.last
    val original = physical.flatMap(_.lastOption).flatMap(_.originalActor)
    val recapturer = if ko then
      s"${if last.movement.side.white then "백" else "흑"} ${original.fold("")(p => s"원래 ${p.at} ")}${roles(last.movement.afterRole.name)}"
    else s"${if last.movement.side.white then "White" else "Black"}'s ${original.fold("")(p => s"original ${p.at} ")}${last.movement.afterRole.name}"
    val balanced = balancedExchange(events.flatMap(_.capture).filterNot(_.side.white).map(_.capturedRole),
      events.flatMap(_.capture).filter(_.side.white).map(_.capturedRole), ko)
    val exchange = balanced.getOrElse(if ko then s"백의 ${lost(true)}과 흑의 ${lost(false)}을 맞바꿉니다."
      else s"White's ${lost(true)} and Black's ${lost(false)} are exchanged.")
    Some(List[Part](MoveRef(path, offsets.head), " … ", MoveRef(path, offsets.last),
      if ko then s"에서 $exchange 되잡은 $recapturer${if last.movement.afterRole.name == "knight" then "는" else "은"} ${last.movement.to.key}에 놓입니다."
      else s": $exchange $recapturer recaptures on ${last.movement.to.key}."))

  /** Quiet direct checks by the same piece, answered only by king moves.
    * Captures, interpositions, discovered checks and incomplete replies keep their explicit account. */
  def checks(value: CheckSequence, ko: Boolean): Option[List[Part]] =
    val events = value.events.sortBy(_.move.offset)
    val offsets = events.map(_.move.offset)
    val path = value.move.path
    val pairs = events.grouped(2).collect {
      case List(check: CheckingMove, reply: CheckReply) => check -> reply
    }.toList
    if pairs.size < 2 || pairs.size * 2 != events.size || !consecutive(offsets) ||
        !events.forall(_.move.path eq path) ||
        path.certified.flatMap(_._2.material).forall(_.events.exists(e => offsets.contains(e.plyOffset))) ||
        path.automaticTerminal.nonEmpty && offsets.last == path.moves.size - 1 ||
        pairs.exists { (check, reply) =>
          val mover = check.detail.mover
          check.openedCheckers.nonEmpty || check.detail.checkers.size != 1 ||
            !check.detail.checkers.exists(p => p.square == mover.to && p.role == mover.afterRole) ||
            check.detail.checkedSide != reply.detail.respondingSide ||
            reply.detail.response.modes != List(RelationCheckResponseMode.KingMove) ||
            reply.detail.response.resource.capture.nonEmpty ||
            reply.detail.mover.beforeRole.name != "king" || reply.detail.mover.afterRole.name != "king" ||
            path.certified.toList.flatMap(_._2.lineEvents).exists(e =>
              e.plyOffset == reply.move.offset && (e.kind == LineEventKind.Check || e.kind == LineEventKind.Mate))
        } || !pairs.sliding(2).forall {
          case List((a, ar), (b, br)) =>
            a.detail.mover.to == b.detail.mover.from && a.detail.mover.afterRole == b.detail.mover.beforeRole &&
              a.detail.mover.side == b.detail.mover.side && ar.detail.mover.to == br.detail.mover.from
          case _ => true
        } then return None
    val first = pairs.head._1
    val last = pairs.last._2
    val checked = if ko then (if first.detail.checkedSide.white then "백" else "흑")
      else (if first.detail.checkedSide.white then "White" else "Black")
    val role = first.detail.mover.afterRole.name
    val origin = first.detail.kingSquare.key
    val destination = last.detail.mover.to.key
    val endpoint = if origin == destination then
      if ko then s"킹이 $destination 칸으로 돌아옵니다" else s"$checked's king returns to $destination"
    else if ko then s"킹이 $origin 칸에서 $destination 칸으로 이동합니다"
    else s"$checked's king moves from $origin to $destination"
    Some(List[Part](first.move, " … ", last.move,
      if ko then s"에서는 같은 ${roles(role)}의 ${pairs.size}차례 체크에 $checked $endpoint. 이는 제시된 응수 수순입니다."
      else s" records ${pairs.size} checks by the same $role; $endpoint in the observed replies."))
