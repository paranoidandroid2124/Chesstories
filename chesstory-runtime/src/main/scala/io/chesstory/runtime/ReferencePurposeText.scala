package io.chesstory.runtime

import CommentaryEnglish.{Part, Rendered, at, possessiveName}
import CommentaryAnswer.{Section, ReferencePurpose, Material, PreparedReply}
import RootQuestionSummary.MoveRef
import MoveWorkProjection.Piece
import ReferencePurposeReading.{Reading, Arm}

/** Both languages render the same selected relations and exact move anchors. */
private[runtime] object ReferencePurposeText:
  private val roles = Map("pawn" -> "폰", "knight" -> "나이트", "bishop" -> "비숍",
    "rook" -> "룩", "queen" -> "퀸", "king" -> "킹")
  private def name(p: Piece, ko: Boolean): String =
    if ko then s"${if p.side.white then "백" else "흑"} ${p.at} ${roles(p.role.name)}" else p.possessiveName
  private def names(pieces: List[Piece], ko: Boolean) = pieces.map(name(_, ko)).mkString(if ko then ", " else " and ")
  private def obj(p: Piece) = if p.role.name == "knight" then "를" else "을"
  private def subject(p: Piece) = if p.role.name == "knight" then "가" else "이"
  private def topic(p: Piece) = if p.role.name == "knight" then "는" else "은"
  private def original(p: Piece, ko: Boolean) =
    if ko then s"원래 ${p.at}의 ${roles(p.role.name)}" else s"the original ${p.at} ${p.role.name}"

  def legalOption(value: CommentaryAnswer.LegalReplyOption, ko: Boolean): List[Part] =
    val proof = value.proof
    def words(en: String, korean: String) = if ko then korean else en
    def role(piece: CommentaryFact.Piece) = if ko then roles(piece.role) else piece.role
    def sequence(moves: List[MoveRef]): List[Part] = moves.zipWithIndex.flatMap((move, n) =>
      (if n == 0 then Nil else List[Part](" ")) :+ move)
    val root = value.referenceSequence.head
    val other = value.alternativeSequence.head
    val left = sequence(value.referenceSequence.tail)
    val right = sequence(value.alternativeSequence.tail)
    if proof.price.nonEmpty then
      val guardMoves = proof.condition.subject == proof.effect.subject
      val subject = if guardMoves then proof.effect.subject.get else proof.effect.target.get
      val target = proof.effect.target.get
      List[Part](root, words(s" moves the ${role(subject)} from ${subject.square}. In the legal example ",
        s" 수로 ${subject.square}의 ${role(subject)}을 옮깁니다. 조건을 확인하는 합법적 예로 "), root, " ") ++ left ++
        List[Part](words(s", the same ${role(proof.effect.subject.get)} cannot capture the ${role(target)}. After ",
          s" 뒤에는 같은 ${role(proof.effect.subject.get)}의 ${role(target)} 잡기가 불가능합니다. "), other, " ") ++ right ++
        List[Part](words(", that capture is available with ", " 뒤에는 "), value.priceMove.get,
          words(". ", "로 잡을 수 있습니다. ")) ++ value.remainingCapturers.toList.flatten.map(actor =>
          words(s"${name(actor, false)} can still capture the ${role(target)}. ",
            s"다른 ${name(actor, true)}의 ${role(target)} 잡기는 여전히 가능합니다. ")) ++
        List[Part](words("This legal difference does not establish the value of the conditional move.",
          "이는 조건부 선택의 차이이며, 그 수의 가치를 입증하지는 않습니다."))
    else if proof.effect.kind == "legal_capture" then
      val provider = proof.action.subject.get
      val checking = value.reference.certified.exists { (_, fact) => fact.lineEvents.exists(event =>
        event.plyOffset == proof.reference.size - 1 && event.kind == lila.chessjudgment.model.judgment.LineEventKind.Check) }
      val recapture = value.reference.certified.toList.flatMap(_._2.material.toList.flatMap(_.events))
        .exists(event => event.plyOffset == proof.reference.size - 1 && event.capture.nonEmpty)
      List[Part](root, words(s" adds ${role(provider)} control of ${proof.condition.square.get}. In the legal example ",
        s" 수로 ${role(provider)}의 ${proof.condition.square.get} 통제를 더합니다. 조건을 확인하는 합법적 예로 "), root, " ") ++ left ++
        List[Part](words(if checking then ", the check can be answered by " else if recapture then ", the same piece can recapture with " else ", that piece can be captured with ",
          if checking then "의 체크에는 " else " 뒤에는 "), value.option,
          words(". After ", if checking then "로 대응할 수 있습니다. " else if recapture then "로 되잡을 수 있습니다. " else "로 잡을 수 있습니다. "), other, " ") ++ right ++
        List[Part](words(s", that capture with the same ${role(provider)} is unavailable.",
          s" 뒤에는 같은 ${role(provider)}의 그 잡기가 불가능합니다."))
    else
      val replyCapture = value.reference.certified.toList.flatMap(_._2.material.toList.flatMap(_.events))
        .find(_.plyOffset == proof.reference.size - 1).flatMap(_.capture)
      val otherCapture = value.alternative.certified.toList.flatMap(_._2.material.toList.flatMap(_.events))
        .exists(e => e.plyOffset == proof.alternative.size - 1 && e.capture.nonEmpty)
      val movedVictim = replyCapture.filter(capture => proof.reference.size == 2 && !otherCapture &&
        proof.alternative.head.take(2) == capture.square.key).toList.flatMap { capture =>
        List[Part](words(" Here ", " 여기서 "), other, words(
          s" has already moved the ${capture.capturedRole.name} from ${capture.square.key}, so the checking reply is without a capture.",
          s" 수는 이미 ${capture.square.key}의 ${roles(capture.capturedRole.name)}을 옮겼으므로, 이 응수는 기물을 잡지 않고 체크만 합니다."))
      }
      val option = proof.action.target.filter(_ => proof.action.kind == "legal_capture").fold(
        words(" available. After ", " 같은 선택도 남습니다. ")) { target =>
        words(s" to capture the ${role(target)} on ${target.square}. After ",
          s"로 ${target.square} ${role(target)}을 잡는 반격이 가능합니다. ")
      }
      List[Part](root, words(" removes the checking effect of the following reply. In the legal example ",
        " 수는 다음 응수의 체크 효과를 없앱니다. 조건을 확인하는 합법적 예로 "), root, " ") ++ left ++
        List[Part](words(", the king is not in check, leaving the option ", " 뒤에는 체크가 아니므로 "), value.option,
          option, other, " ") ++ right ++
        List[Part](words(", the king is in check and the same move cannot be played immediately.",
          " 뒤에는 체크를 해결해야 하므로 그 수를 바로 둘 수 없습니다.")) ++ movedVictim ++
        List[Part](words(" This proves an available option, not compensation for its material cost.",
          " 남겨 둔 합법적 선택이며, 그 대가를 보상받는다는 뜻은 아닙니다."))

  def preparedRoute(value: CommentaryAnswer.ClearedRoute, ko: Boolean): List[Part] =
    List[Part](if ko then "제시된 수순의 " else "In the given line, ") ++
      value.preparations.zipWithIndex.flatMap((move, index) =>
        (if index == 0 then Nil else List[Part](" … ")) :+ move) ++
      List[Part](if ko then s"를 거쳐 ${original(value.originalUser, ko)}의 ${value.user.at} 진입로를 차례로 비웁니다. "
        else s" successively clear the route for ${original(value.originalUser, ko)} to reach ${value.user.at}. ")

  def activityUse(value: PurposeActivityReading.Reading, ko: Boolean,
      shared: Option[PurposeActivityReading.Reading]): List[Part] =
    val reply: List[Part] = if value.preparations.isEmpty then Nil else
      List(MoveRef(value.path, 1), if ko then " 응수에서는 " else " is answered in the following way: ")
    reply ++ activity(value, ko, includeCause = value.seed.control.nonEmpty || value.preparations.nonEmpty, shared)

  def activity(value: PurposeActivityReading.Reading, ko: Boolean, includeCause: Boolean,
      shared: Option[PurposeActivityReading.Reading] = None, includeReply: Boolean = true): List[Part] =
    val entry = value.path.moves(value.entry).slice(2, 4)
    val cause = if !includeCause then Nil else value.seed.control.toList.flatMap(controlCause(_, ko)) ++ value.seed.mechanism.toList.flatMap {
      case route: CommentaryAnswer.SquareReoccupation => List[Part](if ko then
        s"${route.square} 칸을 비워 ${original(value.original, ko)}의 진입로를 만듭니다. "
        else s"Vacates ${route.square} for ${original(value.original, ko)}. ")
      case route: CommentaryAnswer.ClearedRoute if route.preparations.nonEmpty => preparedRoute(route, ko)
      case route: CommentaryAnswer.ClearedRoute => List[Part](if ko then
        s"${route.before.at} 칸을 비워 ${original(value.original, ko)}의 활동 경로를 엽니다. "
        else s"Clears ${route.before.at} for ${original(value.original, ko)} to become active. ")
      case _: CommentaryAnswer.PieceRoute => Nil
      case _ => Nil
    } ++ Option.when(includeCause && value.seed.provider.nonEmpty)(if value.seed.followingSupport then
      if ko then s"${original(value.original, ko)}을 새로 지원하며 $entry 진출 뒤에도 지원을 이어갑니다. "
      else s"Newly supports ${original(value.original, ko)}, maintaining support as it advances to $entry. "
      else if ko then s"$entry 칸을 새로 지원하여 ${original(value.original, ko)}의 진출을 받칩니다. "
      else s"Newly guards $entry for ${original(value.original, ko)} to arrive. ").toList
    val route = value.route.filter(n => includeCause || n > 0).zipWithIndex.flatMap((offset, index) =>
      (if index == 0 then Nil else List[Part]("–")) :+ MoveRef(value.path, offset))
    val action: List[Part] = route ++ List[Part](if ko then
      s"로 ${roles(value.original.role.name)}${subject(value.original)} ${if value.original.role.name == "pawn" then "전진하여" else "활동에 나서"}" else
      s" brings the ${value.original.role.name} into play") ++
      List[Part]((if value.attacks.nonEmpty then
        if ko then s" ${names(value.attacks, ko)}${obj(value.attacks.last)} 공격합니다."
        else s", attacking ${names(value.attacks, ko)}."
      else if value.guards.nonEmpty then
        if ko then s" 공격받는 ${names(value.guards, ko)}${obj(value.guards.last)} 지킵니다."
        else s", defending ${names(value.guards, ko)} under attack."
      else value.capture.fold(if ko then " 교환에 참여합니다." else ", entering the exchange.")(p =>
        if ko then s" ${name(p, ko)}${obj(p)} 잡습니다." else s", capturing ${name(p, ko)}.")))
    val retreat = value.retreat.toList.flatMap { (offset, before, _) =>
      List[Part](if ko then " 이 수순에서는 " else " In this line, ", MoveRef(value.path, offset),
        if ko then s"로 공격받은 ${roles(before.role.name)}${subject(before)} 물러납니다."
        else s" moves the attacked ${before.role.name} away.")
    }
    val recovery = value.recovery.toList.flatMap { (offset, target) =>
      if value.recapturedGuard.nonEmpty then List[Part](if ko then " 이어 " else " After ",
        MoveRef(value.path, value.use + 1), if ko then "의 교환에는 " else ", ", MoveRef(value.path, offset),
        if ko then s"로 되잡아 ${roles(value.original.role.name)}${subject(value.original)} ${value.path.moves(offset).slice(2, 4)}에 남습니다."
          else s" recaptures, leaving the ${value.original.role.name} on ${value.path.moves(offset).slice(2, 4)}.")
      else value.recapturingKing match
        case Some((reply, king)) => List[Part](if ko then " 이어 " else " Then ", MoveRef(value.path, offset),
          if ko then "에 " else " is answered by ", MoveRef(value.path, reply),
          if ko then s"로 되잡아 ${if king.side.white then "백" else "흑"} 킹이 ${king.at}로 나옵니다."
            else s", bringing ${if king.side.white then "White" else "Black"}'s king to ${king.at}.")
        case None => List[Part](if ko then " 이어 " else " Then ", MoveRef(value.path, offset),
          if ko then s"로 ${name(target, ko)}${obj(target)} 잡습니다." else s" captures ${name(target, ko)}.")
    }
    val condition: List[Part] = if includeCause || !includeReply then Nil else
      List(MoveRef(value.path, 1), if ko then " 응수에서는 " else " is answered by ")
    val common: List[Part] = route ++ value.retreat.toList.flatMap((offset, _, _) =>
      List[Part]("–", MoveRef(value.path, offset))) ++ List[Part](if ko then
        s"로 같은 방식으로 ${roles(value.original.role.name)}${obj(value.original)} 활동시킵니다."
        else s" realizes the same activity of the ${value.original.role.name}.")
    val continued = value.continuation.toList.flatMap { (next, targets) =>
      List[Part](" ") ++ controlUse(next, ko, reply = false) ++ List[Part](if ko then
        s" ${name(next.user, ko)}${subject(next.user)} ${names(targets, ko)}${obj(targets.last)} 공격합니다."
        else s" ${name(next.user, ko)} attacks ${names(targets, ko)}.")
    }
    condition ++ cause ++ (if shared.exists(value.sameActivity) then common else action ++ retreat) ++ recovery ++ continued

  private def controlCause(value: ReferencePurposeReading.ControlUse, ko: Boolean): List[Part] =
    val relation = value.target.fold(if ko then s"${value.user.at} 칸 통제" else s"control of ${value.user.at}")(p =>
      if ko then s"${name(p, ko)}에 대한 지원" else s"guard of ${name(p, ko)}")
    if value.removed then
      (if value.change == 0 then Nil else List[Part](MoveRef(value.path, value.change), if ko then "에서 " else " ")) ++
        List[Part](if ko then s"${original(value.controller, ko)}${obj(value.controller)} 교환하여 $relation 기능을 없앱니다. "
          else s"exchanges ${name(value.controller, ko)}, removing its $relation. ")
    else List[Part](if ko then s"${name(value.controller, ko)}의 $relation 경로를 막습니다. "
      else s"cuts off the $relation supplied by ${name(value.controller, ko)}. ")

  private def controlUse(value: ReferencePurposeReading.ControlUse, ko: Boolean, reply: Boolean): List[Part] =
    val condition: List[Part] = if reply then List(MoveRef(value.path, 1),
      if ko then " 응수에서는 " else " is the observed reply; ") else List(if ko then "제시된 수순의 " else "In the shown continuation, ")
    val result = value.target.fold(
      if ko then s"에서 남은 ${original(value.originalUser, ko)}${subject(value.originalUser)} 그 칸을 실제로 사용합니다."
      else s" brings ${original(value.originalUser, ko)} to that square.")(p =>
      if ko then s"로 ${name(p, ko)}${obj(p)} 잡아 바뀐 방어를 이용합니다."
      else s" captures ${name(p, ko)}, using that change in its defense.")
    controlCause(value, ko) ++ condition ++ List[Part](MoveRef(value.path, value.use), result) ++
      Option.when(value.remaining.nonEmpty)(if ko then s" 그 칸에는 여전히 ${names(value.remaining, ko)}의 공격이 있습니다."
        else s" The square is still attacked by ${names(value.remaining, ko)}.").toList

  def purpose(value: Reading, ko: Boolean, priced: Boolean = false,
      mechanisms: List[CommentaryAnswer.Statement] = Nil): List[Part] =
    val ref = value.reference
    val played = value.played
    val sharedShields = ref.shields.filter(s => played.shields.contains(s) &&
      !ref.evasion.exists(_.king == s._2))
    def root(arm: Arm): List[Part] =
      def sentences(values: List[String]): List[List[Part]] = values.map(s => List(
        if ko then s else s.take(1).toUpperCase(java.util.Locale.ROOT) + s.drop(1)))
      val obligation = arm.evasion.toList.map { evasion => evasion.kind match
        case "king_move" => if ko then s"${names(evasion.checkers, ko)}의 체크를 피해 킹을 ${evasion.king.at}로 옮깁니다."
          else s"Moves the king to ${evasion.king.at}, out of check from ${names(evasion.checkers, ko)}."
        case "capture_checker" => if ko then s"${names(evasion.checkers, ko)}${obj(evasion.checkers.last)} 잡아 체크를 해소합니다."
          else s"Captures the checking ${names(evasion.checkers, ko)}."
        case _ => if ko then s"${names(evasion.checkers, ko)}에서 킹으로 이어지는 공격선을 막아 체크를 해소합니다."
          else s"Blocks the line from ${names(evasion.checkers, ko)} to the king, meeting the check."
      }
      val capture = arm.captured.filter(_ => !priced && arm.evasion.isEmpty).map(p => if ko then s"${name(p, ko)}${obj(p)} 잡아 교환을 시작합니다."
        else s"captures ${name(p, ko)}, starting the exchange.").toList
      val castling = arm.castlePreparation.toList.map(preparation => if ko then
        s"${roles(arm.actor.role.name)}을 옮겨 ${arm.actor.at}을 비우고 ${if preparation.kingSide then "킹사이드" else "퀸사이드"} 캐슬링 경로를 엽니다."
        else s"Moves the ${arm.actor.role.name} off ${arm.actor.at}, opening the ${if preparation.kingSide then "kingside" else "queenside"} castling route.")
      val deferred = value.preparedDevelopment(arm).toList.map { (preparation, option) =>
        val target = option.proof.action.target.get
        val destination = option.proof.reference.last.slice(2, 4)
        List[Part](if ko then s"${destination}로 들어오는 상대 ${roles(target.role)}을 잡을 선택을 준비하고, 실제 "
          else s"Prepares a capture against the opposing ${target.role}'s entry on $destination; after the actual ",
          MoveRef(arm.path, 1), if ko then " 응수에는 " else " reply, ", MoveRef(arm.path, preparation.offset),
          if ko then "로 같은 캐슬링 경로를 엽니다." else " opens the same castling route.")
      }
      val escape = Option.when(arm.escaped.nonEmpty && arm.evasion.isEmpty)(if ko then
        s"${original(arm.actor, ko)}${obj(arm.actor)} 옮겨 ${names(arm.escaped, ko)}의 공격을 피합니다."
        else s"moves the ${arm.actor.role.name} out of the attack from ${names(arm.escaped, ko)}.").toList
      val released = Option.when(arm.releasedGuards.nonEmpty)(if ko then
        s"${names(arm.releasedGuards, ko)}에 대한 ${roles(arm.actor.role.name)}의 방어를 놓습니다."
        else s"gives up the ${arm.actor.role.name}'s guard of ${names(arm.releasedGuards, ko)}.").toList
      val guard = arm.protections.filterNot(p => arm.shields.exists(_._2 == p.target)).map { p =>
        val timing: List[Part] = if !p.afterReply then Nil else
          List(MoveRef(arm.path, 1), if ko then " 응수의 공격에도 대비합니다. " else " is the observed attacking reply; ")
        val lead = if ko then
          s"${name(arm.moved, ko)}${subject(arm.moved)} ${names(p.attackers, ko)}에게 공격받는 ${name(p.target, ko)}${obj(p.target)} "
          else s"Uses ${name(arm.moved, ko)} to guard ${name(p.target, ko)} against ${names(p.attackers, ko)}"
        if value.continuedRootContact(arm, p.target, attack = false) then
          timing ++ List[Part](lead + (if ko then "" else "; this guard continues through "), MoveRef(arm.path, arm.through),
            if ko then "까지 계속 지킵니다." else ".")
        else timing ++ List[Part](lead + (if ko then "지킵니다." else "."))
      }
      val shield = arm.shields.filterNot(s => sharedShields.contains(s) ||
        arm.evasion.exists(e => e.king == s._2)).map((attacker, target) => if ko then
        s"${name(arm.moved, ko)}${subject(arm.moved)} ${name(attacker, ko)}에서 ${name(target, ko)} 쪽으로 이어지는 공격선을 막습니다."
        else s"interposes ${name(arm.moved, ko)} on the line from ${name(attacker, ko)} to ${name(target, ko)}.")
      // When the attacked pawn immediately takes the exchanging bishop, the
      // trade already expresses that contact; it is not another lasting task.
      val checking = arm.path.certified.toList.flatMap(_._2.lineEvents).exists(e =>
        e.plyOffset == 0 && e.kind == lila.chessjudgment.model.judgment.LineEventKind.Check)
      // The reply paragraph owns the actual check and its answer together.
      val check = Option.when(checking && !priced)(if ko then "상대 킹에 체크를 겁니다." else "gives check to the opposing king.").toList
      val targets = arm.attacks.filterNot(p => checking && p.role.name == "king" ||
        arm.captured.nonEmpty && arm.retainedMover.isEmpty ||
        priced && arm.captured.nonEmpty && arm.reply == p && arm.replyCapture.contains(arm.moved) ||
        arm.exchange.nonEmpty && p.role == arm.actor.role)
      val attack = List(false, true).flatMap { continued =>
        val group = targets.filter(p => value.continuedRootContact(arm, p, attack = true) == continued)
        Option.when(group.nonEmpty) {
          val lead = if ko then s"${names(group, ko)}${obj(group.last)} 새로 공격합니다."
            else s"Newly attacks ${names(group, ko)}."
          if continued then List[Part](lead + (if ko then " 이 공격은 " else " This attack continues through "),
            MoveRef(arm.path, arm.through), if ko then "까지 이어집니다." else ".")
          else List[Part](lead)
        }
      }
      val file = arm.rookFile.toList.map(kind =>
        if ko then s"룩을 ${if kind == "open" then "열린" else "반열린"} ${arm.moved.at.head} 파일에 배치합니다."
        else s"places the rook on the $kind ${arm.moved.at.head}-file.")
      val exchange = arm.exchange.toList.map(_ =>
        if ko then s"${roles(arm.actor.role.name)} 교환을 제안합니다."
        else s"offers a ${arm.actor.role.name} exchange.")
      val use = arm.supportedArrival.toList.map(arrival =>
        val recovery = arm.supportUse.filter(_.target == arrival.target)
        if ko then
          s"제시된 수순에서 ${arrival.target.at}에 진출하는 ${roles(arrival.target.role.name)}${obj(arrival.target)} 지원합니다." +
            recovery.fold("")(u => s" 잡힌 뒤 ${roles(u.provider.role.name)}${subject(u.provider)} 되잡습니다.")
        else s"in the shown line supports the ${arrival.target.role.name}'s arrival on ${arrival.target.at}" +
          recovery.fold(".")(u => s", then recaptures with the ${u.provider.role.name}."))
      val post = value.supportedPost.toList.filterNot(square => arm.supportedArrival.exists(_.target.at == square) &&
        arm.guards == List(arm.moved)).map(square => if arm.guards.nonEmpty then
        if ko then s"${names(arm.guards, ko)}이 $square 진출을 받칩니다."
        else s"${names(arm.guards, ko)} supports piece play on $square."
      else if ko then s"$square 칸은 폰의 지원을 받지 못합니다."
      else s"$square has no pawn support.")
      val unknown = Option.when(!value.hasPurpose(arm))(
        if ko then "이 수의 장기 목적은 제시된 수순에서 드러나지 않습니다."
        else "This move's longer-term purpose is not established by the shown continuation.").toList
      val causal = arm.controlUse.filter(_ => value.sharedControlUse.isEmpty)
      val parts = sentences(obligation ++ castling) ++ deferred ++ (if causal.nonEmpty then
        List(controlUse(causal.get, ko, reply = !(causal.get.path eq arm.path))) ++ sentences(released)
      else if arm.activity.nonEmpty then
        val value = arm.activity.get
        val supported = if value.original == arm.actor || arm.supportUse.nonEmpty then sentences(use) else Nil
        val condition: List[Part] = if value.path eq arm.path then Nil else
          List(MoveRef(value.path, 1), if ko then " 응수에서는 " else " is the observed reply; ")
        supported ++ List(condition ++ activity(value, ko, includeCause = supported.isEmpty,
          includeReply = supported.isEmpty)) ++ sentences(escape ++ check ++ released)
      else sentences(capture ++ escape) ++ guard ++ sentences(shield ++ check) ++ attack ++
        sentences(file ++ exchange ++ use ++ post ++ released ++ unknown))
      parts.zipWithIndex.flatMap((sentence, index) => (if index == 0 then Nil else List[Part](" ")) ++ sentence)
    val common: List[Part] = if sharedShields.isEmpty then Nil else
      List(MoveRef(ref.path, 0), if ko then "와 " else " and ", MoveRef(played.path, 0),
        (if ko then " 모두 " else " both ") + sharedShields.map((attacker, target) =>
          if ko then s"${name(attacker, ko)}에서 ${name(target, ko)} 쪽으로 이어지는 공격선을 막습니다."
          else s"block the line from ${name(attacker, ko)} to ${name(target, ko)}.").mkString(" ") + " ")
    val opening: List[Part] = common ++ value.arms.flatMap { arm =>
      value.preparation.filter(_.root eq arm.path).fold {
        val words = root(arm)
        val uses = mechanisms.filter(m => m.references.exists(_.path eq arm.path) &&
          !arm.activity.exists(_.expresses(m)) && !value.conditionalPurposes.exists(option => m match
            case CommentaryAnswer.Action(_, MoveWorkProjection.Detail.OpenedRay(_, user, end)) =>
              option.proof.owner.comparison.referenceMove == arm.path.moves.head &&
                option.proof.condition.subject.exists(_.witness == user) && option.proof.condition.square.contains(end)
            case _ => false)).flatMap(m =>
          (if ko then condition(m).get else CommentaryEnglish.statement(m)) :+ " ")
        (if words.isEmpty then Nil else List[Part](MoveRef(arm.path, 0), " — ") ++ words :+ " ") ++ uses
      }(prepared => MoveIntent.purpose(prepared, ko) :+ " ")
      }
    val timing = value.sharedCapture.toList.flatMap { shared =>
      val later = if shared.referenceOffset > shared.playedOffset then ref else played
      val offset = math.max(shared.referenceOffset, shared.playedOffset)
      List[Part](if ko then " 두 수순 모두 같은 " + original(shared.actor, ko) + "으로 " +
        name(shared.victim, ko) + obj(shared.victim) + " 잡습니다. 다만 " else " Both lines use " + original(shared.actor, ko) +
        " to capture " + name(shared.victim, ko) + "; in the ",
        MoveRef(later.path, 0), if ko then " 수순에서는 " else " line it happens later, with ",
        MoveRef(later.path, offset), if ko then "까지 교환을 미룹니다. 차이는 교환의 유무만이 아니라 그 전에 확보한 기물의 활동입니다."
        else ". The difference includes the activity gained before the trade, not just whether to exchange.")
    }
    val reply = value.sharedReplyPressure.toList.flatMap(p => List[Part](" ", MoveRef(ref.path, 0),
      if ko then "와 " else " and ", MoveRef(played.path, 0), if ko then " 모두 " else " both meet ",
      MoveRef(ref.path, 1), (if ko then " 응수에서 " else ": the reply ") + pressure(p, ko)))
    val sharedPawns = pawnStructure(value.reference.pawns.filter(p => value.sharedPawnStructure(p.original)), ko)
    val preparation = value.sharedPreparation match
      case List((referencePath, referenceOffset), (playedPath, playedOffset)) =>
        List[Part](if ko then " 두 수순 모두 같은 기물들로 이 구상을 실현합니다. " else
          " Both lines realize this idea with the same pieces: ",
          MoveRef(referencePath, 0), if ko then s" 쪽은 ${referenceOffset / 2}수 뒤 " else
            s" reaches it ${referenceOffset / 2} moves later with ",
          MoveRef(referencePath, referenceOffset), if ko then ", " else "; ",
          MoveRef(playedPath, 0), if ko then s" 쪽은 ${playedOffset / 2}수 뒤 " else
            s" reaches it ${playedOffset / 2} moves later with ",
          MoveRef(playedPath, playedOffset),
          if value.samePreparationPosition then
            if ko then "에서 같은 배치에 도달합니다. 이 두 수순은 수순 전환이므로 구상 자체가 한쪽만의 장점은 아닙니다."
            else ". These two continuations transpose into the same arrangement; the idea is not exclusive to either root."
          else if ko then " 뒤입니다. 그때의 교환 결과와 남는 기물은 아래에서 따로 비교합니다."
            else ". The exchanges and remaining pieces at those checkpoints are compared below.")
      case _ => Nil
    val convergence = Option.when(value.sameAftermath)(List[Part](" ", MoveRef(ref.path, 0),
      if ko then "와 " else " and ", MoveRef(played.path, 0),
      if ko then "의 제시된 수순은 각각 " else " reach the same arrangement and retain the same original pieces after ",
      MoveRef(ref.path, ref.through), if ko then ", " else " and ", MoveRef(played.path, played.through),
      if ko then " 뒤 같은 배치와 같은 원래 기물들로 이어집니다. 이 두 수순의 후속 활동은 한쪽만의 이점이 아닙니다."
      else ". The following activity in these two lines is not exclusive to either move.")).toList.flatten
    val commonAccess = value.sharedControlUse.toList.flatMap { (left, right) =>
      List[Part](if ko then
        if left.removed then s" 두 선택 모두 ${original(left.controller, ko)}${obj(left.controller)} 교환해 ${left.user.at}에 대한 그 기물의 통제를 없앱니다. "
        else s" 두 선택 모두 ${name(left.controller, ko)}의 ${left.user.at} 방향 통제 경로를 막습니다. "
      else if left.removed then s" Both choices exchange ${name(left.controller, ko)}, removing its control of ${left.user.at}. "
        else s" Both choices block ${name(left.controller, ko)}'s control of ${left.user.at}. ",
        MoveRef(ref.path, left.use), if ko then "와 " else " and ", MoveRef(played.path, right.use),
        if ko then s"에서 남은 ${original(left.originalUser, ko)}${subject(left.originalUser)} 그 칸을 실제로 사용합니다."
        else s" then bring ${original(left.originalUser, ko)} to that square.") ++
        Option.when(left.remaining.nonEmpty)(if ko then s" 그 칸에는 여전히 ${names(left.remaining, ko)}의 공격이 있습니다."
          else s" The square is still attacked by ${names(left.remaining, ko)}.").toList
    }
    opening ++ preparation ++ timing ++ reply ++ convergence ++ commonAccess ++ Option.when(sharedPawns.nonEmpty)(
      (if ko then " 두 비교 지점 모두 " else " At both checkpoints: ") +
        sharedPawns.mkString(" ")).toList

  private def comparison(section: Section): Option[Reading] = section.statements.iterator.flatMap {
    case ReferencePurpose(value) => Some(value)
    case PreparedReply(value) => value.owner.comparison
    case _ => None
  }.nextOption()

  private def shareable(losses: List[List[Piece]]): List[Piece] =
    val intersection = losses.headOption.toList.flatten.filter(p => losses.forall(_.contains(p)))
    if intersection.nonEmpty && (losses.forall(_.toSet == intersection.toSet) ||
        intersection.exists(_.side.white) && intersection.exists(!_.side.white)) then intersection else Nil

  /** Only delivery containers move. Each original statement still owns its full ledger. */
  def coalesce(sections: List[Section]): List[Section] =
    val reading = sections.flatMap(_.statements).collectFirst { case ReferencePurpose(value) => value }
      .filter(_.preparation.nonEmpty)
    if reading.isEmpty then return sections
    val prices = sections.zipWithIndex.flatMap { (section, index) => section.statements.collect {
      case price: Material if !section.focus.exists(f => price.references.contains(f.move)) => price -> index
    }}.distinctBy(_._1)
    val losses = prices.flatMap((price, _) => reading.get.physicalLosses(price).map(price -> _)).toMap
    var remaining = prices.map(_._1).filter(losses.contains)
    val destinations = scala.collection.mutable.Map.empty[Material, Int]
    var grouping = true
    while grouping do
      val candidates = remaining.combinations(2).flatMap { pair =>
        val common = shareable(pair.map(losses))
        Option.when(common.nonEmpty)(remaining.filter(p => common.forall(losses(p).contains)))
      }.toList.distinct.sortBy(group => (-group.size, -shareable(group.map(losses)).size))
      candidates.headOption match
        case Some(group) =>
          val first = prices.filter((price, _) => group.contains(price)).map(_._2).min
          group.foreach(price => destinations(price) = first)
          remaining = remaining.filterNot(group.contains)
        case None => grouping = false
    val moved = sections.zipWithIndex.map { (section, index) =>
      val kept = section.statements.filter {
        case price: Material => !destinations.contains(price) || destinations(price) == index
        case _ => true
      }
      val added = prices.collect { case (price, from) if from != index && destinations.get(price).contains(index) => price }
      val statements = kept ++ added
      val refs = statements.flatMap(_.references).toSet
      section.copy(statements = statements, required = (section.required.filter(refs) ++ added.flatMap(_.references)).distinct)
    }.filter(_.statements.nonEmpty)
    moved.foldLeft(List.empty[Section]) { (out, next) =>
      def branch(section: Section) = section.statements.collectFirst { case PreparedReply(value) => value }
      def merged(previous: Section) = previous.copy(statements = previous.statements ++ next.statements,
        required = (previous.required ++ next.required).distinct, focus = (previous.focus ++ next.focus).distinct)
      val pricedPosition = out.lastOption.filter(previous =>
        previous.statements.exists(_.isInstanceOf[ReferencePurpose]) && next.statements.exists {
          case CommentaryAnswer.ReferencePosition(_, arm) => previous.statements.exists {
            case price: Material => (price.path eq arm.path) && losses.contains(price)
            case _ => false
          }
          case _ => false
        }).map(merged)
      val joinedReplies = for
        previous <- out.lastOption
        a <- branch(previous)
        b <- branch(next)
        if (a.owner eq b.owner) && a.path.ids.take(2) == b.path.ids.take(2) &&
          a.path.positions.take(3) == b.path.positions.take(3) && a.path.moves.take(2) == b.path.moves.take(2)
        if (previous.statements ++ next.statements).forall {
          case _: PreparedReply | _: Material => true
          case _ => false
        }
      yield merged(previous)
      pricedPosition.orElse(joinedReplies).fold(out :+ next)(joined => out.dropRight(1) :+ joined)
    }

  def groupedCosts(section: Section, ko: Boolean): Option[Rendered] = for
    value <- comparison(section).filter(_.preparation.nonEmpty)
    prices = section.statements.collect { case price: Material => price }
    if prices.size >= 2
    accounts = prices.flatMap(price => value.physicalLosses(price).map(price -> _))
    if accounts.size == prices.size
    common = shareable(accounts.map(_._2))
    if common.nonEmpty
  yield
    def scope(price: Material): List[Part] =
      val offsets = if value.arms.exists(_.path eq price.path) && !(value.preparation.get.root eq price.path)
        then List(0) else List(0, 1, 2)
      offsets.zipWithIndex.flatMap((n, i) => (if i == 0 then Nil else List[Part](" ")) :+ MoveRef(price.path, n))
    val choices = prices.zipWithIndex.flatMap((price, i) =>
      (if i == 0 then Nil else List[Part](if ko then ", " else "; ")) ++ scope(price))
    val rest = accounts.flatMap { (price, lost) =>
      val extra = lost.filterNot(common.contains)
      if extra.isEmpty then Nil else List[Part](" ") ++ scope(price) ++
        List((if ko then " 쪽은 추가로 " else " additionally: ") + lossText(extra, ko))
    }
    Rendered(choices ++ List((if ko then " 수순은 아래 비교 시점까지 모두 " else
      " share this cost up to their checkpoints below: ") + lossText(common, ko)) ++ rest, prices)

  def groupedPurpose(section: Section, ko: Boolean): Option[Rendered] =
    val prepared = for
      purpose <- section.statements.collectFirst { case value: ReferencePurpose => value }
      costs <- groupedCosts(section, ko)
    yield
      val mechanisms = section.statements.filter(purpose.value.rootUses.contains)
      Rendered(this.purpose(purpose.value, ko, priced = true, mechanisms) ++ List(" ") ++ costs.parts,
        purpose :: (mechanisms ++ costs.expressed))
    prepared.orElse(rootPrices(section, ko))

  def lossText(pieces: List[Piece], ko: Boolean, distinguish: Set[String] = Set.empty, aggregate: Boolean = true): String =
    def side(white: Boolean) = pieces.filter(_.side.white == white)
    def items(xs: List[Piece]) = xs.map(_.role).distinct.map { role =>
      val group = xs.filter(_.role == role)
      if group.size == 1 then
        if ko then s"${group.head.at} ${roles(role.name)}" else s"original ${group.head.at} ${role.name}"
      else if ko then s"${roles(role.name)} ${Map(2 -> "둘", 3 -> "셋", 4 -> "넷").getOrElse(group.size, group.size.toString)}"
      else s"${group.size} ${role.name}s"
    }.mkString(if ko then "·" else " and ")
    val white = side(true)
    val black = side(false)
    def suffix(xs: List[Piece], vowel: String, consonant: String) =
      val last = xs.map(_.role).distinct.last
      if last.name == "knight" && xs.count(_.role == last) == 1 then vowel else consonant
    if white.nonEmpty && black.nonEmpty then
      val pairedRoles = white.map(_.role).distinct.filter(role => !distinguish(role.name) &&
        white.count(_.role == role) == black.count(_.role == role))
      val balanced = Option.when(aggregate)(CommentarySequenceText.balancedExchange(white.map(_.role), black.map(_.role), ko)).flatten
      if aggregate && balanced.isEmpty && pairedRoles.nonEmpty then
        val pairedPieces = pieces.filter(p => pairedRoles.contains(p.role))
        lossText(pairedPieces, ko) + " " + lossText(pieces.filterNot(pairedPieces.contains), ko, distinguish, aggregate = false)
      else if balanced.nonEmpty && distinguish.nonEmpty then pieces.map(_.role).distinct.map(role =>
        lossText(pieces.filter(_.role == role), ko, distinguish.filter(_ == role.name), aggregate = !distinguish(role.name))).mkString(" ")
      else balanced.getOrElse(
        if ko then s"백의 ${items(white)}${suffix(white, "와", "과")} 흑의 ${items(black)}${suffix(black, "를", "을")} 교환합니다."
        else s"White's ${items(white)} and Black's ${items(black)} are exchanged.")
    else
      val lost = if white.nonEmpty then white else black
      if ko then s"${if white.nonEmpty then "백" else "흑"}의 ${items(lost)}${suffix(lost, "를", "을")} 내줍니다."
      else s"${if white.nonEmpty then "White" else "Black"} gives up the ${items(lost)}."

  /** The original ledgers remain the two owners. Only exactly identified
    * physical losses can be shared; equal material scores are insufficient. */
  private def rootPrices(section: Section, ko: Boolean): Option[Rendered] = section.statements.collect {
    case purpose: ReferencePurpose => purpose
  } match
    case List(purpose) =>
      val value = purpose.value
      val prices = section.statements.collect { case m: Material if value.arms.exists(_.path eq m.path) => m }
      val paired = value.arms.flatMap(arm => prices.find(_.path eq arm.path).map(arm -> _))
      val complete = prices.nonEmpty && prices.size == paired.size && prices.map(_.path).distinct.size == prices.size &&
        paired.forall { (arm, price) =>
          arm.losses.size == price.cost.events.size && arm.losses.distinct.size == arm.losses.size &&
            arm.path.certified.flatMap(_._2.material).exists(m =>
              m.events.filter(_.plyOffset <= arm.through) == price.cost.events)
        }
      if !complete then None else
        val sharedLosses = if paired.size == 2 then paired.head._1.losses.filter(paired.last._1.losses.contains) else Nil
        // Sharing just one side of two different trades decomposes both trades
        // into separate losses again. Share an exchange, or a whole same loss.
        val common = if sharedLosses.nonEmpty && (paired.forall(_._1.losses.toSet == sharedLosses.toSet) ||
            sharedLosses.exists(_.side.white) && sharedLosses.exists(!_.side.white)) then sharedLosses else Nil
        val shared: List[Part] = if common.isEmpty then Nil else
          List(if ko then " 아래 비교 시점까지 두 수순 모두 " + lossText(common, ko)
            else " Up to the checkpoints below, in both lines " + lossText(common, ko))
        val different = paired.flatMap { (arm, _) =>
          val rest = arm.losses.filterNot(common.contains)
          val otherLosses = paired.filterNot(_._1 eq arm).flatMap(_._1.losses).filterNot(common.contains)
          val distinguish = rest.map(_.role.name).toSet.filter(role =>
            otherLosses.exists(_.role.name == role) &&
              rest.filter(_.role.name == role).toSet != otherLosses.filter(_.role.name == role).toSet)
          if rest.isEmpty then Nil else List[Part](" ", MoveRef(arm.path, 0),
            (if ko then (if common.nonEmpty then "에서는 그 밖에 " else " 수순의 비교 시점까지 ")
              else (if common.nonEmpty then " additionally: " else " through its checkpoint: ")) + lossText(rest, ko, distinguish))
        }
        val mechanisms = section.statements.filter(value.rootUses.contains)
        Some(Rendered(ReferencePurposeText.purpose(value, ko, priced = true, mechanisms) ++ shared ++ different,
          purpose :: (mechanisms ++ prices)))
    case _ => None

  /** A reply, its check obligations and its observed endpoint are one account.
    * Keep the original statements as receipts; do not credit a hidden event. */
  def groupedPosition(section: Section, ko: Boolean, announced: Set[Reading] = Set.empty): Option[Rendered] =
    section.statements.collect { case p: CommentaryAnswer.ReferencePosition => p } match
      case List(p) =>
        val checks = section.statements.collect { case c: CommentaryAnswer.CheckSequence if
          c.events.forall(_.move.path eq p.arm.path) => c }
        val ends = section.statements.collect { case e: CommentaryAnswer.RecordedEnd if
          (e.move.path eq p.arm.path) && e.move.offset == p.arm.through => e }
        val priced = section.statements.exists {
          case price: Material => (price.path eq p.arm.path) && p.value.physicalLosses(price).nonEmpty
          case _ => false
        }
        val purposeAnnounced = announced(p.value) || section.statements.takeWhile(_ != p).exists {
          case ReferencePurpose(value) => value eq p.value
          case _ => false
        }
        Some(Rendered(position(p.value, p.arm, ko, checks, ends.nonEmpty, priced, purposeAnnounced), p :: (checks ++ ends)))
      case _ => None

  private def checkedReplies(value: CommentaryAnswer.CheckSequence, ko: Boolean): List[Part] =
    import CommentaryAnswer.*
    val pairs = value.events.grouped(2).collect {
      case List(check: CheckingMove, reply: CheckReply) if
        check.move.offset + 1 == reply.move.offset && (check.move.path eq reply.move.path) &&
        check.openedCheckers.isEmpty && check.detail.checkers.size == 1 &&
        check.detail.checkers.head.square == check.detail.mover.to &&
        (!reply.detail.response.modes.contains(lila.chessjudgment.model.judgment.RelationCheckResponseMode.CaptureChecker) ||
          reply.detail.response.resource.capture.nonEmpty) => check -> reply
    }.toList
    if pairs.size * 2 != value.events.size || pairs.isEmpty then CommentarySequenceText.checkSequence(value, ko)
    else CommentarySequenceText.checks(value, ko).getOrElse(pairs.zipWithIndex.flatMap { case ((check, reply), i) =>
      val resource = reply.detail.response.resource
      val modes = reply.detail.response.modes
      val action = if modes.contains(lila.chessjudgment.model.judgment.RelationCheckResponseMode.CaptureChecker) then
        if ko then s"체크한 ${roles(resource.capture.get.capturedRole.name)}${if resource.capture.get.capturedRole.name == "knight" then "를" else "을"} 잡습니다"
        else s"captures the checking ${resource.capture.get.capturedRole.name}"
      else if modes.contains(lila.chessjudgment.model.judgment.RelationCheckResponseMode.Interpose) then
        if ko then s"${roles(resource.movement.afterRole.name)}${if resource.movement.afterRole.name == "knight" then "를" else "을"} 끼워 체크를 막습니다"
        else s"interposes the ${resource.movement.afterRole.name} against the check"
      else if ko then "킹이 체크를 피합니다" else "moves the king out of check"
      (if i == 0 then Nil else List[Part](if ko then "; 이어 " else "; then ")) ++
        List[Part](check.move, if ko then "에는 " else " is answered by ", reply.move,
          (if ko then "로 " else ", which ") + action)
    } ++ List("."))

  private def pressure(p: ReferencePurposeReading.ReplyPressure, ko: Boolean): String =
    val targets = p.targets.filterNot(target => p.alignments.exists(_._1 == target))
    (Option.when(targets.nonEmpty)(
      if ko then s"${names(targets, ko)}${obj(targets.last)} 새로 공격합니다."
      else s"newly attacks ${names(targets, ko)}.").toList ++ p.alignments.map { (front, rear) =>
      if ko then s"${name(front, ko)}${subject(front)} 뒤의 ${name(rear, ko)}에 핀됩니다."
      else s"pins ${name(front, ko)} to ${name(rear, ko)}."
    }).mkString(" ")

  private def pawnContact(work: ExchangeActivity.PawnWork, ko: Boolean): String =
    if ko then s"${name(work.actor, ko)}${subject(work.actor)} ${names(work.targets, ko)}${obj(work.targets.last)} 압박합니다."
    else s"${name(work.actor, ko)} pressures ${names(work.targets, ko)}."

  private def pawnGuards(work: ExchangeActivity.PawnWork, ko: Boolean): List[String] =
    Option.when(work.guards.nonEmpty)(if ko then
      s"${names(work.guards, ko)}${subject(work.guards.last)} 다음 전진 칸인 ${work.next} 칸을 통제합니다."
      else s"${names(work.guards, ko)} control${if work.guards.size == 1 then "s" else ""} ${work.next}, the next advance square.").toList

  private def pawnStructure(pawns: List[ReferencePurposeReading.PawnChange], ko: Boolean): List[String] =
    pawns.groupBy(p => (p.current.side, p.current.at.head)).toList
      .sortBy((key, _) => (if key._1.white then 0 else 1, key._2)).flatMap { (_, group) =>
        val doubled = (group.map(_.current) ++ group.flatMap(_.doubled)).distinct.sortBy(_.at)
        val isolated = group.forall(_.isolated)
        val changed = if doubled.size > 1 then List(if ko then
          s"${doubled.map(_.at).mkString("·")}에 ${if isolated then "고립된 " else ""}더블폰이 남습니다."
          else s"The pawns on ${doubled.map(_.at).mkString(" and ")} are doubled${if isolated then " and isolated" else ""}.")
        else group.map { p =>
          val properties = Option.when(p.passed.contains(true))(if ko then "통과폰" else "passed").toList ++
            Option.when(p.isolated)(if ko then "고립폰" else "isolated").toList
          if properties.nonEmpty then
            if ko then s"${name(p.current, ko)}${topic(p.current)} ${properties.mkString("·")}입니다."
            else s"${name(p.current, ko).capitalize} is ${properties.mkString(" and ")}."
          else if ko then s"원래 ${p.original.at} 폰이 ${p.current.at}에 남습니다."
          else s"The original ${p.original.at} pawn remains on ${p.current.at}."
        }
        changed ++ group.filter(p => doubled.size > 1 && p.passed.contains(true)).map(p =>
          if ko then s"${p.current.at} 폰은 통과폰입니다." else s"The ${p.current.at} pawn is passed.")
      }

  def position(value: Reading, arm: Arm, ko: Boolean,
      checks: List[CommentaryAnswer.CheckSequence] = Nil, recordedEnd: Boolean = false,
      priced: Boolean = false, purposeAnnounced: Boolean = false): List[Part] =
    val responseAction = if priced && arm.replyCapture.nonEmpty then "" else arm.replyCapture.map(p =>
      if p == arm.moved && arm.exchange.exists(_.offers.contains(arm.path.moves(1))) then
        if ko then "교환을 받아들입니다." else "accepts the offered exchange."
      else if arm.losses.nonEmpty && arm.captured.nonEmpty && p == arm.moved then
        if ko then "되잡기로 교환에 응합니다." else "recaptures in the exchange."
      else if ko then s"${name(p, ko)}${obj(p)} 잡습니다."
      else s"captures ${name(p, ko)}.").getOrElse(if arm.replyEvades then
        if ko then s"공격받던 ${original(arm.reply, ko)}${obj(arm.reply)} 피합니다."
        else s"moves the attacked ${arm.reply.role.name} away."
      else if arm.replyShields.nonEmpty then
        if ko then s"${arm.path.moves(1).slice(2, 4)}에 ${roles(arm.reply.role.name)}${obj(arm.reply)} 끼워 ${names(arm.replyShields, ko)} 쪽 공격선을 막습니다."
        else s"interposes the ${arm.reply.role.name} on ${arm.path.moves(1).slice(2, 4)}, blocking the attack on ${names(arm.replyShields, ko)}."
      else "")
    val replyPressure = arm.replyPressure.filterNot(value.sharedReplyPressure.contains).toList.map(pressure(_, ko))
    val deniedCastle = arm.castlePreparation.toList.flatMap(_.blocked.map { (_, attacker, squares) =>
      if ko then s"${name(attacker, ko)}이 ${squares.mkString("·")}을 통제하여 열린 캐슬링 경로를 막습니다."
      else s"${name(attacker, ko)} controls ${squares.mkString(" and ")}, closing the castling route."
    })
    val response = (List(responseAction).filter(_.nonEmpty) ++ deniedCastle ++ replyPressure).mkString(" ")
    val selected = value.coreFunctions(arm)
    val described = selected.map { f =>
      val p = f.current.get
      val supports = f.supports.filterNot(value.alreadyExplainedSupport(arm, f, _, purposeAnnounced))
      val attacks = f.attacks.filterNot(value.alreadyExplainedAttack(arm, f, _, purposeAnnounced))
      val tasks = f.openFile.map(file => if ko then s"열린 $file 파일에서 활동합니다"
        else s"works on the open $file-file").toList ++
        Option.when(supports.nonEmpty)(if ko then s"${names(supports, ko)}${obj(supports.last)} 지킵니다"
          else s"guards ${names(supports, ko)}").toList ++
        Option.when((value.purposeUsers(arm)(f.original) || arm.supportedArrival.exists(_.original == f.original) ||
          arm.occupant.contains(p)) &&
          f.openFile.isEmpty && attacks.nonEmpty)(
          if ko then s"${names(attacks, ko)}${obj(attacks.last)} 공격합니다"
          else s"attacks ${names(attacks, ko)}").toList
      val place = if ko then s"${name(p, ko)}${topic(p)}" else name(p, ko)
      f -> Option.when(tasks.nonEmpty)(place + " " + tasks.mkString(if ko then "; " else " and ") + ".")
    }
    val retained = described.collect { case (f, None) if !arm.occupant.contains(f.current.get) &&
        (f.supports.isEmpty && !f.attacks.exists(value.alreadyExplainedAttack(arm, f, _, purposeAnnounced)) ||
          value.contrasts.exists(c => c.reference.original == f.original &&
          c.reference.current.isDefined != c.played.current.isDefined)) => f.current.get }
    val functions = described.flatMap(_._2) ++ Option.when(retained.nonEmpty)(
      if ko then s"${names(retained, ko)}${obj(retained.last)} 남깁니다."
      else names(retained, ko) + (if retained.size == 1 then " remains." else " remain.")).toList
    val pawns = arm.pawns.filter(p => p.current.at != p.original.at || p.doubled.nonEmpty ||
      p.isolated || p.passed.contains(true) || value.arms.exists(other =>
        (other ne arm) && other.losses.contains(p.original)))
    val structure = pawnStructure(pawns.filterNot(p => value.sharedPawnStructure(p.original)), ko) ++
      pawns.filter(_.guards.isEmpty).map(p => if ko then s"${p.current.at} 폰에는 현재 방어자가 없습니다."
        else s"The ${p.current.at} pawn currently has no defender.")
    // An unchanged empty square is already described in the purpose paragraph.
    // A new occupant or changed support is still a distinct consequence.
    val post = value.supportedPost.toList.filterNot(_ => arm.occupant.isEmpty &&
      arm.guards.toSet == arm.remainingGuards.toSet).map { square =>
      val subjectText = arm.occupant.fold(if ko then s"$square 칸은" else square)(p =>
        if ko then name(p, ko) + topic(p) else name(p, ko))
      val hostile = arm.occupant.exists(_.side != arm.actor.side)
      if arm.remainingGuards.nonEmpty then
        if ko then s"$subjectText ${if arm.guards.isEmpty then "이제 " else ""}${names(arm.remainingGuards, ko)}의 ${if hostile then "공격" else "지원"}을 받습니다."
        else s"$subjectText is ${if arm.guards.isEmpty then "now " else ""}${if hostile then "attacked" else "supported"} by ${names(arm.remainingGuards, ko)}."
      else if hostile then
        if ko then s"$subjectText ${if arm.actor.side.white then "백" else "흑"} 폰의 공격을 받지 않습니다."
        else s"$subjectText is not attacked by ${if arm.actor.side.white then "White" else "Black"}'s pawns."
      else if ko then s"$subjectText ${if arm.guards.nonEmpty then "이제 " else ""}폰의 지원이 없습니다."
      else s"$subjectText ${if arm.guards.nonEmpty then "no longer has" else "has no"} pawn support."
    }
    val pawnLoss = Option.when(arm.actor.role.name == "pawn" && arm.retainedMover.isEmpty &&
      !arm.losses.contains(arm.actor))(
      if ko then s"${original(arm.actor, ko)}은 교환되어 남지 않습니다."
      else s"${original(arm.actor, ko).capitalize} has been exchanged away.").toList
    val reserve = Option.when((arm eq value.reference) && value.played.actor.role.name == "pawn" &&
      value.played.retainedMover.isEmpty)(value.retainedChoice).flatten.toList.filterNot(p =>
        pawns.exists(_.current == p)).map(p =>
      if ko then s"${name(p, ko)}${obj(p)} 남깁니다." else s"This continuation retains ${name(p, ko)}.")
    val entry = arm.replyAt.filter(p => Set("knight", "bishop")(p.role.name) &&
      p.at == arm.moved.at && arm.replyGuards.nonEmpty).toList.map(p =>
      if ko then s"응수로 들어온 ${name(p, ko)}${topic(p)} ${names(arm.replyGuards, ko)}의 지원을 받습니다."
      else s"The replying ${name(p, ko)} is supported by ${names(arm.replyGuards, ko)}.")
    val checkedReply = checks.exists(c => c.events.exists {
      case r: CommentaryAnswer.CheckReply => r.move == MoveRef(arm.path, 1)
      case _ => false
    } && c.events.exists(_.move == MoveRef(arm.path, 0)))
    val checkingReply = checks.exists(_.events.exists {
      case c: CommentaryAnswer.CheckingMove => c.move == MoveRef(arm.path, 1)
      case _ => false
    })
    val introduction: List[Part] = if arm.counterplay.nonEmpty then
      val counter = arm.counterplay.get
      val defended = value.replyDefense(arm).toList.flatMap { other =>
        List[Part](if ko then "다른 선택인 " else "In the other choice, ", MoveRef(other.entry.path, 0),
          if ko then s" 수순에서는 ${name(other.controller, ko)}${subject(other.controller)} 남아 "
            else s", ${name(other.controller, ko)} remains and meets ", other.entry,
          if ko then "에 " else " with ", other.capture, if ko then "로 대응합니다. " else ". ")
      }
      defended ++ List[Part](MoveRef(arm.path, 0), " — ") ++ counter.control.toList.flatMap(controlCause(_, ko)) ++
        activity(counter.activity, ko, includeCause = false, includeReply = false) ++ List[Part](" ") ++
        checks.flatMap(c => checkedReplies(c, ko) :+ " ")
    else if checkedReply then checks.flatMap(c => checkedReplies(c, ko) :+ " ")
      else if checkingReply then List[Part](MoveRef(arm.path, 0), " — ") ++
        checks.flatMap(c => checkedReplies(c, ko) :+ " ") ++
        Option.when(response.nonEmpty)(if ko then "이 첫 응수는 " + response + " "
          else "That first reply " + response + " ").toList
      else List[Part](MoveRef(arm.path, 0), if ko then " — " else " — after ", MoveRef(arm.path, 1),
        if response.isEmpty then (if ko then " 뒤, " else ", ") else ": " + response + " ") ++
        checks.flatMap(c => checkedReplies(c, ko) :+ " ")
    val outcome = post ++ functions ++ structure ++ pawnLoss ++ reserve ++ entry
    val pawnWork = arm.pawnWork.toList.flatMap { work =>
      val rootContact = work.offset == 0 && work.targets.forall(arm.attacks.contains)
      Option.when(!rootContact)(pawnContact(work, ko)).toList ++ pawnGuards(work, ko)
    }
    introduction ++ List[Part](MoveRef(arm.path, arm.through),
      (if outcome.isEmpty then (if ko then "까지 기록되어 있습니다." else " is the observed checkpoint.")
        else (if ko then "까지의 수순에서는 " else ": in this continuation, ") + outcome.mkString(" "))) ++
      (if pawnWork.isEmpty then Nil else List[Part](" " + pawnWork.mkString(" "))) ++
      Option.when(recordedEnd)(if ko then " 이 뒤의 응수는 기록되어 있지 않습니다."
        else " No further reply is recorded.").toList ++
      Option.when((arm eq value.played) && value.reference.through != value.played.through)(if ko then
        " 두 비교 지점의 길이가 달라 진행 속도의 우열을 뜻하지는 않습니다."
        else " The checkpoints have different lengths, so this does not establish a tempo advantage.").toList

  def exchangeReply(value: CommentaryAnswer.ReferenceExchangeReply, ko: Boolean): List[Part] =
    val branch = value.branch
    val state = branch.activity.get
    val actor = value.value.question.actor
    val queens = actor.role.name == "queen" && state.mover.nonEmpty &&
      state.targets.exists((original, current) => original.role.name == "queen" && current.nonEmpty)
    val retained = if queens then
      if ko then "양쪽 퀸이 남습니다." else "both queens remain."
    else state.mover.fold(
      if ko then s"${original(actor, ko)}${topic(actor)} 교환되어 남지 않습니다."
      else s"${original(actor, ko)} has been exchanged away.")(p =>
        if ko then s"${original(actor, ko)}${topic(actor)} ${p.at}에 남습니다."
        else s"${original(actor, ko)} remains on ${p.at}.")
    val file = state.file.toList.map(work =>
      if ko then s"${name(work.actor, ko)}${subject(work.actor)} 열린 ${work.file} 파일에서 활동합니다."
      else s"${name(work.actor, ko)} works on the open ${work.file}-file.")
    val pawn = state.pawn.filter(_.continuing).toList.flatMap(work => pawnContact(work, ko) :: pawnGuards(work, ko))
    List[Part](MoveRef(branch.path, 0), if ko then "의 다른 관측 응수 " else " also has the observed reply ",
      MoveRef(branch.path, 1), if ko then "에서는 " else "; through ", MoveRef(branch.path, branch.through),
      (if ko then "까지 " else ", ") + (retained :: file ::: pawn).mkString(" "))

  def contrast(value: Reading, ko: Boolean): List[Part] =
    val left = value.reference
    val right = value.played
    def work(f: ReferencePurposeReading.Function): String = f.current match
      case None => if ko then "교환되어 남지 않습니다" else "has been exchanged away"
      case Some(piece) =>
        val tasks = f.openFile.map(file => if ko then s"열린 $file 파일에서 활동합니다"
          else s"works on the open $file-file").toList ++
          Option.when(f.supports.nonEmpty)(if ko then s"${names(f.supports, ko)}${obj(f.supports.last)} 지킵니다"
            else s"guards ${names(f.supports, ko)}").toList
        (if ko then s"${piece.at}에 남습니다" else s"remains on ${piece.at}") +
          (if tasks.isEmpty then "" else "; " + tasks.mkString(if ko then ", " else " and "))
    val boundary: List[Part] = List(if ko then "기능 차이는 " else "The functional comparison is between ",
      MoveRef(left.path, 0), if ko then " 수순의 " else " at ", MoveRef(left.path, left.through),
      if ko then " 뒤와 " else " and ", MoveRef(right.path, 0), if ko then " 수순의 " else " at ",
      MoveRef(right.path, right.through), if ko then " 뒤에서 확인할 수 있습니다. " else ". ") ++
      Option.when(left.through != right.through)(if ko then
        "두 기록의 끝 시점이 달라, 이것만으로 어느 쪽이 더 빠르다고 판단할 수는 없습니다. "
        else "These checkpoints have different lengths, so they do not establish a tempo advantage. ").toList
    boundary ++ value.contrasts.take(2).flatMap { c =>
      // The material paragraph already identifies the piece exchanged away;
      // the contrast now tells what its surviving counterpart still does.
      val surviving = if c.reference.current.isEmpty then Some(right -> c.played)
        else if c.played.current.isEmpty then Some(left -> c.reference) else None
      surviving match
        case Some((arm, function)) => List[Part](MoveRef(arm.path, 0),
          if ko then "에서는 " + original(function.original, ko) + topic(function.original) + " " + work(function) + ". "
          else " keeps " + original(function.original, ko) + ": it " + work(function) + ". ")
        case None => List[Part](original(c.reference.original, ko) + (if ko then topic(c.reference.original) + " " else " "),
          MoveRef(left.path, 0), if ko then "에서는 " + work(c.reference) + ". " else " " + work(c.reference) + "; in ",
          MoveRef(right.path, 0), if ko then "에서는 " + work(c.played) + ". " else " it " + work(c.played) + ". ")
    }

  def functionalPurpose(value: MovePurpose.Reading, ko: Boolean): List[Part] =
    val q = value.question
    if q.plan.isEmpty && q.support.isEmpty && q.activity.isEmpty then
      return List[Part](MoveRef(q.path, 0), if q.kind == "supported_advance" then
        if value.followsSupport then
          if ko then s" 수는 ${q.user.at}의 ${roles(q.user.role.name)}을 새로 지원하며 ${q.square}로 진출한 뒤에도 그 지원을 이어갑니다. 실현 여부는 응수에 따라 달라집니다."
          else s" newly supports the ${q.user.role.name} on ${q.user.at}, maintaining that support as it advances to ${q.square}. Its realization depends on the reply."
        else if ko then s" 수는 ${q.square} 통제를 더하여 ${roles(q.user.role.name)}의 진출을 받칩니다. 실현 여부는 응수에 따라 달라집니다."
        else s" adds control of ${q.square}, supporting the ${q.user.role.name}'s advance. Its realization depends on the reply."
      else if ko then s" 수는 ${roles(q.user.role.name)}의 ${q.square} 진입 경로를 엽니다. 실현 여부는 응수에 따라 달라집니다."
        else s" opens a route for the ${q.user.role.name} through ${q.square}. Its realization depends on the reply.")
    val plan = value.witnessedPlan.get
    val (own, enemy) = plan.targets.partition(_.side == q.actor.side)
    val tasks = Option.when(own.nonEmpty)(if ko then s"${names(own, ko)}${obj(own.last)} 지킵니다."
      else s"guards ${names(own, ko)}.").toList ++
      Option.when(enemy.nonEmpty)(if ko then s"${names(enemy, ko)}${obj(enemy.last)} 공격합니다."
        else s"attacks ${names(enemy, ko)}.").toList
    List[Part](MoveRef(q.path, 0), if ko then " 뒤 조사한 " else " has the observed reply ",
      MoveRef(plan.path, 1), if ko then " 응수에서는 같은 기물이 " else "; the same piece follows ") ++
      plan.route.drop(1).zipWithIndex.flatMap((offset, index) =>
        (if index == 0 then Nil else List[Part](" … ")) :+ MoveRef(plan.path, offset)) ++
      List[Part]((if ko then "로 재배치되어 " else " and ") + tasks.mkString(" "))

  def functionalReply(value: MovePurpose.Reading, branch: MovePurpose.Branch, ko: Boolean,
      announced: Set[Reading] = Set.empty): List[Part] =
    val q = value.question
    val progress: List[Part] = if branch.activityRoute.isEmpty then Nil else
      List[Part](MoveRef(branch.path, 1), if ko then " 응수에서는 " else " is followed by ") ++
        branch.activityRoute.zipWithIndex.flatMap((offset, index) =>
          (if index == 0 then Nil else List[Part]("–")) :+ MoveRef(branch.path, offset)) ++
        List[Part](if ko then s"로 ${roles(q.user.role.name)}${obj(q.user)} 재배치합니다. ${q.square}에서의 활동까지는 이 수순에 나타나지 않습니다."
          else s" to redeploy the ${q.user.role.name}. This line does not yet show its activity on ${q.square}.")
    if branch.purposeActivity.nonEmpty then
      val use = branch.purposeActivity.get
      val alreadyShown = announced.exists(_.arms.flatMap(_.activity).exists(previous =>
        previous.entry == use.entry && previous.use == use.use && previous.through == use.through &&
          previous.sameActivity(use) && previous.path.moves.take(use.through + 1) == use.path.moves.take(use.through + 1) &&
          previous.path.positions.take(use.through + 2) == use.path.positions.take(use.through + 2)))
      if alreadyShown then return List[Part](MoveRef(use.path, 1),
        if ko then " 응수에서는 앞서 설명한 " else " realizes the previously described activity through ",
        MoveRef(use.path, use.use), if ko then s"까지의 ${roles(use.original.role.name)} 활동을 실현합니다." else ".")
      val shared = value.branches.takeWhile(_ ne branch).flatMap(_.purposeActivity)
        .find(previous => previous.sameActivity(use) && previous.differentOutcome(use))
      val cause = use.preparations.nonEmpty ||
        q.replyActivities.nonEmpty && use.seed.cause.nonEmpty && !q.purposeActivity.exists(_.sameTask(use))
      val conditional = if cause && !use.seed.control.exists(_.change == 1) then
        List[Part](MoveRef(use.path, 1), if ko then " 응수에서는 " else " permits ") else Nil
      val retained = if use.original != q.user && progress.nonEmpty then progress :+ " " else Nil
      return retained ++ conditional ++ activity(use, ko, includeCause = cause, shared)
    if progress.nonEmpty then return progress
    def contact(actor: Piece, targets: List[Piece]): String =
      val (own, enemy) = targets.partition(_.side == actor.side)
      val duties = Option.when(own.nonEmpty)(if ko then s"${names(own, ko)}${obj(own.last)} 지킵니다."
        else s"guards ${names(own, ko)}.").toList ++
        Option.when(enemy.nonEmpty)(if ko then s"${names(enemy, ko)}${obj(enemy.last)} 공격합니다."
          else s"attacks ${names(enemy, ko)}.").toList
      s"${name(actor, ko)}${if ko then subject(actor) else ""} " + duties.mkString(" ")
    val realized = branch.support match
      case Some(state) =>
        val asset = state.asset.fold(List(if ko then "지원하던 폰은 이 구간에서 사라집니다."
          else "The supported pawn is gone in this segment."))(p => List(state.provider.fold(
            if state.guards.nonEmpty then
              if ko then s"${names(state.guards, ko)}${subject(state.guards.last)} ${name(p, ko)}의 방어를 맡습니다."
              else s"${names(state.guards, ko)} take over the guard of ${name(p, ko)}."
            else if ko then s"${name(p, ko)}에는 방어자가 없습니다."
              else s"${name(p, ko)} has no guard.")(p2 => contact(p2, List(p)))))
        asset ++ state.activity.toList.map(p => if ko then s"그동안 ${name(p, ko)}${subject(p)} 활동에 나섭니다."
          else s"Meanwhile ${name(p, ko)} becomes active.") ++
          Option.when(state.blockaderRemains)(if ko then "폰 앞의 봉쇄 기물은 아직 남아 있습니다."
            else "The blockading piece still remains in front of the pawn.").toList
      case None => branch.pressure match
        case Some(pressure) => List(contact(pressure.actor, pressure.targets)) ++
          Option.when(!pressure.sameTask)(if ko then "계획했던 나이트 교환 제안까지 실현된 것은 아닙니다."
            else "This does not realize the proposed knight exchange.").toList
        case None =>
          val used = branch.realizes
          List(if ko then
            if used then s"${name(branch.actor, ko)}${subject(branch.actor)} ${q.square} 진출을 실현합니다."
            else s"${q.square}를 쓰는 대신 ${name(branch.actor, ko)}의 수가 이어집니다. 이 구간에서 계획의 실현은 확인되지 않습니다."
          else if used then s"${name(branch.actor, ko)} realizes the entry on ${q.square}."
            else s"The line continues with ${name(branch.actor, ko)} instead of using ${q.square}; the plan is not realized in this segment.")
    val changed = branch.relocated.map { (p, current) => if ko then
      s"${original(p, ko)}${topic(p)} " + current.fold("교환되어 그 기능의 대상이 사라집니다.")(now =>
        s"${now.at}로 옮겨 같은 칸을 상대로 한 기능은 달라집니다.")
      else s"${original(p, ko)} " + current.fold("has been exchanged away.")(now =>
        s"has moved to ${now.at}, changing the original task.") }
    val contested = Option.when(branch.controllers.nonEmpty)(if ko then
      s"${names(branch.controllers, ko)}${subject(branch.controllers.last)} ${q.square}를 새로 통제합니다."
      else s"${names(branch.controllers, ko)} now control ${q.square}.").toList ++ branch.occupant.toList.map(p =>
        if ko then s"${name(p, ko)}${subject(p)} ${q.square}를 차지합니다."
        else s"${name(p, ko)} occupies ${q.square}.")
    val companions = value.companions(branch).map { (p, current) => if ko then
      s"다른 ${roles(p.role.name)}${topic(p)} " + current.fold("교환되어 함께 활동할 선택이 사라집니다.")(now =>
        s"${now.at}에 남아 함께 활동할 수 있습니다.")
      else s"The other ${p.role.name} from ${p.at} " + current.fold("has been exchanged away.")(now =>
        s"remains available on ${now.at}.") }
    List[Part](MoveRef(branch.path, 1), if branch.use > 2 then
      if ko then " 응수 뒤의 수순에서는 나중에 " else " is followed later in the line by "
      else if ko then " 응수에는 " else " is met by ", MoveRef(branch.path, branch.use),
      (if ko then " 뒤 " else "; ") + (realized ++ changed ++ contested ++ companions).mkString(" ")) ++
      Option.when(branch.through != branch.use)(List[Part](if ko then " 이어지는 교환·체크는 " else " The connected exchange/check continues through ",
        MoveRef(branch.path, branch.through), if ko then "까지 확인했습니다." else ".")).toList.flatten

  def condition(value: CommentaryAnswer.Statement): Option[List[Part]] = value match
    case value: CommentaryAnswer.LegalReplyOption => Some(legalOption(value, ko = true))
    case CommentaryAnswer.ActivityUse(value, shared) => Some(activityUse(value, ko = true, shared))
    case CommentaryAnswer.FunctionPurpose(value) if value.witnessedPlan.nonEmpty ||
        value.question.plan.isEmpty && value.question.support.isEmpty && value.question.activity.isEmpty =>
      Some(functionalPurpose(value, ko = true))
    case CommentaryAnswer.FunctionReply(value, branch) if value.question.activity.isEmpty =>
      Some(functionalReply(value, branch, ko = true))
    case CommentaryAnswer.PreparedIdea(value) => Some(MoveIntent.purpose(value, ko = true))
    case CommentaryAnswer.PreparedReply(value) => Some(MoveIntent.reply(value, ko = true))
    case CommentaryAnswer.FirstReplyChoice(root, preferred, alternative, quality) =>
      Some(List[Part](root, " 이후 별도로 조사한 응수에서는 ", preferred,
        if quality == PrincipalVariationComparison.QualityBand.Close then "와 " else "가 ", alternative,
        if quality == PrincipalVariationComparison.QualityBand.Close then "가 비슷한 수준입니다."
        else "보다 더 높게 평가됩니다."))
    case route: CommentaryAnswer.PieceRoute =>
      Some(List[Part](s"제시된 수순에서 ${original(route.original, true)}${subject(route.original)} ") ++
        route.references.zipWithIndex.flatMap((move, index) =>
          (if index == 0 then Nil else List[Part](" … ")) :+ move) ++ List[Part]("로 재배치됩니다."))
    case value @ CommentaryAnswer.ClearedRoute(move, before, _, _, use, user, targets, passage, _, preparations) =>
      val enemy = targets.filter(_.side != user.side)
      val own = targets.filter(_.side == user.side)
      val actions = Option.when(enemy.nonEmpty)(s"${names(enemy, true)}${obj(enemy.last)} 공격합니다").toList ++
        Option.when(own.nonEmpty)(s"${names(own, true)}${obj(own.last)} 지킵니다").toList
      Some((if preparations.nonEmpty then preparedRoute(value, ko = true)
        else List[Part](move, s" 수로 ${before.at} 칸을 비워 길을 엽니다. 제시된 수순에서 ")) ++ List(use,
        (if passage then s" 수로 ${if user.side.white then "백" else "흑"} ${roles(user.role.name)}${subject(user)} 그 길을 지나 ${user.at}에 진출합니다."
          else s" 뒤 ${name(user, true)}${subject(user)} 열린 길로 ${actions.mkString("; ")}.")))
    case CommentaryAnswer.SquareReoccupation(move, _, _, square, use, _, _) =>
      Some(List(move, s" 수로 $square 칸을 비우고, 이후 ", use, " 수로 그 칸을 차지합니다."))
    case CommentaryAnswer.Action(move, MoveWorkProjection.Detail.OpenedRay(from, actor, end)) =>
      Some(List(move, s" 수로 $from 칸을 비워 ${name(actor, true)}의 $end 방향 길을 엽니다."))
    case CommentaryAnswer.Action(move, MoveWorkProjection.Detail.ChangedLines(Nil, opened, Nil)) if opened.nonEmpty =>
      Some(List(move, " 뒤 " + opened.map((actor, target) =>
        s"${name(actor, true)}의 ${name(target, true)} 공격선도 열립니다.").mkString(" ")))
    case value @ CommentaryAnswer.Action(_, MoveWorkProjection.Detail.Attack(actor, target, gained)) => Some(List(value.move,
      s" 뒤 ${name(actor, true)}${subject(actor)} ${name(target, true)}${obj(target)} " +
        (if gained then "새로 공격합니다." else "계속 공격합니다.")))
    case value @ CommentaryAnswer.Action(_, MoveWorkProjection.Detail.Evades(target, attacker)) => Some(List(value.move,
      s" 수로 ${name(target, true)}${subject(target)} ${name(attacker, true)}의 공격을 피합니다."))
    case value @ CommentaryAnswer.Action(_, MoveWorkProjection.Detail.Support(actor, target, opposition, _)) =>
      Some(List(value.move, s" 뒤 ${name(actor, true)}${subject(actor)} " +
        (if opposition.isEmpty then "" else s"${names(opposition, true)}에게 공격받는 ") +
        s"${name(target, true)}${obj(target)} 지킵니다."))
    case CommentaryAnswer.ControlState(move, actor, target, square, present, remaining) =>
      val what = target.fold(s"$square 칸")(name(_, true))
      val verb = target.fold("통제")(p => if p.side == actor.side then "지원" else "공격")
      val rest = remaining.toList.map {
        case Nil => s" 다른 ${if actor.side.white then "백" else "흑"} 기물의 ${verb}도 없습니다."
        case pieces => s" ${names(pieces, true)}의 ${verb}은 남아 있습니다."
      }.mkString
      Some(List(move, s" 뒤 ${name(actor, true)}${subject(actor)} $what${target.fold("을")(obj)} " +
        (if present then s"${verb}합니다." else s"${verb}하지 않습니다.") + rest))
    case _ => None
