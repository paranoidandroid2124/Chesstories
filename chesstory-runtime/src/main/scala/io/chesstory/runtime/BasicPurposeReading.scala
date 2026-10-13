package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation

/** Current purposes can suffice for an admitted close comparison. The grade
  * only permits this reading; it does not supply the purpose or prove safety.
  * A moved piece's present contacts can be named without selecting its whole
  * future history. Quiet pressure includes its replacement guard; lost defence
  * and another piece's new attack retain the fuller answer. Future episodes
  * stay available with their original prices. */
private[runtime] object BasicPurposeReading:
  import CommentaryAnswer.*
  import CommentaryEnglish.at
  import MoveWorkProjection.{Detail, LocalCandidate, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  def simplify(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position], notes: List[LocalCandidate], previous: Answer): Answer =
    paths.requireOwner(packet)
    if previous.question != "selected_comparison" || previous.core.flatMap(_.statements)
        .exists(_.isInstanceOf[Convergence]) || pair.size != 2 ||
        pair.exists(path => path.moves.size < 2 || path.automaticTerminal.nonEmpty) then return previous
    val primary = packet.primary.map {
      case PlayerFacingPrimary.MoveVerdict(ref) => ref
      case PlayerFacingPrimary.BestChoice(ref) => ref
    }
    val assessment = primary.flatMap(packet.evidenceGraph.candidateComparisonRecord).flatMap(_.payload match
      case CandidateComparisonEvidence(value) => Some(value)
      case _ => None).filter(value => Set(value.referenceLine, value.candidateLine) == pair.map(_.line.ref).toSet &&
        Set(MoveChoiceVerdict.MatchesReference, MoveChoiceVerdict.PlayableLoss)(value.comparison.verdict))
    if assessment.isEmpty || pair.exists(path =>
      !packet.candidateLines.find(_.ref == path.line.ref).exists(_.evaluation match
        case CandidateLineEvaluation.EngineSearch(line) => line.mate.isEmpty
        case _ => false)) then return previous

    val purposes = pair.map { path =>
      for
        before <- path.positions.headOption.flatMap(positions.get)
        after <- path.positions.lift(1).flatMap(positions.get)
        if before.complete(RelationFactKind.GeometricControl) && after.complete(RelationFactKind.GeometricControl)
        actor <- before.occupant(path.moves.head.take(2))
        moved <- paths.trackedPieceAfter(path, actor, 1).flatten
        if before.view.currentCheckSources(actor.side).isEmpty &&
          before.view.attackedTargets(actor.side).forall(_._1 != actor) &&
          after.view.attackedTargets(actor.side).forall(_._1 != moved)
        rootNotes = notes.filter(note => note.edge.evidence.id == path.ids.head &&
          note.edge.from == path.positions.head && note.edge.to == path.positions(1))
        familiar = rootNotes.find(_.note.detail match
            case _: Detail.Development | _: Detail.Castle | _: Detail.RookFile => true
            case _ => false)
        pressure = rootNotes.filter(_.note.detail match
          case Detail.Attack(provider, _, _) => provider == moved
          case Detail.Support(provider, target, opposition, _) => opposition.nonEmpty &&
            (provider == moved || before.controlledBy(actor)(target.at))
          case _ => false)
        note <- familiar.orElse(pressure.find(_.note.actor == moved))
        central = note.note.detail match
          case Detail.Development(_, _, _, squares) => squares.toSet
          case _ => Set.empty[String]
        // Name present contacts that the development statement does not
        // already express. This claims an attack, not a won piece or a reason
        // for the small evaluation difference. A discovered attack by a
        // different piece still needs the fuller selected explanation.
        contacts = after.controlledBy(moved).toList.sorted.flatMap(after.occupant).filter(target =>
          target.side != actor.side && !(target.role.name == "pawn" && central(target.at)))
        if after.view.attackedTargets(!actor.side).forall { (target, _) =>
          after.controllers(target.at, actor.side).forall(controller => controller == moved ||
            before.controllers(target.at, actor.side).contains(controller))
        }
        // A quiet pressure answer states the current replacement guard;
        // losing the last guard still requires the fuller explanation.
        if (before.view.attackedTargets(actor.side).map(_._1) ++ after.view.attackedTargets(actor.side).map(_._1))
          .distinct.forall { target =>
            before.contains(target) && after.contains(target) &&
              (before.controllers(target.at, actor.side) == after.controllers(target.at, actor.side) ||
                familiar.isEmpty && before.controllers(target.at, actor.side).nonEmpty &&
                  after.controllers(target.at, actor.side).nonEmpty && pressure.exists(_.note.detail match
                    case Detail.Support(_, supported, _, _) => supported == target
                    case _ => false))
          }
        if path.certified.flatMap(_._2.material).exists(_.events.forall(_.plyOffset != 0)) &&
          !path.certified.toList.flatMap(_._2.lineEvents).exists(event =>
            event.plyOffset == 0 && event.kind == LineEventKind.Check)
      yield
        val detail = note.note.detail match
          case Detail.RookFile(piece, fileType, _) => Detail.RookFile(piece, fileType, None)
          case value => value
        val statements = if familiar.isEmpty then pressure.map(note => Action(MoveRef(path, 0), note.note.detail))
          else Action(MoveRef(path, 0), detail) :: contacts.map(target =>
            ControlState(MoveRef(path, 0), moved, Some(target), target.at, present = true))
        Section(statements, focus =
          Option.when(detail == note.note.detail)(Focus(MoveRef(path, 0), note)).toList) ->
          (note.evidence ++ Option.when(familiar.isEmpty)(pressure.flatMap(_.evidence)).toList.flatten ++
            after.sourceRefs(after.sources(moved, contacts.map(_.at)))).distinct
    }
    purposes match
      case List(Some(left), Some(right)) =>
        val recorded = previous.core ++ previous.details
        val detail = Section(recorded.flatMap(_.statements).distinct,
          recorded.flatMap(_.required).distinct, recorded.flatMap(_.focus).distinctBy(_.move))
        val purposes = List(left._1, right._1).sortBy(section =>
          if packet.assembly.playedTransition.exists(edge =>
            section.statements.head.references.head.path.moves.head == edge.moveUci) then 0 else 1)
        // A normal purpose does not hide the opponent's actual immediate
        // check or capture. Close that selected reply and its trade through
        // the same mechanism as every other answer, preserving old detail.
        val replies = pair.filter(path => path.certified.exists { (_, fact) =>
          fact.material.exists(_.events.exists(_.plyOffset == 1)) ||
            fact.lineEvents.exists(event => event.plyOffset == 1 && event.kind == LineEventKind.Check)
        }).map(path => MoveRef(path, 1))
        val core = purposes ++ Option.when(replies.nonEmpty)(Section(List(Continuation(replies)))).toList
        RootQuestionSummary.completePairAnswer(packet, paths, pair, previous.question, core)
          .map(answer => answer.copy(details = List(detail), comparison = None,
            evidence = (answer.evidence ++ previous.evidence ++ left._2 ++ right._2 ++ primary.toList.map(_.id)).distinct))
          .flatMap(answer => MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, answer))
          .getOrElse(previous)
      case _ =>
        val current = purposes.flatten.find { (section, _) => packet.assembly.playedTransition.exists(edge =>
          section.references.head.path.moves.head == edge.moveUci) }
        val basic = previous.core.flatMap(_.statements).exists {
          case Action(move, _: Detail.Development | _: Detail.Castle | _: Detail.RookFile) =>
            move.offset == 0 && packet.assembly.playedTransition.exists(_.moveUci == move.path.moves.head)
          case route: ClearedRoute => route.move.offset == 0 && route.development.nonEmpty &&
            packet.assembly.playedTransition.exists(_.moveUci == route.move.path.moves.head)
          case route: RouteUse => route.move.offset == 0 && route.opening.exists {
            case _: Detail.Development | _: Detail.Castle | _: Detail.RookFile => true
            case _ => false
          } && packet.assembly.playedTransition.exists(_.moveUci == route.move.path.moves.head)
          case _ => false
        }
        if current.isEmpty then previous else
          val (purpose, proof) = current.get
          val root = purpose.references.head
          val existing = previous.core.flatMap(_.statements).find {
            case Action(move, _: Detail.Development | _: Detail.Castle | _: Detail.RookFile) => move == root
            case _ => false
          }
          // An integrated route already names its purpose. For a standalone
          // current action, keep that exact statement and its dependencies,
          // but read it before this arm's later continuation.
          val owner = previous.core.indexWhere(section => existing.fold(
            section.references.contains(root))(section.statements.contains))
          if owner < 0 || basic && existing.isEmpty then previous else
            val section = previous.core(owner)
            val action = existing.getOrElse(purpose.statements.head)
            previous.copy(core = previous.core.updated(owner, section.copy(
              statements = action :: section.statements.filterNot(_ == action),
              focus = (section.focus ++ purpose.focus).distinctBy(_.move))),
              evidence = (previous.evidence ++ proof ++ primary.toList.map(_.id)).distinct)
