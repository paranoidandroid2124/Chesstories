package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** Read what the trade changes, using the current answered replies and root
  * comparison. A useful role may pass to another original piece; an old PV
  * or a merely legal route never supplies a new evaluation. */
private[runtime] object ExchangeFunctionReading:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, LocalCandidate, Piece, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  final case class ClosedLine(slider: Piece, target: Piece, blocker: Piece)
  final case class KeptRole(path: PreparedPath, exchange: Int, use: Int, activation: Int,
      exchanged: Piece, retained: Piece, arrived: Piece, target: Piece, otherTargets: List[Piece],
      restored: Option[(Piece, Piece)])
  final case class RookAccess(after: MoveRef, rook: Piece, removed: Piece, square: String, target: Piece)
  final case class PawnFile(decline: PreparedPath, file: Char, removed: Piece, retained: Piece,
      blocked: Piece, attackers: List[Piece], restrictedAdvance: Option[Piece])

  final class Checked private[ExchangeFunctionReading] (private val owner: PreparedPaths,
      val capture: PreparedPath, val closed: Option[ClosedLine], val kept: Option[KeptRole],
      val access: Option[RookAccess], val pawnFile: Option[PawnFile], val evidence: List[String]):
    def belongsTo(paths: PreparedPaths): Boolean = owner eq paths
    def references: List[MoveRef] = (List(0, 1, 2).map(MoveRef(capture, _)) ++
      kept.toList.flatMap(value => (0 to value.use).map(MoveRef(value.path, _))) ++
      access.toList.map(_.after) ++ pawnFile.toList.map(value => MoveRef(value.decline, 1))).distinct

  private def position(paths: PreparedPaths, path: PreparedPath, count: Int): Option[Position] =
    paths.positionView(path, count).filter(view =>
      Set(RelationFactKind.GeometricControl, RelationFactKind.RayBarrier, RelationFactKind.LegalMove)
        .subsetOf(view.completeKinds)).map(new Position(_))

  private def owned(paths: PreparedPaths, path: PreparedPath): Boolean =
    paths.paths.exists(_ eq path) && path.certified.nonEmpty && path.ids == path.declared &&
      path.moves == path.sourceMoves && path.automaticTerminal.isEmpty

  private def pieces(p: Position): List[Piece] =
    chess.Square.all.flatMap(square => p.occupant(square.key))

  private def at(paths: PreparedPaths, path: PreparedPath, piece: Piece, count: Int): Option[Piece] =
    paths.trackedPieceAfter(path, piece, count).flatten

  private def events(path: PreparedPath, from: Int, through: Int): List[ObservedLineMaterialEvent] =
    path.certified.flatMap(_._2.material).toList.flatMap(_.events)
      .filter(event => event.plyOffset >= from && event.plyOffset <= through)

  /** An offer and its immediate recapture must remove these exact actors. */
  private def traded(paths: PreparedPaths, path: PreparedPath, original: Piece, offset: Int): Option[Piece] =
    for
      _ <- Option.when(offset >= 1 && offset + 1 < path.moves.size)(())
      before <- position(paths, path, offset)
      victim <- at(paths, path, original, offset)
      taking <- before.view.legalRows.flatMap(_.find(_.move == path.moves(offset)))
      if taking.captures(victim) && taking.actor.side != original.side
      reply <- position(paths, path, offset + 1)
      recapture <- reply.view.legalRows.flatMap(_.find(_.move == path.moves(offset + 1)))
      if recapture.captures(taking.actor.copy(square = taking.to)) && recapture.actor.side == original.side &&
        paths.trackedPieceAfter(path, original, offset + 1).contains(None)
      after <- position(paths, path, offset + 2)
      replacement <- after.occupant(recapture.to.key)
      if replacement.side == original.side && replacement.role == recapture.actor.role
    yield replacement

  private def retained(paths: PreparedPaths, path: PreparedPath, original: Piece,
      destination: String, slider: Piece, target: Piece): Option[KeptRole] =
    if !owned(paths, path) then None else
      // An offer can be accepted later, and the retained piece can start its
      // work before that exchange. Follow both original identities through a
      // bounded observed sequence; move order is not a functional premise.
      (2 until math.min(path.moves.size - 2, 6) by 2).iterator.flatMap { exchange =>
        for
          root <- position(paths, path, 0)
          before <- position(paths, path, exchange)
          kept <- at(paths, path, original, exchange)
          offer <- before.view.legalRows.flatMap(_.find(_.move == path.moves(exchange)))
          if offer.actor != kept && offer.actor.side == kept.side && offer.actor.role == kept.role &&
            offer.to.key == destination && offer.capture.isEmpty
          exchanged <- pieces(root).find(piece =>
            at(paths, path, piece, exchange).contains(offer.actor))
          trade <- (exchange + 1 until math.min(path.moves.size - 1, 12) by 2).iterator
            .flatMap(offset => traded(paths, path, exchanged, offset).map(offset -> _)).nextOption()
          (taken, replacement) = trade
          if replacement.square.key == destination && replacement.role.name == "pawn"
          // The original pressure can be shared: a rerouted slider attacks the
          // old pawn, while the retained piece attacks its connected pawn.
          // Both attacks and that pawn-support relation must be observed.
          linked = pieces(root).filter(piece => piece.side == target.side && piece.role.name == "pawn" &&
            piece != target && (root.controlledBy(target)(piece.square.key) ||
              root.controlledBy(piece)(target.square.key)))
          use <- (taken + 1 until math.min(path.moves.size, 14) by 2).find { offset =>
            (for
              p <- position(paths, path, offset + 1)
              actor <- at(paths, path, original, offset + 1)
              pawn <- at(paths, path, target, offset + 1)
            yield pawn == target && (p.controlledBy(actor)(pawn.square.key) ||
              at(paths, path, slider, offset + 1).exists(current => p.controlledBy(current)(pawn.square.key)) &&
                linked.exists(other => p.contains(other) && p.controlledBy(actor)(other.square.key)))).contains(true)
          }
          activation <- (0 to use by 2).reverse.find(offset =>
            at(paths, path, original, offset).exists(piece => path.moves(offset).take(2) == piece.square.key))
          arrived <- at(paths, path, original, use + 1)
          after <- position(paths, path, use + 1)
          pawn <- (target :: linked).find(piece => after.contains(piece) &&
            after.controlledBy(arrived)(piece.square.key))
          restored = Option.when(pawn != target)(at(paths, path, slider, use + 1).map(_ -> target)).flatten
          otherTargets = pieces(after).filter(piece => piece.side != arrived.side && piece.role.name == "pawn" &&
            piece != pawn && after.controlledBy(arrived)(piece.square.key)).sortBy(_.square.key)
        yield KeptRole(path, exchange, use, activation, exchanged, original, arrived, pawn, otherTargets, restored)
      }.nextOption()

  /** The offered exchange can free an entry square previously controlled by
    * the captured piece. This is a legal, unattacked rook entry toward a pawn,
    * not an engine ranking of the unplayed transfer. */
  private def rookAccess(paths: PreparedPaths, path: PreparedPath, rook: Piece,
      removed: Piece, targets: List[Piece]): Option[RookAccess] =
    if rook.role.name != "rook" || path.moves.size < 4 then None else
      for
        before <- position(paths, path, 1)
        current <- position(paths, path, 4)
        if current.contains(rook) && current.view.position.sideToMove.contains(rook.side)
        rows <- current.view.legalRows
        result <- targets.sortBy(_.square.key).iterator.flatMap { target =>
          val square = target.square.key.take(1) + rook.square.key.drop(1)
          val low = math.min(square.last.toInt, target.square.key.last.toInt)
          val high = math.max(square.last.toInt, target.square.key.last.toInt)
          val between = (low + 1 until high).map(rank => square.head.toString + rank.toChar)
          // Leaving the current square can uncover a slider behind the rook.
          val opensEnemyRay = current.rays.exists { (_, ray) =>
            val squares = ray.geometry.squares.map(_.key)
            ray.side != rook.side && squares.contains(square) &&
              squares.takeWhile(_ != square).flatMap(current.occupant) == List(rook)
          }
          Option.when(square != rook.square.key && square != target.square.key &&
            current.contains(target) && current.empty(square) &&
            before.controlledBy(removed)(square) && current.controllers(square, !rook.side).isEmpty &&
            rows.exists(row => row.actor == rook && row.to.key == square && row.capture.isEmpty) &&
            between.forall(current.empty) && !opensEnemyRay)(
            RookAccess(MoveRef(path, 3), rook, removed, square, target))
        }.nextOption()
      yield result

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      proof: TargetResponseInvestigation.FirstTarget, notes: List[LocalCandidate]): Option[Checked] =
    paths.requireOwner(packet)
    if !proof.belongsTo(packet, paths) || pair.size != 2 || !pair.forall(owned(paths, _)) then return None
    val q = proof.question
    for
      capture <- proof.captureDecline.pathsByMove.get(q.referenceMove)
      if capture.moves.size >= 3 && q.captureMoves.contains(capture.moves(1)) &&
        pair.exists(path => path.ids.head == q.rootMoveOccurrenceId)
      if owned(paths, capture)
      root <- position(paths, capture, 0)
      child <- position(paths, capture, 1)
      after <- position(paths, capture, 3)
      replacement <- traded(paths, capture, q.originalTarget, 1)
      takenBy <- child.view.legalRows.flatMap(_.find(_.move == capture.moves(1))).map(_.actor)
      // A later matched score may change its finite PV. A separately admitted
      // root-search continuation can still witness the retained piece's use;
      // it supplies no replacement score for the selected root comparison.
      retainedPaths = (pair ++ packet.assembly.input.evaluationObservations.filter(observation =>
        observation.source == EvaluationSource.RootSearch &&
          observation.evaluation.engineLine.exists(_.depth >= proof.captureDecline.work.requiredDepth))
        .flatMap(observation => paths.certifiedPath(packet, observation.evaluation.moves)))
        .filter(path => path.moves.head != q.rootMove).distinct
      loss = (for
        slider <- pieces(root) if slider.side == q.originalTarget.side &&
          Set("queen", "rook", "bishop")(slider.role.name) && after.contains(slider)
        target <- pieces(root) if target.side != slider.side && target.role.name == "pawn" &&
          after.contains(target) && root.controlledBy(slider)(target.square.key)
        (_, blockers) <- after.blockedRay(slider, target).toList
        if replacement.role.name == "pawn" && blockers == List(replacement) &&
          child.blockedRay(slider, target).exists(_._2 == List(q.target))
        kept <- retainedPaths.flatMap(path =>
          retained(paths, path, q.originalTarget, q.target.square.key, slider, target))
      yield ClosedLine(slider, target, replacement) -> kept).headOption
      decline = proof.captureDecline.pathsByMove.get(q.alternativeMove)
      // A structural alternative need not lead the numerical reply ranking.
      // Keep its ownership in the same completed full-pool search and describe
      // its function without transferring the leader's evaluation to it.
      file = (decline.toList ++ proof.fullPool.pathsByMove.values.toList.sortBy(_.moves.mkString))
        .distinct.iterator.flatMap { line => for
        _ <- Option.when(q.originalTarget.role.name == "pawn" && takenBy.role.name == "pawn" &&
          replacement.role.name == "pawn" && owned(paths, line) &&
          q.declineMoves.contains(line.moves(1)))(())
        before <- position(paths, line, 1)
        keptPosition <- position(paths, line, 2)
        retained <- at(paths, line, takenBy, 2)
        if retained.role.name == "pawn" && retained.square.key.head == takenBy.square.key.head &&
          retained.square != takenBy.square && line.moves(1).take(2) == takenBy.square.key
        blocked <- pieces(keptPosition).find(piece => piece.side == q.originalTarget.side &&
          piece.role.name == "pawn" && piece.square.key.head == retained.square.key.head &&
          piece.square.key.last.toInt == retained.square.key.last.toInt + (if retained.side.white then 1 else -1))
        file = takenBy.square.key.head
        if pieces(before).exists(piece => piece.role.name == "pawn" && piece.square.key.head == file) &&
          !pieces(after).exists(piece => piece.role.name == "pawn" && piece.square.key.head == file)
        attackers = keptPosition.controllers(retained.square.key, q.originalTarget.side)
        if attackers.nonEmpty
        restricted = pieces(child).find(piece => piece.side != q.target.side && piece.role.name == "pawn" &&
          piece.square.key.head == q.target.square.key.head &&
          piece.square.key.last.toInt + (if piece.side.white then 1 else -1) == q.target.square.key.last.toInt)
      yield PawnFile(line, file, takenBy, retained, blocked, attackers, restricted) }.nextOption()
      // A real continuation identifies the relevant pawn. Availability alone
      // cannot invent a useful target or replace an adequate current reply.
      targets = decline.toList.flatMap(path => notes.flatMap { note =>
        val offset = path.ids.indexOf(note.edge.evidence.id)
        note.note.detail match
          case Detail.Attack(actor, target, true) if offset >= 2 && offset <= 4 &&
              actor.side == q.originalTarget.side && actor.role.name == "rook" && target.role.name == "pawn" &&
              root.contains(target) && at(paths, path, target, offset + 1).contains(target) &&
              at(paths, capture, target, 4).contains(target) => Some(target)
          case _ => None
      }).distinct
      access = Option.when(loss.isEmpty)(rookAccess(paths, capture, replacement, takenBy, targets)).flatten
      if loss.nonEmpty || access.nonEmpty || file.nonEmpty
      checkedPaths = (List(capture) ++ loss.toList.map(_._2.path) ++ decline.toList ++ file.toList.map(_.decline)).distinct
      sources = checkedPaths.flatMap(path => path.ids.take(9) ++
        (0 to math.min(path.moves.size, 9)).flatMap(count =>
          paths.positionView(path, count).toList.map(_.occurrence.id))) ++
        loss.toList.flatMap { (closed, _) =>
          root.sourceRefs(root.sources(closed.slider, List(closed.target.square.key))) ++
            after.sourceRefs(after.blockedRay(closed.slider, closed.target).map(_._1).toList)
        } ++ access.toList.flatMap { entry =>
          val current = position(paths, capture, 4).get
          child.sourceRefs(child.sources(entry.removed, List(entry.square))) ++
            current.sourceRefs(current.view.legalRows.toList.flatten.filter(row =>
              row.actor == entry.rook && row.to.key == entry.square).map(_.sourceId))
        }
    yield new Checked(paths, capture, loss.map(_._1), loss.map(_._2), access, file, sources.distinct)

