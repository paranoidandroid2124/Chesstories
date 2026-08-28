package lila.chessjudgment.model.line

import chess.{ HalfMoveClock, Move, Position, Role }
import chess.format.Fen

/**
 * The single authoritative result of replaying a legal engine line.
 * Board-aware consumers share this object instead of parsing the same UCI
 * sequence into tactical, structural, and assessment-specific line types.
 */
private[chessjudgment] final case class LegalReplayStep(
    ply: Int,
    uci: String,
    before: Position,
    move: Move,
    after: Position,
    capturedRole: Option[Role]
)

private[chessjudgment] object LegalReplayStep:
  def fromMove(ply: Int, before: Position, move: Move): LegalReplayStep =
    require(ply > 0, "a legal replay step must have a positive ply")
    require(move.before == before, "a legal replay move must belong to its exact source occurrence")
    val capturedRole =
      move.capture.flatMap(before.board.roleAt).orElse(before.board.roleAt(move.dest))
    LegalReplayStep(
      ply = ply,
      uci = PrincipalVariationEvidence.normalizeUci(move.toUci.uci),
      before = before,
      move = move,
      after = move.after,
      capturedRole = capturedRole
    )

private[chessjudgment] final class StripedLruCache[K, V](
    maxEntries: Int,
    requestedStripes: Int = 64
):
  require(maxEntries > 0)
  require(requestedStripes > 0)

  private val stripeCount = math.min(maxEntries, requestedStripes)
  private val caches = Vector.tabulate(stripeCount) { index =>
    val stripeMaxEntries =
      maxEntries / stripeCount + Option.when(index < maxEntries % stripeCount)(1).getOrElse(0)
    new java.util.LinkedHashMap[K, V](stripeMaxEntries, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean =
        size() > stripeMaxEntries
  }
  private val inFlight =
    new java.util.concurrent.ConcurrentHashMap[K, java.util.concurrent.CompletableFuture[V]]()

  private def cached(cache: java.util.LinkedHashMap[K, V], key: K): Option[V] =
    cache.synchronized {
      if cache.containsKey(key) then Some(cache.get(key)) else None
    }

  def getOrCompute(key: K)(compute: => V): V =
    val cache = caches(Math.floorMod(key.##, stripeCount))
    cached(cache, key).getOrElse {
      val promise = new java.util.concurrent.CompletableFuture[V]()
      val existing = inFlight.putIfAbsent(key, promise)
      if existing != null then
        try existing.join()
        catch
          case failure: java.util.concurrent.CompletionException if failure.getCause != null =>
            throw failure.getCause
      else
        try
          val value = cached(cache, key).getOrElse {
            val calculated = compute
            cache.synchronized(cache.put(key, calculated))
            calculated
          }
          promise.complete(value)
          value
        catch
          case failure: Throwable =>
            promise.completeExceptionally(failure)
            throw failure
        finally inFlight.remove(key, promise)
    }

/** Exact occurrence key for computations whose result retains scalachess
  * `Move.before`/`Move.after` objects. Board- or history-equivalent positions
  * cannot share those objects: each move remains owned by the Position
  * instance that generated it. Semantic board geometry uses its separate
  * cache upstream.
  */
private[chessjudgment] final class PositionOccurrenceKey private (
    private val position: Position
):
  private val cachedHash = System.identityHashCode(position)

  override def hashCode: Int = cachedHash

  override def equals(other: Any): Boolean =
    other match
      case that: PositionOccurrenceKey =>
        position.asInstanceOf[AnyRef] eq that.position.asInstanceOf[AnyRef]
      case _ => false

private[chessjudgment] object PositionOccurrenceKey:
  def from(position: Position): PositionOccurrenceKey =
    new PositionOccurrenceKey(position)

private[chessjudgment] object PrincipalVariationEvidence:

  private val PositionCacheMaxEntries = 4096
  private val positionCache =
    new StripedLruCache[String, Option[Position]](PositionCacheMaxEntries)
  private val legalMoveCache =
    new StripedLruCache[PositionOccurrenceKey, List[Move]](PositionCacheMaxEntries)

  def readPosition(fen: String): Option[Position] =
    val key = normalizeFen(fen)
    positionCache.getOrCompute(key) {
      Fen
        .read(chess.variant.Standard, Fen.Full(key))
        .map(withFenHalfMoveClock(_, key))
    }

  /** The sole legal-move generator for commentary analysis. The complete
    * Position key retains clocks and repetition history, so Move.after values
    * are never shared across merely board-equivalent occurrences.
    *
    * scalachess exposes both standard-UCI king-destination castling and the
    * UCI_Chess960 king-to-rook alias even in a Standard position. Chesstory's
    * public engine contract is Standard, so only the actual king-destination
    * representation owns the legal resource. This is representation
    * canonicalization, not result deduplication.
    */
  private[chessjudgment] def actualLegalMoves(position: Position): List[Move] =
    legalMoveCache.getOrCompute(PositionOccurrenceKey.from(position))(
      position.legalMoves.toList.filter(move =>
        move.castle.forall(castle => move.dest == castle.kingTo)
      ).sortBy(_.toUci.uci)
    )

  final case class LineMoveRef(
      ply: Int,
      uci: String,
      fenAfter: String
  )

  /** scalachess positions do not retain the FEN clock field when parsed.
    * Restore it once at the shared parsing boundary so rule assessment and
    * every derived Move.after carry the actual occurrence state.
    */
  private[chessjudgment] def withFenHalfMoveClock(position: Position, fen: String): Position =
    fen.trim.split("\\s+").lift(4).flatMap(_.toIntOption).filter(_ >= 0) match
      case Some(clock) if position.history.halfMoveClock != HalfMoveClock(clock) =>
        position.updateHistory(_.setHalfMoveClock(HalfMoveClock(clock)))
      case _ => position

  final case class LineVariationRef(
      moves: List[LineMoveRef]
  )

  final case class LineFacts(
      line: LineVariationRef,
      first: LineMoveRef,
      reply: Option[LineMoveRef],
      continuation: Option[LineMoveRef],
      continuationTail: List[LineMoveRef] = Nil
  )

  def legalFenAfter(fen: String, uciMove: String): Option[String] =
    legalMoveReplay(fen, List(uciMove), startPly = 0)
      .flatMap(_.headOption)
      .map(step => Fen.write(step.after).value)

  def legalReplay(startFen: String, moves: List[String], startPly: Int): Option[List[(String, LineMoveRef)]] =
    legalMoveReplay(startFen, moves, startPly).map(_.map { step =>
      Fen.write(step.before).value ->
        LineMoveRef(step.ply, step.uci, Fen.write(step.after).value)
    })

  def legalMoveReplay(
      startFen: String,
      moves: List[String],
      startPly: Int
  ): Option[List[LegalReplayStep]] =
    readPosition(startFen).flatMap { position =>
      legalMoveReplayFromPosition(position, moves, startPly)
    }

  private[chessjudgment] def legalMoveReplayFromPosition(
      start: Position,
      moves: List[String],
      startPly: Int
  ): Option[List[LegalReplayStep]] =
    val accepted = scala.collection.mutable.ListBuffer.empty[LegalReplayStep]
    var ply = startPly
    var legal = true
    var current = start
    moves.iterator.map(normalizeUci).foreach { uci =>
      if legal then
        legalMove(current, uci) match
          case Some(move) =>
            val before = current
            ply += 1
            accepted += LegalReplayStep.fromMove(ply, before, move)
            current = move.after
          case None =>
            legal = false
    }
    Option.when(legal)(accepted.toList)

  def normalizeUci(uci: String): String =
    Option(uci).getOrElse("").trim.toLowerCase

  def sameBoardState(left: String, right: String): Boolean =
    semanticBoardStateFen(left).zip(semanticBoardStateFen(right)).exists {
      case (leftState, rightState) => leftState == rightState
    }

  def semanticBoardStateFen(fen: String): Option[String] =
    val fields = normalizeFen(fen).split("\\s+").filter(_.nonEmpty).toList
    Option.when(fields.size >= 4)(fields.take(4).mkString(" "))

  private def legalMove(position: Position, uci: String): Option[Move] =
    val normalized = normalizeUci(uci)
    Option
      .when(normalized.matches("^[a-h][1-8][a-h][1-8][qrbn]?$"))(normalized)
      .flatMap(candidate =>
        actualLegalMoves(position).find(move => normalizeUci(move.toUci.uci) == candidate)
      )

  def normalizeFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).mkString(" ")
