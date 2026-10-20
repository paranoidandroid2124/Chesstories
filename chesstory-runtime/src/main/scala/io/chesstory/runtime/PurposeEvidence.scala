package io.chesstory.runtime

import RootQuestionSummary.MoveRef

/** A useful effect, its comparison and the replies that qualify it are separate
  * receipts. A geometric contact alone supplies none of these receipts. */
private[runtime] final case class PurposeEvidence(
    uses: List[MoveRef] = Nil,
    choices: List[MoveRef] = Nil,
    replies: List[MoveRef] = Nil,
    leadingReply: List[MoveRef] = Nil,
    corresponding: List[MoveRef] = Nil):
  def realized: Boolean = uses.nonEmpty
  def compared: Boolean = realized && choices.map(_.path.moves.head).distinct.size == 2
  def answered: Boolean = compared && replies.nonEmpty
  def repliesExplained: Boolean = realized && replies.nonEmpty
  def priority: (Int, Int, Int, Int, Int, Int) =
    (if compared then 0 else 1,
      if compared && corresponding.map(_.path.moves.head).distinct.size == 2 then 0 else 1,
      if uses.exists(use => choices.exists(_.path eq use.path)) then 0 else 1,
      if leadingReply.nonEmpty then 0 else 1,
      if replies.nonEmpty then 0 else 1, if realized then 0 else 1)
  def outranks(other: PurposeEvidence): Boolean =
    summon[Ordering[(Int, Int, Int, Int, Int, Int)]].lt(priority, other.priority)
  def references: List[MoveRef] = (uses ++ choices ++ replies ++ leadingReply ++ corresponding).distinct

private[runtime] object PurposeEvidence:
  /** Match the same physical beneficiary's witnessed action across the actual
    * choices. This aligns a comparison; it does not by itself prove a purpose. */
  def correspondingEntries(paths: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath], original: MoveWorkProjection.Piece,
      square: String): List[MoveRef] =
    val entries = pair.flatMap { path =>
      (0 until math.min(path.moves.size, 18) by 2).find(n =>
        path.moves(n).slice(2, 4) == square &&
          paths.trackedPieceAfter(path, original, n).flatten.exists(_.square.key == path.moves(n).take(2)))
        .map(MoveRef(path, _))
    }
    if entries.map(_.path.moves.head).distinct.size == 2 then entries else Nil

  def conditionDiffers(paths: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath])(
      condition: (ObservedLineProtocol.PreparedPath, MoveWorkProjection.Position) => Boolean): Boolean =
    val values = pair.map(path => paths.purposePosition(path, 1).filter(_.complete(
      lila.chessjudgment.model.judgment.RelationFactKind.GeometricControl)).map(condition(path, _)))
    values.size == 2 && values.forall(_.nonEmpty) && values.distinct.size == 2

  /** Availability alone is a fact. Coupled to an observed use it can compare
    * preparing an action with the actual other move leaving its route blocked. */
  def entryDiffers(paths: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath], original: MoveWorkProjection.Piece, square: String): Boolean =
    conditionDiffers(paths, pair) { (path, at) =>
      paths.trackedPieceAfter(path, original, 1).flatten.exists(piece =>
        piece.square.key == square || at.empty(square) && at.controlledBy(piece)(square))
    }
