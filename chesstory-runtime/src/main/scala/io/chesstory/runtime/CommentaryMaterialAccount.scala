package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** Counts one capture interval in its original admitted material ledger. */
private[runtime] object CommentaryMaterialAccount:
  final case class Cost(
      commonExchanges: List[(EvidencePieceRole, Int)],
      whiteAdditionalLosses: List[(EvidencePieceRole, Int)],
      blackAdditionalLosses: List[(EvidencePieceRole, Int)],
      events: List[ObservedLineMaterialEvent],
      offsets: List[Int]
  )

  private val roles = List("pawn", "knight", "bishop", "rook", "queen").map(EvidencePieceRole(_))

  def physicalLosses(paths: ObservedLineProtocol.PreparedPaths, price: CommentaryAnswer.Material):
      Option[List[MoveWorkProjection.Piece]] =
    paths.continuationReadings.find(r => (r.path eq price.path) && r.complete).flatMap { reading =>
      val events = price.cost.events.flatMap(material => reading.events.find(event =>
        event.plyOffset == material.plyOffset && material.promotionGainCp == 0 &&
          material.capture.exists(c => event.originalVictim.exists(p =>
            p.role == c.capturedRole && p.side != c.side))))
      val losses = events.flatMap(_.originalVictim)
      Option.when(events.size == price.cost.events.size && losses.distinct.size == losses.size)(losses)
    }

  /** The interval runs from the first selected capture through the last.
    * Every material change inside it must be present. Promotions require a
    * different account; quiet context moves may be retained in offsets.
    */
  def calculate(
      material: LineMaterialSummary,
      eventOffsets: List[Int],
      offsets: List[Int],
      plyCount: Int
  ): Option[Cost] =
    if eventOffsets.isEmpty || eventOffsets.distinct.size != eventOffsets.size ||
        !offsets.forall(offset => offset >= 0 && offset < plyCount) ||
        !eventOffsets.forall(offsets.contains) then None
    else
      val events = eventOffsets.flatMap(offset => material.events.find(_.plyOffset == offset))
      val interval = material.events.filter(event =>
        event.plyOffset >= eventOffsets.min && event.plyOffset <= eventOffsets.max)
      if events.size != eventOffsets.size || interval.toSet != events.toSet ||
          events.exists(event => event.capture.isEmpty || event.promotionGainCp != 0) then None
      else
        val captures = events.flatMap(_.capture)
        def losses(white: Boolean) = captures.filter(_.side.white != white)
          .groupMapReduce(_.capturedRole)(_ => 1)(_ + _)
        val white = losses(true)
        val black = losses(false)
        val common = roles.map(role => role -> math.min(white.getOrElse(role, 0), black.getOrElse(role, 0)))
          .filter(_._2 > 0)
        val commonCounts = common.toMap
        def additional(counts: Map[EvidencePieceRole, Int]) = roles.map(role =>
          role -> (counts.getOrElse(role, 0) - commonCounts.getOrElse(role, 0))).filter(_._2 > 0)
        Some(Cost(common, additional(white), additional(black), events, offsets.distinct.sorted))
