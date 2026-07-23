package lila.chessjudgment.analysis.move

import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*

object MoveMotifNormalizer:

  def fromMotifs(
      id: String,
      moveUci: String,
      motifs: List[Motif],
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): List[EvidenceRecord] =
    motifs.zipWithIndex.map { case (motif, index) =>
      val event =
        MoveMotifEvent.fromMotif(
          rootMove = moveUci,
          motif = motif,
          position = position,
          line = line,
          scope = scope
        )
      val ref =
        EvidenceRef(
          id = eventId(id, event, index),
          producer = EvidenceProducer.MoveMotifProducer,
          layer = EvidenceLayer.MoveMotif,
          position = position,
          line = line,
          scope = scope,
          confidence = EvidenceConfidence.BoardDerived
        )
      EvidenceRecord(
        ref = ref,
        payload = MoveMotifEvidence(event),
        parents = parents
      )
    }

  private def eventId(baseId: String, event: MoveMotifEvent, index: Int): String =
    val move = event.eventMove.getOrElse("state")
    val raw = s"$baseId:${event.plyOffset}:$move:${event.kind}:$index"
    raw.replaceAll("[^A-Za-z0-9:_.-]", "-").toLowerCase
