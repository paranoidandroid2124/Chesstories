package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.{
  EvidenceBackedJudgmentPacket,
  JudgmentAssemblyContext,
  JudgmentClaim
}

/** The question-side input that an acquisition oracle may inspect.
  *
  * Actual engine variations and probe results are deliberately absent: an
  * oracle Q producer must acquire its own evidence instead of editing the
  * production acquisition output after seeing it.
  */
final case class JudgmentQuestionInput(
    fen: String,
    playedMoveUci: String,
    ply: Option[Int],
    openingContext: Option[RawOpeningContext],
    movePrefixUci: List[String]
)

object JudgmentQuestionInput:
  def fromRaw(raw: RawMoveReviewInput): JudgmentQuestionInput =
    JudgmentQuestionInput(
      fen = raw.fen,
      playedMoveUci = raw.playedMoveUci,
      ply = raw.ply,
      openingContext = raw.openingContext,
      movePrefixUci = raw.movePrefixUci
    )

/** Explicit, invocation-local substitutions at the native judgment boundaries. */
final case class JudgmentBoundaryIntervention(
    q: Option[JudgmentQuestionInput => RawMoveReviewInput] = None,
    f: Option[RawMoveReviewInput => JudgmentAssemblyContext] = None,
    c: Option[JudgmentAssemblyContext => JudgmentAssemblyContext] = None,
    jp: Option[JudgmentAssemblyContext => List[JudgmentClaim]] = None,
    ja: Option[(JudgmentAssemblyContext, List[JudgmentClaim]) => ClaimCandidateGraph] = None,
    r: Option[(JudgmentAssemblyContext, ClaimCandidateGraph) => ClaimRankingResult] = None
)

object JudgmentBoundaryIntervention:
  val identity: JudgmentBoundaryIntervention = JudgmentBoundaryIntervention()

/** The exact native values consumed and produced by one judgment invocation. */
final case class JudgmentBoundaryExecution(
    q: RawMoveReviewInput,
    f: JudgmentAssemblyContext,
    c: JudgmentAssemblyContext,
    jp: List[JudgmentClaim],
    ja: ClaimCandidateGraph,
    r: ClaimRankingResult,
    context: JudgmentAssemblyContext,
    packet: Option[EvidenceBackedJudgmentPacket]
)
