package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** A legal resource and the best returned competing choice have different
  * evidence owners. A full legal survey chooses the competitor; one exact
  * comparison supplies both values. Neither receipt changes the root grade.
  */
private[runtime] object CommentaryChoiceValue:
  import CommentaryProposal.CompareMoves
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import PrincipalVariationComparison.QualityBand
  import TargetResponseInvestigation.Search

  final class Checked private[CommentaryChoiceValue] (
      private val owner: EvidenceBackedJudgmentPacket, private val prepared: PreparedPaths,
      val question: CompareMoves, val survey: Search, val comparison: Search,
      val answer: CommentaryInvestigation.Answer, val reference: PreparedPath,
      val alternative: PreparedPath):
    def belongsTo(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): Boolean =
      (owner eq packet) && (prepared eq paths)
    def useful: Boolean = answer.qualityBand.contains(QualityBand.Close) ||
      Set("equal", "worse")(answer.evaluationRelation)
    def refuted: Boolean = answer.qualityBand.contains(QualityBand.Separated) &&
      answer.evaluationRelation == "better"
    def json: JsObject = Json.obj("prefix_moves_uci" -> question.prefixMovesUci,
      "reference_move_uci" -> question.referenceMove,
      "alternative_move_uci" -> question.alternativeMove,
      "survey_work_id" -> survey.work.workId,
      "survey_execution_key_sha256" -> survey.work.executionKeySha256,
      "comparison_work_id" -> comparison.work.workId,
      "comparison_execution_key_sha256" -> comparison.work.executionKeySha256,
      "engine_profile" -> comparison.work.engineProfile.wireId,
      "required_depth" -> comparison.work.requiredDepth,
      "completed_depth" -> answer.referenceDepth,
      "search_position_id" -> comparison.anchor.positionId,
      "ply_offset" -> comparison.anchor.plyOffset,
      "useful" -> useful, "refuted" -> refuted)

  def pairDepth(packet: EvidenceBackedJudgmentPacket, prefix: List[String],
      searches: List[Search], profile: String, minimumDepth: Int): Int =
    latestPool(packet, prefix, searches, profile, minimumDepth).flatMap(_.completedDepth)
      .getOrElse(minimumDepth)

  /** A pair that completed beyond the selection pool requires a new full
    * survey at that level, rather than repeatedly issuing its old lower
    * request or borrowing incomparable values. */
  def selectionDepth(packet: EvidenceBackedJudgmentPacket, question: CompareMoves,
      searches: List[Search], profile: String, minimumDepth: Int): Int =
    (pairDepth(packet, question.prefixMovesUci, searches, profile, minimumDepth) ::
      searches.filter(search => search.belongsTo(packet) &&
        search.anchor.prefixMoves == question.prefixMovesUci && search.work.engineProfile.wireId == profile)
        .flatMap(search => search.pairAnswer(packet, question.referenceMove, question.alternativeMove)
          .filter(answer => answer.referenceDepth == answer.alternativeDepth)
          .flatMap(_.referenceDepth))).max

  private def latestPool(packet: EvidenceBackedJudgmentPacket, prefix: List[String],
      searches: List[Search], profile: String, minimumDepth: Int): Option[Search] =
    searches.reverse.filter(search => search.belongsTo(packet) &&
      search.anchor.prefixMoves == prefix && search.anchor.plyOffset == prefix.size &&
      search.work.rootRestriction == EngineRootRestriction.Unrestricted &&
      search.work.engineProfile.wireId == profile &&
        search.completedDepth.exists(_ >= minimumDepth))
      .sortBy(search => (-search.completedDepth.get, -search.work.requiredDepth)).headOption

  /** A later exact counter can retire this selection pool. Its owned pair
    * supplies the contradiction; values are never copied into the old pool.
    * Do not fall back to an older selection after the current one failed. */
  private def selection(packet: EvidenceBackedJudgmentPacket, prefix: List[String],
      searches: List[Search], profile: String, minimumDepth: Int): Option[CommentaryComparisonConflict.Selection] =
    latestPool(packet, prefix, searches, profile, minimumDepth)
      .map(pool => new CommentaryComparisonConflict.Selection(packet, pool, searches)).filterNot(_.rankingReversed)

  def survey(packet: EvidenceBackedJudgmentPacket, prefix: List[String],
      searches: List[Search], profile: String, minimumDepth: Int): Option[Search] =
    selection(packet, prefix, searches, profile, minimumDepth).map(_.pool)

  def strongestOther(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pool: Search, referenceMove: String): Option[String] =
    paths.requireOwner(packet)
    for
      _ <- Option.when(pool.belongsTo(packet) &&
        pool.work.rootRestriction == EngineRootRestriction.Unrestricted && pool.completedDepth.nonEmpty)(())
      path <- pool.lines.headOption.flatMap(line => pool.pathsByMove.get(line.evaluation.moves.head))
      if paths.paths.exists(_ eq path)
      rows <- paths.positionView(path, pool.anchor.plyOffset).flatMap(_.legalRows)
      if rows.exists(_.move == referenceMove)
      other <- pool.lines.find(line => line.evaluation.moves.head != referenceMove)
      move = other.evaluation.moves.head
      if rows.exists(_.move == move)
    yield move

  /** Only the returned near-leading group is known. A finite top-N survey
    * does not establish that unreturned replies are outside that group.
    */
  def adequateReplies(packet: EvidenceBackedJudgmentPacket, pool: Search): List[String] =
    if !pool.belongsTo(packet) || pool.work.rootRestriction != EngineRootRestriction.Unrestricted ||
        pool.completedDepth.isEmpty then Nil
    else pool.lines.headOption.toList.flatMap { leader =>
      val move = leader.evaluation.moves.head
      move :: pool.lines.tail.flatMap { peer =>
        val other = peer.evaluation.moves.head
        Option.when(pool.pairAnswer(packet, move, other).exists(answer =>
          answer.qualityBand.contains(QualityBand.Close)))(other)
      }
    }

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      question: CompareMoves, searches: List[Search], profile: String,
      minimumDepth: Int): Option[Checked] =
    resolve(packet, paths, question, searches, profile, minimumDepth).toOption.flatten

  /** The exact current selection/value conflict is a required local dependency.
    * Its child's answer cannot become this choice's owned numerical value. */
  def conflict(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      question: CompareMoves, searches: List[Search], profile: String,
      minimumDepth: Int): Option[CommentaryComparisonConflict.Conflict] =
    resolve(packet, paths, question, searches, profile, minimumDepth).left.toOption

  def resolve(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      question: CompareMoves, searches: List[Search], profile: String,
      minimumDepth: Int): Either[CommentaryComparisonConflict.Conflict, Option[Checked]] =
    rawCandidate(packet, paths, question, searches, profile, minimumDepth) match
      case None => Right(None)
      case Some((value, current)) =>
        current.valueConflict(Set(question.referenceMove, question.alternativeMove)).toLeft(Some(value))

  private def rawCandidate(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      question: CompareMoves, searches: List[Search], profile: String,
      minimumDepth: Int): Option[(Checked, CommentaryComparisonConflict.Selection)] =
    paths.requireOwner(packet)
    for
      current <- selection(packet, question.prefixMovesUci, searches, profile, minimumDepth)
      pool = current.pool
      strongest <- strongestOther(packet, paths, pool, question.referenceMove)
      if strongest == question.alternativeMove
      selected <- pool.pathsByMove.get(strongest)
      epochStart = current.epochStart
      compared <- searches.reverse.filter(search => search.belongsTo(packet) &&
        search.anchor.scopeKey == pool.anchor.scopeKey && search.work.engineProfile == pool.work.engineProfile &&
        search.completedDepth.exists(_ >= minimumDepth))
        .sortBy(search => -search.work.requiredDepth).find(search =>
          searches.indexWhere(_ eq search) >= epochStart &&
          search.pairAnswer(packet, question.referenceMove, question.alternativeMove).exists(answer =>
            answer.referenceDepth == pool.completedDepth && answer.alternativeDepth == pool.completedDepth))
      answer <- compared.pairAnswer(packet, question.referenceMove, question.alternativeMove)
      reference <- compared.pathsByMove.get(question.referenceMove)
      alternative <- compared.pathsByMove.get(question.alternativeMove)
      if List(reference, alternative, selected).forall(path => paths.paths.exists(_ eq path))
      count = pool.anchor.plyOffset
      if reference.moves.take(count) == question.prefixMovesUci &&
        alternative.moves.take(count) == question.prefixMovesUci &&
        reference.ids.take(count) == alternative.ids.take(count) &&
        reference.positions.take(count + 1) == alternative.positions.take(count + 1) &&
        alternative.moves.take(count + 1) == selected.moves.take(count + 1) &&
        alternative.ids.take(count + 1) == selected.ids.take(count + 1) &&
        alternative.positions.take(count + 2) == selected.positions.take(count + 2)
    yield (new Checked(packet, paths, question, pool, compared, answer, reference, alternative), current)
