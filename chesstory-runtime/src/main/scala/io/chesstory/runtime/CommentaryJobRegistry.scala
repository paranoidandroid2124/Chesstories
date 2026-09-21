package io.chesstory.runtime

import java.security.SecureRandom
import java.util.Base64
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.util.zip.{ GZIPInputStream, GZIPOutputStream }

import scala.collection.mutable
import scala.util.control.NonFatal
import play.api.libs.json.Json

enum CommentaryJobCreationRejection:
  case RegistryCapacityReached
  case JobStartRejected(reason: CommentaryJobStartRejection)

enum EngineWorkReportSubmissionRejection:
  case JobNotFound
  case WorkNotOutstanding

sealed trait CommentaryJobDelivery:
  def jobId: String

final case class CommentaryJobSnapshot(
    jobId: String,
    requestId: String,
    deadlineEpochMs: Long,
    state: CommentaryJobState
) extends CommentaryJobDelivery

/** Terminal jobs cannot consume more reports. Keep their complete public result,
  * compressed until delivery, without retaining the calculation graph or a second
  * uncompressed copy. Reads still return the exact original JSON bytes.
  */
private[runtime] final class RetainedCommentaryJobResult(
    val jobId: String,
    source: Array[Byte]
) extends CommentaryJobDelivery:
  private val compressedJsonBytes = RetainedCommentaryJobResult.compress(source)

  private[runtime] def writeCompressed(output: java.io.OutputStream): Unit = output.write(compressedJsonBytes)
  private[runtime] def compressedLength: Int = compressedJsonBytes.length

  private[runtime] def jsonBytes: Array[Byte] =
    val gzip = new GZIPInputStream(new ByteArrayInputStream(compressedJsonBytes))
    try gzip.readAllBytes()
    finally gzip.close()

private[runtime] object RetainedCommentaryJobResult:
  def compress(source: Array[Byte]): Array[Byte] =
    val buffer = new ByteArrayOutputStream()
    val gzip = new GZIPOutputStream(buffer)
    try gzip.write(source)
    finally gzip.close()
    buffer.toByteArray

final class CommentaryJobRegistry(
    maximumRetainedJobs: Int,
    terminalJobRetentionMillis: Long
):
  require(maximumRetainedJobs > 0, "maximumRetainedJobs must be positive")
  require(terminalJobRetentionMillis >= 0, "terminalJobRetentionMillis must not be negative")

  private final class RetainedCommentaryJob(
      val requestId: String,
      var state: CommentaryJobState | RetainedCommentaryJobResult,
      val deadlineEpochMs: Long,
      @volatile var forgetAfterEpochMs: Long
  )

  private val membershipLock = new Object
  private val secureRandom = SecureRandom()
  private val retainedJobs = mutable.Map.empty[String, RetainedCommentaryJob]
  private var reservedCreations = 0

  def createJob(
      requestId: String,
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      playedMoveUci: String,
      resultingFen: String,
      policy: CommentaryJobPolicy,
      nowEpochMs: Long,
      inputLines: List[lila.chessjudgment.model.judgment.SuppliedCommentaryLine] = Nil,
      proposals: List[CommentaryProposal.Request] = Nil
  ): Either[CommentaryJobCreationRejection, CommentaryJobDelivery] =
    removeForgottenJobsAt(nowEpochMs)
    if !reserveCreationSlot() then Left(CommentaryJobCreationRejection.RegistryCapacityReached)
    else
      val started =
        try CommentaryJobReducer.startJob(
          initialFen,
          movePrefixUci,
          currentFen,
          playedMoveUci,
          resultingFen,
          policy,
          nowEpochMs,
          inputLines,
          proposals
        )
        catch
          case NonFatal(error) =>
            releaseCreationSlot()
            throw error
      started match
        case Left(reason) =>
          releaseCreationSlot()
          Left(CommentaryJobCreationRejection.JobStartRejected(reason))
        case Right(state) =>
          val (jobId, retained) = membershipLock.synchronized:
            require(reservedCreations > 0, "commentary job creation reservation was lost")
            reservedCreations -= 1
            val jobId = nextUnusedJobId()
            val retained = retainNewJobAt(
              requestId,
              state,
              policy.deadlineEpochMs,
              nowEpochMs
            )
            retainedJobs.update(jobId, retained)
            (jobId, retained)
          // Projection may be expensive; never hold the registry membership lock for it.
          retained.synchronized { Right(snapshotOf(jobId, retained)) }

  def expireAndReadJobAt(jobId: String, nowEpochMs: Long): Option[CommentaryJobDelivery] =
    removeForgottenJobsAt(nowEpochMs)
    retainedJobFor(jobId) match
      case None => None
      case Some(retained) =>
        retained.synchronized:
          if !isCurrentRetainedJob(jobId, retained) then None
          else
            retained.state match
              case state: CommentaryJobState =>
                transitionRetainedJobAt(jobId, retained, CommentaryJobReducer.expireJob(state, nowEpochMs), nowEpochMs)
              case _: RetainedCommentaryJobResult => ()
            Some(snapshotOf(jobId, retained))

  def submitEngineWorkReportAt(
      jobId: String,
      report: EngineWorkReport,
      nowEpochMs: Long
  ): Either[EngineWorkReportSubmissionRejection, CommentaryJobDelivery] =
    removeForgottenJobsAt(nowEpochMs)
    retainedJobFor(jobId) match
      case None => Left(EngineWorkReportSubmissionRejection.JobNotFound)
      case Some(retained) =>
        retained.synchronized:
          if !isCurrentRetainedJob(jobId, retained) then
            Left(EngineWorkReportSubmissionRejection.JobNotFound)
          else
            retained.state match
              case _: RetainedCommentaryJobResult =>
                Left(EngineWorkReportSubmissionRejection.WorkNotOutstanding)
              case state: CommentaryJobState =>
                CommentaryJobReducer.submitIssuedWorkReport(state, report, nowEpochMs) match
                  case Left(EngineWorkReportRejection.WorkNotOutstanding) =>
                    Left(EngineWorkReportSubmissionRejection.WorkNotOutstanding)
                  case Right(nextState) =>
                    transitionRetainedJobAt(jobId, retained, nextState, nowEpochMs)
                    Right(snapshotOf(jobId, retained))

  def cancelJobAt(jobId: String, nowEpochMs: Long): Option[CommentaryJobDelivery] =
    removeForgottenJobsAt(nowEpochMs)
    retainedJobFor(jobId) match
      case None => None
      case Some(retained) =>
        retained.synchronized:
          if !isCurrentRetainedJob(jobId, retained) then None
          else
            retained.state match
              case state: CommentaryJobState =>
                val expired = CommentaryJobReducer.expireJob(state, nowEpochMs)
                transitionRetainedJobAt(jobId, retained, expired, nowEpochMs)
                transitionRetainedJobAt(jobId, retained, CommentaryJobReducer.cancelJob(expired), nowEpochMs)
              case _: RetainedCommentaryJobResult => ()
            Some(snapshotOf(jobId, retained))

  /** The caller has consumed or abandoned the result. No consumer remains for a
    * stopped projection: release the graph/result without constructing another one.
    * Serialize with reports on this job; membership locks never cover projection.
    */
  def releaseJob(jobId: String): Boolean = retainedJobFor(jobId).exists { retained =>
    retained.synchronized {
      membershipLock.synchronized {
        if retainedJobs.get(jobId).contains(retained) then
          retainedJobs.remove(jobId)
          true
        else false
      }
    }
  }

  private def retainNewJobAt(
      requestId: String,
      state: CommentaryJobState,
      deadlineEpochMs: Long,
      nowEpochMs: Long
  ): RetainedCommentaryJob =
    new RetainedCommentaryJob(
      requestId,
      state,
      deadlineEpochMs,
      forgetAfterStateTransitionAt(None, state, deadlineEpochMs, nowEpochMs)
    )

  private def transitionRetainedJobAt(
      jobId: String,
      retained: RetainedCommentaryJob,
      nextState: CommentaryJobState,
      nowEpochMs: Long
  ): Unit =
    assertCurrentRetainedJob(jobId, retained)
    val forgetAfterEpochMs = forgetAfterStateTransitionAt(
      Some(retained),
      nextState,
      retained.deadlineEpochMs,
      nowEpochMs
    )
    retained.state = nextState
    retained.forgetAfterEpochMs = forgetAfterEpochMs

  private def forgetAfterStateTransitionAt(
      previous: Option[RetainedCommentaryJob],
      nextState: CommentaryJobState,
      deadlineEpochMs: Long,
      nowEpochMs: Long
  ): Long =
    val enteringTerminal = !previous.exists(retained => isTerminalJobState(retained.state)) &&
      isTerminalJobState(nextState)
    if enteringTerminal then
      nextState match
        case stopped: StoppedCommentaryJob
            if stopped.stopCondition == CommentaryJobStopCondition.DeadlineExceeded =>
          retentionDeadlineFrom(deadlineEpochMs)
        case _ => retentionDeadlineFrom(nowEpochMs)
    else previous.fold(retentionDeadlineFrom(deadlineEpochMs))(_.forgetAfterEpochMs)

  private def retentionDeadlineFrom(epochMs: Long): Long =
    if epochMs > Long.MaxValue - terminalJobRetentionMillis then Long.MaxValue
    else epochMs + terminalJobRetentionMillis

  private def isTerminalJobState(state: CommentaryJobState | RetainedCommentaryJobResult): Boolean = state match
    case _: AwaitingEngineWork => false
    case _: CompletedCommentaryJob | _: StoppedCommentaryJob | _: RetainedCommentaryJobResult => true

  private def removeForgottenJobsAt(nowEpochMs: Long): Unit =
    val candidates = membershipLock.synchronized:
      retainedJobs.iterator.collect {
        case (jobId, retained) if retained.forgetAfterEpochMs <= nowEpochMs =>
          (jobId, retained)
      }.toList
    candidates.foreach { case (jobId, retained) =>
      retained.synchronized:
        membershipLock.synchronized:
          retainedJobs.get(jobId) match
            case Some(current)
                if (current eq retained) && retained.forgetAfterEpochMs <= nowEpochMs =>
              retainedJobs.remove(jobId)
            case _ => ()
    }

  private def retainedJobFor(jobId: String): Option[RetainedCommentaryJob] =
    membershipLock.synchronized:
      retainedJobs.get(jobId)

  private def reserveCreationSlot(): Boolean =
    membershipLock.synchronized:
      if retainedJobs.size + reservedCreations >= maximumRetainedJobs then false
      else
        reservedCreations += 1
        true

  private def releaseCreationSlot(): Unit =
    membershipLock.synchronized:
      require(reservedCreations > 0, "commentary job creation reservation was lost")
      reservedCreations -= 1

  private def isCurrentRetainedJob(jobId: String, retained: RetainedCommentaryJob): Boolean =
    membershipLock.synchronized:
      retainedJobs.get(jobId) match
        case Some(current) => current eq retained
        case None => false

  private def assertCurrentRetainedJob(jobId: String, retained: RetainedCommentaryJob): Unit =
    if !isCurrentRetainedJob(jobId, retained) then
      throw IllegalStateException("retained job membership was lost after admission")

  private def nextUnusedJobId(): String =
    var jobId = ""
    while jobId.isEmpty || retainedJobs.contains(jobId) do
      val randomBytes = Array.ofDim[Byte](24)
      secureRandom.nextBytes(randomBytes)
      jobId = Base64.getUrlEncoder.withoutPadding.encodeToString(randomBytes)
    jobId

  private def snapshotOf(jobId: String, retained: RetainedCommentaryJob): CommentaryJobDelivery =
    retained.state match
      case result: RetainedCommentaryJobResult => result
      case state: CommentaryJobState =>
        val snapshot = CommentaryJobSnapshot(jobId, retained.requestId, retained.deadlineEpochMs, state)
        if !isTerminalJobState(state) then snapshot
        else
          val bytes = Json.toBytes(PositionCommentaryJobProtocol.projectJobSnapshot(snapshot))
          val result = RetainedCommentaryJobResult(jobId, bytes)
          retained.state = result
          result
