package io.chesstory.runtime

import java.security.SecureRandom
import java.util.Base64

import scala.collection.mutable

enum CommentaryJobCreationRejection:
  case RegistryCapacityReached
  case JobStartRejected(reason: CommentaryJobStartRejection)

enum EngineWorkReportSubmissionRejection:
  case JobNotFound
  case WorkNotOutstanding

final case class CommentaryJobSnapshot(
    jobId: String,
    requestId: String,
    deadlineEpochMs: Long,
    state: CommentaryJobState
)

final class CommentaryJobRegistry(
    maximumRetainedJobs: Int,
    terminalJobRetentionMillis: Long
):
  require(maximumRetainedJobs > 0, "maximumRetainedJobs must be positive")
  require(terminalJobRetentionMillis >= 0, "terminalJobRetentionMillis must not be negative")

  private final class RetainedCommentaryJob(
      val requestId: String,
      var state: CommentaryJobState,
      val deadlineEpochMs: Long,
      @volatile var forgetAfterEpochMs: Long
  )

  private val creationLock = new Object
  private val membershipLock = new Object
  private val secureRandom = SecureRandom()
  private val retainedJobs = mutable.Map.empty[String, RetainedCommentaryJob]

  def createJob(
      requestId: String,
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      policy: CommentaryJobPolicy,
      nowEpochMs: Long
  ): Either[CommentaryJobCreationRejection, CommentaryJobSnapshot] =
    creationLock.synchronized:
      removeForgottenJobsAt(nowEpochMs)
      val capacityReached = membershipLock.synchronized:
        retainedJobs.size >= maximumRetainedJobs
      if capacityReached then Left(CommentaryJobCreationRejection.RegistryCapacityReached)
      else
        CommentaryJobReducer.startJob(initialFen, movePrefixUci, currentFen, policy, nowEpochMs) match
          case Left(reason) => Left(CommentaryJobCreationRejection.JobStartRejected(reason))
          case Right(state) =>
            membershipLock.synchronized:
              if retainedJobs.size >= maximumRetainedJobs then
                throw IllegalStateException("retained job capacity changed while creation was serialized")
              val jobId = nextUnusedJobId()
              val retained = retainNewJobAt(
                requestId,
                state,
                policy.deadlineEpochMs,
                nowEpochMs
              )
              retainedJobs.update(jobId, retained)
              Right(snapshotOf(jobId, retained))

  def expireAndReadJobAt(jobId: String, nowEpochMs: Long): Option[CommentaryJobSnapshot] =
    removeForgottenJobsAt(nowEpochMs)
    retainedJobFor(jobId) match
      case None => None
      case Some(retained) =>
        retained.synchronized:
          if !isCurrentRetainedJob(jobId, retained) then None
          else
            transitionRetainedJobAt(
              jobId,
              retained,
              CommentaryJobReducer.expireJob(retained.state, nowEpochMs),
              nowEpochMs
            )
            Some(snapshotOf(jobId, retained))

  def submitEngineWorkReportAt(
      jobId: String,
      report: EngineWorkReport,
      nowEpochMs: Long
  ): Either[EngineWorkReportSubmissionRejection, CommentaryJobSnapshot] =
    removeForgottenJobsAt(nowEpochMs)
    retainedJobFor(jobId) match
      case None => Left(EngineWorkReportSubmissionRejection.JobNotFound)
      case Some(retained) =>
        retained.synchronized:
          if !isCurrentRetainedJob(jobId, retained) then
            Left(EngineWorkReportSubmissionRejection.JobNotFound)
          else
            CommentaryJobReducer.submitIssuedWorkReport(retained.state, report, nowEpochMs) match
              case Left(EngineWorkReportRejection.WorkNotOutstanding) =>
                Left(EngineWorkReportSubmissionRejection.WorkNotOutstanding)
              case Right(nextState) =>
                transitionRetainedJobAt(jobId, retained, nextState, nowEpochMs)
                Right(snapshotOf(jobId, retained))

  def cancelJobAt(jobId: String, nowEpochMs: Long): Option[CommentaryJobSnapshot] =
    removeForgottenJobsAt(nowEpochMs)
    retainedJobFor(jobId) match
      case None => None
      case Some(retained) =>
        retained.synchronized:
          if !isCurrentRetainedJob(jobId, retained) then None
          else
            transitionRetainedJobAt(
              jobId,
              retained,
              CommentaryJobReducer.expireJob(retained.state, nowEpochMs),
              nowEpochMs
            )
            transitionRetainedJobAt(
              jobId,
              retained,
              CommentaryJobReducer.cancelJob(retained.state),
              nowEpochMs
            )
            Some(snapshotOf(jobId, retained))

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
          deadlineEpochMs + terminalJobRetentionMillis
        case _ => nowEpochMs + terminalJobRetentionMillis
    else previous.fold(deadlineEpochMs + terminalJobRetentionMillis)(_.forgetAfterEpochMs)

  private def isTerminalJobState(state: CommentaryJobState): Boolean = state match
    case _: AwaitingEngineWork => false
    case _: CompletedCommentaryJob | _: StoppedCommentaryJob => true

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

  private def snapshotOf(jobId: String, retained: RetainedCommentaryJob): CommentaryJobSnapshot =
    CommentaryJobSnapshot(
      jobId,
      retained.requestId,
      retained.deadlineEpochMs,
      retained.state
    )
