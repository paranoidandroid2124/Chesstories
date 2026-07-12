package lila.study

import com.softwaremill.macwire.*
import play.api.Configuration
import scala.annotation.unused

import lila.core.config.*

@Module
final class Env(
    appConfig: Configuration,
    lightUserApi: lila.core.user.LightUserApi,
    userApi: lila.core.user.UserApi,
    analyser: lila.tree.Analyser,
    analysisJson: lila.tree.AnalysisJson,
    annotator: lila.tree.Annotator,
    mongo: lila.db.Env,
    net: lila.core.config.NetConfig,
    cacheApi: lila.memo.CacheApi
)(using
    Executor,
    Scheduler,
    akka.stream.Materializer,
    lila.core.config.RateLimit
):

  private lazy val studyDb = mongo.asyncDb("study", appConfig.get[String]("study.mongodb.uri"))

  def version(@unused studyId: StudyId): Fu[Int] =
    fuccess(0)

  def isConnected(@unused studyId: StudyId, @unused userId: UserId): Fu[Boolean] =
    fuccess(false)

  val studyRepo = StudyRepo(studyDb(CollName("study")))

  val chapterRepo = ChapterRepo(studyDb(CollName("study_chapter_flat")))
  private val topicRepo = StudyTopicRepo(studyDb(CollName("study_topic")))
  private val userTopicRepo = StudyUserTopicRepo(studyDb(CollName("study_user_topic")))

  lazy val jsonView = wire[JsonView]

  private lazy val chapterMaker = wire[ChapterMaker]

  private lazy val explorerGame = wire[ExplorerGameApi]

  private lazy val studyMaker = wire[StudyMaker]

  private lazy val studyInvite = wire[StudyInvite]

  private lazy val sequencer = wire[StudySequencer]

  lazy val topicApi = wire[StudyTopicApi]

  lazy val api: StudyApi = wire[StudyApi]

  lazy val pager = wire[StudyPager]

  lazy val preview = wire[ChapterPreviewApi]

  lazy val pgnDump = wire[PgnDump]

  def findConnectedUsersIn(@unused studyId: StudyId)(
      @unused filter: Iterable[UserId] => Fu[List[UserId]]
  ): Fu[List[UserId]] =
    fuccess(Nil)

  lila.common.Cli.handle:
    case "study" :: "rank" :: "reset" :: Nil =>
      studyRepo.resetAllRanks.map: count =>
        s"$count done"
