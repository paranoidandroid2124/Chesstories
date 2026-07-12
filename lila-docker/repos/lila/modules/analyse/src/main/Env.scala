package lila.analyse

import com.softwaremill.macwire.*

import lila.core.config.{ CollName, NetConfig }

@Module
final class Env(
    db: lila.db.Db,
    net: NetConfig
)(using Executor):

  lazy val analysisRepo = AnalysisRepo(db(CollName("analysis2")))

  lazy val requesterApi = RequesterApi(db(CollName("analysis_requester")))

  lazy val importHistory = ImportHistoryApi(
    accountColl = db(CollName("analysis_import_account")),
    analysisColl = db(CollName("analysis_import_history"))
  )

  lazy val analyser = wire[Analyser]

  lazy val annotator = Annotator(net.domain)

  val jsonView = JsonView
