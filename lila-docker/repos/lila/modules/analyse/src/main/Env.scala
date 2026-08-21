package lila.analyse

import com.softwaremill.macwire.*

import lila.core.config.NetConfig

@Module
final class Env(net: NetConfig):

  lazy val annotator = Annotator(net.domain)
