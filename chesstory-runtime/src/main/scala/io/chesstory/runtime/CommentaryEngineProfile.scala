package io.chesstory.runtime

/** The closed server allowlist for browser-executed commentary engine work. */
enum CommentaryEngineProfile(val wireId: String):
  case Sf18SmallnetT2H16V1
      extends CommentaryEngineProfile("sf18-smallnet-t2-h16-v1")

object CommentaryEngineProfile:
  val ServerAdmitted: CommentaryEngineProfile = Sf18SmallnetT2H16V1

  def parseWireId(value: String): Option[CommentaryEngineProfile] = value match
    case ServerAdmitted.wireId => Some(ServerAdmitted)
    case _                     => None
