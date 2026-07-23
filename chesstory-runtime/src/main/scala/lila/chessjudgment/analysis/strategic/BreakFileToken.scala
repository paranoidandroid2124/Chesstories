package lila.chessjudgment.analysis.strategic

private[strategic] object BreakFileToken:

  private val FileMarkerToken =
    """(?i)(?:^|[^a-z0-9])([a-h])[-_](?:file|break|pawn)(?=$|[^a-z0-9])""".r
  private val SquareToken =
    """(?i)(?:^|[^a-z0-9])([a-h])[1-8](?=$|[^a-z0-9])""".r
  private val SingleFileToken =
    """(?i)^\s*([a-h])\s*$""".r

  def extract(raw: String): Option[String] =
    val tokenSource = Option(raw).getOrElse("").trim
    if tokenSource.isEmpty then None
    else
      FileMarkerToken
        .findFirstMatchIn(tokenSource)
        .orElse(SquareToken.findFirstMatchIn(tokenSource))
        .orElse(SingleFileToken.findFirstMatchIn(tokenSource))
        .map(_.group(1).toLowerCase)

  def extractOrEmpty(raw: String): String =
    extract(raw).getOrElse("")

  def extractOrUnknown(raw: String): String =
    extract(raw).getOrElse("unknown")
