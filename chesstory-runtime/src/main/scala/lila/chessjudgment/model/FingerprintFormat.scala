package lila.chessjudgment.model

private[chessjudgment] object FingerprintFormat:

  def fixed2(value: Double): String =
    if value.isNaN || value.isInfinity then value.toString
    else
      val scaled = math.round(value * 100.0)
      val sign = if scaled < 0 then "-" else ""
      val abs = math.abs(scaled)
      val cents = abs % 100
      val centsText = if cents < 10 then s"0$cents" else cents.toString
      s"$sign${abs / 100}.$centsText"
