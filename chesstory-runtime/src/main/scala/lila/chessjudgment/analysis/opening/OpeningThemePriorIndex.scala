package lila.chessjudgment.analysis.opening

import java.util.Locale

import scala.io.Source
import scala.util.Using

import lila.chessjudgment.model.judgment.*

final class OpeningThemePriorIndex private (
    entries: List[OpeningThemePrior]
):

  private val byLineage: Map[String, OpeningThemePrior] =
    entries.flatMap(entry => entry.lineage.map(_ -> entry)).toMap

  private val byFamily: Map[OpeningFamily, OpeningThemePrior] =
    entries
      .filter(entry => entry.lineage.exists(OpeningThemePriorIndex.isFamilyFallbackLineage))
      .flatMap(entry => entry.family.map(_ -> entry))
      .toMap

  def allEntries: List[OpeningThemePrior] =
    entries

  def priorFor(lineage: Option[String], family: Option[OpeningFamily]): Option[OpeningThemePrior] =
    priorFor(lineage, family, None)

  def priorFor(
      lineage: Option[String],
      family: Option[OpeningFamily],
      openingName: Option[String]
  ): Option[OpeningThemePrior] =
    priorSelectionFor(lineage, family, openingName).map(_.prior)

  def priorSelectionFor(
      lineage: Option[String],
      family: Option[OpeningFamily],
      openingName: Option[String]
  ): Option[OpeningThemePriorSelection] =
    val requestedLineage = lineage.map(OpeningRecognitionIndex.normalizeLineage)
    requestedLineage
      .flatMap(priorForLineage)
      .orElse(priorForNameHint(openingName, family, requestedLineage))
      .orElse(priorForFamily(family, requestedLineage))

  private def priorForLineage(lineage: String): Option[OpeningThemePriorSelection] =
    byLineage
      .get(lineage)
      .map(prior => priorSelection(prior, OpeningThemePriorMatchSource.ExactLineage, Some(lineage), Some(lineage)))
      .orElse:
        OpeningThemePriorIndex.LineageAliases
          .get(lineage)
          .flatMap(canonical =>
            byLineage
              .get(canonical)
              .map(prior => priorSelection(prior, OpeningThemePriorMatchSource.LineageAlias, Some(lineage), Some(canonical)))
          )

  private def priorForNameHint(
      openingName: Option[String],
      family: Option[OpeningFamily],
      requestedLineage: Option[String]
  ): Option[OpeningThemePriorSelection] =
    openingName
      .flatMap(name => OpeningThemePriorIndex.lineageHintForName(name, family))
      .flatMap(lineage =>
        byLineage
          .get(lineage)
          .map(prior => priorSelection(prior, OpeningThemePriorMatchSource.NameHint, requestedLineage, Some(lineage)))
      )

  private def priorForFamily(
      family: Option[OpeningFamily],
      requestedLineage: Option[String]
  ): Option[OpeningThemePriorSelection] =
    family.flatMap: openingFamily =>
      byFamily
        .get(openingFamily)
        .map(prior =>
          priorSelection(
            prior,
            OpeningThemePriorMatchSource.FamilyFallback,
            requestedLineage,
            prior.lineage
          )
        )

  private def priorSelection(
      prior: OpeningThemePrior,
      source: OpeningThemePriorMatchSource,
      requestedLineage: Option[String],
      canonicalLineage: Option[String]
  ): OpeningThemePriorSelection =
    OpeningThemePriorSelection(
      prior = prior,
      matchSource = source,
      requestedLineage = requestedLineage,
      canonicalLineage = canonicalLineage.orElse(prior.lineage)
    )

object OpeningThemePriorIndex:

  private final case class LineageRule(
      lineage: String,
      families: Set[OpeningFamily],
      lineageAliases: List[String],
      nameNeedles: List[String]
  )

  private val LineageRules: List[LineageRule] =
    List(
      rule(
        "flank/kings_indian_attack",
        Set(OpeningFamily.A),
        aliases = List("king_s_indian_attack", "kings_indian_attack"),
        needles = List("king s indian attack")
      ),
      rule("flank/nimzo_larsen", Set(OpeningFamily.A), aliases = List("nimzo_larsen_attack", "nimzo_larsen"), needles = List("nimzo larsen")),
      rule("flank/bird", Set(OpeningFamily.A), aliases = List("bird_opening"), needles = List("bird opening")),
      rule("flank/reti", Set(OpeningFamily.A), aliases = List("reti", "reti_opening"), needles = List("reti opening")),
      rule("flank/zukertort", Set(OpeningFamily.A), aliases = List("zukertort_opening"), needles = List("zukertort")),
      rule("flank/english", Set(OpeningFamily.A), aliases = List("english", "english_opening"), needles = List("english opening")),
      rule("flank/dutch", Set(OpeningFamily.A), aliases = List("dutch_defense"), needles = List("dutch defense")),
      rule("flank/benoni", Set(OpeningFamily.A), aliases = List("benoni_defense"), needles = List("benoni defense")),
      rule("indian/catalan", Set(OpeningFamily.E), aliases = List("catalan_opening", "catalan"), needles = List("catalan")),
      rule(
        "indian/kings_indian",
        Set(OpeningFamily.E),
        aliases = List("kings_indian", "king_s_indian_defense", "kings_indian_defense"),
        needles = List("king s indian defense")
      ),
      rule(
        "indian/gruenfeld",
        Set(OpeningFamily.D),
        aliases = List("gruenfeld", "gruenfeld_defense", "grunfeld", "grunfeld_defense"),
        needles = List("gruenfeld defense", "grunfeld defense")
      ),
      rule("indian/nimzo_indian", Set(OpeningFamily.E), aliases = List("nimzo_indian", "nimzo_indian_defense"), needles = List("nimzo indian")),
      rule(
        "indian/queens_indian",
        Set(OpeningFamily.E),
        aliases = List("queen_s_indian_defense", "queens_indian_defense"),
        needles = List("queen s indian")
      ),
      rule("indian/bogo_indian", Set(OpeningFamily.E), aliases = List("bogo_indian_defense"), needles = List("bogo indian")),
      rule("indian/indian_game", Set(OpeningFamily.E), aliases = List("indian_game"), needles = List("indian game")),
      rule("semi_open/caro_kann", Set(OpeningFamily.B), aliases = List("caro_kann", "caro_kann_defense"), needles = List("caro kann")),
      rule("semi_open/scandinavian", Set(OpeningFamily.B), aliases = List("scandinavian_defense"), needles = List("scandinavian")),
      rule("semi_open/alekhine", Set(OpeningFamily.B), aliases = List("alekhine_defense"), needles = List("alekhine")),
      rule("semi_open/pirc_modern", Set(OpeningFamily.B), aliases = List("pirc_defense"), needles = List("pirc", "robatsch", "modern defense", "rat defense")),
      rule("semi_open/sicilian", Set(OpeningFamily.B), aliases = List("sicilian", "sicilian_defense"), needles = List("sicilian")),
      rule("open_games/italian", Set(OpeningFamily.C), aliases = List("italian", "italian_game"), needles = List("italian game")),
      rule("open_games/kings_gambit", Set(OpeningFamily.C), aliases = List("kings_gambit", "king_s_gambit"), needles = List("king s gambit")),
      rule("open_games/ruy_lopez", Set(OpeningFamily.C), aliases = List("ruy_lopez", "spanish_opening"), needles = List("ruy lopez", "spanish opening")),
      rule("open_games/scotch", Set(OpeningFamily.C), aliases = List("scotch", "scotch_game"), needles = List("scotch game")),
      rule("open_games/four_knights", Set(OpeningFamily.C), aliases = List("four_knights_game"), needles = List("four knights")),
      rule("open_games/vienna", Set(OpeningFamily.C), aliases = List("vienna_game"), needles = List("vienna game")),
      rule("open_games/petrov", Set(OpeningFamily.C), aliases = List("petrov_defense", "petroff_defense"), needles = List("petrov", "petroff")),
      rule("open_games/philidor", Set(OpeningFamily.C), aliases = List("philidor_defense"), needles = List("philidor")),
      rule("open_games/kings_pawn", Set(OpeningFamily.C), aliases = List("kings_pawn", "king_s_pawn_game"), needles = List("king s pawn game")),
      rule(
        "open_games/kings_knight",
        Set(OpeningFamily.C),
        aliases = List("kings_knight", "king_s_knight_opening"),
        needles = List("king s knight opening")
      ),
      rule("open_games/french_winawer", Set(OpeningFamily.C), aliases = List("french_winawer"), needles = List("french defense winawer")),
      rule("open_games/french", Set(OpeningFamily.C), aliases = List("french", "french_defense"), needles = List("french defense")),
      rule("open_games/center_game", Set(OpeningFamily.C), aliases = List("center_game"), needles = List("center game")),
      rule("queen_pawn/slav", Set(OpeningFamily.D), aliases = List("slav", "slav_defense"), needles = List("slav defense")),
      rule("queen_pawn/albin", Set(OpeningFamily.D), aliases = List("albin_countergambit"), needles = List("albin countergambit")),
      rule("queen_pawn/london", Set(OpeningFamily.D), aliases = List("london_system"), needles = List("london system")),
      rule(
        "queen_pawn/queens_gambit",
        Set(OpeningFamily.D),
        aliases = List("queen_s_gambit", "queens_gambit"),
        needles = List("queen s gambit")
      ),
      rule(
        "queen_pawn/queen_pawn_system",
        Set(OpeningFamily.D),
        aliases = List("queen_s_pawn_game", "queens_pawn_game", "colle_system"),
        needles = List("queen s pawn game", "colle system")
      )
    )

  private val LineageAliases: Map[String, String] =
    LineageRules.flatMap(rule => rule.lineageAliases.map(alias => alias -> rule.lineage)).toMap

  private def rule(
      lineage: String,
      families: Set[OpeningFamily],
      aliases: List[String],
      needles: List[String]
  ): LineageRule =
    LineageRule(lineage, families, lineageAliases = aliases.distinct, nameNeedles = needles.distinct)

  private val FamilyFallbackLineages: Set[String] =
    Set("eco_a", "eco_b", "eco_c", "eco_d", "eco_e")

  private[opening] def isFamilyFallbackLineage(lineage: String): Boolean =
    FamilyFallbackLineages.contains(lineage)

  private[opening] def lineageHintForName(
      openingName: String,
      family: Option[OpeningFamily]
  ): Option[String] =
    val normalized = normalizeOpeningName(openingName)
    LineageRules.collectFirst {
      case rule
          if family.forall(rule.families.contains) &&
            rule.nameNeedles.exists(needle => normalized.contains(needle)) =>
        rule.lineage
    }

  private[chessjudgment] def familyHintForName(openingName: String): Option[OpeningFamily] =
    val normalized = normalizeOpeningName(openingName)
    LineageRules.collectFirst {
      case rule if rule.families.size == 1 && rule.nameNeedles.exists(needle => normalized.contains(needle)) =>
        rule.families.head
    }

  private[opening] def lineageForOpeningName(openingName: String): String =
    val familyName = Option(openingName).getOrElse("").split(":", 2).headOption.getOrElse("")
    lineageHintForName(openingName, None)
      .orElse(lineageHintForName(familyName, None))
      .getOrElse(lineageSlug(familyName))

  private def lineageSlug(raw: String): String =
    Option(raw)
      .getOrElse("")
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^a-z0-9]+", "_")
      .stripPrefix("_")
      .stripSuffix("_")

  private def normalizeOpeningName(raw: String): String =
    Option(raw)
      .getOrElse("")
      .toLowerCase(Locale.ROOT)
      .replace('\u00e9', 'e')
      .replace('\u00e8', 'e')
      .replace('\u00fc', 'u')
      .replaceAll("[^a-z0-9]+", " ")
      .replaceAll("\\s+", " ")
      .trim

  val TsvHeader: String =
    "lineage\tfamily\tthemes\ttypical_pawn_structures\tcenter_breaks\tdevelopment_priorities\tgambit_compensation\tstrategic_plan_priors"

  private val ResourcePath = "lila/chessjudgment/openings/opening_theme_prior_index.tsv"

  lazy val default: OpeningThemePriorIndex =
    fromEntries(loadResourceRows(ResourcePath))

  val empty: OpeningThemePriorIndex =
    fromEntries(Nil)

  def fromEntries(entries: IterableOnce[OpeningThemePrior]): OpeningThemePriorIndex =
    OpeningThemePriorIndex(entries.iterator.toList)

  def fromTsvLines(lines: IterableOnce[String]): OpeningThemePriorIndex =
    fromEntries(parseRows(lines))

  def tsvRow(entry: OpeningThemePrior): String =
    List(
      entry.lineage.getOrElse(""),
      entry.family.map(_.toString).getOrElse(""),
      entry.themes.map(_.toString).mkString("|"),
      entry.typicalPawnStructures.mkString("|"),
      entry.centerBreaks.mkString("|"),
      entry.developmentPriorities.mkString("|"),
      entry.gambitCompensation.toString,
      entry.strategicPlanPriors.mkString("|")
    ).mkString("\t")

  private def loadResourceRows(path: String): List[OpeningThemePrior] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).toList.flatMap: stream =>
      Using.resource(Source.fromInputStream(stream, "UTF-8")): source =>
        parseRows(source.getLines()).toList

  private def parseRows(lines: IterableOnce[String]): Iterable[OpeningThemePrior] =
    lines.iterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .dropWhile(_.toLowerCase(Locale.ROOT).startsWith("lineage\t"))
      .flatMap(parseLine)
      .toList

  private def parseLine(line: String): Option[OpeningThemePrior] =
    line.split("\t", -1).toList match
      case lineageRaw :: familyRaw :: themesRaw :: structuresRaw :: breaksRaw :: developmentRaw ::
          gambitRaw :: plansRaw :: _ =>
        val themes = splitList(themesRaw).flatMap(theme)
        val lineage = OpeningRecognitionIndex.cleanText(lineageRaw).map(OpeningRecognitionIndex.normalizeLineage)
        val family = OpeningRecognitionIndex.cleanText(familyRaw).flatMap(OpeningFamily.fromRaw)
        Option.when(lineage.nonEmpty || family.nonEmpty)(
          OpeningThemePrior(
            lineage = lineage,
            family = family,
            themes = themes.distinct,
            typicalPawnStructures = splitList(structuresRaw),
            centerBreaks = splitList(breaksRaw),
            developmentPriorities = splitList(developmentRaw),
            gambitCompensation = gambitRaw.trim.equalsIgnoreCase("true"),
            strategicPlanPriors = splitList(plansRaw)
          )
        )
      case _ => None

  private def splitList(raw: String): List[String] =
    Option(raw).getOrElse("").split("[|]").toList.map(_.trim).filter(_.nonEmpty)

  private def theme(raw: String): Option[OpeningTheme] =
    val normalized = raw.trim
    OpeningTheme.values.find(_.toString == normalized)
      .orElse:
        val key = normalized.toLowerCase(Locale.ROOT).replace("-", "").replace("_", "")
        OpeningTheme.values.find(_.toString.toLowerCase(Locale.ROOT) == key)
