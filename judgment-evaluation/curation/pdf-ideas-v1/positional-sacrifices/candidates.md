# Mastering Positional Sacrifices - source-only curation

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This artifact uses `document_id: ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; every locator below is this document ID plus 1-based PDF page(s). No local filesystem path or visual source asset is retained.

Scope: this is a reading and curation artifact based only on the assigned PDF. PDF page references are 1-based physical PDF pages, not the printed page numbers in the book's contents. The notes are deliberately short paraphrases for human semantic review, not labels, answer strings, or a move-to-meaning oracle. No engine was used, and the book's assessments are not asserted as engine truth.

"primary.pgn" contains only the five full source-game main lines below. Variations and alternative interpretations stay in prose so that a reviewer can assess the idea rather than match an expected line. Every primary is a complete game as presented in the PDF; no fragment, "SetUp", or "FEN" tag is needed.

## Primary set

| Game | PDF pages / core page | Anchor | Sacrifice and compensation | Neutral diversity |
|---|---:|---|---|---|
| John William Schulten - Paul Morphy, New York 1858, 0-1 | 16-19 / 17 | 6.Bd2 e3! 7.Bxe3 O-O | Pawn for time, development, and a dislocated white structure | Open game; Black initiative; opening to attack; long horizon |
| Viktor Kortchnoi - Friso Nijboer, Netherlands tt 1992/93 (9), 1-0 | 98-101 / 100 | 17.Rb1 g5 18.Nxc5! dxc5 19.Bxc5 | Minor piece, then rook, for connected passers and square control | Closed King’s Indian; White queenside versus Black flank attack; structural breakthrough |
| Shakhriyar Mamedyarov - Viswanathan Anand, Zagreb 2019 (9), 1-0 | 315-320 / 318 | 25.Nh6+ gxh6 26.fxe4 e5 27.Rd5!? Bxd5 28.exd5 | Exchange for a passed d-pawn, bishop pair, space, and initiative | Modern opening; restrained long-term pressure with explicit defense |
| Gert Ligterink - John Nunn, Marbella 1982 (6), 0-1 | 434-438 / 435 | 14.e5 Ndxe5 15.Nxe5 Nxe5 16.f4 Ng4! 17.Rxe8 Rxe8 | Queen for rook and pawn, with king insecurity, piece activity, and later pawn mass | Benoni; Black counterplay; defense-resource and transition test |
| David Bronstein - Ljubomir Ljubojevic, Petropolis 1973 (11), 1-0 | 452-456 / 454 | 16.Bb3 Bc5 17.Qf4! Bxg1 18.d6 | Full rook for central domination and attacking access; later a second exchange is offered | Four Pawns Alekhine; concrete forcing sequence; source-recorded defensive resource |

### John William Schulten - Paul Morphy

- Identity: White John William Schulten; Black Paul Morphy; event/year New York 1858; result 0-1. The PDF does not supply a full date or round.
- Source pages: 16-19. Core idea page: 17. Exact full main line: "primary.pgn", first game.
- Anchor sequence: 5.d3 Bb4 6.Bd2 e3! 7.Bxe3 O-O 8.Bd2 Bxc3 9.bxc3 Re8+.
- Evidence (paraphrased): The book presents e3 as a healthy pawn given up without an immediate recovery or forced mate. Its proposed return is time, ordinary development, and a white structure/piece placement that is difficult to coordinate.
- Alternative / counterpoint: The PDF names 8...Re8 and 8...c6 as alternatives. It also gives White's 11.h3 and later 12.h3 Bxe2 13.Nxe2 as defensive ideas, noting that Black's compensation can then become unclear; the game’s later attack follows White's 12.dxc6?.
- Diversity metadata: sacrifice side Black; material initially one pawn; open-file/development theme; opening phase; asymmetrical pawn structure after bxc3; attack/initiative perspective; long-term concept that later becomes tactical.
- PGN/replay: complete 20-move source game, 40 plies, parse/replay PASS. Unresolved: no legal-move ambiguity; exact calendar date and round are absent from the PDF.

### Viktor Kortchnoi - Friso Nijboer

- Identity: White Viktor Kortchnoi; Black Friso Nijboer; event/year Netherlands tt 1992/93 (9); result 1-0. The PDF does not expand tt or provide a full date.
- Source pages: 98-101. Core idea page: 100; conversion is on 101. Exact full main line: "primary.pgn", second game.
- Anchor sequence: 17.Rb1 g5 18.Nxc5! dxc5 19.Bxc5 Ng6 20.Bb6! Qf6 21.c5 g4 22.d6.
- Evidence (paraphrased): In a blocked King’s Indian, the book treats Nxc5 as a way to tear down Black’s queenside blockade and create connected c/d pawns. White then declines a tempting exchange on f8, advances centrally against the kingside pawn storm, and converts the initial piece investment into a rook investment while retaining light-square control.
- Alternative / counterpoint: The source warns that 20.Bxf8 Bxf8 would hand Black the dark squares. It also notes that 29...Nxc5 could restore material, but gives 30.Nd5 as a positional reason why the resulting position remains poor for Black.
- Diversity metadata: sacrifice side White; primary anchor is a knight sacrifice, followed by a rook sacrifice; closed center; middlegame; connected passers and light-square control; queenside/center versus flank attack; medium-to-long conversion horizon.
- PGN/replay: complete 33-move source game, 65 plies, parse/replay PASS. Unresolved: no legal-move ambiguity; source abbreviation/date only.

### Shakhriyar Mamedyarov - Viswanathan Anand

- Identity: White Shakhriyar Mamedyarov; Black Viswanathan Anand; event/year Zagreb 2019 (9); result 1-0. The PDF does not supply a full date.
- Source pages: 315-320. Core idea page: 318. Exact full main line: "primary.pgn", third game.
- Anchor sequence: 25.Nh6+ gxh6 26.fxe4 e5 27.Rd5!? Bxd5 28.exd5 Ng6 29.Rf1 Re7.
- Evidence (paraphrased): The book presents Rd5 as an artistic Russian exchange sacrifice after Black tries to close the position. The surviving d-pawn, bishop pair, space, and patient piece play carry the compensation, rather than an immediate forced finish.
- Alternative / counterpoint: The PDF calls 27.Bc1 a calm way to retain an edge and repeatedly identifies ...b5 as Black’s resource, including the Rxf7/...Rd6 idea. It further records 37.Qf1 as a better balance and 38...Qf2! as a saving chance, so this should not be treated as an uncontested verdict.
- Diversity metadata: sacrifice side White; exchange (rook for bishop); Vienna-type opening; middlegame with partially fixed center; passed pawn/bishop-pair compensation; attack-defense balance; long horizon with live defensive resources.
- PGN/replay: complete 45-move source game, 89 plies, parse/replay PASS. Unresolved: no legal-move ambiguity; full date absent.

### Gert Ligterink - John Nunn

- Identity: White Gert Ligterink; Black John Nunn; event/year Marbella 1982 (6); result 0-1. The PDF does not supply a full date.
- Source pages: 434-438. Core idea page: 435. Exact full main line: "primary.pgn", fourth game.
- Anchor sequence: 14.e5 Ndxe5 15.Nxe5 Nxe5 16.f4 Ng4! 17.Rxe8 Rxe8 18.Ne2 Ne3.
- Evidence (paraphrased): Black leaves the queen on e8 to be exchanged for a rook, then relies on the exposed white king, the misplaced g5-bishop, active minor pieces, and pawn collection. The game later transitions from pressure into a material-winning endgame with a dangerous c-pawn.
- Alternative / counterpoint: The source offers 16...h6 17.Bh4 g5 as a non-queen-sacrificial solution. For defense it highlights 20.Qc1, 27.Kxg2, and 29.h3; it also says 26...Nc4 would keep dynamic balance, so White’s inaccuracies materially affect the final result.
- Diversity metadata: sacrifice side Black; queen for rook and pawn at the anchor; Benoni structure; middlegame to endgame transition; king safety, dark-squared bishop activity, and outside passed pawn; defensive-resource perspective.
- PGN/replay: complete 37-move source game, 74 plies, parse/replay PASS. Unresolved: no legal-move ambiguity; full date absent.

### David Bronstein - Ljubomir Ljubojevic

- Identity: White David Bronstein; Black Ljubomir Ljubojevic; event/year Petropolis 1973 (11); result 1-0. The PDF does not supply a full date.
- Source pages: 452-456. Core idea page: 454. Exact full main line: "primary.pgn", fifth game.
- Anchor sequence: 16.Bb3 Bc5 17.Qf4! Bxg1 18.d6 Qc8 19.Ke2 Bc5 20.Ne4 N8d7 21.Rc1 Qc6 22.Rxc5!.
- Evidence (paraphrased): The book frames White’s willingness to lose the g1-rook as a full rook sacrifice supported by central dominance and access to the black king. The later Rxc5 keeps the line concrete, but the example is especially useful because the source also records ways for Black to resist or even take over.
- Alternative / counterpoint: The PDF gives 18...Qc5 19.Ne4 Qd4 20.Rd1 Qxb2 as the correct defensive plan in a later game, and calls 19.O-O-O a stronger practical continuation than the played king move. This is therefore a tactical-forcing and defensive-resource case, not a clean proof that the sacrifice always works.
- Diversity metadata: sacrifice side White; full rook for no immediate material return, then a separate exchange offer; Four Pawns Alekhine structure with a large center; middlegame; central domination/open king; short-to-medium forcing horizon; attack with explicit counterplay caveat.
- PGN/replay: complete 41-move source game, 81 plies, parse/replay PASS. Unresolved: no legal-move ambiguity; full date absent.

## Alternate pool - not included in primary.pgn

| Game | PDF pages / anchor | Why it is a reserve | Caution before promotion |
|---|---:|---|---|
| Vitaly Kunin - Ilja Zaragatski, Maastricht 2009 (3), 0-1 | 221-225 / 3...b5 4.cxb5 a6 | Standard Benko pawn sacrifice: long-term queenside activity, exchange-friendly endgame, and domination after heavy-piece entry. | Strong pawn-sacrifice fallback, but overlaps the primary pawn category; its full main line was not exported or replayed here. |
| Dmitry Rovner - Mikhail Tal, Riga 1955 (18), 0-1 | 324-328 / 18...Rxf3! | French exchange sacrifice for king damage, f3-square pressure, and a vulnerable d4-pawn. | Direct attacking counterpart to the Russian exchange sacrifice; reserve only, with no full PGN in this delivery. |
| Boris Spassky - David Bronstein, Amsterdam/Leeuwarden ct 1956 (12), 1-0 | 429-434 / 10...Nxf1! 11.Qxh4 Nxe3 | Queen for two minor pieces and a pawn, intended to keep a compact, coordinated position rather than force a quick mate. | The PDF itself notes a missed dynamic chance at 20...Ne7!; useful for disagreement testing, but not a cleaner primary than Nunn’s successful queen sacrifice. |

## Diversity check

The primary set spans Black and White sacrifices; pawn, minor-piece, exchange, queen, and rook material patterns; open and closed centers; flank play, structural breakthrough, central restriction, king attack, defense, and endgame conversion. The two intentionally more disputed examples are Mamedyarov-Anand and Bronstein-Ljubojevic, so a reviewer can test whether an explanation acknowledges concrete defensive resources instead of flattening every sacrifice into the same generic story.
