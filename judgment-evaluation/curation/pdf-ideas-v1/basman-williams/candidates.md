# The Basman-Williams Attack - curated representative games

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This artifact uses `document_id: ref-009749ddb7ef4fe0a033b33e9712fc74`; every locator below is this document ID plus 1-based PDF page(s). No local filesystem path or visual source asset is retained.

Scope: this is a read-only curation from the designated PDF only (`ref-009749ddb7ef4fe0a033b33e9712fc74`). No internet material or engine evaluation was added. Commentary below is short paraphrase for human review, not a label set, answer key, or one-to-one oracle.

Corpus boundary: the move and position data supports development curation and session semantic review only; this file is reviewer context.

## Primary selection

| White - Black | Printed event/place string, year, result | PDF pages; key page | Main idea axis | Complete main line |
|---|---|---:|---|---|
| M.Basman - N.Grinberg | Ramat Hasharon, 1980, 1-0 | 7-10; 8-9 | early flank expansion, central counterplay and restraint | `primary.pgn`, first game; full game, no fragment/FEN |
| S.Williams - P.Poobalasingam | Hastings, 2008, 1-0 | 10-12; 11-12 | over-advanced flank pawn, structural constraint, delayed queenside break | `primary.pgn`, second game; full game, no fragment/FEN |
| M.Vachier Lagrave - I.Nepomniachtchi | FIDE Grand Prix (Jerusalem), 2019, 1/2-1/2 | 22-24; 23-24 | pawn investment, central counterbreak, initiative transfer | `primary.pgn`, third game; full game, no fragment/FEN |
| R.Rapport - N.Grandelius | Malmö, 2013, 1/2-1/2 | 13-15; 14-15 | material grab versus long compensation and defensive resource | `primary.pgn`, fourth game; full game, no fragment/FEN |
| R.Rapport - P.Svidler | Grand Chess Tour Rapid (Paris), 2021, 1-0 | 28-29; 29 | tactical forcing line and short finish | `primary.pgn`, fifth game; full game, no fragment/FEN |

The PDF supplies only a year for every primary and does not state a round. Those fields are `?` in the PGN; the first, second, and fourth printed strings do not cleanly distinguish event from site, so they are reproduced without adding a more specific claim.

### M.Basman - N.Grinberg

- Identity: M.Basman - N.Grinberg; printed string `Ramat Hasharon 1980`; result 1-0. PDF 7-10; core discussion on 8-9.
- Main line: complete 34-move game in the first PGN record; no SetUp/FEN is needed.
- Idea anchor and local run: `10.Qb3 e6 11.g4! Ng7 12.Bh6 f5 13.Bh3`. The unusual h-pawn start becomes a space-and-piece bind rather than an immediate forced attack.
- Paraphrased evidence: the book presents the h-pawn advance as a disruption that makes the h5-knight awkward, then treats g4 and Bh6 as a squeeze on Black's coordination. It also shows that Black can seek activity instead of merely accepting the bind, so the game is not evidence that flank space automatically wins.
- Alternative / counterexample: the book points to `10...a6` as a more active response, and later gives `20...Nh5!` as a tactical way to obtain real counterplay. These are reviewer prompts, not evaluated truth claims.
- Neutral diversity metadata: Grünfeld-type center; opening into strategic middlegame; no planned material imbalance; White attacking space, Black central/counterplay perspective; medium horizon.
- Unresolved ambiguity: no move ambiguity remains after source-page checking and replay. Exact date, round, and whether the printed `Ramat Hasharon` is event or site are not stated.

### S.Williams - P.Poobalasingam

- Identity: S.Williams - P.Poobalasingam; printed string `Hastings 2008`; result 1-0. PDF 10-12; core discussion on 11-12.
- Main line: complete 42-move game in the second PGN record; no SetUp/FEN is needed.
- Idea anchor and local run: `8.Be2 h6 9.h5 g5? 10.Be3 b6 11.Bd1 Bd7 12.Nge2 c6? 13.Bxc5`. The early black g-pawn advance shuts down Black's own counterplay and leaves targets that White can exploit without rushing a king attack.
- Paraphrased evidence: the book uses the game to connect the light-square weaknesses and space advantage with later mobility on the b-file and queenside. This is a useful failure mode for an aggressive flank plan: closing the wing can become a self-imposed restriction.
- Alternative / counterexample: the PDF gives `9...Nxh5 10.Bxh5 gxh5 11.Be3 Rg8` as a live alternative with chances for both sides.
- Neutral diversity metadata: King’s Indian-style closed center; middlegame to conversion; roughly balanced material until the late sequence; White structural/expansion perspective, Black defensive perspective; long horizon.
- Unresolved ambiguity: no move ambiguity remains after source-page checking and replay. Exact date, round, and the event-versus-site meaning of `Hastings` are not printed.

### M.Vachier Lagrave - I.Nepomniachtchi

- Identity: M.Vachier Lagrave - I.Nepomniachtchi; FIDE Grand Prix (Jerusalem), 2019; result 1/2-1/2. PDF 22-24; core discussion on 23-24.
- Main line: complete 32-move game in the third PGN record; no SetUp/FEN is needed.
- Idea anchor and local run: `23.Ne2 Nb5?! 24.a4 Nd6 25.Ng3? f5! 26.exf5 Qd5! 27.Nc4 Nxc4 28.Qxc4 Qxc4 29.Rxc4 Nb6`. The anchor is Black's central break, which turns White's attacking grip into a defensive problem.
- Paraphrased evidence: the book frames White's earlier g-pawn thrust and e-pawn play as a pawn investment for development, initiative, and a broad center. It then identifies Black's reorganization and `...f5` as the move that releases the center and transfers the initiative; the draw is agreed while the tension remains.
- Alternative / counterexample: the source prefers `20.Nfe2` to the played knight route and suggests `23...O-O` as a more testing Black continuation than the played `23...Nb5`.
- Neutral diversity metadata: open Grünfeld-type structure; early middlegame transition; White offers a pawn for activity; Black counterattacking/central-break perspective; medium horizon.
- Unresolved ambiguity: no move ambiguity remains after source-page checking and replay. Exact date and round are not printed.

### R.Rapport - N.Grandelius

- Identity: R.Rapport - N.Grandelius; printed string `Malmö 2013`; result 1/2-1/2. PDF 13-15; core discussion on 14-15.
- Main line: complete 48-move game in the fourth PGN record; no SetUp/FEN is needed.
- Idea anchor and local run: `19.Kd1 Nf6 20.e5 Ng4?? 21.Kc2 Ba5 22.Ng5 Bf5+ 23.Kb3 Bd8`. The corresponding defensive resource recorded by the book is the unplayed `20...Ne4!`.
- Paraphrased evidence: after Black collects material, the book emphasizes the exposed king and undeveloped queenside as compensation for White. It also records a defensive alternative that would keep the game contested, while White later lets much of the practical advantage go; the draw therefore tests both compensation and conversion judgment.
- Alternative / counterexample: the PDF also gives `14...Rd4` as a less greedy option and `32.Nd3` as a way for White to keep stronger chances than the played `32.d6?`.
- Neutral diversity metadata: King’s Indian/Benoni pawn center; sharp middlegame into a reduced-material ending; Black has an extra pawn/active material grab against White’s activity; mutual attack-defense perspective; long horizon.
- Unresolved ambiguity: no move ambiguity remains after source-page checking and replay. Exact date, round, and whether `Malmö` is event or site are not printed.

### R.Rapport - P.Svidler

- Identity: R.Rapport - P.Svidler; Grand Chess Tour Rapid (Paris), 2021; result 1-0. PDF 28-29; core discussion on 29.
- Main line: complete 18-move game in the fifth PGN record; no SetUp/FEN is needed.
- Idea anchor and local run: `15.h6 Bh8 16.Rh4! Bxf3 17.Rf4! Qd8 18.Bxd7!`. The rook lift creates linked threats and closes the game before a long strategic conversion.
- Paraphrased evidence: the book treats the h6 advance and successive rook moves as a concrete sequence that Black overlooked in rapid play. The resigning position is especially useful for assessing whether an explanation identifies the relation between the rook lift, the attacked bishop, and the final capture.
- Alternative / counterexample: the source recommends `14...Nc6 15.hxg6 hxg6 16.Ra3 Rf5 17.Bd2` instead of the played queen move, leaving White with a nice but non-forced advantage.
- Neutral diversity metadata: Benko-style queenside pawn offer with h-pawn pressure; opening-to-tactical finish; transient pawn imbalance; White forcing-attack perspective, Black defense perspective; short horizon.
- Unresolved ambiguity: no move ambiguity remains after source-page checking and replay. Exact date and round are not printed.

## Alternate candidates

| White - Black | Printed event/place string, year, result | PDF pages; key page | Why held in reserve |
|---|---|---:|---|
| V.Topalov - A.Giri | Candidates Tournament (Moscow), 2016, 1/2-1/2 | 17-18; 18 | Long Benko-style compensation reaches an enduring endgame, with source alternatives showing how Black could preserve queenside pressure earlier. Useful if a longer defense/transition case is needed. |
| A.Grischuk - M.Vachier Lagrave | FIDE Grand Prix (Riga), 2019, 0-1 | 18-20; 19-20 | Black’s initiative grows through `...c3!` after a structural decision by White. It is a reserve case for attack reversal and time-pressure collapse, not selected because the primary set already contains a clearer central-break game. |
| Ding Liren - M.Vachier Lagrave | Candidates Tournament (Ekaterinburg), 2021, 1/2-1/2 | 26-28; 26-27 | `11...Bxc3+!` and the intended dark-square wall give a concentrated defensive-resource case, while White’s `15.Nd4!` supplies a counter-resource. Held in reserve because the full game is very long and the five primaries already cover the required axes more compactly. |

## Diversity coverage summary

| Dimension | Coverage across primaries |
|---|---|
| Flank play | Basman-Grinberg converts h4/h5 into a bind; Rapport-Svidler turns h-pawn pressure into a forcing attack. |
| Risk of excessive advance | Williams-Poobalasingam centers on Black’s `...g5?`, which curtails Black’s own counterplay. |
| Central counterplay | Vachier Lagrave-Nepomniachtchi pivots on `...f5!` releasing the center. |
| Long compensation and defense | Rapport-Grandelius combines a material grab, an unplayed defensive resource, and a later conversion miss. |
| Phase and horizon | The set spans opening finish, closed strategic conversion, sharp initiative transfer, and a long draw. |
| Perspective | White attacking space/forcing play, Black central counterplay, and mutual defensive conversion are all represented. |

No engine judgment was performed. The book’s variations are retained only as natural-language review prompts and should not be treated as engine truth.
