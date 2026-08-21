# The AI Revolution in Chess - representative-game curation

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This artifact uses `document_id: ref-dc427a9593ab48248d19627b245fafaa`; every locator below is this document ID plus 1-based PDF page(s). No local filesystem path or visual source asset is retained.

Source scope: only `Doknjas, Joshua - The AI Revolution in Chess` (the designated PDF, `document_id: ref-dc427a9593ab48248d19627b245fafaa`). PDF page numbers below are physical, 1-based PDF pages. This is a reading/curation record: the short evidence statements paraphrase the book and do not turn its analysis into engine truth, an answer key, labels, or an oracle.

`primary.pgn` contains only positions and mainlines for the five primary selections. Four are complete games from the standard initial position; Lc0 - Stockfish is explicitly marked with `[Fragment "true"]` because the book says that game began after `12...g5`. Its `SetUp`/`FEN` was legally replayed from the printed prelude. The PGN contains no comments, evaluations, or variations.

## Primary selections

### S. Karjakin - J. Duda, Lindores Abbey Preliminaries 2020 (rapid), 1-0

- PDF range and key pages: 203-209; central idea on 205 (`14.e5!`), with the defensive comparison on 209.
- Mainline: complete from the initial position; exact movetext is in `primary.pgn` under `[White "S.Karjakin"]` / `[Black "J.Duda"]` (55 moves, no `SetUp`/`FEN`).
- Idea anchor: `13...Be7 14.e5! dxe5 15.Bxf6 Bxf6 16.Bh5+ g6 17.Ne4 O-O`. The follow-through is `18...Bg7 19.Rxf8+ Qxf8 20.Bf3`, when White accepts the material deficit for activity.
- Paraphrased evidence: The book presents `14.e5` as a modern, strategically motivated pawn offer in a line otherwise known for forcing calculation. Its stated compensation is active pieces and Black bishops that struggle to join the game, rather than a quick forcing win.
- Alternative / counter-reading: `20...a5!` is given as a sturdier defence, stopping `Qa5` and enabling the light-squared bishop to develop. This keeps the selection from implying that the sacrifice works automatically; Black's passive `20...Ra7?` is part of the game-specific story.
- Neutral diversity metadata: Poisoned Pawn Najdorf; opening into active middlegame; two-pawn material deficit after the offer; central pawn break; White initiative versus Black's defence; medium/long idea horizon.
- PGN replay: parsed and replayed legally (109 plies); no unresolved move ambiguity.

### A. Grischuk - M. Vachier Lagrave, Riga FIDE Grand Prix 2019, 0-1

- PDF range and key pages: 231-237; opening anchor on 231 (`3.h4!?`), structural conversion on 236 (`18...c4!`).
- Mainline: complete from the initial position; exact movetext is in `primary.pgn` under `[White "A.Grischuk"]` / `[Black "M.Vachier Lagrave"]` (47 moves, no `SetUp`/`FEN`).
- Idea anchor: `1.d4 Nf6 2.c4 g6 3.h4!? c5 4.d5 b5 5.cxb5 a6`; the key continuation is `17...Ne5 18.Rf1?! c4! 19.b3 Rfc8 20.Bd2 Nbd3`.
- Paraphrased evidence: The book frames the early h-pawn move as an Anti-Grunfeld attempt, while this game also shows Black obtaining Benko-like initiative. The `...c4` advance fixes the d3 outpost and makes the sacrificed-pawn compensation a concrete piece-activity story.
- Alternative / counter-reading: The notes offer `10.e4` as a possible improvement over `10.Ra3`, and `18.Na2` as a way to contest the knight structure instead of allowing the same `...c4` mechanism. The game can therefore probe whether a reviewer distinguishes the system's intent from this particular implementation.
- Neutral diversity metadata: Anti-Grunfeld/Benko-type opening; flank pawn advance; early queenside material imbalance; structural restriction and outposts; Black initiative; opening-to-middlegame horizon.
- PGN replay: parsed and replayed legally (94 plies); no unresolved move ambiguity.

### Lc0 - Stockfish, TCEC 2020, 1-0

- PDF range and key pages: 374-380; pawn offer on 375 (`14.c5!`), positional lock on 377 (`20.g4!`), eventual break on 380 (`54.a5!`).
- Mainline: `[Fragment "true"]` source slice starting after `12...g5`; exact movetext is in `primary.pgn` under `[White "Lc0"]` / `[Black "Stockfish"]` (moves 13-110). The source page explicitly says the game started there, so the printed 1-12 prelude is not presented as invented game history; `SetUp`/`FEN` encodes that legally reconstructed position.
- Idea anchor: `13...Ng6 14.c5! Nxc5 15.b4 Na6`; the longer payoff is `19...Bb7? 20.g4! h4` and, after patient regrouping, `53...Kh8 54.a5! bxa5`.
- Paraphrased evidence: The book describes `c5` as a pawn sacrifice that opens the c-file and ties a knight to passive queenside defence. Once Black's key bishop is misplaced, White can close the kingside, improve slowly, and finally open the opposite wing with `a5`.
- Alternative / counter-reading: A more active `19...Bf6` plan is supplied to keep the bishop and queen coordinated; the text also treats `20...hxg4` or `20...fxg3` as tougher practical tries than the game. This is useful for assessing whether a system can name a long-term plan without erasing defensive choices.
- Neutral diversity metadata: King’s Indian pawn structure; closed middlegame into a very long ending; temporary pawn sacrifice; structural constraint and delayed breakthrough; White pressure versus Black resource selection; long idea horizon; engine-versus-engine source.
- PGN replay: parsed and replayed legally (195 plies). A text-extraction `Nf6`/`Nf7` ambiguity at Black's 29th move was resolved by a source-page check as `29...Nf7`; no unresolved ambiguity remains.

### A. Firouzja - M. Karthikeyan, Asian Continental 2019, 0-1

- PDF range and key pages: 443-449; queen offer on 444 (`9...Qxc3+!!`), second-front conversion on 448 (`27...f5!`).
- Mainline: complete from the initial position; exact movetext is in `primary.pgn` under `[White "A.Firouzja"]` / `[Black "M.Karthikeyan"]` (52 moves, no `SetUp`/`FEN`).
- Idea anchor: `8.Nd2? cxd4 9.Nb3 Qxc3+!! 10.bxc3 dxe3 11.f3?! Nh5`; a later conversion sequence is `26.Rhb1 Bc6 27.Bg2 f5! 28.gxf6 Bxf6 29.Rf1 Bxc3`.
- Paraphrased evidence: The book treats the queen sacrifice as a strategic material imbalance: two minor pieces, a pawn, dark-square control, and a pawn structure in which White's heavy pieces lack open lines. It then uses `...f5` to show Black extending pressure to a second front before simplifying.
- Alternative / counter-reading: The book gives the more conventional `9...Qe5` as a viable material-preserving approach, and discusses `11.Bd3` as a more testing White setup. This separates the striking sacrifice from the claim that it is the only playable practical choice.
- Neutral diversity metadata: Indian-style central structure; queen-for-minor-pieces imbalance; Black long-term compensation; dark-square/outpost control; proactive attack followed by simplifying transition; middlegame-to-ending horizon.
- PGN replay: parsed and replayed legally (104 plies); no unresolved move ambiguity.

### J. Duda - S. Vidit, FIDE World Cup 2021, 1-0

- PDF range and key pages: 503-508; defensive fork on 507 (`32...Rf4!` versus the game), king-placement and pawn race on 508 (`35.Kh1!`).
- Mainline: complete from the initial position; exact movetext is in `primary.pgn` under `[White "J.Duda"]` / `[Black "S.Vidit"]` (50 moves, no `SetUp`/`FEN`).
- Idea anchor: `31.Nf2 Rf8+ 32.Kg2 Re8? 33.Nd3 g5 34.Nb4 Re2+ 35.Kh1!`; the transition is `36.Ra1 c5 37.Nxa6 b4 38.Nxb4 cxb4 39.a6`.
- Paraphrased evidence: The book uses the ending to contrast an apparently defensible material balance with the practical need for active counterplay. `Kh1` removes checking motifs, and the subsequent passed-pawn race turns coordination and move order into the decisive issue.
- Alternative / counter-reading: `32...Rf4! 33.Ra1 Rc4 34.Nd3 c5` is the supplied active defensive plan; the notes also consider an immediate queenside liquidation with `34...c5`. This gives a concrete defence resource rather than treating the played loss as inevitable.
- Neutral diversity metadata: Ruy Lopez opening resolving into knight-versus-three-pawns play; defence/resource selection; king-safety transition; passed-pawn race and finish; Black defensive perspective versus White conversion; endgame-focused horizon.
- PGN replay: parsed and replayed legally (99 plies); no unresolved move ambiguity.

## Reserve candidates (not included in `primary.pgn`)

| Game identity | PDF pages / core page | Why keep as reserve | Distinctive uncertainty or alternative |
| --- | --- | --- | --- |
| S. Martinovic - M. Carlsen, FIDE World Cup 2021, 0-1 | 88-94; core 88 (`5...c5!?`) | Chapter One’s fresh Grunfeld pawn sacrifice: Black keeps development and initiative rather than recapturing conventionally. It adds a Black-side opening-sacrifice option from a chapter not used by the primary set. | The book gives `9.a3` as the critical challenge; the played `9.Nf3?!` helps Black retain pressure. |
| F. Berkes - D. Forcen Esteban, Spanish League Honour Division 2020, 1-0 | 305-310; core 306 (`10.e4!`) and 308 (`16.h4!`) | Chapter Five’s French-structure pawn sacrifice makes the gradual kingside build-up especially clear, including the choice of whether Black takes the h-pawn. It is a useful replacement if a human game is preferred over the engine-versus-engine closed-position example. | The book points to `15...a6!` and later defensive piece exchanges as more resilient than the game. |
| A. Giri - I. Nepomniachtchi, FIDE Candidates 2020-21, 0-1 | 483-489; core 486 (`19.Kf1!`) | A sharp exchange sacrifice with a forcing opening route, bishop-pair compensation, and a later defensive/fortress question. It is a reserve for an explicitly tactical line rather than another pawn-structure case. | `19...bxc4` and `19...Rxc4` are treated as different Black choices; the game’s `...b4` changes the practical problem. |

## Diversity check

The five primary selections come from Chapters 3, 4, 6, 7, and 8 - one per chapter - and the reserves add Chapters 1 and 5. The primary set intentionally spreads the main ideas across a Najdorf central pawn offer, an early h-pawn system, a closed-structure c-file/queenside plan, a queen sacrifice for long-term coordination, and an active-defence/pawn-race ending.

Opening family, phase, material relation, and viewpoint also vary: four human games plus one engine game; White-led initiative and Black-led compensation; opening novelty, strategic middlegame, and conversion-heavy endgame; pawn, exchange-like, and queen-scale material imbalances. No selection is included for fame alone: each has a complete source mainline, a clear book-grounded idea, and at least one source-supplied alternative or resource.
