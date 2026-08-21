# The Modernized Benko Gambit — position-line fragment stratum

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This artifact uses `document_id: ref-edf868ccf0a3425488f55ef6b4b08acf`; every locator below is this document ID plus 1-based PDF page(s). No local filesystem path or visual source asset is retained.

## Scope and handling notes

- Sole source: *The Modernized Benko Gambit* (Milos Perunovic, 2018), the designated read-only PDF. No web material or engine assessment was used.
- PDF page numbers below are 1-based PDF pages, not printed book-page numbers.
- Every primary score is a book-provided fragment, not an invented complete game. A `*` in `primary.pgn` means “fragment ends here,” not a draw. Where the PDF does not print a result, the identity record says `?`.
- All five primary fragments begin from the normal initial position. No `SetUp`/`FEN` tags were needed or added.
- This is reviewer-facing curation, not an answer key: it contains no target labels, IDs, or one-to-one oracle. The PGN positions and moves, like the prose, are limited to development curation and session semantic review; they are not runtime or test inputs.
- Complete-score check: one PDF-only pass across all 240 pages found no standard terminal result token (`1-0`, `0-1`, or `1/2-1/2`) and no suitable complete, result-known score in the selected material. No external score was consulted or completed.

## Position-line fragment stratum — eligibility boundary

The five primary records are explicitly a **position-line fragment stratum**: they are eligible only for local move/idea-continuation semantic review. They are not evidence for a game ending or conversion, are not a count of complete games, and are ineligible for any runtime/test oracle. The existing `[Fragment "true"]` and `[Result "*"]` PGN markers remain the only fragment notation; no schema or consumer is added.

## Primary selection — position-line fragment stratum

### Book analysis fragment — 5.e3 / 9.Nf3

- Identity: White `?`; Black `?`; event/year/result `?`. The PDF presents a theoretical fragment rather than a credited game score.
- PDF pages: 59-62. Core idea page: 61.
- Mainline fragment:

```pgn
1. d4 Nf6 2. c4 c5 3. d5 b5 4. cxb5 a6 5. e3 axb5 6. Bxb5 Qa5+ 7. Nc3 Bb7 8. Bd2 Qb6 9. Nf3 Nxd5 10. Nxd5 Bxd5 11. a4 e6 12. Bc3 Be7 13. O-O O-O 14. Ne5 d6 15. Nc4 Bxc4 16. Qg4 g6 17. Qxc4 *
```

- Anchor: `8...Qb6 9.Nf3 Nxd5 10.Nxd5 Bxd5`; the source uses `9.Nf3` to frame White’s deliberate return of the extra pawn while retaining positional control.
- Evidence, paraphrased: The chapter treats 5.e3 as a serious practical test and offers the sharp recapture route. In the displayed main line, the pawn is given back rather than defended indefinitely, leaving White with the more stable-looking positional assets in the author’s discussion.
- Alternative or contrary reading: The PDF also gives `9.Qb3 e6 10.e4 Nxe4` and `9.Bc4 e6 10.e4 Nxe4` as sharper ways for the position to change; the anchor is therefore a return/control choice, not the only continuation.
- Neutral diversity metadata: 5th-move alternative; early middlegame; temporary pawn plus is relinquished; central pawn exchanges; White-control viewpoint; short-to-medium horizon.
- Replay: parsed and replayed from the initial position, 33 plies, no illegal move. Final FEN and parser details are in `validation.md`.
- Unresolved ambiguity: none in the stored fragment; player/event/result are intentionally unknown rather than reconstructed.

### Dreev, A — Perunovic, M, Berlin 2015

- Identity: White Dreev, A; Black Perunovic, M; event Berlin; year 2015; result `?`.
- PDF pages: 44-46. Core idea page: 46.
- Mainline fragment:

```pgn
1. d4 Nf6 2. c4 c5 3. d5 b5 4. Qc2 bxc4 5. e4 d6 6. Bxc4 g6 7. Nf3 Bg7 8. O-O O-O 9. h3 Nbd7 10. Nc3 Rb8 11. Re1 Nh5 12. Be2 Ne5 13. Nxe5 Bxe5 14. Bxh5 gxh5 15. Bh6 Re8 *
```

- Anchor: `9...Nbd7 10.Nc3 Rb8 11.Re1 Nh5 12.Be2 Ne5 13.Nxe5 Bxe5 14.Bxh5 gxh5 15.Bh6 Re8`.
- Evidence, paraphrased: The pages first establish Black’s Qc2-side development route, then show a knight reroute and a kingside pawn capture that open a flank/f-file counterplay plan. The stated follow-up is rook and king activation on the kingside, so the fragment offers Black-side activity rather than another White-containment example.
- Alternative or contrary reading: The neighbouring `9...Nfd7` route can instead aim for ...e6 or ...f5. The indicated ...Kh8/...Rg8 continuation begins after this fragment ends, so it is a local plan, not evidence of a completed conversion.
- Neutral diversity metadata: 4th-move Qc2 branch; early-to-middle middlegame; no immediate deliberate material imbalance; h/g-pawn and f-file tension; Black flank counterplay viewpoint; short-to-medium horizon.
- Replay: parsed and replayed from the initial position, 30 plies, no illegal move. Final FEN and parser details are in `validation.md`.
- Unresolved ambiguity: the PDF prints no result or full score, so the stored record remains a fragment with result `?`.

This replaces Mamedyarov-Jones in the primary stratum because it supplies the missing Black-side flank/f-file counterplay axis. It does not claim that this route settles the opening; the source itself supplies alternative Black move orders.

### Tomashevsky, V — Perunovic, M, Yerevan 2014

- Identity: White Tomashevsky, V; Black Perunovic, M; event Yerevan; year 2014; result `?`.
- PDF pages: 180-189. Core idea page: 189.
- Mainline fragment:

```pgn
1. d4 Nf6 2. c4 c5 3. d5 b5 4. cxb5 a6 5. bxa6 g6 6. Nc3 Bg7 7. Nf3 O-O 8. g3 d6 9. Bg2 Nbd7 10. O-O Nb6 11. Re1 Bxa6 12. Rb1 Bc4 13. e4 Bxa2 14. Nxa2 Rxa2 15. b4 c4 16. Nd4 Ng4 17. Rf1 Na4 *
```

- Anchor: `12...Bc4 13.e4 Bxa2 14.Nxa2 Rxa2 15.b4 c4 16.Nd4 Ng4 17.Rf1 Na4`.
- Evidence, paraphrased: The bishop placement attacks two queenside targets and the c-pawn advance fixes White’s queenside play. The author says that, despite winning the cited game after `16...Ng4`, White had the stronger defensive reply `17.Re2`; this makes the fragment useful for separating a played continuation from a defensive resource.
- Alternative or contrary reading: `17.Re2` is the explicit source-side improvement over the stored `17.Rf1` game branch. The source also gives `16...Na4` as another way to continue the structural pressure.
- Neutral diversity metadata: main-line fianchetto; early-to-middle middlegame; Black has recovered queenside material; c4 pawn restricts space; Black restriction versus White defence; medium horizon.
- Replay: parsed and replayed from the initial position, 34 plies, no illegal move. Final FEN and parser details are in `validation.md`.
- Unresolved ambiguity: the author’s prose says he won, but the PDF does not print a final score; result remains `?`.

### Book analysis fragment — 5.Nc3 / 11...Nxe4

- Identity: White `?`; Black `?`; event/year/result `?`. This is an explicitly analysed book line, not a credited historical score.
- PDF pages: 130-137. Core idea page: 135; long-horizon material outcome on page 137.
- Mainline fragment:

```pgn
1. d4 Nf6 2. c4 c5 3. d5 b5 4. cxb5 a6 5. Nc3 axb5 6. e4 b4 7. Nb5 d6 8. Bc4 Nbd7 9. Nf3 Nb6 10. Bd3 g6 11. b3 Nxe4 12. Bxe4 Bg7 13. Bd2 Ba6 14. Bd3 Bxa1 15. Qxa1 f6 16. Qd1 Qd7 17. Qe2 O-O 18. h4 Kh8 19. h5 g5 20. O-O Nxd5 21. a4 bxa3 22. Nxa3 Bxd3 23. Qxd3 Nf4 24. Bxf4 gxf4 *
```

- Anchor: `10...g6 11.b3 Nxe4 12.Bxe4 Bg7 13.Bd2 Ba6 14.Bd3 Bxa1`.
- Evidence, paraphrased: The source introduces the knight capture as a sacrifice-based answer to the awkward knight on b5. Its continuation converts the initial material concession into a rook-and-pawn versus two-knights imbalance with central and file-based activity, rather than a short forced finish.
- Alternative or contrary reading: The book also gives the quieter `11...Bg7`; and after the sacrifice, White can decline the immediate capture with `12.Bb2`. The compensation story is therefore contingent on both sides’ choices.
- Neutral diversity metadata: 5th-move Nc3 branch; early middlegame entering a long material imbalance; temporary minor-piece sacrifice followed by rook capture; queenside and kingside pawn tension; Black compensation viewpoint; long horizon.
- Replay: parsed and replayed from the initial position, 48 plies, no illegal move. Final FEN and parser details are in `validation.md`.
- Unresolved ambiguity: none in the stored line; game identity/result are not supplied by the source.

### Gelfand, B — Carlsen, M, Zurich 2014

- Identity: White Gelfand, B; Black Carlsen, M; event Zurich; year 2014; result `?`.
- PDF pages: 194-196. Core idea pages: 195-196.
- Mainline fragment:

```pgn
1. d4 Nf6 2. c4 c5 3. d5 b5 4. cxb5 a6 5. bxa6 g6 6. Nc3 Bg7 7. Nf3 O-O 8. e4 Qa5 9. Bd3 Nxd5 10. exd5 Bxc3+ 11. bxc3 Qxc3+ 12. Qd2 Qxa1 13. O-O Bxa6 14. Bb2 Qxa2 15. Ra1 Qb3 *
```

- Anchor: `8...Qa5 9.Bd3 Nxd5 10.exd5 Bxc3+ 11.bxc3 Qxc3+ 12.Qd2 Qxa1`.
- Evidence, paraphrased: The source presents Bd3 as allowing a direct tactical sequence: a central capture opens the bishop/queen route, and the queen reaches the a1 rook. It is selected for the compact, fully printed forcing continuation, not because of the players’ prominence.
- Alternative or contrary reading: The source treats `9.e5` as a separate critical try, while the branch after `10...Bxc3+` offers other White recaptures. The stored fragment tests one concrete error path rather than claiming universal force.
- Neutral diversity metadata: 9.Nd2 chapter’s preceding alternative; early middlegame; rapid minor-piece/rook material transformation; open central and queenside lines; Black tactical-forcing viewpoint; short horizon.
- Replay: parsed and replayed from the initial position, 30 plies, no illegal move. Final FEN and parser details are in `validation.md`.
- Unresolved ambiguity: the PDF gives no complete score or final result; result remains `?`.

## Reserve candidates (not primary)

| Reference | PDF pages / core | Why it remains useful | Source-grounded reserve line | Replay status |
| --- | --- | --- | --- | --- |
| Mamedyarov, S — Jones, G, Bastia 2010 | 43-45 / 45 | White’s Na3/Rb1/b3 containment setup is a clear contrast case, but it overlaps the other White defensive material more than the Dreev fragment does. | `1.d4 Nf6 2.c4 c5 3.d5 b5 4.Qc2 bxc4 5.e4 d6 6.Bxc4 g6 7.Nf3 Bg7 8.O-O O-O 9.h3 Ba6 10.Na3 Nfd7 11.Rb1 Nb6 12.b3 Bxc4 13.Nxc4 Nxc4 14.bxc4 *` | 27 plies, legal |
| Silman, J — Christiansen, L, Los Angeles 1989 | 125-128 / 127-128 | A queen-for-rook-and-pawn imbalance with a documented defensive move later in the line; clear enough to reserve, but it overlaps the primary tactical-sacrifice axis. | `1.d4 Nf6 2.c4 c5 3.d5 b5 4.cxb5 a6 5.Nc3 axb5 6.e4 b4 7.Nb5 d6 8.Bf4 g5 9.Bxg5 Nxe4 10.Bf4 Qa5 11.Bc4 Bg7 12.Qe2 b3+ 13.Kf1 f5 14.f3 O-O 15.fxe4 fxe4 16.g3 Qxa2 17.Rxa2 bxa2 18.Bxa2 Rxa2 19.Nc7 Bf5 20.Ne6 Rxb2 21.Nxf8 Rxe2 22.Nxe2 Kxf8 *` | 44 plies, legal |
| L'Ami, E — Perunovic, M, Reykjavik 2015 | 173-176 / 174 | A White restriction setup around Bc6/d5/e8 versus Black’s move-order choice. It is a useful counterexample to automatic queenside pressure, but overlaps the primary White-containment material. | `1.d4 Nf6 2.c4 c5 3.d5 b5 4.cxb5 a6 5.bxa6 g6 6.Nc3 Bg7 7.e4 O-O 8.a7 Rxa7 9.Nf3 e6 10.Be2 exd5 11.exd5 d6 12.O-O Na6 13.Bb5 Qb6 14.a4 Bb7 15.Nd2 *` | 29 plies, legal |

## Diversity summary

The five primary fragments are intentionally spread over a pawn-return line, Black-side flank/f-file counterplay, a fianchetto-side structural clamp plus defensive correction, a long-horizon minor-piece sacrifice, and a compact tactical sequence. Their openings range from 4.Qc2 through 5.e3/5.Nc3 to accepted-main-line and 9.Nd2 structures; the viewpoints alternate between White restraint/defence and Black compensation/forcing play. The reserves retain the White-containment contrast, a queen-sacrifice material shape, and another White restriction example without making fame a selection criterion.
