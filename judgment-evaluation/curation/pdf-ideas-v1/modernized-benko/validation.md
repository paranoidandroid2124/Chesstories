# Validation record

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This validation uses `document_id: ref-edf868ccf0a3425488f55ef6b4b08acf`; every page below is a 1-based PDF page.

## Method and limits

- Read only the designated PDF. The original PDF was not altered, and no internet source or engine evaluation was used.
- Used text extraction to locate candidate fragments, then checked the source pages before curation.
- `python-chess` parsed every stored primary PGN and replayed every main-line move from the normal initial position. No fragment needs a reconstructed FEN.
- A PGN result of `*` is a fragment terminator. It does not assert the game’s historical result.

## Position-line fragment stratum boundary

All five primary entries are a **position-line fragment stratum**, not complete-game records. They are eligible only for local move/idea-continuation semantic review; they are ineligible as game-end or conversion evidence, as a complete-game count, or as a runtime/test oracle. The pre-existing `[Fragment "true"]` and `[Result "*"]` tags remain unchanged, and no schema or consumer was added.

## Complete-score existence recheck

One PDF-only recheck covered all 240 PDF pages. No extractable standard terminal result token (`1-0`, `0-1`, or `1/2-1/2`) was found, and the source pages used here present analysed lines or cited fragments rather than a complete, result-known score. Therefore no suitable complete score was confirmed inside the PDF; no external score was sought or invented.

## Source-page comparison

| 1-based PDF pages checked | What was visually checked |
| --- | --- |
| 4-5 | Contents and stated repertoire framing. |
| 44-46 | Qc2 setup, the Dreev-Perunovic route through ...Nbd7/...Nh5, flank/f-file plan, and source credit. |
| 61-62 | 5.e3 pawn-return framing, anchor, and continuation. |
| 135-137 | 11...Nxe4 material transition, alternatives, and final imbalance description. |
| 189 | Tomashevsky-Perunovic game branch and the stated 17.Re2 defensive alternative. |
| 195-196 | Gelfand-Carlsen forcing sequence and source credit. |
| 127-128 | Silman-Christiansen reserve line and queen-sacrifice/defence context. |
| 174 | L'Ami-Perunovic reserve line and move-order counterexample. |

## Primary PGN parse and replay

`python-chess` parser/replay result: five of five stored primary fragments parsed without errors and completed every listed ply legally.

| Fragment | Plies | Final FEN after stored fragment | Status | Unresolved point |
| --- | ---: | --- | --- | --- |
| Book analysis, 5.e3 / 9.Nf3 | 33 | `rn3rk1/4bp1p/1q1pp1p1/1Bp5/P1Q5/2B1P3/1P3PPP/R4RK1 b - - 0 17` | pass | No game credit/result printed. |
| Dreev-Perunovic, Berlin 2015 | 30 | `1rbqr1k1/p3pp1p/3p3B/2pPb2p/4P3/2N4P/PPQ2PP1/R3R1K1 w - - 2 16` | pass | Result/full score absent. |
| Tomashevsky-Perunovic, Yerevan 2014 | 34 | `3q1rk1/4ppbp/3p2p1/3P4/nPpNP1n1/6P1/r4PBP/1RBQ1RK1 w - - 4 18` | pass | Result/full score absent; source-side 17.Re2 is an alternative, not stored main line. |
| Book analysis, 5.Nc3 / 11...Nxe4 | 48 | `r4r1k/3qp2p/3p1p2/2p4P/5p2/NP1Q1N2/5PP1/5RK1 w - - 0 25` | pass | No game credit/result printed. |
| Gelfand-Carlsen, Zurich 2014 | 30 | `rn3rk1/3ppp1p/b5p1/2pP4/8/1q1B1N2/1B1Q1PPP/R5K1 w - - 2 16` | pass | Result/full score absent. |

## Reserve line spot checks

The three reserve move strings in `candidates.md` were also parsed and replayed from the normal initial position: Mamedyarov-Jones (27 plies), Silman-Christiansen (44 plies), and L'Ami-Perunovic (29 plies). All three were legal.

## Source-level ambiguity held out of the selection

Pantelic, S — Nestorovic, N, Novi Sad 2014 (PDF pages 202-203) is a high-interest tactical reserve but was not stored as a primary or reserve PGN. Page 202 prints `15.Ra3`, while page 203 prints `17...Qxa1` and its diagram after `15...Ne5` still shows White’s rook on a1. Those three source signals cannot all describe one legal position. A text-first repair would make `17...Qxa3` legal; the diagram/tactic-first reading supports `Qxa1` but leaves no legal displayed 15th White move. No repair was silently chosen.

## Validation summary

- Primary legality: 5 passed, 0 unresolved move sequences.
- Reserve legality: 3 passed.
- Remaining source issue: 1 excluded, internally inconsistent Pantelic-Nestorovic notation. It does not block use of the five primary fragments.

