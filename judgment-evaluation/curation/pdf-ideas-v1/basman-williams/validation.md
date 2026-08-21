# Validation log

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This validation uses `document_id: ref-009749ddb7ef4fe0a033b33e9712fc74`; every page below is a 1-based PDF page.

## Scope and source control

- Sole source read: document_id `ref-009749ddb7ef4fe0a033b33e9712fc74`.
- The original PDF was read only. No internet source, engine evaluation, repository file, production code, schema, test, fixture, or snapshot was changed.
- Final output contains only `candidates.md`, `primary.pgn`, and this log.

## Source-page checks

The listed 1-based source pages were checked against the final notation and review context.

| 1-based PDF pages checked | What was cross-checked |
|---:|---|
| 7-10 | M.Basman - N.Grinberg heading, full-game continuation, early `g4`/`Bh6` bind, noted active alternatives, and 1-0 result. |
| 10-12 | S.Williams - P.Poobalasingam heading, `9...g5?`, structural sequence, queenside conversion, and 1-0 result. |
| 13-15 | R.Rapport - N.Grandelius heading, material-grab sequence, the stated `20...Ne4!` resource, final draw, and the move glyphs that needed king-piece disambiguation. |
| 22-24 | M.Vachier Lagrave - I.Nepomniachtchi heading, `7.g4`, `9.e4`, Black’s reorganization and `25...f5!`, and draw result. |
| 28-29 | R.Rapport - P.Svidler heading, tactical run `15.h6` through `18.Bxd7`, and 1-0 result. |
| 17-20 | Reserve candidates Topalov-Giri and Grischuk-Vachier Lagrave, their identity lines, long-compensation/initiative themes, and results. |
| 25-27 | Reserve candidate Ding Liren-Vachier Lagrave, `11...Bxc3+!`, `15.Nd4!`, and its defensive-wall context. |

## PGN parser and legal replay

`python-chess` 1.11.2 parsed every game in `primary.pgn`. Each main line was then replayed from the normal initial position, move by move. No game is a fragment, and no `SetUp` or `FEN` tag is present or needed.

| Game | Plies replayed | Parse | Legal replay | Main-line ambiguity |
|---|---:|---|---|---|
| M.Basman - N.Grinberg | 67 | pass, 0 parser errors | pass | none |
| S.Williams - P.Poobalasingam | 83 | pass, 0 parser errors | pass | none |
| M.Vachier Lagrave - I.Nepomniachtchi | 63 | pass, 0 parser errors | pass | none |
| R.Rapport - N.Grandelius | 96 | pass, 0 parser errors | pass | none |
| R.Rapport - P.Svidler | 35 | pass, 0 parser errors | pass | none |

## Corrections made during validation

- PDF text extraction maps some chess glyphs to non-piece characters. Source-page checking established that Basman-Grinberg uses `15.Ke2`, `19...Kxc7`, and `27.Ke3`, not bishop/knight substitutes.
- The same visual check established Rapport-Grandelius `33...Ke5`, not `33...Be5`.
- These are notation-transcription corrections only. The final PGN parse and replay fully pass.

## Remaining limitations and blockers

- No legal-move blocker remains: 5/5 primary games passed parsing and replay, 0 unresolved move ambiguities.
- The PDF omits precise dates and rounds for all five primaries, and does not always distinguish a city from an event name. Those fields are marked `?` or retained as the exact printed string rather than guessed.
- Variations mentioned in `candidates.md` are book-attributed alternatives for human interpretation, not engine-verified assessments or production assertions.

