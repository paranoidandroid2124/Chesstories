# Validation record

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This validation uses `document_id: ref-dc427a9593ab48248d19627b245fafaa`; every page below is a 1-based PDF page.

## Scope and boundary

- Source read: only the designated PDF (`document_id: ref-dc427a9593ab48248d19627b245fafaa`); no internet material was consulted.
- Source PDF: treated as read-only. No production code, schema, test, snapshot, fixture, or existing repository file was changed.
- Curation boundary: `primary.pgn` supplies only positions and mainline moves. Four entries start from the standard initial position; Lc0 - Stockfish is tagged `[Fragment "true"]` and begins from the book's stated start position after `12...g5`, represented by a legally replayed `SetUp`/`FEN`. The surrounding prose is source-grounded natural-language context, not an engine verdict, answer key, label set, or one-to-one oracle.
- No chess-engine evaluation was run.

## Source-page check

Key 1-based source pages were checked against the extracted text before the final transcription:

| Source game | Source PDF pages checked | What was checked |
| --- | --- | --- |
| Karjakin - Duda | 205 | `14.e5!` is the book's central pawn-offer anchor. |
| Grischuk - Vachier Lagrave | 231, 236 | Early `3.h4!?`, then `18...c4!` and its d3-outpost purpose. |
| Lc0 - Stockfish | 375, 377, 378, 380 | `14.c5!`, `20.g4!`, the exact `29...Nf7`, and `54.a5!`. |
| Firouzja - Karthikeyan | 444, 448 | `9...Qxc3+!!` and `27...f5!`. |
| Duda - Vidit | 507, 508 | `32...Rf4!` as a supplied defensive alternative, `35.Kh1!`, and the pawn-race sequence. |

The source-page check resolved one extraction ambiguity: the Lc0 - Stockfish mainline is `29...Nf7`, not `29...Nf6`. The source-page check showed `Nf7`, which makes the later `32...Nc5` legal. No other unresolved transcription ambiguity remains.

## PGN parse and replay

Validation used `python-chess` to parse each game in `primary.pgn`, then replay every mainline move from the standard initial position while checking legality at every ply.

| White - Black | Result tag | Plies replayed | Parser errors | Replay result |
| --- | ---: | ---: | ---: | --- |
| S.Karjakin - J.Duda | 1-0 | 109 | 0 | pass |
| A.Grischuk - M.Vachier Lagrave | 0-1 | 94 | 0 | pass |
| Lc0 - Stockfish | 1-0 | 195 | 0 | pass (from FEN after `12...g5`) |
| A.Firouzja - M.Karthikeyan | 0-1 | 104 | 0 | pass |
| J.Duda - S.Vidit | 1-0 | 99 | 0 | pass |

Summary: 5/5 primary PGNs parse and replay legally; unresolved legal/transcription issues: 0. Four entries are complete games from the standard initial position. Lc0 - Stockfish is tagged `[Fragment "true"]` as a source-indicated fragment (moves 13-110) and therefore uses the legally reconstructed `SetUp`/`FEN` position after `12...g5`.

## Remaining blockers

None for the requested curation deliverables.

