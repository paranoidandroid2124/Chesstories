# Validation record

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This validation uses `document_id: ref-35c04c3495434a6290c20ee4712660a6`; every page below is a 1-based PDF page.

## Source and method

- Sole source read: document_id `ref-35c04c3495434a6290c20ee4712660a6`; no internet material was consulted.
- Source handling: read-only. No internet material, engine evaluation, production-code change, schema change, test change, snapshot, or fixture was used.
- File check: the PDF has 280 1-based file pages. Page references in the curation use this file numbering.
- Text extraction was used only to locate candidates and moves. Core source pages were checked against the curation before integration.

## Source-page cross-checks

| Game identity | Core PDF page(s) checked | What was matched |
| --- | --- | --- |
| N.Grandelius - E.Hedman | 48 | The 12...Rd8 diagram, its immediate move context, and the defensive-purpose discussion matched the extracted notation. |
| Sta.Kovac - N.González Rabago | 103-104 | The 17...Qd7 pin and the follow-up pressure on the b5-knight/d5-pawn were visible across the page boundary. |
| M.Warmerdam - D.Navara | 172 | The 13...c4 anchor, its stated e4 target, and the subsequent 14.Nxc4 Qc7 15.Nd2 Nc5 sequence matched. |
| J.Refalo - J.Guevara Pijoan | 249 | The preparatory queen moves and 24...g4 pawn lever were visually confirmed with their immediate continuation. |
| S.Grishchenko - R.Murtazin | 258 | The 15...h5 anchor, its alternative-plan discussion, and the h4 transition in the main score were visually confirmed. |


## PGN parse and replay

Each entry in primary.pgn was loaded with python-chess, checked for parser errors, and replayed one legal move at a time from the standard initial position. No SetUp/FEN header or reconstructed fragment was needed.

| Game identity | Complete score | Legal plies replayed | Parse/replay result | Unresolved ambiguity |
| --- | --- | ---: | --- | --- |
| N.Grandelius - E.Hedman | Yes | 100 | PASS; no parser errors and every move legal | The 0-1 score is present, but the practical ending mechanism is not encoded by the final board. |
| Sta.Kovac - N.González Rabago | Yes | 69 | PASS; no parser errors and every move legal | The 1/2-1/2 score is source-reported; it is not a checkmate/stalemate position. |
| M.Warmerdam - D.Navara | Yes | 100 | PASS; no parser errors and every move legal | The 0-1 score is present, but the practical ending mechanism is not encoded by the final board. |
| J.Refalo - J.Guevara Pijoan | Yes | 69 | PASS; no parser errors and every move legal | The 1/2-1/2 score is source-reported; it is not a checkmate/stalemate position. |
| S.Grishchenko - R.Murtazin | Yes | 85 | PASS; no parser errors and every move legal | The 1/2-1/2 score is source-reported; it is not a checkmate/stalemate position. |

## Remaining blockers

No blocker remains for the five curated, legal main scores. The PDF does not state exact calendar dates, rounds, sites beyond the supplied event/location text, or the practical cause of the non-terminal result; those values were intentionally not invented. Engine truth was intentionally not sought or asserted.

