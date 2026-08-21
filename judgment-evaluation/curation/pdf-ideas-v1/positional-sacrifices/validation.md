# Validation record

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This validation uses `document_id: ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; every page below is a 1-based PDF page.

## Scope and method

- Sole evidence source: document_id `ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; no web source, engine, production code, schema, test, snapshot, or fixture was read or changed.
- PDF page numbers below are 1-based physical PDF pages. The PDF is a reflowed edition, so these intentionally do not rely on the book's printed contents-page numbering.
- Chess figurines in the PDF were visually read and transcribed to standard SAN. Only the full printed main line was put in "primary.pgn"; source variations remain prose in "candidates.md".
- PGN parsing and move replay used python-chess 1.11.2. No engine evaluation was invoked.

## Source-page check

The following 1-based source-page ranges were checked against the extracted text and the reconstructed main line:

| Primary game | Source PDF pages checked | Core pages visually checked | What was checked |
|---|---:|---:|---|
| Schulten - Morphy | 16-19 | 16, 17, 19 | identity, e3 pawn-sacrifice diagram/anchor, finish |
| Kortchnoi - Nijboer | 98-101 | 98, 100, 101 | identity, Nxc5 structural break, rook-sacrifice conversion/end |
| Mamedyarov - Anand | 315-320 | 315, 318, 320 | identity, Rd5 exchange-sacrifice diagram and alternatives, result |
| Ligterink - Nunn | 434-438 | 434, 435, 438 | identity, Ng4 queen-sacrifice diagram, transition/result |
| Bronstein - Ljubojevic | 452-456 | 452, 454, 456 | identity, Qf4/Bxg1 rook-sacrifice line and defensive resource, result |


## PGN parse and replay

| Game | Full source game? | Mainline plies replayed | Result tag | Parse/replay |
|---|---|---:|---|---|
| John William Schulten - Paul Morphy | Yes | 40 | 0-1 | PASS |
| Viktor Kortchnoi - Friso Nijboer | Yes | 65 | 1-0 | PASS |
| Shakhriyar Mamedyarov - Viswanathan Anand | Yes | 89 | 1-0 | PASS |
| Gert Ligterink - John Nunn | Yes | 74 | 0-1 | PASS |
| David Bronstein - Ljubomir Ljubojevic | Yes | 81 | 1-0 | PASS |

All five games parse as one game each and replay to the recorded result without an illegal move. No game is a fragment, so no SetUp or FEN header was used.

One provisional transcription error was caught during replay, before artifact creation: the Bronstein - Ljubojevic draft had 34...Ra5, which is illegal. The source-page check shows 34...a5 followed by 35...Ra6+, and the final PGN uses that legal source reading.

## Remaining uncertainty and blockers

- Legal-move status: no unresolved ambiguity in any primary.
- Identity metadata: the PDF lacks full calendar dates for every selected game; it uses the abbreviations "tt" and "ct" in two event names. These were preserved rather than expanded or invented.
- Analytical status: several source pages explicitly give alternative defenses or missed chances. They are recorded in "candidates.md"; this validation does not claim that the book's prose is engine truth.
- Blocking issue: none for the requested three curation artifacts.

