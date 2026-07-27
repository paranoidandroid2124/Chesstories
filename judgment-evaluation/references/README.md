# Local Chess Reference Index

This directory contains a metadata-only registry for 17 read-only chess PDF references. The PDFs themselves are not copied into the evaluation harness, and no source prose, screenshots, extracted analysis, or local filesystem paths are retained.

## Locator policy

The only permissible source locator is the pair `{document_id, pdf_page}` from [source-index.json](./source-index.json). `pdf_page` is a one-based PDF page number. Basenames are retained solely to bind each opaque document ID to the byte-level checksum of the local reference.

The reconstructed diagnostic-explore locators are PDF page 85 for *The AI Revolution in Chess* and PDF page 107 for *Game Changer*. They are recorded only as locator metadata.

## Contamination boundary

`explore_contaminated: true` identifies the nine references overlapping the historical GoodNotes seed: *Re-Engineering the Chess Classics*, *Mastering Positional Sacrifices*, *The Basman Williams Attack*, *Nimzo-Indian: A Complete Opening Repertoire for Black*, *The Modernized Modern Benoni*, *The Modernized Benko Gambit*, *The AI Revolution in Chess*, *Game Changer*, and *The Silicon Road to Chess Improvement*. The remaining eight references are marked `false`.

## Integrity totals

- Documents: 17
- Bytes: 518,543,566
- PDF pages: 9,188
- Explore-contaminated: 9
- Uncontaminated: 8

Each `sha256` value hashes the complete source file bytes. `source_type` is fixed to `local-reference-only` for every entry.
