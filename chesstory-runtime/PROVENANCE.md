# Provenance

- Source snapshot: `Chesstories` (formerly `CondensedChess`) commit
  `1f7eef8ed5205e632cdc1cd03a1d3f389c435cf6`
- Extracted path: `lila-docker/repos/lila/modules/chessJudgmentCore`
- Snapshot date: 2026-07-12
- Consolidated path: `chesstory-runtime/`
- Consolidated snapshot: shadow commit `cdd552930049443c942547c3925472d2061ea1d5`
  plus its 18 tracked working-tree updates on 2026-07-23
- Git audit at extraction: 818 commits on the extracted path, one recorded author
- The lila-wide package exports and logger shim were deliberately not copied
- The `lila.chessjudgment` namespace was retained to make extraction parity
  mechanical; the namespace is not a license boundary

The historical public repository is AGPL-3.0. This independent repository does
not attempt to revoke any historical grant. Before external distribution, code
provenance and third-party notices must receive specialist legal review.

Direct runtime dependencies at extraction:

- scalachess 17.14.2 (MIT); its packaged opening data is generated from the
  CC0-licensed `lichess-org/chess-openings` dataset, but this Runtime does not
  import or query that API
- Play JSON 3.0.6
- MUnit 1.2.1 (test only)

The Runtime does not package the former corpus-derived opening-recognition TSV
or a theme-prior resource. Book-derived evaluation samples are kept outside the
Runtime and are not part of Runtime tests or distribution.

Runtime tests may retain factual FEN and UCI move sequences. They do not retain
book prose, annotations, book/author labels, or book-specific evaluation data.
