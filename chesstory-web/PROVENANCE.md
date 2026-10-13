# Source provenance

The commentary frontend was moved on 2026-10-13 from
`lila-docker/repos/lila/ui/` in the Chesstories working tree on
`codex/functional-choice-value` (HEAD `c73eb64117ae2fde3bde9267c935e092b6672d9b`).
The move includes the user's uncommitted deduplication and commentary changes;
it is not a fresh copy of that Git commit. The before-file inventory and hashes
are in `target/research-runs/lila-exit-20261013/`, with the retained audit in
`research/lila-exit-20261013.json`.

The six `moveReview*` modules, commentary view/styles, UCI protocol/profile,
board preview, VNode helpers and icons now have one canonical implementation
here. Chess boundary helpers and
engine work types were extracted into shared modules rather than copied into
the new host. The existing reference-purpose regression test was moved with
its original assertions.

Later on the same date, the legacy application tree and temporary compatibility
exports were retired. The standalone host now uses a single standard-chess UCI
path; unused legacy engine catalog, study controls and glyph adapters were removed.
The original source and uncommitted changes remain recoverable as described in
[the cleanup record](../research/standalone-cleanup-20261013/README.md).
The retained `lila.chessjudgment` runtime namespace still denotes extracted
commentary code, not a dependency on the old application.

The repository's [AGPL-3.0 license](../LICENSE) continues to apply to its source.
The standalone runtime has its own [provenance record](../chesstory-runtime/PROVENANCE.md).
Removing the Lila application dependency does not remove library provenance.

## Pinned runtime libraries

| Package | Version | Package license | Source |
|---|---|---|---|
| `@lichess-org/chessground` | 9.9.0 | GPL-3.0-or-later | https://github.com/lichess-org/chessground |
| `chessops` | 0.15.0 | GPL-3.0-or-later | https://github.com/niklasf/chessops |
| `@lichess-org/stockfish-web` | 0.4.2 | AGPL-3.0-or-later | https://github.com/lichess-org/stockfish-web |
| `snabbdom` | 3.5.1 | MIT | https://github.com/snabbdom/snabbdom |

Board pieces come from Chessground's bundled `chessground.cburnett.css`.
The build preserves dependency license files in `dist/licenses/`.

The Stockfish network is `nn-4ca89e4b3abf.nnue`, fetched from
https://tests.stockfishchess.org/api/nn/nn-4ca89e4b3abf.nnue.
Its pinned SHA-256 is
`4ca89e4b3abfbe9df13e4f3db2acb64dc6ddc7a9becb2ac1cf388f4d66b3bd94`.
Stockfish-web's README identifies the upstream build and network sources.
Its JS/WASM, README and license are obtained from the locked package, not a
previous research execution.
