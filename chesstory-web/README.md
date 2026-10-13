# Standalone Chesstories workspace

See [the repository README](../README.md) for setup and use.

`npm start` serves the built UI on loopback port 8787 and launches the compiled
Java runtime directly on a private loopback port with a fresh bearer token.
The same-origin proxy keeps that token out of the browser. It accepts only the
commentary API routes, bounds request bodies to 1 MiB, and rejects foreign
origins/hosts. COOP/COEP headers enable the two-thread Stockfish worker.

The built app uses no Lila application source, Play server, database, external
analysis service, or research-run cache. It retains the existing standard-only
`public-commentary-v6` protocol, legal replay, evidence ownership checks,
10-second commentary budget and D18 ceiling. Chessground, chessops, scalachess
and the pinned Stockfish build remain library dependencies.

## Commands

```powershell
npm ci
npm run build:runtime # also rebuild after a Scala change
npm run build         # also rebuild after a UI change
npm start
```

The frontend build emits a source input manifest and rejects inputs under
`lila-docker` or a `target/` directory. Engine JS/WASM comes from the locked npm
package. The first build downloads the exact NNUE network from the Stockfish
test server, validates its full SHA-256, and caches it under ignored
`assets/engine/`. Subsequent builds and normal use need no network download.

`npm run check` checks the extracted frontend and its new host. `npm test` runs
eight focused checks covering PGN preservation, legal history, engine limits
and cancellation, the previous two purpose explanations, and the conditional
reply preview regression. This does not run the full historical Lila suite.

`tests/regression/` also retains 24 existing commentary/protocol suites and
their support module. Run one explicitly, for example:

```powershell
npm test -- regression/moveReviewProtocol
```

These migrated modules are not added to the default test run. Their imports
were checked during cleanup; that does not mean the historical failures have
been resolved. See [the preservation record](../research/standalone-cleanup-20261013/README.md)
for the retained suites, archived host-specific suites and prior failures.

## Local settings

| Environment variable | Default | Purpose |
|---|---|---|
| `CHESSTORIES_PORT` | `8787` | Local browser port |
| `CHESSTORIES_JAVA` | `java` on PATH | Java 21 executable |
| `CHESSTORIES_SOCKET_TEMP` | `%PUBLIC%/Chesstories/socket-temp` on Windows | ASCII socket directory for the JDK 21 selector-pipe issue |

Only the local loopback interface is supported. The launcher owns the runtime
process and stops it on normal `Ctrl+C` shutdown. Java starts with a 64 MiB
initial heap and a 768 MiB maximum heap; Stockfish runs in the browser with
the existing two-thread, 16 MiB hash profile.

## Current scope

- One standard-chess game at a time; up to 1 MB input and 600 total moves across branches.
- PGN comments, annotation glyphs and variations survive export. New moves add branches.
- FEN positions, click/drag movement, castling and selectable promotion pieces.
- Local browser autosave plus PGN download; autosave is not a shared database or a backup.
- English/Korean controls and the existing commentary renderer. Prose without an approved Korean rendering retains its marked English original.
- Server study accounts, synchronization and collaborative editing are outside this version.

The old Lila application tree and compatibility exports have been retired.
The original tree remains in the ignored task archive; its uncommitted source
and this app's pre-cleanup source also have a retained compressed backup.
Keep current commentary changes in `src/review/`.

Ordinary analysis and commentary now share one standard-chess UCI path.
The unused Lila engine catalog, alternative legacy protocol, icon-font adapter
and study-write controls have been removed. Evidence decoding, ownership
checks, cancellation accounting and the D18 request ceiling remain in place.
