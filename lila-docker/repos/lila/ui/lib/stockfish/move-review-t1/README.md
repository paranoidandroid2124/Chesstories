# Move Review single-thread Stockfish

This directory carries the exact non-pthread fallback for the
`sf18-smallnet-single-t1-h16-v1` Move Review profile. It is not the public
`lite-single` build.

The source is pinned to:

- official Stockfish commit `cb3d4ee9b47d0c5aae855b12379378ea1439675c`;
- the `sscg13/threat-small` changes used by `@lichess-org/stockfish-web@0.2.3`;
- `nn-4ca89e4b3abf.nnue` (full SHA-256
  `4ca89e4b3abfbe9df13e4f3db2acb64dc6ddc7a9becb2ac1cf388f4d66b3bd94`);
- the non-pthread/Asyncify web port from `nmrugg/stockfish.js` commit
  `31a98753a5d932511693f44775da908377c24513` (`v18.0.0`);
- Emscripten `3.1.7`, the version required by that port.

`sf18-smallnet-single.patch` is the complete diff from the pinned official
Stockfish commit to the build source. `build.sh` fetches both pinned trees,
validates the NNUE, builds the two files in `assets`, and runs `verify.mjs`.
Run it from an environment where Emscripten 3.1.7 has been activated:

```sh
bash ./build.sh
```

The verifier checks the committed artifact hashes, confirms that the WASM
memory is not shared, observes `uciok` and `readyok`, and completes depth 16
searches for both Standard and Chess960 positions.

Stockfish and the Stockfish.js port are distributed under GPL-3.0-or-later;
the exact upstream source, copyright notices, and `Copying.txt` are fetched by
the reproducible build above.
