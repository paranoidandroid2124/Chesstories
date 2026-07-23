# Chesstory Runtime

Private standalone runtime for Chesstory's causal graph judgment.

The runtime accepts a narrow FEN/UCI/PV/probe contract and returns only the
public explanation payload. It must not depend on the lila application,
controllers, database, or internal graph transport types.

The existing `lila.chessjudgment` package name is retained during extraction
to keep the first parity diff mechanical. Package renaming is not a license
boundary and will be considered only after standalone parity is proven.

This local repository has no remote by design.

## Run

```text
sbt run
```

The process listens on `127.0.0.1:8091` by default. `CHESSTORY_HOST` and
`CHESSTORY_PORT` override the address. Set `CHESSTORY_TOKEN` to require a
Bearer token; a token is mandatory when binding outside loopback.
`CHESSTORY_WORKERS` bounds concurrent CPU work (default: 1–8 based on available processors).

`POST /v1/move-meaning` accepts the versioned
`chesstory.move-meaning.request.v1` envelope. `GET /health` verifies that the
canonical scalachess opening database and Chesstory's theme-prior resource are
available before the server starts.
