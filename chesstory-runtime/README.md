# Chesstory Runtime

Private standalone runtime for Chesstory's causal graph judgment.

Player use is the position-commentary job path: the server issues exact
browser engine work and the browser returns only the issued work report.
Direct FEN/UCI/PV/probe inputs are development-only. The runtime must not
depend on the lila application, controllers, database, or internal graph
transport types.

## Run

```text
sbt run
```

The process listens on `127.0.0.1:8091` by default. `CHESSTORY_HOST` and
`CHESSTORY_PORT` override the address. Set `CHESSTORY_TOKEN` to require a
Bearer token; a token is mandatory when binding outside loopback.
`CHESSTORY_WORKERS` bounds concurrent CPU work (default: 1–8 based on available processors).

## Player API

Player use is the position-commentary job path only:

- `POST /v1/position-commentary-jobs`
- `GET /v1/position-commentary-jobs/{jobId}`
- `POST /v1/position-commentary-jobs/{jobId}/engine-work-reports`
- `DELETE /v1/position-commentary-jobs/{jobId}`

The server creates one root-candidate search, an optional played-vs-best
comparison, and only the causal searches requested by the v6 semantic graph.
It owns work identity, legal replay, evidence admission, and the completed
commentary response. The browser only executes issued engine work and returns
its line suffixes. Active and stopped job responses use the atomic
`public-commentary-v6` cohort (`request`, `report`, `error`, `status`,
`response`). A request supplies the complete
`initial_fen` + `move_prefix_uci` history and its exact `current_fen`; the
server legally replays and binds that history before issuing work. Browser
`white_score` values use the White perspective of the existing ceval contract.

The sole server-admitted binding is `sf18-smallnet-t2-h16-v1`: before a job
POST, the browser preflights that exact dedicated SF18 smallnet worker. Generic
or user-selected ceval, threads/hash selection, downgrade, retry, and fallback
are not player-use. Issued work, reports, status, and responses carry required
profile equality; missing or unsupported profiles are rejected before semantic
use. This equality proves configured contract binding, not remote attestation.

`GET /health` is a process-liveness check. It returns the health schema version
and does not certify semantic resources, engine availability, or player-job
readiness.

The runtime meaning and authority boundary is documented in
[`docs/JudgmentBoundary.md`](docs/JudgmentBoundary.md). That document is a
boundary map; executable types, admission checks, and public schemas remain the
contract authority.

## Development-only direct API

`RuntimeProtocol.evaluate` and `RuntimePublicResponseCli` remain development
interfaces for the v6 direct request/response contract. They are not a player
HTTP API.
