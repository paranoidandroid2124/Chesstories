# Chesstory Runtime

Private standalone runtime for deterministic chess commentary.
[JudgmentBoundary](docs/JudgmentBoundary.md) defines current meaning, ownership,
investigation policy, and budgets. [Research status](../research/README.md)
is the entry point for current results, previous failures, and remaining work.
Executable types, admission checks, and public schemas are the contract authority.

## Run

From this directory:

```text
sbt run
```

The default address is `127.0.0.1:8091`.

- `CHESSTORY_HOST` / `CHESSTORY_PORT`: address overrides.
- `CHESSTORY_TOKEN`: Bearer authentication, required outside loopback.
- `CHESSTORY_WORKERS`: concurrent CPU work, default 1–8 based on processors.

On the Windows/JDK 21.0.9 research host, an `Invalid argument: connect` failure
in `sun.nio.ch.UnixDomainSockets.connect` was resolved by setting the server/test
JVM option below to an existing ASCII-only absolute directory:

```text
-Djdk.net.unixdomain.tmpdir=C:/Codes/Chesstories/chesstory-runtime/target/socket-temp
```

## Player API

| Method | Route |
|---|---|
| POST | /v1/position-commentary-jobs |
| GET | /v1/position-commentary-jobs/{jobId} |
| POST | /v1/position-commentary-jobs/{jobId}/engine-work-reports |
| DELETE | /v1/position-commentary-jobs/{jobId} |

Requests, reports, statuses, errors, and responses use the atomic
`public-commentary-v6` cohort. Supply complete `initial_fen + move_prefix_uci`
history and exact `current_fen`. Optional unscored `input_lines` retain nested
continuations with `pgn_record`, `analysis`, or `study` provenance.
Optional `proposals` preserve separately attributed questions, candidates,
priority sources, and evidence references. They request an exact legal move
observation or a same-work root/child comparison; uninterpreted claims remain
explicit and do not become evidence.

The server owns work identity, legal replay, evidence admission, and commentary.
The browser executes only issued work and reports suffixes with `white_score`
in the existing ceval White perspective. Independently admitted observations
survive progress, completion, and interruption even without an assessment.
Single-move and chronological PGN review share the coordinator and result cache.

Jobs with at most 10 seconds remaining start directly at depth 12 and prioritize
the reviewed piece's first capture/non-capture response comparison. Longer jobs
retain the depth-8 preview and depth-16 verification. Normal commentary work is
capped at depth 18. The original deadline includes browser engine preparation,
requests, and delivery; missing evidence does not extend it.

Before accepting requests, the runtime initializes its legal-observation and
serialization path with an unscored standard opening. This performs no engine
search and creates no reusable job or case-specific answer. During short jobs,
pending work receives its required observation certificate. A completed purpose
answer publishes the board resources needed by its statements, links, and early
path positions, while keeping complete move paths, scores, and material events.
Unpublished optional position references are null, not dangling identifiers.

## Browser engine binding

The sole admitted binding is `sf18-smallnet-t2-h16-v1`. After initial observations,
the browser prepares its dedicated SF18 smallnet worker before `work:0` and reuses
it for the job. Preparation is cancellable within the issued elapsed-time limit.
Boot failure, unsupported browser, exhausted budget, cancellation, and search
failure remain distinct. Missing/unsupported profiles are rejected before use.

Profile equality is configured binding, not remote attestation. Generic ceval,
user-selected threads/hash, automatic fallback, and downgrade are outside this
preparation path. The review boundary converts legal king-to-rook castling to
king-destination UCI using the board and sets `UCI_Chess960=false`; ordinary live
analysis retains king-to-rook notation.

## Development interfaces and scope

`RuntimeProtocol.evaluate` and `RuntimePublicResponseCli` are development-only
FEN/UCI/PV interfaces. See the [evaluation adapter](../judgment-evaluation/README.md).
`GET /health` reports process liveness and health schema version, not engine or
semantic readiness.

The implemented path supports bounded question discovery, submitted legal
hypotheses and scoped comparisons, shared follow-up investigation, and finite
material/support comparisons. General quiet-plan
discovery, sufficient strategic compensation, and whole-game moment selection
remain incomplete. Read the boundary map for exact state meanings and limits.
