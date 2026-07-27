# Chesstory runtime native observation adapter

This external JSONL adapter calls `RuntimeProtocol.evaluateWithBoundary` with
the identity intervention and captures the runtime's native Q → F → C → Jp →
Ja → R → P boundary values. It does not acquire engine evidence, implement
chess judgment, or reproduce any production decision rule.

Each UTF-8 input line is a `chesstory.move-meaning.request.v1` envelope. Each
output line is a `chesstory.runtime-observation.v2` document containing the
exact public response and a deterministic structural snapshot for every native
boundary that was reached. Invalid and rejected requests retain the public
error response while marking Q through P as unavailable, rather than inventing
artifacts for boundaries that did not execute. V remains a typed, explicit
unavailability: the runtime exposes structured projection but has no
verbalization stage.

## Native v2 is not semantic v1

The native runtime ADTs are not losslessly equivalent to the frozen semantic
stage schemas under `schemas/v1`. For example, native Ja has
Certified/Deferred/Rejected decisions while semantic v1 has an
admitted/rejected shape, and native Jp does not carry the v1 assertion and
numeric-confidence contract. Every stage therefore reports
`v1_semantic_mapping_status: unavailable` with a stage-specific reason. A
consumer must not validate or interpret the native tree as a v1 stage payload.
The distinct observation schema version makes strict v1 consumers fail closed;
tolerant consumers can continue reading the unchanged `request_sha256`,
`runtime`, `public_response`, and `stages` top-level locations after explicitly
accepting v2.

## Structural snapshot contract

The adapter's generic encoder uses only public `Product.productElementNames`
and `Product.productIterator` traversal plus explicit handling for
Option/Either, Scala maps, sets, ordered collections, arrays, primitive numeric
types, Java enums, and Play JSON values. Product runtime type, constructor,
field index/name/order, collection implementation kind, and exact numeric type
and representation are retained. Sequence and array order is preserved. Only
maps, sets, Play JSON object fields, and JSON object keys used for hashing are
sorted, using their encoded canonical JSON.

Three native immutable value classes do not implement `Product`:
`TypedEvidenceGraph`, `RelationFactEvidence`, and
`EvidenceBackedJudgmentPacket`. Narrow external structural adapters expose only
their public constructor state under each original native type and constructor
name (`records`; `detail`/`lineMoves`; and
`assembly`/`probeRequests`, respectively). Derived lookup and policy methods
are not serialized or recomputed, and no judgment rule is copied.

Unsupported native types and cycles fail closed; they are never converted with
`toString`. The encoder does not use Java serialization or reflective field
access. Repeated references to the same immutable ADT value are encoded at each
value position without identity markers. Object-identity sharing is deliberately
outside this value-snapshot contract; a true recursion-path cycle is rejected.

Each artifact contains its upstream artifact SHA-256, and each stage record
contains the SHA-256 of the complete artifact body. Hash input is UTF-8 JSON
with object keys sorted lexicographically and array order retained, identified
as `chesstory.sorted-object-keys-json-sha256.v1`. The public response has a
separate hash. Schemas are in:

- `schemas/native-v2/native-tree.schema.json`
- `schemas/native-v2/runtime-observation.schema.json`
- `schemas/native-v2/native-engineering-invocation.schema.json`
- `schemas/native-v2/native-engineering-result.schema.json`
- `schemas/native-v2/native-engineering-provider-io.schema.json`
- `schemas/native-v2/native-engineering-provider-failure.schema.json`
- `schemas/native-v2/native-engineering-report.schema.json`

`NativeInterventionAdapterCli` accepts the native engineering invocation
envelope and echoes the exact binding and invocation hash into its result. The
external Python runner separately captures the invocation, raw provider I/O,
and validated result. Command arguments, working directory, and executable are
represented in captured artifacts only by SHA-256 bindings; local paths are not
part of the machine-readable evidence.

Persistent command providers use the v2 provider binding, which requires the
same normalized millisecond timeout used by the invocation deadline. A timeout
captures a separate immutable, path-opaque failure document before the runner
rethrows the failure. Legacy v1 command bindings remain readable only for
historical artifacts produced before this timeout contract existed.

## Compact cause-audit observation

`CauseAuditAdapterCli` is a smaller, external read-only view for broad cause
audits. It invokes `RuntimeProtocol.evaluateWithBoundary` once with the identity
intervention, then follows the native C evidence records into the Jp, Ja, R,
packet, and public P values. It does not reproduce a production cause or policy
rule.

Every `RelativeCauseFact` is emitted with its candidate comparison and root
moves, source side, attribution, graph-derived role/event line/importance,
three proof sections, production proof-kind labels, and SHA-256 hashes of the
complete native source records. Object bindings and targets come directly from
the public `EvidenceObjectBinding` and `TypedEvidenceGraph` helpers. Every
Cause-owned direct binding is emitted separately so actor, target, mechanism,
and consequence cannot be assembled from different records. Claim linkage is
direct-evidence only: ancestor closure remains diagnostic context and cannot
authorize Jp, R, packet, or public-P survival. The adapter preserves Ja status,
R deduplication paths/ranks, packet membership, and the claim IDs that actually
appear as ideas under the production player-facing selector.

Generic fallback causes remain visible as C diagnostics. Their
`fallback_dominance` value is the decision persisted by R after readiness and
same-channel comparison; `null` means that the cause did not enter that
player-facing dominance set. Packet and P consume the persisted decision and
the adapter never recomputes it.

To keep batch output small, the adapter emits a hash and status summary of the
public response instead of copying the full response body. It retains both the
individual owned bindings and aggregate actor/target/mechanism/consequence/
witness summaries; only an individual owned binding can satisfy readiness.
The ordinary native observation adapter remains the lossless public-response
capture.

The exact public `probe_requests` objects are also repeated at the top level.
An external runner can therefore acquire only the probes issued by this
invocation, append validated results to the original runtime request, and
rerun the same compact adapter until the desired cause evidence is closed.

The CLI reads and writes one UTF-8 JSON document per line:

```text
sbt -batch -error "runMain io.chesstory.evaluation.runtimeadapter.CauseAuditAdapterCli" < requests.jsonl > cause-observations.jsonl
```

Requires JDK 21 and sbt 1.11.7.

```text
cd judgment-evaluation/runtime-adapter
sbt -batch -error compile
sbt -batch -error run < requests.jsonl > observations.jsonl
```
