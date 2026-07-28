# Chesstory runtime native observation adapter

This external JSONL adapter calls `RuntimeProtocol.evaluateWithBoundary` with
the identity intervention and captures the runtime's native Q → F → C → Jp →
Ja → R → P boundary values. It does not acquire engine evidence, implement
chess judgment, or reproduce any production decision rule.

Each UTF-8 input line is a `chesstory.move-meaning.request.v1` envelope. Each
output line is a `chesstory.runtime-observation.v3` document containing the
exact public response and a deterministic structural snapshot for every native
boundary that was reached. Invalid and rejected requests retain the public
error response while marking Q through P as unavailable, rather than inventing
artifacts for boundaries that did not execute. V remains a typed, explicit
unavailability: the runtime exposes structured projection but has no
verbalization stage.

## Native tree v2 is not semantic v1

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
accepting v3.

The 0.3.0 CLI now emits native observation/boundary v3 because the public
response is v3. Archived native-v2 schemas and runners remain frozen; the
existing v2 runner must reject the new CLI until a separate v3 runner migration.

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
`assembly`/`probeRequests`/`playerFacingClaimDecisions`/
`onlyMoveConstraintResolutions`/`causeExposureResolution`/
`causeDispositionLedger`, respectively). Derived lookup and policy methods
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
- `schemas/native-v3/runtime-observation.schema.json`
- `schemas/native-v2/runtime-observation.schema.json` (archived observations)
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

The active contracts are `chesstory.move-meaning.response.v3`,
`chesstory.cause-audit-runtime-observation.v3`, and
`chesstory.eval.cause-audit-actual-cascade.v3`. The v1/v2 cascade schemas remain
readable only for archived exploration artifacts. The generic runtime CLI
remains `0.3.0`; the Cause-audit CLI is `0.3.2`.

The cause-audit CLI selects V3 when invoked without arguments; the explicit
`--observation-schema-version=3` spelling selects the same active contract.
Only `--historical-observation-schema-version=2` selects the archived V2 view.
The V3 view additionally
captures the complete R-native Cause selections directly from
`causeExposureResolution` before packet construction, both as the top-level
`r_native_cause_selections` multiset and as each Cause's `r.native_selection`.
Its C-owned channel rows also carry the full semantic line and horizon; these
fields are deliberately absent from the unchanged v2 view. Each v3 C-owned and
R-selected channel also carries a nullable `proof_segment` encoded by the
runtime from that channel's `rootOwnedProof` alone. It is sentence-ready move
evidence, not a readiness, selection, fallback, or importance input; unsafe
compression remains `null` without removing the Cause. V3 also records
the R-native and packet-native `DirectCauseImportanceResolution`, including
explicit incomparability and unmeasured channels, plus the same public
projection shape at R, packet, and public P. This lets the evaluator verify importance copying
without reconstructing the importance policy. The public shape retains every
profile-pair relation, including incomparable pairs and both causal signatures,
as well as per-Cause measured/unmeasured/dominance decisions.

Cause-audit CLI `0.3.2` also records the exact engine-backed
`PlayedVsBest` verdict independently from Cause availability. V3 carries the
same compact DTO at `verdict.packet_canonical`,
`verdict.selected_projection`, and `verdict.final_public_response`; the
adapter fails closed unless all three are exactly equal. The DTO contains only
comparison kind, mover, played/reference moves, mover-oriented win-percent
delta and loss, verdict/quality, and optional mate outcome. It never borrows
claim or Cause evidence. A valid engine verdict therefore remains observable
when no differential Cause is certified, while an unavailable verdict is
explicitly `null` at all three locations. The verdict DTO portion of historical
V2 output is unchanged.
V3 additionally carries `verdict.classification_authority`, recomputed from the
registered PlayedVsBest line nodes by the versioned central
`VerdictThresholdPolicy`. This audit-only basis binds the public labels,
delta/loss, candidate-set type, and mover-oriented mate transition without
duplicating classification thresholds in the evaluator.

Every `RelativeCauseFact` is emitted with its candidate comparison and root
moves, source side, attribution, graph-derived role/event line/importance,
three proof sections, production proof-kind labels, and SHA-256 hashes of the
complete native source records. Raw typed channels remain diagnostic, while
`RelativeCauseConstructionAdmission.admittedDirectChannels` is the sole authority for deciding
whether an individual Cause-owned channel is sentence-ready. Whole-Cause
readiness additionally requires a registered Cause, an engine-backed distinct-
root comparison, valid comparison orientation and source/attribution, matched
root ownership, direct-proof eligibility, owned depth, and exclusion of the
legacy independent OnlyMove Cause. Every public-ready Cause-owned channel is
emitted separately so actor, target, mechanism, consequence, and change cannot
be assembled from different records.
Claim linkage is
direct-evidence only: ancestor closure remains diagnostic context and cannot
authorize Jp, R, packet, or public-P survival. The adapter preserves Ja status,
R deduplication paths/ranks, typed packet selection, and the exact Cause IDs
that appear as ideas under the production player-facing selector.

The raw `r.ranked` table is diagnostic canonical host transport, ordered only
by exposure tier descending and claim ID ascending. It carries no independent
chess-priority or salience score. The evaluator
counts a Cause as surviving R only from its selected native cross-comparison
decision. `comparison_exposure_rank` is comparison-exposure authority and never chess
importance. V3 importance is judged only from measured root-owned effect
profiles, their typed partial-order relations, and a certified `unique_top`;
unmeasured or incomparable effects remain explicitly unscored. Packet
membership comes from typed Cause selections. The C-level `binding_tier`
records only whether the Cause binding is primary, supporting, or context; it
is not an importance signal.
Public P membership and metadata are independently parsed from each emitted
idea and must exactly match the packet selection. The shared typed shape binds
Cause ID/kind, comparison-exposure rank/order/exposure/effect, source side, both attribution root
flags, the unique host claim/family/tier, and the full comparison identity,
viewpoint, verdict, delta, compared move, and semantic lines. It also binds the
only-move qualifier and every channel's registered carrier metadata,
provenance, actor/target/mechanism/consequence, witness, semantic line, horizon,
causal signature, direct change, and played-facing change. Public carrier and
provenance metadata are accepted only when they equal the registered graph
reference. Public channel objects are reconstructed with the production
`DirectCauseChannel.causalSignature` implementation and must equal the
Cause-owned public-ready channel set. Thus packet loss cannot be called R loss,
and a mutated P idea cannot pass merely because its Cause ID survived.
For a full, renderable projection, the evaluator also compares the complete
packet and public selection multisets and requires every raw public Cause ID to
have exactly one typed parse. An unregistered, malformed, duplicated, missing,
or extra public idea therefore fails P even when an expected Cause was
preserved correctly. When the primary comparison is unavailable, R-to-packet
equality remains mandatory while an empty public payload is the expected
boundary; any public Cause emitted in that state still fails P.

V3 additionally preserves the runtime's sentence-ready idea units and
idea-level importance. R-native and packet-native values must always be exact;
packet-native and public-P values must also be exact for a full, renderable
projection, while a withheld projection must keep public units and importance
empty under the primary availability reason.
Every raw public Cause item must be a `cause_facet`, link to exactly one unit,
and appear in that unit's ordered membership; unit lead, kind, importance
layer, priority status, and serialization order are copied without evaluator
ranking. Priority is therefore judged through the native idea-importance lead,
so the liability and resource facets of one exact PlayedVsBest responsibility
unit remain distinct Cause meanings without competing as two public ideas.
Malformed or incomplete membership fails closed. Archived V2 artifacts remain
readable; regenerated rank diagnostics intentionally omit the retired claim
salience and heuristic-priority fields.

In this contract, `ideas` are Cause facets and `idea_units` are the
sentence/priority units. Only `idea_importance.unique_top` can authorize the
top sentence-ready idea. A null value means that no unique top was certified;
array position must never replace it. The separate `importance.unique_top`
remains Cause-facet diagnostics and has no sentence-top authority.

Line and evidence IDs in this exact comparison are transport/provenance
identities only. They never enter semantic fallback dominance or cross-
comparison priority: those policies continue to use role, rank, and normalized
root move. This keeps harmless ID allocation separate from loss or corruption
at the packet-to-public boundary.

Owned actor/target/mechanism/consequence fields establish structural tuple
completeness only. The cause-audit oracle has no typed values for those four
fields, so the harness does not claim semantic value equality. It also has no
typed attribution-polarity label that could distinguish a generic candidate's
“allows liability” from “creates value”; that semantic assessment is explicitly
reported as not performed rather than treated as an exact match.

The observation also exposes the production `RootOwnedCausalEpisode` inventory
and the episodes retained by each Cause. Each episode carries one root actor,
one concrete target, its verified causal-link kinds, the resulting consequence,
and the replay prefix from root to result. Per-Cause proof state includes total
and forcing-tactical episode counts; the adapter only serializes these native
derivations and does not infer episode ownership itself.

Only-move candidate-set facts are emitted separately as packet-derived
`only_move_constraints`. A constraint is `diagnostic_only` unless the packet's
retained Cause on the exact comparison and reference line passes the production
object-readiness and root-ownership policy. An admitted qualifier contains only
the exact Cause reference and relation strength; it never synthesizes actor,
target, mechanism, or consequence fields. The current relation is
`same_channel_association`, which explicitly does not license an “only because”
claim without an all-alternatives mechanism contrast.

Generic fallback causes remain visible as C diagnostics. Their
`fallback_dominance` value is the decision persisted by R after readiness and
same-channel comparison; `null` means that the cause did not enter that
player-facing dominance set. The packet factory independently recomputes the
complete exposure resolution and closes only when it exactly equals R's
persisted decisions; P then consumes the closed packet selections. The adapter
only serializes those native values and never makes a replacement decision.

V3 also records the complete terminal `cause_disposition_ledger` for every C
Cause at R and in the packet: status, authority reason, proposed/certified/
rank-eligible claim IDs, selected owner, and related Cause/claim IDs. It uses
`RuntimeProtocol.causeDispositionPublicJson` directly for both canonical
summaries and strictly parses the selected and final public
`idea_status_detail`; the adapter never recounts or reclassifies a disposition.
The evaluator checks one ledger row per C record, exact selected-owner closure
against R selections, exact R-to-packet DTO copying, and exact packet-summary
to public copying, including withheld public idea payloads.

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

sbt -batch -error "runMain io.chesstory.evaluation.runtimeadapter.CauseAuditAdapterCli --historical-observation-schema-version=2" < requests.jsonl > historical-v2-cause-observations.jsonl
```

Requires JDK 21 and sbt 1.11.7.

```text
cd judgment-evaluation/runtime-adapter
sbt -batch -error compile
sbt -batch -error run < requests.jsonl > observations.jsonl
```
