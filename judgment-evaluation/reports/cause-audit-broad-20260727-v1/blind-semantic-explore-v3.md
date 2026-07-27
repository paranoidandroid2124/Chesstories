# Blind Semantic Audit: EXPLORE v3

## Scope

This is a blind human-proxy chess-semantic audit of the 16 freeze-manifest cases in `partition=explore`. I used only the authorized case, adjudicated-oracle, forced-engine, v3 explore-runtime, and manifest artifacts, plus my completed v2 blind assessment as the comparison baseline. I did not inspect production code, implementation files, git state, candidate identity, prior fixes, or sealed content.

`Survives` means a C-linked claim reaches the selected public payload and the recorded public response is `ready`/`ok`. The runtime exposes only the public body hash, so exact prose wording cannot be independently checked.

## Concise result

V3 is substantially leaner and operationally cleaner than v2, but it does not improve correct-core Cause selection.

- V3 verdicts remain 0 pass, 9 partial, 7 fail.
- Correct core still reaches the selected payload in the same 6 cases: `cause-audit-cs-02`, `cause-audit-cs-08`, `cause-audit-cs-09`, `cause-audit-dv-03`, `cause-audit-dv-05`, `cause-audit-sw-01`.
- Only a partial surrogate still reaches the payload in the same 5 cases: `cause-audit-cs-06`, `cause-audit-cs-12`, `cause-audit-dv-02`, `cause-audit-dv-10`, `cause-audit-sw-05`.
- No correct or close cause still survives in the same 3 answerable cases: `cause-audit-cs-05`, `cause-audit-dv-06`, `cause-audit-sw-04`.
- No-adverse handling is unchanged: `cause-audit-cs-01` selects only positive context; `cause-audit-cs-07` suppresses every C cause.
- All 16 v3 probe closures are complete, versus only 3/16 in v2.

## Aggregate v2 to v3 comparison

| Measure | v2 | v3 | Change |
|---|---:|---:|---:|
| Total C records | 196 | 164 | -32 (-16.3%) |
| Selected C records | 59 | 40 | -19 (-32.2%) |
| Selected public idea claim IDs | 111 | 95 | -16 (-14.4%) |
| Semantic duplicate groups | 33 | 28 | -5 |
| C records inside duplicate groups | 89 | 72 | -17 |
| Redundant records beyond one per duplicate group | 56 | 44 | -12 |
| Selected C records inside duplicate groups | 38 | 22 | -16 |
| C records with no focused relative target | 123/196 (62.8%) | 91/164 (55.5%) | Better by 7.3 points |
| Selected C records with no focused relative target | 32/59 (54.2%) | 29/40 (72.5%) | Worse by 18.3 points |
| Median object binding count | 426.5 | 423 | Essentially unchanged |
| Maximum object binding count | 862 | 777 | Lower, still extreme |
| Selected causes with neither owned tactical nor long-term proof | 4 | 1 | Improved |
| Selected source-inverted `conversion_miss` records | 9 | 9 | No improvement |
| Cases with selected explicitly forbidden MaterialSwing | 6 | 5 | One-case improvement |
| Fully closed probe views | 3/16 | 16/16 | Improved by 13 cases |

The main tradeoff is clear: selection is much smaller, but the surviving set is not more target-specific. V3 preferentially removes many target-bearing strategic duplicates while retaining empty-target material, tactical, and conversion records.

## Per-case comparison

| Case | v3 verdict | Correct core in selected payload | v2 to v3 C / selected-C counts | Semantic change |
|---|---|---|---|---|
| `cause-audit-cs-01` | Partial | No adverse core needed; positive plan only | 10->8 / 2->2 | Improved C noise; polarity and positive fallback unchanged. |
| `cause-audit-cs-02` | Partial | Yes, one PlanContradiction | 18->18 / 6->2 | Strong selection-noise improvement; forbidden MaterialSwing still rank 0. |
| `cause-audit-cs-05` | Fail | No | 3->1 / 3->1 | False material and defensive records removed; remaining missed-tactical cause is still wrong. |
| `cause-audit-cs-06` | Partial | Partial plan surrogate | 25->25 / 8->3 | Five duplicate plan selections removed; e-file/c-file target error and wrong top causes remain. |
| `cause-audit-cs-07` | Fail at C | No cause needed; all C suppressed | 8->8 / 0->0 | Unchanged safe downstream abstention and unchanged bad C generation. |
| `cause-audit-cs-08` | Partial | Yes, missed `Nd5` resource | 10->10 / 3->3 | Unchanged. |
| `cause-audit-cs-09` | Partial | Yes, MaterialSwing | 12->7 / 1->1 | Five false defensive causes removed; correct consequence and empty targets unchanged. |
| `cause-audit-cs-12` | Fail | Partial `Nc4` reference surrogate | 2->1 / 1->1 | False material record removed; actor/source-side mismatch unchanged. |
| `cause-audit-dv-02` | Fail | Partial conversion outcome only | 6->3 / 2->1 | Leaner but mixed: active-rook reference surrogate removed, leaving only forbidden MaterialSwing. |
| `cause-audit-dv-03` | Partial | Yes, TacticalRefutationOfPlayed | 10->10 / 2->2 | Unchanged. |
| `cause-audit-dv-05` | Partial | Yes, played refutation | 11->7 / 2->2 | C noise improved; correct OnlyMoveNecessity remains generated but unselected. |
| `cause-audit-dv-06` | Fail | No | 13->4 / 1->1 | Core availability regressed: closer defensive-resource records were removed, leaving four wrong tactical records. |
| `cause-audit-dv-10` | Fail | Partial conversion outcome only | 3->3 / 1->1 | Unchanged. |
| `cause-audit-sw-01` | Partial | Yes, played refutation and missed `Rd1` | 20->14 / 9->9 | Unselected noise improved; selected noise and reference-side conversion inversion unchanged. |
| `cause-audit-sw-04` | Fail | No | 28->28 / 12->9 | Three duplicate plan selections removed; correct ordinal 16 still dies and inverted conversion causes remain. |
| `cause-audit-sw-05` | Partial | Partial f-file surrogate | 17->17 / 6->2 | Strong selection-noise improvement; forbidden material and incomplete promotion mechanism remain. |

Overall v3 change classes:

- Noise or overclaim improved without a new core: 10 cases - `cause-audit-cs-01`, `cause-audit-cs-02`, `cause-audit-cs-05`, `cause-audit-cs-06`, `cause-audit-cs-09`, `cause-audit-cs-12`, `cause-audit-dv-05`, `cause-audit-sw-01`, `cause-audit-sw-04`, `cause-audit-sw-05`.
- Semantically unchanged: 4 cases - `cause-audit-cs-07`, `cause-audit-cs-08`, `cause-audit-dv-03`, `cause-audit-dv-10`.
- Mixed or regressed core availability: 2 cases - `cause-audit-dv-02`, `cause-audit-dv-06`.

## Shared defect families

### 1. Actor and target binding contamination remains the dominant C defect

Earliest stage: C.

All 164 C records still have object binding counts of at least 133; the median is 423. Selected records still bind 52-179 actor candidates, with a median of 100. Ninety-one causes have no focused target. Among selected causes, empty focused targets rise from 54.2% in v2 to 72.5% in v3.

Concrete unchanged failures include the e-file instead of c-file in `cause-audit-cs-06`, no e6/a5/king targets in `cause-audit-cs-09`, no pawn chain in `cause-audit-dv-03`, no b5/Rxb5 target in `cause-audit-dv-06`, and a rook/a2/a7 target instead of bishop/c6-passer semantics in `cause-audit-sw-04`.

### 2. Evaluation delta still collapses into material or tactical categories

Earliest stage: C.

V3 reduces `material_swing` records from 44 to 37 and selected material records from 11 to 10. The selected records occur in 9 cases, while only `cause-audit-cs-09` expects MaterialSwing. V3 removes the forbidden selected material claim from `cause-audit-cs-05`, but forbidden MaterialSwing still survives in `cause-audit-cs-02`, `cause-audit-cs-06`, `cause-audit-dv-02`, `cause-audit-dv-05`, and `cause-audit-sw-05`.

Wrong selected MissedTacticalResource labels also remain for quiet or strategic alternatives in `cause-audit-cs-05`, `cause-audit-cs-06`, `cause-audit-dv-06`, and `cause-audit-sw-04`.

### 3. Comparator duplication is reduced, not solved

Earliest stage: C.

The duplicate signature counts improve from 33 groups/89 records/56 extras to 28 groups/72 records/44 extras. Selected causes inside duplicate groups fall from 38 to 22. Duplicate groups still affect `cause-audit-cs-01`, `cause-audit-cs-02`, `cause-audit-cs-06`, `cause-audit-cs-08`, `cause-audit-dv-06`, `cause-audit-sw-01`, `cause-audit-sw-04`, and `cause-audit-sw-05`.

### 4. Conversion source-side and polarity inversion is unchanged downstream

Earliest stage: C.

V3 has 10 `conversion_miss` records, all `source_side=reference` with `reference_creates_resource`. Nine are selected: five in `cause-audit-sw-01` and four in `cause-audit-sw-04`. V2 also selected nine. The better reference move should secure conversion; the played move owns the conversion miss.

### 5. Correct-core selection is flat

Earliest stage for generation defects: C. Earliest stage for survival defects: P/R.

No case gains a newly selected correct core in v3. The six yes-core, five partial-surrogate, three no-core, and two no-adverse outcomes are exactly the same as v2.

- `cause-audit-dv-05`: correct OnlyMoveNecessity remains generated but unselected.
- `cause-audit-sw-04`: correct reference ConversionSecured at ordinal 16 remains unselected while source-inverted conversion causes survive.
- `cause-audit-dv-06`: v3 removes the closer defensive-resource surrogates altogether and retains only wrong missed-tactical causes.

### 6. Abstention behavior is unchanged

Earliest stage: C.

`cause-audit-cs-01` is cleaner at C but still selects two positive causes instead of an explicit no-adverse result. `cause-audit-cs-07` still generates eight unsupported causes and suppresses all of them. Downstream safety is preserved; C answerability is not.

### 7. Proof closure and proof gating improve

Earliest stage: runtime evidence closure and selection.

All 16 v3 views report `all_closed=true`, eliminating the 13-case closure warning from v2. Selected causes with neither owned tactical nor admissible long-term proof fall from four to one; the remaining record is `cause-audit-sw-04` ordinal 14. This is a real reliability improvement, although it does not repair the cause semantics.

## Bottom line

V3 improves noise control, duplicate selection, proof closure, and one forbidden-material case. It does not improve the number of cases with a correct selected Cause, does not repair reference-side conversion polarity, and does not make selected actors or targets more exact. The semantic bottleneck remains at C; ranking and projection are leaner but still preserve the same core successes and failures as v2.
