# Blind Semantic Audit: EXPLORE

## Scope and method

This is a blind human-proxy chess-semantic audit of the 16 cases assigned to `partition=explore` by the freeze manifest. I used only the authorized case, adjudicated-oracle, forced-engine, explore-runtime, and manifest artifacts. I did not inspect production code, implementation files, candidate identity, git state, prior fixes, or sealed content.

The judgment is strict. A cause must get the actor/source side, target, mechanism, consequence, polarity, and priority substantially right. A correct high-level kind without the chess object or forcing mechanism is only partial. `Survives` below means the C-linked claim ID reaches the selected public payload and the recorded public response is `ready`/`ok`; the runtime artifact exposes only the public body hash, so exact prose wording cannot be independently checked.

## Result

- Cases: 16 explore only.
- Overall verdicts: 0 pass, 9 partial, 7 fail.
- Actual C records: 196 total, 59 selected C records.
- Public path: all 16 projections are renderable and all 16 public responses are `ready`/`ok`.
- Correct core reaches the selected payload in 6 cases: `cause-audit-cs-02`, `cause-audit-cs-08`, `cause-audit-cs-09`, `cause-audit-dv-03`, `cause-audit-dv-05`, `cause-audit-sw-01`.
- Only a semantically adjacent surrogate reaches the selected payload in 5 cases: `cause-audit-cs-06`, `cause-audit-cs-12`, `cause-audit-dv-02`, `cause-audit-dv-10`, `cause-audit-sw-05`.
- No correct or sufficiently close cause reaches the selected payload in 3 answerable cases: `cause-audit-cs-05`, `cause-audit-dv-06`, `cause-audit-sw-04`.
- The 2 no-adverse cases split: `cause-audit-cs-01` selects only positive, engine-consistent context; `cause-audit-cs-07` suppresses all eight C records.

Overall partial cases: `cause-audit-cs-01`, `cause-audit-cs-02`, `cause-audit-cs-06`, `cause-audit-cs-08`, `cause-audit-cs-09`, `cause-audit-dv-03`, `cause-audit-dv-05`, `cause-audit-sw-01`, `cause-audit-sw-05`.

Overall fail cases: `cause-audit-cs-05`, `cause-audit-cs-07`, `cause-audit-cs-12`, `cause-audit-dv-02`, `cause-audit-dv-06`, `cause-audit-dv-10`, `cause-audit-sw-04`.

## Per-case judgment

| Case | Verdict | Oracle chess core | Actual C and downstream result | Earliest visible defect |
|---|---|---|---|---|
| `cause-audit-cs-01` | Partial | No defensible adverse cause; `b7b5` is engine rank 1 | Ten positive/context causes are emitted. Selected plan-improvement causes plausibly free the c8 bishop and do not blame the move, but there is no explicit abstention. | C |
| `cause-audit-cs-02` | Partial | `Bf3` fails to reinforce d4; `...Bxd4` and `...Ne6` release the blockade | Plan-contradiction records name d4/the knight and survive. A forbidden MaterialSwing claim is selected and outranks the strategic explanation; targets and mechanism are diffuse. | C |
| `cause-audit-cs-05` | Fail | `Be2` is the wrong development order and blocks active coordination | C emits only MaterialSwing, DefensiveResource, and MissedTacticalResource. All three are selected; no move-order, plan, or activity cause exists. | C |
| `cause-audit-cs-06` | Partial | `Re8` abandons the c-file and permits `Rc7` | Plan-contradiction surrogates survive and include c7, but they name the e-file. Material and missed-tactical claims also survive; positive restriction records credit the losing move. | C |
| `cause-audit-cs-07` | Fail | No stable adverse cause for `Bxd2` | C invents four plan contradictions around a2 and four restriction gains around d2. None reaches projection, so downstream suppression is safe. | C |
| `cause-audit-cs-08` | Partial | `Qd8` misses the necessary `Nd5` counterattack and king relief | A selected MissedTacticalResource cause correctly points to `Nd5` and is priority 0. It has no focused target and omits only-defense, draw, exchanges, and king-safety semantics. | C |
| `cause-audit-cs-09` | Partial | `Qa5` permits `Nxe6`, `Bb6`, and e-file entry, losing material | Selected MaterialSwing has correct polarity and consequence. Every focused target set is empty, so e6, a5, the queen, and the king are absent from C semantics. | C |
| `cause-audit-cs-12` | Fail | Immediate `c4` closes tension too soon; `Nc4` first preserves the right move order and meets `b4` | The selected cause is a positive reference-side PlanImprovement for `Nc4`. It is useful contrast, but it replaces the played c-pawn actor and never states premature closure or the b4 clamp. | C |
| `cause-audit-dv-02` | Fail | `Rf5` permits rook exchange and loses the win through inactivity | C has MaterialSwing and reference DefensiveResource only. A linked conversion claim survives, but no ConversionMiss, ActivityLoss, checking-distance, or rook-exchange mechanism exists. | C |
| `cause-audit-dv-03` | Partial | `h6` loses the race to `f6`; timely `...f6` wins | Selected TacticalRefutationOfPlayed is correct and priority 0. The f5-g4-h5 chain and break timing have no focused target representation; MaterialSwing is an outcome surrogate. | C |
| `cause-audit-dv-05` | Partial | `Kb6` is the only drawing resource; `h6` permits h5/f5 breakthroughs | Correct TacticalRefutationOfPlayed survives. Correct OnlyMoveNecessity is generated at C ordinal 11 but is not selected, and forbidden MaterialSwing is selected. | C; additional P/R loss |
| `cause-audit-dv-06` | Fail | `b5` fixes a target for `Rxb5` and gives up active drawing chances | No pawn-target or strategic-concession cause exists. The only selected C is a reference-side MissedTacticalResource for `h5`; the closer defensive-resource record is suppressed. | C; additional P/R loss |
| `cause-audit-dv-10` | Fail | `Bxg4` releases the king and loses the zugzwang win; only `Kf4` maintains restriction | Selected MaterialSwing carries a conversion outcome, but no OnlyMoveNecessity, ConversionMiss, or OpponentRestriction exists, and every focused target set is empty. | C |
| `cause-audit-sw-01` | Partial | `e4` wastes the checking tempo; `Rd1+` is the only win and `Ra3` forces the draw | Correct played tactical refutation and missed `Rd1` resource both survive with top priority. Five selected ConversionMiss records are wrongly owned by the winning reference move, and target geometry is empty. | C |
| `cause-audit-sw-04` | Fail | `Be5` loses a tempo before controlling the c6 passer | The one good reference contrast, ConversionSecured for `bxc6` at ordinal 16, is not selected. Selected plan causes target a rook/a2/a7, while four selected ConversionMiss records are assigned to the winning reference king move. | C; additional P/R loss |
| `cause-audit-sw-05` | Partial | `Rxh6` abandons control and allows the immediate `f3-f2-f1=Q` race | Candidate-side plan causes identify the f-file and rook and survive, but omit the black f-pawn and f1. Selected MaterialSwing is forbidden and replaces exact tactical-liability/refutation semantics. | C |

## Shared defect families

### 1. Actor and target binding contamination

Earliest stage: C.

This affects all 16 explore cases. Every one of the 196 C records has an object `binding_count` of at least 127; the median is 426.5 and the maximum is 862. The selected records still carry 52-179 actor candidates, with a median of 102. A focused `relative_cause_targets` set is empty in 123/196 records (62.8%) and in 32/59 selected records (54.2%).

The practical result is that exact chess actors and targets are usually recoverable only from the root move or oracle, not from C object semantics. Concrete failures include the e-file instead of c-file in `cause-audit-cs-06`, no e6/a5/king targets in `cause-audit-cs-09`, no pawn-chain target in `cause-audit-dv-03`, no b5/Rxb5 target in `cause-audit-dv-06`, and a rook/a2/a7 target instead of the bishop/c6 passer in `cause-audit-sw-04`.

### 2. Evaluation delta collapses into material or tactical labels

Earliest stage: C.

`material_swing` appears 44 times across 12 cases. Eleven material C records are selected across 10 cases, but only `cause-audit-cs-09` has MaterialSwing in the adjudicated expected kinds. In six cases the selected material cause is explicitly forbidden: `cause-audit-cs-02`, `cause-audit-cs-05`, `cause-audit-cs-06`, `cause-audit-dv-02`, `cause-audit-dv-05`, `cause-audit-sw-05`.

Likewise, selected MissedTacticalResource records are semantically wrong for quiet or strategic alternatives in `cause-audit-cs-05`, `cause-audit-cs-06`, `cause-audit-dv-06`, and `cause-audit-sw-04`. The score gap is real, but the mechanism is not automatically material or tactical.

### 3. Per-comparator proliferation and semantic duplication

Earliest stage: C.

There are 196 causes for 16 positions, an average of 12.25 per case. Grouping by case, kind, source, candidate root, reference root, and focused targets yields 33 duplicate semantic groups containing 89 records, or 56 redundant records beyond one per group. Duplicate groups occur in 11 cases: `cause-audit-cs-01`, `cause-audit-cs-02`, `cause-audit-cs-06`, `cause-audit-cs-08`, `cause-audit-cs-09`, `cause-audit-dv-02`, `cause-audit-dv-05`, `cause-audit-dv-06`, `cause-audit-sw-01`, `cause-audit-sw-04`, `cause-audit-sw-05`.

This is not harmless verbosity. Thirty-eight selected causes belong to a duplicate group, and the repetitions create competing priorities and contradictory ownership.

### 4. Reference-side polarity inversion for conversion misses

Earliest stage: C.

All 11 `conversion_miss` records occur in `cause-audit-sw-01` and `cause-audit-sw-04`. Ten are `source_side=reference` and one is `shared`; all use `reference_creates_resource` on the better move. Nine of these records are selected. A conversion miss is an adverse property of the played move, while the reference move secures or preserves conversion. This actor/source inversion is therefore semantic, not merely terminological.

### 5. Correct cause loses at selection

Earliest stage for the survival failure: P/R, although all affected cases already contain C-level noise.

- `cause-audit-dv-05`: ordinal 11 correctly identifies OnlyMoveNecessity for `Kb6` but is not selected. A correct played tactical-refutation cause does survive, so the final result is partial rather than total loss.
- `cause-audit-dv-06`: the closest defensive-resource record, ordinal 12, is suppressed while ordinal 13 MissedTacticalResource is selected.
- `cause-audit-sw-04`: ordinal 16 correctly connects `bxc6` with passer control and secured conversion, but it is suppressed while source-inverted conversion and missed-tactical records survive.

### 6. No-adverse fallback is inconsistent

Earliest stage: C.

The two no-adverse cases take different paths. `cause-audit-cs-01` emits ten causes and selects two positive plan explanations; this is safe in polarity but not an explicit abstention. `cause-audit-cs-07` emits eight unsupported causes, including adverse plan claims, then suppresses all of them. Projection protects the latter case, but C itself does not honor answerability.

### 7. Proof-closure caution

Thirteen of the 16 runtime views report `all_closed=false` even though the case and public response are complete. The affected cases are `cause-audit-cs-01`, `cause-audit-cs-02`, `cause-audit-cs-06`, `cause-audit-cs-07`, `cause-audit-cs-08`, `cause-audit-cs-09`, `cause-audit-cs-12`, `cause-audit-dv-02`, `cause-audit-dv-03`, `cause-audit-dv-06`, `cause-audit-dv-10`, `cause-audit-sw-01`, `cause-audit-sw-04`. I treat this as a confidence warning, not by itself as proof that a semantic claim is false.

## Bottom line

The explore runtime consistently detects large evaluation changes and often gets broad polarity right. Its main weakness is earlier and more structural: C does not produce atomic chess causes. It over-binds actors and targets, generates one generic cause per comparator, and maps score gaps into material/tactical categories. Projection sometimes suppresses the noise, but in several cases it selects the wrong surrogate or reverses source ownership. No explore case meets strict exact-semantic pass criteria.
