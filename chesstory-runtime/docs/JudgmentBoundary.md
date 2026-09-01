# 체스토리 판단 경계

이 문서는 현재 런타임의 의미 소유권만 기록한다. 가족별 정확한 명제,
하부 premise, 폐쇄 부재, 공개 필드, 형식 테스트와 레퍼런스·커버리지 판정은
[`l2-proof-provenance.json`](../../judgment-evaluation/references/l2-proof-provenance.json)이
소유한다. 이 문서에 그 원장을 복제하지 않는다. JSON 원장은 CI용 provenance
인벤토리이며 런타임 보드 사실이나 증명 premise는 아니다.

## 세 경계

### Assessment

Assessment는 후보 수의 평가와 선택을 소유한다. `CandidateLineNode`,
`CandidateComparison`, `BestChoice`, `MoveVerdict`, engine rank와 score는 이
경계에만 속한다. 현재 유일한 comparison transport는 실전수와 최선수를
정량 비교하는 `PlayedVsBest`, 그리고 실전수가 최선일 때 public
`best_choice`가 표시할 runner-up을 정량 비교하는 `BestVsSecond`뿐이다.
`BestVsSecond`는 최선수의 causal idea가 아니다. 이 값들은 어떤 L2 증명이
존재하는지, 어느 branch가 인과 기준인지, proof ID나 dependency fingerprint가
무엇인지 결정하지 않는다.

### Occurrence-directed Explanation proof

Explanation은 인증된 수순 occurrence에 대한 명시적 요청에서만 실행된다.
`ExplanationRequest`가 수·before/after position·ply를 지정하고,
`OccurrenceExplanationDemand`가 이를 그래프의 유일한 observed root
occurrence에 결속한다. 각 증명군은 이미 등록된 `LegalLine` replay와
L0/L0.5/L1 결과를 자기 체스 의미에 따라 orientation한다. 평가 role, rank,
verdict 또는 점수로 branch를 고르지 않는다.

요청이 없으면 `OccurrenceExplanationAssembler`는 typed L2 proof나
`OccurrenceExplanationCause`를 생산하지 않는다. 요청 자체도 체스 명제의
premise가 아니다.

### Structured Presentation

Presentation은 Assessment와 0..N개의 exact explanation proof를 나란히
투영한다. Runtime과 public schema는 자연어 문장이 아니라 subject occurrence,
typed branch와 ordered step, actor/target, lower premise, closed absence/state,
독립 proof path와 dependency fingerprint를 운반한다.

프런트의 목표 소비는 이 구조로 실제 root와 분석 branch, 수순 timeline,
보드 강조, 폐쇄 라벨과 proof-path 선택을 표시하는 것이다. 체스 의미를 다시
판정하거나 family별 장문 문장을 별도 진실 원천으로 만들 수 없다. 현재 확인된
UI branch는 이전 `causal_explanations` transport를 읽으므로 이 계약의 실제
소비자로 승인되지 않았다. 현 완료 경계는 exact Runtime/schema와 immutable
producer fixture까지다. 프런트 구현과 테스트는 별도 작업에서
`occurrence_explanations` fixture를 직접 소비한 뒤에만 완료로 기록한다.

## Line 소유권과 occurrence provenance

`LegalLineNode`는 합법 재생된 한 root move의 role-neutral owner다. 한 root
move에는 정확히 하나의 `LegalLine` owner만 존재하며, replay와 line evidence를
소유한다. `LineNodeRef`에는 평가 role이나 rank가 없다.

`CandidateLineNode`는 기존 `LegalLine`을 참조하는 Assessment projection이다.
role, rank와 evaluation은 이 projection에만 있다. projection의 선택이나 값이
바뀌어도 같은 replay의 `LegalLine` owner와 root transition identity는 바뀌지
않는다. 반대로 assessment candidate가 없는 line을 평가 사실로 가장하지 않는다.

실제성은 line selection role이 아니라 다음 인증의 결합이 소유한다.

- canonical game history와 정확히 일치하는 root move 및 before/after position
- 그 root의 exact `MoveTransitionEdge` owner
- 그 transition을 시작하는 exact `LegalLine` owner와 canonical replay
- typed `CausalRootProvenance`

따라서 observed 수가 engine best인 경우에도 그 단일 `LegalLine` owner가
`ObservedGameRoot`가 될 수 있다. 별도 played owner를 복제하지 않는다.
counterfactual root는 `CounterfactualAnalyzedRoot`이며, observed branch에서도
history가 인증하는 것은 root뿐이다. 뒤의 PV suffix는
`CertifiedAnalysisMove`이지 실제 대국 수순이 아니다.

현재 공개 explanation subject는 history가 인증한 observed root occurrence다.
상대 응수나 뒤 occurrence를 별도 subject로 요청하는 실제 상위 producer가
생기기 전에는 이를 지원한다고 주장하거나 빈 API를 만들지 않는다.

## L0~L2 권한

- **L0**는 한 position occurrence의 폐쇄된 보드 사실과 부재다.
- **L0.5**는 같은 합법 수의 before/after에서 직접 인증되는 정확한 변화다.
- **L1**은 한 transition 직후의 자원·제약과 폐쇄 인벤토리다.
- **L2**는 둘 이상의 ordered occurrence 또는 exact sibling을 기존 하부
  record로 연결하는 bounded causal proof다.

L2는 공격맵, 합법수, ray, pin, pawn structure를 다시 계산하지 않는다.
평가 변화나 책의 `best`, `only`, `prevents`, `plan`, `counterplay`도 premise로
사용하지 않는다. 필요한 하부 사실이 없으면 L0~L1 실패로 추정하지 않고
coverage 결손으로 남긴다.

## 공통 L2 봉인 조건

각 typed proof는 다음을 모두 보존해야 한다.

1. 정확한 subject transition/history occurrence와 그 `LegalLine` owner
2. 가족 의미로 orientation된 branch와 ordered step occurrence
3. 정확한 기물 identity, 좌표, actor와 target
4. 소비한 모든 L0/L0.5/L1 result 및 source evidence ID
5. 필요한 합법 자원 부재와 상태를 발급한 폐쇄 인벤토리
6. 서로 독립인 모든 proof path
7. semantic proposition과 별도의 branch/history/transposition occurrence
8. subject, branch owner와 root transition, proposition, premise, closure,
   path를 포함한 완전한 record/dependency identity

같은 semantic position의 transposition은 proposition을 공유할 수 있다.
그러나 history occurrence, branch occurrence와 proof path를 합치지 않는다.
같은 actor/target이나 같은 문구를 이유로 서로 다른 증명을 dedup하지 않는다.

L2에는 별도의 결과 캐시나 재사용 권위가 없다. dependency fingerprint는
인증된 proof identity를 닫기 위한 값이지 demand 없이 계산하거나 오래된 결과를
재사용할 권한이 아니다. 평가 score, rank, verdict와 comparison identity는
fingerprint에 들어가지 않는다. 실제 replay, subject transition, lower record,
closed inventory 또는 path가 바뀌면 같은 proof occurrence가 아니다.

## 현재 typed 증명군과 유일 생산자

모든 가족은 explicit occurrence demand 아래서만 실행된다.
`OccurrenceExplanationAssembler`가 유일한 orchestration·graph admission
경계이고, 가족별 체스 명제는 아래 생산자가 직접 인증한다.

| typed 증명군 | 유일 graph/proof 생산자 |
| --- | --- |
| `UniqueCheckReplyDefenderDisplacementBeforeCapture` | `RelationCausalProofAssembler.uniqueCheckReply` |
| `SoleRecapturerRemovalBeforeTargetCapture` | `RelationCausalProofAssembler.soleRecapturerRemoval` |
| `VacatedGateEnablesUnrecapturableSliderCapture` | `RelationCausalProofAssembler.vacatedGateCapture` |
| `SquareReleaseRoute` | `RelationCausalProofAssembler.squareReleaseRoute` |
| `CaptureExclusionMoveOrder` | `OccurrenceExplanationAssembler.captureExclusionRecords`, backed directly by `CaptureExclusionMoveOrderProof.certifyDemanded` |
| `PassedPawnProgressRealizedAfterOnlyLegalReply` | `PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler.fromDemand`; its lower event is solely produced by `PassedPawnResultEventAssembler.fromDemand` |

`RelationCausalProofAssembler`가 공유하는 것은 deterministic graph ownership뿐이다.
각 가족의 private demand가 available root occurrences를 자기 lower facts로
orientation하며 공통 평가 pair나 범용 branch graph는 없다. 상세 명제와
레퍼런스 지지 수준은 canonical provenance 인벤토리에서만 유지한다.

## 생산부터 공개까지

player job의 현재 흐름은 다음과 같다.

1. 실제 engine report에서 합법 replay를 admission하여 role-neutral
   `LegalLine` inventory를 만든다.
2. 그 inventory를 참조하는 Assessment projection을 별도로 만든다.
3. 상위 `ExplanationRequest`가 있으면 exact observed subject를 resolve한다.
4. 여섯 가족의 유일 producer가 등록된 branch와 하부 inventory만 소비하여
   proof를 fail-closed로 생산한다.
5. `OccurrenceExplanationCause`가 exact subject transition owner와 exact
   typed proof owner를 두 parent로 결속한다.
6. `EvidenceBackedJudgmentPacket.occurrenceExplanations`가 인증된 Cause/proof
   쌍만 보존한다.
7. `RuntimeProtocol`이 이를 public-v6의 `occurrence_explanations`로 투영한다.
8. schema와 Python boundary는 ID, enum, shape와 cross-reference를 검증할 뿐
   가족의 체스 논리를 복제하지 않는다.

공개 branch에는 `line_owner_evidence_id`,
`root_transition_evidence_id`, typed root provenance와 ordered steps가 남는다.
outer `subject_occurrence`가 proof의 표시 대상을 한 번만 소유하며, Cause는
line ID만 같다는 이유로 다른 시간 occurrence의 proof를 받아들이지 않는다.

## 계산 범위와 P2 line availability

Explanation branch universe는 이미 실제 producer가 제공하고 합법 replay로
admission된 root/focus line의 합집합이다. 현재 engine work 정책의 MultiPV
상한이나 Assessment의 후보 선택을 L2 계약으로 승격하지 않는다.

필요한 sibling이 inventory에 없으면 새 Stockfish 실행, 별도 analyzer,
추론 replay 또는 테스트 전용 line source를 만들지 않는다. proof는 생산되지
않고 `line unavailable`인 P2 coverage 결손으로 남는다. demand 없는 미계산도
권한 결함이 아니다. 반대로 실제 inventory에 있는 line을 평가 projection에서
제외하기 위해 임의 top-N, score threshold, horizon을 만들 수 없다.

누락된 lower premise나 closed absence도 동일하게 fail-closed다. 테스트 수,
손제작 wire fixture, wrapper·adapter·dedup으로 생산 계약의 결손을 감추지
않는다. L0~L1은 실제 하부 사실 오류가 증명될 때만 다시 연다.

## 소비 경계

Assessment는 무엇이 좋은 수인지 말할 수 있고, typed Explanation은 인증된
occurrence 사이의 좁은 인과 명제만 말할 수 있다. Presentation은 둘을 보여
주지만 어느 쪽의 의미도 새로 만들지 않는다. 일반 준비·예방·반격·기동,
장기 계획, 의도와 가치 판단은 exact 하부 closure와 별도 L2 join이 없으면
현재 증명군의 상위 의미로 승인하지 않는다.

공개 계약·레퍼런스 anchor·지원/비지원 명제·coverage 분류·형식 테스트가
변경되면 반드시
[`l2-proof-provenance.json`](../../judgment-evaluation/references/l2-proof-provenance.json)을
같이 갱신한다. 별도 개념 청사진이나 가족별 문서를 추가하지 않는다.
