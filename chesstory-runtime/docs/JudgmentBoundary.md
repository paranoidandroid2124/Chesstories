# 체스토리 판단 권위와 L2 경계

이 문서는 런타임의 안정적인 의미 경계와 현재 봉인된 공개 증명군만
기록한다. 정확한 타입, enum, 직렬화 필드와 admission 조건의 실행
권위는 코드와 JSON Schema다. 평가 실험 절차나 파생 통계를 복제하지
않는다.

## 1. 런타임 권위 흐름

플레이어 경로는 `Q → F → C → Jp → Ja → R → P`다.

| 단계 | 책임 |
| --- | --- |
| Q | canonical history와 focus를 검증하고 필요한 root/focus/causal engine work만 발급한다. |
| F | 합법 재생된 position, line, transition 및 폐쇄 인벤토리를 만든다. |
| C | 등록된 사실에서 원인 후보와 정확한 증명 경로를 조립한다. |
| Jp | 그래프에 결속된 claim 후보를 만든다. |
| Ja | claim 종류별 증명 의무를 통과한 것만 승인한다. |
| R | 승인된 원인 중 공개 집합과 순서를 결정한다. |
| P | 선택된 사실과 증명을 새 의미를 만들지 않고 JSON으로 투영한다. |

브라우저는 서버가 발급한 exact engine profile과 work만 실행하고 line
suffix를 반환한다. work 선택, 합법수 열거, 원인 조립과 공개 승인은
서버가 소유한다. 서버는 Stockfish 프로세스를 실행하지 않는다.

주요 단일 권위는 다음과 같다.

| 사실 또는 결정 | 권위 |
| --- | --- |
| player-use work 원장과 demand | `CommentaryJobReducer` |
| engine suffix의 replay와 admission | `EngineLineAdmission` |
| occurrence별 합법수와 line replay | `PrincipalVariationEvidence` / `CanonicalLineReplay` |
| 한 position의 표준 보드 관계 | `PositionRelationExtractor` |
| 한 transition의 관계 변화 | canonical relation-delta inventory |
| 사실과 provenance 그래프 | `TypedEvidenceGraph` |
| claim 진실 승인 | `ClaimTruthPolicy` |
| 공개 원인 선택 | `ClaimArbitrator`가 호출하는 exposure pipeline |
| 공개 packet과 JSON | `EvidenceBackedJudgmentPacket`과 runtime projection |

표현 계층, 평가 하네스, 별도 analyzer는 위 권위를 대체하거나 보완하는
fallback truth source가 될 수 없다.

## 2. 층의 경계

- **L0**: 한 position occurrence의 점유, 순수 기하 통제, 지원/공격
  투영, 합법수·합법 자원, 킹 안전, ray, pawn topology와 폐쇄 부재다.
- **L0.5**: 동일한 합법 수의 before/after L0 inventory에서 직접
  증명되는 통제자·지원·line·순서 보존 `RayBarrier` topology 변화다.
- **L1**: 한 transition에서 L0/L0.5를 수직 결합한 폐쇄 자원·제약
  사실이다. 포획 뒤 재포획, 체크의 실제 응수 집합, slider reach,
  pawn topology, stalemate 등이 여기에 속한다.
- **L2**: 둘 이상의 ordered occurrence 또는 exact sibling branch의
  인증 결과를 연결하는 bounded causal proof다. 실제 후속 소비,
  상대 자원, 필요한 부재와 반사실이 닫혀야 한다.
- **상위 설명 계층**: 어떤 증명을 보여 줄지 선택하고 문장으로
  표현하며 장기적 가치나 계획을 평가한다. 이것은 L2 증명 권한이
  아니다.

L2는 공격맵, 합법수, ray, 핀, pawn topology를 다시 계산하지 않는다.
체크, 파일 변화, 평가치 변화 같은 단일 신호에서 준비·예방·템포·기동
등의 이름을 승격하지 않는다.

## 3. 공개 사실의 공통 봉인 조건

새 공개 사실은 구현 전에 다음을 모두 답해야 한다.

1. 유일한 생산자는 누구인가.
2. 어떤 보드 명제를 어느 좌표와 기물에 대해 증명하는가.
3. 어떤 dependency 변화에서만 다시 계산하는가.
4. 필요한 부재를 어느 폐쇄 인벤토리가 인증하는가.
5. 실제 상위 소비자는 누구인가.
6. 같은 사실을 다른 모듈이 다시 계산하지 않는가.
7. 여러 아이디어와 독립 증명 경로를 잃지 않는가.
8. transposition의 의미 공유와 발생 경로 보존을 함께 만족하는가.

답할 수 없는 값은 공개 계약으로 만들지 않는다. 미구현 관계는
명시적 결손이며, 증명할 수 없어 비워 둔 결과는 올바른 fail-closed
상태다.

## 4. 봉인된 공통 L2 뼈대

`BoundedCausalProof`는 다음 소유권을 분리한다.

- semantic proposition은 같은 의미 명제를 공유한다.
- occurrence는 actual/counterfactual branch와 ordered root/step
  provenance, 정확한 before/after position을 보존한다.
- 독립 증명은 별도 path occurrence로 남고 각 path가 자기 L0/L1
  premise use와 closed-absence use를 소유한다.
- premise는 하위 result ID, source evidence ID, branch와 step을 잃지
  않는다.
- closed absence는 query, position, scope, 정확한 `LegalLine`과 그
  after-step position occurrence를 보존한다. 특정 L1 결과가 임의의
  위치 부재 발급자로 가장하지 않는다.
- dependency fingerprint는 두 line, 실제 demand, proposition,
  occurrence와 모든 proof path를 포함한다.

동일 semantic position에 도달한 transposition은 proposition을 공유할
수 있지만 서로 다른 history와 branch occurrence를 합치지 않는다.
문장이 같거나 target이 같다는 이유로 proof path를 dedup하지 않는다.

L2 전용 보드 캐시는 두지 않는다. 부재 질의 캐시는 L0의
`PositionRelationInventoryCertificate`가 완전한 query identity로
유일하게 소유하며 L1과 L2가 같은 사실을 다시 판정하지 않는다.
동일 immutable assembly snapshot의 같은
dependency에 인증된 결과가 정확히 하나 있을 때만 파생 전에 재사용한다.
없으면 새로 파생하고 복수 소유자는 fail-closed한다. 하위 dependency가
바뀌면 새 fingerprint로 demand가 실행된다.

## 5. 현재 봉인된 L2 증명군

현재 공통 뼈대를 끝까지 사용하는 증명군은 세 개다.

### 강제 응수에 따른 수비 자원 차이

- 유일 생산자는 `ForcedReplyResourceDifferentialAssembler`다.
- actionable `PlayedVsBest` 비교가 `WrongMoveOrder` 증명을 요구할
  때만 reference/played pair를 계산한다.
- reference의 첫 수가 체크를 만들고 L1 inventory가 유일한 즉시
  응수를 닫으며, 다음 같은 편의 실현수가 정확한 대상을 잡아야 한다.
- `forced_displacement`는 실전에서 재포획한 바로 그 수비 기물이
  reference의 유일 응수로 이동한 경우만 승인한다.
- reference branch에는 해당 수비 기물의 `LegalCaptureOf`가 없고,
  played branch에는 같은 수비 기물의 합법 재포획이 실제 line으로
  있어야 한다.
- 부재는 reference `LegalLine` evidence와 그 line의 정확한 after-step
  position occurrence가 함께 발급자를 소유한다. L1 relation은 체크와
  포획 명제의 premise이지 임의 위치 부재의 별도 진실 원천이 아니다.
- 결과는 `WrongMoveOrder`의 direct proof로 claim 승인·선택을 거쳐
  public v6의 typed `resource_differential_proof`까지 소비된다.

따라서 현재 말할 수 있는 명제는 “기준 수의 유일한 체크 응수가 실전의
재포획자를 정확히 이동시켜 뒤의 실현수를 가능하게 하지만, 실전 순서는
그 자원을 남겨 같은 실현수에 재포획을 허용했다”는 좁은 경우다.

### 유일 재포획자 제거에 따른 수비 의무 변화

- 유일 생산자는 `DefenseObligationChangeAssembler`다.
- actionable `PlayedVsBest`의 exact demand가 있을 때만 같은 reference/played
  sibling pair를 계산한다.
- reference 첫 수는 실전 즉시 exploit의 유일 재포획자를 정확히 잡고,
  상대의 정확히 하나인 재포획으로 되잡혀야 한다. 같은 편의 세 번째 수는
  played 첫 수와 동일한 exploit·대상을 실제로 소비해야 한다.
- reference의 later exploit에는 합법 재포획이 하나도 없어야 하고,
  `LegalCaptureOf(수비측, exploit 칸)`의 폐쇄 부재를 해당
  `LegalLine`의 after-step position occurrence가 발급해야 한다. L1
  `CaptureRecaptureInventory`는 이 결과와 일치하는 포획 premise다.
- played의 immediate exploit에는 제거 전의 동일 기물이 정확히 하나인
  합법 재포획자로 남아 실제 둘째 수로 나타나야 한다.
- 세 L1 inventory, 두 occurrence branch, 관련 기물 identity, exploit과
  재포획 수, exact demand를 dependency fingerprint와 한 proof path에
  보존한다.
- 결과는 `WrongMoveOrder`의 독립 direct proof channel로 선택되어 public
  v6의 typed `defense_obligation_change_proof`까지 소비된다.

따라서 이 증명군은 일반 과부하나 일반 수비 붕괴를 말하지 않는다.
“기준 순서가 실전의 유일 재포획자를 먼저 제거했기 때문에 뒤의 동일
exploit에는 대체 재포획이 없지만, 실전 즉시 exploit에는 그 수비자가
남아 있다”는 명제만 승인한다.

### 폐쇄 응수 아래 통과폰 결과

- 유일 생산자는 `PassedPawnResultProofAssembler`다.
- exact `PlayedVsBest` demand가 만든 passed-pawn-result event 중 통과폰 진행의 실제
  후속 결과가 있는 endpoint만 계산한다.
- root 뒤의 전체 합법 응수는 structural-delta의 canonical reply
  inventory가 발급하고 passed-pawn-result event가 every-and-only branch coverage를
  인증한다. 각 응수 branch에서는 같은 기능의 통과폰 결과가 실제 legal
  occurrence로 실현돼야 한다.
- expected route와 각 reply route는 exact dependency·goal·functional
  match premise를 별도 proof path로 보존한다. 여러 실현 경로는 하나로
  줄이지 않는다.
- 원시 passed-pawn-result event는 내부 premise와 probe 소유자일 뿐이다. 공개
  `PassedPawnResult` 원인과 typed `passed_pawn_result_proof`는 위 L2 결과만
  소비한다.

따라서 이 증명군은 “현재 수가 만든 정확한 통과폰 관계를 뒤의 같은
편 수가 실제로 소비했고, 모든 합법 즉시 응수에서 그 결과 occurrence까지
발급된 정확한 canonical 수순 범위 안에 성립했다”까지만 말한다. 이 proof는
source root의 결과만 인증하며 played sibling의 부재나 `missed`를 주장하지
않는다. 장기적 승리, 최선의 계획, 의도나 일반적 준비도 주장하지 않는다.

현재는 일반 과부하, 일반 간섭, 모든 합법 repair까지 닫힌 수비 붕괴,
교환·이동에 의한 일반 수비 의무 이전, 지연된 준비, 예방, 반격,
기동을 증명하지 않는다. 대체 수비자와 repair 전체, intervening
dependency와 실제 후속 소비가 닫히기 전에는 이 이름들을 만들지 않는다.

## 6. 공개와 평가의 분리

player response는 선택된 verdict와 원인이 소유한 증명만 투영한다.
typed L2 proof는 branch occurrence, proof paths, exact premises,
closed absence, 관련 기물과 realizing/defense move를 보존한다. 공개
계층은 actor/mechanism 문구나 confidence로 빈 필드를 채우지 않는다.

개발용 stage intervention, corpus 봉인, 통계와 release 절차는
[`../../judgment-evaluation/README.md`](../../judgment-evaluation/README.md)의
범위다. 평가 결과는 런타임 사실 생산자나 L2 증명 권한이 아니다.
