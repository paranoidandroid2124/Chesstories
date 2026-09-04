# 체스토리 판단 경계

이 문서는 현재 런타임의 의미 소유권만 기록한다. 레퍼런스 관찰과 현재
커버리지 결손은
[`commentary-reference-ledger.json`](../../judgment-evaluation/references/commentary-reference-ledger.json)이
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

### Occurrence relation

현재 등록된 typed L2 relation producer와 relation demand 경로는 없다. 삭제된
계약의 특수한 조건을 일반 관계로 이름만 바꾸어 남기지 않았으며, 새 관계 모델은
아직 결정하지 않았다. 이 상태는 L0~L1 사실의 부재나 반증을 뜻하지 않는다.
관계 생산 실패가 이미 성립한 하부 사실을 제거할 권한도 없다.

### Structured Presentation

현재 public-v6 move commentary는 Assessment의 `primary`만 운반한다. 삭제된
relation payload를 빈 배열, 범용 wrapper, legacy alias로 유지하지 않는다.
Presentation은 체스 의미를 다시 판정하거나 자유 문장을 별도 진실 원천으로
만들 수 없다.

## Line 소유권과 occurrence provenance

`LegalLineNode`는 합법 재생된 한 root move의 role-neutral owner다. 한 root
move에는 정확히 하나의 `LegalLine` owner만 존재하며, replay와 line evidence를
소유한다. `LineNodeRef`에는 평가 role이나 rank가 없다.

`CandidateLineNode`는 기존 `LegalLine`을 참조하는 Assessment projection이다.
role, rank와 evaluation은 이 projection에만 있다. projection의 선택이나 값이
바뀌어도 같은 replay의 `LegalLine` owner와 root transition identity는 바뀌지
않는다. 반대로 assessment candidate가 없는 line을 평가 사실로 가장하지 않는다.

현재 입력이 실제 대국 occurrence로 확정하는 범위는 observed root 한 수뿐이다.
그 결합은 다음 기존 소유자로 보존된다.

- canonical game history와 정확히 일치하는 root move 및 before/after position
- 그 root의 exact `MoveTransitionEdge` owner
- 그 transition을 시작하는 exact `LegalLine` owner와 canonical replay

따라서 observed 수가 engine best인 경우에도 그 단일 `LegalLine` owner를
별도 played owner로 복제하지 않는다. 다른 root와 모든 PV suffix는 합법 재생된
분석 occurrence일 뿐 실제 대국 수순으로 승격되지 않는다. 현재 이 차이를 별도
provenance ADT나 이름뿐인 wrapper로 다시 복제하지 않는다.

그러나 admission된 PV suffix의 각 ply도 독립 graph occurrence로 실체화된다.
각 suffix position은 정확한 incoming `LineContinuation` transition을 부모로
가지며, 그 transition은 해당 `LegalLine` replay를 부모로 가진다. 따라서 같은
`LegalLine` 안의 cycle이나 서로 다른 admitted `LegalLine`들이 같은 semantic
position으로 합쳐지는 transposition에서도 history occurrence와 evidence ID는
합쳐지지 않는다. 한 `LegalLine` 안의 새 branch나 nested transposition까지
지원한다는 뜻은 아니다. 이 소유권은 분석 occurrence를 observed game history로
승격시키지 않는다.

현재 공개 relation subject는 없다. 상대 응수나 뒤 occurrence를 별도 subject로
요청하는 생산 경로도 지원한다고 주장하거나 빈 API로 예약하지 않는다.

## L0~L2 권한

- **L0**는 한 position occurrence의 폐쇄된 보드 사실과 부재다.
- **L0.5**는 같은 합법 수의 before/after에서 직접 인증되는 정확한 변화다.
- **L1**은 한 transition 직후의 자원·제약과 폐쇄 인벤토리다.
- **L2** typed relation producer는 현재 없다.

L0/L0.5/L1 record는 root뿐 아니라 admission된 replay의 모든 ply occurrence에
demand와 무관하게 생성된다. L2 관계의 생산·인증·공개 실패는 이미 성립한 이
하부 record나 다른 관계를 제거하지 않는다.

평가 변화나 책의 `best`, `only`, `prevents`, `plan`, `counterplay`를 하부 사실로
가장하지 않는다. 필요한 하부 사실이 없으면 미관찰·미검색·반증을 섞지 않고
coverage 결손으로 남긴다. 같은 semantic position의 transposition도 history
occurrence와 proof path를 합치는 근거가 아니다.

## 현재 관계 상태

현재 typed relation family, 전용 proof type, occurrence demand assembler,
packet relation lane, public serializer와 public schema relation variant는 없다.
따라서 관계별 producer·봉인 조건·dependency fingerprint도 현재 권위로 문서화할
대상이 아니다. 레퍼런스 원장의 사례는 관찰과 미해결 근거만 보존한다.

## 생산부터 공개까지

player job의 현재 흐름은 다음과 같다.

1. 실제 engine report에서 합법 replay를 admission하여 role-neutral
   `LegalLine` inventory를 만든다.
2. 각 line의 root와 모든 suffix ply를 독립 position/transition occurrence로
   실체화한다.
3. 모든 transition occurrence에 대해 exact relation delta와 structural record를
   생산한다.
4. 그 inventory를 참조하는 Assessment projection을 별도로 만든다.
5. Assessment 결과를 public-v6 `primary` commentary로 투영한다.
6. schema와 Python boundary는 공개 JSON shape를 검증한다.

`executePreparedReview`는 이 과정에서 완성된 `JudgmentAssemblyContext`를
`ReviewExecution.assembly`에 항상 보존하고, public `primary`는
`ReviewExecution.judgment`의 optional projection으로만 만든다. 선택된
Assessment/comparison chain의 검증 실패는 그 projection만 비우며 assembly나
형제 lower record를 소멸시키지 않는다. 다만 현재 public runtime schema는
primary-only이므로 `judgment`가 없으면 player job은 기존
`ReviewConstructionFailed` stop을 유지한다.

현재 이 흐름에는 relation 생산·packet·serializer·public schema lane이 없다.
그 부재는 이미 성립한 하부 명제를 삭제하거나 의미 부재로 판정하는 gate가
아니다.

## 계산 범위와 P2 line availability

`AdmittedMoveReviewInput.legalRootMoves`는 board에서 생성한 완전한 합법 root
move 집합을 보존한다. `legalLines`는 실제 producer가 제공하고 합법 replay로
admission된 root/focus line의 합집합일 뿐이다. `rootMovesWithoutAdmittedLine`은
전자의 원소 중 후자에 없는 수를 계산한다. 이 상태는 의미 조립에 사용할 line이
없다는 뜻이지, 그 수가 존재하지 않거나 반증되었거나 engine 내부에서 전혀
검색되지 않았다는 뜻이 아니다.

현재 engine work 정책의 MultiPV 상한과 report 수 검사는 runtime의 계산 비용
경계에만 남는다. 의미 admission은 그 상한과 같은 개수의 root line을 요구하지
않으며, MultiPV나 Assessment 후보 선택을 L2 계약으로 승격하지 않는다.
focused report가 서로 다른 best/played line을 직접 제공하면 `PlayedVsBest`는 그
두 line만으로 닫힌다. root ranking의 두 번째 수는 played가 best일 때
`BestVsSecond`를 만드는 경우에만 요구한다.

필요한 sibling이 inventory에 없으면 새 Stockfish 실행, 별도 analyzer,
추론 replay 또는 테스트 전용 line source를 만들지 않는다. proof는 생산되지
않고 `line unavailable`인 P2 coverage 결손으로 남는다. demand 없는 미계산도
권한 결함이 아니다. 반대로 실제 inventory에 있는 line을 평가 projection에서
제외하기 위해 임의 top-N, score threshold, horizon을 만들 수 없다.

누락된 lower premise나 closed absence도 동일하게 fail-closed다. 테스트 수,
손제작 wire fixture, wrapper·adapter·dedup으로 생산 계약의 결손을 감추지
않는다. L0~L1은 실제 하부 사실 오류가 증명될 때만 다시 연다.

## 소비 경계

Assessment는 무엇이 좋은 수인지 말할 수 있지만 occurrence 사이의 관계를
대신 만들 수 없다. Presentation도 공개 payload에 없는 의미를 새로 만들지
않는다. 일반 준비·예방·반격·기동, 장기 계획, 의도와 가치 판단을 현재의
Assessment나 삭제된 계약 이름으로 승인하지 않는다.

레퍼런스 관찰이나 locator가 변경되면 반드시
[`commentary-reference-ledger.json`](../../judgment-evaluation/references/commentary-reference-ledger.json)을
같이 갱신한다. 빈 계약이나 미래 구조 청사진을 추가하지 않는다.
