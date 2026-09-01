# 체스토리 판단 권위와 L2 경계

이 문서는 런타임의 안정적인 의미 경계와 현재 봉인된 공개 증명군만
기록한다. 정확한 타입, enum, 직렬화 필드와 admission 조건의 실행
권위는 코드와 JSON Schema다. 평가 실험 절차나 파생 통계를 복제하지
않는다.

## 1. 런타임 권위 흐름

플레이어 경로는 `Q → F → C → Jp → Ja → R → P`다.

| 단계 | 책임 |
| --- | --- |
| Q | canonical history와 focus를 검증하고 기존 PV/MultiPV에 필요한 engine work만 발급한다. |
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

### 인간 이론서 검증 게이트

체스 아이디어의 의미 요건은 인간 이론서에서 가져오되, 책의 표현이
런타임 사실 권위가 되지는 않는다. 새 L2 증명군은 구현 전에 정확한
책·PDF page/example, 그 예가 연결하는 선행 수·후속 소비·상대 자원과
대안, 필요한 하위 premise를 함께 기록해야 한다. 이름만 닮았거나
평가치가 좋아졌다는 이유로 계약을 만들 수 없다.

source identity는
[`source-index.json`](../../judgment-evaluation/references/source-index.json)이
소유한다. 그 document locator의 page는 모두 1-based다. 표의 외부 용어집은
URL로 따로 식별하며, 수순과 현재 계약의 의미 상한을 함께 보존한다.
활성 증명군과 생산자·공개 필드·형식 테스트·anchor의 대응은
[`l2-proof-provenance.json`](../../judgment-evaluation/references/l2-proof-provenance.json)이
CI에서 전수 대조한다. 이 manifest는 레퍼런스 provenance 게이트일 뿐이며
런타임 보드 사실, dispatch 또는 캐시 dependency에는 참여하지 않는다.

| 현재 증명군 | 레퍼런스 검증 수준 | anchor | 코드가 추가로 요구하는 형식적 충분조건 |
| --- | --- | --- | --- |
| `UniqueCheckReplyDefenderDisplacementBeforeCapture` | **exact ordered skeleton** | Chess.com, [*Deflection*](https://www.chess.com/terms/deflection-chess): `Bxf7+`의 유일 합법 응수 `Kxf7` 뒤 queen 방어가 사라지는 예. `ref-bc7b3f1ff52f4c7a9940dad53cd6b975`, p.178의 `13.Qa4+! ... 14.Qxb4`는 forcing/zwischenzug **motif-only** 보조 anchor다. | 동일한 유일 응수자가 played sibling의 정확한 재포획자이고, reference 후속 포획에는 합법 재포획이 없을 때만 승인한다. |
| `SoleRecapturerRemovalBeforeTargetCapture` | **exact ordered skeleton + 책의 motif-only breadth** | Chess.com, [*Removing the Defender*](https://www.chess.com/terms/removing-the-defender-chess)는 rook이 유일 수비자를 잡고 상대가 그 rook을 재포획한 뒤 원래 target의 보호가 사라져 실제로 material을 얻는 구조를 명시한다. `ref-4f1abd0745b24746b58928d162f837f6`, pp.154–156; `ref-7a451d5c87b9422a901f0486386aeb44`, pp.269–272; `ref-dc427a9593ab48248d19627b245fafaa`, p.131은 defender displacement/removal의 **motif-only** 전략적 폭이다. | 일반 과부하가 아니라, sibling의 유일 재포획자를 먼저 제거하고 상대 재포획 뒤 같은 exploit이 실제로 소비되며 대체 재포획자가 없을 때만 승인한다. |
| `VacatedGateEnablesUnrecapturableSliderCapture` | **motif-only; full contract exact example은 아직 없음** | `ref-4f1abd0745b24746b58928d162f837f6`, pp.145–147의 `47.Rxc5 bxc5 48.Qh4`와 pp.153–156의 열린 g-file/diagonal; `ref-7a451d5c87b9422a901f0486386aeb44`, pp.270–272의 e-file/e7 진입은 line opening/invasion 의미를 검증하지만 현재 계약 전체의 실행 예는 아니다. | 일반 파일 개방이나 계획이 아니라, 정확한 gate 이탈·같은 slider의 reach 유지·뒤의 실제 포획·played sibling의 닫힌 부재를 모두 증명하는 보수적 formal strengthening이다. 이를 책이 그대로 검증한 계약이라고 부르지 않는다. |
| `SquareReleaseRoute` | **one-leg와 multi-leg terminal의 exact ordered skeleton** | `ref-dc427a9593ab48248d19627b245fafaa`, pp.130–131의 P.Prohaszka–K.Wang은 `30.Bg2! ... 31.Rc6!`와 `30.R1d2?` sibling을 함께 주는 one-leg exact 사례다. 같은 책 pp.124–125의 Goryachkina–Dubov는 `...Rc6`가 b6을 비운 뒤 동일 knight가 `d5-b6-c4-a3-b1`로 이동해 `Nxb1`을 실행하고 실제 `Rxb1`이 이어지는 capture-terminal exact 사례다. `ref-7a451d5c87b9422a901f0486386aeb44`, pp.57–58은 `b4`가 b3을 비운 뒤 동일 knight가 `a5-b3-c1-d3-f4`로 와 `Nf4+`를 실행하고 실제 `Kf7`이 이어지는 check-terminal exact 사례다. 이 체크에는 합법 응수가 다섯 개이므로 유일·강제라고 부르지 않는다. `ref-4f1abd0745b24746b58928d162f837f6`, pp.160–164, `ref-bc7b3f1ff52f4c7a9940dad53cd6b975`, pp.379–382, `ref-03c77bd9c6c6ae987e07067822fd63b`, pp.130–132는 route·timing의 **exact component/motif**만 제공한다. | release부터 첫 route leg까지의 vacancy, 같은 physical mover의 모든 route endpoint와 intervening `OccupiedBy`, exact terminal L1 capture/check, 필요한 실제 다음 응수, sibling의 blocker·route-origin 연속 점유와 첫 leg 부재가 모두 있어야 한다. retained interval 밖의 기물 identity, 최선성·유일 원인·준비·기동·outpost·계획은 승인하지 않는다. |
| `PassedPawnProgressRealizedAfterOnlyLegalReply` | **exact route component; singleton reply closure는 runtime strengthening** | `ref-dc427a9593ab48248d19627b245fafaa`, pp.507–508의 `37.Nxa6 ... 39.a6 ... 41.a8=Q`는 실제 통과폰 진행·승격 occurrence를 검증한다. 책이 root의 합법 응수 인벤토리를 발급한 것은 아니다. | root의 합법 응수가 정확히 하나임을 canonical inventory가 인증하고, observed played root와 그 인증 분석 continuation에서 뒤의 통과폰 진행 결과 occurrence가 발급될 때만 승인한다. 승리·의도·장기 계획이나 다른 sibling 실패는 말하지 않는다. |

`exact example`은 인간 자료가 계약의 핵심 수순 구조까지 보여 준다는 뜻이고,
`motif-only`는 이름과 인과 방향의 의미 상한만 제공한다는 뜻이다. 후자의
formal strengthening은 코드에서 정확할 수 있지만, 그 자체를 책으로 검증된
새 체스 명칭으로 승격하지 않는다. exact source example이 없는 상태도 이 표에
그대로 남겨야 하며, 문서 문구로 검증 완료를 가장하지 않는다.

vacancy라는 단어만 나오는 예도 positive anchor로 승격하지 않는다.
`ref-b7e552e8d8914cdc31ce1002fbb8252e`, p.52의 e6은 해당 수 전에 이미
비어 있고, `ref-7a451d5c87b9422a901f0486386aeb44`, p.619는 terminal
소비와 sibling closure가 없는 diagonal clearance다.
`ref-bc7b3f1ff52f4c7a9940dad53cd6b975`, p.325는 선택되지 않은 도착지이고,
`ref-dc427a9593ab48248d19627b245fafaa`, p.130의 `27.e4`는 e3을
비우지만 뒤의 `Be3` 점유 occurrence가 없다. 이들은 명칭이 아니라 필요한
후속 소비가 빠진 적대 경계다.

향후 outpost·기동의 기준은 `ref-4f1abd0745b24746b58928d162f837f6`
pp.149–169와 `ref-7a451d5c87b9422a901f0486386aeb44` pp.257–269다.
예방의 핵심 반례는 `ref-b7e552e8d8914cdc31ce1002fbb8252e` p.33의
`10.a4` 뒤에도 남는 `...Nb6`이며, 구체적 활성 자원은 pp.37–38에 나온다.
반격의 기준은 `ref-dc427a9593ab48248d19627b245fafaa` p.507의
`32...Rf4!`처럼 상대 계획과 별개의 자원 및 시간 순서를 함께 보이는
경우다. 이 premise들이 닫히기 전에는 해당 L2 이름을 만들지 않는다.

따라서 레퍼런스는 motif 목록을 복사하는 재료가 아니라 구현이 말해도
되는 최대 의미의 상한이다. 코드가 이 연결을 닫지 못하면 그 아이디어는
아직 미구현이다.

## 4. 봉인된 공통 L2 뼈대

`BoundedCausalProof`는 다음 소유권을 분리한다.

- semantic proposition은 같은 의미 명제를 공유한다.
- occurrence는 actual/counterfactual branch와 ordered root/step
  provenance, 정확한 before/after position을 보존한다.
- step index·ply·before/after position의 직접 연속성이 occurrence 연결을
  인증한다. 이를 재진술하는 별도 `incomingLink`나 자체 발급 link hash는 없다.
- 독립 증명은 별도 path occurrence로 남고 각 path가 자기 L0/L1
  premise use와 closed-absence use를 소유한다.
- premise는 하위 result ID, source evidence ID, branch와 step을 잃지
  않는다.
- closed absence는 query, position, scope, 정확한 `LegalLine`과 그
  after-step position occurrence를 보존한다. 특정 L1 결과가 임의의
  위치 부재 발급자로 가장하지 않는다.
- dependency fingerprint는 증명에 실제로 쓰인 line owner, proposition,
  occurrence, 모든 proof path와 하부 premise/closure identity만 포함한다.
  비교 점수·verdict·confidence와 demand record는 포함하지 않는다.

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

`PlayedVsBest` 비교는 계산과 공개를 요청하는 상위 dispatch gate다. 이
gate는 non-actionable 비교에서 생산을 생략할 수 있지만 typed L2의
proposition, premise, parent, dependency, proof ID가 될 수 없다. Cause는
완성된 평가와 평가와 무관하게 봉인된 typed proof를 별도 parent로 결속해
무엇을 노출할지 결정한다.

관계 기반 세 증명군의 predispatch는 canonical L1 producer의 활성
contract key만 읽고 결과를 미리 materialize하지 않는다. 네 번째
compared-line 증명군인 square-release route는 이미 인증된 canonical replay의
exact legal-move occurrence만 순회하며, played sibling이 대응 pre-use
occurrence를 가질 수 없는 reference suffix는 seed materialization 전에
제외한다. 어느 경로도 새 합법수나 보드 관계를 계산하지 않는다. vacancy
route의 공석 closure는 reference occurrence를
한 번만 전진하며 최초 non-vacant state에서 닫히고, 각 position inventory의
완전한 `Vacant(square)` query cache를 재사용한다. occupation seed마다 앞 수순을
다시 전수 검사하지 않는다. 통과폰 증명군은
root `StructuralDelta`의 canonical 응수 인벤토리가 singleton이고 그 수가
main replay의 첫 응수와 같은지를 먼저 검사한다. 이 cheap gate를 통과한
경우에만 main replay의 `PassedPawnProgress` changed occurrence와 causal
episode를 materialize한다. 구조 결과가 없는 합법 전이도 동일한
`StructuralDeltaProducer`가 빈 폐쇄 인벤토리로 인증한다. 상위 층은 이
값을 "미계산"과 구별할 수 있지만 새로운 보드 계산은 하지 않는다.

### 구조 감사 결과

- **P0는 발견되지 않았다.** L0~L1을 다시 열어야 할 보드 사실 오류도
  발견되지 않았다.
- **P1 평가 오염은 제거했다.** 다섯 typed L2의 proposition, premise,
  parent, dependency/cache와 proof ID에서 `PlayedVsBest` record 및
  score/verdict/confidence를 제거했다. 비교는 dispatch와 상위 Cause
  결속에만 남는다.
- **P1 occurrence 손실은 제거했다.** 여러 exact proof record를 한 Cause
  draft로 합치던 경로를 record별 draft로 분리했다. 한 proof record 안의
  독립 path는 그대로 한 proof set에 남는다.
- **P1 graph line-reference 누락은 제거했다.** square-release payload가
  두 branch를 보존하면서도 공통 `payloadLineRefs`에 등록되지 않아 played
  line 기준 조회에서는 해당 증명이 사라질 수 있었다. 다른 compared-line
  L2와 동일하게 reference와 played line을 모두 등록하고 양쪽 조회를
  회귀 테스트로 봉인했다.
- **P1 provenance 과장은 제거했다.** played root만 관측 수이고 모든 PV
  suffix는 `certified_analysis_move`다. 공개 이름도
  `played_root_analysis_continuation`으로 통일하여 전체 suffix를 실제
  게임이라고 부르지 않는다.
- **P1 공개 exposure 교차곱은 제거했다.** 생산 계약과 동일하게
  `WrongMoveOrder`·`MissedTacticalResource`·`MissedSquareRelease`는 `primary`,
  `PassedPawnProgress`는 `complementary`만 schema·Python·UI에서
  허용한다.
- **P1 wire enum 왜곡은 제거했다.** 하위 `ClosedLegalMovementMode`를
  투영 전에 단순 소문자화하여 `ControlledDestination`을
  `controlleddestination`으로 만들던 경로를 없앴다. Runtime의 단일 enum
  serializer가 공개 계약의 `controlled_destination`을 발급하고 정확한 wire
  회귀 테스트가 이를 봉인한다.
- **P1 causal occurrence 오결속은 제거했다.** 최초 release 뒤 칸이 다시
  점유·재이탈한 경우의 두 번째 occupation을 최초 수의 결과로 붙이던 반례를
  재현했다. 이 증명된 P1 때문에 L0~L1 봉인을 최소 범위로 다시 열어, 기존
  closed position inventory에 `Vacant(square)` 한 query를 추가했다. L2는
  release 뒤 occupation 직전까지 모든 occurrence가 발급한 exact vacancy
  state를 소비하며, 고정 horizon으로 결손을 숨기지 않는다.
- **P1 actor/target identity 누락은 제거했다.** unique-check와
  sole-recapturer 가족이 pre-realizer/pre-exploit 구간의 actor·target을
  transition footprint guard로만 확인하던 경로를 없앴다. 각 after-step의
  exact `OccupiedBy` authority를 premise manifest, proof path와 dependency에
  보존하고 재인증한다. 누락되거나 query role이 바뀐 certificate는 fail-closed다.
- **P1 sibling endpoint 대리는 제거했다.** gate와 square-release 가족이
  first-use 직전 한 시점의 blocker/mover만 확인하면 중간 이탈·복귀를 놓칠
  수 있었다. 공통 `VacancySiblingClosure`가 played branch의 모든 선행
  occurrence에서 exact blocker와 first-route origin의 `OccupiedBy`를 요구한다.
- **P1 multi-occurrence 재인증 거짓 음성은 제거했다.** 같은 route를 다시
  인증할 때 내부 proof 객체의 JVM 동일성까지 요구하던 비교를 제거했다.
  LegalLine issuer, root/future/intervening occurrence, side·role·square와 각
  persistence occurrence/proof ID를 전부 포함한 trajectory key로 재결속한다.
  이 키는 dependency에도 그대로 들어가며 축약 horizon이나 부분 캐시 키가 아니다.
- **P1 public identity 분리는 제거했다.** 문법상 유효한 top-level move와
  participant movement, branch step, premise role/index, closure role/query를
  서로 다른 좌표로 위조할 수 있던 transport 경로를 family별 structural
  binder로 닫았다. capture terminal의 recapture도 UCI와 좌표를 함께 바꾼
  경우 final route square와 captured mover identity가 어긋나면 거부한다.
  Python과 UI는 이 ID·좌표 등식만 검사하고 보드 사실을 다시 계산하지
  않는다.
- **P1 가변 occurrence 절단은 제거했다.** VacatedGate UI가 state inventory를
  항상 다섯 개로 가정해 늦은 exploit의 합법 proof를 버리던 경로를
  terminal premise의 exact step index `k`와 `k+3` state occurrence로 결속했다.
  고정 horizon이나 최소 개수만으로 복잡도를 숨기지 않는다.
- **P1 의미 없는 분류 이름은 제거했다.** 단기 exact cause를 `Tactical`과
  단일-use `Causal`로 갈라 상위 체스 의미를 암시하지 않고, 실제 공통
  검증 경계를 뜻하는 `BoundedCausal` claim family 하나로 묶었다.
- **P1 가족별 공통 계약 재구현을 줄였다.** compared-line branch role,
  public premise/path/closure projection과 vacancy sibling 하한은 공통 owner가
  맡는다. 네 assembler가 반복하던 cache-owner 조회, semantic/occurrence 충돌
  거부, canonical parent/path와 `EvidenceRecord` 발급도
  `ExactCausalProofOwnerReuse.comparedLineRecords` 한 경계로 모았다. 가족별
  object는 서로 다른 체스 명제의 exact derivation과 typed payload만 보유한다.
- **P1 생산자 소유권 혼재를 제거했다.** 이전 임시 공용 파일 한
  파일에 두 독립 생산자를 두지 않는다. gate와 square-release는 각각 한
  assembler 파일·object만 가지며 alias, wrapper, legacy producer는 없다.
- **P2 dead surface는 축소했다.** PassedPawn 전용 공개 line/provenance와
  issuer/reply scope는 실제 생산 값인 `played`, `observed_game_root`,
  `played_line`, `played_transition`만 노출한다. 다른 family가 생기기
  전에는 reference/alternative 값을 미리 열지 않는다.
- **P2 predispatch 과잉 계산은 제거했다.** square-release route는 played
  sibling이 필요한 pre-use occurrence를 보유한 reference index까지만 exact
  legal-move seed를 materialize한다. 이는 top-N이나 horizon이 아니라 typed
  proof의 필수 branch cardinality로 닫힌 demand 범위다.
- **P2 identity recipe 중복은 제거했다.** 네 compared-line family가 각각
  만들던 `side+role@square` 기물 키를 공통 `BoundedCausalIdentity` 한 곳으로
  옮겨 semantic/dependency ID의 다중 권위를 없앴다.
- **P2 표현 커버리지는 보강했다.** 실제 projector가 있던 VacatedGate를
  영어·한국어 view 경로에서도 직접 렌더하고, square-release도 exact
  release/ordered route/terminal/reply와 premise/absence/state를 직접 소비한다. 이것은
  생산 권한을 대신하는 expected logic가 아니라 이미 승인된 wire의 소비
  회귀 테스트다.

이번 변경은 새 board cache, full recomputation을 delta라고 부르는 경로,
top-N·고정 horizon·점수 threshold·임의 radius, 별도 engine/analyzer,
legacy adapter나 두 번째 producer를 추가하지 않는다. 공통
`BoundedCausalProof`/`ExactCausalProofOwnerReuse`가 identity, occurrence,
path set, closure와 owner reuse를 소유하고, 가족별 코드는 exact admission
관계만 소유한다. 두 vacancy family는 `VacancySiblingClosure`에서 exact root
release, played blocker/mover state와 같은 from/to move absence의 공통 하한만
공유한다. slider geometry와 destination occupation은 각 typed family가 따로
소유한다. 따라서 현재 감사에서 공통 causal-proof kernel을 새로 재구현한
P0/P1 중복은 발견되지 않았다.

## 5. 현재 봉인된 L2 증명군

현재 공통 뼈대를 끝까지 사용하는 증명군은 다섯 개다.

### `UniqueCheckReplyDefenderDisplacementBeforeCapture`: 후속 포획 전에 강제된 수비자 이동

- 유일 생산자는 `UniqueCheckReplyDefenderDisplacementBeforeCaptureAssembler`다.
- actionable `PlayedVsBest` 비교가 `WrongMoveOrder` 증명을 요구할
  때만 reference/played pair를 계산한다.
- reference의 첫 수가 체크를 만들고 L1 inventory가 유일한 즉시
  응수를 닫으며, 다음 같은 편의 실현수가 정확한 대상을 잡아야 한다.
- 실전에서 재포획한 바로 그 수비 기물이
  reference의 유일 응수로 이동한 경우만 승인한다.
- reference branch에는 해당 수비 기물의 `LegalCaptureOf`가 없고,
  played branch에는 같은 수비 기물의 합법 재포획이 실제 line으로
  있어야 한다.
- 부재는 reference `LegalLine` evidence와 그 line의 정확한 after-step
  position occurrence가 함께 발급자를 소유한다. L1 relation은 체크와
  포획 명제의 premise이지 임의 위치 부재의 별도 진실 원천이 아니다.
- 결과는 `WrongMoveOrder`의 direct proof로 claim 승인·선택을 거쳐
  public v6의 typed `unique_check_reply_defender_displacement_before_capture_proof`까지 소비된다.

따라서 현재 말할 수 있는 명제는 “기준 수의 유일한 체크 응수가 실전의
재포획자를 정확히 이동시켜 뒤의 실현수를 가능하게 하지만, 실전 순서는
그 자원을 남겨 같은 실현수에 재포획을 허용했다”는 좁은 경우다.

### `SoleRecapturerRemovalBeforeTargetCapture`: 대상 포획 전에 이루어진 유일 재포획자 제거

- 유일 생산자는 `SoleRecapturerRemovalBeforeTargetCaptureAssembler`다.
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
  재포획 수를 dependency fingerprint와 한 proof path에 보존한다. exact
  demand는 이 생산을 여는 dispatch gate일 뿐 증명 입력에는 들어가지 않는다.
- 결과는 `WrongMoveOrder`의 독립 direct proof channel로 선택되어 public
  v6의 typed `sole_recapturer_removal_before_target_capture_proof`까지 소비된다.

따라서 이 증명군은 일반 과부하나 일반 수비 붕괴를 말하지 않는다.
“기준 순서가 실전의 유일 재포획자를 먼저 제거했기 때문에 뒤의 동일
exploit에는 대체 재포획이 없지만, 실전 즉시 exploit에는 그 수비자가
남아 있다”는 명제만 승인한다.

### `VacatedGateEnablesUnrecapturableSliderCapture`: 비워진 gate와 같은 slider의 후속 포획

- 유일 생산자는 `VacatedGateEnablesUnrecapturableSliderCaptureAssembler`다.
- actionable `PlayedVsBest`가 있고 reference root의 L1
  `SliderReachDelta` 및 뒤의 L1 `CaptureRecaptureInventory`가 활성인
  occurrence index만 demand한다.
- reference root mover가 slider 앞의 정확한 gate를 비우고, 같은 slider가
  intervening occurrence마다 그 reach를 유지한 뒤 정확한 상대 기물을
  실제로 포획해야 한다. 포획 직후 상대의 합법 재포획은 없어야 한다.
- played sibling은 exploit 전까지 같은 continuation을 보존하면서 slider,
  target, gate blocker의 정확한 점유와 blocked reach를 유지해야 한다.
  그 위치에는 reference exploit move와 같은 target에 대한 대체 합법
  capture가 모두 없어야 한다.
- 두 L1 premise, 모든 중간 positive state, 세 폐쇄 부재, 두 branch와
  실제 later capture를 occurrence와 dependency fingerprint에 보존한다.
- 결과는 `MissedTacticalResource`의 독립 direct proof로 승인·선택되어
  public v6의 typed `vacated_gate_enables_unrecapturable_slider_capture_proof`까지 소비된다.

따라서 이는 일반적인 “파일을 열었다”나 장기 계획을 뜻하지 않는다.
“reference root가 비운 정확한 gate를 같은 slider의 뒤 포획이 실제로
소비하며, played sibling에서는 blocker가 남아 그 합법 자원이 닫혀
있다”는 capture-only causal structure만 말한다.

### `SquareReleaseRoute`: release를 소비하는 bounded same-object route

- 유일 생산자는 독립 파일의 `SquareReleaseRouteAssembler`다. gate 가족과
  seed, result 또는 owner를 변환하지 않으며 alias·adapter·보조 producer가 없다.
- actionable `PlayedVsBest` demand가 있을 때만 canonical reference replay의
  root release와 step 2 이후 첫 occupation 후보를 materialize한다. 그 뒤
  lower `LineObjectTrajectory`의 first-next-movement authority를 따라 retained
  line 전체에서 같은 physical mover를 잇고, 첫 exact L1 capture 또는
  created-check terminal에서 닫는다. top-N, 고정 horizon, score threshold와
  별도 board/analyzer는 사용하지 않는다.
- root movement는 exact blocker의 칸 `S`를 떠나야 하고, 첫 route leg는
  같은 편의 non-capture로 `S`에 들어와야 한다. release 뒤 첫 leg 직전까지
  모든 reference after-step은 `Vacant(S)`를 발급한다.
- 각 route leg는 exact legal-move semantic ID, issuer occurrence와 source
  premise ID를 보존한다. 모든 endpoint의 `OccupiedBy`와 두 leg 사이 모든
  after-step의 `OccupiedBy` persistence가 같은 side·role·square movement
  chain을 닫는다. 이는 retained interval 안의 physical mover identity를
  증명하지만 전역·무제한 piece token은 아니다.
- one-leg `Occupation`은 terminal L1이 없는 degenerate route다. multi-leg
  `Capture`는 exact `CaptureRecaptureInventory`, `CreatedCheck`는 exact
  `CreatedCheckResponseInventory`를 final leg에 결속한다. capture와 ongoing
  check는 바로 다음 actual legal reply occurrence까지 보존하고, ongoing
  check의 reply는 exact response membership을 요구한다. checkmate만 reply가
  없다. capture가 unrecapturable이거나 check reply가 singleton이라는 더
  강한 조건은 요구하거나 주장하지 않는다.
- played sibling은 first route leg 직전까지 매 occurrence에서 exact blocker와
  route-origin mover를 계속 점유해야 한다. 마지막 시점만 맞는 중간
  이탈·복귀는 거부한다. 같은 first leg `LegalMoveFromTo(F,S)`의 폐쇄 부재는
  그 마지막 pre-leg position inventory가 발급한다. 두 branch의 중간 응수가
  같아야 한다는 휴리스틱은 없다.
- reference branch는 terminal 또는 필요한 reply까지만,
  `played_root_analysis_continuation`은 first leg 직전까지만 보존한다.
  semantic proposition이 같아도 line/transposition history가 다르면 occurrence를
  합치지 않는다. 같은 proposition·branch occurrence를 지지하는 독립 terminal
  L1 derivation은 한 proof set의 별도 path로 남고 모든 path ID가 dependency에
  들어간다.
- dependency fingerprint는 두 LegalLine owner, proposition, branch occurrence,
  모든 path와 route trajectory의 완전한 occurrence/state key를 포함한다.
  평가·verdict·demand ID는 실행 gate일 뿐 premise나 cache key가 아니다.
- 결과는 `MissedSquareRelease`의 primary direct proof로 Cause 승인·선택을
  거쳐 Runtime/public-v6의 ordered `route`, typed `terminal`, optional
  `terminal_reply_move`와 `projectSquareReleaseRoute`까지 실제 소비된다.

따라서 현재 승인 가능한 명제는 “reference root가 exact square를 비웠고,
sibling에서는 첫 leg가 닫힌 채 blocker와 route-origin이 계속 남아 있지만,
reference에서는 같은 physical mover가 ordered route로 그 square를 사용한 뒤
exact capture/check resource를 실제로 실행했다”까지다. 이 구조를 인간의
‘기동’, ‘준비’, ‘outpost’, ‘예방’ 또는 좋은 계획으로 부르는 것, release가
유일 원인이라는 것, retained interval 밖의 identity와 장기 가치는 승인하지
않는다.

### `PassedPawnProgressRealizedAfterOnlyLegalReply`: 유일 합법 응수 뒤의 통과폰 진행

- 유일 생산자는 `PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler`다.
- exact `PlayedVsBest` demand가 선택한 played endpoint 중 통과폰 진행의
  인증 분석 후속 결과가 있는 경우만 계산한다. demand가 event나 typed
  proof의 ID·parent·premise가 되지는 않는다.
- root 뒤의 합법 응수가 정확히 하나라는 사실은 structural-delta의
  canonical reply inventory가 발급한다. 현재 생산자는 그 수가 기존 main
  PV의 첫 응수와 동일한 경우에만 changed occurrence와 episode를 만들며,
  필요한 replay 범위가 모두 인증된 경우만 witness를 만든다. 다중 응수나
  별도 probe 경로는 없다.
- 공개 occurrence는 observed played root와 그 뒤의 인증 분석 응수·결과를
  잇는 단 하나의 `played_root_analysis_continuation`이다. 각 graph-owned
  dependency route와 terminal result가 한 proof path를 이루며, 동일
  occurrence에 독립적인 하위 dependency 경로가 여러 개면 어느 것도
  dedup하지 않고 공통 proof set에 모두 보존한다.
- 원시 passed-pawn-result event는 내부 premise일 뿐이다. 공개 원인과 typed
  L2 proof는 닫힌 유일-응수 occurrence만 소비한다.
- 현재 상위 소비자는 actionable `PlayedVsBest`에서 실전 수가 실제로 만든
  결과를 `candidate`/`played_value`의 complementary facet으로만 선택한다.
  평가 손실의 원인이나 reference에서 놓친 자원으로 바꾸어 말하지 않는다.

따라서 이 증명군은 “observed root와 유일한 합법 즉시 응수, 뒤의 인증 분석
실현 수가 하나의 canonical continuation에 있고, 그 경로의 정확한 하위
dependency가 terminal 통과폰 진행 결과에 도달했다”까지만 말한다. 이
proof는 played root 뒤 분석 continuation의 결과만 인증하며 sibling의 부재나 `missed`를
주장하지 않는다. 장기적 승리, 최선의 계획, 의도나 일반적 준비도 주장하지
않는다.

### 공개 계약 전수 인벤토리

아래 표는 공개 다섯 계약에 대해 생산자, 정확한 premise ID, 폐쇄 부재,
branch와 실제 소비를 한곳에 고정한다. premise ID는 평가 비교 ID가 아니라
각 proof path가 보존하는 하위 occurrence/result ID다.

| 계약 | 좌표·기물 명제와 하위 premise | 폐쇄 인벤토리 | branch·path·dependency | 유일 생산자와 실제 소비자 |
| --- | --- | --- | --- | --- |
| `UniqueCheckReplyDefenderDisplacementBeforeCapture` | exact trigger/forced-reply/realizer/captured-target/disabled-defender와 `created-check-response`, `reference-capture-recapture`, `played-capture-recapture` result ID | reference exploit 뒤 `LegalCaptureOf` 부재 | `counterfactual_reference`와 `played_root_analysis_continuation`; line owner·세 premise·absence·모든 path/occurrence가 fingerprint를 구성하며 평가 demand는 제외 | 전용 assembler → `WrongMoveOrder` Cause → Runtime/public-v6 → `projectUniqueCheckReplyDefenderDisplacementBeforeCapture` |
| `SoleRecapturerRemovalBeforeTargetCapture` | exact remover/removed-defender/removal-recapture/exploit/target/played-recapture와 `reference-defender-removal`, `reference-later-exploit-inventory`, `played-immediate-exploit-inventory` result ID | reference later exploit 뒤 replacement `LegalCaptureOf` 부재 | 같은 두 branch; line owner·세 premise·absence·모든 path/occurrence만 fingerprint에 포함 | 전용 assembler → `WrongMoveOrder` Cause → Runtime/public-v6 → `projectSoleRecapturerRemovalBeforeTargetCapture` |
| `VacatedGateEnablesUnrecapturableSliderCapture` | exact gate mover/slider/target/later capture와 `reference-root-slider-reach`, `reference-exploit-capture` result ID; reference target 및 played slider/target/gate의 모든 pre-exploit `OccupiedBy` | reference immediate recapture, played exact exploit move, played replacement capture의 세 폐쇄 부재 | 같은 두 branch; 두 premise·모든 연속 positive state·세 absence·모든 path/occurrence와 line owner가 fingerprint를 구성 | `VacatedGateEnablesUnrecapturableSliderCaptureAssembler` → `MissedTacticalResource` Cause → Runtime/public-v6 → `projectVacatedGateEnablesUnrecapturableSliderCapture` |
| `SquareReleaseRoute` | exact release·ordered route legal-move semantic/issuer/source premise ID; optional terminal capture/check result ID; 모든 pre-first-leg `Vacant(S)`, route endpoint·gap persistence와 played blocker/route-origin `OccupiedBy` ID | played first-leg 직전 exact `LegalMoveFromTo(F,S)` 부재 | reference는 terminal/reply까지, played는 first leg 직전까지; 모든 path와 complete trajectory key, occurrence 및 두 line owner가 fingerprint를 구성 | `SquareReleaseRouteAssembler` → `MissedSquareRelease` Cause → Runtime/public-v6 typed route/terminal/reply → `projectSquareReleaseRoute` |
| `PassedPawnProgressRealizedAfterOnlyLegalReply` | exact root actor/result actor/target subjects와 event result ID, ordered `dependency:*` 및 `result` route ID | structural-delta의 singleton legal-reply inventory와 각 dependency state certificate | observed root + certified suffix인 단일 `played_root_analysis_continuation`; event/inventory owner·모든 route/path/occurrence가 fingerprint를 구성 | 전용 proof assembler → `PassedPawnProgress` complementary Cause → Runtime/public-v6 → `projectPassedPawnProgressRealizedAfterOnlyLegalReply` |

다섯 생산자는 changed lower dependency와 상위 `PlayedVsBest` dispatch가
함께 있을 때만 실행한다. semantic proposition은 같아도 occurrence와
transposition history는 합치지 않으며, 한 occurrence의 독립 path만 한
proof set에 모두 보존한다. Cause draft도 exact proof record마다 하나씩
생산하므로 서로 다른 proof occurrence가 같은 actor/target이라는 이유로
합쳐지지 않는다. Runtime, schema와 UI는 위 ID와 수순 연속성을 검증해
소비할 뿐 공격맵·합법수·ray·pawn topology를 재계산하지 않는다.

### 참 사례와 적대 경계

| 계약 | 승인 가능한 참 사례 | 가장 위험한 거짓 양성 | 의도적으로 남는 거짓 음성/상위 의미 |
| --- | --- | --- | --- |
| unique-check displacement | 정확히 같은 수비자가 유일 체크 응수로 이동한 뒤 동일 target 포획의 재포획 자원이 사라짐 | 체크라는 이유만으로 다른 수비자 이동, 복수 응수, 대체 재포획을 무시함 | non-check deflection, 복수 합법 응수 중 전략적으로 강제된 응수, 일반 tempo/deflection |
| sole-recapturer removal | 실전의 유일 재포획자를 먼저 잡고 그 제거자가 되잡힌 뒤 동일 exploit을 실제 소비함 | 제거된 말과 실전 재포획자의 identity가 다르거나 다른 합법 재포획자가 있는데도 ‘수비자 제거’로 부름 | 재포획 없는 제거, 일반 과부하·간섭·수비 붕괴 |
| vacated-gate slider capture | exact blocker 이탈 뒤 같은 slider가 reach를 유지하여 exact target을 잡고 sibling에는 같은 자원이 없음 | 다른 slider/target을 대입하거나 단순 열린 선, target 이동, 대체 capture를 무시함 | non-capture 침투, rook lift, 장기 파일 장악, 일반 준비·기동 |
| square-release route | one-leg `30.Bg2! ... Rc6!`; Doknjas의 `...Rc6` 뒤 동일 knight `d5-b6-c4-a3-b1`과 `Nxb1 Rxb1`; Sadler의 `b4` 뒤 동일 knight `a5-b3-c1-d3-f4+ Kf7`처럼 release·route·terminal·sibling을 정확히 닫음 | 중간 재점유·same-role 이탈/복귀, route leg 생략, 다른 기물의 terminal, terminal L1·actual reply 누락, 다섯 check reply를 ‘강제’로 승격, 책 평가만으로 승인 | retained interval 밖의 token, terminal 뒤 장기 가치, 일반 준비·기동·outpost·예방 |
| passed-pawn after only reply | observed root 뒤 singleton 합법 응수와 인증 분석 continuation의 terminal passed-pawn result | PV suffix 전체를 실제 게임으로 부르거나 평가 손실·승리·불가피한 승격을 역추론함 | 복수 합법 응수가 모두 같은 결과에 이르는 경우, demand되지 않은 endpoint, 장기 계획·의도 |

여기서 demand되지 않아 생산되지 않은 참 관계는 demand-bounded 계산의
의도된 거짓 음성이지 권한 결함이 아니다. 반대로 demand가 있다는 사실은
어느 명제의 premise도 아니다.

현재는 일반 과부하, 일반 간섭, 모든 합법 repair까지 닫힌 수비 붕괴,
교환·이동에 의한 일반 수비 의무 이전, non-capture 또는 여러 단계의
일반 준비, 예방, 반격, 기동·outpost를 증명하지 않는다. 대체 수비자와
repair 전체, sibling counterfactual, square 안정성, intervening
dependency와 실제 후속 소비가 닫히기 전에는 이 이름들을 만들지 않는다.

향후 의미 커버리지는 현재 타입 이름을 늘리는 작업이 아니다. 레퍼런스의
인과 사례를 현재 권한과 대조한 결과는 다음 네 범주다.

| 분류 | 의미군 | 레퍼런스가 요구하는 연결과 현재 판정 |
| --- | --- | --- |
| **현재 정확히 증명 가능** | 위 다섯 봉인 계약, 그중 one/multi-leg `SquareReleaseRoute` | 표에 적은 exact side/role/square·movement occurrence/target, ordered occurrence, actual reply, branch, premise와 closure까지만 가능하다. route는 retained interval의 동일 physical mover를 증명하지만 전역 token이나 넓은 체스 이름은 승인하지 않는다. |
| **필요한 L0/L0.5/L1 사실이 부족함** | 일반 수비 의무 변화 | 제거·교환·간섭 뒤 모든 합법 repair/대체 수비와 두 의무의 동시 수행 불가능성을 닫는 인벤토리가 없다. |
| **필요한 L0/L0.5/L1 사실이 부족함** | 예방 | 수가 없을 때의 exact 상대 resource route와 수 뒤 동일 resource의 소멸을 같은 occurrence 좌표로 닫는 branch inventory가 일반형으로 없다. Doknjas `ref-b6054fc614ae9d9c17933ce856013081`, pp.74–81의 `b4/...b4/...Nc5`와 `d5/...exd4/...c5`는 정확한 대안 수순을 주지만 ‘좋은 counterplay의 방지’라는 평가는 premise가 될 수 없다. |
| **필요한 L0/L0.5/L1 사실이 부족함** | 반격 | 상대 위협의 exact realization과 별개의 forcing resource, 양측의 reply closure 및 선후 경주를 함께 발급하는 하위 계약이 없다. `ref-dc427a9593ab48248d19627b245fafaa`, p.507의 `32...Rf4!`는 motif와 수순 anchor이지 폐쇄 인벤토리가 아니다. |
| **필요한 L0/L0.5/L1 사실이 부족함** | 안정된 outpost·일반 수비 기동 | 상대 pawn challenge, 합법 교환·추방, 대체 경로 전체의 부재와 도착 뒤 역할 소비가 닫히지 않았다. |
| **하부 사실은 충분하지만 L2 결합 계약이 없음** | exact same-resource presence→absence timing | positive legal-move occurrence와 exact `LegalMoveFromTo`/`LegalCaptureOf` 부재는 발급할 수 있지만, 동일 상대 자원의 branch 간 identity, 현재 수가 바꾼 premise, 뒤의 actual consumer를 한 계약으로 아직 결속하지 않는다. Najdorf의 이른/늦은 `Nc6`와 `...Nb4` 사례는 이 경계를 감사하는 anchor이지 아직 ‘예방’ 증명이 아니다. |
| **하부 사실은 충분하지만 L2 결합 계약이 없음** | released access를 다른 기물이 소비하는 cross-piece route | ordered legal moves와 terminal L1은 있으나 현재 route 계약은 first occupant와 terminal mover가 같은 physical piece일 때만 닫힌다. `ref-03c77bd9c6c6ae987e07067822fd63b`, pp.130–132처럼 다른 기물이 최종 자원을 소비하는 사례는 별도 causal join이 필요하다. |
| **설명 선택·평가·장기 계획처럼 L2보다 높은 층의 책임임** | 준비, 예방, 반격, 기동, 공간, 약점이라는 일반 라벨과 최선성·의도·장기 가치 | 하위 exact proof가 생겨도 어느 인간 용어로 묶어 보여 줄지와 그 가치 평가는 exposure/설명 계층의 책임이다. 평가치나 책의 서술로 L2 명제를 역생성하지 않는다. |

특히 outpost는 L0의 정적 `outpost=true`가 아니다. 같은 기물이 실제로
도달하거나 점유한 occurrence, 지원·통제, 상대 pawn challenge 및 합법
교환/추방 자원의 부재, 뒤의 실제 역할 소비와 sibling 반사실까지 묶일 때
L2가 말할 수 있다.

### 새 활성 목표와 후보 경계

old vacancy working name과 개념별 청사진은 one/multi-leg
`SquareReleaseRoute` 정규 계약·전수 인벤토리로 흡수했다. 별도 alias,
호환 wrapper나 같은 의미의 두 번째 문서를 보존하지 않는다. 이 단계에서
현재 lower authority로 닫히면서 미구현이던 same-piece release route는 없다.

다음 공통 후보는 ‘수비 의무 변화’라는 이름이 아니라 **동일 상대 자원의
presence→absence와 뒤의 actual consumer**라는 구조다. 이는 예방, 수비 의무,
counterplay timing이 공유할 수 있지만 세 인간 라벨 중 어느 것도 미리
승인하지 않는다. 다음 조사에서는 Doknjas Najdorf의 `18...Bc4` 뒤에도
`19.Nb4`가 남는 적대 반례, 이른 `22.Nc6?`와 실제 `32.Nc6`의 timing,
Sadler·Van Delft의 구체적 대안 가지를 함께 대조한다.

활성 완료 조건은 (1) 같은 side/from/to/capture/target 자원의 positive
occurrence와 sibling closed absence, (2) 현재 수가 바꾼 exact lower premise,
(3) 그 부재를 실제 premise로 소비하는 뒤 occurrence, (4) actual과 exact
counterfactual branch, 모든 독립 path·transposition·완전한 dependency를
현 인벤토리만으로 닫는 완결 사례군을 찾는 것이다. 하나라도 없으면 새
enum을 만들지 않고 P2 lower-premise 결손 또는 상위 설명 책임으로 기록한다.

## 6. 공개와 평가의 분리

player response는 선택된 verdict와 원인이 소유한 증명만 투영한다.
typed L2 proof는 branch occurrence, proof paths, exact premises,
closed absence, 관련 기물과 realizing/defense move를 보존한다. 공개
계층은 actor/mechanism 문구나 confidence로 빈 필드를 채우지 않는다.
자연어 생성은 이 구조를 소비할 수 있는 후속 표현 기능일 뿐 프로젝트의
증명 목표가 아니다.

공개 trust boundary는 생산자보다 넓지 않다.

- `cause_evidence_id`와 `channel_id`는 한 commentary 전체에서 occurrence
  소유자가 유일하다. 같은 ID 아래 다른 proof path를 숨길 수 없다.
- `exposure`는 R이 인증한 `primary | complementary`를 그대로 운반한다.
  브라우저는 proof path 또는 branch occurrence 수로 이 등급을 다시 정하지 않는다.
- candidate/reference source와 comparison kind가 지시하는 실제 primary
  root가 event/actor/proof occurrence를 소유한다. 현재 다섯 typed L2
  증명군은 actionable `PlayedVsBest` 수요에서만 공개된다.
- Runtime·Python·브라우저는 공격맵이나 합법 자원을 다시 계산하거나
  typed proof의 체스 의미를 재승인하지 않는다. 서버가 낸 wire vocabulary,
  ID·branch·path·use 교차참조, root 운송 소유권과 occurrence/FEN/수순
  연속성만 fail-closed로 검증한다.

개발용 stage intervention, corpus 봉인, 통계와 release 절차는
[`../../judgment-evaluation/README.md`](../../judgment-evaluation/README.md)의
범위다. 평가 결과는 런타임 사실 생산자나 L2 증명 권한이 아니다.
