# 체스토리 판단 경계와 런타임 해설 완성 방법론

## 1. 완성 상태

체스토리 백엔드의 책임은 문장을 그럴듯하게 만드는 것이 아니다. 합법 수순, 엔진 비교, 보드 관계, 구조 변화, 계획의 인과관계를 하나의 닫힌 증거 그래프로 만들고, 그 그래프가 지지하는 판단만 공개하는 것이다.

완성된 런타임은 다음 조건을 동시에 만족한다.

1. 같은 사실이나 판단을 소유하는 타입이 둘 이상 존재하지 않는다.
2. 모든 공개 판단은 등록된 증거와 연결된다.
3. 평가값의 관점, 수순의 합법성, 비교 대상이 중간 단계에서 바뀌지 않는다.
4. 인간적 아이디어의 탐색과 사실의 인증을 분리한다.
5. LLM은 인증되지 않은 체스 사실을 새로 만들 수 없다.
6. 의미 품질 평가는 배포 코드의 대형 테스트가 아니라 독립된 블라인드 코퍼스에서 수행한다.

## 2. 단일 권위 흐름

```mermaid
flowchart LR
    A["요청: FEN, 둔 수, Stockfish 후보선"] --> B["정규화된 입력"]
    B --> C["Position / Line / Transition"]
    C --> D["TypedEvidenceGraph"]
    D --> E["JudgmentClaim 후보"]
    E --> F["ClaimTruthPolicy 진실 승인"]
    F --> K["ClaimDeduplicator 의미 중복 제거"]
    K --> L["ClaimArbitrator Cause 공개 결정·canonical claim host 운반"]
    L --> M["DirectCauseImportancePolicy typed 부분순서 + OrderingPolicy selectionOrder"]
    M --> G["닫힌 EvidenceBackedJudgmentPacket"]
    G --> H["직접 공개 JSON 투영"]
    H --> I["화면 또는 근거 제한 LLM"]
    D --> J["추가 probe 요청"]
    J --> A
```

각 단계의 권위는 하나뿐이다.

| 의미 | 단일 권위 |
|---|---|
| Stockfish 후보선 | `EngineLine` |
| 합법 수순 재생 | `PrincipalVariationEvidence.legalMoveReplay` |
| 조립 중 상태 | `JudgmentAssemblyContext` |
| 사실과 출처 관계 | `TypedEvidenceGraph` |
| played/reference 수치 비교와 verdict | `CandidateComparisonEvidence(CandidateComparisonFact)` |
| 후보군 구성과 best/second 분류 | `CandidateSetDescriptor` |
| 공개 가능한 체스 판단 | `JudgmentClaim` |
| claim 진실 승인 | `ClaimTruthPolicy` |
| 승인 claim의 의미 중복 제거 | `ClaimDeduplicator` |
| Cause의 비교 공개 선택과 tier/claim-ID canonical host 운반 | `ClaimArbitrator`가 호출하는 `PlayerFacingCauseExposurePipeline` |
| 모든 C Cause의 Jp→Ja→R 최종 상태 원장 | `CauseDispositionPolicy` |
| 이미 선택된 Cause effect의 typed domain 내 중요도 해석 | `DirectCauseImportancePolicy` |
| 중요도 부분순서를 선택 집합 불변의 공개 순서로 반영 | `PlayerFacingIdeaOrderingPolicy` |
| 정확한 zugzwang/tablebase 사실 증명(F evidence; 독립 공개 권한 아님) | `EndgameZugzwangProof` |
| 런타임 전달물 | `EvidenceBackedJudgmentPacket` |
| 화면·LLM 입력 형식 | 저장되지 않는 직접 JSON 투영 |

공개 형식은 별도 의미 모델이 아니다. 그래프와 claim을 읽는 일회성 표현이다. 따라서 표현 형식이 바뀌어도 체스 사실과 판단 권위는 늘어나지 않는다.

여기서 선택 권위와 중요도 해석 권위는 역할이 다르다. `PlayerFacingCauseExposurePipeline`만 공개 Cause 집합과 비교 관점을 선택하고, `DirectCauseImportancePolicy`는 그 집합을 변경하지 않은 채 이미 인증된 typed effect 사이의 부분순서만 해석한다. `PlayerFacingIdeaOrderingPolicy`는 그 결과의 dominance layer를 sentence-ready idea unit과 그 Cause facet의 `selectionOrder`에 반영할 수 있지만 Cause를 추가·삭제·변형할 수 없다. 따라서 importance 계층은 두 번째 선택 arbitrator가 아니다.

## 3. 구성 시점 불변식

패킷은 공개 생성자를 갖지 않는다. 생성 시 다음 조건을 모두 확인한 조립 상태만 패킷이 된다.

- 루트 position, 후보 line, evidence record가 존재한다.
- evidence ID는 중복되지 않는다.
- 모든 evidence parent가 같은 그래프에 등록되어 있다.
- evidence가 가리키는 position과 line이 조립 상태에 존재한다.
- 모든 claim의 근거가 그래프에 등록되어 있다.
- 모든 상대평가의 비교·원인·판정 근거가 그래프에 등록되어 있다.
- C에 등록된 모든 `RelativeCause` evidence ID는 `CauseDispositionLedger`에 정확히 한 번 존재한다. 원장은 새 판정을 만들지 않고 `ClaimTruthPolicy`의 Ja 결정, `ClaimDeduplicator` trace, canonical `PlayerFacingCauseExposureResolution`만 소비하여 `selected`, `dominated`, `redundant`, `diagnostic`, `inferior`, `admission-deferred`, `rejected`, `unproposed`, `object-unready` 중 하나로 귀결한다. 준비된 Cause가 claim host 없이 남으면 `unproposed`, 승인 뒤 claim dedup에서 host를 잃으면 해당 trace에 결속된 `redundant`이므로 조용히 사라질 수 없다.
- C의 의미 정규화는 공개 준비가 된 Cause를 동일 `RelativeCauseSemanticFrameKey` 안에서만 다룬다. 그 안에서 root-owned `causal_signature`가 하나라도 겹치는 Cause들을 **연결요소**로 묶어 proof·support를 한 번 병합한다. 따라서 `{A,B}`와 `{B,C}`는 한 Cause가 되지만 서로 겹치지 않는 `{A}`와 `{C}`는 같은 frame이어도 별도 Cause로 보존된다. 병합 뒤에는 기존에 승인된 signature와 다시 계산한 비모호 signature의 교집합만 `DirectEffectAdmission.Restricted`에 남긴다. 같은 signature의 effect descriptor가 충돌하면 임의의 승자를 고르지 않고 그 채널만 fail-closed한다. 이는 exact-equality 대표 선택이나 strict-subset 승자 방식이 아니다. 공개 미준비 Cause는 자기 evidence ID와 함께 서로 분리해 보존한다.
- 공개 fallback 억제는 `RelativeCauseDominancePolicy` 한 곳에서만 판정한다. representative를 고르기 전에 독립적으로 공개 가능한 모든 Cause가 자기 ID로 eligibility에 참여하며, 실제 선택 가능한 **하나의** specific Cause가 동일 effect mode로 generic Cause의 모든 직접 effect를 스스로 소유하고, 같은 root·source·attribution·event line에서 generic의 공개 tier를 낮추지 않을 때만 억제한다. dominance 뒤에는 retained Cause끼리만 representative를 고른다. 이때 한 Cause가 다른 Cause를 흡수하려면 중앙 dominance가 이미 증명한 fallback obligation을 자기 혼자 superset으로 보존해야 하고 비교 공개 우선순위를 역전할 수 없다. obligation이 서로 비교 불가능하면 둘 다 남으며 sibling의 obligation을 합칠 수 없다. 모든 제거 fallback은 최종 selected representative 중 적어도 하나를 실제 maximal direct dominator로 계속 가져야 한다. exact owned effect channel 일치는 필요조건일 뿐 종류 간 edge를 만들지 않으며, edge는 엄격한 의미 정제만 허용한다. 현재 전술 그래프는 `MissedTacticalResource → {WrongMoveOrder, RecaptureRecoveryWindow, KingForcing, MaterialSwing}`, `CandidateTacticalLiability → MaterialSwing`, `MaterialSwing → {WrongRecapturer, SacrificeCompensation, TacticalRefutationOfPlayed}`, `TacticalRefutationOfPlayed → {TempoLoss, KingForcing}`다. 전략 그래프는 `MissedStrategicImprovement → {StructuralImprovement, TargetPressureGain, CenterControlGain, PawnBreakOpportunity, ActivityGain, OpponentRestriction, PlanImprovement}`, `StructuralImprovement → PawnWeaknessTarget`, `StrategicConcession → {TargetPressureRelease, KingSafetyConcession, ActivityLoss, PlanContradiction}`다. 따라서 `MaterialSwing → WrongMoveOrder`, 전술 fallback과 draw/conversion 사이, 구조·전략 fallback과 conversion 사이처럼 의미 축이 다른 관계는 금지한다. `MaterialSwing`은 exact same owned channel의 `TacticalRefutationOfPlayed`를 억제할 수 없고 그 반대만 가능하며, 채널이 다르면 둘 다 보존한다. plan 정제는 specific Cause가 실제 `PlanResult`/`PlanRestriction` 직접 proof를 소유해야 한다. Primary specific은 Primary/Complementary fallback을 지배할 수 있지만 Complementary specific은 Primary fallback을 제거할 수 없고, sibling Cause의 채널 합산이나 diagnostic Cause의 억제도 금지한다.
- ordered line pair마다 비교 레코드는 하나뿐이며, delta·승률 변화·mate·verdict는 두 등록 line에서 정규 생성한 값과 정확히 같다.
- 후보군 descriptor는 실제 best/second pair 한 레코드만 소유하고, 그 분류에 사용한 top-3 line/eval을 parent로 가진다.
- F는 Cause draft와 무관하게 각 비교의 reference/candidate endpoint inventory를 만든다. 합법 재생된 line의 material·mate·qualitative family는 각각 독립적인 completeness를 가지므로 한 family의 누락이 다른 family를 지우지 않는다. material은 완전한 material window와 정확한 값, mate는 engine-backed mate 평가와 root-owned mate 관찰의 일치, qualitative은 정확한 합법 line에서 도출된 전체 관찰을 요구한다. structural은 root move의 합법적인 exact endpoint delta, strategic은 해당 비교의 모든 axis·source·outcome이 완전한 exact contrast를 요구한다. 필요한 source/counterpart inventory가 incomplete·중복 모호·불일치이면 C는 draft나 다른 family의 근거로 채우지 않고 해당 차등 채널을 명시적으로 보류한다.
- 외부 `currentEvalCp`, `deltaVsBaseline`, probe 요약 평가값은 입력 계약에서 제거되었으며, 그래프 평가는 등록된 line 또는 검증된 tablebase 결과에서만 파생된다.

이는 사후 테스트가 구조를 정당화하는 방식이 아니다. 불완전한 상태를 최종 타입으로 만들 수 없게 하는 구성 규칙이다.

## 4. 체스 판단의 증명 의무

모든 공개 claim은 종류에 맞는 증명 의무를 갖는다.

### 공통 의무

- **합법성**: 사용한 수와 PV는 해당 FEN에서 합법적으로 재생되어야 한다.
- **관점 일관성**: 평가값은 한 번만 mover 관점으로 변환되어야 한다.
- **출처 폐쇄성**: claim의 모든 evidence와 그 parent가 그래프에 존재해야 한다.
- **주체 결합**: claim은 position, played move, reference move, candidate line, threat, plan 중 정확한 주체를 가져야 한다.
- **대상 결합**: 구체적인 기물·칸·파일을 말하면 해당 object binding이 있어야 한다.

### 전술 claim

관계나 모티프 이름만으로는 부족하다. 합법 수순, 공격·수비 관계, 강제 응수 또는 평가 변화가 같은 원인 사슬에 있어야 한다.

### 전략 claim

정적 feature만으로는 부족하다. 구조 또는 기물 배치의 관찰, 후보선 간 차이, 지속 가능한 결과가 같은 축에서 연결되어야 한다.

### 오프닝 claim

오프닝 이름만으로는 증명되지 않는다. 인식된 계보와 현재 보드에서 관찰된 theme가 일치해야 한다.

### 엔드게임 claim

기법 이름만으로는 부족하다. 진입 조건, 유지해야 할 칸, 실제 수순 결과 또는 정확한 tablebase 증거가 있어야 한다.

### 판정 claim

`good`, `inaccuracy`, `mistake`, `blunder`, `only move`는 단일 position의 속성이 아니다. played 결과와 reference 결과를 동일 관점에서 비교한 증거가 필요하다.

## 5. 인간적 아이디어를 찾는 방법

인간적 아이디어의 탐색과 진실 판정을 한 모델에 맡기면 정밀도와 재현율을 동시에 통제할 수 없다. LLM 제안자를 도입할 때도 두 역할은 다음처럼 분리한다.

1. 결정론적 detector와 LLM은 **가설 제안자**가 될 수 있다.
2. 두 제안자는 모두 동일한 `JudgmentClaim` 후보 형식만 만든다.
3. 어느 제안자도 최종 권위를 갖지 않는다.
4. 단일 인증 정책이 그래프에서 증명 의무를 충족한 후보만 승인한다.
5. 승인된 claim만 중요도 경쟁을 거쳐 공개된다.

LLM은 사람이 알아보기 쉬운 계획 가설, 비교 관점, 장기적 목적을 넓게 제안하는 데 사용한다. 예를 들어 “최악의 기물 개선”, “폰 브레이크 준비”, “상대 반격 억제”를 후보로 제안할 수 있다. 그러나 LLM의 문구는 근거가 아니다. 제안한 actor, target, mechanism, consequence가 그래프에서 결합되지 않으면 후보는 폐기된다.

이 구조는 LLM의 인간적 탐색 능력을 사용하면서도 환각을 권위로 승격시키지 않는다. 결정론적 detector와 LLM detector는 중복 권위가 아니라 동일 인증기로 들어가는 서로 다른 후보 탐색기다.

## 6. 직접 공개 투영과 향후 LLM 계약

공개 JSON은 패킷에서 다음 정보만 직접 읽는다.

- verdict와 비교한 수
- R이 선택한 Cause의 종류, 비교 공개 권한인 `comparison_exposure_rank`, typed importance dominance layer를 반영한 `selection_order`, 공개 tier, effect mode, source side와 attribution
- `CauseDispositionLedger`만 기계적으로 집계한 `idea_status_detail`: authority, 전체 Cause 수, selected Cause ID, 상태·이유별 count, abstention code
- 그 Cause가 소유한 정확한 `DirectCauseChannel`의 actor, target, mechanism, consequence
- 채널에서 직접 일어난 `directChange`와 그 비교가 played move에 뜻하는 `playedChange`
- 채널의 carrier, provenance, line, horizon과 witness
- 채널의 `rootOwnedProof`에서만 압축한 nullable `proof_segment`
- Cause를 정확히 한 번 배치하는 승인 claim host의 family와 tier
- Cause에 결속된 only-move qualifier

Claim host는 표시 위치와 family/tier metadata만 제공할 뿐 Cause의 우선순위나 인과 근거를 결정하지 않는다. 일반 claim evidence, sibling Cause, context evidence 또는 parent closure에서 actor·target·mechanism·consequence를 다시 조립할 수 없다. 공개 Cause 한 개는 R이 선택한 `cause_evidence_id`, `comparison_exposure_rank`, `selection_order`와 그 안의 완전한 직접 채널을 그대로 운반한다. `comparison_exposure_rank`는 PlayedVsBest·BestVsSecond·PlayedVsAlternative 사이의 **비교 공개 권한 순서**이고 개별 Cause의 체스적 중요도 점수가 아니다. played orientation의 `PlayedVsAlternative`가 자기 exact compatible channel로 임계값 이상의 개선과 `CandidateAllowsLiability`를 함께 증명하면 동일 의미의 `PlayedVsBest` 책임 Cause가 없을 때 Complementary로 생존하며, 동일 PVB Primary가 있으면 PVB만 책임 대표로 남는다. 동일 `PlayedVsBest` 안에서 대안 자원을 Complementary로 내리는 판정은 fallback 억제 뒤에도 살아남은 하나의 played-liability Cause가 resource의 모든 채널과 같은 exact effect frame을 소유하고, direct change가 명시된 보완쌍일 때만 허용한다. sibling liability의 채널을 합치거나 같은 비교·종류·target만으로 대응시킬 수 없으며, 최종 tier가 확정된 뒤 단일 rank 정책이 `comparison_exposure_rank`를 처음부터 다시 계산한다. `selection_order`는 그 뒤 `PlayerFacingIdeaOrderingPolicy`가 증명된 idea-level typed dominance layer를 먼저, 동률이면 lead Cause의 comparison rank와 idea ID를 사용해 부여한다. 이는 선택 집합이나 Cause 의미를 바꾸지 않지만 단순 직렬화 순서도 아니다.

하나의 문장화 가능한 아이디어가 반드시 Cause 하나와 같지는 않다. 동일 `PlayedVsBest` 관계에서 정확히 한 played-liability Cause가 대안 resource Cause의 모든 직접 채널과 같은 effect frame을 자기 근거로 대응시킬 때만 `ExactPvbResponsibilityPair`가 생성된다. `PlayerFacingIdeaUnitPolicy`는 이 이미 인증된 pair만 받아 책임 Cause를 lead로, 대안 자원을 counterfactual facet으로 둔 하나의 `PlayerFacingIdeaUnit`을 만든다. Cause 기록·극성·source·attribution은 합치지 않으며, 대응이 없거나 둘 이상이면 각각 독립 unit으로 남긴다. 일반 channel 유사성, claim host, support ID 또는 비교 순위로 새 pair를 추론할 수 없다. 공개 `ideas`의 각 원소는 `item_role=cause_facet`과 `idea_unit_id`를 가지며, 실제 최상위 권한은 `idea_importance.unique_top`과 unit의 `priority_status`에만 있다. 배열의 첫 원소는 그것만으로 최상위가 아니다.

`idea_status_detail`은 새로운 fallback 또는 공개 판정 권위가 아니다. status/reason count는 원장의 enum을 모두 포함하는 고정 key 집합으로 집계하고, selected ID는 정렬해 싣는다. C Cause가 하나도 없으면 abstention은 `no_relative_cause_generated`, Cause는 있지만 selected가 없으면 원장에 실제 기록된 terminal reason code의 중복 제거 집합, selected가 하나라도 있으면 빈 목록이다. primary comparison이 engine-backed여서 full/renderable explanation을 투영할 때 공개 `ideas[*].cause_evidence_id`의 집합은 원장의 selected ID 집합과 정확히 같아야 하며 누락·추가·중복이면 projection이 fail-closed한다. 이 경우에만 기존 `idea_status`의 `certified`/`no_certified_differential_idea`를 이 닫힌 동일 집합에서 정한다. primary comparison이 engine-backed가 아니어서 payload 자체가 withheld되면 public ideas는 구조상 비어 있고 `idea_status`는 `unavailable`로 남는다. 이때 자체 비교가 engine-backed인 보조 비교 Cause가 R에서 이미 selected일 수 있으므로, 그 selected 원장은 삭제하거나 거부하지 않고 `idea_status_detail`에 진단 상태로 그대로 보존한다.

`RelativeCauseBinding.bindingTier`와 v3 진단 필드 `binding_tier`는 Cause가 비교 관계에 결속되는 역할(`Primary`/`Supporting`/`Context`)만 나타낸다. 이것 역시 체스적 중요도가 아니며 `importance` 부분순서나 `comparison_exposure_rank`를 대신할 수 없다.

설명 객체에는 selected Cause와 무관한 played PV 전체를 별도 `line`으로 싣지 않는다. 수순이 문장 근거가 되려면 해당 채널의 `rootOwnedProof`에서 직접 압축한 `proof_segment` 안에 있어야 한다. line episode는 검증된 root-to-event replay prefix만, plan result는 그 proof가 소유한 root-to-source enabling path만 싣고, root-local motif·relation·threat·transition은 정확한 root move 한 수만 싣는다. strategic wrapper는 primitive segment를 그대로 재사용한다. descendant·sibling·witness 수순을 합성하지 않으며 안전하게 압축할 수 없으면 `null`을 싣되 Cause 자체를 제거하지 않는다. 이 segment는 문장화 근거일 뿐 준비도·선택·fallback·importance 권위로 사용하지 않는다. 특히 reference resource를 설명하면서 played line의 후손 수를 근거로 섞는 통로를 공개 DTO 수준에서 닫는다.

별도 `importance` 객체는 R이 이미 선택한 집합을 바꾸지 않고 `RootOwnedEffectPolicy`가 승인한 직접 채널의 typed 결과만 부분순서로 비교한다. 비교 권한을 주는 universe는 **semantic root board + 정규화된 `PlayerFacingImpact`**뿐이다. comparison, event line, source/attribution, exposure, effect mode, actor, direct/played change와 source-side stake는 완전한 provenance로 보존되지만 그 값의 equality가 magnitude 비교 권한을 주거나 막지는 않는다. 같은 universe 안에서도 typed effect domain과 필요한 effect identity가 맞아야 하며, 실제 board-mate episode만 terminal result로서 cross-domain 우위를 가질 수 있다. material effect는 포획 대상의 role·square·value라는 event salience와 beneficiary 기준의 durable outcome을 분리한다. 중요도 magnitude는 complete observed material window에서 양수로 유지되는 stable floor와 그 onset만 사용하며, event salience는 문장화 정보일 뿐 ranking 값이 아니다. cause kind, claim host의 family/tier, support/provenance 수, ID, 비교 delta와 line 전체 평가는 중요도 크기나 점수로 쓰이지 않는다. 다른 typed family와 미측정 proof는 `incomparable`로 보존한다. Cause 전체를 낮은 dominance layer로 내리려면 **하나의 동일한 다른 Cause**가 그 Cause의 선택된 모든 측정 채널을 각각 엄격 지배해야 하며, 서로 다른 sibling Cause의 우위를 합산할 수 없다. 미측정·모호 채널은 계속 fail-closed한다. `idea_importance`는 각 unit의 책임-owning lead만 같은 정책에 넣어 pair의 두 facet이 서로 경쟁하지 않게 한다. 그 뒤 `PlayerFacingIdeaOrderingPolicy`가 증명된 dominance graph의 maximal layer부터 unit과 그 연속된 facet의 `selection_order`를 다시 부여하되 선택 집합은 불변이다. claim host 행은 별도 휴리스틱 점수 없이 exposure tier 내림차순, claim ID 오름차순으로만 운반된다. 모든 선택 idea lead의 모든 채널이 측정되고 frontier unit이 정확히 하나일 때만 `unique_top`을 공개한다.

Packet은 공개 시점에 exposure를 다시 판정하지 않는다. R이 산출한 canonical `PlayerFacingCauseExposureResolution` 전체를 factory에서 재계산 결과와 동일한지 검증한 뒤 그대로 보존하고, `importance`는 그 안의 `importanceResolution`만 읽는다. 따라서 중요도 해석은 새로운 선택 권위나 공개 fallback 경로가 아니다.

공개 `importance`는 선택 Cause ID, 모든 profile, `incomparable`을 포함한 모든 profile 관계의 양쪽 `causal_signature`, nullable domain, Cause별 measured/unmeasured channel 결정까지 보존한다. 관계를 Cause 쌍으로 합치거나 incomparable 관계를 개수만 남기고 버리지 않는다. `relation_summary`의 comparable/incomparable 수는 Cause 쌍이 아니라 실제 측정된 **effect profile 쌍**의 수다. 여러 직접 채널을 가진 Cause를 하나의 합산 점수로 압축하지 않기 때문에 이 단위를 명시적으로 유지한다.

공개 `idea_status`는 문장 생성 여부가 아니라 인증 경계를 나타낸다.

- `certified`: R이 선택해 공개하는 differential Cause가 하나 이상 있다.
- `no_certified_differential_idea`: exact engine-backed `PlayedVsBest` 1차 비교는 있어 verdict를 공개할 수 있지만, 공개 인증된 differential Cause는 없다. 이는 수 전체를 판단 불능으로 만드는 상태가 아니라 Cause에 대해서만 명시적으로 보류한 상태다.
- `unavailable`: exact engine-backed 1차 비교 자체가 없어 verdict와 differential idea를 판단할 근거가 없다.

모든 합법 수는 보드를 변화시키며 F 수준의 사실을 만들 수 있다. 그러나 모든 수에 “가장 중요한 차등 아이디어”가 증명되는 것은 아니므로, 시스템은 Cause를 억지로 생성하지 않는다. Cause가 0개여도 exact engine-backed `PlayedVsBest` verdict는 그대로 공개하고 `idea_status=no_certified_differential_idea`로 구분한다.

현재 production 경계는 이 JSON의 직접 투영에서 끝난다. 아래 LLM 문장화와 검증은 아직 배포 코드에 구현됐다는 설명이 아니라, 이후 구현이 따라야 할 **규범 계약**이다.

향후 LLM에는 이 JSON과 다음 규칙을 함께 준다.

1. 각 인과 문장은 하나의 `cause_evidence_id`와 하나 이상의 `causal_signature`를 인용한다.
2. 한 문장의 actor, target, mechanism, consequence와 변화는 같은 `DirectCauseChannel`에서만 읽는다.
3. `directChange`, `playedChange`, effect mode와 comparison 관점을 서로 바꾸어 말할 수 없다.
4. Claim host, sibling Cause, 일반 claim evidence와 `context` 근거를 원인 보충재로 사용할 수 없다.
5. 기물, 칸, 수순, 평가 방향은 선택된 채널과 그 provenance 범위를 벗어날 수 없다.
6. 상대평가가 없으면 실수·탁월·유일수 판정을 만들 수 없다.
7. 계획의 미래 결과가 조건부이면 조건을 생략할 수 없다.
8. 근거가 부족하면 문장을 보강하지 말고 해당 설명을 생략한다.

향후 LLM 출력 검사는 체스 의미를 다시 추론하지 않는다. 존재하지 않는 Cause·채널 인용, 스키마 위반, 허용되지 않은 새 수·칸·기물 토큰만 거부한다. 이 검증기가 실제로 구현되기 전에는 문장 수준의 구조 오류가 원천적으로 불가능하다고 주장하지 않는다.

공개 의미의 루트는 두 종류뿐이다. 인과 아이디어·계획은 R이 선택한 Cause와 그 Cause가 소유한 정확한 `DirectCauseChannel`에서, verdict는 player-facing `Primary` `PlayedVsBest` 비교에서 직접 투영된다. 따라서 verdict는 전체 수의 1차 판정 범위이고, 각 idea의 `comparison`은 그 idea가 무엇과 무엇을 비교하는지 결정하는 유일한 권위다. 둘을 합쳐 별도 의미 모델을 만들거나 verdict의 비교를 모든 idea에 덮어씌우지 않는다. 승인 claim은 선택된 Cause를 정확히 한 host에 배치하지만 Cause의 의미나 근거를 확장하지 않는다. `EndgameZugzwangProof`와 tablebase 결과는 F의 typed evidence로 계속 보존되지만, 그 존재만으로 C→Jp→Ja→R을 우회하는 독립 exact payload를 만들 수 없다. 공개하려면 먼저 선택된 Cause의 `EndgameHorizon` root-owned effect로 승격되어야 한다.

## 7. 블라인드 실패의 병목을 찾는 식별 실험

최종 문장 하나만 채점해서는 어느 단계가 실패했는지 식별할 수 없다. 런타임을 다음 합성으로 본다.

`출력 = V ∘ P ∘ R ∘ Ja ∘ Jp ∘ C ∘ F ∘ Q(입력)`

1. **Q — Input/evidence acquisition**: 필요한 depth·MultiPV·probe·tablebase 증거가 충분하고 반복 가능한가.
2. **F — Fact**: 보드 사실, 합법 수순, 엔진 비교가 맞는가.
3. **C — Cause**: 후보선 차이의 실제 원인을 잡았는가.
4. **Jp — Judgment proposal**: 인간이 중요하게 보는 아이디어가 후보 집합에 들어왔는가.
5. **Ja — Judgment admission**: 참인 후보를 승인하고 거짓 후보를 거부했는가.
6. **R — Rank**: 여러 승인 claim 중 핵심을 먼저 골랐는가.
7. **P — Projection**: packet의 의미가 공개 JSON에서 보존됐는가.
8. **V — Verbalization**: 보존된 의미를 정확하고 이해하기 쉽게 문장화했는가.

`Jp`와 `Ja`를 합치면 모델의 탐색력 부족과 인증 정책의 과잉 거부를 구분할 수 없다. `P`를 생략하면 그래프에는 있던 정보가 끝단에서 사라진 결함을 LLM 문제로 오인한다.

### 실험을 실행 가능하게 만드는 선행 산출물

아래 oracle 실험은 현재 production 런타임에 구현된 기능이라고 주장하지 않는다. 실행 전 별도 평가 하네스에 다음을 먼저 만든다.

1. Q/F/C/Jp/Ja/R/P/V 각 단계의 버전된 입력·출력 JSON Schema
2. 표본마다 모든 중간 산출물, 모델·엔진·정책 버전, 입력 해시를 보존하는 artifact capture
3. 실제 산출물과 전문가 oracle을 같은 인터페이스에 주입하는 stage adapter
4. all-oracle, one-stage, leave-one-actual, 누적·factorial 조합을 동일 seed와 고정 조건으로 실행하는 runner
5. 참조 관계를 보존하는 ID 재부여, 의미 없는 map/set 순서, schema가 동치로 선언한 null/생략 표현만 통일하는 canonicalizer

이 다섯 가지와 코퍼스 버전이 고정되기 전에는 병목 실험을 “실행 가능” 또는 “증명 완료”라고 부르지 않는다.

### 동일 스키마 oracle 개입

Oracle은 자연어 정답을 중간 단계에 억지로 넣지 않는다. 실제 단계와 정확히 같은 스키마의 전문가 산출물로 한 단계만 교체한다. Oracle 작성자는 해당 단계까지의 입력과 작성 지침만 보고, 실제 뒤 단계 산출물, 최종 해설, gold 평가 결과를 보지 않는다.

모든 단계 교체의 공통 primary endpoint는 최종 해설 성공 여부 `E`다. 전문가가 `answerable`로 합의한 표본에서 다음 조건을 모두 만족하면 `E(x)=1`이다.

1. 시스템이 보류하지 않는다.
2. 공개 의미 단위가 모두 등록 근거 범위 안에 있고 사실 오류가 없다.
3. 최상위 핵심 claim 중 하나가 통계 unblinding 전에 고정된 해당 run의 허용 정답 집합에 들어간다.
4. 시스템/arm을 모르는 독립 평가자의 설명 유용성 합의가 사전 등록 임계값 이상이다.

`unanswerable`로 합의한 표본은 보류하고 근거 없는 의미를 공개하지 않았을 때만 `E(x)=1`이다. 허용 정답 집합 작성자와 유용성 평가자는 서로의 결과와 arm을 보지 않는다.

평가자 독립성은 이름을 가리는 것만으로 성립하지 않는다. 원자 cluster마다 `oracle chain 작성`, `기본 라벨 작성`, `held-out human 해설 작성`, `out-of-set claim 판정`, `최종 유용성 평가`의 다섯 역할은 서로 다른 사람이 맡는다. 인력 제약으로 완전 분리가 불가능하면 실행 전에 고정한 cross-fit 역할표를 사용하되, 같은 사람은 자신이 앞 역할에서 본 원자 cluster를 뒤 역할에서 판정하거나 평가할 수 없다. 역할 배정, 열람한 artifact hash, 열람 시각을 append-only 접근 로그에 남긴다. Out-of-set 판정자에게는 문체, 원문 문장, provider 정보, arm별 ID·순서·confidence를 제거하고 의미 중복을 합친 canonical atomic claim과 그 합법 수순·보드·엔진 검증 재료만 보여 준다. 유용성 평가자는 최종 문장을 보아야 하므로 source/arm 무작위화에 더해 같은 cluster의 oracle·라벨·human 해설·out-of-set 판정 역할에서 제외한다.

Q 출력은 원시 요청을 사실 단계가 소비할 수 있게 만든 동일 스키마의 엔진 증거 pack이다. 실제 Q에는 depth, nodes, MultiPV line, 반복별 평가, probe와 tablebase 출처, 누락 상태를 그대로 기록한다. 개입 Q는 표본을 보기 전에 고정한 더 높은 depth·MultiPV, 반복 안정성 기준, 적용 가능한 tablebase를 사용하되 실제 Q와 같은 스키마를 쓴다. `actual-Q`와 이 `richer/stable-Q`를 나머지 실제 단계에 짝지어 주입하고, Q만 바꿨을 때 최종 `E`와 `D_Q`가 회복되는지 먼저 본다. 회복되면 자료 준비 병목이고, Q가 충분한 상태에서도 F 교체가 필요할 때만 fact 구현 병목으로 귀속한다.

단일 단계 교체 전에, upstream 출력에 조건화해 서로 일관되게 작성한 stable-Q→F→V 전체 oracle chain으로 `all-oracle ceiling`을 실행한다. 서로 결과를 보지 않은 두 팀이 독립 coherent chain을 작성한다. 두 chain은 모집단 변이를 추정하는 임의표본으로 보지 않고 고정된 독립 재현·민감도 분석으로 각각 보고한다. 두 fixed sensitivity chain이 모두 사전 등록 ceiling 기준을 통과해야만 각 chain에 대해 단계 개입 실험을 독립 실행한다. 그 뒤 사전 등록한 핵심 개입 효과의 방향이 두 chain에서 일치할 때만 귀속 결론을 내린다. 불일치하면 한 chain을 선택·평균·pooling하지 않고 oracle 지침·표현의 귀속 `판정 불가`로 foundation decision tree에 되돌린다. 이 보수적 규칙은 결과를 보기 전에 고정한다. Oracle 작성자 모집단으로 일반화하려면 사전 검정력에 맞는 수의 독립 team cluster를 별도로 모집한다.

Ceiling이 출시 endpoint를 통과하지 못하면 다음 foundation decision tree를 순서대로 실행한다.

1. 먼저 하네스 identity/null control을 통과시킨다. Actual artifact를 adapter에 넣고 빼면 byte-equivalent canonical 의미가 나와야 하고, no-op stage 교체의 `E/D` 차이는 0이어야 한다. Runner는 같은 seed·artifact hash에서 재현되고, canonicalizer는 idempotent하며, evaluator는 사전 작성 sentinel과 blind duplicate에 같은 판정을 내려야 한다. 실패하면 평가 하네스 결함이다.
2. 두 독립 chain의 단계별 의미가 semantic-equivalence 기준에서 재현되는지 본다. 불일치하면 oracle 지침·정의 문제다.
3. 합의된 인간 의미를 각 단계 schema로 encode→decode해 정보 손실 없이 복원되는지 독립 전문가가 본다. 실패하면 schema 표현력 결함이다.
4. `E`의 네 조건을 각각 분해해 보류, 근거·사실, 허용 핵심 claim, 유용성 중 어디서 ceiling이 깨지는지 본다.
5. 실패 표본의 answerable·허용 집합을 arm-blind하게 재심한다. 합의가 불안정하면 라벨·자료 결함이다.
6. 같은 입력에 대한 held-out human 해설의 `E`를 측정한다. 사람 해설도 모든 system/oracle arm과 함께 source/arm-blind open-world adjudication에 넣는다. 아직 oracle graph에 없는 claim은 통계 unblinding 전에 합법 재생과 엔진·보드 검증을 거쳐, 유효하면 공통 의미 ID로 run oracle graph와 모든 arm의 허용 집합에 함께 추가하고 다시 동결한다. 이 절차에서 거부된 미매핑 단정만 근거 위반으로 센다. 명시적 설명 거부를 abstention으로, 핵심 claim·유용성은 시스템과 같은 규칙으로 판정한다. 사람 ceiling도 기준에 못 미치면 대상 분포 또는 통과 기준 자체가 실현 불가능하다.

이 분해가 끝나기 전에는 production 단계 하나를 병목으로 지목하지 않는다.

Ceiling이 통과하면 복수·비인접 병목을 놓치지 않도록 두 방향의 개입을 함께 사용한다.

- `Δᵢᴱ(x) = E(one-stage-oracleᵢ(x)) - E(actual(x))`: 나머지가 실제일 때 단계 `i` 하나의 교체가 충분한가.
- `Λᵢᴱ(x) = E(all-oracle(x)) - E(leave-i-actual(x))`: 나머지가 모두 올바를 때 실제 단계 `i`가 성공을 깨뜨리는가.
- actual에서 Q→V로 하나씩 oracle을 누적하는 forward substitution과, all-oracle에서 V→Q로 하나씩 actual을 복원하는 backward substitution: 순서와 상호작용 경계를 찾는다.

단계별 지표 `Dᵢ`는 회복 여부를 결정하는 endpoint가 아니라, `Δᵢᴱ` 또는 `Λᵢᴱ`가 유의할 때 어떤 의미가 고쳐졌는지 설명하는 secondary diagnostic이다. 실행 전에 `E`, 각 `Dᵢ`, 허용 정답 집합, 유용성 임계값을 고정한다.

| 단계 | secondary diagnostic `Dᵢ` |
|---|---|
| Q | 요구 후보선 coverage·top-k 반복 안정성·평가 분산·mate/tablebase 재현성·증거 누락률 |
| F | 합법성·평가관점·fact exact accuracy |
| C | 원인 방향과 actor/target/mechanism/consequence exact match |
| Jp | 허용 대안 집합에 대한 후보 recall |
| Ja | 승인 precision을 하한으로 둔 constrained recall |
| R | 핵심 claim top-1 agreement |
| P | packet 의미 필드 보존율 |
| V | 근거 위반 0을 제약으로 둔 설명 유용성 |

표본 사이에 game, opening lineage, tactical archetype, counterfactual 관계 중 하나라도 있으면 같은 연결요소로 합치는 **원자 cluster**를 먼저 고정한다. 이 전이적 연결요소를 쪼개지 않는 cluster-paired bootstrap에서 각 사전 등록 대조량의 단측 95% 신뢰구간 하한이 0보다 클 때만 최종 실패에 대한 효과가 있다고 본다. 여러 단계·family를 동시에 탐색할 때는 Holm 방식으로 family-wise error를 통제하고, 탐색 결과와 최종 확인 가설을 분리한다.

단일 순차 개입만으로는 단계 간 상호작용을 유일하게 식별할 수 없다. 의심 단계와 바로 다음 단계에 대해 `actual/oracle × actual/oracle` 조합을 실행해 **인접 인터페이스 교체 민감도**를 측정한다. 여기서 상호작용이 남거나 여러 단계가 같은 회복을 설명하면 관련 단계의 더 넓은 factorial 또는 사전 지정 mediation 분석으로 확장하고, 그 전에는 단일 근본 원인으로 단정하지 않는다.

| 관찰 | 의심되는 인터페이스 | 조치 |
|---|---|---|
| actual-Q→stable-Q에서 `Δ/Λ`와 `D_Q`가 함께 양성 | 엔진 증거 depth·MultiPV·probe·tablebase 준비 | acquisition 설정·자료 생성 수정 |
| stable-Q에서도 F의 `Δ/Λ`와 `D_F`가 함께 양성 | fact 추출·합법 재생·관점 구현 | evidence producer 수정 |
| C의 `Δ/Λ`와 `D_C`가 함께 양성 | 인과 비교·귀속 | cause binding 수정 |
| Jp의 `Δ/Λ`가 양성이고 실제 후보 recall이 낮다 | detector/LLM 탐색력 | 후보 제안자 개선 |
| 후보에는 있고 Ja의 `Δ/Λ`가 양성 | 인증 의무의 과잉 거부·오승인 | 단일 admission policy 수정 |
| 승인 claim은 맞고 R의 `Δ/Λ`가 양성 | Cause 공개·중복 제거·typed 중요도 | exposure/dedup/importance 정책 수정 |
| packet은 맞고 P의 `Δ/Λ`가 양성 | 직접 투영 손실 | projection 계약 수정 |
| P까지 맞고 V의 `Δ/Λ`가 양성 | 문장화 | verbalizer 수정 |
| 전문가 재라벨 간 합의·반복성이 낮다 | 자료·정의 | 코퍼스와 annotation 규칙 재작성 |

같은 스키마만으로 oracle의 표현 분포가 통제되지는 않는다. 개입 문맥 `c`를 one-stage actual-rest, leave-one-actual oracle-rest, 누적, factorial 각각으로 구분한다. 각 `(i,c)`에 대해 세 arm을 같은 뒤 단계로 실행한다.

- `actualᵢ,c`: 해당 문맥의 실제 단계 출력
- `format-controlᵢ,c`: 실제 의미는 그대로 두고 참조 보존 ID, 의미 없는 map/set 순서, schema가 null과 동치라고 선언한 생략 표현만 공통 형식으로 정규화
- `oracleᵢ,c`: 같은 문맥에 조건화한 oracle 의미를 canonicalizer로 정규화

Confidence 값, ranked list의 순서, 필드의 실제 missingness는 뒤 단계의 admission·rank·abstention을 바꾸는 작동 의미이므로 format-control이 바꿀 수 없다. 이 세 변수의 효과를 분해할 때는 각각 actual/oracle 값을 독립 요인으로 둔 사전 등록 factorial arm을 추가하고, 표본 수가 부족하면 부분 factorial의 alias 구조를 미리 명시한다. 이 개입은 형식 효과가 아니라 interface-policy 효과로 별도 보고한다.

형식 효과는 `Φᵢ,c = E(format-controlᵢ,c) - E(actualᵢ,c)`, 전체 의미 교체 효과는 `Γᵢ,c = E(oracleᵢ,c) - E(format-controlᵢ,c)`다. Confidence·rank·missingness factorial을 실행한 경우에만 `Γᵢ,c`를 그 요인별 효과와 나머지 내용 효과로 더 분해한다. 특히 leave-one-actual은 all-oracle rest 안에서 raw actual, 비의미 형식만 정규화한 actual, oracle의 세 arm을 모두 둔다. 같은 원자 cluster의 paired 단측 신뢰구간과 사전 등록 Holm 보정을 통과할 때만 semantic 교체 효과로 해석한다. 따라서 이 실험이 증명하는 것은 “고정된 대상 분포와 뒤 단계 아래에서 어느 인터페이스의 의미를 교체하면 실패가 회복되는가”이며, 원인에 대한 무제한의 보편 명제가 아니다.

양성 효과가 검출되지 않았다는 사실은 잔여 병목이 없다는 증거가 아니다. 실행 전에 각 핵심 `(i,c)`와 `Δᵢᴱ`, `Λᵢᴱ`, `Γᵢ,c`에 대해 무시 가능한 최대 잔여 이득 `ε_gain(i,c)`을 따로 등록한다. “잔여 actionable oracle gain이 없다”고 판정하려면 fresh-confirm에서 해당 대조량의 Holm-adjusted 단측 신뢰구간 상한이 `ε_gain(i,c)` 이하인 동등성 기준을 모두 통과해야 한다. 검정력이 부족하거나 상한이 경계를 넘으면 효과의 유의성 여부와 관계없이 `판정 불가`다.

## 8. 블라인드 코퍼스 설계

평가 자료는 배포 테스트 코드에 넣지 않는다. 버전이 있는 외부 JSONL 또는 데이터 저장소로 관리한다. 병목 탐색용 `diagnostic-explore`, 선택한 귀속 가설을 한 번 확인하는 `diagnostic-confirm`, 최종 출시 성능만 보는 봉인 `blind`를 원자 cluster 단위로 분리한다. 자료가 작으면 explore 내부에서 nested cross-fitting으로 stage를 선택하고 바깥 fold에서 귀속을 확인하되, blind는 독립 실행자가 최종 후보를 한 번 평가할 때만 연다. confirm이나 blind 결과를 개발 자료로 재사용하면 다음 반복에는 새 버전을 만든다.

### 표본 구성

- opening, middlegame, endgame을 분리한다.
- tactical, strategic, defensive, conversion, material, evaluation family를 층화한다.
- good move와 bad move를 같은 모티프 안에서 짝지은 counterfactual 표본을 둔다.
- 같은 position에서 played/reference를 바꾼 대조 표본을 둔다.
- 유명 퍼즐·오프닝 이름만 기억해 맞힐 수 없도록 계보 단위로 학습 자료/diagnostic-explore/diagnostic-confirm/blind를 분리한다.
- Stockfish depth, MultiPV 수, tablebase 유무를 기록한다.
- game, opening lineage, tactical archetype, counterfactual 관계의 전이적 연결요소를 원자 cluster로 만든 뒤, 같은 cluster가 explore/confirm/blind 둘 이상에 들어가지 않게 분리한다.

### 라벨 구조

평가자는 완성 문장을 먼저 쓰지 않는다. 다음 순서로 독립 라벨을 남긴다.

1. 핵심 비교 수
2. 원인과 결과
3. actor, target, mechanism, consequence
4. 필수 PV 구간
5. 핵심 claim과 보조 claim
6. 확신도와 허용 가능한 대안
7. 마지막에만 자연어 해설

복수 평가자가 동의하지 않은 항목은 단일 정답으로 만들지 않고 대안 집합과 합의율을 보존한다. 모델의 목표 상한은 “한 명의 문장 복제”가 아니라 사람 간 합의 수준이다.

`answerable` 여부와 기본 허용 정답 집합은 시스템 출력 전에 작성한다. Closed-world 오판을 피하기 위해 모든 system/oracle arm과 held-out-human에서 나온 out-of-set 핵심 claim을 출처와 arm 정보 없이 섞어 독립 전문가가 사전 고정한 semantic-equivalence, 합법 재생, 엔진·보드 검증 규칙으로 판정한다. 이때 앞서 고정한 원자 cluster별 역할 분리 또는 cross-fit 제한을 그대로 적용하고, 판정 입력은 canonical atomic claim으로 제한한다. 올바른 새 대안은 arm identity와 통계 결과를 열기 전에 공통 의미 ID로 run oracle graph와 모든 arm의 허용 집합에 일괄 반영해 다시 동결한다. 이는 사전 동결한 후보 시스템을 변경하는 일이 아니라, 사전 등록 절차에 따라 해당 run의 평가 집합을 확정하는 일이다. 이 절차에서 거부된 미매핑 단정만 근거 위반으로 센다. Primary `E`는 이 동결된 run 집합만 사용하며 이후에는 바뀌지 않는다. 사전 기본 집합만 쓴 결과는 sensitivity로 함께 보고하고, 통계 unblinding 뒤 발견된 대안은 현재 합격 판정에 넣지 않고 다음 코퍼스 버전에만 반영한다. 최종 유용성 평가자는 어느 arm의 출력인지, 중간 산출물과 gold 작성자가 누구인지 모르는 상태로 무작위 순서의 결과만 평가한다.

## 9. 측정과 통과 기준

구조 지표와 의미 지표를 분리한다.

### 구조 지표

- 합법 수순 오류: 0
- 평가 관점 반전 오류: 0
- 그래프 밖 evidence 인용: 0
- 공개 claim의 근거 폐쇄율: 100%
- LLM의 미등록 수·칸·기물 추가: 0

### 의미 지표

- claim 사실 정밀도
- Jp 후보 recall
- Ja 승인 precision, recall, calibration
- 핵심 claim top-1 및 top-3 합의율
- actor/target/mechanism/consequence exact match
- 원인 방향과 played/reference 귀속 정확도
- 조건부 계획을 확정적으로 말한 비율
- family·게임 단계별 coverage
- answerable 표본에 대한 abstention 비율
- confidence 임계값별 selective-risk/coverage 곡선
- confidence calibration
- 사람 평가자 대비 설명 유용성

정밀도만으로는 전부 보류하는 시스템이 통과할 수 있으므로 단독 기준으로 쓰지 않는다. 출시 전에 대상 분포, family·게임 단계별 최소 coverage, 최대 abstention, 비열등성 허용폭 `δ`를 미리 등록한다.

기본 출시 기준은 다음과 같다. 비율 기준은 점추정치가 아니라 사전 등록한 단측 신뢰구간으로 판정한다.

- blind set 승인 claim 사실 정밀도의 단측 95% 하한이 97% 이상
- 문장 verifier 구현 전에는 근거 없는 공개 문장 비율의 단측 95% 상한이 사전 등록 한계 이하
- 문장 verifier 구현 후에는 verifier가 허용할 수 없는 참조·토큰의 구조적 유출 0
- 전문가가 answerable로 합의한 동일 표본에서 `d_cov(x) = I(system이 핵심 claim을 제공) - 0.95 × mean_r I(held-out human r가 핵심 claim을 제공)`로 정의한 family·게임 단계별 paired coverage 차이의 단측 95% 하한이 0 이상
- 같은 answerable 표본과 분모에서 `d_abs(x) = I(system이 보류) - mean_r I(held-out human r가 명시적으로 보류)`로 정의한 paired abstention 차이의 단측 95% 상한이 5%p 이하. 사람 간 라벨 불일치율은 abstention의 대용치로 쓰지 않고 별도 ambiguity 지표로 보고
- 50%, 70%, 90% 고정 coverage 지점 각각에서 paired cluster bootstrap의 단측 비열등 신뢰구간이 등록 기준선을 통과
- 핵심 claim top-k와 설명 유용성에서 paired cluster bootstrap `95% CI lower(system - held-out human) ≥ -δ_endpoint`

모든 bootstrap은 앞서 고정한 원자 cluster를 보존하는 paired 방식으로 실행한다. Human comparator가 들어가고 같은 평가자가 여러 원자 cluster를 맡았다면 원자 cluster와 held-out-human 평가자 ID를 독립 재표집하는 사전 지정 multiway cluster bootstrap으로 관측된 cluster×평가자 cell을 보존한다. 평가자가 한 cluster에만 중첩되어 있다면 cluster를 먼저, 그 안의 평가자를 다음으로 재표집하는 nested cluster/rater bootstrap을 쓴다. 이로써 표본 불확실성과 인간 평가자 불확실성을 함께 반영한다. 여러 primary 비교에는 사전 등록한 Holm 보정을 적용한다. 비열등 허용폭은 endpoint마다 별도로 배포 전에 정하며, 비율 endpoint의 시작 제안값만 3%p로 둔다. 파일럿의 cluster 내 상관, 평가자 간 상관과 예상 효과로 최소 유효 cluster·held-out-human 수와 검정력을 사전 계산하고, 그 수에 못 미친 전체·하위 구간은 “통과”가 아니라 “판정 불가”로 둔다. 전체 평균으로 취약 family를 숨길 수 없다.

## 10. 실행 순서

1. Q/F/C/Jp/Ja/R/P/V 스키마, artifact capture, adapter, runner, canonicalizer를 먼저 버전 고정한다.
2. 대상 분포, 공통 최종 endpoint `E`, 단계별 진단 `Dᵢ`, answerable·허용 정답 정의, endpoint별 `δ`, 대조별 `ε_gain`, coverage/abstention 하한, 다중 비교 규칙과 최소 cluster·held-out-human 수를 등록한다.
3. 원자 cluster 단위로 `diagnostic-explore`, `diagnostic-confirm`, `blind`를 나누고 confirm·blind를 봉인한다. 같은 단위로 역할-disjoint 또는 cross-fit 배정표와 artifact 접근 정책도 고정한다.
4. explore에서 Q/F/C/Jp/Ja/R/P/V 실제 기준선, actual-Q 대 richer/stable-Q 대조, coherent all-oracle ceiling을 저장한다.
5. ceiling이 통과할 때만 one-stage `Δᵢᴱ`, leave-one-actual `Λᵢᴱ`, forward/backward substitution과 각 문맥의 `Φᵢ,c/Γᵢ,c`를 실행한다.
6. `Dᵢ`와 필요한 factorial 개입으로 후보 인터페이스를 좁히고, out-of-set claim의 arm-blind adjudication을 결과 공개 전에 고정한다.
7. 선택한 귀속 가설과 효과 방향을 confirm에서 한 번 재현한 뒤 해당 단계만 수정한다.
8. 변경된 새 후보는 새 diagnostic 버전 또는 사전 고정 cross-fit 바깥 fold에서 ceiling·`Δ/Λ/Φ/Γ`와 구조 경계를 다시 실행한다. 잔여 actionable oracle gain이 없다는 결론은 fresh-confirm에서 Holm-adjusted 단측 상한이 사전 `ε_gain` 이하인 동등성 기준을 통과한 대조에만 내리고, 나머지는 `판정 불가`로 남긴다.
9. Jp recall이 확인된 병목일 때만 LLM 가설 제안자를 도입하고, 제안자별 생성량이 아니라 Ja를 통과한 추가 coverage와 정밀도로 비교한다.
10. 최종 수정 뒤 code, model weight/API version, prompt·decoding, engine 설정, policy, schema, adapter·runner·canonicalizer·evaluator, projection, dependency·tablebase 버전, seed, 기본 annotation hash와 open-world adjudication 절차, 역할 배정·cross-fit schedule hash와 접근 정책, 봉인된 전체 corpus·원자 cluster·split 입력 해시를 하나의 signed candidate manifest로 freeze한다. Manifest에는 실행 뒤 생기는 출력, 판정 결과, run 허용 집합을 넣지 않는다. 이전 선택·수정에 전혀 쓰지 않은 fresh-confirm을 이 candidate manifest로 한 번 실행한다. 실행이 끝나면 통계 unblinding 전에 candidate-manifest hash, split ID와 입력 해시, 모든 중간·원시 provider I/O artifact, source-blind adjudication 결과, append-only 접근 로그 hash, 최종 run oracle graph·허용 집합 해시를 append-only signed run attestation으로 남긴다. API weight snapshot을 고정할 수 없으면 provider version과 원시 I/O artifact를 attestation에 필수로 남긴다. 실패하면 후보를 unfreeze하고 blind는 열지 않으며, 다음 후보에는 새 fresh-confirm 버전을 쓴다.
11. Fresh-confirm을 통과한 동결 후보만 독립 실행자가 봉인 blind에서 최종 시스템 성능을 한 번 평가한다.
12. Fresh-confirm과 blind의 서로 다른 signed run attestation이 동일한 candidate-manifest hash를 참조하고 각자 봉인된 split hash와 run별 허용 집합을 완전하게 기록했는지 확인한 뒤, 기준을 만족한 candidate manifest·두 attestation·코퍼스 해시를 함께 고정한다.
13. fresh-confirm 또는 blind 결과를 개발에 사용했다면 다음 후보는 새 코퍼스 버전으로 평가한다.

## 11. 무엇이 증명되고 무엇이 남는가

### 구조적으로 증명되는 것

그래프 삽입은 동일 ID의 다른 레코드를 거부하고, packet factory는 다음의 의미 결합을 확인한다.

- line node와 합법 재생·평가 evidence가 같은 line과 값을 가리킨다.
- candidate comparison의 delta·승률·mate·verdict가 등록된 두 line에서 정규 생성한 값과 일치한다.
- 후보군 descriptor의 소유자는 하나이고 실제 best/second pair 및 top-3 근거와 일치한다.
- transition의 양 끝 position이 등록되어 있고 transition evidence가 실제 edge와 일치한다.
- 상대평가 evidence가 등록된 played/reference line·transition과 정확히 일치한다.
- claim의 모든 evidence와 상호작용 evidence가 등록되어 있다.
- evidence parent 관계가 닫혀 있고 순환하지 않는다.
- player-facing Cause 집합이 직접 채널 준비도, provisional 비교 공개 판정과 단일 `RelativeCauseDominancePolicy`의 최종 fallback 억제를 중앙에서 다시 계산한 결과와 일치한다.
- packet의 Cause disposition ID 집합이 C의 전체 RelativeCause ID 집합과 정확히 같고, R에 도달한 각 disposition이 canonical dominance/cross-comparison 결과와 일치하며, `selected` ID와 owner가 packet의 실제 selected Cause 집합과 정확히 같다. Packet은 이미 끝난 Jp/Ja 결정을 다시 추론하지 않고 원장의 전수 coverage와 R 일관성만 검증한다.
- 공개 `idea_status_detail`은 위 원장만 입력으로 삼는다. primary comparison이 engine-backed인 full/renderable 투영에서는 공개 idea Cause ID 집합이 원장의 selected 집합과 정확히 같을 때만 `idea_status`와 explanations를 완성한다. primary가 불충분한 withheld 투영은 public ideas를 비우되 보조 비교에서 이미 selected인 원장을 summary에 보존한다. 어느 경로도 Jp/Ja/R을 재실행하거나 Cause/fallback을 생성하지 않는다.
- 선택된 각 Cause가 정확히 한 승인 claim host에 배치되고 R의 비교 공개 권한 `comparison_exposure_rank`와 typed importance dominance layer를 반영한 `selection_order`가 packet에 함께 보존된다.
- 각 Cause 선택이 같은 Cause의 등록된 직접 채널을 `causal_signature`, carrier와 `directChange`로 정확히 가리킨다.
- 선택된 Cause가 공개 단계에서 재해석 불능이면 조용히 생략하지 않고 invariant 위반으로 실패하며, 선택 채널의 `proof_segment`는 오직 해당 `rootOwnedProof`에서만 생성된다.
- typed importance 해석은 선택 집합을 변경하지 않으며, 승인된 root-owned effect 중 같은 semantic root board와 `PlayerFacingImpact` 아래 typed 단위가 비교 가능한 결과만 frontier와 dominance로 표시한다. `PlayerFacingIdeaOrderingPolicy`는 이 부분순서의 layer에 따라 idea unit과 그 Cause facet의 `selectionOrder`만 재부여한다. Packet의 모든 profile 관계와 Cause별 측정 coverage는 공개 JSON에 channel signature 단위로 보존된다.

공개 투영은 이 factory를 통과한 packet만 받는다. 일반 아이디어·계획은 R이 선택한 Cause의 정확한 `DirectCauseChannel`만 읽고, claim은 그 Cause의 유일한 host로만 사용한다. Verdict는 검증된 player-facing `Primary` `PlayedVsBest` 비교만 읽으며 각 idea는 자기 Cause의 comparison을 그대로 보존한다. 독립 line/tablebase/exact payload는 공개 준비도를 만들 수 없다. 따라서 Cause에서 직접 proof channel로 거슬러 올라가는 귀납으로 모든 인과 근거가 같은 유한 DAG 안에 있음을 보일 수 있다. Packet의 selection DTO와 `proof_segment`는 새 의미를 복사하거나 재판정하지 않고 등록된 Cause·채널·root-owned proof의 exact key와 exact move만 운반하므로, 끝단 표현이 두 번째 판단 권위가 되지 않는다.

이 fail-closed 변경은 Cause로 아직 승격되지 않은 정확한 zugzwang 설명의 공개 recall을 줄인다. 이는 독립 exact fallback을 되살려 보완하지 않는다. 필요한 coverage는 `EndgameZugzwangProof`를 정확한 source·actor·target·mechanism·consequence와 비교 관점에 결속한 `EndgameHorizon` Cause channel 생성으로 복원해야 한다.

이 성질은 테스트 개수로 성립하는 것이 아니다. 생성 경로와 공개 경계의 조건으로 성립하며, 남은 소수 테스트는 외부 계약의 연결이 깨지지 않았는지만 확인한다.

### 실증해야 하는 것

구조만으로 “사람이 가장 중요하게 보는 아이디어를 골랐다”는 명제는 증명되지 않는다. 현재 production은 단일 증거 그래프, 인증, 직접 투영 경계와 동일 typed 단위 안의 제한적 중요도 부분순서를 제공한다. 교차 family 우선순위와 미측정 proof의 인간적 중요도는 여전히 실증 대상이다. LLM 후보 제안자, 단계별 oracle evaluator, 문장 verifier는 이 경계를 소비하는 후속 실증 장치이며 배포 코드 안에 정답 코퍼스를 복제하지 않는다.

런타임 해설력의 완성은 보편적인 수학 명제가 아니라, 미리 고정한 대상 분포에서 다음을 동시에 만족한다는 통계적 주장이다.

1. 구조 오류가 0이다.
2. 정밀도와 coverage/abstention 하한을 모두 만족한다.
3. family·게임 단계별로 사람 대비 비열등성 신뢰구간을 통과한다.
4. actual-Q/stable-Q, coherent all-oracle ceiling, one-stage, leave-one-actual, 양방향 누적·factorial 개입을 독립 confirm에서 재현해 남은 손실을 자료 준비 또는 특정 단계 조합에 귀속하고, 귀속되지 않은 잔여 oracle gain은 사전 `ε_gain`에 대한 단측 상한 동등성 기준을 통과하거나 `판정 불가`로 남긴다.

이 구분을 지키면 구조적 정당성을 테스트 양으로 대체하지 않으면서, 모델 부족·인증 정책·투영·자료 결함을 반증 가능한 방식으로 분리해 해설력을 완성할 수 있다.
