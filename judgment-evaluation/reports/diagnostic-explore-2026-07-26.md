# Judgment bottleneck diagnostic — 2026-07-26

## 결론

현재 **생산 판단 품질의 병목 단계는 판정 불가**다. 모든 최종 보고서에서 `production_bottleneck`은 `null`이다.

대신 서로 다른 두 지점은 정확히 확인됐다.

1. 실제 운영 native 경로는 세 표본 모두 **Q → F → C → Jp → Ja → R → P**에 도달했다. V는 운영 verbalizer가 없어서 `typed-unavailable`이다.
2. Frozen-v1 의미 실험은 actual native F를 v1 `TypedEvidenceGraph` 의미 계약으로 옮기는 무손실 bridge가 없어 **F 입력 경계에서 처음 중단**된다.

두 번째는 생산 F의 품질 실패가 아니라 **평가 인터페이스의 첫 차단점**이다. 현재 2원자 cluster, held-out human 0명, 독립 coherent semantic oracle chain 0개이므로 이 차단점을 넘어 특정 생산 단계를 원인으로 귀속할 통계적·실험적 근거가 없다.

## 여기서 병목의 뜻

이 작업의 병목은 지연시간, CPU, 처리량이 아니다. 공통 최종 endpoint `E`의 실패를 제한하는 stage 또는 인접 interface의 **의미 품질 병목**이다.

- production bottleneck: 실제 단계 의미를 oracle 의미로 바꿨을 때 `E`가 인과적으로 회복되고, 두 독립 chain·cluster inference·Holm 보정을 통과한 원인
- experiment/interface blocker: 그 인과 실험을 정확한 계약으로 실행하지 못하게 하는 관측·mapping·adapter 차단
- evidence/power blocker: 실행은 가능해도 표본·평가자·독립 oracle가 부족해 귀속을 확정하지 못하는 상태

현재 상태는 각각 `null`, `native F → frozen-v1 F semantic bridge`, `2<30 clusters / 0<3 humans / 0<2 semantic oracle chains`다.

## 영어 backend 정합성

판단·시스템 backend의 canonical language는 영어다. 초기 외부 설정에 `verbalization_policy.language: ko`와 한국어 `labels.natural_language`가 들어간 것은 요구사항이 아니라 구현 설정 오류였다.

최종 corpus `judgment-boundary-v1.3.0`에서는 다음을 강제한다.

- `preregistration.verbalization_policy.language == "en"`
- 모든 explore 판단 라벨의 `language == "en"`
- 판단 라벨, blind model-proxy view/vote/aggregate, provider I/O는 영어
- `doctor`가 위 조건을 검사하고 불일치 시 실패
- 한국어는 README, 이 보고서, 사용자 대화 같은 설명 표면에만 사용

최종 raw SHA-256:

- corpus manifest: `bc3159dc1c5d358f9fd3de9db1e49b74fe73402a4fc74044540b6e2e163d8bfe`
- preregistration: `913f6a04b50e79aa482e0bbfcb6ed43c7aeb233b8fa4752904003cf2c6fc0bdd`
- diagnostic-explore JSONL: `2c89aae238cb55212d729aa2d48fa985b94e440f83b251b2f2bc8ab8b7735c15`

## 고정된 평가 표면

- Q/F/C/Jp/Ja/R/P/V 입력·출력 JSON Schema 16개와 공통 envelope
- raw input/output, validated/canonical output, provider I/O, metadata의 불변 저장
- 내용 해시와 append-only hash-chain artifact/access ledger
- actual, format-control, oracle, replay, shell-free live-command adapter
- source/arm-blind endpoint `E` 평가 계약
- 참조·ID 관계를 보존하고 비의미 형식만 정규화하는 canonicalizer
- paired atomic-cluster bootstrap, Holm 보정, 별도 attribution/equivalence threshold
- signed candidate manifest와 pre-unblinding/finalization 분리
- 313개 frozen arm: all-actual, stable-Q, all-oracle, one-stage, leave-one-actual, forward/backward cumulative, 7개 adjacent factorial

고정 plan SHA-256은 `bc6447d322f43162487be5b219dddfebea3c4d4847c0dd3c78a5dadd22eb9beb`이다.

## 코퍼스와 전문가 재구성

`diagnostic-explore`는 3표본, 2원자 cluster다. 두 원자 cluster는 Grünfeld `...b5` 아이디어와 Ng5/Nh4 counterfactual이다. 참고 서적은 문구 정답이 아니라 아이디어 seed로만 사용했다. 원문·이미지·추출문·로컬 전체 경로를 저장하지 않고 `{document_id, pdf_page}`만 남긴 뒤 비교 수, 인과, actor/target/mechanism/consequence와 필수 PV를 독립 재구성했다.

모든 라벨은 `single-reconstruction`, `reconstructed-explore-only`다. 복수 독립 전문가 합의, confirm 또는 blind gold를 대신하지 않는다.

## Richer/stable Q

최종 run은 `q-stable-diagnostic-20260726-v11`이다. Stockfish 17.1을 depth 20, MultiPV 4, fresh process 3회, Threads 1, Hash 64MB로 실행했다. 실행 파일 SHA-256은 `5f95eaea0d4eb697381989187ce6eb4d6ad59283c34421765ecc73cdb09ba766`이며 raw provider I/O에는 절대경로 대신 `sha256:<digest>`만 기록된다.

| 위치 | root-side 결과 | 반복 진단 |
|---|---|---|
| Grünfeld, Black to move | `b7b5 -6`, `c8d7 -77`, `f6d7 -82`, `f6e8 -92` cp | top-4 집합·순서·점수 안정, missing-evidence rate 0.20 |
| Game Changer, White to move | `f3g5 +61`, `f3h4 +44`, `h1c1 +33`, `a2a4 +23`; forced `d3c2 +17` cp | top-4 집합·순서·점수 안정, missing-evidence rate 1/6 |

두 counterfactual 표본은 같은 위치와 Q pack을 공유하되 played move만 다르게 결속한다. 이 결과는 Q의 재현성과 후보 coverage를 보여 줄 뿐 Q가 생산 병목이 아니라고 증명하지 않는다.

- immutable run artifact report SHA-256: `bed846487bb0cc08343abe6103cc7aebddabc4e10fce05e2ba67d2584a580b9f`
- `reports/` JSON copy SHA-256: `4bb87d5f82b642a4abd34c3707f13bd16e85451180c55c27fd28d928aee97c92`
- ledger SHA-256: `df5a687614ad88e27f6f7c4c1047e9481ff7f647ea6bb6cee840becfdbd5b61a`

## Native Q–P 관측

최종 run은 timeout 결속과 단일 실행 보존을 보강한 뒤 다시 취득한 `native-engineering-diagnostic-20260726-v5`다.

- registered arms: 313
- planned/materialized rows: 939/939
- 실제 provider invocation: 3
- provider-completed rows: 3
- `typed-unavailable` rows: 936
- invoked/completed arms: 1/1
- actual baseline은 세 표본 모두 identity 및 `actual-native`
- 세 표본 모두 Q→P observed
- V는 세 표본 모두 `typed-unavailable`

936행은 호출한 것으로 세지 않는다. Frozen-v1 oracle/format-control 의미를 native-v2 selection으로 가장하지 않고 명시적 부재 행으로만 남겼다. Actual baseline arm과 row는 non-identity override를 거부한다.

각 실제 호출의 invocation, raw provider I/O, validated result는 별도 artifact다. Provider command argv, executable, working directory와 adapter identity는 로컬 문자열이 아니라 SHA-256 결속만 저장된다.

V2 command provider binding은 실제 deadline과 동일한 `300000`ms를 포함하며 binding SHA-256은 `96dfff1b7c5488a0a21717b3d773a417bb4656a762131f8a63aabecdf80d0c10`이다.

- immutable run artifact report SHA-256: `4100126614239dbff454a5a9e482077da9fd3b70b728dc8f8f6d9004ebf936c7`
- `reports/` JSON copy SHA-256: `f2062edfcd8c0b2f3aa0fc97bd9edb033946ac8d34caff0f4c183d98191e1bb2`
- ledger SHA-256: `4f6fcca91a8b3edf33710632696ecc71f2f66a4245fce8656c5940623b359986`

이 mode는 `engineering-mode-is-hard-sealed-against-inference`이며 생산 귀속을 만들 수 없다.

## Frozen-v1 gated runner

최종 run은 `bottleneck-diagnostic-20260726-v7`이다. 313개 등록 arm 중 foundation 구간 9개만 실행했다.

- all-actual: 세 표본 모두 Q를 capture한 뒤 frozen-v1 F에서 `unavailable`
- all-oracle team-a/team-b: 독립 oracle Q pack이 없어 `not-passed`
- identity/null, same-seed, sentinel/blind-duplicate 외부 control: `not-run`
- canonicalizer full-chain control: 완전한 8단계 baseline이 없어 `not-passed`
- one-stage, leave-one-actual, 누적, factorial, 통계 대조: gate가 닫혀 실행하지 않음

상태는 `foundation-not-passed`, 귀속은 `indeterminate`, 생산 병목은 `null`이다.

- immutable run artifact report SHA-256: `229db104aefe153ce15d1de24ed62cf74a4d8d7b53eba71a68e33db4216a12a7`
- `reports/` JSON copy SHA-256: `2cdedeb60499414e6a86c0cdbedf91e37c02d88d37abdf197638e969533c2a2c`
- ledger SHA-256: `97cd0fe6a719a6c4eedfc3d93365be28ef32dbd6c22a78367c97b3ac15d679b4`

## Blind model + Stockfish proxy

사용자 제안에 따라 별도의 세 *Chess Structures* 연습 위치에서 세 역할의 fresh subagent가 source와 정답을 보지 않고 영어 출력만 평가했다. Source key는 9개 vote와 3개 aggregate가 원장에 고정된 뒤 공개됐다. Stockfish 증거와 책의 아이디어를 독립 재구성해 결합했으며 원문 prose나 이미지는 저장하지 않았다.

Run `blind-model-proxy-20260726-v3` 결과:

| blind item | core idea mean / range | usefulness mean / range | engineering proxy E |
|---|---:|---:|---:|
| `proxy-7c61a8d4` | 0.950 / 0.050 | 0.930 / 0.050 | 1 |
| `proxy-29bd52e7` | 0.973 / 0.030 | 0.957 / 0.020 | 1 |
| `proxy-f0e319ab` | 0.843 / 0.230 | 0.873 / 0.160 | 1 |

세 항목 모두 engineering proxy는 통과했다. 다만 evidence tier는 `model-proxy-nonhuman`, held-out human 수는 0이고 `human_gate_eligible`, `release_eligible`, `attribution_eligible`은 모두 false다. 따라서 이는 인간 평가의 유용한 사전 점검이지 인간 평가 자체도, 생산 병목 증거도 아니다.

- immutable run artifact report SHA-256: `7c4dc47b05ddb74f8b99de6e7fa81ab8ad9ab22f4064e93c1aa389844a4d1d44`
- ledger SHA-256: `d4b45c426a49b861efccb0dbffbcb32b8fcb6d9bf1d928bde4193f7a4236c820`
- access-log SHA-256: `0d240b2e7dc8d27e890823d12d2a9fb34f24c9b499a7e2b7764d42476750542b`

## 운영 무개입 경계

호출별 intervention이 모두 비어 있으면 공개 `assemble`, `packet`, runtime evaluation은 기존 actual assembler 순서와 예외 전파를 유지한다. 외부 callback만 실패 폐쇄된다. P selection은 원 native public probe object와 exact equality를 요구하므로 필드 삭제·추가가 허용되지 않는다. 전역 스위치, 환경변수 또는 파일로 켜지는 우회 경로는 없다.

`chesstory-runtime`과 외부 `runtime-adapter`는 이 경계 수정 후 순차 컴파일을 통과했다. 테스트 파일은 추가하거나 수정하지 않았고, 운영 판단 모듈·체스 규칙·판단 정책을 외부 하네스에 복제하지 않았다.

## 10절 release gate

Fresh-confirm 또는 blind를 열지 않았고 candidate를 freeze하지 않았다.

- corpus qualification: `harness-and-diagnostic-explore-only`
- confirm/fresh-confirm/blind: 독립 annotation이 없는 봉인 예약본
- held-out human evaluator: 0명
- coherent semantic oracle chain: 0/2
- effective atomic clusters: 2/30
- runtime V: unavailable
- frozen-v1 F–P semantic bridge: unavailable

따라서 현재 얻을 수 있는 최대 결론은 다음과 같다.

> 실제 native Q–P 체인은 세 진단 표본에서 도달한다. Frozen-v1 병목 실험의 첫 interface blocker는 F semantic bridge다. 그러나 생산 품질 병목 단계는 아직 판정할 수 없다.

## 다음에 필요한 독립 입력

1. 새 corpus version 아래 최소 30원자 cluster와 held-out human 3명 이상을 확보한다.
2. 서로 결과를 보지 않은 두 팀이 stable-Q→F→C→Jp→Ja→R→P→V coherent semantic oracle chain을 만든다.
3. Native F–P와 frozen-v1 사이의 무손실·검증 가능한 bridge를 정의하거나 frozen semantic contract 자체를 native 계약에 맞춰 새 버전으로 등록한다.
4. Source/arm-blind evaluator와 identity/null, same-seed, sentinel duplicate control을 제공한다.
5. 두 all-oracle ceiling이 모두 통과한 뒤에만 Δ/Λ/Φ/Γ와 adjacent factorial을 실행한다.
6. 선택한 가설은 독립 confirm에서 재현하고, 새 후보의 fresh-confirm 동등성까지 통과한 뒤에만 blind를 한 번 연다.
