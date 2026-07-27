# Chesstory Judgment Evaluation Harness

이 디렉터리는 Chesstory의 판단 품질을 배포 코드 밖에서 식별하기 위한 외부 평가 하네스다. 판단 과정을 `Q → F → C → Jp → Ja → R → P → V`의 버전된 계약으로 나누고, 실제 산출물과 같은 스키마의 전문가 oracle을 단계별로 교체해 최종 실패가 어느 인터페이스에서 회복되는지 측정한다.

이 하네스가 존재하거나 `doctor`가 통과한다는 사실만으로 생산 판단의 정확성, 설명 품질, 병목 식별이 증명되지는 않는다. 현재 생산 경계와 예약 코퍼스로 얻을 수 있는 결론은 **판정 불가(indeterminate)** 다.

기계가 소비하는 판단 계약·코퍼스 라벨·provider 입출력·프록시 평가는 영어를 canonical backend language로 사용한다. `preregistration.verbalization_policy.language`와 각 explore 라벨의 `language`는 모두 `en`이어야 하며 `doctor`가 이를 강제한다. 이 README와 사용자 보고서의 한국어는 설명 표면일 뿐 실행 payload에 들어가지 않는다.

## 목적과 비목표

목적은 다음과 같다.

- 각 단계의 입력·출력을 동일한 JSON Schema로 고정한다.
- 원시 입력·출력, 검증본, 정규본, provider I/O, 버전·seed·해시를 재현 가능한 artifact로 보존한다.
- actual, format-control, oracle을 같은 adapter 인터페이스로 주입한다.
- all-actual, stable-Q, all-oracle, one-stage, leave-one-actual, 양방향 누적 대체와 인접 factorial arm을 동일 조건에서 실행한다.
- 원자 클러스터 단위 통계로 효과와 형식 효과를 분리하되, foundation control과 all-oracle ceiling을 먼저 통과시킨다.

명시적인 비목표는 다음과 같다.

- 운영 기본 응답과 판단 결과를 바꾸지 않는다. 운영 쪽 변경은 호출별·기본 무효인 stage 경계와 typed trace에 한정한다.
- 배포 테스트 코드를 늘리거나 봉인 코퍼스를 운영 모듈의 테스트 fixture로 옮기지 않는다.
- 운영 모듈, 체스 규칙, 판단 정책을 이 디렉터리에 복제하지 않는다.
- 공개 응답을 관측했다는 이유로 노출되지 않은 중간 stage artifact를 추정하거나 만들어 내지 않는다.
- 참고 서적의 원문, 이미지, 분석 페이지 또는 PDF 파일을 재배포하지 않는다.

## 판단 단계

| 단계 | 책임 | 대표 진단 |
|---|---|---|
| Q | 입력과 엔진·probe·tablebase 증거 취득 | 후보 coverage, 반복 안정성, 평가 분산, 누락률 |
| F | 합법 수순, 보드 사실, 평가 관점과 비교 수치 확정 | legality, perspective, fact exact accuracy |
| C | 후보선 차이의 actor, target, mechanism, consequence 결속 | 원인 방향과 구성요소 exact match |
| Jp | 사람이 중요하게 볼 판단 후보와 plan event 제안 | 허용 대안 집합에 대한 후보 recall |
| Ja | 참인 후보 승인과 거짓 후보 거부 | precision-constrained recall |
| R | 승인 claim의 중복 제거와 중요도 순위 결정 | 핵심 claim top-1 agreement |
| P | packet 의미를 공개 JSON으로 손실 없이 투영 | 의미 필드 보존율 |
| V | 투영된 의미를 근거 제한 문장으로 표현 | 근거 위반 0 하의 유용성 |

`Jp`와 `Ja`를 분리해야 탐색 실패와 과잉 거부를 구분할 수 있고, `P`와 `V`를 분리해야 투영 손실을 문장화 실패로 오인하지 않는다.

## 구현 구조

| 위치 | 역할 |
|---|---|
| `schemas/v1/` | Q/F/C/Jp/Ja/R/P/V 각각의 입력·출력 스키마 16개와 공통 envelope 계약 |
| `src/chesstory_eval/schemas.py` | 스키마 참조 해석, registry 검증, stage 문서 검증 |
| `src/chesstory_eval/capture.py` | 원시 입력·출력, validated/canonical 출력, provider I/O와 metadata의 불변 저장, 해시 연결 ledger와 접근 로그 |
| `src/chesstory_eval/adapters.py` | actual/oracle/replay/외부 command 산출물을 같은 stage 호출 계약으로 변환하고 가용성 경계를 강제 |
| `src/chesstory_eval/stockfish.py` | 운영 런타임 밖에서 richer/stable Q 증거를 취득 |
| `src/chesstory_eval/design.py` | 두 독립 oracle chain을 사용하는 고정 intervention arm 계획 생성 |
| `src/chesstory_eval/runner.py` | schema 검증, adapter 호출, artifact capture, foundation gate, ceiling gate와 단계 개입 실행 |
| `src/chesstory_eval/canonicalize.py` | 참조를 보존한 ID 재할당과 스키마가 허용한 비의미 형식만 정규화 |
| `src/chesstory_eval/statistics.py` | 원자 클러스터 paired bootstrap, 사전 등록 대조량과 Holm 다중비교 보정 |
| `src/chesstory_eval/evaluation.py` | source/arm-blind 최종 endpoint `E`의 diagnostic replay와 release live 평가 계약 |
| `src/chesstory_eval/native_diagnostic.py` | frozen v1의 313개 arm을 native-v2 shadow로 물질화하고 actual Q–P invocation/result/provider I/O 또는 구조화된 provider failure를 별도 결속하는 비추론 진단 runner |
| `src/chesstory_eval/model_proxy.py` | role-blind 비인간 평가 vote를 검증·집계하되 human/release/attribution 자격으로 승격하지 않는 보조 계층 |
| `corpus/v1/` | manifest, 사전 등록, 역할·접근 정책, explore와 봉인 예약 split |
| `references/` | 로컬 참고 PDF의 불투명 ID·해시·페이지 메타데이터만 보존하는 색인 |
| `runtime-adapter/` | 운영 판단 규칙을 복제하지 않고 native Q–P ADT와 typed V 부재를 결정적으로 캡처하는 얇은 외부 Scala 어댑터 |

### Schema와 artifact capture

모든 stage 문서는 공통 envelope 안에 stage, sample/cluster/split/run ID, seed, 입력 해시, upstream artifact 해시, producer 버전과 payload를 가진다. Runner는 다음 단계로 넘기기 전에 입력과 출력을 해당 버전의 스키마로 검증한다.

Artifact store는 stage별 raw input, raw output, validated output, canonical output, provider I/O와 metadata를 각각 내용 해시로 저장한다. 기존 내용과 다른 bytes로 같은 artifact를 덮어쓰지 않으며, ledger와 봉인 접근 로그는 이전 event hash를 잇는 append-only chain으로 검증한다.

### Adapter와 runner

Stage adapter는 실제 산출물과 oracle 산출물을 같은 입력 payload와 같은 출력 스키마 아래에서 호출한다. Replay는 sample, stage, source, oracle chain, 실험 문맥과 입력 해시가 일치하는 artifact만 사용할 수 있다. 열린 진단 경로는 코퍼스의 외부 취득 Q, `--stage-replay`, `--evaluator-replay`를 계속 사용할 수 있다. Live 경로는 `--live-command-config`의 shell-free 명령만 호출하며, 명시된 route가 provider I/O 뒤 실패하면 replay로 조용히 대체하지 않는다.

Live config의 최상위 계약은 `chesstory.eval.live-command-config.v1`이며 `stage_routes`와 단일 `evaluator_route`를 가진다. 각 route는 `argv` 문자열 배열, 1–3600초의 `timeout_seconds`, 명시적 `env` 객체와 `bound_files` 배열을 반드시 포함한다. 각 bound-file 항목은 정확히 `{argv_index,path,sha256,size}`이고, `path`는 해당 `argv` 원소와 문자 단위로 같은 canonical absolute regular non-link file이어야 한다. `argv[0]`은 반드시 결속되므로 PATH 탐색 실행은 허용되지 않는다. 실행 파일뿐 아니라 argv로 전달하는 script, JAR, model, engine 파일도 각각 결속해야 한다. Oracle route는 `oracle_chain`도 포함한다. 알 수 없는 필드, 중복 JSON key/route, 비유한 수, shell 문자열, NUL, secret/token/password/key 계열 환경변수와 알려진 secret 형태의 값은 거부된다. 봉인 실행은 Q/F/C/Jp/Ja/R/P/V의 actual 8개와 사전 등록된 두 chain별 oracle 16개 route가 정확히 모두 있어야 한다.

Candidate freeze는 component root 안의 config와 bound file을 `live-command-config` component의 exact file manifest에 넣고, root 밖 파일은 별도 signed `external_executable_binding`에 canonical absolute-path fingerprint, byte SHA-256, size와 route/argv 사용처로 결속한다. 이 결속은 split open 전과 각 호출 직전·직후 다시 검증된다. Release 실행은 Windows에서 각 파일을 read-only, share-read-only handle로 열어 호출이 끝날 때까지 write/delete/rename sharing을 금지한다. 이 보장이 없는 플랫폼의 release 실행은 명시적으로 fail-closed한다. 이는 argv 밖에서 프로세스가 암묵적으로 여는 미신고 파일까지 자동 발견한다는 뜻은 아니므로 route 소유자는 모든 실행 의존 파일을 `bound_files`에 열거해야 한다.

Evaluator의 stdin은 canonical V에서 만든 `chesstory.eval.source-blind-view.v1` 객체 그 자체뿐이다. Sample ID와 canonical-output hash는 arm/source/oracle/provider 정보가 없는 전용 control-plane 환경변수로 전달하며, 응답이 두 값을 정확히 echo해야 한다. Release 응답은 endpoint policy의 정확한 rater roster, rater별 `{evaluator_id, score}` vote와 그 산술평균인 `usefulness_consensus`를 포함해야 한다. 평가 artifact, stdout/stderr/base64를 포함한 raw provider I/O와 각 rater의 접근 event는 immutable run tree와 append-only 접근 로그에 남는다.

고정 계획은 두 독립 oracle chain에 대해 actual/format-control/oracle 조건, stable-Q, all-oracle, one-stage, leave-one-actual, forward/backward substitution, 인접 stage factorial을 포함한다. 현재 계획은 313개 arm이다. Runner는 하네스 identity/null control과 두 chain의 all-oracle ceiling이 통과한 뒤에만 개입 arm과 통계 대조를 확장한다.

### Canonicalizer와 statistics

Canonicalizer는 스키마가 명시적으로 허용한 세 변환만 한다.

- identity/reference 관계를 함께 보존하는 opaque ID 재할당
- `unordered-set`으로 선언된 배열의 표현 순서 정규화
- `null-equivalent-omission`으로 선언된 `null`의 생략

각 arm은 Q부터 V까지 하나의 canonicalization/transition session을 사용한다. 앞 단계에서 정의된 ID mapping을 다음 단계가 이어받고, duplicate definition·dangling reference, Ja 밖 R claim, packet 밖 P 의미, P 밖 V citation과 허가되지 않은 move/square/piece span을 fail-closed한다. Confidence, ranked list 순서, 실제 missingness, 수치, 문구, 집합 cardinality는 바꾸지 않는다. 따라서 format-control은 작동 의미를 바꾸는 정책 개입이 아니다.

통계 계층은 같은 원자 클러스터의 treatment/control을 짝지은 bootstrap을 사용하고, 사전 등록된 단측 신뢰구간과 Holm 보정을 적용한다. 유의하지 않음은 동등성의 증거가 아니다. 잔여 이득이 없다는 결론에는 별도 fresh-confirm에서 `epsilon_gain`에 대한 단측 상한 기준을 통과해야 한다.

병목 귀속은 foundation control과 두 all-oracle ceiling이 먼저 통과한 완전 실행에서만 결정론적으로 계산한다. `diagnostic-explore`는 Φ를 제외한 Δ/Λ/Γ 중 두 fixed oracle chain이 동일한 `(contrast, context, focus_stage, held_stage, held_source)` 좌표에서 별도로 사전 등록한 `attribution_minimum_delta`, 양의 단측 CI 하한, Holm-adjusted `p`, 최소 원자 cluster를 모두 만족한 항목을 후보 목록으로만 남기며 `production_bottleneck`은 항상 `null`이다. 잔여 이득 동등성 경계인 `epsilon_gain`이나 release 비열등성 delta를 양성 귀속의 최소 효과량으로 재사용하지 않으며, 별도 귀속 threshold가 없으면 fail-closed한다. `diagnostic-confirm`은 정확히 사전 등록된 positive hypothesis와 chain별 contrast ID가 같은 좌표로 재현될 때만 확정한다. 비-factorial 좌표는 `confirmed-stage-bottleneck`과 stage를 반환하지만, factorial Γ는 `confirmed-interface-conditioned-bottleneck`과 exact held interface를 반환하고 단일 stage `production_bottleneck`은 `null`로 유지한다. `fresh-confirm`과 `blind`는 새 병목 귀속을 만들지 않는다.

## 생산 런타임의 현재 관측 경계

운영 런타임에는 호출별 `JudgmentBoundaryIntervention`과 `RuntimeBoundaryIntervention`이 있다. 아무 개입도 넘기지 않는 기본값은 기존 경로와 같은 판단·공개 응답을 만들며, 전역 상태·환경변수·파일로 활성화되는 우회 경로는 없다. Oracle callback은 각 stage의 입력만 받고 실제 stage 산출물을 보지 않으며, 반환값은 질문 불변식, F/C 소유권, claim/evidence closure, rank·dedup 계보, packet-derived P payload와 probe 부분수열 조건을 통과해야 한다.

외부 native-v2 adapter가 정확히 관측하는 경계는 다음과 같다.

- **Q/F/C/Jp/Ja/R/P는 observed-native**: 정상적으로 packet이 만들어진 실제 요청에서 각 native ADT를 구조 손실 없이 캡처한다.
- **V는 typed unavailable**: 운영 verbalizer가 없으므로 빈 문자열이나 공개 응답을 V로 가장하지 않는다.
- packet이 만들어지지 않은 요청은 Q–R까지만 관측하고 P를 `unavailable`로 기록한다. 잘못된 요청은 도달하지 않은 모든 경계를 명시한다.
- 전체 observation은 stage별 SHA-256과 upstream hash chain을 가지며, 공개 응답은 별도 hash로 보존한다.

다만 native ADT와 frozen v1 의미 스키마는 같은 계약이 아니다. F/C context, Jp의 assertion·confidence, Ja의 Certified/Deferred/Rejected, R 상세 trace, P runtime projection을 v1 의미 필드로 옮기는 무손실·검증 가능한 bridge가 아직 없다. 따라서 native artifact를 억지로 v1 F–P에 주입하지 않으며, 현재 v1 병목 runner는 F에서 fail-closed한다. 이는 **생산 F 품질의 실패가 아니라 평가 인터페이스의 첫 차단점**이다. 두 독립 oracle chain과 source-blind evaluator도 준비되지 않았으므로 `production_bottleneck`은 계속 `null`이다.

## 참고 자료와 저작권 경계

`references/source-index.json`은 17개 로컬 참고 PDF에 대해 불투명 `document_id`, basename/title, SHA-256, byte size, page count와 오염 표식만 기록한다. 허용 locator는 1부터 시작하는 `{document_id, pdf_page}`뿐이다. 로컬 전체 경로, PDF 본문, 스크린샷, 추출 분석은 artifact나 코퍼스에 넣지 않는다.

체스 서적의 아이디어는 전문가 포지션 평가와 explore oracle 후보의 출발점으로 사용할 수 있다. 다만 결과는 다음 경계를 지킨 독립 재구성이어야 한다.

- 원문 문장을 복사하지 않고 포지션의 비교 수, 인과, actor/target/mechanism/consequence와 필수 PV를 새로 구성한다.
- 합법 수순, 보드 상태와 엔진 비교를 독립 검증한다.
- 실제 stage와 같은 스키마로만 기록하며 출처 문구를 근거 자체로 취급하지 않는다.
- 역사적 GoodNotes seed와 겹치는 9개 문서는 `explore_contaminated: true`로 유지하고 confirm/blind의 독립 근거로 재사용하지 않는다.

이 경계는 참고 아이디어의 채용을 허용하면서도 저작물 복제와 oracle 권위의 혼동을 막는다.

## 코퍼스 봉인과 현재 한계

원자 cluster는 game, opening lineage, tactical archetype, counterfactual 관계의 전이적 연결요소이며 split 사이에서 나눌 수 없다.

- `diagnostic-explore`: 열림. 현재 3표본, 2원자 클러스터이며 책 기반 독립 재구성과 하네스 진단에만 쓴다.
- `diagnostic-confirm`: 봉인된 1개 예약 레코드와 1클러스터. 독립 curator의 새 annotation을 기다리는 자리표시자이지 confirm-qualified set이 아니다.
- `fresh-confirm`: 봉인된 1개 예약 레코드와 1클러스터. 최종 후보를 고정한 뒤 한 번 실행할 새 확인 집합의 자리표시자다.
- `blind`: 봉인된 1개 예약 레코드와 1클러스터. 독립 blind custodian이 교체·관리해야 하는 자리표시자이지 release-qualified blind set이 아니다.

각 봉인 split은 허용된 독립 실행자 역할과 custodian token 없이는 열 수 없고, blind는 signed fresh-confirm PASS 뒤에만 한 번 열 수 있다. 개발자 역할의 열람은 거부된다. Release 역할 배열은 고정 순서·무중복이어야 하고 모든 배정 cluster는 manifest 전역 cluster 집합에 속해야 한다. 사람 ID는 `custodian-person:` 뒤 32자리 소문자 hex인 custodian-issued opaque ID만 허용하며, frozen roster는 issuer·registry SHA-256과 자연인별 alias 금지 선언을 함께 결속한다.

한 번 열기 claim의 `O_EXCL`만으로는 custodian 권한을 이길 수 없다. Custodian이 root나 claim을 삭제·복원할 수 없다는 보장은 하네스 내부 사실이 아니라 외부 신뢰 가정이다. 따라서 release access policy는 custodian root의 direct-child storage-attestation bytes를 SHA-256으로 지정해야 한다. 그 문서는 immutable store, 비-custodian write 금지, delete/rename 금지, ACL evidence hash와 동시에 custodian 삭제 권한이 외부 신뢰 경계임을 선언한다. Candidate freeze와 모든 sealed-store open/verify는 이 파일을 재검증하며, binding이 없거나 달라지면 fail-closed한다. 실제 저장소·ACL 운영과 custodian의 비삭제 의무는 독립 보안 통제와 감사를 받아야 한다.

봉인 파일의 해시가 맞는다는 사실은 접근 무결성만 뜻하며 라벨 품질이나 검정력을 뜻하지 않는다.

사전 등록 최소치는 30원자 클러스터, held-out human 평가자 3명, 독립 oracle chain 2개다. 현재 explore의 2클러스터와 세 봉인 예약의 각 1클러스터는 이 최소치에 크게 못 미친다. Manifest의 자격은 `harness-and-diagnostic-explore-only`, power 상태는 `insufficient-for-inference`다. 현재 자료로 효과 유의성, 단계 귀속, release 통과를 주장할 수 없다.

## 실행

Python 3.11 이상이 필요하다. 아래 명령은 이 디렉터리에서 실행한다.

소스 checkout을 바로 사용할 때 PowerShell에서는 다음처럼 module 경로를 설정한다.

```powershell
$env:PYTHONPATH = "src"
```

POSIX shell에서는 각 명령 앞에 `PYTHONPATH=src`를 붙일 수 있다. 또는 editable install을 하면 별도 `PYTHONPATH`가 필요 없다.

```text
python -m pip install -e .
```

### 계약과 봉인 점검

```text
python -m chesstory_eval doctor --root .
```

`doctor`는 영어 backend 정책, 16개 stage schema, open split과 참고 locator, sealed split의 개발자 접근 거부와 파일 해시, canonicalizer 불변식, artifact ledger control, 고정 arm 계획을 점검한다. 현재 통과 결과는 17개 참고 문서, explore 3표본·2클러스터, 313개 arm을 보고한다. 이는 하네스 건강 점검이지 생산 품질 합격이 아니다.

### 고정 intervention 계획

```text
python -m chesstory_eval plan --root . --output artifacts/arm-plan.json
```

`--output`을 생략하면 요약만 표준 출력으로 확인할 수 있다. 계획의 opaque arm ID, stage별 source, chain, 문맥과 plan hash를 결과 공개 전에 고정한다.

### Richer/stable Q 진단

```text
python -m chesstory_eval q-diagnostic --root . --stockfish STOCKFISH_EXECUTABLE --run-id q-diagnostic-explore --output artifacts/q-diagnostic-explore.json
```

이 명령은 열린 explore split만 대상으로 더 높은 depth·MultiPV와 반복 안정성 조건의 Q 증거를 외부 Stockfish에서 취득하고 원시 engine I/O까지 capture한다. 기본값은 depth 20, MultiPV 4, 3회 반복, 안정성 허용폭 15cp, Threads 1, Hash 64MB다. Q의 coverage와 안정성이 좋아져도 downstream stage나 생산 병목이 식별되는 것은 아니며, 현재 2클러스터에서는 보고서가 `indeterminate`여야 한다.

Q와 native diagnostic은 단일 실행 명령이다. 정규화된 run ID의 불변 디렉터리가 이미 존재하면 provider를 시작하기 전에 거부하며, `--output`도 같은 정규화 규칙으로 그 디렉터리 내부를 가리킬 수 없게 막는다.

### Native engineering diagnostic

```text
python -m chesstory_eval native-diagnostic --root . --run-id native-diagnostic-explore --sbt SBT_EXECUTABLE --adapter-root runtime-adapter --output reports/native-diagnostic-explore.json
```

이 명령은 embedded actual Q를 운영 request envelope로 엄격히 옮겨 실제 identity 경계를 호출한다. Frozen 313-arm 계획의 939행을 모두 보고서에 물질화하지만 provider를 호출한 행과 의미 bridge가 없어 `typed-unavailable`인 행을 별도로 센다. 각 실제 호출은 invocation, raw provider I/O, validated result를 서로 다른 불변 artifact로 저장하고, 실행 명령·작업 위치·실행 파일은 로컬 경로 대신 SHA-256 결속만 남긴다. Provider 응답은 호출 전체 기준 기본 300초로 제한되며 `--provider-timeout-seconds`로 0.001–3600초 범위에서 조정할 수 있다. 입력은 밀리초로 올림 정규화한 뒤 실제 deadline과 v2 provider binding에 같은 값을 사용한다. Timeout이면 invocation 다음에 경로 비노출 진단 해시·종료 결과를 가진 `native-provider-failure` artifact를 원장에 기록한 뒤 실패를 재전파한다. Native-v2 선택은 frozen-v1 oracle 의미를 주장하지 않으며 이 모드는 추론·생산 귀속을 구조적으로 금지한다.

### Blind model proxy

`model_proxy.py`는 source와 정답을 보지 않은 역할별 평가자의 영어 view/vote를 결속하고 Stockfish 증거와 함께 engineering endpoint를 집계할 수 있다. 이 결과의 tier는 항상 `model-proxy-nonhuman`이고 held-out-human 수는 0이다. 따라서 일치도가 높더라도 3명 이상의 독립 인간 평가자 조건, release gate 또는 생산 병목 귀속을 대신하지 않는다.

### Provisional full-chain proxy diagnostic

`provisional-proxy-diagnostic`는 운영 모듈을 복제하거나 호출 경로를 바꾸지 않는 외부 진단이다. Frozen 313-arm 계획과 explore 3표본의 939행을 모두 물질화하고, Q→F→C→Jp→Ja→R→P→V 산출물을 같은 frozen-v1 schema, canonicalizer와 append-only artifact ledger로 결속한다. 의미가 같은 stage prefix는 한 번만 실행·capture하되, 각 행은 원래 arm source와 공유 artifact hash를 명시적으로 참조한다. Cache key에는 sample, stage, effective source prefix와 upstream canonical output hash가 포함되므로 의미가 다른 실행을 합치지 않는다.

완성 probe report는 실행 전에 모든 참조 artifact의 경로와 SHA-256을 재검증하고 Q에 명시적으로 결속한다. Pre-fix, rank/projection-only, current-semantic-fix report도 각각 명시적으로 받아 같은 검증을 거친 뒤 `C→C→V` engineering trajectory로 별도 기록한다. 939행 matrix는 current report에 결속된 descriptive proxy reachability일 뿐이며 이 production trajectory의 근거로 가장하지 않는다. Machine-readable schema, receipt, stage output, evaluator와 report payload는 영어만 사용하고, canonical ASCII가 아닌 run ID나 artifact 내부 문자열은 capture 전에 거부한다. Expert label을 보조한 oracle과 영어 proxy V는 engineering reachability를 진단할 뿐이며, 독립 인간 평가·formal inference·release gate·`production_bottleneck`을 만들지 않는다.

```text
python -B -m chesstory_eval provisional-proxy-diagnostic --root . --run-id provisional-model-proxy-chain-20260727-v1 --native-run artifacts/native-engineering-diagnostic-20260726-v5 --oracle-q-run artifacts/q-stable-diagnostic-20260726-v11 --baseline-probe-completion-report artifacts/probe-completion-diagnostic-20260727-v1/report.json --intermediate-probe-completion-report artifacts/probe-completion-diagnostic-20260727-v2/report.json --probe-completion-report artifacts/probe-completion-diagnostic-20260727-v11/report.json --output reports/provisional-model-proxy-chain-20260727-v1.json
```

위 명령의 v11 결속 실행은 완료됐다. 외부 보고서 `reports/provisional-model-proxy-chain-20260727-v1.json`의 물리 SHA-256은 `634d3fe155b3b6784daaa51e0649f672818b64530f815b2ae014af9a88bb5773`이고, run 내부의 `artifacts/provisional-model-proxy-chain-20260727-v1/provisional-model-proxy-report.json`은 같은 JSON 의미를 가지며 물리 SHA-256은 `affd05266eee9ee23b2f2632f7ae32c4cf19aa440ac1da39b92bc61a8abef469`이다. `artifact-ledger.jsonl`은 445개 record, SHA-256 `14513314ac1408f825919adb416c21602a2f7f4e04dee97122a1f69855ef2258`, 최종 chain head `21d2db1023f454d054c7bcce180ef14a3931b5af7231455dade50b47ca6d8180`으로 검증됐다.

실행 수치는 313 arm, 939행, 7,512개 row-stage 참조, 357개 고유 stage capture, 7,155개 재사용 참조, 87개 고유 endpoint 평가다. Hash-bound runtime checkpoint의 실용적 engineering trajectory는 `C→C→V`이며 current primary locus는 미구현 verbalization 경계 `V`다. 이는 구체적인 수정·구현 우선순위 판정이다. 반면 세 explore 표본·두 원자 cluster, model-proxy 비인간 평가, 독립 인간 평가자 부재라는 통계적 한계 때문에 formal inference는 `indeterminate`, `production_bottleneck`은 별도 필드에서 `null`로 유지한다.

### Broad blind Cause audit

`cause-audit`는 기존 313-arm 실험을 다시 실행하지 않고 C 원인 판정과 그 원인에서 이어지는 Jp→Ja→R→P만 넓게 검사하는 외부 경로다. 운영 판단 규칙을 복제하지 않으며 `CauseAuditAdapterCli`의 compact native view, 기존 Stockfish 취득기, 선택적 `reply_multipv` probe 완료기, schema registry, canonicalizer와 immutable artifact store를 그대로 연결한다.

코퍼스와 oracle은 런타임을 호출하기 전에 먼저 freeze한다. 논리 split은 원본 JSONL의 역사적 partition을 바꾸지 않고, 라벨이나 런타임을 보지 않는 `minimum SHA-256 per cause_family` 규칙으로 8개 cause family 각각에서 정확히 1개를 `sealed_confirm`, 나머지 2개를 `explore`로 고른다. Freeze manifest는 salt, 입력 형식, 각 case의 원래/논리 partition, 선택 해시와 전체 assignment 해시를 모두 보존한다.

```text
python -m chesstory_eval cause-audit --root . --action freeze --run-id CAUSE_FREEZE_RUN --cases CASES.jsonl --oracle-label oracle-a=ORACLE_A.jsonl --oracle-label oracle-b=ORACLE_B.jsonl --adjudicated-label ADJUDICATED.jsonl --contamination-exclusion "CASE_ID=PRE_FREEZE_REASON" --output CAUSE_FREEZE.json

python -m chesstory_eval cause-audit --root . --action acquire --run-id CAUSE_Q_RUN --cases CASES.jsonl --manifest CAUSE_FREEZE.json --reference-labels ADJUDICATED.jsonl --partition explore --stockfish STOCKFISH_EXECUTABLE --output CAUSE_Q.json

python -m chesstory_eval cause-audit --root . --action run --run-id CAUSE_RUNTIME_RUN --cases CASES.jsonl --manifest CAUSE_FREEZE.json --acquisition CAUSE_Q.json --partition explore --stockfish STOCKFISH_EXECUTABLE --sbt SBT_EXECUTABLE --adapter-root runtime-adapter --output CAUSE_RUNTIME.json

python -m chesstory_eval cause-audit --root . --action compare --run-id CAUSE_COMPARE_RUN --cases CASES.jsonl --manifest CAUSE_FREEZE.json --labels ADJUDICATED.jsonl --runtime-run CAUSE_RUNTIME.json --partition explore --output CAUSE_REPORT.json
```

Stockfish root search와 런타임이 정확히 발행한 probe는 내용 해시 cache를 사용한다. `run`은 각 요청을 persistent JSONL adapter에 보내고, 발행된 probe만 검증·취득해 원래 request의 `probeResults`에 추가한 뒤 닫힐 때까지 다시 호출한다. 각 case에는 실제 C cause와 연결된 Jp claim, Ja 결정, R rank/dedup, packet/public P 도달 여부가 남는다. `compare`는 허용된 primary cause 중 하나가 played-vs-reference 관계와 직접 attribution을 만족하는지, 금지 fallback이 대신 선택됐는지, 경쟁 원인이 앞서 랭크됐는지, actor/target/mechanism/consequence typed binding과 downstream 단계가 어디서 사라졌는지를 센다. Oracle에 열거되지 않은 추가 cause는 금지 라벨이나 priority inversion 근거가 없는 한 자동으로 false positive 처리하지 않는다.

Typed object 진단은 하나의 Cause가 소유한 동일한 direct binding 안에 actor/target/mechanism/consequence가 함께 있는지를 검사한다. 서로 다른 binding, sibling Cause, ancestor/context evidence의 필드를 합쳐 완전한 원인으로 세지 않는다. Oracle의 자유 문구와 그 object가 체스 의미상 정확히 같은지, oracle의 white/black `source_side`와 runtime의 candidate/reference/shared/mixed가 같은 뜻인지는 문자열 유사도로 발명하지 않는다. Report는 이를 `not-performed-requires-blind-human-proxy-adjudication`으로 명시하며, 정확한 의미 일치는 별도 blind human-proxy adjudication이 맡아야 한다.

각 단계의 `--case-id`는 여러 번 줄 수 있다. 따라서 수정 뒤에는 영향 family의 case만 다시 adapter에 보내고, 나머지 Stockfish root/probe 결과는 내용 해시 cache에서 재사용할 수 있다. `--case-id`를 생략하면 선택 partition 전체를 실행한다.

봉인 oracle의 원문, 기대 cause, PV와 rationale은 freeze/report에 복사하지 않는다. Manifest와 case judgment에는 source file/canonical row hash와 verdict만 남긴다. `sealed_confirm`은 수정 후보를 고정한 뒤 같은 네 단계에서 partition만 바꾸어 한 번 확인한다. 이 24개 진단 코퍼스는 결함 family를 찾는 engineering audit이며 release 통계나 보편적 `production_bottleneck` 주장을 만들지 않는다.

Production `run`은 `explore`와 `sealed_confirm`을 한 호출에서 섞지 않는다. 봉인 실행에는 수정 후보를 나타내는 불변 파일을 `--candidate-binding`으로 반드시 주어야 하며 그 byte SHA-256을 runtime run에 결속한다. Engine-only `acquire`는 후보 고정 전에도 허용하지만, 이 결속 없이 `sealed_confirm` C→P를 호출하는 공식 하네스 경로는 거부된다.

### Gated experiment runner

현재 가용성 경계를 확인하는 smoke run은 다음과 같다.

```text
python -m chesstory_eval run --root . --run-id explore-smoke --split diagnostic-explore --output artifacts/explore-smoke-report.json
```

Embedded Q 뒤의 native 단계는 관측되지만 frozen v1 의미 bridge와 evaluator가 없으므로 이 실행은 완전한 병목 실험이 아니며 F `unavailable`, foundation-not-passed와 `indeterminate`를 기록하는 것이 정상이다.

실제 개입 실행에는 동일 입력 해시와 문맥에 맞는 stage replay, 두 coherent oracle chain의 산출물, source-blind evaluator replay와 필요한 foundation control artifact가 먼저 있어야 한다. 관련 옵션은 반복해서 줄 수 있다.

```text
python -m chesstory_eval run --root . --run-id explore-001 --split diagnostic-explore --stage-replay STAGE_REPLAY_JSONL --evaluator-replay EVALUATOR_REPLAY_JSONL --foundation-controls FOUNDATION_CONTROLS_JSON --candidate-manifest-hash CANDIDATE_MANIFEST_SHA256 --output artifacts/explore-001-report.json
```

`run`은 foundation control과 두 all-oracle ceiling이 모두 통과할 때만 one-stage, leave-one-actual, 누적 대체와 factorial arm을 확장한다. Explore에서 나온 방향은 개발 가설일 뿐이며, 독립적으로 다시 만든 새 confirm corpus에서 같은 방향이 재현되기 전에는 생산 병목으로 귀속하지 않는다.

### 봉인 실행의 두 단계 확정

`fresh-confirm`과 `blind`는 통계 분석을 실행과 같은 호출에서 만들지 않는다. 봉인 `run`은 stage/evaluator replay를 거부하고 signed candidate에 사전 결속된 `--live-command-config`만 사용한다. Config는 split을 열기 전에 한 번 읽어 원본 파일 SHA-256과 canonical document SHA-256을 고정하고 immutable run artifact, execution manifest와 execution bundle에 결속한다. 그 뒤 고정된 전체 계획을 gate 규칙에 따라 실행하고 `pre-unblinding-execution.json`만 남긴 뒤 `awaiting-pre-unblinding-attestation`으로 멈춘다. 이 시점에는 contrast, equivalence, experiment report가 존재하지 않아야 한다. 그 raw tree와 execution bundle을 phase A가 서명한 뒤에만 `finalize-run`이 통계를 계산한다.

```text
python -m chesstory_eval freeze-candidate --root . --candidate-id CANDIDATE_ID --components COMPONENTS.json --live-command-config LIVE_COMMAND_CONFIG.json --output SIGNED_CANDIDATE.json --key-id KEY_ID

python -m chesstory_eval run --root . --run-id confirm-001 --split fresh-confirm --candidate-manifest SIGNED_CANDIDATE.json --endpoint-policy FROZEN_ENDPOINT_POLICY.json --live-command-config LIVE_COMMAND_CONFIG.json --foundation-controls FOUNDATION_CONTROLS.json --adjudication primary=SOURCE_BLIND_ADJUDICATION.json

python -m chesstory_eval attest-run --root . --run-id confirm-001 --split fresh-confirm --candidate-manifest SIGNED_CANDIDATE.json --execution-manifest-path pre-unblinding-execution-manifest.json --execution-bundle-path pre-unblinding-execution.json --endpoint-policy-path pre-unblinding-endpoint-policy.json --oracle-graph-path pre-unblinding-oracle-graph.json --allowed-set-path pre-unblinding-allowed-set.json --adjudication primary=adjudication-primary.json --output SIGNED_CONFIRM_PHASE_A.json --key-id KEY_ID

python -m chesstory_eval finalize-run --root . --run-id confirm-001 --split fresh-confirm --candidate-manifest SIGNED_CANDIDATE.json --pre-unblinding-attestation SIGNED_CONFIRM_PHASE_A.json
```

`finalize-run`은 phase A 서명, candidate/run/split, bundle의 document hash와 서명 당시 run tree 전체를 정확히 확인한다. 서명 전 finalize, raw artifact 변경, 다른 bundle 대입과 재-finalize는 거부한다. Fresh-confirm의 release 자격은 이렇게 확정된 report와 equivalence만으로 별도 `qualify-fresh-confirm` phase B에서 판정한다. Blind도 같은 `run → attest-run → finalize-run` 순서를 사용하며, 시작 전에 동일 candidate에 대한 fresh-confirm PASS가 추가로 필요하다.

현재 포함된 diagnostic-confirm/fresh-confirm/blind 파일은 release-qualified 코퍼스가 아니라 예약 자리표시자다. 따라서 실제 봉인 실행은 독립 curator/custodian이 새 코퍼스와 역할 배정을 완성할 때까지 fail-closed가 정상 동작이다.
