# Chesstory Judgment Evaluation Harness

이 디렉터리는 Chesstory의 판단 품질을 배포 코드 밖에서 식별하기 위한 외부 평가 하네스다. 실제 산출물과 같은 스키마의 전문가 oracle과 불변 artifact를 결속해 외부 진단을 수행한다.

이 하네스가 존재하거나 `doctor`가 통과한다는 사실만으로 생산 판단의 정확성, 설명 품질, 병목 식별이 증명되지는 않는다. 현재 생산 경계와 예약 코퍼스로 얻을 수 있는 결론은 **판정 불가(indeterminate)** 다.

기계가 소비하는 판단 계약·코퍼스 라벨·provider 입출력은 구조화된 JSON으로만 교환한다. 이 README와 사용자 보고서는 실행 payload에 들어가지 않는다.

## 목적과 비목표

목적은 다음과 같다.

- 각 단계의 입력·출력을 동일한 JSON Schema로 고정한다.
- 원시 입력·출력, 검증본, 정규본, provider I/O, 버전·seed·해시를 재현 가능한 artifact로 보존한다.
- actual, format-control, oracle을 같은 adapter 인터페이스로 주입한다.
- all-actual, stable-Q, all-oracle, one-stage, leave-one-actual, 양방향 누적 대체와 인접 factorial arm을 동일 조건에서 실행한다.
- 원자 클러스터 단위 통계로 효과와 형식 효과를 분리하되, foundation control과 all-oracle ceiling을 먼저 통과시킨다.

명시적인 비목표는 다음과 같다.

- 운영 기본 응답과 판단 결과를 바꾸지 않는다.
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

`Jp`와 `Ja`를 분리해야 탐색 실패와 과잉 거부를 구분할 수 있으며, `P`는 `Q → F → C → Jp → Ja → R → P`에서 끝나는 구조화된 runtime projection 경계다.

## 구현 구조

| 위치 | 역할 |
|---|---|
| `schemas/v1/` | 보존된 stage 입력·출력 스키마와 공통 envelope 계약 |
| `src/chesstory_eval/schemas.py` | 스키마 참조 해석, registry 검증, stage 문서 검증 |
| `src/chesstory_eval/capture.py` | 원시 입력·출력, validated/canonical 출력, provider I/O와 metadata의 불변 저장, 해시 연결 ledger와 접근 로그 |
| `src/chesstory_eval/adapters.py` | actual/oracle/replay/외부 command 산출물을 같은 stage 호출 계약으로 변환하고 가용성 경계를 강제 |
| `src/chesstory_eval/stockfish.py` | 운영 런타임 밖에서 richer/stable Q 증거를 취득 |
| `src/chesstory_eval/design.py` | 두 독립 oracle chain을 사용하는 고정 intervention arm 계획 생성 |
| `src/chesstory_eval/runner.py` | schema 검증, adapter 호출, artifact capture, foundation gate, ceiling gate와 단계 개입 실행 |
| `src/chesstory_eval/canonicalize.py` | 참조를 보존한 ID 재할당과 스키마가 허용한 비의미 형식만 정규화 |
| `src/chesstory_eval/statistics.py` | 원자 클러스터 paired bootstrap, 사전 등록 대조량과 Holm 다중비교 보정 |
| `src/chesstory_eval/evaluation.py` | source/arm-blind 최종 endpoint `E`의 diagnostic replay와 release live 평가 계약 |
| `corpus/v1/` | manifest, 사전 등록, 역할·접근 정책, explore와 봉인 예약 split |
| `references/` | 로컬 참고 PDF의 불투명 ID·해시·페이지 메타데이터만 보존하는 색인 |
| `runtime-adapter/` | development-only current public-v4 schema JSONL adapter. CauseAudit는 untrusted raw transport/probe capture만 하며 player authority나 quality proof를 주장하지 않는다. |

### Schema와 artifact capture

모든 stage 문서는 공통 envelope 안에 stage, sample/cluster/split/run ID, seed, 입력 해시, upstream artifact 해시, producer 버전과 payload를 가진다. Runner는 다음 단계로 넘기기 전에 입력과 출력을 해당 버전의 스키마로 검증한다.

Artifact store는 stage별 raw input, raw output, validated output, canonical output, provider I/O와 metadata를 각각 내용 해시로 저장한다. 기존 내용과 다른 bytes로 같은 artifact를 덮어쓰지 않으며, ledger와 봉인 접근 로그는 이전 event hash를 잇는 append-only chain으로 검증한다.

### Adapter와 runner

Stage adapter는 실제 산출물과 oracle 산출물을 같은 입력 payload와 같은 출력 스키마 아래에서 호출한다. Replay는 sample, stage, source, oracle chain, 실험 문맥과 입력 해시가 일치하는 artifact만 사용할 수 있다. 열린 진단 경로는 코퍼스의 외부 취득 Q, `--stage-replay`, `--evaluator-replay`를 계속 사용할 수 있다. Live 경로는 `--live-command-config`의 shell-free 명령만 호출하며, 명시된 route가 provider I/O 뒤 실패하면 replay로 조용히 대체하지 않는다.

Live config의 최상위 계약은 `chesstory.eval.live-command-config.v1`이며 `stage_routes`와 단일 `evaluator_route`를 가진다. 각 route는 `argv` 문자열 배열, 1–3600초의 `timeout_seconds`, 명시적 `env` 객체와 `bound_files` 배열을 반드시 포함한다. 각 bound-file 항목은 정확히 `{argv_index,path,sha256,size}`이고, `path`는 해당 `argv` 원소와 문자 단위로 같은 canonical absolute regular non-link file이어야 한다. `argv[0]`은 반드시 결속되므로 PATH 탐색 실행은 허용되지 않는다. 실행 파일뿐 아니라 argv로 전달하는 script, JAR, model, engine 파일도 각각 결속해야 한다. Oracle route는 `oracle_chain`도 포함한다. 알 수 없는 필드, 중복 JSON key/route, 비유한 수, shell 문자열, NUL, secret/token/password/key 계열 환경변수와 알려진 secret 형태의 값은 거부된다. 봉인 실행의 route 구성은 보존된 schema 계약으로 검증된다.

Candidate freeze는 component root 안의 config와 bound file을 `live-command-config` component의 exact file manifest에 넣고, root 밖 파일은 별도 signed `external_executable_binding`에 canonical absolute-path fingerprint, byte SHA-256, size와 route/argv 사용처로 결속한다. 이 결속은 split open 전과 각 호출 직전·직후 다시 검증된다. Release 실행은 Windows에서 각 파일을 read-only, share-read-only handle로 열어 호출이 끝날 때까지 write/delete/rename sharing을 금지한다. 이 보장이 없는 플랫폼의 release 실행은 명시적으로 fail-closed한다. 이는 argv 밖에서 프로세스가 암묵적으로 여는 미신고 파일까지 자동 발견한다는 뜻은 아니므로 route 소유자는 모든 실행 의존 파일을 `bound_files`에 열거해야 한다.

개발 evaluator `E`의 stdin은 `chesstory.eval.source-blind-p-view.v1` 객체 그 자체뿐이며 canonical P의 `payload`를 정확히 얕게 복사한 것이다. `E`는 Cause, verdict, importance, legality를 재판정하지 않는다. Sample ID와 canonical-output hash는 arm/source/oracle/provider 정보가 없는 전용 control-plane 환경변수로 전달하며, 응답이 두 값을 정확히 echo해야 한다. Release 응답은 endpoint policy의 정확한 rater roster, rater별 `{evaluator_id, score}` vote와 그 산술평균인 `usefulness_consensus`를 포함해야 한다. 평가 artifact, stdout/stderr/base64를 포함한 raw provider I/O와 각 rater의 접근 event는 immutable run tree와 append-only 접근 로그에 남는다.

고정 계획은 두 독립 oracle chain에 대해 actual/format-control/oracle 조건, stable-Q, all-oracle, one-stage, leave-one-actual, forward/backward substitution, 인접 stage factorial을 포함한다. schema 수와 arm 수는 현행 `STAGES`에서 `doctor`와 `plan`이 도출한다. Runner는 하네스 identity/null control과 두 chain의 all-oracle ceiling이 통과한 뒤에만 개입 arm과 통계 대조를 확장한다.

### Canonicalizer와 statistics

Canonicalizer는 스키마가 명시적으로 허용한 세 변환만 한다.

- identity/reference 관계를 함께 보존하는 opaque ID 재할당
- `unordered-set`으로 선언된 배열의 표현 순서 정규화
- `null-equivalent-omission`으로 선언된 `null`의 생략

개발 하네스의 각 arm은 P까지 하나의 canonicalization/transition session을 사용하지만 player/runtime 권위를 대체하거나 확장하지 않는다. 앞 단계에서 정의된 ID mapping을 다음 단계가 이어받고, duplicate definition·dangling reference, Ja 밖 R claim, packet 밖 P 의미를 fail-closed한다. Confidence, ranked list 순서, 실제 missingness, 수치, 문구, 집합 cardinality는 바꾸지 않는다. 따라서 format-control은 작동 의미를 바꾸는 정책 개입이 아니다.

통계 계층은 같은 원자 클러스터의 treatment/control을 짝지은 bootstrap을 사용하고, 사전 등록된 단측 신뢰구간과 Holm 보정을 적용한다. 유의하지 않음은 동등성의 증거가 아니다. 잔여 이득이 없다는 결론에는 별도 fresh-confirm에서 `epsilon_gain`에 대한 단측 상한 기준을 통과해야 한다.

병목 귀속은 foundation control과 두 all-oracle ceiling이 먼저 통과한 완전 실행에서만 결정론적으로 계산한다. `diagnostic-explore`는 Φ를 제외한 Δ/Λ/Γ 중 두 fixed oracle chain이 동일한 `(contrast, context, focus_stage, held_stage, held_source)` 좌표에서 별도로 사전 등록한 `attribution_minimum_delta`, 양의 단측 CI 하한, Holm-adjusted `p`, 최소 원자 cluster를 모두 만족한 항목을 후보 목록으로만 남기며 `production_bottleneck`은 항상 `null`이다. 잔여 이득 동등성 경계인 `epsilon_gain`이나 release 비열등성 delta를 양성 귀속의 최소 효과량으로 재사용하지 않으며, 별도 귀속 threshold가 없으면 fail-closed한다. `diagnostic-confirm`은 정확히 사전 등록된 positive hypothesis와 chain별 contrast ID가 같은 좌표로 재현될 때만 확정한다. 비-factorial 좌표는 `confirmed-stage-bottleneck`과 stage를 반환하지만, factorial Γ는 `confirmed-interface-conditioned-bottleneck`과 exact held interface를 반환하고 단일 stage `production_bottleneck`은 `null`로 유지한다. `fresh-confirm`과 `blind`는 새 병목 귀속을 만들지 않는다.

## CauseAudit의 현재 관측 경계

CauseAudit는 development-only untrusted raw public-response/probe capture 경로다. `RuntimePublicResponseCli`에서 받은 current public-v4 schema JSONL의 raw bytes, SHA-256, length와 런타임이 발행한 probe의 direct acquisition, cap, binding만 기록한다. Cause, verdict, importance, idea status는 읽거나 재판정하지 않는다.

player/runtime authority는 `Q → F → C → Jp → Ja → R → P`에만 있으며, current public-v4 schema는 development-only untrusted raw transport일 뿐 authority가 아니다. `run`은 player authority나 quality proof가 아니다.

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

`doctor`는 현행 `STAGES`에서 도출한 stage schema 수와 고정 arm 계획, open split과 참고 locator, sealed split의 개발자 접근 거부와 파일 해시, canonicalizer 불변식, artifact ledger control을 점검한다. 현재 통과 결과는 참고 문서와 explore 표본·클러스터 및 도출된 arm 계획을 보고한다. 이는 하네스 건강 점검이지 생산 품질 합격이 아니다.

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

Q diagnostic은 단일 실행 명령이다. 정규화된 run ID의 불변 디렉터리가 이미 존재하면 provider를 시작하기 전에 거부하며, `--output`도 같은 정규화 규칙으로 그 디렉터리 내부를 가리킬 수 없게 막는다.

### CauseAudit raw capture

`cause-audit`는 explore-only development untrusted raw public-response/probe capture 경로다. 현재 CLI action은 `freeze`, `acquire`, `run`뿐이며 candidate, sealed, player, quality proof를 만들지 않는다.

```text
python -m chesstory_eval cause-audit --root . --action freeze --run-id CAUSE_FREEZE_RUN --cases CASES.jsonl --oracle-label oracle-a=ORACLE_A.jsonl --oracle-label oracle-b=ORACLE_B.jsonl --adjudicated-label ADJUDICATED.jsonl --contamination-exclusion "CASE_ID=PRE_FREEZE_REASON" --output CAUSE_FREEZE.json

python -m chesstory_eval cause-audit --root . --action acquire --run-id CAUSE_Q_RUN --cases CASES.jsonl --manifest CAUSE_FREEZE.json --reference-labels ADJUDICATED.jsonl --stockfish STOCKFISH_EXECUTABLE --output CAUSE_Q.json

python -m chesstory_eval cause-audit --root . --action run --run-id CAUSE_RUNTIME_RUN --cases CASES.jsonl --manifest CAUSE_FREEZE.json --acquisition CAUSE_Q.json --stockfish STOCKFISH_EXECUTABLE --sbt SBT_EXECUTABLE --adapter-root runtime-adapter --output CAUSE_RUNTIME.json
```

`freeze`는 case와 manifest-bound label을 결속한다. `acquire`와 `run`은 freeze manifest의 순서 있는 explore case set 전체만 사용한다. acquisition v2에는 runtime request를 저장하지 않는다. CauseAudit에는 cache path가 없으며, 각 Q acquisition과 runtime-issued probe를 직접 취득해 raw provider I/O와 함께 capture한다. `run`은 외부 acquisition JSON을 source artifact run의 captured acquisition document와 exact value로 대조하고, 그 store와 각 Q stage-capture ledger binding을 검증한 뒤에만 captured engine pack에서 request를 다시 만든다.

`RuntimePublicResponseCli`의 current public-v4 schema body는 operational probe extraction에만 일시 사용한다. runtime report는 byte-exact raw request/response JSONL의 base64, SHA-256, length와 validated probe records를 유지하지만 `rounds[].body`는 저장하지 않는다. 이는 parsed response body persistence를 제거하는 breaking boundary다. 이 raw capture는 runtime producer attribution이 아니다. report는 adapter main class나 sbt hash를 bind하지 않으며 runtime-adapter/chesstory-runtime source, build, JDK, dependency closure를 attest하지 않는다.

### Gated experiment runner

현재 가용성 경계를 확인하는 smoke run은 다음과 같다.

```text
python -m chesstory_eval run --root . --run-id explore-smoke --split diagnostic-explore --output artifacts/explore-smoke-report.json
```

Embedded Q만 관측되고 provider/replay가 없는 F 이상 단계는 unavailable이므로, 이 실행은 완전한 병목 실험이 아니며 foundation-not-passed와 indeterminate가 정상이다.

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
