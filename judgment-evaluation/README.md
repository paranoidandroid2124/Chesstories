# Chesstory Judgment Evaluation Harness

이 디렉터리는 런타임의 `Q → F → C → Jp → Ja → R → P` 파이프라인에서
병목을 찾는 development-only stage-intervention 하네스다. player-use
사실, claim 또는 L2 증명을 생산하지 않으며 평가 결과로 런타임 의미를
승격하지 않는다.

런타임 의미 경계는
[`../chesstory-runtime/docs/JudgmentBoundary.md`](../chesstory-runtime/docs/JudgmentBoundary.md)가
설명한다.

## 권위와 비목표

README는 진입점일 뿐 실행 계약을 복제하지 않는다.

| 내용 | 실행 권위 |
| --- | --- |
| 단계 순서와 arm model | `src/chesstory_eval/model.py`, `design.py` |
| 입력·출력 shape | `schemas/`와 `SchemaRegistry` |
| CLI와 옵션 | `python -m chesstory_eval COMMAND --help` |
| canonicalization | `canonicalize.py` |
| corpus 접근과 split 봉인 | `corpus.py`, 각 manifest와 access policy |
| freeze·attestation·qualification | `attestation.py` |
| 통계와 병목 귀속 | `statistics.py`, `attribution.py` |

하네스는 다음을 하지 않는다.

- 엔진 평가 하나에서 인간 아이디어나 원인을 역추론하지 않는다.
- oracle, 책, evaluator를 production truth source로 사용하지 않는다.
- unavailable stage를 replay나 fallback으로 조용히 채우지 않는다.
- 유의하지 않음을 동등성이나 release 통과로 해석하지 않는다.
- 열린 diagnostic 결과를 confirm/blind 결과로 재사용하지 않는다.

## 단계

| 단계 | 관측 대상 |
| --- | --- |
| Q | engine evidence input |
| F | normalized facts |
| C | candidate causes |
| Jp | proposed judgments |
| Ja | admitted judgments |
| R | selected and ordered explanations |
| P | public projection |

Actual과 oracle은 같은 stage schema를 통과한다. 각 arm은 실제 source를
oracle로 바꾸는 위치만 달라야 하며, runner는 앞 단계의 validated output만
다음 단계에 전달한다.

## Artifact와 봉인 경계

Artifact store는 raw input/output, validated/canonical output과 provider
I/O를 내용 해시로 보존한다. ledger와 접근 기록은 append-only chain이다.
같은 artifact identity를 다른 bytes로 덮어쓰지 않는다.

Live route는 shell-free argv와 모든 외부 실행 파일의 해시를 candidate
freeze 전에 결속한다. 실행 직전·직후에도 같은 bytes인지 확인하며,
release 모드에서 플랫폼이 write/delete 방지를 제공하지 못하면
fail-closed한다. 이것은 선언하지 않은 실행 의존성을 자동 발견하거나
hostile process를 원격 attestation한다는 뜻이 아니다.

봉인 실행은 통계 공개와 분리된다.

1. candidate와 live routes를 freeze한다.
2. 고정 계획을 실행해 pre-unblinding raw bundle만 만든다.
3. raw tree와 adjudication을 attestation한다.
4. attestation이 맞을 때만 `finalize-run`으로 통계를 계산한다.
5. fresh-confirm qualification이 통과한 동일 candidate만 blind를 열 수
   있다.

현재 자격과 표본 수는 이 문서가 아니라 manifest와 `doctor` 결과를
따른다. 저장소의 sealed split 자리표시자는 그 자체로 confirm/blind 또는
release-qualified corpus가 아니다.

## 실행

Python 3.11 이상이 필요하다. 이 디렉터리에서 실행한다.

```powershell
$env:PYTHONPATH = "src"
python -m chesstory_eval doctor --root .
python -m chesstory_eval plan --root . --output artifacts/arm-plan.json
```

또는 editable install 뒤 `judgment-eval` entrypoint를 사용할 수 있다.

```text
python -m pip install -e .
judgment-eval doctor --root .
```

`doctor`는 schema, arm plan, locator, split 접근, canonicalizer와 artifact
control의 현재 상태를 검사한다. 하네스 건강 점검이지 해설 품질이나
release 합격 증명은 아니다.

열린 진단 명령은 다음과 같다. 정확한 인수와 현행 기본값은 각 명령의
`--help`를 사용한다.

```text
python -m chesstory_eval q-diagnostic --help
python -m chesstory_eval cause-audit --help
python -m chesstory_eval run --root . --run-id explore-smoke --split diagnostic-explore --output artifacts/explore-smoke-report.json
```

`cause-audit`는 explore-only raw public-response/probe capture다. Cause,
verdict, importance 또는 idea status를 재판정하지 않으며 player authority나
quality proof가 아니다.

봉인 경로의 명령 순서는 다음과 같다. 각 단계의 필수 binding은 CLI와
schema가 소유한다.

```text
freeze-candidate
run
attest-run
finalize-run
qualify-fresh-confirm
```

## Runtime public-response adapter

`runtime-adapter`의 `RuntimePublicResponseCli`는 development-only JSONL
transport다. 각 UTF-8 request를 한 번 평가하고 `http_status`와 변경하지
않은 `body`를 쓴다. player HTTP API가 아니다.

`runtime-adapter` 디렉터리에서 실행한다.

```text
sbt -batch -error "runMain io.chesstory.evaluation.runtimeadapter.RuntimePublicResponseCli" < requests.jsonl > responses.jsonl
```

JDK 21과 sbt 1.11.7이 필요하다.

## 인간 이론서와 코퍼스

[`references/source-index.json`](references/source-index.json)이 등록된
reference source identity, checksum, page count와 contamination
표식의 단일 권위다. 허용 locator는 `{document_id, pdf_page}`뿐이며 local path, 원문,
스크린샷과 추출 본문은 artifact에 넣지 않는다.

책의 아이디어는 독립적인 포지션·수순·반사실을 구성하는 출발점일 뿐
정답 label이 아니다. 합법 수순과 보드 사실을 별도로 검증하고, 오염된
explore source를 confirm/blind 근거로 재사용하지 않는다.

레퍼런스 문장·후보 설명·복사 PGN을 저장소 안의 별도 의미 원장으로
복제하지 않는다. 현재 공개 L2 명제의 page anchor와 형식적 충분조건은
[`../chesstory-runtime/docs/JudgmentBoundary.md`](../chesstory-runtime/docs/JudgmentBoundary.md)에만
기록하며, 실제 체스 사실은 언제나 런타임의 인증된 L0/L0.5/L1 producer가
발급한다.
