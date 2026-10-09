"""Build a local evidence report; it neither queries an engine nor edits product data."""
from __future__ import annotations
from pathlib import Path
from datetime import datetime, timezone
import argparse
import hashlib
import html
import json
import re
import chess
import chess.svg

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent

FINAL_INPUT_SHA256 = {
    'source_human': '644c6b5f54bd1ae34e329dfdd0bf9c407b9b02623a276cfa76a9fc9da1135664',
    'completion_audit': 'c45d8b57cc824235ced017939e7f05ee60bb115161134013f28bc4fe7a149269',
    'final_review': 'd18c1d86dcf4b3c8f008046ecd6b04d5a0c8408d8a8b243904cede8af0a7a056',
}

def local_path(value):
    path = Path(value)
    path = (path if path.is_absolute() else ROOT / path).resolve(strict=True)
    path.relative_to(ROOT)
    return path

def verify_hashes(values):
    for value, expected in values.items():
        assert digest(local_path(value)) == expected, f'Changed evidence: {value}'

def verify_final_inputs(args, source_human, completion, review):
    for name, expected in FINAL_INPUT_SHA256.items():
        assert digest(getattr(args, name)) == expected, f'Not the sealed final10s {name}'
    assert source_human['epoch'] == '10s' and source_human['criteria_unchanged'] is True
    verify_hashes({source_human['criteria_path']: source_human['criteria_sha256'],
                   source_human['basic_path']: source_human['basic_sha256'],
                   source_human['original_basic_path']: source_human['original_basic_sha256']})
    source_basic = load(local_path(source_human['basic_path']))
    assert source_basic['run_status'] == 'finished' and source_basic['expected_case_count'] == 8
    assert source_basic['fixed_criteria_unchanged'] is True
    assert len(source_basic['cases']) == len(source_human['cases']) == 8
    assert len({case['id'] for case in source_basic['cases']}) == 8
    for basic, reading in zip(source_basic['cases'], source_human['cases']):
        assert basic['id'] == reading['id'] and basic['pending'] is False
        assert basic['adapter_error'] is None and basic['query_count_is_lower_bound'] is False
        assert hashlib.sha256(basic['text'].encode('utf-8')).hexdigest() == reading['basic_text_sha256']
        assert basic['default_question'] == reading['default_question']
    source_full = sum(case['minimum_answer_complete'] is True for case in source_human['cases'])
    assert source_human['counts'] == {'cases': 8, 'full_minimum_basic_answers_complete': source_full, 'partial': 8-source_full}
    independent_path = HERE / 'source-scene-current10s-normal21-human-text-reading.json'
    index_path = HERE / 'source-scene-current10s-normal21-concise-execution-index.json'
    verify_hashes({independent_path: '5661bb9b430e7dbf1d6b7dcb4c51da064cea4b57468f4891117e6d0a8c9942de',
                   index_path: 'd003e3b4342d53819b90819ee7d7d058a21a587618788c6020f49e09188669d9'})
    independent = load(independent_path)
    index = load(index_path)
    assert independent['criteria']['sha256'] == source_human['criteria_sha256']
    assert independent['criteria']['modified'] is False
    assert independent['input']['sha256'] == source_human['basic_sha256']
    assert independent['current_result']['fixed_full_minimum_complete'] == source_full
    assert len(independent['cases']) == 8
    for reading, other in zip(source_human['cases'], independent['cases']):
        assert reading['id'] == other['id'] and reading['basic_text_sha256'] == other['text_sha256']
    verify_hashes({index['full_audit_path']: index['full_audit_sha256']})
    assert len(index['cases']) == 8
    overall = index['overall']
    assert overall['actual_go_count'] == overall['dispatches'] == overall['native_execution_rows'] == 49
    assert overall['outcomes'] == {'completed': 47, 'executor_failed': 2}
    assert overall['all_issued_depths_at_most18'] is True
    assert overall['all_native_report_owners_exact'] is True
    assert overall['all_returned_native_paths_legally_replayed'] is True
    assert overall['shipped_schema_error_count'] == 0 and overall['post_run_asset_identity'] == 'unchanged'
    assert all(c['schema_errors'] == c['owner_errors'] == c['legal_errors'] == 0 for c in index['cases'])
    assert completion['study_id'] == 'H05-fresh10s-failed-comparisons-nominal16-20261009'
    assert completion['nominal_depth'] == 16 and completion['profile'] == 'sf18-smallnet-t2-h16-v1'
    assert completion['actual_Threads'] == 2 and completion['actual_Hash_MB'] == 16
    assert completion['new_actual_research_queries'] == 3 and completion['retries'] == 0
    assert completion['all_completed16'] is True and completion['product_reports_submitted'] == 0
    assert completion['original_failed_reports_preserved'] is True
    assert completion['original_product_primary_unchanged'] is True
    queries = completion['queries']
    assert len(queries) == 3 and {case['original_failed_work_id'] for case in queries} == {'work:2', 'work:3', 'work:4'}
    for query in queries:
        assert query['same_full_history_position_restriction_MPV_and_h16'] is True
        assert query['not_submitted_as_product_report'] is True
        assert query['scope']['search_limits']['depth'] == 16
        assert query['actual_go'].startswith('go depth 16 nodes 200000000 movetime 60000')
        assert query['actual_completed_outcome']['kind'] == 'completed'
        assert query['actual_completed_outcome']['completedDepth'] == 16
        checks = query['final_batch_checks']
        assert checks['nominal_depth'] == 16
        for key in ('exact_fen', 'all_required_pvs_present', 'exact_pv_depths', 'unbounded',
                    'distinct_roots', 'restricted_roots_respected', 'all_pvs_legally_replayed', 'bestmove_matches_leader'):
            assert checks[key] is True
    verify_hashes(completion['source_sha256'])
    study_root = ROOT / 'target/functional-judgment-20261009/h05-fresh10s-failed-comparisons-nominal16-completion'
    verify_hashes({study_root/'study-plan.json': completion['study_plan_sha256'],
                   study_root/'stage1.json': completion['stage1_sha256']})
    assert completion['session_manifest']['shutdown']['exit_code'] == 0
    assert completion['session_manifest']['shutdown']['adapter_forced_kill'] is False
    assert completion['costs']['raw_engine_time_sum_ms'] == sum(q['actual_completed_outcome']['engineTimeMs'] for q in queries)
    assert completion['costs']['session_wall_ms'] == completion['session_manifest']['session_wall_ms']
    verify_hashes(review['source_sha256'])
    verify_hashes(review['linked_records_sha256'])
    verify_hashes({HERE / 'current10s-rpc-certificate-cost-readonly-audit.json':
                   'be75dc66269e5f2a39a8804f6160eba144e65067137d953f887e8cddf2e4faaf'})
    assert review['compiled_main_manifest_sha256'] == '0ad4a34061285eac22c9df15a0d8a4d14ba0ee46e6fa0826214d7b3181773690'
    assert review['final_targeted_runtime_checks']['tests'] == 210
    assert review['final_targeted_runtime_checks']['all_passed'] is True
    assert review['human_utility_scope']['criteria_not_changed'] is True
    return source_full, completion['costs']

def load(path):
    return json.loads(path.read_text(encoding='utf-8'))

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def esc(value):
    return html.escape(str(value))

def link(path, label):
    path = local_path(path)
    href = '/' + path.relative_to(ROOT).as_posix()
    return f'<a href="{esc(href)}">{esc(label)}</a>'

def basic_card(basic, reading):
    missing = ''.join(f'<li>{esc(value)}</li>' for value in reading['missing'])
    verdict = basic['verdict']
    assert verdict == reading['fresh_verdict']
    delta = verdict['delta']['candidate_win_percent_delta_for_mover']
    grade = verdict.get('verdict_code', verdict['kind'])
    return f'''<article class="card"><div class="eyebrow">실제 기본 해설 · {esc(basic['game_id'])}</div>
    <h3>{'Nb1: 조건부 지원 설명' if basic['game_id'] != 'H05' else 'g4: 판단 이유가 빠진 출력'}</h3>
    <p>{'조건부 되잡기와 현재 지원은 읽을 수 있지만, 실제로 유용한 선택의 비교는 기본 해설에서 회수할 수 없습니다.' if basic['game_id'] != 'H05' else '물질 손실이 없는 수순이라는 설명만으로는 g4의 판단과 준비의 목적을 이해할 수 없습니다.'}</p>
    <div class="stamp partial">최소 판단 설명 미완료</div>
    <p class="meta">이 새 실행의 판정: {esc(grade)} · 엔진 승률 환산 차이 {delta:.6f} 퍼센트포인트</p>
    <p class="meta">실제 엔진 {basic['new_engine_queries']}회 · 작업 {basic['job_elapsed_ms']/1000:.3f}초 · {esc(basic['default_question'])}</p>
    <details><summary>실제 영어 본문 전체</summary><p class="actual">{esc(basic['body'])}</p></details>
    <details><summary>기본 출력에서 빠진 내용</summary><ul>{missing}</ul></details></article>'''

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--source-human', type=Path, required=True)
    parser.add_argument('--completion-audit', type=Path, required=True)
    parser.add_argument('--final-review', type=Path, required=True)
    args = parser.parse_args()
    args.source_human = local_path(args.source_human)
    args.completion_audit = local_path(args.completion_audit)
    args.final_review = local_path(args.final_review)
    runtime = ROOT / 'target/functional-judgment-20261009/final-choice-price-scope-depth18-withdeps10s'
    result = load(runtime / 'result.json')
    runlog = (runtime / 'run.log').read_text(encoding='utf-8')
    match = re.search(r'OK \((\d+) tests\)', runlog)
    assert result['exit_code'] == 0 and match, 'Do not publish an invented test success'
    tests = int(match[1])
    assert tests == 210
    source_human = load(args.source_human)
    completion = load(args.completion_audit)
    review = load(args.final_review)
    source_full, completion_costs = verify_final_inputs(args, source_human, completion, review)
    firstpass = load(HERE / 'fresh-native-two-case10s-basic-human-firstpass.json')
    fresh = ROOT / 'target/functional-judgment-20261009/fresh-native-two-case-10s-normal21-depth18/actual-fresh-run'
    manifest = load(fresh / 'manifest.json')
    assert manifest['post_identity_check'] == 'unchanged' and manifest['new_queries'] == 15
    assert manifest['query_count_is_lower_bound'] is False
    assert firstpass['criteria_unchanged'] is True and firstpass['new_two_case_full_minimum_answers'] == 0
    assert len(firstpass['cases']) == 2 and all(c['minimum_answer_complete'] is False for c in firstpass['cases'])
    basics = [load(fresh / (value['game_id'] + '-basic-only.json')) for value in firstpass['cases']]
    for basic, reading in zip(basics, firstpass['cases']):
        assert digest(fresh / (basic['game_id'] + '-basic-only.json')) == reading['basic_sha256']
    temporal = load(HERE / 'h05-same-placement-different-turn-function-audit.json')
    white = chess.Board(temporal['earlier_g4_after_Kf6']['fen'])
    black = chess.Board(temporal['later_g4_after_preparation']['fen'])
    assert white.board_fen() == black.board_fen() and white.turn != black.turn
    boards = [chess.svg.board(board=white, arrows=[chess.svg.Arrow(chess.C3, chess.B4, color='#267c5bee')], size=420),
              chess.svg.board(board=black, arrows=[chess.svg.Arrow(chess.B4, chess.A3, color='#ac523cee')], size=420)]
    cards = ''.join(basic_card(basic, reading) for basic, reading in zip(basics, firstpass['cases']))
    evidences = [
        (HERE / 'fresh-native-two-case10s-basic-human-firstpass.json', '실제 재실행 BASIC 선판독'),
        (HERE / 'nb1-produced10s-basic-human-firstpass.json', 'NB1 실제 26개 보고서 재생 선판독'),
        (HERE / 'nb1-produced10s-delivery-and-price-audit.json', 'NB1 가격·소유권·정상 전달 감사'),
        (HERE / 'h05-bounded18-response-function-and-price-audit-v2.json', 'H05 깊이18 실제 13개 비교·원래 기물 가격'),
        (HERE / 'h05-bounded18-response-function-independent-review.json', 'H05 비교 독립 검토'),
        (HERE / 'h05-bounded18-time-scope-correction.json', '엔진 시간·벽시계 시간의 구분'),
        (HERE / 'h05-same-placement-different-turn-function-audit.json', '같은 배치·다른 차례의 범위'),
        (HERE / 'coordinated-cap-and-mixed-anchor-repair10s-final-seal.json', '깊이·동일 비교 범위·정확한 정지 이유의 수정'),
        (HERE / 'depth18-runtime-failure-attribution-head40a-to10s.json', '기존 실패의 실제 재현과 한계'),
        (HERE / 'ui-depth18-execution-boundary-final-seal.json', 'UI 깊이 상한·72개 검사'),
        (args.source_human, '동일 이론서 8장면의 최종 기본 해설 판독'),
        (HERE / 'source-scene-current10s-normal21-human-text-reading.json', '8장면의 독립 본문 판독과 목적 설명의 기준'),
        (HERE / 'source-scene-current10s-normal21-concise-execution-index.json', '8장면 실제 49개 검색·전달·소유권·실패 색인'),
        (args.completion_audit, '실패한 동일 깊이16 비교를 충분히 완료한 조사'),
        (HERE / 'h05-fresh10s-nominal16-plan-metadata-correction.json', '별도 조사 계획의 표시 오류와 실제 실행 범위 정정'),
        (args.final_review, '최종 연결 코드·범위 독립 검토'),
        (HERE / 'current10s-rpc-certificate-cost-readonly-audit.json', '현재 공개 근거의 크기·처리 비용·줄일 때 지켜야 할 의존 관계'),
        (HERE / 'bounded-medium-goal-partial-outcome-and-depth18-handoff.json', '중기 목표의 미달성 기준과 깊이18 안의 후속 과제')
    ]
    evidence_links = ''.join('<li>' + link(path, label) + '</li>' for path, label in evidences)
    built = datetime.now(timezone.utc).isoformat()
    body = f'''<!doctype html><html lang="ko"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
    <title>깊이18에서 선택 가치와 판단을 검증한 작업 기록</title>
    <style>
    :root{{color-scheme:light;--ink:#182b35;--muted:#586970;--green:#246b52;--line:#d6dfdf;--amber:#925f1a}}
    *{{box-sizing:border-box}} body{{margin:0;background:#f6f5ef;color:var(--ink);font:16px/1.7 'Malgun Gothic','Segoe UI',sans-serif}}
    main{{max-width:1150px;margin:auto;padding:42px 26px 75px}}h1{{font-size:38px;line-height:1.3;margin:10px 0 18px;letter-spacing:-1px}}h2{{font-size:25px;margin:0 0 16px}}h3{{font-size:20px;line-height:1.45}}
    p{{margin:10px 0 16px}}a{{color:#225e80;text-underline-offset:3px}}section{{margin-top:36px}}.eyebrow{{font-size:13px;color:var(--green);letter-spacing:.06em;font-weight:700}}.lead{{font-size:20px;max-width:850px}}.meta{{font-size:13px;color:var(--muted)}}
    .metrics{{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:25px}}.metric{{padding:18px;background:white;border:1px solid var(--line);border-radius:12px}}.metric strong{{font-size:30px;display:block}}.metric span{{font-size:13px;color:var(--muted)}}
    .grid{{display:grid;grid-template-columns:1fr 1fr;gap:20px}}.card{{background:white;border:1px solid var(--line);border-radius:14px;padding:25px;min-width:0}}.notice{{border-left:4px solid var(--amber);padding:14px 20px;background:#faf0dc;border-radius:0 10px 10px 0}}
    .stamp{{display:inline-block;font-size:12px;font-weight:700;padding:4px 10px;border-radius:30px}}.partial{{background:#f6e8c8;color:#775111}}.board svg{{width:100%;height:auto;max-width:420px;display:block;margin:0 auto}}.sequence{{font:14px/1.6 Consolas,monospace;color:#40555d;padding:10px 0}}
    table{{border-collapse:collapse;width:100%;background:white;border:1px solid var(--line);font-size:14px}}th,td{{text-align:left;vertical-align:top;padding:15px;border-bottom:1px solid var(--line)}}th{{background:#edf2f1;font-weight:700}}.table-wrap{{overflow:auto}}
    details{{border-top:1px solid var(--line);padding:13px 0;margin-top:12px}}summary{{cursor:pointer;font-size:14px;font-weight:700}}.actual{{white-space:pre-wrap;font-family:'Segoe UI',sans-serif;font-size:14px;line-height:1.85;color:#35484f}}li{{margin:7px 0}}footer{{margin-top:45px;padding-top:20px;border-top:1px solid var(--line);font-size:12px;color:var(--muted)}}
    @media(max-width:800px){{main{{padding:28px 17px 50px}}h1{{font-size:29px}}.metrics{{grid-template-columns:1fr 1fr}}.grid{{grid-template-columns:1fr}}.card{{padding:20px}}th,td{{padding:10px;min-width:130px}}}}
    </style><main><div class="eyebrow">선택 가치 · 상대의 방어 · 필요한 대가</div><h1>깊이18 안에서 무엇을 확인했는가</h1>
    <p class="lead">일반 분석의 깊이 상한을 고정하고, 같은 비교의 새 반론과 실제 대가를 해설에 연결하는 경로를 강화했습니다. 수의 원래 판단을 충분히 설명하는 능력은 여전히 미완성입니다.</p>
    <div class="metrics"><div class="metric"><strong>18</strong><span>새 작업의 최대 요청 깊이</span></div><div class="metric"><strong>{tests}/210</strong><span>선택·소유권·가격·정지 회귀 검사</span></div><div class="metric"><strong>13/13</strong><span>완료한 별도 깊이18 실제 비교</span></div><div class="metric"><strong>0/2</strong><span>최종 두 사례의 완전한 BASIC 판단 설명</span></div></div>
    <p class="meta">18은 엔진에 요청하는 분석 깊이입니다. 엔진 내부의 선택 확장과 표시 수순 길이는 더 길 수 있습니다. 과거 깊이20·24 자료는 새 깊이18 성공으로 계산하지 않았습니다.</p>
    <section><h2>실제로 자동 출력에 남은 것</h2><p>아래는 최종 코드와 정상 UI 경로로 새 엔진을 실행한 두 사례입니다. 먼저 기본 해설을 읽고 기록한 뒤 내부 증명과 실패 기록을 조사했습니다.</p><div class="grid">{cards}</div>
    <p class="notice">별도로 기존 실제 보고서 26개를 현재 코드로 정확히 재생하면, NB1의 Nef6 아래에서 Rd6의 유용성과 필요한 교환 대가를 설명합니다. 그 기존 분석의 판정은 mistake, 승률 환산 차이는 −5.071729 퍼센트포인트입니다. 위 새 분석의 inaccuracy 판정과 서로 대체하지 않았습니다. 이전 전체 기준 0/4도 개선 성공으로 다시 계산하지 않았습니다.</p></section>
    <section><h2>같은 배치라도 계획의 가치는 차례에 달린다</h2><p>H05의 확인한 Ke3–Ke6–Kd2–Ke5 분기에서는 폰을 계속 보존하는 것보다 필요한 교환을 제때 실행할 기회가 중요했습니다. 이 분기의 두 수순은 기물 배치가 같지만 차례와 전체 이력이 다릅니다. 원래 g4 판단의 모든 원인이 확인됐다는 뜻은 아닙니다.</p>
    <div class="grid"><article class="card board"><div class="eyebrow">백 차례 · 기록된 다음 수 cxb4</div><div class="sequence">Ke3 Kf6 g4 Ke6 Kd2 Ke5</div>{boards[0]}<p>앞선 g4 비교의 긍정적으로 평가된 기록 수순은 여기서 cxb4를 이어갑니다. 흑의 …bxa3보다 먼저 교환할 수 있는 차례가 남아 있습니다.</p></article>
    <article class="card board"><div class="eyebrow">흑 차례 · 직접 비교한 반론 …bxa3</div><div class="sequence">Ke3 Ke6 Kd2 Ke5 g4</div>{boards[1]}<p>이 위치에서는 흑이 먼저 …bxa3를 실행할 수 있습니다. 확인한 강한 반론은 e4 포획이나 f4 진입 차단보다 퀸사이드의 돌파였습니다.</p></article></div>
    <p class="meta">백 차례의 cxb4는 앞선 비교에 등장한 합법적 후속 수입니다. 이 나중 위치에서 새로 검증한 최선 선택의 값은 아닙니다. 서로 다른 위치·차례의 수치를 합쳐 원래 평가 차이의 원인으로 사용하지 않았습니다.</p></section>
    <section><h2>상대의 응수에 따라 필요한 일이 달라진다</h2><div class="table-wrap"><table><thead><tr><th>실제 비교 위치</th><th>같은 비교 안의 백 기준 값</th><th>확인한 의미</th></tr></thead><tbody>
    <tr><td>Ke3 f6 뒤</td><td>Kf3 +455 / g4 0</td><td>왕의 준비를 유지하고 당장의 폰 전진을 미루는 선택이 유리합니다. 해당 좋은 수순에서도 g4는 나중에 실행됩니다.</td></tr>
    <tr><td>Ke3 Kf6 뒤</td><td>Kf4 +558 / g4 +483</td><td>g4도 긍정적으로 평가된 대안입니다. 모든 응수에서 같은 대기 계획만 요구하면 잘못된 설명이 됩니다.</td></tr>
    <tr><td>Ke3 Ke6 Kd2 Ke5 뒤</td><td>cxb4 +463 / g4 −898</td><td>준비한 퀸사이드 교환을 실행하는 일이 중심입니다. e4 폰의 영구 보존은 필수 조건이 아닙니다.</td></tr></tbody></table></div>
    <p class="meta">세 행은 별도 조사에서 완료한 동일 깊이18의 직접 비교입니다. 각 행 안에서만 비교합니다. 자동 제품의 수치 판정이나 전체 방어에 대한 승리 증명으로 사용하지 않았습니다. 원래 기물의 손실·되잡기·체크·승격은 각 정확한 수순에 연결해 기록했습니다.</p></section>
    <section><h2>코드에서 강화한 부분과 남은 결함</h2><div class="table-wrap"><table><thead><tr><th>문제</th><th>이번에 강화한 부분</th><th>남은 과제</th></tr></thead><tbody>
    <tr><td>합법인 수가 유용한 이유로 커짐</td><td>현재 전체 후보 조사와 같은 위치·깊이의 직접 비교를 구분하고, 둘 다 소유한 경우에만 선택 가치를 인정합니다.</td><td>조건부 선택 가치가 원래 수의 판단을 어떻게 바꾸는지 충분히 조직해야 합니다.</td></tr>
    <tr><td>같은 비교의 다른 표기가 새 반론을 차단함</td><td>생략된 기본 접두 수순과 명시된 동일 접두 수순을 하나의 논리적 범위로 읽습니다. 기존 ‘유용함’을 반박하는 실제 새 비교도 반영합니다.</td><td>차례·위치·원래 기물·전체 이력이 다른 값은 계속 구분해야 합니다.</td></tr>
    <tr><td>응수가 바뀌면 옛 지원 역할을 유지함</td><td>현재의 실제 기물과 대체 지원을 다시 읽고, 사용한 기물의 첫 후속 손실과 연결된 교환·체크·승격 대가를 보존합니다.</td><td>현재 지원 사실만으로 새 계획의 유용성을 상속하지 않습니다. 바뀐 계획은 새 판단이 필요합니다.</td></tr>
    <tr><td>재검증마다 깊이가 증가함</td><td>서버와 UI 모두 새 작업의 요청 깊이18 상한을 적용합니다. 한도를 넘는 필요는 미완료로 남깁니다.</td><td>부모·후속 비교의 조사 범위와 갱신 기준을 유한하게 정의해야 합니다. 상한만으로 조사 배분 문제가 해결되지는 않습니다.</td></tr>
    <tr><td>무엇 때문에 사실 확인이 중단됐는지 사라짐</td><td>실제로 시도하다 지역 확장 한도에 막힌 정확한 의무에만 정지 이유를 남깁니다. 미확인 사실과 필요한 확인 범위를 보존합니다.</td><td>개별 검색 한도와 후보 조사 경쟁 때문에 필요한 질문이 일찍 종료되는 사례가 남습니다.</td></tr></tbody></table></div></section>
    <section><h2>시간이 부족한 비교는 같은 깊이에서 한 번 끝내 보았다</h2><p>최종 H05 자동 실행의 세 후속 비교는 깊이18 상한에 막힌 것이 아닙니다. 한 비교는 깊이16에 도달했으나 8초 한도를 1밀리초 넘었고, 두 비교는 5초 한도에서 깊이13·12로 끝났습니다.</p>
    <p>같은 초기 위치·전체 수순·후보 제한·분석 깊이16과 엔진 설정을 유지하고, 별도 조사에서 각 비교의 시간과 노드 한도를 늘려 한 번 확인했습니다. 세 비교 모두 완료됐습니다. 실제 엔진 시간 합계 {completion_costs['raw_engine_time_sum_ms']/1000:.3f}초, 세션 전체 {completion_costs['session_wall_ms']/1000:.3f}초가 들었습니다. 원래 실패는 보존했고, 새 조사 값은 제품 보고서나 원래 수의 판정에 넣지 않았습니다. 멀티스레드 분석의 수치 변화를 한도 변경만의 효과로 단정하지 않습니다.</p>{link(args.completion_audit, '세 비교의 실제 결과와 비용 보기')}</section>
    <section><h2>검증 범위</h2><p>최종 컴파일은 main79개와 test123개에 대해 통과했습니다. 현재 소유권·가격·선택·정지 관련 {tests}개 검사는 모두 통과했고, UI 깊이 경계와 기존 통제72개도 통과했습니다. NB1의 현재 실제 보고서 재생은 원래 수치 판정과 필요한 고유 포획7건을 보존했고, 전달 스키마 오류는 없었습니다.</p>
    <p>전체 테스트가 모두 통과했다고 보고하지 않았습니다. 이전 코드에서도 재현되는 실패11건과 일반30초에서 완료하지 못하는 경로, 공동 예산에서 상대 질문이 밀리는 위험은 별도로 남겼습니다. 더 긴 조사에서 통과한 것을 일반 정책의 통과로 계산하지 않았습니다.</p>
    <p>추가 이론서의 동일8장면도 최종 코드·깊이18 제한·정상 실행 경로에서 다시 판독했습니다. 부모와 독립 판독 모두 완전한 답 {source_full}/8입니다. 실제 새 검색49회 중47회가 완료됐고, 두 검색의 시간·노드 한도 실패는 그대로 남겼습니다. 모든 검색이 완료된 나머지 여섯 장면도 필요한 목적·판단 연결을 충분히 설명하지 못했습니다. 이 결함을 검색 한도만의 문제로 볼 수 없습니다.</p>
    <p>평범한 Rd3에서는 c3 압박과 대체 룩의 e2 보호를 읽을 수 있지만, Rd2를 이어가는 압박의 목적이 흩어져 있습니다. 이 대체 보호 사실은 이전8b 출력에도 있었으므로 새10s 의미 개선으로 세지 않았습니다. 일반 목적 설명에 유일한 최선 수나 평가 차이의 완전한 원인을 요구하지 않았습니다.</p>
    <p>과거0/8과 최종 결과는 기록에서 구분하고, 이미 본 사례를 새 검증 자료로 부르지 않습니다. 공개 판정의 수순 배열에 노출된 사실을 밝히고, 본문에 없는 내용은 설명 성공으로 계산하지 않았습니다.</p>{link(args.source_human, '8장면의 실제 기본 해설 판독 보기')}</section>
    <section><h2>이후 작업의 기준</h2><p>새로운 깊이 증가보다 필요한 질문의 완료와 반박 조건이 우선입니다. 같은 깊이에서도 필요한 비교가 끝나지 못하는 이유를 먼저 없애고, 현재의 강한 방어 아래 어떤 기능이나 계획이 남는지를 작은 판단 답으로 연결해야 합니다.</p>
    <p>그 판단이 다른 주요 응수에서도 성립하는지, 필요한 대가가 같은 설명 안에 남는지, 평범한 수는 짧은 목적 설명으로 충분한지를 기본 출력만 읽고 평가해야 합니다. 더 많은 관계나 더 긴 수순을 출력하는 것으로 진척을 세지 않습니다.</p></section>
    <section><h2>깊이를 늘리기 전에 줄일 수 있는 처리 비용</h2><p>NB1의 새 실행은192.274초였고, 엔진이 기록한 시간은25.985초, Java 요청 처리 합계는104.480초였습니다. 전달하는 상태의 크기도 약92KB에서21.3MB로 늘었습니다. 여덟 이론서 장면은 전체399.620초 중 Java 요청 처리212.774초, 엔진 어댑터93.217초가 기록됐습니다.</p>
    <p>요청 처리는 판단 갱신·공개 근거 구성·JSON 직렬화·전달·읽기를 함께 포함합니다. 특정 함수의 비용으로 확정한 측정은 아닙니다. 이미 수순과 결과를 재사용하는 경로도 있습니다. 다음 개선은 이 단계들을 따로 측정하고, 전체 내부 기록을 보존하면서 현재 판단과 필요한 대가를 성립시키는 근거만 공개하는 범위를 정의하는 것입니다. 오래된 영수증도 현재 충돌 검증의 필수 부모일 수 있어, 단순히 오래된 행이나 개수 초과 행을 자르는 방식은 충분하지 않습니다.</p></section>
    <section><h2>검토 가능한 근거</h2><ul>{evidence_links}</ul><p class="meta">이론서 읽기 범위: Think Like a Super-GM 독서판의 등록된 장면22쪽, 100 Endgame Patterns의 해당 장면3쪽과 도표. 책 전체를 읽었다고 보고하지 않았습니다.</p></section>
    <footer>생성 UTC {esc(built)} · 로컬 검토 자료 · 자동 BASIC, 별도 조사, 보고서 재생, 회귀 검사를 구분함 · 원래 성공 기준 유지</footer></main></html>'''
    target = HERE / 'report.html'
    target.write_text(body, encoding='utf-8')
    inputs = [runtime/'result.json', runtime/'run.log', args.source_human, args.completion_audit,
              args.final_review, HERE/'fresh-native-two-case10s-basic-human-firstpass.json',
              fresh/'manifest.json', HERE/'h05-same-placement-different-turn-function-audit.json',
              *[path for path, label in evidences]]
    metadata = {'generated_utc': built, 'report_sha256': digest(target), 'test_count': tests,
                'source_sha256': {path.relative_to(ROOT).as_posix():digest(path) for path in inputs},
                'fixed_criteria_not_changed': True, 'new_engine_queries': 0,
                'automatic_full_success_not_inferred_from_test_pass': True}
    (HERE/'report-provenance.json').write_text(json.dumps(metadata, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
    print(json.dumps({'report': str(target), 'sha256': digest(target), 'engine_queries': 0}))

if __name__ == '__main__':
    main()
