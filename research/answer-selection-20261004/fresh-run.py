"""Prepare by default; run the frozen18 once only with --run and matching build manifests."""
import argparse
import gzip
import hashlib
import importlib.util
import json
import os
import queue
import subprocess
import threading
import time
from pathlib import Path

import chess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
ORIGINAL = ROOT / 'target/relationship-delivery-20261004/fresh-probe-v6'
DEPENDENCIES = ORIGINAL.parent / 'frontend-probe'
REQUESTS = HERE / 'balance-final18-requests.json'
INPUT_SHA = 'd2054a176b06ed598ef86a4c5a6907ac8b6eed3641a5e14070fe3be97624bafd'
JOBS_SHA = '5b3cee064925a8aa628a2de2800173469ff1d17522cd462c0e6ea19b66128d20'
PROFILE = 'sf18-smallnet-t2-h16-v1'
TRANSPORT = ROOT / 'target/annotated-games-20261003/FullGameProbe.scala'
ASSET = ROOT / 'target/annotated-games-20261003/engine/uci.mjs'
WORK_CAP = 40


def canonical(value):
    return json.dumps(value, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def save(path, value):
    Path(path).write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')


def load(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))


def prepared():
    frozen = load(REQUESTS)
    assert sha(REQUESTS) == INPUT_SHA, 'Frozen requests file changed'
    assert hashlib.sha256(canonical(frozen['jobs'])).hexdigest() == JOBS_SHA == frozen['jobs_sha256']
    assert frozen['status'] == 'final18_frozen_before_product' and len(frozen['jobs']) == 18
    assert len({job['id'] for job in frozen['jobs']}) == 18
    assert len({job['game_identity_key'] for job in frozen['jobs']}) == 18
    for job in frozen['jobs']:
        request = job['request']
        assert request['schema_version'] == 'chesstory.position-commentary.job-request.v6'
        assert request['engine_profile'] == PROFILE and request['remaining_budget_ms'] == 30000
        assert not job['automatic_threats'] and not job['probe_threats']
        board = chess.Board(request['initial_fen'])
        for uci in request['move_prefix_uci']:
            board.push_uci(uci)
        assert board.fen(en_passant='fen') == request['current_fen']
        board.push_uci(request['focus']['played_move_uci'])
        assert board.fen(en_passant='fen') == request['focus']['resulting_fen']
    manifest = {'status': 'prepared_not_executed', 'engine_executed': False,
        'product_job_executed': False, 'product_prose_read': False, 'game_count': 18,
        'request_file_sha256': INPUT_SHA, 'jobs_sha256': JOBS_SHA,
        'game_ids': [job['id'] for job in frozen['jobs']],
        'budget_ms_per_job': 30000, 'automatic_threats': False, 'root_depth': 16, 'preview_depth': 8,
        'engine_profile': PROFILE, 'threads': 2, 'hash_mb': 16,
        'selection': 'Exact frozen18 in stored order; no balance recomputation, alternate slot, exclusion by played-move error, retry, or product answer quality.',
        'source_hashes': {str(path.relative_to(ROOT)).replace('\\', '/'): sha(path) for path in
            [TRANSPORT, ORIGINAL / 'run.py', ORIGINAL / 'build.mjs', ORIGINAL / 'engine-bridge.ts',
             HERE / 'fresh-run.py', HERE / 'fresh-build.mjs', HERE / 'fresh-engine-bridge.ts']},
        'input_hashes': {name: sha(HERE / name) for name in
            ['balance-final18-requests.json', 'balance-policy.json', 'balance-audit.json']},
        'frontend_build_required': 'Run fresh-build.mjs after product source freeze; execution refuses an absent or stale fresh-bridge-manifest.json.',
        'transport_compile_required': 'Compile unchanged FullGameProbe.scala against the frozen main classes; classpath JSON must record transport_source_sha256.',
        'product_manifest_contract': 'Successful frozen compile manifest with exit_code, source_sha256 and class_sha256, paired with actual runtime_classpath. Caller supplies the approved freeze; identity checks do not independently establish chess-policy quality.',
        'fact_scope': 'Reuse previous facts(snapshot): every admitted path is replayed legally, original actors/roles/captures/terminal state exported, and all root-comparison candidate pair IDs retained. No basic text or prose is read or used for filtering.',
        'limits': {'job_deadline': 'Live public reducer deadline from remaining_budget_ms=30000; no virtual clock extension.',
            'engine_lease': 'Actual frontend min(maxSearchElapsedMs, deadline-now), with stop then lease+2000ms kill guard.',
            'engine_initialization_timeout_ms': 15000, 'node_ready_rpc_timeout_seconds': 20,
            'engine_work_rpc_timeout_seconds': 40, 'reducer_rpc_timeout_seconds': 30,
            'adapter_work_cap_per_job': WORK_CAP, 'process_shutdown_grace_seconds': 15,
            'failure_semantics': 'Original reports, decoder failures, reducer errors and stop conditions preserved. A missing bridge report gets the existing executor_failed engine_failure wrapper, explicitly labelled adapter-generated. RPC timeout/work cap leaves original pending work visible; no completed status is invented.',
        'timing_scope': 'Standalone warm engine jobs, first JVM begin distinguished; initialization separately measured. A startup failure includes the failed startup lifecycle in its job elapsed time. No browser/HTTP/Docker/mobile/whole-game UI deadline or deployment latency claim.'}}
    save(HERE / 'fresh-prepared-manifest.json', manifest)
    return frozen['jobs'], manifest


def check_hashes(base, values, label):
    changed = [path for path, digest in values.items() if not (base / path).is_file() or sha(base / path) != digest]
    assert not changed, f'{label} changed: {changed}'


def validate_runtime(args, manifest):
    cp, product = load(args.classpath), load(args.product_manifest)
    assert product['exit_code'] == 0
    check_hashes(ROOT, product['source_sha256'], 'Product sources')
    assert cp.get('transport_source_sha256') == sha(TRANSPORT), 'Transport source hash missing/mismatched'
    first_class = next(iter(product['class_sha256']))
    product_dir = next(Path(path) for path in cp['runtime_classpath']
                       if Path(path).is_dir() and (Path(path) / first_class).is_file())
    check_hashes(product_dir, product['class_sha256'], 'Product runtime classes')
    bridge_path = HERE / 'fresh-bridge-manifest.json'
    assert bridge_path.is_file(), 'Build fresh frontend contract after source freeze before --run'
    bridge = load(bridge_path)
    check_hashes(ROOT, bridge['source_hashes'], 'Frontend sources')
    check_hashes(HERE, bridge['harness_hashes'], 'Research bridge/harness')
    check_hashes(ROOT, bridge['reused_original_hashes'], 'Original reused research sources')
    check_hashes(DEPENDENCIES, bridge['dependency_hashes'], 'Declared frontend dependencies')
    balance = load(HERE / 'balance-policy.json')
    for asset in balance['assets']:
        assert sha(asset['path']) == asset['sha256'], f'Frozen engine/asset changed: {asset["path"]}'
    expected_node = next(asset for asset in balance['assets'] if asset['path'].lower().endswith('/node.exe'))
    assert sha(args.node) == expected_node['sha256'], 'Node binary differs from frozen balance engine'
    runtime_hashes = {}
    for entry in cp['runtime_classpath']:
        path = Path(entry)
        if path.is_dir():
            runtime_hashes[entry] = hashlib.sha256(''.join(
                f'{item.relative_to(path)}:{sha(item)}\n' for item in sorted(path.rglob('*.class'))).encode()).hexdigest()
        else:
            runtime_hashes[entry] = sha(path)
    manifest.update({'classpath_sha256': sha(args.classpath), 'product_manifest_sha256': sha(args.product_manifest),
        'product_manifest_path': str(args.product_manifest.resolve()), 'classpath_path': str(args.classpath.resolve()),
        'product_class_directory': str(product_dir), 'product_class_count': len(product['class_sha256']),
        'runtime_hashes': runtime_hashes, 'bridge_manifest_sha256': sha(bridge_path),
        'engine_assets': balance['assets'], 'source_and_class_and_bridge_identity_passed': True})
    return cp, product, bridge


class RPC:
    def __init__(self, command, stderr):
        self.process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=stderr, text=True, encoding='utf-8', cwd=ROOT)
        self.lines = queue.Queue()
        def read():
            for line in self.process.stdout:
                self.lines.put(line)
            self.lines.put(None)
        threading.Thread(target=read, daemon=True).start()

    def receive(self, timeout):
        try:
            line = self.lines.get(timeout=timeout)
        except queue.Empty:
            raise TimeoutError(f'Research RPC exceeded {timeout}s') from None
        if line is None:
            raise RuntimeError('Research process ended without response')
        return json.loads(line)

    def call(self, value, timeout):
        self.process.stdin.write(json.dumps(value, separators=(',', ':')) + '\n')
        self.process.stdin.flush()
        return self.receive(timeout)

    def close(self):
        try:
            self.process.stdin.close()
            self.process.wait(timeout=15)
            return {'exit_code': self.process.returncode, 'adapter_forced_kill': False}
        except (OSError, subprocess.TimeoutExpired):
            self.process.kill()
            self.process.wait(timeout=5)
            return {'exit_code': self.process.returncode, 'adapter_forced_kill': True}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run', action='store_true')
    parser.add_argument('--classpath', type=Path)
    parser.add_argument('--product-manifest', type=Path)
    parser.add_argument('--node', default='C:/Program Files/nodejs/node.exe')
    parser.add_argument('--java-heap-mb', type=int, default=768)
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'target/answer-selection-20261004/fresh-run')
    args = parser.parse_args()
    if args.run and not (args.classpath and args.product_manifest):
        parser.error('--run requires frozen --classpath and --product-manifest')
    jobs, manifest = prepared()
    if not args.run:
        print(json.dumps({'game_count': len(jobs), 'status': 'prepared_not_executed',
                          'engine_executed': False, 'product_job_executed': False}))
        return
    cp, product, bridge = validate_runtime(args, manifest)
    specification = importlib.util.spec_from_file_location('reused_fresh_probe_facts', ORIGINAL / 'run.py')
    reuse = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(reuse)  # Original __main__ guard prevents old preparation/execution.
    out = args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)  # Never replace an earlier run or failed attempt.
    began_run = time.monotonic()
    manifest.update({'status': 'running', 'output_directory': str(out), 'product_job_executed': False,
                     'product_job_execution_requested': False,
                     'engine_executed': False, 'startups': [], 'shutdowns': [],
                     'java_heap_mb': args.java_heap_mb, 'java_initial_heap_mb': 32, 'java_code_cache_mb': 64})
    save(out / 'manifest.json', manifest)
    results, java, node = [], None, None
    stderr = (out / 'process-stderr.log').open('w', encoding='utf-8')

    def stop_processes():
        nonlocal java, node
        for name, rpc in [('node_bridge', node), ('java_reducer', java)]:
            if rpc:
                manifest['shutdowns'].append({'process': name, **rpc.close()})
        java = node = None

    def start_processes():
        nonlocal java, node
        start = time.monotonic()
        java = RPC([cp['java'], '-Xms32m', '-Xmx' + str(args.java_heap_mb) + 'm',
                    '-XX:ReservedCodeCacheSize=64m', '-cp', os.pathsep.join(cp['runtime_classpath']),
                    'io.chesstory.runtime.FullGameProbe'], stderr)
        node = RPC([args.node, str(HERE / 'fresh-engine-bridge.bundle.mjs'), str(ASSET)], stderr)
        manifest['engine_executed'] = True
        startup = {'startup_index': len(manifest['startups']) + 1, 'status': 'starting'}
        manifest['startups'].append(startup)
        save(out / 'manifest.json', manifest)
        ready = node.receive(20)
        assert ready.get('ready'), 'Actual frontend bridge did not initialize'
        startup.update({'status': 'ready',
            'adapter_startup_ms': round((time.monotonic() - start) * 1000), 'raw_ready': ready,
            'engine_ready_before_job': True, 'jvm_first_begin_not_in_startup_measurement': True})
        save(out / 'manifest.json', manifest)

    def journal_row(handle, value):
        handle.write(json.dumps(value, ensure_ascii=False, separators=(',', ':')) + '\n')
        handle.flush()

    try:
        with gzip.open(out / 'journal.jsonl.gz', 'wt', encoding='utf-8') as journal, \
             gzip.open(out / 'final-snapshots.jsonl.gz', 'wt', encoding='utf-8') as finals:
            for index, job in enumerate(jobs):
                request = job['request']  # Never mutate or replace this request.
                snapshot, works, adapter_error = None, [], None
                issued_count = bridge_execution_count = submission_attempts = report_responses = 0
                request_started = time.monotonic()
                try:
                    if not java or not node:
                        start_processes()
                    request_started = time.monotonic()
                    manifest['product_job_execution_requested'] = True
                    wrapper = java.call({'command': 'begin', 'request': request, 'probe_threats': False}, 30)
                    manifest['product_job_executed'] = True
                    journal_row(journal, {'game_id': job['id'], 'request': request, 'begin_response': wrapper})
                    if 'research_error' in wrapper:
                        raise RuntimeError('Public reducer rejected begin; exact response stored')
                    snapshot = wrapper['snapshot']
                    while 'issued_engine_work' in snapshot:
                        work = snapshot['issued_engine_work']
                        issued_count += 1
                        if len(works) >= WORK_CAP:
                            adapter_error = 'adapter_work_cap_exceeded_original_work_left_pending'
                            journal_row(journal, {'game_id': job['id'], 'unexecuted_issued_work': work,
                                                 'adapter_error': adapter_error, 'snapshot': snapshot})
                            break
                        journal_row(journal, {'game_id': job['id'], 'issued_work': work,
                                             'execution_started': True, 'snapshot_before': snapshot})
                        execution = node.call({'request': request, 'snapshot': snapshot}, 40)
                        bridge_execution_count += 1
                        report = execution.get('report')
                        report_source = 'actual_frontend_report'
                        if report is None:
                            report_source = 'adapter_generated_engine_failure_after_missing_frontend_report'
                            report = {'schema_version': 'chesstory.position-commentary.engine-work-report.v6',
                                'engine_profile': work['engine_profile'], 'work_id': work['work_id'],
                                'execution_key_sha256': work['execution_key_sha256'],
                                'outcome': {'kind': 'executor_failed', 'failure_code': 'engine_failure'}}
                        step = {'game_id': job['id'], 'issued_work': work, 'report': report,
                                'execution': execution, 'report_source': report_source}
                        works.append(step)
                        journal_row(journal, {**step, 'submitted_to_public_reducer': False})
                        submission_attempts += 1
                        wrapper = java.call({'command': 'report', 'report': report}, 30)
                        report_responses += 1
                        journal_row(journal, {**step, 'submitted_to_public_reducer': True, 'response': wrapper})
                        if 'research_error' in wrapper:
                            raise RuntimeError('Public reducer rejected report; exact response stored')
                        snapshot = wrapper['snapshot']
                except (OSError, TimeoutError, ValueError, RuntimeError, AssertionError) as error:
                    adapter_error = f'{type(error).__name__}: {error}'
                    journal_row(journal, {'game_id': job['id'], 'adapter_error': adapter_error,
                                         'last_snapshot': snapshot, 'no_request_retry': True})
                    stop_processes()  # Restart only for the NEXT fixed request, never rerun this job.
                state = snapshot.get('state', snapshot.get('progress', {}).get('phase', 'unknown')) if snapshot else 'unavailable'
                result = {'game_id': job['id'], 'request_id': request['request_id'], 'elapsed_ms':
                    round((time.monotonic() - request_started) * 1000), 'work_count': len(works),
                    'issued_work_count': issued_count, 'bridge_execution_response_count': bridge_execution_count,
                    'report_submission_attempt_count': submission_attempts, 'reducer_report_response_count': report_responses,
                    'state': state, 'stop_condition': snapshot.get('stop_condition') if snapshot else None,
                    'adapter_error': adapter_error, 'pending_work_preserved': bool(snapshot and 'issued_engine_work' in snapshot),
                    'execution_group': 'first_job_engine_ready_jvm_first_begin' if index == 0 else 'later_fixed_job',
                    'executor_failure_report_count': sum(step['report'].get('outcome', {}).get('kind') == 'executor_failed' for step in works),
                    'adapter_generated_failure_report_count': sum(step['report_source'] != 'actual_frontend_report' for step in works)}
                facts_path = out / f"{job['id']}-obligation-facts.json"
                try:
                    factual = reuse.facts(snapshot) if snapshot else {'paths': [], 'comparison_pairs': []}
                    factual.update({'game_id': job['id'], 'exact_request': request,
                        'snapshot_sha256': hashlib.sha256(canonical(snapshot)).hexdigest(),
                        'execution_status': result, 'extraction_scope': manifest['fact_scope'],
                        'product_prose_used': False, 'reader_oracle_written': False,
                        'limits': 'Only actually admitted provided lines; no claim all alternatives or possible threats investigated.'})
                except (KeyError, ValueError, AssertionError, IndexError) as error:
                    factual = {'game_id': job['id'], 'exact_request': request, 'execution_status': result,
                        'facts_export_error': f'{type(error).__name__}: {error}',
                        'raw_snapshot_preserved': True, 'product_prose_used': False, 'reader_oracle_written': False}
                save(facts_path, factual)
                result['obligation_facts_sha256'] = sha(facts_path)
                journal_row(finals, {'game_id': job['id'], 'request': request, 'snapshot': snapshot, 'result': result})
                results.append(result)
                save(out / 'results.json', results)
                print(json.dumps({key: result[key] for key in ('game_id', 'elapsed_ms', 'work_count', 'state', 'adapter_error')}), flush=True)
    finally:
        stop_processes()
        stderr.close()
        manifest.update({'status': 'cohort_finished' if len(results) == 18 else 'adapter_interrupted_partial_cohort',
            'completed_request_attempts': len(results), 'elapsed_ms': round((time.monotonic() - began_run) * 1000),
            'original_failure_or_stop_not_reclassified_as_success': True, 'no_balance_reselection': True})
        try:
            check_hashes(ROOT, product['source_sha256'], 'Product sources after run')
            check_hashes(ROOT, bridge['source_hashes'], 'Frontend sources after run')
            check_hashes(Path(manifest['product_class_directory']), product['class_sha256'], 'Product classes after run')
            manifest['post_run_source_and_class_identity'] = 'unchanged'
        except AssertionError as error:
            manifest['post_run_source_and_class_identity'] = f'drift_detected: {error}'
        manifest['output_hashes'] = {path.name: sha(path) for path in out.iterdir() if path.is_file() and path.name != 'manifest.json'}
        save(out / 'manifest.json', manifest)


if __name__ == '__main__':
    main()
