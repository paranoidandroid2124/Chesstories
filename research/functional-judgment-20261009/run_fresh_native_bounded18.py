"""Two original native cases, one strict finite production run, no copied or synthetic reports."""
from __future__ import annotations
import argparse
import datetime
import gzip
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
PLAN = ROOT / 'research/functional-judgment-20261009/fresh-native-two-case-depth18-plan.json'
BRIDGE = ROOT / 'target/functional-judgment-20261009/product-bridge-v21'
TRANSPORT = ROOT / 'target/annotated-games-20261003/FullGameProbe.scala'
ASSET = ROOT / 'target/annotated-games-20261003/engine/uci.mjs'
RPC_SOURCE = ROOT / 'research/answer-selection-20261004/fresh-run.py'

def read(path):
    return json.loads(path.read_text(encoding='utf-8'))

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def save(path, value):
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')

def check_hashes(base, hashes):
    for path, expected in hashes.items():
        assert sha(base / path) == expected, path

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--version', default='10s')
    parser.add_argument('--plan-sha256', required=True)
    parser.add_argument('--prepare-only', action='store_true')
    args = parser.parse_args()
    assert args.version.isalnum()
    assert sha(PLAN) == args.plan_sha256
    policy = read(PLAN)
    compiled = ROOT / 'target/relationship-delivery-20261004'
    cp = read(compiled / f'classpath-{args.version}.json')
    product = read(compiled / f'main-manifest-{args.version}.json')
    assert product['exit_code'] == 0
    where = ROOT / f'target/functional-judgment-20261009/fresh-native-two-case-{args.version}-normal21-depth18'
    if not where.exists():
        where.mkdir()
        rows = [read(ROOT / name)[0] for name in policy['source_inputs']]
        assert [row['game_id'] for row in rows] == policy['cases_in_fixed_order']
        assert all(row['request']['remaining_budget_ms'] == 180000 for row in rows)
        assert all(not any(key in row['request'] for key in ['proposals', 'input_lines']) for row in rows)
        save(where / 'fixed-requests.json', rows)
        frozen_inputs = [PLAN, Path(__file__), RPC_SOURCE, TRANSPORT,
            compiled/f'main-manifest-{args.version}.json', compiled/f'classpath-{args.version}.json',
            *[ROOT/name for name in policy['source_inputs']],
            *sorted(path for path in ASSET.parent.iterdir() if path.is_file())]
        source_hashes = {str(path.relative_to(ROOT)).replace('\\', '/'):sha(path)
            for path in frozen_inputs}
        bridge = read(BRIDGE / 'bridge-manifest.json')
        check_hashes(ROOT, product['source_sha256'])
        check_hashes(Path(cp['runtime_classpath'][0]), product['class_sha256'])
        check_hashes(ROOT, bridge['source_hashes'])
        assert sha(BRIDGE / 'fresh-bridge.bundle.mjs') == bridge['bundle_sha256']
        classes = where / 'transport-classes'
        classes.mkdir()
        command = [cp['java'], '-Xms32m', '-Xmx1024m', '-XX:ReservedCodeCacheSize=96m',
            '-cp', os.pathsep.join(cp['compiler_classpath']), 'dotty.tools.dotc.Main',
            '-classpath', os.pathsep.join(cp['runtime_classpath']), '-d', str(classes), str(TRANSPORT)]
        before = time.monotonic()
        with (where / 'transport-compile.log').open('w', encoding='utf-8') as log:
            result = subprocess.run(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT)
        freeze = {'sealed_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
            'compile_exit_code': result.returncode, 'compile_elapsed_ms':round((time.monotonic()-before)*1000),
            'version':args.version, 'plan_sha256':sha(PLAN), 'source_sha256':source_hashes,
            'product_manifest_sha256':sha(compiled/f'main-manifest-{args.version}.json'),
            'product_source_sha256':product['source_sha256'], 'product_class_sha256':product['class_sha256'],
            'transport_class_sha256':{str(path.relative_to(classes)).replace('\\', '/'):sha(path)
                for path in classes.rglob('*') if path.is_file()},
            'normal21_manifest_sha256':sha(BRIDGE/'bridge-manifest.json'),
            'normal21_bundle_sha256':bridge['bundle_sha256'], 'normal21_source_sha256':bridge['source_hashes'],
            'request_sha256':sha(where/'fixed-requests.json'), 'engine_queries_at_freeze':0}
        save(where/'pre-dispatch-freeze.json', freeze)
        assert result.returncode == 0, 'Preserved transport compilation failure'
    freeze = read(where/'pre-dispatch-freeze.json')
    assert freeze['compile_exit_code'] == 0 and freeze['plan_sha256'] == args.plan_sha256
    def verify():
        check_hashes(ROOT, freeze['source_sha256'])
        check_hashes(ROOT, freeze['product_source_sha256'])
        check_hashes(Path(cp['runtime_classpath'][0]), freeze['product_class_sha256'])
        check_hashes(where/'transport-classes', freeze['transport_class_sha256'])
        check_hashes(ROOT, freeze['normal21_source_sha256'])
        assert sha(BRIDGE/'fresh-bridge.bundle.mjs') == freeze['normal21_bundle_sha256']
        assert sha(where/'fixed-requests.json') == freeze['request_sha256']
    verify()
    if args.prepare_only:
        print(json.dumps({'prepared':str(where), 'freeze_sha256':sha(where/'pre-dispatch-freeze.json'),
            'engine_queries':0}), flush=True)
        return
    out = where / 'actual-fresh-run'
    assert not out.exists(), 'Preserve all attempts; no retry or overwrite'
    out.mkdir()
    spec = importlib.util.spec_from_file_location('normal_public_rpc', RPC_SOURCE)
    reuse = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(reuse)
    reuse.ROOT = ROOT
    class StreamingRPC(reuse.RPC):
        unusable = False
        def receive(self, timeout):
            try:
                return super().receive(timeout)
            except Exception:
                self.unusable = True
                raise
        def call(self, value, timeout):
            try:
                json.dump(value, self.process.stdin, separators=(',', ':'))
                self.process.stdin.write('\n')
                self.process.stdin.flush()
            except Exception:
                self.unusable = True
                raise
            return self.receive(timeout)
    stderr = (out/'process.log').open('w', encoding='utf-8')
    began = time.monotonic()
    java, node = None, None
    manifest = {'started_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),
        'status':'running', 'freeze_sha256':sha(where/'pre-dispatch-freeze.json'),
        'version':args.version,'normal_UI':21,'maximum_pending_depth':18,
        'production_budget_overlay':False,'engine_lease_overlay':False,'replayed_reports':0,
        'new_queries':0,'dispatch_attempts':0,'query_count_is_lower_bound':False,
        'cases':[], 'post_identity_check':None}
    save(out/'manifest.json', manifest)
    try:
        java = StreamingRPC([cp['java'],'-Xms64m','-Xmx2048m','-XX:ReservedCodeCacheSize=96m',
            '-cp',os.pathsep.join([str(where/'transport-classes'),*cp['runtime_classpath']]),
            'io.chesstory.runtime.FullGameProbe'],stderr)
        node = StreamingRPC(['C:/Program Files/nodejs/node.exe',str(BRIDGE/'fresh-bridge.bundle.mjs'),str(ASSET)],stderr)
        manifest['engine_ready'] = node.receive(20)
        assert manifest['engine_ready'].get('ready')
        for row in read(where/'fixed-requests.json'):
            started = time.monotonic()
            case = {'game_id':row['game_id'],'request':row['request'],'new_queries':0,
                'dispatch_attempts':0,'query_count_is_lower_bound':False,
                'reports':[],'rpc_timings':[], 'error':None,'snapshot':None}
            def call(command):
                before = time.monotonic()
                wrapper = java.call(command,240)
                case['rpc_timings'].append({'command':command['command'],
                    'elapsed_ms':round((time.monotonic()-before)*1000)})
                assert 'research_error' not in wrapper, wrapper.get('research_error')
                return wrapper['snapshot']
            with gzip.open(out/f"{row['game_id']}-journal.jsonl.gz",'wt',encoding='utf-8',compresslevel=1) as journal:
                def record(value):
                    json.dump(value,journal,ensure_ascii=False,separators=(',', ':'))
                    journal.write('\n')
                    journal.flush()
                try:
                    snapshot = call({'command':'begin','request':row['request'],'probe_threats':False})
                    case['snapshot'] = snapshot
                    record({'command':'begin','snapshot':snapshot})
                    while 'issued_engine_work' in snapshot:
                        work = snapshot['issued_engine_work']
                        assert work['search_limits']['depth'] <= 18, 'No above18 work may reach the engine'
                        assert len(case['reports']) < policy['independent_adapter_guard_works_per_case']
                        if snapshot['deadline_epoch_ms'] <= round(time.time()*1000):
                            snapshot = call({'command':'expire'})
                            case['snapshot'] = snapshot
                            record({'command':'expire','snapshot':snapshot})
                            break
                        case['dispatch_attempts'] += 1
                        manifest['dispatch_attempts'] += 1
                        try:
                            execution = node.call({'request':row['request'],'snapshot':snapshot},
                                max(40,work['max_search_elapsed_ms']/1000+20))
                        except Exception:
                            case['query_count_is_lower_bound'] = True
                            manifest['query_count_is_lower_bound'] = True
                            raise
                        go_count = sum(1 for event in execution.get('execution', {}).get('transcript', [])
                            for line in event.get('sent', '').splitlines()
                            if line.strip().startswith('go '))
                        case['new_queries'] += go_count
                        manifest['new_queries'] += go_count
                        record({'issued_work':work,'execution':execution})
                        report = execution.get('report')
                        assert report, 'Normal21 rejected work or executor produced no report; preserve pending'
                        case['reports'].append({'issued_work':work,'report':report})
                        snapshot = call({'command':'report','report':report})
                        case['snapshot'] = snapshot
                        record({'command':'report','snapshot':snapshot})
                        print(json.dumps({'case':row['game_id'],'work':work['work_id'],
                            'depth':work['search_limits']['depth'],'outcome':report['outcome']['kind'],
                            'job_elapsed_ms':round((time.monotonic()-started)*1000)}),flush=True)
                except Exception as error:
                    case['error'] = repr(error)
                case['elapsed_ms'] = round((time.monotonic()-started)*1000)
                case['pending'] = 'issued_engine_work' in (case['snapshot'] or {})
                payload = {'game_id':row['game_id'],'request':row['request'],'snapshot':case['snapshot']}
                raw = json.dumps(payload,ensure_ascii=False,separators=(',', ':')).encode('utf-8')
                (out/f"{row['game_id']}-final-snapshot.json.gz").write_bytes(gzip.compress(raw,mtime=0))
                # The parent receives only this BASIC view before any private audit.
                current = case['snapshot'] or {}
                public = current.get('result', current.get('assessment', {}))
                played = next((review for review in public.get('selected_move_reviews', [])
                    if 'played' in review.get('selection', {}).get('roles', [])), {})
                roots = [candidate['detail'] for candidate in current.get('observations', {})
                    .get('selection', {}).get('candidates', [])
                    if candidate.get('detail', {}).get('type') == 'root_comparison']
                assert len(roots) <= 1
                detail = roots[0] if roots else {}
                basic_path = out/f"{row['game_id']}-basic-only.json"
                save(basic_path, {'export_kind':'Actual fresh current-production BASIC; private trace omitted',
                    'game_id':row['game_id'], 'current_fen':row['request']['current_fen'],
                    'focus':row['request']['focus'], 'body':detail.get('summary'),
                    'default_question':detail.get('summary_question', {}).get('question'),
                    'verdict':played.get('commentary', {}).get('primary'),
                    'new_engine_queries':case['new_queries'], 'job_elapsed_ms':case['elapsed_ms'],
                    'dispatch_attempts':case['dispatch_attempts'],
                    'query_count_is_lower_bound':case['query_count_is_lower_bound'],
                    'pending':case['pending'], 'adapter_error':case['error'],
                    'snapshot_sha256':sha(out/f"{row['game_id']}-final-snapshot.json.gz")})
                save(out/f"{row['game_id']}-case-record.json",case)
                manifest['cases'].append(dict({key:case[key] for key in
                    ['game_id','new_queries','dispatch_attempts','query_count_is_lower_bound',
                     'elapsed_ms','error','pending']},
                    basic_only_path=str(basic_path), basic_only_sha256=sha(basic_path)))
                save(out/'manifest.json',manifest)
                if java.unusable or node.unusable:
                    manifest['stopped_after_rpc_failure'] = row['game_id']
                    break
    except Exception as error:
        manifest['outer_error'] = repr(error)
    finally:
        manifest['shutdown'] = {}
        for name, rpc in [('node',node),('java',java)]:
            if rpc is not None:
                try:
                    manifest['shutdown'][name] = rpc.close()
                except Exception as error:
                    manifest['shutdown'][name] = {'cleanup_error':repr(error)}
        stderr.close()
        manifest['elapsed_ms'] = round((time.monotonic()-began)*1000)
        manifest['finished_utc'] = datetime.datetime.now(datetime.timezone.utc).isoformat()
        try:
            verify()
            manifest['post_identity_check'] = 'unchanged'
        except Exception as error:
            manifest['post_identity_check'] = 'failed'
            manifest['identity_check_error'] = repr(error)
        manifest['status'] = ('finished' if not manifest.get('outer_error') and
            manifest['post_identity_check'] == 'unchanged' and
            len(manifest['cases']) == 2 and not any(case['error'] or case['pending'] for case in manifest['cases'])
            else 'attempted_incomplete')
        save(out/'manifest.json',manifest)
        print(json.dumps({key:value for key,value in manifest.items() if key not in ['engine_ready','shutdown']}),flush=True)

if __name__ == '__main__':
    main()
