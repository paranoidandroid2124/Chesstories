"""Compile named current test sources against one verified main freeze only."""
import argparse
import hashlib
import json
import os
import pathlib
import subprocess
import time

ROOT = pathlib.Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser()
parser.add_argument('version')
parser.add_argument('label')
parser.add_argument('--sources',nargs='+',required=True)
parser.add_argument('--suites',nargs='+',required=True)
parser.add_argument('--heap-mb',type=int,default=2048)
args = parser.parse_args()
assert args.version.isalnum() and all(c.isalnum() or c in '-_' for c in args.label)
TARGET = ROOT/'target/functional-judgment-20261009'/args.label
assert not TARGET.exists(), 'Keep previous attempts intact'
TARGET.mkdir()


def read(path):
    return json.loads(path.read_text(encoding='utf-8'))


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def save(path,data):
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')


base = ROOT/'target/relationship-delivery-20261004'
main = read(base/f'main-manifest-{args.version}.json')
cp = read(base/f'classpath-{args.version}.json')
assert main['exit_code'] == 0
sources = sorted(ROOT/f'chesstory-runtime/src/test/scala/io/chesstory/runtime/{name}.scala' for name in args.sources)
assert all(path.is_file() for path in sources)
before = {str(path.relative_to(ROOT)):sha(path) for path in sources}
test_deps = [path for path in (ROOT/'chesstory-runtime/target/streams/test/dependencyClasspath/_global/streams/export')
    .read_text(encoding='utf-8').strip().split(';') if path.endswith('.jar')]


def verify_main():
    for name,digest in main['source_sha256'].items():
        assert sha(ROOT/name)==digest,'Main source changed: '+name
    for name,digest in main['class_sha256'].items():
        assert sha(pathlib.Path(cp['runtime_classpath'][0])/name)==digest,name


verify_main()
plan = {'version':args.version,'test_sources_sha256':before,'main_manifest_sha256':sha(base/f'main-manifest-{args.version}.json'),
    'main_class_count':len(main['class_sha256']),'test_jars_sha256':{path:sha(pathlib.Path(path)) for path in test_deps},
    'suites':args.suites,'heap_mb':args.heap_mb,'engine_queries':0,'old_test_classes_on_classpath':False}
save(TARGET/'plan.json',plan)
classes = TARGET/'test-classes'
classes.mkdir()
classpath = [cp['runtime_classpath'][0],*test_deps]
start=time.monotonic()
compile_result = subprocess.run([cp['java'],f'-Xmx{args.heap_mb}m','-cp',os.pathsep.join(cp['compiler_classpath']),
    'dotty.tools.dotc.Main','-release:21','-classpath',os.pathsep.join(classpath),'-d',str(classes),
    *map(str,sources)],capture_output=True,text=True,encoding='utf-8')
(TARGET/'compile.log').write_text(compile_result.stdout+compile_result.stderr,encoding='utf-8')
if compile_result.returncode:
    print((compile_result.stdout+compile_result.stderr)[-14000:])
    save(TARGET/'result.json', {'compile_exit_code': compile_result.returncode,
        'test_exit_code': None, 'engine_queries': 0,
        'plan_sha256': sha(TARGET/'plan.json'),
        'compile_log_sha256': sha(TARGET/'compile.log')})
    raise SystemExit(compile_result.returncode)
run = subprocess.run([cp['java'],f'-Xmx{args.heap_mb}m','-cp',os.pathsep.join([str(classes),*classpath]),
    'org.junit.runner.JUnitCore',*['io.chesstory.runtime.'+suite for suite in args.suites]],
    capture_output=True,text=True,encoding='utf-8')
(TARGET/'run.log').write_text(run.stdout+run.stderr,encoding='utf-8')
print(run.stdout+run.stderr)
after = {str(path.relative_to(ROOT)):sha(path) for path in sources}
assert before==after,'Test source drift during execution'
verify_main()
result = {'compile_exit_code':compile_result.returncode,'exit_code':run.returncode,
    'elapsed_seconds':round(time.monotonic()-start,3),'plan_sha256':sha(TARGET/'plan.json'),
    'compile_log_sha256':sha(TARGET/'compile.log'),'run_log_sha256':sha(TARGET/'run.log'),
    'test_sources_sha256':after,'compiled_test_classes_sha256':{
        path.relative_to(classes).as_posix():sha(path) for path in classes.rglob('*') if path.is_file()}}
save(TARGET/'result.json',result)
print(json.dumps({key:value for key,value in result.items() if not key.endswith('classes_sha256') and key!='test_sources_sha256'}))
raise SystemExit(run.returncode)
