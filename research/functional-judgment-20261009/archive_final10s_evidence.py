"""Archive selected exact diagnostic bytes, without repeated public-frame histories."""
from pathlib import Path
from datetime import datetime, timezone
import gzip
import hashlib
import json
import shutil

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
OUT = HERE / 'final10s-evidence'
assert not OUT.exists(), 'Preserve the first archive attempt'
OUT.mkdir()

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def exact(source, name):
    target = OUT / name
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source, target)
    assert sha(source) == sha(target)
    return {'source': source.relative_to(ROOT).as_posix(), 'archive': target.relative_to(HERE).as_posix(),
            'sha256': sha(target), 'bytes': target.stat().st_size, 'byte_exact': True}

files = []
compiled = ROOT / 'target/relationship-delivery-20261004'
for name in ['main-manifest-10s.json', 'test-manifest-10s.json', 'classpath-10s.json']:
    files.append(exact(compiled/name, 'compile/'+name))
runtime = ROOT / 'target/functional-judgment-20261009/final-choice-price-scope-depth18-withdeps10s'
for name in ['plan.json', 'compile.log', 'run.log', 'result.json']:
    files.append(exact(runtime/name, 'runtime210/'+name))
fresh = ROOT / 'target/functional-judgment-20261009/fresh-native-two-case-10s-normal21-depth18'
files.append(exact(fresh/'pre-dispatch-freeze.json', 'fresh-normal15/pre-dispatch-freeze.json'))
files.append(exact(fresh/'fixed-requests.json', 'fresh-normal15/fixed-requests.json'))
files.append(exact(fresh/'actual-fresh-run/manifest.json', 'fresh-normal15/manifest.json'))
for case in ['FC-SG45-NB1', 'H05']:
    for suffix in ['basic-only.json', 'final-snapshot.json.gz']:
        name = case+'-'+suffix
        files.append(exact(fresh/'actual-fresh-run'/name, 'fresh-normal15/'+name))
    journal = fresh/'actual-fresh-run'/(case+'-journal.jsonl.gz')
    target = OUT/'fresh-normal15'/(case+'-execution-lines.jsonl.gz')
    count = 0
    with gzip.open(journal, 'rb') as source, target.open('wb') as raw:
        with gzip.GzipFile(filename='', fileobj=raw, mode='wb', mtime=0) as output:
            for line in source:
                record = json.loads(line)
                if 'execution' in record:
                    output.write(line)
                    count += 1
    check = list(gzip.open(target, 'rb'))
    assert len(check) == count
    files.append({'source': journal.relative_to(ROOT).as_posix(), 'source_gzip_sha256':sha(journal),
                  'archive': target.relative_to(HERE).as_posix(), 'sha256':sha(target),
                  'bytes':target.stat().st_size, 'execution_records':count,
                  'selection_rule':'Every original issued_work+execution line, byte-exact uncompressed line, in original order. Repeated public snapshots are omitted; final full snapshot is archived separately.'})
files.append(exact(ROOT/'target/annotated-games-20261003/FullGameProbe.scala', 'support-sources/FullGameProbe.scala'))
node_post = ROOT/'target/functional-judgment-20261009/native-choice-produced-export10s/final-current-postcheck.json'
files.append(exact(node_post, 'native26/final-current-postcheck.json'))
manifest = {'archived_utc':datetime.now(timezone.utc).isoformat(),
            'scope':'Selected exact compilation,210 current tests, actual fresh15 queries and final public snapshots. No value/descriptor/report reinterpretation.',
            'files':files, 'new_engine_queries':0, 'product_changes':0,
            'original_target_files_preserved':True}
(OUT/'archive-manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
print(json.dumps({'files':len(files), 'bytes':sum(value['bytes'] for value in files),
                  'manifest_sha256':sha(OUT/'archive-manifest.json')}))
