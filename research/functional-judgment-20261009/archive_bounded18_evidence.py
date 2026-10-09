"""Preserve exact completed study bytes in a small portable compressed archive."""
import gzip
import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
STUDY = ROOT/'target/functional-judgment-20261009/h05-response-plan-bounded18-ui21b'
ARCHIVE = ROOT/'research/functional-judgment-20261009/bounded18-study-evidence'
assert not ARCHIVE.exists(), 'Never overwrite preserved evidence'
ARCHIVE.mkdir()
files = [STUDY/name for name in ['study-plan.json','phase2-continuation-study-plan.json',
    'preparation-manifest.json',*['stage-'+suffix for suffix in [
        '01-root-and-current-pools18.json','02-current-owned-comparisons18.json',
        '03-current-owned-comparisons18.json','04-root-response-function18.json','05-root-response-function18.json']]]]
files += [STUDY/epoch/name for epoch in ['execution','execution-phase2']
    for name in ['results.json','summary.json','manifest.json','boot.json']]
index = {}
for source in files:
    raw = source.read_bytes()
    relative = source.relative_to(STUDY)
    destination = ARCHIVE/relative.with_suffix('.json.gz')
    destination.parent.mkdir(parents=True,exist_ok=True)
    compressed = gzip.compress(raw,mtime=0)
    destination.write_bytes(compressed)
    assert gzip.decompress(compressed) == raw
    index[str(relative).replace('\\','/')] = {'raw_sha256':hashlib.sha256(raw).hexdigest(),
        'compressed_sha256':hashlib.sha256(compressed).hexdigest(),
        'raw_bytes':len(raw),'compressed_bytes':len(compressed)}
manifest = {'kind':'Byte-exact portable archive of actual depth18 study; no report/descriptor/value mutations',
    'original_study_directory':str(STUDY),'files':index,'new_engine_queries':0,
    'actual_query_count':13,'maximum_nominal_depth':18,'product_reports_submitted':0}
(ARCHIVE/'archive-manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
print(json.dumps({'files':len(index),'raw_bytes':sum(v['raw_bytes'] for v in index.values()),
    'compressed_bytes':sum(v['compressed_bytes'] for v in index.values()),
    'manifest_sha256':hashlib.sha256((ARCHIVE/'archive-manifest.json').read_bytes()).hexdigest()}))
