"""Preserve the exact separate three-query study, including the plan-copy error."""
from pathlib import Path
from datetime import datetime, timezone
import gzip
import hashlib
import json

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
SOURCE = ROOT / 'target/functional-judgment-20261009/h05-fresh10s-failed-comparisons-nominal16-completion'
OUT = HERE / 'nominal16-completion-evidence'
assert not OUT.exists(), 'Preserve prior archive attempts'
OUT.mkdir()

def sha(data):
    return hashlib.sha256(data).hexdigest()

selected = [
    'study-plan.json', 'stage1.json', 'prepare-manifest.json', 'session.py',
    'execution/boot.json', 'execution/manifest.json', 'execution/results.json',
    'execution/summary.json', 'execution/sealed-stage-01.json',
    'runtime/bridge-manifest.json', 'runtime/direct-bridge.ts',
]
entries = []
for name in selected:
    source = SOURCE / name
    data = source.read_bytes()
    target = OUT / (name + '.gz')
    target.parent.mkdir(parents=True, exist_ok=True)
    with target.open('wb') as stream:
        with gzip.GzipFile(filename='', fileobj=stream, mode='wb', mtime=0) as compressed:
            compressed.write(data)
    assert gzip.decompress(target.read_bytes()) == data
    entries.append({'source': source.relative_to(ROOT).as_posix(),
                    'archive': target.relative_to(HERE).as_posix(),
                    'original_sha256': sha(data), 'gzip_sha256': sha(target.read_bytes()),
                    'original_bytes': len(data), 'gzip_bytes': target.stat().st_size,
                    'uncompressed_byte_exact': True})
manifest = {
    'archived_utc': datetime.now(timezone.utc).isoformat(),
    'scope': 'Exactly three separately authorized actual H05 nominal16 research comparisons; original failures, public grade and plan-copy metadata mistake are not rewritten.',
    'new_engine_queries': 0, 'product_changes': 0,
    'metadata_correction': {'path': 'h05-fresh10s-nominal16-plan-metadata-correction.json',
                            'sha256': sha((HERE/'h05-fresh10s-nominal16-plan-metadata-correction.json').read_bytes())},
    'files': entries,
    'engine_assets_not_embedded': 'Exact asset hashes are retained in the original preparation manifest; bundled executable assets and trained network are not duplicated here.',
}
(OUT/'archive-manifest.json').write_text(json.dumps(manifest, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
print(json.dumps({'files':len(entries), 'compressed_bytes':sum(e['gzip_bytes'] for e in entries),
                  'manifest_sha256':sha((OUT/'archive-manifest.json').read_bytes())}))
