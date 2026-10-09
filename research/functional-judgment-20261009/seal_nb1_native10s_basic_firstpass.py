"""BASIC-only parent seal; no private receipt, price or trace is opened."""
from pathlib import Path
from datetime import datetime, timezone
import hashlib
import json

ROOT = Path(__file__).resolve().parents[2]
HERE = ROOT / 'research/functional-judgment-20261009'
SOURCE = HERE / 'nb1-produced10s-basic-only.json'
OUT = HERE / 'nb1-produced10s-basic-human-firstpass.json'
assert not OUT.exists()
digest = hashlib.sha256(SOURCE.read_bytes()).hexdigest()
assert digest == 'ea1741153df6754a612cc483c4f2f68fdcf426dab368b1abe5c5646ce704b18f'
json.loads(SOURCE.read_text(encoding='utf-8'))
result = {
    'sealed_utc': datetime.now(timezone.utc).isoformat(),
    'reader': 'Parent BASIC first pass; no10s private task/price/UI diagnosis read before this seal',
    'basic_path': SOURCE.relative_to(ROOT).as_posix(), 'basic_sha256': digest,
    'prior_outputs_known': True, 'not_a_blind_holdout': True, 'criteria_unchanged': True,
    'full_old_cases_passed': 0, 'full_fixed_source_cases_passed': 0,
    'recoverable_from_basic': [
        'Nc4 supplies current d6 control, while Nb1 can return with Nc3.',
        'In the matched Nef6 position, Rd6 is competitive with Ra7 after Nc4, while Nb1 prefers Nc3 to immediate Rd6.',
        'Qc7 evades the rook attack and keeps targeting it; the conditional Qxd6/Nxd6 transaction and missing Nb1 recapture are explicit.',
        'Later Rad1 changes current support; the separate recorded continuation carries a displayed rook loss and queen capture.'
    ],
    'not_recoverable_from_basic': [
        'How the adequate own plan changes under every scoped relevant defense.',
        'How the later original-knight loss and replacement queen support change choice value.',
        'A complete bounded reason for the original numerical mistake judgment.'
    ],
    'basic_text_assessment': 'The same bounded useful matched-response and price explanation as10r remains. Its long separate relation/history tail competes with the leading point. No new full root judgment success is counted.',
    'native_vs_fresh_scope': 'This exact26 native-report result is distinct from the new15-query ordinary two-case run; neither prose nor values are substituted between them.'
}
OUT.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
print(json.dumps({'path': str(OUT), 'sha256': hashlib.sha256(OUT.read_bytes()).hexdigest()}))
