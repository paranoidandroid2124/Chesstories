"""Seal the parent's BASIC-only reading before opening either private fresh trace."""
from pathlib import Path
from datetime import datetime, timezone
import hashlib
import json

ROOT = Path(__file__).resolve().parents[2]
HERE = ROOT / 'research/functional-judgment-20261009'
SOURCE = ROOT / 'target/functional-judgment-20261009/fresh-native-two-case-10s-normal21-depth18/actual-fresh-run'
OUT = HERE / 'fresh-native-two-case10s-basic-human-firstpass.json'
assert not OUT.exists()
readings = [
    {'game_id': 'FC-SG45-NB1', 'minimum_answer_complete': False,
     'recoverable': [
         'Nc4 gives the original knight d6 control; Nb1 lacks the same immediate Nxd6 recapture in the explicitly conditional Nef6/Rd6/Qxd6 line.',
         'Qc7 evades the rook attack; later Rad1 supplies current rook support.',
         'The displayed current queen exchange and subsequent e4 pawn capture are readable in their recorded branch.'],
     'missing': [
         'A checked comparison of the useful own plans under the relevant current defenses.',
         'Why the conditional d6 option changes the original judgment rather than merely being legally available.',
         'How later loss of the original provider and changed support alter the current choice value.'],
     'excess': 'A long relation and event tail competes with the conditional lead.',
     'assessment': 'Partial conditional geometry and recorded price explanation; no complete root-value answer.'},
    {'game_id': 'H05', 'minimum_answer_complete': False,
     'recoverable': ['The two displayed finite forecasts contain no material loss or promotion through their stated endpoints.'],
     'missing': [
         'Why spending g4 immediately is inferior to preparing king progress.',
         'The role of retained waiting resources, effective f6 resistance and the opportunity to execute a useful queenside exchange.',
         'How the useful plan changes when the opponent chooses another relevant defense.'],
     'excess': 'The material-neutral summary alone gives no mover-purpose explanation.',
     'assessment': 'Incomplete ordinary-purpose and comparison answer. The fresh root forecast differs from the historical native replay; no reason for that difference is inferred from BASIC.'}
]
for value in readings:
    path = SOURCE / (value['game_id'] + '-basic-only.json')
    basic = json.loads(path.read_text(encoding='utf-8'))
    assert basic['game_id'] == value['game_id'] and basic['adapter_error'] is None
    value.update({'basic_path': path.relative_to(ROOT).as_posix(),
                  'basic_sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
                  'body_characters': len(basic['body'] or ''),
                  'default_question': basic['default_question'],
                  'fresh_verdict': basic['verdict'],
                  'pending': basic['pending']})
result = {'sealed_utc': datetime.now(timezone.utc).isoformat(),
          'reader': 'Parent; only the two BASIC-only exports read before sealing',
          'prior_outputs_known': True, 'not_a_blind_holdout': True,
          'criteria_unchanged': True, 'private_trace_read_before_seal': False,
          'old_four_full_utility_passed': 0, 'fixed_eight_source_full_passed': 0,
          'new_two_case_full_minimum_answers': 0,
          'fresh_numeric_scope': 'Each fresh root verdict belongs to its own new normal-engine receipt. It is not required to equal an older analysis. Child interpretations must preserve this current root verdict.',
          'cases': readings}
OUT.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
print(json.dumps({'path': str(OUT), 'sha256': hashlib.sha256(OUT.read_bytes()).hexdigest(),
                  'basic_firstpass_sealed': True, 'new_engine_queries': 0}))
