"""Parent literal-text judgment; public endpoint exposure is disclosed, not hidden."""
from pathlib import Path
from datetime import datetime, timezone
import hashlib
import json

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
SOURCE = HERE / 'source-scene-current10s-normal21-basic-text-only.json'
ORIGINAL = HERE / 'source-scene-current10s-normal21-basic.json'
CRITERIA = HERE / 'human-criteria-and-scenes.json'
OUT = HERE / 'source-scene-current10s-normal21-basic-human-firstpass.json'
assert not OUT.exists()

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

assert sha(SOURCE) == 'd019408f255d78372220f236a9dd18a4c1de6016100df7f0d3d501af139038c1'
assert sha(ORIGINAL) == '1002bff3864444fb7f1aca57d3fd7830eebb2d2a311c0209616663b2dce9d53d'
assert sha(CRITERIA) == '4701af6026b62be14d3ac03bcef40d62d3a58e4badfe9ddda6b4f9b054900ed4'
basic = json.loads(SOURCE.read_text(encoding='utf-8'))
original = json.loads(ORIGINAL.read_text(encoding='utf-8'))
readings = [
    {'id': 'FJ-SG35-ROOT-NG8',
     'recoverable': ['The original e7 knight returns via g8 to f6 and restores d5 support.',
                     'Temporary support losses, replacement queen protection of c6, and recorded rook/queen trades are described.'],
     'missing': ['The positive improvement beyond restoring a support role it already had on e7.',
                 'A compact purpose explaining the underemployed knight and retained useful central/kingside choices.'],
     'excess': 'Support-loss comparisons and later queen/rook/capture events compete with the rerouting purpose.',
     'criterion_boundary': 'A concrete improving purpose suffices; no whole attack or unique numerical cause is required. Mere route and restored old support do not yet convey that improvement.'},
    {'id': 'FJ-SG35-ROOT-ND6',
     'recoverable': ['The explicitly conditional Rxb6/Rxb6/Qxc4 geometry and b6 pawn capture.'],
     'missing': ['The consequence of committing the c4 knight early: b6 loss and useful White a-pawn progress.',
                 'The actual consequential response and price instead of the incidental conditional queen capture.'],
     'excess': 'The conditional same-queen capture dominates the lead.'},
    {'id': 'FJ-SG35-LATE-ND6',
     'recoverable': ['Nd6 evades the queen attack, supports f5, and the later c4-knight exchange leaves the f6 knight.',
                     'The two exact e4 exchanges are visible in their own branches.'],
     'missing': ['Why keeping the choice of which knight enters e4 is useful at this later decision.',
                 'The surviving knight’s ready d5 role and continuing pressure, without requiring the author’s exact PV.'],
     'excess': 'Repeated survival/attack events compete with the option-preservation judgment.'},
    {'id': 'FJ-SG35-LATE-NE4',
     'recoverable': ['Ne4 attacks Ng3; both Bxe4/fxe4 and Nxe4/dxe4 exchange prices are explicit.'],
     'missing': ['Why this immediate f6-knight commitment reduces useful pressure or options compared with delayed Nd6.',
                 'The functional difference after the exchange, rather than just its material sequence.'],
     'excess': 'Several reference-line rook and queen attacks obscure the actual early-commitment issue.'},
    {'id': 'FJ-SG42-RD3',
     'recoverable': ['c3/Bb3 alignment, c3 pressure, the same rook’s Rd2 continuation, and current Re8 replacement support of e2.',
                     'The observed d4 capture and the separate pawn exchange are stated.'],
     'missing': ['A centered purpose connecting rook activation and bishop restriction with maintaining the passed-pawn clamp.',
                 'How the described Rd2/Rd4 change performs the continuing conversion task; the facts are scattered and repeatedly described as lost attacks.'],
     'excess': 'Lost-support and broken-alignment comparisons bury the useful purpose.',
     'purpose_components_present': True,
     'criterion_boundary': 'No free c3 win, exact b2 route, permanent original defender, or sole small-score cause is required. This is an organization/task-connection failure, not a demand for every legal reply.'},
    {'id': 'FJ-SG42-BD3',
     'recoverable': ['Bd3 supplies e2 support and the later Bf5/Ra3 line attacks the White bishop.'],
     'missing': ['Why strengthening the clamp helps the continuing rook conversion when the bishop and rook roles change.',
                 'A centered purpose for this sound alternative instead of an unconnected Rf8 contrast.'],
     'excess': 'Pawn exchanges, bishop evasions and the Rf8 branch lead before the useful purpose.'},
    {'id': 'FJ-SG17-BXF4',
     'recoverable': ['Bxf4 takes the knight and evades a queen attack.',
                     'The actual Rxb7+ discovered check and Bd7 block are explicit, with recorded queen/rook trades.'],
     'missing': ['A compact missed-defense judgment explaining the checking temptation and its consequence.',
                 'The consequential g-file cost/constraint and value under stronger resistance; surviving pieces alone do not establish it.'],
     'excess': 'A long parallel exchange and king/check history obscures the main refutation and its scope.'},
    {'id': 'FJ-SG17-QXD7',
     'recoverable': ['Qxd7 takes the rook, Qxe5 takes the loose bishop, and Bxf4 would take the knight.'],
     'missing': ['Why White’s safety, active bishop and compensation change the judgment of the exchange gain.',
                 'The missed defensive resource organized as the comparison answer, with the relevant consequential resistance.'],
     'excess': 'Queen attacks, routine rook support and recorded continuation labels replace the compensation judgment.'}
]
assert len(basic['cases']) == len(original['cases']) == len(readings) == 8
for row, previous, reading in zip(basic['cases'], original['cases'], readings):
    assert row['id'] == previous['id'] == reading['id']
    assert row['text'] == previous['text'] and row['default_question'] == previous['default_question']
    assert not row['pending'] and row['adapter_error'] is None
    reading.update({'minimum_answer_complete': False, 'status': 'partial',
                    'basic_text_sha256':hashlib.sha256(row['text'].encode('utf-8')).hexdigest(),
                    'basic_text_characters':len(row['text']), 'default_question':row['default_question'],
                    'verdict_code':row['verdict'].get('verdict_code', row['verdict']['kind'])})
result = {'sealed_utc':datetime.now(timezone.utc).isoformat(),
          'epoch':'10s', 'reader':'Parent literal BASIC text first pass before private source8 audit',
          'basic_path':SOURCE.relative_to(ROOT).as_posix(), 'basic_sha256':sha(SOURCE),
          'original_basic_path':ORIGINAL.relative_to(ROOT).as_posix(), 'original_basic_sha256':sha(ORIGINAL),
          'criteria_path':CRITERIA.relative_to(ROOT).as_posix(), 'criteria_sha256':sha(CRITERIA),
          'criteria_unchanged':True, 'private_trace_read_before_seal':False,
          'public_endpoint_exposure':'Parent already viewed original public primary endpoint move arrays before the corrected text-only view. Earlier products/book controls were also known. This is not a blinded or strictly text-only exposure. Judgments above count only the literal body; nothing recoverable solely from endpoint paths is credited.',
          'counts':{'cases':8,'full_minimum_basic_answers_complete':0,'partial':8},
          'historical_counts_not_changed':{'old_four': '0/4', 'source_after8b':'0/8'},
          'cases':readings}
OUT.write_text(json.dumps(result,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
print(json.dumps({'path':str(OUT),'sha256':sha(OUT),'full_complete':0,'cases':8}))
