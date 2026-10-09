"""Read-only independent replay of the thirteen existing physical receipts."""
from pathlib import Path
import datetime
import hashlib
import json
import chess

ROOT = Path(__file__).resolve().parents[2]
STUDY = ROOT / 'target/functional-judgment-20261009/h05-response-plan-bounded18-ui21b'
SOURCE = ROOT / 'research/functional-judgment-20261009/h05-bounded18-response-function-and-price-audit-v2.json'
OUT = ROOT / 'research/functional-judgment-20261009/h05-bounded18-response-function-independent-review.json'
assert not OUT.exists(), 'Never replace a sealed audit'
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
read = lambda p: json.loads(p.read_text(encoding='utf-8'))
audit = read(SOURCE)
for rel, digest in audit['study_sources_sha256'].items():
    assert sha(ROOT / rel) == digest, rel
plans = [STUDY / 'study-plan.json', STUDY / 'phase2-continuation-study-plan.json']
manifests = [read(STUDY / name / 'manifest.json') for name in ('execution', 'execution-phase2')]
summaries = [read(STUDY / name / 'summary.json') for name in ('execution', 'execution-phase2')]
rows = [row for name in ('execution', 'execution-phase2') for row in read(STUDY / name / 'results.json')]
by_reading = {row['id']: row for row in audit['query_readings']}
assert len(rows) == len(by_reading) == len({row['id'] for row in rows}) == 13
stages = {}
for manifest, plan in zip(manifests, plans):
    assert sha(plan) == manifest['study_sha256']
    assert manifest['frozen_runtime_hashes_passed_before_and_after']
    assert manifest['changed_frozen_inputs'] == []
    assert manifest['product_reports_submitted'] == 0
    for source in manifest['stages']:
        path = Path(source['path'])
        assert sha(path) == source['sha256']
        stage = read(path)
        assert stage['epoch_id'] == manifest['epoch_id']
        assert stage['depth'] == 18 and stage['product_reports_submitted'] == 0
        for query in stage['queries']:
            stages[query['id']] = (query, source['sha256'])
original = read(plans[0])['original_request']

def physical(fen, moves, prefix):
    board = chess.Board(fen)
    identities = {square: {'origin_square': chess.square_name(square),
        'color': 'white' if piece.color else 'black', 'original_role': piece.symbol().lower()}
        for square, piece in board.piece_map().items()}
    sans, events = [], []
    for index, uci in enumerate(moves):
        move = chess.Move.from_uci(uci)
        assert move in board.legal_moves, (fen, index, uci)
        san = board.san(move)
        actor = identities.pop(move.from_square)
        victim_square = move.to_square
        if board.is_en_passant(move):
            victim_square += -8 if board.turn else 8
        victim_piece = board.piece_at(victim_square)
        victim = identities.pop(victim_square, None)
        assert not board.is_castling(move), 'This audit is scoped to the actual pawn/king receipts'
        identities[move.to_square] = actor
        board.push(move)
        sans.append(san)
        if victim is not None or move.promotion is not None:
            events.append({'ply_offset_zero_based': index, 'uci': uci, 'san': san,
                'original_actor': actor, 'original_victim': victim,
                'captured_current_role': None if victim_piece is None else victim_piece.symbol().lower(),
                'promotion': None if move.promotion is None else chess.piece_symbol(move.promotion),
                'gives_check': board.is_check(), 'fen_after': board.fen()})
    return {'whole_san': sans, 'whole_moves_uci': moves, 'prefix_length': prefix,
        'literal_capture_and_promotion_events': events, 'final_fen': board.fen(),
        'legally_replayed': True, 'terminal_status': 'terminal' if board.is_game_over() else 'nonterminal',
        'final_original_pieces': {chess.square_name(sq): piece for sq, piece in identities.items()}}

checked = []
engine_options = {}
for index, (row, summary) in enumerate(zip(rows, [s for group in summaries for s in group])):
    if index in (0, manifests[0]['new_engine_queries']):
        engine_options = {}
    query, expected_stage = stages[row['id']]
    assert row['query'] == query and row['stage_sha256'] == expected_stage
    assert query['request'] == original
    work = query['work']
    reading = by_reading[row['id']]
    assert reading['work'] == work and reading['execution_key'] == work['executionKeySha256']
    assert reading['request'] == query['request'] and reading['stage_sha256'] == expected_stage
    assert work['workId'] == row['id'] == summary['id']
    assert work['engineProfile'] == original['engine_profile'] == 'sf18-smallnet-t2-h16-v1'
    assert work['searchLimits']['depth'] == 18
    assert work['searchLimits']['nodes'] == 250_000_000
    assert work['searchLimits']['movetimeMs'] == 120_000 and work['maxSearchElapsedMs'] == 121_000
    board = chess.Board(work['enginePositionInitialFen'])
    for uci in work['enginePositionMovesUci']:
        move = chess.Move.from_uci(uci)
        assert move in board.legal_moves
        board.push(move)
    assert board.fen() == work['searchFen']
    legal = sorted(move.uci() for move in board.legal_moves)
    assert query['all_legal_root_uci'] == legal
    assert work['enginePositionMovesUci'] == original['move_prefix_uci'] + query['relative_prefix_uci']
    restriction = work['rootRestriction']
    if restriction['kind'] == 'restricted':
        named = restriction['movesUci']
        assert len(named) == len(set(named)) and set(named) <= set(legal)
    else:
        assert restriction == {'kind': 'unrestricted'}
        named = []
    execution = row['result']['execution']
    assert row['result']['independent_research']
    outcome = execution['outcome']
    assert outcome['kind'] == summary['kind'] == 'completed'
    assert outcome['completedDepth'] == summary['completed_depth'] == 18
    assert outcome['nodes'] == summary['nodes']
    assert execution['elapsed_ms'] == summary['elapsed_ms']
    assert summary['elapsed_ms'] <= work['maxSearchElapsedMs']
    assert len(outcome['lineSuffixes']) == work['searchLimits']['multiPv'] == len(reading['values'])
    assert outcome['bestmoveUci'] == outcome['lineSuffixes'][0]['moves'][0]
    intermediate = [ev.get('depth', 0) for ev in execution['evaluations']]
    intermediate += [pv.get('depth', 0) for ev in execution['evaluations'] for pv in ev.get('pvs', [])]
    assert max(intermediate) <= 18
    sent = [entry['sent'] for entry in execution['transcript'] if 'sent' in entry]
    for command in sent:
        if command.startswith('setoption name ') and ' value ' in command:
            key, value = command.removeprefix('setoption name ').split(' value ', 1)
            engine_options[key] = value
    assert engine_options['Threads'] == '2' and engine_options['Hash'] == '16'
    assert engine_options['MultiPV'] == str(work['searchLimits']['multiPv'])
    for option in ['setoption name Clear Hash', 'ucinewgame']:
        assert option in sent
    position = f"position fen {work['enginePositionInitialFen']} moves {' '.join(work['enginePositionMovesUci'])}".strip()
    assert position in [value.strip() for value in sent]
    go = 'go depth 18 nodes 250000000 movetime 120000'
    if named:
        go += ' searchmoves ' + ' '.join(named)
    assert go in sent
    receipt_values = []
    for suffix, value, exported in zip(outcome['lineSuffixes'], reading['values'], summary['values']):
        assert suffix['moves'] == value['uci'] == exported['uci']
        assert suffix['whiteScore'] == value['white_score'] == exported['white_score']
        assert suffix['depth'] == value['depth'] == exported['depth'] == 18
        assert value['root_uci'] == suffix['moves'][0]
        if named:
            assert value['root_uci'] in named
        ledger = physical(work['enginePositionInitialFen'], work['enginePositionMovesUci'] + suffix['moves'],
                          len(work['enginePositionMovesUci']))
        assert ledger == value['physical_ledger'], row['id'] + ': physical price ledger'
        assert value['san'] == ledger['whole_san'][len(work['enginePositionMovesUci']):]
        receipt_values.append({'move_uci': value['root_uci'], 'san': value['root_san'],
            'white_score': value['white_score'],
            'capture_promotion_events': len(ledger['literal_capture_and_promotion_events']),
            'final_fen': ledger['final_fen'], 'terminal_status': ledger['terminal_status']})
    checked.append({'query_id': row['id'], 'work_id': work['workId'],
        'execution_key': work['executionKeySha256'], 'history': work['enginePositionMovesUci'],
        'search_fen': work['searchFen'], 'restriction': restriction, 'legal_count': len(legal),
        'nominal_depth': 18, 'selective_depth': outcome['selectiveDepth'],
        'uci_elapsed_ms': summary['elapsed_ms'], 'adapter_wall_ms': summary['wall_ms'],
        'nodes': outcome['nodes'], 'same_query_values': receipt_values,
        'whole_stage_query_equality': True, 'exact_physical_price_ledger_equality': True})

uci = sum(row['uci_elapsed_ms'] for row in checked)
wall = sum(row['adapter_wall_ms'] for row in checked)
nodes = sum(row['nodes'] for row in checked)
assert uci == audit['cost']['sum_uci_elapsed_ms'] == 137507
assert wall == audit['cost']['sum_adapter_query_wall_ms'] == 137628
assert nodes == audit['cost']['total_nodes'] == 286469730
assert sum(m['session_wall_ms'] for m in manifests) == audit['cost']['sum_session_wall_ms'] == 979037
calendar = round((datetime.datetime.fromisoformat(manifests[-1]['finished_utc']) -
                  datetime.datetime.fromisoformat(manifests[0]['started_utc'])).total_seconds() * 1000)
assert calendar == audit['cost']['total_calendar_study_wall_ms'] == 1264101
result = {'sealed_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
    'kind': 'Independent read-only validation and scope review of existing thirteen depth18 queries',
    'source_audit_sha256': sha(SOURCE), 'source_receipts_sha256': audit['study_sources_sha256'],
    'plans_sha256': {str(p.relative_to(ROOT)): sha(p) for p in plans},
    'new_engine_queries': 0, 'new_product_reports': 0, 'existing_queries_verified': 13,
    'all_request_and_whole_stage_work_objects_equal': True,
    'actual_profile_per_query_verified': 'Threads2 Hash16 and current MultiPV tracked separately in each sequential process; ClearHash ucinewgame each query, nominal requested/completed18',
    'complete_legal_root_inventories_and_all_returned_pvs_independently_replayed': True,
    'all_original_actor_victim_capture_check_promotion_ledgers_equal': True,
    'epoch_note': 'Both sequential processes retain the sealed study epoch UUID. Each query clears hash and starts ucinewgame; this is not one uninterrupted engine process.',
    'cost': {'uci_ms': uci, 'adapter_query_ms': wall, 'session_ms': 979037,
        'calendar_ms': calendar, 'nodes': nodes, 'nested_do_not_sum': True},
    'scope_findings': [
        'The root Ke3/g4 named receipt supports its finite local ordering only; it is not a global root-best proof or a reason for the untouched product grade.',
        'Ke3 f6: Kf3/g4 455/0 belongs to the named pair. The separate full pool 446/0 must remain distinct. The winning-valued Kf3 line itself later uses g4, refuting permanent preservation as a necessary explanation.',
        'Ke3 Kf6: pool Kf4/g4 556/551 and pair 558/483 show another positive-valued g4 plan. Their margins must not be mixed, and positive cp is not a solved win or unique necessity.',
        'Ke3 Ke6 Kd2 Ke5: the current own pair cxb4/g4 463/-898 supports a queenside task at this exact conditional position. The earlier full pool 538/517/182 is a separate measurement.',
        'After cxb4, exact lines can lose original Pe4 and still retain favourable bounded values. Their whole physical ledgers include subsequent captures/checks/promotions; e4 survival alone cannot be the necessary function.',
        'After g4, the same named Black receipt gives bxa3 -790, Kxe4 +627, Kf4 +689 in White cp. Among those tested replies bxa3 is strongest; a primary e4-capture or f4-control explanation is refuted at that stage.',
        'Returned unrestricted MultiPV3 is not an all-legal-reply value inventory. Same-query ranks are owned; missing replies and root causation remain unknown.',
        'Nominal18 is enforced in all intermediate received depth fields; selective extensions up to98 and longer PVs do not establish an exhaustive eighteen-ply solution.'
    ],
    'automatic_Basic_full_success': 0, 'fixed_old_full': '0/4 unchanged',
    'fixed_source_full': '0/8 unchanged', 'criteria_modified': False,
    'query_checks': checked}
OUT.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
print(json.dumps({'path': str(OUT), 'sha256': sha(OUT), 'queries': len(checked),
                  'uci_ms': uci, 'calendar_ms': calendar, 'new_engine_queries': 0}))
