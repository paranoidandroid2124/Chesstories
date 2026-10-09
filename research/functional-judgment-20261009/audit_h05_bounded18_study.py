"""Audit actual finite depth18 study; never submit product reports or alter input evidence."""
from __future__ import annotations
import hashlib
import json
import argparse
import gzip
from datetime import datetime, timezone
from pathlib import Path
import chess

ROOT = Path(__file__).resolve().parents[2]
STUDY = ROOT / 'target/functional-judgment-20261009/h05-response-plan-bounded18-ui21b'
OUT = ROOT / 'research/functional-judgment-20261009/h05-bounded18-response-function-and-price-audit-v2.json'
ARCHIVE = ROOT / 'research/functional-judgment-20261009/bounded18-study-evidence'
USE_ARCHIVE = False

def raw_bytes(path):
    if USE_ARCHIVE and path.is_relative_to(STUDY):
        relative = path.relative_to(STUDY)
        compressed = (ARCHIVE / relative.with_suffix('.json.gz')).read_bytes()
        original = gzip.decompress(compressed)
        expected = json.loads((ARCHIVE/'archive-manifest.json').read_text(encoding='utf-8'))['files'][str(relative).replace('\\', '/')]
        assert hashlib.sha256(compressed).hexdigest() == expected['compressed_sha256']
        assert hashlib.sha256(original).hexdigest() == expected['raw_sha256']
        return original
    return path.read_bytes()

def load(path):
    return json.loads(raw_bytes(path))

def digest(path):
    return hashlib.sha256(raw_bytes(path)).hexdigest()

def ledger(fen, prefix, local):
    board = chess.Board(fen)
    originals = {square: {'origin_square': chess.square_name(square),
        'color': 'white' if piece.color else 'black', 'original_role': piece.symbol().lower()}
        for square, piece in board.piece_map().items()}
    events, moves, positions = [], [], [board.fen()]
    for offset, uci in enumerate(prefix + local):
        move = chess.Move.from_uci(uci)
        assert move in board.legal_moves, (offset, uci, board.fen())
        san = board.san(move)
        piece = board.piece_at(move.from_square)
        original = originals.pop(move.from_square)
        victim_square = move.to_square
        if board.is_en_passant(move):
            victim_square += -8 if piece.color else 8
        victim = board.piece_at(victim_square)
        original_victim = originals.pop(victim_square, None)
        if victim or move.promotion:
            events.append({'ply_offset_zero_based': offset, 'uci': uci, 'san': san,
                'original_actor': original, 'original_victim': original_victim,
                'captured_current_role': victim.symbol().lower() if victim else None,
                'promotion': chess.piece_symbol(move.promotion) if move.promotion else None})
        # All preserved H05 paths contain kings/pawns only and no castling.
        assert not board.is_castling(move)
        originals[move.to_square] = original
        board.push(move)
        moves.append(san)
        positions.append(board.fen())
        if events and events[-1]['ply_offset_zero_based'] == offset:
            events[-1]['gives_check'] = board.is_check()
            events[-1]['fen_after'] = board.fen()
    return {'whole_san': moves, 'whole_moves_uci': prefix + local,
        'prefix_length': len(prefix), 'literal_capture_and_promotion_events': events,
        'final_fen': board.fen(), 'legally_replayed': True,
        'terminal_status': 'checkmate' if board.is_checkmate() else 'stalemate' if board.is_stalemate() else 'nonterminal',
        'final_original_pieces': {chess.square_name(sq): value for sq, value in sorted(originals.items())}}

def main():
    global USE_ARCHIVE, OUT
    parser = argparse.ArgumentParser()
    parser.add_argument('--archive', action='store_true')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    USE_ARCHIVE = args.archive
    if args.output is not None:
        OUT = args.output.resolve()
    all_rows, all_summary, manifests, sources = [], [], [], {}
    for name in ['execution', 'execution-phase2']:
        where = STUDY / name
        rows, summary, manifest = [load(where / f) for f in ['results.json', 'summary.json', 'manifest.json']]
        assert manifest['frozen_runtime_hashes_passed_before_and_after']
        assert manifest['shutdown']['exit_code'] == 0 and not manifest['shutdown']['adapter_forced_kill']
        assert manifest['product_reports_submitted'] == 0
        assert len(rows) == len(summary) == manifest['new_engine_queries']
        for file in ['results.json', 'summary.json', 'manifest.json']:
            sources[str((where / file).relative_to(ROOT)).replace('\\', '/')] = digest(where / file)
        all_rows.extend(rows)
        all_summary.extend(summary)
        manifests.append(manifest)
    assert len(all_rows) == 13
    readings, selective_depths, evaluation_depths = [], [], []
    for row, summary in zip(all_rows, all_summary):
        query, work = row['query'], row['query']['work']
        assert row['id'] == summary['id'] == work['workId']
        assert work['searchLimits']['depth'] == summary['completed_depth'] == 18
        assert summary['kind'] == 'completed' and summary['pv_legal']
        assert summary['execution_key'] == work['executionKeySha256']
        physical = row['result']['execution']
        outcome = physical['outcome']
        assert outcome['kind'] == 'completed' and outcome['completedDepth'] == 18
        assert len(outcome['lineSuffixes']) == len(summary['values'])
        for actual, exported in zip(outcome['lineSuffixes'], summary['values']):
            assert actual['depth'] == exported['depth'] == 18
            assert actual['moves'] == exported['uci'] and actual['whiteScore'] == exported['white_score']
        assert outcome['bestmoveUci'] == summary['values'][0]['root_uci']
        selective_depths.append(outcome['selectiveDepth'])
        evaluation_depths.extend(value['depth'] for value in physical['evaluations'])
        assert all(value['depth'] <= 18 for value in physical['evaluations'])
        prefix = query['relative_prefix_uci']
        assert prefix == work['enginePositionMovesUci']
        board = chess.Board(work['enginePositionInitialFen'])
        for move in prefix:
            board.push_uci(move)
        assert board.fen() == work['searchFen']
        readings.append({'id': row['id'], 'execution_key': work['executionKeySha256'],
            'request': query['request'], 'work': work,
            'stage_sha256': row['stage_sha256'], 'scope': query['scope'],
            'values': [dict(value, physical_ledger=ledger(work['enginePositionInitialFen'], prefix, value['uci']))
                for value in summary['values']]})
    start = datetime.fromisoformat(manifests[0]['started_utc'])
    end = datetime.fromisoformat(manifests[-1]['finished_utc'])
    result = {'schema_version': 1, 'sealed_utc': datetime.now(timezone.utc).isoformat(),
        'kind': 'Actual standalone depth18 response/choice study with original physical-piece prices',
        'audit_input_mode': 'exact portable archive' if USE_ARCHIVE else 'original completed study files',
        'study_sources_sha256': sources,
        'maximum_requested_and_completed_depth': 18, 'new_queries': 13, 'completed_queries': 13,
        'physical_receipt_and_exported_value_identity_checked': True,
        'maximum_intermediate_completed_depth': max(evaluation_depths),
        'maximum_selective_depth': max(selective_depths),
        'depth_scope': 'The cap is the requested/completed nominal engine depth. Engine selective extensions and returned PV lengths may exceed18; they are not separately issued deeper comparisons.',
        'preserved_initial_audit_sha256': digest(ROOT / 'research/functional-judgment-20261009/h05-bounded18-response-function-and-price-audit.json'),
        'product_reports_submitted': 0, 'engine_processes': 2, 'processes_sequential': True,
        'one_persistent_engine_for_continuation_phases_2_to_5': True,
        'cost': {'total_calendar_study_wall_ms': round((end-start).total_seconds()*1000),
            'sum_session_wall_ms': sum(m['session_wall_ms'] for m in manifests),
            'sum_uci_elapsed_ms': sum(s['elapsed_ms'] for s in all_summary),
            'sum_adapter_query_wall_ms': sum(s['wall_ms'] for s in all_summary),
            'total_nodes': sum(s['nodes'] for s in all_summary),
            'timing_note': 'UCI and adapter times are nested in session and calendar time; do not sum them. Calendar includes reading/stage-sealing gaps. Initial EOF ended the first engine epoch; continuation used a second sequential process.'},
        'all_paths_legally_replayed': True,
        'comparison_rule': 'Compare scores only inside each actually completed same-query pair/pool. No cross-query arithmetic, no original-grade graft, no historical24 relabeling, and no global root-best certificate from a named pair.',
        'human_conclusions': [
            {'finding': 'Root named18 Ke3 is above g4; after g4, the returned effective reply is f6.',
             'limit': 'Finite search evidence, not a solved endgame theorem or exhaustive root ranking.'},
            {'finding': 'After Ke3 f6, current unrestricted18 and own named18 both choose Kf3 above immediate g4. The direct pair is +455 versus0 Whitecp.',
             'function': 'Maintain useful king manoeuvring and defer spending the g-pawn advance under this blockade. The named continuation still spends g4 later; permanent pawn preservation is unnecessary.'},
            {'finding': 'After Ke3 Kf6, g4 remains positively valued: full pool +556/+551 Kf4/g4; separate direct pair +558/+483.',
             'function': 'This reply permits another adequate plan. Do not claim g4 must always be deferred, or treat a five-centipawn pool difference as the stable direct-pair margin.'},
            {'finding': 'At Ke3 Ke6 Kd2 Ke5, the current White pool selects cxb4 and its direct pair is +463 versus−898 for g4.',
             'function': 'Queenside preparation/conversion changes the needed task; simply preserving e4 or seeking f4 is not the main invariant.'},
            {'finding': 'After that cxb4, the current Black pool returns cxb4/Kxe4/c3; direct cxb4 versus Kxe4 is +613 versus+709 Whitecp.',
             'function': 'The e4 pawn can be lost within a still favourable tested conversion. Its first recorded loss and subsequent capture/check/promotion prices are retained per exact path.'},
            {'finding': 'After g4 at that same prepared stage, Black bxa3 is far stronger than Kxe4 or Kf4 in the named18 report:−790/+627/+689 Whitecp.',
             'function': 'The strongest actual tested counter is a queenside break. Treating e4 capture or f4 denial as the principal explanation is refuted here.'}
        ],
        'automatic_delivery_status': 'These independent research comparisons do not complete or replace the current automatic BASIC answer. The exact current10r H05 replay stopped with its bounded witnessed manoeuvre only.',
        'full_old_cases_passed': 0, 'full_fixed_source_cases_passed': 0,
        'criteria_unchanged': True, 'known_H05_extension_not_new_holdout': True,
        'query_readings': readings}
    assert not OUT.exists(), 'Preserve prior audit; choose a new epoch instead of replacing it'
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(result, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
    print(json.dumps({'output': str(OUT), 'sha256': digest(OUT), 'queries': 13, 'cost': result['cost']}))

if __name__ == '__main__':
    main()
