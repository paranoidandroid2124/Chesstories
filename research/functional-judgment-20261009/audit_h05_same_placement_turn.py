"""Static exact-history observation from two already completed18 forecasts; no engine calls."""
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
import chess

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT/'research/functional-judgment-20261009/h05-bounded18-response-function-and-price-audit-v2.json'
OUT = ROOT/'research/functional-judgment-20261009/h05-same-placement-different-turn-function-audit.json'
audit = json.loads(SOURCE.read_text(encoding='utf-8'))
rows = {row['id']:row for row in audit['query_readings']}
base = 'H05-response-plan-bounded18-ui21-20261009-'
good = rows[base+'root-Ke3-e5f6-g4-current-other-pair18']
bad = rows[base+'prefix4-g4-Black-named18']
line = next(value for value in good['values'] if value['root_uci']=='g3g4')
good_prefix = good['work']['enginePositionMovesUci']+line['uci'][:4]
bad_prefix = bad['work']['enginePositionMovesUci']
assert good_prefix == ['f3e3','e5f6','g3g4','f6e6','e3d2','e6e5']
assert bad_prefix == ['f3e3','e5e6','e3d2','e6e5','g3g4']
initial = good['work']['enginePositionInitialFen']
assert initial == bad['work']['enginePositionInitialFen']

def replay(prefix):
    board = chess.Board(initial)
    san=[]
    for uci in prefix:
        move=chess.Move.from_uci(uci)
        assert move in board.legal_moves
        san.append(board.san(move))
        board.push(move)
    return board, {'prefix_uci':prefix,'san':san,'fen':board.fen(),
        'side_to_move':'white' if board.turn else 'black',
        'legal_moves_uci':sorted(move.uci() for move in board.legal_moves)}

good_board,good_state = replay(good_prefix)
bad_board,bad_state = replay(bad_prefix)
assert good_board.board_fen()==bad_board.board_fen()
assert good_board.turn==chess.WHITE and bad_board.turn==chess.BLACK
assert chess.Move.from_uci('c3b4') in good_board.legal_moves
assert chess.Move.from_uci('b4a3') in bad_board.legal_moves
assert line['uci'][4]=='c3b4'
assert bad['values'][0]['root_uci']=='b4a3'
result = {'sealed_utc':datetime.now(timezone.utc).isoformat(),
    'kind':'Same physical placement, different exact history and turn; static observation from existing18 receipts',
    'source_sha256':hashlib.sha256(SOURCE.read_bytes()).hexdigest(),
    'good_source_query':good['id'],'good_source_key':good['execution_key'],
    'bad_source_query':bad['id'],'bad_source_key':bad['execution_key'],
    'earlier_g4_after_Kf6':good_state,'later_g4_after_preparation':bad_state,
    'physical_piece_placement_equal':True,'side_to_move_equal':False,
    'whole_history_equal':False,'full_fen_equal':False,
    'function': 'The timing of the useful queenside exchange matters. With White to move, the recorded adequate g4 line continues cxb4 before Black can make the observed bxa3 counter. At the same piece placement with Black to move, bxa3 is the first returned and directly compared strong counter. Keeping the original pawn on g3 permanently is not necessary for every favourable line.',
    'ownership_consequence':'Canonical turn/full FEN, entire history, occurrence and current work must remain distinct when preserving an idea across changed forecasts. Mixed default/explicit anchor normalization must not turn these two boards into one numerical scope.',
    'claim_boundary':'The good cxb4 is a legal witnessed continuation in the earlier owned g4 comparison. No fresh best-choice certificate exists at this six-ply later White position; +483 belongs to the earlier g4 pair. The bad bxa3 value belongs to its own exact Black position. No arithmetic combines these values or attributes the whole root difference to this observation.',
    'criteria_changed':False,'new_engine_queries':0,'product_reports_submitted':0,
    'full_automatic_basic_success_increment':0}
assert not OUT.exists()
OUT.write_text(json.dumps(result,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
print(json.dumps({'path':str(OUT),'sha256':hashlib.sha256(OUT.read_bytes()).hexdigest(),
    'same_placement':True,'different_turn':True,'new_engine_queries':0}))
