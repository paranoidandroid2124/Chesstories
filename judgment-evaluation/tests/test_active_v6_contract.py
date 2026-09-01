from __future__ import annotations

import base64
import copy
import io
import json
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from chesstory_eval.cause_audit import _checked_runtime_response
from chesstory_eval.hashing import sha256_bytes
from chesstory_eval.model import ContractError, IntegrityError
from chesstory_eval.runtime_jsonl_session import (
    _RuntimeExchange,
    _RuntimeJsonlSession,
    _verified_response_jsonl_bytes,
)
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
PASSED_PAWN_FEN = "7k/8/5KB1/8/8/8/P7/8 w - - 0 1"
PASSED_PAWN_AFTER_A3 = "7k/8/5KB1/8/8/P7/8/8 b - - 0 1"


# Schema-only wire sample. Exact private ancestry and owner identities are
# certified by the Scala assembler-to-serializer tests, not these placeholders.
def _passed_pawn_progress_realized_after_only_legal_reply_proof() -> dict[str, object]:
    analysis_continuation_branch_id = "1" * 64
    played_line = {
        "line_id": "line.played.passed-pawn-result",
        "line_role": "played",
        "line_rank": 2,
        "root_move": "a2a3",
    }
    after_reply = "6k1/8/5KB1/8/8/P7/8/8 w - - 1 2"
    after_result = "6k1/8/5KB1/8/P7/8/8/8 b - - 0 2"
    root_key = f"1:a2a3:{PASSED_PAWN_FEN}:{PASSED_PAWN_AFTER_A3}"
    reply_key = f"2:h8g8:{PASSED_PAWN_AFTER_A3}:{after_reply}"
    result_key = f"3:a3a4:{after_reply}:{after_result}"
    dependency_key = "passed-pawn-result-dependency"

    def object_state_dependency_proof(
        issuer_evidence_id: str,
        issuer_occurrence_id: str,
        issuer_line: dict[str, object],
        scope: str,
    ) -> dict[str, object]:
        return {
            "dependency_kind": "object_state_precondition",
            "proof_kind": "object_state",
            "squares": [
                {"role": "root_from", "square": "a2"},
                {"role": "root_to", "square": "a3"},
                {"role": "future_from", "square": "a3"},
                {"role": "future_to", "square": "a4"},
            ],
            "pieces": [
                {"role": "root_before", "side": "white", "piece": "pawn"},
                {"role": "tracked", "side": "white", "piece": "pawn"},
                {"role": "future_after", "side": "white", "piece": "pawn"},
            ],
            "relation_issuers": [],
            "position_state_issuers": [
                {
                    "state": {
                        "kind": "occupied_by",
                        "side": "white",
                        "square": "a3",
                        "piece": "pawn",
                    },
                    "semantic_proof_id": "3" * 64,
                    "issuer_evidence_id": issuer_evidence_id,
                    "issuer_occurrence_id": issuer_occurrence_id,
                    "step_key": reply_key,
                    "ply": 2,
                    "move_uci": "h8g8",
                    "fen_before": PASSED_PAWN_AFTER_A3,
                    "fen_after": after_reply,
                    "line": issuer_line,
                    "scope": scope,
                }
            ],
        }

    root_step = {
        "step_index": 0,
        "step_key": root_key,
        "ply": 1,
        "move_uci": "a2a3",
        "fen_before": PASSED_PAWN_FEN,
        "fen_after": PASSED_PAWN_AFTER_A3,
        "line": played_line,
        "provenance": "observed_game_move",
    }
    result_step = {
        "step_index": 2,
        "step_key": result_key,
        "ply": 3,
        "move_uci": "a3a4",
        "fen_before": after_reply,
        "fen_after": after_result,
        "line": played_line,
        "provenance": "certified_analysis_move",
    }
    reply_step = {
        "step_index": 1,
        "step_key": reply_key,
        "ply": 2,
        "move_uci": "h8g8",
        "fen_before": PASSED_PAWN_AFTER_A3,
        "fen_after": after_reply,
        "line": played_line,
        "provenance": "certified_analysis_move",
    }
    return {
        "source_evidence_id": "passed-pawn-result-proof-evidence",
        "event_evidence_id": "passed-pawn-result-event-evidence",
        "semantic_id": "a" * 64,
        "occurrence_id": "b" * 64,
        "dependency_fingerprint": "c" * 64,
        "result_target_subjects": [
            "20:passed-pawn-advanced5:white2:a32:a41:4:relations:[established:pawn_passage:"
            + "6" * 64
            + ",removed:pawn_passage:"
            + "7" * 64
            + "]:derived:[]"
        ],
        "root_actor": {
            "side": "white",
            "piece_before": "pawn",
            "piece_after": "pawn",
            "from": "a2",
            "to": "a3",
            "legal_move_relation": "d" * 64,
        },
        "realizing_actor": {
            "side": "white",
            "piece_before": "pawn",
            "piece_after": "pawn",
            "from": "a3",
            "to": "a4",
            "legal_move_relation": "e" * 64,
        },
        "root_line": played_line,
        "root_move": "a2a3",
        "root_ply": 1,
        "realizing_move": "a3a4",
        "realizing_ply": 3,
        "result_ply_offset": 2,
        "closed_legal_reply_inventory": {
            "issuer_evidence_id": "structural-delta-evidence",
            "root_after": {"fen": PASSED_PAWN_AFTER_A3, "ply": 1, "scope": "played_transition"},
            "legal_reply_move": "h8g8",
            "analysis_continuation_branch_id": analysis_continuation_branch_id,
        },
        "branches": [
            {
                "branch_id": analysis_continuation_branch_id,
                "role": "played_root_analysis_continuation",
                "reply_move": "h8g8",
                "source_occurrence_id": "0" * 64,
                "line": played_line,
                "root_provenance": "observed_game_root",
                "steps": [root_step, reply_step, result_step],
            },
        ],
        "proof_paths": [
            {
                "path_occurrence_id": "8" * 64,
                "analysis_continuation_branch_id": analysis_continuation_branch_id,
                "realization_actor": {
                    "side": "white",
                    "piece_before": "pawn",
                    "piece_after": "pawn",
                    "from": "a3",
                    "to": "a4",
                    "legal_move_relation": "e" * 64,
                },
                "realization_move": "a3a4",
                "realization_ply": 3,
                "premises": [
                    {
                        "role": "dependency",
                        "lower_kind": "passed_pawn_progress_dependency",
                        "lower_semantic_key": dependency_key,
                        "source_premise_ids": sorted([
                            "passed-pawn-result-event-evidence",
                            "object-state-owner.analysis",
                            "4" * 64,
                        ]),
                        "branch_id": analysis_continuation_branch_id,
                        "branch_role": "played_root_analysis_continuation",
                        "from_step_index": 0,
                        "to_step_index": 2,
                        "dependency_proof": object_state_dependency_proof(
                            "object-state-owner.analysis",
                            "4" * 64,
                            played_line,
                            "played_line",
                        ),
                    },
                    {
                        "role": "result",
                        "lower_kind": "passed_pawn_progress",
                        "lower_semantic_key": "analysis-result",
                        "source_premise_ids": ["passed-pawn-result-event-evidence"],
                        "branch_id": analysis_continuation_branch_id,
                        "branch_role": "played_root_analysis_continuation",
                        "from_step_index": 2,
                        "to_step_index": 2,
                    },
                ],
                "closure_use_ids": ["9" * 64],
            }
        ],
        "lower_premise_ids": sorted([
            "4" * 64,
            "object-state-owner.analysis",
            "passed-pawn-result-event-evidence",
            "structural-delta-evidence",
        ]),
    }


def _line_access_passed_pawn_progress_realized_after_only_legal_reply_proof() -> dict[str, object]:
    """Exact wire projection for axb3 ... Rxh1, Rxa4.

    The root capture vacates a2, the certified reply preserves the open a-file,
    and the later rook capture removes the adjacent-file pawn.  The fixture
    carries only the already-issued relation and position-state authorities;
    it does not infer either fact from an evaluation.
    """

    proof = _passed_pawn_progress_realized_after_only_legal_reply_proof()
    analysis_continuation_branch_id = "1" * 64
    initial = "6kr/8/5K2/8/p7/1n6/P7/R6B w - - 0 1"
    after_root = "6kr/8/5K2/8/p7/1P6/8/R6B b - - 0 1"
    after_reply = "6k1/8/5K2/8/p7/1P6/8/R6r w - - 0 2"
    after_result = "6k1/8/5K2/8/R7/1P6/8/7r b - - 0 2"
    root_key = f"1:a2b3:{initial}:{after_root}"
    reply_key = f"2:h8h1:{after_root}:{after_reply}"
    result_key = f"3:a1a4:{after_reply}:{after_result}"
    played_line = {
        "line_id": "line.played.line-access-passed-pawn-result",
        "line_role": "played",
        "line_rank": 2,
        "root_move": "a2b3",
    }

    def step(
        *,
        index: int,
        ply: int,
        move: str,
        before: str,
        after: str,
        line: dict[str, object],
        provenance: str,
    ) -> dict[str, object]:
        value: dict[str, object] = {
            "step_index": index,
            "step_key": f"{ply}:{move}:{before}:{after}",
            "ply": ply,
            "move_uci": move,
            "fen_before": before,
            "fen_after": after,
            "line": line,
            "provenance": provenance,
        }
        return value

    continuation_steps = [
        step(
            index=0,
            ply=1,
            move="a2b3",
            before=initial,
            after=after_root,
            line=played_line,
            provenance="observed_game_move",
        ),
        step(
            index=1,
            ply=2,
            move="h8h1",
            before=after_root,
            after=after_reply,
            line=played_line,
            provenance="certified_analysis_move",
        ),
        step(
            index=2,
            ply=3,
            move="a1a4",
            before=after_reply,
            after=after_result,
            line=played_line,
            provenance="certified_analysis_move",
        ),
    ]
    def dependency_proof(
        *,
        line: dict[str, object],
        scope: str,
        owner: str,
        occurrence_id: str,
        lower_source: str,
        state_owner: str,
        state_occurrence_id: str,
    ) -> dict[str, object]:
        return {
            "dependency_kind": "line_access_precondition",
            "proof_kind": "line_access",
            "squares": [
                {"role": "vacated_gate", "square": "a2"},
                {"role": "enabled_from", "square": "a1"},
                {"role": "enabled_to", "square": "a4"},
            ],
            "pieces": [
                {"role": "enabled_piece", "side": "white", "piece": "rook"}
            ],
            "relation_issuers": [
                {
                    "contract": "slider_reach_delta",
                    "result_key": "slider_reach_delta:" + "1" * 64,
                    "occurrence_id": occurrence_id,
                    "step_key": root_key,
                    "source_premise_ids": [lower_source],
                    "issuer_evidence_id": owner,
                    "line": line,
                    "scope": scope,
                }
            ],
            "position_state_issuers": [
                {
                    "state": {
                        "kind": "slider_reach",
                        "side": "white",
                        "square": "a1",
                        "piece": "rook",
                        "file_step": 0,
                        "rank_step": 1,
                        "segment": [
                            {"square": "a2", "target": "empty", "occupant_piece": None},
                            {"square": "a3", "target": "empty", "occupant_piece": None},
                            {"square": "a4", "target": "enemy", "occupant_piece": "pawn"},
                        ],
                    },
                    "semantic_proof_id": "7" * 64,
                    "issuer_evidence_id": state_owner,
                    "issuer_occurrence_id": state_occurrence_id,
                    "step_key": reply_key,
                    "ply": 2,
                    "move_uci": "h8h1",
                    "fen_before": after_root,
                    "fen_after": after_reply,
                    "line": line,
                    "scope": scope,
                }
            ],
        }

    proof.update(
        result_target_subjects=[
            "19:passed-pawn-created5:white2:b3:relations:[established:pawn_passage:"
            + "6" * 64
            + "]:derived:[]"
        ],
        root_actor={
            "side": "white",
            "piece_before": "pawn",
            "piece_after": "pawn",
            "from": "a2",
            "to": "b3",
            "legal_move_relation": "d" * 64,
        },
        realizing_actor={
            "side": "white",
            "piece_before": "rook",
            "piece_after": "rook",
            "from": "a1",
            "to": "a4",
            "legal_move_relation": "e" * 64,
        },
        root_line=played_line,
        root_move="a2b3",
        realizing_move="a1a4",
    )
    proof["closed_legal_reply_inventory"]["root_after"] = {
        "fen": after_root,
        "ply": 1,
        "scope": "played_transition",
    }
    proof["closed_legal_reply_inventory"]["legal_reply_move"] = "h8h1"
    proof["closed_legal_reply_inventory"]["analysis_continuation_branch_id"] = (
        analysis_continuation_branch_id
    )
    proof["branches"] = [
        {
            "branch_id": analysis_continuation_branch_id,
            "role": "played_root_analysis_continuation",
            "reply_move": "h8h1",
            "source_occurrence_id": "0" * 64,
            "line": played_line,
            "root_provenance": "observed_game_root",
            "steps": continuation_steps,
        },
    ]
    path = proof["proof_paths"][0]
    path.update(
        analysis_continuation_branch_id=analysis_continuation_branch_id,
        realization_actor=copy.deepcopy(proof["realizing_actor"]),
        realization_move="a1a4",
    )
    dependency = path["premises"][0]
    analysis_proof = dependency_proof(
        line=played_line,
        scope="played_line",
        owner="line-access-relation-owner.analysis",
        occurrence_id="2" * 64,
        lower_source="l1-slider-reach.analysis",
        state_owner="line-access-state-owner.analysis",
        state_occurrence_id="3" * 64,
    )
    dependency["dependency_proof"] = analysis_proof
    dependency["source_premise_ids"] = sorted(
        [
            "passed-pawn-result-event-evidence",
            "line-access-relation-owner.analysis",
            "2" * 64,
            "l1-slider-reach.analysis",
            "line-access-state-owner.analysis",
            "3" * 64,
        ]
    )
    proof["lower_premise_ids"] = sorted(
        {
            *proof["lower_premise_ids"],
            *dependency["source_premise_ids"],
        }
    )
    return proof


def _vacated_gate_enables_unrecapturable_slider_capture_proof() -> dict[str, object]:
    root = "7k/q7/8/8/8/8/N7/R6K w - - 0 1"
    reference_moves = ["a2b4", "h8g8", "a1a7"]
    played_moves = ["h1h2", "h8g8"]
    reference_fens = [
        "7k/q7/8/8/1N6/8/8/R6K b - - 1 1",
        "6k1/q7/8/8/1N6/8/8/R6K w - - 2 2",
        "6k1/R7/8/8/1N6/8/8/7K b - - 0 2",
    ]
    played_fens = [
        "7k/q7/8/8/8/8/N6K/R7 b - - 1 1",
        "6k1/q7/8/8/8/8/N6K/R7 w - - 2 2",
    ]
    reference_id = "1" * 64
    played_id = "2" * 64

    def steps(moves: list[str], fens: list[str], observed: bool) -> list[dict[str, object]]:
        return [
            {
                "step_index": index,
                "provenance": (
                    "observed_game_move"
                    if observed and index == 0
                    else "certified_analysis_move"
                ),
                "ply": index + 1,
                "move_uci": move,
                "fen_before": root if index == 0 else fens[index - 1],
                "fen_after": fens[index],
            }
            for index, move in enumerate(moves)
        ]

    def closure(
        use_id: str,
        role: str,
        issuer: str,
        issuer_evidence_id: str,
        issuer_occurrence_id: str,
        query: str,
        branch_id: str,
        branch_role: str,
        step_index: int,
        fen: str,
        ply: int,
        scope: str,
    ) -> dict[str, object]:
        return {
            "use_id": use_id,
            "role": role,
            "semantic_proof_id": ("a" if branch_id == reference_id else "b") * 64,
            "issuer": issuer,
            "issuer_evidence_id": issuer_evidence_id,
            "issuer_occurrence_id": issuer_occurrence_id,
            "query": query,
            "branch_id": branch_id,
            "branch_role": branch_role,
            "after_step_index": step_index,
            "position": {"fen": fen, "ply": ply, "scope": scope},
        }

    reference_occurrence = "8" * 64
    played_occurrence = "9" * 64
    path = {
        "path_occurrence_id": "3" * 64,
        "premises": [
            {
                "role": "reference_root_slider_reach",
                "contract": "slider_reach_delta",
                "result_id": f"slider_reach_delta:{'4' * 64}",
                "issuer_evidence_id": "line.reference",
                "issuer_occurrence_id": "5" * 64,
                "source_premise_ids": sorted(["line.reference", "5" * 64, "lower.reach"]),
                "branch_id": reference_id,
                "branch_role": "counterfactual_reference",
                "step_index": 0,
            },
            {
                "role": "reference_exploit_capture",
                "contract": "capture_recapture_inventory",
                "result_id": f"capture_recapture_inventory:{'6' * 64}",
                "issuer_evidence_id": "line.reference",
                "issuer_occurrence_id": "7" * 64,
                "source_premise_ids": sorted(["line.reference", "7" * 64, "lower.capture"]),
                "branch_id": reference_id,
                "branch_role": "counterfactual_reference",
                "step_index": 2,
            },
        ],
        "closed_absence_uses": [
            closure("a" * 64, "reference_immediate_recapture_absent", "position_relation_extractor.closed_relation_inventory", "line.reference", reference_occurrence, "legal-capture:black:a7", reference_id, "counterfactual_reference", 2, reference_fens[2], 3, "best_line"),
            closure("b" * 64, "played_exploit_move_absent", "position_relation_extractor.closed_relation_inventory", "line.played", played_occurrence, "legal-move-from-to:white:a1:a7", played_id, "played_root_analysis_continuation", 1, played_fens[1], 2, "played_line"),
            closure("c" * 64, "played_replacement_capture_absent", "position_relation_extractor.closed_relation_inventory", "line.played", played_occurrence, "legal-capture:white:a7", played_id, "played_root_analysis_continuation", 1, played_fens[1], 2, "played_line"),
        ],
        "closed_state_uses": [
            closure("d" * 64, "reference_intervening_slider_reach", "position_relation_extractor.closed_position_state_inventory", "line.reference", "d" * 64, "slider-reach:white:rook@a1:north:open", reference_id, "counterfactual_reference", 1, reference_fens[1], 2, "best_line"),
            closure("e" * 64, "played_slider_occupied", "position_relation_extractor.closed_position_state_inventory", "line.played", played_occurrence, "occupied-by:white:rook@a1", played_id, "played_root_analysis_continuation", 1, played_fens[1], 2, "played_line"),
            closure("f" * 64, "played_target_occupied", "position_relation_extractor.closed_position_state_inventory", "line.played", played_occurrence, "occupied-by:black:queen@a7", played_id, "played_root_analysis_continuation", 1, played_fens[1], 2, "played_line"),
            closure("0" * 64, "played_gate_blocker_occupied", "position_relation_extractor.closed_position_state_inventory", "line.played", played_occurrence, "occupied-by:white:knight@a2", played_id, "played_root_analysis_continuation", 1, played_fens[1], 2, "played_line"),
            closure("1" * 63 + "0", "played_blocked_slider_reach", "position_relation_extractor.closed_position_state_inventory", "line.played", played_occurrence, "slider-reach:white:rook@a1:north:blocked-a2", played_id, "played_root_analysis_continuation", 1, played_fens[1], 2, "played_line"),
        ],
    }
    return {
        "source_evidence_id": "direct.source",
        "semantic_id": "4" * 64,
        "occurrence_id": "5" * 64,
        "dependency_fingerprint": "6" * 64,
        "counterfactual_reference_branch": {
            "branch_id": reference_id,
            "line_id": "line.reference",
            "line_role": "best_reference",
            "branch_role": "counterfactual_reference",
            "root_provenance": "counterfactual_analyzed_root",
            "line_rank": 1,
            "root_move": reference_moves[0],
            "steps": steps(reference_moves, reference_fens, False),
        },
        "played_root_branch": {
            "branch_id": played_id,
            "line_id": "line.played",
            "line_role": "played",
            "branch_role": "played_root_analysis_continuation",
            "root_provenance": "observed_game_root",
            "line_rank": 1,
            "root_move": played_moves[0],
            "steps": steps(played_moves, played_fens, True),
        },
        "proof_paths": [path],
        "participants": {
            "enabler": {"side": "white", "from": "a2", "to": "b4", "piece_before": "knight", "piece_after": "knight"},
            "slider": {"side": "white", "piece": "rook", "square": "a1"},
            "gate_blocker": {"side": "white", "piece": "knight", "square": "a2"},
            "exploit": {"side": "white", "from": "a1", "to": "a7", "piece_before": "rook", "piece_after": "rook"},
            "captured_target": {"side": "black", "piece": "queen", "square": "a7"},
        },
        "exploit_move": "a1a7",
    }


def _sole_recapturer_removal_before_target_capture_proof() -> dict[str, object]:
    root = "7k/4p3/5n2/3r2B1/8/8/2B5/K2Q2R1 w - - 0 1"
    reference_fens = [
        "7k/4p3/5B2/3r4/8/8/2B5/K2Q2R1 b - - 0 1",
        "7k/8/5p2/3r4/8/8/2B5/K2Q2R1 w - - 0 2",
        "7k/8/5p2/3Q4/8/8/2B5/K5R1 b - - 0 2",
    ]
    played_fens = [
        "7k/4p3/5n2/3Q2B1/8/8/2B5/K5R1 b - - 0 1",
        "7k/4p3/8/3n2B1/8/8/2B5/K5R1 w - - 0 2",
    ]
    reference_moves = ["g5f6", "e7f6", "d1d5"]
    played_moves = ["d1d5", "f6d5"]
    reference_id = "1" * 64
    played_id = "2" * 64

    def steps(moves: list[str], fens: list[str], observed: bool) -> list[dict[str, object]]:
        return [
            {
                "step_index": index,
                "provenance": (
                    "observed_game_move"
                    if observed and index == 0
                    else "certified_analysis_move"
                ),
                "ply": index + 1,
                "move_uci": move,
                "fen_before": root if index == 0 else fens[index - 1],
                "fen_after": fens[index],
            }
            for index, move in enumerate(moves)
        ]

    def premise(
        role: str,
        contract: str,
        branch_id: str,
        branch_role: str,
        step_index: int,
        suffix: str,
    ) -> dict[str, object]:
        issuer_evidence_id = (
            "line.reference"
            if branch_role == "counterfactual_reference"
            else "line.played"
        )
        issuer_occurrence_id = suffix * 64
        return {
            "role": role,
            "contract": contract,
            "result_id": f"{contract}:{suffix * 64}",
            "issuer_evidence_id": issuer_evidence_id,
            "issuer_occurrence_id": issuer_occurrence_id,
            "source_premise_ids": sorted(
                [f"source.{suffix}", issuer_evidence_id, issuer_occurrence_id]
            ),
            "branch_id": branch_id,
            "branch_role": branch_role,
            "step_index": step_index,
        }

    return {
        "source_evidence_id": "defense.source",
        "semantic_id": "a" * 64,
        "occurrence_id": "b" * 64,
        "dependency_fingerprint": "c" * 64,
        "counterfactual_reference_branch": {
            "branch_id": reference_id,
            "line_id": "line.reference",
            "line_role": "best_reference",
            "branch_role": "counterfactual_reference",
            "root_provenance": "counterfactual_analyzed_root",
            "line_rank": 1,
            "root_move": reference_moves[0],
            "steps": steps(reference_moves, reference_fens, False),
        },
        "played_root_branch": {
            "branch_id": played_id,
            "line_id": "line.played",
            "line_role": "played",
            "branch_role": "played_root_analysis_continuation",
            "root_provenance": "observed_game_root",
            "line_rank": 1,
            "root_move": played_moves[0],
            "steps": steps(played_moves, played_fens, True),
        },
        "proof_paths": [
            {
                "path_occurrence_id": "d" * 64,
                "premises": [
                    premise("reference_defender_removal", "capture_recapture_inventory", reference_id, "counterfactual_reference", 0, "0"),
                    premise("reference_later_exploit_inventory", "capture_recapture_inventory", reference_id, "counterfactual_reference", 2, "4"),
                    premise("played_immediate_exploit_inventory", "capture_recapture_inventory", played_id, "played_root_analysis_continuation", 0, "5"),
                ],
                "closed_absence_uses": [
                    {
                        "use_id": "6" * 64,
                        "role": "reference_replacement_recapture_absent",
                        "semantic_proof_id": "7" * 64,
                        "issuer": "position_relation_extractor.closed_relation_inventory",
                        "issuer_evidence_id": "line.reference",
                        "issuer_occurrence_id": "8" * 64,
                        "query": "legal-capture:black:d5",
                        "branch_id": reference_id,
                        "branch_role": "counterfactual_reference",
                        "after_step_index": 2,
                        "position": {"fen": reference_fens[2], "ply": 3, "scope": "best_line"},
                    }
                ],
            }
        ],
        "participants": {
            "remover": {"side": "white", "from": "g5", "to": "f6", "piece_before": "bishop", "piece_after": "bishop"},
            "removed_defender": {"side": "black", "piece": "knight", "square": "f6"},
            "removal_recapture": {"side": "black", "from": "e7", "to": "f6", "piece_before": "pawn", "piece_after": "pawn", "move_uci": "e7f6"},
            "later_exploit": {"side": "white", "from": "d1", "to": "d5", "piece_before": "queen", "piece_after": "queen"},
            "captured_target": {"side": "black", "piece": "rook", "square": "d5"},
            "played_sole_recapture": {"side": "black", "from": "f6", "to": "d5", "piece_before": "knight", "piece_after": "knight", "move_uci": "f6d5"},
        },
        "later_exploit_move": "d1d5",
        "played_sole_recapture_move": "f6d5",
    }


def _promoting_sole_recapturer_removal_before_target_capture_proof() -> dict[str, object]:
    proof = _sole_recapturer_removal_before_target_capture_proof()
    root = "k2q1r2/2P3Bn/8/8/8/8/8/K7 w - - 0 1"
    reference_fens = [
        "k2q1B2/2P4n/8/8/8/8/8/K7 b - - 0 1",
        "k2q1n2/2P5/8/8/8/8/8/K7 w - - 0 2",
        "k2Q1n2/8/8/8/8/8/8/K7 b - - 0 2",
    ]
    played_fens = [
        "k2Q1r2/6Bn/8/8/8/8/8/K7 b - - 0 1",
        "k2r4/6Bn/8/8/8/8/8/K7 w - - 0 2",
    ]
    reference_moves = ["g7f8", "h7f8", "c7d8q"]
    played_moves = ["c7d8q", "f8d8"]

    def steps(moves: list[str], fens: list[str], observed: bool) -> list[dict[str, object]]:
        return [
            {
                "step_index": index,
                "provenance": (
                    "observed_game_move"
                    if observed and index == 0
                    else "certified_analysis_move"
                ),
                "ply": index + 1,
                "move_uci": move,
                "fen_before": root if index == 0 else fens[index - 1],
                "fen_after": fens[index],
            }
            for index, move in enumerate(moves)
        ]

    reference = proof["counterfactual_reference_branch"]
    played = proof["played_root_branch"]
    reference["root_move"] = reference_moves[0]
    reference["steps"] = steps(reference_moves, reference_fens, False)
    played["root_move"] = played_moves[0]
    played["steps"] = steps(played_moves, played_fens, True)
    proof["participants"] = {
        "remover": {
            "side": "white",
            "from": "g7",
            "to": "f8",
            "piece_before": "bishop",
            "piece_after": "bishop",
        },
        "removed_defender": {"side": "black", "piece": "rook", "square": "f8"},
        "removal_recapture": {
            "side": "black",
            "from": "h7",
            "to": "f8",
            "piece_before": "knight",
            "piece_after": "knight",
            "move_uci": "h7f8",
        },
        "later_exploit": {
            "side": "white",
            "from": "c7",
            "to": "d8",
            "piece_before": "pawn",
            "piece_after": "queen",
        },
        "captured_target": {"side": "black", "piece": "queen", "square": "d8"},
        "played_sole_recapture": {
            "side": "black",
            "from": "f8",
            "to": "d8",
            "piece_before": "rook",
            "piece_after": "rook",
            "move_uci": "f8d8",
        },
    }
    proof["later_exploit_move"] = "c7d8q"
    proof["played_sole_recapture_move"] = "f8d8"
    absence = proof["proof_paths"][0]["closed_absence_uses"][0]
    absence["query"] = "legal-capture:black:d8"
    absence["position"] = {
        "fen": reference_fens[2],
        "ply": 3,
        "scope": "best_line",
    }
    return proof


class RuntimePublicResponseTransportTest(unittest.TestCase):
    @staticmethod
    def _exchange(http_status: int, body: dict[str, object]) -> _RuntimeExchange:
        return _RuntimeExchange(
            http_status=http_status,
            body=body,
            request_jsonl_base64="",
            request_jsonl_sha256="",
            request_jsonl_length=0,
            response_jsonl_base64="",
            response_jsonl_sha256="",
            response_jsonl_length=0,
        )

    def test_public_l2_schema_exposes_only_typed_proof_families(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        schema = registry.load(move_path)
        definitions = schema["$defs"]

        self.assertNotIn("standardCausalFacet", definitions)
        self.assertNotIn("genericCausalChannel", definitions)
        self.assertNotIn("engineWorkRequiredResponse", definitions)
        self.assertNotIn("probeRequest", definitions)
        self.assertEqual(
            schema["oneOf"],
            [
                {"$ref": "#/$defs/errorResponse"},
                {"$ref": "#/$defs/readyResponse"},
            ],
        )
        self.assertEqual(
            definitions["causalFacet"]["oneOf"],
            [
                {"$ref": "#/$defs/wrongMoveOrderCausalFacet"},
                {"$ref": "#/$defs/passedPawnProgressFacet"},
                {"$ref": "#/$defs/missedTacticalResourceCausalFacet"},
            ],
        )


    def test_public_causal_owner_ids_are_globally_unique(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")

        def facet(cause_id: str, channel_id: str) -> dict[str, object]:
            return {
                "cause_evidence_id": cause_id,
                "kind": "wrong_move_order",
                "exposure": "primary",
                "channels": [
                    {
                        "channel_id": channel_id,
                        "unique_check_reply_defender_displacement_before_capture_proof": {
                            "counterfactual_reference_branch": {"root_move": "b1c3"},
                            "played_root_branch": {"root_move": "e2e4"},
                        },
                    }
                ],
            }

        def errors_for(facets: list[dict[str, object]]) -> list[str]:
            commentary = {
                "primary": {
                    "kind": "move_verdict",
                    "reference_endpoint": {"moves": ["b1c3"]},
                    "played_endpoint": {"moves": ["e2e4"]},
                },
                "causal_explanations": facets,
            }
            errors: list[str] = []
            registry._validate_move_commentary_proof_transport(commentary, "$", errors)
            return errors

        self.assertTrue(errors_for([facet("cause", "channel-a"), facet("cause", "channel-b")]))
        self.assertTrue(errors_for([facet("cause-a", "channel"), facet("cause-b", "channel")]))

    def test_public_causal_exposure_is_fixed_by_cause_kind(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")

        for kind, exposure in (
            ("wrong_move_order", "complementary"),
            ("missed_tactical_resource", "complementary"),
            ("passed_pawn_progress", "primary"),
        ):
            commentary = {
                "primary": {},
                "causal_explanations": [
                    {
                        "cause_evidence_id": f"cause-{kind}",
                        "kind": kind,
                        "exposure": exposure,
                        "channels": [{"channel_id": f"channel-{kind}"}],
                    }
                ],
            }
            errors: list[str] = []
            registry._validate_move_commentary_proof_transport(commentary, "$", errors)
            with self.subTest(kind=kind, exposure=exposure):
                self.assertTrue(any(".exposure:" in error for error in errors))

    def test_sole_recapturer_removal_before_target_capture_requires_its_exact_three_premise_manifest(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        definitions = registry.load(move_path)["$defs"]
        proof_schema = definitions["soleRecapturerRemovalBeforeTargetCaptureProof"]
        proof = _sole_recapturer_removal_before_target_capture_proof()

        def validation_errors(candidate: dict[str, object]) -> list[str]:
            errors: list[str] = []
            registry._validate(candidate, proof_schema, move_path, "$", errors)
            if not errors:
                registry._validate_sole_recapturer_removal_before_target_capture_proof_identifiers(
                    candidate, "$", errors
                )
            return errors

        self.assertEqual(validation_errors(proof), [])
        for premise in proof["proof_paths"][0]["premises"]:
            self.assertIn(premise["issuer_evidence_id"], premise["source_premise_ids"])
            self.assertIn(premise["issuer_occurrence_id"], premise["source_premise_ids"])

        resource_premise = {
            "role": "created_check_response",
            "contract": "created_check_response_inventory",
            "result_id": f"created_check_response_inventory:{'1' * 64}",
            "issuer_evidence_id": "line.reference",
            "issuer_occurrence_id": "2" * 64,
            "source_premise_ids": sorted(["line.reference", "2" * 64, "lower.check"]),
            "branch_id": "3" * 64,
            "branch_role": "counterfactual_reference",
            "step_index": 0,
        }
        resource_errors: list[str] = []
        registry._validate(
            resource_premise,
            definitions["uniqueCheckReplyDefenderDisplacementBeforeCapturePremiseUse"],
            move_path,
            "$",
            resource_errors,
        )
        self.assertEqual(resource_errors, [])

        missing_removal = copy.deepcopy(proof)
        missing_removal["proof_paths"][0]["premises"].pop(0)
        self.assertTrue(validation_errors(missing_removal))

        malformed_role = copy.deepcopy(proof)
        malformed_role["proof_paths"][0]["premises"][1]["role"] = "reference_defender_removal"
        self.assertTrue(validation_errors(malformed_role))

        wrong_premise_branch = copy.deepcopy(proof)
        wrong_premise_branch["proof_paths"][0]["premises"][1]["branch_id"] = "2" * 64
        self.assertTrue(validation_errors(wrong_premise_branch))

        wrong_absence_position = copy.deepcopy(proof)
        wrong_absence_position["proof_paths"][0]["closed_absence_uses"][0]["position"]["fen"] = FEN
        self.assertTrue(validation_errors(wrong_absence_position))

        noncanonical_sources = copy.deepcopy(proof)
        noncanonical_sources["proof_paths"][0]["premises"][0]["source_premise_ids"] = ["source.z", "source.a"]
        self.assertTrue(validation_errors(noncanonical_sources))

        missing_premise_owner = copy.deepcopy(proof)
        del missing_premise_owner["proof_paths"][0]["premises"][0]["issuer_evidence_id"]
        self.assertTrue(validation_errors(missing_premise_owner))

        unretained_premise_occurrence = copy.deepcopy(proof)
        unretained_premise_occurrence["proof_paths"][0]["premises"][0][
            "issuer_occurrence_id"
        ] = "f" * 64
        self.assertTrue(validation_errors(unretained_premise_occurrence))

        independent_paths = copy.deepcopy(proof)
        independent_path = copy.deepcopy(independent_paths["proof_paths"][0])
        independent_path["path_occurrence_id"] = "e" * 64
        independent_path["closed_absence_uses"][0]["use_id"] = "f" * 64
        independent_paths["proof_paths"].append(independent_path)
        self.assertEqual(validation_errors(independent_paths), [])

        noncanonical_paths = copy.deepcopy(proof)
        second_path = copy.deepcopy(noncanonical_paths["proof_paths"][0])
        second_path["path_occurrence_id"] = "e" * 64
        second_path["closed_absence_uses"][0]["use_id"] = "f" * 64
        noncanonical_paths["proof_paths"].append(second_path)
        noncanonical_paths["proof_paths"].reverse()
        self.assertTrue(validation_errors(noncanonical_paths))

        duplicate_absence = copy.deepcopy(proof)
        duplicate_path = copy.deepcopy(duplicate_absence["proof_paths"][0])
        duplicate_path["path_occurrence_id"] = "e" * 64
        duplicate_absence["proof_paths"].append(duplicate_path)
        self.assertTrue(validation_errors(duplicate_absence))

        defense_channel = {
            "channel_id": "defense.channel",
            "sole_recapturer_removal_before_target_capture_proof": proof,
        }
        channel_errors: list[str] = []
        registry._validate(
            defense_channel,
            definitions["wrongMoveOrderCausalChannel"],
            move_path,
            "$",
            channel_errors,
        )
        self.assertEqual(channel_errors, [])

        mixed_channel = copy.deepcopy(defense_channel)
        mixed_channel["unique_check_reply_defender_displacement_before_capture_proof"] = proof
        channel_errors = []
        registry._validate(
            mixed_channel,
            definitions["wrongMoveOrderCausalChannel"],
            move_path,
            "$",
            channel_errors,
        )
        self.assertTrue(channel_errors)

    def test_exact_raw_jsonl_outer_shape_schema_and_request_binding(self) -> None:
        class FakeProcess:
            def __init__(self) -> None:
                self.stdin = io.BytesIO()

            def poll(self) -> None:
                return None

        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "raw-public-response",
            "input": {},
        }
        body = {
            "schema_version": "chesstory.move-meaning.response.v6",
            "request_id": "raw-public-response",
            "status": "error",
            "error": "invalid_request",
        }
        response_line = (
            json.dumps(
                {"http_status": 400, "body": body},
                ensure_ascii=False,
                separators=(", ", ": "),
            )
            + "\r\n"
        ).encode("utf-8")
        session = _RuntimeJsonlSession(
            ["not-started"], cwd=ROOT / "runtime-adapter", timeout_seconds=1.0
        )
        process = FakeProcess()
        session._process = process  # type: ignore[assignment]
        session._output.put(("line", response_line))

        exchange = session.invoke(request)

        request_line = process.stdin.getvalue()
        self.assertEqual(
            base64.b64decode(exchange.request_jsonl_base64, validate=True),
            request_line,
        )
        self.assertEqual(exchange.request_jsonl_sha256, sha256_bytes(request_line))
        self.assertEqual(exchange.request_jsonl_length, len(request_line))
        self.assertEqual(
            _verified_response_jsonl_bytes(
                exchange.response_jsonl_base64, exchange.response_jsonl_sha256
            ),
            response_line,
        )
        self.assertEqual(exchange.response_jsonl_length, len(response_line))
        self.assertEqual((exchange.http_status, exchange.body), (400, body))

        unbound_body = {key: value for key, value in body.items() if key != "request_id"}
        for unbound_request in (
            {key: value for key, value in request.items() if key != "request_id"},
            {**request, "request_id": "not a valid request id"},
        ):
            with self.subTest(unbound_request=unbound_request):
                session._output.put(
                    (
                        "line",
                        json.dumps({"http_status": 400, "body": unbound_body}).encode(
                            "utf-8"
                        ),
                    )
                )
                self.assertEqual(session.invoke(unbound_request).body, unbound_body)

        self.assertEqual(_checked_runtime_response(exchange), "error")

        for invalid_body in (
            {**body, "request_id": "other-request"},
            {key: value for key, value in body.items() if key != "request_id"},
        ):
            with self.subTest(invalid_body=invalid_body), self.assertRaises(ContractError):
                session._output.put(
                    (
                        "line",
                        json.dumps({"http_status": 400, "body": invalid_body}).encode(
                            "utf-8"
                        ),
                    )
                )
                session.invoke(request)

        for invalid_outer in (
            {"http_status": 400},
            {"http_status": 400, "body": body, "extra": True},
            {
                "http_status": 400,
                "body": {
                    "schema_version": "chesstory.move-meaning.response.v6",
                    "request_id": "raw-public-response",
                    "status": "error",
                },
            },
            {
                "http_status": 400,
                "body": {
                    **body,
                    "unexpected": [],
                },
            },
        ):
            with self.subTest(invalid_outer=invalid_outer), self.assertRaises(ContractError):
                session._output.put(("line", json.dumps(invalid_outer).encode("utf-8")))
                session.invoke(request)

        with self.assertRaises(IntegrityError):
            _verified_response_jsonl_bytes(exchange.response_jsonl_base64, "f" * 64)

    def test_public_move_schema_accepts_legal_passed_pawn_progress_shape(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        reference_endpoint = {
            "kind": "engine_search",
            "moves": ["a2a4", "h8g8", "a4a5"],
            "win_percent_for_mover": 50,
            "depth": 16,
        }
        played_endpoint = {
            "kind": "engine_search",
            "moves": ["a2a3", "h8g8", "a3a4"],
            "win_percent_for_mover": 50,
            "depth": 16,
        }
        ready = {
            "schema_version": "chesstory.move-meaning.response.v6",
            "status": "ready",
            "move_commentary": {
                "primary": {
                    "kind": "move_verdict",
                    "comparison_evidence_id": "comparison-evidence",
                    "verdict_code": "blunder",
                    "verdict_confidence": "engine_backed",
                    "mover": "white",
                    "delta": {
                        "kind": "engine_evaluation",
                        "candidate_win_percent_delta_for_mover": -60,
                    },
                    "reference_endpoint": reference_endpoint,
                    "played_endpoint": played_endpoint,
                },
                "causal_explanations": [
                    {
                        "cause_evidence_id": "cause-evidence",
                        "kind": "passed_pawn_progress",
                        "exposure": "complementary",
                        "channels": [
                            {
                                "channel_id": "channel-1",
                                "passed_pawn_progress_realized_after_only_legal_reply_proof": _passed_pawn_progress_realized_after_only_legal_reply_proof(),
                            }
                        ],
                    }
                ],
            },
        }
        registry.validate_document(ready, move_path, label="public v6 legal passed-pawn-result schema shape")

        retired_wrapper = copy.deepcopy(ready)
        facet = retired_wrapper["move_commentary"]["causal_explanations"][0]
        retired_wrapper["move_commentary"]["causal_explanations"][0] = {
            "kind": "single_cause",
            "facets": [facet],
        }
        with self.assertRaises(ContractError):
            registry.validate_document(
                retired_wrapper,
                move_path,
                label="retired singleton causal wrapper",
            )

        for field, value in {
            "facet_role": "lead",
            "proof_confidence": "legal_replay_verified",
            "effect_mode": "played_value",
            "source_side": "candidate",
            "event_move": "a2a3",
            "comparison_kind": "played_vs_best",
        }.items():
            retired_duplicate = copy.deepcopy(ready)
            retired_duplicate["move_commentary"]["causal_explanations"][0][
                field
            ] = value
            with self.subTest(field=field), self.assertRaises(ContractError):
                registry.validate_document(
                    retired_duplicate,
                    move_path,
                    label="retired duplicate facet metadata",
                )

        for exposure in (None, "inferred_from_proof_count"):
            invalid_exposure = copy.deepcopy(ready)
            facet = invalid_exposure["move_commentary"]["causal_explanations"][0]
            if exposure is None:
                del facet["exposure"]
            else:
                facet["exposure"] = exposure
            with self.subTest(exposure=exposure), self.assertRaises(ContractError):
                registry.validate_document(
                    invalid_exposure,
                    move_path,
                    label="server-owned Cause exposure",
                )

        reference_ready = copy.deepcopy(ready)
        reference_primary = reference_ready["move_commentary"]["primary"]
        reference_primary["reference_endpoint"]["moves"][0] = "a2a3"
        reference_primary["played_endpoint"]["moves"][0] = "a2a4"
        reference_facet = reference_ready["move_commentary"]["causal_explanations"][0]
        reference_proof = reference_facet["channels"][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        reference_proof["root_line"]["line_role"] = "best_reference"
        reference_proof["closed_legal_reply_inventory"]["root_after"]["scope"] = "reference_transition"
        for branch in reference_proof["branches"]:
            branch["line"]["line_role"] = "best_reference"
            branch["root_provenance"] = "counterfactual_analyzed_root"
            for step in branch["steps"]:
                if step["step_index"] == 0:
                    step["line"]["line_role"] = "best_reference"
                step["provenance"] = "certified_analysis_move"
        for path in reference_proof["proof_paths"]:
            for premise in path["premises"]:
                dependency = premise.get("dependency_proof")
                if not dependency:
                    continue
                for collection in ("relation_issuers", "position_state_issuers"):
                    for issuer in dependency[collection]:
                        if issuer["line"]["line_id"] == "line.played.passed-pawn-result":
                            issuer["line"]["line_role"] = "best_reference"
                            issuer["scope"] = "best_line"
        with self.assertRaises(ContractError):
            registry.validate_document(
                reference_ready,
                move_path,
                label="reference alternative cannot claim a passed-pawn result",
            )

        defense_ready = copy.deepcopy(ready)
        defense_primary = defense_ready["move_commentary"]["primary"]
        defense_primary["reference_endpoint"]["moves"] = ["g5f6", "e7f6", "d1d5"]
        defense_primary["played_endpoint"]["moves"] = ["d1d5", "f6d5"]
        defense_facet = defense_ready["move_commentary"]["causal_explanations"][0]
        defense_facet.update(
            kind="wrong_move_order",
            exposure="primary",
        )
        defense_facet["channels"] = [
            {
                "channel_id": "channel-sole-recapturer-removal-before-target-capture",
                "sole_recapturer_removal_before_target_capture_proof": _sole_recapturer_removal_before_target_capture_proof(),
            }
        ]
        registry.validate_document(
            defense_ready,
            move_path,
            label="public v6 legal sole-recapturer-removal-before-target-capture schema shape",
        )
        direct_ready = copy.deepcopy(ready)
        direct_primary = direct_ready["move_commentary"]["primary"]
        direct_primary["reference_endpoint"]["moves"] = ["a2b4", "h8g8", "a1a7", "g8f8"]
        direct_primary["played_endpoint"]["moves"] = ["h1h2", "h8g8"]
        direct_facet = direct_ready["move_commentary"]["causal_explanations"][0]
        direct_facet.update(
            {
                "kind": "missed_tactical_resource",
                "exposure": "primary",
            }
        )
        direct_facet["channels"] = [
            {
                "channel_id": "channel-direct-line-access",
                "vacated_gate_enables_unrecapturable_slider_capture_proof": _vacated_gate_enables_unrecapturable_slider_capture_proof(),
            }
        ]
        registry.validate_document(
            direct_ready,
            move_path,
            label="public v6 exact direct-line-access preparation",
        )
        for label, document, exposure in (
            ("wrong move order", defense_ready, "complementary"),
            ("missed tactical resource", direct_ready, "complementary"),
            ("passed pawn progress", ready, "primary"),
        ):
            invalid = copy.deepcopy(document)
            invalid["move_commentary"]["causal_explanations"][0]["exposure"] = exposure
            with self.subTest(cause=label), self.assertRaises(ContractError):
                registry.validate_document(
                    invalid,
                    move_path,
                    label=f"invalid {label} exposure",
                )
        dangling_direct_branch = copy.deepcopy(direct_ready)
        dangling_direct_proof = dangling_direct_branch["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0]["vacated_gate_enables_unrecapturable_slider_capture_proof"]
        dangling_direct_proof["proof_paths"][0]["closed_absence_uses"][1][
            "branch_id"
        ] = "1" * 64
        with self.assertRaises(ContractError):
            registry.validate_document(
                dangling_direct_branch,
                move_path,
                label="direct proof use with a dangling branch",
            )
        generic_direct = copy.deepcopy(direct_ready)
        generic_direct["move_commentary"]["causal_explanations"][0]["channels"] = [
            {
                "channel_id": "channel-generic-missed-resource",
                "actor": {
                    "move_uci": "a2b4",
                    "side": "white",
                    "piece": "knight",
                    "from": "a2",
                    "to": "b4",
                },
                "targets": [],
                "mechanisms": [],
                "consequences": [],
                "witnesses": [],
                "proof_line_moves": ["a2b4"],
            }
        ]
        with self.assertRaises(ContractError):
            registry.validate_document(
                generic_direct,
                move_path,
                label="missed tactical resource cannot use a generic channel",
            )
        multiple_proof_paths = copy.deepcopy(ready)
        proof_paths = multiple_proof_paths["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"]
        independent_path = copy.deepcopy(proof_paths[0])
        independent_path["path_occurrence_id"] = "a" * 64
        proof_paths.append(independent_path)
        registry.validate_document(
            multiple_proof_paths,
            move_path,
            label="public v6 independent proof paths",
        )

        line_access_document = copy.deepcopy(ready)
        line_access_document["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"] = _line_access_passed_pawn_progress_realized_after_only_legal_reply_proof()
        line_access_document["move_commentary"]["primary"]["played_endpoint"]["moves"][0] = "a2b3"
        registry.validate_document(
            line_access_document,
            move_path,
            label="public v6 branch-consistent line-access dependency proof",
        )
        valid_dependency_documents = [line_access_document]

        invalid_documents = []
        passed_pawn_progress_without_typed_proof = copy.deepcopy(ready)
        del passed_pawn_progress_without_typed_proof["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        invalid_documents.append(passed_pawn_progress_without_typed_proof)

        passed_pawn_progress_with_generic_segment = copy.deepcopy(ready)
        passed_pawn_progress_with_generic_segment["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["proof_segment"] = {
            "terminal_relation": "realizes_passed_pawn_progress",
            "steps": [{"ply_offset": 0, "move_uci": "a2a3", "role": "root_action"}],
        }
        invalid_documents.append(passed_pawn_progress_with_generic_segment)

        typed_passed_pawn_progress_under_standard_kind = copy.deepcopy(ready)
        typed_passed_pawn_progress_under_standard_kind["move_commentary"]["causal_explanations"][0][
            "kind"
        ] = "material_swing"
        invalid_documents.append(typed_passed_pawn_progress_under_standard_kind)

        passed_pawn_progress_without_closed_reply_branch = copy.deepcopy(ready)
        del passed_pawn_progress_without_closed_reply_branch["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["branches"][0]
        invalid_documents.append(passed_pawn_progress_without_closed_reply_branch)

        passed_pawn_progress_with_second_analysis_branch = copy.deepcopy(ready)
        duplicate_proof = passed_pawn_progress_with_second_analysis_branch["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        duplicate_branch = copy.deepcopy(duplicate_proof["branches"][0])
        duplicate_branch.update(
            branch_id="3" * 64,
            reply_move="h8h7",
            source_occurrence_id="7" * 64,
        )
        duplicate_proof["branches"].append(duplicate_branch)
        invalid_documents.append(passed_pawn_progress_with_second_analysis_branch)

        passed_pawn_progress_with_unproved_inventory_reply = copy.deepcopy(ready)
        unproved_proof = passed_pawn_progress_with_unproved_inventory_reply["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        unproved_proof["closed_legal_reply_inventory"]["legal_reply_move"] = "h8h7"
        invalid_documents.append(passed_pawn_progress_with_unproved_inventory_reply)

        passed_pawn_progress_with_forged_observed_suffix = copy.deepcopy(ready)
        forged_suffix_proof = passed_pawn_progress_with_forged_observed_suffix[
            "move_commentary"
        ]["causal_explanations"][0]["channels"][0][
            "passed_pawn_progress_realized_after_only_legal_reply_proof"
        ]
        forged_suffix_proof["branches"][0]["steps"][1]["provenance"] = (
            "observed_game_move"
        )
        invalid_documents.append(passed_pawn_progress_with_forged_observed_suffix)

        passed_pawn_progress_with_reference_root_line = copy.deepcopy(ready)
        passed_pawn_progress_with_reference_root_line["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0][
            "passed_pawn_progress_realized_after_only_legal_reply_proof"
        ]["root_line"]["line_role"] = "best_reference"
        invalid_documents.append(passed_pawn_progress_with_reference_root_line)

        passed_pawn_progress_with_counterfactual_branch_root = copy.deepcopy(ready)
        passed_pawn_progress_with_counterfactual_branch_root["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0][
            "passed_pawn_progress_realized_after_only_legal_reply_proof"
        ]["branches"][0]["root_provenance"] = "counterfactual_analyzed_root"
        invalid_documents.append(passed_pawn_progress_with_counterfactual_branch_root)

        passed_pawn_progress_with_reference_reply_scope = copy.deepcopy(ready)
        passed_pawn_progress_with_reference_reply_scope["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0][
            "passed_pawn_progress_realized_after_only_legal_reply_proof"
        ]["closed_legal_reply_inventory"]["root_after"]["scope"] = (
            "reference_transition"
        )
        invalid_documents.append(passed_pawn_progress_with_reference_reply_scope)

        passed_pawn_progress_with_reference_state_scope = copy.deepcopy(ready)
        passed_pawn_progress_with_reference_state_scope["move_commentary"][
            "causal_explanations"
        ][0]["channels"][0][
            "passed_pawn_progress_realized_after_only_legal_reply_proof"
        ]["proof_paths"][0]["premises"][0]["dependency_proof"][
            "position_state_issuers"
        ][0]["scope"] = "best_line"
        invalid_documents.append(passed_pawn_progress_with_reference_state_scope)

        wrong_move_order_without_l2 = copy.deepcopy(ready)
        wrong_move_order_without_l2["move_commentary"]["causal_explanations"][0][
            "kind"
        ] = "wrong_move_order"
        invalid_documents.append(wrong_move_order_without_l2)

        retired_structural_units = copy.deepcopy(ready)
        retired_structural_units["move_commentary"]["structural_idea_units"] = []
        invalid_documents.append(retired_structural_units)

        retired_only_move_qualifiers = copy.deepcopy(ready)
        retired_only_move_qualifiers["move_commentary"]["causal_explanations"][0][
            "only_move_qualifiers"
        ] = []
        invalid_documents.append(retired_only_move_qualifiers)

        retired_responsibility_links = copy.deepcopy(ready)
        retired_responsibility_links["move_commentary"]["responsibility_links"] = []
        invalid_documents.append(retired_responsibility_links)

        legacy_plan_event = copy.deepcopy(ready)
        legacy_plan_event["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["proof_segment"] = {
            "terminal_relation": "realizes_passed_pawn_progress",
            "steps": [
                {
                    "ply_offset": 0,
                    "move_uci": "a2a3",
                    "role": "root_action",
                    "plan_event": {
                        "goal_kind": "passed_pawn_manufacture",
                        "actor_from": "a2",
                        "actor_to": "a3",
                        "targets": [],
                        "results": [],
                    },
                }
            ],
        }
        invalid_documents.append(legacy_plan_event)

        stale_cause = copy.deepcopy(ready)
        stale_cause["move_commentary"]["causal_explanations"][0][
            "kind"
        ] = "wrong_recapturer"
        invalid_documents.append(stale_cause)

        stale_tempo_loss = copy.deepcopy(ready)
        stale_tempo_loss["move_commentary"]["causal_explanations"][0][
            "kind"
        ] = "tempo_loss"
        invalid_documents.append(stale_tempo_loss)

        uncertified_plan_refutation = copy.deepcopy(ready)
        uncertified_plan_refutation["move_commentary"]["causal_explanations"][0][
            "kind"
        ] = "plan_contradiction"
        invalid_documents.append(uncertified_plan_refutation)

        stale_object = copy.deepcopy(ready)
        stale_object["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["targets"] = [{"kind": "motif", "key": "legacy"}]
        invalid_documents.append(stale_object)

        stale_terminal = copy.deepcopy(ready)
        stale_terminal["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["proof_segment"] = {
            "terminal_relation": "restricts_opponent_resource",
            "steps": [{"ply_offset": 0, "move_uci": "a2a3", "role": "root_action"}],
        }
        invalid_documents.append(stale_terminal)

        missing_reply_occurrence = copy.deepcopy(ready)
        del missing_reply_occurrence["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["branches"][0]["source_occurrence_id"]
        invalid_documents.append(missing_reply_occurrence)

        forged_reply_root_line = copy.deepcopy(ready)
        forged_proof = forged_reply_root_line["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        forged_branch = forged_proof["branches"][0]
        forged_line = {
            "line_id": "line.forged.shared-root",
            "line_role": "played",
            "line_rank": 99,
            "root_move": "a2a3",
        }
        forged_branch["line"] = copy.deepcopy(forged_line)
        forged_branch["steps"][0]["line"] = copy.deepcopy(forged_line)
        invalid_documents.append(forged_reply_root_line)

        incomplete_premise_manifest = copy.deepcopy(ready)
        premises = incomplete_premise_manifest["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        premises[1] = copy.deepcopy(premises[0])

        missing_dependency_proof = copy.deepcopy(ready)
        premises = missing_dependency_proof["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        del premises[0]["dependency_proof"]

        dependency_proof_on_result = copy.deepcopy(ready)
        premises = dependency_proof_on_result["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        premises[1]["dependency_proof"] = copy.deepcopy(premises[0]["dependency_proof"])
        premises[1]["source_premise_ids"] = copy.deepcopy(premises[0]["source_premise_ids"])
        premises[1]["from_step_index"] = premises[0]["from_step_index"]
        premises[1]["to_step_index"] = premises[0]["to_step_index"]

        mismatched_dependency_kind = copy.deepcopy(ready)
        premises = mismatched_dependency_kind["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        premises[0]["dependency_proof"]["proof_kind"] = "line_access"

        detached_result = copy.deepcopy(ready)
        premise = detached_result["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][1]
        premise["from_step_index"] = 0
        premise["to_step_index"] = 0

        duplicated_dependency = copy.deepcopy(ready)
        premises = duplicated_dependency["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        premises.insert(1, copy.deepcopy(premises[0]))
        for label, document in (
            ("transport does not require a producer-family premise role manifest", incomplete_premise_manifest),
            ("transport does not require a dependency witness from its role name", missing_dependency_proof),
            ("transport does not prohibit a typed witness from another premise role", dependency_proof_on_result),
            ("transport does not re-adjudicate dependency-kind and proof-kind pairing", mismatched_dependency_kind),
            ("transport preserves a supplied endpoint range without re-adjudicating its role", detached_result),
            ("transport preserves duplicate semantic premises as distinct supplied occurrences", duplicated_dependency),
        ):
            registry.validate_document(document, move_path, label=label)

        unrejudged_ray_geometry = copy.deepcopy(valid_dependency_documents[0])
        state = unrejudged_ray_geometry["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0][
            "dependency_proof"
        ]["position_state_issuers"][0]["state"]
        state["file_step"] = 1
        state["rank_step"] = 1
        state["segment"] = []
        registry.validate_document(
            unrejudged_ray_geometry,
            move_path,
            label="transport does not reconstruct sealed ray geometry or first-occupant closure",
        )

        discontinuous_occurrence = copy.deepcopy(ready)
        proof = discontinuous_occurrence["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        continuation_steps = proof["branches"][0]["steps"]
        continuation_steps[-1]["fen_before"] = continuation_steps[0]["fen_after"]
        continuation_steps[-1]["step_key"] = (
            f'{continuation_steps[-1]["ply"]}:{continuation_steps[-1]["move_uci"]}:'
            f'{continuation_steps[-1]["fen_before"]}:{continuation_steps[-1]["fen_after"]}'
        )
        invalid_documents.append(discontinuous_occurrence)

        skipped_ply = copy.deepcopy(ready)
        proof = skipped_ply["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        reply_step = proof["branches"][0]["steps"][1]
        reply_step["ply"] = 3
        reply_step["step_key"] = (
            f'{reply_step["ply"]}:{reply_step["move_uci"]}:'
            f'{reply_step["fen_before"]}:{reply_step["fen_after"]}'
        )
        invalid_documents.append(skipped_ply)

        detached_root_actor = copy.deepcopy(ready)
        proof = detached_root_actor["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        proof["root_actor"]["to"] = "b3"
        registry.validate_document(
            detached_root_actor,
            move_path,
            label="transport trusts the typed root actor issued by the Scala authority",
        )

        foreign_realizing_side = copy.deepcopy(ready)
        proof = foreign_realizing_side["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        proof["realizing_actor"]["side"] = "black"
        registry.validate_document(
            foreign_realizing_side,
            move_path,
            label="transport does not re-prove the realizing actor relationship",
        )

        illegal_reply_replay = copy.deepcopy(ready)
        proof = illegal_reply_replay["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        proof["branches"][0]["steps"][1]["move_uci"] = "h8h7"
        invalid_documents.append(illegal_reply_replay)

        forged_closure_root = copy.deepcopy(ready)
        proof = forged_closure_root["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        proof["closed_legal_reply_inventory"]["root_after"]["fen"] = FEN
        invalid_documents.append(forged_closure_root)

        detached_realization_ply = copy.deepcopy(ready)
        proof = detached_realization_ply["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        proof["proof_paths"][0]["realization_ply"] = 4
        invalid_documents.append(detached_realization_ply)

        mismatched_relation_issuer = copy.deepcopy(valid_dependency_documents[0])
        premises = mismatched_relation_issuer["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        premises[0]["dependency_proof"]["relation_issuers"][0][
            "contract"
        ] = "pawn_topology_transition"
        invalid_documents.append(mismatched_relation_issuer)

        unowned_relation_owner = copy.deepcopy(valid_dependency_documents[0])
        premise = unowned_relation_owner["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0]
        relation = premise["dependency_proof"]["relation_issuers"][0]
        premise["source_premise_ids"].remove(relation["issuer_evidence_id"])
        invalid_documents.append(unowned_relation_owner)

        unowned_relation_occurrence = copy.deepcopy(valid_dependency_documents[0])
        premise = unowned_relation_occurrence["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0]
        relation = premise["dependency_proof"]["relation_issuers"][0]
        premise["source_premise_ids"].remove(relation["occurrence_id"])
        invalid_documents.append(unowned_relation_occurrence)

        unowned_relation_lower_source = copy.deepcopy(valid_dependency_documents[0])
        premise = unowned_relation_lower_source["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0]
        relation = premise["dependency_proof"]["relation_issuers"][0]
        premise["source_premise_ids"].remove(relation["source_premise_ids"][0])
        invalid_documents.append(unowned_relation_lower_source)

        detached_relation_occurrence = copy.deepcopy(valid_dependency_documents[0])
        proof = detached_relation_occurrence["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        relation = proof["proof_paths"][0]["premises"][0]["dependency_proof"][
            "relation_issuers"
        ][0]
        relation["step_key"] = proof["branches"][0]["steps"][1]["step_key"]
        registry.validate_document(
            detached_relation_occurrence,
            move_path,
            label="transport preserves a typed relation issuer on its branch without re-deriving its causal index",
        )

        mismatched_relation_scope = copy.deepcopy(valid_dependency_documents[0])
        relation = mismatched_relation_scope["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0][
            "dependency_proof"
        ]["relation_issuers"][0]
        relation["scope"] = "candidate_line"
        invalid_documents.append(mismatched_relation_scope)

        unowned_position_state = copy.deepcopy(valid_dependency_documents[0])
        premises = unowned_position_state["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"]
        state_owner = premises[0]["dependency_proof"]["position_state_issuers"][0][
            "issuer_evidence_id"
        ]
        premises[0]["source_premise_ids"].remove(state_owner)
        invalid_documents.append(unowned_position_state)

        unowned_position_state_occurrence = copy.deepcopy(valid_dependency_documents[0])
        premise = unowned_position_state_occurrence["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0]
        state_occurrence = premise["dependency_proof"]["position_state_issuers"][0][
            "issuer_occurrence_id"
        ]
        premise["source_premise_ids"].remove(state_occurrence)
        invalid_documents.append(unowned_position_state_occurrence)

        detached_position_state_line = copy.deepcopy(valid_dependency_documents[0])
        proof = detached_position_state_line["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        analysis_state = proof["proof_paths"][0]["premises"][0]["dependency_proof"][
            "position_state_issuers"
        ][0]
        analysis_state["line"] = {
            "line_id": "line.foreign",
            "line_role": "alternative",
            "line_rank": 3,
            "root_move": "h8h1",
        }
        analysis_state["scope"] = "candidate_line"
        invalid_documents.append(detached_position_state_line)

        discontinuous_position_state_route = copy.deepcopy(valid_dependency_documents[0])
        state = discontinuous_position_state_route["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0]["premises"][0]["dependency_proof"][
            "position_state_issuers"
        ][0]
        state["fen_before"] = FEN
        invalid_documents.append(discontinuous_position_state_route)

        duplicate_path_identifier = copy.deepcopy(ready)
        proof_paths = duplicate_path_identifier["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"]
        duplicate_path = copy.deepcopy(proof_paths[0])
        duplicate_path["closure_use_ids"] = ["a" * 64]
        proof_paths.append(duplicate_path)
        invalid_documents.append(duplicate_path_identifier)

        dangling_branch_reference = copy.deepcopy(ready)
        dangling_branch_reference["move_commentary"]["causal_explanations"][0][
            "channels"
        ][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]["proof_paths"][0][
            "analysis_continuation_branch_id"
        ] = "f" * 64
        invalid_documents.append(dangling_branch_reference)

        for document in invalid_documents:
            with self.subTest(document=document), self.assertRaises(ContractError):
                registry.validate_document(document, move_path, label="closed v6 shape")

    def test_schema_registry_rejects_unsupported_validation_vocabulary(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            schema_path = root / "unsupported.schema.json"
            schema_path.write_text(
                json.dumps(
                    {
                        "$schema": "https://json-schema.org/draft/2020-12/schema",
                        "type": "integer",
                        "multipleOf": 2,
                    }
                ),
                encoding="utf-8",
            )
            with self.assertRaisesRegex(ContractError, "unsupported schema keyword"):
                SchemaRegistry(root).validate_document(
                    4,
                    schema_path,
                    label="unsupported vocabulary",
                )

    def test_best_choice_runner_up_cannot_improve_on_rank_one(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        endpoint = {
            "kind": "engine_search",
            "moves": ["e2e4"],
            "win_percent_for_mover": 50,
            "depth": 16,
        }
        ready = {
            "schema_version": "chesstory.move-meaning.response.v6",
            "status": "ready",
            "move_commentary": {
                "primary": {
                    "kind": "best_choice",
                    "comparison_evidence_id": "best-vs-second",
                    "runner_up_verdict_code": "matches_reference",
                    "verdict_confidence": "engine_backed",
                    "mover": "white",
                    "delta": {
                        "kind": "engine_evaluation",
                        "candidate_win_percent_delta_for_mover": 0,
                    },
                    "best_endpoint": endpoint,
                    "runner_up_endpoint": copy.deepcopy(endpoint),
                },
            },
        }
        registry.validate_document(ready, move_path, label="best choice")

        impossible = copy.deepcopy(ready)
        impossible["move_commentary"]["primary"]["runner_up_verdict_code"] = (
            "improves_on_reference"
        )
        with self.assertRaises(ContractError):
            registry.validate_document(impossible, move_path, label="impossible runner-up")

    def test_endpoint_domains_reject_invalid_values(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"

        terminal_endpoint = {
            "kind": "exact_automatic_terminal",
            "moves": ["e2e4"],
            "terminal": {"kind": "stalemate"},
        }
        no_mate_endpoint = {
            "kind": "engine_search",
            "moves": ["e2e4"],
            "win_percent_for_mover": 50,
            "depth": 8,
        }
        base_ready = {
            "schema_version": "chesstory.move-meaning.response.v6",
            "status": "ready",
            "move_commentary": {
                "primary": {
                    "kind": "move_verdict",
                    "comparison_evidence_id": "comparison",
                    "verdict_code": "matches_reference",
                    "verdict_confidence": "engine_backed",
                    "mover": "white",
                    "delta": {
                        "kind": "engine_evaluation",
                        "candidate_win_percent_delta_for_mover": 0,
                    },
                    "reference_endpoint": no_mate_endpoint,
                    "played_endpoint": copy.deepcopy(no_mate_endpoint),
                }
            },
        }
        registry.validate_document(base_ready, move_path, label="bounded ready")
        mate_depth_zero = copy.deepcopy(base_ready)
        mate_depth_zero["move_commentary"]["primary"]["reference_endpoint"] = {
            "kind": "engine_search",
            "moves": ["e2e4"],
            "win_percent_for_mover": 100,
            "depth": 0,
            "mate_forecast_white_pov": 1,
        }
        registry.validate_document(mate_depth_zero, move_path, label="mate depth zero")

        invalid_documents: list[dict[str, object]] = []
        bad_endpoint_moves = copy.deepcopy(base_ready)
        bad_endpoint_moves["move_commentary"]["primary"]["reference_endpoint"]["moves"] = [
            "e2e4"
        ] * 81
        invalid_documents.append(bad_endpoint_moves)
        bad_terminal_moves = copy.deepcopy(base_ready)
        bad_terminal_moves["move_commentary"]["primary"]["reference_endpoint"] = (
            terminal_endpoint
        )
        bad_terminal_moves["move_commentary"]["primary"]["played_endpoint"] = (
            copy.deepcopy(terminal_endpoint)
        )
        bad_terminal_moves["move_commentary"]["primary"]["verdict_confidence"] = (
            "legal_replay_verified"
        )
        bad_terminal_moves["move_commentary"]["primary"]["delta"]["kind"] = "outcome_only"
        bad_terminal_moves["move_commentary"]["primary"]["played_endpoint"]["moves"] = [
            "e2e4"
        ] * 81
        invalid_documents.append(bad_terminal_moves)
        bad_depth = copy.deepcopy(base_ready)
        bad_depth["move_commentary"]["primary"]["reference_endpoint"]["depth"] = 101
        invalid_documents.append(bad_depth)
        bad_mate = copy.deepcopy(mate_depth_zero)
        bad_mate["move_commentary"]["primary"]["reference_endpoint"]["mate_forecast_white_pov"] = 1001
        invalid_documents.append(bad_mate)
        bad_delta = copy.deepcopy(base_ready)
        bad_delta["move_commentary"]["primary"]["delta"][
            "candidate_win_percent_delta_for_mover"
        ] = 101
        invalid_documents.append(bad_delta)
        bad_no_mate_depth = copy.deepcopy(base_ready)
        bad_no_mate_depth["move_commentary"]["primary"]["reference_endpoint"]["depth"] = 7
        invalid_documents.append(bad_no_mate_depth)
        for document in invalid_documents:
            with self.subTest(document=document), self.assertRaises(ContractError):
                registry.validate_document(document, move_path, label="closed numeric bound")

    def test_focused_response_exposes_only_selected_v6_reviews(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        position_path = (
            ROOT
            / "schemas"
            / "public-commentary-v6"
            / "position-commentary-response.schema.json"
        )
        endpoint = {
            "kind": "engine_search",
            "moves": ["e2e4"],
            "win_percent_for_mover": 50,
            "depth": 16,
        }
        commentary = {
            "primary": {
                "kind": "move_verdict",
                "comparison_evidence_id": "comparison",
                "verdict_code": "matches_reference",
                "verdict_confidence": "engine_backed",
                "mover": "white",
                "delta": {
                    "kind": "engine_evaluation",
                    "candidate_win_percent_delta_for_mover": 0,
                },
                "reference_endpoint": endpoint,
                "played_endpoint": copy.deepcopy(endpoint),
            }
        }
        base = {
            "schema_version": "chesstory.position-commentary.response.v6",
            "annotation_policy_revision": "chesstory.verdict-threshold-policy.v2",
            "engine_profile": "sf18-smallnet-t2-h16-v1",
            "request_id": "move-choices-pair",
            "job_id": "a" * 32,
            "variant": "standard",
            "current_fen": FEN,
            "focus": {
                "kind": "played_move",
                "played_move_uci": "e2e4",
                "resulting_fen": FEN,
            },
            "progress": {
                "phase": "completed",
                "legal_move_count": 20,
                "root_candidate_lines_admitted": 3,
                "selected_commentaries_completed": 1,
                "physical_works_issued": 1,
                "physical_reports_accepted": 1,
            },
            "result": {
                "kind": "selected_move_choices",
                "selected_move_reviews": [
                    {
                        "legal_move_index": 0,
                        "move_uci": "e2e4",
                        "selection": {"roles": ["best", "played"], "root_rank": 1},
                        "commentary": commentary,
                    },
                ],
            },
        }
        claims = [{"rule": "threefold_repetition", "availability": "available_now"}]
        registry.validate_document(base, position_path, label="focused review")
        paired = copy.deepcopy(base)
        paired["result"]["selected_move_reviews"] = [
            {
                "legal_move_index": 1,
                "move_uci": "d2d4",
                "selection": {"roles": ["best"], "root_rank": 1},
                "line_insight": {"endpoint": copy.deepcopy(endpoint)},
            },
            {
                "legal_move_index": 0,
                "move_uci": "e2e4",
                "selection": {"roles": ["played"], "root_rank": 2},
                "commentary": copy.deepcopy(commentary),
            },
        ]
        registry.validate_document(paired, position_path, label="paired focused review")
        with_claim = copy.deepcopy(base)
        with_claim["result"]["draw_claims"] = claims
        registry.validate_document(with_claim, position_path, label="focused review with claim")

        invalid_results = []
        unsupported_inventory = copy.deepcopy(base)
        unsupported_inventory["result"] = {
            "kind": "unsupported_inventory",
            "all_move_reviews": [],
        }
        invalid_results.append(unsupported_inventory)
        server_prose = copy.deepcopy(base)
        server_prose["result"]["presentation"] = "Unbound prose"
        invalid_results.append(server_prose)
        malformed_claims = copy.deepcopy(with_claim)
        malformed_claims["result"]["draw_claims"] = [{"rule": "threefold_repetition"}]
        invalid_results.append(malformed_claims)
        extra_key = copy.deepcopy(with_claim)
        extra_key["result"]["extra"] = True
        invalid_results.append(extra_key)
        reference_only_pair = copy.deepcopy(paired)
        reference_only_pair["result"]["selected_move_reviews"][1] = copy.deepcopy(
            reference_only_pair["result"]["selected_move_reviews"][0]
        )
        invalid_results.append(reference_only_pair)
        reversed_pair = copy.deepcopy(paired)
        reversed_pair["result"]["selected_move_reviews"].reverse()
        invalid_results.append(reversed_pair)
        played_only_single = copy.deepcopy(paired)
        played_only_single["result"]["selected_move_reviews"] = [
            copy.deepcopy(played_only_single["result"]["selected_move_reviews"][1])
        ]
        invalid_results.append(played_only_single)
        for document in invalid_results:
            with self.subTest(document=document), self.assertRaises(ContractError):
                registry.validate_document(document, position_path, label="non-v6 focused response")

    def test_checked_response_rejects_http_status_contradictions(self) -> None:
        contradictions = (
            self._exchange(202, {"status": "ready"}),
            self._exchange(200, {"status": "error", "error": "invalid_request"}),
            self._exchange(202, {"status": "engine_work_required"}),
        )
        for exchange in contradictions:
            with self.subTest(exchange=exchange), self.assertRaises(ContractError):
                _checked_runtime_response(exchange)


if __name__ == "__main__":
    unittest.main()
