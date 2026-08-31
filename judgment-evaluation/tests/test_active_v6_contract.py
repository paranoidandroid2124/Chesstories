from __future__ import annotations

import base64
import copy
import io
import json
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch

from chesstory_eval import cause_audit
from chesstory_eval.cause_audit import _checked_runtime_response
from chesstory_eval.hashing import sha256_bytes
from chesstory_eval.model import ContractError, IntegrityError
from chesstory_eval.probe_completion import (
    _RuntimeExchange,
    _RuntimeJsonlSession,
    _validate_probe_request,
    _verified_response_jsonl_bytes,
)
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
PASSED_PAWN_FEN = "7k/8/5KB1/8/8/8/P7/8 w - - 0 1"
PASSED_PAWN_AFTER_A3 = "7k/8/5KB1/8/8/P7/8/8 b - - 0 1"


# Schema-only wire sample. Exact private ancestry and owner identities are
# certified by the Scala assembler-to-serializer tests, not these placeholders.
def _passed_pawn_result_proof() -> dict[str, object]:
    expected_branch_id = "1" * 64
    reply_branch_id = "2" * 64
    played_line = {
        "line_id": "line.played.passed-pawn-result",
        "line_role": "played",
        "line_rank": 2,
        "root_move": "a2a3",
    }
    reply_line = {
        "line_id": "line.reply.h8g8",
        "line_role": "branch_reply",
        "line_rank": 1,
        "root_move": "h8g8",
    }
    after_reply = "6k1/8/5KB1/8/8/P7/8/8 w - - 1 2"
    after_result = "6k1/8/5KB1/8/P7/8/8/8 b - - 0 2"
    root_key = f"1:a2a3:{PASSED_PAWN_FEN}:{PASSED_PAWN_AFTER_A3}"
    reply_key = f"2:h8g8:{PASSED_PAWN_AFTER_A3}:{after_reply}"
    result_key = f"3:a3a4:{after_reply}:{after_result}"
    expected_dependency_key = "expected-passed-pawn-result-dependency"

    def object_state_dependency_proof() -> dict[str, object]:
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
    expected_result_step = {
        "step_index": 1,
        "step_key": result_key,
        "ply": 3,
        "move_uci": "a3a4",
        "fen_before": after_reply,
        "fen_after": after_result,
        "line": played_line,
        "provenance": "certified_analysis_move",
        "incoming_link": {
            "kind": "certified_causal_dependency",
            "from_step_key": root_key,
            "to_step_key": result_key,
            "occurrence_link_key": expected_dependency_key,
        },
    }
    reply_step = {
        "step_index": 1,
        "step_key": reply_key,
        "ply": 2,
        "move_uci": "h8g8",
        "fen_before": PASSED_PAWN_AFTER_A3,
        "fen_after": after_reply,
        "line": reply_line,
        "provenance": "certified_analysis_move",
        "incoming_link": {
            "kind": "adjacent_legal_replay",
            "from_step_key": root_key,
            "to_step_key": reply_key,
            "occurrence_link_key": "adjacent-reply",
        },
    }
    observed_result_step = {
        **expected_result_step,
        "step_index": 2,
        "line": reply_line,
        "incoming_link": {
            "kind": "adjacent_legal_replay",
            "from_step_key": reply_key,
            "to_step_key": result_key,
            "occurrence_link_key": "adjacent-result",
        },
    }
    return {
        "contract": "passed_pawn_result_under_closed_replies",
        "source_evidence_id": "passed-pawn-result-proof-evidence",
        "event_evidence_id": "passed-pawn-result-event-evidence",
        "comparison_evidence_id": "comparison-evidence",
        "semantic_id": "a" * 64,
        "occurrence_id": "b" * 64,
        "dependency_fingerprint": "c" * 64,
        "consequence_kind": "passed_pawn_progress",
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
            "issuer": "structural_delta.canonical_legal_reply_inventory",
            "issuer_evidence_id": "structural-delta-evidence",
            "coverage_issuer": "passed_pawn_result_event.branch_complete_reply_coverage",
            "coverage_evidence_id": "passed-pawn-result-event-evidence",
            "root_after": {"fen": PASSED_PAWN_AFTER_A3, "ply": 1, "scope": "played_transition"},
            "legal_reply_moves": ["h8g8"],
            "branch_by_reply": [
                {"reply_move": "h8g8", "branch_id": reply_branch_id}
            ],
            "certified_horizon_ply_offset": 2,
        },
        "branches": [
            {
                "branch_id": expected_branch_id,
                "role": "expected_result_route",
                "line": played_line,
                "root_provenance": "observed_game_root",
                "steps": [root_step, expected_result_step],
            },
            {
                "branch_id": reply_branch_id,
                "role": "legal_reply",
                "reply_move": "h8g8",
                "source_probe_id": "probe.reply.h8g8",
                "line": played_line,
                "root_provenance": "observed_game_root",
                "steps": [root_step, reply_step, observed_result_step],
            },
        ],
        "proof_paths": [
            {
                "path_occurrence_id": "8" * 64,
                "reply_branch_id": reply_branch_id,
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
                "realization_match_kind": "exact_move",
                "premises": [
                    {
                        "role": "comparison_demand",
                        "lower_kind": "played_vs_best_demand",
                        "lower_semantic_key": "comparison-key",
                        "source_premise_ids": ["comparison-evidence"],
                        "branch_id": expected_branch_id,
                        "branch_role": "expected_result_route",
                        "related_branch_ids": [],
                        "from_step_index": 0,
                        "to_step_index": 0,
                    },
                    {
                        "role": "expected_dependency",
                        "lower_kind": "passed_pawn_result_dependency",
                        "lower_semantic_key": expected_dependency_key,
                        "source_premise_ids": ["passed-pawn-result-event-evidence"],
                        "branch_id": expected_branch_id,
                        "branch_role": "expected_result_route",
                        "related_branch_ids": [],
                        "from_step_index": 0,
                        "to_step_index": 1,
                        "dependency_proof": object_state_dependency_proof(),
                    },
                    {
                        "role": "expected_result",
                        "lower_kind": "passed_pawn_result",
                        "lower_semantic_key": "expected-result",
                        "source_premise_ids": ["passed-pawn-result-event-evidence"],
                        "branch_id": expected_branch_id,
                        "branch_role": "expected_result_route",
                        "related_branch_ids": [],
                        "from_step_index": 1,
                        "to_step_index": 1,
                    },
                    {
                        "role": "observed_dependency",
                        "lower_kind": "observed_passed_pawn_result_dependency",
                        "lower_semantic_key": "observed-dependency",
                        "source_premise_ids": ["passed-pawn-result-event-evidence"],
                        "branch_id": reply_branch_id,
                        "branch_role": "legal_reply",
                        "related_branch_ids": [],
                        "from_step_index": 0,
                        "to_step_index": 2,
                        "dependency_proof": object_state_dependency_proof(),
                    },
                    {
                        "role": "observed_result",
                        "lower_kind": "observed_passed_pawn_result",
                        "lower_semantic_key": "observed-result",
                        "source_premise_ids": ["passed-pawn-result-event-evidence"],
                        "branch_id": reply_branch_id,
                        "branch_role": "legal_reply",
                        "related_branch_ids": [],
                        "from_step_index": 2,
                        "to_step_index": 2,
                    },
                    {
                        "role": "functional_match",
                        "lower_kind": "passed_pawn_result_functional_match",
                        "lower_semantic_key": "functional-match",
                        "source_premise_ids": ["passed-pawn-result-event-evidence"],
                        "branch_id": reply_branch_id,
                        "branch_role": "legal_reply",
                        "related_branch_ids": [expected_branch_id],
                        "from_step_index": 2,
                        "to_step_index": 2,
                    },
                ],
                "closure_use_ids": ["9" * 64],
            }
        ],
        "lower_premise_ids": [
            "comparison-evidence",
            "passed-pawn-result-event-evidence",
            "structural-delta-evidence",
        ],
    }


def _defense_obligation_change_proof() -> dict[str, object]:
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
        return {
            "role": role,
            "contract": contract,
            "result_id": f"{contract}:{suffix * 64}",
            "source_premise_ids": [f"source.{suffix}"],
            "branch_id": branch_id,
            "branch_role": branch_role,
            "step_index": step_index,
        }

    return {
        "contract": "defense_obligation_change",
        "mechanism": "sole_recapturer_removal",
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
            "branch_role": "observed_played_root",
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
                    premise("played_immediate_exploit_inventory", "capture_recapture_inventory", played_id, "observed_played_root", 0, "5"),
                ],
                "closed_absence_uses": [
                    {
                        "use_id": "6" * 64,
                        "role": "reference_replacement_recapture_absent",
                        "semantic_proof_id": "7" * 64,
                        "issuer": "position_relation_extractor.closed_relation_inventory",
                        "issuer_evidence_id": "reference-line-evidence",
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


def _promoting_defense_obligation_change_proof() -> dict[str, object]:
    proof = _defense_obligation_change_proof()
    root = "3qkr2/2P3B1/8/8/8/8/8/K7 w - - 0 1"
    reference_fens = [
        "3qkB2/2P5/8/8/8/8/8/K7 b - - 0 1",
        "3q1k2/2P5/8/8/8/8/8/K7 w - - 0 2",
        "3Q1k2/8/8/8/8/8/8/K7 b - - 0 2",
    ]
    played_fens = [
        "3Qkr2/8/8/8/8/8/8/K7 b - - 0 1",
        "3rk3/8/8/8/8/8/8/K7 w - - 0 2",
    ]
    reference_moves = ["g7f8", "e8f8", "c7d8q"]
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
            "from": "e8",
            "to": "f8",
            "piece_before": "king",
            "piece_after": "king",
            "move_uci": "e8f8",
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
    def _probe(identifier: str) -> dict[str, object]:
        return {
            "id": identifier,
            "fen": FEN,
            "moves": ["e2e4"],
            "depth": 16,
            "purpose": "branch_reply",
            "multiPv": 1,
            "candidateMove": "e2e4",
            "variationHash": "a" * 64,
            "horizon": "ply:2147483647",
        }

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

    def test_standard_causal_channel_rejects_unproduced_change_names(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        schema = registry.load(move_path)
        channel_schema = schema["$defs"]["standardCausalChannel"]
        channel = {
            "channel_id": "cause-channel:closed-change-vocabulary",
            "causal_signature": "cause.closed-change-vocabulary",
            "direct_change": "occurred",
            "played_change": "lost",
            "actor": {
                "move_uci": "e2e4",
                "side": "white",
                "piece": "pawn:e2",
                "from": "e2",
                "to": "e4",
            },
            "targets": [],
            "mechanisms": [],
            "consequences": [],
            "witnesses": [],
            "proof_line_moves": ["e2e4"],
        }

        errors: list[str] = []
        registry._validate(channel, channel_schema, move_path, "$", errors)
        self.assertEqual(errors, [])

        for retired in ("prevented", "refuted", "missed"):
            invalid = {**channel, "direct_change": retired}
            errors = []
            registry._validate(invalid, channel_schema, move_path, "$", errors)
            self.assertTrue(errors, retired)

        for retired in ("prevented", "refuted"):
            invalid = {**channel, "played_change": retired}
            errors = []
            registry._validate(invalid, channel_schema, move_path, "$", errors)
            self.assertTrue(errors, retired)

    def test_defense_obligation_change_requires_its_exact_three_premise_manifest(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        definitions = registry.load(move_path)["$defs"]
        proof_schema = definitions["defenseObligationChangeProof"]
        proof = _defense_obligation_change_proof()

        def validation_errors(candidate: dict[str, object]) -> list[str]:
            errors: list[str] = []
            registry._validate(candidate, proof_schema, move_path, "$", errors)
            if not errors:
                registry._validate_defense_obligation_change_proof_identifiers(
                    candidate, "$", errors
                )
            return errors

        self.assertEqual(validation_errors(proof), [])

        def set_exploit_move(candidate: dict[str, object], move: str) -> None:
            candidate["later_exploit_move"] = move
            candidate["counterfactual_reference_branch"]["steps"][2]["move_uci"] = move
            candidate["played_root_branch"]["root_move"] = move
            candidate["played_root_branch"]["steps"][0]["move_uci"] = move

        for promoted_piece, suffix in (
            ("queen", "q"),
            ("rook", "r"),
            ("bishop", "b"),
            ("knight", "n"),
        ):
            promotion = _promoting_defense_obligation_change_proof()
            promotion["participants"]["later_exploit"]["piece_after"] = promoted_piece
            set_exploit_move(promotion, f"c7d8{suffix}")
            self.assertEqual(validation_errors(promotion), [])

        missing_promotion_suffix = _promoting_defense_obligation_change_proof()
        set_exploit_move(missing_promotion_suffix, "c7d8")
        self.assertTrue(validation_errors(missing_promotion_suffix))

        wrong_promotion_suffix = _promoting_defense_obligation_change_proof()
        set_exploit_move(wrong_promotion_suffix, "c7d8r")
        self.assertTrue(validation_errors(wrong_promotion_suffix))

        nonpromotion_suffix = copy.deepcopy(proof)
        set_exploit_move(nonpromotion_suffix, "d1d5q")
        self.assertTrue(validation_errors(nonpromotion_suffix))

        mechanism_mismatch = copy.deepcopy(proof)
        mechanism_mismatch["mechanism"] = "forced_displacement"
        self.assertTrue(validation_errors(mechanism_mismatch))

        missing_removal = copy.deepcopy(proof)
        missing_removal["proof_paths"][0]["premises"].pop(0)
        self.assertTrue(validation_errors(missing_removal))

        detached_defender = copy.deepcopy(proof)
        detached_defender["participants"]["removed_defender"]["square"] = "a1"
        self.assertTrue(validation_errors(detached_defender))

        detached_removal_recapture = copy.deepcopy(proof)
        detached_removal_recapture["participants"]["removal_recapture"]["to"] = "e6"
        detached_removal_recapture["participants"]["removal_recapture"]["move_uci"] = "e7e6"
        detached_removal_recapture["counterfactual_reference_branch"]["steps"][1]["move_uci"] = "e7e6"
        self.assertTrue(validation_errors(detached_removal_recapture))

        detached_played_recapture = copy.deepcopy(proof)
        detached_played_recapture["participants"]["played_sole_recapture"]["from"] = "h7"
        detached_played_recapture["participants"]["played_sole_recapture"]["move_uci"] = "h7d5"
        detached_played_recapture["played_root_branch"]["steps"][1]["move_uci"] = "h7d5"
        detached_played_recapture["played_sole_recapture_move"] = "h7d5"
        self.assertTrue(validation_errors(detached_played_recapture))

        malformed_role = copy.deepcopy(proof)
        malformed_role["proof_paths"][0]["premises"][1]["role"] = "reference_defender_removal"
        self.assertTrue(validation_errors(malformed_role))

        wrong_premise_branch = copy.deepcopy(proof)
        wrong_premise_branch["proof_paths"][0]["premises"][1]["branch_id"] = "2" * 64
        self.assertTrue(validation_errors(wrong_premise_branch))

        wrong_replacement_absence = copy.deepcopy(proof)
        wrong_replacement_absence["proof_paths"][0]["closed_absence_uses"][0][
            "query"
        ] = "legal-capture:white:d5"
        self.assertTrue(validation_errors(wrong_replacement_absence))

        wrong_absence_position = copy.deepcopy(proof)
        wrong_absence_position["proof_paths"][0]["closed_absence_uses"][0]["position"]["fen"] = FEN
        self.assertTrue(validation_errors(wrong_absence_position))

        noncanonical_sources = copy.deepcopy(proof)
        noncanonical_sources["proof_paths"][0]["premises"][0]["source_premise_ids"] = ["source.z", "source.a"]
        self.assertTrue(validation_errors(noncanonical_sources))

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
            "causal_signature": "defense.signature",
            "direct_change": "occurred",
            "played_change": "missed",
            "defense_obligation_change_proof": proof,
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
        mixed_channel["resource_differential_proof"] = proof
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

        status, probes, stop_reason = _checked_runtime_response(exchange)
        self.assertEqual((status, probes), ("error", None))
        self.assertEqual(stop_reason, "runtime_error:invalid_request")
        self.assertNotEqual(stop_reason, "no_probe_requests")

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
                    "probe_requests": [],
                },
            },
        ):
            with self.subTest(invalid_outer=invalid_outer), self.assertRaises(ContractError):
                session._output.put(("line", json.dumps(invalid_outer).encode("utf-8")))
                session.invoke(request)

        with self.assertRaises(IntegrityError):
            _verified_response_jsonl_bytes(exchange.response_jsonl_base64, "f" * 64)

    def test_public_move_schema_accepts_legal_passed_pawn_result_shape(self) -> None:
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
                    "verdict_code": "matches_reference",
                    "verdict_confidence": "engine_backed",
                    "mover": "white",
                    "delta": {
                        "kind": "engine_evaluation",
                        "candidate_win_percent_delta_for_mover": 0,
                    },
                    "reference_endpoint": reference_endpoint,
                    "played_endpoint": played_endpoint,
                },
                "causal_explanations": [
                    {
                        "kind": "single_cause",
                        "facets": [
                            {
                                "facet_role": "lead",
                                "cause_evidence_id": "cause-evidence",
                                "kind": "passed_pawn_result",
                                "proof_confidence": "legal_replay_verified",
                                 "effect_mode": "played_value",
                                 "exposure": "primary",
                                 "source_side": "candidate",
                                 "comparison_kind": "played_vs_best",
                                "channels": [
                                    {
                                        "channel_id": "channel-1",
                                         "causal_signature": "passed-pawn-result-channel",
                                         "direct_change": "occurred",
                                         "passed_pawn_result_proof": _passed_pawn_result_proof(),
                                     }
                                ],
                            }
                        ],
                    }
                ],
            },
        }
        registry.validate_document(ready, move_path, label="public v6 legal passed-pawn-result schema shape")

        defense_ready = copy.deepcopy(ready)
        defense_facet = defense_ready["move_commentary"]["causal_explanations"][0][
            "facets"
        ][0]
        defense_facet.update(
            kind="wrong_move_order",
            effect_mode="alternative_resource",
            source_side="reference",
        )
        defense_facet["channels"] = [
            {
                "channel_id": "channel-defense-obligation-change",
                "causal_signature": "defense-obligation-change-channel",
                "direct_change": "occurred",
                "played_change": "missed",
                "defense_obligation_change_proof": _defense_obligation_change_proof(),
            }
        ]
        registry.validate_document(
            defense_ready,
            move_path,
            label="public v6 legal defense-obligation-change schema shape",
        )

        multiple_proof_paths = copy.deepcopy(ready)
        proof_paths = multiple_proof_paths["move_commentary"]["causal_explanations"][0][
            "facets"
        ][0]["channels"][0]["passed_pawn_result_proof"]["proof_paths"]
        independent_path = copy.deepcopy(proof_paths[0])
        independent_path["path_occurrence_id"] = "a" * 64
        proof_paths.append(independent_path)
        registry.validate_document(
            multiple_proof_paths,
            move_path,
            label="public v6 independent proof paths",
        )

        dependency_variants = [
            {
                "dependency_kind": "line_access_precondition",
                "proof_kind": "line_access",
                "squares": [
                    {"role": "vacated_gate", "square": "a3"},
                    {"role": "enabled_from", "square": "a1"},
                    {"role": "enabled_to", "square": "a4"},
                ],
                "pieces": [
                    {"role": "enabled_piece", "side": "white", "piece": "rook"}
                ],
                "relation_issuers": [
                    {
                        "contract": "slider_reach_delta",
                        "relation_kind": "slider_reach_delta",
                        "result_key": "slider_reach_delta:" + "1" * 64,
                        "occurrence_id": "2" * 64,
                        "step_key": "1:a2a3:issuer-before:issuer-after",
                        "source_premise_ids": ["l1-slider-reach"],
                    }
                ],
            },
            {
                "dependency_kind": "response_continuation_precondition",
                "proof_kind": "pawn_break_follow_up",
                "squares": [
                    {"role": "reply_from", "square": "b4"},
                    {"role": "reply_to", "square": "a3"},
                    {"role": "follow_up_from", "square": "a3"},
                    {"role": "follow_up_to", "square": "a4"},
                    {"role": "released_passed_pawn", "square": "a3"},
                ],
                "pieces": [
                    {"role": "trigger_pawn", "side": "white", "piece": "pawn"},
                    {"role": "responder_pawn", "side": "black", "piece": "pawn"},
                    {"role": "follow_up_pawn", "side": "white", "piece": "pawn"},
                ],
                "relation_issuers": [
                    {
                        "contract": "pawn_topology_transition",
                        "relation_kind": "pawn_topology_transition",
                        "result_key": "pawn_topology_transition:" + "3" * 64,
                        "occurrence_id": "4" * 64,
                        "step_key": "2:b4a3:issuer-before:issuer-after",
                        "source_premise_ids": ["l1-pawn-topology"],
                    }
                ],
            },
            {
                "dependency_kind": "response_continuation_precondition",
                "proof_kind": "capture_follow_up",
                "squares": [
                    {"role": "reply_from", "square": "b4"},
                    {"role": "reply_to", "square": "a3"},
                    {"role": "follow_up_from", "square": "a2"},
                    {"role": "follow_up_to", "square": "a3"},
                ],
                "pieces": [
                    {"role": "trigger_piece", "side": "white", "piece": "bishop"},
                    {"role": "responder_piece", "side": "black", "piece": "rook"},
                    {"role": "follow_up_piece", "side": "white", "piece": "queen"},
                ],
                "relation_issuers": [
                    {
                        "contract": "capture_recapture_inventory",
                        "relation_kind": "capture_recapture_inventory",
                        "result_key": "capture_recapture_inventory:" + "5" * 64,
                        "occurrence_id": "6" * 64,
                        "step_key": "2:b4a3:issuer-before:issuer-after",
                        "source_premise_ids": ["l1-capture-recapture"],
                    }
                ],
            },
        ]
        valid_dependency_documents = []
        for variant in dependency_variants:
            document = copy.deepcopy(ready)
            premises = document["move_commentary"]["causal_explanations"][0]["facets"][0][
                "channels"
            ][0]["passed_pawn_result_proof"]["proof_paths"][0]["premises"]
            issuer = variant["relation_issuers"][0]
            issuer_sources = [issuer["occurrence_id"], *issuer["source_premise_ids"]]
            for premise_index in (1, 3):
                premises[premise_index]["dependency_proof"] = copy.deepcopy(variant)
                premises[premise_index]["source_premise_ids"] = sorted(
                    [*premises[premise_index]["source_premise_ids"], *issuer_sources]
                )
            registry.validate_document(
                document,
                move_path,
                label=f"public v6 {variant['proof_kind']} dependency proof",
            )
            valid_dependency_documents.append(document)

        invalid_documents = []
        passed_pawn_result_without_typed_proof = copy.deepcopy(ready)
        del passed_pawn_result_without_typed_proof["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]
        invalid_documents.append(passed_pawn_result_without_typed_proof)

        passed_pawn_result_with_generic_segment = copy.deepcopy(ready)
        passed_pawn_result_with_generic_segment["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["proof_segment"] = {
            "terminal_relation": "realizes_passed_pawn_result",
            "steps": [{"ply_offset": 0, "move_uci": "a2a3", "role": "root_action"}],
        }
        invalid_documents.append(passed_pawn_result_with_generic_segment)

        passed_pawn_result_with_duplicated_event_move = copy.deepcopy(ready)
        passed_pawn_result_with_duplicated_event_move["move_commentary"]["causal_explanations"][0]["facets"][0][
            "event_move"
        ] = "a2a3"
        invalid_documents.append(passed_pawn_result_with_duplicated_event_move)

        typed_passed_pawn_result_under_standard_kind = copy.deepcopy(ready)
        typed_passed_pawn_result_under_standard_kind["move_commentary"]["causal_explanations"][0]["facets"][0][
            "kind"
        ] = "material_swing"
        invalid_documents.append(typed_passed_pawn_result_under_standard_kind)

        passed_pawn_result_without_closed_reply_branch = copy.deepcopy(ready)
        del passed_pawn_result_without_closed_reply_branch["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["branches"][1]
        invalid_documents.append(passed_pawn_result_without_closed_reply_branch)

        passed_pawn_result_with_unmapped_reply_branch = copy.deepcopy(ready)
        unmapped_proof = passed_pawn_result_with_unmapped_reply_branch["move_commentary"][
            "causal_explanations"
        ][0]["facets"][0]["channels"][0]["passed_pawn_result_proof"]
        unmapped_branch = copy.deepcopy(unmapped_proof["branches"][1])
        unmapped_branch.update(
            branch_id="3" * 64,
            reply_move="h8h7",
            source_probe_id="probe.reply.h8h7",
        )
        unmapped_proof["branches"].append(unmapped_branch)
        with self.assertRaisesRegex(ContractError, "branch ids must match"):
            registry.validate_document(
                passed_pawn_result_with_unmapped_reply_branch,
                move_path,
                label="unmapped legal-reply branch",
            )

        passed_pawn_result_with_unproved_inventory_reply = copy.deepcopy(ready)
        unproved_proof = passed_pawn_result_with_unproved_inventory_reply["move_commentary"][
            "causal_explanations"
        ][0]["facets"][0]["channels"][0]["passed_pawn_result_proof"]
        unproved_branch = copy.deepcopy(unproved_proof["branches"][1])
        unproved_branch.update(
            branch_id="3" * 64,
            reply_move="h8h7",
            source_probe_id="probe.reply.h8h7",
        )
        unproved_proof["branches"].append(unproved_branch)
        unproved_proof["closed_legal_reply_inventory"]["legal_reply_moves"].append("h8h7")
        unproved_proof["closed_legal_reply_inventory"]["branch_by_reply"].append(
            {"reply_move": "h8h7", "branch_id": "3" * 64}
        )
        with self.assertRaisesRegex(ContractError, "proof_paths.*cover"):
            registry.validate_document(
                passed_pawn_result_with_unproved_inventory_reply,
                move_path,
                label="inventory reply without a proof path",
            )

        wrong_move_order_without_l2 = copy.deepcopy(ready)
        wrong_move_order_without_l2["move_commentary"]["causal_explanations"][0]["facets"][0][
            "kind"
        ] = "wrong_move_order"
        wrong_move_order_without_l2["move_commentary"]["causal_explanations"][0]["facets"][0][
            "source_side"
        ] = "reference"
        wrong_move_order_without_l2["move_commentary"]["causal_explanations"][0]["facets"][0][
            "effect_mode"
        ] = "alternative_resource"
        invalid_documents.append(wrong_move_order_without_l2)

        retired_structural_units = copy.deepcopy(ready)
        retired_structural_units["move_commentary"]["structural_idea_units"] = []
        invalid_documents.append(retired_structural_units)

        retired_only_move_qualifiers = copy.deepcopy(ready)
        retired_only_move_qualifiers["move_commentary"]["causal_explanations"][0]["facets"][0][
            "only_move_qualifiers"
        ] = []
        invalid_documents.append(retired_only_move_qualifiers)

        retired_responsibility_links = copy.deepcopy(ready)
        retired_responsibility_links["move_commentary"]["responsibility_links"] = []
        invalid_documents.append(retired_responsibility_links)

        legacy_plan_event = copy.deepcopy(ready)
        legacy_plan_event["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["proof_segment"] = {
            "terminal_relation": "realizes_passed_pawn_result",
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
        stale_cause["move_commentary"]["causal_explanations"][0]["facets"][0][
            "kind"
        ] = "wrong_recapturer"
        invalid_documents.append(stale_cause)

        stale_tempo_loss = copy.deepcopy(ready)
        stale_tempo_loss["move_commentary"]["causal_explanations"][0]["facets"][0][
            "kind"
        ] = "tempo_loss"
        invalid_documents.append(stale_tempo_loss)

        uncertified_plan_refutation = copy.deepcopy(ready)
        uncertified_plan_refutation["move_commentary"]["causal_explanations"][0]["facets"][0][
            "kind"
        ] = "plan_contradiction"
        invalid_documents.append(uncertified_plan_refutation)

        stale_object = copy.deepcopy(ready)
        stale_object["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["targets"] = [{"kind": "motif", "key": "legacy"}]
        invalid_documents.append(stale_object)

        stale_terminal = copy.deepcopy(ready)
        stale_terminal["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["proof_segment"] = {
            "terminal_relation": "restricts_opponent_resource",
            "steps": [{"ply_offset": 0, "move_uci": "a2a3", "role": "root_action"}],
        }
        invalid_documents.append(stale_terminal)

        missing_reply_probe = copy.deepcopy(ready)
        del missing_reply_probe["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["branches"][1]["source_probe_id"]
        invalid_documents.append(missing_reply_probe)

        mismatched_source_side = copy.deepcopy(ready)
        mismatched_source_side["move_commentary"]["causal_explanations"][0]["facets"][0][
            "effect_mode"
        ] = "alternative_resource"
        invalid_documents.append(mismatched_source_side)

        incomplete_premise_manifest = copy.deepcopy(ready)
        premises = incomplete_premise_manifest["move_commentary"]["causal_explanations"][0][
            "facets"
        ][0]["channels"][0]["passed_pawn_result_proof"]["proof_paths"][0]["premises"]
        premises[1] = copy.deepcopy(premises[0])
        invalid_documents.append(incomplete_premise_manifest)

        missing_dependency_proof = copy.deepcopy(ready)
        premises = missing_dependency_proof["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["proof_paths"][0]["premises"]
        del premises[1]["dependency_proof"]
        invalid_documents.append(missing_dependency_proof)

        dependency_proof_on_result = copy.deepcopy(ready)
        premises = dependency_proof_on_result["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["proof_paths"][0]["premises"]
        premises[2]["dependency_proof"] = copy.deepcopy(premises[1]["dependency_proof"])
        invalid_documents.append(dependency_proof_on_result)

        mismatched_dependency_kind = copy.deepcopy(ready)
        premises = mismatched_dependency_kind["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["proof_paths"][0]["premises"]
        premises[1]["dependency_proof"]["proof_kind"] = "line_access"
        invalid_documents.append(mismatched_dependency_kind)

        mismatched_relation_issuer = copy.deepcopy(valid_dependency_documents[0])
        premises = mismatched_relation_issuer["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["proof_paths"][0]["premises"]
        premises[1]["dependency_proof"]["relation_issuers"][0][
            "relation_kind"
        ] = "pawn_topology_transition"
        invalid_documents.append(mismatched_relation_issuer)

        duplicate_path_identifier = copy.deepcopy(ready)
        proof_paths = duplicate_path_identifier["move_commentary"]["causal_explanations"][0][
            "facets"
        ][0]["channels"][0]["passed_pawn_result_proof"]["proof_paths"]
        duplicate_path = copy.deepcopy(proof_paths[0])
        duplicate_path["closure_use_ids"] = ["a" * 64]
        proof_paths.append(duplicate_path)
        invalid_documents.append(duplicate_path_identifier)

        dangling_branch_reference = copy.deepcopy(ready)
        dangling_branch_reference["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["passed_pawn_result_proof"]["proof_paths"][0]["reply_branch_id"] = "f" * 64
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
                    "candidate_set": {"type": "style_choice"},
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

    def test_branch_reply_and_endpoint_domains_reject_invalid_values(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        branch_probe = self._probe("branch")
        self.assertEqual(_validate_probe_request(branch_probe)["id"], "branch")

        bad_multipv = copy.deepcopy(branch_probe)
        bad_multipv["multiPv"] = 2
        with self.assertRaises(ContractError):
            _validate_probe_request(bad_multipv)
        bad_horizon = copy.deepcopy(branch_probe)
        bad_horizon["horizon"] = "ply:2147483648"
        with self.assertRaises(ContractError):
            _validate_probe_request(bad_horizon)
        missing_reply = copy.deepcopy(branch_probe)
        missing_reply["moves"] = []
        with self.assertRaises(ContractError):
            _validate_probe_request(missing_reply)
        illegal_reply = copy.deepcopy(branch_probe)
        illegal_reply["moves"] = ["e7e5"]
        with self.assertRaises(ContractError):
            _validate_probe_request(illegal_reply)

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
                "causal_waves_completed": 0,
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
            self._exchange(
                200,
                {
                    "status": "engine_work_required",
                    "probe_requests": [self._probe("engine")],
                },
            ),
            self._exchange(202, {"status": "ready"}),
            self._exchange(200, {"status": "error", "error": "invalid_request"}),
        )
        for exchange in contradictions:
            with self.subTest(exchange=exchange), self.assertRaises(ContractError):
                _checked_runtime_response(exchange)

    def test_cause_audit_rejects_each_overflow_batch_without_search(self) -> None:
        class FakeArtifactStore:
            def __init__(self, root: Path, run_id: str) -> None:
                self.root = Path(root)
                self.run_id = run_id
                self.run_root = self.root / run_id

            @staticmethod
            def _segment(value: str) -> str:
                return value

            def verify(self) -> None:
                return None

        def assert_overflow_rejects_without_search(
            probe_requests: list[dict[str, object]], current_results: list[dict[str, str]]
        ) -> None:
            exchange = self._exchange(
                202,
                {
                    "schema_version": "chesstory.move-meaning.response.v6",
                    "status": "engine_work_required",
                    "probe_requests": probe_requests,
                },
            )

            class FakeSession:
                def __enter__(self) -> FakeSession:
                    return self

                def __exit__(self, exc_type: object, exc: object, traceback: object) -> None:
                    return None

                def invoke(self, request: object) -> _RuntimeExchange:
                    return exchange

            with TemporaryDirectory() as directory:
                temporary_root = Path(directory).resolve()
                (temporary_root / "acquisition").mkdir()
                store = FakeArtifactStore(temporary_root, "run")
                with (
                    patch.object(cause_audit, "ArtifactStore", FakeArtifactStore),
                    patch.object(cause_audit, "_schema_tools", return_value=(None, None)),
                    patch.object(cause_audit, "_verify_manifest", return_value=(None, None)),
                    patch.object(
                        cause_audit,
                        "_selected_cases",
                        return_value=[{"case_id": "case"}],
                    ),
                    patch.object(
                        cause_audit,
                        "_read_object",
                        return_value={"run_id": "acquisition"},
                    ),
                    patch.object(
                        cause_audit,
                        "_acquisition_rows",
                        return_value={"case": ({}, {})},
                    ),
                    patch.object(
                        cause_audit,
                        "_runtime_request",
                        return_value={
                            "input": {"probeResults": copy.deepcopy(current_results)}
                        },
                    ),
                    patch.object(cause_audit, "sha256_file", return_value="digest"),
                    patch.object(
                        cause_audit,
                        "_RuntimeJsonlSession",
                        return_value=FakeSession(),
                    ),
                    patch.object(cause_audit, "_search_probe") as search_probe,
                ):
                    with self.assertRaises(ContractError):
                        cause_audit.run_cause_audit(
                            root=temporary_root,
                            store=store,
                            manifest_path=temporary_root,
                            cases_path=temporary_root,
                            acquisition_path=temporary_root,
                            stockfish_path=temporary_root,
                            sbt_path=temporary_root,
                            adapter_root=temporary_root,
                            timeout_seconds=1.0,
                            provider_timeout_seconds=1.0,
                            max_probe_rounds=1,
                        )
                self.assertEqual(search_probe.call_count, 0)

        assert_overflow_rejects_without_search(
            [self._probe(f"batch-{index}") for index in range(13)], []
        )
        assert_overflow_rejects_without_search(
            [self._probe("new")], [{"id": f"old-{index}"} for index in range(12)]
        )


if __name__ == "__main__":
    unittest.main()
