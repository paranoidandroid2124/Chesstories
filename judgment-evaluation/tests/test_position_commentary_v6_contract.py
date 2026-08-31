from __future__ import annotations

import copy
import unittest
from pathlib import Path

from chesstory_eval.model import ContractError
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
SCHEMAS = ROOT / "schemas" / "public-commentary-v6"
INITIAL_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
AFTER_E4 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
PASSED_PAWN_INITIAL_FEN = "7k/8/5KB1/8/8/8/P7/8 w - - 0 1"
PASSED_PAWN_AFTER_A3 = "7k/8/5KB1/8/8/P7/8/8 b - - 0 1"
PROFILE = "sf18-smallnet-t2-h16-v1"


# Schema-only wire sample. Exact private ancestry and owner identities are
# certified by the Scala assembler-to-serializer tests, not these placeholders.
def _passed_pawn_result_proof(after_a3: str) -> dict[str, object]:
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
    root_key = f"1:a2a3:{PASSED_PAWN_INITIAL_FEN}:{after_a3}"
    reply_key = f"2:h8g8:{after_a3}:{after_reply}"
    result_key = f"3:a3a4:{after_reply}:{after_result}"
    expected_dependency_key = "expected-passed-pawn-result-dependency"
    root_step = {
        "step_index": 0,
        "step_key": root_key,
        "ply": 1,
        "move_uci": "a2a3",
        "fen_before": PASSED_PAWN_INITIAL_FEN,
        "fen_after": after_a3,
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
        "fen_before": after_a3,
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
        "comparison_evidence_id": "comparison.played-vs-best",
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
            "root_after": {"fen": after_a3, "ply": 1, "scope": "played_transition"},
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
                        "source_premise_ids": ["comparison.played-vs-best"],
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
            "comparison.played-vs-best",
            "passed-pawn-result-event-evidence",
            "structural-delta-evidence",
        ],
    }


class PositionCommentaryV6ContractTest(unittest.TestCase):
    def setUp(self) -> None:
        self.registry = SchemaRegistry(ROOT / "schemas")

    def validate(self, schema_name: str, document: object) -> None:
        self.registry.validate_document(
            document,
            SCHEMAS / schema_name,
            label=schema_name,
        )

    @staticmethod
    def progress(phase: str, *, issued: int, accepted: int) -> dict[str, object]:
        return {
            "phase": phase,
            "legal_move_count": 20,
            "root_candidate_lines_admitted": 0 if phase == "root_search" else 3,
            "selected_commentaries_completed": 1 if phase == "completed" else 0,
            "physical_works_issued": issued,
            "physical_reports_accepted": accepted,
            "causal_waves_completed": 1 if phase == "completed" else 0,
        }

    @staticmethod
    def focus() -> dict[str, object]:
        return {
            "kind": "played_move",
            "played_move_uci": "e2e4",
            "resulting_fen": AFTER_E4,
        }

    @classmethod
    def root_work(cls) -> dict[str, object]:
        return {
            "work_id": "work:0",
            "purpose": "root_search",
            "engine_profile": PROFILE,
            "execution_key_sha256": "a" * 64,
            "variant": "standard",
            "engine_position_initial_fen": INITIAL_FEN,
            "engine_position_moves_uci": [],
            "search_fen": INITIAL_FEN,
            "root_restriction": {"kind": "unrestricted"},
            "search_limits": {
                "depth": 16,
                "nodes": 5_000_000,
                "movetime_ms": 5_000,
                "multi_pv": 3,
            },
            "max_search_elapsed_ms": 6_000,
        }

    def test_request_status_report_and_error_are_one_closed_v6_cohort(self) -> None:
        request = {
            "schema_version": "chesstory.position-commentary.job-request.v6",
            "request_id": "contract-v6",
            "variant": "standard",
            "initial_fen": INITIAL_FEN,
            "move_prefix_uci": [],
            "current_fen": INITIAL_FEN,
            "focus": self.focus(),
            "engine_profile": PROFILE,
        }
        self.validate("position-commentary-job-request.schema.json", request)

        status = {
            "schema_version": "chesstory.position-commentary.job-status.v6",
            "request_id": "contract-v6",
            "job_id": "a" * 32,
            "engine_profile": PROFILE,
            "variant": "standard",
            "deadline_epoch_ms": 9_999_999_999_999,
            "focus": self.focus(),
            "state": "awaiting_engine_work",
            "progress": self.progress("root_search", issued=1, accepted=0),
            "issued_engine_work": self.root_work(),
        }
        self.validate("position-commentary-job-status.schema.json", status)

        report = {
            "schema_version": "chesstory.position-commentary.engine-work-report.v6",
            "engine_profile": PROFILE,
            "work_id": "work:0",
            "execution_key_sha256": "a" * 64,
            "outcome": {
                "kind": "completed",
                "line_suffixes": [
                    {
                        "moves": [move],
                        "depth": 16,
                        "white_score": {"kind": "cp", "value": score},
                    }
                    for move, score in (("e2e4", 25), ("d2d4", 20), ("g1f3", 15))
                ],
            },
        }
        self.validate("position-commentary-engine-work-report.schema.json", report)
        self.validate(
            "position-commentary-job-error.schema.json",
            {
                "schema_version": "chesstory.position-commentary.job-error.v6",
                "request_id": "contract-v6",
                "job_id": "a" * 32,
                "engine_profile": PROFILE,
                "error": "invalid_engine_work_report",
            },
        )

        for document, schema in (
            ({**request, "variant": "chess960"}, "position-commentary-job-request.schema.json"),
            ({**status, "generation": 1}, "position-commentary-job-status.schema.json"),
            ({**report, "receipts": []}, "position-commentary-engine-work-report.schema.json"),
        ):
            with self.subTest(schema=schema), self.assertRaises(ContractError):
                self.validate(schema, document)

    def test_status_purpose_owns_its_exact_search_shape(self) -> None:
        base = {
            "schema_version": "chesstory.position-commentary.job-status.v6",
            "request_id": "shape-v6",
            "job_id": "b" * 32,
            "engine_profile": PROFILE,
            "variant": "standard",
            "deadline_epoch_ms": 9_999_999_999_999,
            "focus": self.focus(),
            "state": "awaiting_engine_work",
            "progress": self.progress("root_search", issued=1, accepted=0),
            "issued_engine_work": self.root_work(),
        }
        self.validate("position-commentary-job-status.schema.json", base)

        drifted = copy.deepcopy(base)
        drifted["issued_engine_work"]["search_limits"]["nodes"] = 2_000_000
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", drifted)

        focus = copy.deepcopy(base)
        focus["progress"] = self.progress("focus_comparison", issued=2, accepted=1)
        focus["issued_engine_work"].update(
            {
                "work_id": "work:1",
                "purpose": "focus_comparison",
                "execution_key_sha256": "b" * 64,
                "root_restriction": {
                    "kind": "restricted",
                    "moves_uci": ["d2d4", "e2e4"],
                },
                "search_limits": {
                    "depth": 16,
                    "nodes": 2_000_000,
                    "movetime_ms": 2_500,
                    "multi_pv": 2,
                },
                "max_search_elapsed_ms": 3_500,
            }
        )
        self.validate("position-commentary-job-status.schema.json", focus)

        causal = copy.deepcopy(focus)
        causal["progress"] = self.progress("causal_probe", issued=3, accepted=2)
        causal["issued_engine_work"].update(
            {
                "work_id": "work:2",
                "purpose": "causal_probe",
                "execution_key_sha256": "c" * 64,
                "engine_position_moves_uci": ["e2e4"],
                "search_fen": AFTER_E4,
                "root_restriction": {"kind": "unrestricted"},
                "search_limits": {
                    "depth": 16,
                    "nodes": 2_000_000,
                    "movetime_ms": 2_500,
                    "multi_pv": 1,
                },
            }
        )
        self.validate("position-commentary-job-status.schema.json", causal)

    def test_completed_response_schema_accepts_legal_passed_pawn_result_shape(self) -> None:
        after_a3 = PASSED_PAWN_AFTER_A3
        reference_endpoint = {
            "kind": "engine_search",
            "moves": ["a2a4", "h8g8", "a4a5"],
            "win_percent_for_mover": 53,
            "depth": 16,
        }
        played_endpoint = {
            "kind": "engine_search",
            "moves": ["a2a3", "h8g8", "a3a4"],
            "win_percent_for_mover": 53,
            "depth": 16,
        }
        commentary = {
            "primary": {
                "kind": "move_verdict",
                "comparison_evidence_id": "comparison.played-vs-best",
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
                            "cause_evidence_id": "cause.passed-pawn-result",
                            "kind": "passed_pawn_result",
                            "proof_confidence": "legal_replay_verified",
                             "effect_mode": "played_value",
                             "exposure": "primary",
                             "source_side": "candidate",
                             "comparison_kind": "played_vs_best",
                            "channels": [
                                {
                                    "channel_id": "cause-channel:exact-passed-pawn-result",
                                     "causal_signature": "cause.passed-pawn-result:passed-status",
                                     "direct_change": "occurred",
                                     "passed_pawn_result_proof": _passed_pawn_result_proof(after_a3),
                                 }
                            ],
                        }
                    ],
                }
            ],
        }
        response = {
            "schema_version": "chesstory.position-commentary.response.v6",
            "annotation_policy_revision": "chesstory.verdict-threshold-policy.v2",
            "request_id": "complete-v6",
            "job_id": "c" * 32,
            "engine_profile": PROFILE,
            "variant": "standard",
            "current_fen": PASSED_PAWN_INITIAL_FEN,
            "focus": {
                "kind": "played_move",
                "played_move_uci": "a2a3",
                "resulting_fen": after_a3,
            },
            "progress": {
                **self.progress("completed", issued=3, accepted=3),
                "legal_move_count": 17,
            },
            "result": {
                "kind": "selected_move_choices",
                "selected_move_reviews": [
                    {
                        "legal_move_index": 1,
                        "move_uci": "a2a4",
                        "selection": {"roles": ["best"], "root_rank": 1},
                        "line_insight": {"endpoint": reference_endpoint},
                    },
                    {
                        "legal_move_index": 0,
                        "move_uci": "a2a3",
                        "selection": {"roles": ["played"], "root_rank": 2},
                        "commentary": commentary,
                    }
                ],
            },
        }
        self.validate("position-commentary-response.schema.json", response)

        no_typed_passed_pawn_result_proof = copy.deepcopy(response)
        del no_typed_passed_pawn_result_proof["result"]["selected_move_reviews"][1]["commentary"][
            "causal_explanations"
        ][0]["facets"][0]["channels"][0]["passed_pawn_result_proof"]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", no_typed_passed_pawn_result_proof)

        retired_projection_fields = copy.deepcopy(response)
        retired_projection_fields["result"]["selected_move_reviews"][1]["commentary"][
            "structural_idea_units"
        ] = []
        with self.assertRaises(ContractError):
            self.validate(
                "position-commentary-response.schema.json", retired_projection_fields
            )

        heuristic = copy.deepcopy(response)
        heuristic["result"]["selected_move_reviews"][1]["commentary"][
            "causal_explanations"
        ][0]["facets"][0]["proof_confidence"] = "heuristic"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", heuristic)


if __name__ == "__main__":
    unittest.main()
