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
def _passed_pawn_progress_realized_after_only_legal_reply_proof(after_a3: str) -> dict[str, object]:
    analysis_continuation_branch_id = "1" * 64
    played_line = {
        "line_id": "line.played.passed-pawn-result",
        "line_role": "played",
        "line_rank": 2,
        "root_move": "a2a3",
    }
    after_reply = "6k1/8/5KB1/8/8/P7/8/8 w - - 1 2"
    after_result = "6k1/8/5KB1/8/P7/8/8/8 b - - 0 2"
    root_key = f"1:a2a3:{PASSED_PAWN_INITIAL_FEN}:{after_a3}"
    reply_key = f"2:h8g8:{after_a3}:{after_reply}"
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
                    "fen_before": after_a3,
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
        "fen_before": PASSED_PAWN_INITIAL_FEN,
        "fen_after": after_a3,
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
        "fen_before": after_a3,
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
            "root_after": {"fen": after_a3, "ply": 1, "scope": "played_transition"},
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

        stopped = {
            **{key: value for key, value in status.items() if key != "issued_engine_work"},
            "state": "stopped",
            "progress": {
                **self.progress("stopped", issued=0, accepted=0),
                "legal_move_count": 0,
            },
            "stop_condition": "engine_execution_failed",
        }
        self.validate("position-commentary-job-status.schema.json", stopped)

        retired_causal_waves = copy.deepcopy(stopped)
        retired_causal_waves["progress"]["causal_waves"] = 0
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", retired_causal_waves)

        invented_stop_condition = copy.deepcopy(stopped)
        invented_stop_condition["stop_condition"] = "no_legal_moves"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", invented_stop_condition)

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

        retired_causal_probe = copy.deepcopy(focus)
        retired_causal_probe["progress"] = self.progress(
            "causal_probe", issued=3, accepted=2
        )
        retired_causal_probe["issued_engine_work"].update(
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
        with self.assertRaises(ContractError):
            self.validate(
                "position-commentary-job-status.schema.json", retired_causal_probe
            )

    def test_completed_response_schema_accepts_legal_passed_pawn_progress_shape(self) -> None:
        after_a3 = PASSED_PAWN_AFTER_A3
        reference_endpoint = {
            "kind": "engine_search",
            "moves": ["a2a4", "h8g8", "a4a5"],
            "win_percent_for_mover": 61,
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
                "verdict_code": "inaccuracy",
                "verdict_confidence": "engine_backed",
                "mover": "white",
                "delta": {
                    "kind": "engine_evaluation",
                    "candidate_win_percent_delta_for_mover": -8,
                },
                "reference_endpoint": reference_endpoint,
                "played_endpoint": played_endpoint,
            },
            "causal_explanations": [
                {
                    "cause_evidence_id": "cause.passed-pawn-result",
                    "kind": "passed_pawn_progress",
                    "exposure": "complementary",
                    "channels": [
                        {
                            "channel_id": "cause-channel:exact-passed-pawn-result",
                            "passed_pawn_progress_realized_after_only_legal_reply_proof": _passed_pawn_progress_realized_after_only_legal_reply_proof(after_a3),
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

        missing_exposure = copy.deepcopy(response)
        del missing_exposure["result"]["selected_move_reviews"][1]["commentary"][
            "causal_explanations"
        ][0]["exposure"]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", missing_exposure)

        no_typed_passed_pawn_progress_realized_after_only_legal_reply_proof = copy.deepcopy(response)
        del no_typed_passed_pawn_progress_realized_after_only_legal_reply_proof["result"]["selected_move_reviews"][1]["commentary"][
            "causal_explanations"
        ][0]["channels"][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", no_typed_passed_pawn_progress_realized_after_only_legal_reply_proof)

        retired_projection_fields = copy.deepcopy(response)
        retired_projection_fields["result"]["selected_move_reviews"][1]["commentary"][
            "structural_idea_units"
        ] = []
        with self.assertRaises(ContractError):
            self.validate(
                "position-commentary-response.schema.json", retired_projection_fields
            )

        detached_analysis_route = copy.deepcopy(response)
        proof = detached_analysis_route["result"]["selected_move_reviews"][1]["commentary"][
            "causal_explanations"
        ][0]["channels"][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        continuation_steps = proof["branches"][0]["steps"]
        continuation_steps[-1]["fen_before"] = continuation_steps[0]["fen_after"]
        continuation_steps[-1]["step_key"] = (
            f'{continuation_steps[-1]["ply"]}:{continuation_steps[-1]["move_uci"]}:'
            f'{continuation_steps[-1]["fen_before"]}:{continuation_steps[-1]["fen_after"]}'
        )
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", detached_analysis_route)

        skipped_ply = copy.deepcopy(response)
        proof = skipped_ply["result"]["selected_move_reviews"][1]["commentary"][
            "causal_explanations"
        ][0]["channels"][0]["passed_pawn_progress_realized_after_only_legal_reply_proof"]
        reply_step = proof["branches"][0]["steps"][1]
        reply_step["ply"] = 3
        reply_step["step_key"] = (
            f'{reply_step["ply"]}:{reply_step["move_uci"]}:'
            f'{reply_step["fen_before"]}:{reply_step["fen_after"]}'
        )
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", skipped_ply)


if __name__ == "__main__":
    unittest.main()
