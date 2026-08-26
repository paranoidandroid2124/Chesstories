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
PROFILE = "sf18-smallnet-t2-h16-v1"


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

    def test_completed_response_keeps_causal_objects_and_their_exact_proof_line(self) -> None:
        endpoint = {
            "kind": "engine_search",
            "moves": ["e2e4", "e7e5"],
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
                "reference_endpoint": endpoint,
                "played_endpoint": copy.deepcopy(endpoint),
                "presentation": "e2e4 matches e2e4.",
            },
            "causal_explanations": [
                {
                    "kind": "single_cause",
                    "presentation": "Exact selected cause.",
                    "facets": [
                        {
                            "facet_role": "lead",
                            "cause_evidence_id": "cause.center",
                            "kind": "center_control_gain",
                            "proof_confidence": "engine_backed",
                            "effect_mode": "played_value",
                            "exposure": "primary",
                            "source_side": "candidate",
                            "event_move": "e2e4",
                            "comparison_kind": "played_vs_best",
                            "only_move_qualifiers": [
                                {
                                    "comparison_evidence_id": "comparison.played-vs-best",
                                    "cause_evidence_id": "cause.center",
                                    "reference_line_id": "line.best.exact-a",
                                    "reference_line_role": "best_reference",
                                    "reference_line_rank": 1,
                                    "reference_line_root_move": "e2e4",
                                    "relation": "same_channel_association",
                                },
                                {
                                    "comparison_evidence_id": "comparison.played-vs-best",
                                    "cause_evidence_id": "cause.center",
                                    "reference_line_id": "line.best.exact-b",
                                    "reference_line_role": "best_reference",
                                    "reference_line_rank": 1,
                                    "reference_line_root_move": "e2e4",
                                    "relation": "same_channel_association",
                                },
                            ],
                            "channels": [
                                {
                                    "channel_id": "cause-channel:exact-center-pressure",
                                    "causal_signature": "cause.center:pressure",
                                    "direct_change": "occurred",
                                    "played_change": "occurred",
                                    "actor": {
                                        "move_uci": "e2e4",
                                        "side": "white",
                                        "piece": "pawn:e2",
                                        "from": "e2",
                                        "to": "e4",
                                    },
                                    "targets": [{"kind": "square", "key": "e4"}],
                                    "mechanisms": [{"kind": "relation", "key": "center-control"}],
                                    "consequences": [{"kind": "consequence", "key": "space"}],
                                    "witnesses": [{"kind": "line", "key": "played"}],
                                    "proof_line_moves": ["e2e4", "e7e5"],
                                    "proof_segment": {
                                        "terminal_relation": "produces_line_consequence",
                                        "steps": [
                                            {
                                                "ply_offset": 0,
                                                "move_uci": "e2e4",
                                                "role": "root_action",
                                            },
                                            {
                                                "ply_offset": 1,
                                                "move_uci": "e7e5",
                                                "role": "terminal_event",
                                            },
                                        ],
                                    },
                                }
                            ],
                        }
                    ],
                }
            ],
            "responsibility_links": [
                {
                    "resource_cause_evidence_id": "cause.resource",
                    "liability_cause_evidence_ids": [
                        "cause.liability.a",
                        "cause.liability.b",
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
            "current_fen": INITIAL_FEN,
            "focus": self.focus(),
            "progress": self.progress("completed", issued=3, accepted=3),
            "result": {
                "kind": "selected_move_choices",
                "selected_move_reviews": [
                    {
                        "legal_move_index": 0,
                        "move_uci": "e2e4",
                        "selection": {"roles": ["best", "played"], "root_rank": 1},
                        "commentary": commentary,
                    }
                ],
            },
        }
        self.validate("position-commentary-response.schema.json", response)

        no_proof_line = copy.deepcopy(response)
        del no_proof_line["result"]["selected_move_reviews"][0]["commentary"][
            "causal_explanations"
        ][0]["facets"][0]["channels"][0]["proof_line_moves"]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", no_proof_line)

        missing_qualifier_array = copy.deepcopy(response)
        del missing_qualifier_array["result"]["selected_move_reviews"][0][
            "commentary"
        ]["causal_explanations"][0]["facets"][0]["only_move_qualifiers"]
        with self.assertRaises(ContractError):
            self.validate(
                "position-commentary-response.schema.json", missing_qualifier_array
            )

        collapsed_qualifier = copy.deepcopy(response)
        qualifier_facet = collapsed_qualifier["result"]["selected_move_reviews"][0][
            "commentary"
        ]["causal_explanations"][0]["facets"][0]
        qualifier_facet["only_move_qualifiers"] = qualifier_facet[
            "only_move_qualifiers"
        ][0]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", collapsed_qualifier)

        duplicate_liability = copy.deepcopy(response)
        duplicate_liability["result"]["selected_move_reviews"][0]["commentary"][
            "responsibility_links"
        ][0]["liability_cause_evidence_ids"] = [
            "cause.liability.a",
            "cause.liability.a",
        ]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", duplicate_liability)

        heuristic = copy.deepcopy(response)
        heuristic["result"]["selected_move_reviews"][0]["commentary"][
            "causal_explanations"
        ][0]["facets"][0]["proof_confidence"] = "heuristic"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-response.schema.json", heuristic)


if __name__ == "__main__":
    unittest.main()
