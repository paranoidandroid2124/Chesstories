from __future__ import annotations

import copy
import json
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

    def test_producer_best_choice_fixture_matches_the_public_contract(self) -> None:
        fixture_path = ROOT / "fixtures" / "public-commentary-v6" / "best-choice-produced.json"
        fixture = json.loads(fixture_path.read_text(encoding="utf-8"))

        self.validate("position-commentary-response.schema.json", fixture)
        selected = fixture["result"]["selected_move_reviews"]
        self.assertEqual(len(selected), 1)
        self.assertEqual(selected[0]["selection"]["roles"], ["best", "played"])
        self.assertEqual(selected[0]["commentary"]["primary"]["kind"], "best_choice")

    def test_producer_occurrence_explanation_fixtures_match_both_public_contracts(self) -> None:
        for fixture_name in (
            "occurrence-explanation-produced.json",
            "relocation-enables-recapture-produced.json",
        ):
            with self.subTest(fixture=fixture_name):
                fixture_path = (
                    ROOT / "fixtures" / "public-commentary-v6" / fixture_name
                )
                fixture = json.loads(fixture_path.read_text(encoding="utf-8"))

                self.validate("position-commentary-response.schema.json", fixture)
                selected = fixture["result"]["selected_move_reviews"]
                commentaries = [
                    review["commentary"]
                    for review in selected
                    if "commentary" in review
                    and "occurrence_explanations" in review["commentary"]
                ]
                self.assertEqual(len(commentaries), 1)
                commentary = commentaries[0]
                self.assertNotIn("causal_explanations", commentary)
                self.assertTrue(commentary["occurrence_explanations"])
                self.registry.validate_document(
                    {
                        "schema_version": "chesstory.move-meaning.response.v6",
                        "status": "ready",
                        "move_commentary": commentary,
                    },
                    ROOT
                    / "schemas"
                    / "public-v6"
                    / "move-meaning-response.schema.json",
                    label=f"Scala-produced occurrence explanation: {fixture_name}",
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
