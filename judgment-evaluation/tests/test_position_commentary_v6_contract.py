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

    def test_certified_observation_fixtures_cross_the_same_job_contract(self) -> None:
        path = ROOT / "fixtures/public-commentary-v6/certified-observations-produced.json"
        fixtures = json.loads(path.read_text(encoding="utf-8"))
        for fixture in fixtures:
            with self.subTest(name=fixture["name"]):
                self.validate("position-commentary-job-status.schema.json", fixture["response"])
        kinds = {
            fact["kind"] for fixture in fixtures
            for fact in fixture["response"]["observations"]["statements"]
        }
        self.assertTrue({"piece_control", "recapture_resources", "check_responses", "check_response",
            "pawn_structure", "structural_change", "stalemate", "castling", "promotion"} <= kinds)
        malformed = copy.deepcopy(next(f for f in fixtures if f["name"] == "moving-target")["response"])
        fact = next(s for s in malformed["observations"]["statements"] if s["kind"] == "piece_control")
        fact["after"]["state"] = "safe"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", malformed)

    def test_resource_and_finite_line_fixtures_cross_the_job_contract(self) -> None:
        path = ROOT / "fixtures/public-commentary-v6/resources-produced.json"
        fixtures = json.loads(path.read_text(encoding="utf-8"))
        self.assertEqual(len(fixtures), 22)
        for fixture in fixtures:
            with self.subTest(name=fixture["name"]):
                self.validate("position-commentary-job-status.schema.json", fixture["response"])

        quiet = next(f for f in fixtures if f["name"] == "quiet-line")["response"]
        material = quiet["observations"]["paths"][0]["line_summary"]["material"]
        self.assertEqual(material["events"], [])
        self.assertEqual(material["net_cp"], 0)
        malformed = copy.deepcopy(quiet)
        malformed["observations"]["paths"][0]["line_summary"]["material"]["net_cp"] = 100
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", malformed)
        malformed = copy.deepcopy(quiet)
        malformed["observations"]["paths"][0]["line_summary"]["scope"] = "forced_gain"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", malformed)

    def test_resource_contract_does_not_turn_geometry_into_legality(self) -> None:
        path = ROOT / "fixtures/public-commentary-v6/resources-produced.json"
        fixtures = json.loads(path.read_text(encoding="utf-8"))
        fixture = next(f["response"] for f in fixtures if f["name"] == "quiet-line")
        position = fixture["observations"]["positions"][0]
        self.assertTrue(position["legal_moves"]["complete"])
        self.assertTrue(position["legal_moves"]["moves"])
        for key, value in (("legality", "safe"), ("legal_moves", []), ("source_id", "unscoped-source")):
            malformed = copy.deepcopy(fixture)
            malformed["observations"]["positions"][0]["pawn_advances"]["paths"][0][key] = value
            with self.subTest(key=key), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", malformed)
        malformed = copy.deepcopy(fixture)
        malformed["observations"]["positions"][0]["legal_moves"]["moves"][0]["destination"] = "a9"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", malformed)

    def test_current_pawn_and_file_states_preserve_unknown_without_new_chess_claims(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/resources-produced.json").read_text(encoding="utf-8"))
        fixture = next(f["response"] for f in fixtures if f["name"] == "unchanged-pawn-states")
        partial = copy.deepcopy(fixture)
        position = partial["observations"]["positions"][0]
        position["pawn_states"]["complete"] = False
        position["pawn_states"]["pawns"][0]["passed"] = None
        position["pawn_files"]["complete"] = False
        position["pawn_files"]["files"] = position["pawn_files"]["files"][1:]
        self.validate("position-commentary-job-status.schema.json", partial)
        for family, collection, key, invalid in (
            ("pawn_states", "pawns", "passed", "unknown"),
            ("pawn_files", "files", "occupancy", "safe_for_rook"),
            ("legal_moves", "moves", "captured_square", "d9"),
        ):
            malformed = copy.deepcopy(fixture)
            malformed["observations"]["positions"][0][family][collection][0][key] = invalid
            with self.subTest(key=key), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", malformed)

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

    @staticmethod
    def proposal() -> dict[str, object]:
        return {
            "id": "proposal:contract",
            "origin": {
                key: {"kind": kind, "producer": "contract-test", "source_refs": []}
                for key, kind in (
                    ("question", "author"), ("candidates", "user"),
                    ("priority", "generator"), ("evidence", "none"),
                )
            },
            "requirements": [
                {"id": "observe", "kind": "observe_move", "prefix_moves_uci": [], "move_uci": "b1c3"},
                {"id": "compare", "kind": "compare_moves", "prefix_moves_uci": ["e2e4"],
                 "reference_move": "e7e5", "alternative_move": "c7c5", "expected_relation": "equal"},
                {"id": "meaning", "kind": "uninterpreted", "text": "Does this prepare a pawn break?"},
            ],
        }

    def test_proposal_input_union_is_closed_under_the_existing_schema_validator(self) -> None:
        request = {
            "schema_version": "chesstory.position-commentary.job-request.v6",
            "request_id": "proposal-contract", "variant": "standard",
            "initial_fen": INITIAL_FEN, "move_prefix_uci": [], "current_fen": INITIAL_FEN,
            "focus": self.focus(), "engine_profile": PROFILE, "proposals": [self.proposal()],
        }
        self.validate("position-commentary-job-request.schema.json", request)
        # Cross-variant and result fields must stay closed when shared $ref fields are composed.
        for index, extra in (
            (0, {"reference_move": "g1f3"}),
            (0, {"state": "observed"}),
            (1, {"move_uci": "b1c3"}),
            (1, {"expected_relation": "best"}),
            (2, {"prefix_moves_uci": []}),
        ):
            malformed = copy.deepcopy(request)
            malformed["proposals"][0]["requirements"][index].update(extra)
            with self.subTest(index=index, extra=extra), self.assertRaises(ContractError):
                self.validate("position-commentary-job-request.schema.json", malformed)
        for extra in ({"admission": "internal"}, {"origin_admission": "internal"}):
            malformed = copy.deepcopy(request)
            malformed["proposals"][0].update(extra)
            with self.subTest(extra=extra), self.assertRaises(ContractError):
                self.validate("position-commentary-job-request.schema.json", malformed)

    def test_proposal_output_preserves_individual_readings_and_rejects_unbound_success_shapes(self) -> None:
        proposal = self.proposal()
        proposal["admission"] = "submitted"
        readings = proposal["requirements"]
        for reading in readings:
            reading.update({"stop_reason": None, "answer": None, "line_id": None,
                            "occurrence_id": None, "position_id": None, "work_id": None})
        readings[0].update({"state": "observed", "line_id": "line:1", "occurrence_id": "edge:1",
                            "position_id": "position:1"})
        evaluation = {"source": "response_comparison", "kind": "engine_search", "depth": 16,
                      "white_score": {"kind": "cp", "value": 0}}
        answer = {
            "work_id": "work:1", "execution_key_sha256": "a" * 64, "engine_profile": PROFILE,
            "required_depth": 16, "reference_depth": 16, "alternative_depth": 16,
            "evaluation_relation": "equal", "reference_line_id": "line:2", "alternative_line_id": "line:3",
            "reference_occurrence_id": "edge:2", "alternative_occurrence_id": "edge:3",
            "reference_evaluation": evaluation, "alternative_evaluation": evaluation,
        }
        readings[1].update({"state": "supported", "answer": answer, "work_id": "work:1"})
        readings[2].update({"state": "unsupported_reading", "stop_reason": "uninterpreted_hypothesis"})
        # Schema shape only: these synthetic IDs and scores do not establish chess evidence.
        status = {
            "schema_version": "chesstory.position-commentary.job-status.v6",
            "request_id": "proposal-contract", "job_id": "a" * 32, "engine_profile": PROFILE,
            "variant": "standard", "deadline_epoch_ms": 10000, "focus": self.focus(),
            "state": "stopped", "progress": self.progress("stopped", issued=2, accepted=2),
            "stop_condition": "cancelled", "observations": {"statements": [], "paths": [],
                "selection": {"version": 1, "candidates": [], "scenes": [],
                              "proposals": [proposal], "local_expansions_used": 0}},
        }
        self.validate("position-commentary-job-status.schema.json", status)
        for index, extra in (
            (0, {"reference_move": "g1f3"}),
            (0, {"line_id": None}),
            (1, {"answer": None}),
            (1, {"move_uci": "b1c3"}),
            (2, {"state": "observed"}),
        ):
            malformed = copy.deepcopy(status)
            malformed["observations"]["selection"]["proposals"][0]["requirements"][index].update(extra)
            with self.subTest(index=index, extra=extra), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", malformed)
        malformed = copy.deepcopy(status)
        del malformed["observations"]["selection"]["local_expansions_used"]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", malformed)

    def test_fact_proposal_input_keeps_selectors_and_sources_closed(self) -> None:
        fact = {"id": "fact", "kind": "fact", "path": {"binding": "route",
                "source": {"kind": "declared", "moves_uci": ["e2e4"]}},
                "predicate": {"kind": "presence", "subject": {"side": "white", "role": "pawn", "square": "e2"}},
                "at": {"kind": "ply", "ply_count": 0}, "expected": "present"}
        proposal = self.proposal()
        proposal["requirements"] += [fact,
            {**copy.deepcopy(fact), "id": "later", "at": {"kind": "event", "event": "move", "ordinal": 1,
                "phase": "after", "from_ply": 0, "through_ply": 2}},
            {"id": "time", "kind": "fact_comparison", "left_requirement_id": "fact",
                "right_requirement_id": "later", "relation": "before"},
            {"id": "both", "kind": "fact_group", "mode": "all", "members": ["fact", "time"]}]
        request = {"schema_version": "chesstory.position-commentary.job-request.v6", "request_id": "fact-contract",
            "variant": "standard", "initial_fen": INITIAL_FEN, "move_prefix_uci": [], "current_fen": INITIAL_FEN,
            "focus": self.focus(), "engine_profile": PROFILE, "proposals": [proposal]}
        self.validate("position-commentary-job-request.schema.json", request)
        for source in ({"kind": "comparison", "requirement_id": "compare", "arm": "reference"},
                       {"kind": "premise", "inquiry_id": "inquiry:1", "premise_id": "premise:1"}):
            variant = copy.deepcopy(request)
            variant["proposals"][0]["requirements"][3]["path"]["source"] = source
            self.validate("position-commentary-job-request.schema.json", variant)
        for extra in ({"expected": "best"}, {"predicate": {"kind": "presence", "side": "white"}},
                      {"result": {"state": "supported"}}, {"at": {"kind": "ply", "ply_count": 81}},
                      {"path": {"binding": "route", "source": {"kind": "premise", "inquiry_id": "inquiry:1",
                          "premise_id": "premise:1", "witness_line_ids": ["foreign"]}}}):
            malformed = copy.deepcopy(request)
            malformed["proposals"][0]["requirements"][3].update(extra)
            with self.subTest(extra=extra), self.assertRaises(ContractError):
                self.validate("position-commentary-job-request.schema.json", malformed)

    def test_fact_proposal_output_keeps_partial_results_separate_from_engine_answers(self) -> None:
        # Shape checks use synthetic IDs; exact ownership is checked by the runtime and transport decoder.
        reading = {"id": "fact", "kind": "fact", "path": {"binding": "route", "extend": True,
                "source": {"kind": "declared", "moves_uci": ["e2e4"]}},
            "predicate": {"kind": "presence", "subject": {"side": "white", "role": "pawn", "square": "e2"}},
            "at": {"kind": "ply", "ply_count": 3}, "expected": "present", "state": "stopped",
            "stop_reason": "investigation_limit", "answer": None, "line_id": None, "occurrence_id": None,
            "position_id": None, "work_id": None, "result": {"state": "partial", "samples": [], "proofs": [],
                "needs": [{"kind": "continuation", "reason": "path_end", "prefix_moves_uci": ["e2e4"],
                    "requirement_id": None, "horizon": 3}], "reason": None}}
        proposal = self.proposal()
        proposal.update({"admission": "submitted", "requirements": [reading]})
        status = {"schema_version": "chesstory.position-commentary.job-status.v6", "request_id": "fact-contract",
            "job_id": "a" * 32, "engine_profile": PROFILE, "variant": "standard", "deadline_epoch_ms": 10000,
            "focus": self.focus(), "state": "stopped", "progress": self.progress("stopped", issued=1, accepted=1),
            "stop_condition": "cancelled", "observations": {"statements": [], "paths": [], "selection": {
                "version": 1, "candidates": [], "scenes": [], "proposals": [proposal], "local_expansions_used": 0}}}
        self.validate("position-commentary-job-status.schema.json", status)
        for state in ("partial", "unavailable_input", "not_applicable"):
            reading["state"] = reading["result"]["state"] = state
            self.validate("position-commentary-job-status.schema.json", status)
        reading["state"] = reading["result"]["state"] = "supported"
        reading["stop_reason"] = None
        sample = {"id": "sample:1", "line_id": "line:1", "binding": "route", "moves_uci": ["e2e4"],
            "from_ply": 1, "through_ply": 1, "position_ids": ["position:1"], "occurrence_ids": ["edge:1"],
            "value": "present", "state": "known", "source_ids": ["source:1"], "reason": None}
        reading["result"].update({"samples": [sample], "proofs": [{"bindings": {"route": ["e2e4"]},
            "sample_ids": ["sample:1"], "requirement_ids": ["fact"]}], "needs": []})
        self.validate("position-commentary-job-status.schema.json", status)
        for extra in ({"answer": {"state": "supported"}}, {"result": {"state": "supported"}},
                      {"state": "partial"}, {"result": {**reading["result"], "proofs": []}},
                      {"result": {**reading["result"], "samples": [{**sample, "state": "unavailable_input"}]}},
                      {"result": {**reading["result"], "samples": [{**sample, "actor": "invented"}]}}):
            malformed = copy.deepcopy(status)
            malformed["observations"]["selection"]["proposals"][0]["requirements"][0].update(extra)
            with self.subTest(extra=extra), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", malformed)

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
            "stop_condition": "search_budget_exhausted",
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
        failed_report = {
            **{key: value for key, value in report.items() if key != "outcome"},
            "outcome": {
                "kind": "executor_failed",
                "failure_code": "search_budget_exhausted",
            },
        }
        self.validate("position-commentary-engine-work-report.schema.json", failed_report)
        failed_report["outcome"]["failure_code"] = "unknown_failure"
        with self.assertRaises(ContractError):
            self.validate("position-commentary-engine-work-report.schema.json", failed_report)
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

        investigation = copy.deepcopy(focus)
        investigation["progress"] = self.progress("investigation_comparison", issued=3, accepted=2)
        investigation.update(
            assessment={"kind": "move_observations"},
            assessment_state="needs_refresh",
            assessment_scope="root_search",
        )
        investigation["issued_engine_work"]["purpose"] = "investigation_comparison"
        for depth in (16, 24, 100):
            investigation["issued_engine_work"]["search_limits"]["depth"] = depth
            with self.subTest(investigation_depth=depth):
                self.validate("position-commentary-job-status.schema.json", investigation)
        for depth in (15, 101):
            invalid = copy.deepcopy(investigation)
            invalid["issued_engine_work"]["search_limits"]["depth"] = depth
            with self.subTest(investigation_depth=depth), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", invalid)
        for field, value in (("nodes", 5_000_000), ("movetime_ms", 5_000), ("multi_pv", 4)):
            invalid = copy.deepcopy(investigation)
            invalid["issued_engine_work"]["search_limits"][field] = value
            with self.subTest(investigation_limit=field), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", invalid)

        response = copy.deepcopy(investigation)
        response["progress"] = self.progress("response_comparison", issued=3, accepted=2)
        response["issued_engine_work"].update(
            purpose="response_comparison",
            engine_position_moves_uci=["e2e4"],
            search_fen=AFTER_E4,
            search_anchor={
                "root_move": "e2e4", "root_occurrence_id": "occurrence:e4",
                "position_id": "position:after-e4", "prefix_moves_uci": ["e2e4"],
            },
            root_restriction={"kind": "restricted", "moves_uci": ["e7e5", "c7c5"]},
        )
        response["issued_engine_work"]["search_limits"]["depth"] = 16
        self.validate("position-commentary-job-status.schema.json", response)
        for verified_work in (base, focus, response):
            deeper = copy.deepcopy(verified_work)
            deeper["issued_engine_work"]["search_limits"]["depth"] = 24
            with self.subTest(purpose=deeper["issued_engine_work"]["purpose"]):
                self.validate("position-commentary-job-status.schema.json", deeper)
        for first_work in (base, focus):
            preview = copy.deepcopy(first_work)
            preview["issued_engine_work"]["search_limits"].update(depth=8, nodes=2_000_000, movetime_ms=2_500)
            preview["issued_engine_work"]["max_search_elapsed_ms"] = 3_500
            self.validate("position-commentary-job-status.schema.json", preview)
            preview["issued_engine_work"]["search_limits"]["depth"] = 9
            with self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", preview)

        pending = copy.deepcopy(base)
        pending.update(assessment={"kind": "move_observations"}, assessment_state="current", assessment_scope="root_search")
        pending["progress"] = self.progress("root_search", issued=2, accepted=1)
        pending["progress"]["selected_commentaries_completed"] = 1
        pending["verification"] = dict(state="pending", target_depth=16, reason="initial_verification",
            trigger_work_ids=[], completed_depth=None, stop_reason=None)
        self.validate("position-commentary-job-status.schema.json", pending)
        for bad_state in ("complete", "stopped", "not_required"):
            invalid = copy.deepcopy(pending)
            invalid["verification"]["state"] = bad_state
            with self.subTest(verification=bad_state), self.assertRaises(ContractError):
                self.validate("position-commentary-job-status.schema.json", invalid)
        del pending["verification"]
        with self.assertRaises(ContractError):
            self.validate("position-commentary-job-status.schema.json", pending)

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
