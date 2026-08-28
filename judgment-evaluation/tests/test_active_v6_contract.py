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

    def test_public_move_schema_closes_runtime_structural_and_plan_shapes(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        endpoint = {
            "kind": "engine_search",
            "moves": ["a1a2"],
            "win_percent_for_mover": 50,
            "depth": 16,
        }
        ready = {
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
                    "reference_endpoint": endpoint,
                    "played_endpoint": copy.deepcopy(endpoint),
                    "presentation": "Matches.",
                },
                "structural_idea_units": [
                    {
                        "structural_evidence_id": "structural-evidence",
                        "consequence_index": 0,
                        "line_id": "line-a1a2",
                        "root_move": "a1a2",
                        "consequence_kind": "slider_reach_changed",
                        "consequence_key": "slider-reach-consequence",
                        "polarity": "neutral",
                        "state_direction": "changed",
                        "strength": 1,
                        "subjects": [
                            {
                                "kind": "slider_reach_change",
                                "side": "white",
                                "slider_before": {"piece": "rook", "square": "a1"},
                                "slider_after": {"piece": "rook", "square": "a2"},
                                "direction": {
                                    "file_step": 1,
                                    "rank_step": 0,
                                    "axis": "rank",
                                },
                                "gained": [
                                    {"square": "b2", "target": {"kind": "empty"}}
                                ],
                                "lost": [
                                    {
                                        "square": "b1",
                                        "target": {
                                            "kind": "enemy_piece",
                                            "piece": "queen",
                                        },
                                    }
                                ],
                                "binding_key": "slider-reach-binding",
                                "proof_sources": [
                                    {
                                        "proof_key": "slider-reach-proof",
                                        "evidence_id": "relation-evidence",
                                    }
                                ],
                            }
                        ],
                        "targets": [],
                    },
                    {
                        "structural_evidence_id": "structural-evidence",
                        "consequence_index": 1,
                        "line_id": "line-a1a2",
                        "root_move": "a1a2",
                        "consequence_kind": "geometric_control_set_changed",
                        "consequence_key": "control-set-consequence",
                        "polarity": "neutral",
                        "state_direction": "changed",
                        "strength": 1,
                        "subjects": [
                            {
                                "kind": "geometric_control_set_change",
                                "root_movement": {
                                    "side": "white",
                                    "from": "a1",
                                    "to": "a2",
                                    "piece_before": "rook",
                                    "piece_after": "rook",
                                },
                                "controlling_side": "white",
                                "target_square": "b2",
                                "before_target": {"kind": "empty"},
                                "after_target": {"kind": "empty"},
                                "before_controllers": [
                                    {"piece": "rook", "square": "a1"}
                                ],
                                "after_controllers": [
                                    {"piece": "rook", "square": "a2"}
                                ],
                                "removed_controllers": [
                                    {"piece": "rook", "square": "a1"}
                                ],
                                "established_controllers": [
                                    {"piece": "rook", "square": "a2"}
                                ],
                                "binding_key": "control-set-binding",
                                "proof_sources": [
                                    {
                                        "proof_key": "control-set-proof",
                                        "evidence_id": "control-relation-evidence",
                                    }
                                ],
                            }
                        ],
                        "targets": [],
                    },
                ],
                "causal_explanations": [
                    {
                        "kind": "single_cause",
                        "presentation": "Exact plan result.",
                        "facets": [
                            {
                                "facet_role": "lead",
                                "cause_evidence_id": "cause-evidence",
                                "kind": "plan_improvement",
                                "proof_confidence": "legal_replay_verified",
                                "effect_mode": "played_value",
                                "exposure": "primary",
                                "source_side": "candidate",
                                "event_move": "a2a3",
                                "comparison_kind": "played_vs_best",
                                "only_move_qualifiers": [],
                                "channels": [
                                    {
                                        "channel_id": "channel-1",
                                        "causal_signature": "plan-result-channel",
                                        "direct_change": "occurred",
                                        "played_change": "occurred",
                                        "actor": {
                                            "move_uci": "a2a3",
                                            "side": "white",
                                            "piece": "pawn",
                                            "from": "a2",
                                            "to": "a3",
                                        },
                                        "targets": [
                                            {"kind": "relation", "key": "passed-status"}
                                        ],
                                        "mechanisms": [],
                                        "consequences": [],
                                        "witnesses": [],
                                        "proof_line_moves": ["a2a3"],
                                        "proof_segment": {
                                            "terminal_relation": "realizes_plan_result",
                                            "steps": [
                                                {
                                                    "ply_offset": 0,
                                                    "move_uci": "a2a3",
                                                    "role": "root_action",
                                                    "plan_event": {
                                                        "goal_kind": "passed_pawn_manufacture",
                                                        "actor_side": "white",
                                                        "actor_role": "pawn",
                                                        "actor_after_role": "pawn",
                                                        "actor_from": "a2",
                                                        "actor_to": "a3",
                                                        "legal_move_relation": "b" * 64,
                                                    },
                                                }
                                            ],
                                        },
                                    }
                                ],
                            }
                        ],
                    }
                ],
            },
        }
        registry.validate_document(ready, move_path, label="runtime v6 shapes")

        invalid_documents = []
        missing_structural_units = copy.deepcopy(ready)
        del missing_structural_units["move_commentary"]["structural_idea_units"]
        invalid_documents.append(missing_structural_units)

        legacy_plan_event = copy.deepcopy(ready)
        legacy_plan_event["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["proof_segment"]["steps"][0]["plan_event"] = {
            "goal_kind": "passed_pawn_manufacture",
            "actor_from": "a2",
            "actor_to": "a3",
            "targets": [],
            "results": [],
        }
        invalid_documents.append(legacy_plan_event)

        stale_cause = copy.deepcopy(ready)
        stale_cause["move_commentary"]["causal_explanations"][0]["facets"][0][
            "kind"
        ] = "wrong_recapturer"
        invalid_documents.append(stale_cause)

        stale_object = copy.deepcopy(ready)
        stale_object["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["targets"][0]["kind"] = "motif"
        invalid_documents.append(stale_object)

        stale_terminal = copy.deepcopy(ready)
        stale_terminal["move_commentary"]["causal_explanations"][0]["facets"][0][
            "channels"
        ][0]["proof_segment"]["terminal_relation"] = "restricts_opponent_resource"
        invalid_documents.append(stale_terminal)

        for document in invalid_documents:
            with self.subTest(document=document), self.assertRaises(ContractError):
                registry.validate_document(document, move_path, label="closed v6 shape")

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
                "structural_idea_units": [],
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
                    "presentation": "Matches.",
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
            "structural_idea_units": [],
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
                "presentation": "Matches.",
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
