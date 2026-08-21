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
    def _probe(identifier: str, *, resource: bool = False) -> dict[str, object]:
        probe: dict[str, object] = {
            "id": identifier,
            "fen": FEN,
            "moves": [],
            "depth": 16,
            "purpose": "reply_multipv",
            "multiPv": 3,
            "candidateMove": "e2e4",
            "variationHash": "a" * 64,
            "horizon": "ply:2147483647",
        }
        if resource:
            probe.update(
                {
                    "moves": ["e2e4"],
                    "multiPv": 1,
                    "opponentResourceMove": "e2e4",
                }
            )
            del probe["horizon"]
        return probe

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
            "schema_version": "chesstory.move-meaning.response.v4",
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
                    "schema_version": "chesstory.move-meaning.response.v4",
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

    def test_probe_and_endpoint_bounds_reject_before_execution(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v4" / "move-meaning-response.schema.json"
        horizon_probe = self._probe("horizon")
        resource_probe = self._probe("resource", resource=True)
        self.assertEqual(_validate_probe_request(horizon_probe)["id"], "horizon")
        self.assertEqual(_validate_probe_request(resource_probe)["id"], "resource")

        bad_multipv = copy.deepcopy(horizon_probe)
        bad_multipv["multiPv"] = 4
        with self.assertRaises(ContractError):
            _validate_probe_request(bad_multipv)
        bad_horizon = copy.deepcopy(horizon_probe)
        bad_horizon["horizon"] = "ply:2147483648"
        with self.assertRaises(ContractError):
            _validate_probe_request(bad_horizon)

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
            "schema_version": "chesstory.move-meaning.response.v4",
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
        bad_probe_count = {
            "schema_version": "chesstory.move-meaning.response.v4",
            "status": "engine_work_required",
            "probe_requests": [self._probe(f"probe-{index}") for index in range(13)],
        }
        invalid_documents.append(bad_probe_count)
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

    def test_move_choices_draw_claims_require_the_server_presentation_pair(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        position_path = (
            ROOT
            / "schemas"
            / "public-commentary-v4"
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
                "presentation": "Matches.",
            }
        }
        base = {
            "schema_version": "chesstory.position-commentary.response.v4",
            "engine_profile": "sf18-smallnet-t2-h16-v1",
            "request_id": "move-choices-pair",
            "current_fen": FEN,
            "result": {
                "kind": "move_choices",
                "move_commentaries": [
                    {
                        "legal_move_index": 0,
                        "move_uci": "e2e4",
                        "commentary": commentary,
                    },
                    {
                        "legal_move_index": 1,
                        "move_uci": "d2d4",
                        "commentary": copy.deepcopy(commentary),
                    },
                ],
            },
        }
        claims = [{"rule": "threefold_repetition", "availability": "available_now"}]
        registry.validate_document(base, position_path, label="move choices without claims")
        paired = copy.deepcopy(base)
        paired["result"].update(
            {
                "draw_claims": claims,
                "presentation": "A draw can be claimed now by threefold repetition.",
            }
        )
        registry.validate_document(paired, position_path, label="paired move choice claims")

        invalid_results = []
        claims_only = copy.deepcopy(base)
        claims_only["result"]["draw_claims"] = claims
        invalid_results.append(claims_only)
        presentation_only = copy.deepcopy(base)
        presentation_only["result"]["presentation"] = "A draw can be claimed now by threefold repetition."
        invalid_results.append(presentation_only)
        empty_presentation = copy.deepcopy(paired)
        empty_presentation["result"]["presentation"] = ""
        invalid_results.append(empty_presentation)
        malformed_claims = copy.deepcopy(paired)
        malformed_claims["result"]["draw_claims"] = [{"rule": "threefold_repetition"}]
        invalid_results.append(malformed_claims)
        extra_key = copy.deepcopy(paired)
        extra_key["result"]["extra"] = True
        invalid_results.append(extra_key)
        for document in invalid_results:
            with self.subTest(document=document), self.assertRaises(ContractError):
                registry.validate_document(document, position_path, label="unpaired move choice claims")

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
                    "schema_version": "chesstory.move-meaning.response.v4",
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
