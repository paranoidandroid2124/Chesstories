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


def _produced_commentary() -> dict[str, object]:
    produced = json.loads(
        (
            ROOT
            / "fixtures"
            / "public-commentary-v6"
            / "best-choice-produced.json"
        ).read_text(encoding="utf-8")
    )
    commentaries = [
        review["commentary"]
        for review in produced["result"]["selected_move_reviews"]
        if "commentary" in review
    ]
    if len(commentaries) != 1:
        raise AssertionError("Scala producer fixture must contain one commentary")
    return copy.deepcopy(commentaries[0])


def _definition_refs(value: object) -> set[str]:
    if isinstance(value, dict):
        direct = {
            ref.removeprefix("#/$defs/")
            for ref in [value.get("$ref")]
            if isinstance(ref, str) and ref.startswith("#/$defs/")
        }
        return direct | set().union(*(_definition_refs(child) for child in value.values()), set())
    if isinstance(value, list):
        return set().union(*(_definition_refs(child) for child in value), set())
    return set()


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

    def test_public_move_schema_has_no_relation_lane(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        schema = registry.load(move_path)
        definitions = schema["$defs"]

        root = {key: value for key, value in schema.items() if key != "$defs"}
        reached: set[str] = set()
        pending = list(_definition_refs(root))
        while pending:
            current = pending.pop()
            self.assertIn(current, definitions)
            if current in reached:
                continue
            reached.add(current)
            pending.extend(_definition_refs(definitions[current]) - reached)
        self.assertEqual(reached, set(definitions))

        self.assertEqual(
            schema["oneOf"],
            [
                {"$ref": "#/$defs/errorResponse"},
                {"$ref": "#/$defs/readyResponse"},
            ],
        )
        self.assertEqual(
            set(definitions["moveCommentary"]["properties"]), {"primary"}
        )

    def test_ready_response_rejects_retired_explanation_properties(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        commentary = _produced_commentary()
        ready = {
            "schema_version": "chesstory.move-meaning.response.v6",
            "status": "ready",
            "move_commentary": commentary,
        }
        registry.validate_document(ready, move_path, label="producer assessment commentary")

        for retired_field in (
            "causal_explanations",
            "requested_explanations",
            "occurrence_explanations",
            "occurrence_relations",
        ):
            retired = copy.deepcopy(ready)
            retired["move_commentary"][retired_field] = []
            with self.subTest(retired_field=retired_field), self.assertRaises(ContractError):
                registry.validate_document(retired, move_path, label="retired explanation lane")

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
