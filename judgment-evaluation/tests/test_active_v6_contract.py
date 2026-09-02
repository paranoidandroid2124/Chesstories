from __future__ import annotations

import base64
import copy
import io
import json
import unittest
from collections.abc import Callable
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


def _produced_occurrence_commentary(
    fixture_name: str = "occurrence-explanation-produced.json",
) -> dict[str, object]:
    produced = json.loads(
        (
            ROOT
            / "fixtures"
            / "public-commentary-v6"
            / fixture_name
        ).read_text(encoding="utf-8")
    )
    commentaries = [
        review["commentary"]
        for review in produced["result"]["selected_move_reviews"]
        if "commentary" in review
        and review["commentary"].get("occurrence_explanations")
    ]
    if len(commentaries) != 1:
        raise AssertionError("Scala producer fixture must contain one occurrence commentary")
    return copy.deepcopy(commentaries[0])


def _produced_occurrence_explanation(
    fixture_name: str = "occurrence-explanation-produced.json",
) -> dict[str, object]:
    commentary = _produced_occurrence_commentary(fixture_name)
    explanations = commentary["occurrence_explanations"]
    if len(explanations) != 1:
        raise AssertionError("Scala producer fixture must contain one occurrence explanation")
    return explanations[0]


def _occurrence_explanation_errors(
    registry: SchemaRegistry,
    move_path: Path,
    candidate: dict[str, object],
) -> list[str]:
    errors: list[str] = []
    explanation_schema = registry.load(move_path)["$defs"]["occurrenceExplanation"]
    registry._validate(candidate, explanation_schema, move_path, "$", errors)
    if not errors:
        registry._validate_occurrence_explanation_transport(
            {"primary": {}, "occurrence_explanations": [candidate]},
            "$",
            errors,
        )
    return errors


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

    def test_public_l2_schema_exposes_only_occurrence_typed_proof_families(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        schema = registry.load(move_path)
        definitions = schema["$defs"]

        self.assertNotIn("standardCausalFacet", definitions)
        self.assertNotIn("genericCausalChannel", definitions)
        self.assertNotIn("causalFacet", definitions)
        self.assertNotIn("wrongMoveOrderCausalFacet", definitions)
        self.assertNotIn("passedPawnProgressFacet", definitions)
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
            definitions["occurrenceExplanation"]["oneOf"],
            [
                {"$ref": "#/$defs/uniqueCheckReplyOccurrenceExplanation"},
                {"$ref": "#/$defs/soleRecapturerOccurrenceExplanation"},
                {"$ref": "#/$defs/vacatedGateOccurrenceExplanation"},
                {"$ref": "#/$defs/squareReleaseRouteOccurrenceExplanation"},
                {"$ref": "#/$defs/captureExclusionOccurrenceExplanation"},
                {"$ref": "#/$defs/relocationEnablesRecaptureOccurrenceExplanation"},
                {"$ref": "#/$defs/passedPawnProgressOccurrenceExplanation"},
            ],
        )

    def test_ready_response_rejects_retired_explanation_properties(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        commentary = _produced_occurrence_commentary()
        ready = {
            "schema_version": "chesstory.move-meaning.response.v6",
            "status": "ready",
            "move_commentary": commentary,
        }
        registry.validate_document(ready, move_path, label="producer assessment commentary")

        for retired_field in ("causal_explanations", "requested_explanations"):
            retired = copy.deepcopy(ready)
            retired["move_commentary"][retired_field] = []
            with self.subTest(retired_field=retired_field), self.assertRaises(ContractError):
                registry.validate_document(retired, move_path, label="retired explanation lane")

    def test_seven_occurrence_typed_proof_contracts_are_closed_and_distinct(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        definitions = registry.load(move_path)["$defs"]
        contracts = {
            "uniqueCheckReplyOccurrenceExplanation": (
                "unique_check_reply_defender_displacement_before_capture",
                "occurrenceUniqueCheckReplyProof",
            ),
            "soleRecapturerOccurrenceExplanation": (
                "sole_recapturer_removal_before_target_capture",
                "occurrenceSoleRecapturerProof",
            ),
            "vacatedGateOccurrenceExplanation": (
                "vacated_gate_enables_unrecapturable_slider_capture",
                "occurrenceVacatedGateProof",
            ),
            "squareReleaseRouteOccurrenceExplanation": (
                "square_release_route",
                "occurrenceSquareReleaseRouteProof",
            ),
            "captureExclusionOccurrenceExplanation": (
                "capture_exclusion_move_order",
                "occurrenceCaptureExclusionMoveOrderProof",
            ),
            "relocationEnablesRecaptureOccurrenceExplanation": (
                "relocation_enables_recapture",
                "occurrenceRelocationEnablesRecaptureProof",
            ),
            "passedPawnProgressOccurrenceExplanation": (
                "passed_pawn_progress_realized_after_only_legal_reply",
                "occurrencePassedPawnProgressProof",
            ),
        }

        for outer_name, (proof_kind, proof_definition) in contracts.items():
            with self.subTest(contract=outer_name):
                outer = definitions[outer_name]
                self.assertFalse(outer["additionalProperties"])
                self.assertEqual(
                    set(outer["required"]),
                    {
                        "cause_evidence_id",
                        "subject_occurrence",
                        "proof_kind",
                        "proof",
                    },
                )
                self.assertEqual(outer["properties"]["proof_kind"], {"const": proof_kind})
                self.assertEqual(
                    outer["properties"]["proof"],
                    {"$ref": f"#/$defs/{proof_definition}"},
                )
                proof = definitions[proof_definition]
                self.assertFalse(proof["additionalProperties"])
                self.assertTrue(
                    {
                        "source_evidence_id",
                        "semantic_id",
                        "occurrence_id",
                        "dependency_fingerprint",
                        "proof_paths",
                    }.issubset(proof["required"])
                )
                self.assertNotIn("subject_occurrence", proof["properties"])

    def test_relocation_producer_fixture_retains_complete_object_continuity(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        explanation = _produced_occurrence_explanation(
            "relocation-enables-recapture-produced.json"
        )
        self.assertEqual(
            _occurrence_explanation_errors(registry, move_path, explanation), []
        )
        self.assertEqual(explanation["proof_kind"], "relocation_enables_recapture")
        proof = explanation["proof"]
        self.assertEqual(len(proof["proof_paths"]), 1)
        path = proof["proof_paths"][0]
        continuity = [
            premise
            for premise in path["premises"]
            if premise["contract"] == "object_continuity_step"
        ]
        self.assertGreater(len(continuity), 9)
        self.assertEqual(
            {premise["role"] for premise in continuity},
            {
                "relocated_branch_target_continuity",
                "retained_branch_target_continuity",
                "relocated_responder_continuity",
                "retained_responder_continuity",
                "relocated_branch_attacker_continuity",
                "retained_branch_attacker_continuity",
            },
        )
        self.assertTrue(
            any(
                premise["transition_kind"] == "secondary"
                and premise["overall_move_uci"] == "e8c8"
                and premise["selected_transition"]["from"] == "a8"
                and premise["selected_transition"]["to"] == "d8"
                for premise in continuity
            )
        )
        self.assertNotEqual(
            proof["participants"]["captured_target"]["square"],
            proof["participants"]["recapture_square"],
        )
        self.assertNotEqual(
            proof["participants"]["tracked_responder_at_seed"],
            proof["participants"]["other_recapturer"],
        )
        occurrence_keys = [
            (
                premise["role"],
                premise["branch_id"],
                premise["step_index"],
                premise["issuer_occurrence_id"],
            )
            for premise in continuity
        ]
        self.assertEqual(len(occurrence_keys), len(set(occurrence_keys)))
        for premise in continuity:
            self.assertEqual(
                premise["source_premise_ids"],
                sorted(
                    [
                        premise["issuer_evidence_id"],
                        premise["issuer_occurrence_id"],
                        f"legal-move:{premise['legal_move_semantic_id']}",
                        f"transition-footprint:{premise['transition_footprint_id']}",
                    ]
                ),
            )
        endpoint_premises = {
            premise["role"]: premise
            for premise in path["premises"]
            if premise["contract"] == "legal_move"
        }
        self.assertEqual(
            endpoint_premises["relocated_target_capture"]["capture"],
            proof["participants"]["captured_target"],
        )
        self.assertEqual(
            endpoint_premises["retained_target_capture"]["capture"],
            proof["participants"]["captured_target"],
        )
        for premise in endpoint_premises.values():
            self.assertEqual(
                premise["source_premise_ids"],
                sorted(
                    [
                        premise["issuer_evidence_id"],
                        premise["issuer_occurrence_id"],
                        f"legal-move:{premise['legal_move_semantic_id']}",
                    ]
                ),
            )

        def remove_continuity(candidate: dict[str, object]) -> None:
            candidate_path = candidate["proof"]["proof_paths"][0]
            premises = candidate_path["premises"]
            index = next(
                index
                for index, premise in enumerate(premises)
                if premise.get("role") == "relocated_branch_target_continuity"
                and premise.get("step_index") == 2
            )
            premises.pop(index)

        def duplicate_occurrence_binding(candidate: dict[str, object]) -> None:
            candidate_path = candidate["proof"]["proof_paths"][0]
            premise = next(
                premise
                for premise in candidate_path["premises"]
                if premise.get("contract") == "object_continuity_step"
            )
            duplicate = copy.deepcopy(premise)
            old_owner = f"transition-footprint:{duplicate['transition_footprint_id']}"
            duplicate["transition_footprint_id"] = "f" * 64
            duplicate["source_premise_ids"] = sorted(
                "transition-footprint:" + "f" * 64 if value == old_owner else value
                for value in duplicate["source_premise_ids"]
            )
            candidate_path["premises"].append(duplicate)

        def remove_secondary_transition(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("transition_kind") == "secondary"
            )
            del premise["selected_transition"]

        def replace_target_capture_victim(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("role") == "relocated_target_capture"
            )
            premise["capture"] = copy.deepcopy(
                candidate["proof"]["participants"]["other_recapturer"]
            )

        def detach_recapture_premise_endpoint(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("role") == "retained_other_recapture"
            )
            premise["movement"]["from"] = "a1"

        def replace_continuity_legal_owner(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("contract") == "object_continuity_step"
            )
            premise["source_premise_ids"] = sorted(
                "legal-move:" + "f" * 64
                if value.startswith("legal-move:")
                else value
                for value in premise["source_premise_ids"]
            )

        def replace_endpoint_legal_owner(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("role") == "relocated_target_capture"
            )
            premise["source_premise_ids"] = sorted(
                "legal-move:" + "f" * 64
                if value.startswith("legal-move:")
                else value
                for value in premise["source_premise_ids"]
            )

        def detach_recapture_inventory_occurrence(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("role") == "relocated_recapture_inventory"
            )
            premise["step_index"] += 1

        def detach_seed_absence_query(candidate: dict[str, object]) -> None:
            absence = candidate["proof"]["proof_paths"][0]["closed_absence_uses"][0]
            absence["query"] = "legal-move-from-to:black:a1:a2"

        def detach_seed_absence_occurrence(candidate: dict[str, object]) -> None:
            absence = candidate["proof"]["proof_paths"][0]["closed_absence_uses"][0]
            absence["after_step_index"] += 1

        def replace_recaptured_attacker(candidate: dict[str, object]) -> None:
            premises = candidate["proof"]["proof_paths"][0]["premises"]
            premise = next(
                premise
                for premise in premises
                if premise.get("role") == "relocated_responder_recapture"
            )
            premise["capture"] = copy.deepcopy(
                candidate["proof"]["participants"]["other_recapturer"]
            )

        def detach_recapture_square(candidate: dict[str, object]) -> None:
            candidate["proof"]["participants"]["recapture_square"] = "a1"

        def replace_tracked_responder_seed(candidate: dict[str, object]) -> None:
            candidate["proof"]["participants"]["tracked_responder_at_seed"] = (
                copy.deepcopy(candidate["proof"]["participants"]["other_recapturer"])
            )

        for mutate in (
            remove_continuity,
            duplicate_occurrence_binding,
            remove_secondary_transition,
            replace_target_capture_victim,
            detach_recapture_premise_endpoint,
            replace_continuity_legal_owner,
            replace_endpoint_legal_owner,
            detach_recapture_inventory_occurrence,
            detach_seed_absence_query,
            detach_seed_absence_occurrence,
            replace_recaptured_attacker,
            detach_recapture_square,
            replace_tracked_responder_seed,
        ):
            candidate = copy.deepcopy(explanation)
            mutate(candidate)
            with self.subTest(mutation=mutate.__name__):
                self.assertTrue(
                    _occurrence_explanation_errors(registry, move_path, candidate)
                )

    def test_occurrence_transport_rejects_only_broken_generic_wiring(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        explanation = _produced_occurrence_explanation()
        proof_key = "proof"

        mutations = (
            lambda candidate: candidate["subject_occurrence"].update(
                transition_evidence_id="detached.transition"
            ),
            lambda candidate: candidate["subject_occurrence"].update(
                line_id="detached.line"
            ),
            lambda candidate: candidate[proof_key]["vacating_branch"]["steps"][1].update(
                fen_before=FEN
            ),
            lambda candidate: candidate[proof_key]["proof_paths"][0]["premises"][0].update(
                branch_id="f" * 64
            ),
            lambda candidate: candidate[proof_key]["proof_paths"][0][
                "closed_state_uses"
            ][0].update(after_step_index=99),
            lambda candidate: candidate[proof_key]["proof_paths"][0][
                "closed_state_uses"
            ][0]["position"].update(fen=FEN),
        )
        for index, mutate in enumerate(mutations):
            candidate = copy.deepcopy(explanation)
            mutate(candidate)
            with self.subTest(wiring_mutation=index):
                self.assertTrue(
                    _occurrence_explanation_errors(registry, move_path, candidate)
                )

    def test_occurrence_transport_rejects_duplicate_owner_and_proof_occurrence(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        explanation = _produced_occurrence_explanation()
        duplicate = copy.deepcopy(explanation)
        errors: list[str] = []
        registry._validate_occurrence_explanation_transport(
            {
                "primary": {},
                "occurrence_explanations": [explanation, duplicate],
            },
            "$",
            errors,
        )
        self.assertTrue(any("duplicate occurrence Cause owner" in error for error in errors))
        self.assertTrue(any("duplicate typed proof occurrence" in error for error in errors))

    def test_occurrence_explanations_are_optional_but_nonempty_when_present(self) -> None:
        registry = SchemaRegistry(ROOT / "schemas")
        move_path = ROOT / "schemas" / "public-v6" / "move-meaning-response.schema.json"
        move_commentary = registry.load(move_path)["$defs"]["moveCommentary"]
        occurrence_property = move_commentary["properties"]["occurrence_explanations"]
        self.assertNotIn("occurrence_explanations", move_commentary["required"])
        errors: list[str] = []
        registry._validate([], occurrence_property, move_path, "$", errors)
        self.assertTrue(errors)

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
