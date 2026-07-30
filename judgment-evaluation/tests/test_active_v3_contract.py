from __future__ import annotations

import base64
import copy
import io
import json
import unittest
from pathlib import Path
from unittest.mock import patch

from chesstory_eval.cause_audit import (
    ACTUAL_VIEW_SCHEMA_VERSION,
    TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    TYPED_RUNTIME_OBSERVATION_SCHEMA,
    _actual_view,
    _run_cause_audit_for_contract,
    _strict_actual_view,
)
from chesstory_eval.hashing import sha256_bytes, sha256_json
from chesstory_eval.model import ContractError, IntegrityError
from chesstory_eval.native_diagnostic import (
    NATIVE_HASH_CONTRACT,
    native_canonical_json,
    native_sha256_json,
)
from chesstory_eval.probe_completion import (
    _RuntimeExchange,
    _RuntimeJsonlSession,
    _runtime_io_document,
    _validate_runtime_observation,
    _verified_response_jsonl_bytes,
)
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
CAUSE_V3_SCHEMA = ROOT / "schemas" / "cause-audit-v3" / "runtime-observation.schema.json"


def _empty_importance_observation() -> dict[str, object]:
    native = {
        "relation_policy_version": "chesstory.direct-cause-importance.relation.v3",
        "ordering_policy_version": (
            "chesstory.player-facing-idea-ordering.dominance-layers.v1"
        ),
        "selected_cause_ids": [],
        "frontier_cause_ids": [],
        "dominated_within_domain_cause_ids": [],
        "unique_top": None,
        "profiles": [],
        "relations": [],
        "decisions": [],
    }
    public = {
        "authority": "root_owned_effect_partial_order",
        "relation_policy_version": "chesstory.direct-cause-importance.relation.v3",
        "ordering_policy_version": (
            "chesstory.player-facing-idea-ordering.dominance-layers.v1"
        ),
        "comparison_exposure_rank_meaning": "comparison_exposure_authority",
        "selected_cause_ids": [],
        "frontier_cause_ids": [],
        "dominated_within_domain_cause_ids": [],
        "unique_top": None,
        "profile_summary": {
            "measured_channels": 0,
            "measured_causes": 0,
            "fully_measured_causes": 0,
            "unmeasured_causes": 0,
        },
        "profiles": [],
        "relations": [],
        "decisions": [],
        "relation_summary": {
            "comparable_profile_pairs": 0,
            "incomparable_profile_pairs": 0,
        },
        "unmeasured": [],
    }
    return {
        "r_native": native,
        "packet_native": None,
        "r_public_projection": public,
        "packet_public_projection": None,
        "public_projection": None,
    }


def _empty_cause_disposition_observation() -> dict[str, object]:
    summary = {
        "authority": "cause_disposition_ledger.v1",
        "total_cause_count": 0,
        "selected_cause_ids": [],
        "status_counts": {
            "selected": 0,
            "dominated": 0,
            "redundant": 0,
            "diagnostic": 0,
            "inferior": 0,
            "admission_deferred": 0,
            "rejected": 0,
            "unproposed": 0,
            "object_unready": 0,
        },
        "reason_counts": {
            "player_facing_selection": 0,
            "dominated_fallback": 0,
            "cross_comparison_redundancy": 0,
            "certified_claim_deduplicated": 0,
            "diagnostic_comparison": 0,
            "inferior_alternative": 0,
            "claim_admission_deferred": 0,
            "claim_admission_rejected": 0,
            "no_claim_proposal": 0,
            "object_readiness_failed": 0,
        },
        "abstention_codes": ["no_relative_cause_generated"],
    }
    return {
        "r_native": [],
        "packet_native": None,
        "r_public_summary": summary,
        "packet_public_summary": None,
        "selected_projection": None,
        "final_public_response": None,
    }


def _valid_active_cause_observation(
    request: dict[str, object],
) -> dict[str, object]:
    importance = _empty_importance_observation()
    return {
        "schema_version": TYPED_RUNTIME_OBSERVATION_SCHEMA,
        "status": "unavailable",
        "reason": "runtime_boundary_execution_absent",
        "request_sha256": native_sha256_json(request),
        "hash_contract": NATIVE_HASH_CONTRACT,
        "runtime": {
            "adapter_name": "chesstory-cause-audit-runtime-adapter",
            "adapter_version": "0.3.2",
            "request_schema": "chesstory.move-meaning.request.v1",
            "response_schema": "chesstory.move-meaning.response.v3",
        },
        "public_response": {
            "http_status": 400,
            "body_sha256": "9" * 64,
            "schema_version": "chesstory.move-meaning.response.v3",
            "request_id": request["request_id"],
            "ok": False,
            "status": "error",
            "availability": None,
            "error": "unsupported_schema_version",
            "primary_engine_backed": False,
            "idea_status": "unavailable",
            "idea_status_detail": None,
        },
        "boundary": {
            "c_reached": False,
            "jp_reached": False,
            "ja_reached": False,
            "r_reached": False,
            "packet_present": False,
            "p_reached": False,
            "cause_count": 0,
        },
        "projection": {"present": False, "packet_present": False},
        "probe_request_count": 0,
        "probe_requests": [],
        "comparison_endpoint_evidence_snapshots": [],
        "causes": [],
        "verdict": {
            "packet_canonical": None,
            "selected_projection": None,
            "final_public_response": None,
            "classification_authority": None,
        },
        "r_native_cause_selections": [],
        "importance": importance,
        "idea_units": {
            "r_native": [],
            "packet_native": None,
            "public_projection": None,
            "public_item_links": [],
            "public_item_membership_closed": True,
            "raw_public_idea_unit_count": 0,
            "parsed_public_idea_unit_count": 0,
            "public_idea_units_parse_closed": True,
        },
        "idea_importance": copy.deepcopy(importance),
        "cause_disposition_ledger": _empty_cause_disposition_observation(),
    }


def _valid_complete_active_cause_observation(
    request: dict[str, object],
) -> dict[str, object]:
    observation = _valid_active_cause_observation(request)
    observation["status"] = "complete"
    observation.pop("reason")
    observation["boundary"]["cause_kind_counts"] = {}  # type: ignore[index]
    observation["projection"] = {
        "present": False,
        "packet_present": False,
        "selected_public_cause_selections": [],
        "selected_public_cause_evidence_ids": [],
        "raw_public_idea_count": 0,
        "parsed_public_idea_count": 0,
        "public_ideas_parse_closed": True,
    }
    observation["root_owned_causal_episode_inventory"] = [
        {
            "source_id": "line:played",
            "line": {
                "id": "line:played",
                "root_move": "e2e4",
                "rank": 0,
                "role": "played",
            },
            "actor": {
                "move": "e2e4",
                "role": "root_move",
                "color": "white",
                "from": "e2",
                "to": "e4",
            },
            "target": "center",
            "mechanisms": ["space_gain"],
            "consequence": "future_recovery",
            "event_move": None,
            "event_ply_offset": 2,
            "chain_moves": ["e7e5"],
            "forcing_tactical_resource": False,
        }
    ]
    observation["only_move_constraints"] = []
    return observation


def _validate_active_cause_observation(
    request: dict[str, object], observation: dict[str, object]
) -> dict[str, object]:
    return _validate_runtime_observation(
        observation,
        expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
        registry=SchemaRegistry(CAUSE_V3_SCHEMA.parent),
        schema_path=CAUSE_V3_SCHEMA,
        expected_request_sha256=native_sha256_json(request),
        expected_hash_contract=NATIVE_HASH_CONTRACT,
    )


class ActiveCauseContractTest(unittest.TestCase):
    def test_production_run_binds_active_v3_schema_and_hash_contract(self) -> None:
        existing_file = ROOT / "pyproject.toml"
        acquisition = {
            "freeze_manifest_sha256": "a" * 64,
            "cases_file_sha256": "a" * 64,
            "engine_sha256": "a" * 64,
        }

        class _BindingCapture:
            @staticmethod
            def capture_run_document(name: str, document: object) -> str:
                del name
                return sha256_json(document)

        with (
            patch("chesstory_eval.cause_audit._verify_manifest", return_value=({}, [])),
            patch("chesstory_eval.cause_audit._selected_cases", return_value=[]),
            patch(
                "chesstory_eval.cause_audit._load_typed_labels",
                return_value=([{"case_id": "case-1"}], [{"case_id": "case-1"}]),
            ),
            patch("chesstory_eval.cause_audit._read_object", return_value=acquisition),
            patch("chesstory_eval.cause_audit.sha256_file", return_value="a" * 64),
            patch("chesstory_eval.cause_audit._acquisition_rows", return_value={}),
            patch(
                "chesstory_eval.cause_audit._RuntimeJsonlSession",
                side_effect=RuntimeError("session configured"),
            ) as session,
            self.assertRaises(RuntimeError),
        ):
            _run_cause_audit_for_contract(
                root=ROOT,
                store=_BindingCapture(),  # type: ignore[arg-type]
                manifest_path=existing_file,
                cases_path=existing_file,
                acquisition_path=existing_file,
                stockfish_path=existing_file,
                sbt_path=existing_file,
                adapter_root=ROOT / "runtime-adapter",
                cache_root=ROOT / "tmp",
                partition="explore",
                timeout_seconds=1.0,
                provider_timeout_seconds=1.0,
                max_probe_rounds=1,
                candidate_binding_path=None,
                requested_case_ids=[],
                actual_schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
                base_oracle_path=existing_file,
            )

        self.assertEqual(
            session.call_args.kwargs["observation_schema_path"],
            CAUSE_V3_SCHEMA.resolve(),
        )
        self.assertEqual(
            session.call_args.kwargs["expected_hash_contract"],
            NATIVE_HASH_CONTRACT,
        )

    def test_active_actual_view_rejects_historical_v2(self) -> None:
        with self.assertRaises(ContractError):
            _strict_actual_view(
                ROOT,
                {"schema_version": ACTUAL_VIEW_SCHEMA_VERSION},
            )

    def test_active_actual_view_preserves_and_rechecks_transport_hashes(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-view",
            "input": {},
        }
        request_sha256 = native_sha256_json(request)
        transport = {
            "round": 1,
            "request_jsonl_sha256": "1" * 64,
            "request_sha256": request_sha256,
            "adapter_request_sha256": request_sha256,
            "hash_contract": NATIVE_HASH_CONTRACT,
            "response_jsonl_sha256": "2" * 64,
            "diagnostic_line_sha256": [],
            "diagnostic_stream_sha256": "3" * 64,
        }
        observation = _valid_complete_active_cause_observation(request)
        _validate_active_cause_observation(request, observation)
        view = _actual_view(
            case={"case_id": "active-cause-view", "partition": "explore"},
            initial_request_sha256="1" * 64,
            final_request_sha256="1" * 64,
            observation=observation,
            transports=[transport],
            issued_probe_hashes=set(),
            result_hashes=set(),
            issued_probe_count=0,
            stop_reason="all_runtime_probes_closed",
            schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
        )

        canonical = _strict_actual_view(ROOT, view)
        self.assertEqual(canonical["request_sha256"], request_sha256)
        self.assertEqual(canonical["hash_contract"], NATIVE_HASH_CONTRACT)
        self.assertEqual(
            canonical["probe_closure"]["runtime_transport"][0]["request_sha256"],
            request_sha256,
        )

        changed = copy.deepcopy(view)
        changed["request_sha256"] = "f" * 64
        with self.assertRaises(IntegrityError):
            _strict_actual_view(ROOT, changed)

        changed = copy.deepcopy(view)
        changed["probe_closure"]["runtime_transport"][0]["request_sha256"] = (  # type: ignore[index]
            "e" * 64
        )
        with self.assertRaises(IntegrityError):
            _strict_actual_view(ROOT, changed)

        changed_observation = copy.deepcopy(observation)
        changed_observation["request_sha256"] = "f" * 64
        with self.assertRaises(IntegrityError):
            _actual_view(
                case={"case_id": "active-cause-view", "partition": "explore"},
                initial_request_sha256="1" * 64,
                final_request_sha256="1" * 64,
                observation=changed_observation,
                transports=[transport],
                issued_probe_hashes=set(),
                result_hashes=set(),
                issued_probe_count=0,
                stop_reason="all_runtime_probes_closed",
                schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
            )


class ActiveProbeCompletionContractTest(unittest.TestCase):
    def test_runtime_session_rejects_partial_schema_hash_binding(self) -> None:
        with self.assertRaises(ContractError):
            _RuntimeJsonlSession(
                ["not-started"],
                cwd=ROOT / "runtime-adapter",
                timeout_seconds=1.0,
                expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
                observation_schema_path=CAUSE_V3_SCHEMA,
            )
        with self.assertRaises(ContractError):
            _RuntimeJsonlSession(
                ["not-started"],
                cwd=ROOT / "runtime-adapter",
                timeout_seconds=1.0,
                expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
                expected_hash_contract=NATIVE_HASH_CONTRACT,
            )

    def test_runtime_invoke_hashes_request_and_preserves_exact_response_bytes(
        self,
    ) -> None:
        class FakeProcess:
            def __init__(self) -> None:
                self.stdin = io.BytesIO()

            def poll(self) -> None:
                return None

        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-wire",
            "input": {"z": 1, "a": 2},
        }
        request_line = f"{native_canonical_json(request)}\n".encode("utf-8")
        observation = _valid_active_cause_observation(request)
        response_line = (
            "  "
            + json.dumps(
                dict(reversed(list(observation.items()))),
                ensure_ascii=False,
                separators=(", ", ": "),
            )
            + " \r\n"
        ).encode("utf-8")
        session = _RuntimeJsonlSession(
            ["not-started"],
            cwd=ROOT / "runtime-adapter",
            timeout_seconds=1.0,
            expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
            observation_schema_path=CAUSE_V3_SCHEMA,
            expected_hash_contract=NATIVE_HASH_CONTRACT,
        )
        process = FakeProcess()
        session._process = process  # type: ignore[assignment]
        session._output.put(("line", response_line))
        exchange = session.invoke(request)

        self.assertEqual(process.stdin.getvalue(), request_line)
        self.assertEqual(exchange.request_jsonl_sha256, sha256_bytes(request_line))
        self.assertEqual(exchange.request_sha256, sha256_bytes(request_line[:-1]))
        self.assertEqual(exchange.hash_contract, NATIVE_HASH_CONTRACT)
        self.assertEqual(exchange.response_jsonl_sha256, sha256_bytes(response_line))
        self.assertEqual(
            base64.b64decode(exchange.response_jsonl_base64, validate=True),
            response_line,
        )
        self.assertEqual(
            _verified_response_jsonl_bytes(
                exchange.response_jsonl_base64,
                exchange.response_jsonl_sha256,
            ),
            response_line,
        )
        with self.assertRaises(IntegrityError):
            _verified_response_jsonl_bytes(
                exchange.response_jsonl_base64,
                "f" * 64,
            )

    def test_active_v3_schema_accepts_valid_response_and_rejects_malformed_fields(
        self,
    ) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-schema",
            "input": {},
        }
        valid = _valid_complete_active_cause_observation(request)
        self.assertEqual(_validate_active_cause_observation(request, valid), valid)

        malformed = copy.deepcopy(valid)
        malformed.pop("importance")
        with self.assertRaises(ContractError):
            _validate_active_cause_observation(request, malformed)

        malformed = copy.deepcopy(valid)
        malformed["importance"] = []
        with self.assertRaises(ContractError):
            _validate_active_cause_observation(request, malformed)

        malformed = copy.deepcopy(valid)
        malformed["unexpected"] = True
        with self.assertRaises(ContractError):
            _validate_active_cause_observation(request, malformed)

    def test_active_v3_rejects_request_or_hash_contract_drift(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-binding",
            "input": {},
        }
        observation = _valid_active_cause_observation(request)
        observation["request_sha256"] = native_sha256_json(
            {**request, "request_id": "another-request"}
        )
        with self.assertRaises(IntegrityError):
            _validate_active_cause_observation(request, observation)

        for contract in (None, "unknown.v1"):
            observation = _valid_active_cause_observation(request)
            if contract is None:
                observation.pop("hash_contract")
            else:
                observation["hash_contract"] = contract
            with self.subTest(contract=contract), self.assertRaises(ContractError):
                _validate_active_cause_observation(request, observation)

    def test_historical_v2_validation_and_transport_shape_remain_legacy(self) -> None:
        historical = {
            "schema_version": "chesstory.cause-audit-runtime-observation.v2",
            "request_sha256": "1" * 64,
            "hash_contract": NATIVE_HASH_CONTRACT,
        }
        self.assertEqual(
            _validate_runtime_observation(
                historical,
                expected_schema="chesstory.cause-audit-runtime-observation.v2",
                registry=None,
                schema_path=None,
            ),
            historical,
        )

        response_bytes = b'{"schema_version":"chesstory.runtime-observation.v2"}\n'
        exchange = _RuntimeExchange(
            observation={"schema_version": "chesstory.runtime-observation.v2"},
            request_jsonl_sha256="1" * 64,
            response_jsonl_sha256=sha256_bytes(response_bytes),
            response_jsonl_base64=base64.b64encode(response_bytes).decode("ascii"),
            diagnostic_line_sha256=(),
            diagnostic_stream_sha256=sha256_bytes(b""),
        )
        document = _runtime_io_document(
            exchange,
            {
                "schema_version": "chesstory.move-meaning.request.v1",
                "request_id": "historical-v2-wire",
                "input": {},
            },
            command=("historical-v2-runtime",),
            round_index=0,
        )
        self.assertNotIn("response_jsonl_base64", document)
        self.assertEqual(document["response_jsonl_sha256"], sha256_bytes(response_bytes))


if __name__ == "__main__":
    unittest.main()
