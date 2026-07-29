from __future__ import annotations

import copy
import io
import unittest
from contextlib import redirect_stderr
from pathlib import Path
from unittest.mock import patch

from chesstory_eval.cause_audit import (
    ACTIVE_CAUSE_ADAPTER_ARGUMENT,
    ACTUAL_VIEW_SCHEMA_VERSION,
    HISTORICAL_CAUSE_ADAPTER_ARGUMENT,
    TYPED_RUNTIME_OBSERVATION_SCHEMA,
    TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    _actual_view,
    _cause_adapter_command,
    _run_cause_audit_for_contract,
    _strict_actual_view,
    compare_cause_audit,
    run_cause_audit,
    run_historical_cause_audit_v2,
)
from chesstory_eval.cli import _parser
from chesstory_eval.hashing import sha256_bytes, sha256_json
from chesstory_eval.model import ContractError, IntegrityError
from chesstory_eval.native_diagnostic import (
    NATIVE_HASH_CONTRACT,
    native_canonical_json,
    native_sha256_json,
)
from chesstory_eval.probe_completion import (
    RUNTIME_OBSERVATION_SCHEMA,
    _RuntimeExchange,
    _RuntimeJsonlSession,
    _validate_runtime_observation,
    complete_runtime_probes,
)
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
NATIVE_V3_SCHEMA = ROOT / "schemas" / "native-v3" / "runtime-observation.schema.json"
CAUSE_V3_SCHEMA = ROOT / "schemas" / "cause-audit-v3" / "runtime-observation.schema.json"


class _FakeRuntime:
    command = ("fake-runtime",)

    def __init__(self, observation: dict[str, object]) -> None:
        self.observation = observation

    def invoke(self, request: object) -> _RuntimeExchange:
        del request
        return _RuntimeExchange(
            observation=self.observation,
            request_jsonl_sha256="1" * 64,
            response_jsonl_sha256="2" * 64,
            diagnostic_line_sha256=(),
            diagnostic_stream_sha256="3" * 64,
        )


class _FakeCapture:
    def write(self, relative_path: str, document: object) -> dict[str, str]:
        return {"path": relative_path, "sha256": sha256_json(document)}


def _run_arguments() -> dict[str, object]:
    placeholder = ROOT / "placeholder"
    return {
        "root": ROOT,
        "store": object(),
        "manifest_path": placeholder,
        "cases_path": placeholder,
        "acquisition_path": placeholder,
        "stockfish_path": placeholder,
        "sbt_path": placeholder,
        "adapter_root": ROOT / "runtime-adapter",
        "cache_root": placeholder,
        "partition": "explore",
        "timeout_seconds": 1.0,
        "provider_timeout_seconds": 1.0,
        "max_probe_rounds": 1,
        "candidate_binding_path": None,
        "requested_case_ids": [],
    }


def _completion(status: str) -> dict[str, object]:
    observation = {
        "observation_status": status,
        "stages": {},
    }
    if status == "complete":
        observation["public_response"] = {
            "body": {"status": "ready", "probe_requests": []},
        }
    return complete_runtime_probes(
        {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": f"sample-{status}",
            "input": {},
        },
        runtime=_FakeRuntime(observation),  # type: ignore[arg-type]
        stockfish=ROOT / "unused-stockfish",
        stockfish_sha256="4" * 64,
        capture=_FakeCapture(),  # type: ignore[arg-type]
        max_rounds=1,
        engine_timeout_seconds=1.0,
    )


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
    observation["boundary"]["cause_kind_counts"] = {}
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


def _valid_comparison_endpoint_snapshot() -> dict[str, object]:
    reference_runtime_line = {
        "id": "reference-line",
        "root_move": "e2e4",
        "rank": 1,
        "role": "best_reference",
    }
    candidate_runtime_line = {
        "id": "candidate-line",
        "root_move": "e2e3",
        "rank": 0,
        "role": "played",
    }
    return {
        "comparison_evidence": {
            "id": "comparison-1",
            "producer": "runtime",
            "layer": "comparison",
            "scope": "root",
            "confidence": "high",
            "line": None,
            "record_registered": True,
            "record_payload_type": "CandidateComparisonEvidence",
            "record_sha256": "4" * 64,
        },
        "comparison": {
            "kind": "played_vs_best",
            "reference_line": reference_runtime_line,
            "candidate_line": candidate_runtime_line,
            "reference_root_move": "e2e4",
            "candidate_root_move": "e2e3",
            "mover": "white",
            "candidate_win_percent_delta_for_mover": -10.0,
            "win_percent_loss_for_mover": 10.0,
            "verdict": "mistake",
            "candidate_set_type": None,
        },
        "reference": {
            "source_side": "reference",
            "line": {
                "line_id": "reference-line",
                "root_move": "e2e4",
                "rank": 1,
                "role": "best_reference",
            },
            "witnesses": [
                {
                    "source_side": "reference",
                    "line": {
                        "line_id": "reference-line",
                        "root_move": "e2e4",
                        "rank": 1,
                        "role": "best_reference",
                    },
                    "carrier": {
                        "id": "carrier-1",
                        "producer": "runtime",
                        "layer": "causal",
                        "scope": "direct",
                        "confidence": "high",
                        "line": reference_runtime_line,
                        "record_registered": True,
                        "record_payload_type": "TacticalMechanismEvidence",
                        "record_sha256": "6" * 64,
                    },
                    "provenance": [
                        {
                            "id": "primitive-1",
                            "producer": "runtime",
                            "layer": "primitive",
                            "scope": "context",
                            "confidence": "high",
                            "line": reference_runtime_line,
                            "record_registered": True,
                            "record_payload_type": "StructuralDeltaEvidence",
                            "record_sha256": "5" * 64,
                        }
                    ],
                    "carrier_ancestor_source_ids": ["primitive-1"],
                    "primitive_proof_source": {
                        "id": "primitive-1",
                        "producer": "runtime",
                        "layer": "primitive",
                        "scope": "context",
                        "confidence": "high",
                        "line": reference_runtime_line,
                        "record_registered": True,
                        "record_payload_type": "StructuralDeltaEvidence",
                        "record_sha256": "5" * 64,
                    },
                    "actor": [
                        {"kind": "move", "key": "e2e4"},
                        {"kind": "side", "key": "white"},
                        {"kind": "piece", "key": "pawn"},
                        {"kind": "square", "key": "e2"},
                        {"kind": "square", "key": "e4"},
                    ],
                    "target": [{"kind": "square", "key": "e5"}],
                    "mechanism": [{"kind": "mechanism", "key": "tempo"}],
                    "consequence": [
                        {"kind": "consequence", "key": "initiative"}
                    ],
                    "witness": [{"kind": "move", "key": "g8f6"}],
                    "horizon": "ply:1",
                    "proof_segment": {
                        "terminal_relation": "makes_structural_transition",
                        "steps": [
                            {
                                "ply_offset": 0,
                                "move_uci": "e2e4",
                                "role": "root_action",
                            }
                        ],
                    },
                    "effect_descriptor": {
                        "effect_scope": {
                            "primitive_kind": "structural_transition",
                            "target_signatures": ["square:e5"],
                            "plan_ids": [],
                            "strategic_axes": [],
                        },
                        "magnitude_status": "exact",
                        "measure": {
                            "kind": "structural_strength",
                            "units": 1,
                        },
                        "material_event_salience": None,
                    },
                }
            ],
        },
        "candidate": {
            "source_side": "candidate",
            "line": {
                "line_id": "candidate-line",
                "root_move": "e2e3",
                "rank": 0,
                "role": "played",
            },
            "witnesses": [],
        },
    }


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
    def test_active_run_is_v3_and_historical_run_is_explicit_v2(self) -> None:
        with patch(
            "chesstory_eval.cause_audit._run_cause_audit_for_contract",
            return_value={"mode": "captured"},
        ) as core:
            self.assertEqual(run_cause_audit(**_run_arguments()), {"mode": "captured"})
            self.assertEqual(
                core.call_args.kwargs["actual_schema_version"],
                TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
            )

        with patch(
            "chesstory_eval.cause_audit._run_cause_audit_for_contract",
            return_value={"mode": "historical"},
        ) as core:
            self.assertEqual(
                run_historical_cause_audit_v2(**_run_arguments()),
                {"mode": "historical"},
            )
            self.assertEqual(
                core.call_args.kwargs["actual_schema_version"],
                ACTUAL_VIEW_SCHEMA_VERSION,
            )
        historical_command = _cause_adapter_command(
            ROOT / "sbt",
            ACTUAL_VIEW_SCHEMA_VERSION,
        )
        self.assertIn(HISTORICAL_CAUSE_ADAPTER_ARGUMENT, historical_command[-1])
        active_command = _cause_adapter_command(
            ROOT / "sbt",
            TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
        )
        self.assertIn(ACTIVE_CAUSE_ADAPTER_ARGUMENT, active_command[-1])

    def test_active_cli_has_no_version_switch_and_historical_is_named(self) -> None:
        parser = _parser()
        active = parser.parse_args(
            [
                "cause-audit",
                "--action",
                "run",
                "--run-id",
                "active",
                "--output",
                "active.json",
                "--cases",
                "cases.jsonl",
            ]
        )
        self.assertEqual(active.command, "cause-audit")
        self.assertFalse(hasattr(active, "cause_contract_version"))

        historical = parser.parse_args(
            [
                "historical-cause-audit-v2",
                "--action",
                "run",
                "--run-id",
                "historical",
                "--output",
                "historical.json",
                "--cases",
                "cases.jsonl",
            ]
        )
        self.assertEqual(historical.command, "historical-cause-audit-v2")

        for contract in ("v2", "v3", "unknown"):
            with redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                parser.parse_args(
                    [
                        "cause-audit",
                        "--action",
                        "run",
                        "--run-id",
                        "bad",
                        "--output",
                        "bad.json",
                        "--cases",
                        "cases.jsonl",
                        "--cause-contract-version",
                        contract,
                    ]
                )

    def test_production_run_binds_only_active_v3_to_raw_schema_and_hash(self) -> None:
        existing_file = ROOT / "pyproject.toml"
        acquisition = {
            "freeze_manifest_sha256": "a" * 64,
            "cases_file_sha256": "a" * 64,
            "engine_sha256": "a" * 64,
        }
        for schema_version, expected_path, expected_contract in (
            (
                TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
                CAUSE_V3_SCHEMA.resolve(),
                NATIVE_HASH_CONTRACT,
            ),
            (ACTUAL_VIEW_SCHEMA_VERSION, None, None),
        ):
            with (
                self.subTest(schema_version=schema_version),
                patch(
                    "chesstory_eval.cause_audit._verify_manifest",
                    return_value=({}, []),
                ),
                patch(
                    "chesstory_eval.cause_audit._selected_cases", return_value=[]
                ),
                patch(
                    "chesstory_eval.cause_audit._read_object",
                    return_value=acquisition,
                ),
                patch(
                    "chesstory_eval.cause_audit.sha256_file", return_value="a" * 64
                ),
                patch(
                    "chesstory_eval.cause_audit._acquisition_rows", return_value={}
                ),
                patch(
                    "chesstory_eval.cause_audit._RuntimeJsonlSession",
                    side_effect=RuntimeError("session configured"),
                ) as session,
                self.assertRaisesRegex(RuntimeError, "session configured"),
            ):
                _run_cause_audit_for_contract(
                    root=ROOT,
                    store=object(),  # type: ignore[arg-type]
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
                    actual_schema_version=schema_version,
                )
            self.assertEqual(
                session.call_args.kwargs["observation_schema_path"], expected_path
            )
            self.assertEqual(
                session.call_args.kwargs["expected_hash_contract"], expected_contract
            )

    def test_active_actual_view_rejects_v2_before_schema_dispatch(self) -> None:
        with self.assertRaisesRegex(ContractError, "unsupported actual cause cascade"):
            _strict_actual_view(
                ROOT,
                {"schema_version": ACTUAL_VIEW_SCHEMA_VERSION},
            )

    def test_active_compare_calls_only_typed_comparator_without_label_sniff(self) -> None:
        placeholder = ROOT / "not-opened"
        with (
            patch(
                "chesstory_eval.cause_audit._peek_label_schema_version",
                side_effect=AssertionError("active compare sniffed a label"),
            ),
            patch(
                "chesstory_eval.cause_audit._compare_typed_cause_audit",
                return_value={"schema_version": "typed"},
            ) as typed,
        ):
            result = compare_cause_audit(
                root=ROOT,
                store=object(),  # type: ignore[arg-type]
                manifest_path=placeholder,
                cases_path=placeholder,
                labels_path=placeholder,
                runtime_run_path=placeholder,
                partition="explore",
                requested_case_ids=[],
                oracle_run_manifest_path=placeholder,
                base_oracle_path=placeholder,
                candidate_set_path=placeholder,
                adjudication_path=placeholder,
                arm_inventory_path=placeholder,
            )
        self.assertEqual(result, {"schema_version": "typed"})
        typed.assert_called_once()

    def test_active_actual_view_preserves_verified_transport_binding(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-valid",
            "input": {},
        }
        request_jsonl_sha256 = "1" * 64
        request_sha256 = native_sha256_json(request)
        transport = {
            "round": 1,
            "request_jsonl_sha256": request_jsonl_sha256,
            "request_sha256": request_sha256,
            "adapter_request_sha256": request_sha256,
            "hash_contract": NATIVE_HASH_CONTRACT,
            "response_jsonl_sha256": "2" * 64,
            "diagnostic_line_sha256": [],
            "diagnostic_stream_sha256": "3" * 64,
        }
        final_transport = {**transport, "round": 2}
        observation = _valid_complete_active_cause_observation(request)
        _validate_active_cause_observation(request, observation)
        view = _actual_view(
            case={"case_id": "active-cause-valid", "partition": "explore"},
            initial_request_sha256=request_jsonl_sha256,
            final_request_sha256=request_jsonl_sha256,
            observation=observation,
            transports=[transport, final_transport],
            issued_probe_hashes=set(),
            result_hashes=set(),
            issued_probe_count=0,
            stop_reason="all_runtime_probes_closed",
            schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
        )

        canonical = _strict_actual_view(ROOT, view)
        self.assertEqual(canonical["request_sha256"], request_sha256)
        self.assertEqual(canonical["hash_contract"], NATIVE_HASH_CONTRACT)
        self.assertEqual(canonical["comparison_endpoint_evidence_snapshots"], [])
        self.assertEqual(
            canonical["probe_closure"]["runtime_transport"][0]["request_sha256"],
            request_sha256,
        )

        missing = copy.deepcopy(view)
        missing.pop("request_sha256")
        with self.assertRaisesRegex(ContractError, "schema validation failed"):
            _strict_actual_view(ROOT, missing)

        changed = copy.deepcopy(view)
        changed["request_sha256"] = "f" * 64
        with self.assertRaisesRegex(IntegrityError, "canonical request hash mismatch"):
            _strict_actual_view(ROOT, changed)

        changed_intermediate = copy.deepcopy(view)
        changed_intermediate["probe_closure"]["runtime_transport"][0][
            "request_sha256"
        ] = "e" * 64
        with self.assertRaisesRegex(IntegrityError, "transport 1 request hash mismatch"):
            _strict_actual_view(ROOT, changed_intermediate)

    def test_active_actual_view_requires_and_copies_endpoint_snapshots(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-snapshots",
            "input": {},
        }
        observation = _valid_complete_active_cause_observation(request)
        observation["comparison_endpoint_evidence_snapshots"] = [
            _valid_comparison_endpoint_snapshot()
        ]
        _validate_active_cause_observation(request, observation)
        transport = {
            "round": 1,
            "request_jsonl_sha256": "1" * 64,
            "request_sha256": native_sha256_json(request),
            "adapter_request_sha256": native_sha256_json(request),
            "hash_contract": NATIVE_HASH_CONTRACT,
            "response_jsonl_sha256": "2" * 64,
            "diagnostic_line_sha256": [],
            "diagnostic_stream_sha256": "3" * 64,
        }
        view = _actual_view(
            case={"case_id": "active-cause-snapshots", "partition": "explore"},
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
        self.assertIsNot(
            view["comparison_endpoint_evidence_snapshots"],
            observation["comparison_endpoint_evidence_snapshots"],
        )
        observation["comparison_endpoint_evidence_snapshots"][0]["candidate"][
            "line"
        ]["line_id"] = "mutated-line"
        self.assertEqual(
            view["comparison_endpoint_evidence_snapshots"][0]["candidate"]["line"][
                "line_id"
            ],
            "candidate-line",
        )
        self.assertEqual(
            _strict_actual_view(ROOT, view)[
                "comparison_endpoint_evidence_snapshots"
            ],
            [_valid_comparison_endpoint_snapshot()],
        )

        for invalid in (None, {}, [None]):
            with self.subTest(invalid=invalid):
                malformed = copy.deepcopy(observation)
                malformed["comparison_endpoint_evidence_snapshots"] = invalid
                with self.assertRaisesRegex(
                    ContractError, "comparison endpoint evidence snapshot array"
                ):
                    _actual_view(
                        case={
                            "case_id": "active-cause-snapshots",
                            "partition": "explore",
                        },
                        initial_request_sha256="1" * 64,
                        final_request_sha256="1" * 64,
                        observation=malformed,
                        transports=[transport],
                        issued_probe_hashes=set(),
                        result_hashes=set(),
                        issued_probe_count=0,
                        stop_reason="all_runtime_probes_closed",
                        schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
                    )

    def test_active_actual_view_rejects_raw_hash_changed_after_validation(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-changed",
            "input": {},
        }
        observation = _valid_complete_active_cause_observation(request)
        _validate_active_cause_observation(request, observation)
        transport = {
            "round": 1,
            "request_jsonl_sha256": "1" * 64,
            "request_sha256": native_sha256_json(request),
            "adapter_request_sha256": native_sha256_json(request),
            "hash_contract": NATIVE_HASH_CONTRACT,
            "response_jsonl_sha256": "2" * 64,
            "diagnostic_line_sha256": [],
            "diagnostic_stream_sha256": "3" * 64,
        }
        observation["request_sha256"] = "f" * 64
        with self.assertRaisesRegex(IntegrityError, "changed before actual view"):
            _actual_view(
                case={"case_id": "active-cause-changed", "partition": "explore"},
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


class ActiveProbeCompletionContractTest(unittest.TestCase):
    def test_runtime_session_defaults_to_strict_native_v3_schema(self) -> None:
        session = _RuntimeJsonlSession(
            ["not-started"],
            cwd=ROOT / "runtime-adapter",
            timeout_seconds=1.0,
        )
        self.assertEqual(session.expected_schema, RUNTIME_OBSERVATION_SCHEMA)
        self.assertEqual(session.observation_schema_path, NATIVE_V3_SCHEMA.resolve())
        self.assertEqual(session.expected_hash_contract, NATIVE_HASH_CONTRACT)

    def test_runtime_session_rejects_partial_schema_hash_binding(self) -> None:
        with self.assertRaisesRegex(
            ContractError, "schema and request hash contract must be bound together"
        ):
            _RuntimeJsonlSession(
                ["not-started"],
                cwd=ROOT / "runtime-adapter",
                timeout_seconds=1.0,
                expected_schema="chesstory.custom-runtime-observation.v3",
                observation_schema_path=CAUSE_V3_SCHEMA,
            )
        with self.assertRaisesRegex(
            ContractError, "schema and request hash contract must be bound together"
        ):
            _RuntimeJsonlSession(
                ["not-started"],
                cwd=ROOT / "runtime-adapter",
                timeout_seconds=1.0,
                expected_schema="chesstory.custom-runtime-observation.v3",
                expected_hash_contract=NATIVE_HASH_CONTRACT,
            )

    def test_active_cause_session_binds_dedicated_schema_and_hash_contract(self) -> None:
        session = _RuntimeJsonlSession(
            ["not-started"],
            cwd=ROOT / "runtime-adapter",
            timeout_seconds=1.0,
            expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
            observation_schema_path=CAUSE_V3_SCHEMA,
            expected_hash_contract=NATIVE_HASH_CONTRACT,
        )
        self.assertEqual(session.expected_schema, TYPED_RUNTIME_OBSERVATION_SCHEMA)
        self.assertEqual(session.observation_schema_path, CAUSE_V3_SCHEMA.resolve())
        self.assertEqual(session.expected_hash_contract, NATIVE_HASH_CONTRACT)

    def test_runtime_invoke_hashes_the_exact_transmitted_request_record(self) -> None:
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
        expected_line = f"{native_canonical_json(request)}\n".encode("utf-8")
        observation = _valid_active_cause_observation(request)
        response_line = f"{native_canonical_json(observation)}\n".encode("utf-8")
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
        with patch(
            "chesstory_eval.probe_completion._validate_runtime_observation",
            wraps=_validate_runtime_observation,
        ) as validator:
            exchange = session.invoke(request)

        self.assertEqual(process.stdin.getvalue(), expected_line)
        self.assertEqual(
            validator.call_args.kwargs["expected_request_sha256"],
            sha256_bytes(expected_line[:-1]),
        )
        self.assertEqual(
            validator.call_args.kwargs["expected_hash_contract"],
            NATIVE_HASH_CONTRACT,
        )
        self.assertEqual(exchange.request_jsonl_sha256, sha256_bytes(expected_line))
        self.assertEqual(exchange.request_sha256, sha256_bytes(expected_line[:-1]))
        self.assertEqual(exchange.hash_contract, NATIVE_HASH_CONTRACT)
        self.assertEqual(exchange.observation, observation)

    def test_valid_active_cause_observations_pass_schema_and_request_hash(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-valid",
            "input": {},
        }
        unavailable = _valid_active_cause_observation(request)
        complete = _valid_complete_active_cause_observation(request)
        adapter_error = copy.deepcopy(unavailable)
        adapter_error["status"] = "adapter_error"
        adapter_error["adapter_error"] = {
            "kind": "cause_audit_observation_failed",
            "class": "java.lang.IllegalStateException",
        }
        adapter_error["public_response"] = {
            "primary_engine_backed": False,
            "idea_status": "unavailable",
            "idea_status_detail": None,
        }
        for field in (
            "reason",
            "boundary",
            "projection",
            "probe_request_count",
            "probe_requests",
        ):
            adapter_error.pop(field)

        for status, observation in (
            ("complete", complete),
            ("unavailable", unavailable),
            ("adapter_error", adapter_error),
        ):
            with self.subTest(status=status):
                validated = _validate_active_cause_observation(request, observation)
                self.assertEqual(validated, observation)

        wrong_complete = copy.deepcopy(complete)
        wrong_complete["public_response"] = adapter_error["public_response"]
        wrong_adapter_error = copy.deepcopy(adapter_error)
        wrong_adapter_error["public_response"] = unavailable["public_response"]
        for status, observation in (
            ("complete", wrong_complete),
            ("adapter_error", wrong_adapter_error),
        ):
            with self.subTest(status=f"{status}-response-shape"), self.assertRaisesRegex(
                ContractError, "schema validation failed"
            ):
                _validate_active_cause_observation(request, observation)

    def test_active_cause_observation_rejects_missing_typed_payload(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-missing-typed",
            "input": {},
        }
        for field in ("importance", "comparison_endpoint_evidence_snapshots"):
            with self.subTest(field=field):
                observation = _valid_active_cause_observation(request)
                observation.pop(field)
                with self.assertRaisesRegex(ContractError, "schema validation failed"):
                    _validate_active_cause_observation(request, observation)

    def test_active_cause_observation_rejects_wrong_type_and_extra_field(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-malformed",
            "input": {},
        }
        for name, mutate in (
            ("wrong-type", lambda value: value.update(importance=[])),
            (
                "wrong-snapshot-type",
                lambda value: value.update(
                    comparison_endpoint_evidence_snapshots={}
                ),
            ),
            ("extra-field", lambda value: value.update(unexpected=True)),
        ):
            observation = _valid_active_cause_observation(request)
            mutate(observation)
            with self.subTest(name=name), self.assertRaisesRegex(
                ContractError, "schema validation failed"
            ):
                _validate_active_cause_observation(request, observation)

    def test_active_cause_observation_rejects_snapshot_witness_polarity(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-snapshot-polarity",
            "input": {},
        }
        observation = _valid_complete_active_cause_observation(request)
        snapshot = _valid_comparison_endpoint_snapshot()
        snapshot["reference"]["witnesses"][0]["source_side"] = "candidate"
        observation["comparison_endpoint_evidence_snapshots"] = [snapshot]
        with self.assertRaisesRegex(ContractError, "schema validation failed"):
            _validate_active_cause_observation(request, observation)

    def test_active_cause_observation_rejects_another_request_hash(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-request-a",
            "input": {},
        }
        observation = _valid_active_cause_observation(request)
        observation["request_sha256"] = native_sha256_json(
            {**request, "request_id": "active-cause-request-b"}
        )
        with self.assertRaisesRegex(IntegrityError, "request hash mismatch"):
            _validate_active_cause_observation(request, observation)

    def test_active_cause_observation_rejects_missing_or_unknown_hash_contract(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-hash-contract",
            "input": {},
        }
        for name, mutate in (
            ("missing", lambda value: value.pop("hash_contract")),
            ("unknown", lambda value: value.update(hash_contract="unknown.v1")),
        ):
            observation = _valid_active_cause_observation(request)
            mutate(observation)
            with self.subTest(name=name), self.assertRaisesRegex(
                ContractError, "schema validation failed"
            ):
                _validate_active_cause_observation(request, observation)

    def test_historical_cause_v2_observation_keeps_legacy_validation_path(self) -> None:
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
        session = _RuntimeJsonlSession(
            ["not-started"],
            cwd=ROOT / "runtime-adapter",
            timeout_seconds=1.0,
            expected_schema="chesstory.cause-audit-runtime-observation.v2",
        )
        self.assertIsNone(session.observation_schema_path)
        self.assertIsNone(session.expected_hash_contract)

    def test_v2_and_malformed_v3_observations_fail_closed(self) -> None:
        registry = SchemaRegistry(NATIVE_V3_SCHEMA.parent)
        with self.assertRaisesRegex(ContractError, "unexpected observation schema"):
            _validate_runtime_observation(
                {"schema_version": "chesstory.runtime-observation.v2"},
                expected_schema=RUNTIME_OBSERVATION_SCHEMA,
                registry=registry,
                schema_path=NATIVE_V3_SCHEMA,
            )
        with self.assertRaisesRegex(ContractError, "schema validation failed"):
            _validate_runtime_observation(
                {"schema_version": RUNTIME_OBSERVATION_SCHEMA},
                expected_schema=RUNTIME_OBSERVATION_SCHEMA,
                registry=registry,
                schema_path=NATIVE_V3_SCHEMA,
            )

    def test_complete_without_probes_is_closed(self) -> None:
        result = _completion("complete")
        self.assertEqual(result["closure_status"], "closed")
        self.assertEqual(result["stop_reason"], "all-runtime-probes-closed")

    def test_adapter_error_without_probes_is_incomplete(self) -> None:
        result = _completion("adapter-error")
        self.assertEqual(result["closure_status"], "incomplete")
        self.assertEqual(result["stop_reason"], "runtime-observation-not-complete")

    def test_unavailable_without_probes_is_incomplete(self) -> None:
        result = _completion("unavailable")
        self.assertEqual(result["closure_status"], "incomplete")
        self.assertEqual(result["stop_reason"], "runtime-observation-not-complete")


if __name__ == "__main__":
    unittest.main()
