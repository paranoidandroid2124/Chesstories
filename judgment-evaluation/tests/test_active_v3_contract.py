from __future__ import annotations

import io
import unittest
from contextlib import redirect_stderr
from pathlib import Path
from unittest.mock import patch

from chesstory_eval.cause_audit import (
    ACTIVE_CAUSE_ADAPTER_ARGUMENT,
    ACTUAL_VIEW_SCHEMA_VERSION,
    HISTORICAL_CAUSE_ADAPTER_ARGUMENT,
    TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    _cause_adapter_command,
    _strict_actual_view,
    compare_cause_audit,
    run_cause_audit,
    run_historical_cause_audit_v2,
)
from chesstory_eval.cli import _parser
from chesstory_eval.hashing import sha256_json
from chesstory_eval.model import ContractError
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


class ActiveProbeCompletionContractTest(unittest.TestCase):
    def test_runtime_session_defaults_to_strict_native_v3_schema(self) -> None:
        session = _RuntimeJsonlSession(
            ["not-started"],
            cwd=ROOT / "runtime-adapter",
            timeout_seconds=1.0,
        )
        self.assertEqual(session.expected_schema, RUNTIME_OBSERVATION_SCHEMA)
        self.assertEqual(session.observation_schema_path, NATIVE_V3_SCHEMA.resolve())

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
