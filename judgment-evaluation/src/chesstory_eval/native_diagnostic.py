from __future__ import annotations

import copy
import hashlib
import json
import math
import os
import re
import shutil
import subprocess
import time
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from queue import Empty, Queue
from threading import Thread
from typing import Any, Protocol

from .capture import ArtifactStore, utc_now
from .design import build_arm_plan
from .hashing import sha256_bytes, sha256_file, sha256_json
from .model import Arm, ContractError, IntegrityError, RunContext, Source, STAGES


NATIVE_OBSERVED_STAGES: tuple[str, ...] = STAGES[:-1]
NATIVE_UNAVAILABLE_STAGE = "V"
NATIVE_HASH_CONTRACT = "chesstory.sorted-object-keys-json-sha256.v1"
NATIVE_OBSERVATION_SCHEMA = "chesstory.runtime-observation.v2"
NATIVE_ENGINEERING_INVOCATION_SCHEMA = (
    "chesstory.runtime-native-engineering-invocation.v1"
)
NATIVE_ENGINEERING_RESULT_SCHEMA = "chesstory.runtime-native-engineering-result.v1"
NATIVE_ENGINEERING_REPORT_SCHEMA = "chesstory.eval.native-engineering-report.v2"
NATIVE_ENGINEERING_PROVIDER_IO_SCHEMA = (
    "chesstory.eval.native-engineering-provider-io.v1"
)
NATIVE_PROVIDER_FAILURE_SCHEMA = "chesstory.eval.native-provider-failure.v1"
NATIVE_PROVIDER_BINDING_SCHEMA = "chesstory.eval.native-provider-binding.v1"
NATIVE_COMMAND_PROVIDER_BINDING_SCHEMA = (
    "chesstory.eval.native-command-provider-binding.v2"
)
MODEL_PROXY_TIER = "model-proxy-nonhuman"

_SHA256 = re.compile(r"^[0-9a-f]{64}$")
_REALIZATION_KINDS = frozenset({"identity", "native-selection"})
_DIRECTIVE_PRODUCER_TIERS = frozenset(
    {"deterministic-engineering", "blind-model-proxy", "held-out-human-rater"}
)


class NativeResultProvider(Protocol):
    """A provider of external native engineering result envelopes."""

    def invoke(self, invocation: Mapping[str, Any]) -> Mapping[str, Any]: ...


class NativeProviderTimeout(ContractError):
    """A bounded provider timeout with path-opaque capture details."""

    def __init__(self, details: Mapping[str, Any]) -> None:
        super().__init__("native JSONL process exceeded its response timeout")
        self.details = _finite_json_object(details, "native provider timeout details")


class JsonlNativeCommandProvider:
    """Persistent JSONL session for the external native intervention main.

    The command is supplied explicitly so this module neither discovers nor
    imports the production runtime.  Non-JSON build-tool output is retained
    only as bounded diagnostics and is never treated as a result artifact.
    """

    def __init__(
        self,
        command: Sequence[str],
        *,
        cwd: Path | str | None = None,
        response_timeout_seconds: float = 300.0,
    ) -> None:
        if isinstance(command, (str, bytes)) or not command:
            raise ContractError("native JSONL command must be a non-empty argument list")
        if any(not isinstance(item, str) or not item for item in command):
            raise ContractError("native JSONL command arguments must be non-empty text")
        if (
            isinstance(response_timeout_seconds, bool)
            or not isinstance(response_timeout_seconds, (int, float))
            or not math.isfinite(float(response_timeout_seconds))
            or float(response_timeout_seconds) < 0.001
            or float(response_timeout_seconds) > 3600.0
        ):
            raise ContractError(
                "native JSONL response timeout must lie between 0.001 and 3600 seconds"
            )
        timeout_milliseconds = math.ceil(float(response_timeout_seconds) * 1000.0)
        self.command = tuple(command)
        self.cwd = None if cwd is None else Path(cwd)
        self.response_timeout_milliseconds = timeout_milliseconds
        self.response_timeout_seconds = timeout_milliseconds / 1000.0
        self._process: subprocess.Popen[str] | None = None
        self._diagnostics: list[str] = []
        self._diagnostic_line_count = 0

    def __enter__(self) -> JsonlNativeCommandProvider:
        self._start()
        return self

    def __exit__(self, exc_type: Any, exc: Any, traceback: Any) -> None:
        self.close(raise_on_failure=exc_type is None)

    def invoke(self, invocation: Mapping[str, Any]) -> Mapping[str, Any]:
        process = self._start()
        if process.stdin is None or process.stdout is None:
            raise ContractError("native JSONL process pipes are unavailable")
        self._diagnostics = []
        self._diagnostic_line_count = 0
        try:
            process.stdin.write(native_canonical_json(invocation) + "\n")
            process.stdin.flush()
        except (BrokenPipeError, OSError) as error:
            raise ContractError("native JSONL process closed its input") from error
        deadline = time.monotonic() + self.response_timeout_seconds
        while True:
            line = self._readline_before_deadline(process, deadline)
            if not line:
                return_code = process.poll()
                detail = " | ".join(self._diagnostics[-5:])
                raise ContractError(
                    "native JSONL process ended before returning an object"
                    f"; exit={return_code}; diagnostics={detail!r}"
                )
            stripped = line.strip()
            if not stripped.startswith("{"):
                if stripped:
                    self._diagnostic_line_count += 1
                    self._diagnostics.append(stripped[:1000])
                    self._diagnostics = self._diagnostics[-20:]
                continue
            try:
                result = json.loads(stripped)
            except json.JSONDecodeError as error:
                raise ContractError("native JSONL process emitted malformed JSON") from error
            if not isinstance(result, Mapping):
                raise ContractError("native JSONL process result must be an object")
            return result

    def _readline_before_deadline(
        self, process: subprocess.Popen[str], deadline: float
    ) -> str:
        if process.stdout is None:
            raise ContractError("native JSONL process output pipe is unavailable")
        outcome: Queue[tuple[str, Any]] = Queue(maxsize=1)

        def read_line() -> None:
            try:
                outcome.put_nowait(("line", process.stdout.readline()))
            except Exception as error:
                outcome.put_nowait(("error", error))

        reader_thread = Thread(
            target=read_line,
            name="chesstory-native-jsonl-reader",
            daemon=True,
        )
        reader_thread.start()
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise self._timeout_exception(process, reader_thread)
        try:
            kind, value = outcome.get(timeout=remaining)
        except Empty as error:
            raise self._timeout_exception(process, reader_thread) from error
        if kind == "error":
            raise ContractError("native JSONL process output could not be read") from value
        return str(value)

    def _timeout_exception(
        self, process: subprocess.Popen[str], reader_thread: Thread
    ) -> NativeProviderTimeout:
        termination_strategy = (
            "windows-process-tree-force"
            if os.name == "nt"
            else "posix-process-group-kill"
        )
        self._abort_unresponsive_process(process)
        reader_thread.join(timeout=0.5)
        reader_stopped = not reader_thread.is_alive()
        self._close_terminated_pipes(process, close_stdout=reader_stopped)
        return NativeProviderTimeout(
            {
                "failure_kind": "response-timeout",
                "deadline_policy": "whole-invocation-monotonic",
                "configured_timeout_milliseconds": (
                    self.response_timeout_milliseconds
                ),
                "diagnostic_line_count": self._diagnostic_line_count,
                "diagnostic_tail_sha256": [
                    sha256_bytes(line.encode("utf-8"))
                    for line in self._diagnostics
                ],
                "termination": {
                    "attempted": True,
                    "strategy": termination_strategy,
                    "parent_return_code": process.poll(),
                    "reader_stopped": reader_stopped,
                },
            }
        )

    def artifact_binding(self) -> dict[str, Any]:
        """Return an exact but path-opaque binding for captured provider I/O."""

        working_directory = (
            Path.cwd() if self.cwd is None else self.cwd
        ).resolve()
        executable = Path(self.command[0])
        if executable.is_absolute() or executable.parent != Path("."):
            if not executable.is_absolute():
                executable = working_directory / executable
            resolved_executable = executable.resolve()
            executable_path = resolved_executable if resolved_executable.is_file() else None
        else:
            discovered = shutil.which(self.command[0])
            executable_path = Path(discovered).resolve() if discovered else None

        unsigned = {
            "schema_version": NATIVE_COMMAND_PROVIDER_BINDING_SCHEMA,
            "provider_kind": "persistent-jsonl-command",
            "transport": "canonical-jsonl-utf8-stdio",
            "provider_identity_sha256": sha256_json(
                {
                    "module": type(self).__module__,
                    "qualified_name": type(self).__qualname__,
                }
            ),
            "command_argv_sha256": sha256_json(list(self.command)),
            "executable_sha256": (
                sha256_file(executable_path) if executable_path is not None else None
            ),
            "working_directory_sha256": sha256_bytes(
                str(working_directory).encode("utf-8")
            ),
            "response_timeout_milliseconds": self.response_timeout_milliseconds,
        }
        return {**unsigned, "binding_sha256": sha256_json(unsigned)}

    def _abort_unresponsive_process(self, process: subprocess.Popen[str]) -> None:
        if self._process is process:
            self._process = None
        if os.name == "nt":
            try:
                subprocess.run(
                    ["taskkill", "/PID", str(process.pid), "/T", "/F"],
                    check=False,
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    timeout=5,
                    creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
                )
            except (OSError, subprocess.TimeoutExpired):
                pass
        else:
            try:
                os.killpg(process.pid, 9)
            except (OSError, ProcessLookupError):
                pass
        if process.poll() is None:
            try:
                process.kill()
            except OSError:
                pass
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass

    @staticmethod
    def _close_terminated_pipes(
        process: subprocess.Popen[str], *, close_stdout: bool
    ) -> None:
        pipes = [process.stdin]
        if close_stdout:
            pipes.append(process.stdout)
        for pipe in pipes:
            if pipe is not None:
                try:
                    pipe.close()
                except OSError:
                    pass

    def close(self, *, raise_on_failure: bool = True) -> None:
        process = self._process
        if process is None:
            return
        self._process = None
        if process.stdin is not None:
            try:
                process.stdin.close()
            except OSError:
                pass
        try:
            return_code = process.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self._abort_unresponsive_process(process)
            self._close_terminated_pipes(process, close_stdout=True)
            return_code = process.poll()
            if return_code is None:
                return_code = -1
        if raise_on_failure and return_code != 0:
            detail = " | ".join(self._diagnostics[-5:])
            raise ContractError(
                f"native JSONL process exited with {return_code}; diagnostics={detail!r}"
            )

    def _start(self) -> subprocess.Popen[str]:
        if self._process is not None:
            if self._process.poll() is not None:
                raise ContractError("native JSONL process is no longer running")
            return self._process
        try:
            self._process = subprocess.Popen(
                list(self.command),
                cwd=self.cwd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                encoding="utf-8",
                bufsize=1,
                creationflags=(
                    getattr(subprocess, "CREATE_NEW_PROCESS_GROUP", 0)
                    if os.name == "nt"
                    else 0
                ),
                start_new_session=os.name != "nt",
            )
        except OSError as error:
            raise ContractError("native JSONL command could not be started") from error
        return self._process


@dataclass(frozen=True)
class NativeEngineeringSample:
    """The only sample state exposed to the native diagnostic provider."""

    sample_id: str
    atomic_cluster_id: str
    corpus_version: str
    split: str
    runtime_request: Mapping[str, Any]

    def __post_init__(self) -> None:
        for label, value in (
            ("sample_id", self.sample_id),
            ("atomic_cluster_id", self.atomic_cluster_id),
            ("corpus_version", self.corpus_version),
            ("split", self.split),
        ):
            if not isinstance(value, str) or not value.strip():
                raise ContractError(f"native diagnostic {label} must be non-empty text")
        request = _finite_json_object(self.runtime_request, "runtime request")
        if request.get("schema_version") != "chesstory.move-meaning.request.v1":
            raise ContractError("native diagnostic requires a runtime request.v1 envelope")
        if not isinstance(request.get("input"), Mapping):
            raise ContractError("native diagnostic runtime request has no input object")


@dataclass(frozen=True)
class NativeArmRealization:
    """An explicitly non-semantic realization of one frozen-plan shadow arm."""

    kind: str
    directives: Mapping[str, Any]
    directive_producer_tier: str = "deterministic-engineering"

    def __post_init__(self) -> None:
        if self.kind not in _REALIZATION_KINDS:
            raise ContractError(f"unsupported native realization kind {self.kind!r}")
        if self.directive_producer_tier not in _DIRECTIVE_PRODUCER_TIERS:
            raise ContractError(
                f"unsupported native directive producer tier {self.directive_producer_tier!r}"
            )
        directives = _finite_json_object(self.directives, "native directives")
        if self.kind == "identity" and directives:
            raise ContractError("identity native realization must have no directives")
        if self.kind == "native-selection" and not directives:
            raise ContractError("native-selection realization requires directives")

    @classmethod
    def identity(cls) -> NativeArmRealization:
        return cls(kind="identity", directives={})

    def as_document(self) -> dict[str, Any]:
        return {
            "kind": self.kind,
            "directive_producer_tier": self.directive_producer_tier,
            "frozen_source_semantics": "not-claimed",
            "directives": _finite_json_object(self.directives, "native directives"),
        }


def native_canonical_json(value: Any) -> str:
    """Canonical JSON used by the external Scala native adapter.

    Unlike the harness artifact hash, this contract has no trailing newline.
    Native stage hashes must never be verified with ``sha256_json``.
    """

    try:
        return json.dumps(
            value,
            ensure_ascii=False,
            sort_keys=True,
            separators=(",", ":"),
            allow_nan=False,
        )
    except (TypeError, ValueError) as error:
        raise ContractError("native contract value must be finite JSON") from error


def native_sha256_json(value: Any) -> str:
    return hashlib.sha256(native_canonical_json(value).encode("utf-8")).hexdigest()


def native_projection_evaluation_view(
    observation: Mapping[str, Any],
) -> dict[str, Any]:
    """Return a source-blind P/public-response view without claiming a V stage."""

    summary = validate_native_observation(observation)
    if not summary["p_observed"]:
        raise ContractError("native projection view requires an observed P boundary")
    return {
        "schema_version": "chesstory.eval.native-projection-blind-view.v1",
        "native_contract_version": "chesstory.runtime-native-boundary.v2",
        "verbalization_status": "unavailable",
        "public_response_sha256": observation["public_response_sha256"],
        "public_response": copy.deepcopy(observation["public_response"]),
    }


def _validate_native_observation_contract(
    observation: Mapping[str, Any],
    *,
    expected_schema: str,
    expected_native_contract_version: str,
    expected_adapter_version: str,
    expected_response_schema: str,
    expected_runtime_request: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Validate one native observation contract and its exact artifact lineage."""

    document = _finite_json_object(observation, "native runtime observation")
    if document.get("schema_version") != expected_schema:
        raise ContractError("unsupported native runtime observation schema")
    if document.get("native_contract_version") != expected_native_contract_version:
        raise ContractError("unsupported native boundary contract")
    if document.get("hash_contract") != NATIVE_HASH_CONTRACT:
        raise ContractError("unsupported native hash contract")
    status = document.get("observation_status")
    if status not in {"complete", "adapter-error"}:
        raise ContractError("native observation status is invalid")

    required = {
        "schema_version",
        "native_contract_version",
        "observation_status",
        "request_sha256",
        "runtime",
        "hash_contract",
        "artifact_chain_head_sha256",
        "stages",
    }
    optional = {
        "public_response",
        "public_response_sha256",
        "adapter_error",
    }
    if not required.issubset(document) or not set(document).issubset(required | optional):
        raise ContractError("native observation has missing or unknown top-level fields")
    _require_sha256(document.get("request_sha256"), "native request SHA-256")
    if expected_runtime_request is not None:
        expected = native_sha256_json(
            _finite_json_object(expected_runtime_request, "expected runtime request")
        )
        if document["request_sha256"] != expected:
            raise IntegrityError("native observation request hash mismatch")

    runtime = document.get("runtime")
    if not isinstance(runtime, Mapping):
        raise ContractError("native observation runtime metadata is missing")
    runtime_required = {
        "adapter_name",
        "adapter_version",
        "request_schema",
        "response_schema",
        "boundary_execution_present",
        "projection_present",
    }
    runtime_optional = {"http_status"}
    if not runtime_required.issubset(runtime) or not set(runtime).issubset(
        runtime_required | runtime_optional
    ):
        raise ContractError("native runtime metadata fields are not exact")
    if (
        runtime.get("adapter_name") != "chesstory-judgment-runtime-adapter"
        or runtime.get("adapter_version") != expected_adapter_version
        or runtime.get("request_schema") != "chesstory.move-meaning.request.v1"
        or runtime.get("response_schema") != expected_response_schema
    ):
        raise ContractError("native runtime producer identity changed")
    for field in ("boundary_execution_present", "projection_present"):
        if not isinstance(runtime.get(field), bool):
            raise ContractError(f"native runtime {field} must be boolean")

    if status == "complete":
        if "adapter_error" in document:
            raise ContractError("complete native observation contains adapter_error")
        if "public_response" not in document or "public_response_sha256" not in document:
            raise ContractError("complete native observation has no public response")
    else:
        error = document.get("adapter_error")
        if not isinstance(error, Mapping) or not isinstance(error.get("kind"), str):
            raise ContractError("adapter-error observation has no typed error")

    if "public_response" in document:
        response = document["public_response"]
        if not isinstance(response, Mapping) or set(response) != {"http_status", "body"}:
            raise ContractError("native public response is malformed")
        claimed_response_hash = document.get("public_response_sha256")
        _require_sha256(claimed_response_hash, "native public response SHA-256")
        if claimed_response_hash != native_sha256_json(response):
            raise IntegrityError("native public response hash mismatch")
    elif "public_response_sha256" in document:
        raise ContractError("native response hash exists without a response")

    stages = document.get("stages")
    if not isinstance(stages, Mapping) or set(stages) != set(STAGES):
        raise ContractError("native observation must declare the exact Q-to-V stages")
    chain_head: str | None = None
    stage_summaries: dict[str, dict[str, Any]] = {}
    for stage in STAGES:
        record = stages[stage]
        if not isinstance(record, Mapping):
            raise ContractError(f"native {stage} record must be an object")
        base_fields = {
            "stage_id",
            "status",
            "boundary_reached",
            "reason",
            "native_contract_version",
            "v1_semantic_mapping_status",
            "v1_semantic_mapping_reason",
            "upstream_artifact_sha256",
            "artifact_sha256",
        }
        artifact_fields = {"artifact_role", "artifact"}
        if not base_fields.issubset(record) or not set(record).issubset(
            base_fields | artifact_fields
        ):
            raise ContractError(f"native {stage} record fields are not exact")
        if record.get("stage_id") != stage:
            raise IntegrityError(f"native {stage} stage identity mismatch")
        if record.get("native_contract_version") != expected_native_contract_version:
            raise ContractError(f"native {stage} contract version changed")
        if record.get("v1_semantic_mapping_status") != "unavailable":
            raise ContractError(f"native {stage} must not claim frozen-v1 mapping")
        for field in ("reason", "v1_semantic_mapping_reason"):
            if not isinstance(record.get(field), str) or not record[field].strip():
                raise ContractError(f"native {stage} {field} must be non-empty text")
        if not isinstance(record.get("boundary_reached"), bool):
            raise ContractError(f"native {stage} boundary_reached must be boolean")
        if record.get("status") not in {"observed-native", "unavailable"}:
            raise ContractError(f"native {stage} status is invalid")
        if record.get("upstream_artifact_sha256") != chain_head:
            raise IntegrityError(f"native {stage} upstream artifact chain mismatch")

        artifact = record.get("artifact")
        if artifact is None:
            if "artifact_role" in record or record.get("artifact_sha256") is not None:
                raise ContractError(f"native {stage} partial artifact declaration")
            if record.get("status") == "observed-native":
                raise ContractError(f"observed native {stage} has no artifact")
            artifact_hash = None
        else:
            if not isinstance(artifact, Mapping):
                raise ContractError(f"native {stage} artifact must be an object")
            if set(artifact) != {
                "native_contract_version",
                "native_tree_contract_version",
                "stage",
                "capture_role",
                "upstream_artifact_sha256",
                "native_tree",
            }:
                raise ContractError(f"native {stage} artifact fields are not exact")
            if (
                artifact.get("native_contract_version")
                != expected_native_contract_version
                or artifact.get("native_tree_contract_version")
                != "chesstory.runtime-native-tree.v2"
                or artifact.get("stage") != stage
                or artifact.get("upstream_artifact_sha256") != chain_head
                or artifact.get("capture_role") != record.get("artifact_role")
            ):
                raise IntegrityError(f"native {stage} artifact binding mismatch")
            if artifact.get("capture_role") not in {
                "observed-native-output",
                "observed-native-input-output",
                "typed-unavailability",
            }:
                raise ContractError(f"native {stage} artifact role is invalid")
            if not isinstance(artifact.get("native_tree"), Mapping):
                raise ContractError(f"native {stage} tree is missing")
            _validate_native_tree(artifact["native_tree"], label=f"native {stage} tree")
            artifact_hash = native_sha256_json(artifact)
            _require_sha256(record.get("artifact_sha256"), f"native {stage} artifact SHA-256")
            if record.get("artifact_sha256") != artifact_hash:
                raise IntegrityError(f"native {stage} artifact hash mismatch")
            chain_head = artifact_hash

        if record.get("status") == "observed-native" and record.get(
            "boundary_reached"
        ) is not True:
            raise ContractError(f"observed native {stage} did not reach its boundary")
        stage_summaries[stage] = {
            "status": record["status"],
            "boundary_reached": record["boundary_reached"],
            "reason": record["reason"],
            "artifact_sha256": artifact_hash,
        }

    if document.get("artifact_chain_head_sha256") != chain_head:
        raise IntegrityError("native observation artifact chain head mismatch")
    if status == "complete":
        judgment_observed = all(
            stage_summaries[stage]["status"] == "observed-native"
            for stage in ("Q", "F", "C", "Jp", "Ja", "R")
        )
        if bool(runtime["boundary_execution_present"]) != judgment_observed:
            raise IntegrityError("native judgment-boundary presence mismatch")
        p_observed = stage_summaries["P"]["status"] == "observed-native"
        if bool(runtime["projection_present"]) != p_observed:
            raise IntegrityError("native projection-boundary presence mismatch")
        v_record = stages["V"]
        if (
            v_record.get("status") != "unavailable"
            or v_record.get("boundary_reached") is not False
            or v_record.get("artifact_role") != "typed-unavailability"
            or not isinstance(v_record.get("artifact"), Mapping)
        ):
            raise ContractError("native V must remain a captured typed unavailability")

    first_unavailable = next(
        (
            stage
            for stage in NATIVE_OBSERVED_STAGES
            if stage_summaries[stage]["status"] != "observed-native"
        ),
        None,
    )
    q_p_complete = first_unavailable is None
    return {
        "observation_status": status,
        "q_p_complete": q_p_complete,
        "q_r_complete": all(
            stage_summaries[stage]["status"] == "observed-native"
            for stage in ("Q", "F", "C", "Jp", "Ja", "R")
        ),
        "p_observed": stage_summaries["P"]["status"] == "observed-native",
        "v_status": "typed-unavailable"
        if isinstance(stages["V"].get("artifact"), Mapping)
        else "unavailable",
        "first_unavailable_stage": first_unavailable,
        "artifact_chain_head_sha256": chain_head,
        "stages": stage_summaries,
    }


def validate_native_observation(
    observation: Mapping[str, Any],
    *,
    expected_runtime_request: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Validate native-v2 structural artifacts and their exact hash lineage."""

    return _validate_native_observation_contract(
        observation,
        expected_schema=NATIVE_OBSERVATION_SCHEMA,
        expected_native_contract_version="chesstory.runtime-native-boundary.v2",
        expected_adapter_version="0.2.0",
        expected_response_schema="chesstory.move-meaning.response.v2",
        expected_runtime_request=expected_runtime_request,
    )


def validate_native_engineering_result(
    result: Mapping[str, Any],
    *,
    invocation: Mapping[str, Any],
) -> dict[str, Any]:
    """Validate the external adapter wrapper and return its observation summary."""

    document = _finite_json_object(result, "native engineering result")
    if document.get("schema_version") != NATIVE_ENGINEERING_RESULT_SCHEMA:
        raise ContractError("unsupported native engineering result schema")
    if document.get("production_attribution_eligible") is not False or document.get(
        "inference_eligible"
    ) is not False:
        raise ContractError("native engineering result may not claim inference eligibility")
    if document.get("invocation_sha256") != native_sha256_json(invocation):
        raise IntegrityError("native engineering invocation hash mismatch")
    if document.get("binding") != invocation.get("binding"):
        raise IntegrityError("native engineering result binding mismatch")
    status = document.get("status")
    if status not in {"completed", "typed-unavailable", "adapter-error"}:
        raise ContractError("native engineering result status is invalid")
    receipts = document.get("directive_receipts")
    if not isinstance(receipts, list):
        raise ContractError("native engineering directive receipts are missing")
    for receipt in receipts:
        _validate_receipt(receipt)

    if status != "adapter-error":
        realization = document.get("realization")
        expected = invocation.get("realization")
        if not isinstance(expected, Mapping):
            raise ContractError("native engineering invocation has no realization")
        expected_echo = {
            "kind": expected.get("kind"),
            "directive_producer_tier": expected.get("directive_producer_tier"),
            "frozen_source_semantics": "not-claimed",
        }
        if realization != expected_echo:
            raise IntegrityError("native engineering realization echo mismatch")

    expected_request = invocation.get("runtime_request")
    if not isinstance(expected_request, Mapping):
        raise ContractError("native engineering invocation has no runtime request")
    if status == "completed":
        observation = document.get("observation")
        if not isinstance(observation, Mapping):
            raise ContractError("completed native engineering result has no observation")
        summary = validate_native_observation(
            observation, expected_runtime_request=expected_request
        )
    elif status == "typed-unavailable":
        reason = document.get("unavailability_reason")
        if not isinstance(reason, str) or not reason:
            raise ContractError("typed-unavailable native result has no reason")
        baseline = document.get("baseline_observation")
        if not isinstance(baseline, Mapping):
            raise ContractError("typed-unavailable native result has no baseline observation")
        baseline_summary = validate_native_observation(
            baseline, expected_runtime_request=expected_request
        )
        attempted = document.get("attempted_observation")
        if attempted is not None:
            if not isinstance(attempted, Mapping):
                raise ContractError("attempted native observation must be an object")
            validate_native_observation(
                attempted, expected_runtime_request=expected_request
            )
        summary = {
            **baseline_summary,
            "q_p_complete": False,
            "typed_unavailability_reason": reason,
        }
    else:
        error = document.get("adapter_error")
        if not isinstance(error, Mapping) or not isinstance(error.get("kind"), str):
            raise ContractError("native adapter error result has no typed error")
        summary = {
            "observation_status": "adapter-error",
            "q_p_complete": False,
            "q_r_complete": False,
            "p_observed": False,
            "v_status": "unavailable",
            "first_unavailable_stage": "Q",
            "artifact_chain_head_sha256": None,
            "stages": {},
            "adapter_error_kind": error["kind"],
        }
    return {"status": status, "summary": summary, "document": document}


class NativeEngineeringRunner:
    """Execute the frozen arm universe as a non-inferential native-v2 shadow.

    Missing semantic oracle/format-control realizations become explicit rows;
    they are never replaced with identity output.  Native selections can be
    executed for engineering diagnosis, but are never treated as frozen-v1
    oracle artifacts or as evidence for a production bottleneck.
    """

    def __init__(
        self,
        *,
        provider: NativeResultProvider | Callable[[Mapping[str, Any]], Mapping[str, Any]],
        store: ArtifactStore | None,
        context: RunContext,
        preregistration: Mapping[str, Any],
    ) -> None:
        self.provider = provider
        self.store = store
        self.context = context
        self.preregistration = _finite_json_object(
            preregistration, "native diagnostic preregistration"
        )
        if context.split != "diagnostic-explore":
            raise ContractError(
                "native engineering runner is restricted to diagnostic-explore"
            )

    def run(
        self,
        samples: Sequence[NativeEngineeringSample],
        *,
        realizations: Mapping[str, NativeArmRealization] | None = None,
        row_realizations: Mapping[
            tuple[str, str], NativeArmRealization
        ] | None = None,
        model_proxy_aggregates: Mapping[tuple[str, str], Mapping[str, Any]] | None = None,
    ) -> dict[str, Any]:
        if not samples:
            raise ContractError("native engineering diagnostic sample set is empty")
        if any(sample.split != self.context.split for sample in samples):
            raise ContractError("native engineering sample split mismatch")
        sample_ids = [sample.sample_id for sample in samples]
        if len(sample_ids) != len(set(sample_ids)):
            raise ContractError("native engineering samples repeat sample IDs")

        chains = self._oracle_chains()
        plan = build_arm_plan(chains)
        plan_metadata = [_arm_metadata(arm) for arm in plan]
        plan_sha256 = sha256_json(plan_metadata)
        by_arm = {arm.arm_id: arm for arm in plan}
        realization_map = dict(realizations or {})
        unknown = set(realization_map) - set(by_arm)
        if unknown:
            raise ContractError(f"native realizations contain unknown arms {sorted(unknown)}")
        row_realization_map = dict(row_realizations or {})
        valid_pairs = {
            (sample.sample_id, arm.arm_id) for sample in samples for arm in plan
        }
        unknown_rows = set(row_realization_map) - valid_pairs
        if unknown_rows:
            raise ContractError("native row realizations contain unknown sample/arm rows")
        baseline_arm = next(
            arm
            for arm in plan
            if all(source == Source.ACTUAL for source in arm.sources.values())
        )
        for arm_id, realization in realization_map.items():
            if not isinstance(realization, NativeArmRealization):
                raise ContractError(f"native realization for {arm_id} has the wrong type")
            if realization.kind == "identity" and arm_id != baseline_arm.arm_id:
                raise ContractError(
                    "identity output cannot realize a non-actual frozen shadow arm"
                )
        for (sample_id, arm_id), realization in row_realization_map.items():
            if not isinstance(realization, NativeArmRealization):
                raise ContractError(
                    f"native row realization for {sample_id}/{arm_id} has the wrong type"
                )
            if realization.kind == "identity" and arm_id != baseline_arm.arm_id:
                raise ContractError(
                    "identity output cannot realize a non-actual frozen shadow row"
                )
        supplied_baseline = realization_map.get(baseline_arm.arm_id)
        if supplied_baseline is not None and supplied_baseline.kind != "identity":
            raise ContractError(
                "actual baseline arm may only use the forced identity realization"
            )
        realization_map[baseline_arm.arm_id] = NativeArmRealization.identity()
        for sample in samples:
            baseline_key = (sample.sample_id, baseline_arm.arm_id)
            supplied_row = row_realization_map.get(baseline_key)
            if supplied_row is not None and supplied_row.kind != "identity":
                raise ContractError(
                    "actual baseline row may only use the forced identity realization"
                )
            row_realization_map[baseline_key] = NativeArmRealization.identity()

        proxy_map = dict(model_proxy_aggregates or {})
        unknown_proxy = set(proxy_map) - valid_pairs
        if unknown_proxy:
            raise ContractError("model-proxy aggregates contain unknown sample/arm rows")

        rows: list[dict[str, Any]] = []
        provider_invocations = 0
        invoked_arm_ids: set[str] = set()
        provider_binding = self._provider_binding()
        if provider_binding.get("provider_kind") == "persistent-jsonl-command":
            if (
                provider_binding.get("schema_version")
                != NATIVE_COMMAND_PROVIDER_BINDING_SCHEMA
            ):
                raise ContractError(
                    "legacy command provider bindings are validation-only"
                )
            if self.store is None:
                raise ContractError(
                    "v2 command provider execution requires an immutable artifact store"
                )
        for arm in plan:
            for sample in samples:
                realization = row_realization_map.get(
                    (sample.sample_id, arm.arm_id), realization_map.get(arm.arm_id)
                )
                if realization is None:
                    rows.append(
                        self._unrealized_row(
                            sample,
                            arm,
                            "frozen-v1-source-arm-has-no-native-v2-realization",
                        )
                    )
                    continue
                invocation = self._invocation(sample, arm, realization, plan_sha256)
                invocation_artifact = self._capture_artifact(
                    f"native-invocation--{sample.sample_id}--{arm.arm_id}",
                    invocation,
                )
                try:
                    provider_result = self._invoke_provider(invocation)
                except NativeProviderTimeout as error:
                    failure_document = _validate_provider_failure(
                        {
                            "schema_version": NATIVE_PROVIDER_FAILURE_SCHEMA,
                            "status": "provider-timeout",
                            "production_attribution_eligible": False,
                            "inference_eligible": False,
                            "production_bottleneck": None,
                            "provider_binding": provider_binding,
                            "invocation_sha256": sha256_json(invocation),
                            "invocation_artifact": invocation_artifact,
                            "failure": error.details,
                        },
                        invocation=invocation,
                        invocation_artifact=invocation_artifact,
                        provider_binding=provider_binding,
                    )
                    self._capture_artifact(
                        f"native-provider-failure--{sample.sample_id}--{arm.arm_id}",
                        failure_document,
                    )
                    if self.store is not None:
                        self.store.verify()
                    raise
                raw_result = _finite_json_object(
                    provider_result, "raw native provider result"
                )
                provider_invocations += 1
                invoked_arm_ids.add(arm.arm_id)
                provider_io = _validate_provider_io(
                    {
                        "schema_version": NATIVE_ENGINEERING_PROVIDER_IO_SCHEMA,
                        "provider_binding": provider_binding,
                        "invocation_sha256": sha256_json(invocation),
                        "raw_result_sha256": sha256_json(raw_result),
                        "invocation": invocation,
                        "raw_result": raw_result,
                    },
                    invocation=invocation,
                    raw_result=raw_result,
                    provider_binding=provider_binding,
                )
                provider_io_artifact = self._capture_artifact(
                    f"native-provider-io--{sample.sample_id}--{arm.arm_id}",
                    provider_io,
                )
                validated = validate_native_engineering_result(
                    raw_result, invocation=invocation
                )
                result_document = validated["document"]
                result_artifact = self._capture_artifact(
                    f"native-result--{sample.sample_id}--{arm.arm_id}",
                    result_document,
                )

                proxy = proxy_map.get((sample.sample_id, arm.arm_id))
                proxy_endpoint = None
                if proxy is not None:
                    if validated["status"] != "completed" or not validated["summary"][
                        "p_observed"
                    ]:
                        raise ContractError(
                            "model-proxy endpoint requires a completed observed P boundary"
                        )
                    proxy_endpoint = _validate_model_proxy_aggregate(proxy)
                rows.append(
                    self._completed_or_unavailable_row(
                        sample=sample,
                        arm=arm,
                        realization=realization,
                        validated=validated,
                        invocation_artifact=invocation_artifact,
                        provider_io_artifact=provider_io_artifact,
                        result_artifact=result_artifact,
                        proxy_endpoint=proxy_endpoint,
                    )
                )

        atomic_cluster_count = len({sample.atomic_cluster_id for sample in samples})
        minimum_clusters = self._minimum_clusters()
        baseline_rows = [row for row in rows if row["arm_id"] == baseline_arm.arm_id]
        exact_baseline_rows = (
            len(baseline_rows) == len(samples)
            and {row["sample_id"] for row in baseline_rows} == set(sample_ids)
        )
        actual_identity_complete = exact_baseline_rows and all(
            row["realization_kind"] == "identity" for row in baseline_rows
        )
        actual_semantic_source_complete = exact_baseline_rows and all(
            row["semantic_source_realization"] == "actual-native"
            for row in baseline_rows
        )
        actual_q_p_complete = (
            actual_identity_complete
            and actual_semantic_source_complete
            and all(row["status"] == "q-p-observed" for row in baseline_rows)
        )
        completed_row_statuses = {"q-p-observed", "native-chain-partial"}
        completed_row_count = sum(
            row["status"] in completed_row_statuses for row in rows
        )
        typed_unavailable_row_count = sum(
            row["status"] == "typed-unavailable" for row in rows
        )
        rows_by_arm = {
            arm.arm_id: [row for row in rows if row["arm_id"] == arm.arm_id]
            for arm in plan
        }
        completed_arm_ids = {
            arm_id
            for arm_id, arm_rows in rows_by_arm.items()
            if len(arm_rows) == len(samples)
            and all(row["status"] in completed_row_statuses for row in arm_rows)
        }
        descriptive_candidates = _descriptive_proxy_candidates(
            rows, baseline_arm.arm_id
        )
        foundation_reasons = [
            "native-v2-does-not-realize-frozen-v1-oracle-or-format-control-semantics",
            "runtime-verbalization-boundary-is-typed-unavailable",
            "held-out-human-endpoint-is-not-part-of-native-engineering-mode",
            "two-independent-semantic-oracle-chains-are-not-realized",
        ]
        if atomic_cluster_count < minimum_clusters:
            foundation_reasons.append(
                f"atomic-clusters-underpowered:{atomic_cluster_count}<{minimum_clusters}"
            )
        if not actual_q_p_complete:
            foundation_reasons.append("actual-native-Q-to-P-chain-is-incomplete")

        report = {
            "schema_version": NATIVE_ENGINEERING_REPORT_SCHEMA,
            "mode": "underpowered-engineering-diagnostic",
            "binding": {
                "run_id": self.context.run_id,
                "corpus_version": self.context.corpus_version,
                "split": self.context.split,
                "candidate_manifest_hash": self.context.candidate_manifest_hash,
                "preregistration_sha256": sha256_json(self.preregistration),
            },
            "created_at": utc_now(),
            "provider_binding": provider_binding,
            "plan": {
                "origin": "frozen-v1-arm-universe-native-v2-shadow",
                "plan_sha256": plan_sha256,
                "registered_arm_count": len(plan),
                "planned_row_count": len(plan) * len(samples),
                "materialized_row_count": len(rows),
                "provider_invocation_count": provider_invocations,
                "completed_row_count": completed_row_count,
                "typed_unavailable_row_count": typed_unavailable_row_count,
                "invoked_arm_count": len(invoked_arm_ids),
                "completed_arm_count": len(completed_arm_ids),
                "frozen_semantic_plan_complete": False,
                "arms": plan_metadata,
            },
            "sample_count": len(samples),
            "atomic_cluster_count": atomic_cluster_count,
            "minimum_atomic_clusters": minimum_clusters,
            "actual_native_chain": {
                "arm_id": baseline_arm.arm_id,
                "identity_realization_for_every_sample": actual_identity_complete,
                "semantic_source_actual_native_for_every_sample": (
                    actual_semantic_source_complete
                ),
                "Q_to_P_observed_for_every_sample": actual_q_p_complete,
                "V_status": "typed-unavailable",
            },
            "foundation_gate": {
                "status": "closed",
                "formal_release_gate_evaluated": False,
                "reasons": foundation_reasons,
            },
            "endpoint_evidence": {
                "held_out_human": {
                    "status": "unavailable",
                    "count": 0,
                    "eligible_for_release_gate": False,
                },
                "model_proxy": {
                    "evidence_tier": MODEL_PROXY_TIER,
                    "aggregate_count": sum(
                        (row.get("descriptive_endpoint") or {}).get("evidence_tier")
                        == MODEL_PROXY_TIER
                        for row in rows
                    ),
                    "human_gate_eligible": False,
                    "release_eligible": False,
                    "attribution_eligible": False,
                },
            },
            "descriptive_candidates": descriptive_candidates,
            "inference": {
                "status": "not-run",
                "reason": "engineering-mode-is-hard-sealed-against-inference",
                "confidence_intervals": None,
                "p_values": None,
                "multiplicity_adjustment": None,
            },
            "attribution": {
                "status": "indeterminate",
                "production_bottleneck": None,
                "production_bottleneck_kind": None,
                "production_interface_bottleneck": None,
                "reason": "native-engineering-results-are-descriptive-only",
            },
            "production_bottleneck": None,
            "rows": rows,
        }
        if self.store is not None:
            self.store.capture_run_document("native-engineering-report", report)
            self.store.verify()
        return report

    def _invoke_provider(self, invocation: Mapping[str, Any]) -> Mapping[str, Any]:
        invoke = getattr(self.provider, "invoke", None)
        result = (
            invoke(invocation)
            if callable(invoke)
            else self.provider(invocation)  # type: ignore[misc,operator]
        )
        if not isinstance(result, Mapping):
            raise ContractError("native result provider must return an object")
        return result

    def _provider_binding(self) -> dict[str, Any]:
        binding_factory = getattr(self.provider, "artifact_binding", None)
        if callable(binding_factory):
            binding = _finite_json_object(
                binding_factory(), "native provider binding"
            )
        else:
            invoke = getattr(self.provider, "invoke", None)
            callable_provider = invoke if callable(invoke) else self.provider
            identity = {
                "module": getattr(
                    callable_provider,
                    "__module__",
                    type(callable_provider).__module__,
                ),
                "qualified_name": getattr(
                    callable_provider, "__qualname__", type(callable_provider).__qualname__
                ),
            }
            unsigned = {
                "schema_version": NATIVE_PROVIDER_BINDING_SCHEMA,
                "provider_kind": "in-process-callable",
                "transport": "python-mapping-call",
                "provider_identity_sha256": sha256_json(identity),
                "command_argv_sha256": None,
                "executable_sha256": None,
                "working_directory_sha256": None,
            }
            binding = {**unsigned, "binding_sha256": sha256_json(unsigned)}
        return _validate_provider_binding(binding)

    def _capture_artifact(
        self, name: str, document: Mapping[str, Any]
    ) -> dict[str, Any]:
        artifact_document = _finite_json_object(document, f"{name} artifact")
        expected = sha256_json(artifact_document)
        reference: dict[str, Any] = {"sha256": expected}
        if self.store is not None:
            captured = self.store.capture_run_document(name, artifact_document)
            if captured != expected:
                raise IntegrityError(f"{name} artifact hash mismatch after capture")
            filename = self.store._segment(name)
            reference["path"] = (
                filename if filename.endswith(".json") else f"{filename}.json"
            )
        return reference

    def _invocation(
        self,
        sample: NativeEngineeringSample,
        arm: Arm,
        realization: NativeArmRealization,
        plan_sha256: str,
    ) -> dict[str, Any]:
        return {
            "schema_version": NATIVE_ENGINEERING_INVOCATION_SCHEMA,
            "binding": {
                "run_id": self.context.run_id,
                "sample_id": sample.sample_id,
                "atomic_cluster_id": sample.atomic_cluster_id,
                "arm_id": arm.arm_id,
                "plan_sha256": plan_sha256,
            },
            "runtime_request": _finite_json_object(
                sample.runtime_request, "runtime request"
            ),
            "realization": realization.as_document(),
        }

    def _unrealized_row(
        self, sample: NativeEngineeringSample, arm: Arm, reason: str
    ) -> dict[str, Any]:
        return {
            **_row_binding(sample, arm),
            "status": "typed-unavailable",
            "realization_kind": "unavailable",
            "semantic_source_realization": "unavailable",
            "reason": reason,
            "native_chain": None,
            "descriptive_endpoint": None,
            "production_attribution_eligible": False,
        }

    def _completed_or_unavailable_row(
        self,
        *,
        sample: NativeEngineeringSample,
        arm: Arm,
        realization: NativeArmRealization,
        validated: Mapping[str, Any],
        invocation_artifact: Mapping[str, Any],
        provider_io_artifact: Mapping[str, Any],
        result_artifact: Mapping[str, Any],
        proxy_endpoint: Mapping[str, Any] | None,
    ) -> dict[str, Any]:
        result_status = str(validated["status"])
        summary = validated["summary"]
        if result_status == "completed" and summary["q_p_complete"]:
            row_status = "q-p-observed"
            reason = "native-Q-to-P-artifact-chain-validated"
        elif result_status == "completed":
            row_status = "native-chain-partial"
            reason = f"first-unavailable-native-stage:{summary['first_unavailable_stage']}"
        elif result_status == "typed-unavailable":
            row_status = "typed-unavailable"
            reason = str(summary.get("typed_unavailability_reason"))
        else:
            row_status = "adapter-error"
            reason = str(summary.get("adapter_error_kind"))
        endpoint = None
        if proxy_endpoint is not None:
            endpoint = {
                "evidence_tier": MODEL_PROXY_TIER,
                "status": proxy_endpoint["status"],
                "engineering_proxy_E": proxy_endpoint["engineering_proxy_E"],
                "human_gate_eligible": False,
                "release_eligible": False,
                "attribution_eligible": False,
                "aggregate_sha256": sha256_json(proxy_endpoint),
            }
        return {
            **_row_binding(sample, arm),
            "status": row_status,
            "realization_kind": realization.kind,
            "directive_producer_tier": realization.directive_producer_tier,
            "semantic_source_realization": "actual-native"
            if realization.kind == "identity"
            else "unavailable",
            "reason": reason,
            "native_chain": summary,
            "invocation_artifact": dict(invocation_artifact),
            "provider_io_artifact": dict(provider_io_artifact),
            "result_artifact": dict(result_artifact),
            "descriptive_endpoint": endpoint,
            "production_attribution_eligible": False,
        }

    def _oracle_chains(self) -> tuple[str, str]:
        value = self.preregistration.get("oracle_chains")
        if (
            not isinstance(value, list)
            or len(value) != 2
            or not all(isinstance(item, str) and item for item in value)
            or len(set(value)) != 2
        ):
            raise ContractError("native diagnostic preregistration requires two oracle chains")
        return value[0], value[1]

    def _minimum_clusters(self) -> int:
        counts = self.preregistration.get("minimum_effective_counts")
        if not isinstance(counts, Mapping):
            raise ContractError("preregistration minimum effective counts are missing")
        value = counts.get("atomic_clusters")
        if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
            raise ContractError("preregistered atomic-cluster minimum is invalid")
        return value


def _validate_receipt(value: Any) -> None:
    if not isinstance(value, Mapping):
        raise ContractError("native directive receipt must be an object")
    expected = {
        "stage",
        "operation",
        "collection",
        "status",
        "available_item_ids",
        "selected_item_ids",
        "candidate_inventory_sha256",
    }
    if set(value) != expected:
        raise ContractError("native directive receipt fields are not exact")
    if value.get("stage") not in {"Q", "F", "C", "Jp", "R", "P"}:
        raise ContractError("native directive receipt stage is unsupported")
    if value.get("operation") != "select-existing" or value.get("status") != "applied":
        raise ContractError("native directive receipt operation/status changed")
    available = value.get("available_item_ids")
    selected = value.get("selected_item_ids")
    for label, items in (("available", available), ("selected", selected)):
        if (
            not isinstance(items, list)
            or any(not isinstance(item, str) or not item for item in items)
            or len(items) != len(set(items))
        ):
            raise ContractError(f"native receipt {label} IDs are malformed")
    if not set(selected).issubset(set(available)):
        raise IntegrityError("native receipt selected IDs were not observed candidates")
    _require_sha256(
        value.get("candidate_inventory_sha256"), "candidate inventory SHA-256"
    )
    if value["candidate_inventory_sha256"] != native_sha256_json(available):
        raise IntegrityError("native candidate inventory hash mismatch")


def _validate_native_tree(value: Any, *, label: str) -> None:
    """Validate the complete deterministic native-tree v2 structural universe."""

    node_count = 0

    def nonempty_text(item: Any, location: str) -> str:
        if not isinstance(item, str) or not item:
            raise ContractError(f"{location} must be non-empty text")
        return item

    def exact(node: Mapping[str, Any], fields: set[str], location: str) -> None:
        if set(node) != fields:
            raise ContractError(f"{location} fields are not exact")

    def integer(item: Any, location: str, *, minimum: int | None = None) -> int:
        if isinstance(item, bool) or not isinstance(item, int):
            raise ContractError(f"{location} must be an integer")
        if minimum is not None and item < minimum:
            raise ContractError(f"{location} is below its minimum")
        return item

    def items(node: Mapping[str, Any], field: str, location: str) -> list[Any]:
        result = node.get(field)
        if not isinstance(result, list):
            raise ContractError(f"{location}.{field} must be an array")
        return result

    def visit(node: Any, location: str, depth: int) -> None:
        nonlocal node_count
        node_count += 1
        if node_count > 2_000_000:
            raise ContractError(f"{label} exceeds the native node-count limit")
        if depth > 512:
            raise ContractError(f"{label} exceeds the native depth limit")
        if not isinstance(node, Mapping):
            raise ContractError(f"{location} must be a native node object")
        kind = node.get("node_kind")
        if kind == "null":
            exact(node, {"node_kind"}, location)
        elif kind == "boolean":
            exact(node, {"node_kind", "scala_type", "value"}, location)
            if node.get("scala_type") != "scala.Boolean" or not isinstance(
                node.get("value"), bool
            ):
                raise ContractError(f"{location} boolean node is malformed")
        elif kind == "string":
            exact(node, {"node_kind", "scala_type", "value"}, location)
            if node.get("scala_type") != "java.lang.String" or not isinstance(
                node.get("value"), str
            ):
                raise ContractError(f"{location} string node is malformed")
        elif kind == "char":
            exact(
                node,
                {"node_kind", "scala_type", "code_point", "value"},
                location,
            )
            point = integer(node.get("code_point"), f"{location}.code_point", minimum=0)
            char = node.get("value")
            if (
                node.get("scala_type") != "scala.Char"
                or point > 65535
                or not isinstance(char, str)
                or len(char) != 1
                or ord(char) != point
            ):
                raise ContractError(f"{location} char node is malformed")
        elif kind == "number":
            encoding = node.get("encoding")
            base = {"node_kind", "scala_type", "encoding", "value"}
            if encoding == "base10":
                exact(node, base, location)
                if not re.fullmatch(r"-?[0-9]+", str(node.get("value", ""))):
                    raise ContractError(f"{location} base10 number is malformed")
            elif encoding == "ieee754-hex":
                exact(node, base | {"raw_bits"}, location)
                if not isinstance(node.get("value"), str) or not re.fullmatch(
                    r"[0-9a-f]{8}(?:[0-9a-f]{8})?", str(node.get("raw_bits", ""))
                ):
                    raise ContractError(f"{location} floating number is malformed")
            elif encoding == "unscaled-base10-with-scale":
                exact(
                    node,
                    base | {"unscaled_value", "scale", "precision"},
                    location,
                )
                if (
                    not isinstance(node.get("value"), str)
                    or not re.fullmatch(r"-?[0-9]+", str(node.get("unscaled_value", "")))
                    or integer(node.get("precision"), f"{location}.precision", minimum=1)
                    < 1
                ):
                    raise ContractError(f"{location} decimal number is malformed")
                integer(node.get("scale"), f"{location}.scale")
            else:
                raise ContractError(f"{location} number encoding is invalid")
            nonempty_text(node.get("scala_type"), f"{location}.scala_type")
        elif kind == "option":
            variant = node.get("variant")
            if variant == "Some":
                exact(node, {"node_kind", "scala_type", "variant", "value"}, location)
                visit(node["value"], f"{location}.value", depth + 1)
            elif variant == "None":
                exact(node, {"node_kind", "scala_type", "variant"}, location)
            else:
                raise ContractError(f"{location} option variant is invalid")
            if node.get("scala_type") != "scala.Option":
                raise ContractError(f"{location} option Scala type changed")
        elif kind == "either":
            exact(node, {"node_kind", "scala_type", "variant", "value"}, location)
            if node.get("scala_type") != "scala.Either" or node.get("variant") not in {
                "Left",
                "Right",
            }:
                raise ContractError(f"{location} either node is malformed")
            visit(node["value"], f"{location}.value", depth + 1)
        elif kind in {"array", "sequence", "iterable", "set"}:
            exact(node, {"node_kind", "collection_type", "values"}, location)
            nonempty_text(node.get("collection_type"), f"{location}.collection_type")
            values = items(node, "values", location)
            for index, child in enumerate(values):
                visit(child, f"{location}.values[{index}]", depth + 1)
            if kind == "set":
                canonical = [native_canonical_json(child) for child in values]
                if canonical != sorted(canonical) or len(canonical) != len(set(canonical)):
                    raise IntegrityError(f"{location} set order/uniqueness changed")
        elif kind == "map":
            exact(node, {"node_kind", "collection_type", "entries"}, location)
            nonempty_text(node.get("collection_type"), f"{location}.collection_type")
            entries = items(node, "entries", location)
            for index, entry in enumerate(entries):
                if not isinstance(entry, Mapping):
                    raise ContractError(f"{location}.entries[{index}] must be an object")
                exact(entry, {"key", "value"}, f"{location}.entries[{index}]")
                visit(entry["key"], f"{location}.entries[{index}].key", depth + 1)
                visit(entry["value"], f"{location}.entries[{index}].value", depth + 1)
            canonical = [native_canonical_json(entry) for entry in entries]
            if canonical != sorted(canonical):
                raise IntegrityError(f"{location} map entry order changed")
        elif kind == "product":
            exact(
                node,
                {"node_kind", "scala_type", "constructor", "arity", "fields"},
                location,
            )
            nonempty_text(node.get("scala_type"), f"{location}.scala_type")
            nonempty_text(node.get("constructor"), f"{location}.constructor")
            arity = integer(node.get("arity"), f"{location}.arity", minimum=0)
            fields = items(node, "fields", location)
            if len(fields) != arity:
                raise ContractError(f"{location} product arity mismatch")
            for index, field in enumerate(fields):
                if not isinstance(field, Mapping):
                    raise ContractError(f"{location}.fields[{index}] must be an object")
                exact(field, {"index", "name", "value"}, f"{location}.fields[{index}]")
                if field.get("index") != index or not isinstance(field.get("name"), str):
                    raise IntegrityError(f"{location} product field order changed")
                visit(field["value"], f"{location}.fields[{index}].value", depth + 1)
        elif kind == "java-enum":
            exact(node, {"node_kind", "scala_type", "constant"}, location)
            nonempty_text(node.get("scala_type"), f"{location}.scala_type")
            nonempty_text(node.get("constant"), f"{location}.constant")
        elif kind == "play-json":
            json_kind = node.get("json_node_type")
            if json_kind == "null":
                exact(node, {"node_kind", "json_node_type"}, location)
            elif json_kind == "boolean":
                exact(node, {"node_kind", "json_node_type", "value"}, location)
                if not isinstance(node.get("value"), bool):
                    raise ContractError(f"{location} Play boolean is malformed")
            elif json_kind == "string":
                exact(node, {"node_kind", "json_node_type", "value"}, location)
                if not isinstance(node.get("value"), str):
                    raise ContractError(f"{location} Play string is malformed")
            elif json_kind == "number":
                exact(
                    node,
                    {
                        "node_kind",
                        "json_node_type",
                        "value",
                        "unscaled_value",
                        "scale",
                        "precision",
                    },
                    location,
                )
                if (
                    not isinstance(node.get("value"), str)
                    or not re.fullmatch(r"-?[0-9]+", str(node.get("unscaled_value", "")))
                ):
                    raise ContractError(f"{location} Play number is malformed")
                integer(node.get("scale"), f"{location}.scale")
                integer(node.get("precision"), f"{location}.precision", minimum=1)
            elif json_kind == "array":
                exact(node, {"node_kind", "json_node_type", "values"}, location)
                for index, child in enumerate(items(node, "values", location)):
                    visit(child, f"{location}.values[{index}]", depth + 1)
            elif json_kind == "object":
                exact(node, {"node_kind", "json_node_type", "fields"}, location)
                fields = items(node, "fields", location)
                names: list[str] = []
                for index, field in enumerate(fields):
                    if not isinstance(field, Mapping):
                        raise ContractError(f"{location}.fields[{index}] must be an object")
                    exact(field, {"name", "value"}, f"{location}.fields[{index}]")
                    if not isinstance(field.get("name"), str):
                        raise ContractError(f"{location}.fields[{index}].name must be text")
                    names.append(str(field["name"]))
                    visit(field["value"], f"{location}.fields[{index}].value", depth + 1)
                if names != sorted(names) or len(names) != len(set(names)):
                    raise IntegrityError(f"{location} Play object field order changed")
            else:
                raise ContractError(f"{location} Play JSON node type is invalid")
        else:
            raise ContractError(f"{location} has unsupported native node kind {kind!r}")

    visit(value, label, 0)


def _validate_model_proxy_aggregate(value: Mapping[str, Any]) -> dict[str, Any]:
    row = _finite_json_object(value, "model-proxy aggregate")
    if row.get("schema_version") != "chesstory.eval.blind-model-proxy-aggregate.v1":
        raise ContractError("unsupported model-proxy aggregate schema")
    if row.get("evidence_tier") != MODEL_PROXY_TIER:
        raise ContractError("native diagnostic model-proxy tier mismatch")
    if (
        row.get("human_gate_eligible") is not False
        or row.get("release_eligible") is not False
        or row.get("production_bottleneck") is not None
    ):
        raise ContractError("model-proxy aggregate may not claim human/release eligibility")
    if row.get("status") not in {"complete", "indeterminate"}:
        raise ContractError("model-proxy aggregate status is invalid")
    endpoint = row.get("engineering_proxy_E")
    if isinstance(endpoint, bool) or endpoint is not None and endpoint not in (0, 1):
        raise ContractError("model-proxy engineering endpoint is invalid")
    if (row["status"] == "complete") != (endpoint is not None):
        raise ContractError("model-proxy endpoint/status mismatch")
    diagnostics = row.get("diagnostics")
    if not isinstance(diagnostics, Mapping) or diagnostics.get("held_out_human_rater_count") != 0:
        raise ContractError("model-proxy aggregate must report zero human raters")
    return row


def _descriptive_proxy_candidates(
    rows: Sequence[Mapping[str, Any]], baseline_arm_id: str
) -> list[dict[str, Any]]:
    baseline: dict[str, Mapping[str, Any]] = {}
    for row in rows:
        endpoint = row.get("descriptive_endpoint")
        if (
            row.get("arm_id") == baseline_arm_id
            and isinstance(endpoint, Mapping)
            and endpoint.get("evidence_tier") == MODEL_PROXY_TIER
            and endpoint.get("engineering_proxy_E") in {0, 1}
        ):
            baseline[str(row["sample_id"])] = row
    grouped: dict[str, list[Mapping[str, Any]]] = {}
    for row in rows:
        if row.get("arm_id") == baseline_arm_id:
            continue
        endpoint = row.get("descriptive_endpoint")
        sample_id = str(row.get("sample_id"))
        if (
            sample_id in baseline
            and isinstance(endpoint, Mapping)
            and endpoint.get("evidence_tier") == MODEL_PROXY_TIER
            and endpoint.get("engineering_proxy_E") in {0, 1}
        ):
            grouped.setdefault(str(row["arm_id"]), []).append(row)
    candidates: list[dict[str, Any]] = []
    for arm_id, arm_rows in grouped.items():
        deltas: list[float] = []
        clusters: set[str] = set()
        for row in arm_rows:
            sample_id = str(row["sample_id"])
            base_endpoint = baseline[sample_id]["descriptive_endpoint"]
            endpoint = row["descriptive_endpoint"]
            deltas.append(
                float(endpoint["engineering_proxy_E"])
                - float(base_endpoint["engineering_proxy_E"])
            )
            clusters.add(str(row["atomic_cluster_id"]))
        delta = sum(deltas) / len(deltas)
        candidates.append(
            {
                "arm_id": arm_id,
                "evidence_tier": MODEL_PROXY_TIER,
                "paired_sample_count": len(deltas),
                "paired_atomic_cluster_count": len(clusters),
                "raw_mean_delta_vs_actual": delta,
                "confidence_interval": None,
                "p_value": None,
                "multiplicity_adjusted_p_value": None,
                "eligible_for_attribution": False,
            }
        )
    return sorted(
        candidates,
        key=lambda row: (-abs(float(row["raw_mean_delta_vs_actual"])), row["arm_id"]),
    )


def _arm_metadata(arm: Arm) -> dict[str, Any]:
    return {
        "arm_id": arm.arm_id,
        "sources": {stage: arm.sources[stage].value for stage in STAGES},
        "oracle_chain": arm.oracle_chain,
        "focus_stage": arm.focus_stage,
        "context": arm.context,
    }


def _row_binding(sample: NativeEngineeringSample, arm: Arm) -> dict[str, Any]:
    return {
        "sample_id": sample.sample_id,
        "atomic_cluster_id": sample.atomic_cluster_id,
        "arm_id": arm.arm_id,
        "registered_sources": {
            stage: arm.sources[stage].value for stage in STAGES
        },
        "oracle_chain": arm.oracle_chain,
        "focus_stage": arm.focus_stage,
        "context": arm.context,
    }


def _finite_json_object(value: Mapping[str, Any], label: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object")
    try:
        copied = json.loads(json.dumps(value, ensure_ascii=False, allow_nan=False))
    except (TypeError, ValueError) as error:
        raise ContractError(f"{label} must be finite JSON") from error
    if not isinstance(copied, dict):
        raise ContractError(f"{label} must be an object")
    return copied


def _validate_provider_io(
    value: Mapping[str, Any],
    *,
    invocation: Mapping[str, Any],
    raw_result: Mapping[str, Any],
    provider_binding: Mapping[str, Any],
) -> dict[str, Any]:
    document = _finite_json_object(value, "native provider I/O")
    if set(document) != {
        "schema_version",
        "provider_binding",
        "invocation_sha256",
        "raw_result_sha256",
        "invocation",
        "raw_result",
    }:
        raise ContractError("native provider I/O fields are not exact")
    if document.get("schema_version") != NATIVE_ENGINEERING_PROVIDER_IO_SCHEMA:
        raise ContractError("native provider I/O schema changed")
    expected_invocation = _finite_json_object(invocation, "native invocation")
    expected_result = _finite_json_object(raw_result, "raw native provider result")
    expected_binding = _validate_provider_binding(provider_binding)
    if document.get("invocation") != expected_invocation:
        raise IntegrityError("native provider I/O invocation changed")
    if document.get("raw_result") != expected_result:
        raise IntegrityError("native provider I/O raw result changed")
    if document.get("provider_binding") != expected_binding:
        raise IntegrityError("native provider I/O binding changed")
    if document.get("invocation_sha256") != sha256_json(expected_invocation):
        raise IntegrityError("native provider I/O invocation hash mismatch")
    if document.get("raw_result_sha256") != sha256_json(expected_result):
        raise IntegrityError("native provider I/O result hash mismatch")
    return document


def _validate_provider_failure(
    value: Mapping[str, Any],
    *,
    invocation: Mapping[str, Any],
    invocation_artifact: Mapping[str, Any],
    provider_binding: Mapping[str, Any],
) -> dict[str, Any]:
    document = _finite_json_object(value, "native provider failure")
    if set(document) != {
        "schema_version",
        "status",
        "production_attribution_eligible",
        "inference_eligible",
        "production_bottleneck",
        "provider_binding",
        "invocation_sha256",
        "invocation_artifact",
        "failure",
    }:
        raise ContractError("native provider failure fields are not exact")
    if (
        document.get("schema_version") != NATIVE_PROVIDER_FAILURE_SCHEMA
        or document.get("status") != "provider-timeout"
        or document.get("production_attribution_eligible") is not False
        or document.get("inference_eligible") is not False
        or document.get("production_bottleneck") is not None
    ):
        raise ContractError("native provider failure policy changed")

    expected_binding = _validate_provider_binding(provider_binding)
    if expected_binding.get("schema_version") != NATIVE_COMMAND_PROVIDER_BINDING_SCHEMA:
        raise ContractError("timeout failure requires a v2 command provider binding")
    if document.get("provider_binding") != expected_binding:
        raise IntegrityError("native provider failure binding changed")
    expected_invocation = _finite_json_object(invocation, "native invocation")
    expected_invocation_hash = sha256_json(expected_invocation)
    if document.get("invocation_sha256") != expected_invocation_hash:
        raise IntegrityError("native provider failure invocation hash mismatch")
    expected_artifact = _finite_json_object(
        invocation_artifact, "native invocation artifact reference"
    )
    if set(expected_artifact) not in (
        {"sha256"},
        {"sha256", "path"},
    ):
        raise ContractError("native invocation artifact reference fields are not exact")
    if expected_artifact.get("sha256") != expected_invocation_hash:
        raise IntegrityError("native invocation artifact reference hash mismatch")
    if "path" in expected_artifact and (
        not isinstance(expected_artifact["path"], str)
        or not expected_artifact["path"]
    ):
        raise ContractError("native invocation artifact path is invalid")
    if document.get("invocation_artifact") != expected_artifact:
        raise IntegrityError("native provider failure invocation reference changed")

    failure = document.get("failure")
    if not isinstance(failure, Mapping) or set(failure) != {
        "failure_kind",
        "deadline_policy",
        "configured_timeout_milliseconds",
        "diagnostic_line_count",
        "diagnostic_tail_sha256",
        "termination",
    }:
        raise ContractError("native provider timeout details are not exact")
    if (
        failure.get("failure_kind") != "response-timeout"
        or failure.get("deadline_policy") != "whole-invocation-monotonic"
        or failure.get("configured_timeout_milliseconds")
        != expected_binding.get("response_timeout_milliseconds")
    ):
        raise IntegrityError("native provider timeout policy/binding mismatch")
    diagnostic_count = failure.get("diagnostic_line_count")
    diagnostic_hashes = failure.get("diagnostic_tail_sha256")
    if (
        isinstance(diagnostic_count, bool)
        or not isinstance(diagnostic_count, int)
        or diagnostic_count < 0
        or not isinstance(diagnostic_hashes, list)
        or len(diagnostic_hashes) > 20
        or diagnostic_count < len(diagnostic_hashes)
    ):
        raise ContractError("native provider timeout diagnostics are malformed")
    for diagnostic_hash in diagnostic_hashes:
        _require_sha256(diagnostic_hash, "native provider diagnostic SHA-256")
    termination = failure.get("termination")
    if not isinstance(termination, Mapping) or set(termination) != {
        "attempted",
        "strategy",
        "parent_return_code",
        "reader_stopped",
    }:
        raise ContractError("native provider timeout termination fields are not exact")
    return_code = termination.get("parent_return_code")
    if (
        termination.get("attempted") is not True
        or termination.get("strategy")
        not in {"windows-process-tree-force", "posix-process-group-kill"}
        or (
            return_code is not None
            and (isinstance(return_code, bool) or not isinstance(return_code, int))
        )
        or not isinstance(termination.get("reader_stopped"), bool)
    ):
        raise ContractError("native provider timeout termination is malformed")
    return document


def _validate_provider_binding(value: Mapping[str, Any]) -> dict[str, Any]:
    binding = _finite_json_object(value, "native provider binding")
    base_fields = {
        "schema_version",
        "provider_kind",
        "transport",
        "provider_identity_sha256",
        "command_argv_sha256",
        "executable_sha256",
        "working_directory_sha256",
        "binding_sha256",
    }
    version = binding.get("schema_version")
    provider_kind = binding.get("provider_kind")
    if version == NATIVE_COMMAND_PROVIDER_BINDING_SCHEMA:
        if set(binding) != base_fields | {"response_timeout_milliseconds"}:
            raise ContractError("native command provider binding fields are not exact")
        if provider_kind != "persistent-jsonl-command":
            raise ContractError("native command provider binding kind changed")
        timeout_milliseconds = binding.get("response_timeout_milliseconds")
        if (
            isinstance(timeout_milliseconds, bool)
            or not isinstance(timeout_milliseconds, int)
            or not 1 <= timeout_milliseconds <= 3_600_000
        ):
            raise ContractError(
                "provider response timeout must lie between 1 and 3600000 milliseconds"
            )
    elif version == NATIVE_PROVIDER_BINDING_SCHEMA:
        if set(binding) != base_fields:
            raise ContractError("legacy native provider binding fields are not exact")
    else:
        raise ContractError("native provider binding schema changed")

    transport = binding.get("transport")
    if provider_kind == "persistent-jsonl-command":
        if transport != "canonical-jsonl-utf8-stdio":
            raise ContractError("native command provider transport changed")
        _require_sha256(binding.get("command_argv_sha256"), "provider command SHA-256")
        _require_sha256(
            binding.get("working_directory_sha256"),
            "provider working-directory SHA-256",
        )
        executable_sha256 = binding.get("executable_sha256")
        if executable_sha256 is not None:
            _require_sha256(executable_sha256, "provider executable SHA-256")
    elif provider_kind == "in-process-callable":
        if version != NATIVE_PROVIDER_BINDING_SCHEMA:
            raise ContractError("native callable provider may only use the v1 binding")
        if transport != "python-mapping-call":
            raise ContractError("native callable provider transport changed")
        if any(
            binding.get(field) is not None
            for field in (
                "command_argv_sha256",
                "executable_sha256",
                "working_directory_sha256",
            )
        ):
            raise ContractError("native callable provider contains command path state")
    else:
        raise ContractError("native provider kind is unsupported")
    _require_sha256(
        binding.get("provider_identity_sha256"), "provider identity SHA-256"
    )
    claimed = _require_sha256(
        binding.get("binding_sha256"), "provider binding SHA-256"
    )
    unsigned = dict(binding)
    unsigned.pop("binding_sha256")
    if claimed != sha256_json(unsigned):
        raise IntegrityError("native provider binding hash mismatch")
    return binding


def _require_sha256(value: Any, label: str) -> str:
    if not isinstance(value, str) or not _SHA256.fullmatch(value):
        raise ContractError(f"{label} is invalid")
    return value


__all__ = [
    "MODEL_PROXY_TIER",
    "NATIVE_COMMAND_PROVIDER_BINDING_SCHEMA",
    "NATIVE_ENGINEERING_INVOCATION_SCHEMA",
    "NATIVE_ENGINEERING_PROVIDER_IO_SCHEMA",
    "NATIVE_ENGINEERING_REPORT_SCHEMA",
    "NATIVE_ENGINEERING_RESULT_SCHEMA",
    "NATIVE_HASH_CONTRACT",
    "NATIVE_OBSERVATION_SCHEMA",
    "NATIVE_OBSERVED_STAGES",
    "NATIVE_PROVIDER_BINDING_SCHEMA",
    "NATIVE_PROVIDER_FAILURE_SCHEMA",
    "NATIVE_UNAVAILABLE_STAGE",
    "JsonlNativeCommandProvider",
    "NativeArmRealization",
    "NativeEngineeringRunner",
    "NativeEngineeringSample",
    "NativeProviderTimeout",
    "NativeResultProvider",
    "native_canonical_json",
    "native_projection_evaluation_view",
    "native_sha256_json",
    "validate_native_engineering_result",
    "validate_native_observation",
]
