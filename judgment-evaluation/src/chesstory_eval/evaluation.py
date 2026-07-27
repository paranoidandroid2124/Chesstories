from __future__ import annotations

import base64
import json
import math
import os
import subprocess
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any, Mapping, Sequence

from .adapters import CommandSpec, strict_json_loads
from .capture import ArtifactStore
from .hashing import json_bytes, read_jsonl, sha256_json
from .model import ContractError, EndpointResult, IntegrityError, Sample, StageUnavailable


_SHA256_LENGTH = 64


class FrozenEndpointPolicy:
    """Pre-unblinding binding for the endpoint allowed set and evaluator graph.

    A release endpoint must not consult the corpus' development labels for its
    allowed set.  This small, content-addressed document is loaded before any
    evaluator output and binds the exact run/candidate/split/seed, the oracle
    graph, the per-sample run allowed set, and the source-blind evaluator IDs.
    """

    SCHEMA_VERSION = "chesstory.eval.endpoint-policy.v1"

    def __init__(
        self,
        document: Mapping[str, Any],
        *,
        expected_binding: Mapping[str, Any],
        release_mode: bool = False,
        role_assignments: Mapping[str, Mapping[str, Sequence[str]]] | None = None,
        required_held_out_raters: int = 0,
    ) -> None:
        if not isinstance(document, Mapping):
            raise ContractError("endpoint policy must be an object")
        if document.get("schema_version") != self.SCHEMA_VERSION:
            raise ContractError("unsupported endpoint-policy schema_version")
        unsigned = dict(document)
        claimed_content_hash = _sha256_text(
            unsigned.pop("content_sha256", None), "endpoint policy content_sha256"
        )
        if sha256_json(unsigned) != claimed_content_hash:
            raise IntegrityError("endpoint policy content hash mismatch")

        binding = document.get("binding")
        if not isinstance(binding, Mapping):
            raise ContractError("endpoint policy binding must be an object")
        expected_keys = {
            "run_id",
            "candidate_manifest_sha256",
            "corpus_version",
            "split",
            "split_sha256",
            "seed",
        }
        if set(binding) != expected_keys:
            raise ContractError(
                "endpoint policy binding must contain exactly run/candidate/corpus/split/seed"
            )
        for key in expected_keys:
            if binding.get(key) != expected_binding.get(key):
                raise IntegrityError(f"endpoint policy {key} binding mismatch")
        _sha256_text(
            binding.get("candidate_manifest_sha256"),
            "endpoint policy candidate_manifest_sha256",
        )
        _sha256_text(binding.get("split_sha256"), "endpoint policy split_sha256")

        frozen_at = document.get("frozen_at")
        if not isinstance(frozen_at, str) or not frozen_at.strip():
            raise ContractError("endpoint policy frozen_at must be non-empty text")
        if document.get("frozen_before_unblinding") is not True:
            raise ContractError(
                "endpoint policy must explicitly assert frozen_before_unblinding=true"
            )

        oracle_graph = document.get("oracle_graph")
        if not isinstance(oracle_graph, Mapping) or not oracle_graph:
            raise ContractError("endpoint policy oracle_graph must be a non-empty object")
        oracle_hash = _sha256_text(
            document.get("oracle_graph_sha256"), "endpoint policy oracle_graph_sha256"
        )
        if sha256_json(oracle_graph) != oracle_hash:
            raise IntegrityError("endpoint policy oracle graph hash mismatch")
        graph_claims = oracle_graph.get("claims")
        if (
            not isinstance(graph_claims, list)
            or not graph_claims
            or any(not isinstance(claim, Mapping) for claim in graph_claims)
        ):
            raise ContractError("endpoint policy oracle graph must contain claim objects")
        graph_claim_ids = [claim.get("claim_id") for claim in graph_claims]
        if (
            any(not isinstance(claim_id, str) or not claim_id.strip() for claim_id in graph_claim_ids)
            or graph_claim_ids != sorted(set(graph_claim_ids))
        ):
            raise ContractError("oracle graph claim_id values must be sorted and unique")

        allowed_set = document.get("allowed_set_by_sample")
        if not isinstance(allowed_set, Mapping) or not allowed_set:
            raise ContractError(
                "endpoint policy allowed_set_by_sample must be a non-empty object"
            )
        frozen_allowed: dict[str, tuple[str, ...]] = {}
        for sample_id, allowed_claim_ids in allowed_set.items():
            if not isinstance(sample_id, str) or not sample_id.strip():
                raise ContractError("endpoint policy sample IDs must be non-empty text")
            if (
                not isinstance(allowed_claim_ids, list)
                or not all(isinstance(value, str) and value.strip() for value in allowed_claim_ids)
                or allowed_claim_ids != sorted(set(allowed_claim_ids))
            ):
                raise ContractError(
                    f"endpoint policy allowed set for {sample_id!r} must be a sorted unique string array"
                )
            frozen_allowed[sample_id] = tuple(allowed_claim_ids)
        graph_claim_id_set = {str(value) for value in graph_claim_ids}
        for sample_id, allowed_claims in frozen_allowed.items():
            unknown = set(allowed_claims) - graph_claim_id_set
            if unknown:
                raise ContractError(
                    f"endpoint allowed set for {sample_id!r} names claims absent from oracle graph: {sorted(unknown)}"
                )
        allowed_hash = _sha256_text(
            document.get("allowed_set_sha256"), "endpoint policy allowed_set_sha256"
        )
        if sha256_json(allowed_set) != allowed_hash:
            raise IntegrityError("endpoint policy allowed-set hash mismatch")

        evaluator_ids = document.get("source_blind_evaluator_ids")
        if (
            not isinstance(evaluator_ids, list)
            or not evaluator_ids
            or not all(isinstance(value, str) and value.strip() for value in evaluator_ids)
            or evaluator_ids != sorted(set(evaluator_ids))
        ):
            raise ContractError(
                "endpoint policy source_blind_evaluator_ids must be a sorted unique string array"
            )

        topology: dict[str, str] = {}
        raters_by_sample: dict[str, tuple[str, ...]] = {}
        adjudicators_by_sample: dict[str, tuple[str, ...]] = {}
        if release_mode:
            supplied_topology = document.get("sample_atomic_cluster_ids")
            supplied_raters = document.get("source_blind_evaluator_ids_by_sample")
            supplied_adjudicators = document.get(
                "out_of_set_adjudicator_ids_by_sample"
            )
            if not isinstance(supplied_topology, Mapping) or set(
                supplied_topology
            ) != set(frozen_allowed):
                raise ContractError("release endpoint policy sample topology is not exact")
            if not isinstance(supplied_raters, Mapping) or set(supplied_raters) != set(
                frozen_allowed
            ):
                raise ContractError("release endpoint per-sample rater roster is not exact")
            if not isinstance(supplied_adjudicators, Mapping) or set(
                supplied_adjudicators
            ) != set(frozen_allowed):
                raise ContractError(
                    "release endpoint per-sample adjudicator roster is not exact"
                )
            if not isinstance(role_assignments, Mapping):
                raise ContractError("release endpoint policy requires frozen role assignments")
            if required_held_out_raters <= 0:
                raise ContractError("release endpoint policy requires a positive rater minimum")
            for sample_id in sorted(frozen_allowed):
                cluster = supplied_topology.get(sample_id)
                if not isinstance(cluster, str) or not cluster.strip():
                    raise ContractError("release endpoint topology cluster IDs are invalid")
                cluster_roles = role_assignments.get(cluster)
                if not isinstance(cluster_roles, Mapping):
                    raise ContractError(
                        f"release endpoint sample {sample_id!r} uses an unscheduled cluster"
                    )
                frozen_raters = _sorted_identity_array(
                    supplied_raters.get(sample_id), f"raters for {sample_id!r}"
                )
                frozen_adjudicators = _sorted_identity_array(
                    supplied_adjudicators.get(sample_id),
                    f"adjudicators for {sample_id!r}",
                )
                expected_raters = tuple(cluster_roles.get("held_out_rater_ids", ()))
                expected_adjudicators = tuple(cluster_roles.get("adjudicator_ids", ()))
                if frozen_raters != expected_raters or frozen_adjudicators != expected_adjudicators:
                    raise ContractError(
                        f"release endpoint identities differ from role schedule for {sample_id!r}"
                    )
                if len(frozen_raters) < required_held_out_raters:
                    raise ContractError(
                        f"release endpoint sample {sample_id!r} has too few held-out raters"
                    )
                if set(frozen_raters) & set(frozen_adjudicators):
                    raise ContractError(
                        f"release endpoint sample {sample_id!r} reuses evaluator/adjudicator identity"
                    )
                topology[sample_id] = cluster
                raters_by_sample[sample_id] = frozen_raters
                adjudicators_by_sample[sample_id] = frozen_adjudicators
            for field, value in (
                ("sample_topology_sha256", supplied_topology),
                ("rater_roster_sha256", supplied_raters),
                ("adjudicator_roster_sha256", supplied_adjudicators),
            ):
                if _sha256_text(document.get(field), f"endpoint policy {field}") != sha256_json(value):
                    raise IntegrityError(f"endpoint policy {field} mismatch")
            scheduled_union = sorted(
                {identity for values in raters_by_sample.values() for identity in values}
            )
            if evaluator_ids != scheduled_union:
                raise ContractError(
                    "endpoint policy global evaluator IDs differ from per-sample roster union"
                )

        self.document = dict(document)
        self.content_sha256 = claimed_content_hash
        self.document_sha256 = sha256_json(document)
        self.oracle_graph_sha256 = oracle_hash
        self.allowed_set_sha256 = allowed_hash
        self.allowed_set_by_sample = frozen_allowed
        self.source_blind_evaluator_ids = frozenset(evaluator_ids)
        self.sample_atomic_cluster_ids = topology
        self.source_blind_evaluator_ids_by_sample = raters_by_sample
        self.out_of_set_adjudicator_ids_by_sample = adjudicators_by_sample
        self.release_mode = release_mode

    def allowed_claim_ids(self, sample_id: str) -> tuple[str, ...]:
        claims = self.allowed_set_by_sample.get(sample_id)
        if claims is None:
            raise ContractError(
                f"endpoint policy has no frozen allowed set for sample {sample_id!r}"
            )
        return claims

    def expected_evaluator_ids(self, sample_id: str) -> tuple[str, ...]:
        values = self.source_blind_evaluator_ids_by_sample.get(sample_id)
        if self.release_mode and values is None:
            raise ContractError(f"endpoint policy has no rater roster for {sample_id!r}")
        return values or ()

    def assert_exact_samples(self, samples: Sequence[Sample]) -> None:
        if not self.release_mode:
            return
        actual = {sample.sample_id: sample.atomic_cluster_id for sample in samples}
        if actual != self.sample_atomic_cluster_ids:
            raise IntegrityError(
                "opened split sample/atomic-cluster topology differs from endpoint policy"
            )


def _sha256_text(value: Any, label: str) -> str:
    if (
        not isinstance(value, str)
        or len(value) != _SHA256_LENGTH
        or any(character not in "0123456789abcdef" for character in value)
    ):
        raise ContractError(f"{label} must be a lowercase 64-character SHA-256")
    return value


def _sorted_identity_array(value: Any, label: str) -> tuple[str, ...]:
    if (
        not isinstance(value, list)
        or not value
        or not all(isinstance(item, str) and item.strip() for item in value)
        or value != sorted(set(value))
    ):
        raise ContractError(f"{label} must be a sorted unique non-empty string array")
    return tuple(value)


class EvaluationAdapter(ABC):
    @abstractmethod
    def evaluate(
        self,
        *,
        sample: Sample,
        evaluation_view: Mapping[str, Any],
    ) -> EndpointResult:
        raise NotImplementedError

    def is_live_command(self) -> bool:
        return False


class CommandEvaluationUnavailable(StageUnavailable):
    """A live evaluator command failed while retaining provider I/O evidence."""

    def __init__(self, reason: str, *, provider_io: Mapping[str, Any]) -> None:
        super().__init__("V", reason)
        self.provider_io = dict(provider_io)


class CommandEvaluationContractError(ContractError):
    """A live evaluator response violated the source-blind contract."""

    def __init__(self, message: str, *, provider_io: Mapping[str, Any]) -> None:
        super().__init__(message)
        self.provider_io = dict(provider_io)


def _validate_evaluator_roster(
    row: Mapping[str, Any],
    *,
    sample_id: str,
    endpoint_policy: FrozenEndpointPolicy | None,
    release_mode: bool,
    location: str,
) -> tuple[str, ...]:
    evaluator_ids = row.get("source_blind_evaluator_ids", [])
    if (
        not isinstance(evaluator_ids, list)
        or not all(isinstance(value, str) and value.strip() for value in evaluator_ids)
        or evaluator_ids != sorted(set(evaluator_ids))
    ):
        raise ContractError(
            f"{location}: source_blind_evaluator_ids must be sorted and unique"
        )
    if release_mode:
        if not evaluator_ids:
            raise ContractError(f"{location}: release evaluator IDs are missing")
        if endpoint_policy is None:
            raise ContractError("release evaluator roster has no frozen endpoint policy")
        if tuple(evaluator_ids) != endpoint_policy.expected_evaluator_ids(sample_id):
            raise ContractError(
                f"{location}: evaluator IDs differ from the exact per-sample frozen roster"
            )
    return tuple(evaluator_ids)


def _endpoint_from_evaluator_row(
    *,
    sample: Sample,
    evaluation_view: Mapping[str, Any],
    row: Mapping[str, Any],
    usefulness_threshold: float,
    endpoint_policy: FrozenEndpointPolicy | None,
    release_mode: bool,
) -> EndpointResult:
    labels = sample.labels
    answerable = labels.get("answerable")
    if not isinstance(answerable, bool):
        raise ContractError(f"sample {sample.sample_id} has no frozen answerable label")
    payload = evaluation_view.get("payload")
    if not isinstance(payload, Mapping):
        raise ContractError("V output payload is missing")
    status = payload.get("status")
    unsupported = row.get("unsupported_meaning_count")
    if (
        not isinstance(unsupported, int)
        or isinstance(unsupported, bool)
        or unsupported < 0
    ):
        raise ContractError("evaluator unsupported_meaning_count must be a non-negative integer")
    if not isinstance(row.get("grounded"), bool) or not isinstance(
        row.get("fact_correct"), bool
    ):
        raise ContractError("evaluator grounded/fact_correct must be booleans")
    grounded = row["grounded"] is True and unsupported == 0
    fact_correct = row["fact_correct"] is True
    canonical_claims = row.get("canonical_core_claim_ids")
    if not isinstance(canonical_claims, list) or not all(
        isinstance(value, str) and value.strip() for value in canonical_claims
    ):
        raise ContractError("evaluator canonical_core_claim_ids must be a string array")
    if endpoint_policy is not None:
        allowed = list(endpoint_policy.allowed_claim_ids(sample.sample_id))
    else:
        if release_mode:
            raise ContractError("release endpoint attempted corpus base-set fallback")
        allowed = labels.get("allowed_core_claim_ids", [])
        if not isinstance(allowed, list) or not all(isinstance(value, str) for value in allowed):
            raise ContractError("allowed_core_claim_ids must be a string array")
    accepted_claim = bool(set(canonical_claims).intersection(allowed))
    consensus = row.get("usefulness_consensus")
    if (
        not isinstance(consensus, (int, float))
        or isinstance(consensus, bool)
        or not math.isfinite(float(consensus))
    ):
        raise ContractError("evaluator usefulness_consensus must be finite numeric")
    useful = float(consensus) >= usefulness_threshold
    abstained = status in {"abstained", "rejected"}

    if answerable:
        conditions = {
            "non_abstention": not abstained,
            "grounded_and_fact_correct": grounded and fact_correct,
            "allowed_top_claim": accepted_claim,
            "useful": useful,
        }
    else:
        conditions = {
            "correct_abstention": abstained,
            "no_unsupported_meaning": unsupported == 0,
        }
    diagnostics_raw = row.get("diagnostics", {})
    if not isinstance(diagnostics_raw, Mapping):
        raise ContractError("evaluator diagnostics must be an object")
    diagnostics: dict[str, float | int | None] = {}
    for key, diagnostic in diagnostics_raw.items():
        if diagnostic is not None and (
            not isinstance(diagnostic, (int, float))
            or isinstance(diagnostic, bool)
            or not math.isfinite(float(diagnostic))
        ):
            raise ContractError(f"diagnostic {key!r} must be finite numeric or null")
        diagnostics[str(key)] = diagnostic
    evaluator_ids = row.get("source_blind_evaluator_ids", [])
    diagnostics["held_out_human_rater_count"] = len(evaluator_ids)
    return EndpointResult(
        value=int(all(conditions.values())),
        conditions=conditions,
        diagnostics=diagnostics,
        evaluator_artifact_hash=sha256_json(row),
    )


class ReplayEvaluationAdapter(EvaluationAdapter):
    """Consumes source/arm-blind evaluator artifacts keyed only by output hash."""

    def __init__(
        self,
        paths: Sequence[Path],
        *,
        usefulness_threshold: float,
        endpoint_policy: FrozenEndpointPolicy | None = None,
        release_mode: bool = False,
        access_store: ArtifactStore | None = None,
    ) -> None:
        if release_mode and endpoint_policy is None:
            raise ContractError(
                "release evaluation requires a pre-unblinding frozen endpoint policy"
            )
        if release_mode and access_store is None:
            raise ContractError(
                "release evaluation requires an append-only evaluator access log"
            )
        self.usefulness_threshold = usefulness_threshold
        self.endpoint_policy = endpoint_policy
        self.release_mode = release_mode
        self.endpoint_policy_hash = (
            endpoint_policy.document_sha256 if endpoint_policy is not None else None
        )
        self.endpoint_allowed_set_source = (
            "pre-unblinding-frozen-run-artifact"
            if endpoint_policy is not None
            else "diagnostic-corpus-base-set-sensitivity-only"
        )
        self.records: dict[tuple[str, str], Mapping[str, Any]] = {}
        for path in paths:
            for line_number, row in enumerate(read_jsonl(path), 1):
                if not isinstance(row, Mapping):
                    raise ContractError(f"{path}:{line_number}: evaluator record must be an object")
                if row.get("schema_version") != "chesstory.eval.blind-evaluation.v1":
                    raise ContractError(f"{path}:{line_number}: unsupported evaluator schema")
                sample_id = row.get("sample_id")
                output_hash = row.get("canonical_output_hash")
                if not isinstance(sample_id, str) or not isinstance(output_hash, str):
                    raise ContractError(f"{path}:{line_number}: evaluator key is incomplete")
                forbidden = {"arm_id", "source", "oracle_chain", "provider"}.intersection(row)
                if forbidden:
                    raise ContractError(
                        f"{path}:{line_number}: evaluator artifact leaks provenance {sorted(forbidden)}"
                    )
                evaluator_ids = _validate_evaluator_roster(
                    row,
                    sample_id=str(sample_id),
                    endpoint_policy=endpoint_policy,
                    release_mode=release_mode,
                    location=f"{path}:{line_number}",
                )
                if release_mode:
                    assert access_store is not None
                    evaluator_hash = sha256_json(row)
                    for evaluator_id in evaluator_ids:
                        access_store.record_access(
                            role=f"final-usefulness-rater:{evaluator_id}",
                            artifact_hash=evaluator_hash,
                            action="source-blind-evaluation",
                            reason=f"sample:{sample_id}",
                        )
                key = (sample_id, output_hash)
                if key in self.records:
                    existing = self.records[key]
                    if existing != row:
                        raise ContractError(f"{path}:{line_number}: conflicting evaluator duplicate")
                self.records[key] = row

    def evaluate(
        self,
        *,
        sample: Sample,
        evaluation_view: Mapping[str, Any],
    ) -> EndpointResult:
        output_hash = sha256_json(evaluation_view)
        row = self.records.get((sample.sample_id, output_hash))
        if row is None:
            raise StageUnavailable(
                "V",
                f"no source-blind evaluator artifact for canonical output {output_hash}",
            )
        return _endpoint_from_evaluator_row(
            sample=sample,
            evaluation_view=evaluation_view,
            row=row,
            usefulness_threshold=self.usefulness_threshold,
            endpoint_policy=self.endpoint_policy,
            release_mode=self.release_mode,
        )


class CommandEvaluationAdapter(EvaluationAdapter):
    """Run a source-blind evaluator as a shell-free external command.

    The exact ``source_blind_evaluation_view`` is the complete stdin document.
    In particular, sample, arm, source, provider, and oracle identities are not
    added to it. The command's response must independently echo the expected
    sample ID and canonical view hash and carry the frozen evaluator roster.
    """

    _SCHEMA_VERSION = "chesstory.eval.blind-evaluation.v1"
    _REQUIRED_RESPONSE_FIELDS = {
        "schema_version",
        "sample_id",
        "canonical_output_hash",
        "source_blind_evaluator_ids",
        "unsupported_meaning_count",
        "grounded",
        "fact_correct",
        "canonical_core_claim_ids",
        "usefulness_consensus",
    }
    _OPTIONAL_RESPONSE_FIELDS = {"diagnostics", "usefulness_votes"}

    def __init__(
        self,
        command: CommandSpec,
        *,
        usefulness_threshold: float,
        endpoint_policy: FrozenEndpointPolicy | None,
        release_mode: bool,
        access_store: ArtifactStore,
    ) -> None:
        if not isinstance(command, CommandSpec):
            raise ContractError("command evaluator requires one validated CommandSpec")
        if (
            not isinstance(usefulness_threshold, (int, float))
            or isinstance(usefulness_threshold, bool)
            or not math.isfinite(float(usefulness_threshold))
        ):
            raise ContractError("usefulness threshold must be finite numeric")
        if release_mode and endpoint_policy is None:
            raise ContractError(
                "release command evaluation requires a frozen endpoint policy"
            )
        if not isinstance(access_store, ArtifactStore):
            raise ContractError("command evaluation requires an artifact/access store")
        reserved_environment = {
            "CHESSTORY_EVAL_SAMPLE_ID",
            "CHESSTORY_EVAL_CANONICAL_OUTPUT_HASH",
        }
        if reserved_environment.intersection(
            key.upper() for key in command.environment_dict()
        ):
            raise ContractError(
                "evaluator route env must not override harness identity bindings"
            )
        self.command = command
        self.usefulness_threshold = float(usefulness_threshold)
        self.endpoint_policy = endpoint_policy
        self.release_mode = release_mode
        self.access_store = access_store
        self._successful_invocation_index = 0
        self.endpoint_policy_hash = (
            endpoint_policy.document_sha256 if endpoint_policy is not None else None
        )
        self.endpoint_allowed_set_source = (
            "pre-unblinding-frozen-run-artifact"
            if endpoint_policy is not None
            else "diagnostic-corpus-base-set-sensitivity-only"
        )

    def is_live_command(self) -> bool:
        return True

    def evaluate(
        self,
        *,
        sample: Sample,
        evaluation_view: Mapping[str, Any],
    ) -> EndpointResult:
        self._validate_source_blind_view(evaluation_view)
        output_hash = sha256_json(evaluation_view)
        request = dict(evaluation_view)
        try:
            request_bytes = json_bytes(request)
        except (TypeError, ValueError) as error:
            raise ContractError("source-blind evaluator view is not finite JSON") from error
        base_provider_io = {
            "command": list(self.command.argv),
            "request": request,
            "control_binding": {
                "sample_id": sample.sample_id,
                "canonical_output_hash": output_hash,
            },
            "stdin_base64": base64.b64encode(request_bytes).decode("ascii"),
        }
        environment = self._minimal_process_environment()
        environment.update(self.command.environment_dict())
        # Identity is control-plane routing, never part of evaluator stdin.
        # The evaluator still receives no arm/source/oracle/provider metadata.
        environment["CHESSTORY_EVAL_SAMPLE_ID"] = sample.sample_id
        environment["CHESSTORY_EVAL_CANONICAL_OUTPUT_HASH"] = output_hash
        try:
            with self.command.invocation_guard(
                release_mode=self.release_mode,
                location="source-blind evaluator route",
            ):
                completed = subprocess.run(
                    list(self.command.argv),
                    input=request_bytes,
                    text=False,
                    capture_output=True,
                    timeout=self.command.timeout_seconds,
                    check=False,
                    env=environment,
                )
        except subprocess.TimeoutExpired as error:
            provider_io = self._provider_io_record(
                base_provider_io,
                returncode=None,
                stdout=error.stdout,
                stderr=error.stderr,
                failure={
                    "kind": "timeout",
                    "timeout_seconds": self.command.timeout_seconds,
                },
            )
            raise CommandEvaluationUnavailable(
                f"external evaluator timed out after {self.command.timeout_seconds} seconds",
                provider_io=provider_io,
            ) from error
        except (OSError, TypeError, ValueError) as error:
            provider_io = self._provider_io_record(
                base_provider_io,
                returncode=None,
                stdout=None,
                stderr=None,
                failure={
                    "kind": "os-error" if isinstance(error, OSError) else "launch-error",
                    "error_type": type(error).__name__,
                    "message": str(error),
                    "errno": error.errno if isinstance(error, OSError) else None,
                },
            )
            raise CommandEvaluationUnavailable(
                f"external evaluator could not start: {type(error).__name__}",
                provider_io=provider_io,
            ) from error
        provider_io = self._provider_io_record(
            base_provider_io,
            returncode=completed.returncode,
            stdout=completed.stdout,
            stderr=completed.stderr,
        )
        if completed.returncode != 0:
            raise CommandEvaluationUnavailable(
                f"external evaluator failed with exit code {completed.returncode}",
                provider_io=provider_io,
            )
        stdout_bytes = self._safe_process_bytes(completed.stdout)
        try:
            response_text = stdout_bytes.decode("utf-8")
        except UnicodeDecodeError as error:
            raise CommandEvaluationContractError(
                "external evaluator emitted non-UTF-8 output",
                provider_io=provider_io,
            ) from error
        try:
            response = strict_json_loads(
                response_text, location="external evaluator response"
            )
        except ContractError as error:
            raise CommandEvaluationContractError(
                "external evaluator emitted invalid strict JSON",
                provider_io=provider_io,
            ) from error
        provider_io = {**provider_io, "response": response}
        try:
            row, evaluator_ids = self._validated_response(
                response,
                sample_id=sample.sample_id,
                output_hash=output_hash,
            )
            endpoint = _endpoint_from_evaluator_row(
                sample=sample,
                evaluation_view=evaluation_view,
                row=row,
                usefulness_threshold=self.usefulness_threshold,
                endpoint_policy=self.endpoint_policy,
                release_mode=self.release_mode,
            )
        except ContractError as error:
            raise CommandEvaluationContractError(
                str(error), provider_io=provider_io
            ) from error

        evaluator_hash = sha256_json(row)
        self._successful_invocation_index += 1
        invocation_index = self._successful_invocation_index
        evaluator_capture_hash = self.access_store.capture_run_document(
            "evaluator-artifact-"
            f"{invocation_index:08d}-{sample.sample_id}-{output_hash}-{evaluator_hash[:16]}",
            row,
        )
        if evaluator_capture_hash != evaluator_hash:
            raise IntegrityError("captured evaluator artifact hash changed")
        provider_io_hash = self.access_store.capture_run_document(
            "evaluator-raw-provider-io-"
            f"{invocation_index:08d}-{sample.sample_id}-"
            f"{output_hash}-{sha256_json(provider_io)[:16]}",
            provider_io,
        )
        self.access_store.record_access(
            role="source-blind-evaluation-runner",
            artifact_hash=provider_io_hash,
            action="capture-raw-provider-io",
            reason=f"sample:{sample.sample_id};canonical-output:{output_hash}",
        )
        for evaluator_id in evaluator_ids:
            self.access_store.record_access(
                role=f"final-usefulness-rater:{evaluator_id}",
                artifact_hash=evaluator_hash,
                action="source-blind-evaluation",
                reason=f"sample:{sample.sample_id}",
            )
        if endpoint.evaluator_artifact_hash != evaluator_hash:
            raise IntegrityError("endpoint result lost evaluator artifact binding")
        return endpoint

    def _validated_response(
        self,
        response: Any,
        *,
        sample_id: str,
        output_hash: str,
    ) -> tuple[Mapping[str, Any], tuple[str, ...]]:
        if not isinstance(response, Mapping):
            raise ContractError("evaluator response must be an object")
        keys = set(response)
        if not self._REQUIRED_RESPONSE_FIELDS.issubset(keys) or keys - (
            self._REQUIRED_RESPONSE_FIELDS | self._OPTIONAL_RESPONSE_FIELDS
        ):
            raise ContractError("evaluator response has missing or unknown schema fields")
        if response.get("schema_version") != self._SCHEMA_VERSION:
            raise ContractError("unsupported live evaluator response schema")
        if response.get("sample_id") != sample_id:
            raise ContractError("evaluator response sample_id echo mismatch")
        if response.get("canonical_output_hash") != output_hash:
            raise ContractError("evaluator response canonical_output_hash echo mismatch")
        forbidden = {"arm_id", "source", "oracle_chain", "provider"}.intersection(response)
        if forbidden:
            raise ContractError(
                f"evaluator response leaks provenance {sorted(forbidden)}"
            )
        evaluator_ids = _validate_evaluator_roster(
            response,
            sample_id=sample_id,
            endpoint_policy=self.endpoint_policy,
            release_mode=self.release_mode,
            location="external evaluator response",
        )
        if self.release_mode or "usefulness_votes" in response:
            self._validate_usefulness_votes(
                response, evaluator_ids, required=self.release_mode
            )
        # A finite canonical round-trip detaches provider-owned mutable objects.
        try:
            row = json.loads(
                json.dumps(response, ensure_ascii=False, allow_nan=False)
            )
        except (TypeError, ValueError) as error:
            raise ContractError("evaluator response is not finite JSON") from error
        return row, evaluator_ids

    @staticmethod
    def _validate_usefulness_votes(
        response: Mapping[str, Any],
        evaluator_ids: tuple[str, ...],
        *,
        required: bool,
    ) -> None:
        votes = response.get("usefulness_votes")
        if votes is None and not required:
            return
        if not isinstance(votes, list) or len(votes) != len(evaluator_ids):
            raise ContractError(
                "evaluator response requires one usefulness vote per declared rater"
            )
        parsed: list[tuple[str, float]] = []
        for index, vote in enumerate(votes):
            if not isinstance(vote, Mapping) or set(vote) != {
                "evaluator_id",
                "score",
            }:
                raise ContractError(
                    f"usefulness_votes[{index}] must contain evaluator_id and score"
                )
            evaluator_id = vote.get("evaluator_id")
            score = vote.get("score")
            if (
                not isinstance(evaluator_id, str)
                or not isinstance(score, (int, float))
                or isinstance(score, bool)
                or not math.isfinite(float(score))
                or not 0.0 <= float(score) <= 1.0
            ):
                raise ContractError(
                    f"usefulness_votes[{index}] identity/score is invalid"
                )
            parsed.append((evaluator_id, float(score)))
        if tuple(identity for identity, _ in parsed) != evaluator_ids:
            raise ContractError(
                "usefulness vote roster/order differs from the declared evaluator roster"
            )
        recomputed = sum(score for _, score in parsed) / len(parsed)
        claimed = response.get("usefulness_consensus")
        if (
            not isinstance(claimed, (int, float))
            or isinstance(claimed, bool)
            or not math.isfinite(float(claimed))
            or not math.isclose(float(claimed), recomputed, rel_tol=0.0, abs_tol=1e-12)
        ):
            raise ContractError(
                "usefulness_consensus does not equal the mean of individual votes"
            )

    @staticmethod
    def _validate_source_blind_view(evaluation_view: Mapping[str, Any]) -> None:
        if not isinstance(evaluation_view, Mapping) or set(evaluation_view) != {
            "schema_version",
            "payload",
        }:
            raise ContractError("evaluator stdin must be exactly source_blind_evaluation_view")
        if evaluation_view.get("schema_version") != (
            "chesstory.eval.source-blind-view.v1"
        ) or not isinstance(evaluation_view.get("payload"), Mapping):
            raise ContractError("evaluator stdin has an invalid source-blind view schema")

    @staticmethod
    def _safe_process_bytes(value: str | bytes | None) -> bytes:
        if value is None:
            return b""
        if isinstance(value, bytes):
            return value
        return value.encode("utf-8", errors="replace")

    @classmethod
    def _provider_io_record(
        cls,
        base: Mapping[str, Any],
        *,
        returncode: int | None,
        stdout: str | bytes | None,
        stderr: str | bytes | None,
        failure: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        stdout_bytes = cls._safe_process_bytes(stdout)
        stderr_bytes = cls._safe_process_bytes(stderr)
        record = {
            **base,
            "returncode": returncode,
            "stdout": stdout_bytes.decode("utf-8", errors="replace"),
            "stderr": stderr_bytes.decode("utf-8", errors="replace"),
            "stdout_base64": base64.b64encode(stdout_bytes).decode("ascii"),
            "stderr_base64": base64.b64encode(stderr_bytes).decode("ascii"),
        }
        if failure is not None:
            record["failure"] = dict(failure)
        return record

    @staticmethod
    def _minimal_process_environment() -> dict[str, str]:
        allowed = {
            "COMSPEC",
            "LANG",
            "LC_ALL",
            "PATH",
            "PATHEXT",
            "SYSTEMROOT",
            "TEMP",
            "TMP",
            "TMPDIR",
            "WINDIR",
        }
        return {
            key: value for key, value in os.environ.items() if key.upper() in allowed
        }


class UnavailableEvaluator(EvaluationAdapter):
    def __init__(self, reason: str) -> None:
        self.reason = reason

    def evaluate(
        self,
        *,
        sample: Sample,
        evaluation_view: Mapping[str, Any],
    ) -> EndpointResult:
        raise StageUnavailable("V", self.reason)


def source_blind_evaluation_view(canonical_v: Mapping[str, Any]) -> dict[str, Any]:
    """Return the only document a final evaluator may observe.

    Run IDs, producer metadata, stage context, artifact lineage, oracle chain,
    and arm assignment are deliberately excluded.  The view contains only the
    canonical public verbalization meaning that a human evaluator must judge.
    """

    payload = canonical_v.get("payload")
    if not isinstance(payload, Mapping):
        raise ContractError("canonical V output has no payload for blind evaluation")
    return {
        "schema_version": "chesstory.eval.source-blind-view.v1",
        "payload": dict(payload),
    }


__all__ = [
    "CommandEvaluationAdapter",
    "CommandEvaluationContractError",
    "CommandEvaluationUnavailable",
    "EvaluationAdapter",
    "FrozenEndpointPolicy",
    "ReplayEvaluationAdapter",
    "UnavailableEvaluator",
    "source_blind_evaluation_view",
]
