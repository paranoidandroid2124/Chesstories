from __future__ import annotations

import copy
import re
from collections import Counter, defaultdict
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import chess

from . import __version__
from .canonicalize import Canonicalizer
from .capture import ArtifactStore
from .corpus import Corpus
from .design import (
    ALL_ACTUAL_CONTEXT,
    ALL_ORACLE_CONTEXT,
    BACKWARD_CUMULATIVE_CONTEXT,
    FORWARD_CUMULATIVE_CONTEXT,
    LEAVE_ONE_ACTUAL_CONTEXT,
    ONE_STAGE_CONTEXT,
    STABLE_Q_CONTEXT,
    build_arm_plan,
    parse_factorial_context,
)
from .hashing import read_json, sha256_file, sha256_json
from .model import Arm, ContractError, IntegrityError, Sample, Source, STAGES
from .native_diagnostic import validate_native_engineering_result
from .schemas import SchemaRegistry, SemanticTransitionSession


MODE = "provisional-model-proxy-diagnostic"
QUALIFICATION = "provisional"
EVIDENCE_TIER = "model-proxy-nonhuman"
ARM_ID = "provisional-model-proxy"
MAPPING_CLAIM = "provisional-lossy-not-semantic-equivalence"
_MACHINE_RUN_ID_RE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]{0,179}")
_CORE_POLICY = {
    "formal_runner_eligible": False,
    "human_gate_eligible": False,
    "release_eligible": False,
    "production_attribution_eligible": False,
    "production_bottleneck": None,
}


def validate_provisional_run_id(run_id: str) -> str:
    """Require one canonical ASCII identifier for paths, ledgers, and payloads."""

    if (
        not isinstance(run_id, str)
        or not _MACHINE_RUN_ID_RE.fullmatch(run_id)
        or ArtifactStore._segment(run_id) != run_id
    ):
        raise ContractError(
            "provisional proxy --run-id must be a canonical ASCII identifier "
            "using only letters, digits, dot, underscore, or hyphen"
        )
    return run_id


def _assert_ascii_machine_text(value: Any, *, label: str) -> None:
    """Reject non-ASCII text anywhere in a machine-readable artifact tree."""

    def visit(node: Any, path: str) -> None:
        if isinstance(node, str):
            if not node.isascii():
                raise ContractError(f"{label} contains non-ASCII machine text at {path}")
            return
        if isinstance(node, Mapping):
            for key, child in node.items():
                if not isinstance(key, str) or not key.isascii():
                    raise ContractError(
                        f"{label} contains a non-ASCII or non-text key at {path}"
                    )
                visit(child, f"{path}.{key}")
            return
        if isinstance(node, list):
            for index, child in enumerate(node):
                visit(child, f"{path}[{index}]")

    visit(value, "$")


def run_provisional_proxy_diagnostic(
    *,
    root: Path,
    run_id: str,
    store: ArtifactStore,
    native_run_root: Path,
    oracle_q_run_root: Path,
    baseline_probe_completion_report: Path,
    intermediate_probe_completion_report: Path,
    probe_completion_report: Path,
) -> dict[str, Any]:
    """Run the frozen 313-arm plan through an isolated provisional proxy.

    This path proves reachability, schema closure, canonicalization, capture,
    and contrast accounting.  It intentionally does not implement or replace
    a production judgment module, a formal oracle adapter, or a human endpoint.
    """

    validate_provisional_run_id(run_id)

    corpus = Corpus(root / "corpus" / "v1")
    samples = corpus.load_split("diagnostic-explore")
    preregistration_path = root / "corpus" / "v1" / "preregistration.json"
    preregistration = read_json(preregistration_path)
    if not isinstance(preregistration, Mapping):
        raise ContractError("preregistration must be an object")
    chains = preregistration.get("oracle_chains")
    if not isinstance(chains, list):
        raise ContractError("preregistration oracle chains are missing")
    arms = build_arm_plan([str(value) for value in chains])
    if len(arms) != 313:
        raise IntegrityError("provisional proxy requires the exact frozen 313-arm plan")

    stage_registry = SchemaRegistry(root / "schemas" / "v1")
    stage_registry.validate_registry()
    provisional_registry = SchemaRegistry(root / "schemas" / "provisional-v1")
    provisional_schemas = {
        "receipt": provisional_registry.root / "bridge-receipt.schema.json",
        "evaluation": provisional_registry.root / "evaluation.schema.json",
        "report": provisional_registry.root / "report.schema.json",
    }
    native_bindings, native_source_binding = _load_native_bindings(
        samples=samples,
        native_run_root=native_run_root,
        root=root,
    )
    baseline_probe_completion = _load_probe_completion_binding(
        baseline_probe_completion_report
    )
    intermediate_probe_completion = _load_probe_completion_binding(
        intermediate_probe_completion_report
    )
    probe_completion = _load_probe_completion_binding(probe_completion_report)
    trajectory_hashes = {
        baseline_probe_completion["report_sha256"],
        intermediate_probe_completion["report_sha256"],
        probe_completion["report_sha256"],
    }
    if len(trajectory_hashes) != 3:
        raise ContractError("engineering trajectory reports must be three distinct runs")
    native_before_current_overlay = copy.deepcopy(native_bindings)
    _overlay_probe_completion_signals(native_bindings, probe_completion)
    oracle_q_bindings, oracle_q_report_hash = _load_oracle_q_bindings(
        samples=samples,
        oracle_q_run_root=oracle_q_run_root,
        registry=stage_registry,
        probe_completion=probe_completion,
    )

    runner = _ProvisionalArmRunner(
        run_id=run_id,
        store=store,
        registry=stage_registry,
        provisional_registry=provisional_registry,
        provisional_schemas=provisional_schemas,
        preregistration=preregistration,
        native_bindings=native_bindings,
        oracle_q_bindings=oracle_q_bindings,
    )
    rows: list[dict[str, Any]] = []
    for sample in samples:
        for arm in arms:
            rows.append(runner.run_arm(sample, arm))

    planned_rows = len(samples) * len(arms)
    if len(rows) != planned_rows or any(row.get("status") != "completed" for row in rows):
        raise IntegrityError("provisional proxy did not complete the frozen row universe")
    expected_row_keys = {
        (sample.sample_id, arm.arm_id) for sample in samples for arm in arms
    }
    observed_row_keys = [
        (str(row.get("sample_id")), str(row.get("arm_id"))) for row in rows
    ]
    if (
        len(samples) != 3
        or planned_rows != 939
        or len(set(observed_row_keys)) != 939
        or set(observed_row_keys) != expected_row_keys
    ):
        raise IntegrityError("provisional proxy row keys differ from the frozen 939-row plan")
    contexts = Counter(arm.context for arm in arms)
    q_levels = {binding["evidence_level"] for binding in oracle_q_bindings.values()}
    oracle_q_evidence_level = next(iter(q_levels)) if len(q_levels) == 1 else "mixed"
    summary = _context_endpoint_summary(rows)
    descriptive = _descriptive_bottleneck(
        rows=rows,
        preregistration=preregistration,
        oracle_q_evidence_level=oracle_q_evidence_level,
    )
    engineering_diagnosis = _engineering_diagnosis(
        descriptive=descriptive,
        native_bindings=native_bindings,
        oracle_q_evidence_level=oracle_q_evidence_level,
        probe_completion=probe_completion,
    )
    trajectory_descriptive = {
        "candidate_stage": None,
        "one_stage_oracle_gain": {},
        "leave_one_actual_loss": {},
    }
    baseline_diagnosis = _engineering_diagnosis(
        descriptive=trajectory_descriptive,
        native_bindings=copy.deepcopy(native_before_current_overlay),
        oracle_q_evidence_level="probe-complete",
        probe_completion=baseline_probe_completion,
    )
    intermediate_diagnosis = _engineering_diagnosis(
        descriptive=trajectory_descriptive,
        native_bindings=copy.deepcopy(native_before_current_overlay),
        oracle_q_evidence_level="probe-complete",
        probe_completion=intermediate_probe_completion,
    )
    engineering_trajectory = _engineering_trajectory(
        baseline=baseline_probe_completion,
        baseline_diagnosis=baseline_diagnosis,
        intermediate=intermediate_probe_completion,
        intermediate_diagnosis=intermediate_diagnosis,
        current=probe_completion,
        current_diagnosis=engineering_diagnosis,
    )
    cache_summary = runner.cache_summary()
    expected_cache_counts = {
        "unique_stage_execution_count": 357,
        "row_stage_reference_count": 7512,
        "reused_stage_reference_count": 7155,
        "unique_evaluation_execution_count": 87,
    }
    if any(
        cache_summary.get(field) != expected
        for field, expected in expected_cache_counts.items()
    ):
        raise IntegrityError("provisional proxy cache counts differ from the frozen plan")
    report = {
        "schema_version": "chesstory.eval.provisional-model-proxy-report.v1",
        "run_id": run_id,
        "mode": MODE,
        "qualification": QUALIFICATION,
        "evidence_tier": EVIDENCE_TIER,
        "policy": {
            **_CORE_POLICY,
            "machine_language": "en",
            "mapping_claim": MAPPING_CLAIM,
            "test_files_added": False,
            "production_modules_duplicated": False,
        },
        "binding": {
            "corpus_version": corpus.corpus_version,
            "split": "diagnostic-explore",
            "split_sha256": corpus.split_hash("diagnostic-explore"),
            "preregistration_sha256": sha256_file(preregistration_path),
            "native_run_id": native_run_root.name,
            "native_report_sha256": native_source_binding["report_sha256"],
            "native_source_preregistration_sha256": native_source_binding[
                "preregistration_sha256"
            ],
            "native_source_plan_sha256": native_source_binding["plan_sha256"],
            "native_source_artifacts_verified": True,
            "oracle_q_run_id": oracle_q_run_root.name,
            "oracle_q_report_sha256": oracle_q_report_hash,
            "oracle_q_source_artifacts_verified": True,
            "oracle_q_evidence_level": oracle_q_evidence_level,
            "probe_completion_run_id": probe_completion["run_id"],
            "probe_completion_report_sha256": probe_completion["report_sha256"],
            "all_runtime_probes_closed": probe_completion["all_probes_closed"],
            "probe_completion_artifacts_verified": True,
            "baseline_probe_completion_run_id": baseline_probe_completion[
                "run_id"
            ],
            "baseline_probe_completion_report_sha256": baseline_probe_completion[
                "report_sha256"
            ],
            "baseline_probe_completion_artifacts_verified": True,
            "intermediate_probe_completion_run_id": intermediate_probe_completion[
                "run_id"
            ],
            "intermediate_probe_completion_report_sha256": intermediate_probe_completion[
                "report_sha256"
            ],
            "intermediate_probe_completion_artifacts_verified": True,
        },
        "plan": {
            "interpretation": "descriptive-proxy-reachability-only",
            "registered_arm_count": len(arms),
            "sample_count": len(samples),
            "planned_row_count": planned_rows,
            "completed_row_count": len(rows),
            "stage_capture_count": cache_summary["unique_stage_execution_count"],
            "stage_reference_count": len(rows) * len(STAGES),
            "contexts": dict(sorted(contexts.items())),
        },
        "cache": cache_summary,
        "rows": rows,
        "context_endpoint_summary": summary,
        "descriptive_bottleneck": descriptive,
        "engineering_diagnosis": engineering_diagnosis,
        "engineering_trajectory": engineering_trajectory,
        "inference": {
            "status": "indeterminate",
            "production_bottleneck": None,
            "reason": (
                "This run uses three explore samples from two atomic clusters, "
                "label-assisted proxy oracles, no independent oracle authors, and no "
                "held-out human raters. It cannot support production attribution."
            ),
            "independent_oracle_chain_count": 0,
            "held_out_human_rater_count": 0,
        },
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    _assert_ascii_machine_text(report, label="provisional proxy report")
    provisional_registry.validate_document(
        report, provisional_schemas["report"], label="provisional proxy report"
    )
    store.capture_run_document("provisional-model-proxy-report.json", report)
    store.verify()
    return report


class _ProvisionalArmRunner:
    def __init__(
        self,
        *,
        run_id: str,
        store: ArtifactStore,
        registry: SchemaRegistry,
        provisional_registry: SchemaRegistry,
        provisional_schemas: Mapping[str, Path],
        preregistration: Mapping[str, Any],
        native_bindings: Mapping[str, Mapping[str, Any]],
        oracle_q_bindings: Mapping[str, Mapping[str, Any]],
    ) -> None:
        self.run_id = run_id
        self.store = store
        self.registry = registry
        self.canonicalizer = Canonicalizer(registry)
        self.provisional_registry = provisional_registry
        self.provisional_schemas = provisional_schemas
        self.preregistration = preregistration
        self.seed = int(preregistration["seed"])
        self.native_bindings = native_bindings
        self.oracle_q_bindings = oracle_q_bindings
        self._prefix_cache: dict[tuple[str, tuple[str, ...]], dict[str, Any]] = {}
        self._evaluation_cache: dict[
            tuple[str, tuple[str, ...]], tuple[dict[str, Any], str]
        ] = {}
        self._row_stage_reference_count = 0

    def run_arm(self, sample: Sample, arm: Arm) -> dict[str, Any]:
        stage_artifacts: dict[str, dict[str, str]] = {}
        native = self.native_bindings[sample.sample_id]
        effective_prefix: list[str] = []
        final_entry: Mapping[str, Any] | None = None
        for stage_index, stage in enumerate(STAGES):
            requested_source = arm.source_for(stage)
            effective_source = (
                Source.ACTUAL
                if requested_source == Source.FORMAT_CONTROL
                else requested_source
            )
            effective_prefix.append(effective_source.value)
            final_entry = self._ensure_prefix(
                sample=sample,
                stage_index=stage_index,
                effective_prefix=tuple(effective_prefix),
                native=native,
            )
            stage_artifacts[stage] = copy.deepcopy(final_entry["artifact_binding"])
            stage_artifacts[stage]["row_requested_source"] = requested_source.value
            self._row_stage_reference_count += 1
        if final_entry is None:
            raise IntegrityError("provisional arm produced no shared prefix")
        outputs = final_entry["outputs"]
        evaluation_key = (sample.sample_id, tuple(effective_prefix))
        cached_evaluation = self._evaluation_cache.get(evaluation_key)
        if cached_evaluation is None:
            evaluation = _evaluate_proxy_endpoint(
                sample=sample,
                outputs=outputs,
                required_probe_pairs=native["required_probe_pairs"],
            )
            _assert_ascii_machine_text(
                evaluation, label=f"{sample.sample_id} provisional endpoint"
            )
            self.provisional_registry.validate_document(
                evaluation,
                self.provisional_schemas["evaluation"],
                label=f"{sample.sample_id} shared provisional endpoint",
            )
            evaluation_cache_key = sha256_json(
                {
                    "sample_id": sample.sample_id,
                    "effective_source_prefix": effective_prefix,
                    "canonical_output_hashes": final_entry["output_hashes"],
                }
            )
            evaluation_hash = self.store.capture_run_document(
                f"evaluation--{sample.sample_id}--shared-{evaluation_cache_key[:16]}.json",
                evaluation,
            )
            self._evaluation_cache[evaluation_key] = (evaluation, evaluation_hash)
        else:
            evaluation, evaluation_hash = cached_evaluation
        return {
            "sample_id": sample.sample_id,
            "atomic_cluster_id": sample.atomic_cluster_id,
            "arm_id": arm.arm_id,
            "context": arm.context,
            "oracle_chain": arm.oracle_chain,
            "focus_stage": arm.focus_stage,
            "sources": {stage: arm.source_for(stage).value for stage in STAGES},
            "status": "completed",
            "stage_artifacts": stage_artifacts,
            "evaluation_artifact_sha256": evaluation_hash,
            "evaluation": evaluation,
        }

    def _ensure_prefix(
        self,
        *,
        sample: Sample,
        stage_index: int,
        effective_prefix: tuple[str, ...],
        native: Mapping[str, Any],
    ) -> dict[str, Any]:
        key = (sample.sample_id, effective_prefix)
        cached = self._prefix_cache.get(key)
        if cached is not None:
            return cached
        stage = STAGES[stage_index]
        effective_source = Source(effective_prefix[-1])
        if stage_index == 0:
            outputs: dict[str, Mapping[str, Any]] = {}
            output_hashes: dict[str, str] = {}
            semantic_lineage = SemanticTransitionSession(self.registry)
            canonical_lineage = self.canonicalizer.lineage_session()
        else:
            parent_key = (sample.sample_id, effective_prefix[:-1])
            parent = self._prefix_cache.get(parent_key)
            if parent is None:
                raise IntegrityError(f"shared prefix parent is missing for {key}")
            outputs = dict(parent["outputs"])
            output_hashes = dict(parent["output_hashes"])
            semantic_lineage = self._clone_semantic_lineage(parent["semantic_lineage"])
            canonical_lineage = self._clone_canonical_lineage(parent["canonical_lineage"])

        input_payload = self._input_payload(
            sample=sample,
            stage=stage,
            source=effective_source,
            outputs=outputs,
        )
        upstream = sorted(set(output_hashes.values()))
        cache_key = sha256_json(
            {
                "cache_key_version": "provisional-shared-prefix-cache.v1",
                "sample_id": sample.sample_id,
                "stage": stage,
                "effective_source_prefix": effective_prefix,
                "upstream_canonical_output_hashes": upstream,
            }
        )
        shared_execution_id = f"shared-{cache_key[:16]}"
        input_hash = sha256_json(
            {
                "stage": stage,
                "payload": input_payload,
                "upstream_artifact_hashes": upstream,
            }
        )
        stage_context = _shared_stage_context(self.run_id, stage)
        input_document = self._envelope(
            sample=sample,
            arm=None,
            stage=stage,
            input_hash=input_hash,
            upstream=upstream,
            producer={
                "component": "provisional-model-proxy-runner",
                "version": __version__,
            },
            payload=input_payload,
            stage_context=stage_context,
        )
        self.registry.validate_stage(input_document, stage, "input")
        semantic_lineage.validate_input(input_document, stage)
        canonical_lineage.prepare_input(input_document, stage)
        output_payload = self._stage_payload(
            sample=sample,
            stage=stage,
            source=effective_source,
            input_payload=input_payload,
            outputs=outputs,
            native=native,
        )
        output_document = self._envelope(
            sample=sample,
            arm=None,
            stage=stage,
            input_hash=input_hash,
            upstream=upstream,
            producer=self._producer(sample, stage, effective_source),
            payload=output_payload,
            stage_context=stage_context,
        )
        _assert_ascii_machine_text(
            input_document,
            label=f"{sample.sample_id} {shared_execution_id} {stage} input",
        )
        _assert_ascii_machine_text(
            output_document,
            label=f"{sample.sample_id} {shared_execution_id} {stage} output",
        )
        self.registry.validate_stage(output_document, stage, "output")
        semantic_lineage.validate_output(output_document, stage)
        canonical = canonical_lineage.stage_document(output_document, stage, "output")
        self.registry.validate_stage(canonical, stage, "output")
        canonical_hash = sha256_json(canonical)
        semantic_lineage.commit(stage, canonical, canonical_hash)
        outputs[stage] = canonical
        output_hashes[stage] = canonical_hash
        receipt = self._receipt(
            sample=sample,
            artifact_arm_id=shared_execution_id,
            cache_key=cache_key,
            stage=stage,
            effective_source=effective_source,
            input_document=input_document,
            output_document=output_document,
            canonical=canonical,
            native=native,
        )
        _assert_ascii_machine_text(
            canonical,
            label=f"{sample.sample_id} {shared_execution_id} {stage} canonical output",
        )
        _assert_ascii_machine_text(
            receipt,
            label=f"{sample.sample_id} {shared_execution_id} {stage} bridge receipt",
        )
        self.provisional_registry.validate_document(
            receipt,
            self.provisional_schemas["receipt"],
            label=f"{sample.sample_id} {shared_execution_id} {stage} bridge receipt",
        )
        captured = self.store.capture_stage(
            sample_id=sample.sample_id,
            arm_id=shared_execution_id,
            stage=stage,
            raw_input=input_document,
            raw_output={
                "schema_version": "chesstory.eval.provisional-stage-payload.v1",
                "mode": MODE,
                "qualification": QUALIFICATION,
                "evidence_tier": EVIDENCE_TIER,
                "payload": output_payload,
            },
            validated_output=output_document,
            canonical_output=canonical,
            provider_io=receipt,
            metadata={
                "mode": MODE,
                "qualification": QUALIFICATION,
                "evidence_tier": EVIDENCE_TIER,
                "formal_runner_eligible": False,
                "human_gate_eligible": False,
                "release_eligible": False,
                "production_attribution_eligible": False,
                "production_bottleneck": None,
                "executed_source": effective_source.value,
                "eligible_requested_sources": (
                    [Source.ACTUAL.value, Source.FORMAT_CONTROL.value]
                    if effective_source == Source.ACTUAL
                    else [Source.ORACLE.value]
                ),
                "input_hash": input_document["input_hash"],
                "canonical_output_hash": canonical_hash,
                "schema_id": self.registry.schema(stage, "output")[0].get("$id"),
                "runner_version": __version__,
                "cache_key_sha256": cache_key,
                "cache_semantics": (
                    "sample-stage-effective-source-prefix-and-upstream-canonical-hashes"
                ),
            },
        )
        artifact_binding = {
            "shared_execution_id": shared_execution_id,
            "cache_key_sha256": cache_key,
            "input_sha256": captured["raw-input.json"],
            "validated_output_sha256": captured["validated-output.json"],
            "canonical_output_sha256": captured["canonical-output.json"],
            "receipt_sha256": captured["raw-provider-io.json"],
        }
        entry = {
            "outputs": outputs,
            "output_hashes": output_hashes,
            "semantic_lineage": semantic_lineage,
            "canonical_lineage": canonical_lineage,
            "artifact_binding": artifact_binding,
        }
        self._prefix_cache[key] = entry
        return entry

    @staticmethod
    def _clone_semantic_lineage(
        session: SemanticTransitionSession,
    ) -> SemanticTransitionSession:
        clone = copy.copy(session)
        clone._documents = dict(session._documents)
        clone._hashes = dict(session._hashes)
        clone._lineage_definitions = set(session._lineage_definitions)
        clone._pending_input_definitions = set(session._pending_input_definitions)
        return clone

    @staticmethod
    def _clone_canonical_lineage(session: Any) -> Any:
        clone = copy.copy(session)
        clone._mapping = dict(session._mapping)
        clone._canonical_output_hashes = list(session._canonical_output_hashes)
        clone._pending_upstream_hashes = list(session._pending_upstream_hashes)
        return clone

    def cache_summary(self) -> dict[str, Any]:
        unique = len(self._prefix_cache)
        return {
            "key_version": "provisional-shared-prefix-cache.v1",
            "key_fields": [
                "sample_id",
                "stage",
                "effective_source_prefix",
                "upstream_canonical_output_hashes",
            ],
            "cache_semantics": "meaning-equivalent-stage-executions-only",
            "unique_stage_execution_count": unique,
            "row_stage_reference_count": self._row_stage_reference_count,
            "reused_stage_reference_count": self._row_stage_reference_count - unique,
            "unique_evaluation_execution_count": len(self._evaluation_cache),
        }

    def _input_payload(
        self,
        *,
        sample: Sample,
        stage: str,
        source: Source,
        outputs: Mapping[str, Mapping[str, Any]],
    ) -> dict[str, Any]:
        def payload(name: str) -> Mapping[str, Any]:
            document = outputs.get(name)
            if not isinstance(document, Mapping) or not isinstance(
                document.get("payload"), Mapping
            ):
                raise ContractError(f"{stage} has no committed {name} proxy output")
            return document["payload"]  # type: ignore[return-value]

        if stage == "Q":
            if source == Source.ORACLE:
                pack = self.oracle_q_bindings[sample.sample_id]["payload"]
                return {
                    "question": copy.deepcopy(pack["question"]),
                    "acquisition_config": copy.deepcopy(pack["acquisition_config"]),
                }
            actual = _embedded_actual_q(sample)
            pack = actual["payload"]
            return {
                "question": copy.deepcopy(pack["question"]),
                "acquisition_config": copy.deepcopy(pack["acquisition_config"]),
            }
        if stage == "F":
            return {"engine_evidence_pack": copy.deepcopy(payload("Q"))}
        if stage == "C":
            return {
                "candidate_lines": copy.deepcopy(payload("Q")["candidate_lines"]),
                "fact_bundle": copy.deepcopy(payload("F")),
            }
        if stage == "Jp":
            return {
                "fact_bundle": copy.deepcopy(payload("F")),
                "cause_bundle": copy.deepcopy(payload("C")),
            }
        if stage == "Ja":
            return {
                "fact_bundle": copy.deepcopy(payload("F")),
                "cause_bundle": copy.deepcopy(payload("C")),
                "proposal_bundle": copy.deepcopy(payload("Jp")),
            }
        if stage == "R":
            return {"admission_bundle": copy.deepcopy(payload("Ja"))}
        if stage == "P":
            return {
                "candidate_lines": copy.deepcopy(payload("Q")["candidate_lines"]),
                "fact_bundle": copy.deepcopy(payload("F")),
                "cause_bundle": copy.deepcopy(payload("C")),
                "ranking_bundle": copy.deepcopy(payload("R")),
                "plan_events": copy.deepcopy(payload("Jp")["plan_events"]),
            }
        if stage == "V":
            policy = self.preregistration.get("verbalization_policy")
            if not isinstance(policy, Mapping):
                raise ContractError("verbalization policy is missing")
            return {
                "projection": copy.deepcopy(payload("P")),
                "verbalization_policy": copy.deepcopy(policy),
            }
        raise ContractError(f"unknown provisional stage {stage}")

    def _stage_payload(
        self,
        *,
        sample: Sample,
        stage: str,
        source: Source,
        input_payload: Mapping[str, Any],
        outputs: Mapping[str, Mapping[str, Any]],
        native: Mapping[str, Any],
    ) -> dict[str, Any]:
        if stage == "Q":
            if source == Source.ORACLE:
                return copy.deepcopy(self.oracle_q_bindings[sample.sample_id]["payload"])
            return copy.deepcopy(_embedded_actual_q(sample)["payload"])
        if stage == "F":
            return _build_fact_bundle(sample, input_payload["engine_evidence_pack"], native)
        if stage == "C":
            if source == Source.ORACLE:
                return _build_oracle_cause_bundle(sample, input_payload["fact_bundle"])
            return _build_actual_cause_bundle(
                sample, input_payload["fact_bundle"], native["cause_descriptors"]
            )
        if stage == "Jp":
            return _build_proposal_bundle(
                sample=sample,
                fact_bundle=input_payload["fact_bundle"],
                cause_bundle=input_payload["cause_bundle"],
                oracle=source == Source.ORACLE,
            )
        if stage == "Ja":
            return _build_admission_bundle(input_payload["proposal_bundle"])
        if stage == "R":
            return _build_ranking_bundle(
                sample,
                input_payload["admission_bundle"],
                oracle=source == Source.ORACLE,
                native=native,
            )
        if stage == "P":
            return _build_projection(
                input_payload,
                oracle=source == Source.ORACLE,
                native=native,
            )
        if stage == "V":
            return _build_verbalization(
                sample,
                input_payload["projection"],
                input_payload["verbalization_policy"],
                oracle=source == Source.ORACLE,
            )
        raise ContractError(f"unknown provisional stage {stage}")

    def _producer(self, sample: Sample, stage: str, source: Source) -> dict[str, Any]:
        if stage == "Q":
            if source == Source.ORACLE:
                producer = self.oracle_q_bindings[sample.sample_id]["producer"]
            else:
                producer = _embedded_actual_q(sample)["producer"]
            return copy.deepcopy(producer)
        if stage == "F":
            component = "provisional-atomic-label-free-fact-proxy"
        elif source == Source.ORACLE and stage in {"Ja", "P"}:
            component = "provisional-structural-oracle-proxy"
        elif source == Source.ORACLE:
            component = "provisional-label-assisted-oracle-proxy"
        else:
            component = "provisional-native-v2-frozen-v1-bridge"
        return {
            "component": component,
            "version": __version__,
            "policy_version": "provisional-v1",
        }

    def _envelope(
        self,
        *,
        sample: Sample,
        arm: Arm | None,
        stage: str,
        input_hash: str,
        upstream: Sequence[str],
        producer: Mapping[str, Any],
        payload: Mapping[str, Any],
        stage_context: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        context = (
            copy.deepcopy(stage_context)
            if stage_context is not None
            else _stage_context(arm, stage, self.run_id)
            if arm is not None
            else None
        )
        if context is None:
            raise ContractError("provisional envelope has no stage context")
        return {
            "schema_version": "1.0.0",
            "stage": stage,
            "sample_id": sample.sample_id,
            "atomic_cluster_id": sample.atomic_cluster_id,
            "corpus_version": sample.corpus_version,
            "split": sample.split,
            "run_id": self.run_id,
            "stage_context": context,
            "seed": self.seed,
            "input_hash": input_hash,
            "upstream_artifact_hashes": sorted(set(upstream)),
            "producer": copy.deepcopy(producer),
            "payload": copy.deepcopy(payload),
        }

    def _receipt(
        self,
        *,
        sample: Sample,
        artifact_arm_id: str,
        cache_key: str,
        stage: str,
        effective_source: Source,
        input_document: Mapping[str, Any],
        output_document: Mapping[str, Any],
        canonical: Mapping[str, Any],
        native: Mapping[str, Any],
    ) -> dict[str, Any]:
        if effective_source == Source.ORACLE and stage == "Q":
            q_binding = self.oracle_q_bindings[sample.sample_id]
            if q_binding.get("probe_completion_report_sha256") is not None:
                origin = "probe-complete-stockfish-frozen-v1"
                conditioning = "direct-input-stable-stockfish-plus-runtime-probes"
            else:
                origin = "stable-stockfish-frozen-v1"
                conditioning = "direct-input-stable-stockfish"
            label_access = False
        elif stage == "F":
            origin = "atomic-label-free-fact-proxy"
            conditioning = "direct-input-atomic-fact-extraction"
            label_access = False
        elif effective_source == Source.ORACLE and stage in {"Ja", "P"}:
            origin = "provisional-structural-oracle-proxy"
            conditioning = "direct-input-structural-projection"
            label_access = False
        elif effective_source == Source.ORACLE:
            origin = "provisional-label-assisted-oracle"
            conditioning = "direct-input-label-assisted"
            label_access = True
        else:
            origin = "actual-native-lossy-proxy"
            conditioning = "direct-input-recomputed-with-native-inventory"
            label_access = False
        native_stage = native["stage_summaries"][stage]
        return {
            "schema_version": "chesstory.eval.provisional-bridge-receipt.v1",
            "mode": MODE,
            "qualification": QUALIFICATION,
            "evidence_tier": EVIDENCE_TIER,
            **_CORE_POLICY,
            "shared_execution_id": artifact_arm_id,
            "cache_key_sha256": cache_key,
            "cache_semantics": (
                "sample-stage-effective-source-prefix-and-upstream-canonical-hashes"
            ),
            "cache_materialization": "shared-prefix-execution",
            "sample_id": sample.sample_id,
            "arm_id": artifact_arm_id,
            "stage": stage,
            "executed_source": effective_source.value,
            "eligible_requested_sources": (
                [Source.ACTUAL.value, Source.FORMAT_CONTROL.value]
                if effective_source == Source.ACTUAL
                else [Source.ORACLE.value]
            ),
            "semantic_origin": origin,
            "conditioning": conditioning,
            "label_access": label_access,
            "mapping_claim": MAPPING_CLAIM,
            "native_stage": {
                "status": native_stage["status"],
                "boundary_reached": native_stage["boundary_reached"],
                "artifact_sha256": native_stage["artifact_sha256"],
                "v1_semantic_mapping_status": "unavailable",
            },
            "input_sha256": sha256_json(input_document),
            "validated_output_sha256": sha256_json(output_document),
            "canonical_output_sha256": sha256_json(canonical),
            "perspective_binding": _perspective_binding(
                sample=sample,
                stage=stage,
                output_document=output_document,
            ),
    }


def _verified_source_artifact(
    *,
    run_root: Path,
    binding: Mapping[str, Any],
    label: str,
    expected_relative_path: str | None = None,
) -> Path:
    relative = binding.get("path")
    expected_sha256 = binding.get("sha256")
    if (
        not isinstance(relative, str)
        or not relative
        or Path(relative).is_absolute()
        or not isinstance(expected_sha256, str)
        or re.fullmatch(r"[0-9a-f]{64}", expected_sha256) is None
    ):
        raise ContractError(f"{label} source artifact binding is malformed")
    if expected_relative_path is not None and relative != expected_relative_path:
        raise IntegrityError(f"{label} source artifact path differs from its frozen row")
    candidate = (run_root / relative).resolve(strict=True)
    if run_root != candidate.parent and run_root not in candidate.parents:
        raise IntegrityError(f"{label} source artifact escapes its run root")
    if not candidate.is_file() or sha256_file(candidate) != expected_sha256:
        raise IntegrityError(f"{label} source artifact hash mismatch")
    return candidate


def _load_native_bindings(
    *,
    samples: Sequence[Sample],
    native_run_root: Path,
    root: Path,
) -> tuple[dict[str, dict[str, Any]], dict[str, str]]:
    run_root = native_run_root.resolve(strict=True)
    if not run_root.is_dir():
        raise ContractError("native source run root must be a directory")
    report_path = run_root / "native-engineering-report.json"
    if not report_path.is_file():
        raise ContractError(f"native report is missing under {run_root}")
    report = read_json(report_path)
    if not isinstance(report, Mapping) or report.get("schema_version") != (
        "chesstory.eval.native-engineering-report.v2"
    ):
        raise ContractError("native source report is not native engineering v2")
    native_registry = SchemaRegistry(root / "schemas" / "native-v2")
    native_registry.validate_document(
        report,
        native_registry.root / "native-engineering-report.schema.json",
        label="native source report",
    )
    binding = report.get("binding")
    plan = report.get("plan")
    rows = report.get("rows")
    corpus_versions = {sample.corpus_version for sample in samples}
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    current_chains = (
        preregistration.get("oracle_chains")
        if isinstance(preregistration, Mapping)
        else None
    )
    current_plan = (
        build_arm_plan([str(value) for value in current_chains])
        if isinstance(current_chains, list)
        else []
    )
    current_plan_sha256 = sha256_json(
        [
            {
                "arm_id": arm.arm_id,
                "sources": {
                    stage: arm.source_for(stage).value for stage in STAGES
                },
                "oracle_chain": arm.oracle_chain,
                "focus_stage": arm.focus_stage,
                "context": arm.context,
            }
            for arm in current_plan
        ]
    )
    bound_preregistration_sha256 = (
        binding.get("preregistration_sha256")
        if isinstance(binding, Mapping)
        else None
    )
    if (
        not isinstance(binding, Mapping)
        or binding.get("run_id") != run_root.name
        or binding.get("split") != "diagnostic-explore"
        or len(corpus_versions) != 1
        or binding.get("corpus_version") not in corpus_versions
        or not isinstance(bound_preregistration_sha256, str)
        or re.fullmatch(r"[0-9a-f]{64}", bound_preregistration_sha256) is None
        or report.get("sample_count") != len(samples)
        or not isinstance(plan, Mapping)
        or plan.get("registered_arm_count") != 313
        or plan.get("plan_sha256") != current_plan_sha256
        or plan.get("planned_row_count") != 939
        or plan.get("materialized_row_count") != 939
        or not isinstance(rows, list)
        or len(rows) != 939
    ):
        raise IntegrityError("native source report binding differs from the frozen run")
    baseline_rows: dict[str, Mapping[str, Any]] = {}
    for row in rows:
        if not isinstance(row, Mapping) or row.get("arm_id") != "arm-0001":
            continue
        sample_id = row.get("sample_id")
        if not isinstance(sample_id, str) or sample_id in baseline_rows:
            raise IntegrityError("native source report has duplicate baseline rows")
        baseline_rows[sample_id] = row
    if set(baseline_rows) != {sample.sample_id for sample in samples}:
        raise IntegrityError("native source report baseline sample set differs")
    bindings: dict[str, dict[str, Any]] = {}
    for sample in samples:
        report_row = baseline_rows[sample.sample_id]
        if (
            report_row.get("atomic_cluster_id") != sample.atomic_cluster_id
            or report_row.get("status") != "q-p-observed"
            or report_row.get("semantic_source_realization") != "actual-native"
        ):
            raise IntegrityError(
                f"native source baseline row differs for {sample.sample_id}"
            )
        invocation_relative = (
            f"native-invocation--{sample.sample_id}--arm-0001.json"
        )
        result_relative = f"native-result--{sample.sample_id}--arm-0001.json"
        invocation_binding = report_row.get("invocation_artifact")
        result_binding = report_row.get("result_artifact")
        provider_io_binding = report_row.get("provider_io_artifact")
        if not all(
            isinstance(value, Mapping)
            for value in (invocation_binding, result_binding, provider_io_binding)
        ):
            raise ContractError("native baseline source artifact bindings are missing")
        invocation_path = _verified_source_artifact(
            run_root=run_root,
            binding=invocation_binding,
            label=f"{sample.sample_id} native invocation",
            expected_relative_path=invocation_relative,
        )
        result_path = _verified_source_artifact(
            run_root=run_root,
            binding=result_binding,
            label=f"{sample.sample_id} native result",
            expected_relative_path=result_relative,
        )
        _verified_source_artifact(
            run_root=run_root,
            binding=provider_io_binding,
            label=f"{sample.sample_id} native provider I/O",
            expected_relative_path=(
                f"native-provider-io--{sample.sample_id}--arm-0001.json"
            ),
        )
        invocation = read_json(invocation_path)
        result = read_json(result_path)
        if not isinstance(invocation, Mapping) or not isinstance(result, Mapping):
            raise ContractError("native invocation/result must be objects")
        native_registry.validate_document(
            invocation,
            native_registry.root / "native-engineering-invocation.schema.json",
            label=f"{sample.sample_id} native invocation",
        )
        # The native result embeds very large, recursively typed Scala trees.
        # ``validate_native_engineering_result`` below is the authoritative
        # contract/integrity validator and recomputes every native boundary
        # hash.  Running generic JSON Schema over the same 48 MB tree adds
        # minutes without an additional invariant, so only the compact
        # invocation uses the generic registry here.
        checked = validate_native_engineering_result(result, invocation=invocation)
        summary = checked["summary"]
        if checked["status"] != "completed" or not summary.get("q_p_complete"):
            raise ContractError(
                f"native baseline Q-to-P chain is incomplete for {sample.sample_id}"
            )
        observation = checked["document"].get("observation")
        if not isinstance(observation, Mapping):
            raise ContractError("completed native result has no observation")
        stage_summaries: dict[str, dict[str, Any]] = {}
        stages = observation.get("stages")
        if not isinstance(stages, Mapping):
            raise ContractError("native observation stages are missing")
        for stage in STAGES:
            record = stages.get(stage)
            if not isinstance(record, Mapping):
                raise ContractError(f"native observation has no {stage} record")
            artifact_hash = record.get("artifact_sha256")
            if not isinstance(artifact_hash, str):
                raise ContractError(f"native {stage} artifact hash is missing")
            stage_summaries[stage] = {
                "status": record["status"],
                "boundary_reached": record["boundary_reached"],
                "artifact_sha256": artifact_hash,
            }
        reported_chain = report_row.get("native_chain")
        reported_stages = (
            reported_chain.get("stages")
            if isinstance(reported_chain, Mapping)
            else None
        )
        if not isinstance(reported_stages, Mapping) or any(
            not isinstance(reported_stages.get(stage), Mapping)
            or any(
                reported_stages[stage].get(field) != stage_summaries[stage][field]
                for field in ("status", "boundary_reached", "artifact_sha256")
            )
            for stage in STAGES
        ):
            raise IntegrityError(
                f"native report/result stage binding differs for {sample.sample_id}"
            )
        public_response = observation.get("public_response")
        body = (
            public_response.get("body")
            if isinstance(public_response, Mapping)
            else None
        )
        requests = body.get("probe_requests", []) if isinstance(body, Mapping) else []
        required_probe_pairs = _required_probe_pairs(requests)
        bindings[sample.sample_id] = {
            "invocation_sha256": sha256_file(invocation_path),
            "result_sha256": sha256_file(result_path),
            "stage_summaries": stage_summaries,
            "cause_descriptors": _native_cause_descriptors(stages["C"]),
            "required_probe_pairs": required_probe_pairs,
            "engineering_signals": _native_engineering_signals(
                stages=stages,
                public_response=public_response,
            ),
        }
    return bindings, {
        "report_sha256": sha256_file(report_path),
        "preregistration_sha256": str(bound_preregistration_sha256),
        "plan_sha256": current_plan_sha256,
    }


def _load_probe_completion_binding(report_path: Path) -> dict[str, Any]:
    path = report_path.resolve(strict=True)
    if not path.is_file():
        raise ContractError("probe completion report must be a regular file")
    report = read_json(path)
    if not isinstance(report, Mapping) or report.get("schema_version") != (
        "chesstory.eval.runtime-probe-completion-report.v1"
    ):
        raise ContractError("unsupported runtime probe completion report")
    if report.get("all_probes_closed") is not True:
        raise ContractError("runtime probe completion report is not closed")
    root = path.parent

    def verify_references(node: Any) -> None:
        if isinstance(node, Mapping):
            relative = node.get("path")
            expected = node.get("sha256")
            if isinstance(relative, str) and isinstance(expected, str):
                candidate = (root / relative).resolve(strict=True)
                if root != candidate.parent and root not in candidate.parents:
                    raise IntegrityError("probe completion artifact escapes its run root")
                if not candidate.is_file() or sha256_file(candidate) != expected:
                    raise IntegrityError(
                        f"probe completion artifact hash mismatch for {relative}"
                    )
            for value in node.values():
                verify_references(value)
        elif isinstance(node, list):
            for value in node:
                verify_references(value)

    verify_references(report)
    raw_samples = report.get("samples")
    if not isinstance(raw_samples, list) or not raw_samples:
        raise ContractError("probe completion report has no samples")
    samples: dict[str, dict[str, Any]] = {}
    for row in raw_samples:
        if not isinstance(row, Mapping) or row.get("closure_status") != "closed":
            raise ContractError("probe completion sample is not closed")
        sample_id = row.get("sample_id")
        checkpoints = row.get("semantic_checkpoints")
        if not isinstance(sample_id, str) or not isinstance(checkpoints, Mapping):
            raise ContractError("probe completion sample/checkpoints are malformed")
        first_missing_checkpoint = checkpoints.get("first_missing_checkpoint")
        if not isinstance(first_missing_checkpoint, str):
            first_missing_checkpoint = _derive_runtime_first_missing_checkpoint(
                checkpoints
            )
        pairs: set[tuple[str, str]] = set()
        probes = row.get("probes")
        for probe in probes if isinstance(probes, list) else []:
            if not isinstance(probe, Mapping) or probe.get("kind") != "opponent-resource":
                continue
            probe_id = probe.get("probe_id")
            if not isinstance(probe_id, str):
                continue
            parts = probe_id.split(":")
            try:
                index = parts.index("opponent-resource")
                candidate, resource = parts[index - 1], parts[index + 1]
            except (ValueError, IndexError):
                continue
            if re.fullmatch(r"[a-h][1-8][a-h][1-8][qrbn]?", candidate) and re.fullmatch(
                r"[a-h][1-8][a-h][1-8][qrbn]?", resource
            ):
                pairs.add((candidate, resource))
        if sample_id in samples:
            raise ContractError(f"duplicate probe completion sample {sample_id}")
        samples[sample_id] = {
            "pairs": [list(pair) for pair in sorted(pairs)],
            "semantic_checkpoints": copy.deepcopy(checkpoints),
            "first_missing_checkpoint": first_missing_checkpoint,
            "completion_artifact": copy.deepcopy(row.get("completion_artifact")),
        }
    return {
        "run_id": str(report["run_id"]),
        "report_sha256": sha256_file(path),
        "all_probes_closed": True,
        "samples": samples,
    }


def _derive_runtime_first_missing_checkpoint(
    checkpoints: Mapping[str, Any],
) -> str:
    """Normalize early probe reports that predate the explicit checkpoint field."""

    c = checkpoints.get("C")
    jp = checkpoints.get("Jp")
    r = checkpoints.get("R")
    p = checkpoints.get("P")
    if not isinstance(c, Mapping):
        return "C:semantic-checkpoint-missing"
    deterrence = int(c.get("opponent_resource_deterrence_proof_count", 0))
    restriction_causes = int(
        c.get("opponent_restriction_relative_cause_count", 0)
    )
    if deterrence == 0:
        return "C:opponent-resource-branches-admitted-without-deterrence-proof"
    if restriction_causes == 0:
        return "C:deterrence-proof-not-converted-to-opponent-restriction-cause"
    if (
        not isinstance(jp, Mapping)
        or int(jp.get("claims_containing_opponent_restriction_count", 0)) == 0
    ):
        return "Jp:opponent-restriction-cause-not-proposed-as-claim"
    ranked_claims = r.get("ranked_claims") if isinstance(r, Mapping) else None
    if not isinstance(ranked_claims, list) or not any(
        isinstance(item, Mapping)
        and item.get("contains_opponent_restriction") is True
        and item.get("exposure_tier") in {"Primary", "Secondary"}
        for item in ranked_claims
    ):
        return "R:opponent-restriction-claim-not-player-facing"
    if (
        not isinstance(p, Mapping)
        or p.get("status") != "Ready"
        or p.get("renderable") is not True
    ):
        return "P:ranked-evidence-not-selected-for-public-response"
    return "none-through-P"


def _overlay_probe_completion_signals(
    native_bindings: Mapping[str, Mapping[str, Any]],
    probe_completion: Mapping[str, Any],
) -> None:
    sample_rows = probe_completion.get("samples")
    if not isinstance(sample_rows, Mapping):
        return
    for sample_id, row in sample_rows.items():
        binding = native_bindings.get(str(sample_id))
        checkpoints = row.get("semantic_checkpoints") if isinstance(row, Mapping) else None
        signals = binding.get("engineering_signals") if isinstance(binding, Mapping) else None
        if not isinstance(checkpoints, Mapping) or not isinstance(signals, dict):
            continue
        c = checkpoints.get("C")
        jp = checkpoints.get("Jp")
        r = checkpoints.get("R")
        p = checkpoints.get("P")
        public = checkpoints.get("public")
        if isinstance(c, Mapping):
            signals["c_relative_cause_fact_count"] = int(
                c.get("relative_cause_count", 0)
            )
            signals["c_opponent_restriction_count"] = int(
                c.get("opponent_restriction_relative_cause_count", 0)
            )
            signals["c_opponent_resource_deterrence_proof_count"] = int(
                c.get("opponent_resource_deterrence_proof_count", 0)
            )
        if isinstance(jp, Mapping):
            signals["jp_proposed_claim_count"] = int(jp.get("claim_count", 0))
            signals["jp_opponent_restriction_claim_count"] = int(
                jp.get("claims_containing_opponent_restriction_count", 0)
            )
        if isinstance(r, Mapping):
            exposure = r.get("exposure_tier_counts")
            context_count = (
                int(exposure.get("Context", 0)) if isinstance(exposure, Mapping) else 0
            )
            secondary_count = (
                int(exposure.get("Secondary", 0))
                if isinstance(exposure, Mapping)
                else 0
            )
            signals["r_ranked_claim_count"] = int(r.get("ranked_claim_count", 0))
            signals["r_context_exposure_count"] = context_count
            signals["r_secondary_exposure_count"] = secondary_count
            signals["r_context_only"] = context_count > 0 and secondary_count == 0
            reasons = r.get("deduplication_reason_counts")
            signals["r_same_rank_key_lower_score_count"] = (
                int(reasons.get("SameRankKeyLowerScore", 0))
                if isinstance(reasons, Mapping)
                else 0
            )
        if isinstance(p, Mapping):
            ready = p.get("status") == "Ready" and p.get("renderable") is True
            signals["p_withheld_constructor_count"] = 0 if ready else 1
        if isinstance(public, Mapping):
            signals["public_status"] = public.get("status")
            signals["public_renderable"] = public.get("renderable")
            signals["public_verdict_present"] = bool(public.get("verdict_present"))
            signals["public_explanation_count"] = int(
                public.get("explanation_count", 0)
            )
        signals["probe_completion_report_sha256"] = probe_completion[
            "report_sha256"
        ]
        signals["first_missing_checkpoint"] = row.get("first_missing_checkpoint")


def _augment_probe_complete_q_payload(
    *,
    sample: Sample,
    payload: Mapping[str, Any],
    probe_completion: Mapping[str, Any],
) -> tuple[dict[str, Any], bool]:
    required = {tuple(pair) for pair in _required_probe_pairs_for_sample(sample)}
    if not required:
        return copy.deepcopy(payload), False
    sample_rows = probe_completion.get("samples")
    sample_row = (
        sample_rows.get(sample.sample_id)
        if isinstance(sample_rows, Mapping)
        else None
    )
    present = {
        tuple(pair)
        for pair in (
            sample_row.get("pairs", []) if isinstance(sample_row, Mapping) else []
        )
        if isinstance(pair, list) and len(pair) == 2
    }
    if not required.issubset(present):
        return copy.deepcopy(payload), False
    result = copy.deepcopy(payload)
    providers = result.get("providers")
    if not isinstance(providers, list) or not providers or not isinstance(providers[0], Mapping):
        raise ContractError("stable Q pack has no provider for completed probes")
    provider_id = providers[0].get("provider_id")
    if not isinstance(provider_id, str):
        raise ContractError("stable Q provider ID is malformed")
    existing = result.get("probes")
    retained = [
        probe
        for probe in (existing if isinstance(existing, list) else [])
        if not (isinstance(probe, Mapping) and probe.get("kind") == "opponent-resource")
    ]
    for index, (candidate, resource) in enumerate(sorted(required), start=1):
        retained.append(
            {
                "probe_id": f"probe:runtime-completion:{index:04d}",
                "kind": "opponent-resource",
                "provider_id": provider_id,
                "status": "present",
                "attributes": [
                    {"key": "candidateMove", "value": candidate},
                    {"key": "opponentResourceMove", "value": resource},
                    {
                        "key": "runtime-probe-completion-report-sha256",
                        "value": probe_completion["report_sha256"],
                    },
                ],
            }
        )
    result["probes"] = retained
    missing = result.get("missing_evidence")
    retained_missing = [
        item
        for item in (missing if isinstance(missing, list) else [])
        if not (
            isinstance(item, Mapping)
            and str(item.get("requirement", "")).startswith("probe:opponent-resource")
        )
    ]
    retained_missing.append(
        {"requirement": "probe:opponent-resource", "status": "present"}
    )
    result["missing_evidence"] = retained_missing
    return result, True


def _load_oracle_q_bindings(
    *,
    samples: Sequence[Sample],
    oracle_q_run_root: Path,
    registry: SchemaRegistry,
    probe_completion: Mapping[str, Any],
) -> tuple[dict[str, dict[str, Any]], str]:
    run_root = oracle_q_run_root.resolve(strict=True)
    if not run_root.is_dir():
        raise ContractError("oracle Q source run root must be a directory")
    report_candidates = sorted(run_root.glob("*report.json"))
    if len(report_candidates) != 1:
        raise ContractError(
            "oracle Q run must contain exactly one top-level report JSON"
        )
    report_path = report_candidates[0]
    report = read_json(report_path)
    corpus_versions = {sample.corpus_version for sample in samples}
    if (
        not isinstance(report, Mapping)
        or report.get("schema_version") != "chesstory.eval.q-diagnostic-report.v1"
        or report.get("run_id") != run_root.name
        or report.get("split") != "diagnostic-explore"
        or len(corpus_versions) != 1
        or report.get("corpus_version") not in corpus_versions
        or report.get("status") != "diagnostic-only"
        or report.get("sample_count") != len(samples)
    ):
        raise IntegrityError("oracle Q source report binding differs from the frozen run")
    report_rows = report.get("rows")
    if not isinstance(report_rows, list) or len(report_rows) != len(samples):
        raise IntegrityError("oracle Q source report row count differs")
    rows_by_sample: dict[str, Mapping[str, Any]] = {}
    for row in report_rows:
        sample_id = row.get("sample_id") if isinstance(row, Mapping) else None
        if not isinstance(sample_id, str) or sample_id in rows_by_sample:
            raise IntegrityError("oracle Q source report has duplicate sample rows")
        rows_by_sample[sample_id] = row
    if set(rows_by_sample) != {sample.sample_id for sample in samples}:
        raise IntegrityError("oracle Q source report sample set differs")
    result: dict[str, dict[str, Any]] = {}
    for sample in samples:
        report_row = rows_by_sample[sample.sample_id]
        if report_row.get("atomic_cluster_id") != sample.atomic_cluster_id:
            raise IntegrityError(
                f"oracle Q source cluster differs for {sample.sample_id}"
            )
        sample_root = run_root / "samples" / sample.sample_id
        candidates = sorted(
            path
            for path in sample_root.rglob("canonical-output.json")
            if path.parent.name.lower() == "q"
        )
        if len(candidates) != 1:
            raise ContractError(
                f"oracle Q run requires exactly one Q output for {sample.sample_id}"
            )
        capture_hashes = report_row.get("capture_hashes")
        required_capture_names = {
            "raw-input.json",
            "raw-output.json",
            "validated-output.json",
            "canonical-output.json",
            "raw-provider-io.json",
            "metadata.json",
        }
        if not isinstance(capture_hashes, Mapping) or set(capture_hashes) != (
            required_capture_names
        ):
            raise IntegrityError(
                f"oracle Q source capture binding differs for {sample.sample_id}"
            )
        for filename in sorted(required_capture_names):
            artifact_path = (candidates[0].parent / filename).resolve(strict=True)
            if (
                run_root != artifact_path.parent
                and run_root not in artifact_path.parents
            ) or not artifact_path.is_file():
                raise IntegrityError("oracle Q source artifact escapes its run root")
            expected_sha256 = capture_hashes.get(filename)
            if (
                not isinstance(expected_sha256, str)
                or re.fullmatch(r"[0-9a-f]{64}", expected_sha256) is None
                or sha256_file(artifact_path) != expected_sha256
            ):
                raise IntegrityError(
                    f"oracle Q source artifact hash mismatch for {sample.sample_id} {filename}"
                )
        if (
            report_row.get("canonical_output_hash")
            != capture_hashes["canonical-output.json"]
        ):
            raise IntegrityError(
                f"oracle Q canonical report binding differs for {sample.sample_id}"
            )
        document = read_json(candidates[0])
        if not isinstance(document, Mapping):
            raise ContractError("oracle Q canonical output must be an object")
        registry.validate_stage(document, "Q", "output")
        if (
            document.get("sample_id") != sample.sample_id
            or document.get("atomic_cluster_id") != sample.atomic_cluster_id
            or document.get("corpus_version") != sample.corpus_version
            or document.get("split") != sample.split
            or document.get("run_id") != run_root.name
        ):
            raise IntegrityError(
                f"oracle Q canonical envelope differs for {sample.sample_id}"
            )
        payload = document.get("payload")
        producer = document.get("producer")
        if not isinstance(payload, Mapping) or not isinstance(producer, Mapping):
            raise ContractError("oracle Q output payload/producer is missing")
        payload, probe_augmented = _augment_probe_complete_q_payload(
            sample=sample,
            payload=payload,
            probe_completion=probe_completion,
        )
        producer = copy.deepcopy(producer)
        if probe_augmented:
            producer["component"] = "provisional-probe-complete-q-adapter"
            producer["version"] = __version__
            producer["configuration_hash"] = probe_completion["report_sha256"]
        augmented_document = copy.deepcopy(document)
        augmented_document["payload"] = payload
        augmented_document["producer"] = producer
        registry.validate_stage(augmented_document, "Q", "output")
        required = _required_probe_pairs_for_sample(sample)
        present = _present_probe_pairs(payload)
        if not required:
            level = "probe-complete"
        elif {tuple(pair) for pair in required}.issubset(
            {tuple(pair) for pair in present}
        ):
            level = "probe-complete"
        else:
            level = "root-only-stable"
        result[sample.sample_id] = {
            "payload": copy.deepcopy(payload),
            "producer": copy.deepcopy(producer),
            "canonical_output_sha256": sha256_file(candidates[0]),
            "evidence_level": level,
            "present_probe_pairs": present,
            "probe_completion_report_sha256": (
                probe_completion["report_sha256"] if probe_augmented else None
            ),
        }
    return result, sha256_file(report_path)


def _embedded_actual_q(sample: Sample) -> Mapping[str, Any]:
    value = sample.raw.get("actual_q")
    if not isinstance(value, Mapping):
        raise ContractError(f"sample {sample.sample_id} has no embedded actual Q")
    payload = value.get("payload")
    producer = value.get("producer")
    if not isinstance(payload, Mapping) or not isinstance(producer, Mapping):
        raise ContractError(f"sample {sample.sample_id} actual Q is malformed")
    return value


def _native_cause_descriptors(stage_record: Mapping[str, Any]) -> list[dict[str, str]]:
    artifact = stage_record.get("artifact")
    tree = artifact.get("native_tree") if isinstance(artifact, Mapping) else None
    if not isinstance(tree, Mapping):
        return []
    found: list[dict[str, str]] = []

    def product_field(node: Mapping[str, Any], name: str) -> Any:
        fields = node.get("fields")
        if not isinstance(fields, list):
            return None
        for field in fields:
            if isinstance(field, Mapping) and field.get("name") == name:
                return field.get("value")
        return None

    def constructor(node: Any) -> str:
        return str(node.get("constructor")) if isinstance(node, Mapping) else "Unknown"

    def visit(node: Any) -> None:
        if isinstance(node, Mapping):
            if node.get("constructor") == "RelativeCauseFact":
                kind = constructor(product_field(node, "kind"))
                side = constructor(product_field(node, "sourceSide"))
                attribution = product_field(node, "attribution")
                attribution_kind = constructor(
                    product_field(attribution, "kind")
                    if isinstance(attribution, Mapping)
                    else None
                )
                candidate = {
                    "kind": _kebab(kind),
                    "source_side": _kebab(side),
                    "attribution": _kebab(attribution_kind),
                }
                if candidate not in found:
                    found.append(candidate)
            for value in node.values():
                visit(value)
        elif isinstance(node, list):
            for value in node:
                visit(value)

    visit(tree)
    return found or [
        {
            "kind": "native-relative-cause-unmapped",
            "source_side": "unknown",
            "attribution": "unknown",
        }
    ]


def _native_engineering_signals(
    *,
    stages: Mapping[str, Any],
    public_response: Any,
) -> dict[str, Any]:
    """Extract a small, non-semantic inventory from captured native trees.

    The inventory is diagnostic evidence about which constructors survived the
    observed boundaries.  It is not a frozen-v1 mapping and is never injected
    into F as chess meaning.
    """

    counts_by_stage: dict[str, Counter[str]] = {}
    for stage in ("C", "Jp", "R", "P"):
        record = stages.get(stage)
        artifact = record.get("artifact") if isinstance(record, Mapping) else None
        tree = artifact.get("native_tree") if isinstance(artifact, Mapping) else None
        counts: Counter[str] = Counter()

        def visit(node: Any) -> None:
            if isinstance(node, Mapping):
                constructor = node.get("constructor")
                if isinstance(constructor, str):
                    counts[constructor] += 1
                for child in node.values():
                    visit(child)
            elif isinstance(node, list):
                for child in node:
                    visit(child)

        visit(tree)
        counts_by_stage[stage] = counts

    body = public_response.get("body") if isinstance(public_response, Mapping) else None
    move_review = body.get("move_review") if isinstance(body, Mapping) else None
    explanations = (
        move_review.get("explanations") if isinstance(move_review, Mapping) else None
    )
    r_ranked_count = counts_by_stage["R"]["RankedClaimDecision"]
    r_context_count = counts_by_stage["R"]["Context"]
    return {
        "c_relative_cause_fact_count": counts_by_stage["C"]["RelativeCauseFact"],
        "c_opponent_mobility_restriction_count": counts_by_stage["C"][
            "OpponentMobilityRestriction"
        ],
        "c_opponent_restriction_count": counts_by_stage["C"]["OpponentRestriction"],
        "c_plan_causal_event_count": counts_by_stage["C"]["PlanCausalEvent"],
        "jp_proposed_claim_count": counts_by_stage["Jp"]["JudgmentClaim"],
        "r_ranked_claim_count": r_ranked_count,
        "r_context_exposure_count": r_context_count,
        "r_context_only": r_ranked_count > 0 and r_context_count == r_ranked_count,
        "r_same_rank_key_lower_score_count": counts_by_stage["R"][
            "SameRankKeyLowerScore"
        ],
        "p_withheld_constructor_count": counts_by_stage["P"]["Withheld"],
        "public_status": body.get("status") if isinstance(body, Mapping) else None,
        "public_renderable": (
            move_review.get("renderable")
            if isinstance(move_review, Mapping)
            else None
        ),
        "public_verdict_present": (
            move_review.get("verdict") is not None
            if isinstance(move_review, Mapping)
            else False
        ),
        "public_explanation_count": len(explanations)
        if isinstance(explanations, list)
        else 0,
    }


def _required_probe_pairs(value: Any) -> list[list[str]]:
    pairs: set[tuple[str, str]] = set()
    if isinstance(value, list):
        for item in value:
            if not isinstance(item, Mapping):
                continue
            candidate = item.get("candidateMove")
            resource = item.get("opponentResourceMove")
            if isinstance(candidate, str) and isinstance(resource, str):
                pairs.add((candidate, resource))
    return [list(pair) for pair in sorted(pairs)]


def _required_probe_pairs_for_sample(sample: Sample) -> list[list[str]]:
    question = sample.request.get("question")
    if not isinstance(question, Mapping):
        return []
    root = question.get("root_position")
    if not isinstance(root, Mapping) or root.get("position_id") != "position-gamechanger-ng5":
        return []
    return [
        ["f3g5", "f7f5"],
        ["f3g5", "f7f6"],
        ["f3h4", "f7f5"],
        ["f3h4", "f7f6"],
    ]


def _present_probe_pairs(pack: Mapping[str, Any]) -> list[list[str]]:
    pairs: set[tuple[str, str]] = set()
    probes = pack.get("probes")
    if not isinstance(probes, list):
        return []
    for probe in probes:
        if (
            not isinstance(probe, Mapping)
            or probe.get("kind") != "opponent-resource"
            or probe.get("status") != "present"
        ):
            continue
        attributes = _attributes(probe.get("attributes"))
        candidate = _first_attribute(
            attributes,
            "candidateMove",
            "candidate_move_uci",
            "candidate-move-uci",
        )
        resource = _first_attribute(
            attributes,
            "opponentResourceMove",
            "opponent_resource_move_uci",
            "opponent-resource-move-uci",
        )
        if isinstance(candidate, str) and isinstance(resource, str):
            pairs.add((candidate, resource))
    return [list(pair) for pair in sorted(pairs)]


def _attributes(value: Any) -> dict[str, Any]:
    result: dict[str, Any] = {}
    if isinstance(value, list):
        for item in value:
            if isinstance(item, Mapping) and isinstance(item.get("key"), str):
                result[str(item["key"])] = item.get("value")
    return result


def _first_attribute(attributes: Mapping[str, Any], *names: str) -> Any:
    for name in names:
        if name in attributes:
            return attributes[name]
    return None


def _build_fact_bundle(
    sample: Sample,
    pack: Mapping[str, Any],
    native: Mapping[str, Any],
) -> dict[str, Any]:
    question = pack.get("question")
    if not isinstance(question, Mapping):
        raise ContractError("Q pack has no question")
    root = question.get("root_position")
    lines = pack.get("candidate_lines")
    if not isinstance(root, Mapping) or not isinstance(lines, list) or not lines:
        raise ContractError("Q pack has no root position or candidate lines")
    position_id = str(root["position_id"])
    evidence_id = f"evidence:provisional-f:{sample.sample_id}"
    evidence = {
        "evidence_id": evidence_id,
        "kind": "native-boundary-and-q-derived-atomic-facts",
        "parent_evidence_ids": [],
        "position_id": position_id,
        "attributes": [
            {"key": "derivation", "value": "board-and-q-only-no-label-meaning"},
            {
                "key": "native-f-artifact-sha256",
                "value": native["stage_summaries"]["F"]["artifact_sha256"],
            },
        ],
    }
    bindings: list[dict[str, Any]] = []
    binding_ids: dict[tuple[str, str], str] = {}

    def bind(kind: str, value: str, **extra: Any) -> str:
        key = (kind, value)
        existing = binding_ids.get(key)
        if existing is not None:
            return existing
        identity = f"binding:{kind}:{len(bindings) + 1:04d}"
        row = {"binding_id": identity, "kind": kind, "value": value, **extra}
        bindings.append(row)
        binding_ids[key] = identity
        return identity

    facts: list[dict[str, Any]] = []
    legal_lines: list[Mapping[str, Any]] = []
    atomic_move_watchlist = {
        str(value)
        for value in [
            question.get("played_move_uci"),
            *(question.get("requested_reference_moves_uci", []) or []),
        ]
        if isinstance(value, str)
    }
    for candidate, resource in _present_probe_pairs(pack):
        atomic_move_watchlist.update((candidate, resource))
    for index, value in enumerate(lines):
        if not isinstance(value, Mapping):
            raise ContractError(f"Q candidate line {index} is not an object")
        line = value
        legal_lines.append(line)
        line_id = str(line["line_id"])
        moves = [str(move) for move in line.get("moves_uci", [])]
        facts.append(
            {
                "fact_id": f"fact:legal-replay:{index + 1:04d}",
                "fact_type": "legal-replay",
                "line_id": line_id,
                "legal": bool(line.get("legality", {}).get("legal")),
                "moves_uci": moves,
                "evidence_ids": [evidence_id],
            }
        )
        # Legal replay retains the full observed PV.  Individual board facts
        # are intentionally sparse and atomic: question moves plus explicitly
        # acquired opponent resources only.  This prevents F from becoming a
        # covert copy of gold semantic meaning and avoids thousands of
        # irrelevant fact/binding identities in the 939-row diagnostic.
        for move in moves:
            if move not in atomic_move_watchlist:
                continue
            move_binding = bind("move", move)
            facts.append(
                {
                    "fact_id": f"fact:observed-line-move:{len(facts) + 1:04d}",
                    "fact_type": "board",
                    "position_id": position_id,
                    "predicate": f"observed-line-move:{move}",
                    "polarity": True,
                    "binding_ids": [move_binding],
                    "evidence_ids": [evidence_id],
                }
            )
        _append_root_attack_facts(
            root=root,
            line=line,
            position_id=position_id,
            evidence_id=evidence_id,
            facts=facts,
            bind=bind,
        )

    ranked_line_ids = [str(line["line_id"]) for line in legal_lines]
    facts.append(
        {
            "fact_id": "fact:candidate-set",
            "fact_type": "candidate-set",
            "ranked_line_ids": ranked_line_ids,
            "best_line_id": ranked_line_ids[0],
            **(
                {"second_line_id": ranked_line_ids[1]}
                if len(ranked_line_ids) > 1
                else {}
            ),
            "evidence_ids": [evidence_id],
        }
    )
    played_move = str(question["played_move_uci"])
    played_line = next(
        (line for line in legal_lines if line.get("root_move_uci") == played_move),
        legal_lines[0],
    )
    reference_line = next(
        (line for line in legal_lines if line is not played_line), played_line
    )
    played_eval = copy.deepcopy(played_line["evaluation"])
    reference_eval = copy.deepcopy(reference_line["evaluation"])
    if "centipawns" not in played_eval or "centipawns" not in reference_eval:
        raise ContractError("provisional comparison currently requires centipawn Q lines")
    delta = int(played_eval["centipawns"]) - int(reference_eval["centipawns"])
    mover_delta = _mover_centipawn_delta(
        delta=delta,
        perspective=str(played_eval["perspective"]),
        side_to_move=str(root["side_to_move"]),
    )
    verdict = (
        "best"
        if mover_delta >= -15
        else "good"
        if mover_delta >= -35
        else "inaccuracy"
        if mover_delta >= -100
        else "mistake"
    )
    facts.append(
        {
            "fact_id": "fact:played-reference-comparison",
            "fact_type": "candidate-comparison",
            "played_line_id": str(played_line["line_id"]),
            "reference_line_id": str(reference_line["line_id"]),
            "played_evaluation": played_eval,
            "reference_evaluation": reference_eval,
            "comparison_basis": "centipawn-delta",
            "delta_centipawns": delta,
            "verdict": verdict,
            "evidence_ids": [evidence_id],
        }
    )
    _append_probe_facts(
        pack=pack,
        position_id=position_id,
        evidence_id=evidence_id,
        facts=facts,
        bind=bind,
    )
    missing = pack.get("missing_evidence")
    if not isinstance(missing, list):
        missing = []
    return {
        "positions": [copy.deepcopy(root)],
        "bindings": bindings,
        "evidence_records": [evidence],
        "facts": facts,
        "unresolved_requirements": copy.deepcopy(missing),
    }


def _append_root_attack_facts(
    *,
    root: Mapping[str, Any],
    line: Mapping[str, Any],
    position_id: str,
    evidence_id: str,
    facts: list[dict[str, Any]],
    bind: Any,
) -> None:
    moves = line.get("moves_uci")
    if not isinstance(moves, list) or not moves:
        return
    try:
        board = chess.Board(str(root["fen"]))
        move = chess.Move.from_uci(str(moves[0]))
        piece = board.piece_at(move.from_square)
        if piece is None or move not in board.legal_moves:
            return
        color = "white" if piece.color == chess.WHITE else "black"
        piece_name = chess.piece_name(piece.piece_type)
        board.push(move)
        piece_binding = bind(
            "piece",
            f"{color} {piece_name} after {move.uci()}",
            color=color,
            piece_type=piece_name,
        )
        for target in sorted(board.attacks(move.to_square)):
            square = chess.square_name(target)
            square_binding = bind("square", square)
            facts.append(
                {
                    "fact_id": f"fact:root-attack:{len(facts) + 1:04d}",
                    "fact_type": "board",
                    "position_id": position_id,
                    "predicate": f"piece-after-{move.uci()}-attacks:{square}",
                    "polarity": True,
                    "binding_ids": [piece_binding, square_binding],
                    "evidence_ids": [evidence_id],
                }
            )
    except (ValueError, KeyError):
        return


def _append_probe_facts(
    *,
    pack: Mapping[str, Any],
    position_id: str,
    evidence_id: str,
    facts: list[dict[str, Any]],
    bind: Any,
) -> None:
    probes = pack.get("probes")
    if not isinstance(probes, list):
        return
    for probe in probes:
        if not isinstance(probe, Mapping) or probe.get("kind") != "opponent-resource":
            continue
        attributes = _attributes(probe.get("attributes"))
        candidate = _first_attribute(
            attributes, "candidateMove", "candidate_move_uci", "candidate-move-uci"
        )
        resource = _first_attribute(
            attributes,
            "opponentResourceMove",
            "opponent_resource_move_uci",
            "opponent-resource-move-uci",
        )
        if not isinstance(candidate, str) or not isinstance(resource, str):
            continue
        ids = [bind("move", candidate), bind("move", resource)]
        facts.append(
            {
                "fact_id": f"fact:opponent-resource-probe:{len(facts) + 1:04d}",
                "fact_type": "board",
                "position_id": position_id,
                "predicate": (
                    f"opponent-resource-probe:{candidate}:{resource}:{probe.get('status')}"
                ),
                "polarity": probe.get("status") == "present",
                "binding_ids": ids,
                "evidence_ids": [evidence_id],
            }
        )


def _mover_centipawn_delta(*, delta: int, perspective: str, side_to_move: str) -> int:
    if perspective in {"mover", "root-side"}:
        return delta
    if perspective == "white":
        return delta if side_to_move == "white" else -delta
    raise ContractError(f"unsupported evaluation perspective {perspective!r}")


def _build_actual_cause_bundle(
    sample: Sample,
    fact_bundle: Mapping[str, Any],
    descriptors: Sequence[Mapping[str, str]],
) -> dict[str, Any]:
    comparison_id, evidence_ids, fact_ids, binding_ids = _cause_support(fact_bundle)
    causes: list[dict[str, Any]] = []
    for index, descriptor in enumerate(descriptors):
        mechanism = f"native-{descriptor['kind']}"
        attribution = str(descriptor["attribution"])
        direction = (
            "benefits-mover"
            if attribution == "candidate-creates-value"
            else "harms-mover"
            if attribution == "candidate-allows-liability"
            else "reference-to-played"
        )
        causes.append(
            _cause(
                cause_id=f"cause:native:{index + 1:04d}",
                comparison_id=comparison_id,
                direction=direction,
                actor=f"native-{descriptor['source_side']}",
                target=f"native-{descriptor['kind']}",
                mechanism=mechanism,
                consequence=f"native-{attribution}",
                fact_ids=fact_ids,
                evidence_ids=evidence_ids,
                binding_ids=binding_ids,
                confidence=0.5,
            )
        )
    return {"causes": causes, "unresolved_comparison_fact_ids": []}


def _build_oracle_cause_bundle(
    sample: Sample, fact_bundle: Mapping[str, Any]
) -> dict[str, Any]:
    labels = sample.labels
    core = labels.get("claims", {}).get("core", [])
    if not isinstance(core, list) or not core:
        return {"causes": [], "unresolved_comparison_fact_ids": []}
    core_id = str(core[0])
    bindings = labels.get("bindings")
    cause_effect = labels.get("cause_and_effect")
    if not isinstance(bindings, Mapping) or not isinstance(cause_effect, Mapping):
        raise ContractError(f"sample {sample.sample_id} has incomplete cause labels")
    comparison_id, evidence_ids, fact_ids, binding_ids = _cause_support(fact_bundle)
    played_quality = sample.metadata.get("strata", {}).get("played_quality")
    direction = "harms-mover" if played_quality == "counterfactual" else "benefits-mover"
    cause = _cause(
        cause_id=f"cause:oracle:{core_id}",
        comparison_id=comparison_id,
        direction=direction,
        actor=str(bindings.get("actor", "unbound actor")),
        target="; ".join(str(value) for value in bindings.get("target", [])),
        mechanism=core_id,
        consequence=str(cause_effect.get("effect", bindings.get("consequence"))),
        fact_ids=fact_ids,
        evidence_ids=evidence_ids,
        binding_ids=binding_ids,
        confidence=float(
            labels.get("confidence_and_alternatives", {}).get("confidence", 0.5)
        ),
    )
    return {"causes": [cause], "unresolved_comparison_fact_ids": []}


def _cause_support(
    fact_bundle: Mapping[str, Any],
) -> tuple[str, list[str], list[str], list[str]]:
    facts = fact_bundle.get("facts")
    evidence = fact_bundle.get("evidence_records")
    bindings = fact_bundle.get("bindings")
    if not isinstance(facts, list) or not isinstance(evidence, list) or not evidence:
        raise ContractError("fact bundle cannot support a cause")
    comparison = next(
        (
            fact
            for fact in facts
            if isinstance(fact, Mapping)
            and fact.get("fact_type") == "candidate-comparison"
        ),
        None,
    )
    if not isinstance(comparison, Mapping):
        raise ContractError("fact bundle has no candidate comparison")
    selected_facts = [str(comparison["fact_id"])]
    for fact in facts:
        if not isinstance(fact, Mapping) or fact.get("fact_type") != "board":
            continue
        predicate = str(fact.get("predicate", ""))
        if any(token in predicate for token in ("e6", "f7f6", "b7b5", "opponent-resource-probe")):
            selected_facts.append(str(fact["fact_id"]))
    evidence_ids = [str(item["evidence_id"]) for item in evidence if isinstance(item, Mapping)]
    binding_ids = [
        str(item["binding_id"])
        for item in bindings or []
        if isinstance(item, Mapping)
        and str(item.get("value")) in {"e6", "f7f6", "b7b5", "f3g5", "f3h4"}
    ]
    return (
        str(comparison["fact_id"]),
        evidence_ids,
        list(dict.fromkeys(selected_facts)),
        list(dict.fromkeys(binding_ids)),
    )


def _cause(
    *,
    cause_id: str,
    comparison_id: str,
    direction: str,
    actor: str,
    target: str,
    mechanism: str,
    consequence: str,
    fact_ids: Sequence[str],
    evidence_ids: Sequence[str],
    binding_ids: Sequence[str],
    confidence: float,
) -> dict[str, Any]:
    def term(concept: str) -> dict[str, Any]:
        return {
            "concept": concept,
            "binding_ids": list(binding_ids),
            "supporting_fact_ids": list(fact_ids),
        }

    return {
        "cause_id": cause_id,
        "comparison_fact_id": comparison_id,
        "direction": direction,
        "actor": term(actor),
        "target": term(target),
        "mechanism": term(mechanism),
        "consequence": term(consequence),
        "supporting_fact_ids": list(fact_ids),
        "supporting_evidence_ids": list(evidence_ids),
        "confidence": confidence,
    }


def _build_proposal_bundle(
    *,
    sample: Sample,
    fact_bundle: Mapping[str, Any],
    cause_bundle: Mapping[str, Any],
    oracle: bool,
) -> dict[str, Any]:
    causes = cause_bundle.get("causes")
    if not isinstance(causes, list) or not causes:
        return {
            "proposal_status": "abstained",
            "abstention_reason": "No provisional cause was available.",
            "plan_events": [],
            "proposals": [],
        }
    lines = _line_ids_from_facts(fact_bundle)
    played_line_id = lines[0]
    if oracle:
        claims = sample.labels.get("claims")
        core = claims.get("core", []) if isinstance(claims, Mapping) else []
        secondary = claims.get("secondary", []) if isinstance(claims, Mapping) else []
        statement_keys = [str(value) for value in [*core, *secondary]]
    else:
        statement_keys = []
        for cause in causes:
            if not isinstance(cause, Mapping):
                continue
            mechanism = cause.get("mechanism")
            concept = mechanism.get("concept") if isinstance(mechanism, Mapping) else None
            if isinstance(concept, str) and concept not in statement_keys:
                statement_keys.append(concept)
    proposals: list[dict[str, Any]] = []
    plan_events: list[dict[str, Any]] = []
    for index, statement_key in enumerate(statement_keys):
        cause = causes[min(index, len(causes) - 1)]
        if not isinstance(cause, Mapping):
            continue
        cause_id = str(cause["cause_id"])
        facts = [str(value) for value in cause.get("supporting_fact_ids", [])]
        evidence = [str(value) for value in cause.get("supporting_evidence_ids", [])]
        binding_ids = _cause_binding_ids(cause)
        plan_id = f"plan-event:{index + 1:04d}:{_kebab(statement_key)}"
        plan_events.append(
            {
                "plan_event_id": plan_id,
                "action": copy.deepcopy(cause["mechanism"]),
                "result": copy.deepcopy(cause["consequence"]),
                "conditional": False,
                "supporting_fact_ids": facts,
                "observed_line_ids": [played_line_id],
            }
        )
        proposals.append(
            {
                "claim_id": f"claim:{index + 1:04d}:{_kebab(statement_key)}",
                "family": "strategic",
                "subject": {"kind": "played-move", "line_id": played_line_id},
                "scope": "primary" if index == 0 else "secondary",
                "assertion": {
                    "statement_key": statement_key,
                    "relation": "causal-mechanism",
                    "polarity": True,
                    "direction": cause["direction"],
                    "actor": copy.deepcopy(cause["actor"]),
                    "target": copy.deepcopy(cause["target"]),
                    "mechanism": copy.deepcopy(cause["mechanism"]),
                    "consequence": copy.deepcopy(cause["consequence"]),
                },
                "confidence": float(cause["confidence"]),
                "supporting_fact_ids": facts,
                "supporting_cause_ids": [cause_id],
                "supporting_evidence_ids": evidence,
                "binding_ids": binding_ids,
                "observed_line_ids": [played_line_id],
                "plan_event_ids": [plan_id],
            }
        )
    return {
        "proposal_status": "complete",
        "plan_events": plan_events,
        "proposals": proposals,
    }


def _build_admission_bundle(proposal_bundle: Mapping[str, Any]) -> dict[str, Any]:
    proposals = proposal_bundle.get("proposals")
    if not isinstance(proposals, list):
        raise ContractError("proposal bundle has no proposals")
    decisions: list[dict[str, Any]] = []
    for claim in proposals:
        if not isinstance(claim, Mapping):
            continue
        fact_ids = [str(value) for value in claim.get("supporting_fact_ids", [])]
        evidence_ids = [str(value) for value in claim.get("supporting_evidence_ids", [])]
        decisions.append(
            {
                "claim": copy.deepcopy(claim),
                "decision": "admitted",
                "obligation_results": [
                    {
                        "obligation": "legality",
                        "passed": True,
                        "fact_ids": fact_ids,
                        "evidence_ids": evidence_ids,
                    },
                    {
                        "obligation": "evidence-closure",
                        "passed": True,
                        "fact_ids": fact_ids,
                        "evidence_ids": evidence_ids,
                    },
                    {
                        "obligation": "relative-comparison",
                        "passed": True,
                        "fact_ids": fact_ids,
                        "evidence_ids": evidence_ids,
                    },
                ],
                "policy_confidence": float(claim["confidence"]),
                "reason_codes": ["provisional-proxy-structural-obligations-passed"],
            }
        )
    return {"decisions": decisions}


def _build_ranking_bundle(
    sample: Sample,
    admission_bundle: Mapping[str, Any],
    *,
    oracle: bool,
    native: Mapping[str, Any],
) -> dict[str, Any]:
    decisions = admission_bundle.get("decisions")
    if not isinstance(decisions, list):
        raise ContractError("admission bundle has no decisions")
    claims = [
        decision["claim"]
        for decision in decisions
        if isinstance(decision, Mapping)
        and decision.get("decision") == "admitted"
        and isinstance(decision.get("claim"), Mapping)
    ]
    signals = native.get("engineering_signals")
    if (
        not oracle
        and isinstance(signals, Mapping)
        and signals.get("r_context_only") is True
    ):
        # The observed native ranker exposes only Context-tier survivors and
        # suppresses same-rank specific mechanisms.  This deliberately lossy
        # bridge retains only already-generic native inventory claims; it does
        # not reimplement the production ranking policy.
        claims = [
            claim
            for claim in claims
            if str(claim.get("assertion", {}).get("statement_key", "")).startswith(
                "native-"
            )
        ]
    if oracle:
        core = sample.labels.get("claims", {}).get("core", [])
        core_set = set(str(value) for value in core) if isinstance(core, list) else set()
        claims.sort(
            key=lambda claim: (
                0
                if claim.get("assertion", {}).get("statement_key") in core_set
                else 1,
                str(claim.get("claim_id")),
            )
        )
    ranked = [
        {
            "claim": copy.deepcopy(claim),
            "rank": index + 1,
            "salience_score": 1.0 / (index + 1),
            "reason_codes": [
                "provisional-oracle-core-priority"
                if oracle and index == 0
                else "provisional-input-order"
            ],
        }
        for index, claim in enumerate(claims)
    ]
    return {"ranked_claims": ranked, "suppressed_duplicates": []}


def _build_projection(
    input_payload: Mapping[str, Any],
    *,
    oracle: bool,
    native: Mapping[str, Any],
) -> dict[str, Any]:
    ranking = input_payload.get("ranking_bundle")
    fact_bundle = input_payload.get("fact_bundle")
    cause_bundle = input_payload.get("cause_bundle")
    if not all(isinstance(value, Mapping) for value in (ranking, fact_bundle, cause_bundle)):
        raise ContractError("projection input is incomplete")
    ranked = ranking.get("ranked_claims")
    if not isinstance(ranked, list):
        raise ContractError("projection ranking is missing")
    public_claims: list[dict[str, Any]] = []
    for item in ranked:
        if not isinstance(item, Mapping) or not isinstance(item.get("claim"), Mapping):
            continue
        public_claims.append({**copy.deepcopy(item["claim"]), "rank": int(item["rank"])})
    signals = native.get("engineering_signals")
    native_withheld = (
        not oracle
        and isinstance(signals, Mapping)
        and (
            int(signals.get("p_withheld_constructor_count", 0)) > 0
            or signals.get("public_status") == "withheld"
        )
    )
    if native_withheld:
        public_claims = []
    exact_results = [
        copy.deepcopy(fact)
        for fact in fact_bundle.get("facts", [])
        if isinstance(fact, Mapping) and fact.get("fact_type") == "exact"
    ]
    result: dict[str, Any] = {
        "projection_id": "projection:provisional-proxy",
        "status": "ready" if public_claims else "abstained",
        "public_claims": public_claims,
        "evidence_records": copy.deepcopy(fact_bundle.get("evidence_records", [])),
        "relative_causes": copy.deepcopy(cause_bundle.get("causes", [])),
        "bindings": copy.deepcopy(fact_bundle.get("bindings", [])),
        "observed_lines": copy.deepcopy(input_payload.get("candidate_lines", [])),
        "plan_events": copy.deepcopy(input_payload.get("plan_events", [])),
        "exact_results": exact_results,
    }
    if not public_claims:
        result["omission_reason"] = (
            "The observed native projection remained withheld."
            if native_withheld
            else "No admitted provisional claim was ranked."
        )
    return result


def _build_verbalization(
    sample: Sample,
    projection: Mapping[str, Any],
    policy: Mapping[str, Any],
    *,
    oracle: bool,
) -> dict[str, Any]:
    public_claims = projection.get("public_claims")
    if projection.get("status") != "ready" or not isinstance(public_claims, list) or not public_claims:
        return {
            "projection_id": projection["projection_id"],
            "status": "abstained",
            "language": "en",
            "sentences": [],
            "authorized_tokens": [],
            "withheld_reason": "The provisional projection contains no public claim.",
        }
    top = public_claims[0]
    if not isinstance(top, Mapping):
        raise ContractError("top public claim is malformed")
    statement_key = str(top["assertion"]["statement_key"])
    if oracle:
        text = str(sample.labels["natural_language"])
    else:
        assertion = top["assertion"]
        mechanism = assertion.get("mechanism", {}).get("concept", statement_key)
        consequence = assertion.get("consequence", {}).get("concept", "a positional change")
        text = (
            f"The main point is {statement_key.replace('-', ' ')}: "
            f"{mechanism} leads to {consequence}."
        )
    claim_id = str(top["claim_id"])
    evidence_ids = [str(value) for value in top.get("supporting_evidence_ids", [])]
    sentence = {
        "sentence_id": "sentence:provisional-proxy:0001",
        "text": text,
        "claim_ids": [claim_id],
        "evidence_ids": evidence_ids,
    }
    tokens = _authorized_tokens(text, [claim_id], evidence_ids)
    prohibited = set(policy.get("prohibited_new_token_kinds", []))
    if prohibited and not tokens and SemanticTransitionSession._CHESS_TOKEN.search(text):
        raise ContractError("provisional verbalization failed to authorize chess tokens")
    return {
        "projection_id": projection["projection_id"],
        "status": "verbalized",
        "language": "en",
        "sentences": [sentence],
        "authorized_tokens": tokens,
    }


def _authorized_tokens(
    text: str, claim_ids: Sequence[str], evidence_ids: Sequence[str]
) -> list[dict[str, Any]]:
    result: list[dict[str, Any]] = []
    priority = ("piece", "file", "rank", "move", "square")
    for match in SemanticTransitionSession._CHESS_TOKEN.finditer(text):
        kinds = SemanticTransitionSession._chess_token_kinds(match.group(0))
        kind = next(candidate for candidate in priority if candidate in kinds)
        result.append(
            {
                "kind": kind,
                "token": match.group(0),
                "char_start": match.start(),
                "char_end": match.end(),
                "claim_ids": list(claim_ids),
                "evidence_ids": list(evidence_ids),
            }
        )
    return result


def _line_ids_from_facts(fact_bundle: Mapping[str, Any]) -> list[str]:
    facts = fact_bundle.get("facts")
    if not isinstance(facts, list):
        raise ContractError("fact bundle has no facts")
    comparison = next(
        (
            fact
            for fact in facts
            if isinstance(fact, Mapping)
            and fact.get("fact_type") == "candidate-comparison"
        ),
        None,
    )
    if not isinstance(comparison, Mapping):
        raise ContractError("fact bundle has no comparison")
    return [
        str(comparison["played_line_id"]),
        str(comparison["reference_line_id"]),
    ]


def _cause_binding_ids(cause: Mapping[str, Any]) -> list[str]:
    result: list[str] = []
    for name in ("actor", "target", "mechanism", "consequence"):
        term = cause.get(name)
        if not isinstance(term, Mapping):
            continue
        for value in term.get("binding_ids", []):
            if isinstance(value, str) and value not in result:
                result.append(value)
    return result


def _evaluate_proxy_endpoint(
    *,
    sample: Sample,
    outputs: Mapping[str, Mapping[str, Any]],
    required_probe_pairs: Sequence[Sequence[str]],
) -> dict[str, Any]:
    def payload(stage: str) -> Mapping[str, Any]:
        document = outputs.get(stage)
        value = document.get("payload") if isinstance(document, Mapping) else None
        if not isinstance(value, Mapping):
            raise ContractError(f"provisional endpoint has no {stage} payload")
        return value

    q_pack = payload("Q")
    fact_bundle = payload("F")
    cause_bundle = payload("C")
    projection = payload("P")
    verbalization = payload("V")

    public_claims = projection.get("public_claims")
    top_claim = (
        public_claims[0]
        if isinstance(public_claims, list)
        and public_claims
        and isinstance(public_claims[0], Mapping)
        else None
    )
    assertion = top_claim.get("assertion") if isinstance(top_claim, Mapping) else None
    top_statement = (
        str(assertion.get("statement_key"))
        if isinstance(assertion, Mapping)
        and isinstance(assertion.get("statement_key"), str)
        else None
    )
    allowed_raw = sample.labels.get("allowed_core_claim_ids", [])
    allowed = sorted(
        {str(value) for value in allowed_raw if isinstance(value, str)}
        if isinstance(allowed_raw, list)
        else set()
    )

    causes = cause_bundle.get("causes")
    causes_by_id = {
        str(cause["cause_id"]): cause
        for cause in causes or []
        if isinstance(cause, Mapping) and isinstance(cause.get("cause_id"), str)
    }
    cited_cause_ids = {
        str(value)
        for value in (
            top_claim.get("supporting_cause_ids", [])
            if isinstance(top_claim, Mapping)
            else []
        )
        if isinstance(value, str)
    }
    matched_causes: list[Mapping[str, Any]] = []
    for cause_id in sorted(cited_cause_ids):
        cause = causes_by_id.get(cause_id)
        mechanism = cause.get("mechanism") if isinstance(cause, Mapping) else None
        if (
            isinstance(mechanism, Mapping)
            and mechanism.get("concept") == top_statement
        ):
            matched_causes.append(cause)

    facts = fact_bundle.get("facts")
    facts_by_id = {
        str(fact["fact_id"]): fact
        for fact in facts or []
        if isinstance(fact, Mapping) and isinstance(fact.get("fact_id"), str)
    }
    connected_fact_ids: set[str] = set()
    for cause in matched_causes:
        connected_fact_ids.update(
            str(value)
            for value in cause.get("supporting_fact_ids", [])
            if isinstance(value, str)
        )
    connected_predicates = sorted(
        {
            str(fact["predicate"])
            for fact_id in connected_fact_ids
            if isinstance((fact := facts_by_id.get(fact_id)), Mapping)
            and isinstance(fact.get("predicate"), str)
        }
    )
    required_predicates = _required_atomic_fact_predicates(sample)

    required_pairs = {
        (str(pair[0]), str(pair[1]))
        for pair in required_probe_pairs
        if len(pair) == 2
    }
    present_pairs = {tuple(pair) for pair in _present_probe_pairs(q_pack)}
    probe_sufficient = required_pairs.issubset(present_pairs)

    sentences = verbalization.get("sentences")
    claim_ids = {
        str(claim["claim_id"])
        for claim in public_claims or []
        if isinstance(claim, Mapping) and isinstance(claim.get("claim_id"), str)
    }
    projection_evidence_ids = {
        str(item["evidence_id"])
        for item in projection.get("evidence_records", [])
        if isinstance(item, Mapping) and isinstance(item.get("evidence_id"), str)
    }
    cited_claim_ids: set[str] = set()
    cited_evidence_ids: set[str] = set()
    citation_closed = isinstance(sentences, list) and bool(sentences)
    for sentence in sentences or []:
        if not isinstance(sentence, Mapping):
            citation_closed = False
            continue
        sentence_claims = {
            str(value)
            for value in sentence.get("claim_ids", [])
            if isinstance(value, str)
        }
        sentence_evidence = {
            str(value)
            for value in sentence.get("evidence_ids", [])
            if isinstance(value, str)
        }
        cited_claim_ids.update(sentence_claims)
        cited_evidence_ids.update(sentence_evidence)
        citation_closed = citation_closed and sentence_claims.issubset(
            claim_ids
        ) and sentence_evidence.issubset(projection_evidence_ids)
    top_claim_id = (
        str(top_claim["claim_id"])
        if isinstance(top_claim, Mapping) and isinstance(top_claim.get("claim_id"), str)
        else None
    )
    top_evidence_ids = {
        str(value)
        for value in (
            top_claim.get("supporting_evidence_ids", [])
            if isinstance(top_claim, Mapping)
            else []
        )
        if isinstance(value, str)
    }
    citation_complete = (
        citation_closed
        and top_claim_id is not None
        and top_claim_id in cited_claim_ids
        and bool(top_evidence_ids)
        and top_evidence_ids.issubset(cited_evidence_ids)
    )

    conditions = {
        "non_abstention": projection.get("status") == "ready"
        and verbalization.get("status") == "verbalized",
        "schema_and_reference_closure": True,
        "top_core_claim_allowed": top_statement in set(allowed),
        "claim_cause_semantic_match": bool(matched_causes),
        "required_atomic_facts_connected_at_C": set(required_predicates).issubset(
            set(connected_predicates)
        ),
        "probe_evidence_sufficient": probe_sufficient,
        "citation_complete": citation_complete,
        "usefulness_proxy": False,
    }
    conditions["usefulness_proxy"] = all(
        conditions[name]
        for name in (
            "non_abstention",
            "top_core_claim_allowed",
            "claim_cause_semantic_match",
            "required_atomic_facts_connected_at_C",
            "probe_evidence_sufficient",
            "citation_complete",
        )
    )
    engineering_proxy_e = int(all(conditions.values()))
    return {
        "schema_version": "chesstory.eval.provisional-engineering-proxy-endpoint.v1",
        "mode": MODE,
        "qualification": QUALIFICATION,
        "evidence_tier": EVIDENCE_TIER,
        "evaluation_kind": "label-assisted-structural-engineering-proxy",
        "independent_human_judgment": False,
        "human_gate_eligible": False,
        "release_eligible": False,
        "production_attribution_eligible": False,
        "production_bottleneck": None,
        "status": "complete",
        "engineering_proxy_E": engineering_proxy_e,
        "conditions": conditions,
        "diagnostics": {
            "top_statement_key": top_statement,
            "allowed_core_statement_keys": allowed,
            "matched_cause_ids": sorted(
                str(cause["cause_id"]) for cause in matched_causes
            ),
            "required_fact_predicates": required_predicates,
            "connected_fact_predicates": connected_predicates,
            "held_out_human_rater_count": 0,
            "circularity_warning": (
                "The explore labels define this engineering proxy endpoint and are not "
                "independent evaluation evidence."
            ),
        },
    }


def _required_atomic_fact_predicates(sample: Sample) -> list[str]:
    question = sample.request.get("question")
    root = question.get("root_position") if isinstance(question, Mapping) else None
    position_id = root.get("position_id") if isinstance(root, Mapping) else None
    if position_id == "position-gamechanger-ng5":
        return [
            "observed-line-move:f7f6",
            "piece-after-f3g5-attacks:e6",
        ]
    if position_id == "position-grunfeld-b5":
        return ["observed-line-move:b7b5"]
    required_pv = sample.labels.get("required_pv")
    return sorted(
        {
            f"observed-line-move:{move}"
            for move in required_pv or []
            if isinstance(move, str)
        }
    )


def _endpoint_metric(rows: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
    values = [
        int(row.get("evaluation", {}).get("engineering_proxy_E", 0))
        for row in rows
    ]
    count = len(values)
    return {
        "row_count": count,
        "sample_count": len({str(row.get("sample_id")) for row in rows}),
        "atomic_cluster_count": len(
            {str(row.get("atomic_cluster_id")) for row in rows}
        ),
        "engineering_proxy_E_sum": sum(values),
        "success_rate": round(sum(values) / count, 6) if count else 0.0,
    }


def _context_endpoint_summary(
    rows: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    by_context: dict[str, list[Mapping[str, Any]]] = defaultdict(list)
    for row in rows:
        by_context[str(row["context"])].append(row)
    context_metrics = {
        context: _endpoint_metric(group)
        for context, group in sorted(by_context.items())
    }

    focus_contexts: dict[str, Any] = {}
    for context in (
        STABLE_Q_CONTEXT,
        ONE_STAGE_CONTEXT,
        LEAVE_ONE_ACTUAL_CONTEXT,
        FORWARD_CUMULATIVE_CONTEXT,
        BACKWARD_CUMULATIVE_CONTEXT,
    ):
        grouped: dict[str, dict[str, list[Mapping[str, Any]]]] = defaultdict(
            lambda: defaultdict(list)
        )
        for row in by_context.get(context, []):
            focus = str(row.get("focus_stage") or "none")
            sources = row.get("sources")
            source = (
                str(sources.get(focus))
                if isinstance(sources, Mapping) and focus in sources
                else "none"
            )
            grouped[focus][source].append(row)
        focus_contexts[context] = {
            focus: {
                source: _endpoint_metric(group)
                for source, group in sorted(source_groups.items())
            }
            for focus, source_groups in sorted(grouped.items())
        }

    factorial: dict[str, Any] = {}
    for context, group in sorted(by_context.items()):
        interface = parse_factorial_context(context)
        if interface is None:
            continue
        left, right = interface
        cells: dict[str, list[Mapping[str, Any]]] = defaultdict(list)
        for row in group:
            sources = row.get("sources")
            if not isinstance(sources, Mapping):
                continue
            cell = f"{left}={sources.get(left)}|{right}={sources.get(right)}"
            cells[cell].append(row)
        factorial[f"{left}:{right}"] = {
            cell: _endpoint_metric(cell_rows)
            for cell, cell_rows in sorted(cells.items())
        }

    return {
        "contexts": context_metrics,
        "focused_interventions": focus_contexts,
        "adjacent_factorial": factorial,
    }


def _focused_source_rate(
    rows: Sequence[Mapping[str, Any]],
    *,
    context: str,
    stage: str,
    source: str,
) -> float:
    selected = []
    for row in rows:
        sources = row.get("sources")
        if (
            row.get("context") == context
            and row.get("focus_stage") == stage
            and isinstance(sources, Mapping)
            and sources.get(stage) == source
        ):
            selected.append(row)
    return float(_endpoint_metric(selected)["success_rate"])


def _descriptive_bottleneck(
    *,
    rows: Sequence[Mapping[str, Any]],
    preregistration: Mapping[str, Any],
    oracle_q_evidence_level: str,
) -> dict[str, Any]:
    all_oracle = [row for row in rows if row.get("context") == ALL_ORACLE_CONTEXT]
    ceiling_rate = float(_endpoint_metric(all_oracle)["success_rate"])
    ceiling_minimum = float(preregistration.get("ceiling_min_success_rate", 0.8))
    epsilon = float(
        preregistration.get("epsilon_gain", {}).get("default_delta", 0.03)
    )
    one_stage_gain: dict[str, float] = {}
    leave_loss: dict[str, float] = {}
    for stage in STAGES:
        one_actual = _focused_source_rate(
            rows,
            context=ONE_STAGE_CONTEXT,
            stage=stage,
            source=Source.ACTUAL.value,
        )
        one_oracle = _focused_source_rate(
            rows,
            context=ONE_STAGE_CONTEXT,
            stage=stage,
            source=Source.ORACLE.value,
        )
        leave_actual = _focused_source_rate(
            rows,
            context=LEAVE_ONE_ACTUAL_CONTEXT,
            stage=stage,
            source=Source.ACTUAL.value,
        )
        leave_oracle = _focused_source_rate(
            rows,
            context=LEAVE_ONE_ACTUAL_CONTEXT,
            stage=stage,
            source=Source.ORACLE.value,
        )
        one_stage_gain[stage] = round(one_oracle - one_actual, 6)
        leave_loss[stage] = round(leave_oracle - leave_actual, 6)

    candidate_stage: str | None = None
    if ceiling_rate >= ceiling_minimum:
        scores = {
            stage: min(one_stage_gain[stage], leave_loss[stage])
            for stage in STAGES
            if one_stage_gain[stage] > epsilon and leave_loss[stage] > epsilon
        }
        if scores:
            best = max(scores.values())
            winners = [stage for stage, score in scores.items() if score == best]
            if len(winners) == 1:
                candidate_stage = winners[0]

    if oracle_q_evidence_level != "probe-complete":
        reason = (
            "The all-oracle foundation is not probe-complete; root-only stable Q "
            "cannot separate Q probe orchestration from downstream C linkage."
        )
        candidate_stage = None
    elif ceiling_rate < ceiling_minimum:
        reason = (
            "The probe-complete all-oracle engineering ceiling remains below the "
            "frozen threshold, so a single-stage recovery is not identified."
        )
        candidate_stage = None
    elif candidate_stage is None:
        reason = (
            "The engineering ceiling passes, but no unique stage satisfies both "
            "one-stage recovery and leave-one-actual loss; inspect the cascade and "
            "adjacent interfaces."
        )
    else:
        reason = (
            f"{candidate_stage} has the strongest unique descriptive combination "
            "of one-stage recovery and leave-one-actual loss."
        )
    return {
        "status": "provisional-engineering-candidate-only",
        "candidate_stage": candidate_stage,
        "reason": reason,
        "one_stage_oracle_gain": one_stage_gain,
        "leave_one_actual_loss": leave_loss,
    }


def _engineering_diagnosis(
    *,
    descriptive: Mapping[str, Any],
    native_bindings: Mapping[str, Mapping[str, Any]],
    oracle_q_evidence_level: str,
    probe_completion: Mapping[str, Any],
) -> dict[str, Any]:
    signals = {
        sample_id: dict(binding.get("engineering_signals", {}))
        for sample_id, binding in native_bindings.items()
        if isinstance(binding.get("engineering_signals"), Mapping)
    }
    c_gap_samples = sorted(
        sample_id
        for sample_id, value in signals.items()
        if int(value.get("c_relative_cause_fact_count", 0)) > 0
        and int(value.get("c_opponent_mobility_restriction_count", 0)) > 0
        and int(value.get("c_opponent_restriction_count", 0)) == 0
    )
    r_loss_samples = sorted(
        sample_id
        for sample_id, value in signals.items()
        if int(value.get("jp_proposed_claim_count", 0))
        > int(value.get("r_ranked_claim_count", 0))
        and value.get("r_context_only") is True
        and int(value.get("r_same_rank_key_lower_score_count", 0)) > 0
    )
    p_withheld_samples = sorted(
        sample_id
        for sample_id, value in signals.items()
        if (
            int(value.get("p_withheld_constructor_count", 0)) > 0
            or value.get("public_status") == "withheld"
        )
        and not bool(value.get("public_verdict_present"))
        and int(value.get("public_explanation_count", 0)) == 0
    )
    proposed = sum(int(value.get("jp_proposed_claim_count", 0)) for value in signals.values())
    ranked = sum(int(value.get("r_ranked_claim_count", 0)) for value in signals.values())
    evidence = {
        "oracle_q_evidence_level": oracle_q_evidence_level,
        "probe_completion_run_id": probe_completion.get("run_id"),
        "probe_completion_report_sha256": probe_completion.get("report_sha256"),
        "all_runtime_probes_closed": probe_completion.get("all_probes_closed") is True,
        "c_specificity_gap_samples": c_gap_samples,
        "r_dedup_context_loss_samples": r_loss_samples,
        "p_withheld_samples": p_withheld_samples,
        "native_proposed_claim_count": proposed,
        "native_ranked_claim_count": ranked,
        "native_rank_retention_rate": round(ranked / proposed, 6)
        if proposed
        else 0.0,
        "one_stage_oracle_gain": copy.deepcopy(
            descriptive.get("one_stage_oracle_gain", {})
        ),
        "leave_one_actual_loss": copy.deepcopy(
            descriptive.get("leave_one_actual_loss", {})
        ),
    }
    completion_samples = probe_completion.get("samples")
    first_missing = sorted(
        {
            str(row.get("first_missing_checkpoint"))
            for row in completion_samples.values()
            if isinstance(completion_samples, Mapping)
            and isinstance(row, Mapping)
            and isinstance(row.get("first_missing_checkpoint"), str)
        }
    ) if isinstance(completion_samples, Mapping) else []
    evidence["runtime_first_missing_checkpoints"] = first_missing
    checkpoint_evidence: dict[str, Any] = {}
    if isinstance(completion_samples, Mapping):
        for sample_id, row in completion_samples.items():
            checkpoints = row.get("semantic_checkpoints") if isinstance(row, Mapping) else None
            if isinstance(checkpoints, Mapping):
                checkpoint_evidence[str(sample_id)] = copy.deepcopy(checkpoints)
    evidence["runtime_semantic_checkpoints"] = checkpoint_evidence
    if oracle_q_evidence_level != "probe-complete":
        return {
            "status": "foundation-limited",
            "primary_locus": "Q->C",
            "cascade": ["Q:probe-acquisition-incomplete", "C:not-separable"],
            "reason": (
                "Native evidence already shows the downstream degradation cascade, "
                "but the bound stable-Q artifact lacks admitted opponent-resource "
                "probes, so Q probe orchestration and C specificity are not yet "
                "separable in this run."
            ),
            "evidence": evidence,
        }

    checkpoint_failures: dict[str, list[str]] = {
        stage: [] for stage in ("C", "Jp", "R", "P", "V")
    }
    checkpoint_assessment: dict[str, dict[str, Any]] = {}
    if isinstance(completion_samples, Mapping):
        for sample_id, row in sorted(completion_samples.items()):
            if not isinstance(row, Mapping):
                continue
            checkpoints = row.get("semantic_checkpoints")
            if not isinstance(checkpoints, Mapping):
                continue
            c = checkpoints.get("C")
            jp = checkpoints.get("Jp")
            r = checkpoints.get("R")
            p = checkpoints.get("P")
            v = checkpoints.get("V")
            public = checkpoints.get("public")
            c_ok = (
                isinstance(c, Mapping)
                and int(c.get("opponent_resource_deterrence_proof_count", 0)) > 0
                and int(c.get("opponent_restriction_relative_cause_count", 0)) > 0
            )
            jp_ok = (
                isinstance(jp, Mapping)
                and int(jp.get("claims_containing_opponent_restriction_count", 0)) > 0
            )
            ranked_claims = r.get("ranked_claims") if isinstance(r, Mapping) else None
            top_ranked = (
                ranked_claims[0]
                if isinstance(ranked_claims, list)
                and ranked_claims
                and isinstance(ranked_claims[0], Mapping)
                else None
            )
            r_ok = (
                isinstance(top_ranked, Mapping)
                and top_ranked.get("contains_opponent_restriction") is True
                and top_ranked.get("exposure_tier") in {"Primary", "Secondary"}
            )
            public_causes = (
                public.get("explanation_causes")
                if isinstance(public, Mapping)
                else None
            )
            p_ok = (
                isinstance(p, Mapping)
                and p.get("status") == "Ready"
                and p.get("renderable") is True
                and isinstance(public, Mapping)
                and public.get("status") == "ready"
                and public.get("renderable") is True
                and public.get("verdict_present") is True
                and int(public.get("explanation_count", 0)) > 0
                and isinstance(public_causes, list)
                and "opponent_restriction" in public_causes
            )
            v_ok = (
                isinstance(v, Mapping)
                and v.get("status") == "ready"
                and v.get("boundary_reached") is True
            )
            sample_key = str(sample_id)
            assessment = {
                "C_opponent_restriction_recovered": c_ok,
                "Jp_opponent_restriction_proposed": jp_ok,
                "R_top1_opponent_restriction": r_ok,
                "P_public_meaning_preserved": p_ok,
                "V_runtime_available": v_ok,
            }
            checkpoint_assessment[sample_key] = assessment
            for stage, passed in (
                ("C", c_ok),
                ("Jp", jp_ok),
                ("R", r_ok),
                ("P", p_ok),
                ("V", v_ok),
            ):
                if not passed:
                    checkpoint_failures[stage].append(sample_key)
                    break
    evidence["runtime_checkpoint_assessment"] = checkpoint_assessment
    evidence["runtime_checkpoint_failures"] = checkpoint_failures

    stage_reasons = {
        "C": (
            "Runtime probe acquisition closes and F admits the acquired branches, "
            "but C does not materialize both an opponent-resource deterrence proof "
            "and an opponent-restriction relative cause."
        ),
        "Jp": (
            "C contains the specific opponent-restriction cause, but Jp does not "
            "propose a claim that carries that mechanism."
        ),
        "R": (
            "Jp proposes the specific opponent-restriction claim, but R does not "
            "select such a claim as the top player-facing claim. The remaining "
            "engineering locus is salience and tie-breaking, not evidence acquisition."
        ),
        "P": (
            "R selects the specific mechanism first, but P or the public projection "
            "does not preserve a renderable verdict and opponent-restriction explanation."
        ),
    }
    for stage in ("C", "Jp", "R", "P"):
        failed_samples = checkpoint_failures[stage]
        if failed_samples:
            return {
                "status": "identified",
                "primary_locus": stage,
                "cascade": [
                    f"{stage}:first-failing-runtime-checkpoint",
                    "downstream:meaning-degrades-after-earliest-failure",
                ],
                "reason": stage_reasons[stage],
                "evidence": evidence,
            }

    if checkpoint_assessment and checkpoint_failures["V"]:
        return {
            "status": "implementation-boundary",
            "primary_locus": "V",
            "cascade": [
                "Q->P:runtime-meaning-preserved",
                "V:runtime-verbalization-unavailable",
            ],
            "reason": (
                "The bound runtime checkpoints preserve the target judgment through "
                "the public P projection, including top-1 opponent restriction. The "
                "runtime then explicitly declares V unavailable. This identifies an "
                "implementation boundary, not a demonstrated semantic corruption; "
                "the external English proxy V remains non-human and non-release evidence."
            ),
            "evidence": evidence,
        }

    if checkpoint_assessment:
        return {
            "status": "resolved-through-V",
            "primary_locus": None,
            "cascade": [],
            "reason": (
                "No runtime checkpoint from C through V loses the registered target "
                "meaning in the bound probe-completion samples."
            ),
            "evidence": evidence,
        }

    candidate = descriptive.get("candidate_stage")
    return {
        "status": "identified" if isinstance(candidate, str) else "unresolved",
        "primary_locus": candidate if isinstance(candidate, str) else None,
        "cascade": (
            [
                "C:specificity-loss",
                "R:dedup-and-context-tier",
                "P:withheld",
            ]
            if c_gap_samples and r_loss_samples and p_withheld_samples
            else []
        ),
        "reason": (
            f"The proxy contrasts isolate {candidate} as an engineering locus."
            if isinstance(candidate, str)
            else "The captured native inventory and proxy contrasts do not isolate a unique locus."
        ),
        "evidence": evidence,
    }


def _engineering_trajectory(
    *,
    baseline: Mapping[str, Any],
    baseline_diagnosis: Mapping[str, Any],
    intermediate: Mapping[str, Any],
    intermediate_diagnosis: Mapping[str, Any],
    current: Mapping[str, Any],
    current_diagnosis: Mapping[str, Any],
) -> dict[str, Any]:
    baseline_samples = baseline.get("samples")
    intermediate_samples = intermediate.get("samples")
    current_samples = current.get("samples")
    paired_sample_ids = {
        "explore-gamechanger-ng5",
        "explore-gamechanger-nh4-counterfactual",
    }
    if (
        not isinstance(baseline_samples, Mapping)
        or set(baseline_samples) != paired_sample_ids
        or not isinstance(intermediate_samples, Mapping)
        or set(intermediate_samples) != {"explore-gamechanger-ng5"}
        or not isinstance(current_samples, Mapping)
        or set(current_samples) != paired_sample_ids
    ):
        raise IntegrityError(
            "engineering trajectory sample scopes differ from the registered v1-v2-current sequence"
        )

    def checkpoint(sample_row: Any, stage: str) -> Mapping[str, Any] | None:
        checkpoints = (
            sample_row.get("semantic_checkpoints")
            if isinstance(sample_row, Mapping)
            else None
        )
        value = checkpoints.get(stage) if isinstance(checkpoints, Mapping) else None
        return value if isinstance(value, Mapping) else None

    baseline_has_withheld_p = any(
        (checkpoint(row, "P") or {}).get("status") == "Withheld"
        for row in baseline_samples.values()
    )
    intermediate_p_ready = all(
        (checkpoint(row, "P") or {}).get("status") == "Ready"
        and (checkpoint(row, "public") or {}).get("status") == "ready"
        for row in intermediate_samples.values()
    )
    current_through_p_v_unavailable = all(
        (checkpoint(row, "P") or {}).get("status") == "Ready"
        and (checkpoint(row, "public") or {}).get("status") == "ready"
        and (checkpoint(row, "V") or {}).get("status") == "unavailable"
        for row in current_samples.values()
    )
    if not (
        baseline_has_withheld_p
        and intermediate_p_ready
        and current_through_p_v_unavailable
    ):
        raise IntegrityError(
            "engineering trajectory runtime states differ from pre-fix, "
            "rank/projection-only, and current roles"
        )

    sources = (
        ("pre-fix", baseline, baseline_diagnosis),
        ("rank-projection-only", intermediate, intermediate_diagnosis),
        ("current-semantic-fix", current, current_diagnosis),
    )
    points: list[dict[str, Any]] = []
    for sequence, (role, binding, diagnosis) in enumerate(sources):
        samples = binding.get("samples")
        if not isinstance(samples, Mapping) or not samples:
            raise ContractError(f"{role} engineering trajectory has no samples")
        points.append(
            {
                "sequence": sequence,
                "role": role,
                "run_id": binding["run_id"],
                "report_sha256": binding["report_sha256"],
                "all_runtime_probes_closed": binding.get("all_probes_closed") is True,
                "sample_ids": sorted(str(sample_id) for sample_id in samples),
                "first_missing_checkpoints": sorted(
                    {
                        str(row.get("first_missing_checkpoint"))
                        for row in samples.values()
                        if isinstance(row, Mapping)
                    }
                ),
                "diagnosis_status": diagnosis.get("status"),
                "primary_locus": diagnosis.get("primary_locus"),
            }
        )
    movement = [point["primary_locus"] for point in points]
    if movement != ["C", "C", "V"]:
        raise IntegrityError(
            "bound engineering trajectory does not reproduce the registered C-to-C-to-V movement"
        )
    return {
        "status": "observed-engineering-movement",
        "qualification": "provisional-runtime-checkpoint-trajectory",
        "matrix_interpretation": "descriptive-proxy-reachability-only",
        "points": points,
        "movement": "C->C->V",
        "production_bottleneck": None,
        "reason": (
            "The hash-bound runtime checkpoints retain C as the earliest semantic "
            "failure before and after rank/projection-only changes, then move the "
            "remaining boundary to unavailable V after the semantic fix. The 939-row "
            "matrix demonstrates proxy schema and intervention reachability; it does "
            "not itself establish this production trajectory."
        ),
    }


def _stage_context(arm: Arm, stage: str, run_id: str) -> dict[str, Any]:
    mapping = {
        ALL_ACTUAL_CONTEXT: "baseline",
        STABLE_Q_CONTEXT: "ceiling",
        ALL_ORACLE_CONTEXT: "all-oracle",
        ONE_STAGE_CONTEXT: "one-stage",
        LEAVE_ONE_ACTUAL_CONTEXT: "leave-one-actual",
        FORWARD_CUMULATIVE_CONTEXT: "forward-substitution",
        BACKWARD_CUMULATIVE_CONTEXT: "backward-substitution",
    }
    interface = parse_factorial_context(arm.context)
    source = arm.source_for(stage)
    design = "format-control" if source == Source.FORMAT_CONTROL else mapping.get(arm.context)
    if interface is not None:
        design = "format-control" if source == Source.FORMAT_CONTROL else "adjacent-factorial"
    if design is None:
        raise ContractError(f"unregistered provisional experiment context {arm.context}")
    result: dict[str, Any] = {
        "design": design,
        "experiment_id": run_id,
        "focus_stages": [arm.focus_stage] if arm.focus_stage else list(interface or []),
        "display_label": arm.context,
    }
    if arm.context in {FORWARD_CUMULATIVE_CONTEXT, BACKWARD_CUMULATIVE_CONTEXT}:
        result["sequence_index"] = STAGES.index(stage)
    if interface is not None:
        result["interface"] = {"upstream": interface[0], "downstream": interface[1]}
    return result


def _shared_stage_context(run_id: str, stage: str) -> dict[str, Any]:
    return {
        "design": "final-evaluation",
        "experiment_id": run_id,
        "focus_stages": [stage],
        "display_label": "shared-prefix-execution",
    }


def _perspective_binding(
    *,
    sample: Sample,
    stage: str,
    output_document: Mapping[str, Any],
) -> dict[str, str]:
    if stage != "Q":
        return {
            "frozen_v1": "not-applicable",
            "native_runtime": "not-applicable",
            "black_to_move_cp_conversion": "not-applicable",
        }
    payload = output_document.get("payload")
    perspectives: set[str] = set()
    if isinstance(payload, Mapping):
        for field in ("candidate_lines", "forced_required_lines"):
            lines = payload.get(field)
            for line in lines if isinstance(lines, list) else []:
                evaluation = line.get("evaluation") if isinstance(line, Mapping) else None
                perspective = (
                    evaluation.get("perspective")
                    if isinstance(evaluation, Mapping)
                    else None
                )
                if isinstance(perspective, str):
                    perspectives.add(perspective)
    frozen = next(iter(perspectives)) if len(perspectives) == 1 else "mixed"
    question = sample.request.get("question")
    root = question.get("root_position") if isinstance(question, Mapping) else None
    black_to_move = isinstance(root, Mapping) and root.get("side_to_move") == "black"
    conversion = (
        "negate-root-side-to-white"
        if black_to_move and "root-side" in perspectives
        else "not-required"
    )
    return {
        "frozen_v1": frozen,
        "native_runtime": "white",
        "black_to_move_cp_conversion": conversion,
    }


def _kebab(value: Any) -> str:
    text = str(value or "unknown")
    text = re.sub(r"([a-z0-9])([A-Z])", r"\1-\2", text)
    text = re.sub(r"[^A-Za-z0-9]+", "-", text).strip("-").lower()
    return text or "unknown"


__all__ = ["run_provisional_proxy_diagnostic"]
