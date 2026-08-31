from __future__ import annotations

import hashlib
import json
import math
import random
from pathlib import Path, PurePosixPath
from typing import Any, Mapping, Sequence

from . import __version__
from .adapters import AdapterResult, StageAdapter, StageInvocation
from .attribution import deterministic_attribution_decision
from .canonicalize import Canonicalizer
from .capture import ArtifactStore, utc_now
from .design import (
    ALL_ACTUAL_CONTEXT,
    ALL_ORACLE_CONTEXT,
    STABLE_Q_CONTEXT,
    build_arm_plan,
    parse_factorial_context,
)
from .evaluation import EvaluationAdapter, source_blind_evaluation_view
from .hashing import read_json, sha256_file, sha256_json
from .model import (
    Arm,
    ContractError,
    RunContext,
    STAGES,
    Sample,
    Source,
    StageSampleView,
    StageUnavailable,
)
from .schemas import SchemaRegistry, SemanticTransitionSession
from .statistics import (
    summarize_fresh_confirm_equivalence,
    summarize_registered_contrasts,
)


class ExperimentRunner:
    def __init__(
        self,
        *,
        registry: SchemaRegistry,
        canonicalizer: Canonicalizer,
        adapter: StageAdapter,
        evaluator: EvaluationAdapter,
        store: ArtifactStore,
        context: RunContext,
        preregistration: Mapping[str, Any],
        foundation_control_results: Mapping[str, Any] | None = None,
        split_hash: str | None = None,
        live_command_config_binding: Mapping[str, Any] | None = None,
    ) -> None:
        self.registry = registry
        self.canonicalizer = canonicalizer
        self.adapter = adapter
        self.evaluator = evaluator
        self.store = store
        self.context = context
        self.preregistration = preregistration
        self.foundation_control_results = dict(foundation_control_results or {})
        self.split_hash = split_hash
        if live_command_config_binding is None:
            self.live_command_config_binding: Mapping[str, Any] | None = None
        elif not isinstance(live_command_config_binding, Mapping):
            raise ContractError("live-command config binding must be an object")
        else:
            try:
                self.live_command_config_binding = json.loads(
                    json.dumps(
                        live_command_config_binding,
                        ensure_ascii=False,
                        allow_nan=False,
                    )
                )
            except (TypeError, ValueError) as error:
                raise ContractError(
                    "live-command config binding must be finite JSON"
                ) from error
        self.foundation_control_artifact_hash: str | None = None
        self.runner_producer = {
            "component": "chesstory-judgment-evaluation-runner",
            "version": __version__,
            "configuration_hash": sha256_json(preregistration),
        }
        self._actual_result_cache: dict[
            tuple[str, str, str, str, str], AdapterResult
        ] = {}

    def run_bottleneck_experiment(
        self,
        samples: Sequence[Sample],
        *,
        defer_analysis: bool = False,
    ) -> Mapping[str, Any]:
        """Execute the frozen plan, optionally stopping at the §10 seal boundary.

        Diagnostic runs retain the one-call behavior.  Sealed runs set
        ``defer_analysis`` and receive only an immutable execution bundle; no
        contrasts, equivalence result, or experiment report are created until
        a separately signed pre-unblinding attestation is verified.
        """

        if defer_analysis:
            if self.live_command_config_binding is None:
                raise ContractError(
                    "sealed execution requires a frozen live-command config binding"
                )
            if not self.adapter.is_live_command_only():
                raise ContractError(
                    "sealed execution forbids replay/embedded stage adapters"
                )
            if not self.evaluator.is_live_command():
                raise ContractError(
                    "sealed execution forbids replay/unavailable evaluators"
                )
            existing = [
                name
                for name in (
                    "pre-unblinding-execution.json",
                    "fresh-confirm-equivalence.json",
                    "experiment-report.json",
                )
                if (self.store.run_root / name).exists()
            ]
            if existing:
                raise ContractError(
                    f"sealed run ID was already executed/finalized: {existing}"
                )
        bundle = self._execute_bottleneck_plan(samples)
        if defer_analysis:
            for forbidden in (
                "fresh-confirm-equivalence.json",
                "experiment-report.json",
            ):
                if (self.store.run_root / forbidden).exists():
                    raise ContractError(
                        f"sealed execution already contains post-unblinding artifact: {forbidden}"
                    )
            bundle_hash = self.store.capture_run_document(
                "pre-unblinding-execution", bundle
            )
            self.store.verify()
            return {
                "schema_version": "chesstory.eval.awaiting-attestation.v1",
                "run_id": self.context.run_id,
                "corpus_version": self.context.corpus_version,
                "split": self.context.split,
                "candidate_manifest_hash": self.context.candidate_manifest_hash,
                "split_input_hash": self.split_hash,
                "status": "awaiting-pre-unblinding-attestation",
                "execution_status": bundle["execution_status"],
                "split_sample_count": bundle["split_sample_count"],
                "atomic_cluster_count": bundle["atomic_cluster_count"],
                "registered_arm_count": bundle["registered_arm_count"],
                "executed_arm_count": bundle["executed_arm_count"],
                "expected_row_count": bundle["expected_row_count"],
                "execution_bundle_artifact": {
                    "path": self._run_document_path("pre-unblinding-execution"),
                    "sha256": bundle_hash,
                },
                "foundation_controls": bundle["foundation_controls"],
                "all_oracle_ceiling": bundle["all_oracle_ceiling"],
            }
        return self._finalize_execution_bundle(bundle)

    def _execute_bottleneck_plan(
        self, samples: Sequence[Sample]
    ) -> dict[str, Any]:
        if not samples:
            raise ContractError("experiment split is empty")
        self.registry.validate_registry()
        if self.foundation_control_results:
            self.foundation_control_artifact_hash = self.store.capture_run_document(
                "foundation-control-evidence", self.foundation_control_results
            )
        chains = self._oracle_chains()
        plan = build_arm_plan(chains)
        plan_metadata = [self._arm_metadata(arm) for arm in plan]
        plan_hash = sha256_json(plan_metadata)
        sample_universe = [
            {
                "sample_id": sample.sample_id,
                "atomic_cluster_id": sample.atomic_cluster_id,
            }
            for sample in sorted(samples, key=lambda value: value.sample_id)
        ]
        sample_universe_hash = sha256_json(sample_universe)
        foundation_arms = [
            arm
            for arm in plan
            if arm.context in {ALL_ACTUAL_CONTEXT, STABLE_Q_CONTEXT, ALL_ORACLE_CONTEXT}
        ]
        rows: list[dict[str, Any]] = []
        for arm in foundation_arms:
            rows.extend(self.run_arm(sample, arm) for sample in samples)

        controls = self._foundation_controls(rows)
        ceiling = self._ceiling_status(rows, chains)
        execution_status = "foundation-not-passed"
        if controls["status"] == "passed" and ceiling["status"] == "passed":
            foundation_ids = {arm.arm_id for arm in foundation_arms}
            for arm in plan:
                if arm.arm_id not in foundation_ids:
                    rows.extend(self.run_arm(sample, arm) for sample in samples)
            incomplete = [row for row in rows if row.get("status") != "completed"]
            if incomplete:
                execution_status = "intervention-incomplete"
            else:
                execution_status = "ready-for-analysis"

        return {
            "schema_version": "chesstory.eval.pre-unblinding-execution.v1",
            "binding": self._run_binding(),
            "created_at": utc_now(),
            "preregistration_sha256": sha256_json(self.preregistration),
            "plan_sha256": plan_hash,
            "plan": plan_metadata,
            "sample_universe": sample_universe,
            "sample_universe_sha256": sample_universe_hash,
            "split_sample_count": len(samples),
            "atomic_cluster_count": len({sample.atomic_cluster_id for sample in samples}),
            "registered_arm_count": len(plan),
            "expected_row_count": len(plan) * len(samples),
            "executed_arm_count": len({row["arm_id"] for row in rows}),
            "execution_status": execution_status,
            "foundation_controls": controls,
            "all_oracle_ceiling": ceiling,
            "endpoint_policy_sha256": getattr(
                self.evaluator, "endpoint_policy_hash", None
            ),
            "endpoint_allowed_set_source": getattr(
                self.evaluator,
                "endpoint_allowed_set_source",
                "unavailable-evaluator",
            ),
            "live_command_config": self.live_command_config_binding,
            "rows": rows,
            "artifact_ledger_sha256_before_bundle": self.store.ledger_hash(),
            "access_log_sha256_before_bundle": self.store.access_log_hash(),
        }

    def finalize_execution_bundle(
        self,
        bundle: Mapping[str, Any],
        *,
        verified_pre_unblinding_attestation: Mapping[str, Any] | None = None,
    ) -> Mapping[str, Any]:
        """Analyze one sealed bundle only with its separately verified attestation."""

        self._validate_finalization_attestation(
            bundle, verified_pre_unblinding_attestation
        )
        return self._finalize_execution_bundle(bundle)

    def _finalize_execution_bundle(
        self, bundle: Mapping[str, Any]
    ) -> Mapping[str, Any]:
        """Shared analysis path for diagnostic one-shot and attested sealed runs."""

        if (self.store.run_root / "experiment-report.json").exists():
            raise ContractError("run was already finalized")
        if self.context.split == "fresh-confirm" and (
            self.store.run_root / "fresh-confirm-equivalence.json"
        ).exists():
            raise ContractError("fresh-confirm run was already finalized")
        plan, sample_universe, rows, controls, ceiling = self._validate_execution_bundle(
            bundle
        )
        plan_hash = str(bundle["plan_sha256"])
        sample_universe_hash = str(bundle["sample_universe_sha256"])
        contrasts: list[Mapping[str, Any]] = []
        execution_status = str(bundle["execution_status"])
        if controls["status"] == "passed" and ceiling["status"] == "passed":
            incomplete = [row for row in rows if row.get("status") != "completed"]
            if incomplete or execution_status != "ready-for-analysis":
                experiment_status = "intervention-incomplete"
                attribution = {
                    "status": "indeterminate",
                    "reason": "one or more registered intervention rows did not complete",
                    "production_bottleneck": None,
                }
            else:
                bootstrap = self.preregistration.get("bootstrap", {})
                if not isinstance(bootstrap, Mapping):
                    raise ContractError("preregistration bootstrap configuration is missing")
                contrast_alternative = "greater"
                selected_hypothesis = self.preregistration.get(
                    "selected_attribution_hypothesis"
                )
                if self.context.split == "fresh-confirm" and selected_hypothesis is not None:
                    if not isinstance(selected_hypothesis, Mapping) or selected_hypothesis.get(
                        "direction"
                    ) not in {"positive", "negative"}:
                        raise ContractError(
                            "selected attribution hypothesis direction is invalid"
                        )
                    contrast_alternative = (
                        "greater"
                        if selected_hypothesis["direction"] == "positive"
                        else "less"
                    )
                contrasts = summarize_registered_contrasts(
                    rows,
                    plan,
                    iterations=int(bootstrap.get("iterations", 0)),
                    seed=int(bootstrap.get("seed", self.context.seed)),
                    confidence=float(bootstrap.get("confidence", 0.95)),
                    alternative=contrast_alternative,
                )
                minimums = self.preregistration.get("minimum_effective_counts", {})
                if not isinstance(minimums, Mapping):
                    raise ContractError("minimum_effective_counts is missing")
                required_clusters = int(minimums.get("atomic_clusters", 0))
                observed_clusters = len(
                    {str(value["atomic_cluster_id"]) for value in sample_universe}
                )
                if observed_clusters < required_clusters:
                    experiment_status = (
                        "fresh-confirm-underpowered"
                        if self.context.split == "fresh-confirm"
                        else (
                            "blind-underpowered"
                            if self.context.split == "blind"
                            else "completed-diagnostic"
                        )
                    )
                    if self.context.split in {
                        "diagnostic-explore",
                        "diagnostic-confirm",
                    }:
                        attribution = deterministic_attribution_decision(
                            split=self.context.split,
                            contrasts=contrasts,
                            preregistration=self.preregistration,
                            oracle_chains=self._oracle_chains(),
                            observed_atomic_clusters=observed_clusters,
                            foundation_passed=True,
                            ceiling_passed=True,
                        )
                    else:
                        attribution = {
                            "status": "indeterminate",
                            "reason": (
                                f"effective atomic clusters {observed_clusters} are below the "
                                f"preregistered minimum {required_clusters}"
                            ),
                            "production_bottleneck": None,
                        }
                else:
                    experiment_status = (
                        "completed-fresh-confirm"
                        if self.context.split == "fresh-confirm"
                        else (
                            "completed-blind"
                            if self.context.split == "blind"
                            else "completed-diagnostic"
                        )
                    )
                    if self.context.split in {
                        "diagnostic-explore",
                        "diagnostic-confirm",
                    }:
                        attribution = deterministic_attribution_decision(
                            split=self.context.split,
                            contrasts=contrasts,
                            preregistration=self.preregistration,
                            oracle_chains=self._oracle_chains(),
                            observed_atomic_clusters=observed_clusters,
                            foundation_passed=True,
                            ceiling_passed=True,
                        )
                    else:
                        attribution = {
                            "status": (
                                "requires-fresh-confirm-qualification"
                                if self.context.split == "fresh-confirm"
                                else "final-blind-performance-ready"
                            ),
                            "reason": (
                                "fresh-confirm contrasts require the separately signed effect-direction, "
                                "equivalence, and release-metric qualification decision"
                                if self.context.split == "fresh-confirm"
                                else (
                                    "blind execution is finalized under the signed candidate and run attestation"
                                )
                            ),
                            "production_bottleneck": None,
                        }
        else:
            experiment_status = "foundation-not-passed"
            attribution = {
                "status": "indeterminate",
                "reason": self._indeterminate_reason(controls, ceiling),
                "production_bottleneck": None,
            }

        equivalence = self._fresh_confirm_equivalence(
            rows=rows,
            plan=plan,
            sample_universe=sample_universe,
            controls=controls,
            ceiling=ceiling,
            plan_hash=plan_hash,
            sample_universe_hash=sample_universe_hash,
            endpoint_policy_hash=bundle.get("endpoint_policy_sha256"),
        )
        equivalence_artifact: Mapping[str, str] | None = None
        if self.context.split == "fresh-confirm":
            equivalence_hash = self.store.capture_run_document(
                "fresh-confirm-equivalence", equivalence
            )
            equivalence_artifact = {
                "path": self._run_document_path("fresh-confirm-equivalence"),
                "sha256": equivalence_hash,
            }

        report = {
            "schema_version": "chesstory.eval.experiment-report.v1",
            "run_id": self.context.run_id,
            "created_at": utc_now(),
            "corpus_version": self.context.corpus_version,
            "split": self.context.split,
            "split_sample_count": len(sample_universe),
            "atomic_cluster_count": len(
                {str(value["atomic_cluster_id"]) for value in sample_universe}
            ),
            "seed": self.context.seed,
            "candidate_manifest_hash": self.context.candidate_manifest_hash,
            "split_input_hash": self.split_hash,
            "endpoint_policy_hash": bundle.get("endpoint_policy_sha256"),
            "endpoint_allowed_set_source": bundle.get("endpoint_allowed_set_source"),
            "live_command_config": bundle.get("live_command_config"),
            "preregistration_hash": sha256_json(self.preregistration),
            "plan_hash": plan_hash,
            "sample_universe_sha256": sample_universe_hash,
            "registered_arm_count": len(plan),
            "expected_row_count": len(plan) * len(sample_universe),
            "executed_arm_count": len({row["arm_id"] for row in rows}),
            "status": experiment_status,
            "foundation_controls": controls,
            "all_oracle_ceiling": ceiling,
            "attribution": attribution,
            "statistical_contrasts": contrasts,
            "fresh_confirm_equivalence": equivalence,
            "fresh_confirm_equivalence_artifact": equivalence_artifact,
            "rows": rows,
            "artifact_ledger_hash_before_report": self.store.ledger_hash(),
            "access_log_hash": self.store.access_log_hash(),
            "pre_unblinding_execution_sha256": sha256_json(bundle),
        }
        self.store.capture_run_document("experiment-report", report)
        self.store.verify()
        return report

    def _validate_finalization_attestation(
        self,
        bundle: Mapping[str, Any],
        attestation: Mapping[str, Any] | None,
    ) -> None:
        if not isinstance(attestation, Mapping):
            raise ContractError(
                "sealed finalization requires a verified pre-unblinding attestation"
            )
        if (
            attestation.get("schema_version") != "chesstory.eval.run-attestation.v1"
            or attestation.get("phase") != "pre-statistical-unblinding"
            or not isinstance(attestation.get("signature"), Mapping)
        ):
            raise ContractError("sealed finalization attestation is not signed phase A")
        if (
            attestation.get("run_id") != self.context.run_id
            or attestation.get("corpus_version") != self.context.corpus_version
            or attestation.get("candidate_manifest_sha256")
            != self.context.candidate_manifest_hash
            or attestation.get("split")
            != {"id": self.context.split, "input_sha256": self.split_hash}
        ):
            raise ContractError("sealed finalization attestation binding mismatch")
        bundle_artifact = attestation.get("execution_bundle_artifact")
        if not isinstance(bundle_artifact, Mapping) or bundle_artifact.get(
            "document_sha256"
        ) != sha256_json(bundle):
            raise ContractError("sealed finalization bundle differs from attested phase A")
        execution_binding = attestation.get("execution_binding")
        rows = bundle.get("rows")
        if not isinstance(execution_binding, Mapping) or not isinstance(rows, list):
            raise ContractError("sealed finalization execution binding is missing")
        if (
            execution_binding.get("plan_sha256") != bundle.get("plan_sha256")
            or execution_binding.get("sample_ids_sha256")
            != sha256_json(
                [
                    value.get("sample_id")
                    for value in bundle.get("sample_universe", [])
                    if isinstance(value, Mapping)
                ]
            )
            or execution_binding.get("execution_status")
            != bundle.get("execution_status")
            or execution_binding.get("bundle_row_universe_sha256")
            != sha256_json(rows)
            or execution_binding.get("bundle_row_count") != len(rows)
        ):
            raise ContractError("sealed finalization execution universe is not attested")

    def _validate_execution_bundle(
        self, bundle: Mapping[str, Any]
    ) -> tuple[
        list[Arm],
        list[Mapping[str, Any]],
        list[Mapping[str, Any]],
        Mapping[str, Any],
        Mapping[str, Any],
    ]:
        if not isinstance(bundle, Mapping) or bundle.get("schema_version") != (
            "chesstory.eval.pre-unblinding-execution.v1"
        ):
            raise ContractError("unsupported pre-unblinding execution bundle")
        if bundle.get("binding") != self._run_binding():
            raise ContractError("execution bundle run/candidate/split binding mismatch")
        if bundle.get("preregistration_sha256") != sha256_json(self.preregistration):
            raise ContractError("execution bundle preregistration binding mismatch")
        if bundle.get("live_command_config") != self.live_command_config_binding:
            raise ContractError("execution bundle live-command config binding mismatch")
        plan = build_arm_plan(self._oracle_chains())
        plan_metadata = [self._arm_metadata(arm) for arm in plan]
        if bundle.get("plan") != plan_metadata or bundle.get("plan_sha256") != sha256_json(
            plan_metadata
        ):
            raise ContractError("execution bundle differs from the frozen arm plan")
        if bundle.get("registered_arm_count") != len(plan):
            raise ContractError("execution bundle does not bind the exact arm plan")
        sample_universe = bundle.get("sample_universe")
        if not isinstance(sample_universe, list) or not sample_universe:
            raise ContractError("execution bundle sample universe is missing")
        if bundle.get("sample_universe_sha256") != sha256_json(sample_universe):
            raise ContractError("execution bundle sample universe hash mismatch")
        sample_ids: set[str] = set()
        sample_clusters: dict[str, str] = {}
        for value in sample_universe:
            if not isinstance(value, Mapping):
                raise ContractError("execution sample-universe row must be an object")
            sample_id = value.get("sample_id")
            cluster_id = value.get("atomic_cluster_id")
            if not isinstance(sample_id, str) or not isinstance(cluster_id, str):
                raise ContractError("execution sample-universe identity is invalid")
            if sample_id in sample_ids:
                raise ContractError("execution sample universe has duplicate sample IDs")
            sample_ids.add(sample_id)
            sample_clusters[sample_id] = cluster_id
        if sample_universe != sorted(
            sample_universe, key=lambda value: str(value["sample_id"])
        ):
            raise ContractError("execution sample universe is not canonical")
        expected_row_count = len(plan) * len(sample_universe)
        if (
            bundle.get("split_sample_count") != len(sample_universe)
            or bundle.get("atomic_cluster_count") != len(set(sample_clusters.values()))
            or bundle.get("expected_row_count") != expected_row_count
        ):
            raise ContractError("execution bundle declared sample/row counts are inconsistent")
        rows = bundle.get("rows")
        controls = bundle.get("foundation_controls")
        ceiling = bundle.get("all_oracle_ceiling")
        if not isinstance(rows, list) or not all(isinstance(row, Mapping) for row in rows):
            raise ContractError("execution bundle rows are invalid")
        if not isinstance(controls, Mapping) or not isinstance(ceiling, Mapping):
            raise ContractError("execution bundle foundation/ceiling decisions are missing")
        if controls.get("status") not in {"passed", "not-passed"} or ceiling.get(
            "status"
        ) not in {"passed", "not-passed"}:
            raise ContractError("execution bundle foundation/ceiling status is invalid")
        registered_arm_ids = {arm.arm_id for arm in plan}
        pairs = [(row.get("sample_id"), row.get("arm_id")) for row in rows]
        if len(pairs) != len(set(pairs)) or any(
            sample_id not in sample_ids or arm_id not in registered_arm_ids
            for sample_id, arm_id in pairs
        ):
            raise ContractError("execution bundle row universe is duplicated or unknown")
        for row in rows:
            sample_id = str(row["sample_id"])
            if row.get("atomic_cluster_id") != sample_clusters[sample_id]:
                raise ContractError("execution row atomic-cluster binding is inconsistent")
            status = row.get("status")
            if status not in {"completed", "unavailable", "harness-error"}:
                raise ContractError("execution row status is invalid")
            if status == "completed":
                if row.get("E") not in {0, 1} or not isinstance(
                    row.get("endpoint_artifact"), Mapping
                ):
                    raise ContractError("completed execution row has no valid endpoint")
            elif row.get("E") is not None:
                raise ContractError("incomplete execution row has an endpoint value")
        if bundle.get("executed_arm_count") != len(
            {str(row["arm_id"]) for row in rows}
        ):
            raise ContractError("execution bundle declared arm count is inconsistent")
        expected_status = bundle.get("execution_status")
        if expected_status not in {
            "foundation-not-passed",
            "intervention-incomplete",
            "ready-for-analysis",
        }:
            raise ContractError("execution bundle status is invalid")
        observed_pairs = set(pairs)
        all_pairs = {
            (sample_id, arm.arm_id) for sample_id in sample_ids for arm in plan
        }
        foundation_arm_ids = {
            arm.arm_id
            for arm in plan
            if arm.context
            in {ALL_ACTUAL_CONTEXT, STABLE_Q_CONTEXT, ALL_ORACLE_CONTEXT}
        }
        foundation_pairs = {
            (sample_id, arm_id)
            for sample_id in sample_ids
            for arm_id in foundation_arm_ids
        }
        gates_passed = (
            controls.get("status") == "passed" and ceiling.get("status") == "passed"
        )
        has_incomplete = any(row.get("status") != "completed" for row in rows)
        if expected_status == "foundation-not-passed":
            if gates_passed or observed_pairs != foundation_pairs:
                raise ContractError(
                    "foundation-not-passed bundle has inconsistent gates or row universe"
                )
        elif observed_pairs != all_pairs or not gates_passed:
            raise ContractError(
                "post-foundation execution bundle has inconsistent gates or row universe"
            )
        elif expected_status == "ready-for-analysis" and has_incomplete:
            raise ContractError("ready execution bundle contains incomplete rows")
        elif expected_status == "intervention-incomplete" and not has_incomplete:
            raise ContractError("incomplete execution bundle contains no incomplete row")
        return plan, sample_universe, rows, controls, ceiling

    def run_arm(self, sample: Sample, arm: Arm) -> dict[str, Any]:
        execution_outputs: dict[str, Mapping[str, Any]] = {}
        canonical_outputs: dict[str, Mapping[str, Any]] = {}
        consumed_hashes: dict[str, str] = {}
        stage_diagnostics: dict[str, Any] = {}
        canonical_lineage = self.canonicalizer.lineage_session()
        semantic_lineage = SemanticTransitionSession(self.registry)
        try:
            for stage in STAGES:
                input_document = self._input_document(
                    sample=sample,
                    arm=arm,
                    stage=stage,
                    outputs=execution_outputs,
                    output_hashes=consumed_hashes,
                )
                self.registry.validate_stage(input_document, stage, "input")
                semantic_lineage.validate_input(input_document, stage)
                canonical_lineage.prepare_input(input_document, stage)
                source = arm.source_for(stage)
                invocation = StageInvocation(
                    sample=StageSampleView.from_sample(sample),
                    stage=stage,
                    source=source,
                    oracle_chain=arm.oracle_chain,
                    context=arm.context,
                    input_document=input_document,
                    seed=self.context.seed,
                )
                cache_key = (
                    sample.sample_id,
                    stage,
                    arm.oracle_chain or "-",
                    arm.context,
                    invocation.input_hash,
                )
                if source == Source.FORMAT_CONTROL:
                    result = self._actual_result_cache.get(cache_key)
                    if result is None:
                        raise StageUnavailable(
                            stage,
                            "format control has no previously captured actual artifact "
                            "under the identical conditioned input",
                        )
                else:
                    result = self.adapter.invoke(invocation)
                    if source == Source.ACTUAL:
                        self._actual_result_cache[cache_key] = result
                output_document = self._output_document(
                    sample=sample,
                    arm=arm,
                    stage=stage,
                    input_document=input_document,
                    result=result,
                )
                self.registry.validate_stage(output_document, stage, "output")
                semantic_lineage.validate_output(output_document, stage)
                canonical = canonical_lineage.stage_document(
                    output_document, stage, "output"
                )
                self.registry.validate_stage(canonical, stage, "output")
                consumed = canonical if source in {Source.FORMAT_CONTROL, Source.ORACLE} else output_document
                canonical_hash = sha256_json(canonical)
                consumed_hash = sha256_json(consumed)
                semantic_lineage.commit(stage, consumed, consumed_hash)
                hashes = self.store.capture_stage(
                    sample_id=sample.sample_id,
                    arm_id=arm.arm_id,
                    stage=stage,
                    raw_input=input_document,
                    raw_output=result.raw_output,
                    validated_output=output_document,
                    canonical_output=canonical,
                    provider_io=result.provider_io,
                    metadata={
                        "source": source.value,
                        "oracle_chain": arm.oracle_chain,
                        "experiment_context": arm.context,
                        "input_hash": input_document["input_hash"],
                        "canonical_output_hash": canonical_hash,
                        "consumed_output_hash": consumed_hash,
                        "schema_id": self.registry.schema(stage, "output")[0].get("$id"),
                        "runner_version": __version__,
                    },
                )
                execution_outputs[stage] = consumed
                canonical_outputs[stage] = canonical
                consumed_hashes[stage] = consumed_hash
                stage_diagnostics[stage] = {
                    "input_hash": input_document["input_hash"],
                    "canonical_output_hash": canonical_hash,
                    "consumed_output_hash": consumed_hash,
                    "capture_hashes": hashes,
                }

            evaluation_view = source_blind_evaluation_view(canonical_outputs["P"])
            endpoint = self.evaluator.evaluate(
                sample=sample,
                evaluation_view=evaluation_view,
            )
            endpoint_name = self._endpoint_document_name(sample.sample_id, arm.arm_id)
            endpoint_document = {
                "schema_version": "chesstory.eval.endpoint-capture.v1",
                "binding": {
                    **self._run_binding(),
                    "sample_id": sample.sample_id,
                    "atomic_cluster_id": sample.atomic_cluster_id,
                    "arm_id": arm.arm_id,
                },
                "evaluation_view_sha256": sha256_json(evaluation_view),
                "E": endpoint.value,
                "endpoint_conditions": dict(endpoint.conditions),
                "diagnostics": dict(endpoint.diagnostics),
                "evaluator_artifact_sha256": endpoint.evaluator_artifact_hash,
            }
            endpoint_capture_hash = self.store.capture_run_document(
                endpoint_name, endpoint_document
            )
            return {
                "sample_id": sample.sample_id,
                "atomic_cluster_id": sample.atomic_cluster_id,
                "arm_id": arm.arm_id,
                "status": "completed",
                "E": endpoint.value,
                "endpoint_conditions": dict(endpoint.conditions),
                "diagnostics": dict(endpoint.diagnostics),
                "evaluator_artifact_hash": endpoint.evaluator_artifact_hash,
                "endpoint_artifact": {
                    "path": self._run_document_path(endpoint_name),
                    "sha256": endpoint_capture_hash,
                },
                "stage_artifacts": stage_diagnostics,
            }
        except StageUnavailable as error:
            failure_artifact_hash = self._capture_failure_io(sample, arm, error.stage, error)
            return {
                "sample_id": sample.sample_id,
                "atomic_cluster_id": sample.atomic_cluster_id,
                "arm_id": arm.arm_id,
                "status": "unavailable",
                "stage": error.stage,
                "reason": error.reason,
                "E": None,
                "diagnostics": {},
                "stage_artifacts": stage_diagnostics,
                "failure_artifact_hash": failure_artifact_hash,
            }
        except ContractError as error:
            failure_artifact_hash = self._capture_failure_io(sample, arm, "contract", error)
            return {
                "sample_id": sample.sample_id,
                "atomic_cluster_id": sample.atomic_cluster_id,
                "arm_id": arm.arm_id,
                "status": "harness-error",
                "reason": str(error),
                "E": None,
                "diagnostics": {},
                "stage_artifacts": stage_diagnostics,
                "failure_artifact_hash": failure_artifact_hash,
            }

    def _capture_failure_io(
        self, sample: Sample, arm: Arm, stage: str, error: Exception
    ) -> str | None:
        provider_io = getattr(error, "provider_io", None)
        if provider_io is None:
            return None
        return self.store.capture_run_document(
            f"failure-{sample.sample_id}-{arm.arm_id}-{stage}",
            {
                "schema_version": "chesstory.eval.provider-failure.v1",
                "sample_id": sample.sample_id,
                "arm_id": arm.arm_id,
                "stage": stage,
                "error_type": type(error).__name__,
                "reason": str(error),
                "provider_io": provider_io,
            },
        )

    def _input_document(
        self,
        *,
        sample: Sample,
        arm: Arm,
        stage: str,
        outputs: Mapping[str, Mapping[str, Any]],
        output_hashes: Mapping[str, str],
    ) -> Mapping[str, Any]:
        payload = self._input_payload(sample, stage, outputs)
        upstream_hashes = [output_hashes[registered] for registered in STAGES if registered in output_hashes]
        input_hash = sha256_json(
            {
                "stage": stage,
                "payload": payload,
                "upstream_artifact_hashes": sorted(set(upstream_hashes)),
            }
        )
        return self._envelope(
            sample=sample,
            arm=arm,
            stage=stage,
            input_hash=input_hash,
            upstream_hashes=upstream_hashes,
            producer=self.runner_producer,
            payload=payload,
        )

    def _output_document(
        self,
        *,
        sample: Sample,
        arm: Arm,
        stage: str,
        input_document: Mapping[str, Any],
        result: AdapterResult,
    ) -> Mapping[str, Any]:
        return self._envelope(
            sample=sample,
            arm=arm,
            stage=stage,
            input_hash=str(input_document["input_hash"]),
            upstream_hashes=input_document["upstream_artifact_hashes"],
            producer=result.producer,
            payload=result.output_payload,
        )

    def _envelope(
        self,
        *,
        sample: Sample,
        arm: Arm,
        stage: str,
        input_hash: str,
        upstream_hashes: Sequence[str],
        producer: Mapping[str, Any],
        payload: Mapping[str, Any],
    ) -> Mapping[str, Any]:
        return {
            "schema_version": "1.0.0",
            "stage": stage,
            "sample_id": sample.sample_id,
            "atomic_cluster_id": sample.atomic_cluster_id,
            "corpus_version": sample.corpus_version,
            "split": sample.split,
            "run_id": self.context.run_id,
            "stage_context": self._stage_context(arm, stage),
            "seed": self.context.seed,
            "input_hash": input_hash,
            "upstream_artifact_hashes": sorted(set(upstream_hashes)),
            "producer": dict(producer),
            "payload": dict(payload),
        }

    def _input_payload(
        self,
        sample: Sample,
        stage: str,
        outputs: Mapping[str, Mapping[str, Any]],
    ) -> Mapping[str, Any]:
        def payload(registered: str) -> Mapping[str, Any]:
            document = outputs.get(registered)
            if not isinstance(document, Mapping) or not isinstance(document.get("payload"), Mapping):
                raise StageUnavailable(stage, f"required upstream {registered} artifact is unavailable")
            return document["payload"]  # type: ignore[return-value]

        if stage == "Q":
            return dict(sample.request)
        if stage == "F":
            return {"engine_evidence_pack": payload("Q")}
        if stage == "C":
            return {
                "candidate_lines": payload("Q")["candidate_lines"],
                "fact_bundle": payload("F"),
            }
        if stage == "Jp":
            return {"fact_bundle": payload("F"), "cause_bundle": payload("C")}
        if stage == "Ja":
            return {
                "fact_bundle": payload("F"),
                "cause_bundle": payload("C"),
                "proposal_bundle": payload("Jp"),
            }
        if stage == "R":
            return {"admission_bundle": payload("Ja")}
        if stage == "P":
            return {
                "candidate_lines": payload("Q")["candidate_lines"],
                "fact_bundle": payload("F"),
                "cause_bundle": payload("C"),
                "ranking_bundle": payload("R"),
            }
        raise ContractError(f"unknown stage: {stage}")

    def _stage_context(self, arm: Arm, stage: str) -> Mapping[str, Any]:
        mapping = {
            ALL_ACTUAL_CONTEXT: "baseline",
            STABLE_Q_CONTEXT: "ceiling",
            ALL_ORACLE_CONTEXT: "all-oracle",
            "one-stage": "one-stage",
            "leave-one-actual": "leave-one-actual",
            "forward-cumulative": "forward-substitution",
            "backward-cumulative": "backward-substitution",
        }
        interface = parse_factorial_context(arm.context)
        source = arm.source_for(stage)
        design = "format-control" if source == Source.FORMAT_CONTROL else mapping.get(arm.context)
        if interface is not None:
            design = "format-control" if source == Source.FORMAT_CONTROL else "adjacent-factorial"
        if design is None:
            raise ContractError(f"unregistered experiment context: {arm.context}")
        result: dict[str, Any] = {
            "design": design,
            "experiment_id": self.context.run_id,
            "focus_stages": ([arm.focus_stage] if arm.focus_stage else list(interface or [])),
            "display_label": arm.context,
        }
        if arm.context in {"forward-cumulative", "backward-cumulative"}:
            result["sequence_index"] = STAGES.index(stage)
        if interface is not None:
            result["interface"] = {"upstream": interface[0], "downstream": interface[1]}
        return result

    def _foundation_controls(self, rows: Sequence[Mapping[str, Any]]) -> Mapping[str, Any]:
        baseline = [row for row in rows if self._arm_context(row["arm_id"]) == ALL_ACTUAL_CONTEXT]
        completed = [row for row in baseline if row.get("status") == "completed"]
        errors = [row for row in rows if row.get("status") == "harness-error"]
        unavailable = [row for row in baseline if row.get("status") == "unavailable"]
        canonicalized = len(completed) == len(baseline) and bool(baseline) and all(
            set(row.get("stage_artifacts", {})) == set(STAGES) for row in completed
        )
        required_external = (
            "identity_null_control",
            "same_seed_reproducibility",
            "sentinel_blind_duplicate",
        )
        external, evidence = self._derive_external_foundation_controls(
            required_external, baseline
        )
        external_passed = all(entry.get("status") == "passed" for entry in external.values())
        status = (
            "passed"
            if len(completed) == len(baseline)
            and not errors
            and canonicalized
            and external_passed
            else "not-passed"
        )
        return {
            "status": status,
            "schema_registry": "passed" if not errors else "failed",
            "canonicalizer_idempotence": "passed" if canonicalized else "not-passed",
            "external_controls": external,
            "external_control_evidence": evidence,
            "baseline_completed": len(completed),
            "baseline_total": len(baseline),
            "unavailable": unavailable,
            "errors": errors,
        }

    def _derive_external_foundation_controls(
        self,
        required_names: Sequence[str],
        baseline_rows: Sequence[Mapping[str, Any]],
    ) -> tuple[dict[str, Any], Mapping[str, Any]]:
        not_run = {
            name: {
                "status": "not-run",
                "derived_from_ledger_artifacts": False,
                "evidence_hash": None,
            }
            for name in required_names
        }
        bundle = self.foundation_control_results
        if not bundle:
            return not_run, {
                "status": "not-run",
                "content_sha256": None,
                "artifact_sha256": None,
            }
        if bundle.get("schema_version") != "chesstory.eval.foundation-evidence.v2":
            return self._invalid_foundation_bundle(
                required_names,
                "foundation controls require ledger-bound v2 artifact references; "
                "embedded equality witnesses are not evidence",
            )
        claimed_hash = bundle.get("content_sha256")
        unsigned = dict(bundle)
        unsigned.pop("content_sha256", None)
        if not self._is_sha256(claimed_hash) or sha256_json(unsigned) != claimed_hash:
            return self._invalid_foundation_bundle(
                required_names,
                "foundation evidence content hash mismatch",
                evidence_hash=claimed_hash if isinstance(claimed_hash, str) else None,
            )
        binding = bundle.get("binding")
        expected_binding = self._run_binding()
        if not isinstance(binding, Mapping) or dict(binding) != expected_binding:
            return self._invalid_foundation_bundle(
                required_names,
                "run/candidate/split/seed binding mismatch",
                evidence_hash=str(claimed_hash),
                binding_matches=False,
            )
        candidate_hash_valid = self._is_sha256(
            expected_binding.get("candidate_manifest_sha256")
        )
        split_hash_valid = self._is_sha256(expected_binding.get("split_sha256"))
        if not candidate_hash_valid or not split_hash_valid:
            return self._invalid_foundation_bundle(
                required_names,
                "ledger-bound controls require candidate and split SHA-256 bindings",
                evidence_hash=str(claimed_hash),
                binding_matches=True,
            )

        sample_clusters: dict[str, str] = {}
        duplicate_sample = False
        for row in baseline_rows:
            sample_id = row.get("sample_id")
            cluster_id = row.get("atomic_cluster_id")
            if not isinstance(sample_id, str) or not isinstance(cluster_id, str):
                duplicate_sample = True
                break
            if sample_id in sample_clusters:
                duplicate_sample = True
                break
            sample_clusters[sample_id] = cluster_id
        expected_sample_clusters = {
            sample_id: sample_clusters[sample_id] for sample_id in sorted(sample_clusters)
        }
        supplied_sample_clusters = bundle.get("sample_clusters")
        if (
            duplicate_sample
            or not expected_sample_clusters
            or not isinstance(supplied_sample_clusters, Mapping)
            or dict(supplied_sample_clusters) != expected_sample_clusters
        ):
            return self._invalid_foundation_bundle(
                required_names,
                "foundation evidence does not cover the exact current-run "
                "baseline sample/cluster set",
                evidence_hash=str(claimed_hash),
                binding_matches=True,
            )

        try:
            ledger_index = self._artifact_ledger_index()
            evidence_path = self._run_document_path("foundation-control-evidence")
            self._verified_known_artifact(
                evidence_path,
                str(self.foundation_control_artifact_hash),
                ledger_index,
                event="run-document",
            )
            baseline_snapshots = {
                str(row["sample_id"]): self._baseline_snapshot(row, ledger_index)
                for row in baseline_rows
            }
        except (ContractError, OSError, ValueError, json.JSONDecodeError) as error:
            return self._invalid_foundation_bundle(
                required_names,
                f"baseline or artifact-ledger binding is incomplete: {error}",
                evidence_hash=str(claimed_hash),
                binding_matches=True,
            )

        controls = bundle.get("controls")
        if not isinstance(controls, Mapping):
            controls = {}
        derived = {
            name: self._derive_foundation_control(
                name,
                controls.get(name),
                str(claimed_hash),
                baseline_snapshots,
                ledger_index,
            )
            for name in required_names
        }
        return derived, {
            "status": (
                "verified" if all(value["status"] == "passed" for value in derived.values())
                else "verified-controls-not-passed"
            ),
            "content_sha256": claimed_hash,
            "artifact_sha256": self.foundation_control_artifact_hash,
            "binding_matches": True,
            "sample_clusters_match": True,
            "ledger_bound": True,
        }

    def _derive_foundation_control(
        self,
        name: str,
        raw: Any,
        evidence_hash: str,
        baseline_snapshots: Mapping[str, Mapping[str, Any]],
        ledger_index: Mapping[str, Sequence[Mapping[str, Any]]],
    ) -> Mapping[str, Any]:
        if not isinstance(raw, Mapping):
            return {
                "status": "not-run",
                "derived_from_ledger_artifacts": False,
                "evidence_hash": evidence_hash,
            }
        # A caller-supplied ``status`` is intentionally ignored.  Only typed
        # immutable artifacts referenced by ledger record hashes are inspected.
        entries = raw.get("samples")
        if not isinstance(entries, list):
            return self._failed_ledger_control(
                evidence_hash, "control samples must be an artifact-reference array"
            )
        indexed: dict[str, Mapping[str, Any]] = {}
        for entry in entries:
            if not isinstance(entry, Mapping) or not isinstance(entry.get("sample_id"), str):
                return self._failed_ledger_control(
                    evidence_hash, "every control entry requires a sample_id"
                )
            sample_id = str(entry["sample_id"])
            if sample_id in indexed:
                return self._failed_ledger_control(
                    evidence_hash, f"duplicate control entry for sample {sample_id!r}"
                )
            indexed[sample_id] = entry
        if set(indexed) != set(baseline_snapshots):
            return self._failed_ledger_control(
                evidence_hash,
                "control artifacts do not cover the exact baseline sample set",
            )

        artifact_hashes: list[str] = []
        artifact_paths: set[str] = set()
        try:
            for sample_id in sorted(baseline_snapshots):
                snapshot = baseline_snapshots[sample_id]
                entry = indexed[sample_id]
                if name == "identity_null_control":
                    self._require_keys(
                        entry,
                        {"sample_id", "no_op_artifact"},
                        "identity/null sample entry",
                    )
                    document, paths, hashes = self._load_control_execution(
                        entry["no_op_artifact"],
                        ledger_index,
                        name=name,
                        role="identity-no-op",
                        baseline_snapshot=snapshot,
                    )
                    for path in paths:
                        self._claim_distinct_control_artifact(path, artifact_paths)
                    artifact_hashes.extend(hashes)
                elif name == "same_seed_reproducibility":
                    self._require_keys(
                        entry,
                        {"sample_id", "first_execution_artifact", "second_execution_artifact"},
                        "same-seed sample entry",
                    )
                    first, first_paths, first_hashes = self._load_control_execution(
                        entry["first_execution_artifact"],
                        ledger_index,
                        name=name,
                        role="first",
                        baseline_snapshot=snapshot,
                    )
                    second, second_paths, second_hashes = self._load_control_execution(
                        entry["second_execution_artifact"],
                        ledger_index,
                        name=name,
                        role="second",
                        baseline_snapshot=snapshot,
                    )
                    for path in (*first_paths, *second_paths):
                        self._claim_distinct_control_artifact(path, artifact_paths)
                    if first["execution_id"] == second["execution_id"]:
                        raise ContractError("same-seed executions are not separately captured")
                    artifact_hashes.extend((*first_hashes, *second_hashes))
                elif name == "sentinel_blind_duplicate":
                    self._require_keys(
                        entry,
                        {"sample_id", "first_evaluator_artifact", "second_evaluator_artifact"},
                        "sentinel sample entry",
                    )
                    sentinel_hash = self._preregistered_sentinel_hash()
                    first, first_path, first_hash = self._load_sentinel_evaluation(
                        entry["first_evaluator_artifact"],
                        ledger_index,
                        role="first",
                        baseline_snapshot=snapshot,
                        sentinel_hash=sentinel_hash,
                    )
                    second, second_path, second_hash = self._load_sentinel_evaluation(
                        entry["second_evaluator_artifact"],
                        ledger_index,
                        role="second",
                        baseline_snapshot=snapshot,
                        sentinel_hash=sentinel_hash,
                    )
                    self._claim_distinct_control_artifact(first_path, artifact_paths)
                    self._claim_distinct_control_artifact(second_path, artifact_paths)
                    if (
                        first_path == second_path
                        or first["execution_id"] == second["execution_id"]
                    ):
                        raise ContractError("sentinel evaluations are not separately captured")
                    compared = (
                        "sentinel_definition_sha256",
                        "canonical_p_output_sha256",
                        "source_blind_view_sha256",
                        "normalized_judgment_sha256",
                        "endpoint",
                    )
                    if any(first[key] != second[key] for key in compared):
                        raise ContractError("sentinel blind-duplicate evaluator results differ")
                    artifact_hashes.extend((first_hash, second_hash))
                else:  # pragma: no cover - fixed internal control tuple
                    raise ContractError(f"unknown foundation control: {name}")
        except (ContractError, OSError, ValueError, json.JSONDecodeError) as error:
            return self._failed_ledger_control(evidence_hash, str(error))

        return {
            "status": "passed",
            "derived_from_ledger_artifacts": True,
            "caller_status_ignored": "status" in raw,
            "evidence_hash": evidence_hash,
            "checks": {
                "exact_sample_set": True,
                "exact_atomic_cluster_set": True,
                "run_candidate_split_seed_bound": True,
                "ledger_records_and_file_hashes_verified": True,
                "baseline_artifacts_and_endpoints_match": True,
            },
            "sample_count": len(baseline_snapshots),
            "atomic_cluster_count": len(
                {str(value["atomic_cluster_id"]) for value in baseline_snapshots.values()}
            ),
            "artifact_sha256": sorted(artifact_hashes),
        }

    def _run_binding(self) -> dict[str, Any]:
        return {
            "run_id": self.context.run_id,
            "candidate_manifest_sha256": self.context.candidate_manifest_hash,
            "corpus_version": self.context.corpus_version,
            "split": self.context.split,
            "split_sha256": self.split_hash,
            "seed": self.context.seed,
        }

    def _endpoint_document_name(self, sample_id: str, arm_id: str) -> str:
        return f"endpoint-{self.store._segment(sample_id)}-{self.store._segment(arm_id)}"

    def _run_document_path(self, name: str) -> str:
        filename = self.store._segment(name)
        return filename if filename.endswith(".json") else f"{filename}.json"

    def _invalid_foundation_bundle(
        self,
        names: Sequence[str],
        reason: str,
        *,
        evidence_hash: str | None = None,
        binding_matches: bool | None = None,
    ) -> tuple[dict[str, Any], Mapping[str, Any]]:
        controls = {
            name: {
                "status": "invalid",
                "derived_from_ledger_artifacts": False,
                "reason": reason,
                "evidence_hash": evidence_hash,
            }
            for name in names
        }
        evidence: dict[str, Any] = {
            "status": "invalid",
            "reason": reason,
            "content_sha256": evidence_hash,
            "artifact_sha256": self.foundation_control_artifact_hash,
        }
        if binding_matches is not None:
            evidence["binding_matches"] = binding_matches
        return controls, evidence

    @staticmethod
    def _failed_ledger_control(evidence_hash: str, reason: str) -> Mapping[str, Any]:
        return {
            "status": "failed",
            "derived_from_ledger_artifacts": False,
            "evidence_hash": evidence_hash,
            "reason": reason,
            "checks": {},
        }

    @staticmethod
    def _require_keys(value: Mapping[str, Any], expected: set[str], label: str) -> None:
        # ``status`` is the sole tolerated extra so it can be explicitly ignored.
        keys = set(value) - {"status"}
        if keys != expected:
            raise ContractError(f"{label} must contain exactly {sorted(expected)}")

    @staticmethod
    def _claim_distinct_control_artifact(path: str, claimed: set[str]) -> None:
        if path in claimed:
            raise ContractError(f"control artifact is reused: {path}")
        claimed.add(path)

    def _control_binding(
        self, snapshot: Mapping[str, Any], control_type: str
    ) -> Mapping[str, Any]:
        return {
            **self._run_binding(),
            "sample_id": snapshot["sample_id"],
            "atomic_cluster_id": snapshot["atomic_cluster_id"],
            "baseline_arm_id": snapshot["baseline_arm_id"],
            "control_type": control_type,
        }

    def _load_control_execution(
        self,
        reference: Any,
        ledger_index: Mapping[str, Sequence[Mapping[str, Any]]],
        *,
        name: str,
        role: str,
        baseline_snapshot: Mapping[str, Any],
    ) -> tuple[Mapping[str, Any], tuple[str, ...], tuple[str, ...]]:
        document, summary_path, summary_digest = self._load_referenced_document(
            reference, ledger_index, event="run-document"
        )
        self._require_keys(
            document,
            {
                "schema_version",
                "control_type",
                "role",
                "binding",
                "execution_id",
                "stage_artifacts",
                "endpoint_artifact",
            },
            "foundation control execution artifact",
        )
        if document.get("schema_version") != "chesstory.eval.foundation-control-execution.v1":
            raise ContractError("unsupported foundation control execution artifact")
        if document.get("control_type") != name or document.get("role") != role:
            raise ContractError("foundation control type/role mismatch")
        if document.get("binding") != self._control_binding(baseline_snapshot, name):
            raise ContractError("foundation execution run/sample/control binding mismatch")
        execution_id = document.get("execution_id")
        if not isinstance(execution_id, str) or not execution_id.strip():
            raise ContractError("foundation execution_id must be non-empty text")
        if execution_id == baseline_snapshot["baseline_arm_id"]:
            raise ContractError("foundation execution reuses the baseline arm identifier")

        stages = document.get("stage_artifacts")
        if not isinstance(stages, Mapping) or set(stages) != set(STAGES):
            raise ContractError("foundation execution must reference every stage exactly once")
        paths: list[str] = [summary_path]
        hashes: list[str] = [summary_digest]
        for stage in STAGES:
            references = stages.get(stage)
            if not isinstance(references, Mapping):
                raise ContractError(f"foundation execution {stage} references are malformed")
            self._require_keys(
                references,
                {"raw_input", "canonical_output", "metadata"},
                f"foundation execution {stage} references",
            )
            record_hashes = {
                value.get("ledger_record_hash")
                for value in references.values()
                if isinstance(value, Mapping)
            }
            if len(record_hashes) != 1 or not self._is_sha256(next(iter(record_hashes), None)):
                raise ContractError(
                    f"foundation execution {stage} files are not one stage-capture event"
                )
            loaded: dict[str, tuple[Mapping[str, Any], str, str]] = {}
            for key in ("raw_input", "canonical_output", "metadata"):
                loaded[key] = self._load_referenced_document(
                    references[key],
                    ledger_index,
                    event="stage-capture",
                    expected_record={
                        "sample_id": baseline_snapshot["sample_id"],
                        "arm_id": execution_id,
                        "stage": stage,
                    },
                )
                paths.append(loaded[key][1])
                hashes.append(loaded[key][2])
            if loaded["raw_input"][2] != baseline_snapshot["input_sha256_by_stage"][stage]:
                raise ContractError(
                    f"foundation execution {stage} input is not the baseline input"
                )
            if loaded["canonical_output"][2] != baseline_snapshot[
                "canonical_output_sha256_by_stage"
            ][stage]:
                raise ContractError(
                    f"foundation execution {stage} canonical output differs from baseline"
                )
            metadata = loaded["metadata"][0]
            expected_metadata = {
                "run_id": self.context.run_id,
                "sample_id": baseline_snapshot["sample_id"],
                "arm_id": execution_id,
                "stage": stage,
                "control_type": name,
                "control_role": role,
                "execution_id": execution_id,
                "binding": self._control_binding(baseline_snapshot, name),
                "seed": self.context.seed,
            }
            if any(metadata.get(key) != value for key, value in expected_metadata.items()):
                raise ContractError(
                    f"foundation execution {stage} metadata binding mismatch"
                )

        endpoint, endpoint_path, endpoint_digest = self._load_referenced_document(
            document["endpoint_artifact"], ledger_index, event="run-document"
        )
        paths.append(endpoint_path)
        hashes.append(endpoint_digest)
        self._require_keys(
            endpoint,
            {
                "schema_version",
                "control_type",
                "role",
                "binding",
                "execution_id",
                "evaluation_view_sha256",
                "E",
                "endpoint_conditions",
                "diagnostics",
                "evaluator_artifact_sha256",
            },
            "foundation control endpoint artifact",
        )
        if endpoint.get("schema_version") != "chesstory.eval.foundation-control-endpoint.v1":
            raise ContractError("unsupported foundation control endpoint artifact")
        if (
            endpoint.get("control_type") != name
            or endpoint.get("role") != role
            or endpoint.get("execution_id") != execution_id
            or endpoint.get("binding") != self._control_binding(baseline_snapshot, name)
        ):
            raise ContractError("foundation control endpoint binding mismatch")
        expected_endpoint = baseline_snapshot["endpoint"]
        for key in (
            "evaluation_view_sha256",
            "E",
            "endpoint_conditions",
            "diagnostics",
            "evaluator_artifact_sha256",
        ):
            if endpoint.get(key) != expected_endpoint[key]:
                raise ContractError(
                    f"foundation control endpoint {key} differs from baseline"
                )
        if len(paths) != len(set(paths)):
            raise ContractError("foundation execution reuses an artifact path")
        return document, tuple(paths), tuple(hashes)

    def _load_sentinel_evaluation(
        self,
        reference: Any,
        ledger_index: Mapping[str, Sequence[Mapping[str, Any]]],
        *,
        role: str,
        baseline_snapshot: Mapping[str, Any],
        sentinel_hash: str,
    ) -> tuple[Mapping[str, Any], str, str]:
        document, path, digest = self._load_referenced_document(
            reference, ledger_index, event="run-document"
        )
        self._require_keys(
            document,
            {
                "schema_version",
                "control_type",
                "role",
                "binding",
                "execution_id",
                "sentinel_definition_sha256",
                "canonical_p_output_sha256",
                "source_blind_view_sha256",
                "normalized_judgment_sha256",
                "endpoint",
            },
            "sentinel evaluator artifact",
        )
        if document.get("schema_version") != "chesstory.eval.sentinel-p-evaluator-artifact.v1":
            raise ContractError("unsupported sentinel evaluator artifact")
        if (
            document.get("control_type") != "sentinel_blind_duplicate"
            or document.get("role") != role
        ):
            raise ContractError("sentinel evaluator type/role mismatch")
        if document.get("binding") != self._control_binding(
            baseline_snapshot, "sentinel_blind_duplicate"
        ):
            raise ContractError("sentinel evaluator run/sample/control binding mismatch")
        execution_id = document.get("execution_id")
        if not isinstance(execution_id, str) or not execution_id.strip():
            raise ContractError("sentinel execution_id must be non-empty text")
        for field in (
            "sentinel_definition_sha256",
            "canonical_p_output_sha256",
            "source_blind_view_sha256",
            "normalized_judgment_sha256",
        ):
            if not self._is_sha256(document.get(field)):
                raise ContractError(f"sentinel {field} must be a lowercase SHA-256")
        if document["sentinel_definition_sha256"] != sentinel_hash:
            raise ContractError("sentinel definition is not the preregistered definition")
        if document["canonical_p_output_sha256"] != baseline_snapshot[
            "canonical_output_sha256_by_stage"
        ]["P"]:
            raise ContractError("sentinel P output is not the current baseline P artifact")
        if document["source_blind_view_sha256"] != baseline_snapshot["endpoint"][
            "evaluation_view_sha256"
        ]:
            raise ContractError("sentinel source-blind view is not the current baseline view")
        if document.get("endpoint") != {
            "E": baseline_snapshot["endpoint"]["E"],
            "endpoint_conditions": baseline_snapshot["endpoint"]["endpoint_conditions"],
            "diagnostics": baseline_snapshot["endpoint"]["diagnostics"],
        }:
            raise ContractError("sentinel endpoint/diagnostics do not match current baseline")
        return document, path, digest

    def _preregistered_sentinel_hash(self) -> str:
        controls = self.preregistration.get("foundation_controls")
        if not isinstance(controls, Mapping):
            raise ContractError("sentinel definition hash was not preregistered")
        sentinel = controls.get("sentinel_blind_duplicate")
        if not isinstance(sentinel, Mapping):
            raise ContractError("sentinel definition hash was not preregistered")
        digest = sentinel.get("definition_sha256")
        if not self._is_sha256(digest):
            raise ContractError("preregistered sentinel definition_sha256 is invalid")
        definition = sentinel.get("definition")
        if definition is not None and sha256_json(definition) != digest:
            raise ContractError("preregistered sentinel definition content hash mismatch")
        return str(digest)

    def _baseline_snapshot(
        self,
        row: Mapping[str, Any],
        ledger_index: Mapping[str, Sequence[Mapping[str, Any]]],
    ) -> Mapping[str, Any]:
        if row.get("status") != "completed":
            raise ContractError("baseline row is not complete")
        sample_id = row.get("sample_id")
        cluster_id = row.get("atomic_cluster_id")
        arm_id = row.get("arm_id")
        if not all(isinstance(value, str) and value for value in (sample_id, cluster_id, arm_id)):
            raise ContractError("baseline row identifiers are incomplete")
        if self._arm_context(str(arm_id)) != ALL_ACTUAL_CONTEXT:
            raise ContractError("foundation snapshot row is not an all-actual baseline")
        artifacts = row.get("stage_artifacts")
        if not isinstance(artifacts, Mapping) or set(artifacts) != set(STAGES):
            raise ContractError("baseline row lacks a complete stage artifact chain")

        input_hashes: dict[str, str] = {}
        canonical_hashes: dict[str, str] = {}
        canonical_documents: dict[str, Mapping[str, Any]] = {}
        sample_segment = self.store._segment(str(sample_id))
        arm_segment = self.store._segment(str(arm_id))
        for stage in STAGES:
            stage_row = artifacts.get(stage)
            if not isinstance(stage_row, Mapping):
                raise ContractError(f"baseline {stage} artifact row is malformed")
            capture_hashes = stage_row.get("capture_hashes")
            if not isinstance(capture_hashes, Mapping):
                raise ContractError(f"baseline {stage} capture hashes are missing")
            base = f"samples/{sample_segment}/{arm_segment}/{self.store._segment(stage.lower())}"
            documents: dict[str, Mapping[str, Any]] = {}
            for filename in ("raw-input.json", "canonical-output.json", "metadata.json"):
                digest = capture_hashes.get(filename)
                if not self._is_sha256(digest):
                    raise ContractError(f"baseline {stage} {filename} digest is invalid")
                path, _ = self._verified_known_artifact(
                    f"{base}/{filename}",
                    str(digest),
                    ledger_index,
                    event="stage-capture",
                    expected_record={
                        "sample_id": sample_id,
                        "arm_id": arm_id,
                        "stage": stage,
                    },
                )
                value = read_json(path)
                if not isinstance(value, Mapping):
                    raise ContractError(f"baseline {stage} {filename} is not an object")
                documents[filename] = value
            raw_input = documents["raw-input.json"]
            canonical = documents["canonical-output.json"]
            metadata = documents["metadata.json"]
            expected_document_binding = {
                "run_id": self.context.run_id,
                "sample_id": sample_id,
                "atomic_cluster_id": cluster_id,
                "corpus_version": self.context.corpus_version,
                "split": self.context.split,
                "seed": self.context.seed,
                "stage": stage,
            }
            for label, document in (("input", raw_input), ("canonical output", canonical)):
                for key, expected in expected_document_binding.items():
                    if document.get(key) != expected:
                        raise ContractError(f"baseline {stage} {label} {key} binding mismatch")
            context = canonical.get("stage_context")
            if (
                not isinstance(context, Mapping)
                or context.get("design") != "baseline"
                or context.get("experiment_id") != self.context.run_id
            ):
                raise ContractError(f"baseline {stage} canonical output context mismatch")
            for key, expected in {
                "run_id": self.context.run_id,
                "sample_id": sample_id,
                "arm_id": arm_id,
                "stage": stage,
                "source": "actual",
                "oracle_chain": None,
                "experiment_context": ALL_ACTUAL_CONTEXT,
            }.items():
                if metadata.get(key) != expected:
                    raise ContractError(f"baseline {stage} metadata {key} binding mismatch")
            expected_metadata_files = {
                key: value for key, value in capture_hashes.items() if key != "metadata.json"
            }
            if metadata.get("files") != expected_metadata_files:
                raise ContractError(f"baseline {stage} metadata/capture hash snapshot mismatch")
            if canonical.get("input_hash") != raw_input.get("input_hash") or metadata.get(
                "input_hash"
            ) != raw_input.get("input_hash"):
                raise ContractError(f"baseline {stage} input lineage mismatch")
            row_canonical = stage_row.get("canonical_output_hash")
            if row_canonical != capture_hashes.get("canonical-output.json"):
                raise ContractError(f"baseline {stage} row/canonical artifact hash mismatch")
            input_hashes[stage] = str(capture_hashes["raw-input.json"])
            canonical_hashes[stage] = str(capture_hashes["canonical-output.json"])
            canonical_documents[stage] = canonical

        endpoint_ref = row.get("endpoint_artifact")
        if not isinstance(endpoint_ref, Mapping):
            raise ContractError("baseline endpoint artifact reference is missing")
        endpoint_path = endpoint_ref.get("path")
        endpoint_hash = endpoint_ref.get("sha256")
        if not isinstance(endpoint_path, str) or not self._is_sha256(endpoint_hash):
            raise ContractError("baseline endpoint artifact reference is malformed")
        path, _ = self._verified_known_artifact(
            endpoint_path,
            str(endpoint_hash),
            ledger_index,
            event="run-document",
        )
        endpoint = read_json(path)
        if not isinstance(endpoint, Mapping):
            raise ContractError("baseline endpoint artifact is not an object")
        self._require_keys(
            endpoint,
            {
                "schema_version",
                "binding",
                "evaluation_view_sha256",
                "E",
                "endpoint_conditions",
                "diagnostics",
                "evaluator_artifact_sha256",
            },
            "baseline endpoint artifact",
        )
        if endpoint.get("schema_version") != "chesstory.eval.endpoint-capture.v1":
            raise ContractError("unsupported baseline endpoint artifact")
        expected_endpoint_binding = {
            **self._run_binding(),
            "sample_id": sample_id,
            "atomic_cluster_id": cluster_id,
            "arm_id": arm_id,
        }
        if endpoint.get("binding") != expected_endpoint_binding:
            raise ContractError("baseline endpoint run/sample binding mismatch")
        expected_view_hash = sha256_json(source_blind_evaluation_view(canonical_documents["P"]))
        if endpoint.get("evaluation_view_sha256") != expected_view_hash:
            raise ContractError("baseline endpoint source-blind view hash mismatch")
        expected_endpoint = {
            "E": row.get("E"),
            "endpoint_conditions": row.get("endpoint_conditions"),
            "diagnostics": row.get("diagnostics"),
            "evaluator_artifact_sha256": row.get("evaluator_artifact_hash"),
        }
        if any(endpoint.get(key) != value for key, value in expected_endpoint.items()):
            raise ContractError("baseline endpoint artifact/result row mismatch")
        if row.get("E") not in {0, 1} or not self._is_sha256(row.get("evaluator_artifact_hash")):
            raise ContractError("baseline endpoint value or evaluator hash is invalid")

        return {
            "sample_id": sample_id,
            "atomic_cluster_id": cluster_id,
            "baseline_arm_id": arm_id,
            "input_sha256_by_stage": input_hashes,
            "canonical_output_sha256_by_stage": canonical_hashes,
            "endpoint": {
                "artifact_sha256": endpoint_hash,
                "evaluation_view_sha256": expected_view_hash,
                **expected_endpoint,
            },
        }

    def _artifact_ledger_index(self) -> dict[str, list[Mapping[str, Any]]]:
        path = self.store.ledger_path
        if not path.is_file():
            raise ContractError("artifact ledger is missing")
        index: dict[str, list[Mapping[str, Any]]] = {}
        previous: str | None = None
        sequence = 1
        with path.open("r", encoding="utf-8") as handle:
            for line_number, line in enumerate(handle, 1):
                if not line.strip():
                    continue
                record = json.loads(line)
                if not isinstance(record, Mapping):
                    raise ContractError(f"artifact ledger line {line_number} is not an object")
                if (
                    record.get("sequence") != sequence
                    or record.get("previous_record_hash") != previous
                ):
                    raise ContractError(f"artifact ledger chain breaks at line {line_number}")
                claimed = record.get("record_hash")
                unsigned = dict(record)
                unsigned.pop("record_hash", None)
                if not self._is_sha256(claimed) or sha256_json(unsigned) != claimed:
                    raise ContractError(
                        f"artifact ledger record hash mismatch at line {line_number}"
                    )
                if record.get("run_id") != self.context.run_id:
                    raise ContractError(
                        f"artifact ledger run binding mismatch at line {line_number}"
                    )
                artifact_path = record.get("artifact_path")
                hashes = record.get("artifact_hashes")
                if artifact_path is not None or hashes is not None:
                    if not isinstance(artifact_path, str) or not isinstance(hashes, Mapping):
                        raise ContractError(
                            f"artifact ledger capture is malformed at line {line_number}"
                        )
                    base, _ = self._safe_run_relative_path(artifact_path)
                    event = record.get("event")
                    for filename, digest in hashes.items():
                        if (
                            not isinstance(filename, str)
                            or PurePosixPath(filename).name != filename
                        ):
                            raise ContractError(
                                f"unsafe ledger artifact filename at line {line_number}"
                            )
                        if not self._is_sha256(digest):
                            raise ContractError(
                                f"invalid ledger artifact digest at line {line_number}"
                            )
                        if event == "run-document":
                            if PurePosixPath(base).name != filename:
                                raise ContractError(
                                    "run-document ledger path mismatch at line "
                                    f"{line_number}"
                                )
                            relative = base
                        else:
                            relative = f"{base}/{filename}"
                        relative, _ = self._safe_run_relative_path(relative)
                        index.setdefault(relative, []).append(
                            {"digest": digest, "record_hash": claimed, "record": record}
                        )
                previous = str(claimed)
                sequence += 1
        return index

    def _load_referenced_document(
        self,
        reference: Any,
        ledger_index: Mapping[str, Sequence[Mapping[str, Any]]],
        *,
        event: str,
        expected_record: Mapping[str, Any] | None = None,
    ) -> tuple[Mapping[str, Any], str, str]:
        if not isinstance(reference, Mapping):
            raise ContractError("artifact reference must be an object")
        self._require_keys(
            reference,
            {"path", "sha256", "ledger_record_hash"},
            "artifact reference",
        )
        relative = reference.get("path")
        digest = reference.get("sha256")
        record_hash = reference.get("ledger_record_hash")
        if not isinstance(relative, str) or not self._is_sha256(digest) or not self._is_sha256(
            record_hash
        ):
            raise ContractError("artifact reference path or digest is invalid")
        relative, path = self._safe_run_relative_path(relative)
        matches = [
            entry
            for entry in ledger_index.get(relative, ())
            if entry.get("digest") == digest
            and entry.get("record_hash") == record_hash
            and isinstance(entry.get("record"), Mapping)
            and entry["record"].get("event") == event
            and (
                expected_record is None
                or all(
                    entry["record"].get(key) == value
                    for key, value in expected_record.items()
                )
            )
        ]
        if len(matches) != 1:
            raise ContractError(f"artifact reference is absent or ambiguous in ledger: {relative}")
        if not path.is_file() or sha256_file(path) != digest:
            raise ContractError(f"artifact file is missing or hash-mismatched: {relative}")
        value = read_json(path)
        if not isinstance(value, Mapping):
            raise ContractError(f"referenced artifact is not a JSON object: {relative}")
        return value, relative, str(digest)

    def _verified_known_artifact(
        self,
        relative: str,
        digest: str,
        ledger_index: Mapping[str, Sequence[Mapping[str, Any]]],
        *,
        event: str,
        expected_record: Mapping[str, Any] | None = None,
    ) -> tuple[Path, Mapping[str, Any]]:
        relative, path = self._safe_run_relative_path(relative)
        matches: list[Mapping[str, Any]] = []
        for entry in ledger_index.get(relative, ()):
            record = entry.get("record")
            if (
                entry.get("digest") == digest
                and isinstance(record, Mapping)
                and record.get("event") == event
                and (
                    expected_record is None
                    or all(record.get(key) == value for key, value in expected_record.items())
                )
            ):
                matches.append(entry)
        if not matches:
            raise ContractError(f"artifact is not bound in ledger: {relative}")
        if not path.is_file() or sha256_file(path) != digest:
            raise ContractError(f"artifact file is missing or hash-mismatched: {relative}")
        return path, matches[-1]

    def _safe_run_relative_path(self, value: str) -> tuple[str, Path]:
        if not isinstance(value, str) or not value or "\\" in value:
            raise ContractError("artifact path must be a non-empty POSIX relative path")
        pure = PurePosixPath(value)
        unsafe_part = any(
            part in {"", ".", ".."} or ":" in part for part in pure.parts
        )
        if pure.is_absolute() or unsafe_part:
            raise ContractError(f"unsafe artifact relative path: {value!r}")
        relative = pure.as_posix()
        run_root = self.store.run_root.resolve()
        path = (run_root / Path(*pure.parts)).resolve()
        try:
            path.relative_to(run_root)
        except ValueError as error:
            raise ContractError(f"artifact path escapes current run: {value!r}") from error
        return relative, path

    def _fresh_confirm_equivalence(
        self,
        *,
        rows: Sequence[Mapping[str, Any]],
        plan: Sequence[Arm],
        sample_universe: Sequence[Mapping[str, Any]],
        controls: Mapping[str, Any],
        ceiling: Mapping[str, Any],
        plan_hash: str,
        sample_universe_hash: str,
        endpoint_policy_hash: Any,
    ) -> Mapping[str, Any]:
        """Run equivalence only over the authoritative full fresh-confirm run.

        ``statistics.summarize_fresh_confirm_equivalence`` is intentionally a
        pure statistical primitive.  This wrapper supplies the trusted run
        identity, full frozen arm plan, exact split sample universe, endpoint
        provenance, and preregistered release gates that the primitive cannot
        infer from a bag of rows.
        """

        epsilon = self.preregistration.get("epsilon_gain")
        epsilon_hash = sha256_json(epsilon) if isinstance(epsilon, Mapping) else None
        row_bindings: list[dict[str, Any]] = []
        for row in sorted(
            rows,
            key=lambda value: (
                str(value.get("sample_id", "")),
                str(value.get("arm_id", "")),
            ),
        ):
            endpoint_artifact = row.get("endpoint_artifact")
            endpoint_hash = (
                endpoint_artifact.get("sha256")
                if isinstance(endpoint_artifact, Mapping)
                else None
            )
            stage_artifacts = row.get("stage_artifacts")
            canonical_stage_hashes = {
                stage: value.get("canonical_output_hash")
                for stage, value in sorted(
                    stage_artifacts.items() if isinstance(stage_artifacts, Mapping) else ()
                )
                if isinstance(stage, str) and isinstance(value, Mapping)
            }
            row_bindings.append(
                {
                    "sample_id": row.get("sample_id"),
                    "atomic_cluster_id": row.get("atomic_cluster_id"),
                    "arm_id": row.get("arm_id"),
                    "status": row.get("status"),
                    "E": row.get("E"),
                    "endpoint_artifact_sha256": endpoint_hash,
                    "evaluator_artifact_sha256": row.get("evaluator_artifact_hash"),
                    "stage_canonical_sha256": canonical_stage_hashes,
                }
            )
        row_universe_hash = sha256_json(row_bindings)
        binding = {
            **self._run_binding(),
            "preregistration_sha256": sha256_json(self.preregistration),
            "plan_sha256": plan_hash,
            "registered_arm_count": len(plan),
            "sample_universe_sha256": sample_universe_hash,
            "row_universe_sha256": row_universe_hash,
            "epsilon_registry_sha256": epsilon_hash,
            "endpoint_policy_sha256": endpoint_policy_hash,
        }
        if self.context.split != "fresh-confirm":
            return {
                "schema_version": "chesstory.eval.fresh-confirm-equivalence.v1",
                "status": "not-applicable",
                "binding": binding,
                "reason_codes": ["split-is-not-fresh-confirm"],
                "interpretation": "residual-gain equivalence is reserved for fresh-confirm",
            }

        reasons: list[str] = []
        if controls.get("status") != "passed":
            reasons.append("foundation-controls-not-passed")
        if ceiling.get("status") != "passed":
            reasons.append("all-oracle-ceiling-not-passed")
        if not self._is_sha256(self.context.candidate_manifest_hash):
            reasons.append("signed-candidate-binding-missing")
        if not self._is_sha256(self.split_hash):
            reasons.append("split-binding-missing")
        if not self._is_sha256(endpoint_policy_hash):
            reasons.append("frozen-endpoint-policy-missing")
        expected_pairs = {
            (str(sample["sample_id"]), arm.arm_id)
            for sample in sample_universe
            for arm in plan
        }
        observed_pairs: list[tuple[str, str]] = []
        for row in rows:
            sample_id = row.get("sample_id")
            arm_id = row.get("arm_id")
            if isinstance(sample_id, str) and isinstance(arm_id, str):
                observed_pairs.append((sample_id, arm_id))
            else:
                reasons.append("row-identity-malformed")
        observed_pair_set = set(observed_pairs)
        if len(observed_pairs) != len(observed_pair_set):
            reasons.append("duplicate-sample-arm-row")
        if observed_pair_set != expected_pairs:
            reasons.append("incomplete-or-extra-sample-arm-universe")
        if any(row.get("status") != "completed" for row in rows):
            reasons.append("not-all-registered-rows-completed")

        minimums = self.preregistration.get("minimum_effective_counts")
        bootstrap = self.preregistration.get("bootstrap")
        multiplicity = self.preregistration.get("multiplicity")
        equivalence_policy = self.preregistration.get("equivalence")
        if not isinstance(minimums, Mapping):
            reasons.append("minimum-effective-counts-missing")
        if not isinstance(bootstrap, Mapping):
            reasons.append("bootstrap-policy-missing")
        if not isinstance(multiplicity, Mapping):
            reasons.append("multiplicity-policy-missing")
        if not isinstance(equivalence_policy, Mapping):
            reasons.append("equivalence-upper-bound-policy-not-preregistered")
        elif equivalence_policy.get("upper_bound_method") != (
            "holm-boundary-plus-bonferroni-simultaneous-one-sided-percentile"
        ):
            reasons.append("unsupported-equivalence-upper-bound-policy")
        if not isinstance(epsilon, Mapping):
            reasons.append("epsilon-registry-missing")

        required_raters = (
            int(minimums.get("held_out_human_raters", 0))
            if isinstance(minimums, Mapping)
            else 0
        )
        observed_raters: list[int] = []
        evaluator_hashes_valid = True
        for row in rows:
            diagnostics = row.get("diagnostics")
            count = (
                diagnostics.get("held_out_human_rater_count")
                if isinstance(diagnostics, Mapping)
                else None
            )
            if isinstance(count, int) and not isinstance(count, bool):
                observed_raters.append(count)
            else:
                observed_raters.append(0)
            if row.get("status") == "completed" and not self._is_sha256(
                row.get("evaluator_artifact_hash")
            ):
                evaluator_hashes_valid = False
        minimum_observed_raters = min(observed_raters) if observed_raters else 0
        if minimum_observed_raters < required_raters:
            reasons.append("minimum-held-out-human-raters-not-met")
        if not evaluator_hashes_valid:
            reasons.append("evaluator-artifact-binding-missing")

        gate = {
            "expected_row_count": len(expected_pairs),
            "observed_row_count": len(rows),
            "exact_sample_arm_universe": observed_pair_set == expected_pairs,
            "all_rows_completed": bool(rows)
            and all(row.get("status") == "completed" for row in rows),
            "minimum_held_out_human_raters": minimum_observed_raters,
            "required_held_out_human_raters": required_raters,
            "evaluator_artifacts_bound": evaluator_hashes_valid,
        }
        reasons = sorted(set(reasons))
        if reasons:
            return {
                "schema_version": "chesstory.eval.fresh-confirm-equivalence.v1",
                "status": "indeterminate",
                "binding": binding,
                "gate": gate,
                "reason_codes": reasons,
                "interpretation": (
                    "equivalence was not tested because the authoritative "
                    "fresh-confirm run gates were not satisfied"
                ),
            }

        assert isinstance(epsilon, Mapping)
        assert isinstance(minimums, Mapping)
        assert isinstance(bootstrap, Mapping)
        assert isinstance(multiplicity, Mapping)
        result = summarize_fresh_confirm_equivalence(
            rows,
            plan,
            epsilon,  # type: ignore[arg-type]
            iterations=int(bootstrap.get("iterations", 0)),
            seed=int(bootstrap.get("seed", self.context.seed)),
            split=self.context.split,
            fresh_confirm=True,
            min_clusters=int(minimums.get("atomic_clusters", 0)),
            alpha=float(multiplicity.get("family_wise_alpha", 0.05)),
        )
        return {**result, "binding": binding, "gate": gate}

    @staticmethod
    def _is_sha256(value: Any) -> bool:
        return isinstance(value, str) and len(value) == 64 and all(
            character in "0123456789abcdef" for character in value
        )

    def _ceiling_status(self, rows: Sequence[Mapping[str, Any]], chains: Sequence[str]) -> Mapping[str, Any]:
        threshold = float(self.preregistration.get("ceiling_min_success_rate", 1.0))
        minimums = self.preregistration.get("minimum_effective_counts", {})
        bootstrap = self.preregistration.get("bootstrap", {})
        if not isinstance(minimums, Mapping) or not isinstance(bootstrap, Mapping):
            raise ContractError("ceiling inference requires preregistered minimums/bootstrap")
        required_clusters = int(minimums.get("atomic_clusters", 0))
        required_raters = int(minimums.get("held_out_human_raters", 0))
        iterations = int(bootstrap.get("iterations", 0))
        confidence = float(bootstrap.get("confidence", 0.95))
        if iterations <= 0 or not 0.0 < confidence < 1.0:
            raise ContractError("invalid preregistered ceiling bootstrap configuration")
        results: dict[str, Any] = {}
        passed = True
        for chain in chains:
            arms = [
                arm
                for arm in build_arm_plan(chains)
                if arm.context == ALL_ORACLE_CONTEXT and arm.oracle_chain == chain
            ]
            arm_ids = {arm.arm_id for arm in arms}
            selected = [row for row in rows if row["arm_id"] in arm_ids]
            completed = [row for row in selected if row.get("status") == "completed"]
            rate = (
                sum(int(row["E"]) for row in completed) / len(completed)
                if len(completed) == len(selected) and selected
                else None
            )
            cluster_count = len(
                {str(row["atomic_cluster_id"]) for row in completed}
            )
            rater_counts = [
                int(row.get("diagnostics", {}).get("held_out_human_rater_count", 0))
                for row in completed
                if isinstance(row.get("diagnostics"), Mapping)
            ]
            minimum_observed_raters = min(rater_counts) if len(rater_counts) == len(completed) and rater_counts else 0
            lower_bound = (
                self._cluster_success_lower_bound(
                    completed,
                    iterations=iterations,
                    confidence=confidence,
                    seed=self._derived_ceiling_seed(chain),
                )
                if len(completed) == len(selected) and selected
                else None
            )
            chain_passed = all(
                (
                    lower_bound is not None,
                    lower_bound is not None and lower_bound >= threshold,
                    cluster_count >= required_clusters,
                    minimum_observed_raters >= required_raters,
                )
            )
            passed = passed and chain_passed
            results[chain] = {
                "status": "passed" if chain_passed else "not-passed",
                "success_rate": rate,
                "required_success_rate": threshold,
                "one_sided_cluster_ci_lower": lower_bound,
                "confidence": confidence,
                "bootstrap_iterations": iterations,
                "atomic_clusters": cluster_count,
                "required_atomic_clusters": required_clusters,
                "minimum_held_out_human_raters": minimum_observed_raters,
                "required_held_out_human_raters": required_raters,
                "completed": len(completed),
                "total": len(selected),
                "failures": [row for row in selected if row.get("status") != "completed"],
            }
        return {"status": "passed" if passed else "not-passed", "chains": results}

    @staticmethod
    def _cluster_success_lower_bound(
        rows: Sequence[Mapping[str, Any]],
        *,
        iterations: int,
        confidence: float,
        seed: int,
    ) -> float:
        clusters: dict[str, list[int]] = {}
        for row in rows:
            value = row.get("E")
            if value not in {0, 1}:
                raise ContractError("ceiling endpoint E must be binary")
            cluster = row.get("atomic_cluster_id")
            if not isinstance(cluster, str) or not cluster:
                raise ContractError("ceiling row is missing atomic_cluster_id")
            clusters.setdefault(cluster, []).append(int(value))
        summaries = [
            (sum(clusters[key]), len(clusters[key])) for key in sorted(clusters)
        ]
        if not summaries:
            raise ContractError("ceiling cluster bootstrap requires rows")
        generator = random.Random(seed)
        draws: list[float] = []
        for _ in range(iterations):
            total = 0
            count = 0
            for _ in summaries:
                successes, size = summaries[generator.randrange(len(summaries))]
                total += successes
                count += size
            draws.append(total / count)
        draws.sort()
        probability = 1.0 - confidence
        position = (len(draws) - 1) * probability
        lower = math.floor(position)
        upper = math.ceil(position)
        if lower == upper:
            return draws[lower]
        weight = position - lower
        return draws[lower] * (1.0 - weight) + draws[upper] * weight

    def _derived_ceiling_seed(self, chain: str) -> int:
        payload = f"{self.context.seed}\x00all-oracle-ceiling\x00{chain}".encode("utf-8")
        return int.from_bytes(hashlib.sha256(payload).digest()[:8], "big")

    def _arm_context(self, arm_id: str) -> str:
        for arm in build_arm_plan(self._oracle_chains()):
            if arm.arm_id == arm_id:
                return arm.context
        raise ContractError(f"unknown arm id in result: {arm_id}")

    def _oracle_chains(self) -> tuple[str, str]:
        value = self.preregistration.get("oracle_chains")
        if not isinstance(value, list) or len(value) != 2 or not all(isinstance(item, str) for item in value):
            raise ContractError("preregistration must freeze exactly two oracle_chains")
        return value[0], value[1]

    @staticmethod
    def _arm_metadata(arm: Arm) -> Mapping[str, Any]:
        return {
            "arm_id": arm.arm_id,
            "sources": {stage: source.value for stage, source in arm.sources.items()},
            "oracle_chain": arm.oracle_chain,
            "focus_stage": arm.focus_stage,
            "context": arm.context,
        }

    @staticmethod
    def _indeterminate_reason(
        controls: Mapping[str, Any], ceiling: Mapping[str, Any]
    ) -> str:
        if controls.get("status") != "passed":
            unavailable = controls.get("unavailable", [])
            if unavailable:
                first = unavailable[0]
                return (
                    "actual chain is not fully executable under the frozen stage contract: "
                    f"{first.get('stage')} - {first.get('reason')}; no production stage attribution is valid"
                )
            return "harness foundation controls did not pass; no production stage attribution is valid"
        return "both independent all-oracle ceilings did not pass; foundation decision tree is required"
