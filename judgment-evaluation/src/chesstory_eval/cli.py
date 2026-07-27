from __future__ import annotations

import argparse
import json
import os
import tempfile
from collections import Counter
from pathlib import Path
from typing import Any, Mapping, Sequence

from . import __version__
from .adapters import (
    AvailabilityGuard,
    CommandStageAdapter,
    CompositeAdapter,
    EmbeddedAcquisitionAdapter,
    LiveCommandConfig,
    ReplayStageAdapter,
    load_live_command_config,
)
from .canonicalize import Canonicalizer
from .capture import ArtifactStore, load_custodian_storage_attestation, utc_now
from .corpus import Corpus
from .design import build_arm_plan
from .evaluation import (
    CommandEvaluationAdapter,
    FrozenEndpointPolicy,
    ReplayEvaluationAdapter,
    UnavailableEvaluator,
)
from .hashing import json_bytes, read_json, sha256_file, sha256_json
from .model import ContractError, HarnessError, RunContext, SplitSealed
from .runner import ExperimentRunner
from .schemas import SchemaRegistry
from .provisional_proxy import (
    run_provisional_proxy_diagnostic,
    validate_provisional_run_id,
)
from .attestation import (
    build_candidate_manifest,
    build_fresh_confirm_qualification,
    build_run_attestation,
    verify_candidate_execution_bindings,
    verify_attested_run_snapshot,
    verify_signed_candidate_manifest,
    verify_signed_fresh_confirm_qualification,
    verify_signed_run_attestation,
    write_signed_candidate_manifest,
    write_signed_fresh_confirm_qualification,
    write_signed_run_attestation,
)


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="judgment-eval",
        description="External Chesstory Q→F→C→Jp→Ja→R→P→V evaluation harness",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    doctor = subparsers.add_parser("doctor", help="verify frozen contracts and harness controls")
    _root_argument(doctor)

    plan = subparsers.add_parser("plan", help="materialize the preregistered intervention plan")
    _root_argument(plan)
    plan.add_argument("--output", type=Path)

    run = subparsers.add_parser("run", help="run the gated bottleneck experiment")
    _root_argument(run)
    run.add_argument("--run-id", required=True)
    run.add_argument("--split", default="diagnostic-explore")
    run.add_argument("--artifacts", type=Path)
    run.add_argument("--component-root", type=Path)
    run.add_argument("--stage-replay", action="append", type=Path, default=[])
    run.add_argument("--evaluator-replay", action="append", type=Path, default=[])
    run.add_argument(
        "--live-command-config",
        type=Path,
        help="pre-frozen strict JSON routes for live stage/oracle/evaluator commands",
    )
    run.add_argument("--foundation-controls", type=Path)
    run.add_argument(
        "--adjudication",
        action="append",
        default=[],
        metavar="NAME=SOURCE_PATH",
        help="source-blind adjudication document to capture before sealed execution",
    )
    run.add_argument(
        "--candidate-manifest-hash",
        help="diagnostic-explore metadata only; sealed runs derive this from --candidate-manifest",
    )
    run.add_argument("--candidate-manifest", type=Path)
    run.add_argument("--endpoint-policy", type=Path)
    run.add_argument("--fresh-confirm-attestation", type=Path)
    run.add_argument("--fresh-confirm-decision", type=Path)
    run.add_argument("--output", type=Path)

    freeze = subparsers.add_parser(
        "freeze-candidate", help="build and sign a release-qualified pre-run candidate manifest"
    )
    _root_argument(freeze)
    freeze.add_argument("--candidate-id", required=True)
    freeze.add_argument("--components", required=True, type=Path)
    freeze.add_argument("--output", required=True, type=Path)
    freeze.add_argument("--key-id", required=True)
    freeze.add_argument("--component-root", type=Path)
    freeze.add_argument(
        "--live-command-config",
        required=True,
        type=Path,
        help="pre-frozen live route config to bind into the signed candidate",
    )

    attest = subparsers.add_parser(
        "attest-run",
        help="sign raw execution/adjudication evidence before statistical unblinding",
    )
    _root_argument(attest)
    attest.add_argument("--run-id", required=True)
    attest.add_argument("--artifacts", type=Path)
    attest.add_argument("--component-root", type=Path)
    attest.add_argument("--candidate-manifest", required=True, type=Path)
    attest.add_argument("--split", required=True)
    attest.add_argument("--execution-manifest-path", required=True)
    attest.add_argument("--execution-bundle-path", required=True)
    attest.add_argument("--endpoint-policy-path", required=True)
    attest.add_argument("--oracle-graph-path", required=True)
    attest.add_argument("--allowed-set-path", required=True)
    attest.add_argument("--adjudication", action="append", default=[], metavar="NAME=RUN_PATH")
    attest.add_argument("--raw-provider-io-required", action="store_true")
    attest.add_argument("--output", required=True, type=Path)
    attest.add_argument("--key-id", required=True)

    finalize = subparsers.add_parser(
        "finalize-run",
        help="compute statistics/report only after verifying signed pre-unblinding attestation",
    )
    _root_argument(finalize)
    finalize.add_argument("--run-id", required=True)
    finalize.add_argument("--artifacts", type=Path)
    finalize.add_argument("--component-root", type=Path)
    finalize.add_argument("--candidate-manifest", required=True, type=Path)
    finalize.add_argument("--split", required=True)
    finalize.add_argument("--pre-unblinding-attestation", required=True, type=Path)
    finalize.add_argument("--output", type=Path)

    qualify = subparsers.add_parser(
        "qualify-fresh-confirm",
        help="sign a post-unblinding PASS certificate bound to a pre-unblinding attestation",
    )
    _root_argument(qualify)
    qualify.add_argument("--run-id", required=True)
    qualify.add_argument("--artifacts", type=Path)
    qualify.add_argument("--component-root", type=Path)
    qualify.add_argument("--candidate-manifest", required=True, type=Path)
    qualify.add_argument("--pre-unblinding-attestation", required=True, type=Path)
    qualify.add_argument("--experiment-report", required=True, type=Path)
    qualify.add_argument("--equivalence", required=True, type=Path)
    qualify.add_argument("--release-evidence", required=True, type=Path)
    qualify.add_argument("--output", required=True, type=Path)
    qualify.add_argument("--key-id", required=True)

    q_diagnostic = subparsers.add_parser(
        "q-diagnostic", help="acquire richer/stable Q evidence with Stockfish"
    )
    _root_argument(q_diagnostic)
    q_diagnostic.add_argument("--stockfish", required=True, type=Path)
    q_diagnostic.add_argument("--output", required=True, type=Path)
    q_diagnostic.add_argument("--run-id", default="q-diagnostic-explore")
    q_diagnostic.add_argument("--artifacts", type=Path)
    q_diagnostic.add_argument("--depth", type=int, default=20)
    q_diagnostic.add_argument("--multipv", type=int, default=4)
    q_diagnostic.add_argument("--repetitions", type=int, default=3)
    q_diagnostic.add_argument("--stability-tolerance-cp", type=int, default=15)
    q_diagnostic.add_argument("--threads", type=int, default=1)
    q_diagnostic.add_argument("--hash-mb", type=int, default=64)
    q_diagnostic.add_argument("--timeout-seconds", type=float, default=180.0)

    native_diagnostic = subparsers.add_parser(
        "native-diagnostic",
        help="capture the actual runtime Q→P boundary and materialize the frozen shadow plan",
    )
    _root_argument(native_diagnostic)
    native_diagnostic.add_argument("--run-id", required=True)
    native_diagnostic.add_argument("--sbt", required=True, type=Path)
    native_diagnostic.add_argument("--adapter-root", type=Path)
    native_diagnostic.add_argument("--artifacts", type=Path)
    native_diagnostic.add_argument("--output", type=Path)
    native_diagnostic.add_argument(
        "--provider-timeout-seconds", type=float, default=300.0
    )

    provisional_proxy = subparsers.add_parser(
        "provisional-proxy-diagnostic",
        help=(
            "materialize the frozen full chain with non-human provisional proxies; "
            "never release- or production-attribution eligible"
        ),
    )
    _root_argument(provisional_proxy)
    provisional_proxy.add_argument("--run-id", required=True)
    provisional_proxy.add_argument("--artifacts", type=Path)
    provisional_proxy.add_argument("--native-run", type=Path)
    provisional_proxy.add_argument("--oracle-q-run", type=Path)
    provisional_proxy.add_argument(
        "--baseline-probe-completion-report",
        required=True,
        type=Path,
        help="explicit immutable pre-fix closed runtime-probe report binding",
    )
    provisional_proxy.add_argument(
        "--intermediate-probe-completion-report",
        required=True,
        type=Path,
        help="explicit immutable rank/projection-only closed runtime-probe report binding",
    )
    provisional_proxy.add_argument(
        "--probe-completion-report",
        required=True,
        type=Path,
        help="explicit immutable closed runtime-probe completion report binding",
    )
    provisional_proxy.add_argument("--output", required=True, type=Path)

    cause_audit = subparsers.add_parser(
        "cause-audit",
        help="freeze, acquire, run, or compare the broad blind C cause audit",
    )
    _root_argument(cause_audit)
    cause_audit.add_argument(
        "--action",
        required=True,
        choices=("freeze", "acquire", "bind-candidate", "run", "compare"),
    )
    cause_audit.add_argument("--run-id", required=True)
    cause_audit.add_argument("--artifacts", type=Path)
    cause_audit.add_argument("--output", required=True, type=Path)
    cause_audit.add_argument("--cases", required=True, type=Path)
    cause_audit.add_argument("--manifest", type=Path)
    cause_audit.add_argument(
        "--oracle-label",
        action="append",
        default=[],
        metavar="ORACLE_ID=PATH",
        help="independent frozen oracle JSONL; repeat for each oracle",
    )
    cause_audit.add_argument("--adjudicated-label", type=Path)
    cause_audit.add_argument(
        "--contamination-exclusion",
        action="append",
        default=[],
        metavar="CASE_ID=REASON",
        help="pre-freeze case excluded from sealed selection with an auditable reason",
    )
    cause_audit.add_argument("--corpus-id", default="cause-audit-v1")
    cause_audit.add_argument("--candidate-id")
    cause_audit.add_argument(
        "--component",
        action="append",
        default=[],
        type=Path,
        help="candidate source file to bind; repeat for every in-scope production file",
    )
    cause_audit.add_argument(
        "--reference-labels",
        type=Path,
        help="optional manifest-bound labels used only to force reference engine lines",
    )
    cause_audit.add_argument(
        "--labels", type=Path, help="manifest-bound oracle labels for compare"
    )
    cause_audit.add_argument("--acquisition", type=Path)
    cause_audit.add_argument("--runtime-run", type=Path)
    cause_audit.add_argument(
        "--case-id",
        action="append",
        default=[],
        help="run only the named case; repeat for an affected-family rerun",
    )
    cause_audit.add_argument(
        "--candidate-binding",
        type=Path,
        help="post-fix immutable candidate file required before sealed production runtime",
    )
    cause_audit.add_argument(
        "--partition", choices=("explore", "sealed_confirm", "all"), default="all"
    )
    cause_audit.add_argument("--stockfish", type=Path)
    cause_audit.add_argument("--sbt", type=Path)
    cause_audit.add_argument("--adapter-root", type=Path)
    cause_audit.add_argument("--cache", type=Path)
    cause_audit.add_argument("--depth", type=int, default=18)
    cause_audit.add_argument("--multipv", type=int, default=4)
    cause_audit.add_argument("--repetitions", type=int, default=1)
    cause_audit.add_argument("--stability-tolerance-cp", type=int, default=20)
    cause_audit.add_argument("--threads", type=int, default=1)
    cause_audit.add_argument("--hash-mb", type=int, default=64)
    cause_audit.add_argument("--timeout-seconds", type=float, default=180.0)
    cause_audit.add_argument(
        "--provider-timeout-seconds", type=float, default=300.0
    )
    cause_audit.add_argument("--max-probe-rounds", type=int, default=6)
    return parser


def _root_argument(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--root",
        type=Path,
        default=Path.cwd(),
        help="judgment-evaluation directory (defaults to current directory)",
    )


def _resolve_root(value: Path) -> Path:
    root = value.resolve()
    if not (root / "schemas" / "v1").is_dir() or not (root / "corpus" / "v1").is_dir():
        raise ContractError(f"not a judgment-evaluation root: {root}")
    return root


def _write_json(path: Path, value: Any) -> None:
    resolved = path.resolve()
    resolved.parent.mkdir(parents=True, exist_ok=True)
    payload = json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True, allow_nan=False) + "\n"
    resolved.write_text(payload, encoding="utf-8", newline="\n")


def _require_output_outside_run_root(
    output: Path | None, *, artifact_root: Path, run_id: str
) -> None:
    if output is None:
        return
    resolved_output = output.resolve()
    run_root = _artifact_run_root(artifact_root, run_id)
    if resolved_output == run_root or run_root in resolved_output.parents:
        raise ContractError(
            "--output must be outside the immutable run directory; the run report is captured there automatically"
        )


def _artifact_run_root(artifact_root: Path, run_id: str) -> Path:
    """Resolve a run directory exactly as ``ArtifactStore`` does."""

    return (artifact_root.resolve() / ArtifactStore._segment(run_id)).resolve()


def _reserve_new_run_root(*, artifact_root: Path, run_id: str) -> Path:
    """Atomically reserve a single-shot run ID before provider execution."""

    run_root = _artifact_run_root(artifact_root, run_id)
    artifact_root.resolve().mkdir(parents=True, exist_ok=True)
    try:
        run_root.mkdir(exist_ok=False)
    except FileExistsError as error:
        raise ContractError(
            f"immutable run already exists for --run-id {run_id!r}; choose a new run ID"
        ) from error
    return run_root


def _q_document(
    *,
    sample: Any,
    run_id: str,
    seed: int,
    payload: Mapping[str, Any],
    producer: Mapping[str, Any],
    direction: str,
) -> dict[str, Any]:
    input_hash = sha256_json({"stage": "Q", "payload": sample.request, "upstream_artifact_hashes": []})
    return {
        "schema_version": "1.0.0",
        "stage": "Q",
        "sample_id": sample.sample_id,
        "atomic_cluster_id": sample.atomic_cluster_id,
        "corpus_version": sample.corpus_version,
        "split": sample.split,
        "run_id": run_id,
        "stage_context": {
            "design": "baseline" if direction == "input" else "ceiling",
            "experiment_id": run_id,
            "focus_stages": ["Q"],
        },
        "seed": seed,
        "input_hash": input_hash,
        "upstream_artifact_hashes": [],
        "producer": dict(producer),
        "payload": dict(payload),
    }


def _doctor(root: Path) -> Mapping[str, Any]:
    registry = SchemaRegistry(root / "schemas" / "v1")
    registry.validate_registry()
    canonicalizer = Canonicalizer(registry)
    corpus = Corpus(root / "corpus" / "v1")
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    verbalization_policy = preregistration.get("verbalization_policy")
    if not isinstance(verbalization_policy, Mapping) or verbalization_policy.get("language") != "en":
        raise ContractError("the judgment harness requires an English verbalization policy")
    corpus.verify_open_splits()
    samples = corpus.load_split("diagnostic-explore")

    sealed: dict[str, str] = {}
    for split in ("diagnostic-confirm", "fresh-confirm", "blind"):
        try:
            corpus.load_split(split)
        except SplitSealed:
            sealed[split] = "refused"
        else:
            raise ContractError(f"sealed split {split!r} opened for developer role")
        entry = corpus.manifest["splits"][split]
        path = (corpus.root / str(entry["path"])).resolve()
        if sha256_file(path) != entry["sha256"]:
            raise ContractError(f"sealed split {split!r} hash mismatch")

    reference_path = (corpus.root / str(corpus.manifest["reference_index"])).resolve()
    reference_index = read_json(reference_path)
    documents = reference_index.get("documents") if isinstance(reference_index, Mapping) else None
    if not isinstance(documents, list):
        raise ContractError("reference index has no documents array")
    document_ids = {
        row.get("document_id")
        for row in documents
        if isinstance(row, Mapping) and isinstance(row.get("document_id"), str)
    }
    if len(document_ids) != len(documents):
        raise ContractError("reference index document IDs are missing or duplicated")
    documents_by_id = {
        str(row["document_id"]): row for row in documents if isinstance(row, Mapping)
    }

    q_hashes: list[str] = []
    for sample in samples:
        if sample.labels.get("language") != "en":
            raise ContractError(f"sample {sample.sample_id} judgment labels are not bound to English")
        locator = sample.metadata.get("source_locator")
        if not isinstance(locator, Mapping) or set(locator) != {"document_id", "pdf_page"}:
            raise ContractError(f"sample {sample.sample_id} has a non-opaque source locator")
        if locator.get("document_id") not in document_ids:
            raise ContractError(f"sample {sample.sample_id} references an unknown document")
        if not isinstance(locator.get("pdf_page"), int) or locator["pdf_page"] < 1:
            raise ContractError(f"sample {sample.sample_id} has an invalid PDF page")
        source_document = documents_by_id[str(locator["document_id"])]
        if locator["pdf_page"] > int(source_document.get("page_count", 0)):
            raise ContractError(f"sample {sample.sample_id} PDF page exceeds its source")
        registered_locator = source_document.get("reconstructed_explore_locator")
        if registered_locator != locator:
            raise ContractError(
                f"sample {sample.sample_id} locator is not registered in the source index"
            )

        embedded = sample.raw.get("actual_q")
        if not isinstance(embedded, Mapping):
            raise ContractError(f"sample {sample.sample_id} has no actual_q artifact")
        payload = embedded.get("payload")
        producer = embedded.get("producer")
        if not isinstance(payload, Mapping) or not isinstance(producer, Mapping):
            raise ContractError(f"sample {sample.sample_id} has malformed actual_q")
        if payload.get("question") != sample.request.get("question"):
            raise ContractError(f"sample {sample.sample_id} Q question is not request-bound")
        if payload.get("acquisition_config") != sample.request.get("acquisition_config"):
            raise ContractError(f"sample {sample.sample_id} Q configuration is not request-bound")

        input_document = _q_document(
            sample=sample,
            run_id="doctor",
            seed=240731,
            payload=sample.request,
            producer={"component": "doctor", "version": __version__},
            direction="input",
        )
        output_document = _q_document(
            sample=sample,
            run_id="doctor",
            seed=240731,
            payload=payload,
            producer=producer,
            direction="output",
        )
        registry.validate_stage(input_document, "Q", "input")
        registry.validate_stage(output_document, "Q", "output")
        canonicalizer.assert_idempotent(output_document, *registry.schema("Q", "output"))
        q_hashes.append(sha256_json(canonicalizer.stage_document(output_document, "Q", "output")))

    fixture_schema = {
        "type": "object",
        "required": ["confidence", "rank", "missingness", "unordered"],
        "properties": {
            "confidence": {"type": "number"},
            "rank": {"type": "integer"},
            "missingness": {"type": "string"},
            "unordered": {
                "type": "array",
                "items": {"type": "string"},
                "x-canonical": {"mode": "unordered-set"},
            },
            "display": {
                "type": ["string", "null"],
                "x-canonical": {"mode": "null-equivalent-omission"},
            },
        },
        "additionalProperties": False,
    }
    fixture = {
        "confidence": 0.731,
        "rank": 2,
        "missingness": "missing",
        "unordered": ["b", "a", "a"],
        "display": None,
    }
    shaped = canonicalizer.canonicalize(fixture, fixture_schema, registry.root / "common.schema.json")
    if shaped != {
        "confidence": 0.731,
        "rank": 2,
        "missingness": "missing",
        "unordered": ["a", "a", "b"],
    }:
        raise ContractError("canonicalizer altered operational meaning or cardinality")

    plan = build_arm_plan(tuple(preregistration["oracle_chains"]))
    context_counts = Counter(arm.context for arm in plan)
    if len(plan) != 313 or len({arm.arm_id for arm in plan}) != len(plan):
        raise ContractError("registered intervention plan is incomplete or non-unique")

    with tempfile.TemporaryDirectory(prefix="chesstory-eval-doctor-") as temporary:
        store = ArtifactStore(Path(temporary), "capture-control")
        store.capture_run_document("control", {"q_canonical_hashes": q_hashes})
        store.verify()
        capture_hash = store.ledger_hash()

    return {
        "schema_version": "chesstory.eval.doctor-report.v1",
        "status": "passed",
        "schemas": 16,
        "explore_samples": len(samples),
        "explore_atomic_clusters": len({sample.atomic_cluster_id for sample in samples}),
        "sealed_access": sealed,
        "reference_documents": len(documents),
        "q_canonical_hashes": q_hashes,
        "canonicalizer_operational_fields_preserved": True,
        "artifact_capture_control_ledger_hash": capture_hash,
        "registered_arm_count": len(plan),
        "arm_context_counts": dict(sorted(context_counts.items())),
    }


def _plan(root: Path) -> Mapping[str, Any]:
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    arms = build_arm_plan(tuple(preregistration["oracle_chains"]))
    arm_rows = [
        {
            "arm_id": arm.arm_id,
            "sources": {stage: source.value for stage, source in arm.sources.items()},
            "oracle_chain": arm.oracle_chain,
            "focus_stage": arm.focus_stage,
            "context": arm.context,
        }
        for arm in arms
    ]
    return {
        "schema_version": "chesstory.eval.arm-plan.v1",
        "arm_count": len(arm_rows),
        "plan_hash": sha256_json(arm_rows),
        "context_counts": dict(sorted(Counter(row["context"] for row in arm_rows).items())),
        "arms": arm_rows,
    }


def _environment_secret(name: str) -> bytes:
    value = os.environ.get(name)
    if not isinstance(value, str) or not value:
        raise ContractError(f"required secret must be supplied through environment variable {name}")
    return value.encode("utf-8")


def _component_root(root: Path, supplied: Path | None) -> Path:
    return (supplied or root.parent).resolve(strict=True)


def _live_config_component_binding(
    config: LiveCommandConfig, *, component_root: Path
) -> dict[str, Any]:
    config.assert_bound_files_unchanged(location="candidate live-command binding")
    try:
        relative_path = config.source_path.relative_to(component_root).as_posix()
    except ValueError as error:
        raise ContractError(
            "live-command config must be a regular file beneath --component-root"
        ) from error
    paths = sorted(
        {relative_path, *config.internal_bound_paths(component_root=component_root)}
    )
    file_manifest = {
        path: {
            "sha256": sha256_file(component_root / path),
            "size": (component_root / path).stat().st_size,
        }
        for path in paths
    }
    component_sha256 = (
        str(next(iter(file_manifest.values()))["sha256"])
        if len(file_manifest) == 1
        else sha256_json(file_manifest)
    )
    return {
        "version": "chesstory.eval.live-command-config.v1",
        "sha256": component_sha256,
        "paths": paths,
        "file_manifest": file_manifest,
        "source_file_sha256": config.source_sha256,
        "document_sha256": config.document_sha256,
        "bound_files_sha256": config.bound_files_sha256,
        "security_mode": config.security_mode,
    }


def _assert_candidate_live_config_binding(
    candidate: Mapping[str, Any],
    config: LiveCommandConfig,
    *,
    component_root: Path,
) -> tuple[str, str]:
    components = candidate.get("components")
    if not isinstance(components, Mapping):
        raise ContractError("signed candidate has no component bindings")
    component = components.get("live-command-config")
    if not isinstance(component, Mapping):
        raise ContractError(
            "sealed candidate does not bind a live-command-config component"
        )
    expected = _live_config_component_binding(config, component_root=component_root)
    for key, value in expected.items():
        if component.get(key) != value:
            raise ContractError(
                f"signed candidate live-command-config {key} binding mismatch"
            )
    file_manifest = component.get("file_manifest")
    if not isinstance(file_manifest, Mapping) or file_manifest != expected["file_manifest"]:
        raise ContractError(
            "signed candidate live-command-config file manifest is not exact"
        )
    external = candidate.get("external_executable_binding")
    expected_external = config.external_executable_binding(
        component_root=component_root
    )
    if not isinstance(external, Mapping) or dict(external) != expected_external:
        raise ContractError(
            "signed candidate external executable binding is not exact"
        )
    return sha256_json(component), sha256_json(external)


def _custodian_state(
    root_corpus: Corpus,
) -> tuple[Path, str, Mapping[str, str]]:
    raw = os.environ.get("CHESSTORY_EVAL_CUSTODIAN_STATE_ROOT")
    if not isinstance(raw, str) or not raw.strip():
        raise ContractError(
            "sealed execution requires CHESSTORY_EVAL_CUSTODIAN_STATE_ROOT"
        )
    supplied_state_root = Path(raw)
    access_path = root_corpus.manifest_resource_paths(release=True)["access_policy"]
    access_policy = read_json(access_path)
    if not isinstance(access_policy, Mapping):
        raise ContractError("release access policy must be an object")
    state = access_policy.get("custodian_state")
    if not isinstance(state, Mapping) or set(state) != {
        "environment_variable",
        "state_id",
        "claim_namespace",
        "root_binding_sha256",
        "storage_attestation_path",
        "storage_attestation_sha256",
    }:
        raise ContractError("release access policy has no custodian_state")
    state_id = state.get("state_id")
    if not isinstance(state_id, str) or not state_id.strip():
        raise ContractError("release custodian state_id is invalid")
    storage_path = state.get("storage_attestation_path")
    storage_sha256 = state.get("storage_attestation_sha256")
    if not isinstance(storage_path, str) or not isinstance(storage_sha256, str):
        raise ContractError("release custodian storage attestation binding is invalid")
    storage_binding = load_custodian_storage_attestation(
        supplied_state_root,
        state_id,
        relative_path=storage_path,
        expected_sha256=storage_sha256,
    )
    return supplied_state_root.resolve(strict=True), state_id, storage_binding


def _verified_candidate_binding(
    *,
    corpus: Corpus,
    preregistration: Mapping[str, Any],
    candidate_path: Path,
    key: bytes,
    split: str,
    component_root: Path,
    custodian_state_root: Path,
) -> tuple[Mapping[str, Any], str]:
    document = verify_signed_candidate_manifest(candidate_path.resolve(), key)
    candidate_hash = sha256_file(candidate_path.resolve())
    corpus_binding = document.get("corpus_binding")
    if not isinstance(corpus_binding, Mapping):
        raise ContractError("signed candidate manifest has no corpus binding")
    if corpus.manifest.get("qualification") != "release-qualified":
        raise ContractError(
            "sealed/release execution requires a release-qualified current corpus"
        )
    if corpus_binding.get("corpus_version") != corpus.corpus_version:
        raise ContractError("candidate manifest corpus version does not match current corpus")
    current_manifest_hash = sha256_file(corpus.root / "manifest.json")
    if corpus_binding.get("manifest_sha256") != current_manifest_hash:
        raise ContractError("candidate manifest does not bind current corpus manifest bytes")
    split_hashes = corpus_binding.get("split_sha256")
    if not isinstance(split_hashes, Mapping) or split_hashes.get(split) != corpus.split_hash(split):
        raise ContractError("candidate manifest does not bind the requested split bytes")
    if document.get("seed") != preregistration.get("seed"):
        raise ContractError("candidate manifest seed does not match preregistration")
    verify_candidate_execution_bindings(
        document,
        component_root=component_root,
        corpus=corpus,
        custodian_state_root=custodian_state_root,
    )
    return document, candidate_hash


def _validate_fresh_confirm_attestation(
    *,
    path: Path,
    key: bytes,
    candidate_manifest_hash: str,
    corpus: Corpus,
    preregistration: Mapping[str, Any],
    decision_path: Path,
    artifact_root: Path,
    custodian_root: Path,
    custodian_state_id: str,
    custodian_storage_binding: Mapping[str, Any],
) -> None:
    attestation = verify_signed_run_attestation(path.resolve(), key)
    if attestation.get("candidate_manifest_sha256") != candidate_manifest_hash:
        raise ContractError("fresh-confirm attestation is for a different candidate")
    split = attestation.get("split")
    if not isinstance(split, Mapping):
        raise ContractError("fresh-confirm attestation has no split binding")
    if split.get("id") != "fresh-confirm":
        raise ContractError("blind opening requires a fresh-confirm attestation")
    if split.get("input_sha256") != corpus.split_hash("fresh-confirm"):
        raise ContractError("fresh-confirm attestation binds different confirm bytes")
    fresh_run_id = attestation.get("run_id")
    if not isinstance(fresh_run_id, str) or not fresh_run_id.strip():
        raise ContractError("fresh-confirm attestation has no run_id")
    fresh_run_root = artifact_root / ArtifactStore._segment(fresh_run_id)
    if not fresh_run_root.is_dir():
        raise ContractError("fresh-confirm authoritative artifact run is unavailable")
    fresh_store = ArtifactStore(
        artifact_root,
        fresh_run_id,
        custodian_state_root=custodian_root,
        custodian_state_id=custodian_state_id,
        custodian_storage_binding=custodian_storage_binding,
    )
    verify_signed_fresh_confirm_qualification(
        decision_path.resolve(),
        key,
        pre_unblinding_attestation_path=path.resolve(),
        candidate_manifest_hash=candidate_manifest_hash,
        corpus=corpus,
        preregistration=preregistration,
        artifact_store=fresh_store,
    )


def _run(root: Path, arguments: argparse.Namespace) -> Mapping[str, Any]:
    registry = SchemaRegistry(root / "schemas" / "v1")
    canonicalizer = Canonicalizer(registry)
    corpus = Corpus(root / "corpus" / "v1")
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    release_mode = corpus.split_state(arguments.split) == "sealed"
    component_root = _component_root(root, arguments.component_root)
    live_config = (
        load_live_command_config(arguments.live_command_config)
        if arguments.live_command_config is not None
        else None
    )
    if release_mode and (arguments.stage_replay or arguments.evaluator_replay):
        raise ContractError(
            "sealed/release execution forbids stage and evaluator replay artifacts"
        )
    if release_mode and live_config is None:
        raise ContractError(
            "sealed/release execution requires --live-command-config before split opening"
        )
    if live_config is not None and arguments.evaluator_replay:
        raise ContractError(
            "choose live evaluator routing or diagnostic evaluator replay, not both"
        )
    if release_mode:
        assert live_config is not None
        live_config.assert_complete_stage_routes(
            tuple(str(value) for value in preregistration["oracle_chains"])
        )
    custodian_root: Path | None = None
    custodian_state_id: str | None = None
    custodian_storage_binding: Mapping[str, Any] | None = None
    if release_mode or arguments.candidate_manifest is not None:
        (
            custodian_root,
            custodian_state_id,
            custodian_storage_binding,
        ) = _custodian_state(corpus)

    candidate_hash = arguments.candidate_manifest_hash
    signed_candidate: Mapping[str, Any] | None = None
    signing_key: bytes | None = None
    if arguments.candidate_manifest and arguments.candidate_manifest_hash:
        raise ContractError(
            "choose --candidate-manifest or diagnostic --candidate-manifest-hash, not both"
        )
    if arguments.candidate_manifest:
        signing_key = _environment_secret("CHESSTORY_EVAL_HMAC_KEY")
        if custodian_root is None:
            raise ContractError("signed candidate requires a frozen custodian state root")
        signed_candidate, candidate_hash = _verified_candidate_binding(
            corpus=corpus,
            preregistration=preregistration,
            candidate_path=arguments.candidate_manifest,
            key=signing_key,
            split=arguments.split,
            component_root=component_root,
            custodian_state_root=custodian_root,
        )
    if release_mode and signed_candidate is None:
        raise ContractError(
            "sealed runs require --candidate-manifest verified with CHESSTORY_EVAL_HMAC_KEY"
        )
    if release_mode and not isinstance(candidate_hash, str):
        raise ContractError("sealed candidate hash must be derived from a signed manifest")

    candidate_live_config_hash: str | None = None
    candidate_external_executable_hash: str | None = None
    if release_mode:
        assert signed_candidate is not None and live_config is not None
        (
            candidate_live_config_hash,
            candidate_external_executable_hash,
        ) = _assert_candidate_live_config_binding(
            signed_candidate,
            live_config,
            component_root=component_root,
        )

    store = ArtifactStore(
        artifact_root,
        arguments.run_id,
        custodian_state_root=custodian_root,
        custodian_state_id=custodian_state_id,
        custodian_storage_binding=custodian_storage_binding,
    )

    live_config_binding: Mapping[str, Any] | None = None
    if live_config is not None:
        live_config_artifact_path = "pre-unblinding-live-command-config.json"
        live_config_artifact_hash = store.capture_run_document(
            "pre-unblinding-live-command-config", live_config.document
        )
        live_config_binding = {
            **live_config.binding(
                artifact_path=live_config_artifact_path,
                artifact_sha256=live_config_artifact_hash,
            ),
            "candidate_component_sha256": candidate_live_config_hash,
            "candidate_external_executable_sha256": candidate_external_executable_hash,
            "frozen_before_split_open": True,
        }
        store.record_access(
            role="experiment-runner",
            artifact_hash=live_config_artifact_hash,
            action="freeze-live-command-config",
            reason=f"run:{arguments.run_id};split:{arguments.split}",
        )

    endpoint_policy: FrozenEndpointPolicy | None = None
    if arguments.endpoint_policy:
        if not isinstance(candidate_hash, str):
            raise ContractError("endpoint policy requires a candidate-manifest binding")
        endpoint_document = read_json(arguments.endpoint_policy.resolve())
        if not isinstance(endpoint_document, Mapping):
            raise ContractError("endpoint policy file must contain an object")
        endpoint_policy = FrozenEndpointPolicy(
            endpoint_document,
            expected_binding={
                "run_id": arguments.run_id,
                "candidate_manifest_sha256": candidate_hash,
                "corpus_version": corpus.corpus_version,
                "split": arguments.split,
                "split_sha256": corpus.split_hash(arguments.split),
                "seed": int(preregistration["seed"]),
            },
            release_mode=release_mode,
            role_assignments=(
                corpus.release_role_assignments(
                    split=arguments.split,
                    minimum_held_out_raters=int(
                        preregistration["minimum_effective_counts"][
                            "held_out_human_raters"
                        ]
                    ),
                )
                if release_mode
                else None
            ),
            required_held_out_raters=(
                int(
                    preregistration["minimum_effective_counts"][
                        "held_out_human_raters"
                    ]
                )
                if release_mode
                else 0
            ),
        )
        captured_policy_hash = store.capture_run_document(
            "pre-unblinding-endpoint-policy", endpoint_document
        )
        if captured_policy_hash != endpoint_policy.document_sha256:
            raise ContractError("captured endpoint policy hash changed")
        store.capture_run_document(
            "pre-unblinding-oracle-graph", endpoint_document["oracle_graph"]
        )
        store.capture_run_document(
            "pre-unblinding-allowed-set",
            endpoint_document["allowed_set_by_sample"],
        )
        store.capture_run_document(
            "pre-unblinding-execution-manifest",
            {
                "schema_version": "chesstory.eval.execution-manifest.v1",
                "run_id": arguments.run_id,
                "candidate_manifest_sha256": candidate_hash,
                "corpus_version": corpus.corpus_version,
                "split": arguments.split,
                "split_input_sha256": corpus.split_hash(arguments.split),
                "endpoint_policy_sha256": endpoint_policy.document_sha256,
                "plan_sha256": _plan(root)["plan_hash"],
                "registered_arm_ids": [
                    arm.arm_id
                    for arm in build_arm_plan(
                        tuple(str(value) for value in preregistration["oracle_chains"])
                    )
                ],
                "sample_ids": sorted(endpoint_document["allowed_set_by_sample"]),
                "live_command_config": live_config_binding,
                "frozen_before_split_open": True,
            },
        )
    elif release_mode:
        raise ContractError(
            "sealed/release E requires --endpoint-policy frozen before unblinding"
        )

    if release_mode:
        if endpoint_policy is None or not arguments.adjudication:
            raise ContractError(
                "sealed execution requires source-blind --adjudication NAME=SOURCE_PATH artifacts"
            )
        supplied_adjudications = _adjudication_path_map(arguments.adjudication)
        seen_adjudicator_pairs: set[tuple[str, str]] = set()
        for name, source_path in sorted(supplied_adjudications.items()):
            adjudication = read_json(Path(source_path).resolve(strict=True))
            if not isinstance(adjudication, Mapping) or adjudication.get(
                "schema_version"
            ) != "chesstory.eval.blind-adjudication.v1":
                raise ContractError(f"adjudication {name!r} has an unsupported schema")
            sample_id = adjudication.get("sample_id")
            adjudicator_id = adjudication.get("adjudicator_id")
            if not isinstance(sample_id, str) or not isinstance(adjudicator_id, str):
                raise ContractError(f"adjudication {name!r} identity is incomplete")
            expected_ids = endpoint_policy.out_of_set_adjudicator_ids_by_sample.get(
                sample_id
            )
            if expected_ids is None or adjudicator_id not in expected_ids:
                raise ContractError(
                    f"adjudication {name!r} identity is not frozen for its sample"
                )
            pair = (sample_id, adjudicator_id)
            if pair in seen_adjudicator_pairs:
                raise ContractError(f"duplicate adjudication pair: {pair!r}")
            seen_adjudicator_pairs.add(pair)
            digest = store.capture_run_document(f"adjudication-{name}", adjudication)
            store.record_access(
                role=f"out-of-set-adjudicator:{adjudicator_id}",
                artifact_hash=digest,
                action="source-blind-adjudication",
                reason=f"sample:{sample_id}",
            )
        expected_pairs = {
            (sample_id, adjudicator_id)
            for sample_id, identities in endpoint_policy.out_of_set_adjudicator_ids_by_sample.items()
            for adjudicator_id in identities
        }
        if seen_adjudicator_pairs != expected_pairs:
            raise ContractError("sealed adjudication set differs from the frozen roster")
    elif arguments.adjudication:
        raise ContractError("--adjudication is accepted only for sealed execution")

    if arguments.split == "blind":
        if (
            arguments.fresh_confirm_attestation is None
            or arguments.fresh_confirm_decision is None
            or signing_key is None
        ):
            raise ContractError(
                "blind split requires both --fresh-confirm-attestation and --fresh-confirm-decision"
            )
        _validate_fresh_confirm_attestation(
            path=arguments.fresh_confirm_attestation,
            key=signing_key,
            candidate_manifest_hash=str(candidate_hash),
            corpus=corpus,
            preregistration=preregistration,
            decision_path=arguments.fresh_confirm_decision,
            artifact_root=artifact_root,
            custodian_root=custodian_root,
            custodian_state_id=custodian_state_id,
            custodian_storage_binding=custodian_storage_binding,
        )
    elif (
        arguments.fresh_confirm_attestation is not None
        or arguments.fresh_confirm_decision is not None
    ):
        raise ContractError("fresh-confirm attestation/decision are accepted only for a blind run")

    if release_mode:
        role = os.environ.get("CHESSTORY_EVAL_ROLE")
        token = os.environ.get("CHESSTORY_EVAL_SPLIT_TOKEN")
        if not role or not token:
            raise ContractError(
                "sealed split role/token must be supplied through CHESSTORY_EVAL_ROLE and CHESSTORY_EVAL_SPLIT_TOKEN"
            )
        samples = corpus.load_split(
            arguments.split,
            role=role,
            unlock_token=token,
            access_store=store,
            access_reason=f"sealed {arguments.split} run bound to {candidate_hash}",
            candidate_manifest_hash=str(candidate_hash),
        )
    else:
        samples = corpus.load_split(arguments.split)
    if endpoint_policy is not None:
        endpoint_policy.assert_exact_samples(samples)

    if release_mode:
        assert live_config is not None
        adapter = AvailabilityGuard(
            CommandStageAdapter(
                live_config.stage_command_mapping(),
                release_mode=True,
            ),
            require_explicit_live=True,
        )
    else:
        adapters: list[Any] = []
        if live_config is not None:
            adapters.append(CommandStageAdapter(live_config.stage_command_mapping()))
        if arguments.stage_replay:
            adapters.append(
                ReplayStageAdapter(
                    [path.resolve() for path in arguments.stage_replay]
                )
            )
        adapters.append(
            EmbeddedAcquisitionAdapter(corpus.embedded_actual_q_packs(samples))
        )
        adapter = AvailabilityGuard(CompositeAdapter(adapters))
    if live_config is not None:
        evaluator = CommandEvaluationAdapter(
            live_config.evaluator_route,
            usefulness_threshold=float(preregistration["usefulness_threshold"]),
            endpoint_policy=endpoint_policy,
            release_mode=release_mode,
            access_store=store,
        )
    elif arguments.evaluator_replay:
        evaluator = ReplayEvaluationAdapter(
            [path.resolve() for path in arguments.evaluator_replay],
            usefulness_threshold=float(preregistration["usefulness_threshold"]),
            endpoint_policy=endpoint_policy,
            release_mode=False,
            access_store=None,
        )
    else:
        evaluator = UnavailableEvaluator(
            "no source-blind evaluator route/artifacts were configured"
        )
    controls = read_json(arguments.foundation_controls.resolve()) if arguments.foundation_controls else {}
    runner = ExperimentRunner(
        registry=registry,
        canonicalizer=canonicalizer,
        adapter=adapter,
        evaluator=evaluator,
        store=store,
        context=RunContext(
            run_id=arguments.run_id,
            seed=int(preregistration["seed"]),
            corpus_version=corpus.corpus_version,
            split=arguments.split,
            artifact_root=artifact_root,
            stage_context={},
            candidate_manifest_hash=candidate_hash,
        ),
        preregistration=preregistration,
        foundation_control_results=controls,
        split_hash=corpus.split_hash(arguments.split),
        live_command_config_binding=live_config_binding,
    )
    return runner.run_bottleneck_experiment(samples, defer_analysis=release_mode)


def _freeze_candidate(root: Path, arguments: argparse.Namespace) -> Mapping[str, Any]:
    corpus = Corpus(root / "corpus" / "v1")
    custodian_root, _, _ = _custodian_state(corpus)
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    component_root = _component_root(root, arguments.component_root)
    live_config = load_live_command_config(arguments.live_command_config)
    live_config.assert_complete_stage_routes(
        tuple(str(value) for value in preregistration["oracle_chains"])
    )
    supplied = read_json(arguments.components.resolve())
    if not isinstance(supplied, Mapping):
        raise ContractError("candidate components file must contain an object")
    components = supplied.get("components", supplied)
    if not isinstance(components, Mapping):
        raise ContractError("candidate components must be an object")
    frozen_components = dict(components)
    supplied_live_binding = frozen_components.get("live-command-config")
    expected_live_binding = _live_config_component_binding(
        live_config, component_root=component_root
    )
    if supplied_live_binding is not None:
        if not isinstance(supplied_live_binding, Mapping) or any(
            supplied_live_binding.get(key) != value
            for key, value in expected_live_binding.items()
        ):
            raise ContractError(
                "components live-command-config binding conflicts with --live-command-config"
            )
    frozen_components["live-command-config"] = expected_live_binding
    external_executable_binding = live_config.external_executable_binding(
        component_root=component_root
    )
    split_entries = corpus.manifest.get("splits")
    if not isinstance(split_entries, Mapping):
        raise ContractError("corpus manifest splits are missing")
    split_hashes = {str(split): corpus.split_hash(str(split)) for split in split_entries}
    document = build_candidate_manifest(
        candidate_id=arguments.candidate_id,
        frozen_at=utc_now(),
        components=frozen_components,
        seed=int(preregistration["seed"]),
        corpus_root=corpus.root,
        split_hashes=split_hashes,
        component_root=component_root,
        custodian_state_root=custodian_root,
        external_executable_binding=external_executable_binding,
    )
    digest = write_signed_candidate_manifest(
        arguments.output.resolve(),
        document,
        _environment_secret("CHESSTORY_EVAL_HMAC_KEY"),
        arguments.key_id,
    )
    return {
        "status": "candidate-frozen",
        "candidate_id": arguments.candidate_id,
        "signed_manifest_sha256": digest,
        "live_command_config": {
            "source_file_sha256": live_config.source_sha256,
            "document_sha256": live_config.document_sha256,
            "component_sha256": sha256_json(
                document["components"]["live-command-config"]
            ),
            "external_executable_sha256": sha256_json(
                document["external_executable_binding"]
            ),
        },
        "output": str(arguments.output.resolve()),
    }


def _adjudication_path_map(values: Sequence[str]) -> dict[str, str]:
    result: dict[str, str] = {}
    for value in values:
        if "=" not in value:
            raise ContractError("--adjudication must use NAME=RUN_PATH")
        name, path = value.split("=", 1)
        if not name.strip() or not path.strip():
            raise ContractError("--adjudication NAME and RUN_PATH must be non-empty")
        if name in result:
            raise ContractError(f"duplicate adjudication name: {name!r}")
        result[name] = path
    if not result:
        raise ContractError("at least one --adjudication NAME=RUN_PATH is required")
    return result


def _attest_run(root: Path, arguments: argparse.Namespace) -> Mapping[str, Any]:
    key = _environment_secret("CHESSTORY_EVAL_HMAC_KEY")
    corpus = Corpus(root / "corpus" / "v1")
    if corpus.split_state(arguments.split) != "sealed":
        raise ContractError("attest-run is reserved for sealed two-phase runs")
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    custodian_root, custodian_state_id, custodian_storage_binding = _custodian_state(
        corpus
    )
    candidate, candidate_hash = _verified_candidate_binding(
        corpus=corpus,
        preregistration=preregistration,
        candidate_path=arguments.candidate_manifest,
        key=key,
        split=arguments.split,
        component_root=_component_root(root, arguments.component_root),
        custodian_state_root=custodian_root,
    )
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    store = ArtifactStore(
        artifact_root,
        arguments.run_id,
        custodian_state_root=custodian_root,
        custodian_state_id=custodian_state_id,
        custodian_storage_binding=custodian_storage_binding,
    )
    components = candidate.get("components")
    weights = components.get("weights") if isinstance(components, Mapping) else None
    live_component = (
        components.get("live-command-config")
        if isinstance(components, Mapping)
        else None
    )
    external_executables = candidate.get("external_executable_binding")
    if not isinstance(live_component, Mapping) or not isinstance(
        external_executables, Mapping
    ):
        raise ContractError(
            "verified candidate lacks executable command bindings"
        )
    provider_io_required = arguments.raw_provider_io_required or (
        isinstance(weights, Mapping) and weights.get("raw_provider_io_required") is True
    )
    document = build_run_attestation(
        artifact_store=store,
        attested_at=utc_now(),
        candidate_manifest_hash=candidate_hash,
        candidate_live_command_component_hash=sha256_json(live_component),
        candidate_external_executable_hash=sha256_json(external_executables),
        split_id=arguments.split,
        split_hash=corpus.split_hash(arguments.split),
        corpus_version=corpus.corpus_version,
        execution_manifest_path=arguments.execution_manifest_path,
        execution_bundle_path=arguments.execution_bundle_path,
        endpoint_policy_path=arguments.endpoint_policy_path,
        oracle_graph_path=arguments.oracle_graph_path,
        allowed_set_path=arguments.allowed_set_path,
        adjudication_paths=_adjudication_path_map(arguments.adjudication),
        raw_provider_io_required=provider_io_required,
    )
    digest = write_signed_run_attestation(
        arguments.output.resolve(),
        document,
        key,
        arguments.key_id,
        artifact_store=store,
    )
    return {
        "status": "run-attested",
        "run_id": arguments.run_id,
        "candidate_manifest_sha256": candidate_hash,
        "signed_attestation_sha256": digest,
        "output": str(arguments.output.resolve()),
    }


def _finalize_run(root: Path, arguments: argparse.Namespace) -> Mapping[str, Any]:
    key = _environment_secret("CHESSTORY_EVAL_HMAC_KEY")
    corpus = Corpus(root / "corpus" / "v1")
    if corpus.split_state(arguments.split) != "sealed":
        raise ContractError("finalize-run is reserved for sealed two-phase runs")
    preregistration = read_json(corpus.root / "preregistration.json")
    if not isinstance(preregistration, Mapping):
        raise ContractError("preregistration must be an object")
    custodian_root, custodian_state_id, custodian_storage_binding = _custodian_state(
        corpus
    )
    _, candidate_hash = _verified_candidate_binding(
        corpus=corpus,
        preregistration=preregistration,
        candidate_path=arguments.candidate_manifest,
        key=key,
        split=arguments.split,
        component_root=_component_root(root, arguments.component_root),
        custodian_state_root=custodian_root,
    )
    attestation = verify_signed_run_attestation(
        arguments.pre_unblinding_attestation.resolve(), key
    )
    if attestation.get("run_id") != arguments.run_id:
        raise ContractError("pre-unblinding attestation run_id mismatch")
    if attestation.get("candidate_manifest_sha256") != candidate_hash:
        raise ContractError("pre-unblinding attestation candidate mismatch")
    split_binding = attestation.get("split")
    if not isinstance(split_binding, Mapping) or split_binding != {
        "id": arguments.split,
        "input_sha256": corpus.split_hash(arguments.split),
    }:
        raise ContractError("pre-unblinding attestation split binding mismatch")
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    store = ArtifactStore(
        artifact_root,
        arguments.run_id,
        custodian_state_root=custodian_root,
        custodian_state_id=custodian_state_id,
        custodian_storage_binding=custodian_storage_binding,
    )
    verify_attested_run_snapshot(store, attestation)
    if (store.run_root / "experiment-report.json").exists() or (
        store.run_root / "fresh-confirm-equivalence.json"
    ).exists():
        raise ContractError("sealed run was already finalized")
    bundle_artifact = attestation.get("execution_bundle_artifact")
    if not isinstance(bundle_artifact, Mapping):
        raise ContractError("attestation has no execution-bundle artifact")
    bundle_path = bundle_artifact.get("path")
    if not isinstance(bundle_path, str):
        raise ContractError("attested execution-bundle path is invalid")
    bundle_file = store.run_root / bundle_path
    if not bundle_file.is_file() or sha256_file(bundle_file) != bundle_artifact.get(
        "sha256"
    ):
        raise ContractError("attested execution-bundle file changed")
    bundle = read_json(bundle_file)
    if not isinstance(bundle, Mapping) or sha256_json(bundle) != bundle_artifact.get(
        "document_sha256"
    ):
        raise ContractError("attested execution-bundle document changed")

    registry = SchemaRegistry(root / "schemas" / "v1")
    evaluator = UnavailableEvaluator("finalization performs no evaluation")
    evaluator.endpoint_policy_hash = bundle.get("endpoint_policy_sha256")  # type: ignore[attr-defined]
    evaluator.endpoint_allowed_set_source = bundle.get(  # type: ignore[attr-defined]
        "endpoint_allowed_set_source"
    )
    runner = ExperimentRunner(
        registry=registry,
        canonicalizer=Canonicalizer(registry),
        adapter=AvailabilityGuard(
            CompositeAdapter([EmbeddedAcquisitionAdapter({})])
        ),
        evaluator=evaluator,
        store=store,
        context=RunContext(
            run_id=arguments.run_id,
            seed=int(preregistration["seed"]),
            corpus_version=corpus.corpus_version,
            split=arguments.split,
            artifact_root=artifact_root,
            stage_context={},
            candidate_manifest_hash=candidate_hash,
        ),
        preregistration=preregistration,
        split_hash=corpus.split_hash(arguments.split),
        live_command_config_binding=(
            bundle.get("live_command_config")
            if isinstance(bundle.get("live_command_config"), Mapping)
            else None
        ),
    )
    report = runner.finalize_execution_bundle(
        bundle,
        verified_pre_unblinding_attestation=attestation,
    )
    if arguments.output:
        _write_json(arguments.output, report)
    return report


def _qualify_fresh_confirm(
    root: Path, arguments: argparse.Namespace
) -> Mapping[str, Any]:
    key = _environment_secret("CHESSTORY_EVAL_HMAC_KEY")
    corpus = Corpus(root / "corpus" / "v1")
    preregistration = read_json(corpus.root / "preregistration.json")
    if not isinstance(preregistration, Mapping):
        raise ContractError("preregistration must be an object")
    custodian_root, custodian_state_id, custodian_storage_binding = _custodian_state(
        corpus
    )
    _, candidate_hash = _verified_candidate_binding(
        corpus=corpus,
        preregistration=preregistration,
        candidate_path=arguments.candidate_manifest,
        key=key,
        split="fresh-confirm",
        component_root=_component_root(root, arguments.component_root),
        custodian_state_root=custodian_root,
    )
    pre_attestation = verify_signed_run_attestation(
        arguments.pre_unblinding_attestation.resolve(), key
    )
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    store = ArtifactStore(
        artifact_root,
        arguments.run_id,
        custodian_state_root=custodian_root,
        custodian_state_id=custodian_state_id,
        custodian_storage_binding=custodian_storage_binding,
    )
    document = build_fresh_confirm_qualification(
        qualified_at=utc_now(),
        pre_unblinding_attestation_path=arguments.pre_unblinding_attestation,
        pre_unblinding_attestation=pre_attestation,
        artifact_store=store,
        candidate_manifest_hash=candidate_hash,
        corpus=corpus,
        preregistration=preregistration,
        experiment_report_path=arguments.experiment_report,
        equivalence_path=arguments.equivalence,
        release_evidence_path=arguments.release_evidence,
    )
    qualification_output = arguments.output.resolve()
    try:
        qualification_output.relative_to(store.run_root.resolve())
    except ValueError:
        pass
    else:
        raise ContractError(
            "fresh-confirm qualification must be stored outside the attested run tree"
        )
    digest = write_signed_fresh_confirm_qualification(
        qualification_output, document, key, arguments.key_id
    )
    return {
        "status": "fresh-confirm-qualified",
        "run_id": arguments.run_id,
        "candidate_manifest_sha256": candidate_hash,
        "signed_qualification_sha256": digest,
        "output": str(qualification_output),
    }


def _root_move_scores(
    pack: Mapping[str, Any], *, field: str = "candidate_lines"
) -> dict[str, Mapping[str, Any]]:
    result: dict[str, Mapping[str, Any]] = {}
    lines = pack.get(field, [])
    if isinstance(lines, list):
        for line in lines:
            if isinstance(line, Mapping) and isinstance(line.get("root_move_uci"), str):
                evaluation = line.get("evaluation")
                if isinstance(evaluation, Mapping):
                    result[str(line["root_move_uci"])] = dict(evaluation)
    return result


def _q_diagnostic(root: Path, arguments: argparse.Namespace) -> Mapping[str, Any]:
    from .stockfish import acquire_stockfish_evidence

    registry = SchemaRegistry(root / "schemas" / "v1")
    canonicalizer = Canonicalizer(registry)
    corpus = Corpus(root / "corpus" / "v1")
    samples = corpus.load_split("diagnostic-explore")
    preregistration = read_json(root / "corpus" / "v1" / "preregistration.json")
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    _require_output_outside_run_root(
        arguments.output, artifact_root=artifact_root, run_id=arguments.run_id
    )
    executable_hash = sha256_file(arguments.stockfish.resolve())
    _reserve_new_run_root(artifact_root=artifact_root, run_id=arguments.run_id)
    store = ArtifactStore(artifact_root, arguments.run_id)
    rows: list[dict[str, Any]] = []

    for sample in samples:
        question = sample.request.get("question")
        original_config = sample.request.get("acquisition_config")
        if not isinstance(question, Mapping) or not isinstance(original_config, Mapping):
            raise ContractError(f"sample {sample.sample_id} has no Q request")
        config = dict(original_config)
        config.update(
            {
                "depth": arguments.depth,
                "multipv": arguments.multipv,
                "repetitions": arguments.repetitions,
                "stability_tolerance_cp": arguments.stability_tolerance_cp,
                "engine_options": {"Threads": arguments.threads, "Hash": arguments.hash_mb},
            }
        )
        result = acquire_stockfish_evidence(
            arguments.stockfish.resolve(),
            question,
            config,
            timeout_seconds=arguments.timeout_seconds,
        )
        pack = result.engine_evidence_pack
        producer = {
            "component": "stockfish-q-acquisition",
            "version": __version__,
            "build_hash": executable_hash,
            "configuration_hash": sha256_json(config),
        }
        input_payload = {"question": dict(question), "acquisition_config": config}
        diagnostic_sample = type(sample)(
            sample_id=sample.sample_id,
            atomic_cluster_id=sample.atomic_cluster_id,
            corpus_version=sample.corpus_version,
            split=sample.split,
            request=input_payload,
            labels=sample.labels,
            metadata=sample.metadata,
            raw=sample.raw,
        )
        input_document = _q_document(
            sample=diagnostic_sample,
            run_id=arguments.run_id,
            seed=int(preregistration["seed"]),
            payload=input_payload,
            producer={"component": "q-diagnostic-runner", "version": __version__},
            direction="input",
        )
        output_document = _q_document(
            sample=diagnostic_sample,
            run_id=arguments.run_id,
            seed=int(preregistration["seed"]),
            payload=pack,
            producer=producer,
            direction="output",
        )
        registry.validate_stage(input_document, "Q", "input")
        registry.validate_stage(output_document, "Q", "output")
        canonical = canonicalizer.stage_document(output_document, "Q", "output")
        registry.validate_stage(canonical, "Q", "output")
        canonicalizer.assert_idempotent(output_document, *registry.schema("Q", "output"))
        capture_hashes = store.capture_stage(
            sample_id=sample.sample_id,
            arm_id="stable-q-diagnostic",
            stage="Q",
            raw_input=input_document,
            raw_output=pack,
            validated_output=output_document,
            canonical_output=canonical,
            provider_io=result.raw_provider_io,
            metadata={
                "source": "richer-stable-q-diagnostic",
                "input_hash": input_document["input_hash"],
                "canonical_output_hash": sha256_json(canonical),
                "schema_id": registry.schema("Q", "output")[0].get("$id"),
            },
        )

        actual_entry = sample.raw.get("actual_q")
        actual_pack = actual_entry.get("payload") if isinstance(actual_entry, Mapping) else {}
        actual_scores = _root_move_scores(actual_pack if isinstance(actual_pack, Mapping) else {})
        stable_ranked_scores = _root_move_scores(pack)
        stable_forced_scores = _root_move_scores(pack, field="forced_required_lines")
        stable_scores = {**stable_forced_scores, **stable_ranked_scores}
        requested = [question.get("played_move_uci"), *question.get("requested_reference_moves_uci", [])]
        requested_moves = [move for move in requested if isinstance(move, str)]
        rows.append(
            {
                "sample_id": sample.sample_id,
                "atomic_cluster_id": sample.atomic_cluster_id,
                "requested_moves": requested_moves,
                "actual_q_scores": actual_scores,
                "stable_q_scores": stable_scores,
                "stable_q_ranked_scores": stable_ranked_scores,
                "stable_q_forced_reference_scores": stable_forced_scores,
                "actual_q_coverage": {
                    move: move in actual_scores for move in requested_moves
                },
                "stable_q_coverage": {
                    move: move in stable_scores for move in requested_moves
                },
                "stable_q_ranked_coverage": {
                    move: move in stable_ranked_scores for move in requested_moves
                },
                "stable_q_diagnostics": dict(result.diagnostics),
                "canonical_output_hash": sha256_json(canonical),
                "capture_hashes": dict(capture_hashes),
            }
        )

    cluster_count = len({sample.atomic_cluster_id for sample in samples})
    required_clusters = int(preregistration["minimum_effective_counts"]["atomic_clusters"])
    report = {
        "schema_version": "chesstory.eval.q-diagnostic-report.v1",
        "run_id": arguments.run_id,
        "corpus_version": corpus.corpus_version,
        "split": "diagnostic-explore",
        "stockfish_sha256": executable_hash,
        "configuration": {
            "depth": arguments.depth,
            "multipv": arguments.multipv,
            "repetitions": arguments.repetitions,
            "stability_tolerance_cp": arguments.stability_tolerance_cp,
            "threads": arguments.threads,
            "hash_mb": arguments.hash_mb,
        },
        "sample_count": len(samples),
        "atomic_cluster_count": cluster_count,
        "status": "diagnostic-only",
        "inference": {
            "status": "indeterminate",
            "production_bottleneck": None,
            "reason": (
                f"Q diagnostics do not identify a production stage; {cluster_count} clusters "
                f"are below the preregistered minimum {required_clusters}"
            ),
        },
        "rows": rows,
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    store.capture_run_document("q-diagnostic-report", report)
    store.verify()
    return report


def _native_runtime_request(sample: Any) -> dict[str, Any]:
    embedded = sample.raw.get("actual_q")
    if not isinstance(embedded, Mapping):
        raise ContractError(f"sample {sample.sample_id} has no actual_q artifact")
    payload = embedded.get("payload")
    if not isinstance(payload, Mapping):
        raise ContractError(f"sample {sample.sample_id} has a malformed actual_q payload")
    if payload.get("probes") not in (None, []):
        raise ContractError(
            f"sample {sample.sample_id} has Q probes without a frozen runtime probe-result mapping"
        )
    question = payload.get("question")
    candidate_lines = payload.get("candidate_lines")
    if not isinstance(question, Mapping) or not isinstance(candidate_lines, list):
        raise ContractError(f"sample {sample.sample_id} cannot form a native runtime request")
    root_position = question.get("root_position")
    if not isinstance(root_position, Mapping):
        raise ContractError(f"sample {sample.sample_id} has no root position")

    variations: list[dict[str, Any]] = []
    for index, line in enumerate(candidate_lines):
        if not isinstance(line, Mapping):
            raise ContractError(f"sample {sample.sample_id} candidate line {index} is malformed")
        evaluation = line.get("evaluation")
        moves = line.get("moves_uci")
        depth = line.get("depth")
        if (
            not isinstance(evaluation, Mapping)
            or evaluation.get("perspective") != "white"
            or not isinstance(moves, list)
            or not moves
            or not all(isinstance(move, str) and move for move in moves)
            or isinstance(depth, bool)
            or not isinstance(depth, int)
            or depth <= 0
        ):
            raise ContractError(
                f"sample {sample.sample_id} candidate line {index} is not runtime-compatible"
            )
        centipawns = evaluation.get("centipawns", 0)
        mate = evaluation.get("mate")
        if isinstance(centipawns, bool) or not isinstance(centipawns, int):
            raise ContractError(
                f"sample {sample.sample_id} candidate line {index} has no integer score"
            )
        if mate is not None and (isinstance(mate, bool) or not isinstance(mate, int)):
            raise ContractError(
                f"sample {sample.sample_id} candidate line {index} has an invalid mate score"
            )
        variations.append(
            {
                "moves": list(moves),
                "scoreCp": centipawns,
                "mate": mate,
                "depth": depth,
            }
        )

    fen = root_position.get("fen")
    played_move = question.get("played_move_uci")
    if not isinstance(fen, str) or not fen or not isinstance(played_move, str) or not played_move:
        raise ContractError(f"sample {sample.sample_id} has an invalid runtime question")
    return {
        "schema_version": "chesstory.move-meaning.request.v1",
        "request_id": sample.sample_id,
        "input": {
            "fen": fen,
            "playedMoveUci": played_move,
            "variations": variations,
            "movePrefixUci": [],
        },
    }


def _native_diagnostic(root: Path, arguments: argparse.Namespace) -> Mapping[str, Any]:
    from .native_diagnostic import (
        JsonlNativeCommandProvider,
        NativeEngineeringRunner,
        NativeEngineeringSample,
    )

    corpus = Corpus(root / "corpus" / "v1")
    samples = corpus.load_split("diagnostic-explore")
    preregistration = read_json(corpus.root / "preregistration.json")
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    _require_output_outside_run_root(
        arguments.output, artifact_root=artifact_root, run_id=arguments.run_id
    )
    adapter_root = (arguments.adapter_root or (root / "runtime-adapter")).resolve(strict=True)
    if not adapter_root.is_dir():
        raise ContractError("native diagnostic adapter root must be a directory")
    sbt = arguments.sbt.resolve(strict=True)
    if not sbt.is_file():
        raise ContractError("--sbt must name an exact executable file")

    native_samples = [
        NativeEngineeringSample(
            sample_id=sample.sample_id,
            atomic_cluster_id=sample.atomic_cluster_id,
            corpus_version=sample.corpus_version,
            split=sample.split,
            runtime_request=_native_runtime_request(sample),
        )
        for sample in samples
    ]
    context = RunContext(
        run_id=arguments.run_id,
        seed=int(preregistration["seed"]),
        corpus_version=corpus.corpus_version,
        split="diagnostic-explore",
        artifact_root=artifact_root,
        stage_context={
            "design": "frozen-v1-arm-universe-native-v2-shadow",
            "formal_inference": False,
        },
    )
    command = [
        str(sbt),
        "-batch",
        "-error",
        "runMain io.chesstory.evaluation.runtimeadapter.NativeInterventionAdapterCli",
    ]
    provider = JsonlNativeCommandProvider(
        command,
        cwd=adapter_root,
        response_timeout_seconds=arguments.provider_timeout_seconds,
    )
    _reserve_new_run_root(artifact_root=artifact_root, run_id=arguments.run_id)
    store = ArtifactStore(artifact_root, arguments.run_id)
    with provider:
        report = NativeEngineeringRunner(
            provider=provider,
            store=store,
            context=context,
            preregistration=preregistration,
        ).run(native_samples)
    if arguments.output:
        _write_json(arguments.output, report)
    return report


def _provisional_proxy_diagnostic(
    root: Path, arguments: argparse.Namespace
) -> Mapping[str, Any]:
    validate_provisional_run_id(arguments.run_id)
    artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
    _require_output_outside_run_root(
        arguments.output,
        artifact_root=artifact_root,
        run_id=arguments.run_id,
    )
    if arguments.output.resolve().exists():
        raise ContractError(
            "--output already exists; provisional diagnostics are single-shot"
        )
    native_run = (
        arguments.native_run
        or artifact_root / "native-engineering-diagnostic-20260726-v5"
    ).resolve(strict=True)
    oracle_q_run = (
        arguments.oracle_q_run
        or artifact_root / "q-stable-diagnostic-20260726-v11"
    ).resolve(strict=True)
    baseline_probe_completion_report = (
        arguments.baseline_probe_completion_report.resolve(strict=True)
    )
    intermediate_probe_completion_report = (
        arguments.intermediate_probe_completion_report.resolve(strict=True)
    )
    probe_completion_report = arguments.probe_completion_report.resolve(strict=True)
    if not native_run.is_dir():
        raise ContractError("--native-run must be an immutable native run directory")
    if not oracle_q_run.is_dir():
        raise ContractError("--oracle-q-run must be an immutable Q run directory")
    for option, report_path in (
        ("--baseline-probe-completion-report", baseline_probe_completion_report),
        (
            "--intermediate-probe-completion-report",
            intermediate_probe_completion_report,
        ),
        ("--probe-completion-report", probe_completion_report),
    ):
        if not report_path.is_file():
            raise ContractError(f"{option} must be an immutable report file")
    _reserve_new_run_root(artifact_root=artifact_root, run_id=arguments.run_id)
    store = ArtifactStore(artifact_root, arguments.run_id)
    report = run_provisional_proxy_diagnostic(
        root=root,
        run_id=arguments.run_id,
        store=store,
        native_run_root=native_run,
        oracle_q_run_root=oracle_q_run,
        baseline_probe_completion_report=baseline_probe_completion_report,
        intermediate_probe_completion_report=intermediate_probe_completion_report,
        probe_completion_report=probe_completion_report,
    )
    _write_json(arguments.output, report)
    return report


def main(argv: Sequence[str] | None = None) -> int:
    parser = _parser()
    arguments = parser.parse_args(argv)
    try:
        root = _resolve_root(arguments.root)
        if arguments.command == "doctor":
            report = _doctor(root)
            print(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True))
            return 0
        if arguments.command == "plan":
            report = _plan(root)
            if arguments.output:
                _write_json(arguments.output, report)
            print(json.dumps({key: report[key] for key in ("arm_count", "plan_hash", "context_counts")}, ensure_ascii=False, indent=2, sort_keys=True))
            return 0
        if arguments.command == "run":
            report = _run(root, arguments)
            if arguments.output:
                _write_json(arguments.output, report)
            summary_keys = [
                "run_id",
                "status",
                "split_sample_count",
                "atomic_cluster_count",
                "registered_arm_count",
                "executed_arm_count",
                "foundation_controls",
                "all_oracle_ceiling",
            ]
            if report.get("status") == "awaiting-pre-unblinding-attestation":
                summary_keys.extend(("execution_status", "execution_bundle_artifact"))
            else:
                summary_keys.append("attribution")
            summary = {key: report[key] for key in summary_keys}
            print(json.dumps(summary, ensure_ascii=False, indent=2, sort_keys=True))
            return 0
        if arguments.command == "freeze-candidate":
            report = _freeze_candidate(root, arguments)
            print(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True))
            return 0
        if arguments.command == "attest-run":
            report = _attest_run(root, arguments)
            print(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True))
            return 0
        if arguments.command == "finalize-run":
            report = _finalize_run(root, arguments)
            print(
                json.dumps(
                    {
                        key: report[key]
                        for key in (
                            "run_id",
                            "status",
                            "split_sample_count",
                            "atomic_cluster_count",
                            "registered_arm_count",
                            "executed_arm_count",
                            "foundation_controls",
                            "all_oracle_ceiling",
                            "attribution",
                            "pre_unblinding_execution_sha256",
                        )
                    },
                    ensure_ascii=False,
                    indent=2,
                    sort_keys=True,
                )
            )
            return 0
        if arguments.command == "qualify-fresh-confirm":
            report = _qualify_fresh_confirm(root, arguments)
            print(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True))
            return 0
        if arguments.command == "q-diagnostic":
            report = _q_diagnostic(root, arguments)
            _write_json(arguments.output, report)
            print(
                json.dumps(
                    {
                        "run_id": report["run_id"],
                        "status": report["status"],
                        "sample_count": report["sample_count"],
                        "atomic_cluster_count": report["atomic_cluster_count"],
                        "inference": report["inference"],
                        "output": str(arguments.output.resolve()),
                    },
                    ensure_ascii=False,
                    indent=2,
                    sort_keys=True,
                )
            )
            return 0
        if arguments.command == "native-diagnostic":
            report = _native_diagnostic(root, arguments)
            plan = report["plan"]
            print(
                json.dumps(
                    {
                        "run_id": report["binding"]["run_id"],
                        "mode": report["mode"],
                        "registered_arm_count": plan["registered_arm_count"],
                        "planned_row_count": plan["planned_row_count"],
                        "materialized_row_count": plan["materialized_row_count"],
                        "provider_invocation_count": plan["provider_invocation_count"],
                        "completed_row_count": plan["completed_row_count"],
                        "typed_unavailable_row_count": plan["typed_unavailable_row_count"],
                        "actual_native_chain": report["actual_native_chain"],
                        "foundation_gate": report["foundation_gate"],
                        "production_bottleneck": report["production_bottleneck"],
                        "output": (
                            str(arguments.output.resolve()) if arguments.output else None
                        ),
                    },
                    ensure_ascii=False,
                    indent=2,
                    sort_keys=True,
                )
            )
            return 0
        if arguments.command == "provisional-proxy-diagnostic":
            report = _provisional_proxy_diagnostic(root, arguments)
            print(
                json.dumps(
                    {
                        "run_id": report["run_id"],
                        "mode": report["mode"],
                        "qualification": report["qualification"],
                        "evidence_tier": report["evidence_tier"],
                        "registered_arm_count": report["plan"][
                            "registered_arm_count"
                        ],
                        "completed_row_count": report["plan"][
                            "completed_row_count"
                        ],
                        "oracle_q_evidence_level": report["binding"][
                            "oracle_q_evidence_level"
                        ],
                        "engineering_diagnosis": report["engineering_diagnosis"],
                        "engineering_trajectory": report["engineering_trajectory"],
                        "production_bottleneck": None,
                        "output_report_sha256": sha256_file(
                            arguments.output.resolve()
                        ),
                    },
                    ensure_ascii=False,
                    indent=2,
                    sort_keys=True,
                )
            )
            return 0
        if arguments.command == "cause-audit":
            from .cause_audit import execute_cause_audit_action

            artifact_root = (arguments.artifacts or (root / "artifacts")).resolve()
            _require_output_outside_run_root(
                arguments.output,
                artifact_root=artifact_root,
                run_id=arguments.run_id,
            )
            _reserve_new_run_root(
                artifact_root=artifact_root, run_id=arguments.run_id
            )
            store = ArtifactStore(artifact_root, arguments.run_id)
            report = execute_cause_audit_action(
                root=root, store=store, arguments=arguments
            )
            _write_json(arguments.output, report)
            summary = {
                "action": arguments.action,
                "run_id": arguments.run_id,
                "schema_version": report.get("schema_version"),
                "partition": report.get("partition"),
                "case_count": report.get(
                    "case_count",
                    report.get("case_binding", {}).get("case_count")
                    if isinstance(report.get("case_binding"), Mapping)
                    else None,
                ),
                "counts": report.get("counts"),
                "output": str(arguments.output.resolve()),
            }
            print(json.dumps(summary, ensure_ascii=True, indent=2, sort_keys=True))
            return 0
        raise ContractError(f"unsupported command: {arguments.command}")
    except (HarnessError, OSError, ValueError) as error:
        parser.exit(2, f"judgment-eval: {error}\n")


__all__ = ["main"]
