"""Fail-closed candidate freezing and post-run attestation contracts.

This module implements the ordering boundary in JudgmentBoundary section 10:
candidate inputs are frozen before execution; raw run/adjudication evidence is
attested before statistical unblinding; and a separate signed qualification
binds the later report and release decision.  Signing keys are accepted as
in-memory bytes and are never placed in a returned document or written beside
an artifact.
"""

from __future__ import annotations

import base64
import json
import hashlib
import math
import os
import random
import re
from pathlib import Path
from typing import Any, Iterable, Mapping, Sequence

from .attribution import (
    normalize_selected_attribution_hypothesis,
    resolve_attribution_minimum_deltas,
    select_attribution_contrasts,
)
from .capture import (
    ArtifactStore,
    load_custodian_storage_attestation,
    verify_signed_document,
    write_signed_manifest,
)
from .corpus import Corpus
from .design import ALL_ACTUAL_CONTEXT, ALL_ORACLE_CONTEXT, build_arm_plan
from .evaluation import source_blind_evaluation_view
from .hashing import json_bytes, read_json, sha256_file, sha256_json
from .model import Arm, ContractError, IntegrityError, STAGES
from .statistics import (
    summarize_fresh_confirm_equivalence,
    summarize_registered_contrasts,
)


CANDIDATE_SCHEMA_VERSION = "chesstory.eval.candidate-manifest.v1"
RUN_ATTESTATION_SCHEMA_VERSION = "chesstory.eval.run-attestation.v1"
RELEASE_QUALIFICATION_SCHEMA_VERSION = (
    "chesstory.eval.fresh-confirm-qualification.v1"
)

REQUIRED_COMPONENTS: tuple[str, ...] = (
    "production",
    "model",
    "weights",
    "prompts",
    "templates",
    "retrieval",
    "runtime",
    "schema",
    "corpus",
    "oracles",
    "evaluators",
    "dependencies",
    "engine",
    "engine-settings",
    "policy",
    "adapter",
    "runner",
    "canonicalizer",
    "projection",
    "tablebase",
    "annotations",
    "adjudication-procedure",
    "role-schedule",
    "access-policy",
    "preregistration",
)

# These paths are derived by the harness, never selected by the candidate
# owner.  Component labels remain useful for binding model/provider/config
# identities, while this snapshot prevents a label such as ``production`` or
# ``runner`` from being satisfied by an unrelated marker file.
REQUIRED_SOURCE_DIRECTORIES: tuple[str, ...] = (
    "chesstory-runtime/src/main",
    "judgment-evaluation/src/chesstory_eval",
    "judgment-evaluation/schemas",
    "judgment-evaluation/runtime-adapter/src/main",
)
REQUIRED_SOURCE_FILES: tuple[str, ...] = (
    "chesstory-runtime/build.sbt",
    "chesstory-runtime/project/build.properties",
    "judgment-evaluation/pyproject.toml",
    "judgment-evaluation/runtime-adapter/build.sbt",
    "judgment-evaluation/runtime-adapter/project/build.properties",
)

_RELEASE_CRITERIA: tuple[str, ...] = (
    "foundation_controls_passed",
    "both_oracle_ceilings_passed",
    "endpoint_E_passed",
    "effect_direction_replicated",
    "equivalence_bounds_established",
    "complete_frozen_plan_executed",
    "held_out_rater_roster_verified",
)

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_FORBIDDEN_CANDIDATE_KEYS = frozenset(
    {
        "allowed_set",
        "adjudication_result",
        "adjudication_results",
        "effect",
        "effects",
        "metric",
        "metrics",
        "output",
        "outputs",
        "p_value",
        "p_values",
        "post_run",
        "provider_io",
        "result",
        "results",
        "run_allowed_set",
        "run_id",
        "run_output",
        "run_outputs",
        "run_result",
        "run_results",
        "score",
        "scores",
        "statistical_results",
    }
)


def relative_file_hash_manifest(
    root: Path, relative_paths: Iterable[str | Path]
) -> dict[str, dict[str, int | str]]:
    """Hash an explicit set of regular files beneath ``root`` deterministically.

    Returned keys are sorted POSIX relative paths and each value contains the
    file's SHA-256 and byte size.  Absolute paths, ``..`` traversal, duplicate
    normalized paths, missing/non-regular files, and any symlink or Windows
    junction in the root-relative path are rejected before hashing.
    """

    root_path = Path(root)
    _reject_link_like(root_path, label="hash-manifest root")
    try:
        resolved_root = root_path.resolve(strict=True)
    except OSError as error:
        raise IntegrityError(f"hash-manifest root is unavailable: {root_path}") from error
    if not resolved_root.is_dir():
        raise IntegrityError(f"hash-manifest root is not a directory: {root_path}")

    normalized: dict[str, Path] = {}
    for supplied in relative_paths:
        relative, key = _normalize_relative_file_path(supplied)
        if key in normalized:
            raise ContractError(f"duplicate normalized manifest path: {key}")
        candidate = root_path / relative
        _reject_path_links(root_path, relative)
        try:
            resolved = candidate.resolve(strict=True)
        except OSError as error:
            raise IntegrityError(f"manifest file is unavailable: {key}") from error
        if not _is_strictly_within(resolved, resolved_root):
            raise IntegrityError(f"manifest path escapes root: {key}")
        if not resolved.is_file():
            raise IntegrityError(f"manifest path is not a regular file: {key}")
        normalized[key] = resolved

    return {
        key: {"sha256": sha256_file(normalized[key]), "size": normalized[key].stat().st_size}
        for key in sorted(normalized)
    }


def tree_file_hash_manifest(root: Path) -> dict[str, dict[str, int | str]]:
    """Hash every regular file in ``root`` without following link-like entries.

    Directory traversal is deterministic.  A symlink, junction, device, socket,
    or other non-regular entry causes the whole snapshot to fail closed instead
    of being omitted from the claimed complete file set.
    """

    relative_paths = _walk_regular_files(Path(root))
    return relative_file_hash_manifest(root, relative_paths)


def required_source_snapshot(component_root: Path) -> dict[str, Any]:
    """Hash the complete executable source surface required by this harness.

    The directory roster is code-owned and exact.  New, removed, or changed
    source files therefore invalidate a frozen candidate even if an
    owner-supplied logical component binding was narrower.  Python bytecode
    caches are rejected instead of ignored: an interpreter may execute a
    cache, so allowing an unhashed cache beside the frozen source would leave
    an executable surface outside the candidate binding.
    """

    root = _resolved_directory(component_root, "component root")
    relative_paths: list[str] = list(REQUIRED_SOURCE_FILES)
    for directory_text in REQUIRED_SOURCE_DIRECTORIES:
        directory_relative, directory_key = _normalize_relative_file_path(
            directory_text
        )
        directory = root / directory_relative
        for child in _walk_regular_files(directory):
            child_path = Path(child)
            if any(
                part.casefold() == "__pycache__" for part in child_path.parts
            ) or child_path.suffix.casefold() in {
                ".pyc",
                ".pyo",
            }:
                raise ContractError(
                    "required source tree contains forbidden Python bytecode cache: "
                    f"{directory_key}/{child_path.as_posix()}"
                )
            relative_paths.append(f"{directory_key}/{child_path.as_posix()}")

    normalized_paths = sorted(set(relative_paths))
    if len(normalized_paths) != len(relative_paths):
        raise ContractError("required source snapshot contains duplicate paths")
    manifest = relative_file_hash_manifest(root, normalized_paths)
    return {
        "directories": list(REQUIRED_SOURCE_DIRECTORIES),
        "required_files": list(REQUIRED_SOURCE_FILES),
        "file_manifest": manifest,
        "file_manifest_sha256": sha256_json(manifest),
    }


def build_candidate_manifest(
    *,
    candidate_id: str,
    frozen_at: str,
    components: Mapping[str, Mapping[str, Any]],
    seed: int,
    corpus_root: Path,
    split_hashes: Mapping[str, str],
    component_root: Path,
    custodian_state_root: Path,
    external_executable_binding: Mapping[str, Any],
) -> dict[str, Any]:
    """Build a pre-run candidate manifest from explicit owner-supplied bindings.

    Every required component must be supplied with a non-empty ``version`` and
    an exact ``sha256``; this function never discovers or substitutes a missing
    production/model/configuration identity.  The corpus must explicitly carry
    the ``release-qualified`` qualification.  The caller must also supply the
    complete split-hash map, which is checked against both the corpus manifest
    and the bytes on disk.  Result-derived, allowed-set, provider-I/O, and other
    post-run fields are forbidden recursively in candidate components.

    ``weights.snapshot_fixed`` is mandatory.  When false, the binding must also
    name ``provider_version`` and set ``raw_provider_io_required`` to true; its
    SHA-256 then binds the provider/weight descriptor rather than pretending an
    unavailable API weight snapshot was frozen.
    """

    candidate_id = _nonempty_text(candidate_id, "candidate_id")
    frozen_at = _nonempty_text(frozen_at, "frozen_at")
    if isinstance(seed, bool) or not isinstance(seed, int):
        raise ContractError("seed must be an explicit integer")
    if not isinstance(components, Mapping):
        raise ContractError("components must be an object")
    _reject_forbidden_candidate_fields(components)

    missing = sorted(set(REQUIRED_COMPONENTS) - set(components))
    if missing:
        raise ContractError(f"candidate component bindings are missing: {missing}")
    frozen_component_root = _resolved_directory(component_root, "component root")
    frozen_external_executables = _validate_external_executable_binding(
        external_executable_binding,
        component_root=frozen_component_root,
        verify_files=True,
    )
    frozen_components: dict[str, dict[str, Any]] = {}
    for name in sorted(components):
        value = components[name]
        if not isinstance(name, str) or not name.strip():
            raise ContractError("candidate component names must be non-empty text")
        frozen_components[name] = _validate_component_binding(
            name, value, component_root=frozen_component_root
        )

    supplied_corpus_root = Path(corpus_root)
    _reject_link_like(supplied_corpus_root, label="candidate corpus root")
    corpus = Corpus(supplied_corpus_root)
    corpus_manifest = corpus.manifest
    if corpus_manifest.get("qualification") != "release-qualified":
        raise ContractError(
            "candidate freeze requires corpus qualification 'release-qualified'"
        )

    support_paths = corpus.manifest_resource_paths(release=True)
    support_relative: dict[str, str] = {}
    for logical_name, supplied_path in support_paths.items():
        resolved = supplied_path.resolve(strict=True)
        if not _is_strictly_within(resolved, frozen_component_root):
            raise IntegrityError(
                f"corpus support file escapes component root: {logical_name!r}"
            )
        support_relative[logical_name] = resolved.relative_to(
            frozen_component_root
        ).as_posix()
    support_file_manifest = relative_file_hash_manifest(
        frozen_component_root, support_relative.values()
    )
    support_bindings = {
        logical_name: {
            "path": relative_path,
            **support_file_manifest[relative_path],
        }
        for logical_name, relative_path in sorted(support_relative.items())
    }
    component_resource_pairs = {
        "corpus": "manifest",
        "annotations": "annotation_index",
        "adjudication-procedure": "adjudication_procedure",
        "role-schedule": "role_schedule",
        "access-policy": "access_policy",
        "preregistration": "preregistration",
    }
    for component_name, logical_name in component_resource_pairs.items():
        component = frozen_components[component_name]
        support = support_bindings[logical_name]
        if component.get("paths") != [support["path"]] or component.get(
            "sha256"
        ) != support["sha256"]:
            raise IntegrityError(
                f"component {component_name!r} does not bind the manifest-selected {logical_name!r} bytes"
            )

    manifest_files = relative_file_hash_manifest(corpus.root, ["manifest.json"])
    manifest_sha256 = str(manifest_files["manifest.json"]["sha256"])
    corpus_binding = frozen_components["corpus"]
    if corpus_binding["version"] != corpus.corpus_version:
        raise IntegrityError("corpus component version does not match corpus manifest")
    if corpus_binding["sha256"] != manifest_sha256:
        raise IntegrityError("corpus component sha256 does not match manifest bytes")

    splits = corpus_manifest.get("splits")
    if not isinstance(splits, Mapping):
        raise ContractError("corpus manifest splits must be an object")
    if set(split_hashes) != set(splits):
        raise ContractError("split_hashes must contain exactly every corpus split")
    supplied_splits = {
        str(split): _require_sha256(digest, f"split_hashes[{split!r}]")
        for split, digest in split_hashes.items()
    }
    split_paths: dict[str, str] = {}
    for split, entry in splits.items():
        if not isinstance(split, str) or not isinstance(entry, Mapping):
            raise ContractError("corpus split entries must be named objects")
        path = entry.get("path")
        if not isinstance(path, str):
            raise ContractError(f"corpus split {split!r} has no path")
        declared = _require_sha256(entry.get("sha256"), f"corpus split {split!r}")
        if supplied_splits[split] != declared:
            raise IntegrityError(f"owner-supplied split hash disagrees for {split!r}")
        split_paths[split] = path
    actual_split_files = relative_file_hash_manifest(corpus.root, split_paths.values())
    for split, path in split_paths.items():
        if actual_split_files[Path(path).as_posix()]["sha256"] != supplied_splits[split]:
            raise IntegrityError(f"sealed split bytes disagree for {split!r}")

    access_policy = read_json(support_paths["access_policy"])
    if not isinstance(access_policy, Mapping):
        raise ContractError("release access policy must be an object")
    custodian_binding = _validate_custodian_state_policy(
        access_policy, custodian_state_root
    )

    preregistration = read_json(support_paths["preregistration"])
    if not isinstance(preregistration, Mapping):
        raise ContractError("release preregistration must be an object")
    minimums = preregistration.get("minimum_effective_counts")
    if not isinstance(minimums, Mapping):
        raise ContractError("release preregistration minimum counts are missing")
    corpus.release_role_assignments(
        split="fresh-confirm",
        minimum_held_out_raters=int(minimums.get("held_out_human_raters", 0)),
    )
    corpus.release_role_assignments(
        split="blind",
        minimum_held_out_raters=int(minimums.get("held_out_human_raters", 0)),
    )

    # Canonical JSON serialization here is also a JSON-safety/finite-number check.
    json_bytes(frozen_components)
    json_bytes(frozen_external_executables)
    source_snapshot = required_source_snapshot(frozen_component_root)
    return {
        "schema_version": CANDIDATE_SCHEMA_VERSION,
        "candidate_id": candidate_id,
        "frozen_at": frozen_at,
        "seed": seed,
        "components": frozen_components,
        "external_executable_binding": frozen_external_executables,
        "required_source_snapshot": source_snapshot,
        "corpus_binding": {
            "corpus_version": corpus.corpus_version,
            "qualification": "release-qualified",
            "manifest_sha256": manifest_sha256,
            "atomic_clusters_sha256": sha256_json(corpus_manifest["atomic_clusters"]),
            "split_sha256": {key: supplied_splits[key] for key in sorted(supplied_splits)},
            "support_files": support_bindings,
            "support_files_sha256": sha256_json(support_bindings),
            "custodian_state": custodian_binding,
        },
    }


def build_run_attestation(
    *,
    artifact_store: ArtifactStore,
    attested_at: str,
    candidate_manifest_hash: str,
    candidate_live_command_component_hash: str,
    candidate_external_executable_hash: str,
    split_id: str,
    split_hash: str,
    corpus_version: str,
    execution_manifest_path: str | Path,
    execution_bundle_path: str | Path,
    endpoint_policy_path: str | Path,
    oracle_graph_path: str | Path,
    allowed_set_path: str | Path,
    adjudication_paths: Mapping[str, str | Path],
    raw_provider_io_required: bool,
) -> dict[str, Any]:
    """Build the pre-unblinding raw-run attestation from a stable store.

    The result binds the signed candidate-manifest hash, exact split input,
    final oracle graph and run allowed set, source-blind adjudication artifacts,
    both append-only ledgers, and every file present below the run root.  Any
    statistical report, equivalence result, or decision file is rejected at
    this phase.  Two
    complete snapshots around a second ``ArtifactStore.verify`` call must match,
    so a concurrently changing or corrupt run cannot be attested.
    """

    if not isinstance(artifact_store, ArtifactStore):
        raise ContractError("artifact_store must be an ArtifactStore")
    attested_at = _nonempty_text(attested_at, "attested_at")
    split_id = _nonempty_text(split_id, "split_id")
    candidate_manifest_hash = _require_sha256(
        candidate_manifest_hash, "candidate_manifest_hash"
    )
    candidate_live_command_component_hash = _require_sha256(
        candidate_live_command_component_hash,
        "candidate live-command component hash",
    )
    candidate_external_executable_hash = _require_sha256(
        candidate_external_executable_hash,
        "candidate external executable hash",
    )
    split_hash = _require_sha256(split_hash, "split_hash")
    corpus_version = _nonempty_text(corpus_version, "corpus_version")
    if not isinstance(raw_provider_io_required, bool):
        raise ContractError("raw_provider_io_required must be boolean")
    _, frozen_execution_path = _normalize_relative_file_path(execution_manifest_path)
    _, frozen_bundle_path = _normalize_relative_file_path(execution_bundle_path)
    _, frozen_policy_path = _normalize_relative_file_path(endpoint_policy_path)
    _, frozen_oracle_path = _normalize_relative_file_path(oracle_graph_path)
    _, frozen_allowed_path = _normalize_relative_file_path(allowed_set_path)
    if not isinstance(adjudication_paths, Mapping) or not adjudication_paths:
        raise ContractError("adjudication_paths must be a non-empty object")
    frozen_adjudication_paths: dict[str, str] = {}
    for name, supplied_path in adjudication_paths.items():
        normalized_name = _nonempty_text(name, "adjudication artifact name")
        _, normalized_path = _normalize_relative_file_path(supplied_path)
        frozen_adjudication_paths[normalized_name] = normalized_path

    artifact_store.verify()
    if artifact_store.ledger_hash() is None:
        raise IntegrityError("run attestation requires artifact-ledger.jsonl")
    if artifact_store.access_log_hash() is None:
        raise IntegrityError("run attestation requires access-log.jsonl")
    first_snapshot = tree_file_hash_manifest(artifact_store.run_root)
    artifact_store.verify()
    second_snapshot = tree_file_hash_manifest(artifact_store.run_root)
    if first_snapshot != second_snapshot:
        raise IntegrityError("run files changed while the attestation snapshot was built")
    forbidden_markers = (
        "experiment-report",
        "equivalence",
        "release-evidence",
        "qualification",
        "decision",
        "statistical-contrast",
    )
    forbidden_present = sorted(
        path
        for path in first_snapshot
        if any(marker in Path(path).name.lower() for marker in forbidden_markers)
    )
    if forbidden_present:
        raise ContractError(
            "pre-unblinding run attestation cannot include statistical/decision artifacts: "
            f"{forbidden_present}"
        )
    ledger_bound_paths: set[str] = set()
    with artifact_store.ledger_path.open("r", encoding="utf-8") as handle:
        for line in handle:
            record = json.loads(line)
            relative = record.get("artifact_path")
            hashes = record.get("artifact_hashes")
            if not isinstance(relative, str) or not isinstance(hashes, Mapping):
                continue
            if relative in first_snapshot and len(hashes) == 1 and Path(relative).name in hashes:
                ledger_bound_paths.add(relative)
            else:
                for filename in hashes:
                    if isinstance(filename, str):
                        ledger_bound_paths.add(f"{relative}/{filename}")
    expected_ledger_coverage = set(first_snapshot) - {
        "artifact-ledger.jsonl",
        "access-log.jsonl",
    }
    if ledger_bound_paths != expected_ledger_coverage:
        raise IntegrityError(
            "pre-unblinding run tree contains unlogged or dangling artifact-ledger paths"
        )

    artifact_ledger = first_snapshot.get("artifact-ledger.jsonl")
    access_log = first_snapshot.get("access-log.jsonl")
    if artifact_ledger is None or access_log is None:
        raise IntegrityError("complete run snapshot does not contain both ledgers")
    oracle_entry = first_snapshot.get(frozen_oracle_path)
    allowed_entry = first_snapshot.get(frozen_allowed_path)
    execution_entry = first_snapshot.get(frozen_execution_path)
    bundle_entry = first_snapshot.get(frozen_bundle_path)
    policy_entry = first_snapshot.get(frozen_policy_path)
    if execution_entry is None:
        raise IntegrityError(
            f"execution manifest is not an attested run file: {frozen_execution_path}"
        )
    if bundle_entry is None:
        raise IntegrityError(
            f"pre-unblinding execution bundle is not an attested run file: {frozen_bundle_path}"
        )
    if policy_entry is None:
        raise IntegrityError(
            f"endpoint policy is not an attested run file: {frozen_policy_path}"
        )
    if oracle_entry is None:
        raise IntegrityError(
            f"oracle graph artifact is not an attested run file: {frozen_oracle_path}"
        )
    if allowed_entry is None:
        raise IntegrityError(
            f"allowed-set artifact is not an attested run file: {frozen_allowed_path}"
        )
    execution_document = _read_run_json(artifact_store, frozen_execution_path)
    if execution_document.get("schema_version") != "chesstory.eval.execution-manifest.v1":
        raise ContractError("unsupported pre-unblinding execution-manifest schema")
    expected_execution_binding = {
        "run_id": artifact_store.run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus_version,
        "split": split_id,
        "split_input_sha256": split_hash,
    }
    for key, expected_value in expected_execution_binding.items():
        if execution_document.get(key) != expected_value:
            raise IntegrityError(f"execution manifest {key} binding mismatch")
    registered_arm_ids = execution_document.get("registered_arm_ids")
    if (
        not isinstance(registered_arm_ids, list)
        or len(registered_arm_ids) != 313
        or registered_arm_ids != sorted(set(registered_arm_ids))
    ):
        raise ContractError("execution manifest does not freeze the full 313-arm plan")
    _require_sha256(execution_document.get("plan_sha256"), "execution plan_sha256")
    execution_bundle = _read_run_json(artifact_store, frozen_bundle_path)
    if execution_bundle.get("schema_version") != (
        "chesstory.eval.pre-unblinding-execution.v1"
    ):
        raise ContractError("unsupported pre-unblinding execution bundle schema")
    bundle_binding = execution_bundle.get("binding")
    if not isinstance(bundle_binding, Mapping):
        raise ContractError("pre-unblinding execution bundle binding is missing")
    for key, expected_value in {
        "run_id": artifact_store.run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus_version,
        "split": split_id,
        "split_sha256": split_hash,
    }.items():
        if bundle_binding.get(key) != expected_value:
            raise IntegrityError(f"execution bundle {key} binding mismatch")
    if (
        execution_bundle.get("plan_sha256") != execution_document.get("plan_sha256")
        or execution_bundle.get("registered_arm_count") != 313
    ):
        raise IntegrityError("execution bundle differs from the frozen 313-arm plan")
    bundle_plan = execution_bundle.get("plan")
    if not isinstance(bundle_plan, list) or [
        value.get("arm_id") if isinstance(value, Mapping) else None
        for value in bundle_plan
    ] != registered_arm_ids:
        raise IntegrityError("execution bundle arm universe differs from execution manifest")
    if execution_document.get("live_command_config") != execution_bundle.get(
        "live_command_config"
    ):
        raise IntegrityError(
            "execution manifest/bundle live-command config bindings differ"
        )
    live_command_config = _validate_live_command_config_binding(
        artifact_store=artifact_store,
        binding=execution_bundle.get("live_command_config"),
        run_file_hashes=first_snapshot,
    )
    captured_live_command_document = _read_run_json(
        artifact_store, str(live_command_config["artifact_path"])
    )
    live_command_argv_by_route = _captured_live_command_argv_by_route(
        captured_live_command_document
    )
    if live_command_config.get(
        "candidate_component_sha256"
    ) != candidate_live_command_component_hash or live_command_config.get(
        "candidate_external_executable_sha256"
    ) != candidate_external_executable_hash:
        raise IntegrityError(
            "run live-command binding differs from the verified signed candidate"
        )
    bundle_samples = execution_bundle.get("sample_universe")
    bundle_rows = execution_bundle.get("rows")
    if not isinstance(bundle_samples, list) or not isinstance(bundle_rows, list):
        raise ContractError("execution bundle sample/row universe is missing")
    if execution_bundle.get("sample_universe_sha256") != sha256_json(bundle_samples):
        raise IntegrityError("execution bundle sample-universe hash mismatch")
    ledger_lines = artifact_store.ledger_path.read_bytes().splitlines(keepends=True)
    if len(ledger_lines) < 2:
        raise IntegrityError("execution bundle has no preceding artifact-ledger state")
    ledger_tail = json.loads(ledger_lines[-1])
    bundle_filename = Path(frozen_bundle_path).name
    if (
        not isinstance(ledger_tail, Mapping)
        or ledger_tail.get("event") != "run-document"
        or ledger_tail.get("artifact_path") != frozen_bundle_path
        or ledger_tail.get("artifact_hashes")
        != {bundle_filename: bundle_entry["sha256"]}
    ):
        raise IntegrityError(
            "execution bundle must be the final raw artifact-ledger capture before phase A"
        )
    ledger_before_bundle = hashlib.sha256(b"".join(ledger_lines[:-1])).hexdigest()
    if execution_bundle.get("artifact_ledger_sha256_before_bundle") != (
        ledger_before_bundle
    ):
        raise IntegrityError("execution bundle pre-capture ledger hash mismatch")
    if execution_bundle.get("access_log_sha256_before_bundle") != access_log["sha256"]:
        raise IntegrityError("execution bundle pre-capture access-log hash mismatch")
    for label in ("foundation_controls", "all_oracle_ceiling"):
        if not isinstance(execution_bundle.get(label), Mapping):
            raise ContractError(f"execution bundle {label} is missing")
        if execution_bundle[label].get("status") not in {"passed", "not-passed"}:
            raise ContractError(f"execution bundle {label} status is invalid")
    policy_document = _read_run_json(artifact_store, frozen_policy_path)
    policy_binding = policy_document.get("binding")
    if not isinstance(policy_binding, Mapping):
        raise ContractError("endpoint policy binding is missing")
    for key, expected_value in {
        "run_id": artifact_store.run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus_version,
        "split": split_id,
        "split_sha256": split_hash,
    }.items():
        if policy_binding.get(key) != expected_value:
            raise IntegrityError(f"endpoint policy {key} binding mismatch")
    claimed_policy_content = _require_sha256(
        policy_document.get("content_sha256"), "endpoint policy content_sha256"
    )
    unsigned_policy = dict(policy_document)
    unsigned_policy.pop("content_sha256", None)
    if sha256_json(unsigned_policy) != claimed_policy_content:
        raise IntegrityError("endpoint policy content hash mismatch")
    policy_file_hash = str(policy_entry["sha256"])
    if (
        execution_document.get("endpoint_policy_sha256") != policy_file_hash
        or execution_bundle.get("endpoint_policy_sha256") != policy_file_hash
        or execution_bundle.get("endpoint_allowed_set_source")
        != "pre-unblinding-frozen-run-artifact"
    ):
        raise IntegrityError(
            "execution manifest/bundle does not bind the frozen endpoint policy"
        )
    oracle_document = _read_run_json(artifact_store, frozen_oracle_path)
    allowed_document = _read_run_json(artifact_store, frozen_allowed_path)
    if oracle_document != policy_document.get("oracle_graph"):
        raise IntegrityError("oracle-graph artifact differs from the frozen endpoint policy")
    if allowed_document != policy_document.get("allowed_set_by_sample"):
        raise IntegrityError("allowed-set artifact differs from the frozen endpoint policy")
    policy_topology = policy_document.get("sample_atomic_cluster_ids")
    policy_raters = policy_document.get("source_blind_evaluator_ids_by_sample")
    if not isinstance(policy_topology, Mapping) or not isinstance(policy_raters, Mapping):
        raise ContractError("endpoint policy topology/rater roster is missing")
    sample_ids = execution_document.get("sample_ids")
    if sample_ids != sorted(policy_topology) or set(policy_raters) != set(policy_topology):
        raise IntegrityError("execution sample universe differs from endpoint policy")
    expected_bundle_samples = [
        {
            "sample_id": sample_id,
            "atomic_cluster_id": policy_topology[sample_id],
        }
        for sample_id in sorted(policy_topology)
    ]
    if bundle_samples != expected_bundle_samples:
        raise IntegrityError("execution bundle sample topology differs from endpoint policy")
    expected_row_count = len(registered_arm_ids) * len(sample_ids)
    if (
        execution_bundle.get("split_sample_count") != len(sample_ids)
        or execution_bundle.get("atomic_cluster_count")
        != len(set(policy_topology.values()))
        or execution_bundle.get("expected_row_count") != expected_row_count
    ):
        raise IntegrityError("execution bundle declared sample/row counts are inconsistent")
    execution_status = execution_bundle.get("execution_status")
    if execution_status not in {
        "foundation-not-passed",
        "intervention-incomplete",
        "ready-for-analysis",
    }:
        raise ContractError("execution bundle status is invalid")
    expected_endpoint_pairs: set[tuple[str, str]] = set()
    bundle_row_pairs: set[tuple[str, str]] = set()
    for row in bundle_rows:
        if not isinstance(row, Mapping):
            raise ContractError("execution bundle row must be an object")
        sample_id = row.get("sample_id")
        arm_id = row.get("arm_id")
        if (
            not isinstance(sample_id, str)
            or not isinstance(arm_id, str)
            or sample_id not in policy_topology
            or arm_id not in registered_arm_ids
        ):
            raise IntegrityError("execution bundle row identity is invalid")
        pair = (sample_id, arm_id)
        if pair in bundle_row_pairs:
            raise IntegrityError(f"execution bundle row is duplicated: {pair!r}")
        bundle_row_pairs.add(pair)
        if row.get("status") == "completed":
            expected_endpoint_pairs.add(pair)
    if execution_bundle.get("executed_arm_count") != len(
        {arm_id for _, arm_id in bundle_row_pairs}
    ):
        raise IntegrityError("execution bundle declared arm count is inconsistent")
    gates_passed = all(
        execution_bundle[label].get("status") == "passed"
        for label in ("foundation_controls", "all_oracle_ceiling")
    )
    if execution_status == "foundation-not-passed":
        if gates_passed:
            raise IntegrityError("foundation-not-passed bundle declares passed gates")
    else:
        full_pairs = {
            (sample_id, arm_id)
            for sample_id in sample_ids
            for arm_id in registered_arm_ids
        }
        if not gates_passed or bundle_row_pairs != full_pairs:
            raise IntegrityError(
                "post-foundation execution bundle has inconsistent gates or row universe"
            )
        incomplete = any(row.get("status") != "completed" for row in bundle_rows)
        if (execution_status == "ready-for-analysis") == incomplete:
            raise IntegrityError("execution bundle status disagrees with row completion")
    endpoint_pairs: set[tuple[str, str]] = set()
    endpoint_artifact_universe: list[tuple[str, str, str]] = []
    endpoint_evaluator_universe: list[tuple[str, str, str]] = []
    for relative_path in first_snapshot:
        if "/" in relative_path or not (
            relative_path.startswith("endpoint-") and relative_path.endswith(".json")
        ):
            continue
        candidate_document = _read_run_json(artifact_store, relative_path)
        if candidate_document.get("schema_version") != "chesstory.eval.endpoint-capture.v1":
            continue
        binding = candidate_document.get("binding")
        if not isinstance(binding, Mapping):
            raise ContractError(f"endpoint capture binding is missing: {relative_path}")
        for key, expected_value in {
            "run_id": artifact_store.run_id,
            "candidate_manifest_sha256": candidate_manifest_hash,
            "corpus_version": corpus_version,
            "split": split_id,
            "split_sha256": split_hash,
        }.items():
            if binding.get(key) != expected_value:
                raise IntegrityError(f"endpoint capture {key} mismatch: {relative_path}")
        sample_id = binding.get("sample_id")
        cluster_id = binding.get("atomic_cluster_id")
        arm_id = binding.get("arm_id")
        if (
            not isinstance(sample_id, str)
            or policy_topology.get(sample_id) != cluster_id
            or arm_id not in registered_arm_ids
        ):
            raise IntegrityError(f"endpoint capture sample/cluster/arm mismatch: {relative_path}")
        pair = (sample_id, str(arm_id))
        if pair in endpoint_pairs:
            raise IntegrityError(f"duplicate endpoint capture for {pair!r}")
        endpoint_pairs.add(pair)
        endpoint_artifact_universe.append(
            (sample_id, str(arm_id), str(first_snapshot[relative_path]["sha256"]))
        )
        evaluator_hash = _require_sha256(
            candidate_document.get("evaluator_artifact_sha256"),
            f"endpoint evaluator artifact {relative_path}",
        )
        evaluation_view_hash = _require_sha256(
            candidate_document.get("evaluation_view_sha256"),
            f"endpoint evaluation view {relative_path}",
        )
        endpoint_evaluator_universe.append(
            (sample_id, evaluation_view_hash, evaluator_hash)
        )
        identities = policy_raters.get(sample_id)
        if not isinstance(identities, list):
            raise ContractError(f"endpoint rater roster is invalid for {sample_id!r}")
        for evaluator_id in identities:
            if not isinstance(evaluator_id, str):
                raise ContractError("endpoint evaluator identity is invalid")
    if endpoint_pairs != expected_endpoint_pairs:
        raise IntegrityError(
            "pre-unblinding endpoint artifacts differ from completed execution-bundle rows"
        )
    exact_run_binding = {
        "run_id": artifact_store.run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus_version,
        "split": split_id,
        "split_sha256": split_hash,
        "seed": policy_binding.get("seed"),
    }
    for row in bundle_rows:
        assert isinstance(row, Mapping)
        _validate_execution_row_artifacts(
            artifact_store=artifact_store,
            row=row,
            run_file_hashes=first_snapshot,
            exact_run_binding=exact_run_binding,
            sample_topology=policy_topology,
            registered_arm_ids=set(registered_arm_ids),
        )
    evaluator_artifact_paths = sorted(
        path
        for path in first_snapshot
        if "/" not in path
        and path.startswith("evaluator-artifact-")
        and path.endswith(".json")
    )
    evaluator_artifact_universe: list[tuple[str, str, str]] = []
    evaluator_access_expected: list[tuple[str, str, str, str]] = []
    for evaluator_path in evaluator_artifact_paths:
        evaluator_document = _read_run_json(artifact_store, evaluator_path)
        evaluator_digest = str(first_snapshot[evaluator_path]["sha256"])
        if sha256_json(evaluator_document) != evaluator_digest:
            raise IntegrityError(
                f"evaluator artifact is not canonical content: {evaluator_path}"
            )
        sample_id, output_hash, evaluator_ids = _validate_evaluator_artifact_document(
            evaluator_document,
            raters_by_sample=policy_raters,
        )
        evaluator_artifact_universe.append(
            (sample_id, output_hash, evaluator_digest)
        )
        evaluator_access_expected.extend(
            (
                f"final-usefulness-rater:{evaluator_id}",
                evaluator_digest,
                "source-blind-evaluation",
                f"sample:{sample_id}",
            )
            for evaluator_id in evaluator_ids
        )
    if sorted(evaluator_artifact_universe) != sorted(endpoint_evaluator_universe):
        raise IntegrityError(
            "endpoint evaluator hashes do not exactly match captured evaluator artifacts"
        )
    frozen_adjudication: dict[str, str] = {}
    expected_adjudicators = policy_document.get(
        "out_of_set_adjudicator_ids_by_sample"
    )
    if not isinstance(expected_adjudicators, Mapping) or not expected_adjudicators:
        raise ContractError("endpoint policy has no per-sample adjudicator roster")
    actual_adjudicator_pairs: set[tuple[str, str]] = set()
    adjudicator_access_requirements: list[tuple[str, str]] = []
    for name, path in frozen_adjudication_paths.items():
        entry = first_snapshot.get(path)
        if entry is None:
            raise IntegrityError(
                f"adjudication artifact is not an attested run file: {path}"
            )
        frozen_adjudication[name] = str(entry["sha256"])
        adjudication_document = _read_run_json(artifact_store, path)
        if adjudication_document.get("schema_version") != (
            "chesstory.eval.blind-adjudication.v1"
        ):
            raise ContractError(f"unsupported adjudication schema: {path}")
        sample_id = adjudication_document.get("sample_id")
        adjudicator_id = adjudication_document.get("adjudicator_id")
        if not isinstance(sample_id, str) or not isinstance(adjudicator_id, str):
            raise ContractError(f"adjudication identity is incomplete: {path}")
        pair = (sample_id, adjudicator_id)
        if pair in actual_adjudicator_pairs:
            raise ContractError(f"duplicate adjudication identity pair: {pair!r}")
        actual_adjudicator_pairs.add(pair)
        adjudicator_access_requirements.append(
            (f"out-of-set-adjudicator:{adjudicator_id}", str(entry["sha256"]))
        )
    expected_adjudicator_pairs = {
        (sample_id, adjudicator_id)
        for sample_id, identities in expected_adjudicators.items()
        if isinstance(sample_id, str) and isinstance(identities, list)
        for adjudicator_id in identities
        if isinstance(adjudicator_id, str)
    }
    if actual_adjudicator_pairs != expected_adjudicator_pairs:
        raise IntegrityError(
            "attested adjudication artifacts differ from the exact frozen roster"
        )
    access_records: list[Mapping[str, Any]] = []
    with artifact_store.access_log_path.open("r", encoding="utf-8") as handle:
        for line in handle:
            value = json.loads(line)
            if isinstance(value, Mapping):
                access_records.append(value)
    for required_role, artifact_hash in adjudicator_access_requirements:
        if not any(
            record.get("event") == "artifact-access"
            and record.get("role") == required_role
            and record.get("artifact_hash") == artifact_hash
            and record.get("action") == "source-blind-adjudication"
            for record in access_records
        ):
            raise IntegrityError(
                f"adjudication access is absent from append-only log: {required_role}"
            )
    evaluator_access_actual = sorted(
        (
            str(record.get("role")),
            str(record.get("artifact_hash")),
            str(record.get("action")),
            str(record.get("reason")),
        )
        for record in access_records
        if record.get("event") == "artifact-access"
        and record.get("action") == "source-blind-evaluation"
        and isinstance(record.get("role"), str)
        and str(record.get("role")).startswith("final-usefulness-rater:")
    )
    if evaluator_access_actual != sorted(evaluator_access_expected):
        raise IntegrityError(
            "evaluator artifacts and source-blind rater access records are not an exact set"
        )
    if not any(
        record.get("event") == "artifact-access"
        and record.get("role") == "experiment-runner"
        and record.get("artifact_hash") == live_command_config["artifact_sha256"]
        and record.get("action") == "freeze-live-command-config"
        for record in access_records
    ):
        raise IntegrityError(
            "live-command config freeze is absent from the append-only access log"
        )
    stage_provider_io_paths = sorted(
        path for path in first_snapshot if path.endswith("/raw-provider-io.json")
    )
    evaluator_provider_io_paths = sorted(
        path
        for path in first_snapshot
        if "/" not in path
        and path.startswith("evaluator-raw-provider-io-")
        and path.endswith(".json")
    )
    provider_io_paths = sorted(stage_provider_io_paths + evaluator_provider_io_paths)
    for provider_path in stage_provider_io_paths:
        _validate_successful_stage_provider_io(
            artifact_store=artifact_store,
            relative_path=provider_path,
            expected_argv_by_route=live_command_argv_by_route,
        )
    provider_response_universe: list[tuple[str, str, str]] = []
    for provider_path in evaluator_provider_io_paths:
        provider_document = _read_run_json(artifact_store, provider_path)
        if set(provider_document) != {
            "command",
            "request",
            "control_binding",
            "stdin_base64",
            "returncode",
            "stdout",
            "stderr",
            "stdout_base64",
            "stderr_base64",
            "response",
        }:
            raise ContractError(
                f"successful evaluator provider I/O fields are not exact: {provider_path}"
            )
        response = provider_document.get("response")
        if not isinstance(response, Mapping):
            raise ContractError(
                f"evaluator raw provider I/O has no response object: {provider_path}"
            )
        sample_id, output_hash, _ = _validate_evaluator_artifact_document(
            response,
            raters_by_sample=policy_raters,
        )
        request = provider_document.get("request")
        stdin = provider_document.get("stdin_base64")
        try:
            decoded_request = base64.b64decode(str(stdin), validate=True).decode(
                "utf-8"
            )
            reparsed_request = json.loads(decoded_request)
        except (ValueError, UnicodeError, json.JSONDecodeError) as error:
            raise IntegrityError(
                f"evaluator provider stdin capture is invalid: {provider_path}"
            ) from error
        if reparsed_request != request or provider_document.get(
            "command"
        ) != live_command_argv_by_route.get(
            "evaluator"
        ) or provider_document.get("control_binding") != {
            "sample_id": sample_id,
            "canonical_output_hash": output_hash,
        } or provider_document.get("returncode") != 0 or not isinstance(
            request, Mapping
        ) or sha256_json(request) != output_hash:
            raise IntegrityError(
                f"evaluator raw provider I/O control binding failed: {provider_path}"
            )
        provider_response_universe.append(
            (sample_id, output_hash, sha256_json(response))
        )
    if sorted(provider_response_universe) != sorted(evaluator_artifact_universe):
        raise IntegrityError(
            "evaluator raw provider responses and evaluator artifacts are not an exact set"
        )
    logged_evaluator_provider_hashes = sorted(
        str(record.get("artifact_hash"))
        for record in access_records
        if record.get("event") == "artifact-access"
        and record.get("role") == "source-blind-evaluation-runner"
        and record.get("action") == "capture-raw-provider-io"
    )
    captured_evaluator_provider_hashes = sorted(
        str(first_snapshot[path]["sha256"]) for path in evaluator_provider_io_paths
    )
    if logged_evaluator_provider_hashes != captured_evaluator_provider_hashes:
        raise IntegrityError(
            "evaluator raw provider I/O files/access records are not an exact set"
        )
    if raw_provider_io_required:
        if not stage_provider_io_paths or not evaluator_provider_io_paths:
            raise IntegrityError(
                "API-weight run requires both stage and evaluator raw provider I/O artifacts"
            )
        for row in bundle_rows:
            if not isinstance(row, Mapping) or row.get("status") != "completed":
                continue
            stage_artifacts = row.get("stage_artifacts")
            if not isinstance(stage_artifacts, Mapping) or any(
                not isinstance(stage_artifacts.get(stage), Mapping)
                or not isinstance(stage_artifacts[stage].get("capture_hashes"), Mapping)
                or "raw-provider-io.json"
                not in stage_artifacts[stage]["capture_hashes"]
                for stage in STAGES
            ):
                raise IntegrityError(
                    "completed API-weight row lacks exact stage raw provider I/O coverage"
                )

    return {
        "schema_version": RUN_ATTESTATION_SCHEMA_VERSION,
        "phase": "pre-statistical-unblinding",
        "run_id": artifact_store.run_id,
        "attested_at": attested_at,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "split": {"id": split_id, "input_sha256": split_hash},
        "corpus_version": corpus_version,
        "execution_manifest_artifact": {
            "path": frozen_execution_path,
            "sha256": execution_entry["sha256"],
        },
        "execution_bundle_artifact": {
            "path": frozen_bundle_path,
            "sha256": bundle_entry["sha256"],
            "document_sha256": sha256_json(execution_bundle),
        },
        "execution_binding": {
            "plan_sha256": execution_document["plan_sha256"],
            "registered_arm_ids_sha256": sha256_json(registered_arm_ids),
            "registered_arm_count": len(registered_arm_ids),
            "sample_ids_sha256": sha256_json(sample_ids),
            "sample_count": len(sample_ids),
            "execution_status": execution_bundle["execution_status"],
            "bundle_row_universe_sha256": sha256_json(bundle_rows),
            "bundle_row_count": len(bundle_rows),
            "endpoint_artifact_universe_sha256": sha256_json(
                sorted(endpoint_artifact_universe)
            ),
            "live_command_config": live_command_config,
            "live_command_config_sha256": sha256_json(live_command_config),
        },
        "endpoint_policy_artifact": {
            "path": frozen_policy_path,
            "sha256": policy_entry["sha256"],
            "document_sha256": sha256_json(policy_document),
        },
        "oracle_graph_artifact": {
            "path": frozen_oracle_path,
            "sha256": oracle_entry["sha256"],
        },
        "allowed_set_artifact": {
            "path": frozen_allowed_path,
            "sha256": allowed_entry["sha256"],
        },
        "adjudication_sha256": {
            key: frozen_adjudication[key] for key in sorted(frozen_adjudication)
        },
        "adjudication_paths": {
            key: frozen_adjudication_paths[key]
            for key in sorted(frozen_adjudication_paths)
        },
        "raw_provider_io": {
            "required": raw_provider_io_required,
            "paths": provider_io_paths,
            "stage_paths": stage_provider_io_paths,
            "evaluator_paths": evaluator_provider_io_paths,
        },
        "artifact_ledger_sha256": artifact_ledger["sha256"],
        "access_log_sha256": access_log["sha256"],
        "run_file_hashes": first_snapshot,
    }


def write_signed_candidate_manifest(
    path: Path, document: Mapping[str, Any], key: bytes, key_id: str
) -> str:
    """Immutably write a signed candidate manifest and return its file SHA-256."""

    _validate_candidate_document(document)
    _validate_signing_material(key, key_id)
    output = Path(path)
    _reject_link_like(output, label="signed candidate output")
    return write_signed_manifest(output, document, key, key_id)


def verify_signed_candidate_manifest(path: Path, key: bytes) -> Mapping[str, Any]:
    """Verify a candidate manifest's HMAC and minimal frozen-document contract."""

    document = _read_and_verify_signed(path, key, CANDIDATE_SCHEMA_VERSION)
    unsigned = dict(document)
    unsigned.pop("signature", None)
    _validate_candidate_document(unsigned)
    return document


def verify_candidate_execution_bindings(
    document: Mapping[str, Any],
    *,
    component_root: Path,
    corpus: Corpus,
    custodian_state_root: Path,
) -> None:
    """Re-hash every owner component and corpus-governance input at execution."""

    unsigned = dict(document)
    unsigned.pop("signature", None)
    _validate_candidate_document(unsigned)
    root = _resolved_directory(component_root, "component root")
    components = document["components"]
    assert isinstance(components, Mapping)
    for name, binding in components.items():
        _validate_component_binding(str(name), binding, component_root=root)
    _validate_external_executable_binding(
        document.get("external_executable_binding"),
        component_root=root,
        verify_files=True,
    )
    expected_source_snapshot = required_source_snapshot(root)
    if document.get("required_source_snapshot") != expected_source_snapshot:
        raise IntegrityError("required executable source changed after candidate freeze")
    corpus_binding = document["corpus_binding"]
    assert isinstance(corpus_binding, Mapping)
    if corpus_binding.get("manifest_sha256") != sha256_file(corpus.root / "manifest.json"):
        raise IntegrityError("candidate corpus manifest changed after freeze")
    support = corpus_binding.get("support_files")
    if not isinstance(support, Mapping):
        raise ContractError("candidate corpus support-file bindings are missing")
    for logical_name, entry in support.items():
        if not isinstance(entry, Mapping):
            raise ContractError(f"corpus support entry {logical_name!r} is invalid")
        path = entry.get("path")
        if not isinstance(path, str):
            raise ContractError(f"corpus support entry {logical_name!r} has no path")
        actual = relative_file_hash_manifest(root, [path])[path]
        if dict(entry) != {"path": path, **actual}:
            raise IntegrityError(f"corpus support file changed: {logical_name!r}")
    if corpus_binding.get("support_files_sha256") != sha256_json(support):
        raise IntegrityError("candidate corpus support-file aggregate changed")
    access_policy = read_json(corpus.manifest_resource_paths(release=True)["access_policy"])
    if not isinstance(access_policy, Mapping):
        raise ContractError("release access policy must be an object")
    actual_custodian = _validate_custodian_state_policy(
        access_policy, custodian_state_root
    )
    if corpus_binding.get("custodian_state") != actual_custodian:
        raise IntegrityError("candidate custodian state binding changed")


def write_signed_run_attestation(
    path: Path,
    document: Mapping[str, Any],
    key: bytes,
    key_id: str,
    *,
    artifact_store: ArtifactStore,
) -> str:
    """Write an immutable signed run attestation outside its attested run root.

    Keeping the signed attestation outside ``run_root`` avoids the impossible
    self-hash cycle that would arise if the claimed complete run-file manifest
    also had to contain its own signature file.  The store is reverified and its
    current tree must still equal the document immediately before signing.
    """

    _validate_run_attestation_document(document)
    _validate_signing_material(key, key_id)
    output = Path(path)
    _reject_link_like(output, label="signed attestation output")
    resolved_parent = output.parent.resolve(strict=False)
    run_root = artifact_store.run_root.resolve(strict=True)
    if resolved_parent == run_root or _is_strictly_within(resolved_parent, run_root):
        raise ContractError("signed run attestation must be stored outside run_root")
    artifact_store.verify()
    current_snapshot = tree_file_hash_manifest(run_root)
    if document.get("run_id") != artifact_store.run_id:
        raise IntegrityError("run attestation run_id does not match ArtifactStore")
    if document.get("run_file_hashes") != current_snapshot:
        raise IntegrityError("run files changed before attestation signing")
    return write_signed_manifest(output, document, key, key_id)


def verify_signed_run_attestation(path: Path, key: bytes) -> Mapping[str, Any]:
    """Verify a run attestation's HMAC and minimal frozen-document contract."""

    document = _read_and_verify_signed(path, key, RUN_ATTESTATION_SCHEMA_VERSION)
    unsigned = dict(document)
    unsigned.pop("signature", None)
    _validate_run_attestation_document(unsigned)
    return document


def verify_attested_run_snapshot(
    artifact_store: ArtifactStore,
    attestation: Mapping[str, Any],
    *,
    allow_append_only_growth: bool = False,
) -> None:
    """Verify that a signed attestation still names the current raw run tree."""

    unsigned = dict(attestation)
    unsigned.pop("signature", None)
    _validate_run_attestation_document(unsigned)
    artifact_store.verify()
    current = tree_file_hash_manifest(artifact_store.run_root)
    attested = attestation.get("run_file_hashes")
    if not isinstance(attested, Mapping):
        raise ContractError("run attestation file manifest is missing")
    valid = (
        _attested_tree_is_unchanged_prefix(artifact_store.run_root, attested, current)
        if allow_append_only_growth
        else current == attested
    )
    if not valid:
        raise IntegrityError("current run tree differs from signed pre-unblinding snapshot")


def build_fresh_confirm_qualification(
    *,
    qualified_at: str,
    pre_unblinding_attestation_path: Path,
    pre_unblinding_attestation: Mapping[str, Any],
    artifact_store: ArtifactStore,
    candidate_manifest_hash: str,
    corpus: Corpus,
    preregistration: Mapping[str, Any],
    experiment_report_path: Path,
    equivalence_path: Path,
    release_evidence_path: Path,
) -> dict[str, Any]:
    """Build the separately signed post-unblinding PASS certificate.

    The pre-unblinding run attestation remains immutable and is referenced by
    file hash.  Statistical/release evidence is bound only in this second
    certificate, preventing a PASS decision from being back-filled into the
    earlier execution attestation.
    """

    qualified_at = _nonempty_text(qualified_at, "qualified_at")
    candidate_manifest_hash = _require_sha256(
        candidate_manifest_hash, "candidate_manifest_hash"
    )
    unsigned_attestation = dict(pre_unblinding_attestation)
    unsigned_attestation.pop("signature", None)
    _validate_run_attestation_document(unsigned_attestation)
    artifact_store.verify()
    current_tree = tree_file_hash_manifest(artifact_store.run_root)
    attested_tree = pre_unblinding_attestation.get("run_file_hashes")
    if not isinstance(attested_tree, Mapping) or not _attested_tree_is_unchanged_prefix(
        artifact_store.run_root, attested_tree, current_tree
    ):
        raise IntegrityError("a pre-unblinding attested run file has changed or disappeared")
    pre_path = Path(pre_unblinding_attestation_path).resolve(strict=True)
    pre_hash = sha256_file(pre_path)
    report = _read_mapping_file(experiment_report_path, "experiment report")
    equivalence = _read_mapping_file(equivalence_path, "equivalence evidence")
    release_evidence = _read_mapping_file(release_evidence_path, "release evidence")
    expected_report_path = (artifact_store.run_root / "experiment-report.json").resolve(
        strict=True
    )
    if expected_report_path != Path(experiment_report_path).resolve(strict=True):
        raise IntegrityError("qualification report is not the current run's captured report")
    report_equivalence_artifact = report.get("fresh_confirm_equivalence_artifact")
    if not isinstance(report_equivalence_artifact, Mapping) or not isinstance(
        report_equivalence_artifact.get("path"), str
    ):
        raise ContractError("experiment report has no equivalence artifact path")
    _, report_equivalence_relative = _normalize_relative_file_path(
        str(report_equivalence_artifact["path"])
    )
    expected_equivalence_path = (
        artifact_store.run_root / report_equivalence_relative
    ).resolve(strict=True)
    if expected_equivalence_path != Path(equivalence_path).resolve(strict=True):
        raise IntegrityError("qualification equivalence file is not the report-bound artifact")
    if report_equivalence_artifact.get("sha256") != sha256_file(
        expected_equivalence_path
    ):
        raise IntegrityError("report-bound equivalence file hash mismatch")
    policy_binding = pre_unblinding_attestation.get("endpoint_policy_artifact")
    if not isinstance(policy_binding, Mapping) or not isinstance(
        policy_binding.get("path"), str
    ):
        raise ContractError("pre-unblinding attestation has no endpoint policy path")
    endpoint_policy = _read_run_json(artifact_store, str(policy_binding["path"]))
    criteria, binding = _assess_fresh_confirm_release(
        artifact_store=artifact_store,
        candidate_manifest_hash=candidate_manifest_hash,
        corpus=corpus,
        preregistration=preregistration,
        pre_attestation=pre_unblinding_attestation,
        endpoint_policy=endpoint_policy,
        report=report,
        equivalence=equivalence,
        release_evidence=release_evidence,
    )
    if not all(criteria.values()):
        failed = sorted(key for key, passed in criteria.items() if not passed)
        raise ContractError(f"fresh-confirm release qualification did not pass: {failed}")
    return {
        "schema_version": RELEASE_QUALIFICATION_SCHEMA_VERSION,
        "qualified_at": qualified_at,
        "status": "PASS",
        "binding": binding,
        "pre_unblinding_attestation_sha256": pre_hash,
        "criteria": criteria,
        "evidence": {
            "experiment_report": {
                "file_sha256": sha256_file(Path(experiment_report_path)),
                "document_sha256": sha256_json(report),
                "document": report,
            },
            "fresh_confirm_equivalence": {
                "file_sha256": sha256_file(Path(equivalence_path)),
                "document_sha256": sha256_json(equivalence),
                "document": equivalence,
            },
            "release_evidence": {
                "file_sha256": sha256_file(Path(release_evidence_path)),
                "document_sha256": sha256_json(release_evidence),
                "document": release_evidence,
            },
            "endpoint_policy": {
                "document_sha256": sha256_json(endpoint_policy),
                "document": endpoint_policy,
            },
        },
    }


def write_signed_fresh_confirm_qualification(
    path: Path, document: Mapping[str, Any], key: bytes, key_id: str
) -> str:
    _validate_qualification_document(document)
    _validate_signing_material(key, key_id)
    return write_signed_manifest(Path(path), document, key, key_id)


def verify_signed_fresh_confirm_qualification(
    path: Path,
    key: bytes,
    *,
    pre_unblinding_attestation_path: Path,
    candidate_manifest_hash: str,
    corpus: Corpus,
    preregistration: Mapping[str, Any],
    artifact_store: ArtifactStore,
) -> Mapping[str, Any]:
    document = _read_and_verify_signed(
        Path(path), key, RELEASE_QUALIFICATION_SCHEMA_VERSION
    )
    unsigned = dict(document)
    unsigned.pop("signature", None)
    _validate_qualification_document(unsigned)
    if document.get("pre_unblinding_attestation_sha256") != sha256_file(
        Path(pre_unblinding_attestation_path).resolve(strict=True)
    ):
        raise IntegrityError("qualification references a different pre-unblinding attestation")
    evidence = document["evidence"]
    assert isinstance(evidence, Mapping)
    extracted: dict[str, Mapping[str, Any]] = {}
    for name in (
        "experiment_report",
        "fresh_confirm_equivalence",
        "release_evidence",
        "endpoint_policy",
    ):
        entry = evidence[name]
        assert isinstance(entry, Mapping)
        payload = entry["document"]
        assert isinstance(payload, Mapping)
        if entry.get("document_sha256") != sha256_json(payload):
            raise IntegrityError(f"qualification embedded {name} hash mismatch")
        extracted[name] = payload
    pre_attestation = verify_signed_run_attestation(
        Path(pre_unblinding_attestation_path), key
    )
    criteria, binding = _assess_fresh_confirm_release(
        artifact_store=artifact_store,
        candidate_manifest_hash=candidate_manifest_hash,
        corpus=corpus,
        preregistration=preregistration,
        pre_attestation=pre_attestation,
        endpoint_policy=extracted["endpoint_policy"],
        report=extracted["experiment_report"],
        equivalence=extracted["fresh_confirm_equivalence"],
        release_evidence=extracted["release_evidence"],
    )
    if document.get("binding") != binding or document.get("criteria") != criteria:
        raise IntegrityError("qualification binding/criteria do not match embedded evidence")
    if document.get("status") != "PASS" or not all(criteria.values()):
        raise ContractError("blind opening requires a verified fresh-confirm PASS")
    return document


def _read_mapping_file(path: Path, label: str) -> Mapping[str, Any]:
    value = read_json(Path(path).resolve(strict=True))
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object")
    return value


def _attested_tree_is_unchanged_prefix(
    run_root: Path,
    attested: Mapping[str, Any],
    current: Mapping[str, Any],
) -> bool:
    append_only = {"artifact-ledger.jsonl", "access-log.jsonl"}
    for path, old_entry in attested.items():
        new_entry = current.get(path)
        if not isinstance(old_entry, Mapping) or not isinstance(new_entry, Mapping):
            return False
        if path not in append_only:
            if old_entry != new_entry:
                return False
            continue
        old_size = old_entry.get("size")
        old_hash = old_entry.get("sha256")
        new_size = new_entry.get("size")
        if (
            isinstance(old_size, bool)
            or not isinstance(old_size, int)
            or not isinstance(new_size, int)
            or new_size < old_size
            or not isinstance(old_hash, str)
        ):
            return False
        digest = hashlib.sha256()
        with (run_root / path).open("rb") as handle:
            remaining = old_size
            while remaining:
                chunk = handle.read(min(1024 * 1024, remaining))
                if not chunk:
                    return False
                digest.update(chunk)
                remaining -= len(chunk)
        if digest.hexdigest() != old_hash:
            return False
    return True


def _assess_fresh_confirm_release(
    *,
    artifact_store: ArtifactStore,
    candidate_manifest_hash: str,
    corpus: Corpus,
    preregistration: Mapping[str, Any],
    pre_attestation: Mapping[str, Any],
    endpoint_policy: Mapping[str, Any],
    report: Mapping[str, Any],
    equivalence: Mapping[str, Any],
    release_evidence: Mapping[str, Any],
) -> tuple[dict[str, bool], dict[str, Any]]:
    """Reconstruct a release decision from phase-A bytes, never report claims."""

    if not isinstance(artifact_store, ArtifactStore):
        raise ContractError("fresh-confirm assessment requires an ArtifactStore")
    candidate_manifest_hash = _require_sha256(
        candidate_manifest_hash, "candidate manifest hash"
    )
    split_hash = corpus.split_hash("fresh-confirm")
    run_id = _nonempty_text(pre_attestation.get("run_id"), "attested run_id")
    if artifact_store.run_id != run_id:
        raise IntegrityError("qualification ArtifactStore is for a different run")
    if pre_attestation.get("candidate_manifest_sha256") != candidate_manifest_hash:
        raise IntegrityError("fresh-confirm attestation candidate mismatch")
    split = pre_attestation.get("split")
    if not isinstance(split, Mapping) or split != {
        "id": "fresh-confirm",
        "input_sha256": split_hash,
    }:
        raise IntegrityError("qualification requires the current fresh-confirm split")
    if pre_attestation.get("corpus_version") != corpus.corpus_version:
        raise IntegrityError("fresh-confirm attestation corpus-version mismatch")

    artifact_store.verify()
    current_tree = tree_file_hash_manifest(artifact_store.run_root)
    attested_run_files = pre_attestation.get("run_file_hashes")
    if not isinstance(attested_run_files, Mapping) or not _attested_tree_is_unchanged_prefix(
        artifact_store.run_root, attested_run_files, current_tree
    ):
        raise IntegrityError("phase-A run bytes changed before qualification")

    attested_policy = pre_attestation.get("endpoint_policy_artifact")
    if not isinstance(attested_policy, Mapping):
        raise ContractError("phase-A attestation has no endpoint-policy artifact")
    policy_path = attested_policy.get("path")
    if not isinstance(policy_path, str):
        raise ContractError("phase-A endpoint-policy path is missing")
    policy_file_entry = attested_run_files.get(policy_path)
    if not isinstance(policy_file_entry, Mapping) or policy_file_entry.get(
        "sha256"
    ) != attested_policy.get("sha256"):
        raise IntegrityError("phase-A endpoint-policy file binding mismatch")
    authoritative_policy = _read_run_json(artifact_store, policy_path)
    if authoritative_policy != endpoint_policy:
        raise IntegrityError("qualification endpoint policy differs from phase-A bytes")
    policy_sha = sha256_json(authoritative_policy)
    if attested_policy.get("document_sha256") != policy_sha:
        raise IntegrityError("qualification endpoint policy is not the attested policy")
    policy_binding = authoritative_policy.get("binding")
    if not isinstance(policy_binding, Mapping):
        raise ContractError("qualification endpoint policy binding is missing")
    expected_policy_binding = {
        "run_id": run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus.corpus_version,
        "split": "fresh-confirm",
        "split_sha256": split_hash,
        "seed": preregistration.get("seed"),
    }
    if dict(policy_binding) != expected_policy_binding:
        raise IntegrityError("qualification endpoint policy binding mismatch")

    topology = authoritative_policy.get("sample_atomic_cluster_ids")
    allowed = authoritative_policy.get("allowed_set_by_sample")
    raters_by_sample = authoritative_policy.get(
        "source_blind_evaluator_ids_by_sample"
    )
    adjudicators_by_sample = authoritative_policy.get(
        "out_of_set_adjudicator_ids_by_sample"
    )
    for label, mapping in (
        ("sample topology", topology),
        ("allowed set", allowed),
        ("rater roster", raters_by_sample),
        ("adjudicator roster", adjudicators_by_sample),
    ):
        if not isinstance(mapping, Mapping) or not mapping:
            raise ContractError(f"endpoint policy {label} is missing")
    assert isinstance(topology, Mapping)
    assert isinstance(allowed, Mapping)
    assert isinstance(raters_by_sample, Mapping)
    assert isinstance(adjudicators_by_sample, Mapping)
    sample_ids = set(topology)
    if set(allowed) != sample_ids or set(raters_by_sample) != sample_ids or set(
        adjudicators_by_sample
    ) != sample_ids:
        raise IntegrityError("endpoint policy sample domains are not exact")
    for field, value in (
        ("allowed_set_sha256", allowed),
        ("sample_topology_sha256", topology),
        ("rater_roster_sha256", raters_by_sample),
        ("adjudicator_roster_sha256", adjudicators_by_sample),
    ):
        if authoritative_policy.get(field) != sha256_json(value):
            raise IntegrityError(f"endpoint policy {field} is not content-derived")

    minimums = preregistration.get("minimum_effective_counts")
    if not isinstance(minimums, Mapping):
        raise ContractError("minimum_effective_counts is missing")
    minimum_clusters = int(minimums.get("atomic_clusters", 0))
    minimum_raters = int(minimums.get("held_out_human_raters", 0))
    role_roster = corpus.release_role_assignments(
        split="fresh-confirm", minimum_held_out_raters=minimum_raters
    )
    for sample_id, cluster in topology.items():
        if not isinstance(sample_id, str) or not isinstance(cluster, str):
            raise ContractError("endpoint sample topology must map text to text")
        expected_roles = role_roster.get(cluster)
        if expected_roles is None:
            raise IntegrityError(f"endpoint policy uses undeclared cluster {cluster!r}")
        actual_raters = raters_by_sample[sample_id]
        actual_adjudicators = adjudicators_by_sample[sample_id]
        if actual_raters != list(expected_roles["held_out_rater_ids"]):
            raise IntegrityError(f"endpoint policy has fake/missing raters for {sample_id!r}")
        if actual_adjudicators != list(expected_roles["adjudicator_ids"]):
            raise IntegrityError(
                f"endpoint policy has fake/missing adjudicators for {sample_id!r}"
            )

    chains = preregistration.get("oracle_chains")
    if not isinstance(chains, list) or len(chains) != 2:
        raise ContractError("qualification requires exactly two frozen oracle chains")
    plan = build_arm_plan(tuple(str(chain) for chain in chains))
    plan_rows = [
        {
            "arm_id": arm.arm_id,
            "sources": {stage: source.value for stage, source in arm.sources.items()},
            "oracle_chain": arm.oracle_chain,
            "focus_stage": arm.focus_stage,
            "context": arm.context,
        }
        for arm in plan
    ]
    expected_plan_hash = sha256_json(plan_rows)
    sample_universe = [
        {"sample_id": sample_id, "atomic_cluster_id": topology[sample_id]}
        for sample_id in sorted(sample_ids)
    ]
    sample_universe_hash = sha256_json(sample_universe)
    execution_binding = pre_attestation.get("execution_binding")
    if not isinstance(execution_binding, Mapping) or not all(
        (
            execution_binding.get("plan_sha256") == expected_plan_hash,
            execution_binding.get("registered_arm_count") == 313,
            execution_binding.get("registered_arm_ids_sha256")
            == sha256_json(sorted(arm.arm_id for arm in plan)),
            execution_binding.get("sample_ids_sha256")
            == sha256_json(sorted(sample_ids)),
            execution_binding.get("sample_count") == len(sample_ids),
        )
    ):
        raise IntegrityError("pre-unblinding execution universe binding mismatch")

    bundle_artifact = pre_attestation.get("execution_bundle_artifact")
    if not isinstance(bundle_artifact, Mapping):
        raise ContractError("phase-A attestation has no execution bundle")
    bundle_path = bundle_artifact.get("path")
    if not isinstance(bundle_path, str):
        raise ContractError("phase-A execution-bundle path is missing")
    bundle_file_entry = attested_run_files.get(bundle_path)
    if not isinstance(bundle_file_entry, Mapping) or bundle_file_entry.get(
        "sha256"
    ) != bundle_artifact.get("sha256"):
        raise IntegrityError("phase-A execution-bundle file binding mismatch")
    bundle = _read_run_json(artifact_store, bundle_path)
    if sha256_json(bundle) != bundle_artifact.get("document_sha256"):
        raise IntegrityError("phase-A execution-bundle document hash mismatch")
    if bundle.get("schema_version") != "chesstory.eval.pre-unblinding-execution.v1":
        raise ContractError("unsupported phase-A execution-bundle schema")
    if bundle.get("binding") != {
        "run_id": run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus.corpus_version,
        "split": "fresh-confirm",
        "split_sha256": split_hash,
        "seed": preregistration.get("seed"),
    }:
        raise IntegrityError("phase-A execution-bundle binding is not exact")
    live_command_config = _validate_live_command_config_binding(
        artifact_store=artifact_store,
        binding=bundle.get("live_command_config"),
        run_file_hashes=attested_run_files,
    )
    if (
        execution_binding.get("live_command_config") != live_command_config
        or execution_binding.get("live_command_config_sha256")
        != sha256_json(live_command_config)
    ):
        raise IntegrityError(
            "phase-A execution binding does not exactly retain live-command config"
        )
    execution_manifest_artifact = pre_attestation.get("execution_manifest_artifact")
    if not isinstance(execution_manifest_artifact, Mapping) or not isinstance(
        execution_manifest_artifact.get("path"), str
    ):
        raise ContractError("phase-A execution-manifest artifact is missing")
    execution_manifest_path = str(execution_manifest_artifact["path"])
    execution_manifest_entry = attested_run_files.get(execution_manifest_path)
    if not isinstance(execution_manifest_entry, Mapping) or execution_manifest_entry.get(
        "sha256"
    ) != execution_manifest_artifact.get("sha256"):
        raise IntegrityError("phase-A execution-manifest file binding mismatch")
    execution_manifest = _read_run_json(artifact_store, execution_manifest_path)
    if execution_manifest.get("live_command_config") != live_command_config:
        raise IntegrityError(
            "phase-A execution manifest/bundle live-command config bindings differ"
        )
    if any(
        (
            bundle.get("plan") != plan_rows,
            bundle.get("plan_sha256") != expected_plan_hash,
            bundle.get("registered_arm_count") != 313,
            bundle.get("sample_universe") != sample_universe,
            bundle.get("sample_universe_sha256") != sample_universe_hash,
            bundle.get("split_sample_count") != len(sample_ids),
            bundle.get("atomic_cluster_count")
            != len({str(cluster) for cluster in topology.values()}),
            bundle.get("endpoint_policy_sha256") != policy_sha,
            bundle.get("endpoint_allowed_set_source")
            != "pre-unblinding-frozen-run-artifact",
        )
    ):
        raise IntegrityError("phase-A bundle differs from the frozen plan/policy universe")
    rows = bundle.get("rows")
    controls = bundle.get("foundation_controls")
    ceiling = bundle.get("all_oracle_ceiling")
    if not isinstance(rows, list) or not isinstance(controls, Mapping) or not isinstance(
        ceiling, Mapping
    ):
        raise ContractError("phase-A bundle rows/controls/ceiling are missing")
    expected_pairs = {
        (sample_id, arm.arm_id) for sample_id in sample_ids for arm in plan
    }
    if bundle.get("expected_row_count") != len(expected_pairs):
        raise IntegrityError("phase-A bundle expected-row count is inconsistent")

    actual_pairs: set[tuple[str, str]] = set()
    endpoint_universe: list[tuple[str, str, str]] = []
    exact_run_binding = {
        "run_id": run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus.corpus_version,
        "split": "fresh-confirm",
        "split_sha256": split_hash,
        "seed": preregistration.get("seed"),
    }
    for row in rows:
        if not isinstance(row, Mapping):
            raise ContractError("phase-A execution row is not an object")
        sample_id = row.get("sample_id")
        arm_id = row.get("arm_id")
        if not isinstance(sample_id, str) or not isinstance(arm_id, str):
            raise ContractError("phase-A execution row identity is malformed")
        pair = (sample_id, arm_id)
        if pair in actual_pairs:
            raise IntegrityError(f"duplicate phase-A sample/arm row: {pair!r}")
        actual_pairs.add(pair)
        endpoint_document = _validate_execution_row_artifacts(
            artifact_store=artifact_store,
            row=row,
            run_file_hashes=attested_run_files,
            exact_run_binding=exact_run_binding,
            sample_topology=topology,
            registered_arm_ids={arm.arm_id for arm in plan},
        )
        if endpoint_document is not None:
            endpoint_ref = row["endpoint_artifact"]
            assert isinstance(endpoint_ref, Mapping)
            endpoint_universe.append((sample_id, arm_id, str(endpoint_ref["sha256"])))
    rows_completed = bool(rows) and all(
        isinstance(row, Mapping) and row.get("status") == "completed" for row in rows
    )
    if any(
        (
            len(plan) != 313,
            actual_pairs != expected_pairs,
            not rows_completed,
            bundle.get("execution_status") != "ready-for-analysis",
            bundle.get("executed_arm_count") != 313,
            execution_binding.get("bundle_row_universe_sha256") != sha256_json(rows),
            execution_binding.get("bundle_row_count") != len(rows),
            execution_binding.get("endpoint_artifact_universe_sha256")
            != sha256_json(sorted(endpoint_universe)),
        )
    ):
        raise IntegrityError("phase-A bundle is not the exact completed 313-arm universe")

    recomputed_ceiling = _recompute_all_oracle_ceiling(
        rows=rows,
        plan=plan,
        chains=[str(chain) for chain in chains],
        preregistration=preregistration,
    )
    if ceiling != recomputed_ceiling:
        raise IntegrityError("phase-A all-oracle ceiling is not endpoint-derived")
    if controls.get("status") != "passed" or ceiling.get("status") != "passed":
        raise ContractError("fresh-confirm qualification requires passed phase-A gates")

    epsilon_registry = preregistration.get("epsilon_gain")
    if not isinstance(epsilon_registry, Mapping) or len(epsilon_registry) != 156:
        raise ContractError(
            "fresh-confirm qualification requires an exact 156-contrast epsilon registry"
        )
    for contrast_id, epsilon in epsilon_registry.items():
        if (
            not isinstance(contrast_id, str)
            or isinstance(epsilon, bool)
            or not isinstance(epsilon, (int, float))
            or not math.isfinite(float(epsilon))
            or float(epsilon) < 0.0
        ):
            raise ContractError("epsilon registry entries are invalid")

    hypothesis = normalize_selected_attribution_hypothesis(
        preregistration, [str(chain) for chain in chains]
    )
    bootstrap = preregistration.get("bootstrap")
    if not isinstance(bootstrap, Mapping):
        raise ContractError("fresh-confirm bootstrap configuration is missing")
    alternative = "greater" if hypothesis["direction"] == "positive" else "less"
    recomputed_contrasts = summarize_registered_contrasts(
        rows,
        plan,
        iterations=int(bootstrap.get("iterations", 0)),
        seed=int(bootstrap.get("seed", preregistration.get("seed", 0))),
        confidence=float(bootstrap.get("confidence", 0.95)),
        alternative=alternative,
    )
    attribution_minimum_deltas = resolve_attribution_minimum_deltas(
        preregistration,
        recomputed_contrasts,
        [str(chain) for chain in chains],
    )
    selected_contrasts = select_attribution_contrasts(
        hypothesis=hypothesis,
        contrasts=recomputed_contrasts,
        oracle_chains=[str(chain) for chain in chains],
    )

    row_universe_hash = _row_universe_sha256(rows)
    recomputed_equivalence = _recompute_fresh_confirm_equivalence(
        rows=rows,
        plan=plan,
        sample_universe=sample_universe,
        controls=controls,
        ceiling=ceiling,
        preregistration=preregistration,
        run_binding=exact_run_binding,
        plan_hash=expected_plan_hash,
        sample_universe_hash=sample_universe_hash,
        endpoint_policy_hash=policy_sha,
    )
    _require_exact_derived_document(
        actual=equivalence,
        expected=recomputed_equivalence,
        label="equivalence evidence",
    )

    equivalence_artifact = report.get("fresh_confirm_equivalence_artifact")
    if not isinstance(equivalence_artifact, Mapping) or set(equivalence_artifact) != {
        "path",
        "sha256",
    }:
        raise ContractError("report equivalence artifact binding is not exact")
    equivalence_path = equivalence_artifact.get("path")
    equivalence_digest = equivalence_artifact.get("sha256")
    if not isinstance(equivalence_path, str):
        raise ContractError("report equivalence artifact path is missing")
    equivalence_digest = _require_sha256(
        equivalence_digest, "report equivalence artifact hash"
    )
    if equivalence_digest != sha256_json(recomputed_equivalence):
        raise IntegrityError("report equivalence artifact hash is not content-derived")
    post_documents = _post_attestation_run_document_bindings(
        artifact_store=artifact_store,
        pre_attestation=pre_attestation,
        expected={
            equivalence_path: equivalence_digest,
            "experiment-report.json": sha256_json(report),
        },
    )
    if post_documents[equivalence_path]["index"] >= post_documents[
        "experiment-report.json"
    ]["index"]:
        raise IntegrityError("equivalence must be captured before the experiment report")

    report_binding = {
        "run_id": run_id,
        "candidate_manifest_hash": candidate_manifest_hash,
        "corpus_version": corpus.corpus_version,
        "split": "fresh-confirm",
        "split_input_hash": split_hash,
        "endpoint_policy_hash": policy_sha,
        "seed": preregistration.get("seed"),
    }
    for key, expected_value in report_binding.items():
        if report.get(key) != expected_value:
            raise IntegrityError(f"fresh-confirm report {key} binding mismatch")
    expected_report_values = {
        "schema_version": "chesstory.eval.experiment-report.v1",
        "split_sample_count": len(sample_ids),
        "atomic_cluster_count": len({str(cluster) for cluster in topology.values()}),
        "registered_arm_count": 313,
        "expected_row_count": len(expected_pairs),
        "executed_arm_count": 313,
        "status": "completed-fresh-confirm",
        "endpoint_allowed_set_source": "pre-unblinding-frozen-run-artifact",
        "live_command_config": bundle.get("live_command_config"),
        "preregistration_hash": sha256_json(preregistration),
        "plan_hash": expected_plan_hash,
        "sample_universe_sha256": sample_universe_hash,
        "foundation_controls": controls,
        "all_oracle_ceiling": ceiling,
        "attribution": {
            "status": "requires-fresh-confirm-qualification",
            "reason": (
                "fresh-confirm contrasts require the separately signed effect-direction, "
                "equivalence, and release-metric qualification decision"
            ),
            "production_bottleneck": None,
        },
        "statistical_contrasts": recomputed_contrasts,
        "fresh_confirm_equivalence": recomputed_equivalence,
        "fresh_confirm_equivalence_artifact": {
            "path": equivalence_path,
            "sha256": equivalence_digest,
        },
        "rows": rows,
        "artifact_ledger_hash_before_report": post_documents[
            "experiment-report.json"
        ]["previous_ledger_sha256"],
        "access_log_hash": pre_attestation.get("access_log_sha256"),
        "pre_unblinding_execution_sha256": bundle_artifact.get("document_sha256"),
    }
    exact_report_keys = set(report_binding) | set(expected_report_values) | {"created_at"}
    if set(report) != exact_report_keys:
        raise ContractError("fresh-confirm report fields are not exact")
    _nonempty_text(report.get("created_at"), "fresh-confirm report created_at")
    for key, expected_value in expected_report_values.items():
        if report.get(key) != expected_value:
            raise IntegrityError(f"fresh-confirm report {key} is not authoritative")

    equivalence_passed = _validate_equivalence_certificate(
        recomputed_equivalence,
        epsilon_registry=epsilon_registry,
        minimum_clusters=minimum_clusters,
        chains=[str(chain) for chain in chains],
        expected_binding={
            "run_id": run_id,
            "candidate_manifest_sha256": candidate_manifest_hash,
            "corpus_version": corpus.corpus_version,
            "split": "fresh-confirm",
            "split_sha256": split_hash,
            "seed": preregistration.get("seed"),
            "preregistration_sha256": sha256_json(preregistration),
            "plan_sha256": expected_plan_hash,
            "registered_arm_count": 313,
            "sample_universe_sha256": sample_universe_hash,
            "row_universe_sha256": row_universe_hash,
            "epsilon_registry_sha256": sha256_json(epsilon_registry),
            "endpoint_policy_sha256": policy_sha,
        },
        expected_row_count=len(expected_pairs),
    )

    expected_release_binding = {
        "run_id": run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus.corpus_version,
        "split": "fresh-confirm",
        "split_input_sha256": split_hash,
        "endpoint_policy_sha256": policy_sha,
        "experiment_report_sha256": sha256_json(report),
        "equivalence_sha256": sha256_json(equivalence),
        "attribution_minimum_delta_sha256": sha256_json(
            attribution_minimum_deltas
        ),
    }
    expected_endpoint_e = _release_endpoint_evidence(
        rows=rows,
        plan=plan,
        sample_ids=sorted(sample_ids),
        raters_by_sample=raters_by_sample,
        endpoint_policy=authoritative_policy,
        preregistration=preregistration,
    )
    expected_effect = _release_effect_evidence(
        hypothesis=hypothesis,
        selected_contrasts=selected_contrasts,
        chains=[str(chain) for chain in chains],
        preregistration=preregistration,
        attribution_minimum_deltas=attribution_minimum_deltas,
    )
    expected_release_evidence = {
        "schema_version": "chesstory.eval.release-evidence.v1",
        "binding": expected_release_binding,
        "endpoint_E": expected_endpoint_e,
        "effect_direction_replication": expected_effect,
    }
    _require_exact_derived_document(
        actual=release_evidence,
        expected=expected_release_evidence,
        label="release evidence",
    )
    endpoint_passed = expected_endpoint_e["status"] == "passed"
    effect_passed = expected_effect["status"] == "passed"
    held_out_verified = (
        expected_endpoint_e["held_out_rater_ids_by_sample"]
        == {sample_id: raters_by_sample[sample_id] for sample_id in sorted(sample_ids)}
        and set(expected_endpoint_e["evaluator_artifact_sha256_by_sample"])
        == sample_ids
    )
    complete_plan = True
    foundation_passed = controls.get("status") == "passed"
    ceilings_passed = ceiling.get("status") == "passed"

    criteria = {
        "foundation_controls_passed": foundation_passed,
        "both_oracle_ceilings_passed": ceilings_passed,
        "endpoint_E_passed": endpoint_passed,
        "effect_direction_replicated": effect_passed,
        "equivalence_bounds_established": equivalence_passed,
        "complete_frozen_plan_executed": complete_plan,
        "held_out_rater_roster_verified": held_out_verified,
    }
    binding = {
        "run_id": run_id,
        "candidate_manifest_sha256": candidate_manifest_hash,
        "corpus_version": corpus.corpus_version,
        "split": "fresh-confirm",
        "split_input_sha256": split_hash,
        "endpoint_policy_sha256": policy_sha,
        "plan_sha256": expected_plan_hash,
        "registered_arm_count": 313,
        "epsilon_registry_sha256": sha256_json(epsilon_registry),
        "epsilon_contrast_count": 156,
        "sample_universe_sha256": sample_universe_hash,
        "expected_row_count": len(expected_pairs),
        "selected_attribution_hypothesis_sha256": sha256_json(hypothesis),
        "selected_contrast_ids_sha256": sha256_json(
            hypothesis["contrast_ids_by_oracle_chain"]
        ),
        "attribution_minimum_delta_sha256": sha256_json(
            attribution_minimum_deltas
        ),
        "live_command_config_sha256": sha256_json(live_command_config),
    }
    return criteria, binding


def _row_universe_sha256(rows: Sequence[Mapping[str, Any]]) -> str:
    bindings: list[dict[str, Any]] = []
    for row in sorted(
        rows,
        key=lambda value: (
            str(value.get("sample_id", "")),
            str(value.get("arm_id", "")),
        ),
    ):
        endpoint_artifact = row.get("endpoint_artifact")
        stage_artifacts = row.get("stage_artifacts")
        bindings.append(
            {
                "sample_id": row.get("sample_id"),
                "atomic_cluster_id": row.get("atomic_cluster_id"),
                "arm_id": row.get("arm_id"),
                "status": row.get("status"),
                "E": row.get("E"),
                "endpoint_artifact_sha256": (
                    endpoint_artifact.get("sha256")
                    if isinstance(endpoint_artifact, Mapping)
                    else None
                ),
                "evaluator_artifact_sha256": row.get("evaluator_artifact_hash"),
                "stage_canonical_sha256": {
                    stage: value.get("canonical_output_hash")
                    for stage, value in sorted(
                        stage_artifacts.items()
                        if isinstance(stage_artifacts, Mapping)
                        else ()
                    )
                    if isinstance(stage, str) and isinstance(value, Mapping)
                },
            }
        )
    return sha256_json(bindings)


def _require_exact_derived_document(
    *, actual: Mapping[str, Any], expected: Mapping[str, Any], label: str
) -> None:
    if dict(actual) != dict(expected):
        raise IntegrityError(f"{label} differs from deterministic authoritative evidence")


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
            raise ContractError("release endpoint E must be binary")
        cluster = row.get("atomic_cluster_id")
        if not isinstance(cluster, str) or not cluster:
            raise ContractError("release endpoint row is missing atomic_cluster_id")
        clusters.setdefault(cluster, []).append(int(value))
    summaries = [(sum(clusters[key]), len(clusters[key])) for key in sorted(clusters)]
    if not summaries:
        raise ContractError("release cluster bootstrap requires rows")
    if iterations <= 0 or not 0.0 < confidence < 1.0:
        raise ContractError("release cluster bootstrap configuration is invalid")
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
    position = (len(draws) - 1) * (1.0 - confidence)
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return draws[lower]
    weight = position - lower
    return draws[lower] * (1.0 - weight) + draws[upper] * weight


def _derived_release_seed(seed: int, purpose: str, identity: str) -> int:
    payload = f"{seed}\x00{purpose}\x00{identity}".encode("utf-8")
    return int.from_bytes(hashlib.sha256(payload).digest()[:8], "big")


def _recompute_all_oracle_ceiling(
    *,
    rows: Sequence[Mapping[str, Any]],
    plan: Sequence[Arm],
    chains: Sequence[str],
    preregistration: Mapping[str, Any],
) -> dict[str, Any]:
    threshold = float(preregistration.get("ceiling_min_success_rate", 1.0))
    minimums = preregistration.get("minimum_effective_counts")
    bootstrap = preregistration.get("bootstrap")
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
        arm_ids = {
            arm.arm_id
            for arm in plan
            if arm.context == ALL_ORACLE_CONTEXT and arm.oracle_chain == chain
        }
        selected = [row for row in rows if row.get("arm_id") in arm_ids]
        completed = [row for row in selected if row.get("status") == "completed"]
        rate = (
            sum(int(row["E"]) for row in completed) / len(completed)
            if len(completed) == len(selected) and selected
            else None
        )
        cluster_count = len({str(row["atomic_cluster_id"]) for row in completed})
        rater_counts = [
            int(row.get("diagnostics", {}).get("held_out_human_rater_count", 0))
            for row in completed
            if isinstance(row.get("diagnostics"), Mapping)
        ]
        minimum_observed_raters = (
            min(rater_counts)
            if len(rater_counts) == len(completed) and rater_counts
            else 0
        )
        lower_bound = (
            _cluster_success_lower_bound(
                completed,
                iterations=iterations,
                confidence=confidence,
                seed=_derived_release_seed(
                    int(preregistration.get("seed", 0)),
                    "all-oracle-ceiling",
                    chain,
                ),
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


def _recompute_fresh_confirm_equivalence(
    *,
    rows: Sequence[Mapping[str, Any]],
    plan: Sequence[Arm],
    sample_universe: Sequence[Mapping[str, Any]],
    controls: Mapping[str, Any],
    ceiling: Mapping[str, Any],
    preregistration: Mapping[str, Any],
    run_binding: Mapping[str, Any],
    plan_hash: str,
    sample_universe_hash: str,
    endpoint_policy_hash: str,
) -> Mapping[str, Any]:
    """Reproduce the runner's pure equivalence analysis from phase-A rows."""

    epsilon = preregistration.get("epsilon_gain")
    epsilon_hash = sha256_json(epsilon) if isinstance(epsilon, Mapping) else None
    row_universe_hash = _row_universe_sha256(rows)
    binding = {
        **dict(run_binding),
        "preregistration_sha256": sha256_json(preregistration),
        "plan_sha256": plan_hash,
        "registered_arm_count": len(plan),
        "sample_universe_sha256": sample_universe_hash,
        "row_universe_sha256": row_universe_hash,
        "epsilon_registry_sha256": epsilon_hash,
        "endpoint_policy_sha256": endpoint_policy_hash,
    }

    reasons: list[str] = []
    if controls.get("status") != "passed":
        reasons.append("foundation-controls-not-passed")
    if ceiling.get("status") != "passed":
        reasons.append("all-oracle-ceiling-not-passed")
    if not isinstance(run_binding.get("candidate_manifest_sha256"), str) or not (
        _SHA256_RE.fullmatch(str(run_binding.get("candidate_manifest_sha256")))
    ):
        reasons.append("signed-candidate-binding-missing")
    if not isinstance(run_binding.get("split_sha256"), str) or not _SHA256_RE.fullmatch(
        str(run_binding.get("split_sha256"))
    ):
        reasons.append("split-binding-missing")
    if not _SHA256_RE.fullmatch(endpoint_policy_hash):
        reasons.append("frozen-endpoint-policy-missing")
    if len(plan) != 313:
        reasons.append("full-frozen-arm-plan-required")

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

    minimums = preregistration.get("minimum_effective_counts")
    bootstrap = preregistration.get("bootstrap")
    multiplicity = preregistration.get("multiplicity")
    equivalence_policy = preregistration.get("equivalence")
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
        evaluator_hash = row.get("evaluator_artifact_hash")
        if row.get("status") == "completed" and (
            not isinstance(evaluator_hash, str)
            or not _SHA256_RE.fullmatch(evaluator_hash)
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
        seed=int(bootstrap.get("seed", run_binding.get("seed", 0))),
        split="fresh-confirm",
        fresh_confirm=True,
        min_clusters=int(minimums.get("atomic_clusters", 0)),
        alpha=float(multiplicity.get("family_wise_alpha", 0.05)),
    )
    return {**result, "binding": binding, "gate": gate}


def _post_attestation_run_document_bindings(
    *,
    artifact_store: ArtifactStore,
    pre_attestation: Mapping[str, Any],
    expected: Mapping[str, str],
) -> dict[str, dict[str, Any]]:
    """Verify that post-A report documents were immutably ledger-captured."""

    attested_files = pre_attestation.get("run_file_hashes")
    if not isinstance(attested_files, Mapping):
        raise ContractError("phase-A run-file manifest is missing")
    ledger_entry = attested_files.get("artifact-ledger.jsonl")
    if not isinstance(ledger_entry, Mapping):
        raise ContractError("phase-A artifact-ledger binding is missing")
    old_size = ledger_entry.get("size")
    old_hash = ledger_entry.get("sha256")
    if (
        isinstance(old_size, bool)
        or not isinstance(old_size, int)
        or old_size <= 0
        or not isinstance(old_hash, str)
    ):
        raise ContractError("phase-A artifact-ledger prefix is invalid")
    raw = artifact_store.ledger_path.read_bytes()
    if len(raw) <= old_size or hashlib.sha256(raw[:old_size]).hexdigest() != old_hash:
        raise IntegrityError("post-A artifact ledger does not retain the signed prefix")
    if raw[old_size - 1 : old_size] != b"\n":
        raise IntegrityError("phase-A artifact ledger did not end on a record boundary")

    normalized_expected: dict[str, str] = {}
    for supplied_path, supplied_digest in expected.items():
        digest = _require_sha256(supplied_digest, "post-A run-document hash")
        _, path = _normalize_relative_file_path(supplied_path)
        if path in normalized_expected:
            raise ContractError("duplicate post-A run-document path")
        if path in attested_files:
            raise IntegrityError("post-A statistical document was already present in phase A")
        file_path = artifact_store.run_root / Path(*path.split("/"))
        if not file_path.is_file() or sha256_file(file_path) != digest:
            raise IntegrityError(f"post-A run-document file/hash mismatch: {path}")
        normalized_expected[path] = digest

    found: dict[str, dict[str, Any]] = {}
    offset = old_size
    for index, line in enumerate(raw[old_size:].splitlines(keepends=True)):
        if not line.endswith(b"\n"):
            raise IntegrityError("post-A artifact ledger has a partial final record")
        previous_hash = hashlib.sha256(raw[:offset]).hexdigest()
        offset += len(line)
        try:
            record = json.loads(line)
        except json.JSONDecodeError as error:
            raise IntegrityError("post-A artifact ledger contains invalid JSON") from error
        if not isinstance(record, Mapping):
            raise IntegrityError("post-A artifact ledger record must be an object")
        path = record.get("artifact_path")
        if path not in normalized_expected:
            continue
        assert isinstance(path, str)
        if path in found:
            raise IntegrityError(f"post-A run-document was ledger-captured twice: {path}")
        filename = Path(path).name
        if (
            record.get("event") != "run-document"
            or record.get("run_id") != artifact_store.run_id
            or record.get("artifact_hashes")
            != {filename: normalized_expected[path]}
        ):
            raise IntegrityError(f"post-A run-document ledger binding is invalid: {path}")
        found[path] = {
            "index": index,
            "previous_ledger_sha256": previous_hash,
        }
    if set(found) != set(normalized_expected):
        raise IntegrityError(
            "post-A report/equivalence documents are not both append-only captured"
        )
    return found


def _release_endpoint_evidence(
    *,
    rows: Sequence[Mapping[str, Any]],
    plan: Sequence[Arm],
    sample_ids: Sequence[str],
    raters_by_sample: Mapping[str, Any],
    endpoint_policy: Mapping[str, Any],
    preregistration: Mapping[str, Any],
) -> dict[str, Any]:
    baseline_arms = [arm for arm in plan if arm.context == ALL_ACTUAL_CONTEXT]
    if len(baseline_arms) != 1:
        raise IntegrityError("frozen plan does not contain exactly one all-actual endpoint arm")
    baseline_arm = baseline_arms[0]
    selected = [row for row in rows if row.get("arm_id") == baseline_arm.arm_id]
    by_sample: dict[str, Mapping[str, Any]] = {}
    for row in selected:
        sample_id = row.get("sample_id")
        if not isinstance(sample_id, str) or sample_id in by_sample:
            raise IntegrityError("all-actual endpoint rows have invalid sample identities")
        by_sample[sample_id] = row
    if set(by_sample) != set(sample_ids):
        raise IntegrityError("all-actual endpoint does not cover the frozen sample universe")
    completed = [by_sample[sample_id] for sample_id in sample_ids if by_sample[sample_id].get("status") == "completed"]

    minimums = preregistration.get("minimum_effective_counts")
    bootstrap = preregistration.get("bootstrap")
    if not isinstance(minimums, Mapping) or not isinstance(bootstrap, Mapping):
        raise ContractError("release endpoint requires preregistered minimums/bootstrap")
    required_clusters = int(minimums.get("atomic_clusters", 0))
    required_raters = int(minimums.get("held_out_human_raters", 0))
    iterations = int(bootstrap.get("iterations", 0))
    confidence = float(bootstrap.get("confidence", 0.95))
    threshold = float(preregistration.get("ceiling_min_success_rate", 1.0))
    analysis_seed = _derived_release_seed(
        int(preregistration.get("seed", 0)),
        "release-endpoint-E",
        baseline_arm.arm_id,
    )
    lower_bound = (
        _cluster_success_lower_bound(
            completed,
            iterations=iterations,
            confidence=confidence,
            seed=analysis_seed,
        )
        if len(completed) == len(sample_ids) and completed
        else None
    )
    success_rate = (
        sum(int(row["E"]) for row in completed) / len(completed)
        if len(completed) == len(sample_ids) and completed
        else None
    )
    cluster_count = len({str(row["atomic_cluster_id"]) for row in completed})
    rater_counts = [
        int(row.get("diagnostics", {}).get("held_out_human_rater_count", 0))
        for row in completed
        if isinstance(row.get("diagnostics"), Mapping)
    ]
    minimum_observed_raters = (
        min(rater_counts)
        if len(rater_counts) == len(completed) and rater_counts
        else 0
    )
    evaluator_hashes: dict[str, str] = {}
    for sample_id in sample_ids:
        row = by_sample[sample_id]
        digest = row.get("evaluator_artifact_hash")
        if row.get("status") == "completed":
            evaluator_hashes[sample_id] = _require_sha256(
                digest, f"release endpoint evaluator hash for {sample_id!r}"
            )
    passed = all(
        (
            len(completed) == len(sample_ids),
            lower_bound is not None,
            lower_bound is not None and lower_bound >= threshold,
            cluster_count >= required_clusters,
            minimum_observed_raters >= required_raters,
            set(evaluator_hashes) == set(sample_ids),
        )
    )
    return {
        "status": "passed" if passed else "not-passed",
        "arm_id": baseline_arm.arm_id,
        "sample_count": len(completed),
        "expected_sample_count": len(sample_ids),
        "atomic_clusters": cluster_count,
        "required_atomic_clusters": required_clusters,
        "success_rate": success_rate,
        "required_success_rate": threshold,
        "one_sided_cluster_ci_lower": lower_bound,
        "confidence": confidence,
        "bootstrap_iterations": iterations,
        "analysis_seed": analysis_seed,
        "minimum_held_out_human_raters": minimum_observed_raters,
        "required_held_out_human_raters": required_raters,
        "held_out_rater_ids_by_sample": {
            sample_id: raters_by_sample[sample_id] for sample_id in sample_ids
        },
        "evaluator_artifact_sha256_by_sample": evaluator_hashes,
        "allowed_set_sha256": endpoint_policy.get("allowed_set_sha256"),
    }


def _release_effect_evidence(
    *,
    hypothesis: Mapping[str, Any],
    selected_contrasts: Mapping[str, Mapping[str, Any]],
    chains: Sequence[str],
    preregistration: Mapping[str, Any],
    attribution_minimum_deltas: Mapping[str, float],
) -> dict[str, Any]:
    multiplicity = preregistration.get("multiplicity")
    minimums = preregistration.get("minimum_effective_counts")
    if not isinstance(multiplicity, Mapping) or not isinstance(minimums, Mapping):
        raise ContractError("release effect requires preregistered multiplicity/minimums")
    alpha = float(multiplicity.get("family_wise_alpha", 0.05))
    required_clusters = int(minimums.get("atomic_clusters", 0))
    direction = str(hypothesis["direction"])
    stage = str(hypothesis["stage"])
    chain_evidence: dict[str, Any] = {}
    overall_passed = True
    common_coordinate: dict[str, Any] | None = None
    for chain in sorted(chains):
        summary = selected_contrasts[chain]
        contrast_id = summary.get("contrast_id")
        if not isinstance(contrast_id, str) or contrast_id not in attribution_minimum_deltas:
            raise ContractError("selected contrast lacks attribution_minimum_delta")
        minimum_delta = float(attribution_minimum_deltas[contrast_id])
        estimate = summary.get("estimate")
        p_holm = summary.get("p_holm")
        n_clusters = summary.get("n_clusters")
        n_samples = summary.get("n_samples")
        if any(
            isinstance(value, bool) or not isinstance(value, (int, float))
            for value in (estimate, p_holm, n_clusters, n_samples)
        ):
            raise IntegrityError("selected contrast has malformed deterministic statistics")
        bound_key = "ci_lower" if direction == "positive" else "ci_upper"
        bound = summary.get(bound_key)
        bound_valid = (
            not isinstance(bound, bool)
            and isinstance(bound, (int, float))
            and math.isfinite(float(bound))
        )
        direction_passed = (
            float(estimate) > 0.0 if direction == "positive" else float(estimate) < 0.0
        )
        bound_passed = bool(bound_valid) and (
            float(bound) > 0.0 if direction == "positive" else float(bound) < 0.0
        )
        passed = all(
            (
                int(n_clusters) >= required_clusters,
                0.0 <= float(p_holm) <= alpha,
                float(estimate) >= minimum_delta,
                direction_passed,
                bound_passed,
            )
        )
        overall_passed = overall_passed and passed
        chain_evidence[chain] = {
            "status": "passed" if passed else "not-passed",
            "direction": direction,
            "contrast_id": contrast_id,
            "contrast": summary.get("contrast"),
            "context": summary.get("context"),
            "focus_stage": summary.get("focus_stage"),
            "held_stage": summary.get("held_stage"),
            "held_source": summary.get("held_source"),
            "estimate": estimate,
            "preregistered_minimum_delta": minimum_delta,
            "one_sided_ci_bound": bound,
            "confidence": summary.get("confidence"),
            "holm_adjusted_p": p_holm,
            "holm_family": summary.get("holm_family"),
            "atomic_clusters": n_clusters,
            "required_atomic_clusters": required_clusters,
            "sample_count": n_samples,
        }
        coordinate = {
            "contrast": summary.get("contrast"),
            "context": summary.get("context"),
            "focus_stage": summary.get("focus_stage"),
            "held_stage": summary.get("held_stage"),
            "held_source": summary.get("held_source"),
        }
        if common_coordinate is None:
            common_coordinate = coordinate
        elif coordinate != common_coordinate:
            raise IntegrityError("release effect chains have different coordinates")
    assert common_coordinate is not None
    interface_conditioned = common_coordinate["held_stage"] is not None
    attribution_status = (
        "confirmed-interface-conditioned-bottleneck"
        if overall_passed and interface_conditioned
        else "confirmed-stage-bottleneck"
        if overall_passed
        else "not-passed"
    )
    return {
        "status": "passed" if overall_passed else "not-passed",
        "direction": direction,
        "selected_stage": stage,
        "attribution_status": attribution_status,
        "production_bottleneck": (
            stage if overall_passed and not interface_conditioned else None
        ),
        "production_interface_bottleneck": (
            common_coordinate if overall_passed and interface_conditioned else None
        ),
        "intervention_coordinate": common_coordinate,
        "minimum_delta_registry_source": "attribution_minimum_delta",
        "family_wise_alpha": alpha,
        "chains": chain_evidence,
    }


def _validate_equivalence_certificate(
    document: Mapping[str, Any],
    *,
    epsilon_registry: Mapping[str, Any],
    minimum_clusters: int,
    chains: list[str],
    expected_binding: Mapping[str, Any],
    expected_row_count: int,
) -> bool:
    if document.get("schema_version") != "chesstory.eval.fresh-confirm-equivalence.v1":
        raise ContractError("unsupported fresh-confirm equivalence schema")
    if document.get("split") != "fresh-confirm" or document.get("fresh_confirm") is not True:
        raise ContractError("equivalence evidence is not from fresh-confirm")
    if document.get("expected_contrast_count") != 156:
        raise ContractError("equivalence evidence does not cover all 156 contrasts")
    if document.get("minimum_atomic_clusters") != minimum_clusters:
        raise ContractError("equivalence minimum-cluster gate differs from preregistration")
    if document.get("oracle_chains") != sorted(chains):
        raise ContractError("equivalence oracle chains differ from the frozen chains")
    if document.get("missing_epsilon_contrast_ids") != [] or document.get(
        "unexpected_epsilon_contrast_ids"
    ) != []:
        raise ContractError("equivalence epsilon registry is incomplete or has extras")
    binding = document.get("binding")
    if not isinstance(binding, Mapping):
        raise ContractError("equivalence authoritative binding is missing")
    for key, expected in expected_binding.items():
        if binding.get(key) != expected:
            raise IntegrityError(f"equivalence {key} binding mismatch")
    gate = document.get("gate")
    if not isinstance(gate, Mapping) or not all(
        (
            gate.get("expected_row_count") == expected_row_count,
            gate.get("observed_row_count") == expected_row_count,
            gate.get("exact_sample_arm_universe") is True,
            gate.get("all_rows_completed") is True,
            int(gate.get("minimum_held_out_human_raters", 0))
            >= int(gate.get("required_held_out_human_raters", 1)),
            gate.get("evaluator_artifacts_bound") is True,
        )
    ):
        raise ContractError("equivalence authoritative execution gate did not pass")
    multiplicity = document.get("multiplicity")
    if not isinstance(multiplicity, Mapping) or not all(
        (
            multiplicity.get("holm_family") == "within-each-fixed-oracle-chain",
            multiplicity.get("upper_bound")
            == "bonferroni-simultaneous-one-sided-percentile",
            multiplicity.get("exact_holm_inverted_upper_bounds") is False,
            multiplicity.get("oracle_chains_pooled") is False,
        )
    ):
        raise ContractError("equivalence multiplicity/bound contract is invalid")
    families = document.get("families")
    if not isinstance(families, list) or {
        family.get("oracle_chain") for family in families if isinstance(family, Mapping)
    } != set(chains):
        raise ContractError("equivalence fixed-chain families are incomplete")
    seen: set[str] = set()
    passed = document.get("status") == "equivalence-established"
    alpha = document.get("alpha")
    if not isinstance(alpha, (int, float)) or isinstance(alpha, bool) or not 0 < float(alpha) < 1:
        raise ContractError("equivalence alpha is invalid")
    for family in families:
        assert isinstance(family, Mapping)
        passed = passed and family.get("status") == "equivalence-established"
        results = family.get("results")
        if not isinstance(results, list):
            raise ContractError("equivalence family results are missing")
        for result in results:
            if not isinstance(result, Mapping):
                raise ContractError("equivalence contrast result is invalid")
            contrast_id = result.get("contrast_id")
            if not isinstance(contrast_id, str) or contrast_id in seen:
                raise IntegrityError("equivalence contrast IDs are invalid or duplicated")
            seen.add(contrast_id)
            if contrast_id not in epsilon_registry:
                raise IntegrityError("equivalence contains an unregistered contrast")
            epsilon = float(epsilon_registry[contrast_id])
            simultaneous = result.get("simultaneous_upper_bound")
            p_holm = result.get("p_holm_boundary")
            passed = passed and all(
                (
                    result.get("equivalent") is True,
                    result.get("holm_boundary_rejected") is True,
                    result.get("simultaneous_bound_below_epsilon") is True,
                    result.get("holm_adjusted_upper_bound") is None,
                    result.get("simultaneous_bound_adjustment") == "bonferroni",
                    isinstance(simultaneous, (int, float)),
                    isinstance(simultaneous, (int, float)) and float(simultaneous) <= epsilon,
                    isinstance(p_holm, (int, float)),
                    isinstance(p_holm, (int, float)) and float(p_holm) <= float(alpha),
                    int(result.get("n_clusters", 0)) >= minimum_clusters,
                )
            )
    if seen != set(epsilon_registry):
        raise IntegrityError("equivalence contrast set differs from exact epsilon registry")
    return bool(passed)


def _validate_qualification_document(document: Mapping[str, Any]) -> None:
    _validate_unsigned_document(document, RELEASE_QUALIFICATION_SCHEMA_VERSION)
    if set(document) != {
        "schema_version",
        "qualified_at",
        "status",
        "binding",
        "pre_unblinding_attestation_sha256",
        "criteria",
        "evidence",
    }:
        raise ContractError("fresh-confirm qualification fields are not exact")
    _nonempty_text(document.get("qualified_at"), "qualified_at")
    if document.get("status") != "PASS":
        raise ContractError("fresh-confirm qualification status must be PASS")
    criteria = document.get("criteria")
    if not isinstance(criteria, Mapping) or set(criteria) != set(_RELEASE_CRITERIA):
        raise ContractError("fresh-confirm qualification criteria are incomplete")
    if any(criteria[key] is not True for key in _RELEASE_CRITERIA):
        raise ContractError("fresh-confirm qualification has a non-passing criterion")
    _require_sha256(
        document.get("pre_unblinding_attestation_sha256"),
        "pre_unblinding_attestation_sha256",
    )
    binding = document.get("binding")
    if not isinstance(binding, Mapping):
        raise ContractError("fresh-confirm qualification binding is missing")
    expected_binding_keys = {
        "run_id",
        "candidate_manifest_sha256",
        "corpus_version",
        "split",
        "split_input_sha256",
        "endpoint_policy_sha256",
        "plan_sha256",
        "registered_arm_count",
        "epsilon_registry_sha256",
        "epsilon_contrast_count",
        "sample_universe_sha256",
        "expected_row_count",
        "selected_attribution_hypothesis_sha256",
        "selected_contrast_ids_sha256",
        "attribution_minimum_delta_sha256",
        "live_command_config_sha256",
    }
    if set(binding) != expected_binding_keys:
        raise ContractError("fresh-confirm qualification binding fields are not exact")
    for key in (
        "candidate_manifest_sha256",
        "split_input_sha256",
        "endpoint_policy_sha256",
        "plan_sha256",
        "epsilon_registry_sha256",
        "sample_universe_sha256",
        "selected_attribution_hypothesis_sha256",
        "selected_contrast_ids_sha256",
        "attribution_minimum_delta_sha256",
        "live_command_config_sha256",
    ):
        _require_sha256(binding.get(key), f"qualification binding {key}")
    if binding.get("split") != "fresh-confirm" or binding.get("registered_arm_count") != 313:
        raise ContractError("qualification does not bind the full fresh-confirm plan")
    if binding.get("epsilon_contrast_count") != 156:
        raise ContractError("qualification epsilon registry is not exact")
    expected_rows = binding.get("expected_row_count")
    if isinstance(expected_rows, bool) or not isinstance(expected_rows, int) or expected_rows <= 0:
        raise ContractError("qualification expected row count is invalid")
    evidence = document.get("evidence")
    expected_evidence = {
        "experiment_report",
        "fresh_confirm_equivalence",
        "release_evidence",
        "endpoint_policy",
    }
    if not isinstance(evidence, Mapping) or set(evidence) != expected_evidence:
        raise ContractError("qualification evidence set is incomplete")
    for name, entry in evidence.items():
        if not isinstance(entry, Mapping) or not isinstance(entry.get("document"), Mapping):
            raise ContractError(f"qualification evidence {name!r} is invalid")
        _require_sha256(entry.get("document_sha256"), f"{name} document_sha256")
        if entry.get("document_sha256") != sha256_json(entry["document"]):
            raise IntegrityError(f"qualification evidence {name!r} hash mismatch")
        if name != "endpoint_policy":
            _require_sha256(entry.get("file_sha256"), f"{name} file_sha256")


def _walk_regular_files(root: Path) -> list[str]:
    _reject_link_like(root, label="tree root")
    try:
        resolved_root = root.resolve(strict=True)
    except OSError as error:
        raise IntegrityError(f"tree root is unavailable: {root}") from error
    if not resolved_root.is_dir():
        raise IntegrityError(f"tree root is not a directory: {root}")

    paths: list[str] = []

    def visit(directory: Path, prefix: tuple[str, ...]) -> None:
        try:
            entries = sorted(os.scandir(directory), key=lambda entry: entry.name)
        except OSError as error:
            raise IntegrityError(f"cannot enumerate attested tree: {directory}") from error
        for entry in entries:
            path = Path(entry.path)
            relative = (*prefix, entry.name)
            _reject_link_like(path, label="tree entry")
            if entry.is_dir(follow_symlinks=False):
                visit(path, relative)
            elif entry.is_file(follow_symlinks=False):
                paths.append(Path(*relative).as_posix())
            else:
                raise IntegrityError(
                    f"non-regular entry in attested tree: {Path(*relative).as_posix()}"
                )

    visit(resolved_root, ())
    return paths


def _read_run_json(store: ArtifactStore, relative_path: str) -> Mapping[str, Any]:
    _, normalized = _normalize_relative_file_path(relative_path)
    path = store.run_root / normalized
    value = read_json(path)
    if not isinstance(value, Mapping):
        raise ContractError(f"attested run JSON must be an object: {normalized}")
    return value


def _validate_execution_row_artifacts(
    *,
    artifact_store: ArtifactStore,
    row: Mapping[str, Any],
    run_file_hashes: Mapping[str, Any],
    exact_run_binding: Mapping[str, Any],
    sample_topology: Mapping[str, Any],
    registered_arm_ids: set[str],
) -> Mapping[str, Any] | None:
    """Rebind one execution row to the bytes that produced its endpoint.

    A row and an endpoint capture are two summaries of the same adjudication.
    Hashing both is insufficient if their semantic fields can disagree, so the
    boundary compares the two documents field-for-field and derives the blind
    evaluator-view hash again from the attested canonical V output.
    """

    sample_id = row.get("sample_id")
    cluster_id = row.get("atomic_cluster_id")
    arm_id = row.get("arm_id")
    if (
        not isinstance(sample_id, str)
        or not isinstance(cluster_id, str)
        or not isinstance(arm_id, str)
        or sample_topology.get(sample_id) != cluster_id
        or arm_id not in registered_arm_ids
    ):
        raise IntegrityError("execution row sample/cluster/arm binding is invalid")

    stage_artifacts = row.get("stage_artifacts")
    if not isinstance(stage_artifacts, Mapping):
        raise ContractError("execution row stage_artifacts must be an object")
    completed = row.get("status") == "completed"
    stage_names = set(stage_artifacts)
    if completed and stage_names != set(STAGES):
        raise IntegrityError("completed execution row must bind every registered stage")
    if not stage_names.issubset(set(STAGES)):
        raise ContractError("execution row contains an unregistered stage artifact")

    for stage in STAGES:
        if stage not in stage_artifacts:
            continue
        stage_entry = stage_artifacts.get(stage)
        if not isinstance(stage_entry, Mapping):
            raise ContractError("execution row stage artifact is invalid")
        if set(stage_entry) != {
            "input_hash",
            "canonical_output_hash",
            "consumed_output_hash",
            "capture_hashes",
        }:
            raise ContractError("execution row stage artifact fields are not exact")
        for key in ("input_hash", "canonical_output_hash", "consumed_output_hash"):
            _require_sha256(stage_entry.get(key), f"execution row {stage} {key}")
        capture_hashes = stage_entry.get("capture_hashes")
        if not isinstance(capture_hashes, Mapping):
            raise ContractError("execution row capture hashes are missing")
        required_capture_files = {
            "raw-input.json",
            "raw-output.json",
            "validated-output.json",
            "canonical-output.json",
            "metadata.json",
        }
        if not required_capture_files.issubset(set(capture_hashes)):
            raise IntegrityError("execution row stage capture is incomplete")
        stage_root = (
            f"samples/{ArtifactStore._segment(sample_id)}/"
            f"{ArtifactStore._segment(arm_id)}/{ArtifactStore._segment(stage.lower())}"
        )
        for filename, digest in capture_hashes.items():
            if (
                not isinstance(filename, str)
                or Path(filename).name != filename
                or "/" in filename
                or "\\" in filename
            ):
                raise ContractError("execution row capture filename is not a leaf name")
            digest = _require_sha256(digest, f"execution row {stage}/{filename}")
            relative = f"{stage_root}/{filename}"
            entry = run_file_hashes.get(relative)
            if not isinstance(entry, Mapping) or entry.get("sha256") != digest:
                raise IntegrityError(
                    "execution row stage artifact differs from attested run tree"
                )
        if stage_entry.get("canonical_output_hash") != capture_hashes.get(
            "canonical-output.json"
        ):
            raise IntegrityError("execution row canonical stage hash mismatch")

    if not completed:
        if row.get("endpoint_artifact") is not None:
            raise IntegrityError("incomplete execution row must not claim an endpoint")
        failure_hash = row.get("failure_artifact_hash")
        if failure_hash is not None:
            failure_hash = _require_sha256(
                failure_hash, "execution row failure artifact hash"
            )
            if not any(
                isinstance(entry, Mapping) and entry.get("sha256") == failure_hash
                for entry in run_file_hashes.values()
            ):
                raise IntegrityError("execution row failure artifact is absent from run tree")
        return None

    endpoint_ref = row.get("endpoint_artifact")
    if not isinstance(endpoint_ref, Mapping) or set(endpoint_ref) != {"path", "sha256"}:
        raise ContractError("completed execution row has no exact endpoint artifact binding")
    endpoint_path = endpoint_ref.get("path")
    endpoint_hash = _require_sha256(
        endpoint_ref.get("sha256"), "execution row endpoint artifact hash"
    )
    if not isinstance(endpoint_path, str):
        raise ContractError("execution row endpoint artifact path must be text")
    _, normalized_endpoint_path = _normalize_relative_file_path(endpoint_path)
    endpoint_entry = run_file_hashes.get(normalized_endpoint_path)
    if (
        not isinstance(endpoint_entry, Mapping)
        or endpoint_entry.get("sha256") != endpoint_hash
    ):
        raise IntegrityError("execution row endpoint artifact binding mismatch")
    endpoint_document = _read_run_json(artifact_store, normalized_endpoint_path)
    if set(endpoint_document) != {
        "schema_version",
        "binding",
        "evaluation_view_sha256",
        "E",
        "endpoint_conditions",
        "diagnostics",
        "evaluator_artifact_sha256",
    } or endpoint_document.get("schema_version") != "chesstory.eval.endpoint-capture.v1":
        raise ContractError("completed endpoint capture fields/schema are not exact")
    expected_endpoint_binding = {
        **dict(exact_run_binding),
        "sample_id": sample_id,
        "atomic_cluster_id": cluster_id,
        "arm_id": arm_id,
    }
    if endpoint_document.get("binding") != expected_endpoint_binding:
        raise IntegrityError("endpoint capture has a different exact row/run binding")
    if row.get("E") not in {0, 1} or endpoint_document.get("E") != row.get("E"):
        raise IntegrityError("execution row E differs from its endpoint capture")
    for row_key, endpoint_key in (
        ("endpoint_conditions", "endpoint_conditions"),
        ("diagnostics", "diagnostics"),
        ("evaluator_artifact_hash", "evaluator_artifact_sha256"),
    ):
        if endpoint_document.get(endpoint_key) != row.get(row_key):
            raise IntegrityError(
                f"execution row {row_key} differs from its endpoint capture"
            )
    if not isinstance(row.get("endpoint_conditions"), Mapping) or not isinstance(
        row.get("diagnostics"), Mapping
    ):
        raise ContractError("completed execution row endpoint fields must be objects")
    _require_sha256(
        row.get("evaluator_artifact_hash"), "execution row evaluator artifact hash"
    )

    v_entry = stage_artifacts["V"]
    assert isinstance(v_entry, Mapping)
    v_hashes = v_entry["capture_hashes"]
    assert isinstance(v_hashes, Mapping)
    v_relative = (
        f"samples/{ArtifactStore._segment(sample_id)}/"
        f"{ArtifactStore._segment(arm_id)}/{ArtifactStore._segment('v')}/"
        "canonical-output.json"
    )
    v_file_entry = run_file_hashes.get(v_relative)
    if (
        not isinstance(v_file_entry, Mapping)
        or v_file_entry.get("sha256") != v_hashes.get("canonical-output.json")
    ):
        raise IntegrityError("completed execution row V artifact binding mismatch")
    canonical_v = _read_run_json(artifact_store, v_relative)
    expected_evaluation_view_hash = sha256_json(
        source_blind_evaluation_view(canonical_v)
    )
    claimed_evaluation_view_hash = _require_sha256(
        endpoint_document.get("evaluation_view_sha256"),
        "endpoint evaluation-view hash",
    )
    if claimed_evaluation_view_hash != expected_evaluation_view_hash:
        raise IntegrityError(
            "endpoint evaluation-view hash is not derived from the attested canonical V"
        )
    return endpoint_document


def _captured_live_command_bound_files_sha256(document: Mapping[str, Any]) -> str:
    if set(document) != {"schema_version", "stage_routes", "evaluator_route"} or document.get(
        "schema_version"
    ) != "chesstory.eval.live-command-config.v1":
        raise ContractError("captured live-command config document is not exact")
    routes = document.get("stage_routes")
    evaluator = document.get("evaluator_route")
    if not isinstance(routes, list) or not routes or not isinstance(evaluator, Mapping):
        raise ContractError("captured live-command routes are missing")

    usages: list[dict[str, Any]] = []
    seen_routes: set[tuple[str, str, str | None]] = set()

    def validate_command(
        command: Mapping[str, Any], *, command_keys: set[str], route_label: str
    ) -> None:
        if set(command) != command_keys:
            raise ContractError(f"captured command fields are not exact: {route_label}")
        argv = command.get("argv")
        timeout = command.get("timeout_seconds")
        environment = command.get("env")
        bound_files = command.get("bound_files")
        if (
            not isinstance(argv, list)
            or not argv
            or any(not isinstance(item, str) or not item or "\x00" in item for item in argv)
        ):
            raise ContractError(f"captured command argv is invalid: {route_label}")
        if (
            isinstance(timeout, bool)
            or not isinstance(timeout, int)
            or timeout <= 0
            or timeout > 3600
        ):
            raise ContractError(f"captured command timeout is invalid: {route_label}")
        if not isinstance(environment, Mapping) or any(
            not isinstance(key, str) or not isinstance(value, str)
            for key, value in environment.items()
        ):
            raise ContractError(f"captured command environment is invalid: {route_label}")
        if not isinstance(bound_files, list) or not bound_files:
            raise ContractError(f"captured command has no bound files: {route_label}")
        indexes: list[int] = []
        for index, entry in enumerate(bound_files):
            label = f"captured {route_label} bound_files[{index}]"
            if not isinstance(entry, Mapping) or set(entry) != {
                "argv_index",
                "path",
                "sha256",
                "size",
            }:
                raise ContractError(f"{label} fields are not exact")
            argv_index = entry.get("argv_index")
            if (
                isinstance(argv_index, bool)
                or not isinstance(argv_index, int)
                or argv_index < 0
                or argv_index >= len(argv)
            ):
                raise ContractError(f"{label}.argv_index is invalid")
            path_text = entry.get("path")
            if argv[argv_index] != path_text:
                raise IntegrityError(f"{label} is not the exact argv path")
            digest = _require_sha256(entry.get("sha256"), f"{label}.sha256")
            size = entry.get("size")
            if isinstance(size, bool) or not isinstance(size, int) or size < 0:
                raise ContractError(f"{label}.size is invalid")
            resolved = _canonical_regular_non_link_file(path_text, f"{label}.path")
            if sha256_file(resolved) != digest or resolved.stat().st_size != size:
                raise IntegrityError(f"{label} bytes changed before phase-A attestation")
            indexes.append(argv_index)
            usages.append(
                {
                    "route": route_label,
                    "argv_index": argv_index,
                    "path": str(resolved),
                    "sha256": digest,
                    "size": size,
                }
            )
        if indexes != sorted(set(indexes)) or indexes[0] != 0:
            raise ContractError(
                f"captured {route_label} bound files must be ordered, unique, and bind argv[0]"
            )

    for index, route in enumerate(routes):
        if not isinstance(route, Mapping):
            raise ContractError(f"captured stage route {index} is not an object")
        stage = route.get("stage")
        source = route.get("source")
        if stage not in STAGES or source not in {"actual", "oracle"}:
            raise ContractError(f"captured stage route {index} identity is invalid")
        expected = {
            "stage",
            "source",
            "argv",
            "timeout_seconds",
            "env",
            "bound_files",
        }
        chain: str | None = None
        if source == "oracle":
            expected.add("oracle_chain")
            chain = route.get("oracle_chain")
            if not isinstance(chain, str) or not chain.strip():
                raise ContractError(f"captured oracle route {index} has no chain")
        identity = (str(stage), str(source), chain)
        if identity in seen_routes:
            raise ContractError(f"captured live-command route is duplicated: {identity!r}")
        seen_routes.add(identity)
        route_label = f"{stage}:{source}" + (f":{chain}" if chain is not None else "")
        validate_command(route, command_keys=expected, route_label=route_label)

    validate_command(
        evaluator,
        command_keys={"argv", "timeout_seconds", "env", "bound_files"},
        route_label="evaluator",
    )
    usages.sort(
        key=lambda entry: (
            str(entry["route"]),
            int(entry["argv_index"]),
            str(entry["path"]),
        )
    )
    return sha256_json(usages)


def _captured_live_command_argv_by_route(
    document: Mapping[str, Any],
) -> dict[str, list[str]]:
    routes = document.get("stage_routes")
    evaluator = document.get("evaluator_route")
    if not isinstance(routes, list) or not isinstance(evaluator, Mapping):
        raise ContractError("captured live-command route document is incomplete")
    result: dict[str, list[str]] = {}
    for route in routes:
        if not isinstance(route, Mapping):
            raise ContractError("captured live-command stage route is invalid")
        stage = route.get("stage")
        source = route.get("source")
        chain = route.get("oracle_chain") if source == "oracle" else None
        label = f"{stage}:{source}" + (f":{chain}" if chain is not None else "")
        argv = route.get("argv")
        if label in result or not isinstance(argv, list):
            raise ContractError("captured live-command route/argv is invalid")
        result[label] = list(argv)
    evaluator_argv = evaluator.get("argv")
    if not isinstance(evaluator_argv, list):
        raise ContractError("captured evaluator argv is invalid")
    result["evaluator"] = list(evaluator_argv)
    return result


def _validate_successful_stage_provider_io(
    *,
    artifact_store: ArtifactStore,
    relative_path: str,
    expected_argv_by_route: Mapping[str, Sequence[str]],
) -> None:
    parts = Path(relative_path).parts
    if len(parts) != 6 or parts[0] != "samples" or parts[-1] != "raw-provider-io.json":
        raise ContractError(f"stage provider I/O path is malformed: {relative_path}")
    stage = parts[4].upper()
    if stage not in STAGES:
        raise ContractError(f"stage provider I/O names an unknown stage: {relative_path}")
    directory = Path(*parts[:-1]).as_posix()
    metadata = _read_run_json(artifact_store, f"{directory}/metadata.json")
    raw_input = _read_run_json(artifact_store, f"{directory}/raw-input.json")
    raw_output = read_json(artifact_store.run_root / f"{directory}/raw-output.json")
    validated = _read_run_json(artifact_store, f"{directory}/validated-output.json")
    provider = _read_run_json(artifact_store, relative_path)
    if metadata.get("stage") != stage:
        raise IntegrityError("stage provider I/O metadata stage mismatch")
    source = metadata.get("source")
    chain = metadata.get("oracle_chain")
    route_source = "actual" if source == "format-control" else source
    if route_source not in {"actual", "oracle"}:
        raise IntegrityError("stage provider I/O metadata source is invalid")
    route_label = f"{stage}:{route_source}"
    if route_source == "oracle":
        if not isinstance(chain, str) or not chain:
            raise IntegrityError("oracle stage provider I/O has no chain binding")
        route_label += f":{chain}"
    expected_argv = expected_argv_by_route.get(route_label)
    if expected_argv is None or provider.get("command") != list(expected_argv):
        raise IntegrityError("stage provider I/O command differs from frozen live route")
    expected_provider_keys = {
        "command",
        "request",
        "stdin_base64",
        "returncode",
        "stdout",
        "stderr",
        "stdout_base64",
        "stderr_base64",
        "response",
    }
    if set(provider) != expected_provider_keys or provider.get("returncode") != 0:
        raise ContractError("successful stage provider I/O fields/status are not exact")
    request = provider.get("request")
    response = provider.get("response")
    if not isinstance(request, Mapping) or set(request) != {
        "schema_version",
        "stage",
        "sample_id",
        "seed",
        "input_hash",
        "input_payload",
    }:
        raise ContractError("stage provider request fields are not exact")
    if request != {
        "schema_version": "chesstory.eval.stage-invocation.v1",
        "stage": stage,
        "sample_id": metadata.get("sample_id"),
        "seed": raw_input.get("seed"),
        "input_hash": raw_input.get("input_hash"),
        "input_payload": raw_input.get("payload"),
    }:
        raise IntegrityError("stage provider request differs from captured raw input")
    stdin = provider.get("stdin_base64")
    try:
        decoded_request = base64.b64decode(str(stdin), validate=True).decode("utf-8")
        reparsed_request = json.loads(decoded_request)
    except (ValueError, UnicodeError, json.JSONDecodeError) as error:
        raise IntegrityError("stage provider stdin capture is invalid") from error
    if reparsed_request != request:
        raise IntegrityError("stage provider stdin bytes differ from its request")
    if not isinstance(response, Mapping) or not isinstance(
        response.get("output_payload"), Mapping
    ) or not isinstance(response.get("producer"), Mapping):
        raise ContractError("stage provider response lacks output_payload/producer")
    if validated.get("payload") != response.get("output_payload") or validated.get(
        "producer"
    ) != response.get("producer"):
        raise IntegrityError("stage provider response differs from validated output")
    if raw_output != response.get("raw_output", response.get("output_payload")):
        raise IntegrityError("stage provider response differs from captured raw output")


def _validate_live_command_config_binding(
    *,
    artifact_store: ArtifactStore,
    binding: Any,
    run_file_hashes: Mapping[str, Any],
) -> dict[str, Any]:
    expected_keys = {
        "schema_version",
        "source_file_sha256",
        "document_sha256",
        "artifact_path",
        "artifact_sha256",
        "bound_files_sha256",
        "security_mode",
        "candidate_component_sha256",
        "candidate_external_executable_sha256",
        "frozen_before_split_open",
    }
    if not isinstance(binding, Mapping) or set(binding) != expected_keys:
        raise ContractError("live-command config execution binding is not exact")
    if binding.get("schema_version") != "chesstory.eval.live-command-config.v1":
        raise ContractError("unsupported live-command config execution binding")
    for key in (
        "source_file_sha256",
        "document_sha256",
        "artifact_sha256",
        "bound_files_sha256",
        "candidate_component_sha256",
        "candidate_external_executable_sha256",
    ):
        _require_sha256(binding.get(key), f"live-command config {key}")
    if binding.get("frozen_before_split_open") is not True:
        raise ContractError("live-command config was not frozen before split opening")
    if binding.get("security_mode") != "windows-deny-write-delete-open-handle-v1":
        raise ContractError("live-command config lacks the release executable lock mode")
    artifact_path = binding.get("artifact_path")
    if not isinstance(artifact_path, str):
        raise ContractError("live-command config artifact path is missing")
    _, normalized_path = _normalize_relative_file_path(artifact_path)
    entry = run_file_hashes.get(normalized_path)
    if not isinstance(entry, Mapping) or entry.get("sha256") != binding.get(
        "artifact_sha256"
    ):
        raise IntegrityError("live-command config artifact is not in the signed run tree")
    document = _read_run_json(artifact_store, normalized_path)
    if (
        sha256_json(document) != binding.get("document_sha256")
        or binding.get("artifact_sha256") != binding.get("document_sha256")
    ):
        raise IntegrityError("live-command config document/artifact hash mismatch")
    if _captured_live_command_bound_files_sha256(document) != binding.get(
        "bound_files_sha256"
    ):
        raise IntegrityError("live-command bound-file aggregate is not config-derived")
    return dict(binding)


def _validate_evaluator_artifact_document(
    document: Mapping[str, Any],
    *,
    raters_by_sample: Mapping[str, Any],
) -> tuple[str, str, tuple[str, ...]]:
    required = {
        "schema_version",
        "sample_id",
        "canonical_output_hash",
        "source_blind_evaluator_ids",
        "unsupported_meaning_count",
        "grounded",
        "fact_correct",
        "canonical_core_claim_ids",
        "usefulness_consensus",
        "usefulness_votes",
    }
    allowed = required | {"diagnostics"}
    if set(document) != required and set(document) != allowed:
        raise ContractError("captured evaluator response fields are not exact")
    if document.get("schema_version") != "chesstory.eval.blind-evaluation.v1":
        raise ContractError("captured evaluator response schema is unsupported")
    sample_id = document.get("sample_id")
    if not isinstance(sample_id, str) or sample_id not in raters_by_sample:
        raise IntegrityError("captured evaluator response sample is not frozen")
    output_hash = _require_sha256(
        document.get("canonical_output_hash"),
        "captured evaluator canonical output hash",
    )
    expected_raters = raters_by_sample.get(sample_id)
    evaluator_ids = document.get("source_blind_evaluator_ids")
    if (
        not isinstance(expected_raters, list)
        or evaluator_ids != expected_raters
        or not isinstance(evaluator_ids, list)
        or evaluator_ids != sorted(set(evaluator_ids))
    ):
        raise IntegrityError("captured evaluator response roster is not the exact frozen roster")
    unsupported = document.get("unsupported_meaning_count")
    if isinstance(unsupported, bool) or not isinstance(unsupported, int) or unsupported < 0:
        raise ContractError("captured evaluator unsupported count is invalid")
    if not isinstance(document.get("grounded"), bool) or not isinstance(
        document.get("fact_correct"), bool
    ):
        raise ContractError("captured evaluator grounded/fact fields must be booleans")
    claims = document.get("canonical_core_claim_ids")
    if not isinstance(claims, list) or any(
        not isinstance(claim_id, str) or not claim_id.strip() for claim_id in claims
    ):
        raise ContractError("captured evaluator core claims are invalid")
    consensus = document.get("usefulness_consensus")
    if (
        isinstance(consensus, bool)
        or not isinstance(consensus, (int, float))
        or not math.isfinite(float(consensus))
    ):
        raise ContractError("captured evaluator usefulness consensus is invalid")
    diagnostics = document.get("diagnostics", {})
    if not isinstance(diagnostics, Mapping) or any(
        value is not None
        and (
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or not math.isfinite(float(value))
        )
        for value in diagnostics.values()
    ):
        raise ContractError("captured evaluator diagnostics are invalid")
    votes = document.get("usefulness_votes")
    if not isinstance(votes, list) or len(votes) != len(evaluator_ids):
        raise ContractError("captured evaluator votes do not cover the frozen roster")
    parsed_votes: list[tuple[str, float]] = []
    for vote in votes:
        if not isinstance(vote, Mapping) or set(vote) != {"evaluator_id", "score"}:
            raise ContractError("captured evaluator vote fields are not exact")
        evaluator_id = vote.get("evaluator_id")
        score = vote.get("score")
        if (
            not isinstance(evaluator_id, str)
            or isinstance(score, bool)
            or not isinstance(score, (int, float))
            or not math.isfinite(float(score))
            or not 0.0 <= float(score) <= 1.0
        ):
            raise ContractError("captured evaluator vote is invalid")
        parsed_votes.append((evaluator_id, float(score)))
    if tuple(evaluator_id for evaluator_id, _ in parsed_votes) != tuple(evaluator_ids):
        raise IntegrityError("captured evaluator vote order/roster differs")
    recomputed_consensus = sum(score for _, score in parsed_votes) / len(parsed_votes)
    if not math.isclose(
        float(consensus), recomputed_consensus, rel_tol=0.0, abs_tol=1e-12
    ):
        raise IntegrityError(
            "captured evaluator usefulness consensus is not the individual-vote mean"
        )
    return sample_id, output_hash, tuple(evaluator_ids)


def _normalize_relative_file_path(value: str | Path) -> tuple[Path, str]:
    if not isinstance(value, (str, Path)):
        raise ContractError(f"manifest path must be text or Path: {value!r}")
    raw = os.fspath(value)
    if not raw or "\x00" in raw:
        raise ContractError("manifest path must be non-empty text without NUL")
    portable = raw.replace("\\", "/")
    segments = portable.split("/")
    if any(segment in {"", ".", ".."} for segment in segments):
        raise ContractError(f"manifest path is not normalized and relative: {raw!r}")
    relative = Path(*segments)
    if relative.is_absolute() or relative.drive or relative.root:
        raise ContractError(f"absolute manifest path is forbidden: {raw!r}")
    return relative, Path(*segments).as_posix()


def _reject_path_links(root: Path, relative: Path) -> None:
    current = root
    _reject_link_like(current, label="hash-manifest root")
    for part in relative.parts:
        current = current / part
        _reject_link_like(current, label="manifest path component")


def _reject_link_like(path: Path, *, label: str) -> None:
    try:
        is_junction = bool(getattr(path, "is_junction", lambda: False)())
        is_link = path.is_symlink()
    except OSError as error:
        raise IntegrityError(f"cannot inspect {label}: {path}") from error
    if is_link or is_junction:
        raise IntegrityError(f"{label} must not be a symlink or junction: {path}")


def _is_strictly_within(path: Path, root: Path) -> bool:
    try:
        path.relative_to(root)
    except ValueError:
        return False
    return path != root


def _validate_component_binding(
    name: str,
    value: Mapping[str, Any],
    *,
    component_root: Path | None = None,
) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ContractError(f"component {name!r} must be an object")
    version = _nonempty_text(value.get("version"), f"component {name!r} version")
    digest = _require_sha256(value.get("sha256"), f"component {name!r} sha256")
    paths = value.get("paths")
    if (
        not isinstance(paths, list)
        or not paths
        or any(not isinstance(path, str) for path in paths)
        or paths != sorted(set(paths))
    ):
        raise ContractError(
            f"component {name!r} paths must be a sorted unique non-empty string array"
        )
    normalized_paths: list[str] = []
    for path in paths:
        _, normalized = _normalize_relative_file_path(path)
        if path != normalized:
            raise ContractError(f"component {name!r} path is non-canonical: {path!r}")
        normalized_paths.append(normalized)
    copied = json.loads(json_bytes(value))
    copied["version"] = version
    copied["sha256"] = digest
    copied["paths"] = normalized_paths
    if component_root is not None:
        actual_manifest = relative_file_hash_manifest(component_root, normalized_paths)
        actual_digest = _component_manifest_digest(actual_manifest)
        if actual_digest != digest:
            raise IntegrityError(
                f"component {name!r} supplied sha256 does not match its actual paths"
            )
        copied["file_manifest"] = actual_manifest
    else:
        file_manifest = value.get("file_manifest")
        if not isinstance(file_manifest, Mapping) or set(file_manifest) != set(
            normalized_paths
        ):
            raise ContractError(
                f"component {name!r} file_manifest must bind exactly its paths"
            )
        frozen_manifest: dict[str, dict[str, int | str]] = {}
        for path in normalized_paths:
            entry = file_manifest.get(path)
            if not isinstance(entry, Mapping):
                raise ContractError(f"component {name!r} file entry is invalid")
            entry_digest = _require_sha256(
                entry.get("sha256"), f"component {name!r} file {path!r}"
            )
            size = entry.get("size")
            if isinstance(size, bool) or not isinstance(size, int) or size < 0:
                raise ContractError(
                    f"component {name!r} file {path!r} size is invalid"
                )
            frozen_manifest[path] = {"sha256": entry_digest, "size": size}
        if _component_manifest_digest(frozen_manifest) != digest:
            raise IntegrityError(f"component {name!r} aggregate sha256 is inconsistent")
        copied["file_manifest"] = frozen_manifest
    if name == "weights":
        snapshot_fixed = value.get("snapshot_fixed")
        if not isinstance(snapshot_fixed, bool):
            raise ContractError("weights.snapshot_fixed must be explicitly boolean")
        if not snapshot_fixed:
            _nonempty_text(value.get("provider_version"), "weights.provider_version")
            if value.get("raw_provider_io_required") is not True:
                raise ContractError(
                    "unfixed API weights require raw_provider_io_required=true"
                )
    return copied


def _component_manifest_digest(
    manifest: Mapping[str, Mapping[str, int | str]],
) -> str:
    if len(manifest) == 1:
        return str(next(iter(manifest.values()))["sha256"])
    return sha256_json(manifest)


def _resolved_directory(path: Path, label: str) -> Path:
    supplied = Path(path)
    _reject_link_like(supplied, label=label)
    try:
        resolved = supplied.resolve(strict=True)
    except OSError as error:
        raise IntegrityError(f"{label} is unavailable: {supplied}") from error
    if not resolved.is_dir():
        raise IntegrityError(f"{label} is not a directory: {supplied}")
    return resolved


def _canonical_regular_non_link_file(value: Any, label: str) -> Path:
    if not isinstance(value, str) or not value or "\x00" in value:
        raise ContractError(f"{label} must be a canonical absolute file path")
    supplied = Path(value)
    if not supplied.is_absolute():
        raise ContractError(f"{label} must be absolute")
    current = Path(supplied.anchor)
    for part in supplied.parts[1:]:
        current = current / part
        _reject_link_like(current, label=label)
    try:
        resolved = supplied.resolve(strict=True)
    except OSError as error:
        raise IntegrityError(f"{label} is unavailable: {supplied}") from error
    if str(supplied) != str(resolved):
        raise ContractError(f"{label} is not the exact canonical path: {resolved}")
    if not resolved.is_file():
        raise ContractError(f"{label} must be a regular file")
    return resolved


def _validate_external_executable_binding(
    value: Any,
    *,
    component_root: Path | None = None,
    verify_files: bool = False,
) -> dict[str, Any]:
    expected_keys = {
        "schema_version",
        "security_mode",
        "files",
        "files_sha256",
        "all_bound_files_sha256",
    }
    if not isinstance(value, Mapping) or set(value) != expected_keys:
        raise ContractError("candidate external executable binding is not exact")
    if value.get("schema_version") != "chesstory.eval.external-executable-binding.v1":
        raise ContractError("unsupported external executable binding schema")
    if value.get("security_mode") != "windows-deny-write-delete-open-handle-v1":
        raise ContractError("release executable binding lacks the required Windows lock mode")
    files = value.get("files")
    if not isinstance(files, list):
        raise ContractError("external executable files must be an array")
    frozen_files: list[dict[str, Any]] = []
    path_order: list[str] = []
    resolved_root = (
        _resolved_directory(component_root, "component root")
        if component_root is not None
        else None
    )
    for index, entry in enumerate(files):
        label = f"external executable files[{index}]"
        if not isinstance(entry, Mapping) or set(entry) != {
            "absolute_path",
            "path_fingerprint_sha256",
            "sha256",
            "size",
            "usages",
        }:
            raise ContractError(f"{label} fields are not exact")
        absolute_path = entry.get("absolute_path")
        if not isinstance(absolute_path, str) or not Path(absolute_path).is_absolute():
            raise ContractError(f"{label}.absolute_path must be absolute text")
        expected_fingerprint = sha256_json(
            {"normalized_absolute_path": os.path.normcase(absolute_path)}
        )
        if entry.get("path_fingerprint_sha256") != expected_fingerprint:
            raise IntegrityError(f"{label} path fingerprint is not content-derived")
        digest = _require_sha256(entry.get("sha256"), f"{label}.sha256")
        size = entry.get("size")
        if isinstance(size, bool) or not isinstance(size, int) or size < 0:
            raise ContractError(f"{label}.size must be a non-negative integer")
        usages = entry.get("usages")
        if not isinstance(usages, list) or not usages:
            raise ContractError(f"{label}.usages must be a non-empty array")
        normalized_usages: list[dict[str, Any]] = []
        usage_order: list[tuple[str, int]] = []
        for usage_index, usage in enumerate(usages):
            if not isinstance(usage, Mapping) or set(usage) != {"route", "argv_index"}:
                raise ContractError(f"{label}.usages[{usage_index}] is not exact")
            route = _nonempty_text(
                usage.get("route"), f"{label}.usages[{usage_index}].route"
            )
            argv_index = usage.get("argv_index")
            if (
                isinstance(argv_index, bool)
                or not isinstance(argv_index, int)
                or argv_index < 0
            ):
                raise ContractError(
                    f"{label}.usages[{usage_index}].argv_index is invalid"
                )
            usage_order.append((route, argv_index))
            normalized_usages.append({"route": route, "argv_index": argv_index})
        if usage_order != sorted(set(usage_order)):
            raise ContractError(f"{label}.usages must be sorted and unique")
        if verify_files:
            resolved = _canonical_regular_non_link_file(absolute_path, f"{label}.path")
            if resolved_root is not None:
                try:
                    resolved.relative_to(resolved_root)
                except ValueError:
                    pass
                else:
                    raise ContractError(
                        f"{label} is inside component_root and must be component-bound"
                    )
            if sha256_file(resolved) != digest or resolved.stat().st_size != size:
                raise IntegrityError(f"{label} bytes differ from the frozen identity")
        path_order.append(absolute_path)
        frozen_files.append(
            {
                "absolute_path": absolute_path,
                "path_fingerprint_sha256": expected_fingerprint,
                "sha256": digest,
                "size": size,
                "usages": normalized_usages,
            }
        )
    if path_order != sorted(set(path_order)):
        raise ContractError("external executable files must be sorted by unique path")
    if value.get("files_sha256") != sha256_json(frozen_files):
        raise IntegrityError("external executable files_sha256 is inconsistent")
    all_bound_files_sha256 = _require_sha256(
        value.get("all_bound_files_sha256"),
        "external executable all_bound_files_sha256",
    )
    return {
        "schema_version": "chesstory.eval.external-executable-binding.v1",
        "security_mode": "windows-deny-write-delete-open-handle-v1",
        "files": frozen_files,
        "files_sha256": sha256_json(frozen_files),
        "all_bound_files_sha256": all_bound_files_sha256,
    }


def _custodian_root_binding(path: Path) -> str:
    resolved = _resolved_directory(path, "custodian state root")
    return sha256_json(
        {"resolved_custodian_state_root": os.path.normcase(str(resolved))}
    )


def _validate_custodian_state_policy(
    access_policy: Mapping[str, Any], custodian_state_root: Path
) -> dict[str, Any]:
    state = access_policy.get("custodian_state")
    if not isinstance(state, Mapping):
        raise ContractError("release access policy has no custodian_state binding")
    expected_keys = {
        "environment_variable",
        "state_id",
        "claim_namespace",
        "root_binding_sha256",
        "storage_attestation_path",
        "storage_attestation_sha256",
    }
    if set(state) != expected_keys:
        raise ContractError("custodian_state policy fields are incomplete")
    if state.get("environment_variable") != "CHESSTORY_EVAL_CUSTODIAN_STATE_ROOT":
        raise ContractError("custodian state environment-variable name is not frozen")
    state_id = _nonempty_text(state.get("state_id"), "custodian state_id")
    if state.get("claim_namespace") != "sealed-open-claims-v1":
        raise ContractError("unsupported custodian claim namespace")
    claimed_root = _require_sha256(
        state.get("root_binding_sha256"), "custodian root binding"
    )
    actual_root = _custodian_root_binding(custodian_state_root)
    if claimed_root != actual_root:
        raise IntegrityError("custodian state root differs from frozen access policy")
    storage_name = state.get("storage_attestation_path")
    if not isinstance(storage_name, str):
        raise ContractError("custodian storage attestation path is missing")
    storage_sha256 = _require_sha256(
        state.get("storage_attestation_sha256"),
        "custodian storage attestation file hash",
    )
    storage_binding = load_custodian_storage_attestation(
        custodian_state_root,
        state_id,
        relative_path=storage_name,
        expected_sha256=storage_sha256,
    )
    return {
        "state_id": state_id,
        "claim_namespace": "sealed-open-claims-v1",
        "root_binding_sha256": claimed_root,
        "storage_attestation": storage_binding,
    }


def _reject_forbidden_candidate_fields(value: Any, location: str = "components") -> None:
    if isinstance(value, Mapping):
        for key, child in value.items():
            if not isinstance(key, str):
                raise ContractError(f"{location} keys must be text")
            normalized = re.sub(r"[^a-z0-9]+", "_", key.lower()).strip("_")
            tokens = set(normalized.split("_"))
            result_tokens = {
                "effect",
                "effects",
                "metric",
                "metrics",
                "output",
                "outputs",
                "result",
                "results",
                "score",
                "scores",
            }
            is_post_run = "run" in tokens or {"post", "run"}.issubset(tokens)
            is_allowed_set = {"allowed", "set"}.issubset(tokens)
            is_provider_io = (
                {"provider", "io"}.issubset(tokens)
                and normalized != "raw_provider_io_required"
            )
            if (
                normalized in _FORBIDDEN_CANDIDATE_KEYS
                or bool(tokens & result_tokens)
                or is_post_run
                or is_allowed_set
                or is_provider_io
            ):
                raise ContractError(
                    f"post-run/result-derived candidate field is forbidden: {location}.{key}"
                )
            _reject_forbidden_candidate_fields(child, f"{location}.{key}")
    elif isinstance(value, (list, tuple)):
        for index, child in enumerate(value):
            _reject_forbidden_candidate_fields(child, f"{location}[{index}]")


def _require_sha256(value: Any, label: str) -> str:
    if not isinstance(value, str) or not _SHA256_RE.fullmatch(value):
        raise ContractError(f"{label} must be a lowercase 64-character SHA-256")
    return value


def _nonempty_text(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ContractError(f"{label} must be non-empty text")
    return value


def _validate_candidate_document(document: Mapping[str, Any]) -> None:
    _validate_unsigned_document(document, CANDIDATE_SCHEMA_VERSION)
    if set(document) != {
        "schema_version",
        "candidate_id",
        "frozen_at",
        "seed",
        "components",
        "external_executable_binding",
        "required_source_snapshot",
        "corpus_binding",
    }:
        raise ContractError("candidate manifest fields are not exact")
    _reject_forbidden_candidate_fields(document, "candidate_manifest")
    _nonempty_text(document.get("candidate_id"), "candidate_id")
    _nonempty_text(document.get("frozen_at"), "frozen_at")
    seed = document.get("seed")
    if isinstance(seed, bool) or not isinstance(seed, int):
        raise ContractError("seed must be an explicit integer")

    components = document.get("components")
    if not isinstance(components, Mapping):
        raise ContractError("candidate components must be an object")
    missing = sorted(set(REQUIRED_COMPONENTS) - set(components))
    if missing:
        raise ContractError(f"candidate component bindings are missing: {missing}")
    for name, binding in components.items():
        if not isinstance(name, str) or not name.strip():
            raise ContractError("candidate component names must be non-empty text")
        _validate_component_binding(name, binding)

    external_executables = _validate_external_executable_binding(
        document.get("external_executable_binding")
    )
    live_component = components.get("live-command-config")
    if not isinstance(live_component, Mapping) or set(live_component) != {
        "version",
        "sha256",
        "paths",
        "file_manifest",
        "source_file_sha256",
        "document_sha256",
        "bound_files_sha256",
        "security_mode",
    }:
        raise ContractError("candidate live-command-config component is not exact")
    for key in ("source_file_sha256", "document_sha256", "bound_files_sha256"):
        _require_sha256(live_component.get(key), f"live-command-config {key}")
    live_manifest = live_component.get("file_manifest")
    if not isinstance(live_manifest, Mapping) or not any(
        isinstance(entry, Mapping)
        and entry.get("sha256") == live_component.get("source_file_sha256")
        for entry in live_manifest.values()
    ):
        raise IntegrityError("live-command-config source file is absent from its component")
    if live_component.get("security_mode") != external_executables.get(
        "security_mode"
    ) or live_component.get("bound_files_sha256") != external_executables.get(
        "all_bound_files_sha256"
    ):
        raise IntegrityError(
            "candidate live-command and external executable bindings disagree"
        )

    source_snapshot = document.get("required_source_snapshot")
    if not isinstance(source_snapshot, Mapping):
        raise ContractError("candidate required_source_snapshot must be an object")
    if source_snapshot.get("directories") != list(REQUIRED_SOURCE_DIRECTORIES):
        raise ContractError("candidate required source directory roster is not exact")
    if source_snapshot.get("required_files") != list(REQUIRED_SOURCE_FILES):
        raise ContractError("candidate required source file roster is not exact")
    source_manifest = source_snapshot.get("file_manifest")
    if not isinstance(source_manifest, Mapping) or not source_manifest:
        raise ContractError("candidate required source file manifest is empty")
    expected_prefixes = tuple(f"{value}/" for value in REQUIRED_SOURCE_DIRECTORIES)
    required_file_set = set(REQUIRED_SOURCE_FILES)
    for path, entry in source_manifest.items():
        if not isinstance(path, str):
            raise ContractError("candidate required source paths must be text")
        _, normalized = _normalize_relative_file_path(path)
        if path != normalized or not (
            path in required_file_set or path.startswith(expected_prefixes)
        ):
            raise ContractError(f"candidate required source path is out of scope: {path!r}")
        if not isinstance(entry, Mapping):
            raise ContractError(f"candidate required source entry is invalid: {path!r}")
        _require_sha256(entry.get("sha256"), f"required source {path!r}")
        size = entry.get("size")
        if isinstance(size, bool) or not isinstance(size, int) or size < 0:
            raise ContractError(f"candidate required source size is invalid: {path!r}")
    if not required_file_set.issubset(source_manifest):
        raise ContractError("candidate required source manifest omits a required build file")
    for directory in REQUIRED_SOURCE_DIRECTORIES:
        prefix = f"{directory}/"
        if not any(path.startswith(prefix) for path in source_manifest):
            raise ContractError(
                f"candidate required source manifest omits directory {directory!r}"
            )
    if source_snapshot.get("file_manifest_sha256") != sha256_json(source_manifest):
        raise IntegrityError("candidate required source aggregate hash is inconsistent")

    corpus_binding = document.get("corpus_binding")
    if not isinstance(corpus_binding, Mapping) or set(corpus_binding) != {
        "corpus_version",
        "qualification",
        "manifest_sha256",
        "atomic_clusters_sha256",
        "split_sha256",
        "support_files",
        "support_files_sha256",
        "custodian_state",
    }:
        raise ContractError("candidate corpus_binding must be an object")
    if corpus_binding.get("qualification") != "release-qualified":
        raise ContractError("candidate corpus_binding must be release-qualified")
    corpus_version = _nonempty_text(
        corpus_binding.get("corpus_version"), "corpus_binding.corpus_version"
    )
    manifest_hash = _require_sha256(
        corpus_binding.get("manifest_sha256"), "corpus_binding.manifest_sha256"
    )
    _require_sha256(
        corpus_binding.get("atomic_clusters_sha256"),
        "corpus_binding.atomic_clusters_sha256",
    )
    split_hashes = corpus_binding.get("split_sha256")
    if not isinstance(split_hashes, Mapping) or not split_hashes:
        raise ContractError("corpus_binding.split_sha256 must be a non-empty object")
    for split, digest in split_hashes.items():
        _nonempty_text(split, "corpus split id")
        _require_sha256(digest, f"corpus split {split!r}")

    support_files = corpus_binding.get("support_files")
    required_support = {
        "manifest",
        "preregistration",
        "role_schedule",
        "access_policy",
        "reference_index",
        "annotation_index",
        "adjudication_procedure",
    }
    if not isinstance(support_files, Mapping) or set(support_files) != required_support:
        raise ContractError("candidate corpus support-file binding is incomplete")
    for logical_name, entry in support_files.items():
        if not isinstance(entry, Mapping):
            raise ContractError(f"corpus support entry {logical_name!r} is invalid")
        path = entry.get("path")
        if not isinstance(path, str):
            raise ContractError(f"corpus support entry {logical_name!r} has no path")
        _, normalized = _normalize_relative_file_path(path)
        if path != normalized:
            raise ContractError(f"corpus support path is non-canonical: {path!r}")
        _require_sha256(entry.get("sha256"), f"corpus support {logical_name!r}")
        size = entry.get("size")
        if isinstance(size, bool) or not isinstance(size, int) or size < 0:
            raise ContractError(f"corpus support size is invalid: {logical_name!r}")
    if corpus_binding.get("support_files_sha256") != sha256_json(support_files):
        raise IntegrityError("candidate support_files_sha256 is inconsistent")
    for component_name, logical_name in {
        "corpus": "manifest",
        "annotations": "annotation_index",
        "adjudication-procedure": "adjudication_procedure",
        "role-schedule": "role_schedule",
        "access-policy": "access_policy",
        "preregistration": "preregistration",
    }.items():
        support_entry = support_files[logical_name]
        component_entry = components[component_name]
        assert isinstance(support_entry, Mapping)
        assert isinstance(component_entry, Mapping)
        if component_entry.get("paths") != [support_entry.get("path")] or component_entry.get(
            "sha256"
        ) != support_entry.get("sha256"):
            raise IntegrityError(
                f"candidate component {component_name!r} differs from corpus support binding"
            )
    custodian = corpus_binding.get("custodian_state")
    if not isinstance(custodian, Mapping) or set(custodian) != {
        "state_id",
        "claim_namespace",
        "root_binding_sha256",
        "storage_attestation",
    }:
        raise ContractError("candidate custodian_state binding is missing")
    _nonempty_text(custodian.get("state_id"), "custodian state_id")
    if custodian.get("claim_namespace") != "sealed-open-claims-v1":
        raise ContractError("candidate custodian claim namespace is invalid")
    _require_sha256(
        custodian.get("root_binding_sha256"), "candidate custodian root binding"
    )
    storage = custodian.get("storage_attestation")
    if not isinstance(storage, Mapping) or set(storage) != {
        "path",
        "sha256",
        "document_sha256",
        "security_mode",
        "issuer_id",
        "issued_at",
        "acl_evidence_sha256",
    }:
        raise ContractError("candidate custodian storage-attestation binding is not exact")
    storage_path = storage.get("path")
    if (
        not isinstance(storage_path, str)
        or not storage_path
        or Path(storage_path).name != storage_path
        or Path(storage_path).is_absolute()
    ):
        raise ContractError("candidate custodian storage-attestation path is invalid")
    for field in ("sha256", "document_sha256", "acl_evidence_sha256"):
        _require_sha256(storage.get(field), f"candidate storage attestation {field}")
    if storage.get("security_mode") != "immutable-store-exclusive-acl-v1":
        raise ContractError("candidate custodian storage security mode is invalid")
    issuer_id = _nonempty_text(storage.get("issuer_id"), "candidate storage issuer_id")
    if not re.fullmatch(r"custodian:[a-z0-9]+(?:-[a-z0-9]+)*", issuer_id):
        raise ContractError("candidate storage issuer_id is not canonical")
    _nonempty_text(storage.get("issued_at"), "candidate storage issued_at")

    corpus_component = components["corpus"]
    if corpus_component.get("version") != corpus_version:
        raise IntegrityError("candidate corpus component/version binding is inconsistent")
    if corpus_component.get("sha256") != manifest_hash:
        raise IntegrityError("candidate corpus component/hash binding is inconsistent")


def _validate_run_attestation_document(document: Mapping[str, Any]) -> None:
    _validate_unsigned_document(document, RUN_ATTESTATION_SCHEMA_VERSION)
    if set(document) != {
        "schema_version",
        "phase",
        "run_id",
        "attested_at",
        "candidate_manifest_sha256",
        "split",
        "corpus_version",
        "execution_manifest_artifact",
        "execution_bundle_artifact",
        "execution_binding",
        "endpoint_policy_artifact",
        "oracle_graph_artifact",
        "allowed_set_artifact",
        "adjudication_sha256",
        "adjudication_paths",
        "raw_provider_io",
        "artifact_ledger_sha256",
        "access_log_sha256",
        "run_file_hashes",
    }:
        raise ContractError("run attestation fields are not exact")
    if document.get("phase") != "pre-statistical-unblinding":
        raise ContractError("run attestation phase must precede statistical unblinding")
    _nonempty_text(document.get("run_id"), "run_id")
    _nonempty_text(document.get("attested_at"), "attested_at")
    _require_sha256(
        document.get("candidate_manifest_sha256"), "candidate_manifest_sha256"
    )
    _nonempty_text(document.get("corpus_version"), "corpus_version")

    split = document.get("split")
    if not isinstance(split, Mapping):
        raise ContractError("run attestation split must be an object")
    _nonempty_text(split.get("id"), "split.id")
    _require_sha256(split.get("input_sha256"), "split.input_sha256")

    adjudication = document.get("adjudication_sha256")
    if not isinstance(adjudication, Mapping) or not adjudication:
        raise ContractError("adjudication_sha256 must be a non-empty object")
    for name, digest in adjudication.items():
        _nonempty_text(name, "adjudication artifact name")
        _require_sha256(digest, f"adjudication_sha256[{name!r}]")

    run_files = document.get("run_file_hashes")
    if not isinstance(run_files, Mapping) or not run_files:
        raise ContractError("run_file_hashes must be a non-empty object")
    normalized_paths: set[str] = set()
    for supplied_path, entry in run_files.items():
        if not isinstance(supplied_path, str) or not isinstance(entry, Mapping):
            raise ContractError("run-file entries must map path text to hash objects")
        _, normalized = _normalize_relative_file_path(supplied_path)
        if normalized != supplied_path or normalized in normalized_paths:
            raise ContractError(f"run-file path is duplicate or non-canonical: {supplied_path!r}")
        normalized_paths.add(normalized)
        _require_sha256(entry.get("sha256"), f"run file {supplied_path!r}")
        size = entry.get("size")
        if isinstance(size, bool) or not isinstance(size, int) or size < 0:
            raise ContractError(f"run file {supplied_path!r} size must be a non-negative integer")

    artifact_ledger_hash = _require_sha256(
        document.get("artifact_ledger_sha256"), "artifact_ledger_sha256"
    )
    access_log_hash = _require_sha256(
        document.get("access_log_sha256"), "access_log_sha256"
    )
    if run_files.get("artifact-ledger.jsonl", {}).get("sha256") != artifact_ledger_hash:
        raise IntegrityError("artifact ledger hash is inconsistent with run_file_hashes")
    if run_files.get("access-log.jsonl", {}).get("sha256") != access_log_hash:
        raise IntegrityError("access log hash is inconsistent with run_file_hashes")

    for label in (
        "execution_manifest_artifact",
        "execution_bundle_artifact",
        "endpoint_policy_artifact",
        "oracle_graph_artifact",
        "allowed_set_artifact",
    ):
        binding = document.get(label)
        if not isinstance(binding, Mapping):
            raise ContractError(f"{label} must bind an attested run file")
        path = binding.get("path")
        if not isinstance(path, str):
            raise ContractError(f"{label}.path must be text")
        _, normalized_path = _normalize_relative_file_path(path)
        if normalized_path != path or path not in run_files:
            raise IntegrityError(f"{label} path is absent or non-canonical")
        digest = _require_sha256(binding.get("sha256"), f"{label}.sha256")
        if run_files[path].get("sha256") != digest:
            raise IntegrityError(f"{label} hash is inconsistent with run_file_hashes")
    endpoint_policy = document["endpoint_policy_artifact"]
    assert isinstance(endpoint_policy, Mapping)
    policy_document_hash = _require_sha256(
        endpoint_policy.get("document_sha256"),
        "endpoint_policy_artifact.document_sha256",
    )
    if endpoint_policy.get("sha256") != policy_document_hash:
        raise IntegrityError(
            "canonical endpoint-policy file hash differs from document hash"
        )
    execution_bundle_artifact = document["execution_bundle_artifact"]
    assert isinstance(execution_bundle_artifact, Mapping)
    bundle_document_hash = _require_sha256(
        execution_bundle_artifact.get("document_sha256"),
        "execution_bundle_artifact.document_sha256",
    )
    if execution_bundle_artifact.get("sha256") != bundle_document_hash:
        raise IntegrityError(
            "canonical execution-bundle file hash differs from document hash"
        )
    execution_binding = document.get("execution_binding")
    if not isinstance(execution_binding, Mapping):
        raise ContractError("run attestation execution_binding is missing")
    for key in (
        "plan_sha256",
        "registered_arm_ids_sha256",
        "sample_ids_sha256",
        "endpoint_artifact_universe_sha256",
        "bundle_row_universe_sha256",
    ):
        _require_sha256(execution_binding.get(key), f"execution_binding.{key}")
    if execution_binding.get("registered_arm_count") != 313:
        raise ContractError("run attestation does not bind the full 313-arm plan")
    sample_count = execution_binding.get("sample_count")
    if isinstance(sample_count, bool) or not isinstance(sample_count, int) or sample_count <= 0:
        raise ContractError("run attestation sample_count is invalid")
    row_count = execution_binding.get("bundle_row_count")
    if isinstance(row_count, bool) or not isinstance(row_count, int) or row_count <= 0:
        raise ContractError("run attestation bundle_row_count is invalid")
    if execution_binding.get("execution_status") not in {
        "foundation-not-passed",
        "intervention-incomplete",
        "ready-for-analysis",
    }:
        raise ContractError("run attestation execution_status is invalid")
    live_binding = execution_binding.get("live_command_config")
    live_binding_hash = _require_sha256(
        execution_binding.get("live_command_config_sha256"),
        "execution_binding.live_command_config_sha256",
    )
    if not isinstance(live_binding, Mapping) or sha256_json(live_binding) != live_binding_hash:
        raise IntegrityError("run attestation live-command config hash is inconsistent")
    expected_live_keys = {
        "schema_version",
        "source_file_sha256",
        "document_sha256",
        "artifact_path",
        "artifact_sha256",
        "bound_files_sha256",
        "security_mode",
        "candidate_component_sha256",
        "candidate_external_executable_sha256",
        "frozen_before_split_open",
    }
    if set(live_binding) != expected_live_keys or live_binding.get(
        "schema_version"
    ) != "chesstory.eval.live-command-config.v1":
        raise ContractError("run attestation live-command config fields are not exact")
    for key in (
        "source_file_sha256",
        "document_sha256",
        "artifact_sha256",
        "bound_files_sha256",
        "candidate_component_sha256",
        "candidate_external_executable_sha256",
    ):
        _require_sha256(live_binding.get(key), f"live_command_config.{key}")
    live_path = live_binding.get("artifact_path")
    if (
        not isinstance(live_path, str)
        or live_path not in run_files
        or run_files[live_path].get("sha256") != live_binding.get("artifact_sha256")
        or live_binding.get("artifact_sha256") != live_binding.get("document_sha256")
        or live_binding.get("security_mode")
        != "windows-deny-write-delete-open-handle-v1"
        or live_binding.get("frozen_before_split_open") is not True
    ):
        raise IntegrityError("run attestation live-command config artifact binding is invalid")

    adjudication_paths = document.get("adjudication_paths")
    if not isinstance(adjudication_paths, Mapping) or set(adjudication_paths) != set(adjudication):
        raise ContractError(
            "adjudication_paths must bind exactly every adjudication_sha256 entry"
        )
    for name, path in adjudication_paths.items():
        if not isinstance(path, str):
            raise ContractError(f"adjudication path for {name!r} must be text")
        _, normalized_path = _normalize_relative_file_path(path)
        if normalized_path != path or path not in run_files:
            raise IntegrityError(f"adjudication path is absent or non-canonical: {path!r}")
        if run_files[path].get("sha256") != adjudication[name]:
            raise IntegrityError(
                f"adjudication hash for {name!r} is not the actual run-file hash"
            )

    provider_io = document.get("raw_provider_io")
    if (
        not isinstance(provider_io, Mapping)
        or set(provider_io) != {"required", "paths", "stage_paths", "evaluator_paths"}
        or not isinstance(provider_io.get("required"), bool)
    ):
        raise ContractError("raw_provider_io must contain an explicit boolean required flag")
    provider_paths = provider_io.get("paths")
    stage_paths = provider_io.get("stage_paths")
    evaluator_paths = provider_io.get("evaluator_paths")
    for label, paths in (
        ("paths", provider_paths),
        ("stage_paths", stage_paths),
        ("evaluator_paths", evaluator_paths),
    ):
        if not isinstance(paths, list) or any(not isinstance(path, str) for path in paths):
            raise ContractError(f"raw_provider_io.{label} must be a list of paths")
        if paths != sorted(set(paths)):
            raise ContractError(f"raw_provider_io.{label} must be sorted and unique")
    assert isinstance(provider_paths, list)
    assert isinstance(stage_paths, list)
    assert isinstance(evaluator_paths, list)
    if provider_paths != sorted(stage_paths + evaluator_paths):
        raise IntegrityError("raw_provider_io.paths is not the exact stage/evaluator union")
    for path in stage_paths:
        if not path.endswith("/raw-provider-io.json") or path not in run_files:
            raise IntegrityError(f"stage raw provider I/O path is invalid: {path!r}")
    for path in evaluator_paths:
        if (
            "/" in path
            or not path.startswith("evaluator-raw-provider-io-")
            or not path.endswith(".json")
            or path not in run_files
        ):
            raise IntegrityError(f"evaluator raw provider I/O path is invalid: {path!r}")
    if provider_io["required"] and (not stage_paths or not evaluator_paths):
        raise IntegrityError(
            "required raw provider I/O must include stage and evaluator captures"
        )


def _validate_signing_material(key: bytes, key_id: str) -> None:
    if not isinstance(key, bytes) or not key:
        raise ContractError("signing key must be non-empty in-memory bytes")
    _nonempty_text(key_id, "key_id")


def _validate_unsigned_document(document: Mapping[str, Any], schema_version: str) -> None:
    if not isinstance(document, Mapping):
        raise ContractError("signed document body must be an object")
    if document.get("schema_version") != schema_version:
        raise ContractError(
            f"unexpected signed document schema: {document.get('schema_version')!r}"
        )
    if "signature" in document:
        raise ContractError("write wrapper accepts an unsigned document only")
    json_bytes(document)


def _read_and_verify_signed(
    path: Path, key: bytes, schema_version: str
) -> Mapping[str, Any]:
    if not isinstance(key, bytes) or not key:
        raise ContractError("verification key must be non-empty in-memory bytes")
    source = Path(path)
    _reject_link_like(source, label="signed document")
    document = read_json(source)
    if not isinstance(document, Mapping):
        raise ContractError("signed document must be an object")
    if document.get("schema_version") != schema_version:
        raise ContractError(
            f"unexpected signed document schema: {document.get('schema_version')!r}"
        )
    verify_signed_document(document, key)
    return document
