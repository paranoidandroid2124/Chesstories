from __future__ import annotations

import copy
import hashlib
import json
import os
import re
import tempfile
from collections import Counter
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import chess

from .canonicalize import Canonicalizer
from .capture import ArtifactStore, utc_now
from .hashing import json_bytes, read_json, read_jsonl, sha256_file, sha256_json
from .model import ContractError, IntegrityError
from .probe_completion import (
    MAX_RUNTIME_PROBE_RESULTS,
    _RuntimeJsonlSession,
    _search_probe,
    _validate_probe_request,
)
from .schemas import SchemaRegistry
from .stockfish import acquire_stockfish_evidence


FREEZE_SCHEMA_VERSION = "chesstory.eval.cause-audit-freeze.v1"
ACQUISITION_SCHEMA_VERSION = "chesstory.eval.cause-audit-acquisition.v1"
RUNTIME_RUN_SCHEMA_VERSION = "chesstory.eval.cause-audit-runtime-run.v1"
ACTUAL_VIEW_SCHEMA_VERSION = "chesstory.eval.cause-audit-actual-cascade.v1"
JUDGMENT_SCHEMA_VERSION = "chesstory.eval.cause-audit-judgment.v1"
REPORT_SCHEMA_VERSION = "chesstory.eval.cause-audit-report.v1"
RUNTIME_REQUEST_SCHEMA = "chesstory.move-meaning.request.v1"
RUNTIME_OBSERVATION_SCHEMA = "chesstory.cause-audit-runtime-observation.v1"
CAUSE_ADAPTER_MAIN = (
    "io.chesstory.evaluation.runtimeadapter.CauseAuditAdapterCli"
)
PARTITIONS = ("explore", "sealed_confirm")
SPLIT_SALT = "cause-audit-v1-family-seal"


def _schema_path(root: Path, name: str) -> Path:
    return root / "schemas" / "cause-audit-v1" / f"{name}.schema.json"


def _schema_tools(root: Path) -> tuple[SchemaRegistry, Canonicalizer]:
    registry = SchemaRegistry(root / "schemas" / "cause-audit-v1")
    return registry, Canonicalizer(registry)


def _assert_ascii(value: Any, label: str) -> None:
    if isinstance(value, str):
        try:
            value.encode("ascii")
        except UnicodeEncodeError as error:
            raise ContractError(f"{label} contains non-ASCII text") from error
        return
    if isinstance(value, Mapping):
        for key, child in value.items():
            _assert_ascii(key, f"{label} key")
            _assert_ascii(child, f"{label}.{key}")
        return
    if isinstance(value, list):
        for index, child in enumerate(value):
            _assert_ascii(child, f"{label}[{index}]")


def _object(value: Any, label: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object")
    try:
        return json.loads(
            json.dumps(
                value,
                ensure_ascii=True,
                sort_keys=True,
                separators=(",", ":"),
                allow_nan=False,
            )
        )
    except (TypeError, ValueError) as error:
        raise ContractError(f"{label} must be finite JSON") from error


def _read_object(path: Path, label: str) -> dict[str, Any]:
    resolved = path.resolve(strict=True)
    value = read_json(resolved)
    return _object(value, label)


def _validate_and_canonicalize(
    *,
    registry: SchemaRegistry,
    canonicalizer: Canonicalizer,
    schema_path: Path,
    document: Mapping[str, Any],
    label: str,
) -> dict[str, Any]:
    value = _object(document, label)
    _assert_ascii(value, label)
    registry.validate_document(value, schema_path, label=label)
    schema = registry.load(schema_path)
    canonicalizer.assert_idempotent(value, schema, schema_path)
    canonical = canonicalizer.canonicalize(value, schema, schema_path)
    if not isinstance(canonical, dict):
        raise ContractError(f"{label} canonicalization did not produce an object")
    registry.validate_document(canonical, schema_path, label=f"canonical {label}")
    return canonical


def _load_cases(
    root: Path,
    path: Path,
    registry: SchemaRegistry,
    canonicalizer: Canonicalizer,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    resolved = path.resolve(strict=True)
    try:
        raw_rows = read_jsonl(resolved)
    except ValueError as error:
        raise ContractError(str(error)) from error
    if not raw_rows:
        raise ContractError("cause audit case file is empty")
    rows: list[dict[str, Any]] = []
    canonical: list[dict[str, Any]] = []
    schema_path = _schema_path(root, "case")
    seen_ids: set[str] = set()
    seen_inventory: set[str] = set()
    seen_source_ordinals: set[tuple[str, int]] = set()
    for index, raw in enumerate(raw_rows, 1):
        row = _object(raw, f"case row {index}")
        item = _validate_and_canonicalize(
            registry=registry,
            canonicalizer=canonicalizer,
            schema_path=schema_path,
            document=row,
            label=f"case row {index}",
        )
        case_id = str(row["case_id"])
        inventory_id = str(row["inventory_id"])
        source = _object(row["source"], f"case {case_id} source")
        source_ordinal = int(row["source_ordinal"])
        source_key = (str(source["document_id"]), source_ordinal)
        if case_id in seen_ids:
            raise ContractError(f"duplicate cause audit case_id: {case_id}")
        if inventory_id in seen_inventory:
            raise ContractError(f"duplicate cause audit inventory_id: {inventory_id}")
        if source_key in seen_source_ordinals:
            raise ContractError(
                f"duplicate source ordinal: {source_key[0]}/{source_key[1]}"
            )
        seen_ids.add(case_id)
        seen_inventory.add(inventory_id)
        seen_source_ordinals.add(source_key)
        expected_partition = (
            "sealed_confirm" if source_ordinal % 3 == 0 else "explore"
        )
        if row["partition"] != expected_partition:
            raise ContractError(
                f"case {case_id} violates the frozen every-third partition rule"
            )
        try:
            board = chess.Board(str(row["fen"]))
        except ValueError as error:
            raise ContractError(f"case {case_id} has malformed FEN") from error
        if board.status() != chess.STATUS_VALID:
            raise ContractError(
                f"case {case_id} has illegal FEN (status={int(board.status())})"
            )
        side = "white" if board.turn == chess.WHITE else "black"
        if row["side_to_move"] != side:
            raise ContractError(f"case {case_id} side_to_move disagrees with FEN")
        move = chess.Move.from_uci(str(row["played_move_uci"]))
        if not board.is_legal(move):
            raise ContractError(f"case {case_id} played move is illegal")
        rows.append(row)
        canonical.append(item)
    rows.sort(key=lambda item: str(item["case_id"]))
    canonical.sort(key=lambda item: str(item["case_id"]))
    return rows, canonical


def _replay_uci(board: chess.Board, moves: Sequence[Any], label: str) -> None:
    replay = board.copy(stack=False)
    for index, raw in enumerate(moves):
        try:
            move = chess.Move.from_uci(str(raw))
        except ValueError as error:
            raise ContractError(f"{label}[{index}] is not a UCI move") from error
        if not replay.is_legal(move):
            raise ContractError(f"{label}[{index}] is illegal at its replay position")
        replay.push(move)


def _load_labels(
    root: Path,
    path: Path,
    cases: Sequence[Mapping[str, Any]],
    registry: SchemaRegistry,
    canonicalizer: Canonicalizer,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    resolved = path.resolve(strict=True)
    try:
        raw_rows = read_jsonl(resolved)
    except ValueError as error:
        raise ContractError(str(error)) from error
    schema_path = _schema_path(root, "oracle-label")
    case_by_id = {str(item["case_id"]): item for item in cases}
    rows: list[dict[str, Any]] = []
    canonical: list[dict[str, Any]] = []
    seen: set[str] = set()
    for index, raw in enumerate(raw_rows, 1):
        row = _object(raw, f"oracle label row {index}")
        item = _validate_and_canonicalize(
            registry=registry,
            canonicalizer=canonicalizer,
            schema_path=schema_path,
            document=row,
            label=f"oracle label row {index}",
        )
        case_id = str(row["case_id"])
        if case_id in seen:
            raise ContractError(f"duplicate oracle case_id: {case_id}")
        case = case_by_id.get(case_id)
        if case is None:
            raise ContractError(f"oracle label references an unknown case_id: {case_id}")
        seen.add(case_id)
        board = chess.Board(str(case["fen"]))
        reference = chess.Move.from_uci(str(row["expected_reference_move_uci"]))
        if not board.is_legal(reference):
            raise ContractError(f"oracle label {case_id} reference move is illegal")
        alternatives = [str(value) for value in row["acceptable_alternatives"]]
        if str(reference) not in alternatives:
            raise ContractError(
                f"oracle label {case_id} acceptable alternatives omit its reference move"
            )
        for alternative in alternatives:
            move = chess.Move.from_uci(alternative)
            if not board.is_legal(move):
                raise ContractError(
                    f"oracle label {case_id} contains an illegal acceptable alternative"
                )
        required_pv = list(row["required_pv"])
        if required_pv:
            _replay_uci(board, required_pv, f"oracle label {case_id} required_pv")
        if row["answerable"] and not row["expected_cause_kinds"]:
            raise ContractError(
                f"answerable oracle label {case_id} has no expected cause kind"
            )
        rows.append(row)
        canonical.append(item)
    if seen != set(case_by_id):
        missing = sorted(set(case_by_id) - seen)
        raise ContractError(
            f"oracle labels do not cover the frozen cases: {missing[:5]}"
        )
    rows.sort(key=lambda item: str(item["case_id"]))
    canonical.sort(key=lambda item: str(item["case_id"]))
    return rows, canonical


def _ids_hash(rows: Sequence[Mapping[str, Any]]) -> str:
    return sha256_json(sorted(str(item["case_id"]) for item in rows))


def _family_balanced_split(
    cases: Sequence[Mapping[str, Any]],
    contamination_exclusions: Sequence[Mapping[str, Any]] = (),
) -> tuple[dict[str, list[str]], list[dict[str, Any]], str]:
    exclusion_by_id: dict[str, str] = {}
    for raw in contamination_exclusions:
        item = _object(raw, "contamination exclusion")
        case_id = str(item.get("case_id", ""))
        reason = str(item.get("reason", ""))
        if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", case_id):
            raise ContractError("contamination exclusion case_id is not canonical")
        if not reason or "\r" in reason or "\n" in reason:
            raise ContractError("contamination exclusion reason is invalid")
        _assert_ascii(reason, "contamination exclusion reason")
        if case_id in exclusion_by_id:
            raise ContractError(f"duplicate contamination exclusion: {case_id}")
        exclusion_by_id[case_id] = reason
    by_family: dict[str, list[Mapping[str, Any]]] = {}
    for case in cases:
        by_family.setdefault(str(case["cause_family"]), []).append(case)
    if len(by_family) != 8 or any(len(values) != 3 for values in by_family.values()):
        raise ContractError(
            "family-balanced split requires exactly 8 cause families with 3 cases each"
        )
    case_ids = {str(case["case_id"]) for case in cases}
    if not set(exclusion_by_id).issubset(case_ids):
        raise ContractError("contamination exclusion references an unknown case")
    canonical_exclusions = [
        {"case_id": case_id, "reason": exclusion_by_id[case_id]}
        for case_id in sorted(exclusion_by_id)
    ]
    assignments: list[dict[str, Any]] = []
    partitions = {"explore": [], "sealed_confirm": []}
    for family in sorted(by_family):
        candidates: list[tuple[str, Mapping[str, Any]]] = []
        for case in by_family[family]:
            case_id = str(case["case_id"])
            digest = hashlib.sha256(
                f"{SPLIT_SALT}|{family}|{case_id}".encode("ascii")
            ).hexdigest()
            candidates.append((digest, case))
        eligible = [
            item for item in candidates if str(item[1]["case_id"]) not in exclusion_by_id
        ]
        if not eligible:
            raise ContractError(f"all cases in cause family {family} are contaminated")
        sealed_id = str(min(eligible, key=lambda item: (item[0], str(item[1]["case_id"])))[1]["case_id"])
        for digest, case in candidates:
            case_id = str(case["case_id"])
            logical = "sealed_confirm" if case_id == sealed_id else "explore"
            partitions[logical].append(case_id)
            assignments.append(
                {
                    "case_id": case_id,
                    "cause_family": family,
                    "embedded_partition": str(case["partition"]),
                    "logical_partition": logical,
                    "sealed_eligible": case_id not in exclusion_by_id,
                    "selection_sha256": digest,
                }
            )
    for values in partitions.values():
        values.sort()
    assignments.sort(key=lambda item: str(item["case_id"]))
    if len(partitions["explore"]) != 16 or len(partitions["sealed_confirm"]) != 8:
        raise ContractError("family-balanced split did not produce a 16/8 partition")
    sealed_families = Counter(
        item["cause_family"]
        for item in assignments
        if item["logical_partition"] == "sealed_confirm"
    )
    explore_families = Counter(
        item["cause_family"]
        for item in assignments
        if item["logical_partition"] == "explore"
    )
    if set(sealed_families.values()) != {1} or set(explore_families.values()) != {2}:
        raise ContractError("family-balanced split does not preserve 2 explore and 1 sealed per family")
    return (
        partitions,
        assignments,
        sha256_json(
            {
                "contamination_exclusions": canonical_exclusions,
                "case_assignments": assignments,
            }
        ),
    )


def _exclusion_spec(value: str) -> dict[str, str]:
    case_id, separator, reason = value.partition("=")
    if (
        not separator
        or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", case_id)
        or not reason
    ):
        raise ContractError("--contamination-exclusion must be CASE_ID=REASON")
    _assert_ascii(reason, "contamination exclusion reason")
    return {"case_id": case_id, "reason": reason}


def _binding_spec(value: str, label: str) -> tuple[str, Path]:
    name, separator, raw_path = value.partition("=")
    if not separator or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", name):
        raise ContractError(f"{label} must be ORACLE_ID=PATH")
    if not raw_path:
        raise ContractError(f"{label} path is empty")
    return name, Path(raw_path).resolve(strict=True)


def freeze_cause_audit(
    *,
    root: Path,
    store: ArtifactStore,
    cases_path: Path,
    oracle_specs: Sequence[str],
    adjudicated_path: Path | None,
    corpus_id: str,
    contamination_exclusion_specs: Sequence[str],
) -> dict[str, Any]:
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", corpus_id):
        raise ContractError("cause audit corpus_id is not canonical")
    registry, canonicalizer = _schema_tools(root)
    cases, canonical_cases = _load_cases(
        root, cases_path, registry, canonicalizer
    )
    specifications: list[tuple[str, str, Path]] = []
    for raw in oracle_specs:
        oracle_id, path = _binding_spec(raw, "--oracle-label")
        specifications.append((oracle_id, "independent_oracle", path))
    if adjudicated_path is not None:
        specifications.append(
            ("adjudicated", "adjudicated_oracle", adjudicated_path.resolve(strict=True))
        )
    if not specifications:
        raise ContractError("freeze requires at least one oracle label source")
    oracle_ids = [item[0] for item in specifications]
    if len(set(oracle_ids)) != len(oracle_ids):
        raise ContractError("oracle IDs must be unique")
    label_bindings: list[dict[str, Any]] = []
    for oracle_id, role, path in specifications:
        labels, canonical_labels = _load_labels(
            root, path, cases, registry, canonicalizer
        )
        label_bindings.append(
            {
                "oracle_id": oracle_id,
                "role": role,
                "label_count": len(labels),
                "source_file_sha256": sha256_file(path),
                "canonical_labels_sha256": sha256_json(canonical_labels),
                "case_ids_sha256": _ids_hash(labels),
            }
        )
    contamination_exclusions = [
        _exclusion_spec(value) for value in contamination_exclusion_specs
    ]
    partition_case_ids, case_assignments, assignment_hash = _family_balanced_split(
        cases, contamination_exclusions
    )
    manifest = {
        "schema_version": FREEZE_SCHEMA_VERSION,
        "corpus_id": corpus_id,
        "frozen_at": utc_now(),
        "case_binding": {
            "case_count": len(cases),
            "source_file_sha256": sha256_file(cases_path.resolve(strict=True)),
            "canonical_cases_sha256": sha256_json(canonical_cases),
            "case_ids_sha256": _ids_hash(cases),
        },
        "split_assignment": {
            "algorithm": "minimum-sha256-per-cause-family-after-contamination-exclusions.v1",
            "salt": SPLIT_SALT,
            "input_format": "{salt}|{cause_family}|{case_id}",
            "unit": "cause_family",
            "contamination_exclusions": contamination_exclusions,
            "family_count": 8,
            "cases_per_family": 3,
            "sealed_per_family": 1,
            "assignment_sha256": assignment_hash,
            "case_assignments": case_assignments,
        },
        "partition_case_ids": partition_case_ids,
        "label_bindings": label_bindings,
        "canonicalization": {
            "contract": "schema-authorized-canonical-json-sha256.v1",
            "row_order": "case_id_ascending",
            "unordered_arrays_from_schema": True,
            "ascii_only": True,
        },
        "policy": {
            "machine_language": "en",
            "sealed_labels_disclosed": False,
            "test_files_added": False,
            "production_modules_duplicated": False,
        },
    }
    canonical = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(root, "freeze-manifest"),
        document=manifest,
        label="cause audit freeze manifest",
    )
    store.capture_run_document("cause-audit-freeze-manifest", canonical)
    store.verify()
    return canonical


def _verify_manifest(
    *,
    root: Path,
    manifest_path: Path,
    cases_path: Path,
    registry: SchemaRegistry,
    canonicalizer: Canonicalizer,
) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    manifest = _read_object(manifest_path, "cause audit freeze manifest")
    manifest = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(root, "freeze-manifest"),
        document=manifest,
        label="cause audit freeze manifest",
    )
    cases, canonical_cases = _load_cases(
        root, cases_path, registry, canonicalizer
    )
    binding = _object(manifest["case_binding"], "freeze case binding")
    checks = {
        "case_count": len(cases),
        "source_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "canonical_cases_sha256": sha256_json(canonical_cases),
        "case_ids_sha256": _ids_hash(cases),
    }
    if binding != checks:
        raise IntegrityError("cause case file no longer matches the freeze manifest")
    split_assignment = _object(
        manifest["split_assignment"], "cause audit split assignment"
    )
    exclusions = split_assignment.get("contamination_exclusions")
    if not isinstance(exclusions, list):
        raise IntegrityError("cause audit split has no contamination exclusion array")
    actual_partitions, actual_assignments, actual_assignment_hash = _family_balanced_split(
        cases, [item for item in exclusions if isinstance(item, Mapping)]
    )
    if manifest["partition_case_ids"] != actual_partitions:
        raise IntegrityError("cause case partitions no longer match the freeze manifest")
    expected_split = {
        "algorithm": "minimum-sha256-per-cause-family-after-contamination-exclusions.v1",
        "salt": SPLIT_SALT,
        "input_format": "{salt}|{cause_family}|{case_id}",
        "unit": "cause_family",
        "contamination_exclusions": exclusions,
        "family_count": 8,
        "cases_per_family": 3,
        "sealed_per_family": 1,
        "assignment_sha256": actual_assignment_hash,
        "case_assignments": actual_assignments,
    }
    if manifest["split_assignment"] != expected_split:
        raise IntegrityError("cause audit split assignment no longer matches its deterministic rule")
    return manifest, cases


def _verify_bound_labels(
    *,
    root: Path,
    path: Path,
    manifest: Mapping[str, Any],
    cases: Sequence[Mapping[str, Any]],
    registry: SchemaRegistry,
    canonicalizer: Canonicalizer,
) -> list[dict[str, Any]]:
    labels, canonical = _load_labels(root, path, cases, registry, canonicalizer)
    file_hash = sha256_file(path.resolve(strict=True))
    canonical_hash = sha256_json(canonical)
    matches = [
        item
        for item in manifest["label_bindings"]
        if isinstance(item, Mapping)
        and item.get("source_file_sha256") == file_hash
        and item.get("canonical_labels_sha256") == canonical_hash
        and item.get("case_ids_sha256") == _ids_hash(labels)
    ]
    if len(matches) != 1:
        raise IntegrityError("oracle labels are not uniquely bound by the freeze manifest")
    return labels


def freeze_candidate_binding(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    candidate_id: str,
    component_paths: Sequence[Path],
) -> dict[str, Any]:
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", candidate_id):
        raise ContractError("bind-candidate requires a canonical --candidate-id")
    registry, canonicalizer = _schema_tools(root)
    _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    if not component_paths:
        raise ContractError("bind-candidate requires at least one --component")
    repository_root = root.parent.resolve(strict=True)
    components: list[dict[str, Any]] = []
    seen: set[str] = set()
    for supplied in component_paths:
        if supplied.is_symlink():
            raise ContractError("candidate components must not be symbolic links")
        path = supplied.resolve(strict=True)
        if not path.is_file():
            raise ContractError("candidate component must be a regular file")
        try:
            relative = path.relative_to(repository_root).as_posix()
        except ValueError as error:
            raise ContractError("candidate component escapes the repository") from error
        _assert_ascii(relative, "candidate component path")
        if relative in seen:
            raise ContractError(f"duplicate candidate component: {relative}")
        seen.add(relative)
        components.append(
            {
                "path": relative,
                "sha256": sha256_file(path),
                "size_bytes": path.stat().st_size,
            }
        )
    components.sort(key=lambda item: str(item["path"]))
    document = {
        "schema_version": "chesstory.eval.cause-audit-candidate-binding.v1",
        "candidate_id": candidate_id,
        "frozen_at": utc_now(),
        "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "components": components,
        "component_set_sha256": sha256_json(components),
        "policy": {
            "purpose": "post-fix-sealed-cause-audit-candidate",
            "repository_relative_paths": True,
            "test_files_added": False,
            "production_modules_duplicated": False,
        },
    }
    _assert_ascii(document, "cause audit candidate binding")
    _validate_candidate_binding(
        root=root,
        path=None,
        document=document,
        manifest_path=manifest_path,
        cases_path=cases_path,
    )
    store.capture_run_document("cause-audit-candidate-binding", document)
    store.verify()
    return document


def _validate_candidate_binding(
    *,
    root: Path,
    path: Path | None,
    document: Mapping[str, Any] | None,
    manifest_path: Path,
    cases_path: Path,
) -> str:
    value = (
        _read_object(path, "cause audit candidate binding")
        if path is not None
        else _object(document, "cause audit candidate binding")
    )
    required = {
        "schema_version",
        "candidate_id",
        "frozen_at",
        "freeze_manifest_sha256",
        "cases_file_sha256",
        "components",
        "component_set_sha256",
        "policy",
    }
    if set(value) != required:
        raise ContractError("candidate binding fields are not exact")
    if value["schema_version"] != "chesstory.eval.cause-audit-candidate-binding.v1":
        raise ContractError("unsupported cause audit candidate binding schema")
    if value["freeze_manifest_sha256"] != sha256_file(manifest_path.resolve(strict=True)):
        raise IntegrityError("candidate binding freeze manifest changed")
    if value["cases_file_sha256"] != sha256_file(cases_path.resolve(strict=True)):
        raise IntegrityError("candidate binding cases file changed")
    components = value.get("components")
    if not isinstance(components, list) or not components:
        raise ContractError("candidate binding has no components")
    repository_root = root.parent.resolve(strict=True)
    verified: list[dict[str, Any]] = []
    seen: set[str] = set()
    for raw in components:
        item = _object(raw, "candidate component binding")
        if set(item) != {"path", "sha256", "size_bytes"}:
            raise ContractError("candidate component fields are not exact")
        relative = str(item["path"])
        clean = Path(relative)
        if clean.is_absolute() or ".." in clean.parts or clean.as_posix() != relative:
            raise ContractError("candidate component path is not repository-relative")
        if relative in seen:
            raise ContractError(f"duplicate candidate component binding: {relative}")
        seen.add(relative)
        component = repository_root / clean
        if component.is_symlink() or not component.is_file():
            raise IntegrityError(f"candidate component is unavailable: {relative}")
        current = {
            "path": relative,
            "sha256": sha256_file(component),
            "size_bytes": component.stat().st_size,
        }
        if item != current:
            raise IntegrityError(f"candidate component changed after freeze: {relative}")
        verified.append(current)
    verified.sort(key=lambda item: str(item["path"]))
    if components != verified or value["component_set_sha256"] != sha256_json(verified):
        raise IntegrityError("candidate component set binding failed")
    expected_policy = {
        "purpose": "post-fix-sealed-cause-audit-candidate",
        "repository_relative_paths": True,
        "test_files_added": False,
        "production_modules_duplicated": False,
    }
    if value.get("policy") != expected_policy:
        raise ContractError("candidate binding policy is invalid")
    _assert_ascii(value, "cause audit candidate binding")
    return sha256_file(path.resolve(strict=True)) if path is not None else sha256_json(value)


def _selected_cases(
    cases: Sequence[Mapping[str, Any]],
    manifest: Mapping[str, Any],
    partition: str,
    requested_case_ids: Sequence[str] = (),
) -> list[dict[str, Any]]:
    if partition not in {*PARTITIONS, "all"}:
        raise ContractError(f"unknown cause audit partition: {partition}")
    assignments = manifest.get("split_assignment", {}).get("case_assignments", [])
    logical_by_id = {
        str(item["case_id"]): str(item["logical_partition"])
        for item in assignments
        if isinstance(item, Mapping)
    }
    requested = set(requested_case_ids)
    if any(
        not isinstance(case_id, str)
        or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", case_id)
        for case_id in requested
    ):
        raise ContractError("--case-id contains a non-canonical ID")
    if len(requested) != len(requested_case_ids):
        raise ContractError("--case-id values must be unique")
    known_ids = {str(item["case_id"]) for item in cases}
    if not requested.issubset(known_ids):
        raise ContractError(
            f"--case-id contains unknown cases: {sorted(requested - known_ids)[:5]}"
        )
    selected: list[dict[str, Any]] = []
    for item in cases:
        case_id = str(item["case_id"])
        logical = logical_by_id.get(case_id)
        if logical not in PARTITIONS:
            raise IntegrityError(f"freeze manifest has no logical partition for {case_id}")
        if partition != "all" and logical != partition:
            continue
        if requested and case_id not in requested:
            continue
        value = dict(item)
        value["embedded_partition"] = value["partition"]
        value["partition"] = logical
        selected.append(value)
    return selected


_FAMILY_TAGS: dict[str, list[str]] = {
    "activity_restriction": ["strategic"],
    "conversion_control": ["conversion"],
    "drawing_defense": ["defensive", "evaluation"],
    "move_order_tempo": ["strategic", "tactical"],
    "pawn_breaks_center": ["strategic"],
    "strategic_plan": ["strategic"],
    "structure_targets": ["strategic"],
    "tactical_resource": ["tactical"],
}


def _question(
    case: Mapping[str, Any], reference_label: Mapping[str, Any] | None
) -> dict[str, Any]:
    references: list[str] = []
    if reference_label is not None:
        references = list(
            dict.fromkeys(
                [
                    str(reference_label["expected_reference_move_uci"]),
                    *[str(item) for item in reference_label["acceptable_alternatives"]],
                ]
            )
        )
    return {
        "root_position": {
            "position_id": str(case["case_id"]),
            "fen": str(case["fen"]),
            "side_to_move": str(case["side_to_move"]),
        },
        "played_move_uci": str(case["played_move_uci"]),
        "move_history_uci": [],
        "requested_reference_moves_uci": references,
        "game_phase": str(case["game_phase"]),
        "family_tags": list(_FAMILY_TAGS[str(case["cause_family"])]),
    }


def _acquisition_config(arguments: Any) -> dict[str, Any]:
    return {
        "depth": int(arguments.depth),
        "multipv": int(arguments.multipv),
        "repetitions": int(arguments.repetitions),
        "stability_tolerance_cp": int(arguments.stability_tolerance_cp),
        "probe_kinds": [],
        "tablebase": "disabled",
        "engine_options": {
            "Threads": int(arguments.threads),
            "Hash": int(arguments.hash_mb),
        },
    }


def _atomic_cache_write(path: Path, document: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = json_bytes(document)
    if path.exists():
        existing = path.read_bytes()
        if existing != payload:
            raise IntegrityError(f"cache key collision at {path}")
        return
    temporary_name: str | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="wb", dir=path.parent, prefix=f".{path.name}.", delete=False
        ) as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
            temporary_name = handle.name
        try:
            os.replace(temporary_name, path)
        except OSError:
            if not path.exists() or path.read_bytes() != payload:
                raise
    finally:
        if temporary_name is not None:
            temporary = Path(temporary_name)
            if temporary.exists():
                temporary.unlink()


def _cache_object(path: Path, expected_key: str, label: str) -> dict[str, Any]:
    document = _read_object(path, label)
    if document.get("cache_key_sha256") != expected_key:
        raise IntegrityError(f"{label} cache key binding failed")
    return document


def _line_to_runtime(
    line: Mapping[str, Any], *, side_to_move: str, label: str
) -> dict[str, Any]:
    moves = line.get("moves_uci")
    evaluation = line.get("evaluation")
    depth = line.get("depth")
    if (
        not isinstance(moves, list)
        or not moves
        or not all(isinstance(item, str) and item for item in moves)
        or not isinstance(evaluation, Mapping)
        or evaluation.get("perspective") != "root-side"
        or isinstance(depth, bool)
        or not isinstance(depth, int)
        or depth < 1
    ):
        raise ContractError(f"{label} is not runtime-compatible")
    perspective = 1 if side_to_move == "white" else -1
    centipawns = evaluation.get("centipawns")
    mate_in = evaluation.get("mate_in")
    if (centipawns is None) == (mate_in is None):
        raise ContractError(f"{label} must have exactly one score kind")
    if centipawns is not None:
        if isinstance(centipawns, bool) or not isinstance(centipawns, int):
            raise ContractError(f"{label} centipawn score is invalid")
        score_cp = perspective * centipawns
        mate = None
    else:
        if isinstance(mate_in, bool) or not isinstance(mate_in, int) or mate_in == 0:
            raise ContractError(f"{label} mate score is invalid")
        mate = perspective * mate_in
        score_cp = 100000 if mate > 0 else -100000
    return {
        "moves": list(moves),
        "scoreCp": score_cp,
        "mate": mate,
        "depth": depth,
    }


def _runtime_request(case: Mapping[str, Any], pack: Mapping[str, Any]) -> dict[str, Any]:
    raw_lines: list[Mapping[str, Any]] = []
    for field in ("candidate_lines", "forced_required_lines"):
        value = pack.get(field)
        if not isinstance(value, list):
            raise ContractError(f"Stockfish pack has no {field} array")
        for item in value:
            if not isinstance(item, Mapping):
                raise ContractError(f"Stockfish pack {field} contains a non-object")
            raw_lines.append(item)
    seen_moves: set[str] = set()
    variations: list[dict[str, Any]] = []
    for index, line in enumerate(raw_lines):
        root_move = str(line.get("root_move_uci", ""))
        if not root_move or root_move in seen_moves:
            continue
        seen_moves.add(root_move)
        variations.append(
            _line_to_runtime(
                line,
                side_to_move=str(case["side_to_move"]),
                label=f"Stockfish line {index}",
            )
        )
    if str(case["played_move_uci"]) not in seen_moves:
        raise ContractError(
            f"Stockfish pack does not cover played move for {case['case_id']}"
        )
    return {
        "schema_version": RUNTIME_REQUEST_SCHEMA,
        "request_id": str(case["case_id"]),
        "input": {
            "fen": str(case["fen"]),
            "playedMoveUci": str(case["played_move_uci"]),
            "variations": variations,
            "movePrefixUci": [],
        },
    }


def acquire_cause_audit(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    reference_labels_path: Path | None,
    stockfish_path: Path,
    cache_root: Path,
    partition: str,
    arguments: Any,
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root)
    manifest, cases = _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    label_by_case: dict[str, dict[str, Any]] = {}
    reference_label_hash: str | None = None
    if reference_labels_path is not None:
        labels = _verify_bound_labels(
            root=root,
            path=reference_labels_path,
            manifest=manifest,
            cases=cases,
            registry=registry,
            canonicalizer=canonicalizer,
        )
        label_by_case = {str(item["case_id"]): item for item in labels}
        reference_label_hash = sha256_file(reference_labels_path.resolve(strict=True))
    selected = _selected_cases(cases, manifest, partition, arguments.case_id)
    if not selected:
        raise ContractError("selected cause audit partition is empty")
    executable = stockfish_path.resolve(strict=True)
    engine_sha256 = sha256_file(executable)
    config = _acquisition_config(arguments)
    rows: list[dict[str, Any]] = []
    cache_hits = 0
    for case in selected:
        case_id = str(case["case_id"])
        question = _question(case, label_by_case.get(case_id))
        cache_key = sha256_json(
            {
                "contract": "cause-audit-stockfish-cache.v1",
                "engine_sha256": engine_sha256,
                "question": question,
                "acquisition_config": config,
            }
        )
        cache_path = (
            cache_root.resolve()
            / "stockfish"
            / engine_sha256
            / f"{cache_key}.json"
        )
        if cache_path.is_file():
            cached = _cache_object(
                cache_path, cache_key, f"Stockfish acquisition for {case_id}"
            )
            if cached.get("engine_sha256") != engine_sha256:
                raise IntegrityError("Stockfish acquisition cache engine binding failed")
            pack = _object(cached.get("engine_evidence_pack"), "cached engine pack")
            diagnostics = _object(cached.get("diagnostics"), "cached diagnostics")
            raw_provider_io = _object(cached.get("raw_provider_io"), "cached UCI I/O")
            cache_hit = True
            cache_hits += 1
        else:
            result = acquire_stockfish_evidence(
                executable,
                question,
                config,
                timeout_seconds=float(arguments.timeout_seconds),
            )
            pack = _object(result.engine_evidence_pack, "engine evidence pack")
            diagnostics = _object(result.diagnostics, "engine diagnostics")
            raw_provider_io = _object(result.raw_provider_io, "raw engine I/O")
            cached = {
                "schema_version": "chesstory.eval.cause-audit-stockfish-cache.v1",
                "cache_key_sha256": cache_key,
                "engine_sha256": engine_sha256,
                "question_sha256": sha256_json(question),
                "acquisition_config_sha256": sha256_json(config),
                "engine_evidence_pack": pack,
                "diagnostics": diagnostics,
                "raw_provider_io": raw_provider_io,
            }
            _assert_ascii(cached, f"Stockfish cache {case_id}")
            _atomic_cache_write(cache_path, cached)
            cache_hit = False
        runtime_request = _runtime_request(case, pack)
        capture_hashes = store.capture_stage(
            sample_id=case_id,
            arm_id="cause-audit-root",
            stage="Q",
            raw_input={"question": question, "acquisition_config": config},
            raw_output=pack,
            validated_output={"engine_evidence_pack": pack, "diagnostics": diagnostics},
            canonical_output={"engine_evidence_pack": pack, "diagnostics": diagnostics},
            provider_io=raw_provider_io,
            metadata={
                "source": "cause-audit-stockfish-acquisition",
                "cache_key_sha256": cache_key,
                "cache_hit": cache_hit,
                "engine_sha256": engine_sha256,
                "runtime_request_sha256": sha256_json(runtime_request),
            },
        )
        rows.append(
            {
                "case_id": case_id,
                "partition": str(case["partition"]),
                "cache_key_sha256": cache_key,
                "cache_hit": cache_hit,
                "question_sha256": sha256_json(question),
                "engine_evidence_pack_sha256": sha256_json(pack),
                "diagnostics": diagnostics,
                "runtime_request": runtime_request,
                "capture_hashes": dict(capture_hashes),
            }
        )
    document = {
        "schema_version": ACQUISITION_SCHEMA_VERSION,
        "run_id": store.run_id,
        "partition": partition,
        "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "reference_labels_file_sha256": reference_label_hash,
        "engine_sha256": engine_sha256,
        "acquisition_config": config,
        "case_count": len(rows),
        "cache": {
            "contract": "cause-audit-stockfish-cache.v1",
            "hit_count": cache_hits,
            "miss_count": len(rows) - cache_hits,
        },
        "rows": rows,
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    _assert_ascii(document, "cause audit acquisition report")
    store.capture_run_document("cause-audit-acquisition", document)
    store.verify()
    return document


def _acquisition_rows(
    acquisition: Mapping[str, Any], selected: Sequence[Mapping[str, Any]]
) -> dict[str, dict[str, Any]]:
    if acquisition.get("schema_version") != ACQUISITION_SCHEMA_VERSION:
        raise ContractError("unsupported cause audit acquisition schema")
    rows = acquisition.get("rows")
    if not isinstance(rows, list):
        raise ContractError("cause audit acquisition has no rows")
    result: dict[str, dict[str, Any]] = {}
    for raw in rows:
        row = _object(raw, "cause audit acquisition row")
        case_id = str(row.get("case_id", ""))
        if not case_id or case_id in result:
            raise ContractError("cause audit acquisition has duplicate or empty case IDs")
        request = row.get("runtime_request")
        if not isinstance(request, Mapping) or request.get("schema_version") != RUNTIME_REQUEST_SCHEMA:
            raise ContractError(f"acquisition row {case_id} has no runtime request")
        result[case_id] = row
    required = {str(item["case_id"]) for item in selected}
    if not required.issubset(result):
        raise ContractError(
            f"cause audit acquisition is missing selected cases: {sorted(required - set(result))[:5]}"
        )
    return result


def _probe_cache_entry(
    *,
    cache_root: Path,
    executable: Path,
    executable_sha256: str,
    request: Mapping[str, Any],
    timeout_seconds: float,
) -> tuple[dict[str, Any], dict[str, Any], bool]:
    validated = _validate_probe_request(request)
    cache_key = sha256_json(
        {
            "contract": "cause-audit-runtime-probe-cache.v1",
            "engine_sha256": executable_sha256,
            "request": validated,
        }
    )
    cache_path = (
        cache_root.resolve()
        / "runtime-probes"
        / executable_sha256
        / f"{cache_key}.json"
    )
    if cache_path.is_file():
        cached = _cache_object(cache_path, cache_key, "runtime probe")
        if cached.get("engine_sha256") != executable_sha256:
            raise IntegrityError("runtime probe cache engine binding failed")
        result = _object(cached.get("result"), "cached runtime probe result")
        uci_io = _object(cached.get("uci_io"), "cached runtime probe UCI I/O")
        return result, uci_io, True
    result, uci_io = _search_probe(
        executable,
        validated,
        timeout_seconds=timeout_seconds,
        executable_sha256=executable_sha256,
    )
    document = {
        "schema_version": "chesstory.eval.cause-audit-runtime-probe-cache.v1",
        "cache_key_sha256": cache_key,
        "engine_sha256": executable_sha256,
        "request_sha256": sha256_json(validated),
        "result": result,
        "uci_io": uci_io,
    }
    _assert_ascii(document, "runtime probe cache")
    _atomic_cache_write(cache_path, document)
    return result, uci_io, False


def _actual_view(
    *,
    case: Mapping[str, Any],
    initial_request_sha256: str,
    final_request_sha256: str,
    observation: Mapping[str, Any],
    transports: Sequence[Mapping[str, Any]],
    issued_probe_hashes: set[str],
    result_hashes: set[str],
    issued_probe_count: int,
    stop_reason: str,
) -> dict[str, Any]:
    causes = observation.get("causes", [])
    if not isinstance(causes, list) or not all(
        isinstance(item, Mapping) for item in causes
    ):
        raise ContractError("cause adapter observation has no cause array")
    status = observation.get("status")
    if status not in {"complete", "unavailable", "adapter_error"}:
        raise ContractError("cause adapter observation status is invalid")
    boundary = observation.get("boundary")
    if not isinstance(boundary, Mapping):
        boundary = {"present": False, "reason": "boundary_not_emitted"}
    projection = observation.get("projection")
    if not isinstance(projection, Mapping):
        projection = {"present": False, "reason": "projection_not_emitted"}
    public_response = observation.get("public_response")
    if not isinstance(public_response, Mapping):
        public_response = {"present": False, "reason": "public_response_not_emitted"}
    runtime = observation.get("runtime")
    if not isinstance(runtime, Mapping):
        raise ContractError("cause adapter observation has no runtime metadata")
    remaining = observation.get("probe_requests", [])
    if not isinstance(remaining, list):
        raise ContractError("cause adapter observation probe_requests is not an array")
    return {
        "schema_version": ACTUAL_VIEW_SCHEMA_VERSION,
        "case_id": str(case["case_id"]),
        "partition": str(case["partition"]),
        "status": status,
        "initial_request_sha256": initial_request_sha256,
        "final_request_sha256": final_request_sha256,
        "runtime_observation_sha256": sha256_json(observation),
        "runtime": dict(runtime),
        "public_response": dict(public_response),
        "boundary": dict(boundary),
        "projection": dict(projection),
        "probe_closure": {
            "round_count": len(transports),
            "issued_probe_count": issued_probe_count,
            "unique_probe_count": len(issued_probe_hashes),
            "all_closed": status == "complete" and not remaining,
            "stop_reason": stop_reason,
            "probe_request_sha256": sorted(issued_probe_hashes),
            "probe_result_sha256": sorted(result_hashes),
            "runtime_transport": [dict(item) for item in transports],
        },
        "cause_count": len(causes),
        "causes": [dict(item) for item in causes],
    }


def run_cause_audit(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    acquisition_path: Path,
    stockfish_path: Path,
    sbt_path: Path,
    adapter_root: Path,
    cache_root: Path,
    partition: str,
    timeout_seconds: float,
    provider_timeout_seconds: float,
    max_probe_rounds: int,
    candidate_binding_path: Path | None,
    requested_case_ids: Sequence[str],
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root)
    manifest, cases = _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    if partition == "all":
        raise ContractError(
            "production cause runtime must run explore and sealed_confirm separately"
        )
    candidate_binding_sha256: str | None = None
    if candidate_binding_path is not None:
        candidate_binding_sha256 = _validate_candidate_binding(
            root=root,
            path=candidate_binding_path,
            document=None,
            manifest_path=manifest_path,
            cases_path=cases_path,
        )
    if partition == "sealed_confirm" and candidate_binding_sha256 is None:
        raise ContractError(
            "sealed_confirm production runtime requires a post-fix --candidate-binding"
        )
    selected = _selected_cases(cases, manifest, partition, requested_case_ids)
    acquisition = _read_object(acquisition_path, "cause audit acquisition")
    if acquisition.get("freeze_manifest_sha256") != sha256_file(
        manifest_path.resolve(strict=True)
    ):
        raise IntegrityError("acquisition freeze manifest binding failed")
    if acquisition.get("cases_file_sha256") != sha256_file(
        cases_path.resolve(strict=True)
    ):
        raise IntegrityError("acquisition cases binding failed")
    rows = _acquisition_rows(acquisition, selected)
    executable = stockfish_path.resolve(strict=True)
    executable_sha256 = sha256_file(executable)
    if acquisition.get("engine_sha256") != executable_sha256:
        raise IntegrityError("acquisition Stockfish binding failed")
    sbt = sbt_path.resolve(strict=True)
    adapter_directory = adapter_root.resolve(strict=True)
    if not adapter_directory.is_dir():
        raise ContractError("cause audit adapter root is not a directory")
    if isinstance(max_probe_rounds, bool) or not 1 <= max_probe_rounds <= 12:
        raise ContractError("max probe rounds must be from 1 through 12")
    command = [
        str(sbt),
        "-batch",
        "-error",
        f"runMain {CAUSE_ADAPTER_MAIN}",
    ]
    case_by_id = {str(item["case_id"]): item for item in selected}
    views: list[dict[str, Any]] = []
    probe_cache_hits = 0
    probe_cache_misses = 0
    with _RuntimeJsonlSession(
        command,
        cwd=adapter_directory,
        timeout_seconds=provider_timeout_seconds,
        expected_schema=RUNTIME_OBSERVATION_SCHEMA,
    ) as session:
        for case_id in sorted(case_by_id):
            case = case_by_id[case_id]
            request = copy.deepcopy(rows[case_id]["runtime_request"])
            if not isinstance(request.get("input"), dict):
                raise ContractError(f"runtime request {case_id} input is not mutable")
            transports: list[dict[str, Any]] = []
            runtime_round_io: list[dict[str, Any]] = []
            probe_io: list[dict[str, Any]] = []
            issued_probe_hashes: set[str] = set()
            result_hashes: set[str] = set()
            issued_probe_count = 0
            known_probe_results: dict[str, dict[str, Any]] = {}
            issued_probe_bindings: dict[str, str] = {}
            first_request_hash: str | None = None
            final_observation: Mapping[str, Any] | None = None
            final_request_hash: str | None = None
            stop_reason = "max_probe_rounds_reached"
            for round_index in range(1, max_probe_rounds + 2):
                exchange = session.invoke(request)
                if first_request_hash is None:
                    first_request_hash = exchange.request_jsonl_sha256
                final_request_hash = exchange.request_jsonl_sha256
                observation = exchange.observation
                final_observation = observation
                transports.append(
                    {
                        "round": round_index,
                        "request_jsonl_sha256": exchange.request_jsonl_sha256,
                        "response_jsonl_sha256": exchange.response_jsonl_sha256,
                        "diagnostic_line_sha256": sorted(
                            set(exchange.diagnostic_line_sha256)
                        ),
                        "diagnostic_stream_sha256": exchange.diagnostic_stream_sha256,
                    }
                )
                runtime_round_io.append(
                    {
                        "round": round_index,
                        "request": copy.deepcopy(request),
                        "observation": dict(observation),
                        "request_jsonl_sha256": exchange.request_jsonl_sha256,
                        "response_jsonl_sha256": exchange.response_jsonl_sha256,
                        "diagnostic_line_sha256": sorted(
                            set(exchange.diagnostic_line_sha256)
                        ),
                        "diagnostic_stream_sha256": exchange.diagnostic_stream_sha256,
                    }
                )
                raw_probes = observation.get("probe_requests", [])
                if not isinstance(raw_probes, list) or not all(
                    isinstance(item, Mapping) for item in raw_probes
                ):
                    raise ContractError("cause adapter probe_requests must be an object array")
                probes = [_validate_probe_request(item) for item in raw_probes]
                ids = [str(item["id"]) for item in probes]
                if len(ids) != len(set(ids)):
                    raise ContractError("cause adapter emitted duplicate probe IDs")
                if not probes:
                    stop_reason = (
                        "all_runtime_probes_closed"
                        if observation.get("status") == "complete"
                        else "adapter_not_complete"
                    )
                    break
                issued_probe_count += len(probes)
                current_results = request["input"].get("probeResults", [])
                if not isinstance(current_results, list):
                    raise ContractError("runtime request probeResults is not an array")
                current_ids = {
                    str(item.get("id"))
                    for item in current_results
                    if isinstance(item, Mapping)
                }
                for probe in probes:
                    probe_id = str(probe["id"])
                    request_hash = sha256_json(probe)
                    previous_binding = issued_probe_bindings.get(probe_id)
                    if previous_binding is not None and previous_binding != request_hash:
                        raise ContractError(
                            f"probe ID {probe_id} changed its request binding"
                        )
                    issued_probe_bindings[probe_id] = request_hash
                    issued_probe_hashes.add(request_hash)
                new_probes = [
                    probe for probe in probes if str(probe["id"]) not in current_ids
                ]
                if not new_probes:
                    stop_reason = "runtime_reissued_fulfilled_probes"
                    break
                if round_index > max_probe_rounds:
                    stop_reason = "max_probe_rounds_reached"
                    break
                for probe in new_probes:
                    probe_id = str(probe["id"])
                    request_hash = sha256_json(probe)
                    previous = known_probe_results.get(probe_id)
                    if previous is not None:
                        result = previous["result"]
                        uci_io = previous["uci_io"]
                        cache_hit = True
                    else:
                        result, uci_io, cache_hit = _probe_cache_entry(
                            cache_root=cache_root,
                            executable=executable,
                            executable_sha256=executable_sha256,
                            request=probe,
                            timeout_seconds=timeout_seconds,
                        )
                        known_probe_results[probe_id] = {
                            "request_sha256": request_hash,
                            "result": result,
                            "uci_io": uci_io,
                        }
                    if cache_hit:
                        probe_cache_hits += 1
                    else:
                        probe_cache_misses += 1
                    result_hashes.add(sha256_json(result))
                    probe_io.append(
                        {
                            "probe_id": probe_id,
                            "request": probe,
                            "request_sha256": request_hash,
                            "result": result,
                            "result_sha256": sha256_json(result),
                            "cache_hit": cache_hit,
                            "uci_io": uci_io,
                        }
                    )
                    current_results.append(result)
                    current_ids.add(probe_id)
                if len(current_results) > MAX_RUNTIME_PROBE_RESULTS:
                    raise ContractError(
                        f"runtime probe result limit exceeded for {case_id}"
                    )
                request["input"]["probeResults"] = current_results
            assert first_request_hash is not None
            assert final_request_hash is not None
            assert final_observation is not None
            view = _actual_view(
                case=case,
                initial_request_sha256=first_request_hash,
                final_request_sha256=final_request_hash,
                observation=final_observation,
                transports=transports,
                issued_probe_hashes=issued_probe_hashes,
                result_hashes=result_hashes,
                issued_probe_count=issued_probe_count,
                stop_reason=stop_reason,
            )
            canonical_view = _validate_and_canonicalize(
                registry=registry,
                canonicalizer=canonicalizer,
                schema_path=_schema_path(root, "actual-cascade-view"),
                document=view,
                label=f"actual cause cascade {case_id}",
            )
            capture_hashes = store.capture_stage(
                sample_id=case_id,
                arm_id="cause-audit-actual",
                stage="C",
                raw_input=rows[case_id]["runtime_request"],
                raw_output=final_observation,
                validated_output=view,
                canonical_output=canonical_view,
                provider_io={
                    "runtime_rounds": runtime_round_io,
                    "runtime_probe_uci": probe_io,
                    "final_runtime_request": request,
                },
                metadata={
                    "source": "compact-cause-audit-runtime-adapter",
                    "runtime_observation_sha256": sha256_json(final_observation),
                    "canonical_view_sha256": sha256_json(canonical_view),
                    "all_runtime_probes_closed": canonical_view["probe_closure"][
                        "all_closed"
                    ],
                },
            )
            canonical_view["capture_hashes"] = dict(capture_hashes)
            # Capture bindings live in the run report, not in the strict actual-view
            # schema. Remove them before the view is consumed or revalidated.
            view_binding = canonical_view.pop("capture_hashes")
            views.append({**canonical_view, "artifact_capture": view_binding})
    document = {
        "schema_version": RUNTIME_RUN_SCHEMA_VERSION,
        "run_id": store.run_id,
        "partition": partition,
        "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "acquisition_file_sha256": sha256_file(acquisition_path.resolve(strict=True)),
        "engine_sha256": executable_sha256,
        "candidate_binding_sha256": candidate_binding_sha256,
        "adapter": {
            "main_class": CAUSE_ADAPTER_MAIN,
            "sbt_sha256": sha256_file(sbt),
            "runtime_observation_schema": RUNTIME_OBSERVATION_SCHEMA,
        },
        "case_count": len(views),
        "cache": {
            "contract": "cause-audit-runtime-probe-cache.v1",
            "probe_hit_count": probe_cache_hits,
            "probe_miss_count": probe_cache_misses,
        },
        "views": views,
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    if candidate_binding_path is not None:
        if _validate_candidate_binding(
            root=root,
            path=candidate_binding_path,
            document=None,
            manifest_path=manifest_path,
            cases_path=cases_path,
        ) != candidate_binding_sha256:
            raise IntegrityError("candidate binding changed during runtime execution")
    _assert_ascii(document, "cause audit runtime run")
    store.capture_run_document("cause-audit-runtime-run", document)
    store.verify()
    return document


def _strict_actual_view(
    root: Path,
    value: Mapping[str, Any],
    registry: SchemaRegistry,
    canonicalizer: Canonicalizer,
) -> dict[str, Any]:
    raw = _object(value, "actual cause cascade")
    raw.pop("artifact_capture", None)
    return _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(root, "actual-cascade-view"),
        document=raw,
        label=f"actual cause cascade {raw.get('case_id')}",
    )


def _camel_to_snake(value: str) -> str:
    return re.sub(r"(?<=[a-z0-9])(?=[A-Z])", "_", value).lower()


def _cause_parts(cause: Mapping[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    c = _object(cause.get("c"), "actual C cause")
    comparison = c.get("comparison")
    return c, dict(comparison) if isinstance(comparison, Mapping) else {}


def _cause_id(cause: Mapping[str, Any]) -> str:
    record = cause.get("cause_record")
    if not isinstance(record, Mapping) or not isinstance(record.get("id"), str):
        raise ContractError("actual cause has no cause record ID")
    return str(record["id"])


def _root_aligned(
    cause: Mapping[str, Any], *, played: str, references: set[str]
) -> bool:
    c, comparison = _cause_parts(cause)
    binding = c.get("binding")
    if not isinstance(binding, Mapping):
        return False
    role = binding.get("role")
    comparison_kind = comparison.get("kind")
    reference_root = comparison.get("reference_root_move")
    candidate_root = comparison.get("candidate_root_move")
    event_line = binding.get("event_line")
    event_root = event_line.get("root_move") if isinstance(event_line, Mapping) else None
    source_side = c.get("source_side")
    if source_side == "candidate":
        source_root_aligned = event_root == candidate_root
    elif source_side == "reference":
        source_root_aligned = event_root == reference_root
    else:
        source_root_aligned = event_root in {candidate_root, reference_root}
    if not source_root_aligned:
        return False
    if role == "primary_played_cause":
        return bool(
            comparison_kind == "played_vs_best"
            and candidate_root == played
            and reference_root in references
        )
    if role == "played_alternative_context":
        return bool(
            comparison_kind == "played_vs_alternative"
            and candidate_root == played
            and reference_root in references
        )
    if role == "candidate_set_constraint":
        return bool(
            comparison_kind == "best_vs_second"
            and reference_root in references
            and event_root == reference_root
        )
    return False


def _attribution_owned(
    cause: Mapping[str, Any], *, expected_source_side: str
) -> bool:
    c, comparison = _cause_parts(cause)
    attribution = c.get("attribution")
    binding = c.get("binding")
    if not isinstance(attribution, Mapping) or not isinstance(binding, Mapping):
        return False
    if comparison.get("mover") != expected_source_side:
        return False
    if attribution.get("root_move_matched") is not True:
        return False
    if attribution.get("direct_proof_eligible") is not True:
        return False
    source_side = c.get("source_side")
    attribution_kind = attribution.get("kind")
    cause_kind = c.get("kind")
    if cause_kind == "conversion_miss":
        return bool(
            source_side == "candidate"
            and attribution_kind == "candidate_allows_liability"
        )
    if cause_kind == "conversion_secured":
        return bool(
            (
                source_side == "reference"
                and attribution_kind == "reference_creates_resource"
            )
            or (
                source_side == "candidate"
                and attribution_kind == "candidate_creates_value"
            )
        )
    if source_side == "candidate":
        return attribution_kind in {
            "candidate_allows_liability",
            "candidate_creates_value",
        }
    if source_side == "reference":
        return attribution_kind == "reference_creates_resource"
    if source_side == "mixed":
        return attribution_kind == "candidate_allows_liability"
    return False


def _rank_indices(cause: Mapping[str, Any]) -> list[int]:
    rank = cause.get("r")
    if not isinstance(rank, Mapping) or not isinstance(rank.get("ranked"), list):
        return []
    return [
        int(item["rank_index"])
        for item in rank["ranked"]
        if isinstance(item, Mapping)
        and isinstance(item.get("rank_index"), int)
        and not isinstance(item.get("rank_index"), bool)
    ]


def _owned_object_bindings(cause: Mapping[str, Any]) -> list[Mapping[str, Any]]:
    c, _ = _cause_parts(cause)
    objects = c.get("objects")
    bindings = objects.get("owned_bindings") if isinstance(objects, Mapping) else None
    if not isinstance(bindings, list):
        return []
    return [item for item in bindings if isinstance(item, Mapping)]


def _binding_fields(binding: Mapping[str, Any]) -> set[str]:
    return {
        field
        for field in ("actor", "target", "mechanism", "consequence")
        if isinstance(binding.get(field), list) and bool(binding[field])
    }


def _has_complete_cause_tuple(cause: Mapping[str, Any]) -> bool:
    required = {"actor", "target", "mechanism", "consequence"}
    return any(_binding_fields(binding) == required for binding in _owned_object_bindings(cause))


def _closest_binding_fields(causes: Sequence[Mapping[str, Any]]) -> set[str]:
    fields = [
        _binding_fields(binding)
        for cause in causes
        for binding in _owned_object_bindings(cause)
    ]
    return max(fields, key=lambda item: (len(item), sorted(item)), default=set())


def _cascade_counts(causes: Sequence[Mapping[str, Any]]) -> dict[str, int]:
    jp = 0
    ja = 0
    ranked = 0
    packet_ids: set[str] = set()
    public_ids: set[str] = set()
    for cause in causes:
        jp_value = cause.get("jp")
        claims = jp_value.get("linked_claims") if isinstance(jp_value, Mapping) else None
        if isinstance(claims, list):
            jp += len(claims)
        ja_value = cause.get("ja")
        decisions = (
            ja_value.get("linked_decisions") if isinstance(ja_value, Mapping) else None
        )
        if isinstance(decisions, list):
            ja += sum(
                1
                for item in decisions
                if isinstance(item, Mapping)
                and item.get("status") in {"certified", "admitted"}
            )
        ranked += len(_rank_indices(cause))
        p_value = cause.get("p")
        if isinstance(p_value, Mapping):
            raw_packet = p_value.get("packet_linked_claim_ids")
            raw_public = p_value.get("selected_public_idea_claim_ids")
            if isinstance(raw_packet, list):
                packet_ids.update(str(item) for item in raw_packet)
            if isinstance(raw_public, list):
                public_ids.update(str(item) for item in raw_public)
    return {
        "c": len(causes),
        "jp": jp,
        "ja": ja,
        "r": ranked,
        "packet": len(packet_ids),
        "p": len(public_ids),
    }


def _publicly_selected(cause: Mapping[str, Any]) -> bool:
    p_value = cause.get("p")
    if not isinstance(p_value, Mapping):
        return False
    selected_ids = p_value.get("selected_public_idea_claim_ids")
    return isinstance(selected_ids, list) and bool(selected_ids)


def _judge_case(
    *,
    case: Mapping[str, Any],
    label: Mapping[str, Any],
    actual: Mapping[str, Any],
) -> dict[str, Any]:
    runtime_status = str(actual["status"])
    causes = actual.get("causes")
    if not isinstance(causes, list):
        raise ContractError("actual cause cascade has no causes")
    typed_causes = [dict(item) for item in causes if isinstance(item, Mapping)]
    oracle_hash = sha256_json(label)
    actual_hash = sha256_json(actual)
    probe_closure = actual.get("probe_closure")
    probes_closed = bool(
        isinstance(probe_closure, Mapping)
        and probe_closure.get("all_closed") is True
    )
    if runtime_status != "complete" or not probes_closed:
        return {
            "schema_version": JUDGMENT_SCHEMA_VERSION,
            "case_id": str(case["case_id"]),
            "partition": str(case["partition"]),
            "answerable": bool(label["answerable"]),
            "status": "runtime_unavailable",
            "oracle_label_sha256": oracle_hash,
            "actual_view_sha256": actual_hash,
            "runtime_status": runtime_status,
            "cause_match": {
                "expected_kind_count": len(label["expected_cause_kinds"]),
                "root_aligned_cause_count": 0,
                "matched_cause_count": 0,
                "matched_cause_record_ids": [],
                "forbidden_cause_count": 0,
                "competing_ranked_cause_count": 0,
            },
            "cascade": {"c": 0, "jp": 0, "ja": 0, "r": 0, "packet": 0, "p": 0},
            "errors": ["runtime_unavailable"],
            "first_failure_stage": "C",
        }
    if not bool(label["answerable"]):
        return {
            "schema_version": JUDGMENT_SCHEMA_VERSION,
            "case_id": str(case["case_id"]),
            "partition": str(case["partition"]),
            "answerable": False,
            "status": "unanswerable",
            "oracle_label_sha256": oracle_hash,
            "actual_view_sha256": actual_hash,
            "runtime_status": runtime_status,
            "cause_match": {
                "expected_kind_count": len(label["expected_cause_kinds"]),
                "root_aligned_cause_count": 0,
                "matched_cause_count": 0,
                "matched_cause_record_ids": [],
                "forbidden_cause_count": 0,
                "competing_ranked_cause_count": 0,
            },
            "cascade": {"c": 0, "jp": 0, "ja": 0, "r": 0, "packet": 0, "p": 0},
            "errors": [],
            "first_failure_stage": None,
        }
    expected_kinds = {
        _camel_to_snake(str(item)) for item in label["expected_cause_kinds"]
    }
    forbidden_kinds = {
        _camel_to_snake(str(item)) for item in label["forbidden_tempting_causes"]
    }
    references = {
        str(label["expected_reference_move_uci"]),
        *[str(item) for item in label["acceptable_alternatives"]],
    }
    played = str(case["played_move_uci"])
    root_aligned = [
        cause
        for cause in typed_causes
        if _root_aligned(cause, played=played, references=references)
    ]
    kind_candidates = [
        cause
        for cause in typed_causes
        if _cause_parts(cause)[0].get("kind") in expected_kinds
    ]
    kind_and_root = [
        cause
        for cause in root_aligned
        if _cause_parts(cause)[0].get("kind") in expected_kinds
    ]
    attribution_owned = [
        cause
        for cause in kind_and_root
        if _attribution_owned(cause, expected_source_side=str(label["source_side"]))
    ]
    matched = [cause for cause in attribution_owned if _has_complete_cause_tuple(cause)]
    forbidden = [
        cause
        for cause in root_aligned
        if _cause_parts(cause)[0].get("kind") in forbidden_kinds
    ]
    competing = [cause for cause in root_aligned if cause not in matched]
    errors: set[str] = set()
    if not matched:
        if attribution_owned:
            closest_fields = _closest_binding_fields(attribution_owned)
            for field, error in (
                ("actor", "missing_actor_binding"),
                ("target", "missing_target_binding"),
                ("mechanism", "missing_mechanism_binding"),
                ("consequence", "missing_consequence_binding"),
            ):
                if field not in closest_fields:
                    errors.add(error)
        elif kind_and_root:
            errors.add("wrong_attribution")
        elif kind_candidates:
            errors.add("wrong_cause_source")
        elif root_aligned:
            errors.add("wrong_cause_kind")
        else:
            errors.add("missing_cause")
    if forbidden:
        errors.add("forbidden_cause_emitted")
        if not matched and any(_publicly_selected(cause) for cause in forbidden):
            errors.add("generic_fallback_takeover")
    matched_ranks = [rank for cause in matched for rank in _rank_indices(cause)]
    competing_ranks = [
        rank
        for cause in competing
        if cause in forbidden
        or _cause_parts(cause)[0].get("kind") not in expected_kinds
        for rank in _rank_indices(cause)
    ]
    if competing_ranks and (not matched_ranks or min(competing_ranks) < min(matched_ranks)):
        errors.add("priority_inversion")
    cascade = _cascade_counts(matched)
    first_failure: str | None = None
    if not matched:
        first_failure = "C"
    elif cascade["jp"] == 0:
        errors.add("jp_loss")
        first_failure = "Jp"
    elif cascade["ja"] == 0:
        errors.add("ja_loss")
        first_failure = "Ja"
    elif cascade["r"] == 0:
        errors.add("r_loss")
        first_failure = "R"
    elif cascade["packet"] == 0:
        errors.add("packet_loss")
        first_failure = "P"
    elif cascade["p"] == 0:
        errors.add("p_loss")
        first_failure = "P"
    return {
        "schema_version": JUDGMENT_SCHEMA_VERSION,
        "case_id": str(case["case_id"]),
        "partition": str(case["partition"]),
        "answerable": True,
        "status": "mismatch" if errors else "matched",
        "oracle_label_sha256": oracle_hash,
        "actual_view_sha256": actual_hash,
        "runtime_status": runtime_status,
        "cause_match": {
            "expected_kind_count": len(expected_kinds),
            "root_aligned_cause_count": len(root_aligned),
            "matched_cause_count": len(matched),
            "matched_cause_record_ids": sorted(_cause_id(item) for item in matched),
            "forbidden_cause_count": len(forbidden),
            "competing_ranked_cause_count": len(competing_ranks),
        },
        "cascade": cascade,
        "errors": sorted(errors),
        "first_failure_stage": first_failure,
    }


def compare_cause_audit(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    labels_path: Path,
    runtime_run_path: Path,
    partition: str,
    requested_case_ids: Sequence[str],
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root)
    manifest, cases = _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    labels = _verify_bound_labels(
        root=root,
        path=labels_path,
        manifest=manifest,
        cases=cases,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    selected = _selected_cases(cases, manifest, partition, requested_case_ids)
    selected_ids = {str(item["case_id"]) for item in selected}
    case_by_id = {str(item["case_id"]): item for item in selected}
    label_by_id = {
        str(item["case_id"]): item
        for item in labels
        if str(item["case_id"]) in selected_ids
    }
    runtime_run = _read_object(runtime_run_path, "cause audit runtime run")
    if runtime_run.get("schema_version") != RUNTIME_RUN_SCHEMA_VERSION:
        raise ContractError("unsupported cause audit runtime run schema")
    if runtime_run.get("freeze_manifest_sha256") != sha256_file(
        manifest_path.resolve(strict=True)
    ):
        raise IntegrityError("runtime run freeze manifest binding failed")
    if runtime_run.get("cases_file_sha256") != sha256_file(
        cases_path.resolve(strict=True)
    ):
        raise IntegrityError("runtime run cases binding failed")
    raw_views = runtime_run.get("views")
    if not isinstance(raw_views, list):
        raise ContractError("cause audit runtime run has no views")
    views: dict[str, dict[str, Any]] = {}
    for raw in raw_views:
        if not isinstance(raw, Mapping):
            raise ContractError("cause audit runtime run contains a non-object view")
        view = _strict_actual_view(root, raw, registry, canonicalizer)
        case_id = str(view["case_id"])
        if case_id in views:
            raise ContractError(f"duplicate actual cause cascade: {case_id}")
        views[case_id] = view
    if not selected_ids.issubset(views):
        raise ContractError(
            f"runtime run is missing selected cases: {sorted(selected_ids - set(views))[:5]}"
        )
    judgments: list[dict[str, Any]] = []
    for case_id in sorted(selected_ids):
        judgment = _judge_case(
            case=case_by_id[case_id],
            label=label_by_id[case_id],
            actual=views[case_id],
        )
        canonical_judgment = _validate_and_canonicalize(
            registry=registry,
            canonicalizer=canonicalizer,
            schema_path=_schema_path(root, "judgment"),
            document=judgment,
            label=f"cause audit judgment {case_id}",
        )
        store.capture_run_document(
            f"cause-audit-judgment-{case_id}", canonical_judgment
        )
        judgments.append(canonical_judgment)
    status_counts = Counter(str(item["status"]) for item in judgments)
    runtime_counts = Counter(str(item["runtime_status"]) for item in judgments)
    error_counts = Counter(
        str(error) for item in judgments for error in item["errors"]
    )
    failure_counts = Counter(
        "none" if item["first_failure_stage"] is None else str(item["first_failure_stage"])
        for item in judgments
    )
    cause_kind_counts = Counter()
    for case_id in selected_ids:
        for cause in views[case_id]["causes"]:
            if isinstance(cause, Mapping):
                c = cause.get("c")
                if isinstance(c, Mapping) and isinstance(c.get("kind"), str):
                    cause_kind_counts[str(c["kind"])] += 1
    report = {
        "schema_version": REPORT_SCHEMA_VERSION,
        "run_id": store.run_id,
        "mode": "blind-cause-backend-audit",
        "partition": partition,
        "bindings": {
            "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
            "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
            "oracle_file_sha256": sha256_file(labels_path.resolve(strict=True)),
            "runtime_run_sha256": sha256_file(runtime_run_path.resolve(strict=True)),
        },
        "evaluation_contract": {
            "primary_label_rule": "match-any-allowed-kind-on-played-vs-acceptable-reference",
            "extra_cause_rule": "unlisted-extra-causes-are-not-false-unless-oracle-forbids-or-priority-inverts",
            "object_rule": "actor-target-mechanism-consequence-must-coexist-in-one-owned-direct-binding",
            "source_rule": "comparison-role-event-root-source-side-and-attribution-polarity-must-agree",
            "oracle_prose_semantic_match": "not-performed-requires-blind-human-proxy-adjudication",
            "sealed_label_disclosure": "hash-and-verdict-only",
        },
        "counts": {
            "cases": len(judgments),
            "answerable": sum(bool(item["answerable"]) for item in judgments),
            "matched": status_counts["matched"],
            "mismatch": status_counts["mismatch"],
            "unanswerable": status_counts["unanswerable"],
            "runtime_unavailable": status_counts["runtime_unavailable"],
        },
        "runtime_status_counts": dict(sorted(runtime_counts.items())),
        "cause_kind_counts": dict(sorted(cause_kind_counts.items())),
        "error_counts": dict(sorted(error_counts.items())),
        "first_failure_counts": dict(sorted(failure_counts.items())),
        "judgments": judgments,
        "policy": {
            "machine_language": "en",
            "human_proxy_mode": "codex-plus-stockfish",
            "release_eligible": False,
            "production_bottleneck_claim": "engineering-diagnostic",
            "test_files_added": False,
            "production_modules_duplicated": False,
        },
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    canonical_report = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(root, "report"),
        document=report,
        label="cause audit report",
    )
    store.capture_run_document("cause-audit-report", canonical_report)
    store.verify()
    return canonical_report


def execute_cause_audit_action(
    *, root: Path, store: ArtifactStore, arguments: Any
) -> dict[str, Any]:
    action = str(arguments.action)
    cases = arguments.cases
    if cases is None:
        raise ContractError("cause-audit requires --cases")
    if action == "freeze":
        return freeze_cause_audit(
            root=root,
            store=store,
            cases_path=cases,
            oracle_specs=arguments.oracle_label,
            adjudicated_path=arguments.adjudicated_label,
            corpus_id=arguments.corpus_id,
            contamination_exclusion_specs=arguments.contamination_exclusion,
        )
    manifest = arguments.manifest
    if manifest is None:
        raise ContractError(f"cause-audit {action} requires --manifest")
    if action == "acquire":
        if arguments.stockfish is None:
            raise ContractError("cause-audit acquire requires --stockfish")
        return acquire_cause_audit(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            reference_labels_path=arguments.reference_labels,
            stockfish_path=arguments.stockfish,
            cache_root=arguments.cache or (root / "tmp" / "cause-audit-cache-v1"),
            partition=arguments.partition,
            arguments=arguments,
        )
    if action == "bind-candidate":
        if arguments.candidate_id is None:
            raise ContractError("bind-candidate requires --candidate-id")
        return freeze_candidate_binding(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            candidate_id=arguments.candidate_id,
            component_paths=arguments.component,
        )
    if action == "run":
        for name in ("acquisition", "stockfish", "sbt"):
            if getattr(arguments, name) is None:
                raise ContractError(f"cause-audit run requires --{name}")
        return run_cause_audit(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            acquisition_path=arguments.acquisition,
            stockfish_path=arguments.stockfish,
            sbt_path=arguments.sbt,
            adapter_root=arguments.adapter_root or (root / "runtime-adapter"),
            cache_root=arguments.cache or (root / "tmp" / "cause-audit-cache-v1"),
            partition=arguments.partition,
            timeout_seconds=float(arguments.timeout_seconds),
            provider_timeout_seconds=float(arguments.provider_timeout_seconds),
            max_probe_rounds=int(arguments.max_probe_rounds),
            candidate_binding_path=arguments.candidate_binding,
            requested_case_ids=arguments.case_id,
        )
    if action == "compare":
        if arguments.labels is None or arguments.runtime_run is None:
            raise ContractError(
                "cause-audit compare requires --labels and --runtime-run"
            )
        return compare_cause_audit(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            labels_path=arguments.labels,
            runtime_run_path=arguments.runtime_run,
            partition=arguments.partition,
            requested_case_ids=arguments.case_id,
        )
    raise ContractError(f"unsupported cause-audit action: {action}")


__all__ = [
    "execute_cause_audit_action",
    "freeze_cause_audit",
    "acquire_cause_audit",
    "run_cause_audit",
    "compare_cause_audit",
    "freeze_candidate_binding",
]
