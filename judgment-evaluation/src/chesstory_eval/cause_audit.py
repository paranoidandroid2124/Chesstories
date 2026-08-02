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
from .cause_semantics import (
    canonical_open_world_cause_candidate,
    canonical_open_world_cause_observation,
    cause_matches_typed_oracle,
    comparison_endpoint_snapshot_index,
    judge_typed_case,
    open_world_meaning_matches_candidate,
    validate_typed_oracle_label,
)
from .hashing import (
    json_bytes,
    read_json,
    read_jsonl,
    sha256_bytes,
    sha256_file,
    sha256_json,
)
from .model import ContractError, IntegrityError
from .native_diagnostic import NATIVE_HASH_CONTRACT
from .probe_completion import (
    MAX_RUNTIME_PROBE_RESULTS,
    _RuntimeJsonlSession,
    _search_probe,
    _validate_probe_request,
    _verified_response_jsonl_bytes,
)
from .schemas import SchemaRegistry
from .stockfish import acquire_stockfish_evidence


FREEZE_SCHEMA_VERSION = "chesstory.eval.cause-audit-freeze.v1"
ACQUISITION_SCHEMA_VERSION = "chesstory.eval.cause-audit-acquisition.v1"
RUNTIME_RUN_SCHEMA_VERSION = "chesstory.eval.cause-audit-runtime-run.v1"
ACTUAL_VIEW_SCHEMA_VERSION = "chesstory.eval.cause-audit-actual-cascade.v2"
JUDGMENT_SCHEMA_VERSION = "chesstory.eval.cause-audit-judgment.v2"
REPORT_SCHEMA_VERSION = "chesstory.eval.cause-audit-report.v2"
TYPED_ACTUAL_VIEW_SCHEMA_VERSION = "chesstory.eval.cause-audit-actual-cascade.v3"
TYPED_JUDGMENT_SCHEMA_VERSION = "chesstory.eval.cause-audit-judgment.v3"
TYPED_REPORT_SCHEMA_VERSION = "chesstory.eval.cause-audit-report.v3"
TYPED_ORACLE_SCHEMA_VERSION = "chesstory.eval.cause-audit-oracle.v3"
TYPED_ORACLE_RUN_SCHEMA_VERSION = "chesstory.eval.cause-audit-oracle-run.v3"
TYPED_OPEN_WORLD_CANDIDATE_SCHEMA_VERSION = (
    "chesstory.eval.cause-audit-open-world-candidates.v3"
)
TYPED_OPEN_WORLD_ADJUDICATION_SCHEMA_VERSION = (
    "chesstory.eval.cause-audit-open-world-adjudication.v3"
)
TYPED_OPEN_WORLD_ARM_INVENTORY_SCHEMA_VERSION = (
    "chesstory.eval.cause-audit-open-world-arm-inventory.v3"
)
RUNTIME_REQUEST_SCHEMA = "chesstory.move-meaning.request.v1"
RUNTIME_OBSERVATION_SCHEMA = "chesstory.cause-audit-runtime-observation.v2"
TYPED_RUNTIME_OBSERVATION_SCHEMA = "chesstory.cause-audit-runtime-observation.v3"
HISTORICAL_ORACLE_SCHEMA_VERSION = "cause-audit-curation-v1"
RUNTIME_RESPONSE_SCHEMA = "chesstory.move-meaning.response.v3"
CAUSE_ADAPTER_NAME = "chesstory-cause-audit-runtime-adapter"
CAUSE_ADAPTER_MAIN = (
    "io.chesstory.evaluation.runtimeadapter.CauseAuditAdapterCli"
)
ACTIVE_CAUSE_ADAPTER_ARGUMENT = "--observation-schema-version=3"
HISTORICAL_CAUSE_ADAPTER_ARGUMENT = "--historical-observation-schema-version=2"
PARTITIONS = ("explore", "sealed_confirm")
SPLIT_SALT = "cause-audit-v1-family-seal"
OBJECT_TUPLE_ASSESSMENT = "structural-completeness-only"
ATTRIBUTION_SEMANTIC_ASSESSMENT = "not-performed-no-typed-oracle-polarity"
TYPED_SEALED_PROJECTION_BLOCK = (
    "typed sealed comparison is disabled until judgments and reports use a "
    "redacted projection that cannot retain meaning_id"
)


def _schema_series(schema_version: str | None) -> str:
    if schema_version is None or schema_version.endswith(".v1"):
        return "cause-audit-v1"
    if schema_version.endswith(".v2"):
        return "cause-audit-v2"
    if schema_version.endswith(".v3"):
        return "cause-audit-v3"
    raise ContractError(f"unsupported cause audit schema version: {schema_version}")


def _cause_adapter_command(sbt: Path, actual_schema_version: str) -> list[str]:
    if actual_schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION:
        adapter_argument = ACTIVE_CAUSE_ADAPTER_ARGUMENT
    elif actual_schema_version == ACTUAL_VIEW_SCHEMA_VERSION:
        adapter_argument = HISTORICAL_CAUSE_ADAPTER_ARGUMENT
    else:
        raise ContractError(
            f"unsupported cause audit run contract: {actual_schema_version}"
        )
    return [
        str(sbt),
        "-batch",
        "-error",
        f"runMain {CAUSE_ADAPTER_MAIN} {adapter_argument}",
    ]


def _schema_path(
    root: Path, name: str, schema_version: str | None = None
) -> Path:
    return root / "schemas" / _schema_series(schema_version) / f"{name}.schema.json"


def _schema_tools(
    root: Path, schema_version: str | None = None
) -> tuple[SchemaRegistry, Canonicalizer]:
    registry = SchemaRegistry(root / "schemas" / _schema_series(schema_version))
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


def _peek_label_schema_version(path: Path) -> str:
    resolved = path.resolve(strict=True)
    try:
        with resolved.open("r", encoding="utf-8") as handle:
            for raw_line in handle:
                if not raw_line.strip():
                    continue
                value = json.loads(raw_line)
                if not isinstance(value, Mapping) or not isinstance(
                    value.get("schema_version"), str
                ):
                    raise ContractError("oracle label first row has no schema_version")
                return str(value["schema_version"])
    except (OSError, json.JSONDecodeError) as error:
        raise ContractError(f"cannot inspect oracle labels: {error}") from error
    raise ContractError("oracle label file is empty")


def _load_typed_labels(
    *,
    root: Path,
    path: Path,
    partition_cases: Sequence[Mapping[str, Any]],
    partition: str,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    if partition not in PARTITIONS:
        raise ContractError("typed oracle loading requires one physical partition")
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    try:
        raw_rows = read_jsonl(path.resolve(strict=True))
    except ValueError as error:
        raise ContractError(str(error)) from error
    case_by_id = {str(item["case_id"]): item for item in partition_cases}
    rows: list[dict[str, Any]] = []
    canonical: list[dict[str, Any]] = []
    seen: set[str] = set()
    schema_path = _schema_path(
        root, "oracle-label", TYPED_ORACLE_SCHEMA_VERSION
    )
    for index, raw in enumerate(raw_rows, 1):
        row = _object(raw, f"typed oracle label row {index}")
        if row.get("schema_version") != TYPED_ORACLE_SCHEMA_VERSION:
            raise ContractError(
                f"typed oracle row {index} has a mixed schema version"
            )
        if row.get("partition") != partition:
            raise IntegrityError(
                "typed oracle shard contains a row from another partition"
            )
        case_id = str(row.get("case_id", ""))
        if case_id in seen:
            raise ContractError(f"duplicate typed oracle case_id: {case_id}")
        case = case_by_id.get(case_id)
        if case is None:
            raise IntegrityError(
                "typed oracle shard contains a case outside its frozen partition"
            )
        item = _validate_and_canonicalize(
            registry=registry,
            canonicalizer=canonicalizer,
            schema_path=schema_path,
            document=row,
            label=f"typed oracle label row {index}",
        )
        validate_typed_oracle_label(item, case)
        _assert_ascii(item, f"typed oracle label {case_id}")
        rows.append(item)
        canonical.append(item)
        seen.add(case_id)
    if not rows:
        raise ContractError("typed oracle shard has no labels")
    if seen != set(case_by_id):
        missing = sorted(set(case_by_id) - seen)
        raise IntegrityError(
            f"typed oracle shard does not cover its frozen partition: {missing[:5]}"
        )
    rows.sort(key=lambda item: str(item["case_id"]))
    canonical.sort(key=lambda item: str(item["case_id"]))
    return rows, canonical


def _canonical_base_oracle_artifact(
    *,
    partition: str,
    labels: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    return {
        "partition": partition,
        "case_count": len(labels),
        "case_ids_sha256": _ids_hash(labels),
        "labels_semantic_sha256": sha256_json(labels),
        "labels": [_object(item, "canonical base-oracle label") for item in labels],
    }


def _typed_base_oracle_binding(
    *,
    base_oracle_path: Path,
    partition: str,
    labels: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    artifact = _canonical_base_oracle_artifact(
        partition=partition,
        labels=labels,
    )
    return {
        "source_file_sha256": sha256_file(base_oracle_path.resolve(strict=True)),
        "canonical_artifact_sha256": sha256_json(artifact),
        "labels_semantic_sha256": sha256_json(labels),
        "case_ids_sha256": _ids_hash(labels),
        "case_count": len(labels),
    }


def _verify_typed_oracle_run_manifest(
    *,
    root: Path,
    path: Path,
    base_oracle_path: Path,
    labels_path: Path,
    arm_inventory_path: Path,
    candidate_set_path: Path,
    adjudication_path: Path,
    labels: Sequence[Mapping[str, Any]],
    partition_cases: Sequence[Mapping[str, Any]],
    manifest_path: Path,
    cases_path: Path,
    partition: str,
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_RUN_SCHEMA_VERSION)
    raw = _read_object(path, "typed oracle run manifest")
    value = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "oracle-run-manifest", TYPED_ORACLE_RUN_SCHEMA_VERSION
        ),
        document=raw,
        label="typed oracle run manifest",
    )
    if value.get("partition") != partition:
        raise IntegrityError("typed oracle run manifest partition mismatch")
    if value.get("freeze_manifest_sha256") != sha256_file(
        manifest_path.resolve(strict=True)
    ):
        raise IntegrityError("typed oracle run manifest freeze binding failed")
    if value.get("cases_file_sha256") != sha256_file(cases_path.resolve(strict=True)):
        raise IntegrityError("typed oracle run manifest cases binding failed")
    if value.get("run_oracle_file_sha256") != sha256_file(
        labels_path.resolve(strict=True)
    ):
        raise IntegrityError("typed oracle run manifest label binding failed")
    if value.get("base_oracle_file_sha256") != sha256_file(
        base_oracle_path.resolve(strict=True)
    ):
        raise IntegrityError("typed oracle run manifest base-oracle binding failed")
    if value.get("case_ids_sha256") != _ids_hash(labels):
        raise IntegrityError("typed oracle run manifest case-ID binding failed")
    open_world = _object(value.get("open_world"), "typed oracle open-world state")
    if open_world.get("candidate_set_sha256") != sha256_file(
        candidate_set_path.resolve(strict=True)
    ):
        raise IntegrityError("typed oracle candidate-set binding failed")
    if open_world.get("adjudication_sha256") != sha256_file(
        adjudication_path.resolve(strict=True)
    ):
        raise IntegrityError("typed oracle adjudication binding failed")
    if open_world.get("status") != "frozen" or open_world.get("pending_count") != 0:
        raise ContractError(
            "typed oracle comparison requires a frozen open-world graph with no pending candidates"
        )
    if open_world.get("source_blind") is not True:
        raise ContractError("typed oracle open-world adjudication was not source blind")
    if open_world.get("statistics_unblinded") is not False:
        raise ContractError("typed oracle was frozen after statistical unblinding")

    base_labels, _ = _load_typed_labels(
        root=root,
        path=base_oracle_path,
        partition_cases=partition_cases,
        partition=partition,
    )
    arm_inventory = _load_open_world_arm_inventory(
        root=root,
        path=arm_inventory_path,
        base_oracle_path=base_oracle_path,
        manifest_path=manifest_path,
        cases_path=cases_path,
        labels=base_labels,
        partition=partition,
    )
    candidate_set = _load_open_world_candidate_set(
        root=root,
        path=candidate_set_path,
        base_oracle_path=base_oracle_path,
        arm_inventory_path=arm_inventory_path,
        arm_inventory=arm_inventory,
        labels=base_labels,
        partition=partition,
    )
    adjudication = _load_open_world_adjudication(
        root=root,
        path=adjudication_path,
        candidate_set_path=candidate_set_path,
        candidate_set=candidate_set,
        partition=partition,
    )
    accepted = [
        item
        for item in adjudication["decisions"]
        if item["decision"] == "accepted"
    ]
    merged = _merge_open_world_labels(
        root=root,
        base_labels=base_labels,
        accepted_decisions=accepted,
        case_judgments=list(adjudication["case_judgments"]),
        candidate_set=candidate_set,
        partition_cases=partition_cases,
    )
    canonical_run = sorted(
        (_object(item, "typed run-oracle label") for item in labels),
        key=lambda item: str(item["case_id"]),
    )
    if merged != canonical_run:
        raise IntegrityError(
            "typed run oracle is not the exact base-plus-accepted all-arm merge"
        )

    expected_open_world = _typed_open_world_manifest_state(
        base_labels=base_labels,
        arm_inventory_path=arm_inventory_path,
        arm_inventory=arm_inventory,
        candidate_set_path=candidate_set_path,
        candidate_set=candidate_set,
        adjudication_path=adjudication_path,
        adjudication=adjudication,
        merged_labels=merged,
    )
    for field, expected in expected_open_world.items():
        if open_world.get(field) != expected:
            raise IntegrityError(
                f"typed oracle open-world {field} binding failed"
            )
    if open_world.get("pending_count") != 0:
        raise ContractError("typed open-world adjudication has pending candidates")
    return {
        "manifest": value,
        "base_labels": base_labels,
        "candidate_set": candidate_set,
        "adjudication": adjudication,
        "arm_inventory": arm_inventory,
    }


def _arm_ids_hash(inventory: Mapping[str, Any]) -> str:
    return sha256_json(sorted(str(item["arm_id"]) for item in inventory["arms"]))


def _candidate_row(case_id: str, payload: Mapping[str, Any]) -> dict[str, Any]:
    canonical_payload = _object(payload, "open-world candidate payload")
    c_semantics_sha256 = sha256_json(canonical_payload["c_semantics"])
    return {
        "candidate_id": "ow:"
        + sha256_json(
            {
                "case_id": case_id,
                "c_semantics_sha256": c_semantics_sha256,
            }
        ),
        "case_id": case_id,
        "c_semantics_sha256": c_semantics_sha256,
        "payload_sha256": sha256_json(canonical_payload),
        "payload": canonical_payload,
    }


def _merge_candidate_payloads(
    payloads: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    if not payloads:
        raise ContractError("cannot merge an empty open-world observation set")
    semantics = [
        _object(item["c_semantics"], "open-world observation C semantics")
        for item in payloads
    ]
    if any(item != semantics[0] for item in semantics[1:]):
        raise IntegrityError("open-world observations merge different C semantics")
    stages = sorted(
        {
            str(stage)
            for payload in payloads
            for stage in payload["observed_stages"]
        }
    )
    variants = {
        json_bytes(variant): _object(variant, "open-world selection variant")
        for payload in payloads
        for variant in payload["selection_variants"]
    }
    merged = {
        "observed_stages": stages,
        "c_semantics": semantics[0],
        "selection_variants": [variants[key] for key in sorted(variants)],
    }
    _validate_open_world_candidate_stages(merged)
    return merged


def _inventory_expected_candidates(
    inventory: Mapping[str, Any],
) -> list[dict[str, Any]]:
    by_key: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for arm in inventory["arms"]:
        for cascade in arm["actual_cascades"]:
            case_id = str(cascade["case_id"])
            for observation in cascade["unmatched_cause_observations"]:
                key = (case_id, str(observation["c_semantics_sha256"]))
                by_key.setdefault(key, []).append(
                    _object(observation["payload"], "arm Cause observation payload")
                )
    candidates = [
        _candidate_row(
            case_id,
            {"c_semantics": _merge_candidate_payloads(payloads)["c_semantics"]},
        )
        for (case_id, _), payloads in by_key.items()
    ]
    return sorted(candidates, key=json_bytes)


def _open_world_candidate_set_document(
    *,
    root: Path,
    partition: str,
    base_oracle_path: Path,
    base_labels: Sequence[Mapping[str, Any]],
    arm_inventory_path: Path,
    arm_inventory: Mapping[str, Any],
) -> dict[str, Any]:
    document = {
        "schema_version": TYPED_OPEN_WORLD_CANDIDATE_SCHEMA_VERSION,
        "partition": partition,
        "arm_scope": "all_arms",
        "arm_inventory_sha256": sha256_file(
            arm_inventory_path.resolve(strict=True)
        ),
        "arm_inventory_semantic_sha256": sha256_json(arm_inventory),
        "arm_ids_sha256": _arm_ids_hash(arm_inventory),
        "base_oracle_file_sha256": sha256_file(
            base_oracle_path.resolve(strict=True)
        ),
        "case_ids_sha256": _ids_hash(base_labels),
        "candidates": _inventory_expected_candidates(arm_inventory),
    }
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    return _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "open-world-candidate-set", TYPED_ORACLE_SCHEMA_VERSION
        ),
        document=document,
        label="typed open-world candidate set",
    )


def _load_open_world_arm_inventory(
    *,
    root: Path,
    path: Path,
    base_oracle_path: Path,
    manifest_path: Path,
    cases_path: Path,
    labels: Sequence[Mapping[str, Any]],
    partition: str,
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    value = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "open-world-arm-inventory", TYPED_ORACLE_SCHEMA_VERSION
        ),
        document=_read_object(path, "typed open-world arm inventory"),
        label="typed open-world arm inventory",
    )
    if value.get("schema_version") != TYPED_OPEN_WORLD_ARM_INVENTORY_SCHEMA_VERSION:
        raise ContractError("unsupported typed open-world arm inventory schema")
    expected_bindings = {
        "partition": partition,
        "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "base_oracle_file_sha256": sha256_file(base_oracle_path.resolve(strict=True)),
        "case_ids_sha256": _ids_hash(labels),
    }
    for field, expected in expected_bindings.items():
        if value.get(field) != expected:
            raise IntegrityError(f"typed open-world arm inventory {field} mismatch")

    known_cases = {str(item["case_id"]) for item in labels}
    arm_ids: set[str] = set()
    runtime_hashes: set[str] = set()
    for arm in value["arms"]:
        arm_id = str(arm["arm_id"])
        runtime_hash = str(arm["runtime_run_file_sha256"])
        if arm_id in arm_ids or runtime_hash in runtime_hashes:
            raise ContractError("typed open-world arm inventory has duplicate arms")
        producer = _object(arm["producer_contract"], "arm producer contract")
        if arm["producer_contract_sha256"] != sha256_json(producer):
            raise IntegrityError("typed open-world arm producer hash mismatch")
        cascades = list(arm["actual_cascades"])
        cascade_ids = [str(item["case_id"]) for item in cascades]
        if len(cascade_ids) != len(set(cascade_ids)):
            raise ContractError("typed open-world arm repeats a case cascade")
        if set(cascade_ids) != known_cases:
            raise IntegrityError(
                "each typed open-world arm must cover the exact frozen case set"
            )
        if (
            arm["case_ids_sha256"] != value["case_ids_sha256"]
            or arm["case_ids_sha256"] != sha256_json(sorted(cascade_ids))
        ):
            raise IntegrityError("typed open-world arm case-ID hash mismatch")
        if arm["actual_cascades_sha256"] != sha256_json(cascades):
            raise IntegrityError("typed open-world arm cascade-set hash mismatch")
        for cascade in cascades:
            observations = list(cascade["unmatched_cause_observations"])
            if cascade["observation_count"] != len(observations):
                raise IntegrityError("typed open-world cascade observation count mismatch")
            semantic_ids: set[str] = set()
            for observation in observations:
                payload = _object(observation["payload"], "arm Cause observation")
                c_hash = sha256_json(payload["c_semantics"])
                if observation["c_semantics_sha256"] != c_hash:
                    raise IntegrityError("arm Cause observation C-semantics hash mismatch")
                if observation["payload_sha256"] != sha256_json(payload):
                    raise IntegrityError("arm Cause observation payload hash mismatch")
                if c_hash in semantic_ids:
                    raise ContractError(
                        "one arm cascade splits identical C semantics into duplicates"
                    )
                _validate_open_world_candidate_stages(payload)
                semantic_ids.add(c_hash)
        arm_ids.add(arm_id)
        runtime_hashes.add(runtime_hash)
    return value


def _load_open_world_candidate_set(
    *,
    root: Path,
    path: Path,
    base_oracle_path: Path,
    arm_inventory_path: Path,
    arm_inventory: Mapping[str, Any],
    labels: Sequence[Mapping[str, Any]],
    partition: str,
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    value = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "open-world-candidate-set", TYPED_ORACLE_SCHEMA_VERSION
        ),
        document=_read_object(path, "typed open-world candidate set"),
        label="typed open-world candidate set",
    )
    if value.get("schema_version") != TYPED_OPEN_WORLD_CANDIDATE_SCHEMA_VERSION:
        raise ContractError("unsupported typed open-world candidate schema")
    if value.get("partition") != partition:
        raise IntegrityError("typed open-world candidate partition mismatch")
    if value.get("arm_scope") != "all_arms":
        raise ContractError("typed open-world candidates must cover all arms")
    expected_inventory_bindings = {
        "arm_inventory_sha256": sha256_file(
            arm_inventory_path.resolve(strict=True)
        ),
        "arm_inventory_semantic_sha256": sha256_json(arm_inventory),
        "arm_ids_sha256": _arm_ids_hash(arm_inventory),
    }
    for field, expected in expected_inventory_bindings.items():
        if value.get(field) != expected:
            raise IntegrityError(
                f"typed open-world candidate {field} binding failed"
            )
    if value.get("base_oracle_file_sha256") != sha256_file(
        base_oracle_path.resolve(strict=True)
    ):
        raise IntegrityError("typed open-world candidate base-oracle binding failed")
    if value.get("case_ids_sha256") != _ids_hash(labels):
        raise IntegrityError("typed open-world candidate case-ID binding failed")

    label_ids = {str(item["case_id"]) for item in labels}
    candidate_ids: set[str] = set()
    semantic_keys: set[tuple[str, str]] = set()
    for candidate in value["candidates"]:
        candidate_id = str(candidate["candidate_id"])
        case_id = str(candidate["case_id"])
        payload = _object(candidate["payload"], "typed open-world candidate payload")
        c_semantics = _object(
            payload["c_semantics"], "typed open-world candidate C semantics"
        )
        if case_id not in label_ids:
            raise IntegrityError(
                "typed open-world candidate references a case outside its partition"
            )
        c_semantics_sha256 = sha256_json(c_semantics)
        expected_id = "ow:" + sha256_json(
            {"case_id": case_id, "c_semantics_sha256": c_semantics_sha256}
        )
        if candidate["c_semantics_sha256"] != c_semantics_sha256:
            raise IntegrityError("typed open-world candidate C-semantics hash mismatch")
        if candidate["payload_sha256"] != sha256_json(payload):
            raise IntegrityError("typed open-world candidate payload hash mismatch")
        if candidate_id != expected_id:
            raise IntegrityError("typed open-world candidate identity mismatch")
        if candidate_id in candidate_ids:
            raise ContractError("duplicate typed open-world candidate_id")
        semantic_key = (case_id, c_semantics_sha256)
        if semantic_key in semantic_keys:
            raise ContractError(
                "typed open-world C semantics were split into multiple candidates"
            )
        candidate_ids.add(candidate_id)
        semantic_keys.add(semantic_key)
    if list(value["candidates"]) != _inventory_expected_candidates(arm_inventory):
        raise IntegrityError(
            "typed open-world candidates are not the exact all-arm unmatched-C union"
        )
    return value


def _validate_open_world_candidate_stages(payload: Mapping[str, Any]) -> None:
    stages = set(str(item) for item in payload["observed_stages"])
    variants = list(payload["selection_variants"])
    if "c_generated" not in stages:
        raise ContractError("typed open-world candidate was not observed at C")
    if ("r_selected" in stages) != bool(variants):
        raise ContractError(
            "typed open-world candidate R stage and selection variants disagree"
        )
    prerequisite = {
        "ja_admitted": "jp_direct",
        "r_selected": "ja_admitted",
        "packet_selected": "r_selected",
        "public_selected": "packet_selected",
    }
    for stage, required in prerequisite.items():
        if stage in stages and required not in stages:
            raise ContractError(
                f"typed open-world candidate {stage} lacks {required}"
            )


def _load_open_world_adjudication(
    *,
    root: Path,
    path: Path,
    candidate_set_path: Path,
    candidate_set: Mapping[str, Any],
    partition: str,
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    value = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "open-world-adjudication", TYPED_ORACLE_SCHEMA_VERSION
        ),
        document=_read_object(path, "typed open-world adjudication"),
        label="typed open-world adjudication",
    )
    if value.get("schema_version") != TYPED_OPEN_WORLD_ADJUDICATION_SCHEMA_VERSION:
        raise ContractError("unsupported typed open-world adjudication schema")
    if value.get("partition") != partition:
        raise IntegrityError("typed open-world adjudication partition mismatch")
    if value.get("arm_scope") != "all_arms":
        raise ContractError("typed open-world adjudication must cover all arms")
    if value.get("candidate_set_sha256") != sha256_file(
        candidate_set_path.resolve(strict=True)
    ):
        raise IntegrityError("typed open-world adjudication candidate-set binding failed")
    if value.get("source_blind") is not True:
        raise ContractError("typed open-world adjudication was not source blind")
    if value.get("statistics_unblinded") is not False:
        raise ContractError(
            "typed open-world adjudication occurred after statistical unblinding"
        )

    candidate_by_id = {
        str(item["candidate_id"]): item for item in candidate_set["candidates"]
    }
    decision_ids: list[str] = []
    for decision in value["decisions"]:
        candidate_id = str(decision["candidate_id"])
        candidate = candidate_by_id.get(candidate_id)
        if candidate is None:
            raise IntegrityError(
                "typed open-world adjudication references an unknown candidate"
            )
        if decision["candidate_sha256"] != sha256_json(candidate):
            raise IntegrityError(
                "typed open-world adjudication candidate hash mismatch"
            )
        if decision["decision"] == "accepted":
            meaning = _object(
                decision["verified_meaning"], "accepted open-world meaning"
            )
            if decision["verified_meaning_sha256"] != sha256_json(meaning):
                raise IntegrityError("accepted open-world meaning hash mismatch")
            if not open_world_meaning_matches_candidate(
                meaning, _object(candidate["payload"], "candidate payload")
            ):
                raise ContractError(
                    "accepted open-world meaning does not exactly verify its candidate"
                )
        decision_ids.append(candidate_id)
    if len(decision_ids) != len(set(decision_ids)):
        raise ContractError("duplicate typed open-world adjudication decision")
    if set(decision_ids) != set(candidate_by_id):
        raise ContractError(
            "typed open-world adjudication has missing or extra candidate decisions"
        )
    case_ids: set[str] = set()
    for judgment in value["case_judgments"]:
        case_id = str(judgment["case_id"])
        if case_id in case_ids:
            raise ContractError("duplicate typed open-world case judgment")
        policy = _object(judgment["policy"], "open-world final case policy")
        if judgment["policy_sha256"] != sha256_json(policy):
            raise IntegrityError("typed open-world final case policy hash mismatch")
        meaning_ids = [str(item) for item in policy["meaning_ids"]]
        if policy["meaning_ids_sha256"] != sha256_json(sorted(meaning_ids)):
            raise IntegrityError("typed open-world final meaning-ID hash mismatch")
        candidate_ids = [
            str(item) for item in judgment["accepted_public_candidate_ids"]
        ]
        if any(item not in candidate_by_id for item in candidate_ids):
            raise IntegrityError(
                "typed open-world case judgment references an unknown candidate"
            )
        if any(
            candidate_by_id[item]["case_id"] != case_id for item in candidate_ids
        ):
            raise IntegrityError(
                "typed open-world case judgment crosses candidate cases"
            )
        case_ids.add(case_id)
    return value


def _accepted_meaning_rows(
    accepted_decisions: Sequence[Mapping[str, Any]],
    candidate_set: Mapping[str, Any],
) -> list[dict[str, Any]]:
    candidate_by_id = {
        str(item["candidate_id"]): item for item in candidate_set["candidates"]
    }
    return sorted(
        (
            {
                "candidate_id": str(decision["candidate_id"]),
                "case_id": str(
                    candidate_by_id[str(decision["candidate_id"])]["case_id"]
                ),
                "verified_meaning": _object(
                    decision["verified_meaning"], "accepted open-world meaning"
                ),
            }
            for decision in accepted_decisions
        ),
        key=lambda item: str(item["candidate_id"]),
    )


def _typed_open_world_manifest_state(
    *,
    base_labels: Sequence[Mapping[str, Any]],
    arm_inventory_path: Path,
    arm_inventory: Mapping[str, Any],
    candidate_set_path: Path,
    candidate_set: Mapping[str, Any],
    adjudication_path: Path,
    adjudication: Mapping[str, Any],
    merged_labels: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    decisions = list(adjudication["decisions"])
    accepted = [item for item in decisions if item["decision"] == "accepted"]
    accepted_rows = _accepted_meaning_rows(accepted, candidate_set)
    rejected_count = sum(item["decision"] == "rejected" for item in decisions)
    case_judgments = list(adjudication["case_judgments"])
    return {
        "status": "frozen",
        "candidate_set_sha256": sha256_file(
            candidate_set_path.resolve(strict=True)
        ),
        "adjudication_sha256": sha256_file(
            adjudication_path.resolve(strict=True)
        ),
        "arm_inventory_sha256": sha256_file(
            arm_inventory_path.resolve(strict=True)
        ),
        "arm_inventory_semantic_sha256": sha256_json(arm_inventory),
        "arm_ids_sha256": _arm_ids_hash(arm_inventory),
        "arm_count": len(arm_inventory["arms"]),
        "candidate_count": len(candidate_set["candidates"]),
        "adjudication_count": len(decisions),
        "accepted_count": len(accepted),
        "rejected_count": rejected_count,
        "case_judgment_count": len(case_judgments),
        "case_judgments_sha256": sha256_json(case_judgments),
        "pending_count": 0,
        "source_blind": adjudication["source_blind"],
        "statistics_unblinded": adjudication["statistics_unblinded"],
        "merge_scope": "all_arms",
        "base_oracle_semantic_sha256": sha256_json(base_labels),
        "accepted_meanings_sha256": sha256_json(accepted_rows),
        "merged_oracle_semantic_sha256": sha256_json(merged_labels),
    }


def _merge_open_world_labels(
    *,
    root: Path,
    base_labels: Sequence[Mapping[str, Any]],
    accepted_decisions: Sequence[Mapping[str, Any]],
    case_judgments: Sequence[Mapping[str, Any]],
    candidate_set: Mapping[str, Any],
    partition_cases: Sequence[Mapping[str, Any]],
) -> list[dict[str, Any]]:
    candidate_by_id = {
        str(item["candidate_id"]): item for item in candidate_set["candidates"]
    }
    case_by_id = {str(item["case_id"]): item for item in partition_cases}
    merged_by_id = {
        str(item["case_id"]): copy.deepcopy(dict(item)) for item in base_labels
    }
    public_candidates_by_case: dict[str, set[str]] = {}
    for decision in accepted_decisions:
        candidate = candidate_by_id[str(decision["candidate_id"])]
        case_id = str(candidate["case_id"])
        merged_by_id[case_id]["meanings"].append(
            copy.deepcopy(decision["verified_meaning"])
        )
        if decision["verified_meaning"]["exposure_policy"] in {
            "primary",
            "complementary",
            "fallback_only",
        }:
            public_candidates_by_case.setdefault(case_id, set()).add(
                str(candidate["candidate_id"])
            )

    judgment_by_case = {
        str(item["case_id"]): item for item in case_judgments
    }
    if set(judgment_by_case) != set(public_candidates_by_case):
        raise ContractError(
            "accepted public-capable candidates require exactly one final case judgment"
        )
    for case_id, expected_candidate_ids in public_candidates_by_case.items():
        judgment = judgment_by_case[case_id]
        if set(judgment["accepted_public_candidate_ids"]) != expected_candidate_ids:
            raise ContractError(
                "final case judgment does not cover exactly its accepted public candidates"
            )
        policy = _object(judgment["policy"], "open-world final case policy")
        exact_meaning_ids = sorted(
            str(item["meaning_id"]) for item in merged_by_id[case_id]["meanings"]
        )
        if list(policy["meaning_ids"]) != exact_meaning_ids:
            raise IntegrityError(
                "final case policy does not own the exact post-merge meaning set"
            )
        merged_by_id[case_id]["disposition"] = policy["disposition"]
        merged_by_id[case_id]["priority"] = copy.deepcopy(policy["priority"])
        accepted_by_id = {
            str(decision["candidate_id"]): decision
            for decision in accepted_decisions
        }
        top = set(str(item) for item in policy["priority"]["required_top_one_of"])
        for candidate_id in expected_candidate_ids:
            meaning = accepted_by_id[candidate_id]["verified_meaning"]
            meaning_id = str(meaning["meaning_id"])
            if meaning_id in top:
                if (
                    meaning.get("generation_policy") != "required"
                    or meaning.get("exposure_policy") != "primary"
                ):
                    raise ContractError(
                        "accepted top candidate must be a required primary meaning"
                    )
            elif meaning.get("generation_policy") != "allowed":
                raise ContractError(
                    "accepted non-top public candidate must remain an allowed meaning"
                )

    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    schema_path = _schema_path(root, "oracle-label", TYPED_ORACLE_SCHEMA_VERSION)
    merged: list[dict[str, Any]] = []
    for case_id in sorted(merged_by_id):
        canonical = _validate_and_canonicalize(
            registry=registry,
            canonicalizer=canonicalizer,
            schema_path=schema_path,
            document=merged_by_id[case_id],
            label=f"merged typed oracle label {case_id}",
        )
        validate_typed_oracle_label(canonical, case_by_id[case_id])
        merged.append(canonical)
    return merged


def _verify_open_world_candidate_coverage(
    *,
    candidate_set: Mapping[str, Any],
    base_labels: Sequence[Mapping[str, Any]],
    views: Mapping[str, Mapping[str, Any]],
) -> None:
    base_by_id = {str(item["case_id"]): item for item in base_labels}
    candidate_by_key = {
        (str(item["case_id"]), str(item["c_semantics_sha256"])): item
        for item in candidate_set["candidates"]
    }
    for case_id, view in sorted(views.items()):
        snapshot_index = comparison_endpoint_snapshot_index(view)
        base_label = base_by_id.get(case_id)
        if base_label is None:
            raise IntegrityError(
                "typed runtime view has no partition-scoped base oracle label"
            )
        for cause in view["causes"]:
            if cause_matches_typed_oracle(cause, base_label, snapshot_index):
                continue
            payload = canonical_open_world_cause_candidate(cause, snapshot_index)
            c_semantics = _object(
                payload["c_semantics"], "generated open-world C semantics"
            )
            key = (case_id, sha256_json(c_semantics))
            candidate = candidate_by_key.get(key)
            if candidate is None:
                raise ContractError(
                    "unmatched generated C Cause has no source-blind open-world candidate"
                )
            frozen_payload = _object(
                candidate["payload"], "frozen open-world candidate payload"
            )
            if frozen_payload["c_semantics"] != c_semantics:
                raise IntegrityError(
                    "open-world candidate C semantics differ from the generated Cause"
                )
            if frozen_payload != payload:
                raise IntegrityError(
                    "source-blind candidate contains behavior beyond exact C semantics"
                )


def _unmatched_observations_for_view(
    *,
    base_label: Mapping[str, Any],
    view: Mapping[str, Any],
) -> list[dict[str, Any]]:
    snapshot_index = comparison_endpoint_snapshot_index(view)
    payloads_by_hash: dict[str, list[dict[str, Any]]] = {}
    for cause in view["causes"]:
        if cause_matches_typed_oracle(cause, base_label, snapshot_index):
            continue
        payload = canonical_open_world_cause_observation(cause, snapshot_index)
        c_hash = sha256_json(payload["c_semantics"])
        payloads_by_hash.setdefault(c_hash, []).append(payload)
    observations = []
    for c_hash, payloads in payloads_by_hash.items():
        payload = _merge_candidate_payloads(payloads)
        observations.append(
            {
                "c_semantics_sha256": c_hash,
                "payload_sha256": sha256_json(payload),
                "payload": payload,
            }
        )
    return sorted(observations, key=json_bytes)


def _runtime_producer_contract(runtime_run: Mapping[str, Any]) -> dict[str, Any]:
    adapter = _object(runtime_run.get("adapter"), "runtime producer adapter")
    return {
        "runtime_run_schema_version": runtime_run.get("schema_version"),
        "runtime_observation_schema": adapter.get("runtime_observation_schema"),
        "adapter_main_class": adapter.get("main_class"),
        "adapter_launcher_sha256": adapter.get("sbt_sha256"),
        "engine_sha256": runtime_run.get("engine_sha256"),
        "candidate_binding_sha256": runtime_run.get("candidate_binding_sha256"),
    }


def _load_typed_runtime_run(
    *,
    root: Path,
    runtime_run_path: Path,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    base_labels: Sequence[Mapping[str, Any]],
    partition: str,
    partition_cases: Sequence[Mapping[str, Any]],
    required_case_ids: Sequence[str] = (),
    require_full_partition: bool,
) -> tuple[dict[str, Any], dict[str, dict[str, Any]]]:
    runtime_run = _read_object(runtime_run_path, "typed cause audit runtime run")
    if runtime_run.get("schema_version") != RUNTIME_RUN_SCHEMA_VERSION:
        raise ContractError("unsupported cause audit runtime run schema")
    if runtime_run.get("partition") != partition:
        raise IntegrityError("typed runtime run partition mismatch")
    if runtime_run.get("freeze_manifest_sha256") != sha256_file(
        manifest_path.resolve(strict=True)
    ):
        raise IntegrityError("runtime run freeze manifest binding failed")
    if runtime_run.get("cases_file_sha256") != sha256_file(
        cases_path.resolve(strict=True)
    ):
        raise IntegrityError("runtime run cases binding failed")
    expected_base_binding = _typed_base_oracle_binding(
        base_oracle_path=base_oracle_path,
        partition=partition,
        labels=base_labels,
    )
    if runtime_run.get("base_oracle") != expected_base_binding:
        raise IntegrityError(
            "typed runtime run base-oracle binding is missing or differs"
        )
    adapter = _object(runtime_run.get("adapter"), "typed runtime adapter binding")
    if adapter.get("main_class") != CAUSE_ADAPTER_MAIN:
        raise IntegrityError("typed runtime run used a non-Cause adapter")
    if adapter.get("runtime_observation_schema") != TYPED_RUNTIME_OBSERVATION_SCHEMA:
        raise IntegrityError("typed runtime run did not use the v3 observation contract")
    raw_views = runtime_run.get("views")
    if not isinstance(raw_views, list):
        raise ContractError("typed cause audit runtime run has no views")
    if runtime_run.get("case_count") != len(raw_views):
        raise IntegrityError("typed runtime run case count does not match its views")

    partition_case_ids = {str(item["case_id"]) for item in partition_cases}
    views: dict[str, dict[str, Any]] = {}
    for raw in raw_views:
        if not isinstance(raw, Mapping):
            raise ContractError("typed runtime run contains a non-object view")
        view = _strict_actual_view(root, raw)
        if view.get("schema_version") != TYPED_ACTUAL_VIEW_SCHEMA_VERSION:
            raise ContractError("typed comparison requires v3 actual Cause views")
        case_id = str(view["case_id"])
        if case_id in views:
            raise ContractError(f"duplicate typed actual Cause cascade: {case_id}")
        if case_id not in partition_case_ids or view.get("partition") != partition:
            raise IntegrityError("typed runtime run crosses its physical partition")
        causes = view.get("causes")
        if not isinstance(causes, list) or view.get("cause_count") != len(causes):
            raise IntegrityError(f"typed actual Cause count mismatch: {case_id}")
        views[case_id] = view

    required = set(required_case_ids)
    if not required.issubset(views):
        raise ContractError(
            "runtime run is missing selected cases: "
            f"{sorted(required - set(views))[:5]}"
        )
    if require_full_partition and set(views) != partition_case_ids:
        raise IntegrityError(
            "typed open-world inventory arm does not cover the exact partition"
        )
    return runtime_run, views


def _verify_runtime_against_arm_inventory(
    *,
    runtime_run_path: Path,
    runtime_run: Mapping[str, Any],
    arm_inventory: Mapping[str, Any],
    base_labels: Sequence[Mapping[str, Any]],
    views: Mapping[str, Mapping[str, Any]],
) -> None:
    runtime_hash = sha256_file(runtime_run_path.resolve(strict=True))
    matching = [
        item
        for item in arm_inventory["arms"]
        if item["runtime_run_file_sha256"] == runtime_hash
    ]
    if len(matching) != 1:
        raise IntegrityError(
            "typed runtime run is not exactly one frozen open-world inventory arm"
        )
    arm = matching[0]
    if arm["arm_id"] != runtime_run.get("run_id"):
        raise IntegrityError("typed runtime run ID differs from its inventory arm ID")
    producer = _runtime_producer_contract(runtime_run)
    if arm["producer_contract"] != producer or arm[
        "producer_contract_sha256"
    ] != sha256_json(producer):
        raise IntegrityError("typed runtime producer contract differs from inventory")
    cascade_by_id = {
        str(item["case_id"]): item for item in arm["actual_cascades"]
    }
    if set(cascade_by_id) != set(views):
        raise IntegrityError("typed runtime case scope differs from its inventory arm")
    base_by_id = {str(item["case_id"]): item for item in base_labels}
    for case_id, view in views.items():
        cascade = cascade_by_id[case_id]
        if cascade["actual_cascade_sha256"] != sha256_json(view):
            raise IntegrityError(
                "typed runtime actual cascade differs from its frozen arm hash"
            )
        observations = _unmatched_observations_for_view(
            base_label=base_by_id[case_id], view=view
        )
        if cascade["unmatched_cause_observations"] != observations:
            raise IntegrityError(
                "typed runtime unmatched-C observations differ from its frozen arm"
            )


def _ids_hash(rows: Sequence[Mapping[str, Any]]) -> str:
    return sha256_json(sorted(str(item["case_id"]) for item in rows))


def _typed_open_world_inputs(
    *,
    root: Path,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    partition: str,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    if partition not in PARTITIONS:
        raise ContractError(
            "typed open-world production requires one physical partition"
        )
    if partition == "sealed_confirm":
        raise ContractError(TYPED_SEALED_PROJECTION_BLOCK)
    registry, canonicalizer = _schema_tools(root)
    manifest, cases = _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    partition_cases = _selected_cases(cases, manifest, partition)
    base_oracle_sha256_before_load = sha256_file(
        base_oracle_path.resolve(strict=True)
    )
    base_labels, _ = _load_typed_labels(
        root=root,
        path=base_oracle_path,
        partition_cases=partition_cases,
        partition=partition,
    )
    if sha256_file(base_oracle_path.resolve(strict=True)) != base_oracle_sha256_before_load:
        raise IntegrityError("base-oracle bytes changed while being validated")
    return partition_cases, base_labels


def freeze_open_world_arm_inventory(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    runtime_run_paths: Sequence[Path],
    partition: str,
) -> dict[str, Any]:
    partition_cases, base_labels = _typed_open_world_inputs(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        base_oracle_path=base_oracle_path,
        partition=partition,
    )
    if not runtime_run_paths:
        raise ContractError(
            "open-world inventory requires at least one --runtime-arm"
        )
    resolved_paths = [path.resolve(strict=True) for path in runtime_run_paths]
    if len(resolved_paths) != len(set(resolved_paths)):
        raise ContractError("open-world inventory repeats a runtime arm path")

    arms: list[dict[str, Any]] = []
    arm_ids: set[str] = set()
    runtime_hashes: set[str] = set()
    for runtime_run_path in resolved_paths:
        runtime_hash_before_load = sha256_file(runtime_run_path)
        runtime_run, views = _load_typed_runtime_run(
            root=root,
            runtime_run_path=runtime_run_path,
            manifest_path=manifest_path,
            cases_path=cases_path,
            base_oracle_path=base_oracle_path,
            base_labels=base_labels,
            partition=partition,
            partition_cases=partition_cases,
            require_full_partition=True,
        )
        base_by_id = {str(item["case_id"]): item for item in base_labels}
        cascades = []
        for case_id in sorted(views):
            view = views[case_id]
            observations = _unmatched_observations_for_view(
                base_label=base_by_id[case_id],
                view=view,
            )
            cascades.append(
                {
                    "case_id": case_id,
                    "actual_cascade_sha256": sha256_json(view),
                    "observation_count": len(observations),
                    "unmatched_cause_observations": observations,
                }
            )
        runtime_hash = sha256_file(runtime_run_path)
        if runtime_hash != runtime_hash_before_load:
            raise IntegrityError("runtime arm bytes changed while being inventoried")
        arm_id = str(runtime_run.get("run_id", ""))
        if arm_id in arm_ids or runtime_hash in runtime_hashes:
            raise ContractError("open-world inventory contains duplicate runtime arms")
        arm_ids.add(arm_id)
        runtime_hashes.add(runtime_hash)
        producer = _runtime_producer_contract(runtime_run)
        arms.append(
            {
                "arm_id": arm_id,
                "runtime_run_file_sha256": runtime_hash,
                "producer_contract_sha256": sha256_json(producer),
                "producer_contract": producer,
                "case_ids_sha256": _ids_hash(base_labels),
                "actual_cascades_sha256": sha256_json(cascades),
                "actual_cascades": cascades,
            }
        )
    document = {
        "schema_version": TYPED_OPEN_WORLD_ARM_INVENTORY_SCHEMA_VERSION,
        "partition": partition,
        "freeze_manifest_sha256": sha256_file(
            manifest_path.resolve(strict=True)
        ),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "base_oracle_file_sha256": sha256_file(
            base_oracle_path.resolve(strict=True)
        ),
        "case_ids_sha256": _ids_hash(base_labels),
        "arms": arms,
    }
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_SCHEMA_VERSION)
    canonical = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "open-world-arm-inventory", TYPED_ORACLE_SCHEMA_VERSION
        ),
        document=document,
        label="typed open-world arm inventory",
    )
    for arm in canonical["arms"]:
        arm["actual_cascades_sha256"] = sha256_json(arm["actual_cascades"])
    canonical = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "open-world-arm-inventory", TYPED_ORACLE_SCHEMA_VERSION
        ),
        document=canonical,
        label="typed open-world arm inventory",
    )
    for arm in canonical["arms"]:
        if arm["actual_cascades_sha256"] != sha256_json(
            arm["actual_cascades"]
        ):
            raise IntegrityError(
                "canonical typed open-world arm cascade-set hash mismatch"
            )
    store.capture_run_document("cause-audit-open-world-arm-inventory", canonical)
    store.verify()
    return canonical


def freeze_open_world_candidate_set(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    arm_inventory_path: Path,
    partition: str,
) -> dict[str, Any]:
    _, base_labels = _typed_open_world_inputs(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        base_oracle_path=base_oracle_path,
        partition=partition,
    )
    arm_inventory = _load_open_world_arm_inventory(
        root=root,
        path=arm_inventory_path,
        base_oracle_path=base_oracle_path,
        manifest_path=manifest_path,
        cases_path=cases_path,
        labels=base_labels,
        partition=partition,
    )
    canonical = _open_world_candidate_set_document(
        root=root,
        partition=partition,
        base_oracle_path=base_oracle_path,
        base_labels=base_labels,
        arm_inventory_path=arm_inventory_path,
        arm_inventory=arm_inventory,
    )
    store.capture_run_document("cause-audit-open-world-candidates", canonical)
    store.verify()
    return canonical


def _typed_oracle_run_manifest_document(
    *,
    root: Path,
    partition: str,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    base_labels: Sequence[Mapping[str, Any]],
    run_oracle_file_sha256: str,
    arm_inventory_path: Path,
    arm_inventory: Mapping[str, Any],
    candidate_set_path: Path,
    candidate_set: Mapping[str, Any],
    adjudication_path: Path,
    adjudication: Mapping[str, Any],
    merged_labels: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    document = {
        "schema_version": TYPED_ORACLE_RUN_SCHEMA_VERSION,
        "partition": partition,
        "freeze_manifest_sha256": sha256_file(
            manifest_path.resolve(strict=True)
        ),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "base_oracle_file_sha256": sha256_file(
            base_oracle_path.resolve(strict=True)
        ),
        "run_oracle_file_sha256": run_oracle_file_sha256,
        "case_ids_sha256": _ids_hash(merged_labels),
        "open_world": _typed_open_world_manifest_state(
            base_labels=base_labels,
            arm_inventory_path=arm_inventory_path,
            arm_inventory=arm_inventory,
            candidate_set_path=candidate_set_path,
            candidate_set=candidate_set,
            adjudication_path=adjudication_path,
            adjudication=adjudication,
            merged_labels=merged_labels,
        ),
    }
    registry, canonicalizer = _schema_tools(root, TYPED_ORACLE_RUN_SCHEMA_VERSION)
    return _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(
            root, "oracle-run-manifest", TYPED_ORACLE_RUN_SCHEMA_VERSION
        ),
        document=document,
        label="typed oracle run manifest",
    )


def finalize_open_world_oracle(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    arm_inventory_path: Path,
    candidate_set_path: Path,
    adjudication_path: Path,
    run_oracle_output_path: Path,
    oracle_run_manifest_output_path: Path,
    partition: str,
) -> dict[str, Any]:
    partition_cases, base_labels = _typed_open_world_inputs(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        base_oracle_path=base_oracle_path,
        partition=partition,
    )
    arm_inventory = _load_open_world_arm_inventory(
        root=root,
        path=arm_inventory_path,
        base_oracle_path=base_oracle_path,
        manifest_path=manifest_path,
        cases_path=cases_path,
        labels=base_labels,
        partition=partition,
    )
    candidate_set = _load_open_world_candidate_set(
        root=root,
        path=candidate_set_path,
        base_oracle_path=base_oracle_path,
        arm_inventory_path=arm_inventory_path,
        arm_inventory=arm_inventory,
        labels=base_labels,
        partition=partition,
    )
    adjudication = _load_open_world_adjudication(
        root=root,
        path=adjudication_path,
        candidate_set_path=candidate_set_path,
        candidate_set=candidate_set,
        partition=partition,
    )
    accepted = [
        item for item in adjudication["decisions"] if item["decision"] == "accepted"
    ]
    merged = _merge_open_world_labels(
        root=root,
        base_labels=base_labels,
        accepted_decisions=accepted,
        case_judgments=list(adjudication["case_judgments"]),
        candidate_set=candidate_set,
        partition_cases=partition_cases,
    )
    _assert_ascii(merged, "typed run oracle")
    run_oracle_bytes = b"".join(json_bytes(item) for item in merged)
    run_output = run_oracle_output_path.resolve()
    manifest_output = oracle_run_manifest_output_path.resolve()
    if run_output == manifest_output:
        raise ContractError(
            "run-oracle output and oracle-run manifest output must differ"
        )
    oracle_run_manifest = _typed_oracle_run_manifest_document(
        root=root,
        partition=partition,
        manifest_path=manifest_path,
        cases_path=cases_path,
        base_oracle_path=base_oracle_path,
        base_labels=base_labels,
        run_oracle_file_sha256=sha256_bytes(run_oracle_bytes),
        arm_inventory_path=arm_inventory_path,
        arm_inventory=arm_inventory,
        candidate_set_path=candidate_set_path,
        candidate_set=candidate_set,
        adjudication_path=adjudication_path,
        adjudication=adjudication,
        merged_labels=merged,
    )
    manifest_bytes = json_bytes(oracle_run_manifest)
    with tempfile.TemporaryDirectory(prefix="chesstory-oracle-finalize-") as directory:
        temporary_root = Path(directory)
        temporary_run = temporary_root / "run-oracle.jsonl"
        temporary_manifest = temporary_root / "oracle-run-manifest.json"
        ArtifactStore._write_immutable(temporary_run, run_oracle_bytes)
        ArtifactStore._write_immutable(temporary_manifest, manifest_bytes)
        temporary_labels, _ = _load_typed_labels(
            root=root,
            path=temporary_run,
            partition_cases=partition_cases,
            partition=partition,
        )
        _verify_typed_oracle_run_manifest(
            root=root,
            path=temporary_manifest,
            base_oracle_path=base_oracle_path,
            labels_path=temporary_run,
            arm_inventory_path=arm_inventory_path,
            candidate_set_path=candidate_set_path,
            adjudication_path=adjudication_path,
            labels=temporary_labels,
            partition_cases=partition_cases,
            manifest_path=manifest_path,
            cases_path=cases_path,
            partition=partition,
        )
    ArtifactStore._write_immutable(run_output, run_oracle_bytes)
    ArtifactStore._write_immutable(manifest_output, manifest_bytes)
    if sha256_file(run_output) != oracle_run_manifest["run_oracle_file_sha256"]:
        raise IntegrityError("typed run-oracle bytes changed during finalization")
    run_labels, _ = _load_typed_labels(
        root=root,
        path=run_output,
        partition_cases=partition_cases,
        partition=partition,
    )
    _verify_typed_oracle_run_manifest(
        root=root,
        path=manifest_output,
        base_oracle_path=base_oracle_path,
        labels_path=run_output,
        arm_inventory_path=arm_inventory_path,
        candidate_set_path=candidate_set_path,
        adjudication_path=adjudication_path,
        labels=run_labels,
        partition_cases=partition_cases,
        manifest_path=manifest_path,
        cases_path=cases_path,
        partition=partition,
    )
    store.capture_run_document("cause-audit-run-oracle-canonical", run_labels)
    store.capture_run_document(
        "cause-audit-oracle-run-manifest", oracle_run_manifest
    )
    store.verify()
    return oracle_run_manifest


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
    temporary_name: str | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="wb", dir=path.parent, prefix=f".{path.name}.", delete=False
        ) as handle:
            temporary_name = handle.name
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        try:
            os.link(temporary_name, path)
        except FileExistsError:
            if path.read_bytes() != payload:
                raise IntegrityError(f"cache key collision at {path}")
    finally:
        if temporary_name is not None:
            Path(temporary_name).unlink(missing_ok=True)


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
    schema_version: str,
) -> dict[str, Any]:
    causes = observation.get("causes", [])
    if not isinstance(causes, list) or not all(
        isinstance(item, Mapping) for item in causes
    ):
        raise ContractError("cause adapter observation has no cause array")
    only_move_constraints = observation.get("only_move_constraints", [])
    if not isinstance(only_move_constraints, list) or not all(
        isinstance(item, Mapping) for item in only_move_constraints
    ):
        raise ContractError("cause adapter observation has invalid only-move constraints")
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
    if schema_version not in {
        ACTUAL_VIEW_SCHEMA_VERSION,
        TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    }:
        raise ContractError(f"unsupported actual cause cascade schema: {schema_version}")
    expected_observation_schema = (
        TYPED_RUNTIME_OBSERVATION_SCHEMA
        if schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION
        else RUNTIME_OBSERVATION_SCHEMA
    )
    if observation.get("schema_version") != expected_observation_schema:
        raise ContractError("cause adapter observation contract does not match its view")
    verified_request_sha256: str | None = None
    verified_hash_contract: str | None = None
    if schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION:
        if not transports:
            raise ContractError("v3 cause adapter observation has no runtime transport")
        final_transport = transports[-1]
        verified_request_sha256 = final_transport.get("request_sha256")
        verified_hash_contract = final_transport.get("hash_contract")
        if (
            not isinstance(verified_request_sha256, str)
            or re.fullmatch(r"[0-9a-f]{64}", verified_request_sha256) is None
        ):
            raise ContractError("v3 cause adapter transport has no verified request hash")
        if verified_hash_contract != NATIVE_HASH_CONTRACT:
            raise ContractError("v3 cause adapter transport has no supported hash contract")
        if observation.get("request_sha256") != verified_request_sha256:
            raise IntegrityError("v3 cause adapter request hash changed before actual view")
        if observation.get("hash_contract") != verified_hash_contract:
            raise IntegrityError("v3 cause adapter hash contract changed before actual view")
    expected_runtime = {
        "adapter_name": CAUSE_ADAPTER_NAME,
        "request_schema": RUNTIME_REQUEST_SCHEMA,
    }
    if schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION:
        expected_runtime["response_schema"] = RUNTIME_RESPONSE_SCHEMA
    if any(runtime.get(key) != expected for key, expected in expected_runtime.items()):
        raise ContractError("cause adapter runtime contract is not the active contract")
    if not isinstance(runtime.get("adapter_version"), str) or not runtime[
        "adapter_version"
    ]:
        raise ContractError("cause adapter runtime contract has no adapter version")
    view = {
        "schema_version": schema_version,
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
        "only_move_constraints": [dict(item) for item in only_move_constraints],
        "causes": [dict(item) for item in causes],
    }
    if schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION:
        assert verified_request_sha256 is not None
        assert verified_hash_contract is not None
        view["request_sha256"] = verified_request_sha256
        view["hash_contract"] = verified_hash_contract
        endpoint_snapshots = observation.get(
            "comparison_endpoint_evidence_snapshots"
        )
        if not isinstance(endpoint_snapshots, list) or not all(
            isinstance(item, Mapping) for item in endpoint_snapshots
        ):
            raise ContractError(
                "v3 cause adapter observation has no comparison endpoint evidence snapshot array"
            )
        view["comparison_endpoint_evidence_snapshots"] = copy.deepcopy(
            endpoint_snapshots
        )
        verdict = observation.get("verdict")
        if not isinstance(verdict, Mapping):
            raise ContractError(
                "v3 cause adapter observation has no exact verdict observation"
            )
        view["verdict"] = dict(verdict)
        native_r = observation.get("r_native_cause_selections")
        if not isinstance(native_r, list) or not all(
            isinstance(item, Mapping) for item in native_r
        ):
            raise ContractError(
                "v3 cause adapter observation has no native R selection array"
            )
        view["r_native_cause_selections"] = [dict(item) for item in native_r]
        importance = observation.get("importance")
        if not isinstance(importance, Mapping):
            raise ContractError(
                "v3 cause adapter observation has no typed importance observation"
            )
        view["importance"] = dict(importance)
        idea_units = observation.get("idea_units")
        if not isinstance(idea_units, Mapping):
            raise ContractError(
                "v3 cause adapter observation has no typed idea-unit observation"
            )
        view["idea_units"] = dict(idea_units)
        idea_importance = observation.get("idea_importance")
        if not isinstance(idea_importance, Mapping):
            raise ContractError(
                "v3 cause adapter observation has no typed idea-importance observation"
            )
        view["idea_importance"] = dict(idea_importance)
        cause_disposition_ledger = observation.get("cause_disposition_ledger")
        if not isinstance(cause_disposition_ledger, Mapping):
            raise ContractError(
                "v3 cause adapter observation has no typed Cause disposition ledger"
            )
        view["cause_disposition_ledger"] = dict(cause_disposition_ledger)
    return view


def _run_cause_audit_for_contract(
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
    actual_schema_version: str,
    base_oracle_path: Path | None = None,
) -> dict[str, Any]:
    registry, canonicalizer = _schema_tools(root)
    if actual_schema_version not in {
        ACTUAL_VIEW_SCHEMA_VERSION,
        TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    }:
        raise ContractError(
            f"unsupported cause audit run contract: {actual_schema_version}"
        )
    actual_registry, actual_canonicalizer = _schema_tools(
        root, actual_schema_version
    )
    typed_contract = actual_schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION
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
    base_oracle_binding: dict[str, Any] | None = None
    base_labels: list[dict[str, Any]] = []
    if typed_contract:
        if base_oracle_path is None:
            raise ContractError("active v3 Cause run requires --base-oracle")
        base_oracle_sha256_before_load = sha256_file(
            base_oracle_path.resolve(strict=True)
        )
        partition_cases = _selected_cases(cases, manifest, partition)
        base_labels, _ = _load_typed_labels(
            root=root,
            path=base_oracle_path,
            partition_cases=partition_cases,
            partition=partition,
        )
        canonical_base_artifact = _canonical_base_oracle_artifact(
            partition=partition,
            labels=base_labels,
        )
        captured_base_sha256 = store.capture_run_document(
            "cause-audit-base-oracle-canonical", canonical_base_artifact
        )
        base_oracle_binding = _typed_base_oracle_binding(
            base_oracle_path=base_oracle_path,
            partition=partition,
            labels=base_labels,
        )
        if (
            base_oracle_binding["source_file_sha256"]
            != base_oracle_sha256_before_load
        ):
            raise IntegrityError("base-oracle bytes changed while being validated")
        if (
            captured_base_sha256
            != base_oracle_binding["canonical_artifact_sha256"]
        ):
            raise IntegrityError("captured base-oracle artifact hash mismatch")
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
    observation_schema = (
        TYPED_RUNTIME_OBSERVATION_SCHEMA
        if actual_schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION
        else RUNTIME_OBSERVATION_SCHEMA
    )
    observation_schema_path = (
        _schema_path(root, "runtime-observation", actual_schema_version)
        if typed_contract
        else None
    )
    command = _cause_adapter_command(sbt, actual_schema_version)
    case_by_id = {str(item["case_id"]): item for item in selected}
    views: list[dict[str, Any]] = []
    probe_cache_hits = 0
    probe_cache_misses = 0
    with _RuntimeJsonlSession(
        command,
        cwd=adapter_directory,
        timeout_seconds=provider_timeout_seconds,
        expected_schema=observation_schema,
        observation_schema_path=observation_schema_path,
        expected_hash_contract=NATIVE_HASH_CONTRACT if typed_contract else None,
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
                transport = {
                    "round": round_index,
                    "request_jsonl_sha256": exchange.request_jsonl_sha256,
                    "response_jsonl_sha256": exchange.response_jsonl_sha256,
                    "diagnostic_line_sha256": sorted(
                        set(exchange.diagnostic_line_sha256)
                    ),
                    "diagnostic_stream_sha256": exchange.diagnostic_stream_sha256,
                }
                if typed_contract:
                    if (
                        exchange.request_sha256 is None
                        or exchange.hash_contract != NATIVE_HASH_CONTRACT
                    ):
                        raise IntegrityError(
                            "active cause adapter exchange lost its verified request binding"
                        )
                    adapter_request_sha256 = observation.get("request_sha256")
                    if adapter_request_sha256 != exchange.request_sha256:
                        raise IntegrityError(
                            "active cause adapter exchange changed its request hash"
                        )
                    transport.update(
                        {
                            "request_sha256": exchange.request_sha256,
                            "adapter_request_sha256": adapter_request_sha256,
                            "hash_contract": exchange.hash_contract,
                        }
                    )
                transports.append(transport)
                runtime_round = {
                    "request": copy.deepcopy(request),
                    "observation": dict(observation),
                    **transport,
                }
                if typed_contract:
                    _verified_response_jsonl_bytes(
                        exchange.response_jsonl_base64,
                        exchange.response_jsonl_sha256,
                    )
                    runtime_round["response_jsonl_base64"] = (
                        exchange.response_jsonl_base64
                    )
                runtime_round_io.append(runtime_round)
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
                schema_version=actual_schema_version,
            )
            canonical_view = _validate_and_canonicalize(
                registry=actual_registry,
                canonicalizer=actual_canonicalizer,
                schema_path=_schema_path(
                    root, "actual-cascade-view", actual_schema_version
                ),
                document=view,
                label=f"actual cause cascade {case_id}",
            )
            if typed_contract:
                _validate_typed_actual_transport_binding(
                    canonical_view,
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
            "runtime_observation_schema": observation_schema,
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
    if typed_contract:
        assert base_oracle_binding is not None
        assert base_oracle_path is not None
        if _typed_base_oracle_binding(
            base_oracle_path=base_oracle_path,
            partition=partition,
            labels=base_labels,
        ) != base_oracle_binding:
            raise IntegrityError("base-oracle binding changed during runtime execution")
        document["base_oracle"] = base_oracle_binding
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
    base_oracle_path: Path | None = None,
) -> dict[str, Any]:
    """Run the sole active Cause contract."""

    return _run_cause_audit_for_contract(
        root=root,
        store=store,
        manifest_path=manifest_path,
        cases_path=cases_path,
        acquisition_path=acquisition_path,
        stockfish_path=stockfish_path,
        sbt_path=sbt_path,
        adapter_root=adapter_root,
        cache_root=cache_root,
        partition=partition,
        timeout_seconds=timeout_seconds,
        provider_timeout_seconds=provider_timeout_seconds,
        max_probe_rounds=max_probe_rounds,
        candidate_binding_path=candidate_binding_path,
        requested_case_ids=requested_case_ids,
        actual_schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
        base_oracle_path=base_oracle_path,
    )


def run_historical_cause_audit_v2(
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
    """Replay the archived v2 Cause contract through an explicit boundary."""

    return _run_cause_audit_for_contract(
        root=root,
        store=store,
        manifest_path=manifest_path,
        cases_path=cases_path,
        acquisition_path=acquisition_path,
        stockfish_path=stockfish_path,
        sbt_path=sbt_path,
        adapter_root=adapter_root,
        cache_root=cache_root,
        partition=partition,
        timeout_seconds=timeout_seconds,
        provider_timeout_seconds=provider_timeout_seconds,
        max_probe_rounds=max_probe_rounds,
        candidate_binding_path=candidate_binding_path,
        requested_case_ids=requested_case_ids,
        actual_schema_version=ACTUAL_VIEW_SCHEMA_VERSION,
    )


def _strict_actual_view_for_contracts(
    root: Path,
    value: Mapping[str, Any],
    *,
    supported: set[str],
) -> dict[str, Any]:
    raw = _object(value, "actual cause cascade")
    raw.pop("artifact_capture", None)
    schema_version = raw.get("schema_version")
    if not isinstance(schema_version, str) or schema_version not in supported:
        raise ContractError(
            f"unsupported actual cause cascade schema: {schema_version}"
        )
    registry, canonicalizer = _schema_tools(root, schema_version)
    canonical = _validate_and_canonicalize(
        registry=registry,
        canonicalizer=canonicalizer,
        schema_path=_schema_path(root, "actual-cascade-view", schema_version),
        document=raw,
        label=f"actual cause cascade {raw.get('case_id')}",
    )
    if schema_version == TYPED_ACTUAL_VIEW_SCHEMA_VERSION:
        _validate_typed_actual_transport_binding(
            canonical,
            label=f"actual cause cascade {raw.get('case_id')}",
        )
    return canonical


def _validate_typed_actual_transport_binding(
    value: Mapping[str, Any], *, label: str
) -> None:
    probe_closure = value.get("probe_closure")
    transports = (
        probe_closure.get("runtime_transport")
        if isinstance(probe_closure, Mapping)
        else None
    )
    if (
        not isinstance(transports, list)
        or not transports
        or not all(isinstance(item, Mapping) for item in transports)
    ):
        raise ContractError(f"{label} has no runtime transport binding")
    first = transports[0]
    final = transports[-1]
    if value.get("initial_request_sha256") != first.get("request_jsonl_sha256"):
        raise IntegrityError(f"{label} initial JSONL request hash mismatch")
    if value.get("final_request_sha256") != final.get("request_jsonl_sha256"):
        raise IntegrityError(f"{label} final JSONL request hash mismatch")
    if value.get("request_sha256") != final.get("request_sha256"):
        raise IntegrityError(f"{label} final canonical request hash mismatch")
    if value.get("hash_contract") != NATIVE_HASH_CONTRACT:
        raise ContractError(f"{label} has an unsupported hash contract")
    for index, transport in enumerate(transports, 1):
        if transport.get("hash_contract") != NATIVE_HASH_CONTRACT:
            raise ContractError(
                f"{label} runtime transport {index} has an unsupported hash contract"
            )
        if transport.get("adapter_request_sha256") != transport.get(
            "request_sha256"
        ):
            raise IntegrityError(
                f"{label} runtime transport {index} request hash mismatch"
            )


def _strict_actual_view(
    root: Path,
    value: Mapping[str, Any],
) -> dict[str, Any]:
    return _strict_actual_view_for_contracts(
        root,
        value,
        supported={TYPED_ACTUAL_VIEW_SCHEMA_VERSION},
    )


def _strict_historical_actual_view(
    root: Path,
    value: Mapping[str, Any],
) -> dict[str, Any]:
    return _strict_actual_view_for_contracts(
        root,
        value,
        supported={
            "chesstory.eval.cause-audit-actual-cascade.v1",
            ACTUAL_VIEW_SCHEMA_VERSION,
        },
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


def _has_structurally_compatible_attribution(
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


def _selected_rank_projection(cause: Mapping[str, Any]) -> list[tuple[str, int]]:
    rank = cause.get("r")
    if not isinstance(rank, Mapping):
        return []
    cross_exposure = rank.get("cross_comparison_exposure")
    if not isinstance(cross_exposure, Mapping) or cross_exposure.get("selected") is not True:
        return []
    cause_id = _cause_id(cause)
    priority_rank = cross_exposure.get("priority_rank")
    if (
        not isinstance(priority_rank, int)
        or isinstance(priority_rank, bool)
        or priority_rank < 0
    ):
        return []
    # Native R priority excludes both host-claim rank and the stable-ID
    # serialization tie-break. Packet loss must not be reclassified as R loss.
    return [(cause_id, priority_rank)]


def _rank_indices(cause: Mapping[str, Any]) -> list[int]:
    return [rank_index for _, rank_index in _selected_rank_projection(cause)]


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


def _packet_public_selection_matches(cause: Mapping[str, Any]) -> bool:
    p_value = cause.get("p")
    if not isinstance(p_value, Mapping) or p_value.get("packet_selected") is not True:
        return False
    cause_id = p_value.get("cause_evidence_id")
    packet = p_value.get("packet_selection")
    public = p_value.get("public_selection")
    public_ids = p_value.get("selected_public_cause_evidence_ids")
    if (
        not isinstance(cause_id, str)
        or p_value.get("public_selection_count") != 1
        or not isinstance(packet, Mapping)
        or not isinstance(public, Mapping)
        or not isinstance(public_ids, list)
        or cause_id not in public_ids
        or packet.get("cause_evidence_id") != cause_id
        or public.get("cause_evidence_id") != cause_id
    ):
        return False
    return dict(public) == dict(packet)


def _global_packet_public_projection_matches(actual: Mapping[str, Any]) -> bool:
    causes = actual.get("causes")
    projection = actual.get("projection")
    if not isinstance(causes, list) or not isinstance(projection, Mapping):
        return False
    if projection.get("present") is not True:
        return False
    raw_public_ids = projection.get("selected_public_cause_evidence_ids")
    typed_public = projection.get("selected_public_cause_selections")
    raw_idea_count = projection.get("raw_public_idea_count")
    parsed_idea_count = projection.get("parsed_public_idea_count")
    parse_diagnostics_present = any(
        field in projection
        for field in (
            "raw_public_idea_count",
            "parsed_public_idea_count",
            "public_ideas_parse_closed",
        )
    )
    if (
        (
            parse_diagnostics_present
            and (
                projection.get("public_ideas_parse_closed") is not True
                or not isinstance(raw_idea_count, int)
                or isinstance(raw_idea_count, bool)
                or raw_idea_count < 0
                or not isinstance(parsed_idea_count, int)
                or isinstance(parsed_idea_count, bool)
                or parsed_idea_count < 0
                or raw_idea_count != parsed_idea_count
            )
        )
        or not isinstance(raw_public_ids, list)
        or not all(isinstance(item, str) and item for item in raw_public_ids)
        or len(raw_public_ids) != len(set(raw_public_ids))
        or not isinstance(typed_public, list)
        or not all(isinstance(item, Mapping) for item in typed_public)
        or (
            parse_diagnostics_present
            and parsed_idea_count != len(typed_public)
        )
    ):
        return False
    typed_public_ids = [item.get("cause_evidence_id") for item in typed_public]
    if not all(isinstance(item, str) and item for item in typed_public_ids):
        return False
    packet_selections: list[Mapping[str, Any]] = []
    registered_ids: list[str] = []
    raw_occurrence_counter: Counter[str] = Counter()
    for cause in causes:
        if not isinstance(cause, Mapping):
            return False
        cause_id = _cause_id(cause)
        registered_ids.append(cause_id)
        p_value = cause.get("p")
        if not isinstance(p_value, Mapping):
            return False
        if p_value.get("cause_evidence_id") != cause_id:
            return False
        public_count = p_value.get("public_selection_count")
        selected_ids = p_value.get("selected_public_cause_evidence_ids")
        if (
            not isinstance(public_count, int)
            or isinstance(public_count, bool)
            or public_count < 0
            or not isinstance(selected_ids, list)
            or not all(isinstance(item, str) for item in selected_ids)
        ):
            return False
        expected_selected_ids = [cause_id] if public_count > 0 else []
        if selected_ids != expected_selected_ids:
            return False
        if public_count > 0:
            raw_occurrence_counter[cause_id] = public_count
        if p_value.get("packet_selected") is True:
            selection = p_value.get("packet_selection")
            if not isinstance(selection, Mapping):
                return False
            packet_selections.append(selection)
    if len(registered_ids) != len(set(registered_ids)):
        return False
    # The projection exposes a distinct raw-ID set, while each registered
    # Cause preserves its raw occurrence count. Their union must close, and
    # every raw occurrence must parse to exactly one typed selection.
    if set(raw_public_ids) != set(raw_occurrence_counter):
        return False
    if raw_occurrence_counter != Counter(typed_public_ids):
        return False
    # Selection order in the observation is serialization only. Compare exact
    # typed objects as multisets so no extra, missing, or mutated public Cause
    # can hide behind one correctly preserved oracle-matched Cause.
    packet_multiset = Counter(sha256_json(dict(item)) for item in packet_selections)
    public_multiset = Counter(sha256_json(dict(item)) for item in typed_public)
    return packet_multiset == public_multiset


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
        selected_ranks = _selected_rank_projection(cause)
        ranked += len(selected_ranks)
        p_value = cause.get("p")
        if isinstance(p_value, Mapping):
            cause_id = p_value.get("cause_evidence_id")
            if p_value.get("packet_selected") is True and isinstance(cause_id, str):
                packet_ids.add(cause_id)
            if isinstance(cause_id, str) and _packet_public_selection_matches(cause):
                public_ids.add(cause_id)
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
    cause_id = p_value.get("cause_evidence_id")
    public_ids = p_value.get("selected_public_cause_evidence_ids")
    return bool(
        isinstance(cause_id, str)
        and isinstance(public_ids, list)
        and cause_id in public_ids
        and isinstance(p_value.get("public_selection_count"), int)
        and not isinstance(p_value.get("public_selection_count"), bool)
        and int(p_value["public_selection_count"]) > 0
    )


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
                "object_tuple_assessment": OBJECT_TUPLE_ASSESSMENT,
                "attribution_semantic_assessment": ATTRIBUTION_SEMANTIC_ASSESSMENT,
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
                "object_tuple_assessment": OBJECT_TUPLE_ASSESSMENT,
                "attribution_semantic_assessment": ATTRIBUTION_SEMANTIC_ASSESSMENT,
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
    attribution_compatible = [
        cause
        for cause in kind_and_root
        if _has_structurally_compatible_attribution(
            cause, expected_source_side=str(label["source_side"])
        )
    ]
    matched = [
        cause
        for cause in attribution_compatible
        if _has_complete_cause_tuple(cause)
    ]
    forbidden = [
        cause
        for cause in root_aligned
        if _cause_parts(cause)[0].get("kind") in forbidden_kinds
    ]
    competing = [cause for cause in root_aligned if cause not in matched]
    errors: set[str] = set()
    global_projection_matches = _global_packet_public_projection_matches(actual)
    if not global_projection_matches:
        errors.add("projection_set_mismatch")
    if not matched:
        if attribution_compatible:
            closest_fields = _closest_binding_fields(attribution_compatible)
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
        if any(_publicly_selected(cause) for cause in forbidden):
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
    if forbidden or not matched:
        first_failure = "C"
    elif cascade["jp"] == 0:
        errors.add("jp_loss")
        first_failure = "Jp"
    elif cascade["ja"] == 0:
        errors.add("ja_loss")
        first_failure = "Ja"
    elif "priority_inversion" in errors:
        first_failure = "R"
    elif cascade["r"] == 0:
        errors.add("r_loss")
        first_failure = "R"
    elif cascade["packet"] == 0:
        errors.add("packet_loss")
        first_failure = "P"
    elif not global_projection_matches:
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
            "object_tuple_assessment": OBJECT_TUPLE_ASSESSMENT,
            "attribution_semantic_assessment": ATTRIBUTION_SEMANTIC_ASSESSMENT,
        },
        "cascade": cascade,
        "errors": sorted(errors),
        "first_failure_stage": first_failure,
    }


def _compare_typed_cause_audit(
    *,
    root: Path,
    store: ArtifactStore,
    manifest_path: Path,
    cases_path: Path,
    base_oracle_path: Path,
    labels_path: Path,
    oracle_run_manifest_path: Path,
    candidate_set_path: Path,
    adjudication_path: Path,
    arm_inventory_path: Path,
    runtime_run_path: Path,
    partition: str,
    requested_case_ids: Sequence[str],
) -> dict[str, Any]:
    if partition == "sealed_confirm":
        raise ContractError(TYPED_SEALED_PROJECTION_BLOCK)
    if partition not in PARTITIONS:
        raise ContractError(
            "typed Cause comparison must load one physical partition at a time"
        )
    registry, canonicalizer = _schema_tools(root)
    manifest, cases = _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    partition_cases = _selected_cases(cases, manifest, partition)
    labels, _ = _load_typed_labels(
        root=root,
        path=labels_path,
        partition_cases=partition_cases,
        partition=partition,
    )
    oracle_verification = _verify_typed_oracle_run_manifest(
        root=root,
        path=oracle_run_manifest_path,
        base_oracle_path=base_oracle_path,
        labels_path=labels_path,
        arm_inventory_path=arm_inventory_path,
        candidate_set_path=candidate_set_path,
        adjudication_path=adjudication_path,
        labels=labels,
        partition_cases=partition_cases,
        manifest_path=manifest_path,
        cases_path=cases_path,
        partition=partition,
    )
    oracle_run = _object(
        oracle_verification["manifest"], "typed oracle run manifest"
    )
    selected = _selected_cases(cases, manifest, partition, requested_case_ids)
    selected_ids = {str(item["case_id"]) for item in selected}
    case_by_id = {str(item["case_id"]): item for item in selected}
    label_by_id = {str(item["case_id"]): item for item in labels}
    if not selected_ids.issubset(label_by_id):
        raise IntegrityError("typed run oracle does not cover the requested cases")

    runtime_run, views = _load_typed_runtime_run(
        root=root,
        runtime_run_path=runtime_run_path,
        manifest_path=manifest_path,
        cases_path=cases_path,
        base_oracle_path=base_oracle_path,
        base_labels=list(oracle_verification["base_labels"]),
        partition=partition,
        partition_cases=partition_cases,
        required_case_ids=sorted(selected_ids),
        require_full_partition=True,
    )
    _verify_open_world_candidate_coverage(
        candidate_set=_object(
            oracle_verification["candidate_set"], "typed open-world candidate set"
        ),
        base_labels=list(oracle_verification["base_labels"]),
        views=views,
    )
    _verify_runtime_against_arm_inventory(
        runtime_run_path=runtime_run_path,
        runtime_run=runtime_run,
        arm_inventory=_object(
            oracle_verification["arm_inventory"], "typed open-world arm inventory"
        ),
        base_labels=list(oracle_verification["base_labels"]),
        views=views,
    )

    judgment_registry, judgment_canonicalizer = _schema_tools(
        root, TYPED_JUDGMENT_SCHEMA_VERSION
    )
    judgments: list[dict[str, Any]] = []
    for case_id in sorted(selected_ids):
        label = label_by_id[case_id]
        view = views[case_id]
        judgment = judge_typed_case(
            case=case_by_id[case_id],
            label=label,
            actual=view,
            oracle_label_sha256=sha256_json(label),
            actual_view_sha256=sha256_json(view),
        )
        canonical_judgment = _validate_and_canonicalize(
            registry=judgment_registry,
            canonicalizer=judgment_canonicalizer,
            schema_path=_schema_path(
                root, "judgment", TYPED_JUDGMENT_SCHEMA_VERSION
            ),
            document=judgment,
            label=f"typed Cause audit judgment {case_id}",
        )
        store.capture_run_document(
            f"cause-audit-judgment-{case_id}", canonical_judgment
        )
        judgments.append(canonical_judgment)

    status_counts = Counter(str(item["status"]) for item in judgments)
    disposition_counts = Counter(str(item["disposition"]) for item in judgments)
    endpoint_counts = Counter(str(item["endpoint_status"]) for item in judgments)
    error_counts = Counter(
        str(error["code"])
        for item in judgments
        for error in item["errors"]
        if isinstance(error, Mapping)
    )
    failure_counts = Counter(
        "none"
        if item["first_failure_stage"] is None
        else str(item["first_failure_stage"])
        for item in judgments
    )
    selected_views = [views[case_id] for case_id in sorted(selected_ids)]
    open_world = _object(oracle_run["open_world"], "typed open-world binding")
    report = {
        "schema_version": TYPED_REPORT_SCHEMA_VERSION,
        "run_id": store.run_id,
        "mode": "typed-semantic-cause-audit",
        "partition": partition,
        "bindings": {
            "freeze_manifest_sha256": sha256_file(
                manifest_path.resolve(strict=True)
            ),
            "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
            "base_oracle_file_sha256": sha256_file(
                base_oracle_path.resolve(strict=True)
            ),
            "run_oracle_file_sha256": sha256_file(
                labels_path.resolve(strict=True)
            ),
            "oracle_run_manifest_sha256": sha256_file(
                oracle_run_manifest_path.resolve(strict=True)
            ),
            "open_world_candidate_set_sha256": sha256_file(
                candidate_set_path.resolve(strict=True)
            ),
            "open_world_arm_inventory_sha256": sha256_file(
                arm_inventory_path.resolve(strict=True)
            ),
            "open_world_adjudication_sha256": sha256_file(
                adjudication_path.resolve(strict=True)
            ),
            "runtime_run_sha256": sha256_file(
                runtime_run_path.resolve(strict=True)
            ),
        },
        "evaluation_contract": {
            "c_rule": "typed-exact-values-in-one-owned-direct-channel",
            "r_rule": "native-pre-packet-selection-and-partial-order",
            "p_rule": "exact-r-to-packet-to-public-semantic-multiset",
            "priority_rule": "ties-and-incomparability-are-not-serialization-order",
            "abstention_rule": "answerable-must-abstain-unresolved-are-distinct",
            "open_world_rule": "all-unmatched-c-source-blind-adjudication-exact-all-arm-merge-before-scoring",
            "sealed_label_disclosure": "disabled-until-redacted-projection",
        },
        "counts": {
            "cases": len(judgments),
            **{f"disposition_{key}": value for key, value in disposition_counts.items()},
            **{f"status_{key}": value for key, value in status_counts.items()},
            **{f"endpoint_{key}": value for key, value in endpoint_counts.items()},
            "generated_causes": sum(
                len(item["causes"]) for item in selected_views
            ),
            "native_r_selections": sum(
                len(item["r_native_cause_selections"])
                for item in selected_views
            ),
        },
        "error_counts": dict(sorted(error_counts.items())),
        "first_failure_counts": dict(sorted(failure_counts.items())),
        "judgments": judgments,
        "policy": {
            "machine_language": "en",
            "release_eligible": False,
            "production_bottleneck_claim": "engineering-diagnostic",
            "oracle_partition_scoped": True,
            "pending_open_world_candidates": int(open_world["pending_count"]),
        },
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    report_registry, report_canonicalizer = _schema_tools(
        root, TYPED_REPORT_SCHEMA_VERSION
    )
    canonical_report = _validate_and_canonicalize(
        registry=report_registry,
        canonicalizer=report_canonicalizer,
        schema_path=_schema_path(root, "report", TYPED_REPORT_SCHEMA_VERSION),
        document=report,
        label="typed Cause audit report",
    )
    store.capture_run_document("cause-audit-report", canonical_report)
    store.verify()
    return canonical_report


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
    oracle_run_manifest_path: Path | None = None,
    base_oracle_path: Path | None = None,
    candidate_set_path: Path | None = None,
    adjudication_path: Path | None = None,
    arm_inventory_path: Path | None = None,
) -> dict[str, Any]:
    """Compare only the active v3 contract; never infer a legacy route."""

    if partition == "sealed_confirm" and any(
        value is not None
        for value in (
            oracle_run_manifest_path,
            base_oracle_path,
            candidate_set_path,
            adjudication_path,
            arm_inventory_path,
        )
    ):
        raise ContractError(TYPED_SEALED_PROJECTION_BLOCK)
    required_paths = {
        "oracle-run-manifest": oracle_run_manifest_path,
        "base-oracle": base_oracle_path,
        "open-world-candidates": candidate_set_path,
        "open-world-adjudication": adjudication_path,
        "open-world-arm-inventory": arm_inventory_path,
    }
    missing = [name for name, value in required_paths.items() if value is None]
    if missing:
        raise ContractError(
            "active v3 Cause comparison requires "
            + ", ".join(f"--{name}" for name in missing)
        )
    assert oracle_run_manifest_path is not None
    assert base_oracle_path is not None
    assert candidate_set_path is not None
    assert adjudication_path is not None
    assert arm_inventory_path is not None
    return _compare_typed_cause_audit(
        root=root,
        store=store,
        manifest_path=manifest_path,
        cases_path=cases_path,
        base_oracle_path=base_oracle_path,
        labels_path=labels_path,
        oracle_run_manifest_path=oracle_run_manifest_path,
        candidate_set_path=candidate_set_path,
        adjudication_path=adjudication_path,
        arm_inventory_path=arm_inventory_path,
        runtime_run_path=runtime_run_path,
        partition=partition,
        requested_case_ids=requested_case_ids,
    )


def compare_historical_cause_audit_v2(
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
    """Compare archived v2 views only through an explicitly named boundary."""

    if _peek_label_schema_version(labels_path) != HISTORICAL_ORACLE_SCHEMA_VERSION:
        raise ContractError("historical v2 comparison requires v1 curation labels")
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
        view = _strict_historical_actual_view(root, raw)
        case_id = str(view["case_id"])
        if case_id in views:
            raise ContractError(f"duplicate actual cause cascade: {case_id}")
        views[case_id] = view
    if not selected_ids.issubset(views):
        raise ContractError(
            f"runtime run is missing selected cases: {sorted(selected_ids - set(views))[:5]}"
        )
    judgments: list[dict[str, Any]] = []
    judgment_registry, judgment_canonicalizer = _schema_tools(
        root, JUDGMENT_SCHEMA_VERSION
    )
    for case_id in sorted(selected_ids):
        judgment = _judge_case(
            case=case_by_id[case_id],
            label=label_by_id[case_id],
            actual=views[case_id],
        )
        canonical_judgment = _validate_and_canonicalize(
            registry=judgment_registry,
            canonicalizer=judgment_canonicalizer,
            schema_path=_schema_path(root, "judgment", JUDGMENT_SCHEMA_VERSION),
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
            "object_tuple_assessment": "structural-completeness-only-no-value-equivalence",
            "attribution_semantic_assessment": "not-performed-no-typed-oracle-polarity",
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
    report_registry, report_canonicalizer = _schema_tools(
        root, REPORT_SCHEMA_VERSION
    )
    canonical_report = _validate_and_canonicalize(
        registry=report_registry,
        canonicalizer=report_canonicalizer,
        schema_path=_schema_path(root, "report", REPORT_SCHEMA_VERSION),
        document=report,
        label="cause audit report",
    )
    store.capture_run_document("cause-audit-report", canonical_report)
    store.verify()
    return canonical_report


def execute_cause_audit_action(
    *, root: Path, store: ArtifactStore, arguments: Any
) -> dict[str, Any]:
    return _execute_cause_audit_action(
        root=root,
        store=store,
        arguments=arguments,
        historical_v2=False,
    )


def execute_historical_cause_audit_v2_action(
    *, root: Path, store: ArtifactStore, arguments: Any
) -> dict[str, Any]:
    return _execute_cause_audit_action(
        root=root,
        store=store,
        arguments=arguments,
        historical_v2=True,
    )


def _execute_cause_audit_action(
    *,
    root: Path,
    store: ArtifactStore,
    arguments: Any,
    historical_v2: bool,
) -> dict[str, Any]:
    action = str(arguments.action)
    if historical_v2 and action not in {"run", "compare"}:
        raise ContractError(
            "historical-cause-audit-v2 supports only run and compare"
        )
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
    if action == "freeze-open-world-inventory":
        if arguments.base_oracle is None:
            raise ContractError(
                "freeze-open-world-inventory requires --base-oracle"
            )
        return freeze_open_world_arm_inventory(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            base_oracle_path=arguments.base_oracle,
            runtime_run_paths=arguments.runtime_arm,
            partition=arguments.partition,
        )
    if action == "freeze-open-world-candidates":
        if arguments.base_oracle is None:
            raise ContractError(
                "freeze-open-world-candidates requires --base-oracle"
            )
        if arguments.open_world_arm_inventory is None:
            raise ContractError(
                "freeze-open-world-candidates requires --open-world-arm-inventory"
            )
        return freeze_open_world_candidate_set(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            base_oracle_path=arguments.base_oracle,
            arm_inventory_path=arguments.open_world_arm_inventory,
            partition=arguments.partition,
        )
    if action == "finalize-open-world-oracle":
        required = {
            "base-oracle": arguments.base_oracle,
            "open-world-arm-inventory": arguments.open_world_arm_inventory,
            "open-world-candidates": arguments.open_world_candidates,
            "open-world-adjudication": arguments.open_world_adjudication,
            "run-oracle-output": arguments.run_oracle_output,
        }
        missing = [name for name, value in required.items() if value is None]
        if missing:
            raise ContractError(
                "finalize-open-world-oracle requires "
                + ", ".join(f"--{name}" for name in missing)
            )
        return finalize_open_world_oracle(
            root=root,
            store=store,
            manifest_path=manifest,
            cases_path=cases,
            base_oracle_path=arguments.base_oracle,
            arm_inventory_path=arguments.open_world_arm_inventory,
            candidate_set_path=arguments.open_world_candidates,
            adjudication_path=arguments.open_world_adjudication,
            run_oracle_output_path=arguments.run_oracle_output,
            oracle_run_manifest_output_path=arguments.output,
            partition=arguments.partition,
        )
    if action == "run":
        for name in ("acquisition", "stockfish", "sbt"):
            if getattr(arguments, name) is None:
                raise ContractError(f"cause-audit run requires --{name}")
        run_arguments = {
            "root": root,
            "store": store,
            "manifest_path": manifest,
            "cases_path": cases,
            "acquisition_path": arguments.acquisition,
            "stockfish_path": arguments.stockfish,
            "sbt_path": arguments.sbt,
            "adapter_root": arguments.adapter_root or (root / "runtime-adapter"),
            "cache_root": arguments.cache or (root / "tmp" / "cause-audit-cache-v1"),
            "partition": arguments.partition,
            "timeout_seconds": float(arguments.timeout_seconds),
            "provider_timeout_seconds": float(arguments.provider_timeout_seconds),
            "max_probe_rounds": int(arguments.max_probe_rounds),
            "candidate_binding_path": arguments.candidate_binding,
            "requested_case_ids": arguments.case_id,
        }
        if historical_v2:
            return run_historical_cause_audit_v2(**run_arguments)
        if arguments.base_oracle is None:
            raise ContractError("active v3 Cause run requires --base-oracle")
        return run_cause_audit(
            **run_arguments,
            base_oracle_path=arguments.base_oracle,
        )
    if action == "compare":
        if arguments.labels is None or arguments.runtime_run is None:
            raise ContractError(
                "cause-audit compare requires --labels and --runtime-run"
            )
        if historical_v2:
            return compare_historical_cause_audit_v2(
                root=root,
                store=store,
                manifest_path=manifest,
                cases_path=cases,
                labels_path=arguments.labels,
                runtime_run_path=arguments.runtime_run,
                partition=arguments.partition,
                requested_case_ids=arguments.case_id,
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
            oracle_run_manifest_path=arguments.oracle_run_manifest,
            base_oracle_path=arguments.base_oracle,
            candidate_set_path=arguments.open_world_candidates,
            adjudication_path=arguments.open_world_adjudication,
            arm_inventory_path=arguments.open_world_arm_inventory,
        )
    raise ContractError(f"unsupported cause-audit action: {action}")


__all__ = [
    "execute_cause_audit_action",
    "execute_historical_cause_audit_v2_action",
    "freeze_cause_audit",
    "acquire_cause_audit",
    "run_cause_audit",
    "run_historical_cause_audit_v2",
    "compare_cause_audit",
    "compare_historical_cause_audit_v2",
    "freeze_candidate_binding",
]
