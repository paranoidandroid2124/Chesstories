from __future__ import annotations

import copy
import hashlib
import json
import re
from collections import Counter
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import chess

from .canonicalize import Canonicalizer
from .capture import ArtifactStore, utc_now
from .hashing import (
    read_json,
    read_jsonl,
    sha256_file,
    sha256_json,
)
from .model import ContractError, IntegrityError
from .runtime_jsonl_session import (
    _RuntimeExchange,
    _RuntimeJsonlSession,
)
from .schemas import SchemaRegistry
from .stockfish import acquire_stockfish_evidence


FREEZE_SCHEMA_VERSION = "chesstory.eval.cause-audit-freeze.v1"
ACQUISITION_SCHEMA_VERSION = "chesstory.eval.cause-audit-acquisition.v2"
RUNTIME_RUN_SCHEMA_VERSION = "chesstory.eval.cause-audit-runtime-raw-public.v1"
RUNTIME_REQUEST_SCHEMA = "chesstory.move-meaning.request.v1"
CAUSE_ADAPTER_MAIN = (
    "io.chesstory.evaluation.runtimeadapter.RuntimePublicResponseCli"
)
PARTITIONS = ("explore", "sealed_confirm")
SPLIT_SALT = "cause-audit-v1-stratum-seal"


def _checked_runtime_response(
    exchange: _RuntimeExchange,
) -> str:
    """Classify one already schema-validated RuntimeProtocol response."""

    status = exchange.body.get("status")
    if status == "ready":
        if exchange.http_status != 200:
            raise ContractError("runtime ready response must use HTTP 200")
        return status
    if status == "error":
        if not 400 <= exchange.http_status <= 499:
            raise ContractError("runtime error response must use a 4xx HTTP status")
        error = exchange.body.get("error")
        if not isinstance(error, str) or not error:
            raise ContractError("runtime error response error must be non-empty text")
        return status
    raise ContractError(f"runtime public response has unknown status: {status!r}")


def _cause_adapter_command(sbt: Path) -> list[str]:
    return [
        str(sbt),
        "-batch",
        "-error",
        f"runMain {CAUSE_ADAPTER_MAIN}",
    ]


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


def _stratum_balanced_split(
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
    by_stratum: dict[str, list[Mapping[str, Any]]] = {}
    for case in cases:
        by_stratum.setdefault(str(case["stratum_id"]), []).append(case)
    if len(by_stratum) != 8 or any(len(values) != 3 for values in by_stratum.values()):
        raise ContractError(
            "stratum-balanced split requires exactly 8 strata with 3 cases each"
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
    for stratum_id in sorted(by_stratum):
        candidates: list[tuple[str, Mapping[str, Any]]] = []
        for case in by_stratum[stratum_id]:
            case_id = str(case["case_id"])
            digest = hashlib.sha256(
                f"{SPLIT_SALT}|{stratum_id}|{case_id}".encode("ascii")
            ).hexdigest()
            candidates.append((digest, case))
        eligible = [
            item for item in candidates if str(item[1]["case_id"]) not in exclusion_by_id
        ]
        if not eligible:
            raise ContractError(f"all cases in stratum {stratum_id} are contaminated")
        sealed_id = str(min(eligible, key=lambda item: (item[0], str(item[1]["case_id"])))[1]["case_id"])
        for digest, case in candidates:
            case_id = str(case["case_id"])
            logical = "sealed_confirm" if case_id == sealed_id else "explore"
            partitions[logical].append(case_id)
            assignments.append(
                {
                    "case_id": case_id,
                    "stratum_id": stratum_id,
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
        raise ContractError("stratum-balanced split did not produce a 16/8 partition")
    sealed_strata = Counter(
        item["stratum_id"]
        for item in assignments
        if item["logical_partition"] == "sealed_confirm"
    )
    explore_strata = Counter(
        item["stratum_id"]
        for item in assignments
        if item["logical_partition"] == "explore"
    )
    if set(sealed_strata.values()) != {1} or set(explore_strata.values()) != {2}:
        raise ContractError("stratum-balanced split does not preserve 2 explore and 1 sealed per stratum")
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
    partition_case_ids, case_assignments, assignment_hash = _stratum_balanced_split(
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
            "algorithm": "minimum-sha256-per-stratum-after-contamination-exclusions.v1",
            "salt": SPLIT_SALT,
            "input_format": "{salt}|{stratum_id}|{case_id}",
            "unit": "stratum_id",
            "contamination_exclusions": contamination_exclusions,
            "stratum_count": 8,
            "cases_per_stratum": 3,
            "sealed_per_stratum": 1,
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
    actual_partitions, actual_assignments, actual_assignment_hash = _stratum_balanced_split(
        cases, [item for item in exclusions if isinstance(item, Mapping)]
    )
    if manifest["partition_case_ids"] != actual_partitions:
        raise IntegrityError("cause case partitions no longer match the freeze manifest")
    expected_split = {
        "algorithm": "minimum-sha256-per-stratum-after-contamination-exclusions.v1",
        "salt": SPLIT_SALT,
        "input_format": "{salt}|{stratum_id}|{case_id}",
        "unit": "stratum_id",
        "contamination_exclusions": exclusions,
        "stratum_count": 8,
        "cases_per_stratum": 3,
        "sealed_per_stratum": 1,
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


def _selected_cases(
    cases: Sequence[Mapping[str, Any]],
    manifest: Mapping[str, Any],
) -> list[dict[str, Any]]:
    partition_case_ids = _object(
        manifest.get("partition_case_ids"), "freeze partition case IDs"
    )
    selected_ids = partition_case_ids.get("explore")
    if not isinstance(selected_ids, list):
        raise IntegrityError("freeze manifest has no ordered explore case IDs")
    case_by_id = {str(item["case_id"]): item for item in cases}
    selected: list[dict[str, Any]] = []
    for raw_case_id in selected_ids:
        case_id = str(raw_case_id)
        item = case_by_id.get(case_id)
        if item is None:
            raise IntegrityError(f"freeze manifest references an unknown explore case: {case_id}")
        value = dict(item)
        value["embedded_partition"] = value["partition"]
        value["partition"] = "explore"
        selected.append(value)
    if len({str(item["case_id"]) for item in selected}) != len(selected):
        raise IntegrityError("freeze manifest has duplicate explore case IDs")
    return selected


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
    selected = _selected_cases(cases, manifest)
    if not selected:
        raise ContractError("selected cause audit partition is empty")
    executable = stockfish_path.resolve(strict=True)
    engine_sha256 = sha256_file(executable)
    config = _acquisition_config(arguments)
    rows: list[dict[str, Any]] = []
    for case in selected:
        case_id = str(case["case_id"])
        question = _question(case, label_by_case.get(case_id))
        result = acquire_stockfish_evidence(
            executable,
            question,
            config,
            timeout_seconds=float(arguments.timeout_seconds),
        )
        pack = _object(result.engine_evidence_pack, "engine evidence pack")
        diagnostics = _object(result.diagnostics, "engine diagnostics")
        raw_provider_io = _object(result.raw_provider_io, "raw engine I/O")
        if pack.get("question") != question:
            raise IntegrityError(f"Stockfish pack question binding failed for {case_id}")
        if pack.get("acquisition_config") != config:
            raise IntegrityError(f"Stockfish pack config binding failed for {case_id}")
        capture_hashes = store.capture_stage(
            sample_id=case_id,
            arm_id="cause-audit-root",
            stage="Q",
            raw_input={"question": question, "acquisition_config": config},
            raw_output={
                "engine_evidence_pack": pack,
                "diagnostics": diagnostics,
            },
            validated_output={"engine_evidence_pack": pack, "diagnostics": diagnostics},
            canonical_output={"engine_evidence_pack": pack, "diagnostics": diagnostics},
            provider_io=raw_provider_io,
            metadata={
                "source": "cause-audit-stockfish-acquisition",
                "engine_sha256": engine_sha256,
                "question_sha256": sha256_json(question),
                "acquisition_config_sha256": sha256_json(config),
                "engine_evidence_pack_sha256": sha256_json(pack),
                "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
                "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
                "case_id": case_id,
                "partition": str(case["partition"]),
            },
        )
        rows.append(
            {
                "case_id": case_id,
                "partition": str(case["partition"]),
                "question_sha256": sha256_json(question),
                "engine_evidence_pack_sha256": sha256_json(pack),
                "diagnostics": diagnostics,
                "capture_hashes": dict(capture_hashes),
            }
        )
    document = {
        "schema_version": ACQUISITION_SCHEMA_VERSION,
        "run_id": store.run_id,
        "partition": "explore",
        "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "reference_labels_file_sha256": reference_label_hash,
        "engine_sha256": engine_sha256,
        "acquisition_config": config,
        "case_count": len(rows),
        "rows": rows,
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    _assert_ascii(document, "cause audit acquisition report")
    store.capture_run_document("cause-audit-acquisition", document)
    store.verify()
    return document


def _acquisition_rows(
    *,
    acquisition: Mapping[str, Any],
    source_store: ArtifactStore,
    selected: Sequence[Mapping[str, Any]],
    manifest_path: Path,
    cases_path: Path,
    engine_sha256: str,
) -> dict[str, tuple[dict[str, Any], dict[str, Any]]]:
    required_document_fields = {
        "schema_version",
        "run_id",
        "partition",
        "freeze_manifest_sha256",
        "cases_file_sha256",
        "reference_labels_file_sha256",
        "engine_sha256",
        "acquisition_config",
        "case_count",
        "rows",
        "artifact_ledger_hash_before_report",
    }
    if set(acquisition) != required_document_fields:
        raise ContractError("cause audit acquisition document fields are not exact")
    if acquisition.get("schema_version") != ACQUISITION_SCHEMA_VERSION:
        raise ContractError("unsupported cause audit acquisition schema")
    if acquisition.get("run_id") != source_store.run_id:
        raise IntegrityError("cause audit acquisition run ID binding failed")
    if acquisition.get("partition") != "explore":
        raise IntegrityError("cause audit acquisition partition is not explore")
    if acquisition.get("freeze_manifest_sha256") != sha256_file(
        manifest_path.resolve(strict=True)
    ):
        raise IntegrityError("acquisition freeze manifest binding failed")
    if acquisition.get("cases_file_sha256") != sha256_file(
        cases_path.resolve(strict=True)
    ):
        raise IntegrityError("acquisition cases binding failed")
    if acquisition.get("engine_sha256") != engine_sha256:
        raise IntegrityError("acquisition Stockfish binding failed")
    config = _object(
        acquisition.get("acquisition_config"), "cause audit acquisition config"
    )
    rows = acquisition.get("rows")
    if not isinstance(rows, list):
        raise ContractError("cause audit acquisition has no rows")
    selected_ids = [str(item["case_id"]) for item in selected]
    row_ids = [
        str(_object(raw, "cause audit acquisition row").get("case_id", ""))
        for raw in rows
    ]
    if acquisition.get("case_count") != len(selected) or row_ids != selected_ids:
        raise IntegrityError("cause audit acquisition case set is not the frozen explore order")
    captured_path = source_store.run_root / "cause-audit-acquisition.json"
    if captured_path.resolve(strict=True) != captured_path:
        raise IntegrityError("captured acquisition document escaped its artifact run")
    captured = _read_object(captured_path, "captured cause audit acquisition")
    if captured != acquisition:
        raise IntegrityError("external acquisition differs from the captured acquisition document")
    try:
        ledger_rows = read_jsonl(source_store.ledger_path.resolve(strict=True))
    except ValueError as error:
        raise IntegrityError("source acquisition ledger is malformed") from error
    ledger = [_object(item, "source acquisition ledger event") for item in ledger_rows]
    captured_hash = sha256_file(captured_path)
    capture_document_events = [
        (index, event)
        for index, event in enumerate(ledger)
        if event.get("event") == "run-document"
        and event.get("run_id") == source_store.run_id
        and event.get("artifact_path") == "cause-audit-acquisition.json"
    ]
    if (
        len(capture_document_events) != 1
        or capture_document_events[0][1].get("artifact_hashes")
        != {"cause-audit-acquisition.json": captured_hash}
    ):
        raise IntegrityError("source store has no unique captured acquisition document")
    capture_document_index, capture_document_event = capture_document_events[0]
    capture_sequence = capture_document_event.get("sequence")
    if (
        isinstance(capture_sequence, bool)
        or not isinstance(capture_sequence, int)
        or capture_sequence < 1
    ):
        raise IntegrityError("captured acquisition ledger sequence is invalid")
    ledger_bytes = source_store.ledger_path.read_bytes()
    ledger_prefix = b"".join(
        ledger_bytes.splitlines(keepends=True)[: capture_sequence - 1]
    )
    if acquisition.get("artifact_ledger_hash_before_report") != hashlib.sha256(
        ledger_prefix
    ).hexdigest():
        raise IntegrityError("captured acquisition report ledger binding failed")

    required_row_fields = {
        "case_id",
        "partition",
        "question_sha256",
        "engine_evidence_pack_sha256",
        "diagnostics",
        "capture_hashes",
    }
    required_capture_files = {
        "raw-input.json",
        "raw-output.json",
        "validated-output.json",
        "canonical-output.json",
        "raw-provider-io.json",
        "metadata.json",
    }
    result: dict[str, tuple[dict[str, Any], dict[str, Any]]] = {}
    for case, raw in zip(selected, rows, strict=True):
        row = _object(raw, "cause audit acquisition row")
        case_id = str(case["case_id"])
        if set(row) != required_row_fields or row.get("case_id") != case_id:
            raise IntegrityError("cause audit acquisition row fields or order changed")
        if row.get("partition") != case["partition"] or row.get("partition") != "explore":
            raise IntegrityError(f"acquisition row {case_id} partition binding failed")
        capture_hashes = _object(
            row.get("capture_hashes"), f"acquisition row {case_id} capture hashes"
        )
        if set(capture_hashes) != required_capture_files:
            raise IntegrityError(f"acquisition row {case_id} capture files are not exact")
        if not all(
            isinstance(digest, str) and re.fullmatch(r"[0-9a-f]{64}", digest)
            for digest in capture_hashes.values()
        ):
            raise ContractError(f"acquisition row {case_id} capture hash is invalid")

        directory = (
            source_store.run_root
            / "samples"
            / ArtifactStore._segment(case_id)
            / "cause-audit-root"
            / "q"
        )
        if directory.resolve(strict=True) != directory:
            raise IntegrityError(f"acquisition capture path escaped its artifact run: {case_id}")
        relative_directory = directory.relative_to(source_store.run_root).as_posix()
        stage_events = [
            (index, event)
            for index, event in enumerate(ledger)
            if event.get("event") == "stage-capture"
            and event.get("run_id") == source_store.run_id
            and event.get("sample_id") == case_id
            and event.get("arm_id") == "cause-audit-root"
            and event.get("stage") == "Q"
            and event.get("artifact_path") == relative_directory
        ]
        if (
            len(stage_events) != 1
            or stage_events[0][1].get("artifact_hashes") != capture_hashes
            or stage_events[0][0] >= capture_document_index
        ):
            raise IntegrityError(f"acquisition row {case_id} has no unique Q stage capture")

        raw_input = _read_object(
            directory / "raw-input.json", f"acquisition raw input for {case_id}"
        )
        raw_output = _read_object(
            directory / "raw-output.json", f"acquisition raw output for {case_id}"
        )
        validated_output = _read_object(
            directory / "validated-output.json",
            f"acquisition validated output for {case_id}",
        )
        canonical_output = _read_object(
            directory / "canonical-output.json",
            f"acquisition canonical output for {case_id}",
        )
        metadata = _read_object(
            directory / "metadata.json", f"acquisition metadata for {case_id}"
        )
        if set(raw_input) != {"question", "acquisition_config"}:
            raise IntegrityError(f"acquisition raw input fields changed for {case_id}")
        question = _object(raw_input.get("question"), f"acquisition question for {case_id}")
        expected_question = _question(case, None)
        if set(question) != set(expected_question):
            raise IntegrityError(f"acquisition question fields changed for {case_id}")
        for field in (
            "root_position",
            "played_move_uci",
            "move_history_uci",
            "game_phase",
        ):
            if question.get(field) != expected_question[field]:
                raise IntegrityError(f"acquisition question case binding failed for {case_id}")
        references = question.get("requested_reference_moves_uci")
        if not isinstance(references, list) or not all(
            isinstance(move, str) and move for move in references
        ):
            raise ContractError(f"acquisition reference moves are invalid for {case_id}")
        if raw_input.get("acquisition_config") != config:
            raise IntegrityError(f"acquisition config binding failed for {case_id}")
        if row.get("question_sha256") != sha256_json(question):
            raise IntegrityError(f"acquisition question hash binding failed for {case_id}")
        if set(raw_output) != {"engine_evidence_pack", "diagnostics"}:
            raise IntegrityError(f"acquisition raw output fields changed for {case_id}")
        pack = _object(
            raw_output.get("engine_evidence_pack"),
            f"acquisition engine pack for {case_id}",
        )
        if pack.get("question") != question:
            raise IntegrityError(f"acquisition pack question binding failed for {case_id}")
        if pack.get("acquisition_config") != config:
            raise IntegrityError(f"acquisition pack config binding failed for {case_id}")
        diagnostics = _object(
            raw_output.get("diagnostics"), f"acquisition diagnostics for {case_id}"
        )
        expected_output = {
            "engine_evidence_pack": pack,
            "diagnostics": diagnostics,
        }
        if validated_output != expected_output or canonical_output != expected_output:
            raise IntegrityError(f"acquisition output binding failed for {case_id}")
        if row.get("engine_evidence_pack_sha256") != sha256_json(pack):
            raise IntegrityError(f"acquisition engine pack hash binding failed for {case_id}")
        if row.get("diagnostics") != diagnostics:
            raise IntegrityError(f"acquisition diagnostics binding failed for {case_id}")
        expected_metadata = {
            "source": "cause-audit-stockfish-acquisition",
            "engine_sha256": engine_sha256,
            "question_sha256": sha256_json(question),
            "acquisition_config_sha256": sha256_json(config),
            "engine_evidence_pack_sha256": sha256_json(pack),
            "freeze_manifest_sha256": acquisition["freeze_manifest_sha256"],
            "cases_file_sha256": acquisition["cases_file_sha256"],
            "case_id": case_id,
            "partition": "explore",
        }
        required_metadata_fields = {
            *expected_metadata,
            "schema_version",
            "run_id",
            "sample_id",
            "arm_id",
            "stage",
            "captured_at",
            "files",
        }
        if set(metadata) != required_metadata_fields:
            raise IntegrityError(f"acquisition metadata fields changed for {case_id}")
        if any(metadata.get(key) != value for key, value in expected_metadata.items()):
            raise IntegrityError(f"acquisition metadata binding failed for {case_id}")
        if (
            metadata.get("schema_version") != "chesstory.eval.artifact-metadata.v1"
            or metadata.get("run_id") != source_store.run_id
            or metadata.get("sample_id") != case_id
            or metadata.get("arm_id") != "cause-audit-root"
            or metadata.get("stage") != "Q"
        ):
            raise IntegrityError(f"acquisition capture identity failed for {case_id}")
        metadata_files = {
            name: digest
            for name, digest in capture_hashes.items()
            if name != "metadata.json"
        }
        if metadata.get("files") != metadata_files:
            raise IntegrityError(f"acquisition metadata file binding failed for {case_id}")
        result[case_id] = (row, pack)

    return result



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
    provider_timeout_seconds: float,
) -> dict[str, Any]:
    """Capture one terminal public RuntimeProtocol response per frozen case."""

    registry, canonicalizer = _schema_tools(root)
    manifest, cases = _verify_manifest(
        root=root,
        manifest_path=manifest_path,
        cases_path=cases_path,
        registry=registry,
        canonicalizer=canonicalizer,
    )
    selected = _selected_cases(cases, manifest)
    acquisition = _read_object(acquisition_path, "cause audit acquisition")
    acquisition_run_id = acquisition.get("run_id")
    if (
        not isinstance(acquisition_run_id, str)
        or ArtifactStore._segment(acquisition_run_id) != acquisition_run_id
    ):
        raise ContractError("acquisition run ID is not an exact artifact segment")
    source_run_root = store.root / acquisition_run_id
    if not source_run_root.is_dir():
        raise IntegrityError("acquisition artifact run directory is unavailable")
    if source_run_root.resolve(strict=True) != source_run_root:
        raise IntegrityError("acquisition artifact run directory escaped --artifacts")
    source_store = ArtifactStore(store.root, acquisition_run_id)
    if source_store.run_root.resolve(strict=True) != source_run_root.resolve(strict=True):
        raise IntegrityError("acquisition ArtifactStore path binding failed")
    source_store.verify()
    executable = stockfish_path.resolve(strict=True)
    executable_sha256 = sha256_file(executable)
    rows = _acquisition_rows(
        acquisition=acquisition,
        source_store=source_store,
        selected=selected,
        manifest_path=manifest_path,
        cases_path=cases_path,
        engine_sha256=executable_sha256,
    )
    acquisition_capture_path = source_store.run_root / "cause-audit-acquisition.json"
    acquisition_capture_sha256 = sha256_file(acquisition_capture_path)
    sbt = sbt_path.resolve(strict=True)
    adapter_directory = adapter_root.resolve(strict=True)
    if not adapter_directory.is_dir():
        raise ContractError("cause audit adapter root is not a directory")

    case_records: list[dict[str, Any]] = []
    with _RuntimeJsonlSession(
        _cause_adapter_command(sbt),
        cwd=adapter_directory,
        timeout_seconds=provider_timeout_seconds,
    ) as session:
        for case in selected:
            case_id = str(case["case_id"])
            _, pack = rows[case_id]
            request = _runtime_request(case, pack)
            exchange = session.invoke(request)
            status = _checked_runtime_response(exchange)
            response_record = {
                "request_jsonl_base64": exchange.request_jsonl_base64,
                "request_jsonl_sha256": exchange.request_jsonl_sha256,
                "request_jsonl_length": exchange.request_jsonl_length,
                "response_jsonl_base64": exchange.response_jsonl_base64,
                "response_jsonl_sha256": exchange.response_jsonl_sha256,
                "response_jsonl_length": exchange.response_jsonl_length,
                "http_status": exchange.http_status,
                "status": status,
            }
            stop_reason = "ready"
            if status == "error":
                response_record["runtime_error"] = exchange.body["error"]
                stop_reason = f"runtime_error:{exchange.body['error']}"
            case_records.append(
                {
                    "case_id": case_id,
                    "stop_reason": stop_reason,
                    "response": response_record,
                }
            )

    document = {
        "schema_version": RUNTIME_RUN_SCHEMA_VERSION,
        "run_id": store.run_id,
        "partition": "explore",
        "freeze_manifest_sha256": sha256_file(manifest_path.resolve(strict=True)),
        "cases_file_sha256": sha256_file(cases_path.resolve(strict=True)),
        "acquisition": {
            "run_id": source_store.run_id,
            "capture_sha256": acquisition_capture_sha256,
        },
        "case_count": len(case_records),
        "cases": case_records,
        "artifact_ledger_hash_before_report": store.ledger_hash(),
    }
    store.capture_run_document("cause-audit-runtime-run", document)
    store.verify()
    return document

def execute_cause_audit_action(
    *,
    root: Path,
    store: ArtifactStore,
    arguments: Any,
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
            arguments=arguments,
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
            provider_timeout_seconds=float(arguments.provider_timeout_seconds),
        )
    raise ContractError(f"unsupported cause-audit action: {action}")


__all__ = [
    "execute_cause_audit_action",
    "freeze_cause_audit",
    "acquire_cause_audit",
    "run_cause_audit",
]
