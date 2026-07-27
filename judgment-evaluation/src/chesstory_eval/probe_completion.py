from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import os
import queue
import re
import subprocess
import threading
import time
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import chess

from .hashing import json_bytes, sha256_bytes, sha256_file, sha256_json, slug
from .model import ContractError, IntegrityError
from .native_diagnostic import native_canonical_json
from .stockfish import (
    StockfishAcquisitionError,
    _UciSession,
    _clear_hash,
    _configure,
    _handshake,
    _parse_bestmove,
    _parse_info,
    _ready,
    _replay,
    _resolve_options,
)


PROBE_COMPLETION_SCHEMA = "chesstory.eval.runtime-probe-completion.v1"
PROBE_RESULT_SCHEMA = "chesstory.eval.runtime-probe-result.v1"
RUNTIME_IO_SCHEMA = "chesstory.eval.runtime-probe-jsonl-io.v1"
UCI_IO_SCHEMA = "chesstory.eval.runtime-probe-uci-io.v1"
REPORT_SCHEMA = "chesstory.eval.runtime-probe-completion-report.v1"
LEDGER_SCHEMA = "chesstory.eval.runtime-probe-artifact-ledger.v1"
RUNTIME_REQUEST_SCHEMA = "chesstory.move-meaning.request.v1"
RUNTIME_OBSERVATION_SCHEMA = "chesstory.runtime-observation.v2"
REPLY_MULTIPV = "reply_multipv"
MAX_RUNTIME_PROBE_RESULTS = 12
_MOVE_RE = re.compile(r"^[a-h][1-8][a-h][1-8][qrbn]?")


def _canonical_line(value: Any) -> bytes:
    return (native_canonical_json(value) + "\n").encode("utf-8")


def _json_clone(value: Any, label: str) -> Any:
    try:
        return json.loads(
            json.dumps(
                value,
                ensure_ascii=False,
                sort_keys=True,
                separators=(",", ":"),
                allow_nan=False,
            )
        )
    except (TypeError, ValueError) as error:
        raise ContractError(f"{label} must be finite JSON") from error


def _mapping(value: Any, label: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object")
    return dict(_json_clone(value, label))


def _safe_text(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip() or "\r" in value or "\n" in value:
        raise ContractError(f"{label} must be non-empty single-line text")
    return value


def _integer(value: Any, label: str, minimum: int, maximum: int) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or not minimum <= value <= maximum:
        raise ContractError(f"{label} must be an integer from {minimum} through {maximum}")
    return value


def _move(value: Any, label: str) -> str:
    text = _safe_text(value, label).lower()
    if _MOVE_RE.fullmatch(text) is None:
        raise ContractError(f"{label} must be a UCI move")
    return text


class _ArtifactCapture:
    """Small immutable writer for this standalone diagnostic run."""

    def __init__(self, root: Path, run_id: str) -> None:
        self.run_id = _safe_text(run_id, "run id")
        self.run_root = root.resolve() / slug(run_id)
        if self.run_root.exists():
            raise IntegrityError(f"probe completion run already exists: {self.run_root}")
        self.run_root.mkdir(parents=True)
        self._entries: list[dict[str, Any]] = []

    def write(self, relative: str, document: Mapping[str, Any]) -> dict[str, str]:
        clean = Path(relative)
        if clean.is_absolute() or ".." in clean.parts or clean.suffix != ".json":
            raise IntegrityError(f"unsafe probe artifact path: {relative!r}")
        path = self.run_root / clean
        payload = json_bytes(_mapping(document, "artifact document"))
        path.parent.mkdir(parents=True, exist_ok=True)
        try:
            with path.open("xb") as handle:
                handle.write(payload)
                handle.flush()
                os.fsync(handle.fileno())
        except FileExistsError as error:
            raise IntegrityError(f"probe artifact already exists: {path}") from error
        digest = sha256_bytes(payload)
        self._entries.append(
            {
                "path": clean.as_posix(),
                "sha256": digest,
                "size_bytes": len(payload),
            }
        )
        return {"path": clean.as_posix(), "sha256": digest}

    def finish(self) -> dict[str, Any]:
        previous: str | None = None
        records: list[dict[str, Any]] = []
        for sequence, entry in enumerate(self._entries, 1):
            unsigned = {
                "schema_version": LEDGER_SCHEMA,
                "run_id": self.run_id,
                "sequence": sequence,
                "previous_record_sha256": previous,
                **entry,
            }
            record = {**unsigned, "record_sha256": sha256_json(unsigned)}
            records.append(record)
            previous = record["record_sha256"]
        ledger_path = self.run_root / "artifact-ledger.jsonl"
        ledger_bytes = b"".join(json_bytes(record) for record in records)
        with ledger_path.open("xb") as handle:
            handle.write(ledger_bytes)
            handle.flush()
            os.fsync(handle.fileno())
        return {
            "path": "artifact-ledger.jsonl",
            "sha256": sha256_bytes(ledger_bytes),
            "records": len(records),
            "chain_head_sha256": previous,
        }


@dataclass(frozen=True)
class _RuntimeExchange:
    observation: Mapping[str, Any]
    request_jsonl_sha256: str
    response_jsonl_sha256: str
    diagnostic_line_sha256: tuple[str, ...]
    diagnostic_stream_sha256: str


class _RuntimeJsonlSession:
    """Persistent sbt JSONL process with byte-exact transport hashes."""

    def __init__(
        self,
        command: Sequence[str],
        *,
        cwd: Path,
        timeout_seconds: float,
        expected_schema: str = RUNTIME_OBSERVATION_SCHEMA,
    ) -> None:
        if isinstance(command, (str, bytes)) or not command:
            raise ContractError("runtime command must be a non-empty argument array")
        self.command = tuple(_safe_text(item, "runtime command argument") for item in command)
        self.cwd = cwd.resolve(strict=True)
        if not self.cwd.is_dir():
            raise ContractError("runtime adapter working directory must be a directory")
        if not math.isfinite(timeout_seconds) or not 0.001 <= timeout_seconds <= 3600.0:
            raise ContractError("runtime timeout must be from 0.001 through 3600 seconds")
        self.timeout_seconds = float(timeout_seconds)
        self.expected_schema = _safe_text(expected_schema, "runtime observation schema")
        self._process: subprocess.Popen[bytes] | None = None
        self._reader: threading.Thread | None = None
        self._output: queue.Queue[tuple[str, bytes | BaseException]] = queue.Queue()

    def __enter__(self) -> _RuntimeJsonlSession:
        self._start()
        return self

    def __exit__(self, exc_type: Any, exc: Any, traceback: Any) -> None:
        self.close()

    def _start(self) -> subprocess.Popen[bytes]:
        if self._process is not None:
            if self._process.poll() is not None:
                raise ContractError("runtime adapter process ended unexpectedly")
            return self._process
        try:
            self._process = subprocess.Popen(
                list(self.command),
                cwd=self.cwd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                creationflags=(
                    getattr(subprocess, "CREATE_NEW_PROCESS_GROUP", 0)
                    if os.name == "nt"
                    else 0
                ),
                start_new_session=os.name != "nt",
            )
        except OSError as error:
            raise ContractError(f"runtime adapter could not start: {error}") from error
        self._reader = threading.Thread(
            target=self._read_output,
            name="chesstory-probe-runtime-reader",
            daemon=True,
        )
        self._reader.start()
        return self._process

    def _read_output(self) -> None:
        process = self._process
        if process is None or process.stdout is None:
            self._output.put(("error", RuntimeError("runtime stdout is unavailable")))
            return
        try:
            for raw in iter(process.stdout.readline, b""):
                self._output.put(("line", raw))
        except BaseException as error:
            self._output.put(("error", error))
        finally:
            self._output.put(("eof", b""))

    def invoke(self, request: Mapping[str, Any]) -> _RuntimeExchange:
        process = self._start()
        if process.stdin is None:
            raise ContractError("runtime stdin is unavailable")
        request_bytes = _canonical_line(request)
        try:
            process.stdin.write(request_bytes)
            process.stdin.flush()
        except (BrokenPipeError, OSError) as error:
            raise ContractError("runtime adapter closed its input") from error
        diagnostics: list[bytes] = []
        deadline = time.monotonic() + self.timeout_seconds
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise ContractError("runtime adapter response timed out")
            try:
                kind, value = self._output.get(timeout=remaining)
            except queue.Empty as error:
                raise ContractError("runtime adapter response timed out") from error
            if kind == "error":
                assert isinstance(value, BaseException)
                raise ContractError("runtime adapter output could not be read") from value
            if kind == "eof":
                raise ContractError(
                    f"runtime adapter ended before a JSON response; exit={process.poll()}"
                )
            assert isinstance(value, bytes)
            stripped = value.strip()
            if not stripped.startswith(b"{"):
                if stripped:
                    diagnostics.append(value)
                continue
            try:
                parsed = json.loads(stripped.decode("utf-8"))
            except (UnicodeError, json.JSONDecodeError) as error:
                raise ContractError("runtime adapter emitted malformed UTF-8 JSON") from error
            observation = _mapping(parsed, "runtime observation")
            if observation.get("schema_version") != self.expected_schema:
                raise ContractError("runtime adapter emitted an unexpected observation schema")
            diagnostic_stream = b"".join(diagnostics)
            return _RuntimeExchange(
                observation=observation,
                request_jsonl_sha256=sha256_bytes(request_bytes),
                response_jsonl_sha256=sha256_bytes(value),
                diagnostic_line_sha256=tuple(sha256_bytes(line) for line in diagnostics),
                diagnostic_stream_sha256=sha256_bytes(diagnostic_stream),
            )

    def close(self) -> None:
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
            process.wait(timeout=30)
        except subprocess.TimeoutExpired:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)
        if self._reader is not None:
            self._reader.join(timeout=2)


def _load_runtime_request(path: Path) -> dict[str, Any]:
    try:
        document = json.loads(path.read_text(encoding="utf-8-sig"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise ContractError(f"runtime request source is invalid JSON: {path}") from error
    root = _mapping(document, "runtime request source")
    candidates: list[Any] = [root, root.get("runtime_request")]
    invocation = root.get("invocation")
    if isinstance(invocation, Mapping):
        candidates.append(invocation.get("runtime_request"))
    for candidate in candidates:
        if isinstance(candidate, Mapping) and candidate.get("schema_version") == RUNTIME_REQUEST_SCHEMA:
            request = _mapping(candidate, "runtime request")
            if not isinstance(request.get("input"), Mapping):
                raise ContractError("runtime request input must be an object")
            _safe_text(request.get("request_id"), "runtime request id")
            return request
    raise ContractError(f"no runtime request.v1 envelope was found in {path}")


def _public_body(observation: Mapping[str, Any]) -> dict[str, Any]:
    response = observation.get("public_response")
    if not isinstance(response, Mapping):
        raise ContractError("runtime observation has no public response")
    body = response.get("body")
    if not isinstance(body, Mapping):
        raise ContractError("runtime public response has no body")
    return dict(body)


def _probe_requests(observation: Mapping[str, Any]) -> list[dict[str, Any]]:
    value = _public_body(observation).get("probe_requests")
    if not isinstance(value, list) or not all(isinstance(item, Mapping) for item in value):
        raise ContractError("runtime probe_requests must be an object array")
    requests = [_mapping(item, "runtime probe request") for item in value]
    ids = [_safe_text(item.get("id"), "probe request id") for item in requests]
    if len(set(ids)) != len(ids):
        raise ContractError("runtime emitted duplicate probe request ids")
    return requests


def _validate_probe_request(request: Mapping[str, Any]) -> dict[str, Any]:
    value = _mapping(request, "probe request")
    if value.get("purpose") != REPLY_MULTIPV:
        raise ContractError(f"unsupported runtime probe purpose: {value.get('purpose')!r}")
    _safe_text(value.get("id"), "probe request id")
    fen = _safe_text(value.get("fen"), "probe request FEN")
    try:
        board = chess.Board(fen)
    except ValueError as error:
        raise ContractError("probe request FEN is invalid") from error
    if board.status() != chess.STATUS_VALID:
        raise ContractError(f"probe request FEN is illegal (status={int(board.status())})")
    _integer(value.get("depth"), "probe request depth", 1, 100)
    _integer(value.get("multiPv"), "probe request multiPv", 1, 8)
    _move(value.get("candidateMove"), "probe request candidateMove")
    variation_hash = _safe_text(value.get("variationHash"), "probe request variationHash")
    if re.fullmatch(r"[0-9a-f]{64}", variation_hash) is None:
        raise ContractError("probe request variationHash must be lowercase SHA-256")
    raw_moves = value.get("moves")
    if not isinstance(raw_moves, list):
        raise ContractError("probe request moves must be an array")
    moves = [_move(item, "probe request move") for item in raw_moves]
    resource = value.get("opponentResourceMove")
    if resource is not None:
        resource = _move(resource, "probe request opponentResourceMove")
        if moves != [resource] or value["multiPv"] != 1 or value.get("horizon") is not None:
            raise ContractError("opponent-resource probe binding is not exact")
    elif moves:
        raise ContractError("unforced reply_multipv probe must not carry request moves")
    return value


def _stream_hash(lines: Sequence[str]) -> str:
    return sha256_bytes("".join(f"{line}\n" for line in lines).encode("utf-8"))


def _search_probe(
    executable: Path,
    request: Mapping[str, Any],
    *,
    timeout_seconds: float,
    executable_sha256: str,
) -> tuple[dict[str, Any], dict[str, Any]]:
    probe = _validate_probe_request(request)
    branch_board = chess.Board(str(probe["fen"]))
    resource = probe.get("opponentResourceMove")
    search_board = branch_board.copy(stack=False)
    prefix: list[str] = []
    if resource is not None:
        resource_move = chess.Move.from_uci(str(resource))
        if not search_board.is_legal(resource_move):
            raise ContractError(f"opponent resource is illegal at requested FEN: {resource}")
        search_board.push(resource_move)
        prefix.append(str(resource))
    depth = int(probe["depth"])
    multipv = int(probe["multiPv"])
    expected_lines = min(multipv, search_board.legal_moves.count())
    if expected_lines < 1:
        raise ContractError("reply_multipv probe reached a terminal search position")

    session: _UciSession | None = None
    identity: Any = None
    applied_options: Mapping[str, Any] = {}
    clear_hash_name: str | None = None
    failure: BaseException | None = None
    selected: list[Any] = []
    try:
        session = _UciSession(executable)
        identity = _handshake(
            session,
            timeout_seconds=timeout_seconds,
            executable_sha256=executable_sha256,
        )
        applied_options, clear_hash_name = _resolve_options(
            identity,
            {"Threads": 1, "Hash": 16},
            multipv,
        )
        _configure(session, applied_options, timeout_seconds=timeout_seconds)
        session.send("ucinewgame")
        _ready(session, timeout_seconds, "new-game readiness")
        _clear_hash(session, clear_hash_name, timeout_seconds)
        position = f"position fen {probe['fen']}"
        if prefix:
            position += " moves " + " ".join(prefix)
        session.send(position)
        session.send(f"go depth {depth}")
        output = session.read_until(
            lambda line: line.startswith("bestmove "),
            timeout_seconds=timeout_seconds,
            context=f"probe bestmove at depth {depth}",
        )
        bestmove, ponder = _parse_bestmove(output[-1])
        if bestmove in {"0000", "(none)"}:
            raise StockfishAcquisitionError("non-terminal probe returned no bestmove")
        _replay(search_board, (bestmove,) + ((ponder,) if ponder else ()))
        infos: dict[int, list[Any]] = {}
        for line in output[:-1]:
            info = _parse_info(line)
            if info is None:
                continue
            _replay(search_board, info.moves)
            infos.setdefault(info.multipv_index, []).append(info)
        for index in range(1, expected_lines + 1):
            candidates = [
                info
                for info in infos.get(index, [])
                if info.depth == depth and info.score_bound is None
            ]
            if not candidates:
                raise StockfishAcquisitionError(
                    f"probe lacks exact MultiPV {index} at depth {depth}"
                )
            selected.append(candidates[-1])
        if selected[0].moves[0] != bestmove:
            raise StockfishAcquisitionError("probe bestmove disagrees with MultiPV 1")
    except BaseException as error:
        failure = error
    finally:
        if session is not None:
            session.close()
    if session is None:
        assert failure is not None
        raise failure
    raw = session.raw()
    commands = [str(item["line"]) for item in raw["commands"] if "line" in item]
    stdout = [str(item["line"]) for item in raw["stdout"] if "line" in item]
    stderr = [str(item["line"]) for item in raw["stderr"] if "line" in item]
    io_document = {
        "schema_version": UCI_IO_SCHEMA,
        "probe_id": probe["id"],
        "engine": {
            "executable_sha256": executable_sha256,
            "name": getattr(identity, "name", None),
            "version": getattr(identity, "version", None),
            "options": dict(applied_options),
        },
        "search": {
            "request_fen": probe["fen"],
            "forced_prefix_uci": prefix,
            "depth": depth,
            "multipv": multipv,
            "score_reported_by_engine_for": (
                "white" if search_board.turn == chess.WHITE else "black"
            ),
            "score_returned_to_runtime_as": "white",
        },
        "commands": commands,
        "stdout": stdout,
        "stderr": stderr,
        "command_stream_sha256": _stream_hash(commands),
        "stdout_stream_sha256": _stream_hash(stdout),
        "stderr_stream_sha256": _stream_hash(stderr),
        "return_code": raw["returncode"],
    }
    if failure is not None:
        if isinstance(failure, Exception):
            raise failure
        raise RuntimeError("probe engine failed with a non-standard exception")
    if raw["returncode"] != 0:
        raise StockfishAcquisitionError(f"Stockfish exited with status {raw['returncode']}")

    perspective = 1 if search_board.turn == chess.WHITE else -1
    reply_lines: list[dict[str, Any]] = []
    for info in selected:
        score_cp: int
        mate: int | None
        if info.score_kind == "cp":
            score_cp = perspective * int(info.score_value)
            mate = None
        else:
            mate = perspective * int(info.score_value)
            if mate == 0:
                raise StockfishAcquisitionError("runtime probe cannot encode mate zero")
            score_cp = 100000 if mate > 0 else -100000
        reply_lines.append(
            {
                "moves": prefix + list(info.moves),
                "scoreCp": score_cp,
                "mate": mate,
                "depth": depth,
            }
        )
    result: dict[str, Any] = {
        "id": probe["id"],
        "fen": probe["fen"],
        "replyLines": reply_lines,
        "purpose": probe["purpose"],
        "probedMove": probe["candidateMove"],
        "depth": depth,
        "candidateMove": probe["candidateMove"],
        "opponentResourceMove": resource,
        "variationHash": probe["variationHash"],
    }
    if probe.get("horizon") is not None:
        result["horizon"] = probe["horizon"]
    return result, io_document


def _walk(value: Any):
    yield value
    if isinstance(value, Mapping):
        for child in value.values():
            yield from _walk(child)
    elif isinstance(value, list):
        for child in value:
            yield from _walk(child)


def _products(tree: Any, constructor: str) -> list[Mapping[str, Any]]:
    return [
        value
        for value in _walk(tree)
        if isinstance(value, Mapping) and value.get("constructor") == constructor
    ]


def _field(product: Mapping[str, Any], name: str) -> Any:
    fields = product.get("fields")
    if not isinstance(fields, list):
        return None
    for item in fields:
        if isinstance(item, Mapping) and item.get("name") == name:
            return item.get("value")
    return None


def _native_value(node: Any) -> Any:
    if not isinstance(node, Mapping):
        if isinstance(node, list):
            return [_native_value(item) for item in node]
        return node
    kind = node.get("node_kind")
    if kind in {"string", "boolean"}:
        return node.get("value")
    if kind == "number":
        raw = node.get("value")
        try:
            return int(raw)
        except (TypeError, ValueError):
            try:
                return float(raw)
            except (TypeError, ValueError):
                return raw
    if kind == "option":
        return None if node.get("variant") == "None" else _native_value(node.get("value"))
    if kind == "sequence":
        values = node.get("values")
        return [_native_value(value) for value in values] if isinstance(values, list) else []
    if kind == "product":
        fields = node.get("fields")
        if not fields:
            return node.get("constructor")
        return {
            str(item.get("name")): _native_value(item.get("value"))
            for item in fields
            if isinstance(item, Mapping) and isinstance(item.get("name"), str)
        }
    return node.get("value", node.get("constructor"))


def _contains_constructor(node: Any, constructor: str) -> bool:
    return any(
        isinstance(value, Mapping) and value.get("constructor") == constructor
        for value in _walk(node)
    )


def _contains_text(node: Any, token: str) -> bool:
    needle = token.casefold()
    return any(
        isinstance(value, str) and needle in value.casefold()
        for value in _walk(node)
    )


def _counts(values: Sequence[Any]) -> dict[str, int]:
    result: dict[str, int] = {}
    for value in values:
        key = str(value)
        result[key] = result.get(key, 0) + 1
    return dict(sorted(result.items()))


def _stage_tree(observation: Mapping[str, Any], stage: str) -> Any:
    stages = observation.get("stages")
    if not isinstance(stages, Mapping):
        return None
    record = stages.get(stage)
    if not isinstance(record, Mapping):
        return None
    artifact = record.get("artifact")
    return artifact.get("native_tree") if isinstance(artifact, Mapping) else None


def _claim_summary(product: Mapping[str, Any]) -> dict[str, Any]:
    claim_id = _native_value(_field(product, "id"))
    claim_text = claim_id if isinstance(claim_id, str) else ""
    cause_match = re.search(r"(?:relative-cause|strategic-mechanism):([^:]+)", claim_text)
    return {
        "id_sha256": sha256_bytes(claim_text.encode("utf-8")) if claim_text else None,
        "cause_or_mechanism": cause_match.group(1) if cause_match else None,
        "family": _native_value(_field(product, "family")),
        "scope": _native_value(_field(product, "scope")),
        "contains_opponent_restriction": (
            _contains_constructor(product, "OpponentRestriction")
            or _contains_text(product, "opponent-restriction")
        ),
    }


def _semantic_checkpoints(observation: Mapping[str, Any]) -> dict[str, Any]:
    f_tree = _stage_tree(observation, "F")
    c_tree = _stage_tree(observation, "C")
    jp_tree = _stage_tree(observation, "Jp")
    ja_tree = _stage_tree(observation, "Ja")
    r_tree = _stage_tree(observation, "R")
    p_tree = _stage_tree(observation, "P")
    diagnostics_by_id: dict[str, Mapping[str, Any]] = {}
    for item in _products(f_tree, "ProbeAdmissionDiagnostic"):
        probe_id = _native_value(_field(item, "probeId"))
        if isinstance(probe_id, str):
            diagnostics_by_id.setdefault(probe_id, item)
    admission_statuses = [
        _native_value(_field(item, "status")) for item in diagnostics_by_id.values()
    ]
    relative_causes = _products(c_tree, "RelativeCauseFact")
    opponent_causes = [
        item for item in relative_causes if _contains_constructor(item, "OpponentRestriction")
    ]
    cause_kinds = [_native_value(_field(item, "kind")) for item in relative_causes]
    jp_claims = _products(jp_tree, "JudgmentClaim")
    admission_decisions = _products(ja_tree, "ClaimAdmissionDecision")
    ranked = _products(r_tree, "RankedClaimDecision")
    ranked_summary: list[dict[str, Any]] = []
    for item in ranked:
        claim = _field(item, "claim")
        ranked_summary.append(
            {
                **(_claim_summary(claim) if isinstance(claim, Mapping) else {}),
                "exposure_tier": _native_value(_field(item, "exposureTier")),
                "exposure_priority": _native_value(_field(item, "exposurePriority")),
                "priority": _native_value(_field(item, "priority")),
            }
        )
    deduplication = _products(r_tree, "ClaimDeduplicationTrace")
    dedup_reasons = [_native_value(_field(item, "reason")) for item in deduplication]
    primary_to_context = 0
    played_transition_to_context = 0
    for item in deduplication:
        original = str(_native_value(_field(item, "originalClaimId")) or "")
        kept = str(_native_value(_field(item, "keptClaimId")) or "")
        if ":primary:" in original and ":context:" in kept:
            primary_to_context += 1
        if "played-transition" in original and ":context:" in kept:
            played_transition_to_context += 1
    projections = _products(p_tree, "ProjectionTrace")
    projection = projections[0] if projections else None
    projection_summary = (
        {
            "selected_payload_source": _native_value(_field(projection, "selectedPayloadSource")),
            "has_explanation": _native_value(_field(projection, "hasExplanation")),
            "has_exact_proof": _native_value(_field(projection, "hasExactProof")),
            "primary_renderable": _native_value(_field(projection, "primaryRenderable")),
            "renderable": _native_value(_field(projection, "renderable")),
            "status": _native_value(_field(projection, "status")),
            "reason": _native_value(_field(projection, "reason")),
        }
        if projection is not None
        else None
    )
    body = _public_body(observation)
    move_review = body.get("move_review")
    explanations = move_review.get("explanations") if isinstance(move_review, Mapping) else None
    public_causes = sorted(
        {
            cause
            for explanation in (explanations if isinstance(explanations, list) else [])
            for cause in (
                explanation.get("chess_relations", [])
                if isinstance(explanation, Mapping)
                else []
            )
            if isinstance(cause, str)
        }
    )
    stages = observation.get("stages")
    v_record = stages.get("V") if isinstance(stages, Mapping) else None
    c_deterrence_count = len(_products(c_tree, "OpponentResourceDeterrenceProof"))
    c_opponent_cause_count = len(opponent_causes)
    jp_opponent_claim_count = sum(
        _contains_constructor(item, "OpponentRestriction")
        or _contains_text(item, "opponent-restriction")
        for item in jp_claims
    )
    tier_counts = _counts([item.get("exposure_tier") for item in ranked_summary])
    public_status = body.get("status")
    if admission_statuses and all(status == "Admitted" for status in admission_statuses) and c_deterrence_count == 0:
        first_missing = "C:opponent-resource-branches-admitted-without-deterrence-proof"
    elif c_deterrence_count > 0 and c_opponent_cause_count == 0:
        first_missing = "C:deterrence-proof-not-converted-to-opponent-restriction-cause"
    elif c_opponent_cause_count > 0 and jp_opponent_claim_count == 0:
        first_missing = "Jp:opponent-restriction-cause-not-proposed-as-claim"
    elif jp_opponent_claim_count > 0 and not any(
        item.get("contains_opponent_restriction")
        and item.get("exposure_tier") in {"Primary", "Secondary"}
        for item in ranked_summary
    ):
        first_missing = "R:opponent-restriction-claim-not-player-facing"
    elif public_status == "withheld":
        first_missing = "P:ranked-evidence-not-selected-for-public-response"
    else:
        first_missing = "none-through-P"
    return {
        "first_missing_checkpoint": first_missing,
        "F": {
            "unique_probe_diagnostic_count": len(diagnostics_by_id),
            "admission_status_counts": _counts(admission_statuses),
            "normalized_threat_branch_count": len(_products(f_tree, "NormalizedThreatBranch")),
        },
        "C": {
            "opponent_resource_deterrence_proof_count": c_deterrence_count,
            "plan_causal_event_evidence_count": len(
                _products(c_tree, "PlanCausalEventEvidence")
            ),
            "relative_cause_count": len(relative_causes),
            "relative_cause_kind_counts": _counts(cause_kinds),
            "opponent_restriction_relative_cause_count": c_opponent_cause_count,
        },
        "Jp": {
            "claim_count": len(jp_claims),
            "claims_containing_opponent_restriction_count": jp_opponent_claim_count,
        },
        "Ja": {
            "admission_decision_count": len(admission_decisions),
            "certified_count": sum(
                _native_value(_field(item, "status")) == "Certified"
                for item in admission_decisions
            ),
            "deferred_count": sum(
                _native_value(_field(item, "status")) == "Deferred"
                for item in admission_decisions
            ),
            "rejected_count": sum(
                _native_value(_field(item, "status")) == "Rejected"
                for item in admission_decisions
            ),
        },
        "R": {
            "ranked_claim_count": len(ranked),
            "exposure_tier_counts": tier_counts,
            "ranked_claims": ranked_summary,
            "deduplication_count": len(deduplication),
            "deduplication_reason_counts": _counts(dedup_reasons),
            "primary_to_context_replacement_count": primary_to_context,
            "played_transition_to_context_replacement_count": played_transition_to_context,
        },
        "P": projection_summary,
        "V": {
            "status": v_record.get("status") if isinstance(v_record, Mapping) else None,
            "boundary_reached": (
                v_record.get("boundary_reached") if isinstance(v_record, Mapping) else None
            ),
            "reason": v_record.get("reason") if isinstance(v_record, Mapping) else None,
        },
        "public": {
            "status": body.get("status"),
            "availability": body.get("availability"),
            "renderable": move_review.get("renderable") if isinstance(move_review, Mapping) else None,
            "explanation_count": len(explanations) if isinstance(explanations, list) else 0,
            "explanation_causes": public_causes,
            "verdict_present": (
                move_review.get("verdict") is not None if isinstance(move_review, Mapping) else False
            ),
            "remaining_probe_request_count": len(_probe_requests(observation)),
        },
    }


def _runtime_io_document(
    exchange: _RuntimeExchange,
    request: Mapping[str, Any],
    *,
    command: Sequence[str],
    round_index: int,
) -> dict[str, Any]:
    return {
        "schema_version": RUNTIME_IO_SCHEMA,
        "round_index": round_index,
        "transport": "canonical-jsonl-utf8-stdio",
        "command_argv_sha256": sha256_json(list(command)),
        "request": _mapping(request, "runtime request"),
        "request_canonical_sha256": sha256_bytes(
            native_canonical_json(request).encode("utf-8")
        ),
        "request_jsonl_sha256": exchange.request_jsonl_sha256,
        "response": _mapping(exchange.observation, "runtime observation"),
        "response_jsonl_sha256": exchange.response_jsonl_sha256,
        "diagnostic_line_count": len(exchange.diagnostic_line_sha256),
        "diagnostic_line_sha256": list(exchange.diagnostic_line_sha256),
        "diagnostic_stream_sha256": exchange.diagnostic_stream_sha256,
    }


def complete_runtime_probes(
    runtime_request: Mapping[str, Any],
    *,
    runtime: _RuntimeJsonlSession,
    stockfish: Path,
    stockfish_sha256: str,
    capture: _ArtifactCapture,
    max_rounds: int,
    engine_timeout_seconds: float,
) -> dict[str, Any]:
    base_request = _mapping(runtime_request, "runtime request")
    sample_id = _safe_text(base_request.get("request_id"), "runtime request id")
    sample_slug = slug(sample_id)
    input_value = _mapping(base_request.get("input"), "runtime request input")
    existing = input_value.get("probeResults", [])
    if not isinstance(existing, list) or not all(isinstance(item, Mapping) for item in existing):
        raise ContractError("runtime input probeResults must be an object array")
    results = [_mapping(item, "existing probe result") for item in existing]
    if len(results) > MAX_RUNTIME_PROBE_RESULTS:
        raise ContractError("runtime input exceeds the probe result limit")
    result_bindings: dict[str, str] = {
        _safe_text(item.get("id"), "existing probe result id"): sha256_json(item)
        for item in results
    }
    round_records: list[dict[str, Any]] = []
    probe_records: list[dict[str, Any]] = []
    final_observation: Mapping[str, Any] | None = None
    stop_reason = "max-rounds-reached"

    for round_index in range(max_rounds + 1):
        request = copy.deepcopy(base_request)
        request_input = _mapping(request.get("input"), "runtime request input")
        if results:
            request_input["probeResults"] = copy.deepcopy(results)
        else:
            request_input.pop("probeResults", None)
        request["input"] = request_input
        exchange = runtime.invoke(request)
        final_observation = exchange.observation
        runtime_io = _runtime_io_document(
            exchange,
            request,
            command=runtime.command,
            round_index=round_index,
        )
        runtime_ref = capture.write(
            f"samples/{sample_slug}/runtime-round-{round_index:02d}.json",
            runtime_io,
        )
        requests = _probe_requests(exchange.observation)
        round_records.append(
            {
                "round_index": round_index,
                "runtime_io": runtime_ref,
                "submitted_probe_result_count": len(results),
                "issued_probe_request_count": len(requests),
                "issued_probe_ids": [item["id"] for item in requests],
                "public_status": _public_body(exchange.observation).get("status"),
            }
        )
        if not requests:
            stop_reason = "all-runtime-probes-closed"
            break
        validated = [_validate_probe_request(item) for item in requests]
        for request_item in validated:
            request_id = str(request_item["id"])
            current_binding = sha256_json(request_item)
            if request_id in result_bindings and result_bindings[request_id] != current_binding:
                raise IntegrityError(f"runtime changed an issued probe binding: {request_id}")
        new_requests = [item for item in validated if str(item["id"]) not in result_bindings]
        if not new_requests:
            stop_reason = "runtime-reissued-fulfilled-probes"
            break
        if round_index >= max_rounds:
            break
        if len(results) + len(new_requests) > MAX_RUNTIME_PROBE_RESULTS:
            raise ContractError("dynamic probe closure exceeds the runtime result limit")
        for request_item in new_requests:
            result, uci_io = _search_probe(
                stockfish,
                request_item,
                timeout_seconds=engine_timeout_seconds,
                executable_sha256=stockfish_sha256,
            )
            probe_key = hashlib.sha256(str(request_item["id"]).encode("utf-8")).hexdigest()[:16]
            uci_ref = capture.write(
                f"samples/{sample_slug}/probe-{probe_key}-uci.json",
                uci_io,
            )
            result_doc = {
                "schema_version": PROBE_RESULT_SCHEMA,
                "request": request_item,
                "request_sha256": sha256_json(request_item),
                "result": result,
                "result_sha256": sha256_json(result),
                "uci_io": uci_ref,
            }
            result_ref = capture.write(
                f"samples/{sample_slug}/probe-{probe_key}-result.json",
                result_doc,
            )
            results.append(result)
            result_bindings[str(request_item["id"])] = sha256_json(request_item)
            probe_records.append(
                {
                    "probe_id": request_item["id"],
                    "kind": (
                        "opponent-resource"
                        if request_item.get("opponentResourceMove") is not None
                        else "branch-reply"
                    ),
                    "depth": request_item["depth"],
                    "multipv": request_item["multiPv"],
                    "horizon": request_item.get("horizon"),
                    "result": result_ref,
                    "uci_io": uci_ref,
                }
            )

    assert final_observation is not None
    summary = {
        "schema_version": PROBE_COMPLETION_SCHEMA,
        "sample_id": sample_id,
        "initial_request_sha256": sha256_json(base_request),
        "stockfish_executable_sha256": stockfish_sha256,
        "closure_status": (
            "closed" if stop_reason == "all-runtime-probes-closed" else "incomplete"
        ),
        "stop_reason": stop_reason,
        "runtime_round_count": len(round_records),
        "acquired_probe_count": len(probe_records),
        "opponent_resource_probe_count": sum(
            item["kind"] == "opponent-resource" for item in probe_records
        ),
        "branch_reply_probe_count": sum(item["kind"] == "branch-reply" for item in probe_records),
        "rounds": round_records,
        "probes": probe_records,
        "final_observation_sha256": sha256_json(final_observation),
        "semantic_checkpoints": _semantic_checkpoints(final_observation),
    }
    summary_ref = capture.write(f"samples/{sample_slug}/completion.json", summary)
    return {**summary, "completion_artifact": summary_ref}


def run_probe_completion(
    request_paths: Sequence[Path],
    *,
    stockfish: Path,
    sbt: Path,
    adapter_root: Path,
    artifact_root: Path,
    run_id: str,
    max_rounds: int = 4,
    runtime_timeout_seconds: float = 300.0,
    engine_timeout_seconds: float = 180.0,
) -> dict[str, Any]:
    if not request_paths:
        raise ContractError("at least one runtime request source is required")
    max_rounds = _integer(max_rounds, "max rounds", 1, 12)
    stockfish = stockfish.resolve(strict=True)
    sbt = sbt.resolve(strict=True)
    adapter_root = adapter_root.resolve(strict=True)
    if not stockfish.is_file() or not sbt.is_file() or not adapter_root.is_dir():
        raise ContractError("Stockfish, sbt, and adapter paths must have the expected types")
    requests = [_load_runtime_request(path.resolve(strict=True)) for path in request_paths]
    request_ids = [str(item["request_id"]) for item in requests]
    if len(set(request_ids)) != len(request_ids):
        raise ContractError("runtime request ids must be unique")
    stockfish_sha256 = sha256_file(stockfish)
    command = [
        str(sbt),
        "-batch",
        "-error",
        "runMain io.chesstory.evaluation.runtimeadapter.RuntimeAdapterCli",
    ]
    capture = _ArtifactCapture(artifact_root, run_id)
    config = {
        "schema_version": "chesstory.eval.runtime-probe-completion-config.v1",
        "run_id": run_id,
        "runtime_command_argv_sha256": sha256_json(command),
        "runtime_adapter_root_sha256": sha256_bytes(str(adapter_root).encode("utf-8")),
        "stockfish_executable_sha256": stockfish_sha256,
        "engine": {"Threads": 1, "Hash": 16, "fixed_depth_from_each_request": True},
        "max_rounds": max_rounds,
        "runtime_timeout_seconds": runtime_timeout_seconds,
        "engine_timeout_seconds": engine_timeout_seconds,
        "request_sources": [
            {"request_id": request["request_id"], "request_sha256": sha256_json(request)}
            for request in requests
        ],
    }
    config_ref = capture.write("config.json", config)
    with _RuntimeJsonlSession(
        command,
        cwd=adapter_root,
        timeout_seconds=runtime_timeout_seconds,
    ) as runtime:
        samples = [
            complete_runtime_probes(
                request,
                runtime=runtime,
                stockfish=stockfish,
                stockfish_sha256=stockfish_sha256,
                capture=capture,
                max_rounds=max_rounds,
                engine_timeout_seconds=engine_timeout_seconds,
            )
            for request in requests
        ]
    report = {
        "schema_version": REPORT_SCHEMA,
        "run_id": run_id,
        "config": config_ref,
        "sample_count": len(samples),
        "all_probes_closed": all(item["closure_status"] == "closed" for item in samples),
        "samples": samples,
    }
    report_ref = capture.write("report.json", report)
    ledger = capture.finish()
    return {
        **report,
        "report_artifact": report_ref,
        "artifact_ledger": ledger,
        "run_root": str(capture.run_root),
    }


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Close RuntimeProtocol reply_multipv probes with deterministic Stockfish "
            "acquisition and capture full Q-to-P native observations."
        )
    )
    parser.add_argument("--request", action="append", type=Path, required=True)
    parser.add_argument("--stockfish", type=Path, required=True)
    parser.add_argument("--sbt", type=Path, required=True)
    parser.add_argument("--adapter-root", type=Path, required=True)
    parser.add_argument("--artifact-root", type=Path, required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--max-rounds", type=int, default=4)
    parser.add_argument("--runtime-timeout-seconds", type=float, default=300.0)
    parser.add_argument("--engine-timeout-seconds", type=float, default=180.0)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    arguments = _parser().parse_args(argv)
    try:
        report = run_probe_completion(
            arguments.request,
            stockfish=arguments.stockfish,
            sbt=arguments.sbt,
            adapter_root=arguments.adapter_root,
            artifact_root=arguments.artifact_root,
            run_id=arguments.run_id,
            max_rounds=arguments.max_rounds,
            runtime_timeout_seconds=arguments.runtime_timeout_seconds,
            engine_timeout_seconds=arguments.engine_timeout_seconds,
        )
    except (ContractError, IntegrityError, StockfishAcquisitionError, OSError) as error:
        print(json.dumps({"ok": False, "error": str(error)}, ensure_ascii=False, sort_keys=True))
        return 2
    print(json.dumps({"ok": True, **report}, ensure_ascii=False, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
