from __future__ import annotations

import base64
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

from .hashing import sha256_bytes
from .model import ContractError, IntegrityError
from .schemas import SchemaRegistry
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


UCI_IO_SCHEMA = "chesstory.eval.runtime-probe-uci-io.v1"
REPLY_MULTIPV = "reply_multipv"
MAX_RUNTIME_PROBE_RESULTS = 12
_MOVE_RE = re.compile(r"^[a-h][1-8][a-h][1-8][qrbn]?")


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


@dataclass(frozen=True)
class _RuntimeExchange:
    http_status: int
    body: Mapping[str, Any]
    request_jsonl_base64: str
    request_jsonl_sha256: str
    request_jsonl_length: int
    response_jsonl_base64: str
    response_jsonl_sha256: str
    response_jsonl_length: int


def _verified_response_jsonl_bytes(
    response_jsonl_base64: Any,
    response_jsonl_sha256: Any,
) -> bytes:
    if not isinstance(response_jsonl_base64, str):
        raise ContractError("runtime response JSONL capture must be base64 text")
    try:
        response_bytes = base64.b64decode(response_jsonl_base64, validate=True)
    except ValueError as error:
        raise ContractError("runtime response JSONL capture is invalid base64") from error
    if sha256_bytes(response_bytes) != response_jsonl_sha256:
        raise IntegrityError("runtime response JSONL capture hash mismatch")
    return response_bytes


class _RuntimeJsonlSession:
    """Persistent sbt JSONL process with byte-exact transport hashes."""

    def __init__(
        self,
        command: Sequence[str],
        *,
        cwd: Path,
        timeout_seconds: float,
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
        self.public_response_schema_path = (
            Path(__file__).resolve().parents[2]
            / "schemas"
            / "public-v6"
            / "move-meaning-response.schema.json"
        ).resolve(strict=True)
        self.public_response_schema_registry = SchemaRegistry(
            self.public_response_schema_path.parent
        )
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
        try:
            request_bytes = (
                json.dumps(
                    dict(request),
                    ensure_ascii=False,
                    separators=(",", ":"),
                    allow_nan=False,
                )
                + "\n"
            ).encode("utf-8")
        except (TypeError, ValueError) as error:
            raise ContractError("runtime request must be finite JSON") from error
        try:
            process.stdin.write(request_bytes)
            process.stdin.flush()
        except (BrokenPipeError, OSError) as error:
            raise ContractError("runtime adapter closed its input") from error
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
            try:
                parsed = json.loads(value.decode("utf-8"))
            except (UnicodeError, json.JSONDecodeError) as error:
                raise ContractError("runtime adapter emitted malformed UTF-8 JSON") from error
            outer = _mapping(parsed, "runtime public response")
            if set(outer) != {"http_status", "body"}:
                raise ContractError(
                    "runtime adapter response must contain exactly http_status and body"
                )
            http_status = _integer(
                outer["http_status"], "runtime response http_status", 100, 599
            )
            body = _mapping(outer["body"], "runtime response body")
            self.public_response_schema_registry.validate_document(
                body,
                self.public_response_schema_path,
                label="runtime public response body",
            )
            request_id = request.get("request_id")
            if (
                isinstance(request_id, str)
                and re.fullmatch(r"[A-Za-z0-9._:-]{1,128}", request_id) is not None
                and body.get("request_id") != request_id
            ):
                raise ContractError(
                    "runtime public response must exactly echo a valid request_id"
                )
            return _RuntimeExchange(
                http_status=http_status,
                body=body,
                request_jsonl_base64=base64.b64encode(request_bytes).decode("ascii"),
                request_jsonl_sha256=sha256_bytes(request_bytes),
                request_jsonl_length=len(request_bytes),
                response_jsonl_base64=base64.b64encode(value).decode("ascii"),
                response_jsonl_sha256=sha256_bytes(value),
                response_jsonl_length=len(value),
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


def _validate_probe_request(request: Mapping[str, Any]) -> dict[str, Any]:
    value = _mapping(request, "probe request")
    common_keys = {
        "id",
        "fen",
        "moves",
        "depth",
        "purpose",
        "multiPv",
        "candidateMove",
        "variationHash",
    }
    variant_keys = {
        key
        for key in ("horizon", "opponentResourceMove", "continuationMoves")
        if key in value
    }
    if len(variant_keys) != 1:
        raise ContractError("runtime probe request must declare exactly one causal objective")
    variant_key = next(iter(variant_keys))
    expected_keys = common_keys | {variant_key}
    if set(value) != expected_keys:
        raise ContractError("runtime probe request fields do not match one exact causal objective")
    if value["purpose"] != REPLY_MULTIPV:
        raise ContractError(f"unsupported runtime probe purpose: {value['purpose']!r}")
    _safe_text(value["id"], "probe request id")
    fen = _safe_text(value["fen"], "probe request FEN")
    try:
        board = chess.Board(fen)
    except ValueError as error:
        raise ContractError("probe request FEN is invalid") from error
    if board.status() != chess.STATUS_VALID:
        raise ContractError(f"probe request FEN is illegal (status={int(board.status())})")
    _integer(value["depth"], "probe request depth", 1, 100)
    multi_pv = _integer(value["multiPv"], "probe request multiPv", 1, 3)
    _move(value["candidateMove"], "probe request candidateMove")
    variation_hash = _safe_text(value["variationHash"], "probe request variationHash")
    if re.fullmatch(r"[0-9a-f]{64}", variation_hash) is None:
        raise ContractError("probe request variationHash must be lowercase SHA-256")
    raw_moves = value["moves"]
    if not isinstance(raw_moves, list):
        raise ContractError("probe request moves must be an array")
    moves = [_move(item, "probe request move") for item in raw_moves]
    if variant_key == "opponentResourceMove":
        resource = _move(
            value["opponentResourceMove"], "probe request opponentResourceMove"
        )
        if moves != [resource] or multi_pv != 1:
            raise ContractError("opponent-resource probe binding is not exact")
    elif variant_key == "continuationMoves":
        raw_continuation = value["continuationMoves"]
        if not isinstance(raw_continuation, list):
            raise ContractError("causal continuation must be an array")
        continuation = [
            _move(item, "probe request continuation move")
            for item in raw_continuation
        ]
        if len(continuation) != 2 or moves != continuation or multi_pv != 1:
            raise ContractError("causal-continuation probe binding is not exact")
    else:
        horizon = _safe_text(value["horizon"], "probe request horizon")
        if len(horizon) > 14:
            raise ContractError("ordinary reply_multipv probe horizon is too long")
        horizon_match = re.fullmatch(r"ply:([1-9][0-9]*)", horizon)
        if horizon_match is None:
            raise ContractError("ordinary reply_multipv probe horizon is invalid")
        horizon_ply = int(horizon_match.group(1))
        if not 1 <= horizon_ply <= 2_147_483_647:
            raise ContractError("ordinary reply_multipv probe horizon is out of range")
        if moves:
            raise ContractError("ordinary reply_multipv probe must not carry request moves")
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
    continuation = probe.get("continuationMoves")
    search_board = branch_board.copy(stack=False)
    prefix: list[str] = []
    forced_moves = (
        list(continuation)
        if continuation is not None
        else [resource]
        if resource is not None
        else []
    )
    for forced in forced_moves:
        forced_move = chess.Move.from_uci(str(forced))
        if not search_board.is_legal(forced_move):
            raise ContractError(f"forced probe move is illegal at requested FEN: {forced}")
        search_board.push(forced_move)
        prefix.append(str(forced))
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
        "variationHash": probe["variationHash"],
    }
    if resource is not None:
        result["opponentResourceMove"] = resource
    if continuation is not None:
        result["continuationMoves"] = continuation
    if probe.get("horizon") is not None:
        result["horizon"] = probe["horizon"]
    return result, io_document
