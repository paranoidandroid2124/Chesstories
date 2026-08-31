from __future__ import annotations

import hashlib
import json
import math
import os
import queue
import re
import subprocess
import threading
import time
from dataclasses import dataclass, replace
from pathlib import Path
from statistics import fmean, pvariance
from typing import Any, Mapping, Sequence, TextIO

import chess

from .model import ContractError


_MOVE_RE = re.compile(r"^[a-h][1-8][a-h][1-8][qrbn]?$")
_TABLEBASE_MODES = {"required-when-applicable", "optional", "disabled"}


class StockfishAcquisitionError(ContractError):
    """No trustworthy Q result exists; partial provider I/O may still be captured."""

    def __init__(self, message: str, raw_provider_io: Mapping[str, Any] | None = None) -> None:
        super().__init__(message)
        self.raw_provider_io = raw_provider_io


@dataclass(frozen=True)
class StockfishAcquisitionResult:
    """Q payload plus diagnostics and non-canonical raw UCI capture."""

    engine_evidence_pack: Mapping[str, Any]
    diagnostics: Mapping[str, Any]
    raw_provider_io: Mapping[str, Any]


@dataclass(frozen=True)
class _UciOption:
    name: str
    option_type: str
    default: str | None
    minimum: int | None
    maximum: int | None
    variants: tuple[str, ...]

    def signature(self) -> tuple[Any, ...]:
        return (self.name, self.option_type, self.default, self.minimum, self.maximum, self.variants)

    def as_dict(self) -> dict[str, Any]:
        result: dict[str, Any] = {"name": self.name, "type": self.option_type}
        if self.default is not None:
            result["default"] = self.default
        if self.minimum is not None:
            result["min"] = self.minimum
        if self.maximum is not None:
            result["max"] = self.maximum
        if self.variants:
            result["var"] = list(self.variants)
        return result


@dataclass(frozen=True)
class _EngineIdentity:
    name: str
    author: str | None
    version: str
    options: tuple[_UciOption, ...]

    def signature(self) -> tuple[Any, ...]:
        return (
            self.name,
            self.author,
            self.version,
            tuple(option.signature() for option in self.options),
        )


@dataclass(frozen=True)
class _ParsedInfo:
    multipv_index: int
    moves: tuple[str, ...]
    depth: int
    selective_depth: int | None
    nodes: int
    score_kind: str
    score_value: int
    score_bound: str | None


@dataclass(frozen=True)
class _SearchLine:
    multipv_index: int
    uci_multipv_index: int
    root_move: str
    moves: tuple[str, ...]
    depth: int
    selective_depth: int | None
    nodes: int
    score_kind: str
    score_value: int
    final_fen: str
    search_kind: str

    def evaluation(self) -> dict[str, Any]:
        score_field = "centipawns" if self.score_kind == "cp" else "mate_in"
        return {"perspective": "root-side", score_field: self.score_value}


@dataclass(frozen=True)
class _Repetition:
    index: int
    root_lines: tuple[_SearchLine, ...]
    forced_required_lines: tuple[_SearchLine, ...]
    total_nodes: int

    @property
    def all_lines(self) -> tuple[_SearchLine, ...]:
        return self.root_lines + self.forced_required_lines


class _UciSession:
    """One fresh process with lossless line capture and bounded reads."""

    def __init__(self, executable: Path) -> None:
        creationflags = getattr(subprocess, "CREATE_NO_WINDOW", 0) if os.name == "nt" else 0
        try:
            self.process = subprocess.Popen(
                [str(executable)],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                encoding="utf-8",
                errors="strict",
                bufsize=1,
                creationflags=creationflags,
            )
        except OSError as error:
            raise StockfishAcquisitionError(f"cannot start Stockfish: {error}") from error
        if self.process.stdin is None or self.process.stdout is None or self.process.stderr is None:
            self.process.kill()
            raise StockfishAcquisitionError("Stockfish pipes were not created")
        self._started_ns = time.monotonic_ns()
        self._stdout_queue: queue.Queue[tuple[str, Any]] = queue.Queue()
        self.commands: list[dict[str, Any]] = []
        self.stdout: list[dict[str, Any]] = []
        self.stderr: list[dict[str, Any]] = []
        self._stdout_thread = threading.Thread(
            target=self._read_stream,
            args=(self.process.stdout, "stdout", self.stdout, self._stdout_queue),
            daemon=True,
        )
        self._stderr_thread = threading.Thread(
            target=self._read_stream,
            args=(self.process.stderr, "stderr", self.stderr, None),
            daemon=True,
        )
        self._stdout_thread.start()
        self._stderr_thread.start()

    def _read_stream(
        self,
        stream: TextIO,
        stream_name: str,
        sink: list[dict[str, Any]],
        output_queue: queue.Queue[tuple[str, Any]] | None,
    ) -> None:
        try:
            while True:
                raw = stream.readline()
                if raw == "":
                    break
                event = {
                    "sequence": len(sink) + 1,
                    "elapsed_ns": time.monotonic_ns() - self._started_ns,
                    "line": raw.rstrip("\r\n"),
                }
                sink.append(event)
                if output_queue is not None:
                    output_queue.put(("line", event["line"]))
        except Exception as error:
            event = {
                "sequence": len(sink) + 1,
                "elapsed_ns": time.monotonic_ns() - self._started_ns,
                "reader_error": f"{type(error).__name__}: {error}",
            }
            sink.append(event)
            if output_queue is not None:
                output_queue.put(("error", event["reader_error"]))
        finally:
            if output_queue is not None:
                output_queue.put(("eof", stream_name))

    def send(self, line: str) -> None:
        if not line or "\r" in line or "\n" in line:
            raise StockfishAcquisitionError("refusing malformed UCI command", self.raw())
        if self.process.poll() is not None:
            raise StockfishAcquisitionError("Stockfish exited before command dispatch", self.raw())
        self.commands.append(
            {
                "sequence": len(self.commands) + 1,
                "elapsed_ns": time.monotonic_ns() - self._started_ns,
                "line": line,
            }
        )
        try:
            assert self.process.stdin is not None
            self.process.stdin.write(line + "\n")
            self.process.stdin.flush()
        except (BrokenPipeError, OSError) as error:
            raise StockfishAcquisitionError("Stockfish command pipe failed", self.raw()) from error

    def read_until(
        self,
        predicate: Any,
        *,
        timeout_seconds: float,
        context: str,
    ) -> list[str]:
        deadline = time.monotonic() + timeout_seconds
        result: list[str] = []
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise StockfishAcquisitionError(f"Stockfish timed out waiting for {context}", self.raw())
            try:
                kind, value = self._stdout_queue.get(timeout=remaining)
            except queue.Empty as error:
                raise StockfishAcquisitionError(
                    f"Stockfish timed out waiting for {context}", self.raw()
                ) from error
            if kind == "error":
                raise StockfishAcquisitionError(f"Stockfish stdout decode failed: {value}", self.raw())
            if kind == "eof":
                raise StockfishAcquisitionError(
                    f"Stockfish closed stdout while waiting for {context}", self.raw()
                )
            line = str(value)
            result.append(line)
            if predicate(line):
                return result

    def close(self) -> None:
        if self.process.poll() is None:
            try:
                self.send("quit")
            except StockfishAcquisitionError:
                pass
            try:
                self.process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=2)
        self._stdout_thread.join(timeout=1)
        self._stderr_thread.join(timeout=1)

    def raw(self) -> dict[str, Any]:
        return {
            "commands": [dict(event) for event in self.commands],
            "stdout": [dict(event) for event in self.stdout],
            "stderr": [dict(event) for event in self.stderr],
            "returncode": self.process.poll(),
        }


def acquire_stockfish_evidence(
    executable: str | Path,
    question: Mapping[str, Any],
    acquisition_config: Mapping[str, Any],
    *,
    timeout_seconds: float = 180.0,
) -> StockfishAcquisitionResult:
    """Acquire schema-v1 Q evidence without making any chess judgment.

    Scores remain exactly in Stockfish's root side-to-move perspective.  Every
    repetition uses a new process, fixed depth/MultiPV, fixed Threads/Hash, and
    a cleared transposition table.  ``python-chess`` is the sole FEN, legal-move,
    and PV replay authority in this external adapter.
    """

    if isinstance(timeout_seconds, bool) or not isinstance(timeout_seconds, (int, float)):
        raise StockfishAcquisitionError("timeout_seconds must be a positive finite number")
    if not math.isfinite(float(timeout_seconds)) or timeout_seconds <= 0:
        raise StockfishAcquisitionError("timeout_seconds must be a positive finite number")
    executable_path = Path(executable).expanduser().resolve()
    if not executable_path.is_file():
        raise StockfishAcquisitionError(f"Stockfish executable is not a file: {executable_path}")
    executable_sha256 = _sha256_file(executable_path)

    normalized_question, board, legal_moves = _validate_question(question)
    normalized_config, requested_options = _validate_config(acquisition_config)
    depth = int(normalized_config["depth"])
    multipv = int(normalized_config["multipv"])
    repetition_count = int(normalized_config["repetitions"])
    expected_root_lines = min(multipv, len(legal_moves))
    played_move = str(normalized_question["played_move_uci"])
    required_moves = tuple(
        dict.fromkeys(
            move
            for move in (
                played_move,
                *normalized_question.get("requested_reference_moves_uci", []),
            )
            if move != "0000"
        )
    )

    raw_repetitions: list[dict[str, Any]] = []
    raw_capture: dict[str, Any] = {
        "schema_version": "chesstory.eval.stockfish-uci-io.v1",
        "executable": f"sha256:{executable_sha256}",
        "executable_sha256": executable_sha256,
        "score_perspective": "root-side",
        "repetitions": raw_repetitions,
    }
    acquired: list[_Repetition] = []
    baseline_identity: _EngineIdentity | None = None
    applied_options: dict[str, Any] | None = None
    clear_hash_name: str | None = None

    for repetition_index in range(repetition_count):
        if _sha256_file(executable_path) != executable_sha256:
            raise StockfishAcquisitionError("Stockfish executable changed during acquisition", raw_capture)
        session: _UciSession | None = None
        searches: list[dict[str, Any]] = []
        failure: Exception | None = None
        try:
            session = _UciSession(executable_path)
            identity = _handshake(
                session,
                timeout_seconds=float(timeout_seconds),
                executable_sha256=executable_sha256,
            )
            if baseline_identity is None:
                baseline_identity = identity
                applied_options, clear_hash_name = _resolve_options(
                    identity, requested_options, multipv
                )
            elif identity.signature() != baseline_identity.signature():
                raise StockfishAcquisitionError(
                    "Stockfish identity or option declarations changed between repetitions"
                )
            assert applied_options is not None
            _configure(session, applied_options, timeout_seconds=float(timeout_seconds))
            session.send("ucinewgame")
            _ready(session, float(timeout_seconds), "new-game readiness")
            _clear_hash(session, clear_hash_name, float(timeout_seconds))

            root_lines, root_nodes = _run_search(
                session,
                board,
                depth=depth,
                configured_multipv=multipv,
                expected_lines=expected_root_lines,
                searchmove=None,
                timeout_seconds=float(timeout_seconds),
            )
            searches.append(
                {
                    "kind": "root-multipv",
                    "depth": depth,
                    "requested_multipv": multipv,
                    "returned_lines": len(root_lines),
                    "nodes": root_nodes,
                }
            )
            forced_lines: list[_SearchLine] = []
            forced_nodes = 0
            root_moves = {line.root_move for line in root_lines}
            for required_move in required_moves:
                if required_move in root_moves:
                    continue
                _clear_hash(session, clear_hash_name, float(timeout_seconds))
                forced, search_nodes = _run_search(
                    session,
                    board,
                    depth=depth,
                    configured_multipv=multipv,
                    expected_lines=1,
                    searchmove=required_move,
                    timeout_seconds=float(timeout_seconds),
                )
                forced_nodes += search_nodes
                role = "played" if required_move == played_move else "reference"
                forced_lines.append(
                    replace(
                        forced[0],
                        search_kind=f"required-{role}-move-searchmoves",
                    )
                )
                searches.append(
                    {
                        "kind": f"required-{role}-move-searchmoves",
                        "move_uci": required_move,
                        "depth": depth,
                        "returned_lines": 1,
                        "nodes": search_nodes,
                    }
                )
            acquired.append(
                _Repetition(
                    index=repetition_index,
                    root_lines=root_lines,
                    forced_required_lines=tuple(forced_lines),
                    total_nodes=root_nodes + forced_nodes,
                )
            )
        except Exception as error:
            failure = error
        finally:
            if session is not None:
                session.close()
                raw = session.raw()
                raw["repetition_index"] = repetition_index
                raw["searches"] = searches
                raw_repetitions.append(raw)
                if failure is None and any(
                    "reader_error" in event for event in (*raw["stdout"], *raw["stderr"])
                ):
                    failure = StockfishAcquisitionError("Stockfish output decoding failed")
                if failure is None and raw["returncode"] != 0:
                    failure = StockfishAcquisitionError(
                        f"Stockfish exited with status {raw['returncode']}"
                    )
        if failure is not None:
            message = (
                str(failure)
                if isinstance(failure, StockfishAcquisitionError)
                else f"unexpected acquisition failure: {type(failure).__name__}: {failure}"
            )
            raise StockfishAcquisitionError(message, raw_capture) from failure

    if _sha256_file(executable_path) != executable_sha256:
        raise StockfishAcquisitionError("Stockfish executable changed during acquisition", raw_capture)
    if baseline_identity is None or applied_options is None:
        raise StockfishAcquisitionError("no Stockfish acquisition completed", raw_capture)

    pack, diagnostics = _assemble_pack(
        question=normalized_question,
        config=normalized_config,
        board=board,
        repetitions=tuple(acquired),
        identity=baseline_identity,
        executable_sha256=executable_sha256,
        applied_options=applied_options,
    )
    return StockfishAcquisitionResult(pack, diagnostics, raw_capture)


def _validate_question(
    question: Mapping[str, Any],
) -> tuple[dict[str, Any], chess.Board, frozenset[str]]:
    if not isinstance(question, Mapping):
        raise StockfishAcquisitionError("question must be an object")
    result = _json_clone(question, "question")
    allowed = {
        "root_position",
        "played_move_uci",
        "move_history_uci",
        "requested_reference_moves_uci",
        "game_phase",
    }
    _exact_keys(result, {"root_position", "played_move_uci"}, allowed, "question")
    root = result["root_position"]
    if not isinstance(root, dict):
        raise StockfishAcquisitionError("question.root_position must be an object")
    _exact_keys(
        root,
        {"position_id", "fen", "side_to_move"},
        {"position_id", "fen", "side_to_move"},
        "question.root_position",
    )
    if not isinstance(root["position_id"], str) or not root["position_id"]:
        raise StockfishAcquisitionError("root position_id must be non-empty")
    if root["side_to_move"] not in {"white", "black"}:
        raise StockfishAcquisitionError("root side_to_move is invalid")
    board = _validated_board(root["fen"])
    board_side = "white" if board.turn == chess.WHITE else "black"
    if root["side_to_move"] != board_side:
        raise StockfishAcquisitionError("FEN turn disagrees with root side_to_move")
    legal_moves = frozenset(move.uci() for move in board.legal_moves)

    played = result["played_move_uci"]
    _schema_move(played, "question.played_move_uci")
    if legal_moves and (played == "0000" or played not in legal_moves):
        raise StockfishAcquisitionError(f"played move {played!r} is illegal")
    if not legal_moves and played != "0000":
        raise StockfishAcquisitionError("terminal root requires played_move_uci='0000'")

    history = result.get("move_history_uci", [])
    if not isinstance(history, list):
        raise StockfishAcquisitionError("move_history_uci must be an array")
    for index, move in enumerate(history):
        _schema_move(move, f"move_history_uci[{index}]")

    references = result.get("requested_reference_moves_uci", [])
    if not isinstance(references, list) or len({_json_text(value) for value in references}) != len(
        references
    ):
        raise StockfishAcquisitionError("requested_reference_moves_uci must be a unique array")
    for index, move in enumerate(references):
        _schema_move(move, f"requested_reference_moves_uci[{index}]")
        if legal_moves and (move == "0000" or move not in legal_moves):
            raise StockfishAcquisitionError(f"requested reference move {move!r} is illegal")
        if not legal_moves and move != "0000":
            raise StockfishAcquisitionError("terminal root has a non-null reference move")

    if "game_phase" in result and result["game_phase"] not in {
        "opening",
        "middlegame",
        "endgame",
    }:
        raise StockfishAcquisitionError("game_phase is invalid")
    return result, board, legal_moves


def _validated_board(fen: Any) -> chess.Board:
    if not isinstance(fen, str) or not fen or "\r" in fen or "\n" in fen:
        raise StockfishAcquisitionError("FEN must be a non-empty safe UCI line")
    try:
        board = chess.Board(fen)
    except ValueError as error:
        raise StockfishAcquisitionError(f"malformed FEN: {error}") from error
    status = board.status()
    if status != chess.STATUS_VALID:
        raise StockfishAcquisitionError(f"illegal FEN (python-chess status={int(status)})")
    return board


def _validate_config(config: Mapping[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    if not isinstance(config, Mapping):
        raise StockfishAcquisitionError("acquisition_config must be an object")
    result = _json_clone(config, "acquisition_config")
    required = {
        "depth",
        "multipv",
        "repetitions",
        "stability_tolerance_cp",
        "probe_kinds",
        "tablebase",
    }
    _exact_keys(
        result,
        required,
        required | {"node_limit", "time_limit_ms", "engine_options"},
        "acquisition_config",
    )
    for field in ("depth", "multipv", "repetitions"):
        _integer(result[field], f"acquisition_config.{field}", 1)
    _integer(result["stability_tolerance_cp"], "stability_tolerance_cp", 0)
    if "node_limit" in result or "time_limit_ms" in result:
        raise StockfishAcquisitionError(
            "fixed-depth acquisition forbids node_limit and time_limit_ms"
        )
    probes = result["probe_kinds"]
    if (
        not isinstance(probes, list)
        or not all(isinstance(probe, str) and probe for probe in probes)
        or len(set(probes)) != len(probes)
    ):
        raise StockfishAcquisitionError("probe_kinds must contain unique non-empty strings")
    if result["tablebase"] not in _TABLEBASE_MODES:
        raise StockfishAcquisitionError("tablebase mode is invalid")

    raw_options = result.get("engine_options", {})
    if not isinstance(raw_options, dict):
        raise StockfishAcquisitionError("engine_options must be an object")
    requested: dict[str, Any] = {}
    folded_names: set[str] = set()
    for name, value in raw_options.items():
        if not isinstance(name, str) or not name or "\r" in name or "\n" in name:
            raise StockfishAcquisitionError("engine option names must be safe non-empty strings")
        if name.casefold() in folded_names:
            raise StockfishAcquisitionError(f"duplicate case-insensitive option {name!r}")
        folded_names.add(name.casefold())
        if not _json_scalar(value):
            raise StockfishAcquisitionError(f"engine option {name!r} is not a finite scalar")
        if isinstance(value, str) and ("\r" in value or "\n" in value):
            raise StockfishAcquisitionError(f"engine option {name!r} contains a newline")
        requested[name] = value
    return result, requested


def _handshake(
    session: _UciSession,
    *,
    timeout_seconds: float,
    executable_sha256: str,
) -> _EngineIdentity:
    session.send("uci")
    lines = session.read_until(
        lambda line: line == "uciok", timeout_seconds=timeout_seconds, context="uciok"
    )
    names = [line[8:] for line in lines if line.startswith("id name ")]
    authors = [line[10:] for line in lines if line.startswith("id author ")]
    if len(names) != 1 or not names[0]:
        raise StockfishAcquisitionError("engine must emit exactly one non-empty id name")
    if len(authors) > 1 or (authors and not authors[0]):
        raise StockfishAcquisitionError("engine emitted malformed id author")
    if re.search(r"\bstockfish\b", names[0], re.IGNORECASE) is None:
        raise StockfishAcquisitionError(f"UCI engine is not Stockfish: {names[0]!r}")
    suffix = re.sub(
        r"^.*?\bstockfish\b", "", names[0], count=1, flags=re.IGNORECASE
    ).strip()
    version = suffix or f"unspecified+sha256.{executable_sha256[:12]}"
    options: list[_UciOption] = []
    seen: set[str] = set()
    for line in lines:
        if not line.startswith("option "):
            continue
        option = _parse_option(line)
        if option.name.casefold() in seen:
            raise StockfishAcquisitionError(f"duplicate UCI option {option.name!r}")
        seen.add(option.name.casefold())
        options.append(option)
    return _EngineIdentity(names[0], authors[0] if authors else None, version, tuple(options))


def _parse_option(line: str) -> _UciOption:
    match = re.fullmatch(
        r"option name (.+?) type (check|spin|combo|button|string)(?: (.*))?", line
    )
    if match is None:
        raise StockfishAcquisitionError(f"malformed UCI option declaration: {line!r}")
    name, option_type, tail = match.groups()
    tail = tail or ""
    markers = list(re.finditer(r"(?:^| )(default|min|max|var)(?: |$)", tail))
    values: dict[str, list[str]] = {key: [] for key in ("default", "min", "max", "var")}
    for index, marker in enumerate(markers):
        end = markers[index + 1].start() if index + 1 < len(markers) else len(tail)
        values[marker.group(1)].append(tail[marker.end() : end].strip())
    if any(len(values[key]) > 1 for key in ("default", "min", "max")):
        raise StockfishAcquisitionError(f"repeated UCI option field: {line!r}")
    minimum = _option_integer(values["min"], "min", line)
    maximum = _option_integer(values["max"], "max", line)
    if minimum is not None and maximum is not None and minimum > maximum:
        raise StockfishAcquisitionError(f"UCI option min exceeds max: {line!r}")
    return _UciOption(
        name,
        option_type,
        values["default"][0] if values["default"] else None,
        minimum,
        maximum,
        tuple(values["var"]),
    )


def _option_integer(values: Sequence[str], field: str, line: str) -> int | None:
    if not values:
        return None
    if re.fullmatch(r"-?[0-9]+", values[0]) is None:
        raise StockfishAcquisitionError(f"malformed option {field}: {line!r}")
    return int(values[0])


def _resolve_options(
    identity: _EngineIdentity,
    requested_options: Mapping[str, Any],
    multipv: int,
) -> tuple[dict[str, Any], str | None]:
    declarations = {option.name.casefold(): option for option in identity.options}
    for required in ("threads", "hash", "multipv"):
        if required not in declarations:
            raise StockfishAcquisitionError(f"Stockfish lacks required option {required!r}")
    requested = dict(requested_options)
    supplied_names = {name.casefold(): name for name in requested}
    if "threads" not in supplied_names:
        requested["Threads"] = 1
    if "hash" not in supplied_names:
        requested["Hash"] = 16
    if "multipv" in supplied_names:
        supplied = requested[supplied_names["multipv"]]
        if isinstance(supplied, bool) or not isinstance(supplied, int) or supplied != multipv:
            raise StockfishAcquisitionError("engine_options.MultiPV must equal config multipv")
    else:
        requested["MultiPV"] = multipv

    applied: dict[str, Any] = {}
    for supplied_name, value in requested.items():
        option = declarations.get(supplied_name.casefold())
        if option is None:
            raise StockfishAcquisitionError(f"Stockfish does not declare {supplied_name!r}")
        if option.option_type == "button":
            raise StockfishAcquisitionError(f"button option {option.name!r} is adapter-managed")
        _validate_option_value(option, value)
        applied[option.name] = value
    for required in ("threads", "hash"):
        value = applied[declarations[required].name]
        if isinstance(value, bool) or not isinstance(value, int) or value < 1:
            raise StockfishAcquisitionError(f"{declarations[required].name} must be positive")
    clear_hash = declarations.get("clear hash")
    if clear_hash is not None and clear_hash.option_type != "button":
        raise StockfishAcquisitionError("Clear Hash is not declared as a button")
    ordered = dict(sorted(applied.items(), key=lambda item: item[0].casefold()))
    return ordered, clear_hash.name if clear_hash else None


def _validate_option_value(option: _UciOption, value: Any) -> None:
    if option.option_type == "spin":
        if isinstance(value, bool) or not isinstance(value, int):
            raise StockfishAcquisitionError(f"spin option {option.name!r} requires an integer")
        if option.minimum is not None and value < option.minimum:
            raise StockfishAcquisitionError(f"option {option.name!r} is below minimum")
        if option.maximum is not None and value > option.maximum:
            raise StockfishAcquisitionError(f"option {option.name!r} exceeds maximum")
    elif option.option_type == "check":
        if not isinstance(value, bool) and str(value).casefold() not in {"true", "false"}:
            raise StockfishAcquisitionError(f"check option {option.name!r} requires true/false")
    elif option.option_type == "combo":
        if not isinstance(value, str) or value not in option.variants:
            raise StockfishAcquisitionError(f"combo option {option.name!r} has invalid value")
    elif option.option_type == "string" and not isinstance(value, str):
        raise StockfishAcquisitionError(f"string option {option.name!r} requires a string")


def _configure(
    session: _UciSession, options: Mapping[str, Any], *, timeout_seconds: float
) -> None:
    for name, value in options.items():
        session.send(f"setoption name {name} value {_uci_value(value)}")
    _ready(session, timeout_seconds, "configured readiness")


def _ready(session: _UciSession, timeout_seconds: float, context: str) -> None:
    session.send("isready")
    session.read_until(
        lambda line: line == "readyok", timeout_seconds=timeout_seconds, context=context
    )


def _clear_hash(
    session: _UciSession, clear_hash_name: str | None, timeout_seconds: float
) -> None:
    if clear_hash_name is not None:
        session.send(f"setoption name {clear_hash_name}")
        _ready(session, timeout_seconds, "hash-clear readiness")


def _run_search(
    session: _UciSession,
    board: chess.Board,
    *,
    depth: int,
    configured_multipv: int,
    expected_lines: int,
    searchmove: str | None,
    timeout_seconds: float,
) -> tuple[tuple[_SearchLine, ...], int]:
    session.send(f"position fen {_board_fen(board)}")
    command = f"go depth {depth}"
    if searchmove is not None:
        command += f" searchmoves {searchmove}"
    session.send(command)
    output = session.read_until(
        lambda line: line.startswith("bestmove "),
        timeout_seconds=timeout_seconds,
        context=f"bestmove at depth {depth}",
    )
    bestmove, ponder = _parse_bestmove(output[-1])
    infos: dict[int, list[_ParsedInfo]] = {}
    for line in output[:-1]:
        info = _parse_info(line)
        if info is None:
            continue
        if not 1 <= info.multipv_index <= configured_multipv:
            raise StockfishAcquisitionError(
                f"out-of-range MultiPV index {info.multipv_index}"
            )
        _replay(board, info.moves)
        if searchmove is not None and info.moves[0] != searchmove:
            raise StockfishAcquisitionError("searchmoves info starts with another move")
        infos.setdefault(info.multipv_index, []).append(info)

    if expected_lines == 0:
        if bestmove not in {"0000", "(none)"} or ponder is not None:
            raise StockfishAcquisitionError("terminal search returned a move or ponder")
        return (), 0
    if bestmove in {"0000", "(none)"}:
        raise StockfishAcquisitionError("non-terminal search returned no bestmove")
    _replay(board, (bestmove,) + ((ponder,) if ponder else ()))
    if searchmove is not None and bestmove != searchmove:
        raise StockfishAcquisitionError("bestmove violates searchmoves")
    impossible = sorted(
        index
        for index, candidates in infos.items()
        if index > expected_lines
        and any(info.depth == depth and info.score_bound is None for info in candidates)
    )
    if impossible:
        raise StockfishAcquisitionError(f"impossible final MultiPV indices: {impossible}")

    selected: list[_SearchLine] = []
    seen_moves: set[str] = set()
    for index in range(1, expected_lines + 1):
        candidates = [
            info
            for info in infos.get(index, [])
            if info.depth == depth and info.score_bound is None
        ]
        if not candidates:
            raise StockfishAcquisitionError(
                f"missing exact MultiPV {index} at fixed depth {depth}"
            )
        info = candidates[-1]
        root_move = info.moves[0]
        if root_move in seen_moves:
            raise StockfishAcquisitionError("duplicate root move in final MultiPV")
        seen_moves.add(root_move)
        final_board = _replay(board, info.moves)
        selected.append(
            _SearchLine(
                multipv_index=index,
                uci_multipv_index=info.multipv_index,
                root_move=root_move,
                moves=info.moves,
                depth=info.depth,
                selective_depth=info.selective_depth,
                nodes=info.nodes,
                score_kind=info.score_kind,
                score_value=info.score_value,
                final_fen=_board_fen(final_board),
                search_kind="root-multipv" if searchmove is None else "played-move-searchmoves",
            )
        )
    if selected[0].root_move != bestmove:
        raise StockfishAcquisitionError("bestmove disagrees with final MultiPV 1")
    return tuple(selected), max(line.nodes for line in selected)


def _parse_bestmove(line: str) -> tuple[str, str | None]:
    match = re.fullmatch(
        r"bestmove (\(none\)|0000|[a-h][1-8][a-h][1-8][qrbn]?)"
        r"(?: ponder ([a-h][1-8][a-h][1-8][qrbn]?))?",
        line,
    )
    if match is None:
        raise StockfishAcquisitionError(f"malformed bestmove line: {line!r}")
    return match.group(1), match.group(2)


def _parse_info(line: str) -> _ParsedInfo | None:
    if not line.startswith("info "):
        return None
    has_score = re.search(r"(?:^| )score(?: |$)", line) is not None
    has_pv = re.search(r"(?:^| )pv(?: |$)", line) is not None
    if not (has_score and has_pv):
        return None
    depth = _integer_field(line, "depth")
    nodes = _integer_field(line, "nodes")
    multipv = _integer_field(line, "multipv", required=False)
    selective_depth = _integer_field(line, "seldepth", required=False)
    score = list(
        re.finditer(
            r"(?:^| )score (cp|mate) (-?[0-9]+)(?: (lowerbound|upperbound))?(?= |$)",
            line,
        )
    )
    pv = list(re.finditer(r"(?:^| )pv (.*)$", line))
    if depth is None or nodes is None or len(score) != 1 or len(pv) != 1:
        raise StockfishAcquisitionError(f"malformed scored info line: {line!r}")
    moves = tuple(pv[0].group(1).strip().split())
    if not moves or any(_MOVE_RE.fullmatch(move) is None for move in moves):
        raise StockfishAcquisitionError(f"malformed PV in info line: {line!r}")
    if depth < 0 or nodes < 0 or (multipv is not None and multipv < 1):
        raise StockfishAcquisitionError(f"invalid UCI counter in info line: {line!r}")
    if selective_depth is not None and selective_depth < 0:
        raise StockfishAcquisitionError(f"negative seldepth in info line: {line!r}")
    return _ParsedInfo(
        multipv or 1,
        moves,
        depth,
        selective_depth,
        nodes,
        score[0].group(1),
        int(score[0].group(2)),
        score[0].group(3),
    )


def _integer_field(line: str, field: str, *, required: bool = True) -> int | None:
    values = re.findall(rf"(?:^| ){re.escape(field)} (-?[0-9]+)(?= |$)", line)
    if len(values) > 1 or (required and not values):
        raise StockfishAcquisitionError(f"malformed {field} field: {line!r}")
    return int(values[0]) if values else None


def _replay(board: chess.Board, moves: Sequence[str]) -> chess.Board:
    replay = board.copy(stack=False)
    for ply, uci in enumerate(moves, 1):
        try:
            move = chess.Move.from_uci(uci)
        except ValueError as error:
            raise StockfishAcquisitionError(f"malformed PV move at ply {ply}: {uci}") from error
        if not replay.is_legal(move):
            raise StockfishAcquisitionError(f"illegal engine PV at ply {ply}: {uci}")
        replay.push(move)
    return replay


def _assemble_pack(
    *,
    question: dict[str, Any],
    config: dict[str, Any],
    board: chess.Board,
    repetitions: tuple[_Repetition, ...],
    identity: _EngineIdentity,
    executable_sha256: str,
    applied_options: Mapping[str, Any],
) -> tuple[dict[str, Any], dict[str, Any]]:
    configuration = {
        "executable_sha256": executable_sha256,
        "engine_name": identity.name,
        "engine_version": identity.version,
        "depth": config["depth"],
        "multipv": config["multipv"],
        "repetitions": config["repetitions"],
        "score_perspective": "root-side",
        "applied_options": dict(applied_options),
    }
    configuration_hash = _sha256_json(configuration)
    provider_id = _stable_id("provider-stockfish", configuration)
    provider = {
        "provider_id": provider_id,
        "kind": "engine",
        "name": identity.name,
        "version": identity.version,
        "configuration_hash": configuration_hash,
    }
    board_configuration = {
        "name": "python-chess",
        "version": chess.__version__,
        "role": "fen-legality-and-pv-replay",
    }
    board_provider_id = _stable_id("provider-board", board_configuration)
    board_provider = {
        "provider_id": board_provider_id,
        "kind": "board-library",
        "name": "python-chess",
        "version": chess.__version__,
        "configuration_hash": _sha256_json(board_configuration),
    }
    position_id = str(question["root_position"]["position_id"])
    first = repetitions[0]

    candidate_lines: list[dict[str, Any]] = []
    forced_required_lines: list[dict[str, Any]] = []
    line_ids: dict[tuple[int, str, str], str] = {}
    for line in first.all_lines:
        line_id = _stable_id(
            "line",
            {
                "provider_id": provider_id,
                "board_provider_id": board_provider_id,
                "position_id": position_id,
                "multipv_index": line.multipv_index,
                "root_move": line.root_move,
                "search_kind": line.search_kind,
            },
        )
        line_ids[(line.multipv_index, line.root_move, line.search_kind)] = line_id
        document: dict[str, Any] = {
            "line_id": line_id,
            "root_position_id": position_id,
            "root_move_uci": line.root_move,
            "moves_uci": list(line.moves),
            "depth": line.depth,
            "nodes": line.nodes,
            "evaluation": line.evaluation(),
            "legality": {
                "legal": True,
                "replayed_plies": len(line.moves),
                "final_fen": line.final_fen,
            },
        }
        if line.selective_depth is not None:
            document["selective_depth"] = line.selective_depth
        if line.search_kind == "root-multipv":
            document["multipv_index"] = line.multipv_index
            candidate_lines.append(document)
        else:
            document["search_kind"] = line.search_kind
            forced_required_lines.append(document)

    repetition_documents: list[dict[str, Any]] = []
    acquisition_ids: list[str] = []
    for repetition in repetitions:
        acquisition_id = _stable_id(
            "acquisition",
            {
                "provider_id": provider_id,
                "position_id": position_id,
                "repetition_index": repetition.index,
            },
        )
        acquisition_ids.append(acquisition_id)
        repetition_documents.append(
            {
                "acquisition_id": acquisition_id,
                "repetition_index": repetition.index,
                "provider_id": provider_id,
                "depth": int(config["depth"]),
                "nodes": repetition.total_nodes,
                "lines": [
                    {
                        "multipv_index": line.multipv_index,
                        "root_move_uci": line.root_move,
                        "evaluation": line.evaluation(),
                    }
                    for line in repetition.root_lines
                ],
                "forced_required_lines": [
                    {
                        "root_move_uci": line.root_move,
                        "evaluation": line.evaluation(),
                        "search_kind": line.search_kind,
                    }
                    for line in repetition.forced_required_lines
                ],
            }
        )

    provider_evidence_id = _stable_id("evidence-provider", configuration)
    board_evidence_id = _stable_id("evidence-provider", board_configuration)
    provider_attributes: dict[str, Any] = {
        "applied_options_json": _json_text(dict(applied_options)),
        "engine_id_name": identity.name,
        "engine_version": identity.version,
        "executable_sha256": executable_sha256,
        "score_perspective": "root-side-to-move",
        "uci_option_declarations_json": _json_text(
            [option.as_dict() for option in identity.options]
        ),
    }
    if identity.author:
        provider_attributes["engine_id_author"] = identity.author
    evidence_records: list[dict[str, Any]] = [
        {
            "evidence_id": provider_evidence_id,
            "kind": "engine-provider-provenance",
            "parent_evidence_ids": [],
            "attributes": _attributes(provider_attributes),
        },
        {
            "evidence_id": board_evidence_id,
            "kind": "board-library-provenance",
            "parent_evidence_ids": [],
            "attributes": _attributes(board_configuration),
        },
    ]
    for repetition, acquisition_id in zip(repetitions, acquisition_ids, strict=True):
        evidence_records.append(
            {
                "evidence_id": _stable_id("evidence-acquisition", acquisition_id),
                "kind": "engine-acquisition",
                "parent_evidence_ids": [provider_evidence_id],
                "position_id": position_id,
                "attributes": _attributes(
                    {
                        "acquisition_id": acquisition_id,
                        "depth": int(config["depth"]),
                        "nodes": repetition.total_nodes,
                        "raw_io_capture_key": f"repetitions/{repetition.index}",
                        "repetition_index": repetition.index,
                    }
                ),
            }
        )
    for line in first.all_lines:
        line_id = line_ids[(line.multipv_index, line.root_move, line.search_kind)]
        evidence_records.append(
            {
                "evidence_id": _stable_id("evidence-line", line_id),
                "kind": "engine-search-line",
                "parent_evidence_ids": [provider_evidence_id, board_evidence_id],
                "position_id": position_id,
                "line_id": line_id,
                "attributes": _attributes(
                    {
                        "acquisition_id": acquisition_ids[0],
                        "raw_io_capture_key": "repetitions/0",
                        "score_perspective": "root-side-to-move",
                        "search_kind": line.search_kind,
                        "uci_multipv_index": line.uci_multipv_index,
                    }
                ),
            }
        )

    missing, probes, tablebase = _missing_states(
        question,
        config,
        repetitions,
        provider_id,
        position_id,
        len(board.piece_map()),
    )
    diagnostics = _diagnostics(question, config, repetitions, len(list(board.legal_moves)), missing)
    pack = {
        "question": question,
        "acquisition_config": config,
        "providers": [provider, board_provider],
        "candidate_lines": candidate_lines,
        "forced_required_lines": forced_required_lines,
        "repetitions": repetition_documents,
        "probes": probes,
        "tablebase_results": tablebase,
        "evidence_records": evidence_records,
        "missing_evidence": missing,
    }
    return pack, diagnostics


def _missing_states(
    question: Mapping[str, Any],
    config: Mapping[str, Any],
    repetitions: Sequence[_Repetition],
    provider_id: str,
    position_id: str,
    piece_count: int,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    orders = [tuple(line.root_move for line in repetition.root_lines) for repetition in repetitions]
    acquired = [
        {line.root_move for line in repetition.all_lines}
        for repetition in repetitions
    ]
    terminal = not orders[0]
    missing: list[dict[str, Any]] = [
        _missing_entry(
            "root-multipv",
            "not-applicable" if terminal else "present",
            "terminal-position" if terminal else None,
            "root has no legal moves" if terminal else None,
        ),
        _missing_entry(
            "played-move-line",
            "not-applicable" if question["played_move_uci"] == "0000" else "present",
            "terminal-position" if question["played_move_uci"] == "0000" else None,
            "played move is the terminal null marker"
            if question["played_move_uci"] == "0000"
            else None,
        ),
    ]
    for move in sorted(question.get("requested_reference_moves_uci", [])):
        count = sum(move in moves for moves in acquired)
        if move == "0000":
            missing.append(
                _missing_entry(
                    f"requested-reference-move:{move}",
                    "not-applicable",
                    "terminal-position",
                    "reference is the terminal null marker",
                )
            )
        elif count == len(repetitions):
            missing.append(_missing_entry(f"requested-reference-move:{move}", "present"))
        else:
            missing.append(
                _missing_entry(
                    f"requested-reference-move:{move}",
                    "missing",
                    "required-move-acquisition-incomplete",
                    f"acquired in {count}/{len(repetitions)} repetitions",
                )
            )
    stability = _stability(repetitions, int(config["stability_tolerance_cp"]))
    missing.append(
        _missing_entry(
            "top-k-repetition-stability",
            "present" if stability["stable"] else "failed",
            None if stability["stable"] else "unstable-root-multipv",
            None
            if stability["stable"]
            else "order, membership, score kind, or score tolerance changed",
        )
    )

    probes: list[dict[str, Any]] = []
    for kind in sorted(config["probe_kinds"]):
        probes.append(
            {
                "probe_id": _stable_id(
                    "probe", {"provider_id": provider_id, "position_id": position_id, "kind": kind}
                ),
                "kind": kind,
                "provider_id": provider_id,
                "status": "unsupported",
                "attributes": [],
                "failure_code": "stockfish-uci-probe-unsupported",
            }
        )
        missing.append(
            _missing_entry(
                f"probe:{kind}",
                "unsupported",
                "stockfish-uci-probe-unsupported",
                "this adapter captures UCI search evidence only",
            )
        )

    mode = str(config["tablebase"])
    if mode == "disabled":
        status, reason, detail = (
            "not-applicable",
            "tablebase-disabled",
            "tablebase acquisition is disabled",
        )
    elif piece_count > 7:
        status, reason, detail = (
            "not-applicable",
            "position-outside-tablebase-range",
            f"position has {piece_count} pieces",
        )
    else:
        status, reason, detail = (
            "unsupported",
            "stockfish-uci-tablebase-result-unsupported",
            "UCI search does not expose a standalone WDL/DTZ result",
        )
    tablebase_result: dict[str, Any] = {
        "tablebase_id": _stable_id(
            "tablebase", {"provider_id": provider_id, "position_id": position_id}
        ),
        "provider_id": provider_id,
        "position_id": position_id,
        "status": status,
    }
    if status == "unsupported":
        tablebase_result["failure_code"] = reason
    missing.append(_missing_entry("tablebase", status, reason, detail))
    return missing, probes, [tablebase_result]


def _diagnostics(
    question: Mapping[str, Any],
    config: Mapping[str, Any],
    repetitions: Sequence[_Repetition],
    legal_move_count: int,
    missing: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    orders = [tuple(line.root_move for line in repetition.root_lines) for repetition in repetitions]
    required_moves = sorted(
        {
            move
            for move in [question["played_move_uci"], *question.get("requested_reference_moves_uci", [])]
            if move != "0000"
        }
    )
    coverage = {
        move: {
            "covered_repetitions": sum(move in order for order in orders),
            "total_repetitions": len(repetitions),
            "coverage_rate": sum(move in order for order in orders) / len(repetitions),
        }
        for move in required_moves
    }
    covered_cells = sum(item["covered_repetitions"] for item in coverage.values())
    total_cells = len(required_moves) * len(repetitions)
    played = str(question["played_move_uci"])
    played_acquired = (
        sum(
            any(line.root_move == played for line in repetition.all_lines)
            for repetition in repetitions
        )
        if played != "0000"
        else 0
    )

    order_counts: dict[tuple[str, ...], int] = {}
    for order in orders:
        order_counts[order] = order_counts.get(order, 0) + 1
    distribution = [
        {"moves_uci": list(order), "repetitions": count}
        for order, count in sorted(order_counts.items(), key=lambda item: (-item[1], item[0]))
    ]
    pairwise_jaccard: list[float] = []
    for left_index in range(len(orders)):
        for right_index in range(left_index + 1, len(orders)):
            left, right = set(orders[left_index]), set(orders[right_index])
            union = left | right
            pairwise_jaccard.append(len(left & right) / len(union) if union else 1.0)

    by_move: dict[str, Any] = {}
    variances: list[float] = []
    mate_checks: list[bool] = []
    for move in sorted({move for order in orders for move in order}):
        observed: list[tuple[int, _SearchLine]] = []
        for repetition in repetitions:
            match = next((line for line in repetition.root_lines if line.root_move == move), None)
            if match:
                observed.append((repetition.index, match))
        kinds = sorted({line.score_kind for _, line in observed})
        item: dict[str, Any] = {
            "observations": len(observed),
            "repetition_indices": [index for index, _ in observed],
            "multipv_indices": [line.multipv_index for _, line in observed],
            "score_kinds": kinds,
            "present_in_every_repetition": len(observed) == len(repetitions),
        }
        if kinds == ["cp"]:
            values = [line.score_value for _, line in observed]
            variance = float(pvariance(values))
            variances.append(variance)
            item.update(
                {
                    "centipawn_mean": fmean(values),
                    "centipawn_min": min(values),
                    "centipawn_max": max(values),
                    "centipawn_range": max(values) - min(values),
                    "centipawn_population_variance": variance,
                }
            )
        elif "mate" in kinds:
            values = [line.score_value for _, line in observed if line.score_kind == "mate"]
            reproducible = (
                kinds == ["mate"]
                and len(observed) == len(repetitions)
                and len(set(values)) == 1
            )
            mate_checks.append(reproducible)
            item.update({"mate_in_values": values, "mate_reproducible": reproducible})
        by_move[move] = item

    stability = _stability(repetitions, int(config["stability_tolerance_cp"]))
    applicable = [entry for entry in missing if entry["status"] != "not-applicable"]
    unavailable = [entry for entry in applicable if entry["status"] != "present"]
    return {
        "score_perspective": "root-side-to-move",
        "fixed_depth": int(config["depth"]),
        "configured_multipv": int(config["multipv"]),
        "expected_top_k": min(int(config["multipv"]), legal_move_count),
        "repetitions": len(repetitions),
        "top_k_coverage": {
            "required_moves": coverage,
            "covered_cells": covered_cells,
            "total_cells": total_cells,
            "coverage_rate": covered_cells / total_cells if total_cells else 1.0,
            "played_move_acquired_repetitions": played_acquired,
            "played_move_acquired_rate": played_acquired / len(repetitions) if played != "0000" else None,
        },
        "top_k_stability": {
            **stability,
            "order_distribution": distribution,
            "modal_order_rate": max(order_counts.values()) / len(repetitions),
            "mean_pairwise_set_jaccard": fmean(pairwise_jaccard) if pairwise_jaccard else 1.0,
        },
        "evaluation_variance": {
            "by_root_move": by_move,
            "max_centipawn_population_variance": max(variances) if variances else None,
            "mate_reproducible": all(mate_checks) if mate_checks else None,
        },
        "nodes": {
            "by_repetition": [repetition.total_nodes for repetition in repetitions],
            "minimum": min(repetition.total_nodes for repetition in repetitions),
            "maximum": max(repetition.total_nodes for repetition in repetitions),
            "mean": fmean(repetition.total_nodes for repetition in repetitions),
        },
        "missing_evidence": {
            "applicable_requirements": len(applicable),
            "unavailable_requirements": len(unavailable),
            "rate": len(unavailable) / len(applicable) if applicable else 0.0,
            "status_counts": {
                status: sum(entry["status"] == status for entry in missing)
                for status in ("present", "not-applicable", "missing", "failed", "unsupported")
            },
        },
    }


def _stability(repetitions: Sequence[_Repetition], tolerance_cp: int) -> dict[str, Any]:
    orders = [tuple(line.root_move for line in repetition.root_lines) for repetition in repetitions]
    order_stable = all(order == orders[0] for order in orders[1:])
    set_stable = all(set(order) == set(orders[0]) for order in orders[1:])
    score_stable = True
    max_range: int | None = None
    for move in sorted({move for order in orders for move in order}):
        scores: list[tuple[str, int]] = []
        for repetition in repetitions:
            match = next((line for line in repetition.root_lines if line.root_move == move), None)
            if match is None:
                score_stable = False
            else:
                scores.append((match.score_kind, match.score_value))
        kinds = {kind for kind, _ in scores}
        if kinds == {"cp"}:
            values = [value for _, value in scores]
            score_range = max(values) - min(values)
            max_range = score_range if max_range is None else max(max_range, score_range)
            if score_range > tolerance_cp:
                score_stable = False
        elif kinds == {"mate"}:
            if len({value for _, value in scores}) != 1:
                score_stable = False
        else:
            score_stable = False
    return {
        "stable": order_stable and set_stable and score_stable,
        "order_stable": order_stable,
        "set_stable": set_stable,
        "score_stable": score_stable,
        "stability_tolerance_cp": tolerance_cp,
        "max_centipawn_range": max_range,
    }


def _missing_entry(
    requirement: str,
    status: str,
    reason: str | None = None,
    detail: str | None = None,
) -> dict[str, Any]:
    result = {"requirement": requirement, "status": status}
    if reason:
        result["reason_code"] = reason
    if detail:
        result["detail"] = detail
    return result


def _board_fen(board: chess.Board) -> str:
    return board.fen(en_passant="fen")


def _schema_move(value: Any, location: str) -> None:
    if not isinstance(value, str) or (value != "0000" and _MOVE_RE.fullmatch(value) is None):
        raise StockfishAcquisitionError(f"{location} is not a schema-valid UCI move")


def _integer(value: Any, location: str, minimum: int) -> None:
    if isinstance(value, bool) or not isinstance(value, int) or value < minimum:
        raise StockfishAcquisitionError(f"{location} must be an integer >= {minimum}")


def _exact_keys(
    value: Mapping[str, Any],
    required: set[str],
    allowed: set[str],
    location: str,
) -> None:
    missing = sorted(required - set(value))
    extras = sorted(set(value) - allowed)
    if missing:
        raise StockfishAcquisitionError(f"{location} is missing fields: {missing}")
    if extras:
        raise StockfishAcquisitionError(f"{location} has forbidden fields: {extras}")


def _json_clone(value: Any, location: str) -> Any:
    try:
        return json.loads(json.dumps(value, ensure_ascii=False, allow_nan=False))
    except (TypeError, ValueError) as error:
        raise StockfishAcquisitionError(f"{location} is not finite JSON data") from error


def _json_scalar(value: Any) -> bool:
    return isinstance(value, (str, bool, int)) or (
        isinstance(value, float) and math.isfinite(value)
    )


def _uci_value(value: Any) -> str:
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        return format(value, ".17g")
    return str(value)


def _attributes(values: Mapping[str, Any]) -> list[dict[str, Any]]:
    return [{"key": key, "value": values[key]} for key in sorted(values)]


def _json_text(value: Any) -> str:
    return json.dumps(
        value, ensure_ascii=False, sort_keys=True, separators=(",", ":"), allow_nan=False
    )


def _stable_id(prefix: str, value: Any) -> str:
    return f"{prefix}-{_sha256_json(value)[:24]}"


def _sha256_json(value: Any) -> str:
    return hashlib.sha256(_json_text(value).encode("utf-8")).hexdigest()


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    try:
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(chunk)
    except OSError as error:
        raise StockfishAcquisitionError(f"cannot hash Stockfish executable: {error}") from error
    return digest.hexdigest()


__all__ = [
    "StockfishAcquisitionError",
    "StockfishAcquisitionResult",
    "acquire_stockfish_evidence",
]
