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

from .hashing import sha256_bytes
from .model import ContractError, IntegrityError
from .schemas import SchemaRegistry


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
            name="chesstory-runtime-jsonl-reader",
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
