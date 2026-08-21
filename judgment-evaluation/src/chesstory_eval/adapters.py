from __future__ import annotations

import base64
import hashlib
import json
import os
import re
import stat
import subprocess
from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterator, Mapping, Sequence

from .hashing import read_jsonl, sha256_json
from .model import (
    ContractError,
    IntegrityError,
    Source,
    STAGES,
    StageSampleView,
    StageUnavailable,
    require_stage,
)


_LIVE_COMMAND_CONFIG_SCHEMA = "chesstory.eval.live-command-config.v1"
_WINDOWS_BOUND_FILE_SECURITY_MODE = "windows-deny-write-delete-open-handle-v1"
_UNSUPPORTED_BOUND_FILE_SECURITY_MODE = "unsupported-platform-release-fail-closed-v1"
_SHA256_RE = re.compile(r"[0-9a-f]{64}")
_SECRET_ENV_KEY = re.compile(
    r"(?:^|_)(?:API_?KEY|AUTH|BEARER|COOKIE|CREDENTIALS?|HMAC|PASSWORD|PASSWD|"
    r"PRIVATE_?KEY|SECRET|SESSION|SIGNING_?KEY|TOKEN)(?:_|$)",
    re.IGNORECASE,
)
_SECRET_ENV_VALUE = re.compile(
    r"(?:-----BEGIN [A-Z ]*PRIVATE KEY-----|(?:^|[^A-Za-z0-9])sk-[A-Za-z0-9_-]{12,}|"
    r"(?:^|[^A-Za-z0-9])gh[pousr]_[A-Za-z0-9]{20,}|"
    r"(?:^|[^A-Za-z0-9])xox[baprs]-[A-Za-z0-9-]{12,}|"
    r"(?:^|[^A-Za-z0-9])AKIA[A-Z0-9]{16}(?:$|[^A-Za-z0-9])|"
    r"\beyJ[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\b|"
    r"[A-Za-z][A-Za-z0-9+.-]*://[^\s/:]+:[^\s/@]+@)",
    re.IGNORECASE,
)


def _reject_json_constant(value: str) -> Any:
    raise ValueError(f"non-finite JSON constant is forbidden: {value}")


def _unique_json_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate JSON object key: {key!r}")
        result[key] = value
    return result


def strict_json_loads(value: str, *, location: str) -> Any:
    """Parse finite JSON without silently accepting duplicate object keys."""

    try:
        return json.loads(
            value,
            object_pairs_hook=_unique_json_object,
            parse_constant=_reject_json_constant,
        )
    except (json.JSONDecodeError, UnicodeError, ValueError, RecursionError) as error:
        raise ContractError(f"{location}: invalid strict JSON: {error}") from error


def _bound_file_security_mode() -> str:
    return (
        _WINDOWS_BOUND_FILE_SECURITY_MODE
        if os.name == "nt"
        else _UNSUPPORTED_BOUND_FILE_SECURITY_MODE
    )


def _assert_regular_non_link_file(path: Path, *, location: str) -> Path:
    supplied = Path(path)
    if not supplied.is_absolute():
        raise ContractError(f"{location} must be an absolute path")
    current = Path(supplied.anchor)
    for part in supplied.parts[1:]:
        current /= part
        try:
            is_link = current.is_symlink()
            is_junction = bool(getattr(current, "is_junction", lambda: False)())
        except OSError as error:
            raise ContractError(f"{location} link status is unreadable: {current}") from error
        if is_link or is_junction:
            raise ContractError(f"{location} must not traverse a symlink or junction")
    try:
        resolved = supplied.resolve(strict=True)
        metadata = os.stat(resolved, follow_symlinks=False)
    except OSError as error:
        raise ContractError(f"{location} is unavailable: {supplied}") from error
    if str(supplied) != str(resolved):
        raise ContractError(
            f"{location} must use its exact canonical absolute path: {resolved}"
        )
    if not stat.S_ISREG(metadata.st_mode):
        raise ContractError(f"{location} must be a regular file")
    return resolved


def _file_sha256_and_size(path: Path) -> tuple[str, int]:
    digest = hashlib.sha256()
    size = 0
    try:
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(chunk)
                size += len(chunk)
    except OSError as error:
        raise IntegrityError(f"bound command file is unreadable: {path}") from error
    return digest.hexdigest(), size


def _existing_argv_file(argument: str) -> Path | None:
    candidates = [argument]
    if argument.startswith("@") and len(argument) > 1:
        candidates.append(argument[1:])
    if "=" in argument:
        candidates.append(argument.split("=", 1)[1])
    for candidate in candidates:
        if not candidate or "\x00" in candidate:
            continue
        try:
            path = Path(candidate)
            inspected = path if path.is_absolute() else Path.cwd() / path
            if inspected.is_symlink() or bool(
                getattr(inspected, "is_junction", lambda: False)()
            ):
                return inspected
            if inspected.is_file():
                return inspected
        except (OSError, ValueError):
            continue
    return None


@dataclass(frozen=True)
class BoundCommandFile:
    """One argv file whose exact path and bytes are frozen by the candidate."""

    argv_index: int
    path: Path
    sha256: str
    size: int

    @classmethod
    def create(
        cls,
        value: Mapping[str, Any],
        *,
        argv: Sequence[str],
        location: str,
    ) -> "BoundCommandFile":
        if not isinstance(value, Mapping) or set(value) != {
            "argv_index",
            "path",
            "sha256",
            "size",
        }:
            raise ContractError(
                f"{location} must contain exactly argv_index, path, sha256, size"
            )
        argv_index = value.get("argv_index")
        if (
            isinstance(argv_index, bool)
            or not isinstance(argv_index, int)
            or argv_index < 0
            or argv_index >= len(argv)
        ):
            raise ContractError(f"{location}.argv_index is outside argv")
        path_text = value.get("path")
        if not isinstance(path_text, str) or not path_text or "\x00" in path_text:
            raise ContractError(f"{location}.path must be non-empty non-NUL text")
        resolved = _assert_regular_non_link_file(
            Path(path_text), location=f"{location}.path"
        )
        if argv[argv_index] != str(resolved):
            raise ContractError(
                f"{location}.path must exactly equal argv[{argv_index}]"
            )
        digest = value.get("sha256")
        if not isinstance(digest, str) or not _SHA256_RE.fullmatch(digest):
            raise ContractError(f"{location}.sha256 must be a lowercase SHA-256")
        size = value.get("size")
        if isinstance(size, bool) or not isinstance(size, int) or size < 0:
            raise ContractError(f"{location}.size must be a non-negative integer")
        frozen = cls(
            argv_index=argv_index,
            path=resolved,
            sha256=digest,
            size=size,
        )
        frozen.assert_unchanged(location=location)
        return frozen

    def as_document(self) -> dict[str, Any]:
        return {
            "argv_index": self.argv_index,
            "path": str(self.path),
            "sha256": self.sha256,
            "size": self.size,
        }

    def assert_unchanged(self, *, location: str) -> None:
        resolved = _assert_regular_non_link_file(self.path, location=f"{location}.path")
        digest, size = _file_sha256_and_size(resolved)
        if digest != self.sha256 or size != self.size:
            raise IntegrityError(
                f"{location} bytes changed: expected {self.sha256}/{self.size}, "
                f"found {digest}/{size}"
            )


@dataclass(frozen=True)
class CommandSpec:
    """One shell-free, secret-free external command route."""

    argv: tuple[str, ...]
    timeout_seconds: int
    environment: tuple[tuple[str, str], ...]
    bound_files: tuple[BoundCommandFile, ...] = ()

    @classmethod
    def create(
        cls,
        argv: Sequence[str],
        *,
        timeout_seconds: int,
        environment: Mapping[str, str],
        location: str,
        bound_files: Sequence[Mapping[str, Any]] | None = None,
        require_bound_files: bool = False,
    ) -> "CommandSpec":
        if isinstance(argv, (str, bytes)):
            raise ContractError(f"{location}.argv must be an argument array, not a shell string")
        frozen_argv = tuple(argv)
        if not frozen_argv or any(
            not isinstance(argument, str)
            or not argument
            or "\x00" in argument
            for argument in frozen_argv
        ):
            raise ContractError(f"{location}.argv must contain non-empty non-NUL strings")
        if (
            not isinstance(timeout_seconds, int)
            or isinstance(timeout_seconds, bool)
            or timeout_seconds <= 0
            or timeout_seconds > 3600
        ):
            raise ContractError(
                f"{location}.timeout_seconds must be an integer from 1 through 3600"
            )
        validated_environment = CommandStageAdapter._validated_environment(
            environment, location=f"{location}.env"
        )
        raw_bound_files = bound_files if bound_files is not None else ()
        if isinstance(raw_bound_files, (str, bytes)) or not isinstance(
            raw_bound_files, Sequence
        ):
            raise ContractError(f"{location}.bound_files must be an array")
        frozen_bound_files = tuple(
            BoundCommandFile.create(
                value,
                argv=frozen_argv,
                location=f"{location}.bound_files[{index}]",
            )
            for index, value in enumerate(raw_bound_files)
        )
        indexes = [entry.argv_index for entry in frozen_bound_files]
        if indexes != sorted(set(indexes)):
            raise ContractError(
                f"{location}.bound_files must be ordered by unique argv_index"
            )
        if require_bound_files and (not frozen_bound_files or indexes[0] != 0):
            raise ContractError(
                f"{location}.bound_files must bind argv[0] as an absolute regular non-link file"
            )
        if frozen_bound_files and indexes[0] != 0:
            raise ContractError(f"{location}.bound_files must include argv[0]")
        bound_indexes = set(indexes)
        for index, argument in enumerate(frozen_argv):
            if index in bound_indexes:
                continue
            discovered = _existing_argv_file(argument)
            if discovered is not None:
                raise ContractError(
                    f"{location}.argv[{index}] names an existing file outside "
                    f"bound_files: {discovered}"
                )
        return cls(
            argv=frozen_argv,
            timeout_seconds=timeout_seconds,
            environment=tuple(sorted(validated_environment.items())),
            bound_files=frozen_bound_files,
        )

    def environment_dict(self) -> dict[str, str]:
        return dict(self.environment)

    def as_document(self) -> dict[str, Any]:
        return {
            "argv": list(self.argv),
            "timeout_seconds": self.timeout_seconds,
            "env": self.environment_dict(),
            "bound_files": [entry.as_document() for entry in self.bound_files],
        }

    def assert_bound_files_unchanged(self, *, location: str) -> None:
        if not self.bound_files:
            raise IntegrityError(f"{location} has no bound argv files")
        for entry in self.bound_files:
            entry.assert_unchanged(
                location=f"{location}.bound_files[{entry.argv_index}]"
            )
        bound_indexes = {entry.argv_index for entry in self.bound_files}
        for index, argument in enumerate(self.argv):
            if index in bound_indexes:
                continue
            discovered = _existing_argv_file(argument)
            if discovered is not None:
                raise IntegrityError(
                    f"{location}.argv[{index}] became an unbound file: {discovered}"
                )

    @contextmanager
    def invocation_guard(
        self, *, release_mode: bool, location: str
    ) -> Iterator[None]:
        """Keep every bound file write/delete locked across a release invocation."""

        if not release_mode:
            yield
            return
        if os.name != "nt":
            raise IntegrityError(
                f"{location} release execution requires {_WINDOWS_BOUND_FILE_SECURITY_MODE}; "
                f"platform advertises {_UNSUPPORTED_BOUND_FILE_SECURITY_MODE}"
            )
        if not self.bound_files:
            raise IntegrityError(f"{location} release route has no bound files")
        handles: list[int] = []
        try:
            for entry in self.bound_files:
                handles.append(_open_windows_read_guard(entry.path, location=location))
            self.assert_bound_files_unchanged(location=f"{location} pre-invocation")
            try:
                yield
            except BaseException as invocation_error:
                try:
                    self.assert_bound_files_unchanged(
                        location=f"{location} post-invocation"
                    )
                except BaseException as integrity_error:
                    raise integrity_error from invocation_error
                raise
            self.assert_bound_files_unchanged(location=f"{location} post-invocation")
        finally:
            _close_windows_handles(handles)


def _open_windows_read_guard(path: Path, *, location: str) -> int:
    if os.name != "nt":
        raise IntegrityError("Windows bound-file guard requested on another platform")
    import ctypes
    from ctypes import wintypes

    create_file = ctypes.windll.kernel32.CreateFileW
    create_file.argtypes = (
        wintypes.LPCWSTR,
        wintypes.DWORD,
        wintypes.DWORD,
        wintypes.LPVOID,
        wintypes.DWORD,
        wintypes.DWORD,
        wintypes.HANDLE,
    )
    create_file.restype = wintypes.HANDLE
    handle = create_file(
        str(path),
        0x80000000,  # GENERIC_READ
        0x00000001,  # FILE_SHARE_READ; deliberately no write/delete sharing
        None,
        3,  # OPEN_EXISTING
        0x00200000,  # FILE_FLAG_OPEN_REPARSE_POINT
        None,
    )
    invalid_handle = wintypes.HANDLE(-1).value
    if handle == invalid_handle:
        error = ctypes.get_last_error()
        raise IntegrityError(
            f"{location} cannot acquire deny-write/delete guard for {path} "
            f"(Windows error {error})"
        )
    return int(handle)


def _close_windows_handles(handles: Sequence[int]) -> None:
    if os.name != "nt":
        return
    import ctypes
    from ctypes import wintypes

    close_handle = ctypes.windll.kernel32.CloseHandle
    close_handle.argtypes = (wintypes.HANDLE,)
    close_handle.restype = wintypes.BOOL
    for handle in reversed(handles):
        close_handle(wintypes.HANDLE(handle))


@dataclass(frozen=True)
class LiveCommandConfig:
    """Validated command routing frozen from one pre-open JSON byte sequence."""

    source_path: Path
    source_sha256: str
    document_sha256: str
    stage_routes: tuple[
        tuple[tuple[str, Source] | tuple[str, Source, str], CommandSpec], ...
    ]
    evaluator_route: CommandSpec
    _document_json: str

    @property
    def document(self) -> Mapping[str, Any]:
        value = strict_json_loads(
            self._document_json, location="frozen live-command config"
        )
        if not isinstance(value, Mapping):
            raise ContractError("frozen live-command config ceased to be an object")
        return value

    def stage_command_mapping(
        self,
    ) -> dict[tuple[str, Source] | tuple[str, Source, str], CommandSpec]:
        return dict(self.stage_routes)

    @property
    def security_mode(self) -> str:
        return _bound_file_security_mode()

    def bound_file_usages(self) -> list[dict[str, Any]]:
        usages = [
            {
                "route": _route_label(route),
                **bound_file.as_document(),
            }
            for route, command in self.stage_routes
            for bound_file in command.bound_files
        ]
        usages.extend(
            {
                "route": "evaluator",
                **bound_file.as_document(),
            }
            for bound_file in self.evaluator_route.bound_files
        )
        return sorted(
            usages,
            key=lambda entry: (
                str(entry["route"]),
                int(entry["argv_index"]),
                str(entry["path"]),
            ),
        )

    @property
    def bound_files_sha256(self) -> str:
        return sha256_json(self.bound_file_usages())

    def internal_bound_paths(self, *, component_root: Path) -> list[str]:
        root = Path(component_root).resolve(strict=True)
        internal: set[str] = set()
        for usage in self.bound_file_usages():
            path = Path(str(usage["path"]))
            try:
                relative = path.relative_to(root)
            except ValueError:
                continue
            internal.add(relative.as_posix())
        return sorted(internal)

    def external_executable_binding(
        self, *, component_root: Path
    ) -> dict[str, Any]:
        root = Path(component_root).resolve(strict=True)
        grouped: dict[str, dict[str, Any]] = {}
        for usage in self.bound_file_usages():
            absolute_path = str(usage["path"])
            try:
                Path(absolute_path).relative_to(root)
            except ValueError:
                pass
            else:
                continue
            entry = grouped.setdefault(
                absolute_path,
                {
                    "absolute_path": absolute_path,
                    "path_fingerprint_sha256": sha256_json(
                        {"normalized_absolute_path": os.path.normcase(absolute_path)}
                    ),
                    "sha256": usage["sha256"],
                    "size": usage["size"],
                    "usages": [],
                },
            )
            if entry["sha256"] != usage["sha256"] or entry["size"] != usage["size"]:
                raise IntegrityError(
                    f"conflicting frozen identities for external file {absolute_path}"
                )
            entry["usages"].append(
                {"route": usage["route"], "argv_index": usage["argv_index"]}
            )
        files = []
        for absolute_path in sorted(grouped):
            entry = grouped[absolute_path]
            entry["usages"] = sorted(
                entry["usages"],
                key=lambda value: (str(value["route"]), int(value["argv_index"])),
            )
            files.append(entry)
        return {
            "schema_version": "chesstory.eval.external-executable-binding.v1",
            "security_mode": self.security_mode,
            "files": files,
            "files_sha256": sha256_json(files),
            "all_bound_files_sha256": self.bound_files_sha256,
        }

    def assert_bound_files_unchanged(self, *, location: str) -> None:
        for route, command in self.stage_routes:
            command.assert_bound_files_unchanged(
                location=f"{location} stage route {_route_label(route)}"
            )
        self.evaluator_route.assert_bound_files_unchanged(
            location=f"{location} evaluator route"
        )

    def assert_complete_stage_routes(self, oracle_chains: Sequence[str]) -> None:
        chains = tuple(oracle_chains)
        if (
            not chains
            or any(not isinstance(chain, str) or not chain.strip() for chain in chains)
            or len(set(chains)) != len(chains)
        ):
            raise ContractError("release oracle chains must be unique non-empty strings")
        expected: set[tuple[str, Source] | tuple[str, Source, str]] = {
            (stage, Source.ACTUAL) for stage in STAGES
        }
        expected.update(
            (stage, Source.ORACLE, chain)
            for stage in STAGES
            for chain in chains
        )
        actual = {route for route, _ in self.stage_routes}
        if actual != expected:
            missing = sorted(_route_label(route) for route in expected - actual)
            extra = sorted(_route_label(route) for route in actual - expected)
            raise ContractError(
                "sealed live-command config must declare exactly every actual/oracle "
                f"stage route; missing={missing}, extra={extra}"
            )
        self.assert_bound_files_unchanged(location="complete live-command config")

    def binding(self, *, artifact_path: str, artifact_sha256: str) -> dict[str, str]:
        if artifact_sha256 != self.document_sha256:
            raise ContractError("captured live-command config differs from frozen document")
        return {
            "schema_version": _LIVE_COMMAND_CONFIG_SCHEMA,
            "source_file_sha256": self.source_sha256,
            "document_sha256": self.document_sha256,
            "artifact_path": artifact_path,
            "artifact_sha256": artifact_sha256,
            "bound_files_sha256": self.bound_files_sha256,
            "security_mode": self.security_mode,
        }


def _route_label(
    route: tuple[str, Source] | tuple[str, Source, str],
) -> str:
    if len(route) == 2:
        return f"{route[0]}:{route[1].value}"
    return f"{route[0]}:{route[1].value}:{route[2]}"


def load_live_command_config(path: Path) -> LiveCommandConfig:
    """Read, strictly validate, and byte/hash-freeze a command config once."""

    supplied = Path(path)
    if supplied.is_symlink() or bool(getattr(supplied, "is_junction", lambda: False)()):
        raise ContractError("live-command config must not be a symlink or junction")
    try:
        resolved = supplied.resolve(strict=True)
    except OSError as error:
        raise ContractError(f"live-command config is not readable: {supplied}") from error
    if not resolved.is_file():
        raise ContractError("live-command config must be a regular file")
    try:
        raw_bytes = resolved.read_bytes()
    except OSError as error:
        raise ContractError(f"live-command config is not readable: {resolved}") from error
    if not raw_bytes or len(raw_bytes) > 1024 * 1024:
        raise ContractError("live-command config must be between 1 byte and 1 MiB")
    try:
        text = raw_bytes.decode("utf-8")
    except UnicodeDecodeError as error:
        raise ContractError("live-command config must be strict UTF-8") from error
    document = strict_json_loads(text, location=str(resolved))
    if not isinstance(document, Mapping):
        raise ContractError("live-command config must contain one JSON object")
    if set(document) != {"schema_version", "stage_routes", "evaluator_route"}:
        raise ContractError(
            "live-command config must contain exactly schema_version, stage_routes, evaluator_route"
        )
    if document.get("schema_version") != _LIVE_COMMAND_CONFIG_SCHEMA:
        raise ContractError("unsupported live-command config schema_version")
    raw_routes = document.get("stage_routes")
    if not isinstance(raw_routes, list) or not raw_routes:
        raise ContractError("live-command config stage_routes must be a non-empty array")
    routes: list[
        tuple[tuple[str, Source] | tuple[str, Source, str], CommandSpec]
    ] = []
    seen: set[tuple[str, Source] | tuple[str, Source, str]] = set()
    for index, raw_route in enumerate(raw_routes):
        location = f"live-command config stage_routes[{index}]"
        if not isinstance(raw_route, Mapping):
            raise ContractError(f"{location} must be an object")
        source_value = raw_route.get("source")
        expected_keys = {
            "stage",
            "source",
            "argv",
            "timeout_seconds",
            "env",
            "bound_files",
        }
        if source_value == Source.ORACLE.value:
            expected_keys.add("oracle_chain")
        if set(raw_route) != expected_keys:
            raise ContractError(f"{location} has missing or unknown fields")
        stage = require_stage(raw_route.get("stage"))
        if not isinstance(source_value, str) or source_value not in {
            Source.ACTUAL.value,
            Source.ORACLE.value,
        }:
            raise ContractError(f"{location}.source must be actual or oracle")
        source = Source(source_value)
        if source == Source.ORACLE:
            chain = raw_route.get("oracle_chain")
            if not isinstance(chain, str) or not chain.strip():
                raise ContractError(f"{location}.oracle_chain must be non-empty text")
            route: tuple[str, Source] | tuple[str, Source, str] = (
                stage,
                source,
                chain,
            )
        else:
            route = (stage, source)
        if route in seen:
            raise ContractError(f"duplicate live-command stage route: {_route_label(route)}")
        seen.add(route)
        env = raw_route.get("env")
        if not isinstance(env, Mapping):
            raise ContractError(f"{location}.env must be an explicit object")
        argv = raw_route.get("argv")
        if not isinstance(argv, list):
            raise ContractError(f"{location}.argv must be an array")
        spec = CommandSpec.create(
            argv,
            timeout_seconds=raw_route.get("timeout_seconds"),
            environment=env,  # type: ignore[arg-type]
            location=location,
            bound_files=raw_route.get("bound_files"),  # type: ignore[arg-type]
            require_bound_files=True,
        )
        routes.append((route, spec))
    evaluator = document.get("evaluator_route")
    if not isinstance(evaluator, Mapping) or set(evaluator) != {
        "argv",
        "timeout_seconds",
        "env",
        "bound_files",
    }:
        raise ContractError(
            "live-command config evaluator_route must contain exactly argv, "
            "timeout_seconds, env, bound_files"
        )
    evaluator_env = evaluator.get("env")
    evaluator_argv = evaluator.get("argv")
    if not isinstance(evaluator_env, Mapping) or not isinstance(evaluator_argv, list):
        raise ContractError("live-command config evaluator_route argv/env types are invalid")
    evaluator_spec = CommandSpec.create(
        evaluator_argv,
        timeout_seconds=evaluator.get("timeout_seconds"),
        environment=evaluator_env,  # type: ignore[arg-type]
        location="live-command config evaluator_route",
        bound_files=evaluator.get("bound_files"),  # type: ignore[arg-type]
        require_bound_files=True,
    )
    canonical_json = json.dumps(
        document,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )
    document_sha256 = hashlib.sha256((canonical_json + "\n").encode("utf-8")).hexdigest()
    return LiveCommandConfig(
        source_path=resolved,
        source_sha256=hashlib.sha256(raw_bytes).hexdigest(),
        document_sha256=document_sha256,
        stage_routes=tuple(routes),
        evaluator_route=evaluator_spec,
        _document_json=canonical_json,
    )


@dataclass(frozen=True)
class StageInvocation:
    sample: StageSampleView
    stage: str
    source: Source
    oracle_chain: str | None
    context: str
    input_document: Mapping[str, Any]
    seed: int

    def __post_init__(self) -> None:
        if not isinstance(self.sample, StageSampleView):
            raise ContractError(
                "StageInvocation accepts only StageSampleView; full Sample exposure is forbidden"
            )
        require_stage(self.stage)
        if not isinstance(self.source, Source):
            raise ContractError("stage invocation source must be a Source")
        if not isinstance(self.input_document, Mapping):
            raise ContractError("stage invocation input_document must be an object")

    @property
    def input_hash(self) -> str:
        value = self.input_document.get("input_hash")
        if not isinstance(value, str):
            raise ContractError(f"{self.stage} input has no input_hash")
        return value


@dataclass(frozen=True)
class AdapterResult:
    output_payload: Mapping[str, Any]
    producer: Mapping[str, Any]
    raw_output: Any
    provider_io: Any | None = None


class CommandStageUnavailable(StageUnavailable):
    """A command could not run; ``provider_io`` retains safe failure evidence."""

    def __init__(
        self,
        stage: str,
        reason: str,
        *,
        provider_io: Mapping[str, Any],
    ) -> None:
        super().__init__(stage, reason)
        self.provider_io = dict(provider_io)


class CommandContractError(ContractError):
    """A command violated its response contract after provider I/O occurred."""

    def __init__(self, message: str, *, provider_io: Mapping[str, Any]) -> None:
        super().__init__(message)
        self.provider_io = dict(provider_io)


class StageAdapter(ABC):
    @abstractmethod
    def invoke(self, invocation: StageInvocation) -> AdapterResult:
        raise NotImplementedError

    def has_live_route(self, invocation: StageInvocation) -> bool:
        """Whether this adapter has an explicit external-command route."""

        return False

    def is_live_command_only(self) -> bool:
        return False


class ReplayStageAdapter(StageAdapter):
    """Exact artifact replay; it never reconstructs a missing production stage.

    Records are keyed by sample, stage, source, chain, context, seed, and
    semantic input hash. A missing conditioned artifact is an explicit
    unavailable arm; context wildcards are intentionally unsupported.
    """

    def __init__(self, jsonl_paths: Sequence[Path]) -> None:
        self._records: dict[
            tuple[str, str, str, str, str, int, str], Mapping[str, Any]
        ] = {}
        for path in jsonl_paths:
            for line_number, row in enumerate(read_jsonl(path), 1):
                if not isinstance(row, Mapping):
                    raise ContractError(f"{path}:{line_number}: adapter record must be an object")
                key = self._key_from_record(row, path, line_number)
                if key in self._records:
                    raise ContractError(f"duplicate adapter replay key in {path}:{line_number}: {key}")
                self._records[key] = row

    def invoke(self, invocation: StageInvocation) -> AdapterResult:
        chain = self._normalized_chain(
            invocation.source,
            invocation.oracle_chain,
            location=f"{invocation.stage} invocation",
        )
        if (
            not isinstance(invocation.context, str)
            or not invocation.context.strip()
            or invocation.context == "*"
        ):
            raise ContractError(
                f"{invocation.stage} invocation requires an explicit non-wildcard context"
            )
        if not isinstance(invocation.seed, int) or isinstance(invocation.seed, bool):
            raise ContractError(f"{invocation.stage} invocation seed must be an integer")
        exact = (
            invocation.sample.sample_id,
            invocation.stage,
            invocation.source.value,
            chain,
            invocation.context,
            invocation.seed,
            invocation.input_hash,
        )
        row = self._records.get(exact)
        if row is None:
            raise StageUnavailable(
                invocation.stage,
                "no exact upstream-conditioned replay artifact for "
                f"source={invocation.source.value}, chain={chain}, context={invocation.context}, "
                f"seed={invocation.seed}, input_hash={invocation.input_hash}",
            )
        output = row.get("output_payload")
        producer = row.get("producer")
        if not isinstance(output, Mapping) or not isinstance(producer, Mapping):
            raise ContractError(f"replay record has no output_payload/producer: {exact}")
        return AdapterResult(
            output_payload=output,
            producer=producer,
            raw_output=row.get("raw_output", output),
            provider_io=row.get("raw_provider_io"),
        )

    @staticmethod
    def _key_from_record(
        row: Mapping[str, Any], path: Path, line_number: int
    ) -> tuple[str, str, str, str, str, int, str]:
        fields = ["sample_id", "stage", "source", "context", "input_hash"]
        if any(not isinstance(row.get(field), str) for field in fields):
            raise ContractError(f"{path}:{line_number}: replay key fields must be strings")
        context = str(row["context"])
        if not context.strip() or context == "*":
            raise ContractError(
                f"{path}:{line_number}: replay context must be explicit and non-wildcard"
            )
        seed = row.get("seed")
        if not isinstance(seed, int) or isinstance(seed, bool):
            raise ContractError(f"{path}:{line_number}: replay seed must be an integer")
        stage = require_stage(str(row["stage"]))
        try:
            source = Source(str(row["source"]))
        except ValueError as error:
            raise ContractError(f"{path}:{line_number}: invalid replay source") from error
        chain = ReplayStageAdapter._normalized_chain(
            source,
            row.get("oracle_chain"),
            location=f"{path}:{line_number}",
        )
        return (
            str(row["sample_id"]),
            stage,
            source.value,
            chain,
            context,
            seed,
            str(row["input_hash"]),
        )

    @staticmethod
    def _normalized_chain(source: Source, value: Any, *, location: str) -> str:
        if source != Source.ORACLE:
            return "-"
        if not isinstance(value, str) or not value.strip():
            raise ContractError(f"{location}: oracle replay requires oracle_chain")
        return value


class CommandStageAdapter(StageAdapter):
    """Invokes an external stage producer through JSON stdin/stdout.

    Non-oracle routes retain the legacy ``(stage, source)`` command key. Oracle
    routes require ``(stage, Source.ORACLE, oracle_chain)`` so independent
    chains cannot silently collapse onto one executable. Source, chain, and
    experiment context are control-plane data and are never sent in provider
    JSON. A successful response must repeat the invocation identity fields.
    """

    _SCHEMA_VERSION = "chesstory.eval.stage-invocation.v1"

    def __init__(
        self,
        commands: Mapping[
            tuple[str, Source] | tuple[str, Source, str],
            CommandSpec | Sequence[str],
        ],
        *,
        timeout_seconds: int = 180,
        environment: Mapping[str, str] | None = None,
        release_mode: bool = False,
    ) -> None:
        if (
            not isinstance(timeout_seconds, int)
            or isinstance(timeout_seconds, bool)
            or timeout_seconds <= 0
        ):
            raise ContractError("command timeout_seconds must be a positive integer")
        normalized: dict[
            tuple[str, Source] | tuple[str, Source, str], CommandSpec
        ] = {}
        for raw_route, raw_command in commands.items():
            route = self._normalize_route(raw_route)
            command = (
                raw_command
                if isinstance(raw_command, CommandSpec)
                else CommandSpec.create(
                    raw_command,
                    timeout_seconds=timeout_seconds,
                    environment=environment or {},
                    location=f"command route {_route_label(route)}",
                )
            )
            if route in normalized:
                raise ContractError(f"duplicate normalized command route: {route!r}")
            if len(route) == 3:
                duplicate_chain = next(
                    (
                        configured_route
                        for configured_route, configured_command in normalized.items()
                        if len(configured_route) == 3
                        and configured_route[0] == route[0]
                        and configured_command.argv == command.argv
                    ),
                    None,
                )
                if duplicate_chain is not None:
                    raise ContractError(
                        "independent oracle chains for one stage must not share "
                        f"the same command: {duplicate_chain!r}, {route!r}"
                    )
            normalized[route] = command
        self.commands = normalized
        self.release_mode = bool(release_mode)
        if self.release_mode:
            if os.name != "nt":
                raise ContractError(
                    "release live commands require the Windows deny-write/delete "
                    "bound-file security mode on this harness"
                )
            for route, command in self.commands.items():
                command.assert_bound_files_unchanged(
                    location=f"release stage route {_route_label(route)}"
                )

    def has_live_route(self, invocation: StageInvocation) -> bool:
        return self._invocation_route(invocation) in self.commands

    def is_live_command_only(self) -> bool:
        return True

    def invoke(self, invocation: StageInvocation) -> AdapterResult:
        route = self._invocation_route(invocation)
        command_spec = self.commands.get(route)
        if command_spec is None:
            raise StageUnavailable(
                invocation.stage,
                f"no external command configured for route {route!r}",
            )
        command = command_spec.argv
        request = {
            "schema_version": self._SCHEMA_VERSION,
            "stage": invocation.stage,
            "sample_id": invocation.sample.sample_id,
            "seed": invocation.seed,
            "input_hash": invocation.input_hash,
            "input_payload": invocation.input_document["payload"],
        }
        environment = self._minimal_process_environment()
        environment.update(command_spec.environment_dict())
        try:
            request_bytes = json.dumps(request, ensure_ascii=False, allow_nan=False).encode(
                "utf-8"
            )
        except (TypeError, ValueError) as error:
            raise ContractError(
                f"{invocation.stage} command request is not finite JSON"
            ) from error
        base_provider_io = {
            "command": list(command),
            "request": request,
            "stdin_base64": base64.b64encode(request_bytes).decode("ascii"),
        }
        try:
            with command_spec.invocation_guard(
                release_mode=self.release_mode,
                location=f"stage route {_route_label(route)}",
            ):
                completed = subprocess.run(
                    list(command),
                    input=request_bytes,
                    text=False,
                    capture_output=True,
                    timeout=command_spec.timeout_seconds,
                    check=False,
                    env=environment,
                )
        except subprocess.TimeoutExpired as error:
            provider_io = self._provider_io_record(
                base_provider_io,
                returncode=None,
                stdout=error.stdout,
                stderr=error.stderr,
                failure={
                    "kind": "timeout",
                    "timeout_seconds": command_spec.timeout_seconds,
                },
            )
            raise CommandStageUnavailable(
                invocation.stage,
                f"external command timed out after {command_spec.timeout_seconds} seconds",
                provider_io=provider_io,
            ) from error
        except (OSError, TypeError, ValueError) as error:
            provider_io = self._provider_io_record(
                base_provider_io,
                returncode=None,
                stdout=None,
                stderr=None,
                failure={
                    "kind": "os-error" if isinstance(error, OSError) else "launch-error",
                    "error_type": type(error).__name__,
                    "message": str(error),
                    "errno": error.errno if isinstance(error, OSError) else None,
                },
            )
            raise CommandStageUnavailable(
                invocation.stage,
                f"external command could not start: {type(error).__name__}",
                provider_io=provider_io,
            ) from error
        provider_io = self._provider_io_record(
            base_provider_io,
            returncode=completed.returncode,
            stdout=completed.stdout,
            stderr=completed.stderr,
        )
        if completed.returncode != 0:
            raise CommandStageUnavailable(
                invocation.stage,
                f"external command failed with exit code {completed.returncode}",
                provider_io=provider_io,
            )
        stdout_bytes = self._safe_process_bytes(completed.stdout)
        try:
            response_text = stdout_bytes.decode("utf-8")
        except UnicodeDecodeError as error:
            raise CommandContractError(
                f"{invocation.stage} command emitted non-UTF-8 output",
                provider_io=provider_io,
            ) from error
        try:
            response = strict_json_loads(
                response_text,
                location=f"{invocation.stage} command response",
            )
        except ContractError as error:
            raise CommandContractError(
                f"{invocation.stage} command emitted invalid JSON",
                provider_io=provider_io,
            ) from error
        provider_io = {**provider_io, "response": response}
        if (
            not isinstance(response, Mapping)
            or not isinstance(response.get("output_payload"), Mapping)
            or not isinstance(response.get("producer"), Mapping)
        ):
            raise CommandContractError(
                f"{invocation.stage} command response has no output_payload/producer",
                provider_io=provider_io,
            )
        self._validate_response_echo(response, request, provider_io)
        return AdapterResult(
            output_payload=response["output_payload"],
            producer=response["producer"],
            raw_output=response.get("raw_output", response["output_payload"]),
            provider_io=provider_io,
        )

    @staticmethod
    def _normalize_route(
        raw_route: tuple[str, Source] | tuple[str, Source, str],
    ) -> tuple[str, Source] | tuple[str, Source, str]:
        if not isinstance(raw_route, tuple) or len(raw_route) not in {2, 3}:
            raise ContractError(
                "command route must be (stage, source) or (stage, source, oracle_chain)"
            )
        stage = require_stage(raw_route[0])
        try:
            source = (
                raw_route[1]
                if isinstance(raw_route[1], Source)
                else Source(raw_route[1])
            )
        except (TypeError, ValueError) as error:
            raise ContractError(f"invalid command source in route {raw_route!r}") from error
        if len(raw_route) == 2:
            if source == Source.ORACLE:
                raise ContractError(
                    "oracle command routes require (stage, Source.ORACLE, oracle_chain)"
                )
            return stage, source
        chain = raw_route[2]
        if source != Source.ORACLE:
            raise ContractError("only oracle command routes may include oracle_chain")
        if not isinstance(chain, str) or not chain.strip():
            raise ContractError("oracle command route requires a non-empty oracle_chain")
        return stage, source, chain

    @staticmethod
    def _invocation_route(
        invocation: StageInvocation,
    ) -> tuple[str, Source] | tuple[str, Source, str]:
        stage = require_stage(invocation.stage)
        if invocation.source != Source.ORACLE:
            return stage, invocation.source
        chain = invocation.oracle_chain
        if not isinstance(chain, str) or not chain.strip():
            raise ContractError(f"{stage} oracle invocation requires oracle_chain")
        return stage, Source.ORACLE, chain

    @staticmethod
    def _safe_process_bytes(value: str | bytes | None) -> bytes:
        if value is None:
            return b""
        if isinstance(value, bytes):
            return value
        return value.encode("utf-8", errors="replace")

    @classmethod
    def _provider_io_record(
        cls,
        base: Mapping[str, Any],
        *,
        returncode: int | None,
        stdout: str | bytes | None,
        stderr: str | bytes | None,
        failure: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        stdout_bytes = cls._safe_process_bytes(stdout)
        stderr_bytes = cls._safe_process_bytes(stderr)
        record = {
            **base,
            "returncode": returncode,
            "stdout": stdout_bytes.decode("utf-8", errors="replace"),
            "stderr": stderr_bytes.decode("utf-8", errors="replace"),
            "stdout_base64": base64.b64encode(stdout_bytes).decode("ascii"),
            "stderr_base64": base64.b64encode(stderr_bytes).decode("ascii"),
        }
        if failure is not None:
            record["failure"] = dict(failure)
        return record

    @staticmethod
    def _validated_environment(
        environment: Mapping[str, str], *, location: str = "command environment"
    ) -> dict[str, str]:
        if not isinstance(environment, Mapping):
            raise ContractError(f"{location} must be an explicit object")
        result: dict[str, str] = {}
        normalized_keys: set[str] = set()
        for key, value in environment.items():
            if (
                not isinstance(key, str)
                or not key
                or "=" in key
                or "\x00" in key
                or re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", key) is None
                or not isinstance(value, str)
                or "\x00" in value
            ):
                raise ContractError(
                    f"{location} keys/values must be valid non-NUL strings"
                )
            if _SECRET_ENV_KEY.search(key) or _SECRET_ENV_VALUE.search(value):
                raise ContractError(
                    f"{location} must be explicitly secret-free; rejected key {key!r}"
                )
            normalized_key = key.upper()
            if normalized_key in normalized_keys:
                raise ContractError(
                    f"{location} contains duplicate case-insensitive key {key!r}"
                )
            normalized_keys.add(normalized_key)
            result[key] = value
        return result

    @staticmethod
    def _minimal_process_environment() -> dict[str, str]:
        """Inherit only process-launch essentials, never custodian/signing secrets."""

        allowed = {
            "COMSPEC",
            "LANG",
            "LC_ALL",
            "PATH",
            "PATHEXT",
            "SYSTEMROOT",
            "TEMP",
            "TMP",
            "TMPDIR",
            "WINDIR",
        }
        return {
            key: value
            for key, value in os.environ.items()
            if key.upper() in allowed
        }

    @staticmethod
    def _validate_response_echo(
        response: Mapping[str, Any],
        request: Mapping[str, Any],
        provider_io: Mapping[str, Any],
    ) -> None:
        fields = ("schema_version", "stage", "sample_id", "seed", "input_hash")
        mismatches = [
            field
            for field in fields
            if field not in response
            or type(response[field]) is not type(request[field])
            or response[field] != request[field]
        ]
        if mismatches:
            raise CommandContractError(
                "command response does not echo invocation identity fields: "
                f"{mismatches}",
                provider_io=provider_io,
            )


class AvailabilityGuard(StageAdapter):
    """Reconcile static availability with explicit live command routing.

    A corpus ``available`` declaration may use diagnostic embedded/replay
    evidence. Any other static status is executable only through a configured
    live route: the route is current execution evidence and its failure remains
    fail-closed. Release callers additionally require every actual and oracle
    invocation to select an explicit live route.
    """

    def __init__(
        self, delegate: StageAdapter, *, require_explicit_live: bool = False
    ) -> None:
        self.delegate = delegate
        self.require_explicit_live = require_explicit_live

    def has_live_route(self, invocation: StageInvocation) -> bool:
        return self.delegate.has_live_route(invocation)

    def is_live_command_only(self) -> bool:
        return self.require_explicit_live and self.delegate.is_live_command_only()

    def invoke(self, invocation: StageInvocation) -> AdapterResult:
        if not isinstance(invocation.sample, StageSampleView):
            raise ContractError(
                "stage adapters require StageSampleView; full corpus Sample exposure is forbidden"
            )
        has_live_route = self.delegate.has_live_route(invocation)
        if invocation.source == Source.ACTUAL:
            entry = invocation.sample.availability_for(invocation.stage)
            if entry.status != "available" and not has_live_route:
                raise StageUnavailable(
                    invocation.stage,
                    f"{entry.reason}; no explicit live route is configured",
                )
            if self.require_explicit_live and not has_live_route:
                raise StageUnavailable(
                    invocation.stage,
                    "sealed execution requires an explicit live actual route",
                )
        elif (
            invocation.source == Source.ORACLE
            and self.require_explicit_live
            and not has_live_route
        ):
            raise StageUnavailable(
                invocation.stage,
                "sealed execution requires an explicit live oracle route",
            )
        return self.delegate.invoke(invocation)


class EmbeddedAcquisitionAdapter(StageAdapter):
    """Reads trusted, actual-only Q packs extracted before adapter dispatch.

    This adapter is deliberately Q-only: using later-stage artifacts to
    reconstruct the structured P endpoint would create a second judgment
    implementation.  Its
    constructor accepts a mapping containing only actual Q entries; the corpus
    row, labels, oracle packs, and final outputs are never retained or exposed
    through ``StageInvocation``.
    """

    def __init__(self, actual_q_packs: Mapping[str, Mapping[str, Any]]) -> None:
        if not isinstance(actual_q_packs, Mapping):
            raise ContractError("embedded actual-Q packs must be a mapping")
        frozen: dict[str, Mapping[str, Any]] = {}
        for sample_id, raw_entry in actual_q_packs.items():
            if not isinstance(sample_id, str) or not sample_id.strip():
                raise ContractError("embedded actual-Q sample IDs must be non-empty text")
            if not isinstance(raw_entry, Mapping):
                raise ContractError(
                    f"embedded actual-Q entry for {sample_id!r} must be an object"
                )
            payload = raw_entry.get("payload")
            producer = raw_entry.get("producer")
            if not isinstance(payload, Mapping) or not isinstance(producer, Mapping):
                raise ContractError(
                    f"embedded actual-Q entry for {sample_id!r} requires payload and producer"
                )
            try:
                # A JSON round-trip both rejects non-finite/non-JSON values and
                # detaches the adapter's pack from the mutable corpus object.
                entry = json.loads(
                    json.dumps(raw_entry, ensure_ascii=False, allow_nan=False)
                )
            except (TypeError, ValueError) as error:
                raise ContractError(
                    f"embedded actual-Q entry for {sample_id!r} is not finite JSON"
                ) from error
            frozen[sample_id] = entry
        self._actual_q_packs = frozen

    def invoke(self, invocation: StageInvocation) -> AdapterResult:
        if invocation.stage != "Q":
            raise StageUnavailable(invocation.stage, "embedded adapter is Q-only")
        if invocation.source != Source.ACTUAL:
            raise StageUnavailable(
                "Q", "embedded acquisition packs are actual-only; oracle Q must be independent"
            )
        if not isinstance(invocation.sample, StageSampleView):
            raise ContractError(
                "embedded acquisition requires a stage-safe sample view"
            )
        entry = self._actual_q_packs.get(invocation.sample.sample_id)
        if not isinstance(entry, Mapping):
            raise StageUnavailable("Q", "no embedded actual acquisition pack")
        payload = entry.get("payload")
        producer = entry.get("producer")
        if not isinstance(payload, Mapping) or not isinstance(producer, Mapping):
            raise ContractError("embedded Q entry requires payload and producer objects")
        input_payload = invocation.input_document.get("payload")
        if not isinstance(input_payload, Mapping):
            raise ContractError("Q invocation input payload is missing")
        if payload.get("question") != input_payload.get("question"):
            raise ContractError("embedded actual Q question is not invocation-bound")
        if payload.get("acquisition_config") != input_payload.get("acquisition_config"):
            raise ContractError("embedded actual Q configuration is not invocation-bound")
        return AdapterResult(
            output_payload=payload,
            producer=producer,
            raw_output=entry.get("raw_output", payload),
            provider_io=entry.get("raw_provider_io"),
        )


class CompositeAdapter(StageAdapter):
    def __init__(self, adapters: Sequence[StageAdapter]) -> None:
        self.adapters = tuple(adapters)

    def invoke(self, invocation: StageInvocation) -> AdapterResult:
        reasons: list[str] = []
        failed_attempts: list[dict[str, Any]] = []
        for adapter in self.adapters:
            try:
                result = adapter.invoke(invocation)
            except CommandStageUnavailable:
                # A configured live route that reached provider I/O must never
                # silently fall through to a replay or embedded substitute.
                raise
            except StageUnavailable as error:
                reasons.append(error.reason)
                provider_io = getattr(error, "provider_io", None)
                if provider_io is not None:
                    failed_attempts.append(
                        {
                            "adapter": type(adapter).__name__,
                            "reason": error.reason,
                            "provider_io": provider_io,
                        }
                    )
                continue
            if failed_attempts:
                return AdapterResult(
                    output_payload=result.output_payload,
                    producer=result.producer,
                    raw_output=result.raw_output,
                    provider_io={
                        "selected_adapter": type(adapter).__name__,
                        "selected_provider_io": result.provider_io,
                        "failed_attempts": failed_attempts,
                    },
                )
            return result
        reason = "; ".join(reasons) or "no adapter accepted invocation"
        if failed_attempts:
            raise CommandStageUnavailable(
                invocation.stage,
                reason,
                provider_io={"failed_attempts": failed_attempts},
            )
        raise StageUnavailable(invocation.stage, reason)

    def has_live_route(self, invocation: StageInvocation) -> bool:
        return any(adapter.has_live_route(invocation) for adapter in self.adapters)

    def is_live_command_only(self) -> bool:
        return bool(self.adapters) and all(
            adapter.is_live_command_only() for adapter in self.adapters
        )
