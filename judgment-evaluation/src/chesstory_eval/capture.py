from __future__ import annotations

import hmac
import json
import os
import re
import tempfile
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Mapping

from .hashing import json_bytes, read_json, sha256_bytes, sha256_file, sha256_json
from .model import IntegrityError


_CUSTODIAN_STORAGE_ATTESTATION_SCHEMA = (
    "chesstory.eval.custodian-storage-attestation.v1"
)
_CUSTODIAN_STORAGE_SECURITY_MODE = "immutable-store-exclusive-acl-v1"
_SHA256_RE = re.compile(r"[0-9a-f]{64}")
_CUSTODIAN_ISSUER_RE = re.compile(r"custodian:[a-z0-9]+(?:-[a-z0-9]+)*")


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def _strict_json_object(path: Path) -> Mapping[str, Any]:
    def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate JSON key {key!r}")
            result[key] = value
        return result

    try:
        raw = path.read_bytes()
        value = json.loads(
            raw.decode("utf-8"),
            object_pairs_hook=unique_object,
            parse_constant=lambda constant: (_ for _ in ()).throw(
                ValueError(f"non-finite JSON constant {constant}")
            ),
        )
    except (OSError, UnicodeError, ValueError, json.JSONDecodeError) as error:
        raise IntegrityError(
            f"custodian storage attestation is invalid: {path}"
        ) from error
    if not isinstance(value, Mapping):
        raise IntegrityError("custodian storage attestation must be an object")
    return value


def load_custodian_storage_attestation(
    root: Path,
    state_id: str,
    *,
    relative_path: str,
    expected_sha256: str,
) -> dict[str, str]:
    """Verify the external immutable-store/ACL attestation bound by policy."""

    supplied_root = Path(root)
    if supplied_root.is_symlink() or bool(
        getattr(supplied_root, "is_junction", lambda: False)()
    ):
        raise IntegrityError("custodian state root must not be a symlink or junction")
    try:
        resolved_root = supplied_root.resolve(strict=True)
    except OSError as error:
        raise IntegrityError("custodian state root is unavailable") from error
    if not resolved_root.is_dir():
        raise IntegrityError("custodian state root must be an existing directory")
    relative = Path(relative_path)
    if (
        not isinstance(relative_path, str)
        or not relative_path
        or relative.is_absolute()
        or relative.name != relative_path
        or relative_path in {".", ".."}
    ):
        raise IntegrityError(
            "custodian storage attestation must be a direct-child canonical filename"
        )
    if not isinstance(expected_sha256, str) or not _SHA256_RE.fullmatch(
        expected_sha256
    ):
        raise IntegrityError("custodian storage attestation policy hash is invalid")
    path = resolved_root / relative_path
    if path.is_symlink() or bool(getattr(path, "is_junction", lambda: False)()):
        raise IntegrityError(
            "custodian storage attestation must not be a symlink or junction"
        )
    try:
        resolved_path = path.resolve(strict=True)
    except OSError as error:
        raise IntegrityError("custodian storage attestation is unavailable") from error
    if resolved_path.parent != resolved_root or not resolved_path.is_file():
        raise IntegrityError(
            "custodian storage attestation must be a regular direct child"
        )
    raw_sha256 = sha256_file(resolved_path)
    if raw_sha256 != expected_sha256:
        raise IntegrityError("custodian storage attestation bytes differ from policy")
    document = _strict_json_object(resolved_path)
    expected_keys = {
        "schema_version",
        "state_id",
        "root_binding_sha256",
        "security_mode",
        "immutable_store",
        "non_custodian_write_denied",
        "non_custodian_delete_rename_denied",
        "custodian_delete_authority_is_external_trust",
        "issuer_id",
        "issued_at",
        "acl_evidence_sha256",
    }
    if set(document) != expected_keys:
        raise IntegrityError("custodian storage attestation fields are not exact")
    if document.get("schema_version") != _CUSTODIAN_STORAGE_ATTESTATION_SCHEMA:
        raise IntegrityError("unsupported custodian storage attestation schema")
    if document.get("state_id") != state_id:
        raise IntegrityError("custodian storage attestation state_id differs")
    root_binding = sha256_json(
        {"resolved_custodian_state_root": os.path.normcase(str(resolved_root))}
    )
    if document.get("root_binding_sha256") != root_binding:
        raise IntegrityError("custodian storage attestation root differs")
    if document.get("security_mode") != _CUSTODIAN_STORAGE_SECURITY_MODE:
        raise IntegrityError("unsupported custodian storage security mode")
    for field in (
        "immutable_store",
        "non_custodian_write_denied",
        "non_custodian_delete_rename_denied",
        "custodian_delete_authority_is_external_trust",
    ):
        if document.get(field) is not True:
            raise IntegrityError(f"custodian storage attestation must assert {field}=true")
    issuer_id = document.get("issuer_id")
    if not isinstance(issuer_id, str) or not _CUSTODIAN_ISSUER_RE.fullmatch(issuer_id):
        raise IntegrityError("custodian storage attestation issuer_id is not canonical")
    issued_at = document.get("issued_at")
    if not isinstance(issued_at, str) or not issued_at.endswith("Z"):
        raise IntegrityError("custodian storage attestation issued_at must be UTC text")
    try:
        parsed_issued_at = datetime.fromisoformat(issued_at[:-1] + "+00:00")
    except ValueError as error:
        raise IntegrityError("custodian storage attestation issued_at is invalid") from error
    if parsed_issued_at.tzinfo is None:
        raise IntegrityError("custodian storage attestation issued_at lacks timezone")
    evidence_sha256 = document.get("acl_evidence_sha256")
    if not isinstance(evidence_sha256, str) or not _SHA256_RE.fullmatch(
        evidence_sha256
    ):
        raise IntegrityError("custodian ACL evidence SHA-256 is invalid")
    return {
        "path": relative_path,
        "sha256": raw_sha256,
        "document_sha256": sha256_json(document),
        "security_mode": _CUSTODIAN_STORAGE_SECURITY_MODE,
        "issuer_id": issuer_id,
        "issued_at": issued_at,
        "acl_evidence_sha256": evidence_sha256,
    }


class ArtifactStore:
    """Immutable artifact files plus hash-chained append-only ledgers."""

    def __init__(
        self,
        root: Path,
        run_id: str,
        *,
        custodian_state_root: Path | None = None,
        custodian_state_id: str | None = None,
        custodian_storage_binding: Mapping[str, Any] | None = None,
    ) -> None:
        self.root = root.resolve()
        self.run_id = self._segment(run_id)
        self.run_root = self.root / self.run_id
        self.run_root.mkdir(parents=True, exist_ok=True)
        self.ledger_path = self.run_root / "artifact-ledger.jsonl"
        self.access_log_path = self.run_root / "access-log.jsonl"
        if len(
            {
                custodian_state_root is None,
                custodian_state_id is None,
                custodian_storage_binding is None,
            }
        ) != 1:
            raise IntegrityError(
                "custodian state root, state_id, and storage binding must be supplied together"
            )
        if custodian_state_root is None:
            self.custodian_state_root: Path | None = None
            self.custodian_state_id: str | None = None
            self.custodian_storage_binding: Mapping[str, str] | None = None
        else:
            supplied_state_root = Path(custodian_state_root)
            if supplied_state_root.is_symlink() or bool(
                getattr(supplied_state_root, "is_junction", lambda: False)()
            ):
                raise IntegrityError(
                    "custodian state root must not be a symlink or junction"
                )
            resolved_state_root = supplied_state_root.resolve(strict=True)
            if not resolved_state_root.is_dir():
                raise IntegrityError("custodian state root must be an existing directory")
            if not isinstance(custodian_state_id, str) or not custodian_state_id.strip():
                raise IntegrityError("custodian_state_id must be non-empty text")
            self.custodian_state_root = resolved_state_root
            self.custodian_state_id = custodian_state_id
            if not isinstance(custodian_storage_binding, Mapping) or set(
                custodian_storage_binding
            ) != {
                "path",
                "sha256",
                "document_sha256",
                "security_mode",
                "issuer_id",
                "issued_at",
                "acl_evidence_sha256",
            }:
                raise IntegrityError("custodian storage binding fields are not exact")
            verified_storage = load_custodian_storage_attestation(
                resolved_state_root,
                custodian_state_id,
                relative_path=str(custodian_storage_binding.get("path")),
                expected_sha256=str(custodian_storage_binding.get("sha256")),
            )
            if dict(custodian_storage_binding) != verified_storage:
                raise IntegrityError("custodian storage binding differs from attestation")
            self.custodian_storage_binding = verified_storage

    def capture_stage(
        self,
        *,
        sample_id: str,
        arm_id: str,
        stage: str,
        raw_input: Any,
        raw_output: Any,
        validated_output: Any,
        canonical_output: Any,
        metadata: Mapping[str, Any],
        provider_io: Any | None = None,
    ) -> Mapping[str, str]:
        directory = (
            self.run_root
            / "samples"
            / self._segment(sample_id)
            / self._segment(arm_id)
            / self._segment(stage.lower())
        )
        directory.mkdir(parents=True, exist_ok=True)
        documents: dict[str, Any] = {
            "raw-input.json": raw_input,
            "raw-output.json": raw_output,
            "validated-output.json": validated_output,
            "canonical-output.json": canonical_output,
        }
        if provider_io is not None:
            documents["raw-provider-io.json"] = provider_io
        hashes: dict[str, str] = {}
        for filename, value in documents.items():
            path = directory / filename
            payload = json_bytes(value)
            self._write_immutable(path, payload)
            hashes[filename] = sha256_bytes(payload)

        meta = dict(metadata)
        meta.update(
            {
                "schema_version": "chesstory.eval.artifact-metadata.v1",
                "run_id": self.run_id,
                "sample_id": sample_id,
                "arm_id": arm_id,
                "stage": stage,
                "captured_at": utc_now(),
                "files": hashes,
            }
        )
        metadata_path = directory / "metadata.json"
        metadata_bytes = json_bytes(meta)
        self._write_immutable(metadata_path, metadata_bytes)
        hashes["metadata.json"] = sha256_bytes(metadata_bytes)
        relative = directory.relative_to(self.run_root).as_posix()
        self._append_chain(
            self.ledger_path,
            {
                "event": "stage-capture",
                "run_id": self.run_id,
                "sample_id": sample_id,
                "arm_id": arm_id,
                "stage": stage,
                "artifact_path": relative,
                "artifact_hashes": hashes,
                "occurred_at": utc_now(),
            },
        )
        return hashes

    def capture_run_document(self, name: str, document: Any) -> str:
        filename = self._segment(name)
        if not filename.endswith(".json"):
            filename += ".json"
        path = self.run_root / filename
        payload = json_bytes(document)
        self._write_immutable(path, payload)
        digest = sha256_bytes(payload)
        self._append_chain(
            self.ledger_path,
            {
                "event": "run-document",
                "run_id": self.run_id,
                "artifact_path": path.relative_to(self.run_root).as_posix(),
                "artifact_hashes": {filename: digest},
                "occurred_at": utc_now(),
            },
        )
        return digest

    def record_access(self, *, role: str, artifact_hash: str, action: str, reason: str) -> None:
        self._append_chain(
            self.access_log_path,
            {
                "event": "artifact-access",
                "run_id": self.run_id,
                "role": role,
                "artifact_hash": artifact_hash,
                "action": action,
                "reason": reason,
                "occurred_at": utc_now(),
            },
        )

    def claim_sealed_split_open(
        self,
        *,
        role: str,
        split_id: str,
        split_hash: str,
        candidate_manifest_hash: str,
        reason: str,
    ) -> None:
        """Atomically consume a sealed split's one-open candidate allowance.

        Claims live under a frozen custodian state root rather than the
        caller-selected artifact root or a particular run directory.  Thus
        neither a new run ID nor a different ``--artifacts`` directory can
        reset the allowance for the same candidate manifest.  The immutable claim is created before
        the append-only run access event.  A crash between those operations
        therefore fails closed: the split remains consumed instead of being
        opened a second time without an audit record.

        Callers that need process-level concurrency must share the same frozen
        custodian state root.  ``O_EXCL`` in ``_write_immutable`` is the cross-process
        arbitration point; no read-then-write race can authorize two opens.
        """

        for label, value in (
            ("role", role),
            ("split_id", split_id),
            ("reason", reason),
        ):
            if not isinstance(value, str) or not value.strip():
                raise IntegrityError(f"sealed access {label} must be non-empty text")
        for label, digest in (
            ("split_hash", split_hash),
            ("candidate_manifest_hash", candidate_manifest_hash),
        ):
            if not isinstance(digest, str) or not re.fullmatch(r"[0-9a-f]{64}", digest):
                raise IntegrityError(
                    f"sealed access {label} must be a lowercase SHA-256"
                )

        if self.custodian_state_root is None or self.custodian_state_id is None:
            raise IntegrityError(
                "sealed split opening requires a frozen custodian state root"
            )
        self._assert_custodian_storage_binding()

        occurred_at = utc_now()
        claim = {
            "schema_version": "chesstory.eval.sealed-open-claim.v1",
            "custodian_state_id": self.custodian_state_id,
            "candidate_manifest_hash": candidate_manifest_hash,
            "split_id": split_id,
            "split_hash": split_hash,
            "first_open_run_id": self.run_id,
            "role": role,
            "reason": reason,
            "occurred_at": occurred_at,
        }
        # Keep the arbitration file as a direct child of the frozen custodian
        # root.  A nested, caller-precreated namespace would let an intermediate
        # directory be replaced by a Windows junction and reset the apparent
        # one-open ledger in another tree.
        claim_path = self._sealed_claim_path(candidate_manifest_hash, split_id)
        self._write_exclusive_immutable(
            claim_path,
            json_bytes(claim),
            conflict_message=(
                "sealed split was already opened for this candidate manifest: "
                f"{split_id}"
            ),
            required_parent=self.custodian_state_root,
        )
        self._append_chain(
            self.access_log_path,
            {
                "event": "artifact-access",
                "run_id": self.run_id,
                "role": role,
                "artifact_hash": split_hash,
                "action": "open-sealed-split-once",
                "reason": reason,
                "candidate_manifest_hash": candidate_manifest_hash,
                "split_id": split_id,
                "claim_sha256": sha256_json(claim),
                "custodian_state_id": self.custodian_state_id,
                "occurred_at": occurred_at,
            },
        )

    def verify(self) -> None:
        if self.custodian_state_root is not None:
            self._assert_custodian_storage_binding()
        self._verify_chain(self.ledger_path)
        self._verify_chain(self.access_log_path)
        self._verify_access_claims()
        if not self.ledger_path.exists():
            return
        with self.ledger_path.open("r", encoding="utf-8") as handle:
            for line_number, line in enumerate(handle, 1):
                record = json.loads(line)
                relative = record.get("artifact_path")
                hashes = record.get("artifact_hashes", {})
                if not relative or not isinstance(hashes, Mapping):
                    continue
                base = self.run_root / relative
                if base.is_file():
                    candidates = {base.name: base}
                else:
                    candidates = {name: base / name for name in hashes}
                for name, expected in hashes.items():
                    path = candidates[name]
                    if not path.is_file():
                        raise IntegrityError(f"ledger artifact missing: {path}")
                    actual = sha256_file(path)
                    if actual != expected:
                        raise IntegrityError(
                            f"ledger artifact hash mismatch at line {line_number}: {path}"
                        )

    def _assert_custodian_storage_binding(self) -> None:
        if (
            self.custodian_state_root is None
            or self.custodian_state_id is None
            or self.custodian_storage_binding is None
        ):
            raise IntegrityError("custodian storage binding is unavailable")
        current = load_custodian_storage_attestation(
            self.custodian_state_root,
            self.custodian_state_id,
            relative_path=self.custodian_storage_binding["path"],
            expected_sha256=self.custodian_storage_binding["sha256"],
        )
        if current != self.custodian_storage_binding:
            raise IntegrityError("custodian storage attestation changed")

    def _verify_access_claims(self) -> None:
        if not self.access_log_path.exists():
            return
        with self.access_log_path.open("r", encoding="utf-8") as handle:
            for line_number, line in enumerate(handle, 1):
                record = json.loads(line)
                if record.get("action") != "open-sealed-split-once":
                    continue
                candidate = record.get("candidate_manifest_hash")
                split_id = record.get("split_id")
                expected = record.get("claim_sha256")
                state_id = record.get("custodian_state_id")
                if (
                    not isinstance(candidate, str)
                    or not re.fullmatch(r"[0-9a-f]{64}", candidate)
                    or not isinstance(split_id, str)
                    or not split_id.strip()
                    or not isinstance(expected, str)
                    or not re.fullmatch(r"[0-9a-f]{64}", expected)
                    or not isinstance(state_id, str)
                    or not state_id.strip()
                ):
                    raise IntegrityError(
                        f"{self.access_log_path}:{line_number}: malformed sealed-open claim binding"
                    )
                if (
                    self.custodian_state_root is None
                    or self.custodian_state_id is None
                    or state_id != self.custodian_state_id
                ):
                    raise IntegrityError(
                        f"{self.access_log_path}:{line_number}: custodian state binding mismatch"
                    )
                claim_path = self._sealed_claim_path(candidate, split_id)
                if not claim_path.is_file():
                    raise IntegrityError(f"sealed-open claim is missing: {claim_path}")
                self._assert_not_link_like(claim_path, "sealed-open claim")
                if sha256_file(claim_path) != expected:
                    raise IntegrityError(
                        f"sealed-open claim hash mismatch: {claim_path}"
                    )

    def ledger_hash(self) -> str | None:
        return sha256_file(self.ledger_path) if self.ledger_path.exists() else None

    def access_log_hash(self) -> str | None:
        return sha256_file(self.access_log_path) if self.access_log_path.exists() else None

    @staticmethod
    def _segment(value: str) -> str:
        segment = re.sub(r"[^A-Za-z0-9._-]+", "-", str(value)).strip(".-")
        if not segment or segment in {".", ".."}:
            raise IntegrityError(f"unsafe artifact path segment: {value!r}")
        return segment[:180]

    def _sealed_claim_path(self, candidate_manifest_hash: str, split_id: str) -> Path:
        if self.custodian_state_root is None:
            raise IntegrityError("sealed-open claim has no custodian state root")
        split_key = sha256_bytes(split_id.encode("utf-8"))
        return self.custodian_state_root / (
            f"sealed-open-claims-v1--{candidate_manifest_hash}--{split_key}.json"
        )

    @staticmethod
    def _assert_not_link_like(path: Path, label: str) -> None:
        try:
            is_link = path.is_symlink()
            is_junction = bool(getattr(path, "is_junction", lambda: False)())
        except OSError as error:
            raise IntegrityError(f"cannot inspect {label}: {path}") from error
        if is_link or is_junction:
            raise IntegrityError(f"{label} must not be a symlink or junction: {path}")

    @staticmethod
    def _write_immutable(path: Path, payload: bytes) -> None:
        if path.exists():
            if path.read_bytes() != payload:
                raise IntegrityError(f"immutable artifact already exists with different bytes: {path}")
            return
        path.parent.mkdir(parents=True, exist_ok=True)
        descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o444)
        try:
            with os.fdopen(descriptor, "wb") as handle:
                handle.write(payload)
                handle.flush()
                os.fsync(handle.fileno())
        except Exception:
            try:
                path.unlink(missing_ok=True)
            finally:
                raise

    @staticmethod
    def _write_exclusive_immutable(
        path: Path,
        payload: bytes,
        *,
        conflict_message: str,
        required_parent: Path | None = None,
    ) -> None:
        """Create a non-idempotent immutable claim with ``O_EXCL`` arbitration."""

        if required_parent is None:
            path.parent.mkdir(parents=True, exist_ok=True)
        else:
            parent = Path(required_parent)
            ArtifactStore._assert_not_link_like(parent, "exclusive claim parent")
            resolved_parent = parent.resolve(strict=True)
            if not resolved_parent.is_dir() or path.parent.resolve(strict=True) != resolved_parent:
                raise IntegrityError("exclusive claim path escaped its frozen parent")
            if path.exists() or path.is_symlink():
                raise IntegrityError(conflict_message)
            ArtifactStore._assert_not_link_like(path, "exclusive claim target")
        try:
            no_follow = getattr(os, "O_NOFOLLOW", 0)
            descriptor = os.open(
                path,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | no_follow,
                0o444,
            )
        except FileExistsError as error:
            raise IntegrityError(conflict_message) from error
        try:
            with os.fdopen(descriptor, "wb") as handle:
                handle.write(payload)
                handle.flush()
                os.fsync(handle.fileno())
            if required_parent is not None:
                ArtifactStore._assert_not_link_like(path, "exclusive claim target")
                if path.parent.resolve(strict=True) != Path(required_parent).resolve(
                    strict=True
                ):
                    raise IntegrityError(
                        "exclusive claim parent changed during atomic creation"
                    )
        except Exception:
            try:
                path.unlink(missing_ok=True)
            finally:
                raise

    def _append_chain(self, path: Path, event: Mapping[str, Any]) -> None:
        previous_hash, sequence = self._chain_tail(path)
        record = dict(event)
        record["sequence"] = sequence
        record["previous_record_hash"] = previous_hash
        record["record_hash"] = sha256_json(record)
        payload = json_bytes(record)
        # Artifact bodies are immutable, but a ledger must remain appendable
        # for the duration of the run.  On Windows, creating it as 0444 makes
        # the second append fail before the hash chain can do its job.
        descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o644)
        with os.fdopen(descriptor, "ab") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())

    @staticmethod
    def _chain_tail(path: Path) -> tuple[str | None, int]:
        if not path.exists() or path.stat().st_size == 0:
            return None, 1
        last: Mapping[str, Any] | None = None
        with path.open("r", encoding="utf-8") as handle:
            for line in handle:
                if line.strip():
                    last = json.loads(line)
        if last is None:
            return None, 1
        return str(last["record_hash"]), int(last["sequence"]) + 1

    @staticmethod
    def _verify_chain(path: Path) -> None:
        if not path.exists():
            return
        expected_previous: str | None = None
        expected_sequence = 1
        with path.open("r", encoding="utf-8") as handle:
            for line_number, line in enumerate(handle, 1):
                record = json.loads(line)
                if record.get("sequence") != expected_sequence:
                    raise IntegrityError(f"{path}:{line_number}: non-contiguous sequence")
                if record.get("previous_record_hash") != expected_previous:
                    raise IntegrityError(f"{path}:{line_number}: hash-chain predecessor mismatch")
                claimed = record.get("record_hash")
                unsigned = dict(record)
                unsigned.pop("record_hash", None)
                actual = sha256_json(unsigned)
                if claimed != actual:
                    raise IntegrityError(f"{path}:{line_number}: record hash mismatch")
                expected_previous = str(claimed)
                expected_sequence += 1


def signed_document(document: Mapping[str, Any], key: bytes, key_id: str) -> dict[str, Any]:
    """HMAC-sign a frozen manifest/attestation; secret keys are never persisted."""
    unsigned = dict(document)
    unsigned.pop("signature", None)
    digest = hmac.new(key, json_bytes(unsigned), "sha256").hexdigest()
    return {
        **unsigned,
        "signature": {
            "algorithm": "HMAC-SHA256",
            "key_id": key_id,
            "value": digest,
        },
    }


def verify_signed_document(document: Mapping[str, Any], key: bytes) -> None:
    signature = document.get("signature")
    if not isinstance(signature, Mapping) or signature.get("algorithm") != "HMAC-SHA256":
        raise IntegrityError("missing or unsupported document signature")
    unsigned = dict(document)
    unsigned.pop("signature", None)
    expected = hmac.new(key, json_bytes(unsigned), "sha256").hexdigest()
    if not hmac.compare_digest(str(signature.get("value", "")), expected):
        raise IntegrityError("document signature mismatch")


def write_signed_manifest(path: Path, document: Mapping[str, Any], key: bytes, key_id: str) -> str:
    signed = signed_document(document, key, key_id)
    payload = json_bytes(signed)
    ArtifactStore._write_immutable(path, payload)
    return sha256_bytes(payload)
