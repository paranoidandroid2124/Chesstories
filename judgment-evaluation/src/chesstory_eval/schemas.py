from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Sequence
from urllib.parse import urldefrag

from .hashing import sha256_json
from .model import ContractError, STAGES, require_stage


_SUPPORTED_SCHEMA_KEYWORDS = frozenset(
    {
        "$schema",
        "$id",
        "$ref",
        "$defs",
        "$comment",
        "title",
        "description",
        "default",
        "deprecated",
        "readOnly",
        "writeOnly",
        "examples",
        "allOf",
        "anyOf",
        "oneOf",
        "not",
        "if",
        "then",
        "else",
        "const",
        "enum",
        "type",
        "propertyNames",
        "required",
        "properties",
        "patternProperties",
        "additionalProperties",
        "dependentRequired",
        "minProperties",
        "maxProperties",
        "minItems",
        "maxItems",
        "uniqueItems",
        "prefixItems",
        "items",
        "contains",
        "minContains",
        "maxContains",
        "minLength",
        "maxLength",
        "pattern",
        "format",
        "minimum",
        "maximum",
        "exclusiveMinimum",
        "exclusiveMaximum",
        "x-canonical",
        "x-canonical-annotation-registry",
        "x-semantic-transition-registry",
    }
)

_SCHEMA_MAP_KEYWORDS = ("$defs", "properties", "patternProperties")
_SCHEMA_LIST_KEYWORDS = ("allOf", "anyOf", "oneOf", "prefixItems")
_SCHEMA_SINGLE_KEYWORDS = (
    "not",
    "if",
    "then",
    "else",
    "propertyNames",
    "additionalProperties",
    "items",
    "contains",
)


_PUBLIC_V6_PROOF_TRANSPORT_SCHEMA_IDS = frozenset(
    {
        "https://chesstories.dev/judgment-evaluation/schemas/public-v6/move-meaning-response.schema.json",
        "https://chesstories.dev/judgment-evaluation/schemas/public-commentary-v6/position-commentary-response.schema.json",
    }
)


def _normalized_fen(value: Any) -> str | None:
    return " ".join(value.split()) if isinstance(value, str) and value.strip() else None


@dataclass(frozen=True)
class IdentityOccurrence:
    """A definition/reference discovered from the frozen schema annotations."""

    kind: str
    namespace: str
    value: str
    path: tuple[str | int, ...]


class SchemaRegistry:
    """Loads the frozen schemas and validates their supported Draft 2020-12 vocabulary.

    The harness intentionally has no network dependency.  The validator covers the
    vocabulary used by this repository's schemas and rejects unknown remote refs.
    """

    def __init__(self, root: Path) -> None:
        self.root = root.resolve()
        self._documents: dict[Path, Mapping[str, Any]] = {}
        self._resolved_paths: dict[Path, Path] = {self.root: self.root}

    def _resolved_schema_path(self, path: Path) -> Path:
        """Resolve each frozen schema path once instead of once per tree node."""

        candidate = Path(path)
        resolved = self._resolved_paths.get(candidate)
        if resolved is None:
            resolved = candidate.resolve()
            self._resolved_paths[candidate] = resolved
            self._resolved_paths.setdefault(resolved, resolved)
        return resolved

    def schema_path(self, stage: str, direction: str) -> Path:
        require_stage(stage)
        if direction not in {"input", "output"}:
            raise ContractError(f"unknown schema direction: {direction}")
        return self.root / f"{stage.lower()}.{direction}.schema.json"

    def schema(self, stage: str, direction: str) -> tuple[Mapping[str, Any], Path]:
        path = self.schema_path(stage, direction)
        return self.load(path), path

    def load(self, path: Path) -> Mapping[str, Any]:
        resolved = self._resolved_schema_path(path)
        if self.root not in resolved.parents and resolved != self.root:
            raise ContractError(f"schema escapes registry root: {path}")
        if resolved not in self._documents:
            try:
                with resolved.open("r", encoding="utf-8") as handle:
                    document = json.load(handle)
            except (OSError, json.JSONDecodeError) as error:
                raise ContractError(f"cannot load schema {resolved}: {error}") from error
            if not isinstance(document, dict):
                raise ContractError(f"schema root must be an object: {resolved}")
            self._validate_schema_vocabulary(document, str(resolved))
            self._documents[resolved] = document
        return self._documents[resolved]

    def _validate_schema_vocabulary(
        self,
        raw_schema: Mapping[str, Any] | bool,
        location: str,
    ) -> None:
        """Reject validation vocabulary that this offline engine cannot execute."""

        if isinstance(raw_schema, bool):
            return
        if not isinstance(raw_schema, Mapping):
            raise ContractError(f"{location}: schema must be an object or boolean")

        unsupported = sorted(set(raw_schema).difference(_SUPPORTED_SCHEMA_KEYWORDS))
        if unsupported:
            raise ContractError(
                f"{location}: unsupported schema keyword(s): {unsupported}"
            )
        schema_format = raw_schema.get("format")
        if schema_format is not None and schema_format != "sha256":
            raise ContractError(
                f"{location}: unsupported schema format {schema_format!r}"
            )

        for keyword in _SCHEMA_MAP_KEYWORDS:
            children = raw_schema.get(keyword)
            if children is None:
                continue
            if not isinstance(children, Mapping):
                raise ContractError(f"{location}.{keyword}: expected object")
            for name, child in children.items():
                self._validate_schema_vocabulary(
                    child,
                    f"{location}.{keyword}.{name}",
                )

        for keyword in _SCHEMA_LIST_KEYWORDS:
            children = raw_schema.get(keyword)
            if children is None:
                continue
            if not isinstance(children, list):
                raise ContractError(f"{location}.{keyword}: expected array")
            for index, child in enumerate(children):
                self._validate_schema_vocabulary(
                    child,
                    f"{location}.{keyword}[{index}]",
                )

        for keyword in _SCHEMA_SINGLE_KEYWORDS:
            if keyword not in raw_schema:
                continue
            self._validate_schema_vocabulary(
                raw_schema[keyword],
                f"{location}.{keyword}",
            )

        dependent_required = raw_schema.get("dependentRequired")
        if dependent_required is not None:
            if not isinstance(dependent_required, Mapping) or any(
                not isinstance(required, list)
                or any(not isinstance(name, str) for name in required)
                for required in dependent_required.values()
            ):
                raise ContractError(
                    f"{location}.dependentRequired: expected arrays of property names"
                )

    def resolve_ref(self, reference: str, base_path: Path) -> tuple[Mapping[str, Any], Path]:
        target, fragment = urldefrag(reference)
        if "://" in target or target.startswith("urn:"):
            raise ContractError(f"remote schema refs are forbidden: {reference}")
        path = self._resolved_schema_path(
            base_path.parent / target if target else base_path
        )
        document: Any = self.load(path)
        if fragment:
            if not fragment.startswith("/"):
                raise ContractError(f"only JSON Pointer schema fragments are supported: {reference}")
            for raw_part in fragment[1:].split("/"):
                part = raw_part.replace("~1", "/").replace("~0", "~")
                if not isinstance(document, Mapping) or part not in document:
                    raise ContractError(f"unresolved schema ref: {reference}")
                document = document[part]
        if not isinstance(document, Mapping):
            raise ContractError(f"schema ref does not resolve to an object: {reference}")
        return document, path

    def dereference(
        self, schema: Mapping[str, Any], base_path: Path
    ) -> tuple[Mapping[str, Any], Path]:
        seen: set[tuple[Path, str]] = set()
        current, current_path = schema, base_path
        while "$ref" in current:
            reference = current["$ref"]
            if not isinstance(reference, str):
                raise ContractError("$ref must be a string")
            marker = (self._resolved_schema_path(current_path), reference)
            if marker in seen:
                raise ContractError(f"cyclic direct schema ref: {reference}")
            seen.add(marker)
            current, current_path = self.resolve_ref(reference, current_path)
        return current, current_path

    def validate_stage(self, document: Any, stage: str, direction: str) -> None:
        schema, path = self.schema(stage, direction)
        errors: list[str] = []
        self._validate(document, schema, path, "$", errors)
        if isinstance(document, Mapping):
            if document.get("stage") != stage:
                errors.append(f"$.stage: expected {stage!r}")
            forbidden = sorted({"arm", "source", "oracle_chain"}.intersection(document))
            if forbidden:
                errors.append(f"$: arm provenance must be out-of-band, found {forbidden}")
        if errors:
            preview = "\n".join(f"- {error}" for error in errors[:30])
            suffix = f"\n- ... {len(errors) - 30} more" if len(errors) > 30 else ""
            raise ContractError(f"{stage} {direction} schema validation failed:\n{preview}{suffix}")

    def validate_document(self, document: Any, path: Path, *, label: str) -> None:
        """Validate a non-stage document with the same offline schema engine.

        Diagnostic side paths use their own schemas and must not be smuggled
        through a frozen Q-to-P stage contract.  This additive entry point
        keeps those documents under the same no-network, fail-closed validator
        without changing any stage validation behavior.
        """

        schema = self.load(path)
        errors: list[str] = []
        self._validate(document, schema, self._resolved_schema_path(path), "$", errors)
        if (
            not errors
            and schema.get("$id") in _PUBLIC_V6_PROOF_TRANSPORT_SCHEMA_IDS
        ):
            self._validate_public_proof_transport(document, "$", errors)
        if errors:
            preview = "\n".join(f"- {error}" for error in errors[:30])
            suffix = f"\n- ... {len(errors) - 30} more" if len(errors) > 30 else ""
            raise ContractError(f"{label} schema validation failed:\n{preview}{suffix}")

    def _validate_public_proof_transport(
        self,
        value: Any,
        location: str,
        errors: list[str],
    ) -> None:
        """Validate only public proof identity, ownership, and occurrence wiring."""

        if isinstance(value, Mapping):
            if isinstance(value.get("primary"), Mapping):
                if isinstance(value.get("occurrence_explanations"), list):
                    self._validate_occurrence_explanation_transport(value, location, errors)
            for key, child in value.items():
                self._validate_public_proof_transport(
                    child,
                    f"{location}.{key}",
                    errors,
                )
        elif isinstance(value, list):
            for index, child in enumerate(value):
                self._validate_public_proof_transport(
                    child,
                    f"{location}[{index}]",
                    errors,
                )

    @staticmethod
    def _unique_objects_by_id(
        values: list[Any],
        id_key: str,
        location: str,
        errors: list[str],
    ) -> dict[str, Mapping[str, Any]]:
        indexed: dict[str, Mapping[str, Any]] = {}
        for index, item in enumerate(values):
            if not isinstance(item, Mapping):
                continue
            identifier = item.get(id_key)
            if not isinstance(identifier, str):
                continue
            if identifier in indexed:
                errors.append(
                    f"{location}[{index}].{id_key}: duplicate public identifier {identifier!r}"
                )
            else:
                indexed[identifier] = item
        return indexed

    @staticmethod
    def _validate_branch_reference(
        branch_id: Any,
        claimed_role: Any,
        branches: Mapping[str, Mapping[str, Any]],
        role_key: str,
        location: str,
        errors: list[str],
    ) -> None:
        if not isinstance(branch_id, str):
            return
        branch = branches.get(branch_id)
        if branch is None:
            errors.append(f"{location}: unknown branch_id {branch_id!r}")
        elif claimed_role is not None and branch.get(role_key) != claimed_role:
            errors.append(
                f"{location}: branch role {claimed_role!r} does not match "
                f"{branch.get(role_key)!r}"
            )

    def _validate_occurrence_explanation_transport(
        self,
        commentary: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        explanations = commentary.get("occurrence_explanations")
        if not isinstance(explanations, list):
            return
        seen_causes: set[str] = set()
        seen_proofs: set[tuple[Any, Any, Any, Any]] = set()
        for index, explanation in enumerate(explanations):
            if not isinstance(explanation, Mapping):
                continue
            item_location = f"{location}.occurrence_explanations[{index}]"
            cause_id = explanation.get("cause_evidence_id")
            if isinstance(cause_id, str):
                if cause_id in seen_causes:
                    errors.append(f"{item_location}.cause_evidence_id: duplicate occurrence Cause owner")
                seen_causes.add(cause_id)

            proof_kind = explanation.get("proof_kind")
            proof = explanation.get("proof")
            if not isinstance(proof, Mapping):
                continue
            proof_identity = (
                proof_kind,
                proof.get("semantic_id"),
                proof.get("occurrence_id"),
                proof.get("dependency_fingerprint"),
            )
            if proof_identity in seen_proofs:
                errors.append(f"{item_location}: duplicate typed proof occurrence")
            seen_proofs.add(proof_identity)

            subject = explanation.get("subject_occurrence")
            if isinstance(subject, Mapping):
                if subject.get("occurrence_id") != subject.get("transition_evidence_id"):
                    errors.append(
                        f"{item_location}.subject_occurrence: occurrence_id must be the exact transition owner"
                    )
            self._validate_occurrence_proof_transport(
                proof,
                subject,
                item_location,
                errors,
            )

    @staticmethod
    def _occurrence_proof_branches(
        proof: Mapping[str, Any],
    ) -> list[Mapping[str, Any]]:
        raw_branches = proof.get("branches")
        if isinstance(raw_branches, list):
            return [branch for branch in raw_branches if isinstance(branch, Mapping)]
        return [
            branch
            for key, branch in proof.items()
            if key.endswith("_branch")
            and isinstance(branch, Mapping)
            and isinstance(branch.get("branch_id"), str)
            and isinstance(branch.get("steps"), list)
        ]

    def _validate_occurrence_proof_transport(
        self,
        proof: Mapping[str, Any],
        subject: Any,
        location: str,
        errors: list[str],
    ) -> None:
        branch_values = self._occurrence_proof_branches(proof)
        branches = self._unique_objects_by_id(
            branch_values,
            "branch_id",
            f"{location}.proof.branches",
            errors,
        )
        self._validate_occurrence_branch_routes(
            branch_values,
            f"{location}.proof",
            errors,
        )
        self._validate_occurrence_subject_membership(
            subject,
            branch_values,
            f"{location}.subject_occurrence",
            errors,
        )
        self._validate_occurrence_proof_references(
            proof,
            branches,
            f"{location}.proof",
            errors,
        )

    @staticmethod
    def _validate_occurrence_branch_routes(
        branch_values: list[Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        for branch_index, branch in enumerate(branch_values):
            raw_steps = branch.get("steps")
            if not isinstance(raw_steps, list):
                continue
            steps = [step for step in raw_steps if isinstance(step, Mapping)]
            if not steps:
                continue
            branch_location = f"{location}.branches[{branch_index}]"
            root = steps[0]
            if "root_move" in branch and branch.get("root_move") != root.get("move_uci"):
                errors.append(
                    f"{branch_location}.root_move: does not identify the first branch occurrence"
                )
            line = branch.get("line")
            if isinstance(line, Mapping) and line.get("root_move") != root.get("move_uci"):
                errors.append(
                    f"{branch_location}.line.root_move: does not identify the first branch occurrence"
                )
            for step_index, step in enumerate(steps):
                step_location = f"{branch_location}.steps[{step_index}]"
                if step.get("step_index") != step_index:
                    errors.append(
                        f"{step_location}.step_index: branch occurrences are not in exact order"
                    )
                if isinstance(line, Mapping) and step.get("line") != line:
                    errors.append(
                        f"{step_location}.line: occurrence does not retain its line owner"
                    )
                if step_index == 0:
                    continue
                previous = steps[step_index - 1]
                previous_ply = previous.get("ply")
                if not isinstance(previous_ply, int) or step.get("ply") != previous_ply + 1:
                    errors.append(
                        f"{step_location}.ply: adjacent occurrences must advance by one ply"
                    )
                if _normalized_fen(step.get("fen_before")) != _normalized_fen(
                    previous.get("fen_after")
                ):
                    errors.append(
                        f"{step_location}.fen_before: adjacent occurrence route is discontinuous"
                    )

    @staticmethod
    def _validate_occurrence_subject_membership(
        subject: Any,
        branch_values: list[Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        if not isinstance(subject, Mapping):
            return
        start = subject.get("start")
        destination = subject.get("destination")
        if not isinstance(start, Mapping) or not isinstance(destination, Mapping):
            return

        for branch in branch_values:
            raw_steps = branch.get("steps")
            if not isinstance(raw_steps, list) or not raw_steps:
                continue
            root = raw_steps[0]
            if not isinstance(root, Mapping):
                continue
            line = branch.get("line")
            line_id = (
                branch.get("line_id")
                if isinstance(branch.get("line_id"), str)
                else line.get("line_id")
                if isinstance(line, Mapping)
                else None
            )
            line_owner_evidence_id = branch.get("line_owner_evidence_id")
            owner_matches = (
                not isinstance(line_owner_evidence_id, str)
                or subject.get("line_owner_evidence_id") == line_owner_evidence_id
            )
            root_transition_evidence_id = branch.get("root_transition_evidence_id")
            transition_matches = (
                not isinstance(root_transition_evidence_id, str)
                or subject.get("transition_evidence_id")
                == root_transition_evidence_id
            )
            root_ply = root.get("ply")
            if (
                subject.get("root_provenance") == branch.get("root_provenance")
                and subject.get("line_id") == line_id
                and subject.get("move_uci") == root.get("move_uci")
                and owner_matches
                and transition_matches
                and _normalized_fen(start.get("fen"))
                == _normalized_fen(root.get("fen_before"))
                and isinstance(root_ply, int)
                and start.get("ply") == root_ply - 1
                and _normalized_fen(destination.get("fen"))
                == _normalized_fen(root.get("fen_after"))
                and destination.get("ply") == root_ply
            ):
                return
        errors.append(f"{location}: subject does not own any supplied branch root occurrence")

    def _validate_occurrence_proof_references(
        self,
        proof: Mapping[str, Any],
        branches: Mapping[str, Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        raw_paths = proof.get("proof_paths")
        paths = raw_paths if isinstance(raw_paths, list) else []
        self._unique_objects_by_id(
            paths,
            "path_occurrence_id",
            f"{location}.proof_paths",
            errors,
        )
        path_ids = [
            path.get("path_occurrence_id")
            for path in paths
            if isinstance(path, Mapping)
            and isinstance(path.get("path_occurrence_id"), str)
        ]
        if path_ids != sorted(path_ids):
            errors.append(
                f"{location}.proof_paths: path_occurrence_id values must be canonical"
            )
        path_bodies = [
            json.dumps(
                {key: value for key, value in path.items() if key != "path_occurrence_id"},
                ensure_ascii=False,
                sort_keys=True,
                separators=(",", ":"),
            )
            for path in paths
            if isinstance(path, Mapping)
        ]
        if len(path_bodies) != len(set(path_bodies)):
            errors.append(
                f"{location}.proof_paths: an identical proof path cannot be relabeled as independent"
            )

        lower_ids = proof.get("lower_premise_ids")
        if isinstance(lower_ids, list) and lower_ids != sorted(lower_ids):
            errors.append(f"{location}.lower_premise_ids: values must be canonical")

        def validate_reference(value: Any, value_location: str) -> None:
            if isinstance(value, Mapping):
                branch_id = value.get("branch_id")
                if isinstance(branch_id, str):
                    branch = branches.get(branch_id)
                    role_key = (
                        "branch_role"
                        if isinstance(branch, Mapping) and "branch_role" in branch
                        else "role"
                    )
                    self._validate_branch_reference(
                        branch_id,
                        value.get("branch_role"),
                        branches,
                        role_key,
                        f"{value_location}.branch_id",
                        errors,
                    )
                    if isinstance(branch, Mapping):
                        raw_steps = branch.get("steps")
                        steps = raw_steps if isinstance(raw_steps, list) else []
                        for index_key in (
                            "step_index",
                            "after_step_index",
                            "from_step_index",
                            "to_step_index",
                        ):
                            step_index = value.get(index_key)
                            if step_index is not None and (
                                not isinstance(step_index, int)
                                or not 0 <= step_index < len(steps)
                            ):
                                errors.append(
                                    f"{value_location}.{index_key}: does not reference a branch occurrence"
                                )
                        step_index = value.get("step_index")
                        if (
                            isinstance(step_index, int)
                            and 0 <= step_index < len(steps)
                            and isinstance(steps[step_index], Mapping)
                            and isinstance(value.get("move_uci"), str)
                            and value.get("move_uci") != steps[step_index].get("move_uci")
                        ):
                            errors.append(
                                f"{value_location}.move_uci: does not equal its branch occurrence"
                            )
                        overall_move = value.get("overall_move_uci")
                        if (
                            isinstance(step_index, int)
                            and 0 <= step_index < len(steps)
                            and isinstance(steps[step_index], Mapping)
                            and isinstance(overall_move, str)
                            and overall_move != steps[step_index].get("move_uci")
                        ):
                            errors.append(
                                f"{value_location}.overall_move_uci: does not equal its branch occurrence"
                            )
                        after_step_index = value.get("after_step_index")
                        position = value.get("position")
                        if (
                            isinstance(after_step_index, int)
                            and 0 <= after_step_index < len(steps)
                            and isinstance(steps[after_step_index], Mapping)
                            and isinstance(position, Mapping)
                            and (
                                _normalized_fen(position.get("fen"))
                                != _normalized_fen(steps[after_step_index].get("fen_after"))
                                or position.get("ply")
                                != steps[after_step_index].get("ply")
                            )
                        ):
                            errors.append(
                                f"{value_location}.position: does not bind the supplied branch occurrence"
                            )

                continuation_branch_id = value.get("analysis_continuation_branch_id")
                if isinstance(continuation_branch_id, str):
                    self._validate_branch_reference(
                        continuation_branch_id,
                        None,
                        branches,
                        "branch_role",
                        f"{value_location}.analysis_continuation_branch_id",
                        errors,
                    )

                source_ids = value.get("source_premise_ids")
                if isinstance(source_ids, list):
                    if source_ids != sorted(source_ids):
                        errors.append(
                            f"{value_location}.source_premise_ids: values must be canonical"
                        )
                    for owner_key in ("issuer_evidence_id", "issuer_occurrence_id"):
                        owner = value.get(owner_key)
                        if isinstance(owner, str) and owner not in source_ids:
                            errors.append(
                                f"{value_location}.{owner_key}: owner is not retained by source_premise_ids"
                            )

                for key, child in value.items():
                    validate_reference(child, f"{value_location}.{key}")
            elif isinstance(value, list):
                for index, child in enumerate(value):
                    validate_reference(child, f"{value_location}[{index}]")

        validate_reference(proof, location)

        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            result_ids = [
                premise.get("result_id")
                for premise in path.get("premises", [])
                if isinstance(premise, Mapping)
                and isinstance(premise.get("result_id"), str)
            ]
            if len(result_ids) != len(set(result_ids)):
                errors.append(
                    f"{path_location}.premises: result_id values must be unique"
                )
            premises = [
                premise
                for premise in path.get("premises", [])
                if isinstance(premise, Mapping)
            ]
            continuity = [
                (premise_index, premise)
                for premise_index, premise in enumerate(premises)
                if premise.get("contract") == "object_continuity_step"
            ]
            continuity_keys = [
                (
                    premise.get("role"),
                    premise.get("branch_id"),
                    premise.get("step_index"),
                    premise.get("issuer_occurrence_id"),
                )
                for _, premise in continuity
            ]
            if len(continuity_keys) != len(set(continuity_keys)):
                errors.append(
                    f"{path_location}.premises: object-continuity occurrence bindings must be unique"
                )

            retained: list[Mapping[str, Any]] = []
            for premise_index, premise in continuity:
                premise_location = f"{path_location}.premises[{premise_index}]"
                source_ids = premise.get("source_premise_ids")
                footprint_id = premise.get("transition_footprint_id")
                legal_move_id = premise.get("legal_move_semantic_id")
                issuer_evidence_id = premise.get("issuer_evidence_id")
                issuer_occurrence_id = premise.get("issuer_occurrence_id")
                if all(
                    isinstance(value, str)
                    for value in (
                        footprint_id,
                        legal_move_id,
                        issuer_evidence_id,
                        issuer_occurrence_id,
                    )
                ) and source_ids != sorted(
                    [
                        issuer_evidence_id,
                        issuer_occurrence_id,
                        f"legal-move:{legal_move_id}",
                        f"transition-footprint:{footprint_id}",
                    ]
                ):
                    errors.append(
                        f"{premise_location}.source_premise_ids: continuity must retain exactly its four certified owners"
                    )

                before = premise.get("before")
                after = premise.get("after")
                selected = premise.get("selected_transition")
                transition_kind = premise.get("transition_kind")
                if not isinstance(before, Mapping) or not isinstance(after, Mapping):
                    continue
                if transition_kind == "retained":
                    retained.append(premise)
                    if "selected_transition" in premise or before != after:
                        errors.append(
                            f"{premise_location}: retained continuity must omit selected_transition and retain one exact object"
                        )
                elif transition_kind in ("primary", "secondary"):
                    if not isinstance(selected, Mapping) or (
                        selected.get("side") != before.get("side")
                        or selected.get("from") != before.get("square")
                        or selected.get("piece_before") != before.get("piece")
                        or selected.get("to") != after.get("square")
                        or selected.get("piece_after") != after.get("piece")
                    ):
                        errors.append(
                            f"{premise_location}.selected_transition: does not bind the exact before/after object"
                        )

            use_ids = [
                use.get("use_id")
                for collection in ("closed_absence_uses", "closed_state_uses")
                for use in path.get(collection, [])
                if isinstance(use, Mapping) and isinstance(use.get("use_id"), str)
            ]
            if len(use_ids) != len(set(use_ids)):
                errors.append(
                    f"{path_location}: closed use_id values must be unique within the proof path"
                )
            if continuity:
                continuity_roles = {
                    premise.get("role") for _, premise in continuity
                }
                continuity_states = [
                    state
                    for state in path.get("closed_state_uses", [])
                    if isinstance(state, Mapping) and state.get("role") in continuity_roles
                ]
                if len(continuity_states) != len(retained):
                    errors.append(
                        f"{path_location}.closed_state_uses: every retained continuity step needs exactly one state binding"
                    )
                for premise in retained:
                    after = premise.get("after")
                    if not isinstance(after, Mapping):
                        continue
                    expected_query = (
                        f"occupied-by:{after.get('side')}:{after.get('piece')}@{after.get('square')}"
                    )
                    matches = [
                        state
                        for state in continuity_states
                        if state.get("role") == premise.get("role")
                        and state.get("branch_id") == premise.get("branch_id")
                        and state.get("branch_role") == premise.get("branch_role")
                        and state.get("after_step_index") == premise.get("step_index")
                        and state.get("query") == expected_query
                    ]
                    if len(matches) != 1:
                        errors.append(
                            f"{path_location}.closed_state_uses: retained continuity is not paired to one exact occupied state"
                        )

        if {
            "relocated_responder_branch",
            "retained_responder_branch",
            "relocation",
            "target_capture",
            "relocated_responder_recapture",
            "retained_other_recapture",
        }.issubset(proof):
            self._validate_relocation_transport_references(proof, location, errors)

    @staticmethod
    def _validate_relocation_transport_references(
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        paths = proof.get("proof_paths")
        participants = proof.get("participants")
        relocated = proof.get("relocated_responder_branch")
        retained = proof.get("retained_responder_branch")
        relocation = proof.get("relocation")
        target_capture = proof.get("target_capture")
        relocated_recapture = proof.get("relocated_responder_recapture")
        retained_recapture = proof.get("retained_other_recapture")
        if not (
            isinstance(paths, list)
            and len(paths) == 1
            and isinstance(paths[0], Mapping)
            and isinstance(participants, Mapping)
            and isinstance(relocated, Mapping)
            and isinstance(retained, Mapping)
            and isinstance(relocation, Mapping)
            and isinstance(target_capture, Mapping)
            and isinstance(relocated_recapture, Mapping)
            and isinstance(retained_recapture, Mapping)
        ):
            return
        premises = paths[0].get("premises")
        if not isinstance(premises, list):
            return
        continuity = [
            premise
            for premise in premises
            if isinstance(premise, Mapping)
            and premise.get("contract") == "object_continuity_step"
        ]

        def continuity_reference(
            role: str,
            branch: Mapping[str, Any],
            until: Any,
            root_piece: Any,
            endpoint_piece: Any,
        ) -> bool:
            uses = [premise for premise in continuity if premise.get("role") == role]
            endpoint_references_match = (
                isinstance(root_piece, Mapping)
                and isinstance(endpoint_piece, Mapping)
                and (
                    root_piece == endpoint_piece
                    if until == 0
                    else bool(uses)
                    and uses[0].get("before") == root_piece
                    and uses[-1].get("after") == endpoint_piece
                )
            )
            return (
                isinstance(until, int)
                and until >= 0
                and [premise.get("step_index") for premise in uses]
                == list(range(until))
                and all(
                    premise.get("branch_id") == branch.get("branch_id")
                    and premise.get("branch_role") == branch.get("branch_role")
                    for premise in uses
                )
                and endpoint_references_match
            )

        sequences = (
            (
                "relocated_branch_target_continuity",
                relocated,
                target_capture.get("relocated_step_index"),
                participants.get("target_at_common_root"),
                participants.get("captured_target"),
            ),
            (
                "retained_branch_target_continuity",
                retained,
                target_capture.get("retained_step_index"),
                participants.get("target_at_common_root"),
                participants.get("captured_target"),
            ),
            (
                "relocated_branch_attacker_continuity",
                relocated,
                target_capture.get("relocated_step_index"),
                participants.get("attacker_at_common_root"),
                participants.get("attacker_at_capture"),
            ),
            (
                "retained_branch_attacker_continuity",
                retained,
                target_capture.get("retained_step_index"),
                participants.get("attacker_at_common_root"),
                participants.get("attacker_at_capture"),
            ),
            (
                "relocated_responder_continuity",
                relocated,
                relocated_recapture.get("step_index"),
                participants.get("tracked_responder_at_seed"),
                participants.get("tracked_responder_at_staging"),
            ),
            (
                "retained_responder_continuity",
                retained,
                retained_recapture.get("step_index"),
                participants.get("tracked_responder_at_seed"),
                participants.get("tracked_responder_at_seed"),
            ),
        )
        for role, branch, until, root_piece, endpoint_piece in sequences:
            if not continuity_reference(role, branch, until, root_piece, endpoint_piece):
                errors.append(
                    f"{location}.proof_paths[0].premises: {role} does not retain every ordered branch occurrence"
                )

        relation_premises = [
            premise
            for premise in premises
            if isinstance(premise, Mapping)
            and premise.get("contract") == "capture_recapture_inventory"
        ]

        def relation_reference(
            role: str,
            branch: Mapping[str, Any],
            step_index: Any,
        ) -> bool:
            matches = [
                premise for premise in relation_premises if premise.get("role") == role
            ]
            return (
                len(matches) == 1
                and isinstance(step_index, int)
                and matches[0].get("branch_id") == branch.get("branch_id")
                and matches[0].get("branch_role") == branch.get("branch_role")
                and matches[0].get("step_index") == step_index
            )

        relation_references_closed = relation_reference(
            "relocated_recapture_inventory",
            relocated,
            target_capture.get("relocated_step_index"),
        ) and relation_reference(
            "retained_recapture_inventory",
            retained,
            target_capture.get("retained_step_index"),
        )
        if not relation_references_closed:
            errors.append(
                f"{location}.proof_paths[0].premises: recapture inventories do not bind their target-capture occurrences"
            )

        absences = paths[0].get("closed_absence_uses")
        tracked_responder = participants.get("tracked_responder_at_seed")
        recapture_square = participants.get("recapture_square")
        absence = (
            absences[0]
            if isinstance(absences, list)
            and len(absences) == 1
            and isinstance(absences[0], Mapping)
            else None
        )
        expected_absence_query = (
            f"legal-move-from-to:{tracked_responder.get('side')}:"
            f"{tracked_responder.get('square')}:{recapture_square}"
            if isinstance(tracked_responder, Mapping)
            and isinstance(recapture_square, str)
            else None
        )
        if not (
            isinstance(absence, Mapping)
            and absence.get("branch_id") == retained.get("branch_id")
            and absence.get("branch_role") == retained.get("branch_role")
            and absence.get("after_step_index")
            == target_capture.get("retained_step_index")
            and absence.get("query") == expected_absence_query
        ):
            errors.append(
                f"{location}.proof_paths[0].closed_absence_uses: retained seed absence does not bind its capture occurrence and query"
            )

        legal_premises = [
            premise
            for premise in premises
            if isinstance(premise, Mapping)
            and premise.get("contract") == "legal_move"
        ]

        def legal_endpoint(
            role: str,
            branch: Mapping[str, Any],
            step_index: Any,
            endpoint: Mapping[str, Any],
            captured_target: Any = None,
        ) -> bool:
            matches = [
                premise for premise in legal_premises if premise.get("role") == role
            ]
            premise = matches[0] if len(matches) == 1 else None
            source_ids = premise.get("source_premise_ids") if premise else None
            source_owners = (
                [
                    premise.get("issuer_evidence_id"),
                    premise.get("issuer_occurrence_id"),
                    f"legal-move:{premise.get('legal_move_semantic_id')}",
                ]
                if premise
                else []
            )
            return (
                isinstance(premise, Mapping)
                and isinstance(step_index, int)
                and premise.get("branch_id") == branch.get("branch_id")
                and premise.get("branch_role") == branch.get("branch_role")
                and premise.get("step_index") == step_index
                and premise.get("move_uci") == endpoint.get("move_uci")
                and premise.get("movement") == endpoint.get("movement")
                and isinstance(premise.get("capture"), Mapping)
                and all(isinstance(owner, str) for owner in source_owners)
                and source_ids == sorted(source_owners)
                and (
                    captured_target is None
                    or premise.get("capture") == captured_target
                )
            )

        target_movement = target_capture.get("movement")
        recaptured_attacker = (
            {
                "side": target_movement.get("side"),
                "piece": target_movement.get("piece_after"),
                "square": participants.get("recapture_square"),
            }
            if isinstance(target_movement, Mapping)
            and isinstance(participants.get("recapture_square"), str)
            else None
        )
        endpoint_references_closed = (
            legal_endpoint(
                "relocated_target_capture",
                relocated,
                target_capture.get("relocated_step_index"),
                target_capture,
                participants.get("captured_target"),
            )
            and legal_endpoint(
                "retained_target_capture",
                retained,
                target_capture.get("retained_step_index"),
                target_capture,
                participants.get("captured_target"),
            )
            and legal_endpoint(
                "relocated_responder_recapture",
                relocated,
                relocated_recapture.get("step_index"),
                relocated_recapture,
                recaptured_attacker,
            )
            and legal_endpoint(
                "retained_other_recapture",
                retained,
                retained_recapture.get("step_index"),
                retained_recapture,
                recaptured_attacker,
            )
            and isinstance(participants.get("recapture_square"), str)
            and isinstance(target_capture.get("movement"), Mapping)
            and target_capture["movement"].get("to")
            == participants.get("recapture_square")
            and isinstance(relocated_recapture.get("movement"), Mapping)
            and relocated_recapture["movement"].get("to")
            == participants.get("recapture_square")
            and isinstance(retained_recapture.get("movement"), Mapping)
            and retained_recapture["movement"].get("to")
            == participants.get("recapture_square")
        )
        relocation_uses = [
            premise
            for premise in continuity
            if premise.get("role") == "relocated_responder_continuity"
            and premise.get("step_index") == relocation.get("step_index")
            and premise.get("overall_move_uci") == relocation.get("move_uci")
            and premise.get("selected_transition") == relocation.get("movement")
        ]
        if not endpoint_references_closed:
            errors.append(
                f"{location}: top-level capture references do not bind their exact premise occurrences and recapture square"
            )
        if len(relocation_uses) != 1:
            errors.append(
                f"{location}.relocation: does not bind one exact continuity transition"
            )

    def validate_registry(self) -> None:
        roots: list[tuple[Mapping[str, Any], Path]] = []
        for stage in STAGES:
            for direction in ("input", "output"):
                schema, path = self.schema(stage, direction)
                roots.append((schema, path))
                if schema.get("$schema") != "https://json-schema.org/draft/2020-12/schema":
                    raise ContractError(f"{path}: Draft 2020-12 declaration is required")
                if not isinstance(schema.get("$id"), str):
                    raise ContractError(f"{path}: versioned $id is required")
        common_path = self.root / "common.schema.json"
        common = self.load(common_path)
        roots.append((common, common_path))
        if common.get("$schema") != "https://json-schema.org/draft/2020-12/schema":
            raise ContractError("common.schema.json must declare Draft 2020-12")
        visited: set[tuple[Path, str]] = set()
        for schema, path in roots:
            self._validate_ref_closure(schema, path, visited)

    def _validate_ref_closure(
        self,
        value: Any,
        base_path: Path,
        visited: set[tuple[Path, str]],
    ) -> None:
        if isinstance(value, Mapping):
            reference = value.get("$ref")
            if reference is not None:
                if not isinstance(reference, str):
                    raise ContractError("$ref must be a string")
                marker = (self._resolved_schema_path(base_path), reference)
                if marker not in visited:
                    visited.add(marker)
                    target, target_path = self.resolve_ref(reference, base_path)
                    self._validate_ref_closure(target, target_path, visited)
            for key, child in value.items():
                if key != "$ref":
                    self._validate_ref_closure(child, base_path, visited)
        elif isinstance(value, list):
            for child in value:
                self._validate_ref_closure(child, base_path, visited)

    def matching_branches(
        self,
        value: Any,
        schema: Mapping[str, Any],
        base_path: Path,
        keyword: str,
    ) -> list[Mapping[str, Any] | bool]:
        branches = schema.get(keyword, [])
        if not isinstance(branches, list):
            return []
        return [
            branch
            for branch in branches
            if isinstance(branch, (dict, bool))
            and not self._branch_errors(value, branch, base_path, "$")
        ]

    def identity_occurrences(
        self,
        value: Any,
        schema: Mapping[str, Any],
        base_path: Path,
    ) -> tuple[IdentityOccurrence, ...]:
        """Return identity meaning declared by ``x-canonical`` annotations.

        Semantic closure and canonicalization deliberately share this walker so
        neither subsystem can silently invent a second ID namespace model.
        """

        found: list[IdentityOccurrence] = []
        self._collect_identity_occurrences(value, schema, base_path, (), found)
        # ``allOf`` composition may expose the same annotation through more
        # than one equivalent route.  Exact duplicates are harmless; distinct
        # paths remain distinct so duplicate definitions fail closed later.
        unique = {
            (item.kind, item.namespace, item.value, item.path): item for item in found
        }
        return tuple(
            unique[key]
            for key in sorted(
                unique,
                key=lambda item: (
                    item[0],
                    item[1],
                    item[2],
                    tuple(str(part) for part in item[3]),
                ),
            )
        )

    def _collect_identity_occurrences(
        self,
        value: Any,
        raw_schema: Mapping[str, Any] | bool,
        base_path: Path,
        path: tuple[str | int, ...],
        found: list[IdentityOccurrence],
    ) -> None:
        if not isinstance(raw_schema, Mapping):
            return
        schema, schema_path = self.dereference(raw_schema, base_path)
        for branch in schema.get("allOf", []):
            self._collect_identity_occurrences(
                value, branch, schema_path, path, found
            )
        if "if" in schema:
            condition_matches = not self._branch_errors(
                value,
                schema["if"],
                schema_path,
                "$",
            )
            selected = "then" if condition_matches else "else"
            if selected in schema:
                self._collect_identity_occurrences(
                    value,
                    schema[selected],
                    schema_path,
                    path,
                    found,
                )
        for keyword in ("oneOf", "anyOf"):
            matches = self.matching_branches(value, schema, schema_path, keyword)
            if keyword == "oneOf" and len(matches) > 1:
                raise ContractError(
                    "identity annotation walker encountered an ambiguous oneOf"
                )
            for branch in matches:
                self._collect_identity_occurrences(
                    value, branch, schema_path, path, found
                )

        annotation = schema.get("x-canonical", {})
        if isinstance(annotation, Mapping) and isinstance(value, str) and path:
            mode = annotation.get("mode")
            if mode in {"identity-definition", "identity-reference"}:
                namespace = annotation.get("namespace", "entity")
                if not isinstance(namespace, str) or not namespace:
                    raise ContractError(
                        f"identity annotation at {path!r} has no namespace"
                    )
                found.append(
                    IdentityOccurrence(str(mode), namespace, value, path)
                )

        if isinstance(value, Mapping):
            properties = schema.get("properties", {})
            pattern_properties = schema.get("patternProperties", {})
            for key, child in value.items():
                child_schema = properties.get(key)
                if child_schema is None:
                    for pattern, candidate in pattern_properties.items():
                        if re.search(pattern, str(key)):
                            child_schema = candidate
                            break
                if child_schema is None:
                    additional = schema.get("additionalProperties")
                    if isinstance(additional, (dict, bool)) and additional is not False:
                        child_schema = additional
                if child_schema is not None:
                    self._collect_identity_occurrences(
                        child,
                        child_schema,
                        schema_path,
                        path + (str(key),),
                        found,
                    )
        elif isinstance(value, list):
            item_schema = schema.get("items", True)
            prefix_items = schema.get("prefixItems", [])
            for index, child in enumerate(value):
                child_schema = (
                    prefix_items[index]
                    if index < len(prefix_items)
                    else item_schema
                )
                self._collect_identity_occurrences(
                    child,
                    child_schema,
                    schema_path,
                    path + (index,),
                    found,
                )

    def _validate(
        self,
        value: Any,
        raw_schema: Mapping[str, Any] | bool,
        base_path: Path,
        location: str,
        errors: list[str],
    ) -> None:
        if raw_schema is True:
            return
        if raw_schema is False:
            errors.append(f"{location}: rejected by false schema")
            return
        schema, schema_path = self.dereference(raw_schema, base_path)

        if "if" in schema:
            condition_matches = not self._branch_errors(
                value,
                schema["if"],
                schema_path,
                location,
            )
            selected = "then" if condition_matches else "else"
            if selected in schema:
                self._validate(
                    value,
                    schema[selected],
                    schema_path,
                    location,
                    errors,
                )
        if "allOf" in schema:
            for subschema in schema["allOf"]:
                self._validate(value, subschema, schema_path, location, errors)
        if "anyOf" in schema:
            candidates = [self._branch_errors(value, branch, schema_path, location) for branch in schema["anyOf"]]
            if not any(not candidate for candidate in candidates):
                errors.append(f"{location}: does not satisfy anyOf")
        if "oneOf" in schema:
            candidates = [self._branch_errors(value, branch, schema_path, location) for branch in schema["oneOf"]]
            if sum(not candidate for candidate in candidates) != 1:
                errors.append(f"{location}: must satisfy exactly one oneOf branch")
        if "not" in schema and not self._branch_errors(value, schema["not"], schema_path, location):
            errors.append(f"{location}: satisfies forbidden schema")

        if "const" in schema and value != schema["const"]:
            errors.append(f"{location}: expected constant {schema['const']!r}")
        if "enum" in schema and value not in schema["enum"]:
            errors.append(f"{location}: value {value!r} is not in enum")

        expected_type = schema.get("type")
        if expected_type is not None and not self._matches_type(value, expected_type):
            errors.append(f"{location}: expected type {expected_type!r}, got {type(value).__name__}")
            return

        if isinstance(value, Mapping):
            property_names = schema.get("propertyNames")
            if isinstance(property_names, (dict, bool)):
                for key in value:
                    self._validate(
                        str(key),
                        property_names,
                        schema_path,
                        f"{location}.<property:{key}>",
                        errors,
                    )
            required = schema.get("required", [])
            for key in required:
                if key not in value:
                    errors.append(f"{location}: missing required property {key!r}")
            for key, dependencies in schema.get("dependentRequired", {}).items():
                if key in value:
                    for dependency in dependencies:
                        if dependency not in value:
                            errors.append(
                                f"{location}: property {key!r} requires {dependency!r}"
                            )
            properties = schema.get("properties", {})
            pattern_properties = schema.get("patternProperties", {})
            for key, child in value.items():
                child_location = f"{location}.{key}"
                if key in properties:
                    self._validate(child, properties[key], schema_path, child_location, errors)
                    continue
                matches = [sub for pattern, sub in pattern_properties.items() if re.search(pattern, str(key))]
                if matches:
                    for subschema in matches:
                        self._validate(child, subschema, schema_path, child_location, errors)
                    continue
                additional = schema.get("additionalProperties", True)
                if additional is False:
                    errors.append(f"{child_location}: additional property is forbidden")
                elif isinstance(additional, (dict, bool)):
                    self._validate(child, additional, schema_path, child_location, errors)
            count = len(value)
            if count < schema.get("minProperties", 0):
                errors.append(f"{location}: too few properties")
            if "maxProperties" in schema and count > schema["maxProperties"]:
                errors.append(f"{location}: too many properties")

        if isinstance(value, list):
            if len(value) < schema.get("minItems", 0):
                errors.append(f"{location}: too few items")
            if "maxItems" in schema and len(value) > schema["maxItems"]:
                errors.append(f"{location}: too many items")
            if schema.get("uniqueItems"):
                encoded = [json.dumps(item, ensure_ascii=False, sort_keys=True, separators=(",", ":")) for item in value]
                if len(encoded) != len(set(encoded)):
                    errors.append(f"{location}: duplicate items")
            if "contains" in schema:
                matching_items = sum(
                    not self._branch_errors(
                        item,
                        schema["contains"],
                        schema_path,
                        f"{location}[{index}]",
                    )
                    for index, item in enumerate(value)
                )
                minimum = schema.get("minContains", 1)
                maximum = schema.get("maxContains")
                if matching_items < minimum:
                    errors.append(
                        f"{location}: contains matched {matching_items} items, below {minimum}"
                    )
                if maximum is not None and matching_items > maximum:
                    errors.append(
                        f"{location}: contains matched {matching_items} items, above {maximum}"
                    )
            prefix_items = schema.get("prefixItems", [])
            for index, item in enumerate(value):
                if index < len(prefix_items):
                    item_schema = prefix_items[index]
                else:
                    item_schema = schema.get("items", True)
                self._validate(item, item_schema, schema_path, f"{location}[{index}]", errors)

        if isinstance(value, str):
            if len(value) < schema.get("minLength", 0):
                errors.append(f"{location}: string is too short")
            if "maxLength" in schema and len(value) > schema["maxLength"]:
                errors.append(f"{location}: string is too long")
            if "pattern" in schema and re.search(schema["pattern"], value) is None:
                errors.append(f"{location}: does not match pattern {schema['pattern']!r}")
            if schema.get("format") == "sha256" and re.fullmatch(r"[0-9a-f]{64}", value) is None:
                errors.append(f"{location}: expected lowercase sha256")

        if isinstance(value, (int, float)) and not isinstance(value, bool):
            if "minimum" in schema and value < schema["minimum"]:
                errors.append(f"{location}: below minimum")
            if "maximum" in schema and value > schema["maximum"]:
                errors.append(f"{location}: above maximum")
            if "exclusiveMinimum" in schema and value <= schema["exclusiveMinimum"]:
                errors.append(f"{location}: below exclusive minimum")
            if "exclusiveMaximum" in schema and value >= schema["exclusiveMaximum"]:
                errors.append(f"{location}: above exclusive maximum")

    def _branch_errors(
        self, value: Any, schema: Mapping[str, Any] | bool, base_path: Path, location: str
    ) -> list[str]:
        branch: list[str] = []
        self._validate(value, schema, base_path, location, branch)
        return branch

    @staticmethod
    def _matches_type(value: Any, expected: str | Sequence[str]) -> bool:
        names = [expected] if isinstance(expected, str) else list(expected)
        checks = {
            "null": value is None,
            "boolean": isinstance(value, bool),
            "object": isinstance(value, Mapping),
            "array": isinstance(value, list),
            "string": isinstance(value, str),
            "integer": isinstance(value, int) and not isinstance(value, bool),
            "number": (
                isinstance(value, (int, float))
                and not isinstance(value, bool)
                and (not isinstance(value, float) or math.isfinite(value))
            ),
        }
        return any(checks.get(name, False) for name in names)


class SemanticTransitionSession:
    """Fail-closed Q→P wiring and reference-closure state for one arm.

    This layer validates transport/meaning preservation only.  It never
    produces a fact, cause, claim, admission, rank, or projection;
    those remain exclusively the responsibility of the invoked stage.
    """

    _OUTPUT_ECHO_FIELDS = (
        "schema_version",
        "stage",
        "sample_id",
        "atomic_cluster_id",
        "corpus_version",
        "split",
        "run_id",
        "stage_context",
        "seed",
        "input_hash",
        "upstream_artifact_hashes",
    )
    def __init__(self, registry: SchemaRegistry) -> None:
        self.registry = registry
        common = registry.load(registry.root / "common.schema.json")
        contract = common.get("x-semantic-transition-registry")
        if not isinstance(contract, Mapping):
            raise ContractError(
                "common schema has no semantic transition registry"
            )
        if contract.get("lineage_order") != list(STAGES):
            raise ContractError(
                "semantic transition registry lineage order is not frozen Q→P"
            )
        self._documents: dict[str, Mapping[str, Any]] = {}
        self._hashes: dict[str, str] = {}
        self._lineage_definitions: set[tuple[str, str]] = set()
        self._next_stage_index = 0
        self._pending_stage: str | None = None
        self._pending_input: Mapping[str, Any] | None = None
        self._pending_input_definitions: set[tuple[str, str]] = set()
        self._validated_output_stage: str | None = None

    def validate_input(self, document: Mapping[str, Any], stage: str) -> None:
        registered = self._require_next(stage)
        if self._pending_stage is not None:
            raise ContractError(
                f"{registered} input arrived before {self._pending_stage} was committed"
            )

        expected_hashes = sorted(set(self._hashes.values()))
        actual_hashes = document.get("upstream_artifact_hashes")
        if actual_hashes != expected_hashes:
            raise ContractError(
                f"{registered} input upstream hashes are not the exact committed lineage"
            )
        payload = self._payload(document, f"{registered} input")
        expected_input_hash = sha256_json(
            {
                "stage": registered,
                "payload": payload,
                "upstream_artifact_hashes": expected_hashes,
            }
        )
        if document.get("input_hash") != expected_input_hash:
            raise ContractError(
                f"{registered} input_hash is not bound to payload and upstream artifacts"
            )

        expected_payload = self._wired_payload(registered, payload)
        if expected_payload is not None and payload != expected_payload:
            raise ContractError(
                f"{registered} input payload is not the exact registered upstream wiring"
            )

        local_definitions = self._validate_reference_closure(
            document,
            registered,
            "input",
            inherited=self._lineage_definitions,
        )
        self._pending_stage = registered
        self._pending_input = document
        self._pending_input_definitions = local_definitions

    def validate_output(self, document: Mapping[str, Any], stage: str) -> None:
        registered = require_stage(stage)
        if self._pending_stage != registered or self._pending_input is None:
            raise ContractError(
                f"{registered} output has no validated input in this arm lineage"
            )
        input_document = self._pending_input
        for field in self._OUTPUT_ECHO_FIELDS:
            if document.get(field) != input_document.get(field):
                raise ContractError(
                    f"{registered} output does not exactly preserve input envelope field {field!r}"
                )

        inherited = self._lineage_definitions
        if registered == "P":
            # The public projection boundary is deliberately self-contained:
            # every reference must be available in the direct stage input,
            # not merely somewhere in an older hidden artifact.
            inherited = self._pending_input_definitions
        self._validate_reference_closure(
            document,
            registered,
            "output",
            inherited=inherited,
        )
        self._validate_stage_semantics(
            registered,
            self._payload(input_document, f"{registered} input"),
            self._payload(document, f"{registered} output"),
        )
        self._validated_output_stage = registered

    def commit(
        self,
        stage: str,
        consumed_document: Mapping[str, Any],
        consumed_hash: str,
    ) -> None:
        registered = require_stage(stage)
        if (
            self._pending_stage != registered
            or self._pending_input is None
            or self._validated_output_stage != registered
        ):
            raise ContractError(f"{registered} cannot commit without a validated transition")
        if sha256_json(consumed_document) != consumed_hash:
            raise ContractError(f"{registered} committed artifact hash does not match its bytes")
        # Canonical output is schema-validated by the runner before commit.
        # Recollecting its definitions makes the consumed (actual or oracle)
        # representation authoritative for the next stage.
        definitions, _ = self._identity_sets(
            consumed_document, registered, "output"
        )
        self._lineage_definitions.update(definitions)
        self._documents[registered] = consumed_document
        self._hashes[registered] = consumed_hash
        self._next_stage_index += 1
        self._pending_stage = None
        self._pending_input = None
        self._pending_input_definitions = set()
        self._validated_output_stage = None

    def _require_next(self, stage: str) -> str:
        registered = require_stage(stage)
        if self._next_stage_index >= len(STAGES):
            raise ContractError("semantic transition session is already complete")
        expected = STAGES[self._next_stage_index]
        if registered != expected:
            raise ContractError(
                f"semantic transition session expected {expected}, got {registered}"
            )
        return registered

    def _wired_payload(
        self,
        stage: str,
        current_payload: Mapping[str, Any],
    ) -> Mapping[str, Any] | None:
        if stage == "Q":
            return None

        def payload(name: str) -> Mapping[str, Any]:
            document = self._documents.get(name)
            if document is None:
                raise ContractError(f"{stage} is missing committed upstream stage {name}")
            return self._payload(document, f"committed {name} output")

        if stage == "F":
            return {"engine_evidence_pack": payload("Q")}
        if stage == "C":
            return {
                "candidate_lines": payload("Q")["candidate_lines"],
                "fact_bundle": payload("F"),
            }
        if stage == "Jp":
            return {"fact_bundle": payload("F"), "cause_bundle": payload("C")}
        if stage == "Ja":
            return {
                "fact_bundle": payload("F"),
                "cause_bundle": payload("C"),
                "proposal_bundle": payload("Jp"),
            }
        if stage == "R":
            return {"admission_bundle": payload("Ja")}
        if stage == "P":
            return {
                "candidate_lines": payload("Q")["candidate_lines"],
                "fact_bundle": payload("F"),
                "cause_bundle": payload("C"),
                "ranking_bundle": payload("R"),
            }
        raise ContractError(f"unknown stage: {stage}")

    def _identity_sets(
        self,
        document: Mapping[str, Any],
        stage: str,
        direction: str,
    ) -> tuple[set[tuple[str, str]], tuple[IdentityOccurrence, ...]]:
        schema, schema_path = self.registry.schema(stage, direction)
        occurrences = self.registry.identity_occurrences(
            document, schema, schema_path
        )
        definitions: dict[tuple[str, str], tuple[str | int, ...]] = {}
        for occurrence in occurrences:
            if occurrence.kind != "identity-definition":
                continue
            key = (occurrence.namespace, occurrence.value)
            previous = definitions.get(key)
            if previous is not None and previous != occurrence.path:
                raise ContractError(
                    f"{stage} {direction} duplicate namespace definition "
                    f"{occurrence.namespace}/{occurrence.value} at "
                    f"{self._display_path(previous)} and {self._display_path(occurrence.path)}"
                )
            definitions[key] = occurrence.path
        return set(definitions), occurrences

    def _validate_reference_closure(
        self,
        document: Mapping[str, Any],
        stage: str,
        direction: str,
        *,
        inherited: set[tuple[str, str]],
    ) -> set[tuple[str, str]]:
        definitions, occurrences = self._identity_sets(document, stage, direction)
        allowed = definitions | inherited
        dangling = [
            occurrence
            for occurrence in occurrences
            if occurrence.kind == "identity-reference"
            and (occurrence.namespace, occurrence.value) not in allowed
        ]
        if dangling:
            first = dangling[0]
            raise ContractError(
                f"{stage} {direction} dangling namespace reference "
                f"{first.namespace}/{first.value} at {self._display_path(first.path)}"
            )
        return definitions

    def _validate_stage_semantics(
        self,
        stage: str,
        input_payload: Mapping[str, Any],
        output_payload: Mapping[str, Any],
    ) -> None:
        if stage == "Q":
            for field in ("question", "acquisition_config"):
                if output_payload.get(field) != input_payload.get(field):
                    raise ContractError(f"Q output does not preserve input {field}")
            return
        if stage == "Ja":
            self._validate_admission_subset(input_payload, output_payload)
        elif stage == "R":
            self._validate_ranking_subset(input_payload, output_payload)
        elif stage == "P":
            self._validate_projection(input_payload, output_payload)

    @staticmethod
    def _claim_id(claim: Any, location: str) -> str:
        if not isinstance(claim, Mapping) or not isinstance(claim.get("claim_id"), str):
            raise ContractError(f"{location} has no claim_id")
        return str(claim["claim_id"])

    def _validate_admission_subset(
        self,
        input_payload: Mapping[str, Any],
        output_payload: Mapping[str, Any],
    ) -> None:
        proposal_bundle = input_payload.get("proposal_bundle")
        if not isinstance(proposal_bundle, Mapping):
            raise ContractError("Ja input proposal_bundle is absent")
        proposals = self._unique_by_id(
            proposal_bundle.get("proposals"), "claim_id", "Jp proposals"
        )
        decisions = output_payload.get("decisions")
        if not isinstance(decisions, list):
            raise ContractError("Ja decisions must be an array")
        seen: set[str] = set()
        for index, decision in enumerate(decisions):
            if not isinstance(decision, Mapping):
                raise ContractError(f"Ja decision {index} is not an object")
            claim = decision.get("claim")
            claim_id = self._claim_id(claim, f"Ja decision {index}")
            if claim_id in seen:
                raise ContractError(f"Ja contains duplicate decision for {claim_id}")
            seen.add(claim_id)
            if proposals.get(claim_id) != claim:
                raise ContractError(
                    f"Ja decision {claim_id} is not an unchanged Jp proposal"
                )

    def _validate_ranking_subset(
        self,
        input_payload: Mapping[str, Any],
        output_payload: Mapping[str, Any],
    ) -> None:
        admission_bundle = input_payload.get("admission_bundle")
        if not isinstance(admission_bundle, Mapping):
            raise ContractError("R input admission_bundle is absent")
        admitted: dict[str, Any] = {}
        for index, decision in enumerate(admission_bundle.get("decisions", [])):
            if not isinstance(decision, Mapping):
                raise ContractError(f"R input decision {index} is not an object")
            if decision.get("decision") != "admitted":
                continue
            claim = decision.get("claim")
            claim_id = self._claim_id(claim, f"R input admitted decision {index}")
            if claim_id in admitted:
                raise ContractError(f"R input has duplicate admitted claim {claim_id}")
            admitted[claim_id] = claim

        ranked = output_payload.get("ranked_claims")
        if not isinstance(ranked, list):
            raise ContractError("R ranked_claims must be an array")
        seen_claims: set[str] = set()
        seen_ranks: set[int] = set()
        for index, item in enumerate(ranked):
            if not isinstance(item, Mapping):
                raise ContractError(f"R ranked claim {index} is not an object")
            claim = item.get("claim")
            claim_id = self._claim_id(claim, f"R ranked claim {index}")
            rank = item.get("rank")
            if not isinstance(rank, int) or isinstance(rank, bool):
                raise ContractError(f"R ranked claim {claim_id} has invalid rank")
            if claim_id in seen_claims:
                raise ContractError(f"R ranks claim {claim_id} more than once")
            if rank in seen_ranks:
                raise ContractError(f"R rank {rank} is not unique")
            seen_claims.add(claim_id)
            seen_ranks.add(rank)
            if admitted.get(claim_id) != claim:
                raise ContractError(
                    f"R ranked claim {claim_id} is not an unchanged Ja admission"
                )

    def _validate_projection(
        self,
        input_payload: Mapping[str, Any],
        output_payload: Mapping[str, Any],
    ) -> None:
        fact_bundle = input_payload.get("fact_bundle")
        cause_bundle = input_payload.get("cause_bundle")
        ranking_bundle = input_payload.get("ranking_bundle")
        if not all(
            isinstance(value, Mapping)
            for value in (fact_bundle, cause_bundle, ranking_bundle)
        ):
            raise ContractError("P input direct context is incomplete")

        ranked = self._unique_nested_claims(
            ranking_bundle.get("ranked_claims"), "P input ranking"
        )
        public_claims = output_payload.get("public_claims")
        if not isinstance(public_claims, list):
            raise ContractError("P public_claims must be an array")
        seen_claims: set[str] = set()
        for index, public in enumerate(public_claims):
            if not isinstance(public, Mapping):
                raise ContractError(f"P public claim {index} is not an object")
            claim_id = self._claim_id(public, f"P public claim {index}")
            if claim_id in seen_claims:
                raise ContractError(f"P materializes claim {claim_id} more than once")
            seen_claims.add(claim_id)
            ranked_item = ranked.get(claim_id)
            if ranked_item is None:
                raise ContractError(f"P public claim {claim_id} was not ranked by R")
            projected = dict(public)
            projected_rank = projected.pop("rank", None)
            if projected_rank != ranked_item.get("rank") or projected != ranked_item.get("claim"):
                raise ContractError(
                    f"P public claim {claim_id} changes R's admitted meaning or rank"
                )

        facts = fact_bundle.get("facts")
        exact_facts = [
            item
            for item in facts if isinstance(item, Mapping) and item.get("fact_type") == "exact"
        ] if isinstance(facts, list) else []
        self._require_materialized_subset(
            output_payload.get("evidence_records"),
            fact_bundle.get("evidence_records"),
            "evidence_id",
            "P evidence",
        )
        self._require_materialized_subset(
            output_payload.get("relative_causes"),
            cause_bundle.get("causes"),
            "cause_id",
            "P cause",
        )
        self._require_materialized_subset(
            output_payload.get("bindings"),
            fact_bundle.get("bindings"),
            "binding_id",
            "P binding",
        )
        self._require_materialized_subset(
            output_payload.get("observed_lines"),
            input_payload.get("candidate_lines"),
            "line_id",
            "P observed line",
        )
        self._require_materialized_subset(
            output_payload.get("exact_results"),
            exact_facts,
            "fact_id",
            "P exact result",
        )

    @staticmethod
    def _unique_by_id(items: Any, field: str, location: str) -> dict[str, Any]:
        if not isinstance(items, list):
            raise ContractError(f"{location} must be an array")
        result: dict[str, Any] = {}
        for index, item in enumerate(items):
            if not isinstance(item, Mapping) or not isinstance(item.get(field), str):
                raise ContractError(f"{location}[{index}] has no {field}")
            identity = str(item[field])
            if identity in result:
                raise ContractError(f"{location} duplicates {field} {identity}")
            result[identity] = item
        return result

    def _unique_nested_claims(self, items: Any, location: str) -> dict[str, Mapping[str, Any]]:
        if not isinstance(items, list):
            raise ContractError(f"{location} must be an array")
        result: dict[str, Mapping[str, Any]] = {}
        for index, item in enumerate(items):
            if not isinstance(item, Mapping):
                raise ContractError(f"{location}[{index}] is not an object")
            claim_id = self._claim_id(item.get("claim"), f"{location}[{index}]")
            if claim_id in result:
                raise ContractError(f"{location} duplicates claim {claim_id}")
            result[claim_id] = item
        return result

    def _require_materialized_subset(
        self,
        materialized: Any,
        direct_source: Any,
        field: str,
        location: str,
    ) -> None:
        source = self._unique_by_id(direct_source, field, f"{location} source")
        selected = self._unique_by_id(materialized, field, location)
        for identity, item in selected.items():
            if source.get(identity) != item:
                raise ContractError(
                    f"{location} {identity} is not unchanged direct-input material"
                )

    @staticmethod
    def _payload(document: Mapping[str, Any], location: str) -> Mapping[str, Any]:
        payload = document.get("payload")
        if not isinstance(payload, Mapping):
            raise ContractError(f"{location} payload is absent")
        return payload

    @staticmethod
    def _display_path(path: tuple[str | int, ...]) -> str:
        result = "$"
        for part in path:
            result += f"[{part}]" if isinstance(part, int) else f".{part}"
        return result
