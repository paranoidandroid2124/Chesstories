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


def _movement_witness_uci(actor: Mapping[str, Any]) -> str | None:
    origin = actor.get("from")
    destination = actor.get("to")
    piece_before = actor.get("piece_before")
    piece_after = actor.get("piece_after")
    if not all(
        isinstance(value, str)
        for value in (origin, destination, piece_before, piece_after)
    ):
        return None
    move = origin + destination
    if piece_before != "pawn":
        return move if piece_after == piece_before else None
    if piece_after == "pawn":
        return move
    suffix = {
        "queen": "q",
        "rook": "r",
        "bishop": "b",
        "knight": "n",
    }.get(piece_after)
    return move + suffix if suffix else None


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
        if not errors:
            self._validate_public_proof_semantics(document, "$", errors)
        if errors:
            preview = "\n".join(f"- {error}" for error in errors[:30])
            suffix = f"\n- ... {len(errors) - 30} more" if len(errors) > 30 else ""
            raise ContractError(f"{label} schema validation failed:\n{preview}{suffix}")

    def _validate_public_proof_semantics(
        self,
        value: Any,
        location: str,
        errors: list[str],
    ) -> None:
        """Close public proof identifiers without claiming private ancestry."""

        if isinstance(value, Mapping):
            passed_pawn_result = value.get("passed_pawn_result_proof")
            if isinstance(passed_pawn_result, Mapping):
                self._validate_passed_pawn_result_proof_identifiers(
                    passed_pawn_result,
                    f"{location}.passed_pawn_result_proof",
                    errors,
                )
            resource = value.get("resource_differential_proof")
            if isinstance(resource, Mapping):
                self._validate_resource_proof_identifiers(
                    resource,
                    f"{location}.resource_differential_proof",
                    errors,
                )
            defense = value.get("defense_obligation_change_proof")
            if isinstance(defense, Mapping):
                self._validate_defense_obligation_change_proof_identifiers(
                    defense,
                    f"{location}.defense_obligation_change_proof",
                    errors,
                )
            for key, child in value.items():
                self._validate_public_proof_semantics(
                    child,
                    f"{location}.{key}",
                    errors,
                )
        elif isinstance(value, list):
            for index, child in enumerate(value):
                self._validate_public_proof_semantics(
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

    def _validate_passed_pawn_result_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        raw_branches = proof.get("branches")
        branches = self._unique_objects_by_id(
            raw_branches if isinstance(raw_branches, list) else [],
            "branch_id",
            f"{location}.branches",
            errors,
        )
        raw_paths = proof.get("proof_paths")
        paths = raw_paths if isinstance(raw_paths, list) else []
        self._unique_objects_by_id(
            paths,
            "path_occurrence_id",
            f"{location}.proof_paths",
            errors,
        )

        expected_branch_ids = {
            branch_id
            for branch_id, branch in branches.items()
            if branch.get("role") == "expected_result_route"
        }
        legal_reply_branches = {
            branch_id: branch
            for branch_id, branch in branches.items()
            if branch.get("role") == "legal_reply"
        }
        if len(expected_branch_ids) != 1:
            errors.append(
                f"{location}.branches: expected exactly one expected_result_route branch"
            )

        inventory = proof.get("closed_legal_reply_inventory")
        if isinstance(inventory, Mapping):
            bindings = inventory.get("branch_by_reply")
            if isinstance(bindings, list):
                binding_reply_moves: list[str] = []
                binding_branch_ids: list[str] = []
                for index, binding in enumerate(bindings):
                    if not isinstance(binding, Mapping):
                        continue
                    branch_id = binding.get("branch_id")
                    reply_move = binding.get("reply_move")
                    if isinstance(reply_move, str):
                        binding_reply_moves.append(reply_move)
                    if isinstance(branch_id, str):
                        binding_branch_ids.append(branch_id)
                    self._validate_branch_reference(
                        branch_id,
                        "legal_reply",
                        branches,
                        "role",
                        f"{location}.closed_legal_reply_inventory.branch_by_reply[{index}].branch_id",
                        errors,
                    )
                    branch = branches.get(branch_id) if isinstance(branch_id, str) else None
                    if branch is not None and branch.get("reply_move") != binding.get("reply_move"):
                        errors.append(
                            f"{location}.closed_legal_reply_inventory.branch_by_reply[{index}]: "
                            "reply_move does not match its branch"
                        )

                inventory_reply_moves = inventory.get("legal_reply_moves")
                if isinstance(inventory_reply_moves, list):
                    if len(binding_reply_moves) != len(set(binding_reply_moves)):
                        errors.append(
                            f"{location}.closed_legal_reply_inventory.branch_by_reply: "
                            "duplicate reply_move binding"
                        )
                    if len(binding_branch_ids) != len(set(binding_branch_ids)):
                        errors.append(
                            f"{location}.closed_legal_reply_inventory.branch_by_reply: "
                            "duplicate branch_id binding"
                        )
                    if set(binding_reply_moves) != set(inventory_reply_moves):
                        errors.append(
                            f"{location}.closed_legal_reply_inventory: legal_reply_moves and "
                            "branch_by_reply moves must match exactly"
                        )

                legal_reply_branch_ids = set(legal_reply_branches)
                if set(binding_branch_ids) != legal_reply_branch_ids:
                    errors.append(
                        f"{location}.closed_legal_reply_inventory.branch_by_reply: "
                        "branch ids must match the legal_reply branches exactly"
                    )
                branch_reply_moves = [
                    branch.get("reply_move")
                    for branch in legal_reply_branches.values()
                    if isinstance(branch.get("reply_move"), str)
                ]
                if len(branch_reply_moves) != len(set(branch_reply_moves)):
                    errors.append(
                        f"{location}.branches: duplicate legal_reply reply_move"
                    )
                if isinstance(inventory_reply_moves, list) and set(
                    branch_reply_moves
                ) != set(inventory_reply_moves):
                    errors.append(
                        f"{location}.closed_legal_reply_inventory.legal_reply_moves: "
                        "moves must match the legal_reply branches exactly"
                    )

        path_reply_branch_ids: list[str] = []
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            reply_branch_id = path.get("reply_branch_id")
            if isinstance(reply_branch_id, str):
                path_reply_branch_ids.append(reply_branch_id)
            self._validate_branch_reference(
                reply_branch_id,
                "legal_reply",
                branches,
                "role",
                f"{path_location}.reply_branch_id",
                errors,
            )
            premises = path.get("premises")
            if not isinstance(premises, list):
                continue
            for premise_index, premise in enumerate(premises):
                if not isinstance(premise, Mapping):
                    continue
                premise_location = f"{path_location}.premises[{premise_index}]"
                self._validate_branch_reference(
                    premise.get("branch_id"),
                    premise.get("branch_role"),
                    branches,
                    "role",
                    f"{premise_location}.branch_id",
                    errors,
                )
                related = premise.get("related_branch_ids")
                if isinstance(related, list):
                    for related_index, branch_id in enumerate(related):
                        self._validate_branch_reference(
                            branch_id,
                            None,
                            branches,
                            "role",
                            f"{premise_location}.related_branch_ids[{related_index}]",
                            errors,
                        )

        if set(path_reply_branch_ids) != set(legal_reply_branches):
            errors.append(
                f"{location}.proof_paths: reply_branch_id values must cover the "
                "legal_reply branches exactly"
            )

    def _validate_resource_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        paths = self._validate_two_branch_causal_proof_identifiers(
            proof,
            location,
            errors,
        )
        self._validate_resource_proof_field_closure(proof, paths, location, errors)

    def _validate_defense_obligation_change_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        paths = self._validate_two_branch_causal_proof_identifiers(
            proof,
            location,
            errors,
        )
        self._validate_defense_obligation_change_field_closure(
            proof,
            paths,
            location,
            errors,
        )

    def _validate_two_branch_causal_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> list[Any]:
        branch_values = [
            proof.get("counterfactual_reference_branch"),
            proof.get("played_root_branch"),
        ]
        branches = self._unique_objects_by_id(
            branch_values,
            "branch_id",
            f"{location}.branches",
            errors,
        )
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
            if isinstance(path, Mapping) and isinstance(path.get("path_occurrence_id"), str)
        ]
        if path_ids != sorted(path_ids):
            errors.append(
                f"{location}.proof_paths: path_occurrence_id values must be canonical"
            )
        absence_use_ids: list[str] = []
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            uses = []
            premises = path.get("premises")
            if isinstance(premises, list):
                uses.extend(("premises", index, item) for index, item in enumerate(premises))
            absences = path.get("closed_absence_uses")
            if isinstance(absences, list):
                uses.extend(
                    ("closed_absence_uses", index, item)
                    for index, item in enumerate(absences)
                )
            for collection, index, use in uses:
                if not isinstance(use, Mapping):
                    continue
                if collection == "premises":
                    source_ids = use.get("source_premise_ids")
                    if isinstance(source_ids, list) and source_ids != sorted(source_ids):
                        errors.append(
                            f"{path_location}.{collection}[{index}].source_premise_ids: "
                            "values must be canonical"
                        )
                else:
                    use_id = use.get("use_id")
                    if isinstance(use_id, str):
                        absence_use_ids.append(use_id)
                self._validate_branch_reference(
                    use.get("branch_id"),
                    use.get("branch_role"),
                    branches,
                    "branch_role",
                    f"{path_location}.{collection}[{index}].branch_id",
                    errors,
                )

        if len(absence_use_ids) != len(set(absence_use_ids)):
            errors.append(
                f"{location}.proof_paths: closed absence use_id values must be globally unique"
            )
        return paths

    @staticmethod
    def _validate_resource_proof_field_closure(
        proof: Mapping[str, Any],
        paths: list[Any],
        location: str,
        errors: list[str],
    ) -> None:
        """Bind the public projection to itself without replaying board facts."""

        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        participants = proof.get("participants")
        if not all(isinstance(value, Mapping) for value in (reference, played, participants)):
            return
        assert isinstance(reference, Mapping)
        assert isinstance(played, Mapping)
        assert isinstance(participants, Mapping)

        participant_names = (
            "trigger",
            "forced_reply",
            "realizer",
            "captured_target",
            "played_defense",
            "disabled_defender",
        )
        participant_values = {name: participants.get(name) for name in participant_names}
        if not all(isinstance(value, Mapping) for value in participant_values.values()):
            return
        trigger = participant_values["trigger"]
        forced_reply = participant_values["forced_reply"]
        realizer = participant_values["realizer"]
        target = participant_values["captured_target"]
        played_defense = participant_values["played_defense"]
        disabled = participant_values["disabled_defender"]
        assert all(
            isinstance(value, Mapping)
            for value in (trigger, forced_reply, realizer, target, played_defense, disabled)
        )

        def steps(branch: Mapping[str, Any]) -> list[Any]:
            value = branch.get("steps")
            return value if isinstance(value, list) else []

        def step(branch: Mapping[str, Any], index: int) -> Mapping[str, Any] | None:
            branch_steps = steps(branch)
            value = branch_steps[index] if index < len(branch_steps) else None
            return value if isinstance(value, Mapping) else None

        def require_equal(actual: Any, expected: Any, field: str) -> None:
            if actual != expected:
                errors.append(f"{location}.{field}: public proof fields do not identify the same occurrence")

        reference_steps = steps(reference)
        played_steps = steps(played)
        if len(reference_steps) < 3 or len(played_steps) < 2:
            return
        reference_root = step(reference, 0)
        reference_reply = step(reference, 1)
        reference_realizer = step(reference, 2)
        played_root = step(played, 0)
        played_reply = step(played, 1)
        if not all(
            value is not None
            for value in (reference_root, reference_reply, reference_realizer, played_root, played_reply)
        ):
            return
        assert reference_root is not None
        assert reference_reply is not None
        assert reference_realizer is not None
        assert played_root is not None
        assert played_reply is not None

        realizing_move = proof.get("realizing_move")
        defense_move = proof.get("played_root_branch_legal_defense_move")
        require_equal(reference.get("root_move"), reference_root.get("move_uci"), "counterfactual_reference_branch.root_move")
        require_equal(played.get("root_move"), played_root.get("move_uci"), "played_root_branch.root_move")
        require_equal(reference_root.get("move_uci"), _movement_witness_uci(trigger), "participants.trigger")
        require_equal(reference_reply.get("move_uci"), forced_reply.get("move_uci"), "participants.forced_reply.move_uci")
        require_equal(forced_reply.get("move_uci"), _movement_witness_uci(forced_reply), "participants.forced_reply")
        require_equal(reference_realizer.get("move_uci"), realizing_move, "realizing_move")
        require_equal(played_root.get("move_uci"), realizing_move, "played_root_branch.steps[0]")
        require_equal(realizing_move, _movement_witness_uci(realizer), "participants.realizer")
        require_equal(played_reply.get("move_uci"), defense_move, "played_root_branch.steps[1]")
        require_equal(played_defense.get("move_uci"), defense_move, "participants.played_defense.move_uci")
        require_equal(defense_move, _movement_witness_uci(played_defense), "participants.played_defense")
        require_equal(trigger.get("side"), realizer.get("side"), "participants.trigger.side")
        require_equal(forced_reply.get("side"), played_defense.get("side"), "participants.forced_reply.side")
        require_equal(forced_reply.get("side"), disabled.get("side"), "participants.disabled_defender.side")
        require_equal(forced_reply.get("side"), target.get("side"), "participants.captured_target.side")
        if trigger.get("side") == forced_reply.get("side"):
            errors.append(f"{location}.participants: trigger and reply must have opposing sides")
        require_equal(target.get("square"), realizer.get("to"), "participants.captured_target.square")
        require_equal(played_defense.get("to"), realizer.get("to"), "participants.played_defense.to")
        require_equal(disabled.get("side"), played_defense.get("side"), "participants.disabled_defender.side")
        require_equal(disabled.get("piece"), played_defense.get("piece_before"), "participants.disabled_defender.piece")
        require_equal(disabled.get("square"), played_defense.get("from"), "participants.disabled_defender.square")

        require_equal(disabled.get("piece"), forced_reply.get("piece_before"), "participants.forced_reply.piece_before")
        require_equal(disabled.get("square"), forced_reply.get("from"), "participants.forced_reply.from")

        expected_query = (
            f"legal-capture:{target.get('side')}:{realizer.get('to')}"
            if isinstance(target.get("side"), str) and isinstance(realizer.get("to"), str)
            else None
        )
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            absences = path.get("closed_absence_uses")
            if not isinstance(absences, list):
                continue
            for absence_index, absence in enumerate(absences):
                if not isinstance(absence, Mapping):
                    continue
                prefix = f"proof_paths[{path_index}].closed_absence_uses[{absence_index}]"
                require_equal(absence.get("query"), expected_query, f"{prefix}.query")
                require_equal(absence.get("branch_id"), reference.get("branch_id"), f"{prefix}.branch_id")
                position = absence.get("position")
                if isinstance(position, Mapping):
                    require_equal(position.get("fen"), reference_realizer.get("fen_after"), f"{prefix}.position.fen")
                    require_equal(position.get("ply"), reference_realizer.get("ply"), f"{prefix}.position.ply")

    @staticmethod
    def _validate_defense_obligation_change_field_closure(
        proof: Mapping[str, Any],
        paths: list[Any],
        location: str,
        errors: list[str],
    ) -> None:
        """Bind the defense proof's public fields without replaying chess."""

        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        participants = proof.get("participants")
        if not all(isinstance(value, Mapping) for value in (reference, played, participants)):
            return
        assert isinstance(reference, Mapping)
        assert isinstance(played, Mapping)
        assert isinstance(participants, Mapping)

        names = (
            "remover",
            "removed_defender",
            "removal_recapture",
            "later_exploit",
            "captured_target",
            "played_sole_recapture",
        )
        values = {name: participants.get(name) for name in names}
        if not all(isinstance(value, Mapping) for value in values.values()):
            return
        remover = values["remover"]
        removed = values["removed_defender"]
        removal_recapture = values["removal_recapture"]
        exploit = values["later_exploit"]
        target = values["captured_target"]
        played_recapture = values["played_sole_recapture"]
        assert all(
            isinstance(value, Mapping)
            for value in (remover, removed, removal_recapture, exploit, target, played_recapture)
        )

        def steps(branch: Mapping[str, Any]) -> list[Any]:
            value = branch.get("steps")
            return value if isinstance(value, list) else []

        def require_equal(actual: Any, expected: Any, field: str) -> None:
            if actual != expected:
                errors.append(
                    f"{location}.{field}: public proof fields do not identify the same occurrence"
                )

        reference_steps = steps(reference)
        played_steps = steps(played)
        if len(reference_steps) != 3 or len(played_steps) != 2:
            return
        if not all(isinstance(value, Mapping) for value in reference_steps + played_steps):
            return
        reference_root, reference_reply, reference_exploit = reference_steps
        played_root, played_reply = played_steps
        assert all(
            isinstance(value, Mapping)
            for value in (reference_root, reference_reply, reference_exploit, played_root, played_reply)
        )

        for branch_name, branch_steps in (
            ("counterfactual_reference_branch", reference_steps),
            ("played_root_branch", played_steps),
        ):
            require_equal(
                [step.get("step_index") for step in branch_steps],
                list(range(len(branch_steps))),
                f"{branch_name}.steps.step_index",
            )
            for index, (before, after) in enumerate(zip(branch_steps, branch_steps[1:])):
                require_equal(
                    after.get("fen_before"),
                    before.get("fen_after"),
                    f"{branch_name}.steps[{index + 1}].fen_before",
                )
                before_ply = before.get("ply")
                expected_ply = before_ply + 1 if isinstance(before_ply, int) else None
                require_equal(
                    after.get("ply"),
                    expected_ply,
                    f"{branch_name}.steps[{index + 1}].ply",
                )

        later_exploit_move = proof.get("later_exploit_move")
        sole_recapture_move = proof.get("played_sole_recapture_move")
        require_equal(reference.get("root_move"), reference_root.get("move_uci"), "counterfactual_reference_branch.root_move")
        require_equal(played.get("root_move"), played_root.get("move_uci"), "played_root_branch.root_move")
        require_equal(reference_root.get("fen_before"), played_root.get("fen_before"), "played_root_branch.steps[0].fen_before")
        require_equal(reference_root.get("ply"), played_root.get("ply"), "played_root_branch.steps[0].ply")
        require_equal(reference_root.get("move_uci"), _movement_witness_uci(remover), "participants.remover")
        require_equal(reference_reply.get("move_uci"), removal_recapture.get("move_uci"), "participants.removal_recapture.move_uci")
        require_equal(removal_recapture.get("move_uci"), _movement_witness_uci(removal_recapture), "participants.removal_recapture")
        require_equal(reference_exploit.get("move_uci"), later_exploit_move, "later_exploit_move")
        require_equal(played_root.get("move_uci"), later_exploit_move, "played_root_branch.steps[0]")
        require_equal(later_exploit_move, _movement_witness_uci(exploit), "participants.later_exploit")
        require_equal(played_reply.get("move_uci"), sole_recapture_move, "played_sole_recapture_move")
        require_equal(played_recapture.get("move_uci"), sole_recapture_move, "participants.played_sole_recapture.move_uci")
        require_equal(sole_recapture_move, _movement_witness_uci(played_recapture), "participants.played_sole_recapture")

        require_equal(remover.get("side"), exploit.get("side"), "participants.later_exploit.side")
        require_equal(removed.get("side"), removal_recapture.get("side"), "participants.removal_recapture.side")
        require_equal(removed.get("side"), target.get("side"), "participants.captured_target.side")
        require_equal(removed.get("side"), played_recapture.get("side"), "participants.played_sole_recapture.side")
        if remover.get("side") == removed.get("side"):
            errors.append(f"{location}.participants: remover and removed defender must have opposing sides")
        require_equal(remover.get("to"), removed.get("square"), "participants.remover.to")
        require_equal(removal_recapture.get("to"), remover.get("to"), "participants.removal_recapture.to")
        require_equal(exploit.get("to"), target.get("square"), "participants.captured_target.square")
        require_equal(played_recapture.get("to"), exploit.get("to"), "participants.played_sole_recapture.to")
        require_equal(played_recapture.get("from"), removed.get("square"), "participants.played_sole_recapture.from")
        require_equal(played_recapture.get("piece_before"), removed.get("piece"), "participants.played_sole_recapture.piece_before")

        reference_id = reference.get("branch_id")
        played_id = played.get("branch_id")
        expected_query = (
            f"legal-capture:{removed.get('side')}:{exploit.get('to')}"
            if isinstance(removed.get("side"), str) and isinstance(exploit.get("to"), str)
            else None
        )
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"proof_paths[{path_index}]"
            premises = path.get("premises")
            if isinstance(premises, list) and len(premises) == 3 and all(
                isinstance(premise, Mapping) for premise in premises
            ):
                require_equal(premises[0].get("branch_id"), reference_id, f"{path_location}.premises[0].branch_id")
                require_equal(premises[1].get("branch_id"), reference_id, f"{path_location}.premises[1].branch_id")
                require_equal(premises[2].get("branch_id"), played_id, f"{path_location}.premises[2].branch_id")
                result_ids = [premise.get("result_id") for premise in premises]
                if len(result_ids) != len(set(result_ids)):
                    errors.append(f"{location}.{path_location}.premises: result_id values must be distinct")
            absences = path.get("closed_absence_uses")
            if not isinstance(absences, list):
                continue
            for absence_index, absence in enumerate(absences):
                if not isinstance(absence, Mapping):
                    continue
                prefix = f"{path_location}.closed_absence_uses[{absence_index}]"
                require_equal(absence.get("query"), expected_query, f"{prefix}.query")
                require_equal(absence.get("branch_id"), reference_id, f"{prefix}.branch_id")
                position = absence.get("position")
                if isinstance(position, Mapping):
                    require_equal(position.get("fen"), reference_exploit.get("fen_after"), f"{prefix}.position.fen")
                    require_equal(position.get("ply"), reference_exploit.get("ply"), f"{prefix}.position.ply")

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
