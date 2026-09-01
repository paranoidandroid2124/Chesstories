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

_UCI_PROMOTION_PIECES = {"q": "queen", "r": "rook", "b": "bishop", "n": "knight"}


def _normalized_fen(value: Any) -> str | None:
    return " ".join(value.split()) if isinstance(value, str) and value.strip() else None


def _causal_step_key(step: Mapping[str, Any]) -> str | None:
    ply = step.get("ply")
    move = step.get("move_uci")
    before = _normalized_fen(step.get("fen_before"))
    after = _normalized_fen(step.get("fen_after"))
    if not isinstance(ply, int) or not isinstance(move, str) or before is None or after is None:
        return None
    return f"{ply}:{move.lower()}:{before}:{after}"


def _movement_matches_uci(movement: Any, move_uci: Any) -> bool:
    if not (
        isinstance(movement, Mapping)
        and isinstance(move_uci, str)
        and movement.get("from") == move_uci[:2]
        and movement.get("to") == move_uci[2:4]
    ):
        return False
    return (
        len(move_uci) == 5
        and movement.get("piece_before") == "pawn"
        and movement.get("piece_after") == _UCI_PROMOTION_PIECES.get(move_uci[4])
    ) or (
        len(move_uci) == 4
        and movement.get("piece_before") == movement.get("piece_after")
    )


def _movement_origin(movement: Mapping[str, Any]) -> tuple[Any, Any, Any]:
    return movement.get("side"), movement.get("piece_before"), movement.get("from")


def _colored_piece_identity(piece: Mapping[str, Any]) -> tuple[Any, Any, Any]:
    return piece.get("side"), piece.get("piece"), piece.get("square")

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
            if isinstance(value.get("primary"), Mapping) and isinstance(
                value.get("causal_explanations"), list
            ):
                self._validate_move_commentary_proof_transport(value, location, errors)
            passed_pawn_progress = value.get("passed_pawn_progress_realized_after_only_legal_reply_proof")
            if isinstance(passed_pawn_progress, Mapping):
                self._validate_passed_pawn_progress_realized_after_only_legal_reply_proof_identifiers(
                    passed_pawn_progress,
                    f"{location}.passed_pawn_progress_realized_after_only_legal_reply_proof",
                    errors,
                )
            resource = value.get("unique_check_reply_defender_displacement_before_capture_proof")
            if isinstance(resource, Mapping):
                self._validate_resource_proof_identifiers(
                    resource,
                    f"{location}.unique_check_reply_defender_displacement_before_capture_proof",
                    errors,
                )
            defense = value.get("sole_recapturer_removal_before_target_capture_proof")
            if isinstance(defense, Mapping):
                self._validate_sole_recapturer_removal_before_target_capture_proof_identifiers(
                    defense,
                    f"{location}.sole_recapturer_removal_before_target_capture_proof",
                    errors,
                )
            direct = value.get("vacated_gate_enables_unrecapturable_slider_capture_proof")
            if isinstance(direct, Mapping):
                self._validate_vacated_gate_enables_unrecapturable_slider_capture_proof_identifiers(
                    direct,
                    f"{location}.vacated_gate_enables_unrecapturable_slider_capture_proof",
                    errors,
                )
            route = value.get("square_release_route_proof")
            if isinstance(route, Mapping):
                self._validate_square_release_route_proof_identifiers(
                    route,
                    f"{location}.square_release_route_proof",
                    errors,
                )
            capture_order = value.get("capture_exclusion_move_order_proof")
            if isinstance(capture_order, Mapping):
                self._validate_capture_exclusion_move_order_proof_identifiers(
                    capture_order,
                    f"{location}.capture_exclusion_move_order_proof",
                    errors,
                )
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

    @staticmethod
    def _validate_move_commentary_proof_transport(
        commentary: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        primary = commentary.get("primary")
        explanations = commentary.get("causal_explanations")
        if not isinstance(primary, Mapping) or not isinstance(explanations, list):
            return
        seen_cause_evidence_ids: set[str] = set()
        seen_channel_ids: set[str] = set()
        for facet_index, facet in enumerate(explanations):
            if not isinstance(facet, Mapping):
                continue
            facet_location = f"{location}.causal_explanations[{facet_index}]"
            cause_evidence_id = facet.get("cause_evidence_id")
            if isinstance(cause_evidence_id, str):
                if cause_evidence_id in seen_cause_evidence_ids:
                    errors.append(
                        f"{facet_location}.cause_evidence_id: duplicate public Cause owner"
                    )
                seen_cause_evidence_ids.add(cause_evidence_id)
            expected_exposure = {
                "wrong_move_order": "primary",
                "missed_tactical_resource": "primary",
                "missed_square_release": "primary",
                "passed_pawn_progress": "complementary",
            }.get(facet.get("kind"))
            if expected_exposure is None or facet.get("exposure") != expected_exposure:
                errors.append(
                    f"{facet_location}.exposure: does not match Cause kind {facet.get('kind')!r}"
                )
            channels = facet.get("channels")
            if not isinstance(channels, list):
                continue
            for channel_index, channel in enumerate(channels):
                if not isinstance(channel, Mapping):
                    continue
                channel_location = f"{facet_location}.channels[{channel_index}]"
                channel_id = channel.get("channel_id")
                if isinstance(channel_id, str):
                    if channel_id in seen_channel_ids:
                        errors.append(
                            f"{channel_location}.channel_id: duplicate public proof occurrence"
                        )
                    seen_channel_ids.add(channel_id)

    def _validate_passed_pawn_progress_realized_after_only_legal_reply_proof_identifiers(
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
        self._validate_passed_pawn_progress_branch_routes(
            branches,
            location,
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

        self._validate_passed_pawn_progress_root_contract(
            proof,
            branches,
            location,
            errors,
        )

        inventory = proof.get("closed_legal_reply_inventory")
        if isinstance(inventory, Mapping):
            branch_id = inventory.get("analysis_continuation_branch_id")
            self._validate_branch_reference(
                branch_id,
                "played_root_analysis_continuation",
                branches,
                "role",
                f"{location}.closed_legal_reply_inventory.analysis_continuation_branch_id",
                errors,
            )
            branch = branches.get(branch_id) if isinstance(branch_id, str) else None
            if isinstance(branch, Mapping) and inventory.get("legal_reply_move") != branch.get("reply_move"):
                errors.append(
                    f"{location}.closed_legal_reply_inventory.legal_reply_move: does not match the analysis continuation"
                )

        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            analysis_continuation_branch_id = path.get("analysis_continuation_branch_id")
            self._validate_branch_reference(
                analysis_continuation_branch_id,
                "played_root_analysis_continuation",
                branches,
                "role",
                f"{path_location}.analysis_continuation_branch_id",
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
                dependency_proof = premise.get("dependency_proof")
                if isinstance(dependency_proof, Mapping):
                    self._validate_passed_pawn_dependency_state_route(
                        dependency_proof,
                        premise,
                        branches,
                        premise_location,
                        errors,
                    )
            self._validate_passed_pawn_progress_path_transport(
                proof,
                path,
                branches,
                path_location,
                errors,
            )

    @staticmethod
    def _validate_passed_pawn_progress_root_contract(
        proof: Mapping[str, Any],
        branches: Mapping[str, Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        analysis_continuation = next(iter(branches.values()), None)
        raw_steps = analysis_continuation.get("steps") if isinstance(analysis_continuation, Mapping) else None
        continuation_steps = raw_steps if isinstance(raw_steps, list) else []
        if not continuation_steps or not all(isinstance(step, Mapping) for step in continuation_steps):
            return
        root = continuation_steps[0]
        result = continuation_steps[-1]
        assert isinstance(root, Mapping)
        assert isinstance(result, Mapping)

        root_move = proof.get("root_move")
        realizing_move = proof.get("realizing_move")
        root_ply = proof.get("root_ply")
        realizing_ply = proof.get("realizing_ply")
        if (
            root_move != root.get("move_uci")
            or root_ply != root.get("ply")
            or proof.get("root_line") != analysis_continuation.get("line")
        ):
            errors.append(f"{location}: root move, ply, and line do not identify one occurrence")
        if (
            realizing_move != result.get("move_uci")
            or realizing_ply != result.get("ply")
        ):
            errors.append(f"{location}: realizing move and ply do not identify the certified analysis occurrence")
        if (
            not isinstance(root_ply, int)
            or not isinstance(realizing_ply, int)
            or proof.get("result_ply_offset") != realizing_ply - root_ply
        ):
            errors.append(f"{location}.result_ply_offset: does not span root to realizing occurrence")

        inventory = proof.get("closed_legal_reply_inventory")
        root_after = inventory.get("root_after") if isinstance(inventory, Mapping) else None
        if (
            not isinstance(root_after, Mapping)
            or root_after.get("fen") != root.get("fen_after")
            or root_after.get("ply") != root.get("ply")
        ):
            errors.append(
                f"{location}.closed_legal_reply_inventory.root_after: closure does not certify the shared root result"
            )
        if (
            len(continuation_steps) < 2
            or not isinstance(inventory, Mapping)
            or inventory.get("legal_reply_move") != continuation_steps[1].get("move_uci")
        ):
            errors.append(
                f"{location}.closed_legal_reply_inventory.legal_reply_move: does not identify the first analysis reply"
            )

    @staticmethod
    def _validate_passed_pawn_progress_branch_routes(
        branches: Mapping[str, Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        for branch_id, branch in branches.items():
            branch_location = f"{location}.branches[{branch_id}]"
            raw_steps = branch.get("steps")
            steps = raw_steps if isinstance(raw_steps, list) else []
            if not steps or not all(isinstance(step, Mapping) for step in steps):
                continue
            typed_steps = [step for step in steps if isinstance(step, Mapping)]
            step_keys = [
                step.get("step_key")
                for step in typed_steps
                if isinstance(step.get("step_key"), str)
            ]
            if len(step_keys) != len(set(step_keys)):
                errors.append(f"{branch_location}.steps: duplicate step_key occurrence")

            branch_line = branch.get("line")
            root = typed_steps[0]
            if root.get("line") != branch_line:
                errors.append(f"{branch_location}.steps[0].line: root occurrence does not belong to the branch line")
            if isinstance(branch_line, Mapping) and root.get("move_uci") != branch_line.get("root_move"):
                errors.append(f"{branch_location}.steps[0].move_uci: root move does not match the branch line")

            observed_root = isinstance(branch_line, Mapping) and branch_line.get("line_role") == "played"
            expected_root_provenance = (
                "observed_game_root" if observed_root else "counterfactual_analyzed_root"
            )
            expected_step_provenance = (
                "observed_game_move" if observed_root else "certified_analysis_move"
            )
            if branch.get("root_provenance") != expected_root_provenance:
                errors.append(f"{branch_location}.root_provenance: does not match root-line ownership")
            if root.get("provenance") != expected_step_provenance:
                errors.append(f"{branch_location}.steps[0].provenance: does not match root occurrence ownership")

            for index, step in enumerate(typed_steps):
                step_location = f"{branch_location}.steps[{index}]"
                if step.get("step_index") != index:
                    errors.append(f"{step_location}.step_index: branch occurrences are not in exact order")
                if step.get("step_key") != _causal_step_key(step):
                    errors.append(f"{step_location}.step_key: does not bind the exact replay occurrence")
                if index == 0:
                    continue
                previous = typed_steps[index - 1]
                previous_ply = previous.get("ply")
                if not isinstance(previous_ply, int) or step.get("ply") != previous_ply + 1:
                    errors.append(f"{step_location}.ply: adjacent occurrences must advance by one ply")
                if step.get("fen_before") != previous.get("fen_after"):
                    errors.append(f"{step_location}.fen_before: adjacent occurrence route is discontinuous")
                if step.get("provenance") != "certified_analysis_move":
                    errors.append(f"{step_location}.provenance: continuation is not certified analysis")
                if step.get("line") != branch_line:
                    errors.append(f"{step_location}.line: analysis occurrences do not share their line owner")

            if branch.get("role") == "played_root_analysis_continuation" and len(typed_steps) > 1:
                reply_move = branch.get("reply_move")
                if typed_steps[1].get("move_uci") != reply_move:
                    errors.append(f"{branch_location}.reply_move: does not identify the first reply occurrence")

    @staticmethod
    def _validate_passed_pawn_progress_path_transport(
        proof: Mapping[str, Any],
        path: Mapping[str, Any],
        branches: Mapping[str, Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        analysis_continuation_branch_id = path.get("analysis_continuation_branch_id")
        analysis_continuation = (
            branches.get(analysis_continuation_branch_id)
            if isinstance(analysis_continuation_branch_id, str)
            else None
        )
        if not isinstance(analysis_continuation, Mapping):
            return
        raw_premises = path.get("premises")
        premises = raw_premises if isinstance(raw_premises, list) else []
        if not all(isinstance(premise, Mapping) for premise in premises):
            return
        steps_raw = analysis_continuation.get("steps")
        steps = steps_raw if isinstance(steps_raw, list) else []
        if not steps:
            return

        realization_move = path.get("realization_move")
        realization_ply = path.get("realization_ply")
        realization_indices = [
            index
            for index, step in enumerate(steps)
            if isinstance(step, Mapping)
            and step.get("move_uci") == realization_move
            and step.get("ply") == realization_ply
        ]
        if len(realization_indices) != 1:
            errors.append(f"{location}: realization_move and realization_ply must identify one analysis occurrence")
            return
        if realization_move != proof.get("realizing_move") or realization_ply != proof.get("realizing_ply"):
            errors.append(f"{location}: proof path does not terminate at the public realizing occurrence")
        for premise_index, premise in enumerate(premises):
            if not isinstance(premise, Mapping):
                continue
            from_index = premise.get("from_step_index")
            to_index = premise.get("to_step_index")
            if (
                premise.get("branch_id") != analysis_continuation_branch_id
                or premise.get("branch_role") != analysis_continuation.get("role")
                or not isinstance(from_index, int)
                or not isinstance(to_index, int)
                or not (0 <= from_index <= to_index < len(steps))
            ):
                errors.append(
                    f"{location}.premises[{premise_index}]: premise does not bind an occurrence range on its path branch"
                )

    @staticmethod
    def _validate_passed_pawn_dependency_state_route(
        dependency: Mapping[str, Any],
        premise: Mapping[str, Any],
        branches: Mapping[str, Mapping[str, Any]],
        location: str,
        errors: list[str],
    ) -> None:
        source_ids = premise.get("source_premise_ids")
        sources = (
            {value for value in source_ids if isinstance(value, str)}
            if isinstance(source_ids, list)
            else set()
        )
        raw_states = dependency.get("position_state_issuers")
        states = raw_states if isinstance(raw_states, list) else []
        for index, state in enumerate(states):
            if not isinstance(state, Mapping):
                continue
            retained_ids = (
                state.get("issuer_evidence_id"),
                state.get("issuer_occurrence_id"),
            )
            if any(not isinstance(value, str) or value not in sources for value in retained_ids):
                errors.append(
                    f"{location}.dependency_proof.position_state_issuers[{index}]: "
                    "state owner and occurrence must both be retained by source_premise_ids"
                )

        branch_id = premise.get("branch_id")
        branch = branches.get(branch_id) if isinstance(branch_id, str) else None
        raw_steps = branch.get("steps") if isinstance(branch, Mapping) else None
        steps = raw_steps if isinstance(raw_steps, list) else []
        from_index = premise.get("from_step_index")
        to_index = premise.get("to_step_index")
        if not isinstance(from_index, int) or not isinstance(to_index, int):
            return
        if not (0 <= from_index <= to_index < len(steps)):
            return

        raw_relations = dependency.get("relation_issuers")
        relations = raw_relations if isinstance(raw_relations, list) else []
        route_steps = steps[from_index : to_index + 1]
        route_steps_by_key = {
            step.get("step_key"): step
            for step in route_steps
            if isinstance(step, Mapping) and isinstance(step.get("step_key"), str)
        }
        branch_line = branch.get("line") if isinstance(branch, Mapping) else None
        for index, issuer in enumerate(relations):
            if not isinstance(issuer, Mapping):
                continue
            issuer_location = f"{location}.dependency_proof.relation_issuers[{index}]"
            retained_ids = [issuer.get("issuer_evidence_id"), issuer.get("occurrence_id")]
            lower_ids = issuer.get("source_premise_ids")
            if isinstance(lower_ids, list):
                retained_ids.extend(lower_ids)
            if any(not isinstance(value, str) or value not in sources for value in retained_ids):
                errors.append(
                    f"{issuer_location}: relation owner, occurrence, and lower sources "
                    "must all be retained by source_premise_ids"
                )
            relation_step = route_steps_by_key.get(issuer.get("step_key"))
            if not isinstance(relation_step, Mapping):
                errors.append(
                    f"{issuer_location}.step_key: relation issuer does not bind an occurrence on its branch route"
                )
            elif issuer.get("line") != branch_line or issuer.get("line") != relation_step.get("line"):
                errors.append(
                    f"{issuer_location}.line: relation issuer does not own its replay route"
                )
            if issuer.get("scope") != "played_line":
                errors.append(f"{issuer_location}.scope: relation issuer scope is not played_line")

        for index, state in enumerate(states):
            if not isinstance(state, Mapping):
                continue
            state_location = f"{location}.dependency_proof.position_state_issuers[{index}]"
            state_step = route_steps_by_key.get(state.get("step_key"))
            if not isinstance(state_step, Mapping) or any(
                state.get(field) != state_step.get(field)
                for field in (
                    "step_key",
                    "ply",
                    "move_uci",
                    "fen_before",
                    "fen_after",
                    "line",
                )
            ):
                errors.append(
                    f"{state_location}: state issuer does not equal its exact branch occurrence"
                )
            state_line = state.get("line")
            if state.get("scope") != "played_line":
                errors.append(f"{state_location}.scope: state issuer scope is not played_line")
            if state_line != branch_line:
                errors.append(
                    f"{state_location}.line: state issuer does not own the dependency route"
                )

    def _validate_vacated_gate_enables_unrecapturable_slider_capture_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        self._validate_two_branch_causal_proof_identifiers(proof, location, errors)
        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        participants = proof.get("participants")
        paths = proof.get("proof_paths")
        if not (
            isinstance(reference, Mapping)
            and isinstance(played, Mapping)
            and isinstance(participants, Mapping)
            and isinstance(paths, list)
            and isinstance(reference.get("steps"), list)
            and isinstance(played.get("steps"), list)
        ):
            return
        reference_steps = reference.get("steps")
        played_steps = played.get("steps")
        enabler = participants.get("enabler")
        slider = participants.get("slider")
        gate_blocker = participants.get("gate_blocker")
        exploit = participants.get("exploit")
        captured_target = participants.get("captured_target")
        if not (
            len(reference_steps) >= 3
            and all(isinstance(step, Mapping) for step in reference_steps)
            and all(isinstance(step, Mapping) for step in played_steps)
            and all(
                isinstance(participant, Mapping)
                for participant in (enabler, slider, gate_blocker, exploit, captured_target)
            )
        ):
            return
        exploit_index = len(reference_steps) - 1
        exploit_move = proof.get("exploit_move")
        if (
            len(played_steps) != exploit_index
            or any(
                reference_steps[index].get("move_uci")
                != played_steps[index].get("move_uci")
                for index in range(1, exploit_index)
            )
            or reference_steps[exploit_index].get("move_uci") != exploit_move
            or not _movement_matches_uci(enabler, reference_steps[0].get("move_uci"))
            or not _movement_matches_uci(exploit, exploit_move)
            or _colored_piece_identity(gate_blocker) != _movement_origin(enabler)
            or _colored_piece_identity(slider) != _movement_origin(exploit)
            or slider.get("piece") not in ("bishop", "rook", "queen")
            or exploit.get("piece_after") != slider.get("piece")
            or enabler.get("side") != slider.get("side")
            or exploit.get("side") != slider.get("side")
            or captured_target.get("square") != exploit.get("to")
            or captured_target.get("side") == slider.get("side")
        ):
            errors.append(
                f"{location}: participants and exploit do not bind the exact reference/Played occurrence"
            )

        reference_id = reference.get("branch_id")
        played_id = played.get("branch_id")
        slider_prefix = (
            f"slider-reach:{slider.get('side')}:{slider.get('piece')}@{slider.get('square')}:"
        )
        expected_absences = (
            (
                "reference_immediate_recapture_absent",
                reference_id,
                reference.get("branch_role"),
                exploit_index,
                f"legal-capture:{captured_target.get('side')}:{exploit.get('to')}",
            ),
            (
                "played_exploit_move_absent",
                played_id,
                played.get("branch_role"),
                exploit_index - 1,
                f"legal-move-from-to:{slider.get('side')}:{slider.get('square')}:{exploit.get('to')}",
            ),
            (
                "played_replacement_capture_absent",
                played_id,
                played.get("branch_role"),
                exploit_index - 1,
                f"legal-capture:{slider.get('side')}:{exploit.get('to')}",
            ),
        )
        target_query = (
            f"occupied-by:{captured_target.get('side')}:{captured_target.get('piece')}"
            f"@{captured_target.get('square')}"
        )
        played_queries = (
            (
                "played_slider_persistence",
                f"occupied-by:{slider.get('side')}:{slider.get('piece')}@{slider.get('square')}",
            ),
            ("played_target_persistence", target_query),
            (
                "played_gate_blocker_persistence",
                f"occupied-by:{gate_blocker.get('side')}:{gate_blocker.get('piece')}"
                f"@{gate_blocker.get('square')}",
            ),
        )
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            premises = path.get("premises")
            absences = path.get("closed_absence_uses")
            states = path.get("closed_state_uses")
            if (
                not isinstance(premises, list)
                or len(premises) != 2
                or not all(isinstance(premise, Mapping) for premise in premises)
                or not isinstance(absences, list)
                or not isinstance(states, list)
            ):
                continue
            if (
                premises[0].get("branch_id") != reference_id
                or premises[0].get("branch_role") != reference.get("branch_role")
                or premises[0].get("step_index") != 0
                or premises[1].get("branch_id") != reference_id
                or premises[1].get("branch_role") != reference.get("branch_role")
                or premises[1].get("step_index") != exploit_index
            ):
                errors.append(
                    f"{path_location}.premises: exact root and terminal exploit occurrences were lost"
                )
            if len(absences) != len(expected_absences) or any(
                not isinstance(use, Mapping)
                or use.get("role") != expected[0]
                or use.get("branch_id") != expected[1]
                or use.get("branch_role") != expected[2]
                or use.get("after_step_index") != expected[3]
                or use.get("query") != expected[4]
                for use, expected in zip(absences, expected_absences)
            ):
                errors.append(f"{path_location}.closed_absence_uses: exact sibling closure was lost")
            expected_states: list[tuple[str, Any, Any, int, str, bool]] = [
                (
                    "reference_intervening_slider_reach",
                    reference_id,
                    reference.get("branch_role"),
                    step_index,
                    slider_prefix,
                    True,
                )
                for step_index in range(1, exploit_index)
            ]
            expected_states.extend(
                (
                    "reference_target_persistence",
                    reference_id,
                    reference.get("branch_role"),
                    step_index,
                    target_query,
                    False,
                )
                for step_index in range(exploit_index)
            )
            expected_states.extend(
                (
                    role,
                    played_id,
                    played.get("branch_role"),
                    step_index,
                    query,
                    False,
                )
                for step_index in range(exploit_index)
                for role, query in played_queries
            )
            expected_states.append(
                (
                    "played_blocked_slider_reach",
                    played_id,
                    played.get("branch_role"),
                    exploit_index - 1,
                    slider_prefix,
                    True,
                )
            )
            if len(states) != len(expected_states) or any(
                not isinstance(state, Mapping)
                or state.get("role") != expected[0]
                or state.get("branch_id") != expected[1]
                or state.get("branch_role") != expected[2]
                or state.get("after_step_index") != expected[3]
                or not isinstance(state.get("query"), str)
                or (
                    not state.get("query").startswith(expected[4])
                    if expected[5]
                    else state.get("query") != expected[4]
                )
                for state, expected in zip(states, expected_states, strict=True)
            ):
                errors.append(f"{path_location}.closed_state_uses: exact state route was lost")

    def _validate_square_release_route_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        """Bind the typed route wire contract to its retained occurrences."""
        self._validate_two_branch_causal_proof_identifiers(proof, location, errors)
        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        participants = proof.get("participants")
        paths = proof.get("proof_paths")
        route = proof.get("route")
        terminal = proof.get("terminal")
        if not (
            isinstance(reference, Mapping)
            and isinstance(played, Mapping)
            and isinstance(participants, Mapping)
            and isinstance(paths, list)
            and isinstance(route, list)
            and route
            and all(isinstance(step, Mapping) for step in route)
            and isinstance(terminal, Mapping)
            and isinstance(reference.get("steps"), list)
            and isinstance(played.get("steps"), list)
            and isinstance(participants.get("releaser"), Mapping)
            and isinstance(participants.get("released_blocker"), Mapping)
            and isinstance(participants.get("route_piece"), Mapping)
        ):
            return
        reference_steps = reference.get("steps")
        played_steps = played.get("steps")
        releaser = participants.get("releaser")
        released_blocker = participants.get("released_blocker")
        route_piece = participants.get("route_piece")
        if not all(
            isinstance(step, Mapping) for step in reference_steps + played_steps
        ):
            return
        route_indices = [step.get("step_index") for step in route]
        if not (
            all(isinstance(index, int) for index in route_indices)
            and route_indices == sorted(set(route_indices))
            and route_indices[0] >= 2
        ):
            errors.append(f"{location}.route: route occurrences are not strictly ordered")
            return
        first_route_index = route_indices[0]
        terminal_index = route_indices[-1]
        terminal_kind = terminal.get("kind")
        terminal_state = terminal.get("terminal_state")
        needs_terminal_resource = terminal_kind in ("capture", "created_check")
        needs_reply = terminal_kind == "capture" or (
            terminal_kind == "created_check" and terminal_state == "ongoing"
        )
        reply_move = proof.get("terminal_reply_move")
        if len(played_steps) != first_route_index:
            errors.append(
                f"{location}: Played must end immediately before the first route leg"
            )
        if len(reference_steps) != terminal_index + 1 + int(needs_reply):
            errors.append(
                f"{location}: reference branch does not end at the terminal or its actual reply"
            )
        if (
            proof.get("terminal_step_index") != terminal_index
            or (needs_reply and not isinstance(reply_move, str))
            or (not needs_reply and reply_move is not None)
            or (
                needs_reply
                and terminal_index + 1 < len(reference_steps)
                and reply_move
                != reference_steps[terminal_index + 1].get("move_uci")
            )
        ):
            errors.append(
                f"{location}: terminal bounds do not match the typed terminal"
            )
        movement_fields = ("side", "from", "to", "piece_before", "piece_after")
        first_route = route[0]
        if (
            _colored_piece_identity(released_blocker) != _movement_origin(releaser)
            or _colored_piece_identity(route_piece) != _movement_origin(first_route)
            or first_route.get("side") != releaser.get("side")
            or first_route.get("to") != releaser.get("from")
            or not _movement_matches_uci(releaser, reference_steps[0].get("move_uci"))
            or any(
                not _movement_matches_uci(step, step.get("move_uci"))
                or step.get("move_uci")
                != reference_steps[index].get("move_uci")
                for step, index in zip(route, route_indices, strict=True)
                if 0 <= index < len(reference_steps)
            )
            or any(
                before.get("side") != after.get("side")
                or before.get("to") != after.get("from")
                or before.get("piece_after") != after.get("piece_before")
                for before, after in zip(route, route[1:])
            )
        ):
            errors.append(
                f"{location}.participants: release and ordered same-piece route identities diverge"
            )

        if terminal_kind == "occupation":
            if len(route) != 1 or needs_terminal_resource or needs_reply:
                errors.append(f"{location}.terminal: occupation must be the one-leg terminal")
        elif terminal_kind == "capture":
            captured = terminal.get("captured_target")
            geometric = terminal.get("geometric_recapturers")
            legal = terminal.get("legal_recaptures")
            restricted = terminal.get("restricted_recaptures")
            last_route = route[-1]
            captured_mover = {
                "side": last_route.get("side"),
                "piece": last_route.get("piece_after"),
                "square": last_route.get("to"),
            }
            if not (
                len(route) >= 2
                and isinstance(captured, Mapping)
                and captured.get("square") == last_route.get("to")
                and captured.get("side") != last_route.get("side")
                and isinstance(geometric, list)
                and isinstance(legal, list)
                and isinstance(restricted, list)
                and all(
                    isinstance(resource, Mapping)
                    and _movement_matches_uci(resource, resource.get("move_uci"))
                    and resource.get("side") == captured.get("side")
                    and resource.get("to") == last_route.get("to")
                    and resource.get("capture") == captured_mover
                    and any(
                        isinstance(piece, Mapping)
                        and piece.get("piece") == resource.get("piece_before")
                        and piece.get("square") == resource.get("from")
                        for piece in geometric
                    )
                    for resource in legal
                )
                and all(
                    isinstance(resource, Mapping)
                    and isinstance(resource.get("piece"), Mapping)
                    and resource.get("destination") == last_route.get("to")
                    and any(
                        isinstance(piece, Mapping)
                        and piece == resource.get("piece")
                        for piece in geometric
                    )
                    for resource in restricted
                )
            ):
                errors.append(
                    f"{location}.terminal: capture does not bind the final route movement and target"
                )
        elif terminal_kind == "created_check":
            responses = terminal.get("responses")
            if not (
                len(route) >= 2
                and terminal.get("checked_side") != route[-1].get("side")
                and isinstance(responses, list)
                and ((terminal_state == "checkmate") == (not responses))
                and all(
                    isinstance(response, Mapping)
                    and isinstance(response.get("resource"), Mapping)
                    and response["resource"].get("side")
                    == terminal.get("checked_side")
                    and _movement_matches_uci(
                        response["resource"], response["resource"].get("move_uci")
                    )
                    for response in responses
                )
            ):
                errors.append(
                    f"{location}.terminal: created-check inventory has inconsistent mover, responses, or terminal state"
                )

        expected_states: list[tuple[str, Any, Any, int, str]] = [
            (
                "reference_vacancy",
                reference.get("branch_id"),
                reference.get("branch_role"),
                step_index,
                f"vacant:{releaser.get('from')}",
            )
            for step_index in range(first_route_index)
        ]
        expected_states.extend(
            (
                f"reference_route_piece_{route_index}",
                reference.get("branch_id"),
                reference.get("branch_role"),
                step_index,
                f"occupied-by:{movement.get('side')}:{movement.get('piece_after')}@{movement.get('to')}",
            )
            for route_index, (movement, step_index) in enumerate(
                zip(route, route_indices, strict=True)
            )
        )
        expected_states.extend(
            (
                f"reference_route_persistence_{route_index}",
                reference.get("branch_id"),
                reference.get("branch_role"),
                step_index,
                f"occupied-by:{movement.get('side')}:{movement.get('piece_after')}@{movement.get('to')}",
            )
            for route_index, (movement, before_index, after_index) in enumerate(
                zip(route, route_indices, route_indices[1:])
            )
            for step_index in range(before_index + 1, after_index)
        )
        expected_states.extend(
            (
                "played_blocker_persistence",
                played.get("branch_id"),
                played.get("branch_role"),
                step_index,
                f"occupied-by:{released_blocker.get('side')}:{released_blocker.get('piece')}"
                f"@{released_blocker.get('square')}",
            )
            for step_index in range(first_route_index)
        )
        expected_states.extend(
            (
                "played_route_origin_persistence",
                played.get("branch_id"),
                played.get("branch_role"),
                step_index,
                f"occupied-by:{route_piece.get('side')}:{route_piece.get('piece')}"
                f"@{route_piece.get('square')}",
            )
            for step_index in range(first_route_index)
        )

        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            premises = path.get("premises")
            absences = path.get("closed_absence_uses")
            states = path.get("closed_state_uses")
            if not (
                isinstance(premises, list)
                and all(isinstance(premise, Mapping) for premise in premises)
                and isinstance(absences, list)
                and isinstance(states, list)
            ):
                continue

            terminal_uses = premises[:1] if needs_terminal_resource else []
            legal_uses = premises[1:] if needs_terminal_resource else premises
            expected_legal = [
                ("reference_release_move", 0, releaser, reference_steps[0].get("move_uci"))
            ]
            expected_legal.extend(
                (
                    f"reference_route_move_{route_index}",
                    step_index,
                    movement,
                    movement.get("move_uci"),
                )
                for route_index, (movement, step_index) in enumerate(
                    zip(route, route_indices, strict=True)
                )
            )
            if needs_reply and terminal_index + 1 < len(reference_steps):
                reply_step = reference_steps[terminal_index + 1]
                expected_legal.append(
                    (
                        "reference_terminal_reply",
                        terminal_index + 1,
                        None,
                        reply_step.get("move_uci"),
                    )
                )
            expected_contract = (
                "capture_recapture_inventory"
                if terminal_kind == "capture"
                else "created_check_response_inventory"
            )
            if needs_terminal_resource and (
                len(terminal_uses) != 1
                or terminal_uses[0].get("role") != "reference_terminal_resource"
                or terminal_uses[0].get("contract") != expected_contract
                or not isinstance(terminal_uses[0].get("result_id"), str)
                or not terminal_uses[0].get("result_id").startswith(
                    f"{expected_contract}:"
                )
                or terminal_uses[0].get("branch_id") != reference.get("branch_id")
                or terminal_uses[0].get("branch_role") != reference.get("branch_role")
                or terminal_uses[0].get("step_index") != terminal_index
            ):
                errors.append(
                    f"{path_location}.premises: exact terminal L1 occurrence was lost"
                )
            if len(legal_uses) != len(expected_legal):
                errors.append(
                    f"{path_location}.premises: ordered release, route, and reply occurrences were lost"
                )
                continue
            for premise, expected in zip(legal_uses, expected_legal, strict=True):
                movement = premise.get("movement")
                if (
                    premise.get("role") != expected[0]
                    or premise.get("branch_id") != reference.get("branch_id")
                    or premise.get("branch_role") != reference.get("branch_role")
                    or premise.get("step_index") != expected[1]
                    or premise.get("move_uci") != expected[3]
                    or not isinstance(movement, Mapping)
                    or (
                        expected[2] is not None
                        and any(
                            movement.get(field) != expected[2].get(field)
                            for field in movement_fields
                        )
                    )
                ):
                    errors.append(
                        f"{path_location}.premises: legal movement does not equal its exact reference occurrence"
                    )

            first_route_use = legal_uses[1]
            if "capture" in first_route_use:
                errors.append(
                    f"{path_location}.premises: the first occupation leg cannot be a capture"
                )

            terminal_route_use = legal_uses[len(route)]
            if terminal_kind == "capture":
                captured = terminal.get("captured_target")
                if terminal_route_use.get("capture") != captured:
                    errors.append(
                        f"{path_location}.premises: capture terminal lost its exact captured target"
                    )
            elif terminal_kind == "created_check" and needs_reply:
                reply_use = legal_uses[-1]
                reply_resource = {
                    **(
                        dict(reply_use.get("movement"))
                        if isinstance(reply_use.get("movement"), Mapping)
                        else {}
                    ),
                    "move_uci": reply_use.get("move_uci"),
                }
                if isinstance(reply_use.get("capture"), Mapping):
                    reply_resource["capture"] = dict(reply_use.get("capture"))
                responses = terminal.get("responses")
                if not isinstance(responses, list) or not any(
                    isinstance(response, Mapping)
                    and response.get("resource") == reply_resource
                    for response in responses
                ):
                    errors.append(
                        f"{path_location}.premises: actual check reply is absent from the exact response inventory"
                    )
            if needs_reply:
                reply_side = legal_uses[-1].get("movement", {}).get("side")
                expected_reply_side = (
                    terminal.get("captured_target", {}).get("side")
                    if terminal_kind == "capture"
                    else terminal.get("checked_side")
                )
                if reply_side != expected_reply_side:
                    errors.append(
                        f"{path_location}.premises: actual terminal reply has the wrong side identity"
                    )

            expected_absence = (
                "played_first_route_leg_absent",
                played.get("branch_id"),
                played.get("branch_role"),
                first_route_index - 1,
                f"legal-move-from-to:{first_route.get('side')}:{first_route.get('from')}:{first_route.get('to')}",
            )
            if len(absences) != 1 or any(
                not isinstance(use, Mapping)
                or use.get("role") != expected_absence[0]
                or use.get("branch_id") != expected_absence[1]
                or use.get("branch_role") != expected_absence[2]
                or use.get("after_step_index") != expected_absence[3]
                or use.get("query") != expected_absence[4]
                for use in absences
            ):
                errors.append(
                    f"{path_location}.closed_absence_uses: sibling first-leg absence was lost"
                )
            if len(states) != len(expected_states):
                errors.append(
                    f"{path_location}.closed_state_uses: exact route persistence inventory was lost"
                )
                continue
            if any(
                not isinstance(state, Mapping)
                or state.get("role") != expected[0]
                or state.get("branch_id") != expected[1]
                or state.get("branch_role") != expected[2]
                or state.get("after_step_index") != expected[3]
                or state.get("query") != expected[4]
                for state, expected in zip(states, expected_states, strict=True)
            ):
                errors.append(
                    f"{path_location}.closed_state_uses: state does not bind its exact route occurrence"
                )

    def _validate_resource_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        self._validate_two_branch_causal_proof_identifiers(proof, location, errors)
        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        participants = proof.get("participants")
        paths = proof.get("proof_paths")
        if not (
            isinstance(reference, Mapping)
            and isinstance(played, Mapping)
            and isinstance(participants, Mapping)
            and isinstance(paths, list)
            and isinstance(reference.get("steps"), list)
            and isinstance(played.get("steps"), list)
        ):
            return
        reference_steps = reference.get("steps")
        played_steps = played.get("steps")
        trigger = participants.get("trigger")
        forced_reply = participants.get("forced_reply")
        realizer = participants.get("realizer")
        captured_target = participants.get("captured_target")
        played_defense = participants.get("played_defense")
        disabled_defender = participants.get("disabled_defender")
        if not (
            len(reference_steps) == 3
            and len(played_steps) == 2
            and all(isinstance(step, Mapping) for step in reference_steps + played_steps)
            and all(
                isinstance(participant, Mapping)
                for participant in (
                    trigger,
                    forced_reply,
                    realizer,
                    captured_target,
                    played_defense,
                    disabled_defender,
                )
            )
        ):
            return
        realizing_move = proof.get("realizing_move")
        defense_move = proof.get("played_root_branch_legal_defense_move")
        if (
            reference_steps[2].get("move_uci") != realizing_move
            or played_steps[0].get("move_uci") != realizing_move
            or forced_reply.get("move_uci") != reference_steps[1].get("move_uci")
            or played_defense.get("move_uci") != defense_move
            or played_steps[1].get("move_uci") != defense_move
            or not _movement_matches_uci(trigger, reference_steps[0].get("move_uci"))
            or not _movement_matches_uci(forced_reply, forced_reply.get("move_uci"))
            or not _movement_matches_uci(realizer, realizing_move)
            or not _movement_matches_uci(played_defense, defense_move)
            or _colored_piece_identity(disabled_defender) != _movement_origin(forced_reply)
            or _colored_piece_identity(disabled_defender) != _movement_origin(played_defense)
            or captured_target.get("square") != realizer.get("to")
            or captured_target.get("side") != disabled_defender.get("side")
            or played_defense.get("to") != realizer.get("to")
            or trigger.get("side") != realizer.get("side")
            or trigger.get("side") == disabled_defender.get("side")
        ):
            errors.append(
                f"{location}: participants and moves do not bind the exact check-displacement occurrences"
            )
        expected_query = (
            f"legal-capture:{captured_target.get('side')}:{realizer.get('to')}"
        )
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            absences = path.get("closed_absence_uses")
            if (
                isinstance(absences, list)
                and len(absences) == 1
                and isinstance(absences[0], Mapping)
                and absences[0].get("query") != expected_query
            ):
                errors.append(
                    f"{location}.proof_paths[{path_index}].closed_absence_uses[0].query: does not close the realizer destination"
                )

    def _validate_sole_recapturer_removal_before_target_capture_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        self._validate_two_branch_causal_proof_identifiers(proof, location, errors)
        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        participants = proof.get("participants")
        paths = proof.get("proof_paths")
        if not (
            isinstance(reference, Mapping)
            and isinstance(played, Mapping)
            and isinstance(participants, Mapping)
            and isinstance(paths, list)
            and isinstance(reference.get("steps"), list)
            and isinstance(played.get("steps"), list)
        ):
            return
        reference_steps = reference.get("steps")
        played_steps = played.get("steps")
        remover = participants.get("remover")
        removed_defender = participants.get("removed_defender")
        removal_recapture = participants.get("removal_recapture")
        later_exploit = participants.get("later_exploit")
        captured_target = participants.get("captured_target")
        played_recapture = participants.get("played_sole_recapture")
        if not (
            len(reference_steps) == 3
            and len(played_steps) == 2
            and all(isinstance(step, Mapping) for step in reference_steps + played_steps)
            and all(
                isinstance(participant, Mapping)
                for participant in (
                    remover,
                    removed_defender,
                    removal_recapture,
                    later_exploit,
                    captured_target,
                    played_recapture,
                )
            )
        ):
            return
        exploit_move = proof.get("later_exploit_move")
        played_recapture_move = proof.get("played_sole_recapture_move")
        if (
            reference_steps[2].get("move_uci") != exploit_move
            or played_steps[0].get("move_uci") != exploit_move
            or removal_recapture.get("move_uci") != reference_steps[1].get("move_uci")
            or played_recapture.get("move_uci") != played_recapture_move
            or played_steps[1].get("move_uci") != played_recapture_move
            or not _movement_matches_uci(remover, reference_steps[0].get("move_uci"))
            or not _movement_matches_uci(removal_recapture, removal_recapture.get("move_uci"))
            or not _movement_matches_uci(later_exploit, exploit_move)
            or not _movement_matches_uci(played_recapture, played_recapture_move)
            or remover.get("to") != removed_defender.get("square")
            or remover.get("side") == removed_defender.get("side")
            or removal_recapture.get("side") != removed_defender.get("side")
            or removal_recapture.get("to") != remover.get("to")
            or later_exploit.get("side") != remover.get("side")
            or captured_target.get("side") != removed_defender.get("side")
            or captured_target.get("square") != later_exploit.get("to")
            or _movement_origin(played_recapture) != _colored_piece_identity(removed_defender)
            or played_recapture.get("to") != later_exploit.get("to")
        ):
            errors.append(
                f"{location}: participants and moves do not bind the exact removal/exploit occurrences"
            )
        expected_query = (
            f"legal-capture:{removed_defender.get('side')}:{later_exploit.get('to')}"
        )
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            absences = path.get("closed_absence_uses")
            if (
                isinstance(absences, list)
                and len(absences) == 1
                and isinstance(absences[0], Mapping)
                and absences[0].get("query") != expected_query
            ):
                errors.append(
                    f"{location}.proof_paths[{path_index}].closed_absence_uses[0].query: does not close the exploit destination"
                )

    def _validate_capture_exclusion_move_order_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
        """Bind the capture-target vacancy claim to all retained occurrences."""
        self._validate_two_branch_causal_proof_identifiers(proof, location, errors)
        reference = proof.get("counterfactual_reference_branch")
        played = proof.get("played_root_branch")
        paths = proof.get("proof_paths")
        if not (
            isinstance(reference, Mapping)
            and isinstance(played, Mapping)
            and isinstance(reference.get("steps"), list)
            and isinstance(played.get("steps"), list)
            and isinstance(paths, list)
        ):
            return
        reference_steps = reference.get("steps")
        played_steps = played.get("steps")
        if not (
            len(reference_steps) >= 3
            and len(reference_steps) % 2 == 1
            and len(played_steps) == 2
            and all(isinstance(step, Mapping) for step in reference_steps + played_steps)
        ):
            errors.append(f"{location}: branch bounds do not retain A...B and B-R")
            return

        reference_id = reference.get("branch_id")
        played_id = played.get("branch_id")
        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            premises = path.get("premises")
            absences = path.get("closed_absence_uses")
            states = path.get("closed_state_uses")
            if not (
                isinstance(premises, list)
                and len(premises) == 4
                and all(isinstance(premise, Mapping) for premise in premises)
                and isinstance(absences, list)
                and isinstance(states, list)
            ):
                continue
            vacating, played_deferred, played_reply, reference_deferred = premises
            deferred_index = reference_deferred.get("step_index")
            expected_coordinates = (
                ("reference_vacating_move", reference_id, reference.get("branch_role"), 0),
                ("played_deferred_move", played_id, played.get("branch_role"), 0),
                ("played_capture_reply", played_id, played.get("branch_role"), 1),
                (
                    "reference_deferred_move",
                    reference_id,
                    reference.get("branch_role"),
                    deferred_index,
                ),
            )
            if any(
                premise.get("role") != expected[0]
                or premise.get("branch_id") != expected[1]
                or premise.get("branch_role") != expected[2]
                or premise.get("step_index") != expected[3]
                for premise, expected in zip(premises, expected_coordinates, strict=True)
            ):
                errors.append(f"{path_location}.premises: exact A/B/R/later-B coordinates were lost")
                continue
            if not (
                isinstance(deferred_index, int)
                and deferred_index >= 2
                and deferred_index % 2 == 0
                and deferred_index == len(reference_steps) - 1
                and all(
                    isinstance(premise.get("move_uci"), str)
                    and _movement_matches_uci(premise.get("movement"), premise.get("move_uci"))
                    for premise in premises
                )
                and vacating.get("move_uci") == reference_steps[0].get("move_uci")
                and played_deferred.get("move_uci") == played_steps[0].get("move_uci")
                and played_reply.get("move_uci") == played_steps[1].get("move_uci")
                and reference_deferred.get("move_uci")
                == reference_steps[deferred_index].get("move_uci")
            ):
                errors.append(f"{path_location}.premises: legal moves do not own their branch steps")
                continue

            vacating_move = vacating.get("movement")
            deferred_move = played_deferred.get("movement")
            reply_move = played_reply.get("movement")
            reply_capture = played_reply.get("capture")
            if not all(
                isinstance(value, Mapping)
                for value in (vacating_move, deferred_move, reply_move, reply_capture)
            ):
                continue
            same_deferred = all(
                played_deferred.get(field) == reference_deferred.get(field)
                for field in (
                    "move_uci",
                    "movement",
                    "movement_mode",
                    "capture",
                    "legal_move_semantic_id",
                )
            )
            if not (
                same_deferred
                and reply_capture.get("square") == reply_move.get("to")
                and vacating_move.get("from") == reply_capture.get("square")
                and vacating_move.get("side") == reply_capture.get("side")
                and vacating_move.get("piece_before") == reply_capture.get("piece")
                and vacating_move.get("side") == deferred_move.get("side")
                and reply_move.get("side") != deferred_move.get("side")
            ):
                errors.append(f"{path_location}.premises: ordinary captured-target vacancy identity diverged")
                continue

            reply_query = (
                f"legal-move-from-to:{reply_move.get('side')}:{reply_move.get('from')}"
                f":{reply_move.get('to')}"
            )
            expected_absence_indices = [0, deferred_index]
            if len(absences) != len(expected_absence_indices) or any(
                not isinstance(use, Mapping)
                or use.get("role") != "reference_capture_reply_absent"
                or use.get("branch_id") != reference_id
                or use.get("branch_role") != reference.get("branch_role")
                or use.get("after_step_index") != index
                or use.get("query") != reply_query
                for use, index in zip(absences, expected_absence_indices, strict=True)
            ):
                errors.append(
                    f"{path_location}.closed_absence_uses: exact reply-absence endpoint pair was lost"
                )

            target_query = f"vacant:{reply_capture.get('square')}"
            reply_actor = (
                f"occupied-by:{reply_move.get('side')}:{reply_move.get('piece_before')}"
                f"@{reply_move.get('from')}"
            )
            deferred_actor = (
                f"occupied-by:{deferred_move.get('side')}:{deferred_move.get('piece_before')}"
                f"@{deferred_move.get('from')}"
            )
            expected_states: list[tuple[str, Any, Any, int, str]] = []
            for index in range(deferred_index + 1):
                expected_states.extend(
                    [
                        (
                            "reference_vacated_target",
                            reference_id,
                            reference.get("branch_role"),
                            index,
                            target_query,
                        ),
                        (
                            "reference_reply_actor",
                            reference_id,
                            reference.get("branch_role"),
                            index,
                            reply_actor,
                        ),
                    ]
                )
                if index < deferred_index:
                    expected_states.append(
                        (
                            "reference_deferred_actor",
                            reference_id,
                            reference.get("branch_role"),
                            index,
                            deferred_actor,
                        )
                    )
            if len(states) != len(expected_states) or any(
                not isinstance(state, Mapping)
                or state.get("role") != expected[0]
                or state.get("branch_id") != expected[1]
                or state.get("branch_role") != expected[2]
                or state.get("after_step_index") != expected[3]
                or state.get("query") != expected[4]
                for state, expected in zip(states, expected_states, strict=True)
            ):
                errors.append(f"{path_location}.closed_state_uses: exact actor/target interval was lost")

    def _validate_two_branch_causal_proof_identifiers(
        self,
        proof: Mapping[str, Any],
        location: str,
        errors: list[str],
    ) -> None:
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

        shared_root: tuple[str | None, Any] | None = None
        for branch_index, branch in enumerate(branch_values):
            if not isinstance(branch, Mapping):
                continue
            raw_steps = branch.get("steps")
            if not isinstance(raw_steps, list) or not all(
                isinstance(step, Mapping) for step in raw_steps
            ):
                continue
            steps = [step for step in raw_steps if isinstance(step, Mapping)]
            if not steps:
                continue
            branch_location = f"{location}.branches[{branch_index}]"
            root = steps[0]
            if branch.get("root_move") != root.get("move_uci"):
                errors.append(
                    f"{branch_location}.root_move: does not identify the first branch occurrence"
                )
            root_occurrence = (
                _normalized_fen(root.get("fen_before")),
                root.get("ply"),
            )
            if shared_root is None:
                shared_root = root_occurrence
            elif root_occurrence != shared_root:
                errors.append(
                    f"{branch_location}.steps[0]: branches do not share one root occurrence"
                )
            for step_index, step in enumerate(steps):
                step_location = f"{branch_location}.steps[{step_index}]"
                if step.get("step_index") != step_index:
                    errors.append(
                        f"{step_location}.step_index: branch occurrences are not in exact order"
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

        for path_index, path in enumerate(paths):
            if not isinstance(path, Mapping):
                continue
            path_location = f"{location}.proof_paths[{path_index}]"
            uses: list[tuple[str, int, Any]] = []
            premise_occurrence_routes: dict[str, tuple[Any, Any]] = {}
            path_closed_use_ids: list[str] = []
            for collection in (
                "premises",
                "closed_absence_uses",
                "closed_state_uses",
            ):
                values = path.get(collection)
                if isinstance(values, list):
                    uses.extend(
                        (collection, index, item)
                        for index, item in enumerate(values)
                    )

            result_ids: list[str] = []
            for collection, index, use in uses:
                if not isinstance(use, Mapping):
                    continue
                use_location = f"{path_location}.{collection}[{index}]"
                if collection == "premises":
                    source_ids = use.get("source_premise_ids")
                    if isinstance(source_ids, list) and source_ids != sorted(source_ids):
                        errors.append(
                            f"{use_location}.source_premise_ids: values must be canonical"
                        )
                    issuer_evidence_id = use.get("issuer_evidence_id")
                    issuer_occurrence_id = use.get("issuer_occurrence_id")
                    if (
                        not isinstance(source_ids, list)
                        or not isinstance(issuer_evidence_id, str)
                        or issuer_evidence_id not in source_ids
                        or not isinstance(issuer_occurrence_id, str)
                        or re.fullmatch(r"[0-9a-f]{64}", issuer_occurrence_id)
                        is None
                        or issuer_occurrence_id not in source_ids
                    ):
                        errors.append(
                            f"{use_location}: relation owner and occurrence must both "
                            "be retained by source_premise_ids"
                        )
                    legal_move_semantic_id = use.get("legal_move_semantic_id")
                    if (
                        isinstance(legal_move_semantic_id, str)
                        and isinstance(source_ids, list)
                        and f"legal-move:{legal_move_semantic_id}" not in source_ids
                    ):
                        errors.append(
                            f"{use_location}: legal movement lower fact is not retained"
                        )
                    result_id = use.get("result_id")
                    if isinstance(result_id, str):
                        result_ids.append(result_id)
                else:
                    use_id = use.get("use_id")
                    if isinstance(use_id, str):
                        path_closed_use_ids.append(use_id)

                branch_id = use.get("branch_id")
                self._validate_branch_reference(
                    branch_id,
                    use.get("branch_role"),
                    branches,
                    "branch_role",
                    f"{use_location}.branch_id",
                    errors,
                )
                branch = branches.get(branch_id) if isinstance(branch_id, str) else None
                if not isinstance(branch, Mapping):
                    continue
                raw_steps = branch.get("steps")
                steps = raw_steps if isinstance(raw_steps, list) else []
                if collection == "premises":
                    step_index = use.get("step_index")
                    if (
                        not isinstance(step_index, int)
                        or not 0 <= step_index < len(steps)
                    ):
                        errors.append(
                            f"{use_location}.step_index: does not reference a branch occurrence"
                        )
                    elif (
                        isinstance(use.get("move_uci"), str)
                        and isinstance(steps[step_index], Mapping)
                        and use.get("move_uci") != steps[step_index].get("move_uci")
                    ):
                        errors.append(
                            f"{use_location}.move_uci: does not equal its branch occurrence"
                        )
                    movement = use.get("movement")
                    move_uci = use.get("move_uci")
                    if isinstance(movement, Mapping) and isinstance(
                        move_uci, str
                    ) and not _movement_matches_uci(movement, move_uci):
                        errors.append(
                            f"{use_location}.movement: identity does not equal move_uci"
                        )
                    issuer_occurrence_id = use.get("issuer_occurrence_id")
                    if isinstance(issuer_occurrence_id, str):
                        route = (use.get("branch_id"), step_index)
                        if issuer_occurrence_id in premise_occurrence_routes:
                            errors.append(
                                f"{use_location}.issuer_occurrence_id: one lower occurrence cannot own multiple premise routes"
                            )
                        else:
                            premise_occurrence_routes[issuer_occurrence_id] = route
                    continue

                after_step_index = use.get("after_step_index")
                if (
                    not isinstance(after_step_index, int)
                    or not 0 <= after_step_index < len(steps)
                    or not isinstance(steps[after_step_index], Mapping)
                ):
                    errors.append(
                        f"{use_location}.after_step_index: does not reference a branch occurrence"
                    )
                    continue
                step = steps[after_step_index]
                position = use.get("position")
                if not isinstance(position, Mapping) or (
                    _normalized_fen(position.get("fen"))
                    != _normalized_fen(step.get("fen_after"))
                    or position.get("ply") != step.get("ply")
                ):
                    errors.append(
                        f"{use_location}.position: does not bind the supplied branch occurrence"
                    )
                expected_scope = (
                    "best_line"
                    if branch.get("branch_role") == "counterfactual_reference"
                    else "played_line"
                )
                if (
                    isinstance(position, Mapping)
                    and position.get("scope") != expected_scope
                ):
                    errors.append(
                        f"{use_location}.position.scope: does not match branch ownership"
                    )

            if len(result_ids) != len(set(result_ids)):
                errors.append(
                    f"{path_location}.premises: result_id values must be unique"
                )
            if len(path_closed_use_ids) != len(set(path_closed_use_ids)):
                errors.append(
                    f"{path_location}: closed use_id values must be unique within the proof path"
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
