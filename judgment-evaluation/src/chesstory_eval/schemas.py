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
            self._documents[resolved] = document
        return self._documents[resolved]

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
        through a frozen Q-to-V stage contract.  This additive entry point
        keeps those documents under the same no-network, fail-closed validator
        without changing any stage validation behavior.
        """

        schema = self.load(path)
        errors: list[str] = []
        self._validate(document, schema, self._resolved_schema_path(path), "$", errors)
        if errors:
            preview = "\n".join(f"- {error}" for error in errors[:30])
            suffix = f"\n- ... {len(errors) - 30} more" if len(errors) > 30 else ""
            raise ContractError(f"{label} schema validation failed:\n{preview}{suffix}")

    def validate_registry(self) -> None:
        for stage in STAGES:
            for direction in ("input", "output"):
                schema, path = self.schema(stage, direction)
                if schema.get("$schema") != "https://json-schema.org/draft/2020-12/schema":
                    raise ContractError(f"{path}: Draft 2020-12 declaration is required")
                if not isinstance(schema.get("$id"), str):
                    raise ContractError(f"{path}: versioned $id is required")
        common = self.load(self.root / "common.schema.json")
        if common.get("$schema") != "https://json-schema.org/draft/2020-12/schema":
            raise ContractError("common.schema.json must declare Draft 2020-12")

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
    """Fail-closed Q→V wiring and reference-closure state for one arm.

    This layer validates transport/meaning preservation only.  It never
    produces a fact, cause, claim, admission, rank, projection, or sentence;
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
    _CHESS_TOKEN = re.compile(
        r"(?<![A-Za-z0-9])(?:"
        r"O-O(?:-O)?|0-0(?:-0)?|"
        r"[KQRBN](?:[a-h1-8]{0,2})x?[a-h][1-8](?:=[QRBN])?[+#]?|"
        r"[a-h](?:x[a-h])?[1-8](?:=[QRBN])?[+#]?|"
        r"[a-h][1-8][a-h][1-8][qrbn]?|"
        r"[a-h][1-8]|"
        r"(?:king|queen|rook|bishop|knight|pawn)|"
        r"[a-h](?:-file|\s+file)|"
        r"[1-8](?:st|nd|rd|th)?(?:-rank|\s+rank)"
        r")(?![A-Za-z0-9])",
        re.IGNORECASE,
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
                "semantic transition registry lineage order is not frozen Q→V"
            )
        grounding = contract.get("verbalization_grounding")
        if not isinstance(grounding, Mapping):
            raise ContractError(
                "semantic transition registry has no verbalization grounding"
            )
        text_joiner = grounding.get("text_joiner")
        if not isinstance(text_joiner, str):
            raise ContractError("verbalization text_joiner must be text")
        self._text_joiner = text_joiner
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
        if registered in {"P", "V"}:
            # These boundaries are deliberately self-contained: every public
            # or verbalized reference must be available in the direct stage
            # input, not merely somewhere in an older hidden artifact.
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
                "plan_events": payload("Jp")["plan_events"],
            }
        if stage == "V":
            # The verbalization policy is frozen runner configuration, not an
            # upstream artifact.  Preserve it while binding projection exactly.
            return {
                "projection": payload("P"),
                "verbalization_policy": current_payload["verbalization_policy"],
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
        elif stage == "V":
            self._validate_verbalization(input_payload, output_payload)

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
            output_payload.get("plan_events"),
            input_payload.get("plan_events"),
            "plan_event_id",
            "P plan event",
        )
        self._require_materialized_subset(
            output_payload.get("exact_results"),
            exact_facts,
            "fact_id",
            "P exact result",
        )

    def _validate_verbalization(
        self,
        input_payload: Mapping[str, Any],
        output_payload: Mapping[str, Any],
    ) -> None:
        projection = input_payload.get("projection")
        policy = input_payload.get("verbalization_policy")
        if not isinstance(projection, Mapping) or not isinstance(policy, Mapping):
            raise ContractError("V input projection/policy is incomplete")
        if output_payload.get("projection_id") != projection.get("projection_id"):
            raise ContractError("V output projection_id does not name its direct P input")
        if output_payload.get("language") != policy.get("language"):
            raise ContractError("V output language does not preserve the frozen policy")

        sentences = output_payload.get("sentences")
        tokens = output_payload.get("authorized_tokens")
        if not isinstance(sentences, list) or not isinstance(tokens, list):
            raise ContractError("V sentences/authorized_tokens must be arrays")
        max_sentences = policy.get("max_sentences")
        if isinstance(max_sentences, int) and len(sentences) > max_sentences:
            raise ContractError("V exceeds the frozen max_sentences policy")

        text_parts: list[str] = []
        sentence_ranges: list[tuple[int, int, Mapping[str, Any]]] = []
        cursor = 0
        for index, sentence in enumerate(sentences):
            if not isinstance(sentence, Mapping) or not isinstance(sentence.get("text"), str):
                raise ContractError(f"V sentence {index} has no text")
            text = str(sentence["text"])
            text_parts.append(text)
            sentence_ranges.append((cursor, cursor + len(text), sentence))
            cursor += len(text) + len(self._text_joiner)
        complete_text = self._text_joiner.join(text_parts)

        authorized_spans: dict[tuple[int, int, str], Mapping[str, Any]] = {}
        for index, token in enumerate(tokens):
            if not isinstance(token, Mapping):
                raise ContractError(f"V authorized token {index} is not an object")
            start, end, spelling = (
                token.get("char_start"),
                token.get("char_end"),
                token.get("token"),
            )
            if (
                not isinstance(start, int)
                or isinstance(start, bool)
                or not isinstance(end, int)
                or isinstance(end, bool)
                or not isinstance(spelling, str)
                or start < 0
                or end <= start
                or end > len(complete_text)
                or complete_text[start:end] != spelling
            ):
                raise ContractError(
                    f"V authorized token {index} does not exactly close over its text span"
                )
            declared_kind = token.get("kind")
            if (
                self._CHESS_TOKEN.fullmatch(spelling) is not None
                and declared_kind not in self._chess_token_kinds(spelling)
            ):
                raise ContractError(
                    f"V authorized token {index} kind does not match {spelling!r}"
                )
            key = (start, end, spelling)
            if key in authorized_spans:
                raise ContractError(f"V duplicates authorized token span {start}:{end}")
            containing = [
                sentence
                for sentence_start, sentence_end, sentence in sentence_ranges
                if sentence_start <= start and end <= sentence_end
            ]
            if len(containing) != 1:
                raise ContractError(
                    f"V authorized token {index} crosses or misses a sentence boundary"
                )
            sentence = containing[0]
            token_claims = set(token.get("claim_ids", []))
            token_evidence = set(token.get("evidence_ids", []))
            if not token_claims and not token_evidence:
                raise ContractError(f"V authorized token {index} has no authorizing citation")
            if not token_claims.issubset(set(sentence.get("claim_ids", []))):
                raise ContractError(
                    f"V authorized token {index} claim is not cited by its sentence"
                )
            if not token_evidence.issubset(set(sentence.get("evidence_ids", []))):
                raise ContractError(
                    f"V authorized token {index} evidence is not cited by its sentence"
                )
            authorized_spans[key] = token

        prohibited = set(policy.get("prohibited_new_token_kinds", []))
        if prohibited:
            for match in self._CHESS_TOKEN.finditer(complete_text):
                if not (self._chess_token_kinds(match.group(0)) & prohibited):
                    continue
                key = (match.start(), match.end(), match.group(0))
                if key not in authorized_spans:
                    raise ContractError(
                        "V contains an unauthorized chess token at "
                        f"{match.start()}:{match.end()}: {match.group(0)!r}"
                    )

    @staticmethod
    def _chess_token_kinds(token: str) -> set[str]:
        lowered = token.lower()
        if lowered in {"king", "queen", "rook", "bishop", "knight", "pawn"}:
            return {"piece"}
        if lowered.endswith("-file") or lowered.endswith(" file"):
            return {"file"}
        if lowered.endswith("-rank") or lowered.endswith(" rank"):
            return {"rank"}
        if re.fullmatch(r"[a-h][1-8]", lowered):
            # Bare algebraic coordinates can be either a destination move or
            # a named square; authorization under either prohibited kind is
            # required by the frozen policy.
            return {"move", "square"}
        return {"move"}

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
