from __future__ import annotations

import copy
import itertools
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping

from .hashing import json_bytes, sha256_json, slug
from .model import ContractError, STAGES, require_stage
from .schemas import SchemaRegistry


@dataclass(frozen=True)
class _Identity:
    namespace: str
    old_id: str
    path: tuple[str | int, ...]


@dataclass(frozen=True)
class _Reference:
    namespace: str
    old_id: str
    path: tuple[str | int, ...]


# Exact canonical labeling deliberately has a hard ceiling.  Returning a
# representation chosen by a heuristic when a graph is too symmetric would
# make equality depend on the provider's opaque IDs.  A caller must instead
# simplify/fix the schema or explicitly raise this reviewed ceiling.
_MAX_EXACT_LABELINGS = 1_000_000


class Canonicalizer:
    """Canonicalizes only transformations explicitly authorized by JSON Schema.

    It never rounds numbers, changes confidence, reorders ranked arrays, fills
    missing values, or rewrites prose.  The original object is never mutated.
    """

    def __init__(self, registry: SchemaRegistry) -> None:
        self.registry = registry

    def lineage_session(self) -> "CanonicalizationSession":
        return CanonicalizationSession(self)

    def stage_document(self, document: Mapping[str, Any], stage: str, direction: str) -> dict[str, Any]:
        schema, schema_path = self.registry.schema(stage, direction)
        canonical = self.canonicalize(document, schema, schema_path)
        if not isinstance(canonical, dict):
            raise ContractError("stage document canonicalization did not produce an object")
        return canonical

    def canonicalize(
        self, value: Any, schema: Mapping[str, Any], schema_path: Path
    ) -> Any:
        canonical, _ = self._canonicalize_with_mapping(
            value,
            schema,
            schema_path,
            inherited={},
            fail_on_dangling=False,
        )
        return canonical

    def _canonicalize_with_mapping(
        self,
        value: Any,
        schema: Mapping[str, Any],
        schema_path: Path,
        *,
        inherited: Mapping[tuple[str, str], str],
        fail_on_dangling: bool,
    ) -> tuple[Any, dict[tuple[str, str], str]]:
        result = copy.deepcopy(value)
        result = self._shape(result, schema, schema_path)
        occurrences = self.registry.identity_occurrences(
            result, schema, schema_path
        )
        identities = [
            _Identity(item.namespace, item.value, item.path)
            for item in occurrences
            if item.kind == "identity-definition"
        ]
        references = [
            _Reference(item.namespace, item.value, item.path)
            for item in occurrences
            if item.kind == "identity-reference"
        ]
        unordered_paths: set[tuple[str | int, ...]] = set()
        self._collect_unordered_paths(
            result, schema, schema_path, (), unordered_paths
        )
        return self._canonical_reallocate(
            result,
            identities,
            references,
            unordered_paths,
            inherited=inherited,
            fail_on_dangling=fail_on_dangling,
        )

    def assert_idempotent(
        self, value: Any, schema: Mapping[str, Any], schema_path: Path
    ) -> None:
        once = self.canonicalize(value, schema, schema_path)
        twice = self.canonicalize(once, schema, schema_path)
        if json_bytes(once) != json_bytes(twice):
            raise ContractError("canonicalizer is not idempotent for the supplied document")

    def _shape(self, value: Any, raw_schema: Mapping[str, Any] | bool, base_path: Path) -> Any:
        if not isinstance(raw_schema, Mapping):
            return value
        schema, schema_path = self.registry.dereference(raw_schema, base_path)
        for branch in schema.get("allOf", []):
            value = self._shape(value, branch, schema_path)
        for keyword in ("oneOf", "anyOf"):
            matches = self.registry.matching_branches(value, schema, schema_path, keyword)
            if keyword == "oneOf" and len(matches) > 1:
                raise ContractError("canonicalizer encountered an ambiguous oneOf")
            for branch in matches:
                value = self._shape(value, branch, schema_path)

        if isinstance(value, dict):
            properties = schema.get("properties", {})
            for key in list(value):
                child_schema = properties.get(key)
                if child_schema is None:
                    child_schema = self._pattern_schema(key, schema)
                if child_schema is None:
                    continue
                annotation = self._annotation(child_schema, schema_path)
                if value[key] is None and self._has_mode(annotation, "null-equivalent-omission"):
                    del value[key]
                    continue
                value[key] = self._shape(value[key], child_schema, schema_path)
            return value

        if isinstance(value, list):
            item_schema = schema.get("items", True)
            shaped = [self._shape(item, item_schema, schema_path) for item in value]
            annotation = self._annotation(schema, schema_path)
            if self._has_mode(annotation, "unordered-set"):
                # Canonicalization may normalize representation order, but it
                # must never change cardinality.  If duplicates are illegal,
                # ``uniqueItems`` rejects them during schema validation.
                shaped = sorted(shaped, key=json_bytes)
            return shaped
        return value

    def _collect_unordered_paths(
        self,
        value: Any,
        raw_schema: Mapping[str, Any] | bool,
        base_path: Path,
        path: tuple[str | int, ...],
        unordered_paths: set[tuple[str | int, ...]],
    ) -> None:
        if not isinstance(raw_schema, Mapping):
            return
        schema, schema_path = self.registry.dereference(raw_schema, base_path)
        for branch in schema.get("allOf", []):
            self._collect_unordered_paths(
                value, branch, schema_path, path, unordered_paths
            )
        for keyword in ("oneOf", "anyOf"):
            matches = self.registry.matching_branches(
                value, schema, schema_path, keyword
            )
            if keyword == "oneOf" and len(matches) > 1:
                raise ContractError(
                    "canonicalizer encountered an ambiguous oneOf"
                )
            for branch in matches:
                self._collect_unordered_paths(
                    value, branch, schema_path, path, unordered_paths
                )

        annotation = self._annotation(schema, schema_path)
        if isinstance(value, dict):
            properties = schema.get("properties", {})
            for key, child in value.items():
                child_schema = properties.get(key)
                if child_schema is None:
                    child_schema = self._pattern_schema(key, schema)
                if child_schema is not None:
                    self._collect_unordered_paths(
                        child,
                        child_schema,
                        schema_path,
                        path + (key,),
                        unordered_paths,
                    )
        elif isinstance(value, list):
            if self._has_mode(annotation, "unordered-set"):
                unordered_paths.add(path)
            item_schema = schema.get("items", True)
            prefix_items = schema.get("prefixItems", [])
            for index, child in enumerate(value):
                child_schema = (
                    prefix_items[index]
                    if index < len(prefix_items)
                    else item_schema
                )
                self._collect_unordered_paths(
                    child,
                    child_schema,
                    schema_path,
                    path + (index,),
                    unordered_paths,
                )

    def _canonical_reallocate(
        self,
        value: Any,
        raw_identities: list[_Identity],
        raw_references: list[_Reference],
        unordered_paths: set[tuple[str | int, ...]],
        *,
        inherited: Mapping[tuple[str, str], str],
        fail_on_dangling: bool,
    ) -> tuple[Any, dict[tuple[str, str], str]]:
        identities_by_path: dict[tuple[str | int, ...], _Identity] = {}
        definitions: dict[tuple[str, str], _Identity] = {}
        for identity in raw_identities:
            previous_at_path = identities_by_path.get(identity.path)
            if previous_at_path is not None:
                if previous_at_path != identity:
                    raise ContractError(f"conflicting identity annotations at {identity.path!r}")
                continue
            key = (identity.namespace, identity.old_id)
            if key in definitions:
                raise ContractError(
                    f"duplicate identity definition: {identity.namespace}/{identity.old_id}"
                )
            identities_by_path[identity.path] = identity
            definitions[key] = identity

        references_by_path: dict[tuple[str | int, ...], _Reference] = {}
        for reference in raw_references:
            previous = references_by_path.get(reference.path)
            if previous is not None:
                if previous != reference:
                    raise ContractError(
                        f"conflicting reference annotations at {reference.path!r}"
                    )
                continue
            references_by_path[reference.path] = reference

        if fail_on_dangling:
            dangling = [
                reference
                for reference in references_by_path.values()
                if (reference.namespace, reference.old_id) not in definitions
                and (reference.namespace, reference.old_id) not in inherited
            ]
            if dangling:
                first = dangling[0]
                raise ContractError(
                    "dangling identity reference during lineage canonicalization: "
                    f"{first.namespace}/{first.old_id} at {first.path!r}"
                )

        colors = self._refine_identity_colors(
            value,
            definitions,
            identities_by_path,
            references_by_path,
            unordered_paths,
            inherited,
        )
        cells = self._label_cells(
            definitions,
            references_by_path,
            colors,
            inherited,
        )
        candidate_count = 1
        for members, _ in cells:
            for factor in range(2, len(members) + 1):
                if candidate_count > _MAX_EXACT_LABELINGS // factor:
                    raise ContractError(
                        "exact canonical graph labeling requires more than "
                        f"{_MAX_EXACT_LABELINGS} candidates; "
                        "refusing an ID-dependent approximation"
                    )
                candidate_count *= factor

        best_bytes: bytes | None = None
        best_value: Any = None
        best_mapping: dict[tuple[str, str], str] | None = None

        def visit(
            cell_index: int,
            mapping: dict[tuple[str, str], str],
        ) -> None:
            nonlocal best_bytes, best_value, best_mapping
            if cell_index == len(cells):
                candidate = copy.deepcopy(value)
                for identity in identities_by_path.values():
                    self._replace_at_path(
                        candidate,
                        identity.path,
                        mapping[(identity.namespace, identity.old_id)],
                    )
                for reference in references_by_path.values():
                    replacement = mapping.get((reference.namespace, reference.old_id))
                    if replacement is not None:
                        self._replace_at_path(candidate, reference.path, replacement)
                # The first shaping pass already performed null-equivalent
                # omission and recursively shaped every value.  Reallocation
                # can only disturb the byte order of authorized unordered
                # arrays, so repeating schema resolution here would be both
                # wasteful and broader than necessary.
                self._sort_unordered_arrays(candidate, (), unordered_paths)
                encoded = json_bytes(candidate)
                if best_bytes is None or encoded < best_bytes:
                    best_bytes = encoded
                    best_value = candidate
                    best_mapping = dict(mapping)
                return

            members, labels = cells[cell_index]
            # ``members`` may be ordered by the opaque old ID solely to make
            # traversal reproducible.  Every bijection is visited, so that
            # order can never break a semantic tie or affect the result.
            for assignment in itertools.permutations(labels):
                for key, label in zip(members, assignment):
                    mapping[key] = label
                visit(cell_index + 1, mapping)
                for key in members:
                    del mapping[key]

        visit(0, dict(inherited))
        if best_bytes is None or best_mapping is None:
            raise ContractError("exact canonical graph labeling produced no candidate")
        return best_value, best_mapping

    def _refine_identity_colors(
        self,
        value: Any,
        definitions: Mapping[tuple[str, str], _Identity],
        identities_by_path: Mapping[tuple[str | int, ...], _Identity],
        references_by_path: Mapping[tuple[str | int, ...], _Reference],
        unordered_paths: set[tuple[str | int, ...]],
        inherited: Mapping[tuple[str, str], str],
    ) -> dict[tuple[str, str], str]:
        colors = {
            key: (
                f"fixed:{inherited[key]}"
                if key in inherited
                else f"namespace:{key[0]}"
            )
            for key in definitions
        }
        old_partition = self._color_partition(colors)

        # Each round roots the complete schema-shaped document at one identity.
        # Consequently both incoming and outgoing references participate, while
        # ordered array indices remain meaningful and unordered arrays become
        # multisets.  The refinement is invariant under opaque-ID renaming.
        for _ in range(len(definitions) + 1):
            signatures = {
                key: json_bytes(
                    [
                        colors[key],
                        self._identity_graph_view(
                            value,
                            (),
                            key,
                            colors,
                            definitions,
                            identities_by_path,
                            references_by_path,
                            unordered_paths,
                            inherited,
                        ),
                    ]
                )
                for key in definitions
            }
            unique = {signature: index for index, signature in enumerate(sorted(set(signatures.values())))}
            width = max(4, len(str(len(unique))))
            updated = {
                key: f"color:{unique[signature]:0{width}d}"
                for key, signature in signatures.items()
            }
            new_partition = self._color_partition(updated)
            colors = updated
            if new_partition == old_partition:
                return colors
            old_partition = new_partition
        raise ContractError("identity graph color refinement did not stabilize")

    def _identity_graph_view(
        self,
        node: Any,
        path: tuple[str | int, ...],
        focus: tuple[str, str],
        colors: Mapping[tuple[str, str], str],
        definitions: Mapping[tuple[str, str], _Identity],
        identities_by_path: Mapping[tuple[str | int, ...], _Identity],
        references_by_path: Mapping[tuple[str | int, ...], _Reference],
        unordered_paths: set[tuple[str | int, ...]],
        inherited: Mapping[tuple[str, str], str],
    ) -> Any:
        identity = identities_by_path.get(path)
        if identity is not None:
            key = (identity.namespace, identity.old_id)
            return ["identity-definition", identity.namespace, "focus" if key == focus else colors[key]]

        reference = references_by_path.get(path)
        if reference is not None:
            target = (reference.namespace, reference.old_id)
            if target in definitions:
                target_color = "focus" if target == focus else colors[target]
                return ["identity-reference", reference.namespace, target_color]
            if target in inherited:
                return [
                    "lineage-reference",
                    reference.namespace,
                    inherited[target],
                ]
            # Undefined references are not authorized for reallocation and
            # therefore remain ordinary, semantically significant values.
            return ["external-reference", reference.namespace, reference.old_id]

        if isinstance(node, dict):
            return {
                key: self._identity_graph_view(
                    child,
                    path + (key,),
                    focus,
                    colors,
                    definitions,
                    identities_by_path,
                    references_by_path,
                    unordered_paths,
                    inherited,
                )
                for key, child in sorted(node.items())
            }
        if isinstance(node, list):
            children = [
                self._identity_graph_view(
                    child,
                    path + (index,),
                    focus,
                    colors,
                    definitions,
                    identities_by_path,
                    references_by_path,
                    unordered_paths,
                    inherited,
                )
                for index, child in enumerate(node)
            ]
            if path in unordered_paths:
                children.sort(key=json_bytes)
            return children
        return node

    @staticmethod
    def _color_partition(
        colors: Mapping[tuple[str, str], str],
    ) -> frozenset[frozenset[tuple[str, str]]]:
        groups: dict[str, set[tuple[str, str]]] = {}
        for key, color in colors.items():
            groups.setdefault(color, set()).add(key)
        return frozenset(frozenset(group) for group in groups.values())

    @staticmethod
    def _label_cells(
        definitions: Mapping[tuple[str, str], _Identity],
        references_by_path: Mapping[tuple[str | int, ...], _Reference],
        colors: Mapping[tuple[str, str], str],
        inherited: Mapping[tuple[str, str], str],
    ) -> list[tuple[tuple[tuple[str, str], ...], tuple[str, ...]]]:
        by_namespace: dict[str, dict[str, list[tuple[str, str]]]] = {}
        for key in definitions:
            if key in inherited:
                continue
            by_namespace.setdefault(key[0], {}).setdefault(colors[key], []).append(key)

        dangling_by_namespace: dict[str, set[str]] = {}
        for reference in references_by_path.values():
            key = (reference.namespace, reference.old_id)
            if key not in definitions and key not in inherited:
                dangling_by_namespace.setdefault(reference.namespace, set()).add(reference.old_id)

        used_by_namespace: dict[str, set[str]] = {}
        for (namespace, _), label in inherited.items():
            used_by_namespace.setdefault(namespace, set()).add(label)

        cells: list[tuple[tuple[tuple[str, str], ...], tuple[str, ...]]] = []
        for namespace in sorted(by_namespace):
            color_groups = by_namespace[namespace]
            total = sum(len(group) for group in color_groups.values())
            width = max(4, len(str(total)))
            reserved = (
                dangling_by_namespace.get(namespace, set())
                | used_by_namespace.get(namespace, set())
            )
            labels: list[str] = []
            index = 1
            while len(labels) < total:
                candidate = f"{slug(namespace)}:{index:0{width}d}"
                index += 1
                if candidate not in reserved:
                    labels.append(candidate)

            offset = 0
            for color in sorted(color_groups):
                members = tuple(sorted(color_groups[color]))
                cell_labels = tuple(labels[offset : offset + len(members)])
                cells.append((members, cell_labels))
                offset += len(members)
        return cells

    @staticmethod
    def _replace_at_path(root: Any, path: tuple[str | int, ...], value: str) -> None:
        if not path:
            raise ContractError("identity annotation cannot replace the document root")
        container = root
        for part in path[:-1]:
            container = container[part]
        container[path[-1]] = value

    @classmethod
    def _sort_unordered_arrays(
        cls,
        node: Any,
        path: tuple[str | int, ...],
        unordered_paths: set[tuple[str | int, ...]],
    ) -> None:
        if isinstance(node, dict):
            for key, child in node.items():
                cls._sort_unordered_arrays(child, path + (key,), unordered_paths)
        elif isinstance(node, list):
            for index, child in enumerate(node):
                cls._sort_unordered_arrays(child, path + (index,), unordered_paths)
            if path in unordered_paths:
                node.sort(key=json_bytes)

    def _annotation(self, raw_schema: Mapping[str, Any] | bool, base_path: Path) -> Mapping[str, Any]:
        if not isinstance(raw_schema, Mapping):
            return {}
        schema, _ = self.registry.dereference(raw_schema, base_path)
        annotation = schema.get("x-canonical", {})
        return annotation if isinstance(annotation, Mapping) else {}

    @classmethod
    def _has_mode(cls, annotation: Mapping[str, Any], expected: str) -> bool:
        mode = annotation.get("mode")
        if mode == expected:
            return True
        modes = annotation.get("modes", [])
        return isinstance(modes, list) and expected in modes

    @staticmethod
    def _pattern_schema(key: str, schema: Mapping[str, Any]) -> Mapping[str, Any] | bool | None:
        for pattern, child_schema in schema.get("patternProperties", {}).items():
            if __import__("re").search(pattern, key):
                return child_schema
        additional = schema.get("additionalProperties")
        return additional if isinstance(additional, (dict, bool)) and additional is not False else None


class CanonicalizationSession:
    """Canonical meaning state for one complete Q→P arm lineage."""

    def __init__(self, canonicalizer: Canonicalizer) -> None:
        self.canonicalizer = canonicalizer
        self._mapping: dict[tuple[str, str], str] = {}
        self._canonical_output_hashes: list[str] = []
        self._next_stage_index = 0
        self._pending_stage: str | None = None
        self._pending_input_hash: str | None = None
        self._pending_upstream_hashes: list[str] = []

    def prepare_input(
        self, document: Mapping[str, Any], stage: str
    ) -> dict[str, Any]:
        registered = self._require_next(stage)
        if self._pending_stage is not None:
            raise ContractError(
                f"canonical lineage still has pending stage {self._pending_stage}"
            )
        schema, schema_path = self.canonicalizer.registry.schema(
            registered, "input"
        )
        canonical, mapping = self.canonicalizer._canonicalize_with_mapping(
            document,
            schema,
            schema_path,
            inherited=self._mapping,
            fail_on_dangling=True,
        )
        if not isinstance(canonical, dict):
            raise ContractError("canonical stage input is not an object")
        self._adopt_definitions(document, registered, "input", mapping)
        upstream_hashes = list(self._canonical_output_hashes)
        canonical["upstream_artifact_hashes"] = upstream_hashes
        payload = canonical.get("payload")
        if not isinstance(payload, Mapping):
            raise ContractError(f"{registered} canonical input has no payload")
        canonical_input_hash = sha256_json(
            {
                "stage": registered,
                "payload": payload,
                "upstream_artifact_hashes": upstream_hashes,
            }
        )
        canonical["input_hash"] = canonical_input_hash
        self._pending_stage = registered
        self._pending_input_hash = canonical_input_hash
        self._pending_upstream_hashes = upstream_hashes
        return canonical

    def stage_document(
        self,
        document: Mapping[str, Any],
        stage: str,
        direction: str = "output",
    ) -> dict[str, Any]:
        registered = require_stage(stage)
        if direction != "output":
            raise ContractError("lineage stage_document only accepts stage outputs")
        if self._pending_stage != registered or self._pending_input_hash is None:
            raise ContractError(
                f"{registered} canonical output has no prepared canonical input"
            )
        schema, schema_path = self.canonicalizer.registry.schema(
            registered, direction
        )
        canonical, mapping = self.canonicalizer._canonicalize_with_mapping(
            document,
            schema,
            schema_path,
            inherited=self._mapping,
            fail_on_dangling=True,
        )
        if not isinstance(canonical, dict):
            raise ContractError("canonical stage output is not an object")
        self._adopt_definitions(document, registered, direction, mapping)
        canonical["input_hash"] = self._pending_input_hash
        canonical["upstream_artifact_hashes"] = list(
            self._pending_upstream_hashes
        )

        # Prove idempotence against the now-complete lineage mapping.  This is
        # stronger than the old stage-local check because upstream-only
        # references participate.
        repeated, _ = self.canonicalizer._canonicalize_with_mapping(
            canonical,
            schema,
            schema_path,
            inherited=self._mapping,
            fail_on_dangling=True,
        )
        if not isinstance(repeated, dict):
            raise ContractError("repeated canonical stage output is not an object")
        repeated["input_hash"] = self._pending_input_hash
        repeated["upstream_artifact_hashes"] = list(
            self._pending_upstream_hashes
        )
        if json_bytes(canonical) != json_bytes(repeated):
            raise ContractError(
                "lineage canonicalizer is not idempotent for the supplied document"
            )

        self._canonical_output_hashes.append(sha256_json(canonical))
        self._next_stage_index += 1
        self._pending_stage = None
        self._pending_input_hash = None
        self._pending_upstream_hashes = []
        return canonical

    def _adopt_definitions(
        self,
        original: Mapping[str, Any],
        stage: str,
        direction: str,
        mapping: Mapping[tuple[str, str], str],
    ) -> None:
        schema, schema_path = self.canonicalizer.registry.schema(stage, direction)
        occurrences = self.canonicalizer.registry.identity_occurrences(
            original, schema, schema_path
        )
        for occurrence in occurrences:
            if occurrence.kind != "identity-definition":
                continue
            key = (occurrence.namespace, occurrence.value)
            label = mapping.get(key)
            if label is None:
                raise ContractError(
                    f"canonical mapping omitted definition {occurrence.namespace}/{occurrence.value}"
                )
            previous = self._mapping.get(key)
            if previous is not None and previous != label:
                raise ContractError(
                    f"lineage remapped {occurrence.namespace}/{occurrence.value}"
                )
            alias = (occurrence.namespace, label)
            alias_previous = self._mapping.get(alias)
            if alias_previous is not None and alias_previous != label:
                raise ContractError(
                    f"canonical namespace label collision: {occurrence.namespace}/{label}"
                )
            self._mapping[key] = label
            self._mapping[alias] = label

    def _require_next(self, stage: str) -> str:
        registered = require_stage(stage)
        if self._next_stage_index >= len(STAGES):
            raise ContractError("canonical lineage session is already complete")
        expected = STAGES[self._next_stage_index]
        if registered != expected:
            raise ContractError(
                f"canonical lineage expected {expected}, got {registered}"
            )
        return registered


def canonical_json(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"), allow_nan=False)
