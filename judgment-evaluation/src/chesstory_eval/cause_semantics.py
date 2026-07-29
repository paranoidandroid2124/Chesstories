from __future__ import annotations

import math
from collections import Counter
from collections.abc import Mapping, Sequence
from typing import Any

import chess

from .hashing import json_bytes, sha256_json
from .model import ContractError, IntegrityError
from .native_diagnostic import native_sha256_json


STAGE_ORDER = {"C": 0, "Jp": 1, "Ja": 2, "R": 3, "P": 4}
IMPORTANCE_RELATION_POLICY_VERSION = (
    "chesstory.direct-cause-importance.relation.v3"
)
IMPORTANCE_ORDERING_POLICY_VERSION = (
    "chesstory.player-facing-idea-ordering.dominance-layers.v1"
)
VERDICT_CLASSIFICATION_POLICY_VERSION = "chesstory.verdict-threshold-policy.v1"


def _object(value: Any, label: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object")
    return dict(value)


def _objects(value: Any, label: str) -> list[dict[str, Any]]:
    if not isinstance(value, list) or not all(isinstance(item, Mapping) for item in value):
        raise ContractError(f"{label} must be an object array")
    return [dict(item) for item in value]


def _normalized_atom(value: Mapping[str, Any]) -> tuple[str, str]:
    kind = value.get("kind")
    key = value.get("key")
    if not isinstance(kind, str) or not kind:
        raise ContractError("semantic chess object has no kind")
    if not isinstance(key, str) or not key.strip():
        raise ContractError("semantic chess object has no key")
    return kind.strip().lower(), key.strip().lower()


def _atom_set(value: Any, label: str) -> set[tuple[str, str]]:
    return {_normalized_atom(item) for item in _objects(value, label)}


def _object_constraint_matches(actual: Any, expected: Mapping[str, Any]) -> bool:
    actual_atoms = _atom_set(actual, "actual semantic objects")
    required = _atom_set(expected.get("required"), "oracle required semantic objects")
    allowed = _atom_set(expected.get("allowed"), "oracle allowed semantic objects")
    return required.issubset(actual_atoms) and actual_atoms.issubset(allowed)


def _expected_actor_atoms(actor: Mapping[str, Any]) -> set[tuple[str, str]]:
    return {
        ("move", str(actor["move"]).strip().lower()),
        ("side", str(actor["side"]).strip().lower()),
        ("piece", str(actor["piece"]).strip().lower()),
        ("square", str(actor["from"]).strip().lower()),
        ("square", str(actor["to"]).strip().lower()),
    }


def _actor_matches(actual: Any, expected: Mapping[str, Any]) -> bool:
    return _atom_set(actual, "actual actor") == _expected_actor_atoms(expected)


def _line_root(value: Any) -> tuple[str | None, str | None]:
    if not isinstance(value, Mapping):
        return None, None
    root = value.get("root_move")
    role = value.get("role")
    return (
        root.strip().lower() if isinstance(root, str) else None,
        role.strip().lower() if isinstance(role, str) else None,
    )


def _line_identity(value: Mapping[str, Any]) -> tuple[Any, Any, Any, str | None]:
    """Normalize runtime ``id`` and semantic ``line_id`` without conflating them."""
    ids = [value.get(name) for name in ("id", "line_id") if value.get(name) is not None]
    if len(ids) != 1 or not isinstance(ids[0], str) or not ids[0]:
        raise IntegrityError("actual C line has no unambiguous identity")
    return (ids[0], value.get("role"), value.get("rank"), _line_root(value)[0])


def _line_matches(actual: Any, expected: Mapping[str, Any]) -> bool:
    return _line_root(actual) == (
        str(expected["root_move"]).strip().lower(),
        str(expected["role"]).strip().lower(),
    )


def _selection_comparison_matches(
    selection: Mapping[str, Any], realization: Mapping[str, Any]
) -> bool:
    actual = selection.get("comparison")
    if not isinstance(actual, Mapping):
        return False
    expected = _object(realization.get("comparison"), "oracle comparison")
    if actual.get("kind") != expected.get("kind") or actual.get("mover") != expected.get("mover"):
        return False
    reference = actual.get("reference")
    candidate = actual.get("candidate")
    event = actual.get("event")
    return bool(
        isinstance(reference, Mapping)
        and isinstance(candidate, Mapping)
        and isinstance(event, Mapping)
        and str(reference.get("root_move", "")).lower()
        == str(expected.get("reference_root_move", "")).lower()
        and str(candidate.get("root_move", "")).lower()
        == str(expected.get("candidate_root_move", "")).lower()
        and _line_matches(
            event,
            {
                "role": expected.get("event_role"),
                "root_move": expected.get("event_root_move"),
            },
        )
    )


def _channel_matches(
    actual: Mapping[str, Any],
    expected: Mapping[str, Any],
    *,
    require_played_change: bool,
) -> bool:
    if actual.get("direct_change") != expected.get("direct_change"):
        return False
    if require_played_change and actual.get("played_change") != expected.get("played_change"):
        return False
    if not _actor_matches(actual.get("actor"), _object(expected.get("actor"), "oracle actor")):
        return False
    for singular, plural in (
        ("target", "targets"),
        ("mechanism", "mechanisms"),
        ("consequence", "consequences"),
    ):
        present = [name for name in (singular, plural) if name in actual]
        if len(present) != 1:
            return False
        if not _object_constraint_matches(
            actual.get(present[0]),
            _object(expected.get(plural), f"oracle {plural}"),
        ):
            return False
    line = actual.get("line")
    if not _line_matches(line, _object(expected.get("line"), "oracle channel line")):
        return False
    actual_horizon = actual.get("horizon")
    expected_horizon = expected.get("horizon")
    return actual_horizon == expected_horizon


def _channels_exact_match(
    actual_channels: Sequence[Mapping[str, Any]],
    expected_channels: Sequence[Mapping[str, Any]],
    *,
    require_played_change: bool,
) -> bool:
    """Require one-to-one semantic channel coverage.

    Oracle ``channels`` are a conjunction, not alternatives. Carrier and
    evidence IDs are deliberately ignored, but every semantic channel on both
    sides must participate in one exact match. This prevents cross-channel
    object union, missing oracle channels, and unlabelled extra noise.
    """

    if not actual_channels or len(actual_channels) != len(expected_channels):
        return False
    edges: dict[int, list[int]] = {
        expected_index: [
            actual_index
            for actual_index, actual in enumerate(actual_channels)
            if _channel_matches(
                actual,
                expected,
                require_played_change=require_played_change,
            )
        ]
        for expected_index, expected in enumerate(expected_channels)
    }
    owner: dict[int, int] = {}

    def assign(expected_index: int, visited: set[int]) -> bool:
        for actual_index in edges[expected_index]:
            if actual_index in visited:
                continue
            visited.add(actual_index)
            previous = owner.get(actual_index)
            if previous is None or assign(previous, visited):
                owner[actual_index] = expected_index
                return True
        return False

    return all(assign(index, set()) for index in range(len(expected_channels)))


def _cause_id(cause: Mapping[str, Any]) -> str:
    record = cause.get("cause_record")
    if not isinstance(record, Mapping) or not isinstance(record.get("id"), str):
        raise ContractError("actual Cause has no cause_record.id")
    return str(record["id"])


def _c_channel_projection(channel: Mapping[str, Any]) -> dict[str, Any]:
    """Neutral observation fields carried through the C lineage."""
    return {
        name: channel.get(name)
        for name in (
            "causal_signature",
            "primitive_signature",
            "direct_change",
            "source_id",
            "provenance_source_ids",
            "carrier",
            "carrier_ancestor_source_ids",
            "provenance",
            "primitive_proof_source",
            "evidence_identity_sha256",
            "proof_role",
            "line_id",
            "actor",
            "target",
            "mechanism",
            "consequence",
            "line",
            "horizon",
            "proof_segment",
            "effect_descriptor",
            "proof_segment_ambiguous",
        )
    }


def _registered_source_ref(value: Any, label: str) -> dict[str, Any]:
    """Validate documentary identity without interpreting its payload type."""

    ref = _object(value, label)
    record_hash = ref.get("record_sha256")
    if (
        not isinstance(ref.get("id"), str)
        or not ref["id"]
        or any(
            not isinstance(ref.get(name), str) or not ref[name]
            for name in ("producer", "layer", "scope", "confidence")
        )
        or ref.get("record_registered") is not True
        or not isinstance(ref.get("record_payload_type"), str)
        or not ref["record_payload_type"]
        or not isinstance(record_hash, str)
        or len(record_hash) != 64
        or any(char not in "0123456789abcdef" for char in record_hash)
    ):
        raise IntegrityError(f"{label} is not an exact registered evidence reference")
    line = ref.get("line")
    if line is not None:
        _line_identity(_object(line, f"{label} line"))
    return ref


def _canonical_atoms(value: Any, label: str) -> tuple[tuple[str, str], ...]:
    atoms = tuple(sorted(_normalized_atom(item) for item in _objects(value, label)))
    if len(atoms) != len(set(atoms)):
        raise IntegrityError(f"{label} has duplicate semantic objects")
    return atoms


def _neutral_witness_projection(
    value: Mapping[str, Any], label: str
) -> dict[str, Any]:
    """Canonical Cause-neutral identity shared by F snapshots and C channels."""

    line = _object(value.get("line"), f"{label} line")
    carrier = _registered_source_ref(value.get("carrier"), f"{label} carrier")
    provenance = [
        _registered_source_ref(item, f"{label} provenance")
        for item in _objects(value.get("provenance"), f"{label} provenance")
    ]
    primitive_source = _registered_source_ref(
        value.get("primitive_proof_source"), f"{label} primitive proof source"
    )
    if not isinstance(value.get("proof_segment"), Mapping):
        raise IntegrityError(f"{label} has no exact proof segment")
    if not isinstance(value.get("effect_descriptor"), Mapping):
        raise IntegrityError(f"{label} has no exact effect descriptor")
    proof_segment = dict(value["proof_segment"])
    effect_descriptor = dict(value["effect_descriptor"])
    ancestor_ids = value.get("carrier_ancestor_source_ids")
    if (
        not isinstance(ancestor_ids, list)
        or any(not isinstance(item, str) or not item for item in ancestor_ids)
        or len(ancestor_ids) != len(set(ancestor_ids))
    ):
        raise IntegrityError(f"{label} has invalid carrier ancestry")
    return {
        "source_side": value.get("source_side"),
        "line": _line_identity(line),
        "carrier": carrier,
        "provenance": sorted(provenance, key=json_bytes),
        "carrier_ancestor_source_ids": tuple(sorted(ancestor_ids)),
        "primitive_proof_source": primitive_source,
        "actor": _canonical_atoms(value.get("actor"), f"{label} actor"),
        "target": _canonical_atoms(value.get("target"), f"{label} target"),
        "mechanism": _canonical_atoms(value.get("mechanism"), f"{label} mechanism"),
        "consequence": _canonical_atoms(
            value.get("consequence"), f"{label} consequence"
        ),
        "witness": _canonical_atoms(value.get("witness"), f"{label} witness"),
        "horizon": value.get("horizon"),
        "proof_segment": proof_segment,
        "effect_descriptor": effect_descriptor,
    }


def comparison_endpoint_snapshot_index(
    actual: Mapping[str, Any],
) -> dict[str, dict[str, Any]]:
    """Validate and index the sole Cause-neutral F endpoint inventory."""

    snapshots = _objects(
        actual.get("comparison_endpoint_evidence_snapshots"),
        "comparison endpoint evidence snapshots",
    )
    result: dict[str, dict[str, Any]] = {}
    for snapshot in snapshots:
        comparison_evidence = _registered_source_ref(
            snapshot.get("comparison_evidence"),
            "comparison endpoint snapshot evidence",
        )
        comparison_id = str(comparison_evidence["id"])
        if comparison_id in result:
            raise IntegrityError(
                "comparison endpoint snapshots have duplicate comparison evidence"
            )
        comparison = _object(
            snapshot.get("comparison"), "comparison endpoint snapshot comparison"
        )
        reference_line = _object(
            comparison.get("reference_line"), "snapshot reference comparison line"
        )
        candidate_line = _object(
            comparison.get("candidate_line"), "snapshot candidate comparison line"
        )
        reference_identity = _line_identity(reference_line)
        candidate_identity = _line_identity(candidate_line)
        if (
            reference_identity[3]
            != str(comparison.get("reference_root_move", "")).lower()
            or candidate_identity[3]
            != str(comparison.get("candidate_root_move", "")).lower()
            or reference_identity == candidate_identity
        ):
            raise IntegrityError(
                "comparison endpoint snapshot lines disagree with its comparison"
            )

        indexed_sides: dict[str, dict[str, Any]] = {}
        witness_owners: dict[bytes, str] = {}
        for source_side, expected_identity in (
            ("reference", reference_identity),
            ("candidate", candidate_identity),
        ):
            side = _object(
                snapshot.get(source_side),
                f"comparison endpoint snapshot {source_side} side",
            )
            side_line = _object(
                side.get("line"), f"comparison endpoint snapshot {source_side} line"
            )
            if (
                side.get("source_side") != source_side
                or _line_identity(side_line) != expected_identity
            ):
                raise IntegrityError(
                    "comparison endpoint snapshot side disagrees with its comparison"
                )
            indexed_witnesses: list[dict[str, Any]] = []
            for witness in _objects(
                side.get("witnesses"),
                f"comparison endpoint snapshot {source_side} witnesses",
            ):
                projection = _neutral_witness_projection(
                    witness, f"comparison endpoint {source_side} witness"
                )
                if (
                    projection["source_side"] != source_side
                    or projection["line"] != expected_identity
                ):
                    raise IntegrityError(
                        "comparison endpoint witness has contradictory side or line"
                    )
                carried_refs = [projection["carrier"], *projection["provenance"]]
                if sum(
                    json_bytes(ref)
                    == json_bytes(projection["primitive_proof_source"])
                    for ref in carried_refs
                ) != 1:
                    raise IntegrityError(
                        "comparison endpoint witness primitive source is not exactly carried"
                    )
                provenance_ids = {
                    str(ref["id"]) for ref in projection["provenance"]
                }
                if not provenance_ids.issubset(
                    set(projection["carrier_ancestor_source_ids"])
                ):
                    raise IntegrityError(
                        "comparison endpoint witness provenance is outside carrier ancestry"
                    )
                for ref_name in ("carrier", "primitive_proof_source"):
                    evidence_line = projection[ref_name].get("line")
                    if evidence_line is not None and _line_identity(
                        _object(evidence_line, "snapshot witness evidence line")
                    ) not in {reference_identity, candidate_identity}:
                        raise IntegrityError(
                            "comparison endpoint witness evidence is outside comparison endpoints"
                        )
                for provenance_ref in projection["provenance"]:
                    evidence_line = provenance_ref.get("line")
                    if evidence_line is not None and _line_identity(
                        _object(evidence_line, "snapshot witness provenance line")
                    ) not in {reference_identity, candidate_identity}:
                        raise IntegrityError(
                            "comparison endpoint witness provenance is outside comparison endpoints"
                        )
                if not all(
                    projection[name]
                    for name in ("actor", "target", "mechanism", "consequence")
                ):
                    raise IntegrityError(
                        "comparison endpoint witness has incomplete direct-effect objects"
                    )
                required_actor = {
                    ("move", expected_identity[3]),
                    ("side", comparison.get("mover")),
                    ("square", str(expected_identity[3])[:2]),
                    ("square", str(expected_identity[3])[2:4]),
                }
                if (
                    not required_actor <= set(projection["actor"])
                    or sum(kind == "piece" for kind, _ in projection["actor"]) != 1
                ):
                    raise IntegrityError(
                        "comparison endpoint witness actor disagrees with its endpoint"
                    )
                effect_scope = _object(
                    projection["effect_descriptor"].get("effect_scope"),
                    "comparison endpoint witness effect scope",
                )
                descriptor_targets = sorted(
                    str(value).strip().lower()
                    for value in effect_scope.get("target_signatures", [])
                )
                witness_targets = sorted(
                    f"{kind}:{key}" for kind, key in projection["target"]
                )
                if descriptor_targets != witness_targets:
                    raise IntegrityError(
                        "comparison endpoint witness descriptor disagrees with its target"
                    )
                proof_steps = _objects(
                    projection["proof_segment"].get("steps"),
                    "comparison endpoint witness proof steps",
                )
                offsets = [step.get("ply_offset") for step in proof_steps]
                if (
                    not proof_steps
                    or proof_steps[0].get("role") != "root_action"
                    or proof_steps[0].get("ply_offset") != 0
                    or str(proof_steps[0].get("move_uci", "")).lower()
                    != expected_identity[3]
                    or offsets != sorted(offsets)
                    or len(offsets) != len(set(offsets))
                    or (
                        len(proof_steps) > 1
                        and (
                            any(
                                step.get("role") != "causal_link"
                                for step in proof_steps[1:-1]
                            )
                            or proof_steps[-1].get("role") != "terminal_event"
                        )
                    )
                ):
                    raise IntegrityError(
                        "comparison endpoint witness proof segment is contradictory"
                    )
                key = json_bytes(projection)
                if key in witness_owners:
                    raise IntegrityError(
                        "comparison endpoint snapshot has an ambiguous duplicate witness"
                    )
                witness_owners[key] = source_side
                indexed_witnesses.append({"value": witness, "projection": projection})
            indexed_sides[source_side] = {
                "line": side_line,
                "line_identity": expected_identity,
                "witnesses": indexed_witnesses,
            }
        result[comparison_id] = {
            "comparison_evidence": comparison_evidence,
            "comparison": comparison,
            "reference": indexed_sides["reference"],
            "candidate": indexed_sides["candidate"],
        }
    return result


def _normalized_direct_effect_admission(
    value: Any, label: str
) -> tuple[str, frozenset[str]]:
    admission = _object(value, label)
    status = admission.get("status")
    signatures = admission.get("causal_signatures")
    if status not in {"unresolved", "restricted"}:
        raise IntegrityError(f"{label} has an invalid status")
    if not isinstance(signatures, list) or any(
        not isinstance(signature, str) or not signature for signature in signatures
    ):
        raise IntegrityError(f"{label} has invalid causal signatures")
    if len(signatures) != len(set(signatures)):
        raise IntegrityError(f"{label} has duplicate causal signatures")
    if status == "unresolved" and signatures:
        raise IntegrityError(f"{label} unresolved state admits causal signatures")
    return status, frozenset(signatures)


def _c_channel_layers(
    cause: Mapping[str, Any],
) -> tuple[
    dict[str, dict[str, Any]],
    dict[str, dict[str, Any]],
    dict[str, dict[str, Any]],
]:
    c_value = _object(cause.get("c"), "actual Cause C state")
    objects = _object(c_value.get("objects"), "actual Cause C objects")

    def layer(name: str) -> dict[str, dict[str, Any]]:
        values = _objects(objects.get(name), f"actual C {name}")
        result: dict[str, dict[str, Any]] = {}
        for value in values:
            signature = value.get("causal_signature")
            if not isinstance(signature, str) or not signature:
                raise IntegrityError(f"actual C {name} has invalid causal_signature")
            if signature in result:
                raise IntegrityError(f"actual C {name} has duplicate causal_signature")
            result[signature] = value
        return result

    return (
        layer("raw_owned_bindings"),
        layer("pre_admission_owned_bindings"),
        layer("owned_bindings"),
    )


def _cause_endpoint_snapshot(
    c_value: Mapping[str, Any],
    snapshot_index: Mapping[str, Mapping[str, Any]],
) -> dict[str, Any]:
    comparison_evidence = _registered_source_ref(
        c_value.get("comparison_evidence"), "actual C comparison evidence"
    )
    snapshot = snapshot_index.get(str(comparison_evidence["id"]))
    if snapshot is None:
        raise IntegrityError(
            "actual C comparison has no Cause-neutral endpoint snapshot"
        )
    if json_bytes(comparison_evidence) != json_bytes(
        snapshot["comparison_evidence"]
    ):
        raise IntegrityError(
            "actual C comparison evidence differs from its endpoint snapshot"
        )
    comparison = _object(c_value.get("comparison"), "actual C comparison")
    if json_bytes(comparison) != json_bytes(snapshot["comparison"]):
        raise IntegrityError(
            "actual C comparison differs from its endpoint snapshot"
        )
    return dict(snapshot)


def _channel_neutral_projection(
    channel: Mapping[str, Any],
    source_side: str,
    label: str,
) -> dict[str, Any]:
    return _neutral_witness_projection(
        {**dict(channel), "source_side": source_side}, label
    )


def _matching_endpoint_witnesses(
    channel: Mapping[str, Any],
    source_side: str,
    snapshot: Mapping[str, Any],
    label: str,
) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    projection = _channel_neutral_projection(channel, source_side, label)
    side = snapshot.get(source_side)
    if not isinstance(side, Mapping):
        raise IntegrityError("actual C owned channel has no concrete endpoint side")
    matches = [
        dict(item)
        for item in side["witnesses"]
        if json_bytes(item["projection"]) == json_bytes(projection)
    ]
    return projection, matches


def _verified_owned_channels(
    cause: Mapping[str, Any],
    snapshot_index: Mapping[str, Mapping[str, Any]],
) -> list[dict[str, Any]]:
    """Independently verify C ownership against the Cause-neutral F snapshot.

    Raw/pre-admission projections and runtime eligibility flags never license
    C truth. Every admitted channel must instead have one unique endpoint
    witness with the same comparison, side, carrier lineage, primitive source,
    semantic objects, proof segment, and effect descriptor.
    """

    c_value = _object(cause.get("c"), "actual Cause C state")
    raw, pre, owned = _c_channel_layers(cause)
    # This is documentary lineage only. It can reject a forged admission, but
    # it cannot make a channel true; truth still requires the F witness below.
    for signature, channel in pre.items():
        if (
            signature not in raw
            or json_bytes(_c_channel_projection(raw[signature]))
            != json_bytes(_c_channel_projection(channel))
        ):
            raise IntegrityError(
                "actual C pre-admission channel is not one exact raw channel"
            )
    for signature, channel in owned.items():
        if (
            signature not in raw
            or signature not in pre
            or json_bytes(_c_channel_projection(raw[signature]))
            != json_bytes(_c_channel_projection(channel))
            or json_bytes(_c_channel_projection(pre[signature]))
            != json_bytes(_c_channel_projection(channel))
        ):
            raise IntegrityError(
                "actual C owned channel has no exact raw/pre-admission lineage"
            )
    _, admitted_signatures = _normalized_direct_effect_admission(
        c_value.get("direct_effect_admission"),
        "actual C direct-effect admission",
    )
    if admitted_signatures != frozenset(owned):
        raise IntegrityError(
            "actual C direct-effect admission disagrees with owned channels"
        )
    snapshot = _cause_endpoint_snapshot(c_value, snapshot_index)
    comparison = _object(c_value.get("comparison"), "actual C comparison")
    binding = _object(c_value.get("binding"), "actual C binding")
    expected_binding_roles = {
        "played_vs_best": "primary_played_cause",
        "played_vs_alternative": "played_alternative_context",
        "best_vs_second": "candidate_set_constraint",
        "reference_vs_alternative": "alternative_diagnostic",
    }
    if binding.get("role") != expected_binding_roles.get(comparison.get("kind")):
        raise IntegrityError("actual C binding role disagrees with comparison kind")

    reference_root = str(comparison.get("reference_root_move", "")).lower()
    candidate_root = str(comparison.get("candidate_root_move", "")).lower()
    reference_identity = _line_identity(
        _object(comparison.get("reference_line"), "actual C reference line")
    )
    candidate_identity = _line_identity(
        _object(comparison.get("candidate_line"), "actual C candidate line")
    )
    if (
        reference_identity[3] != reference_root
        or candidate_identity[3] != candidate_root
    ):
        raise IntegrityError("actual C comparison lines disagree with root aliases")

    source_side = c_value.get("source_side")
    event = _object(binding.get("event_line"), "actual C event line")
    evidence = _objects(binding.get("evidence_lines"), "actual C evidence lines")
    event_identity = _line_identity(event)
    endpoint_identities = {reference_identity, candidate_identity}
    if any(_line_identity(line) not in endpoint_identities for line in evidence):
        raise IntegrityError("actual C evidence line is outside comparison endpoints")
    if not any(_line_identity(line) == event_identity for line in evidence):
        raise IntegrityError("actual C binding does not contain its event evidence")
    if (
        binding.get("source_side") != source_side
        or event_identity not in endpoint_identities
    ):
        raise IntegrityError("actual C binding has invalid source or event endpoint")

    # Context-only Causes remain observable, but cannot satisfy a direct C
    # realization merely because raw or pre-admission material exists.
    if not owned:
        return []

    if source_side == "reference":
        source_identity = reference_identity
    elif source_side == "candidate":
        source_identity = candidate_identity
    else:
        raise IntegrityError("actual C owned channel has no concrete source side")
    if event_identity != source_identity:
        raise IntegrityError("actual C owned channel borrows a sibling endpoint")

    attribution = _object(c_value.get("attribution"), "actual C attribution")
    attribution_kind = str(attribution.get("kind", ""))
    if (
        attribution_kind == "reference_creates_resource"
        and source_side != "reference"
    ) or (
        attribution_kind.startswith("candidate_") and source_side != "candidate"
    ) or (
        attribution_kind != "reference_creates_resource"
        and not attribution_kind.startswith("candidate_")
    ):
        raise IntegrityError("actual C attribution disagrees with its endpoint side")

    proof = _object(c_value.get("proof"), "actual C proof")
    if proof.get("present") is not True:
        raise IntegrityError("actual C owned direct channels require present proof")
    direct = _object(proof.get("direct"), "actual C direct proof")
    if direct.get("role") != "direct_proof":
        raise IntegrityError("actual C owned channels require a direct proof channel")
    sources = _objects(direct.get("sources"), "actual C direct proof sources")
    source_ids = [source.get("id") for source in sources]
    if (
        any(not isinstance(source_id, str) or not source_id for source_id in source_ids)
        or len(source_ids) != len(set(source_ids))
    ):
        raise IntegrityError("actual C direct proof source IDs are ambiguous")
    sources_by_id = {str(source["id"]): source for source in sources}
    for source in sources:
        _registered_source_ref(source, "actual C direct proof source")

    verified: list[dict[str, Any]] = []
    used_witnesses: set[bytes] = set()
    for signature, channel in owned.items():
        source_id = channel.get("source_id")
        carrier = _registered_source_ref(
            channel.get("carrier"), "actual C owned channel carrier"
        )
        provenance = [
            _registered_source_ref(item, "actual C owned channel provenance")
            for item in _objects(
                channel.get("provenance"), "actual C owned channel provenance"
            )
        ]
        primitive_source = _registered_source_ref(
            channel.get("primitive_proof_source"),
            "actual C owned channel primitive proof source",
        )
        evidence_identity = {
            "carrier": carrier,
            "provenance": provenance,
            "primitive_proof_source": primitive_source,
        }
        if channel.get("evidence_identity_sha256") != native_sha256_json(
            evidence_identity
        ):
            raise IntegrityError("actual C evidence identity digest is invalid")
        if carrier.get("id") != source_id:
            raise IntegrityError("actual C carrier does not identify its source")

        provenance_ids = channel.get("provenance_source_ids")
        ancestor_ids = channel.get("carrier_ancestor_source_ids")
        projected_ids = [item.get("id") for item in provenance]
        if (
            not isinstance(provenance_ids, list)
            or any(not isinstance(item, str) or not item for item in provenance_ids)
            or len(provenance_ids) != len(set(provenance_ids))
            or len(projected_ids) != len(set(projected_ids))
            or set(projected_ids) != set(provenance_ids)
            or not isinstance(ancestor_ids, list)
            or any(not isinstance(item, str) or not item for item in ancestor_ids)
            or len(ancestor_ids) != len(set(ancestor_ids))
            or not set(provenance_ids).issubset(ancestor_ids)
        ):
            raise IntegrityError("actual C channel provenance or ancestry is contradictory")

        exact_primitive_refs = [
            evidence_ref
            for evidence_ref in [carrier, *provenance]
            if json_bytes(evidence_ref) == json_bytes(primitive_source)
        ]
        if len(exact_primitive_refs) != 1:
            raise IntegrityError(
                "actual C primitive proof source is not one exact carried source"
            )

        matching_source = sources_by_id.get(str(source_id))
        if matching_source is None:
            raise IntegrityError(
                "actual C owned carrier is absent from the direct proof sources"
            )
        carrier_fields = (
            "id",
            "producer",
            "layer",
            "scope",
            "confidence",
            "line",
            "record_registered",
            "record_payload_type",
            "record_sha256",
        )
        if json_bytes({name: carrier.get(name) for name in carrier_fields}) != json_bytes(
            {name: matching_source.get(name) for name in carrier_fields}
        ):
            raise IntegrityError(
                "actual C carrier metadata disagrees with direct source"
            )

        line = _object(channel.get("line"), "actual C owned channel line")
        if (
            channel.get("line_id") != line.get("line_id")
            or _line_identity(line) != event_identity
        ):
            raise IntegrityError(
                "actual C owned channel line disagrees with its event endpoint"
            )
        for evidence_ref in [carrier, *provenance, primitive_source]:
            evidence_line = evidence_ref.get("line")
            if evidence_line is not None and _line_identity(
                _object(evidence_line, "actual C evidence line")
            ) not in endpoint_identities:
                raise IntegrityError(
                    "actual C evidence line is outside comparison endpoints"
                )
        if channel.get("proof_role") != "direct_proof":
            raise IntegrityError("actual C owned channel is not direct proof")

        projection, matches = _matching_endpoint_witnesses(
            channel,
            str(source_side),
            snapshot,
            f"actual C owned channel {signature}",
        )
        if len(matches) != 1:
            raise IntegrityError(
                "actual C owned channel does not match exactly one neutral endpoint witness"
            )
        witness_key = json_bytes(projection)
        if witness_key in used_witnesses:
            raise IntegrityError(
                "actual C owned channels ambiguously reuse one neutral endpoint witness"
            )
        used_witnesses.add(witness_key)
        verified.append(channel)

    return verified


def _pre_admission_endpoint_channels(
    cause: Mapping[str, Any],
    snapshot_index: Mapping[str, Mapping[str, Any]],
) -> list[dict[str, Any]]:
    """Classify admission loss without granting raw/pre channels C authority."""

    c_value = _object(cause.get("c"), "actual Cause C state")
    raw, pre, _ = _c_channel_layers(cause)
    snapshot = _cause_endpoint_snapshot(c_value, snapshot_index)
    source_side = c_value.get("source_side")
    if source_side not in {"reference", "candidate"}:
        return []
    side = snapshot[source_side]
    witnessed: dict[bytes, dict[str, Any]] = {}
    for channel in [*pre.values(), *raw.values()]:
        try:
            projection = _channel_neutral_projection(
                channel,
                str(source_side),
                "actual C diagnostic pre-admission channel",
            )
        except (ContractError, IntegrityError):
            continue
        if any(
            json_bytes(item["projection"]) == json_bytes(projection)
            for item in side["witnesses"]
        ):
            witnessed.setdefault(json_bytes(projection), channel)
    return list(witnessed.values())


def _c_semantics_matches_realization(
    c_semantics: Mapping[str, Any],
    verified_channels: Sequence[Mapping[str, Any]],
    realization: Mapping[str, Any],
) -> bool:
    attribution = c_semantics.get("attribution")
    comparison = c_semantics.get("comparison")
    if not isinstance(attribution, Mapping):
        return False
    if not isinstance(comparison, Mapping):
        return False
    if c_semantics.get("cause_kind") != realization.get("cause_kind"):
        return False
    if c_semantics.get("source_side") != realization.get("source_side"):
        return False
    if attribution.get("kind") != realization.get("attribution_kind"):
        return False
    expected = _object(realization.get("comparison"), "oracle comparison")
    if (
        comparison.get("kind") != expected.get("kind")
        or comparison.get("mover") != expected.get("mover")
        or str(comparison.get("reference_root_move", "")).lower()
        != str(expected.get("reference_root_move", "")).lower()
        or str(comparison.get("candidate_root_move", "")).lower()
        != str(expected.get("candidate_root_move", "")).lower()
        or str(comparison.get("event_role", "")).lower()
        != str(expected.get("event_role", "")).lower()
        or str(comparison.get("event_root_move", "")).lower()
        != str(expected.get("event_root_move", "")).lower()
    ):
        return False
    return _channels_exact_match(
        verified_channels,
        _objects(realization.get("channels"), "oracle channels"),
        require_played_change=False,
    )


def _c_value_matches_realization(
    c_value: Mapping[str, Any],
    verified_channels: Sequence[Mapping[str, Any]],
    realization: Mapping[str, Any],
) -> bool:
    attribution = c_value.get("attribution")
    comparison = c_value.get("comparison")
    binding = c_value.get("binding")
    if not all(isinstance(value, Mapping) for value in (attribution, comparison, binding)):
        return False
    event = binding.get("event_line")
    if not isinstance(event, Mapping):
        return False
    return _c_semantics_matches_realization(
        {
            "cause_kind": c_value.get("kind"),
            "source_side": c_value.get("source_side"),
            "attribution": {"kind": attribution.get("kind")},
            "comparison": {
                "kind": comparison.get("kind"),
                "mover": comparison.get("mover"),
                "reference_root_move": comparison.get("reference_root_move"),
                "candidate_root_move": comparison.get("candidate_root_move"),
                "event_role": event.get("role"),
                "event_root_move": event.get("root_move"),
            },
        },
        verified_channels,
        realization,
    )


def _c_header_matches_realization(
    c_value: Mapping[str, Any], realization: Mapping[str, Any]
) -> bool:
    """Match the C claim header without treating any channel as admitted truth."""

    attribution = c_value.get("attribution")
    comparison = c_value.get("comparison")
    binding = c_value.get("binding")
    if not all(
        isinstance(value, Mapping) for value in (attribution, comparison, binding)
    ):
        return False
    event = binding.get("event_line")
    if not isinstance(event, Mapping):
        return False
    expected = _object(realization.get("comparison"), "oracle comparison")
    return bool(
        c_value.get("kind") == realization.get("cause_kind")
        and c_value.get("source_side") == realization.get("source_side")
        and attribution.get("kind") == realization.get("attribution_kind")
        and comparison.get("kind") == expected.get("kind")
        and comparison.get("mover") == expected.get("mover")
        and str(comparison.get("reference_root_move", "")).lower()
        == str(expected.get("reference_root_move", "")).lower()
        and str(comparison.get("candidate_root_move", "")).lower()
        == str(expected.get("candidate_root_move", "")).lower()
        and _line_matches(
            event,
            {
                "role": expected.get("event_role"),
                "root_move": expected.get("event_root_move"),
            },
        )
    )


def _neutral_witness_matches_oracle_channel(
    witness: Mapping[str, Any], expected: Mapping[str, Any]
) -> bool:
    value = _object(witness.get("value"), "neutral endpoint witness")
    if not _actor_matches(
        value.get("actor"), _object(expected.get("actor"), "oracle actor")
    ):
        return False
    for singular, plural in (
        ("target", "targets"),
        ("mechanism", "mechanisms"),
        ("consequence", "consequences"),
    ):
        if not _object_constraint_matches(
            value.get(singular),
            _object(expected.get(plural), f"oracle {plural}"),
        ):
            return False
    return bool(
        _line_matches(value.get("line"), _object(expected.get("line"), "oracle line"))
        and value.get("horizon") == expected.get("horizon")
    )


def _neutral_witnesses_cover_oracle_channels(
    witnesses: Sequence[Mapping[str, Any]],
    expected_channels: Sequence[Mapping[str, Any]],
) -> bool:
    if not expected_channels:
        return False
    edges = {
        expected_index: [
            witness_index
            for witness_index, witness in enumerate(witnesses)
            if _neutral_witness_matches_oracle_channel(witness, expected)
        ]
        for expected_index, expected in enumerate(expected_channels)
    }
    owner: dict[int, int] = {}

    def assign(expected_index: int, visited: set[int]) -> bool:
        for witness_index in edges[expected_index]:
            if witness_index in visited:
                continue
            visited.add(witness_index)
            previous = owner.get(witness_index)
            if previous is None or assign(previous, visited):
                owner[witness_index] = expected_index
                return True
        return False

    return all(assign(index, set()) for index in range(len(expected_channels)))


def _snapshot_supports_realization(
    snapshot: Mapping[str, Any], realization: Mapping[str, Any]
) -> bool:
    comparison = _object(snapshot.get("comparison"), "endpoint snapshot comparison")
    expected = _object(realization.get("comparison"), "oracle comparison")
    source_side = realization.get("source_side")
    attribution_kind = str(realization.get("attribution_kind", ""))
    if source_side not in {"reference", "candidate"}:
        return False
    if (
        (attribution_kind == "reference_creates_resource" and source_side != "reference")
        or (attribution_kind.startswith("candidate_") and source_side != "candidate")
        or (
            attribution_kind != "reference_creates_resource"
            and not attribution_kind.startswith("candidate_")
        )
    ):
        return False
    side = _object(snapshot.get(str(source_side)), "endpoint snapshot side")
    side_line = _object(side.get("line"), "endpoint snapshot side line")
    if not (
        comparison.get("kind") == expected.get("kind")
        and comparison.get("mover") == expected.get("mover")
        and str(comparison.get("reference_root_move", "")).lower()
        == str(expected.get("reference_root_move", "")).lower()
        and str(comparison.get("candidate_root_move", "")).lower()
        == str(expected.get("candidate_root_move", "")).lower()
        and _line_matches(
            side_line,
            {
                "role": expected.get("event_role"),
                "root_move": expected.get("event_root_move"),
            },
        )
    ):
        return False
    return _neutral_witnesses_cover_oracle_channels(
        side["witnesses"],
        _objects(realization.get("channels"), "oracle channels"),
    )


def _selection_realization_matches(
    selection: Mapping[str, Any],
    realization: Mapping[str, Any],
    c_value: Mapping[str, Any],
    verified_channels: Sequence[Mapping[str, Any]],
) -> bool:
    attribution = selection.get("attribution")
    if not isinstance(attribution, Mapping):
        return False
    if selection.get("cause_kind") != realization.get("cause_kind"):
        return False
    if selection.get("source_side") != realization.get("source_side"):
        return False
    if selection.get("effect_mode") != realization.get("effect_mode"):
        return False
    if attribution.get("kind") != realization.get("attribution_kind"):
        return False
    if attribution.get("root_move_matched") is not True:
        return False
    if attribution.get("direct_proof_eligible") is not True:
        return False
    if not _selection_comparison_matches(selection, realization):
        return False
    if not _selection_is_bound_to_verified_c(
        selection, c_value, verified_channels
    ):
        return False
    actual_channels = _objects(selection.get("channels"), "R-native channels")
    expected_channels = _objects(realization.get("channels"), "oracle channels")
    return _channels_exact_match(
        actual_channels,
        expected_channels,
        require_played_change=True,
    )


def _selection_evidence_ref_projection(value: Any, label: str) -> dict[str, Any]:
    evidence_ref = _object(value, label)
    line = evidence_ref.get("line")
    projected_line: dict[str, Any] | None
    if line is None:
        projected_line = None
    else:
        exact_line = _object(line, f"{label} line")
        projected_line = {
            "line_id": exact_line.get("line_id", exact_line.get("id")),
            "role": exact_line.get("role"),
            "rank": exact_line.get("rank"),
            "root_move": exact_line.get("root_move"),
        }
    return {
        "id": evidence_ref.get("id"),
        "producer": evidence_ref.get("producer"),
        "layer": evidence_ref.get("layer"),
        "scope": evidence_ref.get("scope"),
        "line": projected_line,
    }


def _sorted_object_projection(value: Any, label: str) -> list[dict[str, Any]]:
    result = [dict(item) for item in _objects(value, label)]
    result.sort(key=json_bytes)
    return result


def _channel_selection_projection(
    channel: Mapping[str, Any], *, c_stage: bool
) -> dict[str, Any]:
    label = "verified C" if c_stage else "R-native"
    provenance = [
        _selection_evidence_ref_projection(item, f"{label} channel provenance")
        for item in _objects(channel.get("provenance"), f"{label} channel provenance")
    ]
    provenance.sort(key=json_bytes)
    result = {
        "carrier": _selection_evidence_ref_projection(
            channel.get("carrier"), f"{label} channel carrier"
        ),
        "provenance": provenance,
        "causal_signature": channel.get("causal_signature"),
        "direct_change": channel.get("direct_change"),
        "line": channel.get("line"),
        "horizon": channel.get("horizon"),
        "proof_segment": channel.get("proof_segment"),
        "effect_descriptor": channel.get("effect_descriptor"),
        "importance_effect": channel.get("importance_effect"),
        "descriptor_ambiguous": channel.get(
            "importance_descriptor_ambiguous" if c_stage else "descriptor_ambiguous"
        ),
        "proof_segment_ambiguous": channel.get("proof_segment_ambiguous"),
    }
    for c_name, selection_name in (
        ("actor", "actor"),
        ("target", "targets"),
        ("mechanism", "mechanisms"),
        ("consequence", "consequences"),
        ("witness", "witnesses"),
    ):
        source_name = c_name if c_stage else selection_name
        result[selection_name] = _sorted_object_projection(
            channel.get(source_name), f"{label} {selection_name}"
        )
    return result


def _selection_is_bound_to_verified_c(
    selection: Mapping[str, Any],
    c_value: Mapping[str, Any],
    verified_channels: Sequence[Mapping[str, Any]],
) -> bool:
    if _normalized_direct_effect_admission(
        selection.get("direct_effect_admission"), "R direct-effect admission"
    ) != _normalized_direct_effect_admission(
        c_value.get("direct_effect_admission"), "actual C direct-effect admission"
    ):
        return False
    by_signature: dict[str, Mapping[str, Any]] = {}
    for channel in verified_channels:
        signature = channel.get("causal_signature")
        if not isinstance(signature, str) or not signature or signature in by_signature:
            return False
        by_signature[signature] = channel
    selected_channels = _objects(selection.get("channels"), "R-native channels")
    consumed: set[str] = set()
    for selected in selected_channels:
        signature = selected.get("causal_signature")
        if not isinstance(signature, str) or signature in consumed:
            return False
        owned = by_signature.get(signature)
        if owned is None or json_bytes(
            _channel_selection_projection(selected, c_stage=False)
        ) != json_bytes(
            _channel_selection_projection(owned, c_stage=True)
        ):
            return False
        consumed.add(signature)
    return bool(consumed)


def _meaning_edges(
    meanings: Sequence[Mapping[str, Any]],
    causes: Sequence[Mapping[str, Any]],
    verified: Mapping[str, Sequence[Mapping[str, Any]]],
) -> tuple[dict[str, list[str]], dict[tuple[str, str], list[dict[str, Any]]]]:
    edges: dict[str, list[str]] = {}
    realizations: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for meaning in meanings:
        meaning_id = str(meaning["meaning_id"])
        matches: list[str] = []
        for cause in causes:
            cause_id = _cause_id(cause)
            matching = [
                dict(realization)
                for realization in _objects(meaning.get("realizations"), "oracle realizations")
                if _c_value_matches_realization(cause["c"], verified[cause_id], realization)
            ]
            if matching:
                matches.append(cause_id)
                realizations[(meaning_id, cause_id)] = matching
        edges[meaning_id] = sorted(matches)
    return edges, realizations


def _maximum_matching(
    meaning_ids: Sequence[str], edges: Mapping[str, Sequence[str]]
) -> dict[str, str]:
    cause_owner: dict[str, str] = {}

    def assign(meaning_id: str, visited: set[str]) -> bool:
        for cause_id in edges.get(meaning_id, ()):  # deterministic sorted input
            if cause_id in visited:
                continue
            visited.add(cause_id)
            previous = cause_owner.get(cause_id)
            if previous is None or assign(previous, visited):
                cause_owner[cause_id] = meaning_id
                return True
        return False

    for meaning_id in meaning_ids:
        assign(meaning_id, set())
    return {meaning_id: cause_id for cause_id, meaning_id in cause_owner.items()}


def _add_error(
    errors: list[dict[str, Any]],
    code: str,
    stage: str,
    *,
    meaning_id: str | None = None,
    cause_evidence_id: str | None = None,
) -> None:
    item: dict[str, Any] = {"code": code, "stage": stage}
    if meaning_id is not None:
        item["meaning_id"] = meaning_id
    if cause_evidence_id is not None:
        item["cause_evidence_id"] = cause_evidence_id
    if item not in errors:
        errors.append(item)


def _linked_claim_ids(cause: Mapping[str, Any]) -> set[str]:
    value = cause.get("jp")
    claims = value.get("linked_claims") if isinstance(value, Mapping) else None
    if not isinstance(claims, list):
        return set()
    cause_id = _cause_id(cause)
    return {
        str(item["id"])
        for item in claims
        if isinstance(item, Mapping)
        and isinstance(item.get("id"), str)
        and item.get("link_kind") == "direct_evidence"
        and isinstance(item.get("evidence_ids"), list)
        and cause_id in item["evidence_ids"]
    }


def _linked_at_jp(cause: Mapping[str, Any]) -> bool:
    return bool(_linked_claim_ids(cause))


def _admitted_claim_ids(cause: Mapping[str, Any]) -> set[str]:
    linked = _linked_claim_ids(cause)
    value = cause.get("ja")
    decisions = value.get("linked_decisions") if isinstance(value, Mapping) else None
    if not isinstance(decisions, list):
        return set()
    return {
        str(item["claim_id"])
        for item in decisions
        if isinstance(item, Mapping)
        and isinstance(item.get("claim_id"), str)
        and item.get("claim_id") in linked
        and item.get("status") in {"certified", "admitted"}
    }


def _linked_at_ja(cause: Mapping[str, Any]) -> bool:
    return bool(_admitted_claim_ids(cause))


def _selection_uses_admitted_host(
    selection: Mapping[str, Any], cause: Mapping[str, Any]
) -> bool:
    host = selection.get("host")
    return bool(
        isinstance(host, Mapping)
        and isinstance(host.get("claim_id"), str)
        and host.get("claim_id") in _admitted_claim_ids(cause)
    )


def _r_selection(cause: Mapping[str, Any]) -> dict[str, Any] | None:
    value = cause.get("r")
    selection = value.get("native_selection") if isinstance(value, Mapping) else None
    return dict(selection) if isinstance(selection, Mapping) else None


def _r_has_selectable_cross_comparison_authority(
    cause: Mapping[str, Any],
) -> bool:
    """Return whether this Cause has retained cross-comparison authority.

    Ja admission and diagnostic R rows do not carry player-facing comparison
    authority. V3 only records cross-comparison decisions retained by the
    central fallback pass, so this is a fallback signal when the fallback
    Cause itself is unavailable, not a reconstruction of provisional R state.
    """

    r_value = cause.get("r")
    decision = (
        r_value.get("cross_comparison_exposure")
        if isinstance(r_value, Mapping)
        else None
    )
    if not isinstance(decision, Mapping):
        return False
    if decision.get("status") not in {
        "selected_primary",
        "selected_complementary",
    } or decision.get("selected") is not True:
        return False
    comparison_rank = decision.get("comparison_exposure_rank")
    if (
        not isinstance(comparison_rank, int)
        or isinstance(comparison_rank, bool)
        or comparison_rank < 0
    ):
        return False
    effect_mode = decision.get("effect_mode")
    if effect_mode not in {
        "played_liability",
        "alternative_resource",
        "played_value",
    }:
        return False
    record = cause.get("cause_record")
    cause_id = record.get("id") if isinstance(record, Mapping) else None
    return bool(
        isinstance(cause_id, str)
        and cause_id
        and decision.get("representative_cause_evidence_id") == cause_id
    )


def _active_fallback_dominator(
    meaning: Mapping[str, Any],
    assignment: Mapping[str, str],
    cause_by_id: Mapping[str, Mapping[str, Any]],
) -> bool:
    """Return whether a more specific meaning reached selectable R authority.

    The fallback Cause's own central dominance decision is authoritative when
    that Cause was generated. Its exact dominating IDs preserve intermediate
    dominance chains that are absent from retained cross-comparison rows. If
    the fallback Cause was not generated, an unambiguous, self-represented V3
    cross-comparison selection is the only available signal. Missing or
    contradictory observations fail closed and keep the fallback obligation.
    """

    declared_dominator_ids = {
        assignment[dominator]
        for dominator in meaning.get("dominated_by", [])
        if dominator in assignment
        and assignment[dominator] in cause_by_id
        and _linked_at_ja(cause_by_id[assignment[dominator]])
    }
    meaning_id = meaning.get("meaning_id")
    if isinstance(meaning_id, str) and meaning_id in assignment:
        fallback = cause_by_id.get(assignment[meaning_id])
        r_value = fallback.get("r") if isinstance(fallback, Mapping) else None
        decision = (
            r_value.get("fallback_dominance")
            if isinstance(r_value, Mapping)
            else None
        )
        if not isinstance(decision, Mapping):
            return False
        raw_ids = decision.get("dominating_cause_evidence_ids")
        if (
            not isinstance(raw_ids, list)
            or not all(isinstance(item, str) and item for item in raw_ids)
            or len(raw_ids) != len(set(raw_ids))
        ):
            return False
        if (
            decision.get("status") == "retained"
            and decision.get("retained") is True
            and not raw_ids
        ):
            return False
        if not (
            decision.get("status") == "dominated_fallback"
            and decision.get("retained") is False
            and raw_ids
        ):
            return False
        return bool(declared_dominator_ids.intersection(raw_ids))

    return any(
        cause_id in declared_dominator_ids
        and _r_has_selectable_cross_comparison_authority(cause_by_id[cause_id])
        for cause_id in declared_dominator_ids
    )


def _requires_forward_survival(
    meaning: Mapping[str, Any],
    assignment: Mapping[str, str],
    cause_by_id: Mapping[str, Mapping[str, Any]],
) -> bool:
    """Keep C generation policy independent from Jp/Ja/R exposure policy."""

    policy = meaning.get("exposure_policy")
    if policy in {"primary", "complementary"}:
        return True
    if policy == "fallback_only":
        return not _active_fallback_dominator(meaning, assignment, cause_by_id)
    return False


def _selection_multiset(values: Sequence[Mapping[str, Any]]) -> Counter[str]:
    return Counter(sha256_json(dict(item)) for item in values)


def _unique_nonempty_strings(value: Any, label: str) -> list[str]:
    if not isinstance(value, list) or not all(
        isinstance(item, str) and item for item in value
    ):
        raise ContractError(f"{label} must be a string array")
    values = [str(item) for item in value]
    if len(values) != len(set(values)):
        raise ContractError(f"{label} has duplicates")
    return values


def _semantic_root_board(case: Mapping[str, Any]) -> str:
    fields = str(case.get("fen", "")).strip().split()
    if len(fields) < 4:
        raise ContractError("cause-audit case has no semantic root board")
    return " ".join(fields[:4])


def _root_board_mover(root_board: str) -> str:
    fields = root_board.split()
    if len(fields) < 2 or fields[1] not in {"w", "b"}:
        raise ContractError("importance universe has no valid side to move")
    return "white" if fields[1] == "w" else "black"


def _selected_actor_side(channel: Mapping[str, Any]) -> str:
    actor = _objects(channel.get("actor"), "selected importance channel actor")
    sides = {
        str(item.get("key")).lower()
        for item in actor
        if item.get("kind") == "side" and item.get("key") in {"white", "black"}
    }
    if len(sides) != 1:
        raise ContractError("selected importance channel has no exact actor side")
    return next(iter(sides))


def _expected_played_change(effect_mode: str, direct_change: str) -> str | None:
    if effect_mode == "alternative_resource":
        return "missed" if direct_change in {"occurred", "prevented", "maintained"} else None
    if effect_mode == "played_liability":
        return direct_change if direct_change in {"occurred", "lost", "refuted", "missed"} else None
    if effect_mode == "played_value":
        return direct_change if direct_change in {"occurred", "prevented", "maintained"} else None
    return None


def _opposite_side(side: str) -> str:
    if side == "white":
        return "black"
    if side == "black":
        return "white"
    raise ContractError("importance frame has an invalid side")


def _expected_player_facing_impact(frame: Mapping[str, Any]) -> str:
    effect_mode = str(frame.get("effect_mode"))
    actor = str(frame.get("actor"))
    direct_change = str(frame.get("direct_change"))
    played_change = str(frame.get("played_change"))
    if _expected_played_change(effect_mode, direct_change) != played_change:
        raise ContractError("importance frame has an invalid played-facing change")
    stake = str(frame.get("stake"))
    try:
        stake_kind, stake_side = stake.split(":", 1)
    except ValueError as error:
        raise ContractError("importance frame has an invalid effect stake") from error
    if stake_side not in {"white", "black"} or stake_kind not in {
        "benefits",
        "harms",
        "preserves",
    }:
        raise ContractError("importance frame has an invalid effect stake")
    favored = (
        stake_side
        if stake_kind == "benefits"
        else _opposite_side(stake_side)
        if stake_kind == "harms"
        else None
    )
    if effect_mode == "alternative_resource" and (
        favored == actor or (stake_kind == "preserves" and stake_side == actor)
    ):
        return f"harms-reviewed-mover:{actor}"
    if effect_mode == "played_liability":
        actor_value = favored == actor or (
            stake_kind == "preserves" and stake_side == actor
        )
        actor_value_denied = actor_value and direct_change in {
            "lost",
            "refuted",
            "missed",
        }
        if favored == _opposite_side(actor) or actor_value_denied:
            return f"harms-reviewed-mover:{actor}"
    if effect_mode == "played_value" and favored == actor:
        return f"benefits-reviewed-mover:{actor}"
    if (
        effect_mode == "played_value"
        and stake_kind == "preserves"
        and stake_side == actor
    ):
        return f"preserves-reviewed-mover:{actor}"
    raise ContractError("importance frame cannot produce its player-facing impact")


def _line_stable_key(line: Mapping[str, Any]) -> str:
    role_names = {
        "played": "Played",
        "best_reference": "BestReference",
        "alternative": "Alternative",
        "threat": "Threat",
    }
    role = role_names.get(str(line.get("role")))
    rank = line.get("rank")
    root_move = line.get("root_move")
    if role is None or not isinstance(rank, int) or isinstance(rank, bool) or not isinstance(
        root_move, str
    ):
        raise ContractError("selected importance channel has an invalid semantic line")
    return f"{role}:{rank}:{root_move.lower()}"


def _importance_domain_ready(profile: Mapping[str, Any]) -> None:
    domain_kind = profile.get("domain_kind")
    domain = profile.get("domain")
    measure = _object(profile.get("measure"), "importance profile measure")
    measure_kind = measure.get("kind")
    if not isinstance(domain, str):
        raise ContractError("importance profile has an invalid typed domain")
    frame = _object(profile.get("frame"), "importance profile frame")
    stake = str(frame.get("stake"))
    scope = _object(profile.get("effect_scope"), "importance profile effect scope")
    targets = _unique_nonempty_strings(
        scope.get("target_signatures"), "importance effect target signatures"
    )
    def exact_integer(field: str, *, nonnegative: bool = False) -> bool:
        value = measure.get(field)
        return (
            isinstance(value, int)
            and not isinstance(value, bool)
            and (not nonnegative or value >= 0)
        )

    if domain_kind == "board_mate":
        ready = (
            domain == "board-mate"
            and measure_kind == "mate_arrival"
            and set(measure) == {"kind", "ply_offset"}
            and exact_integer("ply_offset", nonnegative=True)
            and stake.startswith("benefits:")
        )
    elif domain_kind == "material":
        ready = (
            domain == "material"
            and measure_kind == "material_outcome"
            and set(measure) == {
                "kind",
                "durable_net_cp",
                "onset_ply_offset",
            }
            and exact_integer("durable_net_cp", nonnegative=True)
            and int(measure["durable_net_cp"]) > 0
            and exact_integer("onset_ply_offset", nonnegative=True)
            and stake.startswith("benefits:")
        )
    elif domain_kind == "threat":
        parts = domain.split(":", 2)
        ready = (
            len(parts) == 3
            and parts[0] == "threat"
            and parts[1] in {"mate", "material", "positional"}
            and bool(targets)
            and parts[2] == "|".join(targets)
            and measure_kind == "threat_horizon"
            and set(measure) == {"kind", "turns_to_impact"}
            and exact_integer("turns_to_impact", nonnegative=True)
            and int(measure["turns_to_impact"]) > 0
            and (
                stake.startswith("benefits:")
                or stake.startswith("preserves:")
            )
        )
    elif domain_kind == "structural":
        parts = domain.split(":")
        origin = parts[1] if len(parts) == 5 else ""
        consequence = parts[2] if len(parts) == 5 else ""
        polarity = parts[3] if len(parts) == 5 else ""
        robustness = parts[4] if len(parts) == 5 else ""
        allowed_robustness = {
            "untested",
            "deferred",
            "superseded",
            "refuted",
            "conditional",
            "robust",
        }
        expected_stake = (
            "preserves:"
            if origin == "planrestriction"
            else "benefits:"
            if polarity == "gain"
            else "harms:"
            if polarity == "loss"
            else ""
        )
        ready = (
            len(parts) == 5
            and parts[0] == "structural"
            and origin in {"roottransition", "planresult", "planrestriction"}
            and consequence.isalnum()
            and bool(consequence)
            and polarity in {"gain", "loss"}
            and (
                (origin == "planresult" and robustness in allowed_robustness)
                or (origin != "planresult" and robustness == "none")
            )
            and measure_kind == "structural_strength"
            and set(measure) == {"kind", "units"}
            and exact_integer("units", nonnegative=True)
            and int(measure["units"]) > 0
            and bool(expected_stake)
            and stake.startswith(expected_stake)
        )
    else:
        ready = False
    if not ready:
        raise ContractError("importance profile domain and measure are inconsistent")


def _validate_material_event_salience(
    descriptor: Mapping[str, Any],
    measure: Mapping[str, Any],
) -> None:
    salience_value = descriptor.get("material_event_salience")
    if measure.get("kind") != "material_outcome":
        if salience_value is not None:
            raise ContractError(
                "non-material importance descriptor carries material event salience"
            )
        return
    salience = _object(salience_value, "material event salience")
    if set(salience) != {
        "move_uci",
        "ply_offset",
        "captured_role",
        "square",
        "target_value_cp",
    }:
        raise ContractError("material event salience has an invalid shape")
    move_uci = salience.get("move_uci")
    captured_role = salience.get("captured_role")
    square = salience.get("square")
    target_value_cp = salience.get("target_value_cp")
    ply_offset = salience.get("ply_offset")
    try:
        if not isinstance(move_uci, str):
            raise ValueError
        chess.Move.from_uci(move_uci)
        if not isinstance(square, str):
            raise ValueError
        chess.parse_square(square)
    except ValueError as error:
        raise ContractError("material event salience has invalid move geometry") from error
    if (
        not isinstance(captured_role, str)
        or not captured_role.strip()
        or not isinstance(target_value_cp, int)
        or isinstance(target_value_cp, bool)
        or target_value_cp <= 0
        or not isinstance(ply_offset, int)
        or isinstance(ply_offset, bool)
        or ply_offset < 0
        or ply_offset != measure.get("onset_ply_offset")
    ):
        raise ContractError("material event salience disagrees with its durable outcome")


def _effect_scope_stable_key(scope: Mapping[str, Any]) -> str:
    primitive = str(scope.get("primitive_kind", "")).replace("_", "")
    targets = _unique_nonempty_strings(
        scope.get("target_signatures"), "importance effect target signatures"
    )
    plans = _unique_nonempty_strings(scope.get("plan_ids"), "importance effect plan ids")
    axes = _objects(scope.get("strategic_axes"), "importance strategic axes")
    axis_keys: list[str] = []
    for axis in axes:
        outcome = axis.get("comparison_outcome")
        if outcome is not None and not isinstance(outcome, str):
            raise ContractError("importance strategic axis has an invalid outcome")
        axis_keys.append(
            ":".join(
                (
                    str(axis.get("kind", "")).replace("_", ""),
                    str(axis.get("polarity", "")).replace("_", ""),
                    str(axis.get("label", "")),
                    outcome.replace("_", "") if isinstance(outcome, str) else "none",
                )
            )
        )
    return (
        f"{primitive}|[{','.join(targets)}]|[{','.join(plans)}]|"
        f"[{','.join(axis_keys)}]"
    )


def _importance_relation(
    left: Mapping[str, Any], right: Mapping[str, Any]
) -> tuple[str, str | None]:
    left_universe = _object(left.get("universe"), "left importance universe")
    right_universe = _object(right.get("universe"), "right importance universe")
    if left_universe != right_universe:
        return "incomparable", None
    left_measure = _object(left.get("measure"), "left importance measure")
    right_measure = _object(right.get("measure"), "right importance measure")
    left_terminal = (
        left.get("domain_kind") == "board_mate"
        and left_measure.get("kind") == "mate_arrival"
    )
    right_terminal = (
        right.get("domain_kind") == "board_mate"
        and right_measure.get("kind") == "mate_arrival"
    )
    if left_terminal != right_terminal:
        return (
            "dominates" if left_terminal else "dominated_by",
            "terminal:board-mate",
        )
    if left.get("domain") != right.get("domain"):
        return "incomparable", None
    left_scope = _object(left.get("effect_scope"), "left importance effect scope")
    right_scope = _object(right.get("effect_scope"), "right importance effect scope")
    if left.get("domain_kind") == "structural":
        unscoped = {
            "primitive_kind": "unspecified",
            "target_signatures": [],
            "plan_ids": [],
            "strategic_axes": [],
        }
        if left_scope == unscoped or right_scope == unscoped or left_scope != right_scope:
            return "incomparable", None
    left_kind = left_measure.get("kind")
    right_kind = right_measure.get("kind")
    if left_kind != right_kind:
        return "incomparable", None
    relation: str
    if left_kind == "mate_arrival":
        left_value = int(left_measure["ply_offset"])
        right_value = int(right_measure["ply_offset"])
        relation = "dominates" if left_value < right_value else "dominated_by" if left_value > right_value else "tied"
    elif left_kind == "material_outcome":
        left_cp = int(left_measure["durable_net_cp"])
        right_cp = int(right_measure["durable_net_cp"])
        left_ply = int(left_measure["onset_ply_offset"])
        right_ply = int(right_measure["onset_ply_offset"])
        if left_cp == right_cp and left_ply == right_ply:
            relation = "tied"
        elif left_cp >= right_cp and left_ply <= right_ply:
            relation = "dominates"
        elif right_cp >= left_cp and right_ply <= left_ply:
            relation = "dominated_by"
        else:
            return "incomparable", None
    elif left_kind == "threat_horizon":
        left_value = int(left_measure["turns_to_impact"])
        right_value = int(right_measure["turns_to_impact"])
        relation = "dominates" if left_value < right_value else "dominated_by" if left_value > right_value else "tied"
    elif left_kind == "structural_strength":
        left_value = int(left_measure["units"])
        right_value = int(right_measure["units"])
        relation = "dominates" if left_value > right_value else "dominated_by" if left_value < right_value else "tied"
    else:
        return "incomparable", None
    domain = str(left["domain"])
    if left.get("domain_kind") == "structural":
        domain = f"{domain}|effect:{_effect_scope_stable_key(left_scope)}"
    return relation, domain


def _expected_importance_relations(
    profiles: Sequence[Mapping[str, Any]],
) -> list[dict[str, Any]]:
    ordered = sorted(
        profiles,
        key=lambda item: (str(item["cause_evidence_id"]), str(item["causal_signature"])),
    )
    expected: list[dict[str, Any]] = []
    for index, left in enumerate(ordered):
        for right in ordered[index + 1 :]:
            if left["cause_evidence_id"] == right["cause_evidence_id"]:
                continue
            relation, domain = _importance_relation(left, right)
            expected.append(
                {
                    "left_cause_id": left["cause_evidence_id"],
                    "left_causal_signature": left["causal_signature"],
                    "right_cause_id": right["cause_evidence_id"],
                    "right_causal_signature": right["causal_signature"],
                    "relation": relation,
                    "domain": domain,
                }
            )
    return expected


def _validate_runtime_relations(
    runtime: Sequence[Mapping[str, Any]], expected: Sequence[Mapping[str, Any]]
) -> None:
    expected_by_pair = {
        (
            str(item["left_cause_id"]),
            str(item["left_causal_signature"]),
            str(item["right_cause_id"]),
            str(item["right_causal_signature"]),
        ): dict(item)
        for item in expected
    }
    expected_unordered = {
        frozenset(((key[0], key[1]), (key[2], key[3]))): key
        for key in expected_by_pair
    }
    seen: set[frozenset[tuple[str, str]]] = set()
    for raw in runtime:
        relation = dict(raw)
        left = (str(relation.get("left_cause_id")), str(relation.get("left_causal_signature")))
        right = (str(relation.get("right_cause_id")), str(relation.get("right_causal_signature")))
        pair = frozenset((left, right))
        if len(pair) != 2 or pair not in expected_unordered:
            raise ContractError("R-native importance has an extra profile relation")
        if pair in seen:
            raise ContractError("R-native importance has a duplicate profile relation")
        seen.add(pair)
        expected_key = expected_unordered[pair]
        observed_key = (left[0], left[1], right[0], right[1])
        if observed_key != expected_key:
            raise ContractError("R-native importance relation direction is not canonical")
        if relation != expected_by_pair[expected_key]:
            raise ContractError("R-native importance relation differs from profile measures")
    if seen != set(expected_unordered):
        raise ContractError("R-native importance is missing a profile relation")


def _expected_importance_decisions(
    selected_channels: Mapping[str, Sequence[str]],
    profiles: Sequence[Mapping[str, Any]],
    relations: Sequence[Mapping[str, Any]],
) -> list[dict[str, Any]]:
    profile_keys = {
        (str(item["cause_evidence_id"]), str(item["causal_signature"]))
        for item in profiles
    }
    decisions: list[dict[str, Any]] = []
    for cause_id, signatures in selected_channels.items():
        measured = [signature for signature in signatures if (cause_id, signature) in profile_keys]
        unmeasured = [signature for signature in signatures if (cause_id, signature) not in profile_keys]
        dominators_by_signature: dict[str, set[str]] = {
            signature: set() for signature in measured
        }
        for relation in relations:
            if relation["right_cause_id"] == cause_id and relation["relation"] == "dominates":
                dominators_by_signature[str(relation["right_causal_signature"])].add(
                    str(relation["left_cause_id"])
                )
            elif relation["left_cause_id"] == cause_id and relation["relation"] == "dominated_by":
                dominators_by_signature[str(relation["left_causal_signature"])].add(
                    str(relation["right_cause_id"])
                )
        common_dominators = (
            set(dominators_by_signature[measured[0]]) if measured else set()
        )
        for signature in measured[1:]:
            common_dominators.intersection_update(
                dominators_by_signature[signature]
            )
        dominators = sorted(common_dominators)
        fully_measured = bool(measured and not unmeasured)
        decisions.append(
            {
                "cause_evidence_id": cause_id,
                "measured_channel_signatures": measured,
                "unmeasured_channel_signatures": unmeasured,
                "dominating_cause_ids": dominators,
                "dominated_within_domain": bool(
                    fully_measured and common_dominators
                ),
                "fully_measured": fully_measured,
            }
        )
    return decisions


def _normalized_decision(value: Mapping[str, Any]) -> dict[str, Any]:
    return {
        "cause_evidence_id": value.get("cause_evidence_id"),
        "measured_channel_signatures": sorted(
            _unique_nonempty_strings(
                value.get("measured_channel_signatures"),
                "importance measured channel signatures",
            )
        ),
        "unmeasured_channel_signatures": sorted(
            _unique_nonempty_strings(
                value.get("unmeasured_channel_signatures"),
                "importance unmeasured channel signatures",
            )
        ),
        "dominating_cause_ids": sorted(
            _unique_nonempty_strings(
                value.get("dominating_cause_ids"), "importance dominating Cause IDs"
            )
        ),
        "dominated_within_domain": value.get("dominated_within_domain"),
        "fully_measured": value.get("fully_measured"),
    }


def _dominance_layers(
    selected_ids: Sequence[str], decisions: Sequence[Mapping[str, Any]]
) -> dict[str, int]:
    selected = set(selected_ids)
    by_id = {str(item["cause_evidence_id"]): item for item in decisions}
    dominated = {
        cause_id
        for cause_id, decision in by_id.items()
        if decision["dominated_within_domain"] is True
    }
    dominators = {
        cause_id: set(str(item) for item in by_id[cause_id]["dominating_cause_ids"])
        for cause_id in dominated
    }
    if any(
        not values or not values.issubset(selected) or cause_id in values
        for cause_id, values in dominators.items()
    ):
        return {cause_id: 0 for cause_id in selected}
    remaining = set(dominated)
    layers = {cause_id: 0 for cause_id in selected - dominated}
    layer = 1
    while remaining:
        next_layer = {
            cause_id
            for cause_id in remaining
            if not (dominators[cause_id] & remaining)
        }
        if not next_layer:
            return {cause_id: 0 for cause_id in selected}
        layers.update({cause_id: layer for cause_id in next_layer})
        remaining -= next_layer
        layer += 1
    return layers


def _canonical_public_importance(value: Mapping[str, Any]) -> dict[str, Any]:
    projected = dict(value)
    for field in (
        "selected_cause_ids",
        "frontier_cause_ids",
        "dominated_within_domain_cause_ids",
    ):
        projected[field] = _unique_nonempty_strings(
            projected.get(field), f"public importance {field}"
        )
    projected["profiles"] = sorted(
        _objects(projected.get("profiles"), "public importance profiles"),
        key=json_bytes,
    )
    projected["relations"] = sorted(
        _objects(projected.get("relations"), "public importance relations"),
        key=json_bytes,
    )
    projected["decisions"] = sorted(
        [
            _normalized_decision(item)
            for item in _objects(projected.get("decisions"), "public importance decisions")
        ],
        key=json_bytes,
    )
    projected["unmeasured"] = sorted(
        _objects(projected.get("unmeasured"), "public importance unmeasured rows"),
        key=json_bytes,
    )
    return projected


def _expected_public_importance(
    selected_ids: Sequence[str],
    profiles: Sequence[Mapping[str, Any]],
    relations: Sequence[Mapping[str, Any]],
    decisions: Sequence[Mapping[str, Any]],
    frontier: Sequence[str],
    dominated: Sequence[str],
    unique_top: str | None,
) -> dict[str, Any]:
    public_profiles = []
    for profile in profiles:
        frame = _object(profile.get("frame"), "importance profile frame")
        public_profiles.append(
            {
                "cause_evidence_id": profile["cause_evidence_id"],
                "causal_signature": profile["causal_signature"],
                "universe": profile["universe"],
                "domain_kind": profile["domain_kind"],
                "domain": profile["domain"],
                "stake": frame["stake"],
                "event_line": frame["event_line"],
                "effect_mode": frame["effect_mode"],
                "direct_change": frame["direct_change"],
                "played_change": frame["played_change"],
                "measure": profile["measure"],
                "effect_scope": profile["effect_scope"],
            }
        )
    return {
        "authority": "root_owned_effect_partial_order",
        "relation_policy_version": IMPORTANCE_RELATION_POLICY_VERSION,
        "ordering_policy_version": IMPORTANCE_ORDERING_POLICY_VERSION,
        "comparison_exposure_rank_meaning": "comparison_exposure_authority",
        "selected_cause_ids": list(selected_ids),
        "frontier_cause_ids": list(frontier),
        "dominated_within_domain_cause_ids": list(dominated),
        "unique_top": unique_top,
        "profile_summary": {
            "measured_channels": len(profiles),
            "measured_causes": len({str(item["cause_evidence_id"]) for item in profiles}),
            "fully_measured_causes": sum(item["fully_measured"] is True for item in decisions),
            "unmeasured_causes": sum(bool(item["unmeasured_channel_signatures"]) for item in decisions),
        },
        "profiles": public_profiles,
        "relations": [dict(item) for item in relations],
        "decisions": [dict(item) for item in decisions],
        "relation_summary": {
            "comparable_profile_pairs": sum(item["relation"] != "incomparable" for item in relations),
            "incomparable_profile_pairs": sum(item["relation"] == "incomparable" for item in relations),
        },
        "unmeasured": [
            {
                "cause_evidence_id": item["cause_evidence_id"],
                "channel_count": len(item["unmeasured_channel_signatures"]),
            }
            for item in decisions
            if item["unmeasured_channel_signatures"]
        ],
    }


_EXACT_VERDICT_FIELDS = {
    "comparison_kind",
    "mover",
    "verdict_code",
    "move_quality",
    "played_move",
    "reference_move",
    "candidate_win_percent_delta_for_mover",
    "win_percent_loss_for_mover",
    "outcome",
    "mate",
}
_VERDICT_AUTHORITY_FIELDS = {
    "policy_version",
    "comparison_kind",
    "mover",
    "played_move",
    "reference_move",
    "candidate_set_type",
    "candidate_win_percent_delta_for_mover",
    "win_percent_loss_for_mover",
    "expected_verdict_code",
    "expected_move_quality",
    "reference_mate_for_mover",
    "played_mate_for_mover",
    "mate_distance_loss_for_mover",
}


def _nullable_verdict(value: Any, label: str) -> dict[str, Any] | None:
    if value is None:
        return None
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object or null")
    return dict(value)


def _canonical_uci_move(value: Any, label: str) -> str:
    if not isinstance(value, str) or value != value.strip().lower():
        raise ContractError(f"{label} is not a canonical UCI move")
    try:
        move = chess.Move.from_uci(value)
    except ValueError as error:
        raise ContractError(f"{label} is not a canonical UCI move") from error
    if move == chess.Move.null() or move.uci() != value:
        raise ContractError(f"{label} is not a canonical UCI move")
    return value


def _finite_number(value: Any, label: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ContractError(f"{label} must be a finite number")
    number = float(value)
    if not math.isfinite(number):
        raise ContractError(f"{label} must be a finite number")
    return number


def _nullable_int(value: Any, label: str) -> int | None:
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, int):
        raise ContractError(f"{label} must be an integer or null")
    return value


def _nullable_nonzero_mate(value: Any, label: str) -> int | None:
    result = _nullable_int(value, label)
    if result == 0:
        raise ContractError(f"{label} must be non-zero")
    return result


def _mate_state(value: int) -> str:
    if value > 0:
        return "forced_win"
    if value < 0:
        return "forced_loss"
    raise ContractError("engine mate score must be non-zero")


def _expected_mate_distance_loss(reference: int | None, played: int | None) -> int | None:
    if reference is None or played is None:
        return None
    if reference > 0 and played > 0 and abs(played) > abs(reference):
        return abs(played) - abs(reference)
    if reference < 0 and played < 0 and abs(played) < abs(reference):
        return abs(reference) - abs(played)
    return None


def _validate_verdict_mate(
    verdict: Mapping[str, Any]
) -> tuple[int | None, int | None, int | None]:
    mate_raw = verdict.get("mate")
    outcome_raw = verdict.get("outcome")
    if mate_raw is None:
        if outcome_raw is not None:
            raise ContractError("exact verdict outcome exists without mate coordinates")
        return None, None, None
    mate = _object(mate_raw, "exact verdict mate")
    if set(mate) != {"reference_for_mover", "played_for_mover", "distance_loss"}:
        raise ContractError("exact verdict mate has non-canonical fields")
    reference = _nullable_nonzero_mate(
        mate.get("reference_for_mover"), "reference mate"
    )
    played = _nullable_nonzero_mate(mate.get("played_for_mover"), "played mate")
    distance = _nullable_int(mate.get("distance_loss"), "mate distance loss")
    if reference is None and played is None:
        raise ContractError("exact verdict mate has no engine mate value")
    expected_distance = _expected_mate_distance_loss(reference, played)
    if distance != expected_distance:
        raise ContractError("exact verdict mate distance loss is inconsistent")
    if played is None:
        if outcome_raw is not None:
            raise ContractError("exact verdict outcome exists without a played mate")
        return reference, played, distance
    outcome = _object(outcome_raw, "exact verdict outcome")
    if set(outcome) != {"state", "certainty", "reference_state", "resistance"}:
        raise ContractError("exact verdict outcome has non-canonical fields")
    expected_reference_state = _mate_state(reference) if reference is not None else None
    expected_resistance = None
    if reference is not None and reference < 0 and played < 0:
        if distance is not None and distance > 0:
            expected_resistance = "shortened"
        elif abs(played) > abs(reference):
            expected_resistance = "extended"
        else:
            expected_resistance = "held"
    if (
        outcome.get("state") != _mate_state(played)
        or outcome.get("certainty") != "engine_mate_score"
        or outcome.get("reference_state") != expected_reference_state
        or outcome.get("resistance") != expected_resistance
    ):
        raise ContractError("exact verdict mate outcome is inconsistent")
    return reference, played, distance


def _verdict_state(
    actual: Mapping[str, Any], case: Mapping[str, Any]
) -> dict[str, Any] | None:
    observation = _object(actual.get("verdict"), "exact verdict observation")
    if set(observation) != {
        "packet_canonical",
        "selected_projection",
        "final_public_response",
        "classification_authority",
    }:
        raise ContractError("exact verdict observation has non-canonical fields")
    packet = _nullable_verdict(
        observation.get("packet_canonical"), "packet canonical verdict"
    )
    selected = _nullable_verdict(
        observation.get("selected_projection"), "selected projection verdict"
    )
    public = _nullable_verdict(
        observation.get("final_public_response"), "final public response verdict"
    )
    authority = _nullable_verdict(
        observation.get("classification_authority"),
        "central verdict classification authority",
    )
    public_response = _object(actual.get("public_response"), "public response summary")
    primary_engine_backed = public_response.get("primary_engine_backed")
    if not isinstance(primary_engine_backed, bool):
        raise ContractError("public response has no explicit primary engine authority")
    copies = (packet, selected, public)
    if primary_engine_backed:
        if any(item is None for item in copies) or authority is None:
            raise ContractError(
                "engine-backed PlayedVsBest verdict and classification authority must be non-null"
            )
    elif any(item is not None for item in copies) or authority is not None:
        raise ContractError(
            "unavailable PlayedVsBest verdict copies and classification authority must all be null"
        )
    else:
        return None
    assert packet is not None and selected is not None and public is not None
    assert authority is not None
    if len({sha256_json(item) for item in copies}) != 1:
        raise ContractError(
            "PlayedVsBest verdict must be exactly equal at packet, projection, and public response"
        )
    verdict = packet
    if set(verdict) != _EXACT_VERDICT_FIELDS:
        raise ContractError("exact PlayedVsBest verdict has non-canonical fields")
    if verdict.get("comparison_kind") != "played_vs_best":
        raise ContractError("exact verdict is not a PlayedVsBest comparison")
    if verdict.get("mover") not in {"white", "black"}:
        raise ContractError("exact PlayedVsBest verdict has an invalid mover")
    played_move = _canonical_uci_move(verdict.get("played_move"), "played verdict move")
    reference_move = _canonical_uci_move(
        verdict.get("reference_move"), "reference verdict move"
    )
    expected_played = _canonical_uci_move(
        case.get("played_move_uci"), "case played move"
    )
    if played_move != expected_played:
        raise ContractError("exact PlayedVsBest verdict has the wrong played move")
    fen = case.get("fen")
    if not isinstance(fen, str):
        raise ContractError("cause-audit case has no root FEN")
    try:
        board = chess.Board(fen)
    except ValueError as error:
        raise ContractError("cause-audit case has an invalid root FEN") from error
    expected_mover = "white" if board.turn == chess.WHITE else "black"
    if verdict.get("mover") != expected_mover:
        raise ContractError("exact PlayedVsBest verdict mover differs from root side to move")
    for move_uci, label in (
        (played_move, "played"),
        (reference_move, "reference"),
    ):
        if not board.is_legal(chess.Move.from_uci(move_uci)):
            raise ContractError(f"exact PlayedVsBest {label} move is illegal at the root")
    delta = _finite_number(
        verdict.get("candidate_win_percent_delta_for_mover"),
        "candidate win-percent delta",
    )
    loss = _finite_number(
        verdict.get("win_percent_loss_for_mover"), "mover win-percent loss"
    )
    if not -100.0 <= delta <= 100.0 or not 0.0 <= loss <= 100.0:
        raise ContractError("exact PlayedVsBest verdict percentage is outside its domain")
    if not math.isclose(loss, max(-delta, 0.0), rel_tol=0.0, abs_tol=1e-9):
        raise ContractError("exact PlayedVsBest verdict loss is inconsistent with its polarity")
    verdict_code = verdict.get("verdict_code")
    expected_quality = {
        "improves_on_reference": "good",
        "matches_reference": "good",
        "playable_loss": "playable",
        "inaccuracy": "bad",
        "mistake": "bad",
        "blunder": "bad",
    }.get(verdict_code)
    if expected_quality is None or verdict.get("move_quality") != expected_quality:
        raise ContractError("exact PlayedVsBest verdict quality is inconsistent")
    if verdict_code == "improves_on_reference" and delta <= 0.0:
        raise ContractError("improving PlayedVsBest verdict has the wrong polarity")
    if verdict_code == "matches_reference" and loss != 0.0:
        raise ContractError("matching PlayedVsBest verdict has the wrong polarity")
    if verdict_code in {"inaccuracy", "mistake", "blunder"} and loss <= 0.0:
        raise ContractError("negative PlayedVsBest verdict has the wrong polarity")
    reference_mate, played_mate, mate_distance_loss = _validate_verdict_mate(verdict)
    if verdict_code == "playable_loss" and loss == 0.0:
        mate = verdict.get("mate")
        if not isinstance(mate, Mapping) or mate.get("distance_loss") is None:
            raise ContractError("zero-loss playable verdict has no mate-distance loss")
    if set(authority) != _VERDICT_AUTHORITY_FIELDS:
        raise ContractError("central verdict classification authority has non-canonical fields")
    if authority.get("policy_version") != VERDICT_CLASSIFICATION_POLICY_VERSION:
        raise ContractError("central verdict classification policy version is invalid")
    if authority.get("candidate_set_type") not in {
        None,
        "only_move",
        "narrow_choice",
        "style_choice",
    }:
        raise ContractError("central verdict classification has an invalid candidate set type")
    authority_delta = _finite_number(
        authority.get("candidate_win_percent_delta_for_mover"),
        "central candidate win-percent delta",
    )
    authority_loss = _finite_number(
        authority.get("win_percent_loss_for_mover"),
        "central mover win-percent loss",
    )
    authority_reference_mate = _nullable_nonzero_mate(
        authority.get("reference_mate_for_mover"), "central reference mate"
    )
    authority_played_mate = _nullable_nonzero_mate(
        authority.get("played_mate_for_mover"), "central played mate"
    )
    authority_distance = _nullable_int(
        authority.get("mate_distance_loss_for_mover"),
        "central mate-distance loss",
    )
    if authority_distance is not None and authority_distance <= 0:
        raise ContractError("central mate-distance loss must be positive")
    authority_matches = (
        authority.get("comparison_kind") == verdict.get("comparison_kind")
        and authority.get("mover") == verdict.get("mover")
        and authority.get("played_move") == played_move
        and authority.get("reference_move") == reference_move
        and math.isclose(authority_delta, delta, rel_tol=0.0, abs_tol=1e-9)
        and math.isclose(authority_loss, loss, rel_tol=0.0, abs_tol=1e-9)
        and authority.get("expected_verdict_code") == verdict_code
        and authority.get("expected_move_quality") == verdict.get("move_quality")
        and authority_reference_mate == reference_mate
        and authority_played_mate == played_mate
        and authority_distance == mate_distance_loss
    )
    if not authority_matches:
        raise ContractError(
            "public PlayedVsBest verdict differs from central classification authority"
        )
    if mate_distance_loss is not None and verdict_code != "playable_loss":
        raise ContractError(
            "mate-distance loss is not classified by central authority as playable_loss"
        )
    return verdict


def _importance_state(
    actual: Mapping[str, Any], case: Mapping[str, Any]
) -> dict[str, Any]:
    verdict = _verdict_state(actual, case)
    observation = _object(actual.get("importance"), "typed importance observation")
    native = _object(observation.get("r_native"), "R-native importance resolution")
    if native.get("relation_policy_version") != IMPORTANCE_RELATION_POLICY_VERSION:
        raise ContractError("R-native importance relation policy version is invalid")
    if native.get("ordering_policy_version") != IMPORTANCE_ORDERING_POLICY_VERSION:
        raise ContractError("R-native importance ordering policy version is invalid")
    selections = _objects(
        actual.get("r_native_cause_selections"), "R-native Cause selections"
    )
    selection_by_id: dict[str, dict[str, Any]] = {}
    selected_channels: dict[str, list[str]] = {}
    channel_by_key: dict[tuple[str, str], dict[str, Any]] = {}
    for selection in selections:
        cause_id = selection.get("cause_evidence_id")
        if not isinstance(cause_id, str) or not cause_id or cause_id in selection_by_id:
            raise ContractError("R-native importance has invalid selected Cause ownership")
        channels = _objects(selection.get("channels"), "R selected importance channels")
        signatures: list[str] = []
        for channel in channels:
            signature = channel.get("causal_signature")
            if not isinstance(signature, str) or not signature:
                raise ContractError("R selected importance channel has no signature")
            key = (cause_id, signature)
            if key in channel_by_key:
                raise ContractError("R selected Cause has duplicate channel signatures")
            signatures.append(signature)
            channel_by_key[key] = channel
        if not signatures:
            raise ContractError("R selected Cause has no importance channels")
        selection_by_id[cause_id] = selection
        selected_channels[cause_id] = signatures
    selected = _unique_nonempty_strings(
        native.get("selected_cause_ids"), "R-native importance selected Cause IDs"
    )
    if set(selected) != set(selection_by_id):
        raise ContractError("R-native importance selected Causes differ from R selections")
    profiles = _objects(native.get("profiles"), "R-native importance profiles")
    profile_by_key: dict[tuple[str, str], dict[str, Any]] = {}
    semantic_board = _semantic_root_board(case)
    for profile in profiles:
        cause_id = profile.get("cause_evidence_id")
        signature = profile.get("causal_signature")
        if not isinstance(cause_id, str) or not isinstance(signature, str):
            raise ContractError("R-native importance profile has no exact owner key")
        key = (cause_id, signature)
        if key not in channel_by_key:
            raise ContractError("R-native importance profile is not a selected exact channel")
        if key in profile_by_key:
            raise ContractError("R-native importance has duplicate Cause/channel profiles")
        selection = selection_by_id[cause_id]
        channel = channel_by_key[key]
        frame = _object(profile.get("frame"), "importance profile frame")
        universe = _object(profile.get("universe"), "importance profile universe")
        actor = _selected_actor_side(channel)
        if universe.get("root_board_state") != semantic_board:
            raise ContractError("importance profile universe has the wrong root board")
        if _root_board_mover(semantic_board) != actor:
            raise ContractError("importance profile actor is not the root side to move")
        if universe.get("impact") != _expected_player_facing_impact(frame):
            raise ContractError("importance profile universe differs from its frame")
        if (
            frame.get("comparison") != selection.get("comparison_semantic_key")
            or frame.get("effect_mode") != selection.get("effect_mode")
            or frame.get("exposure") != selection.get("exposure")
            or frame.get("source_side") != selection.get("source_side")
            or frame.get("attribution")
            != _object(selection.get("attribution"), "R selection attribution").get("kind")
            or frame.get("direct_change") != channel.get("direct_change")
            or frame.get("played_change") != channel.get("played_change")
            or frame.get("actor") != actor
            or frame.get("event_line")
            != _line_stable_key(_object(channel.get("line"), "R selected channel line"))
        ):
            raise ContractError("importance profile frame is not bound to its selected channel")
        descriptor = _object(
            channel.get("effect_descriptor"), "selected channel effect descriptor"
        )
        if (
            channel.get("descriptor_ambiguous") is not False
            or descriptor.get("magnitude_status") != "exact"
            or descriptor.get("measure") != profile.get("measure")
            or descriptor.get("effect_scope") != profile.get("effect_scope")
        ):
            raise ContractError("importance profile differs from its exact channel descriptor")
        _validate_material_event_salience(
            descriptor,
            _object(profile.get("measure"), "importance profile measure"),
        )
        measured_effect = _object(
            channel.get("importance_effect"),
            "selected channel exact measured effect",
        )
        if (
            measured_effect.get("stake") != frame.get("stake")
            or measured_effect.get("domain_kind") != profile.get("domain_kind")
            or measured_effect.get("domain") != profile.get("domain")
            or measured_effect.get("measure") != profile.get("measure")
            or measured_effect.get("effect_scope") != profile.get("effect_scope")
        ):
            raise ContractError(
                "importance profile differs from its exact channel measured effect"
            )
        _importance_domain_ready(profile)
        profile_by_key[key] = profile
    expected_relations = _expected_importance_relations(profiles)
    runtime_relations = _objects(native.get("relations"), "R-native importance relations")
    _validate_runtime_relations(runtime_relations, expected_relations)
    expected_decisions = _expected_importance_decisions(
        selected_channels, profiles, expected_relations
    )
    runtime_decisions = _objects(native.get("decisions"), "R-native importance decisions")
    runtime_by_id: dict[str, dict[str, Any]] = {}
    for decision in runtime_decisions:
        cause_id = decision.get("cause_evidence_id")
        if not isinstance(cause_id, str) or cause_id in runtime_by_id:
            raise ContractError("R-native importance has duplicate or invalid decisions")
        runtime_by_id[cause_id] = _normalized_decision(decision)
    expected_by_id = {
        str(item["cause_evidence_id"]): _normalized_decision(item)
        for item in expected_decisions
    }
    if runtime_by_id != expected_by_id:
        raise ContractError("R-native importance decisions differ from profile relations")
    dominated_set = {
        str(item["cause_evidence_id"])
        for item in expected_decisions
        if item["dominated_within_domain"] is True
    }
    frontier_set = set(selected) - dominated_set
    native_dominated = _unique_nonempty_strings(
        native.get("dominated_within_domain_cause_ids"),
        "R-native dominated Cause IDs",
    )
    if set(native_dominated) != dominated_set:
        raise ContractError("R-native importance dominated Cause set is inconsistent")
    native_frontier = _unique_nonempty_strings(
        native.get("frontier_cause_ids"), "R-native frontier Cause IDs"
    )
    if set(native_frontier) != frontier_set:
        raise ContractError("R-native importance frontier Cause set is inconsistent")
    unique_top = (
        next(iter(frontier_set))
        if expected_decisions
        and all(item["fully_measured"] is True for item in expected_decisions)
        and len(frontier_set) == 1
        else None
    )
    if native.get("unique_top") != unique_top:
        raise ContractError("R-native importance unique_top is inconsistent")
    observed_orders = [item.get("selection_order") for item in selections]
    if (
        any(not isinstance(item, int) or isinstance(item, bool) for item in observed_orders)
        or sorted(int(item) for item in observed_orders) != list(range(len(selections)))
    ):
        raise ContractError("R-native Cause selection_order is not contiguous")
    ordered_selections = sorted(
        selections,
        key=lambda item: int(item["selection_order"]),
    )
    ordered_ids = [str(item["cause_evidence_id"]) for item in ordered_selections]
    frontier = [cause_id for cause_id in ordered_ids if cause_id in frontier_set]
    dominated = [cause_id for cause_id in ordered_ids if cause_id in dominated_set]
    if selected != ordered_ids or native_frontier != frontier or native_dominated != dominated:
        raise ContractError("R-native importance order differs from public idea selection order")
    expected_public = _expected_public_importance(
        ordered_ids,
        profiles,
        expected_relations,
        expected_decisions,
        frontier,
        dominated,
        unique_top,
    )
    r_public = _object(
        observation.get("r_public_projection"), "R public importance projection"
    )
    if sha256_json(_canonical_public_importance(r_public)) != sha256_json(
        _canonical_public_importance(expected_public)
    ):
        raise ContractError("R public importance projection differs from native profiles")
    public_response = _object(actual.get("public_response"), "public response summary")
    primary_engine_backed = public_response.get("primary_engine_backed")
    if not isinstance(primary_engine_backed, bool):
        raise ContractError("public response has no explicit primary engine authority")
    projection = _object(actual.get("projection"), "public Cause projection")
    if primary_engine_backed:
        if (
            public_response.get("status") != "ready"
            or projection.get("present") is not True
            or projection.get("status") != "ready"
            or projection.get("reason") is not None
            or projection.get("renderable") is not True
            or projection.get("selected_payload_source") != "full"
        ):
            raise ContractError(
                "engine-backed primary verdict was not preserved by public projection"
            )
    elif projection.get("present") is True and (
        public_response.get("status") != "withheld"
        or projection.get("status") != "withheld"
        or projection.get("reason") != "insufficient_engine_depth"
        or projection.get("renderable") is not False
        or projection.get("selected_payload_source") != "empty"
    ):
        raise ContractError(
            "public projection availability differs from primary engine authority"
        )
    public_selections = _objects(
        projection.get("selected_public_cause_selections"),
        "public Cause selections",
    )
    expected_idea_status = (
        "certified"
        if public_selections
        else "no_certified_differential_idea"
        if primary_engine_backed
        else "unavailable"
    )
    if public_response.get("idea_status") != expected_idea_status:
        raise ContractError("public idea_status differs from engine-backed Cause availability")
    packet_native_raw = observation.get("packet_native")
    packet_raw = observation.get("packet_public_projection")
    public_raw = observation.get("public_projection")
    if packet_native_raw is not None and not isinstance(packet_native_raw, Mapping):
        raise ContractError("packet-native importance must be an object or null")
    if packet_raw is not None and not isinstance(packet_raw, Mapping):
        raise ContractError("packet importance projection must be an object or null")
    if public_raw is not None and not isinstance(public_raw, Mapping):
        raise ContractError("public importance projection must be an object or null")
    return {
        "native": native,
        "selected": ordered_ids,
        "profiles": profiles,
        "relations": expected_relations,
        "decisions": expected_decisions,
        "packet_native": dict(packet_native_raw) if isinstance(packet_native_raw, Mapping) else None,
        "r_public": r_public,
        "packet_public": dict(packet_raw) if isinstance(packet_raw, Mapping) else None,
        "public": dict(public_raw) if isinstance(public_raw, Mapping) else None,
        "verdict": verdict,
        "public_payload_renderable": primary_engine_backed,
    }


def _idea_unit_state(
    actual: Mapping[str, Any], cause_importance: Mapping[str, Any]
) -> dict[str, Any]:
    public_payload_renderable = cause_importance.get("public_payload_renderable") is True
    observation = _object(actual.get("idea_units"), "typed idea-unit observation")
    r_units = _objects(observation.get("r_native"), "R-native idea units")

    def optional_units(value: Any, label: str) -> list[dict[str, Any]] | None:
        if value is None:
            return None
        return _objects(value, label)

    packet_units = optional_units(
        observation.get("packet_native"), "packet-native idea units"
    )
    public_units = optional_units(
        observation.get("public_projection"), "public idea units"
    )
    item_links = _objects(
        observation.get("public_item_links"), "public idea-item links"
    )
    raw_unit_count = observation.get("raw_public_idea_unit_count")
    parsed_unit_count = observation.get("parsed_public_idea_unit_count")
    public_units_parse_closed = bool(
        observation.get("public_idea_units_parse_closed") is True
        and isinstance(raw_unit_count, int)
        and not isinstance(raw_unit_count, bool)
        and raw_unit_count >= 0
        and isinstance(parsed_unit_count, int)
        and not isinstance(parsed_unit_count, bool)
        and parsed_unit_count >= 0
        and raw_unit_count == parsed_unit_count
        and public_units is not None
        and parsed_unit_count == len(public_units)
    )

    unit_ids: set[str] = set()
    member_ids: set[str] = set()
    lead_ids: list[str] = []
    expected_links: list[dict[str, str]] = []
    cause_to_lead: dict[str, str] = {}
    members_by_lead: dict[str, list[str]] = {}
    native_structure_closed = True
    for order, unit in enumerate(r_units):
        idea_id = unit.get("idea_id")
        kind = unit.get("kind")
        lead = unit.get("lead_cause_evidence_id")
        members_raw = unit.get("member_cause_evidence_ids")
        importance_layer = unit.get("importance_layer")
        priority_status = unit.get("priority_status")
        serialization_order = unit.get("serialization_order")
        members = (
            [str(item) for item in members_raw]
            if isinstance(members_raw, list)
            and all(isinstance(item, str) and item for item in members_raw)
            else []
        )
        structurally_valid = bool(
            isinstance(idea_id, str)
            and idea_id
            and idea_id not in unit_ids
            and kind in {"single_cause", "exact_pvb_responsibility"}
            and isinstance(lead, str)
            and lead
            and idea_id == lead
            and members
            and len(members) == len(set(members))
            and members[0] == lead
            and not member_ids.intersection(members)
            and isinstance(importance_layer, int)
            and not isinstance(importance_layer, bool)
            and importance_layer >= 0
            and priority_status
            in {"unique_top", "frontier", "dominated", "unmeasured"}
            and serialization_order == order
            and (kind != "single_cause" or len(members) == 1)
            and (kind != "exact_pvb_responsibility" or len(members) >= 2)
        )
        if not structurally_valid:
            raise ContractError("R-native idea unit has invalid structure")
        unit_ids.add(str(idea_id))
        member_ids.update(members)
        lead_ids.append(str(lead))
        members_by_lead[str(lead)] = members
        for cause_id in members:
            cause_to_lead[cause_id] = str(lead)
            expected_links.append(
                {
                    "cause_evidence_id": cause_id,
                    "item_role": "cause_facet",
                    "idea_unit_id": str(idea_id),
                }
            )

    selections = _objects(
        actual.get("r_native_cause_selections"), "R-native Cause selections"
    )
    ordered_selections = sorted(selections, key=lambda item: int(item["selection_order"]))
    selected_ids = [str(item.get("cause_evidence_id")) for item in ordered_selections]
    selection_by_id = {
        str(item["cause_evidence_id"]): item for item in ordered_selections
    }
    native_structure_closed = bool(
        native_structure_closed
        and len(unit_ids) == len(r_units)
        and len(lead_ids) == len(set(lead_ids))
        and list(cause_to_lead) == selected_ids
    )
    if not native_structure_closed:
        raise ContractError("R-native idea units do not partition ordered R selections")

    idea_importance_observation = _object(
        actual.get("idea_importance"), "typed idea-importance observation"
    )
    native_importance = _object(
        idea_importance_observation.get("r_native"),
        "R-native idea-importance resolution",
    )
    if native_importance.get("relation_policy_version") != IMPORTANCE_RELATION_POLICY_VERSION:
        raise ContractError("R-native idea-importance relation policy version is invalid")
    if native_importance.get("ordering_policy_version") != IMPORTANCE_ORDERING_POLICY_VERSION:
        raise ContractError("R-native idea-importance ordering policy version is invalid")
    idea_selected = _unique_nonempty_strings(
        native_importance.get("selected_cause_ids"),
        "R-native idea-importance selected lead IDs",
    )
    idea_profiles = _objects(
        native_importance.get("profiles"), "R-native idea-importance profiles"
    )
    idea_relations = _objects(
        native_importance.get("relations"), "R-native idea-importance relations"
    )
    idea_decisions = _objects(
        native_importance.get("decisions"), "R-native idea-importance decisions"
    )
    r_public_importance = _object(
        idea_importance_observation.get("r_public_projection"),
        "R public idea-importance projection",
    )

    def optional_object(value: Any, label: str) -> dict[str, Any] | None:
        if value is None:
            return None
        return _object(value, label)

    packet_native_importance = optional_object(
        idea_importance_observation.get("packet_native"),
        "packet-native idea importance",
    )
    packet_public_importance = optional_object(
        idea_importance_observation.get("packet_public_projection"),
        "packet public idea importance",
    )
    public_importance = optional_object(
        idea_importance_observation.get("public_projection"),
        "public idea importance",
    )

    lead_set = set(lead_ids)
    expected_profiles = [
        dict(item)
        for item in cause_importance["profiles"]
        if str(item["cause_evidence_id"]) in lead_set
    ]
    if sha256_json(idea_profiles) != sha256_json(expected_profiles):
        raise ContractError(
            "R-native idea-importance profiles are not the exact lead-Cause subset"
        )
    expected_relations = _expected_importance_relations(expected_profiles)
    _validate_runtime_relations(idea_relations, expected_relations)
    selected_channels: dict[str, list[str]] = {}
    for lead in lead_ids:
        selection = selection_by_id.get(lead)
        if selection is None:
            raise ContractError("R-native idea unit lead is not a selected Cause")
        selected_channels[lead] = [
            str(channel["causal_signature"])
            for channel in _objects(
                selection.get("channels"), "idea-lead selected channels"
            )
        ]
    expected_decisions = _expected_importance_decisions(
        selected_channels, expected_profiles, expected_relations
    )
    runtime_decisions_by_id: dict[str, dict[str, Any]] = {}
    for decision in idea_decisions:
        cause_id = decision.get("cause_evidence_id")
        if not isinstance(cause_id, str) or cause_id in runtime_decisions_by_id:
            raise ContractError(
                "R-native idea-importance has duplicate or invalid decisions"
            )
        runtime_decisions_by_id[cause_id] = _normalized_decision(decision)
    expected_decisions_by_id = {
        str(item["cause_evidence_id"]): _normalized_decision(item)
        for item in expected_decisions
    }
    if runtime_decisions_by_id != expected_decisions_by_id:
        raise ContractError(
            "R-native idea-importance decisions differ from lead profile relations"
        )

    dominated_set = {
        str(item["cause_evidence_id"])
        for item in expected_decisions
        if item["dominated_within_domain"] is True
    }
    frontier_set = lead_set - dominated_set
    unique_top = (
        next(iter(frontier_set))
        if expected_decisions
        and all(item["fully_measured"] is True for item in expected_decisions)
        and len(frontier_set) == 1
        else None
    )
    if native_importance.get("unique_top") != unique_top:
        raise ContractError("R-native idea-importance unique_top is inconsistent")
    layers = _dominance_layers(lead_ids, expected_decisions)

    def comparison_rank(cause_id: str) -> int:
        value = selection_by_id[cause_id].get("comparison_exposure_rank")
        if not isinstance(value, int) or isinstance(value, bool) or value < 0:
            raise ContractError("R-native idea lead has an invalid comparison rank")
        return value

    expected_unit_order = sorted(
        r_units,
        key=lambda unit: (
            layers[str(unit["lead_cause_evidence_id"])],
            comparison_rank(str(unit["lead_cause_evidence_id"])),
            str(unit["idea_id"]),
        ),
    )
    expected_lead_ids = [
        str(unit["lead_cause_evidence_id"]) for unit in expected_unit_order
    ]
    if idea_selected != expected_lead_ids:
        raise ContractError(
            "R-native idea-importance order differs from idea-unit ordering"
        )
    native_frontier = _unique_nonempty_strings(
        native_importance.get("frontier_cause_ids"),
        "R-native idea-importance frontier lead IDs",
    )
    native_dominated = _unique_nonempty_strings(
        native_importance.get("dominated_within_domain_cause_ids"),
        "R-native idea-importance dominated lead IDs",
    )
    expected_frontier = [
        lead for lead in expected_lead_ids if lead in frontier_set
    ]
    expected_dominated = [
        lead for lead in expected_lead_ids if lead in dominated_set
    ]
    if native_frontier != expected_frontier or native_dominated != expected_dominated:
        raise ContractError("R-native idea-importance frontier order is inconsistent")

    expected_unit_ids = [str(unit["idea_id"]) for unit in expected_unit_order]
    if [str(unit["idea_id"]) for unit in r_units] != expected_unit_ids:
        raise ContractError("R-native idea-unit serialization order is inconsistent")
    for order, unit in enumerate(r_units):
        lead = str(unit["lead_cause_evidence_id"])
        decision = expected_decisions_by_id[lead]
        expected_status = (
            "unmeasured"
            if decision["fully_measured"] is not True
            else "unique_top"
            if unique_top == lead
            else "dominated"
            if decision["dominated_within_domain"] is True
            else "frontier"
        )
        members = [str(item) for item in unit["member_cause_evidence_ids"]]
        expected_members = [lead] + sorted(
            (cause_id for cause_id in members if cause_id != lead),
            key=lambda cause_id: (comparison_rank(cause_id), cause_id),
        )
        if (
            unit.get("importance_layer") != layers[lead]
            or unit.get("priority_status") != expected_status
            or unit.get("serialization_order") != order
            or members != expected_members
        ):
            raise ContractError(
                "R-native idea-unit layer, priority, member, or order is inconsistent"
            )

    expected_public_importance = _expected_public_importance(
        expected_lead_ids,
        expected_profiles,
        expected_relations,
        expected_decisions,
        expected_frontier,
        expected_dominated,
        unique_top,
    )
    if sha256_json(_canonical_public_importance(r_public_importance)) != sha256_json(
        _canonical_public_importance(expected_public_importance)
    ):
        raise ContractError(
            "R public idea-importance projection differs from lead profiles"
        )

    public_membership_closed = bool(
        (
            public_payload_renderable
            and observation.get("public_item_membership_closed") is True
            and public_units_parse_closed
            and item_links == expected_links
        )
        or (
            not public_payload_renderable
            and public_units in (None, [])
            and item_links == []
            and raw_unit_count == 0
            and parsed_unit_count == 0
        )
    )
    has_units = bool(r_units)
    r_to_packet_exact = bool(
        (
            packet_units is not None
            and sha256_json(r_units) == sha256_json(packet_units)
        )
        or (not has_units and packet_units is None)
    )
    packet_to_public_exact = bool(
        (
            public_payload_renderable
            and packet_units is not None
            and public_units is not None
            and sha256_json(packet_units) == sha256_json(public_units)
        )
        or (
            public_payload_renderable
            and not has_units
            and public_units in (None, [])
            and packet_units in (None, [])
        )
        or (not public_payload_renderable and public_units in (None, []))
    )
    idea_importance_r_to_packet_exact = bool(
        (
            packet_native_importance is not None
            and packet_public_importance is not None
            and sha256_json(native_importance)
            == sha256_json(packet_native_importance)
            and sha256_json(r_public_importance)
            == sha256_json(packet_public_importance)
        )
        or (
            not has_units
            and packet_native_importance is None
            and packet_public_importance is None
        )
    )
    idea_importance_packet_to_public_exact = bool(
        (
            public_payload_renderable
            and packet_public_importance is not None
            and public_importance is not None
            and sha256_json(packet_public_importance)
            == sha256_json(public_importance)
        )
        or (
            public_payload_renderable
            and not has_units
            and public_importance is None
        )
        or (not public_payload_renderable and public_importance is None)
    )
    return {
        "r_units": r_units,
        "lead_ids": lead_ids,
        "members_by_lead": members_by_lead,
        "cause_to_lead": cause_to_lead,
        "native_structure_closed": native_structure_closed,
        "public_membership_closed": public_membership_closed,
        "r_to_packet_exact": r_to_packet_exact,
        "packet_to_public_exact": packet_to_public_exact,
        "idea_importance_r_to_packet_exact": idea_importance_r_to_packet_exact,
        "idea_importance_packet_to_public_exact": (
            idea_importance_packet_to_public_exact
        ),
        "importance": {
            "native": native_importance,
            "selected": idea_selected,
            "profiles": expected_profiles,
            "relations": expected_relations,
            "decisions": expected_decisions,
        },
    }


_CAUSE_DISPOSITION_FIELDS = {
    "cause_evidence_id",
    "status",
    "reason",
    "proposed_claim_ids",
    "certified_claim_ids",
    "rank_eligible_claim_ids",
    "selected_owner_claim_id",
    "related_cause_evidence_ids",
    "related_claim_ids",
}
_CAUSE_DISPOSITION_STATUSES = {
    "selected",
    "dominated",
    "redundant",
    "diagnostic",
    "inferior",
    "admission_deferred",
    "rejected",
    "unproposed",
    "object_unready",
}
_CAUSE_DISPOSITION_REASONS = {
    "player_facing_selection",
    "dominated_fallback",
    "cross_comparison_redundancy",
    "certified_claim_deduplicated",
    "diagnostic_comparison",
    "inferior_alternative",
    "claim_admission_deferred",
    "claim_admission_rejected",
    "no_claim_proposal",
    "object_readiness_failed",
}
_CAUSE_DISPOSITION_SUMMARY_FIELDS = {
    "authority",
    "total_cause_count",
    "selected_cause_ids",
    "status_counts",
    "reason_counts",
    "abstention_codes",
}


def _cause_disposition_ledger_state(
    actual: Mapping[str, Any], causes: Sequence[Mapping[str, Any]]
) -> dict[str, bool]:
    observation = _object(
        actual.get("cause_disposition_ledger"),
        "typed Cause disposition ledger observation",
    )
    r_native = _objects(
        observation.get("r_native"), "R-native Cause disposition ledger"
    )

    def canonical_ids(value: Any, label: str) -> list[str]:
        ids = _unique_nonempty_strings(value, label)
        if ids != sorted(ids):
            raise ContractError(f"{label} must be canonically ordered")
        return ids

    disposition_by_id: dict[str, dict[str, Any]] = {}
    selected_owner_by_id: dict[str, str] = {}
    for raw in r_native:
        disposition = dict(raw)
        if set(disposition) != _CAUSE_DISPOSITION_FIELDS:
            raise ContractError("R-native Cause disposition has non-canonical fields")
        cause_id = disposition.get("cause_evidence_id")
        status = disposition.get("status")
        reason = disposition.get("reason")
        if (
            not isinstance(cause_id, str)
            or not cause_id
            or cause_id in disposition_by_id
            or status not in _CAUSE_DISPOSITION_STATUSES
            or reason not in _CAUSE_DISPOSITION_REASONS
        ):
            raise ContractError("R-native Cause disposition identity is invalid")
        for field in (
            "proposed_claim_ids",
            "certified_claim_ids",
            "rank_eligible_claim_ids",
            "related_cause_evidence_ids",
            "related_claim_ids",
        ):
            canonical_ids(disposition.get(field), f"Cause disposition {field}")
        owner = disposition.get("selected_owner_claim_id")
        if owner is not None and (not isinstance(owner, str) or not owner):
            raise ContractError("Cause disposition selected owner is invalid")
        if (status == "selected") != (owner is not None):
            raise ContractError("Cause disposition selected owner is structurally inconsistent")
        if isinstance(owner, str):
            selected_owner_by_id[cause_id] = owner
        disposition_by_id[cause_id] = disposition
    r_ids = list(disposition_by_id)
    if r_ids != sorted(r_ids):
        raise ContractError("R-native Cause disposition ledger is not canonically ordered")

    cause_ids = sorted(_cause_id(cause) for cause in causes)
    coverage_exact = r_ids == cause_ids
    selections = _objects(
        actual.get("r_native_cause_selections"), "R-native Cause selections"
    )
    selection_owner_by_id: dict[str, str] = {}
    for selection in selections:
        cause_id = selection.get("cause_evidence_id")
        host = _object(selection.get("host"), "R selection host")
        owner = host.get("claim_id")
        if (
            not isinstance(cause_id, str)
            or not cause_id
            or cause_id in selection_owner_by_id
            or not isinstance(owner, str)
            or not owner
        ):
            raise ContractError("R selection owner closure is invalid")
        selection_owner_by_id[cause_id] = owner
    selection_exact = selected_owner_by_id == selection_owner_by_id

    def optional_ledger(value: Any, label: str) -> list[dict[str, Any]] | None:
        if value is None:
            return None
        return _objects(value, label)

    def summary(value: Any, label: str) -> dict[str, Any]:
        item = _object(value, label)
        if set(item) != _CAUSE_DISPOSITION_SUMMARY_FIELDS:
            raise ContractError(f"{label} has non-canonical fields")
        if item.get("authority") != "cause_disposition_ledger.v1":
            raise ContractError(f"{label} has the wrong authority")
        total = item.get("total_cause_count")
        if not isinstance(total, int) or isinstance(total, bool) or total < 0:
            raise ContractError(f"{label} has an invalid Cause count")
        canonical_ids(item.get("selected_cause_ids"), f"{label} selected Cause IDs")
        canonical_ids(item.get("abstention_codes"), f"{label} abstention codes")
        status_counts = _object(item.get("status_counts"), f"{label} status counts")
        reason_counts = _object(item.get("reason_counts"), f"{label} reason counts")
        if set(status_counts) != _CAUSE_DISPOSITION_STATUSES or set(
            reason_counts
        ) != _CAUSE_DISPOSITION_REASONS:
            raise ContractError(f"{label} count keys are not canonical")
        if any(
            not isinstance(count, int) or isinstance(count, bool) or count < 0
            for count in (*status_counts.values(), *reason_counts.values())
        ):
            raise ContractError(f"{label} has an invalid count")
        return item

    r_summary = summary(
        observation.get("r_public_summary"), "R Cause disposition public summary"
    )
    summary_coverage_exact = bool(
        r_summary["total_cause_count"] == len(r_native)
        and r_summary["selected_cause_ids"] == sorted(selected_owner_by_id)
    )
    packet_native = optional_ledger(
        observation.get("packet_native"), "packet-native Cause disposition ledger"
    )
    packet_summary_raw = observation.get("packet_public_summary")
    packet_summary = (
        None
        if packet_summary_raw is None
        else summary(packet_summary_raw, "packet Cause disposition public summary")
    )
    r_to_packet_exact = bool(
        packet_native is not None
        and packet_summary is not None
        and sha256_json(r_native) == sha256_json(packet_native)
        and sha256_json(r_summary) == sha256_json(packet_summary)
    )
    selected_raw = observation.get("selected_projection")
    final_raw = observation.get("final_public_response")
    public_response = _object(actual.get("public_response"), "public response summary")
    top_level_raw = public_response.get("idea_status_detail")
    selected_summary = (
        None
        if selected_raw is None
        else summary(selected_raw, "selected projection idea_status_detail")
    )
    final_summary = (
        None
        if final_raw is None
        else summary(final_raw, "final public response idea_status_detail")
    )
    top_level_summary = (
        None
        if top_level_raw is None
        else summary(top_level_raw, "actual top-level idea_status_detail")
    )
    packet_to_public_exact = bool(
        packet_summary is not None
        and selected_summary is not None
        and final_summary is not None
        and top_level_summary is not None
        and sha256_json(packet_summary) == sha256_json(selected_summary)
        and sha256_json(packet_summary) == sha256_json(final_summary)
        and sha256_json(packet_summary) == sha256_json(top_level_summary)
    )
    return {
        "coverage_exact": coverage_exact and summary_coverage_exact,
        "selection_exact": selection_exact,
        "r_to_packet_exact": r_to_packet_exact,
        "packet_to_public_exact": packet_to_public_exact,
    }


def _importance_decision(
    state: Mapping[str, Any], cause_id: str
) -> dict[str, Any] | None:
    values = [
        item
        for item in state["decisions"]
        if item.get("cause_evidence_id") == cause_id
    ]
    return dict(values[0]) if len(values) == 1 else None


def _oriented_importance_relations(
    state: Mapping[str, Any], left_cause_id: str, right_cause_id: str
) -> list[tuple[str, str, str]]:
    inverse = {
        "dominates": "dominated_by",
        "dominated_by": "dominates",
        "tied": "tied",
        "incomparable": "incomparable",
    }
    values: list[tuple[str, str, str]] = []
    for relation in state["relations"]:
        if (
            relation.get("left_cause_id") == left_cause_id
            and relation.get("right_cause_id") == right_cause_id
        ):
            values.append(
                (
                    str(relation.get("left_causal_signature")),
                    str(relation.get("right_causal_signature")),
                    str(relation.get("relation")),
                )
            )
        elif (
            relation.get("left_cause_id") == right_cause_id
            and relation.get("right_cause_id") == left_cause_id
        ):
            values.append(
                (
                    str(relation.get("right_causal_signature")),
                    str(relation.get("left_causal_signature")),
                    inverse[str(relation.get("relation"))],
                )
            )
    return values


def _importance_tied(
    state: Mapping[str, Any], left_cause_id: str, right_cause_id: str
) -> bool | None:
    for cause_id in (left_cause_id, right_cause_id):
        decision = _importance_decision(state, cause_id)
        if decision is None or decision.get("fully_measured") is not True:
            return None
    relations = _oriented_importance_relations(
        state, left_cause_id, right_cause_id
    )
    if not relations or any(value[2] == "incomparable" for value in relations):
        return None
    left_signatures = {
        str(item["causal_signature"])
        for item in state["profiles"]
        if item.get("cause_evidence_id") == left_cause_id
    }
    right_signatures = {
        str(item["causal_signature"])
        for item in state["profiles"]
        if item.get("cause_evidence_id") == right_cause_id
    }
    covered_left = {left for left, _, relation in relations if relation == "tied"}
    covered_right = {right for _, right, relation in relations if relation == "tied"}
    if not left_signatures or not right_signatures:
        return None
    return bool(
        all(relation == "tied" for _, _, relation in relations)
        and covered_left == left_signatures
        and covered_right == right_signatures
    )


def _importance_dominates(
    state: Mapping[str, Any], higher_cause_id: str, lower_cause_id: str
) -> bool | None:
    for cause_id in (higher_cause_id, lower_cause_id):
        decision = _importance_decision(state, cause_id)
        if decision is None or decision.get("fully_measured") is not True:
            return None
    lower_signatures = {
        str(item["causal_signature"])
        for item in state["profiles"]
        if item.get("cause_evidence_id") == lower_cause_id
    }
    if not lower_signatures:
        return None
    relations = _oriented_importance_relations(
        state, higher_cause_id, lower_cause_id
    )
    dominated_lower = {
        lower_signature
        for _, lower_signature, relation in relations
        if relation == "dominates"
    }
    if dominated_lower == lower_signatures:
        return True
    comparable = {
        lower_signature
        for _, lower_signature, relation in relations
        if relation != "incomparable"
    }
    if comparable == lower_signatures:
        return False
    return None


def _public_selections(actual: Mapping[str, Any]) -> list[dict[str, Any]]:
    projection = actual.get("projection")
    raw = projection.get("selected_public_cause_selections") if isinstance(projection, Mapping) else None
    return [dict(item) for item in raw] if isinstance(raw, list) and all(isinstance(item, Mapping) for item in raw) else []


def _per_cause_boundary_state(
    cause: Mapping[str, Any],
) -> tuple[dict[str, Any] | None, dict[str, Any] | None, bool]:
    cause_id = _cause_id(cause)
    value = cause.get("p")
    if not isinstance(value, Mapping) or value.get("cause_evidence_id") != cause_id:
        return None, None, False
    packet = value.get("packet_selection")
    public = value.get("public_selection")
    packet_value = dict(packet) if isinstance(packet, Mapping) else None
    public_value = dict(public) if isinstance(public, Mapping) else None
    packet_selected = value.get("packet_selected")
    public_count = value.get("public_selection_count")
    public_ids = value.get("selected_public_cause_evidence_ids")
    valid = bool(
        isinstance(packet_selected, bool)
        and packet_selected == (packet_value is not None)
        and isinstance(public_count, int)
        and not isinstance(public_count, bool)
        and public_count in {0, 1}
        and isinstance(public_ids, list)
        and public_ids == ([cause_id] if public_count == 1 else [])
        and (public_value is not None) == (public_count == 1)
        and (
            public_value is None
            or public_value.get("cause_evidence_id") == cause_id
        )
        and (
            packet_value is None
            or packet_value.get("cause_evidence_id") == cause_id
        )
    )
    return packet_value, public_value, valid


def _validate_object_constraint(value: Mapping[str, Any], label: str) -> None:
    required = _atom_set(value.get("required"), f"{label}.required")
    allowed = _atom_set(value.get("allowed"), f"{label}.allowed")
    if not required:
        raise ContractError(f"{label} must require at least one semantic object")
    if not required.issubset(allowed):
        raise ContractError(f"{label}.required must be a subset of allowed")


def validate_typed_oracle_label(
    label: Mapping[str, Any], case: Mapping[str, Any]
) -> None:
    disposition = label.get("disposition")
    meanings = _objects(label.get("meanings"), "oracle meanings")
    ids = [str(item["meaning_id"]) for item in meanings]
    if len(ids) != len(set(ids)):
        raise ContractError("typed oracle has duplicate meaning_id values")
    known = set(ids)
    meaning_by_id = {str(item["meaning_id"]): item for item in meanings}
    board = chess.Board(str(case["fen"]))
    dominance: dict[str, set[str]] = {meaning_id: set() for meaning_id in known}
    realization_owner: dict[str, str] = {}
    for meaning in meanings:
        meaning_id = str(meaning["meaning_id"])
        generation = meaning.get("generation_policy")
        exposure = meaning.get("exposure_policy")
        if (generation == "forbidden") != (exposure == "forbidden"):
            raise ContractError(
                f"oracle meaning {meaning_id} must pair forbidden generation and exposure"
            )
        if generation != "forbidden" and exposure not in {
            "primary",
            "complementary",
            "diagnostic_only",
            "fallback_only",
        }:
            raise ContractError(f"oracle meaning {meaning_id} has an invalid policy pair")
        dominated_by = meaning.get("dominated_by", [])
        if exposure == "fallback_only" and not dominated_by:
            raise ContractError(
                f"oracle fallback meaning {meaning_id} has no explicit dominator"
            )
        if exposure != "fallback_only" and dominated_by:
            raise ContractError(
                f"oracle non-fallback meaning {meaning_id} declares dominance"
            )
        for dominator in meaning.get("dominated_by", []):
            if dominator not in known or dominator == meaning_id:
                raise ContractError(f"oracle meaning {meaning_id} has an invalid dominator")
            dominance[meaning_id].add(str(dominator))
        for realization in _objects(meaning.get("realizations"), "oracle realizations"):
            realization_key = sha256_json(realization)
            previous_owner = realization_owner.get(realization_key)
            if previous_owner is not None:
                raise ContractError(
                    "typed oracle assigns one semantic realization more than once"
                )
            realization_owner[realization_key] = meaning_id
            comparison = _object(realization.get("comparison"), "oracle comparison")
            for field in ("reference_root_move", "candidate_root_move", "event_root_move"):
                move = chess.Move.from_uci(str(comparison[field]))
                if not board.is_legal(move):
                    raise ContractError(f"oracle {meaning_id} has illegal {field}")
            actor = _object(
                _objects(realization.get("channels"), "oracle channels")[0].get("actor"),
                "oracle actor",
            )
            event_move = str(comparison["event_root_move"])
            if actor.get("move") != event_move:
                raise ContractError(f"oracle {meaning_id} actor move differs from its event root")
            if actor.get("from") != event_move[:2] or actor.get("to") != event_move[2:4]:
                raise ContractError(f"oracle {meaning_id} actor squares differ from its event root")
            piece = board.piece_at(chess.parse_square(event_move[:2]))
            if piece is None or chess.piece_name(piece.piece_type) != actor.get("piece"):
                raise ContractError(f"oracle {meaning_id} actor piece is not on its source square")
            expected_side = "white" if piece.color == chess.WHITE else "black"
            if actor.get("side") != expected_side or comparison.get("mover") != expected_side:
                raise ContractError(f"oracle {meaning_id} actor side or mover is inverted")
            for channel in _objects(realization.get("channels"), "oracle channels"):
                channel_actor = _object(channel.get("actor"), "oracle actor")
                if channel_actor != actor:
                    raise ContractError(f"oracle {meaning_id} realizations disagree on actor")
                for field in ("targets", "mechanisms", "consequences"):
                    _validate_object_constraint(
                        _object(channel.get(field), f"oracle {field}"),
                        f"oracle {meaning_id}.{field}",
                    )
    dominance_visiting: set[str] = set()
    dominance_visited: set[str] = set()

    def visit_dominance(node: str) -> None:
        if node in dominance_visiting:
            raise ContractError("oracle fallback dominance is cyclic")
        if node in dominance_visited:
            return
        dominance_visiting.add(node)
        for parent in dominance[node]:
            visit_dominance(parent)
        dominance_visiting.remove(node)
        dominance_visited.add(node)

    for meaning_id in sorted(known):
        visit_dominance(meaning_id)
    for meaning in meanings:
        meaning_id = str(meaning["meaning_id"])
        for dominator in meaning.get("dominated_by", []):
            if meaning_by_id[str(dominator)].get("exposure_policy") not in {
                "primary",
                "complementary",
            }:
                raise ContractError(
                    f"oracle fallback {meaning_id} has a non-public dominator"
                )
    priority = _object(label.get("priority"), "oracle priority")
    tie_members: set[str] = set()
    for group in priority.get("tie_groups", []):
        if not isinstance(group, list) or len(group) < 2 or not set(group).issubset(known):
            raise ContractError("oracle priority has an invalid tie group")
        if any(
            meaning_by_id[str(item)].get("exposure_policy")
            in {"diagnostic_only", "forbidden"}
            for item in group
        ):
            raise ContractError("oracle priority ties a non-public meaning")
        overlap = tie_members.intersection(group)
        if overlap:
            raise ContractError("oracle priority tie groups overlap")
        tie_members.update(str(item) for item in group)
    adjacency: dict[str, set[str]] = {meaning_id: set() for meaning_id in known}
    for edge in priority.get("precedes", []):
        item = _object(edge, "oracle priority edge")
        higher = str(item["higher"])
        lower = str(item["lower"])
        if higher not in known or lower not in known or higher == lower:
            raise ContractError("oracle priority has an invalid precedence edge")
        if any(
            meaning_by_id[item].get("exposure_policy")
            in {"diagnostic_only", "forbidden"}
            for item in (higher, lower)
        ):
            raise ContractError("oracle priority orders a non-public meaning")
        adjacency[higher].add(lower)
    visiting: set[str] = set()
    visited: set[str] = set()

    def visit(node: str) -> None:
        if node in visiting:
            raise ContractError("oracle priority precedence is cyclic")
        if node in visited:
            return
        visiting.add(node)
        for child in adjacency[node]:
            visit(child)
        visiting.remove(node)
        visited.add(node)

    for meaning_id in sorted(known):
        visit(meaning_id)
    for group in priority.get("tie_groups", []):
        members = set(str(item) for item in group)
        for source in members:
            reachable: set[str] = set()
            stack = list(adjacency[source])
            while stack:
                node = stack.pop()
                if node in reachable:
                    continue
                reachable.add(node)
                stack.extend(adjacency[node])
            if reachable.intersection(members):
                raise ContractError("oracle tie group contains a precedence relation")
    top = priority.get("required_top_one_of", [])
    if not isinstance(top, list) or not set(top).issubset(known):
        raise ContractError("oracle priority has invalid required_top_one_of")
    if disposition == "answerable" and not top:
        raise ContractError("answerable typed oracle requires a top meaning")
    if disposition != "answerable" and top:
        raise ContractError("non-answerable typed oracle cannot require a top meaning")
    for group in priority.get("tie_groups", []):
        group_members = {str(item) for item in group}
        if set(top).intersection(group_members) and not group_members.issubset(
            set(top)
        ):
            raise ContractError(
                "oracle required top must include the whole tied frontier"
            )
    if disposition == "must_abstain" and any(
        item.get("exposure_policy")
        in {"primary", "complementary", "fallback_only"}
        for item in meanings
    ):
        raise ContractError(
            "must-abstain typed oracle cannot require player-facing exposure"
        )
    for meaning_id in top:
        meaning = next(item for item in meanings if item["meaning_id"] == meaning_id)
        if meaning.get("generation_policy") != "required" or meaning.get("exposure_policy") != "primary":
            raise ContractError(
                "answerable top meanings must be required primary meanings"
            )


def judge_typed_case(
    *,
    case: Mapping[str, Any],
    label: Mapping[str, Any],
    actual: Mapping[str, Any],
    oracle_label_sha256: str | None = None,
    actual_view_sha256: str | None = None,
) -> dict[str, Any]:
    """Judge one v3 Cause cascade without using prose or support identity.

    C is matched only against independently verified owned direct channels.
    R is judged from the native pre-packet selection. P is only an exact-copy
    boundary.
    """

    validate_typed_oracle_label(label, case)
    case_id = str(case["case_id"])
    disposition = str(label["disposition"])
    runtime_status = str(actual.get("status"))
    oracle_hash = oracle_label_sha256 or sha256_json(dict(label))
    actual_hash = actual_view_sha256 or sha256_json(dict(actual))
    base = {
        "schema_version": "chesstory.eval.cause-audit-judgment.v3",
        "case_id": case_id,
        "partition": str(case["partition"]),
        "disposition": disposition,
        "oracle_label_sha256": oracle_hash,
        "actual_view_sha256": actual_hash,
        "runtime_status": runtime_status,
        "semantic_assessment": "typed-same-channel-exact",
    }
    probes = actual.get("probe_closure")
    if runtime_status != "complete" or not (
        isinstance(probes, Mapping) and probes.get("all_closed") is True
    ):
        return {
            **base,
            "status": "runtime_unavailable",
            "endpoint_status": "not_scored",
            "meaning_matches": [],
            "unmatched_generated_cause_ids": [],
            "unmatched_public_cause_ids": [],
            "priority": {"status": "not_scored", "violations": []},
            "cascade": {"c": 0, "jp": 0, "ja": 0, "r": 0, "packet": 0, "p": 0},
            "errors": [{"code": "runtime_unavailable", "stage": "C"}],
            "first_failure_stage": "C",
        }
    causes = [dict(item) for item in _objects(actual.get("causes"), "actual Causes")]
    cause_by_id = {_cause_id(item): item for item in causes}
    if len(cause_by_id) != len(causes):
        raise ContractError("actual view has duplicate Cause IDs")
    snapshot_index = comparison_endpoint_snapshot_index(actual)
    verified_channels = {
        _cause_id(item): _verified_owned_channels(item, snapshot_index)
        for item in causes
    }
    if disposition == "unresolved":
        return {
            **base,
            "status": "unresolved",
            "endpoint_status": "not_scored",
            "meaning_matches": [],
            "unmatched_generated_cause_ids": [],
            "unmatched_public_cause_ids": [],
            "priority": {"status": "not_scored", "violations": []},
            "cascade": {"c": 0, "jp": 0, "ja": 0, "r": 0, "packet": 0, "p": 0},
            "errors": [],
            "first_failure_stage": None,
        }

    importance = _importance_state(actual, case)
    idea_units = _idea_unit_state(actual, importance)
    cause_disposition = _cause_disposition_ledger_state(actual, causes)
    meanings = _objects(label.get("meanings"), "oracle meanings")
    positive = [item for item in meanings if item.get("generation_policy") != "forbidden"]
    forbidden = [item for item in meanings if item.get("generation_policy") == "forbidden"]
    edges, matching_realizations = _meaning_edges(positive, causes, verified_channels)
    ordered_meanings = sorted(
        (str(item["meaning_id"]) for item in positive),
        key=lambda meaning_id: (
            0
            if next(item for item in positive if item["meaning_id"] == meaning_id).get(
                "generation_policy"
            )
            == "required"
            else 1,
            meaning_id,
        ),
    )
    assignment = _maximum_matching(ordered_meanings, edges)
    errors: list[dict[str, Any]] = []
    if not cause_disposition["coverage_exact"]:
        _add_error(errors, "cause_disposition_c_coverage_mismatch", "R")
    if not cause_disposition["selection_exact"]:
        _add_error(errors, "cause_disposition_selection_mismatch", "R")
    if not cause_disposition["r_to_packet_exact"]:
        _add_error(errors, "cause_disposition_r_to_packet_mismatch", "P")
    if not cause_disposition["packet_to_public_exact"]:
        _add_error(errors, "cause_disposition_packet_to_public_mismatch", "P")
    if not idea_units["r_to_packet_exact"]:
        _add_error(errors, "idea_unit_r_to_packet_mismatch", "P")
    if not idea_units["packet_to_public_exact"]:
        _add_error(errors, "idea_unit_packet_to_public_mismatch", "P")
    if not idea_units["public_membership_closed"]:
        _add_error(errors, "idea_unit_membership_mismatch", "P")
    if not idea_units["idea_importance_r_to_packet_exact"]:
        _add_error(errors, "idea_importance_r_to_packet_mismatch", "P")
    if not idea_units["idea_importance_packet_to_public_exact"]:
        _add_error(errors, "idea_importance_packet_to_public_mismatch", "P")
    for meaning in positive:
        meaning_id = str(meaning["meaning_id"])
        if meaning.get("generation_policy") == "required" and meaning_id not in assignment:
            realizations = _objects(meaning.get("realizations"), "oracle realizations")
            matching_header = any(
                _c_header_matches_realization(cause["c"], realization)
                for cause in causes
                for realization in realizations
            )
            admission_dropped = any(
                _c_header_matches_realization(cause["c"], realization)
                and _channels_exact_match(
                    _pre_admission_endpoint_channels(cause, snapshot_index),
                    _objects(realization.get("channels"), "oracle channels"),
                    require_played_change=False,
                )
                for cause in causes
                for realization in realizations
            )
            cause_not_generated = any(
                _snapshot_supports_realization(snapshot, realization)
                for snapshot in snapshot_index.values()
                for realization in realizations
            ) and not matching_header
            _add_error(
                errors,
                (
                    "cause_admission_dropped"
                    if admission_dropped
                    else "cause_not_generated"
                    if cause_not_generated
                    else "missing_required_meaning"
                ),
                "C",
                meaning_id=meaning_id,
            )
    forbidden_cause_ids: set[str] = set()
    for meaning in forbidden:
        meaning_id = str(meaning["meaning_id"])
        for cause in causes:
            cause_id = _cause_id(cause)
            if any(
                _c_value_matches_realization(cause["c"], verified_channels[cause_id], realization)
                for realization in _objects(meaning.get("realizations"), "oracle realizations")
            ):
                forbidden_cause_ids.add(cause_id)
                _add_error(
                    errors,
                    "forbidden_generated_meaning",
                    "C",
                    meaning_id=meaning_id,
                    cause_evidence_id=cause_id,
                )
    matched_cause_ids = set(assignment.values()) | forbidden_cause_ids
    unmatched_generated = sorted(set(cause_by_id) - matched_cause_ids)
    for cause_id in unmatched_generated:
        _add_error(errors, "unmapped_generated_cause", "C", cause_evidence_id=cause_id)

    meaning_by_id = {str(item["meaning_id"]): item for item in meanings}
    meaning_matches: list[dict[str, Any]] = []
    for meaning_id in sorted(assignment):
        cause_id = assignment[meaning_id]
        cause = cause_by_id[cause_id]
        meaning = meaning_by_id[meaning_id]
        jp = _linked_at_jp(cause)
        ja = _linked_at_ja(cause)
        forward_required = _requires_forward_survival(
            meaning, assignment, cause_by_id
        )
        if forward_required and not jp:
            _add_error(errors, "jp_loss", "Jp", meaning_id=meaning_id, cause_evidence_id=cause_id)
        if forward_required and jp and not ja:
            _add_error(errors, "ja_loss", "Ja", meaning_id=meaning_id, cause_evidence_id=cause_id)
        native = _r_selection(cause)
        valid_native = False
        if native is not None:
            valid_native = bool(
                native.get("cause_evidence_id") == cause_id
                and _selection_uses_admitted_host(native, cause)
                and any(
                    _selection_realization_matches(
                        native,
                        realization,
                        cause["c"],
                        verified_channels[cause_id],
                    )
                    for realization in matching_realizations[(meaning_id, cause_id)]
                )
            )
            if not valid_native:
                _add_error(
                    errors,
                    "wrong_r_semantics",
                    "R",
                    meaning_id=meaning_id,
                    cause_evidence_id=cause_id,
                )
            policy = meaning.get("exposure_policy")
            if policy in {"diagnostic_only", "forbidden"}:
                _add_error(
                    errors,
                    "forbidden_r_exposure",
                    "R",
                    meaning_id=meaning_id,
                    cause_evidence_id=cause_id,
                )
            if policy == "primary" and native.get("exposure") != "primary":
                _add_error(
                    errors,
                    "wrong_exposure_tier",
                    "R",
                    meaning_id=meaning_id,
                    cause_evidence_id=cause_id,
                )
            if policy == "complementary" and native.get("exposure") != "complementary":
                _add_error(
                    errors,
                    "wrong_exposure_tier",
                    "R",
                    meaning_id=meaning_id,
                    cause_evidence_id=cause_id,
                )
            if policy == "fallback_only" and _active_fallback_dominator(
                meaning, assignment, cause_by_id
            ):
                _add_error(
                    errors,
                    "dominated_fallback_exposed",
                    "R",
                    meaning_id=meaning_id,
                    cause_evidence_id=cause_id,
                )
        elif forward_required:
            _add_error(
                errors,
                "r_loss",
                "R",
                meaning_id=meaning_id,
                cause_evidence_id=cause_id,
            )
        packet, _, _ = _per_cause_boundary_state(cause)
        meaning_matches.append(
            {
                "meaning_id": meaning_id,
                "cause_evidence_id": cause_id,
                "jp_survived": jp,
                "ja_survived": ja,
                "r_selected": native is not None,
                "r_semantics_matched": valid_native,
                "packet_selected": packet is not None,
                "public_selected": any(
                    item.get("cause_evidence_id") == cause_id for item in _public_selections(actual)
                ),
            }
        )

    top_level_r = actual.get("r_native_cause_selections")
    top_level_r_values = (
        [dict(item) for item in top_level_r]
        if isinstance(top_level_r, list) and all(isinstance(item, Mapping) for item in top_level_r)
        else []
    )
    all_per_cause_r = [selection for cause in causes if (selection := _r_selection(cause)) is not None]
    if _selection_multiset(top_level_r_values) != _selection_multiset(all_per_cause_r):
        _add_error(errors, "r_selection_set_mismatch", "R")

    r_selected_cause_ids = [
        str(item["cause_evidence_id"])
        for item in all_per_cause_r
        if isinstance(item.get("cause_evidence_id"), str)
    ]
    importance_authority_error = False
    if Counter(importance["selected"]) != Counter(r_selected_cause_ids):
        importance_authority_error = True
        _add_error(errors, "importance_r_selection_set_mismatch", "R")
    else:
        for selection in all_per_cause_r:
            cause_id = str(selection["cause_evidence_id"])
            channels = selection.get("channels")
            if not isinstance(channels, list) or not all(
                isinstance(item, Mapping)
                and isinstance(item.get("causal_signature"), str)
                for item in channels
            ):
                raise ContractError("R selection has invalid importance channels")
            decision = _importance_decision(importance, cause_id)
            if decision is None or {
                str(item["causal_signature"]) for item in channels
            } != set(decision["measured_channel_signatures"]) | set(
                decision["unmeasured_channel_signatures"]
            ):
                raise ContractError(
                    "R-native importance channels do not match the selected Cause"
                )

    r_public_importance = importance["r_public"]
    packet_native_importance = importance["packet_native"]
    packet_public_importance = importance["packet_public"]
    public_importance = importance["public"]
    public_payload_renderable = importance["public_payload_renderable"] is True
    if r_selected_cause_ids:
        if (
            packet_native_importance is None
            or sha256_json(importance["native"])
            != sha256_json(packet_native_importance)
            or packet_public_importance is None
            or sha256_json(r_public_importance)
            != sha256_json(packet_public_importance)
        ):
            _add_error(errors, "importance_r_to_packet_mismatch", "P")
        if public_payload_renderable and (
            packet_public_importance is None
            or public_importance is None
            or sha256_json(packet_public_importance) != sha256_json(public_importance)
        ):
            _add_error(errors, "importance_packet_to_public_mismatch", "P")
        if not public_payload_renderable and public_importance is not None:
            _add_error(errors, "importance_packet_to_public_mismatch", "P")
    else:
        if packet_native_importance is not None and sha256_json(
            importance["native"]
        ) != sha256_json(packet_native_importance):
            _add_error(errors, "importance_r_to_packet_mismatch", "P")
        if packet_public_importance is not None and sha256_json(
            r_public_importance
        ) != sha256_json(packet_public_importance):
            _add_error(errors, "importance_r_to_packet_mismatch", "P")
        if public_importance is not None:
            _add_error(errors, "importance_packet_to_public_mismatch", "P")

    cause_to_meaning = {
        cause_id: meaning_id for meaning_id, cause_id in assignment.items()
    }
    idea_importance = idea_units["importance"]
    cause_to_lead = idea_units["cause_to_lead"]
    members_by_lead = idea_units["members_by_lead"]
    selected_leads = [str(item) for item in idea_importance["selected"]]
    unit_meanings_by_lead = {
        lead: {
            cause_to_meaning[cause_id]
            for cause_id in members_by_lead.get(lead, [])
            if cause_id in cause_to_meaning
        }
        for lead in selected_leads
    }
    selected_meanings = (
        set().union(*unit_meanings_by_lead.values()) if selected_leads else set()
    )
    lead_mapping_closed = all(lead in cause_to_meaning for lead in selected_leads)
    idea_priority_closed = bool(
        idea_units["native_structure_closed"]
        and idea_units["public_membership_closed"]
        and idea_units["r_to_packet_exact"]
        and idea_units["packet_to_public_exact"]
        and idea_units["idea_importance_r_to_packet_exact"]
        and idea_units["idea_importance_packet_to_public_exact"]
        and lead_mapping_closed
    )
    top = [str(item) for item in label["priority"]["required_top_one_of"]]
    importance_not_scored: set[str] = set()
    if disposition == "answerable" and not idea_priority_closed:
        importance_not_scored.add("incomplete_idea_unit_authority")
    if disposition == "answerable" and idea_priority_closed and not any(
        meaning_id in selected_meanings for meaning_id in top
    ):
        _add_error(errors, "missing_top_r_selection", "R")

    priority_violations: list[dict[str, Any]] = []
    if disposition == "answerable" and idea_priority_closed and any(
        meaning_id in selected_meanings for meaning_id in top
    ):
        unique_top = idea_importance["native"].get("unique_top")
        if importance_authority_error:
            importance_not_scored.add("authority_mismatch")
        elif unique_top is None:
            if any(
                decision.get("fully_measured") is not True
                for decision in idea_importance["decisions"]
            ):
                importance_not_scored.add("unmeasured")
            else:
                frontier_leads = [
                    str(item)
                    for item in idea_importance["native"]["frontier_cause_ids"]
                ]
                tied_frontier = bool(frontier_leads) and all(
                    _importance_tied(idea_importance, left, right) is True
                    for index, left in enumerate(frontier_leads)
                    for right in frontier_leads[index + 1 :]
                )
                if (
                    tied_frontier
                    and all(
                        unit_meanings_by_lead.get(lead, set()).intersection(top)
                        for lead in frontier_leads
                    )
                ):
                    pass
                elif len(frontier_leads) > 1:
                    importance_not_scored.add("incomparable")
                else:
                    importance_authority_error = True
                    _add_error(errors, "importance_resolution_inconsistent", "R")
        else:
            actual_top_meanings = unit_meanings_by_lead.get(str(unique_top), set())
            lead_meaning = cause_to_meaning.get(str(unique_top))
            if lead_meaning is None or not actual_top_meanings:
                importance_authority_error = True
                _add_error(
                    errors,
                    "importance_resolution_inconsistent",
                    "R",
                    cause_evidence_id=str(unique_top),
                )
            elif not actual_top_meanings.intersection(top):
                expected_top = sorted(top)[0]
                violation = {
                    "code": "importance_inversion",
                    "higher": expected_top,
                    "lower": lead_meaning,
                }
                priority_violations.append(violation)
                _add_error(errors, "importance_inversion", "R")

    for group in (
        label["priority"]["tie_groups"] if idea_priority_closed else []
    ):
        selected = [
            str(meaning_id)
            for meaning_id in group
            if str(meaning_id) in selected_meanings
        ]
        for index, left in enumerate(selected):
            for right in selected[index + 1 :]:
                left_lead = cause_to_lead.get(assignment[left])
                right_lead = cause_to_lead.get(assignment[right])
                if left_lead is None or right_lead is None:
                    importance_not_scored.add("unmapped_idea_unit_tie")
                    continue
                if left_lead == right_lead:
                    continue
                relation = _importance_tied(
                    idea_importance, left_lead, right_lead
                )
                if relation is None:
                    importance_not_scored.add("incomparable_or_unmeasured_tie")
                elif not relation:
                    violation = {
                        "code": "tie_relation_mismatch",
                        "meaning_ids": sorted((left, right)),
                    }
                    if violation not in priority_violations:
                        priority_violations.append(violation)
                    _add_error(errors, "tie_relation_mismatch", "R")
    for edge in (
        label["priority"]["precedes"] if idea_priority_closed else []
    ):
        higher = str(edge["higher"])
        lower = str(edge["lower"])
        if higher in selected_meanings and lower in selected_meanings:
            higher_lead = cause_to_lead.get(assignment[higher])
            lower_lead = cause_to_lead.get(assignment[lower])
            if higher_lead is None or lower_lead is None:
                importance_not_scored.add("unmapped_idea_unit_order")
                continue
            if higher_lead == lower_lead:
                continue
            relation = _importance_dominates(
                idea_importance, higher_lead, lower_lead
            )
            if relation is None:
                importance_not_scored.add("incomparable_or_unmeasured_order")
            elif not relation:
                violation = {
                    "code": "importance_inversion",
                    "higher": higher,
                    "lower": lower,
                }
                priority_violations.append(violation)
                _add_error(errors, "importance_inversion", "R")

    all_packet: list[dict[str, Any]] = []
    all_per_cause_public: list[dict[str, Any]] = []
    for cause in causes:
        cause_id = _cause_id(cause)
        native = _r_selection(cause)
        packet, per_cause_public, boundary_valid = _per_cause_boundary_state(cause)
        if not boundary_valid:
            _add_error(
                errors,
                "packet_to_public_mismatch",
                "P",
                cause_evidence_id=cause_id,
            )
        if _selection_multiset([native] if native is not None else []) != _selection_multiset(
            [packet] if packet is not None else []
        ):
            _add_error(
                errors,
                "r_to_packet_mismatch",
                "P",
                cause_evidence_id=cause_id,
            )
        if public_payload_renderable and _selection_multiset(
            [packet] if packet is not None else []
        ) != _selection_multiset(
            [per_cause_public] if per_cause_public is not None else []
        ):
            _add_error(
                errors,
                "packet_to_public_mismatch",
                "P",
                cause_evidence_id=cause_id,
            )
        if not public_payload_renderable and per_cause_public is not None:
            _add_error(
                errors,
                "packet_to_public_mismatch",
                "P",
                cause_evidence_id=cause_id,
            )
        if packet is not None:
            all_packet.append(packet)
        if per_cause_public is not None:
            all_per_cause_public.append(per_cause_public)
    public_values = _public_selections(actual)
    if _selection_multiset(all_per_cause_r) != _selection_multiset(all_packet):
        _add_error(errors, "r_to_packet_mismatch", "P")
    if public_payload_renderable and _selection_multiset(
        all_packet
    ) != _selection_multiset(all_per_cause_public):
        _add_error(errors, "packet_to_public_mismatch", "P")
    if _selection_multiset(all_per_cause_public) != _selection_multiset(public_values):
        _add_error(errors, "packet_to_public_mismatch", "P")
    projection = actual.get("projection")
    raw_idea_count = (
        projection.get("raw_public_idea_count")
        if isinstance(projection, Mapping)
        else None
    )
    parsed_idea_count = (
        projection.get("parsed_public_idea_count")
        if isinstance(projection, Mapping)
        else None
    )
    public_idea_parse_closed = bool(
        isinstance(projection, Mapping)
        and projection.get("public_ideas_parse_closed") is True
        and isinstance(raw_idea_count, int)
        and not isinstance(raw_idea_count, bool)
        and raw_idea_count >= 0
        and isinstance(parsed_idea_count, int)
        and not isinstance(parsed_idea_count, bool)
        and parsed_idea_count >= 0
        and raw_idea_count == parsed_idea_count
        and parsed_idea_count == len(public_values)
    )
    if not public_idea_parse_closed:
        _add_error(errors, "public_idea_parse_mismatch", "P")
    raw_public_ids = (
        projection.get("selected_public_cause_evidence_ids")
        if isinstance(projection, Mapping)
        else None
    )
    typed_public_ids = [item.get("cause_evidence_id") for item in public_values]
    if not isinstance(raw_public_ids, list) or Counter(raw_public_ids) != Counter(typed_public_ids):
        _add_error(errors, "public_id_set_mismatch", "P")

    mapped_public_ids = {
        item["cause_evidence_id"] for item in meaning_matches if item["public_selected"]
    }
    unmatched_public = sorted(
        {
            str(item.get("cause_evidence_id"))
            for item in public_values
            if isinstance(item.get("cause_evidence_id"), str)
        }
        - mapped_public_ids
    )
    for cause_id in unmatched_public:
        _add_error(errors, "unmapped_public_cause", "R", cause_evidence_id=cause_id)

    endpoint_status = "pass"
    if disposition == "answerable" and not any(
        item["meaning_id"] in top and item["public_selected"] and item["r_semantics_matched"]
        for item in meaning_matches
    ):
        endpoint_status = "fail"
    elif disposition == "answerable" and importance_not_scored:
        endpoint_status = "not_scored"
    if disposition == "must_abstain" and public_values:
        endpoint_status = "fail"
        _add_error(errors, "abstention_violation", "R")
    errors.sort(
        key=lambda item: (
            STAGE_ORDER[str(item["stage"])],
            str(item["code"]),
            str(item.get("meaning_id", "")),
            str(item.get("cause_evidence_id", "")),
        )
    )
    first_failure = errors[0]["stage"] if errors else None
    status = (
        "must_abstain_pass"
        if disposition == "must_abstain" and endpoint_status == "pass" and not errors
        else "mismatch"
        if errors or endpoint_status == "fail"
        else "unresolved"
        if endpoint_status == "not_scored"
        else "matched"
    )
    return {
        **base,
        "status": status,
        "endpoint_status": endpoint_status,
        "meaning_matches": meaning_matches,
        "unmatched_generated_cause_ids": unmatched_generated,
        "unmatched_public_cause_ids": unmatched_public,
        "priority": {
            "status": (
                "mismatch"
                if priority_violations or importance_authority_error
                else "not_scored"
                if importance_not_scored
                else "matched"
            ),
            "violations": priority_violations,
        },
        "cascade": {
            "c": len(assignment),
            "jp": sum(item["jp_survived"] for item in meaning_matches),
            "ja": sum(item["ja_survived"] for item in meaning_matches),
            "r": len(all_per_cause_r),
            "packet": len(all_packet),
            "p": len(public_values),
        },
        "errors": errors,
        "first_failure_stage": first_failure,
    }


def _canonical_candidate_atoms(value: Any, label: str) -> list[dict[str, str]]:
    atoms = [
        {"kind": kind, "key": key}
        for kind, key in sorted(_atom_set(value, label))
    ]
    return sorted(atoms, key=json_bytes)


def _canonical_candidate_channel(
    channel: Mapping[str, Any], *, played_change: Any = None
) -> dict[str, Any]:
    def semantic_objects(singular: str, plural: str) -> Any:
        present = [name for name in (singular, plural) if name in channel]
        if len(present) != 1:
            raise ContractError(
                f"open-world candidate channel requires exactly one {singular}/{plural} field"
            )
        return channel[present[0]]

    root_move, role = _line_root(channel.get("line"))
    if root_move is None or role is None:
        raise ContractError("open-world candidate channel has no semantic line")
    value = {
        "direct_change": channel.get("direct_change"),
        "actor": _canonical_candidate_atoms(channel.get("actor"), "candidate actor"),
        "targets": _canonical_candidate_atoms(
            semantic_objects("target", "targets"), "candidate targets"
        ),
        "mechanisms": _canonical_candidate_atoms(
            semantic_objects("mechanism", "mechanisms"),
            "candidate mechanisms",
        ),
        "consequences": _canonical_candidate_atoms(
            semantic_objects("consequence", "consequences"),
            "candidate consequences",
        ),
        "line": {"role": role, "root_move": root_move},
        "horizon": channel.get("horizon"),
    }
    if played_change is not None:
        value["played_change"] = played_change
    return value


def canonical_open_world_cause_observation(
    cause: Mapping[str, Any],
    snapshot_index: Mapping[str, Mapping[str, Any]],
) -> dict[str, Any]:
    """Project one unmatched Cause for the internal frozen arm inventory.

    This diagnostic projection records stage and selection behavior, but it is
    never the source-blind payload shown to the adjudicator.
    """

    c_value = _object(cause.get("c"), "actual Cause C state")
    comparison = _object(c_value.get("comparison"), "actual C comparison")
    binding = _object(c_value.get("binding"), "actual C binding")
    event = _object(binding.get("event_line"), "actual C event line")
    attribution = _object(c_value.get("attribution"), "actual C attribution")
    owned = _verified_owned_channels(cause, snapshot_index)
    c_channels = [_canonical_candidate_channel(item) for item in owned]
    c_channels.sort(key=json_bytes)
    c_semantics = {
        "cause_kind": c_value.get("kind"),
        "source_side": c_value.get("source_side"),
        "attribution": {
            "kind": attribution.get("kind"),
        },
        "comparison": {
            "kind": comparison.get("kind"),
            "mover": comparison.get("mover"),
            "reference_root_move": comparison.get("reference_root_move"),
            "candidate_root_move": comparison.get("candidate_root_move"),
            "event_role": event.get("role"),
            "event_root_move": event.get("root_move"),
        },
        "channels": c_channels,
    }
    native = _r_selection(cause)
    selection_variants: list[dict[str, Any]] = []
    if native is not None:
        if not _selection_is_bound_to_verified_c(native, c_value, owned):
            raise ContractError(
                "open-world R selection is not bound to its verified C channels"
            )
        selected_channels = _objects(
            native.get("channels"), "open-world selected channels"
        )
        projected_channels: list[dict[str, Any]] = []
        for selected in selected_channels:
            signature = selected.get("causal_signature")
            exact = [
                channel
                for channel in owned
                if channel.get("causal_signature") == signature
                and channel.get("direct_change") == selected.get("direct_change")
            ]
            if len(exact) != 1:
                raise ContractError(
                    "open-world selected channel is not owned by exactly one C channel"
                )
            projected_channels.append(
                _canonical_candidate_channel(
                    exact[0], played_change=selected.get("played_change")
                )
            )
        projected_channels.sort(key=json_bytes)
        selection_variants.append(
            {
                "exposure": native.get("exposure"),
                "effect_mode": native.get("effect_mode"),
                "channels": projected_channels,
            }
        )
    packet, public, _ = _per_cause_boundary_state(cause)
    stages = ["c_generated"]
    if _linked_at_jp(cause):
        stages.append("jp_direct")
    if _linked_at_ja(cause):
        stages.append("ja_admitted")
    if native is not None:
        stages.append("r_selected")
    if packet is not None:
        stages.append("packet_selected")
    if public is not None:
        stages.append("public_selected")
    return {
        "observed_stages": sorted(stages),
        "c_semantics": c_semantics,
        "selection_variants": selection_variants,
    }


def canonical_open_world_cause_candidate(
    cause: Mapping[str, Any],
    snapshot_index: Mapping[str, Mapping[str, Any]],
) -> dict[str, Any]:
    """Return the source-blind atomic C claim, excluding observed behavior."""

    observation = canonical_open_world_cause_observation(cause, snapshot_index)
    return {"c_semantics": observation["c_semantics"]}


def cause_matches_typed_oracle(
    cause: Mapping[str, Any],
    label: Mapping[str, Any],
    snapshot_index: Mapping[str, Mapping[str, Any]],
) -> bool:
    c_value = _object(cause.get("c"), "actual Cause C state")
    channels = _verified_owned_channels(cause, snapshot_index)
    return any(
        _c_value_matches_realization(c_value, channels, realization)
        for meaning in _objects(label.get("meanings"), "oracle meanings")
        for realization in _objects(meaning.get("realizations"), "oracle realizations")
    )


def open_world_meaning_matches_candidate(
    meaning: Mapping[str, Any], candidate_payload: Mapping[str, Any]
) -> bool:
    generation_policy = meaning.get("generation_policy")
    if generation_policy not in {"allowed", "required"}:
        return False
    if (
        meaning.get("exposure_policy") == "diagnostic_only"
        and generation_policy != "allowed"
    ):
        return False
    c_semantics = _object(candidate_payload.get("c_semantics"), "candidate C semantics")
    attribution = _object(c_semantics.get("attribution"), "candidate attribution")
    comparison = _object(c_semantics.get("comparison"), "candidate comparison")
    candidate_channels = _objects(c_semantics.get("channels"), "candidate C channels")
    # Every generated C belongs in the frozen inventory, including an unready
    # C that owns no sentence-capable direct channel.  Such a candidate has no
    # exact realization to certify: raw/context evidence must never be
    # promoted into C truth merely to make it adjudicable.
    if not candidate_channels:
        return False
    realizations = _objects(meaning.get("realizations"), "verified realizations")
    c_matching = [
        realization
        for realization in realizations
        if _c_semantics_matches_realization(
            c_semantics, candidate_channels, realization
        )
    ]
    if not c_matching or len(c_matching) != len(realizations):
        return False
    # Diagnostic meanings certify only the atomic C claim.  Their runtime
    # selection behavior is deliberately hidden from source-blind
    # adjudication, and indirect/shared comparisons do not own a public
    # played-effect orientation.  The realization schema still carries
    # effect fields so one shape can be used throughout the oracle, but those
    # fields have no adjudicative authority for diagnostic-only meanings.
    if meaning.get("exposure_policy") == "diagnostic_only":
        return True
    effect_mode = _source_blind_effect_mode(c_semantics)
    if effect_mode is None:
        return False
    return all(
        realization.get("effect_mode") == effect_mode
        and all(
            channel.get("played_change")
            == _source_blind_played_change(
                effect_mode, str(channel.get("direct_change"))
            )
            for channel in _objects(realization.get("channels"), "verified channels")
        )
        for realization in realizations
    )


def _source_blind_effect_mode(c_semantics: Mapping[str, Any]) -> str | None:
    attribution = _object(c_semantics.get("attribution"), "candidate attribution")
    comparison = _object(c_semantics.get("comparison"), "candidate comparison")
    comparison_kind = comparison.get("kind")
    source_side = c_semantics.get("source_side")
    attribution_kind = attribution.get("kind")
    event_root = comparison.get("event_root_move")
    reference_root = comparison.get("reference_root_move")
    candidate_root = comparison.get("candidate_root_move")

    # PlayedVsBest and PlayedVsAlternative both orient candidateLine as the
    # move actually played.  BestVsSecond orients referenceLine as played,
    # even though that line's semantic role remains `best_reference` rather
    # than `played`.  Comparison kind therefore owns the orientation; an
    # event-role string alone cannot determine polarity.
    if comparison_kind in {"played_vs_best", "played_vs_alternative"}:
        if source_side == "candidate" and event_root == candidate_root:
            if attribution_kind == "candidate_allows_liability":
                return "played_liability"
            if attribution_kind == "candidate_creates_value":
                return "played_value"
        if (
            source_side == "reference"
            and event_root == reference_root
            and attribution_kind == "reference_creates_resource"
        ):
            return "alternative_resource"
    elif (
        comparison_kind == "best_vs_second"
        and source_side == "reference"
        and event_root == reference_root
        and attribution_kind == "reference_creates_resource"
    ):
        return "played_value"

    # ReferenceVsAlternative, shared/mixed sources, and non-owning
    # attributions have diagnostic value only.  They cannot authorize a
    # player-facing effect mode without observing runtime behavior.
    return None


def _source_blind_played_change(
    effect_mode: str, direct_change: str
) -> str | None:
    if effect_mode == "alternative_resource":
        return "missed" if direct_change in {"occurred", "prevented", "maintained"} else None
    if effect_mode == "played_liability":
        return direct_change if direct_change in {"occurred", "lost", "refuted", "missed"} else None
    if effect_mode == "played_value":
        return direct_change if direct_change in {"occurred", "prevented", "maintained"} else None
    return None


__all__ = [
    "canonical_open_world_cause_candidate",
    "canonical_open_world_cause_observation",
    "cause_matches_typed_oracle",
    "judge_typed_case",
    "open_world_meaning_matches_candidate",
    "validate_typed_oracle_label",
]
