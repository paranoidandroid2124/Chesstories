from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor
import copy
import json
import tempfile
import threading
import unittest
from pathlib import Path

from chesstory_eval.cause_audit import (
    ACTUAL_VIEW_SCHEMA_VERSION,
    TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    TYPED_OPEN_WORLD_ADJUDICATION_SCHEMA_VERSION,
    TYPED_OPEN_WORLD_ARM_INVENTORY_SCHEMA_VERSION,
    TYPED_ORACLE_SCHEMA_VERSION,
    _atomic_cache_write,
    _inventory_expected_candidates,
    _judge_case,
    _load_open_world_arm_inventory,
    _load_typed_runtime_run,
    _load_typed_labels,
    _merge_candidate_payloads,
    _open_world_candidate_set_document,
    _schema_path,
    _schema_tools,
    _typed_oracle_run_manifest_document,
    _typed_base_oracle_binding,
    _verify_open_world_candidate_coverage,
    _verify_typed_oracle_run_manifest,
    compare_cause_audit,
)
from chesstory_eval.cause_semantics import (
    _active_fallback_dominator,
    _dominance_layers,
    _expected_importance_decisions,
    _importance_relation,
    _idea_unit_state,
    _importance_state,
    canonical_open_world_cause_candidate,
    canonical_open_world_cause_observation,
    cause_matches_typed_oracle,
    comparison_endpoint_snapshot_index,
    judge_typed_case,
    open_world_meaning_matches_candidate,
)
from chesstory_eval.hashing import json_bytes, sha256_file, sha256_json
from chesstory_eval.model import ContractError, IntegrityError
from chesstory_eval.native_diagnostic import native_sha256_json


ROOT = Path(__file__).resolve().parents[1]


def refresh_c_source_identity_digests(item: dict[str, object]) -> None:
    for layer in (
        "raw_owned_bindings",
        "pre_admission_owned_bindings",
        "owned_bindings",
    ):
        channel = item["c"]["objects"][layer][0]
        channel["evidence_identity_sha256"] = native_sha256_json(
            {
                "carrier": channel["carrier"],
                "provenance": channel["provenance"],
                "primitive_proof_source": channel["primitive_proof_source"],
            }
        )


def refresh_selection_lineage_from_c(item: dict[str, object]) -> None:
    channel = item["c"]["objects"]["owned_bindings"][0]

    def projected_ref(value: dict[str, object]) -> dict[str, object]:
        line = value.get("line")
        projected_line = None
        if isinstance(line, dict):
            projected_line = {
                "line_id": line.get("line_id", line.get("id")),
                "role": line.get("role"),
                "rank": line.get("rank"),
                "root_move": line.get("root_move"),
            }
        return {
            "id": value.get("id"),
            "producer": value.get("producer"),
            "layer": value.get("layer"),
            "scope": value.get("scope"),
            "line": projected_line,
        }

    shared = {
        "carrier": projected_ref(channel["carrier"]),
        "provenance": [projected_ref(value) for value in channel["provenance"]],
        "causal_signature": channel["causal_signature"],
        "direct_change": channel["direct_change"],
        "actor": copy.deepcopy(channel["actor"]),
        "targets": copy.deepcopy(channel["target"]),
        "mechanisms": copy.deepcopy(channel["mechanism"]),
        "consequences": copy.deepcopy(channel["consequence"]),
        "witnesses": copy.deepcopy(channel["witness"]),
        "line": copy.deepcopy(channel["line"]),
        "horizon": channel["horizon"],
        "proof_segment": copy.deepcopy(channel["proof_segment"]),
        "effect_descriptor": copy.deepcopy(channel["effect_descriptor"]),
        "importance_effect": copy.deepcopy(channel["importance_effect"]),
        "descriptor_ambiguous": channel["importance_descriptor_ambiguous"],
        "proof_segment_ambiguous": channel["proof_segment_ambiguous"],
    }
    selections = (
        item["r"].get("native_selection"),
        item["p"].get("packet_selection"),
        item["p"].get("public_selection"),
    )
    for selection_value in selections:
        if isinstance(selection_value, dict):
            played_change = selection_value["channels"][0]["played_change"]
            selection_value["channels"][0] = copy.deepcopy(shared)
            selection_value["channels"][0]["played_change"] = played_change


def selection(
    cause_id: str = "cause-1",
    cause_kind: str = "wrong_move_order",
    *,
    rank_field: str = "priority_rank",
    selection_order: int = 0,
    importance_units: int = 10,
    importance_target: str = "e5",
) -> dict[str, object]:
    reference = {
        "line_id": "reference-line",
        "role": "best_reference",
        "rank": 1,
        "root_move": "e2e4",
    }
    candidate = {
        "line_id": "played-line",
        "role": "played",
        "rank": 9,
        "root_move": "e2e3",
    }
    carrier = {
        "id": "relation-1",
        "producer": "move_motif_producer",
        "layer": "move_motif",
        "scope": "best_line",
        "line": copy.deepcopy(reference),
    }
    value = {
        "cause_evidence_id": cause_id,
        rank_field: 0,
        "selection_order": selection_order,
        "exposure": "primary",
        "effect_mode": "alternative_resource",
        "cause_kind": cause_kind,
        "source_side": "reference",
        "direct_effect_admission": {
            "status": "restricted",
            "causal_signatures": ["owned-channel"],
        },
        "attribution": {
            "kind": "reference_creates_resource",
            "root_move_matched": True,
            "direct_proof_eligible": True,
        },
        "host": {
            "claim_id": f"claim-{cause_id}",
            "family": "tactical",
            "tier": "primary",
        },
        "comparison": {
            "evidence_id": "comparison-1",
            "kind": "played_vs_best",
            "mover": "white",
            "reference": copy.deepcopy(reference),
            "candidate": copy.deepcopy(candidate),
            "event": copy.deepcopy(reference),
            "compared_move": "e2e3",
            "verdict": "mistake",
            "candidate_win_percent_delta_for_mover": -12.0,
            "win_percent_loss_for_mover": 12.0,
        },
        "comparison_semantic_key": (
            "PlayedVsBest|White|BestReference:1:e2e4|"
            "Played:9:e2e3|-0x1.8p3|Mistake|"
        ),
        "channels": [
            {
                "carrier": carrier,
                "provenance": [],
                "causal_signature": "owned-channel",
                "direct_change": "occurred",
                "played_change": "missed",
                "actor": [
                    {"kind": "move", "key": "e2e4"},
                    {"kind": "side", "key": "white"},
                    {"kind": "piece", "key": "pawn"},
                    {"kind": "square", "key": "e2"},
                    {"kind": "square", "key": "e4"},
                ],
                "targets": [{"kind": "square", "key": "e5"}],
                "mechanisms": [{"kind": "mechanism", "key": "tempo"}],
                "consequences": [{"kind": "consequence", "key": "initiative"}],
                "witnesses": [{"kind": "move", "key": "g8f6"}],
                "line": copy.deepcopy(reference),
                "horizon": "ply:1",
                "proof_segment": None,
                "effect_descriptor": {
                    "effect_scope": {
                        "primitive_kind": "structural_transition",
                        "target_signatures": [f"square:{importance_target}"],
                        "plan_ids": [],
                        "plan_result_semantic_key": None,
                        "strategic_axes": [],
                    },
                    "magnitude_status": "exact",
                    "measure": {
                        "kind": "structural_strength",
                        "units": importance_units,
                    },
                    "material_event_salience": None,
                },
                "importance_effect": {
                    "stake": "benefits:white",
                    "domain_kind": "structural",
                    "domain": "structural:roottransition:spacegain:gain:none",
                    "measure": {
                        "kind": "structural_strength",
                        "units": importance_units,
                    },
                    "effect_scope": {
                        "primitive_kind": "structural_transition",
                        "target_signatures": [f"square:{importance_target}"],
                        "plan_ids": [],
                        "plan_result_semantic_key": None,
                        "strategic_axes": [],
                    },
                },
                "descriptor_ambiguous": False,
                "proof_segment_ambiguous": False,
            }
        ],
        "only_move": None,
    }
    if rank_field == "priority_rank":
        value.pop("direct_effect_admission")
        value.pop("comparison_semantic_key")
        for channel in value["channels"]:
            channel.pop("proof_segment")
            channel.pop("effect_descriptor")
            channel.pop("importance_effect")
            channel.pop("descriptor_ambiguous")
            channel.pop("proof_segment_ambiguous")
    return value


def typed_case() -> dict[str, object]:
    return {
        "case_id": "case-1",
        "partition": "explore",
        "fen": "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "played_move_uci": "e2e3",
    }


def typed_channel(
    *, target: str = "e5", mechanism: str = "tempo", consequence: str = "initiative"
) -> dict[str, object]:
    return {
        "direct_change": "occurred",
        "played_change": "missed",
        "actor": {
            "move": "e2e4",
            "side": "white",
            "piece": "pawn",
            "from": "e2",
            "to": "e4",
        },
        "targets": {
            "required": [{"kind": "square", "key": target}],
            "allowed": [{"kind": "square", "key": target}],
        },
        "mechanisms": {
            "required": [{"kind": "mechanism", "key": mechanism}],
            "allowed": [{"kind": "mechanism", "key": mechanism}],
        },
        "consequences": {
            "required": [{"kind": "consequence", "key": consequence}],
            "allowed": [{"kind": "consequence", "key": consequence}],
        },
        "line": {"role": "best_reference", "root_move": "e2e4"},
        "horizon": "ply:1",
    }


def typed_meaning(
    meaning_id: str,
    cause_kind: str,
    *,
    generation: str = "required",
    exposure: str = "primary",
    dominated_by: list[str] | None = None,
    channels: list[dict[str, object]] | None = None,
) -> dict[str, object]:
    return {
        "meaning_id": meaning_id,
        "generation_policy": generation,
        "exposure_policy": exposure,
        "dominated_by": list(dominated_by or []),
        "realizations": [
            {
                "cause_kind": cause_kind,
                "comparison": {
                    "kind": "played_vs_best",
                    "mover": "white",
                    "reference_root_move": "e2e4",
                    "candidate_root_move": "e2e3",
                    "event_role": "best_reference",
                    "event_root_move": "e2e4",
                },
                "source_side": "reference",
                "attribution_kind": "reference_creates_resource",
                "effect_mode": "alternative_resource",
                "channels": copy.deepcopy(channels or [typed_channel()]),
            }
        ],
    }


def typed_label(
    meanings: list[dict[str, object]] | None = None,
    *,
    disposition: str = "answerable",
    top: list[str] | None = None,
    tie_groups: list[list[str]] | None = None,
    precedes: list[dict[str, str]] | None = None,
) -> dict[str, object]:
    values = meanings or [typed_meaning("main", "wrong_move_order")]
    return {
        "schema_version": TYPED_ORACLE_SCHEMA_VERSION,
        "case_id": "case-1",
        "partition": "explore",
        "disposition": disposition,
        "meanings": values,
        "priority": {
            "tie_groups": tie_groups or [],
            "precedes": precedes or [],
            "required_top_one_of": (
                list(top)
                if top is not None
                else (["main"] if disposition == "answerable" else [])
            ),
        },
    }


def typed_cause(
    cause_id: str = "cause-typed",
    cause_kind: str = "wrong_move_order",
    *,
    comparison_exposure_rank: int = 0,
    selection_order: int = 0,
    importance_units: int = 10,
    importance_target: str = "e5",
    exposure: str = "primary",
    jp: bool = True,
    ja: bool = True,
    native: bool = True,
    packet: bool | None = None,
    public: bool | None = None,
) -> dict[str, object]:
    exact = selection(
        cause_id,
        cause_kind,
        rank_field="comparison_exposure_rank",
        selection_order=selection_order,
        importance_units=importance_units,
        importance_target=importance_target,
    )
    exact["comparison_exposure_rank"] = comparison_exposure_rank
    exact["exposure"] = exposure
    claim_id = f"claim-{cause_id}"
    packet = native if packet is None else packet
    public = packet if public is None else public
    oracle_channel = typed_channel()
    actor = oracle_channel["actor"]
    assert isinstance(actor, dict)
    c_actor = [
        {"kind": "move", "key": actor["move"]},
        {"kind": "side", "key": actor["side"]},
        {"kind": "piece", "key": actor["piece"]},
        {"kind": "square", "key": actor["from"]},
        {"kind": "square", "key": actor["to"]},
    ]
    owned_channel = {
        "signature": "binding-owned-channel",
        "causal_signature": "owned-channel",
        "primitive_signature": "primitive-owned-channel",
        "source_id": f"proof-{cause_id}",
        "provenance_source_ids": [f"primitive-{cause_id}"],
        "carrier": {
            "id": f"proof-{cause_id}",
            "producer": "runtime",
            "layer": "causal",
            "scope": "direct",
            "confidence": "high",
            "line": {"id": "reference-line", "role": "best_reference", "rank": 1, "root_move": "e2e4"},
            "record_registered": True,
            "record_payload_type": "TacticalMechanismEvidence",
            "record_sha256": "0" * 64,
        },
        "carrier_ancestor_source_ids": [f"primitive-{cause_id}"],
        "provenance": [{
            "id": f"primitive-{cause_id}",
            "producer": "runtime",
            "layer": "primitive",
            "scope": "context",
            "confidence": "high",
            "line": {"id": "reference-line", "role": "best_reference", "rank": 1, "root_move": "e2e4"},
            "record_registered": True,
            "record_payload_type": "StructuralDeltaEvidence",
            "record_sha256": "1" * 64,
        }],
        "primitive_proof_source": {
            "id": f"primitive-{cause_id}",
            "producer": "runtime",
            "layer": "primitive",
            "scope": "context",
            "confidence": "high",
            "line": {"id": "reference-line", "role": "best_reference", "rank": 1, "root_move": "e2e4"},
            "record_registered": True,
            "record_payload_type": "StructuralDeltaEvidence",
            "record_sha256": "1" * 64,
        },
        "proof_role": "direct_proof",
        "line_id": "reference-line",
        "proof_segment": {
            "terminal_relation": "makes_structural_transition",
            "steps": [{"role": "root_action", "ply_offset": 0, "move_uci": "e2e4"}],
        },
        "effect_descriptor": copy.deepcopy(exact["channels"][0]["effect_descriptor"]),
        "importance_effect": copy.deepcopy(exact["channels"][0]["importance_effect"]),
        "importance_descriptor_ambiguous": False,
        "proof_segment_ambiguous": False,
        "specific_target_mechanism_ready": True,
        "direct_change": "occurred",
        "actor": c_actor,
        "target": [{"kind": "square", "key": importance_target}],
        "mechanism": [{"kind": "mechanism", "key": "tempo"}],
        "consequence": [{"kind": "consequence", "key": "initiative"}],
        "witness": copy.deepcopy(exact["channels"][0]["witnesses"]),
        "line": {"line_id": "reference-line", "role": "best_reference", "rank": 1, "root_move": "e2e4"},
        "horizon": "ply:1",
    }
    owned_channel["evidence_identity_sha256"] = native_sha256_json(
        {
            "carrier": owned_channel["carrier"],
            "provenance": owned_channel["provenance"],
            "primitive_proof_source": owned_channel["primitive_proof_source"],
        }
    )
    item = {
        "cause_record": {"id": cause_id},
        "c": {
            "kind": cause_kind,
            "source_side": "reference",
            "comparison_evidence": {
                "id": "comparison-1",
                "producer": "runtime",
                "layer": "comparison",
                "scope": "comparison",
                "confidence": "high",
                "line": None,
                "record_registered": True,
                "record_payload_type": "CandidateComparisonEvidence",
                "record_sha256": "2" * 64,
            },
            "comparison": {
                "kind": "played_vs_best",
                "reference_line": {"id": "reference-line", "role": "best_reference", "rank": 1, "root_move": "e2e4"},
                "candidate_line": {"id": "candidate-line", "role": "played", "rank": 1, "root_move": "e2e3"},
                "reference_root_move": "e2e4",
                "candidate_root_move": "e2e3",
                "mover": "white",
            },
            "attribution": {
                "kind": "reference_creates_resource",
                "root_move_matched": True,
                "direct_proof_eligible": True,
                "reason": None,
            },
            "direct_effect_admission": {
                "status": "restricted",
                "causal_signatures": ["owned-channel"],
            },
            "binding": {
                "role": "primary_played_cause",
                "source_side": "reference",
                "binding_tier": "primary",
                "event_line": {
                    "id": "reference-line",
                    "role": "best_reference",
                    "rank": 1,
                    "root_move": "e2e4",
                },
                "evidence_lines": [{"id": "reference-line", "role": "best_reference", "rank": 1, "root_move": "e2e4"}],
            },
            "proof": {
                "present": True,
                "direct": {
                    "role": "direct_proof",
                    "strength": "primary",
                    "proof_kind_labels": ["direct"],
                    "sources": [{
                        "id": f"proof-{cause_id}",
                        "producer": "runtime",
                        "layer": "causal",
                        "scope": "direct",
                        "confidence": "high",
                        "line": {
                            "id": "reference-line",
                            "role": "best_reference",
                            "rank": 1,
                            "root_move": "e2e4",
                        },
                        "record_registered": True,
                        "record_payload_type": "TacticalMechanismEvidence",
                        "record_sha256": "0" * 64,
                        "proof_kind_labels": ["direct"],
                    }],
                },
                "contrast": {
                    "role": "contrast_proof",
                    "strength": "supporting",
                    "proof_kind_labels": [],
                    "sources": [],
                },
                "context": {
                    "role": "context_support",
                    "strength": "weak_hint",
                    "proof_kind_labels": [],
                    "sources": [{"id": f"context-{cause_id}"}],
                },
                "strategic_identity": {
                    "axis_keys": [],
                    "mechanism_kinds": [],
                    "mechanism_source_ids": [],
                    "signal_source_ids": [],
                },
            },
            "objects": {
                "raw_owned_bindings": [copy.deepcopy(owned_channel)],
                "pre_admission_owned_bindings": [copy.deepcopy(owned_channel)],
                "owned_bindings": [copy.deepcopy(owned_channel)],
            },
        },
        "jp": {
            "linked_claims": [
                {
                    "id": claim_id,
                    "evidence_ids": [cause_id],
                    "link_kind": "direct_evidence",
                }
            ]
            if jp
            else []
        },
        "ja": {
            "linked_decisions": [
                {"claim_id": claim_id, "status": "certified"}
            ]
            if ja
            else []
        },
        "r": {
            "fallback_dominance": {
                "status": "retained",
                "retained": True,
                "dominating_cause_evidence_ids": [],
            },
            "cross_comparison_exposure": {
                "status": (
                    "selected_complementary"
                    if native and exposure == "complementary"
                    else "selected_primary"
                    if native
                    else "diagnostic_comparison"
                ),
                "selected": native,
                "reason": (
                    "played_vs_best_alternative_resource"
                    if native
                    else "played_vs_best_outcome_is_diagnostic"
                ),
                "representative_cause_evidence_id": cause_id,
                "effect_mode": "alternative_resource",
                "comparison_exposure_rank": comparison_exposure_rank if native else None,
            },
            "native_selection": copy.deepcopy(exact) if native else None,
        },
        "p": {
            "packet_present": packet,
            "cause_evidence_id": cause_id,
            "packet_selected": packet,
            "packet_selection": copy.deepcopy(exact) if packet else None,
            "public_selection_count": 1 if public else 0,
            "public_selection": copy.deepcopy(exact) if public else None,
            "selected_public_cause_evidence_ids": [cause_id] if public else [],
        },
    }
    refresh_selection_lineage_from_c(item)
    return item


def material_outcome_cause(
    cause_id: str,
    cause_kind: str,
    *,
    durable_net_cp: int,
    target_value_cp: int,
    onset_ply_offset: int = 0,
) -> dict[str, object]:
    item = typed_cause(cause_id, cause_kind)
    measure = {
        "kind": "material_outcome",
        "durable_net_cp": durable_net_cp,
        "onset_ply_offset": onset_ply_offset,
    }
    salience = {
        "move_uci": "e2e4",
        "ply_offset": onset_ply_offset,
        "captured_role": "queen" if target_value_cp == 900 else "rook",
        "square": "e4",
        "target_value_cp": target_value_cp,
    }
    for selection_value in (
        item["r"]["native_selection"],
        item["p"]["packet_selection"],
        item["p"]["public_selection"],
    ):
        assert isinstance(selection_value, dict)
        channel = selection_value["channels"][0]
        descriptor = channel["effect_descriptor"]
        measured_effect = channel["importance_effect"]
        descriptor["measure"] = copy.deepcopy(measure)
        descriptor["material_event_salience"] = copy.deepcopy(salience)
        measured_effect["domain_kind"] = "material"
        measured_effect["domain"] = "material"
        measured_effect["measure"] = copy.deepcopy(measure)
    return item


def fixture_effect_scope_key(scope: dict[str, object]) -> str:
    assert scope["strategic_axes"] == []
    semantic_key = scope["plan_result_semantic_key"]
    plans = [] if semantic_key is not None else scope["plan_ids"]
    return (
        f"{str(scope['primitive_kind']).replace('_', '')}|"
        f"[{','.join(str(item) for item in scope['target_signatures'])}]|"
        f"[{','.join(str(item) for item in plans)}]|[]|"
        f"{semantic_key if semantic_key is not None else 'none'}"
    )


def typed_importance(
    native_values: list[dict[str, object]],
    *,
    unique_top: str | None | object = Ellipsis,
    unmeasured_cause_ids: set[str] | None = None,
) -> dict[str, object]:
    unmeasured = set(unmeasured_cause_ids or set())
    profiles: list[dict[str, object]] = []
    selected_channels: dict[str, list[str]] = {}
    for item in native_values:
        cause_id = str(item["cause_evidence_id"])
        channels = item["channels"]
        assert isinstance(channels, list)
        selected_channels[cause_id] = [
            str(channel["causal_signature"])
            for channel in channels
            if isinstance(channel, dict)
        ]
        if cause_id not in unmeasured:
            for channel in channels:
                assert isinstance(channel, dict)
                descriptor = channel["effect_descriptor"]
                assert isinstance(descriptor, dict)
                measured_effect = channel["importance_effect"]
                assert isinstance(measured_effect, dict)
                profiles.append(
                    {
                        "cause_evidence_id": cause_id,
                        "causal_signature": str(channel["causal_signature"]),
                        "frame": {
                            "comparison": str(item["comparison_semantic_key"]),
                            "event_line": "BestReference:1:e2e4",
                            "source_side": "reference",
                            "attribution": "reference_creates_resource",
                            "exposure": str(item["exposure"]),
                            "effect_mode": str(item["effect_mode"]),
                            "actor": "white",
                            "direct_change": str(channel["direct_change"]),
                            "played_change": str(channel["played_change"]),
                            "stake": str(measured_effect["stake"]),
                        },
                        "universe": {
                            "root_board_state": (
                                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/"
                                "RNBQKBNR w KQkq -"
                            ),
                            "impact": "harms-reviewed-mover:white",
                        },
                        "domain_kind": str(measured_effect["domain_kind"]),
                        "domain": str(measured_effect["domain"]),
                        "measure": copy.deepcopy(measured_effect["measure"]),
                        "effect_scope": copy.deepcopy(measured_effect["effect_scope"]),
                    }
                )
    profiles.sort(
        key=lambda item: (
            str(item["cause_evidence_id"]),
            str(item["causal_signature"]),
        )
    )

    relations: list[dict[str, object]] = []
    for index, left in enumerate(profiles):
        for right in profiles[index + 1 :]:
            if left["cause_evidence_id"] == right["cause_evidence_id"]:
                continue
            same_universe = left["universe"] == right["universe"]
            same_domain = left["domain"] == right["domain"]
            left_measure = left["measure"]
            right_measure = right["measure"]
            assert isinstance(left_measure, dict) and isinstance(right_measure, dict)
            left_kind = left_measure["kind"]
            right_kind = right_measure["kind"]
            left_scope = left["effect_scope"]
            right_scope = right["effect_scope"]
            assert isinstance(left_scope, dict) and isinstance(right_scope, dict)
            left_scope_key = fixture_effect_scope_key(left_scope)
            structural_scope_ready = (
                left["domain_kind"] != "structural"
                or (
                    left_scope == right_scope
                    and left_scope
                    != {
                        "primitive_kind": "unspecified",
                        "target_signatures": [],
                        "plan_ids": [],
                        "strategic_axes": [],
                        "plan_result_semantic_key": None,
                    }
                )
            )
            if (
                same_universe
                and same_domain
                and structural_scope_ready
                and left_kind == right_kind
            ):
                if left_kind == "material_outcome":
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
                        relation = "incomparable"
                else:
                    left_units = int(left_measure["units"])
                    right_units = int(right_measure["units"])
                    relation = (
                        "dominates"
                        if left_units > right_units
                        else "dominated_by"
                        if left_units < right_units
                        else "tied"
                    )
                domain: str | None = (
                    f"{left['domain']}|effect:{left_scope_key}"
                    if left["domain_kind"] == "structural"
                    else str(left["domain"])
                )
                if relation == "incomparable":
                    domain = None
            else:
                relation = "incomparable"
                domain = None
            relations.append(
                {
                    "left_cause_id": left["cause_evidence_id"],
                    "left_causal_signature": left["causal_signature"],
                    "right_cause_id": right["cause_evidence_id"],
                    "right_causal_signature": right["causal_signature"],
                    "relation": relation,
                    "domain": domain,
                }
            )
    decisions: list[dict[str, object]] = []
    for item in native_values:
        cause_id = str(item["cause_evidence_id"])
        signatures = selected_channels[cause_id]
        measured = [
            signature
            for signature in signatures
            if any(
                profile["cause_evidence_id"] == cause_id
                and profile["causal_signature"] == signature
                for profile in profiles
            )
        ]
        unmeasured_signatures = [
            signature for signature in signatures if signature not in measured
        ]
        dominators_by_signature: dict[str, set[str]] = {
            signature: set() for signature in measured
        }
        for relation_item in relations:
            if (
                relation_item["right_cause_id"] == cause_id
                and relation_item["relation"] == "dominates"
            ):
                dominators_by_signature[
                    str(relation_item["right_causal_signature"])
                ].add(str(relation_item["left_cause_id"]))
            if (
                relation_item["left_cause_id"] == cause_id
                and relation_item["relation"] == "dominated_by"
            ):
                dominators_by_signature[
                    str(relation_item["left_causal_signature"])
                ].add(str(relation_item["right_cause_id"]))
        common_dominators = (
            set(dominators_by_signature[measured[0]]) if measured else set()
        )
        for signature in measured[1:]:
            common_dominators.intersection_update(
                dominators_by_signature[signature]
            )
        dominators = sorted(common_dominators)
        fully_measured = bool(measured and not unmeasured_signatures)
        decisions.append(
            {
                "cause_evidence_id": cause_id,
                "measured_channel_signatures": measured,
                "unmeasured_channel_signatures": unmeasured_signatures,
                "dominating_cause_ids": dominators,
                "dominated_within_domain": bool(
                    fully_measured and common_dominators
                ),
                "fully_measured": fully_measured,
            }
        )
    dominated_set = {
        str(item["cause_evidence_id"])
        for item in decisions
        if item["dominated_within_domain"] is True
    }
    remaining = set(dominated_set)
    dominators_by_cause = {
        str(item["cause_evidence_id"]): set(
            str(value) for value in item["dominating_cause_ids"]
        )
        for item in decisions
        if item["dominated_within_domain"] is True
    }
    layer_by_id = {
        str(item["cause_evidence_id"]): 0
        for item in decisions
        if item["dominated_within_domain"] is not True
    }
    layer = 1
    while remaining:
        next_layer = {
            cause_id
            for cause_id in remaining
            if not (dominators_by_cause[cause_id] & remaining)
        }
        assert next_layer
        layer_by_id.update({cause_id: layer for cause_id in next_layer})
        remaining -= next_layer
        layer += 1
    ordered = sorted(
        native_values,
        key=lambda item: (
            layer_by_id[str(item["cause_evidence_id"])],
            int(item["comparison_exposure_rank"]),
            str(item["cause_evidence_id"]),
        ),
    )
    for selection_order, item in enumerate(ordered):
        item["selection_order"] = selection_order
    selected_ids = [str(item["cause_evidence_id"]) for item in ordered]
    dominated_ids = [item for item in selected_ids if item in dominated_set]
    frontier = [item for item in selected_ids if item not in dominated_set]
    resolved_top = (
        (frontier[0] if len(frontier) == 1 and not unmeasured else None)
        if unique_top is Ellipsis
        else unique_top
    )
    native = {
        "relation_policy_version": "chesstory.direct-cause-importance.relation.v3",
        "ordering_policy_version": (
            "chesstory.player-facing-idea-ordering.dominance-layers.v1"
        ),
        "selected_cause_ids": list(selected_ids),
        "frontier_cause_ids": list(frontier),
        "dominated_within_domain_cause_ids": list(dominated_ids),
        "unique_top": resolved_top,
        "profiles": profiles,
        "relations": relations,
        "decisions": decisions,
    }
    public_profiles = [
        {
            "cause_evidence_id": item["cause_evidence_id"],
            "causal_signature": item["causal_signature"],
            "universe": copy.deepcopy(item["universe"]),
            "domain_kind": item["domain_kind"],
            "domain": item["domain"],
            "stake": item["frame"]["stake"],
            "event_line": item["frame"]["event_line"],
            "effect_mode": item["frame"]["effect_mode"],
            "direct_change": item["frame"]["direct_change"],
            "played_change": item["frame"]["played_change"],
            "measure": copy.deepcopy(item["measure"]),
            "effect_scope": copy.deepcopy(item["effect_scope"]),
        }
        for item in profiles
    ]
    public_projection = {
        "authority": "root_owned_effect_partial_order",
        "relation_policy_version": "chesstory.direct-cause-importance.relation.v3",
        "ordering_policy_version": (
            "chesstory.player-facing-idea-ordering.dominance-layers.v1"
        ),
        "comparison_exposure_rank_meaning": "comparison_exposure_authority",
        "selected_cause_ids": list(selected_ids),
        "frontier_cause_ids": list(frontier),
        "dominated_within_domain_cause_ids": list(dominated_ids),
        "unique_top": resolved_top,
        "profile_summary": {
            "measured_channels": len(profiles),
            "measured_causes": len({item["cause_evidence_id"] for item in profiles}),
            "fully_measured_causes": sum(
                bool(item["fully_measured"]) for item in decisions
            ),
            "unmeasured_causes": sum(
                bool(item["unmeasured_channel_signatures"]) for item in decisions
            ),
        },
        "profiles": public_profiles,
        "relations": copy.deepcopy(relations),
        "decisions": copy.deepcopy(decisions),
        "relation_summary": {
            "comparable_profile_pairs": sum(
                item["relation"] != "incomparable" for item in relations
            ),
            "incomparable_profile_pairs": sum(
                item["relation"] == "incomparable" for item in relations
            ),
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
    return {
        "r_native": native,
        "packet_native": copy.deepcopy(native),
        "r_public_projection": public_projection,
        "packet_public_projection": copy.deepcopy(public_projection),
        "public_projection": copy.deepcopy(public_projection),
    }


def typed_verdict() -> dict[str, object]:
    return {
        "comparison_kind": "played_vs_best",
        "mover": "white",
        "verdict_code": "mistake",
        "move_quality": "bad",
        "played_move": "e2e3",
        "reference_move": "e2e4",
        "candidate_win_percent_delta_for_mover": -10.0,
        "win_percent_loss_for_mover": 10.0,
        "outcome": None,
        "mate": None,
    }


def typed_verdict_authority(
    verdict: dict[str, object] | None = None,
) -> dict[str, object]:
    value = verdict or typed_verdict()
    mate = value.get("mate")
    mate_value = mate if isinstance(mate, dict) else {}
    return {
        "policy_version": "chesstory.verdict-threshold-policy.v1",
        "comparison_kind": value["comparison_kind"],
        "mover": value["mover"],
        "played_move": value["played_move"],
        "reference_move": value["reference_move"],
        "candidate_set_type": None,
        "candidate_win_percent_delta_for_mover": value[
            "candidate_win_percent_delta_for_mover"
        ],
        "win_percent_loss_for_mover": value["win_percent_loss_for_mover"],
        "expected_verdict_code": value["verdict_code"],
        "expected_move_quality": value["move_quality"],
        "reference_mate_for_mover": mate_value.get("reference_for_mover"),
        "played_mate_for_mover": mate_value.get("played_for_mover"),
        "mate_distance_loss_for_mover": mate_value.get("distance_loss"),
    }


def typed_verdict_observation(primary_engine_backed: bool) -> dict[str, object]:
    value = typed_verdict() if primary_engine_backed else None
    return {
        "packet_canonical": copy.deepcopy(value),
        "selected_projection": copy.deepcopy(value),
        "final_public_response": copy.deepcopy(value),
        "classification_authority": (
            typed_verdict_authority(value) if value is not None else None
        ),
    }


def typed_idea_unit_observations(
    native_values: list[dict[str, object]],
    packet_values: list[dict[str, object]],
    public_values: list[dict[str, object]],
    cause_importance: dict[str, object] | None = None,
) -> tuple[dict[str, object], dict[str, object]]:
    idea_importance = copy.deepcopy(
        cause_importance
        if cause_importance is not None
        else typed_importance(copy.deepcopy(native_values))
    )
    native_importance = idea_importance["r_native"]
    assert isinstance(native_importance, dict)
    selected_ids = native_importance["selected_cause_ids"]
    assert isinstance(selected_ids, list)
    decisions = native_importance["decisions"]
    assert isinstance(decisions, list)
    decision_by_id = {
        str(item["cause_evidence_id"]): item
        for item in decisions
        if isinstance(item, dict)
    }
    unique_top = native_importance["unique_top"]
    frontier = set(str(item) for item in native_importance["frontier_cause_ids"])
    layers = _dominance_layers(
        [str(item) for item in selected_ids],
        [item for item in decisions if isinstance(item, dict)],
    )
    units: list[dict[str, object]] = []
    for order, raw_cause_id in enumerate(selected_ids):
        cause_id = str(raw_cause_id)
        decision = decision_by_id[cause_id]
        if decision["fully_measured"] is not True:
            priority_status = "unmeasured"
        elif unique_top == cause_id:
            priority_status = "unique_top"
        elif cause_id in frontier:
            priority_status = "frontier"
        else:
            priority_status = "dominated"
        units.append(
            {
                "idea_id": cause_id,
                "kind": "single_cause",
                "lead_cause_evidence_id": cause_id,
                "member_cause_evidence_ids": [cause_id],
                "importance_layer": layers[cause_id],
                "priority_status": priority_status,
                "serialization_order": order,
            }
        )

    native_ids = {str(item["cause_evidence_id"]) for item in native_values}
    packet_ids = {str(item["cause_evidence_id"]) for item in packet_values}
    public_ids = {str(item["cause_evidence_id"]) for item in public_values}
    packet_units = copy.deepcopy(units) if packet_ids == native_ids else None
    public_units = [
        copy.deepcopy(unit)
        for unit in units
        if str(unit["lead_cause_evidence_id"]) in public_ids
    ]
    public_links = [
        {
            "cause_evidence_id": str(cause_id),
            "item_role": "cause_facet",
            "idea_unit_id": str(unit["idea_id"]),
        }
        for unit in public_units
        for cause_id in unit["member_cause_evidence_ids"]
    ]
    if packet_units is None:
        idea_importance["packet_native"] = None
        idea_importance["packet_public_projection"] = None
    if not public_values:
        idea_importance["public_projection"] = None
    unit_observation = {
        "r_native": units,
        "packet_native": packet_units,
        "public_projection": public_units,
        "raw_public_idea_unit_count": len(public_units),
        "parsed_public_idea_unit_count": len(public_units),
        "public_idea_units_parse_closed": True,
        "public_item_links": public_links,
        "public_item_membership_closed": True,
    }
    return unit_observation, idea_importance


_DISPOSITION_STATUSES = (
    "selected",
    "dominated",
    "redundant",
    "diagnostic",
    "inferior",
    "admission_deferred",
    "rejected",
    "unproposed",
    "object_unready",
)
_DISPOSITION_REASONS = (
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
)


def typed_cause_disposition_observation(
    causes: list[dict[str, object]],
    native_values: list[dict[str, object]],
) -> tuple[dict[str, object], dict[str, object]]:
    selected_owner_by_id: dict[str, str] = {}
    for selection_value in native_values:
        cause_id = str(selection_value["cause_evidence_id"])
        host = selection_value["host"]
        assert isinstance(host, dict)
        selected_owner_by_id[cause_id] = str(host["claim_id"])
    dispositions: list[dict[str, object]] = []
    for cause_value in sorted(causes, key=lambda item: str(item["cause_record"]["id"])):
        cause_record = cause_value["cause_record"]
        assert isinstance(cause_record, dict)
        cause_id = str(cause_record["id"])
        owner = selected_owner_by_id.get(cause_id)
        claim_ids = [owner] if owner is not None else []
        dispositions.append(
            {
                "cause_evidence_id": cause_id,
                "status": "selected" if owner is not None else "unproposed",
                "reason": (
                    "player_facing_selection" if owner is not None else "no_claim_proposal"
                ),
                "proposed_claim_ids": claim_ids,
                "certified_claim_ids": claim_ids,
                "rank_eligible_claim_ids": claim_ids,
                "selected_owner_claim_id": owner,
                "related_cause_evidence_ids": [],
                "related_claim_ids": [],
            }
        )
    status_counts = {
        status: sum(item["status"] == status for item in dispositions)
        for status in _DISPOSITION_STATUSES
    }
    reason_counts = {
        reason: sum(item["reason"] == reason for item in dispositions)
        for reason in _DISPOSITION_REASONS
    }
    selected_ids = sorted(selected_owner_by_id)
    summary = {
        "authority": "cause_disposition_ledger.v1",
        "total_cause_count": len(dispositions),
        "selected_cause_ids": selected_ids,
        "status_counts": status_counts,
        "reason_counts": reason_counts,
        "abstention_codes": (
            []
            if selected_ids
            else ["no_relative_cause_generated"]
            if not dispositions
            else ["no_claim_proposal"]
        ),
    }
    observation = {
        "r_native": dispositions,
        "packet_native": copy.deepcopy(dispositions),
        "r_public_summary": copy.deepcopy(summary),
        "packet_public_summary": copy.deepcopy(summary),
        "selected_projection": copy.deepcopy(summary),
        "final_public_response": copy.deepcopy(summary),
    }
    return observation, summary


def typed_endpoint_snapshots(
    causes: list[dict[str, object]],
) -> list[dict[str, object]]:
    by_comparison: dict[str, dict[str, object]] = {}
    witnessed: dict[str, set[bytes]] = {}
    for cause in causes:
        c_value = cause["c"]
        assert isinstance(c_value, dict)
        comparison_evidence = c_value["comparison_evidence"]
        comparison = c_value["comparison"]
        assert isinstance(comparison_evidence, dict) and isinstance(comparison, dict)
        comparison_id = str(comparison_evidence["id"])
        snapshot = by_comparison.get(comparison_id)
        if snapshot is None:
            snapshot = {
                "comparison_evidence": copy.deepcopy(comparison_evidence),
                "comparison": copy.deepcopy(comparison),
                "reference": {
                    "source_side": "reference",
                    "line": {
                        "line_id": comparison["reference_line"]["id"],
                        "role": comparison["reference_line"]["role"],
                        "rank": comparison["reference_line"]["rank"],
                        "root_move": comparison["reference_line"]["root_move"],
                    },
                    "witnesses": [],
                },
                "candidate": {
                    "source_side": "candidate",
                    "line": {
                        "line_id": comparison["candidate_line"]["id"],
                        "role": comparison["candidate_line"]["role"],
                        "rank": comparison["candidate_line"]["rank"],
                        "root_move": comparison["candidate_line"]["root_move"],
                    },
                    "witnesses": [],
                },
            }
            by_comparison[comparison_id] = snapshot
            witnessed[comparison_id] = set()
        else:
            assert snapshot["comparison"] == comparison
            assert snapshot["comparison_evidence"] == comparison_evidence

        objects = c_value["objects"]
        assert isinstance(objects, dict)
        raw_channels = objects["raw_owned_bindings"]
        assert isinstance(raw_channels, list)
        source_side = str(c_value["source_side"])
        if source_side not in {"reference", "candidate"}:
            continue
        side = snapshot[source_side]
        assert isinstance(side, dict)
        witnesses = side["witnesses"]
        assert isinstance(witnesses, list)
        for channel in raw_channels:
            assert isinstance(channel, dict)
            if not all(
                isinstance(channel.get(name), value_type)
                for name, value_type in (
                    ("carrier", dict),
                    ("provenance", list),
                    ("carrier_ancestor_source_ids", list),
                    ("primitive_proof_source", dict),
                    ("line", dict),
                    ("actor", list),
                    ("target", list),
                    ("mechanism", list),
                    ("consequence", list),
                    ("witness", list),
                    ("proof_segment", dict),
                    ("effect_descriptor", dict),
                )
            ):
                continue
            witness = {
                "source_side": source_side,
                "line": copy.deepcopy(channel["line"]),
                "carrier": copy.deepcopy(channel["carrier"]),
                "provenance": copy.deepcopy(channel["provenance"]),
                "carrier_ancestor_source_ids": copy.deepcopy(
                    channel["carrier_ancestor_source_ids"]
                ),
                "primitive_proof_source": copy.deepcopy(
                    channel["primitive_proof_source"]
                ),
                "actor": copy.deepcopy(channel["actor"]),
                "target": copy.deepcopy(channel["target"]),
                "mechanism": copy.deepcopy(channel["mechanism"]),
                "consequence": copy.deepcopy(channel["consequence"]),
                "witness": copy.deepcopy(channel["witness"]),
                "horizon": channel.get("horizon"),
                "proof_segment": copy.deepcopy(channel["proof_segment"]),
                "effect_descriptor": copy.deepcopy(channel["effect_descriptor"]),
            }
            key = json.dumps(witness, sort_keys=True, separators=(",", ":")).encode()
            if key not in witnessed[comparison_id]:
                witnessed[comparison_id].add(key)
                witnesses.append(witness)
    return list(by_comparison.values())


def typed_snapshot_index(
    causes: list[dict[str, object]],
) -> dict[str, dict[str, object]]:
    return comparison_endpoint_snapshot_index(
        {"comparison_endpoint_evidence_snapshots": typed_endpoint_snapshots(causes)}
    )


def typed_actual(
    causes: list[dict[str, object]],
    *,
    importance: dict[str, object] | None = None,
    primary_engine_backed: bool = True,
    endpoint_snapshots: list[dict[str, object]] | None = None,
) -> dict[str, object]:
    initial_native: list[dict[str, object]] = []
    for item in causes:
        r_value = item["r"]
        assert isinstance(r_value, dict)
        if isinstance(r_value["native_selection"], dict):
            initial_native.append(copy.deepcopy(r_value["native_selection"]))
    importance_value = copy.deepcopy(importance or typed_importance(initial_native))
    native_importance = importance_value["r_native"]
    assert isinstance(native_importance, dict)
    ordered_ids = native_importance["selected_cause_ids"]
    assert isinstance(ordered_ids, list)
    order_by_id = {str(cause_id): order for order, cause_id in enumerate(ordered_ids)}
    for item in causes:
        r_value = item["r"]
        p_value = item["p"]
        assert isinstance(r_value, dict) and isinstance(p_value, dict)
        if not primary_engine_backed:
            p_value["public_selection"] = None
            p_value["public_selection_count"] = 0
            p_value["selected_public_cause_evidence_ids"] = []
        for selection_value in (
            r_value.get("native_selection"),
            p_value.get("packet_selection"),
            p_value.get("public_selection"),
        ):
            if isinstance(selection_value, dict):
                cause_id = str(selection_value["cause_evidence_id"])
                if cause_id in order_by_id:
                    selection_value["selection_order"] = order_by_id[cause_id]
    native_values: list[dict[str, object]] = []
    packet_values: list[dict[str, object]] = []
    public_values: list[dict[str, object]] = []
    for item in causes:
        r_value = item["r"]
        p_value = item["p"]
        assert isinstance(r_value, dict) and isinstance(p_value, dict)
        if isinstance(r_value["native_selection"], dict):
            native_values.append(copy.deepcopy(r_value["native_selection"]))
        if isinstance(p_value["packet_selection"], dict):
            packet_values.append(copy.deepcopy(p_value["packet_selection"]))
        if isinstance(p_value["public_selection"], dict):
            public_values.append(copy.deepcopy(p_value["public_selection"]))
    native_values.sort(key=lambda item: int(item["selection_order"]))
    packet_values.sort(key=lambda item: int(item["selection_order"]))
    public_values.sort(key=lambda item: int(item["selection_order"]))
    idea_units_value, idea_importance_value = typed_idea_unit_observations(
        native_values,
        packet_values,
        public_values,
        importance_value,
    )
    disposition_value, disposition_summary = typed_cause_disposition_observation(
        causes, native_values
    )
    if not packet_values:
        importance_value["packet_native"] = None
        importance_value["packet_public_projection"] = None
    if not public_values:
        importance_value["public_projection"] = None
    return {
        "status": "complete",
        "public_response": {
            "primary_engine_backed": primary_engine_backed,
            "status": "ready" if primary_engine_backed else "withheld",
            "idea_status": (
                "certified"
                if public_values
                else "no_certified_differential_idea"
                if primary_engine_backed
                else "unavailable"
            ),
            "idea_status_detail": disposition_summary,
        },
        "verdict": typed_verdict_observation(primary_engine_backed),
        "probe_closure": {"all_closed": True},
        "comparison_endpoint_evidence_snapshots": copy.deepcopy(
            endpoint_snapshots
            if endpoint_snapshots is not None
            else typed_endpoint_snapshots(causes)
        ),
        "causes": causes,
        "r_native_cause_selections": native_values,
        "importance": importance_value,
        "idea_units": idea_units_value,
        "idea_importance": idea_importance_value,
        "cause_disposition_ledger": disposition_value,
        "projection": {
            "present": True,
            "status": "ready" if primary_engine_backed else "withheld",
            "reason": None if primary_engine_backed else "insufficient_engine_depth",
            "renderable": primary_engine_backed,
            "selected_payload_source": "full" if primary_engine_backed else "empty",
            "selected_payload_sha256": "0" * 64,
            "raw_public_idea_count": len(public_values),
            "parsed_public_idea_count": len(public_values),
            "public_ideas_parse_closed": True,
            "selected_public_cause_evidence_ids": [
                str(item["cause_evidence_id"]) for item in public_values
            ],
            "selected_public_cause_selections": public_values,
        },
    }


def pair_typed_idea_unit(
    actual: dict[str, object], lead_id: str, member_id: str
) -> None:
    unit = {
        "idea_id": lead_id,
        "kind": "exact_pvb_responsibility",
        "lead_cause_evidence_id": lead_id,
        "member_cause_evidence_ids": [lead_id, member_id],
        "importance_layer": 0,
        "priority_status": "unique_top",
        "serialization_order": 0,
    }
    unit_observation = actual["idea_units"]
    assert isinstance(unit_observation, dict)
    unit_observation.update(
        {
            "r_native": [copy.deepcopy(unit)],
            "packet_native": [copy.deepcopy(unit)],
            "public_projection": [copy.deepcopy(unit)],
            "raw_public_idea_unit_count": 1,
            "parsed_public_idea_unit_count": 1,
            "public_idea_units_parse_closed": True,
            "public_item_links": [
                {
                    "cause_evidence_id": lead_id,
                    "item_role": "cause_facet",
                    "idea_unit_id": lead_id,
                },
                {
                    "cause_evidence_id": member_id,
                    "item_role": "cause_facet",
                    "idea_unit_id": lead_id,
                },
            ],
            "public_item_membership_closed": True,
        }
    )
    selections = actual["r_native_cause_selections"]
    assert isinstance(selections, list)
    lead_selections = [
        copy.deepcopy(item)
        for item in selections
        if isinstance(item, dict) and item["cause_evidence_id"] == lead_id
    ]
    assert len(lead_selections) == 1
    actual["idea_importance"] = typed_importance(lead_selections)


def open_world_candidate(
    cause_value: dict[str, object], case_id: str = "case-1"
) -> dict[str, object]:
    payload = canonical_open_world_cause_candidate(
        cause_value, typed_snapshot_index([cause_value])
    )
    c_semantics_sha256 = sha256_json(payload["c_semantics"])
    return {
        "candidate_id": "ow:"
        + sha256_json(
            {
                "case_id": case_id,
                "c_semantics_sha256": c_semantics_sha256,
            }
        ),
        "case_id": case_id,
        "c_semantics_sha256": c_semantics_sha256,
        "payload_sha256": sha256_json(payload),
        "payload": payload,
    }


def write_open_world_contract(
    directory: str,
    *,
    decision: str = "accepted",
    include_candidate: bool = True,
    include_decision: bool = True,
    native: bool = True,
    owned_channels: bool = True,
    adjudicated_exposure: str | None = None,
    public_top: bool = True,
) -> dict[str, object]:
    root = Path(directory)
    paths = {
        name: root / name
        for name in (
            "freeze.json",
            "cases.jsonl",
            "base.jsonl",
            "run.jsonl",
            "candidates.json",
            "arm-inventory.json",
            "adjudication.json",
            "manifest.json",
        )
    }
    paths["freeze.json"].write_text("freeze\n", encoding="ascii")
    paths["cases.jsonl"].write_text("cases\n", encoding="ascii")
    case = typed_case()
    cause_value = typed_cause(
        "cause-open-world", "tempo_loss", native=native, packet=native, public=native
    )
    if not owned_channels:
        objects = cause_value["c"]["objects"]
        objects["raw_owned_bindings"] = copy.deepcopy(objects["owned_bindings"])
        objects["owned_bindings"] = []
        cause_value["c"]["direct_effect_admission"] = {
            "status": "restricted",
            "causal_signatures": [],
        }
    candidate = open_world_candidate(cause_value)
    base = typed_label()
    exposure = adjudicated_exposure or (
        "primary" if native else "diagnostic_only"
    )
    public_capable = exposure in {"primary", "complementary", "fallback_only"}
    generation = "required" if public_capable and public_top else "allowed"
    verified = typed_meaning(
        "open-world-tempo",
        "tempo_loss",
        generation=generation,
        exposure=exposure,
    )
    run = copy.deepcopy(base)
    if decision == "accepted" and include_candidate and include_decision:
        run["meanings"].append(copy.deepcopy(verified))
        if public_capable:
            run["disposition"] = "answerable"
            run["priority"] = (
                {
                    "tie_groups": [],
                    "precedes": [
                        {"higher": "open-world-tempo", "lower": "main"}
                    ],
                    "required_top_one_of": ["open-world-tempo"],
                }
                if public_top
                else {
                    "tie_groups": [],
                    "precedes": [
                        {"higher": "main", "lower": "open-world-tempo"}
                    ],
                    "required_top_one_of": ["main"],
                }
            )
    paths["base.jsonl"].write_text(json.dumps(base) + "\n", encoding="ascii")
    paths["run.jsonl"].write_text(json.dumps(run) + "\n", encoding="ascii")
    base_labels, _ = _load_typed_labels(
        root=ROOT,
        path=paths["base.jsonl"],
        partition_cases=[case],
        partition="explore",
    )
    run_labels, _ = _load_typed_labels(
        root=ROOT,
        path=paths["run.jsonl"],
        partition_cases=[case],
        partition="explore",
    )
    candidates = [candidate] if include_candidate else []
    observation_payload = canonical_open_world_cause_observation(
        cause_value, typed_snapshot_index([cause_value])
    )
    observation = {
        "c_semantics_sha256": sha256_json(observation_payload["c_semantics"]),
        "payload_sha256": sha256_json(observation_payload),
        "payload": observation_payload,
    }
    observations = [observation] if include_candidate else []
    cascades = [
        {
            "case_id": "case-1",
            "actual_cascade_sha256": "3" * 64,
            "observation_count": len(observations),
            "unmatched_cause_observations": observations,
        }
    ]
    producer = {
        "runtime_run_schema_version": "chesstory.eval.cause-audit-runtime-run.v1",
        "runtime_observation_schema": "chesstory.cause-audit-runtime-observation.v3",
        "adapter_main_class": "io.chesstory.evaluation.runtimeadapter.CauseAuditAdapterCli",
        "adapter_launcher_sha256": "4" * 64,
        "engine_sha256": "5" * 64,
        "candidate_binding_sha256": None,
    }
    arm_inventory = {
        "schema_version": TYPED_OPEN_WORLD_ARM_INVENTORY_SCHEMA_VERSION,
        "partition": "explore",
        "freeze_manifest_sha256": sha256_file(paths["freeze.json"]),
        "cases_file_sha256": sha256_file(paths["cases.jsonl"]),
        "base_oracle_file_sha256": sha256_file(paths["base.jsonl"]),
        "case_ids_sha256": sha256_json(["case-1"]),
        "arms": [
            {
                "arm_id": "runtime-arm-1",
                "runtime_run_file_sha256": "6" * 64,
                "producer_contract_sha256": sha256_json(producer),
                "producer_contract": producer,
                "case_ids_sha256": sha256_json(["case-1"]),
                "actual_cascades_sha256": sha256_json(cascades),
                "actual_cascades": cascades,
            }
        ],
    }
    paths["arm-inventory.json"].write_text(
        json.dumps(arm_inventory), encoding="ascii"
    )
    candidate_set = _open_world_candidate_set_document(
        root=ROOT,
        partition="explore",
        base_oracle_path=paths["base.jsonl"],
        base_labels=base_labels,
        arm_inventory_path=paths["arm-inventory.json"],
        arm_inventory=arm_inventory,
    )
    assert candidate_set["candidates"] == candidates
    paths["candidates.json"].write_text(
        json.dumps(candidate_set), encoding="ascii"
    )
    decisions: list[dict[str, object]] = []
    if include_candidate and include_decision:
        accepted = decision == "accepted"
        decisions.append(
            {
                "candidate_id": candidate["candidate_id"],
                "candidate_sha256": sha256_json(candidate),
                "decision": decision,
                "verified_meaning": copy.deepcopy(verified) if accepted else None,
                "verified_meaning_sha256": (
                    sha256_json(verified) if accepted else None
                ),
            }
        )
    case_judgments: list[dict[str, object]] = []
    if (
        decision == "accepted"
        and include_candidate
        and include_decision
        and public_capable
    ):
        policy = {
            "disposition": "answerable",
            "meaning_ids": ["main", "open-world-tempo"],
            "meaning_ids_sha256": sha256_json(
                ["main", "open-world-tempo"]
            ),
            "priority": copy.deepcopy(run["priority"]),
        }
        case_judgments.append(
            {
                "case_id": "case-1",
                "accepted_public_candidate_ids": [candidate["candidate_id"]],
                "policy_sha256": sha256_json(policy),
                "policy": policy,
            }
        )
    adjudication = {
        "schema_version": TYPED_OPEN_WORLD_ADJUDICATION_SCHEMA_VERSION,
        "partition": "explore",
        "arm_scope": "all_arms",
        "candidate_set_sha256": sha256_file(paths["candidates.json"]),
        "source_blind": True,
        "statistics_unblinded": False,
        "decisions": decisions,
        "case_judgments": case_judgments,
    }
    paths["adjudication.json"].write_text(
        json.dumps(adjudication), encoding="ascii"
    )
    manifest = _typed_oracle_run_manifest_document(
        root=ROOT,
        partition="explore",
        manifest_path=paths["freeze.json"],
        cases_path=paths["cases.jsonl"],
        base_oracle_path=paths["base.jsonl"],
        base_labels=base_labels,
        run_oracle_file_sha256=sha256_file(paths["run.jsonl"]),
        arm_inventory_path=paths["arm-inventory.json"],
        arm_inventory=arm_inventory,
        candidate_set_path=paths["candidates.json"],
        candidate_set=candidate_set,
        adjudication_path=paths["adjudication.json"],
        adjudication=adjudication,
        merged_labels=run_labels,
    )
    paths["manifest.json"].write_text(json.dumps(manifest), encoding="ascii")
    return {
        "paths": paths,
        "case": case,
        "cause": cause_value,
        "candidate": candidate,
        "arm_inventory": arm_inventory,
        "base_labels": base_labels,
        "run_labels": run_labels,
    }


class HistoricalV2CauseAuditBoundaryTest(unittest.TestCase):
    def test_historical_v2_c_r_p_boundaries(self) -> None:
        def cause(
            cause_id: str = "cause-1",
            kind: str = "wrong_move_order",
            rank: int = 0,
            *,
            packet: bool = True,
        ) -> dict[str, object]:
            selected = {"cause_evidence_id": cause_id, "priority_rank": rank}
            return {
                "cause_record": {"id": cause_id},
                "c": {
                    "kind": kind,
                    "source_side": "reference",
                    "comparison": {
                        "kind": "played_vs_best",
                        "reference_root_move": "e2e4",
                        "candidate_root_move": "e2e3",
                        "mover": "white",
                    },
                    "attribution": {
                        "kind": "reference_creates_resource",
                        "root_move_matched": True,
                        "direct_proof_eligible": True,
                    },
                    "binding": {"role": "primary_played_cause", "event_line": {"root_move": "e2e4"}},
                    "objects": {
                        "owned_bindings": [
                            {field: [{}] for field in ("actor", "target", "mechanism", "consequence")}
                        ]
                    },
                },
                "jp": {"linked_claims": [{}]},
                "ja": {"linked_decisions": [{"status": "certified"}]},
                "r": {"cross_comparison_exposure": {"selected": True, "priority_rank": rank}},
                "p": {
                    "cause_evidence_id": cause_id,
                    "packet_selected": packet,
                    "packet_selection": copy.deepcopy(selected) if packet else None,
                    "public_selection_count": 1,
                    "public_selection": copy.deepcopy(selected),
                    "selected_public_cause_evidence_ids": [cause_id],
                },
            }

        def judge(
            causes: list[dict[str, object]],
            forbidden: list[str],
            projection: dict[str, object] | None = None,
        ) -> dict[str, object]:
            selections = [copy.deepcopy(cause["p"]["public_selection"]) for cause in causes]
            projection = projection or {
                "present": True,
                "raw_public_idea_count": len(selections),
                "parsed_public_idea_count": len(selections),
                "public_ideas_parse_closed": True,
                "selected_public_cause_evidence_ids": [item["cause_evidence_id"] for item in selections],
                "selected_public_cause_selections": selections,
            }
            return _judge_case(
                case={"case_id": "case-1", "partition": "explore", "played_move_uci": "e2e3"},
                label={
                    "answerable": True,
                    "expected_cause_kinds": ["WrongMoveOrder"],
                    "forbidden_tempting_causes": forbidden,
                    "expected_reference_move_uci": "e2e4",
                    "acceptable_alternatives": [],
                    "source_side": "white",
                },
                actual={"status": "complete", "probe_closure": {"all_closed": True}, "projection": projection, "causes": causes},
            )

        mismatch = cause()
        mismatch["p"]["public_selection"] = {"cause_evidence_id": "cause-1", "altered": True}
        baseline = cause()
        extra_projection = {
            "present": True,
            "raw_public_idea_count": 2,
            "parsed_public_idea_count": 2,
            "public_ideas_parse_closed": True,
            "selected_public_cause_evidence_ids": ["cause-1", "unregistered-extra"],
            "selected_public_cause_selections": [copy.deepcopy(baseline["p"]["public_selection"]), {"cause_evidence_id": "unregistered-extra"}],
        }
        cases = (
            ("R survives packet loss", [cause(packet=False)], [], None, "P", {"packet_loss"}, {"r": 1, "packet": 0, "p": 0}),
            ("packet/public exact mismatch", [mismatch], [], None, "P", {"projection_set_mismatch"}, None),
            ("extra unregistered public cause", [baseline], [], extra_projection, "P", {"projection_set_mismatch"}, None),
            ("forbidden public cause", [cause(), cause("forbidden", "tempo_loss")], ["TempoLoss"], None, "C", {"forbidden_cause_emitted", "generic_fallback_takeover"}, None),
            ("priority inversion", [cause("expected", rank=1), cause("competing", "material_swing")], [], None, "R", {"priority_inversion"}, None),
        )
        for name, causes, forbidden, projection, stage, errors, cascade in cases:
            with self.subTest(name=name):
                judgment = judge(causes, forbidden, projection)
                self.assertEqual(judgment["first_failure_stage"], stage)
                self.assertTrue(errors.issubset(judgment["errors"]))
                if cascade:
                    self.assertEqual({field: judgment["cascade"][field] for field in cascade}, cascade)


class TypedCauseSemanticContractTest(unittest.TestCase):
    @staticmethod
    def judge(
        label: dict[str, object],
        causes: list[dict[str, object]],
        *,
        importance: dict[str, object] | None = None,
        endpoint_snapshots: list[dict[str, object]] | None = None,
    ) -> dict[str, object]:
        return judge_typed_case(
            case=typed_case(),
            label=label,
            actual=typed_actual(
                causes,
                importance=importance,
                endpoint_snapshots=endpoint_snapshots,
            ),
        )

    def test_exact_same_channel_meaning_survives_c_through_public_p(self) -> None:
        judgment = self.judge(typed_label(), [typed_cause()])
        self.assertEqual(judgment["status"], "matched")
        self.assertEqual(judgment["endpoint_status"], "pass")
        self.assertEqual(judgment["errors"], [])
        self.assertEqual(
            judgment["cascade"],
            {"c": 1, "jp": 1, "ja": 1, "r": 1, "packet": 1, "p": 1},
        )

    def test_same_kind_with_wrong_target_is_a_c_failure(self) -> None:
        item = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            item["c"]["objects"][layer][0]["target"] = [{"kind": "square", "key": "d5"}]
            item["c"]["objects"][layer][0]["effect_descriptor"]["effect_scope"]["target_signatures"] = ["square:d5"]
        judgment = self.judge(typed_label(), [item])
        self.assertEqual(judgment["first_failure_stage"], "C")

    def test_oracle_channels_are_a_conjunction_not_a_cross_channel_union(self) -> None:
        second = typed_channel(
            target="d5", mechanism="space", consequence="center_control"
        )
        label = typed_label(
            [
                typed_meaning(
                    "main",
                    "wrong_move_order",
                    channels=[typed_channel(), second],
                )
            ]
        )
        item = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            owned = item["c"]["objects"][layer][0]
            owned["target"].append({"kind": "square", "key": "d5"})
            owned["mechanism"].append({"kind": "mechanism", "key": "space"})
            owned["consequence"].append({"kind": "consequence", "key": "center_control"})
            owned["effect_descriptor"]["effect_scope"]["target_signatures"] = ["square:e5", "square:d5"]
        judgment = self.judge(label, [item])
        self.assertEqual(judgment["first_failure_stage"], "C")

    def test_c_admission_lineage_rejects_owned_only_forgery(self) -> None:
        def owned(item: dict[str, object]) -> dict[str, object]:
            return item["c"]["objects"]["owned_bindings"][0]

        mutations = (
            lambda channel: channel.__setitem__("source_id", "sibling-cause"),
            lambda channel: channel.__setitem__("provenance_source_ids", ["sibling-cause"]),
            lambda channel: channel["actor"].__setitem__(2, {"kind": "piece", "key": "knight"}),
            lambda channel: channel.__setitem__("target", [{"kind": "square", "key": "d5"}]),
            lambda channel: channel.__setitem__("mechanism", [{"kind": "mechanism", "key": "space"}]),
            lambda channel: channel.__setitem__("consequence", [{"kind": "consequence", "key": "center_control"}]),
            lambda channel: channel["proof_segment"].__setitem__("terminal_relation", "creates_threat"),
            lambda channel: channel["effect_descriptor"]["effect_scope"].__setitem__("target_signatures", ["square:d5"]),
        )
        for mutate in mutations:
            with self.subTest(mutate=mutate):
                item = typed_cause()
                mutate(owned(item))
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [item])

    def test_c_admission_state_is_exactly_bound_to_owned_signatures(self) -> None:
        wrong_signature = typed_cause()
        wrong_signature["c"]["direct_effect_admission"]["causal_signatures"] = [
            "sibling-channel"
        ]
        with self.assertRaisesRegex(IntegrityError, "admission disagrees"):
            self.judge(typed_label(), [wrong_signature])

        unresolved_owned = typed_cause()
        unresolved_owned["c"]["direct_effect_admission"] = {
            "status": "unresolved",
            "causal_signatures": [],
        }
        with self.assertRaisesRegex(IntegrityError, "admission disagrees"):
            self.judge(typed_label(), [unresolved_owned])

    def test_c_rejects_a_partially_verified_admitted_channel_set(self) -> None:
        item = typed_cause()
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            invalid = copy.deepcopy(item["c"]["objects"][layer][0])
            invalid["signature"] = "binding-invalid-channel"
            invalid["causal_signature"] = "invalid-channel"
            invalid["primitive_signature"] = "primitive-invalid-channel"
            invalid["actor"][0]["key"] = "e2e3"
            item["c"]["objects"][layer].append(invalid)
        item["c"]["direct_effect_admission"]["causal_signatures"].append(
            "invalid-channel"
        )
        for selection_value in (
            item["r"]["native_selection"],
            item["p"]["packet_selection"],
            item["p"]["public_selection"],
        ):
            selection_value["direct_effect_admission"]["causal_signatures"].append(
                "invalid-channel"
            )
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [item])

    def test_r_selection_must_preserve_c_admission_and_channel_lineage(self) -> None:
        admission_drift = typed_cause()
        admission_drift["r"]["native_selection"]["direct_effect_admission"][
            "causal_signatures"
        ] = ["sibling-channel"]
        judgment = self.judge(typed_label(), [admission_drift])
        self.assertEqual(judgment["first_failure_stage"], "R")
        self.assertIn("wrong_r_semantics", [error["code"] for error in judgment["errors"]])

        for field, mutate in (
            (
                "carrier",
                lambda channel: channel["carrier"].__setitem__("id", "sibling-source"),
            ),
            (
                "provenance",
                lambda channel: channel.__setitem__("provenance", []),
            ),
            (
                "effect_descriptor",
                lambda channel: (
                    channel["effect_descriptor"]["effect_scope"].__setitem__(
                        "plan_ids", ["borrowed-plan"]
                    ),
                    channel["importance_effect"]["effect_scope"].__setitem__(
                        "plan_ids", ["borrowed-plan"]
                    ),
                ),
            ),
        ):
            with self.subTest(field=field):
                item = typed_cause()
                mutate(item["r"]["native_selection"]["channels"][0])
                judgment = self.judge(typed_label(), [item])
                self.assertEqual(judgment["first_failure_stage"], "R")
                self.assertIn(
                    "wrong_r_semantics",
                    [error["code"] for error in judgment["errors"]],
                )

    def test_c_admitted_only_is_integrity_error_and_context_borrowing_is_c_mismatch(self) -> None:
        admitted_only = typed_cause()
        forged = copy.deepcopy(admitted_only["c"]["objects"]["owned_bindings"][0])
        forged["causal_signature"] = "admitted-only-forgery"
        admitted_only["c"]["objects"]["owned_bindings"].append(forged)
        admitted_only["c"]["direct_effect_admission"]["causal_signatures"].append(
            "admitted-only-forgery"
        )
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [admitted_only])

        context_borrowing = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            channel = context_borrowing["c"]["objects"][layer][0]
            channel["source_id"] = "context-source"
            channel["carrier"]["id"] = "context-source"
        context_borrowing["c"]["proof"]["context"] = {
            "role": "context_support",
            "strength": "weak_hint",
            "proof_kind_labels": [],
            "sources": [{
                "id": "context-source",
                "producer": "runtime",
                "layer": "causal",
                "scope": "direct",
                "confidence": "high",
                "line": {
                    "id": "reference-line",
                    "role": "best_reference",
                    "rank": 1,
                    "root_move": "e2e4",
                },
                "record_registered": True,
                "record_payload_type": "LineFactEvidence",
                "record_sha256": "0" * 64,
                "proof_kind_labels": ["context"],
            }],
        }
        refresh_c_source_identity_digests(context_borrowing)
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [context_borrowing])

    def test_c_comparison_and_line_identity_swaps_are_integrity_errors(self) -> None:
        mutations = (
            lambda item: item["c"]["comparison"]["reference_line"].__setitem__("id", "candidate-line"),
            lambda item: item["c"]["comparison"]["candidate_line"].__setitem__("root_move", "e2e4"),
            lambda item: item["c"]["binding"]["event_line"].__setitem__("id", "candidate-line"),
            lambda item: item["c"]["objects"]["owned_bindings"][0].__setitem__("line_id", "candidate-line"),
        )
        for mutate in mutations:
            with self.subTest(mutation=mutate):
                item = typed_cause()
                mutate(item)
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [item])

    def test_c_empty_owned_is_valid_inventory_and_booleans_are_not_truth(self) -> None:
        no_direct = typed_cause()
        no_direct["c"]["objects"]["owned_bindings"] = []
        no_direct["c"]["direct_effect_admission"] = {
            "status": "restricted",
            "causal_signatures": [],
        }
        judgment = self.judge(typed_label(), [no_direct])
        self.assertEqual(judgment["first_failure_stage"], "C")
        self.assertIn(
            "cause_admission_dropped",
            [error["code"] for error in judgment["errors"]],
        )

        booleans_forged = typed_cause()
        booleans_forged["c"]["attribution"]["root_move_matched"] = False
        booleans_forged["c"]["attribution"]["direct_proof_eligible"] = False
        matched = self.judge(typed_label(), [booleans_forged])
        self.assertEqual(matched["status"], "matched")

    def test_reference_creates_resource_normal_c_match_is_preserved(self) -> None:
        item = typed_cause()
        self.assertEqual(item["c"]["attribution"]["kind"], "reference_creates_resource")
        self.assertEqual(self.judge(typed_label(), [item])["status"], "matched")

    def test_c_failure_reasons_distinguish_generation_and_admission(self) -> None:
        seed = typed_cause()
        snapshots = typed_endpoint_snapshots([seed])

        no_cause = self.judge(
            typed_label(), [], endpoint_snapshots=copy.deepcopy(snapshots)
        )
        self.assertIn(
            "cause_not_generated", {item["code"] for item in no_cause["errors"]}
        )

        admission_dropped = typed_cause()
        admission_dropped["c"]["objects"]["owned_bindings"] = []
        admission_dropped["c"]["direct_effect_admission"] = {
            "status": "restricted",
            "causal_signatures": [],
        }
        dropped = self.judge(
            typed_label(),
            [admission_dropped],
            endpoint_snapshots=copy.deepcopy(snapshots),
        )
        self.assertIn(
            "cause_admission_dropped",
            {item["code"] for item in dropped["errors"]},
        )

        no_direct_evidence = typed_cause()
        no_direct_evidence["c"]["objects"]["raw_owned_bindings"] = []
        no_direct_evidence["c"]["objects"]["pre_admission_owned_bindings"] = []
        no_direct_evidence["c"]["objects"]["owned_bindings"] = []
        no_direct_evidence["c"]["direct_effect_admission"] = {
            "status": "restricted",
            "causal_signatures": [],
        }
        missing = self.judge(
            typed_label(),
            [no_direct_evidence],
            endpoint_snapshots=copy.deepcopy(snapshots),
        )
        self.assertIn(
            "missing_required_meaning",
            {item["code"] for item in missing["errors"]},
        )

    def test_neutral_snapshot_rejects_coordinated_c_rewrite(self) -> None:
        item = typed_cause()
        snapshots = typed_endpoint_snapshots([item])
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            channel = item["c"]["objects"][layer][0]
            channel["carrier"]["producer"] = "forged_producer"
            channel["provenance"][0]["producer"] = "forged_producer"
            channel["primitive_proof_source"]["producer"] = "forged_producer"
            channel["actor"][2] = {"kind": "piece", "key": "knight"}
            channel["target"] = [{"kind": "square", "key": "d5"}]
            channel["mechanism"] = [{"kind": "mechanism", "key": "space"}]
            channel["consequence"] = [
                {"kind": "consequence", "key": "center_control"}
            ]
            channel["effect_descriptor"]["effect_scope"][
                "target_signatures"
            ] = ["square:d5"]
        item["c"]["proof"]["direct"]["sources"][0][
            "producer"
        ] = "forged_producer"
        refresh_c_source_identity_digests(item)
        with self.assertRaisesRegex(IntegrityError, "neutral endpoint witness"):
            self.judge(
                typed_label(), [item], endpoint_snapshots=copy.deepcopy(snapshots)
            )

    def test_neutral_snapshot_rejects_coordinated_source_polarity_rewrite(self) -> None:
        item = typed_cause()
        snapshots = typed_endpoint_snapshots([item])
        c_value = item["c"]
        candidate_line = copy.deepcopy(c_value["comparison"]["candidate_line"])
        c_value["source_side"] = "candidate"
        c_value["attribution"]["kind"] = "candidate_allows_liability"
        c_value["binding"]["source_side"] = "candidate"
        c_value["binding"]["event_line"] = copy.deepcopy(candidate_line)
        c_value["binding"]["evidence_lines"] = [copy.deepcopy(candidate_line)]
        c_value["proof"]["direct"]["sources"][0]["line"] = copy.deepcopy(
            candidate_line
        )
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            channel = c_value["objects"][layer][0]
            channel["line_id"] = "candidate-line"
            channel["line"] = {
                "line_id": "candidate-line",
                "role": "played",
                "rank": 1,
                "root_move": "e2e3",
            }
            channel["actor"] = [
                {"kind": "move", "key": "e2e3"},
                {"kind": "side", "key": "white"},
                {"kind": "piece", "key": "pawn"},
                {"kind": "square", "key": "e2"},
                {"kind": "square", "key": "e3"},
            ]
            channel["proof_segment"]["steps"][0]["move_uci"] = "e2e3"
            for ref_name in ("carrier", "primitive_proof_source"):
                channel[ref_name]["line"] = copy.deepcopy(candidate_line)
            channel["provenance"][0]["line"] = copy.deepcopy(candidate_line)
        refresh_c_source_identity_digests(item)

        with self.assertRaisesRegex(IntegrityError, "neutral endpoint witness"):
            self.judge(
                typed_label(), [item], endpoint_snapshots=copy.deepcopy(snapshots)
            )

    def test_neutral_snapshot_documentary_closure_and_uniqueness(self) -> None:
        item = typed_cause()
        outside_ancestry = typed_endpoint_snapshots([item])
        outside_ancestry[0]["reference"]["witnesses"][0][
            "carrier_ancestor_source_ids"
        ] = []
        with self.assertRaisesRegex(IntegrityError, "outside carrier ancestry"):
            self.judge(
                typed_label(), [item], endpoint_snapshots=outside_ancestry
            )

        duplicate = typed_endpoint_snapshots([item])
        alias = copy.deepcopy(duplicate[0]["reference"]["witnesses"][0])
        alias["actor"].reverse()
        duplicate[0]["reference"]["witnesses"].append(alias)
        with self.assertRaisesRegex(IntegrityError, "ambiguous duplicate witness"):
            self.judge(typed_label(), [item], endpoint_snapshots=duplicate)

    def test_candidate_owned_conversion_changes_keep_neutral_witness(self) -> None:
        for cause_kind, attribution_kind, direct_change in (
            ("conversion_miss", "candidate_allows_liability", "missed"),
            ("conversion_secured", "candidate_creates_value", "occurred"),
        ):
            with self.subTest(cause_kind=cause_kind):
                item = typed_cause(cause_kind=cause_kind, native=False)
                c_value = item["c"]
                c_value["source_side"] = "candidate"
                c_value["attribution"]["kind"] = attribution_kind
                c_value["binding"]["source_side"] = "candidate"
                c_value["binding"]["event_line"] = copy.deepcopy(
                    c_value["comparison"]["candidate_line"]
                )
                c_value["binding"]["evidence_lines"] = [
                    copy.deepcopy(c_value["comparison"]["candidate_line"])
                ]
                c_value["proof"]["direct"]["sources"][0]["line"] = copy.deepcopy(
                    c_value["comparison"]["candidate_line"]
                )
                for layer in (
                    "raw_owned_bindings",
                    "pre_admission_owned_bindings",
                    "owned_bindings",
                ):
                    channel = c_value["objects"][layer][0]
                    channel["direct_change"] = direct_change
                    channel["line_id"] = "candidate-line"
                    channel["line"] = {
                        "line_id": "candidate-line",
                        "role": "played",
                        "rank": 1,
                        "root_move": "e2e3",
                    }
                    channel["actor"] = [
                        {"kind": "move", "key": "e2e3"},
                        {"kind": "side", "key": "white"},
                        {"kind": "piece", "key": "pawn"},
                        {"kind": "square", "key": "e2"},
                        {"kind": "square", "key": "e3"},
                    ]
                    channel["proof_segment"]["steps"][0]["move_uci"] = "e2e3"
                    for ref_name in ("carrier", "primitive_proof_source"):
                        channel[ref_name]["line"] = copy.deepcopy(
                            c_value["comparison"]["candidate_line"]
                        )
                    channel["provenance"][0]["line"] = copy.deepcopy(
                        c_value["comparison"]["candidate_line"]
                    )
                refresh_c_source_identity_digests(item)
                label = typed_label(
                    [typed_meaning("conversion", cause_kind, exposure="diagnostic_only")],
                    disposition="diagnostic_only",
                    top=[],
                )
                realization = label["meanings"][0]["realizations"][0]
                realization["source_side"] = "candidate"
                realization["attribution_kind"] = attribution_kind
                realization["comparison"]["event_role"] = "played"
                realization["comparison"]["event_root_move"] = "e2e3"
                realization["channels"][0]["direct_change"] = direct_change
                realization["channels"][0]["actor"] = {
                    "move": "e2e3",
                    "side": "white",
                    "piece": "pawn",
                    "from": "e2",
                    "to": "e3",
                }
                realization["channels"][0]["line"] = {
                    "role": "played",
                    "root_move": "e2e3",
                }
                snapshot_index = typed_snapshot_index([item])
                self.assertTrue(
                    cause_matches_typed_oracle(item, label, snapshot_index)
                )

    def test_defensive_resource_reference_creates_resource_has_no_kind_filter(self) -> None:
        label = typed_label([typed_meaning("defense", "defensive_resource")], top=["defense"])
        self.assertEqual(
            self.judge(label, [typed_cause(cause_kind="defensive_resource")])["status"],
            "matched",
        )

    def test_coherent_direct_channel_failures_are_c_mismatches(self) -> None:
        actor_mismatch = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            actor_mismatch["c"]["objects"][layer][0]["actor"][0]["key"] = "e2e3"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [actor_mismatch])

        for field, mutate in (
            (
                "proof_root",
                lambda channel: channel["proof_segment"]["steps"][0].__setitem__("move_uci", "e2e3"),
            ),
            (
                "descriptor_targets",
                lambda channel: channel["effect_descriptor"]["effect_scope"].__setitem__("target_signatures", ["square:d5"]),
            ),
        ):
            with self.subTest(field=field):
                item = typed_cause()
                for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
                    mutate(item["c"]["objects"][layer][0])
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [item])

    def test_r_rejects_importance_and_diagnostic_drift_from_verified_c(self) -> None:
        item = typed_cause()
        selected = item["r"]["native_selection"]["channels"][0]
        selected["carrier"]["id"] = "sibling-source"
        judgment = self.judge(typed_label(), [item])
        self.assertEqual(judgment["first_failure_stage"], "R")
        self.assertIn("wrong_r_semantics", [error["code"] for error in judgment["errors"]])

    def test_c_carrier_provenance_and_neutral_projection_repros_are_rejected(self) -> None:
        provenance_forged = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            provenance_forged["c"]["objects"][layer][0]["provenance_source_ids"] = ["forged"]
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [provenance_forged])

        terminal_forged = typed_cause()
        endpoint_snapshots = typed_endpoint_snapshots([terminal_forged])
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            terminal_forged["c"]["objects"][layer][0]["proof_segment"]["terminal_relation"] = "creates_threat"
        with self.assertRaises(IntegrityError):
            self.judge(
                typed_label(),
                [terminal_forged],
                endpoint_snapshots=endpoint_snapshots,
            )

        source_line_forged = typed_cause()
        source_line_forged["c"]["proof"]["direct"]["sources"][0]["line"]["root_move"] = "e2e3"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [source_line_forged])

        descriptor_absent = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            descriptor_absent["c"]["objects"][layer][0]["effect_descriptor"] = None
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [descriptor_absent])

    def test_c_coordinated_source_metadata_rewrites_keep_no_authority(self) -> None:
        provenance_rewrite = typed_cause()
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            channel = provenance_rewrite["c"]["objects"][layer][0]
            channel["provenance"][0]["producer"] = "forged_producer"
            channel["primitive_proof_source"]["producer"] = "forged_producer"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [provenance_rewrite])

        carrier_rewrite = typed_cause()
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            carrier_rewrite["c"]["objects"][layer][0]["carrier"][
                "producer"
            ] = "forged_producer"
        carrier_rewrite["c"]["proof"]["direct"]["sources"][0][
            "producer"
        ] = "forged_producer"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [carrier_rewrite])

    def test_c_descriptor_target_signatures_normalize_case(self) -> None:
        item = typed_cause()
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            item["c"]["objects"][layer][0]["effect_descriptor"]["effect_scope"]["target_signatures"] = [" Square:E5 "]
            item["c"]["objects"][layer][0]["importance_effect"]["effect_scope"]["target_signatures"] = [" Square:E5 "]
        refresh_selection_lineage_from_c(item)
        self.assertEqual(self.judge(typed_label(), [item])["status"], "matched")

    def test_endpoint_descriptor_preserves_plan_subject_and_rejects_aliases(self) -> None:
        item = typed_cause()
        channel = item["c"]["objects"]["raw_owned_bindings"][0]
        channel["target"] = [{"kind": "plan_subject", "key": "material-sacrifice:e5"}]
        channel["effect_descriptor"]["effect_scope"]["target_signatures"] = [
            " PlanSubject:MATERIAL-SACRIFICE:E5 "
        ]
        typed_snapshot_index([item])

        channel["effect_descriptor"]["effect_scope"]["target_signatures"] = [
            "Relation:material-sacrifice:e5"
        ]
        with self.assertRaises(IntegrityError):
            typed_snapshot_index([item])

    def test_c_multistep_proof_is_ordered_and_role_closed(self) -> None:
        item = typed_cause()
        steps = [
            {"role": "root_action", "ply_offset": 0, "move_uci": "e2e4"},
            {"role": "causal_link", "ply_offset": 1, "move_uci": "e7e5"},
            {"role": "terminal_event", "ply_offset": 2, "move_uci": "g1f3"},
        ]
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            item["c"]["objects"][layer][0]["proof_segment"]["steps"] = copy.deepcopy(steps)
        refresh_selection_lineage_from_c(item)
        self.assertEqual(self.judge(typed_label(), [item])["status"], "matched")

        for field, value in (("role", "terminal_event"), ("ply_offset", 0)):
            with self.subTest(field=field):
                forged = typed_cause()
                for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
                    forged["c"]["objects"][layer][0]["proof_segment"]["steps"] = copy.deepcopy(steps)
                endpoint_snapshots = typed_endpoint_snapshots([forged])
                for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
                    forged["c"]["objects"][layer][0]["proof_segment"]["steps"][1][field] = value
                with self.assertRaises(IntegrityError):
                    self.judge(
                        typed_label(),
                        [forged],
                        endpoint_snapshots=endpoint_snapshots,
                    )

    def test_c_ten_non_defensive_proof_terminal_families_match(self) -> None:
        families = (
            ("line_episode", "produces_line_consequence", "LineFactEvidence"),
            ("root_line_event", "is_root_line_event", "LineFactEvidence"),
            ("endgame_horizon", "triggers_endgame_horizon", "LineFactEvidence"),
            ("structural_transition", "makes_structural_transition", "StructuralDeltaEvidence"),
            ("root_move_motif", "instantiates_motif", "MoveMotifEvidence"),
            ("root_relation", "instantiates_relation", "RelationFactEvidence"),
            ("threat_creation", "creates_threat", "ThreatEpisodeEvidence"),
            ("threat_defense", "defends_threat", "ThreatEpisodeEvidence"),
            ("plan_result", "realizes_plan_result", "PlanCausalEventEvidence"),
            ("plan_restriction", "restricts_opponent_resource", "PlanCausalEventEvidence"),
        )
        for primitive, terminal, payload_type in families:
            with self.subTest(primitive=primitive):
                item = typed_cause()
                for layer in (
                    "raw_owned_bindings",
                    "pre_admission_owned_bindings",
                    "owned_bindings",
                ):
                    descriptor = item["c"]["objects"][layer][0]["effect_descriptor"]
                    descriptor["effect_scope"]["primitive_kind"] = primitive
                    item["c"]["objects"][layer][0]["proof_segment"]["terminal_relation"] = terminal
                    channel = item["c"]["objects"][layer][0]
                    channel["importance_effect"]["effect_scope"]["primitive_kind"] = primitive
                    channel["provenance"][0]["record_payload_type"] = payload_type
                    channel["primitive_proof_source"]["record_payload_type"] = payload_type
                refresh_c_source_identity_digests(item)
                refresh_selection_lineage_from_c(item)
                self.assertEqual(self.judge(typed_label(), [item])["status"], "matched")

        forged = typed_cause()
        endpoint_snapshots = typed_endpoint_snapshots([forged])
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            channel = forged["c"]["objects"][layer][0]
            channel["effect_descriptor"]["effect_scope"]["primitive_kind"] = (
                "threat_creation"
            )
            channel["proof_segment"]["terminal_relation"] = "creates_threat"
        with self.assertRaises(IntegrityError):
            self.judge(
                typed_label(),
                [forged],
                endpoint_snapshots=endpoint_snapshots,
            )

    def test_c_defensive_recapture_resource_keeps_its_reference_root(self) -> None:
        item = typed_cause("cause-defensive", "defensive_resource")
        candidate_line = {
            "id": "candidate-line",
            "role": "played",
            "rank": 1,
            "root_move": "e2e3",
        }
        primitive = {
            "id": "proof-cause-defensive",
            "producer": "runtime",
            "layer": "causal",
            "scope": "direct",
            "confidence": "high",
            "line": copy.deepcopy(candidate_line),
            "record_registered": True,
            "record_payload_type": "CandidateComparisonEvidence",
            "record_sha256": "0" * 64,
        }
        provenance = {
            "id": "primitive-cause-defensive",
            "producer": "runtime",
            "layer": "primitive",
            "scope": "context",
            "confidence": "high",
            "line": copy.deepcopy(candidate_line),
            "record_registered": True,
            "record_payload_type": "LineFactEvidence",
            "record_sha256": "1" * 64,
        }
        direct_source = copy.deepcopy(primitive)
        direct_source["proof_kind_labels"] = ["direct"]
        item["c"]["proof"]["direct"]["sources"] = [direct_source]
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            channel = item["c"]["objects"][layer][0]
            channel["carrier"] = copy.deepcopy(primitive)
            channel["primitive_proof_source"] = copy.deepcopy(primitive)
            channel["provenance"] = [copy.deepcopy(provenance)]
            channel["provenance_source_ids"] = [provenance["id"]]
            channel["carrier_ancestor_source_ids"] = [provenance["id"]]
            channel["effect_descriptor"]["effect_scope"]["primitive_kind"] = (
                "defensive_recapture_resource"
            )
            channel["importance_effect"]["effect_scope"]["primitive_kind"] = (
                "defensive_recapture_resource"
            )
            channel["proof_segment"]["terminal_relation"] = (
                "creates_defensive_recapture_resource"
            )
        refresh_c_source_identity_digests(item)
        refresh_selection_lineage_from_c(item)
        label = typed_label(
            [typed_meaning("main", "defensive_resource")]
        )
        self.assertEqual(self.judge(label, [item])["status"], "matched")

        registry, _ = _schema_tools(ROOT, TYPED_ACTUAL_VIEW_SCHEMA_VERSION)
        schema_path = _schema_path(
            ROOT, "actual-cascade-view", TYPED_ACTUAL_VIEW_SCHEMA_VERSION
        )
        schema = registry.load(schema_path)
        self.assertEqual(
            registry._branch_errors(
                item["c"]["objects"]["owned_bindings"][0],
                schema["$defs"]["ownedDirectCauseChannelV3"],
                schema_path,
                "$",
            ),
            [],
        )

    def test_c_direct_source_record_registration_triples(self) -> None:
        unregistered = typed_cause()
        source = unregistered["c"]["proof"]["direct"]["sources"][0]
        source["record_registered"] = False
        source["record_payload_type"] = None
        source["record_sha256"] = None
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            carrier = unregistered["c"]["objects"][layer][0]["carrier"]
            carrier["record_registered"] = False
            carrier["record_payload_type"] = None
            carrier["record_sha256"] = None
        refresh_c_source_identity_digests(unregistered)
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [unregistered])

        for field, value in (("record_payload_type", "LineFactEvidence"), ("record_sha256", "0" * 64)):
            with self.subTest(unregistered_field=field):
                forged = typed_cause()
                source = forged["c"]["proof"]["direct"]["sources"][0]
                source["record_registered"] = False
                source[field] = value
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [forged])
        registered = typed_cause()
        registered["c"]["proof"]["direct"]["sources"][0]["record_sha256"] = "bad"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [registered])

    def test_c_context_only_preserves_mismatch_but_not_document_contradiction(self) -> None:
        coherent = typed_cause()
        coherent["c"]["attribution"]["kind"] = "context_only"
        coherent["c"]["objects"]["owned_bindings"] = []
        coherent["c"]["direct_effect_admission"] = {
            "status": "restricted",
            "causal_signatures": [],
        }
        judgment = self.judge(typed_label(), [coherent])
        self.assertEqual(judgment["first_failure_stage"], "C")

        contradictory = typed_cause()
        contradictory["c"]["attribution"]["kind"] = "context_only"
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            contradictory["c"]["objects"][layer][0]["carrier"]["id"] = "forged-carrier"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [contradictory])

        duplicate_source = typed_cause()
        duplicate_source["c"]["attribution"]["kind"] = "context_only"
        duplicate_source["c"]["proof"]["direct"]["sources"].append(
            copy.deepcopy(
                duplicate_source["c"]["proof"]["direct"]["sources"][0]
            )
        )
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [duplicate_source])

    def test_c_context_only_still_rejects_proof_and_descriptor_contradictions(self) -> None:
        for name, mutate in (
            (
                "descriptor_terminal",
                lambda channel: channel["proof_segment"].__setitem__(
                    "terminal_relation", "creates_threat"
                ),
            ),
            (
                "context_proof_role_target",
                lambda channel: (
                    channel.__setitem__("proof_role", "context_support"),
                    channel["effect_descriptor"]["effect_scope"].__setitem__(
                        "target_signatures", ["square:d5"]
                    ),
                ),
            ),
        ):
            with self.subTest(name=name):
                item = typed_cause()
                item["c"]["attribution"]["kind"] = "context_only"
                for layer in (
                    "raw_owned_bindings",
                    "pre_admission_owned_bindings",
                    "owned_bindings",
                ):
                    mutate(item["c"]["objects"][layer][0])
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [item])

    def test_c_provenance_ancestry_and_registration_gate(self) -> None:
        forged = typed_cause()
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            channel = forged["c"]["objects"][layer][0]
            channel["provenance_source_ids"] = ["forged-provenance"]
            channel["provenance"][0]["id"] = "forged-provenance"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [forged])

        unregistered = typed_cause()
        for layer in (
            "raw_owned_bindings",
            "pre_admission_owned_bindings",
            "owned_bindings",
        ):
            provenance = unregistered["c"]["objects"][layer][0]["provenance"][0]
            provenance["record_registered"] = False
            provenance["record_payload_type"] = None
            provenance["record_sha256"] = None
            unregistered["c"]["objects"][layer][0][
                "primitive_proof_source"
            ] = copy.deepcopy(provenance)
        refresh_c_source_identity_digests(unregistered)
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [unregistered])

    def test_c_rejects_candidate_direct_source_viewpoint_but_allows_raw_aliases(self) -> None:
        candidate_source = typed_cause()
        second = copy.deepcopy(candidate_source["c"]["proof"]["direct"]["sources"][0])
        second["id"] = "proof-candidate"
        candidate_source["c"]["proof"]["direct"]["sources"].append(second)
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings", "owned_bindings"):
            channel = candidate_source["c"]["objects"][layer][0]
            channel["source_id"] = "proof-candidate"
            channel["carrier"]["id"] = "proof-candidate"
            channel["carrier"]["line"] = {"line_id": "candidate-line", "role": "played", "rank": 1, "root_move": "e2e3"}
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [candidate_source])

        alias = typed_cause()
        duplicate = copy.deepcopy(alias["c"]["objects"]["raw_owned_bindings"][0])
        duplicate["causal_signature"] = "raw-semantic-alias"
        alias["c"]["objects"]["raw_owned_bindings"].append(duplicate)
        self.assertEqual(self.judge(typed_label(), [alias])["status"], "matched")

        sparse_alias = typed_cause()
        duplicate = copy.deepcopy(sparse_alias["c"]["objects"]["raw_owned_bindings"][0])
        duplicate["causal_signature"] = "raw-sparse-semantic-alias"
        duplicate["effect_descriptor"] = None
        sparse_alias["c"]["objects"]["raw_owned_bindings"].append(duplicate)
        self.assertEqual(
            self.judge(typed_label(), [sparse_alias])["status"], "matched"
        )

    def test_c_direct_witness_rejects_swapped_roots_and_duplicate_signatures(self) -> None:
        mutations = (
            lambda item: item["c"]["comparison"].__setitem__("reference_root_move", "e2e3"),
            lambda item: item["c"]["binding"]["event_line"].__setitem__("root_move", "e2e3"),
            lambda item: item["c"]["objects"]["owned_bindings"][0]["line"].__setitem__("root_move", "e2e3"),
            lambda item: item["c"]["objects"]["owned_bindings"].append(copy.deepcopy(item["c"]["objects"]["owned_bindings"][0])),
        )
        for mutate in mutations:
            with self.subTest(mutate=mutate):
                item = typed_cause()
                mutate(item)
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [item])

    def test_c_raw_or_pre_duplicate_signature_and_role_mismatch_are_integrity_errors(self) -> None:
        for layer in ("raw_owned_bindings", "pre_admission_owned_bindings"):
            with self.subTest(layer=layer):
                item = typed_cause()
                duplicate = copy.deepcopy(item["c"]["objects"][layer][0])
                duplicate["source_id"] = f"ambiguous-{layer}"
                item["c"]["objects"][layer].append(duplicate)
                with self.assertRaises(IntegrityError):
                    self.judge(typed_label(), [item])
        item = typed_cause()
        item["c"]["binding"]["role"] = "alternative_diagnostic"
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [item])

    def test_c_proof_presence_and_evidence_endpoint_closure_are_integrity_errors(self) -> None:
        proof_absent = typed_cause()
        proof_absent["c"]["proof"]["present"] = False
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [proof_absent])

        sibling_evidence = typed_cause()
        sibling_evidence["c"]["binding"]["evidence_lines"].append(
            {"id": "sibling-line", "role": "threat", "rank": 1, "root_move": "d2d4"}
        )
        with self.assertRaises(IntegrityError):
            self.judge(typed_label(), [sibling_evidence])

    def test_r_semantics_reject_played_change_or_source_mutation(self) -> None:
        for field, value in (
            ("played_change", "occurred"),
            ("source_side", "candidate"),
        ):
            with self.subTest(field=field):
                item = typed_cause()
                native = item["r"]["native_selection"]
                assert isinstance(native, dict)
                if field == "played_change":
                    native["channels"][0][field] = value
                else:
                    native[field] = value
                item["p"]["packet_selection"] = copy.deepcopy(native)
                item["p"]["public_selection"] = copy.deepcopy(native)
                with self.assertRaises(ContractError):
                    self.judge(typed_label(), [item])

    def test_packet_mutation_is_p_loss_not_r_loss(self) -> None:
        item = typed_cause()
        packet = item["p"]["packet_selection"]
        assert isinstance(packet, dict)
        packet["comparison_exposure_rank"] = 7
        judgment = self.judge(typed_label(), [item])
        self.assertEqual(judgment["first_failure_stage"], "P")
        codes = [error["code"] for error in judgment["errors"]]
        self.assertIn("r_to_packet_mismatch", codes)
        self.assertNotIn("r_loss", codes)

    def test_jp_and_ja_survival_require_the_causes_own_direct_claim(self) -> None:
        wrong_jp = typed_cause()
        wrong_jp["jp"]["linked_claims"][0]["evidence_ids"] = ["sibling-cause"]
        judgment = self.judge(typed_label(), [wrong_jp])
        self.assertEqual(judgment["first_failure_stage"], "Jp")
        self.assertIn("jp_loss", [error["code"] for error in judgment["errors"]])

        wrong_ja = typed_cause()
        wrong_ja["ja"]["linked_decisions"][0]["claim_id"] = "sibling-claim"
        judgment = self.judge(typed_label(), [wrong_ja])
        self.assertEqual(judgment["first_failure_stage"], "Ja")
        self.assertIn("ja_loss", [error["code"] for error in judgment["errors"]])

    def test_per_cause_packet_identity_cannot_be_hidden_by_a_global_swap(self) -> None:
        label = typed_label(
            [
                typed_meaning("main", "wrong_move_order"),
                typed_meaning("peer", "material_swing"),
            ]
        )
        main = typed_cause()
        peer = typed_cause("cause-peer", "material_swing")
        for boundary in ("packet_selection", "public_selection"):
            main_value = copy.deepcopy(main["p"][boundary])
            main["p"][boundary] = copy.deepcopy(peer["p"][boundary])
            peer["p"][boundary] = main_value
        judgment = self.judge(label, [main, peer])
        self.assertEqual(judgment["first_failure_stage"], "P")
        self.assertIn(
            "packet_to_public_mismatch",
            [error["code"] for error in judgment["errors"]],
        )

    def test_generation_and_exposure_obligations_are_independent(self) -> None:
        diagnostic = typed_label(
            [
                typed_meaning(
                    "diagnostic",
                    "wrong_move_order",
                    exposure="diagnostic_only",
                )
            ],
            disposition="must_abstain",
            top=[],
        )
        diagnostic_cause = typed_cause(
            jp=False, ja=False, native=False, packet=False, public=False
        )
        judgment = self.judge(diagnostic, [diagnostic_cause])
        self.assertEqual(judgment["status"], "must_abstain_pass")
        self.assertEqual(judgment["endpoint_status"], "pass")
        self.assertEqual(judgment["errors"], [])

        allowed_complement = typed_meaning(
            "complement",
            "material_swing",
            generation="allowed",
            exposure="complementary",
        )
        label = typed_label(
            [typed_meaning("main", "wrong_move_order"), allowed_complement]
        )
        absent = self.judge(label, [typed_cause()])
        self.assertEqual(absent["status"], "matched")
        generated = typed_cause(
            "cause-complement",
            "material_swing",
            exposure="complementary",
            jp=False,
            ja=False,
            native=False,
            packet=False,
            public=False,
        )
        present = self.judge(label, [typed_cause(), generated])
        self.assertEqual(present["first_failure_stage"], "Jp")
        self.assertIn(
            "jp_loss", [error["code"] for error in present["errors"]]
        )

    def test_oracle_cannot_assign_one_semantic_realization_twice(self) -> None:
        duplicate = typed_meaning(
            "duplicate",
            "wrong_move_order",
            generation="forbidden",
            exposure="forbidden",
        )
        label = typed_label(
            [typed_meaning("main", "wrong_move_order"), duplicate]
        )
        with self.assertRaisesRegex(ContractError, "realization more than once"):
            self.judge(label, [typed_cause()])

    def test_active_dominator_suppresses_fallback_forward_obligation(self) -> None:
        label = typed_label(
            [
                typed_meaning("main", "wrong_move_order"),
                typed_meaning(
                    "fallback",
                    "material_swing",
                    exposure="fallback_only",
                    dominated_by=["main"],
                ),
            ]
        )
        fallback = typed_cause(
            "cause-fallback",
            "material_swing",
            jp=False,
            ja=False,
            native=False,
            packet=False,
            public=False,
        )
        fallback["r"]["fallback_dominance"] = {
            "status": "dominated_fallback",
            "retained": False,
            "dominating_cause_evidence_ids": ["cause-typed"],
        }
        judgment = self.judge(label, [typed_cause(), fallback])
        self.assertEqual(judgment["status"], "matched")
        self.assertNotIn(
            "jp_loss", [error["code"] for error in judgment["errors"]]
        )

        exposed = typed_cause(
            "cause-fallback",
            "material_swing",
            exposure="complementary",
        )
        exposed["r"]["fallback_dominance"] = {
            "status": "dominated_fallback",
            "retained": False,
            "dominating_cause_evidence_ids": ["cause-typed"],
        }
        judgment = self.judge(label, [typed_cause(), exposed])
        self.assertEqual(judgment["first_failure_stage"], "R")
        self.assertIn(
            "dominated_fallback_exposed",
            [error["code"] for error in judgment["errors"]],
        )

    def test_ja_certified_diagnostic_dominator_does_not_suppress_fallback(self) -> None:
        fallback_meaning = typed_meaning(
            "fallback",
            "material_swing",
            exposure="fallback_only",
            dominated_by=["main"],
        )
        diagnostic = typed_cause(native=False)
        self.assertTrue(diagnostic["ja"]["linked_decisions"])
        assignment = {"main": "cause-typed", "fallback": "cause-fallback"}
        cause_by_id = {
            "cause-typed": diagnostic,
            "cause-fallback": typed_cause("cause-fallback", "material_swing"),
        }

        self.assertFalse(
            _active_fallback_dominator(fallback_meaning, assignment, cause_by_id)
        )

        selected = typed_cause()
        cause_by_id["cause-typed"] = selected
        fallback_cause = cause_by_id["cause-fallback"]
        fallback_cause["r"]["fallback_dominance"] = {
            "status": "dominated_fallback",
            "retained": False,
            "dominating_cause_evidence_ids": ["cause-typed"],
        }
        self.assertTrue(
            _active_fallback_dominator(fallback_meaning, assignment, cause_by_id)
        )

        unadmitted = copy.deepcopy(selected)
        unadmitted["ja"]["linked_decisions"] = []
        cause_by_id["cause-typed"] = unadmitted
        self.assertFalse(
            _active_fallback_dominator(fallback_meaning, assignment, cause_by_id)
        )

        for mutation in (
            {"cross_comparison_exposure": None},
            {
                "cross_comparison_exposure": {
                    **selected["r"]["cross_comparison_exposure"],
                    "representative_cause_evidence_id": "sibling-cause",
                }
            },
            {
                "cross_comparison_exposure": {
                    **selected["r"]["cross_comparison_exposure"],
                    "status": "diagnostic_comparison",
                }
            },
        ):
            with self.subTest(mutation=mutation):
                intermediate = copy.deepcopy(selected)
                intermediate["r"].update(mutation)
                cause_by_id["cause-typed"] = intermediate
                self.assertTrue(
                    _active_fallback_dominator(
                        fallback_meaning, assignment, cause_by_id
                    )
                )

        cause_by_id["cause-typed"] = selected
        for dominance in (
            None,
            {
                "status": "dominated_fallback",
                "retained": False,
                "dominating_cause_evidence_ids": [],
            },
            {
                "status": "dominated_fallback",
                "retained": False,
                "dominating_cause_evidence_ids": ["sibling-cause"],
            },
            {
                "status": "retained",
                "retained": False,
                "dominating_cause_evidence_ids": ["cause-typed"],
            },
        ):
            with self.subTest(dominance=dominance):
                fallback_cause["r"]["fallback_dominance"] = dominance
                self.assertFalse(
                    _active_fallback_dominator(
                        fallback_meaning, assignment, cause_by_id
                    )
                )

        assignment_without_fallback = {"main": "cause-typed"}
        self.assertTrue(
            _active_fallback_dominator(
                fallback_meaning, assignment_without_fallback, cause_by_id
            )
        )
        no_cross = copy.deepcopy(selected)
        no_cross["r"]["cross_comparison_exposure"] = None
        cause_by_id["cause-typed"] = no_cross
        self.assertFalse(
            _active_fallback_dominator(
                fallback_meaning, assignment_without_fallback, cause_by_id
            )
        )

    def test_importance_decision_does_not_union_sibling_dominators(self) -> None:
        selected_channels = {
            "compound": ["compound-material", "compound-threat"],
            "material-only": ["material-only-channel"],
            "threat-only": ["threat-only-channel"],
        }
        profiles = [
            {"cause_evidence_id": cause_id, "causal_signature": signature}
            for cause_id, signature in (
                ("compound", "compound-material"),
                ("compound", "compound-threat"),
                ("material-only", "material-only-channel"),
                ("threat-only", "threat-only-channel"),
            )
        ]
        relations = [
            {
                "left_cause_id": "material-only",
                "left_causal_signature": "material-only-channel",
                "right_cause_id": "compound",
                "right_causal_signature": "compound-material",
                "relation": "dominates",
            },
            {
                "left_cause_id": "threat-only",
                "left_causal_signature": "threat-only-channel",
                "right_cause_id": "compound",
                "right_causal_signature": "compound-threat",
                "relation": "dominates",
            },
        ]

        decisions = _expected_importance_decisions(
            selected_channels, profiles, relations
        )
        compound = next(
            item for item in decisions if item["cause_evidence_id"] == "compound"
        )
        self.assertTrue(compound["fully_measured"])
        self.assertEqual(compound["dominating_cause_ids"], [])
        self.assertFalse(compound["dominated_within_domain"])

    def test_importance_decision_accepts_one_dominator_for_every_channel(self) -> None:
        selected_channels = {
            "weaker": ["weaker-material", "weaker-threat"],
            "stronger": ["stronger-material", "stronger-threat"],
        }
        profiles = [
            {"cause_evidence_id": cause_id, "causal_signature": signature}
            for cause_id, signature in (
                ("weaker", "weaker-material"),
                ("weaker", "weaker-threat"),
                ("stronger", "stronger-material"),
                ("stronger", "stronger-threat"),
            )
        ]
        relations = [
            {
                "left_cause_id": "stronger",
                "left_causal_signature": "stronger-material",
                "right_cause_id": "weaker",
                "right_causal_signature": "weaker-material",
                "relation": "dominates",
            },
            {
                "left_cause_id": "stronger",
                "left_causal_signature": "stronger-threat",
                "right_cause_id": "weaker",
                "right_causal_signature": "weaker-threat",
                "relation": "dominates",
            },
        ]

        decisions = _expected_importance_decisions(
            selected_channels, profiles, relations
        )
        weaker = next(
            item for item in decisions if item["cause_evidence_id"] == "weaker"
        )
        self.assertTrue(weaker["fully_measured"])
        self.assertEqual(weaker["dominating_cause_ids"], ["stronger"])
        self.assertTrue(weaker["dominated_within_domain"])

        with_unmeasured = _expected_importance_decisions(
            {
                **selected_channels,
                "weaker": [
                    "weaker-material",
                    "weaker-threat",
                    "weaker-unmeasured",
                ],
            },
            profiles,
            relations,
        )
        incomplete_weaker = next(
            item
            for item in with_unmeasured
            if item["cause_evidence_id"] == "weaker"
        )
        self.assertFalse(incomplete_weaker["fully_measured"])
        self.assertEqual(incomplete_weaker["dominating_cause_ids"], ["stronger"])
        self.assertFalse(incomplete_weaker["dominated_within_domain"])

    def test_ties_use_typed_effect_relations_not_exposure_rank(self) -> None:
        label = typed_label(
            [
                typed_meaning("main", "wrong_move_order"),
                typed_meaning("peer", "material_swing"),
            ],
            top=["main", "peer"],
            tie_groups=[["main", "peer"]],
        )
        causes = [
            typed_cause(comparison_exposure_rank=7),
            typed_cause(
                "cause-peer", "material_swing", comparison_exposure_rank=0
            ),
        ]
        native = [
            copy.deepcopy(item["r"]["native_selection"])
            for item in causes
        ]
        importance = typed_importance(
            native,
            unique_top=None,
        )
        judgment = self.judge(label, causes, importance=importance)
        self.assertEqual(judgment["status"], "matched")

        dominance_causes = [
            typed_cause(comparison_exposure_rank=7, importance_units=10),
            typed_cause(
                "cause-peer",
                "material_swing",
                comparison_exposure_rank=0,
                importance_units=9,
            ),
        ]
        dominance_native = [
            copy.deepcopy(item["r"]["native_selection"])
            for item in dominance_causes
        ]
        importance = typed_importance(dominance_native)
        judgment = self.judge(label, dominance_causes, importance=importance)
        self.assertEqual(judgment["first_failure_stage"], "R")
        self.assertIn(
            "tie_relation_mismatch",
            [error["code"] for error in judgment["errors"]],
        )

    def test_incomparable_or_unmeasured_importance_never_claims_a_top(self) -> None:
        causes = [
            typed_cause(),
            typed_cause(
                "cause-peer", "material_swing", importance_target="d5"
            ),
        ]
        label = typed_label(
            [
                typed_meaning("main", "wrong_move_order"),
                typed_meaning(
                    "peer",
                    "material_swing",
                    channels=[typed_channel(target="d5")],
                ),
            ]
        )
        native = [
            copy.deepcopy(item["r"]["native_selection"])
            for item in causes
        ]
        incomparable = typed_importance(
            native,
            unique_top=None,
        )
        judgment = self.judge(label, causes, importance=incomparable)
        self.assertEqual(judgment["status"], "unresolved")
        self.assertEqual(judgment["endpoint_status"], "not_scored")
        self.assertEqual(judgment["priority"]["status"], "not_scored")
        self.assertNotIn(
            "importance_inversion",
            [error["code"] for error in judgment["errors"]],
        )

        unmeasured = typed_importance(
            native,
            unique_top=None,
            unmeasured_cause_ids={"cause-typed"},
        )
        judgment = self.judge(label, causes, importance=unmeasured)
        self.assertEqual(judgment["status"], "unresolved")
        self.assertEqual(judgment["priority"]["status"], "not_scored")

    def test_plan_result_importance_uses_the_central_semantic_key(self) -> None:
        native = [
            cause["r"]["native_selection"]
            for cause in (typed_cause(), typed_cause("cause-peer", "wrong_move_order"))
        ]
        profiles = typed_importance(
            native,
            unique_top=None,
        )["r_native"]["profiles"]
        left, right = copy.deepcopy(profiles[0]), copy.deepcopy(profiles[1])
        for profile in (left, right):
            scope = profile["effect_scope"]
            self.assertIsInstance(scope, dict)
            scope.update(
                primitive_kind="plan_result",
                plan_ids=["plan-a"],
                plan_result_semantic_key="exact-plan-result-a",
            )

        relation, domain = _importance_relation(left, right)
        self.assertEqual(relation, "tied")
        self.assertIn("planresult|[square:e5]|[]|[]|exact-plan-result-a", domain)

        different_plan = copy.deepcopy(right)
        different_plan["effect_scope"]["plan_ids"] = ["plan-b"]
        self.assertEqual(_importance_relation(left, different_plan), ("incomparable", None))

        changed = copy.deepcopy(right)
        changed["effect_scope"]["plan_result_semantic_key"] = (
            "exact-plan-result-selected-response-b"
        )
        self.assertEqual(_importance_relation(left, changed), ("incomparable", None))

    def test_typed_importance_overrides_the_opposite_exposure_rank_order(self) -> None:
        causes = [
            typed_cause(comparison_exposure_rank=0, importance_units=9),
            typed_cause(
                "cause-peer",
                "material_swing",
                comparison_exposure_rank=9,
                importance_units=10,
            ),
        ]
        label = typed_label(
            [
                typed_meaning("main", "wrong_move_order"),
                typed_meaning("peer", "material_swing"),
            ]
        )
        native = [
            copy.deepcopy(item["r"]["native_selection"])
            for item in causes
        ]
        importance = typed_importance(native)
        judgment = self.judge(label, causes, importance=importance)
        self.assertEqual(judgment["first_failure_stage"], "R")
        self.assertIn(
            "importance_inversion",
            [error["code"] for error in judgment["errors"]],
        )

    def test_importance_is_copied_exactly_from_r_through_public_projection(self) -> None:
        packet_actual = typed_actual([typed_cause()])
        packet_importance = packet_actual["importance"]
        assert isinstance(packet_importance, dict)
        packet_native = packet_importance["packet_native"]
        assert isinstance(packet_native, dict)
        packet_native["unique_top"] = None
        judgment = judge_typed_case(
            case=typed_case(),
            label=typed_label(),
            actual=packet_actual,
        )
        self.assertEqual(judgment["first_failure_stage"], "P")
        self.assertIn(
            "importance_r_to_packet_mismatch",
            [error["code"] for error in judgment["errors"]],
        )

        actual = typed_actual([typed_cause()])
        importance = actual["importance"]
        assert isinstance(importance, dict)
        public = importance["public_projection"]
        assert isinstance(public, dict)
        public["unique_top"] = "sibling-cause"
        judgment = judge_typed_case(
            case=typed_case(),
            label=typed_label(),
            actual=actual,
        )
        self.assertEqual(judgment["first_failure_stage"], "P")
        self.assertIn(
            "importance_packet_to_public_mismatch",
            [error["code"] for error in judgment["errors"]],
        )

    def test_importance_internal_sets_fail_closed_when_inconsistent(self) -> None:
        actual = typed_actual([typed_cause()])
        importance = actual["importance"]
        assert isinstance(importance, dict)
        native = importance["r_native"]
        assert isinstance(native, dict)
        native["frontier_cause_ids"] = []
        with self.assertRaisesRegex(ContractError, "frontier Cause set"):
            judge_typed_case(
                case=typed_case(),
                label=typed_label(),
                actual=actual,
            )

    def test_importance_relation_is_recomputed_after_measure_reversal(self) -> None:
        actual = typed_actual(
            [
                typed_cause("cause-a", importance_units=10),
                typed_cause("cause-b", "material_swing", importance_units=9),
            ]
        )
        for selection_value in actual["r_native_cause_selections"]:
            cause_id = str(selection_value["cause_evidence_id"])
            selection_value["channels"][0]["effect_descriptor"]["measure"][
                "units"
            ] = (9 if cause_id == "cause-a" else 10)
            selection_value["channels"][0]["importance_effect"]["measure"][
                "units"
            ] = (9 if cause_id == "cause-a" else 10)
        native = actual["importance"]["r_native"]
        for profile in native["profiles"]:
            cause_id = str(profile["cause_evidence_id"])
            profile["measure"]["units"] = 9 if cause_id == "cause-a" else 10
        with self.assertRaisesRegex(ContractError, "relation differs"):
            _importance_state(actual, typed_case())

    def test_material_importance_uses_durable_outcome_not_capture_target_value(self) -> None:
        queen_exchange = material_outcome_cause(
            "queen-exchange",
            "material_swing",
            durable_net_cp=400,
            target_value_cp=900,
        )
        free_rook = material_outcome_cause(
            "free-rook",
            "missed_tactical_resource",
            durable_net_cp=500,
            target_value_cp=500,
        )
        state = _importance_state(
            typed_actual([queen_exchange, free_rook]),
            typed_case(),
        )

        relation = state["relations"][0]
        self.assertEqual(relation["left_cause_id"], "free-rook")
        self.assertEqual(relation["right_cause_id"], "queen-exchange")
        self.assertEqual(relation["relation"], "dominates")
        decisions = {
            decision["cause_evidence_id"]: decision
            for decision in state["decisions"]
        }
        self.assertTrue(decisions["queen-exchange"]["dominated_within_domain"])
        self.assertEqual(
            decisions["queen-exchange"]["dominating_cause_ids"],
            ["free-rook"],
        )
        self.assertEqual(state["native"]["frontier_cause_ids"], ["free-rook"])

    def test_importance_relation_universe_is_complete_and_unique(self) -> None:
        mutations = {
            "missing": lambda rows: rows.pop(),
            "extra": lambda rows: rows.append(
                {
                    "left_cause_id": "cause-a",
                    "left_causal_signature": "owned-channel",
                    "right_cause_id": "cause-a",
                    "right_causal_signature": "forged-channel",
                    "relation": "incomparable",
                    "domain": None,
                }
            ),
            "duplicate": lambda rows: rows.append(copy.deepcopy(rows[0])),
        }
        for name, mutate in mutations.items():
            with self.subTest(name=name):
                actual = typed_actual(
                    [
                        typed_cause("cause-a", importance_units=10),
                        typed_cause(
                            "cause-b", "material_swing", importance_units=9
                        ),
                    ]
                )
                rows = actual["importance"]["r_native"]["relations"]
                mutate(rows)
                with self.assertRaises(ContractError):
                    _importance_state(actual, typed_case())

    def test_one_cause_may_own_multiple_exact_profiles_but_not_duplicate_keys(self) -> None:
        item = typed_cause()
        for selection_value in (
            item["r"]["native_selection"],
            item["p"]["packet_selection"],
            item["p"]["public_selection"],
        ):
            assert isinstance(selection_value, dict)
            second = copy.deepcopy(selection_value["channels"][0])
            second["causal_signature"] = "owned-channel-2"
            second["effect_descriptor"]["effect_scope"]["target_signatures"] = [
                "square:d5"
            ]
            second["importance_effect"]["effect_scope"]["target_signatures"] = [
                "square:d5"
            ]
            selection_value["channels"].append(second)
        actual = typed_actual([item])
        state = _importance_state(actual, typed_case())
        self.assertEqual(len(state["profiles"]), 2)
        self.assertEqual(state["relations"], [])
        self.assertEqual(
            set(state["decisions"][0]["measured_channel_signatures"]),
            {"owned-channel", "owned-channel-2"},
        )

        duplicate = copy.deepcopy(actual)
        duplicate["importance"]["r_native"]["profiles"].append(
            copy.deepcopy(duplicate["importance"]["r_native"]["profiles"][0])
        )
        with self.assertRaisesRegex(ContractError, "duplicate Cause/channel"):
            _importance_state(duplicate, typed_case())

    def test_selection_order_must_be_exact_and_contiguous(self) -> None:
        for name, orders in (("duplicate", [0, 0]), ("noncontiguous", [0, 2])):
            with self.subTest(name=name):
                actual = typed_actual(
                    [
                        typed_cause("cause-a", importance_units=10),
                        typed_cause(
                            "cause-b", "material_swing", importance_units=10
                        ),
                    ]
                )
                for selection_value, order in zip(
                    actual["r_native_cause_selections"], orders, strict=True
                ):
                    selection_value["selection_order"] = order
                with self.assertRaisesRegex(ContractError, "selection_order is not contiguous"):
                    _importance_state(actual, typed_case())

    def test_importance_id_sets_ignore_order_but_reject_membership_drift(self) -> None:
        actual = typed_actual(
            [
                typed_cause("cause-a", importance_units=10),
                typed_cause("cause-b", "material_swing", importance_units=9),
            ]
        )
        unordered = copy.deepcopy(actual)
        unordered["importance"]["r_native"]["selected_cause_ids"].reverse()
        unordered["importance"]["r_public_projection"]["selected_cause_ids"].reverse()
        _importance_state(unordered, typed_case())

        native_drift = copy.deepcopy(actual)
        native_drift["importance"]["r_native"]["selected_cause_ids"][0] = "forged-cause"
        with self.assertRaises(ContractError):
            _importance_state(native_drift, typed_case())

    def test_schema_canonical_importance_id_sets_preserve_explicit_orders(self) -> None:
        actual = typed_actual(
            [
                typed_cause("cause-z", comparison_exposure_rank=0, importance_units=10),
                typed_cause("cause-a", comparison_exposure_rank=1, importance_units=10),
            ]
        )
        registry, canonicalizer = _schema_tools(
            ROOT, TYPED_ACTUAL_VIEW_SCHEMA_VERSION
        )
        schema_path = _schema_path(
            ROOT, "actual-cascade-view", TYPED_ACTUAL_VIEW_SCHEMA_VERSION
        )
        schema = registry.load(schema_path)
        importance_schema = schema["$defs"]["importanceObservation"]
        canonical = copy.deepcopy(actual)
        canonical["importance"] = canonicalizer.canonicalize(
            actual["importance"], importance_schema, schema_path
        )
        canonical["idea_importance"] = canonicalizer.canonicalize(
            actual["idea_importance"], importance_schema, schema_path
        )
        cause_importance = _importance_state(canonical, typed_case())
        _idea_unit_state(canonical, cause_importance)

    def test_explicit_importance_orders_remain_authoritative(self) -> None:
        actual = typed_actual(
            [
                typed_cause("cause-z", comparison_exposure_rank=0, importance_units=10),
                typed_cause("cause-a", comparison_exposure_rank=1, importance_units=10),
            ]
        )
        r_order_drift = copy.deepcopy(actual)
        selection_by_id = {
            str(item["cause_evidence_id"]): item
            for item in r_order_drift["r_native_cause_selections"]
        }
        selection_by_id["cause-z"]["selection_order"] = 1
        selection_by_id["cause-a"]["selection_order"] = 0
        cause_importance = _importance_state(r_order_drift, typed_case())
        with self.assertRaises(ContractError):
            _idea_unit_state(r_order_drift, cause_importance)

        idea_order_drift = copy.deepcopy(actual)
        idea_order_drift["idea_units"]["r_native"][0]["serialization_order"] = 1
        with self.assertRaises(ContractError):
            _idea_unit_state(
                idea_order_drift,
                _importance_state(idea_order_drift, typed_case()),
            )

    def test_three_level_dominance_chain_peels_one_layer_at_a_time(self) -> None:
        causes = [
            typed_cause(
                "cause-a", comparison_exposure_rank=9, importance_units=30
            ),
            typed_cause(
                "cause-b",
                "material_swing",
                comparison_exposure_rank=0,
                importance_units=20,
            ),
            typed_cause(
                "cause-c",
                "plan_contradiction",
                comparison_exposure_rank=0,
                importance_units=10,
            ),
        ]
        actual = typed_actual(causes)
        self.assertEqual(
            [
                (item["cause_evidence_id"], item["selection_order"])
                for item in actual["r_native_cause_selections"]
            ],
            [("cause-a", 0), ("cause-b", 1), ("cause-c", 2)],
        )
        label = typed_label(
            [
                typed_meaning("main", "wrong_move_order"),
                typed_meaning("middle", "material_swing"),
                typed_meaning("last", "plan_contradiction"),
            ]
        )
        judgment = judge_typed_case(case=typed_case(), label=label, actual=actual)
        self.assertEqual(judgment["status"], "matched")

    def test_profile_comparison_and_root_mover_are_exactly_bound(self) -> None:
        actual = typed_actual([typed_cause()])
        actual["importance"]["r_native"]["profiles"][0]["frame"][
            "comparison"
        ] = "forged-comparison"
        with self.assertRaisesRegex(ContractError, "selected channel"):
            _importance_state(actual, typed_case())

        actual = typed_actual([typed_cause()])
        black_case = typed_case()
        black_case["fen"] = (
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"
        )
        black_case["played_move_uci"] = "e7e6"
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            actual["verdict"][copy_name].update(
                mover="black",
                played_move="e7e6",
                reference_move="e7e5",
            )
        actual["verdict"]["classification_authority"].update(
            mover="black",
            played_move="e7e6",
            reference_move="e7e5",
        )
        actual["importance"]["r_native"]["profiles"][0]["universe"][
            "root_board_state"
        ] = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq -"
        with self.assertRaisesRegex(ContractError, "root side to move"):
            _importance_state(actual, black_case)

    def test_profile_domain_cannot_be_self_consistently_retyped(self) -> None:
        def actual_with_profile() -> tuple[dict[str, object], dict[str, object]]:
            actual = typed_actual([typed_cause()])
            profile = actual["importance"]["r_native"]["profiles"][0]
            return actual, profile

        mutations = {
            "mate-domain": lambda actual, profile: profile.update(
                domain_kind="board_mate",
                domain="material",
                measure={"kind": "mate_arrival", "ply_offset": 1},
            ),
            "threat-target": lambda actual, profile: profile.update(
                domain_kind="threat",
                domain="threat:mate:square:d5",
                measure={"kind": "threat_horizon", "turns_to_impact": 1},
            ),
            "structural-grammar": lambda actual, profile: profile.update(
                domain="structural:roottransition:spacegain:gain:robust"
            ),
            "structural-valid-kind-retype": lambda actual, profile: profile.update(
                domain="structural:roottransition:mobilitygain:gain:none"
            ),
            "structural-stake": lambda actual, profile: profile["frame"].update(
                stake="preserves:white"
            ),
        }
        for name, mutate in mutations.items():
            with self.subTest(name=name):
                actual, profile = actual_with_profile()
                mutate(actual, profile)
                descriptor = actual["r_native_cause_selections"][0]["channels"][0][
                    "effect_descriptor"
                ]
                descriptor["measure"] = copy.deepcopy(profile["measure"])
                with self.assertRaisesRegex(ContractError, "exact channel measured effect"):
                    _importance_state(actual, typed_case())

    def test_exact_threat_and_structural_importance_requires_positive_magnitude(self) -> None:
        def threat_actual(turns_to_impact: int) -> dict[str, object]:
            item = typed_cause()
            measure = {
                "kind": "threat_horizon",
                "turns_to_impact": turns_to_impact,
            }
            for selection_value in (
                item["r"]["native_selection"],
                item["p"]["packet_selection"],
                item["p"]["public_selection"],
            ):
                assert isinstance(selection_value, dict)
                channel = selection_value["channels"][0]
                descriptor = channel["effect_descriptor"]
                measured_effect = channel["importance_effect"]
                descriptor["measure"] = copy.deepcopy(measure)
                measured_effect["domain_kind"] = "threat"
                measured_effect["domain"] = "threat:mate:square:e5"
                measured_effect["measure"] = copy.deepcopy(measure)
            return typed_actual([item])

        _importance_state(typed_actual([typed_cause(importance_units=1)]), typed_case())
        _importance_state(threat_actual(1), typed_case())
        for invalid in (0, -1):
            with self.subTest(domain="structural", magnitude=invalid):
                with self.assertRaisesRegex(
                    ContractError, "domain and measure are inconsistent"
                ):
                    _importance_state(
                        typed_actual([typed_cause(importance_units=invalid)]),
                        typed_case(),
                    )
            with self.subTest(domain="threat", magnitude=invalid):
                with self.assertRaisesRegex(
                    ContractError, "domain and measure are inconsistent"
                ):
                    _importance_state(threat_actual(invalid), typed_case())

    def test_played_liability_denied_actor_value_normalizes_to_mover_harm(self) -> None:
        item = typed_cause()
        for selection_value in (
            item["r"]["native_selection"],
            item["p"]["packet_selection"],
            item["p"]["public_selection"],
        ):
            assert isinstance(selection_value, dict)
            selection_value["effect_mode"] = "played_liability"
            selection_value["channels"][0]["direct_change"] = "lost"
            selection_value["channels"][0]["played_change"] = "lost"
        actual = typed_actual([item])
        state = _importance_state(actual, typed_case())
        self.assertEqual(
            state["profiles"][0]["universe"]["impact"],
            "harms-reviewed-mover:white",
        )

    def test_idea_status_distinguishes_meaning_from_engine_availability(self) -> None:
        certified = typed_actual([typed_cause()])
        self.assertEqual(
            certified["public_response"]["idea_status"], "certified"
        )
        _importance_state(certified, typed_case())

        verdict_without_idea = typed_actual([])
        self.assertEqual(
            verdict_without_idea["public_response"]["idea_status"],
            "no_certified_differential_idea",
        )
        no_idea_state = _importance_state(verdict_without_idea, typed_case())
        self.assertEqual(no_idea_state["verdict"], typed_verdict())

        unavailable = typed_actual([], primary_engine_backed=False)
        _importance_state(unavailable, typed_case())

        erased_verdict = typed_actual([])
        erased_verdict["public_response"]["status"] = "withheld"
        erased_verdict["projection"].update(
            status="withheld",
            reason="no_player_facing_reason",
            renderable=False,
            selected_payload_source="empty",
        )
        with self.assertRaisesRegex(ContractError, "verdict was not preserved"):
            _importance_state(erased_verdict, typed_case())

        for actual, forged in (
            (typed_actual([typed_cause()]), "no_certified_differential_idea"),
            (typed_actual([]), "unavailable"),
        ):
            actual["public_response"]["idea_status"] = forged
            with self.assertRaisesRegex(ContractError, "idea_status"):
                _importance_state(actual, typed_case())

    def test_typed_p_rejects_any_raw_public_idea_that_did_not_parse(self) -> None:
        actual = typed_actual([typed_cause()])
        actual["projection"]["raw_public_idea_count"] = 2
        actual["projection"]["parsed_public_idea_count"] = 1
        actual["projection"]["public_ideas_parse_closed"] = False

        judgment = judge_typed_case(
            case=typed_case(), label=typed_label(), actual=actual
        )

        self.assertEqual(judgment["first_failure_stage"], "P")
        self.assertTrue(
            any(
                error["code"] == "public_idea_parse_mismatch"
                and error["stage"] == "P"
                for error in judgment["errors"]
            )
        )

    def test_typed_p_rejects_idea_unit_item_membership_and_priority_drift(self) -> None:
        mutations = (
            (
                "item-role",
                lambda actual: actual["idea_units"]["public_item_links"][0].update(
                    item_role="forged_role"
                ),
                "idea_unit_membership_mismatch",
            ),
            (
                "item-unit-id",
                lambda actual: actual["idea_units"]["public_item_links"][0].update(
                    idea_unit_id="forged-unit"
                ),
                "idea_unit_membership_mismatch",
            ),
            (
                "public-priority",
                lambda actual: actual["idea_units"]["public_projection"][0].update(
                    priority_status="dominated"
                ),
                "idea_unit_packet_to_public_mismatch",
            ),
            (
                "packet-lead",
                lambda actual: actual["idea_units"]["packet_native"][0].update(
                    lead_cause_evidence_id="forged-lead"
                ),
                "idea_unit_r_to_packet_mismatch",
            ),
            (
                "public-idea-importance",
                lambda actual: actual["idea_importance"]["public_projection"].update(
                    unique_top=None
                ),
                "idea_importance_packet_to_public_mismatch",
            ),
        )
        for name, mutate, expected_code in mutations:
            with self.subTest(name=name):
                actual = typed_actual([typed_cause()])
                mutate(actual)
                judgment = judge_typed_case(
                    case=typed_case(), label=typed_label(), actual=actual
                )
                self.assertEqual(judgment["first_failure_stage"], "P")
                self.assertTrue(
                    any(
                        error["code"] == expected_code and error["stage"] == "P"
                        for error in judgment["errors"]
                    )
                )

    def test_exact_pvb_facets_share_one_idea_priority_authority(self) -> None:
        lead = typed_cause("cause-a", "wrong_move_order")
        resource = typed_cause("cause-b", "material_swing")
        actual = typed_actual([lead, resource])
        pair_typed_idea_unit(actual, "cause-a", "cause-b")
        label = typed_label(
            [
                typed_meaning("liability", "wrong_move_order"),
                typed_meaning("resource", "material_swing"),
            ],
            top=["resource"],
        )

        judgment = judge_typed_case(
            case=typed_case(), label=label, actual=actual
        )

        self.assertEqual(judgment["status"], "matched")
        self.assertEqual(judgment["endpoint_status"], "pass")
        self.assertEqual(judgment["priority"]["status"], "matched")
        self.assertEqual(judgment["errors"], [])

    def test_native_idea_authority_rejects_internal_profile_and_unit_drift(self) -> None:
        mutations = (
            (
                "lead-profile",
                lambda actual: actual["idea_importance"]["r_native"]["profiles"][0][
                    "measure"
                ].update(units=99),
            ),
            (
                "lead-decision",
                lambda actual: actual["idea_importance"]["r_native"]["decisions"][
                    0
                ].update(fully_measured=False),
            ),
            (
                "unit-layer",
                lambda actual: actual["idea_units"]["r_native"][0].update(
                    importance_layer=4
                ),
            ),
            (
                "unit-status",
                lambda actual: actual["idea_units"]["r_native"][0].update(
                    priority_status="dominated"
                ),
            ),
        )
        for name, mutate in mutations:
            with self.subTest(name=name):
                actual = typed_actual([typed_cause()])
                mutate(actual)
                with self.assertRaises(ContractError):
                    judge_typed_case(
                        case=typed_case(), label=typed_label(), actual=actual
                    )

    def test_withheld_primary_keeps_secondary_r_and_packet_without_public_p_loss(self) -> None:
        actual = typed_actual(
            [typed_cause()],
            primary_engine_backed=False,
        )

        judgment = judge_typed_case(
            case=typed_case(), label=typed_label(), actual=actual
        )

        self.assertFalse(any(error["stage"] == "P" for error in judgment["errors"]))
        self.assertTrue(actual["causes"][0]["p"]["packet_selected"])
        self.assertIsNone(actual["causes"][0]["p"]["public_selection"])
        self.assertEqual(actual["idea_units"]["public_projection"], [])
        self.assertIsNone(actual["idea_importance"]["public_projection"])

        forged_public = copy.deepcopy(actual)
        forged_public["idea_units"]["public_projection"] = copy.deepcopy(
            forged_public["idea_units"]["packet_native"]
        )
        forged = judge_typed_case(
            case=typed_case(), label=typed_label(), actual=forged_public
        )
        self.assertTrue(
            any(
                error["code"] == "idea_unit_packet_to_public_mismatch"
                and error["stage"] == "P"
                for error in forged["errors"]
            )
        )

    def test_cause_disposition_ledger_closes_c_r_packet_and_public_boundaries(self) -> None:
        mutations = (
            (
                "missing-c-record",
                lambda actual: actual["cause_disposition_ledger"]["r_native"].clear(),
                "cause_disposition_c_coverage_mismatch",
                "R",
            ),
            (
                "wrong-selected-owner",
                lambda actual: actual["cause_disposition_ledger"]["r_native"][0].update(
                    selected_owner_claim_id="forged-claim"
                ),
                "cause_disposition_selection_mismatch",
                "R",
            ),
            (
                "packet-dto",
                lambda actual: actual["cause_disposition_ledger"]["packet_native"][0].update(
                    reason="diagnostic_comparison"
                ),
                "cause_disposition_r_to_packet_mismatch",
                "P",
            ),
            (
                "selected-summary",
                lambda actual: actual["cause_disposition_ledger"][
                    "selected_projection"
                ].update(total_cause_count=8),
                "cause_disposition_packet_to_public_mismatch",
                "P",
            ),
            (
                "top-level-summary",
                lambda actual: actual["public_response"]["idea_status_detail"].update(
                    total_cause_count=8
                ),
                "cause_disposition_packet_to_public_mismatch",
                "P",
            ),
        )
        for name, mutate, expected_code, expected_stage in mutations:
            with self.subTest(name=name):
                actual = typed_actual([typed_cause()])
                mutate(actual)
                judgment = judge_typed_case(
                    case=typed_case(), label=typed_label(), actual=actual
                )
                self.assertTrue(
                    any(
                        error["code"] == expected_code
                        and error["stage"] == expected_stage
                        for error in judgment["errors"]
                    )
                )

    def test_exact_verdict_observation_rejects_null_and_mutated_copies(self) -> None:
        missing = typed_actual([])
        missing["verdict"]["selected_projection"] = None
        with self.assertRaisesRegex(ContractError, "must be non-null"):
            _importance_state(missing, typed_case())

        changed_copy = typed_actual([])
        changed_copy["verdict"]["final_public_response"]["reference_move"] = "g1f3"
        with self.assertRaisesRegex(ContractError, "exactly equal"):
            _importance_state(changed_copy, typed_case())

        for name, field, value, message in (
            (
                "comparison-kind",
                "comparison_kind",
                "played_vs_alternative",
                "not a PlayedVsBest",
            ),
            (
                "played-move",
                "played_move",
                "d2d4",
                "wrong played move",
            ),
            (
                "numeric-loss",
                "win_percent_loss_for_mover",
                9.0,
                "loss is inconsistent",
            ),
        ):
            with self.subTest(name=name):
                actual = typed_actual([])
                for copy_name in (
                    "packet_canonical",
                    "selected_projection",
                    "final_public_response",
                ):
                    actual["verdict"][copy_name][field] = value
                with self.assertRaisesRegex(ContractError, message):
                    _importance_state(actual, typed_case())

        mixed_authority = typed_actual([])
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            mixed_authority["verdict"][copy_name]["cause_evidence_id"] = "forged"
        with self.assertRaisesRegex(ContractError, "non-canonical fields"):
            _importance_state(mixed_authority, typed_case())

        unavailable = typed_actual([], primary_engine_backed=False)
        unavailable["verdict"]["final_public_response"] = typed_verdict()
        with self.assertRaisesRegex(ContractError, "must all be null"):
            _importance_state(unavailable, typed_case())

    def test_exact_verdict_rejects_red_team_board_and_mate_mutations(self) -> None:
        wrong_mover = typed_actual([])
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            wrong_mover["verdict"][copy_name]["mover"] = "black"
        wrong_mover["verdict"]["classification_authority"]["mover"] = "black"
        with self.assertRaisesRegex(ContractError, "root side to move"):
            _importance_state(wrong_mover, typed_case())

        illegal_reference = typed_actual([])
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            illegal_reference["verdict"][copy_name]["reference_move"] = "a1a8"
        illegal_reference["verdict"]["classification_authority"][
            "reference_move"
        ] = "a1a8"
        with self.assertRaisesRegex(ContractError, "reference move is illegal"):
            _importance_state(illegal_reference, typed_case())

        zero_mate = typed_actual([])
        zero_mate_value = {
            "reference_for_mover": 0,
            "played_for_mover": 0,
            "distance_loss": None,
        }
        zero_outcome = {
            "state": "unknown",
            "certainty": "engine_mate_score",
            "reference_state": "unknown",
            "resistance": None,
        }
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            zero_mate["verdict"][copy_name]["mate"] = copy.deepcopy(zero_mate_value)
            zero_mate["verdict"][copy_name]["outcome"] = copy.deepcopy(zero_outcome)
        zero_mate["verdict"]["classification_authority"].update(
            reference_mate_for_mover=0,
            played_mate_for_mover=0,
        )
        with self.assertRaisesRegex(ContractError, "must be non-zero"):
            _importance_state(zero_mate, typed_case())

    def test_exact_verdict_uses_central_mate_classification_authority(self) -> None:
        forced_reversal = typed_actual([])
        public_reversal = {
            **typed_verdict(),
            "verdict_code": "inaccuracy",
            "move_quality": "bad",
            "candidate_win_percent_delta_for_mover": -100.0,
            "win_percent_loss_for_mover": 100.0,
            "mate": {
                "reference_for_mover": 3,
                "played_for_mover": -3,
                "distance_loss": None,
            },
            "outcome": {
                "state": "forced_loss",
                "certainty": "engine_mate_score",
                "reference_state": "forced_win",
                "resistance": None,
            },
        }
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            forced_reversal["verdict"][copy_name] = copy.deepcopy(public_reversal)
        forced_reversal["verdict"]["classification_authority"] = {
            **typed_verdict_authority(public_reversal),
            "expected_verdict_code": "blunder",
            "expected_move_quality": "bad",
        }
        with self.assertRaisesRegex(ContractError, "central classification authority"):
            _importance_state(forced_reversal, typed_case())

        shortened_loss = typed_actual([])
        public_shortened = {
            **typed_verdict(),
            "verdict_code": "matches_reference",
            "move_quality": "good",
            "candidate_win_percent_delta_for_mover": 0.0,
            "win_percent_loss_for_mover": 0.0,
            "mate": {
                "reference_for_mover": -5,
                "played_for_mover": -3,
                "distance_loss": 2,
            },
            "outcome": {
                "state": "forced_loss",
                "certainty": "engine_mate_score",
                "reference_state": "forced_loss",
                "resistance": "shortened",
            },
        }
        for copy_name in (
            "packet_canonical",
            "selected_projection",
            "final_public_response",
        ):
            shortened_loss["verdict"][copy_name] = copy.deepcopy(public_shortened)
        shortened_loss["verdict"]["classification_authority"] = {
            **typed_verdict_authority(public_shortened),
            "expected_verdict_code": "playable_loss",
            "expected_move_quality": "playable",
        }
        with self.assertRaisesRegex(ContractError, "central classification authority"):
            _importance_state(shortened_loss, typed_case())

    def test_v3_schema_preserves_v2_transport_and_adds_typed_binding(self) -> None:
        v2_registry, _ = _schema_tools(ROOT, ACTUAL_VIEW_SCHEMA_VERSION)
        v3_registry, _ = _schema_tools(ROOT, TYPED_ACTUAL_VIEW_SCHEMA_VERSION)
        v2_path = _schema_path(
            ROOT, "actual-cascade-view", ACTUAL_VIEW_SCHEMA_VERSION
        )
        v3_path = _schema_path(
            ROOT, "actual-cascade-view", TYPED_ACTUAL_VIEW_SCHEMA_VERSION
        )
        v2 = v2_registry.load(v2_path)
        v3 = v3_registry.load(v3_path)

        self.assertEqual(
            v2["properties"]["schema_version"]["const"],
            ACTUAL_VIEW_SCHEMA_VERSION,
        )
        for field in (
            "comparison_endpoint_evidence_snapshots",
            "r_native_cause_selections",
            "importance",
            "idea_units",
            "idea_importance",
            "cause_disposition_ledger",
            "verdict",
            "request_sha256",
            "hash_contract",
        ):
            self.assertNotIn(field, v2["required"])
            self.assertIn(field, v3["required"])

        v2_transport = v2["properties"]["probe_closure"]["properties"][
            "runtime_transport"
        ]["items"]
        v3_transport = v3["properties"]["probe_closure"]["properties"][
            "runtime_transport"
        ]["items"]
        for field in ("request_sha256", "adapter_request_sha256", "hash_contract"):
            self.assertNotIn(field, v2_transport["required"])
            self.assertIn(field, v3_transport["required"])

        runtime_reasons = {
            "better_alternative_exposes_exact_played_liability",
            "conflicting_root_owned_effect_truth",
        }
        for version, schema in ((2, v2), (3, v3)):
            exposure = schema["$defs"]["causeCascade"]["properties"]["r"][
                "properties"
            ]["cross_comparison_exposure"]
            reason_values = next(
                branch["properties"]["reason"]["enum"]
                for branch in exposure["oneOf"]
                if branch.get("type") == "object"
            )
            self.assertTrue(runtime_reasons.issubset(reason_values), version)
            if version == 2:
                self.assertIn("more_specific_equivalent_cause", reason_values)


    def test_typed_oracle_loader_rejects_cross_partition_rows(self) -> None:
        label = typed_label()
        label["partition"] = "sealed_confirm"
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "oracle.jsonl"
            path.write_text(json.dumps(label) + "\n", encoding="utf-8")
            with self.assertRaises(IntegrityError):
                _load_typed_labels(
                    root=ROOT,
                    path=path,
                    partition_cases=[typed_case()],
                    partition="explore",
                )


    def test_typed_runtime_loader_rejects_base_oracle_binding_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            case = fixture["case"]
            labels = fixture["base_labels"]
            runtime_path = Path(directory) / "runtime.json"
            runtime = {
                "schema_version": "chesstory.eval.cause-audit-runtime-run.v1",
                "run_id": "runtime-arm-1",
                "partition": "explore",
                "freeze_manifest_sha256": sha256_file(paths["freeze.json"]),
                "cases_file_sha256": sha256_file(paths["cases.jsonl"]),
                "base_oracle": _typed_base_oracle_binding(
                    base_oracle_path=paths["base.jsonl"],
                    partition="explore",
                    labels=labels,
                ),
                "adapter": {
                    "main_class": "io.chesstory.evaluation.runtimeadapter.CauseAuditAdapterCli",
                    "sbt_sha256": "4" * 64,
                    "runtime_observation_schema": "chesstory.cause-audit-runtime-observation.v3",
                },
                "case_count": 1,
                "views": [typed_actual([typed_cause()])],
            }
            runtime["base_oracle"]["labels_semantic_sha256"] = "0" * 64
            runtime_path.write_text(json.dumps(runtime), encoding="ascii")
            with self.assertRaisesRegex(IntegrityError, "base-oracle binding"):
                _load_typed_runtime_run(
                    root=ROOT,
                    runtime_run_path=runtime_path,
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    base_oracle_path=paths["base.jsonl"],
                    base_labels=labels,
                    partition="explore",
                    partition_cases=[case],
                    require_full_partition=True,
                )

    def test_open_world_acceptance_is_an_exact_frozen_oracle_merge(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            _verify_open_world_candidate_coverage(
                candidate_set=verification["candidate_set"],
                base_labels=verification["base_labels"],
                views={"case-1": typed_actual([fixture["cause"]])},
            )
            manifest = verification["manifest"]
            self.assertEqual(manifest["open_world"]["accepted_count"], 1)
            self.assertEqual(manifest["open_world"]["merge_scope"], "all_arms")

    def test_accepted_public_candidate_requires_a_complete_final_case_policy(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            adjudication = json.loads(
                paths["adjudication.json"].read_text(encoding="ascii")
            )
            adjudication["case_judgments"] = []
            paths["adjudication.json"].write_text(
                json.dumps(adjudication), encoding="ascii"
            )
            manifest = json.loads(paths["manifest.json"].read_text(encoding="ascii"))
            manifest["open_world"]["adjudication_sha256"] = sha256_file(
                paths["adjudication.json"]
            )
            manifest["open_world"]["case_judgment_count"] = 0
            manifest["open_world"]["case_judgments_sha256"] = sha256_json([])
            paths["manifest.json"].write_text(
                json.dumps(manifest), encoding="ascii"
            )
            with self.assertRaisesRegex(
                ContractError, "exactly one final case judgment"
            ):
                _verify_typed_oracle_run_manifest(
                    root=ROOT,
                    path=paths["manifest.json"],
                    base_oracle_path=paths["base.jsonl"],
                    labels_path=paths["run.jsonl"],
                    arm_inventory_path=paths["arm-inventory.json"],
                    candidate_set_path=paths["candidates.json"],
                    adjudication_path=paths["adjudication.json"],
                    labels=fixture["run_labels"],
                    partition_cases=[fixture["case"]],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    partition="explore",
                )

    def test_accepted_top_must_be_required_primary_and_non_top_must_be_allowed(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory, public_top=False)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            meaning = verification["adjudication"]["decisions"][0][
                "verified_meaning"
            ]
            self.assertEqual(meaning["generation_policy"], "allowed")
            self.assertEqual(meaning["exposure_policy"], "primary")
            self.assertEqual(
                fixture["run_labels"][0]["priority"]["required_top_one_of"],
                ["main"],
            )

        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            adjudication = json.loads(
                paths["adjudication.json"].read_text(encoding="ascii")
            )
            verified = adjudication["decisions"][0]["verified_meaning"]
            verified["generation_policy"] = "allowed"
            adjudication["decisions"][0]["verified_meaning_sha256"] = sha256_json(
                verified
            )
            paths["adjudication.json"].write_text(
                json.dumps(adjudication), encoding="ascii"
            )
            manifest = json.loads(paths["manifest.json"].read_text(encoding="ascii"))
            manifest["open_world"]["adjudication_sha256"] = sha256_file(
                paths["adjudication.json"]
            )
            paths["manifest.json"].write_text(
                json.dumps(manifest), encoding="ascii"
            )
            with self.assertRaisesRegex(
                ContractError, "top candidate must be a required primary"
            ):
                _verify_typed_oracle_run_manifest(
                    root=ROOT,
                    path=paths["manifest.json"],
                    base_oracle_path=paths["base.jsonl"],
                    labels_path=paths["run.jsonl"],
                    arm_inventory_path=paths["arm-inventory.json"],
                    candidate_set_path=paths["candidates.json"],
                    adjudication_path=paths["adjudication.json"],
                    labels=fixture["run_labels"],
                    partition_cases=[fixture["case"]],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    partition="explore",
                )

    def test_open_world_rejection_remains_an_unmapped_c_error(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory, decision="rejected", native=False
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            actual = typed_actual([fixture["cause"]])
            _verify_open_world_candidate_coverage(
                candidate_set=verification["candidate_set"],
                base_labels=verification["base_labels"],
                views={"case-1": actual},
            )
            judgment = judge_typed_case(
                case=fixture["case"],
                label=fixture["run_labels"][0],
                actual=actual,
            )
            self.assertIn(
                "unmapped_generated_cause",
                {item["code"] for item in judgment["errors"]},
            )
            self.assertEqual(judgment["first_failure_stage"], "C")

    def test_zero_owned_channel_c_is_inventoried_but_rejected_only(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory,
                decision="rejected",
                native=False,
                owned_channels=False,
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            candidate = fixture["candidate"]
            assert isinstance(candidate, dict)
            payload = candidate["payload"]
            assert isinstance(payload, dict)
            c_semantics = payload["c_semantics"]
            assert isinstance(c_semantics, dict)
            self.assertEqual(c_semantics["channels"], [])
            inventory_payload = fixture["arm_inventory"]["arms"][0][
                "actual_cascades"
            ][0]["unmatched_cause_observations"][0]["payload"]
            self.assertEqual(
                inventory_payload["c_semantics"]["channels"], []
            )
            source_blind_bytes = json.dumps(candidate, sort_keys=True)
            self.assertNotIn("raw_owned_bindings", source_blind_bytes)
            self.assertNotIn("owned-channel", source_blind_bytes)

            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            actual = typed_actual([fixture["cause"]])
            _verify_open_world_candidate_coverage(
                candidate_set=verification["candidate_set"],
                base_labels=verification["base_labels"],
                views={"case-1": actual},
            )
            judgment = judge_typed_case(
                case=fixture["case"],
                label=fixture["run_labels"][0],
                actual=actual,
            )
            self.assertIn(
                "unmapped_generated_cause",
                {item["code"] for item in judgment["errors"]},
            )
            self.assertEqual(judgment["first_failure_stage"], "C")

    def test_zero_owned_channel_c_cannot_be_accepted(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory,
                native=False,
                owned_channels=False,
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            with self.assertRaisesRegex(
                ContractError,
                "does not exactly verify its candidate",
            ):
                _verify_typed_oracle_run_manifest(
                    root=ROOT,
                    path=paths["manifest.json"],
                    base_oracle_path=paths["base.jsonl"],
                    labels_path=paths["run.jsonl"],
                    arm_inventory_path=paths["arm-inventory.json"],
                    candidate_set_path=paths["candidates.json"],
                    adjudication_path=paths["adjudication.json"],
                    labels=fixture["run_labels"],
                    partition_cases=[fixture["case"]],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    partition="explore",
                )

    def test_open_world_missing_decision_and_missing_candidate_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory, decision="rejected", include_decision=False
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            with self.assertRaisesRegex(ContractError, "missing or extra"):
                _verify_typed_oracle_run_manifest(
                    root=ROOT,
                    path=paths["manifest.json"],
                    base_oracle_path=paths["base.jsonl"],
                    labels_path=paths["run.jsonl"],
                    arm_inventory_path=paths["arm-inventory.json"],
                    candidate_set_path=paths["candidates.json"],
                    adjudication_path=paths["adjudication.json"],
                    labels=fixture["run_labels"],
                    partition_cases=[fixture["case"]],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    partition="explore",
                )
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory,
                decision="rejected",
                include_candidate=False,
                include_decision=False,
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            with self.assertRaisesRegex(ContractError, "no source-blind"):
                _verify_open_world_candidate_coverage(
                    candidate_set=verification["candidate_set"],
                    base_labels=verification["base_labels"],
                    views={"case-1": typed_actual([fixture["cause"]])},
                )

    def test_open_world_c_only_candidate_has_no_borrowed_selection_semantics(self) -> None:
        cause_value = typed_cause(
            "cause-diagnostic", "tempo_loss", native=False, packet=False, public=False
        )
        snapshot_index = typed_snapshot_index([cause_value])
        payload = canonical_open_world_cause_candidate(cause_value, snapshot_index)
        observation = canonical_open_world_cause_observation(
            cause_value, snapshot_index
        )
        self.assertEqual(set(payload), {"c_semantics"})
        self.assertEqual(observation["selection_variants"], [])
        self.assertIn("c_generated", observation["observed_stages"])
        self.assertNotIn("r_selected", observation["observed_stages"])
        source_blind_bytes = json.dumps(payload, sort_keys=True)
        self.assertNotIn("cause-diagnostic", source_blind_bytes)
        self.assertNotIn("owned-channel", source_blind_bytes)
        self.assertNotIn("causal_signature", source_blind_bytes)
        self.assertNotIn("observed_stages", source_blind_bytes)
        self.assertNotIn("selection_variants", source_blind_bytes)
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory, native=False)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            accepted = verification["adjudication"]["decisions"][0]
            self.assertEqual(
                accepted["verified_meaning"]["exposure_policy"],
                "diagnostic_only",
            )

    def test_open_world_c_identity_excludes_runtime_attribution_booleans(self) -> None:
        original = typed_cause(
            "cause-diagnostic", "tempo_loss", native=False, packet=False, public=False
        )
        forged_runtime_flags = copy.deepcopy(original)
        forged_runtime_flags["c"]["attribution"]["root_move_matched"] = False
        forged_runtime_flags["c"]["attribution"]["direct_proof_eligible"] = False
        snapshot_index = typed_snapshot_index([original])
        self.assertEqual(
            canonical_open_world_cause_candidate(original, snapshot_index),
            canonical_open_world_cause_candidate(
                forged_runtime_flags, snapshot_index
            ),
        )
        self.assertEqual(
            sha256_json(
                canonical_open_world_cause_candidate(original, snapshot_index)[
                    "c_semantics"
                ]
            ),
            sha256_json(
                canonical_open_world_cause_candidate(
                    forged_runtime_flags, snapshot_index
                )["c_semantics"]
            ),
        )

    def test_open_world_inventory_rejects_r_channel_borrowing(self) -> None:
        borrowed = typed_cause("cause-borrowed", "tempo_loss")
        borrowed["r"]["native_selection"]["channels"][0]["carrier"]["id"] = (
            "sibling-source"
        )
        with self.assertRaisesRegex(ContractError, "not bound"):
            canonical_open_world_cause_observation(
                borrowed, typed_snapshot_index([borrowed])
            )

    def test_source_blind_artifacts_exclude_arm_and_runtime_behavior(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            candidates = paths["candidates.json"].read_text(encoding="ascii")
            adjudication = paths["adjudication.json"].read_text(encoding="ascii")
            inventory = paths["arm-inventory.json"].read_text(encoding="ascii")
            source_blind = candidates + adjudication
            for private_value in (
                "runtime-arm-1",
                "observed_stages",
                "selection_variants",
                "r_selected",
                "packet_selected",
                "public_selected",
            ):
                self.assertNotIn(private_value, source_blind)
            self.assertIn("runtime-arm-1", inventory)
            self.assertIn("observed_stages", inventory)
            self.assertIn("selection_variants", inventory)
            candidate_document = json.loads(candidates)
            self.assertEqual(
                set(candidate_document["candidates"][0]["payload"]),
                {"c_semantics"},
            )

    def test_all_arm_inventory_unions_c_identity_not_selection_behavior(self) -> None:
        primary = typed_cause("cause-primary", "tempo_loss", exposure="primary")
        complementary = typed_cause(
            "cause-complementary", "tempo_loss", exposure="complementary"
        )

        def observation_row(value: dict[str, object]) -> dict[str, object]:
            payload = canonical_open_world_cause_observation(
                value, typed_snapshot_index([value])
            )
            return {
                "c_semantics_sha256": sha256_json(payload["c_semantics"]),
                "payload_sha256": sha256_json(payload),
                "payload": payload,
            }

        inventory = {
            "arms": [
                {
                    "actual_cascades": [
                        {
                            "case_id": "case-1",
                            "unmatched_cause_observations": [
                                observation_row(primary)
                            ],
                        }
                    ]
                },
                {
                    "actual_cascades": [
                        {
                            "case_id": "case-1",
                            "unmatched_cause_observations": [
                                observation_row(complementary)
                            ],
                        }
                    ]
                },
            ]
        }
        candidates = _inventory_expected_candidates(inventory)
        self.assertEqual(len(candidates), 1)
        self.assertEqual(
            candidates[0]["payload"],
            canonical_open_world_cause_candidate(
                primary, typed_snapshot_index([primary])
            ),
        )
        self.assertNotIn("selection_variants", candidates[0]["payload"])

    def test_each_frozen_arm_requires_complete_nonduplicated_case_coverage(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            inventory = copy.deepcopy(fixture["arm_inventory"])
            second_label = copy.deepcopy(fixture["base_labels"][0])
            second_label["case_id"] = "case-2"
            labels = [fixture["base_labels"][0], second_label]
            inventory["case_ids_sha256"] = sha256_json(
                ["case-1", "case-2"]
            )
            paths["arm-inventory.json"].write_text(
                json.dumps(inventory), encoding="ascii"
            )
            with self.assertRaisesRegex(
                IntegrityError, "exact frozen case set"
            ):
                _load_open_world_arm_inventory(
                    root=ROOT,
                    path=paths["arm-inventory.json"],
                    base_oracle_path=paths["base.jsonl"],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    labels=labels,
                    partition="explore",
                )

        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(directory)
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            inventory = copy.deepcopy(fixture["arm_inventory"])
            arm = inventory["arms"][0]
            duplicate = copy.deepcopy(arm["actual_cascades"][0])
            duplicate["actual_cascade_sha256"] = "7" * 64
            arm["actual_cascades"].append(duplicate)
            arm["actual_cascades_sha256"] = sha256_json(
                arm["actual_cascades"]
            )
            paths["arm-inventory.json"].write_text(
                json.dumps(inventory), encoding="ascii"
            )
            with self.assertRaisesRegex(ContractError, "repeats a case"):
                _load_open_world_arm_inventory(
                    root=ROOT,
                    path=paths["arm-inventory.json"],
                    base_oracle_path=paths["base.jsonl"],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    labels=fixture["base_labels"],
                    partition="explore",
                )

    def test_c_only_accepted_public_meaning_fails_at_r_not_c(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory,
                native=False,
                adjudicated_exposure="primary",
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            verification = _verify_typed_oracle_run_manifest(
                root=ROOT,
                path=paths["manifest.json"],
                base_oracle_path=paths["base.jsonl"],
                labels_path=paths["run.jsonl"],
                arm_inventory_path=paths["arm-inventory.json"],
                candidate_set_path=paths["candidates.json"],
                adjudication_path=paths["adjudication.json"],
                labels=fixture["run_labels"],
                partition_cases=[fixture["case"]],
                manifest_path=paths["freeze.json"],
                cases_path=paths["cases.jsonl"],
                partition="explore",
            )
            accepted = verification["adjudication"]["decisions"][0]
            self.assertEqual(
                accepted["verified_meaning"]["generation_policy"], "required"
            )
            self.assertEqual(
                accepted["verified_meaning"]["exposure_policy"], "primary"
            )
            actual = typed_actual([typed_cause(), fixture["cause"]])
            judgment = judge_typed_case(
                case=fixture["case"],
                label=fixture["run_labels"][0],
                actual=actual,
            )
            self.assertNotIn(
                "unmapped_generated_cause",
                {item["code"] for item in judgment["errors"]},
            )
            self.assertEqual(judgment["first_failure_stage"], "R")

    def test_mixed_arm_selection_behavior_does_not_define_candidate_truth(self) -> None:
        primary = typed_cause("cause-primary", "tempo_loss", exposure="primary")
        complementary = typed_cause(
            "cause-complementary", "tempo_loss", exposure="complementary"
        )
        merged_observation = _merge_candidate_payloads(
            [
                canonical_open_world_cause_observation(
                    primary, typed_snapshot_index([primary])
                ),
                canonical_open_world_cause_observation(
                    complementary, typed_snapshot_index([complementary])
                ),
            ]
        )
        self.assertEqual(len(merged_observation["selection_variants"]), 2)
        candidate = canonical_open_world_cause_candidate(
            primary, typed_snapshot_index([primary])
        )
        verified = typed_meaning(
            "open-world-tempo",
            "tempo_loss",
            generation="required",
            exposure="primary",
        )
        self.assertTrue(open_world_meaning_matches_candidate(verified, candidate))
        self.assertNotIn("selection_variants", candidate)

    def test_best_vs_second_reference_resource_is_played_value(self) -> None:
        cause_value = typed_cause("cause-best-vs-second", "tempo_loss")
        cause_value["c"]["comparison"]["kind"] = "best_vs_second"
        cause_value["c"]["binding"]["role"] = "candidate_set_constraint"
        candidate = canonical_open_world_cause_candidate(
            cause_value, typed_snapshot_index([cause_value])
        )
        verified = typed_meaning(
            "best-keeps-resource",
            "tempo_loss",
            generation="required",
            exposure="primary",
        )
        realization = verified["realizations"][0]
        realization["comparison"]["kind"] = "best_vs_second"
        realization["effect_mode"] = "played_value"
        realization["channels"][0]["played_change"] = "occurred"
        self.assertTrue(open_world_meaning_matches_candidate(verified, candidate))

        wrong_polarity = copy.deepcopy(verified)
        wrong_polarity["realizations"][0]["effect_mode"] = (
            "alternative_resource"
        )
        wrong_polarity["realizations"][0]["channels"][0][
            "played_change"
        ] = "missed"
        self.assertFalse(
            open_world_meaning_matches_candidate(wrong_polarity, candidate)
        )

    def test_direct_reference_vs_alternative_c_is_diagnostic_only(self) -> None:
        cause_value = typed_cause(
            "cause-reference-vs-alternative", "tempo_loss", native=False
        )
        cause_value["c"]["comparison"]["kind"] = "reference_vs_alternative"
        cause_value["c"]["binding"]["role"] = "alternative_diagnostic"
        candidate = canonical_open_world_cause_candidate(
            cause_value, typed_snapshot_index([cause_value])
        )

        verified = typed_meaning(
            "diagnostic-reference-vs-alternative",
            "tempo_loss",
            generation="allowed",
            exposure="diagnostic_only",
        )
        realization = verified["realizations"][0]
        realization["comparison"]["kind"] = "reference_vs_alternative"
        # Structurally required fields cannot acquire public authority for a
        # comparison that has no played-side orientation.
        realization["effect_mode"] = "played_liability"
        realization["channels"][0]["played_change"] = "lost"
        self.assertTrue(
            open_world_meaning_matches_candidate(verified, candidate)
        )

        public_attempt = copy.deepcopy(verified)
        public_attempt["exposure_policy"] = "primary"
        self.assertFalse(
            open_world_meaning_matches_candidate(public_attempt, candidate)
        )

    def test_open_world_pending_c_only_candidate_blocks_scoring(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = write_open_world_contract(
                directory,
                decision="rejected",
                include_decision=False,
                native=False,
            )
            paths = fixture["paths"]
            assert isinstance(paths, dict)
            manifest = json.loads(paths["manifest.json"].read_text(encoding="ascii"))
            manifest["open_world"]["status"] = "pending"
            manifest["open_world"]["pending_count"] = 1
            paths["manifest.json"].write_text(json.dumps(manifest), encoding="ascii")
            with self.assertRaisesRegex(ContractError, "no pending"):
                _verify_typed_oracle_run_manifest(
                    root=ROOT,
                    path=paths["manifest.json"],
                    base_oracle_path=paths["base.jsonl"],
                    labels_path=paths["run.jsonl"],
                    arm_inventory_path=paths["arm-inventory.json"],
                    candidate_set_path=paths["candidates.json"],
                    adjudication_path=paths["adjudication.json"],
                    labels=fixture["run_labels"],
                    partition_cases=[fixture["case"]],
                    manifest_path=paths["freeze.json"],
                    cases_path=paths["cases.jsonl"],
                    partition="explore",
                )

    def test_open_world_acceptance_cannot_smuggle_an_extra_realization(self) -> None:
        cause_value = typed_cause("cause-open-world", "tempo_loss")
        payload = canonical_open_world_cause_candidate(
            cause_value, typed_snapshot_index([cause_value])
        )
        verified = typed_meaning(
            "open-world-tempo", "tempo_loss", generation="allowed"
        )
        extra = copy.deepcopy(verified["realizations"][0])
        extra["cause_kind"] = "activity_gain"
        verified["realizations"].append(extra)
        self.assertFalse(open_world_meaning_matches_candidate(verified, payload))

    def test_typed_sealed_comparison_refuses_before_opening_label_or_store(self) -> None:
        missing = ROOT / "does-not-exist.json"
        with self.assertRaisesRegex(ContractError, "redacted projection"):
            compare_cause_audit(
                root=ROOT,
                store=None,  # type: ignore[arg-type]
                manifest_path=missing,
                cases_path=missing,
                labels_path=missing,
                runtime_run_path=missing,
                partition="sealed_confirm",
                requested_case_ids=[],
                oracle_run_manifest_path=missing,
                base_oracle_path=missing,
                candidate_set_path=missing,
                adjudication_path=missing,
            )


class CauseAuditCacheContractTest(unittest.TestCase):
    def test_concurrent_conflicting_writes_preserve_one_canonical_document(self) -> None:
        documents = (
            {"cache_key_sha256": "a" * 64, "writer": "left"},
            {"cache_key_sha256": "a" * 64, "writer": "right"},
        )
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "entry.json"
            barrier = threading.Barrier(len(documents))

            def write(document: dict[str, str]) -> IntegrityError | None:
                barrier.wait()
                try:
                    _atomic_cache_write(path, document)
                except IntegrityError as error:
                    return error
                return None

            with ThreadPoolExecutor(max_workers=len(documents)) as executor:
                outcomes = tuple(executor.map(write, documents))

            self.assertEqual(outcomes.count(None), 1)
            self.assertIsInstance(next(error for error in outcomes if error), IntegrityError)
            self.assertIn(path.read_bytes(), {json_bytes(document) for document in documents})
            self.assertEqual(list(path.parent.iterdir()), [path])


if __name__ == "__main__":
    unittest.main()
