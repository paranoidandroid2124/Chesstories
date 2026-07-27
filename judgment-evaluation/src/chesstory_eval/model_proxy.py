from __future__ import annotations

import json
import math
import re
from collections.abc import Mapping, Sequence
from typing import Any

from .hashing import sha256_json
from .model import ContractError, IntegrityError


MODEL_PROXY_TIER = "model-proxy-nonhuman"
MODEL_PROXY_ROLES = (
    "grounding-auditor",
    "chess-meaning-rater",
    "usefulness-rater",
)
_HEX_64 = re.compile(r"^[0-9a-f]{64}$")
_FORBIDDEN_BLIND_KEYS = frozenset(
    {
        "allowed_core_claim_ids",
        "arm",
        "arm_id",
        "atomic_cluster_id",
        "candidate_manifest_sha256",
        "document_id",
        "expected_answer",
        "labels",
        "oracle_chain",
        "pdf_page",
        "provider",
        "run_id",
        "sample_id",
        "source",
        "source_locator",
        "split",
    }
)
_VOTE_KEYS = frozenset(
    {
        "schema_version",
        "evidence_tier",
        "human_gate_eligible",
        "blind_item_id",
        "blind_payload_sha256",
        "view_sha256",
        "role",
        "rater_id",
        "abstained",
        "grounded",
        "fact_correct",
        "unsupported_meaning_count",
        "core_idea_score",
        "usefulness_score",
        "reason_codes",
        "notes",
    }
)


def build_blind_model_proxy_view(
    *,
    blind_item_id: str,
    role: str,
    evaluation_nonce: str,
    candidate_output: Mapping[str, Any],
    stockfish_evidence: Mapping[str, Any],
) -> dict[str, Any]:
    """Build the only payload a non-human blind proxy rater may observe.

    Candidate, source, corpus, arm, oracle-chain, and answer-key identities are
    excluded.  The model receives final candidate meaning plus a Stockfish and
    legal-replay verification pack.  This evidence tier is permanently
    ineligible for the held-out-human release gate.
    """

    _nonempty_text(blind_item_id, "blind item ID")
    _nonempty_text(evaluation_nonce, "evaluation nonce")
    if role not in MODEL_PROXY_ROLES:
        raise ContractError(f"unknown model-proxy role {role!r}")
    candidate = _finite_json_object(candidate_output, "candidate output")
    evidence = _finite_json_object(stockfish_evidence, "Stockfish evidence")
    _reject_provenance_keys(candidate, location="candidate output")
    _reject_provenance_keys(evidence, location="Stockfish evidence")
    _validate_candidate_output(candidate)
    _validate_stockfish_evidence(evidence)

    blind_payload = {
        "schema_version": "chesstory.eval.blind-model-proxy-payload.v1",
        "evidence_tier": MODEL_PROXY_TIER,
        "human_gate_eligible": False,
        "blind_item_id": blind_item_id,
        "evaluation_nonce": evaluation_nonce,
        "candidate_output": candidate,
        "stockfish_evidence": evidence,
    }
    return {
        "schema_version": "chesstory.eval.blind-model-proxy-view.v1",
        "evidence_tier": MODEL_PROXY_TIER,
        "human_gate_eligible": False,
        "blind_item_id": blind_item_id,
        "blind_payload_sha256": sha256_json(blind_payload),
        "role": role,
        "rubric": _role_rubric(role),
        "payload": blind_payload,
    }


def validate_blind_model_proxy_vote(
    *, view: Mapping[str, Any], vote: Mapping[str, Any]
) -> dict[str, Any]:
    normalized_view = _validate_view(view)
    row = _finite_json_object(vote, "model-proxy vote")
    if set(row) != _VOTE_KEYS:
        raise ContractError("model-proxy vote has missing or unknown fields")
    if row.get("schema_version") != "chesstory.eval.blind-model-proxy-vote.v1":
        raise ContractError("unsupported model-proxy vote schema")
    if row.get("evidence_tier") != MODEL_PROXY_TIER:
        raise ContractError("model-proxy vote has the wrong evidence tier")
    if row.get("human_gate_eligible") is not False:
        raise ContractError("model-proxy vote must be human-gate-ineligible")
    for key in ("blind_item_id", "blind_payload_sha256", "role"):
        if row.get(key) != normalized_view[key]:
            raise IntegrityError(f"model-proxy vote {key} echo mismatch")
    if row.get("view_sha256") != sha256_json(normalized_view):
        raise IntegrityError("model-proxy vote view hash echo mismatch")
    _nonempty_text(row.get("rater_id"), "model-proxy rater ID")
    for key in ("abstained", "grounded", "fact_correct"):
        if not isinstance(row.get(key), bool):
            raise ContractError(f"model-proxy vote {key} must be boolean")
    unsupported = row.get("unsupported_meaning_count")
    if isinstance(unsupported, bool) or not isinstance(unsupported, int) or unsupported < 0:
        raise ContractError("unsupported_meaning_count must be a nonnegative integer")
    for key in ("core_idea_score", "usefulness_score"):
        score = row.get(key)
        if (
            isinstance(score, bool)
            or not isinstance(score, (int, float))
            or not math.isfinite(float(score))
            or not 0.0 <= float(score) <= 1.0
        ):
            raise ContractError(f"model-proxy vote {key} must lie in [0, 1]")
        row[key] = float(score)
    reason_codes = row.get("reason_codes")
    if (
        not isinstance(reason_codes, list)
        or any(not isinstance(value, str) or not value for value in reason_codes)
        or reason_codes != sorted(set(reason_codes))
    ):
        raise ContractError("model-proxy reason codes must be sorted and unique")
    if not isinstance(row.get("notes"), str):
        raise ContractError("model-proxy notes must be text")
    return row


def aggregate_blind_model_proxy_votes(
    *,
    views: Sequence[Mapping[str, Any]],
    votes: Sequence[Mapping[str, Any]],
    usefulness_threshold: float = 0.67,
    core_idea_threshold: float = 0.67,
    maximum_score_range: float = 0.25,
) -> dict[str, Any]:
    """Aggregate exactly three role-blinded non-human votes.

    Any abstention, boolean disagreement, or score range above the frozen
    tolerance yields ``indeterminate``.  Scores are never averaged to conceal
    disagreement.  Even a passing proxy endpoint cannot satisfy a human or
    release qualification gate.
    """

    thresholds = {
        "usefulness": _unit_interval(usefulness_threshold, "usefulness threshold"),
        "core_idea": _unit_interval(core_idea_threshold, "core-idea threshold"),
        "maximum_score_range": _unit_interval(
            maximum_score_range, "maximum score range"
        ),
    }
    if len(views) != len(MODEL_PROXY_ROLES) or len(votes) != len(MODEL_PROXY_ROLES):
        raise ContractError("model-proxy aggregation requires exactly three views and votes")
    normalized_views: dict[str, dict[str, Any]] = {}
    for view in views:
        normalized = _validate_view(view)
        role = str(normalized["role"])
        if role in normalized_views:
            raise ContractError("model-proxy views repeat a role")
        normalized_views[role] = normalized
    if set(normalized_views) != set(MODEL_PROXY_ROLES):
        raise ContractError("model-proxy views do not cover the exact role roster")
    payload_hashes = {view["blind_payload_sha256"] for view in normalized_views.values()}
    item_ids = {view["blind_item_id"] for view in normalized_views.values()}
    if len(payload_hashes) != 1 or len(item_ids) != 1:
        raise IntegrityError("model-proxy role views do not bind one blind payload")

    rows: dict[str, dict[str, Any]] = {}
    for vote in votes:
        if not isinstance(vote, Mapping):
            raise ContractError("model-proxy vote must be an object")
        role = vote.get("role")
        if role not in normalized_views or role in rows:
            raise ContractError("model-proxy votes repeat or invent a role")
        rows[str(role)] = validate_blind_model_proxy_vote(
            view=normalized_views[str(role)], vote=vote
        )
    if set(rows) != set(MODEL_PROXY_ROLES):
        raise ContractError("model-proxy votes do not cover the exact role roster")
    rater_ids = [str(rows[role]["rater_id"]) for role in MODEL_PROXY_ROLES]
    if len(set(rater_ids)) != len(rater_ids):
        raise ContractError("model-proxy roles must be performed by distinct raters")

    abstained = any(bool(rows[role]["abstained"]) for role in MODEL_PROXY_ROLES)
    grounded_values = [bool(rows[role]["grounded"]) for role in MODEL_PROXY_ROLES]
    fact_values = [bool(rows[role]["fact_correct"]) for role in MODEL_PROXY_ROLES]
    core_scores = [float(rows[role]["core_idea_score"]) for role in MODEL_PROXY_ROLES]
    usefulness_scores = [float(rows[role]["usefulness_score"]) for role in MODEL_PROXY_ROLES]
    boolean_disagreement = len(set(grounded_values)) > 1 or len(set(fact_values)) > 1
    core_range = max(core_scores) - min(core_scores)
    usefulness_range = max(usefulness_scores) - min(usefulness_scores)
    score_disagreement = (
        core_range > thresholds["maximum_score_range"]
        or usefulness_range > thresholds["maximum_score_range"]
    )
    indeterminate = abstained or boolean_disagreement or score_disagreement
    core_mean = sum(core_scores) / len(core_scores)
    usefulness_mean = sum(usefulness_scores) / len(usefulness_scores)
    unsupported = max(
        int(rows[role]["unsupported_meaning_count"]) for role in MODEL_PROXY_ROLES
    )
    conditions = {
        "no_abstention": not abstained,
        "unanimously_grounded": all(grounded_values),
        "unanimously_fact_correct": all(fact_values),
        "zero_unsupported_meaning": unsupported == 0,
        "core_idea_threshold": core_mean >= thresholds["core_idea"],
        "usefulness_threshold": usefulness_mean >= thresholds["usefulness"],
        "agreement_tolerance": not boolean_disagreement and not score_disagreement,
    }
    engineering_proxy_e = None if indeterminate else int(all(conditions.values()))
    return {
        "schema_version": "chesstory.eval.blind-model-proxy-aggregate.v1",
        "evidence_tier": MODEL_PROXY_TIER,
        "human_gate_eligible": False,
        "release_eligible": False,
        "production_bottleneck": None,
        "blind_item_id": next(iter(item_ids)),
        "blind_payload_sha256": next(iter(payload_hashes)),
        "status": "indeterminate" if indeterminate else "complete",
        "engineering_proxy_E": engineering_proxy_e,
        "conditions": conditions,
        "thresholds": thresholds,
        "diagnostics": {
            "core_idea_mean": core_mean,
            "core_idea_range": core_range,
            "usefulness_mean": usefulness_mean,
            "usefulness_range": usefulness_range,
            "maximum_unsupported_meaning_count": unsupported,
            "rater_count": len(rater_ids),
            "held_out_human_rater_count": 0,
        },
        "votes": [rows[role] for role in MODEL_PROXY_ROLES],
    }


def _validate_view(view: Mapping[str, Any]) -> dict[str, Any]:
    row = _finite_json_object(view, "model-proxy view")
    if set(row) != {
        "schema_version",
        "evidence_tier",
        "human_gate_eligible",
        "blind_item_id",
        "blind_payload_sha256",
        "role",
        "rubric",
        "payload",
    }:
        raise ContractError("model-proxy view has missing or unknown fields")
    if row.get("schema_version") != "chesstory.eval.blind-model-proxy-view.v1":
        raise ContractError("unsupported model-proxy view schema")
    if row.get("evidence_tier") != MODEL_PROXY_TIER or row.get(
        "human_gate_eligible"
    ) is not False:
        raise ContractError("model-proxy view may not claim human eligibility")
    role = row.get("role")
    if role not in MODEL_PROXY_ROLES or row.get("rubric") != _role_rubric(str(role)):
        raise ContractError("model-proxy view rubric/role binding changed")
    payload = row.get("payload")
    if not isinstance(payload, Mapping):
        raise ContractError("model-proxy view payload is missing")
    if set(payload) != {
        "schema_version",
        "evidence_tier",
        "human_gate_eligible",
        "blind_item_id",
        "evaluation_nonce",
        "candidate_output",
        "stockfish_evidence",
    }:
        raise ContractError("model-proxy payload has missing or unknown fields")
    if payload.get("schema_version") != (
        "chesstory.eval.blind-model-proxy-payload.v1"
    ):
        raise ContractError("unsupported model-proxy payload schema")
    _nonempty_text(payload.get("evaluation_nonce"), "evaluation nonce")
    candidate = payload.get("candidate_output")
    evidence = payload.get("stockfish_evidence")
    if not isinstance(candidate, Mapping) or not isinstance(evidence, Mapping):
        raise ContractError("model-proxy candidate/evidence payload is missing")
    _validate_candidate_output(candidate)
    _validate_stockfish_evidence(evidence)
    if row.get("blind_item_id") != payload.get("blind_item_id"):
        raise IntegrityError("model-proxy view blind item binding changed")
    if row.get("blind_payload_sha256") != sha256_json(payload):
        raise IntegrityError("model-proxy view payload hash changed")
    if row.get("evidence_tier") != payload.get("evidence_tier") or payload.get(
        "human_gate_eligible"
    ) is not False:
        raise IntegrityError("model-proxy payload evidence tier changed")
    _reject_provenance_keys(payload, location="model-proxy payload")
    return row


def _validate_stockfish_evidence(evidence: Mapping[str, Any]) -> None:
    required = {
        "engine_name",
        "engine_sha256",
        "configuration",
        "fen",
        "side_to_move",
        "legal_position",
        "repetition_stable",
        "candidate_lines",
        "verified_variations",
    }
    if set(evidence) != required:
        raise ContractError("Stockfish evidence has missing or unknown fields")
    _nonempty_text(evidence.get("engine_name"), "engine name")
    if not isinstance(evidence.get("engine_sha256"), str) or not _HEX_64.fullmatch(
        str(evidence["engine_sha256"])
    ):
        raise ContractError("engine SHA-256 is invalid")
    _nonempty_text(evidence.get("fen"), "evidence FEN")
    if evidence.get("side_to_move") not in {"white", "black"}:
        raise ContractError("evidence side_to_move is invalid")
    if evidence.get("legal_position") is not True:
        raise ContractError("blind proxy requires an independently legal position")
    if not isinstance(evidence.get("repetition_stable"), bool):
        raise ContractError("repetition_stable must be boolean")
    configuration = evidence.get("configuration")
    if not isinstance(configuration, Mapping) or not configuration:
        raise ContractError("Stockfish configuration is missing")
    lines = evidence.get("candidate_lines")
    if not isinstance(lines, list) or not lines:
        raise ContractError("Stockfish candidate lines are missing")
    for index, line in enumerate(lines):
        if not isinstance(line, Mapping) or set(line) != {
            "rank",
            "root_move_uci",
            "score_cp",
            "depth",
            "nodes",
            "pv_uci",
        }:
            raise ContractError(f"Stockfish candidate line {index} is malformed")
        if line.get("rank") != index + 1:
            raise ContractError("Stockfish candidate lines must retain rank order")
        _nonempty_text(line.get("root_move_uci"), "root move")
        if isinstance(line.get("score_cp"), bool) or not isinstance(
            line.get("score_cp"), int
        ):
            raise ContractError("Stockfish line score must be integer centipawns")
        if (
            isinstance(line.get("depth"), bool)
            or not isinstance(line.get("depth"), int)
            or int(line["depth"]) <= 0
            or isinstance(line.get("nodes"), bool)
            or not isinstance(line.get("nodes"), int)
            or int(line["nodes"]) < 0
        ):
            raise ContractError("Stockfish line depth/nodes are invalid")
        pv = line.get("pv_uci")
        if not isinstance(pv, list) or not pv or any(
            not isinstance(move, str) or not move for move in pv
        ):
            raise ContractError("Stockfish line PV is invalid")
    variations = evidence.get("verified_variations")
    if not isinstance(variations, list):
        raise ContractError("verified Stockfish variations must be an array")
    seen_variations: set[str] = set()
    for index, variation in enumerate(variations):
        if not isinstance(variation, Mapping) or set(variation) != {
            "variation_id",
            "moves_uci",
            "legal",
            "score_cp",
            "score_perspective",
            "depth",
            "nodes",
            "purpose",
        }:
            raise ContractError(f"verified Stockfish variation {index} is malformed")
        variation_id = variation.get("variation_id")
        _nonempty_text(variation_id, "variation ID")
        if variation_id in seen_variations:
            raise ContractError("verified Stockfish variation IDs must be unique")
        seen_variations.add(str(variation_id))
        moves = variation.get("moves_uci")
        if not isinstance(moves, list) or not moves or any(
            not isinstance(move, str) or not move for move in moves
        ):
            raise ContractError("verified variation moves are invalid")
        if variation.get("legal") is not True:
            raise ContractError("verified variation must pass independent legal replay")
        if variation.get("score_perspective") != "root-side":
            raise ContractError("verified variation score perspective must be root-side")
        if isinstance(variation.get("score_cp"), bool) or not isinstance(
            variation.get("score_cp"), int
        ):
            raise ContractError("verified variation score must be integer centipawns")
        if (
            isinstance(variation.get("depth"), bool)
            or not isinstance(variation.get("depth"), int)
            or int(variation["depth"]) <= 0
            or isinstance(variation.get("nodes"), bool)
            or not isinstance(variation.get("nodes"), int)
            or int(variation["nodes"]) < 0
        ):
            raise ContractError("verified variation depth/nodes are invalid")
        _nonempty_text(variation.get("purpose"), "verified variation purpose")


def _validate_candidate_output(candidate: Mapping[str, Any]) -> None:
    if set(candidate) != {"language", "text", "claim_citations"}:
        raise ContractError(
            "blind candidate output must contain language, text, and claim_citations"
        )
    if candidate.get("language") != "en":
        raise ContractError("blind candidate output must use canonical backend language en")
    text = _nonempty_text(candidate.get("text"), "candidate text")
    if "\ufffd" in text:
        raise ContractError("candidate text contains a Unicode replacement character")
    citations = candidate.get("claim_citations")
    if (
        not isinstance(citations, list)
        or any(not isinstance(value, str) or not value for value in citations)
        or citations != sorted(set(citations))
    ):
        raise ContractError("candidate claim citations must be sorted and unique")


def _role_rubric(role: str) -> dict[str, Any]:
    common = {
        "required_vote_schema": "chesstory.eval.blind-model-proxy-vote.v1",
        "score_range": [0.0, 1.0],
        "must_check": [
            "legality-and-PV-consistency",
            "unsupported-chess-meaning",
            "core-idea-specificity",
            "player-facing-usefulness",
        ],
        "do_not_infer": [
            "source",
            "arm",
            "oracle-chain",
            "book-answer-key",
            "human-consensus",
        ],
    }
    focus = {
        "grounding-auditor": "Treat any unsupported move, square, causal link, or evaluation direction as a hard failure.",
        "chess-meaning-rater": "Judge whether the explanation identifies the position-specific mechanism rather than a generic slogan.",
        "usefulness-rater": "Judge whether a player can understand why the move matters and what changes in the position.",
    }
    return {**common, "role": role, "primary_focus": focus[role]}


def _reject_provenance_keys(value: Any, *, location: str) -> None:
    if isinstance(value, Mapping):
        forbidden = _FORBIDDEN_BLIND_KEYS.intersection(str(key) for key in value)
        if forbidden:
            raise ContractError(f"{location} leaks blind provenance keys {sorted(forbidden)}")
        for key, child in value.items():
            _reject_provenance_keys(child, location=f"{location}.{key}")
    elif isinstance(value, list):
        for index, child in enumerate(value):
            _reject_provenance_keys(child, location=f"{location}[{index}]")


def _finite_json_object(value: Mapping[str, Any], label: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ContractError(f"{label} must be an object")
    try:
        return json.loads(json.dumps(value, ensure_ascii=False, allow_nan=False))
    except (TypeError, ValueError) as error:
        raise ContractError(f"{label} must be finite JSON") from error


def _nonempty_text(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ContractError(f"{label} must be non-empty text")
    return value


def _unit_interval(value: Any, label: str) -> float:
    if (
        isinstance(value, bool)
        or not isinstance(value, (int, float))
        or not math.isfinite(float(value))
        or not 0.0 <= float(value) <= 1.0
    ):
        raise ContractError(f"{label} must lie in [0, 1]")
    return float(value)


__all__ = [
    "MODEL_PROXY_ROLES",
    "MODEL_PROXY_TIER",
    "aggregate_blind_model_proxy_votes",
    "build_blind_model_proxy_view",
    "validate_blind_model_proxy_vote",
]
