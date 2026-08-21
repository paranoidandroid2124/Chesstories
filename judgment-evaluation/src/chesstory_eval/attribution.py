from __future__ import annotations

import math
from collections.abc import Mapping, Sequence
from typing import Any

from .design import (
    INTERVENTION_CONTEXTS,
    LEAVE_ONE_ACTUAL_CONTEXT,
    ONE_STAGE_CONTEXT,
    STABLE_Q_CONTEXT,
    build_arm_plan,
    parse_factorial_context,
)
from .model import ContractError, IntegrityError, STAGES
from .statistics import registered_contrast_specifications


SEMANTIC_CONTRASTS = frozenset({"delta", "lambda", "gamma"})
_DIAGNOSTIC_SPLITS = frozenset({"diagnostic-explore", "diagnostic-confirm"})
_NON_ATTRIBUTION_SPLITS = frozenset({"fresh-confirm", "blind"})
_HYPOTHESIS_KEYS = frozenset(
    {
        "stage",
        "direction",
        "contrast_ids_by_oracle_chain",
        "estimand",
    }
)
_ESTIMAND = "oracle-substitution-recovers-final-E"
_CONTRAST_ORDER = {"delta": 0, "lambda": 1, "gamma": 2}


def deterministic_attribution_decision(
    *,
    split: str,
    contrasts: Sequence[Mapping[str, Any]],
    preregistration: Mapping[str, Any],
    oracle_chains: Sequence[str],
    observed_atomic_clusters: int,
    foundation_passed: bool,
    ceiling_passed: bool,
) -> dict[str, Any]:
    """Return the only stage-attribution decision licensed by the frozen run.

    Attribution is kept at the exact intervention coordinate
    ``(contrast, context, focus_stage, held_stage, held_source)``.  In
    particular, adjacent-factorial Gamma effects are never pooled or collapsed
    merely because they share a focus stage.  The two oracle chains remain
    fixed independent sensitivity analyses.

    ``diagnostic-explore`` can emit candidate hypotheses but can never name a
    production bottleneck.  ``diagnostic-confirm`` can name the stage from its
    one exact preregistered hypothesis only after both chains independently
    satisfy every positive-effect gate.  Fresh-confirm and blind are explicitly
    non-attribution splits.
    """

    chains = _normalize_chains(oracle_chains)
    base: dict[str, Any] = {
        "schema_version": "chesstory.eval.attribution-decision.v1",
        "production_bottleneck": None,
        "production_bottleneck_kind": None,
        "production_interface_bottleneck": None,
    }
    if not foundation_passed:
        return {
            **base,
            "status": "indeterminate",
            "reason": "foundation controls did not pass before attribution",
            "reason_codes": ["foundation-controls-not-passed"],
            "candidate_hypotheses": [],
        }
    if not ceiling_passed:
        return {
            **base,
            "status": "indeterminate",
            "reason": "both independent all-oracle ceilings did not pass",
            "reason_codes": ["both-oracle-ceilings-not-passed"],
            "candidate_hypotheses": [],
        }

    required_clusters = _required_atomic_clusters(preregistration)
    if (
        isinstance(observed_atomic_clusters, bool)
        or not isinstance(observed_atomic_clusters, int)
        or observed_atomic_clusters < 0
    ):
        raise ContractError("observed atomic-cluster count must be a nonnegative integer")
    if observed_atomic_clusters < required_clusters:
        return {
            **base,
            "status": "indeterminate",
            "reason": (
                f"effective atomic clusters {observed_atomic_clusters} are below the "
                f"preregistered minimum {required_clusters}"
            ),
            "reason_codes": ["minimum-atomic-clusters-not-met"],
            "candidate_hypotheses": [],
            "observed_atomic_clusters": observed_atomic_clusters,
            "required_atomic_clusters": required_clusters,
        }

    if split in _NON_ATTRIBUTION_SPLITS:
        return {
            **base,
            "status": "no-new-attribution",
            "reason": f"{split} is not licensed to create a new stage attribution",
            "reason_codes": ["split-does-not-create-attribution"],
            "candidate_hypotheses": [],
        }
    if split not in _DIAGNOSTIC_SPLITS:
        return {
            **base,
            "status": "indeterminate",
            "reason": f"split {split!r} has no registered attribution transition",
            "reason_codes": ["unsupported-attribution-split"],
            "candidate_hypotheses": [],
        }

    indexed = _index_semantic_contrasts(contrasts, chains)
    try:
        minimum_deltas, minimum_source = _minimum_delta_registry(
            preregistration, indexed
        )
        alpha = _family_wise_alpha(preregistration)
    except ContractError as error:
        return {
            **base,
            "status": "indeterminate",
            "reason": str(error),
            "reason_codes": ["attribution-thresholds-not-preregistered"],
            "candidate_hypotheses": [],
        }

    if split == "diagnostic-explore":
        candidates = _explore_candidates(
            indexed=indexed,
            chains=chains,
            minimum_deltas=minimum_deltas,
            minimum_delta_source=minimum_source,
            required_clusters=required_clusters,
            alpha=alpha,
        )
        return {
            **base,
            "status": "candidate-hypotheses-only",
            "reason": (
                "concordant positive semantic substitutions are exploration "
                "candidates only; an exact independent confirm is still required"
                if candidates
                else "no exact intervention coordinate passed in both fixed oracle chains"
            ),
            "reason_codes": (
                ["independent-confirm-required"]
                if candidates
                else ["no-concordant-positive-semantic-substitution"]
            ),
            "candidate_hypotheses": candidates,
            "family_wise_alpha": alpha,
            "required_atomic_clusters": required_clusters,
            "minimum_delta_registry_source": minimum_source,
        }

    try:
        hypothesis = normalize_selected_attribution_hypothesis(
            preregistration, chains
        )
        selected = select_attribution_contrasts(
            hypothesis=hypothesis,
            contrasts=contrasts,
            oracle_chains=chains,
        )
    except (ContractError, IntegrityError) as error:
        return {
            **base,
            "status": "indeterminate",
            "reason": str(error),
            "reason_codes": ["selected-attribution-hypothesis-invalid"],
            "candidate_hypotheses": [],
        }

    chain_evidence: dict[str, Any] = {}
    all_passed = True
    for chain in chains:
        summary = selected[chain]
        contrast_id = str(summary["contrast_id"])
        evidence = _positive_gate_evidence(
            summary,
            minimum_delta=minimum_deltas[contrast_id],
            required_clusters=required_clusters,
            alpha=alpha,
        )
        chain_evidence[chain] = evidence
        all_passed = all_passed and evidence["status"] == "passed"

    coordinate = _coordinate_document(next(iter(selected.values())))
    if all_passed:
        if coordinate["held_stage"] is not None:
            return {
                **base,
                "status": "confirmed-interface-conditioned-bottleneck",
                "reason": (
                    "the exact preregistered factorial Gamma substitution replicated "
                    "in both fixed oracle chains at one held interface coordinate; "
                    "it is not licensed as a single-stage bottleneck"
                ),
                "reason_codes": [],
                "production_bottleneck_kind": "interface-conditioned",
                "production_interface_bottleneck": coordinate,
                "selected_attribution_hypothesis": hypothesis,
                "intervention_coordinate": coordinate,
                "chain_evidence": chain_evidence,
                "family_wise_alpha": alpha,
                "required_atomic_clusters": required_clusters,
                "minimum_delta_registry_source": minimum_source,
            }
        return {
            **base,
            "status": "confirmed-stage-bottleneck",
            "reason": (
                "the exact preregistered positive semantic substitution "
                "replicated in both fixed oracle chains"
            ),
            "reason_codes": [],
            "production_bottleneck": hypothesis["stage"],
            "production_bottleneck_kind": "stage",
            "selected_attribution_hypothesis": hypothesis,
            "intervention_coordinate": coordinate,
            "chain_evidence": chain_evidence,
            "family_wise_alpha": alpha,
            "required_atomic_clusters": required_clusters,
            "minimum_delta_registry_source": minimum_source,
        }

    directions = {
        chain: _observed_direction(selected[chain]) for chain in chains
    }
    disagreement = len(set(directions.values())) > 1
    return {
        **base,
        "status": "indeterminate",
        "reason": (
            "the two fixed oracle chains disagree on the selected effect direction"
            if disagreement
            else (
                "the exact preregistered positive effect did not satisfy every "
                "delta, confidence, Holm, and cluster gate in both chains"
            )
        ),
        "reason_codes": [
            "fixed-oracle-chain-direction-disagreement"
            if disagreement
            else "selected-positive-effect-not-replicated"
        ],
        "candidate_hypotheses": [],
        "selected_attribution_hypothesis": hypothesis,
        "intervention_coordinate": coordinate,
        "chain_evidence": chain_evidence,
        "observed_directions_by_oracle_chain": directions,
        "family_wise_alpha": alpha,
        "required_atomic_clusters": required_clusters,
        "minimum_delta_registry_source": minimum_source,
    }


def normalize_selected_attribution_hypothesis(
    preregistration: Mapping[str, Any], oracle_chains: Sequence[str]
) -> dict[str, Any]:
    """Normalize the exact hypothesis shape shared with release qualification.

    The chain-specific contrast IDs bind contrast type, context, focus stage,
    and held-stage/source metadata.  :func:`select_attribution_contrasts`
    verifies those coordinates against the recomputed summaries before any
    decision is possible.
    """

    chains = _normalize_chains(oracle_chains)
    hypothesis = preregistration.get("selected_attribution_hypothesis")
    if not isinstance(hypothesis, Mapping) or set(hypothesis) != _HYPOTHESIS_KEYS:
        raise ContractError(
            "confirm requires an exact preregistered selected contrast/stage/direction binding"
        )
    stage = hypothesis.get("stage")
    direction = hypothesis.get("direction")
    selected = hypothesis.get("contrast_ids_by_oracle_chain")
    if stage not in STAGES:
        raise ContractError("selected attribution stage is not registered")
    if hypothesis.get("estimand") != _ESTIMAND:
        raise ContractError(
            "selected attribution must estimate oracle substitution recovery of final E"
        )
    if direction != "positive":
        raise ContractError(
            "a causal limiting-stage attribution requires positive final-E recovery"
        )
    if not isinstance(selected, Mapping) or set(selected) != set(chains):
        raise ContractError(
            "selected attribution must bind one contrast ID for each fixed oracle chain"
        )
    normalized: dict[str, str] = {}
    for chain in chains:
        contrast_id = selected.get(chain)
        if not isinstance(contrast_id, str) or not contrast_id.strip():
            raise ContractError(
                f"selected attribution contrast is missing for chain {chain!r}"
            )
        normalized[chain] = contrast_id
    if len(set(normalized.values())) != len(normalized):
        raise ContractError("fixed oracle chains must select distinct contrast IDs")
    return {
        "stage": stage,
        "direction": "positive",
        "estimand": _ESTIMAND,
        "contrast_ids_by_oracle_chain": normalized,
    }


def select_attribution_contrasts(
    *,
    hypothesis: Mapping[str, Any],
    contrasts: Sequence[Mapping[str, Any]],
    oracle_chains: Sequence[str],
) -> dict[str, Mapping[str, Any]]:
    """Resolve one exact, same-coordinate semantic contrast per fixed chain."""

    chains = _normalize_chains(oracle_chains)
    by_id: dict[str, Mapping[str, Any]] = {}
    for summary in contrasts:
        contrast_id = summary.get("contrast_id")
        if not isinstance(contrast_id, str) or not contrast_id or contrast_id in by_id:
            raise IntegrityError("recomputed statistical contrast IDs are invalid")
        by_id[contrast_id] = summary
    selected_ids = hypothesis.get("contrast_ids_by_oracle_chain")
    if not isinstance(selected_ids, Mapping) or set(selected_ids) != set(chains):
        raise ContractError("selected attribution chain bindings are invalid")

    selected: dict[str, Mapping[str, Any]] = {}
    common_coordinate: tuple[Any, ...] | None = None
    for chain in chains:
        contrast_id = selected_ids[chain]
        summary = by_id.get(contrast_id)
        if summary is None:
            raise ContractError(
                f"preregistered confirm contrast is absent: {contrast_id!r}"
            )
        if (
            summary.get("oracle_chain") != chain
            or summary.get("focus_stage") != hypothesis.get("stage")
            or summary.get("alternative") != "greater"
            or summary.get("contrast") not in SEMANTIC_CONTRASTS
            or summary.get("value_key") != "E"
            or summary.get("contrast_id") != _expected_contrast_id(summary)
        ):
            raise IntegrityError(
                f"preregistered contrast/stage/direction binding differs for {chain!r}"
            )
        coordinate = _coordinate(summary)
        if common_coordinate is None:
            common_coordinate = coordinate
        elif coordinate != common_coordinate:
            raise ContractError(
                "confirm chains do not select the same contrast/context/held coordinate"
            )
        selected[chain] = summary
    return selected


def resolve_attribution_minimum_deltas(
    preregistration: Mapping[str, Any],
    contrasts: Sequence[Mapping[str, Any]],
    oracle_chains: Sequence[str],
) -> dict[str, float]:
    """Resolve the exact preregistered positive-effect threshold by contrast ID.

    The resolver derives its complete semantic Delta/Lambda/Gamma ID set from
    the recomputed registered contrasts and never substitutes ``epsilon_gain``
    or a release noninferiority delta for ``attribution_minimum_delta``.
    """

    chains = _normalize_chains(oracle_chains)
    indexed = _index_semantic_contrasts(contrasts, chains)
    minimum_deltas, _ = _minimum_delta_registry(preregistration, indexed)
    return minimum_deltas


def _explore_candidates(
    *,
    indexed: Mapping[tuple[Any, ...], Mapping[str, Mapping[str, Any]]],
    chains: tuple[str, str],
    minimum_deltas: Mapping[str, float],
    minimum_delta_source: str,
    required_clusters: int,
    alpha: float,
) -> list[dict[str, Any]]:
    candidates: list[dict[str, Any]] = []
    for coordinate in sorted(indexed, key=_coordinate_sort_key):
        by_chain = indexed[coordinate]
        if set(by_chain) != set(chains):
            continue
        evidence: dict[str, Any] = {}
        passed = True
        contrast_ids: dict[str, str] = {}
        for chain in chains:
            summary = by_chain[chain]
            contrast_id = str(summary["contrast_id"])
            contrast_ids[chain] = contrast_id
            gate = _positive_gate_evidence(
                summary,
                minimum_delta=minimum_deltas[contrast_id],
                required_clusters=required_clusters,
                alpha=alpha,
            )
            evidence[chain] = gate
            passed = passed and gate["status"] == "passed"
        if not passed:
            continue
        first = by_chain[chains[0]]
        candidates.append(
            {
                "hypothesis": {
                    "stage": first["focus_stage"],
                    "direction": "positive",
                    "estimand": _ESTIMAND,
                    "contrast_ids_by_oracle_chain": contrast_ids,
                },
                "intervention_coordinate": _coordinate_document(first),
                "chain_evidence": evidence,
                "minimum_delta_registry_source": minimum_delta_source,
            }
        )
    return candidates


def _index_semantic_contrasts(
    contrasts: Sequence[Mapping[str, Any]], chains: tuple[str, str]
) -> dict[tuple[Any, ...], dict[str, Mapping[str, Any]]]:
    indexed: dict[tuple[Any, ...], dict[str, Mapping[str, Any]]] = {}
    seen_ids: set[str] = set()
    expected_by_id = {
        str(specification["contrast_id"]): specification
        for specification in registered_contrast_specifications(tuple(build_arm_plan(chains)))
    }
    for summary in contrasts:
        if not isinstance(summary, Mapping):
            raise IntegrityError("statistical contrast summary must be an object")
        contrast = summary.get("contrast")
        contrast_id = summary.get("contrast_id")
        chain = summary.get("oracle_chain")
        if not isinstance(contrast_id, str) or not contrast_id or contrast_id in seen_ids:
            raise IntegrityError("registered contrast IDs must be unique non-empty text")
        expected_specification = expected_by_id.get(contrast_id)
        if expected_specification is None or any(
            summary.get(key) != expected_specification[key]
            for key in (
                "contrast_id",
                "contrast",
                "symbol",
                "context",
                "focus_stage",
                "oracle_chain",
                "held_stage",
                "held_source",
                "treatment_arm",
                "control_arm",
            )
        ):
            raise IntegrityError(
                "registered contrast summary differs from the exact fixed oracle-chain plan"
            )
        if chain not in chains:
            raise IntegrityError("registered contrast names an unregistered oracle chain")
        expected_family = f"fixed-oracle-chain:{chain}"
        if summary.get("holm_family") != expected_family:
            raise IntegrityError("registered contrast has an invalid Holm family")
        seen_ids.add(contrast_id)
        if contrast not in SEMANTIC_CONTRASTS:
            continue
        if summary.get("focus_stage") not in STAGES:
            raise IntegrityError("semantic contrast has an invalid focus stage")
        if summary.get("value_key") != "E" or summary.get("alternative") != "greater":
            raise IntegrityError(
                "stage attribution requires a positive one-sided final-E contrast"
            )
        if contrast_id != _expected_contrast_id(summary):
            raise IntegrityError(
                "semantic contrast ID does not bind its exact intervention coordinate"
            )
        coordinate = _coordinate(summary)
        by_chain = indexed.setdefault(coordinate, {})
        if chain in by_chain:
            raise IntegrityError(
                "multiple semantic contrasts occupy one chain/intervention coordinate"
            )
        by_chain[str(chain)] = summary
    if seen_ids != set(expected_by_id):
        raise IntegrityError(
            "registered contrast IDs do not match the exact fixed oracle-chain plan"
        )
    if not indexed or any(set(by_chain) != set(chains) for by_chain in indexed.values()):
        raise IntegrityError(
            "attribution requires every semantic intervention coordinate in each fixed oracle chain"
        )
    return indexed


def _positive_gate_evidence(
    summary: Mapping[str, Any],
    *,
    minimum_delta: float,
    required_clusters: int,
    alpha: float,
) -> dict[str, Any]:
    estimate = _finite_number(summary.get("estimate"), "contrast estimate")
    ci_lower = _finite_number(summary.get("ci_lower"), "one-sided CI lower bound")
    p_holm = _finite_number(summary.get("p_holm"), "Holm-adjusted p-value")
    n_clusters = summary.get("n_clusters")
    if (
        isinstance(n_clusters, bool)
        or not isinstance(n_clusters, int)
        or n_clusters < 0
    ):
        raise IntegrityError("contrast atomic-cluster count is invalid")
    if not 0.0 <= p_holm <= 1.0:
        raise IntegrityError("Holm-adjusted p-value is outside [0, 1]")
    gates = {
        "minimum_delta_met": estimate >= minimum_delta,
        "one_sided_ci_strictly_positive": ci_lower > 0.0,
        "holm_family_wise_alpha_met": p_holm <= alpha,
        "minimum_atomic_clusters_met": n_clusters >= required_clusters,
    }
    passed = all(gates.values())
    return {
        "status": "passed" if passed else "not-passed",
        "contrast_id": summary["contrast_id"],
        "estimate": estimate,
        "preregistered_minimum_delta": minimum_delta,
        "one_sided_ci_lower": ci_lower,
        "holm_adjusted_p": p_holm,
        "family_wise_alpha": alpha,
        "atomic_clusters": n_clusters,
        "required_atomic_clusters": required_clusters,
        "gates": gates,
    }


def _minimum_delta_registry(
    preregistration: Mapping[str, Any],
    indexed: Mapping[tuple[Any, ...], Mapping[str, Mapping[str, Any]]],
) -> tuple[dict[str, float], str]:
    summaries = [summary for by_chain in indexed.values() for summary in by_chain.values()]
    expected_ids = {str(summary["contrast_id"]) for summary in summaries}
    raw = preregistration.get("attribution_minimum_delta")
    source = "attribution_minimum_delta"
    if isinstance(raw, bool) or not isinstance(raw, (int, float, Mapping)):
        raise ContractError(
            "attribution requires a separate preregistered attribution_minimum_delta; "
            "epsilon_gain and release endpoint deltas are not attribution thresholds"
        )
    if isinstance(raw, (int, float)):
        minimum = _finite_nonnegative(raw, "attribution minimum delta")
        return {contrast_id: minimum for contrast_id in expected_ids}, source

    supplied_ids = set(raw)
    if supplied_ids == expected_ids:
        return {
            contrast_id: _finite_nonnegative(
                raw[contrast_id], f"minimum delta for {contrast_id}"
            )
            for contrast_id in sorted(expected_ids)
        }, source

    default_keys = {
        "default_delta",
        "default_lambda",
        "default_gamma",
        "Q_stable",
    }
    if supplied_ids != default_keys:
        raise ContractError(
            "minimum-delta registry must exactly name every semantic contrast or "
            "the four frozen default_delta/default_lambda/default_gamma/Q_stable keys"
        )
    defaults = {
        key: _finite_nonnegative(raw[key], f"minimum delta {key}")
        for key in sorted(default_keys)
    }
    resolved: dict[str, float] = {}
    for summary in summaries:
        contrast_id = str(summary["contrast_id"])
        if (
            summary.get("focus_stage") == "Q"
            and summary.get("context") == "stable-q"
            and summary.get("contrast") == "delta"
        ):
            key = "Q_stable"
        else:
            key = f"default_{summary['contrast']}"
        resolved[contrast_id] = defaults[key]
    return resolved, source


def _required_atomic_clusters(preregistration: Mapping[str, Any]) -> int:
    minimums = preregistration.get("minimum_effective_counts")
    if not isinstance(minimums, Mapping):
        raise ContractError("minimum_effective_counts is missing")
    required = minimums.get("atomic_clusters")
    chains = minimums.get("independent_oracle_chains")
    if isinstance(required, bool) or not isinstance(required, int) or required <= 0:
        raise ContractError("minimum atomic-cluster count must be a positive integer")
    if chains != 2:
        raise ContractError("stage attribution requires exactly two independent oracle chains")
    return required


def _family_wise_alpha(preregistration: Mapping[str, Any]) -> float:
    multiplicity = preregistration.get("multiplicity")
    if not isinstance(multiplicity, Mapping):
        raise ContractError("multiplicity policy is missing")
    if multiplicity.get("method") != "Holm" or multiplicity.get("direction") != (
        "one-sided"
    ):
        raise ContractError("attribution requires preregistered one-sided Holm control")
    alpha = _finite_number(
        multiplicity.get("family_wise_alpha"), "family-wise alpha"
    )
    if not 0.0 < alpha < 1.0:
        raise ContractError("family-wise alpha must lie strictly between zero and one")
    return alpha


def _coordinate(summary: Mapping[str, Any]) -> tuple[Any, ...]:
    contrast = summary.get("contrast")
    context = summary.get("context")
    focus_stage = summary.get("focus_stage")
    held_stage = summary.get("held_stage")
    held_source = summary.get("held_source")
    if contrast not in SEMANTIC_CONTRASTS:
        raise IntegrityError("attribution coordinate is not a semantic contrast")
    if not isinstance(context, str) or not context:
        raise IntegrityError("attribution coordinate context is invalid")
    if focus_stage not in STAGES:
        raise IntegrityError("attribution coordinate focus stage is invalid")
    try:
        factorial_pair = parse_factorial_context(context)
    except (TypeError, ValueError) as error:
        raise IntegrityError("attribution coordinate context is malformed") from error
    if factorial_pair is not None:
        left, right = factorial_pair
        expected_held = right if focus_stage == left else left if focus_stage == right else None
        if (
            contrast != "gamma"
            or expected_held is None
            or held_stage != expected_held
            or held_source not in {"actual", "oracle"}
        ):
            raise IntegrityError(
                "factorial Gamma coordinate does not preserve its held interface metadata"
            )
    else:
        if context not in INTERVENTION_CONTEXTS:
            raise IntegrityError("attribution coordinate context is not registered")
        if held_stage is not None or held_source is not None:
            raise IntegrityError(
                "non-factorial attribution coordinate has unexpected held metadata"
            )
        if contrast == "delta" and context not in {
            STABLE_Q_CONTEXT,
            ONE_STAGE_CONTEXT,
        }:
            raise IntegrityError("Delta attribution coordinate has an invalid context")
        if contrast == "lambda" and context != LEAVE_ONE_ACTUAL_CONTEXT:
            raise IntegrityError("Lambda attribution coordinate has an invalid context")
        if context == STABLE_Q_CONTEXT and focus_stage != "Q":
            raise IntegrityError("stable-Q attribution must focus Q")
    return contrast, context, focus_stage, held_stage, held_source


def _coordinate_document(summary: Mapping[str, Any]) -> dict[str, Any]:
    contrast, context, focus_stage, held_stage, held_source = _coordinate(summary)
    return {
        "contrast": contrast,
        "context": context,
        "focus_stage": focus_stage,
        "held_stage": held_stage,
        "held_source": held_source,
    }


def _expected_contrast_id(summary: Mapping[str, Any]) -> str:
    contrast, context, focus_stage, held_stage, held_source = _coordinate(summary)
    identity = (
        contrast,
        context,
        focus_stage,
        summary.get("oracle_chain"),
        held_stage,
        held_source,
    )
    return ":".join("-" if value is None else str(value) for value in identity)


def _coordinate_sort_key(coordinate: tuple[Any, ...]) -> tuple[Any, ...]:
    contrast, context, focus_stage, held_stage, held_source = coordinate
    return (
        STAGES.index(str(focus_stage)),
        _CONTRAST_ORDER[str(contrast)],
        str(context),
        "" if held_stage is None else str(held_stage),
        "" if held_source is None else str(held_source),
    )


def _observed_direction(summary: Mapping[str, Any]) -> str:
    estimate = _finite_number(summary.get("estimate"), "contrast estimate")
    if estimate > 0.0:
        return "positive"
    if estimate < 0.0:
        return "negative"
    return "zero"


def _normalize_chains(oracle_chains: Sequence[str]) -> tuple[str, str]:
    if isinstance(oracle_chains, (str, bytes)):
        raise ContractError("oracle chains must be a sequence")
    chains = tuple(oracle_chains)
    if (
        len(chains) != 2
        or any(not isinstance(chain, str) or not chain for chain in chains)
        or len(set(chains)) != 2
    ):
        raise ContractError("attribution requires exactly two distinct oracle chains")
    ordered = tuple(sorted(chains))
    return ordered[0], ordered[1]


def _finite_nonnegative(value: Any, label: str) -> float:
    number = _finite_number(value, label)
    if number < 0.0:
        raise ContractError(f"{label} must be nonnegative")
    return number


def _finite_number(value: Any, label: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ContractError(f"{label} must be numeric")
    number = float(value)
    if not math.isfinite(number):
        raise ContractError(f"{label} must be finite")
    return number


__all__ = [
    "SEMANTIC_CONTRASTS",
    "deterministic_attribution_decision",
    "normalize_selected_attribution_hypothesis",
    "resolve_attribution_minimum_deltas",
    "select_attribution_contrasts",
]
