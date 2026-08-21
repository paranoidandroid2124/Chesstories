from __future__ import annotations

import hashlib
import math
import random
from collections.abc import Iterable, Mapping, Sequence
from statistics import fmean
from typing import Any

from .design import (
    BACKWARD_CUMULATIVE_CONTEXT,
    FORWARD_CUMULATIVE_CONTEXT,
    INTERVENTION_CONTEXTS,
    LEAVE_ONE_ACTUAL_CONTEXT,
    ONE_STAGE_CONTEXT,
    STABLE_Q_CONTEXT,
    parse_factorial_context,
)
from .model import STAGES, Arm, Source


DEFAULT_BOOTSTRAP_ITERATIONS = 10_000

_EQUIVALENCE_CONTRASTS = frozenset({"delta", "lambda", "gamma"})

_SEQUENTIAL_CONTEXT_ORDER = {
    context: index
    for index, context in enumerate(
        (
            STABLE_Q_CONTEXT,
            ONE_STAGE_CONTEXT,
            LEAVE_ONE_ACTUAL_CONTEXT,
            FORWARD_CUMULATIVE_CONTEXT,
            BACKWARD_CUMULATIVE_CONTEXT,
        )
    )
}


def paired_cluster_contrast(
    rows: Iterable[Mapping[str, Any]],
    treatment_arm: str,
    control_arm: str,
    value_key: str = "E",
    iterations: int = DEFAULT_BOOTSTRAP_ITERATIONS,
    seed: int = 0,
    *,
    confidence: float = 0.95,
    alternative: str = "greater",
) -> dict[str, Any]:
    """Estimate a deterministic paired atomic-cluster contrast.

    Pairing is exact at ``sample_id``.  The bootstrap resamples whole atomic
    clusters and carries every paired sample in a selected cluster together;
    it never resamples or splits rows inside a cluster.  The reported bound is
    a one-sided percentile bound.  ``p_raw`` is the plus-one corrected tail
    probability after centering the bootstrap distribution on the zero null.

    ``value_key='E'`` enforces binary values.  Other keys accept finite numeric
    diagnostics.  A key may be dotted (for example ``diagnostics.D_Q``), and a
    non-dotted key absent at the row top level is looked up in ``diagnostics``.
    """

    _validate_contrast_arguments(
        treatment_arm=treatment_arm,
        control_arm=control_arm,
        value_key=value_key,
        iterations=iterations,
        seed=seed,
        confidence=confidence,
        alternative=alternative,
    )
    treatment: dict[str, tuple[str, float]] = {}
    control: dict[str, tuple[str, float]] = {}

    for row_number, row in enumerate(rows, start=1):
        if not isinstance(row, Mapping):
            raise TypeError(f"row {row_number} must be a mapping")
        arm_id = row.get("arm_id")
        if arm_id not in {treatment_arm, control_arm}:
            continue
        sample_id = _required_text(row, "sample_id", row_number)
        cluster_id = _required_text(row, "atomic_cluster_id", row_number)
        value = _extract_numeric_value(row, value_key, row_number)
        destination = treatment if arm_id == treatment_arm else control
        if sample_id in destination:
            raise ValueError(
                f"duplicate row for sample {sample_id!r} and arm {arm_id!r}"
            )
        destination[sample_id] = (cluster_id, value)

    if not treatment and not control:
        raise ValueError("no rows matched either requested arm")
    treatment_ids = set(treatment)
    control_ids = set(control)
    if treatment_ids != control_ids:
        missing_treatment = sorted(control_ids - treatment_ids)
        missing_control = sorted(treatment_ids - control_ids)
        raise ValueError(
            "paired arms contain different sample sets; "
            f"missing treatment={missing_treatment}, missing control={missing_control}"
        )
    if not treatment_ids:
        raise ValueError("paired contrast requires at least one sample")

    cluster_differences: dict[str, list[float]] = {}
    for sample_id in sorted(treatment_ids):
        treatment_cluster, treatment_value = treatment[sample_id]
        control_cluster, control_value = control[sample_id]
        if treatment_cluster != control_cluster:
            raise ValueError(
                f"sample {sample_id!r} changes atomic cluster across paired arms: "
                f"{treatment_cluster!r} != {control_cluster!r}"
            )
        cluster_differences.setdefault(treatment_cluster, []).append(
            treatment_value - control_value
        )

    clusters = sorted(cluster_differences)
    cluster_summaries = [
        (sum(cluster_differences[cluster]), len(cluster_differences[cluster]))
        for cluster in clusters
    ]
    observed = sum(total for total, _ in cluster_summaries) / sum(
        count for _, count in cluster_summaries
    )

    generator = random.Random(seed)
    bootstrap_estimates: list[float] = []
    cluster_count = len(cluster_summaries)
    for _ in range(iterations):
        sampled_total = 0.0
        sampled_count = 0
        for _ in range(cluster_count):
            total, count = cluster_summaries[generator.randrange(cluster_count)]
            sampled_total += total
            sampled_count += count
        bootstrap_estimates.append(sampled_total / sampled_count)

    ordered = sorted(bootstrap_estimates)
    alpha = 1.0 - confidence
    if alternative == "greater":
        ci_lower: float | None = _quantile(ordered, alpha)
        ci_upper: float | None = None
        tail_count = sum(
            estimate - observed >= observed
            for estimate in bootstrap_estimates
        )
    else:
        ci_lower = None
        ci_upper = _quantile(ordered, confidence)
        tail_count = sum(
            estimate - observed <= observed
            for estimate in bootstrap_estimates
        )
    p_raw = (tail_count + 1.0) / (iterations + 1.0)
    bootstrap_mean = fmean(bootstrap_estimates)
    if iterations > 1:
        bootstrap_standard_error = math.sqrt(
            sum(
                (estimate - bootstrap_mean) ** 2
                for estimate in bootstrap_estimates
            )
            / (iterations - 1)
        )
    else:
        bootstrap_standard_error = 0.0

    return {
        "treatment_arm": treatment_arm,
        "control_arm": control_arm,
        "value_key": value_key,
        "estimate": observed,
        "ci_lower": ci_lower,
        "ci_upper": ci_upper,
        "confidence": confidence,
        "alternative": alternative,
        "p_raw": p_raw,
        "bootstrap_standard_error": bootstrap_standard_error,
        "iterations": iterations,
        "seed": seed,
        "n_samples": len(treatment_ids),
        "n_clusters": cluster_count,
    }


def holm_adjust(p_values: Sequence[float]) -> list[float]:
    """Return Holm step-down adjusted p-values in the original order."""

    if isinstance(p_values, (str, bytes)):
        raise TypeError("p_values must be a sequence of probabilities")
    values = list(p_values)
    for index, value in enumerate(values):
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise TypeError(f"p-value at index {index} must be numeric")
        if not math.isfinite(float(value)) or not 0.0 <= float(value) <= 1.0:
            raise ValueError(f"p-value at index {index} is outside [0, 1]")
    count = len(values)
    if count == 0:
        return []

    ordered = sorted(range(count), key=lambda index: (float(values[index]), index))
    adjusted = [0.0] * count
    running_maximum = 0.0
    for rank, original_index in enumerate(ordered):
        candidate = min(1.0, (count - rank) * float(values[original_index]))
        running_maximum = max(running_maximum, candidate)
        adjusted[original_index] = running_maximum
    return adjusted


def summarize_registered_contrasts(
    rows: Iterable[Mapping[str, Any]],
    plan: Iterable[Arm],
    value_key: str = "E",
    iterations: int = DEFAULT_BOOTSTRAP_ITERATIONS,
    seed: int = 0,
    *,
    confidence: float = 0.95,
    alternative: str = "greater",
) -> list[dict[str, Any]]:
    """Summarize every registered Delta, Lambda, Phi, and Gamma contrast.

    One Holm family is formed within each fixed oracle chain.  Oracle chains
    remain separate sensitivity analyses: every result names one chain and no
    effects are averaged or pooled across chains.
    """

    row_values = tuple(rows)
    arms = tuple(plan)
    _validate_plan(arms)
    specifications = registered_contrast_specifications(arms)
    summaries: list[dict[str, Any]] = []
    for specification in specifications:
        local_seed = _derived_seed(seed, specification["contrast_id"], value_key)
        result = paired_cluster_contrast(
            row_values,
            str(specification["treatment_arm"]),
            str(specification["control_arm"]),
            value_key=value_key,
            iterations=iterations,
            seed=local_seed,
            confidence=confidence,
            alternative=alternative,
        )
        summaries.append({**specification, **result, "analysis_seed": seed})

    # The two fixed oracle chains are independent sensitivity analyses, not
    # exchangeable observations.  Each chain therefore forms its own Holm
    # family; effects are never pooled across chains.
    by_chain: dict[str, list[int]] = {}
    for index, summary in enumerate(summaries):
        by_chain.setdefault(str(summary["oracle_chain"]), []).append(index)
    for chain, indices in sorted(by_chain.items()):
        adjusted = holm_adjust([float(summaries[index]["p_raw"]) for index in indices])
        for index, p_holm in zip(indices, adjusted):
            summaries[index]["p_holm"] = p_holm
            summaries[index]["holm_family"] = f"fixed-oracle-chain:{chain}"
    return summaries


def summarize_fresh_confirm_equivalence(
    rows: Iterable[Mapping[str, Any]],
    plan: Iterable[Arm],
    epsilon_gain: Mapping[str, float],
    value_key: str = "E",
    iterations: int = DEFAULT_BOOTSTRAP_ITERATIONS,
    seed: int = 0,
    *,
    split: str,
    fresh_confirm: bool,
    min_clusters: int,
    alpha: float = 0.05,
) -> dict[str, Any]:
    """Test preregistered residual-gain equivalence, conservatively.

    This is deliberately a *fresh-confirm-only* analysis.  ``epsilon_gain``
    must name every registered Delta, Lambda, and Gamma ``contrast_id``
    exactly; this function never invents, inherits, or fills an epsilon.  The
    two fixed oracle chains form separate Holm families and are never pooled.

    For each contrast, samples are paired by ``sample_id`` and whole atomic
    clusters are resampled with replacement.  The boundary hypothesis is
    ``H0: gain >= epsilon_gain`` against ``H1: gain < epsilon_gain``.  Its
    plus-one bootstrap p-value is obtained by centering the paired-cluster
    bootstrap error distribution at that boundary.  Those p-values receive a
    Holm step-down adjustment within each fixed oracle chain.

    Exact rectangular confidence bounds obtained by inverting the entire Holm
    closure are intentionally *not* claimed.  The result therefore labels its
    ordinary one-sided percentile bound ``unadjusted_upper_bound`` and sets
    ``holm_adjusted_upper_bound`` to ``None``.  To keep the decision fail-closed
    while still furnishing a multiplicity-controlled upper bound, it also
    reports a Bonferroni simultaneous one-sided percentile bound at
    ``1 - alpha / m`` for each of the ``m`` contrasts in that chain.  A
    contrast is called equivalent only when both (a) its boundary hypothesis
    is rejected by Holm at ``alpha`` and (b) that conservative simultaneous
    upper bound is no greater than its exact preregistered epsilon.  Thus a
    nonsignificant result, a wide bound, incomplete pairing, too few clusters,
    an incomplete epsilon registry, or any non-fresh split produces
    ``not-established`` or ``indeterminate`` -- never equivalence.

    The percentile bootstrap and its simultaneous Bonferroni construction
    have the usual bootstrap approximation assumptions; the latter controls
    family-wise coverage conservatively under those assumptions without
    requiring the two oracle chains to be exchangeable.
    """

    row_values = tuple(rows)
    arms = tuple(plan)
    _validate_plan(arms)
    _validate_equivalence_arguments(
        epsilon_gain=epsilon_gain,
        value_key=value_key,
        iterations=iterations,
        seed=seed,
        split=split,
        fresh_confirm=fresh_confirm,
        min_clusters=min_clusters,
        alpha=alpha,
    )

    specifications = [
        specification
        for specification in registered_contrast_specifications(arms)
        if specification["contrast"] in _EQUIVALENCE_CONTRASTS
    ]
    chains = sorted(
        {str(specification["oracle_chain"]) for specification in specifications}
    )
    if len(chains) != 2:
        raise ValueError(
            "fresh-confirm equivalence requires exactly two fixed oracle chains"
        )

    expected_epsilon_ids = {
        str(specification["contrast_id"]) for specification in specifications
    }
    supplied_epsilon_ids = set(epsilon_gain)
    missing_epsilon_ids = sorted(expected_epsilon_ids - supplied_epsilon_ids)
    unexpected_epsilon_ids = sorted(supplied_epsilon_ids - expected_epsilon_ids)
    gate_reasons: list[str] = []
    if split != "fresh-confirm":
        gate_reasons.append("split-must-be-fresh-confirm")
    if fresh_confirm is not True:
        gate_reasons.append("fresh-confirm-flag-not-true")
    if missing_epsilon_ids:
        gate_reasons.append("epsilon-registry-incomplete")
    if unexpected_epsilon_ids:
        gate_reasons.append("epsilon-registry-has-unregistered-contrasts")

    base_report: dict[str, Any] = {
        "schema_version": "chesstory.eval.fresh-confirm-equivalence.v1",
        "split": split,
        "fresh_confirm": fresh_confirm is True,
        "value_key": value_key,
        "alpha": float(alpha),
        "iterations": iterations,
        "analysis_seed": seed,
        "minimum_atomic_clusters": min_clusters,
        "registered_contrasts": sorted(_EQUIVALENCE_CONTRASTS),
        "expected_contrast_count": len(specifications),
        "oracle_chains": chains,
        "multiplicity": {
            "holm_family": "within-each-fixed-oracle-chain",
            "threshold_test": "H0: gain >= epsilon_gain; H1: gain < epsilon_gain",
            "upper_bound": "bonferroni-simultaneous-one-sided-percentile",
            "exact_holm_inverted_upper_bounds": False,
            "oracle_chains_pooled": False,
        },
        "missing_epsilon_contrast_ids": missing_epsilon_ids,
        "unexpected_epsilon_contrast_ids": unexpected_epsilon_ids,
        "reason_codes": gate_reasons,
        "families": [],
    }
    if gate_reasons:
        return {
            **base_report,
            "status": "indeterminate",
            "interpretation": (
                "equivalence was not tested because fresh-confirm or exact "
                "preregistration gates were not satisfied"
            ),
        }

    by_chain: dict[str, list[dict[str, Any]]] = {chain: [] for chain in chains}
    for specification in specifications:
        by_chain[str(specification["oracle_chain"])].append(specification)

    families: list[dict[str, Any]] = []
    for chain in chains:
        chain_specifications = by_chain[chain]
        family_size = len(chain_specifications)
        simultaneous_confidence = 1.0 - float(alpha) / family_size
        results: list[dict[str, Any]] = []
        family_reasons: list[str] = []
        for specification in chain_specifications:
            contrast_id = str(specification["contrast_id"])
            epsilon = float(epsilon_gain[contrast_id])
            local_seed = _derived_seed(
                seed, f"fresh-confirm-equivalence:{contrast_id}", value_key
            )
            try:
                bootstrap = _paired_cluster_bootstrap_distribution(
                    row_values,
                    treatment_arm=str(specification["treatment_arm"]),
                    control_arm=str(specification["control_arm"]),
                    value_key=value_key,
                    iterations=iterations,
                    seed=local_seed,
                )
            except (TypeError, ValueError) as error:
                family_reasons.append("incomplete-or-invalid-paired-data")
                results.append(
                    {
                        **specification,
                        "holm_family": f"fixed-oracle-chain:{chain}",
                        "epsilon_gain": epsilon,
                        "status": "indeterminate",
                        "reason": str(error),
                        "p_raw_boundary": None,
                        "p_holm_boundary": None,
                        "unadjusted_upper_bound": None,
                        "holm_adjusted_upper_bound": None,
                        "simultaneous_upper_bound": None,
                    }
                )
                continue

            cluster_count = int(bootstrap["n_clusters"])
            observed = float(bootstrap["estimate"])
            estimates = bootstrap["bootstrap_estimates"]
            ordered = sorted(estimates)
            unadjusted_upper = _quantile(ordered, 1.0 - float(alpha))
            simultaneous_upper = _quantile(ordered, simultaneous_confidence)
            threshold = observed - epsilon
            tail_count = sum(
                estimate - observed <= threshold for estimate in estimates
            )
            p_raw = (tail_count + 1.0) / (iterations + 1.0)
            status = "pending-holm"
            reason: str | None = None
            if cluster_count < min_clusters:
                status = "indeterminate"
                reason = (
                    f"paired atomic clusters {cluster_count} are below the "
                    f"preregistered minimum {min_clusters}"
                )
                family_reasons.append("minimum-atomic-clusters-not-met")
            results.append(
                {
                    **specification,
                    "holm_family": f"fixed-oracle-chain:{chain}",
                    "epsilon_gain": epsilon,
                    "estimate": observed,
                    "n_samples": int(bootstrap["n_samples"]),
                    "n_clusters": cluster_count,
                    "iterations": iterations,
                    "seed": local_seed,
                    "status": status,
                    "reason": reason,
                    "boundary_null": "gain >= epsilon_gain",
                    "boundary_alternative": "gain < epsilon_gain",
                    "p_raw_boundary": p_raw,
                    "p_holm_boundary": None,
                    "unadjusted_upper_bound": unadjusted_upper,
                    "unadjusted_confidence": 1.0 - float(alpha),
                    "holm_adjusted_upper_bound": None,
                    "simultaneous_upper_bound": simultaneous_upper,
                    "simultaneous_confidence": simultaneous_confidence,
                    "simultaneous_bound_adjustment": "bonferroni",
                }
            )

        family_reasons = sorted(set(family_reasons))
        if family_reasons:
            for result in results:
                result["p_holm_boundary"] = None
                result["equivalent"] = False
                if result["status"] == "pending-holm":
                    result["status"] = "indeterminate"
                    result["reason"] = (
                        "the fixed-chain Holm family is incomplete or below its "
                        "preregistered cluster gate"
                    )
            family_status = "indeterminate"
        else:
            adjusted = holm_adjust(
                [float(result["p_raw_boundary"]) for result in results]
            )
            for result, p_holm in zip(results, adjusted):
                result["p_holm_boundary"] = p_holm
                holm_pass = p_holm <= float(alpha)
                bound_pass = (
                    float(result["simultaneous_upper_bound"])
                    <= float(result["epsilon_gain"])
                )
                equivalent = holm_pass and bound_pass
                result["holm_boundary_rejected"] = holm_pass
                result["simultaneous_bound_below_epsilon"] = bound_pass
                result["equivalent"] = equivalent
                result["status"] = (
                    "equivalence-established" if equivalent else "not-established"
                )
                result["reason"] = (
                    None
                    if equivalent
                    else (
                        "the Holm-adjusted boundary test and conservative "
                        "simultaneous upper-bound criterion did not both pass"
                    )
                )
            family_status = (
                "equivalence-established"
                if all(bool(result["equivalent"]) for result in results)
                else "not-established"
            )

        families.append(
            {
                "oracle_chain": chain,
                "holm_family": f"fixed-oracle-chain:{chain}",
                "family_size": family_size,
                "status": family_status,
                "reason_codes": family_reasons,
                "results": results,
            }
        )

    family_reason_codes = sorted(
        {
            str(reason)
            for family in families
            for reason in family["reason_codes"]
        }
    )
    if any(family["status"] == "indeterminate" for family in families):
        overall_status = "indeterminate"
    elif all(family["status"] == "equivalence-established" for family in families):
        overall_status = "equivalence-established"
    else:
        overall_status = "not-established"
    interpretation = (
        "all preregistered residual-gain equivalence criteria passed in both "
        "fixed oracle-chain sensitivity analyses"
        if overall_status == "equivalence-established"
        else (
            "no absence-of-actionable-oracle-gain conclusion is licensed; "
            "nonsignificance is not equivalence"
        )
    )
    return {
        **base_report,
        "status": overall_status,
        "interpretation": interpretation,
        "reason_codes": family_reason_codes,
        "families": families,
    }


def registered_contrast_specifications(arms: tuple[Arm, ...]) -> list[dict[str, Any]]:
    specifications: list[dict[str, Any]] = []
    sequential: dict[tuple[str, str, str], dict[Source, Arm]] = {}
    for arm in arms:
        if arm.context not in INTERVENTION_CONTEXTS:
            continue
        if arm.focus_stage not in STAGES or arm.oracle_chain is None:
            raise ValueError(
                f"focused context {arm.context!r} has invalid focus/chain on {arm.arm_id}"
            )
        key = (arm.context, arm.focus_stage, arm.oracle_chain)
        source = arm.source_for(arm.focus_stage)
        variants = sequential.setdefault(key, {})
        if source in variants:
            raise ValueError(f"duplicate {source.value} variant for focused group {key}")
        variants[source] = arm

    sequential_keys = sorted(
        sequential,
        key=lambda key: (
            _SEQUENTIAL_CONTEXT_ORDER[key[0]],
            STAGES.index(key[1]),
            key[2],
        ),
    )
    for context, focus_stage, chain in sequential_keys:
        variants = sequential[(context, focus_stage, chain)]
        _require_triplet(variants, (context, focus_stage, chain))
        _require_matching_rest(variants, focus_stage)
        actual = variants[Source.ACTUAL]
        format_control = variants[Source.FORMAT_CONTROL]
        oracle = variants[Source.ORACLE]
        metadata = {
            "context": context,
            "focus_stage": focus_stage,
            "oracle_chain": chain,
            "held_stage": None,
            "held_source": None,
        }
        if context in {STABLE_Q_CONTEXT, ONE_STAGE_CONTEXT}:
            specifications.append(
                _contrast_specification(
                    "delta", "Δ", oracle, actual, metadata
                )
            )
        if context == LEAVE_ONE_ACTUAL_CONTEXT:
            specifications.append(
                _contrast_specification(
                    "lambda", "Λ", oracle, actual, metadata
                )
            )
        specifications.append(
            _contrast_specification(
                "phi", "Φ", format_control, actual, metadata
            )
        )
        specifications.append(
            _contrast_specification(
                "gamma", "Γ", oracle, format_control, metadata
            )
        )

    factorial_groups: dict[tuple[str, str], dict[tuple[Source, Source], Arm]] = {}
    for arm in arms:
        pair = parse_factorial_context(arm.context)
        if pair is None:
            continue
        if arm.focus_stage is not None or arm.oracle_chain is None:
            raise ValueError(f"malformed factorial arm metadata: {arm.arm_id}")
        left, right = pair
        key = (arm.context, arm.oracle_chain)
        cell = (arm.source_for(left), arm.source_for(right))
        grid = factorial_groups.setdefault(key, {})
        if cell in grid:
            raise ValueError(f"duplicate factorial cell {cell} in {key}")
        grid[cell] = arm

    for context, chain in sorted(factorial_groups):
        pair = parse_factorial_context(context)
        if pair is None:  # pragma: no cover - key was constructed from a parsed context
            raise AssertionError("factorial parser became inconsistent")
        left, right = pair
        grid = factorial_groups[(context, chain)]
        expected_cells = {
            (left_source, right_source)
            for left_source in Source
            for right_source in Source
            if not (
                left_source == Source.FORMAT_CONTROL
                and right_source == Source.FORMAT_CONTROL
            )
        }
        if set(grid) != expected_cells:
            missing = sorted(
                (left_source.value, right_source.value)
                for left_source, right_source in expected_cells - set(grid)
            )
            raise ValueError(
                f"factorial grid {context!r}/{chain!r} is incomplete: {missing}"
            )

        for focus_stage, held_stage, focus_index, held_index in (
            (left, right, 0, 1),
            (right, left, 1, 0),
        ):
            for held_source in (Source.ACTUAL, Source.ORACLE):
                def cell_for(focus_source: Source) -> Arm:
                    cell_values = [Source.ACTUAL, Source.ACTUAL]
                    cell_values[focus_index] = focus_source
                    cell_values[held_index] = held_source
                    return grid[(cell_values[0], cell_values[1])]

                actual = cell_for(Source.ACTUAL)
                format_control = cell_for(Source.FORMAT_CONTROL)
                oracle = cell_for(Source.ORACLE)
                metadata = {
                    "context": context,
                    "focus_stage": focus_stage,
                    "oracle_chain": chain,
                    "held_stage": held_stage,
                    "held_source": held_source.value,
                }
                specifications.append(
                    _contrast_specification(
                        "phi", "Φ", format_control, actual, metadata
                    )
                )
                specifications.append(
                    _contrast_specification(
                        "gamma", "Γ", oracle, format_control, metadata
                    )
                )
    return specifications


def _contrast_specification(
    contrast: str,
    symbol: str,
    treatment: Arm,
    control: Arm,
    metadata: Mapping[str, Any],
) -> dict[str, Any]:
    identity = (
        contrast,
        metadata["context"],
        metadata["focus_stage"],
        metadata["oracle_chain"],
        metadata["held_stage"],
        metadata["held_source"],
    )
    readable = ":".join("-" if value is None else str(value) for value in identity)
    return {
        "contrast_id": readable,
        "contrast": contrast,
        "symbol": symbol,
        **metadata,
        "treatment_arm": treatment.arm_id,
        "control_arm": control.arm_id,
    }


def _require_triplet(
    variants: Mapping[Source, Arm], group: tuple[str, str, str]
) -> None:
    expected = set(Source)
    if set(variants) != expected:
        missing = sorted(source.value for source in expected - set(variants))
        extra = sorted(source.value for source in set(variants) - expected)
        raise ValueError(
            f"focused group {group} is not an actual/format/oracle triplet; "
            f"missing={missing}, extra={extra}"
        )


def _require_matching_rest(variants: Mapping[Source, Arm], focus_stage: str) -> None:
    rest_signatures = {
        tuple(
            (stage, arm.source_for(stage))
            for stage in STAGES
            if stage != focus_stage
        )
        for arm in variants.values()
    }
    if len(rest_signatures) != 1:
        raise ValueError(
            f"focused variants for {focus_stage} do not hold surrounding stages fixed"
        )


def _validate_plan(arms: tuple[Arm, ...]) -> None:
    if not arms:
        raise ValueError("plan must contain at least one arm")
    identifiers: set[str] = set()
    for index, arm in enumerate(arms):
        if not isinstance(arm, Arm):
            raise TypeError(f"plan item {index} must be an Arm")
        if arm.arm_id in identifiers:
            raise ValueError(f"duplicate arm_id in plan: {arm.arm_id!r}")
        identifiers.add(arm.arm_id)
        missing = set(STAGES) - set(arm.sources)
        unknown = set(arm.sources) - set(STAGES)
        if missing or unknown:
            raise ValueError(
                f"arm {arm.arm_id} has an invalid source map; "
                f"missing={sorted(missing)}, unknown={sorted(unknown)}"
            )
        for stage, source in arm.sources.items():
            if not isinstance(source, Source):
                raise TypeError(f"arm {arm.arm_id} source for {stage} is not a Source")


def _validate_contrast_arguments(
    *,
    treatment_arm: str,
    control_arm: str,
    value_key: str,
    iterations: int,
    seed: int,
    confidence: float,
    alternative: str,
) -> None:
    if not isinstance(treatment_arm, str) or not treatment_arm.strip():
        raise ValueError("treatment_arm must be non-empty text")
    if not isinstance(control_arm, str) or not control_arm.strip():
        raise ValueError("control_arm must be non-empty text")
    if treatment_arm == control_arm:
        raise ValueError("treatment_arm and control_arm must differ")
    if not isinstance(value_key, str) or not value_key.strip():
        raise ValueError("value_key must be non-empty text")
    if isinstance(iterations, bool) or not isinstance(iterations, int) or iterations <= 0:
        raise ValueError("iterations must be a positive integer")
    if isinstance(seed, bool) or not isinstance(seed, int):
        raise TypeError("seed must be an integer")
    if isinstance(confidence, bool) or not isinstance(confidence, (int, float)):
        raise TypeError("confidence must be numeric")
    if not math.isfinite(float(confidence)) or not 0.0 < float(confidence) < 1.0:
        raise ValueError("confidence must be strictly between zero and one")
    if alternative not in {"greater", "less"}:
        raise ValueError("alternative must be 'greater' or 'less'")


def _validate_equivalence_arguments(
    *,
    epsilon_gain: Mapping[str, float],
    value_key: str,
    iterations: int,
    seed: int,
    split: str,
    fresh_confirm: bool,
    min_clusters: int,
    alpha: float,
) -> None:
    if isinstance(alpha, bool) or not isinstance(alpha, (int, float)):
        raise TypeError("alpha must be numeric")
    if not math.isfinite(float(alpha)) or not 0.0 < float(alpha) < 1.0:
        raise ValueError("alpha must be strictly between zero and one")
    # Reuse the common bootstrap validation without weakening the public
    # paired-contrast contract.
    _validate_contrast_arguments(
        treatment_arm="equivalence-treatment",
        control_arm="equivalence-control",
        value_key=value_key,
        iterations=iterations,
        seed=seed,
        confidence=1.0 - float(alpha),
        alternative="less",
    )
    if not isinstance(epsilon_gain, Mapping):
        raise TypeError("epsilon_gain must be a mapping keyed by contrast_id")
    if not epsilon_gain:
        raise ValueError("epsilon_gain must not be empty")
    for contrast_id, epsilon in epsilon_gain.items():
        if not isinstance(contrast_id, str) or not contrast_id.strip():
            raise ValueError("epsilon_gain keys must be non-empty contrast_id text")
        if isinstance(epsilon, bool) or not isinstance(epsilon, (int, float)):
            raise TypeError(f"epsilon_gain[{contrast_id!r}] must be numeric")
        value = float(epsilon)
        if not math.isfinite(value) or value < 0.0:
            raise ValueError(
                f"epsilon_gain[{contrast_id!r}] must be finite and non-negative"
            )
        if value_key == "E" and value > 1.0:
            raise ValueError(
                f"epsilon_gain[{contrast_id!r}] cannot exceed one for binary E"
            )
    if not isinstance(split, str) or not split.strip():
        raise ValueError("split must be non-empty text")
    if not isinstance(fresh_confirm, bool):
        raise TypeError("fresh_confirm must be boolean")
    if (
        isinstance(min_clusters, bool)
        or not isinstance(min_clusters, int)
        or min_clusters < 2
    ):
        raise ValueError("min_clusters must be an integer of at least two")


def _paired_cluster_bootstrap_distribution(
    rows: Iterable[Mapping[str, Any]],
    *,
    treatment_arm: str,
    control_arm: str,
    value_key: str,
    iterations: int,
    seed: int,
) -> dict[str, Any]:
    """Return the bootstrap draws needed for a threshold-equivalence test."""

    _validate_contrast_arguments(
        treatment_arm=treatment_arm,
        control_arm=control_arm,
        value_key=value_key,
        iterations=iterations,
        seed=seed,
        confidence=0.95,
        alternative="less",
    )
    treatment: dict[str, tuple[str, float]] = {}
    control: dict[str, tuple[str, float]] = {}
    for row_number, row in enumerate(rows, start=1):
        if not isinstance(row, Mapping):
            raise TypeError(f"row {row_number} must be a mapping")
        arm_id = row.get("arm_id")
        if arm_id not in {treatment_arm, control_arm}:
            continue
        sample_id = _required_text(row, "sample_id", row_number)
        cluster_id = _required_text(row, "atomic_cluster_id", row_number)
        value = _extract_numeric_value(row, value_key, row_number)
        destination = treatment if arm_id == treatment_arm else control
        if sample_id in destination:
            raise ValueError(
                f"duplicate row for sample {sample_id!r} and arm {arm_id!r}"
            )
        destination[sample_id] = (cluster_id, value)

    if not treatment and not control:
        raise ValueError("no rows matched either requested arm")
    treatment_ids = set(treatment)
    control_ids = set(control)
    if treatment_ids != control_ids:
        missing_treatment = sorted(control_ids - treatment_ids)
        missing_control = sorted(treatment_ids - control_ids)
        raise ValueError(
            "paired arms contain different sample sets; "
            f"missing treatment={missing_treatment}, missing control={missing_control}"
        )
    if not treatment_ids:
        raise ValueError("paired contrast requires at least one sample")

    cluster_differences: dict[str, list[float]] = {}
    for sample_id in sorted(treatment_ids):
        treatment_cluster, treatment_value = treatment[sample_id]
        control_cluster, control_value = control[sample_id]
        if treatment_cluster != control_cluster:
            raise ValueError(
                f"sample {sample_id!r} changes atomic cluster across paired arms: "
                f"{treatment_cluster!r} != {control_cluster!r}"
            )
        cluster_differences.setdefault(treatment_cluster, []).append(
            treatment_value - control_value
        )

    clusters = sorted(cluster_differences)
    cluster_summaries = [
        (sum(cluster_differences[cluster]), len(cluster_differences[cluster]))
        for cluster in clusters
    ]
    observed = sum(total for total, _ in cluster_summaries) / sum(
        count for _, count in cluster_summaries
    )
    generator = random.Random(seed)
    bootstrap_estimates: list[float] = []
    cluster_count = len(cluster_summaries)
    for _ in range(iterations):
        sampled_total = 0.0
        sampled_count = 0
        for _ in range(cluster_count):
            total, count = cluster_summaries[generator.randrange(cluster_count)]
            sampled_total += total
            sampled_count += count
        bootstrap_estimates.append(sampled_total / sampled_count)
    return {
        "estimate": observed,
        "bootstrap_estimates": bootstrap_estimates,
        "n_samples": len(treatment_ids),
        "n_clusters": cluster_count,
    }


def _required_text(row: Mapping[str, Any], key: str, row_number: int) -> str:
    value = row.get(key)
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"row {row_number} has no non-empty {key}")
    return value


def _extract_numeric_value(
    row: Mapping[str, Any], value_key: str, row_number: int
) -> float:
    missing = object()
    current: Any = row
    if "." in value_key:
        for component in value_key.split("."):
            if not component or not isinstance(current, Mapping):
                current = missing
                break
            current = current.get(component, missing)
            if current is missing:
                break
    else:
        current = row.get(value_key, missing)
        if current is missing:
            diagnostics = row.get("diagnostics")
            if isinstance(diagnostics, Mapping):
                current = diagnostics.get(value_key, missing)
    if current is missing:
        raise ValueError(f"row {row_number} has no value for {value_key!r}")

    if value_key == "E":
        if isinstance(current, bool):
            return float(int(current))
        if not isinstance(current, int) or current not in {0, 1}:
            raise ValueError(f"row {row_number} E must be binary 0/1")
        return float(current)
    if isinstance(current, bool) or not isinstance(current, (int, float)):
        raise TypeError(f"row {row_number} value {value_key!r} must be numeric")
    value = float(current)
    if not math.isfinite(value):
        raise ValueError(f"row {row_number} value {value_key!r} must be finite")
    return value


def _quantile(ordered: Sequence[float], probability: float) -> float:
    if not ordered:
        raise ValueError("quantile requires at least one value")
    if probability <= 0.0:
        return float(ordered[0])
    if probability >= 1.0:
        return float(ordered[-1])
    position = (len(ordered) - 1) * probability
    lower_index = math.floor(position)
    upper_index = math.ceil(position)
    if lower_index == upper_index:
        return float(ordered[lower_index])
    weight = position - lower_index
    return float(
        ordered[lower_index] * (1.0 - weight) + ordered[upper_index] * weight
    )


def _derived_seed(seed: int, contrast_id: object, value_key: str) -> int:
    if isinstance(seed, bool) or not isinstance(seed, int):
        raise TypeError("seed must be an integer")
    payload = f"{seed}\x00{contrast_id}\x00{value_key}".encode("utf-8")
    return int.from_bytes(hashlib.sha256(payload).digest()[:8], "big")


__all__ = [
    "DEFAULT_BOOTSTRAP_ITERATIONS",
    "holm_adjust",
    "paired_cluster_contrast",
    "registered_contrast_specifications",
    "summarize_fresh_confirm_equivalence",
    "summarize_registered_contrasts",
]
