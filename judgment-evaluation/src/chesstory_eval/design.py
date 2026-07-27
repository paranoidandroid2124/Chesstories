from __future__ import annotations

from collections.abc import Callable, Iterable, Sequence
from itertools import product

from .model import STAGES, Arm, Source


ALL_ACTUAL_CONTEXT = "all-actual"
BASELINE_CONTEXT = ALL_ACTUAL_CONTEXT
STABLE_Q_CONTEXT = "stable-q"
ALL_ORACLE_CONTEXT = "all-oracle"
ONE_STAGE_CONTEXT = "one-stage"
LEAVE_ONE_ACTUAL_CONTEXT = "leave-one-actual"
FORWARD_CUMULATIVE_CONTEXT = "forward-cumulative"
BACKWARD_CUMULATIVE_CONTEXT = "backward-cumulative"
FACTORIAL_CONTEXT_PREFIX = "adjacent-factorial"

INTERVENTION_CONTEXTS: tuple[str, ...] = (
    STABLE_Q_CONTEXT,
    ONE_STAGE_CONTEXT,
    LEAVE_ONE_ACTUAL_CONTEXT,
    FORWARD_CUMULATIVE_CONTEXT,
    BACKWARD_CUMULATIVE_CONTEXT,
)

_VARIANT_SOURCES: tuple[Source, ...] = (
    Source.ACTUAL,
    Source.FORMAT_CONTROL,
    Source.ORACLE,
)


def build_arm_plan(
    oracle_chains: Sequence[str] = ("team-a", "team-b"),
) -> list[Arm]:
    """Build the frozen, source-blind intervention plan.

    The returned order and opaque ``arm-####`` identifiers are deterministic.
    Source choices and coherent-oracle-chain names live only in :class:`Arm`
    metadata; neither is encoded in an arm identifier.

    Every focused intervention is represented by an actual, format-control,
    and oracle triplet under identical surrounding stages.  Adjacent factorial
    blocks contain the registered actual/oracle 2x2 plus the four cells needed
    to format-control either focus while the other factor stays actual/oracle.

    Exactly two chains are required because they are fixed, independent
    sensitivity analyses, not exchangeable observations to pool.
    """

    chains = _validate_oracle_chains(oracle_chains)
    specifications: list[tuple[dict[str, Source], str | None, str | None, str]] = []

    def register(
        sources: dict[str, Source],
        *,
        chain: str | None,
        focus_stage: str | None,
        context: str,
    ) -> None:
        specifications.append(
            (_complete_sources(sources), chain, focus_stage, context)
        )

    register(
        _uniform_sources(Source.ACTUAL),
        chain=None,
        focus_stage=None,
        context=BASELINE_CONTEXT,
    )

    # Q acquisition is a separately registered foundation comparison.  The
    # per-chain controls keep the downstream conditioning identical to the
    # richer/stable-Q artifact for that coherent chain.
    for chain in chains:
        for variant in _VARIANT_SOURCES:
            sources = _uniform_sources(Source.ACTUAL)
            sources["Q"] = variant
            register(
                sources,
                chain=chain,
                focus_stage="Q",
                context=STABLE_Q_CONTEXT,
            )

    for chain in chains:
        register(
            _uniform_sources(Source.ORACLE),
            chain=chain,
            focus_stage=None,
            context=ALL_ORACLE_CONTEXT,
        )

    for chain in chains:
        for stage in STAGES:
            _register_triplet(
                register,
                base=_uniform_sources(Source.ACTUAL),
                focus_stage=stage,
                chain=chain,
                context=ONE_STAGE_CONTEXT,
            )

    for chain in chains:
        for stage in STAGES:
            _register_triplet(
                register,
                base=_uniform_sources(Source.ORACLE),
                focus_stage=stage,
                chain=chain,
                context=LEAVE_ONE_ACTUAL_CONTEXT,
            )

    # At a cumulative boundary, stages before the focus are oracle and stages
    # after it are actual.  The focus triplet is the boundary intervention.
    for chain in chains:
        for index, stage in enumerate(STAGES):
            base = {
                candidate: (
                    Source.ORACLE if candidate_index < index else Source.ACTUAL
                )
                for candidate_index, candidate in enumerate(STAGES)
            }
            _register_triplet(
                register,
                base=base,
                focus_stage=stage,
                chain=chain,
                context=FORWARD_CUMULATIVE_CONTEXT,
            )

    # Backward substitution crosses the same boundaries in the reverse
    # preregistered order.  It remains a distinct context because conditioned
    # replay artifacts and conclusions must not be silently shared by order.
    for chain in chains:
        for index in range(len(STAGES) - 1, -1, -1):
            stage = STAGES[index]
            base = {
                candidate: (
                    Source.ORACLE if candidate_index < index else Source.ACTUAL
                )
                for candidate_index, candidate in enumerate(STAGES)
            }
            _register_triplet(
                register,
                base=base,
                focus_stage=stage,
                chain=chain,
                context=BACKWARD_CUMULATIVE_CONTEXT,
            )

    # These eight cells contain the actual/oracle 2x2 and all Phi/Gamma
    # controls with the other factor held actual/oracle.  A double-format cell
    # has no registered contrast and is intentionally not executed.
    for chain in chains:
        for left, right in zip(STAGES, STAGES[1:]):
            context = factorial_context(left, right)
            for left_source, right_source in product(_VARIANT_SOURCES, repeat=2):
                if (
                    left_source == Source.FORMAT_CONTROL
                    and right_source == Source.FORMAT_CONTROL
                ):
                    continue
                sources = _uniform_sources(Source.ACTUAL)
                sources[left] = left_source
                sources[right] = right_source
                register(
                    sources,
                    chain=chain,
                    focus_stage=None,
                    context=context,
                )

    arms = [
        Arm(
            arm_id=f"arm-{index:04d}",
            sources=sources,
            oracle_chain=chain,
            focus_stage=focus_stage,
            context=context,
        )
        for index, (sources, chain, focus_stage, context) in enumerate(
            specifications, start=1
        )
    ]
    _validate_built_plan(arms, chains)
    return arms


def factorial_context(left_stage: str, right_stage: str) -> str:
    """Return the stable context key for one adjacent interface."""

    if left_stage not in STAGES or right_stage not in STAGES:
        raise ValueError("factorial stages must be registered stages")
    if STAGES.index(right_stage) != STAGES.index(left_stage) + 1:
        raise ValueError(
            f"factorial stages must be adjacent and ordered: {left_stage}, {right_stage}"
        )
    return f"{FACTORIAL_CONTEXT_PREFIX}:{left_stage}:{right_stage}"


def parse_factorial_context(context: str) -> tuple[str, str] | None:
    """Parse a factorial key, returning ``None`` for a different context."""

    if not isinstance(context, str):
        raise TypeError("context must be text")
    prefix = f"{FACTORIAL_CONTEXT_PREFIX}:"
    if not context.startswith(prefix):
        return None
    parts = context.split(":")
    if len(parts) != 3:
        raise ValueError(f"malformed adjacent-factorial context: {context!r}")
    left, right = parts[1], parts[2]
    expected = factorial_context(left, right)
    if context != expected:
        raise ValueError(f"malformed adjacent-factorial context: {context!r}")
    return left, right


def _register_triplet(
    register: Callable[..., None],
    *,
    base: dict[str, Source],
    focus_stage: str,
    chain: str,
    context: str,
) -> None:
    for variant in _VARIANT_SOURCES:
        sources = dict(base)
        sources[focus_stage] = variant
        register(
            sources,
            chain=chain,
            focus_stage=focus_stage,
            context=context,
        )


def _uniform_sources(source: Source) -> dict[str, Source]:
    return {stage: source for stage in STAGES}


def _complete_sources(sources: dict[str, Source]) -> dict[str, Source]:
    if set(sources) != set(STAGES):
        missing = sorted(set(STAGES) - set(sources))
        extra = sorted(set(sources) - set(STAGES))
        raise ValueError(f"arm source map is incomplete; missing={missing}, extra={extra}")
    completed: dict[str, Source] = {}
    for stage in STAGES:
        source = sources[stage]
        if not isinstance(source, Source):
            raise TypeError(f"source for {stage} must be a Source")
        completed[stage] = source
    return completed


def _validate_oracle_chains(chains: Sequence[str]) -> tuple[str, str]:
    if isinstance(chains, (str, bytes)):
        raise TypeError("oracle_chains must be a sequence of two names")
    values = tuple(chains)
    if len(values) != 2:
        raise ValueError("exactly two independent oracle chains are required")
    if any(not isinstance(value, str) or not value.strip() for value in values):
        raise ValueError("oracle chain names must be non-empty text")
    if len(set(values)) != len(values):
        raise ValueError("oracle chain names must be distinct")
    return values[0], values[1]


def _validate_built_plan(arms: Iterable[Arm], chains: tuple[str, str]) -> None:
    values = tuple(arms)
    identifiers = [arm.arm_id for arm in values]
    if len(identifiers) != len(set(identifiers)):
        raise AssertionError("arm identifiers are not unique")
    all_oracle = [arm for arm in values if arm.context == ALL_ORACLE_CONTEXT]
    if {arm.oracle_chain for arm in all_oracle} != set(chains):
        raise AssertionError("all-oracle ceiling does not contain both fixed chains")
    for arm in values:
        _complete_sources(dict(arm.sources))
        if Source.ORACLE in arm.sources.values() and arm.oracle_chain not in chains:
            raise AssertionError(
                f"oracle arm {arm.arm_id} has no registered coherent chain"
            )


__all__ = [
    "ALL_ACTUAL_CONTEXT",
    "ALL_ORACLE_CONTEXT",
    "BACKWARD_CUMULATIVE_CONTEXT",
    "BASELINE_CONTEXT",
    "FACTORIAL_CONTEXT_PREFIX",
    "FORWARD_CUMULATIVE_CONTEXT",
    "INTERVENTION_CONTEXTS",
    "LEAVE_ONE_ACTUAL_CONTEXT",
    "ONE_STAGE_CONTEXT",
    "STABLE_Q_CONTEXT",
    "build_arm_plan",
    "factorial_context",
    "parse_factorial_context",
]
