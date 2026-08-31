from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Mapping

STAGES: tuple[str, ...] = ("Q", "F", "C", "Jp", "Ja", "R", "P")


class HarnessError(RuntimeError):
    """Base error for a contract or experiment failure."""


class ContractError(HarnessError):
    """A stage document or corpus record violates a frozen contract."""


class IntegrityError(HarnessError):
    """A hash, seal, append-only chain, or lineage check failed."""


class StageUnavailable(HarnessError):
    def __init__(self, stage: str, reason: str) -> None:
        super().__init__(f"{stage}: {reason}")
        self.stage = stage
        self.reason = reason


class SplitSealed(HarnessError):
    """The requested split is sealed for the current role/run."""


class Source(str, Enum):
    ACTUAL = "actual"
    ORACLE = "oracle"
    FORMAT_CONTROL = "format-control"


@dataclass(frozen=True)
class Producer:
    component: str
    version: str
    configuration_hash: str | None = None
    build_hash: str | None = None

    def as_dict(self) -> dict[str, str]:
        result = {"component": self.component, "version": self.version}
        if self.configuration_hash is not None:
            result["configuration_hash"] = self.configuration_hash
        if self.build_hash is not None:
            result["build_hash"] = self.build_hash
        return result


@dataclass(frozen=True)
class Sample:
    sample_id: str
    atomic_cluster_id: str
    corpus_version: str
    split: str
    request: Mapping[str, Any]
    labels: Mapping[str, Any]
    metadata: Mapping[str, Any]
    raw: Mapping[str, Any]


@dataclass(frozen=True)
class StageAvailability:
    """A single fail-closed actual-stage availability declaration.

    This small immutable value is safe to expose to an in-process stage
    adapter.  In particular, it contains neither corpus labels nor embedded
    actual/oracle/final artifacts.
    """

    stage: str
    status: str
    reason: str

    def __post_init__(self) -> None:
        require_stage(self.stage)
        if self.status not in {"available", "unavailable", "external"}:
            raise ContractError(
                f"{self.stage} availability has invalid status {self.status!r}"
            )
        if not isinstance(self.reason, str) or not self.reason.strip():
            raise ContractError(f"{self.stage} availability requires a reason")


@dataclass(frozen=True)
class StageSampleView:
    """The only sample information visible through the stage-adapter API.

    Stage producers receive their semantic input in ``StageInvocation``'s
    validated input document.  This view carries only routing identifiers and
    actual-stage availability metadata.  It deliberately has no ``labels``,
    ``raw``, source locator, expert annotation, embedded acquisition pack, or
    previously captured final output field.
    """

    sample_id: str
    atomic_cluster_id: str
    corpus_version: str
    split: str
    stage_availability: tuple[StageAvailability, ...]

    def __post_init__(self) -> None:
        for name, value in (
            ("sample_id", self.sample_id),
            ("atomic_cluster_id", self.atomic_cluster_id),
            ("corpus_version", self.corpus_version),
            ("split", self.split),
        ):
            if not isinstance(value, str) or not value.strip():
                raise ContractError(f"adapter sample view {name} must be non-empty text")
        if not isinstance(self.stage_availability, tuple):
            raise ContractError("adapter sample view availability must be immutable")
        stages = tuple(entry.stage for entry in self.stage_availability)
        if stages != STAGES:
            raise ContractError(
                "adapter sample view must declare actual-stage availability "
                f"exactly once in stage order {STAGES}"
            )

    @classmethod
    def from_sample(cls, sample: Sample) -> "StageSampleView":
        availability = sample.metadata.get("stage_availability")
        if not isinstance(availability, Mapping):
            raise ContractError(
                f"sample {sample.sample_id!r} has no actual-stage availability mapping"
            )
        entries: list[StageAvailability] = []
        if set(availability) != set(STAGES):
            raise ContractError(
                f"sample {sample.sample_id!r} must declare availability for every stage"
            )
        for stage in STAGES:
            value = availability.get(stage)
            if not isinstance(value, Mapping):
                raise ContractError(
                    f"sample {sample.sample_id!r} {stage} availability must be an object"
                )
            status = value.get("status")
            reason = value.get("reason")
            if not isinstance(status, str) or not isinstance(reason, str):
                raise ContractError(
                    f"sample {sample.sample_id!r} {stage} availability is malformed"
                )
            entries.append(StageAvailability(stage=stage, status=status, reason=reason))
        return cls(
            sample_id=sample.sample_id,
            atomic_cluster_id=sample.atomic_cluster_id,
            corpus_version=sample.corpus_version,
            split=sample.split,
            stage_availability=tuple(entries),
        )

    def availability_for(self, stage: str) -> StageAvailability:
        registered = require_stage(stage)
        for entry in self.stage_availability:
            if entry.stage == registered:
                return entry
        # ``__post_init__`` makes this unreachable, but adapters fail closed if
        # an object is forged or deserialized without normal construction.
        raise ContractError(f"actual-stage availability is absent for {registered}")


@dataclass(frozen=True)
class RunContext:
    run_id: str
    seed: int
    corpus_version: str
    split: str
    artifact_root: Path
    stage_context: Mapping[str, Any]
    candidate_manifest_hash: str | None = None


@dataclass(frozen=True)
class Arm:
    arm_id: str
    sources: Mapping[str, Source]
    oracle_chain: str | None
    focus_stage: str | None
    context: str

    def source_for(self, stage: str) -> Source:
        return self.sources.get(stage, Source.ACTUAL)


@dataclass(frozen=True)
class EndpointResult:
    value: int
    conditions: Mapping[str, bool]
    diagnostics: Mapping[str, float | int | None]
    evaluator_artifact_hash: str


def require_stage(stage: str) -> str:
    if stage not in STAGES:
        raise ContractError(f"unknown stage: {stage!r}")
    return stage
