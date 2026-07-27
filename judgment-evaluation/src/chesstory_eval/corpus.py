from __future__ import annotations

import hashlib
import json
import re
from pathlib import Path
from typing import Any, Mapping

from .capture import ArtifactStore
from .hashing import json_bytes, read_json, read_jsonl, sha256_file, sha256_json
from .model import ContractError, IntegrityError, STAGES, Sample, SplitSealed


_RELATION_TYPES: tuple[str, ...] = (
    "game",
    "opening-lineage",
    "tactical-archetype",
    "counterfactual",
)
_RELEASE_ROLES: tuple[str, ...] = (
    "oracle-chain-author",
    "base-label-author",
    "held-out-human-explainer",
    "out-of-set-adjudicator",
    "final-usefulness-rater",
)
_CUSTODIAN_PERSON_ID_RE = re.compile(r"custodian-person:[0-9a-f]{32}")
_CUSTODIAN_ISSUER_ID_RE = re.compile(
    r"custodian:[a-z0-9](?:[a-z0-9._-]{0,62}[a-z0-9])?"
)
_CANONICAL_CHAIN_ID_RE = re.compile(
    r"[a-z0-9](?:[a-z0-9._-]{0,62}[a-z0-9])?"
)
_SHA256_RE = re.compile(r"[0-9a-f]{64}")


class Corpus:
    def __init__(self, root: Path) -> None:
        self.root = root.resolve()
        manifest_path = self.root / "manifest.json"
        manifest = read_json(manifest_path)
        if not isinstance(manifest, Mapping):
            raise ContractError("corpus manifest must be an object")
        if manifest.get("schema_version") != "chesstory.eval.corpus-manifest.v1":
            raise ContractError("unsupported corpus manifest schema_version")
        self.manifest = manifest
        self.corpus_version = self._text(manifest, "corpus_version")
        self._opened_relation_keys: dict[tuple[str, str], str] = {}
        self._verify_manifest_topology()

    def load_split(
        self,
        split: str,
        *,
        role: str = "developer",
        unlock_token: str | None = None,
        access_store: ArtifactStore | None = None,
        access_reason: str = "experiment execution",
        candidate_manifest_hash: str | None = None,
    ) -> list[Sample]:
        entry = self._split_entry(split)
        state = entry.get("state")
        if state == "sealed":
            allowed_roles = entry.get("open_roles", [])
            if role not in allowed_roles:
                raise SplitSealed(f"split {split!r} is sealed for role {role!r}")
            expected = entry.get("unlock_token_sha256")
            if not isinstance(expected, str) or unlock_token is None:
                raise SplitSealed(f"split {split!r} requires a custodian unlock token")
            actual = hashlib.sha256(unlock_token.encode("utf-8")).hexdigest()
            if not __import__("hmac").compare_digest(actual, expected):
                raise SplitSealed(f"split {split!r} unlock token is invalid")
        elif state != "open":
            raise ContractError(f"split {split!r} has unknown state {state!r}")

        path = self._split_path(entry)
        self._verify_file_entry(path, entry)
        if state == "sealed":
            if access_store is None:
                raise SplitSealed(
                    f"split {split!r} requires an append-only access store"
                )
            if entry.get("open_once_per_candidate_manifest") is True:
                if (
                    not isinstance(candidate_manifest_hash, str)
                    or len(candidate_manifest_hash) != 64
                    or any(character not in "0123456789abcdef" for character in candidate_manifest_hash)
                ):
                    raise SplitSealed(
                        f"split {split!r} requires a lowercase candidate-manifest SHA-256"
                    )
                access_store.claim_sealed_split_open(
                    role=role,
                    split_id=split,
                    split_hash=str(entry["sha256"]),
                    candidate_manifest_hash=candidate_manifest_hash,
                    reason=access_reason,
                )
            else:
                access_store.record_access(
                    role=role,
                    artifact_hash=str(entry["sha256"]),
                    action="open-sealed-split",
                    reason=access_reason,
                )
        rows = read_jsonl(path)
        expected_records = entry.get("records")
        if expected_records != len(rows):
            raise IntegrityError(
                f"split {split!r} record count mismatch: {len(rows)} != {expected_records}"
            )
        samples = [self._sample(row, split, index + 1) for index, row in enumerate(rows)]
        ids = [sample.sample_id for sample in samples]
        if len(ids) != len(set(ids)):
            raise ContractError(f"split {split!r} contains duplicate sample_id values")
        self._validate_opened_split(split, samples)
        return samples

    def embedded_actual_q_packs(
        self, samples: list[Sample] | tuple[Sample, ...]
    ) -> dict[str, Mapping[str, Any]]:
        """Extract only trusted actual-Q packs for the embedded Q adapter.

        The returned detached mapping contains no labels, raw corpus rows,
        oracle artifacts, source locators, or final outputs.  Generic stage
        adapters must never receive ``Sample.raw`` merely to locate actual Q.
        """

        packs: dict[str, Mapping[str, Any]] = {}
        for sample in samples:
            if sample.corpus_version != self.corpus_version:
                raise ContractError(
                    f"sample {sample.sample_id!r} does not belong to this corpus version"
                )
            entry = sample.raw.get("actual_q")
            if entry is None:
                continue
            if not isinstance(entry, Mapping):
                raise ContractError(
                    f"sample {sample.sample_id!r} actual_q must be an object"
                )
            try:
                detached = json.loads(json_bytes(entry))
            except (TypeError, ValueError) as error:
                raise ContractError(
                    f"sample {sample.sample_id!r} actual_q is not finite JSON"
                ) from error
            if not isinstance(detached, Mapping):
                raise ContractError(
                    f"sample {sample.sample_id!r} actual_q must remain an object"
                )
            packs[sample.sample_id] = detached
        return packs

    def split_hash(self, split: str) -> str:
        entry = self._split_entry(split)
        return str(entry["sha256"])

    def split_state(self, split: str) -> str:
        return str(self._split_entry(split).get("state"))

    def manifest_resource_paths(self, *, release: bool) -> dict[str, Path]:
        """Resolve every corpus-governance file named by the manifest.

        A release corpus must bind the preregistration, role schedule, access
        policy, reference index, annotation index, and adjudication procedure
        in addition to every split and the manifest itself.  Paths may live
        outside ``corpus/v1`` (the reference index does), so containment is
        enforced later against the frozen workspace component root.
        """

        required = [
            "preregistration",
            "role_schedule",
            "access_policy",
            "reference_index",
        ]
        if release:
            required.extend(["annotation_index", "adjudication_procedure"])
        resources: dict[str, Path] = {"manifest": self.root / "manifest.json"}
        for key in required:
            value = self.manifest.get(key)
            if not isinstance(value, str) or not value.strip():
                raise ContractError(
                    f"release corpus manifest resource {key!r} is missing"
                )
            resources[key] = self.root / value
        for split, entry in self._splits().items():
            resources[f"split:{split}"] = self._split_path(entry)
        return resources

    def role_schedule(self) -> Mapping[str, Any]:
        path = self.manifest_resource_paths(release=False)["role_schedule"]
        value = read_json(path)
        if not isinstance(value, Mapping):
            raise ContractError("role schedule must be an object")
        return value

    def release_role_assignments(
        self,
        *,
        split: str,
        minimum_held_out_raters: int,
    ) -> dict[str, dict[str, tuple[str, ...]]]:
        """Validate cluster-disjoint release roles and return frozen identities."""

        if minimum_held_out_raters <= 0:
            raise ContractError("minimum held-out rater count must be positive")
        schedule = self.role_schedule()
        if schedule.get("schema_version") != "chesstory.eval.role-schedule.v1":
            raise ContractError("unsupported role-schedule schema_version")
        if schedule.get("unit") != "atomic_cluster_id":
            raise ContractError("release role schedule must use atomic_cluster_id")
        if schedule.get("status") != "frozen-release-roster":
            raise ContractError("release role schedule is not a frozen release roster")
        identity_authority = schedule.get("identity_authority")
        if not isinstance(identity_authority, Mapping) or set(identity_authority) != {
            "scheme",
            "issuer_id",
            "registry_sha256",
            "aliases_forbidden",
        }:
            raise ContractError(
                "release role schedule has no exact custodian identity authority"
            )
        if identity_authority.get("scheme") != "custodian-issued-opaque-v1":
            raise ContractError("unsupported release identity authority scheme")
        issuer_id = identity_authority.get("issuer_id")
        if not isinstance(issuer_id, str) or not _CUSTODIAN_ISSUER_ID_RE.fullmatch(
            issuer_id
        ):
            raise ContractError("release identity issuer_id is not canonical")
        registry_sha256 = identity_authority.get("registry_sha256")
        if not isinstance(registry_sha256, str) or not _SHA256_RE.fullmatch(
            registry_sha256
        ):
            raise ContractError("release identity registry_sha256 is invalid")
        if identity_authority.get("aliases_forbidden") is not True:
            raise ContractError(
                "release identity authority must forbid aliases for one natural person"
            )
        assignments = schedule.get("assignments")
        if not isinstance(assignments, list) or not assignments:
            raise ContractError("release role schedule assignments are empty")
        declared_clusters = self.manifest.get("atomic_clusters", {}).get(split)
        if not isinstance(declared_clusters, list) or not declared_clusters:
            raise ContractError(f"release split {split!r} has no atomic clusters")
        cluster_set = set(declared_clusters)
        atomic_clusters = self.manifest.get("atomic_clusters")
        if not isinstance(atomic_clusters, Mapping):
            raise ContractError("release manifest atomic cluster map is missing")
        global_clusters: set[str] = set()
        for declared_split, values in atomic_clusters.items():
            if (
                not isinstance(declared_split, str)
                or not isinstance(values, list)
                or values != sorted(set(values))
                or any(
                    not isinstance(cluster, str)
                    or not cluster.strip()
                    or cluster != cluster.strip()
                    for cluster in values
                )
            ):
                raise ContractError("release manifest has invalid global cluster IDs")
            overlap = global_clusters & set(values)
            if overlap:
                raise ContractError(
                    f"release atomic clusters occur in multiple splits: {sorted(overlap)}"
                )
            global_clusters.update(values)
        preregistration = read_json(
            self.manifest_resource_paths(release=False)["preregistration"]
        )
        if not isinstance(preregistration, Mapping):
            raise ContractError("release preregistration must be an object")
        oracle_chains = preregistration.get("oracle_chains")
        if (
            not isinstance(oracle_chains, list)
            or len(oracle_chains) != 2
            or any(
                not isinstance(chain, str)
                or not _CANONICAL_CHAIN_ID_RE.fullmatch(chain)
                for chain in oracle_chains
            )
            or len(set(oracle_chains)) != 2
        ):
            raise ContractError(
                "release role schedule requires exactly two independent oracle chains"
            )
        if schedule.get("roles") != list(_RELEASE_ROLES):
            raise ContractError(
                "release role schedule roles must be exact, ordered, and duplicate-free"
            )
        by_cluster: dict[str, dict[str, set[str]]] = {
            cluster: {} for cluster in declared_clusters
        }
        seen_assignments: set[tuple[str, str, str]] = set()
        for index, assignment in enumerate(assignments):
            if not isinstance(assignment, Mapping):
                raise ContractError(f"role assignment {index} must be an object")
            person = self._text(assignment, "person_id")
            if not _CUSTODIAN_PERSON_ID_RE.fullmatch(person):
                raise ContractError(
                    f"role assignment {index} person_id is not custodian-issued opaque form"
                )
            role = self._text(assignment, "role")
            if role not in _RELEASE_ROLES:
                raise ContractError(f"role assignment {index} has an unknown role")
            expected_fields = {"person_id", "role", "atomic_cluster_ids"}
            if role == "oracle-chain-author":
                expected_fields.add("oracle_chain")
            if role == "final-usefulness-rater":
                expected_fields.add("evidence_tier")
            if set(assignment) != expected_fields:
                raise ContractError(
                    f"role assignment {index} fields are not exact for {role!r}"
                )
            if role == "final-usefulness-rater" and assignment.get(
                "evidence_tier"
            ) != "held-out-human":
                raise ContractError(
                    "release final-usefulness-rater must carry evidence_tier="
                    "'held-out-human'; model-proxy votes never satisfy the human gate"
                )
            oracle_chain = assignment.get("oracle_chain")
            role_key = role
            if role == "oracle-chain-author":
                if oracle_chain not in oracle_chains:
                    raise ContractError(
                        f"role assignment {index} names an unregistered oracle chain"
                    )
                role_key = f"oracle-chain-author:{oracle_chain}"
            clusters = assignment.get("atomic_cluster_ids")
            if (
                not isinstance(clusters, list)
                or not clusters
                or any(not isinstance(cluster, str) or not cluster.strip() for cluster in clusters)
                or clusters != sorted(set(clusters))
            ):
                raise ContractError(
                    f"role assignment {index} atomic_cluster_ids must be sorted and unique"
                )
            for cluster in clusters:
                if cluster not in global_clusters:
                    raise ContractError(
                        f"role assignment {index} names unknown global cluster {cluster!r}"
                    )
                if cluster not in cluster_set:
                    continue
                key = (person, role_key, cluster)
                if key in seen_assignments:
                    raise ContractError(f"duplicate release role assignment: {key!r}")
                seen_assignments.add(key)
                by_cluster[cluster].setdefault(role_key, set()).add(person)

        frozen: dict[str, dict[str, tuple[str, ...]]] = {}
        for cluster in declared_clusters:
            roles = by_cluster[cluster]
            raters = roles.get("final-usefulness-rater", set())
            adjudicators = roles.get("out-of-set-adjudicator", set())
            base_label_authors = roles.get("base-label-author", set())
            human_explainers = roles.get("held-out-human-explainer", set())
            oracle_authors_by_chain = {
                chain: roles.get(f"oracle-chain-author:{chain}", set())
                for chain in oracle_chains
            }
            if len(raters) < minimum_held_out_raters:
                raise ContractError(
                    f"cluster {cluster!r} has {len(raters)} held-out raters; "
                    f"{minimum_held_out_raters} required"
                )
            if not adjudicators:
                raise ContractError(f"cluster {cluster!r} has no out-of-set adjudicator")
            if not base_label_authors:
                raise ContractError(f"cluster {cluster!r} has no base-label author")
            if not human_explainers:
                raise ContractError(f"cluster {cluster!r} has no held-out human explainer")
            if any(not people for people in oracle_authors_by_chain.values()):
                raise ContractError(
                    f"cluster {cluster!r} has no author for every frozen oracle chain"
                )
            identity_groups = [
                *oracle_authors_by_chain.values(),
                base_label_authors,
                human_explainers,
                adjudicators,
                raters,
            ]
            for left_index, left in enumerate(identity_groups):
                for right in identity_groups[left_index + 1 :]:
                    if left & right:
                        raise ContractError(
                            f"cluster {cluster!r} reuses an identity across independent roles"
                        )
            frozen[cluster] = {
                "oracle_author_ids_by_chain": tuple(
                    f"{chain}:{person}"
                    for chain in oracle_chains
                    for person in sorted(oracle_authors_by_chain[chain])
                ),
                "base_label_author_ids": tuple(sorted(base_label_authors)),
                "held_out_human_explainer_ids": tuple(sorted(human_explainers)),
                "held_out_rater_ids": tuple(sorted(raters)),
                "adjudicator_ids": tuple(sorted(adjudicators)),
            }
        return frozen

    def verify_open_splits(self) -> None:
        for split, entry in self._splits().items():
            if entry.get("state") == "open":
                path = self._split_path(entry)
                self._verify_file_entry(path, entry)
                self.load_split(split)

    def _sample(self, row: Any, split: str, line_number: int) -> Sample:
        if not isinstance(row, Mapping):
            raise ContractError(f"{split}:{line_number}: sample must be an object")
        required = {
            "schema_version",
            "sample_id",
            "atomic_cluster_id",
            "strata",
            "request",
            "labels",
            "stage_availability",
        }
        missing = sorted(required - set(row))
        if missing:
            raise ContractError(f"{split}:{line_number}: missing fields {missing}")
        if row.get("schema_version") != "chesstory.eval.sample.v1":
            raise ContractError(f"{split}:{line_number}: unsupported sample schema")
        request = row.get("request")
        labels = row.get("labels")
        if not isinstance(request, Mapping) or not isinstance(labels, Mapping):
            raise ContractError(f"{split}:{line_number}: request/labels must be objects")
        self._validate_label_order(labels, split, line_number)
        strata = row.get("strata")
        if not isinstance(strata, Mapping):
            raise ContractError(f"{split}:{line_number}: strata must be an object")
        availability = self._validate_stage_availability(row, split, line_number)
        relations = row.get("relations")
        if relations is not None and not isinstance(relations, Mapping):
            raise ContractError(f"{split}:{line_number}: relations must be an object")
        metadata = {
            "strata": strata,
            "source_locator": row.get("source_locator"),
            "stage_availability": availability,
            "counterfactual_group": row.get("counterfactual_group"),
            "relations": relations,
        }
        return Sample(
            sample_id=self._text(row, "sample_id"),
            atomic_cluster_id=self._text(row, "atomic_cluster_id"),
            corpus_version=self.corpus_version,
            split=split,
            request=request,
            labels=labels,
            metadata=metadata,
            raw=row,
        )

    @staticmethod
    def _validate_stage_availability(
        row: Mapping[str, Any], split: str, line_number: int
    ) -> Mapping[str, Any]:
        availability = row.get("stage_availability")
        if not isinstance(availability, Mapping):
            raise ContractError(
                f"{split}:{line_number}: stage_availability must be an object"
            )
        if set(availability) != set(STAGES):
            raise ContractError(
                f"{split}:{line_number}: stage_availability must declare exactly {STAGES}"
            )
        for stage in STAGES:
            entry = availability.get(stage)
            if not isinstance(entry, Mapping) or set(entry) != {"status", "reason"}:
                raise ContractError(
                    f"{split}:{line_number}: {stage} availability requires status/reason only"
                )
            status = entry.get("status")
            reason = entry.get("reason")
            if status not in {"available", "unavailable", "external"}:
                raise ContractError(
                    f"{split}:{line_number}: {stage} availability status is invalid"
                )
            if not isinstance(reason, str) or not reason.strip():
                raise ContractError(
                    f"{split}:{line_number}: {stage} availability reason is required"
                )
        return availability

    @staticmethod
    def _validate_label_order(labels: Mapping[str, Any], split: str, line_number: int) -> None:
        order = [
            "comparison_moves",
            "cause_and_effect",
            "bindings",
            "required_pv",
            "claims",
            "confidence_and_alternatives",
            "language",
            "natural_language",
        ]
        present = [key for key in order if key in labels]
        if present and list(labels.keys())[: len(present)] != present:
            raise ContractError(
                f"{split}:{line_number}: label fields must follow boundary annotation order"
            )
        answerable = labels.get("answerable")
        if answerable not in {True, False, None}:
            raise ContractError(f"{split}:{line_number}: answerable must be boolean or null")

    def _verify_manifest_topology(self) -> None:
        splits = self._splits()
        expected = {
            "diagnostic-explore",
            "diagnostic-confirm",
            "fresh-confirm",
            "blind",
        }
        if set(splits) != expected:
            raise ContractError(f"corpus splits must be exactly {sorted(expected)}")
        seen: dict[str, str] = {}
        clusters = self.manifest.get("atomic_clusters")
        if not isinstance(clusters, Mapping):
            raise ContractError("manifest atomic_clusters must be an object")
        if set(clusters) != set(splits):
            raise ContractError("manifest atomic_clusters must name every split exactly")
        for split, cluster_ids in clusters.items():
            if split not in splits or not isinstance(cluster_ids, list):
                raise ContractError("invalid atomic_clusters split mapping")
            if (
                not cluster_ids
                or any(not isinstance(cluster, str) or not cluster.strip() for cluster in cluster_ids)
                or len(cluster_ids) != len(set(cluster_ids))
            ):
                raise ContractError(
                    f"manifest atomic_clusters[{split!r}] must be unique non-empty text"
                )
            for cluster in cluster_ids:
                if cluster in seen:
                    raise ContractError(
                        f"atomic cluster {cluster!r} leaks across {seen[cluster]!r} and {split!r}"
                    )
                seen[str(cluster)] = split

        closure = self.manifest.get("cluster_relation_closure")
        if closure != list(_RELATION_TYPES):
            raise ContractError(
                "cluster_relation_closure must freeze game, opening-lineage, "
                "tactical-archetype, and counterfactual in that order"
            )
        relation_manifest = self.manifest.get("relation_key_sha256")
        if not isinstance(relation_manifest, Mapping) or set(relation_manifest) != set(
            _RELATION_TYPES
        ):
            raise ContractError("manifest relation_key_sha256 is incomplete")
        for relation_type in _RELATION_TYPES:
            split_mapping = relation_manifest.get(relation_type)
            if not isinstance(split_mapping, Mapping) or set(split_mapping) != set(splits):
                raise ContractError(
                    f"manifest relation hashes for {relation_type!r} must name every split"
                )
            owners: dict[str, str] = {}
            for split, digests in split_mapping.items():
                if (
                    not isinstance(digests, list)
                    or len(digests) != len(set(digests))
                    or any(
                        not isinstance(digest, str)
                        or len(digest) != 64
                        or any(character not in "0123456789abcdef" for character in digest)
                        for digest in digests
                    )
                ):
                    raise ContractError(
                        f"manifest relation hashes for {relation_type!r}/{split!r} are invalid"
                    )
                for digest in digests:
                    if digest in owners:
                        raise ContractError(
                            f"{relation_type} relation leaks across "
                            f"{owners[digest]!r} and {split!r}"
                        )
                    owners[digest] = str(split)

    def _validate_opened_split(self, split: str, samples: list[Sample]) -> None:
        clusters = self.manifest["atomic_clusters"]
        declared_clusters = clusters.get(split) if isinstance(clusters, Mapping) else None
        if not isinstance(declared_clusters, list):
            raise ContractError(f"manifest has no atomic cluster list for {split!r}")
        row_clusters = {sample.atomic_cluster_id for sample in samples}
        if row_clusters != set(declared_clusters):
            raise IntegrityError(
                f"split {split!r} row clusters do not exactly match its manifest list"
            )

        actual: dict[str, dict[str, set[str]]] = {
            relation_type: {} for relation_type in _RELATION_TYPES
        }
        for sample in samples:
            for relation_type, values in self._relation_values(sample).items():
                for value in values:
                    digest = self._relation_digest(relation_type, value)
                    actual[relation_type].setdefault(digest, set()).add(
                        sample.atomic_cluster_id
                    )
        for relation_type, relation_keys in actual.items():
            for digest, related_clusters in relation_keys.items():
                if len(related_clusters) != 1:
                    raise ContractError(
                        f"split {split!r} breaks {relation_type!r} transitive closure "
                        f"across clusters {sorted(related_clusters)}"
                    )
            declared = self.manifest["relation_key_sha256"][relation_type][split]
            if set(relation_keys) != set(declared):
                raise IntegrityError(
                    f"split {split!r} {relation_type!r} row relation keys do not "
                    "exactly match the manifest"
                )
            for digest in relation_keys:
                key = relation_type, digest
                previous_split = self._opened_relation_keys.get(key)
                if previous_split is not None and previous_split != split:
                    raise ContractError(
                        f"opened split relation leakage: {relation_type!r} is shared by "
                        f"{previous_split!r} and {split!r}"
                    )
                self._opened_relation_keys[key] = split

    @staticmethod
    def _relation_values(sample: Sample) -> dict[str, set[str]]:
        values: dict[str, set[str]] = {relation_type: set() for relation_type in _RELATION_TYPES}
        strata = sample.metadata.get("strata")
        if isinstance(strata, Mapping):
            lineage = strata.get("lineage")
            motif = strata.get("motif")
            if lineage is not None:
                values["opening-lineage"].add(
                    Corpus._relation_text(lineage, "strata.lineage")
                )
            if motif is not None:
                values["tactical-archetype"].add(
                    Corpus._relation_text(motif, "strata.motif")
                )
        counterfactual = sample.metadata.get("counterfactual_group")
        if counterfactual is not None:
            values["counterfactual"].add(
                Corpus._relation_text(counterfactual, "counterfactual_group")
            )

        explicit = sample.metadata.get("relations")
        if explicit is not None:
            if not isinstance(explicit, Mapping) or not set(explicit).issubset(
                _RELATION_TYPES
            ):
                raise ContractError(
                    f"sample {sample.sample_id!r} has unsupported relation metadata"
                )
            for relation_type, supplied in explicit.items():
                candidates = supplied if isinstance(supplied, list) else [supplied]
                if not candidates:
                    raise ContractError(
                        f"sample {sample.sample_id!r} relation {relation_type!r} is empty"
                    )
                for candidate in candidates:
                    values[str(relation_type)].add(
                        Corpus._relation_text(
                            candidate,
                            f"relations.{relation_type}",
                        )
                    )
        return values

    @staticmethod
    def _relation_text(value: Any, location: str) -> str:
        if not isinstance(value, str) or not value.strip() or value != value.strip():
            raise ContractError(f"{location} relation IDs must be trimmed non-empty text")
        return value

    @staticmethod
    def _relation_digest(relation_type: str, value: str) -> str:
        return sha256_json({"relation_type": relation_type, "value": value})

    def _splits(self) -> Mapping[str, Mapping[str, Any]]:
        splits = self.manifest.get("splits")
        if not isinstance(splits, Mapping):
            raise ContractError("manifest splits must be an object")
        return splits  # type: ignore[return-value]

    def _split_entry(self, split: str) -> Mapping[str, Any]:
        entry = self._splits().get(split)
        if not isinstance(entry, Mapping):
            raise ContractError(f"unknown corpus split: {split!r}")
        return entry

    def _split_path(self, entry: Mapping[str, Any]) -> Path:
        relative = entry.get("path")
        if not isinstance(relative, str):
            raise ContractError("split path must be a string")
        path = (self.root / relative).resolve()
        if self.root not in path.parents:
            raise ContractError(f"split path escapes corpus root: {relative}")
        return path

    @staticmethod
    def _verify_file_entry(path: Path, entry: Mapping[str, Any]) -> None:
        if not path.is_file():
            raise IntegrityError(f"corpus split file is missing: {path}")
        expected = entry.get("sha256")
        if not isinstance(expected, str) or len(expected) != 64:
            raise ContractError(f"split has invalid sha256: {path}")
        actual = sha256_file(path)
        if actual != expected:
            raise IntegrityError(f"corpus split seal mismatch: {path}")

    @staticmethod
    def _text(mapping: Mapping[str, Any], key: str) -> str:
        value = mapping.get(key)
        if not isinstance(value, str) or not value.strip():
            raise ContractError(f"{key} must be non-empty text")
        return value
