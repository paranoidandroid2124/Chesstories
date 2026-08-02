from __future__ import annotations

import base64
import copy
import io
import json
import unittest
from pathlib import Path
from unittest.mock import patch

from chesstory_eval.cause_audit import (
    ACTUAL_VIEW_SCHEMA_VERSION,
    TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
    TYPED_RUNTIME_OBSERVATION_SCHEMA,
    _actual_view,
    _strict_actual_view,
    run_cause_audit,
)
from chesstory_eval.hashing import sha256_bytes, sha256_json
from chesstory_eval.model import ContractError, IntegrityError
from chesstory_eval.native_diagnostic import (
    NATIVE_HASH_CONTRACT,
    native_canonical_json,
    native_sha256_json,
)
from chesstory_eval.probe_completion import (
    _RuntimeExchange,
    _RuntimeJsonlSession,
    _claim_content_trace,
    _runtime_io_document,
    _semantic_checkpoints,
    _validate_runtime_observation,
    _verified_response_jsonl_bytes,
)
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
CAUSE_V3_SCHEMA = ROOT / "schemas" / "cause-audit-v3" / "runtime-observation.schema.json"


def _empty_importance_observation() -> dict[str, object]:
    native = {
        "relation_policy_version": "chesstory.direct-cause-importance.relation.v3",
        "ordering_policy_version": (
            "chesstory.player-facing-idea-ordering.dominance-layers.v1"
        ),
        "selected_cause_ids": [],
        "frontier_cause_ids": [],
        "dominated_within_domain_cause_ids": [],
        "unique_top": None,
        "profiles": [],
        "relations": [],
        "decisions": [],
    }
    public = {
        "authority": "root_owned_effect_partial_order",
        "relation_policy_version": "chesstory.direct-cause-importance.relation.v3",
        "ordering_policy_version": (
            "chesstory.player-facing-idea-ordering.dominance-layers.v1"
        ),
        "comparison_exposure_rank_meaning": "comparison_exposure_authority",
        "selected_cause_ids": [],
        "frontier_cause_ids": [],
        "dominated_within_domain_cause_ids": [],
        "unique_top": None,
        "profile_summary": {
            "measured_channels": 0,
            "measured_causes": 0,
            "fully_measured_causes": 0,
            "unmeasured_causes": 0,
        },
        "profiles": [],
        "relations": [],
        "decisions": [],
        "relation_summary": {
            "comparable_profile_pairs": 0,
            "incomparable_profile_pairs": 0,
        },
        "unmeasured": [],
    }
    return {
        "r_native": native,
        "packet_native": None,
        "r_public_projection": public,
        "packet_public_projection": None,
        "public_projection": None,
    }


def _empty_cause_disposition_observation() -> dict[str, object]:
    summary = {
        "authority": "cause_disposition_ledger.v1",
        "total_cause_count": 0,
        "selected_cause_ids": [],
        "status_counts": {
            "selected": 0,
            "dominated": 0,
            "redundant": 0,
            "diagnostic": 0,
            "inferior": 0,
            "admission_deferred": 0,
            "rejected": 0,
            "unproposed": 0,
            "object_unready": 0,
        },
        "reason_counts": {
            "player_facing_selection": 0,
            "dominated_fallback": 0,
            "cross_comparison_redundancy": 0,
            "certified_claim_deduplicated": 0,
            "diagnostic_comparison": 0,
            "inferior_alternative": 0,
            "claim_admission_deferred": 0,
            "claim_admission_rejected": 0,
            "no_claim_proposal": 0,
            "object_readiness_failed": 0,
        },
        "abstention_codes": ["no_relative_cause_generated"],
    }
    return {
        "r_native": [],
        "packet_native": None,
        "r_public_summary": summary,
        "packet_public_summary": None,
        "selected_projection": None,
        "final_public_response": None,
    }


def _valid_active_cause_observation(
    request: dict[str, object],
) -> dict[str, object]:
    importance = _empty_importance_observation()
    return {
        "schema_version": TYPED_RUNTIME_OBSERVATION_SCHEMA,
        "status": "unavailable",
        "reason": "runtime_boundary_execution_absent",
        "request_sha256": native_sha256_json(request),
        "hash_contract": NATIVE_HASH_CONTRACT,
        "runtime": {
            "adapter_name": "chesstory-cause-audit-runtime-adapter",
            "adapter_version": "0.3.2",
            "request_schema": "chesstory.move-meaning.request.v1",
            "response_schema": "chesstory.move-meaning.response.v3",
        },
        "public_response": {
            "http_status": 400,
            "body_sha256": "9" * 64,
            "schema_version": "chesstory.move-meaning.response.v3",
            "request_id": request["request_id"],
            "ok": False,
            "status": "error",
            "availability": None,
            "error": "unsupported_schema_version",
            "primary_engine_backed": False,
            "idea_status": "unavailable",
            "idea_status_detail": None,
        },
        "boundary": {
            "c_reached": False,
            "jp_reached": False,
            "ja_reached": False,
            "r_reached": False,
            "packet_present": False,
            "p_reached": False,
            "cause_count": 0,
        },
        "projection": {"present": False, "packet_present": False},
        "probe_request_count": 0,
        "probe_requests": [],
        "comparison_endpoint_evidence_snapshots": [],
        "causes": [],
        "verdict": {
            "packet_canonical": None,
            "selected_projection": None,
            "final_public_response": None,
            "classification_authority": None,
        },
        "r_native_cause_selections": [],
        "importance": importance,
        "idea_units": {
            "r_native": [],
            "packet_native": None,
            "public_projection": None,
            "public_item_links": [],
            "public_item_membership_closed": True,
            "raw_public_idea_unit_count": 0,
            "parsed_public_idea_unit_count": 0,
            "public_idea_units_parse_closed": True,
        },
        "idea_importance": copy.deepcopy(importance),
        "cause_disposition_ledger": _empty_cause_disposition_observation(),
    }


def _valid_complete_active_cause_observation(
    request: dict[str, object],
) -> dict[str, object]:
    observation = _valid_active_cause_observation(request)
    observation["status"] = "complete"
    observation.pop("reason")
    observation["boundary"]["cause_kind_counts"] = {}  # type: ignore[index]
    observation["projection"] = {
        "present": False,
        "packet_present": False,
        "selected_public_cause_selections": [],
        "selected_public_cause_evidence_ids": [],
        "raw_public_idea_count": 0,
        "parsed_public_idea_count": 0,
        "public_ideas_parse_closed": True,
    }
    observation["root_owned_causal_episode_inventory"] = [
        {
            "source_id": "line:played",
            "line": {
                "id": "line:played",
                "root_move": "e2e4",
                "rank": 0,
                "role": "played",
            },
            "actor": {
                "move": "e2e4",
                "role": "root_move",
                "color": "white",
                "from": "e2",
                "to": "e4",
            },
            "target": "center",
            "mechanisms": ["space_gain"],
            "consequence": "future_recovery",
            "event_move": None,
            "event_ply_offset": 2,
            "chain_moves": ["e7e5"],
            "forcing_tactical_resource": False,
        }
    ]
    observation["only_move_constraints"] = []
    return observation


def _validate_active_cause_observation(
    request: dict[str, object], observation: dict[str, object]
) -> dict[str, object]:
    return _validate_runtime_observation(
        observation,
        expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
        registry=SchemaRegistry(CAUSE_V3_SCHEMA.parent),
        schema_path=CAUSE_V3_SCHEMA,
        expected_request_sha256=native_sha256_json(request),
        expected_hash_contract=NATIVE_HASH_CONTRACT,
    )


class ActiveCauseContractTest(unittest.TestCase):
    def test_v3_effect_scope_requires_the_plan_result_semantic_key_contract(self) -> None:
        definitions = (
            (
                ROOT / "schemas" / "public-v3" / "move-meaning-response.schema.json",
                "effectScope",
            ),
            (
                ROOT / "schemas" / "cause-audit-v3" / "actual-cascade-view.schema.json",
                "importanceEffectScope",
            ),
        )
        non_plan = {
            "primitive_kind": "structural_transition",
            "target_signatures": ["square:e5"],
            "plan_ids": [],
            "plan_result_semantic_key": None,
            "strategic_axes": [],
        }
        plan_result = {
            **non_plan,
            "primitive_kind": "plan_result",
            "plan_ids": ["plan-annotation"],
            "plan_result_semantic_key": "exact-plan-result",
        }
        invalid = (
            {key: value for key, value in non_plan.items() if key != "plan_result_semantic_key"},
            {**non_plan, "plan_result_semantic_key": 7},
            {**plan_result, "plan_result_semantic_key": ""},
            {**non_plan, "plan_result_semantic_key": "exact-plan-result"},
            {**plan_result, "plan_result_semantic_key": None},
        )
        for schema_path, definition_name in definitions:
            registry = SchemaRegistry(schema_path.parent)
            definition = registry.load(schema_path)["$defs"][definition_name]
            with self.subTest(schema=definition_name):
                self.assertEqual(registry._branch_errors(non_plan, definition, schema_path, "$"), [])
                self.assertEqual(registry._branch_errors(plan_result, definition, schema_path, "$"), [])
                for malformed in invalid:
                    self.assertTrue(registry._branch_errors(malformed, definition, schema_path, "$"))

    def test_v3_observations_schema_accepts_old_and_active_contracts(self) -> None:
        schema = ROOT / "schemas" / "public-v3" / "move-meaning-response.schema.json"
        registry = SchemaRegistry(schema.parent)
        definitions = registry.load(schema)["$defs"]

        def ref(
            identifier: str, producer: str, layer: str, line: object = None,
            scope: str = "current_position",
        ) -> dict[str, object]:
            return {"id": identifier, "producer": producer, "layer": layer, "scope": scope, "line": line}

        line = {"id": "line:played", "root_move": "d2d4", "role": "played", "rank": 0}
        source = ref("source:strategic", "strategic_feature_producer", "strategic")
        candidate_carrier = ref(
            "carrier:candidate", "relative_move_producer", "candidate_comparison", line, "counterfactual"
        )
        candidate_line = {"root_move": "d2d4", "role": "played", "rank": 0}
        reference_line = {"root_move": "e2e4", "role": "best_reference", "rank": 0}
        candidate_observation = {
            "claim_id": "claim:candidate", "carrier": candidate_carrier, "parents": [source],
            "kind": "candidate_comparison",
            "comparison": {
                "kind": "played_vs_best", "mover": "white", "reference": reference_line,
                "candidate": candidate_line, "candidate_win_percent_delta_for_mover": -12.5,
                "verdict": "inaccuracy", "candidate_set_type": "narrow_choice",
                "recapture_resource": None,
            },
        }
        strategic_carrier = ref("carrier:strategic", "strategic_mechanism_producer", "strategic_mechanism", line)
        strategic_observation = {
            "claim_id": "claim:strategic", "carrier": strategic_carrier,
            "parents": [source, candidate_carrier], "kind": "strategic_mechanism",
            "mechanism": {
                "kind": "target_pressure",
                "signals": [
                    {
                        "kind": "strategic_fact", "key": "target:e5", "source": source,
                        "strength": 2,
                        "axis": {"kind": "target", "polarity": "gain", "key": "target:e5"},
                    }
                ],
                "semantic_keys": [{"kind": "strategic_kind", "values": ["target_pressure"]}],
            },
        }
        detail = _empty_cause_disposition_observation()["r_public_summary"]
        old_review = {
            "renderable": False, "verdict": None, "idea_status": "unavailable",
            "idea_status_detail": detail, "explanations": [],
        }
        active_empty = {**old_review, "observations": []}
        active_nonempty = {
            **old_review, "renderable": True,
            "verdict": {
                "comparison_kind": "played_vs_best", "mover": "white",
                "verdict_code": "inaccuracy", "move_quality": "bad",
                "played_move": "d2d4", "reference_move": "e2e4",
                "candidate_win_percent_delta_for_mover": -12.5, "win_percent_loss_for_mover": 12.5,
                "outcome": None, "mate": None,
            },
            "idea_status": "no_certified_differential_idea",
            "observations": [candidate_observation, strategic_observation],
        }
        for name, review, definition in (
            ("old absent", old_review, definitions["withheldMoveReview"]),
            ("active empty", active_empty, definitions["withheldMoveReview"]),
            ("active nonempty", active_nonempty, definitions["readyMoveReview"]),
        ):
            with self.subTest(observations=name):
                self.assertEqual(registry._branch_errors(review, definition, schema, "$"), [])
        malformed_observations = copy.deepcopy(active_nonempty)
        malformed_observations["observations"][0]["unexpected"] = True
        self.assertTrue(registry._branch_errors(malformed_observations, definitions["readyMoveReview"], schema, "$"))

    def test_production_run_binds_active_v3_schema_and_hash_contract(self) -> None:
        existing_file = ROOT / "pyproject.toml"
        acquisition = {
            "freeze_manifest_sha256": "a" * 64,
            "cases_file_sha256": "a" * 64,
            "engine_sha256": "a" * 64,
        }

        class _BindingCapture:
            @staticmethod
            def capture_run_document(name: str, document: object) -> str:
                del name
                return sha256_json(document)

        with (
            patch("chesstory_eval.cause_audit._verify_manifest", return_value=({}, [])),
            patch("chesstory_eval.cause_audit._selected_cases", return_value=[]),
            patch(
                "chesstory_eval.cause_audit._load_typed_labels",
                return_value=([{"case_id": "case-1"}], [{"case_id": "case-1"}]),
            ),
            patch("chesstory_eval.cause_audit._read_object", return_value=acquisition),
            patch("chesstory_eval.cause_audit.sha256_file", return_value="a" * 64),
            patch("chesstory_eval.cause_audit._acquisition_rows", return_value={}),
            patch(
                "chesstory_eval.cause_audit._RuntimeJsonlSession",
                side_effect=RuntimeError("session configured"),
            ) as session,
            self.assertRaises(RuntimeError),
        ):
            run_cause_audit(
                root=ROOT,
                store=_BindingCapture(),  # type: ignore[arg-type]
                manifest_path=existing_file,
                cases_path=existing_file,
                acquisition_path=existing_file,
                stockfish_path=existing_file,
                sbt_path=existing_file,
                adapter_root=ROOT / "runtime-adapter",
                cache_root=ROOT / "tmp",
                partition="explore",
                timeout_seconds=1.0,
                provider_timeout_seconds=1.0,
                max_probe_rounds=1,
                candidate_binding_path=None,
                requested_case_ids=[],
                base_oracle_path=existing_file,
            )

        self.assertEqual(
            session.call_args.kwargs["observation_schema_path"],
            CAUSE_V3_SCHEMA.resolve(),
        )
        self.assertEqual(
            session.call_args.kwargs["expected_hash_contract"],
            NATIVE_HASH_CONTRACT,
        )

    def test_active_actual_view_rejects_historical_v2(self) -> None:
        with self.assertRaises(ContractError):
            _strict_actual_view(
                ROOT,
                {"schema_version": ACTUAL_VIEW_SCHEMA_VERSION},
            )

    def test_active_actual_view_preserves_and_rechecks_transport_hashes(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-view",
            "input": {},
        }
        request_sha256 = native_sha256_json(request)
        transport = {
            "round": 1,
            "request_jsonl_sha256": "1" * 64,
            "request_sha256": request_sha256,
            "adapter_request_sha256": request_sha256,
            "hash_contract": NATIVE_HASH_CONTRACT,
            "response_jsonl_sha256": "2" * 64,
            "diagnostic_line_sha256": [],
            "diagnostic_stream_sha256": "3" * 64,
        }
        def actual_view(value: dict[str, object]) -> dict[str, object]:
            return _actual_view(
                case={"case_id": "active-cause-view", "partition": "explore"},
                initial_request_sha256="1" * 64,
                final_request_sha256="1" * 64,
                observation=value,
                transports=[transport],
                issued_probe_hashes=set(),
                result_hashes=set(),
                issued_probe_count=0,
                stop_reason="all_runtime_probes_closed",
                schema_version=TYPED_ACTUAL_VIEW_SCHEMA_VERSION,
            )

        observation = _valid_complete_active_cause_observation(request)
        _validate_active_cause_observation(request, observation)
        view = actual_view(observation)

        canonical = _strict_actual_view(ROOT, view)
        self.assertEqual(canonical["request_sha256"], request_sha256)
        self.assertEqual(canonical["hash_contract"], NATIVE_HASH_CONTRACT)
        self.assertEqual(
            canonical["probe_closure"]["runtime_transport"][0]["request_sha256"],
            request_sha256,
        )

        semantic_key = "exact-plan-result-from-runtime"
        marked_observation = copy.deepcopy(observation)
        marked_observation["importance"]["r_native"]["profiles"] = [  # type: ignore[index]
            {"effect_scope": {"plan_result_semantic_key": semantic_key}}
        ]
        copied = actual_view(marked_observation)
        self.assertEqual(
            copied["importance"]["r_native"]["profiles"][0]["effect_scope"][  # type: ignore[index]
                "plan_result_semantic_key"
            ],
            semantic_key,
        )

        changed = copy.deepcopy(view)
        changed["request_sha256"] = "f" * 64
        with self.assertRaises(IntegrityError):
            _strict_actual_view(ROOT, changed)

        changed = copy.deepcopy(view)
        changed["probe_closure"]["runtime_transport"][0]["request_sha256"] = (  # type: ignore[index]
            "e" * 64
        )
        with self.assertRaises(IntegrityError):
            _strict_actual_view(ROOT, changed)

        changed_observation = copy.deepcopy(observation)
        changed_observation["request_sha256"] = "f" * 64
        with self.assertRaises(IntegrityError):
            actual_view(changed_observation)


class ActiveProbeCompletionContractTest(unittest.TestCase):
    def test_runtime_session_rejects_partial_schema_hash_binding(self) -> None:
        with self.assertRaises(ContractError):
            _RuntimeJsonlSession(
                ["not-started"],
                cwd=ROOT / "runtime-adapter",
                timeout_seconds=1.0,
                expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
                observation_schema_path=CAUSE_V3_SCHEMA,
            )
        with self.assertRaises(ContractError):
            _RuntimeJsonlSession(
                ["not-started"],
                cwd=ROOT / "runtime-adapter",
                timeout_seconds=1.0,
                expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
                expected_hash_contract=NATIVE_HASH_CONTRACT,
            )

    def test_runtime_invoke_hashes_request_and_preserves_exact_response_bytes(
        self,
    ) -> None:
        class FakeProcess:
            def __init__(self) -> None:
                self.stdin = io.BytesIO()

            def poll(self) -> None:
                return None

        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-wire",
            "input": {"z": 1, "a": 2},
        }
        request_line = f"{native_canonical_json(request)}\n".encode("utf-8")
        observation = _valid_active_cause_observation(request)
        response_line = (
            "  "
            + json.dumps(
                dict(reversed(list(observation.items()))),
                ensure_ascii=False,
                separators=(", ", ": "),
            )
            + " \r\n"
        ).encode("utf-8")
        session = _RuntimeJsonlSession(
            ["not-started"],
            cwd=ROOT / "runtime-adapter",
            timeout_seconds=1.0,
            expected_schema=TYPED_RUNTIME_OBSERVATION_SCHEMA,
            observation_schema_path=CAUSE_V3_SCHEMA,
            expected_hash_contract=NATIVE_HASH_CONTRACT,
        )
        process = FakeProcess()
        session._process = process  # type: ignore[assignment]
        session._output.put(("line", response_line))
        exchange = session.invoke(request)

        self.assertEqual(process.stdin.getvalue(), request_line)
        self.assertEqual(exchange.request_jsonl_sha256, sha256_bytes(request_line))
        self.assertEqual(exchange.request_sha256, sha256_bytes(request_line[:-1]))
        self.assertEqual(exchange.hash_contract, NATIVE_HASH_CONTRACT)
        self.assertEqual(exchange.response_jsonl_sha256, sha256_bytes(response_line))
        self.assertEqual(
            base64.b64decode(exchange.response_jsonl_base64, validate=True),
            response_line,
        )
        self.assertEqual(
            _verified_response_jsonl_bytes(
                exchange.response_jsonl_base64,
                exchange.response_jsonl_sha256,
            ),
            response_line,
        )
        with self.assertRaises(IntegrityError):
            _verified_response_jsonl_bytes(
                exchange.response_jsonl_base64,
                "f" * 64,
            )

    def test_active_v3_schema_accepts_valid_response_and_rejects_malformed_fields(
        self,
    ) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-schema",
            "input": {},
        }
        valid = _valid_complete_active_cause_observation(request)
        self.assertEqual(_validate_active_cause_observation(request, valid), valid)

        malformed = copy.deepcopy(valid)
        malformed.pop("importance")
        with self.assertRaises(ContractError):
            _validate_active_cause_observation(request, malformed)

        malformed = copy.deepcopy(valid)
        malformed["importance"] = []
        with self.assertRaises(ContractError):
            _validate_active_cause_observation(request, malformed)

        malformed = copy.deepcopy(valid)
        malformed["unexpected"] = True
        with self.assertRaises(ContractError):
            _validate_active_cause_observation(request, malformed)

    def test_active_v3_rejects_request_or_hash_contract_drift(self) -> None:
        request = {
            "schema_version": "chesstory.move-meaning.request.v1",
            "request_id": "active-cause-binding",
            "input": {},
        }
        observation = _valid_active_cause_observation(request)
        observation["request_sha256"] = native_sha256_json(
            {**request, "request_id": "another-request"}
        )
        with self.assertRaises(IntegrityError):
            _validate_active_cause_observation(request, observation)

        for contract in (None, "unknown.v1"):
            observation = _valid_active_cause_observation(request)
            if contract is None:
                observation.pop("hash_contract")
            else:
                observation["hash_contract"] = contract
            with self.subTest(contract=contract), self.assertRaises(ContractError):
                _validate_active_cause_observation(request, observation)

    def test_historical_v2_validation_and_transport_shape_remain_legacy(self) -> None:
        historical = {
            "schema_version": "chesstory.cause-audit-runtime-observation.v2",
            "request_sha256": "1" * 64,
            "hash_contract": NATIVE_HASH_CONTRACT,
        }
        self.assertEqual(
            _validate_runtime_observation(
                historical,
                expected_schema="chesstory.cause-audit-runtime-observation.v2",
                registry=None,
                schema_path=None,
            ),
            historical,
        )

        response_bytes = b'{"schema_version":"chesstory.runtime-observation.v2"}\n'
        exchange = _RuntimeExchange(
            observation={"schema_version": "chesstory.runtime-observation.v2"},
            request_jsonl_sha256="1" * 64,
            response_jsonl_sha256=sha256_bytes(response_bytes),
            response_jsonl_base64=base64.b64encode(response_bytes).decode("ascii"),
            diagnostic_line_sha256=(),
            diagnostic_stream_sha256=sha256_bytes(b""),
        )
        document = _runtime_io_document(
            exchange,
            {
                "schema_version": "chesstory.move-meaning.request.v1",
                "request_id": "historical-v2-wire",
                "input": {},
            },
            command=("historical-v2-runtime",),
            round_index=0,
        )
        self.assertNotIn("response_jsonl_base64", document)
        self.assertEqual(document["response_jsonl_sha256"], sha256_bytes(response_bytes))


class NativeContentTraceContractTest(unittest.TestCase):
    def test_v3_content_trace_links_are_exact_and_fail_closed(self) -> None:
        schema = ROOT / "schemas" / "native-v3" / "runtime-observation.schema.json"

        def s(value: str) -> dict[str, object]:
            return {"node_kind": "string", "scala_type": "java.lang.String", "value": value}

        def q(*values: object) -> dict[str, object]:
            return {"node_kind": "sequence", "collection_type": "scala.collection.immutable.List", "values": copy.deepcopy(list(values))}

        def o(value: object) -> dict[str, object]:
            return {"node_kind": "option", "scala_type": "scala.Option", "variant": "Some", "value": copy.deepcopy(value)}

        def e(value: str) -> dict[str, object]:
            return {"node_kind": "java-enum", "scala_type": "test.Status", "constant": value}

        def p(name: str, **fields: object) -> dict[str, object]:
            return {"node_kind": "product", "scala_type": f"test.{name}", "constructor": name, "arity": len(fields),
                    "fields": [{"index": index, "name": key, "value": copy.deepcopy(value)} for index, (key, value) in enumerate(fields.items())]}

        def field(product: dict[str, object], name: str) -> object:
            return next(item["value"] for item in product["fields"] if item["name"] == name)  # type: ignore[index]

        def build(kinds: tuple[str, ...] = (), p_observed: bool = True) -> tuple[dict[str, object], dict[str, object]]:
            request = {"schema_version": "chesstory.move-meaning.request.v1", "request_id": "native-content-trace", "input": {}}
            refs = [p("EvidenceRef", id=s(f"carrier:{index}")) for index in range(len(kinds))]
            claims = [p("JudgmentClaim", id=s(f"content:{index}"), content=o(p(kind, carrier=ref)))
                      for index, (kind, ref) in enumerate(zip(kinds, refs))]
            ids = [f"content:{index}" for index in range(len(claims))]
            packet = p("EvidenceBackedJudgmentPacket",
                       assembly=p("JudgmentAssemblyContext", claims=q(*claims)),
                       selectedContentClaimIds=q(*(s(item) for item in ids)))
            trees = {
                "Q": p("Q"), "F": p("F", records=q(*(p("EvidenceRecord", ref=ref) for ref in refs))),
                "C": p("C"), "Jp": q(*claims),
                "Ja": p("Ja", decisions=q(*(p("ClaimAdmissionDecision", claim=claim, status=e("Certified")) for claim in claims))),
                "R": p("ClaimRankingResult", ranked=q(*(p("RankedClaimDecision", claim=claim) for claim in claims)),
                       selectedContentClaimIds=q(*(s(item) for item in ids))),
                "P": p("P", packet=packet), "V": p("V"),
            }
            stages: dict[str, object] = {}
            upstream: str | None = None
            for stage in ("Q", "F", "C", "Jp", "Ja", "R", "P", "V"):
                observed = stage != "V" and (stage != "P" or p_observed)
                record: dict[str, object] = {
                    "stage_id": stage, "status": "observed-native" if observed else "unavailable",
                    "boundary_reached": observed, "reason": "captured" if observed else "unavailable",
                    "native_contract_version": "chesstory.runtime-native-boundary.v3",
                    "v1_semantic_mapping_status": "unavailable", "v1_semantic_mapping_reason": "unavailable",
                    "upstream_artifact_sha256": upstream, "artifact_sha256": None,
                }
                if observed or stage == "V":
                    role = "typed-unavailability" if stage == "V" else "observed-native-input-output" if stage == "P" else "observed-native-output"
                    artifact = {"native_contract_version": "chesstory.runtime-native-boundary.v3",
                                "native_tree_contract_version": "chesstory.runtime-native-tree.v2",
                                "stage": stage, "capture_role": role, "upstream_artifact_sha256": upstream, "native_tree": trees[stage]}
                    upstream = native_sha256_json(artifact)
                    record.update({"artifact_role": role, "artifact": artifact, "artifact_sha256": upstream})
                stages[stage] = record
            detail = {"authority": "cause_disposition_ledger.v1", "total_cause_count": 0, "selected_cause_ids": [],
                      "status_counts": dict.fromkeys(("selected", "dominated", "redundant", "diagnostic", "inferior", "admission_deferred", "rejected", "unproposed", "object_unready"), 0),
                      "reason_counts": dict.fromkeys(("player_facing_selection", "dominated_fallback", "cross_comparison_redundancy", "certified_claim_deduplicated", "diagnostic_comparison", "inferior_alternative", "claim_admission_deferred", "claim_admission_rejected", "no_claim_proposal", "object_readiness_failed"), 0), "abstention_codes": []}
            response = ({"http_status": 200, "body": {"schema_version": "chesstory.move-meaning.response.v3", "request_id": request["request_id"], "ok": True, "status": "withheld",
                         "availability": {"state": "withheld", "reason": "insufficient_engine_depth"}, "probe_requests": [], "move_review": {"renderable": False, "verdict": None, "idea_status": "unavailable", "idea_status_detail": detail, "explanations": []}}}
                        if p_observed else {"http_status": 400, "body": {"schema_version": "chesstory.move-meaning.response.v3", "request_id": request["request_id"], "ok": False, "status": "error", "error": "invalid_request"}})
            return {
                "schema_version": "chesstory.runtime-observation.v3", "native_contract_version": "chesstory.runtime-native-boundary.v3",
                "observation_status": "complete", "request_sha256": native_sha256_json(request),
                "runtime": {"adapter_name": "chesstory-judgment-runtime-adapter", "adapter_version": "0.3.0",
                            "request_schema": "chesstory.move-meaning.request.v1", "response_schema": "chesstory.move-meaning.response.v3",
                            "http_status": 200 if p_observed else 400, "boundary_execution_present": True, "projection_present": p_observed},
                "public_response": response, "public_response_sha256": native_sha256_json(response),
                "hash_contract": NATIVE_HASH_CONTRACT, "artifact_chain_head_sha256": upstream, "stages": stages,
            }, request

        def tree(observation: dict[str, object], stage: str) -> dict[str, object]:
            return observation["stages"][stage]["artifact"]["native_tree"]  # type: ignore[index]

        def rechain(observation: dict[str, object]) -> None:
            upstream: str | None = None
            for stage in ("Q", "F", "C", "Jp", "Ja", "R", "P", "V"):
                record = observation["stages"][stage]  # type: ignore[index]
                record["upstream_artifact_sha256"] = upstream  # type: ignore[index]
                artifact = record.get("artifact")  # type: ignore[union-attr]
                if artifact is None:
                    record["artifact_sha256"] = None  # type: ignore[index]
                else:
                    artifact["upstream_artifact_sha256"] = upstream  # type: ignore[index]
                    upstream = native_sha256_json(artifact)
                    record["artifact_sha256"] = upstream  # type: ignore[index]
            observation["artifact_chain_head_sha256"] = upstream

        def mutate(observation: dict[str, object], action: str) -> None:
            if action == "artifact":
                tree(observation, "F")["constructor"] = "changed"
            elif action == "missing":
                claim = tree(observation, "Jp")["values"][0]  # type: ignore[index]
                fields = claim["fields"]  # type: ignore[index]
                fields[:] = [item for item in fields if item["name"] != "content"]  # type: ignore[index]
                for index, item in enumerate(fields): item["index"] = index  # type: ignore[index]
                claim["arity"] = len(fields)  # type: ignore[index]
            elif action == "foreign":
                field(field(field(tree(observation, "F"), "records")["values"][0], "ref"), "id")["value"] = "foreign"  # type: ignore[index]
            elif action == "ambiguous":
                values = field(tree(observation, "F"), "records")["values"]  # type: ignore[index]
                values.append(copy.deepcopy(values[0]))  # type: ignore[union-attr,index]
            elif action == "ja":
                field(field(field(tree(observation, "Ja"), "decisions")["values"][0], "claim"), "id")["value"] = "changed"  # type: ignore[index]
            elif action == "duplicate-claim":
                field(tree(observation, "Jp")["values"][1], "id")["value"] = "content:0"  # type: ignore[index]
                field(field(field(tree(observation, "Ja"), "decisions")["values"][1], "claim"), "id")["value"] = "content:0"  # type: ignore[index]
            elif action in {"duplicate", "unknown"}:
                values = field(tree(observation, "R"), "selectedContentClaimIds")["values"]  # type: ignore[index]
                values.append(copy.deepcopy(values[0])) if action == "duplicate" else values[0].update({"value": "unknown"})  # type: ignore[union-attr,index]
            elif action == "deferred":
                field(field(tree(observation, "Ja"), "decisions")["values"][0], "status")["constant"] = "Deferred"  # type: ignore[index]
            else:
                packet = field(tree(observation, "P"), "packet")
                if action == "p-order":
                    field(packet, "selectedContentClaimIds")["values"].reverse()  # type: ignore[index]
                elif action == "p-hash":
                    field(field(field(packet, "assembly"), "claims")["values"][0], "id")["value"] = "changed"  # type: ignore[index]
                else:
                    raise AssertionError(action)

        def validate(request: dict[str, object], observation: dict[str, object], schema_bound: bool = False) -> dict[str, object]:
            return _validate_runtime_observation(
                observation, expected_schema="chesstory.runtime-observation.v3",
                registry=SchemaRegistry(schema.parents[1]) if schema_bound else None,
                schema_path=schema if schema_bound else None,
                expected_request_sha256=native_sha256_json(request), expected_hash_contract=NATIVE_HASH_CONTRACT,
            )

        schema_observation, schema_request = build(("CandidateComparison",), True)
        self.assertEqual(_semantic_checkpoints(validate(schema_request, schema_observation, True))["claim_content_trace"]["state"], "observed")

        rows = (
            ("observed empty", (), True, None, "ok"),
            ("candidate and strategic", ("CandidateComparison", "StrategicMechanism"), True, None, "ok"),
            ("missing content", ("CandidateComparison",), True, "missing", "trace-error"),
            ("foreign carrier", ("CandidateComparison",), True, "foreign", "trace-error"),
            ("ambiguous carrier", ("CandidateComparison",), True, "ambiguous", "trace-error"),
            ("Jp to Ja mutation", ("CandidateComparison",), True, "ja", "trace-error"),
            ("duplicate Jp claim ID", ("CandidateComparison", "StrategicMechanism"), True, "duplicate-claim", "trace-error"),
            ("duplicate R ID", ("CandidateComparison",), True, "duplicate", "trace-error"),
            ("unknown R ID", ("CandidateComparison",), True, "unknown", "trace-error"),
            ("non-certified selected", ("CandidateComparison",), True, "deferred", "ok"),
            ("P unavailable", ("CandidateComparison",), False, None, "ok"),
            ("R to P order", ("CandidateComparison", "StrategicMechanism"), True, "p-order", "trace-error"),
            ("R to P hash", ("CandidateComparison",), True, "p-hash", "trace-error"),
            ("artifact hash", ("CandidateComparison",), True, "artifact", "validation-error"),
        )
        for name, kinds, p_observed, action, expected in rows:
            with self.subTest(case=name):
                observation, request = build(kinds, p_observed)
                if action:
                    mutate(observation, action)
                    if action != "artifact":
                        rechain(observation)
                if expected == "validation-error":
                    with self.assertRaises(IntegrityError):
                        validate(request, observation)
                    continue
                validated = validate(request, observation)
                if expected == "trace-error":
                    with self.assertRaises((ContractError, IntegrityError)):
                        _claim_content_trace(validated)
                    continue
                trace = _claim_content_trace(validated)
                if name == "observed empty":
                    self.assertEqual((trace["claim_links"], trace["r_selected_content_claim_ids"]), ([], []))
                elif name == "P unavailable":
                    self.assertEqual(trace["P"], {"state": "unavailable", "reason": "unavailable"})
                else:
                    self.assertEqual(trace["r_selected_content_claim_ids"], ["content:0"] if name == "non-certified selected" else ["content:0", "content:1"])
                    self.assertEqual(trace["P"]["state"], "observed")  # type: ignore[index]
                    self.assertRegex(trace["P"]["packet_sha256"], r"^[0-9a-f]{64}$")  # type: ignore[index]
                    self.assertTrue(all(item["jp_claim_sha256"] == item["ja_claim_sha256"] == item["r_claim_sha256"] == item["p_claim_sha256"] for item in trace["claim_links"]))  # type: ignore[index]
                    if name == "non-certified selected":
                        self.assertEqual(trace["claim_links"][0]["ja_status"], "Deferred")  # type: ignore[index]
                    else:
                        self.assertEqual({item["kind"] for item in trace["claim_links"]}, {"CandidateComparison", "StrategicMechanism"})  # type: ignore[index]

if __name__ == "__main__":
    unittest.main()
