from __future__ import annotations

import copy
import json
import unittest
from pathlib import Path

from chesstory_eval.model import ContractError
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]


class CommentarySelectionContractTest(unittest.TestCase):
    """Grammar and runtime wire compatibility; no engine accuracy claims."""

    def setUp(self) -> None:
        self.registry = SchemaRegistry(ROOT / "schemas")
        self.base = {
            "id": "candidate", "kind": "conditional_line", "state": "ready", "selection": "context",
            "root_occurrence_id": "occurrence", "occurrence_ids": ["occurrence"],
            "position_ids": ["before", "after"], "line_ids": ["line"],
            "evidence_ids": ["line-proof"], "source_refs": [],
            "detail": {"type": "observed_line", "scope": "observed_line"},
        }

    def test_actual_runtime_responses_pass_the_shared_response_schema(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-selection-produced.json").read_text(encoding="utf-8"))
        self.assertEqual(len(fixtures), 2)
        for fixture in fixtures:
            with self.subTest(name=fixture["name"]):
                self.registry.validate_document(
                    fixture["response"], ROOT / "schemas/public-commentary-v6/position-commentary-response.schema.json",
                    label=fixture["name"],
                )
                self.assertEqual(len(fixture["response"]["observations"]["selection"]["scenes"]), 1)

    def test_runtime_investigation_responses_pass_status_and_completed_schemas(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        self.assertEqual(len(fixtures), 49)
        for fixture in fixtures:
            response = fixture["response"]
            name = "position-commentary-job-status" if response["schema_version"].endswith("job-status.v6") else "position-commentary-response"
            with self.subTest(name=fixture["name"]):
                self.registry.validate_document(response, ROOT / f"schemas/public-commentary-v6/{name}.schema.json", label=fixture["name"])
                selection = response["observations"]["selection"]
                self.assertTrue(selection.get("inquiries") or selection.get("continuation_inquiries"))

    def test_legal_check_metadata_is_optional_boolean_not_a_recovery_claim(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/resources-produced.json").read_text(encoding="utf-8"))
        position = next(p for f in fixtures for p in f["response"]["observations"]["positions"] if p["legal_moves"]["moves"])
        schema = ROOT / "schemas/public-v6/position-resources.schema.json"
        self.assertNotIn("gives_check", position["legal_moves"]["moves"][0])
        self.registry.validate_document(position, schema, label="legacy check unknown")
        for value in (False, True, None, 1, "true", []):
            altered = copy.deepcopy(position)
            altered["legal_moves"]["moves"][0]["gives_check"] = value
            with self.subTest(value=value):
                if type(value) is bool:
                    self.registry.validate_document(altered, schema, label="certified legal check metadata")
                else:
                    with self.assertRaises(ContractError):
                        self.registry.validate_document(altered, schema, label="malformed legal check metadata")
        position["legal_moves"]["moves"][0]["forces_recovery"] = True
        with self.assertRaises(ContractError):
            self.registry.validate_document(position, schema, label="no recovery escalation")

    def test_continuation_premises_require_closed_explicit_observation_bindings(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-support-frontier-complete")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("missing", "empty", "no_origin", "no_trigger", "no_requirement", "negative_source", "duplicate_source", "unknown_state", "unknown_kind", "proof_claim"):
            altered = copy.deepcopy(observations)
            question = next(q for q in altered["selection"]["continuation_inquiries"] if "support_application_not_observed" in q["kinds"])
            premise = next(p for p in question["premises"] if p["kind"] == "support_application_not_observed")
            if mutation == "missing":
                del question["premises"]
            elif mutation == "empty":
                question["premises"] = []
            elif mutation == "no_origin":
                premise["source_line_ids"] = []
            elif mutation == "no_trigger":
                premise["trigger_occurrence_ids"] = []
            elif mutation == "no_requirement":
                del premise["requirement_id"]
            elif mutation == "negative_source":
                premise["comparison_source_indices"] = [-1]
            elif mutation == "duplicate_source":
                premise["comparison_source_indices"] = [0, 0]
            elif mutation == "unknown_state":
                premise["observation_state"] = "tactically_stable"
            elif mutation == "unknown_kind":
                premise["kind"] = "any_future_support"
            else:
                premise["forced_result"] = True
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="per-premise wire grammar")

    def test_root_search_records_keep_pending_completed_and_unbound_stopped_reports_distinct(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "opening-equal-alternative")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("pending_returned", "completed_empty", "completed_unbound", "half_binding", "stopped_no_reason", "unknown_purpose", "duplicate_score"):
            altered = copy.deepcopy(observations)
            search = next(row for row in altered["selection"]["root_searches"] if row["candidates"])
            if mutation == "pending_returned":
                search["state"] = "awaiting_engine"
            elif mutation == "completed_empty":
                search["candidates"] = []
            elif mutation == "completed_unbound":
                search["candidates"][0]["line_id"] = None
                search["candidates"][0]["occurrence_id"] = None
            elif mutation == "half_binding":
                search["state"] = "stopped"
                search["stop_reason"] = "review_construction_failed"
                search["candidates"][0]["line_id"] = None
            elif mutation == "stopped_no_reason":
                search["state"] = "stopped"
            elif mutation == "unknown_purpose":
                search["purpose"] = "full_tactical_proof"
            else:
                search["candidates"][0]["evaluation"] = {"cp": 50}
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="root search lifecycle grammar")

    def test_pv_facets_close_only_their_bounded_comparison(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = next(row["response"] for row in fixtures if row["name"] == "opening-equal-alternative")
        observations = response["observations"]
        inquiry = next(row for row in observations["selection"]["inquiries"] if row["alternative_move"] == "f1c4")
        candidate = next(row for row in observations["selection"]["candidates"] if row["id"] in inquiry["explanation_candidate_ids"])
        self.assertEqual(inquiry["answer"]["evaluation_relation"], "equal")
        self.assertEqual(candidate["state"], "ready")
        self.assertEqual(candidate["detail"]["material_relation"], "equal")
        self.assertEqual(candidate["detail"]["root_observation_relation"], "different")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        candidate["state"] = "needs_explanation"
        with self.assertRaises(ContractError):
            self.registry.validate_document(observations, schema, label="bounded difference state")
        candidate["state"] = "ready"
        candidate["detail"]["scope"] = "forced_result"
        with self.assertRaises(ContractError):
            self.registry.validate_document(observations, schema, label="no scope escalation")

    def test_continuation_progress_cannot_claim_stability_or_drop_half_its_contract(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-deep-complete")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("stable", "explained", "missing_half", "duplicate_question"):
            altered = copy.deepcopy(observations)
            question = altered["selection"]["continuation_inquiries"][0]
            if mutation == "stable":
                question["tactically_stable"] = True
            elif mutation == "explained":
                question["state"] = "fully_explained"
            elif mutation == "missing_half":
                del question["unresolved_source_line_ids"]
            else:
                question["followup_question_ids"] = [question["id"], question["id"]]
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="bounded investigation progress")

    def test_recovery_route_and_shared_control_cannot_claim_a_forced_result(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-route-sampled")
        for mutation in ("best", "score", "closed", "awaiting_with_limit"):
            altered = copy.deepcopy(observations)
            route = next(q["route_challenge"] for q in altered["selection"]["continuation_inquiries"] if q.get("route_challenge"))
            if mutation == "best":
                route["best_reply"] = True
            elif mutation == "score":
                route["move_classes"][0]["evaluation"] = {"cp": 100}
            elif mutation == "closed":
                route["state"] = "recovery_impossible"
            else:
                route["state"] = "awaiting_engine"
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="same-search route grammar")
        observed = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-other-target")
        shared = next(difference["shared_attackers"][0] for candidate in observed["selection"]["candidates"] for difference in candidate["detail"].get("differences", []) if difference.get("shared_attackers"))
        shared["forced_fork"] = True
        with self.assertRaises(ContractError):
            self.registry.validate_document(observed, schema, label="control does not assert forced capture")

    def test_support_application_requires_a_king_and_closed_legal_inventory(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-support-application")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("nonking", "incomplete", "forced", "unknown_application"):
            altered = copy.deepcopy(observations)
            application = next(candidate["detail"]["support_applications"][0]["application"] for candidate in altered["selection"]["candidates"] if candidate["detail"].get("support_applications"))
            if mutation == "nonking":
                application["original_attacker"]["role"] = "rook"
            elif mutation == "incomplete":
                application["legal_inventory_complete"] = False
            elif mutation == "forced":
                application["forced_defense"] = True
            else:
                application["kind"] = "any_piece_capture"
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="bounded king-capture application")

    def test_support_frontier_keeps_its_observation_source_explicit(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-support-frontier-awaiting")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("missing_observation", "material_observation", "invented_kind", "forced_claim"):
            altered = copy.deepcopy(observations)
            source = next(source for inquiry in altered["selection"]["continuation_inquiries"] for source in inquiry.get("comparison_sources", []) if source["kind"] == "support_application")
            if mutation == "missing_observation":
                del source["observation_line_id"]
            elif mutation == "material_observation":
                source["kind"] = "material_recovery"
            elif mutation == "invented_kind":
                source["kind"] = "sufficient_support"
            else:
                source["forced"] = True
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="exact support observation provenance")

    def test_refreshed_pair_does_not_invent_an_original_root_rank(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = next(row["response"] for row in fixtures if row["name"] == "refreshed-assessment")
        selection = response["result"]["selected_move_reviews"][0]["selection"]
        self.assertNotIn("root_rank", selection)
        schema = ROOT / "schemas/public-commentary-v6/position-commentary-response.schema.json"
        for rank in (None, 2, 3):
            pair = copy.deepcopy(response)
            if rank is not None:
                pair["result"]["selected_move_reviews"][0]["selection"]["root_rank"] = rank
            self.registry.validate_document(pair, schema, label="pair preserves rank provenance")
            pair["assessment_scope"] = "root_search"
            with self.subTest(rank=rank), self.assertRaises(ContractError):
                self.registry.validate_document(pair, schema, label="root best needs original rank one")

    def test_exposure_and_direct_recapture_are_closed_observation_facts(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for name, field in (("continuation-exposure-awaiting", "capture_exposures"), ("continuation-direct-support-awaiting", "direct_support_recaptures")):
            observations = next(row["response"]["observations"] for row in fixtures if row["name"] == name)
            for mutation in ("missing_premise", "profitable"):
                altered = copy.deepcopy(observations)
                fact = next(candidate["detail"][field][0] for candidate in altered["selection"]["candidates"] if candidate["detail"].get(field))
                if mutation == "profitable":
                    fact["profitable"] = True
                elif field == "capture_exposures":
                    fact["vacated_blockers"] = []
                else:
                    del fact["capturer"]
                with self.subTest(name=name, mutation=mutation), self.assertRaises(ContractError):
                    self.registry.validate_document(altered, schema, label="physical support and exposure evidence")

    def test_recapturer_support_requires_both_known_transition_points(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = next(row["response"]["observations"] for row in fixtures if row["name"] == "continuation-support-application")
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("unknown", "missing_before", "score"):
            altered = copy.deepcopy(observations)
            row = next(row for candidate in altered["selection"]["candidates"] for row in candidate["detail"].get("differences", []) if row["kind"] == "recapturer_support")
            if mutation == "unknown":
                row["left"]["before"]["support"] = {"state": "unknown", "source_ids": []}
            elif mutation == "missing_before":
                del row["left"]["before"]
            else:
                row["evaluation_cause"] = True
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="known bounded support transitions")

    def test_conditional_scores_cannot_be_relabelled_as_root_observations(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-selection-produced.json").read_text(encoding="utf-8"))
        observations = fixtures[0]["response"]["observations"]
        path = next(row for row in observations["paths"] if row["kind"] == "analysis" and len(row["moves"]) > 1)
        entry = {"search_position_id": path["position_ids"][1], "ply_offset": 1,
                 "evaluation": {"source": "response_comparison", "kind": "engine_search", "depth": 16,
                                "white_score": {"kind": "cp", "value": -50}}}
        path["conditional_evaluations"] = [entry]
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        self.registry.validate_document(observations, schema, label="synthetic scoped score grammar only")
        for mutation in ("root", "offset", "source", "cap"):
            altered = copy.deepcopy(observations)
            target = next(row for row in altered["paths"] if row["id"] == path["id"])
            if mutation == "root":
                target["evaluations"] = [entry["evaluation"]]
            elif mutation == "offset":
                target["conditional_evaluations"][0]["ply_offset"] = 0
            elif mutation == "source":
                target["conditional_evaluations"][0]["evaluation"]["source"] = "root_search"
            else:
                target["conditional_evaluations"] = [entry] * 3
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="scope cannot be promoted")

    def test_response_answers_and_observed_functions_keep_distinct_contracts(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = next(row["response"] for row in fixtures if row["name"] == "response-answered")
        observations = response["observations"]
        answered = next(row for row in observations["selection"]["response_inquiries"] if row["state"] == "answered")
        self.assertEqual(answered["answer"]["ply_offset"], 1)
        self.assertEqual(answered["answer"]["evaluation_relation"], "worse")
        observed = next(row["response"]["observations"] for row in fixtures if row["name"] == "response-observed")
        self.assertTrue(any(row["state"] == "observed" and row["answer"] is None
                            for row in observed["selection"]["response_inquiries"]))
        self.assertFalse(any(e["source"] == "response_comparison" for row in observations["paths"] for e in row.get("evaluations", [])))
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        for mutation in ("root_scope", "offset", "observed_score", "unknown_ready"):
            changed = copy.deepcopy(observations)
            question = next(row for row in changed["selection"]["response_inquiries"] if row["state"] == "answered")
            if mutation == "root_scope":
                question["scope"] = "root_choice"
            elif mutation == "offset":
                question["answer"]["ply_offset"] = 0
            elif mutation == "observed_score":
                question["state"] = "observed"
            else:
                candidate = next(row for row in changed["selection"]["candidates"]
                                 if row["detail"].get("response_inquiry_id") == question["id"] and row["state"] == "ready")
                self.assertEqual(candidate["state"], "ready")
                candidate["detail"]["branches"][0]["recaptures"] = {"state": "unknown", "moves": []}
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(changed, schema, label="child scope and completion")

    def test_child_work_has_a_required_anchor_and_does_not_reset_its_limits(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = next(row["response"] for row in fixtures if row["name"] == "response-awaiting")
        schema = ROOT / "schemas/public-commentary-v6/position-commentary-job-status.schema.json"
        for mutation in ("missing_anchor", "root_with_child_anchor", "new_lease"):
            changed = copy.deepcopy(response)
            work = changed["issued_engine_work"]
            if mutation == "missing_anchor":
                del work["search_anchor"]
            elif mutation == "root_with_child_anchor":
                work["purpose"] = "investigation_comparison"
            else:
                work["max_search_elapsed_ms"] = 6000
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(changed, schema, label="child work envelope")

    def test_response_search_grammar_keeps_scope_and_selection_provenance_explicit(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = copy.deepcopy(next(row["response"]["observations"] for row in fixtures if row["name"] == "response-answered"))
        question = next(row for row in observations["selection"]["response_inquiries"] if row["state"] == "answered")
        answer = question["answer"]
        search = {key: answer[key] for key in ("work_id", "execution_key_sha256", "engine_profile", "required_depth")}
        search.update({key: question[key] for key in ("root_move", "root_move_occurrence_id", "child_position_id")})
        search.update(multi_pv=2, root_restriction={"kind": "restricted", "moves_uci": [question["reference_move"], question["alternative_move"]]}, candidates=[])
        for side in ("reference", "alternative"):
            search["candidates"].append({"move_uci": question[f"{side}_move"], "line_id": answer[f"{side}_line_id"],
                                         "occurrence_id": answer[f"{side}_occurrence_id"], "evaluation": answer[f"{side}_evaluation"]})
            question[f"{side}_selection"] = {"source": "child_search", "work_id": search["work_id"], "line_id": answer[f"{side}_line_id"]}
        observations["selection"]["response_searches"] = [search]
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        self.registry.validate_document(observations, schema, label="shared search grammar only")
        for mutation in ("root_score", "unbounded_sample", "missing_selection", "seed_with_receipt", "best_claim"):
            altered = copy.deepcopy(observations)
            entry = altered["selection"]["response_searches"][0]
            inquiry = next(row for row in altered["selection"]["response_inquiries"] if row["id"] == question["id"])
            if mutation == "root_score":
                entry["candidates"][0]["evaluation"]["source"] = "root_search"
            elif mutation == "unbounded_sample":
                entry["multi_pv"] = 4
            elif mutation == "missing_selection":
                del inquiry["reference_selection"]
            elif mutation == "seed_with_receipt":
                inquiry["reference_selection"]["source"] = "legal_seed"
            else:
                inquiry["reference_selection"]["source"] = "proven_best"
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="no scope escalation")

    def test_investigation_pools_and_child_classes_keep_focus_comparison_at_two(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = copy.deepcopy(next(row["response"] for row in fixtures if row["name"] == "response-awaiting"))
        work = response["issued_engine_work"]
        schema = ROOT / "schemas/public-commentary-v6/position-commentary-job-status.schema.json"
        # Grammar only: the consumer additionally matches each entire class to its exact legal catalogue.
        for restriction, multi_pv in (({"kind": "unrestricted"}, 3),
                                      ({"kind": "restricted", "moves_uci": ["d5e4"]}, 1),
                                      ({"kind": "restricted", "moves_uci": ["g8f7", "g8f8", "g8g7", "g8h7"]}, 3)):
            work["root_restriction"] = restriction
            work["search_limits"]["multi_pv"] = multi_pv
            self.registry.validate_document(response, schema, label="child scope independent of returned sample size")
            changed = copy.deepcopy(response)
            changed["issued_engine_work"]["purpose"] = "focus_comparison"
            del changed["issued_engine_work"]["search_anchor"]
            with self.assertRaises(ContractError):
                self.registry.validate_document(changed, schema, label="focus pairs stay bounded to two moves")
        work["purpose"] = "investigation_comparison"
        del work["search_anchor"]
        self.registry.validate_document(response, schema, label="root investigation can sample a larger restricted pool")

    def test_search_history_grammar_keeps_legacy_one_ply_and_explicit_bounded_prefixes(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = copy.deepcopy(next(row["response"] for row in fixtures if row["name"] == "response-awaiting"))
        schema = ROOT / "schemas/public-commentary-v6/position-commentary-job-status.schema.json"
        work = response["issued_engine_work"]
        work["search_anchor"].pop("prefix_moves_uci", None)
        # Numeric grammar only. These repeated tokens are not an admitted chess history.
        work["engine_position_moves_uci"] = ["e2e4"] * 601
        self.registry.validate_document(response, schema, label="root allowance plus one child anchor")
        work["engine_position_moves_uci"].append("e2e4")
        with self.assertRaises(ContractError):
            self.registry.validate_document(response, schema, label="no second child allowance")
        work["engine_position_moves_uci"].pop()
        work["search_anchor"]["prefix_moves_uci"] = ["e2e4"] * 79
        work["engine_position_moves_uci"] = ["e2e4"] * 679
        self.registry.validate_document(response, schema, label="explicit bounded search prefix grammar")
        work["engine_position_moves_uci"].append("e2e4")
        with self.assertRaises(ContractError):
            self.registry.validate_document(response, schema, label="full search prefix bound")
        work["engine_position_moves_uci"] = ["e2e4"] * 601
        work["purpose"] = "investigation_comparison"
        del work["search_anchor"]
        work["root_restriction"] = {"kind": "restricted", "moves_uci": ["e2e4", "d2d4"]}
        work["search_limits"]["multi_pv"] = 2
        with self.assertRaises(ContractError):
            self.registry.validate_document(response, schema, label="root allowance remains 600")
        work["engine_position_moves_uci"].pop()
        self.registry.validate_document(response, schema, label="unchanged root history grammar")

    def test_exact_search_prefix_grammar_requires_paired_offset_and_preserves_target_answer_scope(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        observations = copy.deepcopy(next(row["response"]["observations"] for row in fixtures if row["name"] == "response-answered"))
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        search = observations["selection"]["response_searches"][0]
        search.update(prefix_moves_uci=[search["root_move"]], ply_offset=1)
        self.registry.validate_document(observations, schema, label="explicit search origin")
        for mutation in ("missing_offset", "missing_prefix", "empty_prefix", "root_offset", "too_long", "target_offset"):
            altered = copy.deepcopy(observations)
            changed = altered["selection"]["response_searches"][0]
            if mutation == "missing_offset":
                del changed["ply_offset"]
            elif mutation == "missing_prefix":
                del changed["prefix_moves_uci"]
            elif mutation == "empty_prefix":
                changed["prefix_moves_uci"] = []
            elif mutation == "root_offset":
                changed["ply_offset"] = 0
            elif mutation == "too_long":
                changed["prefix_moves_uci"] = [changed["root_move"]] * 80
            else:
                next(q for q in altered["selection"]["response_inquiries"] if q["answer"])["answer"]["ply_offset"] = 2
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(altered, schema, label="prefix and offset grammar")

    def test_continuation_profiles_and_pairs_retain_bounded_observation_grammar(self) -> None:
        fixtures = json.loads((ROOT / "fixtures/public-commentary-v6/commentary-investigation-produced.json").read_text(encoding="utf-8"))
        response = next(row["response"] for row in fixtures if row["name"] == "continuation-move-order")
        observations = response["observations"]
        schema = ROOT / "schemas/public-v6/move-observations.schema.json"
        profiles = [row for row in observations["selection"]["candidates"] if row["detail"]["type"] == "line_continuation"]
        pairs = [row for row in observations["selection"]["candidates"] if row["detail"]["type"] == "line_continuation_comparison"]
        self.assertEqual(len(profiles), 2)
        self.assertTrue(pairs)
        self.registry.validate_document(observations, schema, label="exact continuation profiles and pairs")
        for mutation in ("forced_profile", "invented_support", "incomplete_legal_claim", "extra_score", "unsupported_relation"):
            changed = copy.deepcopy(observations)
            profile = next(row["detail"] for row in changed["selection"]["candidates"] if row["detail"]["type"] == "line_continuation")
            pair = next(row["detail"] for row in changed["selection"]["candidates"] if row["detail"]["type"] == "line_continuation_comparison")
            if mutation == "forced_profile":
                profile["scope"] = "forced_line"
            elif mutation == "invented_support":
                profile["support_comparisons"][0]["state"] = "best_defense"
            elif mutation == "incomplete_legal_claim":
                next(d for d in pair["differences"] if d["kind"] == "legal_capture_availability")["left"]["legal_inventory_complete"] = False
            elif mutation == "extra_score":
                profile["engine_score"] = 500
            else:
                pair["differences"][0]["kind"] = "forced_recovery"
            with self.subTest(mutation=mutation), self.assertRaises(ContractError):
                self.registry.validate_document(changed, schema, label="bounded continuation grammar")

    def validate(self, candidate: dict | None = None, scene: dict | None = None) -> None:
        observations = {"statements": [], "paths": []}
        if candidate is not None:
            observations["selection"] = {
                "version": 1, "candidates": [candidate], "scenes": [] if scene is None else [scene],
            }
        self.registry.validate_document(
            observations, ROOT / "schemas/public-v6/move-observations.schema.json", label="selection grammar",
        )

    def comparison(self, ready: bool) -> dict:
        row = copy.deepcopy(self.base)
        row.update(kind="comparison", state="ready" if ready else "partial", selection="selected")
        row["detail"] = {
            "type": "observed_material_comparison" if ready else "evaluation_comparison",
            "scope": "observed_lines", "comparison_evidence_id": "comparison-proof",
            "comparison_kind": "played_vs_best", "reference_line_id": "reference" if ready else None,
            "candidate_line_id": "line", "missing_line_ids": [] if ready else ["reference"],
            "material_relation": "different" if ready else "unavailable",
            "explanation_gap": None if ready else "missing_path",
            "text": "A bounded observed-line statement.",
        }
        if ready:
            row["detail"].update(perspective="white", reference_net_cp=0, candidate_net_cp=-100)
        return row

    def test_optional_selection_and_referenced_line_context(self) -> None:
        self.validate()
        self.validate(self.base)
        for field in ("moves", "summary"):
            duplicate = copy.deepcopy(self.base)
            duplicate["detail"][field] = []
            with self.assertRaises(ContractError):
                self.validate(duplicate)

    def test_ready_and_partial_comparisons_have_different_obligations(self) -> None:
        ready = self.comparison(True)
        self.validate(ready)
        partial = self.comparison(False)
        self.validate(partial)
        equal = copy.deepcopy(ready)
        equal["state"] = "needs_explanation"
        equal["detail"].update(type="evaluation_comparison", material_relation="equal",
                               explanation_gap="beyond_material_scope")
        self.validate(equal)
        for gap in ("incomplete_line", "material_evidence_unavailable"):
            unavailable = copy.deepcopy(partial)
            unavailable["detail"].update(reference_line_id="reference", missing_line_ids=[], explanation_gap=gap)
            self.validate(unavailable)
            unavailable["detail"].update(perspective="white", reference_net_cp=0, candidate_net_cp=0)
            with self.subTest(gap=gap), self.assertRaises(ContractError):
                self.validate(unavailable)
        for field, value in (
            ("reference_line_id", None), ("missing_line_ids", ["missing"]),
            ("reference_net_cp", None), ("type", "evaluation_comparison"),
            ("material_relation", "equal"), ("explanation_gap", "beyond_material_scope"),
        ):
            malformed = copy.deepcopy(ready)
            malformed["detail"][field] = value
            with self.subTest(field=field), self.assertRaises(ContractError):
                self.validate(malformed)
        partial["state"] = "ready"
        with self.assertRaises(ContractError):
            self.validate(partial)
        equal["state"] = "ready"
        with self.assertRaises(ContractError):
            self.validate(equal)

    def test_source_kind_and_stop_are_not_interchangeable(self) -> None:
        row = copy.deepcopy(self.base)
        source = {"line_id": "line", "kind": "supplied", "source": "study",
                  "accepted_ply_count": 2, "stop": "invalid_continuation"}
        row.update(state="partial", source_refs=[source])
        self.validate(row)
        source["kind"] = "evaluation"
        with self.assertRaises(ContractError):
            self.validate(row)
        source.update(source="root_search", stop=None)
        self.validate(row)

    def test_all_local_detail_variants_are_closed_typed_shapes(self) -> None:
        actor = {"position_id": "after", "side": "white", "role": "rook", "square": "e4"}
        target = {"position_id": "after", "side": "black", "role": "pawn", "square": "e5"}
        details = [
            {"type": "development", "before_role": "knight", "from": "g1", "to": "f3"},
            {"type": "castling", "rook_from": "h1", "rook_to": "f1"},
            {"type": "capture", "captured_role": "pawn", "captured_square": "e5",
             "captured_position_id": "before", "recapture": "unknown"},
            {"type": "evades_attack", "target": target, "attacker": actor},
            {"type": "attack", "target": target, "gained": True},
            {"type": "support", "target": actor, "opposition": [target], "gained": True},
            {"type": "rook_connection", "target": actor},
            {"type": "passed_pawn", "to": "e4", "previously_passed": None},
            {"type": "king_entry", "controlled_squares": ["e5"]},
            {"type": "pawn_block", "to": "e4", "pawn": target, "destination": "e4"},
            {"type": "opened_ray", "vacated_square": "e2", "end": "e5"},
            {"type": "rook_file", "file_type": "open", "supported_square": None, "helpers": []},
        ]
        for detail in details:
            row = copy.deepcopy(self.base)
            row.update(kind="local_fact", state="ready", selection="selected", detail={"actor": actor, **detail})
            row["highlight"] = {
                "occurrence_id": "occurrence", "text": "A local fact.", "squares": ["e2", "e4"],
                "premise_ids": [], "frames": [
                    {"phase": phase, "position_id": position, "squares": ["e2", "e4"], "source_ids": []}
                    for phase, position in (("before", "before"), ("after", "after"))
                ],
            }
            with self.subTest(kind=detail["type"]):
                self.validate(row)
                row["detail"]["importance"] = 1
                with self.assertRaises(ContractError):
                    self.validate(row)

    def test_scene_state_and_reference_list_shapes_are_explicit(self) -> None:
        scene = {"id": "scene", "root_occurrence_id": "occurrence", "occurrence_ids": ["occurrence"],
                 "line_ids": ["line"], "candidate_ids": ["candidate"], "state": "partial"}
        self.validate(self.comparison(False), scene)
        scene["candidate_ids"] = []
        with self.assertRaises(ContractError):
            self.validate(self.comparison(False), scene)

    def test_inquiry_states_and_evaluation_observation_shapes(self) -> None:
        piece = {"position_id": "root", "side": "white", "role": "knight", "square": "g1"}
        row = {"id": "inquiry", "scope": "root_choice", "source": "automatic_observation",
               "root_occurrence_id": "actual", "anchor_occurrence_ids": ["actual"], "position_id": "root",
               "ordering_source_ids": [],
               "reference_move": "g1f3", "alternative_move": "g1h3",
               "reasons": [{"kind": "same_actor_choice", "evidence_id": "position-owner",
                            "source_id": "a" * 64, "source_line_id": None, "anchor_move": "g1f3",
                            "actor": piece, "capture": None, "promotion_role": None, "responds_to_check": False}],
               "state": "deferred", "stop_reason": "investigation_limit", "answer": None}
        def validate() -> None:
            self.registry.validate_document(
                {"statements": [], "paths": [], "selection": {"version": 1, "candidates": [], "scenes": [], "inquiries": [row]}},
                ROOT / "schemas/public-v6/move-observations.schema.json", label="inquiry grammar")
        validate()
        row["state"] = "awaiting_engine"
        with self.assertRaises(ContractError):
            validate()
        row["stop_reason"] = None
        validate()
        evaluation = {"source": "investigation_comparison", "kind": "engine_search", "depth": 16,
                      "white_score": {"kind": "cp", "value": 20}}
        row.update(state="answered", answer={
            "work_id": "work:1", "execution_key_sha256": "b" * 64, "engine_profile": "sf18-smallnet-t2-h16-v1",
            "required_depth": 16, "reference_depth": 16, "alternative_depth": 16, "evaluation_relation": "equal",
            "reference_line_id": "left", "alternative_line_id": "right",
            "reference_occurrence_id": "left-edge", "alternative_occurrence_id": "right-edge",
            "reference_evaluation": evaluation, "alternative_evaluation": copy.deepcopy(evaluation)})
        validate()
        row["stop_reason"] = "deadline_exceeded"
        with self.assertRaises(ContractError):
            validate()
        row["assessment_effect"] = "needs_refresh"
        validate()
        row["answer"]["reference_evaluation"]["white_score"] = {"kind": "mate", "value": 0}
        with self.assertRaises(ContractError):
            validate()
        row.update(state="stopped", stop_reason="reference_changed", answer=None)
        validate()
        row["ordering_candidate_ids"] = []
        with self.assertRaises(ContractError):
            validate()
        row.update(explanation_candidate_ids=[], next_evidence="unresolved")
        validate()
        row["reasons"][0]["kind"] = "unregistered_reason"
        with self.assertRaises(ContractError):
            validate()


if __name__ == "__main__":
    unittest.main()
