from __future__ import annotations

import json
import re
import unittest
from pathlib import Path, PurePosixPath


REPO = Path(__file__).resolve().parents[2]
REFERENCES = REPO / "judgment-evaluation" / "references"
MANIFEST = REFERENCES / "l2-proof-provenance.json"
SOURCE_INDEX = REFERENCES / "source-index.json"
BOUNDED_PROOF = (
    REPO
    / "chesstory-runtime"
    / "src"
    / "main"
    / "scala"
    / "lila"
    / "chessjudgment"
    / "model"
    / "judgment"
    / "BoundedCausalProof.scala"
)
ASSEMBLY = (
    REPO
    / "chesstory-runtime"
    / "src"
    / "main"
    / "scala"
    / "lila"
    / "chessjudgment"
    / "analysis"
    / "assembly"
)

CONTRACT_KEYS = {
    "contract_kind",
    "producer",
    "proof_type",
    "wire",
    "audit_mapping",
    "implementation_status",
    "reference_support",
    "references",
    "unsupported_claims",
    "formal_test",
}
AUDIT_MAPPING_KEYS = {
    "sole_producer",
    "coordinate_piece_identity_proposition",
    "lower_premises",
    "closed_inventory",
    "branch_occurrence",
    "proof_paths_transposition",
    "dependency_cache",
    "consumers",
    "no_recomputation_owner",
}
LEDGER_KEYS = {
    "case_id",
    "locators",
    "game",
    "position_anchor",
    "occurrence_evidence",
    "causal_proposition_support",
    "premise",
    "current_move_change",
    "branch_response",
    "pedagogical_alternatives",
    "later_consumer",
    "needed_closed_absence",
    "sibling_difference",
    "transposition_occurrence",
    "mapped_contracts",
    "current_coverage_categories",
}
REFERENCE_SUPPORT = {"exact_proposition_anchor", "exact_occurrence_component", "motif_only"}
COVERAGE_CATEGORIES = {
    "current_exact_proof",
    "lower_fact_gap",
    "l2_join_or_demand_gap",
    "higher_layer_responsibility",
}
FORBIDDEN_REFERENCE_KEYS = {"local_path", "source_prose", "screenshot", "extracted_analysis"}
STALE_OWNERSHIP_NAMES = {
    "RelativeCause",
    "WrongMoveOrder",
    "browser_consumer",
    "projectCausalChannel",
    "causal_explanations",
    "requested_explanations",
}


def _json(path: Path) -> dict[str, object]:
    return json.loads(path.read_text(encoding="utf-8"))


def _repo_path(raw: str) -> Path:
    relative = PurePosixPath(raw)
    if relative.is_absolute() or ".." in relative.parts:
        raise AssertionError(f"repository path must be relative and closed: {raw}")
    return REPO.joinpath(*relative.parts)


def _definition_refs(value: object) -> set[str]:
    if isinstance(value, dict):
        direct = {
            ref.removeprefix("#/$defs/")
            for ref in [value.get("$ref")]
            if isinstance(ref, str) and ref.startswith("#/$defs/")
        }
        return direct | set().union(*(_definition_refs(child) for child in value.values()), set())
    if isinstance(value, list):
        return set().union(*(_definition_refs(child) for child in value), set())
    return set()


def _all_keys(value: object) -> set[str]:
    if isinstance(value, dict):
        return set(value) | set().union(*(_all_keys(child) for child in value.values()), set())
    if isinstance(value, list):
        return set().union(*(_all_keys(child) for child in value), set())
    return set()


def _definition_closure(definitions: dict[str, object], start: str) -> set[str]:
    reached: set[str] = set()
    pending = [start]
    while pending:
        current = pending.pop()
        if current in reached:
            continue
        if current not in definitions:
            raise AssertionError(f"unknown schema definition: {current}")
        reached.add(current)
        pending.extend(_definition_refs(definitions[current]) - reached)
    return reached


class L2ProofProvenanceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.manifest = _json(MANIFEST)
        cls.contracts = cls.manifest["contracts"]
        cls.ledger = cls.manifest["complete_game_ledger"]
        cls.source_index = {
            document["document_id"]: document
            for document in _json(SOURCE_INDEX)["documents"]
        }
        schema_path = _repo_path(cls.manifest["host"]["schema"]["file"])
        cls.schema = _json(schema_path)

    def test_manifest_is_the_exact_active_contract_inventory(self) -> None:
        self.assertEqual(
            set(self.manifest),
            {
                "schema_version",
                "authority",
                "runtime_consumed",
                "cache_dependency",
                "host",
                "coverage_categories",
                "contracts",
                "complete_game_ledger",
            },
        )
        self.assertEqual(self.manifest["schema_version"], "2.0.0")
        self.assertEqual(self.manifest["authority"], "ci_only_reference_provenance_not_board_truth")
        self.assertIs(self.manifest["runtime_consumed"], False)
        self.assertIs(self.manifest["cache_dependency"], False)
        self.assertEqual(set(self.manifest["coverage_categories"]), COVERAGE_CATEGORIES)

        serialized = json.dumps(self.manifest, sort_keys=True)
        for stale in STALE_OWNERSHIP_NAMES:
            self.assertNotIn(stale, serialized)

        contract_kinds = [contract["contract_kind"] for contract in self.contracts]
        proof_types = [contract["proof_type"]["symbol"] for contract in self.contracts]
        formal_tests = [contract["formal_test"]["symbol"] for contract in self.contracts]
        producer_handlers = [
            (
                contract["producer"]["file"],
                contract["producer"]["symbol"],
                contract["producer"]["method"],
            )
            for contract in self.contracts
        ]
        for values in (contract_kinds, proof_types, formal_tests, producer_handlers):
            self.assertEqual(len(values), len(set(values)))
        self.assertEqual(len(self.contracts), 6)
        for contract in self.contracts:
            self.assertEqual(set(contract), CONTRACT_KEYS)
            self.assertEqual(set(contract["audit_mapping"]), AUDIT_MAPPING_KEYS)

        enum_text = BOUNDED_PROOF.read_text(encoding="utf-8")
        enum_block = re.search(
            r"enum BoundedCausalContractKind:(.*?)(?=\n\s*def semanticNamespace)",
            enum_text,
            re.DOTALL,
        )
        self.assertIsNotNone(enum_block)
        active_contracts = set(
            re.findall(r"^\s+case (\w+)\s*$", enum_block.group(1), re.MULTILINE)
        )
        self.assertEqual(set(contract_kinds), active_contracts)

    def test_occurrence_demand_is_the_only_public_explanation_host(self) -> None:
        host = self.manifest["host"]
        demand_text = _repo_path(host["demand"]["file"]).read_text(encoding="utf-8")
        self.assertRegex(demand_text, rf"\bcase class {host['demand']['symbol']}\b")
        self.assertIn("availableBranches", demand_text)
        for forbidden in ("CandidateComparisonKind", "MoveVerdict", "actionable", "threshold"):
            self.assertNotIn(forbidden, demand_text)

        orchestrator = host["orchestrator"]
        orchestrator_text = _repo_path(orchestrator["file"]).read_text(encoding="utf-8")
        self.assertRegex(orchestrator_text, rf"\bobject {orchestrator['symbol']}\b")
        demand_index = orchestrator_text.index("OccurrenceExplanationDemand.resolve")
        proof_index = orchestrator_text.index(f"{orchestrator['proof_entrypoint']}(")
        assessment_index = orchestrator_text.index("RelativeAssessmentAssembler.enrichAssessment")
        cause_index = orchestrator_text.index(f"{orchestrator['cause_entrypoint']}(")
        self.assertLess(demand_index, proof_index)
        self.assertLess(proof_index, assessment_index)
        self.assertLess(assessment_index, cause_index)

        cause = host["cause"]
        cause_text = _repo_path(cause["file"]).read_text(encoding="utf-8")
        evidence_text = _repo_path(cause["evidence_file"]).read_text(encoding="utf-8")
        self.assertRegex(cause_text, rf"\bcase class {cause['symbol']}\b")
        self.assertIn("proofOwnsSubject", cause_text)
        self.assertRegex(evidence_text, rf"\bcase class {cause['evidence_symbol']}\b")

        occurrence_assembler = (ASSEMBLY / "OccurrenceExplanationAssembler.scala").read_text(
            encoding="utf-8"
        )
        self.assertEqual(occurrence_assembler.count("OccurrenceExplanationCause("), 1)
        self.assertEqual(
            occurrence_assembler.count("EvidenceProducer.OccurrenceExplanationProducer"), 1
        )

        packet = host["packet"]
        packet_text = _repo_path(packet["file"]).read_text(encoding="utf-8")
        self.assertRegex(packet_text, rf"\bclass {packet['symbol']}\b")
        self.assertRegex(packet_text, rf"\bval {packet['field']}: List\[CertifiedOccurrenceExplanation\]")
        self.assertIn("CertifiedOccurrenceExplanation.fromRecord", packet_text)

        runtime = host["runtime"]
        runtime_text = _repo_path(runtime["file"]).read_text(encoding="utf-8")
        self.assertRegex(runtime_text, rf"\bdef {runtime['serializer_symbol']}\b")
        self.assertIn(f"packet.{packet['field']}.map({runtime['serializer_symbol']})", runtime_text)
        self.assertEqual(runtime_text.count(f'"{runtime["field"]}" -> occurrenceExplanations'), 1)
        self.assertNotIn("causal_explanations", runtime_text)
        self.assertNotIn("requested_explanations", runtime_text)

    def test_each_family_has_one_producer_proof_wire_and_formal_test(self) -> None:
        declared_producer_files = {contract["producer"]["file"] for contract in self.contracts}
        discovered_producer_files = {
            path.relative_to(REPO).as_posix()
            for path in ASSEMBLY.glob("*.scala")
            if "EvidenceProducer.CausalProofProducer" in path.read_text(encoding="utf-8")
        }
        self.assertEqual(discovered_producer_files, declared_producer_files)

        runtime_text = _repo_path(self.manifest["host"]["runtime"]["file"]).read_text(
            encoding="utf-8"
        )
        serializer_start = runtime_text.index(
            f"private def {self.manifest['host']['runtime']['serializer_symbol']}("
        )
        serializer_end = runtime_text.index("\n    private def causalBranchJson", serializer_start)
        serializer = runtime_text[serializer_start:serializer_end]

        definitions = self.schema["$defs"]
        union_name = self.manifest["host"]["schema"]["union_definition"]
        union_targets = {
            item["$ref"].removeprefix("#/$defs/")
            for item in definitions[union_name]["oneOf"]
        }
        self.assertEqual(
            union_targets,
            {contract["wire"]["variant_definition"] for contract in self.contracts},
        )

        for contract in self.contracts:
            producer = contract["producer"]
            producer_path = _repo_path(producer["file"])
            producer_text = producer_path.read_text(encoding="utf-8")
            self.assertEqual(
                len(re.findall(rf"\bobject {re.escape(producer['symbol'])}\b", producer_text)), 1
            )
            self.assertEqual(
                len(re.findall(rf"\bdef {re.escape(producer['method'])}\s*\(", producer_text)), 1
            )

            proof = contract["proof_type"]
            proof_text = _repo_path(proof["file"]).read_text(encoding="utf-8")
            self.assertEqual(
                len(re.findall(rf"\bclass {re.escape(proof['symbol'])}\b", proof_text)), 1
            )
            self.assertIn(proof["symbol"], producer_text)

            formal_test = contract["formal_test"]
            formal_text = _repo_path(formal_test["file"]).read_text(encoding="utf-8")
            self.assertEqual(
                len(re.findall(rf"\bclass {re.escape(formal_test['symbol'])}\b", formal_text)), 1
            )
            self.assertIn('test("', formal_text)

            wire = contract["wire"]
            variant = definitions[wire["variant_definition"]]
            self.assertIs(variant["additionalProperties"], False)
            self.assertEqual(
                set(variant["required"]),
                {"cause_evidence_id", "subject_occurrence", "proof_kind", wire["proof_field"]},
            )
            self.assertEqual(variant["properties"]["proof_kind"]["const"], wire["proof_kind"])
            self.assertEqual(
                variant["properties"][wire["proof_field"]]["$ref"],
                f"#/$defs/{wire['proof_definition']}",
            )
            self.assertIs(definitions[wire["proof_definition"]]["additionalProperties"], False)
            self.assertIn(wire["proof_kind"], serializer)
            self.assertIn(f'"{wire["proof_field"]}" ->', serializer)
            self.assertNotIn(f'"{wire["proof_kind"]}_proof" ->', serializer)

    def test_every_contract_answers_the_nine_authority_questions(self) -> None:
        definitions = self.schema["$defs"]
        host = self.manifest["host"]
        self.assertEqual(
            [
                contract["contract_kind"]
                for contract in self.contracts
                if "direct_fixture" in contract["audit_mapping"]["consumers"]
            ],
            ["CaptureExclusionMoveOrder"],
        )
        for contract in self.contracts:
            audit = contract["audit_mapping"]
            producer = contract["producer"]
            self.assertEqual(
                audit["sole_producer"],
                f"{producer['file']}#{producer['symbol']}.{producer['method']}",
            )

            wire = contract["wire"]
            closure = _definition_closure(definitions, wire["proof_definition"])
            schema_fragment = json.dumps(
                {name: definitions[name] for name in sorted(closure)}, sort_keys=True
            )

            proposition = audit["coordinate_piece_identity_proposition"]
            self.assertEqual(set(proposition), {"statement", "wire_fields"})
            self.assertTrue(proposition["statement"].strip())
            self.assertTrue(proposition["wire_fields"])
            for field in proposition["wire_fields"]:
                self.assertIn(f'"{field}"', schema_fragment)

            lower = audit["lower_premises"]
            self.assertEqual(
                set(lower), {"contracts", "dynamic_id_fields", "issuer_source_kinds"}
            )
            for contract_name in lower["contracts"]:
                self.assertIn(contract_name, schema_fragment)
            for field in lower["dynamic_id_fields"]:
                self.assertIn(f'"{field}"', schema_fragment)
            self.assertTrue(all(value.strip() for value in lower["issuer_source_kinds"]))

            inventory = audit["closed_inventory"]
            self.assertEqual(
                set(inventory), {"issuers", "fields", "certified_absence_or_state"}
            )
            for issuer in inventory["issuers"]:
                self.assertIn(issuer, schema_fragment)
            for field in inventory["fields"]:
                self.assertIn(f'"{field}"', schema_fragment)
            self.assertTrue(inventory["certified_absence_or_state"])

            branch = audit["branch_occurrence"]
            self.assertEqual(
                set(branch), {"roles", "identity_fields", "provenance_fields"}
            )
            for role in branch["roles"]:
                self.assertIn(role, schema_fragment)
            for field in branch["identity_fields"] + branch["provenance_fields"]:
                self.assertIn(f'"{field}"', schema_fragment)

            paths = audit["proof_paths_transposition"]
            self.assertEqual(set(paths), {"cardinality", "identity_fields", "rule"})
            self.assertIn(paths["cardinality"], {"exactly_one_current_contract", "one_or_more"})
            for field in paths["identity_fields"]:
                self.assertIn(f'"{field}"', schema_fragment)
            self.assertIn("transposition", paths["rule"])

            dependency = audit["dependency_cache"]
            self.assertEqual(
                set(dependency), {"changed_dependencies", "fingerprint_field", "cache_owner"}
            )
            self.assertGreaterEqual(len(dependency["changed_dependencies"]), 5)
            self.assertEqual(dependency["fingerprint_field"], "dependency_fingerprint")
            self.assertIn('"dependency_fingerprint"', schema_fragment)
            self.assertTrue(dependency["cache_owner"].strip())

            consumers = audit["consumers"]
            expected_consumer_keys = {
                "cause",
                "packet",
                "runtime_field",
                "schema_variant",
                "runtime_producer_tests",
                "completed_through",
                "ui_status",
                "ui_owner",
                "ui_required_fields",
            }
            if "direct_fixture" in consumers:
                expected_consumer_keys.add("direct_fixture")
            self.assertEqual(
                set(consumers),
                expected_consumer_keys,
            )
            self.assertEqual(consumers["cause"], host["cause"]["symbol"])
            self.assertEqual(
                consumers["packet"], f"{host['packet']['symbol']}.{host['packet']['field']}"
            )
            self.assertEqual(consumers["runtime_field"], host["runtime"]["field"])
            self.assertEqual(consumers["schema_variant"], wire["variant_definition"])

            runtime_tests = consumers["runtime_producer_tests"]
            self.assertEqual(set(runtime_tests), {"file", "cases"})
            runtime_test_file = _repo_path(runtime_tests["file"])
            self.assertTrue(runtime_test_file.is_file())
            runtime_test_text = runtime_test_file.read_text(encoding="utf-8")
            runtime_test_lines = runtime_test_text.splitlines()
            self.assertTrue(runtime_tests["cases"])
            test_names = [case["name"] for case in runtime_tests["cases"]]
            self.assertEqual(len(test_names), len(set(test_names)))
            for test_case in runtime_tests["cases"]:
                self.assertEqual(set(test_case), {"line", "name"})
                test_line = test_case["line"]
                test_name = test_case["name"]
                self.assertIsInstance(test_line, int)
                self.assertGreaterEqual(test_line, 1)
                self.assertLessEqual(test_line, len(runtime_test_lines))
                self.assertEqual(
                    runtime_test_lines[test_line - 1].strip(),
                    f'test("{test_name}"):',
                )
                self.assertEqual(runtime_test_text.count(f'test("{test_name}")'), 1)

            direct_fixture = consumers.get("direct_fixture")
            if direct_fixture is None:
                expected_completion = (
                    "public schema and producer-based RuntimeProtocol test coverage"
                )
            else:
                self.assertEqual(
                    set(direct_fixture), {"file", "proof_kind", "producer_test_name"}
                )
                self.assertEqual(direct_fixture["proof_kind"], wire["proof_kind"])
                self.assertIn(direct_fixture["producer_test_name"], test_names)
                fixture = _json(_repo_path(direct_fixture["file"]))
                occurrence_explanations = [
                    explanation
                    for review in fixture["result"]["selected_move_reviews"]
                    for explanation in review.get("commentary", {}).get(
                        host["runtime"]["field"], []
                    )
                ]
                self.assertTrue(occurrence_explanations)
                self.assertEqual(
                    {item["proof_kind"] for item in occurrence_explanations},
                    {wire["proof_kind"]},
                )
                expected_completion = (
                    "public schema, producer-based RuntimeProtocol test coverage, "
                    "and immutable Scala-produced fixture"
                )
            self.assertEqual(
                consumers["completed_through"],
                expected_completion,
            )
            self.assertEqual(consumers["ui_status"], "pending_completion")
            self.assertEqual(consumers["ui_owner"], "separate frontend task")
            self.assertEqual(
                set(consumers["ui_required_fields"]),
                {"subject", "branches", "steps", "pieces", "closures", "proof_paths"},
            )

            self.assertTrue(audit["no_recomputation_owner"].strip())
            self.assertRegex(
                audit["no_recomputation_owner"], r"(does not|remains the owner)"
            )

    def test_all_public_schema_definitions_are_reachable(self) -> None:
        definitions = self.schema["$defs"]
        root = {key: value for key, value in self.schema.items() if key != "$defs"}
        reached: set[str] = set()
        pending = list(_definition_refs(root))
        while pending:
            current = pending.pop()
            self.assertIn(current, definitions)
            if current in reached:
                continue
            reached.add(current)
            pending.extend(_definition_refs(definitions[current]) - reached)
        self.assertEqual(reached, set(definitions))

        schema_host = self.manifest["host"]["schema"]
        move_commentary = definitions["moveCommentary"]
        root_property = schema_host["root_property"]
        self.assertEqual(
            move_commentary["properties"][root_property]["items"]["$ref"],
            f"#/$defs/{schema_host['union_definition']}",
        )
        self.assertNotIn("causal_explanations", move_commentary["properties"])
        self.assertNotIn("requested_explanations", move_commentary["properties"])

    def test_sources_and_complete_game_ledger_are_closed(self) -> None:
        self.assertTrue(FORBIDDEN_REFERENCE_KEYS.isdisjoint(_all_keys(self.manifest)))
        contract_kinds = {contract["contract_kind"] for contract in self.contracts}
        for contract in self.contracts:
            self.assertEqual(contract["implementation_status"], "current_exact_proof")
            self.assertIn(contract["reference_support"], REFERENCE_SUPPORT)
            self.assertTrue(contract["references"])
            self.assertEqual(contract["unsupported_claims"], sorted(set(contract["unsupported_claims"])))
            supports = {reference["support"] for reference in contract["references"]}
            self.assertIn(contract["reference_support"], supports)
            for reference in contract["references"]:
                self.assertEqual(set(reference), {"support", "locator"})
                self.assertIn(reference["support"], REFERENCE_SUPPORT)
                self._assert_registered_locator(reference["locator"])

        expected_locator_pages = {
            "ruy-lopez-game-12": ("ref-b6054fc614ae9d9c17933ce856013081", 78, 83),
            "ruy-lopez-game-15": ("ref-b6054fc614ae9d9c17933ce856013081", 98, 105),
            "najdorf-game-15": ("ref-b7e552e8d8914cdc31ce1002fbb8252e", 123, 132),
            "najdorf-game-16": ("ref-b7e552e8d8914cdc31ce1002fbb8252e", 132, 145),
            "najdorf-game-18": ("ref-b7e552e8d8914cdc31ce1002fbb8252e", 152, 164),
            "game-changer-bold-sir-lancelot": (
                "ref-4f1abd0745b24746b58928d162f837f6",
                158,
                164,
            ),
            "silicon-road-game-33": ("ref-7a451d5c87b9422a901f0486386aeb44", 269, 272),
            "van-delft-game-24": ("ref-bc7b3f1ff52f4c7a9940dad53cd6b975", 124, 129),
            "van-delft-game-20": ("ref-bc7b3f1ff52f4c7a9940dad53cd6b975", 106, 109),
            "van-delft-game-54": ("ref-bc7b3f1ff52f4c7a9940dad53cd6b975", 264, 269),
            "game-changer-not-so-quiet-game": (
                "ref-4f1abd0745b24746b58928d162f837f6",
                344,
                353,
            ),
            "game-changer-bishop-interference-illustrations": (
                "ref-4f1abd0745b24746b58928d162f837f6",
                417,
            ),
            "game-changer-bb7-bishop-interference": (
                "ref-4f1abd0745b24746b58928d162f837f6",
                417,
                420,
            ),
            "game-changer-bxc3-bishop-interference": (
                "ref-4f1abd0745b24746b58928d162f837f6",
                421,
                424,
            ),
            "silicon-road-game-64": ("ref-7a451d5c87b9422a901f0486386aeb44", 503, 520),
            "silicon-road-game-97": ("ref-7a451d5c87b9422a901f0486386aeb44", 795, 812),
            "silicon-road-game-101": ("ref-7a451d5c87b9422a901f0486386aeb44", 835, 846),
            "ai-revolution-square-release-goryachkina-dubov": (
                "ref-dc427a9593ab48248d19627b245fafaa",
                124,
                125,
            ),
            "ai-revolution-square-release-prohaszka-wang": (
                "ref-dc427a9593ab48248d19627b245fafaa",
                130,
                131,
            ),
            "ai-revolution-passed-pawn-route": (
                "ref-dc427a9593ab48248d19627b245fafaa",
                507,
                508,
            ),
        }
        self.assertEqual({entry["case_id"] for entry in self.ledger}, set(expected_locator_pages))
        used_categories: set[str] = set()
        for entry in self.ledger:
            self.assertEqual(set(entry), LEDGER_KEYS)
            self.assertIn(entry["occurrence_evidence"], {"exact", "motif_only"})
            self.assertIn(
                entry["causal_proposition_support"],
                {"exact", "exact_occurrence_component", "motif_only"},
            )
            for field in (
                "premise",
                "current_move_change",
                "branch_response",
                "pedagogical_alternatives",
                "later_consumer",
                "needed_closed_absence",
                "sibling_difference",
                "transposition_occurrence",
            ):
                self.assertTrue(entry[field], f"{entry['case_id']} needs {field}")
                self.assertTrue(all(isinstance(value, str) and value.strip() for value in entry[field]))
            self.assertTrue(set(entry["mapped_contracts"]).issubset(contract_kinds))
            categories = set(entry["current_coverage_categories"])
            self.assertTrue(categories)
            self.assertTrue(categories.issubset(COVERAGE_CATEGORIES))
            used_categories.update(categories)
            if "current_exact_proof" in categories:
                self.assertTrue(entry["mapped_contracts"])
                self.assertIn(
                    entry["causal_proposition_support"], {"exact", "exact_occurrence_component"}
                )

            document_id, *pages = expected_locator_pages[entry["case_id"]]
            self.assertEqual(pages, list(dict.fromkeys(pages)))
            self.assertEqual(
                entry["locators"],
                [
                    {"document_id": document_id, "pdf_page": page}
                    for page in pages
                ],
            )
            locator_keys = [
                (locator["document_id"], locator["pdf_page"])
                for locator in entry["locators"]
            ]
            self.assertEqual(
                len(locator_keys),
                len(set(locator_keys)),
                f"{entry['case_id']} has duplicate locators",
            )
            for locator in entry["locators"]:
                self._assert_registered_locator(locator)
        self.assertEqual(used_categories, COVERAGE_CATEGORIES)

        ledger_by_id = {entry["case_id"]: entry for entry in self.ledger}
        goryachkina = ledger_by_id["ai-revolution-square-release-goryachkina-dubov"]
        self.assertEqual(goryachkina["causal_proposition_support"], "exact_occurrence_component")
        self.assertEqual(goryachkina["current_coverage_categories"], ["lower_fact_gap"])
        self.assertNotIn(
            "retained-blocker sibling keeps b6 occupied",
            json.dumps(goryachkina).lower(),
        )

        passed_pawn = ledger_by_id["ai-revolution-passed-pawn-route"]
        self.assertEqual(passed_pawn["causal_proposition_support"], "exact_occurrence_component")
        self.assertEqual(passed_pawn["current_coverage_categories"], ["lower_fact_gap"])

        ruy_game_15 = ledger_by_id["ruy-lopez-game-15"]
        self.assertEqual(ruy_game_15["occurrence_evidence"], "exact")
        self.assertEqual(ruy_game_15["causal_proposition_support"], "exact")
        self.assertIn("analysis branch", ruy_game_15["game"].lower())
        self.assertIn("not the leko-navara main line", ruy_game_15["position_anchor"].lower())
        self.assertTrue(
            all("analysis" in response.lower() for response in ruy_game_15["branch_response"])
        )
        serialized_ruy_game_15 = json.dumps(ruy_game_15).lower()
        self.assertNotIn("complete game records", serialized_ruy_game_15)
        self.assertNotIn("actual line", serialized_ruy_game_15)
        self.assertEqual(ruy_game_15["mapped_contracts"], [])
        self.assertEqual(
            ruy_game_15["current_coverage_categories"], ["l2_join_or_demand_gap"]
        )

        bishop_illustrations = ledger_by_id[
            "game-changer-bishop-interference-illustrations"
        ]
        self.assertEqual(bishop_illustrations["occurrence_evidence"], "motif_only")
        self.assertEqual(bishop_illustrations["causal_proposition_support"], "motif_only")
        self.assertEqual(
            bishop_illustrations["current_coverage_categories"],
            ["higher_layer_responsibility"],
        )
        self.assertIn(
            "do not represent actual positions",
            bishop_illustrations["position_anchor"].lower(),
        )
        serialized_bishop_illustrations = json.dumps(bishop_illustrations).lower()
        self.assertIsNone(
            re.search(r"\b\d{1,2}(?:\.\.\.|\.)[a-z]", serialized_bishop_illustrations),
            "non-actual illustrations must not contain a numbered replay occurrence",
        )
        self.assertEqual(bishop_illustrations["mapped_contracts"], [])

        bb7_bishop = ledger_by_id["game-changer-bb7-bishop-interference"]
        self.assertEqual(bb7_bishop["occurrence_evidence"], "exact")
        self.assertEqual(bb7_bishop["causal_proposition_support"], "exact_occurrence_component")
        self.assertEqual(
            bb7_bishop["current_coverage_categories"],
            ["l2_join_or_demand_gap", "higher_layer_responsibility"],
        )
        self.assertEqual(bb7_bishop["mapped_contracts"], [])
        bb7_anchor = bb7_bishop["position_anchor"].lower()
        self.assertIn("12...bb7", bb7_anchor)
        self.assertNotIn("12...bxc3", bb7_anchor)
        bb7_sources = " ".join(bb7_bishop["branch_response"]).lower()
        for source_token in (
            "practice occurrence",
            "13...re8",
            "alphazero-analysis occurrence",
            "13...nd7",
        ):
            self.assertIn(source_token, bb7_sources)

        bxc3_bishop = ledger_by_id["game-changer-bxc3-bishop-interference"]
        self.assertEqual(bxc3_bishop["occurrence_evidence"], "exact")
        self.assertEqual(
            bxc3_bishop["causal_proposition_support"], "exact_occurrence_component"
        )
        self.assertEqual(
            bxc3_bishop["current_coverage_categories"],
            ["l2_join_or_demand_gap", "higher_layer_responsibility"],
        )
        self.assertEqual(bxc3_bishop["mapped_contracts"], [])
        bxc3_anchor = bxc3_bishop["position_anchor"].lower()
        self.assertIn("12...bxc3", bxc3_anchor)
        self.assertNotIn("12...bb7", bxc3_anchor)
        bxc3_sources = " ".join(bxc3_bishop["branch_response"]).lower()
        for source_token in (
            "13.qh4 practice game",
            "13...kh8 analysis",
            "13.ne2 practice game",
        ):
            self.assertIn(source_token, bxc3_sources)

        exact_cases = {
            entry["case_id"]
            for entry in self.ledger
            if "current_exact_proof" in entry["current_coverage_categories"]
        }
        self.assertEqual(
            exact_cases,
            {"ruy-lopez-game-12", "ai-revolution-square-release-prohaszka-wang"},
        )

    def _assert_registered_locator(self, locator: dict[str, object]) -> None:
        self.assertEqual(set(locator), {"document_id", "pdf_page"})
        document_id = locator["document_id"]
        self.assertIn(document_id, self.source_index)
        page = locator["pdf_page"]
        self.assertIsInstance(page, int)
        self.assertGreaterEqual(page, 1)
        self.assertLessEqual(page, self.source_index[document_id]["page_count"])


if __name__ == "__main__":
    unittest.main()
