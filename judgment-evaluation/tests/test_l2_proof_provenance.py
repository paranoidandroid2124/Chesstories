from __future__ import annotations

import json
import re
import unittest
from pathlib import Path, PurePosixPath
from urllib.parse import urlparse


REPO = Path(__file__).resolve().parents[2]
REFERENCES = REPO / "judgment-evaluation" / "references"
MANIFEST = REFERENCES / "l2-proof-provenance.json"
SOURCE_INDEX = REFERENCES / "source-index.json"
SCHEMA = REPO / "judgment-evaluation" / "schemas" / "public-v6" / "move-meaning-response.schema.json"
BOUNDED_PROOF = REPO / "chesstory-runtime" / "src" / "main" / "scala" / "lila" / "chessjudgment" / "model" / "judgment" / "BoundedCausalProof.scala"
CAUSES = REPO / "chesstory-runtime" / "src" / "main" / "scala" / "lila" / "chessjudgment" / "model" / "judgment" / "RelativeMoveAssessment.scala"
DISPATCH = REPO / "chesstory-runtime" / "src" / "main" / "scala" / "lila" / "chessjudgment" / "analysis" / "assembly" / "RelativeAssessmentAssembler.scala"
PRODUCERS = DISPATCH.parent
RUNTIME_MAIN = REPO / "chesstory-runtime" / "src" / "main"

ALLOWED_COVERAGE = {"exact_ordered_skeleton", "motif_only", "exact_route_component"}
FORBIDDEN_REFERENCE_KEYS = {"local_path", "source_prose", "screenshot", "extracted_analysis"}


def _json(path: Path) -> dict[str, object]:
    return json.loads(path.read_text(encoding="utf-8"))


def _repo_path(raw: str) -> Path:
    relative = PurePosixPath(raw)
    if relative.is_absolute() or ".." in relative.parts:
        raise AssertionError(f"repository path must be relative and closed: {raw}")
    return REPO.joinpath(*relative.parts)


def _refs(value: object) -> set[str]:
    if isinstance(value, dict):
        direct = {
            ref.removeprefix("#/$defs/")
            for ref in [value.get("$ref")]
            if isinstance(ref, str) and ref.startswith("#/$defs/")
        }
        return direct | set().union(*(_refs(child) for child in value.values()), set())
    if isinstance(value, list):
        return set().union(*(_refs(child) for child in value), set())
    return set()


def _reachable_definitions(definitions: dict[str, object], start: str) -> set[str]:
    reached: set[str] = set()
    pending = [start]
    while pending:
        current = pending.pop()
        for target in _refs(definitions[current]):
            if target not in reached:
                reached.add(target)
                pending.append(target)
    return reached


class L2ProofProvenanceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.manifest = _json(MANIFEST)
        cls.contracts = cls.manifest["contracts"]
        cls.source_index = {
            document["document_id"]: document
            for document in _json(SOURCE_INDEX)["documents"]
        }
        cls.schema = _json(SCHEMA)

    def test_manifest_is_the_exact_active_private_contract_set(self) -> None:
        self.assertEqual(
            set(self.manifest),
            {"schema_version", "authority", "runtime_consumed", "cache_dependency", "contracts"},
        )
        self.assertEqual(self.manifest["schema_version"], "1.0.0")
        self.assertEqual(self.manifest["authority"], "ci_only_reference_provenance_not_board_truth")
        self.assertIs(self.manifest["runtime_consumed"], False)
        self.assertIs(self.manifest["cache_dependency"], False)

        contract_kinds = [contract["contract_kind"] for contract in self.contracts]
        producer_symbols = [contract["producer"]["symbol"] for contract in self.contracts]
        proof_types = [contract["proof_type"]["symbol"] for contract in self.contracts]
        public_fields = [contract["public_consumer"]["field"] for contract in self.contracts]
        browser_symbols = [contract["browser_consumer"]["projection_symbol"] for contract in self.contracts]
        browser_discriminants = [contract["browser_consumer"]["discriminant"] for contract in self.contracts]
        for values in (
            contract_kinds,
            producer_symbols,
            proof_types,
            public_fields,
            browser_symbols,
            browser_discriminants,
        ):
            self.assertEqual(len(values), len(set(values)))
        for contract in self.contracts:
            self.assertEqual(
                set(contract),
                {
                    "contract_kind",
                    "producer",
                    "proof_type",
                    "public_consumer",
                    "browser_consumer",
                    "cause",
                    "coverage_status",
                    "references",
                    "unsupported_claims",
                    "formal_test",
                },
            )

        enum_text = BOUNDED_PROOF.read_text(encoding="utf-8")
        enum_block = re.search(
            r"enum BoundedCausalContractKind:(.*?)(?=\n\s*def semanticNamespace)",
            enum_text,
            re.DOTALL,
        )
        self.assertIsNotNone(enum_block)
        active_contracts = set(re.findall(r"^\s+case (\w+)\s*$", enum_block.group(1), re.MULTILINE))
        self.assertEqual(set(contract_kinds), active_contracts)

        cause_text = CAUSES.read_text(encoding="utf-8")
        cause_enum_block = re.search(
            r"enum RelativeCauseKind:(.*?)(?=\n\ncase class RelativeCauseFact)",
            cause_text,
            re.DOTALL,
        )
        self.assertIsNotNone(cause_enum_block)
        active_causes = set(
            re.findall(r"^\s+case (\w+)\s*$", cause_enum_block.group(1), re.MULTILINE)
        )
        self.assertEqual(
            {contract["cause"]["scala_symbol"] for contract in self.contracts},
            active_causes,
            "every Cause name must be consumed by a reference-grounded typed L2 contract",
        )

        discovered_producers: list[tuple[str, str]] = []
        for path in PRODUCERS.glob("*.scala"):
            text = path.read_text(encoding="utf-8")
            if "EvidenceProducer.CausalProofProducer" not in text:
                continue
            symbols = re.findall(r"\bobject (\w+Assembler)\b", text)
            self.assertTrue(symbols, path.as_posix())
            self.assertEqual(len(symbols), len(set(symbols)), path.as_posix())
            discovered_producers.extend(
                (path.relative_to(REPO).as_posix(), symbol)
                for symbol in symbols
            )
        declared_producers = [
            (contract["producer"]["file"], contract["producer"]["symbol"])
            for contract in self.contracts
        ]
        self.assertCountEqual(declared_producers, discovered_producers)

    def test_references_are_registered_and_do_not_claim_more_than_the_sources(self) -> None:
        required_unsupported = {
            "VacatedGateEnablesUnrecapturableSliderCapture": {"general_preparation", "plan"},
            "SquareReleaseRoute": {
                "general_preparation",
                "global_unbounded_piece_token_identity",
                "good_move",
                "maneuver",
                "outpost",
                "plan",
                "sole_cause",
                "unbounded_vacancy",
            },
            "PassedPawnProgressRealizedAfterOnlyLegalReply": {"forced_win", "plan"},
        }
        for contract in self.contracts:
            status = contract["coverage_status"]
            self.assertIn(status, ALLOWED_COVERAGE)
            references = contract["references"]
            self.assertTrue(references)
            locators = [reference["locator"] for reference in references if "locator" in reference]
            self.assertTrue(
                locators,
                f"{contract['contract_kind']} needs at least one registered human-book locator",
            )
            coverages = {reference["coverage"] for reference in references}
            self.assertIn(status, coverages)
            if status != "exact_ordered_skeleton":
                self.assertNotIn("exact_ordered_skeleton", coverages)

            unsupported = contract["unsupported_claims"]
            self.assertEqual(unsupported, sorted(set(unsupported)))
            self.assertTrue(
                required_unsupported.get(contract["contract_kind"], set()).issubset(unsupported)
            )

            for reference in references:
                self.assertTrue(FORBIDDEN_REFERENCE_KEYS.isdisjoint(reference))
                self.assertIn(reference["coverage"], ALLOWED_COVERAGE)
                locator = reference.get("locator")
                external_url = reference.get("external_url")
                self.assertNotEqual(locator is None, external_url is None)
                if locator is not None:
                    self.assertEqual(set(locator), {"document_id", "pdf_page"})
                    self.assertIn(locator["document_id"], self.source_index)
                    document = self.source_index[locator["document_id"]]
                    self.assertGreaterEqual(locator["pdf_page"], 1)
                    self.assertLessEqual(locator["pdf_page"], document["page_count"])
                else:
                    parsed = urlparse(external_url)
                    self.assertEqual(parsed.scheme, "https")
                    self.assertTrue(parsed.netloc)

    def test_every_producer_reaches_one_existing_proof_consumer_and_formal_test(self) -> None:
        definitions = self.schema["$defs"]
        dispatch_text = DISPATCH.read_text(encoding="utf-8")
        cause_text = CAUSES.read_text(encoding="utf-8")
        manifest_name = MANIFEST.name
        self.assertFalse(any(manifest_name in path.read_text(encoding="utf-8") for path in RUNTIME_MAIN.rglob("*.scala")))

        for contract in self.contracts:
            producer = contract["producer"]
            producer_file = _repo_path(producer["file"])
            producer_text = producer_file.read_text(encoding="utf-8")
            self.assertEqual(len(re.findall(rf"\bobject {re.escape(producer['symbol'])}\b", producer_text)), 1)
            self.assertEqual(
                dispatch_text.count(f"{producer['symbol']}.fromDemand("),
                1,
                producer["symbol"],
            )

            proof_type = contract["proof_type"]
            proof_text = _repo_path(proof_type["file"]).read_text(encoding="utf-8")
            self.assertEqual(len(re.findall(rf"\bclass {re.escape(proof_type['symbol'])}\b", proof_text)), 1)

            cause = contract["cause"]
            self.assertEqual(
                len(re.findall(rf"^\s+case {re.escape(cause['scala_symbol'])}\s*$", cause_text, re.MULTILINE)),
                1,
            )
            facet = definitions[cause["schema_facet_definition"]]
            self.assertEqual(facet["properties"]["kind"]["const"], cause["wire_kind"])

            public = contract["public_consumer"]
            serializer_text = _repo_path(public["serializer_file"]).read_text(encoding="utf-8")
            self.assertEqual(
                len(re.findall(rf"\bdef {re.escape(public['serializer_symbol'])}\b", serializer_text)),
                1,
            )
            self.assertEqual(serializer_text.count(f'"{public["field"]}"'), 1)
            channel = definitions[public["schema_channel_definition"]]
            self.assertEqual(set(channel["required"]), {"channel_id", public["field"]})
            self.assertEqual(set(channel["properties"]), {"channel_id", public["field"]})
            field_owners = [
                name
                for name, definition in definitions.items()
                if public["field"] in definition.get("properties", {})
            ]
            self.assertEqual(field_owners, [public["schema_channel_definition"]])
            self.assertEqual(
                channel["properties"][public["field"]]["$ref"],
                f"#/$defs/{public['schema_proof_definition']}",
            )
            self.assertIn(
                public["schema_channel_definition"],
                _reachable_definitions(definitions, cause["schema_facet_definition"]),
            )

            browser = contract["browser_consumer"]
            self.assertEqual(
                set(browser),
                {"file", "projection_symbol", "discriminant"},
            )
            browser_file = _repo_path(browser["file"])
            browser_text = browser_file.read_text(encoding="utf-8")
            projection_symbol = browser["projection_symbol"]
            discriminant = browser["discriminant"]
            self.assertEqual(
                len(re.findall(rf"\bfunction {re.escape(projection_symbol)}\b", browser_text)),
                1,
            )
            self.assertEqual(browser_text.count(f"kind: '{discriminant}';"), 1)
            projection_start = browser_text.index(f"function {projection_symbol}(")
            projection_end = browser_text.find("\nfunction ", projection_start + 1)
            projection_text = browser_text[
                projection_start : projection_end if projection_end >= 0 else len(browser_text)
            ]
            self.assertIn(f"kind: '{discriminant}' as const", projection_text)

            causal_dispatch_start = browser_text.index("function projectCausalChannel(")
            causal_dispatch_end = browser_text.index(
                "\ninterface MoveReviewWireStep", causal_dispatch_start
            )
            causal_dispatch = browser_text[causal_dispatch_start:causal_dispatch_end]
            field_marker = f"'{public['field']}'"
            dispatch_start = causal_dispatch.index(field_marker)
            later_fields = [
                position
                for other_contract in self.contracts
                if other_contract is not contract
                for position in [
                    causal_dispatch.find(
                        f"'{other_contract['public_consumer']['field']}'",
                        dispatch_start + len(field_marker),
                    )
                ]
                if position >= 0
            ]
            dispatch_end = min(later_fields, default=len(causal_dispatch))
            dispatch_text_for_field = causal_dispatch[dispatch_start:dispatch_end]
            self.assertIn(f"return {projection_symbol}(", dispatch_text_for_field)
            self.assertNotIn(manifest_name, browser_text)

            formal_test = contract["formal_test"]
            test_text = _repo_path(formal_test["file"]).read_text(encoding="utf-8")
            self.assertEqual(len(re.findall(rf"\bclass {re.escape(formal_test['symbol'])}\b", test_text)), 1)
            self.assertIn('test("', test_text)


if __name__ == "__main__":
    unittest.main()
