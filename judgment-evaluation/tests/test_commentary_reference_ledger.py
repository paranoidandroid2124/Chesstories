from __future__ import annotations

import json
import unittest
from pathlib import Path


REPO = Path(__file__).resolve().parents[2]
REFERENCES = REPO / "judgment-evaluation" / "references"
LEDGER = REFERENCES / "commentary-reference-ledger.json"
SOURCE_INDEX = REFERENCES / "source-index.json"
OCCURRENCE_EVIDENCE = {"exact", "motif_only"}
FORBIDDEN_PROVENANCE_KEYS = {
    "local_path",
    "source_prose",
    "screenshot",
    "extracted_analysis",
}


def _json(path: Path) -> dict[str, object]:
    return json.loads(path.read_text(encoding="utf-8"))


def _all_keys(value: object) -> set[str]:
    if isinstance(value, dict):
        return set(value) | set().union(*(_all_keys(child) for child in value.values()), set())
    if isinstance(value, list):
        return set().union(*(_all_keys(child) for child in value), set())
    return set()


class CommentaryReferenceLedgerTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.document = _json(LEDGER)
        cls.entries = cls.document["cases"]
        cls.source_index = {
            document["document_id"]: document
            for document in _json(SOURCE_INDEX)["documents"]
        }

    def test_metadata_is_non_runtime_reference_observation(self) -> None:
        self.assertEqual(
            set(self.document),
            {
                "schema_version",
                "authority",
                "runtime_consumed",
                "cache_dependency",
                "cases",
            },
        )
        self.assertEqual(self.document["schema_version"], "3.0.0")
        self.assertEqual(
            self.document["authority"],
            "ci_only_reference_provenance_not_board_truth",
        )
        self.assertIs(self.document["runtime_consumed"], False)
        self.assertIs(self.document["cache_dependency"], False)

    def test_reference_cases_have_unique_identity_and_registered_locators(self) -> None:
        self.assertTrue(FORBIDDEN_PROVENANCE_KEYS.isdisjoint(_all_keys(self.document)))
        case_ids = [entry["case_id"] for entry in self.entries]
        self.assertTrue(case_ids)
        self.assertEqual(len(case_ids), len(set(case_ids)))

        for entry in self.entries:
            self.assertTrue({"case_id", "locators", "source_context"}.issubset(entry))
            self.assertIsInstance(entry["case_id"], str)
            self.assertTrue(entry["case_id"].strip())
            self.assertIsInstance(entry["source_context"], str)
            self.assertTrue(entry["source_context"].strip())

            for field, value in entry.items():
                if field in {"case_id", "locators", "source_context"}:
                    continue
                if field == "occurrence_evidence":
                    self.assertIn(value, OCCURRENCE_EVIDENCE)
                elif isinstance(value, str):
                    self.assertTrue(value.strip())
                else:
                    self.assertIsInstance(value, list)
                    self.assertTrue(value)
                    self.assertTrue(
                        all(isinstance(item, str) and item.strip() for item in value)
                    )

            locators = entry["locators"]
            self.assertTrue(locators)
            locator_keys: list[tuple[str, int]] = []
            for locator in locators:
                self.assertEqual(set(locator), {"document_id", "pdf_page"})
                document_id = locator["document_id"]
                page = locator["pdf_page"]
                self.assertIn(document_id, self.source_index)
                self.assertIs(type(page), int)
                self.assertGreaterEqual(page, 1)
                self.assertLessEqual(page, self.source_index[document_id]["page_count"])
                locator_keys.append((document_id, page))
            self.assertEqual(len(locator_keys), len(set(locator_keys)))


if __name__ == "__main__":
    unittest.main()
