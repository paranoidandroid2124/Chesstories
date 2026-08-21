from __future__ import annotations

import copy
import json
import unittest
from pathlib import Path

from chesstory_eval.model import ContractError
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]
SCHEMAS = ROOT / "schemas" / "public-move-review-v5"


class MoveReviewV5ContractTest(unittest.TestCase):
    def setUp(self) -> None:
        self.registry = SchemaRegistry(SCHEMAS)

    def fixture(self, name: str) -> object:
        return json.loads((SCHEMAS / name).read_text(encoding="utf-8"))

    def test_fixtures_match_the_three_v5_wire_schemas(self) -> None:
        pairs = (
            ("move-review-job-request.schema.json", "move-review-job-request.fixture.json"),
            ("move-review-snapshot.schema.json", "move-review-completed.fixture.json"),
            (
                "move-review-engine-work-report.schema.json",
                "move-review-engine-work-report.fixture.json",
            ),
        )
        for schema_name, fixture_name in pairs:
            with self.subTest(fixture=fixture_name):
                self.registry.validate_document(
                    self.fixture(fixture_name),
                    SCHEMAS / schema_name,
                    label=fixture_name,
                )

    def test_snapshot_contract_rejects_an_extra_key(self) -> None:
        fixture = copy.deepcopy(self.fixture("move-review-completed.fixture.json"))
        assert isinstance(fixture, dict)
        fixture["legacy_commentary"] = True
        with self.assertRaises(ContractError):
            self.registry.validate_document(
                fixture,
                SCHEMAS / "move-review-snapshot.schema.json",
                label="snapshot with extra key",
            )

    def test_completed_snapshot_rejects_legacy_prose_and_judgment_fields(self) -> None:
        mutations = (
            ("core", "headline", "Backend prose"),
            ("core", "is_book", True),
            ("core", "judgment_basis", {}),
            ("core", "primary_reason", None),
            ("reason", "reason_code", "development"),
            ("reason", "title", "Development"),
            ("reason", "explanation", "Backend prose"),
            ("proof", "moves_uci", ["g1f3"]),
        )
        for owner, key, value in mutations:
            with self.subTest(field=key):
                fixture = copy.deepcopy(self.fixture("move-review-completed.fixture.json"))
                assert isinstance(fixture, dict)
                if owner == "core":
                    target = fixture["core"]
                elif owner == "reason":
                    target = fixture["evidence"]["reasons"][0]
                else:
                    target = fixture["evidence"]["proofs"][0]
                target[key] = value
                with self.assertRaises(ContractError):
                    self.registry.validate_document(
                        fixture,
                        SCHEMAS / "move-review-snapshot.schema.json",
                        label=f"snapshot with legacy {key}",
                    )

    def test_each_proof_step_requires_backend_san_and_resulting_fen(self) -> None:
        for required_field in ("move_san", "fen_after"):
            with self.subTest(field=required_field):
                fixture = copy.deepcopy(self.fixture("move-review-completed.fixture.json"))
                assert isinstance(fixture, dict)
                del fixture["evidence"]["proofs"][0]["steps"][0][required_field]
                with self.assertRaises(ContractError):
                    self.registry.validate_document(
                        fixture,
                        SCHEMAS / "move-review-snapshot.schema.json",
                        label=f"proof step without {required_field}",
                    )

    def test_primary_and_support_reason_ref_boundaries(self) -> None:
        accepted = (
            ("reason.development", []),
            ("reason.development", ["reason.support.1"]),
            (
                "reason.development",
                ["reason.support.1", "reason.support.2"],
            ),
            (None, []),
        )
        for primary_ref, support_refs in accepted:
            with self.subTest(primary=primary_ref, support_count=len(support_refs)):
                fixture = copy.deepcopy(self.fixture("move-review-completed.fixture.json"))
                assert isinstance(fixture, dict)
                fixture["core"]["primary_reason_ref"] = primary_ref
                fixture["core"]["support_reason_refs"] = support_refs
                self.registry.validate_document(
                    fixture,
                    SCHEMAS / "move-review-snapshot.schema.json",
                    label="snapshot at accepted reason-ref boundary",
                )

        rejected = (
            (
                "reason.development",
                ["reason.support.1", "reason.support.2", "reason.support.3"],
            ),
            (None, ["reason.support.1"]),
        )
        for primary_ref, support_refs in rejected:
            with self.subTest(primary=primary_ref, support_count=len(support_refs)):
                fixture = copy.deepcopy(self.fixture("move-review-completed.fixture.json"))
                assert isinstance(fixture, dict)
                fixture["core"]["primary_reason_ref"] = primary_ref
                fixture["core"]["support_reason_refs"] = support_refs
                with self.assertRaises(ContractError):
                    self.registry.validate_document(
                        fixture,
                        SCHEMAS / "move-review-snapshot.schema.json",
                        label="snapshot beyond reason-ref boundary",
                    )


if __name__ == "__main__":
    unittest.main()
