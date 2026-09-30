from __future__ import annotations

import copy
import json
import unittest
from pathlib import Path

from chesstory_eval.model import ContractError
from chesstory_eval.schemas import SchemaRegistry


ROOT = Path(__file__).resolve().parents[1]


class HighlightLinksContractTest(unittest.TestCase):
    def setUp(self) -> None:
        fixtures = json.loads(
            (ROOT / "fixtures/public-commentary-v6/certified-observations-produced.json").read_text(encoding="utf-8")
        )
        self.observations = next(f for f in fixtures if f["name"] == "Bf3")["response"]["observations"]
        path = self.observations["paths"][0]
        positions = {p["id"]: p for p in self.observations["positions"]}
        self.highlight = {
            "occurrence_id": path["occurrence_ids"][0],
            "text": "The bishop move opens the queen's diagonal.",
            "squares": ["c2", "h2"],
            "premise_ids": self.observations["statements"][0]["premise_ids"],
            "frames": [
                {
                    "phase": phase,
                    "position_id": position_id,
                    "squares": ["c2", "h2"],
                    "source_ids": [positions[position_id]["rays"]["lines"][0]["source_id"]],
                }
                for phase, position_id in zip(("before", "after"), path["position_ids"])
            ],
        }
        self.registry = SchemaRegistry(ROOT / "schemas")

    def validate(self, highlight: dict) -> None:
        self.observations["highlights"] = [highlight]
        self.registry.validate_document(
            self.observations,
            ROOT / "schemas/public-v6/move-observations.schema.json",
            label="highlight links",
        )

    def test_complete_linked_and_exact_legacy_highlights_are_accepted(self) -> None:
        self.validate(self.highlight)
        single = copy.deepcopy(self.highlight)
        single["frames"] = single["frames"][:1]
        single["frames"][0]["source_ids"] = []
        single["premise_ids"] = []
        self.validate(single)
        single["premise_ids"] = [f"full-reference:{i}" for i in range(64)]
        self.validate(single)
        self.validate({k: self.highlight[k] for k in ("occurrence_id", "text", "squares")})

    def test_partial_links_and_malformed_reference_shapes_are_rejected(self) -> None:
        for field in ("premise_ids", "frames"):
            with self.subTest(missing=field):
                malformed = copy.deepcopy(self.highlight)
                del malformed[field]
                with self.assertRaises(ContractError):
                    self.validate(malformed)
        for field, values in {
            "premise_ids": (None, [""], [7], ["same", "same"], [f"full-reference:{i}" for i in range(65)]),
            "frames": ([], None, [None], [self.highlight["frames"][0]] * 3),
        }.items():
            for value in values:
                with self.subTest(field=field, value=value):
                    malformed = copy.deepcopy(self.highlight)
                    malformed[field] = value
                    with self.assertRaises(ContractError):
                        self.validate(malformed)
        for field, values in {
            "phase": ("transition", None),
            "position_id": ("", None),
            "squares": ([], ["a9"], ["h2", "h2"]),
            "source_ids": (["unknown"], ["f" * 64, "f" * 64], [7]),
        }.items():
            for value in values:
                with self.subTest(frame_field=field, value=value):
                    malformed = copy.deepcopy(self.highlight)
                    malformed["frames"][0][field] = value
                    with self.assertRaises(ContractError):
                        self.validate(malformed)
            malformed = copy.deepcopy(self.highlight)
            del malformed["frames"][0][field]
            with self.assertRaises(ContractError):
                self.validate(malformed)

    def test_duplicate_phases_and_extra_frame_fields_are_rejected(self) -> None:
        malformed = copy.deepcopy(self.highlight)
        malformed["frames"][1] = {**malformed["frames"][0], "squares": ["g2"]}
        with self.assertRaises(ContractError):
            self.validate(malformed)
        malformed = copy.deepcopy(self.highlight)
        malformed["frames"][0]["fen"] = self.observations["positions"][0]["fen"]
        with self.assertRaises(ContractError):
            self.validate(malformed)


if __name__ == "__main__":
    unittest.main()
