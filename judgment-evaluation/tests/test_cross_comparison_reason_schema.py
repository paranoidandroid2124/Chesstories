from __future__ import annotations

import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def exposure_reasons(version: int) -> set[str]:
    schema_path = (
        ROOT
        / "schemas"
        / f"cause-audit-v{version}"
        / "actual-cascade-view.schema.json"
    )
    schema = json.loads(schema_path.read_text(encoding="utf-8"))
    exposure = schema["$defs"]["causeCascade"]["properties"]["r"]["properties"][
        "cross_comparison_exposure"
    ]
    object_branch = next(
        branch for branch in exposure["oneOf"] if branch.get("type") == "object"
    )
    return set(object_branch["properties"]["reason"]["enum"])


class CrossComparisonReasonSchemaTest(unittest.TestCase):
    def test_runtime_emittable_reasons_are_explicit_in_v2_and_v3(self) -> None:
        required = {
            "better_alternative_exposes_exact_played_liability",
            "conflicting_root_owned_effect_truth",
        }
        for version in (2, 3):
            with self.subTest(version=version):
                self.assertTrue(required.issubset(exposure_reasons(version)))

    def test_historical_v2_reason_remains_supported(self) -> None:
        self.assertIn("more_specific_equivalent_cause", exposure_reasons(2))


if __name__ == "__main__":
    unittest.main()
