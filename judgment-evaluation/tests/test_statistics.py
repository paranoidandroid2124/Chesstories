from __future__ import annotations

import unittest

from chesstory_eval.statistics import (
    _paired_cluster_bootstrap_distribution,
    paired_cluster_contrast,
)


def paired_rows() -> list[dict[str, object]]:
    return [
        {
            "sample_id": sample,
            "atomic_cluster_id": cluster,
            "arm_id": arm,
            "E": value,
        }
        for sample, cluster, treatment, control in [
            ("a1", "a", 1, 0),
            ("a2", "a", 1, 0),
            ("b1", "b", 0, 1),
        ]
        for arm, value in [("treatment", treatment), ("control", control)]
    ]


class PairedClusterStatisticsTest(unittest.TestCase):
    def test_whole_clusters_preserve_sample_weighting(self) -> None:
        distribution = _paired_cluster_bootstrap_distribution(
            paired_rows(),
            treatment_arm="treatment",
            control_arm="control",
            value_key="E",
            iterations=200,
            seed=19,
        )
        self.assertEqual(distribution["estimate"], 1 / 3)
        self.assertEqual(distribution["n_samples"], 3)
        self.assertEqual(distribution["n_clusters"], 2)
        # Selecting two whole clusters can only give aa, ab/ba, or bb.
        self.assertEqual(set(distribution["bootstrap_estimates"]), {-1, 1 / 3, 1})

    def test_contrast_order_and_iterable_do_not_change_seeded_result(self) -> None:
        rows = paired_rows()
        for alternative in ("less", "greater"):
            with self.subTest(alternative=alternative):
                result = paired_cluster_contrast(
                    rows, "treatment", "control", iterations=200,
                    seed=19, confidence=0.8, alternative=alternative,
                )
                reversed_result = paired_cluster_contrast(
                    reversed(rows), "treatment", "control", iterations=200,
                    seed=19, confidence=0.8, alternative=alternative,
                )
                self.assertEqual(result, reversed_result)
                self.assertEqual(result["estimate"], 1 / 3)
                self.assertEqual(result["n_samples"], 3)
                self.assertEqual(result["n_clusters"], 2)
                self.assertEqual(result["confidence"], 0.8)
                self.assertIsNone(result["ci_lower" if alternative == "less" else "ci_upper"])
                self.assertEqual(result["ci_upper" if alternative == "less" else "ci_lower"],
                                 1 if alternative == "less" else -1)

    def test_missing_duplicate_and_changed_cluster_are_rejected(self) -> None:
        rows = paired_rows()
        invalid_sets = [
            (rows[:-1], "different sample sets"),
            (rows + [rows[0]], "duplicate row"),
            ([{**row, "atomic_cluster_id": "changed"} if i == 0 else row
              for i, row in enumerate(rows)], "changes atomic cluster"),
        ]
        for invalid, message in invalid_sets:
            with self.subTest(message=message), self.assertRaisesRegex(ValueError, message):
                paired_cluster_contrast(invalid, "treatment", "control", iterations=5)

    def test_arguments_are_validated_before_consuming_rows(self) -> None:
        def unreadable_rows():
            raise AssertionError("invalid arguments must not consume rows")
            yield  # pragma: no cover

        for arguments in ({"confidence": 1}, {"alternative": "two-sided"}, {"iterations": 0}):
            with self.subTest(arguments=arguments), self.assertRaises(ValueError):
                paired_cluster_contrast(unreadable_rows(), "treatment", "control", **arguments)


if __name__ == "__main__":
    unittest.main()
