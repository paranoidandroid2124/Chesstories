# PDF ideas v1 — development curation corpus

## Purpose and non-use boundary

This corpus is for development curation and in-session Codex/human semantic review only. It has no production, runtime, or test consumer.

It must not be used as an exact-string or label comparison set, a golden or snapshot source, engine truth, held-out evidence, confirmation evidence, blind-evaluation evidence, or an answer-key substitute. It does not provide expected-output strings or exact answer IDs.

The book-grounded ideas are concise paraphrased evidence, not copied source prose and not a claim that one explanation is uniquely correct. A reviewer may provide several valid explanations, foreground a different source-supported alternative or defensive resource, or abstain when the available evidence does not support a causal account.

Verdict confidence and Cause confidence are separate dimensions. A reviewer can be confident about a broad verdict while uncertain about the stated cause, or confident that a causal theme is present while uncertain about the overall verdict. Neither confidence should be inferred from the other.

## Source policy

[source-index.json](../../references/source-index.json) is the single authority for source identity. The only permitted locator is the pair of a document_id and one or more 1-based PDF pages. The corpus retains neither local filesystem paths nor visual source assets or long source quotations.

| Artifact directory | document_id |
| --- | --- |
| ai-revolution | ref-dc427a9593ab48248d19627b245fafaa |
| modern-benoni | ref-35c04c3495434a6290c20ee4712660a6 |
| positional-sacrifices | ref-bc7b3f1ff52f4c7a9940dad53cd6b975 |
| modernized-benko | ref-edf868ccf0a3425488f55ef6b4b08acf |
| basman-williams | ref-009749ddb7ef4fe0a033b33e9712fc74 |

Each book directory has three artifacts:

- candidates.md: concise source-paraphrased ideas, alternatives, and uncertainty.
- primary.pgn: the retained mainlines with SourceDocument and SourcePDFPages provenance tags.
- validation.md: source-page and one-time parse/replay notes.

Fragment eligibility and source completeness live beside the affected records
in their `candidates.md`, `primary.pgn`, and `validation.md`. This README does
not repeat derived record counts, byte checksums, or validation totals. Those
values are not a runtime contract and would become a second, manually updated
authority.

## Review handling

Read candidates.md as contextual, human-facing review material and primary.pgn as a finite source transcription. Do not transform the prose, position identity, or source result into a deterministic target. Preserve uncertainty where the source omits a result, a practical ending mechanism, a date, a round, or a uniquely compelling continuation.
