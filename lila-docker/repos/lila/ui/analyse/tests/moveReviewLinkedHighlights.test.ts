import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { moveReviewObservations } from '../src/moveReview';
import { observationFixtureJob, type CertifiedObservationFixture } from './moveReviewTestSupport';

test('typed producer fixtures retain every delivered linked highlight, frame and independent fact', t => {
  const fixtures = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../research/role-use-implementation-2026-09-06/public-linked/fixtures.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ) as CertifiedObservationFixture[];
  assert.ok(fixtures.length > 0);
  let highlights = 0;
  let emptyPremises = 0;
  for (const fixture of fixtures) {
    const wire = fixture.response.observations as {
      highlights: {
        occurrence_id: string;
        text: string;
        squares: Key[];
        premise_ids: string[];
        frames: { phase: 'before' | 'after'; position_id: string; squares: Key[]; source_ids: string[] }[];
      }[];
      statements: unknown[];
      positions: unknown[];
      paths: unknown[];
    };
    const decoded = moveReviewObservations(observationFixtureJob(fixture))!;
    assert.ok(decoded, fixture.name);
    const expected = wire.highlights.map(h => {
      assert.ok(Array.isArray(h.premise_ids) && h.premise_ids.length <= 64, fixture.name);
      if (h.premise_ids.length === 0) emptyPremises++;
      assert.deepEqual(
        h.frames.map(f => f.phase),
        ['before', 'after'],
        fixture.name,
      );
      return {
        occurrenceId: h.occurrence_id,
        text: h.text,
        squares: h.squares,
        premiseIds: h.premise_ids,
        frames: h.frames.map(f => ({
          phase: f.phase,
          positionId: f.position_id,
          squares: f.squares,
          sourceIds: f.source_ids,
        })),
      };
    });
    assert.deepEqual(decoded.highlights ?? [], expected, fixture.name);
    assert.equal(decoded.statements.length, wire.statements.length, fixture.name);
    assert.equal(decoded.paths.length, wire.paths.length, fixture.name);
    assert.deepEqual(decoded.positions, wire.positions, fixture.name);
    highlights += expected.length;
  }
  assert.ok(highlights > 0, 'The real producer run must exercise linked highlight delivery');
  assert.ok(emptyPremises > 0, 'The compact producer export must exercise implicit edge/position owners');
  t.diagnostic(
    `${fixtures.length} real producer fixtures; ${highlights} linked highlights preserved exactly; ${emptyPremises} empty premise arrays`,
  );
});
