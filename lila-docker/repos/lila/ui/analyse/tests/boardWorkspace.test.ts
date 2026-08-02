import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import * as Prefs from 'lib/prefs';
import {
  boardCoordsToViewConfig,
  boardLabelModeFromCoords,
  boardLabelModeToCoords,
} from '../src/boardWorkspace';

describe('board workspace label modes', () => {
  test('maps stored coordinates and workspace modes', () => {
    for (const [coords, mode] of [
      [Prefs.Coords.Hidden, 'off'],
      [Prefs.Coords.Inside, 'inside'],
      [Prefs.Coords.Outside, 'rim'],
      [Prefs.Coords.All, 'full'],
    ] as const) {
      assert.equal(boardLabelModeFromCoords(coords), mode);
      assert.equal(boardLabelModeToCoords(mode), coords);
    }
  });

  test('derives board-view flags from stored coordinate modes', () => {
    assert.deepEqual(boardCoordsToViewConfig(Prefs.Coords.Hidden), {
      coordinates: false,
      coordinatesOnSquares: false,
    });
    assert.deepEqual(boardCoordsToViewConfig(Prefs.Coords.Inside), {
      coordinates: true,
      coordinatesOnSquares: false,
    });
    assert.deepEqual(boardCoordsToViewConfig(Prefs.Coords.All), {
      coordinates: true,
      coordinatesOnSquares: true,
    });
  });
});
