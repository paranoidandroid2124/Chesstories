import { h, type VNode } from 'snabbdom';
import { moveReviewCopy, type MoveReviewLocale, type MoveReviewViewState } from '../src/moveReview';
import { renderMoveReview, type MoveReviewPanelProps } from '../src/view/moveReview';
import { patch } from '../src/view/util';
import { observationFixtureJob, type CertifiedObservationFixture } from './moveReviewTestSupport';

/** Browser fixture entrypoint: bundle with the normal source aliases and load the product CSS.
 * Supply the runtime fixture beside the page, or use ?data=/path/to/fixture.json&case=moving-target.
 * This mounts the actual product renderer and board; no running application or engine is needed.
 */
export function mountMoveReviewFixturePreview(
  root: HTMLElement,
  fixtures: CertifiedObservationFixture[],
): void {
  const parameters = new URLSearchParams(window.location.search);
  let selected = fixtures.find(fixture => fixture.name === parameters.get('case')) ?? fixtures[0];
  if (!selected) throw new Error('No runtime observation fixtures');
  let locale: MoveReviewLocale = parameters.get('lang') === 'ko-KR' ? 'ko-KR' : 'en-US';
  let orientation: Color = 'white';
  let view: MoveReviewViewState = { evidenceExpanded: false };
  let job = observationFixtureJob(selected);
  let vnode: VNode | HTMLElement = root;
  const actions: MoveReviewPanelProps['actions'] = {
    selectCandidate: uci => {
      view = { selectedCandidateUci: uci, evidenceExpanded: view.evidenceExpanded };
      redraw();
    },
    toggleEvidence: () => {
      view.evidenceExpanded = !view.evidenceExpanded;
      redraw();
    },
    toggleProof: id => {
      view.expandedProofId = view.expandedProofId === id ? undefined : id;
      redraw();
    },
    previewFrame: frame => {
      view.hoveredFrame = frame;
      redraw();
    },
    clearPreview: () => {
      view.hoveredFrame = undefined;
      redraw();
    },
    pinFrame: frame => {
      view.pinnedFrame = frame;
      view.hoveredFrame = undefined;
      redraw();
    },
    clearPin: () => {
      view.pinnedFrame = undefined;
      redraw();
    },
    retry() {},
    addProof() {},
    viewAddedLine() {},
  };
  function redraw(): void {
    vnode = patch(
      vnode,
      h('main#move-review-fixture-preview', { attrs: { 'data-fixture': selected.name } }, [
        h('label', [
          'Runtime fixture ',
          h(
            'select',
            {
              attrs: { 'aria-label': 'Runtime fixture' },
              props: { value: selected.name },
              on: {
                change: event => {
                  selected = fixtures.find(
                    fixture => fixture.name === (event.target as HTMLSelectElement).value,
                  )!;
                  job = observationFixtureJob(selected);
                  view = { evidenceExpanded: false };
                  redraw();
                },
              },
            },
            fixtures.map(fixture =>
              h(
                'option',
                { attrs: { value: fixture.name, selected: fixture.name === selected.name } },
                fixture.name,
              ),
            ),
          ),
        ]),
        h('label', [
          'Language ',
          h(
            'select',
            {
              attrs: { 'aria-label': 'Language' },
              props: { value: locale },
              on: {
                change: event => {
                  locale = (event.target as HTMLSelectElement).value as MoveReviewLocale;
                  redraw();
                },
              },
            },
            [
              h('option', { attrs: { value: 'en-US', selected: locale === 'en-US' } }, 'English'),
              h('option', { attrs: { value: 'ko-KR', selected: locale === 'ko-KR' } }, '한국어'),
            ],
          ),
        ]),
        h(
          'button',
          {
            attrs: { type: 'button' },
            on: {
              click: () => {
                orientation = orientation === 'white' ? 'black' : 'white';
                redraw();
              },
            },
          },
          'Flip board',
        ),
        renderMoveReview({
          job,
          view,
          copy: moveReviewCopy(locale),
          locale,
          orientation,
          actions,
          canWrite: false,
          liveEnginePaused: false,
        }),
      ]),
    );
  }
  redraw();
}

if (typeof document !== 'undefined') {
  const root = document.getElementById('move-review-fixture-preview');
  if (root) {
    const data =
      new URLSearchParams(window.location.search).get('data') ?? './certified-observations-produced.json';
    fetch(data)
      .then(response => {
        if (!response.ok) throw new Error(`Fixture request failed: ${response.status}`);
        return response.json();
      })
      .then(fixtures => mountMoveReviewFixturePreview(root, fixtures))
      .catch(error => {
        root.textContent = String(error);
        root.setAttribute('data-error', 'true');
      });
  }
}
