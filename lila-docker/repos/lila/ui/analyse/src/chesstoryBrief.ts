type ChesstoryBriefSectionKey =
  | 'opening-idea'
  | 'middlegame-plan'
  | 'current-decision'
  | 'better-plan'
  | 'evidence';

export interface ChesstoryBriefSection {
  key: ChesstoryBriefSectionKey;
  title: string;
  body: string;
  pending: boolean;
  items?: string[];
  tone?: 'good' | 'bad' | 'neutral';
}

export interface ChesstoryMoveMeaningPayload {
  verdict?: {
    verdict_code?: string;
    move_quality?: string;
    played_move?: string;
    reference_move?: string;
    outcome?: {
      state?: string;
      certainty?: string;
      reference_state?: string;
      resistance?: string;
    };
    mate?: {
      reference_for_mover?: number;
      played_for_mover?: number;
      distance_loss?: number;
    };
  };
  explanations?: ChesstoryExplanation[];
  tested_plan_limits?: Array<{
    role?: string;
    move?: ChesstoryMoveReference;
    plan?: ChesstoryPlan;
  }>;
  exact_endgame_techniques?: ChesstoryExactTechnique[];
}

interface ChesstoryExplanation {
  move?: string;
  reference_move?: string;
  move_quality?: string;
  role?: string;
  ideas?: ChesstoryIdea[];
  chess_relations?: string[];
  line?: string[];
}

interface ChesstoryIdea {
  kind?: string;
  name?: string;
  subject?: string;
  scope?: string;
  confidence?: string;
  target?: { squares?: string[]; files?: string[] };
  evidence?: {
    layers?: string[];
    scopes?: string[];
    causes?: string[];
  };
  plans?: ChesstoryPlan[];
  endgame_techniques?: ChesstoryTechnique[];
}

interface ChesstoryMoveReference {
  uci?: string;
  san?: string;
  notation?: string;
  turn?: { move_number?: number; side?: string; notation?: string };
}

interface ChesstoryRoute {
  piece?: string;
  from?: string;
  to?: string;
}

interface ChesstoryPlanResult {
  stage?: string;
  kind?: string;
  direction?: string;
  subjects?: string[];
  move?: ChesstoryMoveReference;
  tested_reply_status?: string;
}

interface ChesstoryPlanContinuation {
  next_move?: ChesstoryMoveReference;
  target?: string;
  connection?: string;
  status?: string;
  successful_replies?: number;
  tested_replies?: number;
  expected_replies?: number;
}

interface ChesstoryPlan {
  goal?: { theme?: string; kind?: string };
  move?: ChesstoryMoveReference;
  move_role?: string;
  transition?: string;
  actor?: ChesstoryRoute;
  targets?: string[];
  results?: ChesstoryPlanResult[];
  tested_continuation?: ChesstoryPlanContinuation;
}

interface ChesstoryTechnique {
  pattern?: string;
  rook_geometry?: string;
  side?: string;
  status?: string;
  trigger_move?: ChesstoryMoveReference;
  forced_results?: string[];
  failure?: string;
}

interface ChesstoryExactTechnique {
  evidence_id?: string;
  pattern?: string;
  status?: string;
  exact_proof?: {
    kind?: string;
    side_to_move?: string;
    all_legal_moves_lose?: boolean;
    legal_replies?: ChesstoryMoveReference[];
  };
}

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  const explanation = (payload?.explanations || []).find(item => item.role === 'played move');
  const plan =
    explanation?.ideas?.flatMap(idea => idea.plans || [])[0] ||
    payload?.tested_plan_limits?.find(limit => limit.role === 'played move')?.plan;
  if (explanation && plan) return planSections(payload, explanation, plan);
  if (explanation) return ideaSections(payload, explanation);
  if (payload?.exact_endgame_techniques?.length)
    return exactTechniqueSections(payload.exact_endgame_techniques);
  if (payload?.verdict?.verdict_code || payload?.verdict?.outcome || payload?.verdict?.mate)
    return verdictSections(payload);
  return placeholderSections();
}

function ideaSections(
  payload: ChesstoryMoveMeaningPayload | undefined,
  explanation: ChesstoryExplanation,
): ChesstoryBriefSection[] {
  const move = moveLabel(explanation.move || payload?.verdict?.played_move || '');
  const reference = moveLabel(explanation.reference_move || payload?.verdict?.reference_move || '');
  const quality = verdictLabel(payload, explanation);
  const ideas = uniqueLabels((explanation.ideas || []).map(idea => idea.name || labelCode(idea.kind))).slice(
    0,
    5,
  );
  const outcomes = explanationOutcomes(explanation).slice(0, 6);
  const focus = outcomes.length ? outcomes : ideaTargetLabels(explanation).slice(0, 6);
  const line = uniqueLabels((explanation.line || []).map(moveLabel)).slice(0, 8);
  const relations = uniqueLabels((explanation.chess_relations || []).map(labelCode)).slice(0, 4);
  const evidence = ideaEvidenceLabels(explanation).slice(0, 8);
  const verdictFacts = verdictOutcomeLabels(payload?.verdict);
  const tone = verdictTone(payload?.verdict, quality);

  return [
    {
      key: 'opening-idea',
      title: 'Main idea',
      body: ideas.length
        ? `${move || 'The move'}: ${joinHuman(ideas)}.`
        : `${move || 'The move'} has a verified board effect.`,
      pending: false,
      items: [`Move verdict: ${quality || 'available'}`, ...verdictFacts],
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'How the move works',
      body: relations.length ? joinHuman(relations) : 'The move itself is the concrete mechanism.',
      pending: false,
      items: relations,
      tone,
    },
    {
      key: 'current-decision',
      title: outcomes.length ? 'What is established' : 'Where the idea applies',
      body: focus.length ? joinHuman(focus) : 'No separate result or target is established yet.',
      pending: false,
      items: focus,
      tone,
    },
    {
      key: 'better-plan',
      title: reference && reference !== move ? 'Better choice and main line' : 'Main line',
      body: line.length ? line.join(' ') : 'No further line is needed for this explanation.',
      pending: false,
      items: reference && reference !== move ? [`Better choice: ${reference}`, ...line] : line,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'Why this reading is justified',
      body: evidence.length ? joinHuman(evidence) : 'No additional evidence detail was published.',
      pending: false,
      items: evidence,
    },
  ];
}

function planSections(
  payload: ChesstoryMoveMeaningPayload | undefined,
  explanation: ChesstoryExplanation,
  plan: ChesstoryPlan,
): ChesstoryBriefSection[] {
  const start = moveNotation(plan.move) || moveLabel(explanation.move || payload?.verdict?.played_move || '');
  const quality = verdictLabel(payload, explanation);
  const goalKind = plan.goal?.kind || 'the current plan';
  const goalTheme = plan.goal?.theme;
  const goal = goalTheme && goalTheme !== goalKind ? `${goalKind} (${goalTheme})` : goalKind;
  const ideas = uniqueLabels((explanation.ideas || []).map(idea => idea.name || labelCode(idea.kind))).slice(
    0,
    4,
  );
  const actor = routeLabel(plan.actor);
  const method = uniqueLabels([
    actor,
    plan.transition ? `transition ${labelCode(plan.transition)}` : '',
    ...(plan.targets || []).map(target => `target ${target}`),
  ]).slice(0, 7);
  const results = uniqueLabels((plan.results || []).map(planResultLabel)).slice(0, 6);
  const line = uniqueLabels((explanation.line || []).map(moveLabel)).slice(0, 8);
  const continuation = plan.tested_continuation;
  const nextMove = moveNotation(continuation?.next_move);
  const continuationFacts = uniqueLabels([
    continuation?.status ? labelCode(continuation.status) : '',
    nextMove ? `next move ${nextMove}${continuation?.target ? ` toward ${continuation.target}` : ''}` : '',
    replyCountLabel(continuation),
    continuation?.connection || '',
    ...ideaEvidenceLabels(explanation),
  ]).slice(0, 7);
  const verdictFacts = verdictOutcomeLabels(payload?.verdict);
  const tone = verdictTone(payload?.verdict, quality);

  return [
    {
      key: 'opening-idea',
      title: 'Main plan',
      body: plan.move_role
        ? `${start || 'The move'} serves as ${plan.move_role} for ${goal}.`
        : `${start || 'The move'} advances ${goal}.`,
      pending: false,
      items: [`Move verdict: ${quality || 'available'}`, ...verdictFacts, ...ideas],
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'How the plan works',
      body: method.length ? joinHuman(method) : 'The starting move directly advances the plan.',
      pending: false,
      items: method,
      tone,
    },
    {
      key: 'current-decision',
      title: 'What the plan achieves',
      body: results.length ? results.join(' ') : 'No further result is established yet.',
      pending: false,
      items: results,
      tone,
    },
    {
      key: 'better-plan',
      title: 'Main line',
      body: line.length ? line.join(' ') : 'No further line is needed for this explanation.',
      pending: false,
      items: line,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'When the continuation occurs',
      body: continuationFacts.length
        ? joinHuman(continuationFacts)
        : 'Only the immediate result is established.',
      pending: false,
      items: continuationFacts,
    },
  ];
}

function explanationOutcomes(explanation: ChesstoryExplanation): string[] {
  return uniqueLabels([
    ...(explanation.ideas || []).flatMap(idea => (idea.endgame_techniques || []).flatMap(techniqueLabels)),
  ]);
}

function verdictSections(payload: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  const verdict = payload.verdict;
  const move = moveLabel(verdict?.played_move || '');
  const reference = moveLabel(verdict?.reference_move || '');
  const label = verdictLabel(payload);
  const facts = verdictOutcomeLabels(verdict);
  const tone = verdictTone(verdict, label);
  return [
    {
      key: 'opening-idea',
      title: 'Move verdict',
      body: `${move || 'The move'} is classified as ${label || 'engine assessed'}.`,
      pending: false,
      items: facts,
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'Engine outcome',
      body: facts.length ? joinHuman(facts) : 'The verdict is bound to the compared engine lines.',
      pending: false,
      items: facts,
      tone,
    },
    {
      key: 'current-decision',
      title: 'Current decision',
      body:
        reference && reference !== move
          ? `${reference} is the verified reference move.`
          : 'The played move matches the reference.',
      pending: false,
      tone,
    },
    {
      key: 'better-plan',
      title: 'Reference move',
      body: reference || 'No different reference move is required.',
      pending: false,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'Why this verdict is shown',
      body: 'The verdict comes from the registered played/reference comparison.',
      pending: false,
    },
  ];
}

function exactTechniqueSections(techniques: ChesstoryExactTechnique[]): ChesstoryBriefSection[] {
  const technique = techniques[0];
  const proof = technique.exact_proof;
  const replies = uniqueLabels((proof?.legal_replies || []).map(moveNotation));
  const pattern = labelCode(technique.pattern || proof?.kind) || 'exact endgame technique';
  const side = labelCode(proof?.side_to_move);
  return [
    {
      key: 'opening-idea',
      title: 'Exact endgame finding',
      body: `${pattern}: ${labelCode(technique.status) || 'tablebase verified'}.`,
      pending: false,
      items: side ? [`Side to move: ${side}`] : undefined,
      tone: 'neutral',
    },
    {
      key: 'middlegame-plan',
      title: 'Why it is exact',
      body: proof?.all_legal_moves_lose
        ? 'Every legal reply loses according to the bound tablebase proof.'
        : 'The result is bound to an exact tablebase proof.',
      pending: false,
    },
    {
      key: 'current-decision',
      title: 'Legal replies',
      body: replies.length ? joinHuman(replies) : 'No reply list is required.',
      pending: false,
      items: replies,
    },
    {
      key: 'better-plan',
      title: 'Technique',
      body: pattern,
      pending: false,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'Proof reference',
      body: technique.evidence_id
        ? `Registered evidence: ${technique.evidence_id}.`
        : 'Registered tablebase evidence.',
      pending: false,
    },
  ];
}

function verdictLabel(payload?: ChesstoryMoveMeaningPayload, explanation?: ChesstoryExplanation): string {
  return labelCode(
    payload?.verdict?.verdict_code || explanation?.move_quality || payload?.verdict?.move_quality,
  );
}

function verdictOutcomeLabels(verdict?: ChesstoryMoveMeaningPayload['verdict']): string[] {
  const outcome = verdict?.outcome;
  const mate = verdict?.mate;
  return uniqueLabels([
    outcome?.state ? `Outcome: ${labelCode(outcome.state)}` : '',
    outcome?.reference_state ? `Reference outcome: ${labelCode(outcome.reference_state)}` : '',
    outcome?.resistance ? `Resistance: ${labelCode(outcome.resistance)}` : '',
    mate?.played_for_mover !== undefined ? `Played mate score: ${mate.played_for_mover}` : '',
    mate?.reference_for_mover !== undefined ? `Reference mate score: ${mate.reference_for_mover}` : '',
    mate?.distance_loss !== undefined ? `Mate-distance loss: ${mate.distance_loss}` : '',
  ]);
}

function verdictTone(
  verdict: ChesstoryMoveMeaningPayload['verdict'] | undefined,
  label: string,
): 'good' | 'bad' | 'neutral' {
  const code = labelCode(verdict?.verdict_code || label);
  if (['inaccuracy', 'mistake', 'blunder', 'forced loss'].includes(code)) return 'bad';
  if (['improves on reference', 'matches reference', 'good', 'forced win'].includes(code)) return 'good';
  return 'neutral';
}

function ideaTargetLabels(explanation: ChesstoryExplanation): string[] {
  return uniqueLabels(
    (explanation.ideas || []).flatMap(idea => [
      ...(idea.target?.squares || []).map(square => `the ${square} square`),
      ...(idea.target?.files || []).map(file => `${file}-file`),
    ]),
  );
}

function ideaEvidenceLabels(explanation: ChesstoryExplanation): string[] {
  return uniqueLabels(
    (explanation.ideas || []).flatMap(idea => {
      const evidence = idea.evidence;
      return [
        idea.subject ? `Subject: ${labelCode(idea.subject)}` : '',
        idea.scope ? `Scope: ${labelCode(idea.scope)}` : '',
        idea.confidence ? `Confidence: ${labelCode(idea.confidence)}` : '',
        ...(evidence?.layers || []).map(layer => `Evidence layer: ${labelCode(layer)}`),
        ...(evidence?.scopes || []).map(scope => `Evidence scope: ${labelCode(scope)}`),
        ...(evidence?.causes || []).map(cause => `Cause: ${labelCode(cause)}`),
      ];
    }),
  );
}

function planResultLabel(result: ChesstoryPlanResult): string {
  const subjects = uniqueLabels(result.subjects || []);
  const change = [labelCode(result.kind), subjects.length ? `for ${joinHuman(subjects)}` : '']
    .filter(Boolean)
    .join(' ');
  const direction = labelCode(result.direction);
  const statement = [labelCode(result.stage), [direction, change].filter(Boolean).join(' ')].filter(Boolean).join(': ');
  const status = labelCode(result.tested_reply_status);
  return [statement, status].filter(Boolean).join(' — ');
}

function replyCountLabel(continuation?: ChesstoryPlanContinuation): string {
  if (continuation?.tested_replies === undefined || continuation.successful_replies === undefined) return '';
  return `${continuation.successful_replies} of ${continuation.tested_replies} tested replies reach the continuation`;
}

function routeLabel(route?: ChesstoryRoute): string {
  const squares = route?.from && route.to ? moveLabel(`${route.from}${route.to}`) : '';
  return [labelCode(route?.piece), squares].filter(Boolean).join(' ');
}

function moveNotation(move?: ChesstoryMoveReference): string {
  return move?.notation || move?.san || moveLabel(move?.uci || '');
}

function techniqueLabels(technique: ChesstoryTechnique): string[] {
  return uniqueLabels([
    technique.pattern || '',
    technique.rook_geometry || '',
    technique.status || '',
    moveNotation(technique.trigger_move) ? `trigger ${moveNotation(technique.trigger_move)}` : '',
    ...(technique.forced_results || []),
    technique.failure || '',
  ]);
}

function placeholderSections(): ChesstoryBriefSection[] {
  return [
    {
      key: 'opening-idea',
      title: 'Opening idea',
      body: 'Chesstory reads the opening structure, tension, or tabiya behind this game.',
      pending: true,
    },
    {
      key: 'middlegame-plan',
      title: 'Plan created by the opening',
      body: 'Your review points to the pawn break, piece route, or target that guides the middlegame.',
      pending: true,
    },
    {
      key: 'current-decision',
      title: 'Current decision',
      body: 'Each move you review shows what the move means beyond the live Stockfish number.',
      pending: true,
    },
    {
      key: 'better-plan',
      title: 'Better plan / alternative',
      body: 'Coach explanation can turn this position read into deeper chess language.',
      pending: true,
    },
    {
      key: 'evidence',
      title: 'Board clues',
      body: 'Board clues, eval shifts, and candidate lines stay visible as the reference.',
      pending: true,
    },
  ];
}

function labelCode(raw?: string): string {
  return (raw || '')
    .replace(/([a-z0-9])([A-Z])/g, '$1 $2')
    .replace(/[_-]+/g, ' ')
    .trim()
    .toLowerCase();
}

function moveLabel(uci: string): string {
  return uci.replace(/^([a-h][1-8])([a-h][1-8])([nbrq])?$/, '$1-$2$3');
}

function uniqueLabels(labels: string[]): string[] {
  return [...new Set(labels.map(label => label.trim()).filter(Boolean))];
}

function joinHuman(items: string[]): string {
  if (items.length <= 1) return items[0] || '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
}
