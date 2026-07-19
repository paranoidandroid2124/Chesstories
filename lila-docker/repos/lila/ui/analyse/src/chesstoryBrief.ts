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
  };
  explanations?: ChesstoryExplanation[];
  tested_plan_limits?: unknown[];
}

interface ChesstoryExplanation {
  move?: string;
  reference_move?: string;
  move_quality?: string;
  role?: string;
  ideas?: ChesstoryIdea[];
  verification?: string[];
  chess_relations?: string[];
  move_details?: string[];
  purposes?: string[];
  line?: string[];
  results?: string[];
  forced_results?: string[];
  techniques?: ChesstoryTechnique[];
  plan?: ChesstoryPlan;
}

interface ChesstoryIdea {
  kind?: string;
  name?: string;
  importance?: string;
  move_quality?: string;
  idea_quality?: string;
  problem?: string;
  failure?: string;
  lost_by_comparison?: string[];
  results?: string[];
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

interface ChesstoryPlanStep {
  move?: ChesstoryMoveReference;
  actor?: ChesstoryRoute;
  connections?: string[];
}

interface ChesstoryPlanResult {
  when?: string;
  change?: string;
  direction?: string;
  subjects?: string[];
  move?: ChesstoryMoveReference;
  tested_reply_status?: string;
}

interface ChesstoryPlanReply {
  reply?: ChesstoryMoveReference;
  effect_on_plan?: string;
  continuation?: ChesstoryMoveReference;
  continuation_match?: string;
  observed_through?: { notation?: string };
  game_result?: string;
  game_end?: ChesstoryMoveReference;
}

interface ChesstoryPlanContinuation {
  next_move?: ChesstoryMoveReference;
  target?: string;
  connection?: string;
  tested_reply_status?: string;
  successful_replies?: number;
  same_move_replies?: number;
  same_function_replies?: number;
  tested_replies?: number;
  expected_replies?: number;
}

interface ChesstoryPlan {
  goal?: { theme?: string; kind?: string };
  move_role?: string;
  plan_change?: string;
  targets?: string[];
  method?: {
    starting_move?: ChesstoryMoveReference;
    actor?: ChesstoryRoute;
    development?: ChesstoryRoute[];
    sequence?: ChesstoryPlanStep[];
  };
  opponent_replies?: ChesstoryPlanReply[];
  results?: ChesstoryPlanResult[];
  continuation?: ChesstoryPlanContinuation;
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

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  const explanation = (payload?.explanations || []).find(item => item.role === 'played move');
  if (!explanation) return placeholderSections();
  return explanation.plan
    ? planSections(payload, explanation, explanation.plan)
    : ideaSections(payload, explanation);
}

function ideaSections(
  payload: ChesstoryMoveMeaningPayload | undefined,
  explanation: ChesstoryExplanation,
): ChesstoryBriefSection[] {
  const move = moveLabel(explanation.move || payload?.verdict?.played_move || '');
  const reference = moveLabel(explanation.reference_move || payload?.verdict?.reference_move || '');
  const quality = labelCode(explanation.move_quality || payload?.verdict?.move_quality);
  const ideas = uniqueLabels((explanation.ideas || []).map(idea => idea.name || labelCode(idea.kind))).slice(
    0,
    5,
  );
  const method = uniqueLabels([...(explanation.move_details || []), ...(explanation.purposes || [])]).slice(
    0,
    6,
  );
  const changes = explanationChanges(explanation).slice(0, 6);
  const line = uniqueLabels((explanation.line || []).map(moveLabel)).slice(0, 8);
  const verification = uniqueLabels(explanation.verification || []).slice(0, 4);
  const relations = uniqueLabels((explanation.chess_relations || []).map(labelCode)).slice(0, 4);
  const tone = quality === 'bad' ? 'bad' : quality ? 'good' : 'neutral';

  return [
    {
      key: 'opening-idea',
      title: 'Main idea',
      body: ideas.length
        ? `${move || 'The move'}: ${joinHuman(ideas)}.`
        : `${move || 'The move'} has a verified board effect.`,
      pending: false,
      items: [`Move quality: ${quality || 'available'}`, ...relations],
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'How the move works',
      body: method.length ? joinHuman(method) : 'The move itself is the concrete mechanism.',
      pending: false,
      items: method,
      tone,
    },
    {
      key: 'current-decision',
      title: 'What changes',
      body: changes.length ? joinHuman(changes) : 'No further board change is established yet.',
      pending: false,
      items: changes,
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
      body: verification.length ? joinHuman(verification) : 'The board effect is directly established.',
      pending: false,
      items: verification,
    },
  ];
}

function planSections(
  payload: ChesstoryMoveMeaningPayload | undefined,
  explanation: ChesstoryExplanation,
  plan: ChesstoryPlan,
): ChesstoryBriefSection[] {
  const start =
    moveNotation(plan.method?.starting_move) ||
    moveLabel(explanation.move || payload?.verdict?.played_move || '');
  const quality = labelCode(explanation.move_quality || payload?.verdict?.move_quality);
  const goalKind = plan.goal?.kind || 'the current plan';
  const goalTheme = plan.goal?.theme;
  const goal = goalTheme && goalTheme !== goalKind ? `${goalKind} (${goalTheme})` : goalKind;
  const ideas = uniqueLabels((explanation.ideas || []).map(idea => idea.name || labelCode(idea.kind))).slice(
    0,
    4,
  );
  const actor = routeLabel(plan.method?.actor);
  const development = (plan.method?.development || []).map(routeLabel);
  const sequence = uniqueLabels((plan.method?.sequence || []).map(step => moveNotation(step.move)));
  const purposes = uniqueLabels(explanation.purposes || []);
  const method = uniqueLabels([
    actor,
    ...development,
    ...purposes,
    sequence.length ? `sequence ${sequence.join(' ')}` : '',
  ]).slice(0, 7);
  const results = uniqueLabels((plan.results || []).map(planResultLabel)).slice(0, 6);
  const replies = uniqueLabels((plan.opponent_replies || []).map(planReplyLabel)).slice(0, 6);
  const line = uniqueLabels((explanation.line || []).map(moveLabel)).slice(0, 8);
  const continuation = plan.continuation;
  const nextMove = moveNotation(continuation?.next_move);
  const continuationFacts = uniqueLabels([
    continuation?.tested_reply_status || '',
    nextMove ? `next move ${nextMove}${continuation?.target ? ` toward ${continuation.target}` : ''}` : '',
    replyCountLabel(continuation),
    continuation?.connection || '',
    ...(explanation.verification || []),
  ]).slice(0, 7);
  const tone = quality === 'bad' ? 'bad' : quality ? 'good' : 'neutral';

  return [
    {
      key: 'opening-idea',
      title: 'Main plan',
      body: plan.move_role
        ? `${start || 'The move'} serves as ${plan.move_role} for ${goal}.`
        : `${start || 'The move'} advances ${goal}.`,
      pending: false,
      items: [`Move quality: ${quality || 'available'}`, ...ideas],
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
      title: replies.length ? "Opponent's replies" : 'Main line',
      body: replies.length
        ? joinHuman(replies)
        : line.length
          ? line.join(' ')
          : 'No tested reply is public yet.',
      pending: false,
      items: line.length ? [`Main line: ${line.join(' ')}`] : undefined,
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

function explanationChanges(explanation: ChesstoryExplanation): string[] {
  return uniqueLabels([
    ...(explanation.results || []),
    ...(explanation.forced_results || []),
    ...(explanation.techniques || []).flatMap(techniqueLabels),
  ]);
}

function planResultLabel(result: ChesstoryPlanResult): string {
  const subjects = uniqueLabels(result.subjects || []);
  const change = [result.change, subjects.length ? `for ${joinHuman(subjects)}` : '']
    .filter(Boolean)
    .join(' ');
  const statement = [result.when, change].filter(Boolean).join(': ');
  return [statement, result.tested_reply_status].filter(Boolean).join(' — ');
}

function planReplyLabel(reply: ChesstoryPlanReply): string {
  const move = moveNotation(reply.reply);
  if (!move) return '';
  const continuation = moveNotation(reply.continuation);
  const follow = continuation
    ? `; ${continuation} follows${reply.continuation_match ? ` with the ${reply.continuation_match}` : ''}`
    : '';
  const result = reply.game_result ? `; game result: ${reply.game_result}` : '';
  return `${move}: ${reply.effect_on_plan || 'the plan is not yet resolved'}${follow}${result}`;
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
