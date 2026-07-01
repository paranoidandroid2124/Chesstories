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
    move_quality?: string;
    played_move?: string;
    reference_move?: string;
  };
  move_semantics?: ChesstoryMoveSemantic[];
}

export interface ChesstoryMoveSemantic {
  subject?: string;
  move_quality?: string;
  idea_type?: string;
  idea?: ChesstoryCode;
  assessment?: {
    move_quality?: ChesstoryCode;
    idea_quality?: ChesstoryCode;
    priority?: ChesstoryCode;
    is_verdict_reason?: boolean;
    is_local_idea?: boolean;
    failure_family?: ChesstoryCode;
    problem?: ChesstoryCode;
  };
  target?: {
    squares?: string[];
    files?: string[];
    pieces?: string[];
  };
  priority?: string;
  failure_family?: string;
  problem?: string;
  comparison_loss?: ChesstoryCode[];
  terminal_consequences?: ChesstoryCode[];
  endgame_technique?: {
    pattern_info?: ChesstoryCode;
    rook_geometry?: ChesstoryCode;
    status_label?: string;
    trigger_move?: string;
    squares?: {
      required?: string[];
      maintained?: string[];
      broken?: string[];
    };
    terminal_consequences?: ChesstoryCode[];
    failure_reason?: ChesstoryCode;
  };
  comparison?: {
    reference_move?: string;
    candidate_move?: string;
    moves?: { role?: string; uci?: string }[];
    lost_ideas?: ChesstoryCode[];
  };
}

interface ChesstoryCode {
  code?: string;
  label?: string;
}

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  if (!payload?.move_semantics?.length) return placeholderSections();

  const semantics = payload.move_semantics;
  const played = semantics.filter(s => s.subject === 'played_move');
  const reference = semantics.filter(s => s.subject === 'reference_move');
  const mainPlayed = played.filter(s => s.priority === 'main');
  const solved = uniqueLabels(played.map(ideaLabel)).slice(0, 4);
  const terminal = uniqueLabels(played.flatMap(s => (s.terminal_consequences || []).map(codeLabel)));
  const technique = uniqueLabels(played.flatMap(techniqueLabels));
  const losses = uniqueLabels(
    semantics.flatMap(s => (s.comparison_loss || s.comparison?.lost_ideas || []).map(codeLabel)),
  );
  const targets = uniqueLabels(played.flatMap(targetLabels)).slice(0, 5);
  const bad = payload.verdict?.move_quality === 'bad' || played.some(s => s.move_quality === 'bad');
  const problem =
    firstLabel(mainPlayed.flatMap(problemLabels)) ||
    (bad ? 'the move gives up more than its idea solves' : undefined);
  const referenceIdeas = uniqueLabels(reference.map(ideaLabel)).slice(0, 3);

  return [
    {
      key: 'opening-idea',
      title: 'Position thread',
      body: solved.length
        ? `The position is asking about ${joinHuman(solved)}.`
        : 'The position needs a concrete plan before the engine line becomes useful.',
      pending: false,
      items: targets.length ? [`Board focus: ${joinHuman(targets)}`] : undefined,
    },
    {
      key: 'middlegame-plan',
      title: bad ? 'Useful idea inside the mistake' : 'What this move handles',
      body: bad
        ? `There may be a local idea, but it does not survive the position's main demand.`
        : `This move handles ${joinHuman(solved)}${terminal.length ? ` and reaches ${joinHuman(terminal)}` : ''}.`,
      pending: false,
      items: [...solved, ...terminal, ...technique].slice(0, 5),
      tone: bad ? 'bad' : 'good',
    },
    {
      key: 'current-decision',
      title: bad ? 'Why it fails' : 'Current decision',
      body: bad
        ? `The main problem is ${problem}.`
        : `The move is not just a verdict; it changes ${targets.length ? joinHuman(targets) : joinHuman(solved)}.`,
      pending: false,
      items: mainPlayed.map(summaryLine).filter(Boolean).slice(0, 3),
      tone: bad ? 'bad' : 'good',
    },
    {
      key: 'better-plan',
      title: bad ? 'What the better move keeps' : 'Compared with the alternatives',
      body:
        losses.length || referenceIdeas.length
          ? `The comparison turns on ${joinHuman([...losses, ...referenceIdeas].slice(0, 4))}.`
          : 'No clear candidate-move loss is available yet.',
      pending: false,
      items: comparisonLines(semantics).slice(0, 3),
      tone: bad ? 'bad' : 'neutral',
    },
    {
      key: 'evidence',
      title: 'Evidence from the board',
      body:
        evidenceLine(played) ||
        'The graph has a move verdict, but not enough public evidence to explain it cleanly.',
      pending: false,
      items: evidenceItems(played).slice(0, 5),
    },
  ];
}

export function chesstoryLlmPayload(payload?: ChesstoryMoveMeaningPayload) {
  return chesstoryBriefSections(payload)
    .filter(section => !section.pending)
    .map(({ key, title, body, items, tone }) => ({
      key,
      title,
      body,
      items: items || [],
      tone: tone || 'neutral',
    }));
}

function placeholderSections(): ChesstoryBriefSection[] {
  return [
    {
      key: 'opening-idea',
      title: 'Opening idea',
      body: 'Which structure, tension, or tabiya did this game come from?',
      pending: true,
    },
    {
      key: 'middlegame-plan',
      title: 'Plan created by the opening',
      body: 'Which pawn break, piece route, or target should guide the middlegame?',
      pending: true,
    },
    {
      key: 'current-decision',
      title: 'Current decision',
      body: 'What did the selected move change in the plan or structure?',
      pending: true,
    },
    {
      key: 'better-plan',
      title: 'Better plan / alternative',
      body: 'Which candidate kept the plan clearer, safer, or more forcing?',
      pending: true,
    },
    {
      key: 'evidence',
      title: 'Evidence from the board',
      body: 'Which engine line, eval shift, or board cue makes the idea trustworthy?',
      pending: true,
    },
  ];
}

function codeLabel(code?: ChesstoryCode): string {
  return code?.label || code?.code?.replace(/_/g, ' ') || '';
}

function ideaLabel(semantic: ChesstoryMoveSemantic): string {
  return codeLabel(semantic.idea) || semantic.idea_type?.replace(/_/g, ' ') || '';
}

function problemLabels(semantic: ChesstoryMoveSemantic): string[] {
  return [
    semantic.assessment?.problem,
    semantic.assessment?.failure_family,
    semantic.problem ? { label: semantic.problem } : undefined,
    semantic.failure_family ? { label: semantic.failure_family } : undefined,
  ]
    .map(codeLabel)
    .filter(Boolean);
}

function targetLabels(semantic: ChesstoryMoveSemantic): string[] {
  const target = semantic.target;
  if (!target) return [];
  return [
    ...(target.squares || []).map(s => `${s}`),
    ...(target.files || []).map(f => `${f}-file`),
    ...(target.pieces || []),
  ];
}

function techniqueLabels(semantic: ChesstoryMoveSemantic): string[] {
  const technique = semantic.endgame_technique;
  if (!technique) return [];
  return [
    codeLabel(technique.pattern_info),
    codeLabel(technique.rook_geometry),
    technique.status_label,
    ...(technique.terminal_consequences || []).map(codeLabel),
    codeLabel(technique.failure_reason),
  ].filter((label): label is string => !!label);
}

function summaryLine(semantic: ChesstoryMoveSemantic): string {
  const idea = ideaLabel(semantic);
  const targets = targetLabels(semantic).slice(0, 3);
  return [idea, targets.length ? `on ${joinHuman(targets)}` : ''].filter(Boolean).join(' ');
}

function comparisonLines(semantics: ChesstoryMoveSemantic[]): string[] {
  return semantics.flatMap(s => {
    const comparison = s.comparison;
    if (!comparison) return [];
    const moves = (comparison.moves || [])
      .filter(move => move.uci)
      .map(move => `${move.role?.replace(/_/g, ' ') || 'move'} ${move.uci}`);
    const lost = (comparison.lost_ideas || []).map(codeLabel);
    return [moves.length ? moves.join(', ') : '', lost.length ? `Lost idea: ${joinHuman(lost)}` : ''].filter(
      Boolean,
    );
  });
}

function evidenceLine(semantics: ChesstoryMoveSemantic[]): string | undefined {
  const terminal = uniqueLabels(semantics.flatMap(s => (s.terminal_consequences || []).map(codeLabel)));
  if (terminal.length) return `The terminal result is ${joinHuman(terminal)}.`;
  const technique = uniqueLabels(semantics.flatMap(techniqueLabels));
  if (technique.length) return `The ending technique evidence is ${joinHuman(technique)}.`;
  const targets = uniqueLabels(semantics.flatMap(targetLabels));
  if (targets.length) return `The concrete board evidence is ${joinHuman(targets.slice(0, 5))}.`;
  return undefined;
}

function evidenceItems(semantics: ChesstoryMoveSemantic[]): string[] {
  return uniqueLabels(
    semantics.flatMap(s => [
      ...targetLabels(s),
      ...(s.terminal_consequences || []).map(codeLabel),
      ...techniqueLabels(s),
    ]),
  );
}

function uniqueLabels(labels: string[]): string[] {
  return [...new Set(labels.map(l => l.trim()).filter(Boolean))];
}

function firstLabel(labels: string[]): string | undefined {
  return labels.find(Boolean);
}

function joinHuman(items: string[]): string {
  if (items.length <= 1) return items[0] || '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
}
