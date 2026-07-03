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
  move_semantics?: ChesstoryMoveSemantic[];
}

export interface ChesstoryMoveSemantic {
  subject?: string;
  line_role?: string;
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
  evidence?: {
    has_carrier?: boolean;
    proof_level?: string;
    target_bound?: boolean;
    cause_ids?: string[];
    source_ids?: string[];
    board_carriers?: {
      subject?: string;
      line_role?: string;
      move_uci?: string;
      role?: string;
      kind?: string;
      value?: string;
      from?: string;
      to?: string;
    }[];
  };
  priority?: string;
  failure_family?: string;
  problem?: string;
  comparison_loss?: ChesstoryComparisonLoss[];
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
    lost_ideas?: ChesstoryComparisonLoss[];
  };
}

interface ChesstoryCode {
  code?: string;
  label?: string;
}

interface ChesstoryComparisonLoss extends ChesstoryCode {
  side?: string;
}

type ChesstoryBoardCarrier = NonNullable<NonNullable<ChesstoryMoveSemantic['evidence']>['board_carriers']>[number];

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  if (!payload?.move_semantics?.some(hasEvidenceCarrier)) return placeholderSections();

  const semantics = payload.move_semantics;
  const played = semantics.filter(s => s.subject === 'played_move');
  const reference = semantics.filter(s => s.subject === 'reference_move');
  const evidencePlayed = played.filter(hasEvidenceCarrier);
  const evidenceReference = reference.filter(hasEvidenceCarrier);
  const bad = payload.verdict?.move_quality === 'bad';
  const playableLoss = normalizeCode(payload.verdict?.verdict_code) === 'playable_loss';
  const problemMove = bad || playableLoss;
  const mainPlayed = evidencePlayed.filter(s => s.priority === 'main');
  const localIdeas = played.filter(s => s.assessment?.is_local_idea && hasEvidenceCarrier(s));
  const verdictReasons = evidencePlayed.filter(s => s.assessment?.is_verdict_reason);
  const positionEvidence = problemMove ? evidenceReference : evidencePlayed;
  const solved = uniqueLabels(positionEvidence.map(ideaLabel)).slice(0, 4);
  const localIdeaLabels = uniqueLabels(localIdeas.map(ideaLabel)).slice(0, 4);
  const terminal = uniqueLabels(evidencePlayed.flatMap(s => (s.terminal_consequences || []).map(codeLabel)));
  const technique = uniqueLabels(evidencePlayed.flatMap(techniqueLabels));
  const losses = uniqueLabels(evidencePlayed.flatMap(playedComparisonLossLabels));
  const targets = uniqueLabels(positionEvidence.flatMap(boardCarrierTargetLabels)).slice(0, 5);
  const problem = firstLabel(verdictReasons.flatMap(problemLabels));
  const referenceIdeas = uniqueLabels(evidenceReference.map(ideaLabel)).slice(0, 3);
  const concreteSolved = targets.length ? targets : solved;
  const handled = [...concreteSolved, ...terminal];
  const positionThread =
    solved.length && targets.length ? `${joinHuman(solved)} around ${joinHuman(targets)}` : joinHuman(concreteSolved);
  const currentChange = targets.length ? joinHuman(targets) : solved.length ? joinHuman(solved) : '';
  const lineEvidence = comparisonLines(evidencePlayed).slice(0, 3);
  const comparisonFocus = [...losses, ...referenceIdeas].slice(0, 4);

  return [
    {
      key: 'opening-idea',
      title: 'Position thread',
      body: positionThread
        ? `The position is asking about ${positionThread}.`
        : 'The position needs a concrete plan before the engine line becomes useful.',
      pending: false,
      items: targets.length ? [`Board focus: ${joinHuman(targets)}`] : undefined,
    },
    {
      key: 'middlegame-plan',
      title: problemMove ? (localIdeaLabels.length ? 'Local idea that failed' : 'What this move misses') : 'What this move handles',
      body: problemMove
        ? localIdeaLabels.length
          ? `The move has ${joinHuman(localIdeaLabels)}, but it does not fully meet the position's main demand.`
          : 'No public local idea is strong enough to explain this mistake.'
        : handled.length
          ? `This move handles ${joinHuman(handled)}.`
          : 'No public carrier is strong enough to explain this move yet.',
      pending: false,
      items: (problemMove ? localIdeaLabels : [...solved, ...terminal, ...technique]).slice(0, 5),
      tone: problemMove ? 'bad' : 'good',
    },
    {
      key: 'current-decision',
      title: problemMove ? (bad ? 'Why it fails' : 'What remains loose') : 'Current decision',
      body: problemMove
        ? problem
          ? `The main problem is ${problem}.`
          : 'The graph has a verdict, but not enough public carrier evidence to explain the move.'
        : currentChange
          ? `The move is not just a verdict; it changes ${currentChange}.`
          : 'The graph has a verdict, but not enough public carrier evidence to explain the move.',
      pending: false,
      items: (problemMove ? uniqueLabels(verdictReasons.flatMap(problemLabels)) : mainPlayed.map(summaryLine).filter(Boolean)).slice(0, 3),
      tone: problemMove ? 'bad' : 'good',
    },
    {
      key: 'better-plan',
      title: problemMove ? 'What the better move keeps' : 'Compared with the alternatives',
      body:
        comparisonFocus.length && lineEvidence.length
          ? `The comparison turns on ${joinHuman(comparisonFocus)}; ${lineEvidence[0]}.`
        : comparisonFocus.length
          ? `The comparison turns on ${joinHuman(comparisonFocus)}.`
          : lineEvidence.length
            ? `The line evidence shows ${lineEvidence[0]}.`
          : 'No clear candidate-move loss is available yet.',
      pending: false,
      items: lineEvidence,
      tone: bad ? 'bad' : 'neutral',
    },
    {
      key: 'evidence',
      title: 'Evidence from the board',
      body:
        evidenceLine(problemMove ? evidenceReference : played) ||
        'The graph has a move verdict, but not enough public evidence to explain it cleanly.',
      pending: false,
      items: evidenceItems(problemMove ? evidenceReference : played).slice(0, 5),
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
  return code?.label || '';
}

function ideaLabel(semantic: ChesstoryMoveSemantic): string {
  return codeLabel(semantic.idea);
}

function problemLabels(semantic: ChesstoryMoveSemantic): string[] {
  return [
    semantic.assessment?.problem,
    semantic.assessment?.failure_family,
  ]
    .map(codeLabel)
    .filter(Boolean);
}

function boardCarrierLabels(semantic: ChesstoryMoveSemantic): string[] {
  return (semantic.evidence?.board_carriers || [])
    .filter(carrier => ['Square', 'File', 'Piece', 'Move', 'Pawn', 'PlanSubject'].includes(carrier.kind || ''))
    .map(boardCarrierLabel);
}

function boardCarrierTargetLabels(semantic: ChesstoryMoveSemantic): string[] {
  return (semantic.evidence?.board_carriers || [])
    .filter(
      carrier =>
        carrier.role === 'target' &&
        ['Square', 'File', 'Piece', 'Move', 'Pawn', 'PlanSubject'].includes(carrier.kind || ''),
    )
    .map(boardCarrierLabel);
}

function boardCarrierLabel(carrier: ChesstoryBoardCarrier): string {
  const value = carrierValueLabel(carrier.kind, carrier.value);
  const route = carrier.from && carrier.to ? `${carrier.from}-${carrier.to}` : '';
  return [value, route].filter(Boolean).join(' ');
}

function carrierValueLabel(kind?: string, value?: string): string {
  const raw = value || '';
  if (kind === 'File' && raw) return `${raw}-file`;
  if (kind === 'Pawn' && raw.startsWith('weak-pawn:')) return `weak pawn ${raw.slice('weak-pawn:'.length)}`;
  if (kind === 'PlanSubject') return planSubjectLabel(raw);
  return raw;
}

function planSubjectLabel(value: string): string {
  const normalized = value.trim().toLowerCase();
  if (normalized.includes(',')) return joinHuman(normalized.split(',').map(planSubjectLabel));
  const squareSubject = normalized.match(/^(line-unlock|material-sacrifice|weak-square|check):([a-h][1-8])$/);
  if (squareSubject) return `${squareSubject[1].replace(/-/g, ' ')} on ${squareSubject[2]}`;
  const passedAdvance = normalized.match(/^passed-pawn-advanced:([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
  if (passedAdvance) return `passed pawn advance ${passedAdvance[1]}-${passedAdvance[2]}`;
  const passedPawn = normalized.match(/^passed-pawn:([a-h][1-8])$/);
  if (passedPawn) return `passed pawn on ${passedPawn[1]}`;
  const breakFile = normalized.match(/^break-file:([a-h])$/);
  if (breakFile) return `${breakFile[1]}-file break`;
  const battery = normalized.match(/^battery-pressure:([a-h][1-8](?:-[a-h][1-8])?)$/);
  if (battery) return `battery pressure ${battery[1]}`;
  const actionSquare = normalized.match(/^(defender-move|material-capture):([a-h][1-8])$/);
  if (actionSquare) return `${actionSquare[1].replace(/-/g, ' ')} on ${actionSquare[2]}`;
  const createdTension = normalized.match(/^created-tension:([a-h][1-8])(-[a-h][1-8])?$/);
  if (createdTension) return `created tension ${createdTension[1]}${createdTension[2] || ''}`;
  const rookLift = normalized.match(/^rook-lift:([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
  if (rookLift) return `rook lift ${rookLift[1]}-${rookLift[2]}`;
  return normalized
    .replace(/pawnbreakpreparation/g, 'pawn break preparation')
    .replace(/pawnbreak/g, 'pawn break')
    .replace(/weakpawn/g, 'weak pawn')
    .replace(/pieceactivation/g, 'piece activation')
    .replace(/-/g, ' ');
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
  const targets = boardCarrierTargetLabels(semantic).slice(0, 3);
  return [idea, targets.length ? `on ${joinHuman(targets)}` : ''].filter(Boolean).join(' ');
}

function comparisonLines(semantics: ChesstoryMoveSemantic[]): string[] {
  return semantics.flatMap(s => {
    const comparison = s.comparison;
    if (!comparison) return [];
    const moves = (comparison.moves || []).filter(move => move.uci);
    const pvMoves = moves
      .filter(move => move.role?.includes('_pv_'))
      .map(move => `${move.role?.replace(/_/g, ' ') || 'pv'} ${move.uci}`);
    const rootMoves = moves
      .filter(move => !move.role?.includes('_pv_'))
      .map(move => `${move.role?.replace(/_/g, ' ') || 'move'} ${move.uci}`);
    const lost = (comparison.lost_ideas || []).filter(isPlayedComparisonLoss).map(codeLabel);
    return [
      pvMoves.length ? `PV continues ${joinHuman(pvMoves)}` : rootMoves.length ? rootMoves.join(', ') : '',
      lost.length ? `Lost idea: ${joinHuman(lost)}` : '',
    ].filter(Boolean);
  });
}

function playedComparisonLossLabels(semantic: ChesstoryMoveSemantic): string[] {
  return [...(semantic.comparison_loss || []), ...(semantic.comparison?.lost_ideas || [])]
    .filter(isPlayedComparisonLoss)
    .map(codeLabel);
}

function isPlayedComparisonLoss(loss: ChesstoryComparisonLoss): boolean {
  const side = loss.side?.toLowerCase();
  return side === 'played_move' || side === 'candidate';
}

function evidenceLine(semantics: ChesstoryMoveSemantic[]): string | undefined {
  const evidenceSemantics = semantics.filter(hasEvidenceCarrier);
  const terminal = uniqueLabels(evidenceSemantics.flatMap(s => (s.terminal_consequences || []).map(codeLabel)));
  if (terminal.length) return `The terminal result is ${joinHuman(terminal)}.`;
  const technique = uniqueLabels(evidenceSemantics.flatMap(techniqueLabels));
  if (technique.length) return `The ending technique evidence is ${joinHuman(technique)}.`;
  const carriers = uniqueLabels(evidenceSemantics.flatMap(boardCarrierLabels));
  if (carriers.length) return `The concrete board evidence is ${joinHuman(carriers.slice(0, 5))}.`;
  return undefined;
}

function evidenceItems(semantics: ChesstoryMoveSemantic[]): string[] {
  return uniqueLabels(
    semantics.filter(hasEvidenceCarrier).flatMap(s => [
      ...boardCarrierLabels(s),
      ...(s.terminal_consequences || []).map(codeLabel),
      ...techniqueLabels(s),
    ]),
  );
}

function hasEvidenceCarrier(semantic: ChesstoryMoveSemantic): boolean {
  return semantic.evidence?.has_carrier === true;
}

function normalizeCode(code?: string): string {
  return (code || '')
    .replace(/([a-z0-9])([A-Z])/g, '$1_$2')
    .toLowerCase();
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
