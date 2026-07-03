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
  move_uci?: string;
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

const broadIdeaLabels = new Set(['piece route', 'piece activity', 'target pressure', 'plan continuity', 'counterplay control']);

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  if (!payload?.move_semantics?.some(hasConcreteSurfaceCarrier)) return placeholderSections();

  const semantics = payload.move_semantics;
  const played = semantics.filter(s => s.subject === 'played_move');
  const reference = semantics.filter(s => s.subject === 'reference_move');
  const evidencePlayed = played.filter(hasConcreteSurfaceCarrier);
  const evidenceReference = reference.filter(hasConcreteSurfaceCarrier);
  const bad = payload.verdict?.move_quality === 'bad';
  const playableLoss = normalizeCode(payload.verdict?.verdict_code) === 'playable_loss';
  const problemMove = bad || playableLoss;
  const mainPlayed = evidencePlayed.filter(s => s.priority === 'main');
  const localIdeas = played.filter(s => s.assessment?.is_local_idea && hasConcreteSurfaceCarrier(s));
  const verdictReasons = evidencePlayed.filter(s => s.assessment?.is_verdict_reason);
  const positionEvidence = problemMove ? evidenceReference : evidencePlayed;
  const solved = cleanTerminalLabels(uniqueLabels(positionEvidence.map(ideaLabel)), problemMove).slice(0, 4);
  const localIdeaLabels = uniqueLabels(localIdeas.map(ideaLabel)).slice(0, 4);
  const terminal = cleanTerminalLabels(uniqueLabels(evidencePlayed.flatMap(terminalLabels)));
  const technique = uniqueLabels(evidencePlayed.flatMap(techniqueLabels));
  const losses = cleanTerminalLabels(uniqueLabels(evidencePlayed.flatMap(playedComparisonLossLabels)), problemMove);
  const targets = conciseCarrierLabels(positionEvidence.flatMap(boardCarrierTargetLabels)).slice(0, 5);
  const currentCarriers = conciseCarrierLabels([...positionEvidence.flatMap(moveCarrierLabels), ...targets])
    .filter(label => !/^[a-h]-file$/.test(label) && !/^the [a-h][1-8] square$/.test(label))
    .slice(0, 3);
  const problem = firstLabel(verdictReasons.flatMap(problemLabels));
  const referenceIdeas = cleanTerminalLabels(uniqueLabels(evidenceReference.map(ideaLabel)), problemMove).slice(0, 3);
  const concreteIdeas = targets.length
    ? solved.filter(label => !broadIdeaLabels.has(label) && !terminal.includes(label) && !/^break file [a-h] created tension /.test(label))
    : solved;
  const concreteSolved = targets.length ? targets : solved;
  const handled = concreteSolved.length ? concreteSolved : terminal;
  const positionThread =
    concreteIdeas.length && targets.length && !targets.some(target => target.startsWith(concreteIdeas[0]))
      ? `${concreteIdeas.length === 2 ? `${concreteIdeas[0]} with ${concreteIdeas[1]}` : joinHuman(concreteIdeas)} around ${joinHuman(targets)}`
      : joinHuman(concreteSolved);
  const currentChange = currentCarriers.length ? joinHuman(currentCarriers) : solved.length ? joinHuman(solved) : '';
  const lineEvidence = comparisonLines(problemMove ? evidencePlayed : currentChange ? played : evidencePlayed)
    .filter(line => line.startsWith('PV continues'))
    .slice(0, 3);
  const terminalProof = terminal.length ? `, proving ${joinHuman(terminal)}` : '';
  const carrierProof =
    !terminal.length && lineEvidence.length
      ? concreteSolved
          .filter(label => !broadIdeaLabels.has(label) && !label.startsWith('the ') && !/^[a-h]-file$/.test(label))
          .slice(0, 3)
      : [];
  const pvProof = carrierProof.length ? `, confirming ${joinHuman(carrierProof)}` : terminalProof;
  const currentDecisionLine =
    currentChange && lineEvidence.length
      ? `The move changes ${currentChange}; ${lineEvidence[0]}${pvProof}.`
      : currentChange
        ? `The move changes ${currentChange}.`
        : 'This move is marked, but the lesson is not clear from the board yet.';
  const comparisonFocus = [...losses, ...referenceIdeas].filter(label => !broadIdeaLabels.has(label)).slice(0, 4);

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
      title: problemMove
        ? localIdeaLabels.length
          ? 'Local idea that failed'
          : 'What this move misses'
        : 'What this move handles',
      body: problemMove
        ? localIdeaLabels.length
          ? `The move has ${joinHuman(localIdeaLabels)}, but it does not fully meet the position's main demand.`
          : 'The move does not reveal a clear useful idea yet.'
        : handled.length
          ? `This move handles ${joinHuman(handled)}.`
          : 'The board does not show a clear enough reason for this move yet.',
      pending: false,
      items: (problemMove ? localIdeaLabels : [...concreteSolved, ...terminal, ...technique]).slice(0, 5),
      tone: problemMove ? 'bad' : 'good',
    },
    {
      key: 'current-decision',
      title: problemMove ? (bad ? 'Why it fails' : 'What remains loose') : 'Current decision',
      body: problemMove
        ? problem
          ? `The main problem is ${problem}.`
          : 'This move is marked, but the lesson is not clear from the board yet.'
        : currentDecisionLine,
      pending: false,
      items: (problemMove
        ? uniqueLabels(verdictReasons.flatMap(problemLabels))
        : currentCarriers.length
          ? currentCarriers
          : mainPlayed.map(summaryLine).filter(Boolean)
      ).slice(0, 3),
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
          : 'No clean better-move lesson is visible yet.',
      pending: false,
      items: lineEvidence,
      tone: problemMove ? 'bad' : 'neutral',
    },
    {
      key: 'evidence',
      title: 'Board clues',
      body:
        evidenceLine(problemMove ? evidenceReference : played) ||
        'This move is marked, but the lesson is not clear from the board yet.',
      pending: false,
      items: evidenceItems(problemMove ? evidenceReference : played).slice(0, 5),
    },
  ];
}

export function chesstoryLlmPayload(payload?: ChesstoryMoveMeaningPayload) {
  const semantics = payload?.move_semantics || [];
  const hasLineProof = semantics.some(semantic =>
    terminalLabels(semantic).length > 0 ||
    techniqueLabels(semantic).length > 0 ||
    (semantic.comparison?.moves || []).some(move => !!move.uci && !!move.role?.includes('_pv_')),
  );
  if (!semantics.some(hasConcreteSurfaceCarrier) || !hasLineProof) return [];
  const sections = chesstoryBriefSections(payload);
  const currentDecisionBody = sections.find(section => section.key === 'current-decision')?.body || '';
  const currentDecisionHasProof = /(?:confirming|proving) /.test(currentDecisionBody);
  const evidenceBody = sections.find(section => section.key === 'evidence')?.body || '';
  const evidenceHasLineProof = /^The terminal result is |^The line (?:wins|loses|confirms) |^The ending technique evidence is /.test(evidenceBody);
  const evidenceProofAlreadyInCurrent =
    (evidenceBody === 'The line wins material.' && currentDecisionBody.includes('material gain')) ||
    (evidenceBody === 'The line loses material.' && currentDecisionBody.includes('material loss'));
  if (!currentDecisionHasProof && !evidenceHasLineProof) return [];
  return sections
    .filter(section => !section.pending)
    .filter(section => section.items?.length || !/not clear from the board|No clean better-move lesson|needs a concrete plan|does not show a clear enough|does not reveal/.test(section.body))
    .filter(section => {
      const handled = section.key === 'middlegame-plan' ? section.body.match(/^This move handles (.*)\.$/)?.[1] : '';
      const handledItems = handled?.replace(/, and | and /g, ', ').split(', ').filter(Boolean);
      const coveredItems = handledItems?.filter(item => currentDecisionBody.includes(item)).length || 0;
      return !handledItems?.length || coveredItems < Math.max(handledItems.length - 1, 1);
    })
    .filter(section => section.key !== 'middlegame-plan' || !currentDecisionHasProof || !section.body.startsWith('This move handles '))
    .filter(section => section.key !== 'better-plan' || !section.items?.some(item => currentDecisionBody.includes(item)))
    .filter(section =>
      section.key !== 'evidence' ||
      (!evidenceProofAlreadyInCurrent &&
        (section.body.startsWith('The terminal result is ') ||
          section.body === 'The line wins material.' ||
          section.body === 'The line loses material.' ||
          section.body.startsWith('The line confirms ') ||
          section.body.startsWith('The ending technique evidence is ')))
    )
    .filter(section => section.key !== 'opening-idea')
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

function codeLabel(code?: ChesstoryCode): string {
  return code?.label || '';
}

function terminalLabels(semantic: ChesstoryMoveSemantic): string[] {
  const bad = normalizeCode(semantic.move_quality) === 'bad';
  const labels = cleanTerminalLabels(uniqueLabels((semantic.terminal_consequences || []).map(codeLabel)), bad);
  return bad ? labels : labels.map(label => label === 'material loss' ? 'material sacrifice' : label);
}

function cleanTerminalLabels(labels: string[], preferLoss = false): string[] {
  if (labels.includes('material gain') && labels.includes('material loss')) {
    return labels.filter(label => label !== (preferLoss ? 'material gain' : 'material loss'));
  }
  if (labels.includes('material gain') && labels.includes('material sacrifice')) {
    return labels.filter(label => label !== 'material sacrifice');
  }
  return labels;
}

function ideaLabel(semantic: ChesstoryMoveSemantic): string {
  const terminal = terminalLabels(semantic);
  if (terminal.length) return joinHuman(terminal);
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
  const carriers = semantic.evidence?.board_carriers || [];
  const actorPiece = actorRoutePiece(carriers);
  return [...carriers]
    .filter(carrier => ['Square', 'File', 'Piece', 'Move', 'Pawn', 'PlanSubject'].includes(carrier.kind || ''))
    .sort((a, b) => boardCarrierRank(a) - boardCarrierRank(b))
    .map(carrier => boardCarrierLabel(carrier, actorPiece));
}

function boardCarrierRank(carrier: ChesstoryBoardCarrier): number {
  if (carrier.role !== 'target') return 4;
  if (carrier.kind === 'PlanSubject' && carrier.value?.startsWith('defender-move:')) return -1;
  if (carrier.kind === 'PlanSubject') return 0;
  if (carrier.kind === 'File' || carrier.kind === 'Square' || carrier.kind === 'Pawn') return 1;
  if (carrier.kind === 'Piece') return 2;
  return 3;
}

function boardCarrierTargetLabels(semantic: ChesstoryMoveSemantic): string[] {
  const carriers = (semantic.evidence?.board_carriers || []).filter(
    carrier =>
      carrier.role === 'target' &&
      ['Square', 'File', 'Piece', 'Move', 'Pawn', 'PlanSubject'].includes(carrier.kind || ''),
  );
  const hasSpecificTarget = carriers.some(carrier => carrier.kind !== 'Piece');
  return conciseCarrierLabels(
    carriers
      .filter(carrier => !hasSpecificTarget || carrier.kind !== 'Piece')
      .sort((a, b) => boardCarrierRank(a) - boardCarrierRank(b))
      .map(boardCarrierLabel),
  );
}

function moveCarrierLabels(semantic: ChesstoryMoveSemantic): string[] {
  const carriers = semantic.evidence?.board_carriers || [];
  const actorPiece = actorRoutePiece(carriers);
  return conciseCarrierLabels(
    carriers
      .filter(carrier => carrier.role === 'actor' && carrier.kind === 'Move' && carrier.from && carrier.to)
      .filter(carrier => !semantic.move_uci || carrier.value === semantic.move_uci)
      .map(carrier => boardCarrierLabel(carrier, actorPiece)),
  );
}

function actorRoutePiece(carriers: ChesstoryBoardCarrier[]): string | undefined {
  const moveFrom = carriers.find(carrier => carrier.role === 'actor' && carrier.kind === 'Move' && carrier.from)?.from;
  const actorSquare = carriers.find(carrier => carrier.role === 'actor' && carrier.kind === 'Square')?.value;
  if (moveFrom && actorSquare && moveFrom !== actorSquare) return undefined;
  const piece = carriers.find(carrier => carrier.role === 'actor' && carrier.kind === 'Piece')?.value?.toLowerCase();
  return piece && piece !== 'pawn' && piece !== 'piece' ? piece : undefined;
}

function boardCarrierLabel(carrier: ChesstoryBoardCarrier, actorPiece?: string): string {
  const value = carrierValueLabel(carrier.kind, carrier.value);
  const route = carrier.from && carrier.to ? `${carrier.from}-${carrier.to}` : '';
  if (carrier.kind === 'Move' && route) {
    const piece = actorPiece ? `${actorPiece} ` : '';
    return `${piece}${route}`;
  }
  return [value, route].filter(Boolean).join(' ');
}

function carrierValueLabel(kind?: string, value?: string): string {
  const raw = value || '';
  if (kind === 'File' && raw) return `${raw}-file`;
  if (kind === 'Pawn' && raw.startsWith('weak-pawn:')) return `weak pawn on ${raw.slice('weak-pawn:'.length)}`;
  if (kind === 'PlanSubject') return planSubjectLabel(raw);
  return raw;
}

function planSubjectLabel(value: string): string {
  const normalized = value.trim().toLowerCase();
  if (normalized.includes(',')) {
    const parts = normalized.split(',').map(planSubjectLabel);
    if (parts.includes('piece activation') && parts.includes('pawn break preparation')) {
      return 'piece development for the pawn break';
    }
    if (parts.includes('opening development') && parts.includes('pawn break preparation')) {
      return 'development for the pawn break';
    }
    if (parts.includes('simplification') && parts.includes('pawn break preparation')) {
      return 'simplification for the pawn break';
    }
    return parts.length === 2 ? `${parts[0]} with ${parts[1]}` : joinHuman(parts);
  }
  const lineUnlock = normalized.match(/^line-unlock:([a-h][1-8])$/);
  if (lineUnlock) return `line opening from ${lineUnlock[1]}`;
  const squareSubject = normalized.match(/^(material-sacrifice|weak-square|check):([a-h][1-8])$/);
  if (squareSubject) return `${squareSubject[1].replace(/-/g, ' ')} on ${squareSubject[2]}`;
  const passedAdvance = normalized.match(/^passed-pawn-advanced:([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
  if (passedAdvance) return `passed pawn advance ${passedAdvance[1]}-${passedAdvance[2]}`;
  const passedPawn = normalized.match(/^passed-pawn:([a-h][1-8])$/);
  if (passedPawn) return `passed pawn on ${passedPawn[1]}`;
  const breakFile = normalized.match(/^break-file:([a-h])$/);
  if (breakFile) return `${breakFile[1]}-file break`;
  const pressure = normalized.match(/^(battery-pressure|pin-pressure):([a-h][1-8](?:-[a-h][1-8])?)$/);
  if (pressure) return `${pressure[1].replace(/-/g, ' ')} ${pressure[2].includes('-') ? 'along' : 'on'} ${pressure[2]}`;
  const defenderMove = normalized.match(/^defender-move:([a-h][1-8])$/);
  if (defenderMove) return `defensive resource on ${defenderMove[1]}`;
  const actionSquare = normalized.match(/^(defender-move|material-capture|material-recapture):([a-h][1-8])$/);
  if (actionSquare) return `${actionSquare[1].replace(/-/g, ' ')} on ${actionSquare[2]}`;
  const createdTension = normalized.match(/^created-tension:([a-h][1-8])(-[a-h][1-8])?$/);
  if (createdTension?.[2]) return `tension between ${createdTension[1]} and ${createdTension[2].slice(1)}`;
  if (createdTension) return `tension on ${createdTension[1]}`;
  const rookLift = normalized.match(/^rook-lift:([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
  if (rookLift) return `rook lift ${rookLift[1]}-${rookLift[2]}`;
  return normalized
    .replace(/pawnbreakpreparation/g, 'pawn break preparation')
    .replace(/pawnbreak/g, 'pawn break')
    .replace(/weakpawnattack/g, 'weak pawn attack')
    .replace(/weakpawn/g, 'weak pawn')
    .replace(/pawnstorm/g, 'pawn storm')
    .replace(/openingdevelopment/g, 'opening development')
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
  if (targets.length && broadIdeaLabels.has(idea)) return joinHuman(targets);
  return [idea, targets.length ? `on ${joinHuman(targets)}` : ''].filter(Boolean).join(' ');
}

function comparisonLines(semantics: ChesstoryMoveSemantic[]): string[] {
  return uniqueLabels(semantics.flatMap(s => {
    const comparison = s.comparison;
    if (!comparison) return [];
    const moves = (comparison.moves || []).filter(move => move.uci);
    const pvMoves = compactRepeatedMoves(moves
      .filter(move => move.role?.includes('_pv_'))
      .map(move => moveLabel(move.uci || '')));
    const rootMoves = moves
      .filter(move => !move.role?.includes('_pv_'))
      .map(move => `${move.role?.replace(/_/g, ' ') || 'move'} ${moveLabel(move.uci || '')}`);
    const lost = (comparison.lost_ideas || []).filter(isPlayedComparisonLoss).map(codeLabel);
    return [
      pvMoves.length ? `PV continues with ${joinHuman(pvMoves)}` : rootMoves.length ? rootMoves.join(', ') : '',
      lost.length ? `Lost idea: ${joinHuman(lost)}` : '',
    ].filter(Boolean);
  }));
}

function compactRepeatedMoves(moves: string[]): string[] {
  const half = moves.length / 2;
  if (moves.length > 2 && moves.length % 2 === 0 && moves.slice(0, half).every((move, i) => move === moves[i + half])) {
    return moves.slice(0, half);
  }
  return moves;
}

function moveLabel(uci: string): string {
  return uci.replace(/^([a-h][1-8])([a-h][1-8])([nbrq])?$/, '$1-$2$3');
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
  const evidenceSemantics = semantics.filter(hasConcreteSurfaceCarrier);
  const terminal = cleanTerminalLabels(uniqueLabels(evidenceSemantics.flatMap(terminalLabels)));
  if (terminal.includes('material sacrifice')) return `The line confirms ${joinHuman(terminal)}.`;
  if (terminal.length === 1 && terminal[0] === 'material gain') return 'The line wins material.';
  if (terminal.length === 1 && terminal[0] === 'material loss') return 'The line loses material.';
  if (terminal.length) return `The terminal result is ${joinHuman(terminal)}.`;
  const technique = uniqueLabels(evidenceSemantics.flatMap(techniqueLabels));
  if (technique.length) return `The ending technique evidence is ${joinHuman(technique)}.`;
  const carriers = conciseCarrierLabels(evidenceSemantics.flatMap(boardCarrierLabels));
  if (carriers.length) return `The concrete board evidence is ${joinHuman(carriers.slice(0, 5))}.`;
  return undefined;
}

function evidenceItems(semantics: ChesstoryMoveSemantic[]): string[] {
  const evidenceSemantics = semantics.filter(hasConcreteSurfaceCarrier);
  const terminal = cleanTerminalLabels(uniqueLabels(evidenceSemantics.flatMap(terminalLabels)));
  return conciseCarrierLabels(
    evidenceSemantics
      .flatMap(s => [
        ...boardCarrierLabels(s),
        ...techniqueLabels(s),
      ])
      .concat(terminal),
  );
}

function hasEvidenceCarrier(semantic: ChesstoryMoveSemantic): boolean {
  return semantic.evidence?.has_carrier === true;
}

function hasConcreteSurfaceCarrier(semantic: ChesstoryMoveSemantic): boolean {
  return (
    hasEvidenceCarrier(semantic) &&
    ((semantic.evidence?.board_carriers || []).length > 0 ||
      terminalLabels(semantic).length > 0 ||
      techniqueLabels(semantic).length > 0)
  );
}

function normalizeCode(code?: string): string {
  return (code || '')
    .replace(/([a-z0-9])([A-Z])/g, '$1_$2')
    .toLowerCase();
}
function uniqueLabels(labels: string[]): string[] {
  return [...new Set(labels.map(l => l.trim()).filter(Boolean))];
}

function conciseCarrierLabels(labels: string[]): string[] {
  const unique = uniqueLabels(labels);
  const standaloneSquare = (label: string) => label.match(/^(?:the )?([a-h][1-8])(?: square)?$/)?.[1];
  const barePieces = new Set(['king', 'queen', 'rook', 'bishop', 'knight', 'pawn', 'piece']);
  const hasConcrete = unique.some(label => !barePieces.has(label));
  const coveredFiles = new Set(unique.flatMap(label => label.match(/^([a-h])-file break$/)?.[1] || []));
  const pieceRoutes = new Set(
    unique.flatMap(label => label.match(/^(?:king|queen|rook|bishop|knight) ([a-h][1-8]-[a-h][1-8])$/)?.slice(1) || []),
  );
  const coveredRoutes = new Set(
    unique.flatMap(label => label.match(/^(?:passed pawn advance|rook lift) ([a-h][1-8]-[a-h][1-8])$/)?.slice(1) || []),
  );
  const coveredSquares = new Set(
    unique.flatMap(label => [
      ...(label.match(/^(?:material sacrifice|weak square|check|weak pawn) on ([a-h][1-8])$/)?.slice(1) || []),
      ...(label.match(/^line opening from ([a-h][1-8])$/)?.slice(1) || []),
      ...(label.match(/^passed pawn on ([a-h][1-8])$/)?.slice(1) || []),
      ...(
        label.match(/^(?:passed pawn advance|rook lift|created tension|(?:battery|pin) pressure (?:on|along)) ([a-h][1-8])(?:-([a-h][1-8]))?$/)
          ?.slice(1)
          .filter(Boolean) || []
      ),
      ...(label.match(/^tension between ([a-h][1-8]) and ([a-h][1-8])$/)?.slice(1) || []),
      ...(label.match(/^tension on ([a-h][1-8])$/)?.slice(1) || []),
      ...(label.match(/^([a-h][1-8])-([a-h][1-8])$/)?.slice(1) || []),
      ...(label.match(/^(?:king|queen|rook|bishop|knight) ([a-h][1-8])-([a-h][1-8])$/)?.slice(1) || []),
    ]),
  );
  const concise = unique
    .filter(label => !hasConcrete || !barePieces.has(label))
    .filter(label => !label.match(/^[a-h][1-8]-[a-h][1-8]$/) || (!coveredRoutes.has(label) && !pieceRoutes.has(label)))
    .filter(label => {
      const route = label.match(/^(?:king|queen|rook|bishop|knight) ([a-h][1-8]-[a-h][1-8])$/)?.[1];
      return !route || !coveredRoutes.has(route);
    })
    .filter(label => !label.match(/^([a-h])-file$/) || !coveredFiles.has(label[0]))
    .filter(label => {
      const square = standaloneSquare(label);
      return !square || !coveredSquares.has(square);
    })
    .map(label => {
      const square = standaloneSquare(label);
      return square ? `the ${square} square` : label;
    });
  const weakSquares = concise.flatMap(label => label.match(/^weak square on ([a-h][1-8])$/)?.slice(1) || []);
  if (weakSquares.length < 2) return concise;
  let weakSquareGroupUsed = false;
  return concise.flatMap(label => {
    if (!label.startsWith('weak square on ')) return [label];
    if (weakSquareGroupUsed) return [];
    weakSquareGroupUsed = true;
    return [`weak squares ${joinHuman(weakSquares)}`];
  });
}

function firstLabel(labels: string[]): string | undefined {
  return labels.find(Boolean);
}

function joinHuman(items: string[]): string {
  if (items.length <= 1) return items[0] || '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
}
