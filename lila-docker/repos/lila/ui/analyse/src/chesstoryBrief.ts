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
  idea_chains?: ChesstoryIdeaChain[];
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
    board_carriers?: ChesstoryBoardCarrier[];
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
}

interface ChesstoryCode {
  code?: string;
  label?: string;
}

interface ChesstoryComparisonLoss extends ChesstoryCode {
  side?: string;
}

interface ChesstoryBoardCarrier {
  subject?: string;
  line_role?: string;
  move_uci?: string;
  role?: string;
  kind?: string;
  value?: string;
  from?: string;
  to?: string;
  semantic_role?: string;
}

const relationCarrierRoles = new Set(['attacker', 'defender', 'blocker', 'beneficiary', 'king', 'mover', 'bait', 'lured']);

export interface ChesstoryIdeaChain {
  key: 'current-move-chain';
  current_move?: string;
  reference_move?: string;
  move_quality?: string;
  subject: string;
  move_semantics?: ChesstoryMoveSemantic[];
  proof_levels: string[];
  carriers: ChesstoryBoardCarrier[];
  function_carriers?: ChesstoryBoardCarrier[];
  pv: string[];
  consequence_carriers: ChesstoryBoardCarrier[];
  terminal_consequences: ChesstoryCode[];
  technique: unknown[];
  player_facing_reason_allowed: true;
}

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  const chains = (payload?.idea_chains || []).filter(chain => chain.player_facing_reason_allowed === true);
  const chain = chains.find(item => item.subject === 'played_move') || chains[0];
  if (!chain) return placeholderSections();

  const currentMove = moveLabel(chain.current_move || payload?.verdict?.played_move || '');
  const referenceMove = moveLabel(chain.reference_move || payload?.verdict?.reference_move || '');
  const moveQuality = labelCode(chain.move_quality || payload?.verdict?.move_quality);
  const proofLevels = uniqueLabels(chain.proof_levels.map(labelCode)).slice(0, 4);
  const ideas = uniqueLabels((chain.move_semantics || []).map(semantic => codeLabel(semantic.idea) || labelCode(semantic.idea_type))).slice(0, 5);
  const carriers = carrierLabels(chain.carriers).slice(0, 5);
  const consequences = carrierLabels(chain.consequence_carriers).slice(0, 6);
  const terminal = uniqueLabels(chain.terminal_consequences.map(codeLabel)).slice(0, 4);
  const technique = techniqueLabels(chain.technique).slice(0, 4);
  const pv = uniqueLabels(chain.pv.map(moveLabel)).slice(0, 6);
  const consequenceItems = uniqueLabels([...consequences, ...terminal, ...technique]).slice(0, 6);
  const chainItems = uniqueLabels([...carriers, ...consequenceItems]).slice(0, 6);
  const tone = moveQuality === 'bad' ? 'bad' : moveQuality ? 'good' : 'neutral';

  return [
    {
      key: 'opening-idea',
      title: 'Idea chain',
      body: chainItems.length ? joinHuman(chainItems) : ideas.length ? joinHuman(ideas) : [subjectLabel(chain.subject), currentMove].filter(Boolean).join(' / '),
      pending: false,
      items: [`Move quality: ${moveQuality || 'available'}`, ...proofLevels.map(level => `Proof: ${level}`), ...ideas.map(idea => `Idea: ${idea}`)],
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'Board carriers',
      body: carriers.length ? joinHuman(carriers) : 'No public board carrier in this chain.',
      pending: false,
      items: carriers,
      tone,
    },
    {
      key: 'current-decision',
      title: 'Concrete consequences',
      body: consequenceItems.length ? joinHuman(consequenceItems) : 'No public consequence carrier in this chain.',
      pending: false,
      items: consequenceItems,
      tone,
    },
    {
      key: 'better-plan',
      title: referenceMove && referenceMove !== currentMove ? 'Reference chain' : 'PV / forced line',
      body: pv.length ? joinHuman(pv) : 'No public PV in this chain.',
      pending: false,
      items: referenceMove && referenceMove !== currentMove ? [`Reference: ${referenceMove}`, ...pv] : pv,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'Evidence',
      body: proofLevels.length ? joinHuman(proofLevels) : 'Available',
      pending: false,
      items: proofLevels,
    },
  ];
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

function carrierLabels(carriers: ChesstoryBoardCarrier[]): string[] {
  const actorPiece = actorRoutePiece(carriers);
  const publicCarriers = carriers
    .filter(
      carrier =>
        carrier.role === 'target' ||
        (carrier.role === 'actor' && carrier.kind === 'Move') ||
        relationCarrierRoles.has(carrier.role || ''),
    )
    .sort((a, b) => boardCarrierRank(a) - boardCarrierRank(b));
  return conciseCarrierLabels(publicCarriers, actorPiece);
}

function boardCarrierRank(carrier: ChesstoryBoardCarrier): number {
  if (carrier.role === 'actor' && carrier.kind === 'Move') return 0;
  if (carrier.role === 'target' && carrier.kind === 'PlanSubject') return 1;
  if (carrier.role === 'target' && (carrier.kind === 'File' || carrier.kind === 'Square' || carrier.kind === 'Pawn')) return 2;
  if (relationCarrierRoles.has(carrier.role || '')) return 3;
  if (carrier.role === 'target' && carrier.kind === 'Piece') return 3;
  if (carrier.role !== 'target') return 4;
  return 4;
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
  const square = squareValue(carrier.value);
  if (carrier.kind === 'Square' && square) return `the ${square} square`;
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
  if (normalized.includes(',')) return joinHuman(normalized.split(',').map(planSubjectLabel));
  const openedLine = normalized.match(/^(?:opened-line|line-unlock):([a-h][1-8])$/);
  if (openedLine) return `opens a line from ${openedLine[1]}`;
  const squareSubject = normalized.match(/^(material-sacrifice|material-capture|material-recapture|weak-square|check):([a-h][1-8])$/);
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
  const createdTension = normalized.match(/^created-tension:([a-h][1-8])(-[a-h][1-8])?$/);
  if (createdTension?.[2]) return `tension between ${createdTension[1]} and ${createdTension[2].slice(1)}`;
  if (createdTension) return `tension on ${createdTension[1]}`;
  const rookLift = normalized.match(/^rook-lift:([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
  if (rookLift) return `rook lift ${rookLift[1]}-${rookLift[2]}`;
  return labelCode(normalized);
}

function techniqueLabels(values: unknown[]): string[] {
  return uniqueLabels(values.flatMap(value => {
    if (!value || typeof value !== 'object') return [];
    const record = value as Record<string, unknown>;
    return [
      nestedCodeLabel(record.pattern_info),
      nestedCodeLabel(record.rook_geometry),
      stringValue(record.status_label),
      stringValue(record.trigger_move).map(move => `trigger ${moveLabel(move)}`),
    ].flat();
  }));
}

function nestedCodeLabel(value: unknown): string[] {
  if (!value || typeof value !== 'object') return [];
  const label = (value as Record<string, unknown>).label;
  return typeof label === 'string' && label ? [label] : [];
}

function stringValue(value: unknown): string[] {
  return typeof value === 'string' && value ? [value] : [];
}

function codeLabel(code?: ChesstoryCode): string {
  return code?.label || labelCode(code?.code);
}

function labelCode(raw?: string): string {
  return (raw || '').replace(/[_-]+/g, ' ').trim().toLowerCase();
}

function subjectLabel(raw: string): string {
  return labelCode(raw) || 'move chain';
}

function moveLabel(uci: string): string {
  return uci.replace(/^([a-h][1-8])([a-h][1-8])([nbrq])?$/, '$1-$2$3');
}

function uniqueLabels(labels: string[]): string[] {
  return [...new Set(labels.map(label => label.trim()).filter(Boolean))];
}

function conciseCarrierLabels(carriers: ChesstoryBoardCarrier[], actorPiece?: string): string[] {
  const unique = uniqueCarriers(carriers);
  const barePieces = new Set(['king', 'queen', 'rook', 'bishop', 'knight', 'pawn', 'piece']);
  const hasConcrete = unique.some(carrier => !(carrier.kind === 'Piece' && barePieces.has(normalizedCarrierValue(carrier))));
  const coveredFiles = new Set(unique.flatMap(breakFileFromCarrier));
  const materialCaptureSquares = new Set(unique.flatMap(carrier => planSubjectSquare(carrier, 'material-capture')));
  const materialSacrificeSquares = new Set(unique.flatMap(carrier => planSubjectSquare(carrier, 'material-sacrifice')));
  const captureSacrificeSquares = new Set([...materialCaptureSquares].filter(square => materialSacrificeSquares.has(square)));
  const coveredRoutes = new Set(
    unique.flatMap(carrier =>
      carrier.kind === 'PlanSubject' ? planSubjectRoute(carrier.value) : [],
    ),
  );
  const coveredSquares = new Set(
    unique.filter(carrier => !standaloneSquareCarrier(carrier)).flatMap(carrierSquares),
  );
  const captureSacrificeUsed = new Set<string>();
  const concise = unique
    .flatMap(carrier => {
      const square =
        planSubjectSquare(carrier, 'material-capture')[0] ||
        planSubjectSquare(carrier, 'material-sacrifice')[0];
      if (!square || !captureSacrificeSquares.has(square)) return [carrier];
      if (captureSacrificeUsed.has(square)) return [];
      captureSacrificeUsed.add(square);
      return [`capture and sacrifice on ${square}`];
    })
    .filter((carrierOrLabel): carrierOrLabel is ChesstoryBoardCarrier | string => {
      if (typeof carrierOrLabel === 'string') return true;
      return !hasConcrete || !(carrierOrLabel.kind === 'Piece' && barePieces.has(normalizedCarrierValue(carrierOrLabel)));
    })
    .filter(carrierOrLabel => {
      if (typeof carrierOrLabel === 'string') return true;
      const route = routeValue(carrierOrLabel);
      return !route || carrierOrLabel.kind !== 'Move' || !coveredRoutes.has(route);
    })
    .filter(carrierOrLabel => {
      if (typeof carrierOrLabel === 'string') return true;
      const file = fileValue(carrierOrLabel);
      return !file || carrierOrLabel.kind !== 'File' || !coveredFiles.has(file);
    })
    .filter(carrierOrLabel => {
      if (typeof carrierOrLabel === 'string') return true;
      const square = standaloneSquareCarrier(carrierOrLabel);
      return !square || !coveredSquares.has(square);
    })
    .map(carrierOrLabel => {
      if (typeof carrierOrLabel === 'string') return carrierOrLabel;
      return boardCarrierLabel(carrierOrLabel, actorPiece);
    });
  const weakSquares = unique.flatMap(carrier => planSubjectSquare(carrier, 'weak-square'));
  if (weakSquares.length < 2) return concise;
  const weakSquareLabels = new Set(weakSquares.map(square => `weak square on ${square}`));
  let weakSquareGroupUsed = false;
  return concise.flatMap(label => {
    if (!weakSquareLabels.has(label)) return [label];
    if (weakSquareGroupUsed) return [];
    weakSquareGroupUsed = true;
    return [`weak squares ${joinHuman(weakSquares)}`];
  });
}

function uniqueCarriers(carriers: ChesstoryBoardCarrier[]): ChesstoryBoardCarrier[] {
  const seen = new Set<string>();
  return carriers.filter(carrier => {
    const key = [carrier.role, carrier.kind, carrier.value, carrier.from, carrier.to, carrier.semantic_role].join('|');
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function normalizedCarrierValue(carrier: ChesstoryBoardCarrier): string {
  return (carrier.value || '').trim().toLowerCase();
}

function squareValue(value?: string): string | undefined {
  const normalized = (value || '').trim().toLowerCase();
  return normalized.match(/^[a-h][1-8]$/) ? normalized : undefined;
}

function fileValue(carrier: ChesstoryBoardCarrier): string | undefined {
  const normalized = normalizedCarrierValue(carrier);
  return carrier.kind === 'File' && normalized.match(/^[a-h]$/) ? normalized : undefined;
}

function routeValue(carrier: ChesstoryBoardCarrier): string | undefined {
  const from = squareValue(carrier.from);
  const to = squareValue(carrier.to);
  return from && to ? `${from}-${to}` : undefined;
}

function standaloneSquareCarrier(carrier: ChesstoryBoardCarrier): string | undefined {
  return carrier.kind === 'Square' ? squareValue(carrier.value) : undefined;
}

function breakFileFromCarrier(carrier: ChesstoryBoardCarrier): string[] {
  const match = carrier.kind === 'PlanSubject' ? normalizedCarrierValue(carrier).match(/^break-file:([a-h])$/) : undefined;
  return match ? [match[1]] : [];
}

function planSubjectSquare(carrier: ChesstoryBoardCarrier, prefix: string): string[] {
  if (carrier.kind !== 'PlanSubject') return [];
  return normalizedCarrierValue(carrier)
    .split(',')
    .flatMap(value => value.match(new RegExp(`^${prefix}:([a-h][1-8])$`))?.slice(1) || []);
}

function planSubjectRoute(value?: string): string[] {
  return (value || '')
    .trim()
    .toLowerCase()
    .split(',')
    .flatMap(part => {
      const route = part.match(/^(?:passed-pawn-advanced|rook-lift):([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
      return route ? [`${route[1]}-${route[2]}`] : [];
    });
}

function carrierSquares(carrier: ChesstoryBoardCarrier): string[] {
  const route = routeValue(carrier)?.split('-') || [];
  if (route.length) return route;
  if (carrier.kind === 'Pawn') {
    const weakPawn = normalizedCarrierValue(carrier).match(/^weak-pawn:([a-h][1-8])$/);
    return weakPawn ? [weakPawn[1]] : [];
  }
  if (carrier.kind !== 'PlanSubject') return [];
  return normalizedCarrierValue(carrier)
    .split(',')
    .flatMap(value => {
      const singleSquare = value.match(
        /^(?:material-sacrifice|material-capture|material-recapture|weak-square|check|defender-move|opened-line|line-unlock|passed-pawn):([a-h][1-8])$/,
      );
      if (singleSquare) return [singleSquare[1]];
      const routeSubject = value.match(/^(?:passed-pawn-advanced|rook-lift):([a-h][1-8])-([a-h][1-8]):rank-\d+$/);
      if (routeSubject) return routeSubject.slice(1, 3);
      const pressure = value.match(/^(?:battery-pressure|pin-pressure|created-tension):([a-h][1-8])(?:-([a-h][1-8]))?$/);
      return pressure ? pressure.slice(1).filter((square): square is string => Boolean(square)) : [];
    });
}

function joinHuman(items: string[]): string {
  if (items.length <= 1) return items[0] || '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
}
