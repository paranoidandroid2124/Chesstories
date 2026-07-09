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
  label?: string;
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
  return uniqueCarriers(carriers)
    .filter(
      carrier =>
        carrier.role === 'target' ||
        (carrier.role === 'actor' && carrier.kind === 'Move') ||
        relationCarrierRoles.has(carrier.role || ''),
    )
    .sort((a, b) => boardCarrierRank(a) - boardCarrierRank(b))
    .map(carrier => boardCarrierLabel(carrier, actorPiece));
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
  const route = carrier.from && carrier.to ? `${carrier.from}-${carrier.to}` : '';
  const value = carrier.label || (carrier.kind === 'Move' && route ? route : carrierValueLabel(carrier.kind, carrier.value));
  if (carrier.kind === 'Move' && route) {
    const piece = actorPiece ? `${actorPiece} ` : '';
    return `${piece}${value || route}`;
  }
  const square = squareValue(carrier.value);
  if (carrier.kind === 'Square' && square) return `the ${square} square`;
  return [value, route].filter(Boolean).join(' ');
}

function carrierValueLabel(kind?: string, value?: string): string {
  const raw = value || '';
  if (kind === 'File' && raw) return `${raw}-file`;
  if (kind === 'Pawn' && raw.startsWith('weak-pawn:')) return `weak pawn on ${raw.slice('weak-pawn:'.length)}`;
  return kind === 'PlanSubject' ? labelCode(raw) : raw;
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

function uniqueCarriers(carriers: ChesstoryBoardCarrier[]): ChesstoryBoardCarrier[] {
  const seen = new Set<string>();
  return carriers.filter(carrier => {
    const key = [carrier.role, carrier.kind, carrier.value, carrier.from, carrier.to, carrier.semantic_role].join('|');
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function squareValue(value?: string): string | undefined {
  const normalized = (value || '').trim().toLowerCase();
  return normalized.match(/^[a-h][1-8]$/) ? normalized : undefined;
}

function joinHuman(items: string[]): string {
  if (items.length <= 1) return items[0] || '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
}
