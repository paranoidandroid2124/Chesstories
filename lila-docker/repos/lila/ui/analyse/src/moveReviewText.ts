import type { MoveReviewFrameSelection, MoveReviewProof } from './moveReview';

export type CommentaryMoveToken = { text: string; frames: MoveReviewFrameSelection[] };

/** Only the supplied owners can resolve a mention. Repeated SAN keeps a choice of positions. */
export function commentaryMoveTokens(text: string, paths: MoveReviewProof[], startPly = 1): CommentaryMoveToken[] {
  const mentions = new Map<string, Map<string, MoveReviewFrameSelection>>();
  for (const path of paths)
    path.moves.forEach((move, index) => {
      const positions = mentions.get(move.label) ?? new Map<string, MoveReviewFrameSelection>();
      // Shared prefixes represent the same move and position, even in two different continuations.
      const identity = JSON.stringify([path.startFen, path.moves.slice(0, index + 1).map(m => [m.uci, m.fenAfter])]);
      if (!positions.has(identity)) positions.set(identity, { proofId: path.id, ply: index + 1, phase: 'after' });
      mentions.set(move.label, positions);
    });
  if (!mentions.size) return [{ text, frames: [] }];
  const labels = [...mentions.keys()].sort((a, b) => b.length - a.length)
    .map(label => label.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'));
  const pattern = new RegExp(`(?<![\\w-])(?:${labels.join('|')})(?![\\w=+#-])`, 'g');
  const result: CommentaryMoveToken[] = [];
  let end = 0;
  for (const match of text.matchAll(pattern)) {
    // A square in prose is not a pawn move just because its spelling is also a SAN.
    if (/^[a-h][1-8]$/.test(match[0]) &&
      /\b(?:on|to|from|through|toward|at|square|squares|clears|controls|occupies)\s+$/i.test(text.slice(0, match.index)))
      continue;
    if (match.index > end) result.push({ text: text.slice(end, match.index), frames: [] });
    let frames = [...mentions.get(match[0])!.values()];
    const sentenceStart = text.lastIndexOf('. ', match.index) + 2;
    const sentenceEnd = text.indexOf('. ', match.index);
    const sentence = text.slice(sentenceStart < 2 ? 0 : sentenceStart, sentenceEnd < 0 ? undefined : sentenceEnd);
    const declared = /\b(?:the provided|the) (\S+) line\b/.exec(sentence);
    if (declared) {
      const owners = new Set(paths.filter(path => path.moves[startPly - 1]?.label === declared[1]).map(path => path.id));
      const rootMention = /\b(?:the provided|the) $/.test(text.slice(0, match.index)) &&
        text.slice(match.index + match[0].length).startsWith(' line');
      const scoped = frames.filter(frame => owners.has(frame.proofId) && (!rootMention || frame.ply === startPly));
      if (scoped.length) frames = scoped;
    }
    result.push({ text: match[0], frames });
    end = match.index + match[0].length;
  }
  if (end < text.length) result.push({ text: text.slice(end), frames: [] });
  return result;
}
