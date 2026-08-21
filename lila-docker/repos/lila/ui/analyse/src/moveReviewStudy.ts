import type { TreeWrapper } from 'lib/tree';
import { makeSquare, parseUci } from 'chessops/util';
import type { NormalMove } from 'chessops/types';
import * as studyApi from './studyApi';
import type { MoveReviewProof, MoveReviewSubject } from './moveReview';

export async function mergeMoveReviewProofIntoStudy(
  tree: TreeWrapper,
  ref: studyApi.StudyRef,
  subject: MoveReviewSubject,
  proof: MoveReviewProof,
): Promise<Tree.Path> {
  let path = subject.before.path;
  for (const proofMove of proof.moves) {
    const parent = tree.nodeAtPath(path);
    if (!parent) throw new Error('Proof parent is no longer available in this Study.');
    const existing = parent.children.find(child => child.uci === proofMove.uci);
    if (existing) {
      if (existing.fen !== proofMove.fenAfter)
        throw new Error('The existing Study line does not match the verified replay.');
      path += existing.id;
      continue;
    }
    const move = parseUci(proofMove.uci) as NormalMove;
    const response = await studyApi.anaMove(ref, {
      orig: makeSquare(move.from),
      dest: makeSquare(move.to),
      fen: parent.fen,
      path,
      variant: subject.variant,
      promotion: move.promotion,
      ch: ref.chapterId,
    });
    if (response.node.fen !== proofMove.fenAfter)
      throw new Error('The Study move response does not match the verified replay.');
    const addedPath = tree.addNode(response.node, path);
    if (!addedPath) throw new Error('Proof parent is no longer available in this Study.');
    path = addedPath;
  }
  return path;
}
