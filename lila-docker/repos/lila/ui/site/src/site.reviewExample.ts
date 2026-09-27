import { Chessground } from '@lichess-org/chessground';
import { Chess } from 'chessops/chess';
import { parseFen, makeFen } from 'chessops/fen';
import { parseUci, makeSquare } from 'chessops/util';
import { makeSan } from 'chessops/san';

export function initReviewExamples(): void {
  document.querySelectorAll<HTMLElement>('.review-example').forEach(example => {
    const initialFen = example.dataset.fen!;
    const lines = { played: ['h2h3'], reference: ['c2c3', 'd7d6', 'd3d4'] };
    const descriptions = {
      played: ['White to move. Compare a useful waiting move with preparation in the centre.', '6. h3 prevents …Bg4, but does not support the central break.'],
      reference: ['White to move. Try preparing d4 first.', '6. c3 supports d4. The c-pawn can recapture if Black exchanges in the centre.', '6…d6 reinforces e5. White can now put the preparation to use.', '7. d4 challenges e5, with c3 supporting the pawn in the centre.'],
    };
    const board = Chessground(example.querySelector<HTMLElement>('.review-example__board')!, {
      fen: initialFen, viewOnly: true, coordinates: true,
      drawable: { enabled: false, visible: true },
      animation: { enabled: !matchMedia('(prefers-reduced-motion: reduce)').matches, duration: 240 },
    });
    let selected: keyof typeof lines = 'played';
    let step = 0;
    const previous = example.querySelector<HTMLButtonElement>('[data-step="prev"]')!;
    const next = example.querySelector<HTMLButtonElement>('[data-step="next"]')!;
    const render = () => {
      const position = Chess.fromSetup(parseFen(initialFen).unwrap()).unwrap();
      let lastMove: [Key, Key] | undefined;
      let san = 'Starting position';
      for (const uci of lines[selected].slice(0, step)) {
        const move = parseUci(uci);
        if (!move || !('from' in move) || !position.isLegal(move)) throw new Error('Invalid public example move');
        san = `${position.fullmoves}${position.turn === 'white' ? '. ' : '… '}${makeSan(position, move)}`;
        lastMove = [makeSquare(move.from), makeSquare(move.to)];
        position.play(move);
      }
      board.set({ fen: makeFen(position.toSetup()), lastMove, turnColor: position.turn });
      board.setAutoShapes(selected === 'reference' && step > 0
        ? [{ orig: 'c3', dest: 'd4', brush: 'blue' }, ...(step === 3
          ? [{ orig: 'd4' as Key, dest: 'e5' as Key, brush: 'green' }]
          : lastMove ? [{ orig: lastMove[0], dest: lastMove[1], brush: 'green' }] : [])]
        : lastMove ? [{ orig: lastMove[0], dest: lastMove[1], brush: 'green' }] : []);
      example.querySelector<HTMLElement>('.review-example__progress')!.textContent = step + ' / ' + lines[selected].length;
      example.querySelector<HTMLElement>('.review-example__hint')!.textContent =
        selected === 'reference' && step > 0 ? 'Blue: support for d4 · Green: move or attack' : 'Choose a move, then use the arrows to compare.';
      previous.disabled = step === 0;
      next.disabled = step === lines[selected].length;
      example.querySelector<HTMLElement>('.review-example__move')!.textContent = san;
      example.querySelector<HTMLElement>('.review-example__caption')!.textContent = descriptions[selected][step];
      example.querySelectorAll<HTMLButtonElement>('[data-line]').forEach(button =>
        button.setAttribute('aria-pressed', String(button.dataset.line === selected)),
      );
    };
    example.querySelectorAll<HTMLButtonElement>('[data-line]').forEach(button => {
      button.addEventListener('click', () => {
        selected = button.dataset.line as keyof typeof lines;
        step = 1;
        render();
      });
    });
    previous.addEventListener('click', () => { step = Math.max(0, step - 1); render(); });
    next.addEventListener('click', () => { step = Math.min(lines[selected].length, step + 1); render(); });
    render();
  });
}

if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initReviewExamples, { once: true });
else initReviewExamples();
