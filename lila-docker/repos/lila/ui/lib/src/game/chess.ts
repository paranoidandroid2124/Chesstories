/* eslint no-restricted-syntax:"error" */ // no side effects allowed due to re-export by index.ts

import { parseUci } from 'chessops';

const destChars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!?';
const files = 'abcdefgh';
const uciChar = Object.fromEntries(
  Array.from(destChars, (char, i) => [char, `${files[i % 8]}${Math.floor(i / 8) + 1}` as Key]),
) as Record<string, Key>;

export const fenColor = (fen: string): Color => (fen.includes(' w') ? 'white' : 'black');

export const readDests = (lines?: string): Dests | null => {
  if (lines == null) return null;
  if (lines === '') return new Map();
  return lines.split(' ').reduce<Dests>((dests, line) => {
    dests.set(
      uciChar[line[0]],
      line
        .slice(1)
        .split('')
        .map(c => uciChar[c]),
    );
    return dests;
  }, new Map());
};

const charByKey = (key: Key): string => destChars[files.indexOf(key[0]) + (Number(key[1]) - 1) * 8];

export const writeDests = (dests: Dests): string => {
  if (!dests.size) return '';
  return Array.from(dests, ([orig, dests]) => {
    const origChar = charByKey(orig);
    const destChars = dests.map(charByKey).join('');
    return origChar + destChars;
  }).join(' ');
};

// Extended Position Description
export const fenToEpd = (fen: FEN): string => fen.split(' ').slice(0, 4).join(' ');

export const plyToTurn = (ply: number): number => Math.floor((ply - 1) / 2) + 1;

export const pieceCount = (fen: FEN): number => fen.split(/\s/)[0].split(/[nbrqkp]/i).length - 1;

export function isUci(maybeUci: string | undefined | null): maybeUci is Uci {
  return !!parseUci(maybeUci ?? '');
}

export function validUci(maybeUci: string | undefined | null): Uci | undefined {
  return isUci(maybeUci) ? maybeUci : undefined;
}
