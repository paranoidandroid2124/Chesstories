const maxInlinePgnChars = 200000;
const base64ChunkSize = 0x2000;

export const emptyPgnError = 'Paste a game before opening the board.';

export const pgnInputError = (rawPgn: string | undefined | null): string => {
  const trimmed = rawPgn?.trim();
  return trimmed && trimmed.length > maxInlinePgnChars
    ? `PGN must be ${maxInlinePgnChars.toLocaleString()} characters or fewer.`
    : emptyPgnError;
};

export type PgnDraftStatus = 'empty' | 'current' | 'ready' | 'invalid';

export const normalizeInlinePgn = (rawPgn: string | undefined | null): string | undefined => {
  const trimmed = rawPgn?.trim();
  return trimmed && trimmed.length <= maxInlinePgnChars ? trimmed : undefined;
};

const encodePgn64 = (pgn: string): string => {
  const bytes = new TextEncoder().encode(pgn);
  let binary = '';
  for (let i = 0; i < bytes.length; i += base64ChunkSize) {
    binary += String.fromCharCode(...bytes.subarray(i, i + base64ChunkSize));
  }
  return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
};

export const submitPgnToImportPipeline = (rawPgn: string): boolean => {
  try {
    const normalized = normalizeInlinePgn(rawPgn);
    if (!normalized) return false;

    const form = document.createElement('form');
    form.method = 'post';
    form.action = '/import';
    form.style.display = 'none';

    const pgn64 = document.createElement('input');
    pgn64.type = 'hidden';
    pgn64.name = 'pgn64';
    pgn64.value = encodePgn64(normalized);

    form.appendChild(pgn64);
    document.body.appendChild(form);
    form.submit();
    return true;
  } catch (_) {
    return false;
  }
};
