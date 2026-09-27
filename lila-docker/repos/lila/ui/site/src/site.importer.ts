import { reviewPgnError } from 'lib/game/reviewPgn';

export function initImporter() {
  const form = document.getElementById('import-workspace') as HTMLFormElement | null;
  const pgn = document.getElementById('import-pgn') as HTMLTextAreaElement | null;
  const file = document.getElementById('import-file') as HTMLInputElement | null;
  const status = document.getElementById('import-workspace-status');
  const submit = document.getElementById('import-submit') as HTMLButtonElement | null;
  if (!form || !pgn || !file || !status || !submit) return;

  const byteLimit = Number(pgn.dataset.importByteLimit);
  const characterLimit = Number(pgn.dataset.importCharacterLimit);
  let generation = 0;
  let reading = false;
  const clearFile = () => {
    file.value = '';
  };
  const overCharacterLimit = (value: string) => value.trim().length > characterLimit;
  const showStatus = (message: string, error = false) => {
    status.textContent = message;
    status.classList.toggle('import-message--error', error);
    status.classList.toggle('import-message', !!message);
    status.setAttribute('role', error ? 'alert' : 'status');
    pgn.setAttribute('aria-invalid', String(error));
    submit.setAttribute('aria-disabled', String(error));
  };
  const syncManualStatus = () => {
    const error = pgn.value.trim() ? reviewPgnError(pgn.value) : undefined;
    showStatus(pgn.value.trim() ? error ?? 'PGN ready. Open the review when you are ready.' : '', !!error);
  };
  const setReading = (value: boolean) => {
    reading = value;
    submit.disabled = value;
    form.toggleAttribute('aria-busy', value);
  };

  pgn.addEventListener('input', () => {
    generation++;
    setReading(false);
    clearFile();
    syncManualStatus();
  });

  file.addEventListener('change', () => {
    const token = ++generation;
    const selected = file.files?.[0];
    if (!selected) {
      setReading(false);
      clearFile();
      syncManualStatus();
      return;
    }
    if (selected.size > byteLimit) {
      clearFile();
      setReading(false);
      showStatus(`Choose a PGN file no larger than ${byteLimit.toLocaleString()} bytes before reading.`, true);
      pgn.focus();
      return;
    }

    setReading(true);
    showStatus(`Reading ${selected.name}...`);
    void selected
      .arrayBuffer()
      .then(bytes => new TextDecoder('utf-8', { fatal: true }).decode(bytes))
      .then(text => {
        if (token !== generation) return;
        if (overCharacterLimit(text)) {
          showStatus(`The loaded file exceeds the ${characterLimit.toLocaleString()}-character PGN limit.`, true);
          pgn.focus();
          return;
        }
        pgn.value = text;
        const error = reviewPgnError(text);
        showStatus(error ?? `Loaded ${selected.name}. Review the PGN, then open the board.`, !!error);
        pgn.focus();
      })
      .catch(() => {
        if (token !== generation) return;
        showStatus('That file could not be read. Paste the PGN text instead.', true);
        pgn.focus();
      })
      .finally(() => {
        if (token !== generation) return;
        clearFile();
        setReading(false);
      });
  });

  syncManualStatus();
  form.addEventListener('submit', event => {
    if (reading) {
      event.preventDefault();
      showStatus('Wait for the selected file to finish loading, or paste PGN text.');
      pgn.focus();
    } else if (reviewPgnError(pgn.value)) {
      event.preventDefault();
      showStatus(reviewPgnError(pgn.value)!, true);
      pgn.focus();
    }
  });
}

if (typeof document !== 'undefined') {
  if (document.readyState === 'loading')
    document.addEventListener('DOMContentLoaded', initImporter, { once: true });
  else initImporter();
}
