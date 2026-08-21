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
  const characterLimitMessage = () =>
    `PGN must be ${characterLimit.toLocaleString()} characters or fewer before opening the board.`;
  const syncManualStatus = () => {
    status.textContent = overCharacterLimit(pgn.value) ? characterLimitMessage() : '';
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
      status.textContent = `Choose a PGN file no larger than ${byteLimit.toLocaleString()} bytes before reading.`;
      pgn.focus();
      return;
    }

    setReading(true);
    status.textContent = `Reading ${selected.name}...`;
    void selected
      .arrayBuffer()
      .then(bytes => new TextDecoder('utf-8', { fatal: true }).decode(bytes))
      .then(text => {
        if (token !== generation) return;
        if (overCharacterLimit(text)) {
          status.textContent = `The loaded file exceeds the ${characterLimit.toLocaleString()}-character PGN limit.`;
          pgn.focus();
          return;
        }
        pgn.value = text;
        status.textContent = `Loaded ${selected.name}. Review the PGN, then open the board.`;
        pgn.focus();
      })
      .catch(() => {
        if (token !== generation) return;
        status.textContent = 'That file could not be read. Paste the PGN text instead.';
        pgn.focus();
      })
      .finally(() => {
        if (token !== generation) return;
        clearFile();
        setReading(false);
      });
  });

  form.addEventListener('submit', event => {
    if (reading) {
      event.preventDefault();
      status.textContent = 'Wait for the selected file to finish loading, or paste PGN text.';
      pgn.focus();
    } else if (overCharacterLimit(pgn.value)) {
      event.preventDefault();
      status.textContent = characterLimitMessage();
      pgn.focus();
    }
  });
}

if (typeof document !== 'undefined') {
  if (document.readyState === 'loading')
    document.addEventListener('DOMContentLoaded', initImporter, { once: true });
  else initImporter();
}
