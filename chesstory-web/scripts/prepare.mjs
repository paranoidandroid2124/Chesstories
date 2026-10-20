import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';

const stampName = '.chesstories-prepared.json';

function files(path) {
  if (!statSync(path).isDirectory()) return [path];
  return readdirSync(path).sort()
    .filter(name => !['target', '.git', '.bsp', '.metals', stampName].includes(name))
    .flatMap(name => files(join(path, name)));
}

function inputHash(root, inputs, version) {
  const hash = createHash('sha256').update(`${root}\0${version}\0`);
  for (const input of inputs) for (const file of files(resolve(root, input)))
    hash.update(file).update('\0').update(readFileSync(file)).update('\0');
  return hash.digest('hex');
}

function outputState(root, outputs) {
  try {
    return outputs().flatMap(output => files(resolve(root, output))).map(file => {
      const info = statSync(file);
      return [file, info.size, info.mtimeMs];
    });
  } catch (error) {
    if (error.code === 'ENOENT') return null;
    throw error;
  }
}

// Invalidate before building: a failed or interrupted update must never launch an older build.
export function prepare({ root, label, inputs, outputs, directory, version = '', build }) {
  const stamp = resolve(root, directory, stampName);
  const input = inputHash(root, inputs, version);
  let saved;
  try { saved = JSON.parse(readFileSync(stamp, 'utf8')); }
  catch (error) { if (error.code !== 'ENOENT' && !(error instanceof SyntaxError)) throw error; }
  const current = outputState(root, outputs);
  if (saved?.input === input && current?.length && JSON.stringify(saved.outputs) === JSON.stringify(current)) {
    console.log(`${label}: up to date.`);
    return false;
  }
  console.log(`${label}: preparing…`);
  rmSync(stamp, { force: true });
  build();
  if (inputHash(root, inputs, version) !== input) throw Error(`${label}: sources changed during preparation. Start again.`);
  const built = outputState(root, outputs);
  if (!built?.length) throw Error(`${label}: required output is missing.`);
  mkdirSync(dirname(stamp), { recursive: true });
  writeFileSync(stamp, JSON.stringify({ input, outputs: built }));
  return true;
}
