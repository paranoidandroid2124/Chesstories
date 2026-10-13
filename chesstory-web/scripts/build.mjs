import { build } from 'esbuild';
import { compileString } from 'sass';
import { readFile, writeFile, mkdir, copyFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';

const root = fileURLToPath(new URL('../', import.meta.url));
process.chdir(root);
await mkdir('dist/engine', { recursive: true });
const bundled = await build({
  entryPoints: ['src/main.ts'], outfile: 'dist/app.js', bundle: true,
  format: 'esm', target: 'es2022', sourcemap: true, metafile: true,
});
// Prevent a research cache or the old server from quietly becoming a build dependency.
for (const file of Object.keys(bundled.metafile.inputs)) {
  if (file.includes('lila-docker') || file.includes('target/')) throw Error(`External source dependency: ${file}`);
}
await writeFile('dist/build-inputs.json', JSON.stringify(Object.keys(bundled.metafile.inputs), null, 2));
const panel = await readFile('src/review/_move-review.scss', 'utf8');
const colors = {
  '$c-bg-box': 'var(--paper)', '$c-font': 'var(--ink)', '$c-font-dim': 'var(--muted)',
  '$c-font-clear': 'var(--ink)', '$c-bg-zebra': 'var(--subtle)', '$c-link': 'var(--link)',
  '$c-primary': 'var(--link)', '$border': '1px solid var(--rule)', '$c-bad': '#b44042',
  '$c-good': '#36724f', '$c-accent': 'var(--accent)',
  '$c-border': 'var(--rule)', '$c-bg-low': 'var(--subtle)', '$c-font-dimmer': 'var(--muted)',
  '$c-secondary': 'var(--accent)', '$m-primary--fade-80': 'var(--subtle)',
};
const variables = [...new Set(panel.match(/\$[\w-]+/g))];
for (const name of variables) if (!(name in colors)) throw Error(`Missing panel color: ${name}`);
const prelude = variables.map(name => `${name}: ${colors[name]};`).join('\n') + `
@mixin mq-at-least-col3 { @media(min-width:1300px) { @content; } }
@mixin mq-is-col1 { @media(max-width:799px) { @content; } }
%square { position:relative; aspect-ratio:1; }
%abs-100 { position:absolute; inset:0; width:100%; height:100%; }
%button-none { border:0; background:none; padding:0; cursor:pointer; }
`;
const css = compileString(prelude + panel, { style: 'compressed' }).css;
const board = await readFile('node_modules/@lichess-org/chessground/assets/chessground.base.css', 'utf8');
const pieces = await readFile('node_modules/@lichess-org/chessground/assets/chessground.cburnett.css', 'utf8');
await writeFile('dist/app.css', board + '\n' + pieces + '\n' + css + '\n' + await readFile('src/style.css', 'utf8'));
await copyFile('index.html', 'dist/index.html');
await copyFile('src/engine/worker.js', 'dist/engine-worker.js');
await mkdir('dist/licenses', { recursive: true });
for (const [source, name] of [
  ['../LICENSE', 'Chesstories-AGPL-3.0.txt'],
  ['node_modules/@lichess-org/chessground/LICENSE', 'chessground.txt'],
  ['node_modules/@lichess-org/stockfish-web/LICENSE', 'stockfish-web.txt'],
  ['node_modules/chessops/LICENSE.txt', 'chessops.txt'],
  ['node_modules/snabbdom/LICENSE', 'snabbdom.txt'],
  ['node_modules/@lichess-org/stockfish-web/README.md', 'stockfish-sources.md'],
  ['PROVENANCE.md', 'provenance.md'],
]) await copyFile(source, `dist/licenses/${name}`);
for (const file of ['sf_18_smallnet.js', 'sf_18_smallnet.wasm'])
  await copyFile(`node_modules/@lichess-org/stockfish-web/${file}`, `dist/engine/${file}`);

const nnue = 'nn-4ca89e4b3abf.nnue';
const expected = '4ca89e4b3abfbe9df13e4f3db2acb64dc6ddc7a9becb2ac1cf388f4d66b3bd94';
const asset = resolve('assets/engine', nnue);
await mkdir('assets/engine', { recursive: true });
let bytes;
try { bytes = await readFile(asset); } catch (error) { if (error.code !== 'ENOENT') throw error; }
if (!bytes) {
  console.log('Downloading the pinned Stockfish network (once)…');
  const response = await fetch(`https://tests.stockfishchess.org/api/nn/${nnue}`, { signal: AbortSignal.timeout(60000) });
  if (!response.ok) throw Error(`Stockfish network download failed: ${response.status}`);
  bytes = Buffer.from(await response.arrayBuffer());
}
if (createHash('sha256').update(bytes).digest('hex') !== expected) throw Error('Stockfish network checksum mismatch');
await writeFile(asset, bytes);
await copyFile(asset, `dist/engine/${nnue}`);
console.log(`Built standalone app: ${Object.keys(bundled.metafile.inputs).length} source inputs, no Lila or research-cache inputs.`);
