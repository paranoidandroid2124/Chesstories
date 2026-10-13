import { build } from 'esbuild';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { rm } from 'node:fs/promises';

process.chdir(fileURLToPath(new URL('../', import.meta.url)));
const requested = process.argv.slice(2);
const names = requested.length ? requested : ['core', 'referencePurpose', 'preview'];
if (names.some(name => !/^(regression\/)?[A-Za-z0-9]+$/.test(name)))
  throw Error('Use a test name, for example: npm test -- regression/moveReviewProtocol');
const outfiles = names.map(name => `test-dist/${name}.test.mjs`);
try {
  await build({ entryPoints: names.map(name => `tests/${name}.test.ts`), outbase: 'tests',
    outdir: 'test-dist', outExtension: { '.js': '.mjs' }, bundle: true, platform: 'node', format: 'esm', target: 'node22' });
  process.exitCode = spawnSync(process.execPath, ['--test', ...outfiles], { stdio: 'inherit' }).status ?? 1;
} finally { await Promise.all(outfiles.map(file => rm(file, { force: true }))); }
