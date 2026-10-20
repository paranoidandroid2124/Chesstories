import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { createServer } from 'node:net';
import { delimiter, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { prepare } from './prepare.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const runtime = resolve(root, '../chesstory-runtime');
const nodeMajor = Number(process.versions.node.split('.')[0]);
const port = Number(process.env.CHESSTORIES_PORT ?? 8787);
// A browser refresh during preparation must not keep the reserved port open.
const reservation = createServer(socket => socket.destroy());

function run(command, args, shell = false) {
  const result = spawnSync(command, args, { cwd: root, stdio: 'inherit', windowsHide: true, shell });
  if (result.error) throw result.error;
  if (result.status !== 0) throw Error(`Preparation failed (${result.status ?? result.signal}). Fix the error above and start again.`);
}

try {
  if (nodeMajor < 22) throw Error('Node.js 22 or newer is required.');
  if (!Number.isInteger(port) || port < 1 || port > 65535) throw Error('Invalid CHESSTORIES_PORT');
  await new Promise((accept, reject) => reservation.once('error', reject).listen(port, '127.0.0.1', accept));
  const java = spawnSync(process.env.CHESSTORIES_JAVA ?? 'java', ['-version'], { encoding: 'utf8', windowsHide: true });
  const javaVersion = `${java.stderr ?? ''}\n${java.stdout ?? ''}`.match(/version "(\d+)[^"]*"/);
  if (java.error || java.status !== 0 || !javaVersion || Number(javaVersion[1]) < 21)
    throw Error('Java 21 or newer is required. Check PATH or CHESSTORIES_JAVA.');

  const dependencies = JSON.parse(readFileSync(resolve(root, 'package.json'), 'utf8'));
  prepare({ root, label: 'Packages', inputs: ['package.json', 'package-lock.json'], version: String(nodeMajor),
    directory: 'node_modules', outputs: () => ['node_modules/.package-lock.json',
      ...Object.keys({ ...dependencies.dependencies, ...dependencies.devDependencies }).map(name => `node_modules/${name}/package.json`)],
    build: () => run('npm', ['ci', '--include=dev', '--no-audit', '--no-fund'], process.platform === 'win32') });
  prepare({ root, label: 'Commentary', inputs: ['../chesstory-runtime/src/main', '../chesstory-runtime/build.sbt',
      '../chesstory-runtime/project', 'scripts/build-runtime.mjs'], version: javaVersion[1],
    directory: '../chesstory-runtime/target', outputs: () => {
      const classpath = resolve(runtime, 'target/runtime-classpath.txt');
      return [classpath, ...readFileSync(classpath, 'utf8').trim().split(delimiter)];
    }, build: () => run(process.execPath, ['scripts/build-runtime.mjs']) });
  prepare({ root, label: 'Screen and engine', inputs: ['src', 'index.html', 'scripts/build.mjs',
      'package.json', 'package-lock.json', 'PROVENANCE.md', '../LICENSE'], version: String(nodeMajor),
    directory: 'dist', outputs: () => ['dist'], build: () => run(process.execPath, ['scripts/build.mjs']) });
  await new Promise(accept => reservation.close(accept));
  await import('./serve.mjs');
} catch (error) {
  reservation.close();
  console.error(error.code === 'EADDRINUSE'
    ? `Port ${port} is already in use. Stop the running app with Ctrl+C, then start again to apply changes.`
    : error.message);
  process.exitCode = 1;
}
