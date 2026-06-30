import cps from 'node:child_process';
import fs from 'node:fs';
import crypto from 'node:crypto';
import { join } from 'node:path';
import { env, c } from './env.ts';
import { jsLogger } from './console.ts';
import { taskOk } from './task.ts';
import { shallowSort, isContained } from './algo.ts';

const manifest = {
  js: {} as Manifest,
  css: {} as Manifest,
  hashed: {} as Manifest,
  dirty: false,
};
let writeTimer: NodeJS.Timeout;
let hydrated = false;

type SplitAsset = { hash?: string; path?: string; imports?: string[]; inline?: string; omit?: boolean };

export type Manifest = { [key: string]: SplitAsset };
export type ManifestUpdate = Partial<Omit<typeof manifest, 'dirty'>>;

export function stopManifest(clear = false): void {
  clearTimeout(writeTimer);
  if (clear) {
    manifest.js = {};
    manifest.css = {};
    manifest.hashed = {};
    manifest.dirty = false;
    hydrated = false;
  }
}

export function updateManifest(update: ManifestUpdate = {}, force = false): void {
  hydrateManifest();
  for (const [key, partial] of Object.entries(update) as [keyof ManifestUpdate, Manifest][]) {
    const full = manifest[key];
    if (isContained(full, partial)) continue;
    for (const [name, entry] of Object.entries(partial)) {
      full[name] = shallowSort({ ...full[name], ...entry });
    }
    manifest[key] = shallowSort(full);
    manifest.dirty = true;
  }
  if (force) manifest.dirty = true;
  if (manifest.dirty) {
    clearTimeout(writeTimer);
    writeTimer = setTimeout(writeManifest, env.watch && !env.startTime ? 750 : 0);
  }
}

function hydrateManifest(): void {
  if (hydrated) return;
  hydrated = true;
  const path = join(env.jsOutDir, 'manifest.json');
  try {
    const current = JSON.parse(fs.readFileSync(path, 'utf8')) as {
      js?: Record<string, SplitAsset | string>;
      css?: Record<string, SplitAsset | string>;
      hashed?: Record<string, SplitAsset | string>;
    };
    manifest.js = inflateManifest(current.js);
    manifest.css = inflateManifest(current.css);
    manifest.hashed = inflateManifest(current.hashed);
  } catch {
    // start from an empty manifest when there is no previous build output
  }
}

async function writeManifest() {
  if (!(env.manifestOk() && taskOk())) return;
  let commitMessage = 'Unknown Commit';
  let commitHash = 'local';
  try {
    commitMessage = cps
      .execSync('git log -1 --pretty=%s', { encoding: 'utf-8' })
      .trim()
      .replaceAll("'", '&#39;')
      .replaceAll('"', '&quot;');
    commitHash = cps.execSync('git rev-parse -q HEAD', { encoding: 'utf-8' }).trim();
  } catch {
    // Ignore git errors
  }

  const clientJs: string[] = [
    'if (!window.site) window.site={};',
    'const s=window.site;',
    's.info={};',
    `s.info.commit='${commitHash}';`,
    `s.info.message='${commitMessage}';`,
    `s.debug=${env.debug};`,
    's.asset={loadEsm:(m,o)=>import(`/assets/compiled/${m}${s.manifest.js[m]?"."+s.manifest.js[m]:""}.js`)' +
      '.then(x=>(x.initModule||x.default)(o.init)),' +
      'loadEsmPage:(m,o)=>s.asset.loadEsm(m,o)};',
    // a light version of loadEsm for embeds. on pages this will be overwritten by site.js
  ];
  if (env.remoteLog) clientJs.push(await jsLogger());

  const pairLine = ([name, info]: [string, SplitAsset]) =>
    `'${name.replaceAll('\\', '/').replaceAll("'", "\\'")}':'${info.hash}'`;

  const jsLines = Object.entries(manifest.js)
    .filter(([name, _]) => !/lib\.[A-Z0-9]{8}/.test(name))
    .map(pairLine)
    .join(',');
  const cssLines = Object.entries(manifest.css).map(pairLine).join(',');
  const hashedLines = Object.entries(manifest.hashed)
    .filter(([, { omit }]) => !omit)
    .map(([name, { hash }]) => pairLine([name, { hash }]))
    .join(',');
  clientJs.push(`s.manifest={\ncss:{${cssLines}},\njs:{${jsLines}},\nhashed:{${hashedLines}}\n};`);

  const hashable = clientJs.join('\n');
  const hash = crypto.createHash('sha256').update(hashable).digest('hex').slice(0, 8);

  const clientManifest = hashable + `\ns.info.date='${new Date().toISOString().split('.')[0] + '+00:00'}';\n`;
  const serverManifest = JSON.stringify(
    {
      js: compactManifest({ ...manifest.js, manifest: { hash } }),
      css: compactManifest(manifest.css),
      hashed: compactManifest(manifest.hashed),
    },
    null,
    env.prod ? undefined : 2,
  );
  await Promise.all([
    fs.promises.writeFile(join(env.jsOutDir, `manifest.${hash}.js`), clientManifest),
    fs.promises.writeFile(join(env.jsOutDir, `manifest.json`), serverManifest),
  ]);
  manifest.dirty = false;
  const serverHash = crypto.createHash('sha256').update(serverManifest).digest('hex').slice(0, 8);
  env.log(
    `'${c.cyan(`public/compiled/manifest.${hash}.js`)}', '${c.cyan(`public/compiled/manifest.json`)}' ${c.grey(serverHash)}`,
    'manifest',
  );
}

function compactManifest(manifest: Manifest): Record<string, SplitAsset | string> {
  const compacted: Record<string, SplitAsset | string> = {};
  for (const [key, info] of Object.entries(manifest)) {
    const infoKeys = Object.keys(info);
    if (infoKeys.length === 1 && infoKeys[0] === 'hash') compacted[key] = info.hash!;
    else compacted[key] = info;
  }
  return compacted;
}

function inflateManifest(source: Record<string, SplitAsset | string> | undefined): Manifest {
  if (!source) return {};
  return Object.fromEntries(
    Object.entries(source).map(([key, info]) => [key, typeof info === 'string' ? { hash: info } : info]),
  );
}
