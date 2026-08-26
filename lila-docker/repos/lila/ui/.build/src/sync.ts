import fs from 'node:fs';
import { join, dirname } from 'node:path';
import { makeTask } from './task.ts';
import { env, c } from './env.ts';
import { glob, isGlob, isFolder, isClose } from './parse.ts';
import { isEquivalent } from './algo.ts';

export async function sync(): Promise<any> {
  if (!env.begin('sync')) return;
  return Promise.all(
    [...env.tasks('sync')].map(async ([pkg, sync]) => {
      const resolvedSrc = await resolveSyncSrc(env.rootDir, sync.src);
      const { root } = await srcRoot(env.rootDir, resolvedSrc);
      await makeTask({
        includes: { path: resolvedSrc, cwd: env.rootDir },
        ctx: 'sync',
        always: true,
        debounce: 300,
        execute: (files, fullList) => {
          if (fullList.length === 0) throw `Not found '${c.cyan(resolvedSrc)}'`;
          const logEvery = !isEquivalent(files, fullList);
          if (!logEvery)
            env.log(`${c.grey(pkg.name)} '${c.cyan(resolvedSrc)}' -> '${c.cyan(sync.dest)}'`, 'sync');
          return Promise.all(
            files.map(async f => {
              if ((await syncOne(f, join(env.rootDir, sync.dest, f.slice(root.length)))) && logEvery)
                env.log(
                  `${c.grey(pkg.name)} '${c.cyan(f.slice(root.length))}' -> '${c.cyan(sync.dest)}'`,
                  'sync',
                );
            }),
          );
        },
      });
    }),
  );
}

export async function resolveSyncSrc(cwd: string, src: string): Promise<string> {
  // pnpm can be configured with `node-linker=hoisted`, which removes per-package `ui/<pkg>/node_modules`.
  // In that case, sync sources under `ui/<pkg>/node_modules/...` should fall back to repo `node_modules/...`.
  if (!src.startsWith('ui/') || !src.includes('/node_modules/')) return src;

  const parts = src.split('/');
  const fallback = parts.length >= 3 && parts[0] === 'ui' ? parts.slice(2).join('/') : src;

  if (await syncSourceExists(cwd, src)) return src;
  if (await syncSourceExists(cwd, fallback)) return fallback;
  return src;
}

async function syncSourceExists(cwd: string, src: string): Promise<boolean> {
  if (isGlob(src)) return (await glob(src, { cwd })).length > 0;
  return fs.promises.access(join(cwd, src)).then(
    () => true,
    () => false,
  );
}

async function syncOne(absSrc: string, absDest: string): Promise<boolean> {
  // TODO are these stats unnecessary now?
  const [src, dest] = (
    await Promise.allSettled([
      fs.promises.stat(absSrc),
      fs.promises.stat(absDest),
      fs.promises.mkdir(dirname(absDest), { recursive: true }),
    ])
  ).map(x => (x.status === 'fulfilled' ? (x.value as fs.Stats) : undefined));
  if (src && !(dest && isClose(src.mtimeMs, dest.mtimeMs))) {
    await fs.promises.copyFile(absSrc, absDest);
    await fs.promises.utimes(absDest, src.atime, src.mtime);
    return true;
  }
  return false;
}

async function srcRoot(cwd: string, path: string): Promise<{ root: string; exact: boolean }> {
  if (!isGlob(path) && !(await isFolder(join(cwd, path))))
    return { root: join(cwd, dirname(path)), exact: true };
  const [head, ...tail] = path.split('/');
  if (isGlob(head)) return { root: cwd, exact: false };
  return srcRoot(join(cwd, head), tail.join('/'));
}
