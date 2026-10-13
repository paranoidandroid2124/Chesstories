import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const result = spawnSync('sbt', ['writeRuntimeClasspath'], {
  cwd: fileURLToPath(new URL('../../chesstory-runtime/', import.meta.url)),
  stdio: 'inherit',
  shell: process.platform === 'win32',
  windowsHide: true,
});
if (result.error) console.error(result.error.message);
process.exit(result.status ?? 1);
