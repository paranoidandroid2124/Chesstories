import { patch } from './view/util';
import makeBoot from './boot';
import makeStart from './start';

export { patch };

const start = makeStart(patch);
const boot = makeBoot(start);

export async function initModule({ mode, cfg }: { mode: 'userAnalysis' | 'replay'; cfg: any }) {
  try {
    await site.asset.loadPieces;
  } catch (e) {
    console.warn('loadPieces failed', e);
  }
  if (mode === 'replay') boot(cfg);
  else userAnalysis(cfg);
}

function userAnalysis(cfg: any) {
  const useExplorerProxy = document.body.dataset.brandExplorerProxy !== '0';
  if (useExplorerProxy && cfg?.explorer) cfg.explorer.endpoint = `${location.origin}/api/explorer`;
  start(cfg);
}
