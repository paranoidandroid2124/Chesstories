import type { AnalyseOpts } from './interfaces';

export default function (start: (opts: AnalyseOpts) => void) {
  return function (cfg: AnalyseOpts) {
    start(cfg);
  };
}
