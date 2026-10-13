import createStockfish from './engine/sf_18_smallnet.js';

const queue = [];
let engine;
onmessage = event => engine ? engine.uci(event.data) : queue.push(event.data);
try {
  const module = await createStockfish();
  for (let index = 0; ; index++) {
    const name = module.getRecommendedNnue(index);
    if (!name) break;
    if (!/^nn-[a-f0-9]{12}\.nnue$/.test(name)) throw Error('Unexpected engine network');
    const response = await fetch(`/engine/${name}`);
    if (!response.ok) throw Error('Engine network unavailable');
    const bytes = await response.arrayBuffer();
    const hash = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', bytes)), b => b.toString(16).padStart(2, '0')).join('');
    if (!hash.startsWith(name.slice(3, 15))) throw Error('Engine network checksum mismatch');
    module.setNnueBuffer(new Uint8Array(bytes), index);
  }
  module.listen = line => postMessage(line);
  engine = module;
  queue.forEach(command => engine.uci(command));
} catch (error) {
  postMessage({ error: error instanceof Error ? error.message : String(error) });
  close();
}
