import { storage } from 'lib/storage';
import { throttle } from 'lib/async';
import { speakable } from 'lib/game/sanWriter';

type MoveSoundOpts = {
  name?: string;
  san?: string;
  volume?: number;
};
type AudioWindow = Window & { readonly webkitAudioContext?: typeof AudioContext };

const primerEvents = ['touchend', 'pointerup', 'pointerdown', 'mousedown', 'keydown'];

export default new (class {
  ctx = makeAudioContext();
  sounds = new Map<string, Sound>();
  paths = new Map<string, string>();
  theme = document.body.dataset.soundSet!;
  speechStorage = storage.boolean('speech.enabled');
  voiceStorage = storage.make('speech.voice');
  volumeStorage = storage.make('sound-volume');
  primer = () => {
    this.ctx?.resume().then(() => {
      setTimeout(() => $('#warn-no-autoplay').removeClass('shown'), 500);
    });
    for (const e of primerEvents) window.removeEventListener(e, this.primer, { capture: true });
  };

  constructor() {
    primerEvents.forEach(e => window.addEventListener(e, this.primer, { capture: true }));
    window.speechSynthesis?.getVoices(); // preload
  }

  async load(name: string, path?: string): Promise<Sound | undefined> {
    try {
      if (!this.ctx) return;
      if (path) this.paths.set(name, path);
      else path = this.paths.get(name) ?? this.resolvePath(name);
      if (!path) return;
      const cachedSound = this.sounds.get(path);
      if (cachedSound) return cachedSound;

      const result = await fetch(path);
      if (!result.ok) throw new Error(`${path} failed ${result.status}`);

      const arrayBuffer = await result.arrayBuffer();
      const audioBuffer = await new Promise<AudioBuffer>((resolve, reject) => {
        if (this.ctx?.decodeAudioData.length === 1)
          this.ctx?.decodeAudioData(arrayBuffer).then(resolve).catch(reject);
        else this.ctx?.decodeAudioData(arrayBuffer, resolve, reject);
      });
      const sound = new Sound(this.ctx, audioBuffer);
      this.sounds.set(path, sound);
      return sound;
    } catch (e) {
      console.warn('sound load failed', name, path, e);
      return;
    }
  }

  resolvePath(name: string): string | undefined {
    if (this.theme === 'silent') return;
    let dir = this.theme;
    if (this.speechStorage.get()) {
      if (['move', 'capture', 'check', 'checkmate'].includes(name)) return;
      dir = 'standard';
    }
    return this.url(`${dir}/${name[0].toUpperCase() + name.slice(1)}.mp3`);
  }

  url(name: string): string {
    return site.asset.url(`sound/${name}`);
  }

  async play(name: string, volume = 1): Promise<void> {
    if (this.theme === 'silent') return;
    const sound = await this.load(name);
    if (sound && (await this.resumeWithTest())) await sound.play(this.getVolume() * volume);
  }

  throttled = throttle(100, (name: string, volume: number) => this.play(name, volume));

  async move(o?: MoveSoundOpts) {
    const volume = o?.volume ?? 1;
    if (o?.name) this.throttled(o.name, volume);
    else {
      if (o?.san?.includes('x')) this.throttled('capture', volume);
      else this.throttled('move', volume);
      if (o?.san?.includes('#')) {
        this.throttled('checkmate', volume);
      } else if (o?.san?.includes('+')) {
        this.throttled('check', volume);
      }
    }
  }

  getVolume() {
    // garbage has been stored here by accident (e972d5612d)
    const v = parseFloat(this.volumeStorage.get() || '');
    return v >= 0 ? v : 0.7;
  }

  getVoice(): SpeechSynthesisVoice | undefined {
    let voicePreference = { name: '', lang: document.documentElement.lang.split('-')[0] };
    const storedVoice = this.voiceStorage.get();
    if (storedVoice) {
      try {
        voicePreference = JSON.parse(storedVoice);
      } catch {}
    }
    const voices = speechSynthesis.getVoices();
    const voiceMap = new Map<string, SpeechSynthesisVoice>();
    for (const code of ['en', document.documentElement.lang.split('-')[0], document.documentElement.lang]) {
      voices
        .filter(v => v.lang.startsWith(code))
        .sort((a, b) => a.lang.localeCompare(b.lang))
        .forEach(v => voiceMap.set(v.name, v));
    }
    return (
      voiceMap.get(voicePreference.name) ??
      [...voiceMap.values()].find(v => v.lang.startsWith(voicePreference.lang))
    );
  }

  sayLazy(text: () => string, cut = false, force = false) {
    if (typeof window.speechSynthesis === 'undefined') return false;
    try {
      if (cut) speechSynthesis.cancel();
      if (!this.speechStorage.get() && !force) return false;
      const msg = new SpeechSynthesisUtterance(text());
      const selectedVoice = this.getVoice();
      if (selectedVoice) {
        msg.voice = selectedVoice;
      } else {
        msg.lang = 'en-GB';
      }
      msg.volume = this.getVolume();
      window.speechSynthesis.speak(msg);
      return true;
    } catch (err) {
      console.error(err);
      return false;
    }
  }

  saySan = (san?: San, cut?: boolean, force?: boolean) => this.sayLazy(() => speakable(san), cut, force);

  preloadBoardSounds() {
    for (const name of ['move', 'capture', 'check', 'checkmate', 'genericNotify']) void this.load(name);
  }

  async resumeWithTest(): Promise<boolean> {
    if (!this.ctx) return false;
    if (this.ctx.state !== 'running' && this.ctx.state !== 'suspended') {
      // in addition to 'closed', iOS has 'interrupted'. who knows what else is out there
      if (this.ctx.state !== 'closed') this.ctx.close();
      this.ctx = makeAudioContext();
      if (this.ctx) {
        for (const s of this.sounds.values()) s.rewire(this.ctx);
      }
    }
    // if suspended, try audioContext.resume() with a timeout (sometimes it never resolves)
    if (this.ctx?.state === 'suspended')
      await new Promise<void>(resolve => {
        const resumeTimer = setTimeout(() => {
          $('#warn-no-autoplay').addClass('shown');
          resolve();
        }, 400);
        this.ctx?.resume().then(() => {
          clearTimeout(resumeTimer);
          resolve();
        });
      });
    if (this.ctx?.state !== 'running') return false;
    $('#warn-no-autoplay').removeClass('shown');
    return true;
  }
})();

class Sound {
  node: GainNode;
  ctx: AudioContext;

  constructor(
    ctx: AudioContext,
    readonly buffer: AudioBuffer,
  ) {
    this.rewire(ctx);
  }

  play(volume = 1): Promise<void> {
    this.node.gain.setValueAtTime(volume, this.ctx.currentTime);
    const source = this.ctx.createBufferSource();
    source.buffer = this.buffer;
    source.connect(this.node);
    return new Promise<void>(resolve => {
      source.onended = () => {
        source.disconnect();
        resolve();
      };
      source.start(0);
    });
  }
  rewire(ctx: AudioContext) {
    this.node?.disconnect();
    this.ctx = ctx;
    this.node = this.ctx.createGain();
    this.node.connect(this.ctx.destination);
  }
}

function makeAudioContext(): AudioContext | undefined {
  const win = window as AudioWindow;
  const AudioContextClass =
    win.webkitAudioContext ?? (typeof AudioContext !== 'undefined' ? AudioContext : undefined);
  return AudioContextClass ? new AudioContextClass({ latencyHint: 'interactive' }) : undefined;
}
