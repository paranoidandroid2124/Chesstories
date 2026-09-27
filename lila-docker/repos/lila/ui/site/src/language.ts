/// <reference lib="es2022.intl" />
import { korean } from './language.ko';

const textSources = new WeakMap<Text, { source: string; rendered: string }>();
const attributeSources = new WeakMap<Element, Map<string, { source: string; rendered: string }>>();
const excluded = 'script,style,code,pre,textarea,[contenteditable],[data-no-translate],.tview2,.comment,#user_tag,.user-link,.study__name,.study__desc';

function translate(source: string): string {
  if (document.documentElement.lang !== 'ko') return source;
  const key = source.trim().replace(/\s+/g, ' ');
  const translation = korean[key];
  if (translation) return source.replace(source.trim(), translation);
  const sentences = Array.from(new Intl.Segmenter('en', { granularity: 'sentence' }).segment(key), s => s.segment.trim());
  if (sentences.length > 1 && sentences.every(sentence => korean[sentence]))
    return source.replace(source.trim(), sentences.map(sentence => korean[sentence]).join(' '));
  const dynamic = key.replace(/^(.+?) ([•-]) Chesstory$/, (_, title: string, separator: string) => `${translate(title)} ${separator} Chesstory`)
    .replace(/^(\d+) matching questions$/, '검색된 질문 $1개')
    .replace(/^(\d+) matching questions for “(.+)”$/, '“$2” 검색 결과 · $1개')
    .replace(/^Results for “(.+)”$/, '“$1” 검색 결과')
    .replace(/^(\d+) moves?$/, '$1수')
    .replace(/^PGN must be ([\d,]+) characters or fewer(?: before opening the board)?\.$/, 'PGN은 $1자까지 입력할 수 있습니다.')
    .replace(/^Choose a PGN file no larger than ([\d,]+) bytes before reading\.$/, '$1바이트 이하의 PGN 파일을 선택해 주세요.')
    .replace(/^The loaded file exceeds the ([\d,]+)-character PGN limit\.$/, '선택한 파일이 PGN $1자 제한을 넘었습니다.')
    .replace(/^Reading (.+)\.\.\.$/, '$1 읽는 중…')
    .replace(/^Loaded (.+)\. Review the PGN, then open the board\.$/, '$1 파일을 읽었습니다. 기보를 확인한 뒤 대국을 열어 주세요.')
    .replace(/^Signed in as (.+)$/, '$1 계정으로 로그인됨')
    .replace(/^Creating the study and carrying over (\d+) saved lines?\.$/, '스터디를 만들고 저장한 수순 $1개를 옮기는 중입니다.')
    .replace(/^Check the PGN: (.+)$/, (_, message: string) => '기보 확인: ' + translate(message))
    .replace(/^Can't play (.+) at move (\d+), ply (\d+)$/, '$2번째 수에서 $1을 둘 수 없습니다 (반수 $3).');
  return source.replace(source.trim(), dynamic);
}

function translateNode(node: Node): void {
  if (node instanceof Text) {
    if (!node.parentElement || node.parentElement.closest(excluded)) return;
    const previous = textSources.get(node);
    const source = previous?.rendered === node.data ? previous.source : node.data;
    const rendered = translate(source);
    textSources.set(node, { source, rendered });
    if (node.data !== rendered) node.data = rendered;
    return;
  }
  if (!(node instanceof Element) || node.matches(excluded.replace('textarea,', ''))) return;
  const sources = attributeSources.get(node) ?? new Map();
  for (const key of ['aria-label', 'title', 'placeholder', 'alt']) {
    const current = node.getAttribute(key);
    if (current === null) continue;
    const previous = sources.get(key);
    const source = previous?.rendered === current ? previous.source : current;
    const rendered = translate(source);
    sources.set(key, { source, rendered });
    if (rendered !== current) node.setAttribute(key, rendered);
  }
  attributeSources.set(node, sources);
  if (node.matches('textarea')) return;
  for (const child of node.childNodes) translateNode(child);
}

export function initLanguage(): void {
  try {
    const saved = localStorage.getItem('chesstory.language');
    if (saved === 'ko' || saved === 'en') document.documentElement.lang = saved;
  } catch { /* The page still supports an in-memory language choice. */ }
  const selector = document.querySelector<HTMLSelectElement>('.js-language-choice');
  const current = document.querySelector<HTMLElement>('.site-language-current');
  if (current) current.textContent = document.documentElement.lang === 'ko' ? '한국어' : 'EN';
  if (selector) {
    selector.value = document.documentElement.lang === 'ko' ? 'ko' : 'en';
    selector.addEventListener('change', () => {
      const language = selector.value;
      if (language !== 'ko' && language !== 'en') return;
      document.documentElement.lang = language;
      if (current) current.textContent = language === 'ko' ? '한국어' : 'EN';
      try { localStorage.setItem('chesstory.language', language); } catch { /* Storage is optional. */ }
      translateNode(document.body);
      const title = document.querySelector('title');
      if (title) translateNode(title);
      window.dispatchEvent(new Event('chesstory:language'));
    });
  }
  translateNode(document.body);
  const title = document.querySelector('title');
  if (title) translateNode(title);
  new MutationObserver(records => {
    for (const record of records) {
      if (record.type === 'childList') for (const node of record.addedNodes) translateNode(node);
      else translateNode(record.target);
    }
  }).observe(document.body, { childList: true, subtree: true, characterData: true, attributes: true,
    attributeFilter: ['aria-label', 'title', 'placeholder', 'alt'] });
}
