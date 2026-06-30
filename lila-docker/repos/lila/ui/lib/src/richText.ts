/* eslint no-restricted-syntax:"error" */ // no side effects allowed due to re-export by index.ts

import type { VNode, Hooks } from 'snabbdom';
import { escapeHtml } from './index';

// from https://github.com/bryanwoods/autolink-js/blob/master/autolink.js
const escapeRegex = (s: string): string => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
let linkRegex: RegExp | undefined;

function internalHostPattern(): string {
  const hosts = new Set<string>(['chesstory.com']);
  if (typeof location !== 'undefined') {
    hosts.add(location.host);
    hosts.add(location.hostname);
  }
  return Array.from(hosts).filter(Boolean).map(escapeRegex).join('|');
}

function getLinkRegex(): RegExp {
  return (linkRegex ??= new RegExp(
    `(^|[\\s\\n]|<[A-Za-z]*\\/?>)((?:(?:https?|ftp):\\/\\/|(?:${internalHostPattern()}))[\\-A-Z0-9+\\u0026\\u2019@#\\/%?=()~_|!:,.;]*[\\-A-Z0-9+\\u0026@#\\/%=~()_|])`,
    'gi',
  ));
}

const newLineRegex: RegExp = /\n/g;

const linkHtml = (href: string, content: string, expandable: boolean = true): string =>
  `<a${expandable ? '' : ' class="text"'} target="_blank" rel="nofollow noreferrer" href="${href}">${content}</a>`;

function toLink(url: string): string {
  if (!url.match(/^[A-Za-z]+:\/\//)) url = 'https://' + url;
  return linkHtml(url, url.replace(/https?:\/\//, ''));
}

const autolink = (str: string, callback: (str: string) => string): string =>
  str.replace(getLinkRegex(), (_, space, url) => space + callback(url));

export const innerHTML = <A>(a: A, toHtml: (a: A) => string): Hooks => ({
  insert(vnode: VNode) {
    (vnode.elm as HTMLElement).innerHTML = toHtml(a);
    vnode.data!.cachedA = a;
  },
  postpatch(old: VNode, vnode: VNode) {
    if (old.data!.cachedA !== a) (vnode.elm as HTMLElement).innerHTML = toHtml(a);
    vnode.data!.cachedA = a;
  },
});

export function enrichText(text: string, allowNewlines = true): string {
  let html = autolink(escapeHtml(text), toLink);
  html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');
  if (allowNewlines) html = html.replace(newLineRegex, '<br>');
  return html;
}
