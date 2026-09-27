/* eslint no-restricted-syntax:"error" */ // no side effects allowed due to re-export by index.ts

import {
  type VNode,
  type VNodeData,
  type VNodeChildElement,
  type Hooks,
  type Attrs,
  type Classes,
  h as snabH,
} from 'snabbdom';
import { toDataIcon } from '../licon';
import { icons } from '../icons';

export type { Attrs, Hooks, Classes, VNode, VNodeData, VNodeChildElement };
export type MaybeVNode = VNode | string | null | undefined;
export type MaybeVNodes = MaybeVNode[];

export function onInsert<A extends HTMLElement>(f: (element: A) => void): Hooks {
  return {
    insert: vnode => f(vnode.elm as A),
  };
}

export function bind<K extends keyof GlobalEventHandlersEventMap>(
  eventName: K,
  f: (ev: GlobalEventHandlersEventMap[K]) => any,
  redraw?: Redraw,
  passive = true,
): Hooks {
  return onInsert(el =>
    el.addEventListener(
      eventName,
      e => {
        const res = f(e);
        if (res === false && !passive) e.preventDefault();
        redraw?.();
        return res;
      },
      { passive },
    ),
  );
}

export const dataIcon = (icon: string): Attrs => ({
  'data-icon': toDataIcon(icon),
});

const normalizeDataIconAttr = (data?: VNodeData): VNodeData | null => {
  if (!data) return null;
  const attrs = data.attrs;
  if (!attrs) return data;
  const icon = attrs['data-icon'];
  if (typeof icon !== 'string') return data;
  const normalized = toDataIcon(icon);
  return normalized === icon ? data : { ...data, attrs: { ...attrs, 'data-icon': normalized } };
};

export function icon(name: keyof typeof icons, className?: string): VNode {
  const klass: Classes = { [`story-icon`]: true, [`story-icon-${name}`]: true };
  if (className) klass[className] = true;
  return snabH(
    'svg',
    {
      class: klass,
      attrs: {
        viewBox: '0 0 24 24',
        width: '20',
        height: '20',
        fill: 'none',
        stroke: 'currentColor',
        'stroke-width': '2',
        'stroke-linecap': 'round',
        'stroke-linejoin': 'round',
        'aria-hidden': 'true',
        focusable: 'false',
      },
    },
    [snabH('path', { attrs: { d: icons[name] } })],
  );
}

export type LooseVNode = VNodeChildElement | boolean;
export type LooseVNodes = LooseVNode | LooseVNodes[];

// '' may be falsy but it's a valid VNode
// 0 may be falsy but it's a valid VNode
const kidFilter = (x: VNodeData | LooseVNodes): boolean => (x && x !== true) || x === '' || x === 0;

const filterKids = (children: LooseVNodes): VNodeChildElement[] => {
  const flatKids: LooseVNode[] = [];
  flattenKids(children, flatKids);
  return flatKids.filter(kidFilter) as VNodeChildElement[];
};

// strip boolean results and flatten arrays in renders.  Allows
//   hl('div', isDivEmpty || [ 'foo', fooHasBar && [ 'has', 'bar' ])
export function hl(sel: string, dataOrKids?: VNodeData | LooseVNodes, kids?: LooseVNodes): VNode {
  if (kids) return snabH(sel, normalizeDataIconAttr(dataOrKids as VNodeData), filterKids(kids));
  if (!kidFilter(dataOrKids)) return snabH(sel);
  if (Array.isArray(dataOrKids) || (typeof dataOrKids === 'object' && 'sel' in dataOrKids!))
    return snabH(sel, filterKids(dataOrKids as LooseVNodes));
  else return snabH(sel, normalizeDataIconAttr(dataOrKids as VNodeData));
}

// for deep trees i think it's more efficient to flatten arrays here than to spread them in renders.
// but we're mostly after cleaner syntax
const flattenKids = (maybeArray: LooseVNodes, out: LooseVNode[]) => {
  if (Array.isArray(maybeArray)) for (const el of maybeArray) flattenKids(el, out);
  else out.push(maybeArray);
};
