import type AnalyseCtrl from './ctrl';
import * as treeOps from 'lib/tree/ops';

export type DiscloseState = undefined | 'expanded' | 'collapsed';

export class IdbTree {
  constructor(private ctrl: AnalyseCtrl) {}

  someCollapsedOf(collapsed: boolean, path = ''): boolean {
    return (
      this.ctrl.disclosureMode() &&
      this.ctrl.tree.walkUntilTrue(
        (n, m) => this.isCollapsible(n, m) && collapsed === Boolean(n.collapsed),
        path,
        path !== '',
      )
    );
  }

  stepLine(fromPath: Tree.Path = this.ctrl.path, which: 'prev' | 'next' = 'next'): Tree.Path {
    let [path, kids] = this.familyOf(fromPath);
    while (path && kids.length < 2 && !this.ctrl.tree.pathIsMainline(path)) {
      [path, kids] = this.familyOf(path);
    }
    const i = kids.findIndex(k => fromPath.slice(path.length).startsWith(k.id));
    const stepTo = which === 'next' ? (kids[i + 1] ?? kids[0]) : (kids[i - 1] ?? kids[kids.length - 1]);
    return !stepTo ? fromPath : path + stepTo.id;
  }

  setCollapsed(path: Tree.Path, collapsed: boolean): void {
    this.ctrl.tree.updateAt(path, n => (n.collapsed = collapsed));
    this.ctrl.redraw();
  }

  setCollapsedFrom(from: Tree.Path, collapsed: boolean, thisBranchOnly = false): void {
    this.ctrl.tree.walkUntilTrue(
      (n, m) => {
        if (this.isCollapsible(n, m)) n.collapsed = collapsed;
        return false;
      },
      from,
      thisBranchOnly,
    );
    this.ctrl.redraw();
  }

  revealNode(path?: string): void {
    let save = false;
    const nodes = path === undefined ? this.ctrl.nodeList : this.ctrl.tree.getNodeList(path);
    for (let i = 0; i < nodes.length; i++) {
      const kid = nodes[i].children[0];
      if (nodes[i].collapsed && kid && nodes[i + 1] && kid !== nodes[i + 1]) {
        nodes[i].collapsed = false;
        save = true;
      }
    }
    if (save) this.ctrl.redraw();
  }

  discloseOf(node: Tree.Node | undefined, isMainline: boolean): DiscloseState {
    if (!node) return undefined;
    return this.isCollapsible(node, isMainline)
      ? this.ctrl.disclosureMode() && node.collapsed
        ? 'collapsed'
        : 'expanded'
      : undefined;
  }

  async merge(): Promise<void> {
    this.collapseDefault();
  }

  private isCollapsible(node: Tree.Node, isMainline: boolean): boolean {
    if (!node) return false;
    const [first, second, third] = node.children.filter(n => !n.comp);
    return Boolean(
      first?.forceVariation ||
        third ||
        (second && treeOps.hasBranching(second, 6)) ||
        (isMainline &&
          this.ctrl.treeView.mode === 'column' &&
          (second || first?.comments?.filter(Boolean).length)),
    );
  }

  private collapseDefault() {
    const depthThreshold = 1;

    const traverse = (node: Tree.Node, depth: number) => {
      if (depth === depthThreshold && this.isCollapsible(node, false)) {
        node.collapsed = true;
      }
      node.children.forEach((n, i) => traverse(n, depth + (i === 0 ? 0 : 1)));
    };
    traverse(this.ctrl.tree.root, 0);
  }

  private familyOf(path: Tree.Path): [Tree.Path, Tree.Node[]] {
    const parentPath = path.slice(0, -2);
    return [parentPath, this.ctrl.tree.nodeAtPath(parentPath).children.filter(x => !x.comp)];
  }
}
