/** In-memory B+ tree. Keys stay inside nodes the client encrypts before upload. */

export interface IndexEntry {
  k: string | null;
  id: string;
}

export type SortNode =
  | { t: "l"; e: IndexEntry[] }
  | { t: "i"; s: IndexEntry[]; c: string[]; n: number[] };

export interface SyncStore {
  root: string | null;
  get(id: string): SortNode;
  set(id: string, node: SortNode): void;
  create(node: SortNode): string;
}

export function compareSortKey(a: string | null, b: string | null): number {
  if (a === b) return 0;
  if (a === null) return 1;
  if (b === null) return -1;
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

export function compareEntries(a: IndexEntry, b: IndexEntry): number {
  const byKey = compareSortKey(a.k, b.k);
  if (byKey !== 0) return byKey;
  if (a.id < b.id) return -1;
  if (a.id > b.id) return 1;
  return 0;
}

export function countNode(store: SyncStore, id: string): number {
  const node = store.get(id);
  if (node.t === "l") return node.e.length;
  return node.n.reduce((sum, count) => sum + count, 0);
}

export function insertEntry(store: SyncStore, entry: IndexEntry, capacity: number): void {
  if (!store.root) {
    store.root = store.create({ t: "l", e: [entry] });
    return;
  }

  const split = insertInto(store, store.root, entry, capacity);
  if (!split.rightId || !split.separator) return;

  const leftId = store.root;
  store.root = store.create({
    t: "i",
    s: [split.separator],
    c: [leftId, split.rightId],
    n: [countNode(store, leftId), countNode(store, split.rightId)],
  });
}

export function removeEntry(store: SyncStore, entry: IndexEntry): boolean {
  if (!store.root) return false;
  return removeFrom(store, store.root, entry);
}

export function treeTotal(store: SyncStore): number {
  if (!store.root) return 0;
  return countNode(store, store.root);
}

export function sliceEntries(store: SyncStore, offset: number, limit: number): IndexEntry[] {
  const out: IndexEntry[] = [];
  if (!store.root || limit <= 0 || offset < 0) return out;
  collect(store, store.root, offset, limit, out);
  return out;
}

export function pageEntries(
  store: SyncStore,
  page: number,
  perPage: number,
  order: "asc" | "desc",
): { entries: IndexEntry[]; total: number } {
  const total = treeTotal(store);
  const start = order === "desc" ? Math.max(0, total - page * perPage) : (page - 1) * perPage;
  const end = order === "desc" ? total - (page - 1) * perPage : Math.min(total, page * perPage);
  const count = Math.max(0, end - start);
  const entries = store.root && count > 0 ? sliceEntries(store, start, count) : [];
  if (order === "desc") entries.reverse();
  return { entries, total };
}

/** Pack an already sorted entry list into a fresh tree. */
export function bulkLoad(store: SyncStore, entries: IndexEntry[], capacity: number): void {
  if (entries.length === 0) {
    store.root = null;
    return;
  }

  let level: { id: string; count: number; first: IndexEntry }[] = [];
  for (let offset = 0; offset < entries.length; offset += capacity) {
    const chunk = entries.slice(offset, offset + capacity);
    const id = store.create({ t: "l", e: chunk });
    level.push({ id, count: chunk.length, first: chunk[0] });
  }

  while (level.length > 1) {
    const next: { id: string; count: number; first: IndexEntry }[] = [];
    for (let offset = 0; offset < level.length; offset += capacity) {
      const group = level.slice(offset, offset + capacity);
      const id = store.create({
        t: "i",
        s: group.slice(1).map((child) => child.first),
        c: group.map((child) => child.id),
        n: group.map((child) => child.count),
      });
      const count = group.reduce((sum, child) => sum + child.count, 0);
      next.push({ id, count, first: group[0].first });
    }
    level = next;
  }

  store.root = level[0].id;
}

export function entriesInOrder(store: SyncStore): IndexEntry[] {
  return sliceEntries(store, 0, treeTotal(store));
}

interface Split {
  separator: IndexEntry | null;
  rightId: string | null;
}

function insertInto(store: SyncStore, nodeId: string, entry: IndexEntry, capacity: number): Split {
  const node = store.get(nodeId);
  if (node.t === "l") {
    const index = lowerBound(node.e, entry);
    if (index < node.e.length && compareEntries(node.e[index], entry) === 0) {
      return { separator: null, rightId: null };
    }
    node.e.splice(index, 0, entry);
    store.set(nodeId, node);
    if (node.e.length <= capacity) return { separator: null, rightId: null };

    const mid = Math.ceil(node.e.length / 2);
    const rightEntries = node.e.splice(mid);
    const rightId = store.create({ t: "l", e: rightEntries });
    store.set(nodeId, node);
    return { separator: rightEntries[0], rightId };
  }

  let childIndex = 0;
  while (childIndex < node.s.length && compareEntries(entry, node.s[childIndex]) >= 0) childIndex += 1;
  const split = insertInto(store, node.c[childIndex], entry, capacity);
  node.n[childIndex] = countNode(store, node.c[childIndex]);
  if (split.rightId && split.separator) {
    node.s.splice(childIndex, 0, split.separator);
    node.c.splice(childIndex + 1, 0, split.rightId);
    node.n.splice(childIndex + 1, 0, countNode(store, split.rightId));
  }
  store.set(nodeId, node);
  if (node.c.length <= capacity) return { separator: null, rightId: null };

  const mid = Math.ceil(node.c.length / 2);
  const promoted = node.s[mid - 1];
  const rightId = store.create({
    t: "i",
    s: node.s.slice(mid),
    c: node.c.slice(mid),
    n: node.n.slice(mid),
  });
  node.s = node.s.slice(0, mid - 1);
  node.c = node.c.slice(0, mid);
  node.n = node.n.slice(0, mid);
  store.set(nodeId, node);
  return { separator: promoted, rightId };
}

function removeFrom(store: SyncStore, nodeId: string, entry: IndexEntry): boolean {
  const node = store.get(nodeId);
  if (node.t === "l") {
    const index = lowerBound(node.e, entry);
    if (index >= node.e.length || compareEntries(node.e[index], entry) !== 0) return false;
    node.e.splice(index, 1);
    store.set(nodeId, node);
    return true;
  }

  let childIndex = 0;
  while (childIndex < node.s.length && compareEntries(entry, node.s[childIndex]) >= 0) childIndex += 1;
  const removed = removeFrom(store, node.c[childIndex], entry);
  if (!removed) return false;
  node.n[childIndex] = countNode(store, node.c[childIndex]);
  store.set(nodeId, node);
  return true;
}

function collect(store: SyncStore, nodeId: string, offset: number, limit: number, out: IndexEntry[]): void {
  if (limit <= 0) return;
  const node = store.get(nodeId);
  if (node.t === "l") {
    out.push(...node.e.slice(offset, offset + limit));
    return;
  }

  let remaining = limit;
  for (let index = 0; index < node.c.length && remaining > 0; index += 1) {
    const count = node.n[index];
    if (offset >= count) {
      offset -= count;
      continue;
    }
    const before = out.length;
    collect(store, node.c[index], offset, Math.min(remaining, count - offset), out);
    remaining -= out.length - before;
    offset = 0;
  }
}

function lowerBound(entries: IndexEntry[], entry: IndexEntry): number {
  let lo = 0;
  let hi = entries.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (compareEntries(entries[mid], entry) < 0) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}
