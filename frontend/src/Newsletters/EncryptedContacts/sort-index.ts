import {
  bulkLoad,
  compareEntries,
  compareSortKey,
  countNode,
  insertEntry,
  pageEntries,
  removeEntry,
  sliceEntries,
  type IndexEntry,
  type SortNode,
  type SyncStore,
} from "./sort-tree";

export const SORT_FIELDS = [
  "first_name",
  "last_name",
  "email",
  "unsubscribe_date",
  "dnd",
] as const;

export type SortField = (typeof SORT_FIELDS)[number];
export type SortOrder = "asc" | "desc";

export interface ContactIndexKeys {
  first_name: string | null;
  last_name: string | null;
  email: string | null;
  unsubscribe_date: string | null;
  dnd: string | null;
}

export interface SortRoot {
  node_id: string;
  generation: number;
}

export interface SortNodeRecord {
  ciphertext: string;
  version: number;
}

export interface SortIndexStore {
  acquireLease(): Promise<{ token: string; expires_at: string }>;
  releaseLease(token: string): Promise<void>;
  getRoot(field: SortField): Promise<SortRoot | null>;
  putRoot(
    field: SortField,
    nodeId: string,
    expectedGeneration: number,
    leaseToken?: string,
  ): Promise<SortRoot>;
  getNode(field: SortField, nodeId: string): Promise<SortNodeRecord | null>;
  putNode(
    field: SortField,
    nodeId: string,
    ciphertext: string,
    expectedVersion: number,
    leaseToken?: string,
  ): Promise<{ version: number }>;
  writeNodes(
    field: SortField,
    nodes: { node_id: string; ciphertext: string }[],
    leaseToken: string,
  ): Promise<void>;
  writeRuns(field: SortField, runs: string[], leaseToken: string, append?: boolean): Promise<void>;
  listRuns(field: SortField): Promise<{ seq: number; ciphertext: string }[]>;
  commit(
    field: SortField,
    leaseToken: string,
    rootNodeId: string | null,
    nodeIds: string[],
  ): Promise<SortRoot | null>;
}

export interface SortIndexOptions {
  capacity?: number;
  padBytes?: number;
  runSize?: number;
}

const DEFAULT_CAPACITY = 32;
const DEFAULT_PAD_BYTES = 16_384;
const DEFAULT_RUN_SIZE = 100;

export function contactIndexKeys(contact: {
  firstName?: unknown;
  lastName?: unknown;
  email?: unknown;
  Email?: unknown;
  emailAddress?: unknown;
  dnd?: unknown;
  dateUnsubscription?: unknown;
  dateunsub?: unknown;
}): ContactIndexKeys {
  const email = contact.email || contact.Email || contact.emailAddress;
  return {
    first_name: sortText(contact.firstName),
    last_name: sortText(contact.lastName),
    email: sortText(email),
    unsubscribe_date: sortUnsubscribe(contact),
    dnd: contact.dnd === true || contact.dnd === "true" || contact.dnd === 1 ? "1" : "0",
  };
}

export class SortIndex {
  private readonly capacity: number;
  private readonly padBytes: number;
  private readonly runSize: number;
  private keyPromise: Promise<CryptoKey> | null = null;

  constructor(
    private readonly store: SortIndexStore,
    private readonly salt: string,
    options: SortIndexOptions = {},
  ) {
    this.capacity = options.capacity ?? DEFAULT_CAPACITY;
    this.padBytes = options.padBytes ?? DEFAULT_PAD_BYTES;
    this.runSize = options.runSize ?? DEFAULT_RUN_SIZE;
  }

  async ready(): Promise<boolean> {
    const root = await this.store.getRoot("first_name");
    return root !== null;
  }

  /**
   * Drop ids the contact table no longer has, and keep one entry per remaining id.
   * A page is otherwise short by every index slot that does not load.
   */
  async removeIds(ids: string[]): Promise<void> {
    const drop = new Set(ids);
    const token = await this.store.acquireLease().then((lease) => lease.token);
    try {
      for (const field of SORT_FIELDS) {
        const existing = await this.readAll(field);
        const seen = new Set<string>();
        const kept: IndexEntry[] = [];
        for (const entry of existing) {
          if (drop.has(entry.id) || seen.has(entry.id)) continue;
          seen.add(entry.id);
          kept.push(entry);
        }
        if (kept.length !== existing.length) await this.commitSorted(field, kept, token);
      }
    } finally {
      await this.store.releaseLease(token);
    }
  }

  async page(
    field: SortField,
    page: number,
    perPage: number,
    order: SortOrder,
  ): Promise<{ ids: string[]; total: number }> {
    const root = await this.store.getRoot(field);
    if (!root) throw new Error("Sort index is not built. Call rebuildSortIndex().");

    const store = await this.tracked(field, root);
    await store.hydrate(root.node_id);
    const total = countNode(store, root.node_id);
    const start = order === "desc" ? Math.max(0, total - page * perPage) : (page - 1) * perPage;
    const end = order === "desc" ? total - (page - 1) * perPage : Math.min(total, page * perPage);
    if (end > start) await this.hydrateSlice(store, root.node_id, start, end - start);
    const result = pageEntries(store, page, perPage, order);
    return { ids: result.entries.map((entry) => entry.id), total: result.total };
  }

  async upsert(id: string, keys: ContactIndexKeys, previous: ContactIndexKeys | null): Promise<void> {
    for (const field of SORT_FIELDS) {
      const next = keys[field];
      const prev = previous ? previous[field] : undefined;
      if (previous && compareSortKey(prev ?? null, next) === 0) continue;
      await this.retry(() =>
        this.replaceOnField(field, previous ? { k: prev ?? null, id } : null, { k: next, id }),
      );
    }
  }

  async remove(id: string, keys: ContactIndexKeys): Promise<void> {
    for (const field of SORT_FIELDS) {
      await this.retry(async () => {
        await this.removeOnField(field, { k: keys[field], id });
      });
    }
  }

  /** Build trees from contacts already in memory. Replaces any existing index. */
  async buildFresh(entries: { id: string; keys: ContactIndexKeys }[]): Promise<void> {
    const token = await this.store.acquireLease().then((lease) => lease.token);
    try {
      for (const field of SORT_FIELDS) {
        const sorted = entries
          .map((entry) => ({ k: entry.keys[field], id: entry.id }))
          .sort(compareEntries);
        await this.commitSorted(field, sorted, token);
      }
    } finally {
      await this.store.releaseLease(token);
    }
  }

  /**
   * Merge new contacts into the existing trees.
   * Reads sealed leaves, not contact blobs.
   */
  async merge(entries: { id: string; keys: ContactIndexKeys }[]): Promise<void> {
    if (entries.length === 0) return;
    if (!(await this.ready())) {
      await this.buildFresh(entries);
      return;
    }

    const token = await this.store.acquireLease().then((lease) => lease.token);
    try {
      for (const field of SORT_FIELDS) {
        const existing = await this.readAll(field);
        const incoming = new Map(entries.map((entry) => [entry.id, entry.keys[field]]));
        const kept = existing.filter((entry) => !incoming.has(entry.id));
        const added = entries.map((entry) => ({ k: entry.keys[field], id: entry.id }));
        const sorted = kept.concat(added).sort(compareEntries);
        await this.commitSorted(field, sorted, token);
      }
    } finally {
      await this.store.releaseLease(token);
    }
  }

  /**
   * Stream contacts, keep at most `runSize` plaintext sort keys per field,
   * and store sealed runs before merging one field at a time.
   */
  async rebuild(
    loadPage: (page: number) => Promise<{ entries: { id: string; keys: ContactIndexKeys }[]; more: boolean }>,
  ): Promise<void> {
    const token = await this.store.acquireLease().then((lease) => lease.token);
    try {
      for (const field of SORT_FIELDS) await this.store.writeRuns(field, [], token, false);
      const buffers = emptyBuffers();

      for (let page = 1; ; page += 1) {
        const batch = await loadPage(page);

        for (const contact of batch.entries) {
          for (const field of SORT_FIELDS) {
            const buffer = buffers.get(field)!;
            buffer.push({ k: contact.keys[field], id: contact.id });
            if (buffer.length >= this.runSize) {
              buffer.sort(compareEntries);
              await this.flushRun(field, buffer, token);
            }
          }
        }

        if (!batch.more) break;
      }

      for (const field of SORT_FIELDS) {
        const buffer = buffers.get(field)!;
        if (buffer.length > 0) {
          buffer.sort(compareEntries);
          await this.flushRun(field, buffer, token);
        }
        const runs = await this.store.listRuns(field);
        const key = await this.encryptionKey();
        const merged: IndexEntry[] = [];
        for (const run of runs.sort((a, b) => a.seq - b.seq)) {
          merged.push(...(await openJson<IndexEntry[]>(key, run.ciphertext)));
        }
        merged.sort(compareEntries);
        await this.commitSorted(field, merged, token);
      }
    } finally {
      await this.store.releaseLease(token);
    }
  }

  private async flushRun(field: SortField, buffer: IndexEntry[], token: string): Promise<void> {
    const key = await this.encryptionKey();
    const pending = buffer.splice(0, buffer.length);
    while (pending.length > 0) {
      let count = pending.length;
      let sealed: string | null = null;
      while (count > 0) {
        try {
          sealed = await sealJson(key, pending.slice(0, count), this.padBytes);
          break;
        } catch (error) {
          if (!(error instanceof Error) || !error.message.includes("too large") || count === 1) throw error;
          count = Math.floor(count / 2);
        }
      }
      pending.splice(0, count);
      await this.store.writeRuns(field, [sealed!], token, true);
    }
  }

  private async commitSorted(field: SortField, entries: IndexEntry[], token: string): Promise<void> {
    const built = new MemoryStore();
    bulkLoad(built, entries, this.capacity);
    if (!built.root) {
      await this.store.commit(field, token, null, []);
      return;
    }

    const key = await this.encryptionKey();
    const payload: { node_id: string; ciphertext: string }[] = [];
    for (const [nodeId, node] of built.nodes) {
      payload.push({ node_id: nodeId, ciphertext: await sealJson(key, node, this.padBytes) });
    }

    for (let offset = 0; offset < payload.length; offset += 100) {
      await this.store.writeNodes(field, payload.slice(offset, offset + 100), token);
    }
    await this.store.commit(field, token, built.root, [...built.nodes.keys()]);
  }

  private async readAll(field: SortField): Promise<IndexEntry[]> {
    const root = await this.store.getRoot(field);
    if (!root) return [];
    const store = await this.tracked(field, root);
    await this.hydrateAll(store, store.root);
    return sliceEntries(store, 0, countNode(store, store.root!));
  }

  private async replaceOnField(
    field: SortField,
    previous: IndexEntry | null,
    next: IndexEntry,
  ): Promise<void> {
    const root = await this.store.getRoot(field);
    const store = await this.tracked(field, root);
    if (previous && store.root) {
      await this.hydratePath(store, previous);
      removeEntry(store, previous);
    }
    if (store.root) await this.hydratePath(store, next);
    insertEntry(store, next, this.capacity);
    await this.flush(store, field);
  }

  private async insertOnField(field: SortField, entry: IndexEntry): Promise<void> {
    const root = await this.store.getRoot(field);
    const store = await this.tracked(field, root);
    if (store.root) await this.hydratePath(store, entry);
    insertEntry(store, entry, this.capacity);
    await this.flush(store, field);
  }

  private async removeOnField(field: SortField, entry: IndexEntry): Promise<void> {
    const root = await this.store.getRoot(field);
    if (!root) return;
    const store = await this.tracked(field, root);
    await this.hydratePath(store, entry);
    removeEntry(store, entry);
    await this.flush(store, field);
  }

  private async tracked(field: SortField, root: SortRoot | null): Promise<TrackedStore> {
    const key = await this.encryptionKey();
    return new TrackedStore(field, this.store, key, root);
  }

  private async hydratePath(store: TrackedStore, entry: IndexEntry): Promise<void> {
    let id = store.root;
    while (id) {
      await store.hydrate(id);
      const node = store.get(id);
      if (node.t === "l") return;
      let child = 0;
      while (child < node.s.length && compareEntries(entry, node.s[child]) >= 0) child += 1;
      id = node.c[child];
    }
  }

  private async hydrateSlice(store: TrackedStore, id: string, offset: number, limit: number): Promise<void> {
    await store.hydrate(id);
    const node = store.get(id);
    if (node.t === "l" || limit <= 0) return;
    for (let index = 0; index < node.c.length && limit > 0; index += 1) {
      const count = node.n[index];
      if (offset >= count) {
        offset -= count;
        continue;
      }
      const take = Math.min(limit, count - offset);
      await this.hydrateSlice(store, node.c[index], offset, take);
      limit -= take;
      offset = 0;
    }
  }

  private async hydrateAll(store: TrackedStore, id: string | null): Promise<void> {
    if (!id) return;
    await store.hydrate(id);
    const node = store.get(id);
    if (node.t === "i") {
      for (const child of node.c) await this.hydrateAll(store, child);
    }
  }

  private async flush(store: TrackedStore, field: SortField): Promise<void> {
    if (!store.hasDirty()) return;
    store.copyOnWrite();
    const key = await this.encryptionKey();
    for (const nodeId of store.createdIds()) {
      const ciphertext = await sealJson(key, store.get(nodeId), this.padBytes);
      await this.store.putNode(field, nodeId, ciphertext, 0);
    }
    if (store.root && store.rootChanged()) {
      await this.store.putRoot(field, store.root, store.generation);
    }
  }

  private async retry(operation: () => Promise<void>): Promise<void> {
    let last: unknown;
    for (let attempt = 0; attempt < 8; attempt += 1) {
      try {
        await operation();
        return;
      } catch (error) {
        last = error;
        const kind = indexErrorKind(error);
        if (kind === "locked") {
          await delay(40 * (attempt + 1));
          continue;
        }
        if (kind === "conflict") continue;
        throw error;
      }
    }
    throw last instanceof Error ? last : new Error("sort index update failed");
  }

  private encryptionKey(): Promise<CryptoKey> {
    if (!this.keyPromise) this.keyPromise = deriveSortIndexKey(this.salt);
    return this.keyPromise;
  }
}

class MemoryStore implements SyncStore {
  readonly nodes = new Map<string, SortNode>();
  root: string | null = null;

  get(id: string): SortNode {
    const node = this.nodes.get(id);
    if (!node) throw new Error("missing sort index node");
    return node;
  }

  set(id: string, node: SortNode): void {
    this.nodes.set(id, node);
  }

  create(node: SortNode): string {
    const id = crypto.randomUUID();
    this.nodes.set(id, node);
    return id;
  }
}

class TrackedStore implements SyncStore {
  readonly nodes = new Map<string, SortNode>();
  private readonly versions = new Map<string, number>();
  private readonly dirty = new Set<string>();
  root: string | null;
  generation: number;
  private initialRoot: string | null;

  constructor(
    private readonly field: SortField,
    private readonly store: SortIndexStore,
    private readonly key: CryptoKey,
    root: SortRoot | null,
  ) {
    this.root = root?.node_id ?? null;
    this.initialRoot = this.root;
    this.generation = root?.generation ?? 0;
  }

  get(id: string): SortNode {
    const node = this.nodes.get(id);
    if (!node) throw new Error("missing sort index node");
    return node;
  }

  set(id: string, node: SortNode): void {
    this.nodes.set(id, node);
    this.dirty.add(id);
  }

  create(node: SortNode): string {
    const id = crypto.randomUUID();
    this.nodes.set(id, node);
    this.dirty.add(id);
    return id;
  }

  async hydrate(id: string): Promise<void> {
    if (this.nodes.has(id)) return;
    const row = await this.store.getNode(this.field, id);
    if (!row) throw new Error("missing sort index node");
    this.nodes.set(id, await openJson<SortNode>(this.key, row.ciphertext));
    this.versions.set(id, row.version);
  }

  createdIds(): string[] {
    return [...this.dirty].filter((id) => !this.versions.has(id));
  }

  hasDirty(): boolean {
    return this.dirty.size > 0;
  }

  rootChanged(): boolean {
    return this.root !== this.initialRoot;
  }

  /** Write a fresh copy of the edited path so the published root swings last. */
  copyOnWrite(): void {
    const idMap = new Map([...this.dirty].map((id) => [id, crypto.randomUUID()]));
    for (const oldId of idMap.keys()) {
      const node = this.get(oldId);
      const copy: SortNode =
        node.t === "l"
          ? { t: "l", e: node.e.map((entry) => ({ ...entry })) }
          : {
              t: "i",
              s: node.s.map((entry) => ({ ...entry })),
              c: node.c.map((childId) => idMap.get(childId) ?? childId),
              n: [...node.n],
            };
      this.nodes.set(idMap.get(oldId)!, copy);
      this.dirty.add(idMap.get(oldId)!);
      this.dirty.delete(oldId);
    }
    if (this.root && idMap.has(this.root)) this.root = idMap.get(this.root)!;
  }
}

function emptyBuffers(): Map<SortField, IndexEntry[]> {
  return new Map(SORT_FIELDS.map((field) => [field, []]));
}

export async function deriveSortIndexKey(salt: string): Promise<CryptoKey> {
  const base = await crypto.subtle.importKey("raw", bufferOf(saltBytes(salt)), "HKDF", false, ["deriveKey"]);
  return crypto.subtle.deriveKey(
    {
      name: "HKDF",
      hash: "SHA-256",
      salt: bufferOf(new TextEncoder().encode("sort-index")),
      info: bufferOf(new TextEncoder().encode("contacts-sort-index-v1")),
    },
    base,
    { name: "AES-GCM", length: 256 },
    false,
    ["encrypt", "decrypt"],
  );
}

async function sealJson(key: CryptoKey, value: unknown, padBytes: number): Promise<string> {
  const plaintext = new TextEncoder().encode(JSON.stringify(value));
  if (plaintext.byteLength > padBytes) throw new Error("sort index node is too large");
  const padded = new Uint8Array(padBytes);
  padded.set(plaintext);
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const ciphertext = new Uint8Array(
    await crypto.subtle.encrypt({ name: "AES-GCM", iv: bufferOf(iv) }, key, bufferOf(padded)),
  );
  const packed = new Uint8Array(iv.byteLength + ciphertext.byteLength);
  packed.set(iv, 0);
  packed.set(ciphertext, iv.byteLength);
  return bytesToBase64(packed);
}

async function openJson<T>(key: CryptoKey, payload: string): Promise<T> {
  const packed = base64ToBytes(payload);
  const iv = packed.slice(0, 12);
  const ciphertext = packed.slice(12);
  const padded = new Uint8Array(
    await crypto.subtle.decrypt({ name: "AES-GCM", iv: bufferOf(iv) }, key, bufferOf(ciphertext)),
  );
  const end = padded.indexOf(0);
  const jsonBytes = end === -1 ? padded : padded.slice(0, end);
  return JSON.parse(new TextDecoder().decode(jsonBytes)) as T;
}

function sortText(value: unknown): string | null {
  if (typeof value !== "string") return null;
  const normalized = value.trim().toLowerCase();
  if (!normalized) return null;
  return normalized.slice(0, 512);
}

function sortUnsubscribe(contact: { dateUnsubscription?: unknown; dateunsub?: unknown }): string | null {
  const value = contact.dateUnsubscription ?? contact.dateunsub;
  if (value === undefined || value === null || value === "" || value === 0 || value === "0") return null;
  if (typeof value === "number" && Number.isFinite(value)) return String(Math.trunc(value)).padStart(16, "0");
  if (typeof value === "string") {
    const trimmed = value.trim();
    if (!trimmed) return null;
    if (/^-?\d+$/.test(trimmed)) {
      const asNumber = Number(trimmed);
      return asNumber === 0 ? null : String(asNumber).padStart(16, "0");
    }
    const parsed = Date.parse(trimmed);
    if (!Number.isNaN(parsed)) return String(parsed).padStart(16, "0");
  }
  return null;
}

function bufferOf(bytes: Uint8Array): ArrayBuffer {
  const copy = new ArrayBuffer(bytes.byteLength);
  new Uint8Array(copy).set(bytes);
  return copy;
}

function saltBytes(salt: string): Uint8Array {
  if (/^[0-9a-f]+$/i.test(salt) && salt.length % 2 === 0) {
    const bytes = new Uint8Array(salt.length / 2);
    for (let index = 0; index < bytes.length; index += 1) {
      bytes[index] = Number.parseInt(salt.slice(index * 2, index * 2 + 2), 16);
    }
    return bytes;
  }
  return new TextEncoder().encode(salt);
}

function bytesToBase64(bytes: Uint8Array): string {
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary);
}

function base64ToBytes(value: string): Uint8Array {
  const binary = atob(value);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) bytes[index] = binary.charCodeAt(index);
  return bytes;
}

function indexErrorKind(error: unknown): "conflict" | "locked" | null {
  const message = error instanceof Error ? error.message : String(error);
  if (message.includes("sort index is locked")) return "locked";
  if (message.includes("conflict")) return "conflict";
  return null;
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
