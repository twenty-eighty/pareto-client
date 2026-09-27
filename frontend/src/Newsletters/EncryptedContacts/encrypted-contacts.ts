import { ContactsApi } from "./contacts";
import { HttpClient } from "./http";
import {
  contactIndexKeys,
  SortIndex,
  type ContactIndexKeys,
  type SortField,
  type SortIndexStore,
  type SortOrder,
  type SortRoot,
} from "./sort-index";
import {
  contactWithoutTags,
  emailHashInput,
  encodeAuthHeader,
  isActiveContact,
  randomSalt,
  searchHashInput,
  searchableTerms,
  sha256Hex,
  tagHashInput,
  unwrapStoredSalt,
  wrapStoredSalt,
} from "./protocol";
import { signAuthEvent, type ContactsSigner } from "./signer";
import type {
  ChallengeResponse,
  ContactInput,
  ContactRecord,
  DecryptedContact,
  LoginResponse,
  TagFilter,
  TagNameFilter,
} from "./types";

export interface EncryptedContactsOptions {
  baseUrl: string;
  signer: ContactsSigner;
}

export interface ContactPage {
  contacts: DecryptedContact[];
  errors: string[];
  /** Number of encrypted rows returned by the server before local decryption. */
  sourceCount: number;
  total?: number;
}

const BULK_BATCH_SIZE = 100;

export class EncryptedContacts {
  readonly api: ContactsApi;
  private readonly http: HttpClient;
  private readonly signer: ContactsSigner;
  private readonly baseUrl: string;
  private jwt?: string;
  private pubkey?: string;
  private salt?: string;
  private authPromise: Promise<void> | null = null;
  private sortIndexEngine: SortIndex | null = null;
  private sortIndexReady: boolean | null = null;

  constructor(options: EncryptedContactsOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, "");
    this.signer = options.signer;
    this.http = new HttpClient({
      baseUrl: this.baseUrl,
      getAuthToken: () => this.jwt,
    });
    this.api = new ContactsApi(this.http);
  }

  async authenticate(): Promise<void> {
    const challenge = await this.http.request<ChallengeResponse>(
      "GET",
      "/api/auth/challenge",
      undefined,
      undefined,
      { skipAuth: true },
    );

    const pubkey = await this.signer.getPublicKey();
    const wrappedSalt = wrapStoredSalt(await this.signer.nip44Encrypt(pubkey, randomSalt()));
    const loginUrl = `${this.baseUrl}/api/auth/login`;
    const event = await signAuthEvent(this.signer, loginUrl, challenge.challenge);
    const login = await this.http.request<LoginResponse>(
      "POST",
      "/api/auth/login",
      undefined,
      { salt: wrappedSalt },
      { skipAuth: true, authorization: encodeAuthHeader(event) },
    );

    if (!login.salt) {
      throw new Error("Login did not return a salt");
    }

    this.jwt = login.token;
    this.pubkey = event.pubkey;
    this.salt = await this.signer.nip44Decrypt(event.pubkey, unwrapStoredSalt(login.salt));
  }

  async ensureAuthenticated(): Promise<void> {
    if (this.jwt && this.salt && this.pubkey) return;

    if (!this.authPromise) {
      this.authPromise = this.authenticate().catch((error: unknown) => {
        this.authPromise = null;
        throw error;
      });
    }

    await this.authPromise;
  }

  async storeContactsBulk(
    contacts: ContactInput[],
    overwrite = false,
  ): Promise<{ status: string; stored: number; tagErrors: string[] }> {
    await this.ensureAuthenticated();
    const withEmail = (contacts || []).filter((contact) => String(contact?.email || "").trim());
    if (withEmail.length === 0) {
      return { status: "ok", stored: 0, tagErrors: [] };
    }

    const tagErrors = await this.ensureTags(withEmail.flatMap((contact) => contact.tags || []));
    const rows = [];
    const imported: { id: string; keys: ContactIndexKeys }[] = [];
    for (const contact of withEmail) {
      const email = String(contact.email).trim();
      const id = crypto.randomUUID();
      rows.push({ ...(await this.prepareContact({ ...contact, email })), id });
      imported.push({ id, keys: contactIndexKeys({ ...contact, email }) });
    }

    let status = "ok";
    const storedIds = new Set<string>();
    let reportedIds = false;
    for (let offset = 0; offset < rows.length; offset += BULK_BATCH_SIZE) {
      const result = await this.api.bulkImport(rows.slice(offset, offset + BULK_BATCH_SIZE), overwrite);
      status = result?.status || status;
      if (Array.isArray(result?.ids)) {
        reportedIds = true;
        for (const id of result.ids) storedIds.add(id);
      }
    }

    const indexed = reportedIds ? imported.filter((entry) => storedIds.has(entry.id)) : imported;
    await this.syncBulkIndex(indexed, overwrite, reportedIds ? storedIds.size : withEmail.length);
    return { status, stored: reportedIds ? storedIds.size : withEmail.length, tagErrors };
  }

  async updateContact(
    id: string,
    contact: ContactInput,
  ): Promise<{ contact: DecryptedContact; tagErrors: string[] }> {
    await this.ensureAuthenticated();
    if (!id) throw new Error("Contact id is required");
    const email = requiredEmail(contact.email);
    const tagErrors = await this.ensureTags(contact.tags);
    const ready = await this.indexIsReady();
    const previousContact = ready ? await this.getContact(id) : null;
    const previous = previousContact ? contactIndexKeys(previousContact) : null;
    const updated = await this.api.update(id, await this.prepareContact({ ...contact, email }));
    if (previous) {
      await this.touchIndex(updated.contact.id, contactIndexKeys({ ...contact, email }), previous);
    }
    return {
      contact: {
        ...contactWithoutTags({ ...contact, email }),
        id: updated.contact.id,
        tags: [...(contact.tags || [])],
        active: isActiveContact({ ...contact, email }),
      },
      tagErrors,
    };
  }

  async getContactsSorted(
    field: SortField,
    page = 1,
    perPage = 100,
    order: SortOrder = "asc",
  ): Promise<ContactPage> {
    await this.ensureAuthenticated();
    if (perPage < 1 || perPage > 100) throw new Error("perPage must be from 1 to 100");
    if (!(await this.indexIsReady())) {
      const total = await this.countContacts();
      if (total === 0) return { contacts: [], errors: [], sourceCount: 0, total: 0 };
      await this.rebuildSortIndex();
    }
    const engine = await this.engine();
    let loaded = await this.readSortedPage(engine, field, page, perPage, order);
    const duplicate = new Set(loaded.ids).size !== loaded.ids.length;
    if (loaded.missingIds.length > 0 || duplicate) {
      try {
        await engine.removeIds(loaded.missingIds);
        loaded = await this.readSortedPage(engine, field, page, perPage, order);
      } catch (error) {
        loaded.errors = loaded.errors.concat(error instanceof Error ? error.message : String(error));
      }
    }
    return {
      contacts: loaded.contacts,
      errors: loaded.errors,
      sourceCount: loaded.sourceCount,
      total: loaded.total,
    };
  }

  private async readSortedPage(
    engine: SortIndex,
    field: SortField,
    page: number,
    perPage: number,
    order: SortOrder,
  ): Promise<{
    contacts: DecryptedContact[];
    errors: string[];
    sourceCount: number;
    total: number;
    ids: string[];
    missingIds: string[];
  }> {
    const { ids, total } = await engine.page(field, page, perPage, order);
    if (ids.length === 0) {
      return { contacts: [], errors: [], sourceCount: 0, total, ids, missingIds: [] };
    }
    const result = await this.api.listByIds(ids);
    const records = result.contacts || [];
    const decrypted = await this.decryptRecords(records);
    const returned = new Set(records.map((contact) => contact.id));
    return {
      ...decrypted,
      total,
      ids,
      missingIds: ids.filter((id) => !returned.has(id)),
    };
  }

  async rebuildSortIndex(): Promise<void> {
    await this.ensureAuthenticated();
    await (await this.engine()).rebuild(async (page) => {
      const result = await this.api.list({ page, per_page: 100 });
      const decrypted = await this.decryptRecords(result.contacts);
      return {
        entries: decrypted.contacts.map((contact) => ({ id: contact.id, keys: contactIndexKeys(contact) })),
        more: result.contacts.length === 100,
      };
    });
    this.sortIndexReady = true;
  }

  async getContacts(page = 1, perPage = 100): Promise<ContactPage> {
    await this.ensureAuthenticated();
    const result = await this.api.list({ page, per_page: perPage });
    return this.decryptRecords(result.contacts);
  }

  async getContact(id: string): Promise<DecryptedContact | null> {
    await this.ensureAuthenticated();
    const result = await this.api.show(id);
    const decrypted = await this.decryptRecords(result?.contact ? [result.contact] : []);
    return decrypted.contacts[0] ?? null;
  }

  async countContacts(filter?: TagNameFilter): Promise<number> {
    await this.ensureAuthenticated();
    if (filter && Object.keys(filter).length > 0) {
      const result = await this.api.tagsCount(await this.hashTagFilter(filter));
      return result?.count ?? 0;
    }
    const total = await this.api.count();
    return total?.count ?? 0;
  }

  async countActiveRecipients(filter?: TagNameFilter): Promise<number> {
    await this.ensureAuthenticated();
    if (filter && Object.keys(filter).length > 0) {
      const result = await this.api.countActive(await this.hashTagFilter(filter));
      return result?.count ?? 0;
    }
    const result = await this.api.countActive();
    return result?.count ?? 0;
  }

  async getContactsByCriteria(
    filter: TagNameFilter,
    page = 1,
    perPage = 100,
  ): Promise<ContactPage> {
    await this.ensureAuthenticated();
    const result = await this.api.tagsFilter(await this.hashTagFilter(filter), {
      page,
      per_page: perPage,
    });
    return this.decryptRecords(result.contacts);
  }

  async searchContacts(searchTerm: string, page = 1, perPage = 25): Promise<ContactPage> {
    await this.ensureAuthenticated();
    const token = await sha256Hex(searchHashInput(this.requireSalt(), String(searchTerm)));
    const result = await this.api.searchByToken(token, { page, per_page: perPage });
    const decrypted = await this.decryptRecords(result.contacts);
    const total =
      result.contacts.length < perPage
        ? (page - 1) * perPage + result.contacts.length
        : page * perPage + 1;
    return { ...decrypted, total };
  }

  async getContactTags(): Promise<{ tags: string[]; errors: string[] }> {
    await this.ensureAuthenticated();
    const pubkey = this.requirePubkey();
    const result = await this.api.listTags();
    const tags: string[] = [];
    const errors: string[] = [];

    for (const tag of result.tags) {
      try {
        const name = await this.signer.nip44Decrypt(pubkey, tag.ciphertext_tag);
        if (name) tags.push(name);
      } catch (error) {
        errors.push(error instanceof Error ? error.message : String(error));
      }
    }

    return { tags, errors };
  }

  async addTag(tag: string): Promise<void> {
    await this.ensureAuthenticated();
    const blindIndex = await sha256Hex(tagHashInput(this.requireSalt(), tag));
    const ciphertextTag = await this.signer.nip44Encrypt(this.requirePubkey(), tag);
    await this.api.upsertTag({
      blind_index: blindIndex,
      ciphertext_tag: ciphertextTag,
      key_version: 1,
    });
  }

  async deleteTag(tag: string): Promise<void> {
    await this.ensureAuthenticated();
    const blindIndex = await sha256Hex(tagHashInput(this.requireSalt(), tag));
    await this.api.deleteTag(blindIndex);
  }

  private async prepareContact(contact: ContactInput) {
    const email = requiredEmail(contact.email);
    const pubkey = this.requirePubkey();
    const salt = this.requireSalt();
    const encryptedData = await this.signer.nip44Encrypt(
      pubkey,
      JSON.stringify(contactWithoutTags(contact)),
    );
    const terms = searchableTerms({ ...contact, email });
    return {
      encrypted_data: encryptedData,
      email_hash: await sha256Hex(emailHashInput(salt, email)),
      search_tokens: await Promise.all(
        terms.map((term) => sha256Hex(searchHashInput(salt, term))),
      ),
      tag_hashes: await Promise.all(
        (contact.tags || []).map((tag) => sha256Hex(tagHashInput(salt, tag))),
      ),
      version: 1,
      active: isActiveContact(contact),
    };
  }

  private async ensureTags(tags: string[] | undefined): Promise<string[]> {
    const errors: string[] = [];
    const unique = [...new Set((tags || []).filter(Boolean))];
    for (const tag of unique) {
      try {
        await this.addTag(tag);
      } catch (error) {
        errors.push(error instanceof Error ? error.message : String(error));
      }
    }
    return errors;
  }

  private async decryptRecords(records: ContactRecord[]): Promise<ContactPage> {
    const pubkey = this.requirePubkey();
    const contacts: DecryptedContact[] = [];
    const errors: string[] = [];

    for (const record of records || []) {
      try {
        const decrypted = JSON.parse(
          await this.signer.nip44Decrypt(pubkey, record.encrypted_data),
        ) as Record<string, unknown>;
        const catalog = await this.decryptTagList(record.tags);
        errors.push(...catalog.errors);
        const { tags: _blobTags, ...contact } = decrypted;
        contacts.push({
          ...contact,
          id: record.id,
          tags: catalog.tags,
          active: record.active,
        });
      } catch (error) {
        errors.push(error instanceof Error ? error.message : String(error));
      }
    }

    return { contacts, errors, sourceCount: records?.length || 0 };
  }

  private async decryptTagList(
    contactTags: ContactRecord["tags"],
  ): Promise<{ tags: string[]; errors: string[] }> {
    const pubkey = this.requirePubkey();
    const tags: string[] = [];
    const errors: string[] = [];

    for (const tag of contactTags || []) {
      if (!tag?.ciphertext_tag) continue;
      try {
        const name = await this.signer.nip44Decrypt(pubkey, tag.ciphertext_tag);
        if (name) tags.push(name);
      } catch (error) {
        errors.push(error instanceof Error ? error.message : String(error));
      }
    }

    return { tags, errors };
  }

  private async hashTagFilter(filter: TagNameFilter): Promise<TagFilter> {
    const salt = this.requireSalt();
    const hash = (tag: string) => sha256Hex(tagHashInput(salt, tag));

    if ("any" in filter) return { any: await hashNames(filter.any, hash) };
    if ("all" in filter) return { all: await hashNames(filter.all, hash) };
    if ("not" in filter) return { not: await this.hashTagFilter(filter.not) };
    if ("and" in filter) {
      return { and: await Promise.all(filter.and.map((part) => this.hashTagFilter(part))) };
    }
    if ("or" in filter) {
      return { or: await Promise.all(filter.or.map((part) => this.hashTagFilter(part))) };
    }
    throw new Error("Unknown tag filter");
  }

  private requireSalt(): string {
    if (!this.salt) throw new Error("Not authenticated");
    return this.salt;
  }

  private requirePubkey(): string {
    if (!this.pubkey) throw new Error("Not authenticated");
    return this.pubkey;
  }

  private async touchIndex(id: string, keys: ContactIndexKeys, previous: ContactIndexKeys | null): Promise<void> {
    if (!(await this.indexIsReady())) return;
    try {
      await (await this.engine()).upsert(id, keys, previous);
    } catch (error) {
      throw indexWriteError("saved", error);
    }
  }

  private async syncBulkIndex(
    imported: { id: string; keys: ContactIndexKeys }[],
    overwrite: boolean,
    stored: number,
  ): Promise<void> {
    const index = await this.engine();
    if (overwrite) {
      await this.rebuildSortIndex();
      return;
    }
    if (await this.indexIsReady()) {
      await index.merge(imported);
      return;
    }
    const total = await this.countContacts();
    if (total === stored) {
      await index.buildFresh(imported);
      this.sortIndexReady = true;
    }
  }

  private async indexIsReady(): Promise<boolean> {
    if (this.sortIndexReady !== null) return this.sortIndexReady;
    this.sortIndexReady = await (await this.engine()).ready();
    return this.sortIndexReady;
  }

  private async engine(): Promise<SortIndex> {
    if (!this.sortIndexEngine) {
      this.sortIndexEngine = new SortIndex(new ApiSortStore(this.api), this.requireSalt());
    }
    return this.sortIndexEngine;
  }
}

async function hashNames(
  tags: string[],
  hash: (tag: string) => Promise<string>,
): Promise<string[]> {
  if (!tags?.length) throw new Error("Tag filter list is empty");
  return Promise.all(tags.map((tag) => hash(tag)));
}

function requiredEmail(email: unknown): string {
  const value = String(email || "").trim();
  if (!value) throw new Error("Email is required");
  return value;
}

class ApiSortStore implements SortIndexStore {
  constructor(private readonly api: ContactsApi) {}

  acquireLease(): Promise<{ token: string; expires_at: string }> {
    return this.api.acquireSortLease();
  }

  releaseLease(token: string): Promise<void> {
    return this.api.releaseSortLease(token).then(() => undefined);
  }

  async getRoot(field: SortField): Promise<SortRoot | null> {
    const result = await this.api.getSortRoot(field);
    return result.root;
  }

  putRoot(field: SortField, nodeId: string, expectedGeneration: number, leaseToken?: string): Promise<SortRoot> {
    return this.api.putSortRoot(field, nodeId, expectedGeneration, leaseToken).then((result) => result.root);
  }

  async getNode(field: SortField, nodeId: string): Promise<{ ciphertext: string; version: number } | null> {
    try {
      return await this.api.getSortNode(field, nodeId);
    } catch (error) {
      if (error instanceof Error && error.message.includes("not found")) return null;
      throw error;
    }
  }

  putNode(
    field: SortField,
    nodeId: string,
    ciphertext: string,
    expectedVersion: number,
    leaseToken?: string,
  ): Promise<{ version: number }> {
    return this.api.putSortNode(field, nodeId, ciphertext, expectedVersion, leaseToken);
  }

  writeNodes(
    field: SortField,
    nodes: { node_id: string; ciphertext: string }[],
    leaseToken: string,
  ): Promise<void> {
    return this.api.writeSortNodes(field, nodes, leaseToken).then(() => undefined);
  }

  writeRuns(field: SortField, runs: string[], leaseToken: string, append?: boolean): Promise<void> {
    return this.api.writeSortRuns(field, runs, leaseToken, append ?? false).then(() => undefined);
  }

  async listRuns(field: SortField): Promise<{ seq: number; ciphertext: string }[]> {
    const result = await this.api.listSortRuns(field);
    return result.runs;
  }

  commit(
    field: SortField,
    leaseToken: string,
    rootNodeId: string | null,
    nodeIds: string[],
  ): Promise<SortRoot | null> {
    return this.api.commitSortIndex(field, leaseToken, rootNodeId, nodeIds).then((result) => result.root);
  }
}

function indexWriteError(action: "saved" | "deleted", error: unknown): Error {
  const message = error instanceof Error ? error.message : String(error);
  return new Error(`Contact was ${action}, and the sort index update failed: ${message}`);
}
