import { HttpClient } from "./http";
import type { SortField, SortRoot } from "./sort-index";
import type {
  ContactRecord,
  CreateOrUpdateContactRequest,
  PaginationOptions,
  TagFilter,
  TagRecord,
  TagSummary,
  UpsertTagRequest,
} from "./types";

export class ContactsApi {
  constructor(private readonly http: HttpClient) {}

  async create(contact: CreateOrUpdateContactRequest): Promise<{ contact: ContactRecord }> {
    return this.http.request("POST", "/api/contacts", { contact });
  }

  async list(opts: PaginationOptions = {}): Promise<{ contacts: ContactRecord[] }> {
    const { page, per_page } = opts;
    return this.http.request("GET", "/api/contacts", undefined, {
      page: page ?? 1,
      per_page: per_page ?? 100,
    });
  }

  async count(): Promise<{ count: number }> {
    return this.http.request("GET", "/api/contacts/count");
  }

  async countActive(filter?: TagFilter): Promise<{ count: number }> {
    if (filter) {
      return this.http.request("POST", "/api/contacts/active/count", { filter });
    }
    return this.http.request("GET", "/api/contacts/active/count");
  }

  async show(id: string): Promise<{ contact: ContactRecord }> {
    return this.http.request("GET", `/api/contacts/${encodeURIComponent(id)}`);
  }

  async update(id: string, contact: CreateOrUpdateContactRequest): Promise<{ contact: ContactRecord }> {
    return this.http.request("PUT", `/api/contacts/${encodeURIComponent(id)}`, { contact });
  }

  async delete(id: string): Promise<{ ok: boolean }> {
    return this.http.request("DELETE", `/api/contacts/${encodeURIComponent(id)}`);
  }

  async searchByToken(
    searchToken: string,
    opts: PaginationOptions = {},
  ): Promise<{ contacts: ContactRecord[] }> {
    const { page, per_page } = opts;
    return this.http.request("POST", "/api/contacts/search", {
      search_token: searchToken,
      page: page ?? 1,
      per_page: per_page ?? 100,
    });
  }

  async tagsFilter(filter: TagFilter, opts: PaginationOptions = {}): Promise<{ contacts: ContactRecord[] }> {
    const { page, per_page } = opts;
    return this.http.request("POST", "/api/contacts/tags/search", {
      filter,
      page: page ?? 1,
      per_page: per_page ?? 100,
    });
  }

  async tagsCount(filter: TagFilter): Promise<{ count: number }> {
    return this.http.request("POST", "/api/contacts/tags/count", { filter });
  }

  async listTags(): Promise<{ tags: TagSummary[] }> {
    return this.http.request("GET", "/api/tags");
  }

  async upsertTag(payload: UpsertTagRequest): Promise<{ tag: TagRecord }> {
    return this.http.request("POST", "/api/tags", payload);
  }

  async bulkImport(
    contacts: CreateOrUpdateContactRequest[],
    overwrite = false,
  ): Promise<{ status: string }> {
    return this.http.request("POST", "/api/contacts/bulk", { contacts, overwrite });
  }

  async listByIds(ids: string[]): Promise<{ contacts: ContactRecord[] }> {
    return this.http.request("POST", "/api/contacts/batch", { ids });
  }

  acquireSortLease(ttlSeconds = 600): Promise<{ token: string; expires_at: string }> {
    return this.http.request("POST", "/api/contacts/sort-index/lease", { ttl_seconds: ttlSeconds });
  }

  releaseSortLease(token: string): Promise<{ ok: boolean }> {
    return this.http.request("DELETE", "/api/contacts/sort-index/lease", { token });
  }

  getSortRoot(field: SortField): Promise<{ root: SortRoot | null }> {
    return this.http.request("GET", `/api/contacts/sort-index/${field}/root`);
  }

  putSortRoot(
    field: SortField,
    nodeId: string,
    expectedGeneration: number,
    leaseToken?: string,
  ): Promise<{ root: SortRoot }> {
    return this.http.request("PUT", `/api/contacts/sort-index/${field}/root`, {
      node_id: nodeId,
      expected_generation: expectedGeneration,
      lease_token: leaseToken,
    });
  }

  getSortNode(field: SortField, nodeId: string): Promise<{ ciphertext: string; version: number }> {
    return this.http.request("GET", `/api/contacts/sort-index/${field}/nodes/${encodeURIComponent(nodeId)}`);
  }

  putSortNode(
    field: SortField,
    nodeId: string,
    ciphertext: string,
    expectedVersion: number,
    leaseToken?: string,
  ): Promise<{ version: number }> {
    return this.http.request("PUT", `/api/contacts/sort-index/${field}/nodes/${encodeURIComponent(nodeId)}`, {
      ciphertext,
      expected_version: expectedVersion,
      lease_token: leaseToken,
    });
  }

  writeSortNodes(
    field: SortField,
    nodes: { node_id: string; ciphertext: string }[],
    leaseToken: string,
  ): Promise<{ ok: boolean }> {
    return this.http.request("POST", `/api/contacts/sort-index/${field}/nodes`, {
      lease_token: leaseToken,
      nodes,
    });
  }

  writeSortRuns(field: SortField, runs: string[], leaseToken: string, append = false): Promise<{ ok: boolean }> {
    return this.http.request("PUT", `/api/contacts/sort-index/${field}/runs`, {
      lease_token: leaseToken,
      runs,
      append,
    });
  }

  listSortRuns(field: SortField): Promise<{ runs: { seq: number; ciphertext: string }[] }> {
    return this.http.request("GET", `/api/contacts/sort-index/${field}/runs`);
  }

  commitSortIndex(
    field: SortField,
    leaseToken: string,
    rootNodeId: string | null,
    nodeIds: string[],
  ): Promise<{ root: SortRoot | null }> {
    return this.http.request("POST", `/api/contacts/sort-index/${field}/commit`, {
      lease_token: leaseToken,
      root_node_id: rootNodeId,
      node_ids: nodeIds,
    });
  }

  async deleteTag(blindIndex: string): Promise<{ ok: boolean }> {
    return this.http.request("DELETE", `/api/tags/${encodeURIComponent(blindIndex)}`);
  }
}
