import { HttpClient } from "./http";
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

  async deleteTag(blindIndex: string): Promise<{ ok: boolean }> {
    return this.http.request("DELETE", `/api/tags/${encodeURIComponent(blindIndex)}`);
  }
}
