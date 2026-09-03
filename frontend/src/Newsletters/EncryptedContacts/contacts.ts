import type { HttpClient } from './http'
import type {
  PaginationOptions,
  ContactRecord,
  CreateOrUpdateContactRequest,
  TagFilter,
  TagSummary,
  UpsertTagRequest,
  TagRecord,
} from './types'

/**
 * Contacts API: create, list, show, update, delete, and search/filter.
 */
export class ContactsApi {
  private readonly http: HttpClient

  constructor(http: HttpClient) {
    this.http = http
  }

  /** Create a new contact. Frontend must provide encrypted fields and hashes. */
  async create(contact: CreateOrUpdateContactRequest): Promise<{ id: string }> {
    return await this.http.request('POST', '/api/contacts', { contact })
  }

  /** List contacts (paginated). */
  async list(opts: PaginationOptions = {}): Promise<{ contacts: ContactRecord[] }> {
    const { page, per_page } = opts
    return await this.http.request('GET', '/api/contacts', undefined, {
      page: page ?? 1,
      per_page: per_page ?? 100,
    })
  }

  /** Count total contacts for the authenticated user. */
  async count(): Promise<{ count: number }> {
    return await this.http.request('GET', '/api/contacts/count')
  }

  /** Get one contact by id. */
  async show(id: string): Promise<ContactRecord> {
    return await this.http.request('GET', `/api/contacts/${encodeURIComponent(id)}`)
  }

  /** Update a contact by id. */
  async update(id: string, contact: CreateOrUpdateContactRequest): Promise<{ id: string }> {
    return await this.http.request('PUT', `/api/contacts/${encodeURIComponent(id)}`, { contact })
  }

  /** Delete a contact by id. */
  async delete(id: string): Promise<{ ok: boolean }> {
    return await this.http.request('DELETE', `/api/contacts/${encodeURIComponent(id)}`)
  }

  /** Search by encrypted search token (deterministic). */
  async searchByToken(
    search_token: string,
    opts: PaginationOptions = {}
  ): Promise<{ contacts: ContactRecord[] }> {
    const { page, per_page } = opts
    return await this.http.request('GET', '/api/contacts/search', undefined, {
      search_token,
      page: page ?? 1,
      per_page: per_page ?? 100,
    })
  }

  /** Filter by tag hashes using the tag filter DSL. */
  async tagsFilter(
    filter: TagFilter,
    opts: PaginationOptions = {}
  ): Promise<{ contacts: ContactRecord[] }> {
    const { page, per_page } = opts
    return await this.http.request('POST', '/api/contacts/tags/search', {
      filter,
      page: page ?? 1,
      per_page: per_page ?? 100,
    })
  }

  /** Count contacts that match a tag filter DSL. */
  async tagsCount(filter: TagFilter): Promise<{ count: number }> {
    return await this.http.request('POST', '/api/contacts/tags/count', { filter })
  }

  /** List user-scoped tags with counts and ciphertext. */
  async listTags(): Promise<{ tags: TagSummary[] }> {
    return await this.http.request('GET', '/api/tags')
  }

  /** Upsert a tag blind index and ciphertext. */
  async upsertTag(payload: UpsertTagRequest): Promise<{ tag: TagRecord }> {
    return await this.http.request('POST', '/api/tags', payload)
  }

  /** Delete a tag by blind index. */
  async deleteTag(blindIndex: string): Promise<{ ok: boolean }> {
    return await this.http.request('DELETE', `/api/tags/${encodeURIComponent(blindIndex)}`)
  }
}
