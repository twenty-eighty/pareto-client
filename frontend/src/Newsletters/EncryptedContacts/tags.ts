import type { HttpClient } from './http'
import type { TagSummary, UpsertTagRequest, TagRecord } from './types'

/**
 * Tags API: list and upsert blind index tags for the authenticated user.
 */
export class TagsApi {
  private readonly http: HttpClient

  constructor(http: HttpClient) {
    this.http = http
  }

  /** List user-scoped tags with counts and ciphertexts. */
  async list(): Promise<{ tags: TagSummary[] }> {
    return await this.http.request('GET', '/api/tags')
  }

  /** Upsert a tag blind index and ciphertext. */
  async upsert(payload: UpsertTagRequest): Promise<{ tag: TagRecord }> {
    return await this.http.request('POST', '/api/tags', payload)
  }
}
