/**
 * Contacts API: create, list, show, update, delete, and search/filter.
 */
export class ContactsApi {
    constructor(http) {
        this.http = http;
    }
    /** Create a new contact. Frontend must provide encrypted fields and hashes. */
    async create(contact) {
        const res = await this.http.request("POST", "/api/contacts", { contact });
        return res;
    }
    /** List contacts (paginated). */
    async list(opts = {}) {
        const { page, per_page } = opts;
        return await this.http.request("GET", "/api/contacts", undefined, { page: page ?? 1, per_page: per_page ?? 100 });
    }
    /** Count total contacts for the authenticated user. */
    async count() {
        return await this.http.request("GET", "/api/contacts/count");
    }
    /** Get one contact by id. */
    async show(id) {
        return await this.http.request("GET", `/api/contacts/${encodeURIComponent(id)}`);
    }
    /** Update a contact by id. */
    async update(id, contact) {
        return await this.http.request("PUT", `/api/contacts/${encodeURIComponent(id)}`, { contact });
    }
    /** Delete a contact by id. */
    async delete(id) {
        return await this.http.request("DELETE", `/api/contacts/${encodeURIComponent(id)}`);
    }
    /** Search by encrypted search token (deterministic). */
    async searchByToken(search_token, opts = {}) {
        const { page, per_page } = opts;
        return await this.http.request("GET", "/api/contacts/search", undefined, { search_token, page: page ?? 1, per_page: per_page ?? 100 });
    }
    /** Filter by tag hashes using the tag filter DSL. */
    async tagsFilter(filter, opts = {}) {
        const { page, per_page } = opts;
        return await this.http.request("POST", "/api/contacts/tags/search", { filter, page: page ?? 1, per_page: per_page ?? 100 });
    }
    /** Count contacts that match a tag filter DSL. */
    async tagsCount(filter) {
        return await this.http.request("POST", "/api/contacts/tags/count", { filter });
    }
    // ========== TAGS (consolidated) ==========
    /** List user-scoped tags with counts and ciphertext. */
    async listTags() {
        return await this.http.request("GET", "/api/tags");
    }
    /** Upsert a tag blind index and ciphertext. */
    async upsertTag(payload) {
        return await this.http.request("POST", "/api/tags", payload);
    }
    /** Delete a tag by blind index. */
    async deleteTag(blindIndex) {
        return await this.http.request("DELETE", `/api/tags/${encodeURIComponent(blindIndex)}`);
    }
}
