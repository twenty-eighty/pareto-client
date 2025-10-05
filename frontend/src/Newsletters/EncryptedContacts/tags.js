/**
 * Tags API: list and upsert blind index tags for the authenticated user.
 */
export class TagsApi {
    constructor(http) {
        this.http = http;
    }
    /** List user-scoped tags with counts and ciphertexts. */
    async list() {
        return await this.http.request("GET", "/api/tags");
    }
    /** Upsert a tag blind index and ciphertext. */
    async upsert(payload) {
        return await this.http.request("POST", "/api/tags", payload);
    }
}
