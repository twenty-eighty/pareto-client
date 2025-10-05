import { HttpClient } from "./http";
import type { TagSummary, UpsertTagRequest, TagRecord } from "./types";
/**
 * Tags API: list and upsert blind index tags for the authenticated user.
 */
export declare class TagsApi {
    private readonly http;
    constructor(http: HttpClient);
    /** List user-scoped tags with counts and ciphertexts. */
    list(): Promise<{
        tags: TagSummary[];
    }>;
    /** Upsert a tag blind index and ciphertext. */
    upsert(payload: UpsertTagRequest): Promise<{
        tag: TagRecord;
    }>;
}
