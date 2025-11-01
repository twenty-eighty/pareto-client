import { HttpClient } from "./http";
import type { PaginationOptions, ContactRecord, CreateOrUpdateContactRequest, TagFilter, TagSummary, UpsertTagRequest, TagRecord } from "./types";
/**
 * Contacts API: create, list, show, update, delete, and search/filter.
 */
export declare class ContactsApi {
    private readonly http;
    constructor(http: HttpClient);
    /** Create a new contact. Frontend must provide encrypted fields and hashes. */
    create(contact: CreateOrUpdateContactRequest): Promise<{
        id: string;
    }>;
    /** List contacts (paginated). */
    list(opts?: PaginationOptions): Promise<{
        contacts: ContactRecord[];
    }>;
    /** Count total contacts for the authenticated user. */
    count(): Promise<{
        count: number;
    }>;
    /** Get one contact by id. */
    show(id: string): Promise<ContactRecord>;
    /** Update a contact by id. */
    update(id: string, contact: CreateOrUpdateContactRequest): Promise<{
        id: string;
    }>;
    /** Delete a contact by id. */
    delete(id: string): Promise<{
        ok: boolean;
    }>;
    /** Search by encrypted search token (deterministic). */
    searchByToken(search_token: string, opts?: PaginationOptions): Promise<{
        contacts: ContactRecord[];
    }>;
    /** Filter by tag hashes using the tag filter DSL. */
    tagsFilter(filter: TagFilter, opts?: PaginationOptions): Promise<{
        contacts: ContactRecord[];
    }>;
    /** Count contacts that match a tag filter DSL. */
    tagsCount(filter: TagFilter): Promise<{
        count: number;
    }>;
    /** List user-scoped tags with counts and ciphertext. */
    listTags(): Promise<{
        tags: TagSummary[];
    }>;
    /** Upsert a tag blind index and ciphertext. */
    upsertTag(payload: UpsertTagRequest): Promise<{
        tag: TagRecord;
    }>;
    /** Delete a tag by blind index. */
    deleteTag(blindIndex: string): Promise<{
        ok: boolean;
    }>;
}
