export interface PaginationOptions {
    page?: number;
    per_page?: number;
}
export interface ContactRecord {
    id: string;
    encrypted_data: string;
    version: number;
    inserted_at: string;
    updated_at: string;
}
export interface CreateOrUpdateContactRequest {
    encrypted_data: string;
    email_hash: string;
    search_tokens?: string[];
    tag_hashes?: string[];
    version?: number;
}
export type TagFilter = {
    any: string[];
} | {
    all: string[];
} | {
    not: TagFilter;
} | {
    and: TagFilter[];
} | {
    or: TagFilter[];
};
export interface TagSummary {
    blind_index: string;
    ciphertext_tag: string;
    key_version: number;
    count: number;
}
export interface TagRecord {
    id: string;
    blind_index: string;
    ciphertext_tag: string;
    key_version: number;
}
export interface UpsertTagRequest {
    blind_index: string;
    ciphertext_tag: string;
    key_version?: number;
}
