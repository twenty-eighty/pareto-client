export interface PaginationOptions {
  page?: number;
  per_page?: number;
}

export interface ContactTag {
  blind_index: string;
  ciphertext_tag: string | null;
  key_version: number | null;
}

export interface ContactRecord {
  id: string;
  encrypted_data: string;
  version: number;
  active: boolean;
  inserted_at: string;
  updated_at: string;
  tags: ContactTag[];
}

export interface CreateOrUpdateContactRequest {
  encrypted_data: string;
  email_hash: string;
  search_tokens?: string[];
  tag_hashes?: string[];
  version?: number;
  active?: boolean;
}

export type TagFilter =
  | { any: string[] }
  | { all: string[] }
  | { not: TagFilter }
  | { and: TagFilter[] }
  | { or: TagFilter[] };

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

export interface ContactInput {
  email: string;
  firstName?: string;
  lastName?: string;
  phone?: string;
  company?: string;
  notes?: string;
  tags?: string[];
  Email?: string;
  emailAddress?: string;
  dnd?: boolean | string | number | null;
  dateUnsubscription?: number | string | null;
  dateunsub?: number | string | null;
  [extra: string]: unknown;
}

export interface DecryptedContact {
  id: string;
  tags: string[];
  email?: string;
  firstName?: string;
  lastName?: string;
  phone?: string;
  company?: string;
  notes?: string;
  active?: boolean;
  [extra: string]: unknown;
}

export type TagNameFilter =
  | { any: string[] }
  | { all: string[] }
  | { not: TagNameFilter }
  | { and: TagNameFilter[] }
  | { or: TagNameFilter[] };

export interface LoginResponse {
  token: string;
  user_id: string;
  salt: string;
}

export interface ChallengeResponse {
  challenge: string;
}
