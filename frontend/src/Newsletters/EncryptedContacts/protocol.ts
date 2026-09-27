export async function sha256Hex(input: string): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(input));
  return toHex(new Uint8Array(digest));
}

export function toHex(bytes: Uint8Array): string {
  return Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join("");
}

export function randomSalt(): string {
  const bytes = new Uint8Array(32);
  crypto.getRandomValues(bytes);
  return toHex(bytes);
}

export function wrapStoredSalt(nip44Payload: string): string {
  return btoa(nip44Payload);
}

export function unwrapStoredSalt(stored: string): string {
  return atob(stored);
}

export function emailHashInput(salt: string, email: string): string {
  return `${salt}:${email.trim().toLowerCase()}`;
}

export function searchHashInput(salt: string, term: string): string {
  return `${salt}:search:${term.toLowerCase()}`;
}

export function tagHashInput(salt: string, tag: string): string {
  return `${salt}:tag:${tag.toLowerCase()}`;
}

export function isActiveContact(contact: {
  email?: unknown;
  Email?: unknown;
  emailAddress?: unknown;
  dnd?: unknown;
  dateUnsubscription?: unknown;
  dateunsub?: unknown;
}): boolean {
  const email = String(contact.email || contact.Email || contact.emailAddress || "").trim();
  if (!email) return false;
  if (contact.dnd === true || contact.dnd === "true" || contact.dnd === 1) return false;

  const unsubscribed = contact.dateUnsubscription ?? contact.dateunsub;
  if (
    unsubscribed !== undefined &&
    unsubscribed !== null &&
    unsubscribed !== "" &&
    unsubscribed !== 0 &&
    unsubscribed !== "0"
  ) {
    return false;
  }

  return true;
}

export function contactWithoutTags<T extends Record<string, unknown>>(contact: T): Omit<T, "tags"> {
  const { tags: _tags, ...rest } = contact;
  return rest;
}

export function searchableTerms(contact: {
  firstName?: unknown;
  lastName?: unknown;
  email?: unknown;
  company?: unknown;
  notes?: unknown;
}): string[] {
  const notes =
    typeof contact.notes === "string"
      ? contact.notes.split(/\s+/).filter((word) => word.length > 2)
      : [];

  const terms: string[] = [];
  const seen = new Set<string>();

  for (const value of [contact.firstName, contact.lastName, contact.email, contact.company, ...notes]) {
    if (typeof value !== "string") continue;

    const term = value.trim();
    const key = term.toLowerCase();
    if (!term || seen.has(key)) continue;

    seen.add(key);
    terms.push(term);
    if (terms.length === 50) break;
  }

  return terms;
}

export interface UnsignedAuthEvent {
  pubkey: string;
  created_at: number;
  kind: 22242;
  tags: string[][];
  content: "";
}

export interface SignedAuthEvent extends UnsignedAuthEvent {
  id: string;
  sig: string;
}

export async function nostrEventId(event: UnsignedAuthEvent): Promise<string> {
  const serialized = JSON.stringify([
    0,
    event.pubkey,
    event.created_at,
    event.kind,
    event.tags,
    event.content,
  ]);
  return sha256Hex(serialized);
}

export function encodeAuthHeader(event: SignedAuthEvent): string {
  return `Bearer Nostr ${btoa(JSON.stringify(event))}`;
}
