import type { ContactInput } from "./EncryptedContacts/types";

export interface SubscriptionChange {
  kind: "subscribe" | "unsubscribe";
  at: number;
  subscriber: Record<string, unknown>;
}

export interface SubscriptionContact {
  id?: string;
  tags?: string[];
  [extra: string]: unknown;
}

export interface SubscriptionContactsApi {
  searchContacts(
    searchTerm: string,
    page?: number,
    perPage?: number,
  ): Promise<{ contacts: SubscriptionContact[] }>;
  updateContact(id: string, contact: ContactInput): Promise<unknown>;
  storeContactsBulk(contacts: ContactInput[], overwrite?: boolean): Promise<unknown>;
}

export function parseSubscriptionChanges(value: unknown): SubscriptionChange[] {
  if (!Array.isArray(value)) return [];

  const changes: SubscriptionChange[] = [];
  for (const item of value) {
    if (!item || typeof item !== "object") continue;
    const record = item as Record<string, unknown>;
    if (record.kind !== "subscribe" && record.kind !== "unsubscribe") continue;
    const at = Number(record.at);
    if (!Number.isFinite(at)) continue;
    const subscriber =
      record.subscriber && typeof record.subscriber === "object"
        ? (record.subscriber as Record<string, unknown>)
        : {};
    changes.push({ kind: record.kind, at, subscriber });
  }
  return changes;
}

export function latestChanges(changes: SubscriptionChange[]): SubscriptionChange[] {
  const byEmail = new Map<string, SubscriptionChange>();

  for (const change of changes) {
    const email = changeEmail(change);
    if (!email) continue;
    const current = byEmail.get(email);
    if (!current || isNewer(change, current)) byEmail.set(email, change);
  }

  return [...byEmail.values()];
}

export function contactTime(value: unknown): number | null {
  if (value === undefined || value === null || value === "" || value === 0 || value === "0") return null;
  const time = typeof value === "number" ? value : Number(value);
  if (!Number.isFinite(time) || time <= 0) return null;
  return time;
}

export function shouldApply(change: SubscriptionChange, contact: SubscriptionContact | null): boolean {
  if (!changeEmail(change) || !Number.isFinite(change.at)) return false;
  if (!contact) return true;

  const subscribedAt = contactTime(contact.datesub ?? contact.dateSubscription) ?? 0;
  const unsubscribedAt = contactTime(contact.dateunsub ?? contact.dateUnsubscription);

  if (change.kind === "unsubscribe") {
    return change.at > subscribedAt && (unsubscribedAt === null || change.at > unsubscribedAt);
  }

  if (unsubscribedAt !== null && change.at > unsubscribedAt) return true;
  return unsubscribedAt === null && change.at > subscribedAt;
}

export async function syncSubscriptionEvents(
  api: SubscriptionContactsApi,
  changes: SubscriptionChange[],
  apply: boolean,
): Promise<{ pending: string[]; applied: number | null }> {
  const pending: string[] = [];
  let applied = 0;

  for (const change of latestChanges(changes)) {
    const email = String(change.subscriber.email || "").trim();
    const existing = await findContactByEmail(api, email);
    if (!shouldApply(change, existing)) continue;

    pending.push(email);
    if (!apply) continue;

    const next = change.kind === "unsubscribe" ? unsubscribedContact(existing, change) : subscribedContact(existing, change);
    if (existing?.id) {
      await api.updateContact(existing.id, next);
    } else {
      await api.storeContactsBulk([next], false);
    }
    applied += 1;
  }

  return { pending: apply ? [] : pending, applied: apply ? applied : null };
}

async function findContactByEmail(
  api: SubscriptionContactsApi,
  email: string,
): Promise<SubscriptionContact | null> {
  const wanted = email.trim().toLowerCase();
  if (!wanted) return null;

  const page = await api.searchContacts(wanted, 1, 100);
  return (
    page.contacts.find((contact) => String(contact.email || "").trim().toLowerCase() === wanted) || null
  );
}

function subscribedContact(existing: SubscriptionContact | null, change: SubscriptionChange): ContactInput {
  const incoming = change.subscriber;
  return {
    ...contactFields(existing),
    email: String(incoming.email || existing?.email || "").trim(),
    firstName: textOrExisting(existing?.firstName, incoming.firstName),
    lastName: textOrExisting(existing?.lastName, incoming.lastName),
    locale: textOrExisting(existing?.locale, incoming.locale),
    pubkey: textOrExisting(existing?.pubkey, incoming.pubkey),
    source: textOrExisting(existing?.source, "opt-in"),
    datesub: change.at,
    dateunsub: null,
    dateUnsubscription: null,
    dnd: false,
    tags: contactTags(existing, incoming),
  };
}

function unsubscribedContact(existing: SubscriptionContact | null, change: SubscriptionChange): ContactInput {
  const incoming = change.subscriber;
  return {
    ...contactFields(existing),
    email: String(incoming.email || existing?.email || "").trim(),
    datesub: contactTime(existing?.datesub ?? existing?.dateSubscription) ?? change.at,
    dateunsub: change.at,
    dateUnsubscription: null,
    dnd: true,
    tags: contactTags(existing, incoming),
  };
}

function contactFields(contact: SubscriptionContact | null): Record<string, unknown> {
  if (!contact) return {};
  const { id: _id, tags: _tags, active: _active, dateunsub: _dateunsub, dateUnsubscription: _dateUnsubscription, ...rest } =
    contact;
  return rest;
}

function contactTags(existing: SubscriptionContact | null, incoming: Record<string, unknown>): string[] {
  if (Array.isArray(existing?.tags)) return existing.tags.filter((tag) => typeof tag === "string");
  if (Array.isArray(incoming.tags)) return incoming.tags.filter((tag) => typeof tag === "string");
  return [];
}

function textOrExisting(current: unknown, incoming: unknown): string | undefined {
  if (typeof current === "string" && current.trim()) return current;
  if (typeof incoming === "string" && incoming.trim()) return incoming;
  return undefined;
}

function changeEmail(change: SubscriptionChange): string {
  return String(change.subscriber.email || "").trim().toLowerCase();
}

function isNewer(next: SubscriptionChange, current: SubscriptionChange): boolean {
  if (next.at !== current.at) return next.at > current.at;
  return next.kind === "unsubscribe" && current.kind !== "unsubscribe";
}
