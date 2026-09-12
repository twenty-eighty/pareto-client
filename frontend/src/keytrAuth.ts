/**
 * NIP-K1 passkey auth via @sovit.xyz/keytr.
 * nsec is never persisted; kind:31777 lives on relays.
 */

import {
  addBackupGateway,
  checkPrfSupport,
  discover,
  fetchKeytrEvents,
  loginWithKeytr,
  publishKeytrEvent,
  PrfNotSupportedError,
} from "@sovit.xyz/keytr";
import { finalizeEvent } from "nostr-tools/pure";

export const KEYTR_RP_ID = "pareto.space";
export const KEYTR_RP_NAME = "Pareto";
export const KEYTR_CLIENT = "Pareto";

export const KEYTR_RELAYS = [
  "wss://portal-relay.pareto.town",
];

const INDEX_KEY = "pareto.auth.keytr.v1";
const DISMISS_KEY = "pareto.auth.keytr.dismiss.v1";

type CredentialEntry = {
  pubkey: string;
  createdAt: string;
};

/** Relay-backed presence: pubkey → kind:31777 found on Keytr relays. */
const relayPresenceCache = new Map<string, boolean>();

function normalizeHex(pubkey: string): string {
  return pubkey.toLowerCase();
}

function loadIndex(): CredentialEntry[] {
  try {
    const raw = localStorage.getItem(INDEX_KEY);
    if (!raw) {
      return [];
    }
    const parsed = JSON.parse(raw);
    return Array.isArray(parsed) ? parsed : [];
  } catch {
    return [];
  }
}

function saveIndex(entries: CredentialEntry[]): void {
  localStorage.setItem(INDEX_KEY, JSON.stringify(entries));
}

export function addToKeytrIndex(pubkey: string): void {
  const hex = normalizeHex(pubkey);
  const index = loadIndex().filter((item) => item.pubkey !== hex);
  index.push({ pubkey: hex, createdAt: new Date().toISOString() });
  saveIndex(index);
  relayPresenceCache.set(hex, true);
}

/**
 * Whether this pubkey has a Keytr passkey backup.
 * Prefers last relay check; falls back to this-browser index.
 */
export function hasKeytrCredential(pubkey: string): boolean {
  const hex = normalizeHex(pubkey);
  if (relayPresenceCache.has(hex)) {
    return relayPresenceCache.get(hex)!;
  }
  return loadIndex().some((item) => item.pubkey === hex);
}

export function indexedKeytrPubkeys(): string[] {
  return loadIndex().map((item) => item.pubkey);
}

export function anyKeytrCredentialKnown(): boolean {
  if ([...relayPresenceCache.values()].some(Boolean)) {
    return true;
  }
  return loadIndex().length > 0;
}

/**
 * Fetch kind:31777 for the given pubkeys and update the presence cache + local index.
 * Failed fetches leave prior cache/index entries unchanged.
 */
export async function refreshKeytrPresence(pubkeys: string[]): Promise<void> {
  const unique = [
    ...new Set(
      pubkeys
        .map((pubkey) => normalizeHex(String(pubkey || "").trim()))
        .filter((pubkey) => /^[0-9a-f]{64}$/.test(pubkey)),
    ),
  ];
  if (unique.length === 0) {
    return;
  }

  await Promise.all(
    unique.map(async (pubkey) => {
      try {
        const events = await fetchKeytrEvents(pubkey, KEYTR_RELAYS);
        const present = events.length > 0;
        relayPresenceCache.set(pubkey, present);
        if (present) {
          addToKeytrIndex(pubkey);
        }
      } catch {
        // Keep previous cache / local index on relay errors.
      }
    }),
  );
}

function loadDismissed(): Set<string> {
  try {
    const raw = localStorage.getItem(DISMISS_KEY);
    if (!raw) {
      return new Set();
    }
    const parsed = JSON.parse(raw);
    return new Set(Array.isArray(parsed) ? parsed.filter((item) => typeof item === "string") : []);
  } catch {
    return new Set();
  }
}

export function dismissPasskeyPrompt(pubkey: string): void {
  const set = loadDismissed();
  set.add(normalizeHex(pubkey));
  localStorage.setItem(DISMISS_KEY, JSON.stringify([...set]));
}

export function isPasskeyPromptDismissed(pubkey: string): boolean {
  return loadDismissed().has(normalizeHex(pubkey));
}

export async function reportPasskeySupport(): Promise<{
  supported: boolean;
  hasCredential: boolean;
}> {
  let supported = false;
  try {
    const info = await checkPrfSupport();
    supported = info.supported === true;
  } catch {
    supported = false;
  }
  return {
    supported,
    hasCredential: anyKeytrCredentialKnown(),
  };
}

export function shouldOfferPasskeyCreate(pubkey: string): boolean {
  const hex = normalizeHex(pubkey);
  return !hasKeytrCredential(hex) && !isPasskeyPromptDismissed(hex);
}

export function hexToNsecBytes(hex: string): Uint8Array {
  const clean = hex.replace(/^0x/, "");
  if (!/^[0-9a-fA-F]{64}$/.test(clean)) {
    throw new Error("Expected a 32-byte hex private key");
  }
  const bytes = new Uint8Array(32);
  for (let i = 0; i < 32; i++) {
    bytes[i] = parseInt(clean.slice(i * 2, i * 2 + 2), 16);
  }
  return bytes;
}

export function mapKeytrError(error: unknown): string {
  if (error instanceof PrfNotSupportedError) {
    return "This browser or authenticator does not support passkeys with PRF.";
  }
  const message = error instanceof Error ? error.message : String(error);
  const name = error instanceof Error ? error.name : "";
  if (name === "NotAllowedError" || /not allowed|aborted|cancel/i.test(message)) {
    return "Passkey was cancelled.";
  }
  if (/password.?manager|1password|bitwarden|intercept|related origin/i.test(message)) {
    return "A password-manager extension blocked the passkey. Try again after disabling it for this site.";
  }
  return message || "Passkey failed";
}

export async function loginWithPasskey(preferredPubkey?: string): Promise<{
  nsecBytes: Uint8Array;
  pubkey: string;
}> {
  const tryPubkey = async (pubkey: string) => {
    const events = await fetchKeytrEvents(pubkey, KEYTR_RELAYS);
    if (!events.length) {
      return null;
    }
    const { nsecBytes } = await loginWithKeytr(events);
    return { nsecBytes, pubkey: normalizeHex(pubkey) };
  };

  if (preferredPubkey) {
    const matched = await tryPubkey(normalizeHex(preferredPubkey));
    if (matched) {
      addToKeytrIndex(matched.pubkey);
      return matched;
    }
  }

  for (const pubkey of indexedKeytrPubkeys()) {
    if (preferredPubkey && normalizeHex(preferredPubkey) === pubkey) {
      continue;
    }
    try {
      const matched = await tryPubkey(pubkey);
      if (matched) {
        addToKeytrIndex(matched.pubkey);
        return matched;
      }
    } catch {
      // try next indexed pubkey, then discover
    }
  }

  const discovered = await discover(KEYTR_RELAYS, { rpId: KEYTR_RP_ID });
  const pubkey = normalizeHex(discovered.pubkey);
  addToKeytrIndex(pubkey);
  return { nsecBytes: discovered.nsecBytes, pubkey };
}

export async function createPasskeyForNsec(
  nsecBytes: Uint8Array,
  pubkey: string,
  displayName?: string | null,
): Promise<void> {
  const label = (displayName && displayName.trim()) || pubkey.slice(0, 16);
  const bundle = await addBackupGateway(nsecBytes, {
    rpId: KEYTR_RP_ID,
    rpName: KEYTR_RP_NAME,
    userName: label,
    userDisplayName: label,
    clientName: KEYTR_CLIENT,
  });
  const signed = finalizeEvent(
    {
      kind: bundle.eventTemplate.kind,
      content: bundle.eventTemplate.content,
      tags: bundle.eventTemplate.tags,
      created_at: bundle.eventTemplate.created_at,
    },
    nsecBytes,
  );
  await publishKeytrEvent(signed, KEYTR_RELAYS);
  addToKeytrIndex(pubkey);
}
