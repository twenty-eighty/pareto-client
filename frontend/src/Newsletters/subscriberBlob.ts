/**
 * Load the NIP-96 subscriber JSON blob locally and return active contacts.
 * The blob stays a client-side store; only decrypted contacts go to the queue server.
 */

const SUBSCRIBERS_D_TAG = "pareto-subscribers";
const APPLICATION_SPECIFIC_KIND = 30078;

export type Subscriber = {
  email?: string;
  Email?: string;
  emailAddress?: string;
  dnd?: boolean | string | number | null;
  dateUnsubscription?: number | string | null;
  dateunsub?: number | string | null;
  firstName?: string | null;
  lastName?: string | null;
  pubKey?: string | null;
  pubkey?: string | null;
  locale?: string | null;
  tags?: string[] | null;
  [key: string]: unknown;
};

type SubscriberEventData = {
  key: string;
  iv: string;
  url: string;
};

function hexToBytes(hex: string): Uint8Array {
  const clean = hex.replace(/^0x/, "");
  const bytes = new Uint8Array(clean.length / 2);
  for (let i = 0; i < bytes.length; i++) {
    bytes[i] = parseInt(clean.slice(i * 2, i * 2 + 2), 16);
  }
  return bytes;
}

function subscriberEmail(subscriber: Subscriber): string {
  return String(subscriber.email || subscriber.Email || subscriber.emailAddress || "").trim();
}

function isUnsubscribed(subscriber: Subscriber): boolean {
  const value = subscriber.dateUnsubscription ?? subscriber.dateunsub;
  return value !== undefined && value !== null && value !== "" && value !== 0 && value !== "0";
}

function isDnd(subscriber: Subscriber): boolean {
  return subscriber.dnd === true || subscriber.dnd === "true" || subscriber.dnd === 1;
}

export function isActiveSubscriber(subscriber: Subscriber): boolean {
  return Boolean(subscriberEmail(subscriber)) && !isDnd(subscriber) && !isUnsubscribed(subscriber);
}

export function activeSubscribers(subscribers: Subscriber[]): Subscriber[] {
  return subscribers.filter(isActiveSubscriber);
}

async function decryptBlob(url: string, keyHex: string, ivHex: string, signal?: AbortSignal): Promise<unknown> {
  const response = await fetch(url, signal ? { signal } : undefined);
  if (!response.ok) {
    throw new Error(`Failed to download subscriber list: ${response.status}`);
  }
  const encryptedBuffer = await response.arrayBuffer();
  const cryptoKey = await crypto.subtle.importKey(
    "raw",
    hexToBytes(keyHex),
    { name: "AES-GCM" },
    false,
    ["decrypt"],
  );
  const decrypted = await crypto.subtle.decrypt(
    { name: "AES-GCM", iv: hexToBytes(ivHex) },
    cryptoKey,
    encryptedBuffer,
  );
  return JSON.parse(new TextDecoder().decode(decrypted));
}

function parseSubscriberEventContent(content: string): SubscriberEventData | null {
  try {
    const parsed = JSON.parse(content);
    if (parsed?.key && parsed?.iv && parsed?.url) {
      return { key: parsed.key, iv: parsed.iv, url: parsed.url };
    }
  } catch {
    return null;
  }
  return null;
}

export type SubscriberBlobPointer = {
  url: string;
  key: string;
  iv: string;
};

function subscribersFromDecoded(decoded: unknown): Subscriber[] {
  const subscribers = Array.isArray((decoded as { subscribers?: Subscriber[] })?.subscribers)
    ? (decoded as { subscribers: Subscriber[] }).subscribers
    : Array.isArray(decoded)
      ? (decoded as Subscriber[])
      : [];
  return activeSubscribers(subscribers);
}

export async function loadActiveSubscribersFromPointer(
  pointer: SubscriberBlobPointer,
  signal?: AbortSignal,
): Promise<Subscriber[]> {
  const decoded = await decryptBlob(pointer.url, pointer.key, pointer.iv, signal);
  return subscribersFromDecoded(decoded);
}

export async function loadActiveSubscribers(ndk: any, pubkey: string): Promise<Subscriber[]> {
  if (!ndk) {
    throw new Error("NDK is not initialized");
  }
  const events = await ndk.fetchEvents({
    kinds: [APPLICATION_SPECIFIC_KIND],
    authors: [pubkey],
    "#d": [SUBSCRIBERS_D_TAG],
  });
  const list = Array.from(events ?? []);
  const pointer = list
    .map((event: { content?: string }) => parseSubscriberEventContent(event.content || ""))
    .find((item): item is SubscriberEventData => item !== null);
  if (!pointer) {
    return [];
  }
  return loadActiveSubscribersFromPointer(pointer);
}
