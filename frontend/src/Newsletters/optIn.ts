/** Public opt-in on the queue. Ciphertext only; the courier renders the confirmation. */

const LOCAL_QUEUE_API_URL = "http://localhost:4433/v1";
const HOSTED_QUEUE_API_URL = "https://queue-server.pareto.space/v1";

/** Local pages talk to the dev queue. Every other host uses the hosted queue. */
export function queueApiBaseUrl(): string {
  const host = typeof window !== "undefined" ? window.location.hostname : "";
  if (host === "localhost" || host === "127.0.0.1") {
    return LOCAL_QUEUE_API_URL;
  }
  return HOSTED_QUEUE_API_URL;
}

/** Platform courier. Newsletter jobs encrypt to this key as well. */
export const COURIER_PUBKEY = "cefbf43addd677426c671d7cd275289be35f7b6b398fced7fae420d060e7a345";

const OPT_IN_TIMEOUT_MS = 20_000;
const HEX_64 = /^[0-9a-fA-F]{64}$/;
const MAILBOX = /^[^\s@<>",;]+@[^\s@<>",;]+\.[^\s@<>",;]+$/;

export type OptInStatus = "SENT" | "QUEUED" | "REJECTED";

export type OptInInput = {
  email?: unknown;
  author?: unknown;
  firstName?: unknown;
  lastName?: unknown;
  pubkey?: unknown;
  locale?: unknown;
  test?: unknown;
};

export type OptInPayload = {
  email: string;
  author: string;
  test: boolean;
  firstName?: string;
  lastName?: string;
  pubkey?: string;
  locale?: string;
};

export type OptInOutcome =
  | { ok: true; jobId: string; status: OptInStatus }
  | { ok: false; message: string };

export function buildOptInPayload(input: OptInInput): { ok: true; payload: OptInPayload } | { ok: false; message: string } {
  const email = text(input.email);
  const author = text(input.author);
  if (!email || !MAILBOX.test(email)) {
    return { ok: false, message: "Enter a valid email address" };
  }
  if (!author || !HEX_64.test(author)) {
    return { ok: false, message: "Missing newsletter author" };
  }

  const payload: OptInPayload = {
    email,
    author,
    test: input.test === true,
  };
  const firstName = text(input.firstName);
  const lastName = text(input.lastName);
  const pubkey = text(input.pubkey);
  const locale = text(input.locale);
  if (firstName) payload.firstName = firstName;
  if (lastName) payload.lastName = lastName;
  if (pubkey && HEX_64.test(pubkey)) payload.pubkey = pubkey;
  if (locale) payload.locale = locale;
  return { ok: true, payload };
}

export function optInEnvelope(ciphertext: string, kid: string, test: boolean, idem?: string) {
  const envelope: {
    namespace: "prod" | "test";
    idem?: string;
    ciphertext_b64: string;
    enc: { alg: "nip44"; kid: string };
    size_bytes: number;
  } = {
    namespace: test ? "test" : "prod",
    ciphertext_b64: ciphertext,
    enc: { alg: "nip44", kid },
    size_bytes: utf8ByteLength(ciphertext),
  };
  const token = text(idem);
  if (token && token.length <= 128) envelope.idem = token;
  return envelope;
}

export function interpretOptInResponse(status: number, body: unknown): OptInOutcome {
  if (status === 200 || status === 202) {
    const record = objectOf(body);
    const result = record?.status;
    const jobId = text(record?.job_id);
    if ((result === "SENT" || result === "QUEUED" || result === "REJECTED") && jobId) {
      return { ok: true, jobId, status: result };
    }
    return { ok: false, message: "Unexpected response from the queue server" };
  }

  const message = errorMessage(body);
  if (status === 429) return { ok: false, message: message || "Too many opt-in requests" };
  if (status === 400) return { ok: false, message: message || "The subscription could not be accepted" };
  return { ok: false, message: message || `Queue server returned ${status}` };
}

export async function submitOptIn(input: {
  payload: OptInPayload;
  ciphertext: string;
  kid: string;
  requestId?: string;
  baseUrl?: string;
  fetchImpl?: typeof fetch;
}): Promise<OptInOutcome> {
  if (!input.ciphertext) {
    return { ok: false, message: "Failed to encrypt the subscription" };
  }
  if (!HEX_64.test(input.kid)) {
    return { ok: false, message: "Missing encryption key" };
  }

  const envelope = optInEnvelope(input.ciphertext, input.kid, input.payload.test, input.requestId);
  const url = `${(input.baseUrl || queueApiBaseUrl()).replace(/\/+$/, "")}/opt-in`;

  let response: Response;
  try {
    const fetchImpl = input.fetchImpl || fetch;
    response = await fetchImpl(url, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(envelope),
      signal: AbortSignal.timeout(OPT_IN_TIMEOUT_MS),
    });
  } catch (error) {
    const message = error instanceof Error ? error.message : "Queue server request failed";
    return { ok: false, message };
  }

  const body = await response.json().catch(() => null);
  return interpretOptInResponse(response.status, body);
}

function text(value: unknown): string | undefined {
  if (typeof value !== "string") return undefined;
  const trimmed = value.trim();
  return trimmed || undefined;
}

function utf8ByteLength(value: string): number {
  return new TextEncoder().encode(value).length;
}

function objectOf(value: unknown): Record<string, unknown> | null {
  if (!value || typeof value !== "object" || Array.isArray(value)) return null;
  return value as Record<string, unknown>;
}

function errorMessage(body: unknown): string | undefined {
  const error = objectOf(objectOf(body)?.error);
  return text(error?.message);
}
