/** Matches the deliverability server's classify limit. */
export const CLASSIFY_BATCH_SIZE = 100;

export type ClassifyStatus = "undeliverable" | "blocked" | "valid" | "uncertain" | "unknown";

const HOSTED_DELIVERABILITY_URL = "https://deliverability.pareto.space";

/** Classify against the deployed server, including when the page is served locally. */
export function deliverabilityApiBaseUrl(): string {
  return HOSTED_DELIVERABILITY_URL;
}

export function classifiableEmail(email: unknown): string | null {
  const normalized = String(email ?? "").trim().toLowerCase();
  const parts = normalized.split("@");
  if (parts.length !== 2) return null;
  const [local, domain] = parts;
  if (!local || !domain || !domain.includes(".")) return null;
  if (/[ ,<>]/.test(normalized)) return null;
  return normalized;
}

function isUndeliverable(value: unknown): boolean {
  return value === true || value === "true" || value === 1 || value === "1";
}

async function sha256Hex(input: string): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(input));
  return Array.from(new Uint8Array(digest), (byte) => byte.toString(16).padStart(2, "0")).join("");
}

/** Stay under the server's 30 requests per 60-second window. */
export const CLASSIFY_INTERVAL_MS = 2_100;

export interface ClassifyResult {
  status: ClassifyStatus;
  reason?: string;
}

export interface CleanableContact {
  id: string;
  email?: unknown;
  Email?: unknown;
  emailAddress?: unknown;
  undeliverable?: unknown;
  verifiedAt?: unknown;
  reason?: unknown;
  active?: unknown;
  [extra: string]: unknown;
}

export interface CleanContactsApi {
  getContacts(page?: number, perPage?: number): Promise<{
    contacts: CleanableContact[];
    sourceCount: number;
    errors?: string[];
  }>;
  updateContact(id: string, contact: CleanableContact): Promise<unknown>;
}

export interface CleanProgress {
  checked: number;
  updated: number;
  undeliverable: number;
  done: boolean;
  cancelled?: boolean;
  error?: string;
}

function contactEmail(contact: CleanableContact): unknown {
  return contact.email || contact.Email || contact.emailAddress;
}

function hasVerifiedAt(value: unknown): boolean {
  return typeof value === "number" && Number.isFinite(value) && value > 0;
}

function storedReason(value: unknown): string | undefined {
  return typeof value === "string" && value.trim() ? value.trim() : undefined;
}

/** Reason is kept for undeliverable, blocked, and uncertain. Valid and unknown stay status-only. */
function reasonFor(result: ClassifyResult): string | undefined {
  if (result.status === "valid" || result.status === "unknown") return undefined;
  return storedReason(result.reason);
}

/**
 * Cache fields written onto the encrypted contact after classify.
 * A repeat pass skips a contact whose flag, reason, and verifiedAt already match.
 */
export function applyDeliverability(
  contact: CleanableContact,
  result: ClassifyResult,
  now: number,
): { contact: CleanableContact; changed: boolean } {
  const next: CleanableContact = { ...contact };
  const status = result.status;
  const flagged = isUndeliverable(contact.undeliverable);
  const verified = hasVerifiedAt(contact.verifiedAt);
  const currentReason = storedReason(contact.reason);
  const nextReason = reasonFor(result);
  const reasonChanged = currentReason !== nextReason;

  if (nextReason) next.reason = nextReason;
  else delete next.reason;

  if (status === "unknown") {
    delete next.undeliverable;
    delete next.verifiedAt;
    return { contact: next, changed: flagged || verified || reasonChanged };
  }

  if (status === "undeliverable") {
    next.undeliverable = true;
    if (!verified) next.verifiedAt = now;
    const inactive = contact.active === false;
    return { contact: next, changed: !flagged || !inactive || !verified || reasonChanged };
  }

  delete next.undeliverable;
  if (!verified) next.verifiedAt = now;
  return { contact: next, changed: flagged || !verified || reasonChanged };
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Walk the contact database one page at a time. The page is released before the next
 * fetch, so the browser holds one batch rather than the whole list.
 */
export async function cleanContactList(options: {
  api: CleanContactsApi;
  classify: (emails: string[]) => Promise<ClassifyResult[]>;
  now?: () => number;
  wait?: (ms: number) => Promise<void>;
  isCancelled?: () => boolean;
  onProgress?: (progress: CleanProgress) => void;
  pageSize?: number;
}): Promise<CleanProgress> {
  const pageSize = options.pageSize ?? CLASSIFY_BATCH_SIZE;
  if (pageSize < 1 || pageSize > CLASSIFY_BATCH_SIZE) {
    throw new Error("batch_too_large");
  }
  const now = options.now || Date.now;
  const wait = options.wait || sleep;
  const isCancelled = options.isCancelled || (() => false);
  const progress: CleanProgress = { checked: 0, updated: 0, undeliverable: 0, done: false };
  let lastClassifyAt = 0;
  let readErrors = 0;

  const report = (done: boolean) => {
    options.onProgress?.({ ...progress, done });
  };

  try {
    let page = 1;
    while (!isCancelled()) {
      const result = await options.api.getContacts(page, pageSize);
      readErrors += result.errors?.length || 0;
      const contacts = result.contacts || [];
      await classifyPage(contacts);
      if (isCancelled() || (result.sourceCount || 0) < pageSize) break;
      page += 1;
    }

    progress.done = true;
    if (isCancelled()) {
      progress.cancelled = true;
    } else if (readErrors > 0) {
      progress.error = `Could not read ${readErrors} contacts`;
    }
    report(true);
    return { ...progress };
  } catch (error) {
    progress.done = true;
    progress.error = error instanceof Error ? error.message : "Failed to clean contacts";
    report(true);
    return { ...progress };
  }

  async function classifyPage(contacts: CleanableContact[]): Promise<void> {
    const batch: CleanableContact[] = [];
    const emails: string[] = [];
    for (const contact of contacts) {
      progress.checked += 1;
      const email = classifiableEmail(contactEmail(contact));
      if (!email || !contact.id) continue;
      batch.push(contact);
      emails.push(email);
    }
    report(false);
    if (emails.length === 0 || isCancelled()) return;

    const elapsed = Date.now() - lastClassifyAt;
    if (lastClassifyAt && elapsed < CLASSIFY_INTERVAL_MS) {
      await wait(CLASSIFY_INTERVAL_MS - elapsed);
    }
    if (isCancelled()) return;

    const statuses = await options.classify(emails);
    lastClassifyAt = Date.now();
    if (statuses.length !== batch.length) {
      throw new Error("invalid_batch");
    }

    for (let index = 0; index < batch.length; index++) {
      if (isCancelled()) return;
      const status = statuses[index];
      if (status.status === "undeliverable") progress.undeliverable += 1;
      const patched = applyDeliverability(batch[index], status, now());
      if (!patched.changed) continue;
      const email = String(contactEmail(batch[index]) || "").trim();
      await options.api.updateContact(batch[index].id, { ...patched.contact, email });
      progress.updated += 1;
      report(false);
    }
  }
}

export interface ClassifyRequest {
  url: string;
  method: "POST";
  payloadSha256: string;
}

type FetchLike = (url: string, init: { method: string; headers: Record<string, string>; body: string }) => Promise<Response>;

const STATUSES = new Set<ClassifyStatus>(["undeliverable", "blocked", "valid", "uncertain", "unknown"]);

function isStatus(value: unknown): value is ClassifyStatus {
  return typeof value === "string" && STATUSES.has(value as ClassifyStatus);
}

function classifyResult(item: { status?: unknown; reason?: unknown } | undefined): ClassifyResult | null {
  if (!item || !isStatus(item.status)) return null;
  if (item.status === "valid" || item.status === "unknown") return { status: item.status };
  const reason = storedReason(item.reason);
  return reason ? { status: item.status, reason } : { status: item.status };
}

/**
 * Ask the deliverability server what it already knows about these addresses.
 * `refresh` stays false: a paid re-check is refused until list-cleaning payment exists.
 * The body is hashed for the NIP-98 payload tag and sent unchanged.
 */
export async function classifyEmails(options: {
  baseUrl: string;
  pubkey: string;
  emails: string[];
  sign: (request: ClassifyRequest) => Promise<string>;
  fetchImpl?: FetchLike;
  wait?: (ms: number) => Promise<void>;
}): Promise<ClassifyResult[]> {
  const pubkey = options.pubkey.trim().toLowerCase();
  if (!/^[0-9a-f]{64}$/.test(pubkey)) {
    throw new Error("invalid_author");
  }
  if (options.emails.length > CLASSIFY_BATCH_SIZE) {
    throw new Error("batch_too_large");
  }

  const emails = options.emails.map((email) => {
    const normalized = classifiableEmail(email);
    if (!normalized) throw new Error("invalid_email");
    return normalized;
  });

  const body = JSON.stringify({ namespace: "prod", emails, refresh: false });
  const url = `${options.baseUrl.replace(/\/+$/, "")}/v1/authors/${pubkey}/classify`;
  const authorization = await options.sign({
    url,
    method: "POST",
    payloadSha256: await sha256Hex(body),
  });
  const fetchImpl = options.fetchImpl || fetch;
  const wait = options.wait || sleep;

  for (let attempt = 0; attempt < 6; attempt++) {
    const response = await fetchImpl(url, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        authorization,
      },
      body,
    });

    if (response.status === 429) {
      const retryAfter = Number(response.headers.get("retry-after"));
      const delay = Number.isFinite(retryAfter) && retryAfter > 0 ? Math.min(retryAfter, 60) * 1000 : 2_000;
      await wait(delay);
      continue;
    }

    if (!response.ok) {
      throw new Error(await errorCode(response));
    }

    const payload = (await response.json()) as { results?: Array<{ status?: unknown; reason?: unknown }> };
    const results = payload.results || [];
    const parsed = results.map(classifyResult);
    if (results.length !== emails.length || parsed.some((item) => item === null)) {
      throw new Error("invalid_batch");
    }
    return parsed as ClassifyResult[];
  }

  throw new Error("rate_limited");
}

async function errorCode(response: Response): Promise<string> {
  try {
    const payload = (await response.json()) as { error?: unknown };
    if (typeof payload.error === "string" && payload.error) return payload.error;
  } catch {
    // The status is enough when the body is not JSON.
  }
  return `Cleaning failed (${response.status})`;
}
