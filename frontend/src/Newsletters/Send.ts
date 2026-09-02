import { NDKEvent } from "@nostr-dev-kit/ndk";
import { Configuration, DefaultApi } from "./queue_server";
import type { CampaignCreate, CampaignStatus, CampaignStatusError } from "./queue_server";
import {
  activeSubscribers,
  loadActiveSubscribers,
  loadActiveSubscribersFromPointer,
  type Subscriber,
  type SubscriberBlobPointer,
} from "./subscriberBlob";

const DEFAULT_BASE_URL = "http://localhost:4433/v1";
const EMAIL_GATEWAY_PUBKEY = "cefbf43addd677426c671d7cd275289be35f7b6b398fced7fae420d060e7a345";

export type NewsletterData = {
  title: string;
  summary: string;
  content: string;
  imageUrl?: string;
  language?: string;
  identifier?: string;
  test?: boolean;
};

type ProgressFn = (progress: Record<string, unknown>) => void;

type SendOptions = {
  author: string;
  newsletterData: NewsletterData;
  identifier?: string;
  subscribers?: Subscriber[];
  subscriberBlob?: SubscriberBlobPointer;
  onProgress?: ProgressFn;
  signal?: AbortSignal;
};

export class NewsletterSendCancelled extends Error {
  constructor(message = "Send cancelled") {
    super(message);
    this.name = "NewsletterSendCancelled";
  }
}

export function isNewsletterSendCancelled(error: unknown): boolean {
  const err = error as { name?: string; code?: string };
  return (
    err?.name === "NewsletterSendCancelled" ||
    err?.name === "AbortError" ||
    err?.name === "CanceledError" ||
    err?.code === "ERR_CANCELED"
  );
}

function throwIfAborted(signal?: AbortSignal): void {
  if (signal?.aborted) {
    throw new NewsletterSendCancelled();
  }
}

function rethrowIfCancelled(error: unknown): void {
  if (isNewsletterSendCancelled(error)) {
    throw error instanceof NewsletterSendCancelled ? error : new NewsletterSendCancelled();
  }
}

type TestOptions = SendOptions & {
  email?: string;
  contact?: Subscriber;
};

type JobSpec = {
  idem: string;
  ciphertext_b64: string;
  enc: { alg: "nip44" | "plain"; kid?: string };
  size_bytes: number;
};

function describeQueueError(error: unknown): string {
  const axiosError = error as {
    message?: string;
    response?: { status?: number; data?: { error?: { message?: string; code?: string; details?: unknown } } };
  };
  const payload = axiosError.response?.data?.error;
  if (payload?.message) {
    const details = payload.details ? ` ${JSON.stringify(payload.details)}` : "";
    return `${payload.message}${details}`;
  }
  if (axiosError.response?.status) {
    return axiosError.message || `Queue server returned ${axiosError.response.status}`;
  }
  return axiosError.message || "Queue server request failed";
}

const TERMINAL_DELIVERIES = new Set(["sent", "failed", "partial"]);

function isTerminalDelivery(delivery?: string | null): boolean {
  return !!delivery && TERMINAL_DELIVERIES.has(delivery);
}

function progressDeliveryCounts(
  status?: CampaignStatus | null,
  fallbackTotal = 0,
): { sent: number; total: number } {
  const counts = status?.counts;
  const sent = counts?.done ?? 0;
  const countedTotal = counts
    ? (counts.done || 0) +
      (counts.failed || 0) +
      (counts.held || 0) +
      (counts.available || 0) +
      (counts.leased || 0) +
      (counts.retryable || 0)
    : 0;
  const total = status?.expected_jobs || countedTotal || fallbackTotal;
  return { sent, total };
}

function formatRecentErrors(errors?: CampaignStatusError[] | null): string {
  if (!errors?.length) {
    return "";
  }
  return errors
    .map((item) => item.error?.code || item.error?.hint || item.idem)
    .filter(Boolean)
    .slice(0, 5)
    .join(", ");
}

function sleep(ms: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) {
      reject(new NewsletterSendCancelled());
      return;
    }
    const timer = setTimeout(resolve, ms);
    const onAbort = () => {
      clearTimeout(timer);
      reject(new NewsletterSendCancelled());
    };
    signal?.addEventListener("abort", onAbort, { once: true });
  });
}

function uniqueTestExternalId(base?: string): string {
  const nonce =
    typeof crypto !== "undefined" && typeof crypto.randomUUID === "function"
      ? crypto.randomUUID()
      : `${Date.now()}-${Math.random().toString(16).slice(2)}`;
  return base ? `${base}-test-${nonce}` : `test-${nonce}`;
}

export class NewsletterSendClient {
  ndk: any;
  baseUrl: string;
  targetPubkey: string;
  jwt: string | null = null;
  jwtExpMs = 0;
  jwtInFlight: Promise<string> | null = null;
  api: DefaultApi | null = null;

  constructor({
    ndk,
    baseUrl = DEFAULT_BASE_URL,
    targetPubkey = EMAIL_GATEWAY_PUBKEY,
  }: { ndk?: any; baseUrl?: string; targetPubkey?: string } = {}) {
    this.ndk = ndk;
    this.baseUrl = baseUrl.replace(/\/+$/, "");
    this.targetPubkey = targetPubkey;
  }

  private log(...args: unknown[]) {
    try {
      console.log("[NewsletterSend]", ...args);
    } catch {
      // ignore
    }
  }

  private async createNip98Authorization(url: string, method = "POST"): Promise<string> {
    if (!this.ndk) {
      throw new Error("NDK not initialized");
    }
    const now = Math.floor(Date.now() / 1000);
    const event = new NDKEvent(this.ndk, {
      kind: 27235,
      content: "",
      tags: [
        ["u", url],
        ["method", method],
        ["t", String(now)],
      ],
    });
    await event.sign();
    return `Nostr ${btoa(JSON.stringify(event.rawEvent()))}`;
  }

  async getJwt(): Promise<string> {
    const now = Date.now();
    if (this.jwt && this.jwtExpMs - now > 60_000) {
      return this.jwt;
    }
    if (this.jwtInFlight) {
      return this.jwtInFlight;
    }

    this.jwtInFlight = (async () => {
      const url = `${this.baseUrl}/auth/token`;
      const auth = await this.createNip98Authorization(url, "POST");
      const res = await fetch(url, { method: "POST", headers: { Authorization: auth } });
      if (!res.ok) {
        throw new Error(`JWT auth failed: ${res.status}`);
      }
      const json = await res.json();
      const token = json.token as string;
      try {
        const payloadStr = atob(token.split(".")[1].replace(/-/g, "+").replace(/_/g, "/"));
        const payload = JSON.parse(payloadStr);
        this.jwtExpMs = payload?.exp ? payload.exp * 1000 : Date.now() + 10 * 60_000;
      } catch {
        this.jwtExpMs = Date.now() + 10 * 60_000;
      }
      this.jwt = token;
      this.api = new DefaultApi(
        new Configuration({
          basePath: this.baseUrl,
          accessToken: token,
        }),
      );
      return token;
    })();

    try {
      return await this.jwtInFlight;
    } finally {
      this.jwtInFlight = null;
    }
  }

  private async client(): Promise<DefaultApi> {
    await this.getJwt();
    if (!this.api) {
      throw new Error("Queue server client is not ready");
    }
    return this.api;
  }

  private async encryptFor(pubkeyHex: string, obj: unknown): Promise<string> {
    if (!this.ndk?.signer) {
      throw new Error("NDK signer is not available");
    }
    const plaintext = typeof obj === "string" ? obj : JSON.stringify(obj);
    return this.ndk.signer.encrypt({ pubkey: pubkeyHex }, plaintext, "nip44");
  }

  private toBase64Utf8(str: string): { b64: string; byteLength: number } {
    const bytes = new TextEncoder().encode(str);
    let binary = "";
    for (let i = 0; i < bytes.length; i++) {
      binary += String.fromCharCode(bytes[i]);
    }
    return { b64: btoa(binary), byteLength: bytes.length };
  }

  private utf8ByteLength(str: string): number {
    return new TextEncoder().encode(str).length;
  }

  private nip44Envelope(ciphertext: string, authorPubkey: string) {
    return {
      ciphertext_b64: ciphertext,
      size_bytes: this.utf8ByteLength(ciphertext),
      enc: { alg: "nip44" as const, kid: authorPubkey },
    };
  }

  private async buildCampaignCipher(
    newsletterData: NewsletterData,
    authorPubkey: string,
  ): Promise<Pick<CampaignCreate, "ciphertext_b64" | "size_bytes" | "enc">> {
    if (newsletterData.test === true) {
      const { b64, byteLength } = this.toBase64Utf8(JSON.stringify(newsletterData));
      return { ciphertext_b64: b64, size_bytes: byteLength, enc: { alg: "plain" } };
    }
    const ciphertext = await this.encryptFor(this.targetPubkey, newsletterData);
    return this.nip44Envelope(ciphertext, authorPubkey);
  }

  private async hashEmail(email: string): Promise<string> {
    const bytes = new TextEncoder().encode(email.toLowerCase());
    const digest = await crypto.subtle.digest("SHA-256", bytes);
    return Array.from(new Uint8Array(digest))
      .map((b) => b.toString(16).padStart(2, "0"))
      .join("");
  }

  async buildJobSpec(
    campaignId: string,
    contact: Subscriber,
    isTest: boolean,
    authorPubkey: string,
  ): Promise<JobSpec> {
    const email = String(contact.email || contact.Email || contact.emailAddress || "").trim();
    if (!email) {
      throw new Error("Contact missing email");
    }
    const idemHash = await this.hashEmail(email);
    if (isTest) {
      const { b64, byteLength } = this.toBase64Utf8(JSON.stringify(contact));
      return {
        idem: `${campaignId}:${idemHash}`,
        ciphertext_b64: b64,
        enc: { alg: "plain" },
        size_bytes: byteLength,
      };
    }
    const ciphertext = await this.encryptFor(this.targetPubkey, contact);
    return {
      idem: `${campaignId}:${idemHash}`,
      ...this.nip44Envelope(ciphertext, authorPubkey),
    };
  }

  private async resolveRecipients(
    author: string,
    subscribers?: Subscriber[],
    subscriberBlob?: SubscriberBlobPointer,
    signal?: AbortSignal,
  ): Promise<Subscriber[]> {
    if (Array.isArray(subscribers)) {
      return activeSubscribers(subscribers);
    }
    if (subscriberBlob?.url && subscriberBlob.key && subscriberBlob.iv) {
      return loadActiveSubscribersFromPointer(subscriberBlob, signal);
    }
    return loadActiveSubscribers(this.ndk, author);
  }

  async sendNewsletter({
    author,
    newsletterData,
    identifier,
    subscribers,
    subscriberBlob,
    onProgress,
    signal,
  }: SendOptions) {
    if (!newsletterData?.title || !newsletterData.summary || !newsletterData.content) {
      throw new Error("Newsletter data must include at least title, summary and content");
    }

    const isTest = newsletterData.test === true;
    throwIfAborted(signal);
    onProgress?.({ phase: "preparing" });
    const recipients = await this.resolveRecipients(author, subscribers, subscriberBlob, signal);
    throwIfAborted(signal);
    if (!isTest && recipients.length === 0) {
      throw new Error("No active subscribers found. The subscriber list could not be loaded.");
    }
    const totals = {
      fetched: recipients.length,
      built: 0,
      accepted: 0,
      duplicates: 0,
      errors: 0,
      pages: 1,
    };
    onProgress?.({ phase: "authenticating", totals });
    const api = await this.client();
    throwIfAborted(signal);
    const externalId = identifier || newsletterData.identifier;
    onProgress?.({ phase: "creating_campaign", totals });
    const cipher = await this.buildCampaignCipher(newsletterData, author);
    throwIfAborted(signal);
    let created;
    try {
      created = await api.createCampaign(
        {
          externalId,
          ownerId: author,
          queue: "newsletter",
          ...cipher,
        },
        { signal },
      );
    } catch (error) {
      rethrowIfCancelled(error);
      throw new Error(describeQueueError(error));
    }
    const campaignId = created.data.campaign_id;
    if (!campaignId) {
      throw new Error("Missing campaign_id from createCampaign");
    }

    throwIfAborted(signal);
    onProgress?.({ phase: "queueing", campaignId, delivery: "queueing", sent: 0, total: totals.fetched, totals });
    const watchAbort = new AbortController();
    const stopWatch = () => watchAbort.abort();
    signal?.addEventListener("abort", stopWatch);
    const deliveryWatch = this.watchCampaignDelivery(String(campaignId), (status) => {
      const delivery = status.delivery || "queueing";
      const { sent, total } = progressDeliveryCounts(status, totals.accepted || totals.fetched);
      onProgress?.({
        phase: delivery,
        campaignId,
        delivery,
        sent,
        total,
        counts: status.counts,
        recent_errors: status.recent_errors || [],
        uploaded_jobs: status.uploaded_jobs,
        expected_jobs: status.expected_jobs,
        totals,
        error: isTerminalDelivery(delivery) && delivery !== "sent" ? formatRecentErrors(status.recent_errors) : undefined,
      });
    }, watchAbort.signal);

    try {
      this.log("Send newsletter start", { externalId, campaignId, recipients: recipients.length, test: isTest });

      const jobs: JobSpec[] = [];
      for (const contact of recipients) {
        throwIfAborted(signal);
        try {
          jobs.push(await this.buildJobSpec(campaignId, contact, isTest, author));
        } catch (error: unknown) {
          rethrowIfCancelled(error);
          totals.errors += 1;
          this.log("Failed to build job", (error as { message?: string })?.message);
        }
      }
      totals.built = jobs.length;
      throwIfAborted(signal);
      onProgress?.({ phase: "page_built", campaignId, built: jobs.length, totals });

      if (!jobs.length) {
        throw new Error("No newsletter jobs could be built from the subscriber list.");
      }

      try {
        const ndjson = jobs.map((job) => JSON.stringify(job)).join("\n") + "\n";
        const enqueueResult = await api.bulkEnqueueJobs(campaignId, ndjson, { signal });
        totals.accepted += enqueueResult.data.accepted || 0;
        totals.duplicates += enqueueResult.data.duplicates || 0;
        totals.errors += enqueueResult.data.errors || 0;
        onProgress?.({
          phase: "page_enqueued",
          campaignId,
          ...enqueueResult.data,
          sent: 0,
          total: totals.accepted || totals.fetched,
          totals,
        });
      } catch (error) {
        rethrowIfCancelled(error);
        throw new Error(describeQueueError(error));
      }

      throwIfAborted(signal);
      try {
        await api.commitCampaign(campaignId, { expected_jobs: totals.accepted }, { signal });
        onProgress?.({
          phase: "sending",
          campaignId,
          delivery: "sending",
          sent: 0,
          total: totals.accepted,
          expected_jobs: totals.accepted,
          totals,
        });
      } catch (error: unknown) {
        rethrowIfCancelled(error);
        const message = describeQueueError(error);
        onProgress?.({ phase: "commit_failed", campaignId, error: message, totals });
        throw new Error(message);
      }

      const finalStatus = await deliveryWatch;
      const delivery = finalStatus?.delivery || "sending";
      if (delivery === "failed") {
        throw new Error(formatRecentErrors(finalStatus?.recent_errors) || "All newsletter deliveries failed");
      }
      if (delivery === "partial") {
        throw new Error(
          `Delivered to some recipients. Failed: ${formatRecentErrors(finalStatus?.recent_errors) || "unknown error"}`,
        );
      }

      const { sent, total } = progressDeliveryCounts(finalStatus, totals.accepted);
      onProgress?.({ phase: "sent", campaignId, delivery: "sent", sent, total, counts: finalStatus?.counts, totals });
      return { ok: true, totals, delivery: "sent" };
    } finally {
      stopWatch();
      signal?.removeEventListener("abort", stopWatch);
    }
  }

  async sendNewsletterTest({ email, contact, author, newsletterData, identifier, onProgress, signal }: TestOptions) {
    const contactRecord: Subscriber = { ...(contact || {}) };
    if (email) {
      contactRecord.email = email;
    }
    if (!contactRecord.email) {
      throw new Error("sendNewsletterTest requires an email address");
    }
    return this.sendNewsletter({
      author,
      newsletterData: { ...newsletterData, identifier: undefined },
      identifier: uniqueTestExternalId(identifier || newsletterData.identifier),
      subscribers: [contactRecord],
      onProgress,
      signal,
    });
  }

  private campaignWsUrl(): { socketUrl: string; authUrl: string } {
    const httpUrl = new URL(this.baseUrl);
    const wsProtocol = httpUrl.protocol === "https:" ? "wss:" : "ws:";
    const authUrl = `${wsProtocol}//${httpUrl.host}/ws`;
    const socketUrl = `${authUrl}/websocket`;
    return { socketUrl, authUrl };
  }

  private async createNip98EventB64(url: string, method = "GET"): Promise<string> {
    const header = await this.createNip98Authorization(url, method);
    return header.replace(/^Nostr\s+/, "");
  }

  private async getCampaignStatus(campaignId: string, signal?: AbortSignal): Promise<CampaignStatus> {
    const api = await this.client();
    const result = await api.getCampaignStatus(String(campaignId), { signal });
    return result.data;
  }

  private async watchCampaignDelivery(
    campaignId: string,
    onStatus: (status: CampaignStatus) => void,
    signal?: AbortSignal,
  ): Promise<CampaignStatus> {
    throwIfAborted(signal);

    const socketPromise = this.watchCampaignViaSocket(campaignId, onStatus, signal).then((status) => {
      if (status) {
        return status;
      }
      throw new Error("campaign_status_socket_unavailable");
    });
    const pollPromise = this.pollCampaignStatus(campaignId, onStatus, signal);

    try {
      return await Promise.any([socketPromise, pollPromise]);
    } catch (error) {
      rethrowIfCancelled(error);
      return pollPromise;
    }
  }

  private async pollCampaignStatus(
    campaignId: string,
    onStatus: (status: CampaignStatus) => void,
    signal?: AbortSignal,
  ): Promise<CampaignStatus> {
    while (true) {
      throwIfAborted(signal);
      const status = await this.getCampaignStatus(campaignId, signal);
      onStatus(status);
      if (isTerminalDelivery(status.delivery)) {
        return status;
      }
      await sleep(2000, signal);
    }
  }

  private async watchCampaignViaSocket(
    campaignId: string,
    onStatus: (status: CampaignStatus) => void,
    signal?: AbortSignal,
  ): Promise<CampaignStatus | null> {
    if (typeof WebSocket === "undefined") {
      return null;
    }

    let socket: WebSocket | undefined;
    try {
      const { socketUrl, authUrl } = this.campaignWsUrl();
      const nip98 = await this.createNip98EventB64(authUrl, "GET");
      const url = `${socketUrl}?vsn=2.0.0&nip98=${encodeURIComponent(nip98)}`;

      return await new Promise<CampaignStatus | null>((resolve, reject) => {
        let settled = false;
        let joinRef = 1;
        let heartbeatTimer: ReturnType<typeof setInterval> | undefined;
        const topic = `campaigns:${campaignId}`;

        const finish = (value: CampaignStatus | null, error?: unknown) => {
          if (settled) {
            return;
          }
          settled = true;
          if (heartbeatTimer) {
            clearInterval(heartbeatTimer);
          }
          signal?.removeEventListener("abort", onAbort);
          try {
            socket?.close();
          } catch {
            // ignore
          }
          if (error) {
            reject(error);
          } else {
            resolve(value);
          }
        };

        const onAbort = () => finish(null, new NewsletterSendCancelled());
        signal?.addEventListener("abort", onAbort);

        socket = new WebSocket(url);

        socket.onerror = () => finish(null);
        socket.onclose = () => {
          if (!settled) {
            finish(null);
          }
        };
        socket.onopen = () => {
          socket?.send(JSON.stringify([String(joinRef), String(joinRef), topic, "phx_join", {}]));
          heartbeatTimer = setInterval(() => {
            joinRef += 1;
            try {
              socket?.send(JSON.stringify([null, String(joinRef), "phoenix", "heartbeat", {}]));
            } catch {
              // ignore
            }
          }, 30_000);
        };
        socket.onmessage = (event) => {
          try {
            const frame = JSON.parse(String(event.data));
            if (!Array.isArray(frame) || frame.length < 5) {
              return;
            }
            const [_join, _ref, frameTopic, name, payload] = frame;
            if (name === "phx_reply" && frameTopic === topic) {
              if (payload?.status !== "ok") {
                finish(null);
              }
              return;
            }
            if (name === "status" && payload) {
              const status = payload as CampaignStatus;
              onStatus(status);
              if (isTerminalDelivery(status.delivery)) {
                finish(status);
              }
            }
          } catch {
            finish(null);
          }
        };
      });
    } catch (error) {
      rethrowIfCancelled(error);
      this.log("Campaign status socket failed, falling back to poll", (error as { message?: string })?.message);
      try {
        socket?.close();
      } catch {
        // ignore
      }
      return null;
    }
  }

  async getCampaignStatusByExternalId(externalId: string): Promise<CampaignStatus | null> {
    if (!externalId) {
      throw new Error("externalId is required");
    }
    const api = await this.client();
    try {
      const result = await api.getCampaignStatusByExternalId(externalId);
      return result.data;
    } catch (error: any) {
      if (error?.response?.status === 404 || /404/.test(error?.message || "")) {
        return null;
      }
      throw error;
    }
  }

  async countActiveRecipients(author: string, subscriberBlob?: SubscriberBlobPointer): Promise<number> {
    const subscribers = await this.resolveRecipients(author, undefined, subscriberBlob);
    return subscribers.length;
  }
}

export function createNewsletterSender(options?: {
  ndk?: any;
  baseUrl?: string;
  targetPubkey?: string;
}) {
  return new NewsletterSendClient(options);
}

export default NewsletterSendClient;
