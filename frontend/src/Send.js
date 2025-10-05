import { NDKEvent } from "@nostr-dev-kit/ndk";
// Newsletter sending client for the Queue Server
// Exposes a small API to Elm via ports (to be wired later)

const SW_URL = '/newsletter-sender-sw.js';
const DEFAULT_BASE_URL = 'http://localhost:4433/v1';

class NewsletterSendClient {
  constructor({ ndk, baseUrl = DEFAULT_BASE_URL, targetPubkey, contacts } = {}) {
    this.ndk = ndk;
    this.baseUrl = baseUrl;
    this.targetPubkey = targetPubkey;
    this.contacts = contacts || null;
    this.swRegistration = null;
    this.swController = null;
    this.jwt = null;
    this.jwtExpMs = 0;
    this.jwtInFlight = null;
    this.loggerPrefix = '[NewsletterSend]';
  }

  setContacts(contacts) { this.contacts = contacts; }
  setTargetPubkey(pubkey) { this.targetPubkey = pubkey; }

  _log(...args) {
    try { console.log(this.loggerPrefix, ...args); } catch (_) {}
  }

  async ensureServiceWorker() {
    if (this.swRegistration && this.swController) return this.swController;
    if (!('serviceWorker' in navigator)) throw new Error('Service workers not supported');

    this._log('Registering service worker', SW_URL);
    this.swRegistration = await navigator.serviceWorker.register(SW_URL, { scope: '/' });
    if (!navigator.serviceWorker.controller) {
      this._log('Waiting for service worker to become controller...');
      await navigator.serviceWorker.ready;
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    this.swController = navigator.serviceWorker.controller || (await navigator.serviceWorker.ready).active;
    this._log('Service worker ready');
    return this.swController;
  }

  swCall(type, payload) {
    return new Promise(async (resolve, reject) => {
      const controller = await this.ensureServiceWorker();
      const channel = new MessageChannel();
      channel.port1.onmessage = (event) => {
        const { ok, data } = event.data || {};
        if (ok) {
          this._log('SW response', type, { ok, preview: typeof data === 'string' ? data.slice(0, 64) : data });
          resolve(data);
        } else {
          this._log('SW error', type, data);
          reject(new Error(data?.error || 'Service worker error'));
        }
      };
      const preview = payload && typeof payload === 'object' ? JSON.stringify(payload).slice(0, 128) : undefined;
      this._log('SW call', type, { preview });
      controller.postMessage({ type, payload }, [channel.port2]);
    });
  }

  async createNip98Authorization(url, method = 'POST') {
    if (!this.ndk) throw new Error('NDK not initialized');
    const now = Math.floor(Date.now() / 1000);
    const event = new NDKEvent(this.ndk, {
      kind: 27235,
      content: '',
      tags: [
        ['u', url],
        ['method', method],
        ['t', String(now)],
      ],
    });
    await event.sign();
    const raw = event.rawEvent();
    const encoded = btoa(JSON.stringify(raw));
    this._log('NIP-98 created', { url, method, created_at: raw.created_at, id: raw.id?.slice(0, 8) });
    return `Nostr ${encoded}`;
  }

  async getJwt() {
    // Return cached JWT if valid for at least 60s more
    const now = Date.now();
    if (this.jwt && this.jwtExpMs && this.jwtExpMs - now > 60_000) {
      this._log('Using cached JWT, expires in ms:', this.jwtExpMs - now);
      return this.jwt;
    }
    if (this.jwtInFlight) {
      this._log('JWT request already in flight, awaiting existing promise');
      return this.jwtInFlight;
    }

    this.jwtInFlight = (async () => {
      const url = `${this.baseUrl}/auth/token`;
      const auth = await this.createNip98Authorization(url, 'POST');
      this._log('Fetching JWT', { url });
      const res = await fetch(url, { method: 'POST', headers: { Authorization: auth } });
      this._log('JWT response status', res.status);
      if (!res.ok) throw new Error(`JWT auth failed: ${res.status}`);
      const json = await res.json();
      const token = json.token;
      try {
        const headerStr = atob(token.split('.')[0].replace(/-/g, '+').replace(/_/g, '/'));
        this._log('JWT header', headerStr);
      } catch (_) {}
      // Try to parse exp from JWT
      try {
        const [, payloadB64] = token.split('.');
        const payloadStr = atob(payloadB64.replace(/-/g, '+').replace(/_/g, '/'));
        const payload = JSON.parse(payloadStr);
        if (payload && payload.exp) {
          this.jwtExpMs = payload.exp * 1000;
          this._log('JWT exp (ms)', this.jwtExpMs, 'ttl ms', this.jwtExpMs - Date.now());
        } else {
          this.jwtExpMs = Date.now() + 10 * 60_000; // fallback 10 min TTL
          this._log('JWT no exp claim found, using fallback TTL');
        }
      } catch (_) {
        this.jwtExpMs = Date.now() + 10 * 60_000;
        this._log('Failed to parse JWT exp, using fallback TTL');
      }
      this.jwt = token;
      this.jwtInFlight = null;
      return token;
    })();

    return this.jwtInFlight;
  }

  async init() {
    await this.ensureServiceWorker();
    const jwt = await this.getJwt();
    try {
      const parts = (jwt || '').split('.');
      const head = jwt ? jwt.slice(0, 10) : '';
      const tail = jwt ? jwt.slice(-10) : '';
      this._log('JWT client integrity', { length: jwt?.length, parts: parts.length, head, tail });
    } catch (_) {}
    this._log('Configuring SW with baseUrl and JWT');
    await this.swCall('configure', { baseUrl: this.baseUrl, jwt });
    return { jwt, baseUrl: this.baseUrl };
  }

  async createCampaign({ id, ownerId, newsletterData, queue = 'newsletter' }) {
    await this.ensureServiceWorker();
    const jwt = await this.getJwt();
    try {
      const parts = (jwt || '').split('.');
      const head = jwt ? jwt.slice(0, 10) : '';
      const tail = jwt ? jwt.slice(-10) : '';
      this._log('JWT pre-set for SW', { length: jwt?.length, parts: parts.length, head, tail });
    } catch (_) {}
    await this.swCall('set-jwt', { jwt });
    const { ciphertext_b64, size_bytes, enc } = await this._buildCampaignCipher(newsletterData);
    const payload = {
      externalId: id,
      ownerId,
      queue,
      ciphertext_b64,
      enc,
      size_bytes
    };
    this._log('Creating campaign', { externalId: id, ownerId: ownerId?.slice?.(0, 8), queue, test: !!(newsletterData && newsletterData.test), size_bytes });
    const result = await this.swCall('create-campaign', payload);
    this._log('Create campaign result', result);
    return result;
  }

  async bulkEnqueue({ campaignId, jobs }) {
    await this.ensureServiceWorker();
    const jwt = await this.getJwt();
    try {
      const parts = (jwt || '').split('.');
      const head = jwt ? jwt.slice(0, 10) : '';
      const tail = jwt ? jwt.slice(-10) : '';
      this._log('JWT pre-set for SW (bulk)', { length: jwt?.length, parts: parts.length, head, tail });
    } catch (_) {}
    await this.swCall('set-jwt', { jwt });
    const ndjson = jobs.map(j => JSON.stringify(j)).join('\n') + '\n';
    this._log('Bulk enqueue', { campaignId, jobs: jobs.length, ndjsonBytes: ndjson.length });
    const result = await this.swCall('bulk-enqueue-jobs', { campaignId, ndjson });
    this._log('Bulk enqueue result', result);
    return result;
  }

  async encryptFor(pubkeyHex, obj) {
    if (!this.ndk) throw new Error('NDK not initialized');
    const plaintext = typeof obj === 'string' ? obj : JSON.stringify(obj);
    return await this.ndk.signer.encrypt({ pubkey: pubkeyHex }, plaintext, 'nip44');
  }

  _toBase64Utf8(str) {
    const encoder = new TextEncoder();
    const bytes = encoder.encode(str);
    let binary = '';
    for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
    const b64 = btoa(binary);
    return { b64, byteLength: bytes.length };
  }

  async _buildCampaignCipher(newsletterData) {
    if (newsletterData && newsletterData.test === true) {
      const plaintext = JSON.stringify(newsletterData);
      const { b64, byteLength } = this._toBase64Utf8(plaintext);
      return { ciphertext_b64: b64, size_bytes: byteLength, enc: { alg: 'plain' } };
    }
    const ciphertext = await this.encryptFor(this.targetPubkey, newsletterData);
    const { b64, byteLength } = this._toBase64Utf8(ciphertext);
    return { ciphertext_b64: b64, size_bytes: byteLength, enc: { alg: 'nip44', kid: this.targetPubkey } };
  }

  // Build a single job spec for a contact
  async buildJobSpec(campaignId, contact, isTest) {
    if (!this.targetPubkey) throw new Error('Missing targetPubkey');
    if (!this.contacts || typeof this.contacts.generateEmailHash !== 'function') {
      throw new Error('Contacts instance not provided or invalid');
    }

    // Create idempotency key using Contacts DB hashing for stability
    const email = contact.email || contact.Email || contact.emailAddress;
    if (!email) throw new Error('Contact missing email');
    const idemHash = await this.contacts.generateEmailHash(email.toLowerCase());

    // Prepare payload: contact only (campaign content is attached to campaign)
    const payload = contact;

    let ciphertext_b64;
    let size_bytes;
    let enc;

    if (isTest === true) {
      // Test mode: send unencrypted payload
      const plaintext = JSON.stringify(payload);
      const encRes = this._toBase64Utf8(plaintext);
      ciphertext_b64 = encRes.b64;
      size_bytes = encRes.byteLength;
      enc = { alg: 'plain' };
      this._log('Built test job (unencrypted)', { email: email?.slice?.(0, 3) + '***', idem: `${campaignId}:${idemHash}`.slice(0, 16) + '...', size_bytes });
    } else {
      // Encrypt with NIP-44 for the target server (to be aligned with server JWE later)
      const ciphertext = await this.encryptFor(this.targetPubkey, payload);
      const encRes = this._toBase64Utf8(ciphertext);
      ciphertext_b64 = encRes.b64;
      size_bytes = encRes.byteLength;
      enc = { alg: 'nip44', kid: this.targetPubkey };
      this._log('Built job (encrypted)', { email: email?.slice?.(0, 3) + '***', idem: `${campaignId}:${idemHash}`.slice(0, 16) + '...', size_bytes });
    }

    return {
      queue: 'newsletter',
      idem: `${campaignId}:${idemHash}`,
      ciphertext_b64,
      enc,
      size_bytes,
    };
  }

  // Stream contacts in pages, encrypt, and bulk enqueue
  async sendNewsletter({ author, newsletterData, identifier, perPage = 500, onProgress } = {}) {
    if (!this.contacts) throw new Error('Contacts instance not set');
    if (!newsletterData || !newsletterData.title || !newsletterData.summary || !newsletterData.content) {
      throw new Error('Newsletter data must include at least title, summary and content');
    }

    // Ensure SW configured with JWT
    await this.init();

    // Create campaign in draft state and capture server-assigned id
    const created = await this.createCampaign({ id: identifier, ownerId: author, newsletterData: newsletterData, queue: 'newsletter' });
    const campaignId = created?.campaign_id;
    if (!campaignId) throw new Error('Missing campaign_id from createCampaign');

    let totals = {
      fetched: 0,
      built: 0,
      accepted: 0,
      duplicates: 0,
      errors: 0,
      pages: 0,
    };

    if (onProgress) onProgress({ phase: 'start', campaignId, page: 0, totals });
    this._log('Send newsletter start', { external_id: identifier, campaignId, perPage, test: !!(newsletterData && newsletterData.test) });

    let page = 1;
    while (true) {
      const result = await this.contacts.getContacts(page, perPage);
      const list = result && result.contacts ? result.contacts : [];
      if (!list.length) break;

      totals.pages += 1;
      totals.fetched += list.length;
      if (onProgress) onProgress({ phase: 'page_fetched', campaignId, page, fetched: list.length, totals });
      this._log('Page fetched', { page, count: list.length });

      // Build job specs
      const jobs = [];
      const isTest = !!(newsletterData && newsletterData.test);
      for (const contact of list) {
        try {
          const job = await this.buildJobSpec(String(campaignId), contact, isTest);
          jobs.push(job);
        } catch (e) {
          // Skip invalid contact; optional: collect diagnostics
          totals.errors += 1;
          this._log('Failed to build job for contact', e?.message);
        }
      }

      totals.built += jobs.length;
      if (onProgress) onProgress({ phase: 'page_built', campaignId, page, built: jobs.length, skipped: list.length - jobs.length, totals });
      this._log('Page built', { page, jobs: jobs.length });

      if (jobs.length) {
        const enqueueResult = await this.bulkEnqueue({ campaignId, jobs });
        const accepted = enqueueResult?.accepted || 0;
        const duplicates = enqueueResult?.duplicates || 0;
        const errors = enqueueResult?.errors || 0;
        totals.accepted += accepted;
        totals.duplicates += duplicates;
        totals.errors += errors;
        if (onProgress) onProgress({ phase: 'page_enqueued', campaignId, page, accepted, duplicates, errors, totals });
        this._log('Page enqueued', { campaignId, page, accepted, duplicates, errors });
      }

      if (list.length < perPage) break; // last page
      page += 1;
    }

    // Commit uploads to start processing
    try {
      const expected = totals.accepted;
      this._log('Committing campaign', { campaignId, expected_jobs: expected });
      const commitRes = await this.commitCampaign({ campaignId, expectedJobs: expected });
      this._log('Commit result', commitRes);
      if (onProgress) onProgress({ phase: 'committed', campaignId, expected_jobs: expected, totals });
    } catch (e) {
      this._log('Commit failed', e?.message);
      if (onProgress) onProgress({ phase: 'commit_failed', campaignId, error: e?.message, totals });
    }

    if (onProgress) onProgress({ phase: 'done', campaignId, page: page - 1, totals });
    this._log('Send newsletter done', { campaignId, totals });
    return { ok: true, totals };
  }

  async commitCampaign({ campaignId, expectedJobs }) {
    await this.ensureServiceWorker();
    const jwt = await this.getJwt();
    await this.swCall('set-jwt', { jwt });
    return await this.swCall('commit-campaign', { campaignId, expected_jobs: expectedJobs });
  }

  async getCampaignStatus(campaignId) {
    await this.ensureServiceWorker();
    const jwt = await this.getJwt();
    await this.swCall('set-jwt', { jwt });
    return await this.swCall('get-campaign-status', { campaignId });
  }
}

export function createNewsletterSender(options) {
  return new NewsletterSendClient(options);
}

export default NewsletterSendClient;


