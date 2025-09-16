// Newsletter Sender Service Worker
// Handles long-running newsletter operations against the Queue Server API

const DEFAULT_BASE_URL = 'http://localhost:4433/v1';

/**
 * State kept in SW memory. The page should send these on startup.
 */
let state = {
  baseUrl: DEFAULT_BASE_URL,
  jwt: null,
};

/**
 * Helper to POST JSON to the queue server
 */
async function postJson(path, body, jwt) {
  const res = await fetch(`${state.baseUrl}${path}`, {
    method: 'POST',
    headers: Object.assign(
      { 'Content-Type': 'application/json' },
      jwt ? { Authorization: `Bearer ${jwt}` } : {}
    ),
    body: JSON.stringify(body || {}),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    throw new Error(`HTTP ${res.status} ${res.statusText} ${text}`);
  }
  const ct = res.headers.get('content-type') || '';
  if (ct.includes('application/json')) return res.json();
  return null;
}

/**
 * Helper to POST NDJSON (text) to the queue server
 */
async function postNdjson(path, ndjsonText, jwt) {
  const res = await fetch(`${state.baseUrl}${path}`, {
    method: 'POST',
    headers: Object.assign(
      { 'Content-Type': 'application/x-ndjson' },
      jwt ? { Authorization: `Bearer ${jwt}` } : {}
    ),
    body: ndjsonText,
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    throw new Error(`HTTP ${res.status} ${res.statusText} ${text}`);
  }
  const ct = res.headers.get('content-type') || '';
  if (ct.includes('application/json')) return res.json();
  return null;
}

/**
 * Message router
 */
self.addEventListener('message', event => {
  const { id, type, payload } = event.data || {};
  const source = event.source || self.clients;
  const port = (event.ports && event.ports[0]) || null;

  const reply = (ok, data) => {
    const message = id ? { id, ok, data } : { ok, data };
    if (port) {
      port.postMessage(message);
    } else {
      source.postMessage(message);
    }
  };

  (async () => {
    switch (type) {
      case 'configure': {
        state.baseUrl = payload?.baseUrl || state.baseUrl;
        state.jwt = payload?.jwt || state.jwt;
        console.log('[NewsletterSW] configured', { baseUrl: state.baseUrl, hasJwt: !!state.jwt });
        reply(true, { configured: true, baseUrl: state.baseUrl });
        break;
      }
      case 'set-jwt': {
        state.jwt = payload?.jwt || null;
        try {
          const token = state.jwt || '';
          const head = token.slice(0, 10);
          const tail = token.slice(-10);
          const parts = token.split('.').length;
          console.log('[NewsletterSW] jwt set', { hasJwt: !!state.jwt, length: token.length, parts, head, tail });
        } catch (_) {}
        reply(true, { jwt: !!state.jwt });
        break;
      }
      case 'create-campaign': {
        if (!state.jwt) throw new Error('Missing JWT');
        console.log('[NewsletterSW] create-campaign', { url: `${state.baseUrl}/campaigns` });
        const result = await postJson('/campaigns', payload, state.jwt);
        reply(true, result);
        break;
      }
      case 'bulk-enqueue-jobs': {
        if (!state.jwt) throw new Error('Missing JWT');
        const { campaignId, ndjson } = payload || {};
        if (!campaignId || !ndjson) throw new Error('Missing campaignId or ndjson');
        console.log('[NewsletterSW] bulk-enqueue', { url: `${state.baseUrl}/campaigns/${campaignId}/jobs/bulk`, bytes: ndjson.length });
        const result = await postNdjson(`/campaigns/${campaignId}/jobs/bulk`, ndjson, state.jwt);
        reply(true, result);
        break;
      }
      case 'lease-jobs': {
        const result = await postJson('/jobs/lease', payload, payload?.consumerToken || null);
        reply(true, result);
        break;
      }
      case 'commit-campaign': {
        if (!state.jwt) throw new Error('Missing JWT');
        const { campaignId, expected_jobs } = payload || {};
        const result = await fetch(`${state.baseUrl}/campaigns/${campaignId}/commit`, {
          method: 'PATCH',
          headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${state.jwt}` },
          body: JSON.stringify(expected_jobs ? { expected_jobs } : {}),
        });
        if (!result.ok) {
          const text = await result.text().catch(() => '');
          throw new Error(`HTTP ${result.status} ${result.statusText} ${text}`);
        }
        const ct = result.headers.get('content-type') || '';
        reply(true, ct.includes('application/json') ? await result.json() : null);
        break;
      }
      case 'get-campaign-status': {
        if (!state.jwt) throw new Error('Missing JWT');
        const { campaignId } = payload || {};
        const res = await fetch(`${state.baseUrl}/campaigns/${campaignId}/status`, {
          headers: { Authorization: `Bearer ${state.jwt}` },
        });
        if (!res.ok) {
          const text = await res.text().catch(() => '');
          throw new Error(`HTTP ${res.status} ${res.statusText} ${text}`);
        }
        const ct = res.headers.get('content-type') || '';
        reply(true, ct.includes('application/json') ? await res.json() : null);
        break;
      }
      default:
        throw new Error(`Unknown message type: ${type}`);
    }
  })().catch(err => {
    console.error('[NewsletterSW] error', type, err?.message);
    reply(false, { error: err.message });
  });
});

// Ensure SW stays alive while tasks are running
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', event => {
  event.waitUntil(self.clients.claim());
});


