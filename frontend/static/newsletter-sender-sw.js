// Newsletter Sender Service Worker
// Handles long-running newsletter operations against the Queue Server API

const DEFAULT_BASE_URL = 'https://queue-server.pareto.space/v1';

/**
 * State kept in SW memory. The page should send these on startup.
 */
let state = {
  baseUrl: DEFAULT_BASE_URL,
  jwt: null,
};

const activeRequests = new Map();

/**
 * Helper to POST JSON to the queue server
 */
async function postJson(baseUrl, path, body, jwt, signal) {
  const res = await fetch(`${baseUrl}${path}`, {
    method: 'POST',
    headers: Object.assign(
      { 'Content-Type': 'application/json' },
      jwt ? { Authorization: `Bearer ${jwt}` } : {}
    ),
    body: JSON.stringify(body || {}),
    signal,
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
async function postNdjson(baseUrl, path, ndjsonText, jwt, signal) {
  const res = await fetch(`${baseUrl}${path}`, {
    method: 'POST',
    headers: Object.assign(
      { 'Content-Type': 'application/x-ndjson' },
      jwt ? { Authorization: `Bearer ${jwt}` } : {}
    ),
    body: ndjsonText,
    signal,
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
  if (type === 'cancel') {
    activeRequests.get(payload?.requestId)?.abort();
    return;
  }
  if (type === 'SKIP_WAITING' || !type || !['configure', 'set-jwt', 'create-campaign', 'bulk-enqueue-jobs', 'lease-jobs', 'commit-campaign', 'get-campaign-status', 'get-campaign-status-by-external-id'].includes(type)) {
    return;
  }
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

  const controller = new AbortController();
  if (id) {
    activeRequests.set(id, controller);
  }
  const request = (async () => {
    const baseUrl = payload?.baseUrl || state.baseUrl;
    const jwt = payload?.jwt || state.jwt;
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
        if (!jwt) throw new Error('Missing JWT');
        console.log('[NewsletterSW] create-campaign', { url: `${baseUrl}/campaigns` });
        const result = await postJson(baseUrl, '/campaigns', payload, jwt, controller.signal);
        reply(true, result);
        break;
      }
      case 'bulk-enqueue-jobs': {
        if (!jwt) throw new Error('Missing JWT');
        const { campaignId, ndjson } = payload || {};
        if (!campaignId || !ndjson) throw new Error('Missing campaignId or ndjson');
        console.log('[NewsletterSW] bulk-enqueue', { url: `${baseUrl}/campaigns/${campaignId}/jobs/bulk`, bytes: ndjson.length });
        const result = await postNdjson(baseUrl, `/campaigns/${campaignId}/jobs/bulk`, ndjson, jwt, controller.signal);
        reply(true, result);
        break;
      }
      case 'lease-jobs': {
        const result = await postJson(baseUrl, '/jobs/lease', payload, payload?.consumerToken || null, controller.signal);
        reply(true, result);
        break;
      }
      case 'commit-campaign': {
        if (!jwt) throw new Error('Missing JWT');
        const { campaignId, expected_jobs } = payload || {};
        const result = await fetch(`${baseUrl}/campaigns/${campaignId}/commit`, {
          method: 'PATCH',
          headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${jwt}` },
          body: JSON.stringify(expected_jobs ? { expected_jobs } : {}),
          signal: controller.signal,
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
        if (!jwt) throw new Error('Missing JWT');
        const { campaignId } = payload || {};
        const res = await fetch(`${baseUrl}/campaigns/${campaignId}/status`, {
          headers: { Authorization: `Bearer ${jwt}` },
          signal: controller.signal,
        });
        if (!res.ok) {
          const text = await res.text().catch(() => '');
          throw new Error(`HTTP ${res.status} ${res.statusText} ${text}`);
        }
        const ct = res.headers.get('content-type') || '';
        reply(true, ct.includes('application/json') ? await res.json() : null);
        break;
      }
      case 'get-campaign-status-by-external-id': {
        if (!jwt) throw new Error('Missing JWT');
        const { externalId } = payload || {};
        if (!externalId) throw new Error('Missing externalId');
        const encodedId = encodeURIComponent(externalId);
        const res = await fetch(`${baseUrl}/campaigns/external/${encodedId}/status`, {
          headers: { Authorization: `Bearer ${jwt}` },
          signal: controller.signal,
        });
        if (res.status === 404) {
          reply(true, null);
          break;
        }
        if (!res.ok) {
          const text = await res.text().catch(() => '');
          throw new Error(`HTTP ${res.status} ${res.statusText} ${text}`);
        }
        const ct = res.headers.get('content-type') || '';
        reply(true, ct.includes('application/json') ? await res.json() : null);
        break;
      }
      default:
        return;
    }
  })()
    .catch(err => {
      console.error('[NewsletterSW] error', type, err?.message);
      reply(false, { error: err.message });
    })
    .finally(() => {
      if (id && activeRequests.get(id) === controller) {
        activeRequests.delete(id);
      }
    });
  event.waitUntil(request);
});


