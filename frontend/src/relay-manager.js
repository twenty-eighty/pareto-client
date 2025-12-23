import { NDKRelaySet } from "@nostr-dev-kit/ndk";

const QUEUE_TIMEOUT_MS = 5000;

const normalizeRelayUrl = (url) => {
  const withProtocol = url.startsWith("wss://") || url.startsWith("ws://") ? url : `wss://${url}`;
  return withProtocol.endsWith("/") ? withProtocol : `${withProtocol}/`;
};

export function createRelayManager(ndk, debugLog, processEvents) {
  const readyRelays = new Set();
  const queuedRequests = [];

  const markReady = (relayUrl) => {
    const normalized = normalizeRelayUrl(relayUrl);
    readyRelays.add(normalized);
    flushQueue(normalized);
  };

  const markDisconnect = (relayUrl) => {
    const normalized = normalizeRelayUrl(relayUrl);
    readyRelays.delete(normalized);
  };

  const fetchEvents = (app, { requestId, filters, closeOnEose, description, relays }) => {
    const targetRelays = relays && relays.length > 0
      ? relays.map(normalizeRelayUrl)
      : Array.from(ndk.pool.relays.keys()).map(normalizeRelayUrl);

    const { readySubset, missing } = splitRelays(targetRelays);

    if (readySubset.length > 0) {
      sendFetch(readySubset, { requestId, filters, closeOnEose, description, app });
    }

    if (missing.length > 0) {
      queuedRequests.push({
        requestId,
        filters,
        closeOnEose,
        description,
        missing: new Set(missing),
        createdAt: Date.now(),
        app
      });
    }
  };

  const splitRelays = (relayUrls) => {
    const readySubset = [];
    const missing = [];
    relayUrls.forEach((url) => {
      if (readyRelays.has(url)) {
        readySubset.push(url);
      } else {
        missing.push(url);
      }
    });
    return { readySubset, missing };
  };

  const flushQueue = (newlyReadyUrl) => {
    const now = Date.now();

    for (let i = queuedRequests.length - 1; i >= 0; i--) {
      const request = queuedRequests[i];

      // Remove expired requests to avoid leaking memory.
      if (now - request.createdAt > QUEUE_TIMEOUT_MS) {
        queuedRequests.splice(i, 1);
        continue;
      }

      if (!request.missing.has(newlyReadyUrl)) {
        continue;
      }

      const readyNow = Array.from(request.missing).filter((url) => readyRelays.has(url));

      if (readyNow.length > 0) {
        sendFetch(readyNow, request);
        readyNow.forEach((url) => request.missing.delete(url));
      }

      if (request.missing.size === 0) {
        queuedRequests.splice(i, 1);
      }
    }
  };

  const sendFetch = (relayUrls, { requestId, filters, closeOnEose, description, app }) => {
    const relaySet = NDKRelaySet.fromRelayUrls(relayUrls, ndk, false, ndk.pool);

    ndk.fetchEvents(filters, { closeOnEose: closeOnEose }, relaySet).then((ndkEvents) => {
      processEvents(app, requestId, description, ndkEvents);
    }).catch((err) => {
      debugLog('fetchEvents error', { relays: relayUrls, err });
    });
  };

  return {
    markReady,
    markDisconnect,
    fetchEvents,
  };
}
