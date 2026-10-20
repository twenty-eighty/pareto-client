import { NDKRelaySet } from "@nostr-dev-kit/ndk";
import { filterRelayUrls } from "./blocked-relays";
import { createRequestBatch, finishFetch, relaysSettled, startFetch } from "./relay-request-batch";

const QUEUE_TIMEOUT_MS = 15000;

const normalizeRelayUrl = (url) => {
  const withProtocol = url.startsWith("wss://") || url.startsWith("ws://") ? url : `wss://${url}`;
  return withProtocol.endsWith("/") ? withProtocol : `${withProtocol}/`;
};

export function createRelayManager(ndk, debugLog, processEvents) {
  const readyRelays = new Set();
  const queuedRequests = [];
  const trackedRelays = new Set(); // keep listeners attached only once per relay
  const batches = new Map();
  let nextGeneration = 1;

  const markReady = (relayUrl) => {
    const normalized = normalizeRelayUrl(relayUrl);
    readyRelays.add(normalized);
    flushQueue(normalized);
  };

  const markDisconnect = (relayUrl) => {
    const normalized = normalizeRelayUrl(relayUrl);
    readyRelays.delete(normalized);
  };

  const emitComplete = (app, requestId) => {
    app.ports.receiveMessage.send({ messageType: "eventsComplete", value: { requestId } });
  };

  const currentBatch = (requestId, generation) => {
    const batch = batches.get(requestId);
    if (!batch || batch.generation !== generation) {
      return null;
    }
    return batch;
  };

  const completeBatch = (app, requestId, generation) => {
    const batch = batches.get(requestId);
    if (!batch || batch.generation !== generation) {
      return;
    }
    batches.delete(requestId);
    emitComplete(app, requestId);
  };

  const settleWaitingRelays = (app, requestId, generation) => {
    const batch = currentBatch(requestId, generation);
    if (batch && relaysSettled(batch)) {
      completeBatch(app, requestId, generation);
    }
  };

  const dropQueued = (requestId, generation) => {
    for (let i = queuedRequests.length - 1; i >= 0; i--) {
      const request = queuedRequests[i];
      if (request.requestId === requestId && request.generation === generation) {
        queuedRequests.splice(i, 1);
      }
    }
  };

  const fetchEvents = (app, { requestId, filters, closeOnEose, description, relays }) => {
    const targetRelays = filterRelayUrls(
      (relays && relays.length > 0
        ? relays.map(normalizeRelayUrl)
        : Array.from(ndk.pool.relays.keys()).map(normalizeRelayUrl))
    );

    // ensure target relays are in the pool and connecting
    targetRelays.forEach((url) => {
      try {
        const relay = ndk.pool.getRelay(url, true);

        // Ensure the relay attempts to connect if it's idle.
        try {
          if (relay.connect) {
            relay.connect();
          }
        } catch (connectErr) {
          debugLog && debugLog("relayManager relay connect failed", { url, connectErr });
        }

        const trackedKey = normalizeRelayUrl(relay.url);

        if (!trackedRelays.has(trackedKey)) {
          trackedRelays.add(trackedKey);

          relay.on("connect", () => {
            debugLog && debugLog("relayManager relay connect", relay.url);
          });

          relay.on("ready", () => {
            debugLog && debugLog("relayManager relay ready", relay.url);
            markReady(relay.url);
          });

          relay.on("disconnect", () => {
            debugLog && debugLog("relayManager relay disconnect", relay.url);
            markDisconnect(relay.url);
          });
        }
      } catch (e) {
        debugLog && debugLog('relayManager getRelay failed', { url, e });
      }
    });

    const { readySubset, missing } = splitRelays(targetRelays);
    const generation = nextGeneration++;
    const batch = createRequestBatch(generation, missing.length > 0);
    batches.set(requestId, batch);

    const request = {
      requestId,
      generation,
      filters,
      closeOnEose,
      description,
      missing: new Set(missing),
      createdAt: Date.now(),
      app,
    };

    if (readySubset.length > 0) {
      sendFetch(readySubset, request);
    }

    if (missing.length > 0) {
      queuedRequests.push(request);
      setTimeout(() => {
        dropQueued(requestId, generation);
        settleWaitingRelays(app, requestId, generation);
      }, QUEUE_TIMEOUT_MS);
    } else if (readySubset.length === 0) {
      settleWaitingRelays(app, requestId, generation);
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

      if (now - request.createdAt > QUEUE_TIMEOUT_MS) {
        queuedRequests.splice(i, 1);
        settleWaitingRelays(request.app, request.requestId, request.generation);
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
        settleWaitingRelays(request.app, request.requestId, request.generation);
      }
    }
  };

  const sendFetch = (relayUrls, request) => {
    const batch = currentBatch(request.requestId, request.generation);
    if (!batch) {
      return;
    }
    startFetch(batch);

    const settleFetch = () => {
      const current = currentBatch(request.requestId, request.generation);
      if (current && finishFetch(current)) {
        completeBatch(request.app, request.requestId, request.generation);
      }
    };

    try {
      const relaySet = NDKRelaySet.fromRelayUrls(relayUrls, ndk, false, ndk.pool);

      ndk.fetchEvents(request.filters, { closeOnEose: request.closeOnEose }, relaySet).then((ndkEvents) => {
        return processEvents(request.app, request.requestId, request.description, ndkEvents, request.filters, false);
      }).catch((err) => {
        debugLog('fetchEvents error', { relays: relayUrls, err });
      }).finally(settleFetch);
    } catch (err) {
      debugLog('fetchEvents error', { relays: relayUrls, err });
      settleFetch();
    }
  };

  return {
    markReady,
    markDisconnect,
    fetchEvents,
  };
}
