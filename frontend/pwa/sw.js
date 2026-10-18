/* Pareto client service worker.
 *
 * BUILD_VERSION is replaced at build time so each deploy produces a new
 * SW byte-for-byte and browsers pick up the update.
 */
importScripts("/newsletter-sender-sw.js");

const BUILD_VERSION = "__PARETO_BUILD_VERSION__";
const CACHE_NAME = "pareto-" + BUILD_VERSION;

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME).then((cache) =>
      cache.addAll([
        "/manifest.webmanifest",
        "/images/icon/android-icon-192x192.png",
        "/images/icon/android-icon-512x512.png",
      ])
    )
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys
            .filter((key) => key.startsWith("pareto-") && key !== CACHE_NAME)
            .map((key) => caches.delete(key))
        )
      )
      .then(() => self.clients.claim())
  );
});

self.addEventListener("message", (event) => {
  if (event.data && event.data.type === "SKIP_WAITING") {
    self.skipWaiting();
  }
});

self.addEventListener("fetch", (event) => {
  const request = event.request;
  if (request.method !== "GET") {
    return;
  }

  const url = new URL(request.url);
  if (url.origin !== self.location.origin) {
    return;
  }

  // Always hit the network for version probes and worker scripts.
  // The installed worker must not intercept importScripts of the sender script,
  // or a failed fetch here aborts evaluation of the next sw.js.
  if (
    url.pathname === "/version.json" ||
    url.pathname === "/sw.js" ||
    url.pathname === "/newsletter-sender-sw.js"
  ) {
    return;
  }

  if (
    url.pathname.startsWith("/api/") ||
    url.pathname.startsWith("/.well-known/")
  ) {
    return;
  }

  if (url.pathname.startsWith("/assets/")) {
    event.respondWith(cacheFirst(request));
    return;
  }

  event.respondWith(networkFirst(request));
});

async function cacheFirst(request) {
  const cached = await caches.match(request);
  if (cached) {
    return cached;
  }
  try {
    const response = await fetch(request);
    if (response && response.ok && response.type === "basic") {
      const cache = await caches.open(CACHE_NAME);
      cache.put(request, response.clone()).catch(() => {});
    }
    return response;
  } catch {
    return offlineResponse();
  }
}

async function networkFirst(request) {
  try {
    const response = await fetch(request);
    if (response && response.ok && response.type === "basic") {
      const cache = await caches.open(CACHE_NAME);
      cache.put(request, response.clone()).catch(() => {});
    }
    return response;
  } catch {
    const cached = await caches.match(request);
    if (cached) {
      return cached;
    }
    return offlineResponse();
  }
}

function offlineResponse() {
  return new Response("", {
    status: 504,
    statusText: "Gateway Timeout",
  });
}
