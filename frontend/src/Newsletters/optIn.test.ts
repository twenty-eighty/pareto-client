import assert from "node:assert/strict";
import test from "node:test";

import {
  COURIER_PUBKEY,
  buildOptInPayload,
  interpretOptInResponse,
  optInEnvelope,
  queueApiBaseUrl,
  submitOptIn,
} from "./optIn.ts";

const author = "ab".repeat(32);
const kid = "cd".repeat(32);

test("the payload is the courier object and nothing else", () => {
  const built = buildOptInPayload({
    email: " Ada@Example.com ",
    author,
    firstName: " Ada ",
    lastName: "",
    pubkey: `  ${author}  `,
    locale: "de-DE",
    test: false,
    authorName: "Ignored",
    html: "<p>no</p>",
  } as never);

  assert.equal(built.ok, true);
  if (!built.ok) return;
  assert.deepEqual(built.payload, {
    email: "Ada@Example.com",
    author,
    test: false,
    firstName: "Ada",
    pubkey: author,
    locale: "de-DE",
  });
  assert.equal(JSON.stringify(built.payload).includes("authorName"), false);
  assert.equal(JSON.stringify(built.payload).includes("html"), false);
});

test("a test subscription sets the test namespace", () => {
  const built = buildOptInPayload({ email: "ada@example.com", author, test: true });
  assert.equal(built.ok, true);
  if (!built.ok) return;
  const envelope = optInEnvelope("ciphertext", kid, built.payload.test, "request-1");
  assert.equal(envelope.namespace, "test");
  assert.equal(envelope.enc.alg, "nip44");
  assert.equal(envelope.enc.kid, kid);
  assert.equal(envelope.idem, "request-1");
  assert.equal(envelope.size_bytes, Buffer.byteLength("ciphertext"));
});

test("size_bytes counts utf-8 bytes", () => {
  const envelope = optInEnvelope("ä", kid, false);
  assert.equal(envelope.namespace, "prod");
  assert.equal(envelope.size_bytes, 2);
  assert.equal("idem" in envelope, false);
});

test("an optional pubkey that is not a key is left out", () => {
  const built = buildOptInPayload({ email: "ada@example.com", author, pubkey: "npub1something" });
  assert.equal(built.ok, true);
  if (!built.ok) return;
  assert.equal(built.payload.pubkey, undefined);
});

test("SENT, QUEUED, and REJECTED stay distinct from transport errors", () => {
  assert.deepEqual(interpretOptInResponse(200, { job_id: "9", status: "SENT" }), {
    ok: true,
    jobId: "9",
    status: "SENT",
  });
  assert.deepEqual(interpretOptInResponse(202, { job_id: "9", status: "QUEUED" }), {
    ok: true,
    jobId: "9",
    status: "QUEUED",
  });
  assert.deepEqual(interpretOptInResponse(200, { job_id: "9", status: "REJECTED" }), {
    ok: true,
    jobId: "9",
    status: "REJECTED",
  });
  assert.deepEqual(
    interpretOptInResponse(429, { error: { code: "rate_limited", message: "Too many opt-in requests" } }),
    { ok: false, message: "Too many opt-in requests" },
  );
  assert.equal(interpretOptInResponse(400, {}).ok, false);
});

test("a local page posts opt-in to the local queue", () => {
  withHost("localhost", () => {
    assert.equal(queueApiBaseUrl(), "http://localhost:4433/v1");
  });
  withHost("127.0.0.1", () => {
    assert.equal(queueApiBaseUrl(), "http://localhost:4433/v1");
  });
  withHost("pareto.space", () => {
    assert.equal(queueApiBaseUrl(), "https://queue-server.pareto.space/v1");
  });
});

function withHost(hostname: string, run: () => void) {
  const previous = globalThis.window;
  globalThis.window = { location: { hostname } } as Window & typeof globalThis.window;
  try {
    run();
  } finally {
    globalThis.window = previous;
  }
}

test("submit posts the envelope and returns the queue status", async () => {
  const built = buildOptInPayload({ email: "ada@example.com", author, test: false });
  assert.equal(built.ok, true);
  if (!built.ok) return;

  let posted: { url: string; body: unknown } | null = null;
  const fetchImpl: typeof fetch = async (url, init) => {
    posted = { url: String(url), body: JSON.parse(String(init?.body)) };
    return new Response(JSON.stringify({ job_id: "42", status: "QUEUED" }), { status: 202 });
  };

  const result = await submitOptIn({
    payload: built.payload,
    ciphertext: "cipher",
    kid,
    requestId: "7",
    baseUrl: "http://127.0.0.1:4433/v1",
    fetchImpl,
  });

  assert.deepEqual(result, { ok: true, jobId: "42", status: "QUEUED" });
  assert.equal(posted?.url, "http://127.0.0.1:4433/v1/opt-in");
  assert.deepEqual(posted?.body, {
    namespace: "prod",
    idem: "7",
    ciphertext_b64: "cipher",
    enc: { alg: "nip44", kid },
    size_bytes: 6,
  });
  assert.equal(COURIER_PUBKEY.length, 64);
});
