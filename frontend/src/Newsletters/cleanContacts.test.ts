import assert from "node:assert/strict";
import test from "node:test";

import { isActiveContact, sha256Hex } from "./EncryptedContacts/protocol.ts";
import {
  CLASSIFY_BATCH_SIZE,
  CLASSIFY_INTERVAL_MS,
  applyDeliverability,
  classifyEmails,
  cleanContactList,
  deliverabilityApiBaseUrl,
  type CleanableContact,
} from "./cleanContacts.ts";

const author = "ab".repeat(32);

test("cleaning classifies against the deployed deliverability server", () => {
  assert.equal(deliverabilityApiBaseUrl(), "https://deliverability.pareto.space");
});

function contact(id: string, email: string, extra: Record<string, unknown> = {}): CleanableContact {
  return { id, email, active: true, ...extra };
}

test("an undeliverable contact is inactive", () => {
  assert.equal(isActiveContact({ email: "ada@example.com", undeliverable: true }), false);
  assert.equal(isActiveContact({ email: "ada@example.com", undeliverable: "true" }), false);
  assert.equal(isActiveContact({ email: "ada@example.com", reason: "unsubscribed" }), false);
  assert.equal(isActiveContact({ email: "ada@example.com", reason: "blocked" }), false);
  assert.equal(isActiveContact({ email: "ada@example.com", reason: "catch-all" }), true);
  assert.equal(isActiveContact({ email: "ada@example.com" }), true);
});

test("classify writes a verdict once and leaves an unknown address untouched", () => {
  const now = 1_700_000_000_000;
  const unknown = applyDeliverability(contact("1", "ada@example.com"), { status: "unknown" }, now);
  assert.equal(unknown.changed, false);
  assert.equal(unknown.contact.verifiedAt, undefined);
  assert.equal(unknown.contact.reason, undefined);

  const bad = applyDeliverability(contact("1", "ada@example.com"), { status: "undeliverable", reason: "5.1.1" }, now);
  assert.equal(bad.changed, true);
  assert.equal(bad.contact.undeliverable, true);
  assert.equal(bad.contact.reason, "5.1.1");
  assert.equal(bad.contact.verifiedAt, now);
  assert.equal(isActiveContact(bad.contact), false);

  const again = applyDeliverability({ ...bad.contact, active: false }, { status: "undeliverable", reason: "5.1.1" }, now + 10);
  assert.equal(again.changed, false);
  assert.equal(again.contact.verifiedAt, now);

  const recovered = applyDeliverability({ ...bad.contact, active: false }, { status: "valid" }, now + 10);
  assert.equal(recovered.changed, true);
  assert.equal(recovered.contact.undeliverable, undefined);
  assert.equal(recovered.contact.reason, undefined);
  assert.equal(isActiveContact(recovered.contact), true);

  const blocked = applyDeliverability(contact("1", "ada@example.com"), { status: "blocked", reason: "unsubscribed" }, now);
  assert.equal(blocked.changed, true);
  assert.equal(blocked.contact.undeliverable, undefined);
  assert.equal(blocked.contact.reason, "unsubscribed");
  assert.equal(blocked.contact.verifiedAt, now);
  assert.equal(isActiveContact(blocked.contact), false);

  const blockedAgain = applyDeliverability(
    { ...blocked.contact, active: true },
    { status: "blocked", reason: "unsubscribed" },
    now + 10,
  );
  assert.equal(blockedAgain.changed, true);

  const blockedStored = applyDeliverability(
    { ...blocked.contact, active: false },
    { status: "blocked", reason: "unsubscribed" },
    now + 10,
  );
  assert.equal(blockedStored.changed, false);

  const uncertain = applyDeliverability(contact("1", "ada@example.com"), { status: "uncertain", reason: "catch-all" }, now);
  assert.equal(uncertain.changed, true);
  assert.equal(uncertain.contact.reason, "catch-all");
  assert.equal(isActiveContact(uncertain.contact), true);
});

test("cleaning keeps one page in memory and skips a second write", async () => {
  const pages = [
    Array.from({ length: CLASSIFY_BATCH_SIZE }, (_, index) => contact(String(index), `p1-${index}@example.com`)),
    [contact("last", "last@example.com", { undeliverable: true, verifiedAt: 50, reason: "5.1.1", active: false })],
  ];
  const held: number[] = [];
  const updates: CleanableContact[] = [];
  const batches: number[] = [];
  const waits: number[] = [];
  let pageLoads = 0;

  const result = await cleanContactList({
    now: () => 99,
    wait: async (ms) => {
      waits.push(ms);
    },
    api: {
      async getContacts(page = 1, perPage = 100) {
        pageLoads += 1;
        assert.equal(perPage, CLASSIFY_BATCH_SIZE);
        const rows = pages[page - 1] || [];
        held.push(rows.length);
        return { contacts: rows, sourceCount: rows.length, errors: [] };
      },
      async updateContact(_id, next) {
        updates.push(next);
      },
    },
    classify: async (emails) => {
      batches.push(emails.length);
      assert.ok(emails.length <= CLASSIFY_BATCH_SIZE);
      return emails.map((email) =>
        email === "last@example.com"
          ? { status: "undeliverable" as const, reason: "5.1.1" }
          : { status: "unknown" as const },
      );
    },
  });

  assert.equal(pageLoads, 2);
  assert.deepEqual(held, [100, 1]);
  assert.deepEqual(batches, [100, 1]);
  assert.equal(updates.length, 0);
  assert.equal(result.checked, 101);
  assert.equal(result.updated, 0);
  assert.equal(result.undeliverable, 1);
  assert.equal(result.done, true);
  assert.ok(waits.length === 1 && waits[0] <= CLASSIFY_INTERVAL_MS && waits[0] > 0);
});

test("a changed verdict is written and the next page is not fetched after cancel", async () => {
  let cancelled = false;
  const seenPages: number[] = [];
  const result = await cleanContactList({
    now: () => 10,
    wait: async () => undefined,
    isCancelled: () => cancelled,
    api: {
      async getContacts(page = 1) {
        seenPages.push(page);
        return {
          contacts: [contact("1", "Ada@Example.com")],
          sourceCount: CLASSIFY_BATCH_SIZE,
          errors: [],
        };
      },
      async updateContact(_id, next) {
        assert.equal(next.undeliverable, true);
        assert.equal(next.reason, "NOT_EXISTS");
        assert.equal(next.verifiedAt, 10);
        cancelled = true;
      },
    },
    classify: async () => [{ status: "undeliverable" as const, reason: "NOT_EXISTS" }],
  });

  assert.deepEqual(seenPages, [1]);
  assert.equal(result.cancelled, true);
  assert.equal(result.updated, 1);
});

test("classify posts the exact hashed body", async () => {
  const emails = ["Ada@Example.com"];
  const body = JSON.stringify({ namespace: "prod", emails: ["ada@example.com"], refresh: false });
  let signed = "";
  let sent = "";
  const statuses = await classifyEmails({
    baseUrl: "http://127.0.0.1:4466/",
    pubkey: author.toUpperCase(),
    emails,
    sign: async (request) => {
      assert.equal(request.url, `http://127.0.0.1:4466/v1/authors/${author}/classify`);
      assert.equal(request.method, "POST");
      assert.equal(request.payloadSha256, await sha256Hex(body));
      signed = "Nostr signed";
      return signed;
    },
    fetchImpl: async (url, init) => {
      assert.equal(url, `http://127.0.0.1:4466/v1/authors/${author}/classify`);
      assert.equal(init.headers.authorization, signed);
      sent = init.body;
      return new Response(JSON.stringify({ results: [{ status: "uncertain", reason: "catch-all" }] }), { status: 200 });
    },
  });

  assert.equal(sent, body);
  assert.deepEqual(statuses, [{ status: "uncertain", reason: "catch-all" }]);
});

test("classify waits and retries when the server is rate limited", async () => {
  let calls = 0;
  const waits: number[] = [];
  const statuses = await classifyEmails({
    baseUrl: "https://deliverability.pareto.space",
    pubkey: author,
    emails: ["ada@example.com"],
    sign: async () => "Nostr signed",
    wait: async (ms) => {
      waits.push(ms);
    },
    fetchImpl: async () => {
      calls += 1;
      if (calls === 1) {
        return new Response(JSON.stringify({ error: "rate_limited" }), {
          status: 429,
          headers: { "retry-after": "2" },
        });
      }
      return new Response(JSON.stringify({ results: [{ status: "valid", reason: "ignored" }] }), { status: 200 });
    },
  });

  assert.deepEqual(waits, [2000]);
  assert.deepEqual(statuses, [{ status: "valid" }]);
});
