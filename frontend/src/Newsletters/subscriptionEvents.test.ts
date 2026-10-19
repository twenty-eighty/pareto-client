import assert from "node:assert/strict";
import test from "node:test";

import { isActiveContact } from "./EncryptedContacts/protocol.ts";
import {
  latestChanges,
  shouldApply,
  syncSubscriptionEvents,
  type SubscriptionChange,
  type SubscriptionContact,
  type SubscriptionContactsApi,
} from "./subscriptionEvents.ts";

function change(kind: "subscribe" | "unsubscribe", email: string, at: number): SubscriptionChange {
  return { kind, at, subscriber: { email } };
}

test("the newest event for an address wins, and an equal time stays unsubscribed", () => {
  const latest = latestChanges([
    change("subscribe", "Ada@Example.com", 100),
    change("unsubscribe", "ada@example.com", 200),
    change("subscribe", "ada@example.com", 200),
    change("subscribe", "other@example.com", 50),
  ]);

  assert.equal(latest.length, 2);
  const ada = latest.find((item) => String(item.subscriber.email).toLowerCase() === "ada@example.com");
  assert.equal(ada?.kind, "unsubscribe");
  assert.equal(ada?.at, 200);
});

test("an older subscribe does not undo a stored unsubscribe", () => {
  const contact = { email: "ada@example.com", datesub: 100, dateunsub: 200, dnd: true };
  assert.equal(shouldApply(change("subscribe", "ada@example.com", 100), contact), false);
  assert.equal(shouldApply(change("unsubscribe", "ada@example.com", 200), contact), false);
});

test("a newer unsubscribe is applied and the contact becomes inactive", () => {
  const contact = { email: "ada@example.com", datesub: 100, dnd: false };
  const event = change("unsubscribe", "ada@example.com", 200);
  assert.equal(shouldApply(event, contact), true);

  const stored = { ...contact, dnd: true, dateunsub: event.at, dateUnsubscription: null };
  assert.equal(isActiveContact(stored), false);
  assert.equal(shouldApply(event, stored), false);
});

test("a newer subscribe clears an older unsubscribe", () => {
  const contact = { email: "ada@example.com", datesub: 100, dateunsub: 200, dnd: true };
  const event = change("subscribe", "ada@example.com", 300);
  assert.equal(shouldApply(event, contact), true);

  const stored = { ...contact, dnd: false, datesub: event.at, dateunsub: null, dateUnsubscription: null };
  assert.equal(isActiveContact(stored), true);
  assert.equal(shouldApply(event, stored), false);
});

test("an unsubscribe for someone who is not stored yet is applied", () => {
  assert.equal(shouldApply(change("unsubscribe", "ada@example.com", 200), null), true);
});

test("applying a reversed event list leaves the newer unsubscribe in place", async () => {
  const stored = new Map<string, SubscriptionContact>();
  const api: SubscriptionContactsApi = {
    async searchContacts(term) {
      const contact = stored.get(term.trim().toLowerCase());
      return { contacts: contact ? [contact] : [] };
    },
    async updateContact(id, contact) {
      stored.set(String(contact.email).trim().toLowerCase(), { ...contact, id });
    },
    async storeContactsBulk(contacts) {
      for (const contact of contacts) {
        stored.set(String(contact.email).trim().toLowerCase(), { ...contact, id: "1" });
      }
    },
  };

  const events = [
    change("unsubscribe", "ada@example.com", 200),
    change("subscribe", "ada@example.com", 100),
  ];
  await syncSubscriptionEvents(api, events, true);
  assert.equal(isActiveContact(stored.get("ada@example.com") || {}), false);

  await syncSubscriptionEvents(api, [...events].reverse(), true);
  assert.equal(isActiveContact(stored.get("ada@example.com") || {}), false);
  assert.equal(stored.get("ada@example.com")?.dateunsub, 200);
});
