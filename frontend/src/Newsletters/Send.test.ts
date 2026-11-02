/// <reference types="node" />
import assert from "node:assert/strict";
import test from "node:test";

import { campaignSettled, progressDeliveryCounts, uniqueRecipients } from "./deliveryProgress.ts";

test("duplicate recipients collapse to one job", () => {
  const unique = uniqueRecipients([
    { email: "Ada@Example.com" },
    { email: "ada@example.com " },
    { Email: "other@example.com" },
    { emailAddress: "OTHER@example.com" },
    { firstName: "no email" },
  ]);

  assert.deepEqual(
    unique.map((contact) => contact.email || contact.Email || contact.emailAddress),
    ["Ada@Example.com", "other@example.com"],
  );
});

test("uploading reports queued rows separately from the recipient total", () => {
  const uploading = progressDeliveryCounts(
    {
      counts: { done: 0, failed: 0, held: 400, available: 0, leased: 0, retryable: 0 },
      expected_jobs: null,
      uploaded_jobs: 400,
    },
    1320,
    500,
  );

  assert.deepEqual(uploading, { uploaded: 500, delivered: 0, sent: 0, total: 1320 });
});

test("delivery advances while the recipient total stays put", () => {
  const sending = progressDeliveryCounts(
    {
      counts: { done: 25, failed: 0, held: 0, available: 1094, leased: 0, retryable: 0 },
      expected_jobs: 1119,
      uploaded_jobs: 1119,
    },
    1320,
    1119,
  );

  assert.deepEqual(sending, { uploaded: 1119, delivered: 25, sent: 25, total: 1320 });
});

test("a sent status is not final until every expected job is done", () => {
  const early = {
    counts: { done: 5, failed: 0, held: 0, available: 1495, leased: 0, retryable: 0 },
    expected_jobs: 1500,
    uploaded_jobs: 1500,
  };

  assert.equal(campaignSettled(early, "sent"), false);
  assert.equal(campaignSettled({ ...early, counts: { ...early.counts, done: 1500, available: 0 } }, "sent"), true);
  assert.equal(campaignSettled(early, "sending"), false);
});
