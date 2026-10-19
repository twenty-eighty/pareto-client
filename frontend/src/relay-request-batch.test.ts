import assert from "node:assert/strict";
import test from "node:test";

import { createRequestBatch, finishFetch, relaysSettled, startFetch } from "./relay-request-batch.ts";

test("one fetch completes the request when every relay was already ready", () => {
  const batch = createRequestBatch(1, false);
  startFetch(batch);
  assert.equal(finishFetch(batch), true);
  assert.equal(finishFetch(batch), false);
});

test("a ready subset does not complete the request while other relays are still connecting", () => {
  const batch = createRequestBatch(1, true);
  startFetch(batch);
  assert.equal(finishFetch(batch), false);

  startFetch(batch);
  assert.equal(relaysSettled(batch), false);
  assert.equal(finishFetch(batch), true);
  assert.equal(relaysSettled(batch), false);
});

test("giving up on unconnected relays completes a request that has no fetch left", () => {
  const batch = createRequestBatch(1, true);
  assert.equal(relaysSettled(batch), true);
  assert.equal(relaysSettled(batch), false);
});

test("a fetch that starts after the request settled does not complete it again", () => {
  const batch = createRequestBatch(1, false);
  startFetch(batch);
  assert.equal(finishFetch(batch), true);
  startFetch(batch);
  assert.equal(batch.pending, 0);
  assert.equal(finishFetch(batch), false);
});
