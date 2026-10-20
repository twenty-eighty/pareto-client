export type RequestBatch = {
  generation: number;
  pending: number;
  waitingForRelays: boolean;
  finished: boolean;
};

export function createRequestBatch(generation: number, waitingForRelays: boolean): RequestBatch {
  return {
    generation,
    pending: 0,
    waitingForRelays,
    finished: false,
  };
}

export function startFetch(batch: RequestBatch): void {
  if (batch.finished) {
    return;
  }
  batch.pending += 1;
}

/** True when this request has no fetches left and no relays left to wait for. */
export function finishFetch(batch: RequestBatch): boolean {
  if (batch.finished) {
    return false;
  }
  batch.pending = Math.max(0, batch.pending - 1);
  return settle(batch);
}

/** Stop waiting for relays that have not connected. In-flight fetches still have to finish. */
export function relaysSettled(batch: RequestBatch): boolean {
  if (batch.finished) {
    return false;
  }
  batch.waitingForRelays = false;
  return settle(batch);
}

function settle(batch: RequestBatch): boolean {
  if (batch.pending === 0 && !batch.waitingForRelays) {
    batch.finished = true;
    return true;
  }
  return false;
}
