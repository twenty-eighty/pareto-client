export type RecipientIdentity = {
  email?: string;
  Email?: string;
  emailAddress?: string;
};

type DeliveryCounts = {
  done?: number;
  failed?: number;
  held?: number;
  available?: number;
  leased?: number;
  retryable?: number;
};

export type DeliveryStatus = {
  counts?: DeliveryCounts | null;
  expected_jobs?: number | null;
  uploaded_jobs?: number | null;
};

export type CampaignProgress = {
  uploaded: number;
  delivered: number;
  sent: number;
  total: number;
};

export function recipientKey(contact: RecipientIdentity): string {
  return String(contact.email || contact.Email || contact.emailAddress || "").trim().toLowerCase();
}

export function uniqueRecipients<T extends RecipientIdentity>(contacts: T[]): T[] {
  const seen = new Set<string>();
  const unique: T[] = [];
  for (const contact of contacts) {
    const key = recipientKey(contact);
    if (!key || seen.has(key)) continue;
    seen.add(key);
    unique.push(contact);
  }
  return unique;
}

export function progressDeliveryCounts(
  status?: DeliveryStatus | null,
  fallbackTotal = 0,
  queued = 0,
): CampaignProgress {
  const counts = status?.counts;
  const delivered = counts?.done ?? 0;
  const stored = counts
    ? (counts.done || 0) +
      (counts.failed || 0) +
      (counts.held || 0) +
      (counts.available || 0) +
      (counts.leased || 0) +
      (counts.retryable || 0)
    : 0;
  const reportedUploaded = typeof status?.uploaded_jobs === "number" ? status.uploaded_jobs : 0;
  const expected = status?.expected_jobs;
  const committedTotal = typeof expected === "number" && expected > 0 ? expected : 0;
  const total = fallbackTotal || committedTotal || stored;
  const uploaded = Math.max(queued, stored, reportedUploaded);
  return {
    uploaded: total > 0 ? Math.min(uploaded, total) : uploaded,
    delivered: total > 0 ? Math.min(delivered, total) : delivered,
    sent: delivered,
    total,
  };
}

const TERMINAL_DELIVERIES = new Set(["sent", "failed", "partial"]);

/** The queue's terminal word is only final once every expected job has left the queue. */
export function campaignSettled(status?: DeliveryStatus | null, delivery?: string | null): boolean {
  if (!delivery || !TERMINAL_DELIVERIES.has(delivery)) {
    return false;
  }
  const done = status?.counts?.done ?? 0;
  const failed = status?.counts?.failed ?? 0;
  const expected = status?.expected_jobs ?? 0;
  return expected <= 0 || done + failed >= expected;
}
