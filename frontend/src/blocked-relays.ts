/** Host-only identity, matching Elm `Relay.toKey`. */
export function relayHost(url: string): string {
  return String(url || "")
    .trim()
    .replace(/^wss?:\/\//i, "")
    .replace(/^https?:\/\//i, "")
    .replace(/\/+$/, "")
    .toLowerCase();
}

const blockedHosts = new Set<string>(["relay.nostr.band"]);

export function setBlockedRelayUrls(urls: string[] | undefined | null): void {
  blockedHosts.clear();
  blockedHosts.add("relay.nostr.band");
  (urls || []).forEach((url) => {
    const host = relayHost(url);
    if (host) {
      blockedHosts.add(host);
    }
  });
}

export function isBlockedRelayUrl(url: string): boolean {
  return blockedHosts.has(relayHost(url));
}

export function filterRelayUrls(urls: string[]): string[] {
  return (urls || []).filter((url) => !isBlockedRelayUrl(url));
}
