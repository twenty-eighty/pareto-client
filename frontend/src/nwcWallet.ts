import { NWAClient, NWCClient } from "@getalby/sdk";

const STORAGE_KEY = "nwcUri";
const ALBY_AUTH_URL = "https://my.albyhub.com/apps/new";
const NWA_RELAYS = ["wss://relay.getalby.com/v1"];
const APP_NAME = "Pareto";
/** Absolute URL — wallets fetch this when showing the connection request. */
const APP_ICON = "https://pareto.space/images/icon/android-icon-192x192.png";

const NWA_METHODS = [
  "get_info",
  "get_balance",
  "pay_invoice",
  "make_invoice",
  "lookup_invoice",
  "list_transactions",
] as const;

export type NwcStatus = {
  connected: boolean;
  alias?: string;
  network?: string;
  methods?: string[];
  lud16?: string;
  /** Balance in sats when the wallet supports get_balance. */
  balanceSats?: number;
  /** Truncated wallet service pubkey for identification. */
  walletPubkeyShort?: string;
  weblnAvailable: boolean;
  /** True when NWC URI is stored or WebLN can pay invoices. */
  canAutoPay: boolean;
};

let cachedClient: NWCClient | null = null;
let cachedUri: string | null = null;
let activeNwa: {
  client: NWAClient;
  unsub: (() => void) | null;
} | null = null;

function normalizeUri(uri: string): string {
  return uri.trim();
}

export function getStoredUri(): string | null {
  try {
    const value = localStorage.getItem(STORAGE_KEY);
    return value && value.trim() ? value.trim() : null;
  } catch {
    return null;
  }
}

export function clearStoredUri(): void {
  try {
    localStorage.removeItem(STORAGE_KEY);
  } catch {
    // ignore storage errors
  }
  closeClient();
}

function storeUri(uri: string): void {
  localStorage.setItem(STORAGE_KEY, uri);
}

function closeClient(): void {
  if (cachedClient) {
    try {
      cachedClient.close();
    } catch {
      // ignore
    }
  }
  cachedClient = null;
  cachedUri = null;
}

function getClient(uri: string): NWCClient {
  const normalized = normalizeUri(uri);
  if (cachedClient && cachedUri === normalized) {
    return cachedClient;
  }
  closeClient();
  cachedClient = new NWCClient({ nostrWalletConnectUrl: normalized });
  cachedUri = normalized;
  return cachedClient;
}

function cacheClient(client: NWCClient): string {
  const uri = client.getNostrWalletConnectUrl(true);
  closeClient();
  storeUri(uri);
  cachedClient = client;
  cachedUri = uri;
  return uri;
}

function shortenPubkey(pubkey: string | undefined | null): string | undefined {
  if (!pubkey || pubkey.length < 16) {
    return pubkey || undefined;
  }
  return `${pubkey.slice(0, 8)}…${pubkey.slice(-8)}`;
}

async function statusFromClient(client: NWCClient): Promise<NwcStatus> {
  const walletPubkeyShort = shortenPubkey(client.walletPubkey);
  let alias: string | undefined;
  let network: string | undefined;
  let methods: string[] | undefined;
  let lud16: string | undefined = client.lud16 || undefined;
  let balanceSats: number | undefined;

  try {
    const info = await client.getInfo();
    if (info.lud16) {
      lud16 = info.lud16;
    }
    const rawAlias = info.alias && info.alias.trim() ? info.alias.trim() : undefined;
    // Keep alias distinct from lud16 so the UI does not show the same value twice.
    alias = rawAlias && rawAlias !== lud16 ? rawAlias : undefined;
    network = info.network || undefined;
    methods = info.methods;
  } catch {
    // keep partial identity from the client / URI
  }

  if (methods?.includes("get_balance") || !methods) {
    try {
      const balance = await client.getBalance();
      if (typeof balance?.balance === "number") {
        // NIP-47 balance is in millisats
        balanceSats = Math.floor(balance.balance / 1000);
      }
    } catch {
      // optional
    }
  }

  return withWebln({
    connected: true,
    alias,
    network,
    methods,
    lud16,
    balanceSats,
    walletPubkeyShort,
  });
}

export function isWeblnAvailable(): boolean {
  return typeof window !== "undefined" && !!(window as any).webln;
}

function withWebln(partial: Omit<NwcStatus, "weblnAvailable" | "canAutoPay">): NwcStatus {
  const weblnAvailable = isWeblnAvailable();
  return {
    ...partial,
    weblnAvailable,
    canAutoPay: !!partial.connected || weblnAvailable,
  };
}

export function getStatusSync(): NwcStatus {
  const uri = getStoredUri();
  return withWebln({ connected: !!uri });
}

export async function connect(uri: string): Promise<NwcStatus> {
  cancelNwa();
  const normalized = normalizeUri(uri);
  if (!normalized) {
    throw new Error("Paste a nostr+walletconnect:// URI");
  }
  if (
    !normalized.startsWith("nostr+walletconnect://") &&
    !normalized.startsWith("nostrwalletconnect://")
  ) {
    throw new Error("URI must start with nostr+walletconnect://");
  }

  const client = getClient(normalized);
  const status = await statusFromClient(client);
  storeUri(normalized);
  return status;
}

export async function connectWithAlby(): Promise<NwcStatus> {
  cancelNwa();
  const client = await NWCClient.fromAuthorizationUrl(ALBY_AUTH_URL, {
    name: APP_NAME,
    icon: APP_ICON,
    requestMethods: [...NWA_METHODS],
  });
  cacheClient(client);
  return statusFromClient(client);
}

export type NwaStartResult = {
  connectionUri: string;
};

export async function startNwa(
  onConnected: (status: NwcStatus) => void,
  onError: (reason: string) => void
): Promise<NwaStartResult> {
  cancelNwa();
  const client = new NWAClient({
    relayUrls: NWA_RELAYS,
    requestMethods: [...NWA_METHODS],
    name: APP_NAME,
    icon: APP_ICON,
  });
  activeNwa = { client, unsub: null };
  try {
    const { unsub } = await client.subscribe({
      onSuccess: async (nwcClient) => {
        try {
          cacheClient(nwcClient);
          cancelNwa();
          onConnected(await statusFromClient(nwcClient));
        } catch (error: any) {
          onError(error?.message || "NWA connection failed");
        }
      },
    });
    if (activeNwa?.client === client) {
      activeNwa.unsub = unsub;
    } else {
      unsub();
    }
    return { connectionUri: client.connectionUri };
  } catch (error: any) {
    cancelNwa();
    throw error;
  }
}

export function cancelNwa(): void {
  if (!activeNwa) {
    return;
  }
  try {
    activeNwa.unsub?.();
  } catch {
    // ignore
  }
  try {
    activeNwa.client.close();
  } catch {
    // ignore
  }
  activeNwa = null;
}

export async function enableWebln(): Promise<NwcStatus> {
  if (!isWeblnAvailable()) {
    throw new Error("No WebLN provider found. Install a Lightning browser extension such as Alby.");
  }
  const webln = (window as any).webln;
  await webln.enable();
  // WebLN does not create an NWC URI; it is used at payment time.
  return withWebln({
    connected: !!getStoredUri(),
    alias: getStoredUri() ? undefined : "WebLN",
  });
}

export async function refreshStatus(): Promise<NwcStatus> {
  const uri = getStoredUri();
  if (!uri) {
    return withWebln({ connected: false });
  }
  try {
    const client = getClient(uri);
    return await statusFromClient(client);
  } catch {
    return withWebln({ connected: true });
  }
}

export function disconnect(): NwcStatus {
  cancelNwa();
  clearStoredUri();
  return withWebln({ connected: false });
}

async function payWithWebln(invoice: string): Promise<{ preimage: string }> {
  if (!isWeblnAvailable()) {
    throw new Error("WEBLN_NOT_AVAILABLE");
  }
  const webln = (window as any).webln;
  if (!webln.enabled) {
    await webln.enable();
  }
  const result = await webln.sendPayment(invoice);
  const preimage = result?.preimage || result?.paymentPreimage || "";
  return { preimage };
}

export async function payInvoice(invoice: string): Promise<{ preimage: string }> {
  const uri = getStoredUri();
  if (uri) {
    const client = getClient(uri);
    const result = await client.payInvoice({ invoice });
    return { preimage: result.preimage };
  }
  if (isWeblnAvailable()) {
    return payWithWebln(invoice);
  }
  throw new Error("NWC_NOT_CONNECTED");
}
