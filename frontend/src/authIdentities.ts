/**
 * Multi-identity store + NDK signer activation.
 * Multi-identity auth: extension / npub / bunker / ncryptsec.
 * Raw nsec is never persisted; ncryptsec may be stored in localStorage.
 */

import NDK, {
  NDKNip07Signer,
  NDKNip46Signer,
  NDKPrivateKeySigner,
  NDKUser,
  nip19,
} from "@nostr-dev-kit/ndk";

const STORAGE_KEY = "pareto.auth.identities.v1";

export type AuthMethod = "extension" | "npub" | "bunker" | "ncryptsec";

export type StoredIdentity = {
  id: string;
  method: AuthMethod;
  pubkey: string;
  label?: string;
  /** NIP-49 encrypted key — never store raw nsec */
  ncryptsec?: string;
  /** bunker:// or nostrconnect:// URI */
  bunkerUri?: string;
  createdAt: number;
};

type IdentityStore = {
  identities: StoredIdentity[];
  activeId: string | null;
};

type ElmApp = {
  ports: {
    receiveMessage: {
      send: (msg: { messageType: string; value: any }) => void;
    };
  };
};

function emptyStore(): IdentityStore {
  return { identities: [], activeId: null };
}

function loadStore(): IdentityStore {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (!raw) {
      return emptyStore();
    }
    const parsed = JSON.parse(raw) as IdentityStore;
    if (!parsed || !Array.isArray(parsed.identities)) {
      return emptyStore();
    }
    return {
      identities: parsed.identities,
      activeId: parsed.activeId ?? null,
    };
  } catch {
    return emptyStore();
  }
}

function saveStore(store: IdentityStore): void {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(store));
}

function publicIdentity(identity: StoredIdentity) {
  return {
    id: identity.id,
    method: identity.method,
    pubkey: identity.pubkey,
    label: identity.label ?? null,
    locked: identity.method === "ncryptsec",
    createdAt: identity.createdAt,
  };
}

function sendIdentities(app: ElmApp, store: IdentityStore = loadStore()): void {
  app.ports.receiveMessage.send({
    messageType: "identities",
    value: {
      identities: store.identities.map(publicIdentity),
      activeId: store.activeId,
    },
  });
}

function methodToLoginMethod(method: AuthMethod): string {
  switch (method) {
    case "extension":
      return "extension";
    case "bunker":
      return "connect";
    case "ncryptsec":
      return "local";
    case "npub":
      return "readonly";
  }
}

function sendUser(app: ElmApp, pubkey: string, method: AuthMethod): void {
  app.ports.receiveMessage.send({
    messageType: "user",
    value: { pubKey: pubkey, method: methodToLoginMethod(method) },
  });
}

function sendAuthError(app: ElmApp, reason: string): void {
  app.ports.receiveMessage.send({
    messageType: "authError",
    value: { reason },
  });
}

function newId(): string {
  if (typeof crypto !== "undefined" && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return `id-${Date.now()}-${Math.random().toString(16).slice(2)}`;
}

function normalizeHexPubkey(pubkey: string): string {
  return pubkey.toLowerCase();
}

function pubkeyFromNpubOrHex(value: string): string {
  const trimmed = value.trim();
  if (/^[0-9a-fA-F]{64}$/.test(trimmed)) {
    return normalizeHexPubkey(trimmed);
  }
  const decoded = nip19.decode(trimmed);
  if (decoded.type !== "npub") {
    throw new Error("Expected an npub or hex pubkey");
  }
  return normalizeHexPubkey(decoded.data as string);
}

function upsertIdentity(store: IdentityStore, identity: StoredIdentity): IdentityStore {
  const existingIndex = store.identities.findIndex(
    (item) => item.pubkey === identity.pubkey && item.method === identity.method,
  );
  if (existingIndex >= 0) {
    const existing = store.identities[existingIndex];
    const merged: StoredIdentity = {
      ...existing,
      ...identity,
      id: existing.id,
    };
    const identities = store.identities.map((item, index) =>
      index === existingIndex ? merged : item,
    );
    return { identities, activeId: existing.id };
  }
  return {
    identities: [...store.identities, identity],
    activeId: identity.id,
  };
}

async function activateSigner(
  ndk: NDK,
  app: ElmApp,
  identity: StoredIdentity,
  password?: string,
): Promise<void> {
  switch (identity.method) {
    case "extension": {
      if (!(window as any).nostr) {
        throw new Error("No browser extension found (window.nostr)");
      }
      const signer = new NDKNip07Signer();
      const user = await signer.user();
      if (normalizeHexPubkey(user.pubkey) !== identity.pubkey) {
        throw new Error("Extension account does not match this identity");
      }
      ndk.signer = signer;
      break;
    }
    case "npub": {
      ndk.signer = undefined;
      ndk.activeUser = new NDKUser({ pubkey: identity.pubkey });
      break;
    }
    case "bunker": {
      if (!identity.bunkerUri) {
        throw new Error("Missing bunker connection");
      }
      const signer = NDKNip46Signer.bunker(ndk, identity.bunkerUri);
      await signer.blockUntilReady();
      const user = await signer.user();
      if (normalizeHexPubkey(user.pubkey) !== identity.pubkey) {
        throw new Error("Bunker pubkey does not match this identity");
      }
      ndk.signer = signer;
      break;
    }
    case "ncryptsec": {
      if (!identity.ncryptsec) {
        throw new Error("Missing ncryptsec");
      }
      if (!password) {
        throw new Error("Password required to unlock this identity");
      }
      const signer = NDKPrivateKeySigner.fromNcryptsec(identity.ncryptsec, password, ndk);
      const user = await signer.user();
      if (normalizeHexPubkey(user.pubkey) !== identity.pubkey) {
        throw new Error("Decrypted key does not match this identity");
      }
      ndk.signer = signer;
      break;
    }
  }

  const store = loadStore();
  store.activeId = identity.id;
  saveStore(store);
  sendIdentities(app, store);
  sendUser(app, identity.pubkey, identity.method);
}

export function publishIdentities(app: ElmApp): void {
  sendIdentities(app);
}

export async function restoreActiveIdentity(ndk: NDK, app: ElmApp): Promise<void> {
  const store = loadStore();
  sendIdentities(app, store);
  if (!store.activeId) {
    return;
  }
  const identity = store.identities.find((item) => item.id === store.activeId);
  if (!identity) {
    return;
  }
  if (identity.method === "ncryptsec") {
    app.ports.receiveMessage.send({
      messageType: "authNeedsUnlock",
      value: { id: identity.id, pubkey: identity.pubkey },
    });
    return;
  }
  try {
    await activateSigner(ndk, app, identity);
  } catch (error: any) {
    sendAuthError(app, error?.message || "Failed to restore identity");
  }
}

export async function handleAuthCommand(
  ndk: NDK,
  app: ElmApp,
  command: string,
  value: any,
): Promise<boolean> {
  try {
    switch (command) {
      case "listIdentities":
        sendIdentities(app);
        return true;

      case "logout": {
        ndk.signer = undefined;
        const store = loadStore();
        store.activeId = null;
        saveStore(store);
        sendIdentities(app, store);
        app.ports.receiveMessage.send({ messageType: "loggedOut", value: null });
        return true;
      }

      case "activateIdentity": {
        const store = loadStore();
        const identity = store.identities.find((item) => item.id === value.id);
        if (!identity) {
          throw new Error("Identity not found");
        }
        await activateSigner(ndk, app, identity, value.password);
        return true;
      }

      case "removeIdentity": {
        const store = loadStore();
        const wasActive = store.activeId === value.id;
        store.identities = store.identities.filter((item) => item.id !== value.id);
        if (wasActive) {
          store.activeId = null;
          ndk.signer = undefined;
          app.ports.receiveMessage.send({ messageType: "loggedOut", value: null });
        }
        saveStore(store);
        sendIdentities(app, store);
        return true;
      }

      case "loginWithExtension": {
        if (!(window as any).nostr) {
          throw new Error("No browser extension found (window.nostr)");
        }
        const signer = new NDKNip07Signer();
        const user = await signer.user();
        const identity: StoredIdentity = {
          id: newId(),
          method: "extension",
          pubkey: normalizeHexPubkey(user.pubkey),
          label: value?.label || "Browser extension",
          createdAt: Date.now(),
        };
        const store = upsertIdentity(loadStore(), identity);
        saveStore(store);
        ndk.signer = signer;
        sendIdentities(app, store);
        sendUser(app, identity.pubkey, "extension");
        return true;
      }

      case "loginWithNpub": {
        const pubkey = pubkeyFromNpubOrHex(value.npub);
        const identity: StoredIdentity = {
          id: newId(),
          method: "npub",
          pubkey,
          label: value?.label || "Read-only",
          createdAt: Date.now(),
        };
        const store = upsertIdentity(loadStore(), identity);
        saveStore(store);
        ndk.signer = undefined;
        ndk.activeUser = new NDKUser({ pubkey });
        sendIdentities(app, store);
        sendUser(app, pubkey, "npub");
        return true;
      }

      case "loginWithBunker": {
        const bunkerUri = String(value.bunkerUri || "").trim();
        if (!bunkerUri) {
          throw new Error("Bunker URI is required");
        }
        const signer = NDKNip46Signer.bunker(ndk, bunkerUri);
        await signer.blockUntilReady();
        const user = await signer.user();
        const identity: StoredIdentity = {
          id: newId(),
          method: "bunker",
          pubkey: normalizeHexPubkey(user.pubkey),
          label: value?.label || "Bunker",
          bunkerUri,
          createdAt: Date.now(),
        };
        const store = upsertIdentity(loadStore(), identity);
        saveStore(store);
        ndk.signer = signer;
        sendIdentities(app, store);
        sendUser(app, identity.pubkey, "bunker");
        return true;
      }

      case "loginWithNcryptsec": {
        const ncryptsec = String(value.ncryptsec || "").trim();
        const password = String(value.password || "");
        if (!ncryptsec.startsWith("ncryptsec")) {
          throw new Error("Expected an ncryptsec string");
        }
        if (!password) {
          throw new Error("Password is required");
        }
        const signer = NDKPrivateKeySigner.fromNcryptsec(ncryptsec, password, ndk);
        const user = await signer.user();
        const identity: StoredIdentity = {
          id: newId(),
          method: "ncryptsec",
          pubkey: normalizeHexPubkey(user.pubkey),
          label: value?.label || "Encrypted key",
          ncryptsec,
          createdAt: Date.now(),
        };
        const store = upsertIdentity(loadStore(), identity);
        saveStore(store);
        ndk.signer = signer;
        sendIdentities(app, store);
        sendUser(app, identity.pubkey, "ncryptsec");
        return true;
      }

      default:
        return false;
    }
  } catch (error: any) {
    sendAuthError(app, error?.message || String(error));
    return true;
  }
}
