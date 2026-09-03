/**
 * Multi-identity store + NDK signer activation.
 * Multi-identity auth: extension / npub / bunker / ncryptsec / passkey.
 * Raw nsec is never persisted; ncryptsec may be stored in localStorage.
 */

import NDK, {
  NDKNip07Signer,
  NDKNip46Signer,
  NDKPrivateKeySigner,
  NDKUser,
  nip19,
} from "@nostr-dev-kit/ndk";
import {
  createPasskeyForNsec,
  dismissPasskeyPrompt,
  hexToNsecBytes,
  loginWithPasskey,
  mapKeytrError,
  reportPasskeySupport,
  shouldOfferPasskeyCreate,
} from "./keytrAuth";

const STORAGE_KEY = "pareto.auth.identities.v1";
const BOOTSTRAP_KEY = "pareto.auth.bootstrap.v1";

export type AuthMethod = "extension" | "npub" | "bunker" | "ncryptsec" | "passkey";

export type StoredIdentity = {
  id: string;
  method: AuthMethod;
  pubkey: string;
  label?: string;
  email?: string;
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
    label: identity.label ?? identity.email ?? null,
    locked: identity.method === "ncryptsec",
    createdAt: identity.createdAt,
  };
}

function loadBootstrapSet(): Set<string> {
  try {
    const raw = localStorage.getItem(BOOTSTRAP_KEY);
    if (!raw) {
      return new Set();
    }
    const parsed = JSON.parse(raw);
    if (!Array.isArray(parsed)) {
      return new Set();
    }
    return new Set(parsed.filter((item) => typeof item === "string"));
  } catch {
    return new Set();
  }
}

function needsBootstrap(pubkey: string): boolean {
  return !loadBootstrapSet().has(normalizeHexPubkey(pubkey));
}

function markBootstrapDone(pubkey: string): void {
  const set = loadBootstrapSet();
  set.add(normalizeHexPubkey(pubkey));
  localStorage.setItem(BOOTSTRAP_KEY, JSON.stringify([...set]));
}

function sendUser(
  app: ElmApp,
  pubkey: string,
  method: AuthMethod,
  extras?: { bootstrap?: boolean; displayName?: string | null; offerPasskey?: boolean },
): void {
  app.ports.receiveMessage.send({
    messageType: "user",
    value: {
      pubKey: pubkey,
      method: methodToLoginMethod(method),
      bootstrap: extras?.bootstrap === true,
      displayName: extras?.displayName ?? null,
      offerPasskey: extras?.offerPasskey === true,
    },
  });
}

async function offerPasskeyIfNeeded(
  method: AuthMethod,
  pubkey: string,
): Promise<boolean> {
  if (method !== "ncryptsec") {
    return false;
  }
  if (!shouldOfferPasskeyCreate(pubkey)) {
    return false;
  }
  try {
    const support = await reportPasskeySupport();
    return support.supported;
  } catch {
    return false;
  }
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
    case "passkey":
      return "passkey";
  }
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
    case "passkey": {
      const { nsecBytes, pubkey } = await loginWithPasskey(identity.pubkey);
      try {
        if (pubkey !== identity.pubkey) {
          throw new Error("Passkey account does not match this identity");
        }
        ndk.signer = new NDKPrivateKeySigner(nsecBytes, ndk);
      } finally {
        nsecBytes.fill(0);
      }
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
  if (identity.method === "passkey") {
    // Don't prompt biometrics on every page load; user can Use the identity.
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
        sendUser(app, identity.pubkey, "ncryptsec", {
          offerPasskey: await offerPasskeyIfNeeded("ncryptsec", identity.pubkey),
        });
        return true;
      }

      case "generateEncryptedKey": {
        const password = String(value.password || "");
        if (password.length < 8) {
          throw new Error("Password must be at least 8 characters");
        }
        const signer = NDKPrivateKeySigner.generate();
        const user = await signer.user();
        const ncryptsec = signer.encryptToNcryptsec(password);
        app.ports.receiveMessage.send({
          messageType: "encryptedKeyGenerated",
          value: {
            publicKey: normalizeHexPubkey(user.pubkey),
            ncryptsec,
          },
        });
        return true;
      }

      case "unlockEmailAccount": {
        const email = String(value.email || "").trim().toLowerCase();
        const password = String(value.password || "");
        const ncryptsec = String(value.ncryptsec || "").trim();
        const publicKeyHint = String(value.publicKeyHint || "").trim();
        const displayNameHint =
          typeof value.displayName === "string" && value.displayName.trim()
            ? value.displayName.trim()
            : null;
        if (!email || !email.includes("@")) {
          throw new Error("A valid email is required");
        }
        if (!password) {
          throw new Error("Password is required");
        }
        if (!ncryptsec.startsWith("ncryptsec")) {
          throw new Error("Expected an ncryptsec string");
        }

        let signer: NDKPrivateKeySigner;
        try {
          signer = NDKPrivateKeySigner.fromNcryptsec(ncryptsec, password, ndk);
        } catch {
          throw new Error("Wrong password");
        }
        const user = await signer.user();
        const pubkey = normalizeHexPubkey(user.pubkey);
        if (publicKeyHint && publicKeyHint !== pubkey) {
          throw new Error("Decrypted key does not match account");
        }
        const displayName = displayNameHint || email.split("@")[0];
        const identity: StoredIdentity = {
          id: newId(),
          method: "ncryptsec",
          pubkey,
          label: displayName,
          email,
          ncryptsec,
          createdAt: Date.now(),
        };
        const store = upsertIdentity(loadStore(), identity);
        saveStore(store);
        ndk.signer = signer;
        sendIdentities(app, store);
        sendUser(app, pubkey, "ncryptsec", {
          bootstrap: needsBootstrap(pubkey),
          displayName,
          offerPasskey: await offerPasskeyIfNeeded("ncryptsec", pubkey),
        });
        return true;
      }

      case "checkPasskeySupport": {
        const support = await reportPasskeySupport();
        app.ports.receiveMessage.send({
          messageType: "passkeySupport",
          value: support,
        });
        return true;
      }

      case "loginWithPasskey": {
        const preferred = String(value?.pubkey || "").trim();
        const { nsecBytes, pubkey } = await loginWithPasskey(preferred || undefined);
        try {
          const signer = new NDKPrivateKeySigner(nsecBytes, ndk);
          const identity: StoredIdentity = {
            id: newId(),
            method: "passkey",
            pubkey,
            label: value?.label || "Passkey",
            createdAt: Date.now(),
          };
          const store = upsertIdentity(loadStore(), identity);
          saveStore(store);
          ndk.signer = signer;
          sendIdentities(app, store);
          sendUser(app, pubkey, "passkey");
        } finally {
          nsecBytes.fill(0);
        }
        return true;
      }

      case "createPasskey": {
        const signer = ndk.signer;
        if (!(signer instanceof NDKPrivateKeySigner) || !signer.privateKey) {
          throw new Error("Unlock this account first to create a passkey");
        }
        const user = await signer.user();
        const pubkey = normalizeHexPubkey(user.pubkey);
        const nsecBytes = hexToNsecBytes(signer.privateKey);
        try {
          await createPasskeyForNsec(nsecBytes, pubkey, value?.displayName || null);
        } finally {
          nsecBytes.fill(0);
        }
        app.ports.receiveMessage.send({
          messageType: "passkeyCreated",
          value: { pubkey },
        });
        return true;
      }

      case "dismissPasskeyPrompt": {
        const pubkey = String(value?.pubKey || value?.pubkey || "").trim();
        if (pubkey) {
          dismissPasskeyPrompt(pubkey);
        }
        return true;
      }

      case "markBootstrapDone": {
        const pubkey = String(value.pubKey || value.pubkey || "").trim();
        if (pubkey) {
          markBootstrapDone(pubkey);
        }
        return true;
      }

      default:
        return false;
    }
  } catch (error: any) {
    const reason =
      command === "loginWithPasskey" || command === "createPasskey" || command === "checkPasskeySupport"
        ? mapKeytrError(error)
        : error?.message || String(error);
    sendAuthError(app, reason);
    return true;
  }
}
