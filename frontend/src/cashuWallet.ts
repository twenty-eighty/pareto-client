/**
 * Cashu / NIP-60 / NIP-61 helpers used from interop ports.
 * Crypto stays in TypeScript; Elm owns events and UI.
 */
import { MintQuoteState, Wallet, sumProofs } from "@cashu/cashu-ts";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";
// nostr-tools/pure named exports resolve at runtime; cast avoids TS moduleResolution quirks.
import * as nostrToolsPure from "nostr-tools/pure";

const { generateSecretKey, getPublicKey } = nostrToolsPure as unknown as {
  generateSecretKey: () => Uint8Array;
  getPublicKey: (secretKey: Uint8Array) => string;
};

export const DEFAULT_MINT_URL = "https://mint.minibits.cash/Bitcoin";

export type CashuProof = {
  id: string;
  amount: number;
  secret: string;
  C: string;
  dleq?: unknown;
};

type Nip44Signer = {
  encrypt: (recipient: { pubkey: string }, plaintext: string, scheme: string) => Promise<string>;
  decrypt: (sender: { pubkey: string }, ciphertext: string, scheme: string) => Promise<string>;
};

/** Dedicated Cashu P2PK keypair. Pubkey is SEC1-compressed with 02 prefix. */
export function createWalletKeypair(): { privkey: string; pubkey: string } {
  const sk = generateSecretKey();
  const privkey = bytesToHex(sk);
  const pubkey = "02" + getPublicKey(sk);
  return { privkey, pubkey };
}

/** Derive Cashu P2PK pubkey (02-prefixed) from an existing wallet privkey hex. */
export function p2pkPubkeyFromPrivkey(privkeyHex: string): string {
  const sk = hexToBytes(privkeyHex);
  return "02" + getPublicKey(sk);
}

export async function encryptWalletContent(
  signer: Nip44Signer,
  ownerPubkey: string,
  tags: string[][]
): Promise<string> {
  return signer.encrypt({ pubkey: ownerPubkey }, JSON.stringify(tags), "nip44");
}

export async function decryptWalletContent(
  signer: Nip44Signer,
  ownerPubkey: string,
  content: string
): Promise<string[][]> {
  const plain = await signer.decrypt({ pubkey: ownerPubkey }, content, "nip44");
  return JSON.parse(plain);
}

export async function encryptTokenPayload(
  signer: Nip44Signer,
  ownerPubkey: string,
  payload: unknown
): Promise<string> {
  return signer.encrypt({ pubkey: ownerPubkey }, JSON.stringify(payload), "nip44");
}

export async function decryptTokenPayload(
  signer: Nip44Signer,
  ownerPubkey: string,
  content: string
): Promise<unknown> {
  const plain = await signer.decrypt({ pubkey: ownerPubkey }, content, "nip44");
  return JSON.parse(plain);
}

export function proofAmount(proof: { amount: unknown }): number {
  const amount = proof.amount as any;
  if (amount == null) {
    return 0;
  }
  if (typeof amount === "number") {
    return amount;
  }
  if (typeof amount === "bigint") {
    return Number(amount);
  }
  if (typeof amount === "string") {
    return Number(amount) || 0;
  }
  if (typeof amount.toNumber === "function") {
    return amount.toNumber();
  }
  if (typeof amount.toNumberUnsafe === "function") {
    return amount.toNumberUnsafe();
  }
  return Number(amount) || 0;
}

export function serializeProofsForElm(proofs: Array<{ id: string; amount: unknown; secret: string; C: string; dleq?: unknown }>): CashuProof[] {
  return proofs.map((proof) => {
    const out: CashuProof = {
      id: proof.id,
      amount: proofAmount(proof),
      secret: proof.secret,
      C: proof.C,
    };
    if (proof.dleq != null) {
      out.dleq = proof.dleq;
    }
    return out;
  });
}

export function getBalance(proofs: Array<{ amount: unknown }>): number {
  if (!proofs || proofs.length === 0) {
    return 0;
  }
  try {
    return proofAmount({ amount: sumProofs(proofs as any) });
  } catch (_err) {
    return proofs.reduce((acc, proof) => acc + proofAmount(proof), 0);
  }
}

/**
 * Swap P2PK-locked nutzap proofs into wallet-owned proofs via mint receive.
 */
export async function redeemNutzap(params: {
  mintUrl: string;
  proofs: unknown[];
  p2pkPrivkey: string;
}): Promise<CashuProof[]> {
  const wallet = new Wallet(params.mintUrl);
  await wallet.loadMint();
  const received = await wallet.receive(params.proofs as any, {
    privkey: params.p2pkPrivkey,
  });
  return serializeProofsForElm(received);
}

/**
 * Swap wallet proofs into P2PK-locked outputs for a NIP-61 nutzap send.
 * `recipientP2pk` must be the Cashu pubkey from the recipient's kind 10019 (02-prefixed).
 */
export async function sendNutzap(params: {
  mintUrl: string;
  proofs: unknown[];
  amount: number;
  recipientP2pk: string;
}): Promise<{ keep: CashuProof[]; send: CashuProof[] }> {
  const pubkey = params.recipientP2pk.startsWith("02") || params.recipientP2pk.startsWith("03")
    ? params.recipientP2pk
    : "02" + params.recipientP2pk;

  const wallet = new Wallet(params.mintUrl);
  await wallet.loadMint();
  const { keep, send } = await wallet.ops
    .send(params.amount, params.proofs as any)
    .asP2PK({ pubkey })
    .includeFees(true)
    .run();

  return {
    keep: serializeProofsForElm(keep),
    send: serializeProofsForElm(send),
  };
}

export type CashuMintQuote = {
  quote: string;
  bolt11: string;
  amount: number;
  mintUrl: string;
};

function sleep(ms: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) {
      reject(new DOMException("Aborted", "AbortError"));
      return;
    }
    const timer = setTimeout(resolve, ms);
    signal?.addEventListener(
      "abort",
      () => {
        clearTimeout(timer);
        reject(new DOMException("Aborted", "AbortError"));
      },
      { once: true },
    );
  });
}

/**
 * Create a BOLT11 mint quote (Lightning invoice) for funding the Cashu wallet.
 */
export async function createMintQuote(params: {
  mintUrl: string;
  amount: number;
}): Promise<CashuMintQuote> {
  try {
    const wallet = new Wallet(params.mintUrl);
    await wallet.loadMint();
    const mintQuote = await wallet.createMintQuoteBolt11(params.amount);
    return {
      quote: mintQuote.quote,
      bolt11: mintQuote.request,
      amount: params.amount,
      mintUrl: params.mintUrl,
    };
  } catch (error) {
    throw new Error(formatMintError(error, params.mintUrl));
  }
}

/**
 * Poll until the mint quote is PAID, then mint proofs.
 */
export async function waitAndMintProofs(params: {
  mintUrl: string;
  amount: number;
  quote: string;
  signal?: AbortSignal;
  timeoutMs?: number;
  pollIntervalMs?: number;
}): Promise<CashuProof[]> {
  try {
    const wallet = new Wallet(params.mintUrl);
    await wallet.loadMint();

    const timeoutMs = params.timeoutMs ?? 10 * 60 * 1000;
    const pollIntervalMs = params.pollIntervalMs ?? 2500;
    const started = Date.now();

    while (true) {
      if (params.signal?.aborted) {
        throw new DOMException("Aborted", "AbortError");
      }

      const checked = await wallet.checkMintQuoteBolt11(params.quote);
      if (checked.state === MintQuoteState.PAID) {
        const proofs = await wallet.mintProofsBolt11(params.amount, params.quote);
        return serializeProofsForElm(proofs);
      }

      if (Date.now() - started > timeoutMs) {
        throw new Error("Timed out waiting for Lightning payment");
      }

      await sleep(pollIntervalMs, params.signal);
    }
  } catch (error) {
    if ((error as { name?: string })?.name === "AbortError") {
      throw error;
    }
    throw new Error(formatMintError(error, params.mintUrl));
  }
}

/** Flatten nested fetch/NetworkError messages into something useful for the UI. */
export function formatMintError(error: unknown, mintUrl: string): string {
  const parts: string[] = [];
  let current: unknown = error;
  let depth = 0;
  while (current && depth < 5) {
    if (current instanceof Error) {
      if (current.message) {
        parts.push(current.message);
      }
      current = (current as Error & { cause?: unknown }).cause;
    } else if (typeof current === "string") {
      parts.push(current);
      break;
    } else {
      break;
    }
    depth += 1;
  }

  const combined = parts.join(" — ");
  const lower = combined.toLowerCase();
  const unreachable =
    lower.includes("failed to fetch") ||
    lower.includes("networkerror") ||
    lower.includes("load failed") ||
    lower.includes("network request failed");

  if (unreachable) {
    return `Cannot reach mint ${mintUrl} (DNS/network error). Check the mint URL or your connection.`;
  }

  if (combined) {
    return `${combined} (${mintUrl})`;
  }

  return `Mint request failed (${mintUrl})`;
}

export type CashuMeltQuotePreview = {
  mintUrl: string;
  invoice: string;
  amount: number;
  feeReserve: number;
  total: number;
};

/**
 * Preview a BOLT11 melt quote (invoice amount + fee reserve).
 */
export async function createMeltQuote(params: {
  mintUrl: string;
  invoice: string;
}): Promise<CashuMeltQuotePreview> {
  try {
    const wallet = new Wallet(params.mintUrl);
    await wallet.loadMint();
    const meltQuote = await wallet.createMeltQuoteBolt11(params.invoice);
    const amount = proofAmount({ amount: meltQuote.amount });
    const feeReserve = proofAmount({ amount: meltQuote.fee_reserve });
    return {
      mintUrl: params.mintUrl,
      invoice: params.invoice,
      amount,
      feeReserve,
      total: amount + feeReserve,
    };
  } catch (error) {
    throw new Error(formatMintError(error, params.mintUrl));
  }
}

/**
 * Pay a Lightning invoice by melting Cashu proofs at the mint.
 * Returns remaining proofs (keep + change) to store in the wallet.
 */
export async function meltToLightning(params: {
  mintUrl: string;
  invoice: string;
  proofs: unknown[];
}): Promise<{
  keep: CashuProof[];
  amount: number;
  feeReserve: number;
  total: number;
}> {
  try {
    const wallet = new Wallet(params.mintUrl);
    await wallet.loadMint();
    const meltQuote = await wallet.createMeltQuoteBolt11(params.invoice);
    const amount = proofAmount({ amount: meltQuote.amount });
    const feeReserve = proofAmount({ amount: meltQuote.fee_reserve });
    const amountToSend = amount + feeReserve;

    const { keep, send } = await wallet.send(amountToSend, params.proofs as any, {
      includeFees: true,
    });
    const meltResponse = await wallet.meltProofsBolt11(meltQuote, send);
    const change = meltResponse?.change ?? [];
    const remaining = [...keep, ...change];

    return {
      keep: serializeProofsForElm(remaining),
      amount,
      feeReserve,
      total: amountToSend,
    };
  } catch (error) {
    throw new Error(formatMintError(error, params.mintUrl));
  }
}
