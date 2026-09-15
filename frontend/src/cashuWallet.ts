/**
 * Cashu / NIP-60 / NIP-61 helpers used from interop ports.
 * Crypto stays in TypeScript; Elm owns events and UI.
 */
import { Wallet, sumProofs } from "@cashu/cashu-ts";
import { bytesToHex } from "@noble/hashes/utils.js";
import { generateSecretKey, getPublicKey } from "nostr-tools/pure";

export const DEFAULT_MINT_URL = "https://stablenut.umint.cash";

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
