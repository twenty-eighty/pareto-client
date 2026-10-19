import { nostrEventId, type SignedAuthEvent, type UnsignedAuthEvent } from "./protocol";

export interface ContactsSigner {
  getPublicKey(): Promise<string>;
  sign(event: UnsignedAuthEvent & { id: string }): Promise<string>;
  nip44Encrypt(pubkey: string, plaintext: string): Promise<string>;
  nip44Decrypt(pubkey: string, payload: string): Promise<string>;
}

export interface NdkLike {
  signer: {
    user(): Promise<{ pubkey: string }>;
    sign(event: UnsignedAuthEvent & { id: string }): Promise<string>;
    encrypt(recipient: { pubkey: string }, plaintext: string, scheme: "nip44"): Promise<string>;
    decrypt(sender: { pubkey: string }, payload: string, scheme: "nip44"): Promise<string>;
  };
}

export function signerFromNdk(ndk: NdkLike): ContactsSigner {
  return {
    async getPublicKey() {
      const user = await ndk.signer.user();
      return user.pubkey;
    },
    sign(event) {
      return ndk.signer.sign(event);
    },
    nip44Encrypt(pubkey, plaintext) {
      return ndk.signer.encrypt({ pubkey }, plaintext, "nip44");
    },
    nip44Decrypt(pubkey, payload) {
      return ndk.signer.decrypt({ pubkey }, payload, "nip44");
    },
  };
}

export async function signAuthEvent(
  signer: ContactsSigner,
  loginUrl: string,
  challenge: string,
): Promise<SignedAuthEvent> {
  const pubkey = await signer.getPublicKey();
  const unsigned: UnsignedAuthEvent = {
    pubkey,
    created_at: Math.floor(Date.now() / 1000),
    kind: 22242,
    tags: [
      ["server", loginUrl],
      ["challenge", challenge],
    ],
    content: "",
  };
  const id = await nostrEventId(unsigned);
  const sig = await signer.sign({ ...unsigned, id });
  return { ...unsigned, id, sig };
}
