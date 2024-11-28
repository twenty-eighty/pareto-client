
import * as bip39 from "@scure/bip39";
import { sha256 } from "@noble/hashes/sha256";
import { wordlist } from "@scure/bip39/wordlists/english";

import { NDKPrivateKeySigner } from "@nostr-dev-kit/ndk";
import NDK from "@nostr-dev-kit/ndk";

const CHALLENGE = "nostrXpasskeys";

class NostrPasskeyModule {
    constructor() {
        this.sk = null; // Private key in hex format
        this.pk = null; // Public key in hex format
        this.ndk = null; // NDK instance
        this.signer = null; // NDK signer
    }

    // Check if Passkeys (WebAuthn) are supported
    async isPasskeySupported() {
        const { supportsWebAuthn } = await import('@lo-fi/webauthn-local-client');
        return supportsWebAuthn();
    }

    // Check if there is an existing passkey
    async hasPasskey() {
        try {
            const { authDefaults, auth } = await import('@lo-fi/webauthn-local-client');
            let authOptions = authDefaults({
                challenge: sha256(CHALLENGE),
                allowCredentials: [], // Empty to allow any credential
            });
            await auth(authOptions);
            return true;
        } catch (error) {
            return false;
        }
    }

    // Create a new passkey
    async createPasskey() {
        const { regDefaults, register } = await import('@lo-fi/webauthn-local-client');
        const regOptions = regDefaults({
            relyingPartyName: "Nostr ❤️ Passkeys",
            user: {
                name: "Nostr account",
                id: new Uint8Array(16), // Random user ID
            },
        });
        await register(regOptions);
    }

    // Login with an existing passkey and create window.nostr
    async loginWithPasskey() {
        const { authDefaults, auth } = await import('@lo-fi/webauthn-local-client');
        const authOptions = authDefaults({
            challenge: sha256(CHALLENGE),
        });
        const authResult = await auth(authOptions);

        // Derive mnemonic and keys
        const mnemonic = bip39.entropyToMnemonic(
            sha256(authResult.response.signature),
            wordlist
        );

        // Generate the private key from the mnemonic
        const seed = await bip39.mnemonicToSeed(mnemonic);
        const skBytes = seed.slice(0, 32);
        const skHex = Buffer.from(skBytes).toString("hex");

        // Initialize NDK with the private key signer
        this.signer = new NDKPrivateKeySigner(skHex);
        this.ndk = new NDK({ signer: this.signer });

        // Get the public key
        this.pk = await this.signer.getPublicKey();

        // Create window.nostr object as per NIP-07
        window.nostr = {
            getPublicKey: this.getPublicKey.bind(this),
            signEvent: this.signEvent.bind(this),
            getRelays: this.getRelays.bind(this),
            nip04: {
                encrypt: this.encryptMessage.bind(this),
                decrypt: this.decryptMessage.bind(this),
            },
        };
    }

    // Get the public key (hex format)
    async getPublicKey() {
        if (!this.pk) {
            throw new Error("Public key not available. Please log in first.");
        }
        return this.pk;
    }

    // Get the private key (hex format) - Use with caution
    async getPrivateKey() {
        if (!this.signer || !this.sk) {
            throw new Error("Private key not available. Please log in first.");
        }
        // WARNING: Exposing the private key can lead to security risks.
        // Ensure that this function is used securely and with user's consent.
        return this.sk;
    }

    // Sign a Nostr event
    async signEvent(event) {
        if (!this.signer) {
            throw new Error("Signer not available. Please log in first.");
        }

        // Use NDK to sign the event
        const ndkEvent = this.ndk.createEvent(event);
        await ndkEvent.sign();

        // Return the signed event object
        return ndkEvent.serialize();
    }

    // Get relay information (implement if needed)
    async getRelays() {
        // Return an object with relay URLs as keys and policy objects as values
        return {};
    }

    // Encrypt a message using NIP-04
    async encryptMessage(pubkey, plaintext) {
        if (!this.signer) {
            throw new Error("Signer not available. Please log in first.");
        }
        // Use NDK's encryption function
        const encryptedText = await this.signer.encrypt(pubkey, plaintext);
        return encryptedText;
    }

    // Decrypt a message using NIP-04
    async decryptMessage(pubkey, ciphertext) {
        if (!this.signer) {
            throw new Error("Signer not available. Please log in first.");
        }
        // Use NDK's decryption function
        const decryptedText = await this.signer.decrypt(pubkey, ciphertext);
        return decryptedText;
    }

    // Delete an existing passkey
    async deletePasskey() {
        if (
            !navigator.credentials ||
            !navigator.credentials.preventSilentAccess
        ) {
            throw new Error(
                "Credential Management API is not supported in this browser."
            );
        }

        try {
            // Prevent silent access to prompt the user
            await navigator.credentials.preventSilentAccess();

            // Inform the user to manually remove the credential if necessary
            alert(
                "Passkey deletion is not fully supported programmatically. Please remove the passkey manually via your browser or operating system settings."
            );
        } catch (error) {
            console.error("Error deleting passkey:", error);
            throw new Error("Failed to delete passkey.");
        }
    }
}

export default NostrPasskeyModule;
