// Import the package
import NDK from "@nostr-dev-kit/ndk";

// Create a new NDK instance with explicit relays
const ndk = new NDK({
    explicitRelayUrls: ["wss://nostr.synalysis.com", "wss://synalysis.nostr1.com"],
});
