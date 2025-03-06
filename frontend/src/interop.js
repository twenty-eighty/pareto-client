
import "./Milkdown/MilkdownEditor.js";

import NDK, { NDKEvent, NDKKind, NDKRelaySet, NDKNip07Signer, NDKPrivateKeySigner, NDKSubscriptionCacheUsage, NDKRelayAuthPolicies } from "@nostr-dev-kit/ndk";
import NDKCacheAdapterDexie from "@nostr-dev-kit/ndk-cache-dexie";
import { BlossomClient } from "blossom-client-sdk/client";
import { init as initNostrLogin, launch as launchNostrLoginDialog } from "nostr-login"
import "./clipboard-component.js";
import "./zap-component.js";
import "./elm-oembed.js";
import debug from 'debug';


// import NostrPasskeyModule from './nostrPasskeyModule.js';
// const nostrPasskey = new NostrPasskeyModule();

// This is called BEFORE your Elm app starts up
// 
// The value returned here will be passed as flags 
// into your `Shared.init` function.
export const flags = ({ env }) => {
  return {
    environment: env.ELM_ENV,
    darkMode: (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches),
    isLoggedIn: JSON.parse(localStorage.getItem('isLoggedIn')) || false,
    locale: navigator.language,
    nativeSharingAvailable: (navigator.share != undefined)
  }
}

const debugLog = debug('pareto-client');

var connected = false;
var windowLoaded = false;

const anonymousSigner = new NDKPrivateKeySigner('cff56394373edfaa281d2e1b5ad1b8cafd8b247f229f2af2c61734fb0c7b3f84');
const anonymousPubKey = 'ecdf32491ef8b5f1902109f495e7ca189c6fcec76cd66b888fa9fc2ce87f40db';

export const onReady = ({ app, env }) => {

  var requestUserWhenLoaded = false;
  var storedCommands = [];

  if (window.matchMedia) {
    window.matchMedia("(prefers-color-scheme: dark)").addListener(e =>
      e.matches &&
      app.ports.receiveMessage.send({ messageType: 'darkMode', value: true })
    );
    window.matchMedia("(prefers-color-scheme: light)").addListener(e =>
      e.matches &&
      app.ports.receiveMessage.send({ messageType: 'darkMode', value: false })
    );
  }

  app.ports.sendCommand.subscribe(({ command: command, value: value }) => {
    if (command === 'connect') {
      connect(app, value.client, value.nip89, value.relays);
    } else if (command === 'loginSignUp') {
      loginSignUp(app);
    } else if (connected) {
      processOnlineCommand(app, command, value);
    } else {
      storedCommands.push({ command: command, value: value });
      debugLog('store command', command);
    }
  });

  window.onload = function () {
    const nostrLoginOptions = {
    };
    initNostrLogin(nostrLoginOptions);

    if (requestUserWhenLoaded) {
      requestUser(app);
    }
    windowLoaded = true;
  };

  // listen to events of nostr-login
  document.addEventListener('nlAuth', (event) => {
    switch (event.detail.type) {
      case 'login':
      case 'signup':
        requestUser(app);
        localStorage.setItem('isLoggedIn', JSON.stringify(true));
        break;

      default:
        app.ports.receiveMessage.send({ messageType: 'loggedOut', value: null });
        localStorage.removeItem('isLoggedIn');
        break;
    }
  });

  function processOnlineCommand(app, command, value) {
    debugLog('process command', command);
    switch (command) {
      case 'encryptString':
        encryptString(app, value.data)
        break;

      case 'downloadAndDecryptFile':
        downloadAndDecryptFile(app, value.url, value.key, value.iv)
        break;

      case 'requestEvents':
        requestEvents(app, value);
        break;

      case 'searchEvents':
        searchEvents(app, value);
        break;

      case 'requestBlossomAuth':
        requestBlossomAuth(app, value);
        break;

      case 'requestNip96Auth':
        requestNip96Auth(app, value);
        break;

      case 'sendEvent':
        sendEvent(app, value);
        break;

      case 'loginWithExtension':
        loginWithExtension(app);
        break;

      case 'requestUser':
        if (!windowLoaded) {
          // delay loading of user until window is loaded
          // and browser extension with window.nostr is ready
          requestUserWhenLoaded = true;
        } else {
          requestUser(app);
        }
        break;
    }
  }

  // 1) A function that imports an AES-GCM key and encrypts `plaintextBytes` with it.
  function encryptData(plaintextBytes, keyBytes) {
    // Import the raw key (32 bytes => AES-256)
    return window.crypto.subtle
      .importKey(
        'raw',
        keyBytes,
        { name: 'AES-GCM' },
        false,
        ['encrypt']
      )
      .then(function (key) {
        // Create a random 12-byte IV
        var ivBytes = window.crypto.getRandomValues(new Uint8Array(12));

        // Encrypt with AES-GCM
        return window.crypto.subtle
          .encrypt({ name: 'AES-GCM', iv: ivBytes }, key, plaintextBytes)
          .then(function (ciphertextBuffer) {
            return {
              ciphertextBuffer: ciphertextBuffer,
              ivBytes: ivBytes
            };
          });
      });
  }

  // 2) Helper to convert bytes to hex string
  function bytesToHex(uint8Arr) {
    return Array.from(uint8Arr)
      .map(function (b) {
        return b.toString(16).padStart(2, '0');
      })
      .join('');
  }

  // 3) Helper to convert bytes to Base64
  function bytesToBase64(uint8Arr) {
    var binary = '';
    for (var i = 0; i < uint8Arr.byteLength; i++) {
      binary += String.fromCharCode(uint8Arr[i]);
    }
    return btoa(binary);
  }

  // 4) This function encrypts the data and sends it through an Elm port as:
  //    { file: { name, size, mime, base64 }, ivHex, keyHex }
  async function encryptString(app, data) {
    var keyBytes = new Uint8Array(32); // 32 bytes for AES-256

    window.crypto.getRandomValues(keyBytes); // fill with cryptographically strong random bytes
    encryptData(new TextEncoder().encode(data), keyBytes)
      .then(function (result) {
        var ciphertextBytes = new Uint8Array(result.ciphertextBuffer);
        var ivBytes = result.ivBytes;

        crypto.subtle.digest('SHA-256', result.ciphertextBuffer).then((hash) => {
          const hashArray = Array.from(new Uint8Array(hash));
          const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');

          // Prepare 'file' data in a shape compatible with elm/file's decoder
          var file = new File([result.ciphertextBuffer], 'data.bin', { type: 'application/octet-stream' });

          // Example: send result to Elm port 
          app.ports.receiveMessage.send({
            messageType: 'encryptedString', value: {
              file: file,
              ivHex: bytesToHex(ivBytes),
              keyHex: bytesToHex(keyBytes),
              sha256: hashHex,
              size: result.ciphertextBuffer.byteLength
            }
          });
        });
      })
      .catch(function (error) {
        console.error('Encryption error:', error);
        app.ports.receiveMessage.send({ messageType: 'error', value: { reason: "failed to encrypt data" } });
      });
  }


  function downloadAndDecryptFile(app, url, keyHex, ivHex) {
    // 1) Fetch the data (as ArrayBuffer)
    return fetch(url)
      .then(function (response) {
        if (!response.ok) {
          throw new Error("Network response was not ok, status=" + response.status);
        }
        return response.arrayBuffer();
      })
      .then(function (encryptedBuffer) {
        // 2) Convert key and IV from hex to bytes
        var keyBytes = hexToBytes(keyHex); // e.g. 32 bytes for AES-256
        var ivBytes = hexToBytes(ivHex);   // typically 12 bytes for GCM

        // 3) Import key for AES-GCM
        return window.crypto.subtle
          .importKey(
            "raw",
            keyBytes,
            { name: "AES-GCM" },
            false,
            ["decrypt"]
          )
          .then(function (cryptoKey) {
            // 4) Decrypt using the imported key
            return window.crypto.subtle.decrypt(
              {
                name: "AES-GCM",
                iv: ivBytes
              },
              cryptoKey,
              encryptedBuffer
            );
          });
      })
      .then(function (decryptedBuffer) {
        // 5) Decode from bytes to UTF-8 string
        var decodedString = new TextDecoder().decode(decryptedBuffer);

        // 6) Send it to Elm via a port (assuming `app.ports.gotDecryptedString` exists)
        app.ports.receiveMessage.send({ messageType: 'decryptedString', value: JSON.parse(decodedString) });
      })
      .catch(function (error) {
        console.error("Error in fetchAndDecryptEncryptedData:", error);
        // You could also send an error message to Elm or handle it differently
        app.ports.receiveMessage.send({ messageType: 'error', value: { reason: "failed to download/decrypt data" } });
      });
  }

  // Convert a hex string (e.g. "deadbeef") to Uint8Array
  function hexToBytes(hexString) {
    if (hexString.length % 2 !== 0) {
      throw new Error("Invalid hex string length");
    }
    var result = new Uint8Array(hexString.length / 2);
    for (var i = 0; i < hexString.length; i += 2) {
      result[i / 2] = parseInt(hexString.substring(i, i + 2), 16);
    }
    return result;
  }

  function connect(app, client, nip89, relays) {
    debugLog('connect to relays', relays);
    const dexieAdapter = new NDKCacheAdapterDexie({ dbName: 'pareto-ndk-cache' });
    window.ndk = new NDK({
      enableOutboxModel: true,
      cacheAdapter: dexieAdapter,
      explicitRelayUrls: relays,
      clientName: client,
      clientNip89: nip89,
      debug: debugLog
    });

    // sign in if a relay requests authorization
    window.ndk.relayAuthDefaultPolicy = NDKRelayAuthPolicies.signIn({ ndk });

    // don't validate each event, it's computational intense
    window.ndk.initialValidationRatio = 0.5;
    window.ndk.lowestValidationRatio = 0.01;

    window.ndk.on("event:invalid-sig", (event) => {
      const { relay } = event;
      debugLog('relay delivered event with invalid signature', relay);
      app.ports.receiveMessage.send({ messageType: 'event:invalid-sig', value: { relay: relay } });
    })
    window.ndk.pool.on("connecting", (relay) => {
      debugLog('connecting relays', relay);
      app.ports.receiveMessage.send({ messageType: 'connecting', value: null });
    })
    window.ndk.pool.on("connect", () => {
      debugLog('relays connected');

      // start sending when at least one relay is ready
      connected = true;

      app.ports.receiveMessage.send({ messageType: 'connected', value: null });

      for (let i = 0; i < storedCommands.length; i++) {
        processOnlineCommand(app, storedCommands[i].command, storedCommands[i].value);
      }

      storedCommands = [];
    })
    window.ndk.pool.on("notice", (relay, notice) => {
      debugLog('relay notice', relay, notice);
      app.ports.receiveMessage.send({ messageType: 'relay:notice', value: { relay: relay, notice: notice } });
    })
    window.ndk.pool.on("relay:connect", (relay) => {
      debugLog('relay connected', relay);
      app.ports.receiveMessage.send({ messageType: 'relay:connected', value: { url: relay.url } });
    })
    window.ndk.pool.on("relay:ready", (relay) => {
      debugLog('relay ready', relay);
      app.ports.receiveMessage.send({ messageType: 'relay:ready', value: { url: relay.url } });
    })
    window.ndk.pool.on("relay:disconnect", (relay) => {
      debugLog('relay disconnected', relay);
      app.ports.receiveMessage.send({ messageType: 'relay:disconnected', value: { url: relay.url } });
    })

    window.ndk.connect(2000);
  }


  function requestEvents(app,
    { requestId: requestId
      , filter: filter
      , closeOnEose: closeOnEose
      , description: description
      , relays: relays
    }
  ) {
    debugLog("FILTER: ", filter, description, " requestId: " + requestId, "closeOnEose: " + closeOnEose, "relays: ", relays);

    const ndkRelays = relays ? NDKRelaySet.fromRelayUrls(relays, window.ndk) : null;

    window.ndk.fetchEvents(filter, { closeOnEose: closeOnEose }, ndkRelays).then((ndkEvents) => {

      processEvents(app, requestId, description, ndkEvents);
    })
  }

  function searchEvents(app,
    { requestId: requestId
      , filters: filters
      , closeOnEose: closeOnEose
      , description: description
      , relays: relays
    }
  ) {
    debugLog("SEARCH FILTERS: ", filters, description, " requestId: " + requestId, "closeOnEose: " + closeOnEose, "relays: ", relays);

    const ndkRelays = relays ? NDKRelaySet.fromRelayUrls(relays, window.ndk) : null;

    window.ndk.fetchEvents(filters, { closeOnEose: closeOnEose }, ndkRelays).then((ndkEvents) => {

      processEvents(app, requestId, description, ndkEvents);
    })
  }

  function processEvents(app, requestId, description, ndkEvents) {

    if (ndkEvents.size == 0) {
      // report back to the application that there are no events
      app.ports.receiveMessage.send({ messageType: 'events', value: { kind: 0, events: [], requestId: requestId } });
      return;
    }

    var eventsSortedByKind = {};
    var highlights = [];
    var zapReceipts = [];

    ndkEvents.forEach(ndkEvent => {
      switch (ndkEvent.kind) {
        case 0: // profile
        case 1: // short text note
        case 3: // follow list
        case 5: // event deletion request
        case 6: // repost
        case 7: // reactions
          {
            eventsSortedByKind = addEvent(eventsSortedByKind, ndkEvent);
            break;
          }

        case 9735: // zap receipt
          // example:
          // ["EVENT","+#a-kinds-sewzz",
          //   {"content":"",
          //    "created_at":1708155853,
          //    "id":"93a65bdce4e4d0ba1f5042fde8e8311781cb5eee13b30e19e4b701a1c0ca6b57",
          //    "kind":9735,
          //    "pubkey":"79f00d3f5a19ec806189fcab03c1be4ff81d18ee4f653c88fac41fe03570f432",
          //    "sig":"eb91b24d8df6e3c6a87d47783eed6bda1232607a43839713fe51d235768bcba0c5ea49ba47f526b00dc4dda42e7ad3caffecbccf773e3120dc45dd1555ecdce9",
          //    "tags":[
          //      ["p","ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600"],
          //      ["a","30023:ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600:1707912490439","wss://nostr.wine"],
          //      ["P","6b0a60cff3eca5a2b2505ccb3f7133d8422045cbef40f3d2c6189fb0b952e7d4"],
          //      ["bolt11","lnbc210n1pjaqca7pp59mrnpu6chr5j3q763wqjqprd5kdqrxqt5x8elr49y7k52gd55lushp5855evqmzhzmv9geeu2pgqc46wdhhnmhg4a6yv77mcau4y08085gscqzzsxqyz5vqsp5zktsuxysy7ffnye2caajk07g8lpwk8geg9f00h5g6ve30s98dfms9qyyssqv86ejdsap4gu3x3ej9mjy4qtuyxws8pxuxh30fr9q9nh6xkf37fx3q858lxkwpyge5udqf35z34gm6ut3w86xh7yafa7aqrguy770wsqy5dke6"],
          //      ["preimage","9887b5e519332ae71991fd5d0d315e6ab24d92979a54ac29c0fd4d74394ecc16"],
          //      ["pubkey","79f00d3f5a19ec806189fcab03c1be4ff81d18ee4f653c88fac41fe03570f432"],
          //      ["description","{\"created_at\":1708155834,\"content\":\"\",\"tags\":[[\"p\",\"ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600\"],[\"a\",\"30023:ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600:1707912490439\",\"wss://nostr.wine\"],[\"amount\",\"21000\"],[\"relays\",\"wss://relay.snort.social\",\"wss://relay.damus.io\",\"wss://nos.lol\",\"wss://nostr.wine\",\"wss://offchain.pub\"]],\"kind\":9734,\"pubkey\":\"6b0a60cff3eca5a2b2505ccb3f7133d8422045cbef40f3d2c6189fb0b952e7d4\",\"id\":\"0519059ed961675cc8709d9f28cb8b842dbc7e54fb5ec00a7f697a722c5bc794\",\"sig\":\"62fcc1f5cfe74bd28d47eb2e50de58444505d2ab58eb065f9b0eaf4dc5457f8d7537a8eb4e34ef36bf676e5aa2a05958f42b67cdccc67581d9b0dff07611430a\"}"]
          //    ]
          //  }]
          {
            const zapReceipt = fillZapReceipt(ndkEvent);
            zapReceipts.push(zapReceipt);
            break;
          }

        case 9802: // highlight
          {
            const highlight = ndkEvent.content;
            const pubkeyHighlight = { pubkey: ndkEvent.pubkey, highlight: highlight };
            highlights.push(pubkeyHighlight);
            break;
          }

        case 10002: // relay list metadata
        case 10003: // bookmark list
        case 10004: // community lists
        case 10050: // relay list for DMs
        case 10063: // relay list for file uploads (Blossom)
        case 10096: // relay list for file uploads (NIP-96)
        case 30000: // follow sets
        case 30003: // bookmark sets
        case 30023: // long-form article
        case 30024: // long-form draft
        case 34550: // community definition
          {
            eventsSortedByKind = addEvent(eventsSortedByKind, ndkEvent);
            break;
          }

        case 10013: // relay list for private content
          {
            unwrapPrivateRelayListEvent(ndkEvent).then(event => {
              app.ports.receiveMessage.send({ messageType: 'events', value: { kind: event.kind, events: [event], requestId: requestId } });
            });
            break;
          }

        case 30078: // application-specific event
          {
            unwrapApplicationSpecificEvent(ndkEvent).then(event => {
              if (event) {
                app.ports.receiveMessage.send({ messageType: 'events', value: { kind: event.kind, events: [event], requestId: requestId } });
              }
            });
            break;
          }

        case 31234: // draft event (NIP-37)
          {
            unwrapDraftEvent(ndkEvent).then(event => {
              if (event) {
                app.ports.receiveMessage.send({ messageType: 'events', value: { kind: event.kind, events: [event], requestId: requestId } });
              }
            });
            break;
          }

        default:
          debugLog("Unhandled event with kind: ", ndkEvent.kind);
          eventsSortedByKind = addEvent(eventsSortedByKind, ndkEvent);
          break;
      }
    });

    for (const kind in eventsSortedByKind) {
      const events = eventsSortedByKind[kind]
      debugLog("Events of kind " + kind + ": ", events.length);
      app.ports.receiveMessage.send({ messageType: 'events', value: { kind: parseInt(kind), events: events, requestId: requestId } });
    }

    if (highlights.length > 0) {
      debugLog("Highlights: ", highlights.length);
      app.ports.receiveMessage.send({ messageType: 'highlights', value: highlights });
    }
    if (zapReceipts.length > 0) {
      debugLog("ZapReceipts: ", zapReceipts.length);
      app.ports.receiveMessage.send({ messageType: 'zap_receipts', value: zapReceipts });
    }
  }

  function decryptRelayList(ndkEvent) {
    const content = ndkEvent.content;
    const decryptedContent = nip4(encryptedContent);
    const decryptedEvent = new NDKEvent(window.ndk, ndkEvent);
    return {
      kind: ndkEvent.kind,
      pubkey: ndkEvent.pubkey,
      content: ""
    }
  }

  function requestBlossomAuth(app, { requestId: requestId, fileId: fileId, serverUrl: serverUrl, content: content, method: method, hash: sha256Hash }) {
    debugLog("Blossom auth request with requestId: " + requestId);

    async function signer(event) {
      return await window.nostr.signEvent(event);
    }

    var authPromise;

    switch (method) {
      case 'PUT':
        authPromise = BlossomClient.createUploadAuth(sha256Hash, signer, content);
        break;

      case 'GET':
        const client = new BlossomClient(serverUrl, signer);
        authPromise = client.getListAuth("file list");
        break;
    }

    // create an upload auth event
    authPromise.then(authHeader => {
      debugLog('authHeader', authHeader);

      // encode it using base64
      const encodedAuthHeader = BlossomClient.encodeAuthorizationHeader(authHeader);

      app.ports.receiveMessage.send(
        {
          messageType: 'blossomAuthHeader'
          , value:
          {
            requestId: requestId
            , fileId: fileId
            , method: method
            , authHeader: encodedAuthHeader
            , serverUrl: serverUrl
            , apiUrl: ""
          }
        });
    })
  }


  function requestNip96Auth(app, { requestId: requestId, fileId: fileId, serverUrl: serverUrl, apiUrl: apiUrl, method: method, hash: sha256Hash }) {
    debugLog("Nip96 auth request with requestId: " + requestId);

    generateNip98Header(apiUrl, method, sha256Hash).then(nip98AuthHeader => {
      debugLog('nip98 header', nip98AuthHeader);
      // encode it using base64
      app.ports.receiveMessage.send(
        {
          messageType: 'nip98AuthHeader'
          , value:
          {
            requestId: requestId
            , fileId: fileId
            , method: method
            , authHeader: nip98AuthHeader
            , serverUrl: serverUrl
            , apiUrl: apiUrl
          }
        }
      );
    });
  }

  async function sendEvent(app, { sendId: sendId, event: event, relays: relays }) {
    debugLog('send event ' + sendId, event, 'relays: ', relays);

    var ndkEvent = new NDKEvent(window.ndk, event);
    const signer = (ndkEvent.pubkey == anonymousPubKey) ? anonymousSigner : window.ndk.signer;

    if (event.kind == 30024) {  // draft event
      ndkEvent = await encapsulateDraftEvent(ndkEvent, signer);
    } else if (event.kind == 30078) {  // application-specific event
      ndkEvent = await encapsulateApplicationSpecificEvent(ndkEvent, signer);
    }

    if (!ndkEvent) {
      debugLog('failed to send event ' + sendId, event, 'relays: ', relays);
      app.ports.receiveMessage.send({ messageType: 'error', value: { sendId: sendId, event: event, relays: relays, reason: "failed to encapsulate event" } });
      return;
    }

    ndkEvent.sign(signer).then(() => {
      debugLog('signed event ' + sendId, ndkEvent);

      var relaysWithProtocol = relays.map(relay => {
        if (!relay.startsWith("wss://") && !relay.startsWith("ws://")) {
          return "wss://" + relay
        } else {
          return relay
        }
      });

      if (relaysWithProtocol.length === 0) {
        relaysWithProtocol = ["wss://pareto.nostr1.com"];
      }
      const relaySet = NDKRelaySet.fromRelayUrls(relaysWithProtocol, window.ndk);
      ndkEvent.publish(relaySet, 5000).then((results) => {
        debugLog('published event ' + sendId, ndkEvent);
        app.ports.receiveMessage.send({ messageType: 'published', value: { sendId: sendId, event: ndkEvent, results: results } });

        // feed sent events into app as if received by relay.
        // thus we can let the event modify the state correctly
        processEvents(app, -1, "sent event", [ndkEvent]);
      }).catch((error) => {
        console.log(error);
        app.ports.receiveMessage.send({ messageType: 'error', value: { sendId: sendId, event: event, relays: relays, reason: "failed to send event" } });
      });
    })
  }

  // https://nips.nostr.com/37
  async function encapsulateDraftEvent(ndkEvent) {
    await ndkEvent.sign();
    const rawEventString = JSON.stringify(ndkEvent.rawEvent());
    const identifier = firstTag(ndkEvent, "d");
    const draftEvent = {
      kind: 31234,
      tags: [
        ["d", identifier],
        ["k", ndkEvent.kind.toString()],
        ["e", ndkEvent.id],
        ["a", ndkEvent.kind + ":" + ndkEvent.pubkey + ":" + identifier],
      ],
      content: await window.ndk.signer.encrypt({ pubkey: ndkEvent.pubkey }, rawEventString, 'nip44'),
      pubkey: ndkEvent.pubkey,
      created_at: ndkEvent.created_at
    }

    var ndkEvent = new NDKEvent(window.ndk, draftEvent)
    return ndkEvent;
  }

  // https://nips.nostr.com/78
  async function encapsulateApplicationSpecificEvent(ndkEvent, signer) {
    // if target pubkey is not specified as a tag, use the pubkey of the event
    var encryptForPubKey = firstTag(ndkEvent, "p");
    if (!encryptForPubKey) {
      // encrypt for current user (supposed to be an author storing his subscribers)
      encryptForPubKey = ndkEvent.pubkey;
    }
    debugLog('encrypt for key', encryptForPubKey);
    // in order to allow anonymous users to subscribe to a newsletter we need to use a different signer
    const encrypted = await signer.encrypt({ pubkey: encryptForPubKey }, ndkEvent.content, 'nip44');
    if (encrypted) {
      ndkEvent.content = encrypted;
      return ndkEvent;
    }
    // don't send unencrypted event
    return null;
  }

  async function unwrapPrivateRelayListEvent(ndkEvent) {
    const stringifiedEvent = await window.ndk.signer.decrypt({ pubkey: ndkEvent.pubkey }, ndkEvent.content, 'nip44');
    ndkEvent.tags = JSON.parse(stringifiedEvent);
    return ndkEvent;
  }

  async function unwrapApplicationSpecificEvent(ndkEvent) {
    const content = await window.ndk.signer.decrypt({ pubkey: ndkEvent.pubkey }, ndkEvent.content, 'nip44');
    if (content) {
      ndkEvent.content = content;
    } else {
      console.log("Unable to decrypt application-specific event. Ignoring the event.")
    }
    return ndkEvent;
  }

  async function unwrapDraftEvent(ndkEvent) {
    const stringifiedEvent = await window.ndk.signer.decrypt({ pubkey: ndkEvent.pubkey }, ndkEvent.content, 'nip44');
    if (stringifiedEvent) {
      const event = JSON.parse(stringifiedEvent);
      return event;
    } else {
      console.log("Unable to decrypt draft event. Ignoring the event.")
    }
  }

  function firstTag(event, tagName) {
    if (event.tags) {
      for (const tag of event.tags) {
        if (tag[0] === tagName) {
          return tag[1];
        }
      }
    }

    return null;
  }

  function addEvent(eventsSortedByKind, ndkEvent) {
    if (eventsSortedByKind[ndkEvent.kind]) {
      eventsSortedByKind[ndkEvent.kind].push(ndkEvent);
    } else {
      eventsSortedByKind[ndkEvent.kind] = [ndkEvent];
    }

    return eventsSortedByKind;
  }


  function lastTagWithId(tags, tagName) {
    for (let i = tags.length - 1; i >= 0; i--) {
      const tag = tags[i];
      if (tag[0] === tagName) {
        return tag[1];
      }
    }
    return null;
  }

  function fillZapReceipt(ndkEvent) {
    const zapReceipt = { id: ndkEvent.id };

    ndkEvent.tags.forEach(tag => {
      switch (tag[0]) {
        case 'P':
          zapReceipt.pubkeySender = tag[1];
          break;
        case 'a':
          zapReceipt.address = tag[1];
          break;
        case 'bolt11':
          zapReceipt.bolt11 = tag[1];
          break;
        case 'e':
          zapReceipt.event = tag[1];
          break;
        case 'p':
          zapReceipt.recipient = tag[1];
          break;
        case 'preimage':
          zapReceipt.preimage = tag[1];
          break;
        case 'description':
          try {
            const json = JSON.parse(tag[1]);
            const tags = json.tags;
            const amountTag = tags.find(tag => tag[0] === 'amount');
            if (amountTag) {
              zapReceipt.amount = amountTag[1];
            }
          } catch (e) {
            console.error(e);
          }
          break;
      }
    });

    return zapReceipt;
  }


  function requestUser(app) {
    if (window.nostr) {
      const nip07signer = new NDKNip07Signer();
      window.ndk.signer = nip07signer;
      nip07signer.user().then(async (user) => {
        if (!!user.npub) {
          app.ports.receiveMessage.send({ messageType: 'user', value: { pubKey: user.pubkey } });
          localStorage.setItem('isLoggedIn', JSON.stringify(true));
        }
      }
      )
    }
  }

  function loginSignUp(app) {
    processOnlineCommand(app, "requestUser")
  }

  function loginWithExtension(app) {
    if (window.nostr) {
      // use nostr-login only if no browser extension is present
      const nostrLoginOptions = {
        "data-methods": ["extension"]
      };
      initNostrLogin(nostrLoginOptions);
    }
  }

  function urlWithoutProtocol(url) {
    const urlObj = new URL(url);
    return urlObj.hostname + urlObj.pathname;
  }

  async function generateNip98Header(requestUrl, httpMethod, sha256Hash) {
    const event = new NDKEvent(window.ndk, {
      kind: NDKKind.HttpAuth,
      tags: [
        ["u", requestUrl],
        ["method", httpMethod],
      ],
    });

    if (["POST", "PUT", "PATCH"].includes(httpMethod)) {
      event.tags.push(["payload", sha256Hash]);
    }

    await event.sign();
    const encodedEvent = btoa(JSON.stringify(event.rawEvent()));
    return `Nostr ${encodedEvent}`;
  }
}