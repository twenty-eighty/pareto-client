
import "./MilkdownEditor.js";

import NDK, { NDKEvent, NDKKind, NDKRelaySet, NDKNip07Signer, NDKSubscriptionCacheUsage, NDKRelayAuthPolicies } from "@nostr-dev-kit/ndk";
import { BlossomClient } from "blossom-client-sdk/client";
import { init as initNostrLogin, launch as launchNostrLoginDialog } from "nostr-login"

// import NostrPasskeyModule from './nostrPasskeyModule.js';
// const nostrPasskey = new NostrPasskeyModule();

const nostrLoginOptions = {
};
initNostrLogin(nostrLoginOptions);

const debug = true;

// This is called BEFORE your Elm app starts up
// 
// The value returned here will be passed as flags 
// into your `Shared.init` function.
export const flags = ({ env }) => {
  return {
    darkMode: (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches),
    isLoggedIn: JSON.parse(localStorage.getItem('isLoggedIn')) || false,
    locale: navigator.language,
    sharingAvailable: (navigator.share != undefined)
  }
}

var connected = false;
var windowLoaded = false;

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
      connect(app, storedCommands, value);
    } else if (command === 'loginSignUp') {
      loginSignUp(app);
    } else if (connected) {
      processOnlineCommand(app, command, value);
    } else {
      storedCommands.push({ command: command, value: value });
      if (debug) {
        console.log('store command', command);
      }
    }
  });

  window.onload = function () {
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
    if (debug) {
      console.log('process command', command);
    }
    switch (command) {
      case 'requestEvents':
        requestEvents(app, value);
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


  function connect(app, storedCommands, relays) {
    if (debug) {
      console.log('connect to relays', relays);
    }
    window.ndk = new NDK({ explicitRelayUrls: relays });

    // sign in if a relay requests authorization
    window.ndk.relayAuthDefaultPolicy = NDKRelayAuthPolicies.signIn({ ndk });

    // don't validate each event, it's computational intense
    window.ndk.initialValidationRatio = 0.5;
    window.ndk.lowestValidationRatio = 0.01;

    window.ndk.on("event:invalid-sig", (event) => {
      const { relay } = event;
      if (debug) {
        console.log('relay delivered event with invalid signature', relay);
      }
      app.ports.receiveMessage.send({ messageType: 'event:invalid-sig', value: { relay: relay } });
    })
    window.ndk.pool.on("connecting", (relay) => {
      if (debug) {
        console.log('connecting relays', relay);
      }
      app.ports.receiveMessage.send({ messageType: 'connecting', value: null });
    })
    window.ndk.pool.on("connect", () => {
      if (debug) {
        console.log('relays connected');
      }
      connected = true;

      app.ports.receiveMessage.send({ messageType: 'connected', value: null });

      for (let i = 0; i < storedCommands.length; i++) {
        processOnlineCommand(app, storedCommands[i].command, storedCommands[i].value);
      }
    })
    window.ndk.pool.on("notice", (relay, notice) => {
      if (debug) {
        console.log('relay notice', relay, notice);
      }
      app.ports.receiveMessage.send({ messageType: 'relay:notice', value: { relay: relay, notice: notice } });
    })
    window.ndk.pool.on("relay:connect", (relay) => {
      if (debug) {
        console.log('relay connected', relay);
      }
      app.ports.receiveMessage.send({ messageType: 'relay:connected', value: urlWithoutProtocol(relay.url) });
    })
    window.ndk.pool.on("relay:ready", (relay) => {
      if (debug) {
        console.log('relay ready', relay);
      }
      app.ports.receiveMessage.send({ messageType: 'relay:ready', value: urlWithoutProtocol(relay.url) });
    })
    window.ndk.pool.on("relay:disconnect", (relay) => {
      if (debug) {
        console.log('relay disconnected', relay);
      }
      app.ports.receiveMessage.send({ messageType: 'relay:disconnected', value: urlWithoutProtocol(relay.url) });
    })

    window.ndk.connect();
  }


  function requestEvents(app,
    { requestId: requestId
      , filter: filter
      , closeOnEose: closeOnEose
      , description: description
      , relays: relays
    }
  ) {
    if (debug) {
      console.log("FILTER: ", filter, description, " requestId: " + requestId, "closeOnEose: " + closeOnEose, "relays: ", relays);
    }

    const ndkRelays = relays ? NDKRelaySet.fromRelayUrls(relays, window.ndk) : null;

    window.ndk.fetchEvents(filter, { closeOnEose: closeOnEose }, ndkRelays).then((ndkEvents) => {
      var articles = [];
      var communities = [];
      var eventsSortedByKind = {};
      var followlists = [];
      var profiles = [];
      var reactions = [];
      var reposts = [];
      var shortNotes = [];
      var highlights = [];
      var zapReceipts = [];

      ndkEvents.forEach(ndkEvent => {
        switch (ndkEvent.kind) {
          case 0: // profile
          case 1: // short text note
          case 3: // follow list
          case 6: // repost
            {
              eventsSortedByKind = addEvent(eventsSortedByKind, ndkEvent);
              break;
            }

          case 7: // reactions
            {
              const reaction =
              {
                pubkey: ndkEvent.pubkey
                , content: ndkEvent.content
                , id: ndkEvent.id
                , "noteid-reactedto": lastTagWithId(ndkEvent.tags, "e")
                , "pubkey-reactedto": lastTagWithId(ndkEvent.tags, "p")
                , "kind-reactedto": lastTagWithId(ndkEvent.tags, "k")
                , "coordinates-reactedto": lastTagWithId(ndkEvent.tags, "a")
              };
              reactions.push(reaction);
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
              if (debug) {
                // console.log("Zap receipt: ", zapReceipt);
              }
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
          case 10004: // community lists
          case 10063: // relay list for file uploads (Blossom)
          case 10096: // relay list for file uploads (NIP-96)
          case 30000: // follow sets
          case 30003: // bookmark sets
          case 30023: // long-form article
          case 34550: // community definition
            {
              eventsSortedByKind = addEvent(eventsSortedByKind, ndkEvent);
              break;
            }

          default:
            if (debug) {
              console.log("Unhandled event with kind: ", ndkEvent.kind);
            }
            eventsSortedByKind = addEvent(eventsSortedByKind, ndkEvent);
            break;
        }
      });

      if (articles.length > 0) {
        if (debug) {
          console.log("Articles: ", articles.length);
        }
        app.ports.receiveMessage.send({ messageType: 'articles', value: articles });
      }
      if (communities.length > 0) {
        if (debug) {
          console.log("Communities: ", communities.length);
        }
        app.ports.receiveMessage.send({ messageType: 'communities', value: communities });
      }
      for (const kind in eventsSortedByKind) {
        const events = eventsSortedByKind[kind]
        if (debug) {
          console.log("Events of kind " + kind + ": ", events.length);
        }
        app.ports.receiveMessage.send({ messageType: 'events', value: { kind: parseInt(kind), events: events, requestId: requestId } });
      }

      if (followlists.length > 0) {
        if (debug) {
          console.log("Follow lists: ", followlists.length);
        }
        app.ports.receiveMessage.send({ messageType: 'followlists', value: followlists });
      }
      if (profiles.length > 0) {
        if (debug) {
          console.log("Profiles: ", profiles.length);
        }
        app.ports.receiveMessage.send({ messageType: 'profiles', value: profiles });
      }
      if (reactions.length > 0) {
        if (debug) {
          console.log("Reactions: ", reactions.length);
        }
        app.ports.receiveMessage.send({ messageType: 'reactions', value: reactions });
      }
      if (reposts.length > 0) {
        if (debug) {
          console.log("Reposts: ", reposts.length);
        }
        app.ports.receiveMessage.send({ messageType: 'reposts', value: reposts });
      }
      if (shortNotes.length > 0) {
        if (debug) {
          console.log("Short notes: ", shortNotes.length);
        }
        app.ports.receiveMessage.send({ messageType: 'short_notes', value: shortNotes });
      }
      if (highlights.length > 0) {
        if (debug) {
          console.log("Highlights: ", highlights.length);
        }
        app.ports.receiveMessage.send({ messageType: 'highlights', value: highlights });
      }
      if (zapReceipts.length > 0) {
        if (debug) {
          console.log("ZapReceipts: ", zapReceipts.length);
        }
        app.ports.receiveMessage.send({ messageType: 'zap_receipts', value: zapReceipts });
      }
    })
  }

  function requestBlossomAuth(app, { requestId: requestId, fileId: fileId, serverUrl: serverUrl, content: content, method: method, hash: sha256Hash }) {
    if (debug) {
      console.log("Blossom auth request with requestId: " + requestId);
    }

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
      if (debug) {
        console.log('authHeader', authHeader);
      }

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
    if (debug) {
      console.log("Nip96 auth request with requestId: " + requestId);
    }

    generateNip98Header(apiUrl, method, sha256Hash).then(nip98AuthHeader => {
      if (debug) {
        console.log('nip98 header', nip98AuthHeader);
      }
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

  function sendEvent(app, { sendId: sendId, event: event }) {
    if (debug) {
      console.log('send event ' + sendId, event);
    }
    const ndkEvent = new NDKEvent(window.ndk, event);
    ndkEvent.sign().then(() => {
      if (debug) {
        console.log('signed event ' + sendId, ndkEvent);
      }
      const relays = ["wss://pareto.nostr1.com"];
      const relaySet = NDKRelaySet.fromRelayUrls(relays, window.ndk);
      ndkEvent.publish(relaySet, 5000).then((results) => {
        if (debug) {
          console.log('published event ' + sendId, ndkEvent);
        }
        app.ports.receiveMessage.send({ messageType: 'published', value: { sendId: sendId, results: results } });
      })
    })
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
          /*
                    user.fetchProfile({ cacheUsage: NDKSubscriptionCacheUsage.CACHE_FIRST })
                      .then(async (profile) => {
                      app.ports.receiveMessage.send({ messageType: 'user', value: { pubKey: user.pubkey, profile: profile }  });
                      localStorage.setItem('isLoggedIn', JSON.stringify(true));
                    })
          */
        }
      }
      )
    }
  }

  function loginSignUp(app) {
    const nostrLoginOptions = {
    };
    launchNostrLoginDialog(nostrLoginOptions);
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