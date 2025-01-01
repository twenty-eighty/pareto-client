module Pareto exposing (..)

import Nostr.Event exposing (Kind(..))
import Nostr.HandlerInformation exposing (HandlerInformation)
import Nostr.Profile exposing (Profile)
import Nostr.Types exposing (PubKey, RelayRole(..), RelayUrl)
import Time
import Nostr.Nip05 as Nip05


-- the follow list of this pubkey contains all Pareto authors
authorsKey : PubKey
authorsKey =
    "0f47948ccf4d12064ede2e0aa744868a2443cb1c42b32c06191e0d902205abef"

rssAuthorsKey : PubKey
rssAuthorsKey =
    "0f4791c38e1236dc55f11acbf37a00da8879906d9374498378db8d6ea7952869"

editorKey : PubKey
editorKey =
    "0f479cb726c1578ca765d5ff6a0c58855263977d5d7cf7b4cea23d42d557c611"

-- name of client in "client" tag when publishing articles
client : String
client =
    "Pareto"

applicationDomain : String
applicationDomain =
    "pareto.space"

applicationUrl : String
applicationUrl =
    "https://" ++ applicationDomain

paretoNip05 : Nip05.Nip05
paretoNip05 =
    { user = "client"
    , domain = applicationDomain
    }

paretoRelay : RelayUrl
paretoRelay =
    "wss://nostr." ++ applicationDomain

defaultNip96Server : String
defaultNip96Server =
    "https://route96.pareto.space"

teamRelay : String
teamRelay =
    "team-relay.pareto.space"

defaultRelays : List String
defaultRelays =
    [ "nostr.pareto.space"
    , "pareto.nostr1.com"
    , "relay.snort.social"
    , "relay.nostr.band"
    , "relay.damus.io"
    , "nos.lol"
    , "offchain.pub"
    , "nostr.wine"
    ]

defaultSearchRelays : List String
defaultSearchRelays =
    [ "relay.nostr.band"
    , "nostr.wine"
    ]

defaultOutboxRelays : List { url : String , role : RelayRole }
defaultOutboxRelays =
    defaultRelays   
    |> List.map (\relayUrl -> { url = relayUrl , role = ReadWriteRelay })

defaultRelayUrls : List String
defaultRelayUrls =
    defaultRelays
    |> List.map (String.append "wss://")

paretoNpub : String
paretoNpub =
    "npub1parecl0l0w6nmtjn7wan9tg3p8kmkpa62c4a65tgq39n7smyu76sht8cm5"

paretoPubKey : PubKey
paretoPubKey =
    "0f479c7dff7bb53dae53f3bb32ad1109edbb07ba562bdd5168044b3f4364e7b5"

handlerIdentifier : String
handlerIdentifier =
    "8020802080208020"

supportedKinds : List Kind
supportedKinds =
    [ KindUserMetadata
    , KindEventDeletionRequest
    , KindFileMetadata
    , KindRelayListMetadata
    , KindBookmarkList
    , KindUserServerList
    , KindFileStorageServerList
    , KindClientAuthentication
    , KindBlobsStoredOnMediaservers
    , KindHTTPAuth
    , KindLongFormContent
    , KindDraftLongFormContent
    , KindDraft
    , KindHandlerRecommendation
    , KindHandlerInformation
    ]

supportedNips : List String
supportedNips =
    [ "01"
    -- , "02"
    -- , "04" -- Encrypted Direct Message
    , "07"
    , "09"
    , "11"
    , "19"
    , "21"
    , "23"
    , "24"
    , "25"
    , "31"
    , "37"
    , "42"
    , "44"
    , "51"
    , "65"
    , "89"
    , "94"
    , "96"
    , "98"
    ]

paretoAbout : String
paretoAbout =
    "A #Nostr client for publishing and an ecosystem for citizen journalism."

paretoHashtags : List String
paretoHashtags =
    [ "pareto"
    , "nostr client"
    , "freedom"
    , "journalism"
    ]

paretoProfile : Profile
paretoProfile =
    { nip05 = Just paretoNip05
    , lud16 = Just "donate2pareto@walletofsatoshi.com"
    , name = Just <| String.toLower client
    , displayName = Just client
    , about = Just paretoAbout
    , picture = Just "https://pfp.nostr.build/4fbcbefaee70ba58804014760d6408adf2e544e4f2d2f7c3be063db8e6ae8f0e.png"
    , banner = Just "https://image.nostr.build/ddc8d30046efb51767c3db33cc9edafb83bd0a61142bd84c4d1e55705b96c819.png"
    , website = Just applicationUrl
    , bot = Nothing
    , npub = Just paretoNpub
    , createdAt = Nothing
    , pubKey = paretoPubKey
    , identities = []
    }

paretoReferences : List (String, Maybe String)
paretoReferences =
    [ ("https://pareto.space/read", Nothing)
    ]

paretoWebTargets : List (String, Maybe String)
paretoWebTargets =
    [ ("https://pareto.space/a/<bech32>", Just "naddr")
--    , ("https://pareto.space/e/<bech32>", Just "nevent")
    , ("https://pareto.space/p/<bech32>", Just "nprofile")
    ]

paretoZapTargets : List (PubKey, RelayUrl, Maybe Int)
paretoZapTargets =
    [ (paretoPubKey, paretoRelay, Just 1)
    ]

paretoAltText : String
paretoAltText =
    "Nostr App: " ++ String.toLower client

applicationInformation : Time.Posix -> HandlerInformation
applicationInformation time =
    { alt = paretoAltText
    , handlerIdentifier = handlerIdentifier
    , hashtags = paretoHashtags
    , kinds = supportedKinds
    , pubKey = paretoPubKey
    , profile = paretoProfile
    , references = paretoReferences
    , time = time
    , webTargets = paretoWebTargets
    , zapTargets = paretoZapTargets
    }