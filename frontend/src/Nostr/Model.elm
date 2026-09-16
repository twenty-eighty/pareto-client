module Nostr.Model exposing
    ( Model
    , Msg(..)
    , TestMode(..)
    , empty
    , init
    , requestRelayNip11
    )

{-| Central Nostr store shape and bootstrap.
Orchestration (`update`, getters, ingest) stays in `Nostr`.
-}

import BrowserEnv exposing (Environment(..))
import Dict exposing (Dict)
import Http
import Nostr.Article exposing (Article)
import Nostr.BookmarkList exposing (BookmarkList)
import Nostr.BookmarkSet exposing (BookmarkSet)
import Nostr.CashuWallet as CashuWallet exposing (CashuTokenEvent, CashuWallet, NutzapMintRecommendation)
import Nostr.Community exposing (Community)
import Nostr.CommunityList exposing (CommunityReference)
import Nostr.ContentRequest exposing (ContentRequestState)
import Nostr.External as External exposing (Hooks)
import Nostr.FollowSet exposing (FollowSet)
import Nostr.Highlights exposing (Highlight)
import Nostr.Nip05 as Nip05 exposing (Nip05, Nip05String)
import Nostr.Nip05Cache exposing (Nip05CacheEntry, Nip05RequestTarget)
import Nostr.Nip10 exposing (TextNote)
import Nostr.Nip11 exposing (Nip11Info, fetchNip11)
import Nostr.Nip18 exposing (Repost)
import Nostr.Nip22 exposing (CommentType)
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Nutzaps exposing (Nutzap)
import Nostr.Profile exposing (Profile, ProfileValidation)
import Nostr.Reactions exposing (Reaction)
import Nostr.Relay as Relay exposing (Relay, RelayState(..), RelayUrl)
import Nostr.RelayList as RelayList
import Nostr.RelayListMetadata exposing (RelayMetadata)
import Nostr.Request exposing (Request, RequestId)
import Nostr.Send exposing (SendRequest, SendRequestId)
import Nostr.Types exposing (Address, EventId, Following, IncomingMessage, PubKey)
import Nostr.Zaps exposing (ZapReceipt)
import Pareto
import Portal
import Set exposing (Set)
import Time exposing (Posix)


type alias Model =
    { articlesByAddress : Dict Address Article
    , articlesByAuthor : Dict PubKey (List Article)
    , articlesByDate : List Article
    , articlesById : Dict EventId Article
    , articleDraftsByDate : List Article
    , articleDraftsById : Dict EventId Article
    , articleDraftRelays : Dict EventId (Dict String RelayUrl)
    , bookmarkLists : Dict PubKey BookmarkList
    , bookmarkSets : Dict PubKey BookmarkSet
    , commentsByAddress : Dict Address (Dict EventId CommentType)
    , articleDetailsRequested : Set Address
    , contentRequestStates : Dict RequestId ContentRequestState
    , communities : Dict PubKey (List Community)
    , communityLists : Dict PubKey (List CommunityReference)
    , defaultRelays : List RelayUrl
    , defaultUser : Maybe PubKey
    , deletedAddresses : Set Address
    , deletedEvents : Dict EventId (Set PubKey) -- all pubkeys that tried to delete an event
    , environment : Environment
    , fileStorageServerLists : Dict PubKey (List String)
    , followLists : Dict PubKey (List Following)
    , followSets : Dict PubKey (Dict String FollowSet) -- follow sets; keys pubKey / identifier
    , highlightsByAddress : Dict Address (Dict EventId Highlight)
    , muteLists : Dict PubKey (List Following)
    , picturePosts : Dict EventId PicturePost
    , nip05Cache : Dict Nip05String Nip05CacheEntry
    , pubKeyByNip05 : Dict Nip05String PubKey
    , poolState : RelayState
    , portalUserInfoPubKey : Dict PubKey Portal.PortalCheckResponse
    , portalUserInfoNip05 : Dict String Portal.PortalCheckResponse
    , profiles : Dict PubKey Profile
    , profileValidations : Dict PubKey ProfileValidation
    , reactionsForEventId : Dict EventId (Dict PubKey Reaction)
    , reactionsForAddress : Dict Address (Dict PubKey Reaction)
    , relays : Dict String Relay
    , relayMetadataLists : Dict PubKey (List RelayMetadata)
    , relaysForPubKey : Dict PubKey (List RelayUrl)
    , repostsByAddress : Dict Address (Dict PubKey Repost)
    , repostsByEventId : Dict EventId (Dict PubKey Repost)
    , searchRelayLists : Dict PubKey (List RelayUrl)
    , privateRelayLists : Dict PubKey (List RelayUrl)
    , localRelays : List RelayUrl
    , shortTextNotes : Dict EventId TextNote
    , shortTextNotesReplies : Dict EventId (Dict EventId TextNote)
    , userServerLists : Dict PubKey (List String)
    , zapReceiptsAddress : Dict String (Dict String ZapReceipt)
    , zapReceiptsEvents : Dict String (Dict String ZapReceipt)
    , cashuWallet : Maybe CashuWallet
    , cashuTokens : Dict EventId CashuTokenEvent
    , nutzapMintRecommendations : Dict PubKey NutzapMintRecommendation
    , nutzapsAddress : Dict String (Dict String Nutzap)
    , nutzapsEvents : Dict String (Dict String Nutzap)
    , cashuBalance : Int
    , redeemedNutzapIds : Set String
    , pendingNutzapRedeems : Set String
    , hooks : Hooks Msg
    , errors : List String
    , requests : Dict RequestId Request
    , sendRequests : Dict SendRequestId SendRequest
    , lastRequestId : RequestId
    , lastSendId : RequestId
    , lastSendRequestId : SendRequestId
    , testMode : TestMode
    }


type Msg
    = ReceivedMessage IncomingMessage
    | CheckNip05Cache Nip05RequestTarget Nip05 Posix
    | Nip05Fetched Nip05 Posix (Result Http.Error Nip05.Nip05Data)
    | Nip11Fetched RelayUrl (Result Http.Error Nip11Info)
    | ReceivedPortalCheckResultPubKey PubKey (Result Http.Error Portal.PortalCheckResponse)
    | ReceivedPortalCheckResultNip05 Nip05 (Result Http.Error Portal.PortalCheckResponse)



-- this type is intentionally separate from the definition in BrowserEnv as these modules should function without each other


type TestMode
    = TestModeOff
    | TestModeEnabled


empty : Model
empty =
    { articlesByAddress = Dict.empty
    , articlesByAuthor = Dict.empty
    , articlesByDate = []
    , articlesById = Dict.empty
    , articleDraftsByDate = []
    , articleDraftsById = Dict.empty
    , articleDraftRelays = Dict.empty
    , bookmarkLists = Dict.empty
    , bookmarkSets = Dict.empty
    , commentsByAddress = Dict.empty
    , articleDetailsRequested = Set.empty
    , contentRequestStates = Dict.empty
    , communities = Dict.empty
    , communityLists = Dict.empty
    , defaultRelays = []
    , defaultUser = Nothing
    , deletedAddresses = Set.empty
    , deletedEvents = Dict.empty
    , environment = StandAlone
    , fileStorageServerLists = Dict.empty
    , hooks = External.noopHooks
    , nip05Cache = Dict.empty
    , picturePosts = Dict.empty
    , pubKeyByNip05 = Pareto.bootstrapPubKeyByNip05
    , poolState = RelayStateUnknown
    , followLists = Dict.singleton Pareto.authorsKey Pareto.authorsFollowList
    , followSets = Dict.empty
    , highlightsByAddress = Dict.empty
    , muteLists = Dict.empty
    , portalUserInfoPubKey = Dict.empty
    , portalUserInfoNip05 = Dict.empty
    , profiles = Dict.empty
    , profileValidations = Dict.empty
    , reactionsForEventId = Dict.empty
    , reactionsForAddress = Dict.empty
    , relayMetadataLists = Dict.empty
    , relays = Dict.empty
    , relaysForPubKey = Dict.empty
    , repostsByAddress = Dict.empty
    , repostsByEventId = Dict.empty
    , searchRelayLists = Dict.empty
    , privateRelayLists = Dict.empty
    , localRelays = []
    , shortTextNotes = Dict.empty
    , shortTextNotesReplies = Dict.empty
    , userServerLists = Dict.empty
    , zapReceiptsAddress = Dict.empty
    , zapReceiptsEvents = Dict.empty
    , cashuWallet = Nothing
    , cashuTokens = Dict.empty
    , nutzapMintRecommendations = Dict.empty
    , nutzapsAddress = Dict.empty
    , nutzapsEvents = Dict.empty
    , cashuBalance = 0
    , redeemedNutzapIds = Set.empty
    , pendingNutzapRedeems = Set.empty
    , errors = []
    , requests = Dict.singleton 0 { id = 0, relatedKinds = [], states = [], description = "preoloaded data" }
    , sendRequests = Dict.empty
    , lastRequestId = 0
    , lastSendId = 0
    , lastSendRequestId = 0
    , testMode = TestModeOff
    }


init : Hooks Msg -> Environment -> TestMode -> List RelayUrl -> List RelayUrl -> ( Model, Cmd Msg )
init hooks environment testMode relayUrls localRelays =
    let
        combinedRelays =
            RelayList.withUniqueEntries (relayUrls ++ localRelays)

        actualRelayUrls =
            -- make sure we get NIP-11 information for test relays
            if testMode == TestModeEnabled then
                Pareto.testRelayUrls ++ combinedRelays

            else
                combinedRelays

        model =
            { empty
                | hooks = hooks
                , environment = environment
                , relays = Relay.initFromUrls combinedRelays
                , defaultRelays = relayUrls
                , localRelays = RelayList.withUniqueEntries localRelays
                , testMode = testMode
            }
    in
    ( model
    , Cmd.batch
        [ hooks.connect combinedRelays
        , requestRelayNip11 model actualRelayUrls
        ]
    )


requestRelayNip11 : Model -> List RelayUrl -> Cmd Msg
requestRelayNip11 model relayUrls =
    relayUrls
        |> List.map (\relayUrl -> fetchNip11 (model.environment /= StandAlone) (Nip11Fetched relayUrl) (Relay.toHttp relayUrl))
        |> Cmd.batch
