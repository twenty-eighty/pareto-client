module Nostr exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Nostr.Article exposing (Article, articleFromEvent, filterMatchesArticle, tagReference)
import Nostr.BookmarkList exposing (BookmarkList, bookmarkListFromEvent)
import Nostr.BookmarkSet exposing (BookmarkSet, bookmarkSetFromEvent)
import Nostr.Community exposing (Community, communityDefinitionFromEvent)
import Nostr.CommunityList exposing (CommunityReference, communityListFromEvent)
import Nostr.Event exposing (Event, EventFilter, Kind(..), TagReference(..), decodeEvent, emptyEventFilter, kindFromNumber, numberForKind, tagReferenceToString)
import Nostr.FileStorageServerList exposing (fileStorageServerListFromEvent)
import Nostr.FollowList exposing (Following, followListFromEvent)
import Nostr.FollowSet exposing (FollowSet, followSetFromEvent)
import Nostr.Nip05 as Nip05 exposing (Nip05, Nip05String, fetchNip05Info, nip05ToString) 
import Nostr.Nip11 exposing (Nip11Info, fetchNip11)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileFromEvent)
import Nostr.Reactions
import Nostr.Relay exposing (Relay, RelayState(..))
import Nostr.RelayListMetadata exposing (RelayMetadata, relayListFromEvent)
import Nostr.Repost exposing (Repost, repostFromEvent)
import Nostr.Request exposing (Request, RequestData(..), RequestId, RequestState(..), relatedKindsForRequest)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.ShortNote exposing (ShortNote, shortNoteFromEvent)
import Nostr.Types exposing (EventId, PubKey)
import Nostr.Zaps exposing (ZapReceipt)
import Html.Attributes exposing (kind)
import Time
import Nostr.Types exposing (RelayUrl)
import Set


type alias OutgoingCommand =
    { command : String
    , value : Encode.Value
    }

type alias IncomingMessage =
    { messageType : String
    , value : Encode.Value
    }


type alias Hooks =
    { connect : List String -> Cmd Msg
    , receiveMessage : (IncomingMessage -> Msg) -> Sub Msg
    , requestEvents : String -> Bool -> RequestId -> Maybe (List String) -> EventFilter -> Cmd Msg
    , requestBlossomListAuth : RequestId -> String -> Cmd Msg
    , requestNip96Auth : RequestId -> String -> String -> Cmd Msg
    , sendEvent : SendId -> Event -> Cmd Msg
    }

type alias Model =
    { relays : List Relay
    , articlesByAuthor : Dict PubKey (List Article)
    , articleDraftsByAuthor : Dict PubKey (List Article)
    , articlesByDate : List Article
    , articleDraftsByDate : List Article
    , bookmarkLists : Dict PubKey BookmarkList
    , bookmarkSets : Dict PubKey BookmarkSet
    , communities : Dict PubKey (List Community)
    , communityLists : Dict PubKey (List CommunityReference)
    , fileStorageServerLists : Dict PubKey (List String)
    , followLists : Dict PubKey (List Following)
    , followSets : Dict PubKey (Dict String FollowSet) -- follow sets; keys pubKey / identifier
    , pubKeyByNip05 : Dict Nip05String PubKey
    , poolState : RelayState
    , profiles : Dict PubKey Nostr.Profile.Profile
    , profileValidations : Dict PubKey ProfileValidation
    , reactions : Dict EventId (Dict EventId Nostr.Reactions.Reaction)
    , relayMetadataLists : Dict PubKey (List RelayMetadata)
    , relaysForPubKey : Dict PubKey (List RelayUrl)
    , reposts : Dict EventId Repost
    , shortTextNotes : Dict String ShortNote
    , zapReceiptsAddress : Dict String (Dict String Nostr.Zaps.ZapReceipt)
    , zapReceiptsEvents : Dict String (Dict String Nostr.Zaps.ZapReceipt)
    , hooks : Hooks
    , errors : List String
    , requests : Dict RequestId Request
    , sendRequests : Dict SendRequestId SendRequest
    , lastRequestId : RequestId
    , lastSendId : RequestId
    , lastSendRequestId : SendRequestId
    }

type alias SendId = Int

type Msg
    = ReceivedMessage IncomingMessage
    | Nip05FetchedForPubKey PubKey Nip05 (Result Http.Error Nip05.Nip05Data)
    | Nip05FetchedForNip05 RequestId Nip05 (Result Http.Error Nip05.Nip05Data)
    | Nip11Fetched String (Result Http.Error Nip11Info)

-- the request ID will be incremented only in request when sending
createRequest : Model -> String -> List Kind -> RequestData -> Request
createRequest model description relatedKinds data =
    { id = model.lastRequestId
    , relatedKinds = relatedKinds
    , value = [ RequestCreated data ]
    , description = description
    }

addToRequest : Model -> Request -> RequestData -> (Model, Request)
addToRequest model request data =
    let
        extendedRequest =
            { request | value = request.value ++ [ RequestCreated data ] }

    in
    ({ model | requests = Dict.insert request.id extendedRequest model.requests }, extendedRequest)

doRequest : Model -> Request -> (Model, Cmd Msg)
doRequest model request =
    let
        -- increment request ID for next request
        newModel =
            { model | lastRequestId = model.lastRequestId + 1 } 
    in
    doRequestWithId newModel request.id request

-- this function is for subsequent requests using the same request ID as a previous one
doRequestWithId : Model -> RequestId -> Request -> (Model, Cmd Msg)
doRequestWithId model requestId request =
    let
        (updatedModel, updatedRequestData, requestCmds) =
            List.foldl (\requestState (modelAcc, reqAcc, cmdAcc) ->
                case requestState of
                    RequestCreated requestData ->
                        let
                            (requestModel, cmd) =
                                performRequest modelAcc request.description requestId requestData
                        in
                        (requestModel, reqAcc ++ [RequestSent requestData], cmdAcc ++ [cmd])

                    RequestSent _ ->
                        (modelAcc, reqAcc ++ [requestState], cmdAcc)

                ) (model, [], []) request.value

        updatedRequest =
            { request | value = updatedRequestData }

        requestCmd =
            case requestCmds of
                [] ->
                    Cmd.none
                [ cmd ] ->
                    cmd

                cmds ->
                    Cmd.batch cmds
            
    in
    ({ updatedModel | requests = Dict.insert requestId updatedRequest model.requests }, requestCmd)

performRequest : Model -> String -> RequestId -> RequestData -> (Model, Cmd Msg)
performRequest model description requestId requestData =
    case requestData of
        RequestArticle relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId relays eventFilter)

        RequestArticles eventFilter ->
            ( { model | articlesByDate = [] }
            , model.hooks.requestEvents description True requestId Nothing eventFilter)

        RequestArticlesFeed eventFilter ->
            ( { model | articlesByDate = [] }
            , model.hooks.requestEvents description False requestId Nothing eventFilter)

        RequestArticleDrafts eventFilter ->
            ( { model | articleDraftsByDate = [] }
            , model.hooks.requestEvents description False requestId Nothing eventFilter)

        RequestBookmarks eventFilter ->
            ( model, model.hooks.requestEvents description True requestId Nothing eventFilter)

        RequestCommunity relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId relays eventFilter)

        RequestFollowSets eventFilter ->
            ( model, model.hooks.requestEvents description True requestId Nothing eventFilter)

        RequestNip05AndArticle nip05 _ ->
            -- identifier not needed here, only after getting nip05 data
            ( model, fetchNip05Info (Nip05FetchedForNip05 requestId nip05) nip05 )

        RequestProfile relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId relays eventFilter)

        RequestProfileByNip05 nip05 ->
            ( model, fetchNip05Info (Nip05FetchedForNip05 requestId nip05) nip05 )

        RequestReactions eventFilter ->
            ( model, model.hooks.requestEvents description False requestId Nothing eventFilter)

        RequestUserData eventFilter ->
            ( model, model.hooks.requestEvents description True requestId Nothing eventFilter)

        RequestBlossomListAuth server ->
            ( model, model.hooks.requestBlossomListAuth requestId server)

        RequestNip98Auth url method ->
            ( model, model.hooks.requestNip96Auth requestId url method)

send : Model -> SendRequest -> (Model, Cmd Msg)
send model sendRequest =
    case sendRequest of
        SendLongFormDraft event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , model.hooks.sendEvent model.lastSendRequestId event )

getAuthor : Model -> PubKey -> Nostr.Profile.Author
getAuthor model pubKey =
    let
        validationStatus =
            getProfileValidationStatus model pubKey
            |> Maybe.withDefault ValidationUnknown
    in
    model.profiles
    |> Dict.get pubKey 
    |> Maybe.map (\profile -> Nostr.Profile.AuthorProfile profile validationStatus)
    |> Maybe.withDefault (Nostr.Profile.AuthorPubkey pubKey)

getProfileValidationStatus : Model -> PubKey -> Maybe ProfileValidation
getProfileValidationStatus model pubKey =
    Dict.get pubKey model.profileValidations

getArticlesByDate : Model -> List Article
getArticlesByDate model =
    model.articlesByDate

getArticleDraftsByDate : Model -> List Article
getArticleDraftsByDate model =
    model.articleDraftsByDate

getArticlesForAuthor : Model -> PubKey -> List Article
getArticlesForAuthor model pubKey =
    model.articlesByAuthor
    |> Dict.get pubKey
    |> Maybe.withDefault []

getArticleDraftsForAuthor : Model -> PubKey -> List Article
getArticleDraftsForAuthor model pubKey =
    model.articleDraftsByAuthor
    |> Dict.get pubKey
    |> Maybe.withDefault []

getArticleForNip19 : Model -> NIP19Type -> Maybe Article
getArticleForNip19 model nip19 =
    case nip19 of
        NAddr { identifier, kind, pubKey, relays } ->
            case kindFromNumber kind of
                KindLongFormContent ->
                    getArticleWithIdentifier model pubKey identifier            

                KindDraftLongFormContent ->
                    getArticleDraftWithIdentifier model pubKey identifier            

                _ ->
                    Nothing
        _ ->
            Nothing

getArticleWithIdentifier : Model -> PubKey -> String -> Maybe Article
getArticleWithIdentifier model pubKey identifier =
    model.articlesByAuthor
    |> Dict.get pubKey
    |> Maybe.andThen (filterArticlesWithIdentifier identifier)

getArticleDraftWithIdentifier : Model -> PubKey -> String -> Maybe Article
getArticleDraftWithIdentifier model pubKey identifier =
    model.articleDraftsByAuthor
    |> Dict.get pubKey
    |> Maybe.andThen (filterArticlesWithIdentifier identifier)

getLastRequestId : Model -> RequestId
getLastRequestId model =
    model.lastRequestId

getLastSendId : Model -> SendRequestId
getLastSendId model =
    model.lastSendRequestId

filterArticlesWithIdentifier : String -> List Article -> Maybe Article
filterArticlesWithIdentifier identifier articles =
    articles
    |> List.filter (\article -> article.identifier == Just identifier)
    |> List.head


getCommunityForNip19 : Model -> NIP19Type -> Maybe Community
getCommunityForNip19 model nip19 =
    case nip19 of
        NAddr { identifier, kind, pubKey, relays } ->
            model.communities
            |> Dict.get pubKey
            |> Maybe.andThen (filterCommunitiesWithIdentifier identifier)
            
        _ ->
            Nothing

filterCommunitiesWithIdentifier : String -> List Community -> Maybe Community
filterCommunitiesWithIdentifier identifier communities =
    communities
    |> List.filter (\community -> community.dtag == Just identifier)
    |> List.head


getFollowsList : Model -> PubKey -> Maybe (List Following)
getFollowsList model pubKey =
    Dict.get pubKey model.followLists

getBookmarks : Model -> PubKey -> Maybe BookmarkList
getBookmarks model pubKey =
    Dict.get pubKey model.bookmarkLists


getReactionsForArticle : Model -> Article -> Maybe (Dict String Nostr.Reactions.Reaction)
getReactionsForArticle model article =
    Dict.get article.id model.reactions

getRelaysForPubKey : Model -> PubKey -> Maybe (List RelayUrl)
getRelaysForPubKey model pubKey =
    Dict.get pubKey model.relaysForPubKey

getRequest : Model -> RequestId -> Maybe Request
getRequest model requestId =
    Dict.get requestId model.requests

getShortNoteById : Model -> String -> Maybe ShortNote
getShortNoteById model noteId =
    Dict.get noteId model.shortTextNotes


getZapReceiptsForArticle : Model -> Article -> Maybe (Dict String Nostr.Zaps.ZapReceipt)
getZapReceiptsForArticle model article =
    let
        tagRef = 
            tagReference article
    in
    
    case tagRef of
        TagReferenceEventId eventId ->
            Dict.get eventId model.zapReceiptsEvents

        TagReferenceCode _ _ _ ->
            Dict.get (tagReferenceToString tagRef) model.zapReceiptsAddress

        TagReferenceIdentifier _ ->
            Nothing

        TagReferenceTag _ ->
            Nothing

getProfile : Model -> PubKey -> Maybe Profile
getProfile model pubkey =
    Dict.get pubkey model.profiles

getProfileByNip05 : Model -> Nip05 -> Maybe Profile
getProfileByNip05 model nip05 =
    getPubKeyByNip05 model nip05
    |> Maybe.andThen (getProfile model)

getPubKeyByNip05 : Model -> Nip05 -> Maybe PubKey
getPubKeyByNip05 model nip05 =
    Dict.get (nip05ToString nip05) model.pubKeyByNip05

requestCommunityPostApprovals : Model -> Community -> Cmd Msg
requestCommunityPostApprovals model community =
    profileFilterForCommunityPostApprovals community
    |> model.hooks.requestEvents "Community post approvals" False -1 Nothing

profileFilterForCommunityPostApprovals : Community -> EventFilter
profileFilterForCommunityPostApprovals community =
    { authors = Just [ community.pubKey ]
    , kinds = Just [ KindCommunityPostApproval ]
    , ids = Nothing
    , tagReferences = Just [ TagReferenceCode KindCommunityDefinition community.pubKey (Maybe.withDefault "" community.dtag) ]
    , limit = Nothing
    , since = Nothing
    , until = Nothing
    }

requestUserData : Model -> PubKey -> (Model, Cmd Msg)
requestUserData model pubKey =
    let
        request =
            { authors = Just [ pubKey ]
            , kinds = Just
                [ KindUserMetadata
                , KindFollows
                , KindMuteList
                , KindRelayListMetadata
                , KindCommunitiesList
                , KindFollowSets
                , KindCommunitiesList
                , KindFileStorageServerList
                ]
            , ids = Nothing
            , tagReferences = Nothing
            , limit = Nothing
            , since = Nothing
            , until = Nothing
            }
            -- assumption: our standard relays are good for the user's profile
            |> RequestProfile Nothing
            |> createRequest model "Related data for logged-in user" []
    in
    doRequest model request

{-
requestRelatedInfo : Model -> PubKey -> Cmd Msg
requestRelatedInfo model pubKey =
    { emptyEventFilter | authors = Just [ pubKey ], kinds = Just [ KindFollows, KindMuteList, KindRelayListMetadata, KindCommunitiesList, KindFollowSets, KindCommunitiesList ] }
    |> model.hooks.requestEvents -1

requestProfiles : Model -> List PubKey -> Maybe (Cmd Msg)
requestProfiles model authors =
    authors
    |> getMissingProfilePubKeys model
    |> profileFilterForAuthors
    |> Maybe.map (model.hooks.requestEvents -1)
-}
getMissingProfilePubKeys : Model -> List PubKey -> List PubKey
getMissingProfilePubKeys model pubKeys =
    pubKeys
    |> List.filterMap (\pubKey ->
            case getProfile model pubKey of
                Just _ ->
                    Nothing
                Nothing ->
                    Just pubKey
        )

profileFilterForAuthors : List String -> Maybe EventFilter
profileFilterForAuthors authors =
    if List.isEmpty authors then
        Nothing
    else
        Just
            { authors = Just authors
            , kinds = Just [ KindUserMetadata ]
            , ids = Nothing
            , tagReferences = Nothing
            , limit = Nothing
            , since = Nothing
            , until = Nothing
            }

getCommunityList : Model -> PubKey -> Maybe (List CommunityReference)
getCommunityList model pubKey =
    Dict.get pubKey model.communityLists

getInteractions : Model -> Article -> Nostr.Reactions.Interactions
getInteractions model article =
    { zaps = getZapReceiptsCountForArticle model article
    , highlights = Nothing
    , reactions = getReactionsCountForArticle model article
    , reposts = Nothing
    , notes = Nothing
    , bookmarks = Nothing
    }

getZapReceiptsCountForArticle : Model -> Article -> Maybe Int
getZapReceiptsCountForArticle model article =
    case getZapReceiptsForArticle model article of
        Just receiptsDict ->
            Dict.values receiptsDict
            |> List.foldl addZapAmount 0
            |> Just

        Nothing ->
            Nothing

addZapAmount : ZapReceipt -> Int -> Int
addZapAmount zapReceipt prevSum =
    zapReceipt.amount
    |> Maybe.map (\amount -> prevSum + amount)
    |> Maybe.withDefault prevSum

getReactionsCountForArticle : Model -> Article -> Maybe Int
getReactionsCountForArticle model article =
    article
    |> getReactionsForArticle model
    |> Maybe.map Dict.size


profileFilterForReactions : List TagReference -> Maybe EventFilter
profileFilterForReactions tagReferences =
    if List.isEmpty tagReferences then
        Nothing
    else
        Just
            { authors = Nothing
            , kinds = Just [ KindZapReceipt, KindHighlights, KindRepost, KindShortTextNote, KindReaction, KindBookmarkSets ]
            , ids = Nothing
            , tagReferences = Just tagReferences
            , limit = Nothing
            , since = Nothing
            , until = Nothing
            }

articleFromList : Model -> EventFilter -> List Article -> Maybe Article
articleFromList model filter articles =
    articles
    |> List.filter (filterMatchesArticle filter)
    |> List.head

cmdBatch2 : Cmd msg -> Cmd msg -> Cmd msg
cmdBatch2 cmd1 cmd2 =
    Cmd.batch [cmd1, cmd2]


empty : Model
empty =
    { relays = []
    , articlesByAuthor = Dict.empty
    , articleDraftsByAuthor = Dict.empty
    , articlesByDate = []
    , articleDraftsByDate = []
    , bookmarkLists = Dict.empty
    , bookmarkSets = Dict.empty
    , communities = Dict.empty
    , communityLists = Dict.empty
    , fileStorageServerLists = Dict.empty
    , hooks =
        { connect = \ _ -> Cmd.none
        , receiveMessage = \_ -> Sub.none
        , requestEvents = \_ _ _ _ _ -> Cmd.none
        , requestBlossomListAuth = \_ _ -> Cmd.none
        , requestNip96Auth = \_ _ _ -> Cmd.none
        , sendEvent = \_ _ -> Cmd.none
        }
    , pubKeyByNip05 = Dict.empty
    , poolState = RelayStateUnknown
    , followLists = Dict.empty
    , followSets = Dict.empty
    , profiles = Dict.empty
    , profileValidations = Dict.empty
    , reactions = Dict.empty
    , relayMetadataLists = Dict.empty
    , relaysForPubKey = Dict.empty
    , reposts = Dict.empty
    , shortTextNotes = Dict.empty
    , zapReceiptsAddress = Dict.empty
    , zapReceiptsEvents = Dict.empty
    , errors = []
    , requests = Dict.empty
    , sendRequests = Dict.empty
    , lastRequestId = 0
    , lastSendId = 0
    , lastSendRequestId = 0
    }

init : Hooks -> List String -> (Model, Cmd Msg)
init hooks relayUrls =
    ({ empty | hooks = hooks, relays = initRelayList relayUrls }
    , Cmd.batch
        [ hooks.connect (List.map Nostr.Relay.websocketUrl relayUrls)
        , requestRelayNip11 relayUrls
        ]
    )

requestRelayNip11 : List String -> Cmd Msg
requestRelayNip11 relayUrls =
    relayUrls
    |> List.map (\urlWithoutProtocol -> fetchNip11 (Nip11Fetched urlWithoutProtocol) urlWithoutProtocol)
    |> Cmd.batch


initRelayList : List String -> List Relay
initRelayList relayUrls =
    relayUrls
    |> List.map (\urlWithoutProtocol -> { urlWithoutProtocol = urlWithoutProtocol, state = RelayStateUnknown, nip11 = Nothing })

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceivedMessage message ->
            case message.messageType of
                "connecting" ->
                    ({ model | poolState = RelayConnecting }, Cmd.none)

                "connected" ->
                    ({ model | poolState = RelayConnected }, Cmd.none)

                "relay:notice" ->
                    (model, Cmd.none)

                "relay:connected" ->
                    case Decode.decodeValue Decode.string message.value of
                        Ok relayUrlWithoutProtocol ->
                            ({ model | relays = Nostr.Relay.updateRelayStatus relayUrlWithoutProtocol RelayConnected model.relays }, Cmd.none)
                        
                        Err error ->
                            ({ model | errors = Decode.errorToString error :: model.errors }, Cmd.none)

                "relay:ready" ->
                    case Decode.decodeValue Decode.string message.value of
                        Ok relayUrlWithoutProtocol ->
                            ({ model | relays = Nostr.Relay.updateRelayStatus relayUrlWithoutProtocol RelayReady model.relays }, Cmd.none)
                        
                        Err error ->
                            ({ model | errors = Decode.errorToString error :: model.errors }, Cmd.none)

                "relay:disconnected" ->
                    case Decode.decodeValue Decode.string message.value of
                        Ok relayUrlWithoutProtocol ->
                            ({ model | relays = Nostr.Relay.updateRelayStatus relayUrlWithoutProtocol RelayDisconnected model.relays }, Cmd.none)
                        
                        Err error ->
                            ({ model | errors = Decode.errorToString error :: model.errors }, Cmd.none)

                "profiles" ->
                    case Decode.decodeValue (Decode.list Nostr.Profile.pubkeyProfileDecoder) message.value of
                        Ok pubkeyProfiles ->
                            updateWithPubkeyProfiles model pubkeyProfiles

                        Err error ->
                            ({ model | errors = Decode.errorToString error :: model.errors }, Cmd.none)

                "reactions" ->
                    case Decode.decodeValue (Decode.list Nostr.Reactions.nostrReactionDecoder) message.value of
                        Ok reactions ->
                            updateWithReactions model reactions

                        Err error ->
                            ({ model | errors = Decode.errorToString error :: model.errors }, Cmd.none)

                "zap_receipts" ->
                    case Decode.decodeValue (Decode.list Nostr.Zaps.nostrZapReceiptDecoder) message.value of
                        Ok zapReceipts ->
                            updateWithZapReceipts model zapReceipts

                        Err error ->
                            ({ model | errors = Decode.errorToString error :: model.errors }, Cmd.none)

                "events" ->
                    case (Decode.decodeValue (Decode.field "requestId" Decode.int) message.value,
                        Decode.decodeValue (Decode.field "kind" Decode.int) message.value,
                        Decode.decodeValue (Decode.field "events" (Decode.list Nostr.Event.decodeEvent)) message.value) of
                        (Ok requestId, Ok kindNum, Ok events) ->
                            updateModelWithEvents model requestId (kindFromNumber kindNum) events

                        (_, _, Ok _) ->
                            ({ model | errors = "Error decoding request ID or kind" :: model.errors }, Cmd.none)

                        (_, _, Err errorDecodingEvents) ->
                            ({ model | errors = Decode.errorToString errorDecodingEvents :: model.errors }, Cmd.none)
                _ ->
                    (model, Cmd.none)

        Nip05FetchedForPubKey pubKey nip05 (Ok nip05Data) ->
        -- TODO: store relays for user
            let
                validationStatus =
                    Dict.get nip05.user nip05Data.names
                    |> Maybe.map (\pubKeyInNip05Data ->
                            if pubKeyInNip05Data == pubKey then
                                ValidationSucceeded
                            else
                                ValidationNotMatchingPubKey
                        )
                    |> Maybe.withDefault ValidationNameMissing
            in
            (updateProfileWithValidationStatus model pubKey validationStatus, Cmd.none)

        Nip05FetchedForPubKey pubKey _ (Err error) ->
            (updateProfileWithValidationStatus model pubKey (ValidationNetworkError error), Cmd.none)

        Nip05FetchedForNip05 requestId nip05 (Ok nip05Data) ->
            updateModelWithNip05Data model requestId nip05 nip05Data


        Nip05FetchedForNip05 requestId nip05 (Err error) ->
            ( { model | errors = ("Error fetching NIP05 data for " ++ nip05ToString nip05 ++ ": " ++ httpErrorToString error) :: model.errors}, Cmd.none )

        Nip11Fetched urlWithoutProtocol (Ok info) ->
            ({ model | relays = Nostr.Relay.updateRelayNip11 urlWithoutProtocol info model.relays }, Cmd.none)

        Nip11Fetched urlWithoutProtocol (Err err) ->
            -- Handle error, e.g., log it, retry, or display to user
            ( { model | errors = ("Error fetching NIP11 data for " ++ urlWithoutProtocol ++ ": " ++ httpErrorToString err) :: model.errors}, Cmd.none )


updateModelWithEvents : Model -> Int -> Kind -> List Event -> (Model, Cmd Msg)
updateModelWithEvents model requestId kind events =
    case kind of
        KindBookmarkList ->
            updateModelWithBookmarkLists model events

        KindBookmarkSets ->
            updateModelWithBookmarkSets model events

        KindCommunityDefinition ->
            updateModelWithCommunityDefinitions model events

        KindCommunitiesList ->
            updateModelWithCommunityLists model events

        KindFileStorageServerList ->
            updateModelWithFileStorageServerLists model requestId events

        KindFollows ->
            updateModelWithFollowLists model events

        KindFollowSets ->
            updateModelWithFollowSets model events

        KindLongFormContent ->
            updateModelWithLongFormContent model requestId events

        KindDraftLongFormContent ->
            updateModelWithLongFormContentDraft model requestId events

        KindShortTextNote ->
            updateModelWithShortTextNotes model requestId events

        KindUserMetadata ->
            updateModelWithUserMetadata model requestId events

        KindRelayListMetadata ->
            updateModelWithRelayListMetadata model events

        _ ->
            (model, Cmd.none)


updateModelWithBookmarkLists : Model -> List Event -> (Model, Cmd Msg)
updateModelWithBookmarkLists model events =
    let
        -- usually there should be only one for the logged-in user
        bookmarkLists =
            events
            |> List.map bookmarkListFromEvent
            |> List.foldl (\(pubKey, bookmarkList) dict ->
                Dict.insert pubKey bookmarkList dict
                ) model.bookmarkLists
    in
    ({ model | bookmarkLists = bookmarkLists}, Cmd.none)

updateModelWithBookmarkSets : Model -> List Event -> (Model, Cmd Msg)
updateModelWithBookmarkSets model events =
    let
        -- usually there should be only one for the logged-in user
        bookmarkSets =
            events
            |> List.map bookmarkSetFromEvent
            |> List.foldl (\(pubKey, bookmarkList) dict ->
                Dict.insert pubKey bookmarkList dict
                ) model.bookmarkSets
    in
    ({ model | bookmarkSets = bookmarkSets}, Cmd.none)


updateModelWithCommunityDefinitions : Model -> List Event -> (Model, Cmd Msg)
updateModelWithCommunityDefinitions model events =
    let
        communityDefinitions =
            events
            |> List.map communityDefinitionFromEvent
            |> List.foldl (\communityDefinition dict ->
                Dict.insert communityDefinition.pubKey [communityDefinition] dict
                ) model.communities
    in
    ({ model | communities = communityDefinitions }, Cmd.none)

updateModelWithCommunityLists : Model -> List Event -> (Model, Cmd Msg)
updateModelWithCommunityLists model events =
    let
        -- usually there should be only one for the logged-in user
        communityLists =
            events
            |> List.map communityListFromEvent
            |> List.foldl (\(pubKey, communityList) dict ->
                Dict.insert pubKey communityList dict
                ) model.communityLists
    in
    ({ model | communityLists = communityLists}, Cmd.none)

updateModelWithFileStorageServerLists : Model -> RequestId -> List Event -> (Model, Cmd Msg)
updateModelWithFileStorageServerLists model requestId events =
    let
        -- usually there should be only one for the logged-in user
        fileStorageServerLists =
            events
            |> List.map fileStorageServerListFromEvent
            |> List.foldl (\(pubKey, fileStorageServerList) dict ->
                Dict.insert pubKey fileStorageServerList dict
                ) model.fileStorageServerLists
    in
    ({ model | fileStorageServerLists = fileStorageServerLists}, Cmd.none)

updateModelWithLongFormContent : Model -> RequestId -> List Event -> (Model, Cmd Msg)
updateModelWithLongFormContent model requestId events =
    let
        (articles, newErrors) =
            events
            |> List.map articleFromEvent
            |> List.foldl (\decodingResult (articleAcc, errors) ->
                case decodingResult of
                    Ok article ->
                        (article :: articleAcc, errors)
                    Err decodingErrors ->
                        (articleAcc, decodingErrors ++ errors)
                ) ([], [])

        -- sort articles, newest first
        articlesByDate =
            articles
            |> List.sortBy (\article ->
                article.publishedAt
                |> Maybe.map (\publishedAt -> Time.posixToMillis publishedAt * -1)
                |> Maybe.withDefault 0
                )

        articlesByAuthor =
            articles
            |> List.foldl (\article dict ->
                    case Dict.get article.author dict of
                        Just articleList ->
                            Dict.insert article.author (article :: articleList) dict
                        Nothing ->
                            Dict.insert article.author [ article ] dict
                ) model.articlesByAuthor

        maybeRequest =
            Dict.get requestId model.requests

        (requestModel, requestCmd) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForArticles model articles request

                Nothing ->
                    (model, Cmd.none)
    in
    ({ requestModel | articlesByAuthor = articlesByAuthor, articlesByDate = articlesByDate, errors = newErrors ++ model.errors }, requestCmd)

updateModelWithLongFormContentDraft : Model -> RequestId -> List Event -> (Model, Cmd Msg)
updateModelWithLongFormContentDraft model requestId events =
    let
        (articles, newErrors) =
            events
            |> List.map articleFromEvent
            |> List.foldl (\decodingResult (articleAcc, errors) ->
                case decodingResult of
                    Ok article ->
                        (article :: articleAcc, errors)
                    Err decodingErrors ->
                        (articleAcc, decodingErrors ++ errors)
                ) ([], [])

        -- sort articles, newest first
        articleDraftsByDate =
            articles
            |> List.sortBy (\article ->
                article.publishedAt
                |> Maybe.map (\publishedAt -> Time.posixToMillis publishedAt * -1)
                |> Maybe.withDefault 0
                )

        articleDraftsByAuthor =
            articles
            |> List.foldl (\article dict ->
                    case Dict.get article.author dict of
                        Just articleList ->
                            Dict.insert article.author (article :: articleList) dict
                        Nothing ->
                            Dict.insert article.author [ article ] dict
                ) model.articleDraftsByAuthor
    in
    ({ model | articleDraftsByAuthor = articleDraftsByAuthor, articleDraftsByDate = articleDraftsByDate, errors = newErrors ++ model.errors }, Cmd.none)

requestRelatedKindsForArticles : Model -> List Article -> Request -> (Model, Cmd Msg)
requestRelatedKindsForArticles model articles request =
    let
        maybeEventFilterForAuthorProfiles =
            articles
            |> Nostr.Article.uniqueArticleAuthors
            |> getMissingProfilePubKeys model
            |> profileFilterForAuthors

        (requestProfileModel, extendedRequestProfile) =
            case maybeEventFilterForAuthorProfiles of
                Just eventFilterForAuthorProfiles ->
                    -- TODO: add relays for request
                    eventFilterForAuthorProfiles
                    |> RequestProfile Nothing
                    |> addToRequest model request

                Nothing ->
                    (model, request)

        (extendedModel, extendedRequestReactions) =
            articles
            |> List.map Nostr.Article.tagReference
            |> profileFilterForReactions
            |> Maybe.map RequestReactions
            |> Maybe.map (addToRequest requestProfileModel extendedRequestProfile)
            |> Maybe.withDefault (requestProfileModel, extendedRequestProfile)

    in
    doRequest extendedModel extendedRequestReactions


updateModelWithShortTextNotes : Model -> RequestId -> List Event -> (Model, Cmd Msg)
updateModelWithShortTextNotes model requestId events =
    let
        shortTextNotes =
            events
            |> List.map shortNoteFromEvent

        shortTextNotesDict =
            shortTextNotes
            |> List.foldl (\shortNote acc ->
                Dict.insert shortNote.id shortNote acc
            ) model.shortTextNotes

        maybeRequest =
            Dict.get requestId model.requests

        (requestModel, requestCmd) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForShortNotes model shortTextNotes request

                Nothing ->
                    (model, Cmd.none)
    in
    ({ requestModel | shortTextNotes = shortTextNotesDict }, requestCmd)

requestRelatedKindsForShortNotes : Model -> List ShortNote -> Request -> (Model, Cmd Msg)
requestRelatedKindsForShortNotes model shortNotes request =
    let
        authorPubKeys =
            shortNotes
            |> List.map .pubKey
            |> Set.fromList
            |> Set.toList

        maybeEventFilterForAuthorProfiles =
            authorPubKeys
            |> getMissingProfilePubKeys model
            |> profileFilterForAuthors

        (requestProfileModel, extendedRequestProfile) =
            case maybeEventFilterForAuthorProfiles of
                Just eventFilterForAuthorProfiles ->
                    -- TODO: add relays for request
                    eventFilterForAuthorProfiles
                    |> RequestProfile Nothing
                    |> addToRequest model request

                Nothing ->
                    (model, request)

        (extendedModel, extendedRequestReactions) =
            shortNotes
            |> List.map Nostr.ShortNote.tagReference
            |> profileFilterForReactions
            |> Maybe.map RequestReactions
            |> Maybe.map (addToRequest requestProfileModel extendedRequestProfile)
            |> Maybe.withDefault (requestProfileModel, extendedRequestProfile)

    in
    doRequest extendedModel extendedRequestReactions


updateModelWithUserMetadata : Model -> RequestId -> List Event -> (Model, Cmd Msg)
updateModelWithUserMetadata model requestId events =
    let
        profiles =
            events
            |> List.filterMap profileFromEvent

        profilesSum =
            profiles
            |> List.foldl (\profile dict ->
                Dict.insert profile.pubKey profile dict
                ) model.profiles

        nip05Requests =
            profiles
            |> List.filterMap (\profile ->
                    Maybe.map (\nip05 -> fetchNip05Info (Nip05FetchedForPubKey profile.pubKey nip05) nip05) profile.nip05
                )

        relatedKinds =
            Dict.get requestId model.requests
            |> relatedKindsForRequest

        (requestModel, relatedRequestCmd) =
            requestRelatedKindsForProfiles model profiles relatedKinds

        requests =
            relatedRequestCmd :: nip05Requests
            |> Cmd.batch
    in
    ({ requestModel | profiles = profilesSum }, requests)

requestRelatedKindsForProfiles : Model -> List Profile -> List Kind -> (Model, Cmd Msg)
requestRelatedKindsForProfiles model profiles kinds =
    if List.member KindLongFormContent kinds then
        profiles
        |> List.map .pubKey
        |> requestArticlesForAuthors model
    else
        (model, Cmd.none)

requestArticlesForAuthors : Model -> List PubKey -> (Model, Cmd Msg)
requestArticlesForAuthors model pubKeys =
    createRequest model "Articles for authors" [] (RequestArticlesFeed { emptyEventFilter | authors = Just pubKeys, kinds = Just [KindLongFormContent] })
    |> doRequest model

updateModelWithRelayListMetadata : Model -> List Event -> (Model, Cmd Msg)
updateModelWithRelayListMetadata model events =
    let
        -- usually there should be only one for the logged-in user
        relayLists =
            events
            |> List.map relayListFromEvent
            |> List.foldl (\(pubKey, relayList) dict ->
                Dict.insert pubKey relayList dict
                ) model.relayMetadataLists
    in
    ({ model | relayMetadataLists = relayLists }, Cmd.none)

updateModelWithFollowLists : Model -> List Event -> (Model, Cmd Msg)
updateModelWithFollowLists model events =
    let
        followLists =
            events
            |> List.map followListFromEvent
            |> List.foldl (\{pubKey, following} dict ->
                Dict.insert pubKey following dict
                ) model.followLists
    in
    ({ model | followLists = followLists }, Cmd.none )



updateModelWithFollowSets : Model -> List Event -> (Model, Cmd Msg)
updateModelWithFollowSets model events =
    let
        followSets =
            events
            |> List.filterMap followSetFromEvent
            |> List.foldl (\(pubKey, followSet) dict ->
                    case Dict.get pubKey dict of
                        Just followSetDict ->
                            Dict.insert pubKey (Dict.insert followSet.identifier followSet followSetDict) dict

                        Nothing ->
                            Dict.insert pubKey (Dict.singleton followSet.identifier followSet) dict
                ) model.followSets
    in
    ({ model | followSets = followSets }, Cmd.none)

insertIntoEventsDict : Event -> Dict Int (List Event) -> Dict Int (List Event)
insertIntoEventsDict event dict =
    let
        kindNum =
            numberForKind event.kind
    in

    case Dict.get kindNum dict of
        Just events ->
            Dict.insert kindNum (event :: events) dict

        Nothing ->
            Dict.singleton kindNum [ event ]

updateModelWithNip05Data : Model -> RequestId -> Nip05 -> Nip05.Nip05Data -> (Model, Cmd Msg)
updateModelWithNip05Data model requestId nip05 nip05Data =
    let
        modelWithValidatedNip05 =
            validateNip05 model nip05 nip05Data

        maybePubKey =
            Dict.get nip05.user nip05Data.names

        loadedProfile =
            getProfileByNip05 modelWithValidatedNip05 nip05

        maybeRequest =
            getRequest model requestId

        maybeRelays =
            case nip05Data.relays of
                Just relayDict ->
                    maybePubKey
                    |> Maybe.andThen (\pubKey -> (Dict.get pubKey relayDict))

                Nothing ->
                    Nothing

        (requestModel, requestProfileCmd) = 
            case (loadedProfile, maybeRequest, maybePubKey) of
                ( Nothing, Just request, Just pubKey ) ->
                    { emptyEventFilter | authors = Just [ pubKey ], kinds = Just [KindUserMetadata] }
                    |> RequestProfile maybeRelays
                    |> addToRequest modelWithValidatedNip05 request
                    |> (\(modelWithRequest, extendedRequest) -> doRequest modelWithRequest extendedRequest)

                ( _, _, _ ) ->
                    (modelWithValidatedNip05, Cmd.none)
    in
    (requestModel, requestProfileCmd)

validateNip05 : Model -> Nip05 -> Nip05.Nip05Data -> Model
validateNip05 model nip05 nip05Data =
    let
        pubKeyInNip05Data =
            Dict.get (nip05.user) nip05Data.names

        loadedProfile =
            pubKeyInNip05Data
            |> Maybe.andThen (getProfile model)

        (pubKeyForUpdate, validationStatus) =
            case (pubKeyInNip05Data, loadedProfile) of
                (Just pubKey, Just profile) ->
                    -- profile is already loaded - update status for pubKey in profile
                    if pubKey == profile.pubKey then
                        (Just profile.pubKey, ValidationSucceeded)
                    else
                        (Just profile.pubKey, ValidationNotMatchingPubKey)

                (Just _, Nothing) ->
                    -- profile not yet loaded - load for pubKey in NIP-05 data
                    (pubKeyInNip05Data, ValidationPending)

                (Nothing, _) ->
                    -- name missing in response
                    (Nothing, ValidationNameMissing)

    in
    updateProfileWithValidationStatus model "1234" validationStatus
    -- updateProfileWithValidationStatus model profile.pubKey validationStatus


updateProfileWithValidationStatus : Model -> PubKey -> ProfileValidation -> Model
updateProfileWithValidationStatus model pubKey valid =
    let
        maybeProfile =
            Dict.get pubKey model.profiles

        updatedNip05Dict =
            maybeProfile
            |> Maybe.andThen .nip05
            |> Maybe.map (\nip05 ->
                Dict.insert (nip05ToString nip05) pubKey model.pubKeyByNip05
                )
            |> Maybe.withDefault model.pubKeyByNip05
    in
    { model | profileValidations = Dict.insert pubKey valid model.profileValidations, pubKeyByNip05 = updatedNip05Dict }


updateWithReactions : Model -> List Nostr.Reactions.Reaction -> (Model, Cmd Msg)
updateWithReactions model reactions =
    let
        extendedReactions =
            reactions
            |> List.foldl extendReactionsDict model.reactions 

    in
    ({ model | reactions = extendedReactions }, Cmd.none )

extendReactionsDict :
    Nostr.Reactions.Reaction
    -> Dict EventId (Dict EventId Nostr.Reactions.Reaction)
    -> Dict EventId (Dict EventId Nostr.Reactions.Reaction)
extendReactionsDict reaction reactionDict =
    case reaction.noteIdReactedTo of
        Just noteIdReactedTo ->
            reactionDict
            |> Dict.get noteIdReactedTo
            |> Maybe.map (Dict.insert reaction.id reaction)
            |> Maybe.map (\extendedReactionDict -> Dict.insert noteIdReactedTo extendedReactionDict reactionDict)
            |> Maybe.withDefault (Dict.insert noteIdReactedTo (Dict.singleton reaction.id reaction) reactionDict)

        Nothing ->
            reactionDict

updateWithPubkeyProfiles : Model -> List Nostr.Profile.PubkeyProfile -> (Model, Cmd Msg)
updateWithPubkeyProfiles model pubkeyProfiles =
    let

        nip05Requests =
            pubkeyProfiles
            |> List.filterMap (\{ pubKey, profile } ->
                    Maybe.map (\nip05 -> fetchNip05Info (Nip05FetchedForPubKey pubKey nip05) nip05) profile.nip05
                )
            |> Cmd.batch

        profilesSum =
            pubkeyProfiles
            |> List.foldl (\{ pubKey, profile } ->
                    Dict.insert pubKey profile
                ) model.profiles 
    in

    ({ model | profiles = profilesSum }, nip05Requests )
                        

updateWithZapReceipts : Model -> List Nostr.Zaps.ZapReceipt -> (Model, Cmd Msg)
updateWithZapReceipts model zapReceipts =
    let
        zapReceiptsForAddresses =
            zapReceipts
            |> List.filterMap (\receipt ->
                case receipt.address of
                    Just address ->
                        Just (address, receipt)

                    Nothing ->
                        Nothing
                ) 
            |> List.foldl addToZapReceiptDict model.zapReceiptsAddress

        zapReceiptsForEvents =
            zapReceipts
            |> List.filterMap (\receipt ->
                case receipt.event of
                    Just event ->
                        Just (event, receipt)

                    Nothing ->
                        Nothing
                ) 
            |> List.foldl addToZapReceiptDict model.zapReceiptsEvents
    in

    ({ model | zapReceiptsAddress = zapReceiptsForAddresses, zapReceiptsEvents = zapReceiptsForEvents }, Cmd.none)
                        
addToZapReceiptDict : (String, Nostr.Zaps.ZapReceipt) -> Dict String (Dict String Nostr.Zaps.ZapReceipt) -> Dict String (Dict String Nostr.Zaps.ZapReceipt)
addToZapReceiptDict (address, receipt) receiptDict =
    let
        updatedDictForAddress =
            Dict.get address receiptDict
            |> Maybe.map (Dict.insert receipt.id receipt)
            |> Maybe.withDefault (Dict.singleton receipt.id receipt)
    in
    Dict.insert address updatedDictForAddress receiptDict

subscriptions : Model -> Sub Msg
subscriptions model =
    model.hooks.receiveMessage ReceivedMessage
