module Nostr exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Nostr.Article exposing (Article, addressComponentsForArticle, addressForArticle, articleFromEvent, filterMatchesArticle, publishedTime, tagReference)
import Nostr.Blossom exposing (userServerListFromEvent)
import Nostr.BookmarkList exposing (BookmarkList, bookmarkListEvent, bookmarkListFromEvent, bookmarkListWithArticle, bookmarkListWithShortNote, bookmarkListWithoutArticle, bookmarkListWithoutShortNote, emptyBookmarkList)
import Nostr.BookmarkSet exposing (BookmarkSet, bookmarkSetFromEvent)
import Nostr.Community exposing (Community, communityDefinitionFromEvent)
import Nostr.CommunityList exposing (CommunityReference, communityListFromEvent)
import Nostr.DeletionRequest exposing (deletionRequestFromEvent)
import Nostr.Event exposing (AddressComponents, Event, EventFilter, Kind(..), Tag(..), TagReference(..), buildAddress, emptyEvent, emptyEventFilter, informationForKind, kindFromNumber, numberForKind, tagReferenceToString)
import Nostr.External exposing (Hooks)
import Nostr.FileStorageServerList exposing (fileStorageServerListFromEvent)
import Nostr.FollowList exposing (emptyFollowList, followListEvent, followListFromEvent, followListWithPubKey, followListWithoutPubKey, pubKeyIsFollower)
import Nostr.FollowSet exposing (FollowSet, followSetFromEvent)
import Nostr.Nip05 as Nip05 exposing (Nip05, Nip05String, fetchNip05Info, nip05ToString)
import Nostr.Nip11 exposing (Nip11Info, fetchNip11)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileFromEvent)
import Nostr.Reactions exposing (Reaction, reactionFromEvent)
import Nostr.Relay exposing (Relay, RelayState(..), hostWithoutProtocol, relayUrlDecoder)
import Nostr.RelayList exposing (relayListFromEvent)
import Nostr.RelayListMetadata exposing (RelayMetadata, relayMetadataListFromEvent)
import Nostr.Repost exposing (Repost)
import Nostr.Request as Request exposing (Request, RequestData(..), RequestId, RequestState(..), relatedKindsForRequest)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.ShortNote exposing (ShortNote, shortNoteFromEvent)
import Nostr.Types exposing (Address, EventId, Following(..), IncomingMessage, PubKey, RelayRole(..), RelayUrl)
import Nostr.Zaps exposing (ZapReceipt)
import Pareto
import Set exposing (Set)
import Time


type alias Model =
    { articlesByAddress : Dict Address Article
    , articlesByAuthor : Dict PubKey (List Article)
    , articlesByDate : List Article
    , articleDraftsByDate : List Article
    , articleDraftRelays : Dict EventId (Set RelayUrl)
    , bookmarkLists : Dict PubKey BookmarkList
    , bookmarkSets : Dict PubKey BookmarkSet
    , communities : Dict PubKey (List Community)
    , communityLists : Dict PubKey (List CommunityReference)
    , defaultRelays : List String
    , defaultUser : Maybe PubKey
    , deletedAddresses : Set Address
    , deletedEvents : Set EventId
    , fileStorageServerLists : Dict PubKey (List String)
    , followLists : Dict PubKey (List Following)
    , followSets : Dict PubKey (Dict String FollowSet) -- follow sets; keys pubKey / identifier
    , pubKeyByNip05 : Dict Nip05String PubKey
    , poolState : RelayState
    , profiles : Dict PubKey Nostr.Profile.Profile
    , profileValidations : Dict PubKey ProfileValidation
    , reactionsForEventId : Dict EventId (Dict PubKey Nostr.Reactions.Reaction)
    , reactionsForAddress : Dict Address (Dict PubKey Nostr.Reactions.Reaction)
    , relays : Dict String Relay
    , relayMetadataLists : Dict PubKey (List RelayMetadata)
    , relaysForPubKey : Dict PubKey (List RelayUrl)
    , reposts : Dict EventId Repost
    , searchRelayLists : Dict PubKey (List RelayUrl)
    , sendsNewsletterPubKey : Dict PubKey Bool
    , sendsNewsletterNip05 : Dict String Bool
    , shortTextNotes : Dict String ShortNote
    , userServerLists : Dict PubKey (List String)
    , zapReceiptsAddress : Dict String (Dict String Nostr.Zaps.ZapReceipt)
    , zapReceiptsEvents : Dict String (Dict String Nostr.Zaps.ZapReceipt)
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
    | Nip05FetchedForPubKey PubKey Nip05 (Result Http.Error Nip05.Nip05Data)
    | Nip05FetchedForNip05 RequestId Nip05 (Result Http.Error Nip05.Nip05Data)
    | Nip11Fetched String (Result Http.Error Nip11Info)
    | ReceivedNewsletterAuthorCheckResultPubKey PubKey (Result Http.Error NewsletterCheckResponse)
    | ReceivedNewsletterAuthorCheckResultNip05 Nip05 (Result Http.Error NewsletterCheckResponse)



-- this type is intentionally separate from the definition in BrowserEnv as these modules should function without each other


type TestMode
    = TestModeOff
    | TestModeEnabled


isAuthor : Model -> PubKey -> Bool
isAuthor model userPubKey =
    getFollowsList model Pareto.authorsKey
        |> Maybe.map (pubKeyIsFollower userPubKey)
        |> Maybe.withDefault False


isEditor : Model -> PubKey -> Bool
isEditor model userPubKey =
    getFollowsList model Pareto.editorKey
        |> Maybe.map (pubKeyIsFollower userPubKey)
        |> Maybe.withDefault False


isBetaTester : Model -> PubKey -> Bool
isBetaTester model userPubKey =
    getFollowsList model Pareto.betaTestKey
        |> Maybe.map (pubKeyIsFollower userPubKey)
        |> Maybe.withDefault False



-- Newsletters


updateNewsletterAvailabilityPubKey : Model -> PubKey -> Cmd Msg
updateNewsletterAvailabilityPubKey model pubKey =
    case Dict.get pubKey model.sendsNewsletterPubKey of
        Just _ ->
            Cmd.none

        Nothing ->
            Http.get
                { url = Pareto.newsletterAuthorCheckEndpointPubKey ++ "/" ++ pubKey
                , expect = Http.expectJson (ReceivedNewsletterAuthorCheckResultPubKey pubKey) decodeNewsletterCheckResponse
                }


updateNewsletterAvailabilityNip05 : Model -> Nip05 -> Cmd Msg
updateNewsletterAvailabilityNip05 model nip05 =
    let
        nip05String =
            nip05ToString nip05
    in
    case Dict.get nip05String model.sendsNewsletterNip05 of
        Just _ ->
            Cmd.none

        Nothing ->
            Http.get
                { url = Pareto.newsletterAuthorCheckEndpointNip05 ++ "/" ++ nip05String
                , expect = Http.expectJson (ReceivedNewsletterAuthorCheckResultNip05 nip05) decodeNewsletterCheckResponse
                }


sendsNewsletterPubKey : Model -> PubKey -> Maybe Bool
sendsNewsletterPubKey model pubKey =
    Dict.get pubKey model.sendsNewsletterPubKey


sendsNewsletterNip05 : Model -> Nip05 -> Maybe Bool
sendsNewsletterNip05 model nip05 =
    let
        resultViaPubKey =
            getPubKeyByNip05 model nip05
                |> Maybe.andThen (sendsNewsletterPubKey model)

        resultViaNip05 =
            Dict.get (nip05ToString nip05) model.sendsNewsletterNip05
    in
    case ( resultViaPubKey, resultViaNip05 ) of
        ( Just True, _ ) ->
            Just True

        ( _, Just True ) ->
            Just True

        ( Just _, Just _ ) ->
            Just False

        _ ->
            Nothing


type alias NewsletterCheckResponse =
    { email : Bool
    }


decodeNewsletterCheckResponse : Decode.Decoder NewsletterCheckResponse
decodeNewsletterCheckResponse =
    Decode.succeed NewsletterCheckResponse
        |> required "email" Decode.bool



-- the request ID will be incremented only in request when sending


createRequest : Model -> String -> List Kind -> RequestData -> Request
createRequest model description relatedKinds data =
    { id = model.lastRequestId
    , relatedKinds = relatedKinds
    , states = [ RequestCreated data ]
    , description = description
    }


addToRequest : Model -> Request -> RequestData -> ( Model, Request )
addToRequest model request data =
    let
        extendedRequest =
            { request | states = request.states ++ [ RequestCreated data ] }
    in
    ( { model | requests = Dict.insert request.id extendedRequest model.requests }, extendedRequest )


doRequest : Model -> Request -> ( Model, Cmd Msg )
doRequest model request =
    let
        -- increment request ID for next request
        newModel =
            { model | lastRequestId = model.lastRequestId + 1 }
    in
    doRequestWithId newModel request.id request



-- this function is for subsequent requests using the same request ID as a previous one


doRequestWithId : Model -> RequestId -> Request -> ( Model, Cmd Msg )
doRequestWithId model requestId request =
    let
        ( updatedModel, updatedRequestData, requestCmds ) =
            List.foldl
                (\requestState ( modelAcc, reqAcc, cmdAcc ) ->
                    case requestState of
                        RequestCreated requestData ->
                            let
                                ( requestModel, cmd ) =
                                    performRequest modelAcc request.description requestId requestData
                            in
                            ( requestModel, reqAcc ++ [ RequestSent requestData ], cmdAcc ++ [ cmd ] )

                        RequestSent _ ->
                            ( modelAcc, reqAcc ++ [ requestState ], cmdAcc )
                )
                ( model, [], [] )
                request.states

        updatedRequest =
            { request | states = updatedRequestData }

        requestCmd =
            case requestCmds of
                [] ->
                    Cmd.none

                [ cmd ] ->
                    cmd

                cmds ->
                    Cmd.batch cmds
    in
    ( { updatedModel | requests = Dict.insert requestId updatedRequest model.requests }, requestCmd )


performRequest : Model -> String -> RequestId -> RequestData -> ( Model, Cmd Msg )
performRequest model description requestId requestData =
    let
        configuredRelays =
            case model.defaultUser of
                Just pubKey ->
                    getReadRelayUrlsForPubKey model pubKey
                        |> List.map (\url -> "wss://" ++ url)

                Nothing ->
                    getDefaultRelays model
                        |> List.map (\url -> "wss://" ++ url)
    in
    case requestData of
        RequestArticle relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ] )

        RequestArticles eventFilters ->
            ( { model | articlesByDate = [] }
            , model.hooks.requestEvents description True requestId configuredRelays eventFilters
            )

        RequestArticlesFeed eventFilters ->
            ( { model | articlesByDate = [] }
            , model.hooks.requestEvents description False requestId configuredRelays eventFilters
            )

        RequestArticleDrafts eventFilters ->
            ( { model | articleDraftsByDate = [] }
            , model.hooks.requestEvents description False requestId configuredRelays eventFilters
            )

        RequestBookmarks eventFilter ->
            ( model, model.hooks.requestEvents description True requestId configuredRelays [ eventFilter ] )

        RequestCommunity relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ] )

        RequestDeletionRequests eventFilter ->
            ( model, model.hooks.requestEvents description True requestId configuredRelays [ eventFilter ] )

        RequestFollowSets eventFilter ->
            ( model, model.hooks.requestEvents description True requestId configuredRelays [ eventFilter ] )

        RequestMediaServerLists eventFilter ->
            ( model, model.hooks.requestEvents description True requestId configuredRelays [ eventFilter ] )

        RequestNip05AndArticle nip05 _ ->
            -- identifier not needed here, only after getting nip05 data
            ( model, fetchNip05Info (Nip05FetchedForNip05 requestId nip05) nip05 )

        RequestProfile relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ] )

        RequestProfileByNip05 nip05 ->
            ( model, fetchNip05Info (Nip05FetchedForNip05 requestId nip05) nip05 )

        RequestReactions eventFilter ->
            ( model, model.hooks.requestEvents description False requestId configuredRelays [ eventFilter ] )

        RequestRelayLists eventFilter ->
            ( model, model.hooks.requestEvents description False requestId configuredRelays [ eventFilter ] )

        RequestSubscribers eventFilter ->
            ( model, model.hooks.requestEvents description False requestId (getApplicationDataRelays model) [ eventFilter ] )

        RequestUserData eventFilter ->
            ( model, model.hooks.requestEvents description True requestId configuredRelays [ eventFilter ] )

        RequestBlossomAuth serverUrl content method ->
            ( model, model.hooks.requestBlossomAuth requestId serverUrl content method )

        RequestNip98Auth serverUrl apiUrl method ->
            ( model, model.hooks.requestNip96Auth requestId serverUrl apiUrl method )

        RequestSearchResults eventFilters ->
            ( { model | articlesByDate = [] }, model.hooks.searchEvents description True requestId (getSearchRelayUrls model model.defaultUser) eventFilters )

        RequestShortNote relays eventFilter ->
            ( model, model.hooks.requestEvents description True requestId (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ] )


send : Model -> SendRequest -> ( Model, Cmd Msg )
send model sendRequest =
    case sendRequest of
        SendApplicationData event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getApplicationDataRelays model) event
            )

        SendBookmarkListWithArticle pubKey address ->
            let
                bookmarkList =
                    getBookmarks model pubKey
                        |> Maybe.withDefault emptyBookmarkList

                event =
                    bookmarkListWithArticle bookmarkList address
                        |> bookmarkListEvent pubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model pubKey) event
            )

        SendBookmarkListWithoutArticle pubKey address ->
            let
                bookmarkList =
                    getBookmarks model pubKey
                        |> Maybe.withDefault emptyBookmarkList

                event =
                    bookmarkListWithoutArticle bookmarkList address
                        |> bookmarkListEvent pubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model pubKey) event
            )

        SendBookmarkListWithShortNote pubKey eventId ->
            let
                bookmarkList =
                    getBookmarks model pubKey
                        |> Maybe.withDefault emptyBookmarkList

                event =
                    bookmarkListWithShortNote bookmarkList eventId
                        |> bookmarkListEvent pubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model pubKey) event
            )

        SendBookmarkListWithoutShortNote pubKey eventId ->
            let
                bookmarkList =
                    getBookmarks model pubKey
                        |> Maybe.withDefault emptyBookmarkList

                event =
                    bookmarkListWithoutShortNote bookmarkList eventId
                        |> bookmarkListEvent pubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model pubKey) event
            )

        SendClientRecommendation relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendFollowList userPubKey followList ->
            let
                event =
                    followList
                        |> followListEvent userPubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model userPubKey) event
            )

        SendFollowListWithPubKey userPubKey toBeFollowedPubKey ->
            let
                followList =
                    getFollowsList model userPubKey
                        |> Maybe.withDefault emptyFollowList

                event =
                    followListWithPubKey followList toBeFollowedPubKey
                        |> followListEvent userPubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model userPubKey) event
            )

        SendFollowListWithoutPubKey userPubKey toBeUnfollowedPubKey ->
            let
                followList =
                    getFollowsList model userPubKey
                        |> Maybe.withDefault emptyFollowList

                event =
                    followListWithoutPubKey followList toBeUnfollowedPubKey
                        |> followListEvent userPubKey
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model (getWriteRelayUrlsForPubKey model userPubKey) event
            )

        SendHandlerInformation relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendLongFormArticle relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendLongFormDraft relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendFileStorageServerList relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendDeletionRequest relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendReaction userPubKey eventId articlePubKey addressComponents ->
            let
                event =
                    emptyEvent userPubKey KindReaction
            in
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model
                (getWriteRelayUrlsForPubKey model userPubKey)
                { event
                    | content = "+"
                    , tags =
                        [ AddressTag addressComponents
                        , EventIdTag eventId
                        , PublicKeyTag articlePubKey Nothing Nothing
                        ]
                }
            )

        SendRelayList relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )

        SendProfile relays event ->
            ( { model | lastSendRequestId = model.lastSendRequestId + 1, sendRequests = Dict.insert model.lastSendRequestId sendRequest model.sendRequests }
            , sendEvent model relays event
            )


sendEvent : Model -> List RelayUrl -> Event -> Cmd Msg
sendEvent model relays event =
    let
        actualWriteRelays =
            if model.testMode == TestModeEnabled then
                Pareto.testRelayUrls

            else
                relays
    in
    model.hooks.sendEvent model.lastSendRequestId actualWriteRelays event


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


getArticle : Model -> AddressComponents -> Maybe Article
getArticle model addressComponents =
    Dict.get (buildAddress addressComponents) model.articlesByAddress


getArticlesByDate : Model -> List Article
getArticlesByDate model =
    model.articlesByDate
        |> List.filter (filterDeletedArticle model)


resetArticles : Model -> Model
resetArticles model =
    { model | articlesByDate = [] }


getArticleDraftsByDate : Model -> List Article
getArticleDraftsByDate model =
    model.articleDraftsByDate
        |> List.filter (filterDeletedArticle model)


getArticleDraftWithIdentifier : Model -> PubKey -> String -> Maybe Article
getArticleDraftWithIdentifier model pubKey identifier =
    model.articleDraftsByDate
        |> List.filter (filterDeletedArticle model)
        |> List.filter
            (\article ->
                article.author
                    == pubKey
                    && article.identifier
                    == Just identifier
            )
        |> List.head


getArticlesForAuthor : Model -> PubKey -> List Article
getArticlesForAuthor model pubKey =
    model.articlesByAuthor
        |> Dict.get pubKey
        |> Maybe.withDefault []
        |> List.filter (filterDeletedArticle model)
        |> sortArticlesByDate


filterDeletedArticle : Model -> Article -> Bool
filterDeletedArticle model article =
    Set.member article.id model.deletedEvents
        || Set.member (addressForArticle article |> Maybe.withDefault "") model.deletedAddresses
        |> not


getArticleForAddressComponents : Model -> AddressComponents -> Maybe Article
getArticleForAddressComponents model addressComponents =
    case addressComponents of
        ( KindLongFormContent, pubKey, identifier ) ->
            getArticleWithIdentifier model pubKey identifier

        ( KindDraftLongFormContent, pubKey, identifier ) ->
            getArticleDraftWithIdentifier model pubKey identifier

        _ ->
            Nothing


getArticleForNip19 : Model -> NIP19Type -> Maybe Article
getArticleForNip19 model nip19 =
    case nip19 of
        NAddr { identifier, kind, pubKey } ->
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


getBlossomServers : Model -> PubKey -> List String
getBlossomServers model pubKey =
    model.userServerLists
        |> Dict.get pubKey
        |> Maybe.withDefault []


getDefaultNip96Servers : Model -> PubKey -> List String
getDefaultNip96Servers model pubKey =
    if isAuthor model pubKey then
        Pareto.defaultNip96ServersAuthors

    else
        Pareto.defaultNip96ServersPublic


getNip96Servers : Model -> PubKey -> List String
getNip96Servers model pubKey =
    model.fileStorageServerLists
        |> Dict.get pubKey
        |> Maybe.withDefault []


getLastRequestId : Model -> RequestId
getLastRequestId model =
    model.lastRequestId


getLastSendRequestId : Model -> SendRequestId
getLastSendRequestId model =
    model.lastSendRequestId


filterArticlesWithIdentifier : String -> List Article -> Maybe Article
filterArticlesWithIdentifier identifier articles =
    articles
        |> List.filter (\article -> article.identifier == Just identifier)
        |> List.head


getCommunityForNip19 : Model -> NIP19Type -> Maybe Community
getCommunityForNip19 model nip19 =
    case nip19 of
        NAddr { identifier, pubKey } ->
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


getReactionsForArticle : Model -> AddressComponents -> Maybe (Dict PubKey Nostr.Reactions.Reaction)
getReactionsForArticle model addressComponents =
    Dict.get (buildAddress addressComponents) model.reactionsForAddress


getRelaysForPubKey : Model -> PubKey -> List ( RelayRole, Relay )
getRelaysForPubKey model pubKey =
    let
        userRelaysFromNip05 =
            Dict.get pubKey model.relaysForPubKey
                |> Maybe.withDefault []

        userRelaysFromEvent =
            getRelayListForPubKey model pubKey

        testRelays =
            if model.testMode == TestModeEnabled then
                Pareto.testRelayUrls

            else
                []

        combinedRelayList =
            (userRelaysFromNip05 ++ testRelays)
                |> List.map (\relayUrl -> { role = ReadWriteRelay, url = relayUrl })
                |> List.append userRelaysFromEvent
                |> relayMetadataListWithUniqueEntries
                |> List.filterMap
                    (\{ role, url } ->
                        Maybe.map
                            (\relay -> ( role, relay ))
                            (Dict.get url model.relays)
                    )
    in
    combinedRelayList



-- this function only returns the relays obtained via relay metadata list (NIP-65 / kind 10002)


getRelayListForPubKey : Model -> PubKey -> List RelayMetadata
getRelayListForPubKey model pubKey =
    Dict.get pubKey model.relayMetadataLists
        |> Maybe.withDefault []


getNip65RelaysForPubKey : Model -> PubKey -> List ( RelayRole, Relay )
getNip65RelaysForPubKey model pubKey =
    Dict.get pubKey model.relayMetadataLists
        |> Maybe.map
            (\relayList ->
                relayList
                    |> relayMetadataListWithUniqueEntries
                    |> List.filterMap
                        (\{ role, url } ->
                            Maybe.map
                                (\relay -> ( role, relay ))
                                (Dict.get url model.relays)
                        )
            )
        |> Maybe.withDefault []


getNip65ReadRelaysForPubKey : Model -> PubKey -> List Relay
getNip65ReadRelaysForPubKey model pubKey =
    getNip65RelaysForPubKey model pubKey
        |> List.filterMap
            (\( role, relay ) ->
                if role == ReadRelay || role == ReadWriteRelay then
                    Just relay

                else
                    Nothing
            )


getNip65WriteRelaysForPubKey : Model -> PubKey -> List Relay
getNip65WriteRelaysForPubKey model pubKey =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls
            |> List.filterMap (getRelayData model)

    else
        getNip65RelaysForPubKey model pubKey
            |> List.filterMap
                (\( role, relay ) ->
                    if role == WriteRelay || role == ReadWriteRelay then
                        Just relay

                    else
                        Nothing
                )


getReadRelaysForPubKey : Model -> PubKey -> List Relay
getReadRelaysForPubKey model pubKey =
    getRelaysForPubKey model pubKey
        |> List.filterMap
            (\( role, relay ) ->
                if role == ReadRelay || role == ReadWriteRelay then
                    Just relay

                else
                    Nothing
            )


getReadRelayUrlsForPubKey : Model -> PubKey -> List String
getReadRelayUrlsForPubKey model pubKey =
    getReadRelaysForPubKey model pubKey
        |> List.map .urlWithoutProtocol


getWriteRelaysForPubKey : Model -> PubKey -> List Relay
getWriteRelaysForPubKey model pubKey =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls
            |> List.filterMap (getRelayData model)

    else
        getRelaysForPubKey model pubKey
            |> List.filterMap
                (\( role, relay ) ->
                    if role == WriteRelay || role == ReadWriteRelay then
                        Just relay

                    else
                        Nothing
                )


getWriteRelayUrlsForPubKey : Model -> PubKey -> List String
getWriteRelayUrlsForPubKey model pubKey =
    getWriteRelaysForPubKey model pubKey
        |> List.map .urlWithoutProtocol


getDraftRelayUrls : Model -> EventId -> List String
getDraftRelayUrls model articleId =
    model.articleDraftRelays
        |> Dict.get articleId
        |> Maybe.withDefault Set.empty
        |> Set.toList


getSearchRelayUrls : Model -> Maybe PubKey -> List RelayUrl
getSearchRelayUrls model maybePubKey =
    case maybePubKey of
        Just pubKey ->
            Dict.get pubKey model.searchRelayLists
                |> Maybe.withDefault (getSearchRelayUrls model Nothing)

        Nothing ->
            case relaysWithSearchCapability model of
                [] ->
                    -- this can happen before the relays have reported their NIP-11 data
                    Pareto.defaultSearchRelays
                        |> List.map (\urlWithoutProtocol -> "wss://" ++ urlWithoutProtocol)

                relayList ->
                    relayList
                        |> List.map (\urlWithoutProtocol -> "wss://" ++ urlWithoutProtocol)


getSearchRelaysForPubKey : Model -> PubKey -> List Relay
getSearchRelaysForPubKey model pubKey =
    Dict.get pubKey model.searchRelayLists
        |> Maybe.map
            (\relayUrls ->
                relayUrls
                    |> List.map
                        (\relayUrl ->
                            Dict.get (hostWithoutProtocol relayUrl) model.relays
                                |> Maybe.withDefault { urlWithoutProtocol = relayUrl, state = RelayStateUnknown, nip11 = Nothing }
                        )
            )
        |> Maybe.withDefault []



-- filter relays that claim to support NIP-50 (Search)


relaysWithSearchCapability : Model -> List RelayUrl
relaysWithSearchCapability model =
    model.relays
        |> Dict.values
        |> List.filterMap
            (\relay ->
                relay.nip11
                    |> Maybe.andThen
                        (\nip11 ->
                            nip11.supportedNips
                                |> Maybe.andThen
                                    (\supportedNips ->
                                        if List.member 50 supportedNips then
                                            Just relay.urlWithoutProtocol

                                        else
                                            Nothing
                                    )
                        )
            )


getRelaysForRequest : Model -> Maybe RequestId -> List RelayUrl
getRelaysForRequest model maybeRequestId =
    let
        maybeRequest =
            maybeRequestId
                |> Maybe.andThen (getRequest model)

        requestUrls =
            maybeRequest
                |> Maybe.andThen Request.relaysOfRequest
                |> Maybe.withDefault []
    in
    case requestUrls of
        [] ->
            getDefaultRelays model

        relayUrls ->
            relayUrls


getDefaultRelays : Model -> List RelayUrl
getDefaultRelays model =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls

    else
        model.defaultRelays


getApplicationDataRelays : Model -> List RelayUrl
getApplicationDataRelays model =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls

    else
        Pareto.applicationDataRelays


getRelayData : Model -> RelayUrl -> Maybe Relay
getRelayData model relayUrl =
    Dict.get relayUrl model.relays


getRequest : Model -> RequestId -> Maybe Request
getRequest model requestId =
    Dict.get requestId model.requests


getShortNoteById : Model -> String -> Maybe ShortNote
getShortNoteById model noteId =
    Dict.get noteId model.shortTextNotes


getShortNotes : Model -> List ShortNote
getShortNotes model =
    Dict.values model.shortTextNotes


getZapReceiptsForArticle : Model -> Article -> Maybe (Dict String Nostr.Zaps.ZapReceipt)
getZapReceiptsForArticle model article =
    let
        tagRef =
            tagReference article
    in
    case tagRef of
        TagReferenceEventId eventId ->
            Dict.get eventId model.zapReceiptsEvents

        TagReferenceCode _ ->
            Dict.get (tagReferenceToString tagRef) model.zapReceiptsAddress

        TagReferenceIdentifier _ ->
            Nothing

        TagReferencePubKey _ ->
            Nothing

        TagReferenceTag _ ->
            Nothing


getProfile : Model -> PubKey -> Maybe Profile
getProfile model pubKey =
    Dict.get pubKey model.profiles


getProfileByNip05 : Model -> Nip05 -> Maybe Profile
getProfileByNip05 model nip05 =
    getPubKeyByNip05 model nip05
        |> Maybe.andThen (getProfile model)


getPubKeyByNip05 : Model -> Nip05 -> Maybe PubKey
getPubKeyByNip05 model nip05 =
    Dict.get (nip05ToString nip05) model.pubKeyByNip05


requestCommunityPostApprovals : Model -> Community -> Cmd Msg
requestCommunityPostApprovals model community =
    [ eventFilterForCommunityPostApprovals community ]
        |> model.hooks.requestEvents "Community post approvals" False -1 []


eventFilterForCommunityPostApprovals : Community -> EventFilter
eventFilterForCommunityPostApprovals community =
    { emptyEventFilter
        | authors = Just [ community.pubKey ]
        , kinds = Just [ KindCommunityPostApproval ]
        , tagReferences = Just [ TagReferenceCode ( KindCommunityDefinition, community.pubKey, Maybe.withDefault "" community.dtag ) ]
    }


requestUserData : Model -> PubKey -> ( Model, Cmd Msg )
requestUserData model pubKey =
    let
        request =
            { emptyEventFilter
                | authors = Just [ pubKey ]
                , kinds =
                    Just
                        [ KindUserMetadata
                        , KindBlockedRelaysList
                        , KindBookmarkList
                        , KindBookmarkSets
                        , KindCommunitiesList
                        , KindFileStorageServerList
                        , KindFollows
                        , KindFollowSets
                        , KindMuteList
                        , KindRelayListMetadata
                        , KindRelayListForDMs
                        , KindRelaySets
                        , KindSearchRelaysList
                        , KindUserServerList
                        ]
            }
                -- assumption: our standard relays are good for the user's profile
                |> RequestProfile Nothing
                |> createRequest model "Related data for logged-in user" []
    in
    doRequest { model | defaultUser = Just pubKey } request


getMissingProfilePubKeys : Model -> List PubKey -> List PubKey
getMissingProfilePubKeys model pubKeys =
    pubKeys
        |> List.filterMap
            (\pubKey ->
                case getProfile model pubKey of
                    Just _ ->
                        Nothing

                    Nothing ->
                        Just pubKey
            )


eventFilterForAuthors : List String -> Maybe EventFilter
eventFilterForAuthors authors =
    if List.isEmpty authors then
        Nothing

    else
        Just
            { emptyEventFilter
                | authors = Just authors
                , kinds = Just [ KindUserMetadata ]
            }


getCommunityList : Model -> PubKey -> Maybe (List CommunityReference)
getCommunityList model pubKey =
    Dict.get pubKey model.communityLists


getInteractions : Model -> Maybe PubKey -> Article -> Nostr.Reactions.Interactions
getInteractions model maybePubKey article =
    let
        maybeAddressComponents =
            addressComponentsForArticle article
    in
    { zaps = getZapReceiptsCountForArticle model article
    , highlights = Nothing
    , reactions = getReactionsCountForArticle model article
    , reposts = Nothing
    , notes = Nothing
    , bookmarks = Maybe.map (getBookmarkListCountForAddressComponents model) maybeAddressComponents
    , isBookmarked =
        Maybe.map (isArticleBookmarked model article) maybePubKey
            |> Maybe.withDefault False
    , reaction =
        case ( maybePubKey, maybeAddressComponents ) of
            ( Just userPubKey, Just addressComponents ) ->
                getReactionForArticle model userPubKey addressComponents

            ( _, _ ) ->
                Nothing
    }


isArticleBookmarked : Model -> Article -> PubKey -> Bool
isArticleBookmarked model article pubKey =
    let
        bookmarkList =
            getBookmarks model pubKey
                |> Maybe.withDefault emptyBookmarkList
    in
    bookmarkList.articles
        |> List.filter
            (\( kind, referencedPubKey, dCode ) ->
                article.kind
                    == kind
                    && article.author
                    == referencedPubKey
                    && article.identifier
                    == Just dCode
            )
        |> List.isEmpty
        |> not


getZapReceiptsCountForArticle : Model -> Article -> Maybe Int
getZapReceiptsCountForArticle model article =
    getZapReceiptsForArticle model article
        |> Maybe.andThen
            (\receiptsDict ->
                Dict.values receiptsDict
                    |> List.foldl addZapAmount 0
                    |> Just
            )


addZapAmount : ZapReceipt -> Int -> Int
addZapAmount zapReceipt prevSum =
    zapReceipt.amount
        |> Maybe.map (\amount -> prevSum + amount)
        |> Maybe.withDefault prevSum


getBookmarkListCountForAddressComponents : Model -> AddressComponents -> Int
getBookmarkListCountForAddressComponents model addressComponents =
    model.bookmarkLists
        |> Dict.values
        |> List.map
            (\bookmarkList ->
                bookmarkList.articles
                    |> List.filter (\articleAddressComponents -> articleAddressComponents == addressComponents)
                    |> List.length
            )
        |> List.sum


getReactionsCountForArticle : Model -> Article -> Maybe Int
getReactionsCountForArticle model article =
    article
        |> addressComponentsForArticle
        |> Maybe.andThen (getReactionsForArticle model)
        |> Maybe.map Dict.size


getReactionForArticle : Model -> PubKey -> AddressComponents -> Maybe Reaction
getReactionForArticle model pubKey addressComponents =
    getReactionsForArticle model addressComponents
        |> Maybe.andThen (Dict.get pubKey)


eventFilterForDeletionRequests : List TagReference -> Maybe EventFilter
eventFilterForDeletionRequests tagsReferences =
    if List.isEmpty tagsReferences then
        Nothing

    else
        Just
            { emptyEventFilter
                | kinds = Just [ KindEventDeletionRequest ]
                , tagReferences = Just tagsReferences
            }


eventFilterForReactions : List TagReference -> Maybe EventFilter
eventFilterForReactions tagReferences =
    if List.isEmpty tagReferences then
        Nothing

    else
        Just
            { emptyEventFilter
                | kinds = Just [ KindZapReceipt, KindHighlights, KindRepost, KindShortTextNote, KindReaction, KindBookmarkList, KindBookmarkSets ]
                , tagReferences = Just tagReferences
            }


articleFromList : EventFilter -> List Article -> Maybe Article
articleFromList filter articles =
    articles
        |> List.filter (filterMatchesArticle filter)
        |> List.head


cmdBatch2 : Cmd msg -> Cmd msg -> Cmd msg
cmdBatch2 cmd1 cmd2 =
    Cmd.batch [ cmd1, cmd2 ]


empty : Model
empty =
    { articlesByAddress = Dict.empty
    , articlesByAuthor = Dict.empty
    , articlesByDate = []
    , articleDraftsByDate = []
    , articleDraftRelays = Dict.empty
    , bookmarkLists = Dict.empty
    , bookmarkSets = Dict.empty
    , communities = Dict.empty
    , communityLists = Dict.empty
    , defaultRelays = []
    , defaultUser = Nothing
    , deletedAddresses = Set.empty
    , deletedEvents = Set.empty
    , fileStorageServerLists = Dict.empty
    , hooks =
        { connect = \_ -> Cmd.none
        , receiveMessage = \_ -> Sub.none
        , requestEvents = \_ _ _ _ _ -> Cmd.none
        , requestBlossomAuth = \_ _ _ _ -> Cmd.none
        , requestNip96Auth = \_ _ _ _ -> Cmd.none
        , searchEvents = \_ _ _ _ _ -> Cmd.none
        , sendEvent = \_ _ _ -> Cmd.none
        }
    , pubKeyByNip05 = Dict.empty
    , poolState = RelayStateUnknown
    , followLists = Dict.singleton Pareto.authorsKey paretoAuthorsFollowList
    , followSets = Dict.empty
    , profiles = Dict.empty
    , profileValidations = Dict.empty
    , reactionsForEventId = Dict.empty
    , reactionsForAddress = Dict.empty
    , relayMetadataLists = Dict.empty
    , relays = Dict.empty
    , relaysForPubKey = Dict.empty
    , reposts = Dict.empty
    , searchRelayLists = Dict.empty
    , sendsNewsletterPubKey = Dict.empty
    , sendsNewsletterNip05 = Dict.empty
    , shortTextNotes = Dict.empty
    , userServerLists = Dict.empty
    , zapReceiptsAddress = Dict.empty
    , zapReceiptsEvents = Dict.empty
    , errors = []
    , requests = Dict.empty
    , sendRequests = Dict.empty
    , lastRequestId = 0
    , lastSendId = 0
    , lastSendRequestId = 0
    , testMode = TestModeOff
    }


init : Hooks Msg -> TestMode -> List String -> ( Model, Cmd Msg )
init hooks testMode relayUrls =
    let
        actualRelayUrls =
            -- make sure we get NIP-11 information for test relays
            if testMode == TestModeEnabled then
                Pareto.testRelayUrls ++ relayUrls

            else
                relayUrls
    in
    ( { empty
        | hooks = hooks
        , relays = initRelayList relayUrls
        , defaultRelays = relayUrls
        , testMode = testMode
      }
    , Cmd.batch
        [ hooks.connect (List.map Nostr.Relay.websocketUrl relayUrls)
        , requestRelayNip11 actualRelayUrls
        ]
    )


paretoAuthorsFollowList : List Following
paretoAuthorsFollowList =
    Pareto.bootstrapAuthorsList
        |> List.map
            (\( nip05, authorPubKey ) ->
                FollowingPubKey
                    { pubKey = authorPubKey
                    , relay = Just Pareto.paretoRelay
                    , petname = Just nip05
                    }
            )


requestRelayNip11 : List String -> Cmd Msg
requestRelayNip11 relayUrls =
    relayUrls
        |> List.map (\urlWithoutProtocol -> fetchNip11 (Nip11Fetched urlWithoutProtocol) urlWithoutProtocol)
        |> Cmd.batch


initRelayList : List String -> Dict String Relay
initRelayList relayUrls =
    relayUrls
        |> List.map (\urlWithoutProtocol -> ( urlWithoutProtocol, { urlWithoutProtocol = urlWithoutProtocol, state = RelayStateUnknown, nip11 = Nothing } ))
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedMessage message ->
            case message.messageType of
                "connecting" ->
                    ( { model | poolState = RelayConnecting }, Cmd.none )

                "connected" ->
                    ( { model | poolState = RelayConnected }, Cmd.none )

                "relay:notice" ->
                    ( model, Cmd.none )

                "relay:connected" ->
                    case Decode.decodeValue relayUrlDecoder message.value of
                        Ok relayUrlWithoutProtocol ->
                            ( { model | relays = Nostr.Relay.updateRelayStatus relayUrlWithoutProtocol RelayConnected model.relays }, Cmd.none )

                        Err error ->
                            ( { model | errors = Decode.errorToString error :: model.errors }, Cmd.none )

                "relay:ready" ->
                    case Decode.decodeValue relayUrlDecoder message.value of
                        Ok relayUrlWithoutProtocol ->
                            ( { model | relays = Nostr.Relay.updateRelayStatus relayUrlWithoutProtocol RelayReady model.relays }, Cmd.none )

                        Err error ->
                            ( { model | errors = Decode.errorToString error :: model.errors }, Cmd.none )

                "relay:disconnected" ->
                    case Decode.decodeValue relayUrlDecoder message.value of
                        Ok relayUrlWithoutProtocol ->
                            ( { model | relays = Nostr.Relay.updateRelayStatus relayUrlWithoutProtocol RelayDisconnected model.relays }, Cmd.none )

                        Err error ->
                            ( { model | errors = Decode.errorToString error :: model.errors }, Cmd.none )

                "profiles" ->
                    case Decode.decodeValue (Decode.list Nostr.Profile.pubkeyProfileDecoder) message.value of
                        Ok pubkeyProfiles ->
                            updateWithPubkeyProfiles model pubkeyProfiles

                        Err error ->
                            ( { model | errors = Decode.errorToString error :: model.errors }, Cmd.none )

                "zap_receipts" ->
                    case Decode.decodeValue (Decode.list Nostr.Zaps.nostrZapReceiptDecoder) message.value of
                        Ok zapReceipts ->
                            updateWithZapReceipts model zapReceipts

                        Err error ->
                            ( { model | errors = Decode.errorToString error :: model.errors }, Cmd.none )

                "events" ->
                    case
                        ( Nostr.External.decodeRequestId message.value
                        , Nostr.External.decodeEventsKind message.value
                        )
                    of
                        ( Ok requestId, Ok kind ) ->
                            case Decode.decodeValue (Decode.field "events" (Decode.list Nostr.Event.decodeEvent)) message.value of
                                Ok events ->
                                    updateModelWithEvents model requestId kind events

                                Err errorDecodingEvents ->
                                    let
                                        kindDesc =
                                            kind
                                                |> informationForKind
                                                |> .description

                                        errorMessage =
                                            "Error decoding events of kind "
                                                ++ String.fromInt (numberForKind kind)
                                                ++ " ("
                                                ++ kindDesc
                                                ++ ") - request ID "
                                                ++ String.fromInt requestId
                                                ++ ": "
                                                ++ Decode.errorToString errorDecodingEvents
                                    in
                                    ( { model | errors = errorMessage :: model.errors }, Cmd.none )

                        ( _, _ ) ->
                            ( { model | errors = "Error decoding request ID or kind" :: model.errors }, Cmd.none )

                "error" ->
                    case Nostr.External.decodeReason message.value of
                        Ok error ->
                            ( { model | errors = error :: model.errors }, Cmd.none )

                        Err error ->
                            ( { model | errors = Decode.errorToString error :: model.errors }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Nip05FetchedForPubKey pubKey nip05 (Ok nip05Data) ->
            let
                maybePubKeyInNip05Data =
                    Dict.get nip05.user nip05Data.names

                nip05Relays =
                    Maybe.map2
                        (\pubKeyInNip05Data relaysDict ->
                            Dict.get pubKeyInNip05Data relaysDict
                                |> Maybe.withDefault []
                        )
                        maybePubKeyInNip05Data
                        nip05Data.relays
                        |> Maybe.withDefault []

                ( validationStatus, relays ) =
                    Dict.get nip05.user nip05Data.names
                        |> Maybe.map
                            (\pubKeyInNip05Data ->
                                if pubKeyInNip05Data == pubKey then
                                    ( ValidationSucceeded, nip05Relays )

                                else
                                    -- ignore relays if pubkey doesn't match in NIP-05
                                    ( ValidationNotMatchingPubKey, [] )
                            )
                        |> Maybe.withDefault ( ValidationNameMissing, [] )

                unknownRelays =
                    relays
                        |> List.map Nostr.Relay.hostWithoutProtocol
                        |> List.filter
                            (\relay ->
                                not <| Dict.member relay model.relays
                            )

                -- don't store relays here, only response for NIP-11 request
                requestNip11Cmd =
                    requestRelayNip11 unknownRelays
            in
            ( updateProfileWithValidationStatus model pubKey validationStatus
            , requestNip11Cmd
            )

        Nip05FetchedForPubKey pubKey _ (Err error) ->
            ( updateProfileWithValidationStatus model pubKey (ValidationNetworkError error), Cmd.none )

        Nip05FetchedForNip05 requestId nip05 (Ok nip05Data) ->
            updateModelWithNip05Data model requestId nip05 nip05Data

        --       Nip05FetchedForNip05 requestId nip05 (Err (Http.BadStatus 404)) ->
        --           ( model, fetchNip05InfoDirectly (Nip05FetchedForNip05 requestId nip05) nip05 )
        Nip05FetchedForNip05 _ nip05 (Err error) ->
            ( { model | errors = ("Error fetching NIP05 data for " ++ nip05ToString nip05 ++ ": " ++ httpErrorToString error) :: model.errors }, Cmd.none )

        Nip11Fetched urlWithoutProtocol (Ok info) ->
            let
                updatedRelay =
                    Dict.get urlWithoutProtocol model.relays
                        |> Maybe.map (\relay -> { relay | nip11 = Just info })
                        |> Maybe.withDefault
                            { urlWithoutProtocol = urlWithoutProtocol
                            , state = RelayStateUnknown
                            , nip11 = Just info
                            }
            in
            ( { model | relays = Dict.insert urlWithoutProtocol updatedRelay model.relays }, Cmd.none )

        Nip11Fetched urlWithoutProtocol (Err err) ->
            let
                updatedRelay =
                    Dict.get urlWithoutProtocol model.relays
                        |> Maybe.map (\relay -> { relay | state = RelayStateNip11RequestFailed err })
                        |> Maybe.withDefault
                            { urlWithoutProtocol = urlWithoutProtocol
                            , state = RelayStateNip11RequestFailed err
                            , nip11 = Nothing
                            }
            in
            ( { model
                | errors = ("Error fetching NIP11 data for " ++ urlWithoutProtocol ++ ": " ++ httpErrorToString err) :: model.errors
                , relays = Dict.insert urlWithoutProtocol updatedRelay model.relays
              }
            , Cmd.none
            )

        ReceivedNewsletterAuthorCheckResultPubKey pubKey (Ok newsletterCheckResponse) ->
            ( { model | sendsNewsletterPubKey = Dict.insert pubKey newsletterCheckResponse.email model.sendsNewsletterPubKey }
            , Cmd.none
            )

        ReceivedNewsletterAuthorCheckResultPubKey pubKey (Err error) ->
            ( { model | errors = ("Error fetching author check result for pubkey " ++ pubKey ++ ": " ++ httpErrorToString error) :: model.errors }
            , Cmd.none
            )

        ReceivedNewsletterAuthorCheckResultNip05 nip05 (Ok newsletterCheckResponse) ->
            ( { model | sendsNewsletterNip05 = Dict.insert (nip05ToString nip05) newsletterCheckResponse.email model.sendsNewsletterNip05 }
            , Cmd.none
            )

        ReceivedNewsletterAuthorCheckResultNip05 nip05 (Err error) ->
            ( { model | errors = ("Error fetching author check result for nip05 " ++ nip05ToString nip05 ++ ": " ++ httpErrorToString error) :: model.errors }
            , Cmd.none
            )


updateModelWithEvents : Model -> Int -> Kind -> List Event -> ( Model, Cmd Msg )
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

        KindEventDeletionRequest ->
            updateModelWithDeletionRequests model events

        KindUserServerList ->
            updateModelWithUserServerLists model requestId events

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

        KindReaction ->
            updateModelWithReactions model requestId events

        KindSearchRelaysList ->
            updateModelWithSearchRelays model requestId events

        KindShortTextNote ->
            updateModelWithShortTextNotes model requestId events

        KindUserMetadata ->
            updateModelWithUserMetadata model requestId events

        KindRelayListMetadata ->
            updateModelWithRelayListMetadata model events

        _ ->
            ( model, Cmd.none )


updateModelWithBookmarkLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithBookmarkLists model events =
    let
        -- usually there should be only one for the logged-in user
        bookmarkLists =
            events
                |> List.map bookmarkListFromEvent
                |> List.foldl
                    (\( pubKey, bookmarkList ) dict ->
                        Dict.insert pubKey bookmarkList dict
                    )
                    model.bookmarkLists
    in
    ( { model | bookmarkLists = bookmarkLists }, Cmd.none )


updateModelWithBookmarkSets : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithBookmarkSets model events =
    let
        -- usually there should be only one for the logged-in user
        bookmarkSets =
            events
                |> List.map bookmarkSetFromEvent
                |> List.foldl
                    (\( pubKey, bookmarkList ) dict ->
                        Dict.insert pubKey bookmarkList dict
                    )
                    model.bookmarkSets
    in
    ( { model | bookmarkSets = bookmarkSets }, Cmd.none )


updateModelWithCommunityDefinitions : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCommunityDefinitions model events =
    let
        communityDefinitions =
            events
                |> List.map communityDefinitionFromEvent
                |> List.foldl
                    (\communityDefinition dict ->
                        Dict.insert communityDefinition.pubKey [ communityDefinition ] dict
                    )
                    model.communities
    in
    ( { model | communities = communityDefinitions }, Cmd.none )


updateModelWithCommunityLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCommunityLists model events =
    let
        -- usually there should be only one for the logged-in user
        communityLists =
            events
                |> List.map communityListFromEvent
                |> List.foldl
                    (\( pubKey, communityList ) dict ->
                        Dict.insert pubKey communityList dict
                    )
                    model.communityLists
    in
    ( { model | communityLists = communityLists }, Cmd.none )


updateModelWithDeletionRequests : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithDeletionRequests model events =
    let
        ( deletedAddresses, deletedEventIds ) =
            events
                |> List.map deletionRequestFromEvent
                |> List.foldl
                    (\deletionRequest ( accAddresses, accEvents ) ->
                        ( Set.union accAddresses deletionRequest.addresses
                        , Set.union accEvents deletionRequest.eventIds
                        )
                    )
                    ( model.deletedAddresses, model.deletedEvents )
    in
    ( { model
        | deletedAddresses = deletedAddresses
        , deletedEvents = deletedEventIds
      }
    , Cmd.none
    )


updateModelWithUserServerLists : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithUserServerLists model _ events =
    let
        -- usually there should be only one for the logged-in user
        userServerLists =
            events
                |> List.map userServerListFromEvent
                |> List.foldl
                    (\( pubKey, userServerList ) dict ->
                        Dict.insert pubKey userServerList dict
                    )
                    model.userServerLists
    in
    ( { model | userServerLists = userServerLists }, Cmd.none )


updateModelWithFileStorageServerLists : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithFileStorageServerLists model _ events =
    let
        -- usually there should be only one for the logged-in user
        fileStorageServerLists =
            events
                |> List.map fileStorageServerListFromEvent
                |> List.foldl
                    (\( pubKey, fileStorageServerList ) dict ->
                        Dict.insert pubKey fileStorageServerList dict
                    )
                    model.fileStorageServerLists
    in
    ( { model | fileStorageServerLists = fileStorageServerLists }, Cmd.none )


updateModelWithLongFormContent : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithLongFormContent model requestId events =
    let
        ( articles, newErrors ) =
            events
                |> List.map articleFromEvent
                |> List.foldl
                    (\decodingResult ( articleAcc, errors ) ->
                        case decodingResult of
                            Ok article ->
                                ( article :: articleAcc, errors )

                            Err decodingErrors ->
                                ( articleAcc, decodingErrors ++ errors )
                    )
                    ( [], [] )

        -- sort articles, newest first
        articlesByDate =
            -- important that existing articles come first so if an article is edited it replaces the existing one
            model.articlesByDate
                ++ articles
                |> List.map
                    (\article ->
                        ( Maybe.withDefault "" (addressForArticle article), article )
                    )
                -- eliminate duplicates
                |> Dict.fromList
                |> Dict.values
                |> sortArticlesByDate

        articlesByAddress =
            articles
                |> List.foldl
                    (\article dict ->
                        case addressForArticle article of
                            Just address ->
                                Dict.insert address article dict

                            Nothing ->
                                dict
                    )
                    model.articlesByAddress

        articlesByAuthor =
            articles
                |> List.foldl
                    (\article dict ->
                        case Dict.get article.author dict of
                            Just articleList ->
                                Dict.insert article.author (appendArticleToList articleList article) dict

                            Nothing ->
                                Dict.insert article.author [ article ] dict
                    )
                    model.articlesByAuthor

        maybeRequest =
            Dict.get requestId model.requests

        ( requestModel, requestCmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForArticles model articles request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { requestModel
        | articlesByAddress = articlesByAddress
        , articlesByAuthor = articlesByAuthor
        , articlesByDate = articlesByDate
        , errors = newErrors ++ model.errors
      }
    , requestCmd
    )


getErrorMessages : Model -> List String
getErrorMessages model =
    model.errors


sortArticlesByDate : List Article -> List Article
sortArticlesByDate articles =
    articles
        |> List.sortBy
            (\article ->
                publishedTime article.createdAt article.publishedAt
                    |> Time.posixToMillis
                    |> (*) -1
            )


appendArticleToList : List Article -> Article -> List Article
appendArticleToList articleList article =
    let
        addressComponents =
            addressComponentsForArticle article

        articleIsInList =
            articleList
                |> List.filter
                    (\articleInList ->
                        addressComponents == addressComponentsForArticle articleInList
                    )
                |> List.isEmpty
                |> not
    in
    if articleIsInList then
        articleList

    else
        articleList ++ [ article ]


updateModelWithLongFormContentDraft : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithLongFormContentDraft model requestId events =
    let
        ( articles, newErrors ) =
            events
                |> List.map articleFromEvent
                |> List.foldl
                    (\decodingResult ( articleAcc, errors ) ->
                        case decodingResult of
                            Ok article ->
                                ( article :: articleAcc, errors )

                            Err decodingErrors ->
                                ( articleAcc, decodingErrors ++ errors )
                    )
                    ( [], [] )

        -- collect relays we read them from so we can delete drafts effectively and efficiently
        articleDraftRelays =
            articles
                |> List.foldl
                    (\article acc ->
                        case ( article.relays, Dict.get article.id acc ) of
                            ( relayUrls, Just relaySet ) ->
                                Dict.insert article.id (Set.union relayUrls relaySet) acc

                            ( relayUrls, Nothing ) ->
                                if not (Set.isEmpty relayUrls) then
                                    Dict.insert article.id relayUrls acc

                                else
                                    acc
                    )
                    model.articleDraftRelays

        -- sort articles, newest first
        articleDraftsByDate =
            -- important that existing drafts come first so if a draft is saved it replaces the existing one
            model.articleDraftsByDate
                ++ articles
                |> List.map
                    (\article ->
                        ( Maybe.withDefault "" article.identifier, article )
                    )
                |> Dict.fromList
                |> Dict.toList
                |> List.map
                    (\( _, article ) ->
                        article
                    )
                |> List.sortBy
                    (\article ->
                        article.publishedAt
                            |> Maybe.map (\publishedAt -> Time.posixToMillis publishedAt * -1)
                            |> Maybe.withDefault 0
                    )

        maybeRequest =
            Dict.get requestId model.requests

        ( requestModel, requestCmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForArticles model articles request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { requestModel
        | articleDraftsByDate = articleDraftsByDate
        , articleDraftRelays = articleDraftRelays
        , errors = newErrors ++ model.errors
      }
    , requestCmd
    )


requestRelatedKindsForArticles : Model -> List Article -> Request -> ( Model, Cmd Msg )
requestRelatedKindsForArticles model articles request =
    let
        ( requestNip27Model, requestWithNip27Requests ) =
            articles
                |> List.map .nip27References
                |> List.concat
                |> appendNip27ProfileRequests model request

        maybeEventFilterForAuthorProfiles =
            articles
                |> Nostr.Article.uniqueArticleAuthors
                |> getMissingProfilePubKeys requestNip27Model
                |> eventFilterForAuthors

        ( requestProfileModel, extendedRequestProfile ) =
            case maybeEventFilterForAuthorProfiles of
                Just eventFilterForAuthorProfiles ->
                    -- TODO: add relays for request
                    eventFilterForAuthorProfiles
                        |> RequestProfile Nothing
                        |> addToRequest model requestWithNip27Requests

                Nothing ->
                    ( model, request )

        ( extendedModel, extendedRequestReactions ) =
            articles
                |> List.map Nostr.Article.tagReference
                |> eventFilterForReactions
                |> Maybe.map RequestReactions
                |> Maybe.map (addToRequest requestProfileModel extendedRequestProfile)
                |> Maybe.withDefault ( requestProfileModel, extendedRequestProfile )

        ( modelWithDeletionRequests, extendedRequestDeletionRequests ) =
            articles
                |> List.filterMap Nostr.Article.addressComponentsForArticle
                |> List.map Nostr.Event.TagReferenceCode
                |> eventFilterForDeletionRequests
                |> Maybe.map RequestDeletionRequests
                |> Maybe.map (addToRequest extendedModel extendedRequestReactions)
                |> Maybe.withDefault ( extendedModel, extendedRequestReactions )
    in
    doRequest modelWithDeletionRequests extendedRequestDeletionRequests


appendNip27ProfileRequests : Model -> Request -> List NIP19Type -> ( Model, Request )
appendNip27ProfileRequests model request nip19List =
    case nip27ProfilesRequest model nip19List of
        Just eventFilterForProfiles ->
            -- TODO: add relays for request
            eventFilterForProfiles
                |> RequestProfile Nothing
                |> addToRequest model request

        Nothing ->
            ( model, request )


nip27ProfilesRequest : Model -> List NIP19Type -> Maybe EventFilter
nip27ProfilesRequest model nip19List =
    let
        pubKeys =
            nip19List
                |> List.filterMap
                    (\nip27Ref ->
                        case nip27Ref of
                            Npub pubKey ->
                                Just pubKey

                            Nsec _ ->
                                Nothing

                            Note _ ->
                                Nothing

                            NProfile { pubKey } ->
                                Just pubKey

                            NEvent _ ->
                                Nothing

                            NAddr _ ->
                                Nothing

                            NRelay _ ->
                                Nothing

                            Unknown _ ->
                                Nothing
                    )
                |> Set.fromList
                |> Set.toList
    in
    pubKeys
        |> getMissingProfilePubKeys model
        |> eventFilterForAuthors


updateModelWithSearchRelays : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithSearchRelays model _ events =
    let
        -- usually there should be only one for the logged-in user
        searchRelaysLists =
            events
                |> List.map relayListFromEvent

        relayListDict =
            searchRelaysLists
                |> List.foldl
                    (\( pubKey, relayList ) dict ->
                        Dict.insert pubKey (relayUrlListWithUniqueEntries relayList) dict
                    )
                    model.searchRelayLists

        unknownRelays =
            searchRelaysLists
                |> List.map (\( _, relayMetadataList ) -> relayMetadataList)
                |> List.concat
                |> List.map (\url -> Nostr.Relay.hostWithoutProtocol url)
                |> List.filter
                    (\relay ->
                        not <| Dict.member relay model.relays
                    )

        requestNip11Cmd =
            requestRelayNip11 unknownRelays
    in
    ( { model | searchRelayLists = relayListDict }, requestNip11Cmd )


updateModelWithReactions : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithReactions model _ events =
    let
        reactions =
            events
                |> List.map reactionFromEvent

        reactionsForEventId =
            reactions
                |> List.foldl
                    (\reaction acc ->
                        case reaction.noteIdReactedTo of
                            Just noteId ->
                                case Dict.get noteId acc of
                                    Just dict ->
                                        Dict.insert noteId (Dict.insert reaction.pubKey reaction dict) acc

                                    Nothing ->
                                        Dict.insert noteId (Dict.singleton reaction.pubKey reaction) acc

                            _ ->
                                acc
                    )
                    model.reactionsForEventId

        reactionsForAddress =
            reactions
                |> List.foldl
                    (\reaction acc ->
                        case reaction.addressComponentsReactedTo of
                            Just addressComponents ->
                                let
                                    address =
                                        buildAddress addressComponents
                                in
                                case Dict.get address acc of
                                    Just dict ->
                                        Dict.insert address (Dict.insert reaction.pubKey reaction dict) acc

                                    Nothing ->
                                        Dict.insert address (Dict.singleton reaction.pubKey reaction) acc

                            _ ->
                                acc
                    )
                    model.reactionsForAddress
    in
    ( { model | reactionsForEventId = reactionsForEventId, reactionsForAddress = reactionsForAddress }, Cmd.none )


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


updateModelWithShortTextNotes : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithShortTextNotes model requestId events =
    let
        shortTextNotes =
            events
                |> List.map shortNoteFromEvent

        shortTextNotesDict =
            shortTextNotes
                |> List.foldl
                    (\shortNote acc ->
                        Dict.insert shortNote.id shortNote acc
                    )
                    model.shortTextNotes

        maybeRequest =
            Dict.get requestId model.requests

        ( requestModel, requestCmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForShortNotes model shortTextNotes request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { requestModel | shortTextNotes = shortTextNotesDict }, requestCmd )


requestRelatedKindsForShortNotes : Model -> List ShortNote -> Request -> ( Model, Cmd Msg )
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
                |> eventFilterForAuthors

        ( requestProfileModel, extendedRequestProfile ) =
            case maybeEventFilterForAuthorProfiles of
                Just eventFilterForAuthorProfiles ->
                    -- TODO: add relays for request
                    eventFilterForAuthorProfiles
                        |> RequestProfile Nothing
                        |> addToRequest model request

                Nothing ->
                    ( model, request )

        ( extendedModel, extendedRequestReactions ) =
            shortNotes
                |> List.map Nostr.ShortNote.tagReference
                |> eventFilterForReactions
                |> Maybe.map RequestReactions
                |> Maybe.map (addToRequest requestProfileModel extendedRequestProfile)
                |> Maybe.withDefault ( requestProfileModel, extendedRequestProfile )
    in
    doRequest extendedModel extendedRequestReactions


updateModelWithUserMetadata : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithUserMetadata model requestId events =
    let
        profiles =
            events
                |> List.filterMap profileFromEvent

        profilesSum =
            profiles
                |> List.foldl
                    (\profile dict ->
                        Dict.insert profile.pubKey profile dict
                    )
                    model.profiles

        nip05Requests =
            profiles
                |> List.filterMap
                    (\profile ->
                        Maybe.map (\nip05 -> fetchNip05Info (Nip05FetchedForPubKey profile.pubKey nip05) nip05) profile.nip05
                    )

        relatedKinds =
            Dict.get requestId model.requests
                |> relatedKindsForRequest

        ( requestModel, relatedRequestCmd ) =
            requestRelatedKindsForProfiles model profiles relatedKinds

        requests =
            relatedRequestCmd
                :: nip05Requests
                |> Cmd.batch
    in
    ( { requestModel | profiles = profilesSum }, requests )


requestRelatedKindsForProfiles : Model -> List Profile -> List Kind -> ( Model, Cmd Msg )
requestRelatedKindsForProfiles model profiles kinds =
    if List.member KindLongFormContent kinds then
        profiles
            |> List.map .pubKey
            |> requestArticlesForAuthors model

    else
        ( model, Cmd.none )


requestArticlesForAuthors : Model -> List PubKey -> ( Model, Cmd Msg )
requestArticlesForAuthors model pubKeys =
    createRequest model "Articles for authors" [] (RequestArticlesFeed [ { emptyEventFilter | authors = Just pubKeys, kinds = Just [ KindLongFormContent ] } ])
        |> doRequest model


updateModelWithRelayListMetadata : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithRelayListMetadata model events =
    let
        -- usually there should be only one for the logged-in user
        relayLists =
            events
                |> List.map relayMetadataListFromEvent

        relayListDict =
            relayLists
                |> List.foldl
                    (\( pubKey, relayList ) dict ->
                        Dict.insert pubKey (relayMetadataListWithUniqueEntries relayList) dict
                    )
                    model.relayMetadataLists

        unknownRelays =
            relayLists
                |> List.map (\( _, relayMetadataList ) -> relayMetadataList)
                |> List.concat
                |> List.map (\{ url } -> Nostr.Relay.hostWithoutProtocol url)
                |> List.filter
                    (\relay ->
                        not <| Dict.member relay model.relays
                    )

        -- insert dummy entries in relays dict
        -- should be updated with NIP-11 data
        relays =
            unknownRelays
                |> List.foldl
                    (\unknownRelay acc ->
                        Dict.insert unknownRelay { nip11 = Nothing, state = RelayStateUnknown, urlWithoutProtocol = unknownRelay } acc
                    )
                    model.relays

        requestNip11Cmd =
            requestRelayNip11 unknownRelays
    in
    ( { model | relayMetadataLists = relayListDict, relays = relays }, requestNip11Cmd )


relayUrlListWithUniqueEntries : List RelayUrl -> List RelayUrl
relayUrlListWithUniqueEntries relayList =
    relayList
        |> Set.fromList
        |> Set.toList


relayMetadataListWithUniqueEntries : List RelayMetadata -> List RelayMetadata
relayMetadataListWithUniqueEntries relayList =
    relayList
        |> List.map (\relayMetadata -> ( relayMetadata.url, relayMetadata.role ))
        |> Dict.fromList
        |> Dict.toList
        |> List.map (\( url, role ) -> { url = Nostr.Relay.hostWithoutProtocol url, role = role })


updateModelWithFollowLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithFollowLists model events =
    let
        followLists =
            events
                |> List.map followListFromEvent
                |> List.foldl
                    (\{ pubKey, following } dict ->
                        Dict.insert pubKey following dict
                    )
                    model.followLists
    in
    ( { model | followLists = followLists }, Cmd.none )


updateModelWithFollowSets : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithFollowSets model events =
    let
        followSets =
            events
                |> List.filterMap followSetFromEvent
                |> List.foldl
                    (\( pubKey, followSet ) dict ->
                        case Dict.get pubKey dict of
                            Just followSetDict ->
                                Dict.insert pubKey (Dict.insert followSet.identifier followSet followSetDict) dict

                            Nothing ->
                                Dict.insert pubKey (Dict.singleton followSet.identifier followSet) dict
                    )
                    model.followSets
    in
    ( { model | followSets = followSets }, Cmd.none )


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


updateModelWithNip05Data : Model -> RequestId -> Nip05 -> Nip05.Nip05Data -> ( Model, Cmd Msg )
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
            nip05Data.relays
                |> Maybe.andThen
                    (\relayDict ->
                        maybePubKey
                            |> Maybe.andThen (\pubKey -> Dict.get pubKey relayDict)
                    )

        ( requestModel, requestProfileCmd ) =
            case ( loadedProfile, maybeRequest, maybePubKey ) of
                ( Nothing, Just request, Just pubKey ) ->
                    { emptyEventFilter | authors = Just [ pubKey ], kinds = Just [ KindUserMetadata ] }
                        |> RequestProfile maybeRelays
                        |> addToRequest modelWithValidatedNip05 request
                        |> (\( modelWithRequest, extendedRequest ) -> doRequest modelWithRequest extendedRequest)

                ( _, _, _ ) ->
                    ( modelWithValidatedNip05, Cmd.none )
    in
    ( requestModel, requestProfileCmd )


validateNip05 : Model -> Nip05 -> Nip05.Nip05Data -> Model
validateNip05 model nip05 nip05Data =
    let
        pubKeyInNip05Data =
            Dict.get nip05.user nip05Data.names

        loadedProfile =
            pubKeyInNip05Data
                |> Maybe.andThen (getProfile model)

        ( pubKeyForUpdate, validationStatus ) =
            case ( pubKeyInNip05Data, loadedProfile ) of
                ( Just pubKey, Just profile ) ->
                    -- profile is already loaded - update status for pubKey in profile
                    if pubKey == profile.pubKey then
                        ( Just profile.pubKey, ValidationSucceeded )

                    else
                        ( Just profile.pubKey, ValidationNotMatchingPubKey )

                ( Just _, Nothing ) ->
                    -- profile not yet loaded - load for pubKey in NIP-05 data
                    ( pubKeyInNip05Data, ValidationPending )

                ( Nothing, _ ) ->
                    -- name missing in response
                    ( Nothing, ValidationNameMissing )
    in
    case pubKeyForUpdate of
        Just pubKey ->
            updateProfileWithValidationStatus model pubKey validationStatus

        Nothing ->
            model


updateProfileWithValidationStatus : Model -> PubKey -> ProfileValidation -> Model
updateProfileWithValidationStatus model pubKey valid =
    let
        maybeProfile =
            Dict.get pubKey model.profiles

        updatedNip05Dict =
            maybeProfile
                |> Maybe.andThen .nip05
                |> Maybe.map
                    (\nip05 ->
                        Dict.insert (nip05ToString nip05) pubKey model.pubKeyByNip05
                    )
                |> Maybe.withDefault model.pubKeyByNip05
    in
    { model | profileValidations = Dict.insert pubKey valid model.profileValidations, pubKeyByNip05 = updatedNip05Dict }


updateWithPubkeyProfiles : Model -> List Nostr.Profile.PubkeyProfile -> ( Model, Cmd Msg )
updateWithPubkeyProfiles model pubkeyProfiles =
    let
        nip05Requests =
            pubkeyProfiles
                |> List.filterMap
                    (\{ pubKey, profile } ->
                        Maybe.map (\nip05 -> fetchNip05Info (Nip05FetchedForPubKey pubKey nip05) nip05) profile.nip05
                    )
                |> Cmd.batch

        profilesSum =
            pubkeyProfiles
                |> List.foldl
                    (\{ pubKey, profile } ->
                        Dict.insert pubKey profile
                    )
                    model.profiles
    in
    ( { model | profiles = profilesSum }, nip05Requests )


updateWithZapReceipts : Model -> List Nostr.Zaps.ZapReceipt -> ( Model, Cmd Msg )
updateWithZapReceipts model zapReceipts =
    let
        zapReceiptsForAddresses =
            zapReceipts
                |> List.filterMap
                    (\receipt ->
                        receipt.address
                            |> Maybe.andThen
                                (\address ->
                                    Just ( address, receipt )
                                )
                    )
                |> List.foldl addToZapReceiptDict model.zapReceiptsAddress

        zapReceiptsForEvents =
            zapReceipts
                |> List.filterMap
                    (\receipt ->
                        receipt.event
                            |> Maybe.andThen
                                (\event ->
                                    Just ( event, receipt )
                                )
                    )
                |> List.foldl addToZapReceiptDict model.zapReceiptsEvents
    in
    ( { model | zapReceiptsAddress = zapReceiptsForAddresses, zapReceiptsEvents = zapReceiptsForEvents }, Cmd.none )


addToZapReceiptDict : ( String, Nostr.Zaps.ZapReceipt ) -> Dict String (Dict String Nostr.Zaps.ZapReceipt) -> Dict String (Dict String Nostr.Zaps.ZapReceipt)
addToZapReceiptDict ( address, receipt ) receiptDict =
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
