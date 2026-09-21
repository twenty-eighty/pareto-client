module Nostr exposing
    ( Model
    , Msg
    , TestMode
    , empty
    , init
    , requestRelayNip11
    , getArticleQueryStatus
    , articleQueryStatusFrom
    , trackContentRequest
    , settleContentRequest
    , failContentRequest
    , getAuthorsFollowList
    , getAuthorsMuteList
    , isMuted
    , getAuthorsPubKeys
    , isAuthor
    , isEditor
    , isBetaTester
    , loadUserDataByPubKey
    , loadUserDataByNip05
    , getPortalUserInfo
    , sendsNewsletterPubKey
    , sendsNewsletterNip05
    , createRequest
    , requestArticleDetails
    , articleNeedsDetails
    , shouldRequestArticleDetails
    , requestDataOfState
    , markArticleDetailsRequested
    , markAddressesRequested
    , addToRequest
    , extendRequestWith
    , configuredRelaysWss
    , doRequest
    , doRequestWithId
    , performRequest
    , applyPerformEffect
    , eventFiltersWithUntil
    , send
    , queueSend
    , sendEvent
    , sendEventWithId
    , getAuthor
    , getPicturePosts
    , getPicturePostById
    , getProfileValidationStatus
    , getArticle
    , getArticlesByDate
    , resetArticles
    , getArticleDraftsByDate
    , getArticleDraftWithIdentifier
    , getArticleDraftWithId
    , getArticlesForAuthor
    , filterDeletedArticle
    , getArticleForAddressComponents
    , getArticleForNip19
    , getArticleWithIdentifier
    , getArticleByNip05AndIdentifier
    , getArticleWithId
    , getBlossomServers
    , getDefaultNip96Servers
    , getDefaultBlossomServers
    , getNip96Servers
    , getLastRequestId
    , getLastSendRequestId
    , filterArticlesWithIdentifier
    , getCommunityForNip19
    , filterCommunitiesWithIdentifier
    , getFollowsList
    , getMuteList
    , getArticleComments
    , getTextNoteCommentsForArticle
    , getArticleCommentComments
    , getBookmarks
    , getReactionsForArticle
    , getReactionsForEventId
    , getRepostsForArticle
    , getRepostsForEventId
    , getRelaysForPubKey
    , getRelayListForPubKey
    , getNip65RelaysForPubKey
    , getNip65ReadRelaysForPubKey
    , getNip65WriteRelaysForPubKey
    , getReadRelaysForPubKey
    , getReadRelayUrlsForPubKey
    , getWriteRelaysForPubKey
    , getWriteRelayUrlsForPubKey
    , getDraftRelayUrls
    , getDraftStorageRelayUrls
    , getLocalRelayUrls
    , getPrivateRelayUrls
    , getBlockedRelayUrls
    , getUserBlockedRelayUrls
    , isBlockedRelay
    , getSearchRelayUrls
    , getSearchRelaysForPubKey
    , relaysWithSearchCapability
    , getRelaysForRequest
    , getDefaultRelays
    , getApplicationDataRelays
    , getRelayData
    , getRequest
    , getShortNoteById
    , setLocalRelays
    , getShortNotes
    , getZapReceiptsForArticle
    , getZapReceiptsForTagReference
    , getZapReceiptsForEventId
    , hasZapReceiptWithBolt11
    , zapReceiptIdsForTagReference
    , getProfile
    , getProfileByNip05
    , nip05LookupKey
    , nip05sEqual
    , getPubKeyByNip05
    , bootstrapPubKeyByNip05
    , insertPubKeyByNip05
    , pubKeyFromNip05Names
    , requestCommunityPostApprovals
    , eventFilterForCommunityPostApprovals
    , requestUserData
    , getMissingProfilePubKeys
    , eventFilterForAuthors
    , getCommunityList
    , isArticleBookmarked
    , areAddressComponentsBookmarked
    , isEventIdBookmarked
    , getZapReceiptsCountForTagReference
    , getZapReceiptsCountForArticle
    , getZapReceiptsCountForComment
    , getNutzapsCountForTagReference
    , getCashuWallet
    , getCashuBalance
    , addCashuBalance
    , clearCashuState
    , clearUserSessionState
    , getCashuProofsForMint
    , getNutzapMintRecommendation
    , getNutzapMintRecommendationFor
    , addZapAmount
    , getBookmarkListCountForAddressComponents
    , getBookmarkListCountForEventId
    , getReactionsCountForArticle
    , getReactionsCountForAddressComponents
    , getReactionsCountForEventId
    , getRepostsCountForArticle
    , getRepostsCountForAddressComponents
    , getRepostsCountForEventId
    , getReactionForArticle
    , getReactionForEventId
    , getHighlightsForAddress
    , getHighlightsCountForAddress
    , highlightsForPubKey
    , notificationsForPubKey
    , unreadNotificationsCount
    , eventFilterForDeletionRequests
    , eventFilterForReactions
    , articleFromList
    , cmdBatch2
    , paretoAuthorsFollowList
    , paretoKnownPubKey
    , update
    , applyIncoming
    , updateModelWithEvents
    , updateModelWithBookmarkLists
    , updateModelWithBookmarkSets
    , updateModelWithCommunityDefinitions
    , updateModelWithCommunityLists
    , updateModelWithDeletionRequests
    , updateModelWithReposts
    , updateModelWithComments
    , updateModelWithPictures
    , uniquePubKeys
    , requestRelatedKindsForArticleComments
    , updateModelWithUserServerLists
    , updateModelWithFileStorageServerLists
    , updateModelWithLongFormContent
    , getErrorMessages
    , sortArticlesByDate
    , updateModelWithLongFormContentDraft
    , requestRelatedKindsForArticles
    , requestArticleDetailsBatch
    , appendNip27ProfileRequests
    , nip27ProfilesRequest
    , updateModelWithSearchRelays
    , updateModelWithBlockedRelays
    , updateModelWithPrivateRelays
    , updateModelWithHighlights
    , updateModelWithReactions
    , updateModelWithShortTextNotes
    , requestRelatedKindsForShortNotes
    , requestRelatedProfiles
    , requestRelatedReactions
    , updateModelWithUserMetadata
    , requestRelatedKindsForProfiles
    , requestArticlesForAuthors
    , updateModelWithRelayListMetadata
    , updateModelWithFollowLists
    , updateModelWithMuteLists
    , updateModelWithFollowSets
    , insertIntoEventsDict
    , requestNip05Info
    , cacheEntryIsFresh
    , addNip05Waiter
    , markNip05TargetPending
    , checkNip05Cache
    , startNip05Request
    , updateWithNip05Result
    , handleNip05Result
    , profileUsesNip05
    , updateProfileWithNip05Data
    , updateModelWithNip05Data
    , identifierFromNip05ArticleRequest
    , validateNip05
    , updateProfileWithValidationStatus
    , updateWithPubkeyProfiles
    , updateWithZapReceipts
    , subscriptions
    )



import BrowserEnv exposing (Environment(..))
import Nostr.Model as Store exposing (Msg(..), TestMode(..))
import Dict exposing (Dict)
import Http
import Nostr.Article exposing (Article, addressComponentsForArticle, addressForArticle, filterMatchesArticle)
import Nostr.Articles as Articles
import Nostr.Blossom as Blossom
import Nostr.EventFilters as EventFilters
import Nostr.Nip05Cache as Nip05Cache exposing (CheckDecision(..), ContentRequestDecision(..), FetchDecision(..), Nip05CacheEntry(..), Nip05RequestTarget(..))
import Nostr.Nip05Apply as Nip05Apply
import Nostr.Notifications as Notifications
import Nostr.Highlights as Highlights
import Nostr.RelayAccess as RelayAccess
import Nostr.PerformRequest as PerformRequest
import Nostr.RelatedRequests as RelatedRequests
import Nostr.BookmarkList as BookmarkList exposing (BookmarkList, emptyBookmarkList)
import Nostr.BookmarkSet as BookmarkSet exposing (BookmarkSet)
import Nostr.CashuWallet as CashuWallet exposing (CashuWallet)
import Nostr.Community as Community exposing (Community)
import Nostr.CommunityList as CommunityList exposing (CommunityReference)
import Nostr.Comments as Comments
import Nostr.CommentsQuery as CommentsQuery
import Nostr.ContentRequest as ContentRequest exposing (ContentRequestState(..))
import Nostr.DeletionRequests as DeletionRequests
import Nostr.Event exposing (AddressComponents, Event, EventFilter, Kind(..), TagReference(..), buildAddress, emptyEventFilter, kindFromNumber, numberForKind, tagReferenceToString)
import Nostr.External exposing (Hooks)
import Nostr.Incoming as Incoming
import Nostr.FileStorageServerList as FileStorageServerList
import Nostr.FollowList as FollowList exposing (pubKeyIsFollower)
import Nostr.FollowSet as FollowSet exposing (FollowSet)
import Nostr.Nip05 as Nip05 exposing (Nip05, Nip05String, fetchNip05Info, nip05ToString)
import Nostr.Nip10 exposing (TextNote, tagReference)
import Nostr.Nip18 exposing (Repost)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (ArticleComment, ArticleCommentComment, CommentType(..))
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Nutzaps as Nutzaps exposing (Nutzap)
import Nostr.PicturePosts as PicturePosts
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Profiles as Profiles
import Ports
import Nostr.Query as Query exposing (ContentQueryStatus(..))
import Nostr.Reactions exposing (Reaction)
import Nostr.ReactionsStore as ReactionsStore
import Nostr.Relay as Relay exposing (Relay, RelayState(..), RelayUrl)
import Nostr.RelayList as RelayList
import Nostr.RelayListMetadata as RelayListMetadata exposing (RelayMetadata)
import Nostr.Reposts as Reposts
import Nostr.Request as Request exposing (Request, RequestData(..), RequestId, RequestState(..), relatedKindsForRequest)
import Nostr.Send as Send exposing (SendRequest(..), SendRequestId)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.ShortNotes as ShortNotes
import Nostr.Types exposing (Address, EventId, Following(..), IncomingMessage, PubKey, RelayRole(..))
import Nostr.Zaps as Zaps exposing (ZapReceipt)
import Nostr.ZapsQuery as ZapsQuery
import Pareto
import Portal
import Set exposing (Set)
import Task
import Time exposing (Posix)




-- Re-exports from Nostr.Model (Elm cannot expose bare imports)
type alias Model =
    Store.Model


type alias Msg =
    Store.Msg


type alias TestMode =
    Store.TestMode


empty : Model
empty =
    Store.empty


init : Hooks Msg -> Environment -> TestMode -> List RelayUrl -> List RelayUrl -> ( Model, Cmd Msg )
init =
    Store.init


setLocalRelays : Model -> List RelayUrl -> ( Model, Cmd Msg )
setLocalRelays model localRelays =
    let
        unique =
            RelayList.withUniqueEntries localRelays

        newRelays =
            unique
                |> List.foldl
                    (\url relays ->
                        case Dict.get (Relay.toKey url) relays of
                            Just _ ->
                                relays

                            Nothing ->
                                Dict.insert (Relay.toKey url)
                                    { url = url
                                    , state = Relay.RelayStateUnknown
                                    , nip11 = Nothing
                                    }
                                    relays
                    )
                    model.relays

        updated =
            { model | localRelays = unique, relays = newRelays }
    in
    ( updated
    , Cmd.batch
        [ model.hooks.connect unique
        , requestRelayNip11 updated unique
        ]
    )


requestRelayNip11 : Model -> List RelayUrl -> Cmd Msg
requestRelayNip11 =
    Store.requestRelayNip11


getArticleQueryStatus : Model -> Nip05 -> String -> Maybe RequestId -> ContentQueryStatus Article
getArticleQueryStatus model nip05 identifier maybeRequestId =
    articleQueryStatusFrom model
        (getArticleByNip05AndIdentifier model nip05 identifier)
        maybeRequestId


{-| Resolve status from a cached article (if any) and an optional in-flight request.
-}
articleQueryStatusFrom : Model -> Maybe Article -> Maybe RequestId -> ContentQueryStatus Article
articleQueryStatusFrom model maybeArticle maybeRequestId =
    Query.statusFrom model.contentRequestStates maybeArticle maybeRequestId


trackContentRequest : Model -> RequestId -> ContentRequestState -> Model
trackContentRequest model requestId state =
    { model | contentRequestStates = ContentRequest.track requestId state model.contentRequestStates }


settleContentRequest : Model -> RequestId -> Model
settleContentRequest model requestId =
    { model | contentRequestStates = ContentRequest.settle requestId model.contentRequestStates }


failContentRequest : Model -> RequestId -> String -> Model
failContentRequest model requestId reason =
    { model | contentRequestStates = ContentRequest.fail requestId reason model.contentRequestStates }


getAuthorsFollowList : Model -> List Following
getAuthorsFollowList model =
    getFollowsList model Pareto.authorsKey
        |> Maybe.withDefault paretoAuthorsFollowList


getAuthorsMuteList : Model -> List Following
getAuthorsMuteList model =
    getMuteList model Pareto.authorsKey
        |> Maybe.withDefault []

isMuted : Model -> Maybe PubKey -> PubKey -> Bool
isMuted model maybeUserPubKey authorPubKey =
    let
        mutedByUser =
            maybeUserPubKey
                |> Maybe.andThen (getMuteList model)
                |> Maybe.map (pubKeyIsFollower authorPubKey)
                |> Maybe.withDefault False

        mutedByAuthor =
            getMuteList model Pareto.authorsKey
                |> Maybe.map (pubKeyIsFollower authorPubKey)
                |> Maybe.withDefault False
    in
    mutedByUser || mutedByAuthor



getAuthorsPubKeys : Model -> List PubKey
getAuthorsPubKeys model =
    getAuthorsFollowList model
        |> List.filterMap
            (\following ->
                case following of
                    FollowingPubKey { pubKey } ->
                        Just pubKey

                    _ ->
                        Nothing
            )


isAuthor : Model -> PubKey -> Bool
isAuthor model userPubKey =
    getAuthorsFollowList model
        |> pubKeyIsFollower userPubKey


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



-- user data from portal server


loadUserDataByPubKey : Model -> PubKey -> Cmd Msg
loadUserDataByPubKey model pubKey =
    case sendsNewsletterPubKey model pubKey of
        Just _ ->
            Cmd.none

        Nothing ->
            Portal.loadUserDataByPubKey ReceivedPortalCheckResultPubKey pubKey


loadUserDataByNip05 : Model -> Nip05 -> Cmd Msg
loadUserDataByNip05 model nip05 =
    case sendsNewsletterNip05 model nip05 of
        Just _ ->
            Cmd.none

        Nothing ->
            Portal.loadUserDataByNip05 ReceivedPortalCheckResultNip05 nip05


getPortalUserInfo : Model -> PubKey -> Maybe Portal.PortalCheckResponse
getPortalUserInfo model pubKey =
    Dict.get pubKey model.portalUserInfoPubKey


sendsNewsletterPubKey : Model -> PubKey -> Maybe Bool
sendsNewsletterPubKey model pubKey =
    getPortalUserInfo model pubKey
        |> Maybe.map .email


sendsNewsletterNip05 : Model -> Nip05 -> Maybe Bool
sendsNewsletterNip05 model nip05 =
    Portal.sendsNewsletter
        (getPubKeyByNip05 model nip05
            |> Maybe.andThen (sendsNewsletterPubKey model)
        )
        (Dict.get (nip05ToString nip05) model.portalUserInfoNip05
            |> Maybe.map .email
        )



-- the request ID will be incremented only in request when sending


createRequest : Model -> String -> List Kind -> RequestData -> Request
createRequest model description relatedKinds data =
    { id = model.lastRequestId
    , relatedKinds = relatedKinds
    , states = [ RequestCreated data ]
    , description = description
    }


{-| Comments, reactions, zaps, and NIP-27 profiles for a single article.
Used when opening an article that was already loaded as a list preview.
-}
requestArticleDetails : Model -> Article -> Maybe Request
requestArticleDetails model article =
    if not (articleNeedsDetails model article) then
        Nothing

    else
        let
            reactionRequest =
                [ Nostr.Article.tagReference article ]
                    |> EventFilters.forReactions
                    |> Maybe.map RequestReactions

            baseRequest =
                { id = model.lastRequestId
                , relatedKinds = []
                , states = List.filterMap (Maybe.map RequestCreated) [ reactionRequest ]
                , description = "Article details"
                }

            ( _, requestWithNip27 ) =
                appendNip27ProfileRequests model baseRequest article.nip27References
        in
        if List.isEmpty requestWithNip27.states then
            Nothing

        else
            Just requestWithNip27


articleNeedsDetails : Model -> Article -> Bool
articleNeedsDetails model article =
    case addressForArticle article of
        Just address ->
            not (Set.member address model.articleDetailsRequested)

        Nothing ->
            True


shouldRequestArticleDetails : Request -> Bool
shouldRequestArticleDetails =
    Request.shouldRequestArticleDetails


requestDataOfState : RequestState -> RequestData
requestDataOfState =
    Request.requestDataOfState


markArticleDetailsRequested : Model -> EventFilter -> Model
markArticleDetailsRequested model eventFilter =
    markAddressesRequested model (PerformRequest.addressesFromReactionFilter eventFilter)


markAddressesRequested : Model -> Set.Set Address -> Model
markAddressesRequested model addresses =
    if Set.isEmpty addresses then
        model

    else
        { model | articleDetailsRequested = Set.union model.articleDetailsRequested addresses }


addToRequest : Model -> Request -> RequestData -> ( Model, Request )
addToRequest model request data =
    let
        extendedRequest =
            { request | states = request.states ++ [ RequestCreated data ] }
    in
    ( { model | requests = Dict.insert request.id extendedRequest model.requests }, extendedRequest )


extendRequestWith : List RequestData -> ( Model, Request ) -> ( Model, Request )
extendRequestWith requestDatas ( model, request ) =
    List.foldl
        (\data ( modelAcc, requestAcc ) ->
            addToRequest modelAcc requestAcc data
        )
        ( model, request )
        requestDatas


configuredRelaysWss : Model -> List RelayUrl
configuredRelaysWss model =
    case model.defaultUser of
        Just pubKey ->
            getReadRelayUrlsForPubKey model pubKey

        Nothing ->
            getDefaultRelays model


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
        result =
            PerformRequest.perform
                { hooks = model.hooks
                , configuredRelays = configuredRelaysWss model
                , applicationDataRelays = getApplicationDataRelays model
                , searchRelayUrls = getSearchRelayUrls model model.defaultUser
                , draftStorageRelays =
                    getDraftStorageRelayUrls model (Maybe.withDefault "" model.defaultUser)
                , delayedPublishingRelays = Pareto.delayedPublishingRelays
                , blockedRelays = blockedRelayUrls model
                , articlesByDate = model.articlesByDate
                , requestNip05 = \reqId nip05 -> requestNip05Info (Nip05ForRequest reqId) nip05
                }
                description
                requestId
                requestData
    in
    ( applyPerformEffect result.modelEffect model requestId, result.cmd )


applyPerformEffect : PerformRequest.ModelEffect -> Model -> RequestId -> Model
applyPerformEffect effect model requestId =
    case effect of
        PerformRequest.NoModelChange ->
            model

        PerformRequest.TrackWaitingForContent ->
            trackContentRequest model requestId WaitingForContent

        PerformRequest.TrackWaitingForNip05 ->
            trackContentRequest model requestId WaitingForNip05

        PerformRequest.MarkArticleDetailsRequested addresses ->
            markAddressesRequested model addresses

        PerformRequest.ClearArticlesByDate ->
            { model | articlesByDate = [] }

        PerformRequest.SetArticlesByDate articles ->
            { model | articlesByDate = articles }

        PerformRequest.ClearArticleDrafts ->
            { model | articleDraftsByDate = [] }

        PerformRequest.ClearPicturePosts ->
            { model | picturePosts = Dict.empty }



eventFiltersWithUntil : List EventFilter -> Maybe Posix -> List EventFilter
eventFiltersWithUntil =
    Request.eventFiltersWithUntil



send : Model -> Time.Posix -> SendRequest -> ( Model, Cmd Msg )
send model now sendRequest =
    let
        payload =
            Send.prepare
                { getBookmarks = getBookmarks model
                , getFollowList = getFollowsList model
                , getMuteList = getMuteList model
                , writeRelaysFor = getWriteRelayUrlsForPubKey model
                , draftStorageRelaysFor = getDraftStorageRelayUrls model
                , applicationDataRelays = getApplicationDataRelays model
                , now = now
                }
                sendRequest
    in
    queueSend model sendRequest payload.relays payload.event


queueSend : Model -> SendRequest -> List RelayUrl -> Event -> ( Model, Cmd Msg )
queueSend model sendRequest relays event =
    let
        sendId =
            model.lastSendRequestId
    in
    ( { model
        | lastSendRequestId = sendId + 1
        , sendRequests = Dict.insert sendId sendRequest model.sendRequests
      }
    , sendEventWithId model sendId relays event
    )


sendEvent : Model -> List RelayUrl -> Event -> Cmd Msg
sendEvent model relays event =
    sendEventWithId model model.lastSendRequestId relays event


sendEventWithId : Model -> SendRequestId -> List RelayUrl -> Event -> Cmd Msg
sendEventWithId model sendId relays event =
    let
        actualWriteRelays =
            if model.testMode == TestModeEnabled then
                Pareto.testRelayUrls

            else
                relays
    in
    model.hooks.sendEvent sendId actualWriteRelays event


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


getPicturePosts : Model -> List PicturePost
getPicturePosts model =
    model.picturePosts
        |> Dict.values
        |> List.filter (\picturePost -> not (isMuted model model.defaultUser picturePost.pubKey))
        |> List.sortBy (\picturePost -> picturePost.createdAt |> Time.posixToMillis |> negate)


getPicturePostById : Model -> EventId -> Maybe PicturePost
getPicturePostById model id =
    model.picturePosts
        |> Dict.get id


getProfileValidationStatus : Model -> PubKey -> Maybe ProfileValidation
getProfileValidationStatus model pubKey =
    Dict.get pubKey model.profileValidations


getArticle : Model -> AddressComponents -> Maybe Article
getArticle model addressComponents =
    Articles.get model addressComponents


getArticlesByDate : Model -> List Article
getArticlesByDate model =
    Articles.publishedByDate model
        |> List.filter (\article -> not (isMuted model model.defaultUser article.author))


resetArticles : Model -> Model
resetArticles model =
    { model | articlesByDate = [] }


getArticleDraftsByDate : Model -> List Article
getArticleDraftsByDate model =
    model.articleDraftsByDate
        |> List.filter (filterDeletedArticle model)
        |> List.filter
            (\article ->
                case model.defaultUser of
                    Just pubKey ->
                        article.author == pubKey

                    Nothing ->
                        False
            )


getArticleDraftWithIdentifier : Model -> PubKey -> String -> Maybe Article
getArticleDraftWithIdentifier model pubKey identifier =
    Articles.getDraftWithIdentifier model pubKey identifier
        |> Maybe.andThen
            (\article ->
                if filterDeletedArticle model article then
                    Just article

                else
                    Nothing
            )


getArticleDraftWithId : Model -> EventId -> Maybe Article
getArticleDraftWithId model id =
    Articles.getDraftWithId model id


getArticlesForAuthor : Model -> PubKey -> List Article
getArticlesForAuthor model pubKey =
    Articles.forAuthor model pubKey


filterDeletedArticle : Model -> Article -> Bool
filterDeletedArticle =
    Articles.isNotDeleted


getArticleForAddressComponents : Model -> AddressComponents -> Maybe Article
getArticleForAddressComponents model addressComponents =
    case addressComponents of
        ( KindLongFormContent, pubKey, identifier ) ->
            Articles.getWithIdentifier model pubKey identifier

        ( KindDraftLongFormContent, pubKey, identifier ) ->
            getArticleDraftWithIdentifier model pubKey identifier

        _ ->
            Nothing


getArticleForNip19 : Model -> NIP19Type -> Maybe Article
getArticleForNip19 model nip19 =
    Articles.getForNip19 model nip19


getArticleWithIdentifier : Model -> PubKey -> String -> Maybe Article
getArticleWithIdentifier model pubKey identifier =
    Articles.getWithIdentifier model pubKey identifier


getArticleByNip05AndIdentifier : Model -> Nip05 -> String -> Maybe Article
getArticleByNip05AndIdentifier model nip05 identifier =
    getPubKeyByNip05 model nip05
        |> Maybe.andThen (\pubKey -> Articles.getWithIdentifier model pubKey identifier)


getArticleWithId : Model -> EventId -> Maybe Article
getArticleWithId model eventId =
    Articles.getWithId model eventId


getBlossomServers : Model -> PubKey -> List String
getBlossomServers model pubKey =
    model.userServerLists
        |> Dict.get pubKey
        |> Maybe.withDefault []


getDefaultNip96Servers : Model -> PubKey -> List String
getDefaultNip96Servers model pubKey =
    if isEditor model pubKey then
        Pareto.defaultNip96ServersAuthors

    else
        Pareto.defaultNip96ServersPublic


getDefaultBlossomServers : Model -> PubKey -> List String
getDefaultBlossomServers model pubKey =
    if isEditor model pubKey then
        Pareto.defaultBlossomServersAuthors

    else
        Pareto.defaultBlossomServersPublic


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
    Articles.filterWithIdentifier identifier articles


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


getMuteList : Model -> PubKey -> Maybe (List Following)
getMuteList model pubKey =
    Dict.get pubKey model.muteLists


getArticleComments : Model -> Maybe PubKey -> AddressComponents -> List ArticleComment
getArticleComments model maybeUserPubKey addressComponents =
    CommentsQuery.articleCommentsMerged model.commentsByAddress model.shortTextNotes addressComponents
        |> List.filter (\comment -> not (isMuted model maybeUserPubKey comment.pubKey))


getTextNoteCommentsForArticle : Model -> AddressComponents -> List CommentType
getTextNoteCommentsForArticle model addressComponents =
    CommentsQuery.textNoteCommentsForArticle model.shortTextNotes addressComponents


getArticleCommentComments : Model -> AddressComponents -> Dict EventId (List ArticleCommentComment)
getArticleCommentComments model addressComponents =
    CommentsQuery.articleCommentComments model.commentsByAddress model.shortTextNotes addressComponents


getBookmarks : Model -> PubKey -> Maybe BookmarkList
getBookmarks model pubKey =
    Dict.get pubKey model.bookmarkLists


getReactionsForArticle : Model -> AddressComponents -> Maybe (Dict PubKey Nostr.Reactions.Reaction)
getReactionsForArticle model addressComponents =
    ReactionsStore.forAddress model addressComponents


getReactionsForEventId : Model -> EventId -> Maybe (Dict PubKey Nostr.Reactions.Reaction)
getReactionsForEventId model eventId =
    ReactionsStore.forEventId model eventId


getRepostsForArticle : Model -> AddressComponents -> Maybe (Dict PubKey Nostr.Nip18.Repost)
getRepostsForArticle model addressComponents =
    Reposts.forAddress model addressComponents


getRepostsForEventId : Model -> EventId -> Maybe (Dict PubKey Nostr.Nip18.Repost)
getRepostsForEventId model eventId =
    Reposts.forEventId model eventId


getRelaysForPubKey : Model -> PubKey -> List ( RelayRole, Relay )
getRelaysForPubKey model pubKey =
    let
        testRelays =
            if model.testMode == TestModeEnabled then
                Pareto.testRelayUrls

            else
                []
    in
    RelayAccess.combinedForPubKey
        model.relaysForPubKey
        model.relayMetadataLists
        model.relays
        testRelays
        pubKey



-- this function only returns the relays obtained via relay metadata list (NIP-65 / kind 10002)


getRelayListForPubKey : Model -> PubKey -> List RelayMetadata
getRelayListForPubKey model pubKey =
    Dict.get pubKey model.relayMetadataLists
        |> Maybe.withDefault []


getNip65RelaysForPubKey : Model -> PubKey -> List ( RelayRole, Relay )
getNip65RelaysForPubKey model pubKey =
    RelayAccess.nip65ForPubKey model.relayMetadataLists model.relays pubKey


getNip65ReadRelaysForPubKey : Model -> PubKey -> List Relay
getNip65ReadRelaysForPubKey model pubKey =
    getNip65RelaysForPubKey model pubKey
        |> RelayAccess.filterRead


getNip65WriteRelaysForPubKey : Model -> PubKey -> List Relay
getNip65WriteRelaysForPubKey model pubKey =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls
            |> List.filterMap (getRelayData model)

    else
        getNip65RelaysForPubKey model pubKey
            |> RelayAccess.filterWrite


getReadRelaysForPubKey : Model -> PubKey -> List Relay
getReadRelaysForPubKey model pubKey =
    getRelaysForPubKey model pubKey
        |> RelayAccess.filterRead


getReadRelayUrlsForPubKey : Model -> PubKey -> List RelayUrl
getReadRelayUrlsForPubKey model pubKey =
    getReadRelaysForPubKey model pubKey
        |> RelayAccess.urls
        |> withoutBlockedRelays model


getWriteRelaysForPubKey : Model -> PubKey -> List Relay
getWriteRelaysForPubKey model pubKey =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls
            |> withoutBlockedRelays model
            |> List.filterMap (getRelayData model)

    else
        getRelaysForPubKey model pubKey
            |> RelayAccess.filterWrite
            |> List.filter (\relay -> not (isBlockedRelay model relay.url))


getWriteRelayUrlsForPubKey : Model -> PubKey -> List RelayUrl
getWriteRelayUrlsForPubKey model pubKey =
    getWriteRelaysForPubKey model pubKey
        |> RelayAccess.urls


getDraftRelayUrls : Model -> EventId -> List RelayUrl
getDraftRelayUrls model articleId =
    model.articleDraftRelays
        |> Dict.get articleId
        |> Maybe.withDefault Dict.empty
        |> Dict.values


{-| Relays for NIP-37 draft wraps: local (this device) then private (10013),
then write/default fallback.
-}
getDraftStorageRelayUrls : Model -> PubKey -> List RelayUrl
getDraftStorageRelayUrls model pubKey =
    case RelayList.withUniqueEntries (getLocalRelayUrls model ++ getPrivateRelayUrls model pubKey) of
        [] ->
            case getWriteRelayUrlsForPubKey model pubKey of
                [] ->
                    getDefaultRelays model

                writeRelays ->
                    writeRelays

        storageRelays ->
            withoutBlockedRelays model storageRelays


getLocalRelayUrls : Model -> List RelayUrl
getLocalRelayUrls model =
    model.localRelays


getPrivateRelayUrls : Model -> PubKey -> List RelayUrl
getPrivateRelayUrls model pubKey =
    Dict.get pubKey model.privateRelayLists
        |> Maybe.withDefault []


getUserBlockedRelayUrls : Model -> PubKey -> List RelayUrl
getUserBlockedRelayUrls model pubKey =
    Dict.get pubKey model.blockedRelayLists
        |> Maybe.withDefault []


blockedRelayUrls : Model -> List RelayUrl
blockedRelayUrls model =
    let
        fromUser =
            model.defaultUser
                |> Maybe.map (getUserBlockedRelayUrls model)
                |> Maybe.withDefault []
    in
    RelayList.withUniqueEntries (Pareto.blockedRelays ++ fromUser)


isBlockedRelay : Model -> RelayUrl -> Bool
isBlockedRelay model url =
    RelayAccess.isBlocked (blockedRelayUrls model) url


withoutBlockedRelays : Model -> List RelayUrl -> List RelayUrl
withoutBlockedRelays model urls =
    RelayAccess.withoutBlocked (blockedRelayUrls model) urls


getBlockedRelayUrls : Model -> List RelayUrl
getBlockedRelayUrls =
    blockedRelayUrls


getSearchRelayUrls : Model -> Maybe PubKey -> List RelayUrl
getSearchRelayUrls model maybePubKey =
    let
        raw =
            case maybePubKey of
                Just pubKey ->
                    Dict.get pubKey model.searchRelayLists
                        |> Maybe.withDefault (RelayAccess.searchUrls model.relays Pareto.defaultSearchRelays)

                Nothing ->
                    RelayAccess.searchUrls model.relays Pareto.defaultSearchRelays

        allowed =
            withoutBlockedRelays model raw
    in
    if List.isEmpty allowed then
        withoutBlockedRelays model Pareto.defaultSearchRelays

    else
        allowed


getSearchRelaysForPubKey : Model -> PubKey -> List Relay
getSearchRelaysForPubKey model pubKey =
    Dict.get pubKey model.searchRelayLists
        |> Maybe.map (RelayAccess.resolveUrls model.relays)
        |> Maybe.withDefault []



-- filter relays that claim to support NIP-50 (Search)


relaysWithSearchCapability : Model -> List RelayUrl
relaysWithSearchCapability model =
    RelayAccess.withSearchCapability model.relays


getRelaysForRequest : Model -> Maybe RequestId -> List RelayUrl
getRelaysForRequest model maybeRequestId =
    let
        requestUrls =
            maybeRequestId
                |> Maybe.andThen (getRequest model)
                |> Maybe.andThen Request.relaysOfRequest
                |> Maybe.withDefault []
    in
    case requestUrls of
        [] ->
            getDefaultRelays model

        relayUrls ->
            withoutBlockedRelays model relayUrls


getDefaultRelays : Model -> List RelayUrl
getDefaultRelays model =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls

    else
        withoutBlockedRelays model model.defaultRelays


getApplicationDataRelays : Model -> List RelayUrl
getApplicationDataRelays model =
    if model.testMode == TestModeEnabled then
        Pareto.testRelayUrls

    else
        Pareto.applicationDataRelays


getRelayData : Model -> RelayUrl -> Maybe Relay
getRelayData model relayUrl =
    Dict.get (Relay.toKey relayUrl) model.relays


getRequest : Model -> RequestId -> Maybe Request
getRequest model requestId =
    Dict.get requestId model.requests


getShortNoteById : Model -> EventId -> Maybe TextNote
getShortNoteById model noteId =
    Dict.get noteId model.shortTextNotes


getShortNotes : Model -> List TextNote
getShortNotes model =
    Dict.values model.shortTextNotes


getZapReceiptsForArticle : Model -> Article -> Maybe (Dict String ZapReceipt)
getZapReceiptsForArticle model article =
    ZapsQuery.forArticle model article


getZapReceiptsForTagReference : Model -> TagReference -> Maybe (Dict String ZapReceipt)
getZapReceiptsForTagReference model tagReference =
    ZapsQuery.forTagReference model tagReference


getZapReceiptsForEventId : Model -> EventId -> Maybe (Dict String ZapReceipt)
getZapReceiptsForEventId model eventId =
    ZapsQuery.forEventId model eventId


hasZapReceiptWithBolt11 : Model -> String -> Bool
hasZapReceiptWithBolt11 model bolt11 =
    ZapsQuery.hasBolt11 model bolt11


zapReceiptIdsForTagReference : Model -> TagReference -> Set String
zapReceiptIdsForTagReference model tagReference =
    ZapsQuery.idsForTagReference model tagReference


getProfile : Model -> PubKey -> Maybe Profile
getProfile model pubKey =
    Dict.get pubKey model.profiles


getProfileByNip05 : Model -> Nip05 -> Maybe Profile
getProfileByNip05 model nip05 =
    getPubKeyByNip05 model nip05
        |> Maybe.andThen (getProfile model)


nip05LookupKey : Nip05 -> String
nip05LookupKey =
    Nip05Cache.lookupKey


nip05sEqual : Nip05 -> Nip05 -> Bool
nip05sEqual =
    Nip05Cache.equal


getPubKeyByNip05 : Model -> Nip05 -> Maybe PubKey
getPubKeyByNip05 model nip05 =
    Dict.get (Nip05Cache.lookupKey nip05) model.pubKeyByNip05


bootstrapPubKeyByNip05 : Dict Nip05String PubKey
bootstrapPubKeyByNip05 =
    Pareto.bootstrapPubKeyByNip05


insertPubKeyByNip05 : Nip05 -> PubKey -> Model -> Model
insertPubKeyByNip05 nip05 pubKey model =
    Nip05Apply.insertPubKey model nip05 pubKey


pubKeyFromNip05Names : Nip05 -> Nip05.Nip05Data -> Maybe PubKey
pubKeyFromNip05Names =
    Nip05Cache.pubKeyFromNames


requestCommunityPostApprovals : Model -> Community -> Cmd Msg
requestCommunityPostApprovals model community =
    [ Community.postApprovalFilter community ]
        |> model.hooks.requestEvents "Community post approvals" False -1 []


eventFilterForCommunityPostApprovals : Community -> EventFilter
eventFilterForCommunityPostApprovals =
    Community.postApprovalFilter


requestUserData : Model -> PubKey -> ( Model, Cmd Msg )
requestUserData model pubKey =
    let
        -- User-bound session data (cashu, drafts, defaultUser) must not leak
        -- across logout or account switch.
        modelForUser =
            if model.defaultUser == Just pubKey then
                model

            else
                clearUserSessionState model

        request =
            -- assumption: our standard relays are good for the user's profile
            Request.userDataFilter pubKey
                |> RequestProfile Nothing
                |> createRequest modelForUser "Related data for logged-in user" []
    in
    let
        updated =
            { modelForUser | defaultUser = Just pubKey }

        ( requested, requestCmd ) =
            doRequest updated request
    in
    ( requested
    , Cmd.batch
        [ model.hooks.setBlockedRelays (blockedRelayUrls updated)
        , requestCmd
        ]
    )


getMissingProfilePubKeys : Model -> List PubKey -> List PubKey
getMissingProfilePubKeys model pubKeys =
    RelatedRequests.missingPubKeys model.profiles pubKeys


eventFilterForAuthors : List PubKey -> Maybe EventFilter
eventFilterForAuthors =
    EventFilters.forAuthors


getCommunityList : Model -> PubKey -> Maybe (List CommunityReference)
getCommunityList model pubKey =
    Dict.get pubKey model.communityLists


isArticleBookmarked : Model -> Article -> PubKey -> Bool
isArticleBookmarked model article pubKey =
    addressComponentsForArticle article
        |> Maybe.map (\addressComponents -> areAddressComponentsBookmarked model addressComponents pubKey)
        |> Maybe.withDefault False


areAddressComponentsBookmarked : Model -> AddressComponents -> PubKey -> Bool
areAddressComponentsBookmarked model addressComponents pubKey =
    getBookmarks model pubKey
        |> Maybe.withDefault emptyBookmarkList
        |> (\bookmarkList -> BookmarkList.containsAddress bookmarkList addressComponents)


isEventIdBookmarked : Model -> EventId -> PubKey -> Bool
isEventIdBookmarked model eventId pubKey =
    getBookmarks model pubKey
        |> Maybe.withDefault emptyBookmarkList
        |> (\bookmarkList -> BookmarkList.containsEventId bookmarkList eventId)


getZapReceiptsCountForTagReference : Model -> TagReference -> Maybe Int
getZapReceiptsCountForTagReference model tagReference =
    getZapReceiptsForTagReference model tagReference
        |> Maybe.map ZapsQuery.totalAmount


getZapReceiptsCountForArticle : Model -> Article -> Maybe Int
getZapReceiptsCountForArticle model article =
    getZapReceiptsForArticle model article
        |> Maybe.map ZapsQuery.totalAmount


getZapReceiptsCountForComment : Model -> EventId -> Maybe Int
getZapReceiptsCountForComment model eventId =
    getZapReceiptsForEventId model eventId
        |> Maybe.map ZapsQuery.totalAmount


addZapAmount : ZapReceipt -> Int -> Int
addZapAmount =
    ZapsQuery.addAmount


getBookmarkListCountForAddressComponents : Model -> AddressComponents -> Int
getBookmarkListCountForAddressComponents model addressComponents =
    BookmarkList.countAddressAcross model.bookmarkLists addressComponents


getBookmarkListCountForEventId : Model -> EventId -> Int
getBookmarkListCountForEventId model eventId =
    BookmarkList.countEventIdAcross model.bookmarkLists eventId


getReactionsCountForArticle : Model -> Article -> Maybe Int
getReactionsCountForArticle model article =
    article
        |> addressComponentsForArticle
        |> Maybe.andThen (ReactionsStore.countForAddress model)


getReactionsCountForAddressComponents : Model -> AddressComponents -> Maybe Int
getReactionsCountForAddressComponents model addressComponents =
    ReactionsStore.countForAddress model addressComponents


getReactionsCountForEventId : Model -> EventId -> Maybe Int
getReactionsCountForEventId model eventId =
    ReactionsStore.countForEventId model eventId


getRepostsCountForArticle : Model -> Article -> Maybe Int
getRepostsCountForArticle model article =
    article
        |> addressComponentsForArticle
        |> Maybe.andThen (Reposts.countForAddress model)


getRepostsCountForAddressComponents : Model -> AddressComponents -> Maybe Int
getRepostsCountForAddressComponents model addressComponents =
    Reposts.countForAddress model addressComponents


getRepostsCountForEventId : Model -> EventId -> Maybe Int
getRepostsCountForEventId model eventId =
    Reposts.countForEventId model eventId


getReactionForArticle : Model -> PubKey -> AddressComponents -> Maybe Reaction
getReactionForArticle model pubKey addressComponents =
    ReactionsStore.reactionForAddress model pubKey addressComponents


getReactionForEventId : Model -> PubKey -> EventId -> Maybe Reaction
getReactionForEventId model pubKey eventId =
    ReactionsStore.reactionForEventId model pubKey eventId


getHighlightsForAddress : Model -> AddressComponents -> List Highlights.Highlight
getHighlightsForAddress model addressComponents =
    Highlights.forAddress model addressComponents


getHighlightsCountForAddress : Model -> AddressComponents -> Maybe Int
getHighlightsCountForAddress model addressComponents =
    Highlights.countForAddress model addressComponents


highlightsForPubKey : Model -> PubKey -> List Highlights.HighlightItem
highlightsForPubKey model pubKey =
    Highlights.forAuthorArticles model pubKey (getArticlesForAuthor model pubKey)


notificationsForPubKey : Model -> PubKey -> List Notifications.NotificationItem
notificationsForPubKey model pubKey =
    Notifications.forAuthorArticles model pubKey (getArticlesForAuthor model pubKey)


unreadNotificationsCount : Model -> PubKey -> Int -> Int
unreadNotificationsCount model pubKey lastSeenMillis =
    Notifications.unreadCount model pubKey (getArticlesForAuthor model pubKey) lastSeenMillis




eventFilterForDeletionRequests : List TagReference -> Maybe EventFilter
eventFilterForDeletionRequests =
    EventFilters.forDeletionRequests


eventFilterForReactions : List TagReference -> Maybe EventFilter
eventFilterForReactions =
    EventFilters.forReactions


articleFromList : EventFilter -> List Article -> Maybe Article
articleFromList filter articles =
    articles
        |> List.filter (filterMatchesArticle filter)
        |> List.head


cmdBatch2 : Cmd msg -> Cmd msg -> Cmd msg
cmdBatch2 cmd1 cmd2 =
    Cmd.batch [ cmd1, cmd2 ]


paretoAuthorsFollowList : List Following
paretoAuthorsFollowList =
    Pareto.authorsFollowList


paretoKnownPubKey : Nip05 -> Maybe PubKey
paretoKnownPubKey nip05 =
    Pareto.bootstrapAuthorsList
        |> Dict.get (nip05ToString nip05)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedMessage message ->
            applyIncoming (Incoming.decode message) model

        CheckNip05Cache target nip05 now ->
            checkNip05Cache model target nip05 now

        Nip05Fetched nip05 requestedAt result ->
            updateWithNip05Result model nip05 requestedAt result

        Nip11Fetched relayUrl result ->
            let
                modelWithRelay =
                    { model | relays = Relay.applyNip11Result relayUrl result model.relays }
            in
            case result of
                Ok _ ->
                    ( modelWithRelay, Cmd.none )

                Err err ->
                    ( { modelWithRelay
                        | errors = ("Error fetching NIP11 data for " ++ Relay.toKey relayUrl ++ ": " ++ httpErrorToString err) :: model.errors
                      }
                    , Cmd.none
                    )

        ReceivedPortalCheckResultPubKey pubKey (Ok portalCheckResponse) ->
            ( { model | portalUserInfoPubKey = Dict.insert pubKey portalCheckResponse model.portalUserInfoPubKey }
            , Cmd.none
            )

        ReceivedPortalCheckResultPubKey pubKey (Err error) ->
            ( { model | errors = ("Error fetching author check result for pubkey " ++ pubKey ++ ": " ++ httpErrorToString error) :: model.errors }
            , Cmd.none
            )

        ReceivedPortalCheckResultNip05 nip05 (Ok portalCheckResponse) ->
            ( { model | portalUserInfoNip05 = Dict.insert (nip05ToString nip05) portalCheckResponse model.portalUserInfoNip05 }
            , Cmd.none
            )

        ReceivedPortalCheckResultNip05 nip05 (Err error) ->
            ( { model | errors = ("Error fetching author check result for nip05 " ++ nip05ToString nip05 ++ ": " ++ httpErrorToString error) :: model.errors }
            , Cmd.none
            )


applyIncoming : Incoming.Effect -> Model -> ( Model, Cmd Msg )
applyIncoming effect model =
    case effect of
        Incoming.SetPoolState state ->
            ( { model | poolState = state }, Cmd.none )

        Incoming.NoOp ->
            ( model, Cmd.none )

        Incoming.SetRelayStatus relayUrl state ->
            ( { model | relays = Relay.updateRelayStatus relayUrl state model.relays }, Cmd.none )

        Incoming.AppendError error ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        Incoming.GotProfiles pubkeyProfiles ->
            updateWithPubkeyProfiles model pubkeyProfiles

        Incoming.GotZapReceipts zapReceipts ->
            updateWithZapReceipts model zapReceipts

        Incoming.GotAuthors authorsData ->
            ( { model | pubKeyByNip05 = Nip05Apply.mergeAuthorPubKeys model.pubKeyByNip05 authorsData }, Cmd.none )

        Incoming.GotEvents requestId kind events ->
            updateModelWithEvents model requestId kind events

        Incoming.EventsComplete requestId ->
            case Dict.get requestId model.contentRequestStates of
                Just WaitingForContent ->
                    ( settleContentRequest model requestId, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Incoming.GotNutzapRedeemed redeemed ->
            handleNutzapRedeemed model redeemed

        Incoming.GotNutzapRedeemFailed nutzapId reason ->
            ( { model
                | pendingNutzapRedeems = Set.remove nutzapId model.pendingNutzapRedeems
                , errors = reason :: model.errors
              }
            , Cmd.none
            )

        Incoming.GotCashuBalance balance ->
            ( { model | cashuBalance = balance }, Cmd.none )



updateModelWithEvents : Model -> Int -> Kind -> List Event -> ( Model, Cmd Msg )
updateModelWithEvents model requestId kind events =
    let
        modelAfterContentRequest =
            case Dict.get requestId model.contentRequestStates of
                Just WaitingForContent ->
                    if ContentRequest.settlesOnKind kind then
                        settleContentRequest model requestId

                    else
                        model

                _ ->
                    model
    in
    case kind of
        KindBookmarkList ->
            updateModelWithBookmarkLists modelAfterContentRequest events

        KindBookmarkSets ->
            updateModelWithBookmarkSets modelAfterContentRequest events

        KindCommunityDefinition ->
            updateModelWithCommunityDefinitions modelAfterContentRequest events

        KindCommunitiesList ->
            updateModelWithCommunityLists modelAfterContentRequest events

        KindEventDeletionRequest ->
            updateModelWithDeletionRequests modelAfterContentRequest events

        KindComment ->
            updateModelWithComments modelAfterContentRequest requestId events

        KindPicture ->
            updateModelWithPictures modelAfterContentRequest requestId events

        KindRepost ->
            updateModelWithReposts modelAfterContentRequest events

        KindGenericRepost ->
            updateModelWithReposts modelAfterContentRequest events

        KindUserServerList ->
            updateModelWithUserServerLists modelAfterContentRequest requestId events

        KindFileStorageServerList ->
            updateModelWithFileStorageServerLists modelAfterContentRequest requestId events

        KindFollows ->
            updateModelWithFollowLists modelAfterContentRequest events

        KindMuteList ->
            updateModelWithMuteLists modelAfterContentRequest events

        KindFollowSets ->
            updateModelWithFollowSets modelAfterContentRequest events

        KindLongFormContent ->
            updateModelWithLongFormContent modelAfterContentRequest requestId events

        KindDraftLongFormContent ->
            updateModelWithLongFormContentDraft modelAfterContentRequest requestId events

        KindReaction ->
            updateModelWithReactions modelAfterContentRequest requestId events

        KindHighlights ->
            updateModelWithHighlights modelAfterContentRequest events

        KindSearchRelaysList ->
            updateModelWithSearchRelays modelAfterContentRequest requestId events

        KindBlockedRelaysList ->
            updateModelWithBlockedRelays modelAfterContentRequest events

        KindPrivateRelayList ->
            updateModelWithPrivateRelays modelAfterContentRequest requestId events

        KindShortTextNote ->
            updateModelWithShortTextNotes modelAfterContentRequest requestId events

        KindUserMetadata ->
            updateModelWithUserMetadata modelAfterContentRequest requestId events

        KindRelayListMetadata ->
            updateModelWithRelayListMetadata modelAfterContentRequest events

        KindCashuWalletEvent ->
            updateModelWithCashuWallet modelAfterContentRequest events

        KindCashuWalletTokens ->
            updateModelWithCashuTokens modelAfterContentRequest events

        KindCashuWalletHistory ->
            updateModelWithCashuHistory modelAfterContentRequest events

        KindNutzapMintRecommendation ->
            updateModelWithNutzapMintRec modelAfterContentRequest events

        KindNutzap ->
            updateModelWithNutzaps modelAfterContentRequest events

        _ ->
            ( modelAfterContentRequest, Cmd.none )


updateModelWithBookmarkLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithBookmarkLists model events =
    ( { model | bookmarkLists = BookmarkList.ingest model.bookmarkLists events }, Cmd.none )


updateModelWithBookmarkSets : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithBookmarkSets model events =
    ( { model | bookmarkSets = BookmarkSet.ingest model.bookmarkSets events }, Cmd.none )


updateModelWithCommunityDefinitions : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCommunityDefinitions model events =
    ( { model | communities = Community.ingest model.communities events }, Cmd.none )


updateModelWithCommunityLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCommunityLists model events =
    ( { model | communityLists = CommunityList.ingest model.communityLists events }, Cmd.none )


updateModelWithDeletionRequests : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithDeletionRequests model events =
    let
        updated =
            DeletionRequests.ingest
                { deletedAddresses = model.deletedAddresses
                , deletedEvents = model.deletedEvents
                }
                events
    in
    ( { model
        | deletedAddresses = updated.deletedAddresses
        , deletedEvents = updated.deletedEvents
      }
    , Cmd.none
    )


updateModelWithReposts : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithReposts model events =
    let
        updated =
            Reposts.ingest
                { repostsByAddress = model.repostsByAddress
                , repostsByEventId = model.repostsByEventId
                }
                events
    in
    ( { model
        | repostsByAddress = updated.repostsByAddress
        , repostsByEventId = updated.repostsByEventId
      }
    , Cmd.none
    )


updateModelWithComments : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithComments model requestId events =
    let
        ( commentsByAddress, comments ) =
            Comments.ingest model.commentsByAddress events

        maybeRequest =
            Dict.get requestId model.requests

        ( modelWithRequest, cmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForArticleComments model comments request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { modelWithRequest | commentsByAddress = commentsByAddress }
    , cmd
    )


updateModelWithPictures : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithPictures model requestId events =
    let
        ( picturePosts, posts ) =
            PicturePosts.ingest model.picturePosts events

        modelWithPicturePosts =
            { model | picturePosts = picturePosts }

        maybeRequest =
            getRequest model requestId

        ( profileRequestModel, maybeRequestWithProfiles ) =
            maybeRequest
                |> Maybe.map (\request -> requestRelatedProfiles (List.map .pubKey posts) ( modelWithPicturePosts, request ))
                |> Maybe.map (requestRelatedReactions (List.map .id posts))
                |> Maybe.map (\( modelWithRequests, extendedRequest ) -> ( modelWithRequests, Just extendedRequest ))
                |> Maybe.withDefault ( modelWithPicturePosts, maybeRequest )
    in
    case maybeRequestWithProfiles of
        Just requestWithProfiles ->
            doRequest profileRequestModel requestWithProfiles

        Nothing ->
            ( modelWithPicturePosts, Cmd.none )


uniquePubKeys : List PubKey -> List PubKey
uniquePubKeys =
    RelatedRequests.uniquePubKeys


requestRelatedKindsForArticleComments : Model -> List CommentType -> Request -> ( Model, Cmd Msg )
requestRelatedKindsForArticleComments model comments request =
    RelatedRequests.commentFollowUps model.profiles comments
        |> (\datas -> extendRequestWith datas ( model, request ))
        |> (\( extendedModel, extendedRequest ) -> doRequest extendedModel extendedRequest )


updateModelWithUserServerLists : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithUserServerLists model _ events =
    ( { model | userServerLists = Blossom.ingest model.userServerLists events }, Cmd.none )


updateModelWithFileStorageServerLists : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithFileStorageServerLists model _ events =
    ( { model | fileStorageServerLists = FileStorageServerList.ingest model.fileStorageServerLists events }, Cmd.none )


updateModelWithLongFormContent : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithLongFormContent model requestId events =
    let
        ingested =
            Articles.ingestPublished
                { articlesByAddress = model.articlesByAddress
                , articlesByAuthor = model.articlesByAuthor
                , articlesByDate = model.articlesByDate
                , articlesById = model.articlesById
                }
                events

        maybeRequest =
            Dict.get requestId model.requests

        ( requestModel, requestCmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForArticles model ingested.articles request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { requestModel
        | articlesByAddress = ingested.articlesByAddress
        , articlesByAuthor = ingested.articlesByAuthor
        , articlesByDate = ingested.articlesByDate
        , articlesById = ingested.articlesById
        , errors = ingested.errors ++ model.errors
      }
    , requestCmd
    )


getErrorMessages : Model -> List String
getErrorMessages model =
    model.errors


sortArticlesByDate : List Article -> List Article
sortArticlesByDate =
    Articles.sortByDate


updateModelWithLongFormContentDraft : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithLongFormContentDraft model requestId events =
    let
        ingested =
            Articles.ingestDrafts
                { articleDraftsByDate = model.articleDraftsByDate
                , articleDraftsById = model.articleDraftsById
                , articleDraftRelays = model.articleDraftRelays
                }
                events

        maybeRequest =
            Dict.get requestId model.requests

        ( requestModel, requestCmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForArticles model ingested.articles request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { requestModel
        | articleDraftsByDate = ingested.articleDraftsByDate
        , articleDraftsById = ingested.articleDraftsById
        , articleDraftRelays = ingested.articleDraftRelays
        , errors = ingested.errors ++ model.errors
      }
    , requestCmd
    )


requestRelatedKindsForArticles : Model -> List Article -> Request -> ( Model, Cmd Msg )
requestRelatedKindsForArticles model articles request =
    let
        wantsSocialDetails =
            List.member KindReaction request.relatedKinds
                || List.member KindComment request.relatedKinds
                || List.member KindZapReceipt request.relatedKinds

        articlesForDetails =
            if shouldRequestArticleDetails request || wantsSocialDetails then
                List.filter (articleNeedsDetails model) articles

            else
                []

        ( modelWithPrimary, extendedRequest ) =
            RelatedRequests.articlePrimaryFollowUps model.profiles articles
                |> (\datas -> extendRequestWith datas ( model, request ))

        ( modelAfterPrimary, primaryCmd ) =
            doRequest modelWithPrimary extendedRequest

        ( modelAfterDetails, detailsCmd ) =
            requestArticleDetailsBatch modelAfterPrimary articlesForDetails
    in
    ( modelAfterDetails, Cmd.batch [ primaryCmd, detailsCmd ] )


requestArticleDetailsBatch : Model -> List Article -> ( Model, Cmd Msg )
requestArticleDetailsBatch model articles =
    case articles of
        [] ->
            ( model, Cmd.none )

        _ ->
            let
                reactionRequest =
                    RelatedRequests.articleDetailsReactionRequest articles

                nip27Refs =
                    articles
                        |> List.map .nip27References
                        |> List.concat

                baseRequest =
                    { id = model.lastRequestId
                    , relatedKinds = []
                    , states = List.filterMap (Maybe.map RequestCreated) [ reactionRequest ]
                    , description = "Article details"
                    }

                ( _, requestWithNip27 ) =
                    appendNip27ProfileRequests model baseRequest nip27Refs
            in
            if List.isEmpty requestWithNip27.states then
                ( model, Cmd.none )

            else
                doRequest model requestWithNip27


appendNip27ProfileRequests : Model -> Request -> List NIP19Type -> ( Model, Request )
appendNip27ProfileRequests model request nip19List =
    RelatedRequests.nip27PubKeys nip19List
        |> RelatedRequests.profileRequestData model.profiles
        |> Maybe.map (\data -> addToRequest model request data)
        |> Maybe.withDefault ( model, request )


nip27ProfilesRequest : Model -> List NIP19Type -> Maybe EventFilter
nip27ProfilesRequest model nip19List =
    RelatedRequests.nip27PubKeys nip19List
        |> RelatedRequests.missingPubKeys model.profiles
        |> EventFilters.forAuthors


updateModelWithSearchRelays : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithSearchRelays model _ events =
    let
        ingested =
            RelayList.ingestSearchRelays model.searchRelayLists model.relays events

        requestNip11Cmd =
            requestRelayNip11 model ingested.unknownRelays
    in
    ( { model | searchRelayLists = ingested.searchRelayLists }, requestNip11Cmd )


updateModelWithBlockedRelays : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithBlockedRelays model events =
    let
        ingested =
            RelayList.ingestBlockedRelays model.blockedRelayLists events

        updated =
            { model | blockedRelayLists = ingested.blockedRelayLists }
    in
    ( updated, model.hooks.setBlockedRelays (blockedRelayUrls updated) )


updateModelWithPrivateRelays : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithPrivateRelays model _ events =
    let
        ingested =
            RelayList.ingestPrivateRelays model.privateRelayLists model.relays events

        requestNip11Cmd =
            requestRelayNip11 model ingested.unknownRelays
    in
    ( { model | privateRelayLists = ingested.privateRelayLists }, requestNip11Cmd )


updateModelWithReactions : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithReactions model _ events =
    let
        updated =
            ReactionsStore.ingest
                { reactionsForEventId = model.reactionsForEventId
                , reactionsForAddress = model.reactionsForAddress
                }
                events
    in
    ( { model
        | reactionsForEventId = updated.reactionsForEventId
        , reactionsForAddress = updated.reactionsForAddress
      }
    , Cmd.none
    )


updateModelWithHighlights : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithHighlights model events =
    let
        updated =
            Highlights.ingest
                { highlightsByAddress = model.highlightsByAddress }
                events
    in
    ( { model | highlightsByAddress = updated.highlightsByAddress }
    , Cmd.none
    )


updateModelWithShortTextNotes : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithShortTextNotes model requestId events =
    let
        ( textNotesDict, textNotes ) =
            ShortNotes.ingest model.shortTextNotes events

        maybeRequest =
            Dict.get requestId model.requests

        ( requestModel, requestCmd ) =
            case maybeRequest of
                Just request ->
                    requestRelatedKindsForShortNotes model textNotes request

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { requestModel | shortTextNotes = textNotesDict }, requestCmd )


requestRelatedKindsForShortNotes : Model -> List TextNote -> Request -> ( Model, Cmd Msg )
requestRelatedKindsForShortNotes model shortNotes request =
    RelatedRequests.shortNoteFollowUps model.profiles shortNotes
        |> (\datas -> extendRequestWith datas ( model, request ))
        |> (\( extendedModel, extendedRequest ) -> doRequest extendedModel extendedRequest )


requestRelatedProfiles : List PubKey -> ( Model, Request ) -> ( Model, Request )
requestRelatedProfiles pubKeys ( model, request ) =
    RelatedRequests.profileRequestData model.profiles pubKeys
        |> Maybe.map (\data -> addToRequest model request data)
        |> Maybe.withDefault ( model, request )


requestRelatedReactions : List EventId -> ( Model, Request ) -> ( Model, Request )
requestRelatedReactions eventIds ( model, request ) =
    eventIds
        |> List.map TagReferenceEventId
        |> RelatedRequests.reactionsRequestData
        |> Maybe.map (\data -> addToRequest model request data)
        |> Maybe.withDefault ( model, request )


updateModelWithUserMetadata : Model -> RequestId -> List Event -> ( Model, Cmd Msg )
updateModelWithUserMetadata model requestId events =
    let
        ( profilesSum, profiles ) =
            Profiles.ingest model.profiles events

        nip05Requests =
            profiles
                |> List.filterMap
                    (\profile ->
                        Maybe.map (requestNip05Info (Nip05ForPubKey profile.pubKey)) profile.nip05
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
    ( { requestModel
        | profiles = profilesSum
        , pubKeyByNip05 = Profiles.addNip05Mappings requestModel.pubKeyByNip05 profiles
      }
    , requests
    )


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
    createRequest model "Articles for authors" [] (RequestArticlesFeed False [ { emptyEventFilter | authors = Just pubKeys, kinds = Just [ KindLongFormContent ] } ])
        |> doRequest model


updateModelWithRelayListMetadata : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithRelayListMetadata model events =
    let
        ingested =
            RelayListMetadata.ingest model.relayMetadataLists model.relays events

        requestNip11Cmd =
            requestRelayNip11 model ingested.unknownRelays
    in
    ( { model | relayMetadataLists = ingested.relayMetadataLists, relays = ingested.relays }, requestNip11Cmd )


updateModelWithFollowLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithFollowLists model events =
    ( { model | followLists = FollowList.ingest model.followLists events }, Cmd.none )


updateModelWithMuteLists : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithMuteLists model events =
    ( { model | muteLists = FollowList.ingest model.muteLists events }, Cmd.none )


updateModelWithFollowSets : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithFollowSets model events =
    ( { model | followSets = FollowSet.ingest model.followSets events }, Cmd.none )


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


requestNip05Info : Nip05RequestTarget -> Nip05 -> Cmd Msg
requestNip05Info target nip05 =
    Task.perform (CheckNip05Cache target nip05) Time.now


cacheEntryIsFresh : Posix -> Nip05CacheEntry -> Bool
cacheEntryIsFresh =
    Nip05Cache.cacheEntryIsFresh


addNip05Waiter : Nip05RequestTarget -> List Nip05RequestTarget -> List Nip05RequestTarget
addNip05Waiter =
    Nip05Cache.addWaiter


markNip05TargetPending : Model -> Nip05RequestTarget -> Model
markNip05TargetPending model target =
    case target of
        Nip05ForPubKey pubKey ->
            { model | profileValidations = Dict.insert pubKey ValidationPending model.profileValidations }

        Nip05ForRequest _ ->
            model


checkNip05Cache : Model -> Nip05RequestTarget -> Nip05 -> Posix -> ( Model, Cmd Msg )
checkNip05Cache model target nip05 now =
    case Nip05Cache.decideCheck model.nip05Cache nip05 target now of
        JoinPending requestedAt waiters ->
            let
                updatedModel =
                    markNip05TargetPending model target
            in
            ( { updatedModel
                | nip05Cache =
                    Dict.insert (nip05ToString nip05) (Nip05Pending requestedAt waiters) model.nip05Cache
              }
            , Cmd.none
            )

        StartFetch waiters ->
            startNip05Request model waiters nip05 now

        UseCached result ->
            handleNip05Result model target nip05 result


startNip05Request : Model -> List Nip05RequestTarget -> Nip05 -> Posix -> ( Model, Cmd Msg )
startNip05Request model targets nip05 requestedAt =
    let
        updatedModel =
            List.foldl (\target modelAcc -> markNip05TargetPending modelAcc target) model targets

        cacheKey =
            nip05ToString nip05
    in
    ( { updatedModel
        | nip05Cache =
            Dict.insert cacheKey (Nip05Pending requestedAt targets) model.nip05Cache
      }
    , fetchNip05Info (model.environment /= StandAlone) (Nip05Fetched nip05 requestedAt) nip05
    )


updateWithNip05Result : Model -> Nip05 -> Posix -> Result Http.Error Nip05.Nip05Data -> ( Model, Cmd Msg )
updateWithNip05Result model nip05 requestedAt result =
    case Nip05Cache.decideFetch model.nip05Cache nip05 requestedAt result of
        DeliverToWaiters waiters fetchResult ->
            let
                cachedModel =
                    { model
                        | nip05Cache =
                            Dict.insert (nip05ToString nip05) (Nip05Cached requestedAt fetchResult) model.nip05Cache
                    }

                ( updatedModel, commands ) =
                    waiters
                        |> List.foldl
                            (\target ( modelAcc, commandsAcc ) ->
                                let
                                    ( nextModel, command ) =
                                        handleNip05Result modelAcc target nip05 fetchResult
                                in
                                ( nextModel, command :: commandsAcc )
                            )
                            ( cachedModel, [] )
            in
            ( updatedModel, Cmd.batch commands )

        IgnoreStale ->
            ( model, Cmd.none )


handleNip05Result : Model -> Nip05RequestTarget -> Nip05 -> Result Http.Error Nip05.Nip05Data -> ( Model, Cmd Msg )
handleNip05Result model target nip05 result =
    case ( target, result ) of
        ( Nip05ForPubKey pubKey, Ok nip05Data ) ->
            updateProfileWithNip05Data model pubKey nip05 nip05Data

        ( Nip05ForPubKey pubKey, Err error ) ->
            if profileUsesNip05 model pubKey nip05 then
                ( updateProfileWithValidationStatus model pubKey (ValidationNetworkError error), Cmd.none )

            else
                ( model, Cmd.none )

        ( Nip05ForRequest requestId, Ok nip05Data ) ->
            updateModelWithNip05Data model requestId nip05 nip05Data

        ( Nip05ForRequest requestId, Err error ) ->
            let
                reason =
                    "Error fetching NIP05 data for " ++ nip05ToString nip05 ++ ": " ++ httpErrorToString error

                modelWithError =
                    { model | errors = reason :: model.errors }
            in
            ( failContentRequest modelWithError requestId reason, Cmd.none )


profileUsesNip05 : Model -> PubKey -> Nip05 -> Bool
profileUsesNip05 model pubKey nip05 =
    Nip05Apply.profileUses model.profiles pubKey nip05


updateProfileWithNip05Data : Model -> PubKey -> Nip05 -> Nip05.Nip05Data -> ( Model, Cmd Msg )
updateProfileWithNip05Data model pubKey nip05 nip05Data =
    if not (profileUsesNip05 model pubKey nip05) then
        ( model, Cmd.none )

    else
        let
            maybePubKeyInNip05Data =
                pubKeyFromNip05Names nip05 nip05Data

            nip05Relays =
                Nip05Apply.relaysForResolvedPubKey nip05Data maybePubKeyInNip05Data

            ( validationStatus, relays ) =
                Nip05Cache.validationForPubKey pubKey maybePubKeyInNip05Data nip05Relays

            unknownRelays =
                Nip05Apply.unknownRelayHosts model.relays relays
        in
        ( updateProfileWithValidationStatus model pubKey validationStatus
        , requestRelayNip11 model unknownRelays
        )


updateModelWithNip05Data : Model -> RequestId -> Nip05 -> Nip05.Nip05Data -> ( Model, Cmd Msg )
updateModelWithNip05Data model requestId nip05 nip05Data =
    let
        modelWithValidatedNip05 =
            validateNip05 model nip05 nip05Data

        maybePubKey =
            pubKeyFromNip05Names nip05 nip05Data

        modelWithMapping =
            case maybePubKey of
                Just pubKey ->
                    insertPubKeyByNip05 nip05 pubKey modelWithValidatedNip05

                Nothing ->
                    modelWithValidatedNip05

        loadedProfile =
            maybePubKey
                |> Maybe.andThen (getProfile modelWithMapping)

        maybeRequest =
            getRequest modelWithMapping requestId

        maybeIdentifier =
            maybeRequest
                |> Maybe.andThen identifierFromNip05ArticleRequest

        maybeRelays =
            Nip05Apply.maybeRelaysForResolvedPubKey nip05Data maybePubKey

        followUpRequests =
            case ( maybeRequest, maybePubKey ) of
                ( Just _, Just pubKey ) ->
                    RelatedRequests.nip05FollowUpRequests
                        { pubKey = pubKey
                        , identifier = maybeIdentifier
                        , relays = maybeRelays
                        , needsProfile = loadedProfile == Nothing
                        , needsArticle =
                            maybeIdentifier
                                |> Maybe.map
                                    (\identifier ->
                                        getArticleWithIdentifier modelWithMapping pubKey identifier == Nothing
                                    )
                                |> Maybe.withDefault False
                        }

                _ ->
                    []

        modelAfterNip05 =
            case
                Nip05Cache.decideContentRequest
                    (Dict.get requestId modelWithMapping.contentRequestStates)
                    maybePubKey
                    followUpRequests
            of
                FailMissingPubKey ->
                    failContentRequest modelWithMapping requestId ("NIP-05 did not resolve a pubkey for " ++ nip05ToString nip05)

                KeepWaitingForArticle ->
                    -- RequestArticle follow-up will move this to WaitingForContent.
                    modelWithMapping

                SettleReady ->
                    -- Pubkey resolved and article already cached (or no identifier).
                    settleContentRequest modelWithMapping requestId

                LeaveUnchanged ->
                    modelWithMapping

        ( requestModel, requestCmd ) =
            case ( maybeRequest, followUpRequests ) of
                ( Just request, _ :: _ ) ->
                    extendRequestWith followUpRequests ( modelAfterNip05, request )
                        |> (\( modelWithRequest, extendedRequest ) -> doRequest modelWithRequest extendedRequest)

                _ ->
                    ( modelAfterNip05, Cmd.none )
    in
    ( requestModel, requestCmd )


identifierFromNip05ArticleRequest : Request -> Maybe String
identifierFromNip05ArticleRequest =
    Request.identifierFromNip05ArticleRequest


validateNip05 : Model -> Nip05 -> Nip05.Nip05Data -> Model
validateNip05 model nip05 nip05Data =
    case Nip05Apply.validationAfterFetch model.profiles nip05 nip05Data of
        Just ( pubKey, validationStatus ) ->
            updateProfileWithValidationStatus model pubKey validationStatus

        Nothing ->
            model


updateProfileWithValidationStatus : Model -> PubKey -> ProfileValidation -> Model
updateProfileWithValidationStatus model pubKey valid =
    Nip05Apply.applyValidationStatus model pubKey valid


updateWithPubkeyProfiles : Model -> List Nostr.Profile.PubkeyProfile -> ( Model, Cmd Msg )
updateWithPubkeyProfiles model pubkeyProfiles =
    let
        nip05Requests =
            pubkeyProfiles
                |> List.filterMap
                    (\{ pubKey, profile } ->
                        Maybe.map (requestNip05Info (Nip05ForPubKey pubKey)) profile.nip05
                    )
                |> Cmd.batch

        ( profilesSum, profiles ) =
            Profiles.ingestPubkeyProfiles model.profiles pubkeyProfiles
    in
    ( { model
        | profiles = profilesSum
        , pubKeyByNip05 = Profiles.addNip05Mappings model.pubKeyByNip05 profiles
      }
    , nip05Requests
    )


updateWithZapReceipts : Model -> List Zaps.ZapReceipt -> ( Model, Cmd Msg )
updateWithZapReceipts model zapReceipts =
    let
        updated =
            Zaps.ingest
                { zapReceiptsAddress = model.zapReceiptsAddress
                , zapReceiptsEvents = model.zapReceiptsEvents
                }
                zapReceipts
    in
    ( { model
        | zapReceiptsAddress = updated.zapReceiptsAddress
        , zapReceiptsEvents = updated.zapReceiptsEvents
      }
    , Cmd.none
    )


updateModelWithCashuWallet : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCashuWallet model events =
    let
        maybeWallet =
            events
                |> List.filterMap CashuWallet.walletFromDecryptedEvent
                |> List.filter (\wallet -> Just wallet.pubKey == model.defaultUser)
                |> List.head

        modelWithWallet =
            case maybeWallet of
                Just wallet ->
                    { model | cashuWallet = Just wallet }

                Nothing ->
                    model
    in
    case modelWithWallet.cashuWallet of
        Just wallet ->
            subscribeToNutzaps modelWithWallet wallet

        Nothing ->
            ( modelWithWallet, Cmd.none )


subscribeToNutzaps : Model -> CashuWallet -> ( Model, Cmd Msg )
subscribeToNutzaps model wallet =
    case model.defaultUser of
        Nothing ->
            ( model, Cmd.none )

        Just userPubKey ->
            let
                trustedMints =
                    case Dict.get userPubKey model.nutzapMintRecommendations of
                        Just rec ->
                            if List.isEmpty rec.mints then
                                []

                            else
                                rec.mints

                        Nothing ->
                            wallet.mints
            in
            if List.isEmpty trustedMints then
                ( model, Cmd.none )

            else
                let
                    filter =
                        { emptyEventFilter
                            | kinds = Just [ KindNutzap ]
                            , tagReferences =
                                Just
                                    (TagReferencePubKey userPubKey
                                        :: List.map TagReferenceU trustedMints
                                    )
                        }

                    relaysFrom10019 =
                        Dict.get userPubKey model.nutzapMintRecommendations
                            |> Maybe.map .relays
                            |> Maybe.withDefault []
                            |> List.map Relay.fromString

                    fallbackRelays =
                        getReadRelayUrlsForPubKey model userPubKey

                    relays =
                        if List.isEmpty relaysFrom10019 then
                            fallbackRelays

                        else
                            relaysFrom10019
                in
                Request.RequestNutzaps relays filter
                    |> createRequest model "Nutzaps for logged-in user" []
                    |> doRequest model


updateModelWithCashuTokens : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCashuTokens model events =
    let
        tokens =
            events
                |> List.filterMap CashuWallet.tokenEventFromDecrypted
                |> List.filter (\token -> Just token.pubKey == model.defaultUser)

        cashuTokens =
            CashuWallet.ingestTokens model.cashuTokens tokens
    in
    ( { model
        | cashuTokens = cashuTokens
        , cashuBalance = CashuWallet.totalBalance cashuTokens
      }
    , Cmd.none
    )


updateModelWithCashuHistory : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithCashuHistory model events =
    let
        ownEvents =
            events
                |> List.filter (\event -> Just event.pubKey == model.defaultUser)

        redeemed =
            Set.union model.redeemedNutzapIds (CashuWallet.redeemedNutzapIdsFromEvents ownEvents)
    in
    ( { model | redeemedNutzapIds = redeemed }, Cmd.none )


updateModelWithNutzapMintRec : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithNutzapMintRec model events =
    let
        recommendations =
            events
                |> List.map CashuWallet.mintRecommendationFromEvent
                |> List.foldl
                    (\rec dict -> Dict.insert rec.pubKey rec dict)
                    model.nutzapMintRecommendations

        modelWithRec =
            { model | nutzapMintRecommendations = recommendations }
    in
    case ( modelWithRec.cashuWallet, modelWithRec.defaultUser ) of
        ( Just wallet, Just userPubKey ) ->
            if List.any (\event -> event.pubKey == userPubKey) events then
                subscribeToNutzaps modelWithRec wallet

            else
                ( modelWithRec, Cmd.none )

        _ ->
            ( modelWithRec, Cmd.none )


updateModelWithNutzaps : Model -> List Event -> ( Model, Cmd Msg )
updateModelWithNutzaps model events =
    let
        nutzaps =
            events
                |> List.filterMap Nutzaps.nutzapFromEvent

        updated =
            Nutzaps.ingest
                { nutzapsAddress = model.nutzapsAddress
                , nutzapsEvents = model.nutzapsEvents
                }
                nutzaps

        modelWithNutzaps =
            { model
                | nutzapsAddress = updated.nutzapsAddress
                , nutzapsEvents = updated.nutzapsEvents
            }
    in
    maybeRedeemNutzaps modelWithNutzaps nutzaps


maybeRedeemNutzaps : Model -> List Nutzap -> ( Model, Cmd Msg )
maybeRedeemNutzaps model nutzaps =
    case ( model.cashuWallet, model.defaultUser ) of
        ( Just wallet, Just userPubKey ) ->
            case Dict.get userPubKey model.nutzapMintRecommendations of
                Just rec ->
                    -- Disabled when 10019 has no pubkey / mints (after Disable).
                    if rec.p2pkPubkey == Nothing || List.isEmpty rec.mints then
                        ( model, Cmd.none )

                    else
                        let
                            trustedMints =
                                rec.mints

                            toRedeem =
                                nutzaps
                                    |> List.filter
                                        (\nutzap ->
                                            not (Set.member nutzap.id model.redeemedNutzapIds)
                                                && not (Set.member nutzap.id model.pendingNutzapRedeems)
                                                && (case nutzap.mintUrl of
                                                        Just mintUrl ->
                                                            List.member mintUrl trustedMints

                                                        Nothing ->
                                                            False
                                                   )
                                        )

                            cmds =
                                toRedeem
                                    |> List.filterMap
                                        (\nutzap ->
                                            nutzap.mintUrl
                                                |> Maybe.map
                                                    (\mintUrl ->
                                                        Ports.redeemNutzap
                                                            { nutzapId = nutzap.id
                                                            , mintUrl = mintUrl
                                                            , proofs = nutzap.proofs
                                                            , p2pkPrivkey = wallet.privkey
                                                            , senderPubKey = Just nutzap.pubKey
                                                            }
                                                    )
                                        )

                            pendingIds =
                                toRedeem
                                    |> List.map .id
                                    |> Set.fromList
                        in
                        ( { model | pendingNutzapRedeems = Set.union model.pendingNutzapRedeems pendingIds }
                        , Cmd.batch cmds
                        )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleNutzapRedeemed : Model -> Incoming.NutzapRedeemed -> ( Model, Cmd Msg )
handleNutzapRedeemed model redeemed =
    case model.defaultUser of
        Nothing ->
            ( { model | pendingNutzapRedeems = Set.remove redeemed.nutzapId model.pendingNutzapRedeems }
            , Cmd.none
            )

        Just userPubKey ->
            let
                now =
                    Time.millisToPosix 0

                tokenEvt =
                    CashuWallet.tokenEvent userPubKey redeemed.mintUrl redeemed.proofs []

                historyEvt =
                    CashuWallet.historyEvent userPubKey
                        { direction = CashuWallet.HistoryIn
                        , amount = redeemed.amount
                        , nutzapEventId = Just redeemed.nutzapId
                        , counterpartPubKey = Maybe.withDefault "" redeemed.senderPubKey
                        , createdTokenEventId = Nothing
                        }

                ( model1, cmd1 ) =
                    send model now (Send.SendCashuTokens tokenEvt)

                ( model2, cmd2 ) =
                    send model1 now (Send.SendCashuHistory historyEvt)
            in
            ( { model2
                | pendingNutzapRedeems = Set.remove redeemed.nutzapId model2.pendingNutzapRedeems
                , redeemedNutzapIds = Set.insert redeemed.nutzapId model2.redeemedNutzapIds
                , cashuBalance = model2.cashuBalance + redeemed.amount
              }
            , Cmd.batch [ cmd1, cmd2 ]
            )


getCashuWallet : Model -> Maybe CashuWallet
getCashuWallet model =
    model.cashuWallet


getCashuBalance : Model -> Int
getCashuBalance model =
    model.cashuBalance


addCashuBalance : Model -> Int -> Model
addCashuBalance model amount =
    { model | cashuBalance = model.cashuBalance + amount }


clearCashuState : Model -> Model
clearCashuState model =
    { model
        | cashuWallet = Nothing
        , cashuTokens = Dict.empty
        , cashuBalance = 0
        , redeemedNutzapIds = Set.empty
        , pendingNutzapRedeems = Set.empty
    }


{-| Clear all logged-in-user session state that must not leak across logout
or account switches. Pubkey-keyed caches (profiles, follows, etc.) are kept.
-}
clearUserSessionState : Model -> Model
clearUserSessionState model =
    let
        cleared =
            clearCashuState model
    in
    { cleared
        | defaultUser = Nothing
        , articleDraftsByDate = []
        , articleDraftsById = Dict.empty
        , articleDraftRelays = Dict.empty
    }


getNutzapMintRecommendation : Model -> Maybe CashuWallet.NutzapMintRecommendation
getNutzapMintRecommendation model =
    model.defaultUser
        |> Maybe.andThen (\pubKey -> Dict.get pubKey model.nutzapMintRecommendations)


getNutzapMintRecommendationFor : Model -> PubKey -> Maybe CashuWallet.NutzapMintRecommendation
getNutzapMintRecommendationFor model pubKey =
    Dict.get pubKey model.nutzapMintRecommendations


getCashuProofsForMint : Model -> String -> ( List CashuWallet.CashuProof, List EventId )
getCashuProofsForMint model mintUrl =
    CashuWallet.proofsAndIdsForMint model.cashuTokens mintUrl


getNutzapsCountForTagReference : Model -> TagReference -> Maybe Int
getNutzapsCountForTagReference model tagReference =
    let
        key =
            tagReferenceToString tagReference

        byAddress =
            Dict.get key model.nutzapsAddress

        byEvent =
            Dict.get key model.nutzapsEvents
    in
    case ( byAddress, byEvent ) of
        ( Just dict, _ ) ->
            Just (Nutzaps.totalAmount dict)

        ( Nothing, Just dict ) ->
            Just (Nutzaps.totalAmount dict)

        ( Nothing, Nothing ) ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    model.hooks.receiveMessage ReceivedMessage
