module Nostr.PerformRequest exposing
    ( ModelEffect(..)
    , Env
    , Result
    , perform
    , addressesFromReactionFilter
    )

{-| Dispatch a single `RequestData` to ports / NIP-05.
Model mutation stays in `Nostr` via `ModelEffect`.
-}

import Nostr.Article exposing (Article, firstCreatedAt)
import Nostr.Event exposing (EventFilter, TagReference(..), buildAddress)
import Nostr.External exposing (Hooks)
import Nostr.Nip05 exposing (Nip05)
import Nostr.Request as Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (Address, RelayUrl)
import Set exposing (Set)


type ModelEffect
    = NoModelChange
    | TrackWaitingForContent
    | TrackWaitingForNip05
    | MarkArticleDetailsRequested (Set Address)
    | ClearArticlesByDate
    | SetArticlesByDate (List Article)
    | ClearArticleDrafts
    | ClearPicturePosts


type alias Env msg =
    { hooks : Hooks msg
    , configuredRelays : List RelayUrl
    , applicationDataRelays : List RelayUrl
    , searchRelayUrls : List RelayUrl
    , delayedPublishingRelays : List RelayUrl
    , articlesByDate : List Article
    , requestNip05 : RequestId -> Nip05 -> Cmd msg
    }


type alias Result msg =
    { modelEffect : ModelEffect
    , cmd : Cmd msg
    }


perform : Env msg -> String -> RequestId -> RequestData -> Result msg
perform env description requestId requestData =
    let
        configuredRelays =
            env.configuredRelays

        requestEvents closeOnEose relays filters =
            env.hooks.requestEvents description closeOnEose requestId relays filters
    in
    case requestData of
        RequestArticle relays eventFilter ->
            { modelEffect = TrackWaitingForContent
            , cmd = requestEvents True (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ]
            }

        RequestArticles eventFilters ->
            { modelEffect = ClearArticlesByDate
            , cmd = requestEvents True configuredRelays eventFilters
            }

        RequestArticlesFeed loadMore eventFilters ->
            let
                ( until, articlesByDate ) =
                    if loadMore then
                        ( firstCreatedAt env.articlesByDate, env.articlesByDate )

                    else
                        ( Nothing, [] )
            in
            { modelEffect = SetArticlesByDate articlesByDate
            , cmd = requestEvents False configuredRelays (Request.eventFiltersWithUntil eventFilters until)
            }

        RequestArticleDrafts eventFilters ->
            { modelEffect = ClearArticleDrafts
            , cmd = requestEvents False configuredRelays eventFilters
            }

        RequestBookmarks eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True configuredRelays [ eventFilter ]
            }

        RequestCommunity relays eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ]
            }

        RequestDeletionRequests eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True configuredRelays [ eventFilter ]
            }

        RequestFollowSets eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True configuredRelays [ eventFilter ]
            }

        RequestFutureArticles eventFilters ->
            { modelEffect = ClearArticlesByDate
            , cmd = requestEvents True env.delayedPublishingRelays eventFilters
            }

        RequestMediaServerLists eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True configuredRelays [ eventFilter ]
            }

        RequestNip05AndArticle nip05 _ ->
            { modelEffect = TrackWaitingForNip05
            , cmd = env.requestNip05 requestId nip05
            }

        RequestPicturesFeed eventFilters ->
            { modelEffect = ClearPicturePosts
            , cmd = requestEvents False configuredRelays eventFilters
            }

        RequestProfile relays eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ]
            }

        RequestProfileByNip05 nip05 ->
            { modelEffect = NoModelChange
            , cmd = env.requestNip05 requestId nip05
            }

        RequestReactions eventFilter ->
            { modelEffect = MarkArticleDetailsRequested (addressesFromReactionFilter eventFilter)
            , cmd = requestEvents False configuredRelays [ eventFilter ]
            }

        RequestRelayLists eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents False configuredRelays [ eventFilter ]
            }

        RequestSubscribers eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents False env.applicationDataRelays [ eventFilter ]
            }

        RequestUserData eventFilter ->
            { modelEffect = NoModelChange
            , cmd = requestEvents True configuredRelays [ eventFilter ]
            }

        RequestBlossomAuth serverUrl content method ->
            { modelEffect = NoModelChange
            , cmd = env.hooks.requestBlossomAuth requestId serverUrl content method
            }

        RequestNip98Auth serverUrl apiUrl content method ->
            { modelEffect = NoModelChange
            , cmd = env.hooks.requestNip96Auth requestId serverUrl apiUrl content method
            }

        RequestSearchResults eventFilters ->
            { modelEffect = ClearArticlesByDate
            , cmd = env.hooks.searchEvents description True requestId env.searchRelayUrls eventFilters
            }

        RequestShortNote relays eventFilter ->
            { modelEffect = TrackWaitingForContent
            , cmd = requestEvents True (Maybe.withDefault [] relays ++ configuredRelays) [ eventFilter ]
            }


addressesFromReactionFilter : EventFilter -> Set Address
addressesFromReactionFilter eventFilter =
    eventFilter.tagReferences
        |> Maybe.withDefault []
        |> List.filterMap
            (\tagRef ->
                case tagRef of
                    TagReferenceCode addressComponents ->
                        Just (buildAddress addressComponents)

                    _ ->
                        Nothing
            )
        |> Set.fromList
