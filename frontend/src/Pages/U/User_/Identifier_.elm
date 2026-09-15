module Pages.U.User_.Identifier_ exposing (Model, Msg, page)

import Components.InteractionButton as InteractionButton exposing (eventIdOfInteractionObject)
import Components.Interactions as Interactions
import Components.ArticleComments as ArticleComments
import Components.SharingButtonDialog as SharingButtonDialog
import Dict exposing (Dict)
import Effect exposing (Effect)
import Layouts
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Query as Query
import Nostr.Query exposing (ContentQueryStatus(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (EventId, PubKey, loggedInPubKey)
import Page exposing (Page)
import Components.ArticlePage as ArticlePage
import Route exposing (Route)
import Shared
import Shared.Msg
import View exposing (View)


page : Shared.Model -> Route { user : String, identifier : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    ArticlePage.layout shared model.shared (articleFromQuery shared model) msgConfig


msgConfig : ArticlePage.MsgConfig Msg
msgConfig =
    { addLoadedContent = AddLoadedContent
    , articleInteractionsSent = ArticleInteractionsSent
    , commentsSent = CommentsSent
    , sharingButtonDialogMsg = SharingButtonDialogMsg
    , navigateBack = NavigateBack
    , followAuthor = FollowAuthor
    , unfollowAuthor = UnfollowAuthor
    , toggleArticleInfo = ToggleArticleInfo
    , noOp = NoOp
    }



-- INIT


type alias Model =
    { shared : ArticlePage.Model Msg
    , commentInteractions : Dict EventId Interactions.Model
    , identifier : String
    , nip05 : Maybe Nip05.Nip05
    , requestId : Maybe RequestId
    }


init : Shared.Model -> Route { user : String, identifier : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            { shared = ArticlePage.initModel AddLoadedContent
            , commentInteractions = Dict.empty
            , identifier = route.params.identifier
            , nip05 = Nip05.parseNip05 route.params.user
            , requestId = Nothing
            }

        ( requestEffect, requestId ) =
            model.nip05
                |> Maybe.map
                    (\nip05 ->
                        let
                            maybeAuthorsPubKey =
                                Nostr.getPubKeyByNip05 shared.nostr nip05

                            maybeArticle =
                                Nostr.getArticleByNip05AndIdentifier shared.nostr nip05 model.identifier

                            followersEffect =
                                ArticlePage.followersEffectForAuthor shared maybeAuthorsPubKey
                        in
                        case ( maybeArticle, maybeAuthorsPubKey ) of
                            ( Just article, _ ) ->
                                ( ArticlePage.effectsForCachedArticle shared article
                                , Nothing
                                )

                            ( Nothing, Just pubKey ) ->
                                ( Effect.batch
                                    [ followersEffect
                                    , { emptyEventFilter
                                        | authors = Just [ pubKey ]
                                        , kinds = Just [ KindLongFormContent ]
                                        , tagReferences = Just [ TagReferenceIdentifier model.identifier ]
                                      }
                                        |> RequestArticle (Just <| Nostr.getReadRelayUrlsForPubKey shared.nostr pubKey)
                                        |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) []
                                        |> Shared.Msg.RequestNostrEvents
                                        |> Effect.sendSharedMsg
                                    ]
                                , Just <| Nostr.getLastRequestId shared.nostr
                                )

                            ( Nothing, Nothing ) ->
                                ( Effect.batch
                                    [ followersEffect
                                    , RequestNip05AndArticle nip05 model.identifier
                                        |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) [ KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets ]
                                        |> Shared.Msg.RequestNostrEvents
                                        |> Effect.sendSharedMsg
                                    ]
                                , Just <| Nostr.getLastRequestId shared.nostr
                                )
                    )
                |> Maybe.withDefault ( Effect.none, Nothing )
    in
    ( { model | requestId = requestId }
    , Effect.batch
        [ requestEffect
        , ArticlePage.scrollToTopEffect
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | AddLoadedContent String
    | ArticleInteractionsSent InteractionButton.InteractionObject (Interactions.Msg Msg)
    | CommentsSent (ArticleComments.Msg Msg)
    | CommentInteractionsSent InteractionButton.InteractionObject (Interactions.Msg Msg)
    | SharingButtonDialogMsg SharingButtonDialog.Msg
    | FollowAuthor PubKey PubKey
    | UnfollowAuthor PubKey PubKey
    | NavigateBack
    | ToggleArticleInfo


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model
            , ArticlePage.followersEffectForAuthor shared
                (model.nip05 |> Maybe.andThen (Nostr.getPubKeyByNip05 shared.nostr))
            )

        AddLoadedContent url ->
            ( { model | shared = ArticlePage.updateAddLoadedContent url model.shared }, Effect.none )

        ArticleInteractionsSent interactionObject innerMsg ->
            let
                ( sharedModel, effect ) =
                    ArticlePage.updateArticleInteractions shared interactionObject innerMsg model.shared msgConfig
            in
            ( { model | shared = sharedModel }, effect )

        CommentsSent innerMsg ->
            let
                ( sharedModel, effect ) =
                    ArticlePage.updateComments shared innerMsg model.shared msgConfig
            in
            ( { model | shared = sharedModel }, effect )

        CommentInteractionsSent interactionObject innerMsg ->
            case eventIdOfInteractionObject interactionObject of
                Just eventId ->
                    Interactions.update
                        { browserEnv = shared.browserEnv
                        , msg = innerMsg
                        , model = Dict.get eventId model.commentInteractions
                        , nostr = shared.nostr
                        , interactionObject = interactionObject
                        , loginStatus = shared.loginStatus
                        , openCommentMsg = Nothing
                        , toModel = \interactionsModel -> { model | commentInteractions = Dict.insert eventId interactionsModel model.commentInteractions }
                        , toMsg = CommentInteractionsSent interactionObject
                        }

                Nothing ->
                    ( model, Effect.none )

        SharingButtonDialogMsg innerMsg ->
            let
                ( sharedModel, effect ) =
                    ArticlePage.updateSharingDialog shared innerMsg model.shared msgConfig
            in
            ( { model | shared = sharedModel }, effect )

        FollowAuthor pubKeyUser pubKeyToBeFollowed ->
            ( model, ArticlePage.followAuthorEffect pubKeyUser pubKeyToBeFollowed )

        UnfollowAuthor pubKeyUser pubKeyToBeUnfollowed ->
            ( model, ArticlePage.unfollowAuthorEffect pubKeyUser pubKeyToBeUnfollowed )

        NavigateBack ->
            ( model, ArticlePage.navigateBackEffect )

        ToggleArticleInfo ->
            ( model, ArticlePage.toggleArticleInfoEffect )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Sub.batch
        [ ArticlePage.subscriptions shared model.shared (articleFromQuery shared model) msgConfig
        , commentInteractionSubscriptions shared model
        ]


commentInteractionSubscriptions : Shared.Model -> Model -> Sub Msg
commentInteractionSubscriptions shared model =
    let
        maybeAddressComponents =
            articleFromQuery shared model
                |> Maybe.andThen addressComponentsForArticle
    in
    case maybeAddressComponents of
        Just addressComponents ->
            Dict.toList model.commentInteractions
                |> List.map
                    (\( eventId, interactions ) ->
                        let
                            maybePubKey =
                                Nostr.getArticleComments shared.nostr (loggedInPubKey shared.loginStatus) addressComponents
                                    |> List.filter (\articleComment -> articleComment.eventId == eventId)
                                    |> List.head
                                    |> Maybe.map .pubKey
                        in
                        case maybePubKey of
                            Just pubKey ->
                                Interactions.subscriptions interactions
                                    |> Sub.map (CommentInteractionsSent (InteractionButton.Comment eventId pubKey))

                            Nothing ->
                                Sub.none
                    )
                |> Sub.batch

        Nothing ->
            Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        queryStatus =
            articleQueryStatus shared model
    in
    { title = ArticlePage.pageTitle queryStatus "Article"
    , body = [ ArticlePage.viewBody shared model.shared queryStatus msgConfig ]
    }


articleQueryStatus : Shared.Model -> Model -> ContentQueryStatus Article
articleQueryStatus shared model =
    case model.nip05 of
        Just nip05 ->
            Nostr.getArticleQueryStatus shared.nostr nip05 model.identifier model.requestId

        Nothing ->
            ContentQueryFailed "Invalid author address"


articleFromQuery : Shared.Model -> Model -> Maybe Article
articleFromQuery shared model =
    Query.contentFromStatus (articleQueryStatus shared model)
