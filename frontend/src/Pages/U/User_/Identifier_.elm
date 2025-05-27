module Pages.U.User_.Identifier_ exposing (Model, Msg, page)

import Components.ArticleComments as ArticleComments
import Components.ArticleInfo as ArticleInfo
import Components.Comment as Comment
import Components.InteractionButton as InteractionButton exposing (eventIdOfInteractionObject)
import Components.Interactions as Interactions
import Components.RelayStatus exposing (Purpose(..))
import Components.SharingButtonDialog as SharingButtonDialog
import Components.ZapDialog as ZapDialog
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Set
import Shared
import Shared.Msg
import Ui.Article exposing (sharingInfoForArticle)
import Ui.Interactions exposing (extendedZapRelays)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles
import Ui.View exposing (viewRelayStatus)
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
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme

        maybeArticle =
            articleFromModel shared model

        articleInfo =
            maybeArticle
                |> Maybe.map (\article ->
                    addressComponentsForArticle article
                        |> Maybe.map (\addressComponents ->
                            let
                                interactionObject =
                                    InteractionButton.Article article.id addressComponents
                            in
                            ArticleInfo.view
                                styles
                                (Nostr.getAuthor shared.nostr article.author)
                                article
                                { browserEnv = shared.browserEnv
                                , model = Just model.articleInteractions
                                , toMsg = ArticleInteractionsSent interactionObject
                                , theme = shared.theme
                                , interactionObject = interactionObject
                                , nostr = shared.nostr
                                , loginStatus = shared.loginStatus
                                , shareInfo = sharingInfoForArticle article (Nostr.getAuthor shared.nostr article.author)
                                , zapRelays = article.relays
                                }
                        )
                        |> Maybe.withDefault emptyHtml
                ) 
                |> Maybe.withDefault emptyHtml
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withLeftPart articleInfo
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { loadedContent : LoadedContent Msg
    , comment : Comment.Model
    , commentInteractions : Dict EventId Interactions.Model
    , articleComments : ArticleComments.Model
    , articleInteractions : Interactions.Model
    , identifier : String
    , nip05 : Maybe Nip05.Nip05
    , requestId : Maybe RequestId
    , zapDialog : ZapDialog.Model
    , sharingButtonDialog : SharingButtonDialog.Model
    }


init : Shared.Model -> Route { user : String, identifier : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            { identifier = route.params.identifier
            , comment = Comment.init {}
            , commentInteractions = Dict.empty
            , articleComments = ArticleComments.init
            , articleInteractions = Interactions.init
            , nip05 = Nip05.parseNip05 route.params.user
            , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
            , requestId = Nothing
            , zapDialog = ZapDialog.init {}
            , sharingButtonDialog = SharingButtonDialog.init
            }

        ( requestEffect, requestId ) =
            model.nip05
                |> Maybe.map
                    (\nip05 ->
                        let
                            maybeAuthorsPubKey =
                                Nostr.getPubKeyByNip05 shared.nostr nip05

                            followersEffect =
                                Shared.createFollowersEffect shared.nostr maybeAuthorsPubKey
                        in
                        case maybeAuthorsPubKey of
                            Just pubKey ->
                                case Nostr.getArticleWithIdentifier shared.nostr pubKey model.identifier of
                                    Just _ ->
                                        -- article already loaded, accessible in view function
                                        ( followersEffect, Nothing )

                                    -- |> Debug.log "User Page -> init: article loaded"
                                    Nothing ->
                                        ( Effect.batch
                                            [ followersEffect
                                            , -- pubkey already loaded, request article
                                              { emptyEventFilter
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

                            --|> Debug.log "User Page -> init: no article"
                            Nothing ->
                                ( Effect.batch
                                    [ followersEffect
                                    , RequestNip05AndArticle nip05 model.identifier
                                        |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) [ KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets ]
                                        |> Shared.Msg.RequestNostrEvents
                                        |> Effect.sendSharedMsg
                                    ]
                                , Just <| Nostr.getLastRequestId shared.nostr
                                )
                     --|> Debug.log "User Page -> init: Fetching PubKey and article..."
                    )
                |> Maybe.withDefault ( Effect.none, Nothing )
    in
    ( { model | requestId = requestId }
    , Effect.batch
        [ requestEffect

        -- jump to top of article
        , Effect.scrollContentToTop
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | AddLoadedContent String
    | ArticleInteractionsSent InteractionButton.InteractionObject (Interactions.Msg Msg)
    | CommentsSent (ArticleComments.Msg Msg)
    | CommentInteractionsSent InteractionButton.InteractionObject (Interactions.Msg Msg)
    | ZapReaction PubKey (List ZapDialog.Recipient)
    | ZapDialogSent (ZapDialog.Msg Msg)
    | SharingButtonDialogMsg SharingButtonDialog.Msg


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            let
                maybeAuthorsPubKey =
                    model.nip05 |> Maybe.andThen (Nostr.getPubKeyByNip05 shared.nostr)

                followersEffect =
                    Shared.createFollowersEffect shared.nostr maybeAuthorsPubKey
            in
            ( model, followersEffect )

        AddLoadedContent url ->
            ( { model | loadedContent = LinkPreview.addLoadedContent model.loadedContent url }, Effect.none )

        ArticleInteractionsSent interactionObject innerMsg ->
            Interactions.update
                { browserEnv = shared.browserEnv
                , msg = innerMsg
                , model = Just model.articleInteractions
                , nostr = shared.nostr
                , interactionObject = interactionObject
                , openCommentMsg = Nothing
                , toModel = \interactionsModel -> { model | articleInteractions = interactionsModel }
                , toMsg = ArticleInteractionsSent interactionObject
                }

        CommentsSent innerMsg ->
            ArticleComments.update
                { browserEnv = shared.browserEnv
                , msg = innerMsg
                , model = model.articleComments
                , nostr = shared.nostr
                , toModel = \articleComments -> { model | articleComments = articleComments }
                , toMsg = CommentsSent
                , translations = shared.browserEnv.translations
                }

        CommentInteractionsSent interactionObject innerMsg ->
            let
                eventId =
                    eventIdOfInteractionObject interactionObject
            in
            Interactions.update
                { browserEnv = shared.browserEnv
                , msg = innerMsg
                , model = Dict.get eventId model.commentInteractions
                , nostr = shared.nostr
                , interactionObject = interactionObject
                , openCommentMsg = Nothing
                , toModel = \interactionsModel -> { model | commentInteractions = Dict.insert eventId interactionsModel model.commentInteractions }
                , toMsg = CommentInteractionsSent interactionObject
                }

        ZapReaction _ recipients ->
            showZapDialog model recipients

        ZapDialogSent innerMsg ->
            ZapDialog.update
                { nostr = shared.nostr
                , msg = innerMsg
                , model = model.zapDialog
                , toModel = \zapDialog -> { model | zapDialog = zapDialog }
                , toMsg = ZapDialogSent
                }

        SharingButtonDialogMsg innerMsg ->
            SharingButtonDialog.update
                { browserEnv = shared.browserEnv
                , model = model.sharingButtonDialog
                , msg = innerMsg
                , toModel = \sharingButtonDialog -> { model | sharingButtonDialog = sharingButtonDialog }
                , toMsg = SharingButtonDialogMsg
                }


showZapDialog : Model -> List ZapDialog.Recipient -> ( Model, Effect Msg )
showZapDialog model recipients =
    let
        ( zapDialogModel, effect ) =
            ZapDialog.show model.zapDialog ZapDialogSent recipients
    in
    ( { model | zapDialog = zapDialogModel }
    , effect
    )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Sub.batch
        [ commentInteractionSubscriptions shared model
        , articleFromModel shared model
            |> Maybe.andThen (\article ->
                addressComponentsForArticle article
                    |> Maybe.map (\addressComponents ->
                        Sub.map (ArticleInteractionsSent (InteractionButton.Article article.id addressComponents)) (Interactions.subscriptions model.articleInteractions)
                    )
            )
            |> Maybe.withDefault Sub.none
        , articleCommentsSubscriptions shared model
        ]

articleCommentsSubscriptions : Shared.Model -> Model -> Sub Msg
articleCommentsSubscriptions shared model =
    let
        articleComments =
            articleFromModel shared model
                |> Maybe.andThen addressComponentsForArticle
                |> Maybe.map (Nostr.getArticleComments shared.nostr)
                |> Maybe.withDefault []
    in
    ArticleComments.subscriptions model.articleComments articleComments
        |> Sub.map CommentsSent


commentInteractionSubscriptions : Shared.Model -> Model -> Sub Msg
commentInteractionSubscriptions shared model =
    let
        maybeAddressComponents =
            articleFromModel shared model
                |> Maybe.andThen addressComponentsForArticle
    in
    case maybeAddressComponents of
        Just addressComponents ->
            Dict.toList model.commentInteractions
                |> List.map (\(eventId, interactions) ->
                    let
                        maybePubKey =
                            Nostr.getArticleComments shared.nostr addressComponents
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
        maybeArticle =
            articleFromModel shared model
    in
    { title = maybeArticle |> Maybe.andThen .title |> Maybe.withDefault "Article"
    , body = [ viewArticle shared model maybeArticle ]
    }


articleFromModel : Shared.Model -> Model -> Maybe Article
articleFromModel shared model =
    model.nip05
        |> Maybe.andThen (Nostr.getPubKeyByNip05 shared.nostr)
        |> Maybe.andThen (\pubKey -> Nostr.getArticleWithIdentifier shared.nostr pubKey model.identifier)


viewArticle : Shared.Model -> Model -> Maybe Article -> Html Msg
viewArticle shared model maybeArticle =
    case maybeArticle of
        Just article ->
            Ui.View.viewArticle
                { articleComments = model.articleComments
                , articleToInteractionsMsg = ArticleInteractionsSent
                , bookmarkButtonMsg = \_ _ -> NoOp
                , bookmarkButtons = Dict.empty
                , browserEnv = shared.browserEnv
                , commentsToMsg = CommentsSent
                , nostr = shared.nostr
                , loginStatus = shared.loginStatus
                , sharing = Just ( model.sharingButtonDialog, SharingButtonDialogMsg )
                , theme = shared.theme
                }
                (Just model.loadedContent)
                model.articleInteractions
                article

        Nothing ->
            viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId

