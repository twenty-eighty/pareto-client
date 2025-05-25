module Pages.U.User_.Identifier_ exposing (Model, Msg, page)

import Components.ArticleInfo as ArticleInfo
import Components.Comment as Comment
import Components.InteractionButton as InteractionButton
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
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Nip18 exposing (articleRepostEvent)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType)
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, PubKey, loggedInPubKey, loggedInSigningPubKey)
import Page exposing (Page)
import Route exposing (Route)
import Set
import Shared
import Shared.Msg
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

        userPubKey =
            loggedInPubKey shared.loginStatus

        articleInfo =
            maybeArticle
                |> Maybe.map
                    (\article ->
                        ArticleInfo.view
                            styles
                            (Nostr.getAuthor shared.nostr article.author)
                            article
                            shared.browserEnv
                            (Nostr.getInteractionsForArticle shared.nostr userPubKey article)
                            shared.nostr
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
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | AddRepost PubKey Article
    | AddLoadedContent String
    | ArticleInteractionsSent InteractionButton.InteractionObject Interactions.Msg
    | OpenComment CommentType
    | CommentSent Comment.Msg
    | CommentInteractionsSent EventId PubKey Interactions.Msg
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

        AddArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithoutArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddArticleReaction userPubKey eventId articlePubKey addressComponents ->
            ( model
            , SendReaction userPubKey eventId articlePubKey (Just addressComponents)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddRepost userPubKey article ->
            let
                relays =
                    article.relays
                        |> Set.toList
                        |> List.append (Nostr.getWriteRelayUrlsForPubKey shared.nostr userPubKey)
            in
            ( model
            , SendRepost relays (articleRepostEvent userPubKey article)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddLoadedContent url ->
            ( { model | loadedContent = LinkPreview.addLoadedContent model.loadedContent url }, Effect.none )

        ArticleInteractionsSent interactionObject innerMsg ->
            Interactions.update
                { msg = innerMsg
                , model = Just model.articleInteractions
                , nostr = shared.nostr
                , interactionObject = interactionObject
                , toModel = \interactionsModel -> { model | articleInteractions = interactionsModel }
                , toMsg = ArticleInteractionsSent interactionObject
                , translations = shared.browserEnv.translations
                }


        OpenComment comment ->
            ( { model | comment = Comment.show model.comment comment }, Effect.none )

        CommentSent innerMsg ->
            Comment.update
                { nostr = shared.nostr
                , msg = innerMsg
                , model = model.comment
                , toModel = \comment -> { model | comment = comment }
                , toMsg = CommentSent
                }

        CommentInteractionsSent eventId pubKey innerMsg ->
            Interactions.update
                { msg = innerMsg
                , model = Dict.get eventId model.commentInteractions
                , nostr = shared.nostr
                , interactionObject = InteractionButton.Comment eventId pubKey
                , toModel = \interactionsModel -> { model | commentInteractions = Dict.insert eventId interactionsModel model.commentInteractions }
                , toMsg = CommentInteractionsSent eventId pubKey
                , translations = shared.browserEnv.translations
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
        [ Sub.map CommentSent (Comment.subscriptions model.comment)
        , commentInteractionSubscriptions shared model
        , articleFromModel shared model
            |> Maybe.andThen (\article ->
                addressComponentsForArticle article
                    |> Maybe.map (\addressComponents ->
                        Sub.map (ArticleInteractionsSent (InteractionButton.Article article.id addressComponents)) (Interactions.subscriptions model.articleInteractions)
                    )
            )
            |> Maybe.withDefault Sub.none
        ]


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
                                |> Maybe.map (\picturePost -> picturePost.pubKey)
                    in
                    case maybePubKey of
                        Just pubKey ->
                            Interactions.subscriptions interactions
                                |> Sub.map (CommentInteractionsSent eventId pubKey)

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
    let
        signingUserPubKey =
            loggedInSigningPubKey shared.loginStatus

        commenting =
            signingUserPubKey
                |> Maybe.andThen (Nostr.getProfile shared.nostr)
                |> Maybe.andThen
                    (\profile ->
                        ( Comment.new
                            { model = model.comment
                            , toMsg = CommentSent
                            , nostr = shared.nostr
                            , profile = profile
                            , loginStatus = shared.loginStatus
                            , browserEnv = shared.browserEnv
                            , theme = shared.theme
                            }
                        , OpenComment
                        )
                            |> Just
                    )
    in
    case maybeArticle of
        Just article ->
            Ui.View.viewArticle
                { theme = shared.theme
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , loginStatus = shared.loginStatus
                , onBookmark = Maybe.map (\pubKey -> ( AddArticleBookmark pubKey, RemoveArticleBookmark pubKey )) signingUserPubKey
                , commenting = commenting
                , onRepost = Maybe.map (\pubKey -> AddRepost pubKey article) signingUserPubKey
                , onReaction = Maybe.map (\pubKey -> AddArticleReaction pubKey) signingUserPubKey

                -- signing is possible also with read-only login
                , onZap = Maybe.map (\pubKey -> ZapReaction pubKey) (loggedInPubKey shared.loginStatus)
                , articleToInteractionsMsg = ArticleInteractionsSent
                , sharing = Just ( model.sharingButtonDialog, SharingButtonDialogMsg )
                }
                (Just model.loadedContent)
                model.articleInteractions
                article

        Nothing ->
            viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId
