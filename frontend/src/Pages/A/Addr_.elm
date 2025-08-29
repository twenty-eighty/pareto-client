module Pages.A.Addr_ exposing (..)

import Components.ArticleComments as ArticleComments
import Components.ArticleInfo as ArticleInfo
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Components.RelayStatus exposing (Purpose(..))
import Components.SharingButtonDialog as SharingButtonDialog
import Components.ZapDialog as ZapDialog
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (div)
import Html.Styled.Attributes exposing (css)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (addressComponentsForArticle)
import Nostr.Event as Event exposing (Kind(..), TagReference(..))
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, loggedInPubKey)
import Page exposing (Page)
import Route exposing (Route)
import Set
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.ArticlePage as Translations
import Ui.Article exposing (sharingInfoForArticle)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (stylesForTheme)
import Ui.View exposing (viewRelayStatus)
import Url
import View exposing (View)


page : Shared.Model -> Route { addr : String } -> Page Model Msg
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

        ( maybeArticle, interactionsModel ) =
            case model of
                Nip19Model { nip19, interactions } ->
                    ( Nostr.getArticleForNip19 shared.nostr nip19, Just interactions )

                ErrorModel _ ->
                    ( Nothing, Nothing )

        articleInfo =
            maybeArticle
                |> Maybe.map
                    (\article ->
                        addressComponentsForArticle article
                            |> Maybe.map
                                (\addressComponents ->
                                    let
                                        interactionObject =
                                            InteractionButton.Article article.id addressComponents
                                    in
                                    ArticleInfo.view
                                        styles
                                        (Nostr.getAuthor shared.nostr article.author)
                                        article
                                        { browserEnv = shared.browserEnv
                                        , model = interactionsModel
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
        |> Layouts.Sidebar.withRightPart articleInfo
        |> Layouts.Sidebar



-- INIT


type Model
    = Nip19Model Nip19ModelData
    | ErrorModel String


type alias Nip19ModelData =
    { loadedContent : LoadedContent Msg
    , articleComments : ArticleComments.Model
    , nip19 : NIP19Type
    , requestId : RequestId
    , interactions : Interactions.Model
    , sharingButtonDialog : SharingButtonDialog.Model
    , zapDialog : ZapDialog.Model
    }


init : Shared.Model -> Route { addr : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.addr

        ( model, maybeArticle ) =
            case decoded of
                Ok nip19 ->
                    ( Nip19Model
                        { articleComments = ArticleComments.init
                        , loadedContent =
                            { loadedUrls = Set.empty
                            , addLoadedContentFunction = AddLoadedContent
                            }
                        , nip19 = nip19
                        , requestId = Nostr.getLastRequestId shared.nostr
                        , zapDialog = ZapDialog.init {}
                        , interactions = Interactions.init
                        , sharingButtonDialog = SharingButtonDialog.init
                        }
                    , Nostr.getArticleForNip19 shared.nostr nip19
                    )

                Err error ->
                    ( ErrorModel error, Nothing )

        effect =
            case ( maybeArticle, model ) of
                ( Nothing, Nip19Model { nip19 } ) ->
                    let
                        maybeAuthorsPubKey =
                            Nostr.getArticleForNip19 shared.nostr nip19 |> Maybe.map .author

                        followersEffect =
                            Shared.createFollowersEffect shared.nostr maybeAuthorsPubKey
                    in
                    -- article not loaded yet, request it now
                    case nip19 of
                        NAddr naddrData ->
                            Effect.batch
                                [ followersEffect
                                , Event.eventFilterForNaddr naddrData
                                    |> RequestArticle
                                        (if naddrData.relays /= [] then
                                            Just naddrData.relays

                                         else
                                            Nothing
                                        )
                                    |> Nostr.createRequest shared.nostr "Article described as NIP-19 NAddr" [ KindUserMetadata ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                ]

                        NEvent neventData ->
                            Effect.batch
                                [ followersEffect
                                , Event.eventFilterForNevent neventData
                                    |> RequestArticle
                                        (if neventData.relays /= [] then
                                            Just neventData.relays

                                         else
                                            Nothing
                                        )
                                    |> Nostr.createRequest shared.nostr "Article described as NIP-19 NEvent" [ KindUserMetadata ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                ]

                        _ ->
                            Effect.none

                ( _, _ ) ->
                    Effect.none
    in
    ( model
    , Effect.batch
        [ effect

        -- jump to top of article
        , Effect.scrollContentToTop
        ]
    )


decodedTagParam : String -> Maybe (List String)
decodedTagParam tag =
    Url.percentDecode tag
        |> Maybe.map List.singleton



-- UPDATE


type Msg
    = AddLoadedContent String
    | CommentsSent (ArticleComments.Msg Msg)
    | ArticleInteractionsSent InteractionButton.InteractionObject (Interactions.Msg Msg)
    | ZapReaction PubKey (List ZapDialog.Recipient)
    | ZapDialogSent (ZapDialog.Msg Msg)
    | SharingButtonDialogMsg SharingButtonDialog.Msg
    | NoOp


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        AddLoadedContent url ->
            case model of
                Nip19Model nip19ModelData ->
                    ( Nip19Model { nip19ModelData | loadedContent = LinkPreview.addLoadedContent nip19ModelData.loadedContent url }, Effect.none )

                _ ->
                    ( model, Effect.none )

        CommentsSent innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    ArticleComments.update
                        { browserEnv = shared.browserEnv
                        , msg = innerMsg
                        , model = nip19ModelData.articleComments
                        , nostr = shared.nostr
                        , toModel = \articleComments -> Nip19Model { nip19ModelData | articleComments = articleComments }
                        , toMsg = CommentsSent
                        , translations = shared.browserEnv.translations
                        }

                _ ->
                    ( model, Effect.none )

        ArticleInteractionsSent interactionObject innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    Interactions.update
                        { browserEnv = shared.browserEnv
                        , msg = innerMsg
                        , model = Just nip19ModelData.interactions
                        , nostr = shared.nostr
                        , interactionObject = interactionObject
                        , openCommentMsg = Nothing
                        , toModel = \interactionsModel -> Nip19Model { nip19ModelData | interactions = interactionsModel }
                        , toMsg = ArticleInteractionsSent interactionObject
                        }

                _ ->
                    ( model, Effect.none )

        ZapReaction _ recipients ->
            case model of
                Nip19Model nip19ModelData ->
                    showZapDialog nip19ModelData recipients

                _ ->
                    ( model, Effect.none )

        ZapDialogSent innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    ZapDialog.update
                        { nostr = shared.nostr
                        , msg = innerMsg
                        , model = nip19ModelData.zapDialog
                        , toModel = \zapDialog -> Nip19Model { nip19ModelData | zapDialog = zapDialog }
                        , toMsg = ZapDialogSent
                        }

                _ ->
                    ( model, Effect.none )

        SharingButtonDialogMsg innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    SharingButtonDialog.update
                        { browserEnv = shared.browserEnv
                        , model = nip19ModelData.sharingButtonDialog
                        , msg = innerMsg
                        , toModel = \sharingButtonDialog -> Nip19Model { nip19ModelData | sharingButtonDialog = sharingButtonDialog }
                        , toMsg = SharingButtonDialogMsg
                        }

                _ ->
                    ( model, Effect.none )

        NoOp ->
            let
                maybeAuthorPubKey =
                    case model of
                        Nip19Model nip19ModelData ->
                            Nostr.getArticleForNip19 shared.nostr nip19ModelData.nip19 |> Maybe.map .author

                        _ ->
                            Nothing

                followersEffect =
                    Shared.createFollowersEffect shared.nostr maybeAuthorPubKey
            in
            ( model, followersEffect )


showZapDialog : Nip19ModelData -> List ZapDialog.Recipient -> ( Model, Effect Msg )
showZapDialog nip19ModelData recipients =
    let
        ( zapDialogModel, effect ) =
            ZapDialog.show nip19ModelData.zapDialog ZapDialogSent recipients
    in
    ( Nip19Model { nip19ModelData | zapDialog = zapDialogModel }
    , effect
    )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    case model of
        Nip19Model nip19ModelData ->
            let
                articleComments =
                    Nostr.getArticleForNip19 shared.nostr nip19ModelData.nip19
                        |> Maybe.andThen addressComponentsForArticle
                        |> Maybe.map (Nostr.getArticleComments shared.nostr (loggedInPubKey shared.loginStatus))
                        |> Maybe.withDefault []
            in
            Sub.batch
                [ ArticleComments.subscriptions nip19ModelData.articleComments articleComments |> Sub.map CommentsSent
                , Nostr.getArticleForNip19 shared.nostr nip19ModelData.nip19
                    |> Maybe.andThen
                        (\article ->
                            addressComponentsForArticle article
                                |> Maybe.map
                                    (\addressComponents ->
                                        Sub.map (ArticleInteractionsSent (InteractionButton.Article article.id addressComponents)) (Interactions.subscriptions nip19ModelData.interactions)
                                    )
                        )
                    |> Maybe.withDefault Sub.none
                ]

        _ ->
            Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    case model of
        Nip19Model { articleComments, loadedContent, nip19, requestId, interactions, sharingButtonDialog } ->
            viewContent shared nip19 articleComments loadedContent requestId interactions sharingButtonDialog

        ErrorModel error ->
            viewError shared error


viewContent : Shared.Model -> NIP19Type -> ArticleComments.Model -> LoadedContent Msg -> RequestId -> Interactions.Model -> SharingButtonDialog.Model -> View Msg
viewContent shared nip19 articleComments loadedContent requestId interactions sharingButtonDialog =
    let
        maybeArticle =
            Nostr.getArticleForNip19 shared.nostr nip19
    in
    { title =
        maybeArticle
            |> Maybe.andThen .title
            |> Maybe.withDefault (Translations.defaultPageTitle [ shared.browserEnv.translations ])
    , body =
        [ maybeArticle
            |> Maybe.map
                (\article ->
                    Ui.View.viewArticle
                        { articleComments = articleComments
                        , articleToInteractionsMsg = ArticleInteractionsSent
                        , bookmarkButtonMsg = \_ _ -> NoOp
                        , bookmarkButtons = Dict.empty
                        , browserEnv = shared.browserEnv
                        , commentsToMsg = CommentsSent
                        , loginStatus = shared.loginStatus
                        , nostr = shared.nostr
                        , onLoadMore = Nothing
                        , sharing = Just ( sharingButtonDialog, SharingButtonDialogMsg )
                        , theme = shared.theme
                        }
                        (Just loadedContent)
                        interactions
                        article
                )
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle (Just requestId))
        ]
    }


viewError : Shared.Model -> String -> View Msg
viewError shared error =
    let
        styles =
            stylesForTheme shared.theme
    in
    { title = Translations.defaultPageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            (styles.colorStyleGrayscaleTitle
                ++ styles.textStyleH3
                ++ [ css
                        [ Tw.m_4
                        ]
                   ]
            )
            [ Html.text <| "Error loading content: " ++ error
            ]
        ]
    }
