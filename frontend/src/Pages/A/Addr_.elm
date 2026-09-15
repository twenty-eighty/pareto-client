module Pages.A.Addr_ exposing (..)

import Components.ArticleComments as ArticleComments
import Components.ArticleInfo as ArticleInfo
import Components.AuthorInteractionsBar as AuthorInteractionsBar exposing (Msg(..))
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Components.SharingButtonDialog as SharingButtonDialog
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (div)
import Html.Styled.Attributes exposing (css)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr exposing (ArticleQueryStatus(..))
import Nostr.Article exposing (addressComponentsForArticle)
import Nostr.Event as Event exposing (Kind(..), TagReference(..))
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, loggedInPubKey)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Set
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.ArticlePage as Translations
import Ui.Article exposing (sharingInfoForArticle)
import Ui.ArticleQuery
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (stylesForTheme)
import Ui.View
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

        articlePreviewsData =
            { articleComments = ArticleComments.init
            , articleToInteractionsMsg = ArticleInteractionsSent
            , bookmarkButtonMsg = \_ _ -> NoOp
            , bookmarkButtons = Dict.empty
            , browserEnv = shared.browserEnv
            , commentsToMsg = CommentsSent
            , deleteButtonMsg = Nothing
            , onLoadMore = Nothing
            , nostr = shared.nostr
            , loginStatus = shared.loginStatus
            , sharing = Just ( SharingButtonDialog.init, SharingButtonDialogMsg )
            , theme = shared.theme
            }

        fromAuthorBarMsg : AuthorInteractionsBar.Msg -> Msg
        fromAuthorBarMsg msg =
            case msg of
                NavBack ->
                    NavigateBack

                Follow pubKeyUser pubKeyToFollow ->
                    FollowAuthor pubKeyUser pubKeyToFollow

                Unfollow pubKeyUser pubKeyToUnfollow ->
                    UnfollowAuthor pubKeyUser pubKeyToUnfollow

                AuthorInteractionsBar.ToggleArticleInfo ->
                    ToggleArticleInfo

                _ ->
                    NoOp

        authorInteractionsBar =
            maybeArticle
                |> Maybe.map
                    (\article ->
                        (AuthorInteractionsBar.new
                            { articlePreviewsData = articlePreviewsData
                            , model = AuthorInteractionsBar.init
                            , interactionsModel = Maybe.withDefault Interactions.init interactionsModel
                            , article = article
                            , toMsg = fromAuthorBarMsg
                            }
                            |> AuthorInteractionsBar.view
                        )
                            { articleInfoToggle = False }
                    )
                |> Maybe.withDefault emptyHtml
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withTopPart authorInteractionsBar "64px"
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
    , requestId : Maybe RequestId
    , interactions : Interactions.Model
    , sharingButtonDialog : SharingButtonDialog.Model
    }


init : Shared.Model -> Route { addr : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.addr

        ( model, requestEffect ) =
            case decoded of
                Ok nip19 ->
                    case Nostr.getArticleForNip19 shared.nostr nip19 of
                        Just article ->
                            ( Nip19Model
                                { articleComments = ArticleComments.init
                                , loadedContent =
                                    { loadedUrls = Set.empty
                                    , addLoadedContentFunction = AddLoadedContent
                                    }
                                , nip19 = nip19
                                , requestId = Nothing
                                , interactions = Interactions.init
                                , sharingButtonDialog = SharingButtonDialog.init
                                }
                            , Effect.batch
                                [ Shared.createFollowersEffect shared.nostr (Just article.author)
                                , Shared.createArticleDetailsEffect shared.nostr (Just article)
                                ]
                            )

                        Nothing ->
                            let
                                maybeAuthorsPubKey =
                                    case nip19 of
                                        NAddr { pubKey } ->
                                            Just pubKey

                                        NEvent { author } ->
                                            author

                                        _ ->
                                            Nothing

                                followersEffect =
                                    Shared.createFollowersEffect shared.nostr maybeAuthorsPubKey

                                ( fetchEffect, requestId ) =
                                    case nip19 of
                                        NAddr naddrData ->
                                            ( Event.eventFilterForNaddr naddrData
                                                |> RequestArticle
                                                    (if naddrData.relays /= [] then
                                                        Just naddrData.relays

                                                     else
                                                        Nothing
                                                    )
                                                |> Nostr.createRequest shared.nostr "Article described as NIP-19 NAddr" [ KindUserMetadata ]
                                                |> Shared.Msg.RequestNostrEvents
                                                |> Effect.sendSharedMsg
                                            , Just <| Nostr.getLastRequestId shared.nostr
                                            )

                                        NEvent neventData ->
                                            ( Event.eventFilterForNevent neventData
                                                |> RequestArticle
                                                    (if neventData.relays /= [] then
                                                        Just neventData.relays

                                                     else
                                                        Nothing
                                                    )
                                                |> Nostr.createRequest shared.nostr "Article described as NIP-19 NEvent" [ KindUserMetadata ]
                                                |> Shared.Msg.RequestNostrEvents
                                                |> Effect.sendSharedMsg
                                            , Just <| Nostr.getLastRequestId shared.nostr
                                            )

                                        _ ->
                                            ( Effect.none, Nothing )
                            in
                            case requestId of
                                Just _ ->
                                    ( Nip19Model
                                        { articleComments = ArticleComments.init
                                        , loadedContent =
                                            { loadedUrls = Set.empty
                                            , addLoadedContentFunction = AddLoadedContent
                                            }
                                        , nip19 = nip19
                                        , requestId = requestId
                                        , interactions = Interactions.init
                                        , sharingButtonDialog = SharingButtonDialog.init
                                        }
                                    , Effect.batch [ followersEffect, fetchEffect ]
                                    )

                                Nothing ->
                                    ( ErrorModel "Unsupported NIP-19 address for article view"
                                    , Effect.none
                                    )

                Err error ->
                    ( ErrorModel error, Effect.none )
    in
    ( model
    , Effect.batch
        [ requestEffect

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
    | SharingButtonDialogMsg SharingButtonDialog.Msg
    | FollowAuthor PubKey PubKey
    | UnfollowAuthor PubKey PubKey
    | NavigateBack
    | ToggleArticleInfo
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
                        , loginStatus = shared.loginStatus
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
                        , loginStatus = shared.loginStatus
                        , openCommentMsg = Nothing
                        , toModel = \interactionsModel -> Nip19Model { nip19ModelData | interactions = interactionsModel }
                        , toMsg = ArticleInteractionsSent interactionObject
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

        FollowAuthor pubKeyUser pubKeyToBeFollowed ->
            ( model
            , SendFollowListWithPubKey pubKeyUser pubKeyToBeFollowed
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        UnfollowAuthor pubKeyUser pubKeyToBeUnfollowed ->
            ( model
            , SendFollowListWithoutPubKey pubKeyUser pubKeyToBeUnfollowed
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        NavigateBack ->
            ( model, Effect.back )

        ToggleArticleInfo ->
            ( model, Effect.sendCmd Ports.toggleArticleInfo )

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


viewContent : Shared.Model -> NIP19Type -> ArticleComments.Model -> LoadedContent Msg -> Maybe RequestId -> Interactions.Model -> SharingButtonDialog.Model -> View Msg
viewContent shared nip19 articleComments loadedContent requestId interactions sharingButtonDialog =
    let
        queryStatus =
            Nostr.articleQueryStatusFrom shared.nostr
                (Nostr.getArticleForNip19 shared.nostr nip19)
                requestId

        maybeArticle =
            case queryStatus of
                ArticleQueryReady article ->
                    Just article

                _ ->
                    Nothing
    in
    { title =
        maybeArticle
            |> Maybe.andThen .title
            |> Maybe.withDefault (Translations.defaultPageTitle [ shared.browserEnv.translations ])
    , body =
        [ case queryStatus of
            ArticleQueryReady article ->
                Ui.View.viewArticle
                    { articleComments = articleComments
                    , articleToInteractionsMsg = ArticleInteractionsSent
                    , bookmarkButtonMsg = \_ _ -> NoOp
                    , bookmarkButtons = Dict.empty
                    , browserEnv = shared.browserEnv
                    , commentsToMsg = CommentsSent
                    , deleteButtonMsg = Nothing
                    , loginStatus = shared.loginStatus
                    , nostr = shared.nostr
                    , onLoadMore = Nothing
                    , sharing = Just ( sharingButtonDialog, SharingButtonDialogMsg )
                    , theme = shared.theme
                    }
                    (Just loadedContent)
                    interactions
                    article

            _ ->
                Ui.ArticleQuery.viewStatus shared.theme shared.browserEnv.translations shared.nostr queryStatus
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
