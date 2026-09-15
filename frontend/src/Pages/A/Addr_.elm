module Pages.A.Addr_ exposing (..)

import Components.ArticleComments as ArticleComments
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Components.SharingButtonDialog as SharingButtonDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (div)
import Html.Styled.Attributes exposing (css)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event as Event exposing (Kind(..))
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Query exposing (ContentQueryStatus(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Components.ArticlePage as ArticlePage
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.ArticlePage as Translations
import Ui.Styles exposing (stylesForTheme)
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
    case model of
        Nip19Model data ->
            ArticlePage.layout shared
                data.shared
                (Nostr.getArticleForNip19 shared.nostr data.nip19)
                msgConfig

        ErrorModel _ ->
            Layouts.Sidebar.new { theme = shared.theme }
                |> Layouts.Sidebar


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


type Model
    = Nip19Model Nip19ModelData
    | ErrorModel String


type alias Nip19ModelData =
    { shared : ArticlePage.Model Msg
    , nip19 : NIP19Type
    , requestId : Maybe RequestId
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
                                { shared = ArticlePage.initModel AddLoadedContent
                                , nip19 = nip19
                                , requestId = Nothing
                                }
                            , ArticlePage.effectsForCachedArticle shared article
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
                                    ArticlePage.followersEffectForAuthor shared maybeAuthorsPubKey

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
                                        { shared = ArticlePage.initModel AddLoadedContent
                                        , nip19 = nip19
                                        , requestId = requestId
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
        , ArticlePage.scrollToTopEffect
        ]
    )



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
    case model of
        ErrorModel _ ->
            ( model, Effect.none )

        Nip19Model data ->
            case msg of
                AddLoadedContent url ->
                    ( Nip19Model { data | shared = ArticlePage.updateAddLoadedContent url data.shared }
                    , Effect.none
                    )

                CommentsSent innerMsg ->
                    let
                        ( sharedModel, effect ) =
                            ArticlePage.updateComments shared innerMsg data.shared msgConfig
                    in
                    ( Nip19Model { data | shared = sharedModel }, effect )

                ArticleInteractionsSent interactionObject innerMsg ->
                    let
                        ( sharedModel, effect ) =
                            ArticlePage.updateArticleInteractions shared interactionObject innerMsg data.shared msgConfig
                    in
                    ( Nip19Model { data | shared = sharedModel }, effect )

                SharingButtonDialogMsg innerMsg ->
                    let
                        ( sharedModel, effect ) =
                            ArticlePage.updateSharingDialog shared innerMsg data.shared msgConfig
                    in
                    ( Nip19Model { data | shared = sharedModel }, effect )

                FollowAuthor pubKeyUser pubKeyToBeFollowed ->
                    ( model, ArticlePage.followAuthorEffect pubKeyUser pubKeyToBeFollowed )

                UnfollowAuthor pubKeyUser pubKeyToBeUnfollowed ->
                    ( model, ArticlePage.unfollowAuthorEffect pubKeyUser pubKeyToBeUnfollowed )

                NavigateBack ->
                    ( model, ArticlePage.navigateBackEffect )

                ToggleArticleInfo ->
                    ( model, ArticlePage.toggleArticleInfoEffect )

                NoOp ->
                    ( model
                    , ArticlePage.followersEffectForAuthor shared
                        (Nostr.getArticleForNip19 shared.nostr data.nip19 |> Maybe.map .author)
                    )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    case model of
        Nip19Model data ->
            ArticlePage.subscriptions shared
                data.shared
                (Nostr.getArticleForNip19 shared.nostr data.nip19)
                msgConfig

        ErrorModel _ ->
            Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    case model of
        Nip19Model data ->
            let
                queryStatus =
                    Nostr.articleQueryStatusFrom shared.nostr
                        (Nostr.getArticleForNip19 shared.nostr data.nip19)
                        data.requestId
            in
            { title =
                ArticlePage.pageTitle queryStatus
                    (Translations.defaultPageTitle [ shared.browserEnv.translations ])
            , body = [ ArticlePage.viewBody shared data.shared queryStatus msgConfig ]
            }

        ErrorModel error ->
            viewError shared error


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
                ++ [ css [ Tw.m_4 ] ]
            )
            [ Html.text <| "Error loading content: " ++ error
            ]
        ]
    }
