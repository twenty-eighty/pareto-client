module Pages.P.Profile_ exposing (Model, Msg, page)

import Components.ArticleComments as ArticleComments
import Components.EmailSubscriptionDialog as EmailSubscriptionDialog
import Components.RelayStatus exposing (Purpose(..))
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (Following(..), PubKey, loggedInSigningPubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Translations.Profile as Translations
import Ui.Profile exposing (FollowType(..), followingProfile)
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..), viewRelayStatus)
import View exposing (View)


page : Shared.Model -> Route { profile : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { pubKey : Maybe PubKey
    , relays : List String
    , requestId : Maybe RequestId
    , emailSubscriptionDialog : EmailSubscriptionDialog.Model
    }


init : Shared.Model -> Route { profile : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        emailSubscriptionDialog =
            case route.hash of
                Just "subscribe" ->
                    EmailSubscriptionDialog.init {}
                        |> EmailSubscriptionDialog.show

                _ ->
                    EmailSubscriptionDialog.init {}

        model =
            decodeParam route.params.profile
                |> Maybe.map
                    (\( pubKey, relays ) ->
                        { pubKey = Just pubKey
                        , relays = relays
                        , requestId = Nothing
                        , emailSubscriptionDialog = emailSubscriptionDialog
                        }
                    )
                |> Maybe.withDefault
                    { pubKey = Nothing
                    , relays = []
                    , requestId = Nothing
                    , emailSubscriptionDialog = emailSubscriptionDialog
                    }

        ( requestProfileEffect, requestId ) =
            model.pubKey
                |> Maybe.map
                    (\pubKey ->
                        case Nostr.getProfile shared.nostr pubKey of
                            Just _ ->
                                ( Effect.none, Nothing )

                            Nothing ->
                                ( filterForAuthor pubKey
                                    |> RequestProfile Nothing
                                    |> Nostr.createRequest shared.nostr "Profile" [ KindLongFormContent ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                , Just <| Nostr.getLastRequestId shared.nostr
                                )
                    )
                |> Maybe.withDefault ( Effect.none, Nothing )

        requestArticlesEffect =
            model.pubKey
                |> Maybe.map (buildRequestArticlesEffect shared.nostr)
                |> Maybe.withDefault Effect.none
    in
    ( { model | requestId = requestId }
    , Effect.batch
        [ requestProfileEffect
        , requestArticlesEffect
        , model.pubKey
            |> Maybe.map Shared.Msg.LoadUserDataByPubKey
            |> Maybe.map Effect.sendSharedMsg
            |> Maybe.withDefault Effect.none
        ]
    )


buildRequestArticlesEffect : Nostr.Model -> PubKey -> Effect Msg
buildRequestArticlesEffect nostr pubKey =
    [ { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just [ pubKey ], limit = Just 20 } ]
        |> RequestArticlesFeed False
        |> Nostr.createRequest nostr "Posts of user" [ KindUserMetadata ]
        |> Shared.Msg.RequestNostrEvents
        |> Effect.sendSharedMsg


filterForAuthor : PubKey -> EventFilter
filterForAuthor author =
    { emptyEventFilter | authors = Just [ author ], kinds = Just [ KindUserMetadata ], limit = Just 1 }


decodeParam : String -> Maybe ( PubKey, List String )
decodeParam profile =
    case Nip19.decode profile of
        Ok (Nip19.NProfile { pubKey, relays }) ->
            Just ( pubKey, relays )

        Ok (Nip19.Npub pubKey) ->
            Just ( pubKey, [] )

        Ok _ ->
            -- unexpected NIP-19 value
            Nothing

        Err _ ->
            Nothing



-- UPDATE


type Msg
    = Follow PubKey PubKey
    | Unfollow PubKey PubKey
    | OpenSubscribeDialog
    | EmailSubscriptionDialogSent (EmailSubscriptionDialog.Msg Msg)
    | NoOp

update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Follow pubKeyUser pubKeyToBeFollowed ->
            ( model
            , SendFollowListWithPubKey pubKeyUser pubKeyToBeFollowed
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        Unfollow pubKeyUser pubKeyToBeUnfollowed ->
            ( model
            , SendFollowListWithoutPubKey pubKeyUser pubKeyToBeUnfollowed
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        OpenSubscribeDialog ->
            ( { model | emailSubscriptionDialog = EmailSubscriptionDialog.show model.emailSubscriptionDialog }
            , Effect.none
            )

        EmailSubscriptionDialogSent innerMsg ->
            EmailSubscriptionDialog.update
                { msg = innerMsg
                , model = model.emailSubscriptionDialog
                , toModel = \emailSubscriptionDialog -> { model | emailSubscriptionDialog = emailSubscriptionDialog }
                , toMsg = EmailSubscriptionDialogSent
                , nostr = shared.nostr
                }

        NoOp ->
            ( model, Effect.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    EmailSubscriptionDialog.subscriptions model.emailSubscriptionDialog
        |> Sub.map EmailSubscriptionDialogSent



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        maybeProfile =
            model.pubKey
                |> Maybe.andThen (Nostr.getProfile shared.nostr)
    in
    { title =
        case ( model.pubKey, maybeProfile ) of
            ( Just pubKey, Just profile ) ->
                Nostr.Profile.profileDisplayName pubKey profile

            ( Just pubKey, Nothing ) ->
                Translations.pubKeyProfilePageTitle [ shared.browserEnv.translations ] ++ " " ++ pubKey

            ( _, _ ) ->
                Translations.defaultProfilePageTitle [ shared.browserEnv.translations ]
    , body =
        [ case ( maybeProfile, model.pubKey ) of
            ( Just profile, _ ) ->
                viewProfile shared model profile

            ( Nothing, Just pubKey ) ->
                viewArticles shared pubKey

            ( Nothing, Nothing ) ->
                viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingProfile model.requestId
        ]
    }


viewProfile : Shared.Model -> Model -> Profile -> Html Msg
viewProfile shared model profile =
    let
        userPubKey =
            loggedInSigningPubKey shared.loginStatus

        sendsNewsletter =
            Nostr.sendsNewsletterPubKey shared.nostr profile.pubKey
                |> Maybe.withDefault False
    in
    div []
        [ Ui.Profile.viewProfile
            profile
            { browserEnv = shared.browserEnv
            , nostr = shared.nostr
            , loginStatus = shared.loginStatus
            , following = followingProfile shared.nostr profile.pubKey Follow Unfollow userPubKey
            , subscribe =
                if sendsNewsletter then
                    Just OpenSubscribeDialog

                else
                    Nothing
            , theme = shared.theme
            , validation =
                Nostr.getProfileValidationStatus shared.nostr profile.pubKey
                    |> Maybe.withDefault ValidationUnknown
            }
        , viewArticles shared profile.pubKey
        , viewEmailSubscriptionDialog shared model profile
        ]


viewArticles : Shared.Model -> PubKey -> Html Msg
viewArticles shared pubKey =
    Nostr.getArticlesForAuthor shared.nostr pubKey
        |> Ui.View.viewArticlePreviews
            ArticlePreviewList
            { articleComments = ArticleComments.init
            , articleToInteractionsMsg = \_ _ -> NoOp
            , commentsToMsg = \_ -> NoOp
            , bookmarkButtonMsg = \_ _ -> NoOp
            , bookmarkButtons = Dict.empty
            , browserEnv = shared.browserEnv
            , deleteButtonMsg = Nothing
            , loginStatus = shared.loginStatus
            , nostr = shared.nostr
            , onLoadMore = Nothing
            , sharing = Nothing
            , theme = shared.theme
            }


viewEmailSubscriptionDialog : Shared.Model -> Model -> Profile -> Html Msg
viewEmailSubscriptionDialog shared model profile =
    EmailSubscriptionDialog.new
        { model = model.emailSubscriptionDialog
        , toMsg = EmailSubscriptionDialogSent
        , nostr = shared.nostr
        , profile = profile
        , loginStatus = shared.loginStatus
        , browserEnv = shared.browserEnv
        , theme = shared.theme
        }
        |> EmailSubscriptionDialog.view
