module Pages.U.User_ exposing (Model, Msg, page)

import Components.EmailSubscriptionDialog as EmailSubscriptionDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05 exposing (Nip05, nip05ToString)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (Following(..), PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Ui.Profile exposing (FollowType(..))
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Shared.Model -> Route { user : String } -> Page Model Msg
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
        { styles = Ui.Styles.stylesForTheme theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { nip05 : Maybe Nip05
    , emailSubscriptionDialog : EmailSubscriptionDialog.Model
    }


init : Shared.Model -> Route { user : String } -> () -> ( Model, Effect Msg )
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
            { nip05 = Nip05.parseNip05 route.params.user
            , emailSubscriptionDialog = emailSubscriptionDialog
            }

        requestEffect =
            model.nip05
                |> Maybe.map
                    (\nip05 ->
                        case Nostr.getPubKeyByNip05 shared.nostr nip05 of
                            Just pubKey ->
                                -- already validated, don't do again
                                [ buildRequestArticlesEffect shared.nostr pubKey

                                -- check if author offers newsletter
                                , Effect.sendSharedMsg (Shared.Msg.LoadUserDataByPubKey pubKey)
                                ]
                                    |> Effect.batch

                            Nothing ->
                                [ RequestProfileByNip05 nip05
                                    |> Nostr.createRequest shared.nostr ("Profile and data of NIP-05 user " ++ nip05ToString nip05) [ KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg

                                -- check if author offers newsletter
                                , Effect.sendSharedMsg (Shared.Msg.LoadUserDataByNip05 nip05)
                                ]
                                    |> Effect.batch
                    )
                |> Maybe.withDefault Effect.none
    in
    ( model
    , requestEffect
    )


buildRequestArticlesEffect : Nostr.Model -> PubKey -> Effect Msg
buildRequestArticlesEffect nostr pubKey =
    [ { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just [ pubKey ], limit = Just 20 } ]
        |> RequestArticlesFeed
        |> Nostr.createRequest nostr "Posts of user" [ KindUserMetadata ]
        |> Shared.Msg.RequestNostrEvents
        |> Effect.sendSharedMsg



-- UPDATE


type Msg
    = Follow PubKey PubKey
    | Unfollow PubKey PubKey
    | OpenSubscribeDialog
    | EmailSubscriptionDialogSent (EmailSubscriptionDialog.Msg Msg)


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
            model.nip05
                |> Maybe.andThen
                    (\nip05 ->
                        Nostr.getProfileByNip05 shared.nostr nip05
                    )
    in
    { title = "Profile"
    , body =
        [ case maybeProfile of
            Just profile ->
                viewProfile shared model profile

            Nothing ->
                emptyHtml
        ]
    }


viewProfile : Shared.Model -> Model -> Profile -> Html Msg
viewProfile shared model profile =
    let
        userPubKey =
            Shared.loggedInSigningPubKey shared.loginStatus

        sendsNewsletter =
            model.nip05
                |> Maybe.andThen (Nostr.sendsNewsletterNip05 shared.nostr)
                |> Maybe.withDefault False
    in
    div []
        [ Ui.Profile.viewProfile
            profile
            { browserEnv = shared.browserEnv
            , nostr = shared.nostr
            , loginStatus = shared.loginStatus
            , following = followingProfile shared.nostr profile.pubKey userPubKey
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
        , Nostr.getArticlesForAuthor shared.nostr profile.pubKey
            |> Ui.View.viewArticlePreviews
                ArticlePreviewList
                { theme = shared.theme
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , userPubKey = Shared.loggedInPubKey shared.loginStatus
                , onBookmark = Nothing
                , commenting = Nothing
                , onReaction = Nothing
                , onRepost = Nothing
                , onZap = Nothing
                , sharing = Nothing
                }
        , viewEmailSubscriptionDialog shared model profile
        ]


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


followingProfile : Nostr.Model -> PubKey -> Maybe PubKey -> FollowType Msg
followingProfile nostr profilePubKey maybePubKey =
    case maybePubKey of
        Just userPubKey ->
            Nostr.getFollowsList nostr userPubKey
                |> Maybe.andThen
                    (\followList ->
                        followList
                            |> List.filterMap
                                (\following ->
                                    case following of
                                        FollowingPubKey { pubKey } ->
                                            if profilePubKey == pubKey then
                                                Just (Following (Unfollow userPubKey))

                                            else
                                                Nothing

                                        FollowingHashtag _ ->
                                            Nothing
                                )
                            |> List.head
                    )
                |> Maybe.withDefault (NotFollowing (Follow userPubKey))

        Nothing ->
            UnknownFollowing
