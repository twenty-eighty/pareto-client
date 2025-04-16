module Pages.Authors exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Layouts
import Nostr
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (Following(..), PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.AuthorsPage as Translations
import Ui.Profile exposing (followingProfile, viewAuthorCard)
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
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
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    {}


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared _ () =
    let
        authorPubKeys =
            Nostr.getAuthorsFollowList shared.nostr
                |> List.filterMap
                    (\following ->
                        case following of
                            FollowingPubKey { pubKey } ->
                                Just pubKey

                            _ ->
                                Nothing
                    )

        fetchProfilesEffect =
            { emptyEventFilter
                | authors = Just authorPubKeys
                , kinds =
                    Just
                        [ KindUserMetadata ]
            }
                |> RequestProfile Nothing
                |> Nostr.createRequest shared.nostr "Profile" [ KindLongFormContent ]
                |> Shared.Msg.RequestNostrEvents
                |> Effect.sendSharedMsg
    in
    ( {}, fetchProfilesEffect )



-- UPDATE


type Msg
    = Follow PubKey PubKey
    | Unfollow PubKey PubKey


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared _ =
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ viewAuthors shared
        ]
    }


viewAuthors : Shared.Model -> Html Msg
viewAuthors shared =
    Nostr.getAuthorsFollowList shared.nostr
        |> List.filterMap
            (\following ->
                case following of
                    FollowingPubKey { pubKey } ->
                        Nostr.getProfile shared.nostr pubKey

                    _ ->
                        Nothing
            )
        |> List.map (viewAuthorCard shared)
        |> div
            [ css
                [ Tw.grid
                , Tw.justify_items_center
                , Bp.xxl
                    [ Tw.grid_cols_3
                    ]
                , Bp.xl
                    [ Tw.grid_cols_2
                    ]
                , Bp.lg
                    [ Tw.grid_cols_2
                    ]
                , Tw.grid_cols_1
                , Tw.gap_4
                ]
            ]


viewAuthorCard : Shared.Model -> Profile -> Html Msg
viewAuthorCard shared profile =
    let
        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    Ui.Profile.viewAuthorCard
        profile
        { browserEnv = shared.browserEnv
        , nostr = shared.nostr
        , loginStatus = shared.loginStatus
        , following = followingProfile shared.nostr profile.pubKey Follow Unfollow userPubKey
        , subscribe = Nothing
        , theme = shared.theme
        , validation =
            Nostr.getProfileValidationStatus shared.nostr profile.pubKey
                |> Maybe.withDefault ValidationUnknown
        }
