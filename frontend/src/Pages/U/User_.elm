module Pages.U.User_ exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.FollowList exposing (Following(..))
import Nostr.Nip05 as Nip05 exposing (Nip05)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Ui.Profile exposing (FollowType(..))
import Ui.Styles exposing (Styles, Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)
import Nostr.Event exposing (Kind(..))
import Nostr.Nip05 exposing (nip05ToString)


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
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


-- INIT


type alias Model =
    { nip05 : Maybe Nip05
    }


init : Shared.Model -> Route { user : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            { nip05 = Nip05.parseNip05 route.params.user }


        requestEffect =
            model.nip05
            |> Maybe.map (\nip05 ->
                case Nostr.getPubKeyByNip05 shared.nostr nip05 of
                    Just pubKey ->
                        -- already validated, don't do again
                        pubKey
                        |> buildRequestArticlesEffect shared.nostr

                    Nothing ->
                        RequestProfileByNip05 nip05
                        |> Nostr.createRequest shared.nostr ("Profile and data of NIP-05 user " ++ nip05ToString nip05) [KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
            )
            |> Maybe.withDefault Effect.none

    in
    ( model, requestEffect
    )

buildRequestArticlesEffect : Nostr.Model -> PubKey -> Effect Msg
buildRequestArticlesEffect nostr pubKey =
    { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just [pubKey], limit = Just 20 }
    |> RequestArticlesFeed 
    |> Nostr.createRequest nostr "Posts of user" [KindUserMetadata]
    |> Shared.Msg.RequestNostrEvents
    |> Effect.sendSharedMsg


-- UPDATE


type Msg
    = Follow PubKey PubKey
    | Unfollow PubKey PubKey


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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        maybeProfile =
            model.nip05
            |> Maybe.andThen (\nip05 ->
                Nostr.getProfileByNip05 shared.nostr nip05
            )
    in
    { title = "Profile"
    , body =
        [ case maybeProfile of
            Just profile ->
                viewProfile shared profile

            Nothing ->
                div [][]
        ]
    }

viewProfile : Shared.Model -> Profile -> Html Msg
viewProfile shared profile =
    div []
        [ Ui.Profile.viewProfile
            profile
            { browserEnv = shared.browserEnv
            , following = followingProfile shared.nostr profile.pubKey (Shared.loggedInPubKey shared.loginStatus)
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
                    , onReaction = Nothing
                    , onZap = Nothing
                    }
        ]

followingProfile : Nostr.Model -> PubKey -> Maybe PubKey -> FollowType Msg
followingProfile nostr profilePubKey maybePubKey =
    case maybePubKey of
        Just userPubKey ->
            Nostr.getFollowsList nostr userPubKey
            |> Maybe.andThen (\followList ->
                followList
                |> List.filterMap (\following ->
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