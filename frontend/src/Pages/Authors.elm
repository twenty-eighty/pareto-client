module Pages.Authors exposing (Model, Msg, page)

import Components.Button as Button
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.ConfigCheck as ConfigCheck
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
import Task exposing (Task)
import Translations.AuthorsPage as Translations
import Ui.Profile exposing (followingProfile, viewAuthorCard)
import Ui.Shared exposing (emptyHtml, viewConfigIssues)
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
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { configChecks : Dict PubKey ConfigCheck.Model
    }


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared _ () =
    let
        authorPubKeys =
            Nostr.getAuthorsPubKeys shared.nostr

        fetchProfilesEffect =
            { emptyEventFilter
                | authors = Just authorPubKeys
                , kinds = Just [ KindUserMetadata ]
            }
                |> RequestProfile Nothing
                |> Nostr.createRequest shared.nostr "Profile" [ KindLongFormContent ]
                |> Shared.Msg.RequestNostrEvents
                |> Effect.sendSharedMsg
    in
    ( { configChecks = Dict.empty }
    , Effect.batch
        [ fetchProfilesEffect
        , Effect.scrollContentToTop
        ]
    )



-- UPDATE


type Msg
    = Follow PubKey PubKey
    | Unfollow PubKey PubKey
    | FetchConfigCheckData
    | PerformConfigChecks
    | ReceivedConfigChecks (Result Never (Dict PubKey ConfigCheck.Model, Cmd Msg))
    | ConfigCheckMsg PubKey ConfigCheck.Msg


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
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

        FetchConfigCheckData ->
            let
                authorPubKeys =
                    Nostr.getAuthorsPubKeys shared.nostr
            in
            ( model
            , Effect.batch
                [ { emptyEventFilter
                    | authors = Just authorPubKeys
                    , kinds = Just  
                        [ KindUserServerList
                        , KindFileStorageServerList
                        , KindRelayListMetadata
                        , KindSearchRelaysList
                        , KindPrivateRelayList
                        , KindRelayListForDMs
                        ]
                  }
                    |> RequestProfile Nothing
                    |> Nostr.createRequest shared.nostr "Config data" [ ]
                    |> Shared.Msg.RequestNostrEvents
                    |> Effect.sendSharedMsg
                , authorPubKeys
                    |> List.map Shared.Msg.LoadUserDataByPubKey
                    |> List.map Effect.sendSharedMsg
                    |> Effect.batch
                ]
            )

        PerformConfigChecks ->
            ( model
            , performConfigChecks shared model
                |> Task.attempt ReceivedConfigChecks
                |> Effect.sendCmd
            )

        ReceivedConfigChecks (Ok ( configChecks, cmd )) ->
            ( { model | configChecks = configChecks }, Effect.sendCmd cmd )

        ReceivedConfigChecks (Err _) ->
            ( model, Effect.none )

        ConfigCheckMsg pubKey configCheckMsg ->
            let
                 configCheckResult =
                    Dict.get pubKey model.configChecks
                        |> Maybe.map (ConfigCheck.update configCheckMsg)
            in
            case configCheckResult of
                Just (configCheckModel, configCheckCmd) ->
                    ( { model | configChecks = Dict.insert pubKey configCheckModel model.configChecks }
                    , configCheckCmd
                        |> Cmd.map (ConfigCheckMsg pubKey)
                        |> Effect.sendCmd
                     )

                Nothing ->
                    ( model, Effect.none )


performConfigChecks : Shared.Model -> Model -> Task Never ((Dict PubKey ConfigCheck.Model), Cmd Msg)
performConfigChecks shared _ =
    Nostr.getAuthorsPubKeys shared.nostr
        |> List.foldl
            (\pubKey ( acc, cmds ) ->
                let
                    ( configCheck, cmd ) =
                        ConfigCheck.performChecks shared.nostr pubKey
                in
                ( (pubKey, configCheck) :: acc, Cmd.map (ConfigCheckMsg pubKey) cmd :: cmds )
            )
            ([], [])
        |> (\( configChecks, cmds ) ->
                ( Dict.fromList configChecks, Cmd.batch cmds )
            )
        |> Task.succeed


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        isBetaTester =
            Shared.loggedInPubKey shared.loginStatus
                |> Maybe.map (Nostr.isBetaTester shared.nostr)
                |> Maybe.withDefault False

        checkConfigButton =
            if isBetaTester then
                div [ css [ Tw.flex, Tw.justify_end, Tw.gap_2, Tw.m_4 ] ]
                    [ Button.new
                        { label = Translations.fetchConfigDataButtonTitle [ shared.browserEnv.translations ]
                        , onClick = Just FetchConfigCheckData
                        , theme = shared.theme
                        }
                        |> Button.view
                    , Button.new
                        { label = Translations.checkConfigButtonTitle [ shared.browserEnv.translations ]
                        , onClick = Just PerformConfigChecks
                        , theme = shared.theme
                        }
                        |> Button.view
                    ]
            else
                emptyHtml
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ checkConfigButton
        , viewAuthors shared model
        ]
    }


viewAuthors : Shared.Model -> Model -> Html Msg
viewAuthors shared model =
    Nostr.getAuthorsPubKeys shared.nostr
        |> List.filterMap
            (\pubKey ->
                Nostr.getProfile shared.nostr pubKey
                |> Maybe.map (\profile -> ( profile, Dict.get pubKey model.configChecks ))
            )
        |> List.map (\( profile, maybeConfigCheck ) -> viewAuthorCard shared profile maybeConfigCheck)
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


viewAuthorCard : Shared.Model -> Profile -> Maybe ConfigCheck.Model -> Html Msg
viewAuthorCard shared profile maybeConfigCheck =
    let
        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
        [ Ui.Profile.viewAuthorCard
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
        , div [ css [ Tw.pl_4 ] ]
            [ maybeConfigCheck
                |> Maybe.map ConfigCheck.getIssues
                |> Maybe.map (viewConfigIssues shared.browserEnv "Config Issues")
                |> Maybe.withDefault emptyHtml
            ]
        ]
