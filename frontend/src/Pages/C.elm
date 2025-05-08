module Pages.C exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, div, h1, h3, img, input, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Community exposing (Community, communityDefinitionFromEvent)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), decodeEvent, emptyEventFilter, numberForKind)
import Nostr.Nip19 as Nip19
import Nostr.Types exposing (IncomingMessage, PubKey)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginStatus(..))
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.Communities as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme(..), fontFamilyInter, fontFamilyUnbounded)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
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
    { communities : Maybe (List Community)
    , searchString : Maybe String
    , searchStringLowerCase : Maybe String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { communities = Nothing
      , searchString = Nothing
      , searchStringLowerCase = Nothing
      }
    , Effect.sendCmd <|
        Ports.requestEvents "Communities" False -1 [] [ communitiesFilter ]
    )


communitiesFilter : EventFilter
communitiesFilter =
    { emptyEventFilter
        | kinds = Just [ KindCommunityDefinition ]
    }



-- UPDATE


type Msg
    = ReceivedMessage IncomingMessage
    | UpdateSearch String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage shared model message

        UpdateSearch searchString ->
            if searchString == "" then
                ( { model | searchString = Nothing, searchStringLowerCase = Nothing }, Effect.none )

            else
                ( { model | searchString = Just searchString, searchStringLowerCase = Just <| String.toLower searchString }, Effect.none )


updateWithMessage : Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage _ model message =
    case message.messageType of
        "events" ->
            case Decode.decodeValue (Decode.list decodeEvent) message.value of
                Ok events ->
                    let
                        communities =
                            events
                                |> List.map communityDefinitionFromEvent
                    in
                    ( { model | communities = Just communities }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.communitiesTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            (css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.min_h_screen
                ]
                :: stylesFromParetoTheme.colorStyleBackground
            )
            [ div
                [ css
                    [ Tw.bg_color Theme.white
                    , Tw.p_6
                    , Tw.rounded_lg
                    , Tw.shadow_lg
                    , Tw.max_w_3xl
                    , Tw.space_y_2
                    ]
                ]
                [ h1
                    ([ css
                        [ Tw.text_4xl
                        , Tw.font_bold
                        , Tw.mb_2
                        ]
                     , fontFamilyUnbounded
                     ]
                        ++ stylesFromParetoTheme.colorStyleGrayscaleTitle
                    )
                    [ text <| Translations.communitiesTitle [ shared.browserEnv.translations ]
                    ]
                , h3
                    ([ css
                        [ Tw.text_2xl
                        , Tw.font_bold
                        , Tw.mb_2
                        ]
                     , fontFamilyUnbounded
                     ]
                        ++ stylesFromParetoTheme.colorStyleGrayscaleTitle
                    )
                    [ text <| Translations.myCommunitiesTitle [ shared.browserEnv.translations ]
                    ]
                , viewFollowedCommunities shared model
                , h3
                    ([ css
                        [ Tw.text_2xl
                        , Tw.font_bold
                        , Tw.mb_2
                        ]
                     , fontFamilyUnbounded
                     ]
                        ++ stylesFromParetoTheme.colorStyleGrayscaleTitle
                    )
                    [ text <| Translations.browseCommunitiesText [ shared.browserEnv.translations ]
                    ]
                , viewSearchBar model
                , viewCommunities model
                ]
            ]
        ]
    }


viewFollowedCommunities : Shared.Model -> Model -> Html Msg
viewFollowedCommunities shared model =
    case ( shared.loginStatus, model.communities ) of
        ( LoggedIn pubKey _, Just communities ) ->
            Nostr.getCommunityList shared.nostr pubKey
                |> Maybe.withDefault []
                |> List.filterMap
                    (\communityRef ->
                        communityForRef communities communityRef.identifier communityRef.pubKey
                    )
                |> List.map (viewCommunityPreview model)
                |> div
                    [ css
                        [ Tw.space_y_2
                        ]
                    ]

        _ ->
            p
                ([ css
                    [ Tw.mb_2 ]
                 , fontFamilyInter
                 ]
                    ++ stylesFromParetoTheme.colorStyleGrayscaleText
                )
                [ text <| Translations.noCommunitiesText [ shared.browserEnv.translations ]
                ]


communityForRef : List Community -> String -> PubKey -> Maybe Community
communityForRef communities identifier pubKey =
    communities
        |> List.filter (\community -> community.dtag == Just identifier && community.pubKey == pubKey)
        |> List.head


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div
        ([ Events.onInput UpdateSearch
         , css
            [ Tw.min_h_6
            , Tw.min_w_20
            ]
         ]
            ++ stylesFromParetoTheme.colorStyleBackground
        )
        [ input
            ([ Attr.attribute "type" "text"
             , Attr.attribute "name" "search"
             , Attr.attribute "id" "search"
             , Attr.attribute "placeholder" "Search for communities"
             , Attr.attribute "value" (model.searchString |> Maybe.withDefault "")
             , css
                [ Tw.w_full
                , Tw.border_2
                , Tw.border_solid
                , Tw.rounded_lg
                , Tw.p_3
                ]
             , fontFamilyInter
             ]
                ++ stylesFromParetoTheme.colorStyleGrayscaleText
                ++ stylesFromParetoTheme.colorStyleBorders
            )
            []
        ]


viewCommunities : Model -> Html Msg
viewCommunities model =
    case model.communities of
        Just communities ->
            communities
                |> List.filter (filteredCommunity model.searchStringLowerCase)
                |> List.map (viewCommunityPreview model)
                |> div
                    [ css
                        [ Tw.space_y_2
                        ]
                    ]

        Nothing ->
            div
                []
                []


filteredCommunity : Maybe String -> Community -> Bool
filteredCommunity maybeSearchString community =
    maybeSearchString
        |> Maybe.map
            (\searchString ->
                String.contains searchString (String.toLower (Maybe.withDefault "" community.dtag))
                    || String.contains searchString (String.toLower <| Maybe.withDefault "" community.name)
                    || String.contains searchString (String.toLower <| Maybe.withDefault "" community.description)
            )
        |> Maybe.withDefault True


viewCommunityPreview : Model -> Community -> Html Msg
viewCommunityPreview _ community =
    div
        [ css
            [ Tw.bg_color Theme.white
            , Tw.p_6
            , Tw.rounded_lg
            , Tw.shadow_lg
            , Tw.max_w_3xl
            , Tw.space_x_2
            , Tw.flex
            , Tw.flex_row
            ]
        ]
        [ viewImagePreview community
        , div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.p_6
                , Tw.max_w_3xl
                , Tw.space_x_2
                , Tw.flex
                , Tw.flex_col
                ]
            ]
            [ h3
                ([ css
                    [ Tw.text_2xl
                    , Tw.font_bold
                    , Tw.mb_2
                    ]
                 , fontFamilyUnbounded
                 ]
                    ++ stylesFromParetoTheme.colorStyleGrayscaleTitle
                )
                [ linkElement community
                    []
                    [ text <| Nostr.Community.communityName community ]
                ]
            , p
                ([ css
                    [ Tw.mb_2 ]
                 , fontFamilyInter
                 ]
                    ++ stylesFromParetoTheme.colorStyleGrayscaleText
                )
                [ text (community.description |> Maybe.withDefault "")
                ]
            ]
        ]


viewImagePreview : Community -> Html msg
viewImagePreview community =
    case community.image of
        Just image ->
            linkElement community
                [ css
                    [ Tw.relative
                    , Tw.mb_4
                    ]
                ]
                [ img
                    [ Attr.src image.url
                    , Attr.alt "Community Image"
                    , css
                        [ Tw.rounded_2xl
                        , Tw.object_cover
                        , Tw.max_w_52
                        , Tw.max_h_36
                        ]
                    ]
                    []
                ]

        Nothing ->
            emptyHtml


linkElement : Community -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
linkElement community attrs content =
    case linkToCommunity community of
        Just url ->
            a (attrs ++ [ Attr.href url ]) content

        Nothing ->
            div attrs content


linkToCommunity : Community -> Maybe String
linkToCommunity community =
    Nip19.NAddr
        { kind = KindCommunityDefinition |> numberForKind
        , pubKey = community.pubKey
        , identifier = Maybe.withDefault "" community.dtag
        , relays = Maybe.map List.singleton Nothing |> Maybe.withDefault []
        }
        |> Nip19.encode
        |> Result.toMaybe
        |> Maybe.map (\naddr -> "/c/" ++ naddr)


stylesFromParetoTheme : Styles msg
stylesFromParetoTheme =
    Ui.Styles.stylesForTheme ParetoTheme
