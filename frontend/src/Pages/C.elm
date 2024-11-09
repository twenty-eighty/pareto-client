module Pages.C exposing (Model, Msg, page)

import Json.Decode as Decode
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Nip19 as Nip19
import Nostr.Community exposing (Community, communityDecoder)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), numberForKind)
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginStatus(..))
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.Communities as Translations
import Ui.Shared exposing (fontFamilyUnbounded, fontFamilyInter)
import View exposing (View)
import Html.Styled exposing (input)
import Shared.Model exposing (LoginStatus(..))


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        {}



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
        Ports.requestEvents "Communities" False -1 Nothing communitiesFilter
            
    )

communitiesFilter : EventFilter
communitiesFilter =
    { authors = Nothing
    , ids = Nothing
    , kinds = Just [ KindCommunityDefinition ]
    , tagReferences = Nothing
    , limit = Nothing
    , since = Nothing
    , until = Nothing
    }



-- UPDATE


type Msg
    = ReceivedMessage Nostr.IncomingMessage
    | UpdateSearch String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage shared model message

        UpdateSearch searchString ->
            if searchString == "" then
                ({ model | searchString = Nothing, searchStringLowerCase = Nothing }, Effect.none)
            else    
                ({ model | searchString = Just searchString, searchStringLowerCase = Just <| String.toLower searchString }, Effect.none)

updateWithMessage : Shared.Model -> Model -> Nostr.IncomingMessage -> (Model, Effect Msg)
updateWithMessage shared model message =
    case message.messageType of
        "communities" ->
            case Decode.decodeValue (Decode.list communityDecoder) message.value of
                Ok communities ->
                    let
                        (bookmarks, requestCmd) =
                            ([], Cmd.none)
                    in
                    ({ model | communities = Just communities }, Effect.sendCmd requestCmd )

                Err error ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.communitiesTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.min_h_screen
                , Tw.bg_color Theme.gray_100
                ]
            ]
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
                    [ css
                        [ Tw.text_4xl
                        , Tw.font_bold
                        , Tw.text_color Theme.gray_900
                        , Tw.mb_2
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text <| Translations.communitiesTitle [ shared.browserEnv.translations ]
                    ]
                , h3
                    [ css
                        [ Tw.text_2xl
                        , Tw.font_bold
                        , Tw.text_color Theme.gray_900
                        , Tw.mb_2
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text <| Translations.myCommunitiesTitle [ shared.browserEnv.translations ]
                    ]
                , viewFollowedCommunities shared model
                , h3
                    [ css
                        [ Tw.text_2xl
                        , Tw.font_bold
                        , Tw.text_color Theme.gray_900
                        , Tw.mb_2
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text <| Translations.browseeCommunitiesText [ shared.browserEnv.translations ]
                    ]
                , viewSearchBar model
                , viewCommunities model
                ]
            ]
        ]
    }

viewFollowedCommunities : Shared.Model -> Model -> Html Msg
viewFollowedCommunities shared model =
    case (shared.loginStatus, model.communities) of
        (LoggedIn pubKey, Just communities) ->
            Nostr.getCommunityList shared.nostr pubKey
            |> Maybe.withDefault []
            |> List.filterMap (\communityRef ->
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
                    [ css
                        [  Tw.text_color Theme.gray_900
                        , Tw.mb_2
                        ]
                    , fontFamilyInter
                    ]
                    [ text <| Translations.noCommunitiesText [ shared.browserEnv.translations ]
                    ]


communityForRef : List Community -> String -> PubKey -> Maybe Community
communityForRef communities identifier pubKey =
    communities
    |> List.filter (\community -> community.dtag == identifier && community.pubKey == pubKey)
    |> List.head

viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div
        [ Events.onInput UpdateSearch
        , css
            [ Tw.min_h_6
            , Tw.min_w_20
            , Tw.bg_color Theme.gray_100
            ]
        ]
        [ input
            [ Attr.attribute "type" "text"
            , Attr.attribute "name" "search"
            , Attr.attribute "id" "search"
            , Attr.attribute "placeholder" "Search for communities"
            , Attr.attribute "value" (model.searchString |> Maybe.withDefault "")
            , css
                [ Tw.w_full
                , Tw.text_color Theme.gray_900
                , Tw.border_color Theme.gray_100
                , Tw.border_2
                , Tw.border_solid
                , Tw.rounded_lg
                , Tw.p_3
                ]
            , fontFamilyInter
            ]
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
    |> Maybe.map (\searchString ->
        String.contains searchString (String.toLower community.dtag) ||
        String.contains searchString (String.toLower <| Maybe.withDefault "" community.name) ||
        String.contains searchString (String.toLower <| Maybe.withDefault "" community.description) 
        )
    |> Maybe.withDefault True
        

viewCommunityPreview : Model -> Community -> Html Msg
viewCommunityPreview model community =
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
                [ css
                    [ Tw.text_2xl
                    , Tw.font_bold
                    , Tw.text_color Theme.gray_900
                    , Tw.mb_2
                    ]
                , fontFamilyUnbounded
                ]
                [ linkElement community
                    [][ text <| Nostr.Community.communityName community ]
                ]
            , p
                [ css
                    [ Tw.text_color Theme.gray_900
                    , Tw.mb_2
                    ]
                , fontFamilyInter
                ]
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
            div [][]

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
        , identifier = community.dtag
        , relays = [ community.relay ]
        }
    |> Nip19.encode
    |> Result.toMaybe
    |> Maybe.map (\naddr -> "/c/" ++ naddr)