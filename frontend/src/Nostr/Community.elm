module Nostr.Community exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder, maybe, string, succeed, list, nullable)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Decode exposing (fail)
import Nostr.Event exposing (EventFilter, Kind)
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (EventId, PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time exposing (Month(..))
import Ui.Profile exposing (defaultProfileImage, validationIcon)
import Ui.Shared exposing (fontFamilyUnbounded, fontFamilyInter)

-- Types

type alias Moderator =
    { pubKey : String
    , relay : String
    , role : String
    }


type alias Image =
    { url : String
    , resolution : Maybe ImageSize
    }

type ImageSize
    = ImageSize Int Int

type alias Relay =
    { url : String
    , relayType : RelayType
    }


type RelayType
    = RelayTypeAuthor
    | RelayTypeRequests
    | RelayTypeApprovals
    | RelayTypeUnknown
    | RelayTypeGeneric


type alias Community =
    { dtag : String
    , pubKey : PubKey
    , name : Maybe String
    , description : Maybe String
    , image : Maybe Image
    , moderators : List Moderator
    , relay : String -- the relay this event was loaded from
    , relays : List Relay
    }

communityMatchesFilter : EventFilter -> Community -> Bool
communityMatchesFilter filter community =
    True

communityName : Community -> String
communityName community =
    case community.name of
        Just name ->
            name

        Nothing ->
            community.dtag

-- Decoders

relayTypeDecoder : Decoder RelayType
relayTypeDecoder =
    Decode.oneOf
        [ string
            |> Decode.andThen
                (\relayTypeString ->
                    case relayTypeString of
                        "author" ->
                            succeed RelayTypeAuthor

                        "requests" ->
                            succeed RelayTypeRequests

                        "approvals" ->
                            succeed RelayTypeApprovals

                        _ ->
                            succeed RelayTypeUnknown
                )
        , succeed RelayTypeGeneric
        ]

imageResolutionDecoder : Decoder ImageSize
imageResolutionDecoder =
    string
        |> Decode.andThen
            (\sizeString ->
                case String.split "x" sizeString of
                    [ widthString, heightString ] ->
                        case ( String.toInt widthString, String.toInt heightString ) of
                            ( Just width, Just height ) ->
                                succeed <| ImageSize width height

                            _ ->
                                fail <| "Invalid numbers in image size: " ++ sizeString

                    _ ->
                        fail <| "Invalid image size format: " ++ sizeString
            )


relayDecoder : Decoder Relay
relayDecoder =
    Decode.succeed Relay
        |> required "url" string
        |> optional "type" relayTypeDecoder RelayTypeGeneric


moderatorDecoder : Decoder Moderator
moderatorDecoder =
    Decode.succeed Moderator
        |> required "pubkey" string
        |> required "relay" string
        |> required "role" string


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "url" string
        |> optional "resolution" (maybe imageResolutionDecoder) Nothing


communityDecoder : Decoder Community
communityDecoder =
    Decode.succeed Community
        |> required "dtag" string
        |> required "pubkey" string
        |> required "name" (nullable string)
        |> optional "description" (maybe string) Nothing
        |> optional "image" (maybe imageDecoder) Nothing
        |> required "moderators" (list moderatorDecoder)
        |> required "relay" string
        |> required "relays" (list relayDecoder)


viewCommunity : BrowserEnv -> Dict PubKey Profile -> Community -> Html msg
viewCommunity browserEnv profiles community =
    div
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
            [ viewImage community.image
            , viewName <| communityName community
            , viewSummary community.description
            , viewModerators browserEnv profiles community.moderators
            ]
        ]

viewImage : Maybe Image -> Html msg
viewImage maybeImage =
    case maybeImage of
        Just image ->
            div
                [ css
                    [ Tw.relative
                    , Tw.mb_4
                    ]
                ]
                [ img
                    [ Attr.src image.url
                    , Attr.alt "Post Image"
                    , css
                        [ Tw.rounded_lg
                        , Tw.w_full
                        , Tw.object_cover
                        ]
                    ]
                    []
                ]

        Nothing ->
            div [][]

viewName : String -> Html msg
viewName name =
    h1
        [ css
            [ Tw.text_4xl
            , Tw.font_bold
            , Tw.text_color Theme.gray_900
            , Tw.mb_2
            ]
        , fontFamilyUnbounded
        ]
        [ text name
        ]

viewSummary : Maybe String -> Html msg
viewSummary maybeDescription =
    case maybeDescription of
        Just description ->
            p
                [ css
                    [ Tw.text_color Theme.gray_600
                    , Tw.text_sm
                    , Tw.mb_4
                    ]
                ]
                [ text description ]
        Nothing ->
            div [][]

viewModerators : BrowserEnv -> Dict PubKey Profile -> List Moderator -> Html msg
viewModerators browserEnv profiles moderators =
    if not (List.isEmpty moderators) then
        h3
            [ css
                [ Tw.text_lg
                , Tw.font_bold
                , Tw.text_color Theme.gray_900
                , Tw.mb_2
                ]
            , fontFamilyUnbounded
            ]
            [ text "Moderators"
            , List.map (\moderator -> viewModerator (Dict.get moderator.pubKey profiles) moderator) moderators
                |> div []
            ]
    else
        div [][]

viewModerator : Maybe Profile -> Moderator -> Html msg
viewModerator maybeProfile moderator =
    case maybeProfile of
        Just profile ->
            viewProfile profile moderator

        Nothing ->
            viewPubKey moderator.pubKey

viewProfile : Profile -> Moderator -> Html msg
viewProfile profile moderator =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.mb_4
            ]
        ]
        [ Ui.Profile.viewProfileImage (div []) profile.picture profile.valid
        , div []
            [ h4
                [ css
                    [ Tw.text_sm
                    , Tw.font_semibold
                    , Tw.text_color Theme.gray_800
                    ]
                ]
                [ text (profileDisplayName moderator.pubKey profile) ]
            ]
        ]


viewPubKey : String -> Html msg
viewPubKey pubKey =
    h4
        [ css
            [ Tw.text_sm
            , Tw.font_semibold
            , Tw.text_color Theme.gray_800
            ]
        ]
        [ text pubKey ]