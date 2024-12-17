module Ui.Profile exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Graphics
import Html.Styled as Html exposing (Html, Attribute, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Nostr.Nip05 as Nip05
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)

defaultProfileImage : String
defaultProfileImage =
    "/images/avatars/placeholder_01.png"


viewProfileSmall : Profile -> ProfileValidation -> Html msg
viewProfileSmall profile validationStatus =
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.space_y_2
                , Tw.mb_4
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfileImageSmall (linkElementForProfile profile) profile.picture validationStatus
                , h2
                    [ css
                        [ Tw.text_sm
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        ]
                    ]
                    [ text (profileDisplayName profile.pubKey profile) ]
                ]
            ]



viewProfile : Theme -> Profile -> ProfileValidation -> Html msg
viewProfile theme profile validationStatus =
    let
        styles =
            stylesForTheme theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.space_y_2
            , Tw.mx_4
            , Tw.mb_4
            ]
        ]
        [ viewBanner profile.banner
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.items_center
                , Tw.space_x_4
                , Tw.mb_4
                ]
            ]
            [ viewProfileImage (div [ css [ Tw.flex_none ]]) profile.picture validationStatus
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.space_y_2
                    , Tw.flex_grow
                    , Tw.mb_4
                    ]
                ]
                [ h2
                    (styles.colorStyleGrayscaleTitle ++ styles.textStyleH2)
                    [ text (profileDisplayName profile.pubKey profile) ]
                , p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text (profile.about |> Maybe.withDefault "") ]
                , viewWebsite styles profile
                , viewNip05 styles profile
                , viewNpub styles profile
                ]
            ]
        ]

viewWebsite : Styles msg -> Profile -> Html msg
viewWebsite styles profile =
    case profile.website of
        Just website ->
            a
                (styles.colorStyleLinks ++ styles.textStyleLinks ++
                [ Attr.href website
                ])
                [ text website ]

        Nothing ->
            div [][]

viewNip05 : Styles msg -> Profile -> Html msg
viewNip05 styles profile =
    case profile.nip05 of
        Just nip05 ->
            p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                [ text <| Nip05.nip05ToDisplayString nip05 ]

        Nothing ->
            div [][]

viewNpub : Styles msg -> Profile -> Html msg
viewNpub styles profile =
    let
        maybeNip19 =
            Nip19.Npub profile.pubKey
            |> Nip19.encode
            |> Result.toMaybe
    in
    case maybeNip19 of
        Just nip19 ->
            p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                [ text nip19 ]

        Nothing ->
            div [][]

viewBanner : Maybe String -> Html msg
viewBanner maybeImage =
    case maybeImage of
        Just image ->
            div
                [ css
                    [ Tw.relative
                    , Tw.mb_4
                    ]
                ]
                [ img
                    [ Attr.src image
                    , Attr.alt "Banner Image"
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

viewProfilePubKey : PubKey -> Html msg
viewProfilePubKey pubKey =
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfileImage (linkElementForProfilePubKey pubKey) (Just defaultProfileImage) ValidationUnknown
                , h2
                    [ css
                        [ Tw.text_sm
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        ]
                    ]
                    [ text pubKey ]
                ]

viewProfileImage : (List (Html msg) -> Html msg) -> Maybe String -> ProfileValidation -> Html msg
viewProfileImage linkElement maybeImage validationStatus =
    let
        image =
            maybeImage
            |> Maybe.withDefault defaultProfileImage
    in
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src image
                , Attr.alt "Avatar"
                , css
                    [ Tw.min_w_28
                    , Tw.min_h_28
                    , Tw.max_w_28
                    , Tw.max_h_28
                    , Tw.p_1
                    , Tw.rounded_full
                    ]
                ]
                []
            ]
        , div 
            [ css
                [  Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.text_color Theme.gray_400
                , Tw.w_4
                , Tw.h_4
                ]
            ]
            [ validationIcon 24 validationStatus
            ]
        ]

validationIcon : Int -> ProfileValidation -> Html msg
validationIcon width validation =
    case validation of
        ValidationUnknown ->
            div [][]

        ValidationPending ->
            Graphics.featherMehIcon width

        ValidationNameMissing ->
            Graphics.featherFrownIcon width

        ValidationNotMatchingPubKey ->
            Graphics.featherFrownIcon width

        ValidationNetworkError _ ->
            Graphics.featherFrownIcon width

        ValidationSucceeded ->
            Graphics.featherSmileIcon width


validationTooltipText : ProfileValidation -> String
validationTooltipText status =
    case status of
        ValidationUnknown ->
            "Unknown profile validation status"

        ValidationPending ->
            "Validating profile..."

        ValidationNameMissing ->
            "Error validating profile: name missing in nostr.json"

        ValidationNotMatchingPubKey ->
            "Error validating profile: public key on server doesn't match"

        ValidationNetworkError error ->
            "Network error validating profile: " ++ httpErrorToString error

        ValidationSucceeded ->
            "Profile validated successfully"


viewProfileImageSmall : (List (Html msg) -> Html msg) -> Maybe String -> ProfileValidation -> Html msg
viewProfileImageSmall linkElement maybeImage validationStatus =
    let
        image =
            maybeImage
            |> Maybe.withDefault defaultProfileImage
    in
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src image
                , Attr.alt "Avatar"
                , css
                    [ Tw.w_10
                    , Tw.h_10
                    , Tw.p_1
                    , Tw.rounded_full
                    ]
                ]
                []
            ]
        , div 
            [ css
                [ Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.text_color Theme.gray_400
                , Tw.max_w_2
                , Tw.max_h_2
                ]
            ]
            [ validationIcon 16 validationStatus
            ]
        ]

timeParagraph : BrowserEnv -> Maybe Time.Posix -> Html msg
timeParagraph browserEnv maybePublishedAt =
    case maybePublishedAt of
        Just publishedAt ->
            p
                [ css
                    [ Tw.text_xs
                    , Tw.text_color Theme.gray_500
                    ]
                ]
                [ text <| BrowserEnv.formatDate browserEnv publishedAt ]

        Nothing ->
            div [][]

profileDisplayName : PubKey -> Profile -> String
profileDisplayName pubKey profile =
    case (profile.displayName, profile.name, profile.nip05) of
        (Just displayName, _, _) ->
            displayName
        (Nothing, Just name, _) ->
            name
        (Nothing, Nothing, Just nip05) ->
            Nip05.nip05ToDisplayString nip05
        (Nothing, Nothing, Nothing) ->
            shortenedPubKey pubKey

shortenedPubKey : String -> String
shortenedPubKey pubKey =
    String.left 6 pubKey ++ "..." ++ String.right 6 pubKey
