module Ui.Profile exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Button as Button
import Components.Icon as Icon exposing (Icon(..), MaterialIcon(..))
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, a, div, h2, img, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode
import Nostr.Nip05 as Nip05
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName, shortenedPubKey)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Set exposing (Set)
import Shared
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Time
import Translations.Profile as Translations
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Shared exposing (emptyHtml, extendedZapRelays, pubkeyInboxRelays, zapButton)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


defaultProfilePicture : String
defaultProfilePicture =
    "/images/avatars/placeholder_01.png"


profilePicture : Int -> Maybe Profile -> String
profilePicture width maybeProfile =
    maybeProfile
        |> Maybe.andThen .picture
        |> Maybe.map (Ui.Shared.extendUrlForScaling width)
        |> Maybe.withDefault defaultProfilePicture


type alias ProfileViewData msg =
    { browserEnv : BrowserEnv
    , following : FollowType msg
    , subscribe : Maybe msg
    , theme : Theme
    , validation : ProfileValidation
    }


type FollowType msg
    = Following (PubKey -> msg) -- unfollow msg
    | NotFollowing (PubKey -> msg) -- follow msg
    | UnknownFollowing


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
            [ viewProfileImageSmall (linkElementForProfile profile validationStatus) (Just profile) validationStatus
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


viewProfile : Profile -> ProfileViewData msg -> Shared.Model -> Html msg
viewProfile profile profileViewData shared =
    let
        styles =
            stylesForTheme profileViewData.theme

        authorInboxRelays =
            pubkeyInboxRelays shared.nostr profile.pubKey

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus

        zapRelays =
            extendedZapRelays authorInboxRelays shared.nostr userPubKey
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
                , Tw.items_start
                , Tw.space_x_4
                , Tw.mb_4
                ]
            ]
            [ viewProfileImage (div [ css [ Tw.flex_none ] ]) (Just profile) profileViewData.validation
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
                , viewNip05 styles profile profileViewData.validation
                , viewLNAddress styles profile zapRelays
                , viewNpub styles profile shared.browserEnv.translations
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    ]
                ]
                [ viewSubscriptionButton profile profileViewData
                , followButton profileViewData.theme profileViewData.browserEnv profile.pubKey profileViewData.following
                ]
            ]
        ]



-- so far only displayed in dev mode until release


viewSubscriptionButton : Profile -> ProfileViewData msg -> Html msg
viewSubscriptionButton _ profileViewData =
    case profileViewData.subscribe of
        Just subscribeMsg ->
            Button.new
                { label = Translations.subscribeButtonTitle [ profileViewData.browserEnv.translations ]
                , onClick = Just subscribeMsg
                , theme = profileViewData.theme
                }
                |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.mail)
                |> Button.view

        _ ->
            emptyHtml


followButton : Theme -> BrowserEnv -> PubKey -> FollowType msg -> Html msg
followButton theme browserEnv profilePubKey following =
    case following of
        Following msg ->
            Button.new
                { label = Translations.unfollowButtonTitle [ browserEnv.translations ]
                , onClick = Just (msg profilePubKey)
                , theme = theme
                }
                |> Button.withIconLeft (Icon.MaterialIcon MaterialCheck 24 (Icon.Color (Color.fromRgba { red = 0.28, green = 0.73, blue = 0.47, alpha = 1.0 })))
                |> Button.view

        NotFollowing msg ->
            Button.new
                { label = Translations.followButtonTitle [ browserEnv.translations ]
                , onClick = Just (msg profilePubKey)
                , theme = theme
                }
                |> Button.view

        UnknownFollowing ->
            emptyHtml


viewWebsite : Styles msg -> Profile -> Html msg
viewWebsite styles profile =
    case profile.website of
        Just website ->
            a
                (styles.colorStyleLinks
                    ++ styles.textStyleLinks
                    ++ [ Attr.href (websiteLink website)
                       ]
                )
                [ text website ]

        Nothing ->
            emptyHtml


websiteLink : String -> String
websiteLink url =
    if not (String.startsWith "http" url) then
        "https://" ++ url

    else
        url


viewNip05 : Styles msg -> Profile -> ProfileValidation -> Html msg
viewNip05 styles profile validation =
    case profile.nip05 of
        Just nip05 ->
            p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++ [ css [ Tw.flex, Tw.items_center ] ])
                [ text <| Nip05.nip05ToDisplayString nip05
                , div
                    [ css
                        [ Tw.mx_0_dot_5
                        , Tw.my_0_dot_5
                        ]
                    ]
                    [ validationIcon 20 validation ]
                ]

        Nothing ->
            emptyHtml


viewLNAddress : Styles msg -> Profile -> Set String -> Html msg
viewLNAddress styles profile zapRelays =
    profile.lud16
        |> Maybe.map
            (\lud16 ->
                p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++ [ css [ Tw.flex, Tw.items_center ] ])
                    [ text <| lud16, zapButton profile.pubKey Nothing zapRelays "0" ]
            )
        |> Maybe.withDefault (emptyHtml)


viewNpub : Styles msg -> Profile -> I18Next.Translations -> Html msg
viewNpub styles profile translations =
    let
        maybeNip19 =
            Nip19.Npub profile.pubKey
                |> Nip19.encode
                |> Result.toMaybe
    in
    case maybeNip19 of
        Just nip19 ->
            p
                (styles.colorStyleGrayscaleText
                    ++ styles.textStyleBody
                    ++ [ css [ Tw.flex ] ]
                )
                [ text <| shortenedPubKey 11 nip19
                , copyButton translations nip19 (shortenedPubKey 11 nip19)
                ]

        Nothing ->
            emptyHtml


copyButton : I18Next.Translations -> String -> String -> Html msg
copyButton translations copyText uniqueId =
    div
        [ css [ Tw.m_0_dot_5 ] ]
        [ div
            [ css [ Tw.cursor_pointer ]
            , Attr.id uniqueId
            ]
            [ Icon.FeatherIcon
                (FeatherIcons.clipboard
                    |> FeatherIcons.withSize 20
                )
                |> Icon.view
            ]
        , Html.node "js-clipboard-component"
            [ Attr.property "buttonId" (Encode.string uniqueId)
            , Attr.property "copyContent" (Encode.string copyText)

            -- TODO: make a stateful profile component and use AlertTimerMessage to give users a visual feedback
            --, Events.on "copiedToClipboard" (Decode.succeed (Translations.npubCopied [ translations ]))
            ]
            []
        ]


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
            emptyHtml


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
        [ viewProfileImage (linkElementForProfilePubKey pubKey) Nothing ValidationUnknown
        , h2
            [ css
                [ Tw.text_sm
                , Tw.font_semibold
                , Tw.text_color Theme.gray_800
                ]
            ]
            [ text pubKey ]
        ]


viewProfileImage : (List (Html msg) -> Html msg) -> Maybe Profile -> ProfileValidation -> Html msg
viewProfileImage linkElement maybeProfile validationStatus =
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src <| profilePicture 112 maybeProfile
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
                [ Tw.absolute
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
            emptyHtml

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


viewProfileImageSmall : (List (Html msg) -> Html msg) -> Maybe Profile -> ProfileValidation -> Html msg
viewProfileImageSmall linkElement maybeProfile validationStatus =
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src <| profilePicture 40 maybeProfile
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
            emptyHtml
