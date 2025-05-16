module Ui.Profile exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Button as Button
import Components.Icon as Icon exposing (Icon(..), MaterialIcon(..))
import Css.Media
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, a, div, h2, h4, img, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Nostr
import Nostr.Nip05 as Nip05
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName, shortenedPubKey)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (Following(..), PubKey)
import Set exposing (Set)
import Shared
import Shared.Model exposing (LoginStatus)
import Tailwind.Breakpoints as Bp exposing (..)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Time
import Translations.Profile as Translations
import Ui.Interactions exposing (extendedZapRelays, pubkeyRelays, zapButton)
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


defaultProfilePicture : String
defaultProfilePicture =
    "/images/avatars/placeholder_01.webp"


profilePicture : Int -> Maybe Profile -> String
profilePicture width maybeProfile =
    maybeProfile
        |> Maybe.andThen .picture
        |> Maybe.map (Ui.Links.scaledImageLink width)
        |> Maybe.withDefault defaultProfilePicture


type alias ProfileViewData msg =
    { browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , following : FollowType msg
    , subscribe : Maybe msg
    , theme : Theme
    , validation : ProfileValidation
    }


type FollowType msg
    = Following (PubKey -> msg) -- unfollow msg
    | NotFollowing (PubKey -> msg) -- follow msg
    | UnknownFollowing


viewProfileSmall : Styles msg -> Profile -> ProfileValidation -> Html msg
viewProfileSmall styles profile validationStatus =
    let
        linkElementWrapper =
            linkElementForProfile profile validationStatus
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.space_y_2
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.items_center
                , Tw.space_x_2
                ]
            ]
            [ viewProfileImageSmall linkElementWrapper (Just profile) validationStatus
            , h2
                (css
                    [ Tw.text_sm
                    , Tw.font_semibold
                    ]
                    :: styles.colorStyleGrayscaleTitle
                )
                [ linkElementWrapper
                    [ text (profileDisplayName profile.pubKey profile) ]
                ]
            ]
        ]


viewAuthorCard : Profile -> ProfileViewData msg -> Html msg
viewAuthorCard profile profileViewData =
    let
        styles =
            stylesForTheme profileViewData.theme

        authorRelays =
            pubkeyRelays profileViewData.nostr profile.pubKey

        userPubKey =
            Shared.loggedInPubKey profileViewData.loginStatus

        zapRelays =
            extendedZapRelays authorRelays profileViewData.nostr userPubKey

        linkElementWrapper =
            linkElementForProfile profile profileViewData.validation
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.space_x_4
            , Tw.px_4
            , Tw.h_20
            , Tw.max_w_xs
            , Bp.sm
                [ Tw.max_w_md
                ]
            , Bp.md
                [ Tw.max_w_sm
                ]
            ]
        ]
        [ viewProfileImageAuthorCard linkElementWrapper (Just profile)
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.flex_grow
                , Tw.min_w_48
                , Bp.sm
                    [ Tw.w_96
                    ]
                , Bp.lg
                    [ Tw.w_60
                    ]
                , Bp.xl
                    [ Tw.w_72
                    ]
                ]
            ]
            [ h4
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleH4)
                [ linkElementWrapper
                    [ text (profileDisplayName profile.pubKey profile) ]
                ]
            , linkElementWrapper [ viewNip05 styles profile ]
            , viewLNAddress styles profile zapRelays
            ]
        , followBookmarkElement profile.pubKey profileViewData.following
        ]


viewProfileImageAuthorCard : (List (Html msg) -> Html msg) -> Maybe Profile -> Html msg
viewProfileImageAuthorCard linkElement maybeProfile =
    div
        [ css
            [ Tw.min_w_16
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src <| profilePicture 64 maybeProfile
                , Attr.alt "Avatar"
                , css
                    [ Tw.w_16
                    , Tw.h_16
                    , Tw.rounded_full
                    ]
                ]
                []
            ]
        ]


followBookmarkElement : PubKey -> FollowType msg -> Html msg
followBookmarkElement profilePubKey following =
    case following of
        Following msg ->
            div
                [ css
                    [ Tw.cursor_pointer
                    ]
                , Events.onClick (msg profilePubKey)
                ]
                [ Icon.MaterialIcon Icon.MaterialBookmark 30 Icon.Inherit
                    |> Icon.view
                ]

        NotFollowing msg ->
            div
                [ css
                    [ Tw.cursor_pointer
                    ]
                , Events.onClick (msg profilePubKey)
                ]
                [ Icon.MaterialIcon Icon.MaterialBookmarkBorder 30 Icon.Inherit
                    |> Icon.view
                ]

        UnknownFollowing ->
            emptyHtml


viewProfile : Profile -> ProfileViewData msg -> Html msg
viewProfile profile profileViewData =
    let
        styles =
            stylesForTheme profileViewData.theme

        authorRelays =
            pubkeyRelays profileViewData.nostr profile.pubKey

        userPubKey =
            Shared.loggedInPubKey profileViewData.loginStatus

        zapRelays =
            extendedZapRelays authorRelays profileViewData.nostr userPubKey
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.space_y_2
            , Tw.mx_4
            , Tw.mb_6
            ]
        ]
        [ viewBanner profile.banner
        , div
            [ css
                [ Tw.flex
                , Bp.md [ Tw.flex_row ]
                , Tw.flex_col
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
                , viewNip05 styles profile
                , viewLNAddress styles profile zapRelays
                , viewNpub styles profile
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


viewNip05 : Styles msg -> Profile -> Html msg
viewNip05 styles profile =
    case profile.nip05 of
        Just nip05 ->
            p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++ [ css [ Tw.overflow_hidden, Tw.text_ellipsis ] ])
                [ text <| Nip05.nip05ToDisplayString nip05 ]

        Nothing ->
            emptyHtml


viewLNAddress : Styles msg -> Profile -> Set String -> Html msg
viewLNAddress styles profile zapRelays =
    profile.lud16
        |> Maybe.map
            (\lud16 ->
                p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++ [ css [ Tw.flex, Tw.items_center, Tw.overflow_hidden, Tw.text_ellipsis ] ])
                    [ zapButton profile.pubKey Nothing zapRelays "0"
                    , text <| lud16
                    ]
            )
        |> Maybe.withDefault emptyHtml


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
                [ text <| shortenedPubKey 11 nip19 ]

        Nothing ->
            emptyHtml


viewBanner : Maybe String -> Html msg
viewBanner maybeImage =
    let
        imageSrc =
            Maybe.withDefault "/images/pareto-banner.png" maybeImage
    in
    div
        [ css
            [ Tw.w_full
            , Bp.lg [ Tw.max_h_80 ]
            , Bp.md [ Tw.max_h_52 ]
            , Css.Media.withMediaQuery [ "(min-width: 360px)" ] [ Tw.max_h_36 ]
            , Tw.overflow_clip
            ]
        ]
        [ img
            [ Attr.src imageSrc
            , Attr.alt "Banner Image"
            , css
                [ Tw.w_full
                , Tw.object_cover
                ]
            ]
            []
        ]


viewProfilePubKey : Styles msg -> PubKey -> Html msg
viewProfilePubKey styles pubKey =
    let
        linkElementWrapper =
            linkElementForProfilePubKey pubKey
    in
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.mb_4
            ]
        ]
        [ viewProfileImage linkElementWrapper Nothing ValidationUnknown
        , h2
            (css
                [ Tw.text_sm
                , Tw.font_semibold
                ]
                :: styles.colorStyleGrayscaleTitle
            )
            [ linkElementWrapper [ text (shortenedPubKey 6 pubKey) ] ]
        ]


viewProfileImage : (List (Html msg) -> Html msg) -> Maybe Profile -> ProfileValidation -> Html msg
viewProfileImage linkElement maybeProfile validationStatus =
    div
        [ css
            [ Tw.w_fit
            , Tw.border_color Theme.white
            , Tw.rounded_full
            , Tw.relative
            , Tw.bg_color Theme.white
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src <| profilePicture 112 maybeProfile
                , Attr.alt "Avatar"
                , css
                    [ Bp.xl [ Tw.w_28, Tw.h_28 ]
                    , Bp.lg [ Tw.w_24, Tw.h_24 ]
                    , Bp.md [ Tw.w_16, Tw.h_16 ]
                    , Tw.w_14
                    , Tw.h_14
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
                , Tw.max_w_2
                , Tw.max_h_2
                ]
            ]
            [ validationIcon 16 validationStatus
            ]
        ]


timeParagraph : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Html msg
timeParagraph styles browserEnv maybePublishedAt =
    case maybePublishedAt of
        Just publishedAt ->
            p
                (css [ Tw.text_xs ] :: styles.colorStyleGrayscaleDisabled)
                [ text <| BrowserEnv.formatDate browserEnv publishedAt ]

        Nothing ->
            emptyHtml


followingProfile : Nostr.Model -> PubKey -> (PubKey -> PubKey -> msg) -> (PubKey -> PubKey -> msg) -> Maybe PubKey -> FollowType msg
followingProfile nostr profilePubKey followMsg unfollowMsg maybePubKey =
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
                                                Just (Following (unfollowMsg userPubKey))

                                            else
                                                Nothing

                                        FollowingHashtag _ ->
                                            Nothing
                                )
                            |> List.head
                    )
                |> Maybe.withDefault (NotFollowing (followMsg userPubKey))

        Nothing ->
            UnknownFollowing
