module Ui.MarketplaceService exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.BookmarkButton as BookmarkButton
import Css
import Dict exposing (Dict)
import Html.Styled exposing (Html, a, div, text)
import Html.Styled.Attributes as Attr exposing (css, href)
import I18Next
import Nostr
import Nostr.Event
import Nostr.MarketplaceService exposing (MarketplaceService)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Author, profileDisplayName)
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (EventId, LoginStatus, PubKey)
import Set
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Article exposing (timeParagraph, viewArticleBookmarkButton, viewHashTags, viewProfileImageSmall, viewProfilePubKey)
import Ui.Links exposing (linkElementForProfile)
import Ui.Styles exposing (Styles, Theme, print)


type alias MarketplaceServicePreviewsData msg =
    { bookmarkButtonMsg : EventId -> BookmarkButton.Msg -> msg
    , bookmarkButtons : Dict EventId BookmarkButton.Model
    , browserEnv : BrowserEnv
    , loginStatus : LoginStatus
    , nostr : Nostr.Model
    , theme : Theme
    }


viewMarketplaceServicePreview : MarketplaceServicePreviewsData msg -> Author -> MarketplaceService -> Html msg
viewMarketplaceServicePreview servicePreviewsData author service =
    let
        styles =
            Ui.Styles.stylesForTheme servicePreviewsData.theme

        ( textWidthAttr, hashtagsHeightAttr ) =
            ( [ Tw.w_80
              , Bp.xxl
                    [ Css.property "width" "950px"
                    ]
              , Bp.xl
                    [ Css.property "width" "700px"
                    ]
              , Bp.lg
                    [ Css.property "width" "600px"
                    ]
              , Bp.md
                    [ Css.property "width" "540px"
                    ]
              , Bp.sm
                    [ Css.property "width" "460px"
                    ]
              ]
            , Css.property "height" "auto"
            )

        serviceLink =
            linkToService service
    in
    div
        (css
            [ Tw.pb_6
            , Tw.border_b
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_3
            , Tw.px_6
            , Bp.xxl
                [ Css.property "width" "1024px"
                ]
            , Bp.xl
                [ Css.property "width" "800px"
                ]
            , Bp.lg
                [ Css.property "width" "720px"
                ]
            , Bp.md
                [ Css.property "width" "640px"
                ]
            , Bp.sm
                [ Tw.h_auto
                , Css.property "width" "550px"
                ]
            ]
            :: styles.colorStyleBorders
        )
        [ viewAuthorAndDatePreview servicePreviewsData author service
        , div
            [ css
                [ Tw.self_stretch
                , Tw.justify_between
                , Tw.items_start
                , Tw.flex
                , Tw.flex_col
                , Tw.gap_4
                , Bp.lg
                    [ Tw.inline_flex
                    , Tw.gap_10
                    ]
                ]
            ]
            [ --previewListImage articlePreviewsData.browserEnv.environment articlePreviewsData.browserEnv.translations articleUrl article
              div
                [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.inline_flex
                    , print [ Tw.block ]
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex_col
                        , Tw.justify_start
                        , Tw.items_start
                        , Tw.gap_3
                        , Tw.flex
                        , Tw.mb_2
                        , Bp.sm
                            [ Tw.w_80
                            ]
                        ]
                    ]
                    [ viewTitlePreview styles service.title serviceLink textWidthAttr
                    , viewListSummary styles textWidthAttr Nothing servicePreviewsData.browserEnv.translations service service.content
                    , viewServiceAmount service
                    , viewHashTags servicePreviewsData.browserEnv.translations service.hashtags (hashtagsHeightAttr :: textWidthAttr)
                    ]
                ]
            ]
        ]


viewServiceAmount : MarketplaceService -> Html msg
viewServiceAmount service =
    text (String.fromInt service.amount ++ " sats")


viewTitlePreview : Styles msg -> String -> String -> List Css.Style -> Html msg
viewTitlePreview styles title linkUrl textWidthAttr =
    a
        (styles.colorStyleGrayscaleTitle
            ++ styles.textStyleH2
            ++ [ css
                    (Tw.line_clamp_2 :: textWidthAttr)
               , href linkUrl
               , Attr.attribute "target" "_blank_"

               --, Attr.attribute "aria-label" (Translations.linkToArticleAriaLabel [ translations ] { title = title })
               ]
        )
        [ text title ]


linkToService : MarketplaceService -> String
linkToService service =
    let
        articleRelays =
            service.relays
                |> Set.toList
                -- append max 5 relays so the link doesn't get infinitely long
                |> List.take 5
                |> List.map websocketUrl

        maybeServiceNaddr =
            Nip19.NAddr
                { identifier = service.identifier
                , pubKey = service.author
                , kind = Nostr.Event.numberForKind service.kind
                , relays = articleRelays
                }
                |> Nip19.encode
                |> Result.toMaybe
    in
    "https://satshoot.com/" ++ (maybeServiceNaddr |> Maybe.withDefault "")


viewListSummary : Styles msg -> List Css.Style -> Maybe String -> I18Next.Translations -> MarketplaceService -> String -> Html msg
viewListSummary styles textWidthAttr articleUrl translations article summaryText =
    let
        summaryLinesAttr =
            if List.length article.hashtags < 1 then
                [ Tw.line_clamp_5 ]

            else
                [ Tw.line_clamp_3 ]
    in
    a
        [ href (articleUrl |> Maybe.withDefault "")

        --, Attr.attribute "aria-label" (Translations.linkToArticleAriaLabel [ translations ] { title = article.title |> Maybe.withDefault article.id })
        ]
        [ div
            (styles.colorStyleGrayscaleText
                ++ styles.textStyleBody
                ++ [ css
                        (summaryLinesAttr ++ textWidthAttr)
                   ]
            )
            [ text summaryText ]
        ]


viewAuthorAndDatePreview : MarketplaceServicePreviewsData msg -> Author -> MarketplaceService -> Html msg
viewAuthorAndDatePreview servicePreviewsData author service =
    let
        styles =
            Ui.Styles.stylesForTheme servicePreviewsData.theme

        followLinks =
            Nostr.isAuthor servicePreviewsData.nostr service.author
    in
    case author of
        Nostr.Profile.AuthorPubkey pubKey ->
            div
                [ css
                    [ Tw.justify_start
                    , Tw.items_center
                    , Tw.gap_2
                    , Tw.inline_flex
                    ]
                ]
                [ viewProfilePubKey servicePreviewsData.browserEnv.environment servicePreviewsData.browserEnv.translations pubKey
                , timeParagraph styles servicePreviewsData.browserEnv Nothing service.createdAt
                ]

        Nostr.Profile.AuthorProfile profile validationStatus ->
            let
                linkElementWrapper =
                    linkElementForProfile True followLinks profile validationStatus
            in
            div
                [ css
                    [ Tw.justify_between
                    , Tw.gap_2
                    , Tw.inline_flex
                    ]
                ]
                [ div
                    [ css
                        [ Tw.justify_start
                        , Tw.items_center
                        , Tw.gap_2
                        , Tw.inline_flex
                        ]
                    ]
                    [ viewProfileImageSmall servicePreviewsData.browserEnv.environment servicePreviewsData.browserEnv.translations linkElementWrapper (Just profile) validationStatus
                    , div
                        [ css
                            [ Tw.justify_start
                            , Tw.items_start
                            , Tw.gap_2
                            , Tw.flex
                            ]
                        ]
                        [ linkElementWrapper
                            [ div
                                (styles.colorStyleGrayscaleText ++ styles.textStyle14)
                                [ text (profileDisplayName profile.pubKey profile) ]
                            ]
                        , timeParagraph styles servicePreviewsData.browserEnv Nothing service.createdAt
                        ]
                    ]

                --, viewArticleBookmarkButton servicePreviewsData service
                ]
