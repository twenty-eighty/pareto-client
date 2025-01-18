module Components.RelayStatus exposing
    ( RelayStatus, new
    , view
    , Purpose(..)
    )

{-|


## Basic usage

@docs RelayStatus, new
@docs view

-}

import Components.Icon exposing (Icon)
import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events
import I18Next
import Nostr.Relay exposing (Relay, RelayState(..))
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.RelayStatusComponent as Translations
import Ui.Styles



-- SETTINGS


type RelayStatus msg
    = Settings
        { relays : List Relay
        , theme : Ui.Styles.Theme
        , translations : I18Next.Translations
        , purpose : Purpose
        }



-- purpose for connecting to relays


type Purpose
    = LoadingArticle
    | LoadingNote
    | LoadingProfile


new : { relays : List Relay, theme : Ui.Styles.Theme, translations : I18Next.Translations, purpose : Purpose } -> RelayStatus msg
new props =
    Settings
        { relays = props.relays
        , theme = props.theme
        , translations = props.translations
        , purpose = props.purpose
        }



-- VIEW


view : RelayStatus msg -> Html msg
view (Settings settings) =
    let
        styles =
            Ui.Styles.stylesForTheme settings.theme

        headline =
            textForPurpose settings.translations settings.purpose
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            , Tw.m_2
            ]
        ]
        [ h3
            (styles.textStyleH3
                ++ styles.colorStyleGrayscaleTitle
                ++ []
            )
            [ text headline
            ]
        , ul
            [ css
                [ Tw.grid
                , Tw.grid_cols_1
                , Tw.gap_6
                , Tw.w_96
                , Tw.m_2
                , Bp.sm
                    [ Tw.m_4
                    ]
                ]
            ]
            (List.map (viewRelay settings.translations) settings.relays)
        ]


viewRelay : I18Next.Translations -> Relay -> Html msg
viewRelay translations relay =
    let
        ( statusText, statusColor ) =
            relayStateInfo translations relay.state
    in
    li
        [ css
            [ Tw.bg_color Theme.white
            , Tw.p_4
            , Tw.rounded
            , Tw.shadow
            , Tw.transition
            , Css.hover
                [ Tw.shadow_lg
                ]
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.mb_2
                ]
            ]
            [ span
                [ css
                    [ Tw.inline_block
                    , Tw.w_3
                    , Tw.h_3
                    , Tw.rounded_full
                    , Tw.bg_color statusColor
                    , Tw.mr_2
                    ]
                ]
                []
            , span
                [ css
                    [ Tw.text_color Theme.gray_700
                    , Tw.font_semibold
                    ]
                ]
                [ text statusText ]
            ]
        , div
            [ css
                [ Tw.text_sm
                , Tw.text_color Theme.gray_600
                , Tw.break_all
                ]
            ]
            [ text relay.urlWithoutProtocol ]
        ]


relayStateInfo : I18Next.Translations -> RelayState -> ( String, Theme.Color )
relayStateInfo translations state =
    case state of
        RelayStateUnknown ->
            ( Translations.relayStateUnknown [ translations ], Theme.gray_400 )

        RelayDisconnected ->
            ( Translations.relayDisconnected [ translations ], Theme.red_500 )

        RelayConnecting ->
            ( Translations.relayConnecting [ translations ], Theme.yellow_500 )

        RelayConnected ->
            ( Translations.relayConnected [ translations ], Theme.green_500 )

        RelayReady ->
            ( Translations.relayReady [ translations ], Theme.blue_500 )


textForPurpose : I18Next.Translations -> Purpose -> String
textForPurpose translations purpose =
    case purpose of
        LoadingArticle ->
            Translations.loadingArticle [ translations ]

        LoadingProfile ->
            Translations.loadingProfile [ translations ]

        LoadingNote ->
            Translations.loadingNote [ translations ]
