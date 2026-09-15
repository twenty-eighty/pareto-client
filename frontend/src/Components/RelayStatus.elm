module Components.RelayStatus exposing
    ( RelayStatus, new
    , view
    , Status(..)
    )

{-| Status panel for single-content loads: loading, not found, or failed.

Originally relay-connection focused; relay list is only shown for loading states.

@docs RelayStatus, new
@docs view
@docs Status

-}

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import I18Next
import Nostr.Relay exposing (Relay, RelayState(..))
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.RelayStatusComponent as Translations
import Ui.Styles exposing (Theme(..))



-- SETTINGS


type RelayStatus msg
    = Settings
        { relays : List Relay
        , theme : Ui.Styles.Theme
        , translations : I18Next.Translations
        , status : Status
        }


{-| What the status panel is showing.
-}
type Status
    = LoadingArticle
    | LoadingNote
    | LoadingProfile
    | ArticleNotFound
    | ArticleLoadFailed
    | NoteNotFound
    | NoteLoadFailed


new : { relays : List Relay, theme : Ui.Styles.Theme, translations : I18Next.Translations, status : Status } -> RelayStatus msg
new props =
    Settings
        { relays = props.relays
        , theme = props.theme
        , translations = props.translations
        , status = props.status
        }



-- VIEW


view : RelayStatus msg -> Html msg
view (Settings settings) =
    let
        styles =
            Ui.Styles.stylesForTheme settings.theme

        headline =
            headlineForStatus settings.translations settings.status

        showRelays =
            case settings.status of
                LoadingArticle ->
                    True

                LoadingNote ->
                    True

                LoadingProfile ->
                    True

                ArticleNotFound ->
                    False

                ArticleLoadFailed ->
                    False

                NoteNotFound ->
                    False

                NoteLoadFailed ->
                    False
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
        , if showRelays then
            ul
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

          else
            text ""
        ]


viewRelay : I18Next.Translations -> Relay -> Html msg
viewRelay translations relay =
    let
        ( statusText, statusColor ) =
            relayStateInfo translations relay.state

        styles =
            Ui.Styles.stylesForTheme ParetoTheme
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
                (css
                    [ Tw.font_semibold ]
                    :: styles.colorStyleGrayscaleText
                )
                [ text statusText ]
            ]
        , div
            (css
                [ Tw.text_sm, Tw.break_all ]
                :: styles.colorStyleGrayscaleText
            )
            [ text relay.urlWithoutProtocol ]
        ]


relayStateInfo : I18Next.Translations -> RelayState -> ( String, Theme.Color )
relayStateInfo translations state =
    case state of
        RelayStateUnknown ->
            ( Translations.relayStateUnknown [ translations ], Theme.gray_400 )

        RelayStateNip11RequestFailed _ ->
            ( Translations.nip11RequestFailed [ translations ], Theme.black )

        RelayDisconnected ->
            ( Translations.relayDisconnected [ translations ], Theme.red_500 )

        RelayConnecting ->
            ( Translations.relayConnecting [ translations ], Theme.yellow_500 )

        RelayConnected ->
            ( Translations.relayConnected [ translations ], Theme.green_500 )

        RelayReady ->
            ( Translations.relayReady [ translations ], Theme.blue_500 )


headlineForStatus : I18Next.Translations -> Status -> String
headlineForStatus translations status =
    case status of
        LoadingArticle ->
            Translations.loadingArticle [ translations ]

        LoadingProfile ->
            Translations.loadingProfile [ translations ]

        LoadingNote ->
            Translations.loadingNote [ translations ]

        ArticleNotFound ->
            Translations.articleNotFound [ translations ]

        ArticleLoadFailed ->
            Translations.articleLoadFailed [ translations ]

        NoteNotFound ->
            Translations.noteNotFound [ translations ]

        NoteLoadFailed ->
            Translations.noteLoadFailed [ translations ]
