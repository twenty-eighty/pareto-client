module Components.SearchBar exposing
    ( Model
    , Msg
    , SearchBar
    , init
    , new
    , subscribe
    , update
    , view
    )

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon
import Css
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, div, input)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Time
import Translations.SearchBar as Translations
import Ui.Styles exposing (Styles)


type SearchBar msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , browserEnv : BrowserEnv
        , styles : Styles msg
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , browserEnv : BrowserEnv
    , styles : Styles msg
    }
    -> SearchBar msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , styles = props.styles
        }


type Model
    = Model
        { searchText : Maybe String
        , lastSearchText : Maybe String
        , debounceStatus : DebounceStatus
        }


type DebounceStatus
    = Inactive
    | Active Int -- Remaining time in milliseconds


init : { searchText : Maybe String } -> Model
init props =
    Model
        { searchText = props.searchText
        , lastSearchText = Nothing
        , debounceStatus = Inactive
        }


type Msg msg
    = QueryUpdated (Maybe String)
    | TriggerSearch { onSearch : msg }
    | Tick Time.Posix
    | NoOp msg


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , onSearch : Maybe String -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            TriggerSearch data ->
                ( Model model
                , Effect.sendMsg data.onSearch
                )

            QueryUpdated newQuery ->
                ( Model { model | searchText = newQuery, debounceStatus = Active 500 }
                , Effect.none
                )

            Tick _ ->
                case model.debounceStatus of
                    Inactive ->
                        -- If debounce is inactive, do nothing
                        ( Model model, Effect.none )

                    Active remainingTime ->
                        if remainingTime <= 0 then
                            if model.lastSearchText /= model.searchText then
                                -- Time has run out, trigger search
                                ( Model { model | lastSearchText = model.searchText, debounceStatus = Inactive }
                                , Effect.sendMsg (props.onSearch model.searchText)
                                )

                            else
                                ( Model model
                                , Effect.none
                                )

                        else
                            -- Decrease remaining time
                            ( Model { model | debounceStatus = Active (remainingTime - 100) }
                            , Effect.none
                            )

            NoOp _ ->
                ( Model model, Effect.none )


view : SearchBar msg -> Html msg
view settings =
    viewSearch settings


viewSearch : SearchBar msg -> Html msg
viewSearch (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.relative
            , Tw.w_full
            , Bp.lg
                [ Css.property "width" "800px"
                ]
            , Bp.md
                [ Css.property "width" "640px"
                ]
            , Bp.sm
                [ Css.property "width" "460px"
                ]
            ]
        ]
        [ div
            (settings.styles.colorStyleGrayscaleMuted
                ++ [ css
                        [ Tw.flex
                        , Tw.absolute
                        , Tw.leading_6
                        , Tw.w_10
                        , Tw.h_10
                        , Tw.items_center
                        , Tw.justify_center
                        , Tw.left_0
                        , Tw.top_0
                        , Tw.pointer_events_none
                        ]
                   ]
            )
            [ Icon.FeatherIcon FeatherIcons.search
                |> Icon.view
            ]
        , input
            ((settings.styles.colorStyleBackground
                |> List.map (Attr.map NoOp)
             )
                ++ [ Attr.placeholder <| Translations.placeholder [ settings.browserEnv.translations ]
                   , Attr.value (Maybe.withDefault "" model.searchText)
                   , Attr.type_ "search"
                   , Events.onInput
                        (\searchText ->
                            if searchText /= "" then
                                QueryUpdated (Just searchText)

                            else
                                QueryUpdated Nothing
                        )
                   , css
                        [ Tw.appearance_none
                        , Tw.bg_scroll
                        , Tw.bg_clip_border
                        , Tw.rounded_md
                        , Tw.border_2
                        , Tw.box_border
                        , Tw.cursor_text
                        , Tw.block
                        , Tw.ps_10
                        , Tw.pe_16
                        , Tw.pl_10
                        , Tw.pr_16
                        , Tw.h_10
                        , Tw.w_full
                        ]
                   ]
            )
            []
            |> Html.map settings.toMsg
        ]


subscribe : Model -> Sub (Msg msg)
subscribe (Model model) =
    case model.debounceStatus of
        Inactive ->
            Sub.none

        Active _ ->
            Time.every 100 Tick
