module Components.Dropdown exposing (..)

import Browser.Dom as Dom
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, button, div, li, strong, text, ul)
import Html.Styled.Attributes as Attr exposing (class, classList, css)
import Html.Styled.Events exposing (..)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Task
import Ui.Styles exposing (Theme(..), darkMode, fontFamilyInter)



-- SETTINGS


type Dropdown item msg
    = Settings
        { model : Model item
        , toMsg : Msg item msg -> msg
        , choices : List item
        , allowNoSelection : Bool
        , toLabel : Maybe item -> String
        , size : Size
        , isDisabled : Bool
        , onChange : Maybe (Maybe item -> msg)
        }


new :
    { model : Model item
    , toMsg : Msg item msg -> msg
    , choices : List item
    , allowNoSelection : Bool
    , toLabel : Maybe item -> String
    }
    -> Dropdown item msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , choices = props.choices
        , allowNoSelection = props.allowNoSelection
        , toLabel = props.toLabel
        , size = Normal
        , isDisabled = False
        , onChange = Nothing
        }



--- MODIFIERS


type Size
    = Normal
    | Small


withSizeSmall : Dropdown item msg -> Dropdown item msg
withSizeSmall (Settings settings) =
    Settings { settings | size = Small }


withDisabled : Dropdown item msg -> Dropdown item msg
withDisabled (Settings settings) =
    Settings { settings | isDisabled = True }


withOnChange :
    (Maybe item -> msg)
    -> Dropdown item msg
    -> Dropdown item msg
withOnChange onChange (Settings settings) =
    Settings { settings | onChange = Just onChange }



-- MODEL


type Model item
    = Model
        { selected : Maybe item
        , search : String
        , isMenuOpen : Bool
        }


init : { selected : Maybe item } -> Model item
init props =
    Model
        { selected = props.selected
        , search = ""
        , isMenuOpen = False
        }



-- UPDATE


type Msg item msg
    = FocusedDropdown
    | BlurredDropdown
    | OpenDropdown
    | CloseDropdown
    | UpdatedSearchInput String
    | SelectedItem
        { item : Maybe item
        , onChange : Maybe msg
        }
    | NoOp


close : Model item -> Model item
close (Model model) =
    Model { model | isMenuOpen = False }


selectItem : Model item -> Maybe item -> Model item
selectItem (Model model) maybeitem =
    Model { model | selected = maybeitem }


selectedItem : Model item -> Maybe item
selectedItem (Model model) =
    model.selected


update :
    { msg : Msg item msg
    , model : Model item
    , toModel : Model item -> model
    , toMsg : Msg item msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model item, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            FocusedDropdown ->
                ( Model model
                , Effect.none
                )

            BlurredDropdown ->
                ( Model { model | search = "", isMenuOpen = False }
                , Effect.none
                )

            OpenDropdown ->
                ( Model { model | isMenuOpen = True }
                , Task.attempt (\_ -> NoOp) (Dom.focus "dropdownMenu")
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            CloseDropdown ->
                ( Model { model | isMenuOpen = False }
                , Effect.none
                )

            UpdatedSearchInput value ->
                ( Model { model | search = value }
                , Effect.none
                )

            SelectedItem data ->
                ( Model
                    { model
                        | search = ""
                        , isMenuOpen = False
                        , selected = data.item
                    }
                , case data.onChange of
                    Just onChange ->
                        Effect.sendMsg onChange

                    Nothing ->
                        Effect.none
                )

            NoOp ->
                ( Model model, Effect.none )



-- VIEW


view : Dropdown item msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        dropdownClickMsg =
            if model.isMenuOpen then
                CloseDropdown

            else
                OpenDropdown

        styles =
            Ui.Styles.stylesForTheme ParetoTheme

        -- View the input of the dropdown, that opens the
        -- menu when focused, and displays the search query
        viewDropdownInput : Html msg
        viewDropdownInput =
            button
                ([ class "dropdown__toggle"
                 , css
                    [ Tw.w_full
                    , Tw.text_left
                    , Tw.border
                    , Tw.rounded_lg
                    , Tw.shadow_sm
                    , Tw.px_4
                    , Tw.py_2
                    , Tw.cursor_pointer
                    , Css.focus
                        [ Tw.outline_none
                        , Tw.ring_2
                        , Tw.ring_color Theme.blue_500
                        , Tw.border_color Theme.blue_500
                        ]
                    ]

                 -- , onBlur (settings.toMsg BlurredDropdown)
                 , onClick (settings.toMsg dropdownClickMsg)
                 ]
                    ++ styles.colorStyleBorders
                    ++ styles.colorStyleGrayscaleText
                    ++ styles.colorStyleBackground
                )
                [ viewSelectedValueOverlay
                ]

        -- If a value is selected, this overlay should
        -- appear over our input field when the menu is closed
        viewSelectedValueOverlay : Html msg
        viewSelectedValueOverlay =
            strong
                [ class "dropdown__selected"
                ]
                [ text (settings.toLabel model.selected) ]

        viewDropdownMenu : Html msg
        viewDropdownMenu =
            if model.isMenuOpen then
                let
                    choices =
                        if settings.allowNoSelection then
                            Nothing :: (settings.choices |> List.map Just)

                        else
                            settings.choices |> List.map Just

                    selectedIndex =
                        choices
                            |> List.indexedMap Tuple.pair
                            |> List.filterMap
                                (\( index, value ) ->
                                    if model.selected == value then
                                        Just index

                                    else
                                        Nothing
                                )
                            |> List.head
                            |> Maybe.withDefault 0
                in
                div
                    ([ Attr.id "dropdownMenu"
                     , Attr.tabindex 1

                     -- position listbox on top of dropdown element, approx. so that selected element is on top of dropdown
                     , Attr.style "top" (String.fromInt (selectedIndex * -40 - 15) ++ "px")
                     , onBlur (settings.toMsg BlurredDropdown)
                     , css
                        [ Tw.absolute
                        , Tw.cursor_pointer
                        , Tw.mt_2
                        , Tw.w_auto
                        , Tw.z_10
                        , Tw.border
                        , Tw.rounded_lg
                        , Tw.shadow_lg
                        ]
                     ]
                        ++ styles.colorStyleBackground
                        ++ styles.colorStyleBorders
                    )
                    [ ul
                        (css [ Tw.py_2 ] :: styles.colorStyleGrayscaleText)
                        (List.map viewDropdownMenuItem choices)
                    ]

            else
                text ""

        viewDropdownMenuItem : Maybe item -> Html msg
        viewDropdownMenuItem item =
            li
                [ onClick (onMenuItemClick item)
                , css
                    [ Tw.block
                    , Tw.px_4
                    , Tw.py_2
                    , Css.hover
                        [ Tw.bg_color Theme.blue_100
                        , darkMode
                            [ Tw.bg_color Theme.blue_900
                            ]
                        ]
                    ]
                ]
                [ text (settings.toLabel item)
                ]

        onMenuItemClick : Maybe item -> msg
        onMenuItemClick item =
            settings.toMsg <|
                case settings.onChange of
                    Just onChange ->
                        SelectedItem
                            { item = item
                            , onChange = Just (onChange item)
                            }

                    Nothing ->
                        SelectedItem
                            { item = item
                            , onChange = Nothing
                            }
    in
    div
        [ class "dropdown"
        , fontFamilyInter
        , classList
            [ ( "dropdown--small", settings.size == Small )
            ]
        , css
            [ Tw.relative
            ]
        ]
        [ viewDropdownInput
        , viewDropdownMenu
        ]
