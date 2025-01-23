module Components.ContextMenu exposing (..)

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as Events exposing (..)



-- SETTINGS


type Dropdown item msg
    = Settings
        { model : Model item
        , toMsg : Msg item msg -> msg
        , choices : List item
        , toLabel : item -> String
        , size : Size
        , isDisabled : Bool
        , onChange : Maybe (item -> msg)
        }


new :
    { model : Model item
    , toMsg : Msg item msg -> msg
    , choices : List item
    , toLabel : item -> String
    }
    -> Dropdown item msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , choices = props.choices
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
    (item -> msg)
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
    | UpdatedSearchInput String
    | SelectedItem
        { item : item
        , onChange : Maybe msg
        }


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
                ( Model { model | isMenuOpen = True }
                , Effect.none
                )

            BlurredDropdown ->
                ( Model { model | search = "", isMenuOpen = False }
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
                        , selected = Just data.item
                    }
                , case data.onChange of
                    Just onChange ->
                        Effect.sendMsg onChange

                    Nothing ->
                        Effect.none
                )



-- VIEW


view : Dropdown item msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        onSearchInput : String -> msg
        onSearchInput value =
            settings.toMsg (UpdatedSearchInput value)

        -- View the input of the dropdown, that opens the
        -- menu when focused, and displays the search query
        viewDropdownInput : Html msg
        viewDropdownInput =
            div [ class "dropdown__toggle" ]
                [ input
                    [ class "dropdown__input"
                    , Attr.disabled settings.isDisabled
                    , onInput onSearchInput
                    , onFocus (settings.toMsg FocusedDropdown)
                    , onBlur (settings.toMsg BlurredDropdown)
                    ]
                    []
                , viewSelectedValueOverlay
                ]

        -- If a value is selected, this overlay should
        -- appear over our input field when the menu is closed
        viewSelectedValueOverlay : Html msg
        viewSelectedValueOverlay =
            case model.selected of
                Nothing ->
                    text ""

                Just item ->
                    if model.isMenuOpen then
                        text ""

                    else
                        Html.strong
                            [ class "dropdown__selected" ]
                            [ text (settings.toLabel item) ]

        viewDropdownMenu : Html msg
        viewDropdownMenu =
            if model.isMenuOpen then
                div [ class "dropdown__menu" ]
                    (List.map viewDropdownMenuItem settings.choices)

            else
                text ""

        viewDropdownMenuItem : item -> Html msg
        viewDropdownMenuItem item =
            button
                [ class "dropdown__menu-item"
                , onClick (onMenuItemClick item)
                ]
                [ text (settings.toLabel item)
                ]

        onMenuItemClick : item -> msg
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
        , Attr.classList
            [ ( "dropdown--small", settings.size == Small )
            ]
        ]
        [ viewDropdownInput
        , viewDropdownMenu
        ]
