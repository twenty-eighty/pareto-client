module Components.Dropdown exposing (..)


import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, li, main_, p, span, strong, text, ul)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, type_)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme


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
            div [ class "dropdown__toggle"
                , css
                    [ Tw.w_full
                    , Tw.text_left
                    , Tw.bg_color Theme.white
                    , Tw.border
                    , Tw.border_color Theme.gray_300
                    , Tw.rounded_lg
                    , Tw.shadow_sm
                    , Tw.px_4
                    , Tw.py_2
                    , Tw.text_color Theme.gray_700
                    , Tw.cursor_pointer
                    , Css.focus
                        [ Tw.outline_none
                        , Tw.ring_2
                        , Tw.ring_color Theme.blue_500
                        , Tw.border_color Theme.blue_500
                        ]
                    ]
                , onClick (settings.toMsg FocusedDropdown)
                ]
                [ {- input
                    [ class "dropdown__input"
                    , type_ "search"
                    , disabled settings.isDisabled
                    , onInput onSearchInput
                    , onFocus (settings.toMsg FocusedDropdown)
                    , onBlur (settings.toMsg BlurredDropdown)
                    ]
                    []
                , -} viewSelectedValueOverlay
                ]

        -- If a value is selected, this overlay should
        -- appear over our input field when the menu is closed
        viewSelectedValueOverlay : Html msg
        viewSelectedValueOverlay = 
            case model.selected of
                Nothing ->
                    text ""

                Just item ->
                    strong
                        [ class "dropdown__selected" ]
                        [ text (settings.toLabel item) ]


        viewDropdownMenu : Html msg
        viewDropdownMenu =
            if model.isMenuOpen then
                div
                    [ Attr.id "dropdownMenu"
                    , css
                        [ Tw.absolute
                        , Tw.cursor_pointer
                        , Tw.mt_2
                        , Tw.w_min
                        , Tw.bg_color Theme.white
                        , Tw.border
                        , Tw.border_color Theme.gray_200
                        , Tw.rounded_lg
                        , Tw.shadow_lg
                        ]
                    ]
                    [ ul
                        [ css
                            [ Tw.py_2
                            , Tw.text_color Theme.gray_700
                            ]
                        ]
                        (List.map viewDropdownMenuItem settings.choices)
                    ]

            else
                text ""
    

        viewDropdownMenuItem : item -> Html msg
        viewDropdownMenuItem item =
                        li [ onClick (onMenuItemClick item)
                            , css
                                [ Tw.block
                                , Tw.px_4
                                , Tw.py_2
                                , Css.hover
                                    [ Tw.bg_color Theme.blue_100
                                    ]
                                ]
                            ]
                            [ text (settings.toLabel item)
                            ]

        onMenuItemClick : item -> msg
        onMenuItemClick item =
            settings.toMsg  <|
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
    div [ class "dropdown"
        , classList
            [ ( "dropdown--small", settings.size == Small )
            ]
        ]
        [ viewDropdownInput
        , viewDropdownMenu
        ]