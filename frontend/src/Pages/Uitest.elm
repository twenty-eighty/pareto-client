module Pages.Uitest exposing (Model, Msg, page)

import Components.Button
import Components.Categories
import Components.Checkbox
import Components.Dropdown
import Components.Icon
import Components.SearchBar
import Components.Switch
import Effect exposing (Effect)
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import I18Next
import Layouts
import Material.Icons exposing (category)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.Uitest as Translations
import Ui.Styles exposing (Theme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { categories : Components.Categories.Model TestCategory
    , checkboxValue : Bool
    , dropdown : Components.Dropdown.Model TestDropdownItem
    , searchbar : Components.SearchBar.Model
    , searchValue : Maybe String
    , switchValue : TestSwitchState
    , theme : Theme
    }


type TestCategory
    = Category1
    | Category2


type TestDropdownItem
    = DropdownItem1
    | DropdownItem2


type TestSwitchState
    = SwitchState1
    | SwitchState2


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { categories = Components.Categories.init { selected = Category2 }
      , checkboxValue = True
      , dropdown = Components.Dropdown.init { selected = Just DropdownItem2 }
      , searchbar = Components.SearchBar.init { searchText = Nothing }
      , searchValue = Nothing
      , switchValue = SwitchState2
      , theme = shared.theme
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | ButtonClick
    | CategoriesSent (Components.Categories.Msg TestCategory Msg)
    | CategorySelected TestCategory
    | CheckboxClicked Bool
    | SwitchClicked TestSwitchState
    | DropdownSent (Components.Dropdown.Msg TestDropdownItem Msg)
    | DropdownChanged (Maybe TestDropdownItem)
    | Search (Maybe String)
    | SearchBarSent (Components.SearchBar.Msg Msg)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        ButtonClick ->
            ( model, Effect.none )

        CategoriesSent innerMsg ->
            Components.Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
                , toMsg = CategoriesSent
                }

        CategorySelected _ ->
            ( model, Effect.none )

        CheckboxClicked value ->
            ( { model | checkboxValue = value }, Effect.none )

        SwitchClicked value ->
            ( { model | switchValue = value }, Effect.none )

        DropdownSent innerMsg ->
            Components.Dropdown.update
                { msg = innerMsg
                , model = model.dropdown
                , toModel = \dropdown -> { model | dropdown = dropdown }
                , toMsg = DropdownSent
                }

        DropdownChanged _ ->
            ( model, Effect.none )

        Search maybeSearchText ->
            ( { model | searchValue = maybeSearchText }, Effect.none )

        SearchBarSent innerMsg ->
            Components.SearchBar.update
                { msg = innerMsg
                , model = model.searchbar
                , toModel = \searchbar -> { model | searchbar = searchbar }
                , toMsg = SearchBarSent
                , onSearch = Search
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SearchBarSent (Components.SearchBar.subscribe model.searchbar)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.dialogTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.p_4
                ]
            ]
            [ testElements shared model
            ]
        ]
    }


testElements : Shared.Model -> Model -> Html Msg
testElements shared model =
    div
        [ css
            [ Tw.w_full
            , Tw.gap_4
            , Tw.grid
            , Tw.grid_cols_1
            , Bp.lg
                [ Tw.grid_cols_2
                ]
            ]
        ]
        (List.map viewElement (elementList shared model))


viewElement : ( String, Html Msg ) -> Html Msg
viewElement ( description, output ) =
    div []
        [ output
        , Html.text description
        ]


elementList : Shared.Model -> Model -> List ( String, Html Msg )
elementList shared model =
    [ ( "Categories selector", categoriesElement shared model )
    , ( "Dropdown listbox", dropdownElement shared model )
    , ( "Checkbox", checkboxElement shared model )
    , ( "Switch", switchElement shared model )
    , ( "primary button", primaryButtonElement shared model )
    , ( "primary button (disabled)", primaryButtonDisabledElement shared model )
    , ( "secondary button", secondaryButtonElement shared model )
    , ( "secondary button (disabled)", secondaryButtonDisabledElement shared model )
    , ( "regular button", regularButtonElement shared model )
    , ( "regular button (disabled)", regularButtonDisabledElement shared model )
    , ( "search bar", searchbarElement shared model )
    ]



-- category selector


categoriesElement : Shared.Model -> Model -> Html Msg
categoriesElement shared model =
    let
        styles =
            Ui.Styles.stylesForTheme model.theme
    in
    Components.Categories.new
        { model = model.categories
        , toMsg = CategoriesSent
        , onSelect = CategorySelected
        , equals = \category1 category2 -> category1 == category2
        , image = categoryImage
        , categories = availableCategories shared.browserEnv.translations
        , browserEnv = shared.browserEnv
        , styles = styles
        }
        |> Components.Categories.view


categoryImage : TestCategory -> Maybe (Html Msg)
categoryImage category =
    case category of
        Category1 ->
            Nothing

        Category2 ->
            Just <| Graphics.featherSmileIcon 30


availableCategories : I18Next.Translations -> List (Components.Categories.CategoryData TestCategory)
availableCategories translations =
    [ { category = Category1, title = Translations.category1Text [ translations ] }
    , { category = Category2, title = Translations.category2Text [ translations ] }
    ]



-- dropdown listbox


dropdownElement : Shared.Model -> Model -> Html Msg
dropdownElement shared model =
    Components.Dropdown.new
        { model = model.dropdown
        , toMsg = DropdownSent
        , choices = [ DropdownItem1, DropdownItem2 ]
        , allowNoSelection = True
        , toLabel = dropdownItemToText shared.browserEnv.translations
        }
        |> Components.Dropdown.withOnChange DropdownChanged
        |> Components.Dropdown.view


dropdownItemToText : I18Next.Translations -> Maybe TestDropdownItem -> String
dropdownItemToText translations maybeDropdownItem =
    case maybeDropdownItem of
        Just DropdownItem1 ->
            Translations.dropdownItem1Text [ translations ]

        Just DropdownItem2 ->
            Translations.dropdownItem2Text [ translations ]

        Nothing ->
            Translations.noDropdownItemText [ translations ]



-- checkbox


checkboxElement : Shared.Model -> Model -> Html Msg
checkboxElement shared model =
    Components.Checkbox.new
        { checked = model.checkboxValue
        , label = Translations.checkboxLabel [ shared.browserEnv.translations ]
        , onClick = CheckboxClicked
        , theme = model.theme
        }
        |> Components.Checkbox.view



-- switch


switchElement : Shared.Model -> Model -> Html Msg
switchElement shared model =
    Components.Switch.new
        { id = "switch-id-1"
        , labelOff = Translations.switchOffLabel [ shared.browserEnv.translations ]
        , labelOn = Translations.switchOnLabel [ shared.browserEnv.translations ]
        , onClick = SwitchClicked
        , state = model.switchValue
        , stateOff = SwitchState1
        , stateOn = SwitchState2
        , theme = model.theme
        }
        |> Components.Switch.view



-- buttons


primaryButtonElement : Shared.Model -> Model -> Html Msg
primaryButtonElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withTypePrimary
        |> Components.Button.view


primaryButtonDisabledElement : Shared.Model -> Model -> Html Msg
primaryButtonDisabledElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withTypePrimary
        |> Components.Button.withDisabled True
        |> Components.Button.view


secondaryButtonElement : Shared.Model -> Model -> Html Msg
secondaryButtonElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withTypeSecondary
        |> Components.Button.view


secondaryButtonDisabledElement : Shared.Model -> Model -> Html Msg
secondaryButtonDisabledElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withTypeSecondary
        |> Components.Button.withDisabled True
        |> Components.Button.view


regularButtonElement : Shared.Model -> Model -> Html Msg
regularButtonElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withIconLeft (Components.Icon.FeatherIcon FeatherIcons.feather)
        |> Components.Button.withIconRight (Components.Icon.MaterialIcon Components.Icon.MaterialFavorite 20 Components.Icon.Inherit)
        |> Components.Button.view


regularButtonDisabledElement : Shared.Model -> Model -> Html Msg
regularButtonDisabledElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withDisabled True
        |> Components.Button.view



-- search bar


searchbarElement : Shared.Model -> Model -> Html Msg
searchbarElement shared model =
    let
        styles =
            Ui.Styles.stylesForTheme model.theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            ]
        ]
        [ Components.SearchBar.new
            { model = model.searchbar
            , toMsg = SearchBarSent
            , browserEnv = shared.browserEnv
            , styles = styles
            }
            |> Components.SearchBar.view
        , case model.searchValue of
            Just searchValue ->
                Html.text <| "Search value: '" ++ searchValue ++ "'"

            Nothing ->
                Html.text "No search value"
        ]
