module Pages.Uitest exposing (Model, Msg, page)

import BrowserEnv
import Components.Button
import Components.Calendar
import Components.Categories
import Components.Checkbox
import Components.Dropdown
import Components.EntryField
import Components.Icon
import Components.InteractionButton
import Components.Interactions
import Components.SearchBar
import Components.SharingButtonDialog
import Components.Switch
import Effect exposing (Effect)
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import I18Next
import Layouts
import Layouts.Sidebar
import Material.Icons exposing (category)
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Set
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Color exposing (Color)
import Tailwind.Utilities as Tw
import Translations.Uitest as Translations
import Ui.Styles exposing (Theme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { calendar : Components.Calendar.Model
    , categories : Components.Categories.Model TestCategory
    , checkboxValue : Bool
    , dropdown : Components.Dropdown.Model TestDropdownItem
    , entryFieldValue : String
    , interactions : Components.Interactions.Model
    , searchbar : Components.SearchBar.Model
    , searchValue : Maybe String
    , sharingButtonDialog : Components.SharingButtonDialog.Model
    , switchValue : TestSwitchState
    , textAreaValue : String
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
    let
        ( calendar, calendarMsg ) =
            Components.Calendar.init { selectableRange = Components.Calendar.Past, selectionMode = Components.Calendar.DaySelection, selectedPublishDate = Nothing }
    in
    ( { calendar = calendar
      , categories = Components.Categories.init { selected = Category2 }
      , checkboxValue = True
      , dropdown = Components.Dropdown.init { selected = Just DropdownItem2 }
      , entryFieldValue = ""
      , interactions = Components.Interactions.init
      , searchbar = Components.SearchBar.init { searchText = Nothing }
      , searchValue = Nothing
      , sharingButtonDialog = Components.SharingButtonDialog.init
      , switchValue = SwitchState2
      , textAreaValue = ""
      , theme = shared.theme
      }
    , calendarMsg
        |> Effect.sendCmd
        |> Effect.map CalendarSent
    )



-- UPDATE


type Msg
    = NoOp
    | ButtonClick
    | CategoriesSent (Components.Categories.Msg TestCategory Msg)
    | CategorySelected TestCategory
    | CalendarSent Components.Calendar.Msg
    | CheckboxClicked Bool
    | EntryFieldChanged String
    | OpenComment
    | InteractionsSent (Components.Interactions.Msg Msg)
    | SwitchClicked TestSwitchState
    | DropdownSent (Components.Dropdown.Msg TestDropdownItem Msg)
    | DropdownChanged (Maybe TestDropdownItem)
    | Search (Maybe String)
    | SearchBarSent (Components.SearchBar.Msg Msg)
    | SharingButtonDialogSent Components.SharingButtonDialog.Msg
    | TextAreaChanged String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        ButtonClick ->
            ( model, Effect.none )

        CalendarSent innerMsg ->
            Components.Calendar.update
                { msg = innerMsg
                , model = model.calendar
                , toModel = \calendar -> { model | calendar = calendar }
                , toMsg = CalendarSent
                }

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

        EntryFieldChanged value ->
            ( { model | entryFieldValue = value }, Effect.none )

        OpenComment ->
            ( model
            , Shared.Msg.ShowAlert "Open comment"
                |> Effect.sendSharedMsg
            )

        InteractionsSent innerMsg ->
            Components.Interactions.update
                { browserEnv = shared.browserEnv
                , msg = innerMsg
                , model = Just model.interactions
                , nostr = shared.nostr
                , interactionObject = Components.InteractionButton.PicturePost "ABCDEF" "BEEFCAFE"
                , openCommentMsg = Nothing
                , toModel = \interactionsModel -> { model | interactions = interactionsModel }
                , toMsg = InteractionsSent
                }

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

        SharingButtonDialogSent innerMsg ->
            Components.SharingButtonDialog.update
                { msg = innerMsg
                , model = model.sharingButtonDialog
                , toModel = \sharingButtonDialog -> { model | sharingButtonDialog = sharingButtonDialog }
                , toMsg = SharingButtonDialogSent
                , browserEnv = shared.browserEnv
                }

        TextAreaChanged value ->
            ( { model | textAreaValue = value }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SearchBarSent (Components.SearchBar.subscribe model.searchbar)
        , Components.Interactions.subscriptions model.interactions
            |> Sub.map InteractionsSent
        ]



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
    [ ( "Calendar", calendarElement shared model )
    , ( "Categories selector", categoriesElement shared model )
    , ( "Dropdown listbox", dropdownElement shared model )
    , ( "Checkbox", checkboxElement shared model )
    , ( "Interactions", interactionsElement shared model )
    , ( "Switch", switchElement shared model )
    , ( "primary button", primaryButtonElement shared model )
    , ( "primary button (disabled)", primaryButtonDisabledElement shared model )
    , ( "secondary button", secondaryButtonElement shared model )
    , ( "secondary button (disabled)", secondaryButtonDisabledElement shared model )
    , ( "regular button with icons", regularButtonElement shared model )
    , ( "regular button (disabled)", regularButtonDisabledElement shared model )
    , ( "entry field", entryFieldElement shared model )
    , ( "text area", textAreaElement shared model )
    , ( "sharing button/dialog", sharingButtonDialogElement shared model )
    , ( "search bar", searchbarElement shared model )
    ]



-- calendar


calendarElement : Shared.Model -> Model -> Html Msg
calendarElement shared model =
    div
        [ css
            [ Tw.w_full
            , Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            ]
        ]
        [ Components.Calendar.new
            { model = model.calendar
            , toMsg = CalendarSent
            , browserEnv = shared.browserEnv
            , theme = model.theme
            }
            |> Components.Calendar.view
        , case Components.Calendar.selectedTime model.calendar of
            Just date ->
                Html.text <| "Calendar date: " ++ BrowserEnv.formatDate shared.browserEnv date

            Nothing ->
                Html.text "No date selected"
        ]



-- category selector


categoriesElement : Shared.Model -> Model -> Html Msg
categoriesElement shared model =
    Components.Categories.new
        { model = model.categories
        , toMsg = CategoriesSent
        , onSelect = CategorySelected
        , equals = \category1 category2 -> category1 == category2
        , image = categoryImage
        , categories = availableCategories shared.browserEnv.translations
        , browserEnv = shared.browserEnv
        , theme = shared.theme
        }
        |> Components.Categories.view


categoryImage : Color -> TestCategory -> Maybe (Html Msg)
categoryImage _ category =
    case category of
        Category1 ->
            Nothing

        Category2 ->
            Just <| Graphics.featherSmileIcon 30


availableCategories : I18Next.Translations -> List (Components.Categories.CategoryData TestCategory)
availableCategories translations =
    [ { category = Category1, title = Translations.category1Text [ translations ], testId = "uitest-category1" }
    , { category = Category2, title = Translations.category2Text [ translations ], testId = "uitest-category2" }
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
        |> Components.Dropdown.withTestAttribute "test"
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



-- interactions


interactionsElement : Shared.Model -> Model -> Html Msg
interactionsElement shared model =
    Components.Interactions.new
        { browserEnv = shared.browserEnv
        , model = Just model.interactions
        , toMsg = InteractionsSent
        , theme = shared.theme
        , interactionObject = Components.InteractionButton.PicturePost "abcd" "cdef"
        , nostr = shared.nostr
        , loginStatus = shared.loginStatus
        , showLabel = True
        }
        |> Components.Interactions.withInteractionElements
            [ Components.Interactions.CommentButtonElement (Just OpenComment)
            , Components.Interactions.LikeButtonElement
            , Components.Interactions.RepostButtonElement
            , Components.Interactions.ZapButtonElement "0" Set.empty
            , Components.Interactions.BookmarkButtonElement
            ]
        |> Components.Interactions.view



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
        |> Components.Button.withTestAttribute "primary-button-enabled"
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
        |> Components.Button.withTestAttribute "primary-button-disabled"
        |> Components.Button.view


secondaryButtonElement : Shared.Model -> Model -> Html Msg
secondaryButtonElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withTypeSecondary
        |> Components.Button.withTestAttribute "secondary-button-enabled"
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
        |> Components.Button.withTestAttribute "secondary-button-disabled"
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
        |> Components.Button.withTestAttribute "regular-button-enabled"
        |> Components.Button.view


regularButtonDisabledElement : Shared.Model -> Model -> Html Msg
regularButtonDisabledElement shared model =
    Components.Button.new
        { label = Translations.category1Text [ shared.browserEnv.translations ]
        , onClick = Just ButtonClick
        , theme = model.theme
        }
        |> Components.Button.withDisabled True
        |> Components.Button.withTestAttribute "regular-button-disabled"
        |> Components.Button.view



-- entry field / text area


entryFieldElement : Shared.Model -> Model -> Html Msg
entryFieldElement shared model =
    Components.EntryField.new
        { value = model.entryFieldValue
        , onInput = EntryFieldChanged
        , theme = model.theme
        }
        |> Components.EntryField.withLabel (Translations.entryFieldLabel [ shared.browserEnv.translations ])
        |> Components.EntryField.withPlaceholder (Translations.entryFieldPlaceholder [ shared.browserEnv.translations ])
        |> Components.EntryField.withTestAttribute "test"
        |> Components.EntryField.view


textAreaElement : Shared.Model -> Model -> Html Msg
textAreaElement shared model =
    Components.EntryField.new
        { value = model.entryFieldValue
        , onInput = EntryFieldChanged
        , theme = model.theme
        }
        |> Components.EntryField.withLabel (Translations.textAreaLabel [ shared.browserEnv.translations ])
        |> Components.EntryField.withPlaceholder (Translations.textAreaPlaceholder [ shared.browserEnv.translations ])
        |> Components.EntryField.withRows 5
        |> Components.EntryField.withTestAttribute "test"
        |> Components.EntryField.view



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



-- sharing button/dialog


sharingButtonDialogElement : Shared.Model -> Model -> Html Msg
sharingButtonDialogElement shared model =
    Components.SharingButtonDialog.new
        { model = model.sharingButtonDialog
        , toMsg = SharingButtonDialogSent
        , browserEnv = shared.browserEnv
        , sharingInfo = { url = Pareto.applicationUrl, title = Pareto.client, text = Pareto.applicationDomain, hashtags = Pareto.paretoHashtags }
        , theme = model.theme
        }
        |> Components.SharingButtonDialog.view
