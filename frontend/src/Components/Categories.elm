module Components.Categories exposing
    ( Categories
    , CategoryData
    , Model
    , Msg
    , init
    , new
    , heightString
    , select
    , selected
    , subscribe
    , update
    , view
    )

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css, attribute)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Breakpoints exposing (lg)
import Tailwind.Utilities as Tw
import Tailwind.Theme exposing (Color)
import Ui.Styles exposing (Theme, darkMode)


type Categories category msg
    = Settings
        { model : Model category
        , toMsg : Msg category msg -> msg
        , onSelect : category -> msg
        , equals : category -> category -> Bool
        , image : Color -> category -> Maybe (Html msg)
        , categories : List (CategoryData category)
        , browserEnv : BrowserEnv
        , theme : Theme
        }


heightString : String
heightString =
    "60px"


select : Model category -> category -> Model category
select (Model model) newCategory =
    Model { model | selected = newCategory }


selected : Model category -> category
selected (Model model) =
    model.selected


new :
    { model : Model category
    , toMsg : Msg category msg -> msg
    , onSelect : category -> msg
    , equals : category -> category -> Bool
    , image : Color -> category -> Maybe (Html msg)
    , categories : List (CategoryData category)
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> Categories category msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onSelect = props.onSelect
        , equals = props.equals
        , image = props.image
        , categories = props.categories
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


type Model category
    = Model
        { selected : category
        }


type alias CategoryData category =
    { category : category
    , title : String
    }


init : { selected : category } -> Model category
init props =
    Model
        { selected = props.selected
        }


type Msg category msg
    = SelectedItem
        { category : category
        , onSelect : msg
        }


update :
    { msg : Msg category msg
    , model : Model category
    , toModel : Model category -> model
    , toMsg : Msg category msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model category, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            SelectedItem data ->
                ( Model
                    { model
                        | selected = data.category
                    }
                , Effect.sendMsg data.onSelect
                )


view : Categories category msg -> Html msg
view categories =
    viewCategories categories


viewCategories : Categories category msg -> Html msg
viewCategories (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    settings.categories
        |> List.map (\categoryData ->
            let
                active =
                    settings.equals model.selected categoryData.category

                styles =
                    Ui.Styles.stylesForTheme settings.theme

                (color, colorDarkMode) =
                    if active then
                        ( styles.color4, styles.color4DarkMode )
                    else
                        ( styles.color3, styles.color3DarkMode )

                imageColor =
                    if settings.browserEnv.darkMode then
                        colorDarkMode
                    else
                        color

                image =
                    settings.image imageColor categoryData.category
            in
            viewCategory color colorDarkMode settings.toMsg settings.onSelect image active categoryData
        )
        |> div
            [ css
                [ Tw.flex
                , lg [ Tw.space_x_4 ]
                , Tw.space_x_2_dot_5
                , Tw.mb_4
                , Tw.px_4
                ]
            ]


viewCategory : Color -> Color -> (Msg category msg -> msg) -> (category -> msg) -> Maybe (Html msg) -> Bool -> CategoryData category -> Html msg
viewCategory color colorDarkMode toMsg onSelect maybeImage active data =
    let
        onClickCategory =
            toMsg (SelectedItem { category = data.category, onSelect = onSelect data.category })

        element =
            if active then
                div

            else
                button

        imageElement =
            maybeImage
                |> Maybe.withDefault (text "")
    in
    element
        [ css
            [ lg [ Tw.px_4 ]
            , Tw.px_0
            , Tw.py_2
            , Tw.rounded_full
            , Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.gap_1
            , Tw.text_color color
            , darkMode
                [ Tw.text_color colorDarkMode
                ]
            ]
        , attribute "aria-label" data.title
        , Events.onClick onClickCategory
        ]
        [ imageElement
        , text data.title
        ]


subscribe : Model category -> Sub (Msg category msg)
subscribe _ =
    Sub.none
