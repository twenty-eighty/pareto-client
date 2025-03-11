module Components.Categories exposing
    ( Categories
    , CategoryData
    , Model
    , Msg
    , init
    , new
    , select
    , selected
    , subscribe
    , update
    , view
    )

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Breakpoints exposing (lg)
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Styles)


type Categories category msg
    = Settings
        { model : Model category
        , toMsg : Msg category msg -> msg
        , onSelect : category -> msg
        , equals : category -> category -> Bool
        , image : category -> Maybe (Html msg)
        , categories : List (CategoryData category)
        , browserEnv : BrowserEnv
        , styles : Styles msg
        }


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
    , image : category -> Maybe (Html msg)
    , categories : List (CategoryData category)
    , browserEnv : BrowserEnv
    , styles : Styles msg
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
        , styles = props.styles
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
        |> List.map (\categoryData -> viewCategory settings.styles settings.toMsg settings.onSelect (settings.image categoryData.category) (settings.equals model.selected categoryData.category) categoryData)
        |> div
            [ css
                [ Tw.flex
                , lg [ Tw.space_x_4 ]
                , Tw.space_x_2_dot_5
                , Tw.mb_10
                , Tw.px_4
                ]
            ]


viewCategory : Styles msg -> (Msg category msg -> msg) -> (category -> msg) -> Maybe (Html msg) -> Bool -> CategoryData category -> Html msg
viewCategory styles toMsg onSelect maybeImage active data =
    let
        onClickCategory =
            toMsg (SelectedItem { category = data.category, onSelect = onSelect data.category })

        ( element, attrs ) =
            if active then
                ( div
                , styles.colorStyleCategoryActive
                )

            else
                ( button
                , styles.colorStyleCategoryInactive
                )

        imageElement =
            maybeImage
                |> Maybe.withDefault (text "")
    in
    element
        ([ css
            [ lg [ Tw.px_4 ]
            , Tw.px_0
            , Tw.py_2
            , Tw.rounded_full
            , Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.gap_1
            ]
         , Events.onClick onClickCategory
         ]
            ++ attrs
        )
        [ imageElement
        , text data.title
        ]


subscribe : Model category -> Sub (Msg category msg)
subscribe _ =
    Sub.none
