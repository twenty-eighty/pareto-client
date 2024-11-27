module Components.Categories exposing
    ( Categories, new
    , CategoryData
    , Model, init
    , Msg, update
    , selected
    , view
    , subscribe
    )

import Auth
import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, strong, text)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, href, type_)
import Html.Styled.Events as Events exposing (..)
import Nostr.Blossom as Blossom exposing (BlobDescriptor)
import Nostr.Nip96 as Nip96 exposing (extendRelativeServerDescriptorUrls)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Styles exposing (Styles)

type Categories category msg
     = Settings
        { model : Model category
        , toMsg : Msg category msg -> msg
        , onSelect : category -> msg
        , categories : List (CategoryData category)
        , browserEnv : BrowserEnv
        , styles : Styles msg
        }

selected : Model category -> category
selected (Model model) =
    model.selected

new :
    { model : Model category
    , toMsg : Msg category msg -> msg
    , onSelect : category -> msg
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
        , categories = props.categories
        , browserEnv = props.browserEnv
        , styles = props.styles
        }


type Model category
    = Model
        { selected : category
        }

type alias CategoryData category =
    { category :  category
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
    , toModel : Model category-> model
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
            (settings.model)
    in
    settings.categories
    |> List.map (\categoryData -> viewCategory settings.styles settings.toMsg settings.onSelect (model.selected == categoryData.category) categoryData)
    |> div
        [ css
            [ Tw.flex
            , Tw.space_x_4
            , Tw.max_w_96
            , Tw.mb_10
            ]
        ]

viewCategory : Styles msg -> (Msg category msg -> msg) -> (category -> msg) -> Bool -> CategoryData category -> Html msg
viewCategory styles toMsg onSelect active data =
    let
        onClickCategory =
            toMsg (SelectedItem { category = data.category, onSelect = onSelect data.category })

        attrs =
            if active then
                styles.colorStyleSitebarItemActiveBackground ++
                styles.colorStyleSitebarItemActive ++
                styles.colorStyleSitebarItemActiveBorder
            else
                styles.colorStyleSitebarItemInactiveBackground ++ styles.colorStyleGrayscaleText
    in
    button
        ([ css
            [ Tw.px_4
            , Tw.py_2
            , Tw.rounded_full
            ]
        , Events.onClick onClickCategory
        ] ++ attrs)
        [ text data.title ]


subscribe : Model category -> Sub (Msg category msg)
subscribe model =
    Sub.none
