module Components.SearchBar exposing
    ( SearchBar, new
    , CategoryData
    , Model, init
    , Msg, update
    , view
    , subscribe
    )

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon
import Css
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, strong, text)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, href, type_)
import Html.Styled.Events as Events exposing (..)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Svg.Loaders as Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.SearchBar as Translations
import Ui.Styles exposing (Styles)

type SearchBar msg
     = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , onSearch : String -> msg
        , browserEnv : BrowserEnv
        , styles : Styles msg
        }

new :
    { model : Model
    , toMsg : Msg msg -> msg
    , onSearch : String -> msg
    , browserEnv : BrowserEnv
    , styles : Styles msg
    }
    -> SearchBar msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onSearch = props.onSearch
        , browserEnv = props.browserEnv
        , styles = props.styles
        }


type Model
    = Model
        { searchText : String
        }

type alias CategoryData category =
    { category :  category
    , title : String
    }


init : { } -> Model
init props =
    Model
        { searchText = ""
        }

type Msg msg
    = Search
        { onSearch : msg
        }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model-> model
    , toMsg : Msg msg -> msg
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
            Search data ->
                ( Model model
                , Effect.sendMsg data.onSearch 
                )


view : SearchBar msg -> Html msg
view settings =
    viewSearch settings

viewSearch : SearchBar msg -> Html msg
viewSearch (Settings settings) =
    let
        (Model model) =
            (settings.model)

        onClickCategory =
            settings.toMsg (Search { onSearch = settings.onSearch "abc"})

        attrs =
                settings.styles.colorStyleCategoryActiveBackground ++
                settings.styles.colorStyleCategoryActive ++
                settings.styles.colorStyleCategoryActiveBorder
    in
        div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.relative
            , Tw.w_full
            , Bp.sm
                [ Css.property "width" "736px"
                ]
            ]
        ]
        [ div
            (settings.styles.colorStyleGrayscaleMuted ++
            [css
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
            ])
            [ Icon.FeatherIcon FeatherIcons.search
                |> Icon.view
            ]
        , input
            [ Attr.placeholder <| Translations.placeholder [ settings.browserEnv.translations ]
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
            []
        ]
    


subscribe : Model -> Sub (Msg msg)
subscribe model =
    Sub.none
