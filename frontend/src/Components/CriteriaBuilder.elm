module Components.CriteriaBuilder exposing (Model, Msg(..), CriteriaBuilder, init, new, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.Dropdown as Dropdown
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, h2, li, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, RelayRole(..), RelayUrl)
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Tailwind.Utilities as Tw
import Translations.CriteriaBuilder as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..))


type Condition
    = HasTagCondition String
    | NotHasTagCondition String
    | AndCondition (List Condition)
    | OrCondition (List Condition)

type Model
    = Model
        { errors : List String
        , selectedTags : List String
        }




type CriteriaBuilder msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , browserEnv : BrowserEnv
        , tags : List String
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , browserEnv : BrowserEnv
    , tags : List String
    , theme : Theme
    }
    -> CriteriaBuilder msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , tags = props.tags
        , theme = props.theme
        }


init : {  } -> Model
init props =
    Model
        { errors = []
        , selectedTags = []
        }


type Msg msg
    = NoOp



update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , testMode : BrowserEnv.TestMode
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
            NoOp ->
                ( Model model, Effect.none )


-- SUBSCRIPTIONS


subscriptions : Model -> (Msg msg -> msg) -> Sub msg
subscriptions _ toMsg =
    Sub.none
        |> Sub.map toMsg



-- VIEW


view : CriteriaBuilder msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
            emptyHtml




viewCriteriaBuilder : CriteriaBuilder msg -> Html (Msg msg)
viewCriteriaBuilder (Settings settings) =
    let
        (Model model) =
            settings.model

    in
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ 
        ]

