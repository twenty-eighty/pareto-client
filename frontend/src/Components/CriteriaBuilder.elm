module Components.CriteriaBuilder exposing (Model, Msg(..), CriteriaBuilder, init, new, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.Dropdown as Dropdown
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, h2, li, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
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

type AllOrOneSelection
    = AnyTags
    | AllSelectedTags
    | AtLeastOneSelectedTag

type Model
    = Model
        { dropdown : Dropdown.Model AllOrOneSelection
        , errors : List String
        , selectedTags : List String
        }




type CriteriaBuilder msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , browserEnv : BrowserEnv
        , tags : List String
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg -> msg
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
        { dropdown = Dropdown.init { selected = Just AllSelectedTags }
        , errors = []
        , selectedTags = []
        }


type Msg
    = UpdateTagChecked String Bool
    | DropdownSent (Dropdown.Msg AllOrOneSelection Msg)
    | DropdownChanged (Maybe AllOrOneSelection)



update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
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
            UpdateTagChecked tag checked ->
                if checked then
                    ( Model { model | selectedTags = tag :: model.selectedTags }, Effect.none )
                else
                    ( Model { model | selectedTags = List.filter (\t -> t /= tag) model.selectedTags }, Effect.none )

            DropdownSent innerMsg ->
                let
                    ( newModel, effect ) =  
                        Dropdown.update
                            { msg = innerMsg
                            , model = model.dropdown
                            , toModel = \dropdown -> Model { model | dropdown = dropdown }
                            , toMsg = DropdownSent 
                            }
                in
                ( newModel, Effect.map props.toMsg effect )

            DropdownChanged maybeSelection ->
                ( Model  model, Effect.none )


-- SUBSCRIPTIONS


subscriptions : Model -> (Msg -> msg) -> Sub msg
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
    viewCriteriaBuilder dialog
    |> Html.map settings.toMsg


viewCriteriaBuilder : CriteriaBuilder msg -> Html Msg 
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
        [ Dropdown.new
            { model = model.dropdown
            , toMsg = DropdownSent
            , choices = [ AnyTags, AtLeastOneSelectedTag, AllSelectedTags ]
            , allowNoSelection = False
            , toLabel = dropdownItemToText settings.browserEnv.translations
            }
            |> Dropdown.withOnChange DropdownChanged
            |> Dropdown.view
        , viewTagCheckboxes settings.theme settings.tags model.selectedTags
        ]

dropdownItemToText : I18Next.Translations -> Maybe AllOrOneSelection -> String
dropdownItemToText translations maybeItem =
    case maybeItem of
        Just item ->
            case item of
                AnyTags ->
                    Translations.anyTagsText [ translations ]

                AllSelectedTags ->
                    Translations.allSelectedTagsText [ translations ]

                AtLeastOneSelectedTag ->
                    Translations.atLeastOneSelectedTagText [ translations ]

        Nothing ->
            -- there should always be a selection
            ""


viewTagCheckboxes : Theme -> List String -> List String -> Html Msg
viewTagCheckboxes theme tags selectedTags =
    tags
    |> List.map (\tag -> ( tag, List.member tag selectedTags ))
    |> List.map (\( tag, checked ) -> viewTagCheckbox theme tag checked )
    |> div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            ]
        ]

viewTagCheckbox : Theme -> String -> Bool -> Html Msg
viewTagCheckbox theme tag checked =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            ]
        ]
        [ Checkbox.new
            { label = tag
            , checked = checked
            , onClick = UpdateTagChecked tag
            , theme = theme
            }
            |> Checkbox.view
        ]