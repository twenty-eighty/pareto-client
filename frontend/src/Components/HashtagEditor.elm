module Components.HashtagEditor exposing
    ( Model
    , Msg
    , HashtagEditor
    , getHashtags
    , init
    , new
    , update
    , view
    )

import Components.EntryField as EntryField
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import I18Next exposing (Translations)
import Translations.HashtagEditor as Translations
import Ui.Styles exposing (Theme)

type HashtagEditor msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , translations : Translations
        , theme : Theme
        }

getHashtags : Model -> List String
getHashtags (Model model) =
    model.editValue
    |> String.split ","
    |> List.map String.trim
    |> List.filter (String.isEmpty >> not)

new :
    { model : Model
    , toMsg : Msg -> msg
    , translations : Translations
    , theme : Theme
    }
    -> HashtagEditor msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , translations = props.translations
        , theme = props.theme
        }


type Model
    = Model
        { editValue : String
        }


init : { hashtags : List String } -> Model
init props =
    Model
        { editValue = props.hashtags |> String.join ", "
        }


type Msg
    = UpdateHashtags String

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
            UpdateHashtags hashtags ->
                ( Model { model | editValue = hashtags }
                , Effect.none
                )


view : HashtagEditor msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    EntryField.new
        { value = model.editValue
        , onInput = UpdateHashtags
        , theme = settings.theme
        }
        |> EntryField.withLabel (Translations.label [settings.translations])
        |> EntryField.withPlaceholder (Translations.placeholder [settings.translations])
        |> EntryField.view
        |> Html.map settings.toMsg
