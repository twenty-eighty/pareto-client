module Components.ConfirmDialog exposing
    ( ConfirmDialog
    , Model
    , Msg
    , init
    , new
    , show
    , update
    , view
    )

{-| A yes/no dialog. The caller supplies the prompt and receives the stored context when the user confirms.


## Basic usage

@docs ConfirmDialog, Model, Msg, init, new, show, update, view

-}

import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import Html.Styled exposing (Html, p, text)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


type ConfirmDialog context msg
    = Settings
        { model : Model context
        , toMsg : Msg -> msg
        , theme : Theme
        }


type Model context
    = Model (State context)


type State context
    = Hidden
    | Visible (Prompt context)


type alias Prompt context =
    { context : context
    , title : String
    , message : String
    , confirmLabel : String
    , cancelLabel : String
    , danger : Bool
    }


type Msg
    = Confirmed
    | Cancelled


new :
    { model : Model context
    , toMsg : Msg -> msg
    , theme : Theme
    }
    -> ConfirmDialog context msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , theme = props.theme
        }


init : Model context
init =
    Model Hidden


show : Prompt context -> Model context -> Model context
show prompt _ =
    Model (Visible prompt)


update :
    { msg : Msg
    , model : Model context
    , toModel : Model context -> model
    , onConfirm : context -> msg
    }
    -> ( model, Effect msg )
update props =
    case props.msg of
        Cancelled ->
            ( props.toModel (Model Hidden), Effect.none )

        Confirmed ->
            case props.model of
                Model (Visible prompt) ->
                    ( props.toModel (Model Hidden)
                    , Effect.sendMsg (props.onConfirm prompt.context)
                    )

                Model Hidden ->
                    ( props.toModel (Model Hidden), Effect.none )


view : ConfirmDialog context msg -> Html msg
view (Settings settings) =
    case settings.model of
        Model Hidden ->
            emptyHtml

        Model (Visible prompt) ->
            let
                confirmButton =
                    Button.new
                        { label = prompt.confirmLabel
                        , onClick = Just (settings.toMsg Confirmed)
                        , theme = settings.theme
                        }
                        |> Button.withTypePrimary
                        |> applyDanger prompt.danger
                        |> Button.view

                cancelButton =
                    Button.new
                        { label = prompt.cancelLabel
                        , onClick = Just (settings.toMsg Cancelled)
                        , theme = settings.theme
                        }
                        |> Button.withTypeSecondary
                        |> Button.view
            in
            ModalDialog.new
                { title = prompt.title
                , content = [ p [] [ text prompt.message ] ]
                , buttons = [ cancelButton, confirmButton ]
                , onClose = settings.toMsg Cancelled
                , theme = settings.theme
                }
                |> ModalDialog.withFixedWidth
                |> ModalDialog.view


applyDanger : Bool -> Button.Button msg -> Button.Button msg
applyDanger danger button =
    if danger then
        Button.withStyleDanger button

    else
        button
