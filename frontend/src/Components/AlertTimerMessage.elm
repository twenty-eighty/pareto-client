module Components.AlertTimerMessage exposing
    ( AlertTimerMessage
    , Model
    , Msg(..)
    , init
    , new
    , update
    , view
    )

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Process
import Tailwind.Utilities as Tw
import Task
import Ui.Styles exposing (Theme, stylesForTheme)


type AlertTimerMessage
    = Settings
        { model : Model
        , theme : Theme
        }


new :
    { model : Model
    , theme : Theme
    }
    -> AlertTimerMessage
new props =
    Settings
        { model = props.model
        , theme = props.theme
        }


type alias Message =
    { message : String
    , timeout : Int
    }


type Model
    = Model
        { messages : List Message
        }


init : {} -> Model
init _ =
    Model
        { messages = []
        }


type Msg
    = AddMessage String Int
    | RemoveMessage


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
            AddMessage message millisecs ->
                case model.messages of
                    [] ->
                        ( Model { messages = [ { message = message, timeout = millisecs } ] }
                        , delay millisecs RemoveMessage
                            |> Effect.map props.toMsg
                        )

                    messages ->
                        ( Model { messages = messages ++ [ { message = message, timeout = millisecs } ] }
                        , Effect.none
                        )

            RemoveMessage ->
                case model.messages of
                    [] ->
                        ( Model model
                        , Effect.none
                        )

                    [ _ ] ->
                        ( Model { model | messages = [] }
                        , Effect.none
                        )

                    _ :: remaining ->
                        ( Model { model | messages = remaining }
                        , remaining
                            |> List.head
                            |> Maybe.map
                                (\firstMessage ->
                                    delay firstMessage.timeout RemoveMessage
                                        |> Effect.map props.toMsg
                                )
                            |> Maybe.withDefault Effect.none
                        )


delay : Int -> msg -> Effect msg
delay time msg =
    Process.sleep (toFloat time)
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
        |> Effect.sendCmd


view : AlertTimerMessage -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        maybeMessage =
            List.head model.messages
    in
    case maybeMessage of
        Just message ->
            let
                styles =
                    stylesForTheme settings.theme
            in
            div
                (styles.colorStyleBackground
                    ++ styles.colorStyleGrayscaleText
                    ++ [ css
                            [ Tw.fixed
                            , Tw.top_2
                            , Tw.right_2
                            , Tw.border_2
                            , Tw.rounded_md
                            , Tw.px_4
                            , Tw.py_2
                            ]
                       ]
                )
                [ text message.message
                ]

        Nothing ->
            div [] []
