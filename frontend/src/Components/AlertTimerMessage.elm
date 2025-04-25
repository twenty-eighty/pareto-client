module Components.AlertTimerMessage exposing
    ( AlertTimerMessage
    , Model
    , Msg(..)
    , init
    , new
    , update
    , view
    )

import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Process
import Tailwind.Utilities as Tw
import Task
import Ui.Styles exposing (Theme, stylesForTheme)
import Ui.Shared exposing (emptyHtml)


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


init : Model
init =
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
    -> ( model, Cmd msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Cmd msg ) -> ( model, Cmd msg )
        toParentModel ( innerModel, cmd ) =
            ( props.toModel innerModel
            , cmd
            )
    in
    toParentModel <|
        case props.msg of
            AddMessage message millisecs ->
                case model.messages of
                    [] ->
                        ( Model { messages = [ { message = message, timeout = millisecs } ] }
                        , delay millisecs RemoveMessage
                            |> Cmd.map props.toMsg
                        )

                    messages ->
                        ( Model { messages = messages ++ [ { message = message, timeout = millisecs } ] }
                        , Cmd.none
                        )

            RemoveMessage ->
                case model.messages of
                    [] ->
                        ( Model model
                        , Cmd.none
                        )

                    [ _ ] ->
                        ( Model { model | messages = [] }
                        , Cmd.none
                        )

                    _ :: remaining ->
                        ( Model { model | messages = remaining }
                        , remaining
                            |> List.head
                            |> Maybe.map
                                (\firstMessage ->
                                    delay firstMessage.timeout RemoveMessage
                                        |> Cmd.map props.toMsg
                                )
                            |> Maybe.withDefault Cmd.none
                        )


delay : Int -> msg -> Cmd msg
delay time msg =
    Process.sleep (toFloat time)
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


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
            emptyHtml
