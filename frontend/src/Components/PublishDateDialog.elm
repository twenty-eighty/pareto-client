module Components.PublishDateDialog exposing (Model, Msg, init, new, show, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Calendar as Calendar
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Tailwind.Utilities as Tw
import Translations.PublishDateDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)
import Time exposing (Posix)
import Components.Button as Button


type Msg msg
    = CloseDialog
    | OkButtonClicked msg
    | CalendarSent Calendar.Msg


type Model
    = Model
        { state : DialogState
        }


type DialogState
    = DialogHidden
    | PublishDateSelection Calendar.Model



type PublishDateDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , onCommitDate : Posix -> msg
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , onCommitDate : Posix -> msg
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> PublishDateDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onCommitDate = props.onCommitDate
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : Model
init =
    Model
        { state = DialogHidden
        }


show : Model -> Maybe Posix -> (Model, Effect (Msg msg))
show (Model model) maybeSelectedPublishDate =
    let
        ( calendarModel, cmd ) =
            Calendar.init
                { selectableRange = Calendar.Past
                , selectionMode = Calendar.DaySelection
                , selectedPublishDate = maybeSelectedPublishDate
                }
    in
    ( Model { model | state = PublishDateSelection calendarModel }
    , Cmd.map CalendarSent cmd
        |> Effect.sendCmd
    )


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
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
            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            OkButtonClicked onCommitDate ->
                ( Model { model | state = DialogHidden }
                , onCommitDate
                    |> Effect.sendMsg 
                )

            CalendarSent innerMsg ->
                case model.state of
                    PublishDateSelection calendarModel ->
                        let
                            ( modelWithUpdate, effect ) =
                                Calendar.update
                                    { msg = innerMsg
                                    , model = calendarModel
                                    , toModel = \calendar -> Model { model | state = PublishDateSelection calendar }
                                    , toMsg = CalendarSent
                                    }
                        in
                        ( modelWithUpdate, Effect.map props.toMsg effect )

                    _ ->
                        ( Model model, Effect.none )


view : PublishDateDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            emptyHtml

        PublishDateSelection calendarModel ->
            ModalDialog.new
                { title = Translations.dialogTitle [ settings.browserEnv.translations ]
                , buttons =
                    [ Button.new
                        { label = Translations.okButtonTitle [ settings.browserEnv.translations ]
                        , onClick = Calendar.selectedTime calendarModel |> Maybe.map settings.onCommitDate |> Maybe.map OkButtonClicked 
                        , theme = settings.theme
                        }
                        |> Button.view
                    ]
                , content =
                    [ Html.div
                        [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
                        [ Calendar.new
                            { model = calendarModel
                            , toMsg = CalendarSent
                            , browserEnv = settings.browserEnv
                            , theme = settings.theme
                            }
                            |> Calendar.view
                        , viewSelectedDate settings.browserEnv (Calendar.selectedTime calendarModel)
                        ]
                    ]
                , onClose = CloseDialog
                , theme = settings.theme
                }
                |> ModalDialog.view
                |> Html.map settings.toMsg


viewSelectedDate : BrowserEnv -> Maybe Posix -> Html msg
viewSelectedDate browserEnv maybeSelectedPublishDate =
    case maybeSelectedPublishDate of
        Just selectedPublishDate ->
            Html.text (BrowserEnv.formatDate browserEnv selectedPublishDate)

        Nothing ->
            emptyHtml