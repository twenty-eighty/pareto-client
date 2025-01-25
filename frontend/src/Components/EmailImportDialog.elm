module Components.EmailImportDialog exposing (EmailImportDialog, Model, Msg, Subscriber, hide, init, new, show, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, button, div, form, h2, img, input, label, li, p, span, text, textarea, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Http
import Nostr
import Nostr.Relay exposing (Relay)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, RelayUrl)
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Svg.Styled as Svg exposing (path, svg)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.EmailImportDialog as Translations
import Ui.Shared
import Ui.Styles exposing (Theme, fontFamilyInter, fontFamilyUnbounded)


type Msg msg
    = CloseDialog
    | ConfigureRelaysClicked
    | ImportClicked
    | UpdateEmailData String


type Model
    = Model
        { state : DialogState
        }


type DialogState
    = DialogHidden
    | DialogVisibleImport EmailImportData
    | DialogVisibleImported EmailSubscriptionData


type alias EmailImportData =
    { enteredEmails : String
    }


type alias EmailSubscriptionData =
    { subscribers : List Subscriber
    }


type alias Subscriber =
    { email : String
    , name : Maybe String
    }


type EmailImportDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> EmailImportDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , nostr = props.nostr
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model
init props =
    Model
        { state = DialogHidden
        }


show : Model -> Model
show (Model model) =
    Model { model | state = DialogVisibleImport { enteredEmails = "" } }


hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
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

            ConfigureRelaysClicked ->
                ( Model model
                , Effect.none
                )

            ImportClicked ->
                ( Model model
                , Effect.none
                )

            UpdateEmailData enteredEmails ->
                case model.state of
                    DialogVisibleImport emailImportData ->
                        let
                            dataWithUpdate =
                                { emailImportData | enteredEmails = enteredEmails }
                        in
                        ( Model { model | state = DialogVisibleImport dataWithUpdate }
                        , Effect.none
                        )

                    _ ->
                        ( Model model, Effect.none )


view : EmailImportDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            div [] []

        DialogVisibleImport emailImportData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewImportDialog dialog emailImportData ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogVisibleImported emailSubscriptionData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewImportedDialog dialog emailSubscriptionData ]
                CloseDialog
                |> Html.map settings.toMsg


viewImportDialog : EmailImportDialog msg -> EmailImportData -> Html (Msg msg)
viewImportDialog (Settings settings) data =
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
        [ textarea
            [ css
                [ Tw.p_2
                , Tw.w_80
                , Tw.h_40
                ]
            , Attr.value data.enteredEmails
            , Events.onInput UpdateEmailData
            ]
            []
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.importButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| ImportClicked
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


viewImportedDialog : EmailImportDialog msg -> EmailSubscriptionData -> Html (Msg msg)
viewImportedDialog (Settings settings) data =
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
        [ recipientsSection (Settings settings)
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.importButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| ImportClicked
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


recipientsSection : EmailImportDialog msg -> Html (Msg msg)
recipientsSection (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            Ui.Styles.stylesForTheme settings.theme
    in
    div [] []


viewSubscribers : EmailImportDialog msg -> List Subscriber -> Html (Msg msg)
viewSubscribers (Settings settings) subscribers =
    div []
        [ ul
            [ Attr.style "list-style-type" "disc"
            , css
                [ Tw.text_base
                , Tw.text_color Theme.purple_900
                , Tw.mb_2
                , Tw.flex
                , Tw.flex_col
                , Tw.gap_y_2
                ]
            ]
            (List.map (viewSubscriber settings.theme) subscribers)
        ]


viewSubscriber : Theme -> Subscriber -> Html (Msg msg)
viewSubscriber theme subscriber =
    div
        []
        []
