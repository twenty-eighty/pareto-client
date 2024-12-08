module Components.PublishArticleDialog exposing (PublishArticleDialog, Model, Msg, new, init, update, view, show, hide)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, button, div, form, h2, img, input, label, li, p, span, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Nostr.Relay exposing (Relay)
import Ports
import Nostr.Relay as Relay
import Nostr.Types exposing (RelayUrl)
import Svg.Styled as Svg exposing (svg, path)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.PublishArticleDialog as Translations
import Shared.Msg exposing (Msg)
import Shared.Model exposing (Model)
import Ui.Shared
import Ui.Styles exposing (Theme, fontFamilyUnbounded, fontFamilyInter)

type Msg msg
    = CloseDialog
    | PublishClicked (List RelayUrl -> msg)
    | ToggleRelay RelayUrl Bool


type Model msg =
    Model
        { state : DialogState
        , relayStates : Dict RelayUrl Bool
        }

type DialogState
    = DialogHidden
    | DialogVisible

type PublishArticleDialog msg
    = Settings
        { model : Model msg
        , toMsg : Msg msg -> msg
        , onPublish : List RelayUrl -> msg
        , relays : List Relay
        , browserEnv : BrowserEnv
        , theme : Theme
        }

new :
    { model : Model msg
    , toMsg : Msg msg -> msg
    , onPublish : List RelayUrl -> msg
    , relays : List Relay
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> PublishArticleDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onPublish = props.onPublish
        , relays = props.relays
        , browserEnv = props.browserEnv
        , theme = props.theme
        }

init : { } -> Model msg
init props =
    Model
        { state = DialogHidden
        , relayStates = Dict.empty
        }

show : Model msg -> Model msg
show (Model model) =
    Model { model | state = DialogVisible }

hide : Model msg -> Model msg
hide (Model model) =
    Model { model | state = DialogHidden }

update :
    { msg : Msg msg
    , model : Model msg
    , toModel : Model msg -> model
    , toMsg : Msg msg -> msg
    , relays : List Relay
    }
    -> ( model, Effect msg )
update props  = 
    let
        (Model model) =
            props.model

        toParentModel : (Model msg, Effect msg) -> (model, Effect msg)
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

            ToggleRelay relayUrl checked ->
                updateRelayChecked (Model model) relayUrl checked

            PublishClicked msg ->
                let
                    -- the list of relays is not stored in the model
                    -- because there may appear more relays after
                    -- init call
                    relayUrls =
                        props.relays
                        |> List.filterMap (\relay ->
                                case Dict.get relay.urlWithoutProtocol model.relayStates of
                                    Just False ->
                                        Nothing

                                    _ ->
                                        Just relay.urlWithoutProtocol
                            )
                in
                ( Model model
                , Effect.sendMsg ( msg relayUrls )
                )

updateRelayChecked : Model msg -> RelayUrl -> Bool -> (Model msg, Effect msg)
updateRelayChecked (Model model) relayUrl newChecked =
    (Model { model | relayStates = Dict.insert relayUrl newChecked model.relayStates }, Effect.none)

view : PublishArticleDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            div [][]

        DialogVisible ->
            Ui.Shared.modalDialog
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewPublishArticleDialog dialog ]
                CloseDialog
            |> Html.map settings.toMsg 
        
    
viewPublishArticleDialog : PublishArticleDialog msg -> Html (Msg msg)
viewPublishArticleDialog (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    div [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ relaysSection (Settings settings)
        -- , zapSplitSection (Settings settings)
        , Button.new
            { label = Translations.publishButtonTitle [ settings.browserEnv.translations ]
            , onClick = PublishClicked settings.onPublish
            , theme = settings.theme
            }
            |> Button.withTypePrimary
            |> Button.withDisabled (numberOfCheckedRelays settings.model settings.relays < 1)
            |> Button.view
        ]

numberOfCheckedRelays : Model msg -> List Relay -> Int
numberOfCheckedRelays (Model model) relays =
    relays
    |> List.filter (\relay -> Dict.get relay.urlWithoutProtocol model.relayStates /= Just False)
    |> List.length

relaysSection : PublishArticleDialog msg -> Html (Msg msg)
relaysSection (Settings settings) =
    let
        (Model model) =
            settings.model

        relays =
            settings.relays
            |> List.map (\relay ->
                    (relay, Dict.get relay.urlWithoutProtocol model.relayStates /= Just False)
                )
    in
    div []
        [
        div
            [ css
                [ Tw.flex
                , Tw.justify_between
                , Tw.items_center
                , Tw.mb_2
                ]
            ]
            [ h2
                [ css
                    [ Tw.text_lg
                    , Tw.text_color Theme.gray_800
                    , Tw.font_bold
                    ]
                , fontFamilyUnbounded
                ]
                [ text <| Translations.relaysTitle [ settings.browserEnv.translations ] ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.justify_between
                , Tw.items_center
                , Tw.mb_2
                ]
            ]
            [ h2
                [ css
                    [ Tw.text_base
                    , Tw.text_color Theme.gray_800
                    ]
                , fontFamilyInter
                ]
                [ text <| Translations.relaysDescription [ settings.browserEnv.translations ] ]
            ]
        , ul
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
            (List.map (viewRelayCheckbox settings.theme) relays)
        ]

viewRelayCheckbox : Theme -> (Relay, Bool) -> Html (Msg msg)
viewRelayCheckbox theme (relay, checked) =
    li
        [ css
            [ Tw.list_none
            , Tw.flex
            , Tw.items_center
            , Tw.justify_between
            , Tw.leading_6
            ]
        ]
        [ Checkbox.new
            { label = relay.urlWithoutProtocol
            , checked = checked
            , onClick = ToggleRelay relay.urlWithoutProtocol
            , theme = theme
            }
            |> Checkbox.withImage (relayIcon relay)
            |> Checkbox.view
        ]

relayIcon : Relay -> String
relayIcon relay =
    relay.nip11
    |> Maybe.andThen .icon
    |> Maybe.withDefault ("https://" ++ relay.urlWithoutProtocol ++ "/favicon.ico")


zapSplitSection : PublishArticleDialog msg -> Html (Msg msg)
zapSplitSection (Settings settings) =
    div []
        [
        div
            [ css
                [ Tw.flex
                , Tw.justify_between
                , Tw.items_center
                , Tw.mt_8
                , Tw.mb_2
                ]
            ]
            [ h2
                [ css
                    [ Tw.text_lg
                    , Tw.text_color Theme.gray_800
                    , Tw.font_bold
                    ]
                , fontFamilyUnbounded
                ]
                [ text <| Translations.revenueTitle [ settings.browserEnv.translations ] ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.justify_between
                , Tw.items_center
                , Tw.mb_6
                ]
            ]
            [ h2
                [ css
                    [ Tw.text_base
                    , Tw.text_color Theme.gray_800
                    ]
                , fontFamilyInter
                ]
                [ text <| Translations.revenueDescription [ settings.browserEnv.translations ] ]
            ]
        ]