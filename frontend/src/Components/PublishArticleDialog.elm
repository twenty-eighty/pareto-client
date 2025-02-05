module Components.PublishArticleDialog exposing (Model, Msg(..), PublishArticleDialog, hide, init, new, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, div, h2, li, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Relay as Relay exposing (Relay)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, PubKey, RelayRole(..), RelayUrl)
import Pareto
import Ports
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Subscribers exposing (Email, Subscriber)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.PublishArticleDialog as Translations
import Ui.Shared
import Ui.Styles exposing (Theme, fontFamilyInter, fontFamilyUnbounded)


type Msg msg
    = ShowDialog
    | CloseDialog
    | ConfigureRelaysClicked
    | PublishClicked (List RelayUrl -> Maybe (List Subscriber) -> msg)
    | ToggleRelay RelayUrl Bool
    | UpdateSendViaEmail Bool
    | ReceivedMessage IncomingMessage


type Model msg
    = Model
        { errors : List String
        , relayStates : Dict RelayUrl Bool
        , sendViaEmail : Bool
        , state : DialogState
        , subscribers : Dict Email Subscriber
        }


type DialogState
    = DialogHidden
    | DialogVisible


type PublishArticleDialog msg
    = Settings
        { model : Model msg
        , toMsg : Msg msg -> msg
        , onPublish : List RelayUrl -> Maybe (List Subscriber) -> msg
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model msg
    , toMsg : Msg msg -> msg
    , onPublish : List RelayUrl -> Maybe (List Subscriber) -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> PublishArticleDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onPublish = props.onPublish
        , nostr = props.nostr
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model msg
init _ =
    Model
        { errors = []
        , state = DialogHidden
        , sendViaEmail = False

        -- authors shouldn't publish on team relay as normal users can't read from it
        , relayStates = Dict.singleton Pareto.teamRelay False
        , subscribers = Dict.empty
        }


hide : Model msg -> Model msg
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model msg
    , toModel : Model msg -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model msg, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            ShowDialog ->
                ( Model { model | state = DialogVisible }
                , Subscribers.load props.nostr props.pubKey
                    |> Effect.sendSharedMsg
                )

            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            ToggleRelay relayUrl checked ->
                updateRelayChecked (Model model) relayUrl checked

            UpdateSendViaEmail newState ->
                ( Model { model | sendViaEmail = newState }, Effect.none )

            ConfigureRelaysClicked ->
                let
                    allListRelays =
                        Nostr.getRelayListForPubKey props.nostr props.pubKey
                            |> extendRelayList Pareto.defaultOutboxRelays
                in
                ( Model model
                , sendRelayListCmd props.pubKey allListRelays
                )

            PublishClicked msg ->
                let
                    isBetaTester =
                        Nostr.isBetaTester props.nostr props.pubKey

                    -- the list of relays is not stored in the model
                    -- because there may appear more relays after
                    -- init call
                    relayUrls =
                        if isBetaTester && model.sendViaEmail then
                            Pareto.applicationDataRelays

                        else
                            Nostr.getWriteRelaysForPubKey props.nostr props.pubKey
                                |> List.filterMap
                                    (\relay ->
                                        case Dict.get relay.urlWithoutProtocol model.relayStates of
                                            Just False ->
                                                Nothing

                                            _ ->
                                                Just relay.urlWithoutProtocol
                                    )

                    emailSubscribers =
                        if model.sendViaEmail then
                            Just model.subscribers

                        else
                            Nothing
                in
                ( Model model
                , Effect.sendMsg (msg relayUrls (Maybe.map Dict.values emailSubscribers))
                )

            ReceivedMessage message ->
                updateWithMessage (Model model) props.pubKey message


sendRelayListCmd : PubKey -> List RelayMetadata -> Effect msg
sendRelayListCmd pubKey relays =
    let
        relaysWithProtocol =
            relays
                |> List.map
                    (\relay ->
                        { relay | url = "wss://" ++ relay.url }
                    )

        relayUrls =
            relays
                |> List.filterMap
                    (\relay ->
                        if relay.role == WriteRelay || relay.role == ReadWriteRelay then
                            Just relay.url

                        else
                            Nothing
                    )
    in
    eventWithRelayList pubKey relaysWithProtocol
        |> SendRelayList relayUrls
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


updateRelayChecked : Model msg -> RelayUrl -> Bool -> ( Model msg, Effect msg )
updateRelayChecked (Model model) relayUrl newChecked =
    ( Model { model | relayStates = Dict.insert relayUrl newChecked model.relayStates }, Effect.none )


updateWithMessage : Model msg -> PubKey -> IncomingMessage -> ( Model msg, Effect msg )
updateWithMessage (Model model) userPubKey message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeEventsKind message.value of
                Ok KindApplicationSpecificData ->
                    case Nostr.External.decodeEvents message.value of
                        Ok events ->
                            let
                                ( subscribers, _, errors ) =
                                    Subscribers.processEvents userPubKey model.subscribers [] events
                            in
                            ( Model { model | subscribers = subscribers, errors = model.errors ++ errors }, Effect.none )

                        _ ->
                            ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model msg -> (Msg msg -> msg) -> Sub msg
subscriptions _ toMsg =
    Ports.receiveMessage ReceivedMessage
        |> Sub.map toMsg



-- VIEW


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
            div [] []

        DialogVisible ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewPublishArticleDialog dialog ]
                CloseDialog
                |> Html.map settings.toMsg


viewPublishArticleDialog : PublishArticleDialog msg -> Html (Msg msg)
viewPublishArticleDialog (Settings settings) =
    let
        (Model model) =
            settings.model

        relays =
            Nostr.getWriteRelaysForPubKey settings.nostr settings.pubKey

        optionalDeliverySection =
            if Dict.size model.subscribers > 0 then
                deliverySection (Settings settings)

            else
                div [] []
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
        [ relaysSection (Settings settings) relays
        , optionalDeliverySection

        -- , zapSplitSection (Settings settings)
        , Button.new
            { label = Translations.publishButtonTitle [ settings.browserEnv.translations ]
            , onClick = Just <| PublishClicked settings.onPublish
            , theme = settings.theme
            }
            |> Button.withTypePrimary
            |> Button.withDisabled (numberOfCheckedRelays settings.model relays < 1)
            |> Button.view
        ]


numberOfCheckedRelays : Model msg -> List Relay -> Int
numberOfCheckedRelays (Model model) relays =
    relays
        |> List.filter (\relay -> Dict.get relay.urlWithoutProtocol model.relayStates /= Just False)
        |> List.length


relaysSection : PublishArticleDialog msg -> List Relay -> Html (Msg msg)
relaysSection (Settings settings) relays =
    let
        (Model model) =
            settings.model

        isBetaTester =
            Nostr.isBetaTester settings.nostr settings.pubKey

        relaysStates =
            -- TODO: This branch is only for test purposes
            if isBetaTester && model.sendViaEmail then
                Pareto.applicationDataRelays
                    |> List.map
                        (\relayUrl ->
                            ( { urlWithoutProtocol = relayUrl
                              , nip11 = Nothing
                              , state = Relay.RelayStateUnknown
                              }
                            , True
                            )
                        )

            else
                relays
                    |> List.map
                        (\relay ->
                            ( relay, Dict.get relay.urlWithoutProtocol model.relayStates /= Just False )
                        )

        styles =
            Ui.Styles.stylesForTheme settings.theme
    in
    if List.length relays > 0 then
        viewRelays (Settings settings) relaysStates

    else
        viewNoRelaysConfigured (Settings settings) styles


viewRelays : PublishArticleDialog msg -> List ( Relay, Bool ) -> Html (Msg msg)
viewRelays (Settings settings) relays =
    div []
        [ div
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


viewNoRelaysConfigured : PublishArticleDialog msg -> Ui.Styles.Styles (Msg msg) -> Html (Msg msg)
viewNoRelaysConfigured (Settings settings) styles =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.justify_between
            , Tw.items_center
            , Tw.gap_2
            , Tw.mb_2
            ]
        ]
        [ h2
            (styles.textStyleH2 ++ styles.colorStyleGrayscaleTitle)
            [ text <| Translations.noRelaysConfiguredTitle [ settings.browserEnv.translations ] ]
        , p
            (styles.textStyleBody ++ styles.colorStyleGrayscaleText)
            [ text <| Translations.noRelaysConfiguredText [ settings.browserEnv.translations ] ]
        , Button.new
            { label = Translations.configureRelaysButtonTitle [ settings.browserEnv.translations ]
            , onClick = Just <| ConfigureRelaysClicked
            , theme = settings.theme
            }
            |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.settings)
            |> Button.withTypePrimary
            |> Button.view
        ]


viewRelayCheckbox : Theme -> ( Relay, Bool ) -> Html (Msg msg)
viewRelayCheckbox theme ( relay, checked ) =
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
            |> Checkbox.withImage (Relay.iconUrl relay)
            |> Checkbox.view
        ]


deliverySection : PublishArticleDialog msg -> Html (Msg msg)
deliverySection (Settings settings) =
    let
        (Model model) =
            settings.model

        subscriberCount =
            Dict.size model.subscribers

        checkboxText =
            if subscriberCount == 1 then
                Translations.sendAlsoViaEmailSingularCheckboxText [ settings.browserEnv.translations ]

            else
                Translations.sendAlsoViaEmailPluralCheckboxText [ settings.browserEnv.translations ] { recipientCount = String.fromInt subscriberCount }
    in
    div []
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_between
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
                [ text <| Translations.deliveryTitle [ settings.browserEnv.translations ] ]
            , div
                [ css
                    [ Tw.font_bold
                    ]
                ]
                [ text "TEST MODE: when sending article via email it will only be published to a test relay."
                ]
            ]
        , Checkbox.new
            { label = checkboxText
            , checked = model.sendViaEmail
            , onClick = UpdateSendViaEmail
            , theme = settings.theme
            }
            |> Checkbox.view
        ]



{-
   zapSplitSection : PublishArticleDialog msg -> Html (Msg msg)
   zapSplitSection (Settings settings) =
       div []
           [ div
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
-}
