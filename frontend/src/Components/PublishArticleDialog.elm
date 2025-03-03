module Components.PublishArticleDialog exposing (Model, Msg(..), PublishArticleDialog, PublishingInfo(..), hide, init, new, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.Dropdown as Dropdown
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, div, h2, li, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
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
import Subscribers
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.PublishArticleDialog as Translations
import Ui.Shared
import Ui.Styles exposing (Theme, fontFamilyInter, fontFamilyUnbounded)


type Msg msg
    = ShowDialog
    | CloseDialog
    | ConfigureRelaysClicked
    | PublishClicked (PublishingInfo -> msg)
    | ToggleRelay RelayUrl Bool
    | ReceivedMessage IncomingMessage
    | DropdownSent (Dropdown.Msg PublishingMode (Msg msg))


type Model
    = Model
        { errors : List String
        , relayStates : Dict RelayUrl Bool
        , state : DialogState
        , publishingListbox : Dropdown.Model PublishingMode
        , subscriberEventData : Maybe Subscribers.SubscriberEventData
        }


type DialogState
    = DialogHidden
    | DialogVisible


type PublishingMode
    = PublishArticleOnly
    | PublishNewsletterOnly
    | PublishArticleAndNewsletter


type PublishingInfo
    = ArticleOnly (List RelayUrl)
    | NewsletterOnly Subscribers.SubscriberEventData
    | ArticleAndNewsletter (List RelayUrl) Subscribers.SubscriberEventData


type PublishArticleDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , onPublish : PublishingInfo -> msg
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , onPublish : PublishingInfo -> msg
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


init : {} -> Model
init _ =
    Model
        { errors = []
        , state = DialogHidden
        , publishingListbox = Dropdown.init { selected = Just PublishArticleOnly }

        -- authors shouldn't publish on team relay as normal users can't read from it
        , relayStates = Dict.singleton Pareto.teamRelay False
        , subscriberEventData = Nothing
        }


hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
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
                    -- the list of relays is not stored in the model
                    -- because there may appear more relays after the
                    -- init call
                    relayUrls =
                        Nostr.getWriteRelaysForPubKey props.nostr props.pubKey
                            |> List.filterMap
                                (\relay ->
                                    case Dict.get relay.urlWithoutProtocol model.relayStates of
                                        Just False ->
                                            Nothing

                                        _ ->
                                            Just relay.urlWithoutProtocol
                                )

                    effect =
                        case
                            ( Dropdown.selectedItem model.publishingListbox
                            , model.subscriberEventData
                            )
                        of
                            ( Just PublishArticleOnly, _ ) ->
                                ArticleOnly relayUrls
                                    |> msg
                                    |> Effect.sendMsg

                            ( Just PublishNewsletterOnly, Just subscriberEventData ) ->
                                NewsletterOnly subscriberEventData
                                    |> msg
                                    |> Effect.sendMsg

                            ( Just PublishArticleAndNewsletter, Just subscriberEventData ) ->
                                ArticleAndNewsletter relayUrls subscriberEventData
                                    |> msg
                                    |> Effect.sendMsg

                            ( _, _ ) ->
                                Effect.none
                in
                ( Model model
                , effect
                )

            ReceivedMessage message ->
                updateWithMessage (Model model) props.pubKey message

            DropdownSent innerMsg ->
                let
                    ( newModel, effect ) =
                        Dropdown.update
                            { msg = innerMsg
                            , model = model.publishingListbox
                            , toModel = \dropdown -> Model { model | publishingListbox = dropdown }
                            , toMsg = DropdownSent
                            }
                in
                ( newModel, Effect.map props.toMsg effect )


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


updateRelayChecked : Model -> RelayUrl -> Bool -> ( Model, Effect msg )
updateRelayChecked (Model model) relayUrl newChecked =
    ( Model { model | relayStates = Dict.insert relayUrl newChecked model.relayStates }, Effect.none )


updateWithMessage : Model -> PubKey -> IncomingMessage -> ( Model, Effect msg )
updateWithMessage (Model model) userPubKey message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeEventsKind message.value of
                Ok KindApplicationSpecificData ->
                    case Nostr.External.decodeEvents message.value of
                        Ok events ->
                            let
                                ( maybeSubscriberEventData, _, errors ) =
                                    Subscribers.processEvents userPubKey [] events
                            in
                            ( Model { model | subscriberEventData = maybeSubscriberEventData, errors = model.errors ++ errors }, Effect.none )

                        _ ->
                            ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> (Msg msg -> msg) -> Sub msg
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

        activeSubscribersCount =
            model.subscriberEventData
                |> Maybe.map .active
                |> Maybe.withDefault 0

        optionalListBox =
            if activeSubscribersCount > 0 then
                Dropdown.new
                    { model = model.publishingListbox
                    , toMsg = DropdownSent
                    , choices = [ PublishArticleOnly, PublishNewsletterOnly, PublishArticleAndNewsletter ]
                    , allowNoSelection = False
                    , toLabel = toLabel settings.browserEnv.translations activeSubscribersCount
                    }
                    |> Dropdown.view

            else
                div [] []

        optionalRelaysSection =
            case Dropdown.selectedItem model.publishingListbox of
                Just PublishArticleOnly ->
                    relaysSection (Settings settings) relays

                Just PublishNewsletterOnly ->
                    div [] []

                Just PublishArticleAndNewsletter ->
                    relaysSection (Settings settings) relays

                Nothing ->
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
        [ optionalListBox
        , optionalRelaysSection
        , Button.new
            { label = Translations.publishButtonTitle [ settings.browserEnv.translations ]
            , onClick = Just <| PublishClicked settings.onPublish
            , theme = settings.theme
            }
            |> Button.withTypePrimary
            |> Button.withDisabled (numberOfCheckedRelays settings.model relays < 1)
            |> Button.view
        ]


toLabel : I18Next.Translations -> Int -> Maybe PublishingMode -> String
toLabel translations activeSubscribersCount maybePublishingMode =
    case ( maybePublishingMode, activeSubscribersCount ) of
        ( Just PublishArticleOnly, _ ) ->
            Translations.onlyPublishArticleText [ translations ]

        ( Just PublishNewsletterOnly, 1 ) ->
            Translations.sendOnlyViaEmailSingularCheckboxText [ translations ]

        ( Just PublishNewsletterOnly, count ) ->
            Translations.sendOnlyViaEmailPluralCheckboxText [ translations ] { recipientCount = String.fromInt count }

        ( Just PublishArticleAndNewsletter, 1 ) ->
            Translations.sendAlsoViaEmailSingularCheckboxText [ translations ]

        ( Just PublishArticleAndNewsletter, count ) ->
            Translations.sendAlsoViaEmailPluralCheckboxText [ translations ] { recipientCount = String.fromInt count }

        ( _, _ ) ->
            ""


numberOfCheckedRelays : Model -> List Relay -> Int
numberOfCheckedRelays (Model model) relays =
    relays
        |> List.filter (\relay -> Dict.get relay.urlWithoutProtocol model.relayStates /= Just False)
        |> List.length


relaysSection : PublishArticleDialog msg -> List Relay -> Html (Msg msg)
relaysSection (Settings settings) relays =
    let
        (Model model) =
            settings.model

        relaysStates =
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
