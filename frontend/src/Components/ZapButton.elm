module Components.ZapButton exposing
    ( ZapButton, new
    , view
    , init, update, Model, Msg
    , subscriptions, withInstanceId, withRelayUrls, withoutLabel
    )

{-|


## Basic usage

@docs ZapButton, new
@docs view


## State management

@docs init, update, Model, Msg

-}

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon
import Components.InteractionButton as InteractionButton exposing (InteractionObject(..), pubKeyOfInteractionObject)
import Effect exposing (Effect)
import FeatherIcons exposing (settings)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import I18Next
import Json.Encode as Encode
import Nostr
import Nostr.Event exposing (Kind(..), TagReference(..))
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Relay exposing (websocketUrl)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (LoginStatus, PubKey, RelayUrl, loggedInPubKey)
import Set exposing (Set)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles



-- MODEL


type Model
    = Model InteractionButton.Model


init : Model
init =
    Model InteractionButton.init



-- UPDATE


type Msg
    = InteractionButtonMsg (InteractionButton.Msg Msg)


update : { msg : Msg, model : Model, nostr : Nostr.Model, toModel : Model -> model, toMsg : Msg -> msg, translations : I18Next.Translations } -> ( model, Effect msg )
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
            InteractionButtonMsg interactionMsg ->
                let
                    ( updatedModel, effect ) =
                        InteractionButton.update
                            { msg = interactionMsg
                            , model = model
                            , nostr = props.nostr
                            , toModel = \interactionButtonModel -> Model interactionButtonModel
                            , translations = props.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )



-- SETTINGS


type ZapButton msg
    = Settings
        { browserEnv : BrowserEnv
        , model : Model
        , instanceId : Maybe String
        , interactionObject : InteractionObject
        , loginStatus : LoginStatus
        , nostr : Nostr.Model
        , showLabel : Bool
        , relayUrls : Set RelayUrl
        , toMsg : Msg -> msg
        , theme : Ui.Styles.Theme
        }


new :
    { browserEnv : BrowserEnv
    , model : Model
    , interactionObject : InteractionObject
    , loginStatus : LoginStatus
    , nostr : Nostr.Model
    , toMsg : Msg -> msg
    , theme : Ui.Styles.Theme
    }
    -> ZapButton msg
new props =
    Settings
        { browserEnv = props.browserEnv
        , model = props.model
        , instanceId = Nothing
        , interactionObject = props.interactionObject
        , loginStatus = props.loginStatus
        , nostr = props.nostr
        , showLabel = True
        , relayUrls = Set.empty
        , toMsg = props.toMsg
        , theme = props.theme
        }


withoutLabel : ZapButton msg -> ZapButton msg
withoutLabel (Settings settings) =
    Settings { settings | showLabel = False }


withInstanceId : String -> ZapButton msg -> ZapButton msg
withInstanceId instanceId (Settings settings) =
    Settings { settings | instanceId = Just instanceId }


withRelayUrls : Set RelayUrl -> ZapButton msg -> ZapButton msg
withRelayUrls relayUrls (Settings settings) =
    Settings { settings | relayUrls = relayUrls }



-- VIEW


view : ZapButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        label =
            if settings.showLabel then
                getZapAmount settings.browserEnv settings.nostr settings.interactionObject |> Just

            else
                Nothing

        nip19 =
            nip19Target settings.interactionObject

        maybeNip19String =
            nip19
                |> Nip19.encode
                |> Result.toMaybe

        nip19TargetAttr =
            case nip19 of
                Nip19.NAddr naddrData ->
                    Nip19.NAddr naddrData
                        |> Nip19.encode
                        |> Result.toMaybe
                        |> Maybe.map (\nip19String -> [ ( "data-naddr", nip19String ) ])
                        |> Maybe.withDefault []

                Nip19.NEvent neventData ->
                    Nip19.NEvent neventData
                        |> Nip19.encode
                        |> Result.toMaybe
                        |> Maybe.map (\nip19String -> [ ( "data-note-id", nip19String ) ])
                        |> Maybe.withDefault []

                _ ->
                    []

        pubKey =
            settings.interactionObject
                |> pubKeyOfInteractionObject

        maybeNpub =
            Nip19.encode (Npub pubKey)
                |> Result.toMaybe

        lud16 =
            pubKey
                |> Nostr.getProfile settings.nostr
                |> Maybe.andThen .lud16

        clickAction =
            -- make clickable only if lud16 is set
            if lud16 /= Nothing then
                Just InteractionButton.NoAction

            else
                Nothing

        zapRelays =
            extendedZapRelays settings.relayUrls settings.nostr (settings.loginStatus |> loggedInPubKey)

        instanceIdSuffix =
            settings.instanceId
                |> Maybe.map (\instanceId -> "-" ++ instanceId)
                |> Maybe.withDefault ""

        ( nostrZapAttributes, zapComponent ) =
            Maybe.map2
                (\npub _ ->
                    let
                        buttonId =
                            "zap-button-" ++ (maybeNip19String |> Maybe.withDefault "") ++ instanceIdSuffix
                    in
                    ( [ ( "id", buttonId )
                      , ( "data-npub", npub )
                      , ( "data-relays", zapRelays |> Set.toList |> String.join "," )
                      , ( "data-button-color", "#334155" )
                      ]
                        ++ nip19TargetAttr
                    , Html.node "js-zap-component"
                        [ Attr.property "buttonId" (Encode.string buttonId) ]
                        []
                    )
                )
                maybeNpub
                lud16
                |> Maybe.withDefault ( [], emptyHtml )
    in
    Html.div []
        [ InteractionButton.new
            { model = model
            , unreactedIcon = Icon.FeatherIcon FeatherIcons.zap
            , reactedIcon = Icon.FeatherIcon FeatherIcons.zap
            , reacted = False
            , toMsg = InteractionButtonMsg
            , theme = settings.theme
            }
            |> InteractionButton.withLabel label
            |> InteractionButton.withAttributes nostrZapAttributes
            |> InteractionButton.withOnClickAction clickAction
            |> InteractionButton.view
            |> Html.map settings.toMsg
        , zapComponent
        ]


nip19Target : InteractionObject -> Nip19.NIP19Type
nip19Target interactionObject =
    case interactionObject of
        Article _ ( kind, pubKey, identifier ) ->
            Nip19.NAddr
                { identifier = identifier
                , pubKey = pubKey
                , kind = Nostr.Event.numberForKind kind
                , relays = []
                }

        Comment eventId pubKey ->
            Nip19.NEvent
                { id = eventId
                , author = Just pubKey
                , kind = Just <| Nostr.Event.numberForKind KindComment
                , relays = []
                }

        PicturePost eventId pubKey ->
            Nip19.NEvent
                { id = eventId
                , author = Just pubKey
                , kind = Just <| Nostr.Event.numberForKind KindPicture
                , relays = []
                }



{-
   Extends the given relays with the inbox relays of the pub-key.
-}


extendedZapRelays : Set String -> Nostr.Model -> Maybe PubKey -> Set String
extendedZapRelays zapRelays nostr maybePubKey =
    let
        pubKeyRelays =
            maybePubKey
                |> Maybe.map (pubkeyRelays nostr)
                |> Maybe.withDefault Set.empty

        defaultRelays =
            Set.fromList nostr.defaultRelays
                |> Set.map websocketUrl

        candidateRelays =
            Set.union zapRelays pubKeyRelays
                |> Set.map websocketUrl
    in
    if Set.size candidateRelays == Set.size zapRelays || Set.size candidateRelays == Set.size pubKeyRelays then
        Set.union candidateRelays defaultRelays

    else
        candidateRelays


pubkeyRelays : Nostr.Model -> PubKey -> Set String
pubkeyRelays nostrModel pubKey =
    pubKey
        |> Nostr.getNip65RelaysForPubKey nostrModel
        |> List.map (\( _, relay ) -> websocketUrl relay.urlWithoutProtocol)
        |> Set.fromList


getZapAmount : BrowserEnv -> Nostr.Model -> InteractionObject -> String
getZapAmount browserEnv nostr interactionObject =
    case interactionObject of
        Article _ addressComponents ->
            TagReferenceCode addressComponents
                |> Nostr.getZapReceiptsCountForTagReference nostr
                |> Maybe.withDefault 0
                |> formatZapNum browserEnv

        Comment eventId _ ->
            TagReferenceEventId eventId
                |> Nostr.getZapReceiptsCountForTagReference nostr
                |> Maybe.withDefault 0
                |> formatZapNum browserEnv

        PicturePost eventId _ ->
            TagReferenceEventId eventId
                |> Nostr.getZapReceiptsCountForTagReference nostr
                |> Maybe.withDefault 0
                |> formatZapNum browserEnv


formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv milliSats =
    browserEnv.formatNumber "0 a" <| toFloat (milliSats // 1000)


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    InteractionButton.subscriptions model
        |> Sub.map InteractionButtonMsg
