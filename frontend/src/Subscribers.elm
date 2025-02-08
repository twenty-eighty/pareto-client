module Subscribers exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Csv.Encode
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Nostr
import Nostr.Event as Event exposing (AddressComponents, Event, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Profile exposing (profileDisplayName)
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (PubKey)
import Pareto
import Shared
import Shared.Msg
import Time


type alias Subscriber =
    { dnd : Maybe Bool
    , email : String
    , name : Maybe String
    , pubKey : Maybe PubKey
    , source : Maybe String
    , dateSubscription : Maybe Time.Posix
    , dateUnsubscription : Maybe Time.Posix
    , tags : Maybe (List String)
    }


type SubscriberField
    = FieldDnd
    | FieldEmail
    | FieldName
    | FieldPubKey
    | FieldSource
    | FieldDateSubscription
    | FieldDateUnsubscription
    | FieldTags


fieldName : SubscriberField -> String
fieldName field =
    case field of
        FieldDnd ->
            "dnd"

        FieldEmail ->
            "email"

        FieldName ->
            "name"

        FieldPubKey ->
            "pubkey"

        FieldSource ->
            "source"

        FieldDateSubscription ->
            "datesub"

        FieldDateUnsubscription ->
            "dateunsub"

        FieldTags ->
            "tags"


type alias Email =
    String


type Modification
    = Subscription Subscriber
    | Unsubscription Subscriber


subscribersDTag : String
subscribersDTag =
    "pareto-subscribers"


newsletterDTag : String
newsletterDTag =
    "pareto-newsletter-"


modificationToString : Modification -> String
modificationToString modification =
    case modification of
        Subscription _ ->
            "subscribed"

        Unsubscription _ ->
            "unsubscribed"


emptySubscriber : Email -> Subscriber
emptySubscriber email =
    { dnd = Nothing
    , email = email
    , name = Nothing
    , pubKey = Nothing
    , source = Nothing
    , dateSubscription = Nothing
    , dateUnsubscription = Nothing
    , tags = Nothing
    }


load : Nostr.Model -> PubKey -> Shared.Msg.Msg
load nostr userPubKey =
    subscribersEventFilter userPubKey
        |> RequestSubscribers
        |> Nostr.createRequest nostr "Load subscribers" []
        |> Shared.Msg.RequestNostrEvents


loadModifications : Nostr.Model -> PubKey -> Shared.Msg.Msg
loadModifications nostr userPubKey =
    modificationsEventFilter userPubKey
        |> RequestSubscribers
        |> Nostr.createRequest nostr "Load subscribers" []
        |> Shared.Msg.RequestNostrEvents


toCsv : List Subscriber -> Csv.Encode.Csv
toCsv subscribers =
    { headers =
        [ fieldName FieldDnd
        , fieldName FieldEmail
        , fieldName FieldName
        , fieldName FieldPubKey
        , fieldName FieldSource
        , fieldName FieldDateSubscription
        , fieldName FieldDateUnsubscription
        , fieldName FieldTags
        ]
    , records =
        subscribers
            |> List.map
                (\subscriber ->
                    [ Maybe.withDefault False subscriber.dnd |> boolToString
                    , subscriber.email
                    , subscriber.name |> Maybe.withDefault ""
                    , subscriber.pubKey |> Maybe.withDefault ""
                    , subscriber.source |> Maybe.withDefault ""
                    , subscriber.dateSubscription |> Maybe.map Iso8601.fromTime |> Maybe.withDefault ""
                    , subscriber.dateUnsubscription |> Maybe.map Iso8601.fromTime |> Maybe.withDefault ""
                    , subscriber.tags |> Maybe.map (String.join ",") |> Maybe.withDefault ""
                    ]
                )
    }


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"


timeToString : Time.Posix -> String
timeToString value =
    value
        |> Time.posixToMillis
        |> String.fromInt


processModifications : Dict Email Subscriber -> List Modification -> Dict Email Subscriber
processModifications subscribers modifications =
    modifications
        |> List.foldl
            (\modification acc ->
                case modification of
                    Subscription subscriber ->
                        Dict.insert subscriber.email subscriber acc

                    Unsubscription subscriber ->
                        Dict.remove subscriber.email acc
            )
            subscribers


merge : Bool -> Dict Email Subscriber -> List Subscriber -> Dict Email Subscriber
merge overwriteExisting existingSubscribers newSubscribers =
    newSubscribers
        |> List.foldr
            (\newSubscriber acc ->
                if overwriteExisting then
                    Dict.insert newSubscriber.email newSubscriber acc

                else if Dict.member newSubscriber.email acc then
                    acc

                else
                    Dict.insert newSubscriber.email newSubscriber acc
            )
            existingSubscribers


remove : Dict Email Subscriber -> List Email -> Dict Email Subscriber
remove existingSubscribers toBeRemovedEmails =
    toBeRemovedEmails
        |> List.foldl
            (\subscriberEmail acc ->
                Dict.remove subscriberEmail acc
            )
            existingSubscribers


subscribersEventFilter : PubKey -> EventFilter
subscribersEventFilter pubKey =
    { emptyEventFilter
        | authors = Just [ pubKey ]
        , kinds = Just [ KindApplicationSpecificData ]
        , tagReferences = Just [ TagReferenceIdentifier subscribersDTag ]
    }


modificationsEventFilter : PubKey -> EventFilter
modificationsEventFilter pubKey =
    { emptyEventFilter
        | authors = Just [ Pareto.subscriptionServerKey ]
        , kinds = Just [ KindApplicationSpecificData ]
        , tagReferences = Just [ TagReferencePubKey pubKey ]
    }


processEvents : PubKey -> Dict Email Subscriber -> List Modification -> List Event -> ( Dict Email Subscriber, List Modification, List String )
processEvents userPubKey existingSubscribers existingModifications events =
    let
        subscriberEvents =
            events
                |> List.filter (\event -> event.pubKey == userPubKey)

        modificationEvents =
            events
                |> List.filter (\event -> event.pubKey == Pareto.subscriptionServerKey)

        ( subscribersDict, decodingErrors1 ) =
            subscriberEvents
                |> List.map subscribersFromEvent
                |> List.foldl
                    (\result ( subscriberDict, errorList ) ->
                        case result of
                            Ok decodedSubscribers ->
                                let
                                    decodedSubscribersDict =
                                        decodedSubscribers
                                            |> List.map (\decodedSubscriber -> ( decodedSubscriber.email, decodedSubscriber ))
                                            |> Dict.fromList
                                in
                                ( Dict.union subscriberDict decodedSubscribersDict, errorList )

                            Err error ->
                                ( subscriberDict, errorList ++ [ Decode.errorToString error ] )
                    )
                    ( existingSubscribers, [] )

        ( modifications, decodingErrors2 ) =
            modificationEvents
                |> List.map modificationsFromEvent
                |> List.foldl
                    (\result ( modificationsAcc, errorList ) ->
                        case result of
                            Ok decodedModifications ->
                                ( modificationsAcc ++ decodedModifications, errorList )

                            Err error ->
                                ( modificationsAcc, errorList ++ [ Decode.errorToString error ] )
                    )
                    ( existingModifications, [] )
    in
    ( subscribersDict, modifications, decodingErrors1 ++ decodingErrors2 )


subscribersFromEvent : Event -> Result Decode.Error (List Subscriber)
subscribersFromEvent event =
    Decode.decodeString (Decode.field "subscribers" subscribersDecoder) event.content


modificationsFromEvent : Event -> Result Decode.Error (List Modification)
modificationsFromEvent event =
    Decode.decodeString modificationsDecoder event.content


modificationsDecoder : Decode.Decoder (List Modification)
modificationsDecoder =
    Decode.oneOf
        [ Decode.field "subscribe" subscribesDecoder
        , Decode.field "unsubscribe" unsubscribesDecoder
        ]


subscribesDecoder : Decode.Decoder (List Modification)
subscribesDecoder =
    Decode.list subscribeDecoder


subscribeDecoder : Decode.Decoder Modification
subscribeDecoder =
    Decode.field (fieldName FieldEmail) Decode.string
        |> Decode.andThen
            (\email ->
                Decode.succeed <| Subscription (emptySubscriber email)
            )


unsubscribesDecoder : Decode.Decoder (List Modification)
unsubscribesDecoder =
    Decode.list unsubscribeDecoder


unsubscribeDecoder : Decode.Decoder Modification
unsubscribeDecoder =
    Decode.field (fieldName FieldEmail) Decode.string
        |> Decode.andThen
            (\email ->
                Decode.succeed <| Unsubscription (emptySubscriber email)
            )


subscribersDecoder : Decode.Decoder (List Subscriber)
subscribersDecoder =
    Decode.list subscriberDecoder


subscriberDecoder : Decode.Decoder Subscriber
subscriberDecoder =
    Decode.succeed Subscriber
        |> optional (fieldName FieldDnd) (Decode.maybe Decode.bool) Nothing
        |> required (fieldName FieldEmail) Decode.string
        |> optional (fieldName FieldName) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldPubKey) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldSource) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldDateSubscription) (Decode.maybe decodePosixTime) Nothing
        |> optional (fieldName FieldDateUnsubscription) (Decode.maybe decodePosixTime) Nothing
        |> optional (fieldName FieldTags) (Decode.maybe (Decode.list Decode.string)) Nothing


decodePosixTime : Decode.Decoder Time.Posix
decodePosixTime =
    Decode.int
        |> Decode.andThen
            (\millis ->
                millis
                    |> Time.millisToPosix
                    |> Decode.succeed
            )


toCSV : List Subscriber -> String
toCSV subscribers =
    [ fieldName FieldDnd
    , fieldName FieldEmail
    , fieldName FieldName
    , fieldName FieldPubKey
    , fieldName FieldSource
    , fieldName FieldDateSubscription
    , fieldName FieldDateUnsubscription
    , fieldName FieldTags
    ]
        |> String.join ","
        |> (\fieldnames ->
                fieldnames
                    ++ "\n"
                    ++ (List.map subscriberToCsv subscribers
                            |> String.concat
                       )
           )


subscriberToCsv : Subscriber -> String
subscriberToCsv subscriber =
    [ subscriber.dnd |> Maybe.map boolToString |> Maybe.withDefault ""
    , subscriber.email
    , subscriber.name |> Maybe.withDefault ""
    , subscriber.pubKey |> Maybe.withDefault ""
    , subscriber.source |> Maybe.withDefault ""
    , subscriber.dateSubscription |> Maybe.map timeToString |> Maybe.withDefault ""
    , subscriber.dateUnsubscription |> Maybe.map timeToString |> Maybe.withDefault ""
    , subscriber.tags
        |> Maybe.map (String.join ",")
        |> Maybe.map (\tagString -> "\"" ++ tagString ++ "\"")
        |> Maybe.withDefault ""
    ]
        |> String.join ","
        |> String.append "\n"


subscriberDataEvent : BrowserEnv -> PubKey -> List Subscriber -> Event
subscriberDataEvent browserEnv pubKey subscribers =
    { pubKey = pubKey
    , createdAt = browserEnv.now
    , kind = KindApplicationSpecificData
    , tags =
        []
            |> Event.addDTag subscribersDTag
    , content = subscribersToJson subscribers
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }


subscribersToJson : List Subscriber -> String
subscribersToJson subscribers =
    [ ( "subscribers"
      , encodeSubscribers subscribers
      )
    ]
        |> Encode.object
        |> Encode.encode 0


encodeSubscribers : List Subscriber -> Encode.Value
encodeSubscribers subscribers =
    subscribers
        |> List.map encodeSubscriber
        |> Encode.list Encode.object


encodeSubscriber : Subscriber -> List ( String, Encode.Value )
encodeSubscriber subscriber =
    [ ( fieldName FieldEmail, Encode.string subscriber.email )
    ]
        |> addBoolToObject FieldDnd subscriber.dnd
        |> addStringToObject FieldName subscriber.name
        |> addStringToObject FieldPubKey subscriber.pubKey
        |> addStringToObject FieldSource subscriber.source
        |> addDateToObject FieldDateSubscription subscriber.dateSubscription
        |> addDateToObject FieldDateUnsubscription subscriber.dateUnsubscription
        |> addStringListToObject FieldTags subscriber.tags


newsletterSubscribersEvent : Shared.Model -> PubKey -> AddressComponents -> List Subscriber -> Event
newsletterSubscribersEvent shared pubKey articleAddressComponents subscribers =
    let
        ( _, _, identifier ) =
            articleAddressComponents

        senderProfileName =
            Nostr.getProfile shared.nostr pubKey
                |> Maybe.map (profileDisplayName pubKey)
    in
    { pubKey = pubKey
    , createdAt = shared.browserEnv.now
    , kind = KindApplicationSpecificData
    , tags =
        []
            -- create unique identifier for every newsletter
            |> Event.addDTag (newsletterDTag ++ identifier)
            -- reference article to be sent as newsletter
            |> Event.addAddressTag articleAddressComponents
            -- reference email gateway as encryption target
            |> Event.addPubKeyTag Pareto.emailGatewayKey Nothing Nothing
    , content = emailSendRequestToJson senderProfileName subscribers
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }


emailSendRequestToJson : Maybe String -> List Subscriber -> String
emailSendRequestToJson maybeSenderName subscribers =
    let
        recipients =
            subscribers
                |> List.filter (\subscriber -> subscriber.dnd /= Just True)
    in
    [ ( "recipients", encodeSubscribers recipients )
    ]
        |> addStringToObject FieldName maybeSenderName
        |> Encode.object
        |> Encode.encode 0


encodeRecipients : List Subscriber -> Encode.Value
encodeRecipients recipients =
    recipients
        |> List.map encodeRecipient
        |> Encode.list Encode.object


encodeRecipient : Subscriber -> List ( String, Encode.Value )
encodeRecipient subscriber =
    [ ( "email", Encode.string subscriber.email )
    ]


addBoolToObject : SubscriberField -> Maybe Bool -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addBoolToObject field maybeValue acc =
    case maybeValue of
        Just value ->
            acc ++ [ ( fieldName field, Encode.bool value ) ]

        Nothing ->
            acc


addStringToObject : SubscriberField -> Maybe String -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addStringToObject field maybeValue acc =
    case maybeValue of
        Just value ->
            acc ++ [ ( fieldName field, Encode.string value ) ]

        Nothing ->
            acc


addDateToObject : SubscriberField -> Maybe Time.Posix -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addDateToObject field maybeValue acc =
    case maybeValue of
        Just value ->
            acc ++ [ ( fieldName field, Encode.int <| Time.posixToMillis value ) ]

        Nothing ->
            acc


addStringListToObject : SubscriberField -> Maybe (List String) -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addStringListToObject field maybeValues acc =
    case maybeValues of
        Just values ->
            acc ++ [ ( fieldName field, Encode.list Encode.string values ) ]

        Nothing ->
            acc
