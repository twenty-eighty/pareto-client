module Subscribers exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Csv.Encode
import Dict exposing (Dict)
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


type alias Subscriber =
    { dnd : Maybe Bool
    , email : String
    , name : Maybe String
    , tags : Maybe (List String)
    }


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
    { headers = [ "email", "name", "tags", "dnd" ]
    , records =
        subscribers
            |> List.map
                (\subscriber ->
                    [ subscriber.email, Maybe.withDefault "" subscriber.name, Maybe.map (String.join ",") subscriber.tags |> Maybe.withDefault "", Maybe.withDefault False subscriber.dnd |> boolToString ]
                )
    }


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"


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
    Decode.field "email" Decode.string
        |> Decode.andThen
            (\email ->
                Decode.succeed <| Subscription (emptySubscriber email)
            )


unsubscribesDecoder : Decode.Decoder (List Modification)
unsubscribesDecoder =
    Decode.list unsubscribeDecoder


unsubscribeDecoder : Decode.Decoder Modification
unsubscribeDecoder =
    Decode.field "email" Decode.string
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
        |> optional "dnd" (Decode.maybe Decode.bool) Nothing
        |> required "email" Decode.string
        |> optional "name" (Decode.maybe Decode.string) Nothing
        |> optional "tags" (Decode.maybe (Decode.list Decode.string)) Nothing


toCSV : List Subscriber -> String
toCSV subscribers =
    "email,name,tags\n"
        ++ (List.map subscriberToCsv subscribers
                |> String.concat
           )


subscriberToCsv : Subscriber -> String
subscriberToCsv subscriber =
    [ subscriber.email
    , Maybe.withDefault "" subscriber.name
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
    [ ( "email", Encode.string subscriber.email )
    ]
        |> addBoolToObject "dnd" subscriber.dnd
        |> addStringToObject "name" subscriber.name
        |> addStringListToObject "tags" subscriber.tags


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
        |> addStringToObject "senderName" maybeSenderName
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


addBoolToObject : String -> Maybe Bool -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addBoolToObject key maybeValue acc =
    case maybeValue of
        Just value ->
            acc ++ [ ( key, Encode.bool value ) ]

        Nothing ->
            acc


addStringToObject : String -> Maybe String -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addStringToObject key maybeValue acc =
    case maybeValue of
        Just value ->
            acc ++ [ ( key, Encode.string value ) ]

        Nothing ->
            acc


addStringListToObject : String -> Maybe (List String) -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
addStringListToObject key maybeValues acc =
    case maybeValues of
        Just values ->
            acc ++ [ ( key, Encode.list Encode.string values ) ]

        Nothing ->
            acc
