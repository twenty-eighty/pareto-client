module Subscribers exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict
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


subscribersDTag : String
subscribersDTag =
    "pareto-subscribers"


newsletterDTag : String
newsletterDTag =
    "pareto-newsletter-"


load : Nostr.Model -> PubKey -> Shared.Msg.Msg
load nostr userPubKey =
    eventFilter userPubKey
        |> RequestSubscribers
        |> Nostr.createRequest nostr "Load subscribers" []
        |> Shared.Msg.RequestNostrEvents


merge : Bool -> List Subscriber -> List Subscriber -> List Subscriber
merge overwriteExisting existingSubscribers newSubscribers =
    let
        existingDict =
            existingSubscribers
                |> List.map (\subscriber -> ( subscriber.email, subscriber ))
                |> Dict.fromList
    in
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
            existingDict
        |> Dict.values


remove : List Subscriber -> List String -> List Subscriber
remove existingSubscribers toBeRemovedEmails =
    existingSubscribers
        |> List.filter
            (\subscriber ->
                not <| List.member subscriber.email toBeRemovedEmails
            )


eventFilter : PubKey -> EventFilter
eventFilter pubKey =
    { emptyEventFilter
        | authors = Just [ pubKey ]
        , kinds = Just [ KindApplicationSpecificData ]
        , tagReferences = Just [ TagReferenceIdentifier subscribersDTag ]
    }


processEvents : List Event -> ( List Subscriber, List String )
processEvents events =
    events
        |> List.map fromEvent
        |> List.foldl
            (\result ( subscriberList, errorList ) ->
                case result of
                    Ok decodedSubscribers ->
                        ( subscriberList ++ decodedSubscribers, errorList )

                    Err error ->
                        ( subscriberList, errorList ++ [ Decode.errorToString error ] )
            )
            ( [], [] )


fromEvent : Event -> Result Decode.Error (List Subscriber)
fromEvent event =
    Decode.decodeString (Decode.field "subscribers" subscribersDecoder) event.content


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
