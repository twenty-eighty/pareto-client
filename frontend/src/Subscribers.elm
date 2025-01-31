module Subscribers exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Nostr.Event as Event exposing (Event, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Types exposing (PubKey)
import Pareto


type alias Subscriber =
    { email : String
    , name : Maybe String
    , tags : List String
    }


subscribersDTag : String
subscribersDTag =
    "pareto:subscribers"


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


eventFilter : PubKey -> EventFilter
eventFilter pubKey =
    { emptyEventFilter
        | authors = Just [ pubKey ]
        , kinds = Just [ KindApplicationSpecificData ]
        , tagReferences = Just [ TagReferenceIdentifier subscribersDTag ]
    }


fromEvent : Event -> Result Decode.Error (List Subscriber)
fromEvent event =
    Decode.decodeString (Decode.field "subscribers" subscribersDecoder) event.content


subscribersDecoder : Decode.Decoder (List Subscriber)
subscribersDecoder =
    Decode.list subscriberDecoder


subscriberDecoder : Decode.Decoder Subscriber
subscriberDecoder =
    Decode.succeed Subscriber
        |> required "email" Decode.string
        |> optional "name" (Decode.maybe Decode.string) Nothing
        |> optional "tags" (Decode.list Decode.string) []


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
        |> String.join ","
        |> (\tagString -> "\"" ++ tagString ++ "\"")
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
    , relay = Pareto.applicationDataRelays |> List.head
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
