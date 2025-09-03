module Newsletters.Types exposing (..)

import Json.Encode as Encode
import Time
import Nostr.Types exposing (PubKey)

type alias Subscriber =
    { dnd : Maybe Bool
    , email : String
    , firstName : Maybe String
    , lastName : Maybe String
    , pubKey : Maybe PubKey
    , source : Maybe String
    , dateSubscription : Time.Posix
    , dateUnsubscription : Maybe Time.Posix
    , tags : Maybe (List String)
    , undeliverable : Maybe String
    , locale : Maybe String
    }


type SubscriberField
    = FieldDnd
    | FieldEmail
    | FieldName
    | FieldFirstName
    | FieldLastName
    | FieldPubKey
    | FieldSource
    | FieldDateSubscription
    | FieldDateUnsubscription
    | FieldTags
    | FieldUndeliverable
    | FieldLocale


fieldName : SubscriberField -> String
fieldName field =
    case field of
        FieldDnd ->
            "dnd"

        FieldEmail ->
            -- this field name needs to be the same in email gateway and subscription server
            "email"

        FieldName ->
            "name"

        FieldFirstName ->
            "firstName"

        FieldLastName ->
            "lastName"

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

        FieldUndeliverable ->
            "undeliverable"

        FieldLocale ->
            "locale"


encodeSubscribers : List Subscriber -> Encode.Value
encodeSubscribers subscribers =
    subscribers
        |> List.map encodeSubscriber
        |> Encode.list Encode.object


encodeSubscriber : Subscriber -> List ( String, Encode.Value )
encodeSubscriber subscriber =
    [ ( fieldName FieldEmail, Encode.string subscriber.email )
    , ( fieldName FieldDateSubscription, Encode.int <| Time.posixToMillis subscriber.dateSubscription )
    ]
        |> addBoolToObject FieldDnd subscriber.dnd
        |> addStringToObject FieldUndeliverable subscriber.undeliverable
        |> addStringToObject FieldFirstName subscriber.firstName
        |> addStringToObject FieldLastName subscriber.lastName
        |> addStringToObject FieldPubKey subscriber.pubKey
        |> addStringToObject FieldSource subscriber.source
        |> addStringToObject FieldLocale subscriber.locale
        |> addDateToObject FieldDateUnsubscription subscriber.dateUnsubscription
        |> addStringListToObject FieldTags subscriber.tags


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

