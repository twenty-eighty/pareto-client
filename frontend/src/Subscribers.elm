module Subscribers exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Csv.Encode
import Dict exposing (Dict)
import I18Next
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Mailcheck
import Nostr
import Nostr.Event as Event exposing (AddressComponents, Event, EventFilter, Kind(..), TagReference(..), emptyEvent, emptyEventFilter)
import Nostr.Profile exposing (Profile, profileDisplayName)
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (PubKey)
import Pareto
import SHA256
import Shared
import Shared.Msg
import Time
import Translations.Subscribers as Translations


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


allSubscriberFields : List SubscriberField
allSubscriberFields =
    [ FieldDnd
    , FieldEmail
    , FieldName
    , FieldFirstName
    , FieldLastName
    , FieldPubKey
    , FieldSource
    , FieldDateSubscription
    , FieldDateUnsubscription
    , FieldTags
    , FieldUndeliverable
    , FieldLocale
    ]


type alias CsvColumnNameMap =
    Dict String SubscriberField


type alias CsvColumnIndexMap =
    List ( Int, SubscriberField )


type alias CsvData =
    List (List String)


defaultCsvColumnNameMap : CsvColumnNameMap
defaultCsvColumnNameMap =
    -- these terms need to be lowercase for case insensitive comparison
    [ ( "email", FieldEmail )
    , ( "emailaddress", FieldEmail )
    , ( "e-mail", FieldEmail )
    , ( "name", FieldName )
    , ( "full name", FieldName )
    , ( "complete name", FieldName )
    , ( "voller name", FieldName )
    , ( "forename", FieldFirstName )
    , ( "given name", FieldFirstName )
    , ( "firstname", FieldFirstName )
    , ( "first name", FieldFirstName )
    , ( "first_name", FieldFirstName )
    , ( "vorname", FieldFirstName )
    , ( "surname", FieldLastName )
    , ( "family name", FieldLastName )
    , ( "lastname", FieldLastName )
    , ( "last name", FieldLastName )
    , ( "last_name", FieldLastName )
    , ( "familienname", FieldLastName )
    , ( "nachname", FieldLastName )
    , ( "zuname", FieldLastName )
    , ( "subscription date", FieldDateSubscription )
    ]
        |> Dict.fromList


buildColumnNameMap : List String -> CsvColumnNameMap
buildColumnNameMap csvColumnNames =
    csvColumnNames
        |> List.foldl
            (\columnName acc ->
                case Dict.get (String.toLower columnName) defaultCsvColumnNameMap of
                    Just subscriberField ->
                        Dict.insert columnName subscriberField acc

                    Nothing ->
                        acc
            )
            Dict.empty


buildCsvColumnIndexMap : CsvColumnNameMap -> List String -> CsvColumnIndexMap
buildCsvColumnIndexMap nameMap columnNames =
    columnNames
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( index, columnName ) ->
                Dict.get columnName nameMap
                    |> Maybe.map
                        (\subscriberField ->
                            ( index, subscriberField )
                        )
            )


buildSubscriberFromCsvRecord : Time.Posix -> CsvColumnIndexMap -> List String -> Maybe Subscriber
buildSubscriberFromCsvRecord now columnIndexMap csvLine =
    let
        presetEmail =
            -- only needed to initialize subscriber and check if email was set with meaningful value
            "this-is-not-an-email"
    in
    csvLine
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( index, value ) ( remainingFields, subscriber ) ->
                case List.head remainingFields of
                    Just ( mappedIndex, subscriberField ) ->
                        if index == mappedIndex then
                            -- add field to subscriber and advance in mapping list
                            ( List.drop 1 remainingFields, setSubscriberField subscriberField value subscriber )

                        else
                            -- column not mapped, simply continue
                            ( remainingFields, subscriber )

                    Nothing ->
                        -- no more mapping entries
                        ( [], subscriber )
            )
            ( columnIndexMap, emptySubscriber presetEmail )
        |> Tuple.second
        |> (\subscriber ->
                if subscriber.email == presetEmail then
                    Nothing

                else
                    Just { subscriber | source = Just "CSV", dateSubscription = now }
           )


setSubscriberField : SubscriberField -> String -> Subscriber -> Subscriber
setSubscriberField field value subscriber =
    case field of
        FieldDnd ->
            { subscriber | dnd = stringToBool value }

        FieldEmail ->
            { subscriber | email = value }

        FieldName ->
            -- This logic is not ideal as there may be mapped (full) name and first or last name
            -- and the order of the fields matters in that case.
            -- Better would be to collect all fields and match later.
            -- However, this should be a rather exotic use case and can be avoided
            -- by mapping either full name or first and last name.
            let
                ( firstName, lastName ) =
                    splitName value

                subscriberWithFirstName =
                    if firstName /= Nothing && subscriber.firstName == Nothing then
                        { subscriber | firstName = firstName }

                    else
                        subscriber

                subscriberWithFirstAndLastName =
                    if lastName /= Nothing && subscriber.lastName == Nothing then
                        { subscriberWithFirstName | lastName = lastName }

                    else
                        subscriberWithFirstName
            in
            subscriberWithFirstAndLastName

        FieldFirstName ->
            { subscriber | firstName = Just value }

        FieldLastName ->
            { subscriber | lastName = Just value }

        FieldPubKey ->
            { subscriber | pubKey = Just value }

        FieldSource ->
            { subscriber | source = Just value }

        FieldDateSubscription ->
            { subscriber | dateSubscription = Iso8601.toTime value |> Result.toMaybe |> Maybe.withDefault (Time.millisToPosix 0) }

        FieldDateUnsubscription ->
            { subscriber | dateUnsubscription = Iso8601.toTime value |> Result.toMaybe }

        FieldTags ->
            { subscriber | tags = String.split "," value |> Just }

        FieldUndeliverable ->
            { subscriber | undeliverable = Just value }

        FieldLocale ->
            { subscriber | locale = Just value }



-- best guess how to split full name to first and last name


splitName : String -> ( Maybe String, Maybe String )
splitName fullName =
    case String.split " " fullName of
        [] ->
            ( Nothing, Nothing )

        [ firstName ] ->
            ( Just firstName, Nothing )

        [ firstName, lastName ] ->
            ( Just firstName, Just lastName )

        [ firstName, middleName, lastName ] ->
            ( Just <| firstName ++ " " ++ middleName, Just lastName )

        firstName :: lastNames ->
            ( Just firstName, Just <| String.join " " lastNames )


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


translatedFieldName : I18Next.Translations -> SubscriberField -> String
translatedFieldName translations field =
    case field of
        FieldDnd ->
            Translations.dndFieldName [ translations ]

        FieldEmail ->
            Translations.emailFieldName [ translations ]

        FieldName ->
            Translations.nameFieldName [ translations ]

        FieldFirstName ->
            Translations.firstNameFieldName [ translations ]

        FieldLastName ->
            Translations.lastNameFieldName [ translations ]

        FieldPubKey ->
            Translations.pubkeyFieldName [ translations ]

        FieldSource ->
            Translations.sourceFieldName [ translations ]

        FieldDateSubscription ->
            Translations.datesubFieldName [ translations ]

        FieldDateUnsubscription ->
            Translations.dateunsubFieldName [ translations ]

        FieldTags ->
            Translations.tagsFieldName [ translations ]

        FieldUndeliverable ->
            Translations.undeliverableFieldName [ translations ]

        FieldLocale ->
            Translations.localeFieldName [ translations ]


subscriberValue : BrowserEnv -> Subscriber -> SubscriberField -> String
subscriberValue browserEnv subscriber field =
    case field of
        FieldDnd ->
            subscriber.dnd
                |> Maybe.map boolToString
                |> Maybe.withDefault (boolToString False)

        FieldEmail ->
            subscriber.email

        FieldName ->
            Maybe.withDefault "" subscriber.firstName ++ " " ++ Maybe.withDefault "" subscriber.lastName

        FieldFirstName ->
            Maybe.withDefault "" subscriber.firstName

        FieldLastName ->
            Maybe.withDefault "" subscriber.lastName

        FieldPubKey ->
            Maybe.withDefault "" subscriber.pubKey

        FieldSource ->
            Maybe.withDefault "" subscriber.source

        FieldDateSubscription ->
            subscriber.dateSubscription
                |> BrowserEnv.formatDate browserEnv

        FieldDateUnsubscription ->
            subscriber.dateUnsubscription
                |> Maybe.map (BrowserEnv.formatDate browserEnv)
                |> Maybe.withDefault ""

        FieldTags ->
            subscriber.tags
                |> Maybe.map (String.join ",")
                |> Maybe.withDefault ""

        FieldUndeliverable ->
            Maybe.withDefault "" subscriber.undeliverable

        FieldLocale ->
            Maybe.withDefault "" subscriber.locale


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
    , firstName = Nothing
    , lastName = Nothing
    , pubKey = Nothing
    , source = Nothing
    , dateSubscription = Time.millisToPosix 0
    , dateUnsubscription = Nothing
    , tags = Nothing
    , undeliverable = Nothing
    , locale = Nothing
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
        |> Nostr.createRequest nostr "Load modifications" []
        |> Shared.Msg.RequestNostrEvents


toCsv : List Subscriber -> Csv.Encode.Csv
toCsv subscribers =
    { headers =
        [ fieldName FieldDnd
        , fieldName FieldEmail
        , fieldName FieldFirstName
        , fieldName FieldLastName
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
                    , subscriber.firstName |> Maybe.withDefault ""
                    , subscriber.lastName |> Maybe.withDefault ""
                    , subscriber.pubKey |> Maybe.withDefault ""
                    , subscriber.source |> Maybe.withDefault ""
                    , subscriber.dateSubscription |> Iso8601.fromTime
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


stringToBool : String -> Maybe Bool
stringToBool value =
    case String.toLower value of
        "true" ->
            Just True

        "yes" ->
            Just True

        "1" ->
            Just True

        "false" ->
            Just False

        "no" ->
            Just False

        "0" ->
            Just False

        _ ->
            Nothing


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
                    Subscription newsubscriber ->
                        Dict.insert newsubscriber.email { newsubscriber | source = Just "opt-in" } acc

                    Unsubscription unsubscribed ->
                        Dict.update
                            unsubscribed.email
                            (Maybe.map
                                (\subscriber ->
                                    { subscriber | dnd = unsubscribed.dnd, dateUnsubscription = unsubscribed.dateUnsubscription }
                                )
                            )
                            acc
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


processEvents : PubKey -> List Modification -> List Event -> ( Maybe SubscriberEventData, List Modification, List String )
processEvents userPubKey existingModifications events =
    let
        subscriberEventData =
            events
                |> List.filter (\event -> event.pubKey == userPubKey)
                |> List.filterMap
                    (\subscriberEvent ->
                        Decode.decodeString decodeSubscriberData subscriberEvent.content
                            |> Result.toMaybe
                    )
                |> List.head

        modificationEvents =
            events
                |> List.filter (\event -> event.pubKey == Pareto.subscriptionServerKey)

        ( modifications, decodingErrors ) =
            modificationEvents
                |> List.map modificationFromEvent
                |> List.foldl
                    (\result ( modificationsAcc, errorList ) ->
                        case result of
                            Ok decodedModification ->
                                ( modificationsAcc ++ [ decodedModification ], errorList )

                            Err error ->
                                ( modificationsAcc, errorList ++ [ Decode.errorToString error ] )
                    )
                    ( existingModifications, [] )
    in
    ( subscriberEventData, modifications, decodingErrors )


subscribersFromEvent : Event -> Result Decode.Error (List Subscriber)
subscribersFromEvent event =
    Decode.decodeString subscriberDataDecoder event.content


subscriberDataDecoder : Decode.Decoder (List Subscriber)
subscriberDataDecoder =
    Decode.field "subscribers" subscribersDecoder


modificationFromEvent : Event -> Result Decode.Error Modification
modificationFromEvent event =
    Decode.decodeString modificationsDecoder event.content
        |> Result.map
            (\modification ->
                case modification of
                    Subscription subscriber ->
                        Subscription { subscriber | dnd = Just False, dateSubscription = event.createdAt }

                    Unsubscription subscriber ->
                        Unsubscription { subscriber | dnd = Just True, dateUnsubscription = Just event.createdAt }
            )


modificationsDecoder : Decode.Decoder Modification
modificationsDecoder =
    Decode.oneOf
        [ Decode.field "subscribe" subscribeDecoder
        , Decode.field "unsubscribe" unsubscribeDecoder
        ]


subscribesDecoder : Decode.Decoder (List Modification)
subscribesDecoder =
    Decode.list subscribeDecoder


subscribeDecoder : Decode.Decoder Modification
subscribeDecoder =
    subscribeInfoDecoder
        |> Decode.andThen
            (\subscribeInfo ->
                let
                    subscriberWithEmail =
                        emptySubscriber subscribeInfo.email

                    subscriber =
                        { subscriberWithEmail
                            | firstName = subscribeInfo.firstName
                            , lastName = subscribeInfo.lastName
                            , locale = subscribeInfo.locale
                            , pubKey = subscribeInfo.pubKey
                        }
                in
                Subscription subscriber
                    |> Decode.succeed
            )



-- these are the fields that were passed via email gateway and subscribe link to author


type alias SubscribeInfo =
    { email : String
    , firstName : Maybe String
    , lastName : Maybe String
    , locale : Maybe String
    , pubKey : Maybe PubKey
    }


subscribeInfoDecoder : Decode.Decoder SubscribeInfo
subscribeInfoDecoder =
    Decode.succeed SubscribeInfo
        |> required (fieldName FieldEmail) Decode.string
        |> optional (fieldName FieldFirstName) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldLastName) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldLocale) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldPubKey) (Decode.maybe Decode.string) Nothing


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
        |> optional (fieldName FieldFirstName) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldLastName) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldPubKey) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldSource) (Decode.maybe Decode.string) Nothing
        |> required (fieldName FieldDateSubscription) decodePosixTime
        |> optional (fieldName FieldDateUnsubscription) (Decode.maybe decodePosixTime) Nothing
        |> optional (fieldName FieldTags) (Decode.maybe (Decode.list Decode.string)) Nothing
        |> optional (fieldName FieldUndeliverable) (Decode.maybe Decode.string) Nothing
        |> optional (fieldName FieldLocale) (Decode.maybe Decode.string) Nothing


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
    , fieldName FieldFirstName
    , fieldName FieldLastName
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
    , subscriber.firstName |> Maybe.withDefault ""
    , subscriber.lastName |> Maybe.withDefault ""
    , subscriber.pubKey |> Maybe.withDefault ""
    , subscriber.source |> Maybe.withDefault ""
    , subscriber.dateSubscription |> timeToString
    , subscriber.dateUnsubscription |> Maybe.map timeToString |> Maybe.withDefault ""
    , subscriber.tags
        |> Maybe.map (String.join ",")
        |> Maybe.map (\tagString -> "\"" ++ tagString ++ "\"")
        |> Maybe.withDefault ""
    ]
        |> String.join ","
        |> String.append "\n"


type alias SubscriberEventData =
    { keyHex : String -- encryption key
    , ivHex : String -- initialization vector of encryption
    , url : String -- URL where to find the encrypted data
    , size : Int -- encrypted data size
    , active : Int -- number of active subscribers
    , total : Int -- total number of subscribers
    }


subscriberEventKey : String
subscriberEventKey =
    "key"


subscriberEventIv : String
subscriberEventIv =
    "iv"


subscriberEventUrl : String
subscriberEventUrl =
    "url"


subscriberEventSize : String
subscriberEventSize =
    "size"


subscriberEventActive : String
subscriberEventActive =
    "active"


subscriberEventTotal : String
subscriberEventTotal =
    "total"


subscriberEventArticle : String
subscriberEventArticle =
    "article"


subscriberEventTitle : String
subscriberEventTitle =
    "title"


subscriberEventSummary : String
subscriberEventSummary =
    "summary"


subscriberEventContent : String
subscriberEventContent =
    "content"


subscriberEventImageUrl : String
subscriberEventImageUrl =
    "image"


subscriberEventLanguage : String
subscriberEventLanguage =
    "language"


decodeSubscriberData : Decode.Decoder SubscriberEventData
decodeSubscriberData =
    Decode.succeed SubscriberEventData
        |> required subscriberEventKey Decode.string
        |> required subscriberEventIv Decode.string
        |> required subscriberEventUrl Decode.string
        |> required subscriberEventSize Decode.int
        |> required subscriberEventActive Decode.int
        |> required subscriberEventTotal Decode.int


subscriberDataEvent : BrowserEnv -> PubKey -> SubscriberEventData -> Event
subscriberDataEvent browserEnv pubKey { keyHex, ivHex, url, size, active, total } =
    { pubKey = pubKey
    , createdAt = browserEnv.now
    , kind = KindApplicationSpecificData
    , tags =
        []
            |> Event.addDTag subscribersDTag
    , content =
        [ ( subscriberEventKey, Encode.string keyHex )
        , ( subscriberEventIv, Encode.string ivHex )
        , ( subscriberEventUrl, Encode.string url )
        , ( subscriberEventSize, Encode.int size )
        , ( subscriberEventActive, Encode.int active )
        , ( subscriberEventTotal, Encode.int total )
        ]
            |> Encode.object
            |> Encode.encode 0
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


type alias ArticleData =
    { title : String
    , summary : String
    , content : String
    , imageUrl : String
    , language : Maybe String
    }


decodeArticleData : Decode.Decoder ArticleData
decodeArticleData =
    Decode.succeed ArticleData
        |> required subscriberEventTitle Decode.string
        |> required subscriberEventSummary Decode.string
        |> required subscriberEventContent Decode.string
        |> required subscriberEventImageUrl Decode.string
        |> optional subscriberEventLanguage (Decode.maybe Decode.string) Nothing


newsletterSubscribersEvent : Shared.Model -> PubKey -> AddressComponents -> ArticleData -> SubscriberEventData -> Event
newsletterSubscribersEvent shared pubKey articleAddressComponents articleData subscriberEventData =
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
            |> Event.addAddressTag articleAddressComponents Nothing
            -- reference email gateway as encryption target
            |> Event.addPubKeyTag Pareto.emailGatewayKey Nothing Nothing
    , content = emailSendRequestToJson shared.nostr.testMode senderProfileName articleData subscriberEventData
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }


emailSendRequestToJson : Nostr.TestMode -> Maybe String -> ArticleData -> SubscriberEventData -> String
emailSendRequestToJson testMode maybeSenderName { title, summary, content, imageUrl, language } { keyHex, ivHex, url, size, active, total } =
    let
        testModeValue =
            if testMode == Nostr.TestModeEnabled then
                Just True

            else
                Nothing
    in
    [ ( "newsletter"
      , [ ( subscriberEventKey, Encode.string keyHex )
        , ( subscriberEventIv, Encode.string ivHex )
        , ( subscriberEventUrl, Encode.string url )
        , ( subscriberEventSize, Encode.int size )
        , ( subscriberEventActive, Encode.int active )
        , ( subscriberEventTotal, Encode.int total )
        , ( subscriberEventArticle
          , [ ( subscriberEventTitle, Encode.string title )
            , ( subscriberEventSummary, Encode.string summary )
            , ( subscriberEventContent, Encode.string content )
            , ( subscriberEventImageUrl, Encode.string imageUrl )
            ]
                |> appendOptionalObjectString subscriberEventLanguage language
                |> Encode.object
          )
        ]
            |> appendOptionalObjectString "authorName" maybeSenderName
            |> appendOptionalObjectBool "test" testModeValue
            |> Encode.object
      )
    ]
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


subscribeEvent : Nostr.Model -> Profile -> Maybe PubKey -> SubscribeInfo -> Event
subscribeEvent nostr authorProfile maybeSigningPubKey { pubKey, email, firstName, lastName, locale } =
    let
        signingPubKey =
            maybeSigningPubKey
                -- in order to create a valid Nostr event we need some secret key to sign
                -- but it wouldn't make sense to transport/store the related public key
                |> Maybe.withDefault Pareto.anonymousPublicKey

        event =
            emptyEvent signingPubKey KindApplicationSpecificData

        -- build a hash from author pubkey and subscriber email to avoid duplicate events for the same thing
        authorEmailHash =
            (email ++ "|" ++ authorProfile.pubKey)
                |> SHA256.fromString
                |> SHA256.toHex

        testModeValue =
            if nostr.testMode == Nostr.TestModeEnabled then
                Just True

            else
                Nothing

        dTag =
            "subscribe-" ++ authorEmailHash
    in
    { event
        | tags =
            []
                |> Event.addDTag dTag
                |> Event.addPubKeyTag Pareto.emailGatewayKey Nothing Nothing
        , content =
            [ ( "subscribe"
                -- the author field name needs to be the same in email gateway and subscription server
              , [ ( "author", Encode.string authorProfile.pubKey )
                , ( fieldName FieldEmail, Encode.string email )
                ]
                    |> appendOptionalObjectString (fieldName FieldFirstName) firstName
                    |> appendOptionalObjectString (fieldName FieldLastName) lastName
                    |> appendOptionalObjectString "authorName" authorProfile.displayName
                    |> appendOptionalObjectString (fieldName FieldPubKey) pubKey
                    |> appendOptionalObjectString (fieldName FieldLocale) locale
                    |> appendOptionalObjectBool "test" testModeValue
                    |> Encode.object
              )
            ]
                |> Encode.object
                |> Encode.encode 0
    }


appendOptionalObjectString : String -> Maybe String -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
appendOptionalObjectString key maybeValue entries =
    case maybeValue of
        Just value ->
            entries ++ [ ( key, Encode.string value ) ]

        Nothing ->
            entries


appendOptionalObjectBool : String -> Maybe Bool -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
appendOptionalObjectBool key maybeValue entries =
    case maybeValue of
        Just value ->
            entries ++ [ ( key, Encode.bool value ) ]

        Nothing ->
            entries


emailValid : String -> Bool
emailValid email =
    Mailcheck.mailParts email
        |> Maybe.map
            (\mailParts ->
                (mailParts.address /= "")
                    && (String.length mailParts.topLevelDomain > 1)
                    && (mailParts.secondLevelDomain /= "")
                    && (numberOfAtChars email == 1)
                    && (not <| String.contains " " email)
            )
        |> Maybe.withDefault False


numberOfAtChars : String -> Int
numberOfAtChars email =
    email
        |> String.indexes "@"
        |> List.length
