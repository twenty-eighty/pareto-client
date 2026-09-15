module Nostr.ZapsQuery exposing
    ( forTagReference
    , forEventId
    , forArticle
    , hasBolt11
    , idsForTagReference
    , totalAmount
    , addAmount
    )

{-| Zap receipt lookups against store dicts.
-}

import Dict exposing (Dict)
import Nostr.Article exposing (Article, tagReference)
import Nostr.Event exposing (TagReference(..), tagReferenceToString)
import Nostr.Types exposing (EventId)
import Nostr.Zaps exposing (ZapReceipt)
import Set exposing (Set)


type alias Store a =
    { a
        | zapReceiptsAddress : Dict String (Dict String ZapReceipt)
        , zapReceiptsEvents : Dict String (Dict String ZapReceipt)
    }


forArticle : Store a -> Article -> Maybe (Dict String ZapReceipt)
forArticle store article =
    forTagReference store (tagReference article)


forTagReference : Store a -> TagReference -> Maybe (Dict String ZapReceipt)
forTagReference store tagRef =
    case tagRef of
        TagReferenceEventId eventId ->
            Dict.get eventId store.zapReceiptsEvents

        TagReferenceCode _ ->
            Dict.get (tagReferenceToString tagRef) store.zapReceiptsAddress

        TagReferenceIdentifier _ ->
            Nothing

        TagReferencePubKey _ ->
            Nothing

        TagReferenceTag _ ->
            Nothing

        TagReferenceU _ ->
            Nothing


forEventId : Store a -> EventId -> Maybe (Dict String ZapReceipt)
forEventId store eventId =
    Dict.get eventId store.zapReceiptsEvents


hasBolt11 : Store a -> String -> Bool
hasBolt11 store bolt11 =
    let
        matches nested =
            nested
                |> Dict.values
                |> List.concatMap Dict.values
                |> List.any (\receipt -> receipt.bolt11 == bolt11)
    in
    matches store.zapReceiptsAddress || matches store.zapReceiptsEvents


idsForTagReference : Store a -> TagReference -> Set String
idsForTagReference store tagRef =
    forTagReference store tagRef
        |> Maybe.map Dict.keys
        |> Maybe.withDefault []
        |> Set.fromList


totalAmount : Dict String ZapReceipt -> Int
totalAmount receiptsDict =
    Dict.values receiptsDict
        |> List.foldl addAmount 0


addAmount : ZapReceipt -> Int -> Int
addAmount receipt acc =
    case receipt.amount of
        Just amount ->
            acc + amount

        Nothing ->
            acc
