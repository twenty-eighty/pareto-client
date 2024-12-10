module Nostr.Article exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Event, EventFilter, Kind(..), Tag(..), TagReference(..))
import Nostr.Nip19 as Nip19
import Nostr.Nip27 as Nip27
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set
import Time
import Ui.Profile exposing (defaultProfileImage)

type alias Article =
    { author : String
    , id : EventId
    , kind : Kind
    , alt : Maybe String
    , content : String
    , image : Maybe String
    , isValid : Maybe String
    , publishedAt : Maybe Time.Posix
    , summary : Maybe String
    , title : Maybe String
    , url : Maybe String
    , identifier : Maybe String
    , hashtags : List String
    , relay : Maybe RelayUrl
    , nip27References : List Nip19.NIP19Type
    }

-- assume published date is event creation date unless specified explicitely in publishedAt tag
emptyArticle : PubKey -> EventId -> Kind -> Time.Posix -> String -> Maybe RelayUrl -> Article
emptyArticle author eventId kind createdAt content relayUrl =
    { author = author
    , id = eventId
    , kind = kind
    , alt = Nothing
    , content = content
    , image = Nothing
    , isValid = Nothing
    , publishedAt = Just createdAt
    , summary = Nothing
    , title = Nothing
    , url = Nothing
    , identifier = Nothing
    , hashtags = []
    , relay = relayUrl
    , nip27References = 
        Nip27.collectNostrLinks content
    }


articleFromEvent : Event -> Result (List String) Article
articleFromEvent event =
    let
        articleWithoutTags =
            emptyArticle event.pubKey event.id event.kind event.createdAt event.content event.relay

        (builtArticle, buildingErrors) =
            event.tags
            |> List.foldl (\tag (article, errors) ->
                case tag of 
                    HashTag hashtag ->
                        ({ article | hashtags = article.hashtags ++ [hashtag] }, errors)

                    AltTag alt ->
                        ({ article | alt = Just alt }, errors)

                    EventDelegationTag identifier ->
                        ({ article | identifier = Just identifier }, errors)

                    ImageTag image _ ->
                        ({ article | image = Just image }, errors)

                    PublishedAtTag publishedAt ->
                        ({ article | publishedAt = Just publishedAt }, errors)

                    SummaryTag summary ->
                        ({ article | summary = Just summary }, errors)

                    TitleTag title ->
                        ({ article | title = Just title }, errors)

                    _ ->
                        (article, errors)
                    ) (articleWithoutTags, [])
    in
    case (builtArticle, buildingErrors) of
        (article, []) ->
            Ok article
        
        (_, errors) ->
            Err errors


tagReference : Article -> TagReference
tagReference article =
    case article.identifier of
        Just identifier ->
            TagReferenceCode KindLongFormContent article.author identifier

        Nothing ->
            TagReferenceEventId article.id


filterMatchesArticle : EventFilter -> Article -> Bool
filterMatchesArticle filter article =
    True
    |> filterMatchesAuthor filter article
    -- TODO: implement for other filter criteria


filterMatchesAuthor : EventFilter -> Article -> Bool -> Bool
filterMatchesAuthor filter article result =
    if result then
        filter.authors
        |> Maybe.map (List.member article.author)
        |> Maybe.withDefault True
    else
        False

removeLeadingHash : String -> String
removeLeadingHash tag =
    if String.startsWith "#" tag then
        String.dropLeft 1 tag
    else
        tag

uniqueArticleAuthors : List Article -> List String
uniqueArticleAuthors articles =
    articles
    |> List.map .author
    |> Set.fromList
    |> Set.toList


addArticles : List Article -> List Article -> List Article
addArticles articleList newArticles =
    List.foldl addArticle articleList newArticles

addArticle : Article -> List Article -> List Article
addArticle newArticle articleList =
    if List.any (isArticleWithIdAndAuthor newArticle.author newArticle.id) articleList then
        newArticle :: articleList
    else
        newArticle :: articleList

isArticleWithIdAndAuthor : PubKey -> EventId -> Article -> Bool
isArticleWithIdAndAuthor author articleId article =
    article.author == author && article.id == articleId

decodeUnixTime : Decoder Time.Posix
decodeUnixTime =
    Decode.int 
        |> Decode.map (\timeInt -> Time.millisToPosix (timeInt * 1000)
        )
