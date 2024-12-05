module Nostr.Event exposing (..)

import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..), NAddrData)
import Nostr.Types exposing (EventId, PubKey)
import Time


type Tag
    = GenericTag String (Encode.Value)
    | AboutTag String
    | AddressTag String
    | AltTag String
    | ClientTag String
    | DescriptionTag String
    | DirTag
    | EventIdTag EventId
    | EventDelegationTag PubKey
    | ExpirationTag Time.Posix
    | ExternalIdTag String
    | FileTag String String
    | HashTag String
    | IdentityTag String String
    | ImageTag String (Maybe ImageSize)
    | LocationTag String String
    | MentionTag PubKey
    | NameTag String
    | PublicKeyTag PubKey (Maybe String) (Maybe String)
    | PublishedAtTag Time.Posix
    | QuotedEventTag EventId
    | RelaysTag (List String)
    | ServerTag String
    | SummaryTag String
    | TitleTag String
    | UrlTag String (Maybe String)
    | ZapTag String

type ImageSize
    = ImageSize Int Int


type alias Event =
    { pubKey : String
    , createdAt : Time.Posix
    , kind : Kind
    , tags : List Tag
    , content : String
    , id : String
    , sig : Maybe String
    , relay : Maybe String
    }


type alias EventFilter =
    { authors : Maybe (List String)
    , ids : Maybe (List String)
    , kinds : Maybe (List Kind)
    , tagReferences : Maybe (List TagReference)
    , limit : Maybe Int
    , since : Maybe Int
    , until : Maybe Int
    }

type TagReference
    = TagReferenceEventId EventId
    | TagReferenceCode Kind PubKey DCode
    | TagReferenceIdentifier String
    | TagReferenceTag String

type alias DCode = String

type Kind
    = KindUnknown Int
    | KindUserMetadata
    | KindShortTextNote
    | KindRecommendRelay
    | KindFollows
    | KindEncryptedDirectMessage
    | KindEventDeletionRequest
    | KindRepost
    | KindReaction
    | KindBadgeAward
    | KindGroupChatMessage
    | KindGroupChatThreadedReply
    | KindGroupThread
    | KindGroupThreadReply
    | KindSeal
    | KindDirectMessage
    | KindGenericRepost
    | KindReactionToWebsite
    | KindChannelCreation
    | KindChannelMetadata
    | KindChannelMessage
    | KindChannelHideMessage
    | KindChannelMuteUser
    | KindChess
    | KindMergeRequest
    | KindBid
    | KindBidConfirmation
    | KindOpenTimestamp
    | KindGiftWrap
    | KindFileMetadata
    | KindLiveChatMessage
    | KindPatches
    | KindIssues
    | KindReplies
    | KindStatus Int
    | KindProblemTracker
    | KindReporting
    | KindLabel
    | KindRelayReview
    | KindAIEmbedding
    | KindTorrent
    | KindTorrentComment
    | KindCoinjoinPool
    | KindCommunityPostApproval
    | KindJobRequest Int
    | KindJobResult Int
    | KindJobFeedback
    | KindGroupControlEvent Int
    | KindZapGoal
    | KindTidalLogin
    | KindZapRequest
    | KindZapReceipt
    | KindHighlights
    | KindMuteList
    | KindPinList
    | KindRelayListMetadata
    | KindBookmarkList
    | KindCommunitiesList
    | KindPublicChatsList
    | KindBlockedRelaysList
    | KindSearchRelaysList
    | KindUserGroups
    | KindInterestsLists
    | KindUserEmojiList
    | KindRelayListForDMs
    | KindUserServerList
    | KindFileStorageServerList
    | KindWalletInfo
    | KindLightningPubRPC
    | KindClientAuthentication
    | KindWalletRequest
    | KindWalletResponse
    | KindNostrConnect
    | KindBlobsStoredOnMediaservers
    | KindHTTPAuth
    | KindFollowSets
    | KindGenericLists
    | KindRelaySets
    | KindBookmarkSets
    | KindCurationSets
    | KindVideoSets
    | KindKindMuteSets
    | KindProfileBadges
    | KindBadgeDefinition
    | KindInterestSets
    | KindCreateUpdateStall
    | KindCreateUpdateProduct
    | KindMarketplaceUIUX
    | KindProductSoldViaAuction
    | KindLongFormContent
    | KindDraftLongFormContent
    | KindEmojiSet
    | KindModularArticleHeader
    | KindModularArticleContent
    | KindReleaseArtifactSet
    | KindApplicationSpecificData
    | KindLiveEvent
    | KindUserStatuses
    | KindSlideSet
    | KindClassifiedListing
    | KindDraftClassifiedListing
    | KindRepositoryAnnouncement
    | KindRepositoryStateAnnouncement
    | KindWikiArticle
    | KindRedirect
    | KindLinkSet
    | KindFeed
    | KindDateBasedCalendar
    | KindTimeBasedCalendar
    | KindCalendar
    | KindCalendarEvent
    | KindHandlerRecommendation
    | KindHandlerInformation
    | KindVideoEvent
    | KindShortFormPortraitVideoEvent
    | KindVideoViewEvent
    | KindCommunityDefinition
    | KindGroupMetadataEvent Int


kindFromNumber : Int -> Kind
kindFromNumber num =
    if num >= 5000 && num <= 5999 then
        KindJobRequest num
    else if num >= 6000 && num <= 6999 then
        KindJobResult num
    else if num >= 9000 && num <= 9030 then
        KindGroupControlEvent num
    else
        case num of
            0 -> KindUserMetadata
            1 -> KindShortTextNote
            2 -> KindRecommendRelay
            3 -> KindFollows
            4 -> KindEncryptedDirectMessage
            5 -> KindEventDeletionRequest
            6 -> KindRepost
            7 -> KindReaction
            8 -> KindBadgeAward
            9 -> KindGroupChatMessage
            10 -> KindGroupChatThreadedReply
            11 -> KindGroupThread
            12 -> KindGroupThreadReply
            13 -> KindSeal
            14 -> KindDirectMessage
            16 -> KindGenericRepost
            17 -> KindReactionToWebsite
            40 -> KindChannelCreation
            41 -> KindChannelMetadata
            42 -> KindChannelMessage
            43 -> KindChannelHideMessage
            44 -> KindChannelMuteUser
            64 -> KindChess
            818 -> KindMergeRequest
            1021 -> KindBid
            1022 -> KindBidConfirmation
            1040 -> KindOpenTimestamp
            1059 -> KindGiftWrap
            1063 -> KindFileMetadata
            1311 -> KindLiveChatMessage
            1617 -> KindPatches
            1621 -> KindIssues
            1622 -> KindReplies
            1630 -> KindStatus 1630
            1631 -> KindStatus 1631
            1632 -> KindStatus 1632
            1633 -> KindStatus 1633
            1971 -> KindProblemTracker
            1984 -> KindReporting
            1985 -> KindLabel
            1986 -> KindRelayReview
            1987 -> KindAIEmbedding
            2003 -> KindTorrent
            2004 -> KindTorrentComment
            2022 -> KindCoinjoinPool
            4550 -> KindCommunityPostApproval
            7000 -> KindJobFeedback
            9041 -> KindZapGoal
            9467 -> KindTidalLogin
            9734 -> KindZapRequest
            9735 -> KindZapReceipt
            9802 -> KindHighlights
            10000-> KindMuteList
            10001 -> KindPinList
            10002 -> KindRelayListMetadata
            10003 -> KindBookmarkList
            10004 -> KindCommunitiesList
            10005 -> KindPublicChatsList
            10006 -> KindBlockedRelaysList
            10007 -> KindSearchRelaysList
            10009 -> KindUserGroups
            10015 -> KindInterestsLists
            10030 -> KindUserEmojiList
            10050 -> KindRelayListForDMs
            10063 -> KindUserServerList
            10096 -> KindFileStorageServerList
            13194 -> KindWalletInfo
            21000 -> KindLightningPubRPC
            22242 -> KindClientAuthentication
            23194 -> KindWalletRequest
            23195 -> KindWalletResponse
            24133 -> KindNostrConnect
            24242 -> KindBlobsStoredOnMediaservers
            27235 -> KindHTTPAuth
            30000 -> KindFollowSets
            30001 -> KindGenericLists
            30002 -> KindRelaySets
            30003 -> KindBookmarkSets
            30004 -> KindCurationSets
            30005 -> KindVideoSets
            30007 -> KindKindMuteSets
            30008 -> KindProfileBadges
            30009 -> KindBadgeDefinition
            30015 -> KindInterestSets
            30017 -> KindCreateUpdateStall
            30018 -> KindCreateUpdateProduct
            30019 -> KindMarketplaceUIUX
            30020 -> KindProductSoldViaAuction
            30023 -> KindLongFormContent
            30024 -> KindDraftLongFormContent
            30030 -> KindEmojiSet
            30040 -> KindModularArticleHeader
            30041 -> KindModularArticleContent
            30063 -> KindReleaseArtifactSet
            30078 -> KindApplicationSpecificData
            30311 -> KindLiveEvent
            30315 -> KindUserStatuses
            30388 -> KindSlideSet
            30402 -> KindClassifiedListing
            30403 -> KindDraftClassifiedListing
            30617 -> KindRepositoryAnnouncement
            30618 -> KindRepositoryStateAnnouncement
            30818 -> KindWikiArticle
            30819 -> KindRedirect
            31388 -> KindLinkSet
            31890 -> KindFeed
            31922 -> KindDateBasedCalendar
            31923 -> KindTimeBasedCalendar
            31924 -> KindCalendar
            31925 -> KindCalendarEvent
            31989 -> KindHandlerRecommendation
            31990 -> KindHandlerInformation
            34235 -> KindVideoEvent
            34236 -> KindShortFormPortraitVideoEvent
            34237 -> KindVideoViewEvent
            34550 -> KindCommunityDefinition
            39000 -> KindGroupMetadataEvent 0
            39001 -> KindGroupMetadataEvent 1
            39002 -> KindGroupMetadataEvent 2
            39003 -> KindGroupMetadataEvent 3
            39004 -> KindGroupMetadataEvent 4
            39005 -> KindGroupMetadataEvent 5
            39006 -> KindGroupMetadataEvent 6
            39007 -> KindGroupMetadataEvent 7
            39008 -> KindGroupMetadataEvent 8
            39009 -> KindGroupMetadataEvent 9
            _ -> KindUnknown num
    
numberForKind : Kind -> Int
numberForKind kind =
    case kind of
        KindUnknown num                     -> num
        KindUserMetadata                    -> 0
        KindShortTextNote                   -> 1
        KindRecommendRelay                  -> 2
        KindFollows                         -> 3
        KindEncryptedDirectMessage          -> 4
        KindEventDeletionRequest            -> 5
        KindRepost                          -> 6
        KindReaction                        -> 7
        KindBadgeAward                      -> 8
        KindGroupChatMessage                -> 9
        KindGroupChatThreadedReply          -> 10
        KindGroupThread                     -> 11
        KindGroupThreadReply                -> 12
        KindSeal                            -> 13
        KindDirectMessage                   -> 14
        KindGenericRepost                   -> 16
        KindReactionToWebsite               -> 17
        KindChannelCreation                 -> 40
        KindChannelMetadata                 -> 41
        KindChannelMessage                  -> 42
        KindChannelHideMessage              -> 43
        KindChannelMuteUser                 -> 44
        KindChess                           -> 64
        KindMergeRequest                    -> 818
        KindBid                             -> 1021
        KindBidConfirmation                 -> 1022
        KindOpenTimestamp                   -> 1040
        KindGiftWrap                        -> 1059
        KindFileMetadata                    -> 1063
        KindLiveChatMessage                 -> 1311
        KindPatches                         -> 1617
        KindIssues                          -> 1621
        KindReplies                         -> 1622
        KindStatus num                      -> 1630
        KindProblemTracker                  -> 1971
        KindReporting                       -> 1984
        KindLabel                           -> 1985
        KindRelayReview                     -> 1986
        KindAIEmbedding                     -> 1987
        KindTorrent                         -> 2003
        KindTorrentComment                  -> 2004
        KindCoinjoinPool                    -> 2022
        KindCommunityPostApproval           -> 4550
        KindJobRequest num                  -> num
        KindJobResult num                   -> num
        KindJobFeedback                     -> 7000
        KindGroupControlEvent num           -> 9000
        KindZapGoal                         -> 9041
        KindTidalLogin                      -> 9467
        KindZapRequest                      -> 9734
        KindZapReceipt                      -> 9735
        KindHighlights                      -> 9802
        KindMuteList                        -> 10000
        KindPinList                         -> 10001
        KindRelayListMetadata               -> 10002
        KindBookmarkList                    -> 10003
        KindCommunitiesList                 -> 10004
        KindPublicChatsList                 -> 10005
        KindBlockedRelaysList               -> 10006
        KindSearchRelaysList                -> 10007
        KindUserGroups                      -> 10009
        KindInterestsLists                  -> 10015
        KindUserEmojiList                   -> 10030
        KindRelayListForDMs                 -> 10050
        KindUserServerList                  -> 10063
        KindFileStorageServerList           -> 10096
        KindWalletInfo                      -> 13194
        KindLightningPubRPC                 -> 21000
        KindClientAuthentication            -> 22242
        KindWalletRequest                   -> 23194
        KindWalletResponse                  -> 23195
        KindNostrConnect                    -> 24133
        KindBlobsStoredOnMediaservers       -> 24242
        KindHTTPAuth                        -> 27235
        KindFollowSets                      -> 30000
        KindGenericLists                    -> 30001
        KindRelaySets                       -> 30002
        KindBookmarkSets                    -> 30003
        KindCurationSets                    -> 30004
        KindVideoSets                       -> 30005
        KindKindMuteSets                    -> 30007
        KindProfileBadges                   -> 30008
        KindBadgeDefinition                 -> 30009
        KindInterestSets                    -> 30015
        KindCreateUpdateStall               -> 30017
        KindCreateUpdateProduct             -> 30018
        KindMarketplaceUIUX                 -> 30019
        KindProductSoldViaAuction           -> 30020
        KindLongFormContent                 -> 30023
        KindDraftLongFormContent            -> 30024
        KindEmojiSet                        -> 30030
        KindModularArticleHeader            -> 30040
        KindModularArticleContent           -> 30041
        KindReleaseArtifactSet              -> 30063
        KindApplicationSpecificData         -> 30078
        KindLiveEvent                       -> 30311
        KindUserStatuses                    -> 30315
        KindSlideSet                        -> 30388
        KindClassifiedListing               -> 30402
        KindDraftClassifiedListing          -> 30403
        KindRepositoryAnnouncement          -> 30617
        KindRepositoryStateAnnouncement     -> 30618
        KindWikiArticle                     -> 30818
        KindRedirect                        -> 30819
        KindLinkSet                         -> 31388
        KindFeed                            -> 31890
        KindDateBasedCalendar               -> 31922
        KindTimeBasedCalendar               -> 31923
        KindCalendar                        -> 31924
        KindCalendarEvent                   -> 31925
        KindHandlerRecommendation           -> 31989
        KindHandlerInformation              -> 31990
        KindVideoEvent                      -> 34235
        KindShortFormPortraitVideoEvent     -> 34236
        KindVideoViewEvent                  -> 34237
        KindCommunityDefinition             -> 34550
        KindGroupMetadataEvent num          -> num

kindDecoder : Decoder Kind
kindDecoder =
    Decode.int
        |> Decode.map kindFromNumber
    
tagReferenceToString : TagReference -> String
tagReferenceToString tagRef =
    case tagRef of
        TagReferenceEventId eventId ->
            eventId
        TagReferenceCode kind pubKey dCode ->
            String.fromInt (numberForKind kind) ++ ":" ++ pubKey ++ ":" ++ dCode

        TagReferenceIdentifier identifier ->
            identifier

        TagReferenceTag tag ->
            tag

imageSizeDecoder : Decoder ImageSize
imageSizeDecoder =
    Decode.string
        |> Decode.andThen
            (\sizeString ->
                case imageSizeFromString sizeString of
                    Just imageSize ->
                        Decode.succeed <| imageSize

                    _ ->
                        Decode.fail <| "Invalid numbers in image size: " ++ sizeString
            )

imageSizeFromString : String -> Maybe ImageSize
imageSizeFromString sizeString =
    case String.split "x" sizeString of
        [ widthString, heightString ] ->
            case ( String.toInt widthString, String.toInt heightString ) of
                ( Just width, Just height ) ->
                    Just <| ImageSize width height
                
                _ ->
                    Nothing

        _ ->
            Nothing

imageSizeToString : ImageSize -> String 
imageSizeToString (ImageSize width height) =
    String.fromInt width ++ "x" ++ String.fromInt height



emptyEventFilter : EventFilter
emptyEventFilter =
    { authors = Nothing
    , ids = Nothing
    , kinds = Nothing
    , tagReferences = Nothing
    , limit = Nothing
    , since = Nothing
    , until = Nothing
    }

-- EVENT DECODING

decodeEvent : Decode.Decoder Event
decodeEvent =
    Decode.succeed Event
        |> Pipeline.required "pubkey" Decode.string
        |> Pipeline.required "created_at" decodeUnixTime
        |> Pipeline.required "kind" kindDecoder
        |> Pipeline.required "tags" (Decode.list decodeTag)
        |> Pipeline.required "content" Decode.string
        |> Pipeline.required "id" Decode.string
        |> Pipeline.optional "sig" (Decode.maybe Decode.string) Nothing
        |> Pipeline.optional "relay" (Decode.maybe relayUrlDecoder) Nothing

relayUrlDecoder : Decode.Decoder String
relayUrlDecoder =
    Decode.field "url" Decode.string

eventFilterForNip19 : NIP19Type -> Maybe EventFilter
eventFilterForNip19 nip19 =
    case nip19 of
        Nip19.NAddr naddrData ->
            Just <| eventFilterForNaddr naddrData

        _ ->
            Nothing

eventFilterForNaddr : NAddrData -> EventFilter
eventFilterForNaddr { identifier, kind, pubKey, relays } =
    { authors = Just [ pubKey ]
    , ids = Nothing
    , kinds = Just [ kindFromNumber kind ]
    , tagReferences = Just [ TagReferenceIdentifier identifier ]
    , limit = Just 1
    , since = Nothing
    , until = Nothing
    }

decodeTag : Decode.Decoder Tag
decodeTag =
    Decode.index 0 Decode.string |> Decode.andThen (\typeStr ->
        case typeStr of
            "a" ->
                Decode.map AddressTag (Decode.index 1 Decode.string)

            "about" ->
                Decode.map AboutTag (Decode.index 1 Decode.string)

            "alt" ->
                Decode.map AltTag (Decode.index 1 Decode.string)

            "c" ->
                Decode.map ClientTag (Decode.index 1 Decode.string)

            "client" ->
                Decode.map ClientTag (Decode.index 1 Decode.string)

            "d" ->
                Decode.map EventDelegationTag (Decode.index 1 Decode.string)

            "dir" ->
                Decode.succeed DirTag

            "description" ->
                Decode.map DescriptionTag (Decode.index 1 Decode.string)

            "e" ->
                Decode.map EventIdTag (Decode.index 1 Decode.string)

            "expiration" ->
                Decode.map ExpirationTag (Decode.index 1 decodeUnixTimeString)

            "f" ->
                Decode.map2 FileTag (Decode.index 1 Decode.string) (Decode.index 2 Decode.string)

            "i" ->
                Decode.map2 IdentityTag (Decode.index 1 Decode.string) (Decode.index 2 Decode.string)

            "image" ->
                Decode.map2 ImageTag (Decode.index 1 Decode.string) (Decode.maybe (Decode.index 2 imageSizeDecoder))

            "l" ->
                Decode.map2 LocationTag (Decode.index 1 Decode.string) (Decode.index 2 Decode.string)

            "m" ->
                Decode.map MentionTag (Decode.index 1 Decode.string)

            "name" ->
                Decode.map NameTag (Decode.index 1 Decode.string)

            "p" ->
                Decode.map3 PublicKeyTag (Decode.index 1 Decode.string) (Decode.maybe (Decode.index 2 Decode.string)) (Decode.maybe (Decode.index 3 Decode.string))

            "published_at" ->
                Decode.map PublishedAtTag (Decode.index 1 decodeUnixTimeString)

            "q" ->
                Decode.map QuotedEventTag (Decode.index 1 Decode.string)

            "r" ->
                Decode.map2 UrlTag (Decode.index 1 Decode.string) (Decode.maybe (Decode.index 1 Decode.string))

            "relays" ->
                Decode.map RelaysTag (Decode.list Decode.string)

            "server" ->
                Decode.map ServerTag (Decode.index 1 Decode.string)

            "summary" ->
                Decode.map SummaryTag (Decode.index 1 Decode.string)

            "t" ->
                Decode.map HashTag (Decode.index 1 Decode.string)

            "title" ->
                Decode.map TitleTag (Decode.index 1 Decode.string)

            "x" ->
                Decode.map ExternalIdTag (Decode.index 1 Decode.string)

            "zap" ->
                Decode.map ZapTag (Decode.index 1 Decode.string)

            _ ->
                Decode.map (GenericTag typeStr) Decode.value
    )

tagToList : Tag -> List String
tagToList tag =
    case tag of
        GenericTag key value ->
            case Decode.decodeValue (Decode.list Decode.string) value of
                Ok stringArray ->
                    stringArray

                Err _ ->
                    [ key ]

        AboutTag value ->
            [ "about", value ]

        AddressTag value ->
            [ "a", value ]

        AltTag value ->
            [ "alt", value ]

        ClientTag value ->
            [ "client", value ]

        DescriptionTag value ->
            [ "description", value ]

        DirTag ->
            [ "dir" ]

        EventIdTag eventId ->
            [ "e", eventId ]

        EventDelegationTag pubKey ->
            [ "d", pubKey ]

        ExpirationTag time ->
            [ "expiration", Time.posixToMillis time |> String.fromInt ]

        ExternalIdTag value ->
            [ "x", value ]
 
        FileTag value1 value2 ->
            [ "f", value1, value2 ]

        HashTag value ->
            [ "t", value ]

        IdentityTag value1 value2 ->
            [ "i", value1, value2 ]

        ImageTag value (Just size) ->
            [ "image", value, imageSizeToString size ]

        ImageTag value Nothing ->
            [ "image", value ]

        LocationTag value1 value2 ->
            [ "l", value1, value2 ]

        MentionTag pubKey ->
            [ "m", pubKey ]

        NameTag value ->
            [ "name", value ]

        PublicKeyTag pubKey maybeRelay maybePetName ->
            [ "p", pubKey ]

        PublishedAtTag time ->
            [ "published_at", Time.posixToMillis time |> String.fromInt ]

        QuotedEventTag eventId ->
            [ "q", eventId ]

        RelaysTag relays ->
            "relays" :: relays

        ServerTag value ->
            [ "server", value ]

        SummaryTag value ->
            [ "summary", value ]

        TitleTag value ->
            [ "title", value ]

        UrlTag value1 maybeValue2 ->
            case maybeValue2 of
                Just value2 ->
                    [ "r", value1, value2 ]
                Nothing ->
                    [ "r", value1 ]

        ZapTag value ->
            [ "zap", value ]

parseAddress : String -> Maybe (Kind, PubKey, String)
parseAddress address =
    case String.split ":" address of
        [kindStr, pubKey, identifier] ->
            String.toInt kindStr
            |> Maybe.map (\kindInt -> (kindFromNumber kindInt, pubKey, identifier))

        _ ->
            Nothing

titleFromTags : List Tag -> Maybe String
titleFromTags tags =
    List.filterMap titleFromTag tags
    |> List.head

titleFromTag : Tag -> Maybe String
titleFromTag tag =
    case tag of
        TitleTag title ->
            Just title

        _ ->
            Nothing

summaryFromTags : List Tag -> Maybe String
summaryFromTags tags =
    List.filterMap summaryFromTag tags
    |> List.head

summaryFromTag : Tag -> Maybe String
summaryFromTag tag =
    case tag of
        SummaryTag summary ->
            Just summary

        _ ->
            Nothing


imageFromTags : List Tag -> Maybe String
imageFromTags tags =
    List.filterMap imageFromTag tags
    |> List.head

imageFromTag : Tag -> Maybe String
imageFromTag tag =
    case tag of
        ImageTag url _ ->
            Just url

        _ ->
            Nothing

decodeUnixTime : Decode.Decoder Time.Posix
decodeUnixTime =
    Decode.int
        |> Decode.map (\unixTime -> Time.millisToPosix (unixTime * 1000))

decodeUnixTimeString : Decode.Decoder Time.Posix
decodeUnixTimeString =
    Decode.string
        |> Decode.map (\unixTimeString ->
            case String.toInt unixTimeString of
                Just unixTime ->
                    Time.millisToPosix (unixTime * 1000)

                Nothing ->
                    Time.millisToPosix 0
        )

-- ENCODING


encodeEventFilter : EventFilter -> Encode.Value
encodeEventFilter filter =
    Encode.object
        ([]
        |> appendStringList "ids" filter.ids
        |> appendStringList "authors" filter.authors
        |> appendKindList filter.kinds
        |> appendTagReferenceList filter.tagReferences
        |> appendInt "since" filter.since
        |> appendInt "until" filter.until
        |> appendInt "limit" filter.limit
        )


appendStringList : String -> Maybe (List String) -> List (String, Encode.Value) -> List (String, Encode.Value)
appendStringList key maybeStringList encodeList =
    case maybeStringList of
        Just stringList ->
            (key, Encode.list Encode.string stringList) :: encodeList

        Nothing ->
            encodeList

appendTagReferenceList : Maybe (List TagReference) -> List (String, Encode.Value) -> List (String, Encode.Value)
appendTagReferenceList maybeTagRefList encodeList =
    case maybeTagRefList of
        Just tagRefList ->
            let
                maybeDcodeList =
                    tagRefList
                    |> (List.filterMap (\tagRef ->
                        case tagRef of
                            TagReferenceEventId _ ->
                                Nothing 
                            TagReferenceCode _ _ _ ->
                                Just <| tagReferenceToString tagRef
                            TagReferenceIdentifier _ ->
                                Nothing 
                            TagReferenceTag _ ->
                                Nothing
                            )
                       )
                    |> (\list ->
                        if List.isEmpty list then
                            Nothing
                        else
                            Just list
                        )
                
                maybeEventIdList =
                    tagRefList
                    |> (List.filterMap (\tagRef ->
                        case tagRef of
                            TagReferenceEventId eventId ->
                                Just eventId
                            TagReferenceCode _ _ _ ->
                                Nothing 
                            TagReferenceIdentifier _ ->
                                Nothing 
                            TagReferenceTag _ ->
                                Nothing
                            )
                       )
                    |> (\list ->
                        if List.isEmpty list then
                            Nothing
                        else
                            Just list
                        )
                
                maybeIdentifierList =
                    tagRefList
                    |> (List.filterMap (\tagRef ->
                        case tagRef of
                            TagReferenceEventId _ ->
                                Nothing 
                            TagReferenceCode _ _ _ ->
                                Nothing
                            TagReferenceIdentifier identifier ->
                                Just identifier
                            TagReferenceTag _ ->
                                Nothing
                            )
                       )
                    |> (\list ->
                        if List.isEmpty list then
                            Nothing
                        else
                            Just list
                        )
                
                maybeTagList =
                    tagRefList
                    |> (List.filterMap (\tagRef ->
                        case tagRef of
                            TagReferenceEventId _ ->
                                Nothing 
                            TagReferenceCode _ _ _ ->
                                Nothing
                            TagReferenceIdentifier _ ->
                                Nothing
                            TagReferenceTag tag ->
                                Just tag
                            )
                       )
                    |> (\list ->
                        if List.isEmpty list then
                            Nothing
                        else
                            Just list
                        )
                
            in
            encodeList
            |> appendStringList "#a" maybeDcodeList
            |> appendStringList "#d" maybeIdentifierList
            |> appendStringList "#e" maybeEventIdList
            |> appendStringList "#t" maybeTagList


        Nothing ->
            encodeList

appendKindList : Maybe (List Kind) -> List (String, Encode.Value) -> List (String, Encode.Value)
appendKindList maybeKindList encodeList =
    let
        maybeIntList =
            maybeKindList
            |> Maybe.map (List.map numberForKind)
        
    in
    appendIntList "kinds" maybeIntList encodeList   


appendIntList : String -> Maybe (List Int) -> List (String, Encode.Value) -> List (String, Encode.Value)
appendIntList key maybeIntList encodeList =
    case maybeIntList of
        Just intList ->
            (key, Encode.list Encode.int intList) :: encodeList

        Nothing ->
            encodeList

appendInt : String -> Maybe Int -> List (String, Encode.Value) -> List (String, Encode.Value)
appendInt key maybeInt encodeList =
    case maybeInt of
        Just num ->
            (key, Encode.int num) :: encodeList

        Nothing ->
            encodeList

encodeEvent : Event -> Encode.Value
encodeEvent event =
    [ ( "pubkey", Encode.string event.pubKey )
    , ( "kind", Encode.int <| numberForKind event.kind )
    , ( "content", Encode.string event.content )
    ]
    |> appendTags event.tags
    |> Encode.object
--        , ( "tags",  )

appendTags : List Tag -> List (String, Encode.Value) -> List (String, Encode.Value)
appendTags tags eventElements =
    let
        tagArrays =
            tags
            |> List.foldl (\tag acc ->
                tagToList tag :: acc
            ) []
    in
    eventElements ++ [ ("tags", Encode.list (Encode.list Encode.string) tagArrays ) ]

-- functions for building Event structure

addClientTag : String -> List Tag -> List Tag
addClientTag client tags =
    ClientTag client :: tags

addTitleTag : Maybe String -> List Tag -> List Tag
addTitleTag maybeTitle tags =
    maybeTitle
    |> Maybe.map (\title -> TitleTag title :: tags)
    |> Maybe.withDefault tags

addServerTag : String -> List Tag -> List Tag
addServerTag serverUrl tags =
    ServerTag serverUrl :: tags

addSummaryTag : Maybe String -> List Tag -> List Tag
addSummaryTag maybeSummary tags =
    maybeSummary
    |> Maybe.map (\summary -> SummaryTag summary :: tags)
    |> Maybe.withDefault tags

addImageTag : Maybe String -> List Tag -> List Tag
addImageTag maybeUrl tags =
    maybeUrl
    |> Maybe.map (\url -> ImageTag url Nothing :: tags)
    |> Maybe.withDefault tags


addIdentifierTag : Maybe String -> List Tag -> List Tag
addIdentifierTag maybeIdentifier tags =
    maybeIdentifier
    |> Maybe.map (\identifier -> EventDelegationTag identifier :: tags)
    |> Maybe.withDefault tags

addPublishedTag : Maybe Time.Posix -> List Tag -> List Tag
addPublishedTag maybeTime tags =
    maybeTime
    |> Maybe.map (\published -> PublishedAtTag published :: tags)
    |> Maybe.withDefault tags


addTagTags : Maybe String -> List Tag -> List Tag
addTagTags maybeTags tags =
    maybeTags
    |> Maybe.map (\tagsString ->
        tagsString
        |> String.split ","
        |> List.map String.trim
        |> List.map HashTag
        |> List.append tags
        )
    |> Maybe.withDefault tags
