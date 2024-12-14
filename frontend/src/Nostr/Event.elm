module Nostr.Event exposing (..)

import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..), NAddrData)
import Nostr.Relay
import Nostr.Types exposing (Address, EventId, PubKey, RelayRole(..), RelayUrl, decodeRelayRole, relayRoleToString)
import Time
import Json.Decode as Decode
import Json.Decode as Decode
import Json.Decode as Decode
import Json.Decode as Decode


type Tag
    = GenericTag String
    | GenericTag2 String String 
    | GenericTag3 String String String 
    | GenericTag4 String String String String
    | AboutTag String
    | AddressTag Address
    | AltTag String
    | ClientTag String (Maybe String) (Maybe String)
    | DescriptionTag String
    | DirTag
    | EventIdTag EventId
    | EventDelegationTag PubKey
    | ExpirationTag Time.Posix
    | ExternalIdTag String
    | FileTag String String
    | HashTag String
    | IdentityTag String String
    | KindTag Kind
    | ImageTag String (Maybe ImageSize)
    | LocationTag String (Maybe String)
    | MentionTag PubKey
    | NameTag String
    | PublicKeyTag PubKey (Maybe String) (Maybe String)
    | PublishedAtTag Time.Posix
    | QuotedEventTag EventId
    | ReferenceTag String (Maybe String)
    | RelayTag String
    | RelaysTag (List String)
    | ServerTag String
    | SummaryTag String
    | TitleTag String
    | UrlTag String RelayRole
    | WebTag String (Maybe String)
    | ZapTag PubKey RelayUrl (Maybe Int)

type ImageSize
    = ImageSize Int Int


type alias Event =
    { pubKey : PubKey
    , createdAt : Time.Posix
    , kind : Kind
    , tags : List Tag
    , content : String
    , id : String
    , sig : Maybe String
    , relay : Maybe RelayUrl
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
    | KindPicture
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
    | KindComment
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
    | KindReservedCashuWalletTokens
    | KindCashuWalletTokens
    | KindCashuWalletHistory
    | KindGroupControlEvent Int
    | KindZapGoal
    | KindNutzap
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
    | KindNutzapMintRecommendation
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
    | KindCommunityDefinition
    | KindCashuWalletEvent
    | KindPeerToPeerOrder
    | KindGroupMetadataEvent Int

type alias KindInformation =
    { description : String
    , link : Maybe KindInformationLink
    }

type KindInformationLink
    = LinkToNip Int
    | LinkToNips (List Int)
    | OtherLink String String

informationForKind : Kind -> KindInformation
informationForKind kind =
    case kind of
        KindUserMetadata                -> { description = "User Metadata", link = Just <| LinkToNip 1 }
        KindShortTextNote               -> { description = "Short Text Note", link = Just <| LinkToNip 1 }
        KindRecommendRelay              -> { description = "Recommend Relay", link = Just <| LinkToNip 1 }
        KindFollows                     -> { description = "Follows", link = Just <| LinkToNip 2 }
        KindEncryptedDirectMessage      -> { description = "Encrypted Direct Messages", link = Just <| LinkToNip 4 }
        KindEventDeletionRequest        -> { description = "Event Deletion Request", link = Just <| LinkToNip 9 }
        KindRepost                      -> { description = "Repost", link = Just <| LinkToNip 18 }
        KindReaction                    -> { description = "Reaction", link = Just <| LinkToNip 25 }
        KindBadgeAward                  -> { description = "Badge Award", link = Just <| LinkToNip 58 }
        KindGroupChatMessage            -> { description = "Chat Message", link = Nothing }
        KindGroupChatThreadedReply      -> { description = "Group Chat Threaded Reply", link = Just <| LinkToNip 29 }
        KindGroupThread                 -> { description = "Thread", link = Nothing }
        KindGroupThreadReply            -> { description = "Group Thread Reply", link = Just <| LinkToNip 29 }
        KindSeal                        -> { description = "Seal", link = Just <| LinkToNip 59 }
        KindDirectMessage               -> { description = "Direct Message", link = Just <| LinkToNip 17 }
        KindGenericRepost               -> { description = "Generic Repost", link = Just <| LinkToNip 18 }
        KindReactionToWebsite           -> { description = "Reaction to a website", link = Just <| LinkToNip 25 }
        KindPicture                     -> { description = "Picture", link = Just <| LinkToNip 68 }
        KindChannelCreation             -> { description = "Channel Creation", link = Just <| LinkToNip 28 }
        KindChannelMetadata             -> { description = "Channel Metadata", link = Just <| LinkToNip 28 }
        KindChannelMessage              -> { description = "Channel Message", link = Just <| LinkToNip 28 }
        KindChannelHideMessage          -> { description = "Channel Hide Message", link = Just <| LinkToNip 28 }
        KindChannelMuteUser             -> { description = "Channel Mute User", link = Just <| LinkToNip 28 }
        KindChess                       -> { description = "Chess (PGN)", link = Just <| LinkToNip 64  }
        KindMergeRequest                -> { description = "Merge Requests", link = Just <| LinkToNip 54 }
        KindBid                         -> { description = "Bid", link = Just <| LinkToNip 15 }
        KindBidConfirmation             -> { description = "Bid confirmation", link = Just <| LinkToNip 15 }
        KindOpenTimestamp               -> { description = "OpenTimestamps", link = Just <| LinkToNip 3 }
        KindGiftWrap                    -> { description = "Gift Wrap", link = Just <| LinkToNip 59 }
        KindFileMetadata                -> { description = "File Metadata", link = Just <| LinkToNip 94 }
        KindComment                     -> { description = "Comment", link = Just <| LinkToNip 22 }
        KindLiveChatMessage             -> { description = "Live Chat Message", link = Just <| LinkToNip 53 }
        KindPatches                     -> { description = "Patches", link = Just <| LinkToNip 34 }
        KindIssues                      -> { description = "Issues", link = Just <| LinkToNip 34 }
        KindReplies                     -> { description = "Replies", link = Just <| LinkToNip 34 }
        KindStatus _                    -> { description = "Status", link = Just <| LinkToNip 34 }
        KindProblemTracker              -> { description = "Problem Tracker", link = Just <| OtherLink "nostrocket" "https://github.com/nostrocket/NIPS/blob/main/Problems.md" }
        KindReporting                   -> { description = "Reporting", link = Just <| LinkToNip 56 }
        KindLabel                       -> { description = "Label", link = Just <| LinkToNip 32 }
        KindRelayReview                 -> { description = "Relay reviews", link = Nothing }
        KindAIEmbedding                 -> { description = "AI Embeddings / Vector lists", link = Just <| OtherLink "NKBIP-02" "https://wikistr.com/nkbip-02" }
        KindTorrent                     -> { description = "Torrent", link = Just <| LinkToNip 35 }
        KindTorrentComment              -> { description = "Torrent Comment", link = Just <| LinkToNip 35 }
        KindCoinjoinPool                -> { description = "Coinjoin Pool", link = Just <| OtherLink "joinstr" "https://gitlab.com/1440000bytes/joinstr/-/blob/main/NIP.md" }
        KindCommunityPostApproval       -> { description = "Community Post Approval", link = Just <| LinkToNip 72 }
        KindJobRequest _                -> { description = "Job Request", link = Just <| LinkToNip 90 }
        KindJobResult _                 -> { description = "Job Result", link = Just <| LinkToNip 90 }
        KindJobFeedback                 -> { description = "Job Feedback", link = Just <| LinkToNip 90 }
        KindReservedCashuWalletTokens   -> { description = "Reserved Cashu Wallet Tokens", link = Just <| LinkToNip 60 }
        KindCashuWalletTokens           -> { description = "Cashu Wallet Tokens", link = Just <| LinkToNip 60 }
        KindCashuWalletHistory          -> { description = "Cashu Wallet History", link = Just <| LinkToNip 60 }
        KindGroupControlEvent _         -> { description = "Group Control Events", link = Just <| LinkToNip 29 }
        KindZapGoal                     -> { description = "Zap Goal", link = Just <| LinkToNip 75 }
        KindNutzap                      -> { description = "Nutzap", link = Just <| LinkToNip 61 }
        KindTidalLogin                  -> { description = "Tidal login", link = Just <| OtherLink "Tidal-nostr" "https://wikistr.com/tidal-nostr" }
        KindZapRequest                  -> { description = "Zap Request", link = Just <| LinkToNip 57 }
        KindZapReceipt                  -> { description = "Zap", link = Just <| LinkToNip 57 }
        KindHighlights                  -> { description = "Highlights", link = Just <| LinkToNip 84 }
        KindMuteList                    -> { description = "Mute list", link = Just <| LinkToNip 51 }
        KindPinList                     -> { description = "Pin list", link = Just <| LinkToNip 51 }
        KindRelayListMetadata           -> { description = "Relay List Metadata", link = Just <| LinkToNip 65 }
        KindBookmarkList                -> { description = "Bookmark list", link = Just <| LinkToNip 51 }
        KindCommunitiesList             -> { description = "Communities list", link = Just <| LinkToNip 51 }
        KindPublicChatsList             -> { description = "Public chats list", link = Just <| LinkToNip 51 }
        KindBlockedRelaysList           -> { description = "Blocked relays list", link = Just <| LinkToNip 51 }
        KindSearchRelaysList            -> { description = "Search relays list", link = Just <| LinkToNip 51 }
        KindUserGroups                  -> { description = "User groups", link = Just <| LinkToNips [51, 29] }
        KindInterestsLists              -> { description = "Interests list", link = Just <| LinkToNip 51 }
        KindNutzapMintRecommendation    -> { description = "Nutzap Mint Recommendation", link = Just <| LinkToNip 61 }
        KindUserEmojiList               -> { description = "User emoji list", link = Just <| LinkToNip 51 }
        KindRelayListForDMs             -> { description = "Relay list to receive DMs", link = Just <| LinkToNips [51, 17] }
        KindUserServerList              -> { description = "User server list", link = Just <| OtherLink "Blossom" "https://github.com/hzrd149/blossom" }
        KindFileStorageServerList       -> { description = "File storage server list", link = Just <| LinkToNip 96 }
        KindWalletInfo                  -> { description = "Wallet Info", link = Just <| LinkToNip 47 }
        KindLightningPubRPC             -> { description = "Lightning Pub RPC", link = Just <| OtherLink "Lightning.Pub" "https://github.com/shocknet/Lightning.Pub/blob/master/proto/autogenerated/client.md" }
        KindClientAuthentication        -> { description = "Client Authentication", link = Just <| LinkToNip 42 }
        KindWalletRequest               -> { description = "Wallet Request", link = Just <| LinkToNip 47 }
        KindWalletResponse              -> { description = "Wallet Response", link = Just <| LinkToNip 47 }
        KindNostrConnect                -> { description = "Nostr Connect", link = Just <| LinkToNip 46 }
        KindBlobsStoredOnMediaservers   -> { description = "Blobs stored on mediaservers", link = Just <| OtherLink "Blossom" "https://github.com/hzrd149/blossom" }
        KindHTTPAuth                    -> { description = "HTTP Auth", link = Just <| LinkToNip 98 }
        KindFollowSets                  -> { description = "Follow sets", link = Just <| LinkToNip 51 }
        KindGenericLists                -> { description = "Generic lists", link = Just <| LinkToNip 51 }
        KindRelaySets                   -> { description = "Relay sets", link = Just <| LinkToNip 51 }
        KindBookmarkSets                -> { description = "Bookmark sets", link = Just <| LinkToNip 51 }
        KindCurationSets                -> { description = "Curation sets", link = Just <| LinkToNip 51 }
        KindVideoSets                   -> { description = "Video sets", link = Just <| LinkToNip 51 }
        KindKindMuteSets                -> { description = "Kind mute sets", link = Just <| LinkToNip 51 }
        KindProfileBadges               -> { description = "Profile Badges", link = Just <| LinkToNip 58 }
        KindBadgeDefinition             -> { description = "Badge Definition", link = Just <| LinkToNip 58 }
        KindInterestSets                -> { description = "Interest sets", link = Just <| LinkToNip 51 }
        KindCreateUpdateStall           -> { description = "Create or update a stall", link = Just <| LinkToNip 15 }
        KindCreateUpdateProduct         -> { description = "Create or update a product", link = Just <| LinkToNip 15 }
        KindMarketplaceUIUX             -> { description = "Marketplace UI/UX", link = Just <| LinkToNip 15 }
        KindProductSoldViaAuction       -> { description = "Product sold as an auction", link = Just <| LinkToNip 15 }
        KindLongFormContent             -> { description = "Long-form Content", link = Just <| LinkToNip 23 }
        KindDraftLongFormContent        -> { description = "Draft Long-form Content", link = Just <| LinkToNip 23 }
        KindEmojiSet                    -> { description = "Emoji sets", link = Just <| LinkToNip 51 }
        KindModularArticleHeader        -> { description = "Modular Article Header", link = Just <| OtherLink "NKBIP-01" "https://wikistr.com/nkbip-01" }
        KindModularArticleContent       -> { description = "Modular Article Content", link = Just <| OtherLink "NKBIP-01" "https://wikistr.com/nkbip-01" }
        KindReleaseArtifactSet          -> { description = "Release artifact sets", link = Just <| LinkToNip 51 }
        KindApplicationSpecificData     -> { description = "Application-specific Data", link = Just <| LinkToNip 78 }
        KindLiveEvent                   -> { description = "Live Event", link = Just <| LinkToNip 53 }
        KindUserStatuses                -> { description = "User Statuses", link = Just <| LinkToNip 38 }
        KindSlideSet                    -> { description = "Slide Set", link = Just <| OtherLink "Corny Chat" "https://cornychat.com/datatypes#kind30388slideset" }
        KindClassifiedListing           -> { description = "Classified Listing", link = Just <| LinkToNip 99 }
        KindDraftClassifiedListing      -> { description = "Draft Classified Listing", link = Just <| LinkToNip 99 }
        KindRepositoryAnnouncement      -> { description = "Repository announcements", link = Just <| LinkToNip 34 }
        KindRepositoryStateAnnouncement -> { description = "Repository state announcements", link = Just <| LinkToNip 34 }
        KindWikiArticle                 -> { description = "Wiki article", link = Just <| LinkToNip 54 }
        KindRedirect                    -> { description = "Redirects", link = Just <| LinkToNip 54 }
        KindLinkSet                     -> { description = "Link Set", link = Just <| OtherLink "Corny Chat" "https://cornychat.com/datatypes#kind31388linkset" }
        KindFeed                        -> { description = "Feed", link = Just <| OtherLink "NUD: Custom Feeds" "https://wikifreedia.xyz/cip-01/" }
        KindDateBasedCalendar           -> { description = "Date-Based Calendar Event", link = Just <| LinkToNip 52 }
        KindTimeBasedCalendar           -> { description = "Time-Based Calendar Event", link = Just <| LinkToNip 52 }
        KindCalendar                    -> { description = "Calendar", link = Just <| LinkToNip 52 }
        KindCalendarEvent               -> { description = "Calendar Event RSVP", link = Just <| LinkToNip 52 }
        KindHandlerRecommendation       -> { description = "Handler recommendation", link = Just <| LinkToNip 89 }
        KindHandlerInformation          -> { description = "Handler information", link = Just <| LinkToNip 89 }
        KindVideoEvent                  -> { description = "Video Event", link = Just <| LinkToNip 71 }
        KindShortFormPortraitVideoEvent -> { description = "Short-form Portrait Video Event", link = Just <| LinkToNip 71 }
        KindCommunityDefinition         -> { description = "Community Definition", link = Just <| LinkToNip 72 }
        KindCashuWalletEvent            -> { description = "Cashu Wallet Event", link = Just <| LinkToNip 60 }
        KindPeerToPeerOrder             -> { description = "Peer-to-peer Order events", link = Just <| LinkToNip 69 }
        KindGroupMetadataEvent _        -> { description = "Group metadata events", link = Just <| LinkToNip 29 }
        KindUnknown number              -> { description = "Unknown kind: " ++ String.fromInt number, link = Nothing }
 
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
            20 -> KindPicture
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
            1111 -> KindComment
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
            7374 -> KindReservedCashuWalletTokens
            7375 -> KindCashuWalletTokens
            7376 -> KindCashuWalletHistory
            9041 -> KindZapGoal
            9321 -> KindNutzap
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
            10019 -> KindNutzapMintRecommendation
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
            34550 -> KindCommunityDefinition
            37375 -> KindCashuWalletEvent
            38383 -> KindPeerToPeerOrder
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
        KindPicture                         -> 20
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
        KindComment                         -> 1111
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
        KindReservedCashuWalletTokens       -> 7374
        KindCashuWalletTokens               -> 7375
        KindCashuWalletHistory              -> 7376
        KindGroupControlEvent num           -> 9000
        KindZapGoal                         -> 9041
        KindNutzap                          -> 9321
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
        KindNutzapMintRecommendation        -> 10019
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
        KindCommunityDefinition             -> 34550
        KindCashuWalletEvent                -> 37375
        KindPeerToPeerOrder                 -> 38383
        KindGroupMetadataEvent num          -> num

kindDecoder : Decoder Kind
kindDecoder =
    Decode.int
        |> Decode.map kindFromNumber
    
kindStringDecoder : Decoder Kind
kindStringDecoder =
    Decode.string
        |> Decode.andThen (\kindString ->
                case String.toInt kindString of
                    Just kindInt ->
                        Decode.succeed (kindFromNumber kindInt)

                    Nothing ->
                        Decode.fail <| "String is not kind number: " ++ kindString
            )
    
tagReferenceToString : TagReference -> String
tagReferenceToString tagRef =
    case tagRef of
        TagReferenceEventId eventId ->
            eventId

        TagReferenceCode kind pubKey dCode ->
            buildAddress (kind, pubKey, dCode)

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
        |> Pipeline.optional "relay" (Decode.maybe Nostr.Relay.relayUrlDecoder) Nothing

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

--           "c" ->
--               Decode.map ClientTag (Decode.index 1 Decode.string)

            "client" ->
                Decode.map3 ClientTag (Decode.index 1 Decode.string) (Decode.maybe (Decode.index 2 Decode.string)) (Decode.maybe (Decode.index 3 Decode.string))

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

            "k" ->
                Decode.map KindTag (Decode.index 1 kindStringDecoder)

            "l" ->
                Decode.map2 LocationTag (Decode.index 1 Decode.string) (Decode.maybe (Decode.index 2 Decode.string))

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
                Decode.oneOf
                    [ Decode.map2 UrlTag (Decode.index 1 Decode.string) (Decode.index 2 decodeRelayRole)
                    , Decode.map2 UrlTag (Decode.index 1 Decode.string) (Decode.succeed ReadWriteRelay)
                    ]

            "relay" ->
                Decode.map RelayTag (Decode.index 1 Decode.string)

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

            "web" ->
                Decode.map2 WebTag (Decode.index 1 Decode.string) (Decode.maybe (Decode.index 2 Decode.string))

            "x" ->
                Decode.map ExternalIdTag (Decode.index 1 Decode.string)

            "zap" ->
                Decode.map3 ZapTag (Decode.index 1 Decode.string) (Decode.index 2 Decode.string) (Decode.maybe (Decode.index 3 decodeStringInt))

            _ ->
                decodeGenericTag
    )

decodeGenericTag : Decode.Decoder Tag
decodeGenericTag =
    Decode.oneOf
        [ Decode.map4 GenericTag4 (Decode.index 0 Decode.string) (Decode.index 1 Decode.string) (Decode.index 2 Decode.string) (Decode.index 3 Decode.string)
        , Decode.map3 GenericTag3 (Decode.index 0 Decode.string) (Decode.index 1 Decode.string) (Decode.index 2 Decode.string)
        , Decode.map2 GenericTag2 (Decode.index 0 Decode.string) (Decode.index 1 Decode.string)
        , Decode.map  GenericTag  (Decode.index 0 Decode.string)
        ]

decodeStringInt : Decode.Decoder Int
decodeStringInt =
    Decode.string
    |> Decode.andThen (\strValue ->
            case String.toInt strValue of
                Just intValue ->
                    Decode.succeed intValue

                Nothing ->
                    Decode.fail <| "String can't be converted to int: " ++ strValue
        )

tagToList : Tag -> List String
tagToList tag =
    case tag of
        GenericTag key ->
            [ key ]

        GenericTag2 key value ->
            [ key, value ]

        GenericTag3 key value1 value2 ->
            [ key, value1, value2 ]

        GenericTag4 key value1 value2 value3 ->
            [ key, value1, value2, value3 ]

        AboutTag value ->
            [ "about", value ]

        AddressTag value ->
            [ "a", value ]

        AltTag value ->
            [ "alt", value ]

        ClientTag client maybeAddress maybeRelay ->
            case (maybeAddress, maybeRelay) of
                (Just address, Just relay) ->
                    [ "client", client, address, relay ]

                (Just address, Nothing) ->
                    [ "client", client, address ]

                _ ->
                    [ "client", client ]

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

        KindTag kind ->
            [ "k", String.fromInt <| numberForKind kind ]

        LocationTag value1 maybeValue2 ->
            case maybeValue2 of
                Just value2 ->
                    [ "l", value1, value2 ]

                Nothing ->
                    [ "l", value1 ]

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

        ReferenceTag reference maybeType ->
            case maybeType of
                Just type_ ->
                    [ "r", reference, type_ ]

                Nothing ->
                    [ "r", reference ]

        RelayTag relay ->
            [ "relay", relay ]

        RelaysTag relays ->
            "relays" :: relays

        ServerTag value ->
            [ "server", value ]

        SummaryTag value ->
            [ "summary", value ]

        TitleTag value ->
            [ "title", value ]

        UrlTag relay role ->
            case relayRoleToString role of
                Just roleString ->
                    [ "r", relay, roleString ]
                Nothing ->
                    [ "r", relay ]

        WebTag target maybeType ->
            case maybeType of
                Just type_ ->
                    [ "web", target, type_ ]

                Nothing ->
                    [ "web", target ]

        ZapTag pubKey relayUrl maybeWeight ->
            case maybeWeight of
                Just weight ->
                    [ "zap", pubKey, relayUrl, String.fromInt weight ]

                Nothing ->
                    [ "zap", pubKey, relayUrl ]

buildAddress : (Kind, PubKey, String) -> Address
buildAddress (kind, pubKey, identifier) =
    String.fromInt (numberForKind kind) ++ ":" ++ pubKey ++ ":" ++ identifier

parseAddress : Address -> Maybe (Kind, PubKey, String)
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

addAddressTag : Address -> List Tag -> List Tag
addAddressTag address tags =
    AddressTag address :: tags

addAltTag : String -> List Tag -> List Tag
addAltTag alt tags =
    AltTag alt :: tags

addClientTag : String -> PubKey -> String -> RelayUrl -> List Tag -> List Tag
addClientTag client pubKey identifier relay tags =
    ClientTag client (Just <| buildAddress (KindHandlerInformation, pubKey, identifier)) (Just relay) :: tags

addDTag : String -> List Tag -> List Tag
addDTag identifier tags =
    EventDelegationTag identifier :: tags

addEventIdTag : EventId -> List Tag -> List Tag
addEventIdTag eventId tags =
    EventIdTag eventId :: tags

addKindTag : Kind -> List Tag -> List Tag
addKindTag kind tags =
    KindTag kind :: tags

addKindTags : List Kind -> List Tag -> List Tag
addKindTags kinds tags =
    kinds
    |> List.map KindTag
    |> List.append tags

addPublishedAtTag : Time.Posix -> List Tag -> List Tag
addPublishedAtTag time tags =
    PublishedAtTag time :: tags

addReferenceTags : List (String, Maybe String) -> List Tag -> List Tag
addReferenceTags references tags =
    references
    |> List.map (\(reference, maybeType) -> ReferenceTag reference maybeType)
    |> List.append tags

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


addHashtagTags : List String -> List Tag -> List Tag
addHashtagTags hashtags tags =
    hashtags
    |> List.map String.trim
    |> List.map HashTag
    |> List.append tags

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

addWebTargetTags : List (String, Maybe String) -> List Tag -> List Tag
addWebTargetTags webTargets =
    webTargets
    |> List.map (\(target, type_) -> WebTag target type_)
    |> List.append


addZapTags : List (PubKey, RelayUrl, Maybe Int) -> List Tag -> List Tag
addZapTags zapWeights tags =
    let
        zapTags =
            zapWeights
            |> List.map (\(pubKey, relayUrl, weight) ->
                    ZapTag pubKey relayUrl weight
                )
    in
    tags ++ zapTags