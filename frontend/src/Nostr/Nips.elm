module Nostr.Nips exposing (..)


type alias Nip =
    { number : String
    , title : String
    }


descriptionForNip : String -> Maybe String
descriptionForNip number =
    case number of
        "01" ->
            Just "Basic protocol flow description"

        "02" ->
            Just "Follow List"

        "03" ->
            Just "OpenTimestamps Attestations for Events"

        "04" ->
            Just "Encrypted Direct Message"

        "05" ->
            Just "Mapping Nostr keys to DNS-based internet identifiers"

        "06" ->
            Just "Basic key derivation from mnemonic seed phrase"

        "07" ->
            Just "window.nostr capability for web browsers"

        "08" ->
            Just "Handling Mentions"

        "09" ->
            Just "Event Deletion Request"

        "10" ->
            Just "Conventions for clients' use of e and p tags in text events"

        "11" ->
            Just "Relay Information Document"

        "13" ->
            Just "Proof of Work"

        "14" ->
            Just "Subject tag in text events"

        "15" ->
            Just "Nostr Marketplace (for resilient marketplaces)"

        "17" ->
            Just "Private Direct Messages"

        "18" ->
            Just "Reposts"

        "19" ->
            Just "bech32-encoded entities"

        "21" ->
            Just "nostr: URI scheme"

        "22" ->
            Just "Comment"

        "23" ->
            Just "Long-form Content"

        "24" ->
            Just "Extra metadata fields and tags"

        "25" ->
            Just "Reactions"

        "26" ->
            Just "Delegated Event Signing"

        "27" ->
            Just "Text Note References"

        "28" ->
            Just "Public Chat"

        "29" ->
            Just "Relay-based Groups"

        "30" ->
            Just "Custom Emoji"

        "31" ->
            Just "Dealing with Unknown Events"

        "32" ->
            Just "Labeling"

        "34" ->
            Just "git stuff"

        "35" ->
            Just "Torrents"

        "36" ->
            Just "Sensitive Content"

        "37" ->
            Just "Draft Events"

        "38" ->
            Just "User Statuses"

        "39" ->
            Just "External Identities in Profiles"

        "40" ->
            Just "Expiration Timestamp"

        "42" ->
            Just "Authentication of clients to relays"

        "44" ->
            Just "Encrypted Payloads (Versioned)"

        "45" ->
            Just "Counting results"

        "46" ->
            Just "Nostr Remote Signing"

        "47" ->
            Just "Nostr Wallet Connect"

        "48" ->
            Just "Proxy Tags"

        "49" ->
            Just "Private Key Encryption"

        "50" ->
            Just "Search Capability"

        "51" ->
            Just "Lists"

        "52" ->
            Just "Calendar Events"

        "53" ->
            Just "Live Activities"

        "54" ->
            Just "Wiki"

        "55" ->
            Just "Android Signer Application"

        "56" ->
            Just "Reporting"

        "57" ->
            Just "Lightning Zaps"

        "58" ->
            Just "Badges"

        "59" ->
            Just "Gift Wrap"

        "60" ->
            Just "Cashu Wallet"

        "61" ->
            Just "Nutzaps"

        "64" ->
            Just "Chess (PGN)"

        "65" ->
            Just "Relay List Metadata"

        "68" ->
            Just "Picture-first feeds"

        "69" ->
            Just "Peer-to-peer Order events"

        "70" ->
            Just "Protected Events"

        "71" ->
            Just "Video Events"

        "72" ->
            Just "Moderated Communities"

        "73" ->
            Just "External Content IDs"

        "75" ->
            Just "Zap Goals"

        "78" ->
            Just "Application-specific data"

        "84" ->
            Just "Highlights"

        "86" ->
            Just "Relay Management API"

        "89" ->
            Just "Recommended Application Handlers"

        "90" ->
            Just "Data Vending Machines"

        "92" ->
            Just "Media Attachments"

        "94" ->
            Just "File Metadata"

        "96" ->
            Just "HTTP File Storage Integration"

        "98" ->
            Just "HTTP Auth"

        "99" ->
            Just "Classified Listings"

        "7D" ->
            Just "Threads"

        "C7" ->
            Just "Chats"

        _ ->
            Nothing
