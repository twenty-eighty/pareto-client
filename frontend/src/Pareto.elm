module Pareto exposing (..)

import Nostr.Event exposing (Kind(..))
import Nostr.HandlerInformation exposing (HandlerInformation)
import Nostr.Nip05 as Nip05
import Nostr.Profile exposing (Profile)
import Nostr.Types exposing (PubKey, RelayRole(..), RelayUrl)
import Time



-- the follow list of this pubkey contains all Pareto authors


authorsKey : PubKey
authorsKey =
    "0f47948ccf4d12064ede2e0aa744868a2443cb1c42b32c06191e0d902205abef"


rssAuthorsKey : PubKey
rssAuthorsKey =
    "0f4791c38e1236dc55f11acbf37a00da8879906d9374498378db8d6ea7952869"


editorKey : PubKey
editorKey =
    "0f479cb726c1578ca765d5ff6a0c58855263977d5d7cf7b4cea23d42d557c611"



-- name of client in "client" tag when publishing articles


client : String
client =
    "Pareto"


applicationDomain : String
applicationDomain =
    "pareto.space"


applicationUrl : String
applicationUrl =
    "https://" ++ applicationDomain


supportEmail : String
supportEmail =
    "support@" ++ applicationDomain


privacyPolicyGerman : String
privacyPolicyGerman =
    "/md/privacy-de.md"


paretoNip05 : Nip05.Nip05
paretoNip05 =
    { user = "client"
    , domain = applicationDomain
    }


paretoRelay : RelayUrl
paretoRelay =
    "wss://nostr." ++ applicationDomain


paretoRelays : List String
paretoRelays =
    [ paretoRelay
    , "wss://pareto.nostr1.com"
    ]


defaultNip96ServersAuthors : List String
defaultNip96ServersAuthors =
    [ "https://route96.pareto.space"
    ]


defaultNip96ServersPublic : List String
defaultNip96ServersPublic =
    [ "https://void.cat"
    ]


applicationDataRelays : List RelayUrl
applicationDataRelays =
    [ "cool-darkness-73116.pktriot.net"
    ]


teamRelay : RelayUrl
teamRelay =
    "team-relay.pareto.space"


paretoOutboxRelays : List RelayUrl
paretoOutboxRelays =
    [ "nostr.pareto.space"
    , "pareto.nostr1.com"
    ]


recommendedOutboxRelays : List RelayUrl
recommendedOutboxRelays =
    [ "relay.snort.social"
    , "relay.nostr.band"
    , "relay.damus.io"
    , "nos.lol"
    , "offchain.pub"
    , "nostr.wine"
    ]


recommendedInboxRelays : List RelayUrl
recommendedInboxRelays =
    [ "nostr.pareto.space"
    , "pareto.nostr1.com"
    ]
        ++ recommendedOutboxRelays


defaultRelays : List RelayUrl
defaultRelays =
    recommendedInboxRelays


defaultSearchRelays : List RelayUrl
defaultSearchRelays =
    [ "relay.nostr.band"
    , "nostr.wine"
    ]


defaultOutboxRelays : List { url : RelayUrl, role : RelayRole }
defaultOutboxRelays =
    defaultRelays
        |> List.map (\relayUrl -> { url = relayUrl, role = ReadWriteRelay })


defaultRelayUrls : List RelayUrl
defaultRelayUrls =
    defaultRelays
        |> List.map (String.append "wss://")



-- list of authors for Pareto category
-- the list is replaced after the follow list of above key authorsKey has been loaded


bootstrapAuthorsList : List ( String, PubKey )
bootstrapAuthorsList =
    [ ( "milosz@pareto.space", "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638" )
    , ( "ashoka@pareto.space", "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5" )
    , ( "roland@pareto.space", "cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2" )
    , ( "donjoe@pareto.space", "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe" )
    , ( "janosch@pareto.space", "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01" )
    , ( "arottmann@grooveix.com", "a95c624384c60902aaac9de52ab0fce39a9e8daa3d2b14236573a093d345522c" )
    , ( "denkbar@grooveix.com", "c631e26716c6b1a404dac828be44fdd403e65a0e5d05af00e24c3524c2b78d3e" )
    , ( "psychobabble@pareto.town", "6734e11d8d67d9ca4dedb920f81182ded2bca918e3e0f3528bd5f4c4c7e34e8f" )
    , ( "walter_siegrist@pareto.space", "78c90fc422bdadafb4df0de5d1ed87133265f896949705bebc61aee44bff983c" )
    , ( "_@pareto.space", "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84" )
    , ( "martin@enlund.org", "4234223996ce6549720e66dd6bc4bb7efb9f25c60c4816d7bc47a65e1d80db24" )
    , ( "christof_weber@pareto.space", "79271c81ef2fd4994c20d73b0555a3e58d7ee9caaa2328082e3a336de18d9066" )
    , ( "jklnz@getalby.com", "a38a5cd0ec24d3f20c4870a032c1989109bf107c4ce8648fdc5a9c4492b1b8f0" )
    , ( "rp@pareto.town", "2fb77d268b93380a7c3152c610bb373165569a664d445a33140ac048c47a6ee1" )
    , ( "marcel@pareto.town", "7f29628df734cf02f3407580184af73d941e940f4a91c61d1e15aa08e160cccc" )
    , ( "peter@pareto.town", "c3b2802bd061e0ea1c670604f87ad4d7ab8b487f01ccb417224baedb4850599c" )
    , ( "michael_meyen@pareto.town", "044da3442a54bd55202b66ca0c4f5fd58cbb158b67f2fb067cc0467c073a8a0e" )
    , ( "Genexyz", "2063cd7955cffdca0cc4ae20f77b2b1eb053010e534c18e8ca70222157bd1320" )
    , ( "te@pareto.town", "fe02e8ece33b9e01b4225a01bb373552dc6f78744ad8121698c30bfaf07fbe0b" )
    , ( "Paul Andersson", "2516d14559174be1a2e578c333ef584e2f32b45931bddb624d0178e4070c8fa1" )
    , ( "Eva Schmidt", "1731c73ccb388c5574b97d349c22c5f34271cef841129eb719e418cc1bca4ecc" )
    , ( "simon.kramer@sk-nostr.ch", "e77fe29e0868513a47b68b5941332139432b8e600140791163173b636fe8bc9d" )
    , ( "NACHHALL", "712db8c83700aca0ffa1c3b759929e18d26fcc2b6eeb07305bdf33760b42cbce" )
    , ( "bitcoinlighthouse@nsec.app", "638384700918e6a472477045dbcc229362ac0e64a48d927c48af609a956b9348" )
    , ( "Aron Morhoff", "9f94e6cc5ce50dcaccfc42b18029aba0ac9215d673197a40172896d3f3472946" )
    , ( "blingbling@pareto.town", "b8af284d20a109766c55cc5d4aea27f9c3df20f68f8d0eb5b5de3349f82c91dd" )
    , ( "snicklink@pareto.town", "d898c64d7901515fbe9d54c64d740713a92e5b4c1783a16e37ff1c531298b265" )
    , ( "1bis19@pareto.town", "df32e891bfcbe0aed4518b21466c1807ea68394fe4f1fcd8c826a3ca1ed0b7e7" )
    , ( "minjun@nostrplebs.com", "10057360336a78d58503ead8f5993cf8f970e1cd66dc4058c33ec508d01ac746" )
    , ( "berny91@nostrpurple.com", "6a90a94022b752923114c0564b640021258850a0b0b6b7db2a366b7ff83e8bd9" )
    , ( "madmunky@nostrplebs.com", "3eacaa768326d7dce80f6ee17ada199bebe7eb3c1a60b39b14e0a58bbac66fe4" )
    , ( "pavlenex@iris.to", "175f568d77fb0cb7400f0ddd8aed1738cd797532b314ef053a1669d4dba7433a" )
    , ( "barbouille@pareto.town", "68c969eafadfc88d5937770d81031fb314b1bb1c201671403de6d930ab67edb9" )
    , ( "free-cities@pareto.town", "fc2470ed196801ddace5c2bcb14a53fa5ee9f81ee365bf4958f94c6117d9ee27" )
    ]


paretoClientNpub : String
paretoClientNpub =
    "npub1parecl0l0w6nmtjn7wan9tg3p8kmkpa62c4a65tgq39n7smyu76sht8cm5"


paretoClientPubKey : PubKey
paretoClientPubKey =
    "0f479c7dff7bb53dae53f3bb32ad1109edbb07ba562bdd5168044b3f4364e7b5"


handlerIdentifier : String
handlerIdentifier =
    "8020802080208020"


supportedKinds : List Kind
supportedKinds =
    [ KindUserMetadata
    , KindEventDeletionRequest
    , KindFileMetadata
    , KindRelayListMetadata
    , KindBookmarkList
    , KindUserServerList
    , KindFileStorageServerList
    , KindClientAuthentication
    , KindBlobsStoredOnMediaservers
    , KindHTTPAuth
    , KindLongFormContent
    , KindDraftLongFormContent
    , KindDraft
    , KindHandlerRecommendation
    , KindHandlerInformation
    ]


supportedNips : List String
supportedNips =
    [ "01"

    -- , "02"
    -- , "04" -- Encrypted Direct Message
    , "07"
    , "09"
    , "11"
    , "19"
    , "21"
    , "23"
    , "24"
    , "25"
    , "31"
    , "37"
    , "42"
    , "44"
    , "51"
    , "65"
    , "89"
    , "94"
    , "96"
    , "98"
    ]


paretoAbout : String
paretoAbout =
    "A #Nostr client for publishing and an ecosystem for citizen journalism."


paretoHashtags : List String
paretoHashtags =
    [ "pareto"
    , "nostr client"
    , "freedom"
    , "journalism"
    ]


paretoProfile : Profile
paretoProfile =
    { nip05 = Just paretoNip05
    , lud16 = Just "donate2pareto@walletofsatoshi.com"
    , name = Just <| String.toLower client
    , displayName = Just client
    , about = Just paretoAbout
    , picture = Just "https://route96.pareto.space/2f792072b0a16feffef970fcdbe6454bea9cabec03fe6ea55a17b91e2a985e3a.webp"
    , banner = Nothing
    , website = Just applicationUrl
    , bot = Nothing
    , npub = Just paretoClientNpub
    , createdAt = Nothing
    , pubKey = paretoClientPubKey
    , identities = []
    }


paretoReferences : List ( String, Maybe String )
paretoReferences =
    [ ( "https://pareto.space/read", Nothing )
    ]


paretoWebTargets : List ( String, Maybe String )
paretoWebTargets =
    [ ( "https://pareto.space/a/<bech32>", Just "naddr" )

    --    , ("https://pareto.space/e/<bech32>", Just "nevent")
    , ( "https://pareto.space/p/<bech32>", Just "nprofile" )
    ]


paretoZapTargets : List ( PubKey, RelayUrl, Maybe Int )
paretoZapTargets =
    [ ( paretoClientPubKey, paretoRelay, Just 1 )
    ]


paretoAltText : String
paretoAltText =
    "Nostr App: " ++ String.toLower client


applicationInformation : Time.Posix -> HandlerInformation
applicationInformation time =
    { alt = paretoAltText
    , handlerIdentifier = handlerIdentifier
    , hashtags = paretoHashtags
    , kinds = supportedKinds
    , pubKey = paretoClientPubKey
    , profile = paretoProfile
    , references = paretoReferences
    , time = time
    , webTargets = paretoWebTargets
    , zapTargets = paretoZapTargets
    }
