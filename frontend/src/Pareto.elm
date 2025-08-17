module Pareto exposing (..)

import Dict exposing (Dict)
import Locale exposing (Language(..))
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


emailGatewayKey : PubKey
emailGatewayKey =
    "cefbf43addd677426c671d7cd275289be35f7b6b398fced7fae420d060e7a345"


emailGatewayTestKey : PubKey
emailGatewayTestKey =
    "0f479318a753ca86fd9b41a5db955d1f9b416c9790f16f9092ed0fe52a85412e"


subscriptionServerKey : PubKey
subscriptionServerKey =
    "f7721f8728935c943710a2f06288cbd56da7ab20b43400a16d26ac58880e0087"


betaTestKey : PubKey
betaTestKey =
    "0f479ef1a8870a917afbae778ea75fedb5db4cb64501e0e64a6d2010a2908e64"


anonymousPublicKey : String
anonymousPublicKey =
    "ecdf32491ef8b5f1902109f495e7ca189c6fcec76cd66b888fa9fc2ce87f40db"


anonymousPrivateKey : String
anonymousPrivateKey =
    "cff56394373edfaa281d2e1b5ad1b8cafd8b247f229f2af2c61734fb0c7b3f84"


newsletterAuthorCheckEndpointPubKey : String
newsletterAuthorCheckEndpointPubKey =
    "https://pareto.town/api/pubkeys"


newsletterAuthorCheckEndpointNip05 : String
newsletterAuthorCheckEndpointNip05 =
    "https://pareto.town/api/nip05"



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


imprint : Language -> Maybe String
imprint language =
    case language of
        German _ ->
            Just imprintGerman

        _ ->
            -- TODO: Add here additional language versions
            Nothing


imprintGerman : String
imprintGerman =
    "/md/imprint-de.md"


privacyPolicy : Language -> Maybe String
privacyPolicy language =
    case language of
        German _ ->
            Just privacyPolicyGerman

        _ ->
            -- TODO: Add here additional language versions
            Nothing


privacyPolicyGerman : String
privacyPolicyGerman =
    "/md/privacy-de.md"


technicalDetails : String
technicalDetails =
    "/md/tech-details.md"


source : String
source =
    "https://github.com/twenty-eighty/pareto-client"


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
    , "wss://nostr.pareto.town"
    , "wss://pareto.nostr1.com"
    ]


paretoNip96Server : String
paretoNip96Server =
    "route96.pareto.space"


defaultNip96ServersAuthors : List String
defaultNip96ServersAuthors =
    [ "https://" ++ paretoNip96Server
    ]


defaultNip96ServersPublic : List String
defaultNip96ServersPublic =
    [ "https://nostr.download"
    ]


applicationDataRelays : List RelayUrl
applicationDataRelays =
    [ "wss://portal-relay.pareto.space"
    , "wss://portal-relay.pareto.town"
    ]


teamRelay : RelayUrl
teamRelay =
    "team-relay.pareto.space"


testRelayUrls : List RelayUrl
testRelayUrls =
    [ "client-test.pareto.space"
    ]


paretoOutboxRelays : List RelayUrl
paretoOutboxRelays =
    [ "nostr.pareto.space"
    , "nostr.pareto.town"
    , "pareto.nostr1.com"
    ]


recommendedOutboxRelays : List RelayUrl
recommendedOutboxRelays =
    [ "relay.nostr.band"
    , "relay.damus.io"
    , "nos.lol"
    , "offchain.pub"
    , "nostr.wine"
    ]


recommendedInboxRelays : List RelayUrl
recommendedInboxRelays =
    [ "nostr.pareto.space"
    , "nostr.pareto.town"
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


bootstrapAuthorsList : Dict String PubKey
bootstrapAuthorsList =
    [ ( "_@pareto.space", "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84" )
    , ( "1millionstimmen@pareto.town", "dfff06f24536cded1e33d5d5473d011553dc80d13f55f25c195c6dcc186c221b" )
    , ( "arottmann@grooveix.com", "a95c624384c60902aaac9de52ab0fce39a9e8daa3d2b14236573a093d345522c" )
    , ( "ashoka@pareto.space", "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5" )
    , ( "barbouille@pareto.town", "68c969eafadfc88d5937770d81031fb314b1bb1c201671403de6d930ab67edb9" )
    , ( "BeateundDieter@pareto.town", "a29cfc651c98569171b8dc7f0b0c6ae7b0bfe6339a7f11abdeac87c5484fac9c" )
    , ( "bitcoinlighthouse@nsec.app", "638384700918e6a472477045dbcc229362ac0e64a48d927c48af609a956b9348" )
    , ( "bitmax@pareto.town", "aa8de34ff8b18ac3cc56decdfe56f17ec3007385993c834c9b99c839a6ffe696" )
    , ( "blingbling@pareto.town", "b8af284d20a109766c55cc5d4aea27f9c3df20f68f8d0eb5b5de3349f82c91dd" )
    , ( "christof_weber@pareto.space", "79271c81ef2fd4994c20d73b0555a3e58d7ee9caaa2328082e3a336de18d9066" )
    , ( "corbett-report@pareto.town", "c0f2b5f38dacea67dcfdd073848bf11eb1f29550ab8579d587e4c41c452446a1" )
    , ( "denkbar@grooveix.com", "c631e26716c6b1a404dac828be44fdd403e65a0e5d05af00e24c3524c2b78d3e" )
    , ( "di.stu@pareto.town", "b9abb1bed8b2a1d7e9c24f589df5253f27c048b2914dcce4fb63241b025d4545" )
    , ( "docwatson@pareto.town", "71ca1183d74b415e829c225d07e6dcf5fb7ef9bddcc7f644466015d4f12a2039" )
    , ( "donjoe@pareto.space", "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe" )
    , ( "eva.schmidt@pareto.town", "1731c73ccb388c5574b97d349c22c5f34271cef841129eb719e418cc1bca4ecc" )
    , ( "free-cities@pareto.town", "fc2470ed196801ddace5c2bcb14a53fa5ee9f81ee365bf4958f94c6117d9ee27" )
    , ( "friedenstaube@pareto.town", "866e013908559f15c5eff9d1295453082f01a1fb5f40a25bcf0776a36a9334e5" )
    , ( "genexyz@pareto.town", "2063cd7955cffdca0cc4ae20f77b2b1eb053010e534c18e8ca70222157bd1320" )
    , ( "georgohrweh@pareto.town", "a296b972062908df04b056edad5d0c9a8847aa1ed9cfe062ed69c0c2e5a7a2e8" )
    , ( "janosch@pareto.space", "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01" )
    , ( "jklnz@getalby.com", "a38a5cd0ec24d3f20c4870a032c1989109bf107c4ce8648fdc5a9c4492b1b8f0" )
    , ( "k-ai-katoura@pareto.town", "43c002e9425ff9146772391958f91cc4d6a5cd4a18270c2f3828b5b0cde0c29a" )
    , ( "kw@pareto.town", "362cb42dafd4aa582078817ae9eba58c5cdaf3a4023baacc58be75ab312c3984" )
    , ( "kaydee@pareto.town", "69eea7349187e4c96c42dbaec1ef76ba4eea0d0f3fa9bd08364e502a4ae31ae6" )
    , ( "kulturkolorist@pareto.town", "41959693dbfe58d58430c0215ed0cb15545b6574a22e14163de3c3213888319c" )
    , ( "marcel@pareto.town", "7f29628df734cf02f3407580184af73d941e940f4a91c61d1e15aa08e160cccc" )
    , ( "martin@enlund.org", "4234223996ce6549720e66dd6bc4bb7efb9f25c60c4816d7bc47a65e1d80db24" )
    , ( "matej@pareto.space", "5aa5e38abbb37f89c863419bd1e4e60aa31d82fa3c39397e386586e3961b8021" )
    , ( "mathias-broeckers@pareto.town", "6e4b486f698ee18dde7e5bec088c786d31f82c93d7d47cd0cdbcd63ddcadf247" )
    , ( "metamsol@pareto.town", "d7e3ec2295429efc6a1067bacc8ca78af80aae94cc04a9b48156de85a38eac4e" )
    , ( "michael_meyen@pareto.town", "044da3442a54bd55202b66ca0c4f5fd58cbb158b67f2fb067cc0467c073a8a0e" )
    , ( "milosz@pareto.space", "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638" )
    , ( "minjun@nostrplebs.com", "10057360336a78d58503ead8f5993cf8f970e1cd66dc4058c33ec508d01ac746" )
    , ( "mkd@pareto.town", "e6c6c4a88a96b58b4f949494142e20ec7af55e037f3505fc379fd343118f4e15" )
    , ( "mwgfd@pareto.town", "3f01ee5e522155cd85c31389fb2ff71113f405b3c861bd427a90df4a09735315" )
    , ( "moneysecrets@pareto.town", "8b46bd30c1f0a3fd8680639b9b88b78c1c1406988546dfe4445810b76539b5d0" )
    , ( "mx12art@pareto.town", "0b26f590631b0fa3048d103410e484e5e22e2d5a8eceaceda9d91b38f81dd1a8" )
    , ( "nexus@pareto.town", "d1d190fa3ecb81a45669bd343d3b9952eb73611cb3cb4c4349980e4de4ce6a6a" )
    , ( "norbs@pareto.town", "0d1702d6ab3cfdbd6a5687118b9f0008022e129003d8a9627c444d4a8f1ac66f" )
    , ( "patric@pareto.town", "0b65f96a7f23a5739c3b58375d36822e68deb5efdaf2afaccbb365fe7fda4c8f" )
    , ( "patrickreiser@pareto.town", "56dbb22fcd72819a43e37222c2a84463c53b6b5ce31f2c3ae97576dee6bb47f8" )
    , ( "paul@pareto.town", "2516d14559174be1a2e578c333ef584e2f32b45931bddb624d0178e4070c8fa1" )
    , ( "peter@pareto.town", "c3b2802bd061e0ea1c670604f87ad4d7ab8b487f01ccb417224baedb4850599c" )
    , ( "power-epgenetics@pareto.town", "04b126500b5e03571ab3f06babe2bab8e0549246a9f82cb023460dc3064a4882" )
    , ( "psychobabble@pareto.town", "6734e11d8d67d9ca4dedb920f81182ded2bca918e3e0f3528bd5f4c4c7e34e8f" )
    , ( "quillie@nostria.space", "6c2d68ba016c291417fd18ea7c06b737ec143f7d56d78fdd44a5b248846525ec" )
    , ( "ralph-boes@pareto.town", "e0a24c5cb212e5a1911d70e148769361e00a92ca4f57990c21051bdefa44b1e7" )
    , ( "rbm@pareto.town", "04cb16e4a61034bc4212af6814bb909bcb3b4915f08ff77d50440a102ec3e5d5" )
    , ( "rm@pareto.town", "2b24a1fa310d5c0f0299c10f13c5f280935ed02407593a8bdb19612d17750f64" )
    , ( "rodant@pareto.space", "71df211931d26ee41121d295bd43cbc7e382505e333b5c13d4016ced9542d9d7" )
    , ( "roland@pareto.space", "cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2" )
    , ( "rosaskanal@pareto.town", "99e7936f129df8c6c826df78a3be975f166081902bde9046848cbf1ed5d2197c" )
    , ( "rp@pareto.town", "2fb77d268b93380a7c3152c610bb373165569a664d445a33140ac048c47a6ee1" )
    , ( "simon.kramer@sk-nostr.ch", "e77fe29e0868513a47b68b5941332139432b8e600140791163173b636fe8bc9d" )
    , ( "sinautoshi@pareto.town", "04ea4f8350f1562aa1d60dc070561f5bb8386a11d1a00570fd7440da210e1713" )
    , ( "snicklink@pareto.town", "d898c64d7901515fbe9d54c64d740713a92e5b4c1783a16e37ff1c531298b265" )
    , ( "someone@pareto.town", "9fec72d579baaa772af9e71e638b529215721ace6e0f8320725ecbf9f77f85b1" )
    , ( "te@pareto.town", "fe02e8ece33b9e01b4225a01bb373552dc6f78744ad8121698c30bfaf07fbe0b" )
    , ( "timogenes@pareto.town", "f292da7ec191c940e0c94cbe3805f701a4ad61336911c910f3ef3d6fc72fca03" )
    , ( "unsere-verfassung@pareto.town", "56f279150240a326e2bd58f6b7a2486c84f4cfb4baacbdbdf1e8145d5fee3024" )
    , ( "unzicker@pareto.town", "0e5b795a07479e4e5fcbe197a09284b6a8fecb89e63481818453baf19176f766" )
    , ( "wagschale@pareto.town", "4f2dadc9135248d24832e1a50c43ac438e8ccfcd81c29abdcf060176ad1a034c" )
    , ( "walter_siegrist@pareto.space", "78c90fc422bdadafb4df0de5d1ed87133265f896949705bebc61aee44bff983c" )
    , ( "womanbynature@pareto.town", "fbb0eb9dec0e6f31f1a677d1eca902a5deb405af8fe0e50ac61c73dcbdb78256" )
    , ( "worteimfluss@pareto.town", "f29724712fed4a85e3b34d23a24e90f8586fba79f93ab0c6dbc2a5e8067b7e16" )

    -- , ( "1bis19@pareto.town", "df32e891bfcbe0aed4518b21466c1807ea68394fe4f1fcd8c826a3ca1ed0b7e7" )
    -- , ( "alix@pareto.town", "c93ea055634fb683be4202dd04e336663fb7823c5f1c35ad26b522a3eed5ac4c" )
    -- , ( "nachhall@pareto.town", "712db8c83700aca0ffa1c3b759929e18d26fcc2b6eeb07305bdf33760b42cbce" )
    -- , ( "schwurbeloase@pareto.town", "6bba4af1d6fe611658705e949c3e921836592f1a11e03d4d13771dfe5ff587e1" )
    -- , ( "volker.schubert@pareto.town", "80f49d824217e4730aadb729d38775bb2a31405b92a3d70ea37b73fcab3cb150" )
    ]
        |> Dict.fromList


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
    , KindRepost
    , KindPicture
    , KindComment
    , KindFileMetadata
    , KindZapRequest
    , KindZapReceipt
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
    , lud06 = Nothing
    , lud16 = Just "client@pareto.space"
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
    , relays = paretoRelays
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
