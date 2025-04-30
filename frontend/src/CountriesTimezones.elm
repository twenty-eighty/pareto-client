module CountriesTimezones exposing (Country(..), codeForCountry, codeToCountryDict, countries, localizedCountryName, timezoneToCountryDict, timezonesForCountry)

import Dict exposing (Dict)
import I18Next
import Translations.CountriesTimezones as Translations

type Country
    = AD
    | AE
    | AF
    | AG
    | AI
    | AL
    | AM
    | AO
    | AQ
    | AR
    | AS
    | AT
    | AU
    | AW
    | AX
    | AZ
    | BA
    | BB
    | BD
    | BE
    | BF
    | BG
    | BH
    | BI
    | BJ
    | BL
    | BM
    | BN
    | BO
    | BQ
    | BR
    | BS
    | BT
    | BW
    | BY
    | BZ
    | CA
    | CC
    | CD
    | CF
    | CG
    | CH
    | CI
    | CK
    | CL
    | CM
    | CN
    | CO
    | CR
    | CU
    | CV
    | CW
    | CX
    | CY
    | CZ
    | DE
    | DJ
    | DK
    | DM
    | DO
    | DZ
    | EC
    | EE
    | EG
    | EH
    | ER
    | ES
    | ET
    | FI
    | FJ
    | FK
    | FM
    | FO
    | FR
    | GA
    | GB
    | GD
    | GE
    | GF
    | GG
    | GH
    | GI
    | GL
    | GM
    | GN
    | GP
    | GQ
    | GR
    | GS
    | GT
    | GU
    | GW
    | GY
    | HK
    | HN
    | HR
    | HT
    | HU
    | ID
    | IE
    | IL
    | IM
    | IN
    | IO
    | IQ
    | IR
    | IS
    | IT
    | JE
    | JM
    | JO
    | JP
    | KE
    | KG
    | KH
    | KI
    | KM
    | KN
    | KP
    | KR
    | KW
    | KY
    | KZ
    | LA
    | LB
    | LC
    | LI
    | LK
    | LR
    | LS
    | LT
    | LU
    | LV
    | LY
    | MA
    | MC
    | MD
    | ME
    | MF
    | MG
    | MH
    | MK
    | ML
    | MM
    | MN
    | MO
    | MP
    | MQ
    | MR
    | MS
    | MT
    | MU
    | MV
    | MW
    | MX
    | MY
    | MZ
    | NA
    | NC
    | NE
    | NF
    | NG
    | NI
    | NL
    | NO
    | NP
    | NR
    | NU
    | NZ
    | OM
    | PA
    | PE
    | PF
    | PG
    | PH
    | PK
    | PL
    | PM
    | PN
    | PR
    | PS
    | PT
    | PW
    | PY
    | QA
    | RE
    | RO
    | RS
    | RU
    | RW
    | SA
    | SB
    | SC
    | SD
    | SE
    | SG
    | SH
    | SI
    | SJ
    | SK
    | SL
    | SM
    | SN
    | SO
    | SR
    | SS
    | ST
    | SV
    | SX
    | SY
    | SZ
    | TC
    | TD
    | TF
    | TG
    | TH
    | TJ
    | TK
    | TL
    | TM
    | TN
    | TO
    | TR
    | TT
    | TV
    | TW
    | TZ
    | UA
    | UG
    | UM
    | US
    | UY
    | UZ
    | VA
    | VC
    | VE
    | VG
    | VI
    | VN
    | VU
    | WF
    | WS
    | YE
    | YT
    | ZA
    | ZM
    | ZW


countries : List Country
countries =
    [ AD
    , AE
    , AF
    , AG
    , AI
    , AL
    , AM
    , AO
    , AQ
    , AR
    , AS
    , AT
    , AU
    , AW
    , AX
    , AZ
    , BA
    , BB
    , BD
    , BE
    , BF
    , BG
    , BH
    , BI
    , BJ
    , BL
    , BM
    , BN
    , BO
    , BQ
    , BR
    , BS
    , BT
    , BW
    , BY
    , BZ
    , CA
    , CC
    , CD
    , CD
    , CF
    , CG
    , CH
    , CI
    , CK
    , CL
    , CM
    , CN
    , CO
    , CR
    , CU
    , CV
    , CW
    , CX
    , CY
    , CZ
    , DE
    , DJ
    , DK
    , DM
    , DO
    , DZ
    , EC
    , EE
    , EG
    , EH
    , ER
    , ES
    , ET
    , FI
    , FJ
    , FK
    , FM
    , FO
    , FR
    , GA
    , GB
    , GD
    , GE
    , GF
    , GG
    , GH
    , GI
    , GL
    , GM
    , GN
    , GP
    , GQ
    , GR
    , GS
    , GT
    , GU
    , GW
    , GY
    , HK
    , HN
    , HR
    , HT
    , HU
    , ID
    , IE
    , IL
    , IM
    , IN
    , IO
    , IQ
    , IR
    , IS
    , IT
    , JE
    , JM
    , JO
    , JP
    , KE
    , KG
    , KH
    , KI
    , KM
    , KN
    , KP
    , KR
    , KW
    , KY
    , KZ
    , LA
    , LB
    , LC
    , LI
    , LK
    , LR
    , LS
    , LT
    , LU
    , LV
    , LY
    , MA
    , MC
    , MD
    , ME
    , MF
    , MG
    , MH
    , MK
    , ML
    , MM
    , MN
    , MO
    , MP
    , MQ
    , MR
    , MS
    , MT
    , MU
    , MV
    , MW
    , MX
    , MY
    , MY
    , MZ
    , NA
    , NC
    , NE
    , NF
    , NG
    , NI
    , NL
    , NO
    , NP
    , NR
    , NU
    , NZ
    , OM
    , PA
    , PE
    , PF
    , PG
    , PH
    , PK
    , PL
    , PM
    , PN
    , PR
    , PS
    , PT
    , PW
    , PY
    , QA
    , RE
    , RO
    , RS
    , RU
    , RW
    , SA
    , SB
    , SC
    , SD
    , SE
    , SG
    , SH
    , SI
    , SJ
    , SK
    , SL
    , SM
    , SN
    , SO
    , SR
    , SS
    , ST
    , SV
    , SX
    , SY
    , SZ
    , TC
    , TD
    , TF
    , TG
    , TH
    , TJ
    , TK
    , TL
    , TM
    , TN
    , TO
    , TR
    , TT
    , TV
    , TW
    , TZ
    , UA
    , UG
    , UM
    , US
    , UY
    , UZ
    , VA
    , VC
    , VE
    , VG
    , VI
    , VN
    , VU
    , WF
    , WS
    , YE
    , YT
    , ZA
    , ZM
    , ZW
    ]


codeForCountry : Country -> String
codeForCountry country =
    case country of
        AD ->
            "AD"

        AE ->
            "AE"

        AF ->
            "AF"

        AG ->
            "AG"

        AI ->
            "AI"

        AL ->
            "AL"

        AM ->
            "AM"

        AO ->
            "AO"

        AQ ->
            "AQ"

        AR ->
            "AR"

        AS ->
            "AS"

        AT ->
            "AT"

        AU ->
            "AU"

        AW ->
            "AW"

        AX ->
            "AX"

        AZ ->
            "AZ"

        BA ->
            "BA"

        BB ->
            "BB"

        BD ->
            "BD"

        BE ->
            "BE"

        BF ->
            "BF"

        BG ->
            "BG"

        BH ->
            "BH"

        BI ->
            "BI"

        BJ ->
            "BJ"

        BL ->
            "BL"

        BM ->
            "BM"

        BN ->
            "BN"

        BO ->
            "BO"

        BQ ->
            "BQ"

        BR ->
            "BR"

        BS ->
            "BS"

        BT ->
            "BT"

        BW ->
            "BW"

        BY ->
            "BY"

        BZ ->
            "BZ"

        CA ->
            "CA"

        CC ->
            "CC"

        CD ->
            "CD"

        CF ->
            "CF"

        CG ->
            "CG"

        CH ->
            "CH"

        CI ->
            "CI"

        CK ->
            "CK"

        CL ->
            "CL"

        CM ->
            "CM"

        CN ->
            "CN"

        CO ->
            "CO"

        CR ->
            "CR"

        CU ->
            "CU"

        CV ->
            "CV"

        CW ->
            "CW"

        CX ->
            "CX"

        CY ->
            "CY"

        CZ ->
            "CZ"

        DE ->
            "DE"

        DJ ->
            "DJ"

        DK ->
            "DK"

        DM ->
            "DM"

        DO ->
            "DO"

        DZ ->
            "DZ"

        EC ->
            "EC"

        EE ->
            "EE"

        EG ->
            "EG"

        EH ->
            "EH"

        ER ->
            "ER"

        ES ->
            "ES"

        ET ->
            "ET"

        FI ->
            "FI"

        FJ ->
            "FJ"

        FK ->
            "FK"

        FM ->
            "FM"

        FO ->
            "FO"

        FR ->
            "FR"

        GA ->
            "GA"

        GB ->
            "GB"

        GD ->
            "GD"

        GE ->
            "GE"

        GF ->
            "GF"

        GG ->
            "GG"

        GH ->
            "GH"

        GI ->
            "GI"

        GL ->
            "GL"

        GM ->
            "GM"

        GN ->
            "GN"

        GP ->
            "GP"

        GQ ->
            "GQ"

        GR ->
            "GR"

        GS ->
            "GS"

        GT ->
            "GT"

        GU ->
            "GU"

        GW ->
            "GW"

        GY ->
            "GY"

        HK ->
            "HK"

        HN ->
            "HN"

        HR ->
            "HR"

        HT ->
            "HT"

        HU ->
            "HU"

        ID ->
            "ID"

        IE ->
            "IE"

        IL ->
            "IL"

        IM ->
            "IM"

        IN ->
            "IN"

        IO ->
            "IO"

        IQ ->
            "IQ"

        IR ->
            "IR"

        IS ->
            "IS"

        IT ->
            "IT"

        JE ->
            "JE"

        JM ->
            "JM"

        JO ->
            "JO"

        JP ->
            "JP"

        KE ->
            "KE"

        KG ->
            "KG"

        KH ->
            "KH"

        KI ->
            "KI"

        KM ->
            "KM"

        KN ->
            "KN"

        KP ->
            "KP"

        KR ->
            "KR"

        KW ->
            "KW"

        KY ->
            "KY"

        KZ ->
            "KZ"

        LA ->
            "LA"

        LB ->
            "LB"

        LC ->
            "LC"

        LI ->
            "LI"

        LK ->
            "LK"

        LR ->
            "LR"

        LS ->
            "LS"

        LT ->
            "LT"

        LU ->
            "LU"

        LV ->
            "LV"

        LY ->
            "LY"

        MA ->
            "MA"

        MC ->
            "MC"

        MD ->
            "MD"

        ME ->
            "ME"

        MF ->
            "MF"

        MG ->
            "MG"

        MH ->
            "MH"

        MK ->
            "MK"

        ML ->
            "ML"

        MM ->
            "MM"

        MN ->
            "MN"

        MO ->
            "MO"

        MP ->
            "MP"

        MQ ->
            "MQ"

        MR ->
            "MR"

        MS ->
            "MS"

        MT ->
            "MT"

        MU ->
            "MU"

        MV ->
            "MV"

        MW ->
            "MW"

        MX ->
            "MX"

        MY ->
            "MY"

        MZ ->
            "MZ"

        NA ->
            "NA"

        NC ->
            "NC"

        NE ->
            "NE"

        NF ->
            "NF"

        NG ->
            "NG"

        NI ->
            "NI"

        NL ->
            "NL"

        NO ->
            "NO"

        NP ->
            "NP"

        NR ->
            "NR"

        NU ->
            "NU"

        NZ ->
            "NZ"

        OM ->
            "OM"

        PA ->
            "PA"

        PE ->
            "PE"

        PF ->
            "PF"

        PG ->
            "PG"

        PH ->
            "PH"

        PK ->
            "PK"

        PL ->
            "PL"

        PM ->
            "PM"

        PN ->
            "PN"

        PR ->
            "PR"

        PS ->
            "PS"

        PT ->
            "PT"

        PW ->
            "PW"

        PY ->
            "PY"

        QA ->
            "QA"

        RE ->
            "RE"

        RO ->
            "RO"

        RS ->
            "RS"

        RU ->
            "RU"

        RW ->
            "RW"

        SA ->
            "SA"

        SB ->
            "SB"

        SC ->
            "SC"

        SD ->
            "SD"

        SE ->
            "SE"

        SG ->
            "SG"

        SH ->
            "SH"

        SI ->
            "SI"

        SJ ->
            "SJ"

        SK ->
            "SK"

        SL ->
            "SL"

        SM ->
            "SM"

        SN ->
            "SN"

        SO ->
            "SO"

        SR ->
            "SR"

        SS ->
            "SS"

        ST ->
            "ST"

        SV ->
            "SV"

        SX ->
            "SX"

        SY ->
            "SY"

        SZ ->
            "SZ"

        TC ->
            "TC"

        TD ->
            "TD"

        TF ->
            "TF"

        TG ->
            "TG"

        TH ->
            "TH"

        TJ ->
            "TJ"

        TK ->
            "TK"

        TL ->
            "TL"

        TM ->
            "TM"

        TN ->
            "TN"

        TO ->
            "TO"

        TR ->
            "TR"

        TT ->
            "TT"

        TV ->
            "TV"

        TW ->
            "TW"

        TZ ->
            "TZ"

        UA ->
            "UA"

        UG ->
            "UG"

        UM ->
            "UM"

        US ->
            "US"

        UY ->
            "UY"

        UZ ->
            "UZ"

        VA ->
            "VA"

        VC ->
            "VC"

        VE ->
            "VE"

        VG ->
            "VG"

        VI ->
            "VI"

        VN ->
            "VN"

        VU ->
            "VU"

        WF ->
            "WF"

        WS ->
            "WS"

        YE ->
            "YE"

        YT ->
            "YT"

        ZA ->
            "ZA"

        ZM ->
            "ZM"

        ZW ->
            "ZW"

localizedCountryName : I18Next.Translations -> Country -> String
localizedCountryName translations country =
    case country of
        AD -> Translations.countryAd [ translations ]
        AE -> Translations.countryAe [ translations ]
        AF -> Translations.countryAf [ translations ]
        AG -> Translations.countryAg [ translations ]
        AI -> Translations.countryAi [ translations ]
        AL -> Translations.countryAl [ translations ]
        AM -> Translations.countryAm [ translations ]
        AO -> Translations.countryAo [ translations ]
        AQ -> Translations.countryAq [ translations ]
        AR -> Translations.countryAr [ translations ]
        AS -> Translations.countryAs [ translations ]
        AT -> Translations.countryAt [ translations ]
        AU -> Translations.countryAu [ translations ]
        AW -> Translations.countryAw [ translations ]
        AX -> Translations.countryAx [ translations ]
        AZ -> Translations.countryAz [ translations ]
        BA -> Translations.countryBa [ translations ]
        BB -> Translations.countryBb [ translations ]
        BD -> Translations.countryBd [ translations ]
        BE -> Translations.countryBe [ translations ]
        BF -> Translations.countryBf [ translations ]
        BG -> Translations.countryBg [ translations ]
        BH -> Translations.countryBh [ translations ]
        BI -> Translations.countryBi [ translations ]
        BJ -> Translations.countryBj [ translations ]
        BL -> Translations.countryBl [ translations ]
        BM -> Translations.countryBm [ translations ]
        BN -> Translations.countryBn [ translations ]
        BO -> Translations.countryBo [ translations ]
        BQ -> Translations.countryBq [ translations ]
        BR -> Translations.countryBr [ translations ]
        BS -> Translations.countryBs [ translations ]
        BT -> Translations.countryBt [ translations ]
        BW -> Translations.countryBw [ translations ]
        BY -> Translations.countryBy [ translations ]
        BZ -> Translations.countryBz [ translations ]
        CA -> Translations.countryCa [ translations ]
        CC -> Translations.countryCc [ translations ]
        CD -> Translations.countryCd [ translations ]
        CF -> Translations.countryCf [ translations ]
        CG -> Translations.countryCg [ translations ]
        CH -> Translations.countryCh [ translations ]
        CI -> Translations.countryCi [ translations ]
        CK -> Translations.countryCk [ translations ]
        CL -> Translations.countryCl [ translations ]
        CM -> Translations.countryCm [ translations ]
        CN -> Translations.countryCn [ translations ]
        CO -> Translations.countryCo [ translations ]
        CR -> Translations.countryCr [ translations ]
        CU -> Translations.countryCu [ translations ]
        CV -> Translations.countryCv [ translations ]
        CW -> Translations.countryCw [ translations ]
        CX -> Translations.countryCx [ translations ]
        CY -> Translations.countryCy [ translations ]
        CZ -> Translations.countryCz [ translations ]
        DE -> Translations.countryDe [ translations ]
        DJ -> Translations.countryDj [ translations ]
        DK -> Translations.countryDk [ translations ]
        DM -> Translations.countryDm [ translations ]
        DO -> Translations.countryDo [ translations ]
        DZ -> Translations.countryDz [ translations ]
        EC -> Translations.countryEc [ translations ]
        EE -> Translations.countryEe [ translations ]
        EG -> Translations.countryEg [ translations ]
        EH -> Translations.countryEh [ translations ]
        ER -> Translations.countryEr [ translations ]
        ES -> Translations.countryEs [ translations ]
        ET -> Translations.countryEt [ translations ]
        FI -> Translations.countryFi [ translations ]
        FJ -> Translations.countryFj [ translations ]
        FK -> Translations.countryFk [ translations ]
        FM -> Translations.countryFm [ translations ]
        FO -> Translations.countryFo [ translations ]
        FR -> Translations.countryFr [ translations ]
        GA -> Translations.countryGa [ translations ]
        GB -> Translations.countryGb [ translations ]
        GD -> Translations.countryGd [ translations ]
        GE -> Translations.countryGe [ translations ]
        GF -> Translations.countryGf [ translations ]
        GG -> Translations.countryGg [ translations ]
        GH -> Translations.countryGh [ translations ]
        GI -> Translations.countryGi [ translations ]
        GL -> Translations.countryGl [ translations ]
        GM -> Translations.countryGm [ translations ]
        GN -> Translations.countryGn [ translations ]
        GP -> Translations.countryGp [ translations ]
        GQ -> Translations.countryGq [ translations ]
        GR -> Translations.countryGr [ translations ]
        GS -> Translations.countryGs [ translations ]
        GT -> Translations.countryGt [ translations ]
        GU -> Translations.countryGu [ translations ]
        GW -> Translations.countryGw [ translations ]
        GY -> Translations.countryGy [ translations ]
        HK -> Translations.countryHk [ translations ]
        HN -> Translations.countryHn [ translations ]
        HR -> Translations.countryHr [ translations ]
        HT -> Translations.countryHt [ translations ]
        HU -> Translations.countryHu [ translations ]
        ID -> Translations.countryId [ translations ]
        IE -> Translations.countryIe [ translations ]
        IL -> Translations.countryIl [ translations ]
        IM -> Translations.countryIm [ translations ]
        IN -> Translations.countryIn [ translations ]
        IO -> Translations.countryIo [ translations ]
        IQ -> Translations.countryIq [ translations ]
        IR -> Translations.countryIr [ translations ]
        IS -> Translations.countryIs [ translations ]
        IT -> Translations.countryIt [ translations ]
        JE -> Translations.countryJe [ translations ]
        JM -> Translations.countryJm [ translations ]
        JO -> Translations.countryJo [ translations ]
        JP -> Translations.countryJp [ translations ]
        KE -> Translations.countryKe [ translations ]
        KG -> Translations.countryKg [ translations ]
        KH -> Translations.countryKh [ translations ]
        KI -> Translations.countryKi [ translations ]
        KM -> Translations.countryKm [ translations ]
        KN -> Translations.countryKn [ translations ]
        KP -> Translations.countryKp [ translations ]
        KR -> Translations.countryKr [ translations ]
        KW -> Translations.countryKw [ translations ]
        KY -> Translations.countryKy [ translations ]
        KZ -> Translations.countryKz [ translations ]
        LA -> Translations.countryLa [ translations ]
        LB -> Translations.countryLb [ translations ]
        LC -> Translations.countryLc [ translations ]
        LI -> Translations.countryLi [ translations ]
        LK -> Translations.countryLk [ translations ]
        LR -> Translations.countryLr [ translations ]
        LS -> Translations.countryLs [ translations ]
        LT -> Translations.countryLt [ translations ]
        LU -> Translations.countryLu [ translations ]
        LV -> Translations.countryLv [ translations ]
        LY -> Translations.countryLy [ translations ]
        MA -> Translations.countryMa [ translations ]
        MC -> Translations.countryMc [ translations ]
        MD -> Translations.countryMd [ translations ]
        ME -> Translations.countryMe [ translations ]
        MF -> Translations.countryMf [ translations ]
        MG -> Translations.countryMg [ translations ]
        MH -> Translations.countryMh [ translations ]
        MK -> Translations.countryMk [ translations ]
        ML -> Translations.countryMl [ translations ]
        MM -> Translations.countryMm [ translations ]
        MN -> Translations.countryMn [ translations ]
        MO -> Translations.countryMo [ translations ]
        MP -> Translations.countryMp [ translations ]
        MQ -> Translations.countryMq [ translations ]
        MR -> Translations.countryMr [ translations ]
        MS -> Translations.countryMs [ translations ]
        MT -> Translations.countryMt [ translations ]
        MU -> Translations.countryMu [ translations ]
        MV -> Translations.countryMv [ translations ]
        MW -> Translations.countryMw [ translations ]
        MX -> Translations.countryMx [ translations ]
        MY -> Translations.countryMy [ translations ]
        MZ -> Translations.countryMz [ translations ]
        NA -> Translations.countryNa [ translations ]
        NC -> Translations.countryNc [ translations ]
        NE -> Translations.countryNe [ translations ]
        NF -> Translations.countryNf [ translations ]
        NG -> Translations.countryNg [ translations ]
        NI -> Translations.countryNi [ translations ]
        NL -> Translations.countryNl [ translations ]
        NO -> Translations.countryNo [ translations ]
        NP -> Translations.countryNp [ translations ]
        NR -> Translations.countryNr [ translations ]
        NU -> Translations.countryNu [ translations ]
        NZ -> Translations.countryNz [ translations ]
        OM -> Translations.countryOm [ translations ]
        PA -> Translations.countryPa [ translations ]
        PE -> Translations.countryPe [ translations ]
        PF -> Translations.countryPf [ translations ]
        PG -> Translations.countryPg [ translations ]
        PH -> Translations.countryPh [ translations ]
        PK -> Translations.countryPk [ translations ]
        PL -> Translations.countryPl [ translations ]
        PM -> Translations.countryPm [ translations ]
        PN -> Translations.countryPn [ translations ]
        PR -> Translations.countryPr [ translations ]
        PS -> Translations.countryPs [ translations ]
        PT -> Translations.countryPt [ translations ]
        PW -> Translations.countryPw [ translations ]
        PY -> Translations.countryPy [ translations ]
        QA -> Translations.countryQa [ translations ]
        RE -> Translations.countryRe [ translations ]
        RO -> Translations.countryRo [ translations ]
        RS -> Translations.countryRs [ translations ]
        RU -> Translations.countryRu [ translations ]
        RW -> Translations.countryRw [ translations ]
        SA -> Translations.countrySa [ translations ]
        SB -> Translations.countrySb [ translations ]
        SC -> Translations.countrySc [ translations ]
        SD -> Translations.countrySd [ translations ]
        SE -> Translations.countrySe [ translations ]
        SG -> Translations.countrySg [ translations ]
        SH -> Translations.countrySh [ translations ]
        SI -> Translations.countrySi [ translations ]
        SJ -> Translations.countrySj [ translations ]
        SK -> Translations.countrySk [ translations ]
        SL -> Translations.countrySl [ translations ]
        SM -> Translations.countrySm [ translations ]
        SN -> Translations.countrySn [ translations ]
        SO -> Translations.countrySo [ translations ]
        SR -> Translations.countrySr [ translations ]
        SS -> Translations.countrySs [ translations ]
        ST -> Translations.countrySt [ translations ]
        SV -> Translations.countrySv [ translations ]
        SX -> Translations.countrySx [ translations ]
        SY -> Translations.countrySy [ translations ]
        SZ -> Translations.countrySz [ translations ]
        TC -> Translations.countryTc [ translations ]
        TD -> Translations.countryTd [ translations ]
        TF -> Translations.countryTf [ translations ]
        TG -> Translations.countryTg [ translations ]
        TH -> Translations.countryTh [ translations ]
        TJ -> Translations.countryTj [ translations ]
        TK -> Translations.countryTk [ translations ]
        TL -> Translations.countryTl [ translations ]
        TM -> Translations.countryTm [ translations ]
        TN -> Translations.countryTn [ translations ]
        TO -> Translations.countryTo [ translations ]
        TR -> Translations.countryTr [ translations ]
        TT -> Translations.countryTt [ translations ]
        TV -> Translations.countryTv [ translations ]
        TW -> Translations.countryTw [ translations ]
        TZ -> Translations.countryTz [ translations ]
        UA -> Translations.countryUa [ translations ]
        UG -> Translations.countryUg [ translations ]
        UM -> Translations.countryUm [ translations ]
        US -> Translations.countryUs [ translations ]
        UY -> Translations.countryUy [ translations ]
        UZ -> Translations.countryUz [ translations ]
        VA -> Translations.countryVa [ translations ]
        VC -> Translations.countryVc [ translations ]
        VE -> Translations.countryVe [ translations ]
        VG -> Translations.countryVg [ translations ]
        VI -> Translations.countryVi [ translations ]
        VN -> Translations.countryVn [ translations ]
        VU -> Translations.countryVu [ translations ]
        WF -> Translations.countryWf [ translations ]
        WS -> Translations.countryWs [ translations ]
        YE -> Translations.countryYe [ translations ]
        YT -> Translations.countryYt [ translations ]
        ZA -> Translations.countryZa [ translations ]
        ZM -> Translations.countryZm [ translations ]
        ZW -> Translations.countryZw [ translations ]

timezonesForCountry : Country -> List String
timezonesForCountry country =
    case country of
        AD ->
            [ "Europe/Andorra" ]

        AE ->
            [ "Asia/Dubai" ]

        AF ->
            [ "Asia/Kabul" ]

        AG ->
            [ "America/Antigua" ]

        AI ->
            [ "America/Anguilla" ]

        AL ->
            [ "Europe/Tirane" ]

        AM ->
            [ "Asia/Yerevan" ]

        AO ->
            [ "Africa/Luanda" ]

        AQ ->
            [ "Antarctica/Palmer"
            , "Antarctica/Casey"
            , "Antarctica/DumontDUrville"
            , "Antarctica/Rothera"
            , "Antarctica/Mawson"
            , "Antarctica/Davis"
            , "Antarctica/Syowa"
            , "Antarctica/Troll"
            , "Antarctica/McMurdo"
            , "Antarctica/Vostok"
            ]

        AR ->
            [ "America/Argentina/Jujuy"
            , "America/Argentina/Salta"
            , "America/Argentina/Tucuman"
            , "America/Argentina/Catamarca"
            , "America/Argentina/La_Rioja"
            , "America/Argentina/Cordoba"
            , "America/Argentina/San_Juan"
            , "America/Argentina/Mendoza"
            , "America/Argentina/San_Luis"
            , "America/Argentina/Buenos_Aires"
            , "America/Argentina/Rio_Gallegos"
            , "America/Argentina/Ushuaia"
            ]

        AS ->
            [ "Pacific/Pago_Pago" ]

        AT ->
            [ "Europe/Vienna" ]

        AU ->
            [ "Australia/Darwin"
            , "Australia/Lindeman"
            , "Australia/Brisbane"
            , "Australia/Lord_Howe"
            , "Australia/Eucla"
            , "Australia/Perth"
            , "Australia/Broken_Hill"
            , "Australia/Sydney"
            , "Australia/Adelaide"
            , "Australia/Melbourne"
            , "Australia/Currie"
            , "Australia/Hobart"
            , "Antarctica/Macquarie"
            ]

        AW ->
            [ "America/Aruba" ]

        AX ->
            [ "Europe/Mariehamn" ]

        AZ ->
            [ "Asia/Baku" ]

        BA ->
            [ "Europe/Sarajevo" ]

        BB ->
            [ "America/Barbados" ]

        BD ->
            [ "Asia/Dhaka" ]

        BE ->
            [ "Europe/Brussels" ]

        BF ->
            [ "Africa/Ouagadougou" ]

        BG ->
            [ "Europe/Sofia" ]

        BH ->
            [ "Asia/Bahrain" ]

        BI ->
            [ "Africa/Bujumbura" ]

        BJ ->
            [ "Africa/Porto-Novo" ]

        BL ->
            [ "America/St_Barthelemy" ]

        BM ->
            [ "Atlantic/Bermuda" ]

        BN ->
            [ "Asia/Brunei" ]

        BO ->
            [ "America/La_Paz" ]

        BQ ->
            [ "America/Kralendijk" ]

        BR ->
            [ "America/Boa_Vista"
            , "America/Belem"
            , "America/Santarem"
            , "America/Manaus"
            , "America/Fortaleza"
            , "America/Noronha"
            , "America/Eirunepe"
            , "America/Araguaina"
            , "America/Recife"
            , "America/Porto_Velho"
            , "America/Maceio"
            , "America/Rio_Branco"
            , "America/Bahia"
            , "America/Cuiaba"
            , "America/Campo_Grande"
            , "America/Sao_Paulo"
            ]

        BS ->
            [ "America/Nassau" ]

        BT ->
            [ "Asia/Thimphu" ]

        BW ->
            [ "Africa/Gaborone" ]

        BY ->
            [ "Europe/Minsk" ]

        BZ ->
            [ "America/Belize" ]

        CA ->
            [ "America/Toronto"
            , "America/Halifax"
            , "America/Moncton"
            , "America/Glace_Bay"
            , "America/St_Johns"
            , "America/Thunder_Bay"
            , "America/Rainy_River"
            , "America/Atikokan"
            , "America/Nipigon"
            , "America/Creston"
            , "America/Vancouver"
            , "America/Winnipeg"
            , "America/Swift_Current"
            , "America/Regina"
            , "America/Blanc-Sablon"
            , "America/Goose_Bay"
            , "America/Edmonton"
            , "America/Fort_Nelson"
            , "America/Dawson_Creek"
            , "America/Whitehorse"
            , "America/Yellowknife"
            , "America/Rankin_Inlet"
            , "America/Iqaluit"
            , "America/Dawson"
            , "America/Pangnirtung"
            , "America/Inuvik"
            , "America/Cambridge_Bay"
            , "America/Resolute"
            ]

        CC ->
            [ "Indian/Cocos" ]

        CD ->
            [ "Africa/Kinshasa", "Africa/Lubumbashi" ]

        CF ->
            [ "Africa/Bangui" ]

        CG ->
            [ "Africa/Brazzaville" ]

        CH ->
            [ "Europe/Zurich" ]

        CI ->
            [ "Africa/Abidjan" ]

        CK ->
            [ "Pacific/Rarotonga" ]

        CL ->
            [ "Pacific/Easter"
            , "America/Santiago"
            , "America/Punta_Arenas"
            ]

        CM ->
            [ "Africa/Douala" ]

        CN ->
            [ "Asia/Shanghai"
            , "Asia/Urumqi"
            ]

        CO ->
            [ "America/Bogota" ]

        CR ->
            [ "America/Costa_Rica" ]

        CU ->
            [ "America/Havana" ]

        CV ->
            [ "Atlantic/Cape_Verde" ]

        CW ->
            [ "America/Curacao" ]

        CX ->
            [ "Indian/Christmas" ]

        CY ->
            [ "Asia/Famagusta"
            , "Asia/Nicosia"
            ]

        CZ ->
            [ "Europe/Prague" ]

        DE ->
            [ "Europe/Busingen"
            , "Europe/Berlin"
            ]

        DJ ->
            [ "Africa/Djibouti" ]

        DK ->
            [ "Europe/Copenhagen" ]

        DM ->
            [ "America/Dominica" ]

        DO ->
            [ "America/Santo_Domingo" ]

        DZ ->
            [ "Africa/Algiers" ]

        EC ->
            [ "Pacific/Galapagos"
            , "America/Guayaquil"
            ]

        EE ->
            [ "Europe/Tallinn" ]

        EG ->
            [ "Africa/Cairo" ]

        EH ->
            [ "Africa/El_Aaiun" ]

        ER ->
            [ "Africa/Asmara" ]

        ES ->
            [ "Atlantic/Canary"
            , "Africa/Ceuta"
            , "Europe/Madrid"
            ]

        ET ->
            [ "Africa/Addis_Ababa" ]

        FI ->
            [ "Europe/Helsinki" ]

        FJ ->
            [ "Pacific/Fiji" ]

        FK ->
            [ "Atlantic/Stanley" ]

        FM ->
            [ "Pacific/Kosrae"
            , "Pacific/Pohnpei"
            , "Pacific/Chuuk"
            ]

        FO ->
            [ "Atlantic/Faroe" ]

        FR ->
            [ "Europe/Paris" ]

        GA ->
            [ "Africa/Libreville" ]

        GB ->
            [ "Europe/London" ]

        GD ->
            [ "America/Grenada" ]

        GE ->
            [ "Asia/Tbilisi" ]

        GF ->
            [ "America/Cayenne" ]

        GG ->
            [ "Europe/Guernsey" ]

        GH ->
            [ "Africa/Accra" ]

        GI ->
            [ "Europe/Gibraltar" ]

        GL ->
            [ "America/Godthab"
            , "America/Scoresbysund"
            , "America/Thule"
            , "America/Danmarkshavn"
            ]

        GM ->
            [ "Africa/Banjul" ]

        GN ->
            [ "Africa/Conakry" ]

        GP ->
            [ "America/Guadeloupe" ]

        GQ ->
            [ "Africa/Malabo" ]

        GR ->
            [ "Europe/Athens" ]

        GS ->
            [ "Atlantic/South_Georgia" ]

        GT ->
            [ "America/Guatemala" ]

        GU ->
            [ "Pacific/Guam" ]

        GW ->
            [ "Africa/Bissau" ]

        GY ->
            [ "America/Guyana" ]

        HK ->
            [ "Asia/Hong_Kong" ]

        HN ->
            [ "America/Tegucigalpa" ]

        HR ->
            [ "Europe/Zagreb" ]

        HT ->
            [ "America/Port-au-Prince" ]

        HU ->
            [ "Europe/Budapest" ]

        ID ->
            [ "Asia/Pontianak"
            , "Asia/Jayapura"
            , "Asia/Makassar"
            , "Asia/Jakarta"
            ]

        IE ->
            [ "Europe/Dublin" ]

        IL ->
            [ "Asia/Jerusalem" ]

        IM ->
            [ "Europe/Isle_of_Man" ]

        IN ->
            [ "Asia/Kolkata" ]

        IO ->
            [ "Indian/Chagos" ]

        IQ ->
            [ "Asia/Baghdad" ]

        IR ->
            [ "Asia/Tehran" ]

        IS ->
            [ "Atlantic/Reykjavik" ]

        IT ->
            [ "Europe/Rome" ]

        JE ->
            [ "Europe/Jersey" ]

        JM ->
            [ "America/Jamaica" ]

        JO ->
            [ "Asia/Amman" ]

        JP ->
            [ "Asia/Tokyo" ]

        KE ->
            [ "Africa/Nairobi" ]

        KG ->
            [ "Asia/Bishkek" ]

        KH ->
            [ "Asia/Phnom_Penh" ]

        KI ->
            [ "Pacific/Tarawa"
            , "Pacific/Kiritimati"
            , "Pacific/Enderbury"
            ]

        KM ->
            [ "Indian/Comoro" ]

        KN ->
            [ "America/St_Kitts" ]

        KP ->
            [ "Asia/Pyongyang" ]

        KR ->
            [ "Asia/Seoul" ]

        KW ->
            [ "Asia/Kuwait" ]

        KY ->
            [ "America/Cayman" ]

        KZ ->
            [ "Asia/Almaty"
            , "Asia/Aqtau"
            , "Asia/Qyzylorda"
            , "Asia/Atyrau"
            , "Asia/Aqtobe"
            , "Asia/Oral"
            ]

        LA ->
            [ "Asia/Vientiane" ]

        LB ->
            [ "Asia/Beirut" ]

        LC ->
            [ "America/St_Lucia" ]

        LI ->
            [ "Europe/Vaduz" ]

        LK ->
            [ "Asia/Colombo" ]

        LR ->
            [ "Africa/Monrovia" ]

        LS ->
            [ "Africa/Maseru" ]

        LT ->
            [ "Europe/Vilnius" ]

        LU ->
            [ "Europe/Luxembourg" ]

        LV ->
            [ "Europe/Riga" ]

        LY ->
            [ "Africa/Tripoli" ]

        MA ->
            [ "Africa/Casablanca" ]

        MC ->
            [ "Europe/Monaco" ]

        MD ->
            [ "Europe/Chisinau" ]

        ME ->
            [ "Europe/Podgorica" ]

        MF ->
            [ "America/Marigot" ]

        MG ->
            [ "Indian/Antananarivo" ]

        MH ->
            [ "Pacific/Majuro"
            , "Pacific/Kwajalein"
            ]

        MK ->
            [ "Europe/Skopje" ]

        ML ->
            [ "Africa/Bamako" ]

        MM ->
            [ "Asia/Rangoon"
            , "Asia/Yangon"
            ]

        MN ->
            [ "Asia/Ulaanbaatar"
            , "Asia/Hovd"
            , "Asia/Choibalsan"
            ]

        MO ->
            [ "Asia/Macau" ]

        MP ->
            [ "Pacific/Saipan" ]

        MQ ->
            [ "America/Martinique" ]

        MR ->
            [ "Africa/Nouakchott" ]

        MS ->
            [ "America/Montserrat" ]

        MT ->
            [ "Europe/Malta" ]

        MU ->
            [ "Indian/Mauritius" ]

        MV ->
            [ "Indian/Maldives" ]

        MW ->
            [ "Africa/Blantyre" ]

        MX ->
            [ "America/Mexico_City"
            , "America/Bahia_Banderas"
            , "America/Merida"
            , "America/Cancun"
            , "America/Mazatlan"
            , "America/Monterrey"
            , "America/Matamoros"
            , "America/Chihuahua"
            , "America/Hermosillo"
            , "America/Ojinaga"
            , "America/Tijuana"
            ]

        MY ->
            [ "Asia/Kuching"
            , "Asia/Kuala_Lumpur"
            ]

        MZ ->
            [ "Africa/Maputo" ]

        NA ->
            [ "Africa/Windhoek" ]

        NC ->
            [ "Pacific/Noumea" ]

        NE ->
            [ "Africa/Niamey" ]

        NF ->
            [ "Pacific/Norfolk" ]

        NG ->
            [ "Africa/Lagos" ]

        NI ->
            [ "America/Managua" ]

        NL ->
            [ "Europe/Amsterdam" ]

        NO ->
            [ "Europe/Oslo" ]

        NP ->
            [ "Asia/Kathmandu" ]

        NR ->
            [ "Pacific/Nauru" ]

        NU ->
            [ "Pacific/Niue" ]

        NZ ->
            [ "Pacific/Auckland"
            , "Pacific/Chatham"
            ]

        OM ->
            [ "Asia/Muscat" ]

        PA ->
            [ "America/Panama" ]

        PE ->
            [ "America/Lima" ]

        PF ->
            [ "Pacific/Marquesas"
            , "Pacific/Tahiti"
            , "Pacific/Gambier"
            ]

        PG ->
            [ "Pacific/Bougainville"
            , "Pacific/Port_Moresby"
            ]

        PH ->
            [ "Asia/Manila" ]

        PK ->
            [ "Asia/Karachi" ]

        PL ->
            [ "Europe/Warsaw" ]

        PM ->
            [ "America/Miquelon" ]

        PN ->
            [ "Pacific/Pitcairn" ]

        PR ->
            [ "America/Puerto_Rico" ]

        PS ->
            [ "Asia/Gaza"
            , "Asia/Hebron"
            ]

        PT ->
            [ "Atlantic/Madeira"
            , "Atlantic/Azores"
            , "Europe/Lisbon"
            ]

        PW ->
            [ "Pacific/Palau" ]

        PY ->
            [ "America/Asuncion" ]

        QA ->
            [ "Asia/Qatar" ]

        RE ->
            [ "Indian/Reunion" ]

        RO ->
            [ "Europe/Bucharest" ]

        RS ->
            [ "Europe/Belgrade" ]

        RU ->
            [ "Asia/Vladivostok"
            , "Europe/Astrakhan"
            , "Asia/Sakhalin"
            , "Europe/Volgograd"
            , "Europe/Saratov"
            , "Asia/Chita"
            , "Asia/Irkutsk"
            , "Asia/Kamchatka"
            , "Europe/Samara"
            , "Asia/Barnaul"
            , "Asia/Novokuznetsk"
            , "Europe/Ulyanovsk"
            , "Europe/Kaliningrad"
            , "Asia/Omsk"
            , "Asia/Novosibirsk"
            , "Europe/Moscow"
            , "Asia/Krasnoyarsk"
            , "Asia/Tomsk"
            , "Asia/Yekaterinburg"
            , "Europe/Kirov"
            , "Asia/Magadan"
            , "Asia/Yakutsk"
            , "Asia/Khandyga"
            , "Asia/Ust-Nera"
            , "Asia/Anadyr"
            , "Asia/Srednekolymsk"
            ]

        RW ->
            [ "Africa/Kigali" ]

        SA ->
            [ "Asia/Riyadh" ]

        SB ->
            [ "Pacific/Guadalcanal" ]

        SC ->
            [ "Indian/Mahe" ]

        SD ->
            [ "Africa/Khartoum" ]

        SE ->
            [ "Europe/Stockholm" ]

        SG ->
            [ "Asia/Singapore" ]

        SH ->
            [ "Atlantic/St_Helena" ]

        SI ->
            [ "Europe/Ljubljana" ]

        SJ ->
            [ "Arctic/Longyearbyen" ]

        SK ->
            [ "Europe/Bratislava" ]

        SL ->
            [ "Africa/Freetown" ]

        SM ->
            [ "Europe/San_Marino" ]

        SN ->
            [ "Africa/Dakar" ]

        SO ->
            [ "Africa/Mogadishu" ]

        SR ->
            [ "America/Paramaribo" ]

        SS ->
            [ "Africa/Juba" ]

        ST ->
            [ "Africa/Sao_Tome" ]

        SV ->
            [ "America/El_Salvador" ]

        SX ->
            [ "America/Lower_Princes" ]

        SY ->
            [ "Asia/Damascus" ]

        SZ ->
            [ "Africa/Mbabane" ]

        TC ->
            [ "America/Grand_Turk" ]

        TD ->
            [ "Africa/Ndjamena" ]

        TF ->
            [ "Indian/Kerguelen" ]

        TG ->
            [ "Africa/Lome" ]

        TH ->
            [ "Asia/Bangkok" ]

        TJ ->
            [ "Asia/Dushanbe" ]

        TK ->
            [ "Pacific/Fakaofo" ]

        TL ->
            [ "Asia/Dili" ]

        TM ->
            [ "Asia/Ashgabat" ]

        TN ->
            [ "Africa/Tunis" ]

        TO ->
            [ "Pacific/Tongatapu" ]

        TR ->
            [ "Europe/Istanbul" ]

        TT ->
            [ "America/Port_of_Spain" ]

        TV ->
            [ "Pacific/Funafuti" ]

        TW ->
            [ "Asia/Taipei" ]

        TZ ->
            [ "Africa/Dar_es_Salaam" ]

        UA ->
            [ "Europe/Simferopol"
            , "Europe/Zaporozhye"
            , "Europe/Uzhgorod"
            , "Europe/Kiev"
            ]

        UG ->
            [ "Africa/Kampala" ]

        UM ->
            [ "Pacific/Wake"
            , "Pacific/Midway"
            ]

        US ->
            [ "Pacific/Honolulu"
            , "America/Phoenix"
            , "America/Los_Angeles"
            , "America/Kentucky/Monticello"
            , "America/Indiana/Tell_City"
            , "America/Kentucky/Louisville"
            , "America/Indiana/Marengo"
            , "America/Indiana/Petersburg"
            , "America/Indiana/Vincennes"
            , "America/Indiana/Vevay"
            , "America/Denver"
            , "America/Indiana/Indianapolis"
            , "America/New_York"
            , "America/Indiana/Winamac"
            , "America/Indiana/Knox"
            , "America/Chicago"
            , "America/Detroit"
            , "America/Boise"
            , "America/Menominee"
            , "America/North_Dakota/New_Salem"
            , "America/North_Dakota/Center"
            , "America/North_Dakota/Beulah"
            , "America/Adak"
            , "America/Metlakatla"
            , "America/Sitka"
            , "America/Juneau"
            , "America/Yakutat"
            , "America/Anchorage"
            , "America/Nome"
            ]

        UY ->
            [ "America/Montevideo" ]

        UZ ->
            [ "Asia/Samarkand"
            , "Asia/Tashkent"
            ]

        VA ->
            [ "Europe/Vatican" ]

        VC ->
            [ "America/St_Vincent" ]

        VE ->
            [ "America/Caracas" ]

        VG ->
            [ "America/Tortola" ]

        VI ->
            [ "America/St_Thomas" ]

        VN ->
            [ "Asia/Ho_Chi_Minh" ]

        VU ->
            [ "Pacific/Efate" ]

        WF ->
            [ "Pacific/Wallis" ]

        WS ->
            [ "Pacific/Apia" ]

        YE ->
            [ "Asia/Aden" ]

        YT ->
            [ "Indian/Mayotte" ]

        ZA ->
            [ "Africa/Johannesburg" ]

        ZM ->
            [ "Africa/Lusaka" ]

        ZW ->
            [ "Africa/Harare" ]


codeToCountryDict : Dict String Country
codeToCountryDict =
    List.foldl
        (\country dict ->
            Dict.insert (codeForCountry country) country dict
        )
        Dict.empty
        countries


timezoneToCountryDict : Dict String Country
timezoneToCountryDict =
    List.foldl
        (\country dict ->
            List.foldl
                (\timezone acc ->
                    Dict.insert timezone country acc
                )
                dict
                (timezonesForCountry country)
        )
        Dict.empty
        countries
