module Locale exposing (..)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import I18Next exposing (Translations)
import Languages.English
import Languages.German
import Material.Icons exposing (language)
import Numeral
import Time exposing (Month(..), Weekday(..))
import Translations.Locale as Translations


type Language
    = English String
    | Dutch
    | Finnish
    | French
    | German String
    | Greek
    | Italian
    | Norwegian
    | Portuguese
    | Spanish
    | Swedish
    | Russian


defaultLanguages : List Language
defaultLanguages =
    [ English "US", French, German "DE", Italian, Portuguese, Spanish, Swedish ]

languageFromLocale : String -> Language
languageFromLocale locale =
    case String.left 2 locale of
        -- TODO: Refactor design to distinguish between locale and language properly`
        "en" ->
            English "US"

        "fr" ->
            French

        "de" ->
            German "DE"

        "it" ->
            Italian

        "pt" ->
            Portuguese

        "es" ->
            Spanish

        "sv" ->
            Swedish

        _ ->
            English "GB"


languageToString : Translations -> Language -> String
languageToString translations language =
    case language of
        English "US" ->
            Translations.englishText [ translations ]

        French ->
            Translations.frenchText [ translations ]

        German _ ->
            Translations.germanText [ translations ]

        Italian ->
            Translations.italianText [ translations ]

        Portuguese ->
            Translations.portugueseText [ translations ]

        Spanish ->
            Translations.spanishText [ translations ]

        Swedish ->
            Translations.swedishText [ translations ]

        _ ->
            "Not supported yet"


languageFromISOCode : String -> Maybe Language
languageFromISOCode code =
    case code of
        "en" ->
            Just (English "US")

        "fr" ->
            Just French

        "de" ->
            Just (German "DE")

        "it" ->
            Just Italian

        "pt" ->
            Just Portuguese

        "es" ->
            Just Spanish

        "sv" ->
            Just Swedish

        _ ->
            Nothing


languageToISOCode : Language -> String
languageToISOCode language =
    case language of
        English "US" ->
            "en"

        French ->
            "fr"

        German _ ->
            "de"

        Italian ->
            "it"

        Portuguese ->
            "pt"

        Spanish ->
            "es"

        Swedish ->
            "sv"

        _ ->
            "en"


dateFormatFromLanguage : Language -> ( DateFormat.Language.Language, List DateFormat.Token, List DateFormat.Token )
dateFormatFromLanguage language =
    case language of
        German _ ->
            ( dateFormatGerman, tokensGermanWithYear, tokensGermanWithoutYear )

        _ ->
            ( DateFormat.Language.english, tokensEnglishWithYear, tokensEnglishWithoutYear )


relativeTimeOptionsFromLanguage : Language -> RelativeTimeOptions
relativeTimeOptionsFromLanguage language =
    case language of
        German _ ->
            relativeTimeOptionsGerman

        _ ->
            DateFormat.Relative.defaultRelativeOptions


numberFormatFromLanguage : Language -> (String -> Float -> String)
numberFormatFromLanguage language =
    case language of
        German _ ->
            Numeral.formatWithLanguage Languages.German.lang

        _ ->
            Numeral.formatWithLanguage Languages.English.lang


dateFormatGerman : DateFormat.Language.Language
dateFormatGerman =
    { toMonthName = toMonthNameGerman
    , toMonthAbbreviation = toMonthAbbreviationGerman
    , toWeekdayName = toWeekdayNameGerman
    , toWeekdayAbbreviation = toWeekdayAbbreviationGerman
    , toAmPm = toAmPmGerman
    , toOrdinalSuffix = toOrdinalSuffixGerman
    }


toMonthNameGerman : Month -> String
toMonthNameGerman month =
    case month of
        Jan ->
            "Januar"

        Feb ->
            "Februar"

        Mar ->
            "März"

        Apr ->
            "April"

        May ->
            "Mai"

        Jun ->
            "Juni"

        Jul ->
            "Juli"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "Oktober"

        Nov ->
            "November"

        Dec ->
            "December"


toMonthAbbreviationGerman : Month -> String
toMonthAbbreviationGerman month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mär"

        Apr ->
            "Apr"

        May ->
            "Mai"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Okt"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


toWeekdayNameGerman : Weekday -> String
toWeekdayNameGerman weekday =
    case weekday of
        Mon ->
            "Montag"

        Tue ->
            "Dienstag"

        Wed ->
            "Mittwoch"

        Thu ->
            "Donnerstag"

        Fri ->
            "Freitag"

        Sat ->
            "Samstag"

        Sun ->
            "Sonntag"


toWeekdayAbbreviationGerman : Time.Weekday -> String
toWeekdayAbbreviationGerman weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Die"

        Wed ->
            "Mit"

        Thu ->
            "Don"

        Fri ->
            "Fre"

        Sat ->
            "Sam"

        Sun ->
            "Son"


toAmPmGerman : Int -> String
toAmPmGerman amPm =
    case amPm of
        0 ->
            "am"

        _ ->
            "pm"


toOrdinalSuffixGerman : Int -> String
toOrdinalSuffixGerman ordinal =
    case ordinal of
        0 ->
            "0"

        _ ->
            "1"


relativeTimeOptionsGerman : RelativeTimeOptions
relativeTimeOptionsGerman =
    { someSecondsAgo = someSecondsAgoGerman
    , someMinutesAgo = someMinutesAgoGerman
    , someHoursAgo = someHoursAgoGerman
    , someDaysAgo = someDaysAgoGerman
    , someMonthsAgo = someMonthsAgoGerman
    , someYearsAgo = someYearsAgoGerman
    , rightNow = "Gerade eben"
    , inSomeSeconds = inSomeSecondsGerman
    , inSomeMinutes = inSomeMinutesGerman
    , inSomeHours = inSomeHoursGerman
    , inSomeDays = inSomeDaysGerman
    , inSomeMonths = inSomeMonthsGerman
    , inSomeYears = inSomeYearsGerman
    }


someSecondsAgoGerman : Int -> String
someSecondsAgoGerman seconds =
    if seconds /= 1 then
        "Vor " ++ String.fromInt seconds ++ " Sekunden"

    else
        "Vor " ++ String.fromInt seconds ++ " Sekunde"


someMinutesAgoGerman : Int -> String
someMinutesAgoGerman minutes =
    if minutes /= 1 then
        "Vor " ++ String.fromInt minutes ++ " Minuten"

    else
        "Vor " ++ String.fromInt minutes ++ " Minute"


someHoursAgoGerman : Int -> String
someHoursAgoGerman hours =
    if hours /= 1 then
        "Vor " ++ String.fromInt hours ++ " Stunden"

    else
        "Vor " ++ String.fromInt hours ++ " Stunde"


someDaysAgoGerman : Int -> String
someDaysAgoGerman days =
    if days /= 1 then
        "Vor " ++ String.fromInt days ++ " Tagen"

    else
        "Vor " ++ String.fromInt days ++ " Tag"


someMonthsAgoGerman : Int -> String
someMonthsAgoGerman months =
    if months /= 1 then
        "Vor " ++ String.fromInt months ++ " Monaten"

    else
        "Vor " ++ String.fromInt months ++ " Monat"


someYearsAgoGerman : Int -> String
someYearsAgoGerman years =
    if years /= 1 then
        "Vor " ++ String.fromInt years ++ " Jahren"

    else
        "Vor " ++ String.fromInt years ++ " Jahr"


inSomeSecondsGerman : Int -> String
inSomeSecondsGerman seconds =
    if seconds /= 1 then
        "In " ++ String.fromInt seconds ++ " Sekunden"

    else
        "In " ++ String.fromInt seconds ++ " Sekunde"


inSomeMinutesGerman : Int -> String
inSomeMinutesGerman minutes =
    if minutes /= 1 then
        "In " ++ String.fromInt minutes ++ " Minuten"

    else
        "In " ++ String.fromInt minutes ++ " Minute"


inSomeHoursGerman : Int -> String
inSomeHoursGerman hours =
    if hours /= 1 then
        "In " ++ String.fromInt hours ++ " Stunden"

    else
        "In " ++ String.fromInt hours ++ " Stunde"


inSomeDaysGerman : Int -> String
inSomeDaysGerman days =
    if days /= 1 then
        "In " ++ String.fromInt days ++ " Tagen"

    else
        "In " ++ String.fromInt days ++ " Tag"


inSomeMonthsGerman : Int -> String
inSomeMonthsGerman months =
    if months /= 1 then
        "In " ++ String.fromInt months ++ " Monaten"

    else
        "In " ++ String.fromInt months ++ " Monat"


inSomeYearsGerman : Int -> String
inSomeYearsGerman years =
    if years /= 1 then
        "Vor " ++ String.fromInt years ++ " Jahren"

    else
        "Vor " ++ String.fromInt years ++ " Jahr"


tokensGermanWithYear : List DateFormat.Token
tokensGermanWithYear =
    tokensGermanWithoutYear
        ++ [ DateFormat.text " "
           , DateFormat.yearNumber
           ]


tokensGermanWithoutYear : List DateFormat.Token
tokensGermanWithoutYear =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text ". "
    , DateFormat.monthNameFull
    ]


tokensEnglishWithYear : List DateFormat.Token
tokensEnglishWithYear =
    tokensEnglishWithoutYear
        ++ [ DateFormat.text ", "
           , DateFormat.yearNumber
           ]


tokensEnglishWithoutYear : List DateFormat.Token
tokensEnglishWithoutYear =
    [ DateFormat.monthNameFull
    , DateFormat.text " "
    , DateFormat.dayOfMonthSuffix
    ]
