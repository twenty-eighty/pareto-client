module Locale.Italian exposing (dateFormatItalian, relativeTimeOptionsItalian, tokensItalianWithYear, tokensItalianWithoutYear)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import Time exposing (Month(..), Weekday(..))


dateFormatItalian : DateFormat.Language.Language
dateFormatItalian =
    { toMonthName = toMonthNameItalian
    , toMonthAbbreviation = toMonthAbbreviationItalian
    , toWeekdayName = toWeekdayNameItalian
    , toWeekdayAbbreviation = toWeekdayAbbreviationItalian
    , toAmPm = toAmPmItalian
    , toOrdinalSuffix = toOrdinalSuffixItalian
    }


toMonthNameItalian : Month -> String
toMonthNameItalian month =
    case month of
        Jan -> "gennaio"
        Feb -> "febbraio"
        Mar -> "marzo"
        Apr -> "aprile"
        May -> "maggio"
        Jun -> "giugno"
        Jul -> "luglio"
        Aug -> "agosto"
        Sep -> "settembre"
        Oct -> "ottobre"
        Nov -> "novembre"
        Dec -> "dicembre"


toMonthAbbreviationItalian : Month -> String
toMonthAbbreviationItalian month =
    case month of
        Jan -> "gen"
        Feb -> "feb"
        Mar -> "mar"
        Apr -> "apr"
        May -> "mag"
        Jun -> "giu"
        Jul -> "lug"
        Aug -> "ago"
        Sep -> "set"
        Oct -> "ott"
        Nov -> "nov"
        Dec -> "dic"


toWeekdayNameItalian : Weekday -> String
toWeekdayNameItalian weekday =
    case weekday of
        Mon -> "lunedì"
        Tue -> "martedì"
        Wed -> "mercoledì"
        Thu -> "giovedì"
        Fri -> "venerdì"
        Sat -> "sabato"
        Sun -> "domenica"


toWeekdayAbbreviationItalian : Time.Weekday -> String
toWeekdayAbbreviationItalian weekday =
    case weekday of
        Mon -> "lun"
        Tue -> "mar"
        Wed -> "mer"
        Thu -> "gio"
        Fri -> "ven"
        Sat -> "sab"
        Sun -> "dom"


toAmPmItalian : Int -> String
toAmPmItalian amPm =
    case amPm of
        0 -> "AM"
        _ -> "PM"


toOrdinalSuffixItalian : Int -> String
toOrdinalSuffixItalian ordinal =
    case ordinal of
        _ -> "º"


relativeTimeOptionsItalian : RelativeTimeOptions
relativeTimeOptionsItalian =
    { someSecondsAgo = someSecondsAgoItalian
    , someMinutesAgo = someMinutesAgoItalian
    , someHoursAgo = someHoursAgoItalian
    , someDaysAgo = someDaysAgoItalian
    , someMonthsAgo = someMonthsAgoItalian
    , someYearsAgo = someYearsAgoItalian
    , rightNow = "Proprio ora"
    , inSomeSeconds = inSomeSecondsItalian
    , inSomeMinutes = inSomeMinutesItalian
    , inSomeHours = inSomeHoursItalian
    , inSomeDays = inSomeDaysItalian
    , inSomeMonths = inSomeMonthsItalian
    , inSomeYears = inSomeYearsItalian
    }


someSecondsAgoItalian : Int -> String
someSecondsAgoItalian seconds =
    if seconds == 1 then
        String.fromInt seconds ++ " secondo fa"
    else
        String.fromInt seconds ++ " secondi fa"


someMinutesAgoItalian : Int -> String
someMinutesAgoItalian minutes =
    if minutes == 1 then
        String.fromInt minutes ++ " minuto fa"
    else
        String.fromInt minutes ++ " minuti fa"


someHoursAgoItalian : Int -> String
someHoursAgoItalian hours =
    if hours == 1 then
        String.fromInt hours ++ " ora fa"
    else
        String.fromInt hours ++ " ore fa"


someDaysAgoItalian : Int -> String
someDaysAgoItalian days =
    if days == 1 then
        String.fromInt days ++ " giorno fa"
    else
        String.fromInt days ++ " giorni fa"


someMonthsAgoItalian : Int -> String
someMonthsAgoItalian months =
    if months == 1 then
        String.fromInt months ++ " mese fa"
    else
        String.fromInt months ++ " mesi fa"


someYearsAgoItalian : Int -> String
someYearsAgoItalian years =
    if years == 1 then
        String.fromInt years ++ " anno fa"
    else
        String.fromInt years ++ " anni fa"


inSomeSecondsItalian : Int -> String
inSomeSecondsItalian seconds =
    if seconds == 1 then
        "Tra " ++ String.fromInt seconds ++ " secondo"
    else
        "Tra " ++ String.fromInt seconds ++ " secondi"


inSomeMinutesItalian : Int -> String
inSomeMinutesItalian minutes =
    if minutes == 1 then
        "Tra " ++ String.fromInt minutes ++ " minuto"
    else
        "Tra " ++ String.fromInt minutes ++ " minuti"


inSomeHoursItalian : Int -> String
inSomeHoursItalian hours =
    if hours == 1 then
        "Tra " ++ String.fromInt hours ++ " ora"
    else
        "Tra " ++ String.fromInt hours ++ " ore"


inSomeDaysItalian : Int -> String
inSomeDaysItalian days =
    if days == 1 then
        "Tra " ++ String.fromInt days ++ " giorno"
    else
        "Tra " ++ String.fromInt days ++ " giorni"


inSomeMonthsItalian : Int -> String
inSomeMonthsItalian months =
    if months == 1 then
        "Tra " ++ String.fromInt months ++ " mese"
    else
        "Tra " ++ String.fromInt months ++ " mesi"


inSomeYearsItalian : Int -> String
inSomeYearsItalian years =
    if years == 1 then
        "Tra " ++ String.fromInt years ++ " anno"
    else
        "Tra " ++ String.fromInt years ++ " anni"


tokensItalianWithYear : List DateFormat.Token
tokensItalianWithYear =
    tokensItalianWithoutYear
        ++ [ DateFormat.text " "
           , DateFormat.yearNumber
           ]


tokensItalianWithoutYear : List DateFormat.Token
tokensItalianWithoutYear =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " "
    , DateFormat.monthNameFull
    ]
