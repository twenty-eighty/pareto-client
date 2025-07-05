module Locale.Spanish exposing (dateFormatSpanish, relativeTimeOptionsSpanish, tokensSpanishWithYear, tokensSpanishWithoutYear)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import Time exposing (Month(..), Weekday(..))


dateFormatSpanish : DateFormat.Language.Language
dateFormatSpanish =
    { toMonthName = toMonthNameSpanish
    , toMonthAbbreviation = toMonthAbbreviationSpanish
    , toWeekdayName = toWeekdayNameSpanish
    , toWeekdayAbbreviation = toWeekdayAbbreviationSpanish
    , toAmPm = toAmPmSpanish
    , toOrdinalSuffix = toOrdinalSuffixSpanish
    }


toMonthNameSpanish : Month -> String
toMonthNameSpanish month =
    case month of
        Jan -> "enero"
        Feb -> "febrero"
        Mar -> "marzo"
        Apr -> "abril"
        May -> "mayo"
        Jun -> "junio"
        Jul -> "julio"
        Aug -> "agosto"
        Sep -> "septiembre"
        Oct -> "octubre"
        Nov -> "noviembre"
        Dec -> "diciembre"


toMonthAbbreviationSpanish : Month -> String
toMonthAbbreviationSpanish month =
    case month of
        Jan -> "ene"
        Feb -> "feb"
        Mar -> "mar"
        Apr -> "abr"
        May -> "may"
        Jun -> "jun"
        Jul -> "jul"
        Aug -> "ago"
        Sep -> "sep"
        Oct -> "oct"
        Nov -> "nov"
        Dec -> "dic"


toWeekdayNameSpanish : Weekday -> String
toWeekdayNameSpanish weekday =
    case weekday of
        Mon -> "lunes"
        Tue -> "martes"
        Wed -> "miércoles"
        Thu -> "jueves"
        Fri -> "viernes"
        Sat -> "sábado"
        Sun -> "domingo"


toWeekdayAbbreviationSpanish : Time.Weekday -> String
toWeekdayAbbreviationSpanish weekday =
    case weekday of
        Mon -> "lun"
        Tue -> "mar"
        Wed -> "mié"
        Thu -> "jue"
        Fri -> "vie"
        Sat -> "sáb"
        Sun -> "dom"


toAmPmSpanish : Int -> String
toAmPmSpanish amPm =
    case amPm of
        0 -> "AM"
        _ -> "PM"


toOrdinalSuffixSpanish : Int -> String
toOrdinalSuffixSpanish ordinal =
    case ordinal of
        _ -> "º"


relativeTimeOptionsSpanish : RelativeTimeOptions
relativeTimeOptionsSpanish =
    { someSecondsAgo = someSecondsAgoSpanish
    , someMinutesAgo = someMinutesAgoSpanish
    , someHoursAgo = someHoursAgoSpanish
    , someDaysAgo = someDaysAgoSpanish
    , someMonthsAgo = someMonthsAgoSpanish
    , someYearsAgo = someYearsAgoSpanish
    , rightNow = "Ahora mismo"
    , inSomeSeconds = inSomeSecondsSpanish
    , inSomeMinutes = inSomeMinutesSpanish
    , inSomeHours = inSomeHoursSpanish
    , inSomeDays = inSomeDaysSpanish
    , inSomeMonths = inSomeMonthsSpanish
    , inSomeYears = inSomeYearsSpanish
    }


someSecondsAgoSpanish : Int -> String
someSecondsAgoSpanish seconds =
    if seconds == 1 then
        "Hace " ++ String.fromInt seconds ++ " segundo"
    else
        "Hace " ++ String.fromInt seconds ++ " segundos"


someMinutesAgoSpanish : Int -> String
someMinutesAgoSpanish minutes =
    if minutes == 1 then
        "Hace " ++ String.fromInt minutes ++ " minuto"
    else
        "Hace " ++ String.fromInt minutes ++ " minutos"


someHoursAgoSpanish : Int -> String
someHoursAgoSpanish hours =
    if hours == 1 then
        "Hace " ++ String.fromInt hours ++ " hora"
    else
        "Hace " ++ String.fromInt hours ++ " horas"


someDaysAgoSpanish : Int -> String
someDaysAgoSpanish days =
    if days == 1 then
        "Hace " ++ String.fromInt days ++ " día"
    else
        "Hace " ++ String.fromInt days ++ " días"


someMonthsAgoSpanish : Int -> String
someMonthsAgoSpanish months =
    if months == 1 then
        "Hace " ++ String.fromInt months ++ " mes"
    else
        "Hace " ++ String.fromInt months ++ " meses"


someYearsAgoSpanish : Int -> String
someYearsAgoSpanish years =
    if years == 1 then
        "Hace " ++ String.fromInt years ++ " año"
    else
        "Hace " ++ String.fromInt years ++ " años"


inSomeSecondsSpanish : Int -> String
inSomeSecondsSpanish seconds =
    if seconds == 1 then
        "En " ++ String.fromInt seconds ++ " segundo"
    else
        "En " ++ String.fromInt seconds ++ " segundos"


inSomeMinutesSpanish : Int -> String
inSomeMinutesSpanish minutes =
    if minutes == 1 then
        "En " ++ String.fromInt minutes ++ " minuto"
    else
        "En " ++ String.fromInt minutes ++ " minutos"


inSomeHoursSpanish : Int -> String
inSomeHoursSpanish hours =
    if hours == 1 then
        "En " ++ String.fromInt hours ++ " hora"
    else
        "En " ++ String.fromInt hours ++ " horas"


inSomeDaysSpanish : Int -> String
inSomeDaysSpanish days =
    if days == 1 then
        "En " ++ String.fromInt days ++ " día"
    else
        "En " ++ String.fromInt days ++ " días"


inSomeMonthsSpanish : Int -> String
inSomeMonthsSpanish months =
    if months == 1 then
        "En " ++ String.fromInt months ++ " mes"
    else
        "En " ++ String.fromInt months ++ " meses"


inSomeYearsSpanish : Int -> String
inSomeYearsSpanish years =
    if years == 1 then
        "En " ++ String.fromInt years ++ " año"
    else
        "En " ++ String.fromInt years ++ " años"


tokensSpanishWithYear : List DateFormat.Token
tokensSpanishWithYear =
    tokensSpanishWithoutYear
        ++ [ DateFormat.text " de "
           , DateFormat.yearNumber
           ]


tokensSpanishWithoutYear : List DateFormat.Token
tokensSpanishWithoutYear =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " de "
    , DateFormat.monthNameFull
    ] 