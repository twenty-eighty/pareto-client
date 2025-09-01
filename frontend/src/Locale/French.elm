module Locale.French exposing (dateFormatFrench, relativeTimeOptionsFrench, tokensFrenchWithYear, tokensFrenchWithoutYear)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import Time exposing (Month(..), Weekday(..))




dateFormatFrench : DateFormat.Language.Language
dateFormatFrench =
    { toMonthName = toMonthNameFrench
    , toMonthAbbreviation = toMonthAbbreviationFrench
    , toWeekdayName = toWeekdayNameFrench
    , toWeekdayAbbreviation = toWeekdayAbbreviationFrench
    , toAmPm = toAmPmFrench
    , toOrdinalSuffix = toOrdinalSuffixFrench
    }


toMonthNameFrench : Month -> String
toMonthNameFrench month =
    case month of
        Jan ->
            "janvier"

        Feb ->
            "février"

        Mar ->
            "mars"

        Apr ->
            "avril"

        May ->
            "mai"

        Jun ->
            "juin"

        Jul ->
            "juillet"

        Aug ->
            "août"

        Sep ->
            "septembre"

        Oct ->
            "octobre"

        Nov ->
            "novembre"

        Dec ->
            "décembre"


toMonthAbbreviationFrench : Month -> String
toMonthAbbreviationFrench month =
    case month of
        Jan ->
            "janv."

        Feb ->
            "févr."

        Mar ->
            "mars"

        Apr ->
            "avr."

        May ->
            "mai"

        Jun ->
            "juin"

        Jul ->
            "juil."

        Aug ->
            "août"

        Sep ->
            "sept."

        Oct ->
            "oct."

        Nov ->
            "nov."

        Dec ->
            "déc."


toWeekdayNameFrench : Weekday -> String
toWeekdayNameFrench weekday =
    case weekday of
        Mon ->
            "lundi"

        Tue ->
            "mardi"

        Wed ->
            "mercredi"

        Thu ->
            "jeudi"

        Fri ->
            "vendredi"

        Sat ->
            "samedi"

        Sun ->
            "dimanche"


toWeekdayAbbreviationFrench : Time.Weekday -> String
toWeekdayAbbreviationFrench weekday =
    case weekday of
        Mon ->
            "lun."

        Tue ->
            "mar."

        Wed ->
            "mer."

        Thu ->
            "jeu."

        Fri ->
            "ven."

        Sat ->
            "sam."

        Sun ->
            "dim."


toAmPmFrench : Int -> String
toAmPmFrench amPm =
    case amPm of
        0 ->
            "AM"

        _ ->
            "PM"


toOrdinalSuffixFrench : Int -> String
toOrdinalSuffixFrench ordinal =
    case ordinal of
        1 ->
            "er"

        _ ->
            "e"


relativeTimeOptionsFrench : RelativeTimeOptions
relativeTimeOptionsFrench =
    { someSecondsAgo = someSecondsAgoFrench
    , someMinutesAgo = someMinutesAgoFrench
    , someHoursAgo = someHoursAgoFrench
    , someDaysAgo = someDaysAgoFrench
    , someMonthsAgo = someMonthsAgoFrench
    , someYearsAgo = someYearsAgoFrench
    , rightNow = "À l'instant"
    , inSomeSeconds = inSomeSecondsFrench
    , inSomeMinutes = inSomeMinutesFrench
    , inSomeHours = inSomeHoursFrench
    , inSomeDays = inSomeDaysFrench
    , inSomeMonths = inSomeMonthsFrench
    , inSomeYears = inSomeYearsFrench
    }


someSecondsAgoFrench : Int -> String
someSecondsAgoFrench seconds =
    if seconds == 1 then
        "Il y a " ++ String.fromInt seconds ++ " seconde"

    else
        "Il y a " ++ String.fromInt seconds ++ " secondes"


someMinutesAgoFrench : Int -> String
someMinutesAgoFrench minutes =
    if minutes == 1 then
        "Il y a " ++ String.fromInt minutes ++ " minute"

    else
        "Il y a " ++ String.fromInt minutes ++ " minutes"


someHoursAgoFrench : Int -> String
someHoursAgoFrench hours =
    if hours == 1 then
        "Il y a " ++ String.fromInt hours ++ " heure"

    else
        "Il y a " ++ String.fromInt hours ++ " heures"


someDaysAgoFrench : Int -> String
someDaysAgoFrench days =
    if days == 1 then
        "Il y a " ++ String.fromInt days ++ " jour"

    else
        "Il y a " ++ String.fromInt days ++ " jours"


someMonthsAgoFrench : Int -> String
someMonthsAgoFrench months =
    if months == 1 then
        "Il y a " ++ String.fromInt months ++ " mois"

    else
        "Il y a " ++ String.fromInt months ++ " mois"


someYearsAgoFrench : Int -> String
someYearsAgoFrench years =
    if years == 1 then
        "Il y a " ++ String.fromInt years ++ " an"

    else
        "Il y a " ++ String.fromInt years ++ " ans"


inSomeSecondsFrench : Int -> String
inSomeSecondsFrench seconds =
    if seconds == 1 then
        "Dans " ++ String.fromInt seconds ++ " seconde"

    else
        "Dans " ++ String.fromInt seconds ++ " secondes"


inSomeMinutesFrench : Int -> String
inSomeMinutesFrench minutes =
    if minutes == 1 then
        "Dans " ++ String.fromInt minutes ++ " minute"

    else
        "Dans " ++ String.fromInt minutes ++ " minutes"


inSomeHoursFrench : Int -> String
inSomeHoursFrench hours =
    if hours == 1 then
        "Dans " ++ String.fromInt hours ++ " heure"

    else
        "Dans " ++ String.fromInt hours ++ " heures"


inSomeDaysFrench : Int -> String
inSomeDaysFrench days =
    if days == 1 then
        "Dans " ++ String.fromInt days ++ " jour"

    else
        "Dans " ++ String.fromInt days ++ " jours"


inSomeMonthsFrench : Int -> String
inSomeMonthsFrench months =
    if months == 1 then
        "Dans " ++ String.fromInt months ++ " mois"

    else
        "Dans " ++ String.fromInt months ++ " mois"


inSomeYearsFrench : Int -> String
inSomeYearsFrench years =
    if years == 1 then
        "Dans " ++ String.fromInt years ++ " an"

    else
        "Dans " ++ String.fromInt years ++ " ans"


tokensFrenchWithYear : List DateFormat.Token
tokensFrenchWithYear =
    tokensFrenchWithoutYear
        ++ [ DateFormat.text " "
           , DateFormat.yearNumber
           ]


tokensFrenchWithoutYear : List DateFormat.Token
tokensFrenchWithoutYear =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " "
    , DateFormat.monthNameFull
    ] 