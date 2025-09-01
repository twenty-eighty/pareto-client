module Locale.Russian exposing (dateFormatRussian, relativeTimeOptionsRussian, tokensRussianWithYear, tokensRussianWithoutYear)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import Time exposing (Month(..), Weekday(..))


dateFormatRussian : DateFormat.Language.Language
dateFormatRussian =
    { toMonthName = toMonthNameRussian
    , toMonthAbbreviation = toMonthAbbreviationRussian
    , toWeekdayName = toWeekdayNameRussian
    , toWeekdayAbbreviation = toWeekdayAbbreviationRussian
    , toAmPm = toAmPmRussian
    , toOrdinalSuffix = toOrdinalSuffixRussian
    }


toMonthNameRussian : Month -> String
toMonthNameRussian month =
    case month of
        Jan ->
            "январь"

        Feb ->
            "февраль"

        Mar ->
            "март"

        Apr ->
            "апрель"

        May ->
            "май"

        Jun ->
            "июнь"

        Jul ->
            "июль"

        Aug ->
            "август"

        Sep ->
            "сентябрь"

        Oct ->
            "октябрь"

        Nov ->
            "ноябрь"

        Dec ->
            "декабрь"


toMonthAbbreviationRussian : Month -> String
toMonthAbbreviationRussian month =
    case month of
        Jan ->
            "янв."

        Feb ->
            "февр."

        Mar ->
            "март"

        Apr ->
            "апр."

        May ->
            "май"

        Jun ->
            "июнь"

        Jul ->
            "июль"

        Aug ->
            "авг."

        Sep ->
            "сент."

        Oct ->
            "окт."

        Nov ->
            "ноя."

        Dec ->
            "дек."


toWeekdayNameRussian : Weekday -> String
toWeekdayNameRussian weekday =
    case weekday of
        Mon ->
            "понедельник"

        Tue ->
            "вторник"

        Wed ->
            "среда"

        Thu ->
            "четверг"

        Fri ->
            "пятница"

        Sat ->
            "суббота"

        Sun ->
            "воскресенье"


toWeekdayAbbreviationRussian : Time.Weekday -> String
toWeekdayAbbreviationRussian weekday =
    case weekday of
        Mon ->
            "пн."

        Tue ->
            "вт."

        Wed ->
            "ср."

        Thu ->
            "чт."

        Fri ->
            "пт."

        Sat ->
            "сб."

        Sun ->
            "вс."


toAmPmRussian : Int -> String
toAmPmRussian amPm =
    case amPm of
        0 ->
            "утра"

        _ ->
            "вечера"


toOrdinalSuffixRussian : Int -> String
toOrdinalSuffixRussian ordinal =
    case modBy 10 ordinal of
        1 ->
            if modBy 100 ordinal == 11 then
                "-е"
            else
                "-е"

        2 ->
            if modBy 100 ordinal == 12 then
                "-е"
            else
                "-е"

        3 ->
            if modBy 100 ordinal == 13 then
                "-е"
            else
                "-е"

        _ ->
            "-е"


relativeTimeOptionsRussian : RelativeTimeOptions
relativeTimeOptionsRussian =
    { someSecondsAgo = someSecondsAgoRussian
    , someMinutesAgo = someMinutesAgoRussian
    , someHoursAgo = someHoursAgoRussian
    , someDaysAgo = someDaysAgoRussian
    , someMonthsAgo = someMonthsAgoRussian
    , someYearsAgo = someYearsAgoRussian
    , rightNow = "Только что"
    , inSomeSeconds = inSomeSecondsRussian
    , inSomeMinutes = inSomeMinutesRussian
    , inSomeHours = inSomeHoursRussian
    , inSomeDays = inSomeDaysRussian
    , inSomeMonths = inSomeMonthsRussian
    , inSomeYears = inSomeYearsRussian
    }


someSecondsAgoRussian : Int -> String
someSecondsAgoRussian seconds =
    let
        lastDigit = modBy 10 seconds
        lastTwoDigits = modBy 100 seconds
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        String.fromInt seconds ++ " секунд назад"
    else
        case lastDigit of
            1 ->
                String.fromInt seconds ++ " секунду назад"

            2 ->
                String.fromInt seconds ++ " секунды назад"

            3 ->
                String.fromInt seconds ++ " секунды назад"

            4 ->
                String.fromInt seconds ++ " секунды назад"

            _ ->
                String.fromInt seconds ++ " секунд назад"


someMinutesAgoRussian : Int -> String
someMinutesAgoRussian minutes =
    let
        lastDigit = modBy 10 minutes
        lastTwoDigits = modBy 100 minutes
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        String.fromInt minutes ++ " минут назад"
    else
        case lastDigit of
            1 ->
                String.fromInt minutes ++ " минуту назад"

            2 ->
                String.fromInt minutes ++ " минуты назад"

            3 ->
                String.fromInt minutes ++ " минуты назад"

            4 ->
                String.fromInt minutes ++ " минуты назад"

            _ ->
                String.fromInt minutes ++ " минут назад"


someHoursAgoRussian : Int -> String
someHoursAgoRussian hours =
    let
        lastDigit = modBy 10 hours
        lastTwoDigits = modBy 100 hours
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        String.fromInt hours ++ " часов назад"
    else
        case lastDigit of
            1 ->
                String.fromInt hours ++ " час назад"

            2 ->
                String.fromInt hours ++ " часа назад"

            3 ->
                String.fromInt hours ++ " часа назад"

            4 ->
                String.fromInt hours ++ " часа назад"

            _ ->
                String.fromInt hours ++ " часов назад"


someDaysAgoRussian : Int -> String
someDaysAgoRussian days =
    let
        lastDigit = modBy 10 days
        lastTwoDigits = modBy 100 days
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        String.fromInt days ++ " дней назад"
    else
        case lastDigit of
            1 ->
                String.fromInt days ++ " день назад"

            2 ->
                String.fromInt days ++ " дня назад"

            3 ->
                String.fromInt days ++ " дня назад"

            4 ->
                String.fromInt days ++ " дня назад"

            _ ->
                String.fromInt days ++ " дней назад"


someMonthsAgoRussian : Int -> String
someMonthsAgoRussian months =
    let
        lastDigit = modBy 10 months
        lastTwoDigits = modBy 100 months
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        String.fromInt months ++ " месяцев назад"
    else
        case lastDigit of
            1 ->
                String.fromInt months ++ " месяц назад"

            2 ->
                String.fromInt months ++ " месяца назад"

            3 ->
                String.fromInt months ++ " месяца назад"

            4 ->
                String.fromInt months ++ " месяца назад"

            _ ->
                String.fromInt months ++ " месяцев назад"


someYearsAgoRussian : Int -> String
someYearsAgoRussian years =
    let
        lastDigit = modBy 10 years
        lastTwoDigits = modBy 100 years
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        String.fromInt years ++ " лет назад"
    else
        case lastDigit of
            1 ->
                String.fromInt years ++ " год назад"

            2 ->
                String.fromInt years ++ " года назад"

            3 ->
                String.fromInt years ++ " года назад"

            4 ->
                String.fromInt years ++ " года назад"

            _ ->
                String.fromInt years ++ " лет назад"


inSomeSecondsRussian : Int -> String
inSomeSecondsRussian seconds =
    let
        lastDigit = modBy 10 seconds
        lastTwoDigits = modBy 100 seconds
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        "через " ++ String.fromInt seconds ++ " секунд"
    else
        case lastDigit of
            1 ->
                "через " ++ String.fromInt seconds ++ " секунду"

            2 ->
                "через " ++ String.fromInt seconds ++ " секунды"

            3 ->
                "через " ++ String.fromInt seconds ++ " секунды"

            4 ->
                "через " ++ String.fromInt seconds ++ " секунды"

            _ ->
                "через " ++ String.fromInt seconds ++ " секунд"


inSomeMinutesRussian : Int -> String
inSomeMinutesRussian minutes =
    let
        lastDigit = modBy 10 minutes
        lastTwoDigits = modBy 100 minutes
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        "через " ++ String.fromInt minutes ++ " минут"
    else
        case lastDigit of
            1 ->
                "через " ++ String.fromInt minutes ++ " минуту"

            2 ->
                "через " ++ String.fromInt minutes ++ " минуты"

            3 ->
                "через " ++ String.fromInt minutes ++ " минуты"

            4 ->
                "через " ++ String.fromInt minutes ++ " минуты"

            _ ->
                "через " ++ String.fromInt minutes ++ " минут"


inSomeHoursRussian : Int -> String
inSomeHoursRussian hours =
    let
        lastDigit = modBy 10 hours
        lastTwoDigits = modBy 100 hours
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        "через " ++ String.fromInt hours ++ " часов"
    else
        case lastDigit of
            1 ->
                "через " ++ String.fromInt hours ++ " час"

            2 ->
                "через " ++ String.fromInt hours ++ " часа"

            3 ->
                "через " ++ String.fromInt hours ++ " часа"

            4 ->
                "через " ++ String.fromInt hours ++ " часа"

            _ ->
                "через " ++ String.fromInt hours ++ " часов"


inSomeDaysRussian : Int -> String
inSomeDaysRussian days =
    let
        lastDigit = modBy 10 days
        lastTwoDigits = modBy 100 days
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        "через " ++ String.fromInt days ++ " дней"
    else
        case lastDigit of
            1 ->
                "через " ++ String.fromInt days ++ " день"

            2 ->
                "через " ++ String.fromInt days ++ " дня"

            3 ->
                "через " ++ String.fromInt days ++ " дня"

            4 ->
                "через " ++ String.fromInt days ++ " дня"

            _ ->
                "через " ++ String.fromInt days ++ " дней"


inSomeMonthsRussian : Int -> String
inSomeMonthsRussian months =
    let
        lastDigit = modBy 10 months
        lastTwoDigits = modBy 100 months
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        "через " ++ String.fromInt months ++ " месяцев"
    else
        case lastDigit of
            1 ->
                "через " ++ String.fromInt months ++ " месяц"

            2 ->
                "через " ++ String.fromInt months ++ " месяца"

            3 ->
                "через " ++ String.fromInt months ++ " месяца"

            4 ->
                "через " ++ String.fromInt months ++ " месяца"

            _ ->
                "через " ++ String.fromInt months ++ " месяцев"


inSomeYearsRussian : Int -> String
inSomeYearsRussian years =
    let
        lastDigit = modBy 10 years
        lastTwoDigits = modBy 100 years
    in
    if lastTwoDigits >= 11 && lastTwoDigits <= 14 then
        "через " ++ String.fromInt years ++ " лет"
    else
        case lastDigit of
            1 ->
                "через " ++ String.fromInt years ++ " год"

            2 ->
                "через " ++ String.fromInt years ++ " года"

            3 ->
                "через " ++ String.fromInt years ++ " года"

            4 ->
                "через " ++ String.fromInt years ++ " года"

            _ ->
                "через " ++ String.fromInt years ++ " лет"


tokensRussianWithYear : List DateFormat.Token
tokensRussianWithYear =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " "
    , DateFormat.monthNameAbbreviated
    , DateFormat.text " "
    , DateFormat.yearNumber
    ]


tokensRussianWithoutYear : List DateFormat.Token
tokensRussianWithoutYear =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " "
    , DateFormat.monthNameAbbreviated
    ] 