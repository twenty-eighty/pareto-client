module Locale exposing (..)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import I18Next exposing (Translations)
import Languages.English
import Languages.German
import Languages.Italian
import Languages.French
import Languages.Spanish
import Languages.Russian
import Locale.French exposing (dateFormatFrench, relativeTimeOptionsFrench, tokensFrenchWithYear, tokensFrenchWithoutYear)
import Locale.German exposing (dateFormatGerman, relativeTimeOptionsGerman, tokensGermanWithYear, tokensGermanWithoutYear)
import Locale.Italian exposing (dateFormatItalian, relativeTimeOptionsItalian, tokensItalianWithYear, tokensItalianWithoutYear)
import Locale.Spanish exposing (dateFormatSpanish, relativeTimeOptionsSpanish, tokensSpanishWithYear, tokensSpanishWithoutYear)
import Locale.Russian exposing (dateFormatRussian, relativeTimeOptionsRussian, tokensRussianWithYear, tokensRussianWithoutYear)
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
    | Polish
    | Portuguese
    | Russian
    | Spanish
    | Swedish


defaultLanguages : List Language
defaultLanguages =
    [ English "US", French, German "DE", Italian, Polish, Portuguese, Russian, Spanish, Swedish ]

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

        "pl" ->
            Polish

        "ru" ->
            Russian

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

        Polish ->
            Translations.polishText [ translations ]

        Russian ->
            Translations.russianText [ translations ]

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

        "pl" ->
            Just Polish

        "pt" ->
            Just Portuguese

        "ru" ->
            Just Russian

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

        Polish ->
            "pl"

        Portuguese ->
            "pt"

        Russian ->
            "ru"

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

        Italian ->
            ( dateFormatItalian, tokensItalianWithYear, tokensItalianWithoutYear )

        French ->
            ( dateFormatFrench, tokensFrenchWithYear, tokensFrenchWithoutYear )

        Spanish ->
            ( dateFormatSpanish, tokensSpanishWithYear, tokensSpanishWithoutYear )

        Russian ->
            ( dateFormatRussian, tokensRussianWithYear, tokensRussianWithoutYear )

        _ ->
            ( DateFormat.Language.english, tokensEnglishWithYear, tokensEnglishWithoutYear )


relativeTimeOptionsFromLanguage : Language -> RelativeTimeOptions
relativeTimeOptionsFromLanguage language =
    case language of
        French ->
            relativeTimeOptionsFrench

        German _ ->
            relativeTimeOptionsGerman

        Italian ->
            relativeTimeOptionsItalian

        Spanish ->
            relativeTimeOptionsSpanish

        Russian ->
            relativeTimeOptionsRussian

        _ ->
            DateFormat.Relative.defaultRelativeOptions


numberFormatFromLanguage : Language -> (String -> Float -> String)
numberFormatFromLanguage language =
    case language of
        German _ ->
            Numeral.formatWithLanguage Languages.German.lang

        Italian ->
            Numeral.formatWithLanguage Languages.Italian.lang

        French ->
            Numeral.formatWithLanguage Languages.French.lang

        Spanish ->
            Numeral.formatWithLanguage Languages.Spanish.lang

        Russian ->
            Numeral.formatWithLanguage Languages.Russian.lang

        _ ->
            Numeral.formatWithLanguage Languages.English.lang



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
