module BrowserEnv exposing (BrowserEnv, Environment(..), Msg(..), formatDate, init, subscriptions, update, updateTimeZone)

import DateFormat
import DateFormat.Language
import DateFormat.Relative exposing (RelativeTimeOptions)
import DefaultLanguage
import Dict exposing (Dict)
import Http
import I18Next
import Json.Decode as Decode
import Locale exposing (Language(..), dateFormatFromLanguage, languageFromLocale, numberFormatFromLanguage, relativeTimeOptionsFromLanguage)
import Nostr.Types exposing (IncomingMessage)
import Ports
import Result exposing (Result)
import Task
import Time exposing (Posix)
import TimeZone
import Url.Builder exposing (Root(..))


type alias BrowserEnv =
    { backendUrl : String
    , dateFormatLanguage : DateFormat.Language.Language
    , dateFormatTokensWithYear : List DateFormat.Token
    , dateFormatTokensWithoutYear : List DateFormat.Token
    , dateFormatRelativeTimeOptions : RelativeTimeOptions
    , darkMode : Bool
    , environment : Environment
    , formatNumber : String -> Float -> String
    , frontendUrl : String
    , installPromptAvailable : Bool
    , language : Language
    , locale : String
    , nativeSharingAvailable : Bool
    , translations : I18Next.Translations
    , now : Posix
    , zone : Time.Zone
    , zoneName : String
    , errors : List String
    }


type Msg
    = UpdateLocale String
    | UpdateTranslations (Result Http.Error I18Next.Translations)
    | UpdateZone Time.Zone
    | ReceivedPortMessage IncomingMessage
    | SwitchToDarkMode Bool
    | Now Posix


type alias InitParams =
    { backendUrl : String
    , darkMode : Bool
    , environment : Maybe String
    , frontendUrl : String
    , locale : String
    , nativeSharingAvailable : Bool
    }


type Environment
    = Production
    | Development


init : InitParams -> ( BrowserEnv, Cmd Msg )
init initParams =
    let
        language =
            languageFromLocale initParams.locale

        ( dateFormatLanguage, dateFormatTokensWithYear, dateFormatTokensWithoutYear ) =
            dateFormatFromLanguage language

        relativeTimeOptions =
            relativeTimeOptionsFromLanguage language

        browserEnv =
            { frontendUrl = initParams.frontendUrl
            , backendUrl = initParams.backendUrl
            , dateFormatLanguage = dateFormatLanguage
            , dateFormatTokensWithYear = dateFormatTokensWithYear
            , dateFormatTokensWithoutYear = dateFormatTokensWithoutYear
            , dateFormatRelativeTimeOptions = relativeTimeOptions
            , darkMode = initParams.darkMode
            , environment = environmentFromString initParams.environment
            , formatNumber = numberFormatFromLanguage language
            , errors = []
            , installPromptAvailable = False
            , language = language
            , locale = initParams.locale
            , nativeSharingAvailable = initParams.nativeSharingAvailable
            , translations = DefaultLanguage.defaultLanguage
            , now = Time.millisToPosix 0
            , zone = Time.utc
            , zoneName = "UTC"
            }
    in
    ( browserEnv
    , Cmd.batch
        [ Task.perform Now Time.now
        , Task.perform UpdateZone Time.here
        , requestTranslations language
        ]
    )


environmentFromString : Maybe String -> Environment
environmentFromString envString =
    case envString of
        Just "dev" ->
            Development

        _ ->
            Production


requestTranslations : Language -> Cmd Msg
requestTranslations language =
    case language of
        German _ ->
            Http.get
                { url = "/translations/lang-" ++ translationsLocale language ++ ".json"
                , expect = Http.expectJson UpdateTranslations I18Next.translationsDecoder
                }

        _ ->
            Cmd.none


formatDate : BrowserEnv -> Posix -> String
formatDate browserEnv time =
    if differsByMoreThan24Hours browserEnv.now time then
        if Time.toYear browserEnv.zone browserEnv.now == Time.toYear browserEnv.zone time then
            DateFormat.formatWithLanguage browserEnv.dateFormatLanguage browserEnv.dateFormatTokensWithoutYear browserEnv.zone time

        else
            DateFormat.formatWithLanguage browserEnv.dateFormatLanguage browserEnv.dateFormatTokensWithYear browserEnv.zone time

    else
        DateFormat.Relative.relativeTimeWithOptions browserEnv.dateFormatRelativeTimeOptions browserEnv.now time


differsByMoreThan24Hours : Posix -> Posix -> Bool
differsByMoreThan24Hours time1 time2 =
    let
        -- Convert Posix times to milliseconds
        millis1 =
            Time.posixToMillis time1

        millis2 =
            Time.posixToMillis time2

        -- Calculate the absolute difference in milliseconds
        diffInMillis =
            abs (millis1 - millis2)

        -- Number of milliseconds in 24 hours
        millisIn24Hours =
            24 * 60 * 60 * 1000
    in
    diffInMillis > millisIn24Hours


translationsLocale : Language -> String
translationsLocale language =
    case language of
        German _ ->
            "de_DE"

        _ ->
            "en_GB"


update : Msg -> BrowserEnv -> ( BrowserEnv, Cmd Msg )
update msg browserEnv =
    case msg of
        UpdateLocale locale ->
            if locale /= browserEnv.locale then
                updateLocale locale browserEnv

            else
                ( browserEnv, Cmd.none )

        UpdateTranslations translationsResult ->
            case translationsResult of
                Ok translations ->
                    ( { browserEnv | translations = translations }, Cmd.none )

                Err error ->
                    ( { browserEnv | errors = httpError error :: browserEnv.errors }, Cmd.none )

        UpdateZone zone ->
            ( { browserEnv | zone = zone }, Cmd.none )

        ReceivedPortMessage portMessage ->
            updateWithPortMessage browserEnv portMessage

        SwitchToDarkMode darkMode ->
            ( { browserEnv | darkMode = darkMode }, Cmd.none )

        Now now ->
            ( { browserEnv | now = now }, Cmd.none )


updateWithPortMessage : BrowserEnv -> IncomingMessage -> ( BrowserEnv, Cmd Msg )
updateWithPortMessage browserEnv portMessage =
    case portMessage.messageType of
        "darkMode" ->
            case Decode.decodeValue Decode.bool portMessage.value of
                Ok darkMode ->
                    ( { browserEnv | darkMode = darkMode }, Cmd.none )

                Err _ ->
                    ( browserEnv, Cmd.none )

        _ ->
            ( browserEnv, Cmd.none )


httpError error =
    case error of
        Http.BadUrl badUrl ->
            "Bad URL: " ++ badUrl

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


modelWithTranslations : BrowserEnv -> Decode.Value -> ( BrowserEnv, Cmd Msg )
modelWithTranslations browserEnv translationsValue =
    case Decode.decodeValue I18Next.translationsDecoder translationsValue of
        Ok translations ->
            ( { browserEnv | translations = translations }, Cmd.none )

        Err error ->
            ( { browserEnv | errors = Decode.errorToString error :: browserEnv.errors }, Cmd.none )


updateLocale : String -> BrowserEnv -> ( BrowserEnv, Cmd Msg )
updateLocale locale browserEnv =
    let
        language =
            languageFromLocale locale
    in
    ( { browserEnv
        | locale = locale
        , language = language

        --           , dateFormatConfig = dateFormatConfigFromLanguage language
      }
    , requestTranslations language
    )



--  dateFormatConfigFromLanguage : Language -> Derberos.Date.Core.Config
--  dateFormatConfigFromLanguage language =
--      let
--          config =
--              case language of
--                  English "us" ->
--                      Derberos.Date.L10n.EN_US.config
--                  German _ ->
--                      CalendarConfigDE.config
--                  Spanish ->
--                      Derberos.Date.L10n.ES_ES.config
--                  _ ->
--                      Derberos.Date.L10n.EN_US.config
--      in
--      { config | getCommonFormatTime = getShortFormatTime }


getShortFormatTime : Time.Zone -> Posix -> String
getShortFormatTime tz time =
    let
        hour =
            Time.toHour tz time
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    hour ++ ":" ++ minute


updateTimeZone : String -> BrowserEnv -> BrowserEnv
updateTimeZone zoneName browserEnv =
    case Dict.get zoneName TimeZone.zones of
        Just zone ->
            { browserEnv | zone = zone (), zoneName = zoneName }

        Nothing ->
            browserEnv


subscriptions : BrowserEnv -> Sub Msg
subscriptions browserEnv =
    Sub.batch
        [ Ports.receiveMessage ReceivedPortMessage

        --, Time.every 1000 Now
        ]
