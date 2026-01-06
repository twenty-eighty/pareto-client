module Components.Calendar exposing (Model, Msg(..), SelectableRange(..), SelectionMode(..), dayRows, init, new, selectedTime, selectedZone, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Dropdown as Dropdown
import Countries
import CountriesTimezones exposing (Country)
import Css
import Derberos.Date.Core exposing (DateRecord, civilToPosix, newDateRecord)
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth, getWeekday, monthToNumber, monthToNumber1, numberOfDaysInMonth, weekdayToNumber)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Graphics
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import I18Next
import Tailwind.Utilities as Tw
import Task
import Time exposing (Month, Posix, Weekday(..), Zone)
import TimeZone
import Translations.Calendar as Translations
import Ui.Styles exposing (Styles, Theme, darkMode, stylesForTheme)


type Model
    = Model
        { currentTime : Maybe Posix
        , selectedZone : Zone
        , selectedZoneName : Maybe String
        , selectedCountry : Maybe CountriesTimezones.Country
        , selectedTime : Maybe Posix
        , displayedMonth : Maybe Month
        , displayedYear : Maybe Int
        , firstSelectableMonth : Maybe Month
        , firstSelectableYear : Maybe Int
        , lastSelectableMonth : Maybe Month
        , lastSelectableYear : Maybe Int
        , selectableRange : SelectableRange
        , selectionMode : SelectionMode
        , availableTimes : List Posix
        , timezoneSelectorOpen : Bool
        , timezoneCountryDict : Dict String CountriesTimezones.Country
        , codeToCountryDict : Dict String CountriesTimezones.Country
        , countrySelectionDropdown : Dropdown.Model CountriesTimezones.Country
        , timezoneSelectionDropdown : Dropdown.Model String
        }


type SelectableRange
    = PastAndFuture
    | Past
    | Future


type SelectionMode
    = DaySelection
    | AvailableTimeSelection


type Msg
    = UpdateCurrentTime Posix
    | UpdateSelectedZone (Result TimeZone.Error ( String, Time.Zone ))
    | UpdateAvailableTimes (List Posix)
    | SelectDay Int
    | SelectTime Posix String
    | GoToPrevMonth
    | GoToNextMonth
    | ToggleTimezoneSelector
    | CountryDropdownSent (Dropdown.Msg CountriesTimezones.Country Msg)
    | TimezoneDropdownSent (Dropdown.Msg String Msg)
    | CountrySelected String
    | TimezoneSelected String


type DayState
    = DayNotPresent
    | DayNotAvailable Int
    | DayAvailable Int (List Posix)
    | DaySelected Int (List Posix)


type Calendar msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , browserEnv : BrowserEnv
        , theme : Theme
        }

new :
    { model : Model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> Calendar msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : { selectableRange : SelectableRange, selectionMode : SelectionMode, selectedPublishDate : Maybe Posix } -> ( Model, Cmd Msg )
init { selectableRange, selectionMode, selectedPublishDate } =
    ( Model
        { currentTime = Nothing
        , selectedZone = Time.utc
        , selectedZoneName = Just "Etc/UTC"
        , selectedCountry = Nothing
        , selectedTime = selectedPublishDate
        , displayedMonth = Nothing
        , displayedYear = Nothing
        , firstSelectableMonth = Nothing
        , firstSelectableYear = Nothing
        , lastSelectableMonth = Nothing
        , lastSelectableYear = Nothing
        , selectableRange = selectableRange
        , selectionMode = selectionMode
        , availableTimes = []
        , timezoneSelectorOpen = False
        , timezoneCountryDict = CountriesTimezones.timezoneToCountryDict
        , codeToCountryDict = CountriesTimezones.codeToCountryDict
        , countrySelectionDropdown = Dropdown.init { selected = Just CountriesTimezones.DE }
        , timezoneSelectionDropdown = Dropdown.init { selected = Nothing }
        }
    , Cmd.batch
        [ Task.perform UpdateCurrentTime Time.now
        , Task.attempt UpdateSelectedZone TimeZone.getZone
        ]
    )


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect Msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , Effect.map props.toMsg effect
            )
    in
    toParentModel <|
        case props.msg of
            UpdateCurrentTime currentTime ->
                ( modelWithTimeAndZone (Model model) (Just currentTime) model.selectedZone, Effect.none )

            UpdateSelectedZone result ->
                case result of
                    Ok ( zoneName, zone ) ->
                        let
                            modelWithZoneName =
                                { model
                                    | selectedZoneName = Just zoneName
                                    , selectedCountry = Dict.get zoneName model.timezoneCountryDict
                                }
                        in
                        ( modelWithTimeAndZone (Model modelWithZoneName) model.currentTime zone, Effect.none )

                    Err _ ->
                        ( Model model, Effect.none )

            UpdateAvailableTimes availableTimes ->
                ( modelWithTimeAndZone (Model { model | availableTimes = availableTimes }) model.currentTime model.selectedZone, Effect.none )

            SelectDay _ ->
                ( Model model, Effect.none )

            SelectTime time _ ->
                ( Model { model | selectedTime = Just time }, Effect.none )

            GoToPrevMonth ->
                ( modelWithPrevMonth (Model model), Effect.none )

            GoToNextMonth ->
                ( modelWithNextMonth (Model model), Effect.none )

            ToggleTimezoneSelector ->
                ( Model { model | timezoneSelectorOpen = not model.timezoneSelectorOpen }, Effect.none )

            CountryDropdownSent innerMsg ->
                Dropdown.update
                    { msg = innerMsg
                    , model = model.countrySelectionDropdown
                    , toModel = \dropdown -> Model { model | countrySelectionDropdown = dropdown }
                    , toMsg = CountryDropdownSent
                    }

            TimezoneDropdownSent innerMsg ->
                Dropdown.update
                    { msg = innerMsg
                    , model = model.timezoneSelectionDropdown
                    , toModel = \dropdown -> Model { model | timezoneSelectionDropdown = dropdown }
                    , toMsg = TimezoneDropdownSent
                    }

            CountrySelected countryCode ->
                let
                    country =
                        Dict.get countryCode model.codeToCountryDict

                    firstTimezone =
                        Maybe.map CountriesTimezones.timezonesForCountry country
                            |> Maybe.andThen List.head

                    modelWithNewTimeZone =
                        case firstTimezone of
                            Just timezone ->
                                { model | timezoneSelectionDropdown = Dropdown.selectItem model.timezoneSelectionDropdown (Just timezone) }

                            Nothing ->
                                model
                in
                ( Model { modelWithNewTimeZone | selectedCountry = country }, Effect.none )

            TimezoneSelected timezone ->
                let
                    newZone =
                        Dict.get timezone TimeZone.zones
                            |> Maybe.map (\a -> a ())
                in
                case newZone of
                    Just zone ->
                        ( Model { model | selectedZoneName = Just timezone, selectedZone = zone }, Effect.none )

                    Nothing ->
                        ( Model model, Effect.none )


selectedZone : Model -> Zone
selectedZone (Model model) =
    model.selectedZone


selectedTime : Model -> Maybe Posix
selectedTime (Model model) =
    model.selectedTime


modelWithPrevMonth : Model -> Model
modelWithPrevMonth (Model model) =
    case ( model.displayedMonth, model.displayedYear ) of
        ( Just month, Just year ) ->
            if month /= Time.Jan then
                Model { model | displayedMonth = Just (getPrevMonth month) }

            else
                Model { model | displayedMonth = Just (getPrevMonth month), displayedYear = Just (year - 1) }

        ( _, _ ) ->
            Model model


modelWithNextMonth : Model -> Model
modelWithNextMonth (Model model) =
    case ( model.displayedMonth, model.displayedYear ) of
        ( Just month, Just year ) ->
            if month /= Time.Dec then
                Model { model | displayedMonth = Just (getNextMonth month) }

            else
                Model { model | displayedMonth = Just (getNextMonth month), displayedYear = Just (year + 1) }

        ( _, _ ) ->
            Model model


modelWithTimeAndZone : Model -> Maybe Posix -> Zone -> Model
modelWithTimeAndZone (Model model) maybeCurrentTime zone =
    case maybeCurrentTime of
        Just currentTime ->
            let
                (Model tempModel) =
                    case model.selectedTime of
                        Just time ->
                            Model { model | displayedMonth = Just (Time.toMonth zone time), displayedYear = Just (Time.toYear zone time) }

                        Nothing ->
                            modelWithFirstAvailableDate (Model { model | currentTime = Just currentTime, selectedZone = zone }) zone currentTime model.availableTimes
            in
            case model.selectableRange of
                PastAndFuture ->
                    Model tempModel

                Past ->
                    Model
                        { tempModel
                            | lastSelectableMonth = Just (Time.toMonth zone currentTime)
                            , lastSelectableYear = Just (Time.toYear zone currentTime)
                        }

                Future ->
                    Model
                        { tempModel
                            | firstSelectableMonth = Just (Time.toMonth zone currentTime)
                            , firstSelectableYear = Just (Time.toYear zone currentTime)
                        }

        Nothing ->
            Model { model | selectedZone = zone }


modelWithFirstAvailableDate : Model -> Zone -> Posix -> List Posix -> Model
modelWithFirstAvailableDate (Model model) zone currentTime availableTimes =
    let
        first =
            firstTime availableTimes currentTime
    in
    case availableTimes of
        [] ->
            Model
                { model
                    | displayedMonth = Just (Time.toMonth zone first)
                    , displayedYear = Just (Time.toYear zone first)
                }

        _ ->
            Model
                { model
                    | displayedMonth = Just (Time.toMonth zone first)
                    , displayedYear = Just (Time.toYear zone first)
                    , selectedTime = Just first

                    --            , selectedDay = Just (Time.toDay zone first)
                    --            , selectedMonth = Just (Time.toMonth zone first)
                    --            , selectedYear = Just (Time.toYear zone first)
                }


firstTime : List Posix -> Posix -> Posix
firstTime times currentTime =
    times
        |> List.map Time.posixToMillis
        |> List.minimum
        |> Maybe.map Time.millisToPosix
        |> Maybe.withDefault currentTime


view : Calendar msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            stylesForTheme settings.theme

        availableTimes =
            case model.selectionMode of
                DaySelection ->
                    availableTimesForMonth (Model model)

                AvailableTimeSelection ->
                    model.availableTimes
    in
    div [ css [ Tw.flex, Tw.space_x_8 ] ]
        [ div [ css [ Tw.w_full ] ]
            ([ div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_between
                    , Tw.h_10
                    , Tw.px_5
                    , Tw.font_bold
                    , Tw.text_color styles.colorB4
                    , Tw.bg_color styles.colorB1
                    , darkMode
                        [ Tw.text_color styles.colorB4DarkMode
                        , Tw.bg_color styles.colorB1DarkMode
                        ]
                    ]
                ]
                [ prevMonthButton (Settings settings)
                , div [ css [ Tw.text_center ] ] [ text <| monthName (Model model) settings.browserEnv ++ " " ++ String.fromInt (Maybe.withDefault 0 model.displayedYear) ]
                , nextMonthButton (Settings settings)
                ]
             , div
                [ css
                    [ Tw.flex
                    , Tw.h_8
                    , Tw.text_color styles.colorB4
                    , Tw.bg_color styles.colorB1
                    , darkMode
                        [ Tw.text_color styles.colorB4DarkMode
                        , Tw.bg_color styles.colorB1DarkMode
                        ]
                    , Tw.text_xs
                    ]
                ]
                [ weekDayCell (firstCharOfWeekday settings.browserEnv Mon)
                , weekDayCell (firstCharOfWeekday settings.browserEnv Tue)
                , weekDayCell (firstCharOfWeekday settings.browserEnv Wed)
                , weekDayCell (firstCharOfWeekday settings.browserEnv Thu)
                , weekDayCell (firstCharOfWeekday settings.browserEnv Fri)
                , weekDayCell (firstCharOfWeekday settings.browserEnv Sat)
                , weekDayCell (firstCharOfWeekday settings.browserEnv Sun)
                ]
             ]
                ++ dayRows (Settings settings) availableTimes
            )

        {-
           , div [ css [ Tw.w_full, Tw.space_y_6, Tw.text_sm ] ]
               [ timezoneSelectorElement (Settings settings)
                   model.selectedCountry
                   model.selectedZoneName
                   settings.browserEnv.translations
               , viewAvailableTimes styles timesOfSelectedDay model.selectedZoneName
               ]
        -}
        ]
        |> Html.map settings.toMsg


availableTimesForMonth : Model -> List Posix
availableTimesForMonth (Model model) =
    let
        currentDayInMonth =
            model.currentTime
                |> Maybe.map (Time.toDay model.selectedZone)
                |> Maybe.withDefault 0

        currentMonthDisplayed =
            case ( model.currentTime, model.displayedMonth, model.displayedYear ) of
                ( Just currentTime, Just displayedMonth, Just displayedYear ) ->
                    (Time.toMonth model.selectedZone currentTime == displayedMonth)
                        && (Time.toYear model.selectedZone currentTime == displayedYear)

                _ ->
                    False
    in
    case ( model.selectableRange, model.displayedMonth, model.displayedYear ) of
        ( PastAndFuture, Just displayedMonth, Just displayedYear ) ->
            if currentMonthDisplayed then
                noonDatesForMonth (Model model) displayedMonth displayedYear 1 (currentDayInMonth - 1)
                    ++ (model.currentTime |> Maybe.withDefault (Time.millisToPosix 0))
                    :: noonDatesForMonth (Model model) displayedMonth displayedYear (currentDayInMonth + 1) (numberOfDaysInMonth displayedYear displayedMonth)

            else
                noonDatesForMonth (Model model) displayedMonth displayedYear 1 (numberOfDaysInMonth displayedYear displayedMonth)

        ( Past, Just displayedMonth, Just displayedYear ) ->
            if currentMonthDisplayed then
                noonDatesForMonth (Model model) displayedMonth displayedYear 1 (currentDayInMonth - 1)
                    ++ [ model.currentTime |> Maybe.withDefault (Time.millisToPosix 0) ]

            else
                noonDatesForMonth (Model model) displayedMonth displayedYear 1 (numberOfDaysInMonth displayedYear displayedMonth)

        ( Future, Just displayedMonth, Just displayedYear ) ->
            if currentMonthDisplayed then
                noonDatesForMonth (Model model) displayedMonth displayedYear (currentDayInMonth + 1) (numberOfDaysInMonth displayedYear displayedMonth)

            else
                noonDatesForMonth (Model model) displayedMonth displayedYear 1 (numberOfDaysInMonth displayedYear displayedMonth)

        _ ->
            []


noonDatesForMonth : Model -> Month -> Int -> Int -> Int -> List Posix
noonDatesForMonth (Model model) month year firstDay lastDay =
    List.range firstDay lastDay
        |> List.map (\day -> newDateRecord year (monthToNumber1 month) day 12 0 0 0 model.selectedZone)
        |> List.map civilToPosix


timezoneSelectorElement : Calendar msg -> Maybe CountriesTimezones.Country -> Maybe String -> I18Next.Translations -> Html Msg
timezoneSelectorElement (Settings settings) selectedCountry selectedZoneName translations =
    let
        (Model model) =
            settings.model
    in
    if model.timezoneSelectorOpen then
        div
            [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
            [ Dropdown.new
                { model = model.countrySelectionDropdown
                , toMsg = CountryDropdownSent
                , choices = CountriesTimezones.countries
                , allowNoSelection = False
                , toLabel = countryToLabel translations
                }
                |> Dropdown.view
            , Dropdown.new
                { model = model.timezoneSelectionDropdown
                , toMsg = TimezoneDropdownSent
                , choices = CountriesTimezones.timezonesForCountry (Dropdown.selectedItem model.countrySelectionDropdown |> Maybe.withDefault CountriesTimezones.DE)
                , allowNoSelection = False
                , toLabel = timezoneToLabel
                }
                |> Dropdown.view
            ]

    else
        div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2 ] ]
            [ Html.text <| Translations.timeZone [ translations ]
            , div
                [ css [ Tw.flex, Tw.flex_row, Tw.cursor_pointer ]
                , onClick (TimezoneSelected (Dropdown.selectedItem model.timezoneSelectionDropdown |> Maybe.withDefault "UTC"))
                ]
                [ Html.text <| timezoneDisplayString selectedCountry selectedZoneName translations
                , text " "
                , Graphics.angleDownIcon 15 "rgb(27, 151, 140)"
                ]
            ]


timezoneDisplayString : Maybe CountriesTimezones.Country -> Maybe String -> I18Next.Translations -> String
timezoneDisplayString selectedCountry selectedZoneName translations =
    case selectedCountry of
        Just country ->
            CountriesTimezones.codeForCountry country
                |> Countries.fromCode
                |> Maybe.map2 (\zoneName ctry -> zoneName ++ " (" ++ ctry.flag ++ " " ++ ctry.name ++ ")") selectedZoneName
                |> Maybe.withDefault "UTC"

        Nothing ->
            "UTC"


countryToLabel : I18Next.Translations -> Maybe Country -> String
countryToLabel translations maybeCountry =
    case maybeCountry of
        Just country ->
            CountriesTimezones.localizedCountryName translations country

        Nothing ->
            "No country"


timezoneToLabel : Maybe String -> String
timezoneToLabel maybeTimezone =
    maybeTimezone
        |> Maybe.withDefault "UTC"


viewAvailableTimes : Styles Msg -> List ( DateRecord, Posix ) -> Maybe String -> Html Msg
viewAvailableTimes styles times maybeTimeZone =
    case maybeTimeZone of
        Just timeZone ->
            div [ css [ Tw.flex, Tw.space_x_4, Tw.font_bold, Tw.text_color styles.colorB1 ] ]
                (List.map (viewAvailableTime styles timeZone) times)

        Nothing ->
            Html.text ""


viewAvailableTime : Styles Msg -> String -> ( DateRecord, Posix ) -> Html Msg
viewAvailableTime styles timeZone ( dateRecord, time ) =
    div
        [ css [ Tw.w_20, Tw.h_10, Tw.bg_color styles.colorB4, Tw.rounded, Css.hover [ Tw.bg_color styles.colorB4 ], Css.hover [ Tw.text_color styles.colorB1 ], Tw.cursor_pointer, Tw.flex, Tw.items_center, Tw.justify_center ]
        , onClick (SelectTime time timeZone)
        ]
        [ text <| String.fromInt dateRecord.hour ++ ":" ++ (String.fromInt dateRecord.minute |> String.padLeft 2 '0') ]


dayRows : Calendar msg -> List Posix -> List (Html Msg)
dayRows (Settings settings) availableTimes =
    let
        (Model model) =
            settings.model
    in
    case ( model.displayedMonth, model.displayedYear ) of
        ( Just displayedMonth, Just displayedYear ) ->
            let
                daysInMonthCount =
                    numberOfDaysInMonth displayedYear displayedMonth

                firstDay =
                    firstDayOfMonth displayedMonth displayedYear model.selectedZone

                firstDayWeekday =
                    getWeekday model.selectedZone firstDay

                emptyDayCellsBefore =
                    weekdayToNumber firstDayWeekday

                emptyDayCellsAfter =
                    7 - modBy 7 (emptyDayCellsBefore + daysInMonthCount)

                monthDays =
                    List.repeat emptyDayCellsBefore DayNotPresent
                        ++ (List.range 1 daysInMonthCount
                                |> List.map
                                    (\day ->
                                        let
                                            timesForDay =
                                                availableTimesForDay (Model model) displayedYear displayedMonth day availableTimes
                                        in
                                        case ( model.selectedTime, timesForDay ) of
                                            ( _, [] ) ->
                                                DayNotAvailable day

                                            ( Just time, _ ) ->
                                                if (day == Time.toDay model.selectedZone time) && (displayedMonth == Time.toMonth model.selectedZone time) && (displayedYear == Time.toYear model.selectedZone time) then
                                                    DaySelected day timesForDay

                                                else
                                                    DayAvailable day timesForDay

                                            ( Nothing, _ ) ->
                                                DayAvailable day timesForDay
                                    )
                           )
                        ++ List.repeat emptyDayCellsAfter DayNotPresent
            in
            dayRow settings.theme monthDays

        ( _, _ ) ->
            [ Html.text "" ]


availableTimesForDay : Model -> Int -> Month -> Int -> List Posix -> List Posix
availableTimesForDay (Model model) year month day availableTimes =
    availableTimes
        |> List.filter (\availableTime -> Time.toYear model.selectedZone availableTime == year && Time.toMonth model.selectedZone availableTime == month && Time.toDay model.selectedZone availableTime == day)


dayRow : Theme -> List DayState -> List (Html Msg)
dayRow theme days =
    let
        daysToPrint =
            List.take 7 days

        remainingDays =
            List.drop 7 days
    in
    if List.isEmpty daysToPrint then
        []

    else
        viewDays theme daysToPrint :: dayRow theme remainingDays


viewDays : Theme -> List DayState -> Html Msg
viewDays theme days =
    div [ css [ Tw.flex, Tw.h_8 ] ]
        (List.map (viewDayCell theme) days)


viewDayCell : Theme -> DayState -> Html Msg
viewDayCell theme dayState =
    let
        styles =
            stylesForTheme theme

        ( cellStyles, cellText ) =
            case dayState of
                DayNotPresent ->
                    ( [ Tw.bg_color styles.colorB1
                      , darkMode [ Tw.bg_color styles.colorB1DarkMode ]
                      ]
                    , ""
                    )

                DayNotAvailable day ->
                    ( [ Tw.bg_color styles.colorB2
                      , Tw.text_color styles.colorB3
                      , darkMode [ Tw.bg_color styles.colorB2DarkMode, Tw.text_color styles.colorB3DarkMode ]
                      ]
                    , String.fromInt day
                    )

                DayAvailable day _ ->
                    ( [ Tw.bg_color styles.colorB3
                      , Css.hover [ Tw.bg_color styles.colorB4 ]
                      , Tw.cursor_pointer
                      , Tw.text_color styles.colorB1
                      , darkMode
                            [ Tw.bg_color styles.colorB3DarkMode
                            , Tw.text_color styles.colorB1DarkMode
                            , Css.hover [ Tw.bg_color styles.colorB4DarkMode ]
                            ]
                      ]
                    , String.fromInt day
                    )

                DaySelected day _ ->
                    ( [ Tw.bg_color styles.colorB4
                      , Css.hover [ Tw.bg_color styles.colorB4 ]
                      , Tw.cursor_pointer
                      , Tw.text_color styles.colorB1
                      , darkMode [ Tw.bg_color styles.colorB4DarkMode, Tw.text_color styles.colorB1DarkMode ]
                      ]
                    , String.fromInt day
                    )
    in
    div
        [ css
            (cellStyles
                ++ [ Tw.w_20
                   , Tw.h_full
                   , Tw.border
                   , Tw.border_color styles.colorB1
                   , Tw.flex
                   , Tw.items_center
                   , Tw.justify_center
                   , Tw.text_xs
                   , darkMode [ Tw.border_color styles.colorB1DarkMode ]
                   ]
            )
        , case dayState of
            DayAvailable day availableTimes ->
                case availableTimes of
                    [ singleTime ] ->
                        onClick (SelectTime singleTime "UTC")

                    _ ->
                        onClick (SelectDay day)

            DaySelected _ _ ->
                css []

            _ ->
                css []
        ]
        [ text cellText ]


commonCellAttributes : Styles Msg -> List (Html.Attribute Msg)
commonCellAttributes styles =
    [ css [ Tw.w_20, Tw.h_full, Tw.border, Tw.border_color styles.colorB4 ] ]


firstDayOfMonth : Month -> Int -> Zone -> Posix
firstDayOfMonth month year tz =
    newDateRecord year (monthToNumber1 month) 1 0 0 0 0 tz
        |> civilToPosix


prevMonthButton : Calendar msg -> Html Msg
prevMonthButton (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            stylesForTheme settings.theme
    in
    case ( model.firstSelectableMonth, model.firstSelectableYear ) of
        ( Just firstSelectableMonth, Just firstSelectableYear ) ->
            case ( model.displayedMonth, model.displayedYear ) of
                ( Just displayedMonth, Just displayedYear ) ->
                    let
                        firstSelectable =
                            firstSelectableYear * 100 + monthToNumber firstSelectableMonth
                    in
                    if displayedYear * 100 + monthToNumber displayedMonth > firstSelectable then
                        div [ css [ Tw.cursor_pointer ], onClick GoToPrevMonth ] [ text "<" ]

                    else
                        div [ css [ Tw.text_color styles.colorB3 ] ] [ text "<" ]

                _ ->
                    div [ css [ Tw.cursor_pointer ], onClick GoToPrevMonth ] [ text "<" ]

        _ ->
            div [ css [ Tw.cursor_pointer ], onClick GoToPrevMonth ] [ text "<" ]


nextMonthButton : Calendar msg -> Html Msg
nextMonthButton (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            stylesForTheme settings.theme
    in
    case ( model.lastSelectableMonth, model.lastSelectableYear ) of
        ( Just lastSelectableMonth, Just lastSelectableYear ) ->
            case ( model.displayedMonth, model.displayedYear ) of
                ( Just displayedMonth, Just displayedYear ) ->
                    let
                        lastSelectable =
                            lastSelectableYear * 100 + monthToNumber lastSelectableMonth
                    in
                    if displayedYear * 100 + monthToNumber displayedMonth < lastSelectable then
                        div [ css [ Tw.cursor_pointer ], onClick GoToNextMonth ] [ text ">" ]

                    else
                        div [ css [ Tw.text_color styles.colorB3 ] ] [ text ">" ]

                _ ->
                    div [ css [ Tw.text_color styles.colorB3 ] ] [ text ">" ]

        _ ->
            div [ css [ Tw.cursor_pointer ], onClick GoToNextMonth ] [ text ">" ]


monthName : Model -> BrowserEnv -> String
monthName (Model model) browserEnv =
    case model.displayedMonth of
        Just month ->
            case month of
                Time.Jan ->
                    "January"

                Time.Feb ->
                    "February"

                Time.Mar ->
                    "March"

                Time.Apr ->
                    "April"

                Time.May ->
                    "May"

                Time.Jun ->
                    "June"

                Time.Jul ->
                    "July"

                Time.Aug ->
                    "August"

                Time.Sep ->
                    "September"

                Time.Oct ->
                    "October"

                Time.Nov ->
                    "November"

                Time.Dec ->
                    "December"

        Nothing ->
            ""


firstCharOfWeekday : BrowserEnv -> Weekday -> String
firstCharOfWeekday browserEnv weekday =
    case weekday of
        Time.Mon ->
            "M"

        Time.Tue ->
            "T"

        Time.Wed ->
            "W"

        Time.Thu ->
            "T"

        Time.Fri ->
            "F"

        Time.Sat ->
            "S"

        Time.Sun ->
            "S"


weekDayCell : String -> Html Msg
weekDayCell day =
    div [ css [ Tw.w_20, Tw.flex, Tw.justify_center, Tw.items_center ] ]
        [ text day ]


dayColumnWidth : Int
dayColumnWidth =
    50


dateRecordsFromTimes : List Posix -> Zone -> List ( DateRecord, Posix )
dateRecordsFromTimes times zone =
    List.map (\time -> ( dateRecordFromTime zone time, time )) times


dateRecordFromTime : Zone -> Posix -> DateRecord
dateRecordFromTime zone time =
    Derberos.Date.Core.addTimezoneMilliseconds zone time
        |> Derberos.Date.Core.posixToCivil
