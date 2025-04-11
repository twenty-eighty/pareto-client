module Components.EntryField exposing
    ( EntryField
    , FieldType(..)
    , new
    , view
    , withLabel
    , withPlaceholder
    , withRequired
    , withRows
    , withSubmitMsg
    , withSuggestions
    , withType
    )

import Html.Styled as Html exposing (Html, datalist, input, option, textarea)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)


type EntryField msg
    = Settings
        { label : Maybe String
        , onSubmit : Maybe msg
        , onInput : String -> msg
        , placeholder : Maybe String
        , required : Bool
        , rows : Int
        , showForm : Bool
        , suggestions : Maybe ( String, List String )
        , theme : Theme
        , type_ : FieldType
        , value : String
        }


type FieldType
    = FieldTypeColor
    | FieldTypeDate
    | FieldTypeDateTimeLocal
    | FieldTypeEmail
    | FieldTypeHidden
    | FieldTypeMonth
    | FieldTypeNumber
    | FieldTypePassword
    | FieldTypeSearch
    | FieldTypeTel
    | FieldTypeText
    | FieldTypeTime
    | FieldTypeUrl
    | FieldTypeWeek


new :
    { value : String
    , onInput : String -> msg
    , theme : Theme
    }
    -> EntryField msg
new props =
    Settings
        { label = Nothing
        , onSubmit = Nothing
        , onInput = props.onInput
        , placeholder = Nothing
        , required = False
        , rows = 1
        , showForm = False
        , suggestions = Nothing
        , theme = props.theme
        , type_ = FieldTypeText
        , value = props.value
        }


withRows : Int -> EntryField msg -> EntryField msg
withRows rows (Settings settings) =
    Settings { settings | rows = rows }


withSubmitMsg : Maybe msg -> EntryField msg -> EntryField msg
withSubmitMsg msg (Settings settings) =
    Settings { settings | onSubmit = msg, showForm = True }


withLabel : String -> EntryField msg -> EntryField msg
withLabel label (Settings settings) =
    Settings { settings | label = Just label }


withPlaceholder : String -> EntryField msg -> EntryField msg
withPlaceholder placeholder (Settings settings) =
    Settings { settings | placeholder = Just placeholder }


withRequired : EntryField msg -> EntryField msg
withRequired (Settings settings) =
    Settings { settings | required = True }


withSuggestions : String -> List String -> EntryField msg -> EntryField msg
withSuggestions identifier suggestions (Settings settings) =
    if List.length suggestions > 0 then
        Settings { settings | suggestions = Just ( identifier, suggestions ) }

    else
        Settings settings


withType : FieldType -> EntryField msg -> EntryField msg
withType type_ (Settings settings) =
    Settings { settings | type_ = type_ }


view : EntryField msg -> Html msg
view (Settings settings) =
    let
        styles =
            stylesForTheme settings.theme

        placeholderAttr =
            case settings.placeholder of
                Just placeholder ->
                    [ Attr.placeholder placeholder ]

                Nothing ->
                    []

        requiredAttr =
            if settings.required then
                [ Attr.required True ]

            else
                []

        ( elementType, attrs ) =
            if settings.rows > 1 then
                ( textarea
                , [ Attr.rows settings.rows
                    , css [  Tw.h_auto

                    ]
                 ] )

            else
                ( input, [ Attr.type_ <| fieldTypeToString settings.type_ ] )

        ( labelElement, nameAttr ) =
            case settings.label of
                Just label ->
                    ( Html.label
                        [ Attr.for label
                        ]
                        [ Html.text label
                        ]
                    , [ Attr.name label ]
                    )

                Nothing ->
                    ( emptyHtml, [] )

        ( suggestionsAttr, suggestionsElement ) =
            case settings.suggestions of
                Just ( identifier, suggestions ) ->
                    ( [ Attr.list identifier ]
                    , suggestions
                        |> List.filter (\suggestion -> suggestion /= settings.value)
                        |> suggestionDataList identifier
                    )

                Nothing ->
                    ( [], emptyHtml )

        ( commitAttr, formElement ) =
            case ( settings.showForm, settings.onSubmit ) of
                ( True, Just onSubmit ) ->
                    ( [ Events.onSubmit onSubmit ], Html.form )

                ( True, Nothing ) ->
                    -- the form element has to be shown constantly to avoid DOM changes while editing
                    ( [], Html.form )

                ( False, _ ) ->
                    ( [], Html.div )
    in
    formElement
        (commitAttr
            ++ [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_1
                    , Tw.w_full
                    ]
               ]
        )
        [ labelElement
        , elementType
            (styles.colorStyleBackground
                ++ attrs
                ++ nameAttr
                ++ placeholderAttr
                ++ requiredAttr
                ++ suggestionsAttr
                ++ [ Attr.value settings.value
                   , Events.onInput settings.onInput
                   , css
                        [ Tw.appearance_none
                        , Tw.bg_scroll
                        , Tw.bg_clip_border
                        , Tw.rounded_md
                        , Tw.border_2
                        , Tw.box_border
                        , Tw.cursor_text
                        , Tw.block
                        , Tw.ps_2
                        , Tw.pe_2
                        , Tw.pl_2
                        , Tw.pr_2
                        , Tw.h_10
                        , Tw.w_full
                        ]
                   ]
            )
            []
        , suggestionsElement
        ]


suggestionDataList : String -> List String -> Html msg
suggestionDataList identifier suggestions =
    datalist
        [ Attr.id identifier
        ]
        (suggestions
            |> List.map
                (\suggestion ->
                    option [ Attr.value suggestion ] []
                )
        )


fieldTypeToString : FieldType -> String
fieldTypeToString fieldType =
    case fieldType of
        FieldTypeColor ->
            "color"

        FieldTypeDate ->
            "date"

        FieldTypeDateTimeLocal ->
            "datetime-local"

        FieldTypeEmail ->
            "email"

        FieldTypeHidden ->
            "hidden"

        FieldTypeMonth ->
            "month"

        FieldTypeNumber ->
            "number"

        FieldTypePassword ->
            "password"

        FieldTypeSearch ->
            "search"

        FieldTypeTel ->
            "tel"

        FieldTypeText ->
            "text"

        FieldTypeTime ->
            "time"

        FieldTypeUrl ->
            "url"

        FieldTypeWeek ->
            "week"
