module Components.EntryField exposing
    ( EntryField
    , FieldType(..)
    , new
    , view
    , withLabel
    , withPlaceholder
    , withRows
    , withType
    )

import Html.Styled as Html exposing (Html, div, input, textarea)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)


type EntryField msg
    = Settings
        { label : Maybe String
        , onInput : String -> msg
        , placeholder : Maybe String
        , rows : Int
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
        , rows = 1
        , onInput = props.onInput
        , placeholder = Nothing
        , theme = props.theme
        , type_ = FieldTypeText
        , value = props.value
        }


withRows : Int -> EntryField msg -> EntryField msg
withRows rows (Settings settings) =
    Settings { settings | rows = rows }


withLabel : String -> EntryField msg -> EntryField msg
withLabel label (Settings settings) =
    Settings { settings | label = Just label }


withPlaceholder : String -> EntryField msg -> EntryField msg
withPlaceholder placeholder (Settings settings) =
    Settings { settings | placeholder = Just placeholder }


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

        ( elementType, attrs ) =
            if settings.rows > 1 then
                ( textarea, [ Attr.rows settings.rows ] )

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
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_1
            , Tw.w_full
            ]
        ]
        [ labelElement
        , elementType
            (styles.colorStyleBackground
                ++ attrs
                ++ nameAttr
                ++ placeholderAttr
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
        ]


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
