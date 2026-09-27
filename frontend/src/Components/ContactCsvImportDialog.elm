module Components.ContactCsvImportDialog exposing
    ( ImportProgress
    , Model
    , Msg
    , PreviewResult(..)
    , applyPreview
    , applyProgress
    , begin
    , decodePreview
    , decodeProgress
    , init
    , new
    , update
    , view
    )

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.Dropdown as Dropdown
import Components.EntryField as EntryField
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, button, div, h2, input, label, table, tbody, td, text, th, thead, tr)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import I18Next
import Json.Decode as Decode
import Newsletters.Subscribers as Subscribers
import Newsletters.Types exposing (SubscriberField(..), fieldName)
import Ports
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Translations.EmailImportDialog as ImportTranslations
import Translations.Subscribers as Translations
import Ui.Styles exposing (Theme, darkMode, stylesForTheme)


type Model
    = Model State


type State
    = Hidden
    | Choosing
    | Reading
    | Configure PreviewData
    | Map MappingData
    | Options OptionsData
    | Importing ImportProgress
    | Finished ImportProgress


type alias PreviewData =
    { rows : List (List String)
    , skipRows : Int
    }


type alias MappingData =
    { skipRows : Int
    , header : List String
    , dropdowns : Dict String (Dropdown.Model SubscriberField)
    }


type alias OptionsData =
    { skipRows : Int
    , mapping : List ( Int, SubscriberField )
    , overwrite : Bool
    , tags : String
    }


type alias ImportProgress =
    { stored : Int
    , skipped : Int
    , errors : Int
    , error : Maybe String
    }


type PreviewResult
    = PreviewCancelled
    | PreviewError String
    | PreviewRows (List (List String))


type Msg
    = CloseDialog
    | ChooseFile
    | UpdateSkippedRows String
    | ContinueToMapping
    | MappingDropdownSent String (Dropdown.Msg SubscriberField Msg)
    | ContinueToOptions
    | OverwriteExistingClicked Bool
    | UpdateTags String
    | StartImport


type ContactCsvImportDialog msg
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
    -> ContactCsvImportDialog msg
new props =
    Settings props


init : Model
init =
    Model Hidden


begin : Model -> Model
begin _ =
    Model Reading


decodePreview : Decode.Value -> Result Decode.Error PreviewResult
decodePreview value =
    Decode.decodeValue previewDecoder value


decodeProgress : Decode.Value -> Result Decode.Error { stored : Int, skipped : Int, errors : Int, done : Bool, error : Maybe String }
decodeProgress value =
    Decode.decodeValue progressDecoder value


applyPreview : PreviewResult -> Model -> Model
applyPreview result _ =
    case result of
        PreviewCancelled ->
            Model Choosing

        PreviewError message ->
            Model (Finished { stored = 0, skipped = 0, errors = 0, error = Just message })

        PreviewRows [] ->
            Model (Finished { stored = 0, skipped = 0, errors = 0, error = Nothing })

        PreviewRows rows ->
            Model (Configure { rows = rows, skipRows = headerSkip rows })


applyProgress : { stored : Int, skipped : Int, errors : Int, done : Bool, error : Maybe String } -> Model -> Model
applyProgress progress (Model state) =
    let
        next =
            { stored = progress.stored
            , skipped = progress.skipped
            , errors = progress.errors
            , error = progress.error
            }
    in
    case state of
        Importing _ ->
            Model <|
                if progress.done then
                    Finished next

                else
                    Importing next

        _ ->
            Model state


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    }
    -> ( model, Effect msg )
update props =
    let
        (Model state) =
            props.model

        changed : ( State, Effect Msg ) -> ( model, Effect msg )
        changed ( next, effect ) =
            ( props.toModel (Model next)
            , Effect.map props.toMsg effect
            )
    in
    changed <|
        case props.msg of
            CloseDialog ->
                ( Hidden
                , case state of
                    Importing _ ->
                        Ports.cancelContactCsvImport |> Effect.sendCmd

                    _ ->
                        Effect.none
                )

            ChooseFile ->
                ( Reading
                , Ports.pickContactCsv |> Effect.sendCmd
                )

            UpdateSkippedRows value ->
                case ( state, String.toInt value ) of
                    ( Configure preview, Just skipRows ) ->
                        ( Configure { preview | skipRows = clamp 0 (max 0 (List.length preview.rows - 1)) skipRows }
                        , Effect.none
                        )

                    _ ->
                        ( state, Effect.none )

            ContinueToMapping ->
                case state of
                    Configure preview ->
                        case headerRow preview of
                            Just header ->
                                ( Map
                                    { skipRows = preview.skipRows
                                    , header = header
                                    , dropdowns = mappingDropdowns header
                                    }
                                , Effect.none
                                )

                            Nothing ->
                                ( state, Effect.none )

                    _ ->
                        ( state, Effect.none )

            MappingDropdownSent columnName innerMsg ->
                case state of
                    Map mapping ->
                        case Dict.get columnName mapping.dropdowns of
                            Just dropdown ->
                                let
                                    ( next, effect ) =
                                        Dropdown.update
                                            { msg = innerMsg
                                            , model = dropdown
                                            , toModel =
                                                \updated ->
                                                    Map { mapping | dropdowns = Dict.insert columnName updated mapping.dropdowns }
                                            , toMsg = MappingDropdownSent columnName
                                            }
                                in
                                case next of
                                    Map _ ->
                                        ( next, effect )

                                    _ ->
                                        ( state, Effect.none )

                            Nothing ->
                                ( state, Effect.none )

                    _ ->
                        ( state, Effect.none )

            ContinueToOptions ->
                case state of
                    Map mapping ->
                        ( Options
                            { skipRows = mapping.skipRows
                            , mapping = selectedMapping mapping.dropdowns
                            , overwrite = False
                            , tags = "import_" ++ BrowserEnv.formatIsoDate props.browserEnv props.browserEnv.now
                            }
                        , Effect.none
                        )

                    _ ->
                        ( state, Effect.none )

            OverwriteExistingClicked overwrite ->
                case state of
                    Options options ->
                        ( Options { options | overwrite = overwrite }, Effect.none )

                    _ ->
                        ( state, Effect.none )

            UpdateTags tags ->
                case state of
                    Options options ->
                        ( Options { options | tags = tags }, Effect.none )

                    _ ->
                        ( state, Effect.none )

            StartImport ->
                case state of
                    Options options ->
                        ( Importing { stored = 0, skipped = 0, errors = 0, error = Nothing }
                        , Ports.startContactCsvImport
                            options.skipRows
                            (List.map (\( index, field ) -> ( index, fieldName field )) options.mapping)
                            options.overwrite
                            (tagList options.tags)
                            |> Effect.sendCmd
                        )

                    _ ->
                        ( state, Effect.none )


view : ContactCsvImportDialog msg -> Html msg
view (Settings settings) =
    let
        (Model state) =
            settings.model

        translations =
            settings.browserEnv.translations

        dialog title body =
            wideDialog settings.theme title body CloseDialog
                |> Html.map settings.toMsg
    in
    case state of
        Hidden ->
            Html.text ""

        Choosing ->
            dialog (ImportTranslations.dialogTitle [ translations ])
                [ column
                    [ text <| Translations.csvChooseFileText [ translations ]
                    , buttons settings.theme
                        [ secondary settings.theme (ImportTranslations.closeButtonTitle [ translations ]) CloseDialog
                        , primary settings.theme (Translations.csvChooseFileText [ translations ]) (Just ChooseFile)
                        ]
                    ]
                ]

        Reading ->
            dialog (ImportTranslations.dialogTitle [ translations ])
                [ text <| Translations.csvReadingFileText [ translations ] ]

        Configure preview ->
            dialog (ImportTranslations.configureCsvImportDialogTitle [ translations ])
                [ viewConfigure settings preview ]

        Map mapping ->
            dialog (ImportTranslations.mapCsvFieldsDialogTitle [ translations ])
                [ viewMapping settings mapping ]

        Options options ->
            dialog (ImportTranslations.dialogTitle [ translations ])
                [ viewOptions settings options ]

        Importing progress ->
            dialog (ImportTranslations.dialogTitle [ translations ])
                [ text <| Translations.csvImportProgressText [ translations ] { stored = String.fromInt progress.stored } ]

        Finished progress ->
            dialog (ImportTranslations.dialogTitle [ translations ])
                [ viewFinished settings progress ]


viewConfigure : { model : Model, toMsg : Msg -> msg, browserEnv : BrowserEnv, theme : Theme } -> PreviewData -> Html Msg
viewConfigure settings preview =
    column
        [ div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2, Tw.items_center ] ]
            [ label [ Attr.for "contact-csv-skip-rows" ]
                [ text <| ImportTranslations.skipRowsFieldLabel [ settings.browserEnv.translations ] ]
            , input
                [ Attr.id "contact-csv-skip-rows"
                , Attr.type_ "number"
                , Attr.min "0"
                , Attr.max <| String.fromInt (max 0 (List.length preview.rows - 1))
                , Attr.value <| String.fromInt preview.skipRows
                , Events.onInput UpdateSkippedRows
                ]
                []
            ]
        , div [ css [ Tw.w_full, Tw.overflow_x_auto ] ]
            [ previewTable preview ]
        , buttons settings.theme
            [ secondary settings.theme (ImportTranslations.closeButtonTitle [ settings.browserEnv.translations ]) CloseDialog
            , primary settings.theme (ImportTranslations.nextButtonTitle [ settings.browserEnv.translations ]) (Just ContinueToMapping)
            ]
        ]


viewMapping : { model : Model, toMsg : Msg -> msg, browserEnv : BrowserEnv, theme : Theme } -> MappingData -> Html Msg
viewMapping settings mapping =
    let
        emailMissing =
            selectedMapping mapping.dropdowns
                |> List.any (\( _, field ) -> field == FieldEmail)
                |> not
    in
    column
        [ div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.w_full ] ]
            (List.indexedMap (viewMappingRow settings mapping) mapping.header)
        , if emailMissing then
            div [ css [ Tw.text_color TwTheme.red_500 ] ]
                [ text <| ImportTranslations.emailNotMappedErrorMessage [ settings.browserEnv.translations ] ]

          else
            Html.text ""
        , buttons settings.theme
            [ secondary settings.theme (ImportTranslations.closeButtonTitle [ settings.browserEnv.translations ]) CloseDialog
            , primary settings.theme
                (ImportTranslations.nextButtonTitle [ settings.browserEnv.translations ])
                (if emailMissing then
                    Nothing

                 else
                    Just ContinueToOptions
                )
            ]
        ]


viewMappingRow : { model : Model, toMsg : Msg -> msg, browserEnv : BrowserEnv, theme : Theme } -> MappingData -> Int -> String -> Html Msg
viewMappingRow settings mapping index columnName =
    let
        key =
            String.fromInt index
    in
    div [ css [ Tw.flex, Tw.flex_row, Tw.gap_4, Tw.items_center, Tw.w_full ] ]
        [ div [ css [ Tw.w_48, Tw.shrink_0 ] ] [ text columnName ]
        , div [ css [ Tw.flex_1, Tw.min_w_0 ] ]
            [ case Dict.get key mapping.dropdowns of
                Just dropdownModel ->
                    Dropdown.new
                    { model = dropdownModel
                    , toMsg = MappingDropdownSent key
                    , choices = availableFields mapping.dropdowns dropdownModel
                    , allowNoSelection = True
                    , toLabel =
                        \maybeField ->
                            case maybeField of
                                Just field ->
                                    Subscribers.translatedFieldName settings.browserEnv.translations field

                                Nothing ->
                                    ImportTranslations.unmappedColumnDropdownValue [ settings.browserEnv.translations ]
                    }
                    |> Dropdown.view

                Nothing ->
                    Html.text ""
            ]
        ]


viewOptions : { model : Model, toMsg : Msg -> msg, browserEnv : BrowserEnv, theme : Theme } -> OptionsData -> Html Msg
viewOptions settings options =
    column
        [ Checkbox.new
            { label = ImportTranslations.overwriteExistingCheckboxLabel [ settings.browserEnv.translations ]
            , onClick = OverwriteExistingClicked
            , checked = options.overwrite
            , theme = settings.theme
            }
            |> Checkbox.view
        , EntryField.new
            { value = options.tags
            , onInput = UpdateTags
            , theme = settings.theme
            }
            |> EntryField.withLabel (ImportTranslations.tagsFieldLabel [ settings.browserEnv.translations ])
            |> EntryField.view
        , buttons settings.theme
            [ secondary settings.theme (ImportTranslations.closeButtonTitle [ settings.browserEnv.translations ]) CloseDialog
            , primary settings.theme (Translations.startImportButtonTitle [ settings.browserEnv.translations ]) (Just StartImport)
            ]
        ]


viewFinished : { model : Model, toMsg : Msg -> msg, browserEnv : BrowserEnv, theme : Theme } -> ImportProgress -> Html Msg
viewFinished settings progress =
    column
        [ case progress.error of
            Just error ->
                div [ css [ Tw.text_color TwTheme.red_500 ] ]
                    [ text <| Translations.csvImportErrorText [ settings.browserEnv.translations ] { error = error } ]

            Nothing ->
                if progress.stored == 0 && progress.skipped == 0 then
                    text <| Translations.csvPreviewEmptyText [ settings.browserEnv.translations ]

                else
                    text <|
                        Translations.csvImportDoneText
                            [ settings.browserEnv.translations ]
                            { stored = String.fromInt progress.stored, skipped = String.fromInt progress.skipped }
        , buttons settings.theme
            [ secondary settings.theme (ImportTranslations.closeButtonTitle [ settings.browserEnv.translations ]) CloseDialog ]
        ]


previewTable : PreviewData -> Html Msg
previewTable preview =
    table [ css [ Tw.text_sm, Tw.border_collapse ] ]
        [ thead []
            [ tr []
                (preview.rows
                    |> List.head
                    |> Maybe.withDefault []
                    |> List.indexedMap
                        (\index _ ->
                            th [ css [ Tw.px_2, Tw.py_1, Tw.text_left ] ]
                                [ text <| " " ++ String.fromInt (index + 1) ]
                        )
                )
            ]
        , tbody []
            (List.indexedMap (previewRow preview.skipRows (columnCount preview.rows)) preview.rows)
        ]


previewRow : Int -> Int -> Int -> List String -> Html Msg
previewRow skipRows columns rowIndex row =
    tr
        [ css <|
            if rowIndex < skipRows then
                [ Tw.line_through, Tw.opacity_50 ]

            else if rowIndex == skipRows then
                [ Tw.font_semibold ]

            else
                []
        ]
        (List.range 0 (columns - 1)
            |> List.map
                (\index ->
                    td [ css [ Tw.px_2, Tw.py_1, Tw.border ] ]
                        [ text <| Maybe.withDefault "" (listAt index row) ]
                )
        )


column : List (Html msg) -> Html msg
column children =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_4, Tw.w_full ] ] children


wideDialog : Theme -> String -> List (Html msg) -> msg -> Html msg
wideDialog theme title content onClose =
    let
        styles =
            stylesForTheme theme
    in
    div
        (css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.z_50
            , Tw.overflow_y_auto
            , Tw.bg_opacity_50
            ]
            :: styles.colorStyleBackground
        )
        [ div
            [ css
                [ Tw.flex
                , Tw.min_h_full
                , Tw.items_center
                , Tw.justify_center
                , Tw.p_8
                ]
            ]
            [ div
                (styles.colorStyleBackground
                    ++ [ css
                            [ Tw.rounded_lg
                            , Tw.shadow_lg
                            , Tw.w_full
                            , Tw.max_w_3xl
                            , Tw.p_8
                            , Tw.flex
                            , Tw.flex_col
                            , Tw.gap_4
                            ]
                       ]
                )
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.justify_between
                        , Tw.items_center
                        , Tw.border_b
                        , Tw.pb_4
                        , Tw.gap_4
                        ]
                    ]
                    [ h2
                        [ css
                            [ Tw.text_lg
                            , Tw.font_semibold
                            , Tw.text_color styles.colorB4
                            , darkMode [ Tw.text_color styles.colorB4DarkMode ]
                            ]
                        ]
                        [ text title ]
                    , button
                        ([ Attr.type_ "button"
                         , Events.onClick onClose
                         , css
                            [ Tw.cursor_pointer
                            , Css.backgroundColor Css.transparent
                            , Tw.border_0
                            ]
                         ]
                            ++ styles.colorStyleGrayscaleText
                        )
                        [ text " ✕ " ]
                    ]
                , div [ css [ Tw.w_full ] ] content
                ]
            ]
        ]


buttons : Theme -> List (Html msg) -> Html msg
buttons _ children =
    div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2 ] ] children


primary : Theme -> String -> Maybe Msg -> Html Msg
primary theme label onClick =
    Button.new { label = label, onClick = onClick, theme = theme }
        |> Button.withTypePrimary
        |> Button.withDisabled (onClick == Nothing)
        |> Button.view


secondary : Theme -> String -> Msg -> Html Msg
secondary theme label msg =
    Button.new { label = label, onClick = Just msg, theme = theme }
        |> Button.withTypeSecondary
        |> Button.view


mappingDropdowns : List String -> Dict String (Dropdown.Model SubscriberField)
mappingDropdowns header =
    let
        automatic =
            Subscribers.buildColumnNameMap header
    in
    header
        |> List.indexedMap
            (\index name ->
                ( String.fromInt index
                , Dropdown.init { selected = Dict.get name automatic }
                )
            )
        |> Dict.fromList


selectedMapping : Dict String (Dropdown.Model SubscriberField) -> List ( Int, SubscriberField )
selectedMapping dropdowns =
    dropdowns
        |> Dict.toList
        |> List.filterMap
            (\( key, dropdown ) ->
                Maybe.map2 Tuple.pair (String.toInt key) (Dropdown.selectedItem dropdown)
            )
        |> List.sortBy Tuple.first


availableFields : Dict String (Dropdown.Model SubscriberField) -> Dropdown.Model SubscriberField -> List SubscriberField
availableFields dropdowns current =
    let
        taken =
            dropdowns
                |> Dict.values
                |> List.filterMap Dropdown.selectedItem
                |> List.filter (\field -> Just field /= Dropdown.selectedItem current)
    in
    List.filter (\field -> not (List.member field taken)) Subscribers.allSubscriberFields


headerRow : PreviewData -> Maybe (List String)
headerRow preview =
    preview.rows
        |> List.drop preview.skipRows
        |> List.head
        |> Maybe.andThen
            (\row ->
                if List.all (\cell -> String.trim cell == "") row then
                    Nothing

                else
                    Just row
            )


headerSkip : List (List String) -> Int
headerSkip rows =
    rows
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( index, row ) ->
                if List.any isEmailHeader row then
                    Just index

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault 0


isEmailHeader : String -> Bool
isEmailHeader value =
    case String.toLower (String.trim value) of
        "email" ->
            True

        "emailaddress" ->
            True

        "e-mail" ->
            True

        _ ->
            False


columnCount : List (List String) -> Int
columnCount rows =
    rows
        |> List.map List.length
        |> List.maximum
        |> Maybe.withDefault 0


listAt : Int -> List String -> Maybe String
listAt index values =
    values
        |> List.drop index
        |> List.head


tagList : String -> List String
tagList tags =
    tags
        |> String.split ","
        |> List.map String.trim
        |> List.filter (\tag -> tag /= "")


previewDecoder : Decode.Decoder PreviewResult
previewDecoder =
    Decode.oneOf
        [ Decode.field "error" Decode.string |> Decode.map PreviewError
        , Decode.field "cancelled" Decode.bool
            |> Decode.andThen
                (\cancelled ->
                    if cancelled then
                        Decode.succeed PreviewCancelled

                    else
                        Decode.fail "not cancelled"
                )
        , Decode.field "rows" (Decode.list (Decode.list Decode.string)) |> Decode.map PreviewRows
        ]


progressDecoder : Decode.Decoder { stored : Int, skipped : Int, errors : Int, done : Bool, error : Maybe String }
progressDecoder =
    Decode.map5
        (\stored skipped errors done error ->
            { stored = stored, skipped = skipped, errors = errors, done = done, error = error }
        )
        (Decode.field "stored" Decode.int)
        (Decode.field "skipped" Decode.int)
        (Decode.field "errors" Decode.int)
        (Decode.field "done" Decode.bool)
        (Decode.maybe (Decode.field "error" Decode.string))
