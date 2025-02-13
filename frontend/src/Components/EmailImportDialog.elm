module Components.EmailImportDialog exposing (EmailImportDialog, Model, Msg, hide, init, new, show, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.Dropdown as Dropdown
import Csv as CsvParser
import Dict exposing (Dict)
import Effect exposing (Effect)
import Email
import File exposing (File)
import File.Select as FileSelect
import Html.Styled as Html exposing (Html, div, text, textarea)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import I18Next
import List.Extra
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey)
import Shared.Model exposing (Model)
import Subscribers exposing (CsvColumnNameMap, CsvData, Modification(..), Subscriber, SubscriberField(..), emptySubscriber, translatedFieldName)
import Table.Paginated as Table exposing (defaultCustomizations)
import Tailwind.Utilities as Tw
import Task exposing (Task)
import Time
import Translations.EmailImportDialog as Translations
import Ui.Shared
import Ui.Styles exposing (Theme)


type Msg msg
    = CloseDialog
    | ConfigureRelaysClicked
    | ImportSingleEmails
    | ImportClicked (List Subscriber) Bool
    | OverwriteExistingClicked Bool
    | UpdateEmailData String
    | ProcessEnteredData String
    | NewTableState Table.State
    | NewMappingTableState Table.State
    | MappingDropdownSent String (Dropdown.Msg SubscriberField (Msg msg))
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | CsvParsed (Result ProcessingError CsvData)
    | CsvProcess (List String) CsvData CsvColumnNameMap
    | CsvProcessed (List Subscriber)


type Model
    = Model
        { state : DialogState
        }


type DialogState
    = DialogHidden
    | DialogSelection
    | DialogImport EmailImportData
    | DialogCsvMapping CsvMappingData
    | DialogCsvProcessingError ProcessingError
    | DialogProcessed EmailProcessedData
    | DialogProcessing String


type ProcessingError
    = CsvParsingError String


type alias EmailImportData =
    { enteredEmails : String
    }


type alias CsvMappingData =
    { -- CSV data without header
      csvData : CsvData
    , header : List String
    , mappingTable : Table.State
    , mappingSelectionDropdowns : Dict String (Dropdown.Model SubscriberField)
    }


type alias EmailProcessedData =
    { subscribers : List Subscriber
    , subscriberTable : Table.State
    , overwriteExisting : Bool
    }


type EmailImportDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> EmailImportDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , nostr = props.nostr
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model
init _ =
    Model
        { state = DialogHidden
        }


show : Model -> Model
show (Model model) =
    Model { model | state = DialogSelection }


hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , onImport : List Subscriber -> Bool -> msg
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , pubKey : PubKey
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            ConfigureRelaysClicked ->
                ( Model model
                , Effect.none
                )

            OverwriteExistingClicked overwriteExisting ->
                case model.state of
                    DialogProcessed emailProcessedData ->
                        let
                            dataWithUpdate =
                                { emailProcessedData | overwriteExisting = overwriteExisting }
                        in
                        ( Model { model | state = DialogProcessed dataWithUpdate }
                        , Effect.none
                        )

                    _ ->
                        ( Model model, Effect.none )

            ImportSingleEmails ->
                ( Model { model | state = DialogImport { enteredEmails = "" } }
                , Effect.none
                )

            ImportClicked subscribers overwriteExisting ->
                ( Model model
                , Effect.sendMsg <| props.onImport subscribers overwriteExisting
                )

            UpdateEmailData enteredEmails ->
                case model.state of
                    DialogImport emailImportData ->
                        let
                            dataWithUpdate =
                                { emailImportData | enteredEmails = enteredEmails }
                        in
                        ( Model { model | state = DialogImport dataWithUpdate }
                        , Effect.none
                        )

                    _ ->
                        ( Model model, Effect.none )

            ProcessEnteredData emailData ->
                let
                    subscribers =
                        emailData
                            |> String.split ","
                            |> List.map String.trim
                            |> List.map Email.parse
                            |> List.filterMap
                                (\parsingResult ->
                                    case parsingResult of
                                        Ok email ->
                                            email
                                                |> Email.toString
                                                |> emptySubscriber
                                                |> (\subscriber -> { subscriber | source = Just "manual", dateSubscription = props.browserEnv.now })
                                                |> Just

                                        Err _ ->
                                            Nothing
                                )

                    processedData =
                        { subscribers = subscribers
                        , subscriberTable =
                            Table.initialState (Subscribers.fieldName FieldEmail) 25
                                |> Table.setTotal (List.length subscribers)
                        , overwriteExisting = False
                        }
                in
                ( Model { model | state = DialogProcessed processedData }, Effect.none )

            NewTableState tableState ->
                case model.state of
                    DialogProcessed processedData ->
                        ( Model { model | state = DialogProcessed { processedData | subscriberTable = tableState } }, Effect.none )

                    _ ->
                        ( Model model, Effect.none )

            NewMappingTableState tableState ->
                case model.state of
                    DialogCsvMapping mappingData ->
                        ( Model { model | state = DialogCsvMapping { mappingData | mappingTable = tableState } }, Effect.none )

                    _ ->
                        ( Model model, Effect.none )

            MappingDropdownSent columnName innerMsg ->
                case model.state of
                    DialogCsvMapping mappingData ->
                        case Dict.get columnName mappingData.mappingSelectionDropdowns of
                            Just dropdownModel ->
                                let
                                    ( newModel, effect ) =
                                        Dropdown.update
                                            { msg = innerMsg
                                            , model = dropdownModel
                                            , toModel = \dropdown -> Model { model | state = DialogCsvMapping { mappingData | mappingSelectionDropdowns = Dict.insert columnName dropdown mappingData.mappingSelectionDropdowns } }
                                            , toMsg = MappingDropdownSent columnName
                                            }
                                in
                                ( newModel, Effect.map props.toMsg effect )

                            Nothing ->
                                -- this case shouldn't occur as there should be a dropdown listbox model for each column
                                ( Model model, Effect.none )

                    _ ->
                        ( Model model, Effect.none )

            CsvRequested ->
                ( Model model
                , FileSelect.file [ "text/csv" ] CsvSelected
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            CsvSelected file ->
                ( Model model
                , Task.perform CsvLoaded (File.toString file)
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            CsvLoaded content ->
                ( Model { model | state = DialogProcessing <| Translations.csvParsingStatusMessage [ props.browserEnv.translations ] }
                , Task.attempt CsvParsed (parseCsvTask content)
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            CsvParsed csvParsingResult ->
                case csvParsingResult of
                    -- separate header from data
                    Ok (header :: csvData) ->
                        let
                            csvColumnCount =
                                header
                                    |> List.length
                        in
                        ( Model
                            { model
                                | state =
                                    DialogCsvMapping
                                        { csvData = csvData
                                        , header = header
                                        , mappingTable =
                                            Table.initialState (mappingColumnName MappingTableColumnName) csvColumnCount
                                                |> Table.setTotal csvColumnCount
                                        , mappingSelectionDropdowns =
                                            let
                                                columnNameMap =
                                                    Subscribers.buildColumnNameMap header
                                            in
                                            -- create a dropdown listbox for each CSV column
                                            header
                                                |> List.map
                                                    (\fieldName ->
                                                        ( fieldName, Dropdown.init { selected = Dict.get fieldName columnNameMap } )
                                                    )
                                                |> Dict.fromList
                                        }
                            }
                        , Effect.none
                        )

                    Ok [] ->
                        ( Model { model | state = DialogCsvProcessingError (CsvParsingError <| Translations.csvFileEmptyErrorMessage [ props.browserEnv.translations ]) }
                        , Effect.none
                        )

                    Err error ->
                        ( Model { model | state = DialogCsvProcessingError error }
                        , Effect.none
                        )

            CsvProcess header csvData csvColumnNameMap ->
                ( Model
                    { model | state = DialogProcessing <| Translations.csvProcessingStatusMessage [ props.browserEnv.translations ] }
                , Task.perform CsvProcessed (mapCsvTask props.browserEnv.now csvColumnNameMap header csvData)
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            CsvProcessed subscribers ->
                let
                    processedData =
                        { subscribers = subscribers
                        , subscriberTable =
                            Table.initialState (Subscribers.fieldName FieldEmail) 25
                                |> Table.setTotal (List.length subscribers)
                        , overwriteExisting = False
                        }
                in
                ( Model
                    { model
                        | state = DialogProcessed processedData
                    }
                , Effect.none
                )


parseCsvTask : String -> Task ProcessingError CsvData
parseCsvTask content =
    case CsvParser.parseRows content of
        Ok csvData ->
            Task.succeed csvData

        Err error ->
            Task.fail (CsvParsingError error)


mapCsvTask : Time.Posix -> CsvColumnNameMap -> List String -> CsvData -> Task Never (List Subscriber)
mapCsvTask now csvColumnNameMap header csvData =
    let
        csvColumnIndexMap =
            header
                |> Subscribers.buildCsvColumnIndexMap csvColumnNameMap
    in
    csvData
        |> List.filterMap (Subscribers.buildSubscriberFromCsvRecord now csvColumnIndexMap)
        |> Task.succeed


view : EmailImportDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            div [] []

        DialogSelection ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewSelectionDialog dialog ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogImport emailImportData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewImportDialog dialog emailImportData ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogCsvMapping csvMappingData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewCsvMappingDialog dialog csvMappingData ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogCsvProcessingError error ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewCsvProcessingErrorDialog dialog error ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogProcessed emailProcessedData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewProcessedDialog dialog emailProcessedData ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogProcessing message ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewProcessingDialog dialog message ]
                CloseDialog
                |> Html.map settings.toMsg


viewSelectionDialog : EmailImportDialog msg -> Html (Msg msg)
viewSelectionDialog (Settings settings) =
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ div [ css [ Tw.my_2 ] ] [ text <| Translations.importSelectionExplanation [ settings.browserEnv.translations ] ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.singleEmailsButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just ImportSingleEmails
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            , Button.new
                { label = Translations.uploadCsvButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| CsvRequested
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


viewImportDialog : EmailImportDialog msg -> EmailImportData -> Html (Msg msg)
viewImportDialog (Settings settings) data =
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ textarea
            [ css
                [ Tw.p_2
                , Tw.w_80
                , Tw.h_40
                ]
            , Attr.value data.enteredEmails
            , Attr.placeholder <| Translations.emailTextareaPlaceholderText [ settings.browserEnv.translations ]
            , Events.onInput UpdateEmailData
            ]
            []
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.nextButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| ProcessEnteredData data.enteredEmails
                , theme = settings.theme
                }
                |> Button.withDisabled (data.enteredEmails == "")
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


viewCsvMappingDialog : EmailImportDialog msg -> CsvMappingData -> Html (Msg msg)
viewCsvMappingDialog (Settings settings) data =
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            , Tw.w_auto
            , Tw.min_w_80
            , Tw.max_h_96
            , Tw.overflow_auto
            ]
        ]
        [ div
            [ css
                [ Tw.max_w_96
                ]
            ]
            [ Table.view
                (mappingTableConfig settings.browserEnv.translations data)
                data.mappingTable
                data.header
                |> Html.fromUnstyled
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.processButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| CsvProcess data.header data.csvData (buildColumnNameMap data)
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]



-- derive column name mapping from state stored in dropdown listboxes


buildColumnNameMap : CsvMappingData -> CsvColumnNameMap
buildColumnNameMap csvMappingData =
    csvMappingData.mappingSelectionDropdowns
        |> Dict.toList
        |> List.filterMap
            (\( fieldName, dropdown ) ->
                Dropdown.selectedItem dropdown
                    |> Maybe.map
                        (\selectedField ->
                            ( fieldName, selectedField )
                        )
            )
        |> Dict.fromList


mappingTableConfig : I18Next.Translations -> CsvMappingData -> Table.Config String (Msg msg)
mappingTableConfig translations csvMappingData =
    Table.customConfig
        { toId = identity
        , toMsg = NewMappingTableState
        , columns =
            [ Table.customColumn
                { id = mappingColumnName MappingTableColumnName
                , name = translatedMappingColumnName translations MappingTableColumnName
                , viewData = identity
                , sorter = Table.unsortable
                }
            , fieldSelectionDropdownColumn translations csvMappingData
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = Table.defaultCustomizations.tableAttrs
            }
        }


fieldSelectionDropdownColumn : I18Next.Translations -> CsvMappingData -> Table.Column String (Msg msg)
fieldSelectionDropdownColumn translations csvMappingData =
    { id = mappingColumnName MappingTableColumnField
    , name = translatedMappingColumnName translations MappingTableColumnField
    , viewData = viewFieldSelectionDropdown translations csvMappingData.mappingSelectionDropdowns
    , sorter = Table.unsortable
    }
        |> Table.veryCustomColumn


viewFieldSelectionDropdown : I18Next.Translations -> Dict String (Dropdown.Model SubscriberField) -> String -> Table.HtmlDetails (Msg msg)
viewFieldSelectionDropdown translations mappingSelectionDropdowns rowValue =
    case Dict.get rowValue mappingSelectionDropdowns of
        Just dropdownModel ->
            let
                -- a field should only be selected by one dropdown listbox.
                fieldsSelectedInOtherDropdownListboxes =
                    mappingSelectionDropdowns
                        |> Dict.values
                        |> List.filterMap Dropdown.selectedItem
                        |> List.filter (\field -> Just field /= Dropdown.selectedItem dropdownModel)

                availableChoices =
                    fieldsSelectedInOtherDropdownListboxes
                        |> List.foldl
                            (\availableInOther acc ->
                                List.Extra.remove availableInOther acc
                            )
                            Subscribers.allSubscriberFields
            in
            [ div
                [ css
                    [ Tw.ml_2
                    ]
                ]
                [ Dropdown.new
                    { model = dropdownModel
                    , toMsg = MappingDropdownSent rowValue
                    , choices = availableChoices
                    , allowNoSelection = True
                    , toLabel =
                        \maybeField ->
                            maybeField
                                |> Maybe.map (Subscribers.translatedFieldName translations)
                                |> Maybe.withDefault (Translations.unmappedColumnDropdownValue [ translations ])
                    }
                    |> Dropdown.view
                ]
                |> Html.toUnstyled
            ]
                |> Table.HtmlDetails []

        Nothing ->
            [ div []
                []
                |> Html.toUnstyled
            ]
                |> Table.HtmlDetails []



{-
   dropdownChoices : List
                   (\columnName ->
                       columnNameMap
                           |> Dict.get columnName
                           |> Maybe.map (Subscribers.translatedFieldName translations)
                           |> Maybe.withDefault "<unmapped>"
                   )
-}


type MappingTableColumn
    = MappingTableColumnName
    | MappingTableColumnField


mappingColumnName : MappingTableColumn -> String
mappingColumnName column =
    case column of
        MappingTableColumnName ->
            "columnName"

        MappingTableColumnField ->
            "subscriberField"


translatedMappingColumnName : I18Next.Translations -> MappingTableColumn -> String
translatedMappingColumnName translations column =
    case column of
        MappingTableColumnName ->
            Translations.nameColumnHeaderInCsvColumnMappingTable [ translations ]

        MappingTableColumnField ->
            Translations.fieldColumnHeaderInCsvColumnMappingTable [ translations ]


viewCsvProcessingErrorDialog : EmailImportDialog msg -> ProcessingError -> Html (Msg msg)
viewCsvProcessingErrorDialog (Settings settings) error =
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            , Tw.w_auto
            , Tw.min_w_80
            ]
        ]
        [ text <| Translations.csvProcessingErrorMessage [ settings.browserEnv.translations ] ++ processingErrorToString error
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            ]
        ]


processingErrorToString : ProcessingError -> String
processingErrorToString error =
    case error of
        CsvParsingError message ->
            message


viewProcessedDialog : EmailImportDialog msg -> EmailProcessedData -> Html (Msg msg)
viewProcessedDialog (Settings settings) data =
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            , Tw.w_auto
            , Tw.min_w_80
            ]
        ]
        [ subscribersSection (Settings settings) data
        , Checkbox.new
            { label = Translations.overwriteExistingCheckboxLabel [ settings.browserEnv.translations ]
            , onClick = OverwriteExistingClicked
            , checked = data.overwriteExisting
            , theme = settings.theme
            }
            |> Checkbox.view
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                , Tw.mt_3
                ]
            ]
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.importButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| ImportClicked data.subscribers data.overwriteExisting
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


viewProcessingDialog : EmailImportDialog msg -> String -> Html (Msg msg)
viewProcessingDialog _ processingMessage =
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            , Tw.w_auto
            , Tw.min_w_80
            ]
        ]
        [ text processingMessage
        ]


subscribersSection : EmailImportDialog msg -> EmailProcessedData -> Html (Msg msg)
subscribersSection (Settings settings) data =
    div
        [ css
            [ Tw.max_h_96
            , Tw.overflow_auto
            ]
        ]
        [ Table.view
            (subscribersTableConfig settings.browserEnv.translations)
            data.subscriberTable
            data.subscribers
            |> Html.fromUnstyled
        ]


subscribersTableConfig : I18Next.Translations -> Table.Config Subscriber (Msg msg)
subscribersTableConfig translations =
    Table.customConfig
        { toId = .email
        , toMsg = NewTableState
        , columns =
            [ Table.stringColumn (Subscribers.fieldName FieldEmail) (translatedFieldName translations FieldEmail) .email
            , Table.stringColumn (Subscribers.fieldName FieldName) (translatedFieldName translations FieldName) (\subscriber -> subscriber.name |> Maybe.withDefault "")
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = Table.defaultCustomizations.tableAttrs
            }
        }
