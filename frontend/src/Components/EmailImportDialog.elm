module Components.EmailImportDialog exposing (EmailImportDialog, Model, Msg, hide, init, new, show, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Csv as CsvParser
import Effect exposing (Effect)
import Email
import File exposing (File)
import File.Select as FileSelect
import Html.Styled as Html exposing (Html, div, text, textarea)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey)
import Process
import Shared.Model exposing (Model)
import Subscribers exposing (CsvColumnNameMap, Modification(..), Subscriber, SubscriberField(..), emptySubscriber, translatedFieldName)
import Table.Paginated as Table exposing (defaultCustomizations)
import Tailwind.Utilities as Tw
import Task exposing (Task)
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
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | CsvParsed (Result ProcessingError CsvData)
    | CsvProcessed (Result ProcessingError (List Subscriber))


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
    = CsvParsingError
    | CsvMappingError


type alias EmailImportData =
    { enteredEmails : String
    }


type alias CsvMappingData =
    { csvData : CsvData
    , mappingTable : Table.State
    }


type alias CsvData =
    List (List String)


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
                                                |> (\subscriber -> { subscriber | source = Just "manual", dateSubscription = Just props.browserEnv.now })
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
                ( Model { model | state = DialogProcessing "Parsing CSV file" }
                , Task.attempt CsvParsed (parseCsvTask content)
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            CsvParsed csvParsingResult ->
                case csvParsingResult of
                    Ok csvData ->
                        let
                            csvColumnCount =
                                csvData
                                    |> List.head
                                    |> Maybe.map List.length
                                    |> Maybe.withDefault 0
                        in
                        ( Model
                            { model
                                | state =
                                    DialogCsvMapping
                                        { csvData = csvData
                                        , mappingTable =
                                            Table.initialState "field" 25
                                                |> Table.setTotal csvColumnCount
                                        }
                            }
                        , Task.attempt CsvProcessed (mapCsvTask Subscribers.substackCsvColumnNameMap csvData)
                            |> Cmd.map props.toMsg
                            |> Effect.sendCmd
                        )

                    Err error ->
                        ( Model { model | state = DialogCsvProcessingError error }
                        , Effect.none
                        )

            CsvProcessed processingResult ->
                case processingResult of
                    Ok subscribers ->
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

                    Err error ->
                        ( Model { model | state = DialogCsvProcessingError error }, Effect.none )


parseCsvTask : String -> Task ProcessingError CsvData
parseCsvTask content =
    case CsvParser.parseRows content of
        Ok csvData ->
            Task.succeed csvData

        Err _ ->
            Task.fail CsvParsingError


mapCsvTask : CsvColumnNameMap -> CsvData -> Task ProcessingError (List Subscriber)
mapCsvTask csvColumnNameMap csvData =
    List.head csvData
        |> Maybe.map (Subscribers.buildCsvColumnIndexMap csvColumnNameMap)
        |> Maybe.map
            (\csvColumnIndexMap ->
                csvData
                    -- skip column names
                    |> List.drop 1
                    |> List.filterMap (Subscribers.buildSubscriberFromCsvRecord csvColumnIndexMap)
                    |> Task.succeed
            )
        |> Maybe.withDefault (Task.fail CsvMappingError)


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
                |> Button.withDisabled (settings.browserEnv.environment == BrowserEnv.Production)
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
    -- TODO: Implement CSV mapping table here...
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
        [{- subscribersSection (Settings settings) data.subscribers
            , Checkbox.new
                { label = "Overwrite existing?"
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
         -}
        ]


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
        CsvParsingError ->
            "Error parsing CSV file"

        CsvMappingError ->
            "Error mapping CSV file"


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
viewProcessingDialog (Settings settings) processingMessage =
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
        []
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
