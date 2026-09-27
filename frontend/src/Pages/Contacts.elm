module Pages.Contacts exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Categories as Categories
import Components.ConfirmDialog as ConfirmDialog
import Components.ContactCsvImportDialog as ContactCsvImportDialog
import Components.TagCombination as TagCombination
import Components.EntryField as EntryField
import Components.Icon as Icon
import Components.SearchBar as SearchBar
import Components.SubscriberEditDialog as SubscriberEditDialog
import Components.Switch as Switch
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html as Unstyled
import Html.Attributes as UnstyledAttr
import Html.Events as UnstyledEvents
import Html.Styled as Html exposing (Html, div, li, p, text, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import I18Next
import Json.Decode as Decode
import Layouts
import Layouts.Sidebar
import Newsletters.ContactDatabase as ContactDatabase
import Newsletters.Subscribers as Subscribers exposing (Email, Modification(..), RecipientSource(..), translatedFieldName)
import Newsletters.Types exposing (Subscriber, SubscriberField(..), fieldName)
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Request exposing (RequestId)
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Route.Path
import Set exposing (Set)
import Shared
import Svg.Loaders as Loaders
import Table.Paginated as Table exposing (defaultCustomizations)
import Tailwind.Theme exposing (Color)
import Tailwind.Utilities as Tw
import Time exposing (Posix)
import Translations.Sidebar
import Translations.Subscribers as Translations
import Ui.Styles exposing (Theme(..), darkMode, stylesForTheme)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update user shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)
        |> Page.withOnQueryParameterChanged
            { key = categoryParamName
            , onChange = CategoryQueryChanged
            }


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    let
        topPart =
            Categories.new
                { model = model.categories
                , toMsg = CategoriesSent
                , onSelect = CategorySelected
                , equals = \category1 category2 -> category1 == category2
                , image = categoryImage
                , categories = availableCategories shared.browserEnv.translations
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> Categories.view
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withTopPart topPart Categories.heightString
        |> Layouts.Sidebar


type PageCategory
    = ContactsCategory
    | TagsCategory


categoryParamName : String
categoryParamName =
    "category"


availableCategories : I18Next.Translations -> List (Categories.CategoryData PageCategory)
availableCategories translations =
    [ { category = ContactsCategory
      , title = Translations.contactsCategory [ translations ]
      , testId = "contacts-category"
      }
    , { category = TagsCategory
      , title = Translations.tagsCategory [ translations ]
      , testId = "contacts-tags-category"
      }
    ]


categoryImage : Color -> PageCategory -> Maybe (Html msg)
categoryImage _ category =
    let
        icon =
            case category of
                ContactsCategory ->
                    FeatherIcons.users

                TagsCategory ->
                    FeatherIcons.tag
    in
    Icon.FeatherIcon icon
        |> Icon.viewWithSize 16
        |> Just


stringFromCategory : PageCategory -> String
stringFromCategory category =
    case category of
        ContactsCategory ->
            "contacts"

        TagsCategory ->
            "tags"


categoryFromString : String -> Maybe PageCategory
categoryFromString category =
    case category of
        "contacts" ->
            Just ContactsCategory

        "tags" ->
            Just TagsCategory

        _ ->
            Nothing


replaceCategoryRoute : PageCategory -> Effect msg
replaceCategoryRoute category =
    Effect.replaceRoute
        { path = Route.Path.Contacts
        , query = Dict.singleton categoryParamName (stringFromCategory category)
        , hash = Nothing
        }



-- INIT


type alias Model =
    { categories : Categories.Model PageCategory
    , confirmDialog : ConfirmDialog.Model String
    , contactDatabase : ContactDatabase.Model
    , csvExport : CsvExportState
    , csvImport : ContactCsvImportDialog.Model
    , cursorRequestId : RequestId
    , errors : List String
    , fileState : FileState
    , migration : MigrationState
    , modificationScan : ModificationScan
    , modifications : List Modification
    , modificationsRequestId : RequestId
    , pageCategory : PageCategory
    , pageEventCount : Int
    , pageOldest : Maybe Posix
    , pendingModifications : List Modification
    , previousPageOldest : Maybe Posix
    , scannedUntil : Maybe Posix
    , subscriptionCursor : Maybe Posix
    , requestId : RequestId
    , recipientSource : RecipientSource
    , subscriptionSync : SubscriptionSync
    , searchBar : SearchBar.Model
    , searchText : Maybe String
    , sortColumn : String
    , sortReversed : Bool
    , sourceRequestId : RequestId
    , subscriberEditDialog : SubscriberEditDialog.Model
    , subscriberTable : Table.State
    , tagCombination : TagCombination.Model
    , subscribers : Dict Email Subscriber
    , tagDraft : String
    }


type CsvExportState
    = ExportIdle
    | Exporting Int
    | ExportFinished Int (Maybe String)


type FileState
    = FileLoading
    | FileReady
    | FileMissing
    | FileError String


type MigrationState
    = MigrationIdle
    | MigrationRunning
    | MigrationDone
    | MigrationFailed String


type SubscriptionSync
    = SyncLoading
    | SyncWaiting
    | SyncChecking
    | SyncReady
    | SyncApplying
    | SyncFailed String


type ModificationScan
    = ScanWaitingForCursor
    | ScanPaging
    | ScanFinished


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        ( contactDatabase, contactDatabaseEffect ) =
            ContactDatabase.init user.pubKey shared.browserEnv.contactDatabaseServerUrl [ ContactDatabase.LoadTags ]

        requestId =
            Nostr.getLastRequestId shared.nostr

        pageCategory =
            Dict.get categoryParamName route.query
                |> Maybe.andThen categoryFromString
                |> Maybe.withDefault ContactsCategory
    in
    ( { categories = Categories.init { selected = pageCategory }
      , confirmDialog = ConfirmDialog.init
      , contactDatabase = contactDatabase
      , csvExport = ExportIdle
      , csvImport = ContactCsvImportDialog.init
      , cursorRequestId = requestId + 2
      , errors = []
      , fileState = FileLoading
      , migration = MigrationIdle
      , modificationScan = ScanWaitingForCursor
      , modifications = []
      , modificationsRequestId = -1
      , pageCategory = pageCategory
      , pageEventCount = 0
      , pageOldest = Nothing
      , pendingModifications = []
      , previousPageOldest = Nothing
      , scannedUntil = Nothing
      , subscriptionCursor = Nothing
      , requestId = requestId
      , recipientSource = SubscriberFile
      , subscriptionSync = SyncLoading
      , searchBar = SearchBar.init { searchText = Nothing }
      , searchText = Nothing
      , sortColumn = fieldName FieldEmail
      , sortReversed = False
      , sourceRequestId = requestId + 1
      , subscriberEditDialog = SubscriberEditDialog.init {}
      , subscriberTable = Table.initialState (fieldName FieldEmail) ContactDatabase.pageSize
      , tagCombination = TagCombination.init
      , subscribers = Dict.empty
      , tagDraft = ""
      }
    , Effect.batch
        [ contactDatabaseEffect
            |> Effect.map ContactDatabaseMsg
        , Subscribers.load shared.nostr user.pubKey
            |> Effect.sendSharedMsg
        , Subscribers.loadRecipientSource (requestId + 1) user.pubKey
            |> Effect.sendSharedMsg
        , Subscribers.loadSubscriptionCursor (requestId + 2) user.pubKey
            |> Effect.sendSharedMsg
        , replaceCategoryRoute pageCategory
        ]
    )



-- UPDATE


type Msg
    = ContactDatabaseMsg ContactDatabase.Msg
    | MigrateClicked
    | AddContactClicked
    | ImportCsvClicked
    | ExportCsvClicked
    | CsvImportSent ContactCsvImportDialog.Msg
    | NewTableState Table.State
    | ReceivedMessage IncomingMessage
    | SearchBarSent (SearchBar.Msg Msg)
    | SearchSubmitted (Maybe String)
    | SetRecipientSource RecipientSource
    | SortColumn String
    | OpenEditSubscriberDialog Subscriber
    | SubscriberEditDialogSent SubscriberEditDialog.Msg
    | UpdateSubscriber String Subscriber
    | TagCombinationSent TagCombination.Msg
    | CategorySelected PageCategory
    | CategoryQueryChanged { from : Maybe String, to : Maybe String }
    | CategoriesSent (Categories.Msg PageCategory Msg)
    | TagDraftChanged String
    | AddTagClicked
    | DeleteTagClicked String
    | ConfirmDialogSent ConfirmDialog.Msg
    | DeleteTagConfirmed String
    | ProcessSubscriptionEventsClicked


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ContactDatabaseMsg contactDatabaseMsg ->
            let
                wasAuthenticated =
                    model.contactDatabase.authenticated

                ( contactDatabase, contactDatabaseEffect ) =
                    ContactDatabase.update contactDatabaseMsg model.contactDatabase

                modelWithDatabase =
                    { model
                        | contactDatabase = contactDatabase
                        , subscriberTable =
                            case contactDatabase.total of
                                Just total ->
                                    Table.setTotal total model.subscriberTable

                                Nothing ->
                                    model.subscriberTable
                    }
            in
            if not wasAuthenticated && contactDatabase.authenticated then
                let
                    ( nextModel, fetchEffect ) =
                        goToPage 1 modelWithDatabase

                    ( checkedModel, checkEffect ) =
                        if model.subscriptionSync == SyncWaiting then
                            beginSubscriptionCheck nextModel

                        else
                            ( nextModel, Effect.none )
                in
                ( checkedModel
                , Effect.batch
                    [ Effect.map ContactDatabaseMsg contactDatabaseEffect
                    , fetchEffect
                    , checkEffect
                    ]
                )

            else
                ( modelWithDatabase
                , Effect.map ContactDatabaseMsg contactDatabaseEffect
                )

        AddContactClicked ->
            let
                blank =
                    Subscribers.emptySubscriber ""

                subscriber =
                    { blank
                        | source = Just "manual"
                        , dateSubscription = shared.browserEnv.now
                        , dnd = Just False
                    }
            in
            ( { model | subscriberEditDialog = SubscriberEditDialog.show model.subscriberEditDialog subscriber }
            , Effect.none
            )

        ImportCsvClicked ->
            ( { model | csvImport = ContactCsvImportDialog.begin model.csvImport }
            , Ports.pickContactCsv |> Effect.sendCmd
            )

        ExportCsvClicked ->
            ( { model | csvExport = Exporting 0 }
            , Ports.exportContactsCsv |> Effect.sendCmd
            )

        CsvImportSent innerMsg ->
            ContactCsvImportDialog.update
                { msg = innerMsg
                , model = model.csvImport
                , toModel = \csvImport -> { model | csvImport = csvImport }
                , toMsg = CsvImportSent
                , browserEnv = shared.browserEnv
                }

        MigrateClicked ->
            ( { model | migration = MigrationRunning }
            , Dict.values model.subscribers
                |> ContactDatabase.storeSubscribers
                |> Effect.map ContactDatabaseMsg
            )

        NewTableState tableState ->
            let
                oldPage =
                    Table.getCurrentPage model.subscriberTable

                newPage =
                    Table.getCurrentPage tableState
            in
            if newPage /= oldPage then
                goToPage newPage { model | subscriberTable = tableState }

            else
                ( { model | subscriberTable = tableState }, Effect.none )

        SearchBarSent innerMsg ->
            SearchBar.update
                { msg = innerMsg
                , model = model.searchBar
                , toModel = \searchBar -> { model | searchBar = searchBar }
                , toMsg = SearchBarSent
                , onSearch = SearchSubmitted
                }

        SearchSubmitted maybeText ->
            let
                searchText =
                    maybeText
                        |> Maybe.map String.trim
                        |> Maybe.andThen
                            (\term ->
                                if term == "" then
                                    Nothing

                                else
                                    Just term
                            )
            in
            if searchText == model.searchText then
                ( model, Effect.none )

            else
                goToPage 1 { model | searchText = searchText }

        SetRecipientSource source ->
            ( { model | recipientSource = source }
            , Subscribers.saveRecipientSource shared.browserEnv user.pubKey source
                |> Effect.sendSharedMsg
            )

        SortColumn column ->
            if not (sortableColumn column) then
                ( model, Effect.none )

            else
                let
                    next =
                        if column == model.sortColumn then
                            { model | sortReversed = not model.sortReversed }

                        else
                            { model | sortColumn = column, sortReversed = False }
                in
                goToPage 1 next

        OpenEditSubscriberDialog subscriber ->
            ( { model | subscriberEditDialog = SubscriberEditDialog.show model.subscriberEditDialog subscriber }, Effect.none )

        SubscriberEditDialogSent innerMsg ->
            SubscriberEditDialog.update
                { msg = innerMsg
                , model = model.subscriberEditDialog
                , toModel = \subscriberEditDialog -> { model | subscriberEditDialog = subscriberEditDialog }
                , toMsg = SubscriberEditDialogSent
                , submit = UpdateSubscriber
                }

        UpdateSubscriber email subscriber ->
            let
                contactDatabase =
                    model.contactDatabase

                trimmed =
                    { subscriber | email = String.trim subscriber.email }
            in
            case Dict.get email contactDatabase.contactIds of
                Just contactId ->
                    ( { model | contactDatabase = { contactDatabase | loading = True } }
                    , ContactDatabase.updateContact contactId trimmed
                        |> Effect.map ContactDatabaseMsg
                    )

                Nothing ->
                    if String.trim email == "" && Subscribers.emailValid trimmed.email then
                        ( { model | contactDatabase = { contactDatabase | loading = True } }
                        , ContactDatabase.addContact trimmed
                            |> Effect.map ContactDatabaseMsg
                        )

                    else
                        ( { model | errors = "Could not save this contact." :: model.errors }, Effect.none )

        TagCombinationSent innerMsg ->
            let
                previousFilter =
                    TagCombination.toFilter model.tagCombination

                ( tagCombination, effect ) =
                    TagCombination.update
                        { msg = innerMsg
                        , model = model.tagCombination
                        , toModel = identity
                        , toMsg = TagCombinationSent
                        }

                next =
                    { model | tagCombination = tagCombination }
            in
            if TagCombination.toFilter tagCombination /= previousFilter then
                let
                    ( paged, pageEffect ) =
                        goToPage 1 next
                in
                ( paged, Effect.batch [ effect, pageEffect ] )

            else
                ( next, effect )

        CategorySelected category ->
            switchCategory model category

        CategoryQueryChanged { to } ->
            switchCategory model (to |> Maybe.andThen categoryFromString |> Maybe.withDefault ContactsCategory)

        CategoriesSent innerMsg ->
            Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
                , toMsg = CategoriesSent
                }

        TagDraftChanged text ->
            ( { model | tagDraft = text }, Effect.none )

        AddTagClicked ->
            let
                tag =
                    String.trim model.tagDraft
            in
            if tag == "" || List.member tag model.contactDatabase.tags then
                ( model, Effect.none )

            else
                ( { model | tagDraft = "" }
                , ContactDatabase.addTag tag
                    |> Effect.map ContactDatabaseMsg
                )

        DeleteTagClicked tag ->
            let
                translations =
                    shared.browserEnv.translations
            in
            ( { model
                | confirmDialog =
                    ConfirmDialog.show
                        { context = tag
                        , title = Translations.deleteTagDialogTitle [ translations ]
                        , message = Translations.deleteTagDialogMessage [ translations ] { tag = tag }
                        , confirmLabel = Translations.deleteTagConfirmButtonTitle [ translations ]
                        , cancelLabel = Translations.deleteTagCancelButtonTitle [ translations ]
                        , danger = True
                        }
                        model.confirmDialog
              }
            , Effect.none
            )

        ConfirmDialogSent innerMsg ->
            ConfirmDialog.update
                { msg = innerMsg
                , model = model.confirmDialog
                , toModel = \confirmDialog -> { model | confirmDialog = confirmDialog }
                , onConfirm = DeleteTagConfirmed
                }

        DeleteTagConfirmed tag ->
            ( { model | tagCombination = TagCombination.removeTag tag model.tagCombination }
            , ContactDatabase.deleteTag tag
                |> Effect.map ContactDatabaseMsg
            )

        ProcessSubscriptionEventsClicked ->
            let
                toApply =
                    if List.isEmpty model.pendingModifications then
                        model.modifications

                    else
                        model.pendingModifications
            in
            if List.isEmpty toApply || model.subscriptionSync == SyncApplying then
                ( model, Effect.none )

            else
                ( { model | subscriptionSync = SyncApplying }
                , Ports.syncSubscriptionEvents True (Subscribers.encodeModifications toApply)
                    |> Effect.sendCmd
                )

        ReceivedMessage message ->
            updateWithMessage user shared model message


updateWithMessage : Auth.User -> Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeRequestId message.value of
                Ok incomingRequestId ->
                    if incomingRequestId == model.cursorRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok events ->
                                ( { model
                                    | subscriptionCursor =
                                        laterTime model.subscriptionCursor (Subscribers.subscriptionCursorFromEvents events)
                                  }
                                , Effect.none
                                )

                            Err _ ->
                                ( model, Effect.none )

                    else if incomingRequestId == model.modificationsRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok events ->
                                let
                                    modificationPage =
                                        Subscribers.modificationPageFromEvents events
                                in
                                ( { model
                                    | modifications =
                                        Subscribers.latestModifications (model.modifications ++ modificationPage.modifications)
                                    , errors = model.errors ++ modificationPage.errors
                                    , pageEventCount = model.pageEventCount + modificationPage.eventCount
                                    , pageOldest = earlierTime model.pageOldest modificationPage.oldest
                                    , scannedUntil = laterTime model.scannedUntil modificationPage.newest
                                  }
                                , Effect.none
                                )

                            Err _ ->
                                ( model, Effect.none )

                    else if incomingRequestId == model.sourceRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok events ->
                                ( { model | recipientSource = Subscribers.recipientSourceFromEvents events }, Effect.none )

                            Err _ ->
                                ( model, Effect.none )

                    else if model.requestId == incomingRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok [] ->
                                ( { model | fileState = FileMissing }, Effect.none )

                            Ok events ->
                                case Nostr.External.decodeEventsKind message.value of
                                    Ok KindApplicationSpecificData ->
                                        let
                                            ( maybeSubscriberEventData, _, errors ) =
                                                Subscribers.processEvents user.pubKey [] events
                                        in
                                        case maybeSubscriberEventData of
                                            Just subscriberEventData ->
                                                ( { model | errors = model.errors ++ errors, fileState = FileLoading }
                                                , Ports.downloadAndDecryptFile subscriberEventData.url
                                                    subscriberEventData.keyHex
                                                    subscriberEventData.ivHex
                                                    |> Effect.sendCmd
                                                )

                                            Nothing ->
                                                ( { model | fileState = FileError "No subscriber file found" }, Effect.none )

                                    _ ->
                                        ( model, Effect.none )

                            Err error ->
                                ( { model | fileState = FileError (Decode.errorToString error) }, Effect.none )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        "eventsComplete" ->
            case Nostr.External.decodeRequestId message.value of
                Ok incomingRequestId ->
                    if incomingRequestId == model.cursorRequestId && model.modificationScan == ScanWaitingForCursor then
                        requestModificationPage user shared model Nothing

                    else if incomingRequestId == model.modificationsRequestId && model.modificationScan == ScanPaging then
                        case Subscribers.nextModificationUntil model.pageEventCount model.pageOldest model.previousPageOldest model.subscriptionCursor of
                            Just until ->
                                requestModificationPage user shared model (Just until)

                            Nothing ->
                                beginSubscriptionCheck { model | modificationScan = ScanFinished }

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        "subscriptionEvents" ->
            case Decode.decodeValue subscriptionEventsDecoder message.value of
                Ok result ->
                    case result.error of
                        Just error ->
                            ( { model | subscriptionSync = SyncFailed error }, Effect.none )

                        Nothing ->
                            if model.subscriptionSync == SyncApplying then
                                let
                                    cleared =
                                        { model
                                            | modifications = []
                                            , pendingModifications = []
                                            , subscriptionSync = SyncReady
                                        }

                                    ( paged, fetchEffect ) =
                                        goToPage (Table.getCurrentPage model.subscriberTable) cleared

                                    ( advanced, saveEffect ) =
                                        advanceSubscriptionCursor user shared paged
                                in
                                ( advanced, Effect.batch [ fetchEffect, saveEffect ] )

                            else
                                let
                                    pendingEmails =
                                        result.pending
                                            |> List.map (String.trim >> String.toLower)
                                            |> Set.fromList

                                    ready =
                                        { model
                                            | pendingModifications =
                                                model.modifications
                                                    |> List.filter
                                                        (\modification ->
                                                            Set.member (modificationEmailKey modification) pendingEmails
                                                        )
                                            , subscriptionSync = SyncReady
                                        }
                                in
                                if List.isEmpty ready.pendingModifications then
                                    advanceSubscriptionCursor user shared ready

                                else
                                    ( ready, Effect.none )

                Err error ->
                    ( { model | subscriptionSync = SyncFailed (Decode.errorToString error) }, Effect.none )

        "decryptedString" ->
            case Decode.decodeValue Subscribers.subscriberDataDecoder message.value of
                Ok subscribers ->
                    let
                        subscribersDict =
                            List.foldl
                                (\subscriber acc ->
                                    Dict.insert subscriber.email subscriber acc
                                )
                                Dict.empty
                                subscribers
                    in
                    ( { model
                        | fileState = FileReady
                        , subscribers = subscribersDict
                      }
                    , Effect.none
                    )

                Err error ->
                    ( { model | fileState = FileError (Decode.errorToString error) }, Effect.none )

        "contactAdded" ->
            case Decode.decodeValue (Decode.maybe (Decode.field "error" Decode.string)) message.value of
                Ok (Just error) ->
                    let
                        contactDatabase =
                            model.contactDatabase
                    in
                    ( { model
                        | errors = error :: model.errors
                        , contactDatabase = { contactDatabase | loading = False }
                      }
                    , Effect.none
                    )

                Ok Nothing ->
                    let
                        tagErrors =
                            Decode.decodeValue (Decode.field "tagErrors" (Decode.list Decode.string)) message.value
                                |> Result.withDefault []
                    in
                    goToPage (Table.getCurrentPage model.subscriberTable)
                        { model | errors = tagErrors ++ model.errors }

                Err error ->
                    let
                        contactDatabase =
                            model.contactDatabase
                    in
                    ( { model
                        | errors = Decode.errorToString error :: model.errors
                        , contactDatabase = { contactDatabase | loading = False }
                      }
                    , Effect.none
                    )

        "contactsStored" ->
            case Decode.decodeValue (Decode.maybe (Decode.field "error" Decode.string)) message.value of
                Ok (Just error) ->
                    ( { model | migration = MigrationFailed error }, Effect.none )

                Ok Nothing ->
                    let
                        tagErrors =
                            Decode.decodeValue (Decode.field "tagErrors" (Decode.list Decode.string)) message.value
                                |> Result.withDefault []

                        ( nextModel, fetchEffect ) =
                            goToPage 1
                                { model
                                    | migration = MigrationDone
                                    , recipientSource = ContactDatabase
                                    , errors = tagErrors ++ model.errors
                                }
                    in
                    ( nextModel
                    , Effect.batch
                        [ fetchEffect
                        , ContactDatabase.loadContactTags user.pubKey
                            |> Effect.sendCmd
                            |> Effect.map ContactDatabaseMsg
                        , Subscribers.saveRecipientSource shared.browserEnv user.pubKey ContactDatabase
                            |> Effect.sendSharedMsg
                        ]
                    )

                Err error ->
                    ( { model | migration = MigrationFailed (Decode.errorToString error) }, Effect.none )

        "contactUpdated" ->
            case Decode.decodeValue (Decode.maybe (Decode.field "error" Decode.string)) message.value of
                Ok (Just error) ->
                    let
                        contactDatabase =
                            model.contactDatabase
                    in
                    ( { model
                        | errors = error :: model.errors
                        , contactDatabase = { contactDatabase | loading = False }
                      }
                    , Effect.none
                    )

                Ok Nothing ->
                    let
                        tagErrors =
                            Decode.decodeValue (Decode.field "tagErrors" (Decode.list Decode.string)) message.value
                                |> Result.withDefault []
                    in
                    goToPage (Table.getCurrentPage model.subscriberTable)
                        { model | errors = tagErrors ++ model.errors }

                Err error ->
                    let
                        contactDatabase =
                            model.contactDatabase
                    in
                    ( { model
                        | errors = Decode.errorToString error :: model.errors
                        , contactDatabase = { contactDatabase | loading = False }
                      }
                    , Effect.none
                    )

        "contactTagDeleted" ->
            goToPage (Table.getCurrentPage model.subscriberTable) model

        "contactCsvPreview" ->
            case ContactCsvImportDialog.decodePreview message.value of
                Ok preview ->
                    ( { model | csvImport = ContactCsvImportDialog.applyPreview preview model.csvImport }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "contactCsvImportProgress" ->
            case ContactCsvImportDialog.decodeProgress message.value of
                Ok progress ->
                    let
                        csvImport =
                            ContactCsvImportDialog.applyProgress progress model.csvImport

                        withImport =
                            { model | csvImport = csvImport }
                    in
                    if progress.done then
                        let
                            ( paged, pageEffect ) =
                                goToPage 1 withImport
                        in
                        ( paged
                        , Effect.batch
                            [ pageEffect
                            , Ports.loadContactTags user.pubKey |> Effect.sendCmd
                            ]
                        )

                    else
                        ( withImport, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "contactCsvExportProgress" ->
            case Decode.decodeValue exportProgressDecoder message.value of
                Ok progress ->
                    ( { model
                        | csvExport =
                            if progress.done then
                                ExportFinished progress.exported progress.error

                            else
                                Exporting progress.exported
                      }
                    , Effect.none
                    )

                Err _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


exportProgressDecoder : Decode.Decoder { exported : Int, done : Bool, error : Maybe String }
exportProgressDecoder =
    Decode.map3
        (\exported done error -> { exported = exported, done = done, error = error })
        (Decode.field "exported" Decode.int)
        (Decode.field "done" Decode.bool)
        (Decode.maybe (Decode.field "error" Decode.string))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.receiveMessage ReceivedMessage
        , ContactDatabase.subscriptions model.contactDatabase
            |> Sub.map ContactDatabaseMsg
        , SearchBar.subscribe model.searchBar
            |> Sub.map SearchBarSent
        ]


switchCategory : Model -> PageCategory -> ( Model, Effect Msg )
switchCategory model category =
    if model.pageCategory == category then
        ( model, Effect.none )

    else
        ( { model
            | pageCategory = category
            , categories = Categories.select model.categories category
          }
        , replaceCategoryRoute category
        )


goToPage : Int -> Model -> ( Model, Effect Msg )
goToPage pageNumber model =
    let
        ( contactDatabase, requestId ) =
            ContactDatabase.prepareRequest model.contactDatabase

        requestEffect =
            let
                trimmedSearch =
                    model.searchText
                        |> Maybe.map String.trim
                        |> Maybe.andThen
                            (\term ->
                                if term == "" then
                                    Nothing

                                else
                                    Just term
                            )
            in
            case ( trimmedSearch, TagCombination.toFilter model.tagCombination ) of
                ( Just term, _ ) ->
                    ContactDatabase.searchContacts requestId term pageNumber ContactDatabase.pageSize

                ( Nothing, Just filter ) ->
                    ContactDatabase.filterContacts requestId (TagCombination.encode filter) pageNumber ContactDatabase.pageSize

                ( Nothing, Nothing ) ->
                    ContactDatabase.loadContacts requestId pageNumber ContactDatabase.pageSize model.sortColumn model.sortReversed
    in
    ( { model
        | contactDatabase = contactDatabase
        , subscriberTable = Table.setCurrentPage pageNumber model.subscriberTable
      }
    , Effect.map ContactDatabaseMsg requestEffect
    )



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.Sidebar.contactDatabaseMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ case model.pageCategory of
            ContactsCategory ->
                viewPage shared model

            TagsCategory ->
                viewTagsPage shared model
        , ConfirmDialog.new
            { model = model.confirmDialog
            , toMsg = ConfirmDialogSent
            , theme = shared.theme
            }
            |> ConfirmDialog.view
        , ContactCsvImportDialog.new
            { model = model.csvImport
            , toMsg = CsvImportSent
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> ContactCsvImportDialog.view
        ]
    }


viewTagsPage : Shared.Model -> Model -> Html Msg
viewTagsPage shared model =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            , Tw.ps_8
            , Tw.pe_4
            , Tw.py_4
            , Tw.max_w_lg
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.items_end
                , Tw.gap_2
                ]
            ]
            [ div
                [ css [ Tw.flex_1 ] ]
                [ EntryField.new
                    { value = model.tagDraft
                    , onInput = TagDraftChanged
                    , theme = shared.theme
                    }
                    |> EntryField.withLabel (Translations.addTagFieldLabel [ shared.browserEnv.translations ])
                    |> EntryField.view
                ]
            , Button.new
                { label = Translations.addTagButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just AddTagClicked
                , theme = shared.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (String.trim model.tagDraft == "")
                |> Button.view
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                ]
            ]
            (List.map (viewTagRow shared.theme shared.browserEnv) model.contactDatabase.tags)
        , viewErrors model
        ]


isExporting : CsvExportState -> Bool
isExporting state =
    case state of
        Exporting _ ->
            True

        _ ->
            False


viewCsvExportStatus : Shared.Model -> CsvExportState -> Html msg
viewCsvExportStatus shared state =
    let
        translations =
            shared.browserEnv.translations

        message =
            case state of
                ExportIdle ->
                    Nothing

                Exporting exported ->
                    Just <| Translations.csvExportProgressText [ translations ] { exported = String.fromInt exported }

                ExportFinished exported (Just error) ->
                    Just <| Translations.csvExportErrorText [ translations ] { error = error }

                ExportFinished exported Nothing ->
                    Just <| Translations.csvExportDoneText [ translations ] { exported = String.fromInt exported }
    in
    case message of
        Nothing ->
            text ""

        Just status ->
            p [] [ text status ]


viewTagRow : Theme -> BrowserEnv -> String -> Html Msg
viewTagRow theme browserEnv tag =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.justify_between
            , Tw.gap_4
            ]
        ]
        [ text tag
        , Button.new
            { label = Translations.removeTagButtonTitle [ browserEnv.translations ]
            , onClick = Just (DeleteTagClicked tag)
            , theme = theme
            }
            |> Button.withTypeSecondary
            |> Button.view
        ]


viewPage : Shared.Model -> Model -> Html Msg
viewPage shared model =
    let
        databaseCount =
            model.contactDatabase.databaseTotal
                |> Maybe.withDefault 0

        fileCount =
            Dict.size model.subscribers

        canMigrate =
            model.fileState == FileReady && fileCount > 0 && model.contactDatabase.authenticated && model.migration /= MigrationRunning
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            , Tw.ps_8
            , Tw.pe_4
            , Tw.py_4
            ]
        ]
        [ p []
            [ text <| Translations.migrateHintText [ shared.browserEnv.translations ] ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.flex_wrap
                , Tw.items_center
                , Tw.gap_4
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_1
                    ]
                ]
                [ p []
                    [ text <| Translations.subscriberFileLabel [ shared.browserEnv.translations ] ++ ": " ++ String.fromInt fileCount ]
                , p []
                    [ text <| Translations.contactDatabaseLabel [ shared.browserEnv.translations ] ++ ": " ++ String.fromInt databaseCount ]
                ]
            , Button.new
                { label = Translations.migrateButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just MigrateClicked
                , theme = shared.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (not canMigrate)
                |> Button.view
            , Button.new
                { label = Translations.addContactButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just AddContactClicked
                , theme = shared.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (not model.contactDatabase.authenticated)
                |> Button.view
            , Button.new
                { label = Translations.importButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just ImportCsvClicked
                , theme = shared.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (not model.contactDatabase.authenticated)
                |> Button.view
            , Button.new
                { label = Translations.exportButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just ExportCsvClicked
                , theme = shared.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (databaseCount < 1 || isExporting model.csvExport)
                |> Button.view
            ]
        , viewCsvExportStatus shared model.csvExport
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.flex_wrap
                , Tw.items_center
                , Tw.gap_2
                ]
            ]
            [ text <| Translations.newsletterSourceLabel [ shared.browserEnv.translations ]
            , Switch.new
                { id = "newsletter-recipient-source"
                , onClick = SetRecipientSource
                , labelOff = Translations.subscriberFileLabel [ shared.browserEnv.translations ]
                , labelOn = Translations.contactDatabaseLabel [ shared.browserEnv.translations ]
                , state = model.recipientSource
                , stateOff = SubscriberFile
                , stateOn = ContactDatabase
                , theme = shared.theme
                }
                |> Switch.view
            ]
        , viewMigration shared.browserEnv model
        , viewFileState shared.browserEnv model
        , viewSubscriptionEvents shared model
        , viewQuery shared model
        , viewDatabase shared model
        , viewErrors model
        , SubscriberEditDialog.new
            { model = model.subscriberEditDialog
            , toMsg = SubscriberEditDialogSent
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> SubscriberEditDialog.withTags model.contactDatabase.tags
            |> SubscriberEditDialog.view
        ]


viewMigration : BrowserEnv -> Model -> Html Msg
viewMigration browserEnv model =
    case model.migration of
        MigrationIdle ->
            text ""

        MigrationRunning ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.items_center
                    ]
                ]
                [ text <| Translations.migratingText [ browserEnv.translations ]
                , Loaders.rings [] |> Html.fromUnstyled
                ]

        MigrationDone ->
            p []
                [ text <| Translations.migrationDoneText [ browserEnv.translations ] ]

        MigrationFailed error ->
            p []
                [ text error ]


requestModificationPage : Auth.User -> Shared.Model -> Model -> Maybe Posix -> ( Model, Effect Msg )
requestModificationPage user shared model until =
    let
        requestId =
            Nostr.getLastRequestId shared.nostr
    in
    ( { model
        | modificationScan = ScanPaging
        , modificationsRequestId = requestId
        , previousPageOldest = model.pageOldest
        , pageEventCount = 0
        , pageOldest = Nothing
      }
    , Subscribers.loadModificationPage requestId user.pubKey model.subscriptionCursor until
        |> Effect.sendSharedMsg
    )


advanceSubscriptionCursor : Auth.User -> Shared.Model -> Model -> ( Model, Effect Msg )
advanceSubscriptionCursor user shared model =
    case model.scannedUntil of
        Nothing ->
            ( model, Effect.none )

        Just scanned ->
            let
                alreadyStored =
                    model.subscriptionCursor
                        |> Maybe.map (\cursor -> Time.posixToMillis scanned <= Time.posixToMillis cursor)
                        |> Maybe.withDefault False
            in
            if alreadyStored then
                ( model, Effect.none )

            else
                ( { model | subscriptionCursor = Just scanned }
                , Subscribers.saveSubscriptionCursor shared.browserEnv user.pubKey scanned
                    |> Effect.sendSharedMsg
                )


earlierTime : Maybe Posix -> Maybe Posix -> Maybe Posix
earlierTime current candidate =
    case ( current, candidate ) of
        ( Nothing, time ) ->
            time

        ( time, Nothing ) ->
            time

        ( Just currentTime, Just candidateTime ) ->
            if Time.posixToMillis candidateTime < Time.posixToMillis currentTime then
                Just candidateTime

            else
                Just currentTime


laterTime : Maybe Posix -> Maybe Posix -> Maybe Posix
laterTime current candidate =
    case ( current, candidate ) of
        ( Nothing, time ) ->
            time

        ( time, Nothing ) ->
            time

        ( Just currentTime, Just candidateTime ) ->
            if Time.posixToMillis candidateTime > Time.posixToMillis currentTime then
                Just candidateTime

            else
                Just currentTime


beginSubscriptionCheck : Model -> ( Model, Effect Msg )
beginSubscriptionCheck model =
    let
        latest =
            Subscribers.latestModifications model.modifications
    in
    if List.isEmpty latest then
        ( { model | modifications = [], pendingModifications = [], subscriptionSync = SyncReady }, Effect.none )

    else if not model.contactDatabase.authenticated then
        ( { model | modifications = latest, subscriptionSync = SyncWaiting }, Effect.none )

    else
        ( { model | modifications = latest, subscriptionSync = SyncChecking }
        , Ports.syncSubscriptionEvents False (Subscribers.encodeModifications latest)
            |> Effect.sendCmd
        )


subscriptionEventsDecoder : Decode.Decoder { pending : List String, applied : Maybe Int, error : Maybe String }
subscriptionEventsDecoder =
    Decode.map3
        (\pending applied error -> { pending = pending, applied = applied, error = error })
        (Decode.field "pending" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "applied" Decode.int))
        (Decode.maybe (Decode.field "error" Decode.string))


modificationEmailKey : Modification -> String
modificationEmailKey modification =
    Subscribers.modificationEmail modification
        |> String.trim
        |> String.toLower


subscriptionScanLoader : Html msg
subscriptionScanLoader =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            , Tw.items_center
            ]
        ]
        [ Loaders.rings [] |> Html.fromUnstyled ]


viewSubscriptionEvents : Shared.Model -> Model -> Html Msg
viewSubscriptionEvents shared model =
    let
        visibleModifications =
            if not (List.isEmpty model.pendingModifications) then
                model.pendingModifications

            else
                case model.subscriptionSync of
                    SyncFailed _ ->
                        model.modifications

                    SyncApplying ->
                        model.modifications

                    _ ->
                        []

        orderedModifications =
            visibleModifications
                |> List.sortBy (\modification -> negate (Subscribers.modificationTime modification))
    in
    case model.subscriptionSync of
        SyncLoading ->
            subscriptionScanLoader

        SyncWaiting ->
            subscriptionScanLoader

        SyncChecking ->
            subscriptionScanLoader

        SyncReady ->
            viewPendingSubscriptionEvents shared orderedModifications False Nothing

        SyncApplying ->
            viewPendingSubscriptionEvents shared orderedModifications True Nothing

        SyncFailed error ->
            viewPendingSubscriptionEvents shared orderedModifications False (Just error)


viewPendingSubscriptionEvents : Shared.Model -> List Modification -> Bool -> Maybe String -> Html Msg
viewPendingSubscriptionEvents shared modifications applying error =
    if List.isEmpty modifications && error == Nothing then
        text ""

    else
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                ]
            ]
            [ p []
                [ text <| Translations.newModifications [ shared.browserEnv.translations ] ]
            , case error of
                Just message ->
                    p [] [ text message ]

                Nothing ->
                    text ""
            , Button.new
                { label = Translations.processModificationsButtonTitle [ shared.browserEnv.translations ]
                , onClick =
                    if applying || List.isEmpty modifications then
                        Nothing

                    else
                        Just ProcessSubscriptionEventsClicked
                , theme = shared.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (applying || List.isEmpty modifications)
                |> Button.view
            , ul []
                (List.map (viewSubscriptionEvent shared.browserEnv) modifications)
            , if applying then
                div
                    [ css
                        [ Tw.flex
                        , Tw.flex_row
                        , Tw.gap_2
                        , Tw.items_center
                        ]
                    ]
                    [ Loaders.rings [] |> Html.fromUnstyled ]

              else
                text ""
            ]


viewSubscriptionEvent : BrowserEnv -> Modification -> Html Msg
viewSubscriptionEvent browserEnv modification =
    case modification of
        Subscription subscriber ->
            li []
                [ text <|
                    Subscribers.modificationToString modification
                        ++ ": "
                        ++ subscriber.email
                        ++ " ("
                        ++ BrowserEnv.formatDate browserEnv subscriber.dateSubscription
                        ++ ")"
                ]

        Unsubscription subscriber ->
            let
                dateSuffix =
                    subscriber.dateUnsubscription
                        |> Maybe.map (\date -> " (" ++ BrowserEnv.formatDate browserEnv date ++ ")")
                        |> Maybe.withDefault ""
            in
            li []
                [ text <| Subscribers.modificationToString modification ++ ": " ++ subscriber.email ++ dateSuffix ]


viewFileState : BrowserEnv -> Model -> Html Msg
viewFileState browserEnv model =
    case model.fileState of
        FileLoading ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.items_center
                    ]
                ]
                [ text <| Translations.loadingSubscribersText [ browserEnv.translations ]
                , Loaders.rings [] |> Html.fromUnstyled
                ]

        FileMissing ->
            p []
                [ text <| Translations.noSubscriberFileText [ browserEnv.translations ] ]

        FileError error ->
            p []
                [ text <| Translations.errorLoadingSubscribersText [ browserEnv.translations ] ++ ": " ++ error ]

        FileReady ->
            text ""


viewQuery : Shared.Model -> Model -> Html Msg
viewQuery shared model =
    let
        styles =
            stylesForTheme shared.theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.flex_wrap
            , Tw.items_start
            , Tw.gap_6
            ]
        ]
        [ SearchBar.new
            { model = model.searchBar
            , toMsg = SearchBarSent
            , browserEnv = shared.browserEnv
            , styles = styles
            }
            |> SearchBar.withFlexibleWidth
            |> SearchBar.view
        , viewTagFilter shared model
        ]


viewTagFilter : Shared.Model -> Model -> Html Msg
viewTagFilter shared model =
    case model.contactDatabase.tags of
        [] ->
            text ""

        tags ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    ]
                ]
                [ p []
                    [ text <| Translations.filterByTags [ shared.browserEnv.translations ] ]
                , TagCombination.new
                    { model = model.tagCombination
                    , toMsg = TagCombinationSent
                    , tags = tags
                    , theme = shared.theme
                    , translations = shared.browserEnv.translations
                    }
                    |> TagCombination.view
                ]


viewDatabase : Shared.Model -> Model -> Html Msg
viewDatabase shared model =
    let
        browserEnv =
            shared.browserEnv

        shown =
            List.length model.contactDatabase.subscribers

        total =
            model.contactDatabase.total
                |> Maybe.withDefault shown

        hasQuery =
            model.searchText /= Nothing || TagCombination.toFilter model.tagCombination /= Nothing
    in
    if model.contactDatabase.loading && not model.contactDatabase.contactsLoaded then
        div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                , Tw.items_center
                ]
            ]
            [ text <| Translations.databaseLoadingText [ browserEnv.translations ]
            , Loaders.rings [] |> Html.fromUnstyled
            ]

    else if shown == 0 && model.contactDatabase.contactsLoaded then
        p []
            [ text <|
                if hasQuery then
                    Translations.noMatchingContactsText [ browserEnv.translations ]

                else
                    Translations.databaseEmptyText [ browserEnv.translations ]
            ]

    else if shown == 0 then
        text ""

    else
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                ]
            ]
            [ p []
                [ text <|
                    Translations.showingContactsText
                        [ browserEnv.translations ]
                        { shown = String.fromInt shown, total = String.fromInt total }
                ]
            , if model.contactDatabase.loading then
                div
                    [ css
                        [ Tw.flex
                        , Tw.flex_row
                        , Tw.gap_2
                        , Tw.items_center
                        ]
                    ]
                    [ text <| Translations.databaseLoadingText [ browserEnv.translations ]
                    , Loaders.rings [] |> Html.fromUnstyled
                    ]

              else
                text ""
            , Table.view
                (subscribersTableConfig browserEnv model.sortColumn model.sortReversed)
                model.subscriberTable
                model.contactDatabase.subscribers
                |> Html.fromUnstyled
            , viewPager shared.theme browserEnv model
            ]


viewPager : Theme -> BrowserEnv -> Model -> Html Msg
viewPager theme browserEnv model =
    let
        currentPage =
            Table.getCurrentPage model.subscriberTable

        pageCount =
            Table.getPageCount model.subscriberTable
    in
    if pageCount <= 1 && currentPage <= 1 then
        text ""

    else
        div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.items_center
                , Tw.gap_2
                ]
            ]
            [ Button.new
                { label = Translations.previousPage [ browserEnv.translations ]
                , onClick = Just <| NewTableState (Table.previousPage model.subscriberTable)
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (currentPage <= 1)
                |> Button.view
            , text <|
                Translations.pageStatus
                    [ browserEnv.translations ]
                    { page = String.fromInt currentPage, pages = String.fromInt (max 1 pageCount) }
            , Button.new
                { label = Translations.nextPage [ browserEnv.translations ]
                , onClick = Just <| NewTableState (Table.nextPage model.subscriberTable)
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (currentPage >= pageCount)
                |> Button.view
            ]


viewErrors : Model -> Html msg
viewErrors model =
    let
        errors =
            model.errors ++ model.contactDatabase.errors
    in
    if List.isEmpty errors then
        text ""

    else
        ul []
            (List.map (\error -> li [] [ text error ]) errors)


subscribersTableConfig : BrowserEnv -> String -> Bool -> Table.Config Subscriber Msg
subscribersTableConfig browserEnv sortColumn sortReversed =
    let
        cellPadding =
            UnstyledAttr.style "padding" "0.75rem 1.5rem 0.75rem 2.5rem"

        columns =
            [ { id = fieldName FieldEmail
              , name = translatedFieldName browserEnv.translations FieldEmail
              , viewData = editSubscriberButton
              , sortable = True
              }
            , { id = fieldName FieldFirstName
              , name = translatedFieldName browserEnv.translations FieldFirstName
              , viewData = \subscriber -> Unstyled.text (subscriber.firstName |> Maybe.withDefault "")
              , sortable = True
              }
            , { id = fieldName FieldLastName
              , name = translatedFieldName browserEnv.translations FieldLastName
              , viewData = \subscriber -> Unstyled.text (subscriber.lastName |> Maybe.withDefault "")
              , sortable = True
              }
            , { id = fieldName FieldTags
              , name = translatedFieldName browserEnv.translations FieldTags
              , viewData = \subscriber -> Unstyled.text (subscriber.tags |> Maybe.map (String.join ", ") |> Maybe.withDefault "")
              , sortable = False
              }
            , { id = fieldName FieldSource
              , name = translatedFieldName browserEnv.translations FieldSource
              , viewData = \subscriber -> Unstyled.text (subscriber.source |> Maybe.withDefault "")
              , sortable = False
              }
            , { id = fieldName FieldDnd
              , name = translatedFieldName browserEnv.translations FieldDnd
              , viewData = dndMark
              , sortable = True
              }
            , { id = fieldName FieldDateUnsubscription
              , name = translatedFieldName browserEnv.translations FieldDateUnsubscription
              , viewData =
                    \subscriber ->
                        Unstyled.text
                            (subscriber.dateUnsubscription
                                |> Maybe.map (BrowserEnv.formatDate browserEnv)
                                |> Maybe.withDefault ""
                            )
              , sortable = True
              }
            ]

        column { id, name, viewData } =
            Table.veryCustomColumn
                { id = id
                , name = name
                , viewData =
                    \row ->
                        { attributes = [ cellPadding ]
                        , children = [ viewData row ]
                        }
                , sorter = Table.unsortable
                }

        sortMarker id sortable =
            if not sortable then
                ""

            else if id == sortColumn then
                if sortReversed then
                    "↑"

                else
                    "↓"

            else
                "↕"
    in
    Table.customConfig
        { toId = .email
        , toMsg = NewTableState
        , columns = List.map (\{ id, name, viewData } -> column { id = id, name = name, viewData = viewData }) columns
        , customizations =
            { defaultCustomizations
                | thead =
                    \_ ->
                        { attributes = []
                        , children =
                            List.map
                                (\{ id, name, sortable } ->
                                    Unstyled.th
                                        (cellPadding
                                            :: (if sortable then
                                                    [ UnstyledEvents.onClick (SortColumn id)
                                                    , UnstyledAttr.style "cursor" "pointer"
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                                        [ Unstyled.text (name ++ "\u{00A0}" ++ sortMarker id sortable) ]
                                )
                                columns
                        }
            }
        }


sortableColumn : String -> Bool
sortableColumn column =
    List.member column
        [ fieldName FieldEmail
        , fieldName FieldFirstName
        , fieldName FieldLastName
        , fieldName FieldDnd
        , fieldName FieldDateUnsubscription
        ]


editSubscriberButton : Subscriber -> Unstyled.Html Msg
editSubscriberButton subscriber =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    div
        [ css
            [ Tw.cursor_pointer
            , Tw.text_color styles.colorB3
            , darkMode
                [ Tw.text_color styles.colorB3DarkMode
                ]
            ]
        , Events.onClick (OpenEditSubscriberDialog subscriber)
        ]
        [ text subscriber.email ]
        |> Html.toUnstyled


dndMark : Subscriber -> Unstyled.Html msg
dndMark subscriber =
    case subscriber.dnd of
        Just True ->
            statusIcon "#dc2626" FeatherIcons.checkCircle

        _ ->
            statusIcon "#16a34a" FeatherIcons.xCircle


statusIcon : String -> FeatherIcons.Icon -> Unstyled.Html msg
statusIcon color icon =
    Unstyled.span
        [ UnstyledAttr.style "display" "inline-flex"
        , UnstyledAttr.style "color" color
        , UnstyledAttr.style "vertical-align" "middle"
        ]
        [ icon
            |> FeatherIcons.withSize 16
            |> FeatherIcons.withStrokeWidth 2
            |> FeatherIcons.toHtml []
        ]
