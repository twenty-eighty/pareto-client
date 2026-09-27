module Pages.ContactDatabase exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Categories as Categories
import Components.ConfirmDialog as ConfirmDialog
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
import Newsletters.Subscribers as Subscribers exposing (Email, RecipientSource(..), translatedFieldName)
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
import Shared
import Svg.Loaders as Loaders
import Table.Paginated as Table exposing (defaultCustomizations)
import Tailwind.Theme exposing (Color)
import Tailwind.Utilities as Tw
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
        { path = Route.Path.ContactDatabase
        , query = Dict.singleton categoryParamName (stringFromCategory category)
        , hash = Nothing
        }



-- INIT


type alias Model =
    { categories : Categories.Model PageCategory
    , confirmDialog : ConfirmDialog.Model String
    , contactDatabase : ContactDatabase.Model
    , errors : List String
    , fileState : FileState
    , migration : MigrationState
    , pageCategory : PageCategory
    , requestId : RequestId
    , recipientSource : RecipientSource
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


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        ( contactDatabase, contactDatabaseEffect ) =
            ContactDatabase.init user.pubKey [ ContactDatabase.LoadTags ]

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
      , errors = []
      , fileState = FileLoading
      , migration = MigrationIdle
      , pageCategory = pageCategory
      , requestId = requestId
      , recipientSource = SubscriberFile
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
        , replaceCategoryRoute pageCategory
        ]
    )



-- UPDATE


type Msg
    = ContactDatabaseMsg ContactDatabase.Msg
    | MigrateClicked
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
                in
                ( nextModel
                , Effect.batch
                    [ Effect.map ContactDatabaseMsg contactDatabaseEffect
                    , fetchEffect
                    ]
                )

            else
                ( modelWithDatabase
                , Effect.map ContactDatabaseMsg contactDatabaseEffect
                )

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
            if column == model.sortColumn then
                ( { model | sortReversed = not model.sortReversed }, Effect.none )

            else
                ( { model | sortColumn = column, sortReversed = False }, Effect.none )

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
            case Dict.get email model.contactDatabase.contactIds of
                Just contactId ->
                    let
                        contactDatabase =
                            model.contactDatabase
                    in
                    ( { model | contactDatabase = { contactDatabase | loading = True } }
                    , ContactDatabase.updateContact contactId subscriber
                        |> Effect.map ContactDatabaseMsg
                    )

                Nothing ->
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

        ReceivedMessage message ->
            updateWithMessage user shared model message


updateWithMessage : Auth.User -> Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeRequestId message.value of
                Ok incomingRequestId ->
                    if incomingRequestId == model.sourceRequestId then
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

        _ ->
            ( model, Effect.none )



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
                    ContactDatabase.loadContacts requestId pageNumber ContactDatabase.pageSize
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
            ]
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
                (sortedSubscribers model)
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
              }
            , { id = fieldName FieldFirstName
              , name = translatedFieldName browserEnv.translations FieldFirstName
              , viewData = \subscriber -> Unstyled.text (subscriber.firstName |> Maybe.withDefault "")
              }
            , { id = fieldName FieldLastName
              , name = translatedFieldName browserEnv.translations FieldLastName
              , viewData = \subscriber -> Unstyled.text (subscriber.lastName |> Maybe.withDefault "")
              }
            , { id = fieldName FieldTags
              , name = translatedFieldName browserEnv.translations FieldTags
              , viewData = \subscriber -> Unstyled.text (subscriber.tags |> Maybe.map (String.join ", ") |> Maybe.withDefault "")
              }
            , { id = fieldName FieldSource
              , name = translatedFieldName browserEnv.translations FieldSource
              , viewData = \subscriber -> Unstyled.text (subscriber.source |> Maybe.withDefault "")
              }
            , { id = fieldName FieldDnd
              , name = translatedFieldName browserEnv.translations FieldDnd
              , viewData = dndMark
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

        sortMarker id =
            if id == sortColumn then
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
        , columns = List.map column columns
        , customizations =
            { defaultCustomizations
                | thead =
                    \_ ->
                        { attributes = []
                        , children =
                            List.map
                                (\{ id, name } ->
                                    Unstyled.th
                                        [ cellPadding
                                        , UnstyledEvents.onClick (SortColumn id)
                                        , UnstyledAttr.style "cursor" "pointer"
                                        ]
                                        [ Unstyled.text (name ++ "\u{00A0}" ++ sortMarker id) ]
                                )
                                columns
                        }
            }
        }


sortedSubscribers : Model -> List Subscriber
sortedSubscribers model =
    let
        order a b =
            let
                result =
                    compare
                        (String.toLower (columnValue model.sortColumn a))
                        (String.toLower (columnValue model.sortColumn b))
            in
            if model.sortReversed then
                case result of
                    LT ->
                        GT

                    EQ ->
                        EQ

                    GT ->
                        LT

            else
                result
    in
    List.sortWith order model.contactDatabase.subscribers


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


columnValue : String -> Subscriber -> String
columnValue column subscriber =
    if column == fieldName FieldFirstName then
        subscriber.firstName |> Maybe.withDefault ""

    else if column == fieldName FieldLastName then
        subscriber.lastName |> Maybe.withDefault ""

    else if column == fieldName FieldTags then
        subscriber.tags |> Maybe.map (String.join ", ") |> Maybe.withDefault ""

    else if column == fieldName FieldSource then
        subscriber.source |> Maybe.withDefault ""

    else if column == fieldName FieldDnd then
        dndValue subscriber.dnd

    else
        subscriber.email


dndValue : Maybe Bool -> String
dndValue value =
    case value of
        Just True ->
            "✓"

        _ ->
            "❎"


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
