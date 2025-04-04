module Pages.Subscribers exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.AlertTimerMessage as AlertTimerMessage
import Components.Button as Button
import Components.EmailImportDialog as EmailImportDialog
import Components.Icon as Icon
import Components.SubscriberEditDialog as SubscriberEditDialog
import Csv.Encode
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import File exposing (File)
import File.Download
import Html.Styled as Html exposing (Html, b, div, li, text, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode
import Layouts
import Material.Icons exposing (email)
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External exposing (decodeAuthHeaderReceived)
import Nostr.Nip96 as Nip96
import Nostr.Request exposing (HttpRequestMethod(..), RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Subscribers exposing (Email, Modification(..), Subscriber, SubscriberField(..), modificationToString, translatedFieldName)
import Svg.Loaders as Loaders
import Table.Paginated as Table exposing (defaultCustomizations)
import Tailwind.Utilities as Tw
import Translations.Sidebar
import Translations.Subscribers as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..), stylesForTheme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { alertTimerMessage : AlertTimerMessage.Model
    , emailImportDialog : EmailImportDialog.Model
    , errors : List String
    , modifications : List Modification
    , requestId : RequestId
    , state : ModelState
    , subscriberEditDialog : SubscriberEditDialog.Model
    , subscribers : Dict Email Subscriber
    , subscriberTable : Table.State
    , serverDesc : Maybe Nip96.ServerDescriptorData
    , fileId : Int
    }


type ModelState
    = Loading
    | Loaded
    | ErrorLoadingSubscribers String
    | Modified
    | Encrypting String String Int Int
    | RequestingNip96Auth String File String String String Int Int Int
    | Uploading String String String Int Int Int
    | Downloading Subscribers.SubscriberEventData
    | Saving
    | Saved


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { alertTimerMessage = AlertTimerMessage.init {}
      , emailImportDialog = EmailImportDialog.init {}
      , errors = []
      , modifications = []
      , requestId = Nostr.getLastRequestId shared.nostr
      , state = Loading
      , subscriberEditDialog = SubscriberEditDialog.init {}
      , subscribers = Dict.empty
      , subscriberTable = Table.initialState (Subscribers.fieldName FieldEmail) 25
      , serverDesc = Nothing
      , fileId = 1
      }
    , [ Subscribers.load shared.nostr user.pubKey
      , Subscribers.loadModifications shared.nostr user.pubKey
      ]
        |> List.map Effect.sendSharedMsg
        |> List.append
            [ Nip96.fetchServerSpec
                (ReceivedNip96ServerDesc ("https://" ++ Pareto.paretoNip96Server))
                ("https://" ++ Pareto.paretoNip96Server)
                |> Effect.sendCmd
            ]
        |> Effect.batch
    )



-- UPDATE


type Msg
    = ImportClicked
    | ExportClicked
    | SaveClicked String String
    | ProcessModificationsClicked
    | AlertTimerMessageSent AlertTimerMessage.Msg
    | EmailImportDialogSent (EmailImportDialog.Msg Msg)
    | AddSubscribers (List Subscriber) Bool
    | RemoveSubscriber String
    | NewTableState Table.State
    | ReceivedMessage IncomingMessage
    | ReceivedNip96ServerDesc String (Result Http.Error Nip96.ServerDescResponse)
    | UploadResultNip96 (Result Http.Error Nip96.UploadResponse) -- fileId, api URL
    | OpenEditSubscriberDialog Subscriber
    | SubscriberEditDialogSent SubscriberEditDialog.Msg
    | UpdateSubscriber String Subscriber


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ImportClicked ->
            ( { model | emailImportDialog = EmailImportDialog.show model.emailImportDialog }, Effect.none )

        ExportClicked ->
            ( model
            , model.subscribers
                |> Dict.values
                |> Subscribers.toCsv
                |> Csv.Encode.toBytes
                |> File.Download.bytes (csvDownloadFileName shared.browserEnv) "text/csv"
                |> Effect.sendCmd
            )

        SaveClicked serverUrl apipUrl ->
            let
                activeSubscriberCount =
                    model.subscribers
                        |> Dict.values
                        |> List.filter (\subscriber -> subscriber.dateUnsubscription == Nothing)
                        |> List.length
            in
            ( { model | state = Encrypting serverUrl apipUrl activeSubscriberCount (Dict.size model.subscribers) }
            , Subscribers.subscribersToJson (Dict.values model.subscribers)
                |> Ports.encryptString
                |> Effect.sendCmd
            )

        ProcessModificationsClicked ->
            ( { model | state = Modified, subscribers = Subscribers.processModifications model.subscribers model.modifications }, Effect.none )

        AlertTimerMessageSent innerMsg ->
            AlertTimerMessage.update
                { msg = innerMsg
                , model = model.alertTimerMessage
                , toModel = \alertTimerMessage -> { model | alertTimerMessage = alertTimerMessage }
                , toMsg = AlertTimerMessageSent
                }

        EmailImportDialogSent innerMsg ->
            EmailImportDialog.update
                { msg = innerMsg
                , model = model.emailImportDialog
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , onImport = AddSubscribers
                , pubKey = user.pubKey
                , toModel = \emailImportDialog -> { model | emailImportDialog = emailImportDialog }
                , toMsg = EmailImportDialogSent
                }

        AddSubscribers newSubscribers overwriteExisting ->
            let
                subscribers =
                    Subscribers.merge overwriteExisting model.subscribers newSubscribers
            in
            ( { model
                | emailImportDialog = EmailImportDialog.hide model.emailImportDialog
                , state = Modified
                , subscribers = subscribers
                , subscriberTable = Table.setTotal (Dict.size subscribers) model.subscriberTable
              }
            , Effect.none
            )

        RemoveSubscriber email ->
            let
                subscribers =
                    Subscribers.remove model.subscribers [ email ]
            in
            ( { model
                | state = Modified
                , subscribers = subscribers
                , subscriberTable = Table.setTotal (Dict.size subscribers) model.subscriberTable
              }
            , Effect.none
            )

        NewTableState tableState ->
            ( { model | subscriberTable = tableState }, Effect.none )

        ReceivedMessage message ->
            updateWithMessage user shared model message

        ReceivedNip96ServerDesc serverUrl serverDescResponseResult ->
            case serverDescResponseResult of
                Ok (Nip96.ServerRedirect serverRedirection) ->
                    ( model
                    , Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc serverUrl) serverRedirection.delegated_to_url)
                    )

                Ok (Nip96.ServerDescriptor serverDescriptorData) ->
                    ( { model | serverDesc = Just <| Nip96.extendRelativeServerDescriptorUrls serverUrl serverDescriptorData }, Effect.none )

                Err error ->
                    ( { model | errors = ("Error getting server description: " ++ httpErrorToString error) :: model.errors }, Effect.none )

        UploadResultNip96 (Ok response) ->
            case ( response.status, response.fileMetadata, model.state ) of
                ( "success", Just fileMetadata, Uploading keyHex ivHex sha256 size active total ) ->
                    if fileMetadata.oxHash == Nothing || fileMetadata.oxHash == Just sha256 then
                        ( { model | state = Saving }
                        , Subscribers.subscriberDataEvent shared.browserEnv user.pubKey { keyHex = keyHex, ivHex = ivHex, url = Maybe.withDefault "" fileMetadata.url, size = size, active = active, total = total }
                            |> SendApplicationData
                            |> Shared.Msg.SendNostrEvent
                            |> Effect.sendSharedMsg
                        )

                    else
                        ( { model | state = Modified, errors = "Uploaded file differs from local file" :: model.errors }, Effect.none )

                ( status, _, _ ) ->
                    ( { model | state = Modified, errors = ("Error uploading subscribers: " ++ status) :: model.errors }, Effect.none )

        UploadResultNip96 (Err error) ->
            ( { model | state = Modified, errors = ("Error uploading subscribers: " ++ httpErrorToString error) :: model.errors }, Effect.none )

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
            ( { model | state = Modified, subscribers = Dict.update email (\_ -> Just subscriber) model.subscribers }
            , Effect.none
            )


csvDownloadFileName : BrowserEnv -> String
csvDownloadFileName browserEnv =
    "subscribers_"
        ++ BrowserEnv.formatIsoDate browserEnv browserEnv.now
        ++ ".csv"


updateWithMessage : Auth.User -> Shared.Model.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeRequestId message.value of
                Ok incomingRequestId ->
                    if model.requestId == incomingRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok [] ->
                                ( { model | state = Loaded }, Effect.none )

                            Ok events ->
                                case Nostr.External.decodeEventsKind message.value of
                                    Ok KindApplicationSpecificData ->
                                        let
                                            ( maybeSubscriberEventData, modifications, errors ) =
                                                Subscribers.processEvents user.pubKey model.modifications events

                                            modelWithModifications =
                                                { model | modifications = modifications, errors = model.errors ++ errors }
                                        in
                                        case maybeSubscriberEventData of
                                            Just subscriberEventData ->
                                                ( { modelWithModifications | state = Downloading subscriberEventData }
                                                , Ports.downloadAndDecryptFile subscriberEventData.url
                                                    subscriberEventData.keyHex
                                                    subscriberEventData.ivHex
                                                    |> Effect.sendCmd
                                                )

                                            Nothing ->
                                                ( { modelWithModifications | state = ErrorLoadingSubscribers "No subscriber event found" }, Effect.none )

                                    _ ->
                                        ( model, Effect.none )

                            Err error ->
                                ( { model | state = ErrorLoadingSubscribers (Decode.errorToString error) }, Effect.none )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        "encryptedString" ->
            case ( model.state, Decode.decodeValue receivedDataDecoder message.value ) of
                ( Encrypting serverUrl apiUrl active total, Ok decoded ) ->
                    ( { model
                        | state = RequestingNip96Auth apiUrl decoded.file decoded.keyHex decoded.ivHex decoded.sha256 decoded.size active total
                      }
                    , PostRequest 1 decoded.sha256
                        |> RequestNip98Auth serverUrl apiUrl
                        |> Nostr.createRequest shared.nostr "NIP-96 auth request for files to be uploaded" []
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    )

                ( Encrypting _ _ _ _, Err error ) ->
                    ( { model | errors = ("Error receiving encrypted file: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

                ( _, _ ) ->
                    ( model, Effect.none )

        "decryptedString" ->
            case Decode.decodeValue Subscribers.subscriberDataDecoder message.value of
                Ok subscribers ->
                    let
                        subscribersDict =
                            subscribers
                                |> List.foldl
                                    (\subscriber acc ->
                                        Dict.insert subscriber.email subscriber acc
                                    )
                                    Dict.empty
                    in
                    ( { model
                        | state = Loaded
                        , subscribers = subscribersDict
                        , subscriberTable = Table.setTotal (Dict.size subscribersDict) model.subscriberTable
                      }
                    , Effect.none
                    )

                Err error ->
                    ( { model | errors = ("Error decoding subscribers: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        "nip98AuthHeader" ->
            case ( model.state, Decode.decodeValue decodeAuthHeaderReceived message.value ) of
                ( RequestingNip96Auth apiUrl file keyHex ivHex sha256 size active total, Ok decoded ) ->
                    ( { model | state = Uploading keyHex ivHex sha256 size active total }
                    , Nip96.uploadFile apiUrl
                        model.fileId
                        { file = file
                        , status = Nip96.Uploading 0.0
                        , caption = Nothing
                        , alt = Nothing
                        , mediaType = Nothing
                        , noTransform = Just True
                        , uploadResponse = Nothing
                        }
                        UploadResultNip96
                        decoded.authHeader
                        |> Effect.sendCmd
                    )

                ( _, Err error ) ->
                    ( { model | errors = ("Error receiving NIP-98 auth header: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

                ( _, _ ) ->
                    ( model, Effect.none )

        "published" ->
            -- currently this page only publishes the list of subscribers so we don't have to check details
            update user shared (AlertTimerMessageSent (AlertTimerMessage.AddMessage "saved subscribers sucessfully" 1000)) { model | state = Saved }

        "error" ->
            -- currently this page only publishes the list of subscribers so we don't have to check details
            update user shared (AlertTimerMessageSent (AlertTimerMessage.AddMessage "error saving subscribers" 2000)) { model | state = Modified }

        _ ->
            ( model, Effect.none )


type alias ReceivedData =
    { file : File
    , ivHex : String
    , keyHex : String
    , sha256 : String
    , size : Int
    }


receivedDataDecoder : Decode.Decoder ReceivedData
receivedDataDecoder =
    Decode.map5 ReceivedData
        (Decode.field "file" File.decoder)
        (Decode.field "ivHex" Decode.string)
        (Decode.field "keyHex" Decode.string)
        (Decode.field "sha256" Decode.string)
        (Decode.field "size" Decode.int)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage



-- VIEW


subscribersTableConfig : BrowserEnv -> Table.Config Subscriber Msg
subscribersTableConfig browserEnv =
    Table.customConfig
        { toId = .email
        , toMsg = NewTableState
        , columns =
            [ Table.veryCustomColumn
                { id = Subscribers.fieldName FieldEmail
                , name = translatedFieldName browserEnv.translations FieldEmail
                , viewData = \subscriber -> editSubscriberButton subscriber
                , sorter = Table.unsortable
                }
            , Table.stringColumn (Subscribers.fieldName FieldFirstName) (translatedFieldName browserEnv.translations FieldFirstName) (\subscriber -> subscriber.firstName |> Maybe.withDefault "")
            , Table.stringColumn (Subscribers.fieldName FieldLastName) (translatedFieldName browserEnv.translations FieldLastName) (\subscriber -> subscriber.lastName |> Maybe.withDefault "")
            , Table.stringColumn (Subscribers.fieldName FieldTags) (translatedFieldName browserEnv.translations FieldTags) (\subscriber -> subscriber.tags |> Maybe.map (String.join ", ") |> Maybe.withDefault "")
            , Table.stringColumn (Subscribers.fieldName FieldDateSubscription) (translatedFieldName browserEnv.translations FieldDateSubscription) (\subscriber -> subscriber.dateSubscription |> BrowserEnv.formatDate browserEnv)
            , Table.stringColumn (Subscribers.fieldName FieldDateUnsubscription) (translatedFieldName browserEnv.translations FieldDateUnsubscription) (\subscriber -> subscriber.dateUnsubscription |> Maybe.map (BrowserEnv.formatDate browserEnv) |> Maybe.withDefault "")
            , Table.stringColumn (Subscribers.fieldName FieldSource) (translatedFieldName browserEnv.translations FieldSource) (\subscriber -> subscriber.source |> Maybe.withDefault "")
            , Table.stringColumn (Subscribers.fieldName FieldUndeliverable) (translatedFieldName browserEnv.translations FieldUndeliverable) (\subscriber -> subscriber.undeliverable |> Maybe.withDefault "")
            , Table.stringColumn (Subscribers.fieldName FieldLocale) (translatedFieldName browserEnv.translations FieldLocale) (\subscriber -> subscriber.locale |> Maybe.withDefault "")
            , Table.veryCustomColumn
                { id = "delete_entry"
                , name = ""
                , viewData = \subscriber -> removeSubscriberButton (RemoveSubscriber subscriber.email)
                , sorter = Table.unsortable
                }
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = Table.defaultCustomizations.tableAttrs
            }
        }


editSubscriberButton : Subscriber -> Table.HtmlDetails Msg
editSubscriberButton subscriber =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    Table.HtmlDetails []
        [ div
            ([ css
                [ Tw.cursor_pointer
                ]
             , Events.onClick (OpenEditSubscriberDialog subscriber)
             ]
                ++ styles.colorStylePrimaryButtonText
            )
            [ text subscriber.email
            ]
            |> Html.toUnstyled
        ]


removeSubscriberButton : Msg -> Table.HtmlDetails Msg
removeSubscriberButton removeMsg =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    Table.HtmlDetails []
        [ div
            ([ css
                [ Tw.cursor_pointer
                ]
             , Events.onClick removeMsg
             ]
                ++ styles.colorStylePrimaryButtonText
            )
            [ Icon.FeatherIcon FeatherIcons.delete
                |> Icon.view
            ]
            |> Html.toUnstyled
        ]


view : Auth.User -> Shared.Model.Model -> Model -> View Msg
view user shared model =
    let
        styles =
            stylesForTheme shared.theme

        subscribersCount =
            Dict.size model.subscribers

        subscribersCountText =
            if subscribersCount > 1 then
                Translations.subscribersCount [ shared.browserEnv.translations ] { subscribersCount = String.fromInt subscribersCount }

            else
                ""
    in
    { title = Translations.Sidebar.subscribersMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.m_2
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.items_center
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ Button.new
                    { label = Translations.importButtonTitle [ shared.browserEnv.translations ]
                    , onClick = Just <| ImportClicked
                    , theme = shared.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.view
                , Button.new
                    { label = Translations.exportButtonTitle [ shared.browserEnv.translations ]
                    , onClick = Just <| ExportClicked
                    , theme = shared.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled (Dict.size model.subscribers < 1)
                    |> Button.view
                , Button.new
                    { label = Translations.saveButtonTitle [ shared.browserEnv.translations ]
                    , onClick = model.serverDesc |> Maybe.map (\serverDesc -> SaveClicked Pareto.paretoNip96Server serverDesc.apiUrl)
                    , theme = shared.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled (model.state /= Modified)
                    |> Button.view
                , div
                    (styles.colorStyleGrayscaleMuted
                        ++ [ css
                                [ Tw.ml_4
                                ]
                           ]
                    )
                    [ text subscribersCountText ]
                ]
            , viewSubscribers shared.browserEnv model
            , viewModifications shared.theme shared.browserEnv model
            , EmailImportDialog.new
                { model = model.emailImportDialog
                , toMsg = EmailImportDialogSent
                , nostr = shared.nostr
                , pubKey = user.pubKey
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> EmailImportDialog.view
            , SubscriberEditDialog.new
                { model = model.subscriberEditDialog
                , toMsg = SubscriberEditDialogSent
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> SubscriberEditDialog.view
            , AlertTimerMessage.new
                { model = model.alertTimerMessage
                , theme = shared.theme
                }
                |> AlertTimerMessage.view
            ]
        ]
    }


viewSubscribers : BrowserEnv -> Model -> Html Msg
viewSubscribers browserEnv model =
    case ( model.state, Dict.size model.subscribers ) of
        ( Loading, _ ) ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ text <| Translations.loadingSubscribersText [ browserEnv.translations ]
                , Loaders.rings [] |> Html.fromUnstyled
                ]

        ( ErrorLoadingSubscribers error, _ ) ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ text <| Translations.errorLoadingSubscribersText [ browserEnv.translations ] ++ ": " ++ error
                ]

        ( _, 0 ) ->
            div
                []
                [ text <| Translations.noSubscribersText [ browserEnv.translations ]
                ]

        ( _, _ ) ->
            div
                [ css
                    [ Tw.m_2
                    ]
                ]
                [ Table.view
                    (subscribersTableConfig browserEnv)
                    model.subscriberTable
                    (Dict.values model.subscribers)
                    |> Html.fromUnstyled
                ]



{-
   ul
       [ css
           [ Tw.flex
           , Tw.flex_col
           , Tw.m_2
           ]
       ]
       (model.subscribers
           |> Dict.values
           |> List.map viewSubscriber
       )
-}


viewModifications : Theme -> BrowserEnv -> Model -> Html Msg
viewModifications theme browserEnv model =
    let
        unprocessedModifications =
            model.modifications
                |> List.filter
                    (\modification ->
                        case modification of
                            Subscription { email } ->
                                Dict.get email model.subscribers
                                    -- subscriber is unsubscribed - allow to resubscribe
                                    |> Maybe.map (\subscriber -> subscriber.dateUnsubscription /= Nothing)
                                    -- subscriber doesn't exist
                                    |> Maybe.withDefault True

                            Unsubscription { email } ->
                                Dict.get email model.subscribers
                                    -- subscriber is already unsubscribed
                                    |> Maybe.map (\subscriber -> subscriber.dateUnsubscription == Nothing)
                                    -- subscriber doesn't exist - ignore unsubscription
                                    |> Maybe.withDefault False
                    )
    in
    if List.length unprocessedModifications > 0 then
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.m_2
                ]
            ]
            [ b
                []
                [ text <| Translations.newModifications [ browserEnv.translations ] ]
            , Button.new
                { label = Translations.processModificationsButtonTitle [ browserEnv.translations ]
                , onClick = Just <| ProcessModificationsClicked
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , ul
                [ css
                    []
                ]
                (List.map (viewModification browserEnv) unprocessedModifications)
            ]

    else
        emptyHtml


viewModification : BrowserEnv -> Modification -> Html Msg
viewModification browserEnv modification =
    case modification of
        Subscription subscriber ->
            li []
                [ text <| modificationToString modification ++ ": " ++ subscriber.email ++ " (" ++ BrowserEnv.formatDate browserEnv subscriber.dateSubscription ++ ")"
                ]

        Unsubscription subscriber ->
            let
                dateSuffix =
                    subscriber.dateUnsubscription
                        |> Maybe.map
                            (\date ->
                                " (" ++ BrowserEnv.formatDate browserEnv date ++ ")"
                            )
                        |> Maybe.withDefault ""
            in
            li []
                [ text <| modificationToString modification ++ ": " ++ subscriber.email ++ dateSuffix
                ]
