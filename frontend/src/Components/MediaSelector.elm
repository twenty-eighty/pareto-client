module Components.MediaSelector exposing
    ( DisplayType(..)
    , MediaSelector
    , Model
    , Msg
    , UploadedFile(..)
    , getMediaType
    , init
    , new
    , show
    , subscribe
    , update
    , view
    , withMediaType
    , fileMetadataForUrl
    )

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown
import Components.Icon as Icon
import Components.ModalDialog as ModalDialog
import Components.UploadDialog as UploadDialog exposing (UploadResponse(..), UploadServer)
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, div, img, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Http
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode
import Nostr
import Nostr.Blossom as Blossom exposing (BlobDescriptor, userServerListFromEvent)
import Nostr.Event exposing (Event, Kind(..))
import Nostr.External
import Nostr.FileStorageServerList exposing (fileStorageServerListFromEvent)
import Nostr.Nip94 as Nip94 exposing (FileMetadata)
import Nostr.Nip96 as Nip96 exposing (extendRelativeServerDescriptorUrls)
import Nostr.Request exposing (HttpRequestMethod(..), RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey, ServerUrl)
import Ports
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.MediaSelector as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)
import Url


type MediaSelector msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , onSelected : Maybe (UploadedFile -> msg)
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


fileMetadataForUrl : Model -> String -> Maybe FileMetadata
fileMetadataForUrl (Model model) url =
    (Dict.values model.uploadedNip96Files ++ Dict.values model.uploadedBlossomFiles)
        |> List.concat
        |> List.filterMap (\uploadedFile ->
            case uploadedFile of
                Nip96File fileMetadata ->
                    if fileMetadata.url == Just url then
                        Just fileMetadata
                    else
                        Nothing

                BlossomFile blobDescriptor ->
                    if blobDescriptor.url == url then
                        blobDescriptor.nip94
                    else
                        Nothing
            )
        |> List.head


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , onSelected : Maybe (UploadedFile -> msg)
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> MediaSelector msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onSelected = props.onSelected
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


withMediaType : Nip96.MediaType -> Model -> Model
withMediaType mediaType (Model model) =
    Model { model | mediaType = Just mediaType }


getMediaType : Model -> Maybe Nip96.MediaType
getMediaType (Model model) =
    model.mediaType


type Model
    = Model
        { selected : Maybe Int
        , search : String
        , displayType : DisplayType
        , serverSelectionDropdown : Components.Dropdown.Model MediaServer
        , blossomServers : Dict ServerUrl ServerState
        , nip96Servers : Dict ServerUrl ServerState
        , nip96ServerDescResponses : Dict String Nip96.ServerDescResponse
        , authHeader : Maybe String
        , uploadedBlossomFiles : Dict ServerUrl (List UploadedFile)
        , uploadedNip96Files : Dict ServerUrl (List UploadedFile)
        , uploadDialog : UploadDialog.Model
        , mediaType : Maybe Nip96.MediaType
        , errors : List String
        }


type MediaServer
    = BlossomMediaServer ServerUrl
    | Nip96MediaServer ServerUrl
    | NoMediaServer
    | AllMediaServers


type DisplayType
    = DisplayModalDialog Bool
    | DisplayEmbedded


type ServerState
    = ServerStateUnknown
    | ServerRedirected String
    | ServerFileListReceived
    | ServerFunctioning
    | ServerFileListFailed Http.Error
    | ServerDescFailed Http.Error


type UploadedFile
    = BlossomFile Blossom.BlobDescriptor
    | Nip96File Nip94.FileMetadata


init :
    { selected : Maybe item
    , toMsg : Msg msg -> msg
    , blossomServers : List String
    , nip96Servers : List String
    , displayType : DisplayType
    }
    -> ( Model, Effect msg )
init props =
    ( Model
        { selected = Just 0
        , search = ""
        , displayType = props.displayType
        , serverSelectionDropdown = Components.Dropdown.init { selected = Just NoMediaServer }
        , blossomServers = serversWithUnknownState Dict.empty props.blossomServers
        , nip96Servers = serversWithUnknownState Dict.empty props.nip96Servers
        , nip96ServerDescResponses = Dict.empty
        , authHeader = Nothing
        , uploadedBlossomFiles = Dict.empty
        , uploadedNip96Files = Dict.empty
        , uploadDialog =
            UploadDialog.init
                { toMsg = UploadDialogSent
                }
        , errors = []
        , mediaType = Nothing
        }
    , Effect.batch
        [ requestBlossomListAuths props.blossomServers
        , requestNip96ServerSpecs props.nip96Servers
        ]
        |> Effect.map props.toMsg
    )


serversWithUnknownState : Dict String ServerState -> List String -> Dict String ServerState
serversWithUnknownState dict serverUrls =
    serverUrls
        |> List.foldl
            (\serverUrl acc ->
                if Dict.member serverUrl acc then
                    acc

                else
                    Dict.insert serverUrl ServerStateUnknown acc
            )
            dict


requestBlossomListAuths : List String -> Effect (Msg msg)
requestBlossomListAuths blossomServers =
    blossomServers
        |> List.map
            (\blossomServer ->
                Effect.sendCmd <| Ports.requestBlossomAuth 1 blossomServer "List Blobs" GetRequest
            )
        |> Effect.batch


requestNip96ServerSpecs : List String -> Effect (Msg msg)
requestNip96ServerSpecs nip96Servers =
    nip96Servers
        |> List.map
            (\nip96Server ->
                Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc nip96Server) nip96Server)
            )
        |> Effect.batch


show : Model -> Model
show (Model model) =
    Model { model | displayType = DisplayModalDialog True }


type Msg msg
    = FocusedDropdown
    | BlurredDropdown
    | ShowMessage String
    | CloseDialog
    | DropdownSent (Components.Dropdown.Msg MediaServer (Msg msg))
    | ChangedSelectedServer MediaServer
    | ConfigureDefaultMediaServer
    | Upload
    | Uploaded UploadDialog.UploadResponse
    | UploadDialogSent UploadDialog.Msg
    | IncomingMessage { messageType : String, value : Encode.Value }
    | ReceivedBlossomFileList String (Result Http.Error (List BlobDescriptor))
    | ReceivedNip96ServerDesc String (Result Http.Error Nip96.ServerDescResponse)
    | ReceivedNip96FileList String (Result Http.Error Nip96.FileList)
    | SelectedItem
        { item : UploadedFile
        , onSelected : Maybe (UploadedFile -> msg)
        }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , pubKey : PubKey
    , nostr : Nostr.Model
    , browserEnv : BrowserEnv
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
    case props.msg of
        FocusedDropdown ->
            ( Model { model | displayType = DisplayModalDialog True }, Effect.none )
                |> toParentModel

        BlurredDropdown ->
            ( Model { model | displayType = DisplayModalDialog False }, Effect.none )
                |> toParentModel

        ShowMessage message ->
            ( Model model, Effect.sendSharedMsg <| Shared.Msg.ShowAlert message )
                |> toParentModel

        CloseDialog ->
            ( Model { model | displayType = DisplayModalDialog False }, Effect.none )
                |> toParentModel

        DropdownSent innerMsg ->
            let
                ( newModel, effect ) =
                    Components.Dropdown.update
                        { msg = innerMsg
                        , model = model.serverSelectionDropdown
                        , toModel = \dropdown -> Model { model | serverSelectionDropdown = dropdown }
                        , toMsg = DropdownSent
                        }
            in
            ( newModel, Effect.map props.toMsg effect )
                |> toParentModel

        ChangedSelectedServer selectedServer ->
            let
                uploadServer =
                    case selectedServer of
                        BlossomMediaServer serverUrl ->
                            Just <| UploadDialog.UploadServerBlossom serverUrl

                        Nip96MediaServer serverUrl ->
                            Dict.get serverUrl model.nip96ServerDescResponses
                                |> Maybe.andThen
                                    (\nip96ServerDescResponse ->
                                        case nip96ServerDescResponse of
                                            Nip96.ServerDescriptor serverDescriptorData ->
                                                Just <| UploadDialog.UploadServerNip96 serverUrl serverDescriptorData

                                            Nip96.ServerRedirect _ ->
                                                Nothing
                                    )

                        NoMediaServer ->
                            Nothing

                        AllMediaServers ->
                            preferredUploadServer (Model model)
            in
            ( Model
                { model
                    | serverSelectionDropdown =
                        Components.Dropdown.selectItem model.serverSelectionDropdown (Just selectedServer)
                    , uploadDialog =
                        UploadDialog.selectServer model.uploadDialog uploadServer
                }
            , Effect.none
            )
                |> toParentModel

        ConfigureDefaultMediaServer ->
            ( Model model
            , Effect.batch
                [ Nip96.sendNip96ServerListCmd props.browserEnv props.pubKey (Nostr.getDefaultNip96Servers props.nostr props.pubKey) (Nostr.getDefaultRelays props.nostr)
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                ]
                |> Effect.map props.toMsg
            )
                |> toParentModel

        Upload ->
            ( Model
                { model
                    | uploadDialog =
                        case model.mediaType of
                            Just mediaType ->
                                model.uploadDialog
                                    |> UploadDialog.withMediaType mediaType
                                    |> UploadDialog.show

                            Nothing ->
                                model.uploadDialog
                                    |> UploadDialog.show
                }
            , Effect.none
            )
                |> toParentModel

        Uploaded uploadResponse ->
            modelWithUploadedFile props.model uploadResponse
                |> toParentModel

        UploadDialogSent innerMsg ->
            UploadDialog.update
                { msg = innerMsg
                , model = model.uploadDialog
                , toModel = \uploadDialog -> Model { model | uploadDialog = uploadDialog }
                , toMsg = props.toMsg << UploadDialogSent
                , onUploaded = props.toMsg << Uploaded
                , pubKey = props.pubKey
                , nostr = props.nostr
                , browserEnv = props.browserEnv
                }
                |> toParentModel

        IncomingMessage { messageType, value } ->
            processIncomingMessage props.pubKey props.model messageType props.toMsg value
                |> toParentModel

        SelectedItem data ->
            case data.onSelected of
                Just onSelected ->
                    ( Model
                        { model
                            | selected = Just 1
                            , displayType = DisplayModalDialog False
                        }
                    , Effect.sendMsg (onSelected data.item)
                    )
                        |> toParentModel

                Nothing ->
                    ( Model { model | selected = Just 1 }
                    , Effect.none
                    )
                        |> toParentModel

        ReceivedBlossomFileList serverUrl result ->
            case result of
                Ok fileList ->
                    ( updateModelWithBlossomFileList (Model model) serverUrl fileList, Effect.none )
                        |> toParentModel

                Err error ->
                    ( Model
                        { model
                            | errors = model.errors ++ [ serverUrl ++ " (Blossom): " ++ httpErrorToString error ]
                            , blossomServers = updateServerState model.blossomServers serverUrl (ServerFileListFailed error)
                        }
                    , Effect.none
                    )
                        |> toParentModel

        ReceivedNip96ServerDesc serverUrl result ->
            case result of
                Ok (Nip96.ServerRedirect serverRedirection) ->
                    ( Model
                        { model
                            | nip96ServerDescResponses = Dict.insert serverUrl (Nip96.ServerRedirect serverRedirection) model.nip96ServerDescResponses
                            , nip96Servers = updateServerState model.nip96Servers serverUrl (ServerRedirected serverRedirection.delegated_to_url)
                        }
                    , Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc serverUrl) serverRedirection.delegated_to_url)
                        |> Effect.map props.toMsg
                    )
                        |> toParentModel

                Ok (Nip96.ServerDescriptor serverDescriptorData) ->
                    let
                        serverDescWithExtendedUrls =
                            extendRelativeServerDescriptorUrls serverUrl serverDescriptorData
                    in
                    ( Model
                        { model
                            | nip96ServerDescResponses = Dict.insert serverUrl (Nip96.ServerDescriptor serverDescWithExtendedUrls) model.nip96ServerDescResponses
                            , nip96Servers = updateServerState model.nip96Servers serverUrl ServerFunctioning
                        }
                    , GetRequest
                        |> RequestNip98Auth serverUrl serverDescWithExtendedUrls.apiUrl
                        |> Nostr.createRequest props.nostr "NIP-96 auth request for file list of server" []
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    )
                        |> toParentModel

                Err error ->
                    ( Model
                        { model
                            | errors = model.errors ++ [ serverUrl ++ " (NIP-96): " ++ httpErrorToString error ]
                            , nip96Servers = updateServerState model.nip96Servers serverUrl (ServerDescFailed error)
                        }
                    , Effect.none
                    )
                        |> toParentModel

        ReceivedNip96FileList serverUrl result ->
            case result of
                Ok fileList ->
                    ( updateModelWithNip96FileList (Model model) serverUrl fileList, Effect.none )
                        |> toParentModel

                Err error ->
                    ( Model
                        { model
                            | errors = model.errors ++ [ serverUrl ++ " (NIP-96): " ++ httpErrorToString error ]
                            , nip96Servers = updateServerState model.nip96Servers serverUrl (ServerFileListFailed error)
                        }
                    , Effect.none
                    )
                        |> toParentModel


modelWithUploadedFile : Model -> UploadResponse -> ( Model, Effect msg )
modelWithUploadedFile model uploadResponse =
    case uploadResponse of
        UploadResponseBlossom apiUrl fileMetadata ->
            modelWithUploadedBlossomFile model apiUrl fileMetadata

        UploadResponseNip96 apiUrl fileMetadata ->
            modelWithUploadedNip96File model apiUrl fileMetadata


modelWithUploadedBlossomFile : Model -> String -> Blossom.BlobDescriptor -> ( Model, Effect msg )
modelWithUploadedBlossomFile (Model model) apiUrl blobDescriptor =
    let
        uploadedFile =
            BlossomFile blobDescriptor

        uploadedBlossomFiles =
            Dict.update apiUrl
                (\maybeFilesList ->
                    case maybeFilesList of
                        Just fileList ->
                            Just <| uploadedFile :: fileList

                        Nothing ->
                            Just [ uploadedFile ]
                )
                model.uploadedBlossomFiles
    in
    ( Model { model | uploadedBlossomFiles = uploadedBlossomFiles }, Effect.none )


modelWithUploadedNip96File : Model -> String -> Nip94.FileMetadata -> ( Model, Effect msg )
modelWithUploadedNip96File (Model model) apiUrl fileMetadata =
    let
        uploadedFile =
            Nip96File fileMetadata

        uploadedNip96Files =
            Dict.update apiUrl
                (\maybeFilesList ->
                    case maybeFilesList of
                        Just fileList ->
                            Just <| uploadedFile :: fileList

                        Nothing ->
                            Just [ uploadedFile ]
                )
                model.uploadedNip96Files
    in
    ( Model { model | uploadedNip96Files = uploadedNip96Files }, Effect.none )


updateModelWithBlossomFileList : Model -> String -> List Blossom.BlobDescriptor -> Model
updateModelWithBlossomFileList (Model model) serverUrl fileList =
    let
        blossomServers =
            updateServerState model.blossomServers serverUrl ServerFileListReceived

        modelWithServerList =
            { model
                | uploadedBlossomFiles = Dict.insert serverUrl (List.map BlossomFile fileList) model.uploadedNip96Files
                , blossomServers = blossomServers
                , uploadDialog = updateUploadDialogServerList model.nip96ServerDescResponses model.uploadDialog blossomServers model.nip96Servers
            }
    in
    Model
        { modelWithServerList
            | serverSelectionDropdown =
                selectableMediaServers (Model modelWithServerList)
                    |> List.head
                    |> Maybe.withDefault NoMediaServer
                    |> Just
                    |> Components.Dropdown.selectItem model.serverSelectionDropdown
        }


updateUploadDialogServerList : Dict String Nip96.ServerDescResponse -> UploadDialog.Model -> Dict String ServerState -> Dict String ServerState -> UploadDialog.Model
updateUploadDialogServerList nip96ServerDescResponses uploadDialogModel blossomServers nip96Servers =
    let
        workingBlossomServers =
            blossomServers
                |> Dict.toList
                |> List.filterMap
                    (\( url, status ) ->
                        -- only consider servers that delivered a file list eligible for uploading files
                        if status == ServerFileListReceived then
                            Just url

                        else
                            Nothing
                    )
                |> List.map UploadDialog.UploadServerBlossom

        workingNip96Servers =
            nip96Servers
                |> Dict.toList
                |> List.filterMap
                    (\( url, status ) ->
                        -- only consider servers that delivered a file list eligible for uploading files
                        if status == ServerFileListReceived then
                            Just url

                        else
                            Nothing
                    )
                |> List.filterMap
                    (\serverUrl ->
                        case Dict.get serverUrl nip96ServerDescResponses of
                            Just (Nip96.ServerDescriptor serverDescriptorData) ->
                                Just (UploadDialog.UploadServerNip96 serverUrl serverDescriptorData)

                            _ ->
                                Nothing
                    )
    in
    UploadDialog.updateServerList uploadDialogModel (workingNip96Servers ++ workingBlossomServers)


updateModelWithNip96FileList : Model -> String -> Nip96.FileList -> Model
updateModelWithNip96FileList (Model model) serverUrl fileList =
    let
        nip96Servers =
            updateServerState model.nip96Servers serverUrl ServerFileListReceived

        modelWithServerList =
            { model
                | uploadedNip96Files = Dict.insert serverUrl (List.map Nip96File fileList.files) model.uploadedNip96Files
                , nip96Servers = nip96Servers
                , uploadDialog = updateUploadDialogServerList model.nip96ServerDescResponses model.uploadDialog model.blossomServers nip96Servers
            }
    in
    Model
        { modelWithServerList
            | serverSelectionDropdown =
                selectableMediaServers (Model modelWithServerList)
                    |> List.head
                    |> Maybe.withDefault NoMediaServer
                    |> Just
                    |> Components.Dropdown.selectItem model.serverSelectionDropdown
        }


updateServerState : Dict String ServerState -> String -> ServerState -> Dict String ServerState
updateServerState dict serverUrl state =
    Dict.insert serverUrl state dict


processIncomingMessage : PubKey -> Model -> String -> (Msg msg -> msg) -> Encode.Value -> ( Model, Effect msg )
processIncomingMessage pubKey xModel messageType toMsg value =
    let
        (Model model) =
            xModel
    in
    case messageType of
        "blossomAuthHeader" ->
            case Decode.decodeValue decodeAuthHeaderReceived value of
                Ok decoded ->
                    ( Model { model | authHeader = Just decoded.authHeader }
                    , Blossom.fetchFileList (ReceivedBlossomFileList decoded.serverUrl) decoded.authHeader decoded.serverUrl pubKey
                        |> Cmd.map toMsg
                        |> Effect.sendCmd
                    )

                Err error ->
                    ( Model { model | errors = model.errors ++ [ Decode.errorToString error ] }, Effect.none )

        "nip98AuthHeader" ->
            case Decode.decodeValue decodeAuthHeaderReceived value of
                Ok decoded ->
                    if decoded.method == "GET" then
                        ( Model { model | authHeader = Just decoded.authHeader }
                        , Nip96.fetchFileList (ReceivedNip96FileList decoded.serverUrl) decoded.authHeader decoded.apiUrl
                            |> Cmd.map toMsg
                            |> Effect.sendCmd
                        )

                    else
                        ( Model model, Effect.none )

                Err error ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding NIP-98 auth header: " ++ Decode.errorToString error ] }, Effect.none )

        -- the file server lists may appear after the media selector is instantiated
        -- so we have to process them when arriving
        "events" ->
            case
                ( Nostr.External.decodeEventsKind value
                , Nostr.External.decodeEvents value
                )
            of
                ( Ok KindUserServerList, Ok events ) ->
                    -- list with Blossom servers
                    updateModelWithUserServerLists (Model model) pubKey toMsg events

                ( Ok KindFileStorageServerList, Ok events ) ->
                    -- list with NIP-96 servers
                    updateModelWithFileStorageServerLists (Model model) pubKey toMsg events

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )


decodeAuthHeaderReceived : Decode.Decoder AuthHeaderReceived
decodeAuthHeaderReceived =
    Decode.map5 AuthHeaderReceived
        (Decode.field "requestId" Decode.int)
        (Decode.field "method" Decode.string)
        (Decode.field "authHeader" Decode.string)
        (Decode.field "serverUrl" Decode.string)
        (Decode.field "apiUrl" Decode.string)


type alias AuthHeaderReceived =
    { requestId : Int
    , method : String
    , authHeader : String
    , serverUrl : String
    , apiUrl : String
    }


updateModelWithUserServerLists : Model -> PubKey -> (Msg msg -> msg) -> List Event -> ( Model, Effect msg )
updateModelWithUserServerLists (Model model) pubKey toMsg events =
    let
        -- usually there should be only one for the logged-in user
        newUserServers =
            events
                |> List.filter (\event -> pubKey == event.pubKey)
                |> List.map userServerListFromEvent
                |> List.foldl
                    (\( _, userServerList ) servers ->
                        servers ++ userServerList
                    )
                    []
                |> List.filter
                    (\server ->
                        not <| Dict.member server model.blossomServers
                    )

        newServerDict =
            serversWithUnknownState model.blossomServers newUserServers

        listAuthRequests =
            requestBlossomListAuths newUserServers
                |> Effect.map toMsg
    in
    ( Model { model | blossomServers = newServerDict }, listAuthRequests )


updateModelWithFileStorageServerLists : Model -> PubKey -> (Msg msg -> msg) -> List Event -> ( Model, Effect msg )
updateModelWithFileStorageServerLists (Model model) pubKey toMsg events =
    let
        -- usually there should be only one for the logged-in user
        newFileStorageServers =
            events
                |> List.filter (\event -> pubKey == event.pubKey)
                |> List.map fileStorageServerListFromEvent
                |> List.foldl
                    (\( _, fileStorageServerList ) servers ->
                        servers ++ fileStorageServerList
                    )
                    []
                |> List.filter
                    (\server ->
                        not <| Dict.member server model.nip96Servers
                    )

        newServerDict =
            serversWithUnknownState model.nip96Servers newFileStorageServers

        serverSpecRequests =
            requestNip96ServerSpecs newFileStorageServers
                |> Effect.map toMsg
    in
    ( Model { model | nip96Servers = newServerDict }, serverSpecRequests )


view : MediaSelector msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    case model.displayType of
        DisplayModalDialog False ->
            emptyHtml

        DisplayModalDialog True ->
            ModalDialog.new
                { title = Translations.selectImageDialogTitle [ settings.browserEnv.translations ]
                , content = [ viewMediaSelector (Settings settings) ]
                , onClose = settings.toMsg CloseDialog
                , theme = settings.theme
                , buttons = [ ]
                }
                |> ModalDialog.view

        DisplayEmbedded ->
            viewMediaSelector (Settings settings)


viewMediaSelector : MediaSelector msg -> Html msg
viewMediaSelector (Settings settings) =
    let
        (Model model) =
            settings.model

        selectableServers =
            selectableMediaServers (Model model)

        filesToShow =
            case Components.Dropdown.selectedItem model.serverSelectionDropdown of
                Just (Nip96MediaServer serverUrl) ->
                    uploadedNip96ImageFiles model.uploadedNip96Files serverUrl

                Just (BlossomMediaServer serverUrl) ->
                    Dict.get serverUrl model.uploadedBlossomFiles
                        |> Maybe.withDefault []

                Just AllMediaServers ->
                    -- respect order of server list
                    (Dict.keys model.blossomServers
                        |> List.filterMap (\blossomServer -> Dict.get blossomServer model.uploadedBlossomFiles)
                        |> List.concat
                    )
                        ++ (Dict.keys model.nip96Servers
                                |> List.map (uploadedNip96ImageFiles model.uploadedNip96Files)
                                |> List.concat
                           )

                Just NoMediaServer ->
                    []

                Nothing ->
                    []
    in
    div []
        [ div
            [ css
                [ Tw.mt_6
                , Tw.flex
                , Tw.justify_between
                , Tw.pb_4
                , Tw.border_b_2
                , Tw.gap_2
                ]
            ]
            [ Components.Dropdown.new
                { model = model.serverSelectionDropdown
                , toMsg = DropdownSent
                , choices = selectableServers
                , allowNoSelection = False
                , toLabel = mediaServerToString settings.browserEnv.translations << Maybe.withDefault NoMediaServer
                }
                |> Components.Dropdown.withOnChange (ChangedSelectedServer << Maybe.withDefault NoMediaServer)
                |> Components.Dropdown.view
                |> Html.map settings.toMsg
            , Button.new
                { label = Translations.uploadButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just Upload
                , theme = settings.theme
                }
                |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.upload)
                |> Button.withDisabled (List.head selectableServers == Just NoMediaServer)
                |> Button.view
                |> Html.map settings.toMsg
            ]
        , viewInstruction settings.browserEnv.translations model.displayType filesToShow
        , viewImages (Settings settings) filesToShow
        , viewConfigureMediaServerMessage (Settings settings)
        , UploadDialog.new
            { model = model.uploadDialog
            , toMsg = UploadDialogSent
            , pubKey = settings.pubKey
            , browserEnv = settings.browserEnv
            , theme = settings.theme
            }
            |> UploadDialog.view
            |> Html.map settings.toMsg
        ]



-- Show instruction how to select image depending on display mode
-- Only show if there's at least one image to select


viewInstruction : I18Next.Translations -> DisplayType -> List UploadedFile -> Html msg
viewInstruction translations displayType filesToShow =
    case ( displayType, List.length filesToShow > 0 ) of
        ( DisplayModalDialog _, True ) ->
            div
                [ css
                    [ Tw.my_2
                    ]
                ]
                [ text <| Translations.imageSelectionInstructionalText [ translations ]
                ]

        ( _, _ ) ->
            emptyHtml


viewConfigureMediaServerMessage : MediaSelector msg -> Html msg
viewConfigureMediaServerMessage (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    if Dict.isEmpty model.blossomServers && Dict.isEmpty model.nip96Servers then
        div
            [ css
                [ Tw.flex
                , Tw.justify_center
                , Tw.items_center
                , Tw.flex_col
                , Tw.my_4
                , Tw.gap_4
                ]
            ]
            [ text <| Translations.noServerConfiguredMessage [ settings.browserEnv.translations ]
            , Button.new
                { label = Translations.configureServerButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just ConfigureDefaultMediaServer
                , theme = settings.theme
                }
                |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.settings)
                |> Button.view
                |> Html.map settings.toMsg
            ]

    else
        emptyHtml


viewImages : MediaSelector msg -> List UploadedFile -> Html msg
viewImages (Settings settings) filesToShow =
    let
        (Model model) =
            settings.model

        dialogAttributes =
            case model.displayType of
                DisplayModalDialog _ ->
                    [ Tw.max_h_96
                    , Tw.w_screen
                    , Tw.max_w_full
                    , Tw.overflow_y_auto
                    , Bp.lg
                        [ Tw.grid_cols_4
                        ]
                    , Bp.md
                        [ Tw.grid_cols_3
                        ]
                    , Tw.grid_cols_2
                    ]

                DisplayEmbedded ->
                    [ Bp.xxl
                        [ Tw.grid_cols_6
                        ]
                    , Bp.xl
                        [ Tw.grid_cols_5
                        ]
                    , Bp.lg
                        [ Tw.grid_cols_4
                        ]
                    , Bp.md
                        [ Tw.grid_cols_3
                        ]
                    , Bp.sm
                        [ Tw.grid_cols_2
                        ]
                    , Tw.grid_cols_1
                    ]
    in
    Keyed.node "div"
        [ css
            (dialogAttributes
                ++ [ Tw.grid
                   , Tw.gap_4
                   , Tw.mt_4
                   ]
            )
        ]
        (filesToShow
            |> List.map
                (\fileToShow ->
                    let
                        uniqueFileId =
                            uniqueIdForUploadedFile fileToShow
                    in
                    ( uniqueFileId
                    , Lazy.lazy5 imagePreview settings.browserEnv.translations settings.onSelected model.displayType uniqueFileId fileToShow
                        |> Html.map settings.toMsg
                    )
                )
        )


preferredUploadServer : Model -> Maybe UploadServer
preferredUploadServer (Model model) =
    availableMediaServers (Model model)
        |> List.head
        |> Maybe.andThen
            (\mediaServer ->
                case mediaServer of
                    BlossomMediaServer serverUrl ->
                        Just <| UploadDialog.UploadServerBlossom serverUrl

                    Nip96MediaServer serverUrl ->
                        Dict.get serverUrl model.nip96ServerDescResponses
                            |> Maybe.andThen
                                (\serverDescResponse ->
                                    case serverDescResponse of
                                        Nip96.ServerDescriptor serverDescriptorData ->
                                            Just <| UploadDialog.UploadServerNip96 serverUrl serverDescriptorData

                                        _ ->
                                            Nothing
                                )

                    _ ->
                        Nothing
            )


selectableMediaServers : Model -> List MediaServer
selectableMediaServers (Model model) =
    let
        servers =
            availableMediaServers (Model model)
    in
    case List.length servers of
        0 ->
            [ NoMediaServer ]

        1 ->
            servers

        _ ->
            AllMediaServers :: servers


availableMediaServers : Model -> List MediaServer
availableMediaServers (Model model) =
    let
        selectableBlossomServers =
            model.blossomServers
                |> Dict.toList
                |> List.filterMap
                    (\( serverUrl, state ) ->
                        if state == ServerFileListReceived then
                            Just (BlossomMediaServer serverUrl)

                        else
                            Nothing
                    )

        selectableNip96Servers =
            model.nip96Servers
                |> Dict.toList
                |> List.filterMap
                    (\( serverUrl, state ) ->
                        if state == ServerFileListReceived then
                            Just (Nip96MediaServer serverUrl)

                        else
                            Nothing
                    )
    in
    selectableNip96Servers ++ selectableBlossomServers


mediaServerToString : I18Next.Translations -> MediaServer -> String
mediaServerToString translations mediaServer =
    case mediaServer of
        AllMediaServers ->
            Translations.allServersListboxEntry [ translations ]

        NoMediaServer ->
            Translations.noServerListboxEntry [ translations ]

        BlossomMediaServer serverUrl ->
            hostOfUrl serverUrl ++ " (Blossom)"

        Nip96MediaServer serverUrl ->
            hostOfUrl serverUrl ++ " (NIP-96)"


hostOfUrl : String -> String
hostOfUrl url =
    case Url.fromString url of
        Just { host } ->
            host

        Nothing ->
            url


uploadedNip96ImageFiles : Dict String (List UploadedFile) -> String -> List UploadedFile
uploadedNip96ImageFiles uploadedNip96Files serverUrl =
    -- filter non-image files
    -- later we could add displaying of video or audio files
    Dict.get serverUrl uploadedNip96Files
        |> Maybe.map
            (\uploadedFiles ->
                uploadedFiles
                    |> List.filter
                        (\uploadedFile ->
                            case uploadedFile of
                                Nip96File fileMetadata ->
                                    Nip94.isImage fileMetadata || Nip94.isAudio fileMetadata

                                BlossomFile _ ->
                                    -- don't know what type blossom file is
                                    True
                        )
            )
        |> Maybe.withDefault []


imagePreview : I18Next.Translations -> Maybe (UploadedFile -> msg) -> DisplayType -> String -> UploadedFile -> Html (Msg msg)
imagePreview translations onSelected displayType uniqueFileId uploadedFile =
    let
        multiSelection =
            -- In case we will support selection of multiple files,
            -- a single click/tap should not commit the selection but only select.
            -- An extra button is then needed to commit the selection.
            False

        selectionAttr =
            if multiSelection then
                [ Events.onDoubleClick (SelectedItem { item = uploadedFile, onSelected = onSelected }) ]

            else
                [ Events.onClick (SelectedItem { item = uploadedFile, onSelected = onSelected }) ]

        commonAttributes =
            selectionAttr
                ++ [ css
                        [ Tw.w_full
                        , Tw.h_auto
                        , Tw.max_h_56
                        , Tw.rounded_lg
                        , Tw.flex
                        , Tw.items_center
                        , Tw.justify_center
                        , Tw.pr_1
                        , Tw.pb_1
                        , Tw.drop_shadow_md
                        , Css.hover
                            [ Tw.pr_0
                            , Tw.pb_0
                            , Tw.drop_shadow_sm
                            ]
                        ]
                   ]
    in
    case uploadedFile of
        Nip96File nip96File ->
            let
                imageWidth =
                    200

                imageUrl =
                    if Nip94.isImage nip96File then
                        nip96File.url
                            |> Maybe.map
                                (\url ->
                                    url ++ "?w=" ++ String.fromInt imageWidth
                                 -- NIP-96 servers can return scaled versions of images
                                )
                            |> Maybe.withDefault ""

                    else if Nip94.isPdf nip96File then
                        "/images/pdf-placeholder.jpeg"

                    else if Nip94.isAudio nip96File then
                        "/images/audio-placeholder.jpeg"

                    else if Nip94.isVideo nip96File then
                        "/images/video-placeholder.jpeg"

                    else
                        "Binary"
            in
            div
                [ css
                    [ Tw.relative
                    ]
                ]
                [ img
                    (commonAttributes
                        ++ [ css
                                []
                           , Attr.alt <| Maybe.withDefault "" nip96File.alt
                           , Attr.src imageUrl
                           ]
                    )
                    []
                , viewCopyButton translations displayType (Maybe.withDefault "" nip96File.url) uniqueFileId
                ]

        BlossomFile blobDescriptor ->
            div
                [ css
                    [ Tw.relative
                    ]
                ]
                [ img
                    (commonAttributes
                        ++ [ css
                                []
                           , Attr.src blobDescriptor.url
                           ]
                    )
                    []
                , viewCopyButton translations displayType blobDescriptor.url uniqueFileId
                ]



-- display copy to clipboard button only in embedded mode, not in small dialog


viewCopyButton : I18Next.Translations -> DisplayType -> String -> String -> Html (Msg msg)
viewCopyButton translations displayType url uniqueId =
    case displayType of
        DisplayModalDialog _ ->
            emptyHtml

        DisplayEmbedded ->
            copyButton translations url uniqueId


uniqueIdForUploadedFile : UploadedFile -> String
uniqueIdForUploadedFile uploadedFile =
    case uploadedFile of
        Nip96File nip96File ->
            uniqueIdForFileMetadata nip96File

        BlossomFile blobDescriptor ->
            blobDescriptor.sha256


uniqueIdForFileMetadata : FileMetadata -> String
uniqueIdForFileMetadata fileMetadata =
    fileMetadata.xHash
        |> Maybe.withDefault (String.fromInt fileMetadata.createdAt)


copyButton : I18Next.Translations -> String -> String -> Html (Msg msg)
copyButton translations copyText uniqueId =
    let
        elementId =
            clipboardElementId ++ "-" ++ uniqueId
    in
    div
        [ css
            [ Tw.absolute
            , Tw.top_0
            , Tw.right_0
            , Tw.p_3
            , Tw.text_color Theme.white
            , Tw.bg_color Theme.black
            , Tw.bg_opacity_50
            , Tw.rounded_md
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.cursor_pointer
                ]
            , Attr.id elementId
            ]
            [ Icon.FeatherIcon FeatherIcons.clipboard
                |> Icon.view
            ]
        , Html.node "js-clipboard-component"
            [ Attr.property "buttonId" (Encode.string elementId)
            , Attr.property "copyContent" (Encode.string copyText)
            , Events.on "copiedToClipboard" (Decode.succeed (ShowMessage <| Translations.copiedLinkAlertMessage [ translations ]))
            ]
            []
        ]


clipboardElementId : String
clipboardElementId =
    "copy-to-clipboard"


subscribe : Model -> Sub (Msg msg)
subscribe (Model model) =
    Sub.batch
        [ Ports.receiveMessage IncomingMessage
        , Sub.map UploadDialogSent (UploadDialog.subscribe model.uploadDialog)
        ]
