module Components.UploadDialog exposing
    ( UploadDialog, new
    , Model, init
    , Msg, update, show
    , UploadServer(..), selectServer, updateServerList
    , UploadResponse(..)
    , view
    , subscribe
    )

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown
import Components.Icon as Icon
import Css
import Dict exposing (Dict)
import FeatherIcons
import File exposing (File)
import File.Select as FileSelect
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, li, main_, option, p, progress, select, span, strong, text, textarea, ul)
import Html.Styled.Attributes as Attr exposing (checked, class, classList, css, disabled, href, type_, value)
import Html.Styled.Events as Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Nostr
import Nostr.Blossom as Blossom exposing (BlobDescriptor)
import Nostr.Nip94 as Nip94
import Nostr.Nip96 as Nip96 exposing (extendRelativeServerDescriptorUrls)
import Nostr.Request exposing (HttpRequestMethod(..), RequestData(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Ports
import Svg.Loaders
import SHA256
import Shared.Msg
import Task exposing (Task)
import Time exposing (Posix, posixToMillis, millisToPosix)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.UploadDialog as Translations
import Ui.Shared exposing (modalDialog)
import Ui.Styles exposing (Styles)
import Url
import Nostr.Nip96 exposing (ServerDescriptorData)
import Components.Dropdown as Dropdown

type UploadDialog msg
     = Settings
        { model : Model 
        , toMsg : Msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Ui.Styles.Theme
        }

new :
    { model : Model
    , toMsg : Msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Ui.Styles.Theme
    }
    -> UploadDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


type Model 
    = Model
        { state : DialogState
        , serverSelectionDropdown : Components.Dropdown.Model UploadServer
        , uploadServers : List UploadServer
        , errors : List String
        , files : Dict Int FileUpload
        , nextFileId : Int
        }

type DialogState
    = DialogClosed
    | DialogVisible

type FileUpload
    = FileUploadBlossom (Maybe String) Blossom.FileUpload
    | FileUploadNip96 (Maybe String) Nip96.FileUpload

type UploadServer
    = UploadServerBlossom String
    | UploadServerNip96 String Nip96.ServerDescriptorData



init :
    { toMsg : Msg -> msg
    }
     -> Model
init props =
    Model
        { state = DialogClosed
        , serverSelectionDropdown = Components.Dropdown.init { selected = Nothing }
        , uploadServers = []
        , files = Dict.empty
        , nextFileId = 0
        , errors = []
        }

show : Model -> Model
show (Model model) =
    Model { model | state = DialogVisible }

selectServer : Model -> Maybe UploadServer -> Model
selectServer (Model model) maybeUploadServer =
    Model
        { model |
            serverSelectionDropdown =
                Components.Dropdown.selectItem model.serverSelectionDropdown maybeUploadServer
        }

updateServerList : Model -> List UploadServer -> Model
updateServerList (Model model) uploadServers =
    case Components.Dropdown.selectedItem model.serverSelectionDropdown of
        -- prefer NIP-96 server for uploads as they accept more metadata
        Just (UploadServerNip96 _ _) ->
            Model { model | uploadServers = uploadServers }

        _ ->
            Model
                { model
                | serverSelectionDropdown =
                    Components.Dropdown.selectItem model.serverSelectionDropdown (List.head uploadServers)
                , uploadServers = uploadServers
                }

type Msg
    = FocusedDropdown
    | BlurredDropdown
    | CloseDialog
    | IncomingMessage { messageType : String , value : Encode.Value }
    | DropdownSent (Components.Dropdown.Msg UploadServer Msg)
    | ChangedSelectedServer UploadServer
    | DragEnter
    | DragOver
    | DragLeave
    | TriggerFileSelect
    | FilesSelected File (List File)
    | ConvertedToUrl Int String
    | UpdateCaption Int String
    | UpdateAltText Int String
    | UpdateMediaType Int String
    | ToggleNoTransform Int
    | StartUpload Int -- FileId
    | HashComputed Int String -- FileId, SHA256 Hash
    | UploadResultBlossom Int String (Result Http.Error Blossom.BlobDescriptor) -- fileId, api URL
    | UploadResultNip96 Int String String (Result Http.Error Nip96.UploadResponse) -- fileId, api URL
    | UploadProgress String Http.Progress
    | ErrorOccurred String

type UploadResponse
    = UploadResponseBlossom String Blossom.BlobDescriptor -- server URL, blob descriptor response
    | UploadResponseNip96 String Nip94.FileMetadata -- server URL, NIP-96 response


update :
    { msg : Msg
    , model : Model 
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , onUploaded : UploadResponse -> msg
    , user : Auth.User
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
    toParentModel <|
        case props.msg of
            FocusedDropdown ->
                ( Model { model | state = DialogVisible }, Effect.none)

            BlurredDropdown ->
                ( Model { model | state = DialogClosed }, Effect.none)

            CloseDialog ->
                ( Model
                    { model
                        | state = DialogClosed
                        , files = Dict.empty
                        , serverSelectionDropdown = Dropdown.close model.serverSelectionDropdown
                    }
                , Effect.none
                )

            IncomingMessage { messageType, value } ->
                processIncomingMessage props.user props.model messageType props.toMsg value

            DropdownSent innerMsg ->
                let
                    (newModel, effect) =
                        Components.Dropdown.update
                            { msg = innerMsg
                            , model = model.serverSelectionDropdown
                            , toModel = \dropdown -> Model { model | serverSelectionDropdown = dropdown }
                            , toMsg = (DropdownSent)
                            }
                in
                (newModel, Effect.map props.toMsg effect)

            ChangedSelectedServer uploadServer ->
                ( Model { model 
                | serverSelectionDropdown =
                    Components.Dropdown.selectItem model.serverSelectionDropdown (Just uploadServer)
                }
                , Effect.none
                )

            DragEnter ->
                ( Model model, Effect.none)

            DragOver ->
                ( Model model, Effect.none)

            DragLeave ->
                ( Model model, Effect.none)

            TriggerFileSelect ->
                ( Model model
                -- multi file selection temporarily disabled
                -- TODO need to improve the metadata editor for multi file
                , FileSelect.files ["image/png", "image/jpg"] FilesSelected
                -- , FileSelect.file ["image/png", "image/jpg"] (\file -> FilesSelected file [])
                |> Cmd.map props.toMsg
                |> Effect.sendCmd
                )

            FilesSelected file files ->
                let
                    ( newFiles, cmds, nextId ) =
                        List.foldl
                            (\fileToUpload ( dict, cmdList, id ) ->
                                let
                                    maybeFileUpload =
                                        case Components.Dropdown.selectedItem model.serverSelectionDropdown of
                                            Just (UploadServerBlossom _) ->
                                                FileUploadBlossom Nothing
                                                    { file = fileToUpload
                                                    , status = Blossom.Selected
                                                    , caption = Nothing
                                                    , uploadResponse = Nothing
                                                    }
                                                    |> Just

                                            Just (UploadServerNip96 _ _) ->
                                                FileUploadNip96 Nothing
                                                    { file = fileToUpload
                                                    , status = Nip96.Selected
                                                    , caption = Nothing
                                                    , alt = Nothing
                                                    , mediaType = Nothing
                                                    , noTransform = Nothing
                                                    , uploadResponse = Nothing
                                                    }
                                                    |> Just

                                            Nothing ->
                                                Nothing
                                    convertToUrlCommand =
                                        if File.size fileToUpload < 1000000 then
                                            Task.perform (ConvertedToUrl id) (File.toUrl fileToUpload)
                                        else
                                            Cmd.none
                                in
                                case maybeFileUpload of
                                    Just fileUpload ->
                                        ( Dict.insert id fileUpload dict, convertToUrlCommand :: cmdList, id + 1 )

                                    Nothing ->
                                        ( dict, cmdList, id )
                            )
                            ( model.files, [], model.nextFileId )
                            (file :: files)
                in
                ( Model
                    { model | files = newFiles
                    , nextFileId = nextId
                    }
                , Cmd.batch cmds
                    |> Cmd.map props.toMsg
                    |> Effect.sendCmd
                )

            ConvertedToUrl fileId fileUrl ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom _ upload) ->
                                        Just <| FileUploadBlossom (Just fileUrl) upload 

                                    Just (FileUploadNip96 _ upload) ->
                                        Just <| FileUploadNip96 (Just fileUrl) upload 

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )

            UpdateCaption fileId captionText ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        if captionText /= "" then
                                            Just <| FileUploadBlossom maybePreviewLink { upload | caption = Just captionText }
                                        else
                                            Just <| FileUploadBlossom maybePreviewLink { upload | caption = Nothing }

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        if captionText /= "" then
                                            Just <| FileUploadNip96 maybePreviewLink { upload | caption = Just captionText }
                                        else
                                            Just <| FileUploadNip96 maybePreviewLink { upload | caption = Nothing }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )

            UpdateAltText fileId altText ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        -- Blossom doesn't know about alt text
                                        Just (FileUploadBlossom maybePreviewLink upload)

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        if altText /= "" then
                                            Just <| FileUploadNip96 maybePreviewLink { upload | alt = Just altText }
                                        else
                                            Just <| FileUploadNip96 maybePreviewLink { upload | alt = Nothing }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )

            UpdateMediaType fileId mediaType ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just (FileUploadBlossom maybePreviewLink upload)

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        if mediaType /= "" then
                                            Just <| FileUploadNip96 maybePreviewLink { upload | mediaType = Just mediaType }
                                        else
                                            Just <| FileUploadNip96 maybePreviewLink { upload | mediaType = Nothing }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )

            ToggleNoTransform fileId ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just (FileUploadBlossom maybePreviewLink upload)

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        case upload.noTransform of
                                            Just True ->
                                                Just <| FileUploadNip96 maybePreviewLink { upload | noTransform = Just False }

                                            Just False ->
                                                Just <| FileUploadNip96 maybePreviewLink { upload | noTransform = Just True }

                                            Nothing ->
                                                Just <| FileUploadNip96 maybePreviewLink { upload | noTransform = Nothing }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )

            StartUpload fileId ->
                let
                    maybeUpload = Dict.get fileId model.files

                    effect =
                        case maybeUpload of
                            Just (FileUploadBlossom _ upload) ->
                                -- Compute the hash of the file in Elm
                                computeFileHash props.toMsg fileId upload.file
                                |> Effect.sendCmd

                            Just (FileUploadNip96 _ upload) ->
                                -- Compute the hash of the file in Elm
                                computeFileHash props.toMsg fileId upload.file
                                |> Effect.sendCmd

                            Nothing ->
                                Effect.none

                    updatedFiles =
                        Dict.update fileId
                            (\maybeUploadInner ->
                                case maybeUploadInner of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just <| FileUploadBlossom maybePreviewLink { upload | status = Blossom.Hashing }

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        Just <| FileUploadNip96 maybePreviewLink { upload | status = Nip96.Hashing }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, effect )

            HashComputed fileId hash ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just <| FileUploadBlossom maybePreviewLink { upload | status = Blossom.AwaitingAuthHeader hash }

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        Just <| FileUploadNip96 maybePreviewLink { upload | status = Nip96.AwaitingAuthHeader hash }

                                    Nothing ->
                                        Nothing
                            )
                            model.files

                    effect =
                        case Components.Dropdown.selectedItem model.serverSelectionDropdown of
                            Just (UploadServerBlossom serverUrl) ->
                                let
                                    upload =
                                        case Dict.get fileId model.files of
                                            Just (FileUploadBlossom _ fileUpload) ->
                                                Just fileUpload
                                            
                                            Just (FileUploadNip96 _ _) ->
                                                Nothing

                                            Nothing ->
                                                Nothing

                                    -- content field of auth header
                                    content =
                                        upload
                                        |> Maybe.andThen .caption
                                        |> Maybe.withDefault "Image upload"
                                in
                                PutRequest fileId hash
                                |> RequestBlossomAuth serverUrl content
                                |> Nostr.createRequest props.nostr "Blossom auth request for files to be uploaded" []
                                |> Shared.Msg.RequestNostrEvents
                                |> Effect.sendSharedMsg

                            Just (UploadServerNip96 serverUrl nip96ServerDescriptorData) ->
                                PostRequest fileId hash
                                |> RequestNip98Auth serverUrl nip96ServerDescriptorData.apiUrl 
                                |> Nostr.createRequest props.nostr "NIP-96 auth request for files to be uploaded" []
                                |> Shared.Msg.RequestNostrEvents
                                |> Effect.sendSharedMsg

                            Nothing ->
                                Effect.none
                in
                ( Model { model | files = updatedFiles }, effect )

            UploadResultBlossom fileId apiUrl (Ok blobDescriptor) ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just <| FileUploadBlossom maybePreviewLink { upload | status = Blossom.Uploaded, uploadResponse = Just blobDescriptor }

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        Just (FileUploadNip96 maybePreviewLink upload)

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }
                , Effect.sendMsg <| props.onUploaded (UploadResponseBlossom apiUrl blobDescriptor)
                )

            UploadResultBlossom fileId apiUrl (Err error) ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just <| FileUploadBlossom maybePreviewLink { upload | status = Blossom.Failed <| httpErrorToString error }

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        Just (FileUploadNip96 maybePreviewLink upload)

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles, errors = model.errors ++ [ httpErrorToString error] }, Effect.none )

            UploadResultNip96 fileId serverUrl _ (Ok response) ->
                let
                    (statusNip96, effect) =
                        case (response.status, response.fileMetadata) of
                            ("success", Just fileMetadata) ->
                                ( Nip96.Uploaded
                                , Effect.sendMsg <| props.onUploaded (UploadResponseNip96 serverUrl fileMetadata)
                                )

                            (responseStatus, _) ->
                                (Nip96.Failed <| Maybe.withDefault responseStatus response.message
                                , Effect.none)

                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just (FileUploadBlossom maybePreviewLink upload)

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        Just <| FileUploadNip96 maybePreviewLink { upload | status = statusNip96, uploadResponse = Just response }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, effect )

            UploadResultNip96 fileId _ apiUrl (Err error) ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just (FileUploadBlossom maybePreviewLink upload) ->
                                        Just (FileUploadBlossom maybePreviewLink upload)

                                    Just (FileUploadNip96 maybePreviewLink upload) ->
                                        Just <| FileUploadNip96 maybePreviewLink { upload | status = Nip96.Failed <| Translations.errorUploadingToNip96ServerText [ props.browserEnv.translations ] ++ apiUrl ++ " : " ++ httpErrorToString error }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles, errors = model.errors ++ [ httpErrorToString error] }, Effect.none )

            UploadProgress tracker progress ->
                let
                    fileId =
                        String.toInt tracker |> Maybe.withDefault -1

                    updatedFiles =
                        if fileId /= -1 then
                            Dict.update fileId
                                (\maybeUpload ->
                                    case maybeUpload of
                                        Just (FileUploadBlossom maybePreviewLink upload) ->
                                            case upload.status of
                                                Blossom.Uploading _ ->
                                                    let
                                                        progressValue =
                                                            case progress of
                                                                Http.Sending { sent, size } ->
                                                                    Http.fractionSent { sent = sent, size = size } * 100

                                                                Http.Receiving _ ->
                                                                    100
                                                    in
                                                    Just <| FileUploadBlossom maybePreviewLink { upload | status = Blossom.Uploading progressValue }

                                                _ ->
                                                    Just <| FileUploadBlossom maybePreviewLink upload

                                        Just (FileUploadNip96 maybePreviewLink upload) ->
                                            case upload.status of
                                                Nip96.Uploading _ ->
                                                    let
                                                        progressValue =
                                                            case progress of
                                                                Http.Sending { sent, size } ->
                                                                    Http.fractionSent { sent = sent, size = size } * 100

                                                                Http.Receiving _ ->
                                                                    100
                                                    in
                                                    Just <| FileUploadNip96 maybePreviewLink { upload | status = Nip96.Uploading progressValue }

                                                _ ->
                                                    Just <| FileUploadNip96 maybePreviewLink upload

                                        Nothing ->
                                            Nothing
                                )
                                model.files

                        else
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )


            ErrorOccurred errorMsg ->
                ( Model { model | errors = model.errors ++ [ errorMsg ] }, Effect.none )


processIncomingMessage : Auth.User -> Model -> String -> (Msg -> msg) -> Encode.Value -> (Model, Effect msg)
processIncomingMessage user (Model model) messageType toMsg value =
    case messageType of
        "blossomAuthHeader" ->
            case (Decode.decodeValue decodeAuthHeaderReceived value) of
                (Ok decoded) ->
                    case ( decoded.method, decoded.fileId ) of
                        ("PUT", Just fileId) ->
                            let
                                updatedFiles =
                                    Dict.update fileId
                                        (\maybeUpload ->
                                            case maybeUpload of
                                                Just (FileUploadBlossom maybePreviewLink upload) ->
                                                    case upload.status of
                                                        Blossom.AwaitingAuthHeader hash ->
                                                            Just <| FileUploadBlossom maybePreviewLink { upload | status = Blossom.ReadyToUpload hash decoded.authHeader }

                                                        _ ->
                                                            Just <| FileUploadBlossom maybePreviewLink upload

                                                Just (FileUploadNip96 maybePreviewLink upload) ->
                                                    Just (FileUploadNip96 maybePreviewLink upload)

                                                Nothing ->
                                                    Nothing
                                        )
                                        model.files

                                -- Start the upload
                                effect =
                                    case Dict.get fileId updatedFiles of
                                        Just (FileUploadBlossom maybePreviewLink upload) ->
                                            case upload.status of
                                                Blossom.ReadyToUpload hash authHeader ->
                                                    let
                                                        apiUrl =
                                                            case Components.Dropdown.selectedItem model.serverSelectionDropdown of
                                                                Just (UploadServerBlossom serverUrl) ->
                                                                    serverUrl

                                                                _ ->
                                                                    ""
                                                    in
                                                    Blossom.uploadFile (UploadResultBlossom fileId apiUrl) authHeader apiUrl upload.file
                                                    |> Cmd.map toMsg
                                                    |> Effect.sendCmd

                                                _ ->
                                                    Effect.none

                                        _ ->
                                            Effect.none

                            in
                            ( Model { model | files = updatedFiles }, effect)

                        (_, _) ->
                            ( Model model, Effect.none)

                (Err error) ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding Blossom auth header: " ++ Decode.errorToString error]}, Effect.none)

        "nip98AuthHeader" ->
            case (Decode.decodeValue decodeAuthHeaderReceived value) of
                (Ok decoded) ->
                    case ( decoded.method, decoded.fileId ) of
                        ("POST", Just fileId) ->
                            let
                                updatedFiles =
                                    Dict.update fileId
                                        (\maybeUpload ->
                                            case maybeUpload of
                                                Just (FileUploadBlossom maybePreviewLink upload) ->
                                                    Just (FileUploadBlossom maybePreviewLink upload)

                                                Just (FileUploadNip96 maybePreviewLink upload) ->
                                                    case upload.status of
                                                        Nip96.AwaitingAuthHeader hash ->
                                                            Just <| FileUploadNip96 maybePreviewLink { upload | status = Nip96.ReadyToUpload hash decoded.authHeader }

                                                        _ ->
                                                            Just <| FileUploadNip96 maybePreviewLink upload

                                                Nothing ->
                                                    Nothing
                                        )
                                        model.files

                                -- Start the upload
                                effect =
                                    case Dict.get fileId updatedFiles of
                                        Just (FileUploadNip96 maybePreviewLink upload) ->
                                            case upload.status of
                                                Nip96.ReadyToUpload hash authHeader ->
                                                    let
                                                        (apiUrl, serverUrl) =
                                                            case Components.Dropdown.selectedItem model.serverSelectionDropdown of
                                                                Just (UploadServerNip96 nip96ServerUrl serverDescriptorData) ->
                                                                    (serverDescriptorData.apiUrl, nip96ServerUrl)

                                                                _ ->
                                                                    ("", "")
                                                    in
                                                    Nip96.uploadFile apiUrl fileId upload (UploadResultNip96 fileId decoded.serverUrl apiUrl) authHeader
                                                    |> Cmd.map toMsg
                                                    |> Effect.sendCmd

                                                _ ->
                                                    Effect.none

                                        _ ->
                                            Effect.none

                            in
                            ( Model { model | files = updatedFiles }, effect)

                        (_, _) ->
                            ( Model model, Effect.none)

                (Err error) ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding NIP-98 auth header: " ++ Decode.errorToString error]}, Effect.none)

        _ ->
            ( Model model, Effect.none)


decodeAuthHeaderReceived : Decode.Decoder AuthHeaderReceived
decodeAuthHeaderReceived =
    Decode.map6 AuthHeaderReceived
        (Decode.field "requestId" Decode.int)
        (Decode.maybe (Decode.field "fileId" Decode.int))
        (Decode.field "method" Decode.string)
        (Decode.field "authHeader" Decode.string)
        (Decode.field "serverUrl" Decode.string)
        (Decode.field "apiUrl" Decode.string)

type alias AuthHeaderReceived =
    { requestId : Int
    , fileId : Maybe Int
    , method : String
    , authHeader : String
    , serverUrl : String
    , apiUrl : String
    }

type DisplayMode
    = WaitingForFiles
    | EditingMetadata Int FileUpload
    | Uploading
    | Finished

displayMode : Model -> DisplayMode
displayMode (Model model) =
    let
        uploads =
            Dict.values model.files

        firstUploadWithoutMetadata =
            model.files
            |> Dict.toList 
            |> uploadsNeedingMetadata
            |> List.head
    in
    if List.length uploads < 1 then
        WaitingForFiles
    else case firstUploadWithoutMetadata of
        Just (fileId, upload) ->
            EditingMetadata fileId upload

        Nothing ->
            if uploadInProgress uploads then
                Uploading
            else
                Finished

uploadsNeedingMetadata : List (Int, FileUpload) -> List (Int, FileUpload)
uploadsNeedingMetadata uploads =
    uploads
    |> List.filter (\(fileId, upload) ->
        case upload of
            FileUploadBlossom _ { status } ->
                status == Blossom.Selected

            FileUploadNip96 _ { status } ->
                status == Nip96.Selected
    )

uploadInProgress : List FileUpload -> Bool
uploadInProgress uploads =
    uploads
    |> List.any (\upload ->
        case upload of
            FileUploadBlossom _ { status } ->
                case status of
                    Blossom.Hashing ->
                        True

                    Blossom.AwaitingAuthHeader _ ->
                        True

                    Blossom.ReadyToUpload _ _ ->
                        True

                    Blossom.Uploading _ ->
                        True

                    _ ->
                        False

            FileUploadNip96 _ { status } ->
                case status of
                    Nip96.Hashing ->
                        True

                    Nip96.AwaitingAuthHeader _ ->
                        True

                    Nip96.ReadyToUpload _ _ ->
                        True

                    Nip96.Uploading _ ->
                        True

                    _ ->
                        False
    )

view : UploadDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    case (model.state, displayMode (Model model)) of
        (DialogClosed, _) ->
            div [][]

        (DialogVisible, WaitingForFiles) ->
            viewWaitingForFiles (Settings settings)
            |> Html.map settings.toMsg

        (DialogVisible, EditingMetadata fileId fileUpload) ->
            viewMetadataDialog (Settings settings) (fileId, fileUpload)

        (DialogVisible, Uploading) ->
            viewUploadingDialog (Settings settings)

        (DialogVisible, Finished) ->
            viewFinishedDialog (Settings settings)


viewWaitingForFiles : UploadDialog msg -> Html Msg
viewWaitingForFiles (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    modalDialog
        (Translations.dialogTitle [ settings.browserEnv.translations ])
        [ Components.Dropdown.new
            { model = model.serverSelectionDropdown
            , toMsg = DropdownSent
            , choices = model.uploadServers
            , toLabel = uploadServerToString
            }
            |> Components.Dropdown.withOnChange ChangedSelectedServer
            |> Components.Dropdown.view
        , div
            [ css 
                [ Tw.p_20
                , Tw.m_2
                , Tw.rounded_2xl
                , Tw.border_color Theme.slate_500
                , Tw.border_dashed
                , Tw.border_4
                ]
            , hijackOn "dragenter" (Decode.succeed DragEnter)
            , hijackOn "dragover" (Decode.succeed DragOver)
            , hijackOn "dragleave" (Decode.succeed DragLeave)
            , hijackOn "drop" dropDecoder
            ]
            [ div [ class "mb-4" ]
                [ button
                    [ onClick TriggerFileSelect
                    , class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                    ]
                    [ text <| Translations.selectFilesButtonTitle [ settings.browserEnv.translations ] ]
                ]
            ]
        ]
        CloseDialog

uploadServerToString : UploadServer -> String
uploadServerToString uploadServer =
    case uploadServer of
        UploadServerBlossom serverUrl ->
            hostOfUrl serverUrl ++ " (Blossom)"

        UploadServerNip96 serverUrl _ ->
            hostOfUrl serverUrl ++ " (NIP-96)"

hostOfUrl : String -> String
hostOfUrl url =
    case Url.fromString url of
        Just { host } ->
            host
        
        Nothing ->
            url


dropDecoder : Decode.Decoder Msg
dropDecoder =
    -- multi-file dropping temporarily disabled
    -- TODO: improve metadata editor for multiple files
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore FilesSelected File.decoder)
    -- Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore (\file1 _ -> FilesSelected file1 []) File.decoder)

hijackOn : String -> Decode.Decoder msg -> Html.Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)

hijack : msg -> (msg, Bool)
hijack msg =
    (msg, True)


viewMetadataDialog : UploadDialog msg -> (Int, FileUpload) -> Html msg
viewMetadataDialog (Settings settings) (fileId, fileUpload) =
    let
        (Model model) =
            settings.model
    in
    modalDialog
        (Translations.editMetadataDialogTitle [ settings.browserEnv.translations ])
        [ div
            [ css 
                [
                ]
            ]
            [ viewFileUpload settings.theme settings.browserEnv (fileId, fileUpload)
            ]
        |> Html.map settings.toMsg
        ]
        (settings.toMsg CloseDialog)


viewUploadingDialog : UploadDialog msg -> Html msg
viewUploadingDialog (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    modalDialog
        (Translations.uploadingStateText [ settings.browserEnv.translations ])
        [ div
            [ css 
                [
                ]
            ]
            [ div []
                (Dict.toList model.files |> List.map (viewFileUpload settings.theme settings.browserEnv))
            ]
        |> Html.map settings.toMsg
        ]
        (settings.toMsg CloseDialog)


viewFinishedDialog : UploadDialog msg -> Html msg
viewFinishedDialog (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    modalDialog
        (Translations.dialogTitle [ settings.browserEnv.translations ])
        [ div [ class "p-4" ]
            [ div []
                (Dict.toList model.files |> List.map (viewFileUpload settings.theme settings.browserEnv))
            , case model.errors of
                [] ->
                    text ""

                errors ->
                    errors
                    |> List.map (\errorMsg ->
                        li [ class "text-red-500" ] [ text errorMsg ]
                    )
                    |> ul []
            ]
        |> Html.map settings.toMsg
        ]
        (settings.toMsg CloseDialog)

viewFileUpload : Ui.Styles.Theme -> BrowserEnv -> ( Int, FileUpload ) -> Html Msg
viewFileUpload theme browserEnv ( fileId, fileUpload ) =
    case fileUpload of
        FileUploadBlossom maybePreviewLink fileUploadBlossom ->
            viewFileUploadBlossom theme browserEnv maybePreviewLink (fileId, fileUploadBlossom)

        FileUploadNip96 maybePreviewLink fileUploadNip96 ->
            viewFileUploadNip96 theme browserEnv maybePreviewLink (fileId, fileUploadNip96)


viewFileUploadBlossom : Ui.Styles.Theme -> BrowserEnv -> Maybe String -> ( Int, Blossom.FileUpload ) -> Html Msg
viewFileUploadBlossom theme browserEnv maybePreviewLink ( fileId, fileUpload ) =
    let
        fileName =
            File.name <| fileUpload.file

        statusView =
            case fileUpload.status of
                Blossom.Selected ->
                    div
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.gap_3
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.flex
                                , Tw.flex_row
                                , Tw.gap_3
                                ]
                            ]
                            [ div
                                [ css
                                    [ Tw.flex
                                    , Tw.flex_col
                                    , Tw.gap_3
                                    ]
                                ]
                                [ div 
                                    [ css
                                        [ Tw.flex
                                        , Tw.flex_row
                                        ]
                                    ]
        
                                    [ label [] [ text <| Translations.imageCaptionFormLabel [ browserEnv.translations ] ]
                                    , textarea
                                        [ onInput (UpdateCaption fileId)
                                        , class "w-full border rounded p-2 mb-2"
                                        ]
                                        [ text <| Maybe.withDefault "" fileUpload.caption ]
                                    ]
                                ]
                            , case maybePreviewLink of
                                Just previewLink ->
                                    img [ Attr.src previewLink
                                        , css
                                            [ Tw.w_40
                                            , Tw.h_40
                                            ]
                                        ]
                                        []

                                Nothing ->
                                    div [][]
                            ]
                        , Button.new
                            { label = Translations.startUploadButtonText [ browserEnv.translations ]
                            , onClick = Just <| StartUpload fileId
                            , theme = theme
                            }
                            |> Button.view
                        ]

                Blossom.Hashing ->
                    text <| Translations.computingFileHashStateText [ browserEnv.translations ]

                Blossom.AwaitingAuthHeader _ ->
                    text <| Translations.awaitingAuthenticationHeaderStateText [ browserEnv.translations ]

                Blossom.ReadyToUpload _ _ ->
                    text <| Translations.readyToUploadStateText [ browserEnv.translations ]

                Blossom.Uploading progressValue ->
                    div []
                        [ progress
                            [ Attr.max "100"
                            , value (String.fromFloat progressValue)
                            , class "w-full"
                            ] []
                        , div [ class "text-sm text-gray-700" ] [ text (String.fromInt (round progressValue) ++ "%") ]
                        ]

                Blossom.Uploaded ->
                    case fileUpload.uploadResponse of
                        Just response ->
                            div []
                                [ div [ class "text-green-500" ] [ text <| Translations.uploadCompletedStateText [ browserEnv.translations ] ]
                                , viewUploadResponseBlossom browserEnv response
                                ]
                    

                        Nothing ->
                            div [ class "text-green-500" ] [ text  <| Translations.uploadCompletedStateText [ browserEnv.translations ] ]

                Blossom.Failed errorMsg ->
                    div [ class "text-red-500" ] [ text (Translations.uploadFailedStateText [ browserEnv.translations ] ++ " " ++ errorMsg) ]
    in
    div [ css
            [ Tw.mb_4
            , Tw.p_4
            , Tw.rounded
            ]
        ]
        [ div
            [ css
                [ Tw.font_bold
                , Tw.mb_2
                ] 
            ]
            [ text fileName ]
        , statusView
        ]



viewFileUploadNip96 : Ui.Styles.Theme -> BrowserEnv -> Maybe String -> ( Int, Nip96.FileUpload ) -> Html Msg
viewFileUploadNip96 theme browserEnv maybePreviewLink ( fileId, fileUpload ) =
    let
        fileName =
            File.name <| fileUpload.file

        statusView =
            case fileUpload.status of
                Nip96.Selected ->
                    div
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.gap_3
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.flex
                                , Tw.flex_row
                                , Tw.gap_3
                                ]
                            ]
                            [ div
                                [ css
                                    [ Tw.flex
                                    , Tw.flex_col
                                    , Tw.gap_3
                                    ]
                                ]
                                [ div 
                                    [ css
                                        [ Tw.flex
                                        , Tw.flex_row
                                        ]
                                    ]
                                    [ label [] [ text <| Translations.imageCaptionFormLabel [ browserEnv.translations ] ]
                                    , textarea
                                        [ onInput (UpdateCaption fileId)
                                        , class "w-full border rounded p-2 mb-2"
                                        ]
                                        [ text <| Maybe.withDefault "" fileUpload.caption ]
                                    ]
                                , div 
                                    [ css
                                        [ Tw.flex
                                        , Tw.flex_row
                                        ]
                                    ]
                                    [ label [] [ text <| Translations.imageAltTextFormLabel [ browserEnv.translations ] ]
                                    , textarea
                                        [ onInput (UpdateAltText fileId)
                                        , class "w-full border rounded p-2 mb-2"
                                        ]
                                        [ text <| Maybe.withDefault "" fileUpload.alt ]
                                    ]
                                , div 
                                    [ css
                                        [ Tw.flex
                                        , Tw.flex_row
                                        ]
                                    ]
                                    [ label [] [ text <| Translations.mediaTypeFormLabel [ browserEnv.translations ] ]
                                    , select
                                        [ onInput (UpdateMediaType fileId)
                                        , class "w-full border rounded p-2 mb-2"
                                        ]
                                        [ option [ value "" ] [ text <| Translations.mediaTypeExplanationText [ browserEnv.translations ] ]
                                        , option [ value "avatar" ] [ text <| Translations.avatarMediaType [ browserEnv.translations ] ]
                                        , option [ value "banner" ] [ text <| Translations.bannerMediaType [ browserEnv.translations ] ]
                                        ]
                                    ]
                                , div 
                                    [ css
                                        [ Tw.flex
                                        , Tw.flex_row
                                        ]
                                    ]
                                    [ label
                                        [ css
                                            [ Tw.inline_flex
                                            , Tw.items_center
                                            , Tw.mt_2
                                            , Tw.gap_1
                                            ]
                                        ]
                                        [ input
                                            [ type_ "checkbox"
                                            , checked <| Maybe.withDefault False fileUpload.noTransform
                                            , onClick (ToggleNoTransform fileId)
                                            ]
                                            []
                                        , span [ class "ml-2" ] [ text <| Translations.noTransformCheckboxText [ browserEnv.translations ] ]
                                        ]
                                    ]
                                ]
                            , case maybePreviewLink of
                                Just previewLink ->
                                    img [ Attr.src previewLink
                                        , css
                                            [ Tw.w_40
                                            , Tw.h_40
                                            ]
                                        ]
                                        []

                                Nothing ->
                                    div [][]
                            ]
                        , Button.new
                            { label = Translations.startUploadButtonText [ browserEnv.translations ]
                            , onClick = Just <| StartUpload fileId
                            , theme = theme
                            }
                            |> Button.view
                        ]

                Nip96.Hashing ->
                    text <| Translations.computingFileHashStateText [ browserEnv.translations ]

                Nip96.AwaitingAuthHeader _ ->
                    text <| Translations.awaitingAuthenticationHeaderStateText [ browserEnv.translations ]

                Nip96.ReadyToUpload _ _ ->
                    text <| Translations.readyToUploadStateText [ browserEnv.translations ]

                Nip96.Uploading progressValue ->
                    div []
                        [ progress
                            [ Attr.max "100"
                            , value (String.fromFloat progressValue)
                            , class "w-full"
                            ] []
                        , div [ class "text-sm text-gray-700" ] [ text (String.fromInt (round progressValue) ++ "%") ]
                        ]

                Nip96.Uploaded ->
                    case fileUpload.uploadResponse of
                        Just response ->
                            div []
                                [ div [ class "text-green-500" ] [ text <| Translations.uploadCompletedStateText [ browserEnv.translations ] ]
                                , viewUploadResponseNip96 browserEnv response
                                ]

                        Nothing ->
                            div [ class "text-green-500" ] [ text <| Translations.uploadCompletedStateText [ browserEnv.translations ] ]

                Nip96.Failed errorMsg ->
                    div [ class "text-red-500" ] [ text (Translations.uploadFailedStateText [ browserEnv.translations ] ++ " " ++ errorMsg) ]
    in
    div [ css
            [ Tw.mb_4
            , Tw.p_4
            , Tw.rounded
            ]
        ]
        [ div
            [ css
                [ Tw.font_bold
                , Tw.mb_2
                ] 
            ]
            [ text fileName ]
        , statusView
        ]

viewUploadResponseBlossom : BrowserEnv -> Blossom.BlobDescriptor -> Html msg
viewUploadResponseBlossom browserEnv response =
    div [ class "mt-2" ]
        (case response.nip94 of
            Just fileMetadata ->
                let
                    url =
                        fileMetadata.url
                        |> Maybe.withDefault (Translations.noUrlProvidedText [ browserEnv.translations ])

                    ox =
                        fileMetadata.oxHash
                        |> Maybe.withDefault (Translations.noHashProvidedText [ browserEnv.translations ])
                in
                [ div
                    [ css
                        [ Tw.text_ellipsis
                        , Tw.overflow_hidden
                        ]
                    ]
                    [ text (Translations.downloadUrlFieldText [ browserEnv.translations ] ++ " " ++ url) ]
                , div
                    [ css
                        [ Tw.text_ellipsis
                        , Tw.overflow_hidden
                        ]
                    ]
                    [ text (Translations.originalFileHashText [ browserEnv.translations ] ++ " " ++ ox) ]
                ]

            Nothing ->
                [ div [] [ text <| Translations.noNip94ProvidedText [ browserEnv.translations ] ] ]
        )

viewUploadResponseNip96 : BrowserEnv -> Nip96.UploadResponse -> Html msg
viewUploadResponseNip96 browserEnv response =
    div [ class "mt-2" ]
        (case response.fileMetadata of
            Just fileMetadata ->
                let
                    url =
                        fileMetadata.url
                        |> Maybe.withDefault (Translations.noUrlProvidedText [ browserEnv.translations ])

                    ox =
                        fileMetadata.oxHash
                        |> Maybe.withDefault (Translations.noHashProvidedText [ browserEnv.translations ])
                in
                [ div
                    [ css
                        [ Tw.text_ellipsis
                        , Tw.overflow_hidden
                        ]
                    ]
                    [ text (Translations.downloadUrlFieldText [ browserEnv.translations ] ++ " " ++ url) ]
                , div
                    [ css
                        [ Tw.text_ellipsis
                        , Tw.overflow_hidden
                        ]
                    ]
                    [ text (Translations.originalFileHashText [ browserEnv.translations ] ++ " " ++ ox) ]
                ]

            Nothing ->
                [ div [] [ text <| Translations.noNip94ProvidedText [ browserEnv.translations ] ] ]
        )

tagValue : List (List String) -> String -> Maybe String
tagValue tags tagName =
    tags
    |> List.filterMap (\tag ->
        if List.head tag == Just tagName then
            tag
            |> List.drop 1
            |> List.head
        else
            Nothing
        ) 
    |> List.head

uploadButton : Ui.Styles.Theme -> Html Msg
uploadButton theme =
    Button.new
        { label = "Upload"
        , onClick = Just TriggerFileSelect
        , theme = theme
        }
        |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.upload)
        |> Button.view


subscribe : Model -> Sub Msg 
subscribe (Model model) =
    Sub.batch
        [ Ports.receiveMessage IncomingMessage
        , model.files
            |> Dict.keys
            |> List.map String.fromInt
            |> List.map (\tracker -> Http.track tracker (UploadProgress tracker))
            |> Sub.batch
        ]


-- COMMANDS


computeFileHash : (Msg -> msg) -> Int -> File -> Cmd msg
computeFileHash toMsg fileId file =
    Task.attempt (hashResultHandler toMsg fileId) (computeHashTask file)


computeHashTask : File -> Task Decode.Error String
computeHashTask file =
    File.toBytes file
        |> Task.map SHA256.fromBytes
        |> Task.map SHA256.toHex


hashResultHandler : (Msg -> msg) -> Int -> Result Decode.Error String -> msg
hashResultHandler toMsg fileId result =
    case result of
        Ok hash ->
            toMsg <| HashComputed fileId hash

        Err _ ->
            toMsg <| ErrorOccurred "Failed to compute file hash."


