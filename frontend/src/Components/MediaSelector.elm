module Components.MediaSelector exposing
    ( MediaSelector, new
    , Model, init, DisplayType(..), UploadedFile(..)
    , Msg, update, show
    , view
    , subscribe
    )

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown
import Components.Icon as Icon
import Components.UploadDialog as UploadDialog exposing (UploadResponse(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, strong, text)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, href, type_)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Nostr
import Nostr.Blossom as Blossom exposing (BlobDescriptor, userServerListFromEvent)
import Nostr.Event exposing (Event, Kind(..))
import Nostr.FileStorageServerList exposing (fileStorageServerListFromEvent)
import Nostr.Nip94 as Nip94
import Nostr.Nip96 as Nip96 exposing (extendRelativeServerDescriptorUrls)
import Nostr.Request exposing (HttpRequestMethod(..), RequestData(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Ports
import Shared.Msg
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.MediaSelector as Translations
import Ui.Styles exposing (Styles, Theme)
import Ui.Shared
import Url
import I18Next
import Components.UploadDialog exposing (UploadServer)


type MediaSelector msg
     = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , onSelected : Maybe (UploadedFile -> msg)
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }

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
        , errors : List String
        }

type MediaServer
    = BlossomMediaServer ServerUrl
    | Nip96MediaServer ServerUrl
    | NoMediaServer
    | AllMediaServers

type alias ServerUrl = String


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
    | ServerHttpError Http.Error

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
        , uploadDialog = UploadDialog.init
            { toMsg = UploadDialogSent
            }
        , errors = []
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
    |> List.foldl (\serverUrl acc ->
        if Dict.member serverUrl acc then
            acc
        else
            Dict.insert serverUrl ServerStateUnknown acc
        ) dict


requestBlossomListAuths : List String -> Effect (Msg msg)
requestBlossomListAuths blossomServers =
    blossomServers
    |> List.map (\blossomServer ->
        Effect.sendCmd <| Ports.requestBlossomAuth 1 blossomServer "List Blobs" GetRequest
    )
    |> Effect.batch

requestNip96ServerSpecs :  List String -> Effect (Msg msg)
requestNip96ServerSpecs nip96Servers =
    nip96Servers
    |> List.map (\nip96Server ->
        Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc nip96Server) nip96Server)
    )
    |> Effect.batch


show : Model -> Model
show (Model model) =
    Model { model | displayType = DisplayModalDialog True }

type Msg msg
    = FocusedDropdown
    | BlurredDropdown
    | CloseDialog
    | DropdownSent (Components.Dropdown.Msg MediaServer (Msg msg))
    | ChangedSelectedServer MediaServer
    | Upload
    | Uploaded UploadDialog.UploadResponse
    | UploadDialogSent UploadDialog.Msg 
    | IncomingMessage { messageType : String , value : Encode.Value }
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
    , user : Auth.User
    , nostr : Nostr.Model
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
                ( Model { model | displayType = DisplayModalDialog True } , Effect.none)

            BlurredDropdown ->
                ( Model { model | displayType = DisplayModalDialog False } , Effect.none)

            CloseDialog ->
                ( Model { model | displayType = DisplayModalDialog False } , Effect.none)

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

            ChangedSelectedServer selectedServer ->
                let
                    uploadServer =
                        case selectedServer of
                            BlossomMediaServer serverUrl ->
                                Just <| UploadDialog.UploadServerBlossom serverUrl

                            Nip96MediaServer serverUrl ->
                                case Dict.get serverUrl model.nip96ServerDescResponses of
                                    Just (Nip96.ServerDescriptor serverDescriptorData) ->
                                        Just <| UploadDialog.UploadServerNip96 serverUrl serverDescriptorData

                                    Just (Nip96.ServerRedirect _) ->
                                        Nothing

                                    Nothing ->
                                        Nothing

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

            Upload ->
                ( Model { model | uploadDialog = UploadDialog.show model.uploadDialog }, Effect.none)

            Uploaded uploadResponse ->
                modelWithUploadedFile props.model uploadResponse

            UploadDialogSent innerMsg ->
                UploadDialog.update
                    { msg = innerMsg
                    , model = model.uploadDialog
                    , toModel = \uploadDialog -> Model { model | uploadDialog = uploadDialog }
                    , toMsg = (props.toMsg << UploadDialogSent)
                    , onUploaded = (props.toMsg << Uploaded)
                    , user = props.user
                    , nostr = props.nostr
                    }

            IncomingMessage { messageType, value } ->
                processIncomingMessage props.user props.model messageType props.toMsg value

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

                    Nothing ->
                        ( Model { model | selected = Just 1 }
                        , Effect.none
                        )

            ReceivedBlossomFileList serverUrl result ->
                case result of
                    Ok fileList ->
                        ( updateModelWithBlossomFileList (Model model) serverUrl fileList, Effect.none )

                    Err error ->
                        (Model
                            { model | errors = model.errors ++ [ serverUrl ++ " (Blossom): " ++ httpErrorToString error ]
                            , blossomServers = updateServerState model.blossomServers serverUrl (ServerFileListFailed error)
                            }
                        , Effect.none
                        )


            ReceivedNip96ServerDesc serverUrl result ->
                case result of
                    Ok (Nip96.ServerRedirect serverRedirection) ->
                        ( Model
                            { model | nip96ServerDescResponses = Dict.insert serverUrl (Nip96.ServerRedirect serverRedirection) model.nip96ServerDescResponses
                            , nip96Servers = updateServerState model.nip96Servers serverUrl (ServerRedirected serverRedirection.delegated_to_url)
                            }
                        , Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc serverUrl) serverRedirection.delegated_to_url)
                        |> Effect.map props.toMsg
                        )

                    Ok (Nip96.ServerDescriptor serverDescriptorData) ->
                        let
                            serverDescWithExtendedUrls =
                                extendRelativeServerDescriptorUrls serverUrl serverDescriptorData
                        in
                        ( Model
                            { model | nip96ServerDescResponses = Dict.insert serverUrl (Nip96.ServerDescriptor serverDescWithExtendedUrls) model.nip96ServerDescResponses
                            , nip96Servers = updateServerState model.nip96Servers serverUrl ServerFunctioning
                            }
                        , GetRequest
                            |> RequestNip98Auth serverUrl serverDescWithExtendedUrls.apiUrl
                            |> Nostr.createRequest props.nostr "NIP-96 auth request for file list of server" []
                            |> Shared.Msg.RequestNostrEvents
                            |> Effect.sendSharedMsg
                        )

                    Err error ->
                        (Model
                            { model | errors = model.errors ++ [ serverUrl ++ " (NIP-96): " ++ httpErrorToString error ]
                            , nip96Servers = updateServerState model.nip96Servers serverUrl (ServerDescFailed error)
                            }
                        , Effect.none
                        )

            ReceivedNip96FileList serverUrl result ->
                case result of
                    Ok fileList ->
                        ( updateModelWithNip96FileList (Model model) serverUrl fileList, Effect.none )

                    Err error ->
                        (Model
                            { model | errors = model.errors ++ [ serverUrl ++ " (NIP-96): " ++ httpErrorToString error ]
                            , nip96Servers = updateServerState model.nip96Servers serverUrl (ServerFileListFailed error)
                            }
                        , Effect.none
                        )

modelWithUploadedFile : Model -> UploadResponse -> (Model, Effect msg)
modelWithUploadedFile model uploadResponse =
    case uploadResponse of
        UploadResponseBlossom apiUrl fileMetadata ->
            modelWithUploadedBlossomFile model apiUrl fileMetadata

        UploadResponseNip96 apiUrl fileMetadata ->
            modelWithUploadedNip96File model apiUrl fileMetadata

modelWithUploadedBlossomFile : Model -> String -> Blossom.BlobDescriptor -> (Model, Effect msg)
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
                            Just [uploadedFile]
                ) model.uploadedBlossomFiles
    in
    ( Model { model | uploadedBlossomFiles = uploadedBlossomFiles }, Effect.none)

modelWithUploadedNip96File : Model -> String -> Nip94.FileMetadata -> (Model, Effect msg)
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
                            Just [uploadedFile]
                ) model.uploadedNip96Files
    in
    ( Model { model | uploadedNip96Files = uploadedNip96Files }, Effect.none)

updateModelWithBlossomFileList : Model -> String -> List Blossom.BlobDescriptor -> Model
updateModelWithBlossomFileList (Model model) serverUrl fileList =
    let
        blossomServers =
            updateServerState model.blossomServers serverUrl ServerFileListReceived
    in
    Model
        { model | uploadedBlossomFiles = Dict.insert serverUrl (List.map BlossomFile fileList) model.uploadedNip96Files
        , blossomServers = blossomServers
        , uploadDialog = updateUploadDialogServerList model.nip96ServerDescResponses model.uploadDialog blossomServers model.nip96Servers
        , serverSelectionDropdown =
            selectableMediaServers (Model model)
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
            |> List.filterMap (\(url, status) ->
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
            |> List.filterMap (\(url, status) ->
                -- only consider servers that delivered a file list eligible for uploading files
                if status == ServerFileListReceived then
                    Just url
                else
                    Nothing
            )
            |> List.filterMap (\serverUrl ->
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
    in
    Model
        { model | uploadedNip96Files = Dict.insert serverUrl (List.map Nip96File fileList.files) model.uploadedNip96Files
        , nip96Servers = nip96Servers
        , uploadDialog = updateUploadDialogServerList model.nip96ServerDescResponses model.uploadDialog model.blossomServers nip96Servers
        , serverSelectionDropdown =
            selectableMediaServers (Model model)
            |> List.head
            |> Maybe.withDefault NoMediaServer
            |> Just
            |> Components.Dropdown.selectItem model.serverSelectionDropdown
        }

updateServerState : Dict String ServerState -> String -> ServerState -> Dict String ServerState
updateServerState dict serverUrl state =
    Dict.insert serverUrl state dict


processIncomingMessage : Auth.User -> Model -> String -> (Msg msg -> msg) -> Encode.Value -> (Model, Effect msg)
processIncomingMessage user xModel messageType toMsg value =
    let
        (Model model) =
            xModel
    in

    case messageType of
        "blossomAuthHeader" ->
            case (Decode.decodeValue decodeAuthHeaderReceived value) of
                Ok decoded ->
                    ( Model { model | authHeader = Just decoded.authHeader }
                    , Blossom.fetchFileList (ReceivedBlossomFileList decoded.serverUrl) decoded.authHeader decoded.serverUrl user.pubKey
                     |> Cmd.map toMsg
                     |> Effect.sendCmd
                    )

                (Err error) ->
                    ( Model { model | errors = model.errors ++ [ Decode.errorToString error ]}, Effect.none)

        "nip98AuthHeader" ->
            case (Decode.decodeValue decodeAuthHeaderReceived value) of
                Ok decoded ->
                    if decoded.method == "GET" then
                        ( Model { model | authHeader = Just decoded.authHeader }
                        , Nip96.fetchFileList (ReceivedNip96FileList decoded.serverUrl) decoded.authHeader decoded.apiUrl
                        |> Cmd.map toMsg
                        |> Effect.sendCmd
                        )
                    else
                        ((Model model), Effect.none)

                Err error ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding NIP-98 auth header: " ++ Decode.errorToString error ]}, Effect.none)

        -- the file server lists may appear after the media selector is instantiated
        -- so we have to process them when arriving
        "events" ->
            case (Decode.decodeValue (Decode.field "kind" Nostr.Event.kindDecoder) value,
                  Decode.decodeValue (Decode.field "events" (Decode.list Nostr.Event.decodeEvent)) value) of
                (Ok KindUserServerList, Ok events) ->
                    updateModelWithUserServerLists (Model model) user toMsg events

                (Ok KindFileStorageServerList, Ok events) ->
                    updateModelWithFileStorageServerLists (Model model) user toMsg events

                _ ->
                    ((Model model), Effect.none)
        _ ->
            ( Model model, Effect.none)


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

updateModelWithUserServerLists : Model -> Auth.User -> (Msg msg -> msg) -> List Event -> (Model, Effect msg)
updateModelWithUserServerLists (Model model) user toMsg events =
    let
        -- usually there should be only one for the logged-in user
        newUserServers =
            events
            |> List.filter (\event -> user.pubKey == event.pubKey)
            |> List.map userServerListFromEvent
            |> List.foldl (\(_, userServerList) servers ->
                servers ++ userServerList
                ) []
            |> List.filter (\server ->
                    not <| Dict.member server model.blossomServers
                )

        newServerDict =
            serversWithUnknownState model.blossomServers newUserServers
        
        listAuthRequests = 
            requestBlossomListAuths newUserServers
            |> Effect.map toMsg

    in
    ( Model {model | blossomServers = newServerDict }, listAuthRequests)

updateModelWithFileStorageServerLists : Model -> Auth.User -> (Msg msg -> msg) -> List Event -> (Model, Effect msg)
updateModelWithFileStorageServerLists (Model model) user toMsg events =
    let
        -- usually there should be only one for the logged-in user
        newFileStorageServers =
            events
            |> List.filter (\event -> user.pubKey == event.pubKey)
            |> List.map fileStorageServerListFromEvent
            |> List.foldl (\(_, fileStorageServerList) servers ->
                servers ++ fileStorageServerList
                ) []
            |> List.filter (\server ->
                    not <| Dict.member server model.nip96Servers
                )

        newServerDict =
            serversWithUnknownState model.nip96Servers newFileStorageServers
        
        serverSpecRequests = 
            requestNip96ServerSpecs newFileStorageServers
            |> Effect.map toMsg
    in
    ( Model { model | nip96Servers = newServerDict}, serverSpecRequests)


view : MediaSelector msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    case model.displayType of
        DisplayModalDialog False ->
            div [][]

        DisplayModalDialog True ->
            Ui.Shared.modalDialog
                "Select image"
                [ viewMediaSelector (Settings settings) ]
                (settings.toMsg CloseDialog)

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
                    (uploadedNip96ImageFiles model.uploadedNip96Files serverUrl)

                Just (BlossomMediaServer serverUrl) ->
                    (Dict.get serverUrl model.uploadedBlossomFiles
                    |> Maybe.withDefault [])

                Just AllMediaServers ->
                    -- respect order of server list
                    (Dict.keys model.blossomServers
                    |> List.filterMap (\blossomServer -> Dict.get blossomServer model.uploadedBlossomFiles)
                    |> List.concat
                    )
                    ++
                    (Dict.keys model.nip96Servers
                    |> List.map (uploadedNip96ImageFiles model.uploadedNip96Files)
                    |> List.concat
                    )

                Just NoMediaServer ->
                    []

                Nothing ->
                    []

        dialogAttributes =
            case model.displayType of
                DisplayModalDialog _ ->
                    [ Tw.max_h_96
                    , Tw.overflow_y_auto
                    , Tw.grid_cols_3
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
    div []
        [ div
            [ css
                [ Tw.mt_6
                , Tw.flex
                , Tw.justify_between
                ]
            ]
            [ Components.Dropdown.new
                { model = model.serverSelectionDropdown
                , toMsg = DropdownSent
                , choices = selectableServers
                , toLabel = mediaServerToString settings.browserEnv.translations
                }
                |> Components.Dropdown.withOnChange ChangedSelectedServer
                |> Components.Dropdown.view
                |> Html.map settings.toMsg
            , Button.new
                { label = Translations.uploadButtonTitle [ settings.browserEnv.translations ]
                , onClick = Upload
                , theme = settings.theme
                }
                |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.upload)
                |> Button.withDisabled (List.length selectableServers < 1)
                |> Button.view
                |> Html.map settings.toMsg
            ]
        ,             {- Image Grid -}
        div
            [ css
                (dialogAttributes ++ 
                [ Tw.grid
                , Tw.gap_4
                , Tw.mt_4
                ])
            ]
            (List.map (imagePreview settings.onSelected) filesToShow
            |> List.map (Html.map settings.toMsg)
            )
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
    
preferredUploadServer : Model -> Maybe UploadServer
preferredUploadServer (Model model) =
    availableMediaServers (Model model)
    |> List.head
    |> Maybe.andThen (\mediaServer ->
        case mediaServer of
            BlossomMediaServer serverUrl ->
                (Just <| UploadDialog.UploadServerBlossom serverUrl)

            Nip96MediaServer serverUrl ->
                Dict.get serverUrl model.nip96ServerDescResponses
                |> Maybe.andThen (\serverDescResponse ->
                    case serverDescResponse of
                        Nip96.ServerDescriptor serverDescriptorData ->
                            (Just <| UploadDialog.UploadServerNip96 serverUrl serverDescriptorData)

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
            |> List.filterMap (\(serverUrl, state) ->
                    if state == ServerFileListReceived then
                        Just (BlossomMediaServer serverUrl)
                    else
                        Nothing
                )

        selectableNip96Servers =
            model.nip96Servers
            |> Dict.toList
            |> List.filterMap (\(serverUrl, state) ->
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
    |> Maybe.map (\uploadedFiles ->
        uploadedFiles
        |> List.filter (\uploadedFile ->
            case uploadedFile of
                Nip96File fileMetadata ->
                    Nip94.isImage fileMetadata

                BlossomFile _ ->
                    -- don't know what type blossom file is
                    True
            )
        ) 
    |> Maybe.withDefault []


imagePreview : Maybe (UploadedFile -> msg) -> UploadedFile -> Html (Msg msg)
imagePreview onSelected uploadedFile =
    let
        commonAttributes =
            [ css
                [ Tw.w_full
                , Tw.h_auto
                , Tw.max_h_56
                , Tw.bg_color Theme.gray_200
                , Tw.rounded_lg
                , Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.text_color Theme.gray_400
                ]
            , Events.onDoubleClick (SelectedItem { item = uploadedFile, onSelected = onSelected })
            ]
    in
    case uploadedFile of
        Nip96File nip96File ->
            let
                imageWidth =
                    200
            in
            img
                (commonAttributes ++
                [ css
                    [ ]
                , Attr.alt <| Maybe.withDefault "" nip96File.alt
                , Attr.src (Maybe.withDefault "" nip96File.url ++ "?w=" ++ String.fromInt imageWidth) -- NIP-96 servers can return scaled versions of images
                ])
                [ ]

        BlossomFile blobDescriptor ->
            img
                (commonAttributes ++
                [ css
                    [ ]
                , Attr.src blobDescriptor.url
                ])
                [ ]

subscribe : Model -> Sub (Msg msg)
subscribe (Model model) =
    Sub.batch
        [ Ports.receiveMessage IncomingMessage
        , Sub.map UploadDialogSent (UploadDialog.subscribe model.uploadDialog)
        ]
