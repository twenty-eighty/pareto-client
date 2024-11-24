module Components.MediaSelector exposing
    ( MediaSelector, new
    , Model, init, DisplayType(..)
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
import Css
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
import Nostr.Blossom as Blossom exposing (BlobDescriptor)
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
import Translations.MediaSelector
import Ui.Styles exposing (Styles, Theme)
import Ui.Shared


type MediaSelector msg
     = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }

new :
    { model : Model
    , toMsg : Msg msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> MediaSelector msg
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
        { selected : Maybe Int
        , search : String
        , displayType : DisplayType
        , serverSelectionDropdown : Components.Dropdown.Model MediaServer
        , selectedServer : MediaServer
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
    = Nip96File Nip94.FileMetadata
    | BlossomFile Blossom.BlobDescriptor


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
        , serverSelectionDropdown = Components.Dropdown.init { selected = Nothing }
        , selectedServer = NoMediaServer
        , blossomServers = serversWithUnknownState props.blossomServers
        , nip96Servers = serversWithUnknownState props.nip96Servers
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

serversWithUnknownState : List String -> Dict String ServerState
serversWithUnknownState serverUrls =
    serverUrls
    |> List.foldl (\serverUrl acc ->
        Dict.insert serverUrl ServerStateUnknown acc
        ) Dict.empty


requestBlossomListAuths : List String -> Effect (Msg msg)
requestBlossomListAuths blossomServers =
    blossomServers
    |> List.map (\blossomServer ->
        Effect.sendCmd <| Ports.requestBlossomListAuth 1 blossomServer
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
        { item : Int
        , onChange : Maybe msg
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
                ( Model { model | selectedServer = selectedServer }, Effect.none )

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
                ( Model 
                    { model
                        | search = ""
                        , selected = Just 1
                    }
                , case data.onChange of
                    Just onChange ->
                        Effect.sendMsg onChange
                    
                    Nothing ->
                        Effect.none
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
        UploadResponseNip96 apiUrl fileMetadata ->
            modelWithUploadedNip96File model apiUrl fileMetadata

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
        , selectedServer =
            selectableMediaServers (Model model)
            |> List.head
            |> Maybe.withDefault NoMediaServer
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
                        Just (UploadDialog.UploadServerNip96 serverDescriptorData)

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
        , selectedServer =
            selectableMediaServers (Model model)
            |> List.head
            |> Maybe.withDefault NoMediaServer
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
            case (Decode.decodeValue (Decode.field "authHeader" Decode.string) value,
                  Decode.decodeValue (Decode.field "url"        Decode.string) value) of
                (Ok authHeader, Ok url) ->
                    ( Model { model | authHeader = Just authHeader }
                    , Blossom.fetchFileList (ReceivedBlossomFileList url) authHeader url user.pubKey
                     |> Cmd.map toMsg
                     |> Effect.sendCmd
                    )

                (Err error, _) ->
                    ( Model { model | errors = model.errors ++ [ Decode.errorToString error ]}, Effect.none)

                (_, _) ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding blossom auth header" ]}, Effect.none)

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
            case model.selectedServer of
                Nip96MediaServer serverUrl ->
                    (uploadedNip96ImageFiles model.uploadedNip96Files serverUrl)

                BlossomMediaServer serverUrl ->
                    (Dict.get serverUrl model.uploadedBlossomFiles
                    |> Maybe.withDefault [])

                AllMediaServers ->
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

                NoMediaServer ->
                    []
    in
    div []
        [ viewHeader model.displayType (Settings settings)
        ,             {- Upload Button -}
        div
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
                , toLabel = mediaServerToString
                }
                |> Components.Dropdown.withOnChange ChangedSelectedServer
                |> Components.Dropdown.view
                |> Html.map settings.toMsg
            ,label
                [ css
                    [ Tw.cursor_pointer
                    , Tw.inline_flex
                    , Tw.items_center
                    , Tw.bg_color Theme.blue_500
                    , Tw.text_color Theme.white
                    , Tw.font_medium
                    , Tw.py_2
                    , Tw.px_4
                    , Tw.rounded_lg
                    , Css.hover
                        [ Tw.bg_color Theme.blue_600
                        ]
                    ]
                , Events.onClick Upload
                ]
                [ text <| Translations.MediaSelector.uploadButtonTitle [settings.browserEnv.translations]
                ]
                |> Html.map settings.toMsg
            ]
        ,             {- Image Grid -}
        div
            [ css
                [ Tw.grid
                , Tw.gap_4
                , Tw.mt_4
                , Bp.xxl
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
            ]
            (List.map imagePreview filesToShow)
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
    
selectableMediaServers : Model -> List MediaServer
selectableMediaServers (Model model) =
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
    case List.length selectableBlossomServers + List.length selectableNip96Servers of
        0 ->
            [ NoMediaServer ]

        1 ->
            selectableBlossomServers ++ selectableNip96Servers

        _ ->
            AllMediaServers :: (selectableBlossomServers ++ selectableNip96Servers)

mediaServerToString : MediaServer -> String
mediaServerToString mediaServer =
    case mediaServer of
        AllMediaServers ->
            "All servers"

        NoMediaServer ->
            "No server available"

        BlossomMediaServer serverUrl ->
            serverUrl ++ " (Blossom)"

        Nip96MediaServer serverUrl ->
            serverUrl ++ " (NIP-96)"


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


viewHeader : DisplayType -> MediaSelector msg -> Html msg
viewHeader displayType (Settings settings) =
    case displayType of
        DisplayModalDialog True ->
            div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.border_b
                    , Tw.pb_4
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        ]
                    ]
                    [ text <| Translations.MediaSelector.uploadButtonTitle [settings.browserEnv.translations] ]
                , button
                    [ css
                        [ Tw.text_color Theme.gray_400
                        , Css.hover
                            [ Tw.text_color Theme.gray_600
                            ]
                        ]
                    , Attr.id "close-modal"
                    , Events.onClick <| settings.toMsg CloseDialog
                    ]
                    [ text " ✕ " ]
                ]

        DisplayModalDialog False ->
            div [][]

        DisplayEmbedded ->
            div [][]

uploadButton =
    Button.new
        { label = "Upload"
        , onClick = Upload
        }
        |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.upload)
        |> Button.view

imagePreview : UploadedFile -> Html msg
imagePreview uploadedFile =
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
