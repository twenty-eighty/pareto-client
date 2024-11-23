module Components.UploadDialog exposing
    ( UploadDialog, new
    , Model, init
    , Msg, update, show
    , view
    , subscribe
    )

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
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
import Nostr.Blossom as Blossom exposing (BlobDescriptor)
import Nostr.Nip94 as Nip94
import Nostr.Nip96 as Nip96 exposing (extendRelativeServerDescriptorUrls)
import Nostr.Request exposing (HttpRequestMethod(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Ports
import Svg.Loaders
import SHA256
import Task exposing (Task)
import Time exposing (Posix, posixToMillis, millisToPosix)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.UploadDialog
import Ui.Styles exposing (Styles)
import Json.Decode as Decode

type UploadDialog msg
     = Settings
        { model : Model 
        , toMsg : Msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , styles : Styles msg
        }

new :
    { model : Model
    , toMsg : Msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , styles : Styles msg
    }
    -> UploadDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , styles = props.styles
        }


type Model 
    = Model
        { dialogVisible : Bool
        , uploadServer : Maybe UploadServer
        , uploadServers : List UploadServer
        , errors : List String
        , files : Dict Int FileUpload
        , nextFileId : Int
        }


type UploadServer
    = UploadServerBlossom String
    | UploadServerNip96 Nip96.ServerDescriptorData


type alias FileUpload =
    { file : File
    , status : UploadStatus
    , caption : String
    , alt : String
    , mediaType : String
    , noTransform : Bool
    , uploadResponse : Maybe UploadResponse
    }


type UploadStatus
    = Selected
    | Hashing
    | AwaitingAuthHeader String -- SHA256 Hash
    | ReadyToUpload String String -- SHA256 Hash, Auth Header
    | Uploading Float -- Progress percentage
    | Uploaded
    | Failed String -- Error message


type alias UploadResponse =
    { status : String
    , message : String
    , processingUrl : Maybe String
    , nip94Event : Maybe Nip94Event
    }


type alias Nip94Event =
    { tags : List (List String)
    , content : String
    }


init :
    { toMsg : Msg -> msg
    }
     -> Model
init props =
    Model
        { dialogVisible = False
        , uploadServer = Nothing
        , uploadServers = []
        , files = Dict.empty
        , nextFileId = 0
        , errors = []
        }

show : Model -> Model
show (Model model) =
    Model { model | dialogVisible = True }

type Msg
    = FocusedDropdown
    | BlurredDropdown
    | CloseDialog
    | IncomingMessage { messageType : String , value : Encode.Value }
    | TriggerFileSelect
    | FilesSelected File (List File)
    | UpdateCaption Int String
    | UpdateAltText Int String
    | UpdateMediaType Int String
    | ToggleNoTransform Int
    | StartUpload Int -- FileId
    | HashComputed Int String -- FileId, SHA256 Hash
    | UploadResult Int (Result Http.Error UploadResponse)
    | UploadProgress String Http.Progress
    | ErrorOccurred String


update :
    { msg : Msg
    , model : Model 
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , user : Auth.User
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
                ( Model { model | dialogVisible = True } , Effect.none)

            BlurredDropdown ->
                ( Model { model | dialogVisible = False } , Effect.none)

            CloseDialog ->
                ( Model { model | dialogVisible = False } , Effect.none)

            IncomingMessage { messageType, value } ->
                processIncomingMessage props.user props.model messageType props.toMsg value

            TriggerFileSelect ->
                ( Model model
                , FileSelect.files ["image/png", "image/jpg"] FilesSelected
                |> Cmd.map props.toMsg
                |> Effect.sendCmd
                )

            FilesSelected file files ->
                let
                    ( newFiles, cmds, nextId ) =
                        List.foldl
                            (\fileToUpload ( dict, cmdList, id ) ->
                                let
                                    fileUpload =
                                        { file = fileToUpload
                                        , status = Selected
                                        , caption = ""
                                        , alt = ""
                                        , mediaType = ""
                                        , noTransform = False
                                        , uploadResponse = Nothing
                                        }
                                in
                                ( Dict.insert id fileUpload dict, cmdList, id + 1 )
                            )
                            ( model.files, [], model.nextFileId )
                            (file :: files)
                in
                ( Model { model | files = newFiles, nextFileId = nextId }, Effect.sendCmd <| Cmd.batch cmds )

            UpdateCaption fileId captionText ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just upload ->
                                        Just { upload | caption = captionText }

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
                                    Just upload ->
                                        Just { upload | alt = altText }

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
                                    Just upload ->
                                        Just { upload | mediaType = mediaType }

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
                                    Just upload ->
                                        Just { upload | noTransform = not upload.noTransform }

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
                            Just upload ->
                                -- Compute the hash of the file in Elm
                                computeFileHash props.toMsg fileId upload.file
                                |> Effect.sendCmd

                            Nothing ->
                                Effect.none

                    updatedFiles =
                        Dict.update fileId
                            (\maybeUploadInner ->
                                case maybeUploadInner of
                                    Just upload ->
                                        Just { upload | status = Hashing }

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
                                    Just upload ->
                                        Just { upload | status = AwaitingAuthHeader hash }

                                    Nothing ->
                                        Nothing
                            )
                            model.files

                    effect =
                        case model.uploadServer of
                            Just (UploadServerBlossom _) ->
                                Effect.none

                            Just (UploadServerNip96 nip96ServerDescriptorData) ->
                                Effect.sendCmd <| Ports.requestNip96Auth fileId nip96ServerDescriptorData.downloadUrl nip96ServerDescriptorData.apiUrl (PostRequest hash)

                            Nothing ->
                                Effect.none
                in
                ( Model { model | files = updatedFiles }, effect )

            UploadResult fileId result ->
                let
                    updatedFiles =
                        Dict.update fileId
                            (\maybeUpload ->
                                case maybeUpload of
                                    Just upload ->
                                        case result of
                                            Ok response ->
                                                if response.status == "success" then
                                                    Just { upload | status = Uploaded, uploadResponse = Just response }
                                                else
                                                    Just { upload | status = Failed response.message, uploadResponse = Just response }

                                            Err error ->
                                                Just { upload | status = Failed (httpErrorToString error) }

                                    Nothing ->
                                        Nothing
                            )
                            model.files
                in
                ( Model { model | files = updatedFiles }, Effect.none )

            UploadProgress tracker progress ->
                let
                    fileId =
                        String.toInt tracker |> Maybe.withDefault -1

                    updatedFiles =
                        if fileId /= -1 then
                            Dict.update fileId
                                (\maybeUpload ->
                                    case maybeUpload of
                                        Just upload ->
                                            case upload.status of
                                                Uploading _ ->
                                                    let
                                                        progressValue =
                                                            case progress of
                                                                Http.Sending { sent, size } ->
                                                                    Http.fractionSent { sent = sent, size = size } * 100

                                                                Http.Receiving _ ->
                                                                    100
                                                    in
                                                    Just { upload | status = Uploading progressValue }

                                                _ ->
                                                    Just upload

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
processIncomingMessage user xModel messageType toMsg value =
    let
        (Model model) =
            xModel
    in

    case messageType of
--      "blossomAuthHeader" ->
--          case (Decode.decodeValue (Decode.field "authHeader" Decode.string) value,
--                Decode.decodeValue (Decode.field "url"        Decode.string) value) of
--              (Ok authHeader, Ok url) ->
--                  ( Model { model | authHeader = Just authHeader }
--                  , Blossom.fetchFileList (ReceivedBlossomFileList url) authHeader url user.pubKey
--                   |> Cmd.map toMsg
--                   |> Effect.sendCmd
--                  )

--              (Err error, _) ->
--                  ( Model { model | errors = model.errors ++ [ Decode.errorToString error ]}, Effect.none)

--              (_, _) ->
--                  ( Model { model | errors = model.errors ++ [ "Error decoding blossom auth header" ]}, Effect.none)

        "nip98AuthHeader" ->
            case (Decode.decodeValue decodeAuthHeaderReceived value) of
                (Ok decoded) ->
                    let
                        updatedFiles =
                            Dict.update decoded.requestId
                                (\maybeUpload ->
                                    case maybeUpload of
                                        Just upload ->
                                            case upload.status of
                                                AwaitingAuthHeader hash ->
                                                    Just { upload | status = ReadyToUpload hash decoded.authHeader }

                                                _ ->
                                                    Just upload

                                        Nothing ->
                                            Nothing
                                )
                                model.files

                        -- Start the upload
                        effect =
                            case Dict.get decoded.requestId updatedFiles of
                                Just upload ->
                                    case upload.status of
                                        ReadyToUpload hash authHeader ->
                                            uploadFile (Model model) decoded.requestId upload.file authHeader
                                            |> Cmd.map toMsg
                                            |> Effect.sendCmd

                                        _ ->
                                            Effect.none

                                _ ->
                                    Effect.none

                    in
                    ( Model { model | files = updatedFiles }, effect)

                (Err error) ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding NIP-98 auth header: " ++ Decode.errorToString error]}, Effect.none)

        _ ->
            ( Model model, Effect.none)


decodeAuthHeaderReceived : Decode.Decoder AuthHeaderReceived
decodeAuthHeaderReceived =
    Decode.map4 AuthHeaderReceived
        (Decode.field "fileId" Decode.int)
        (Decode.field "authHeader" Decode.string)
        (Decode.field "serverUrl" Decode.string)
        (Decode.field "apiUrl" Decode.string)

type alias AuthHeaderReceived =
    { requestId : Int
    , authHeader : String
    , serverUrl : String
    , apiUrl : String
    }

view : UploadDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    div [ class "p-4" ]
        [ div [ class "mb-4" ]
            [ button
                [ onClick TriggerFileSelect
                , class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                ]
                [ text "Select Files" ]
            ]
        , div []
            (Dict.toList model.files |> List.map viewFileUpload)
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


viewFileUpload : ( Int, FileUpload ) -> Html Msg
viewFileUpload ( fileId, fileUpload ) =
    let
        fileName =
            File.name <| fileUpload.file

        statusView =
            case fileUpload.status of
                Selected ->
                    div []
                        [ label [] [ text "Caption (optional):" ]
                        , textarea
                            [ onInput (UpdateCaption fileId)
                            , class "w-full border rounded p-2 mb-2"
                            ]
                            [ text fileUpload.caption ]
                        , label [] [ text "Alt Text (optional):" ]
                        , textarea
                            [ onInput (UpdateAltText fileId)
                            , class "w-full border rounded p-2 mb-2"
                            ]
                            [ text fileUpload.alt ]
                        , label [] [ text "Media Type (optional):" ]
                        , select
                            [ onInput (UpdateMediaType fileId)
                            , class "w-full border rounded p-2 mb-2"
                            ]
                            [ option [ value "" ] [ text "Select Media Type" ]
                            , option [ value "avatar" ] [ text "Avatar" ]
                            , option [ value "banner" ] [ text "Banner" ]
                            ]
                        , label [ class "inline-flex items-center mt-2" ]
                            [ input
                                [ type_ "checkbox"
                                , checked fileUpload.noTransform
                                , onClick (ToggleNoTransform fileId)
                                ]
                                []
                            , span [ class "ml-2" ] [ text "No Transform" ]
                            ]
                        , button
                            [ onClick (StartUpload fileId)
                            , class "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded mt-4"
                            ]
                            [ text "Start Upload" ]
                        ]

                Hashing ->
                    text "Computing file hash..."

                AwaitingAuthHeader _ ->
                    text "Awaiting authentication header..."

                ReadyToUpload _ _ ->
                    text "Ready to upload..."

                Uploading progressValue ->
                    div []
                        [ progress
                            [ Attr.max "100"
                            , value (String.fromFloat progressValue)
                            , class "w-full"
                            ] []
                        , div [ class "text-sm text-gray-700" ] [ text (String.fromInt (round progressValue) ++ "%") ]
                        ]

                Uploaded ->
                    case fileUpload.uploadResponse of
                        Just response ->
                            div []
                                [ div [ class "text-green-500" ] [ text "Upload completed." ]
                                , viewUploadResponse response
                                ]

                        Nothing ->
                            div [ class "text-green-500" ] [ text "Upload completed." ]

                Failed errorMsg ->
                    div [ class "text-red-500" ] [ text ("Upload failed: " ++ errorMsg) ]
    in
    div [ class "mb-4 border p-4 rounded" ]
        [ div [ class "font-bold mb-2" ] [ text fileName ]
        , statusView
        ]


viewUploadResponse : UploadResponse -> Html msg
viewUploadResponse response =
    div [ class "mt-2" ]
        (case response.nip94Event of
            Just nip94Event ->
                let
                    url =
                        tagValue nip94Event.tags "url"
                        |> Maybe.withDefault "No URL provided"

                    ox =
                        tagValue nip94Event.tags "ox"
                        |> Maybe.withDefault "No hash provided"
                in
                [ div [] [ text ("Download URL: " ++ url) ]
                , div [] [ text ("Original File Hash (ox): " ++ ox) ]
                ]

            Nothing ->
                [ div [] [ text "No NIP-94 event provided." ] ]
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

uploadButton =
    Button.new
        { label = "Upload"
        , onClick = TriggerFileSelect
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


uploadFile : Model -> Int -> File -> String -> Cmd Msg
uploadFile (Model model) fileId file authHeader =
    let
        url =
            "https://placeholder.api.url/upload" -- Replace with actual URL

        maybeUpload =
            Dict.get fileId model.files

        body =
            case maybeUpload of
                Just upload ->
                    multipartBody upload

                Nothing ->
                    Http.emptyBody

        request_ =
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Nostr " ++ authHeader) ]
                , url = url
                , body = body
                , expect = Http.expectJson (UploadResult fileId) uploadResponseDecoder
                , timeout = Nothing
                , tracker = Just (String.fromInt fileId)
                }
    in
    request_


multipartBody : FileUpload -> Http.Body
multipartBody upload =
    let
        sizeString =
            String.fromInt <| File.size upload.file

        contentTypeString =
             File.mime upload.file

        noTransformString =
            if upload.noTransform then
                "true"
            else
                "false"

        expirationString =
            "" -- Empty string for no expiration; can adjust as needed

        -- Collect form fields
        formFields =
            [ Http.stringPart "caption" upload.caption
            , Http.stringPart "alt" upload.alt
            , Http.stringPart "media_type" upload.mediaType
            , Http.stringPart "no_transform" noTransformString
            , Http.stringPart "size" sizeString
            , Http.stringPart "content_type" contentTypeString
            , Http.stringPart "expiration" expirationString
            , Http.filePart "file" upload.file
            ]
    in
    Http.multipartBody formFields


-- JSON DECODERS


uploadResponseDecoder : Decode.Decoder UploadResponse
uploadResponseDecoder =
    Decode.map4 UploadResponse
        (Decode.field "status" Decode.string)
        (Decode.field "message" Decode.string)
        (Decode.field "processing_url" (Decode.nullable Decode.string))
        (Decode.field "nip94_event" (Decode.nullable nip94EventDecoder))


nip94EventDecoder : Decode.Decoder Nip94Event
nip94EventDecoder =
    Decode.map2 Nip94Event
        (Decode.field "tags" (Decode.list (Decode.list Decode.string)))
        (Decode.field "content" Decode.string)

