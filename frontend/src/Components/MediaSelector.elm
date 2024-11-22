module Components.MediaSelector exposing
    ( MediaSelector, new
    , Model, init, DisplayType(..)
    , Msg, update, show
    , view
    , subscribe
    )

import Auth
import BrowserEnv exposing (BrowserEnv)
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, strong, text)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, href, type_)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Nostr.Blossom as Blossom exposing (BlobDescriptor)
import Nostr.Nip94 as Nip94
import Nostr.Nip96 as Nip96 exposing (extendRelativeServerDescriptorUrls)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Ports
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.MediaSelector
import Ui.Styles exposing (Styles)
import Dict

{-
blossomServer =
    "https://nostrmedia.com"
    -- "https://naughty-wood-53400.pktriot.net"
    --"http://localhost:4884"

nip96Server =
    "https://void.cat"
    -- "https://nostrmedia.com"
    -- "https://nostr.build"
-}

type MediaSelector msg
     = Settings
        { model : Model 
        , toMsg : Msg msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , styles : Styles msg
        }

new :
    { model : Model
    , toMsg : Msg msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , styles : Styles msg
    }
    -> MediaSelector msg
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
        { selected : Maybe Int
        , search : String
        , displayType : DisplayType
        , selectedServer : Maybe String
        , blossomServers : Dict String ServerState
        , nip96Servers : Dict String ServerState
        , nip96ServerDescResponses : Dict String Nip96.ServerDescResponse
        , authHeader : Maybe String
        , uploadedBlossomFiles : Dict String (List UploadedFile)
        , uploadedNip96Files : Dict String (List UploadedFile)
        , errors : List String
        }

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
        , selectedServer = Nothing
        , blossomServers = serversWithUnknownState props.blossomServers
        , nip96Servers = serversWithUnknownState props.nip96Servers
        , nip96ServerDescResponses = Dict.empty
        , authHeader = Nothing
        , uploadedBlossomFiles = Dict.empty
        , uploadedNip96Files = Dict.empty
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
                        , Effect.sendCmd <| Ports.requestNip96Auth 1 serverUrl serverDescWithExtendedUrls.apiUrl "GET"
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

updateModelWithBlossomFileList : Model -> String -> List Blossom.BlobDescriptor -> Model
updateModelWithBlossomFileList (Model model) serverUrl fileList =
    Model
        { model | uploadedBlossomFiles = Dict.insert serverUrl (List.map BlossomFile fileList) model.uploadedNip96Files
        , blossomServers = updateServerState model.blossomServers serverUrl ServerFileListReceived
        }

updateModelWithNip96FileList : Model -> String -> Nip96.FileList -> Model
updateModelWithNip96FileList (Model model) serverUrl fileList =
    Model
        { model | uploadedNip96Files = Dict.insert serverUrl (List.map Nip96File fileList.files) model.uploadedNip96Files
        , nip96Servers = updateServerState model.nip96Servers serverUrl ServerFileListReceived
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
            case (Decode.decodeValue (Decode.field "authHeader" Decode.string) value,
                  Decode.decodeValue (Decode.field "serverUrl"  Decode.string) value,
                  Decode.decodeValue (Decode.field "apiUrl"     Decode.string) value) of
                (Ok authHeader, Ok serverUrl, Ok apiUrl) ->
                    ( Model { model | authHeader = Just authHeader }
                    , Nip96.fetchFileList (ReceivedNip96FileList serverUrl) authHeader apiUrl
                     |> Cmd.map toMsg
                     |> Effect.sendCmd
                    )

                (_, _, _) ->
                    ( Model { model | errors = model.errors ++ [ "Error decoding NIP-98 auth header" ]}, Effect.none)

        _ ->
            ( Model model, Effect.none)


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
            div
                [ css
                    [ Tw.fixed
                    , Tw.inset_0
                    , Tw.bg_opacity_50
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.z_50
                    , Tw.max_h_screen
                    ]
                , Attr.id "modal-overlay"
                ]
            [ div
                [ css
                    [ Tw.bg_color Theme.white
                    , Tw.rounded_lg
                    , Tw.shadow_lg
                    , Tw.w_full
                    , Tw.max_w_lg
                    , Tw.p_6
                    ]
                ]
                [ viewMediaSelector (Settings settings)
                ]
            ]

        DisplayEmbedded ->
            viewMediaSelector (Settings settings)

viewMediaSelector : MediaSelector msg -> Html msg 
viewMediaSelector (Settings settings) =
    let
        (Model model) =
            settings.model


        filesToShow =
            case model.selectedServer of
                Just serverUrl ->
                    (Dict.get serverUrl model.uploadedNip96Files
                    |> Maybe.withDefault [])
                    ++
                    (Dict.get serverUrl model.uploadedBlossomFiles
                    |> Maybe.withDefault [])

                Nothing ->
                    -- respect order of server list
                    (Dict.keys model.blossomServers
                    |> List.filterMap (\blossomServer -> Dict.get blossomServer model.uploadedBlossomFiles)
                    |> List.concat
                    )
                    ++
                    (Dict.keys model.nip96Servers
                    |> List.filterMap (\nip96Server -> Dict.get nip96Server model.uploadedNip96Files)
                    |> List.concat
                    )

    in
    div []
        [ viewHeader model.displayType (Settings settings)
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
        ,             {- Upload Button -}
        div
            [ css
                [ Tw.mt_6
                , Tw.flex
                , Tw.justify_end
                ]
            ]
            [ label
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
                ]
                [ input
                    [ Attr.type_ "file"
                    , Attr.multiple True
                    , css
                        [ Tw.hidden
                        ]
                    ]
                    []
                , text <| Translations.MediaSelector.uploadImages [settings.browserEnv.translations] ]
            ]
        ]
    
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
                    [ text <| Translations.MediaSelector.selectImage [settings.browserEnv.translations] ]
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
            img
                (commonAttributes ++
                [ css
                    [ ]
                , Attr.alt <| Maybe.withDefault "" nip96File.alt
                , Attr.src (nip96File.url |> Maybe.withDefault "")
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
subscribe model =
    Ports.receiveMessage IncomingMessage
