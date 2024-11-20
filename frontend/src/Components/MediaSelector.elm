module Components.MediaSelector exposing
    ( MediaSelector, new
    , Model, init
    , Msg, update
    , view
    , subscribe
    )

import Auth
import BrowserEnv exposing (BrowserEnv)
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, strong, text)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, href, type_)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Nostr.Blossom as Blossom exposing (BlobDescriptor)
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

blossomServer =
    "https://nostrmedia.com"
    -- "https://naughty-wood-53400.pktriot.net"
    --"http://localhost:4884"

nip96Server =
    "https://void.cat"
    -- "https://nostrmedia.com"
    -- "https://nostr.build"

type MediaSelector msg
     = Settings
        { model : Model 
        , toMsg : Msg msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , styles : Styles msg
        , blossomServers : List String
        , nip96Servers : List String
        }

new :
    { model : Model
    , toMsg : Msg msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , styles : Styles msg
    , blossomServers : List String
    , nip96Servers : List String
    }
    -> MediaSelector msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , styles = props.styles
        , blossomServers = props.blossomServers
        , nip96Servers = props.nip96Servers
        }


type Model 
    = Model
        { selected : Maybe Int
        , search : String
        , isMenuOpen : Bool
        , authHeader : Maybe String
        , nip96Files : List Nip96.File
        , errors : List String
        }



init : { selected : Maybe item, toMsg : Msg msg -> msg } -> ( Model, Effect msg )
init props =
    ( Model
        { selected = Just 0
        , search = ""
        , isMenuOpen = False
        , authHeader = Nothing
        , nip96Files = []
        , errors = []
        }
--  , Effect.sendCmd <| Ports.requestBlossomListAuth 1 blossomServer
    , Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc nip96Server) nip96Server)
    |> Effect.map props.toMsg
    )

type Msg msg
    = FocusedDropdown
    | BlurredDropdown
    | UpdatedSearchInput String
    | IncomingMessage { messageType : String , value : Encode.Value }
    | ReceivedBlossomFileList (Result Http.Error (List BlobDescriptor))
    | ReceivedNip96ServerDesc String (Result Http.Error Nip96.ServerDescResponse)
    | ReceivedNip96FileList (Result Http.Error Nip96.FileList)
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
                ( Model { model | isMenuOpen = True }
                , Effect.none
                )

            BlurredDropdown ->
                ( Model { model | search = "", isMenuOpen = False }
                , Effect.none
                )

            UpdatedSearchInput input ->
                ( Model { model | search = input, isMenuOpen = False }
                , Effect.none
                )

            IncomingMessage { messageType, value } ->
                processIncomingMessage props.user props.model messageType props.toMsg value

            SelectedItem data ->
                ( Model 
                    { model
                        | search = ""
                        , isMenuOpen = False
                        , selected = Just 1
                    }
                , case data.onChange of
                    Just onChange ->
                        Effect.sendMsg onChange
                    
                    Nothing ->
                        Effect.none
                )

            ReceivedBlossomFileList result ->
                case result of
                    Ok fileList ->
                        (Model model, Effect.none)
                    Err error ->
                        (Model model, Effect.none)


            ReceivedNip96ServerDesc serverUrl result ->
                case result of
                    Ok (Nip96.ServerRedirect serverRedirection) ->
                        ( Model model
                        , Effect.sendCmd (Nip96.fetchServerSpec (ReceivedNip96ServerDesc serverUrl) serverRedirection.delegated_to_url)
                        |> Effect.map props.toMsg
                        )

                    Ok (Nip96.ServerDescriptor serverDescriptorData) ->
                        let
                            serverDescWithExtendedUrls =
                                extendRelativeServerDescriptorUrls serverUrl serverDescriptorData
                        in
                        ( Model model
                        , Effect.sendCmd <| Ports.requestNip96Auth 1 serverDescWithExtendedUrls.apiUrl "GET"
                        )

                    Err error ->
                        let
                            errorstring =
                                httpErrorToString error
                        in
                        (Model { model | errors = model.errors ++ [ errorstring ] }, Effect.none)

            ReceivedNip96FileList result ->
                case result of
                    Ok fileList ->
                        (Model { model | nip96Files = fileList.files }, Effect.none)
                    Err error ->
                        (Model model, Effect.none)

processIncomingMessage : Auth.User -> Model -> String -> (Msg msg -> msg) -> Encode.Value -> (Model, Effect msg)
processIncomingMessage user xModel messageType toMsg value =
    let
        (Model model) =
            xModel
    in

    case messageType of
        "blossomAuthHeader" ->
            case Decode.decodeValue (Decode.field "authHeader" Decode.string) value of
                Ok authHeader ->
                    ( Model { model | authHeader = Just authHeader }
                    , Blossom.fetchFileList ReceivedBlossomFileList authHeader blossomServer user.pubKey
                     |> Cmd.map toMsg
                     |> Effect.sendCmd
                    )

                Err error ->
                    ( Model model, Effect.none)

        "nip98AuthHeader" ->
            case (Decode.decodeValue (Decode.field "authHeader" Decode.string) value,
                  Decode.decodeValue (Decode.field "url"        Decode.string) value) of
                (Ok authHeader, Ok url) ->
                    ( Model { model | authHeader = Just authHeader }
                    , Nip96.fetchFileList ReceivedNip96FileList authHeader url
                     |> Cmd.map toMsg
                     |> Effect.sendCmd
                    )

                (_, _) ->
                    ( Model model, Effect.none)

        _ ->
            ( Model model, Effect.none)


view : MediaSelector msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        onSearchInput : String -> msg
        onSearchInput value =
            settings.toMsg (UpdatedSearchInput value)

        -- View the input of the dropdown, that opens the 
        -- menu when focused, and displays the search query
        viewDropdownInput : Html msg
        viewDropdownInput =
            div [ class "dropdown__toggle" ]
                [ input
                    [ class "dropdown__input"
                    , type_ "search"
                    , onInput onSearchInput
                    , onFocus (settings.toMsg FocusedDropdown)
                    , onBlur (settings.toMsg BlurredDropdown)
                    ]
                    []
                , viewSelectedValueOverlay
                ]

        -- If a value is selected, this overlay should
        -- appear over our input field when the menu is closed
        viewSelectedValueOverlay : Html msg
        viewSelectedValueOverlay = 
            case model.selected of
                Nothing ->
                    text ""

                Just item ->
                    if model.isMenuOpen then
                        text ""

                    else
                        strong
                            [ class "dropdown__selected" ]
                            [ text "text" ] -- (settings.toLabel item) ]


        viewDropdownMenu : Html msg
        viewDropdownMenu =
            if model.isMenuOpen then
                div [ class "dropdown__menu" ]
                    [] -- (List.map viewDropdownMenuItem settings.choices)

            else
                text ""
    in
    div
        [ css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.bg_opacity_50
            , Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.z_50
            ]
        , Attr.id "modal-overlay"
        ]
        [         {- Modal Content -}
        div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.w_full
                , Tw.max_w_lg
                , Tw.p_6
                ]
            ]
            [             {- Modal Header -}
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
                    ]
                    [ text " ✕ " ]
                ]
            ,             {- Image Grid -}
            div
                [ css
                    [ Tw.grid
                    , Tw.grid_rows_2
                    , Tw.grid_cols_3
                    , Tw.gap_4
                    , Tw.mt_4
                    ]
                ]
                (List.map imagePreview model.nip96Files)
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
        ]
    
imagePreview : Nip96.File -> Html msg
imagePreview nip96File =
    img
        [ css
            [ Tw.w_full
            , Tw.h_24
            , Tw.bg_color Theme.gray_200
            , Tw.rounded_lg
            , Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.text_color Theme.gray_400
            ]
        , Attr.src (nip96File.url |> Maybe.withDefault "")
        ]
        [ text " Image 1 " ]

subscribe : Model -> Sub (Msg msg)
subscribe model =
    Ports.receiveMessage IncomingMessage
