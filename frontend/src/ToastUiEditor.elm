module ToastUiEditor exposing
    ( Content
    , Mode(..)
    , Theme(..)
    , defaults
    , destroy
    , init
    , onChange
    , update
    , view
    , withHeight
    , withInitialValue
    , withMode
    , withTheme
    , withZoom
    )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (attribute, style)
import Html.Styled.Events exposing (on)
import Json.Decode as JD
import Json.Encode as JE
import Tuple


type alias WysiwygSelection =
    ( Int, Int )


type alias MarkdownSelection =
    ( ( Int, Int ), ( Int, Int ) )


type Selection
    = WysiwygSelection
    | MarkdownSelection


type ViewState
    = WysiwygView { scrollpos : Int, selection : WysiwygSelection }
    | MarkdownView { scrollpos : Int, selection : MarkdownSelection }



-- content, state are current
-- ondisconnect is last state when custom element disconnectedCallback()
-- was called


type alias Content =
    String


type alias State =
    { content : Content, state : ViewState }


type Model
    = Connected State
    | Disconnected State



{--INIT --}


init : Model
init =
    Disconnected
        { content = ""
        , state = WysiwygView { scrollpos = 0, selection = ( 0, 0 ) }
        }



{--add option for set initial state --}


type Theme
    = Light
    | Dark


type Mode
    = Wysiwyg
    | Markdown



{--Options --}


type alias Options msg =
    { initialValue : Content
    , theme : Theme
    , mode : Mode
    , height : String
    , zoom : String
    , onchange : Maybe (Content -> msg)
    , destroy : Bool
    }


defaults : Options msg
defaults =
    { initialValue = ""
    , theme = Light
    , mode = Wysiwyg
    , height = "600px"
    , zoom = "1"
    , onchange = Nothing
    , destroy = False
    }


onChange : Maybe (Content -> msg) -> Options msg -> Options msg
onChange msg_ options_ =
    { options_ | onchange = msg_ }


destroy : Bool -> Options msg -> Options msg
destroy b options_ =
    { options_ | destroy = b }


destroyAttr : Bool -> List (Html.Attribute msg)
destroyAttr b =
    let
        bstr : String
        bstr =
            if b then
                "true"

            else
                "false"
    in
    [ bstr |> attribute "destroy" ]


withZoom : String -> Options msg -> Options msg
withZoom zoom_ options_ =
    { options_ | zoom = zoom_ }


zoomAttr : String -> List (Html.Attribute msg)
zoomAttr zoom_ =
    [ zoom_ |> attribute "zoom" ]


withInitialValue : String -> Options msg -> Options msg
withInitialValue value_ options =
    { options | initialValue = value_ }


initialValueAttr : String -> List (Html.Attribute msg)
initialValueAttr val =
    [ val |> attribute "initialvalue" ]


withTheme : Theme -> Options msg -> Options msg
withTheme theme options =
    { options | theme = theme }


themeAttr : Theme -> List (Html.Attribute msg)
themeAttr theme_ =
    case theme_ of
        Light ->
            []

        Dark ->
            [ attribute "theme" "dark" ]


withMode : Mode -> Options msg -> Options msg
withMode mode options =
    { options | mode = mode }


modeAttr : Mode -> List (Html.Attribute msg)
modeAttr mode_ =
    [ attribute "mode" (modeToString mode_) ]


modeToString : Mode -> String
modeToString mode_ =
    case mode_ of
        Wysiwyg ->
            "wysiwyg"

        Markdown ->
            "markdown"


withHeight : String -> Options msg -> Options msg
withHeight height_ options =
    { options | height = height_ }


heightAttr : String -> List (Html.Attribute msg)
heightAttr height_ =
    [ attribute "height" height_ ]


type Msg
    = OnLoad State
    | OnChange State
    | OnBlur State
    | OnFocus State
    | OnDisconnect State


loggingEventHandlers : (Content -> msg) -> List (Html.Attribute msg)
loggingEventHandlers msg =
    [ --on "load" <| loggingDecoder (decodeContent msg)
      on "change" <| loggingDecoder (decodeContent msg)

    --, on "blur_" <| loggingDecoder (decodeContent msg)
    --, on "focus" <| loggingDecoder (decodeContent msg)
    ]


eventHandlers : Bool -> (Content -> msg) -> List (Html.Attribute msg)
eventHandlers shouldLog msg_ =
    if shouldLog then
        loggingEventHandlers msg_

    else
        [ --on "load" <| decodeContent msg_
          on "change" <| decodeContent msg_

        --, on "blur_" <| decodeContent msg_
        --, on "focus" <| decodeContent msg_
        ]


onEvents : Bool -> Maybe (Content -> msg) -> List (Html.Attribute msg)
onEvents shouldLog maybeMsg =
    case maybeMsg of
        Nothing ->
            []

        Just msg ->
            eventHandlers noLog msg


noLog : Bool
noLog =
    False


withLog : Bool
withLog =
    True


view : Options msg -> Html msg
view options =
    Html.node "elm-toastui-editor"
        ([ style "height" "100vh"
         , style "font-size" "15px"
         ]
            ++ onEvents withLog options.onchange
            ++ initialValueAttr options.initialValue
            ++ themeAttr options.theme
            ++ modeAttr options.mode
            ++ heightAttr options.height
            ++ zoomAttr options.zoom
            ++ destroyAttr options.destroy
        )
        []



{--UPDATE --}


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnLoad state ->
            ( Connected state, Cmd.none )

        OnChange state ->
            ( Connected state, Cmd.none )

        OnBlur state ->
            ( Connected state, Cmd.none )

        OnFocus state ->
            ( Connected state, Cmd.none )

        OnDisconnect state ->
            ( Disconnected state, Cmd.none )



-- DECODERS


decDetailState : (State -> Msg) -> JD.Decoder Msg
decDetailState msg_ =
    decodeDetail msg_ decodeState


decodeContent : (Content -> msg) -> JD.Decoder msg
decodeContent msg_ =
    JD.map msg_ (JD.at [ "detail", "content" ] JD.string)


decodeDetail : (a -> msg) -> JD.Decoder a -> JD.Decoder msg
decodeDetail msg_ decoder =
    JD.map msg_ (JD.field "detail" decoder)


decodeState : JD.Decoder State
decodeState =
    let
        toRecord =
            \sp sel -> { scrollpos = sp, selection = sel }
    in
    JD.map2 State
        (JD.field "content" JD.string)
        (JD.oneOf
            [ JD.map MarkdownView
                (JD.map2 toRecord
                    (JD.field "scrollpos" JD.int)
                    (JD.field "selection" <| decodePair (decodePair JD.int))
                )
            , JD.map WysiwygView
                (JD.map2 toRecord
                    (JD.field "scrollpos" JD.int)
                    (JD.field "selection" (decodePair JD.int))
                )
            ]
        )


decodePair : JD.Decoder a -> JD.Decoder ( a, a )
decodePair dec =
    JD.map2 Tuple.pair
        (JD.index 0 dec)
        (JD.index 1 dec)


loggingDecoder : JD.Decoder a -> JD.Decoder a
loggingDecoder realDecoder =
    JD.value
        |> JD.andThen
            (\event ->
                case JD.decodeValue realDecoder event of
                    Ok decoded ->
                        JD.succeed decoded

                    Err error ->
                        error
                            |> JD.errorToString
                            |> JD.fail
            )



-- ENCODERS


encodeViewState : ViewState -> JE.Value
encodeViewState state =
    case state of
        WysiwygView { scrollpos, selection } ->
            JE.object
                [ ( "mode", JE.string "wysiwyg" )
                , ( "scrollpos", JE.int scrollpos )
                , ( "selection", encWysiwygSel selection )
                ]

        MarkdownView { scrollpos, selection } ->
            JE.object
                [ ( "mode", JE.string "markdown" )
                , ( "scrollpos", JE.int scrollpos )
                , ( "selection", encMarkdownSel selection )
                ]


encWysiwygSel : WysiwygSelection -> JE.Value
encWysiwygSel ( start, end ) =
    JE.list JE.int [ start, end ]


encMarkdownSel : MarkdownSelection -> JE.Value
encMarkdownSel ( ( sr, sc ), ( er, ec ) ) =
    JE.list (JE.list JE.int)
        [ [ sr, sc ], [ er, ec ] ]
