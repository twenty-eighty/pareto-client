module MilkdownEditor exposing
    ( Content
    , DarkMode(..)
    , defaults
    , destroy
    , init
    , onBlur
    , onChange
    , onFocus
    , onLoad
    , update
    , view
    , withContent
    , withHeight
    , withDarkMode
    )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (attribute, style)
import Html.Styled.Events exposing (on)
import Json.Decode as JD


-- TYPES

type alias Content =
    String


type DarkMode
    = Light
    | Dark


type alias Options msg =
    { content : Content
    , darkMode : DarkMode
    , height : String
    , onchange : Maybe (Content -> msg)
    , onfocus : Maybe msg
    , onblur : Maybe msg
    , onload : Maybe msg
    , destroy : Bool
    }


-- DEFAULT OPTIONS

defaults : Options msg
defaults =
    { content = ""
    , darkMode = Light
    , height = "600px"
    , onchange = Nothing
    , onfocus = Nothing
    , onblur = Nothing
    , onload = Nothing
    , destroy = False
    }


-- OPTION SETTERS

withContent : String -> Options msg -> Options msg
withContent value options =
    { options | content = value }


withDarkMode : DarkMode -> Options msg -> Options msg
withDarkMode darkMode options =
    { options | darkMode = darkMode }


withHeight : String -> Options msg -> Options msg
withHeight height options =
    { options | height = height }


onChange : Maybe (Content -> msg) -> Options msg -> Options msg
onChange handler options =
    { options | onchange = handler }


onFocus : Maybe msg -> Options msg -> Options msg
onFocus handler options =
    { options | onfocus = handler }


onBlur : Maybe msg -> Options msg -> Options msg
onBlur handler options =
    { options | onblur = handler }


onLoad : Maybe msg -> Options msg -> Options msg
onLoad handler options =
    { options | onload = handler }


destroy : Bool -> Options msg -> Options msg
destroy shouldDestroy options =
    { options | destroy = shouldDestroy }


-- VIEW

view : Options msg -> Html msg
view options =
    Html.node "elm-milkdown-editor"
        ([ style "height" options.height
         , style "font-size" "1em"
         ]
            ++ contentAttr options.content
            ++ themeAttr options.darkMode
            ++ destroyAttr options.destroy
            ++ eventHandlers options
        )
        []


-- ATTRIBUTE HELPERS

contentAttr : String -> List (Html.Attribute msg)
contentAttr val =
    [ attribute "content" val ]


themeAttr : DarkMode -> List (Html.Attribute msg)
themeAttr darkMode =
    [ attribute "theme" (case darkMode of
                            Light -> "nord"
                            Dark -> "nord-dark"
                        )
    ]



destroyAttr : Bool -> List (Html.Attribute msg)
destroyAttr shouldDestroy =
    [ attribute "destroy" (if shouldDestroy then "true" else "false") ]


-- EVENT HANDLERS

eventHandlers : Options msg -> List (Html.Attribute msg)
eventHandlers options =
    List.concat
        [ case options.onchange of
            Just handler ->
                [ on "change" (decodeContent handler) ]

            Nothing ->
                []

        , case options.onfocus of
            Just handler ->
                [ on "focus" (JD.succeed handler) ]

            Nothing ->
                []

        , case options.onblur of
            Just handler ->
                [ on "blur" (JD.succeed handler) ]

            Nothing ->
                []

        , case options.onload of
            Just handler ->
                [ on "load" (JD.succeed handler) ]

            Nothing ->
                []
        ]


-- DECODERS

decodeContent : (Content -> msg) -> JD.Decoder msg
decodeContent toMsg =
    JD.at [ "detail", "content" ] JD.string
        |> JD.map toMsg


-- INITIAL MODEL

type alias Model =
    { content : Content }


init : Model
init =
    { content = "" }


-- UPDATE

type Msg msg
    = OnLoad msg
    | OnChange Content
    | OnFocus msg
    | OnBlur msg


update : Msg msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnLoad userMsg ->
            ( model, Cmd.none )

        OnChange newContent ->
            ( { model | content = newContent }, Cmd.none )

        OnFocus userMsg ->
            ( model, Cmd.none )

        OnBlur userMsg ->
            ( model, Cmd.none )
