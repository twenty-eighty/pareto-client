module Milkdown.MilkdownEditor exposing
    ( Content
    , DarkMode(..)
    , Model
    , Msg
    , defaults
    , destroy
    , init
    , onBlur
    , onChange
    , onFileRequest
    , onFocus
    , onLoad
    , setSelectedImage
    , update
    , view
    , withContent
    , withDarkMode
    , withHeight
    )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (attribute, style)
import Html.Styled.Events exposing (on)
import Json.Decode as JD
import Json.Encode as JE



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
    , onfilerequest : Maybe msg
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
    , onfilerequest = Nothing
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


onFileRequest : Maybe msg -> Options msg -> Options msg
onFileRequest handler options =
    { options | onfilerequest = handler }


destroy : Bool -> Options msg -> Options msg
destroy shouldDestroy options =
    { options | destroy = shouldDestroy }



-- VIEW


view : (Msg msg -> msg) -> Options msg -> Model -> Html msg
view toMsg options model =
    Html.node "elm-milkdown-editor"
        ([ style "height" options.height
         , style "font-size" "1em"
         , on "receivedSelectedFile" (JD.succeed (toMsg OnReceivedSelectedFile))
         ]
            ++ contentAttr options.content
            ++ selectedFileAttr model.selectedFile
            ++ themeAttr options.darkMode
            ++ destroyAttr options.destroy
            ++ eventHandlers options
        )
        []



-- ATTRIBUTE HELPERS


contentAttr : String -> List (Html.Attribute msg)
contentAttr val =
    [ attribute "content" val ]


selectedFileAttr : Maybe ( String, Maybe String, Maybe String ) -> List (Html.Attribute msg)
selectedFileAttr selectedFile =
    case selectedFile of
        Just ( url, caption, maybeAlt ) ->
            let
                value =
                    JE.object
                        [ ( "url", JE.string url )
                        , ( "caption", JE.string (Maybe.withDefault "" caption) )
                        , ( "alt", JE.string (Maybe.withDefault "" maybeAlt) )
                        ]
                        |> JE.encode 0
            in
            [ attribute "selectedfile" value
            ]

        Nothing ->
            [ attribute "selectedfile" "{}" ]


themeAttr : DarkMode -> List (Html.Attribute msg)
themeAttr darkMode =
    [ attribute "theme"
        (case darkMode of
            Light ->
                "nord"

            Dark ->
                "nord-dark"
        )
    ]


destroyAttr : Bool -> List (Html.Attribute msg)
destroyAttr shouldDestroy =
    [ attribute "destroy"
        (if shouldDestroy then
            "true"

         else
            "false"
        )
    ]



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
        , case options.onfilerequest of
            Just handler ->
                [ on "filerequest" (JD.succeed handler) ]

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
    { content : Content
    , selectedFile : Maybe ( String, Maybe String, Maybe String ) -- url, caption, alt
    }


init : Model
init =
    { content = ""
    , selectedFile = Nothing
    }



-- UPDATE


type Msg msg
    = OnLoad msg
    | OnChange Content
    | OnFocus msg
    | OnBlur msg
    | OnFileRequest msg
    | OnReceivedSelectedFile


update : Msg msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnLoad _ ->
            ( model, Cmd.none )

        OnChange newContent ->
            ( { model | content = newContent }, Cmd.none )

        OnFocus _ ->
            ( model, Cmd.none )

        OnBlur _ ->
            ( model, Cmd.none )

        OnFileRequest _ ->
            ( model, Cmd.none )

        OnReceivedSelectedFile ->
            ( { model | selectedFile = Nothing }, Cmd.none )


setSelectedImage : Model -> String -> Maybe String -> Maybe String -> Model
setSelectedImage model url caption alt =
    { model | selectedFile = Just ( url, caption, alt ) }
