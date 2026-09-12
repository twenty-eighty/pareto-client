module Components.HashtagEditor exposing
    ( Model
    , Msg
    , HashtagEditor
    , getHashtags
    , init
    , new
    , update
    , view
    )

import Browser.Dom as Dom
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, button, div, input, label, span, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import I18Next exposing (Translations)
import Json.Decode as Decode
import Process
import Task
import Tailwind.Utilities as Tw
import Translations.HashtagEditor as Translations
import Ui.Styles exposing (Theme, darkMode, stylesForTheme)


draftInputId : String
draftInputId =
    "hashtag-editor-draft"


maxTagLength : Int
maxTagLength =
    50


maxTagCount : Int
maxTagCount =
    20


type HashtagEditor msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , translations : Translations
        , theme : Theme
        }


type Model
    = Model Internal


type alias Internal =
    { initialHashtags : List String
    , hashtags : List String
    , draft : String
    , highlight : Maybe String
    }


type Msg
    = DraftChanged String
    | DraftKeyDown String
    | CommitDraft
    | RemoveHashtag String
    | EditHashtag String
    | ClearHighlight
    | FocusDone


new :
    { model : Model
    , toMsg : Msg -> msg
    , translations : Translations
    , theme : Theme
    }
    -> HashtagEditor msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , translations = props.translations
        , theme = props.theme
        }


init : { hashtags : List String } -> Model
init props =
    let
        normalized =
            normalizeList props.hashtags
    in
    Model
        { initialHashtags = normalized
        , hashtags = normalized
        , draft = ""
        , highlight = Nothing
        }


getHashtags : Model -> List String
getHashtags (Model model) =
    (commitTokens model.hashtags (splitDraft model.draft)).hashtags



-- NORMALIZATION


normalizeList : List String -> List String
normalizeList values =
    (commitTokens [] values).hashtags


splitDraft : String -> List String
splitDraft value =
    value
        |> String.replace "/" ","
        |> String.split ","


allowedChar : Char -> Bool
allowedChar c =
    Char.isAlphaNum c || c == ' ' || c == '_' || c == '-' || c == '.'


stripLeadingHashes : String -> String
stripLeadingHashes value =
    if String.startsWith "#" value then
        stripLeadingHashes (String.dropLeft 1 value)

    else
        value


stripWrappingQuotes : String -> String
stripWrappingQuotes value =
    let
        trimmed =
            String.trim value

        quoted quote =
            String.startsWith quote trimmed
                && String.endsWith quote trimmed
                && String.length trimmed
                >= 2
    in
    if quoted "\"" || quoted "'" then
        String.slice 1 -1 trimmed
            |> String.trim

    else
        trimmed


stripTrailingPunct : String -> String
stripTrailingPunct value =
    if String.endsWith "." value || String.endsWith "," value || String.endsWith ";" value then
        stripTrailingPunct (String.dropRight 1 value)

    else
        value


normalizeToken : String -> Maybe String
normalizeToken raw =
    raw
        |> stripWrappingQuotes
        |> stripLeadingHashes
        |> String.filter allowedChar
        |> String.trim
        |> stripTrailingPunct
        |> String.trim
        |> String.toLower
        |> String.left maxTagLength
        |> (\s ->
                if s == "" then
                    Nothing

                else
                    Just s
           )


type alias CommitResult =
    { hashtags : List String
    , highlight : Maybe String
    }


commitTokens : List String -> List String -> CommitResult
commitTokens existing rawTokens =
    rawTokens
        |> List.filterMap normalizeToken
        |> List.foldl addToken { hashtags = existing, highlight = Nothing }


addToken : String -> CommitResult -> CommitResult
addToken token acc =
    if List.member token acc.hashtags then
        { acc | highlight = Just token }

    else if List.length acc.hashtags >= maxTagCount then
        acc

    else
        { acc | hashtags = acc.hashtags ++ [ token ] }


containsSeparator : String -> Bool
containsSeparator value =
    String.contains "," value || String.contains "/" value



-- UPDATE


update :
    { msg : Msg
    , model : Model
    , modifiedMsg : Maybe (Bool -> msg)
    , toModel : Model -> model
    , toMsg : Msg -> msg
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

        withModified : Internal -> Effect msg -> ( Model, Effect msg )
        withModified next extra =
            let
                nextModel =
                    Model next

                modified =
                    model.initialHashtags /= getHashtags nextModel

                modifiedEffect =
                    props.modifiedMsg
                        |> Maybe.map (\toMsg -> Effect.sendMsg (toMsg modified))
                        |> Maybe.withDefault Effect.none
            in
            ( nextModel
            , Effect.batch [ extra, modifiedEffect ]
            )

        highlightEffect : Maybe String -> Effect msg
        highlightEffect maybeHighlight =
            case maybeHighlight of
                Just _ ->
                    Process.sleep 800
                        |> Task.perform (\_ -> props.toMsg ClearHighlight)
                        |> Effect.sendCmd

                Nothing ->
                    Effect.none

        focusDraftEffect : Effect msg
        focusDraftEffect =
            Dom.focus draftInputId
                |> Task.attempt (\_ -> props.toMsg FocusDone)
                |> Effect.sendCmd

        applyCommit : Bool -> String -> ( Model, Effect msg )
        applyCommit keepFocus raw =
            let
                result =
                    commitTokens model.hashtags (splitDraft raw)
            in
            withModified
                { model
                    | hashtags = result.hashtags
                    , draft = ""
                    , highlight = result.highlight
                }
                (Effect.batch
                    [ highlightEffect result.highlight
                    , if keepFocus then
                        focusDraftEffect

                      else
                        Effect.none
                    ]
                )
    in
    toParentModel <|
        case props.msg of
            DraftChanged value ->
                if containsSeparator value then
                    applyCommit True value

                else
                    withModified { model | draft = value } Effect.none

            DraftKeyDown "Enter" ->
                applyCommit True model.draft

            DraftKeyDown "Backspace" ->
                if String.isEmpty (String.trim model.draft) then
                    case List.reverse model.hashtags of
                        last :: rest ->
                            withModified
                                { model
                                    | hashtags = List.reverse rest
                                    , highlight = Nothing
                                }
                                Effect.none

                        [] ->
                            ( Model model, Effect.none )

                else
                    ( Model model, Effect.none )

            DraftKeyDown _ ->
                ( Model model, Effect.none )

            CommitDraft ->
                if String.trim model.draft == "" then
                    ( Model model, Effect.none )

                else
                    applyCommit False model.draft

            FocusDone ->
                ( Model model, Effect.none )

            RemoveHashtag tag ->
                withModified
                    { model
                        | hashtags = List.filter ((/=) tag) model.hashtags
                        , highlight = Nothing
                    }
                    Effect.none

            EditHashtag tag ->
                withModified
                    { model
                        | hashtags = List.filter ((/=) tag) model.hashtags
                        , draft = tag
                        , highlight = Nothing
                    }
                    focusDraftEffect

            ClearHighlight ->
                ( Model { model | highlight = Nothing }, Effect.none )



-- VIEW


view : HashtagEditor msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            stylesForTheme settings.theme

        atLimit =
            List.length model.hashtags >= maxTagCount

        hint =
            if atLimit then
                Translations.limitReached [ settings.translations ]

            else
                Translations.help [ settings.translations ]
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_1
            , Tw.w_full
            ]
        ]
        [ label [ css [ Tw.text_sm ] ] [ text (Translations.label [ settings.translations ]) ]
        , Keyed.node "div"
            (styles.colorStyleBackground
                ++ styles.colorStyleBorders
                ++ [ css
                        [ Tw.flex
                        , Tw.flex_wrap
                        , Tw.items_center
                        , Tw.gap_2
                        , Tw.w_full
                        , Tw.min_h_10
                        , Tw.px_2
                        , Tw.py_2
                        , Tw.rounded_md
                        , Tw.border_2
                        , Tw.box_border
                        ]
                   ]
            )
            (List.map
                (\tag -> ( "tag-" ++ tag, viewBadge settings.toMsg styles model.highlight tag ))
                model.hashtags
                ++ [ ( "hashtag-draft-input", viewDraftInput settings.toMsg model.draft atLimit settings.translations ) ]
            )
        , div
            (styles.colorStyleGrayscaleMuted
                ++ [ css [ Tw.text_sm ] ]
            )
            [ text hint ]
        ]


viewBadge : (Msg -> msg) -> Ui.Styles.Styles msg -> Maybe String -> String -> Html msg
viewBadge toMsg styles highlight tag =
    let
        isHighlighted =
            highlight == Just tag
    in
    div
        [ Attr.attribute "data-test" ("hashtag-badge-" ++ String.replace " " "-" tag)
        , Events.onClick (toMsg (EditHashtag tag))
        , css
            ([ Tw.inline_flex
             , Tw.items_center
             , Tw.gap_1
             , Tw.px_3
             , Tw.py_1
             , Tw.rounded_full
             , Tw.text_sm
             , Tw.cursor_pointer
             ]
                ++ (if isHighlighted then
                        [ Tw.bg_color styles.colorB5
                        , Tw.text_color styles.colorB1
                        , darkMode
                            [ Tw.bg_color styles.colorB5DarkMode
                            , Tw.text_color styles.colorB1DarkMode
                            ]
                        ]

                    else
                        [ Tw.bg_color styles.colorB4
                        , Tw.text_color styles.colorB1
                        , Css.hover
                            [ Tw.bg_color styles.colorB5
                            ]
                        , darkMode
                            [ Tw.bg_color styles.colorB4DarkMode
                            , Tw.text_color styles.colorB1DarkMode
                            , Css.hover
                                [ Tw.bg_color styles.colorB5DarkMode
                                ]
                            ]
                        ]
                   )
            )
        ]
        [ span [] [ text ("#" ++ tag) ]
        , button
            [ Attr.type_ "button"
            , Attr.attribute "aria-label" ("Remove " ++ tag)
            , Events.stopPropagationOn "click" (Decode.succeed ( toMsg (RemoveHashtag tag), True ))
            , css
                [ Tw.ml_1
                , Tw.border_0
                , Css.backgroundColor Css.transparent
                , Tw.cursor_pointer
                , Tw.p_0
                , Tw.leading_none
                , Tw.text_color styles.colorB1
                , darkMode [ Tw.text_color styles.colorB1DarkMode ]
                ]
            ]
            [ text "×" ]
        ]


viewDraftInput : (Msg -> msg) -> String -> Bool -> Translations -> Html msg
viewDraftInput toMsg draft atLimit translations =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.gap_1
            , Tw.flex_grow
            , Tw.min_w_32
            ]
        ]
        [ span [ css [ Tw.opacity_60 ] ] [ text "#" ]
        , input
            [ Attr.type_ "text"
            , Attr.id draftInputId
            , Attr.value draft
            , Attr.placeholder (Translations.placeholder [ translations ])
            , Attr.attribute "data-test" "hashtag-editor-input"
            , Attr.disabled atLimit
            , Events.onInput (DraftChanged >> toMsg)
            , Events.onBlur (toMsg CommitDraft)
            , onEditorKeyDown toMsg
            , css
                [ Tw.appearance_none
                , Css.backgroundColor Css.transparent
                , Tw.border_0
                , Tw.outline_none
                , Tw.flex_grow
                , Tw.min_w_24
                , Tw.py_1
                , Tw.w_full
                ]
            ]
            []
        ]


onEditorKeyDown : (Msg -> msg) -> Html.Attribute msg
onEditorKeyDown toMsg =
    Events.preventDefaultOn "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed ( toMsg (DraftKeyDown key), True )

                    else if key == "Backspace" then
                        Decode.succeed ( toMsg (DraftKeyDown key), False )

                    else
                        Decode.fail "ignored"
                )
        )
