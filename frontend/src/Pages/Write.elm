module Pages.Write exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.MediaSelector as MediaSelector exposing (MediaSelector)
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Hex
import Html.Styled as Html exposing (Html, a, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Layouts
import MilkdownEditor as Milkdown
import Murmur3
import Nostr
import Nostr.Event as Event exposing (Event, Kind(..), Tag(..))
import Nostr.Nip19 as Nip19
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time
import Translations.Write
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared route
        , update = update shared user
        , subscriptions = subscriptions
        , view = view user shared.browserEnv
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        {}


-- INIT


type alias Model =
    { title : Maybe String
    , summary : Maybe String
    , image : Maybe String
    , content : Maybe String
    , identifier : Maybe String
    , tags : Maybe String
    , now : Time.Posix
    , mediaSelector : MediaSelector.Model
    }

init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    let
        maybeNip19 =
            case Dict.get "a" route.query of
                Just address ->
                    Nip19.decode address
                    |> Result.toMaybe

                Nothing ->
                    Nothing

        maybeArticle =
            maybeNip19
            |> Maybe.andThen (\nip19 -> Nostr.getArticleForNip19 shared.nostr nip19)

        effect =
            case (maybeArticle, maybeNip19) of
                (Nothing, Just nip19) ->
                    Event.eventFilterForNip19 nip19
                    |> Maybe.map RequestArticle
                    |> Maybe.map (Nostr.createRequest shared.nostr "Article described as NIP-19 for editing" [])
                    |> Maybe.map Shared.Msg.RequestNostrEvents
                    |> Maybe.map Effect.sendSharedMsg
                    |> Maybe.withDefault Effect.none

                (_, _) ->
                    Effect.none

        (mediaSelector, mediaSelectorEffect) =
            MediaSelector.init { selected = Nothing, toMsg = MediaSelectorSent }

        model =
            case maybeArticle of
                Just article ->
                    { title = article.title
                    , summary = article.summary
                    , image = article.image
                    , content = Just article.content
                    , identifier = article.identifier
                    , tags = tagsString article.hashtags
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    }

                Nothing ->
                    { title = Nothing
                    , summary = Nothing
                    , image = Nothing
                    , content = Nothing
                    , identifier = Nothing
                    , tags = Nothing
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    }

    in
    (model, mediaSelectorEffect)

tagsString : List String -> Maybe String
tagsString tags =
    case tags of
        [] ->
            Nothing

        [ tag ] ->
            Just tag
        
        tagList ->
            Just <| String.join ", " tagList


-- UPDATE


type Msg
    = EditorChanged String
    | EditorFocused
    | EditorBlurred
    | EditorLoaded
    | UpdateTitle String
    | UpdateSubtitle String
    | UpdateTags String
    | Publish
    | SaveDraft
    | Now Time.Posix
    | MediaSelectorSent (MediaSelector.Msg Msg)


update : Shared.Model -> Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update shared user msg model =
    case msg of
        EditorChanged newContent ->
            if newContent == "" then
                ( { model | content = Nothing }, Effect.none )
            else
                ( { model | content = Just newContent }, Effect.none )

        EditorFocused ->
            ( model, Effect.none )

        EditorBlurred ->
            ( model, Effect.none )

        EditorLoaded ->
            ( model, Effect.none )

        UpdateTitle title ->
            if title == "" then
                ( { model | title = Nothing }, Effect.none )
            else
                ( { model | title = Just <| filterTitleChars title }, Effect.none )

        UpdateSubtitle summary ->
            if summary == "" then
                ( { model | summary = Nothing }, Effect.none )
            else
                ( { model | summary = Just <| filterTitleChars summary }, Effect.none )

        UpdateTags tags ->
            if tags == "" then
                ( { model | tags = Nothing }, Effect.none )
            else
                ( { model | tags = Just tags }, Effect.none )

        Publish ->
            ( model, Effect.none )

        SaveDraft ->
            ( model, sendDraftCmd shared model user )

        Now now ->
            if model.identifier == Nothing then
                ( { model | now = now, identifier = now |> Time.posixToMillis |> String.fromInt |> Just }, Effect.none )
            else
                ( { model | now = now }, Effect.none )

        MediaSelectorSent innerMsg ->
            MediaSelector.update
                { user = user
                , msg = innerMsg
                , model = model.mediaSelector
                , toModel = \mediaSelector -> { model | mediaSelector = mediaSelector }
                , toMsg = MediaSelectorSent
                }


sendDraftCmd : Shared.Model -> Model -> Auth.User -> Effect Msg
sendDraftCmd shared model user =
    eventWithContent shared model user KindDraftLongFormContent
    |> SendLongFormDraft 
    |> Shared.Msg.SendNostrEvent
    |> Effect.sendSharedMsg

eventWithContent : Shared.Model -> Model -> Auth.User -> Kind -> Event
eventWithContent shared model user kind =
    { pubKey = user.pubKey
    , createdAt = shared.browserEnv.now
    , kind = kind
    , tags =
        [ ]
        |> Event.addTitleTag model.title
        |> Event.addSummaryTag model.summary
        |> Event.addImageTag model.image
        |> Event.addIdentifierTag model.identifier
        |> Event.addTagTags model.tags
        |> Event.addClientTag Pareto.client
    , content = model.content |> Maybe.withDefault ""
    , id = ""
    , sig = Nothing
    }

filterTitleChars : String -> String
filterTitleChars title =
    title
    |> String.trimLeft
    |> String.replace "\n" ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Now
        , Sub.map MediaSelectorSent (MediaSelector.subscribe model.mediaSelector)
        -- Sub.none
        ]



-- VIEW


view : Auth.User -> BrowserEnv -> Model -> View Msg
view user browserEnv model =
    { title = "Write"
    , body =
        [ div
            [ css
                [ Tw.mx_10
                , Tw.mt_12
                , Tw.space_y_5
                , Bp.xl
                    [ Tw.mx_40
                    ]
                , Bp.md
                    [ Tw.mx_20
                    ]
                ]
            ]
            [ viewTitle browserEnv model
            , viewSubtitle browserEnv model
            , viewEditor browserEnv model
            , viewTags  browserEnv model
            , saveButtons browserEnv model
            , MediaSelector.new
                { model = model.mediaSelector
                , toMsg = MediaSelectorSent
                , pubKey = user.pubKey
                , browserEnv = browserEnv
                }
                |> MediaSelector.view
            ]
        ]
    }

viewTitle : BrowserEnv -> Model -> Html Msg
viewTitle browserEnv model =
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ node "auto-resize-textarea"
            [ Attr.attribute "value" (model.title |> Maybe.withDefault "")
            , Attr.attribute "color" "rgb(55,55,55)"
            , Attr.attribute "fontfamily" "Inter"
            , Attr.attribute "fontsize" "36px"
            , Attr.attribute "fontWeight" "700"
            , Attr.attribute "placeholder" ( Translations.Write.editorTitlePlaceholderText [ browserEnv.translations ])
            , on "input-change" (Decode.map UpdateTitle decodeInputChange)
            ]
            []
        ]
    

viewSubtitle : BrowserEnv -> Model -> Html Msg
viewSubtitle browserEnv model =
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ node "auto-resize-textarea"
            [ Attr.attribute "value" (model.summary |> Maybe.withDefault "")
            , Attr.attribute "color" "rgb(55,55,55)"
            , Attr.attribute "fontfamily" "Inter"
            , Attr.attribute "fontsize" "19px"
            , Attr.attribute "fontWeight" "700"
            , Attr.attribute "placeholder" ( Translations.Write.editorSubtitlePlaceholderText [ browserEnv.translations ])
            , on "input-change" (Decode.map UpdateSubtitle decodeInputChange)
            ]
            []
        ]

decodeInputChange : Decode.Decoder String
decodeInputChange =
    Decode.field "detail" (Decode.field "value" Decode.string)


viewEditor : BrowserEnv -> Model -> Html Msg
viewEditor browserEnv model =
            div
                [ css
                    [ Tw.w_full
                    ]
                ]
                [ milkdownEditor (model.content |> Maybe.withDefault "")
                ]

statusDiv : Model -> Html msg
statusDiv model =
    div []
        [ code
            [ style "color" "#017575"
            , style "font-family" "Monaco"
            , style "font-weight" ""
            , style "font-size" "12px"
            , style "border-radius" "4px"
            , style "margin-left" "16px"
            ]
            [ text (modelToStatusString model) ]
        ]

modelToStatusString : Model -> String
modelToStatusString model =
    model.content
    |> Maybe.map hexHash
    |> Maybe.withDefault ""

hexHash : Milkdown.Content -> String
hexHash str =
    str
        |> Murmur3.hashString 1234
        |> Hex.toString
        |> (++) "0x"


milkdownEditor : Milkdown.Content -> Html Msg
milkdownEditor content =
   Milkdown.view
        (Milkdown.defaults
            |> Milkdown.withContent content
--          |> Milkdown.withPlaceholder "Start typing..."
            |> Milkdown.withTheme Milkdown.Dark
            |> Milkdown.onChange (Just EditorChanged)
            |> Milkdown.onFocus (Just EditorFocused)
            |> Milkdown.onBlur (Just EditorBlurred)
            |> Milkdown.onLoad (Just EditorLoaded)
        )

sampleDoc : String
sampleDoc =
    """
# heading 1

some text

## heading 2

* a **list**
* **another** item
* *third*

> quote
> me
> on

```python
for x in range(100):
    print(x)

```"""

viewTags : BrowserEnv -> Model -> Html Msg
viewTags browserEnv model =
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [         {- Label -}
        label
            [ Attr.for "entry-field"
            , css
                [ Tw.block
                , Tw.text_color Theme.gray_700
                , Tw.text_sm
                , Tw.font_medium
                , Tw.mb_2
                ]
            ]
            [ text "Tags" ]
        ,         {- Input Field -}
        input
            [ Attr.type_ "text"
            , Attr.id "entry-field"
            , Attr.placeholder "Tags, comma separated"
            , Attr.value (model.tags |> Maybe.withDefault "")
            , Events.onInput UpdateTags
            , css
                [ Tw.w_full
                , Tw.px_4
                , Tw.py_2
                , Tw.border
                , Tw.border_color Theme.gray_300
                , Tw.rounded_lg
                , Tw.transition_all
                , Tw.duration_200
                , Css.focus
                    [ Tw.outline_none
                    , Tw.ring_2
                    , Tw.ring_color Theme.blue_500
                    , Tw.border_color Theme.blue_500
                    ]
                ]
            ]
            []
        ]
    
        


saveButtons : BrowserEnv -> Model -> Html Msg
saveButtons browserEnv mmodel =
    div
        [ css
            [ Tw.flex
            , Tw.justify_between
            , Tw.mb_10
            ]
        ]
        [ saveDraftButton browserEnv
        , publishButton browserEnv
        ]


publishButton : BrowserEnv -> Html Msg
publishButton browserEnv =
    button
        [ css
            [ Tw.bg_color Theme.orange_500
            , Tw.text_color Theme.white
            , Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Css.hover
                [ Tw.bg_color Theme.orange_700
                ]
            ]
        , Events.onClick Publish
        ]
        [ text <| Translations.Write.publishButtonTitle [ browserEnv.translations ] ]

saveDraftButton : BrowserEnv -> Html Msg
saveDraftButton browserEnv =
    button
        [ css
            [ Tw.bg_color Theme.gray_200
            , Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Css.hover
                [ Tw.bg_color Theme.gray_300
                ]
            ]
        , Events.onClick SaveDraft
        ]
        [ text <| Translations.Write.saveDraftButtonTitle [ browserEnv.translations ] ]