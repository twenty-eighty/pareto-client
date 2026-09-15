module Components.ArticleHighlights exposing
    ( Model
    , Msg
    , init
    , update
    , view
    , subscriptions
    )

{-| Display article highlights (NIP-84) and publish a selection as kind 9802.
-}

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, blockquote, div, h2, p, span, text)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Highlights exposing (Highlight)
import Nostr.Profile exposing (ProfileValidation(..), profileDisplayName)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, LoginStatus, loggedInSigningPubKey)
import Ports
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.ArticleHighlights as Translations
import Ui.Profile exposing (viewProfileImageSmall)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


type Model
    = Model
        { awaitingSelection : Bool
        }


init : Model
init =
    Model { awaitingSelection = False }


type Msg
    = RequestSelection
    | ReceivedMessage IncomingMessage
    | RequestLogin


type alias TextSelection =
    { text : String
    , context : Maybe String
    }


update :
    { browserEnv : BrowserEnv
    , msg : Msg
    , model : Model
    , article : Article
    , loginStatus : LoginStatus
    , toModel : Model -> model
    , toMsg : Msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParent ( inner, effect ) =
            ( props.toModel inner, effect )
    in
    toParent <|
        case props.msg of
            RequestLogin ->
                ( Model model
                , Effect.sendSharedMsg Shared.Msg.TriggerLogin
                )

            RequestSelection ->
                case loggedInSigningPubKey props.loginStatus of
                    Just _ ->
                        ( Model { model | awaitingSelection = True }
                        , Effect.sendCmd Ports.requestTextSelection
                        )

                    Nothing ->
                        ( Model model
                        , Effect.sendSharedMsg Shared.Msg.TriggerLogin
                        )

            ReceivedMessage message ->
                case ( model.awaitingSelection, message.messageType ) of
                    ( True, "textSelection" ) ->
                        case Decode.decodeValue textSelectionDecoder message.value of
                            Ok selection ->
                                publishSelection props.browserEnv props.article props.loginStatus selection
                                    |> (\( effect, nextAwaiting ) ->
                                            ( Model { model | awaitingSelection = nextAwaiting }
                                            , effect
                                            )
                                       )

                            Err _ ->
                                ( Model { model | awaitingSelection = False }
                                , Effect.sendSharedMsg
                                    (Shared.Msg.ShowAlert
                                        (Translations.selectionEmptyAlert [ props.browserEnv.translations ])
                                    )
                                )

                    _ ->
                        ( Model model, Effect.none )


publishSelection : BrowserEnv -> Article -> LoginStatus -> TextSelection -> ( Effect msg, Bool )
publishSelection browserEnv article loginStatus selection =
    let
        trimmed =
            String.trim selection.text
    in
    if String.isEmpty trimmed then
        ( Effect.sendSharedMsg
            (Shared.Msg.ShowAlert
                (Translations.selectionEmptyAlert [ browserEnv.translations ])
            )
        , False
        )

    else
        case ( loggedInSigningPubKey loginStatus, addressComponentsForArticle article ) of
            ( Just userPubKey, Just addressComponents ) ->
                ( SendHighlight userPubKey
                    article.id
                    article.author
                    addressComponents
                    article.kind
                    trimmed
                    selection.context
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                , False
                )

            _ ->
                ( Effect.sendSharedMsg Shared.Msg.TriggerLogin
                , False
                )


textSelectionDecoder : Decode.Decoder TextSelection
textSelectionDecoder =
    Decode.succeed TextSelection
        |> DecodePipeline.required "text" Decode.string
        |> DecodePipeline.optional "context" (Decode.nullable Decode.string) Nothing


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    if model.awaitingSelection then
        Ports.receiveMessage ReceivedMessage

    else
        Sub.none


view :
    { browserEnv : BrowserEnv
    , model : Model
    , nostr : Nostr.Model
    , article : Article
    , loginStatus : LoginStatus
    , theme : Theme
    , toMsg : Msg -> msg
    }
    -> Html msg
view props =
    let
        styles =
            stylesForTheme props.theme

        highlights =
            addressComponentsForArticle props.article
                |> Maybe.map (Nostr.getHighlightsForAddress props.nostr)
                |> Maybe.withDefault []

        canHighlight =
            addressComponentsForArticle props.article /= Nothing
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            , Tw.w_full
            ]
        ]
        [ viewHeader styles props.browserEnv canHighlight props.loginStatus props.theme
        , viewList props.browserEnv styles props.nostr highlights
        ]
        |> Html.map props.toMsg


viewHeader : Styles Msg -> BrowserEnv -> Bool -> LoginStatus -> Theme -> Html Msg
viewHeader styles browserEnv canHighlight loginStatus theme =
    div
        [ css
            [ Tw.flex
            , Tw.flex_wrap
            , Tw.items_center
            , Tw.justify_between
            , Tw.gap_3
            , Tw.w_full
            ]
        ]
        [ h2
            (styles.colorStyleGrayscaleTitle
                ++ styles.textStyleH3
                ++ [ css [ Tw.m_0 ] ]
            )
            [ text (Translations.sectionTitle [ browserEnv.translations ]) ]
        , if canHighlight then
            Button.new
                { label = Translations.highlightSelectionButton [ browserEnv.translations ]
                , onClick =
                    Just
                        (case loggedInSigningPubKey loginStatus of
                            Just _ ->
                                RequestSelection

                            Nothing ->
                                RequestLogin
                        )
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.view

          else
            emptyHtml
        ]


viewList : BrowserEnv -> Styles Msg -> Nostr.Model -> List Highlight -> Html Msg
viewList browserEnv styles nostr highlights =
    case highlights of
        [] ->
            p
                (styles.colorStyleGrayscaleMuted
                    ++ styles.textStyleBody
                    ++ [ css [ Tw.m_0 ] ]
                )
                [ text (Translations.emptyText [ browserEnv.translations ]) ]

        _ ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_3
                    ]
                ]
                (List.map (viewHighlight browserEnv styles nostr) highlights)


viewHighlight : BrowserEnv -> Styles Msg -> Nostr.Model -> Highlight -> Html Msg
viewHighlight browserEnv styles nostr highlight =
    let
        profile =
            Nostr.getProfile nostr highlight.pubKey

        name =
            profile
                |> Maybe.map (\p -> profileDisplayName p.pubKey p)
                |> Maybe.withDefault (String.left 8 highlight.pubKey ++ "…")
    in
    div
        [ css
            [ Tw.flex
            , Tw.gap_3
            , Tw.p_3
            , Tw.rounded_lg
            ]
        ]
        [ viewProfileImageSmall browserEnv.environment (\children -> div [ css [ Tw.flex_none ] ] children) profile (Nostr.getProfileValidationStatus nostr highlight.pubKey |> Maybe.withDefault ValidationUnknown)
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_1
                , Tw.min_w_0
                , Tw.flex_1
                ]
            ]
            [ span
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody ++ [ css [ Tw.font_semibold ] ])
                [ text name ]
            , blockquote
                (styles.colorStyleGrayscaleText
                    ++ styles.textStyleBody
                    ++ [ css
                            [ Tw.m_0
                            , Tw.pl_3
                            , Tw.border_l_2
                            , Tw.border_solid
                            ]
                       ]
                )
                [ text highlight.content ]
            , span
                (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
                [ text (BrowserEnv.formatDate browserEnv highlight.createdAt) ]
            ]
        ]
