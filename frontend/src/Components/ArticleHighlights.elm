module Components.ArticleHighlights exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

{-| Display article highlights (NIP-84) and publish a selection as kind 9802.

The author of a highlight can delete it with a NIP-09 deletion request.

-}

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, blockquote, div, h2, p, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.DeletionRequest exposing (deletionEvent)
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Highlights exposing (Highlight)
import Nostr.Profile exposing (ProfileValidation(..), profileDisplayName)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (EventId, IncomingMessage, LoginStatus, PubKey, loggedInSigningPubKey)
import I18Next
import Ports
import Process
import Shared.Msg
import Task
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Translations.ArticleHighlights as Translations
import Ui.Profile exposing (viewProfileImageSmall)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


type DeleteState
    = Deleting SendRequestId Highlight
    | Deleted Highlight


type HighlightPublish
    = HighlightIdle
    | HighlightSelecting
    | HighlightPublishing SendRequestId
    | HighlightPublished


type alias State =
    { highlightPublish : HighlightPublish
    , deletions : Dict EventId DeleteState
    , selection : Maybe TextSelection
    }


type Model
    = Model State


init : Model
init =
    Model
        { highlightPublish = HighlightIdle
        , deletions = Dict.empty
        , selection = Nothing
        }


type Msg
    = RequestSelection
    | ReceivedMessage IncomingMessage
    | RequestLogin
    | DeleteHighlight EventId
    | ClearHighlighted
    | KeepSelection


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
    , nostr : Nostr.Model
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
                requestSelection props model

            ClearHighlighted ->
                case model.highlightPublish of
                    HighlightPublished ->
                        ( Model { model | highlightPublish = HighlightIdle }, Effect.none )

                    _ ->
                        ( Model model, Effect.none )

            KeepSelection ->
                ( Model model, Effect.none )

            DeleteHighlight eventId ->
                deleteHighlight props model eventId

            ReceivedMessage message ->
                let
                    modelWithSelection =
                        updateSelection model message

                    ( modelAfterDeletion, deletionEffect ) =
                        applyDeletionFeedback props.browserEnv.translations modelWithSelection message

                    ( modelAfterHighlight, highlightEffect ) =
                        applyHighlightFeedback props.browserEnv.translations modelAfterDeletion message
                in
                let
                    delayEffect =
                        case ( model.highlightPublish, modelAfterHighlight.highlightPublish ) of
                            ( HighlightPublishing _, HighlightPublished ) ->
                                Process.sleep 3000
                                    |> Task.perform (\_ -> props.toMsg ClearHighlighted)
                                    |> Effect.sendCmd

                            _ ->
                                Effect.none
                in
                ( Model modelAfterHighlight, Effect.batch [ deletionEffect, highlightEffect, delayEffect ] )


deleteHighlight :
    { a
        | browserEnv : BrowserEnv
        , article : Article
        , loginStatus : LoginStatus
        , nostr : Nostr.Model
    }
    -> State
    -> EventId
    -> ( Model, Effect msg )
deleteHighlight props model eventId =
    case loggedInSigningPubKey props.loginStatus of
        Just userPubKey ->
            case highlightOwnedBy props.nostr props.article userPubKey eventId of
                Just highlight ->
                    ( Model
                        { model
                            | deletions =
                                Dict.insert eventId
                                    (Deleting (Nostr.getLastSendRequestId props.nostr) highlight)
                                    model.deletions
                        }
                    , deletionEvent userPubKey props.browserEnv.now eventId "Deleting highlight" Nothing [ KindHighlights ]
                        |> SendDeletionRequest (Nostr.getWriteRelayUrlsForPubKey props.nostr userPubKey)
                        |> Shared.Msg.SendNostrEvent
                        |> Effect.sendSharedMsg
                    )

                Nothing ->
                    ( Model model, Effect.none )

        Nothing ->
            ( Model model
            , Effect.sendSharedMsg Shared.Msg.TriggerLogin
            )


highlightOwnedBy : Nostr.Model -> Article -> PubKey -> EventId -> Maybe Highlight
highlightOwnedBy nostr article userPubKey eventId =
    addressComponentsForArticle article
        |> Maybe.map (Nostr.getHighlightsForAddress nostr)
        |> Maybe.withDefault []
        |> List.filter (\highlight -> highlight.id == eventId && highlight.pubKey == userPubKey)
        |> List.head


applyDeletionFeedback :
    I18Next.Translations
    -> State
    -> IncomingMessage
    -> ( State, Effect msg )
applyDeletionFeedback translations model message =
    case message.messageType of
        "published" ->
            case Nostr.External.decodeSendId message.value of
                Ok sendId ->
                    ( { model | deletions = markDeletionPublished sendId model.deletions }
                    , Effect.none
                    )

                Err _ ->
                    ( model, Effect.none )

        "error" ->
            case ( Nostr.External.decodeSendId message.value, Nostr.External.decodeReason message.value ) of
                ( Ok sendId, Ok reason ) ->
                    if deletionMatches sendId model.deletions then
                        ( { model | deletions = clearDeletion sendId model.deletions }
                        , Translations.deleteError [ translations ] { reason = reason }
                            |> Shared.Msg.ShowAlert
                            |> Effect.sendSharedMsg
                        )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


markDeletionPublished : SendRequestId -> Dict EventId DeleteState -> Dict EventId DeleteState
markDeletionPublished sendId deletions =
    Dict.map
        (\_ state ->
            case state of
                Deleting id highlight ->
                    if id == sendId then
                        Deleted highlight

                    else
                        state

                Deleted _ ->
                    state
        )
        deletions


clearDeletion : SendRequestId -> Dict EventId DeleteState -> Dict EventId DeleteState
clearDeletion sendId deletions =
    Dict.filter
        (\_ state ->
            case state of
                Deleting id _ ->
                    id /= sendId

                Deleted _ ->
                    True
        )
        deletions


deletionMatches : SendRequestId -> Dict EventId DeleteState -> Bool
deletionMatches sendId deletions =
    Dict.values deletions
        |> List.any
            (\state ->
                case state of
                    Deleting id _ ->
                        id == sendId

                    Deleted _ ->
                        False
            )


applyHighlightFeedback :
    I18Next.Translations
    -> State
    -> IncomingMessage
    -> ( State, Effect msg )
applyHighlightFeedback translations model message =
    case ( model.highlightPublish, message.messageType ) of
        ( HighlightPublishing sendId, "published" ) ->
            case Nostr.External.decodeSendId message.value of
                Ok incomingSendId ->
                    if incomingSendId == sendId then
                        ( { model | highlightPublish = HighlightPublished }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        ( HighlightPublishing sendId, "error" ) ->
            case ( Nostr.External.decodeSendId message.value, Nostr.External.decodeReason message.value ) of
                ( Ok incomingSendId, Ok reason ) ->
                    if incomingSendId == sendId then
                        ( { model | highlightPublish = HighlightIdle }
                        , Translations.highlightError [ translations ] { reason = reason }
                            |> Shared.Msg.ShowAlert
                            |> Effect.sendSharedMsg
                        )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


publishSelection : BrowserEnv -> Nostr.Model -> Article -> LoginStatus -> TextSelection -> ( Effect msg, HighlightPublish )
publishSelection browserEnv nostr article loginStatus selection =
    let
        trimmed =
            String.trim selection.text
    in
    if String.isEmpty trimmed then
        ( Effect.sendSharedMsg
            (Shared.Msg.ShowAlert
                (Translations.selectionEmptyAlert [ browserEnv.translations ])
            )
        , HighlightIdle
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
                , HighlightPublishing (Nostr.getLastSendRequestId nostr)
                )

            _ ->
                ( Effect.sendSharedMsg Shared.Msg.TriggerLogin
                , HighlightIdle
                )


textSelectionDecoder : Decode.Decoder TextSelection
textSelectionDecoder =
    Decode.succeed TextSelection
        |> DecodePipeline.required "text" Decode.string
        |> DecodePipeline.optional "context" (Decode.nullable Decode.string) Nothing


requestSelection :
    { a
        | browserEnv : BrowserEnv
        , article : Article
        , loginStatus : LoginStatus
        , nostr : Nostr.Model
    }
    -> State
    -> ( Model, Effect msg )
requestSelection props model =
    case ( loggedInSigningPubKey props.loginStatus, model.highlightPublish, model.selection ) of
        ( _, HighlightSelecting, _ ) ->
            ( Model model, Effect.none )

        ( _, HighlightPublishing _, _ ) ->
            ( Model model, Effect.none )

        ( _, HighlightPublished, _ ) ->
            ( Model model, Effect.none )

        ( Nothing, _, _ ) ->
            ( Model model
            , Effect.sendSharedMsg Shared.Msg.TriggerLogin
            )

        ( Just _, _, Nothing ) ->
            ( Model model, Effect.none )

        ( Just _, _, Just selection ) ->
            if selectionAlreadyHighlighted selection (activeHighlights props.nostr props.article model.deletions) then
                ( Model model, Effect.none )

            else
                publishSelection props.browserEnv props.nostr props.article props.loginStatus selection
                    |> (\( effect, highlightPublish ) ->
                            ( Model { model | highlightPublish = highlightPublish }
                            , effect
                            )
                       )


updateSelection : State -> IncomingMessage -> State
updateSelection model message =
    if message.messageType == "textSelection" then
        case Decode.decodeValue textSelectionDecoder message.value of
            Ok selection ->
                if String.isEmpty (String.trim selection.text) then
                    { model | selection = Nothing }

                else
                    { model | selection = Just selection }

            Err _ ->
                model

    else
        model


activeHighlights : Nostr.Model -> Article -> Dict EventId DeleteState -> List Highlight
activeHighlights nostr article deletions =
    let
        loaded =
            addressComponentsForArticle article
                |> Maybe.map (Nostr.getHighlightsForAddress nostr)
                |> Maybe.withDefault []

        deleting =
            deletions
                |> Dict.values
                |> List.filterMap
                    (\state ->
                        case state of
                            Deleting _ highlight ->
                                Just highlight

                            Deleted _ ->
                                Nothing
                    )
    in
    loaded ++ deleting


selectionAlreadyHighlighted : TextSelection -> List Highlight -> Bool
selectionAlreadyHighlighted selection highlights =
    let
        needle =
            String.trim selection.text
    in
    not (String.isEmpty needle)
        && List.any (\highlight -> String.trim highlight.content == needle) highlights


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage


highlightInProgress : HighlightPublish -> Bool
highlightInProgress highlightPublish =
    case highlightPublish of
        HighlightSelecting ->
            True

        HighlightPublishing _ ->
            True

        HighlightIdle ->
            False

        HighlightPublished ->
            False


deletionInProgress : Dict EventId DeleteState -> Bool
deletionInProgress deletions =
    Dict.values deletions
        |> List.any
            (\state ->
                case state of
                    Deleting _ _ ->
                        True

                    Deleted _ ->
                        False
            )


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
        (Model model) =
            props.model

        styles =
            stylesForTheme props.theme

        loaded =
            addressComponentsForArticle props.article
                |> Maybe.map (Nostr.getHighlightsForAddress props.nostr)
                |> Maybe.withDefault []

        highlights =
            retainedHighlights model.deletions loaded ++ loaded

        canHighlight =
            addressComponentsForArticle props.article /= Nothing

        alreadyHighlighted =
            model.selection
                |> Maybe.map (\selection -> selectionAlreadyHighlighted selection (activeHighlights props.nostr props.article model.deletions))
                |> Maybe.withDefault False
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            , Tw.w_full
            ]
        ]
        [ viewHeader styles props.browserEnv canHighlight props.loginStatus props.theme model.highlightPublish (model.selection /= Nothing) alreadyHighlighted
        , viewList props.browserEnv styles props.nostr props.loginStatus props.theme model.deletions highlights
        ]
        |> Html.map props.toMsg


viewHeader : Styles Msg -> BrowserEnv -> Bool -> LoginStatus -> Theme -> HighlightPublish -> Bool -> Bool -> Html Msg
viewHeader styles browserEnv canHighlight loginStatus theme highlightPublish hasSelection alreadyHighlighted =
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
            viewHighlightButton browserEnv loginStatus theme highlightPublish hasSelection alreadyHighlighted

          else
            emptyHtml
        ]


viewHighlightButton : BrowserEnv -> LoginStatus -> Theme -> HighlightPublish -> Bool -> Bool -> Html Msg
viewHighlightButton browserEnv loginStatus theme highlightPublish hasSelection alreadyHighlighted =
    let
        busy =
            highlightInProgress highlightPublish

        published =
            highlightPublish == HighlightPublished

        canSubmit =
            hasSelection && not alreadyHighlighted

        onClick =
            if published then
                Just RequestSelection

            else if busy || not canSubmit then
                Nothing

            else
                Just
                    (case loggedInSigningPubKey loginStatus of
                        Just _ ->
                            RequestSelection

                        Nothing ->
                            RequestLogin
                    )

        button =
            Button.new
                { label =
                    if busy then
                        Translations.highlightingSelectionButton [ browserEnv.translations ]

                    else if published then
                        Translations.highlightedButton [ browserEnv.translations ]

                    else
                        Translations.highlightSelectionButton [ browserEnv.translations ]
                , onClick = onClick
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.withIntermediateState busy
                |> Button.withDisabled (not canSubmit && not busy && not published)
        viewed =
            if published then
                button
                    |> Button.withStyleSuccess
                    |> Button.view

            else
                button
                    |> Button.view
    in
    div
        [ Events.preventDefaultOn "mousedown" (Decode.succeed ( KeepSelection, True )) ]
        [ viewed ]


retainedHighlights : Dict EventId DeleteState -> List Highlight -> List Highlight
retainedHighlights deletions loaded =
    let
        loadedIds =
            loaded
                |> List.map .id
                |> Set.fromList
    in
    deletions
        |> Dict.values
        |> List.filterMap highlightOfDeleteState
        |> List.filter (\highlight -> not (Set.member highlight.id loadedIds))


highlightOfDeleteState : DeleteState -> Maybe Highlight
highlightOfDeleteState state =
    case state of
        Deleting _ highlight ->
            Just highlight

        Deleted highlight ->
            Just highlight


viewList : BrowserEnv -> Styles Msg -> Nostr.Model -> LoginStatus -> Theme -> Dict EventId DeleteState -> List Highlight -> Html Msg
viewList browserEnv styles nostr loginStatus theme deletions highlights =
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
                (List.map (viewHighlight browserEnv styles nostr loginStatus theme deletions) highlights)


viewHighlight : BrowserEnv -> Styles Msg -> Nostr.Model -> LoginStatus -> Theme -> Dict EventId DeleteState -> Highlight -> Html Msg
viewHighlight browserEnv styles nostr loginStatus theme deletions highlight =
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
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_wrap
                    , Tw.items_center
                    , Tw.justify_between
                    , Tw.gap_2
                    , Tw.min_w_0
                    ]
                ]
                [ span
                    (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody ++ [ css [ Tw.font_semibold, Tw.min_w_0, Tw.truncate ] ])
                    [ text name ]
                , viewDeleteButton browserEnv theme loginStatus highlight (Dict.get highlight.id deletions)
                ]
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


viewDeleteButton : BrowserEnv -> Theme -> LoginStatus -> Highlight -> Maybe DeleteState -> Html Msg
viewDeleteButton browserEnv theme loginStatus highlight maybeState =
    if loggedInSigningPubKey loginStatus == Just highlight.pubKey then
        let
            buttonState =
                case maybeState of
                    Just (Deleting _ _) ->
                        { label = Translations.deletingButton [ browserEnv.translations ]
                        , onClick = Just (DeleteHighlight highlight.id)
                        , inProgress = True
                        , succeeded = False
                        }

                    Just (Deleted _) ->
                        { label = Translations.deletedButton [ browserEnv.translations ]
                        , onClick = Nothing
                        , inProgress = False
                        , succeeded = True
                        }

                    Nothing ->
                        { label = Translations.deleteButton [ browserEnv.translations ]
                        , onClick = Just (DeleteHighlight highlight.id)
                        , inProgress = False
                        , succeeded = False
                        }

            button =
                Button.new
                    { label = buttonState.label
                    , onClick = buttonState.onClick
                    , theme = theme
                    }
                    |> Button.withSizeSmall
                    |> Button.withIntermediateState buttonState.inProgress
        in
        div
            [ css [ Tw.shrink_0 ] ]
            [ (if buttonState.succeeded then
                button |> Button.withStyleSuccess

               else
                button |> Button.withStyleDanger
              )
                |> Button.view
            ]

    else
        emptyHtml
