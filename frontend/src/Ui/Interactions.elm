module Ui.Interactions exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon exposing (Icon)
import Components.InteractionButton
import Components.Interactions
import Components.SharingButtonDialog as SharingButtonDialog
import FeatherIcons
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import I18Next
import Json.Encode as Encode
import Nostr
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (LoginStatus, PubKey, loggedInPubKey)
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


type alias Actions msg =
    { addBookmark : Maybe msg
    , removeBookmark : Maybe msg
    , addReaction : Maybe msg
    , removeReaction : Maybe msg
    , addRepost : Maybe msg
    , startComment : Maybe msg
    }


type alias PreviewData msg =
    { browserEnv : BrowserEnv
    , loginStatus : LoginStatus
    , maybeNip19Target : Maybe String
    , zapRelays : Set String
    , actions : Actions msg
    , interactionsModel : Components.Interactions.Model
    , interactionObject : Components.InteractionButton.InteractionObject
    , toInteractionsMsg : Components.Interactions.Msg msg -> msg
    , openCommentMsg : Maybe msg
    , nostr : Nostr.Model
    , sharing : Maybe ( SharingButtonDialog.Model, SharingButtonDialog.Msg -> msg )
    , sharingInfo : SharingButtonDialog.SharingInfo
    , translations : I18Next.Translations
    , theme : Theme
    }


viewInteractions : PreviewData msg -> String -> Html msg
viewInteractions previewData instanceId =
    Components.Interactions.new
        { browserEnv = previewData.browserEnv
        , model = Just previewData.interactionsModel
        , toMsg = previewData.toInteractionsMsg
        , theme = previewData.theme
        , interactionObject = previewData.interactionObject
        , nostr = previewData.nostr
        , loginStatus = previewData.loginStatus
        }
        |> Components.Interactions.withInteractionElements
            [ Components.Interactions.CommentButtonElement previewData.openCommentMsg
            , Components.Interactions.LikeButtonElement
            , Components.Interactions.RepostButtonElement
            , Components.Interactions.ZapButtonElement instanceId previewData.zapRelays
            , Components.Interactions.BookmarkButtonElement
            , Components.Interactions.ShareButtonElement previewData.sharingInfo
            ]
        |> Components.Interactions.view
{-
-}

viewReactions : Icon -> Maybe msg -> Maybe String -> PreviewData msg -> String -> Html msg
viewReactions icon maybeMsg maybeCount previewData instanceId =
    let
        onClickAttr =
            case maybeMsg of
                Just msg ->
                    [ Events.onClick msg, css [ Tw.cursor_pointer ] ]

                Nothing ->
                    []
    in
    div
        [ css
            [ Tw.rounded_3xl
            , Tw.justify_center
            , Tw.items_center
            , Tw.gap_1
            , Tw.flex
            ]
        ]
        [ if icon == Icon.FeatherIcon FeatherIcons.zap then
            zapButton (previewData.loginStatus |> loggedInPubKey) previewData.maybeNip19Target previewData.zapRelays instanceId

          else
            div
                (onClickAttr
                    ++ [ css
                            [ Tw.w_5
                            , Tw.h_5
                            , Tw.px_0_dot_5
                            , Tw.py_0_dot_5
                            , Tw.justify_center
                            , Tw.items_center
                            , Tw.flex
                            ]
                       ]
                )
                [ Icon.view icon ]
        , div
            []
            [ text (maybeCount |> Maybe.withDefault "0") ]
        ]


formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv milliSats =
    browserEnv.formatNumber "0 a" <| toFloat (milliSats // 1000)


zapButton : Maybe PubKey -> Maybe String -> Set String -> String -> Html msg
zapButton maybePubKey maybeNip19Target zapRelays instanceId =
    let
        maybeNip19TargetAttr =
            maybeNip19Target
                |> Maybe.map
                    (\nip19Target ->
                        if String.startsWith "note" nip19Target then
                            [ Attr.attribute "data-note-id" nip19Target ]

                        else
                            [ Attr.attribute "data-naddr" nip19Target ]
                    )

        maybeNpub =
            maybePubKey
            |> Maybe.andThen (\pubKey -> Nip19.encode (Npub pubKey) |> Result.toMaybe)

        ( nostrZapAttributes, zapComponent ) =
            maybeNpub
                |> Maybe.map
                    (\npub ->
                        ( [ Attr.id ("zap-button-" ++ instanceId)
                          , Attr.attribute "data-npub" npub
                          , Attr.attribute "data-relays" (zapRelays |> Set.toList |> String.join ",")
                          , Attr.attribute "data-button-color" "#334155"
                          ]
                            ++ Maybe.withDefault [] maybeNip19TargetAttr
                        , Html.node "js-zap-component"
                            [ Attr.property "buttonId" (Encode.string ("zap-button-" ++ instanceId)) ]
                            []
                        )
                    )
                |> Maybe.withDefault ( [], emptyHtml )
    in
    Html.button
        (nostrZapAttributes
            ++ [ css
                    [ Tw.w_5
                    , Tw.h_5
                    , Tw.px_0_dot_5
                    , Tw.py_0_dot_5
                    , Tw.justify_center
                    , Tw.items_center
                    , Tw.flex
                    ]
               ]
        )
        [ Icon.view (Icon.FeatherIcon FeatherIcons.zap), zapComponent ]



{-
   Extends the given relays with the inbox relays of the pub-key.
-}


extendedZapRelays : Set String -> Nostr.Model -> Maybe PubKey -> Set String
extendedZapRelays zapRelays nostrModel maybePubKey =
    let
        pubKeyRelays =
            maybePubKey
                |> Maybe.map (pubkeyRelays nostrModel)
                |> Maybe.withDefault Set.empty

        defaultRelays =
            Set.fromList nostrModel.defaultRelays |> Set.map websocketUrl

        candidateRelays =
            Set.union zapRelays pubKeyRelays
    in
    if Set.size candidateRelays == Set.size zapRelays || Set.size candidateRelays == Set.size pubKeyRelays then
        Set.union candidateRelays defaultRelays

    else
        candidateRelays


pubkeyRelays : Nostr.Model -> PubKey -> Set String
pubkeyRelays nostrModel pubKey =
    pubKey
        |> Nostr.getNip65RelaysForPubKey nostrModel
        |> List.map (\( _, relay ) -> websocketUrl relay.urlWithoutProtocol)
        |> Set.fromList
