module Ui.Interactions exposing (PreviewData, extendedZapRelays, formatZapNum, pubkeyRelays, viewInteractions, viewReactions)

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon exposing (Icon)
import Components.InteractionButton
import Components.Interactions
import Components.SharingButtonDialog as SharingButtonDialog
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import I18Next
import Nostr
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (LoginStatus, PubKey, loggedInPubKey)
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Theme)


type alias PreviewData msg =
    { browserEnv : BrowserEnv
    , loginStatus : LoginStatus
    , maybeNip19Target : Maybe String
    , zapRelays : Set String
    , interactionsModel : Components.Interactions.Model
    , interactionObject : Components.InteractionButton.InteractionObject
    , toInteractionsMsg : Components.Interactions.Msg msg -> msg
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
        , showLabel = True
        }
        |> Components.Interactions.withInteractionElements
            [ Components.Interactions.CommentButtonElement Nothing
            , Components.Interactions.LikeButtonElement
            , Components.Interactions.RepostButtonElement
            , Components.Interactions.ZapButtonElement instanceId previewData.zapRelays
            , Components.Interactions.BookmarkButtonElement
            , Components.Interactions.ShareButtonElement previewData.sharingInfo
            ]
        |> Components.Interactions.view


viewReactions : Icon -> Maybe msg -> Maybe String -> PreviewData msg -> String -> Html msg
viewReactions icon maybeMsg maybeCount _ _ =
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
        [ div
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


extendedZapRelays : Set String -> Nostr.Model -> LoginStatus -> Set String
extendedZapRelays zapRelays nostrModel loginStatus =
    let
        pubKeyRelays =
            loginStatus
                |> loggedInPubKey
                |> Maybe.map (pubkeyRelays nostrModel)
                |> Maybe.withDefault Set.empty

        defaultRelays =
            Set.fromList nostrModel.defaultRelays
                |> Set.map websocketUrl

        candidateRelays =
            Set.union zapRelays pubKeyRelays
                |> Set.map websocketUrl
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
