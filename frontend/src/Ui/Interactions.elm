module Ui.Interactions exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Icon as Icon exposing (Icon)
import Components.SharingButtonDialog as SharingButtonDialog
import Dict
import FeatherIcons
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import I18Next
import Json.Encode as Encode
import Nostr
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Reactions exposing (Interactions)
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (PubKey)
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme)


type alias Actions msg =
    { addBookmark : Maybe msg
    , removeBookmark : Maybe msg
    , addReaction : Maybe msg
    , removeReaction : Maybe msg
    , addRepost : Maybe msg
    , startComment : Maybe msg
    }


type alias PreviewData msg =
    { pubKey : PubKey
    , maybeNip19Target : Maybe String
    , zapRelays : Set String
    , actions : Actions msg
    , interactions : Interactions
    , sharing : Maybe ( SharingButtonDialog.Model, SharingButtonDialog.Msg -> msg )
    , sharingInfo : SharingButtonDialog.SharingInfo
    , translations : I18Next.Translations
    , theme : Theme
    }


viewInteractions : Styles msg -> BrowserEnv -> PreviewData msg -> String -> Html msg
viewInteractions styles browserEnv previewData instanceId =
    let
        actions =
            previewData.actions

        interactions =
            previewData.interactions

        ( bookmarkIcon, bookmarkMsg ) =
            if interactions.isBookmarked then
                ( Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit, actions.removeBookmark )

            else
                ( Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit, actions.addBookmark )

        ( reactionIcon, reactionMsg ) =
            case interactions.reaction of
                Just _ ->
                    ( Icon.MaterialIcon Icon.MaterialFavorite 30 (Icon.Color (Color.fromRgba { red = 1.0, green = 0.0, blue = 0.0, alpha = 1.0 })), actions.removeReaction )

                Nothing ->
                    ( Icon.MaterialIcon Icon.MaterialFavoriteBorder 30 Icon.Inherit, actions.addReaction )

        ( repostIcon, repostMsg ) =
            case interactions.repost of
                Just _ ->
                    ( Icon.MaterialIcon Icon.MaterialRepeatOn 30 Icon.Inherit
                      -- disable reposting if done already by user
                    , Nothing
                    )

                Nothing ->
                    ( Icon.MaterialIcon Icon.MaterialRepeat 30 Icon.Inherit
                    , actions.addRepost
                    )

        commentsCount =
            List.length interactions.articleComments + Dict.size interactions.articleCommentComments
    in
    div
        (css
            [ Tw.justify_start
            , Tw.items_center
            , Tw.gap_6
            , Tw.inline_flex
            ]
            :: styles.colorStyleGrayscaleText
        )
        [ viewReactions (Icon.FeatherIcon FeatherIcons.messageSquare) actions.startComment (Just <| String.fromInt commentsCount) previewData instanceId
        , viewReactions reactionIcon reactionMsg (Maybe.map String.fromInt interactions.reactions) previewData instanceId
        , viewReactions repostIcon repostMsg (Maybe.map String.fromInt interactions.reposts) previewData instanceId
        , viewReactions (Icon.FeatherIcon FeatherIcons.zap) Nothing (Maybe.map (formatZapNum browserEnv) interactions.zaps) previewData instanceId
        , viewReactions bookmarkIcon bookmarkMsg (Maybe.map String.fromInt interactions.bookmarks) previewData instanceId
        , previewData.sharing
            |> Maybe.map
                (\( sharingButtonDialog, sharingButtonDialogMsg ) ->
                    SharingButtonDialog.new
                        { model = sharingButtonDialog
                        , browserEnv = browserEnv
                        , sharingInfo = previewData.sharingInfo
                        , translations = previewData.translations
                        , theme = previewData.theme
                        , toMsg = sharingButtonDialogMsg
                        }
                        |> SharingButtonDialog.view
                )
            |> Maybe.withDefault emptyHtml
        ]


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
            zapButton previewData.pubKey previewData.maybeNip19Target previewData.zapRelays instanceId

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


zapButton : PubKey -> Maybe String -> Set String -> String -> Html msg
zapButton pubKey maybeNip19Target zapRelays instanceId =
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
            Nip19.encode (Npub pubKey) |> Result.toMaybe

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
