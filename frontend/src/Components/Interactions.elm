module Components.Interactions exposing
    ( Interactions, new
    , view
    , init, update, Model, Msg
    , InteractionElement(..), subscriptions, withInteractionElements, withoutIgnoringDeviceSize
    )

{-|


## Basic usage

@docs Interactions, new
@docs view


## State management

@docs init, update, Model, Msg

-}

import BrowserEnv exposing (BrowserEnv)
import Components.BookmarkButton as BookmarkButton
import Components.CommentButton as CommentButton
import Components.InteractionButton exposing (InteractionObject(..))
import Components.LikeButton as LikeButton
import Components.RepostButton as RepostButton
import Components.SharingButtonDialog as SharingButtonDialog
import Components.ZapButton as ZapButton
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Nostr
import Nostr.Types exposing (LoginStatus, RelayUrl)
import Set exposing (Set)
import Tailwind.Breakpoints as Bp exposing (..)
import Tailwind.Utilities as Tw
import Ui.Styles


type InteractionElement msg
    = BookmarkButtonElement
    | CommentButtonElement (Maybe msg)
    | LikeButtonElement
    | RepostButtonElement
    | ShareButtonElement SharingButtonDialog.SharingInfo
    | ZapButtonElement String (Set RelayUrl)



-- MODEL


type Model
    = Model
        { bookmarkButton : BookmarkButton.Model
        , commentButton : CommentButton.Model
        , likeButton : LikeButton.Model
        , repostButton : RepostButton.Model
        , sharingButtonDialog : SharingButtonDialog.Model
        , zapButton : ZapButton.Model
        }


init : Model
init =
    Model
        { bookmarkButton = BookmarkButton.init
        , commentButton = CommentButton.init
        , likeButton = LikeButton.init
        , repostButton = RepostButton.init
        , sharingButtonDialog = SharingButtonDialog.init
        , zapButton = ZapButton.init
        }



-- UPDATE


type Msg msg
    = BookmarkButtonMsg BookmarkButton.Msg
    | CommentButtonMsg (CommentButton.Msg (Msg msg))
    | CommentButtonClicked msg
    | LikeButtonMsg LikeButton.Msg
    | RepostButtonMsg RepostButton.Msg
    | SharingButtonDialogMsg SharingButtonDialog.Msg
    | ZapButtonMsg ZapButton.Msg


update :
    { browserEnv : BrowserEnv
    , msg : Msg msg
    , model : Maybe Model
    , nostr : Nostr.Model
    , interactionObject : InteractionObject
    , openCommentMsg : Maybe msg
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model
                |> Maybe.withDefault init

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            BookmarkButtonMsg bookmarkMsg ->
                let
                    ( updatedModel, effect ) =
                        BookmarkButton.update
                            { msg = bookmarkMsg
                            , model = Just model.bookmarkButton
                            , toModel = \innerModel -> Model { model | bookmarkButton = innerModel }
                            , nostr = props.nostr
                            , toMsg = BookmarkButtonMsg
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )

            CommentButtonMsg commentMsg ->
                let
                    ( updatedModel, effect ) =
                        CommentButton.update
                            { msg = commentMsg
                            , model = model.commentButton
                            , toModel = \innerModel -> Model { model | commentButton = innerModel }
                            , nostr = props.nostr
                            , toMsg = CommentButtonMsg
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )

            CommentButtonClicked commentButtonClickedMsg ->
                ( Model model
                , commentButtonClickedMsg
                    |> Effect.sendMsg
                )

            LikeButtonMsg likeMsg ->
                let
                    ( updatedModel, effect ) =
                        LikeButton.update
                            { msg = likeMsg
                            , model = model.likeButton
                            , toModel = \innerModel -> Model { model | likeButton = innerModel }
                            , nostr = props.nostr
                            , toMsg = LikeButtonMsg
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )

            RepostButtonMsg repostMsg ->
                let
                    ( updatedModel, effect ) =
                        RepostButton.update
                            { msg = repostMsg
                            , model = model.repostButton
                            , toModel = \innerModel -> Model { model | repostButton = innerModel }
                            , nostr = props.nostr
                            , toMsg = RepostButtonMsg
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )

            SharingButtonDialogMsg sharingButtonDialogMsg ->
                let
                    ( updatedModel, effect ) =
                        SharingButtonDialog.update
                            { msg = sharingButtonDialogMsg
                            , model = model.sharingButtonDialog
                            , toModel = \innerModel -> Model { model | sharingButtonDialog = innerModel }
                            , browserEnv = props.browserEnv
                            , toMsg = SharingButtonDialogMsg
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )

            ZapButtonMsg zapMsg ->
                let
                    ( updatedModel, effect ) =
                        ZapButton.update
                            { msg = zapMsg
                            , model = model.zapButton
                            , toModel = \innerModel -> Model { model | zapButton = innerModel }
                            , nostr = props.nostr
                            , toMsg = ZapButtonMsg
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )



-- SETTINGS


type Interactions msg
    = Settings
        { browserEnv : BrowserEnv
        , interactionElements : List (InteractionElement msg)
        , model : Maybe Model
        , interactionObject : InteractionObject
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , toMsg : Msg msg -> msg
        , theme : Ui.Styles.Theme
        , ignoreDeviceSize : Bool
        }


new :
    { browserEnv : BrowserEnv
    , model : Maybe Model
    , toMsg : Msg msg -> msg
    , theme : Ui.Styles.Theme
    , interactionObject : InteractionObject
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    }
    -> Interactions msg
new props =
    Settings
        { browserEnv = props.browserEnv
        , interactionElements = [ BookmarkButtonElement, LikeButtonElement, RepostButtonElement, ZapButtonElement "0" Set.empty ]
        , model = props.model
        , toMsg = props.toMsg
        , interactionObject = props.interactionObject
        , loginStatus = props.loginStatus
        , nostr = props.nostr
        , theme = props.theme
        , ignoreDeviceSize = True
        }


withoutIgnoringDeviceSize : Interactions msg -> Interactions msg
withoutIgnoringDeviceSize (Settings settings) =
    Settings { settings | ignoreDeviceSize = False }


withInteractionElements : List (InteractionElement msg) -> Interactions msg -> Interactions msg
withInteractionElements interactionElements (Settings settings) =
    Settings { settings | interactionElements = interactionElements }



-- HELPERS


getBookmarkButton : Interactions msg -> Html (Msg msg)
getBookmarkButton (Settings settings) =
    let
        (Model model) =
            settings.model
                |> Maybe.withDefault init
    in
    BookmarkButton.new
        { model = Just model.bookmarkButton
        , toMsg = BookmarkButtonMsg
        , theme = settings.theme
        , interactionObject = settings.interactionObject
        , nostr = settings.nostr
        , loginStatus = settings.loginStatus
        }
        |> BookmarkButton.view


getCommentButton : Interactions msg -> Maybe msg -> Html (Msg msg)
getCommentButton (Settings settings) clickedMsg =
    let
        (Model model) =
            settings.model
                |> Maybe.withDefault init
    in
    CommentButton.new
        { model = model.commentButton
        , toMsg = CommentButtonMsg
        , theme = settings.theme
        , interactionObject = settings.interactionObject
        , nostr = settings.nostr
        , loginStatus = settings.loginStatus
        }
        |> CommentButton.withClickedMsg (clickedMsg |> Maybe.map CommentButtonClicked)
        |> CommentButton.view


getLikeButton : Interactions msg -> Html (Msg msg)
getLikeButton (Settings settings) =
    let
        (Model model) =
            settings.model
                |> Maybe.withDefault init
    in
    LikeButton.new
        { interactionObject = settings.interactionObject
        , loginStatus = settings.loginStatus
        , model = model.likeButton
        , nostr = settings.nostr
        , theme = settings.theme
        , toMsg = LikeButtonMsg
        }
        |> LikeButton.view


getRepostButton : Interactions msg -> Html (Msg msg)
getRepostButton (Settings settings) =
    let
        (Model model) =
            settings.model
                |> Maybe.withDefault init
    in
    RepostButton.new
        { model = model.repostButton
        , toMsg = RepostButtonMsg
        , theme = settings.theme
        , interactionObject = settings.interactionObject
        , nostr = settings.nostr
        , loginStatus = settings.loginStatus
        }
        |> RepostButton.view


getShareButton : Interactions msg -> SharingButtonDialog.SharingInfo -> Html (Msg msg)
getShareButton (Settings settings) sharingInfo =
    let
        (Model model) =
            settings.model
                |> Maybe.withDefault init
    in
    SharingButtonDialog.new
        { browserEnv = settings.browserEnv
        , model = model.sharingButtonDialog
        , toMsg = SharingButtonDialogMsg
        , theme = settings.theme
        , sharingInfo = sharingInfo
        }
        |> SharingButtonDialog.view


getZapButton : Interactions msg -> String -> Set RelayUrl -> Html (Msg msg)
getZapButton (Settings settings) instanceId relayUrls =
    let
        (Model model) =
            settings.model
                |> Maybe.withDefault init
    in
    ZapButton.new
        { browserEnv = settings.browserEnv
        , model = model.zapButton
        , toMsg = ZapButtonMsg
        , theme = settings.theme
        , interactionObject = settings.interactionObject
        , nostr = settings.nostr
        , loginStatus = settings.loginStatus
        }
        |> ZapButton.withRelayUrls relayUrls
        |> ZapButton.withInstanceId instanceId
        |> ZapButton.view



-- VIEW


view : Interactions msg -> Html msg
view interactions =
    let
        (Settings settings) =
            interactions

        flexDirections =
            if settings.ignoreDeviceSize then
                [ Tw.flex_row ]

            else
                [ Bp.lg [ Tw.flex_row ], Tw.flex_col ]
    in
    div
        [ Attr.css
            ([ Tw.flex
             , Tw.flex_wrap
             , Tw.gap_x_4
             , Tw.items_center
             ]
                ++ flexDirections
            )
        ]
        (settings.interactionElements
            |> List.map
                (\interactionElement ->
                    case interactionElement of
                        BookmarkButtonElement ->
                            getBookmarkButton interactions

                        CommentButtonElement clickedMsg ->
                            getCommentButton interactions clickedMsg

                        LikeButtonElement ->
                            getLikeButton interactions

                        RepostButtonElement ->
                            getRepostButton interactions

                        ShareButtonElement sharingInfo ->
                            getShareButton interactions sharingInfo

                        ZapButtonElement instanceId relayUrls ->
                            getZapButton interactions instanceId relayUrls
                )
        )
        |> Html.map settings.toMsg


subscriptions : Model -> Sub (Msg msg)
subscriptions (Model model) =
    Sub.batch
        [ BookmarkButton.subscriptions model.bookmarkButton
            |> Sub.map BookmarkButtonMsg
        , CommentButton.subscriptions model.commentButton
            |> Sub.map CommentButtonMsg
        , LikeButton.subscriptions model.likeButton
            |> Sub.map LikeButtonMsg
        , RepostButton.subscriptions model.repostButton
            |> Sub.map RepostButtonMsg
        , ZapButton.subscriptions model.zapButton
            |> Sub.map ZapButtonMsg
        ]
