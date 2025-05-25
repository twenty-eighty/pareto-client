module Components.Interactions exposing
    ( Interactions, InteractionElement(..)
    , new, withRelayUrls, withInteractionElements
    , view
    , init, update, Model, Msg
    , subscriptions
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
import Components.LikeButton as LikeButton
import Components.RepostButton as RepostButton
import Components.SharingButtonDialog as SharingButtonDialog
import Components.InteractionButton exposing (InteractionObject(..))
import Components.ZapButton as ZapButton
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Nostr
import Nostr.Types exposing (LoginStatus)
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Ui.Styles

type InteractionElement msg
    = BookmarkButtonElement
    | CommentButtonElement (Maybe msg)
    | LikeButtonElement
    | RepostButtonElement
    | ShareButtonElement SharingButtonDialog.SharingInfo
    | ZapButtonElement String

-- MODEL

type Model =
    Model
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
    | OpenComment msg
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
    } -> ( model, Effect msg )
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
                    (updatedModel, effect) =
                        BookmarkButton.update
                            { msg = bookmarkMsg
                            , model = model.bookmarkButton
                            , toModel = \innerModel -> Model { model | bookmarkButton = innerModel }
                            , nostr = props.nostr
                            , toMsg = BookmarkButtonMsg
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )

            CommentButtonMsg commentMsg ->
                let
                    (updatedModel, effect) =
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

            OpenComment openCommentMsg ->
                ( Model model
                , openCommentMsg
                    |> Effect.sendMsg
                )

            LikeButtonMsg likeMsg ->
                let
                    (updatedModel, effect) =
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
                    (updatedModel, effect) =
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
                    (updatedModel, effect) =
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
                    (updatedModel, effect) =
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
        , relayUrls : Set String
        , toMsg : Msg msg -> msg
        , theme : Ui.Styles.Theme
        }


new : 
    { browserEnv : BrowserEnv
    , model : Maybe Model
    , toMsg : Msg msg -> msg
    , theme : Ui.Styles.Theme
    , interactionObject : InteractionObject
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    } -> Interactions msg
new props =
    Settings
        { browserEnv = props.browserEnv
        , interactionElements = [ BookmarkButtonElement, LikeButtonElement, RepostButtonElement, ZapButtonElement "0" ]
        , model = props.model
        , toMsg = props.toMsg
        , interactionObject = props.interactionObject
        , loginStatus = props.loginStatus
        , nostr = props.nostr
        , relayUrls = Set.empty
        , theme = props.theme
        }

withInteractionElements : List (InteractionElement msg) -> Interactions msg -> Interactions msg
withInteractionElements interactionElements (Settings settings) =
    Settings { settings | interactionElements = interactionElements }

withRelayUrls : Set String -> Interactions msg -> Interactions msg
withRelayUrls relayUrls (Settings settings) =
    Settings { settings | relayUrls = relayUrls }


-- HELPERS

getBookmarkButton : Interactions msg -> Html (Msg msg)
getBookmarkButton (Settings settings) =
    let
        (Model model) =
            settings.model
            |> Maybe.withDefault init
    in
    BookmarkButton.new
        { model = model.bookmarkButton
        , toMsg = BookmarkButtonMsg
        , theme = settings.theme
        , interactionObject = settings.interactionObject
        , nostr = settings.nostr
        , loginStatus = settings.loginStatus
        }
        |> BookmarkButton.view


getCommentButton : Interactions msg -> Maybe msg -> Html (Msg msg)
getCommentButton (Settings settings) openCommentMsg =
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
        |> CommentButton.withOpenCommentMsg (openCommentMsg |> Maybe.map OpenComment)
        |> CommentButton.view


getLikeButton : Interactions msg -> Html (Msg msg)
getLikeButton (Settings settings) =
    let
        (Model model) =
            settings.model
            |> Maybe.withDefault init
    in
    LikeButton.new
        {interactionObject = settings.interactionObject
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

getZapButton : Interactions msg -> String -> Html (Msg msg)
getZapButton (Settings settings) instanceId =
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
        , relayUrls = settings.relayUrls
        }
        |> ZapButton.withRelayUrls settings.relayUrls
        |> ZapButton.withInstanceId instanceId
        |> ZapButton.view


-- VIEW


view : Interactions msg -> Html msg
view interactions =
    let
        (Settings settings) =
            interactions
    in
    div
        [ Attr.css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_4
            , Tw.items_center
            ]
        ]
        ( settings.interactionElements
            |> List.map (\interactionElement ->
                case interactionElement of
                    BookmarkButtonElement ->
                        getBookmarkButton interactions
                    CommentButtonElement openCommentMsg ->
                        getCommentButton interactions openCommentMsg
                    LikeButtonElement ->
                        getLikeButton interactions
                    RepostButtonElement ->
                        getRepostButton interactions
                    ShareButtonElement sharingInfo ->
                        getShareButton interactions sharingInfo
                    ZapButtonElement instanceId ->
                        getZapButton interactions instanceId
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