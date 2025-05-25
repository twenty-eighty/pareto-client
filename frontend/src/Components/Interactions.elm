module Components.Interactions exposing
    ( Interactions, new, withRelayUrls
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
import Components.InteractionButton exposing (InteractionObject(..))
import Components.ZapButton as ZapButton
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import I18Next
import Nostr
import Nostr.Types exposing (LoginStatus)
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Ui.Styles


-- MODEL

type Model =
    Model
        { bookmarkButton : BookmarkButton.Model
        , commentButton : CommentButton.Model
        , likeButton : LikeButton.Model
        , repostButton : RepostButton.Model
        , zapButton : ZapButton.Model
        }

init : Model
init =
    Model
        { bookmarkButton = BookmarkButton.init
        , commentButton = CommentButton.init
        , likeButton = LikeButton.init
        , repostButton = RepostButton.init
        , zapButton = ZapButton.init
        }


-- UPDATE

type Msg
    = BookmarkButtonMsg BookmarkButton.Msg
    | CommentButtonMsg CommentButton.Msg
    | LikeButtonMsg LikeButton.Msg
    | RepostButtonMsg RepostButton.Msg
    | ZapButtonMsg ZapButton.Msg


update :
    { msg : Msg
    , model : Maybe Model
    , nostr : Nostr.Model
    , interactionObject : InteractionObject
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , translations : I18Next.Translations
    } -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model
            |> Maybe.withDefault init

        toParentModel : ( Model, Effect Msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect |> Effect.map props.toMsg
            )
    in
    toParentModel <|
        case props.msg of
            BookmarkButtonMsg bookmarkMsg ->
                BookmarkButton.update
                    { msg = bookmarkMsg
                    , model = model.bookmarkButton
                    , toModel = \innerModel -> Model { model | bookmarkButton = innerModel }
                    , nostr = props.nostr
                    , toMsg = BookmarkButtonMsg
                    , translations = props.translations
                    }

            CommentButtonMsg commentMsg ->
                CommentButton.update
                    { msg = commentMsg
                    , model = model.commentButton
                    , toModel = \innerModel -> Model { model | commentButton = innerModel }
                    , nostr = props.nostr
                    , toMsg = CommentButtonMsg
                    , translations = props.translations
                    }

            LikeButtonMsg likeMsg ->
                LikeButton.update
                    { msg = likeMsg
                    , model = model.likeButton
                    , toModel = \innerModel -> Model { model | likeButton = innerModel }
                    , nostr = props.nostr
                    , toMsg = LikeButtonMsg
                    , translations = props.translations
                    }

            RepostButtonMsg repostMsg ->
                RepostButton.update
                    { msg = repostMsg
                    , model = model.repostButton
                    , toModel = \innerModel -> Model { model | repostButton = innerModel }
                    , nostr = props.nostr
                    , toMsg = RepostButtonMsg
                    , translations = props.translations
                    }

            ZapButtonMsg zapMsg ->
                ZapButton.update
                    { msg = zapMsg
                    , model = model.zapButton
                    , toModel = \innerModel -> Model { model | zapButton = innerModel }
                    , nostr = props.nostr
                    , toMsg = ZapButtonMsg
                    , translations = props.translations
                    }



-- SETTINGS


type Interactions msg
    = Settings
        { browserEnv : BrowserEnv
        , model : Maybe Model
        , instanceId : String
        , interactionObject : InteractionObject
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , relayUrls : Set String
        , toMsg : Msg -> msg
        , theme : Ui.Styles.Theme
        }


new : 
    { browserEnv : BrowserEnv
    , model : Maybe Model
    , toMsg : Msg -> msg
    , theme : Ui.Styles.Theme
    , interactionObject : InteractionObject
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    } -> Interactions msg
new props =
    Settings
        { browserEnv = props.browserEnv
        , model = props.model
        , instanceId = "0"
        , toMsg = props.toMsg
        , interactionObject = props.interactionObject
        , loginStatus = props.loginStatus
        , nostr = props.nostr
        , relayUrls = Set.empty
        , theme = props.theme
        }


withRelayUrls : Set String -> Interactions msg -> Interactions msg
withRelayUrls relayUrls (Settings settings) =
    Settings { settings | relayUrls = relayUrls }


-- HELPERS

getBookmarkButton : Interactions msg -> Html Msg
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


getCommentButton : Interactions msg -> Html Msg
getCommentButton (Settings settings) =
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
        |> CommentButton.view


getLikeButton : Interactions msg -> Html Msg
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


getRepostButton : Interactions msg -> Html Msg
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


getZapButton : Interactions msg -> Html Msg
getZapButton (Settings settings) =
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
        |> ZapButton.withInstanceId settings.instanceId
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
        [ getBookmarkButton interactions
        , getCommentButton interactions
        , getLikeButton interactions
        , getRepostButton interactions
        , getZapButton interactions
        ] 
        |> Html.map settings.toMsg


subscriptions : Model -> Sub Msg
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