module Pages.A.Addr_ exposing (..)

import Browser.Dom
import BrowserEnv exposing (BrowserEnv)
import Components.RelayStatus exposing (Purpose(..))
import Components.ZapDialog as ZapDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Layouts
import Nostr
import Nostr.Event as Event
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, PubKey)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Msg
import Shared.Model
import Tailwind.Utilities as Tw
import Task
import Translations.ArticlePage as Translations
import Ui.Styles exposing (Theme)
import Ui.View exposing (viewRelayStatus)
import Url
import View exposing (View)
import Html.Styled exposing (div)
import Ui.Styles exposing (stylesForTheme)
import Nostr.Article exposing (addressComponentsForArticle)


page : Shared.Model -> Route { addr : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


-- INIT


type Model
    = Nip19Model Nip19ModelData
    | ErrorModel String


type alias Nip19ModelData =
    { nip19 : NIP19Type
    , requestId : RequestId
    , zapDialog : ZapDialog.Model Msg
    }

init : Shared.Model -> Route { addr : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.addr

        (model, maybeArticle) =
            case decoded of
                Ok nip19 ->
                    ( Nip19Model
                        { nip19 = nip19
                        , requestId = Nostr.getLastRequestId shared.nostr
                        , zapDialog = ZapDialog.init {}
                        }
                    , Nostr.getArticleForNip19 shared.nostr nip19
                    )

                Err error ->
                    ( ErrorModel error, Nothing )
        effect =
            case (maybeArticle, model) of
                ( Nothing, Nip19Model { nip19, requestId }) ->
                    -- article not loaded yet, request it now
                    case nip19 of
                        NAddr naddrData ->
                            Event.eventFilterForNaddr naddrData
                                |> RequestArticle (if naddrData.relays /= [] then Just naddrData.relays else Nothing)
                                |> Nostr.createRequest shared.nostr "Article described as NIP-19" [KindUserMetadata]
                                |> Shared.Msg.RequestNostrEvents
                                |> Effect.sendSharedMsg
                    
                        _ ->
                            Effect.none

                (_, _) ->
                    Effect.none
                
    in
    ( model
    , Effect.batch
       [ effect
       -- jump to top of article
       , Effect.sendCmd <| Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 0)
       ]
    )

decodedTagParam : String -> Maybe (List String)
decodedTagParam tag =
    Url.percentDecode tag
    |> Maybe.map (List.singleton)

-- UPDATE


type Msg
    = OpenGetStarted
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | ZapReaction PubKey (List ZapDialog.Recipient)
    | ZapDialogSent (ZapDialog.Msg Msg)
    | NoOp

update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd <| Ports.requestUser)

        AddArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithoutArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddArticleReaction userPubKey eventId articlePubKey addressComponents ->
            ( model
            , SendReaction userPubKey eventId articlePubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        ZapReaction userPubKey recipients ->
            case model of
                Nip19Model nip19ModelData ->
                    showZapDialog nip19ModelData recipients

                _ ->
                    ( model, Effect.none )

        ZapDialogSent innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    ZapDialog.update
                        { nostr = shared.nostr
                        , msg = innerMsg
                        , model = nip19ModelData.zapDialog
                        , toModel = \zapDialog -> Nip19Model { nip19ModelData | zapDialog = zapDialog}
                        , toMsg = ZapDialogSent
                        }

                _ ->
                    ( model, Effect.none )

        NoOp ->
            ( model, Effect.none )

showZapDialog : Nip19ModelData -> List ZapDialog.Recipient -> (Model, Effect Msg)
showZapDialog nip19ModelData recipients =
    let
        (zapDialogModel, effect) =
            ZapDialog.show nip19ModelData.zapDialog ZapDialogSent recipients
    in
    ( Nip19Model { nip19ModelData | zapDialog = zapDialogModel }
    , effect
    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    case model of
        Nip19Model { nip19, requestId } ->
            viewContent shared nip19 requestId

        ErrorModel error ->
            viewError shared error

viewContent : Shared.Model -> NIP19Type -> RequestId -> View Msg
viewContent shared nip19 requestId =
    let
        maybeArticle =
            Nostr.getArticleForNip19 shared.nostr nip19

        addressComponents =
            maybeArticle
            |> Maybe.andThen addressComponentsForArticle

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    { title =
        maybeArticle
        |> Maybe.andThen .title
        |> Maybe.withDefault (Translations.defaultPageTitle [ shared.browserEnv.translations ])
    , body =
        [ maybeArticle 
            |> Maybe.map (
                Ui.View.viewArticle
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , userPubKey = Shared.loggedInPubKey shared.loginStatus
                    , onBookmark = Maybe.map (\pubKey -> (AddArticleBookmark pubKey, RemoveArticleBookmark pubKey)) userPubKey
                    , onReaction = Maybe.map (\pubKey -> AddArticleReaction pubKey) userPubKey
                    , onZap = Maybe.map (\pubKey -> ZapReaction pubKey) userPubKey
                    }
                )
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle (Just requestId))
        ]
    }

viewError : Shared.Model -> String -> View Msg
viewError shared error =
    let
        styles =
            stylesForTheme shared.theme
    in
    { title = Translations.defaultPageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3 ++
            [ css
                [ Tw.m_4
                ]
            ])
            [ Html.text <| "Error loading content: " ++ error
            ]
        ]
    }