module Pages.A.Addr_ exposing (..)

import Browser.Dom
import BrowserEnv exposing (BrowserEnv)
import Components.RelayStatus exposing (Purpose(..))
import Effect exposing (Effect)
import Layouts
import Nostr
import Nostr.Event as Event
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Msg
import Shared.Model
import Task
import Translations.ArticlePage as Translations
import Ui.Styles exposing (Theme)
import Ui.View exposing (viewRelayStatus)
import Url
import View exposing (View)


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


type alias Model =
    { nip19 : Maybe NIP19Type
    , requestId : Maybe RequestId
    }

-- ["REQ"," 30023-#d,autho-538",{"kinds":[30023],"#d":["1707912490439"],"authors":["ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600"]}]
-- ["REQ","uthors-ids-ki-plocc",{"authors":["ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600"],"ids":["1707912490439"],"kinds":[30023],"limit":20}]

init : Shared.Model -> Route { addr : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.addr

        (maybeNip19, maybeArticle) =
            case decoded of
                Ok nip19 ->
                    (Just nip19, Nostr.getArticleForNip19 shared.nostr nip19)

                Err _ ->
                    (Nothing, Nothing)

        (effect, requestId) =
            case (maybeArticle, maybeNip19) of
                (Nothing, Just (NAddr naddrData)) ->
                   ( Event.eventFilterForNaddr naddrData
                        |> RequestArticle (if naddrData.relays /= [] then Just naddrData.relays else Nothing)
                        |> Nostr.createRequest shared.nostr "Article described as NIP-19" [KindUserMetadata]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                (_, _) ->
                    ( Effect.none, Nothing)
                
    in
    ( { nip19 = maybeNip19
      , requestId = requestId
      }
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
    | NoOp

update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd <| Ports.requestUser)

        NoOp ->
            ( model, Effect.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        maybeArticle =
            model.nip19
            |> Maybe.andThen (Nostr.getArticleForNip19 shared.nostr)
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
                    , onBookmark = Nothing
                    }
                )
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId)

        ]
    }