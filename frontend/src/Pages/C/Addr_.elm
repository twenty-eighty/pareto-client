module Pages.C.Addr_ exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Css
import Json.Decode as Decode
import Effect exposing (Effect)
import Graphics
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Community exposing (Community, communityMatchesFilter)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), eventFilterForNaddr)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations
import Ui.Styles exposing (Theme)
import Ui.View
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
    { community : Maybe Community
    , nip19 : Maybe NIP19Type
    }

-- ["REQ"," 30023-#d,autho-538",{"kinds":[30023],"#d":["1707912490439"],"authors":["ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600"]}]
-- ["REQ","uthors-ids-ki-plocc",{"authors":["ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600"],"ids":["1707912490439"],"kinds":[30023],"limit":20}]

init : Shared.Model -> Route { addr : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.addr

        (maybeNip19, maybeCommunity) =
            case decoded of
                Ok nip19 ->
                    (Just nip19, Nostr.getCommunityForNip19 shared.nostr nip19)

                Err _ ->
                    (Nothing, Nothing)

        effect =
            case (maybeCommunity, maybeNip19) of
                (Nothing, Just (NAddr naddrData)) ->
                    eventFilterForNaddr naddrData
                    |> RequestCommunity (Just naddrData.relays)
                    |> Nostr.createRequest shared.nostr "Community for NIP-19 address" [ KindUserMetadata ]
                    |> Shared.Msg.RequestNostrEvents
                    |> Effect.sendSharedMsg

                (_, _) ->
                    Effect.none
                
    in
    ( { community = maybeCommunity
      , nip19 = maybeNip19
      }
    , effect
    )

decodedTagParam : String -> Maybe (List String)
decodedTagParam tag =
    Url.percentDecode tag
    |> Maybe.map (List.singleton)

-- UPDATE


type Msg
    = OpenGetStarted
    | ReceivedMessage Nostr.IncomingMessage
    | NostrMsg Nostr.Msg


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd <| Ports.requestUser
            )

        ReceivedMessage message ->
            case message.messageType of
                "communities" ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        NostrMsg _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = "Read"
    , body =
        [
        model.nip19
        |> Maybe.map (Nostr.getCommunityForNip19 shared.nostr)
        |> Maybe.map (viewCommunity shared.browserEnv shared.nostr)
        |> Maybe.withDefault (div [][])
        ]
    }

viewCommunity : BrowserEnv -> Nostr.Model -> Maybe Community -> Html Msg
viewCommunity browserEnv nostr maybeCommunity =
    case maybeCommunity of
        Just community ->
            Ui.View.viewCommunity browserEnv nostr community 
        Nothing ->
            div [][]