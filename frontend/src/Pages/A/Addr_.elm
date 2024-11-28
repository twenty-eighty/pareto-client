module Pages.A.Addr_ exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event as Event
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..))
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Msg
import Shared.Model
import Translations.Sidebar as Translations
import Ui.Styles exposing (Styles, Theme)
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
    { nip19 : Maybe NIP19Type
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

        effect =
            case (maybeArticle, maybeNip19) of
                (Nothing, Just (NAddr naddrData)) ->
                    Event.eventFilterForNaddr naddrData
                    |> RequestArticle (Just naddrData.relays)
                    |> Nostr.createRequest shared.nostr "Article described as NIP-19" [KindUserMetadata]
                    |> Shared.Msg.RequestNostrEvents
                    |> Effect.sendSharedMsg

                (_, _) ->
                    Effect.none
                
    in
    ( { nip19 = maybeNip19
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


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd <| Ports.requestUser
            )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = Translations.readMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ model.nip19
          |> Maybe.andThen (Nostr.getArticleForNip19 shared.nostr)
          |> viewArticle (Ui.Styles.stylesForTheme shared.theme) shared.browserEnv shared.nostr
        ]
    }

viewArticle : Styles Msg -> BrowserEnv -> Nostr.Model -> Maybe Article -> Html Msg
viewArticle styles browserEnv nostr maybeArticle =
    case maybeArticle of
        Just article ->
            Ui.View.viewArticle styles browserEnv nostr article 

        Nothing ->
            div [][]