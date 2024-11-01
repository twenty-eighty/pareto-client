module Pages.P.Profile_ exposing (Model, Msg, page)

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
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile)
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (PubKey)
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
import Ui.Article
import Ui.Profile
import Ui.Shared exposing (fontFamilyUnbounded, fontFamilyInter)
import View exposing (View)


page : Shared.Model -> Route { profile : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        {}


-- INIT


type alias Model =
    { pubKey : Maybe PubKey
    , relays : List String
    }


init : Shared.Model -> Route { profile : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            decodeParam route.params.profile
            |> Maybe.map (\(pubKey, relays) ->
                { pubKey = Just pubKey
                , relays = relays
                }
            )
            |> Maybe.withDefault { pubKey = Nothing, relays = [] }

        requestEffect =
            model.pubKey
            |> Maybe.map (\pubKey ->
                case Nostr.getProfile shared.nostr pubKey of
                    Just _ ->
                        Effect.none

                    Nothing ->
                        filterForAuthor pubKey
                        |> RequestProfile 
                        |> Nostr.createRequest shared.nostr "Profile" [KindLongFormContent]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
            )
            |> Maybe.withDefault Effect.none

    in
    ( model, requestEffect
    )

filterForAuthor : PubKey -> EventFilter
filterForAuthor author =
            { emptyEventFilter | authors = Just [author], kinds = Just [ KindUserMetadata ], limit = Just 1 }

decodeParam : String -> Maybe (PubKey, List String)
decodeParam profile =
    case Nip19.decode profile of
        Ok (Nip19.NProfile { pubKey , relays }) ->
            Just (pubKey, relays)

        Ok _ ->
            -- unexpected NIP-19 value
            Nothing

        Err _ ->
            Nothing


-- UPDATE


type Msg
    = NoOp


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        maybeProfile =
            model.pubKey
            |> Maybe.andThen (\pubKey ->
                Nostr.getProfile shared.nostr pubKey
            )
    in
    { title = "Profile"
    , body =
        [ case maybeProfile of
            Just profile ->
                viewProfile shared profile

            Nothing ->
                div [][]
        ]
    }

viewProfile : Shared.Model -> Profile -> Html Msg
viewProfile shared profile =
    div []
        [ Ui.Profile.viewProfile profile
        , Nostr.getArticlesForAuthor shared.nostr profile.pubKey
        |> viewArticlePreviews shared.browserEnv shared.nostr 
        ]

viewArticlePreviews : BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article ->
        let
            author = 
                (Nostr.getAuthor nostr article.author)

            interactions =
                (Nostr.getInteractions nostr article)
        in
        Ui.Article.viewArticlePreview browserEnv author article interactions False
        )
    |> div []