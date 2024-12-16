module Pages.P.Profile_ exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Components.RelayStatus exposing (Purpose(..))
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile)
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Translations.Sidebar as Translations
import Ui.ArticleOld
import Ui.Profile
import Ui.Styles exposing (Styles, Theme)
import Ui.View exposing (viewRelayStatus)
import View exposing (View)
import Nostr.Profile exposing (ProfileValidation(..))


page : Shared.Model -> Route { profile : String } -> Page Model Msg
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
    { pubKey : Maybe PubKey
    , relays : List String
    , requestId : Maybe RequestId
    }


init : Shared.Model -> Route { profile : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            decodeParam route.params.profile
            |> Maybe.map (\(pubKey, relays) ->
                { pubKey = Just pubKey
                , relays = relays
                , requestId = Nothing
                }
            )
            |> Maybe.withDefault { pubKey = Nothing, relays = [], requestId = Nothing }

        ( requestProfileEffect, requestId ) =
            model.pubKey
            |> Maybe.map (\pubKey ->
                case Nostr.getProfile shared.nostr pubKey of
                    Just _ ->
                        (Effect.none, Nothing)

                    Nothing ->
                        ( filterForAuthor pubKey
                            |> RequestProfile Nothing
                            |> Nostr.createRequest shared.nostr "Profile" [KindLongFormContent]
                            |> Shared.Msg.RequestNostrEvents
                            |> Effect.sendSharedMsg
                        , Just <| Nostr.getLastRequestId shared.nostr
                        )
            )
            |> Maybe.withDefault (Effect.none, Nothing)

        requestArticlesEffect =
            model.pubKey
            |> Maybe.map (buildRequestArticlesEffect shared.nostr)
            |> Maybe.withDefault Effect.none
    in
    ( { model | requestId = requestId }
    , Effect.batch
        [ requestProfileEffect
        , requestArticlesEffect
        ] 
    )

buildRequestArticlesEffect : Nostr.Model -> PubKey -> Effect Msg
buildRequestArticlesEffect nostr pubKey =
    { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just [pubKey], limit = Just 20 }
    |> RequestArticlesFeed 
    |> Nostr.createRequest nostr "Posts of user" [KindUserMetadata]
    |> Shared.Msg.RequestNostrEvents
    |> Effect.sendSharedMsg

filterForAuthor : PubKey -> EventFilter
filterForAuthor author =
            { emptyEventFilter | authors = Just [author], kinds = Just [ KindUserMetadata ], limit = Just 1 }

decodeParam : String -> Maybe (PubKey, List String)
decodeParam profile =
    case Nip19.decode profile of
        Ok (Nip19.NProfile { pubKey, relays }) ->
            Just (pubKey, relays)

        Ok (Nip19.Npub pubKey) ->
            Just (pubKey, [])

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
            |> Maybe.andThen (Nostr.getProfile shared.nostr)
    in
    { title = Translations.readMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ case maybeProfile of
            Just profile ->
                viewProfile shared profile

            Nothing ->
                viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingProfile model.requestId
        ]
    }

viewProfile : Shared.Model -> Profile -> Html Msg
viewProfile shared profile =
    div []
        [ Ui.Profile.viewProfile profile (Nostr.getProfileValidationStatus shared.nostr profile.pubKey |> Maybe.withDefault ValidationUnknown)
        , Nostr.getArticlesForAuthor shared.nostr profile.pubKey
        |> viewArticlePreviews (Ui.Styles.stylesForTheme shared.theme) shared.browserEnv shared.nostr 
        ]

viewArticlePreviews : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews styles browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article ->
        let
            author = 
                (Nostr.getAuthor nostr article.author)

            interactions =
                (Nostr.getInteractions nostr Nothing article)
        in
        Ui.ArticleOld.viewArticlePreview browserEnv styles author article interactions False
        )
    |> div []