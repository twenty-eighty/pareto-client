module Pages.U.User_ exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05 exposing (Nip05)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
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
import Ui.ArticleOld
import Ui.Profile
import Ui.Styles exposing (fontFamilyUnbounded, fontFamilyInter)
import Ui.Styles exposing (referenceDesignStyles)
import View exposing (View)
import Nostr.Event exposing (Kind(..))
import Nostr.Nip05 exposing (nip05ToString)


page : Shared.Model -> Route { user : String } -> Page Model Msg
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
    { nip05 : Maybe Nip05
    }


init : Shared.Model -> Route { user : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            { nip05 = Nip05.parseNip05 route.params.user }


        requestEffect =
            model.nip05
            |> Maybe.map (\nip05 ->
                case Nostr.getPubKeyByNip05 shared.nostr nip05 of
                    Just _ ->
                        -- already validated, don't do again
                        Effect.none

                    Nothing ->
                        RequestProfileByNip05 nip05
                        |> Nostr.createRequest shared.nostr ("Profile and data of NIP-05 user " ++ nip05ToString nip05) [KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
            )
            |> Maybe.withDefault Effect.none

    in
    ( model, requestEffect
    )


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
            model.nip05
            |> Maybe.andThen (\nip05 ->
                Nostr.getProfileByNip05 shared.nostr nip05
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
        [ Ui.Profile.viewProfile profile (Nostr.getProfileValidationStatus shared.nostr profile.pubKey |> Maybe.withDefault ValidationUnknown)
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
        Ui.ArticleOld.viewArticlePreview browserEnv referenceDesignStyles author article interactions False
        )
    |> div []