module Pages.E.Event_ exposing (..)

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
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..))
import Nostr.Nip19 as Nip19
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations
import Url
import View exposing (View)
import Ports
import Time


page : Shared.Model -> Route { event : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
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
    { articles : List Article
    , filter : EventFilter
    }


init : Route { event : String } -> () -> ( Model, Effect Msg )
init route () =
    let
        decoded =
            Nip19.decode route.params.event

        filter =
            { authors = Nothing
            , ids = Nothing
            , kinds = Just [KindLongFormContent]
            , tagReferences = tagReferencesForParam route.params.event
            , limit = Just 20
            , since = Nothing
            , until = Nothing
            }
    in
    ( { articles = []
      , filter = filter
      }
    , Effect.sendCmd <| Ports.requestEvents ("NIP-19 event: " ++ route.params.event) True -1 filter
    )

tagReferencesForParam : String -> Maybe (List TagReference)
tagReferencesForParam tag =
    decodedTagParam tag
    |> Maybe.map TagReferenceEventId
    |> Maybe.map (List.singleton)

decodedTagParam : String -> Maybe String
decodedTagParam tag =
    Url.percentDecode tag

-- UPDATE


type Msg
    = OpenGetStarted
    | ReceivedMessage Nostr.IncomingMessage
    | NostrMsg Nostr.Msg


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model
            , Effect.sendCmd <| Ports.requestUser
            )

        ReceivedMessage message ->
            ( model, Effect.none )

        NostrMsg _ ->
            ( model, Effect.none )

addArticle : List Article -> Article -> List Article
addArticle articleList newArticle =
    if List.any (isArticleWithIdAndAuthor newArticle.author newArticle.id) articleList then
        newArticle :: articleList
    else
        newArticle :: articleList

isArticleWithIdAndAuthor : String -> String -> Article -> Bool
isArticleWithIdAndAuthor author articleId article =
    article.author == author && article.id == articleId

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = "Read"
    , body = [                                  {- Main Content -}
            div
                [ css
                    [ Tw.flex
                    , Tw.space_x_4
                    , Tw.mb_6
                    ]
                ]
                [ button
                    [ css
                        [ Tw.bg_color Theme.purple_100
                        , Tw.text_color Theme.purple_600
                        , Tw.px_4
                        , Tw.py_2
                        , Tw.rounded_full
                        ]
                    ]
                    [ text "Articles" ]
                , button
                    [ css
                        [ Tw.bg_color Theme.gray_100
                        , Tw.text_color Theme.gray_600
                        , Tw.px_4
                        , Tw.py_2
                        , Tw.rounded_full
                        ]
                    ]
                    [ text "Highlights" ]
                ]
            , Nostr.getArticlesByDate shared.nostr
            |> Nostr.viewArticlePreviews shared.browserEnv shared.nostr 
            ]
    }
