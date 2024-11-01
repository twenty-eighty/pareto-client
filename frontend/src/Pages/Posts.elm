module Pages.Posts exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), kindDecoder, emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations
import Translations.Posts
import Ui.Article
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared user
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared user
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        {}


-- INIT


type alias Model =
    { category : Maybe Category
    }

type Category
    = Published
    | Drafts

type alias CategoryData =
    { category : Category
    , title : String
    , request : RequestData
    }

categories : I18Next.Translations -> Auth.User -> List CategoryData
categories translations user =
    [ { category = Published
      , title = Translations.Posts.publishedCategory [ translations ]
      , request = RequestArticles { emptyEventFilter | authors = Just [user.pubKey], kinds = Just [KindLongFormContent] }
      }
    , { category = Drafts
      , title = Translations.Posts.draftsCategory [ translations ]
      , request = RequestArticles { emptyEventFilter | authors = Just [user.pubKey], kinds = Just [KindDraftLongFormContent] }
      }
    ]


init : Shared.Model -> Auth.User -> () -> ( Model, Effect Msg )
init shared user () =
    let
        emptyModel =
            { category = Nothing }
    in
    -- select first category of list
    categories shared.browserEnv.translations user
    |> List.head
    |> Maybe.map (\firstCategory -> update shared (SelectCategory firstCategory) emptyModel)
    |> Maybe.withDefault (emptyModel, Effect.none)



-- UPDATE


type Msg
    = SelectCategory CategoryData


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        SelectCategory categoryData ->
            ( { model | category = Just categoryData.category }
            , categoryData.request
            |> Nostr.createRequest shared.nostr "Posts of user" []
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Auth.User -> Model -> View Msg
view shared user model =
    { title = Translations.postsMenuItemText [shared.browserEnv.translations]
    , body =
        [ viewCategories model shared.browserEnv user
        , viewArticles shared model
        ]
    }

viewArticles : Shared.Model -> Model -> Html Msg
viewArticles shared model =
    case model.category of
        Just Published ->
            Nostr.getArticlesByDate shared.nostr
            |> viewArticlePreviews shared.browserEnv shared.nostr

        Just Drafts ->
            Nostr.getArticleDraftsByDate shared.nostr
            |> viewArticleDraftPreviews shared.browserEnv shared.nostr

        Nothing ->
            div [][]

viewCategories : Model -> BrowserEnv -> Auth.User -> Html Msg
viewCategories model browserEnv user =
    categories browserEnv.translations user
    |> List.map (\categoryData -> viewCategory (model.category == Just categoryData.category) categoryData)
    |> div
        [ css
            [ Tw.flex
            , Tw.space_x_4
            , Tw.mb_6
            ]
        ]

viewCategory : Bool -> CategoryData -> Html Msg
viewCategory active data =
    let
        attrs =
            if active then
                [ Tw.bg_color Theme.purple_100
                , Tw.text_color Theme.purple_600
                ]
            else
                [ Tw.bg_color Theme.gray_100
                , Tw.text_color Theme.gray_600
                ]
    in
    button
        [ css
            ([ Tw.px_4
            , Tw.py_2
            , Tw.rounded_full
            ] ++ attrs)
        , Events.onClick (SelectCategory data)
        ]
        [ text data.title ]

viewArticlePreviews : BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> Ui.Article.viewArticlePreview browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
    |> div []

viewArticleDraftPreviews : BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticleDraftPreviews browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> Ui.Article.viewArticleDraftPreview browserEnv article)
    |> div []


