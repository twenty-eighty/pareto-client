module Pages.Posts exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Categories
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
import Ui.Styles exposing (Styles, Theme)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared user
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


-- INIT


type alias Model =
    { categories : Components.Categories.Model Category
    }

type Category
    = Published
    | Drafts

availableCategories : I18Next.Translations -> List (Components.Categories.CategoryData Category)
availableCategories translations =
    [ { category = Published
      , title = Translations.Posts.publishedCategory [ translations ]
      }
    , { category = Drafts
      , title = Translations.Posts.draftsCategory [ translations ]
      }
    ]


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    updateModelWithCategory
        shared
        { categories = Components.Categories.init { selected = Published } }
        Published

-- UPDATE


type Msg
    = CategorySelected Category
    | CategoriesSent (Components.Categories.Msg Category Msg)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        CategorySelected category ->
            updateModelWithCategory shared model category

        CategoriesSent innerMsg ->
            Components.Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories}
                , toMsg = CategoriesSent
                }

updateModelWithCategory : Shared.Model -> Model -> Category -> (Model, Effect Msg)
updateModelWithCategory shared model category =
    let
        filter =
            case (shared.loginStatus, category) of
                (Shared.Model.LoggedIn pubKey, Published) ->
                    { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just [pubKey], limit = Just 20 }

                (Shared.Model.LoggedIn pubKey, Drafts) ->
                    { emptyEventFilter | kinds = Just [KindDraftLongFormContent], authors = Just [pubKey], limit = Just 20 }

                (_, _) ->
                    emptyEventFilter
    in
    ( model
    , RequestArticlesFeed filter
      |> Nostr.createRequest shared.nostr "Posts of user" [KindUserMetadata]
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
        [ Components.Categories.new
            { model = model.categories
            , toMsg = CategoriesSent
            , onSelect = CategorySelected
            , categories = availableCategories shared.browserEnv.translations
            , browserEnv = shared.browserEnv
            , styles = Ui.Styles.stylesForTheme shared.theme
            }
            |> Components.Categories.view
        , viewArticles shared model
        ]
    }

viewArticles : Shared.Model -> Model -> Html Msg
viewArticles shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme
    in
    case Components.Categories.selected model.categories of
        Published ->
            Nostr.getArticlesByDate shared.nostr
            |> viewArticlePreviews styles shared.browserEnv shared.nostr

        Drafts ->
            Nostr.getArticleDraftsByDate shared.nostr
            |> viewArticleDraftPreviews styles shared.browserEnv shared.nostr

viewArticlePreviews : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews styles browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> Ui.Article.viewArticlePreviewList styles browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
    |> div []

viewArticleDraftPreviews : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticleDraftPreviews styles browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> Ui.Article.viewArticleDraftPreview styles browserEnv article)
    |> div []


