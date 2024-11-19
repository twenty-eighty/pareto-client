module Pages.Read exposing (Model, Msg, page, init, update, view, subscriptions)

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
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations
import Translations.Read
import Ui.Styles exposing (referenceDesignStyles)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)
import Ports
import Pareto


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        { styles = referenceDesignStyles }


-- INIT


type alias Model =
    { categories : Components.Categories.Model Category
    }

type Category
    = Global
    | Pareto
    | Followed
    | Highlighter


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        filter =
            { emptyEventFilter | kinds = Just [KindLongFormContent] , limit = Just 20 }
    in
    ( { categories = Components.Categories.init { selected = Global }}
    , RequestArticlesFeed filter
      |> Nostr.createRequest shared.nostr "Long-form articles" [KindUserMetadata]
      |> Shared.Msg.RequestNostrEvents
      |> Effect.sendSharedMsg
    )



-- UPDATE


type Msg
    = OpenGetStarted
    | CategorySelected Category
    | CategoriesSent (Components.Categories.Msg Category Msg)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd <| Ports.requestUser
            )

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
            case category of
                Global ->
                    { emptyEventFilter | kinds = Just [KindLongFormContent], limit = Just 20 }

                Pareto ->
                    { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just (paretoFollowsList shared.nostr) , limit = Just 20 }

                Followed ->
                    { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just (userFollowsList shared.nostr shared.loginStatus) , limit = Just 20 }

                Highlighter ->
                    { emptyEventFilter | kinds = Just [KindLongFormContent], limit = Just 20 }
    in
    ( model
    , RequestArticlesFeed filter
      |> Nostr.createRequest shared.nostr "Long-form articles" [KindUserMetadata]
      |> Shared.Msg.RequestNostrEvents
      |> Effect.sendSharedMsg
    )

paretoFollowsList : Nostr.Model -> List PubKey
paretoFollowsList nostr =
    Nostr.getFollowsList nostr Pareto.authorsKey
    |> Maybe.map (List.map .pubKey)
    |> Maybe.withDefault []


userFollowsList : Nostr.Model -> Shared.Model.LoginStatus -> List PubKey
userFollowsList nostr loginStatus =
    case loginStatus of
        Shared.Model.LoggedIn pubKey ->
            Nostr.getFollowsList nostr pubKey
            |> Maybe.map (List.map .pubKey)
            |> Maybe.withDefault []

        _ ->
            []

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


availableCategories : Nostr.Model -> Shared.Model.LoginStatus -> I18Next.Translations -> List (Components.Categories.CategoryData Category)
availableCategories nostr loginStatus translations =
    let
        paretoCategories =
            if paretoFollowsList nostr /= [] then
                [ paretoCategory translations ]
            else
                []

        followedCategories =
            if userFollowsList nostr loginStatus /= [] then
                [ followedCategory translations ]
            else
                []
    in
    followedCategories ++ paretoCategories ++
    [ { category = Global
      , title = Translations.Read.globalFeedCategory [ translations ]
      }
--   , { category = Highlighter
--     , title = Translations.Read.highlighterFeedCategory [ translations ]
--     }
    ]

paretoCategory : I18Next.Translations -> Components.Categories.CategoryData Category
paretoCategory translations =
    { category = Pareto
    , title = Translations.Read.paretoFeedCategory [ translations ]
    }

followedCategory : I18Next.Translations -> Components.Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.Read.followedFeedCategory [ translations ]
    }

view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = Translations.readMenuItemText [shared.browserEnv.translations]
    , body = [                                  {- Main Content -}
            Components.Categories.new
                { model = model.categories
                , toMsg = CategoriesSent
                , onSelect = CategorySelected
                , categories = availableCategories shared.nostr shared.loginStatus shared.browserEnv.translations
                , browserEnv = shared.browserEnv
                , styles = referenceDesignStyles
                }
                |> Components.Categories.view
            , Nostr.getArticlesByDate shared.nostr
             |> Ui.View.viewArticlePreviews ArticlePreviewList Ui.Styles.referenceDesignStyles shared.browserEnv shared.nostr 
            ]
    }
