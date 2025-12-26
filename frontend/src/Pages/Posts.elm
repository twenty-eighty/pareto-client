module Pages.Posts exposing (Model, Msg, page)

import Auth
import Components.ArticleComments as ArticleComments
import Components.Categories as Categories
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, article, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Article exposing (addressComponentsForArticle, nip19ForArticle)
import Nostr.DeletionRequest exposing (deletionEvent)
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, RelayUrl, loggedInPubKey)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Set exposing (Set)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.Posts as Translations
import Translations.Sidebar
import Ui.Article
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


categoryParamName : String
categoryParamName =
    "category"


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update user shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    let
        topPart =
            Categories.new
                { model = model.categories
                , toMsg = CategoriesSent
                , onSelect = CategorySelected
                , equals = \category1 category2 -> category1 == category2
                , image = \_ _ -> Nothing
                , categories = availableCategories shared
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> Categories.view
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withTopPart topPart Categories.heightString
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { categories : Categories.Model Category
    , path : Route.Path.Path
    }


type Category
    = Published
    | Drafts
    | Future


availableCategories : Shared.Model -> List (Categories.CategoryData Category)
availableCategories shared =
    let
        isBetaTester =
            loggedInPubKey shared.loginStatus
                |> Maybe.map (Nostr.isBetaTester shared.nostr)
                |> Maybe.withDefault False

        delayedCategory =
            if isBetaTester then
                [ { category = Future
                , title = Translations.futureCategory [ shared.browserEnv.translations ]
                , testId = "posts-future"
                } ]
            else
                []
    in
    [ { category = Published
      , title = Translations.publishedCategory [ shared.browserEnv.translations ]
      , testId = "posts-published"
      }
    , { category = Drafts
      , title = Translations.draftsCategory [ shared.browserEnv.translations ]
      , testId = "posts-drafts"
      }
    ] ++ delayedCategory


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        category =
            Dict.get categoryParamName route.query
                |> Maybe.andThen categoryFromString
                |> Maybe.withDefault Published
    in
    updateModelWithCategory
        user
        shared
        { categories = Categories.init { selected = category }
        , path = route.path
        }
        category


categoryFromString : String -> Maybe Category
categoryFromString categoryString =
    case categoryString of
        "published" ->
            Just Published

        "drafts" ->
            Just Drafts

        "future" ->
            Just Future

        _ ->
            Nothing


stringFromCategory : Category -> String
stringFromCategory category =
    case category of
        Published ->
            "published"

        Drafts ->
            "drafts"

        Future ->
            "future"


-- UPDATE


type Msg
    = CategorySelected Category
    | CategoriesSent (Categories.Msg Category Msg)
    | DeleteEvent (Set RelayUrl) (List Kind) EventId (Maybe AddressComponents) -- draft event id
    | EditDraft String
    | NoOp

update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        CategorySelected category ->
            updateModelWithCategory user shared model category

        CategoriesSent innerMsg ->
            Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
                , toMsg = CategoriesSent
                }

        DeleteEvent relayUrls kinds eventId maybeAddressComponents ->
            ( model
            , deletionEvent user.pubKey shared.browserEnv.now eventId "Deleting article or draft" maybeAddressComponents kinds
                |> SendDeletionRequest (relayUrls |> Set.toList)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        EditDraft nip19 ->
            ( model, Effect.pushRoute { path = Route.Path.Write, query = Dict.singleton "a" nip19, hash = Nothing } )

        NoOp ->
            ( model, Effect.none )


updateModelWithCategory : Auth.User -> Shared.Model -> Model -> Category -> ( Model, Effect Msg )
updateModelWithCategory user shared model category =
    let
        ( request, filters, description ) =
            case category of
                Published ->
                    ( RequestArticlesFeed False
                    , [ { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just [ user.pubKey ], limit = Just 20 } ]
                    , "Posts of user"
                    )

                Drafts ->
                    ( RequestArticleDrafts
                    , [ { emptyEventFilter | kinds = Just [ KindDraftLongFormContent, KindDraft ], authors = Just [ user.pubKey ] }
                      , { emptyEventFilter | kinds = Just [ KindDraftLongFormContent ], tagReferences = Just [ TagReferencePubKey user.pubKey ] }
                      ]
                    , "Drafts of user"
                    )

                Future ->
                    ( RequestFutureArticles
                    , [ { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just [ user.pubKey ], limit = Just 20 } ]
                    , "Future posts of user"
                    )
    in
    ( model
    , [ Effect.replaceRoute { path = model.path, query = Dict.singleton categoryParamName (stringFromCategory category), hash = Nothing }
      , filters
            |> request
            |> Nostr.createRequest shared.nostr description [ KindUserMetadata ]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
      ]
        |> Effect.batch
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = Translations.Sidebar.postsMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ viewArticles shared model
        ]
    }


viewArticles : Shared.Model -> Model -> Html Msg
viewArticles shared model =
    let
        articles =
            case Categories.selected model.categories of
                Published ->
                    Nostr.getArticlesByDate shared.nostr

                Drafts ->
                    Nostr.getArticleDraftsByDate shared.nostr

                Future ->
                    Nostr.getArticlesByDate shared.nostr
    in
    articles
        |> Ui.View.viewArticlePreviews
            ArticlePreviewList
            { articleComments = ArticleComments.init
            , articleToInteractionsMsg = \_ _ -> NoOp
            , bookmarkButtonMsg = \_ _ -> NoOp
            , bookmarkButtons = Dict.empty
            , browserEnv = shared.browserEnv
            , commentsToMsg = \_ -> NoOp
            , deleteButtonMsg = Just DeleteEvent
            , nostr = shared.nostr
            , loginStatus = shared.loginStatus
            , onLoadMore = Nothing
            , sharing = Nothing
            , theme = shared.theme
            }

