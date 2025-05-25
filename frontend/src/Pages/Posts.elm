module Pages.Posts exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Categories as Categories
import Components.Interactions as Interactions
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, article, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Article exposing (Article, nip19ForArticle)
import Nostr.DeletionRequest exposing (draftDeletionEvent)
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Profile exposing (Author)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
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
        , view = view shared user
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
                , categories = availableCategories shared.browserEnv.translations
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


availableCategories : I18Next.Translations -> List (Categories.CategoryData Category)
availableCategories translations =
    [ { category = Published
      , title = Translations.publishedCategory [ translations ]
      }
    , { category = Drafts
      , title = Translations.draftsCategory [ translations ]
      }
    ]


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

        _ ->
            Nothing


stringFromCategory : Category -> String
stringFromCategory category =
    case category of
        Published ->
            "published"

        Drafts ->
            "drafts"



-- UPDATE


type Msg
    = CategorySelected Category
    | CategoriesSent (Categories.Msg Category Msg)
    | DeleteDraft String (Maybe String) -- draft event id
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

        DeleteDraft draftArticleId draftIdentifier ->
            ( model
            , draftDeletionEvent user.pubKey shared.browserEnv.now draftArticleId "Deleting draft" draftIdentifier
                |> SendDeletionRequest (Nostr.getDraftRelayUrls shared.nostr draftArticleId)
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
                    ( RequestArticlesFeed
                    , [ { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just [ user.pubKey ], limit = Just 20 } ]
                    , "Posts of user"
                    )

                Drafts ->
                    ( RequestArticleDrafts
                    , [ { emptyEventFilter | kinds = Just [ KindDraftLongFormContent, KindDraft ], authors = Just [ user.pubKey ], limit = Just 20 }
                      , { emptyEventFilter | kinds = Just [ KindDraftLongFormContent ], tagReferences = Just [ TagReferencePubKey user.pubKey ], limit = Just 20 }
                      ]
                    , "Drafts of user"
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


view : Shared.Model.Model -> Auth.User -> Model -> View Msg
view shared user model =
    { title = Translations.Sidebar.postsMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ viewArticles shared model user.pubKey
        ]
    }


viewArticles : Shared.Model -> Model -> PubKey -> Html Msg
viewArticles shared model userPubKey =
    case Categories.selected model.categories of
        Published ->
            Nostr.getArticlesByDate shared.nostr
                |> Ui.View.viewArticlePreviews
                    ArticlePreviewList
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , loginStatus = shared.loginStatus
                    , onBookmark = Nothing
                    , commenting = Nothing
                    , onReaction = Nothing
                    , onRepost = Nothing
                    , onZap = Nothing
                    , articleToInteractionsMsg = \_ _ -> NoOp
                    , openCommentMsg = Nothing
                    , sharing = Nothing
                    }

        Drafts ->
            let
                author =
                    Nostr.getAuthor shared.nostr userPubKey
            in
            Nostr.getArticleDraftsByDate shared.nostr
                |> viewArticleDraftPreviews shared.theme shared.browserEnv author


viewArticleDraftPreviews : Theme -> BrowserEnv -> Author -> List Article -> Html Msg
viewArticleDraftPreviews theme browserEnv author articles =
    articles
        |> List.take 20
        |> List.map (\article -> viewArticleDraftPreview theme browserEnv author article)
        |> div []


viewArticleDraftPreview : Ui.Styles.Theme -> BrowserEnv -> Author -> Article -> Html Msg
viewArticleDraftPreview theme browserEnv author article =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.mb_4
            ]
        ]
        [ div
            [ css
                [ Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.min_w_96
                , Tw.max_w_3xl
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.justify_between
                    , Tw.mb_3
                    ]
                ]
                [ Ui.Article.timeParagraph styles browserEnv article.publishedAt article.createdAt
                , deleteDraftButton theme (Translations.deleteDraftButtonLabel [ browserEnv.translations ]) article
                , editDraftButton theme (Translations.editDraftButtonLabel [ browserEnv.translations ]) article
                ]
            , Ui.Article.viewTitleSummaryImagePreview browserEnv.translations styles author article
            , Ui.Article.viewTags browserEnv.translations article
            ]
        ]


deleteDraftButton : Theme -> String -> Article -> Html Msg
deleteDraftButton theme label article =
    Button.new
        { label = label
        , onClick = Just <| DeleteDraft article.id article.identifier
        , theme = theme
        }
        |> Button.withStyleDanger
        |> Button.view


editDraftButton : Theme -> String -> Article -> Html Msg
editDraftButton theme label article =
    Button.new
        { label = label
        , onClick = Maybe.map EditDraft (nip19ForArticle article)
        , theme = theme
        }
        |> Button.view
