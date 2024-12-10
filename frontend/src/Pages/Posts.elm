module Pages.Posts exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button exposing (Button)
import Components.Categories
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.DeletionRequest exposing (draftDeletionEvent)
import Nostr.Event exposing (EventFilter, Kind(..), kindDecoder, emptyEventFilter)
import Nostr.Nip19 as Nip19
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.Posts as Translations
import Translations.Sidebar
import Ui.Article
import Ui.Styles exposing (Styles, Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared
        , update = update user shared
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
      , title = Translations.publishedCategory [ translations ]
      }
    , { category = Drafts
      , title = Translations.draftsCategory [ translations ]
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
    | DeleteDraft String (Maybe String) -- draft event id
    | EditDraft String

update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
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

        DeleteDraft draftArticleId draftIdentifier ->
            ( model
            , draftDeletionEvent user.pubKey shared.browserEnv.now draftArticleId "Deleting draft" draftIdentifier
                |> SendDeletionRequest (Nostr.getDraftRelayUrls shared.nostr draftArticleId)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        EditDraft nip19 ->
            (model, Effect.pushRoute { path = Route.Path.Write, query = Dict.singleton "a" nip19, hash = Nothing } )

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
    { title = Translations.Sidebar.postsMenuItemText [shared.browserEnv.translations]
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
            |> Ui.View.viewArticlePreviews ArticlePreviewList styles shared.browserEnv shared.nostr 

        Drafts ->
            Nostr.getArticleDraftsByDate shared.nostr
            |> viewArticleDraftPreviews shared.theme shared.browserEnv shared.nostr

viewArticlePreviews : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews styles browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> Ui.Article.viewArticlePreviewList styles browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
    |> div []

viewArticleDraftPreviews : Theme -> BrowserEnv -> Nostr.Model -> List Article -> Html Msg
viewArticleDraftPreviews theme browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> viewArticleDraftPreview theme browserEnv article)
    |> div []


viewArticleDraftPreview : Ui.Styles.Theme -> BrowserEnv -> Article -> Html Msg
viewArticleDraftPreview theme browserEnv article =
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
                [ Ui.Article.timeParagraph styles browserEnv article.publishedAt
                , deleteDraftButton theme (Translations.deleteDraftButtonLabel [browserEnv.translations]) article
                , editDraftButton theme (Translations.editDraftButtonLabel [browserEnv.translations]) article
                ]
            , Ui.Article.viewTitleSummaryImagePreview styles article
            , Ui.Article.viewTags styles article
            ]
        ]


deleteDraftButton : Theme -> String -> Article -> Html Msg
deleteDraftButton theme label article =
    Button.new
        { label = label
        , onClick = Just <| DeleteDraft article.id article.identifier
        , theme = theme
        }
        |> Button.view


editDraftButton : Theme -> String -> Article -> Html Msg
editDraftButton theme label article =
    Button.new
        { label = label
        , onClick = Maybe.map EditDraft (editDraftLink article)
        , theme = theme
        }
        |> Button.view


editDraftLink : Article -> Maybe String
editDraftLink article =
    Nip19.NAddr 
        { identifier = article.identifier |> Maybe.withDefault ""
        , pubKey = article.author
        , kind = Nostr.Event.numberForKind article.kind
        , relays = []
        }
    |> Nip19.encode
    |> Result.toMaybe