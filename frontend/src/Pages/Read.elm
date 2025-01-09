module Pages.Read exposing (Model, Msg, page, init, update, view, subscriptions)

import BrowserEnv exposing (BrowserEnv)
import Components.Categories
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layouts
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle, addressForArticle, nip19ForArticle)
import Nostr.Event exposing (AddressComponents, EventFilter, Kind(..), kindDecoder, emptyEventFilter)
import Nostr.FollowList exposing (followingPubKey)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (EventId, PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Shared.Msg
import Translations.Read
import Translations.Sidebar
import Ui.ShortNote exposing (ShortNotesViewData)
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)
import Ports
import Pareto
import Material.Icons exposing (category)
import Ui.ShortNote as ShortNote


page : Shared.Model -> Route () -> Page Model Msg
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
    { categories : Components.Categories.Model Category
    , path : Route.Path.Path
    }

type Category
    = Global
    | Pareto
    | Followed
    | Highlighter
    | Rss

stringFromCategory : Category -> String 
stringFromCategory category =
    case category of
        Global ->
            "global"

        Pareto ->
            "pareto"

        Followed ->
            "followed"

        Highlighter ->
            "highlights"

        Rss ->
            "rss"


categoryFromString : String -> Maybe Category
categoryFromString categoryString =
    case categoryString of
        "global" ->
            Just Global

        "pareto" ->
            Just Pareto

        "followed" ->
            Just Followed

        "highlights" ->
            Just Highlighter

        _ ->
            Nothing

categoryParamName : String
categoryParamName =
    "category"

init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    let
        category =
            Dict.get categoryParamName route.query
            |> Maybe.andThen categoryFromString
            |> Maybe.withDefault Global

        correctedCategory =
            case category of
                Pareto ->
                    if (paretoFollowsList shared.nostr) == [] then
                        Global
                    else
                        category

                Followed ->
                    if (userFollowsList shared.nostr shared.loginStatus) == [] then
                        Global
                    else
                        category

                _ ->
                    category

        changeCategoryEffect =
            if correctedCategory /= category then
                Effect.replaceRoute { path = route.path, query = Dict.singleton categoryParamName (stringFromCategory correctedCategory), hash = Nothing }
            else
                Effect.none
    in
    ( { categories = Components.Categories.init { selected = correctedCategory }
      , path = route.path
      }
    , Effect.batch
        [ changeCategoryEffect
        , RequestArticlesFeed (filterForCategory shared correctedCategory)
            |> Nostr.createRequest shared.nostr "Long-form articles" [KindUserMetadata, KindEventDeletionRequest]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        ]
    )



-- UPDATE


type Msg
    = OpenGetStarted
    | CategorySelected Category
    | CategoriesSent (Components.Categories.Msg Category Msg)
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | RemoveArticleReaction PubKey EventId  -- event ID of like
    | AddShortNoteBookmark PubKey EventId
    | RemoveShortNoteBookmark PubKey EventId


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

        AddArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithoutArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddArticleReaction userPubKey eventId articlePubKey addressComponents ->
            ( model
            , SendReaction userPubKey eventId articlePubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveArticleReaction pubKey eventId ->
            ( model, Effect.none )

        AddShortNoteBookmark pubKey eventId ->
            ( model
            , SendBookmarkListWithShortNote pubKey eventId
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveShortNoteBookmark pubKey eventId ->
            ( model
            , SendBookmarkListWithoutShortNote pubKey eventId
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

updateModelWithCategory : Shared.Model -> Model -> Category -> (Model, Effect Msg)
updateModelWithCategory shared model category =
    ( model
    , Effect.batch
        [ Effect.replaceRoute { path = model.path, query = Dict.singleton categoryParamName (stringFromCategory category), hash = Nothing }
        , RequestArticlesFeed (filterForCategory shared category)
            |> Nostr.createRequest shared.nostr "Long-form articles" [KindUserMetadata, KindEventDeletionRequest]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        ]
    )

filterForCategory : Shared.Model -> Category -> EventFilter
filterForCategory shared category =
    case category of
        Global ->
            { emptyEventFilter | kinds = Just [KindLongFormContent], limit = Just 20 }

        Pareto ->
            { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just (paretoFollowsList shared.nostr) , limit = Just 20 }

        Followed ->
            { emptyEventFilter | kinds = Just [KindLongFormContent], authors = Just (userFollowsList shared.nostr shared.loginStatus) , limit = Just 20 }

        Highlighter ->
            { emptyEventFilter | kinds = Just [KindLongFormContent], limit = Just 20 }

        Rss ->
            { emptyEventFilter | kinds = Just [KindShortTextNote], authors = Just (paretoRssFollowsList shared.nostr) , limit = Just 20 }

paretoFollowsList : Nostr.Model -> List PubKey
paretoFollowsList nostr =
    case Nostr.getFollowsList nostr Pareto.authorsKey of
        Just followsList ->
            followsList
            |> List.filterMap followingPubKey

        Nothing ->
            []

paretoRssFollowsList : Nostr.Model -> List PubKey
paretoRssFollowsList nostr =
    case Nostr.getFollowsList nostr Pareto.rssAuthorsKey of
        Just followsList ->
            followsList
            |> List.filterMap followingPubKey

        Nothing ->
            []

userFollowsList : Nostr.Model -> Shared.Model.LoginStatus -> List PubKey
userFollowsList nostr loginStatus =
    case Shared.loggedInPubKey loginStatus of
        Just pubKey ->
            case Nostr.getFollowsList nostr pubKey of
                Just followsList ->
                    followsList
                    |> List.filterMap followingPubKey

                Nothing ->
                    []

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

        paretoRssCategories =
            if paretoRssFollowsList nostr /= [] then
                [ paretoRssCategory translations ]
            else
                []

        followedCategories =
            if userFollowsList nostr loginStatus /= [] then
                [ followedCategory translations ]
            else
                []
    in
    followedCategories ++ paretoCategories ++ -- paretoRssCategories ++
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

paretoRssCategory : I18Next.Translations -> Components.Categories.CategoryData Category
paretoRssCategory translations =
    { category = Rss
    , title = Translations.Read.rssFeedCategory [ translations ]
    }

followedCategory : I18Next.Translations -> Components.Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.Read.followedFeedCategory [ translations ]
    }

view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    { title = Translations.Sidebar.readMenuItemText [shared.browserEnv.translations]
    , body = [                                  {- Main Content -}
            Components.Categories.new
                { model = model.categories
                , toMsg = CategoriesSent
                , onSelect = CategorySelected
                , categories = availableCategories shared.nostr shared.loginStatus shared.browserEnv.translations
                , browserEnv = shared.browserEnv
                , styles = styles
                }
                |> Components.Categories.view
            , viewContent shared model userPubKey
            ]
    }

viewContent : Shared.Model -> Model -> Maybe PubKey -> Html Msg
viewContent shared model userPubKey =
    let
        viewArticles =
            Nostr.getArticlesByDate shared.nostr
                |> Ui.View.viewArticlePreviews
                    ArticlePreviewList
                        { theme = shared.theme
                        , browserEnv = shared.browserEnv
                        , nostr = shared.nostr
                        , userPubKey = userPubKey
                        , onBookmark = Maybe.map (\pubKey -> (AddArticleBookmark pubKey, RemoveArticleBookmark pubKey)) userPubKey
                        , onReaction = Maybe.map (\pubKey -> AddArticleReaction pubKey) userPubKey
                        , onZap = Nothing
                        }

        viewNotes =
            Nostr.getShortNotes shared.nostr
            |> viewShortNotes
                { theme = shared.theme
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , userPubKey = userPubKey
                , onBookmark = Maybe.map (\pubKey -> (AddShortNoteBookmark pubKey, RemoveShortNoteBookmark pubKey)) userPubKey
                }

    in
    case Components.Categories.selected model.categories of
        Global ->
            viewArticles
            
        Pareto ->
            viewArticles
            
        Followed ->
            viewArticles
            
        Highlighter ->
            viewArticles
            
        Rss ->
            viewNotes

viewShortNotes : ShortNotesViewData Msg -> List ShortNote -> Html Msg
viewShortNotes shortNotesViewData shortNotes =
    shortNotes
    |> List.map (\shortNote ->
        ShortNote.viewShortNote
            shortNotesViewData
            { author = Nostr.getAuthor shortNotesViewData.nostr shortNote.pubKey
            , actions =
                { addBookmark = Nothing
                , removeBookmark = Nothing
                , addReaction = Nothing
                , removeReaction = Nothing
                }
            , interactions = 
                { zaps = Nothing
                , highlights = Nothing
                , reactions = Nothing
                , reposts = Nothing
                , notes = Nothing
                , bookmarks = Nothing
                , isBookmarked = False
                , reaction = Nothing
                }
            }
            shortNote
    )
    |> div []