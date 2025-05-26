module Pages.Read exposing (Model, Msg, init, page, subscriptions, update, view)

import Components.BookmarkButton as BookmarkButton
import Components.Categories as Categories
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attr exposing (css)
import I18Next
import Layouts
import Layouts.Sidebar
import Material.Icons exposing (category)
import Nostr
import Nostr.Event exposing (AddressComponents, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.FollowList exposing (followingPubKey)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (EventId, LoginStatus, PubKey, loggedInPubKey)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Tailwind.Theme exposing (Color)
import Translations.Read
import Translations.Sidebar
import Ui.ShortNote as ShortNote exposing (ShortNotesViewData)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
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
            , image = categoryImage
            , categories = availableCategories shared.nostr shared.loginStatus shared.browserEnv.translations
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
    , bookmarkButtons : Dict EventId BookmarkButton.Model
    }


type Category
    = Global
    | Pareto
    | Friedenstaube
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

        Friedenstaube ->
            "friedenstaube"

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

        "friedenstaube" ->
            Just Friedenstaube

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
                |> Maybe.withDefault Pareto

        signUpEffect =
            if route.hash == Just "signup" then
                Ports.signUp
                    |> Effect.sendCmd

            else
                Effect.none

        correctedCategory =
            case category of
                Pareto ->
                    -- a fixed authors list will be used for bootstrapping so Pareto can be the initial category
                    category

                Followed ->
                    if userFollowsList shared.nostr shared.loginStatus == [] then
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
    ( { categories = Categories.init { selected = correctedCategory }
      , path = route.path
      , bookmarkButtons = Dict.empty
      }
    , Effect.batch
        [ changeCategoryEffect
        , RequestArticlesFeed [ filterForCategory shared correctedCategory ]
            |> Nostr.createRequest shared.nostr "Long-form articles" [ KindUserMetadata, KindEventDeletionRequest ]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        , signUpEffect
        ]
    )



-- UPDATE


type Msg
    = CategorySelected Category
    | CategoriesSent (Categories.Msg Category Msg)
    | BookmarkButtonMsg EventId BookmarkButton.Msg
    | NoOp

update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        CategorySelected category ->
            updateModelWithCategory shared model category

        CategoriesSent innerMsg ->
            Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
                , toMsg = CategoriesSent
                }

        BookmarkButtonMsg eventId innerMsg ->
            BookmarkButton.update
                { msg = innerMsg
                , model = Dict.get eventId model.bookmarkButtons
                , nostr = shared.nostr
                , toModel = \bookmarkButton -> { model | bookmarkButtons = Dict.insert eventId bookmarkButton model.bookmarkButtons }
                , toMsg = BookmarkButtonMsg eventId
                , translations = shared.browserEnv.translations
                }

        NoOp ->
            ( model, Effect.none )


updateModelWithCategory : Shared.Model -> Model -> Category -> ( Model, Effect Msg )
updateModelWithCategory shared model category =
    ( model
    , Effect.batch
        [ Effect.replaceRoute { path = model.path, query = Dict.singleton categoryParamName (stringFromCategory category), hash = Nothing }
        , RequestArticlesFeed [ filterForCategory shared category ]
            |> Nostr.createRequest shared.nostr "Long-form articles" [ KindUserMetadata, KindEventDeletionRequest ]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        ]
    )


filterForCategory : Shared.Model -> Category -> EventFilter
filterForCategory shared category =
    case category of
        Global ->
            { emptyEventFilter | kinds = Just [ KindLongFormContent ], limit = Just 20 }

        Pareto ->
            { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just (paretoFollowsList shared.nostr), limit = Just 20 }

        Friedenstaube ->
            { emptyEventFilter
                | kinds = Just [ KindLongFormContent ]
                , authors = Just (paretoFollowsList shared.nostr)
                , tagReferences =
                    Just
                        [ TagReferenceTag "Frieden"
                        , TagReferenceTag "frieden"
                        , TagReferenceTag "Friedenstaube"
                        , TagReferenceTag "friedenstaube"
                        ]
                , limit = Just 20
            }

        Followed ->
            { emptyEventFilter | kinds = Just [ KindLongFormContent ], authors = Just (userFollowsList shared.nostr shared.loginStatus), limit = Just 20 }

        Highlighter ->
            { emptyEventFilter | kinds = Just [ KindLongFormContent ], limit = Just 20 }

        Rss ->
            { emptyEventFilter | kinds = Just [ KindShortTextNote ], authors = Just (paretoRssFollowsList shared.nostr), limit = Just 20 }


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


userFollowsList : Nostr.Model -> LoginStatus -> List PubKey
userFollowsList nostr loginStatus =
    case loggedInPubKey loginStatus of
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
    model.bookmarkButtons
    |> Dict.toList
    |> List.map (\(eventId, bookmarkButton) -> BookmarkButton.subscriptions bookmarkButton |> Sub.map (BookmarkButtonMsg eventId))
    |> Sub.batch


-- VIEW


availableCategories : Nostr.Model -> LoginStatus -> I18Next.Translations -> List (Categories.CategoryData Category)
availableCategories nostr loginStatus translations =
    let
        paretoCategories =
            [ paretoCategory translations ]

        friedenstaubeCategories =
            [ friedenstaubeCategory translations ]

        {-
           paretoRssCategories =
               if paretoRssFollowsList nostr /= [] then
                   [ paretoRssCategory translations ]

               else
                   []
        -}
        followedCategories =
            if userFollowsList nostr loginStatus /= [] then
                [ followedCategory translations ]

            else
                []
    in
    paretoCategories
        ++ friedenstaubeCategories
        ++ followedCategories
        ++ [ { category = Global
             , title = Translations.Read.globalFeedCategory [ translations ]
             }

           --   , { category = Highlighter
           --     , title = Translations.Read.highlighterFeedCategory [ translations ]
           --     }
           ]


paretoCategory : I18Next.Translations -> Categories.CategoryData Category
paretoCategory translations =
    { category = Pareto
    , title = Translations.Read.paretoFeedCategory [ translations ]
    }


friedenstaubeCategory : I18Next.Translations -> Categories.CategoryData Category
friedenstaubeCategory _ =
    { category = Friedenstaube
    , title = "Friedenstaube"
    }



{-
   paretoRssCategory : I18Next.Translations -> Categories.CategoryData Category
   paretoRssCategory translations =
       { category = Rss
       , title = Translations.Read.rssFeedCategory [ translations ]
       }
-}


followedCategory : I18Next.Translations -> Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.Read.followedFeedCategory [ translations ]
    }


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        userPubKey =
            loggedInPubKey shared.loginStatus
    in
    { title = Translations.Sidebar.readMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ viewContent shared model userPubKey
        ]
    }


categoryImage : Color -> Category -> Maybe (Html msg)
categoryImage color category =
    let
        iconColor =
            Icon.TailwindColor color

        image src =
            Html.div
                [ css
                    [ Tw.w_4
                    , Tw.h_4
                    ]
                ]
                [ Html.img
                    [ Attr.src src
                    ]
                    []
                ]
    in
    case category of
        Pareto ->
            Icon.ParetoIcon Icon.ParetoCube 16 iconColor
                |> Icon.view
                |> Just

        Friedenstaube ->
            Icon.ParetoIcon Icon.ParetoPeaceDove 16 iconColor
                |> Icon.view
                |> Just

        Followed ->
            Icon.ParetoIcon Icon.ParetoFollowed 16 iconColor
                |> Icon.view
                |> Just

        Global ->
            Icon.ParetoIcon Icon.ParetoGlobe 16 iconColor
                |> Icon.view
                |> Just

        _ ->
            Just <| image "/images/icon/Pareto-Log2.png"


viewContent : Shared.Model -> Model -> Maybe PubKey -> Html Msg
viewContent shared model userPubKey =
    let
        viewArticles =
            Nostr.getArticlesByDate shared.nostr
                |> Ui.View.viewArticlePreviews
                    ArticlePreviewList
                    { theme = shared.theme
                    , bookmarkButtonMsg = BookmarkButtonMsg
                    , bookmarkButtons = model.bookmarkButtons
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , loginStatus = shared.loginStatus
                    , commenting = Nothing
                    , articleToInteractionsMsg = \_ _ -> NoOp
                    , articleCommentInteractions = Dict.empty
                    , commentsToInteractionsMsg = \_ _ -> NoOp
                    , openCommentMsg = Nothing
                    , sharing = Nothing
                    }

        viewNotes =
            Nostr.getShortNotes shared.nostr
                |> viewShortNotes
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , userPubKey = userPubKey
                    }
    in
    case Categories.selected model.categories of
        Global ->
            viewArticles

        Pareto ->
            viewArticles

        Friedenstaube ->
            viewArticles

        Followed ->
            viewArticles

        Highlighter ->
            viewArticles

        Rss ->
            viewNotes


viewShortNotes : ShortNotesViewData -> List ShortNote -> Html Msg
viewShortNotes shortNotesViewData shortNotes =
    shortNotes
        |> List.map
            (\shortNote ->
                ShortNote.viewShortNote
                    shortNotesViewData
                    { author = Nostr.getAuthor shortNotesViewData.nostr shortNote.pubKey
                    , interactions =
                        { zaps = Nothing
                        , articleComments = []
                        , articleCommentComments = Dict.empty
                        , highlights = Nothing
                        , reactions = Nothing
                        , reposts = Nothing
                        , notes = Nothing
                        , bookmarks = Nothing
                        , isBookmarked = False
                        , reaction = Nothing
                        , repost = Nothing
                        }
                    }
                    shortNote
            )
        |> div []
