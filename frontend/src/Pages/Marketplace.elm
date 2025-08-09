module Pages.Marketplace exposing (Model, Msg, page)

import Components.BookmarkButton as BookmarkButton
import Components.Categories as Categories
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled exposing (Html, text)
import I18Next
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.FollowList exposing (followingPubKey)
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (EventId, LoginStatus, PubKey, loggedInPubKey)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Theme exposing (Color)
import Translations.Read
import Translations.Sidebar
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
        , RequestMarketplaceServices [ filterForCategory shared correctedCategory ]
            |> Nostr.createRequest shared.nostr "Marketplace services" [ KindUserMetadata, KindEventDeletionRequest ]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        , signUpEffect
        ]
    )


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


filterForCategory : Shared.Model -> Category -> EventFilter
filterForCategory shared category =
    case category of
        Global ->
            { emptyEventFilter | kinds = Just [ KindSatshootService ], limit = Just 20 }

        Pareto ->
            { emptyEventFilter | kinds = Just [ KindSatshootService ], authors = Just (paretoFollowsList shared.nostr), limit = Just 20 }

        Friedenstaube ->
            { emptyEventFilter
                | kinds = Just [ KindSatshootService ]
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
            { emptyEventFilter | kinds = Just [ KindSatshootService ], authors = Just (userFollowsList shared.nostr shared.loginStatus), limit = Just 20 }


paretoFollowsList : Nostr.Model -> List PubKey
paretoFollowsList nostr =
    case Nostr.getFollowsList nostr Pareto.authorsKey of
        Just followsList ->
            followsList
                |> List.filterMap followingPubKey

        Nothing ->
            []



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
            ( model
            , Effect.none
            )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    model.bookmarkButtons
        |> Dict.toList
        |> List.map (\( eventId, bookmarkButton ) -> BookmarkButton.subscriptions bookmarkButton |> Sub.map (BookmarkButtonMsg eventId))
        |> Sub.batch



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        userPubKey =
            loggedInPubKey shared.loginStatus
    in
    { title = Translations.Sidebar.readMenuItemText [ shared.browserEnv.translations ]
    , body = [ viewContent shared model userPubKey ]
    }


viewContent : Shared.Model.Model -> Model -> Maybe PubKey -> Html Msg
viewContent shared model maybePubkey =
    text "Marketplace of ideas"


categoryImage : Color -> Category -> Maybe (Html msg)
categoryImage color category =
    let
        iconColor =
            Icon.TailwindColor color
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


followedCategory : I18Next.Translations -> Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.Read.followedFeedCategory [ translations ]
    }
