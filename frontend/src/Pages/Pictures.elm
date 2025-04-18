module Pages.Pictures exposing (Model, Msg, init, page, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Categories
import Components.Icon as Icon
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attr exposing (css)
import I18Next
import Layouts
import Material.Icons exposing (category)
import Nostr
import Nostr.Event exposing (AddressComponents, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.FollowList exposing (followingPubKey)
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, PubKey)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.Read
import Translations.Sidebar
import Ui.PicturePost as PicturePost exposing (PicturePostsViewData)
import Ui.Styles exposing (Theme)
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
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
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
    | Memes


stringFromCategory : Category -> String
stringFromCategory category =
    case category of
        Global ->
            "global"

        Pareto ->
            "pareto"

        Followed ->
            "followed"

        Memes ->
            "memes"


categoryFromString : String -> Maybe Category
categoryFromString categoryString =
    case categoryString of
        "global" ->
            Just Global

        "pareto" ->
            Just Pareto

        "followed" ->
            Just Followed

        "memes" ->
            Just Memes

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
    ( { categories = Components.Categories.init { selected = correctedCategory }
      , path = route.path
      }
    , Effect.batch
        [ changeCategoryEffect
        , RequestArticlesFeed [ filterForCategory shared correctedCategory ]
            |> Nostr.createRequest shared.nostr "Picture posts" [ KindUserMetadata, KindEventDeletionRequest ]
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        , signUpEffect
        ]
    )



-- UPDATE


type Msg
    = CategorySelected Category
    | CategoriesSent (Components.Categories.Msg Category Msg)
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | RemoveArticleReaction PubKey EventId -- event ID of like
    | AddShortNoteBookmark PubKey EventId
    | RemoveShortNoteBookmark PubKey EventId


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        CategorySelected category ->
            updateModelWithCategory shared model category

        CategoriesSent innerMsg ->
            Components.Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
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

        RemoveArticleReaction _ _ ->
            -- TODO: Delete like
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
            { emptyEventFilter | kinds = Just [ KindPicture ], limit = Just 20 }

        Pareto ->
            { emptyEventFilter | kinds = Just [ KindPicture ], authors = Just (paretoFollowsList shared.nostr), limit = Just 20 }

        Followed ->
            { emptyEventFilter | kinds = Just [ KindPicture ], authors = Just (userFollowsList shared.nostr shared.loginStatus), limit = Just 20 }

        Memes ->
            { emptyEventFilter
                | kinds = Just [ KindPicture ]

                {- , authors = Just (paretoFollowsList shared.nostr) -}
                , limit = Just 20
            }


paretoFollowsList : Nostr.Model -> List PubKey
paretoFollowsList nostr =
    case Nostr.getFollowsList nostr Pareto.authorsKey of
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
subscriptions _ =
    Sub.none



-- VIEW


availableCategories : Nostr.Model -> Shared.Model.LoginStatus -> I18Next.Translations -> List (Components.Categories.CategoryData Category)
availableCategories nostr loginStatus translations =
    let
        paretoCategories =
            [ paretoCategory translations ]

        followedCategories =
            if userFollowsList nostr loginStatus /= [] then
                [ followedCategory translations ]

            else
                []

        isBetaTester =
            Shared.loggedInPubKey loginStatus
                |> Maybe.map (Nostr.isBetaTester nostr)
                |> Maybe.withDefault False

        memesCategories =
            if isBetaTester then
                [ memesCategory translations ]

            else
                []
    in
    paretoCategories
        ++ followedCategories
        ++ memesCategories
        ++ [ { category = Global
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



{-
   paretoRssCategory : I18Next.Translations -> Components.Categories.CategoryData Category
   paretoRssCategory translations =
       { category = Rss
       , title = Translations.Read.rssFeedCategory [ translations ]
       }
-}


followedCategory : I18Next.Translations -> Components.Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.Read.followedFeedCategory [ translations ]
    }


memesCategory : I18Next.Translations -> Components.Categories.CategoryData Category
memesCategory translations =
    { category = Memes
    , title = Translations.Read.memesFeedCategory [ translations ]
    }


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    { title = Translations.Sidebar.readMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ {- Main Content -}
          Components.Categories.new
            { model = model.categories
            , toMsg = CategoriesSent
            , onSelect = CategorySelected
            , equals = \category1 category2 -> category1 == category2
            , image = categoryImage shared.browserEnv
            , categories = availableCategories shared.nostr shared.loginStatus shared.browserEnv.translations
            , browserEnv = shared.browserEnv
            , styles = styles
            }
            |> Components.Categories.view
        , viewContent shared model userPubKey
        ]
    }


categoryImage : BrowserEnv -> Category -> Maybe (Html msg)
categoryImage _ category =
    let
        iconColor =
            Icon.Color <| Color.fromRgba { red = 0.392, green = 0.455, blue = 0.545, alpha = 1.0 }

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
viewContent shared model _ =
    let
        viewMemes =
            Nostr.getPicturePosts shared.nostr
                |> viewPicturePosts
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    }
    in
    case Components.Categories.selected model.categories of
        Global ->
            viewMemes

        Pareto ->
            viewMemes

        Followed ->
            viewMemes

        Memes ->
            viewMemes


viewPicturePosts : PicturePostsViewData -> List PicturePost -> Html Msg
viewPicturePosts picturePostsViewData picturePosts =
    picturePosts
        |> List.map
            (\picturePost ->
                PicturePost.viewPicturePost
                    picturePostsViewData
                    {}
                    picturePost
            )
        |> div []
