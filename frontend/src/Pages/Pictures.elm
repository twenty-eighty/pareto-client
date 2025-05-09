module Pages.Pictures exposing (Model, Msg, init, page, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Button as Button
import Components.Categories
import Components.Icon as Icon
import Components.PicturePostDialog as PicturePostDialog
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Lazy as Lazy
import I18Next
import Layouts
import Layouts.Sidebar
import Material.Icons exposing (category)
import Nostr
import Nostr.Event exposing (AddressComponents, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.FollowList exposing (followingPubKey)
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.ShortNote exposing (ShortNote)
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
import Translations.Pictures as Translations
import Ui.PicturePost as PicturePost exposing (PicturePostsViewData)
import Ui.Shared exposing (emptyHtml)
import Ui.ShortNote exposing (ShortNotesViewData)
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
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    let
        postButton =
            Shared.loggedInSigningPubKey shared.loginStatus
                |> Maybe.map (\pubKey ->
                    if Nostr.isBetaTester shared.nostr pubKey then
                        Html.div
                            [ css
                                [ Tw.flex
                                , Tw.mx_8
                                , Tw.my_4
                                ]
                            ]
                            [ Button.new
                                { theme = shared.theme
                                , onClick = Just (ShowPicturePostDialog pubKey)
                                , label = Translations.newPicturePostButtonLabel [ shared.browserEnv.translations ]
                                }
                                |> Button.view
                            ]
                    else
                        emptyHtml
                )
                |> Maybe.withDefault emptyHtml
        topPart =
            Html.div
                []
                [ postButton
                ,  Components.Categories.new
                    { model = model.categories
                    , toMsg = CategoriesSent
                    , onSelect = CategorySelected
                    , equals = \category1 category2 -> category1 == category2
                    , image = categoryImage shared.browserEnv
                    , categories = availableCategories shared.nostr shared.loginStatus shared.browserEnv.translations
                    , browserEnv = shared.browserEnv
                    , theme = shared.theme
                    }
                    |> Components.Categories.view
                ]
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withTopPart topPart
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { categories : Components.Categories.Model Category
    , path : Route.Path.Path
    , picturePostDialog : Maybe PicturePostDialog.Model
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
      , picturePostDialog = Nothing
      }
    , Effect.batch
        [ changeCategoryEffect
        , RequestPicturesFeed [ filterForCategory shared correctedCategory ]
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
    | ShowPicturePostDialog PubKey
    | PicturePostDialogMsg PicturePostDialog.Msg

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

        ShowPicturePostDialog pubKey ->
            let
                ( picturePostDialog, effect ) =
                    PicturePostDialog.init { nostr = shared.nostr, pubKey = pubKey, language = shared.browserEnv.language }
            in
            ( { model | picturePostDialog = Just <| PicturePostDialog.show picturePostDialog }
            , Effect.map PicturePostDialogMsg effect
            )


        PicturePostDialogMsg innerMsg ->
            model.picturePostDialog
                |> Maybe.map (\picturePostDialog ->
                    PicturePostDialog.update
                        { msg = innerMsg
                        , model = picturePostDialog
                        , toModel = \dDialog -> { model | picturePostDialog = Just dDialog }
                        , toMsg = PicturePostDialogMsg
                        , nostr = shared.nostr
                        , browserEnv = shared.browserEnv
                        }
                    )
                |> Maybe.withDefault ( model, Effect.none )

updateModelWithCategory : Shared.Model -> Model -> Category -> ( Model, Effect Msg )
updateModelWithCategory shared model category =
    ( model
    , Effect.batch
        [ Effect.replaceRoute { path = model.path, query = Dict.singleton categoryParamName (stringFromCategory category), hash = Nothing }
        , RequestPicturesFeed [ filterForCategory shared category ]
            |> Nostr.createRequest shared.nostr "Picture posts" [ KindUserMetadata, KindEventDeletionRequest ]
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
                | kinds = Just [ KindShortTextNote ]
                , authors = Just [ "a6fdf45b4921d5bfe9d48d2a03d4e71b7340d8166a9da83dae2896239145f104" ]
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
subscriptions model =
    model.picturePostDialog
        |> Maybe.map (\picturePostDialog ->
            PicturePostDialog.subscriptions picturePostDialog
                |> Sub.map PicturePostDialogMsg
        )
        |> Maybe.withDefault Sub.none



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

        _ =
            if isBetaTester then
                [ memesCategory translations ]

            else
                []
    in
    paretoCategories
        ++ followedCategories
        -- ++ memesCategories
        {- apparently the content warning field is not consistently filled, so we don't show the global category
        ++ [ { category = Global
             , title = Translations.Read.globalFeedCategory [ translations ]
             }
           ]
        -}


paretoCategory : I18Next.Translations -> Components.Categories.CategoryData Category
paretoCategory translations =
    { category = Pareto
    , title = Translations.paretoFeedCategory [ translations ]
    }


followedCategory : I18Next.Translations -> Components.Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.followedFeedCategory [ translations ]
    }


memesCategory : I18Next.Translations -> Components.Categories.CategoryData Category
memesCategory translations =
    { category = Memes
    , title = Translations.memesFeedCategory [ translations ]
    }


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ viewContent shared model userPubKey
        , model.picturePostDialog
            |> Maybe.map (\picturePostDialog ->
                PicturePostDialog.new 
                    { model = picturePostDialog
                    , toMsg = PicturePostDialogMsg
                    , nostr = shared.nostr
                    , loginStatus = shared.loginStatus
                    , browserEnv = shared.browserEnv
                    , theme = shared.theme
                    }
                    |> PicturePostDialog.view
            )
            |> Maybe.withDefault emptyHtml
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
                |> List.filter filterPostsWithContentWarning
                |> viewPicturePosts
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    }

        viewShortTextNotes =
            Nostr.getShortNotes shared.nostr
                |> viewShortNotes
                    shared
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , userPubKey = Nothing
                    , onBookmark = Nothing
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
            viewShortTextNotes


filterPostsWithContentWarning : PicturePost -> Bool
filterPostsWithContentWarning picturePost =
    picturePost.contentWarning == Nothing


viewPicturePosts : PicturePostsViewData -> List PicturePost -> Html Msg
viewPicturePosts picturePostsViewData picturePosts =
    picturePosts
        |> List.map
            (\picturePost ->
                Lazy.lazy2
                    (PicturePost.viewPicturePost picturePostsViewData)
                    { author = Nostr.getAuthor picturePostsViewData.nostr picturePost.pubKey
                    }
                    picturePost
            )
        |> Html.div
            [ css
                [ Tw.px_4
                ]
            ]


viewShortNotes : Shared.Model -> ShortNotesViewData msg -> List ShortNote -> Html msg
viewShortNotes shared shortNotesViewData shortNotes =
    shortNotes
        |> List.map
            (\shortNote ->
                Ui.ShortNote.viewShortNote
                    shortNotesViewData
                    { author = Nostr.getAuthor shared.nostr shortNote.pubKey
                    , actions =
                        { addBookmark = Nothing
                        , removeBookmark = Nothing
                        , addReaction = Nothing
                        , removeReaction = Nothing
                        , addRepost = Nothing
                        , startComment = Nothing
                        }
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
        |> Html.div
            [ css
                [ Tw.px_4
                ]
            ]
