module Pages.Pictures exposing (Model, Msg, init, page, subscriptions, update, view)

import Components.Button as Button
import Components.Categories as Categories
import Components.Icon as Icon
import Components.PicturePostDialog as PicturePostDialog
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Lazy as Lazy
import I18Next
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event exposing (AddressComponents, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.FollowList exposing (followingPubKey)
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (EventId, LoginStatus, PubKey, loggedInPubKey, loggedInSigningPubKey)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Tailwind.Color exposing (Color)
import Translations.Pictures as Translations
import Ui.PicturePost as PicturePost exposing (PicturePostsViewData)
import Ui.ShortNote exposing (ShortNotesViewData)
import Ui.Shared exposing (emptyHtml)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    let
        postButton =
            loggedInSigningPubKey shared.loginStatus
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
                ,  Categories.new
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
                ]
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withTopPart topPart ("(" ++ Categories.heightString ++ " + " ++ Button.heightString ++ ")")
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { categories : Categories.Model Category
    , path : Route.Path.Path
    , picturePostDialog : Maybe PicturePostDialog.Model
    , interactions : Dict.Dict EventId Interactions.Model
    }


type Category
    = Global
    | Pareto
    | Followed
    | Memes
    | Art


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

        Art ->
            "art"

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

        "art" ->
            Just Art

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
                        Pareto

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
      , picturePostDialog = Nothing
      , interactions = Dict.empty
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
    | CategoriesSent (Categories.Msg Category Msg)
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | RemoveArticleReaction PubKey EventId -- event ID of like
    | AddShortNoteBookmark PubKey EventId
    | RemoveShortNoteBookmark PubKey EventId
    | ShowPicturePostDialog PubKey
    | PicturePostDialogMsg PicturePostDialog.Msg
    | InteractionsSent EventId PubKey Interactions.Msg


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
            , SendReaction userPubKey eventId articlePubKey (Just addressComponents)
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
                    PicturePostDialog.init { nostr = shared.nostr, pubKey = pubKey, language = shared.browserEnv.language, category = postDialogCategory model }
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

        InteractionsSent eventId pubKey interactionsMsg ->
            Interactions.update
                { msg = interactionsMsg
                , model = Dict.get eventId model.interactions
                , nostr = shared.nostr
                , interactionObject = InteractionButton.PicturePost eventId pubKey
                , toModel = \interactionsModel -> { model | interactions = Dict.insert eventId interactionsModel model.interactions }
                , toMsg = InteractionsSent eventId pubKey
                , translations = shared.browserEnv.translations
                }

postDialogCategory : Model -> Maybe PicturePostDialog.PostCategory
postDialogCategory model =
    case Categories.selected model.categories of
        Memes ->
            Just PicturePostDialog.Meme

        Art ->
            Just PicturePostDialog.Art

        _ ->
            Nothing

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
    let
        paretoAuthors =
            paretoFollowsList shared.nostr
    in
    case category of
        Global ->
            { emptyEventFilter | kinds = Just [ KindPicture ], limit = Just 20 }

        Pareto ->
            { emptyEventFilter | kinds = Just [ KindPicture ], authors = Just (paretoAuthors), limit = Just 20 }

        Followed ->
            { emptyEventFilter | kinds = Just [ KindPicture ], authors = Just (userFollowsList shared.nostr shared.loginStatus), limit = Just 20 }

        Memes ->
            { emptyEventFilter
                | kinds = Just [ KindPicture ]
                , authors = Just paretoAuthors
                , tagReferences = Just [ TagReferenceTag (PicturePostDialog.categoryToHashtag PicturePostDialog.Meme) ]
                , limit = Just 20
            }

        Art ->
            { emptyEventFilter
                | kinds = Just [ KindPicture ]
                , authors = Just paretoAuthors
                , tagReferences = Just [ TagReferenceTag (PicturePostDialog.categoryToHashtag PicturePostDialog.Art) ]
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


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Sub.batch
        [ model.picturePostDialog
            |> Maybe.map (\picturePostDialog ->
                PicturePostDialog.subscriptions picturePostDialog
                    |> Sub.map PicturePostDialogMsg
            )
            |> Maybe.withDefault Sub.none
        , Dict.toList model.interactions
            |> List.map (\(eventId, interactions) ->
                let
                    maybePubKey =
                        Nostr.getPicturePosts shared.nostr
                            |> List.filter (\picturePost -> picturePost.id == eventId)
                            |> List.head
                            |> Maybe.map (\picturePost -> picturePost.pubKey)
                in
                case maybePubKey of
                    Just pubKey ->
                        Interactions.subscriptions interactions
                            |> Sub.map (InteractionsSent eventId pubKey)

                    Nothing ->
                        Sub.none
            )
            |> Sub.batch
        ]



-- VIEW


availableCategories : Nostr.Model -> LoginStatus -> I18Next.Translations -> List (Categories.CategoryData Category)
availableCategories nostr loginStatus translations =
    let
        paretoCategories =
            [ paretoCategory translations ]

        followedCategories =
            if userFollowsList nostr loginStatus /= [] then
                [ followedCategory translations ]

            else
                []

        memesCategories =
            [ memesCategory translations ]

        artCategories =
            [ artCategory translations ]
    in
    paretoCategories
        ++ followedCategories
        ++ memesCategories
        ++ artCategories
        {- apparently the content warning field is not consistently filled, so we don't show the global category
        ++ [ { category = Global
             , title = Translations.Read.globalFeedCategory [ translations ]
             }
           ]
        -}


paretoCategory : I18Next.Translations -> Categories.CategoryData Category
paretoCategory translations =
    { category = Pareto
    , title = Translations.paretoFeedCategory [ translations ]
    }


followedCategory : I18Next.Translations -> Categories.CategoryData Category
followedCategory translations =
    { category = Followed
    , title = Translations.followedFeedCategory [ translations ]
    }


memesCategory : I18Next.Translations -> Categories.CategoryData Category
memesCategory translations =
    { category = Memes
    , title = Translations.memesFeedCategory [ translations ]
    }


artCategory : I18Next.Translations -> Categories.CategoryData Category
artCategory translations =
    { category = Art
    , title = Translations.artFeedCategory [ translations ]
    }


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        userPubKey =
            loggedInPubKey shared.loginStatus
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


categoryImage : Color -> Category -> Maybe (Html msg)
categoryImage iconColor category =
    case category of
        Pareto ->
            Icon.ParetoIcon Icon.ParetoCube 16 (Icon.TailwindColor iconColor)
                |> Icon.view
                |> Just

        Followed ->
            Icon.ParetoIcon Icon.ParetoFollowed 16 (Icon.TailwindColor iconColor)
                |> Icon.view
                |> Just

        Global ->
            Icon.ParetoIcon Icon.ParetoGlobe 16 (Icon.TailwindColor iconColor)
                |> Icon.view
                |> Just

        Memes ->
            Icon.ParetoIcon Icon.ParetoMemes 16 (Icon.TailwindColor iconColor)
                |> Icon.view
                |> Just

        Art ->
            Icon.ParetoIcon Icon.ParetoArt 16 (Icon.TailwindColor iconColor)
                |> Icon.view
                |> Just


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
                    , interactions = model.interactions
                    , loginStatus = shared.loginStatus
                    }

        _ =
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
    case Categories.selected model.categories of
        Global ->
            viewMemes

        Pareto ->
            viewMemes

        Followed ->
            viewMemes

        Memes ->
            viewMemes

        Art ->
            viewMemes


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
                    , toInteractionsMsg = InteractionsSent picturePost.id picturePost.pubKey
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
