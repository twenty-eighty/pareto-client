module Pages.Bookmarks exposing (Model, Msg, page)

import Auth
import BrowserEnv
import Components.ArticleComments as ArticleComments
import Components.ArticleHighlights as ArticleHighlights
import Components.BookmarkButton as BookmarkButton
import Components.Categories as Categories
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, blockquote, div, span, text)
import Html.Styled.Attributes exposing (css, href)
import I18Next
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.BookmarkList exposing (BookmarkList, BookmarkType(..), bookmarkListFromEvent, bookmarksCount, emptyBookmarkList)
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.External
import Nostr.Highlights as Highlights exposing (Highlight)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.Bookmarks as Translations
import Ui.Article exposing (linkToArticle)
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (stylesForTheme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared user route
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout user shared)
        |> Page.withOnQueryParameterChanged
            { key = categoryParamName
            , onChange = CategoryQueryChanged
            }


toLayout : Auth.User -> Shared.Model -> Model -> Layouts.Layout Msg
toLayout user shared model =
    let
        bookmarkList =
            Nostr.getBookmarks shared.nostr user.pubKey
                |> Maybe.withDefault emptyBookmarkList

        topPart =
            Categories.new
                { model = model.categories
                , toMsg = CategoriesSent
                , onSelect = CategorySelected
                , equals = \category1 category2 -> category1 == category2
                , image = \_ _ -> Nothing
                , categories = availableCategories bookmarkList (Nostr.highlightsByAuthor shared.nostr user.pubKey) shared.browserEnv.translations
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
    { bookmarkButtons : Dict EventId BookmarkButton.Model
    , categories : Categories.Model BookmarkType
    , path : Route.Path.Path
    , selectedBookmarkType : BookmarkType
    }


categoryParamName : String
categoryParamName =
    "category"


stringFromCategory : BookmarkType -> String
stringFromCategory bookmarkType =
    case bookmarkType of
        ArticleBookmark ->
            "articles"

        HighlightBookmark ->
            "highlights"

        NoteBookmark ->
            "notes"


categoryFromString : String -> Maybe BookmarkType
categoryFromString categoryString =
    case categoryString of
        "articles" ->
            Just ArticleBookmark

        "highlights" ->
            Just HighlightBookmark

        "notes" ->
            Just NoteBookmark

        _ ->
            Nothing


categoryFromQuery : Maybe String -> BookmarkType
categoryFromQuery maybeCategory =
    maybeCategory
        |> Maybe.andThen categoryFromString
        |> Maybe.withDefault ArticleBookmark


init : Shared.Model -> Auth.User -> Route () -> () -> ( Model, Effect Msg )
init shared user route () =
    let
        bookmarkType =
            Dict.get categoryParamName route.query
                |> categoryFromQuery

        contentRequest =
            Nostr.getBookmarks shared.nostr user.pubKey
                |> Maybe.map (requestForBookmarkContent shared.nostr ArticleBookmark)
                |> Maybe.withDefault Effect.none
    in
    ( { bookmarkButtons = Dict.empty
      , categories = Categories.init { selected = bookmarkType }
      , path = route.path
      , selectedBookmarkType = bookmarkType
      }
    , Effect.batch
        [ replaceCategoryRoute route.path bookmarkType
        , contentRequest
        , requestMyHighlights shared.nostr user.pubKey
        , requestForSelectedBookmark shared.nostr user.pubKey bookmarkType
        , Effect.scrollContentToTop
        ]
    )


replaceCategoryRoute : Route.Path.Path -> BookmarkType -> Effect msg
replaceCategoryRoute path bookmarkType =
    Effect.replaceRoute
        { path = path
        , query = Dict.singleton categoryParamName (stringFromCategory bookmarkType)
        , hash = Nothing
        }


requestMyHighlights : Nostr.Model -> Nostr.Types.PubKey -> Effect msg
requestMyHighlights nostr pubKey =
    { emptyEventFilter
        | authors = Just [ pubKey ]
        , kinds = Just [ KindHighlights ]
        , limit = Just 100
    }
        |> RequestBookmarks
        |> Nostr.createRequest nostr "My highlights" [ KindUserMetadata ]
        |> Shared.Msg.RequestNostrEvents
        |> Effect.sendSharedMsg


requestForBookmarkContent : Nostr.Model -> BookmarkType -> BookmarkList -> Effect Msg
requestForBookmarkContent nostr bookmarkType bookmarkList =
    case bookmarkType of
        ArticleBookmark ->
            bookmarkList.articles
                |> List.filter
                    (\addressComponents ->
                        -- only request articles we don't have yet
                        Nostr.getArticle nostr addressComponents == Nothing
                    )
                |> List.map
                    (\( kind, pubKey, identifier ) ->
                        [ { emptyEventFilter
                            | authors = Just [ pubKey ]
                            , kinds = Just [ kind ]
                            , tagReferences = Just [ TagReferenceIdentifier identifier ]
                          }
                        ]
                            |> RequestArticlesFeed False
                            |> Nostr.createRequest nostr "Bookmark articles" [ KindUserMetadata ]
                            |> Shared.Msg.RequestNostrEvents
                            |> Effect.sendSharedMsg
                    )
                |> Effect.batch

        HighlightBookmark ->
            Effect.none

        NoteBookmark ->
            Effect.none


-- UPDATE


type Msg
    = ReceivedMessage IncomingMessage
    | BookmarkButtonMsg EventId BookmarkButton.Msg
    | CategoriesSent (Categories.Msg BookmarkType Msg)
    | CategorySelected BookmarkType
    | CategoryQueryChanged { from : Maybe String, to : Maybe String }
    | BookmarkRemoved
    | NoOp


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage user shared model message

        BookmarkButtonMsg eventId innerMsg ->
            BookmarkButton.update
                { msg = innerMsg
                , model = Dict.get eventId model.bookmarkButtons
                , nostr = shared.nostr
                , onRemoveMsg = Just BookmarkRemoved
                , toModel = \bookmarkButton -> { model | bookmarkButtons = Dict.insert eventId bookmarkButton model.bookmarkButtons }
                , toMsg = BookmarkButtonMsg eventId
                , translations = shared.browserEnv.translations
                }
        BookmarkRemoved ->
            let
                numberOfBookmarks =
                    Nostr.getBookmarks shared.nostr user.pubKey
                        |> Maybe.map bookmarksCount
                        |> Maybe.withDefault 0

                redirectForEmptyList =
                    if numberOfBookmarks <= 1 then
                        Effect.replaceRoute { hash = Nothing, path = Route.Path.Read, query = Dict.empty }

                    else
                        Effect.none
            in
            ( model , redirectForEmptyList)


        CategoriesSent innerMsg ->
            Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
                , toMsg = CategoriesSent
                }

        CategorySelected bookmarkType ->
            switchToCategory user shared model bookmarkType

        CategoryQueryChanged { to } ->
            switchToCategory user shared model (categoryFromQuery to)

        NoOp ->
            ( model, Effect.none )


switchToCategory : Auth.User -> Shared.Model -> Model -> BookmarkType -> ( Model, Effect Msg )
switchToCategory user shared model bookmarkType =
    if model.selectedBookmarkType == bookmarkType then
        ( model, Effect.none )

    else
        ( { model
            | categories = Categories.select model.categories bookmarkType
            , selectedBookmarkType = bookmarkType
          }
        , Effect.batch
            [ replaceCategoryRoute model.path bookmarkType
            , requestForSelectedBookmark shared.nostr user.pubKey bookmarkType
            ]
        )


requestForSelectedBookmark : Nostr.Model -> Nostr.Types.PubKey -> BookmarkType -> Effect Msg
requestForSelectedBookmark nostr pubKey bookmarkType =
    case bookmarkType of
        HighlightBookmark ->
            Nostr.highlightsByAuthor nostr pubKey
                |> List.filterMap .addressComponents
                |> requestArticles nostr

        ArticleBookmark ->
            Effect.none

        NoteBookmark ->
            Effect.none

updateWithMessage : Auth.User -> Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeEventsKind message.value of
                Ok KindHighlights ->
                    case Nostr.External.decodeEvents message.value of
                        Ok events ->
                            ( model
                            , events
                                |> List.map Highlights.fromEvent
                                |> List.filterMap .addressComponents
                                |> requestArticles shared.nostr
                            )

                        _ ->
                            ( model, Effect.none )

                Ok KindBookmarkList ->
                    case Nostr.External.decodeEvents message.value of
                        Ok events ->
                            let
                                requestEffect =
                                    events
                                        |> List.map bookmarkListFromEvent
                                        |> List.filterMap
                                            (\( pubKey, bookmarkList ) ->
                                                if pubKey == user.pubKey then
                                                    Just bookmarkList

                                                else
                                                    Nothing
                                            )
                                        |> List.head
                                        |> Maybe.map (requestForBookmarkContent shared.nostr ArticleBookmark)
                                        |> Maybe.withDefault Effect.none
                            in
                            ( model, requestEffect )

                        _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.receiveMessage ReceivedMessage
        , model.bookmarkButtons
            |> Dict.toList
            |> List.map (\(eventId, bookmarkButton) -> BookmarkButton.subscriptions bookmarkButton |> Sub.map (BookmarkButtonMsg eventId))
            |> Sub.batch
        ]



-- VIEW


view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared model =
    let
        bookmarkList =
            Nostr.getBookmarks shared.nostr user.pubKey
                |> Maybe.withDefault emptyBookmarkList
    in
    { title = Translations.bookmarksTitle [ shared.browserEnv.translations ]
    , body =
        [ viewBookmarks user shared model bookmarkList
        ]
    }


viewBookmarks : Auth.User -> Shared.Model -> Model -> BookmarkList -> Html Msg
viewBookmarks user shared model bookmarkList =
    case model.selectedBookmarkType of
        ArticleBookmark ->
            viewArticleBookmarks shared model bookmarkList.articles

        HighlightBookmark ->
            viewHighlightBookmarks shared (Nostr.highlightsByAuthor shared.nostr user.pubKey)

        NoteBookmark ->
            viewNoteBookmarks shared model bookmarkList.notes


viewArticleBookmarks : Shared.Model -> Model -> List AddressComponents -> Html Msg
viewArticleBookmarks shared model addressComponents =
    addressComponents
        |> List.filterMap (Nostr.getArticle shared.nostr)
        |> Nostr.sortArticlesByDate
        |> Ui.View.viewArticlePreviews
            ArticlePreviewList
            { articleComments = ArticleComments.init
            , articleHighlights = ArticleHighlights.init
            , articleToInteractionsMsg = \_ _ -> NoOp
            , bookmarkButtonMsg = BookmarkButtonMsg
            , bookmarkButtons = model.bookmarkButtons
            , browserEnv = shared.browserEnv
            , commentsToMsg = \_ -> NoOp
            , highlightsToMsg = \_ -> NoOp
            , deleteButtonMsg = Nothing
            , nostr = shared.nostr
            , loginStatus = shared.loginStatus
            , onLoadMore = Nothing
            , sharing = Nothing
            , theme = shared.theme
            }


viewHashtagBookmarks : Shared.Model -> Model -> List String -> Html Msg
viewHashtagBookmarks _ _ _ =
    emptyHtml


viewHighlightBookmarks : Shared.Model -> List Highlight -> Html Msg
viewHighlightBookmarks shared highlights =
    case highlights of
        [] ->
            emptyHtml

        _ ->
            let
                styles =
                    stylesForTheme shared.theme
            in
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_3
                    , Tw.p_4
                    , Tw.w_full
                    ]
                ]
                (List.map (viewHighlightBookmark shared styles) highlights)


viewHighlightBookmark : Shared.Model -> Ui.Styles.Styles Msg -> Highlight -> Html Msg
viewHighlightBookmark shared styles highlight =
    let
        article =
            highlight.addressComponents
                |> Maybe.andThen (Nostr.getArticle shared.nostr)

        articleHref =
            article
                |> Maybe.andThen (\loadedArticle -> linkToArticle (Nostr.getAuthor shared.nostr loadedArticle.author) loadedArticle)
                |> Maybe.withDefault "#"

        articleTitle =
            article
                |> Maybe.andThen .title
                |> Maybe.withDefault ""
    in
    a
        [ href articleHref
        , css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_1
            , Tw.p_3
            , Tw.rounded_lg
            , Tw.no_underline
            ]
        ]
        [ blockquote
            (styles.colorStyleGrayscaleText
                ++ styles.textStyleBody
                ++ [ css
                        [ Tw.m_0
                        , Tw.pl_3
                        , Tw.border_l_2
                        , Tw.border_solid
                        , Tw.line_clamp_3
                        ]
                   ]
            )
            [ text highlight.content ]
        , if String.isEmpty articleTitle then
            emptyHtml

          else
            span
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody ++ [ css [ Tw.font_medium, Tw.truncate ] ])
                [ text articleTitle ]
        , span
            (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
            [ text (BrowserEnv.formatDate shared.browserEnv highlight.createdAt) ]
        ]


viewNoteBookmarks : Shared.Model -> Model -> List EventId -> Html Msg
viewNoteBookmarks _ _ _ =
    emptyHtml


viewUrlBookmarks : Shared.Model -> Model -> List String -> Html Msg
viewUrlBookmarks _ _ _ =
    emptyHtml


requestArticles : Nostr.Model -> List AddressComponents -> Effect msg
requestArticles nostr addressComponents =
    addressComponents
        |> List.filter (\address -> Nostr.getArticle nostr address == Nothing)
        |> List.map
            (\( kind, pubKey, identifier ) ->
                { emptyEventFilter
                    | authors = Just [ pubKey ]
                    , kinds = Just [ kind ]
                    , tagReferences = Just [ TagReferenceIdentifier identifier ]
                }
                    |> RequestBookmarks
                    |> Nostr.createRequest nostr "Bookmark articles" [ KindUserMetadata ]
                    |> Shared.Msg.RequestNostrEvents
                    |> Effect.sendSharedMsg
            )
        |> Effect.batch


availableCategories : BookmarkList -> List Highlight -> I18Next.Translations -> List (Categories.CategoryData BookmarkType)
availableCategories bookmarkList highlights translations =
    let
        articleBookmarkCategory =
            if List.length bookmarkList.articles > 0 then
                [ { category = ArticleBookmark
                  , title = Translations.articlesTitle [ translations ]
                  , testId = "bookmarks-articles"
                  }
                ]

            else
                []

        highlightBookmarkCategory =
            if List.length highlights > 0 then
                [ { category = HighlightBookmark
                  , title = Translations.highlightsTitle [ translations ]
                  , testId = "bookmarks-highlights"
                  }
                ]

            else
                []

        noteBookmarkCategory =
            if List.length bookmarkList.notes > 0 then
                [ { category = NoteBookmark
                  , title = Translations.notesTitle [ translations ]
                  , testId = "bookmarks-notes"
                  }
                ]

            else
                []
    in
    articleBookmarkCategory ++ highlightBookmarkCategory ++ noteBookmarkCategory
