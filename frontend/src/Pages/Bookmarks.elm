module Pages.Bookmarks exposing (Model, Msg, page)

import Auth
import Components.Categories as Categories
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Json.Decode as Decode
import Layouts
import Nostr exposing (getBookmarks)
import Nostr.BookmarkList exposing (BookmarkList, BookmarkType(..), bookmarkListFromEvent, bookmarksCount, emptyBookmarkList)
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, PubKey)
import Route exposing (Route)
import Route.Path
import Page exposing (Page)
import Ports
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.Bookmarks as Translations
import Ui.Styles exposing (Theme, fontFamilyUnbounded, fontFamilyInter)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared user
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }




-- INIT


type alias Model =
    { categories : Categories.Model BookmarkType
    , selectedBookmarkType : BookmarkType
    }


init : Shared.Model -> Auth.User -> () -> ( Model, Effect Msg )
init shared user () =
    let
        contentRequest =
            Nostr.getBookmarks shared.nostr user.pubKey
            |> Maybe.map (requestForBookmarkContent shared.nostr ArticleBookmark)
            |> Maybe.withDefault Effect.none
    in
    ( { categories = Categories.init { selected = ArticleBookmark }
      , selectedBookmarkType = ArticleBookmark
      }
    , contentRequest
    )

requestForBookmarkContent : Nostr.Model -> BookmarkType -> BookmarkList -> Effect Msg
requestForBookmarkContent nostr bookmarkType bookmarkList =
    case bookmarkType of
        ArticleBookmark ->
            bookmarkList.articles
            |> List.filter (\addressComponents ->
                    -- only request articles we don't have yet
                    Nostr.getArticle nostr addressComponents == Nothing
                )
            |> List.map (\(kind, pubKey, identifier) ->
                { emptyEventFilter
                | authors = Just [ pubKey ]
                , kinds = Just [ kind ]
                , tagReferences = Just [ TagReferenceIdentifier identifier ]
                }
                |> RequestArticlesFeed 
                |> Nostr.createRequest nostr "Bookmark articles" [KindUserMetadata]
                |> Shared.Msg.RequestNostrEvents
                |> Effect.sendSharedMsg
            )
            |> Effect.batch

        HashtagBookmark ->
            -- { authors = Nothing
            -- , ids = Nothing
            -- , kinds = Just [ KindLongFormContent ]
            -- , tagReferences =
            --     bookmarkList.hashtags
            --     |> List.map TagReferenceTag
            --     |> Just
            -- , limit = Nothing
            -- , since = Nothing
            -- , until = Nothing
            -- }
            Effect.none

        NoteBookmark ->
            -- { authors = Nothing
            -- , ids = Nothing
            -- , kinds = Just [ KindLongFormContent ]
            -- , tagReferences =
            --     bookmarkList.notes
            --     |> List.map TagReferenceEventId
            --     |> Just
            -- , limit = Nothing
            -- , since = Nothing
            -- , until = Nothing
            -- }
            Effect.none

        UrlBookmark ->
            Effect.none



-- UPDATE


type Msg
    =  ReceivedMessage IncomingMessage
    | CategoriesSent (Categories.Msg BookmarkType Msg)
    | CategorySelected BookmarkType
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents

update : Auth.User -> Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage user shared model message

        CategoriesSent innerMsg ->
            Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories}
                , toMsg = CategoriesSent
                }

        CategorySelected bookmarkType ->
            ( { model | selectedBookmarkType = bookmarkType }, Effect.none )

        AddArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveArticleBookmark pubKey addressComponents ->
            let
                numberOfBookmarks =
                    Nostr.getBookmarks shared.nostr user.pubKey
                    |> Maybe.map bookmarksCount
                    |> Maybe.withDefault 0

                redirectForEmptyList =
                    if numberOfBookmarks <= 1 then
                        -- assume that we're about to delete the last bookmark
                        Effect.replaceRoute { hash = Nothing , path = Route.Path.Read , query = Dict.empty }
                    else
                        Effect.none
            in
            ( model
            , Effect.batch
                [ redirectForEmptyList
                , SendBookmarkListWithoutArticle pubKey addressComponents
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                ]
            )

updateWithMessage : Auth.User -> Shared.Model.Model -> Model -> IncomingMessage -> (Model, Effect Msg)
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case (Decode.decodeValue (Decode.field "kind" Nostr.Event.kindDecoder) message.value) of
                Ok KindBookmarkList ->
                    case (Decode.decodeValue (Decode.field "events" (Decode.list Nostr.Event.decodeEvent)) message.value) of
                        Ok events ->
                            let
                                requestEffect =
                                    events
                                    |> List.map bookmarkListFromEvent
                                    |> List.filterMap (\(pubKey, bookmarkList) ->
                                            if pubKey == user.pubKey then
                                                Just bookmarkList
                                            else
                                                Nothing
                                        )
                                    |> List.head
                                    |> Maybe.map (requestForBookmarkContent shared.nostr model.selectedBookmarkType)
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
    Ports.receiveMessage ReceivedMessage


-- VIEW

view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme

        bookmarkList =
            Nostr.getBookmarks shared.nostr user.pubKey
            |> Maybe.withDefault emptyBookmarkList
    in
    { title = Translations.bookmarksTitle [ shared.browserEnv.translations ]
    , body =
        [ Categories.new
            { model = model.categories
            , toMsg = CategoriesSent
            , onSelect = CategorySelected
            , categories = availableCategories bookmarkList shared.browserEnv.translations
            , browserEnv = shared.browserEnv
            , styles = styles
            }
            |> Categories.view
        , viewBookmarks user shared model bookmarkList
        ]
    }

viewBookmarks : Auth.User -> Shared.Model -> Model -> BookmarkList -> Html Msg
viewBookmarks user shared model bookmarkList =
    case model.selectedBookmarkType of
        ArticleBookmark ->
            viewArticleBookmarks user shared model bookmarkList.articles

        HashtagBookmark ->
            viewHashtagBookmarks user shared model bookmarkList.hashtags

        NoteBookmark ->
            viewNoteBookmarks user shared model bookmarkList.notes

        UrlBookmark ->
            viewUrlBookmarks user shared model bookmarkList.urls

viewArticleBookmarks : Auth.User -> Shared.Model -> Model -> List AddressComponents -> Html Msg
viewArticleBookmarks user shared model addressComponents =
    addressComponents
    |> List.filterMap (Nostr.getArticle shared.nostr)
    |> Nostr.sortArticlesByDate
    |> Ui.View.viewArticlePreviews
        ArticlePreviewList
            { theme = shared.theme
            , browserEnv = shared.browserEnv
            , nostr = shared.nostr
            , userPubKey = Just user.pubKey
            , onBookmark = Just (AddArticleBookmark user.pubKey, RemoveArticleBookmark user.pubKey)
            , onReaction = Nothing
            , onZap = Nothing
            }

viewHashtagBookmarks user shared model hashtags =
    div [][]

viewNoteBookmarks user shared model notes =
    div [][]

viewUrlBookmarks user shared model urls =
    div [][]

availableCategories : BookmarkList -> I18Next.Translations -> List (Categories.CategoryData BookmarkType)
availableCategories bookmarkList translations =
    let
        articleBookmarkCategory =
            if List.length bookmarkList.articles > 0 then
                [ { category = ArticleBookmark , title = Translations.articlesTitle [ translations ] } ]
            else
                []

        hashtagBookmarkCategory =
            if List.length bookmarkList.hashtags > 0 then
                [ { category = HashtagBookmark, title = Translations.hashtagsTitle [ translations ] } ]
            else
                []

        urlBookmarkCategory =
            if List.length bookmarkList.urls > 0 then
                [ { category = UrlBookmark, title = Translations.urlsTitle [ translations ] } ]
            else
                []

        noteBookmarkCategory =
            if List.length bookmarkList.notes > 0 then
                [ { category = NoteBookmark, title = Translations.notesTitle [ translations ] } ]
            else
                []
    in
    articleBookmarkCategory ++ hashtagBookmarkCategory ++ urlBookmarkCategory ++ noteBookmarkCategory