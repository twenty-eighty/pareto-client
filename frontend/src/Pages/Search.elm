module Pages.Search exposing (Model, Msg, page)

import Components.SearchBar as SearchBar
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, p)
import Html.Styled.Attributes exposing (css)
import Layouts
import Nostr
import Nostr.Event exposing (EventFilter, Kind(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Nip19 as Nip19
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.Search as Translations
import Ui.Styles exposing (Theme, stylesForTheme)
import Ui.View exposing (ArticlePreviewType(..))
import Url
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
    { searchBar : SearchBar.Model
    }


queryDictKey : String
queryDictKey =
    "query"


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    let
        maybeSearchText =
            Dict.get queryDictKey route.query
                |> Maybe.andThen Url.percentDecode
    in
    case maybeSearchText of
        Just searchText ->
            ( { searchBar = SearchBar.init { searchText = Just searchText } }
            , searchEffect shared searchText
            )

        Nothing ->
            ( { searchBar = SearchBar.init { searchText = Nothing } }
            , Effect.sendSharedMsg Shared.Msg.ResetArticles
            )



-- UPDATE


type Msg
    = Search (Maybe String)
    | SearchBarSent (SearchBar.Msg Msg)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Search maybeSearchText ->
            performSearch shared model (Maybe.map String.trim maybeSearchText)

        SearchBarSent innerMsg ->
            SearchBar.update
                { msg = innerMsg
                , model = model.searchBar
                , toModel = \searchBar -> { model | searchBar = searchBar }
                , toMsg = SearchBarSent
                , onSearch = Search
                }


performSearch : Shared.Model -> Model -> Maybe String -> ( Model, Effect Msg )
performSearch shared model maybeSearchText =
    case maybeSearchText of
        Just searchText ->
            case Nip19.decode searchText of
                Ok (Nip19.Npub _) ->
                    ( model
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.P_Profile_ { profile = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.Note _) ->
                    ( model
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.A_Addr_ { addr = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.NProfile _) ->
                    ( model
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.P_Profile_ { profile = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.NEvent _) ->
                    ( model
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.E_Event_ { event = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.NAddr _) ->
                    ( model
                    , Effect.pushRoute { path = Route.Path.A_Addr_ { addr = searchText }, query = Dict.empty, hash = Nothing }
                    )

                _ ->
                    -- try to decode search string as NIP-05 handle
                    case Nip05.parseNip05 searchText of
                        Just nip05 ->
                            ( model
                            , Effect.pushRoute { path = Route.Path.U_User_ { user = Nip05.nip05ToString nip05 }, query = Dict.empty, hash = Nothing }
                            )

                        -- neither NIP-19 nor NIP-05 identifier - search via search relays
                        Nothing ->
                            ( model
                            , Effect.batch
                                [ searchEffect shared searchText
                                , Effect.pushRoute { path = Route.Path.Search, query = Dict.singleton queryDictKey (Url.percentEncode searchText), hash = Nothing }
                                ]
                            )

        Nothing ->
            ( model, Effect.replaceRoute { path = Route.Path.Search, query = Dict.empty, hash = Nothing } )


searchEffect : Shared.Model -> String -> Effect Msg
searchEffect shared searchText =
    RequestSearchResults (searchEventFilters searchText)
        |> Nostr.createRequest shared.nostr "Search" []
        |> Shared.Msg.RequestNostrEvents
        |> Effect.sendSharedMsg



-- Nostr.getSearchRelayUrls model maybePubKey


searchEventFilters : String -> List EventFilter
searchEventFilters searchText =
    [ { emptyEventFilter | kinds = Just [ KindLongFormContent ], search = Just searchText, limit = Just 20 } ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SearchBarSent (SearchBar.subscribe model.searchBar)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ viewSearch shared model
        ]
    }


viewSearch : Shared.Model -> Model -> Html Msg
viewSearch shared model =
    let
        styles =
            stylesForTheme shared.theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_5
            , Tw.justify_center
            , Tw.max_w_full
            , Tw.m_4
            ]
        ]
        [ p
            []
            [ Html.text <| Translations.explanation1 [ shared.browserEnv.translations ]
            ]
        , p
            []
            [ Html.text <| Translations.explanation2 [ shared.browserEnv.translations ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.justify_center
                ]
            ]
            [ SearchBar.new
                { model = model.searchBar
                , toMsg = SearchBarSent
                , browserEnv = shared.browserEnv
                , styles = styles
                }
                |> SearchBar.view
            ]
        , viewArticles shared
        ]


viewArticles : Shared.Model -> Html Msg
viewArticles shared =
    Nostr.getArticlesByDate shared.nostr
        |> Ui.View.viewArticlePreviews
            ArticlePreviewList
            { theme = shared.theme
            , browserEnv = shared.browserEnv
            , nostr = shared.nostr
            , userPubKey = Shared.loggedInPubKey shared.loginStatus
            , onBookmark = Nothing
            , onReaction = Nothing
            , onRepost = Nothing
            , onZap = Nothing
            }
