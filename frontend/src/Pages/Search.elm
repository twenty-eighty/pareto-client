module Pages.Search exposing (Model, Msg, page)

import BrowserEnv exposing (Environment)
import Components.ArticleComments as ArticleComments
import Components.ArticleHighlights as ArticleHighlights
import Components.SearchBar as SearchBar
import Css
import Dict
import Effect exposing (Effect)
import Html.Styled exposing (Html, a, div, img, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event exposing (EventFilter, Kind(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Nip19 as Nip19
import Nostr.Profile as Profile
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Pareto exposing (BootstrapAuthor)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.Search as Translations
import Ui.Links
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
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { searchBar : SearchBar.Model
    , query : Maybe String
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
            ( { searchBar = SearchBar.init { searchText = Just searchText }
              , query = Just searchText
              }
            , searchEffect shared searchText
            )

        Nothing ->
            ( { searchBar = SearchBar.init { searchText = Nothing }
              , query = Nothing
              }
            , Effect.sendSharedMsg Shared.Msg.ResetArticles
            )



-- UPDATE


type Msg
    = Search (Maybe String)
    | SearchBarSent (SearchBar.Msg Msg)
    | NoOp


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

        NoOp ->
            ( model, Effect.none )


performSearch : Shared.Model -> Model -> Maybe String -> ( Model, Effect Msg )
performSearch shared model maybeSearchText =
    case maybeSearchText of
        Just searchText ->
            let
                modelWithQuery =
                    { model | query = Just searchText }
            in
            case Nip19.decode searchText of
                Ok (Nip19.Npub _) ->
                    ( modelWithQuery
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.P_Profile_ { profile = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.Note _) ->
                    ( modelWithQuery
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.A_Addr_ { addr = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.NProfile _) ->
                    ( modelWithQuery
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.P_Profile_ { profile = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.NEvent _) ->
                    ( modelWithQuery
                    , Effect.batch
                        [ Effect.pushRoute { path = Route.Path.E_Event_ { event = searchText }, query = Dict.empty, hash = Nothing } ]
                    )

                Ok (Nip19.NAddr _) ->
                    ( modelWithQuery
                    , Effect.pushRoute { path = Route.Path.A_Addr_ { addr = searchText }, query = Dict.empty, hash = Nothing }
                    )

                _ ->
                    -- try to decode search string as NIP-05 handle
                    case Nip05.parseNip05 searchText of
                        Just nip05 ->
                            ( modelWithQuery
                            , Effect.pushRoute { path = Route.Path.U_User_ { user = Nip05.nip05ToString nip05 }, query = Dict.empty, hash = Nothing }
                            )

                        -- neither NIP-19 nor NIP-05 identifier - search via search relays
                        Nothing ->
                            ( modelWithQuery
                            , Effect.batch
                                [ searchEffect shared searchText
                                , Effect.pushRoute { path = Route.Path.Search, query = Dict.singleton queryDictKey (Url.percentEncode searchText), hash = Nothing }
                                ]
                            )

        Nothing ->
            ( { model | query = Nothing }, Effect.replaceRoute { path = Route.Path.Search, query = Dict.empty, hash = Nothing } )


searchEffect : Shared.Model -> String -> Effect Msg
searchEffect shared searchText =
    Effect.batch
        [ RequestSearchResults (searchEventFilters searchText)
            |> Nostr.createRequest shared.nostr "Search" []
            |> Shared.Msg.RequestNostrEvents
            |> Effect.sendSharedMsg
        , requestAuthorProfiles shared
        ]


requestAuthorProfiles : Shared.Model -> Effect Msg
requestAuthorProfiles shared =
    let
        authorPubKeys =
            Nostr.getAuthorsPubKeys shared.nostr
    in
    if List.isEmpty authorPubKeys then
        Effect.none

    else
        { emptyEventFilter
            | authors = Just authorPubKeys
            , kinds = Just [ KindUserMetadata ]
        }
            |> RequestProfile Nothing
            |> Nostr.createRequest shared.nostr "Search authors" []
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
            [ text <| Translations.explanation1 [ shared.browserEnv.translations ]
            ]
        , p
            []
            [ text <| Translations.explanation2 [ shared.browserEnv.translations ]
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
        , viewAuthorHits shared model.query
        , viewArticles shared model.query
        ]


type alias AuthorHit =
    { nip05 : String
    , pubKey : String
    , label : String
    , picture : Maybe String
    }


viewAuthorHits : Shared.Model -> Maybe String -> Html Msg
viewAuthorHits shared maybeQuery =
    case maybeQuery of
        Just query ->
            case authorHits shared.nostr query of
                [] ->
                    text ""

                hits ->
                    div
                        [ css
                            (resultColumnCss
                                ++ [ Tw.flex
                                   , Tw.flex_col
                                   , Tw.gap_2
                                   , Tw.self_center
                                   ]
                            )
                        ]
                        [ p
                            [ css [ Tw.font_semibold ] ]
                            [ text <| Translations.authorsHeading [ shared.browserEnv.translations ] ]
                        , div
                            [ css
                                [ Tw.flex
                                , Tw.flex_col
                                , Tw.gap_2
                                ]
                            ]
                            (List.map (viewAuthorHit shared.browserEnv.environment) hits)
                        ]

        Nothing ->
            text ""


viewAuthorHit : Environment -> AuthorHit -> Html Msg
viewAuthorHit environment hit =
    let
        body =
            case hit.picture of
                Just pictureUrl ->
                    let
                        sources =
                            Ui.Links.scaledImageSources environment 48 pictureUrl
                    in
                    [ img
                        [ Attr.src sources.src
                        , Attr.attribute "srcset" sources.srcset
                        , Attr.alt ""
                        , css
                            [ Tw.h_12
                            , Tw.w_12
                            , Tw.shrink_0
                            , Tw.rounded_full
                            , Tw.object_cover
                            ]
                        ]
                        []
                    , authorHitText hit
                    ]

                Nothing ->
                    [ authorHitText hit ]
    in
    case Ui.Links.linkToProfilePubKey True hit.pubKey of
        Just url ->
            a
                [ Attr.href url
                , css authorHitCardCss
                ]
                body

        Nothing ->
            div
                [ css authorHitCardCss ]
                body


authorHitText : AuthorHit -> Html Msg
authorHitText hit =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_1
            , Tw.min_w_0
            ]
        ]
        [ p
            [ css [ Tw.text_lg, Tw.font_semibold ] ]
            [ text hit.label ]
        , p
            [ css [ Tw.text_sm ] ]
            [ text hit.nip05 ]
        ]


authorHitCardCss : List Css.Style
authorHitCardCss =
    [ Tw.flex
    , Tw.flex_row
    , Tw.items_center
    , Tw.gap_3
    , Tw.border
    , Tw.rounded_md
    , Tw.p_3
    , Tw.w_full
    , Tw.no_underline
    ]


authorHits : Nostr.Model -> String -> List AuthorHit
authorHits nostr query =
    let
        needle =
            normalizeForSearch query
    in
    if String.length needle < 2 || String.startsWith "#" needle then
        []

    else
        Pareto.bootstrapAuthorsList
            |> Dict.toList
            |> List.filterMap (authorHit nostr needle)
            |> List.sortBy (\hit -> String.toLower hit.label)


authorHit : Nostr.Model -> String -> ( String, BootstrapAuthor ) -> Maybe AuthorHit
authorHit nostr needle ( nip05, entry ) =
    let
        profileLabel =
            Nostr.getProfile nostr entry.pubKey
                |> Maybe.map (Profile.profileDisplayName entry.pubKey)

        label =
            Maybe.withDefault entry.name profileLabel

        picture =
            Nostr.getProfile nostr entry.pubKey
                |> Maybe.andThen .picture
                |> Maybe.andThen nonemptyString

        haystacks =
            [ label, entry.name, nip05, nip05Local nip05 ]
                ++ profileNames nostr entry.pubKey
    in
    if List.any (\value -> String.contains needle (normalizeForSearch value)) haystacks then
        Just
            { nip05 = nip05
            , pubKey = entry.pubKey
            , label = label
            , picture = picture
            }

    else
        Nothing


profileNames : Nostr.Model -> String -> List String
profileNames nostr pubKey =
    case Nostr.getProfile nostr pubKey of
        Just profile ->
            List.filterMap identity [ profile.displayName, profile.name ]

        Nothing ->
            []


nonemptyString : String -> Maybe String
nonemptyString value =
    let
        trimmed =
            String.trim value
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed


nip05Local : String -> String
nip05Local nip05 =
    String.split "@" nip05
        |> List.head
        |> Maybe.withDefault nip05


normalizeForSearch : String -> String
normalizeForSearch value =
    value
        |> String.toLower
        |> String.replace "-" " "
        |> String.replace "_" " "
        |> String.words
        |> String.join " "


viewArticles : Shared.Model -> Maybe String -> Html Msg
viewArticles shared maybeQuery =
    div
        [ css
            (resultColumnCss
                ++ [ Tw.flex
                   , Tw.flex_col
                   , Tw.gap_2
                   , Tw.self_center
                   ]
            )
        ]
        [ case maybeQuery of
            Just _ ->
                p
                    [ css [ Tw.font_semibold ] ]
                    [ text <| Translations.articlesHeading [ shared.browserEnv.translations ] ]

            Nothing ->
                text ""
        , Nostr.getArticlesByDate shared.nostr
            |> Ui.View.viewArticlePreviews
                ArticlePreviewList
                { articleComments = ArticleComments.init
                , articleHighlights = ArticleHighlights.init
                , articleToInteractionsMsg = \_ _ -> NoOp
                , bookmarkButtonMsg = \_ _ -> NoOp
                , bookmarkButtons = Dict.empty
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
        ]


{-| Same widths as `Ui.Article.viewArticlePreviewList`.
-}
resultColumnCss : List Css.Style
resultColumnCss =
    [ Tw.w_full
    , Bp.xxl
        [ Css.property "width" "1024px"
        ]
    , Bp.xl
        [ Css.property "width" "800px"
        ]
    , Bp.lg
        [ Css.property "width" "720px"
        ]
    , Bp.md
        [ Css.property "width" "640px"
        ]
    , Bp.sm
        [ Css.property "width" "550px"
        ]
    ]
