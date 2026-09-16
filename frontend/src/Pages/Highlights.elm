module Pages.Highlights exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, blockquote, div, h1, p, span, text)
import Html.Styled.Attributes exposing (css, href)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Highlights exposing (HighlightItem)
import Nostr.Profile exposing (ProfileValidation(..), profileDisplayName)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.Highlights as Translations
import Ui.Article exposing (linkToArticle)
import Ui.Profile exposing (viewProfileImageSmall)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
        , update = update
        , subscriptions = subscriptions
        , view = view user shared
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
    {}


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( {}
    , Effect.batch
        [ Shared.createHighlightsActivityEffect shared.nostr user.pubKey
        , Effect.scrollContentToTop
        ]
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared _ =
    let
        styles =
            stylesForTheme shared.theme

        items =
            Nostr.highlightsForPubKey shared.nostr user.pubKey
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_6
                , Tw.p_4
                , Bp.md [ Tw.p_6 ]
                , Tw.max_w_3xl
                , Tw.mx_auto
                , Tw.w_full
                ]
            ]
            [ h1
                (styles.colorStyleGrayscaleTitle
                    ++ styles.textStyleH1
                    ++ [ css [ Tw.m_0 ] ]
                )
                [ text (Translations.pageTitle [ shared.browserEnv.translations ]) ]
            , viewBody shared styles items
            ]
        ]
    }


viewBody : Shared.Model -> Styles Msg -> List HighlightItem -> Html Msg
viewBody shared styles items =
    case items of
        [] ->
            p
                (styles.colorStyleGrayscaleText
                    ++ styles.textStyleBody
                    ++ [ css [ Tw.m_0 ] ]
                )
                [ text (Translations.emptyText [ shared.browserEnv.translations ]) ]

        _ ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_3
                    ]
                ]
                (List.map (viewItem shared styles) items)


viewItem : Shared.Model -> Styles Msg -> HighlightItem -> Html Msg
viewItem shared styles item =
    let
        translations =
            shared.browserEnv.translations

        actorProfile =
            Nostr.getProfile shared.nostr item.highlight.pubKey

        actorName =
            actorProfile
                |> Maybe.map (\profile -> profileDisplayName profile.pubKey profile)
                |> Maybe.withDefault (String.left 8 item.highlight.pubKey ++ "…")

        articleAuthor =
            Nostr.getAuthor shared.nostr item.article.author

        articleHref =
            linkToArticle articleAuthor item.article
                |> Maybe.withDefault "#"

        articleTitle =
            item.article.title
                |> Maybe.withDefault (Translations.unknownArticleTitle [ translations ])
    in
    a
        [ href articleHref
        , css
            [ Tw.flex
            , Tw.gap_3
            , Tw.p_3
            , Tw.rounded_lg
            , Tw.no_underline
            , Tw.transition_colors
            , Tw.duration_150
            ]
        ]
        [ viewProfileImageSmall shared.browserEnv.environment (\children -> div [ css [ Tw.flex_none ] ] children) actorProfile (Nostr.getProfileValidationStatus shared.nostr item.highlight.pubKey |> Maybe.withDefault ValidationUnknown)
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_1
                , Tw.min_w_0
                , Tw.flex_1
                ]
            ]
            [ div
                [ css [ Tw.flex, Tw.flex_wrap, Tw.items_baseline, Tw.gap_x_1 ] ]
                [ span
                    (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody ++ [ css [ Tw.font_semibold ] ])
                    [ text actorName ]
                , span
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text (Translations.highlightedYourArticle [ translations ]) ]
                ]
            , blockquote
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
                [ text item.highlight.content ]
            , span
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody ++ [ css [ Tw.font_medium, Tw.truncate ] ])
                [ text articleTitle ]
            , span
                (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
                [ text (BrowserEnv.formatDate shared.browserEnv item.highlight.createdAt) ]
            ]
        ]
