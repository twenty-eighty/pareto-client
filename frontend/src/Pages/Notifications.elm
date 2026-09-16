module Pages.Notifications exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, div, h1, p, span, text)
import Html.Styled.Attributes exposing (css, href)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Notifications exposing (NotificationItem, NotificationKind(..))
import Nostr.Profile exposing (ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.Notifications as Translations
import Ui.Article exposing (linkToArticle)
import Ui.Profile exposing (viewProfileImageSmall)
import Ui.Shared exposing (emptyHtml)
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
        [ Shared.createNotificationsActivityEffect shared.nostr user.pubKey
        , Effect.sendSharedMsg (Shared.Msg.MarkNotificationsSeen user.pubKey)
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
            Nostr.notificationsForPubKey shared.nostr user.pubKey
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
            , viewBody shared styles user.pubKey items
            ]
        ]
    }


viewBody : Shared.Model -> Styles Msg -> PubKey -> List NotificationItem -> Html Msg
viewBody shared styles _ items =
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


viewItem : Shared.Model -> Styles Msg -> NotificationItem -> Html Msg
viewItem shared styles item =
    let
        translations =
            shared.browserEnv.translations

        actorProfile =
            Nostr.getProfile shared.nostr item.actorPubKey

        actorName =
            actorProfile
                |> Maybe.map (\profile -> profileDisplayName profile.pubKey profile)
                |> Maybe.withDefault (String.left 8 item.actorPubKey ++ "…")

        articleAuthor =
            Nostr.getAuthor shared.nostr item.article.author

        articleHref =
            linkToArticle articleAuthor item.article
                |> Maybe.withDefault "#"

        articleTitle =
            item.article.title
                |> Maybe.withDefault (Translations.unknownArticleTitle [ translations ])

        actionText =
            case item.kind of
                ReactionNotification ->
                    Translations.likedYourArticle [ translations ]

                CommentNotification ->
                    Translations.commentedOnYourArticle [ translations ]

                RepostNotification ->
                    Translations.repostedYourArticle [ translations ]

                ZapNotification ->
                    Translations.zappedYourArticle [ translations ]

                NutzapNotification ->
                    Translations.nutzappedYourArticle [ translations ]

        detailBlock =
            case ( item.kind, item.detail ) of
                ( ReactionNotification, Just emoji ) ->
                    span [ css [ Tw.ml_1 ] ] [ text emoji ]

                ( CommentNotification, Just snippet ) ->
                    p
                        (styles.colorStyleGrayscaleText
                            ++ styles.textStyleBody
                            ++ [ css [ Tw.m_0, Tw.mt_1, Tw.line_clamp_2 ] ]
                        )
                        [ text snippet ]

                ( ZapNotification, Just amount ) ->
                    span
                        (styles.colorStyleGrayscaleText
                            ++ [ css [ Tw.ml_1, Tw.font_medium ] ]
                        )
                        [ text ("· " ++ amount) ]

                ( NutzapNotification, Just amount ) ->
                    span
                        (styles.colorStyleGrayscaleText
                            ++ [ css [ Tw.ml_1, Tw.font_medium ] ]
                        )
                        [ text ("· " ++ amount) ]

                _ ->
                    emptyHtml
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
        [ viewProfileImageSmall shared.browserEnv.environment (\children -> div [ css [ Tw.flex_none ] ] children) actorProfile (Nostr.getProfileValidationStatus shared.nostr item.actorPubKey |> Maybe.withDefault ValidationUnknown)
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
                    [ text actionText ]
                , detailBlock
                ]
            , span
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody ++ [ css [ Tw.font_medium, Tw.truncate ] ])
                [ text articleTitle ]
            , span
                (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
                [ text (BrowserEnv.formatDate shared.browserEnv item.createdAt) ]
            ]
        ]
