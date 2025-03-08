module Pages.About exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, div, img, span, text)
import Html.Styled.Attributes as Attr exposing (css, href, src, target, width)
import I18Next
import Layouts
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..), KindInformationLink(..), Tag(..), TagReference(..), buildAddress, numberForKind)
import Nostr.HandlerInformation exposing (HandlerInformation, buildHandlerInformation)
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileToJson)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (Following(..), PubKey)
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (LoginStatus(..))
import Shared.Msg
import Tailwind.Utilities as Tw
import Time
import Translations.About as Translations
import Ui.Profile exposing (FollowType(..))
import Ui.Styles exposing (Styles, Theme, stylesForTheme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
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
    {}


init : Shared.Model -> () -> ( Model, Effect Msg )
init _ () =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = RecommendClient PubKey HandlerInformation
    | PublishHandlerInformation PubKey HandlerInformation
    | PublishClientProfile PubKey HandlerInformation
    | PublishAuthorsList PubKey


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        RecommendClient pubKey handlerInformation ->
            ( model
            , sendClientRecommendation shared.nostr pubKey handlerInformation
            )

        PublishHandlerInformation pubKey handlerInformation ->
            ( model
            , sendHandlerInformation shared.nostr pubKey handlerInformation
            )

        PublishClientProfile pubKey handlerInformation ->
            ( model
            , sendClientProfile shared.nostr pubKey handlerInformation.profile
            )

        PublishAuthorsList pubKey ->
            let
                authorsFollowList =
                    Pareto.bootstrapAuthorsList
                        |> List.map
                            (\( nip05, authorPubKey ) ->
                                FollowingPubKey
                                    { pubKey = authorPubKey
                                    , relay = Just Pareto.paretoRelay
                                    , petname = Just nip05
                                    }
                            )
            in
            ( model
            , SendFollowList pubKey authorsFollowList
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )


sendClientRecommendation : Nostr.Model -> PubKey -> HandlerInformation -> Effect Msg
sendClientRecommendation nostr pubKey handlerInformation =
    { pubKey = pubKey
    , createdAt = Time.millisToPosix 0
    , kind = KindHandlerRecommendation
    , tags =
        [ EventDelegationTag (numberForKind KindLongFormContent |> String.fromInt)
        , GenericTag [ "a", buildAddress ( KindHandlerInformation, handlerInformation.pubKey, handlerInformation.handlerIdentifier ), Pareto.paretoRelay, "web" ]
        ]
    , content = ""
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }
        |> SendClientRecommendation (Nostr.getWriteRelayUrlsForPubKey nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendHandlerInformation : Nostr.Model -> PubKey -> HandlerInformation -> Effect Msg
sendHandlerInformation nostr pubKey handlerInformation =
    buildHandlerInformation handlerInformation
        |> SendHandlerInformation (Nostr.getWriteRelayUrlsForPubKey nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendClientProfile : Nostr.Model -> PubKey -> Profile -> Effect Msg
sendClientProfile nostr pubKey profile =
    { pubKey = pubKey
    , createdAt = Time.millisToPosix 0
    , kind = KindUserMetadata
    , tags = []
    , content = profileToJson profile
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }
        |> SendProfile (Nostr.getWriteRelayUrlsForPubKey nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = Translations.aboutPageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.m_4
                ]
            ]
            [ viewContent shared (Pareto.applicationInformation shared.browserEnv.now)
            , viewFooter shared.theme shared.browserEnv
            ]
        ]
    }


viewSupportInformation : Theme -> I18Next.Translations -> Html Msg
viewSupportInformation theme translations =
    let
        styles =
            stylesForTheme theme
    in
    Html.p
        (styles.textStyleBody
            ++ styles.colorStyleGrayscaleText
            ++ [ css
                    [ Tw.my_4
                    ]
               ]
        )
        [ text <| Translations.feedbackEmailInfoText [ translations ]
        , a
            (styles.textStyleLinks
                ++ styles.colorStyleLinks
                ++ [ Attr.href <| "mailto:" ++ Pareto.supportEmail
                   ]
            )
            [ text Pareto.supportEmail
            ]
        ]


viewDonationInformation : Theme -> I18Next.Translations -> Html Msg
viewDonationInformation theme translations =
    let
        styles =
            stylesForTheme theme
    in
    Html.p
        (styles.textStyleBody
            ++ styles.colorStyleGrayscaleText
            ++ [ css
                    [ Tw.my_4
                    ]
               ]
        )
        [ div [ css [ Tw.mb_4 ] ] [ text <| Translations.donationInfoText [ translations ] ]
        , a
            (styles.textStyleLinks
                ++ styles.colorStyleLinks
                ++ [ Attr.href <| "https://geyser.fund/project/pareto"
                   , target "_blank"
                   ]
            )
            [ img
                [ src "https://storage.googleapis.com/geyser-projects-media/app/logo-name-dark.svg"
                , width 200
                ]
                []
            ]
        ]


viewContent : Shared.Model -> HandlerInformation -> Html Msg
viewContent shared handlerInformation =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            ]
        ]
        [ Ui.Profile.viewProfile
            handlerInformation.profile
            { browserEnv = shared.browserEnv
            , following = UnknownFollowing
            , subscribe = Nothing
            , theme = shared.theme
            , validation =
                Nostr.getProfileValidationStatus shared.nostr handlerInformation.pubKey
                    |> Maybe.withDefault ValidationUnknown
            }
            shared
        , viewSupportInformation shared.theme shared.browserEnv.translations
        , viewDonationInformation shared.theme shared.browserEnv.translations
        , viewActionButtons shared.theme shared.browserEnv handlerInformation shared.loginStatus
        , viewTechDetails shared.theme shared.browserEnv
        ]


viewActionButtons : Theme -> BrowserEnv -> HandlerInformation -> LoginStatus -> Html Msg
viewActionButtons theme browserEnv handlerInformation loginStatus =
    case loginStatus of
        LoggedIn pubKey ->
            let
                styles =
                    stylesForTheme theme
            in
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_3
                    ]
                ]
                [ div
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text <| Translations.recomendationExplanation [ browserEnv.translations ]
                    ]
                , Button.new
                    { label = Translations.recommendClientButtonTitle [ browserEnv.translations ]
                    , onClick = Just <| RecommendClient pubKey handlerInformation
                    , theme = theme
                    }
                    |> Button.view
                , viewPublishProfileButton theme browserEnv pubKey handlerInformation
                , viewPublishHandlerInformationButton theme browserEnv pubKey handlerInformation
                , viewPublishAuthorsListButton theme browserEnv pubKey
                ]

        _ ->
            div [] []


viewPublishAuthorsListButton : Theme -> BrowserEnv -> PubKey -> Html Msg
viewPublishAuthorsListButton theme browserEnv pubKey =
    if pubKey == Pareto.authorsKey then
        Button.new
            { label = Translations.publishAuthorsListButtonTitle [ browserEnv.translations ]
            , onClick = Just <| PublishAuthorsList pubKey
            , theme = theme
            }
            |> Button.view

    else
        div [] []


viewPublishProfileButton : Theme -> BrowserEnv -> PubKey -> HandlerInformation -> Html Msg
viewPublishProfileButton theme browserEnv pubKey handlerInformation =
    if pubKey == handlerInformation.pubKey then
        Button.new
            { label = Translations.publishProfileButtonTitle [ browserEnv.translations ]
            , onClick = Just <| PublishClientProfile pubKey handlerInformation
            , theme = theme
            }
            |> Button.view

    else
        div [] []


viewPublishHandlerInformationButton : Theme -> BrowserEnv -> PubKey -> HandlerInformation -> Html Msg
viewPublishHandlerInformationButton theme browserEnv pubKey handlerInformation =
    if pubKey == handlerInformation.pubKey then
        Button.new
            { label = Translations.publishHandlerInformationButtonTitle [ browserEnv.translations ]
            , onClick = Just <| PublishHandlerInformation pubKey handlerInformation
            , theme = theme
            }
            |> Button.view

    else
        div [] []


viewTechDetails : Theme -> BrowserEnv -> Html Msg
viewTechDetails theme browserEnv =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    Html.div []
        [ span styles.textStyleBody
            [ text (Translations.hintToTechDetails [ browserEnv.translations ])
            , a
                (href "/tech-details" :: styles.textStyleLinks ++ styles.colorStyleArticleHashtags)
                [ text (Translations.seeThis [ browserEnv.translations ]) ]
            ]
        ]


viewFooter : Theme -> BrowserEnv -> Html Msg
viewFooter theme browserEnv =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.gap_3
            , Tw.mb_24
            ]
        ]
        [ Html.span
            []
            [ text <| Translations.aboutFrontendText [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href "https://elm.land/"
                       , target "_blank"
                       ]
                )
                [ text "Elm Land"
                ]
            , text <| Translations.aboutFrontendText2 [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href "https://elm-lang.org/"
                       , target "_blank"
                       ]
                )
                [ text "Elm"
                ]
            , text <| Translations.aboutFrontendText3 [ browserEnv.translations ] ++ " "
            ]
        , Html.span
            []
            [ text <| Translations.aboutBackendText [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href "https://www.phoenixframework.org/"
                       , target "_blank"
                       ]
                )
                [ text "Phoenix Framework"
                ]
            , text "."
            ]
        , Html.span
            []
            [ text <| Translations.sourceCodeText [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href Pareto.source
                       , target "_blank"
                       ]
                )
                [ text Pareto.source
                ]
            , text "."
            ]
        , viewPrivacyPolicyLink styles browserEnv.translations browserEnv.language
        ]


viewPrivacyPolicyLink : Styles Msg -> I18Next.Translations -> Language -> Html Msg
viewPrivacyPolicyLink styles translations language =
    case Pareto.privacyPolicy language of
        Just _ ->
            a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href <| Route.Path.toString Route.Path.Privacy ]
                )
                [ text <| Translations.privacyPolicyLinkText [ translations ] ]

        _ ->
            div [] []
