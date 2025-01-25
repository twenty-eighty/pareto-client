module Pages.About exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, div, span, text)
import Html.Styled.Attributes as Attr exposing (css, href)
import I18Next
import Layouts
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..), KindInformationLink(..), Tag(..), TagReference(..), buildAddress, informationForKind, numberForKind)
import Nostr.HandlerInformation exposing (HandlerInformation, WebTarget, buildHandlerInformation)
import Nostr.Nips exposing (descriptionForNip)
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
        , GenericTag4 "a" (buildAddress ( KindHandlerInformation, handlerInformation.pubKey, handlerInformation.handlerIdentifier )) Pareto.paretoRelay "web"
        ]
    , content = ""
    , id = ""
    , sig = Nothing
    , relay = Nothing
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
    , relay = Nothing
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
            [ viewHandlerInformation shared.theme shared.browserEnv shared.loginStatus shared.nostr (Pareto.applicationInformation shared.browserEnv.now)
            , viewSupportedNips shared.theme shared.browserEnv Pareto.supportedNips
            , viewFooter shared.theme shared.browserEnv
            ]
        ]
    }


viewHandlerInformation : Theme -> BrowserEnv -> LoginStatus -> Nostr.Model -> HandlerInformation -> Html Msg
viewHandlerInformation theme browserEnv loginStatus nostr handlerInformation =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            ]
        ]
        [ Ui.Profile.viewProfile
            handlerInformation.profile
            { browserEnv = browserEnv
            , following = UnknownFollowing
            , isAuthor = False
            , subscribe = Nothing
            , theme = theme
            , validation =
                Nostr.getProfileValidationStatus nostr handlerInformation.pubKey
                    |> Maybe.withDefault ValidationUnknown
            }
        , viewActionButtons theme browserEnv handlerInformation loginStatus
        , viewWebTargets theme browserEnv handlerInformation.webTargets
        , viewSupportedKinds theme browserEnv handlerInformation.kinds
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


viewSupportedKinds : Theme -> BrowserEnv -> List Kind -> Html Msg
viewSupportedKinds theme browserEnv kinds =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    Html.div
        []
        [ Html.h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedKindsTitle [ browserEnv.translations ]
            ]
        , Html.ul
            []
            (List.map (viewSupportedKind theme) kinds)
        ]


viewSupportedKind : Theme -> Kind -> Html Msg
viewSupportedKind theme kind =
    let
        styles =
            stylesForTheme theme

        kindInfo =
            informationForKind kind
    in
    Html.li
        []
        [ text <| String.fromInt (numberForKind kind) ++ ": " ++ kindInfo.description ++ " ("
        , viewKindLink styles kindInfo.link
        , text ")"
        ]


viewKindLink : Styles Msg -> Maybe KindInformationLink -> Html Msg
viewKindLink styles link =
    case link of
        Just (LinkToNip nipNumber) ->
            let
                linkText =
                    "NIP " ++ String.fromInt nipNumber

                linkUrl =
                    "https://nips.nostr.com/" ++ String.fromInt nipNumber
            in
            viewKindLink styles (Just <| OtherLink linkText linkUrl)

        Just (LinkToNips nipNumbers) ->
            nipNumbers
                |> List.map (\nipNumber -> viewKindLink styles (Just <| LinkToNip nipNumber))
                |> List.intersperse (Html.span [] [ text ", " ])
                |> Html.span []

        Just (OtherLink title url) ->
            a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href url
                       ]
                )
                [ text title
                ]

        Nothing ->
            span [] []


viewWebTargets : Theme -> BrowserEnv -> List WebTarget -> Html Msg
viewWebTargets theme browserEnv webTargets =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    Html.div
        []
        [ Html.h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedWebTargetsTitle [ browserEnv.translations ]
            ]
        , Html.ul
            []
            (List.map (viewWebTarget theme) webTargets)
        ]


viewWebTarget : Theme -> WebTarget -> Html Msg
viewWebTarget _ ( target, maybeType ) =
    let
        webTargetType =
            case maybeType of
                Just type_ ->
                    " (" ++ type_ ++ ")"

                Nothing ->
                    ""
    in
    Html.li
        []
        [ text <| target ++ webTargetType
        ]


viewSupportedNips : Theme -> BrowserEnv -> List String -> Html Msg
viewSupportedNips theme browserEnv supportedNips =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    Html.div
        [ css
            [ Tw.mt_3
            ]
        ]
        [ Html.h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedNipsTitle [ browserEnv.translations ]
            ]
        , Html.ul
            []
            (List.map (viewNip theme) supportedNips)
        ]


viewNip : Theme -> String -> Html Msg
viewNip theme nip =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    let
        nipLink =
            case String.toInt nip of
                Just nipNum ->
                    "https://nips.nostr.com/" ++ String.fromInt nipNum

                Nothing ->
                    "https://nips.nostr.com/" ++ nip
    in
    Html.li
        []
        [ Html.a
            (styles.textStyleLinks
                ++ styles.colorStyleArticleHashtags
                ++ [ href nipLink
                   ]
            )
            [ text <| "NIP-" ++ nip ++ nipInfoText nip
            ]
        ]


nipInfoText : String -> String
nipInfoText nip =
    case descriptionForNip nip of
        Just description ->
            " (" ++ description ++ ")"

        Nothing ->
            ""


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
            , Tw.mb_4
            ]
        ]
        [ span
            []
            [ text <| Translations.aboutFrontendText [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href "https://elm.land/"
                       ]
                )
                [ text "Elm Land"
                ]
            , text <| Translations.aboutFrontendText2 [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href "https://elm-lang.org/"
                       ]
                )
                [ text "Elm"
                ]
            , text <| Translations.aboutFrontendText3 [ browserEnv.translations ] ++ " "
            ]
        , span
            []
            [ text <| Translations.aboutBackendText [ browserEnv.translations ] ++ " "
            , a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href "https://www.phoenixframework.org/"
                       ]
                )
                [ text "Phoenix Framework"
                ]
            , text "."
            ]
        , text <| Translations.sourceCodeText [ browserEnv.translations ]
        , viewPrivacyPolicyLink styles browserEnv.translations browserEnv.language
        ]


viewPrivacyPolicyLink : Styles Msg -> I18Next.Translations -> Language -> Html Msg
viewPrivacyPolicyLink styles translations language =
    case language of
        German _ ->
            a
                (styles.textStyleLinks
                    ++ styles.colorStyleArticleHashtags
                    ++ [ Attr.href <| Route.Path.toString Route.Path.Privacy
                       ]
                )
                [ text <| Translations.privacyPolicyLinkText [ translations ]
                ]

        _ ->
            div [] []
