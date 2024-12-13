module Pages.About exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (Kind(..), KindInformationLink(..), Tag(..), TagReference(..), buildAddress, emptyEventFilter, informationForKind, numberForKind)
import Nostr.HandlerInformation exposing (HandlerInformation, WebTarget)
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileToJson)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey)
import Pareto
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginStatus(..))
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.About as Translations
import Ui.Profile
import Ui.Styles exposing (Styles, Theme, stylesForTheme)
import View exposing (View)
import Time


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


-- INIT


type alias Model =
    { }

init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( {}, Effect.none)


-- UPDATE


type Msg
    = RecommendClient PubKey HandlerInformation
    | PublishHandlerInformation PubKey HandlerInformation
    | PublishClientProfile PubKey HandlerInformation


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

sendClientRecommendation : Nostr.Model -> PubKey -> HandlerInformation -> Effect Msg
sendClientRecommendation nostr pubKey handlerInformation =
    { pubKey = pubKey
    , createdAt = Time.millisToPosix 0
    , kind = KindHandlerRecommendation
    , tags =
        [ EventDelegationTag (numberForKind KindLongFormContent |> String.fromInt)
        , GenericTag4 "a" (buildAddress (KindLongFormContent, handlerInformation.pubKey, handlerInformation.handlerIdentifier)) Pareto.paretoRelay "web"
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
    { pubKey = pubKey
    , createdAt = Time.millisToPosix 0
    , kind = KindHandlerRecommendation
    , tags =
        [ EventDelegationTag handlerInformation.handlerIdentifier
        , KindTag KindLongFormContent
        , GenericTag4 "a" (buildAddress (KindLongFormContent, handlerInformation.pubKey, handlerInformation.handlerIdentifier)) Pareto.paretoRelay "web"
        ]
    , content = ""
    , id = ""
    , sig = Nothing
    , relay = Nothing
    }
    |> SendHandlerInformation (Nostr.getWriteRelayUrlsForPubKey nostr pubKey)
    |> Shared.Msg.SendNostrEvent
    |> Effect.sendSharedMsg


sendClientProfile : Nostr.Model -> PubKey -> Profile -> Effect Msg
sendClientProfile nostr pubKey profile =
    { pubKey = pubKey
    , createdAt = Time.millisToPosix 0
    , kind = KindUserMetadata
    , tags = [ ]
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
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.aboutPageTitle [shared.browserEnv.translations]
    , body =
        [ div
            [ css
                [ Tw.m_4
                ]
            ]
            [ viewHandlerInformation shared.theme shared.browserEnv shared.loginStatus shared.nostr (Pareto.applicationInformation shared.browserEnv.now)
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
            (Nostr.getProfileValidationStatus nostr handlerInformation.pubKey
                |> Maybe.withDefault ValidationUnknown
            )
        , viewActionButtons theme browserEnv handlerInformation loginStatus
        , viewWebTargets theme browserEnv handlerInformation.webTargets
        , viewSupportedKinds theme browserEnv handlerInformation.kinds
        ]

{-
applicationInformationEvent time =
    { alt = paretoAltText
    , handlerIdentifier = handlerIdentifier
    , hashtags = paretoHashtags
    , kinds = supportedKinds
    , pubKey = paretoPubKey
    , profile = paretoProfile
    , references = paretoReferences
    , time = time
    , webTargets = paretoWebTargets
    , zapTargets = paretoZapTargets
    }
-}

viewActionButtons : Theme -> BrowserEnv -> HandlerInformation -> LoginStatus -> Html Msg
viewActionButtons theme browserEnv handlerInformation loginStatus =
    case loginStatus of
        LoggedIn pubKey ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_3
                    ]
                ]
                [ Button.new
                    { label = Translations.recommendClientButtonTitle [ browserEnv.translations ]
                    , onClick = Just <| RecommendClient pubKey handlerInformation
                    , theme = theme
                    }
                    |> Button.view
                , viewPublishProfileButton theme browserEnv pubKey handlerInformation
                , viewPublishHandlerInformationButton theme browserEnv pubKey handlerInformation
                ]

        _ ->
            div [][]

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
        div [][]

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
        div [][]


viewSupportedKinds : Theme -> BrowserEnv -> List Kind -> Html Msg
viewSupportedKinds theme browserEnv kinds =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    Html.div
        [
        ]
        [ Html.h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedKindsTitle [ browserEnv.translations ]
            ]
        , Html.ul
            [
            ]
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
        [
        ]
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
            |> List.intersperse (Html.span [][ text ", "])
            |> Html.span []

        Just (OtherLink title url) ->
            a  
                (styles.textStyleLinks ++ styles.colorStyleArticleHashtags ++
                [ Attr.href url
                ])
                [ text title
                ]

        Nothing ->
            span [][]
        

viewWebTargets : Theme -> BrowserEnv -> List WebTarget -> Html Msg
viewWebTargets theme browserEnv webTargets =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    Html.div
        [
        ]
        [ Html.h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedWebTargetsTitle [ browserEnv.translations ]
            ]
        , Html.ul
            [
            ]
            (List.map (viewWebTarget theme) webTargets)
        ]

viewWebTarget : Theme -> WebTarget -> Html Msg
viewWebTarget theme (target, maybeType) =
    let
        webTargetType =
            case maybeType of
                Just type_ ->
                    " (" ++ type_ ++ ")"

                Nothing ->
                    ""
    in
    Html.li
        [
        ]
        [ text <| target ++ webTargetType
        ]
