module Pages.TechDetails exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled exposing (Html, a, div, h3, li, span, text, ul)
import Html.Styled.Attributes exposing (css, href, target)
import Layouts
import Nostr.Event exposing (Kind, KindInformationLink(..), informationForKind, numberForKind)
import Nostr.HandlerInformation exposing (WebTarget)
import Nostr.Nips exposing (descriptionForNip)
import Page exposing (Page)
import Pages.A.Addr_ exposing (toLayout)
import Pareto
import Route exposing (Route)
import Shared
import Tailwind.Utilities as Tw
import Time exposing (Month(..))
import Translations.TechDetails as Translations
import Ui.Styles exposing (Styles, Theme, stylesForTheme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
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


init : () -> ( Model, Effect Msg )
init () =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    let
        handlerInfo =
            Pareto.applicationInformation shared.browserEnv.now
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.m_4
                , Tw.flex
                , Tw.flex_col
                , Tw.gap_4
                ]
            ]
            [ viewWebTargets shared.theme shared.browserEnv handlerInfo.webTargets
            , viewSupportedKinds shared.theme shared.browserEnv handlerInfo.kinds
            , viewSupportedNips shared.theme shared.browserEnv Pareto.supportedNips
            ]
        ]
    }


viewWebTargets : Theme -> BrowserEnv -> List WebTarget -> Html Msg
viewWebTargets theme browserEnv webTargets =
    let
        styles =
            Ui.Styles.stylesForTheme theme

        viewWebTarget ( target, maybeType ) =
            let
                webTargetType =
                    maybeType |> fold "" (\type_ -> " (" ++ type_ ++ ")")
            in
            li [] [ text <| target ++ webTargetType ]
    in
    div
        []
        [ h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedWebTargetsTitle [ browserEnv.translations ] ]
        , ul [] (List.map viewWebTarget webTargets)
        ]


viewSupportedKinds : Theme -> BrowserEnv -> List Kind -> Html Msg
viewSupportedKinds theme browserEnv kinds =
    let
        styles =
            stylesForTheme theme

        viewSupportedKind kind =
            let
                kindInfo =
                    informationForKind kind
            in
            li []
                [ text <| String.fromInt (numberForKind kind) ++ ": " ++ kindInfo.description ++ " ("
                , viewKindLink styles kindInfo.link
                , text ")"
                ]
    in
    div
        []
        [ h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedKindsTitle [ browserEnv.translations ] ]
        , ul [] (List.map viewSupportedKind kinds)
        ]


viewKindLink : Styles Msg -> Maybe KindInformationLink -> Html Msg
viewKindLink styles link =
    case link of
        Just (LinkToNip nipNumber) ->
            let
                linkText =
                    "NIP " ++ String.fromInt nipNumber
            in
            viewKindLink styles (Just <| OtherLink linkText (nipUrl nipNumber))

        Just (LinkToNips nipNumbers) ->
            nipNumbers
                |> List.map (\nipNumber -> viewKindLink styles (Just <| LinkToNip nipNumber))
                |> List.intersperse (span [] [ text ", " ])
                |> span []

        Just (OtherLink title url) ->
            a (styles.textStyleLinks ++ styles.colorStyleArticleHashtags ++ [ href url, target "_blank" ])
                [ text title ]

        Nothing ->
            span [] []


viewSupportedNips : Theme -> BrowserEnv -> List String -> Html Msg
viewSupportedNips theme browserEnv supportedNips =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    div
        [ css [ Tw.mt_3, Tw.mb_16 ] ]
        [ h3 (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.supportedNipsTitle [ browserEnv.translations ] ]
        , ul [] (List.map (viewNip theme) supportedNips)
        ]


viewNip : Theme -> String -> Html Msg
viewNip theme nip =
    let
        styles =
            Ui.Styles.stylesForTheme theme

        nipInfoText =
            descriptionForNip nip
                |> Maybe.map (\description -> " (" ++ description ++ ")")
                |> Maybe.withDefault ""

        nipLink =
            String.toInt nip |> fold ("https://nips.nostr.com/" ++ nip) nipUrl
    in
    li []
        [ a
            (styles.textStyleLinks
                ++ styles.colorStyleArticleHashtags
                ++ [ href nipLink, target "_blank" ]
            )
            [ text <| "NIP-" ++ nip ++ nipInfoText ]
        ]


nipUrl : Int -> String
nipUrl nipNum =
    "https://nips.nostr.com/" ++ String.fromInt nipNum


fold : b -> (a -> b) -> Maybe a -> b
fold default fn maybeA =
    maybeA |> Maybe.map fn |> Maybe.withDefault default
