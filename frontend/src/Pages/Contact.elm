module Pages.Contact exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Layouts
import Layouts.Sidebar
import Nostr.Event exposing (Kind(..), TagReference(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Tailwind.Utilities as Tw
import Translations.Contact as Translations
import Ui.Styles exposing (stylesForTheme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)

publicKeyFileName : String
publicKeyFileName =
    "secure_pareto_space-public_key.asc"

page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared 
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar


-- INIT


type alias Model =
    { 
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { } , Effect.none)


-- UPDATE


type Msg
    = NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ pageContents shared ]
    }

pageContents : Shared.Model -> Html Msg
pageContents shared =
    let
        styles =
            stylesForTheme shared.theme
    in
    div
        [ css
            [ Tw.m_4
            , Tw.space_y_2
            ]
        ]
        [ Html.h1
            (styles.textStyleH1)
            [ text <| Translations.pageTitle [ shared.browserEnv.translations ] ]
        , Html.p
            [ css
                [ 
                ]
            ]
            [ text <| Translations.supportInformation [ shared.browserEnv.translations ]
            , text " "
            , Html.a
                (styles.textStyleLinks ++ styles.colorStyleLinks ++ 
                [ Attr.href <| "mailto:" ++ Pareto.supportEmail ])
                [ text <| Pareto.supportEmail ]
            , text "."
            ]
        , Html.p
            [ css
                [ 
                ]
            ]
            [ text <| Translations.encryptedContactInformation [ shared.browserEnv.translations ]
            , text " "
            , Html.a
                (styles.textStyleLinks ++ styles.colorStyleLinks ++ 
                [ Attr.href <| "mailto:" ++ Pareto.secureEmail ])
                [ text <| Pareto.secureEmail ]
            , text "."
            ]
        , Html.p []
            [ text <| Translations.pgpKeyInfoText [ shared.browserEnv.translations ]
            , text " "
            , Html.a
                (styles.textStyleLinks ++ styles.colorStyleLinks ++
                [ Attr.href <| "/" ++ publicKeyFileName, Attr.download publicKeyFileName ])
                [ text publicKeyFileName ]
            , text "."
            ]
        ]

